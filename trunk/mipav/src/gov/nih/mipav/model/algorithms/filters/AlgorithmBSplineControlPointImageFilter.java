	package gov.nih.mipav.model.algorithms.filters;
	
	
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.BSplineKernelFunction;
import gov.nih.mipav.model.structures.CoxDeBoorBSplineKernelFunction;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;

import java.io.IOException;

import Jama.Matrix;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
	
	
	/**
	 
	 *
	 * @version  0.1 April 28, 2014
	 * @author   William Gandler
	 * 
	  * This a a port of itkBSplineControlPointImageFilter.txx from the itk package.  Here is the original itk header
	  * from the itkBSplineControlPointImageFIlter.h file:
	  * Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkBSplineControlPointImageFilter.h,v $
  Language:  C++
  Date:      $Date: 2009/05/04 14:10:34 $
  Version:   $Revision: 1.5 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.
     
     The original code was contributed in the Insight Journal paper:
     "N-D C^k B-Spline Scattered Data Approximation"
     by Nicholas J. Tustison, James C. Gee
     http://hdl.handle.net/1926/140
     http://www.insight-journal.org/browse/publication/57

	 */
	public class AlgorithmBSplineControlPointImageFilter extends AlgorithmBase {
		private int nDims;
		private int splineOrder[];
		private boolean doMultiLevel;
		private int maximumNumberOfLevels;
		private float origin[];
		private float resolutions[];
		private int extents[];
		private double direction[][];
		private int numberOfControlPoints[];
		private CoxDeBoorBSplineKernelFunction kernel[];
		private BSplineKernelFunction kernelOrder0;
		private BSplineKernelFunction kernelOrder1;
		private BSplineKernelFunction kernelOrder2;
		private BSplineKernelFunction kernelOrder3;
		private int numberOfLevels[];
		private int closeDimension[];
		// Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon = 2.2204460e-16
        // epsilon is called the largest relative spacing
        //epsilon = 1.0;
        //neweps = 1.0;

        //while (true) {

        //     if (1.0 == (1.0 + neweps)) {
        //         break;
        //     } else {
        //         epsilon = neweps;
        //        neweps = neweps / 2.0;
        //     }
        // } // while(true)
		private double epsilon = 2.2204460e-16;
		private double BSplineEpsilon;
		private ModelImage inputImage = null;
		private int extentsLength;
		private int extentsSlice;
		private int xyzExtents;
		private ModelImage outputImage = null;
		private ModelImage psiLattice = null;
		private Matrix[] refinedLatticeCoefficients = null;
	    
	    public AlgorithmBSplineControlPointImageFilter(int nDims) {
	    	this.nDims = nDims;
	    	splineOrder = new int[nDims];
	        int i;
	        for (i = 0; i < nDims; i++) {
	        	splineOrder[i] = 3;
	        }
	        doMultiLevel = false;
	        maximumNumberOfLevels = 1;
	        origin = new float[nDims];
	        resolutions = new float[nDims];
	        for (i = 0; i < nDims; i++) {
	        	resolutions[i] = 1.0f;
	        }
	        extents = new int[nDims];
	        direction = new double[Math.min(nDims,3)][Math.min(nDims,3)];
	        for (i = 0; i < Math.min(nDims,3); i++) {
	        	direction[i][i] = 1.0;
	        }
	        numberOfControlPoints = new int[nDims];
	        kernel = new CoxDeBoorBSplineKernelFunction[nDims];
	        for (i = 0; i < nDims; i++) {
	        	numberOfControlPoints[i] = splineOrder[i] + 1;
	        	kernel[i] = new CoxDeBoorBSplineKernelFunction(splineOrder[i]);	
	        }
	        kernelOrder0 = new BSplineKernelFunction(0);
	        kernelOrder1 = new BSplineKernelFunction(1);
	        kernelOrder2 = new BSplineKernelFunction(2);
	        kernelOrder3 = new BSplineKernelFunction(3);
	        numberOfLevels = new int[nDims];
	        for (i = 0; i < nDims; i++) {
	        	numberOfLevels[i] = 1;
	        }
	        closeDimension = new int[nDims];
	        for (i = 0; i < nDims; i++) {
	        	closeDimension[i] = 0;
	        }
	        BSplineEpsilon = epsilon;
	        refinedLatticeCoefficients = new Matrix[nDims];
	    }
	    
	  //~ Methods --------------------------------------------------------------------------------------------------------
	    
	    /**
	     * Prepares this class for destruction.
	     */
	    public void finalize() {
	    	int i;
	    	splineOrder = null;
	    	extents = null;
	        origin = null;
	        resolutions = null;
	        if (direction != null) {
	        	for (i = 0; i < direction.length; i++) {
	        		direction[i] = null;
	        	}
	        	direction = null;
	        }
	    	numberOfControlPoints = null;
	    	if (kernel != null) {
		        for (i = 0; i < kernel.length; i++) {
		            kernel[i].finalize();	
		        }
		        kernel = null;
	    	}
	    	kernelOrder0 = null;
	        kernelOrder1 = null;
	        kernelOrder2 = null;
	        kernelOrder3 = null;
	    	closeDimension = null;
	    	numberOfLevels = null;
	    	inputImage = null;
	    	outputImage = null;
	    	psiLattice = null;
	    	if (refinedLatticeCoefficients != null) {
	    		for (i = 0; i < refinedLatticeCoefficients.length; i++) {
	    			refinedLatticeCoefficients[i] = null;
	    		}
	    	}
	        super.finalize();
	    }
	    
	    public void setInput(ModelImage inputImage) {
	    	this.inputImage = inputImage;
	    }
	    
	    public double[] getOutputBuffer() {
	    	double outputBuffer[] = new double[extentsLength];
	    	try {
	    		outputImage.exportData(0, extentsLength, outputBuffer);
	    	}
	    	catch(IOException e) {
	    		MipavUtil.displayError("IOException on outputImage.exportData");
	    		return null;
	    	}
	    	return outputBuffer;
	    }
	    
	    public void setOrigin(float[] origin) {
	    	for (int i = 0; i < nDims; i++) {
	    		this.origin[i] = origin[i];
	    	}
	    }
	    
	    public void setResolutions(float[] resolutions) {
	    	for (int i = 0; i < nDims; i++) {
	    		this.resolutions[i] = resolutions[i];
	    	}
	    }
	    
	    public void setDirection(double[][] direction) {
	        for (int i = 0; i < Math.min(nDims,3); i++) {
	        	for (int j = 0; j < Math.min(nDims,3); j++) {
	        		this.direction[i][j] = direction[i][j];
	        	}
	        }
	    }
	    
	    public void setExtents(int[] extents) {
	    	extentsLength = 1;
	    	for (int i = 0; i < nDims; i++) {
	    		this.extents[i] = extents[i];
	    		extentsLength *= extents[i];
	    	}
	    	extentsSlice = extents[0] * extents[1];
	    	xyzExtents = extentsSlice;
	    	if (nDims > 2) {
	    		xyzExtents = extentsSlice * extents[2];
	    	}
	    }
	    
	    public void generateData() {
	    	beforeThreadedGenerateData();
	    	threadedGenerateData();
	    }
	    
	    private void beforeThreadedGenerateData() {
	        for (int i = 0; i < nDims; i++) {
	        	if (extents[i] == 0) {
	        		MipavUtil.displayError("Extents must be specified");
	        		return;
	        	}
	        }
	        
	        outputImage = new ModelImage(ModelStorageBase.DOUBLE, extents, "outputImage");
	        int fileInfoLength = outputImage.getFileInfo().length;
	    	for (int i = 0; i < fileInfoLength; i++) {
	    		outputImage.getFileInfo(i).setOrigin(origin);
	    		outputImage.getFileInfo(i).setResolutions(resolutions);
	    	}
	    	
	    	for (int i = 0; i < Math.min(nDims,3); i++) {
                for (int j = 0; j < Math.min(nDims,3); j++) {
                    outputImage.getMatrix().set(i, j, (float)direction[i][j]);
                }
            }
	    	
	    	int maximumNumberOfSpans = 0;
	    	for (int d = 0; d < nDims; d++) {
	    		int numberOfSpans = numberOfControlPoints[d] - splineOrder[d];
	    		numberOfSpans <<= (numberOfLevels[d] - 1);
	    		if (numberOfSpans > maximumNumberOfSpans) {
	    			maximumNumberOfSpans = numberOfSpans;
	    		}
	    	} // for (int d = 0; d < nDims; d++)
	    	BSplineEpsilon = 100.0 * epsilon;
	    	while ((double)maximumNumberOfSpans == (double)maximumNumberOfSpans - BSplineEpsilon) {
	    		BSplineEpsilon *= 10.0;
	    	}
	    	
	    	for (int i = 0; i < nDims; i++) {
	    		numberOfControlPoints[i] = inputImage.getExtents()[i];
	    	}
	    }
	    
	    private void threadedGenerateData() {
	    	double collapsedPhiLattices[][] = new double[nDims + 1][];
	        int collapsedPhiLatticeIndex[][] = new int[nDims+1][];
	        int size = 1;
	        int it;
	        for (int i = 0; i <= nDims; i++) {
	            size = 1;
	            if (i == 0) {
	            	collapsedPhiLatticeIndex[i] = new int[1];
	            	collapsedPhiLatticeIndex[i][0] = 1;
	            }
	            else {
	            	collapsedPhiLatticeIndex[i] = new int[i];
	            }
	            for (int j = 0; j < i; j++) {
	            	size *= inputImage.getExtents()[j];
	            	collapsedPhiLatticeIndex[i][j] = inputImage.getExtents()[j];
	            }
	            collapsedPhiLattices[i] = new double[size];
	        }
	        try {
	        	inputImage.exportData(0, size, collapsedPhiLattices[nDims]);
	        }
	        catch(IOException e) {
	        	MipavUtil.displayError("IOException on inputImage.exportData");
	        	return;
	        }
	        int totalNumberOfSpans[] = new int[nDims];
	        for (int i = 0; i <  nDims; i++) {
	        	if (closeDimension[i] != 0) {
	        		totalNumberOfSpans[i] = inputImage.getExtents()[i];
	        	}
	        	else {
	        	    totalNumberOfSpans[i] = inputImage.getExtents()[i] - splineOrder[i];
	        	}
	        } // for (int i = 0; i < nDims; i++)
	        double U[] = new double[nDims];
	        double currentU[] = new double[nDims];
	        for (int i = 0; i < nDims; i++) {
	        	currentU[i] = -1;
	        }
	        
	        double outputBuffer[] = new double[extentsLength];
	        
	        int idx[] = new int[nDims];
	        for (it = 0; it < extentsLength; it++) {
	            idx[0] = it % extents[0];
	            idx[1] = (it % extentsSlice) / extents[0];
	            if (nDims > 2) {
	            	idx[2] = (it  % xyzExtents)/ extentsSlice;
	            	if (nDims > 3) {
	            		idx[3] = it / xyzExtents;
	            	}
	            }
	            for (int i = 0; i < nDims; i++) {
	                U[i] = ((double)(totalNumberOfSpans[i] * (idx[i] /* - startIndex[i] */)))/(double)(extents[i] - 1);
	                //if (Math.abs(U[i] - totalNumberOfSpans[i]) <= BSplineEpsilon) {
	                if ((Math.abs(U[i] - totalNumberOfSpans[i]))/totalNumberOfSpans[i] < 1.0E-6) {
	                	U[i] = totalNumberOfSpans[i] - BSplineEpsilon;
	                }
	                if (U[i] >= totalNumberOfSpans[i]) {
	                	MipavUtil.displayError("The collapse point component " + U[i] + 
	                			" is outside the corresponding parametric domain of [0, " + totalNumberOfSpans[i] +
	                			"].");
	                	return;
	                }
	            } // for (int i = 0; i < nDims; i++)
	            for (int i = nDims - 1; i >= 0; i--) {
	                if (U[i] != currentU[i]) {
	                	for (int j = i; j >= 0; j--) {
	                		collapsePhiLattice(collapsedPhiLattices[j+1],collapsedPhiLatticeIndex[j+1], collapsedPhiLattices[j],
	                				collapsedPhiLatticeIndex[j], U[j], j);
	                		currentU[j] = U[j];
	                	}
	                	break;
	                }
	            } // for (int i = nDims - 1; i >= 0; i--)
	            outputBuffer[it] = collapsedPhiLattices[0][0];
	        } // for (it = 0; it < extentsLength; it++)
	        try {
	        	outputImage.importData(0, outputBuffer, true);
	        }
	        catch(IOException e) {
	        	MipavUtil.displayError("IOException on outputImage.importData");
	        	return;
	        }
	    }
	    
	    private void collapsePhiLattice(double[] lattice, int latticeIndex[], double[] collapsedLattice, int collapsedLatticeIndex[],
	    		double u, int dimension) {
	    	int idx[] = new int[4];
	    	int sliceSize = 0;
	    	int xyzSize = 0;
	    	if (latticeIndex.length >= 2) {
	    	    sliceSize = latticeIndex[0] * latticeIndex[1];
	    	    if (latticeIndex.length >= 3) {
	    	    	xyzSize = sliceSize * latticeIndex[2];
	    	    }
	        }
 	    	for (int it = 0; it < collapsedLattice.length; it++) {
	    		if (collapsedLatticeIndex.length == 1) {
	    			idx[0] = it;
	    			idx[1] = 0;
	    			idx[2] = 0;
	    			idx[3] = 0;
	    		}
	    		else if (collapsedLatticeIndex.length == 2) {
	    			idx[0] = it % collapsedLatticeIndex[0];
	    			idx[1] = it / collapsedLatticeIndex[0];
	    			idx[2] = 0;
	    			idx[3] = 0;
	    		}
	    		else if (collapsedLatticeIndex.length == 3) {
	    		    idx[0] = it % collapsedLatticeIndex[0];
        	        idx[1] = (it % sliceSize) / collapsedLatticeIndex[0];
        	    	idx[2] = (it % xyzSize)/ sliceSize;
        	    	idx[3] = 0;
        	    }
	    		else {
	    			idx[0] = it % collapsedLatticeIndex[0];
        	        idx[1] = (it % sliceSize) / collapsedLatticeIndex[0];
        	    	idx[2] = (it % xyzSize)/ sliceSize;
        	    	idx[3] = it / xyzSize;	
	    		}
	    		double data;
	    		data = 0.0;
	    		for (int i = 0; i < splineOrder[dimension] + 1; i++) {
	    		    idx[dimension] = (int)(u) + i;
	    		    double v = u - idx[dimension] + 0.5 *(splineOrder[dimension] - 1);
	    		    double B = 0.0;
	    		    switch (splineOrder[dimension]) {
	    		    case 0:
	    		    	B = kernelOrder0.evaluate(v);
	    		    	break;
	    		    case 1:
	    		    	B = kernelOrder1.evaluate(v);
	    		    	break;
	    		    case 2:
	    		    	B = kernelOrder2.evaluate(v);
	    		    	break;
	    		    case 3:
	    		    	B = kernelOrder3.evaluate(v);
	    		    	break;
	    		    default:
	    		    	B = kernel[dimension].evaluate(v);
	    		    }
	    		    if (closeDimension[dimension] != 0) {
	    		    	idx[dimension] %= latticeIndex[dimension];
	    		    }
	    		    int position = idx[0] + idx[1] * latticeIndex[0] + idx[2] * sliceSize + idx[3] * xyzSize;
	    		    data += (lattice[position] * B);
	    		} // for (int i = 0; i < splineOrder[dimension] + 1; i++)
	    		collapsedLattice[it] = data;
	    	}
	    }
	    
	    private void setNumberOfLevels(int[] levels) {
	    	numberOfLevels = levels;
	    	maximumNumberOfLevels = 1;
	    	for (int i = 0; i < nDims; i++) {
	    		if (numberOfLevels[i] == 0) {
	    			MipavUtil.displayError("The number of levels in each dimension must be greater than 0");
	    			return;
	    		}
	    		if (numberOfLevels[i] > maximumNumberOfLevels) {
	    			maximumNumberOfLevels = numberOfLevels[i];
	    		}
	    	}
	    	Preferences.debug("Setting numberOfLevels array to:\n", Preferences.DEBUG_ALGORITHM);
	    	for (int i = 0; i < nDims; i++) {
	    		Preferences.debug("numberOfLevels["+i+"] = " + numberOfLevels[i] + "\n", Preferences.DEBUG_ALGORITHM);
	    	}
	    	Preferences.debug("Setting maximumNumberOfLevels to " + maximumNumberOfLevels + "\n", Preferences.DEBUG_ALGORITHM);
	    	
	    	if (maximumNumberOfLevels > 1) {
	    		doMultiLevel = true;
	    	}
	    	else {
	    		doMultiLevel = false;
	    	}
	    	setSplineOrder(splineOrder);
	    }
	    
	    public void setSplineOrder(int[] order) {
	    	Preferences.debug("Setting splineOrder array to:\n", Preferences.DEBUG_ALGORITHM);
	    	for (int i = 0; i < nDims; i++) {
	    		Preferences.debug("splineOrder["+i+"] = " + splineOrder[i] + "\n", Preferences.DEBUG_ALGORITHM);
	    	}
	    	splineOrder = order;
	    	for (int i = 0; i < nDims; i++ ) {
	            if (splineOrder[i] == 0 ) {
	                MipavUtil.displayError("The spline order in each dimension must be greater than 0");
	                return;
	            }
	            kernel[i] = new CoxDeBoorBSplineKernelFunction(splineOrder[i]);
	            if (doMultiLevel) {
	                double C[][] = kernel[i].getShapeFunctionsInZeroToOneInterval();
	                double R[][] = new double[C.length][C[0].length];
	                double S[][] = new double[C.length][C[0].length];
	                for (int j = 0; j < C.length; j++) {
	                	for (int k = 0; k < C[0].length; k++) {
	                		R[j][k] = C[j][k];
	                		S[j][k] = C[j][k];
	                	}
	                }
	                for (int j = 0; j < C[0].length; j++) {
	                	double c = Math.pow(2.0, C[0].length - j - 1.0);
	                	
	                	for (int k = 0; k < C.length; k++) {
	                		R[k][j] *= c;
	                	}
	                }
	                Matrix RMat = new Matrix(R);
	                RMat = RMat.transpose();
	                RMat = RMat.flipud();
	                Matrix SMat = new Matrix(S);
	                SMat = SMat.transpose();
	                SMat = SMat.flipud();
	                // Original code has vnl_svd<realType>(R).solve(S)
	                // From vnl_matrix_inverse.h
	                //: Calculates inverse of a matrix (wrapper around vnl_svd<double>)
	                //  vnl_matrix_inverse is a wrapper around vnl_svd<double> that allows
	                //  you to write
	                //  \code
	                //  x = vnl_matrix_inverse<double>(A) * b;
	                //  \endcode
	                //  This is exactly equivalent to
	                //  \code
	                //  x = vnl_svd<double>(A).solve(b);
	                //  \endcode
	                //  but is arguably clearer, and also allows for the vnl_matrix_inverse
	                //  class to be changed to use vnl_qr, say.
	                Matrix QMat = (RMat.inverse()).times(SMat);
	                refinedLatticeCoefficients[i] = QMat.extract(2,  SMat.getColumnDimension());
	            } // if (doMultiLevel)
	    	} // for (i = 0; i < nDims; i++)
	    }
	    
	    public ModelImage refineControlPointLattice(int[] numberOfLevels) {
	    	setNumberOfLevels(numberOfLevels);
	    	psiLattice = (ModelImage)inputImage.clone("psiLattice");
	    	if (psiLattice.getFileInfo() != null) {
	    		FileInfoBase[] fileInfo = psiLattice.getFileInfo();
	    		for (int i = 0; i < fileInfo.length; i++) {
	    			fileInfo[i].setOrigin(origin);
	    			fileInfo[i].setResolutions(resolutions);
	    		}
	    	}
	    	for (int i = 0; i < Math.min(nDims,3); i++) {
                for (int j = 0; j < Math.min(nDims,3); j++) {
                    psiLattice.getMatrix().set(i, j, (float)direction[i][j]);
                }
            }
	    	
	    	for (int m = 1; m < maximumNumberOfLevels; m++) {
	    		int[] numberOfNewControlPoints = new int[nDims];
	    		for (int i = 0; i < nDims; i++) {
	    			numberOfNewControlPoints[i] = psiLattice.getExtents()[i];
	    		}
	    		for (int i = 0; i < nDims; i++) {
	    			if (m < numberOfLevels[i]) {
	    				numberOfNewControlPoints[i] = 2 * numberOfNewControlPoints[i] - splineOrder[i];
	    			}
	    		}
	    		int size[] = new int[nDims];
	    		int sizeLength = 1;
	    		for (int i = 0; i < nDims; i++) {
	    			if (closeDimension[i] != 0) {
	    				size[i] = numberOfNewControlPoints[i] - splineOrder[i];
	    			}
	    			else {
	    				size[i] = numberOfNewControlPoints[i];
	    			}
	    			sizeLength *= size[i];
	    		}
	    		int sliceSize = size[0] * size[1];
	    		int xyzSize = sliceSize;
	    		if (nDims > 2) {
	    			xyzSize = sliceSize * size[2];
	    		}
	    		
	    		double refinedLattice[] = new double[sizeLength];
	    		int idx[] = new int[nDims];
	    		int idxPsi[] = new int[nDims];
	    		int tmp[] = new int[nDims];
	    		int tmpPsi[] = new int[nDims];
	    		int off[] = new int[nDims];
	    		int offPsi[] = new int[nDims];
	    		int sizePsi[] = new int[nDims];
	    		int size2[] = new int[nDims];
	    		for (int i = 0; i < nDims; i++) {
	    			size2[i]= 2;
	    		}
	    		int sliceSize2 = size2[0] * size2[1];
	    		int xyzSize2 = sliceSize2;
	    		if (nDims > 2) {
	    			xyzSize2 = sliceSize2 * size2[2];
	    		}
	    		int N = 1;
	    		for (int i = 0; i < nDims; i++) {
	    			N *= (splineOrder[i] + 1);
	    			sizePsi[i] = splineOrder[i] + 1;
	    		}
	    		int sizePsiSlice = sizePsi[0] * sizePsi[1];
	    		int xyzSizePsi = sizePsiSlice;
	    		if (nDims > 2) {
	    			xyzSizePsi = sizePsiSlice * sizePsi[2];
	    		}
	    		int it = 0;
	    		while (it < sizeLength) {
	    			idx[0] = it % size[0];
	    			idx[1] = (it % sliceSize)/size[0];
	    			if (nDims > 2) {
	    				idx[2] = (it % xyzSize)/ sliceSize;
	    				if (nDims > 3) {
	    					idx[3] = it / xyzSize;
	    				}
	    			}
	    			for (int i = 0; i < nDims; i++) {
	    				if (m < numberOfLevels[i]) {
	    					idxPsi[i] = (int)(0.5 * idx[i]);
	    				}
	    				else {
	    					idxPsi[i] = idx[i];
	    				}
	    			}
	    			for (int i = 0; i < 2 << (nDims - 1); i++) {
	    			 	double sum = 0.0;
	    			 	double val;
	    			 	off[0] = i % size2[0];
	    			 	off[1] = (i % sliceSize2) / size2[0];
	    			 	if (nDims > 2) {
	    			 		off[2] = (i  % xyzSize2)/ sliceSize2;
	    			 		if (nDims > 3) {
		                		off[3] = i / xyzSize2;
		                	}
	    			 	}
	    			 	
	    			 	boolean outOfBoundary = false;
	    			 	for (int j = 0; j < nDims; j++) {
	    			 	    tmp[j] = idx[j] + off[j];
	    			 	    if ((tmp[j] >= numberOfNewControlPoints[j]) && (closeDimension[j] == 0)) {
	    			 	    	outOfBoundary = true;
	    			 	    	break;
	    			 	    }
	    			 	    if (closeDimension[j] != 0) {
	    			 	    	tmp[j] %= size[j];
	    			 	    }
	    			 	} // for (int j = 0; j < nDims; j++)
	    			 	if (outOfBoundary) {
	    			 		continue;
	    			 	}
	    			 	for (int j = 0; j < N; j++) {
	    			 	    offPsi[0] = j % sizePsi[0];
	    			 	    offPsi[1] = (j % sizePsiSlice) / sizePsi[0];
	    			 	    if (nDims > 2) {
	    			 	    	offPsi[2] = (j % xyzSizePsi) / sizePsiSlice;
	    			 	    	if (nDims > 3) {
	    			 	    		offPsi[3] = j / xyzSizePsi;
	    			 	    	}
	    			 	    }
	    			 	    
	    			 	    boolean outOfBoundary2 = false;
	    			 	    for (int k = 0; k < nDims; k++) {
	    			 	    	tmpPsi[k] = idxPsi[k] + offPsi[k];
	    			 	    	if ((tmpPsi[k] >= inputImage.getExtents()[k]) && (closeDimension[k] == 0)) {
	    			 	    		outOfBoundary2 = true;
	    			 	    		break;
	    			 	    	}
	    			 	    	if (closeDimension[k] != 0) {
	    			 	    		tmpPsi[k] %= psiLattice.getExtents()[k];
	    			 	    	}
	    			 	    } // for (int k = 0; k < nDims; k++)
	    			 	    if (outOfBoundary2) {
	    			 	    	continue;
	    			 	    } 
	    			 	    double coeff = 1.0;
	    			 	    for (int k = 0; k < nDims; k++) {
	    			 	    	coeff *= refinedLatticeCoefficients[k].get(off[k], offPsi[k]);
	    			 	    }
	    			 	    if (nDims == 2) {
	    			 	        val = psiLattice.getDouble(tmpPsi[0], tmpPsi[1]);
	    			 	    }
	    			 	    else if (nDims == 3) {
	    			 	    	val = psiLattice.getDouble(tmpPsi[0], tmpPsi[1], tmpPsi[2]);
	    			 	    }
	    			 	    else {
	    			 	    	val = psiLattice.getDouble(tmpPsi[0], tmpPsi[1], tmpPsi[2], tmpPsi[3]);	
	    			 	    }
	    			 	    val *= coeff;
	    			 	    sum += val;
	    			 	} // for (int j = 0; j < N; j++)
	    			 	if (nDims == 2) {
	    			 	    refinedLattice[tmp[0] + tmp[1] * size[0]] = sum;
	    			 	}
	    			 	else if (nDims == 3) {
	    			 		refinedLattice[tmp[0] + tmp[1] * size[0] + tmp[2] * sliceSize] = sum; 
	    			 	}
	    			 	else {
	    			 		refinedLattice[tmp[0] + tmp[1] * size[0] + tmp[2] * sliceSize + tmp[3] * xyzSize] = sum; 	
	    			 	}
	    			} // for (int i = 0; i < 2 << (nDims - 1); i++)
	    			
	    		    boolean isEvenIndex = false;
	    		    while (!isEvenIndex && (it < sizeLength)) {
	    			    it++;
	    			    idx[0] = it % size[0];
		    			idx[1] = (it % sliceSize)/size[0];
		    			if (nDims > 2) {
		    				idx[2] = (it % xyzSize)/ sliceSize;
		    				if (nDims > 3) {
		    					idx[3] = it / xyzSize;
		    				}
		    			}
		    			isEvenIndex = true;
		    			for (int i = 0; i < nDims; i++) {
		    				if ((idx[i] % 2) != 0) {
		    					isEvenIndex = false;
		    				}
		    			}
	    		    } // while (!isEvenIndex && (it < sizeLength))
	    		} // while (it < sizeLength)
	    		psiLattice.disposeLocal();
	    		psiLattice = null;
	    		psiLattice = new ModelImage(ModelStorageBase.DOUBLE, size, "psiLattice");
	    		try {
	    			psiLattice.importData(0, refinedLattice, true);
	    		}
	    		catch (IOException e) {
	    			MipavUtil.displayError("IOException on psiLattice.importData(0, refinedLattice, true)");
	    			return null;
	    		}
	    	} // for (m = 1; m < maximumNumberOfLevels; m++)
	    	
	    	// Specify the pose parameters of the control point lattice
	    	float localResolutions[] = new float[nDims];
	    	double localOrigin[] = new double[nDims];
	    	for (int i = 0; i < nDims; i++) {
	    		double domain = resolutions[i] * (extents[i] - 1);
	    		int totalNumberOfSpans = psiLattice.getExtents()[i];
	    		if (closeDimension[i] == 0) {
	    			totalNumberOfSpans -= splineOrder[i];
	    		}
	    		localResolutions[i] = (float)(domain / (double)totalNumberOfSpans);
	    		localOrigin[i] = -0.5 * localResolutions[i] * (splineOrder[i] - 1);
	    	} // for (int i = 0; i < nDims; i++)
	    	float localOrigin2[] = new float[nDims];
	    	for (int i = 0; i <Math.min(nDims,3); i++) {
	    		for (int j = 0; j < Math.min(nDims,3); j++) {
	    			localOrigin2[i] += (float)(direction[i][j] * localOrigin[j]);
	    		}
	    	}
	    	if (nDims > 3) {
	    		localOrigin2[3] = (float)localOrigin[3];
	    	}
	    	
	    	if (psiLattice.getFileInfo() != null) {
	    		FileInfoBase[] fileInfo = psiLattice.getFileInfo();
	    		for (int i = 0; i < fileInfo.length; i++) {
	    			fileInfo[i].setOrigin(localOrigin2);
	    			fileInfo[i].setResolutions(localResolutions);
	    		}
	    	}
	    	for (int i = 0; i < Math.min(nDims,3); i++) {
                for (int j = 0; j < Math.min(nDims,3); j++) {
                    psiLattice.getMatrix().set(i, j, (float)direction[i][j]);
                }
            }
	        return psiLattice;	
	    }

	    /**
	     * Starts the program.
	     */
	    public void runAlgorithm() {
	        

	        if (srcImage == null) {
	            displayError("Source Image is null");
	            finalize();

	            return;
	        }
	    }
}

