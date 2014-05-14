	package gov.nih.mipav.model.algorithms.filters;
	
import gov.nih.mipav.model.structures.BSplineKernelFunction;
import gov.nih.mipav.model.structures.CoxDeBoorBSplineKernelFunction;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;

import java.io.IOException;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector4d;
import Jama.Matrix;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
	
	
	/**
	 
	 *
	 * @version  0.1 April 28, 2014
	 * @author   William Gandler
	 * 
	  * This a a port of itkBSplineScatteredDataPointSetToImageFilter.hxx from the itk package.  Here is the original itk header
	  * from the itkBSplineScatteredDataPointSetToImageFilter.hxx file:
	  *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
     The original code was contributed in the Insight Journal paper:
     "N-D C^k B-Spline Scattered Data Approximation"
     by Nicholas J. Tustison, James C. Gee
     http://hdl.handle.net/1926/140
     http://www.insight-journal.org/browse/publication/57
  
	 */
	public class AlgorithmBSplineScatteredDataPointSetToImageFilter extends AlgorithmPointSetToImageFilter {
		
		private int splineOrder[];
		private int numberOfControlPoints[];
		private CoxDeBoorBSplineKernelFunction kernel[];
		private BSplineKernelFunction kernelOrder0;
		private BSplineKernelFunction kernelOrder1;
		private BSplineKernelFunction kernelOrder2;
		private BSplineKernelFunction kernelOrder3;
		private int closeDimension[];
		private boolean doMultiLevel;
		private boolean generateOutputImage;
		private int numberOfLevels[];
		private int maximumNumberOfLevels;
		ModelImage phiLattice;
		ModelImage psiLattice;
		private Vector<Double> inputPointData;
		private Vector<Double> outputPointData;
		private Vector<Double> pointWeights;
		private boolean usePointWeights;
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
		private boolean isFittingComplete;
		private Matrix[] refinedLatticeCoefficients;
		private int currentLevel;
		private int[] currentNumberOfControlPoints;
		private ModelImage omegaLattice;
		private ModelImage deltaLattice;
	    
	    public AlgorithmBSplineScatteredDataPointSetToImageFilter(int nDims) {
	        super(nDims);
	        splineOrder = new int[nDims];
	        int i;
	        for (i = 0; i < nDims; i++) {
	        	splineOrder[i] = 3;
	        }
	        numberOfControlPoints = new int[nDims];
	        currentNumberOfControlPoints = new int[nDims];
	        kernel = new CoxDeBoorBSplineKernelFunction[nDims];
	        for (i = 0; i < nDims; i++) {
	        	numberOfControlPoints[i] = splineOrder[i] + 1;
	        	currentNumberOfControlPoints[i] = splineOrder[i] + 1;
	        	kernel[i] = new CoxDeBoorBSplineKernelFunction(splineOrder[i]);
	        }
	        kernelOrder0 = new BSplineKernelFunction(0);
	        kernelOrder1 = new BSplineKernelFunction(1);
	        kernelOrder2 = new BSplineKernelFunction(2);
	        kernelOrder3 = new BSplineKernelFunction(3);
	        closeDimension = new int[nDims];
	        for (i = 0; i < nDims; i++) {
	        	closeDimension[i] = 0;
	        }
	        doMultiLevel = false;
	        generateOutputImage = true;
	        numberOfLevels = new int[nDims];
	        for (i = 0; i < nDims; i++) {
	        	numberOfLevels[i] = 1;
	        }
	        maximumNumberOfLevels = 1;
	        // itk only needs data type and extents to construct an image, but MIPAV needs extents[] as well.
	        phiLattice = null;
	        psiLattice = null;
	        inputPointData = new Vector<Double>();
	        outputPointData = new Vector<Double>();
	        
	        usePointWeights = false;
	        pointWeights = new Vector<Double>();
	        BSplineEpsilon = epsilon;
	        isFittingComplete = false;
	        refinedLatticeCoefficients = new Matrix[nDims];
	    }
	    
	  //~ Methods --------------------------------------------------------------------------------------------------------

	    /**
	     * Prepares this class for destruction.
	     */
	    public void finalize() {
	    	int i;
	    	splineOrder = null;
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
	    	phiLattice = null;
	    	if (psiLattice != null) {
	    		psiLattice.disposeLocal();
	    		psiLattice = null;
	    	}
	    	if (inputPointData != null) {
	    	    inputPointData.clear();
	    	    inputPointData = null;
	        }
	    	if (outputPointData != null) {
	    		outputPointData.clear();
	    		outputPointData = null;
	    	}
	    	if (pointWeights != null) {
	    		pointWeights.clear();
	    		pointWeights = null;
	    	}
	    	refinedLatticeCoefficients = null;
	    	currentNumberOfControlPoints = null;
	    	omegaLattice = null;
	    	deltaLattice = null;
	        super.finalize();
	    }
	    
	    public void setGenerateOutputImage(boolean generateOutputImage) {
	    	this.generateOutputImage = generateOutputImage;
	    }
	    
	    public ModelImage getPhiLattice() {
	    	return phiLattice;
	    }
	    
	    public void setNumberOfLevels(int levels) {
	    	for (int i = 0; i < nDims; i++) {
	    		this.numberOfLevels[i] = levels;
	    	}
	    	setNumberOfLevels(numberOfLevels);
	    }
	    
	    public void setNumberOfLevels(int[] levels) {
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
	    	Preferences.debug("Setting numberOfLevels array to:\n");
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
	    	int i, j, k;
	    	splineOrder = order;
	    	Preferences.debug("Setting splineOrder array to order array:\n", Preferences.DEBUG_ALGORITHM);
	    	for (i = 0; i < nDims; i++) {
	    		Preferences.debug("splineOrder["+i+"] = " + order[i] + "\n", Preferences.DEBUG_ALGORITHM);
	    	}
	    	for (i = 0; i < nDims; i++ ) {
	            if (splineOrder[i] == 0 ) {
	                MipavUtil.displayError("The spline order in each dimension must be greater than 0");
	                return;
	            }
	            kernel[i] = new CoxDeBoorBSplineKernelFunction(splineOrder[i]);
	            if (doMultiLevel) {
	                double C[][] = kernel[i].getShapeFunctionsInZeroToOneInterval();
	                double R[][] = new double[C.length][C[0].length];
	                double S[][] = new double[C.length][C[0].length];
	                for (j = 0; j < C.length; j++) {
	                	for (k = 0; k < C[0].length; k++) {
	                		R[j][k] = C[j][k];
	                		S[j][k] = C[j][k];
	                	}
	                }
	                for (j = 0; j < C[0].length; j++) {
	                	double c = Math.pow(2.0, C[0].length - j - 1.0);
	                	
	                	for (k = 0; k < C.length; k++) {
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
	    
	    public void setSplineOrder(int order) {
	    	for (int i = 0; i < nDims; i++) {
	    		this.splineOrder[i] = order;
	    	}
	    	setSplineOrder(splineOrder);
	    }
	    
	    public void setNumberOfControlPoints(int[] numberOfControlPoints) {
	    	for (int i = 0; i < nDims; i++) {
	    		this.numberOfControlPoints[i] = numberOfControlPoints[i];
	    	}
	    }
	    
	    public void setPointWeights(Vector<Double> weights) {
	    	usePointWeights = true;
	    	this.pointWeights.clear();
	    	for (int i = 0; i < weights.size(); i++) {
	    		this.pointWeights.add(weights.get(i));
	    	}
	    }
	    
	    public void generateData() {
	    	super.generateData();
	    	// Perform some error checking on the input
	    	if (pointData.size() != pointLocation.size()) {
	    		MipavUtil.displayError("pointData and pointLocation must have the same size");
	    		return;
	    	}
	    	if (usePointWeights  && (pointWeights.size() != pointData.size())) {
	    		System.err.println("pointWeights.size() = " + pointWeights.size());
	    		System.err.println("pointData.size() = " + pointData.size());
	    		MipavUtil.displayError("pointsWeights and pointData must have the same size");
	    		return;
	    	}
	    	for (int i = 0; i < nDims; i++) {
	    		if (numberOfControlPoints[i] < splineOrder[i] + 1){
	    			MipavUtil.displayError("The number of control points must be greater than the spline order");
	    			return;
	    		}
	    	}
	    	
	    	// Calculate the appropriate epsilon value.
	    	int maximumNumberOfSpans = 0;
	    	for (int d = 0; d < nDims; d++) {
	    		int numberOfSpans = numberOfControlPoints[d] - splineOrder[d];
	    		numberOfSpans <<= (numberOfLevels[d] - 1);
	    		if (numberOfSpans > maximumNumberOfSpans) {
	    			maximumNumberOfSpans = numberOfSpans;
	    		}
	    	}
	    	BSplineEpsilon = 100.0 * epsilon;
	    	while ((double)maximumNumberOfSpans == ((double)maximumNumberOfSpans - BSplineEpsilon)) {
	    	    BSplineEpsilon *= 10.0;	
	    	}
	    	
	    	inputPointData.clear();
	    	outputPointData.clear();
	    	if (!usePointWeights) {
	    		pointWeights.clear();
	    	}
	    	for (int i = 0; i < pointData.size(); i++) {
	    	    if (!usePointWeights) {	
	    	    	pointWeights.add(1.0);
	    	    }
	    	    inputPointData.add(pointData.get(i));
	    	    outputPointData.add(pointData.get(i));
	    	}
	    	currentLevel = 0;
	    	for (int i = 0; i < nDims; i++) {
	    	    currentNumberOfControlPoints[i] = numberOfControlPoints[i];
	    	}
	    	beforeThreadedGenerateData();
	    	threadedGenerateData();
	    	afterThreadedGenerateData();
	    	updatePointSet();
	    	
	    	if (doMultiLevel) {
	    		psiLattice = new ModelImage(ModelStorageBase.DOUBLE, phiLattice.getExtents(), "psiLattice");
	    	} // if (doMultiLevel)
	    	
	    	for (currentLevel = 1; currentLevel < maximumNumberOfLevels; currentLevel++) {
	    	   int psiLength = 1;
	    	   for (int i = 0; i < nDims; i++) {
	    		   psiLength *= psiLattice.getExtents()[i];
	    	   }
	    	   for (int itpsi = 0; itpsi < psiLength; itpsi++) {
	    		   psiLattice.set(itpsi, phiLattice.getDouble(itpsi) + psiLattice.getDouble(itpsi));
	    	   }
	    	   refineControlPointLattice();
	    	   
	    	   for (int i = 0; i < nDims; i++) {
	    		   if (currentLevel < numberOfLevels[i]) {
	    			   currentNumberOfControlPoints[i] = 2 * currentNumberOfControlPoints[i] - splineOrder[i];
	    		   }
	    	   } // for (int i = 0; i < nDims; i++)
	    	   
	    	   Preferences.debug("Current level = " + currentLevel + "\n", Preferences.DEBUG_ALGORITHM);
	    	   for (int i = 0; i < nDims; i++) {
	    	       Preferences.debug("Current number of control points for dimension " + i + " = " + 
	    	        currentNumberOfControlPoints[i] + "\n", Preferences.DEBUG_ALGORITHM);
	    	   }
	    	   
	    	   double averageDifference = 0.0;
	    	   double totalWeight = 0.0;
	    	   
	    	   int itin = 0;
	    	   while (itin < inputPointData.size()) {
	    		    inputPointData.set(itin,inputPointData.get(itin) - outputPointData.get(itin)); 
    		    	double weight = pointWeights.get(itin);
    		    	averageDifference += Math.abs(inputPointData.get(itin) - outputPointData.get(itin)) * weight;
    		    	totalWeight += weight;
	    		    
	    		    itin++;
	    	   } // while (itin < inputPointData.size())
	    	   
	    	   if (totalWeight > 0) {
	    		   Preferences.debug("The average weighted difference norm of the point set is " +
	    	                         (averageDifference/totalWeight) + "\n", Preferences.DEBUG_ALGORITHM);
	    	   }
	    	   
	    	   beforeThreadedGenerateData();
		       threadedGenerateData();
		       afterThreadedGenerateData();
		       updatePointSet();
	    	} // for (currentLevel = 1; currentLevel < maximumNumberOfLevels; currentLevel++)
	    	
	    	if (doMultiLevel) {
	    		int psiLength = 1;
	    	    for (int i = 0; i < nDims; i++) {
	    		    psiLength *= psiLattice.getExtents()[i];
	    	    }
	    	    for (int itpsi = 0; itpsi < psiLength; itpsi++) {
	    		    psiLattice.set(itpsi, phiLattice.getDouble(itpsi) + psiLattice.getDouble(itpsi));
	    	    }
	    	    
	    	    for (int itpsi = 0; itpsi < psiLength; itpsi++) {
	    		    phiLattice.set(itpsi, psiLattice.getDouble(itpsi));
	    	    }
	    	    updatePointSet();
	    	} // if (doMultiLevel)
	    	
	    	isFittingComplete = true;
	    	if (generateOutputImage) {
	    		threadedGenerateData();
	    	}
	    	
	    	setPhiLatticeParametricDomainParameters();
	    }
	    
	    private void beforeThreadedGenerateData() {
	        if (!isFittingComplete) {
	        	int size[] = new int[nDims];
	        	for (int i = 0; i < nDims; i++) {
	        		if (closeDimension[i] != 0) {
	        			size[i] = currentNumberOfControlPoints[i] - splineOrder[i];
	        		}
	        		else {
	        			size[i] = currentNumberOfControlPoints[i];
	        		}
	        	}
	        	if (omegaLattice != null) {
	        		omegaLattice.disposeLocal();
	        		omegaLattice = null;
	        	}
	        	omegaLattice = new ModelImage(ModelStorageBase.DOUBLE, size, "omegaLattice");
	        	if (deltaLattice != null) {
	        		deltaLattice.disposeLocal();
	        		deltaLattice = null;
	        	}
	        	deltaLattice = new ModelImage(ModelStorageBase.DOUBLE, size, "deltaLattice");
	        } // if (!isFittingComplete)
	    }
	    
	    private void threadedGenerateData() {
	    	if (!isFittingComplete) {
	    		threadedGenerateDataForFitting();
	    	}
	    	else {
	    		threadedGenerateDataForReconstruction();
	    	}
	    	
	    }
	    
	    private void threadedGenerateDataForFitting() {
	        int size[] = new int[nDims];
	        int neighborhoodWeightArrayLength = 1;
	        for (int i = 0; i < nDims; i++) {
	            size[i] = splineOrder[i] + 1;
	            neighborhoodWeightArrayLength *= size[i];
	        }
	        int neighborhoodWeightArraySliceSize = size[0]*size[1];
	        int xyzNeighborhoodWeightArray = neighborhoodWeightArraySliceSize;
	        if (nDims > 2) {
	        	xyzNeighborhoodWeightArray *= size[2];
	        }
	        double neighborhoodWeightArray[] = new double[neighborhoodWeightArrayLength];
	        double p[] = new double[nDims];
	        double r[] = new double[nDims];
	        for (int i = 0; i < nDims; i++) {
	        	r[i] = (double)(currentNumberOfControlPoints[i] - splineOrder[i])/((extents[i] - 1.0) * resolutions[i]);
	        }
	        
	        for (int n = 0; n < pointLocation.size(); n++) {
	        	double point[] = new double[nDims];
	        	point[0] = pointLocation.get(n).X;
	        	point[1] = pointLocation.get(n).Y;
	        	if (nDims > 2) {
	        	    point[2] = pointLocation.get(n).Z;
	        	    if (nDims > 3) {
	        	    	point[3] = pointLocation.get(n).W;
	        	    }
	        	}
	        	for (int i = 0; i < nDims; i++) {
	        	    int totalNumberOfSpans = currentNumberOfControlPoints[i] - splineOrder[i];
	        	    
	        	    p[i] = (point[i] - origin[i]) * r[i];
	        	    //if (Math.abs(p[i] - totalNumberOfSpans) <= BSplineEpsilon) {
	        	    if ((Math.abs(p[i] - totalNumberOfSpans))/totalNumberOfSpans < 1.0E-6) {
	        	    	p[i] = (double)(totalNumberOfSpans) - BSplineEpsilon;
	        	    }
	        	    if (p[i] >= totalNumberOfSpans) {
	        	    	MipavUtil.displayError("The reparameterized point component p["+i+"] =" + p[i] + 
	        	    			" is outside the corresponding parametric domain of [0, " + totalNumberOfSpans + "].");
	        	    	return;
	        	    }
	        	} // for (int i = 0; i < nDims; i++)
	        	
	        	double w2Sum = 0.0;
	        	int idx[] = new int[nDims];
	        	for (int itw = 0; itw < neighborhoodWeightArrayLength; itw++) {
	        	    double B = 1.0;
	        	    idx[0] = itw % size[0];
	        	    idx[1] = (itw % neighborhoodWeightArraySliceSize) / size[0];
	        	    if (nDims > 2) {
	        	    	idx[2] = (itw  % xyzNeighborhoodWeightArray) / neighborhoodWeightArraySliceSize;
	        	    	if (nDims > 3) {
	        	    		idx[3] = itw / xyzNeighborhoodWeightArray;
	        	    	}
	        	    }
	        	    for (int i = 0; i < nDims; i++) {
	        	        double u  = (p[i] - (int)p[i] - idx[i]) + 0.5*(splineOrder[i] - 1);
	        	        switch (splineOrder[i]) {
	        	        case 0:
	        	        	B *= kernelOrder0.evaluate(u);
	        	        	break;
	        	        case 1:
	        	        	B *= kernelOrder1.evaluate(u);
	        	        	break;
	        	        case 2:
	        	        	B *= kernelOrder2.evaluate(u);
	        	        	break;
	        	        case 3:
	        	        	B *= kernelOrder3.evaluate(u);
	        	        	break;
	        	        default:
	        	        	B *= kernel[i].evaluate(u);
	        	        } // switch (splineOrder[i)
	        	    } // for (int i = 0; i < nDims; i++)
	        	    neighborhoodWeightArray[itw] = B;
	        	    w2Sum += B * B;
	        	} // for (int itw = 0; itw < neighborhoodWeightArrayLength; itw++)
	        	
	        	for (int itw = 0; itw < neighborhoodWeightArrayLength; itw++) {
	        		idx[0] = itw % size[0];
	        	    idx[1] = (itw % neighborhoodWeightArraySliceSize) / size[0];
	        	    if (nDims > 2) {
	        	    	idx[2] = (itw % xyzNeighborhoodWeightArray) / neighborhoodWeightArraySliceSize;
	        	    	if (nDims > 3) {
	        	    		idx[3] = itw / xyzNeighborhoodWeightArray;
	        	    	}
	        	    }
	        	    for (int i = 0; i < nDims; i++) {
	        	        idx[i] += (int)(p[i]);
	        	        if (closeDimension[i] != 0) {
	        	        	idx[i] %= size[i];
	        	        }
	        	    } // for (int i = 0; i < nDims; i++)
	        	    double wc = pointWeights.get(n);
	        	    double t = neighborhoodWeightArray[itw];
	        	    int index;
	        	    boolean okay = true;
	        	    for (int i = 0; i < nDims; i++) {
	        	    	if ((idx[i] < 0) || (idx[i] >= omegaLattice.getExtents()[i])) {
	        	    		okay = false; 
	        	    	}
	        	    }
	        	    if (nDims == 2) {
	        	    	index = idx[0] + idx[1] * omegaLattice.getExtents()[0];
	        	    }
	        	    else if (nDims == 3) {
	        	    	index = idx[0] + idx[1] * omegaLattice.getExtents()[0] + 
	        	    			idx[2] * omegaLattice.getExtents()[0] * omegaLattice.getExtents()[1];
	        	    }
	        	    else {
	        	    	index = idx[0] + idx[1] * omegaLattice.getExtents()[0] + 
	        	    			idx[2] * omegaLattice.getExtents()[0] * omegaLattice.getExtents()[1] +
	        	    			idx[3] * omegaLattice.getExtents()[0] * omegaLattice.getExtents()[1] * omegaLattice.getExtents()[2];
	        	    }
	        	    if (okay) {
	        	        omegaLattice.set(index, omegaLattice.getDouble(index) + wc * t * t);
	        	    }
	        	    double data = inputPointData.get(n);
	        	    data *= (t * t * t * wc / w2Sum);
	        	    if (okay) {
	        	        deltaLattice.set(index, deltaLattice.getDouble(index) + data);
	        	    }
	        	} // for (int itw = 0; itw < neighborhoodWeightArrayLength; itw++)
	        } // for (int n = 0; n < pointLocation.size(); n++)
	    }
	    
	    private void threadedGenerateDataForReconstruction() {
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
	            	size *= phiLattice.getExtents()[j];
	            	collapsedPhiLatticeIndex[i][j] = phiLattice.getExtents()[j];
	            }
	            collapsedPhiLattices[i] = new double[size];
	        }
	        try {
	        	phiLattice.exportData(0, size, collapsedPhiLattices[nDims]);
	        }
	        catch(IOException e) {
	        	MipavUtil.displayError("IOException on phiLattice.exportData");
	        	return;
	        }
	        int totalNumberOfSpans[] = new int[nDims];
	        for (int i = 0; i <  nDims; i++) {
	        	if (closeDimension[i] != 0) {
	        		totalNumberOfSpans[i] = phiLattice.getExtents()[i];
	        	}
	        	else {
	        	    totalNumberOfSpans[i] = phiLattice.getExtents()[i] - splineOrder[i];
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
	            	idx[2] = (it % xyzExtents) / extentsSlice;
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
	    
	    private void afterThreadedGenerateData() {
	      if (!isFittingComplete) {
	          // Accumulate all the delta lattice and omega lattice values to
	    	  // calculate the final phi lattice.
	    	  // Not necessary if only 1 thread was used
	    	  
	    	  // Generate the control point lattice
	    	  int size[] = new int[nDims];
	    	  int latticeLength = 1;
	    	  for (int i = 0; i < nDims; i++) {
	    		  if (closeDimension[i] != 0) {
	    			  size[i] = currentNumberOfControlPoints[i] - splineOrder[i];
	    		  }
	    		  else {
	    			  size[i] = currentNumberOfControlPoints[i];
	    		  }
	    		  latticeLength *= size[i];
	    	  } // for (int i = 0; i < nDims; i++)
	    	  double latticeBuffer[] = new double[latticeLength];
	    	  for (int itp = 0; itp < latticeLength; itp++) {
	    		  double P = 0;
	    		  if (omegaLattice.getDouble(itp) != 0) {
	    			  P = deltaLattice.getDouble(itp) / omegaLattice.getDouble(itp);
	    			  if ((Double.isNaN(P)) || (Double.isInfinite(P))) {
	    				  P = 0;
	    			  }
	    			  latticeBuffer[itp] = P;
	    		  }
	    	  }
	    	  phiLattice = new ModelImage(ModelStorageBase.DOUBLE, size, "phiLattice");
	    	  try {
	    		  phiLattice.importData(0, latticeBuffer, true);
	    	  }
	    	  catch (IOException e) {
	    		  MipavUtil.displayError("IOException on phiLattice.importData");
	    		  return;
	    	  }
	      } // if (!isFittingComplete)
	    }
	    
	    private void updatePointSet() {
	    	double collapsedPhiLattices[][] = new double[nDims + 1][];
	        int collapsedPhiLatticeIndex[][] = new int[nDims+1][];
	        int size = 1;
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
	            	size *= phiLattice.getExtents()[j];
	            	collapsedPhiLatticeIndex[i][j] = phiLattice.getExtents()[j];
	            }
	            collapsedPhiLattices[i] = new double[size];
	        }
	        try {
	        	phiLattice.exportData(0, size, collapsedPhiLattices[nDims]);
	        }
	        catch(IOException e) {
	        	MipavUtil.displayError("IOException on phiLattice.exportData");
	        	return;
	        }
	        int totalNumberOfSpans[] = new int[nDims];
	        for (int i = 0; i <  nDims; i++) {
	        	if (closeDimension[i] != 0) {
	        		totalNumberOfSpans[i] = phiLattice.getExtents()[i];
	        	}
	        	else {
	        	    totalNumberOfSpans[i] = phiLattice.getExtents()[i] - splineOrder[i];
	        	}
	        } // for (int i = 0; i < nDims; i++)
	        double U[] = new double[nDims];
	        double currentU[] = new double[nDims];
	        for (int i = 0; i < nDims; i++) {
	        	currentU[i] = -1;
	        }
	        
	        for (int itin = 0; itin < inputPointData.size(); itin++) {
	        	double point[] = new double[nDims];
	        	point[0] = pointLocation.get(itin).X;
	        	point[1] = pointLocation.get(itin).Y;
	            if (nDims > 2) {
	            	point[2] = pointLocation.get(itin).Z;
	            	if (nDims > 3) {
	            		point[3] = pointLocation.get(itin).W;
	            	}
	            }
	            for (int i = 0; i < nDims; i++) {
	            	U[i] = totalNumberOfSpans[i] * (point[i] - origin[i])/((extents[i] - 1) * resolutions[i]);
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
	            outputPointData.set(itin,collapsedPhiLattices[0][0]);
	        } // for (int itin = 0; itin < inputPointData.size(); itin++) 
	        	
	    }
	    
	    private void refineControlPointLattice() {
	        int[] numberOfNewControlPoints = currentNumberOfControlPoints.clone();
	        for (int i = 0; i < nDims; i++) {
	            if (currentLevel < numberOfLevels[i])	{
	            	numberOfNewControlPoints[i] = 2 * numberOfNewControlPoints[i] - splineOrder[i];
	            }
	        } // for (int i = 0; i < nDims; i++)
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
	        } // for (int i = 0; i < nDims; i++)
	        int sliceSize = size[0] * size[1];
	        int xyzSize = sliceSize;
	        if (nDims > 2) {
	        	xyzSize = sliceSize * size[2];
	        }
	        double[] refinedLattice = new double[sizeLength];
	        int sizePsi[] = new int[nDims];
	        int size2[] = new int[nDims];
	        for (int i = 0; i < nDims; i++) {
	        	size2[i] = 2;
	        }
	        int size2Slice = size2[0] * size2[1];
	        int xyzSize2 = size2Slice;
	        if (nDims > 2) {
	        	xyzSize2 = size2Slice * size2[2];
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
	        
	        int idx[] = new int[nDims];
	        int idxPsi[] = new int[nDims];
	        int off[] = new int[nDims];
	        int tmp[] = new int[nDims];
	        int offPsi[] = new int[nDims];
	        int tmpPsi[] = new int[nDims];
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
	                if (currentLevel < numberOfLevels[i]) {
	                	idxPsi[i] = (int)(0.5 * idx[i]);
	                }
	                else {
	                	idxPsi[i] = idx[i];
	                }
	            } // for (int i = 0; i < nDims; i++)
	            for (int i = 0; i < 2 << (nDims - 1); i++) {
	                double sum = 0.0;
	                double val = 0.0;
	                off[0] = i % size2[0];
	                off[1] = (i % size2Slice)/size2[0];
	                if (nDims > 2) {
	                	off[2] = (i % xyzSize2) /size2Slice;
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
	                    offPsi[1] = (j % sizePsiSlice)/sizePsi[0];
	                    if (nDims > 2) {
	                    	offPsi[2] = (j % xyzSizePsi) / sizePsiSlice;
	                    	if (nDims > 3) {
	                    		offPsi[3] = j / xyzSizePsi;
	                    	}
	                    }
	                    boolean isOutOfBoundary = false;
	                    for (int k = 0; k < nDims; k++) {
	                    	tmpPsi[k] = idxPsi[k] + offPsi[k];
	                    	if ((tmpPsi[k] >= currentNumberOfControlPoints[k]) && (closeDimension[k] == 0)) {
	                    		isOutOfBoundary = true;
	                    		break;
	                    	}
	                    	if (closeDimension[k] != 0) {
	                    		tmpPsi[k] %= psiLattice.getExtents()[k];
	                    	}
	                    } // for (int k = 0; k < nDims; k++)
	                    if (isOutOfBoundary) {
	                    	continue;
	                    }
	                    double coeff = 1.0;
	                    for (int k = 0; k < nDims; k++) {
	                    	coeff *= refinedLatticeCoefficients[k].get(off[k], offPsi[k]);
	                    } // for (int k = 0; k < nDims; k++)
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
	                    refinedLattice[tmp[0] + size[0]*tmp[1]] = sum;
	                }
	                else if (nDims == 3) {
	                	refinedLattice[tmp[0] + size[0] * tmp[1] + sliceSize * tmp[2]] = sum;
	                }
	                else {
	                	refinedLattice[tmp[0] + size[0] * tmp[1] + sliceSize * tmp[2] + xyzSize * tmp[3]] = sum;
	                }
	            } // for (int i = 0; i < 2 << (nDims - 1); i++)
	            
	            boolean isEvenIndex = false;
	            while (!isEvenIndex && it < sizeLength) {
	            	it++;
	            	idx[0] = it % size[0];
	 	            idx[1] = (it % sliceSize)/size[0];
	 	            if (nDims > 2) {
	 	            	idx[2] = (it % xyzSize) / sliceSize;
	 	            	if (nDims > 3) {
	 	            		idx[3] = it / xyzSize;
	 	            	}
	 	            }
	 	            isEvenIndex = true;
	 	            for (int i = 0; i < nDims; i++) {
	 	                if ((idx[i] % 2) != 0) {
	 	                	isEvenIndex = false;
	 	                }
	 	            } // for (int i = 0; i < nDims; i++)
	            } // while (!isEvenIndex && it < sizeLength)
	        } // (while it < sizeLength)
	        
	        psiLattice.disposeLocal();
	        psiLattice = null;
	        psiLattice = new ModelImage(ModelStorageBase.DOUBLE, size, "psiLattice");
	        try {
	        	psiLattice.importData(0, refinedLattice, true);
	        }
	        catch (IOException e) {
	        	MipavUtil.displayError("IOException on psiLattice.importData(0, refinedLattice, true)");
	        	return;
	        }
	    }
	    
	    private void setPhiLatticeParametricDomainParameters() {
	    	float localResolutions[] = new float[nDims];
	    	float localOrigin[] = new float[nDims];
	    	for (int i = 0; i < nDims; i++) {
	    		double domain = resolutions[i] * (extents[i] - 1);
	    		int totalNumberOfSpans = phiLattice.getExtents()[i];
	    		if (closeDimension[i] == 0) {
	    			totalNumberOfSpans -= splineOrder[i];
	    		}
	    		localResolutions[i] = (float)(domain / totalNumberOfSpans);
	    		localOrigin[i] = (float)(-0.5 * localResolutions[i] * (splineOrder[i] - 1));
	    	}
	    	float localOrigin2[] = new float[nDims];
	    	for (int i = 0; i < Math.min(nDims,3); i++) {
	    		for (int j = 0; j < Math.min(nDims,3); j++) {
	    			localOrigin2[i] += (direction[i][j] * localOrigin[j]);
	    		}
	    	}
	    	if (nDims > 3) {
	    		localOrigin2[3] = localOrigin[3];
	    	}
	    	
	    	for (int i = 0; i < nDims; i++) {
	    		localOrigin2[i] += origin[i];
	    	}
	    	
	    	int fileInfoLength = phiLattice.getFileInfo().length;
	    	for (int i = 0; i < fileInfoLength; i++) {
	    		phiLattice.getFileInfo(i).setOrigin(localOrigin2);
	    		phiLattice.getFileInfo(i).setResolutions(localResolutions);
	    	}
	    	
	    	for (int i = 0; i < Math.min(nDims,3); i++) {
                for (int j = 0; j < Math.min(nDims,3); j++) {
                    phiLattice.getMatrix().set(i, j, (float)direction[i][j]);
                }
            }
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

