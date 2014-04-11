	package gov.nih.mipav.model.algorithms.filters;
	
	
	import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmConvolver;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.GenerateGaussian;
import gov.nih.mipav.model.structures.BSplineKernelFunction;
import gov.nih.mipav.model.structures.CoxDeBoorBSplineKernelFunction;
import gov.nih.mipav.model.structures.ModelImage;
	








import gov.nih.mipav.model.structures.ModelStorageBase;

	import java.io.IOException;
import java.util.Vector;

import Jama.Matrix;
import WildMagic.LibFoundation.Mathematics.Vector3d;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJProgressBar;
	
	
	/**
	 
	 *
	 * @version  0.1 April 1, 2014
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
		private boolean constructPhiLattice;
		ModelImage phiLattice;
		private boolean constructPsiLattice;
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
		private double[] omegaLattice;
		private double[] deltaLattice;
	
		/**
	     * Constructor which sets the source and destination images
	     *
	     * @param  destImg   the destination image
	     * @param  srcImg    the source image
	     * @param  confidenceImage
	     * @param  maskFlag  the mask flag
	     */
	    public AlgorithmBSplineScatteredDataPointSetToImageFilter(ModelImage destImg, ModelImage srcImg) {
	        super(destImg, srcImg);
	    }
	    
	    public AlgorithmBSplineScatteredDataPointSetToImageFilter(int nDims) {
	        super(nDims);
	        splineOrder = new int[nDims];
	        int i;
	        for (i = 0; i < nDims; i++) {
	        	splineOrder[i] = 3;
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
	        constructPhiLattice = false;
	        phiLattice = null;
	        constructPsiLattice = true;
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
	        destImage = null;
	        srcImage = null;
	        super.finalize();
	    }
	    
	    public void setGenerateOutputImage(boolean generateOutputImage) {
	    	this.generateOutputImage = generateOutputImage;
	    }
	    
	    public void setNumberOfLevels(int nLevels) {
	    	for (int i = 0; i < nDims; i++) {
	    		this.numberOfLevels[i] = nLevels;
	    	}
	    	setNumberOfLevels(numberOfLevels);
	    }
	    
	    public void setNumberOfLevels(int[] numberOfLevels) {
	    	for (int i = 0; i < nDims; i++) {
	    		this.numberOfLevels[i] = numberOfLevels[i];
	    		if (numberOfLevels[i] > maximumNumberOfLevels) {
	    			maximumNumberOfLevels = numberOfLevels[i];
	    		}
	    	}
	    	if (maximumNumberOfLevels > 1) {
    			doMultiLevel = true;
    		}
    		else {
    			doMultiLevel = false;
    		}
	    	setSplineOrder(splineOrder);
	    }
	    
	    public void setSplineOrder(int sOrder) {
	    	int i, j, k;
	    	for (i = 0; i < nDims; i++) {
	    		this.splineOrder[i] = sOrder;
	    	}
	    	setSplineOrder(splineOrder);
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
	                Matrix QMat = RMat.solve(SMat);
	                refinedLatticeCoefficients[i] = QMat.extract(2,  SMat.getColumnDimension());
	            } // if (doMultiLevel)
	    	} // for (i = 0; i < nDims; i++)
	    }
	    
	    public void setSplineOrder(int[] splineOrder) {
	    	for (int i = 0; i < nDims; i++) {
	    		this.splineOrder[i] = splineOrder[i];
	    	}
	    }
	    
	    public void setNumberOfControlPoints(int[] numberOfControlPoints) {
	    	for (int i = 0; i < nDims; i++) {
	    		this.numberOfControlPoints[i] = numberOfControlPoints[i];
	    	}
	    }
	    
	    public void setPointWeights(Vector<Double> pointWeights) {
	    	usePointWeights = true;
	    	this.pointWeights.clear();
	    	for (int i = 0; i < pointWeights.size(); i++) {
	    		this.pointWeights.add(pointWeights.get(i));
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
	    	currentNumberOfControlPoints = numberOfControlPoints.clone();
	    	beforeThreadedGenerateData();
	    	threadedGenerateData();
	    	afterThreadedGenerateData();
	    	updatePointSet();
	    	
	    }
	    
	    private void beforeThreadedGenerateData() {
	        if (!isFittingComplete) {
	        	int latticeSize = 1;
	        	int size[] = new int[nDims];
	        	for (int i = 0; i < nDims; i++) {
	        		if (closeDimension[i] != 0) {
	        			size[i] = currentNumberOfControlPoints[i] - splineOrder[i];
	        		}
	        		else {
	        			size[i] = currentNumberOfControlPoints[i];
	        		}
	        		latticeSize *= size[i];
	        	}
	        	omegaLattice = new double[latticeSize];
	        	deltaLattice = new double[latticeSize];
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
	        double neighborhoodWeightArray[] = new double[neighborhoodWeightArrayLength];
	        double p[] = new double[nDims];
	        double r[] = new double[nDims];
	        for (int i = 0; i < nDims; i++) {
	        	r[i] = (double)(currentNumberOfControlPoints[i] - splineOrder[i])/((size[i] - 1.0) * resolutions[i]);
	        }
	        
	        for (int n = 0; n < pointLocation.size(); n++) {
	        	double point[] = new double[nDims];
	        	point[0] = pointLocation.get(n).X;
	        	point[1] = pointLocation.get(n).Y;
	        	if (nDims > 2) {
	        	    point[2] = pointLocation.get(n).Z;
	        	}
	        	for (int i = 0; i < nDims; i++) {
	        	    int totalNumberOfSpans = currentNumberOfControlPoints[i] - splineOrder[i];
	        	    
	        	    p[i] = (point[i] - origin[i]) * r[i];
	        	    if (Math.abs(p[i] - totalNumberOfSpans) <= BSplineEpsilon) {
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
	        	    	idx[2] = itw / neighborhoodWeightArraySliceSize;
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
	        	    	idx[2] = itw / neighborhoodWeightArraySliceSize;
	        	    }
	        	    for (int i = 0; i < nDims; i++) {
	        	        idx[i] += (int)(p[i]);
	        	        if (closeDimension[i] != 0) {
	        	        	idx[i] %= size[i];
	        	        }
	        	    } // for (int i = 0; i < nDims; i++)
	        	    double wc = pointWeights.get(n);
	        	    double t = neighborhoodWeightArray[itw];
	        	    omegaLattice[itw] = omegaLattice[itw] + wc * t * t;
	        	    double data = inputPointData.get(n);
	        	    data *= (t * t * t * wc / w2Sum);
	        	    deltaLattice[itw] = deltaLattice[itw] + data;
	        	} // for (int itw = 0; itw < neighborhoodWeightArrayLength; itw++)
	        } // for (int n = 0; n < pointLocation.size(); n++)
	    }
	    
	    private void threadedGenerateDataForReconstruction() {
	        double collapsedPhiLattices[][] = new double[nDims + 1][];
	        int collapsedPhiLatticeIndex[][] = new int[nDims+1][];
	        for (int i = 0; i <= nDims; i++) {
	            int size = 1;
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
	    }
	    
	    private void collapsePhiLattice(double[] lattice, int latticeIndex[], double[] collapsedLattice, int collapsedLatticeIndex[],
	    		double u, int dimension) {
	    	int idx[] = new int[3];
	    	int sliceSize = latticeIndex[0] * latticeIndex[1];
 	    	for (int it = 0; it < collapsedLattice.length; it++) {
	    		if (collapsedLatticeIndex.length == 1) {
	    			idx[0] = it;
	    			idx[1] = 0;
	    			idx[2] = 0;
	    		}
	    		else if (collapsedLatticeIndex.length == 2) {
	    			idx[0] = it % collapsedLatticeIndex[0];
	    			idx[1] = it / collapsedLatticeIndex[0];
	    			idx[2] = 0;
	    		}
	    		else {
	    		    idx[0] = it % collapsedLatticeIndex[0];
        	        idx[1] = (it % sliceSize) / collapsedLatticeIndex[0];
        	    	idx[2] = it / sliceSize;
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
	    		    int position = idx[0] + idx[1] * latticeIndex[0] + idx[2] * sliceSize;
	    		    data += (lattice[position] * B);
	    		} // for (int i = 0; i < splineOrder[dimension] + 1; i++)
	    		collapsedLattice[it] = data;
	    	}
	    }
	    
	    private void afterThreadedGenerateData() {
	    	
	    }
	    
	    private void updatePointSet() {
	    	
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

