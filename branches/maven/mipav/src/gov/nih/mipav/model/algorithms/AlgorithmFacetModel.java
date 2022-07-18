package gov.nih.mipav.model.algorithms;

import java.io.IOException;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewUserInterface;


// These routines implement the text in: 1.) Computer and Robot Vision, Volume I, Robert M. Haralick and Linda G. Shapiro,
// Addison-Wesley Publishing Company, Inc., 1992, Chapter 8, The Facet Model, pp. 371-452.
// 2.) Integrated Directional Derivative Gradient Operator by Oscar A. Zuniga and Robert M. Haralick, IEEE Transactions
// on Systems, Man, and Cybernetics, Vol. SMC-17, No. 3, May/June 1987, pp. 508-517.
public  class AlgorithmFacetModel extends AlgorithmBase {
	// ~ Static fields/initializers
	// Only FACET_BASED_PEAK_NOISE_REMOVAL, ITERATED_FACET_MODEL, and GRADIENT_BASED_FACET_EDGE_DETECTION
	// can use 3 by 3 masks.
	public static final int FACET_BASED_PEAK_NOISE_REMOVAL = 1;
	public static final int ITERATED_FACET_MODEL = 2;
	public static final int GRADIENT_BASED_FACET_EDGE_DETECTION = 3;
	public static final int ZERO_CROSSING_EDGE_DETECTOR = 4;
	// INTEGRATED_DIRECTIONAL_DERIVATIVE uses only blockSide = 5 or 7.
	public static final int INTEGRATED_DIRECTIONAL_DERIVATIVE_ANGLE = 5;
	public static final int INTEGRATED_DIRECTIONAL_DERIVATIVE_MAGNITUDE = 6;
	public static final int CORNER_DETECTOR = 7;
	
    // -------------------------------------------------------------------------------------
	
	//~ Instance fields ------------------------------------------------------------------------------------------------
	// Square blocks of blockSide by blockSide pixels used.  BlockSide must be an odd integer.
	private int blockSide;
	
	// Probability of rejecting a true hypothesis.  A reasonable range for alpha is .01 to .05
	// Used only in facetBasedPeakNoiseRemoval and gradientBasedFacetEdgeDetection
    private double alpha;
    
    // Tells which of the facet model routines to use
    private int routine;
    
    // Used only in cornerDetector
    private double gradientDirectionThreshold = 20.0;
    
    private int xDim;
	private int yDim;
	private double buffer[];
	private double result[];
	private int blockHalf;
    
  //~ Constructors ---------------------------------------------------------------------------------------------------
    public AlgorithmFacetModel(ModelImage srcImg, int routine, int blockSide, double alpha,
    		double gradientDirectionThreshold) {
    	super(null, srcImg);
    	this.routine = routine;
    	this.blockSide = blockSide;
    	this.alpha = alpha;
    	this.gradientDirectionThreshold = gradientDirectionThreshold;
    }
    
    public AlgorithmFacetModel(ModelImage destImg, ModelImage srcImg, int routine, int blockSide, double alpha,
    		double gradientDirectionThreshold) {
    	super(destImg, srcImg);
    	this.routine = routine;
    	this.blockSide = blockSide;
    	this.alpha = alpha;
    	this.gradientDirectionThreshold = gradientDirectionThreshold;
    }
    
    /**
     * Starts the program.
     */
    public void runAlgorithm() {
    	int zDim;
    	int tDim;
    	int nDims;
    	int sliceSize;
    	int x, y, z, t;
    	blockHalf = (blockSide - 1)/2;
    	boolean test = false;
    	
    	if (test) {
    	    testBivariateCubicCoefficients5By5();
    	    setCompleted(false);
    	    return;
    	}

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
        
        fireProgressStateChanged(0, srcImage.getImageName(), "Facet Model ...");
        
        nDims = srcImage.getNDims();
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        if (nDims > 2) {
        	zDim = srcImage.getExtents()[2];
        }
        else {
        	zDim = 1;
        }
        if (nDims > 3) {
        	tDim = srcImage.getExtents()[3];
        }
        else {
        	tDim = 1;
        }
        sliceSize = xDim * yDim;
        buffer = new double[sliceSize];
        result = new double[sliceSize];
        
        for (t = 0; t < tDim; t++) {
        	for (z = 0; z < zDim; z++) {
        		try {
        			srcImage.exportData((z + t*zDim)*sliceSize, sliceSize, buffer);
        		}
        		catch(IOException e) {
        			MipavUtil.displayError("IOException " + e + " on srcImage.exportData");
        			setCompleted(false);
        			return;
        		}
        		for (x = 0; x < xDim; x++) {
        			    for (y = 0; y < blockHalf; y++) {
        			        result[y*xDim + x] = buffer[y*xDim+x];
        			    }
        			    for (y = yDim - 1; y > yDim-1-blockHalf; y--) {
        			        result[y*xDim + x] = buffer[y*xDim+x];
        			    }
        		}
        		
        		for (y = blockHalf; y < yDim-blockHalf; y++) {
        			for (x = 0; x < blockHalf; x++) {
        			    result[y*xDim + x] = buffer[y*xDim + x];
        			}
        			for (x = xDim-1; x > xDim-1-blockHalf; x--) {
        			    result[y*xDim + x] = buffer[y*xDim + x];
        			}
        		}
        	    switch(routine) {
        	    case FACET_BASED_PEAK_NOISE_REMOVAL:
        	    	facetBasedPeakNoiseRemoval();
        	    	break;
        	    case ITERATED_FACET_MODEL:
        	    	iteratedFacetModel();
        	    	break;
        	    case GRADIENT_BASED_FACET_EDGE_DETECTION:
        	    	gradientBasedFacetEdgeDetection();
        	    	break;
        	    case ZERO_CROSSING_EDGE_DETECTOR:
        	    	zeroCrossingEdgeDetector();
        	    	break;
        	    case INTEGRATED_DIRECTIONAL_DERIVATIVE_ANGLE:
        	    case INTEGRATED_DIRECTIONAL_DERIVATIVE_MAGNITUDE:
        	    	integratedDirectionalDerivative();
        	    	break;
        	    case CORNER_DETECTOR:
        	    	cornerDetector();
        	    	break;
        	    }
        	
        		if (destImage != null) {
        			try {
        			    destImage.importData((z + t*zDim)*sliceSize, result, false);
        			}
        			catch(IOException e) {
        				MipavUtil.displayError("IOException " + e + " on destImage.importData");
        				setCompleted(false);
        				return;
        			}
        		}
        		else {
        			try {
        			    srcImage.importData((z + t*zDim)*sliceSize, result, false);
        			}
        			catch(IOException e) {
        				MipavUtil.displayError("IOException " + e + " on srcImage.importData");
        				setCompleted(false);
        				return;
        			}
        		}
        	} // for (z = 0; z < zDim; z++)
        } // for (t = 0; t < tDim; t++)
        if (destImage != null) {
        	destImage.calcMinMax();
        }
        else {
        	srcImage.calcMinMax();
        }
        setCompleted(true);
        return;
    }
    
    private void facetBasedPeakNoiseRemoval() {
    	// g(x,y) = a*x + beta*y + gamma
    	// tStatistic has N-3 degrees of freedom
    	int x, y;
    	int delx; int dely;
    	double a;
    	double beta;
    	double gamma;
    	double delxg;
    	int delx2;
    	double delyg;
    	int dely2;
    	double gSum;
    	double diff;
    	double epsilonSquared;
    	double tStatistic;
    	Statistics stat;
    	int N = blockSide*blockSide - 1;
    	double degreesOfFreedom = N - 3;
    	double answer[] = new double[1];
        for (y = blockHalf; y < yDim-blockHalf; y++) {
        	for (x = blockHalf; x < xDim-blockHalf; x++) {
        		delxg = 0.0;
        		delx2 = 0;
        		delyg = 0.0;
        		dely2 = 0;
        		gSum = 0.0;
        	    for (dely = -blockHalf; dely <= blockHalf; dely++) {
        	    	for (delx = -blockHalf; delx <= blockHalf; delx++) {
        	    		if ((dely != 0) || (delx != 0)) {
	        	    	    delxg += (delx * buffer[(y+dely)*xDim + (x + delx)]);
	        	    	    delx2 += (delx * delx);
	        	    	    delyg += (dely * buffer[(y+dely)*xDim + (x + delx)]);
	        	    	    dely2 += (dely * dely);
	        	    	    gSum += buffer[(y + dely)*xDim + (x + delx)];
        	    		} // if ((dely != 0) || (delx != 0))
        	    	} // for (delx = -blockHalf; delx <= blockHalf; delx++)
        	    } // for (dely = -blockHalf; dely <= blockHalf; dely++)
        	    a = delxg/delx2;
        	    beta = delyg/dely2;
        	    gamma = gSum/N;
        	    epsilonSquared = 0.0;
        	    for (dely = -blockHalf; dely <= blockHalf; dely++) {
        	    	for (delx = -blockHalf; delx <= blockHalf; delx++) {
        	    		if ((dely != 0) || (delx != 0)) {
	        	    	    diff = a*delx + beta*dely + gamma - buffer[(y+dely)*xDim + (x+delx)];
	        	    	    epsilonSquared += (diff * diff);
        	    		} // if ((dely != 0) || (delx != 0))
        	    	} // for (delx = -blockHalf; delx <= blockHalf; delx++)
        	    } // for (dely = -blockHalf; dely <= blockHalf; dely++)
        	    tStatistic = (buffer[y*xDim+x] - gamma)/Math.sqrt((1.0 + 1.0/N)*epsilonSquared);
        	    stat = new Statistics(Statistics.STUDENTS_T_DISTRIBUTION_PROBABILITY_DENSITY_FUNCTION, tStatistic, degreesOfFreedom,
        	    		answer);
        	    stat.run();
        	    if ((answer[0] < alpha/2.0) || (answer[0] > 1.0 - alpha/2.0)) {
        	    	result[y*xDim + x] = gamma;
        	    }
        	    else {
        	    	result[y*xDim+x] = buffer[y*xDim+x];
        	    }
        	} // for (x = blockHalf; x < xDim-blockHalf; x++)
        } // for (y = blockHalf; y < yDim-blockHalf; y++)
    } // private void facetBasedPeakNoiseRemoval()
    
    private void iteratedFacetModel() {
    	// g(x,y) = a*x + beta*y + gamma
    	int x, y;
    	int delx; int dely;
    	double a[][] = new double[yDim-2*blockHalf][xDim-2*blockHalf];
    	double beta[][] = new double[yDim-2*blockHalf][xDim-2*blockHalf];
    	double gamma[][] = new double[yDim-2*blockHalf][xDim-2*blockHalf];
    	double delxg;
    	int delx2;
    	double delyg;
    	int dely2;
    	double gSum;
    	double diff;
    	double epsilonSquared[][] = new double[yDim-2*blockHalf][xDim-2*blockHalf];
    	int N = blockSide*blockSide - 1;
    	double lowestEpsilonSquared;
    	int lowestDelx;
    	int lowestDely;
    	
        for (y = blockHalf; y < yDim-blockHalf; y++) {
        	for (x = blockHalf; x < xDim-blockHalf; x++) {
        		delxg = 0.0;
        		delx2 = 0;
        		delyg = 0.0;
        		dely2 = 0;
        		gSum = 0.0;
        	    for (dely = -blockHalf; dely <= blockHalf; dely++) {
        	    	for (delx = -blockHalf; delx <= blockHalf; delx++) {
        	    		if ((dely != 0) || (delx != 0)) {
	        	    	    delxg += (delx * buffer[(y+dely)*xDim + (x + delx)]);
	        	    	    delx2 += (delx * delx);
	        	    	    delyg += (dely * buffer[(y+dely)*xDim + (x + delx)]);
	        	    	    dely2 += (dely * dely);
	        	    	    gSum += buffer[(y + dely)*xDim + (x + delx)];
        	    		} // if ((dely != 0) || (delx != 0))
        	    	} // for (delx = -blockHalf; delx <= blockHalf; delx++)
        	    } // for (dely = -blockHalf; dely <= blockHalf; dely++)
        	    a[y-blockHalf][x-blockHalf] = delxg/delx2;
        	    beta[y-blockHalf][x-blockHalf] = delyg/dely2;
        	    gamma[y-blockHalf][x-blockHalf] = gSum/N;
        	    for (dely = -blockHalf; dely <= blockHalf; dely++) {
        	    	for (delx = -blockHalf; delx <= blockHalf; delx++) {
        	    		if ((dely != 0) || (delx != 0)) {
	        	    	    diff = a[y-blockHalf][x-blockHalf]*delx + beta[y-blockHalf][x-blockHalf]*dely 
	        	    	    		+ gamma[y-blockHalf][x-blockHalf] - buffer[(y+dely)*xDim + (x+delx)];
	        	    	    epsilonSquared[y-blockHalf][x-blockHalf] += (diff * diff);
        	    		} // if ((dely != 0) || (delx != 0))
        	    	} // for (delx = -blockHalf; delx <= blockHalf; delx++)
        	    } // for (dely = -blockHalf; dely <= blockHalf; dely++)	
        	} // for (x = blockHalf; x < xDim-blockHalf; x++) 
        } // for (y = blockHalf; y < yDim-blockHalf; y++)
        for (y = blockHalf; y < yDim-blockHalf; y++) {
        	for (x = blockHalf; x < xDim-blockHalf; x++) {
        		lowestEpsilonSquared = Double.MAX_VALUE;
        		lowestDelx = 0;
        		lowestDely = 0;
        		for (dely = -blockHalf; dely <= blockHalf; dely++) {
        	    	for (delx = -blockHalf; delx <= blockHalf; delx++) {
        	    		if ((y+dely-blockHalf >= 0) && (y+dely-blockHalf < yDim - 2*blockHalf) &&
        	    			(x+delx-blockHalf >= 0) && (x+delx-blockHalf < xDim - 2*blockHalf)) {
	        	    		if (epsilonSquared[y+dely-blockHalf][x+delx-blockHalf] < lowestEpsilonSquared) {
	        	    			lowestEpsilonSquared = epsilonSquared[y+dely-blockHalf][x+delx-blockHalf];
	        	    			lowestDelx = delx;
	        	    			lowestDely = dely;
	        	    		}
        	    		} // if ((y+dely-blockHalf >= 0) && (y+dely-blockHalf < yDim - 2*blockHalf) &&
        	    	} // for (delx = -blockHalf; delx <= blockHalf; delx++)
        	    } // for (dely = -blockHalf; dely <= blockHalf; dely++)
        		result[y*xDim+x] = a[y+lowestDely-blockHalf][x+lowestDelx-blockHalf]*(-lowestDelx) 
        				+ beta[y+lowestDely-blockHalf][x+lowestDelx-blockHalf]*(-lowestDely)
        				+ gamma[y+lowestDely-blockHalf][x+lowestDelx-blockHalf];
        	} // for (x = blockHalf; x < xDim-blockHalf; x++)
        } // for (y = blockHalf; y < yDim-blockHalf; y++)
    }
    
    private void gradientBasedFacetEdgeDetection() {
    	// g(x,y) = a*x + beta*y + gamma
    	int x, y;
    	int delx; int dely;
    	double a[][] = new double[yDim-2*blockHalf][xDim-2*blockHalf];
    	double beta[][] = new double[yDim-2*blockHalf][xDim-2*blockHalf];
    	double gamma;
    	double delxg;
    	int delx2;
    	double delyg;
    	int dely2;
    	double gSum;
    	double diff;
    	double epsilonSquaredTotal = 0;
    	double epsilonSquaredAverage;
    	double sigmaSquared;
    	int numeratorMultiplier = 0;
    	double multiplier;
    	double chiSquared;
    	Statistics stat;
    	double degreesOfFreedom = 2.0;
    	double answer[] = new double[1];
    	int N = blockSide*blockSide - 1;
    	
        for (y = blockHalf; y < yDim-blockHalf; y++) {
        	for (x = blockHalf; x < xDim-blockHalf; x++) {
        		delxg = 0.0;
        		delx2 = 0;
        		delyg = 0.0;
        		dely2 = 0;
        		gSum = 0.0;
        	    for (dely = -blockHalf; dely <= blockHalf; dely++) {
        	    	for (delx = -blockHalf; delx <= blockHalf; delx++) {
        	    		if ((dely != 0) || (delx != 0)) {
	        	    	    delxg += (delx * buffer[(y+dely)*xDim + (x + delx)]);
	        	    	    delx2 += (delx * delx);
	        	    	    delyg += (dely * buffer[(y+dely)*xDim + (x + delx)]);
	        	    	    dely2 += (dely * dely);
	        	    	    gSum += buffer[(y + dely)*xDim + (x + delx)];
        	    		} // if ((dely != 0) || (delx != 0))
        	    	} // for (delx = -blockHalf; delx <= blockHalf; delx++)
        	    } // for (dely = -blockHalf; dely <= blockHalf; dely++)
        	    a[y-blockHalf][x-blockHalf] = delxg/delx2;
        	    beta[y-blockHalf][x-blockHalf] = delyg/dely2;
        	    gamma = gSum/N;
        	    for (dely = -blockHalf; dely <= blockHalf; dely++) {
        	    	for (delx = -blockHalf; delx <= blockHalf; delx++) {
        	    		if ((dely != 0) || (delx != 0)) {
	        	    	    diff = a[y-blockHalf][x-blockHalf]*delx + beta[y-blockHalf][x-blockHalf]*dely 
	        	    	    		+ gamma - buffer[(y+dely)*xDim + (x+delx)];
	        	    	    epsilonSquaredTotal += (diff * diff);
        	    		} // if ((dely != 0) || (delx != 0))
        	    	} // for (delx = -blockHalf; delx <= blockHalf; delx++)
        	    } // for (dely = -blockHalf; dely <= blockHalf; dely++)	
        	} // for (x = blockHalf; x < xDim-blockHalf; x++) 
        } // for (y = blockHalf; y < yDim-blockHalf; y++)
        epsilonSquaredAverage = epsilonSquaredTotal/((xDim-2*blockHalf)*(yDim-2*blockHalf));
        sigmaSquared = epsilonSquaredAverage/((2*blockHalf+1)*(2*blockHalf+1)-3);
        for (dely = -blockHalf; dely <= blockHalf; dely++) {
        	for (delx = -blockHalf; delx <= blockHalf; delx++) {
        		numeratorMultiplier += 2*delx*delx;
        	}
        }
        multiplier = numeratorMultiplier/sigmaSquared;
        for (y = blockHalf; y < yDim-blockHalf; y++) {
        	for (x = blockHalf; x < xDim-blockHalf; x++) {
        	    chiSquared = multiplier*(a[y-blockHalf][x-blockHalf]*a[y-blockHalf][x-blockHalf] +
        	    		                 beta[y-blockHalf][x-blockHalf]*beta[y-blockHalf][x-blockHalf]);
        	    stat = new Statistics(Statistics.CHI_SQUARED_CUMULATIVE_DISTRIBUTION_FUNCTION, chiSquared, degreesOfFreedom,
        	    		answer);
        	    stat.run();
        	    if (answer[0] >= 1.0 - alpha) {
        	    	result[y*xDim+x] = 1;
        	    }
        	    else {
        	    	result[y*xDim+x] = 0;
        	    }
        	} // for (x = blockHalf; x < xDim-blockHalf; x++)
        } // for (y = blockHalf; y < yDim-blockHalf; y++)	
    }
    
    private boolean isEdgePixel(double k1, double k2, double k3, double k4, double k5, double k6, double k7,
    		double k8, double k9, double k10, double sinAngle, double cosAngle, double rho[]) {
    	// If for some rho, where abs(pho) is slightly smaller than the length of a side of a pixel,
    	// thirdDirectionalDerivative < 0.0, secondDirectionalDerivative = 0, and firstDirectionalDerivative != 0,
    	// we have discovered a negatively sloped zero crossing of the estimated second directional derivative in
    	// the direction of the gradient.  We mark the center pixel of the neighborhood  as an edge pixel,
    	// and if required we make a note of the subpixel location of the zero crossing.
    	double thirdDirectionalDerivative;
    	double firstDirectionalDerivative;
    	// Value of rho that makes the second directional derivative equal to zero.
    	rho[0] = -(k4*sinAngle*sinAngle + k5*sinAngle*cosAngle + k6*cosAngle*cosAngle)/
    			(3.0*(k7 * sinAngle * sinAngle * sinAngle + k8 * sinAngle *sinAngle * cosAngle +
    	    			k9 *sinAngle * cosAngle * cosAngle + k10 * cosAngle * cosAngle * cosAngle));
    	if ((rho[0] <= -1.0) || (rho[0] >= 1.0)) {
    		return false;
    	}
    	thirdDirectionalDerivative = 6.0*(k7*sinAngle*sinAngle*sinAngle + k8*sinAngle*sinAngle*cosAngle
    			+ k9*sinAngle*cosAngle*cosAngle + k10*cosAngle*cosAngle*cosAngle);
    	if (thirdDirectionalDerivative >= 0.0) {
    		return false;
    	}
    	firstDirectionalDerivative = (k2 + 2.0*k4*rho[0]*sinAngle + k5*rho[0]*cosAngle + 3.0*k7*rho[0]*rho[0]*sinAngle*sinAngle +
    			2.0 * k8 * rho[0] * rho[0] * sinAngle * cosAngle + k9 * rho[0] * rho[0] * cosAngle * cosAngle)*sinAngle
    			+(k3 + k5 * rho[0] * sinAngle + 2.0 * k6 * rho[0] * cosAngle + k8 * rho[0] * rho[0] * sinAngle * sinAngle
    			+ 2.0 * k9 * rho[0] * rho[0] * cosAngle * sinAngle + 3.0 * k10 * rho[0] * rho[0] * cosAngle * cosAngle)*cosAngle;
    	if (firstDirectionalDerivative != 0) {
    		return true;
    	}
    	else {
    		return false;
    	}
    }
    
    @SuppressWarnings("unused")
	private void directionalDerivatives(double k1, double k2, double k3, double k4, double k5, double k6, double k7,
    		double k8, double k9, double k10, double sinAngle, double cosAngle, double rho, double firstDirectionalDerivative[],
    		double secondDirectionalDerivative[], double thirdDirectionalDerivative[]) {
    	// If for some rho, where abs(pho) is slightly smaller than the length of a side of a pixel,
    	// thirdDirectionalDerivative < 0.0, secondDirectionalDerivative = 0, and firstDirectionalDerivative != 0,
    	// we have discovered a negatively sloped zero crossing of the estimated second directional derivative in
    	// the direction of the gradient.  We mark the center pixel of the neighborhood  as an edge pixel,
    	// and if required we make a note of the subpixel location of the zero crossing.
    	firstDirectionalDerivative[0] = (k2 + 2.0*k4*rho*sinAngle + k5*rho*cosAngle + 3.0*k7*rho*rho*sinAngle*sinAngle +
    			2.0 * k8 * rho * rho * sinAngle * cosAngle + k9 * rho * rho * cosAngle * cosAngle)*sinAngle
    			+(k3 + k5 * rho * sinAngle + 2.0 * k6 * rho * cosAngle + k8 * rho * rho * sinAngle * sinAngle
    			+ 2.0 * k9 * rho * rho * cosAngle * sinAngle + 3.0 * k10 * rho * rho * cosAngle * cosAngle)*cosAngle;
    	secondDirectionalDerivative[0] = 6.0*(k7 * sinAngle * sinAngle * sinAngle + k8 * sinAngle *sinAngle * cosAngle +
    			k9 *sinAngle * cosAngle * cosAngle + k10 * cosAngle * cosAngle * cosAngle)*rho
    			+2.0*(k4*sinAngle*sinAngle + k5*sinAngle*cosAngle + k6*cosAngle*cosAngle);
    	thirdDirectionalDerivative[0] = 6.0*(k7*sinAngle*sinAngle*sinAngle + k8*sinAngle*sinAngle*cosAngle
    			+ k9*sinAngle*cosAngle*cosAngle + k10*cosAngle*cosAngle*cosAngle);
    }
    
    private void testBivariateCubicCoefficients5By5() {
    	// Kernels for directly estimating the coefficients k1,...,k10 of the bivariate cubic f(y,x) = k1 + k2*y + k3*x
    	// + k4*y*y + k5*x*y + k6*x*x + k7*y*y*y + k8*x*y*y + k9*x*x*y + k10*x*x*x
    	// Verified that Figure 8.9 in Computer and Robot Vision for 5 by 5 cubic facet coefficients matches 
    	// results from equations in Integrated Directional Derivative Gradient Operator appendix.
    	blockHalf = 2;
    	int delx;
    	int dely;
    	double denom;
    	long R0 = 0;
    	long R1 = 0;
    	long R2 = 0;
    	long R3 = 0;
    	long C0 = 0;
    	long C1 = 0;
    	long C2 = 0;
        long C3 = 0;
        long dely2;
        long delx2;
        long dely4;
        long delx4;
        long G;
        long A;
        long B;
        long Q;
        long T;
        long U;
        long V;
        long W;
        long Z;
        long TR1;
        long QC1;
        long WR2;
        long UC1;
        long ZR1;
        long VC2;
        ViewUserInterface UI = ViewUserInterface.getReference();
    	
    	denom = 0.0;
    	for (dely = -blockHalf; dely <= blockHalf; dely++) {
    		for (delx = -blockHalf; delx <= blockHalf; delx++) {
    		    UI.setDataText("k5_numerator["+dely+"]["+delx+"] = " + (delx*dely) + "\n");
    		    denom += delx * delx * dely * dely;
    		} // for (delx = -blockHalf; delx <= blockHalf; delx++)
    	} // for (dely = -blockHalf; dely <= blockHalf; dely++)
    	UI.setDataText("k5_denominator = " + denom + "\n");
    	for (dely = -blockHalf; dely <= blockHalf; dely++) {
    		dely2 = dely * dely;
    		dely4 = dely2 * dely2;
    		R0 += 1;
    		R1 += dely2;
    		R2 += dely4;
    		R3 += dely4 * dely2;
    	} // for (dely = -blockHalf; dely <= blockHalf; dely++)
		for (delx = -blockHalf; delx <= blockHalf; delx++) {
			delx2 = delx * delx;
			delx4 = delx2 * delx2;
		    C0 += 1;
		    C1 += delx2;
		    C2 += delx4;
		    C3 += delx4 * delx2;
		} // for (delx = -blockHalf; delx <= blockHalf; delx++)
    	G = R0*R2*C0*C2 - R1*R1*C1*C1;
    	A = R1*R3*C0*C2 - R2*R2*C1*C1;
    	B = R0*R2*C1*C3 - R1*R1*C2*C2;
    	Q = C0*(R0*R2 - R1*R1);
    	T = R0*(C0*C2 - C1*C1);
    	U = C0*(R1*R3 - R2*R2);
    	V = C1*(R0*R2 - R1*R1);
    	W = R1*(C0*C2 - C1*C1);
    	Z = R0*(C1*C3 - C2*C2);
    	TR1 = T * R1;
    	QC1 = Q * C1;
    	WR2 = W * R2;
    	UC1 = U * C1;
    	ZR1 = Z * R1;
    	VC2 = V * C2;
    	
    	for (dely = -blockHalf; dely <= blockHalf; dely++) {
    		dely2 = dely * dely;
    		for (delx = -blockHalf; delx <= blockHalf; delx++) {
    			delx2 = delx * delx;
    		    UI.setDataText("k1_numerator["+dely+"]["+delx+"] = " + ((G - TR1*dely2 - QC1*delx2)/700) + "\n");
    		} // for (delx = -blockHalf; delx <= blockHalf; delx++)
    	} // for (dely = -blockHalf; dely <= blockHalf; dely++)
    	UI.setDataText("k1_denominator = " + (Q*T/700) + "\n");
    	for (dely = -blockHalf; dely <= blockHalf; dely++) {
    		dely2 = dely * dely;
    		for (delx = -blockHalf; delx <= blockHalf; delx++) {
    			delx2 = delx * delx;
    		    UI.setDataText("k2_numerator["+dely+"]["+delx+"] = " + ((A - WR2*dely2 - UC1*delx2)*dely/1200) + "\n");
    		} // for (delx = -blockHalf; delx <= blockHalf; delx++)
    	} // for (dely = -blockHalf; dely <= blockHalf; dely++)
        UI.setDataText("k2_denominator = " + (U*W/1200) + "\n");
        for (dely = -blockHalf; dely <= blockHalf; dely++) {
    		dely2 = dely * dely;
    		for (delx = -blockHalf; delx <= blockHalf; delx++) {
    			delx2 = delx * delx;
    		    UI.setDataText("k3_numerator["+dely+"]["+delx+"] = " + ((B - ZR1*dely2 - VC2*delx2)*delx/1200) + "\n");
    		} // for (delx = -blockHalf; delx <= blockHalf; delx++)
    	} // for (dely = -blockHalf; dely <= blockHalf; dely++)
        UI.setDataText("k3_denominator = " + (V*Z/1200) + "\n");
        for (dely = -blockHalf; dely <= blockHalf; dely++) {
    		dely2 = dely * dely;
    		for (delx = -blockHalf; delx <= blockHalf; delx++) {
    			delx2 = delx * delx;
    		    UI.setDataText("k4_numerator["+dely+"]["+delx+"] = " + ((R0*dely2 - R1)/5) + "\n");
    		} // for (delx = -blockHalf; delx <= blockHalf; delx++)
    	} // for (dely = -blockHalf; dely <= blockHalf; dely++)
    	UI.setDataText("k4_denominator = " + (Q/5) + "\n");
    	for (dely = -blockHalf; dely <= blockHalf; dely++) {
    		for (delx = -blockHalf; delx <= blockHalf; delx++) {
    			delx2 = delx * delx;
    		    UI.setDataText("k6_numerator["+dely+"]["+delx+"] = " + ((C0*delx2 - C1)/5) + "\n"); 
    		} // for (delx = -blockHalf; delx <= blockHalf; delx++)
    	} // for (dely = -blockHalf; dely <= blockHalf; dely++)
    	UI.setDataText("k6_denominator = " + (T/5) + "\n");
    	for (dely = -blockHalf; dely <= blockHalf; dely++) {
    		dely2 = dely * dely;
    		for (delx = -blockHalf; delx <= blockHalf; delx++) {
    		    UI.setDataText("k7_numerator["+dely+"]["+delx+"] = " + ((R1*dely2 - R2)*dely/12) + "\n");
    		} // for (delx = -blockHalf; delx <= blockHalf; delx++)
    	} // for (dely = -blockHalf; dely <= blockHalf; dely++)
    	UI.setDataText("k7_denominator = " + (U/12) + "\n");
    	for (dely = -blockHalf; dely <= blockHalf; dely++) {
    		dely2 = dely * dely;
    		for (delx = -blockHalf; delx <= blockHalf; delx++) {
    		    UI.setDataText("k8_numerator["+dely+"]["+delx+"] = " + ((R0*dely2 - R1)*delx/5) + "\n");
    		} // for (delx = -blockHalf; delx <= blockHalf; delx++)
    	} // for (dely = -blockHalf; dely <= blockHalf; dely++)
    	UI.setDataText("k8_denominator = " + (V/5) + "\n");
    	for (dely = -blockHalf; dely <= blockHalf; dely++) {
    		for (delx = -blockHalf; delx <= blockHalf; delx++) {
    			delx2 = delx * delx;
    		    UI.setDataText("k9_numerator["+dely+"]["+delx+"] = " + ((C0*delx2 - C1)*dely/5) + "\n");
    		} // for (delx = -blockHalf; delx <= blockHalf; delx++)
    	} // for (dely = -blockHalf; dely <= blockHalf; dely++)
    	UI.setDataText("k9_denominator = " + (W/5) + "\n");
    	for (dely = -blockHalf; dely <= blockHalf; dely++) {
    		for (delx = -blockHalf; delx <= blockHalf; delx++) {
    			delx2 = delx * delx;
    		    UI.setDataText("k10_numerator[" + dely + "][" + delx + "] = " + ((C1*delx2 - C2)*delx/12) + "\n");
    		} // for (delx = -blockHalf; delx <= blockHalf; delx++)
    	} // for (dely = -blockHalf; dely <= blockHalf; dely++)
    	UI.setDataText("k10_denominator = " + (Z/12) + "\n");
    }
    
    private void bivariateCubicCoefficients(int x, int y, double k1[], double k2[], double k3[], double k4[],
    		double k5[], double k6[], double k7[], double k8[], double k9[], double k10[], 
    		double sinAngle[], double cosAngle[], double gradientAngle[]) {
    	// Kernels for directly estimating the coefficients k1,...,k10 of the bivariate cubic f(y,x) = k1 + k2*y + k3*x
    	// + k4*y*y + k5*x*y + k6*x*x + k7*y*y*y + k8*x*y*y + k9*x*x*y + k10*x*x*x
    	int delx;
    	int dely;
    	double num;
    	double denom;
    	long R0 = 0;
    	long R1 = 0;
    	long R2 = 0;
    	long R3 = 0;
    	long C0 = 0;
    	long C1 = 0;
    	long C2 = 0;
        long C3 = 0;
        long dely2;
        long delx2;
        long dely4;
        long delx4;
        long G;
        long A;
        long B;
        long Q;
        long T;
        long U;
        long V;
        long W;
        long Z;
        long TR1;
        long QC1;
        long WR2;
        long UC1;
        long ZR1;
        long VC2;
        double buf;
    	
    	num = 0.0;
    	denom = 0.0;
    	for (dely = -blockHalf; dely <= blockHalf; dely++) {
    		for (delx = -blockHalf; delx <= blockHalf; delx++) {
    		    num += delx * dely *buffer[(y+dely)*xDim+x+delx];
    		    denom += delx * delx * dely * dely;
    		} // for (delx = -blockHalf; delx <= blockHalf; delx++)
    	} // for (dely = -blockHalf; dely <= blockHalf; dely++)
    	k5[0] = num/denom;
    	for (dely = -blockHalf; dely <= blockHalf; dely++) {
    		dely2 = dely * dely;
    		dely4 = dely2 * dely2;
    		R0 += 1;
    		R1 += dely2;
    		R2 += dely4;
    		R3 += dely4 * dely2;
    	} // for (dely = -blockHalf; dely <= blockHalf; dely++)
		for (delx = -blockHalf; delx <= blockHalf; delx++) {
			delx2 = delx * delx;
			delx4 = delx2 * delx2;
		    C0 += 1;
		    C1 += delx2;
		    C2 += delx4;
		    C3 += delx4 * delx2;
		} // for (delx = -blockHalf; delx <= blockHalf; delx++)
    	G = R0*R2*C0*C2 - R1*R1*C1*C1;
    	A = R1*R3*C0*C2 - R2*R2*C1*C1;
    	B = R0*R2*C1*C3 - R1*R1*C2*C2;
    	Q = C0*(R0*R2 - R1*R1);
    	T = R0*(C0*C2 - C1*C1);
    	U = C0*(R1*R3 - R2*R2);
    	V = C1*(R0*R2 - R1*R1);
    	W = R1*(C0*C2 - C1*C1);
    	Z = R0*(C1*C3 - C2*C2);
    	TR1 = T * R1;
    	QC1 = Q * C1;
    	WR2 = W * R2;
    	UC1 = U * C1;
    	ZR1 = Z * R1;
    	VC2 = V * C2;
    	k1[0] = 0.0;
    	k2[0] = 0.0;
    	k3[0] = 0.0;
    	k4[0] = 0.0;
    	k6[0] = 0.0;
    	k7[0] = 0.0;
    	k8[0] = 0.0;
    	k9[0] = 0.0;
    	k10[0] = 0.0;
    	for (dely = -blockHalf; dely <= blockHalf; dely++) {
    		dely2 = dely * dely;
    		for (delx = -blockHalf; delx <= blockHalf; delx++) {
    			buf = buffer[(y+dely)*xDim+x+delx];
    			delx2 = delx * delx;
    		    k1[0] += (G - TR1*dely2 - QC1*delx2)*buf;
    		    k2[0] += (A - WR2*dely2 - UC1*delx2)*dely*buf;
    		    k3[0] += (B - ZR1*dely2 - VC2*delx2)*delx*buf;
    		    k4[0] += (R0*dely2 - R1)*buf;
    		    k6[0] += (C0*delx2 - C1)*buf;
    		    k7[0] += (R1*dely2 - R2)*dely*buf;
    		    k8[0] += (R0*dely2 - R1)*delx*buf;
    		    k9[0] += (C0*delx2 - C1)*dely*buf;
    		    k10[0] += (C1*delx2 - C2)*delx*buf;
    		} // for (delx = -blockHalf; delx <= blockHalf; delx++)
    	} // for (dely = -blockHalf; dely <= blockHalf; dely++)
    	k1[0] = k1[0]/(Q*T);
    	k2[0] = k2[0]/(U*W);
    	k3[0] = k3[0]/(V*Z);
    	k4[0] = k4[0]/Q;
    	k6[0] = k6[0]/T;
    	k7[0] = k7[0]/U;
    	k8[0] = k8[0]/V;
    	k9[0] = k9[0]/W;
    	k10[0] = k10[0]/Z;
    	denom = Math.sqrt(k2[0]*k2[0] + k3[0]*k3[0]);
    	sinAngle[0] = k2[0]/denom;
    	cosAngle[0] = k3[0]/denom;
    	gradientAngle[0] = Math.atan2(sinAngle[0], cosAngle[0]);
    }
    
    
    private void bivariateCubicCoefficients5By5(int x, int y, double k1[], double k2[], double k3[], double k4[],
    		double k5[], double k6[], double k7[], double k8[], double k9[], double k10[], 
    		double sinAngle[], double cosAngle[], double gradientAngle[]) {
    	// Kernels for directly estimating the coefficients k1,...,k10 of the bivariate cubic f(y,x) = k1 + k2*y + k3*x
    	// + k4*y*y + k5*x*y + k6*x*x + k7*y*y*y + k8*x*y*y + k9*x*x*y + k10*x*x*x for a 5 by 5 neighborhood
    	double denom;
    	k1[0] = (1.0/175.0)*(-13.0*buffer[(y-2)*xDim+x-2] + 2.0*buffer[(y-2)*xDim+x-1] + 7.0*buffer[(y-2)*xDim+x]
    		 +2.0*buffer[(y-2)*xDim+x+1] -13.0*buffer[(y-2)*xDim+x+2] +2.0*buffer[(y-1)*xDim+x-2] + 17.0*buffer[(y-1)*xDim+x-1]
    	     +22.0*buffer[(y-1)*xDim+x] + 17.0*buffer[(y-1)*xDim+x+1] + 2.0*buffer[(y-1)*xDim+x+2] + 7.0*buffer[y*xDim+x-2]
    	     +22.0*buffer[y*xDim+x-1] + 27.0*buffer[y*xDim+x] + 22.0*buffer[y*xDim+x+1] + 7.0*buffer[y*xDim+x+2]
    	     +2.0*buffer[(y+1)*xDim+x-2] + 17.0*buffer[(y+1)*xDim+x-1] + 22.0*buffer[(y+1)*xDim+x] + 17.0*buffer[(y+1)*xDim+x+1]
    	     +2.0*buffer[(y+1)*xDim+x+2] -13.0*buffer[(y+2)*xDim+x-2] +2.0*buffer[(y+2)*xDim+x-1] + 7.0*buffer[(y+2)*xDim+x] 
    	     +2.0*buffer[(y+2)*xDim+x+1] -13.0*buffer[(y+2)*xDim+x+2]);
    	k2[0] = (1.0/420.0)*(31.0*buffer[(y-2)*xDim+x-2] - 5.0*buffer[(y-2)*xDim+x-1] - 17.0*buffer[(y-2)*xDim+x]
       		 -5.0*buffer[(y-2)*xDim+x+1] +31.0*buffer[(y-2)*xDim+x+2] -44.0*buffer[(y-1)*xDim+x-2] -62.0*buffer[(y-1)*xDim+x-1]
       	     -68.0*buffer[(y-1)*xDim+x] - 62.0*buffer[(y-1)*xDim+x+1] - 44.0*buffer[(y-1)*xDim+x+2] 
       	     +44.0*buffer[(y+1)*xDim+x-2] + 62.0*buffer[(y+1)*xDim+x-1] + 68.0*buffer[(y+1)*xDim+x] + 62.0*buffer[(y+1)*xDim+x+1]
       	     +44.0*buffer[(y+1)*xDim+x+2] -31.0*buffer[(y+2)*xDim+x-2] +5.0*buffer[(y+2)*xDim+x-1] + 17.0*buffer[(y+2)*xDim+x] 
       	     +5.0*buffer[(y+2)*xDim+x+1] -31.0*buffer[(y+2)*xDim+x+2]);
    	k3[0] = (1.0/420.0)*(31.0*buffer[(y-2)*xDim+x-2] - 44.0*buffer[(y-2)*xDim+x-1]
       		 +44.0*buffer[(y-2)*xDim+x+1] -31.0*buffer[(y-2)*xDim+x+2] -5.0*buffer[(y-1)*xDim+x-2] - 62.0*buffer[(y-1)*xDim+x-1]
       	     + 62.0*buffer[(y-1)*xDim+x+1] + 5.0*buffer[(y-1)*xDim+x+2] - 17.0*buffer[y*xDim+x-2]
       	     -68.0*buffer[y*xDim+x-1]  + 68.0*buffer[y*xDim+x+1] + 17.0*buffer[y*xDim+x+2]
       	     -5.0*buffer[(y+1)*xDim+x-2] - 62.0*buffer[(y+1)*xDim+x-1] + 62.0*buffer[(y+1)*xDim+x+1]
       	     +5.0*buffer[(y+1)*xDim+x+2] +31.0*buffer[(y+2)*xDim+x-2] -44.0*buffer[(y+2)*xDim+x-1] 
       	     +44.0*buffer[(y+2)*xDim+x+1] -31.0*buffer[(y+2)*xDim+x+2]);
    	k4[0] = (1.0/70.0)*(2.0*buffer[(y-2)*xDim+x-2] + 2.0*buffer[(y-2)*xDim+x-1] + 2.0*buffer[(y-2)*xDim+x]
       		 +2.0*buffer[(y-2)*xDim+x+1] +2.0*buffer[(y-2)*xDim+x+2] -1.0*buffer[(y-1)*xDim+x-2] - 1.0*buffer[(y-1)*xDim+x-1]
       	     -1.0*buffer[(y-1)*xDim+x] - 1.0*buffer[(y-1)*xDim+x+1] - 1.0*buffer[(y-1)*xDim+x+2] - 2.0*buffer[y*xDim+x-2]
       	     -2.0*buffer[y*xDim+x-1] - 2.0*buffer[y*xDim+x] - 2.0*buffer[y*xDim+x+1] - 2.0*buffer[y*xDim+x+2]
       	     -1.0*buffer[(y+1)*xDim+x-2] - 1.0*buffer[(y+1)*xDim+x-1] - 1.0*buffer[(y+1)*xDim+x] - 1.0*buffer[(y+1)*xDim+x+1]
       	     -1.0*buffer[(y+1)*xDim+x+2] +2.0*buffer[(y+2)*xDim+x-2] +2.0*buffer[(y+2)*xDim+x-1] + 2.0*buffer[(y+2)*xDim+x] 
       	     +2.0*buffer[(y+2)*xDim+x+1] +2.0*buffer[(y+2)*xDim+x+2]);
    	k5[0] = (1.0/100.0)*(+4.0*buffer[(y-2)*xDim+x-2] + 2.0*buffer[(y-2)*xDim+x-1] 
       		 -2.0*buffer[(y-2)*xDim+x+1] -4.0*buffer[(y-2)*xDim+x+2] +2.0*buffer[(y-1)*xDim+x-2] + 1.0*buffer[(y-1)*xDim+x-1]
       	     - 1.0*buffer[(y-1)*xDim+x+1] - 2.0*buffer[(y-1)*xDim+x+2] 
       	     -2.0*buffer[(y+1)*xDim+x-2] - 1.0*buffer[(y+1)*xDim+x-1]  + 1.0*buffer[(y+1)*xDim+x+1]
       	     +2.0*buffer[(y+1)*xDim+x+2] -4.0*buffer[(y+2)*xDim+x-2] -2.0*buffer[(y+2)*xDim+x-1] 
       	     +2.0*buffer[(y+2)*xDim+x+1] +4.0*buffer[(y+2)*xDim+x+2]);
    	k6[0] = (1.0/70.0)*(2.0*buffer[(y-2)*xDim+x-2] - 1.0*buffer[(y-2)*xDim+x-1] - 2.0*buffer[(y-2)*xDim+x]
       		 -1.0*buffer[(y-2)*xDim+x+1] +2.0*buffer[(y-2)*xDim+x+2] +2.0*buffer[(y-1)*xDim+x-2] - 1.0*buffer[(y-1)*xDim+x-1]
       	     -2.0*buffer[(y-1)*xDim+x] - 1.0*buffer[(y-1)*xDim+x+1] + 2.0*buffer[(y-1)*xDim+x+2] + 2.0*buffer[y*xDim+x-2]
       	     -1.0*buffer[y*xDim+x-1] - 2.0*buffer[y*xDim+x] - 1.0*buffer[y*xDim+x+1] + 2.0*buffer[y*xDim+x+2]
       	     +2.0*buffer[(y+1)*xDim+x-2] - 1.0*buffer[(y+1)*xDim+x-1] - 2.0*buffer[(y+1)*xDim+x] - 1.0*buffer[(y+1)*xDim+x+1]
       	     +2.0*buffer[(y+1)*xDim+x+2] +2.0*buffer[(y+2)*xDim+x-2] -1.0*buffer[(y+2)*xDim+x-1] - 2.0*buffer[(y+2)*xDim+x] 
       	     -1.0*buffer[(y+2)*xDim+x+1] +2.0*buffer[(y+2)*xDim+x+2]);
    	k7[0] = (1.0/60.0)*(-1.0*buffer[(y-2)*xDim+x-2] - 1.0*buffer[(y-2)*xDim+x-1] - 1.0*buffer[(y-2)*xDim+x]
       		 -1.0*buffer[(y-2)*xDim+x+1] -1.0*buffer[(y-2)*xDim+x+2] +2.0*buffer[(y-1)*xDim+x-2] + 2.0*buffer[(y-1)*xDim+x-1]
       	     +2.0*buffer[(y-1)*xDim+x] + 2.0*buffer[(y-1)*xDim+x+1] + 2.0*buffer[(y-1)*xDim+x+2] 
       	     -2.0*buffer[(y+1)*xDim+x-2] - 2.0*buffer[(y+1)*xDim+x-1] - 2.0*buffer[(y+1)*xDim+x] - 2.0*buffer[(y+1)*xDim+x+1]
       	     -2.0*buffer[(y+1)*xDim+x+2] +1.0*buffer[(y+2)*xDim+x-2] +1.0*buffer[(y+2)*xDim+x-1] + 1.0*buffer[(y+2)*xDim+x] 
       	     +1.0*buffer[(y+2)*xDim+x+1] +1.0*buffer[(y+2)*xDim+x+2]);
    	k8[0] = (1.0/140.0)*(-4.0*buffer[(y-2)*xDim+x-2] - 2.0*buffer[(y-2)*xDim+x-1] 
       		 +2.0*buffer[(y-2)*xDim+x+1] +4.0*buffer[(y-2)*xDim+x+2] +2.0*buffer[(y-1)*xDim+x-2] + 1.0*buffer[(y-1)*xDim+x-1]
       	     - 1.0*buffer[(y-1)*xDim+x+1] - 2.0*buffer[(y-1)*xDim+x+2] + 4.0*buffer[y*xDim+x-2]
       	     +2.0*buffer[y*xDim+x-1]  - 2.0*buffer[y*xDim+x+1] - 4.0*buffer[y*xDim+x+2]
       	     +2.0*buffer[(y+1)*xDim+x-2] + 1.0*buffer[(y+1)*xDim+x-1]  - 1.0*buffer[(y+1)*xDim+x+1]
       	     -2.0*buffer[(y+1)*xDim+x+2] -4.0*buffer[(y+2)*xDim+x-2] -2.0*buffer[(y+2)*xDim+x-1] 
       	     +2.0*buffer[(y+2)*xDim+x+1] +4.0*buffer[(y+2)*xDim+x+2]);
    	k9[0] = (1.0/140.0)*(-4.0*buffer[(y-2)*xDim+x-2] + 2.0*buffer[(y-2)*xDim+x-1] + 4.0*buffer[(y-2)*xDim+x]
       		 +2.0*buffer[(y-2)*xDim+x+1] -4.0*buffer[(y-2)*xDim+x+2] -2.0*buffer[(y-1)*xDim+x-2] + 1.0*buffer[(y-1)*xDim+x-1]
       	     +2.0*buffer[(y-1)*xDim+x] + 1.0*buffer[(y-1)*xDim+x+1] - 2.0*buffer[(y-1)*xDim+x+2] 
       	     +2.0*buffer[(y+1)*xDim+x-2] - 1.0*buffer[(y+1)*xDim+x-1] - 2.0*buffer[(y+1)*xDim+x] - 1.0*buffer[(y+1)*xDim+x+1]
       	     +2.0*buffer[(y+1)*xDim+x+2] +4.0*buffer[(y+2)*xDim+x-2] -2.0*buffer[(y+2)*xDim+x-1] - 4.0*buffer[(y+2)*xDim+x] 
       	     -2.0*buffer[(y+2)*xDim+x+1] +4.0*buffer[(y+2)*xDim+x+2]);
    	k10[0] = (1.0/60.0)*(-1.0*buffer[(y-2)*xDim+x-2] + 2.0*buffer[(y-2)*xDim+x-1] 
       		 -2.0*buffer[(y-2)*xDim+x+1] +1.0*buffer[(y-2)*xDim+x+2] -1.0*buffer[(y-1)*xDim+x-2] + 2.0*buffer[(y-1)*xDim+x-1]
       	     - 2.0*buffer[(y-1)*xDim+x+1] + 1.0*buffer[(y-1)*xDim+x+2] - 1.0*buffer[y*xDim+x-2]
       	     +2.0*buffer[y*xDim+x-1]  - 2.0*buffer[y*xDim+x+1] + 1.0*buffer[y*xDim+x+2]
       	     -1.0*buffer[(y+1)*xDim+x-2] + 2.0*buffer[(y+1)*xDim+x-1]  - 2.0*buffer[(y+1)*xDim+x+1]
       	     +1.0*buffer[(y+1)*xDim+x+2] -1.0*buffer[(y+2)*xDim+x-2] +2.0*buffer[(y+2)*xDim+x-1] 
       	     -2.0*buffer[(y+2)*xDim+x+1] +1.0*buffer[(y+2)*xDim+x+2]);
    	denom = Math.sqrt(k2[0]*k2[0] + k3[0]*k3[0]);
    	sinAngle[0] = k2[0]/denom;
    	cosAngle[0] = k3[0]/denom;
    	gradientAngle[0] = Math.atan2(sinAngle[0], cosAngle[0]);
    }
    
    private void zeroCrossingEdgeDetector() {
    	int x, y;
    	double k1[] = new double[1];
    	double k2[] = new double[1];
    	double k3[] = new double[1];
    	double k4[] = new double[1];
    	double k5[] = new double[1];
    	double k6[] = new double[1];
    	double k7[] = new double[1];
    	double k8[] = new double[1];
    	double k9[] = new double[1];
    	double k10[] = new double[1];
    	double sinAngle[] = new double[1];
    	double cosAngle[] = new double[1];
    	double gradientAngle[] = new double[1];
    	double rho[] = new double[1];
    	boolean edge;
    	
    	for (y = blockHalf; y < yDim-blockHalf; y++) {
        	for (x = blockHalf; x < xDim-blockHalf; x++) {
        		if (blockSide == 5) {
        		    bivariateCubicCoefficients5By5(x, y, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, sinAngle, cosAngle, gradientAngle);
        		}
        		else {
        			bivariateCubicCoefficients(x, y, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, sinAngle, cosAngle, gradientAngle);	
        		}
        		edge = isEdgePixel(k1[0], k2[0], k3[0], k4[0], k5[0], k6[0], k7[0],
        	    		k8[0], k9[0], k10[0], sinAngle[0], cosAngle[0], rho);
        		if (edge) {
        			result[y*xDim+x] = 1;
        		}
        		else {
        			result[y*xDim+x] = 0;
        		}
        	} // for (x = blockHalf; x < xDim-blockHalf; x++)
    	} // for (y = blockHalf; y < yDim-blockHalf; y++)
    }
    
    private void cornerDetector() {
    	int x, y;
    	double k1[] = new double[1];
    	double k2[] = new double[1];
    	double k3[] = new double[1];
    	double k4[] = new double[1];
    	double k5[] = new double[1];
    	double k6[] = new double[1];
    	double k7[] = new double[1];
    	double k8[] = new double[1];
    	double k9[] = new double[1];
    	double k10[] = new double[1];
    	double sinAngle[] = new double[1];
    	double cosAngle[] = new double[1];
    	double gradientAngle[] = new double[1];
    	double rho[] = new double[1];
    	boolean edge;
    	double gradientDirection;
    	
    	for (y = blockHalf; y < yDim-blockHalf; y++) {
        	for (x = blockHalf; x < xDim-blockHalf; x++) {
        		if (blockSide == 5) {
        		    bivariateCubicCoefficients5By5(x, y, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, sinAngle, cosAngle, gradientAngle);
        		}
        		else {
        			bivariateCubicCoefficients(x, y, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, sinAngle, cosAngle, gradientAngle);	
        		}
        		edge = isEdgePixel(k1[0], k2[0], k3[0], k4[0], k5[0], k6[0], k7[0],
        	    		k8[0], k9[0], k10[0], sinAngle[0], cosAngle[0], rho);
        		if (edge) {
        			gradientDirection = -2.0*(k2[0]*k2[0]*k6[0] - k2[0]*k3[0]*k5[0] + k3[0]*k3[0]*k4[0])/
        					Math.pow(k2[0]*k2[0] + k3[0]*k3[0],1.5);
        			if (Math.abs(gradientDirection) > gradientDirectionThreshold) {
        			    result[y*xDim+x] = 1;
        			}
        			else {
        				result[y*xDim+x] = 0;
        			}
        		}
        		else {
        			result[y*xDim+x] = 0;
        		}
        	} // for (x = blockHalf; x < xDim-blockHalf; x++)
    	} // for (y = blockHalf; y < yDim-blockHalf; y++)	
    }
    
    private void integratedDirectionalDerivative() {
    	int x,y;
    	double D1 = 0.0;
    	double D2 = 0.0;
    	for (y = blockHalf; y < yDim-blockHalf; y++) {
        	for (x = blockHalf; x < xDim-blockHalf; x++) {
        		if (blockSide == 5) {
        			D1 = rowDerivativeMask5By5(x,y);
        			D2 = columnDerivativeMask5By5(x,y);
        		}
        		else if (blockSide == 7) {
        			D2 = rowDerivativeMask7By7(x,y);
        			D2 = columnDerivativeMask7By7(x,y);
        		}
        		if (routine == INTEGRATED_DIRECTIONAL_DERIVATIVE_ANGLE) {
        			result[y*xDim+x] = Math.atan2(D1,D2);
        		}
        		else if (routine == INTEGRATED_DIRECTIONAL_DERIVATIVE_MAGNITUDE){
        		    result[y*xDim+x] = Math.sqrt(D1*D1 + D2*D2);	
        		}
        	} // for (x = blockHalf; x < xDim-blockHalf; x++)
    	} // for (y = blockHalf; y < yDim-blockHalf; y++)
    }
    
    private double rowDerivativeMask5By5(int x, int y) {
    	// D1 = L*L*K7 + (1/3)*L*L*K9 + K2
    	// D2 = L*L*K10 + (1/3)*L*L*K8 + K3
    	// gradient direction thetaMax = atan(D1/D2)
    	// integrated first directional derivative = sqrt(D1*D1 + D2*D2)
    	// This routine calculates D1 with the row derivative mask
    	return  (1.0/10500.0)*(-116.0*buffer[(y-2)*xDim + x-2] -530.0*buffer[(y-2)*xDim+x-1] -668.0*buffer[(y-2)*xDim+x]
    			-530.0*buffer[(y-2)*xDim+x+1] -116.0*buffer[(y-2)*xDim+x+2] -128*buffer[(y-1)*xDim+x-2] -335.0*buffer[(y-1)*xDim+x-1]
    		    -404.0*buffer[(y-1)*xDim+x] -335.0*buffer[(y-1)*xDim+x+1] -128.0*buffer[(y-1)*xDim+x+2] +128.0*buffer[(y+1)*xDim+x-2]
    		    +335.0*buffer[(y+1)*xDim+x-1] + 404.0*buffer[(y+1)*xDim+x] + 335.0*buffer[(y+1)*xDim+x+1] + 128.0*buffer[(y+1)*xDim+x+2]
    		    +116.0*buffer[(y+2)*xDim+x-2] + 530.0*buffer[(y+2)*xDim+x-1] + 668.0*buffer[(y+2)*xDim+x] + 530.0*buffer[(y+2)*xDim+x+1]
    		    +116.0*buffer[(y+2)*xDim+x+2]);
    }
    
    private double columnDerivativeMask5By5(int x, int y) {
    	// D1 = L*L*K7 + (1/3)*L*L*K9 + K2
    	// D2 = L*L*K10 + (1/3)*L*L*K8 + K3
    	// gradient direction thetaMax = atan(D1/D2)
    	// integrated first directional derivative = sqrt(D1*D1 + D2*D2)
    	// This routine calculates D2 with the column derivative mask
    	return  (1.0/10500.0)*(-116.0*buffer[(y-2)*xDim + x-2] -530.0*buffer[(y-1)*xDim+x-2] -668.0*buffer[y*xDim+x-2]
    			-530.0*buffer[(y+1)*xDim+x-2] -116.0*buffer[(y+2)*xDim+x-2] -128*buffer[(y-2)*xDim+x-1] -335.0*buffer[(y-1)*xDim+x-1]
    		    -404.0*buffer[y*xDim+x-1] -335.0*buffer[(y+1)*xDim+x-1] -128.0*buffer[(y+2)*xDim+x-1] +128.0*buffer[(y-2)*xDim+x+1]
    		    +335.0*buffer[(y-1)*xDim+x+1] + 404.0*buffer[y*xDim+x+1] + 335.0*buffer[(y+1)*xDim+x+1] + 128.0*buffer[(y+2)*xDim+x+1]
    		    +116.0*buffer[(y-2)*xDim+x+2] + 530.0*buffer[(y-1)*xDim+x+2] + 668.0*buffer[y*xDim+x+2] + 530.0*buffer[(y+1)*xDim+x+2]
    		    +116.0*buffer[(y+2)*xDim+x+2]);
    }
    
    private double rowDerivativeMask7By7(int x, int y) {
    	// D1 = L*L*K7 + (1/3)*L*L*K9 + K2
    	// D2 = L*L*K10 + (1/3)*L*L*K8 + K3
    	// gradient direction thetaMax = atan(D1/D2)
    	// integrated first directional derivative = sqrt(D1*D1 + D2*D2)
    	// This routine calculates D1 with the row derivative mask
    	return  (1.0/28224.0)*(-3.0*buffer[(y-3)*xDim+x-3] -348.0*buffer[(y-3)*xDim+x-2] -555.0*buffer[(y-3)*xDim+x-1]
    		    -624.0*buffer[(y-3)*xDim+x] -555.0*buffer[(y-3)*xDim+x+1] -348.0*buffer[(y-3)*xDim+x+2] -3.0*buffer[(y-3)*xDim+x+3]
    		    -142.0*buffer[(y-2)*xDim+x-3] -372.0*buffer[(y-2)*xDim+x-2] -510.0*buffer[(y-2)*xDim+x-1] -556.0*buffer[(y-2)*xDim+x]
    		    -510.0*buffer[(y-2)*xDim+x+1] -372.0*buffer[(y-2)*xDim+x+2] -142.0*buffer[(y-2)*xDim+x+3] -113.0*buffer[(y-1)*xDim+x-3]
    		    -228.0*buffer[(y-1)*xDim+x-2] -297.0*buffer[(y-1)*xDim+x-1] -320.0*buffer[(y-1)*xDim+x] -297.0*buffer[(y-1)*xDim+x+1]
    		    -228.0*buffer[(y-1)*xDim+x+2] -113.0*buffer[(y-1)*xDim+x+3] +113.0*buffer[(y+1)*xDim+x-3] +228.0*buffer[(y+1)*xDim+x-2]
    		    +297.0*buffer[(y+1)*xDim+x-1] +320.0*buffer[(y+1)*xDim+x] +297.0*buffer[(y+1)*xDim+x+1] +228.0*buffer[(y+1)*xDim+x+2]
    		    +113.0*buffer[(y+1)*xDim+x+3] +142.0*buffer[(y+2)*xDim+x-3] +372.0*buffer[(y+2)*xDim+x-2] +510.0*buffer[(y+2)*xDim+x-1]
    		    +556.0*buffer[(y+2)*xDim+x] + 510.0*buffer[(y+2)*xDim+x+1] +372.0*buffer[(y+2)*xDim+x+2] +142.0*buffer[(y+2)*xDim+x+3]
    		    +3.0*buffer[(y+3)*xDim+x-3] +348.0*buffer[(y+3)*xDim+x-2] +555.0*buffer[(y+3)*xDim+x-1] +624.0*buffer[(y+3)*xDim+x]
    		    +555.0*buffer[(y+3)*xDim+x+1] +348.0*buffer[(y+3)*xDim+x+2] +3.0*buffer[(y+3)*xDim+x+3]);
    }
    
    private double columnDerivativeMask7By7(int x, int y) {
    	// D1 = L*L*K7 + (1/3)*L*L*K9 + K2
    	// D2 = L*L*K10 + (1/3)*L*L*K8 + K3
    	// gradient direction thetaMax = atan(D1/D2)
    	// integrated first directional derivative = sqrt(D1*D1 + D2*D2)
    	// This routine calculates D2 with the column derivative mask
    	return  (1.0/28224.0)*(-3.0*buffer[(y-3)*xDim+x-3] -348.0*buffer[(y-2)*xDim+x-3] -555.0*buffer[(y-1)*xDim+x-3]
    		    -624.0*buffer[y*xDim+x-3] -555.0*buffer[(y+1)*xDim+x-3] -348.0*buffer[(y+2)*xDim+x-3] -3.0*buffer[(y+3)*xDim+x-3]
    		    -142.0*buffer[(y-3)*xDim+x-2] -372.0*buffer[(y-2)*xDim+x-2] -510.0*buffer[(y-1)*xDim+x-2] -556.0*buffer[y*xDim+x-2]
    		    -510.0*buffer[(y+1)*xDim+x-2] -372.0*buffer[(y+2)*xDim+x-2] -142.0*buffer[(y+3)*xDim+x-2] -113.0*buffer[(y-3)*xDim+x-1]
    		    -228.0*buffer[(y-2)*xDim+x-1] -297.0*buffer[(y-1)*xDim+x-1] -320.0*buffer[y*xDim+x-1] -297.0*buffer[(y+1)*xDim+x-1]
    		    -228.0*buffer[(y+2)*xDim+x-1] -113.0*buffer[(y+3)*xDim+x-1] +113.0*buffer[(y-3)*xDim+x+1] +228.0*buffer[(y-2)*xDim+x+1]
    		    +297.0*buffer[(y-1)*xDim+x+1] +320.0*buffer[y*xDim+x+1] +297.0*buffer[(y+1)*xDim+x+1] +228.0*buffer[(y+2)*xDim+x+1]
    		    +113.0*buffer[(y+3)*xDim+x+1] +142.0*buffer[(y-3)*xDim+x+2] +372.0*buffer[(y-2)*xDim+x+2] +510.0*buffer[(y-1)*xDim+x+2]
    		    +556.0*buffer[y*xDim+x+2] + 510.0*buffer[(y+1)*xDim+x+2] +372.0*buffer[(y+2)*xDim+x+2] +142.0*buffer[(y+3)*xDim+x+2]
    		    +3.0*buffer[(y-3)*xDim+x+3] +348.0*buffer[(y-2)*xDim+x+3] +555.0*buffer[(y-1)*xDim+x+3] +624.0*buffer[y*xDim+x+3]
    		    +555.0*buffer[(y+1)*xDim+x+3] +348.0*buffer[(y+2)*xDim+x+3] +3.0*buffer[(y+3)*xDim+x+3]);
    }
	
}