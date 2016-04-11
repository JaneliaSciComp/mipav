package gov.nih.mipav.model.algorithms;

import java.io.IOException;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;


// These routines implement the text in Computer and Robot Vision, Volume I, Robert M. Haralick and Linda G. Shapiro,
// Addison-Wesley Publishing Company, Inc., 1992, Chapter 8, The Facet Model, pp. 371-452.
public  class AlgorithmFacetModel extends AlgorithmBase {
	// ~ Static fields/initializers
	public static final int FACET_BASED_PEAK_NOISE_REMOVAL = 1;
	public static final int ITERATED_FACET_MODEL = 2;
	public static final int GRADIENT_BASED_FACET_EDGE_DETECTION = 3;
    // -------------------------------------------------------------------------------------
	
	//~ Instance fields ------------------------------------------------------------------------------------------------
	// Square blocks of blockSide by blockSide pixels used.  BlockSide must be an odd integer.
	private int blockSide;
	
	// Probability of rejecting a true hypothesis.  A reasonable range for alpha is .01 to .05
    private double alpha;
    
    // Tells which of the facet model routines to use
    private int routine;
    
    private int iterations = 1;
    
    private int xDim;
	private int yDim;
	private double buffer[];
	private double result[];
	private int blockHalf;
    
  //~ Constructors ---------------------------------------------------------------------------------------------------
    public AlgorithmFacetModel(ModelImage srcImg, int routine, int blockSide, double alpha) {
    	super(null, srcImg);
    	this.routine = routine;
    	this.blockSide = blockSide;
    	this.alpha = alpha;
    }
    
    public AlgorithmFacetModel(ModelImage destImg, ModelImage srcImg, int routine, int blockSide, double alpha) {
    	super(destImg, srcImg);
    	this.routine = routine;
    	this.blockSide = blockSide;
    	this.alpha = alpha;
    }
    
    /**
     * Starts the program.
     */
    public void runAlgorithm() {
    	int zDim;
    	int tDim;
    	int nDims;
    	int sliceSize;
    	double temp[];
    	int x, y, z, t;
    	int iter;
    	blockHalf = (blockSide - 1)/2;

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
        
        for (iter = 0; iter < iterations; iter++) {
	        for (t = 0; t < tDim; t++) {
	        	for (z = 0; z < zDim; z++) {
	        		if (iter == 0) {
		        		try {
		        			srcImage.exportData((z + t*zDim)*sliceSize, sliceSize, buffer);
		        		}
		        		catch(IOException e) {
		        			MipavUtil.displayError("IOException " + e + " on srcImage.exportData");
		        			setCompleted(false);
		        			return;
		        		}
	        		} // if (iter == 0)
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
	        	    }
	        	
	        		if (iter == iterations-1) {
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
	        		} // if (iter == iterations-1)
	        		else {
	        			temp = result;
	        			result = buffer;
	        			buffer = temp;
	        		}
	        	} // for (z = 0; z < zDim; z++)
	        } // for (t = 0; t < tDim; t++)
        } // for (iter = 0; iter < iterations; iter++)
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
        	    		if (epsilonSquared[y+dely-blockHalf][x+delx-blockHalf] < lowestEpsilonSquared) {
        	    			lowestEpsilonSquared = epsilonSquared[y+dely-blockHalf][x+delx-blockHalf];
        	    			lowestDelx = delx;
        	    			lowestDely = dely;
        	    		}
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
	
}