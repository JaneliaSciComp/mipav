package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.Color;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;


/**
 * With a small number of records, it is feasible use RANDOM_INIT to perform multiple restarts efficiently.
 * With a small sample size, the subsampling for initialization is not effective in the Bradley-Fayyad
 * initialization.  The Bradley-Fayyad initialization is useful when applied to smaller data sets, but
 * it is best suited for large-scale data.  By initializing a general clustering algorithm near the nodes,
 * not only are the true clusters found more often, but it follows that the clustering algorithm will 
 * iterate fewer times prior to convergence.
 * 
 * From the Celebi reference:
 * Consider a point xi, two cluster centers ca and cb and a distance metric d.  Using the triangle 
 * inequality, we have d(ca,cb) <= d(xi,ca) + d(xi,cb).  Therefore, if we know that 2d(xi,ca) <= d(ca,cb),
 * we can conclude that d(xi,ca) <= d(xi,cb) without having to calculate d(xi,cb).
 References:
 1.) "A systematic evaluation of different methods for initializing the K-means clustering algorithm"
     by Anna D. Peterson, Arka. P. Ghosh, and Ranjan Maitra, IEEE Transactions on Knowledge and
     Data Engineering
 2.) "Refining Initial Points for K-Means Clustering" by P.S. Bradley and Usama M. Fayyad.
 3.) "Hierarchical Grouping to Optimize an Objective Function" by Joe H. Ward, Jr.,
     Journal of the American Statistical Association, Volume 58, Issue 301, March, 1963, pp. 236-244.
 4.) "Improving the Performance of K-Means for Color Quantization" by M. Emre Celebi, 
     Image and Vision Computing, Volume 29, No. 4, 2011, pp. 260-271.
 */
public class AlgorithmKMeans extends AlgorithmBase {
	
	private static final int RANDOM_INIT = 0;
	
	private static final int BRADLEY_FAYYAD_INIT = 1;
	
	private static final int HIERARCHICAL_GROUPING_INIT = 2;
	
	private static final int MAXMIN_INIT = 3;
	
    private static final int K_MEANS = 0;
	
	private static final int GLOBAL_K_MEANS = 1;
	
	private static final int FAST_GLOBAL_K_MEANS = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------
	
	private ModelImage image;
	
	// K-means, global k-means, or fast global k-means
	private int algoSelection;
	
	// Take resolutions from the image
    // Use 1.0 in every dimension if not scaled.
    // Subscript goes from 0 to nDims - 1
    private double scale[];
    
    // First subscript x = 0, y = 1, z = 2, t = 3
    // Second subscript 0 to nPoints-1 for each point
    // Value is the point position
	private double[][] pos;
	
	// subscript goes from 0 to nPoints-1 for each point
    // Value is the cluster number from 0 to numberClusters-1.
	private int[] groupNum;
	
	// First subscript x = 0, y = 1, z = 2, t = 3
    // Second subscript 0 to numberClusters-1 for each cluster
    // Value is the cluster position
	private double[][] centroidPos;
	
	private String resultsFileName;
	
	private int initSelection;
	
	private float redBuffer[] = null;
	
	private float greenBuffer[] = null;
	
	private float blueBuffer[] = null;
	
	// Scale factor used in RGB-CIELab conversions.  255 for ARGB, could be higher for ARGB_USHORT.
    private double scaleMax = 255.0;
    
    // If true, each color is weighed proportional to its frequency
    private boolean useColorHistogram = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     
     */
    public AlgorithmKMeans(ModelImage image, int algoSelection, double[][] pos, double[] scale, int groupNum[],
    		               double[][] centroidPos, String resultsFileName, int initSelection, float[] redBuffer,
    		               float[] greenBuffer, float[] blueBuffer, double scaleMax, boolean useColorHistogram) {

        this.image = image;
        this.algoSelection = algoSelection;
        this.pos = pos;
        this.scale = scale;
        this.groupNum = groupNum;
        this.centroidPos = centroidPos;
        this.resultsFileName = resultsFileName;
        this.initSelection = initSelection;
        this.redBuffer = redBuffer;
        this.greenBuffer = greenBuffer;
        this.blueBuffer = blueBuffer;
        this.scaleMax = scaleMax;
        this.useColorHistogram = useColorHistogram;
    }

    

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        
        super.finalize();
    }

    

    /**
     * Starts the algorithm.
     */
	public void runAlgorithm() {
    	int nDims;
    	int nPoints;
    	int numberClusters;
    	int startingPointIndex[];
    	int i, j, k, m, n, s;
    	/** Reference to the random number generator. */
        RandomNumberGen randomGen;
        int possibleStart;
        boolean alreadyUsed;
        double distSquared;
        double minDistSquared;
        double diff;
        boolean changeOccurred;
        int originalGroupNum;
        int iteration;
        int pointsInCluster[];
        Color color[];
        VOI newPtVOI;
        float xArr[] = new float[1];
        float yArr[] = new float[1];
        float zArr[] = new float[1];
        File file;
        RandomAccessFile raFile;
        String dataString = "";
        int subsampleNumber;
        int subsampleSize;
        int subsampleIndex[];
        int possibleSample;
        double subsamplePos[][];
        int clustersWithoutPoints = 0;
        double maxDistSquared;
        int maxDistPoint = 0;
        int clusterWithMaxDistPoint = 0;
        double localCM[][][];
        int numberCMPoints = 0;
        double unionCM[][];
        boolean haveCMPoint;
        int CMPointsAdded = 0;
        double localFM[][][];
        double totalDistSquared[];
        double minTotalDistSquared;
        int bestFMIndex = 0;
        double centroidStartPos[][];
        int groupsPresent;
        int pointsInGroup[];
        int highestGroupPresent;
        int hierGroup[][];
        double essGroup[];
        int bestFirstIndex = 0;
        int bestSecondIndex = 0;
        double essIncrease;
        double minessIncrease;
        double sum;
        double sumSq;
        double newess;
        double bestnewess = 0.0;
        int newPointsInGroup;
        int bestNewPointsInGroup = 0;
        boolean found;
        int groupIndex;
        int currentClusters;
        double minDistSquaredSet[] = null;
        int minIndexSet[] = null;
        int newIndex = 0;
        int length = 0;
        int colors[] = null;
		//Vector<Vector3f> colors;
		int[] indexTable = null;
		int colorsFound;
		//Vector3f colorVector;
		double varR, varG, varB;
		// Observer = 2 degrees, Illuminant = D65
        double XN = 95.047;
        double YN = 100.000;
        double ZN = 108.883;
        double X, Y, Z;
        double varX, varY, varZ;
        float a, b;
        byte buffer[];
        int instances[] = null;
        int numTimes;
        double weight[] = null;
        double totalWeight[] = null;
        double scale2[] = new double[scale.length];
        boolean equalScale;
        // Actually square of distances divided by 4
        double centroidDistances[][] = null;
        int presentClusters;
        int initialClusterLocation;
        double totalMinDistSquared = 0.0;
        double bestTotalMinDistSquared;
        double centroidPosStart[][];
        
        for (i = 0; i < scale.length; i++) {
        	scale2[i] = scale[i]*scale[i];
        }
        equalScale = true;
        for (i = 1; i < scale.length; i++) {
        	if (scale[i] != scale[0]) {
        		equalScale = false;
        	}
        }
         
        numberClusters = centroidPos[0].length;
    	pointsInCluster = new int[numberClusters];
    	
    	centroidDistances = new double[numberClusters][numberClusters];
        
        if ((redBuffer != null) || (greenBuffer != null) || (blueBuffer != null)) {
        	// Separate on a and b chrominance components in CIELAB space
        	// Ignore the L luminance component
        	// No matter what the dimensionality of the original image,
        	// the kmeans color segmentation becomes a 2 dimensional problem.
        	nDims = 2;
        	if (redBuffer != null) {
        	    length = redBuffer.length;
        	}
        	else {
        		length = greenBuffer.length;
        	}
		    colors = new int[length];
		    if (useColorHistogram) {
		    	instances = new int[length];
		    }
		    indexTable = new int[length];
		    for (i = 0; i < length; i++) {
		    	indexTable[i] = -1;
		    }
		    colorsFound = 0;
		    fireProgressStateChanged("Finding the colors present");
		    if ((redBuffer != null) && (greenBuffer != null) && (blueBuffer != null)) {
			    for (i = 0; i < length; i++) {
			    	if (indexTable[i] == -1) {
				    	colors[colorsFound] = i;
				    	indexTable[i] = colorsFound;
				    	numTimes = 1;
				    	for (j = i+1; j < length; j++) {
				    		if ((redBuffer[j] == redBuffer[i]) && (greenBuffer[j] == greenBuffer[i]) &&
				    			(blueBuffer[j] == blueBuffer[i])) {
				    		    indexTable[j] = colorsFound;
				    		    numTimes++;
				    		} // if ((redBuffer[j] == redBuffer[i]) && (greenBuffer[j] == greenBuffer[i])
				    	} // for (j = i+1; j < length; j++)
				    	if (useColorHistogram) {
				    		instances[colorsFound] = numTimes;
				    	}
				    	colorsFound++;
			    	} // if (indexTable[i] == -1)
			    } // for (i = 0; i < length; i++)
		    } // if ((redBuffer != null) && (greenBuffer != null) && (blueBuffer != null))
		    else if ((redBuffer != null) && (greenBuffer != null)) {
		    	for (i = 0; i < length; i++) {
			    	if (indexTable[i] == -1) {
				    	colors[colorsFound] = i;
				    	indexTable[i] = colorsFound;
				    	numTimes = 1;
				    	for (j = i+1; j < length; j++) {
				    		if ((redBuffer[j] == redBuffer[i]) && (greenBuffer[j] == greenBuffer[i])) {
				    		    indexTable[j] = colorsFound;
				    		    numTimes++;
				    		} // if ((redBuffer[j] == redBuffer[i]) && (greenBuffer[j] == greenBuffer[i]))
				    	} // for (j = i+1; j < length; j++)
				    	if (useColorHistogram) {
				    		instances[colorsFound] = numTimes;
				    	}
				    	colorsFound++;
			    	} // if (indexTable[i] == -1)
			    } // for (i = 0; i < length; i++)	
		    } // else if ((redBuffer != null) && (greenBuffer != null))
		    else if ((redBuffer != null) && (blueBuffer != null)) {
		    	for (i = 0; i < length; i++) {
			    	if (indexTable[i] == -1) {
				    	colors[colorsFound] = i;
				    	indexTable[i] = colorsFound;
				    	numTimes = 1;
				    	for (j = i+1; j < length; j++) {
				    		if ((redBuffer[j] == redBuffer[i]) && (blueBuffer[j] == blueBuffer[i])) {
				    		    indexTable[j] = colorsFound;
				    		    numTimes++;
				    		} // if ((redBuffer[j] == redBuffer[i]) && (blueBuffer[j] == blueBuffer[i])
				    	} // for (j = i+1; j < length; j++)
				    	if (useColorHistogram) {
				    		instances[colorsFound] = numTimes;
				    	}
				    	colorsFound++;
			    	} // if (indexTable[i] == -1)
			    } // for (i = 0; i < length; i++)	
		    } // else if ((redBuffer != null) && (blueBuffer != null))
		    else if ((greenBuffer != null) && (blueBuffer != null)) {
		    	for (i = 0; i < length; i++) {
			    	if (indexTable[i] == -1) {
				    	colors[colorsFound] = i;
				    	indexTable[i] = colorsFound;
				    	numTimes = 1;
				    	for (j = i+1; j < length; j++) {
				    		if ((greenBuffer[j] == greenBuffer[i]) && (blueBuffer[j] == blueBuffer[i])) {
				    		    indexTable[j] = colorsFound;
				    		    numTimes++;
				    		} // if ((greenBuffer[j] == greenBuffer[i]) && (blueBuffer[j] == blueBuffer[i]))
				    	} // for (j = i+1; j < length; j++)
				    	if (useColorHistogram) {
				    		instances[colorsFound] = numTimes;
				    	}
				    	colorsFound++;
			    	} // if (indexTable[i] == -1)
			    } // for (i = 0; i < length; i++)	
	        } // else if ((greenBuffer != null) && (blueBuffer != null))
		    Preferences.debug("Colors found = " + colorsFound + "\n");
		    pos = new double[2][colorsFound];
		    groupNum = new int[colorsFound];
		    if (useColorHistogram) {
		    	weight = new double[colorsFound];
		    	totalWeight = new double[numberClusters];
		    	for (i = 0; i < colorsFound; i++) {
		    		weight[i] = ((double)instances[i])/((double)length);
		    	}
		    	instances = null;
		    } // if (useColorHistogram)
		    
		    fireProgressStateChanged("Converting RGB to CIELAB");
		    for (i = 0; i < colorsFound; i++) {
		        if (redBuffer != null) {
		            varR = redBuffer[colors[i]]/scaleMax;
		        }
		        else {
		        	varR = 0.0;
		        }
		        if (greenBuffer != null) {
                    varG = greenBuffer[colors[i]]/scaleMax;
		        }
		        else {
		        	varG = 0.0;
		        }
		        if (blueBuffer != null) {
                    varB = blueBuffer[colors[i]]/scaleMax;
		        }
		        else {
		        	varB = 0.0;
		        }
                
                if (varR <= 0.04045) {
                    varR = varR/12.92;
                }
                else {
                    varR = Math.pow((varR + 0.055)/1.055, 2.4);
                }
                if (varG <= 0.04045) {
                    varG = varG/12.92;
                }
                else {
                    varG = Math.pow((varG + 0.055)/1.055, 2.4);
                }
                if (varB <= 0.04045) {
                    varB = varB/12.92;
                }
                else {
                    varB = Math.pow((varB + 0.055)/1.055, 2.4);
                }
                
                varR = 100.0 * varR;
                varG = 100.0 * varG;
                varB = 100.0 * varB;
                
                // Observer = 2 degrees, Illuminant = D65
                X = 0.4124*varR + 0.3576*varG + 0.1805*varB;
                Y = 0.2126*varR + 0.7152*varG + 0.0722*varB;
                Z = 0.0193*varR + 0.1192*varG + 0.9505*varB;
                
                varX = X/ XN;
                varY = Y/ YN;
                varZ = Z/ ZN;
                
                if (varX > 0.008856) {
                    varX = Math.pow(varX, 1.0/3.0);
                }
                else {
                    varX = (7.787 * varX) + (16.0/116.0);
                }
                if (varY > 0.008856) {
                    varY = Math.pow(varY, 1.0/3.0);
                }
                else {
                    varY = (7.787 * varY) + (16.0/116.0);
                }
                if (varZ > 0.008856) {
                    varZ = Math.pow(varZ, 1.0/3.0);
                }
                else {
                    varZ = (7.787 * varZ) + (16.0/116.0);
                }
                
                //L = (float)((116.0 * varY) - 16.0);
                a = (float)(500.0 * (varX - varY));
                b = (float)(200.0 * (varY - varZ));
                
                pos[0][i] = a;
                pos[1][i] = b;
		    } // for (i = 0; i < colorsFound; i++)	
		    colors = null;
        } // if ((redBuffer != null) || (greenBuffer != null) || (blueBuffer != null))
    	
    	nDims = pos.length;
    	nPoints = pos[0].length;
    	switch(algoSelection) {
    	case K_MEANS:
    
    	
    	switch(initSelection) {
    	case RANDOM_INIT:
	    	// Randomly choose one point as the starting centroid of each cluster
	    	startingPointIndex = new int[numberClusters];
	    	for (i = 0; i < numberClusters; i++) {
	    		startingPointIndex[i] = -1;
	    	}
	    	for (i = 0; i < nPoints; i++) {
	    		groupNum[i] = -1;
	    	}
	    	randomGen = new RandomNumberGen();
	    	Preferences.debug("\n");
	    	for (i = 0; i < numberClusters; i++) {
	    		Preferences.debug("Starting centroid " + (i+1) + "\n");
	    		do {
	    			alreadyUsed = false;
	    		    possibleStart = randomGen.genUniformRandomNum(0, nPoints - 1);
	    		    for (j = 0; j < i; j++) {
	    		    	if (startingPointIndex[j] == possibleStart) {
	    		    		alreadyUsed = true;
	    		    	}
	    		    } // for (j = 0; j < i; j++)
	    		} while (alreadyUsed);
	    		startingPointIndex[i] = possibleStart;
	    		groupNum[possibleStart] = i;
	    		for (j = 0; j < nDims; j++) {
	    		    centroidPos[j][i] = (double)pos[j][possibleStart];
	    		    Preferences.debug("Dimension " + (j+1) + " at " + centroidPos[j][i] + "\n");
	    		}
	    	} // for (i = 0; i < numberClusters; i++)
	    	startingPointIndex = null;
	    	break;
    	case BRADLEY_FAYYAD_INIT:
    		subsampleNumber = 10;
    		subsampleSize = nPoints/10;
    		subsampleIndex = new int[subsampleSize];
    		startingPointIndex = new int[numberClusters];
    		subsamplePos = new double[nDims][subsampleSize];
    		localCM = new double[nDims][numberClusters][subsampleNumber];
    		unionCM = new double[nDims][numberClusters*subsampleNumber];
    		centroidStartPos = new double[nDims][numberClusters];
    		randomGen = new RandomNumberGen();
    	    // 1.) Randomly choose one point as the starting centroid of each cluster
	    	for (i = 0; i < numberClusters; i++) {
	    		startingPointIndex[i] = -1;
	    	}
	    	randomGen = new RandomNumberGen();
	    	Preferences.debug("\n");
	    	for (i = 0; i < numberClusters; i++) {
	    		Preferences.debug("Starting centroid " + (i+1) + "\n");
	    		do {
	    			alreadyUsed = false;
	    		    possibleStart = randomGen.genUniformRandomNum(0, nPoints - 1);
	    		    for (j = 0; j < i; j++) {
	    		    	if (startingPointIndex[j] == possibleStart) {
	    		    		alreadyUsed = true;
	    		    	}
	    		    } // for (j = 0; j < i; j++)
	    		} while (alreadyUsed);
	    		startingPointIndex[i] = possibleStart;
	    		for (j = 0; j < nDims; j++) {
	    		    centroidStartPos[j][i] = (double)pos[j][possibleStart];
	    		    Preferences.debug("Dimension " + (j+1) + " at " + centroidStartPos[j][i] + "\n");
	    		}
	    	} // for (i = 0; i < numberClusters; i++)
    		for (i = 0; i < subsampleNumber; i++) {
    			// Randomly select subsampleSize of the data points
    			for (j = 0; j < subsampleSize; j++) {
    				subsampleIndex[j] = -1;
    			}
    			for (j = 0; j < subsampleSize; j++) {
    				groupNum[j] = -1;
    			}
                for (j = 0; j < subsampleSize; j++) {
                	do {
                		alreadyUsed = false;
                		possibleSample = randomGen.genUniformRandomNum(0, nPoints - 1);
                		for (k = 0; k < j; k++) {
                			if (subsampleIndex[k] == possibleSample) {
                				alreadyUsed = true;
                			}
                		} // for (k = 0; k < j; k++)
                	} while (alreadyUsed);
                	subsampleIndex[j] = possibleSample;
                	for (k = 0; k < nDims; k++) {
                		subsamplePos[k][j] = (double)pos[k][possibleSample];
                	}
                } // for (j = 0; j < subsampleSize; j++)
                // Reinitialize centroidPos with centroidStartPos
                for (j = 0; j < numberClusters; j++) {
                	for (k = 0; k < nDims; k++) {
                		centroidPos[k][j] = centroidStartPos[k][j];
                	}
                } // for (j = 0; j < numberClusters; j++)
                changeOccurred = true;
                iteration = 1;
                while (changeOccurred){
                	fireProgressStateChanged("Iteration = " + iteration + " on part 1 subsample number " + (i+1));
                	Preferences.debug("Iteration = " + iteration + " on part 1 subsample number " + (i+1));
                	iteration++;
                	changeOccurred = false;
                	for (j = 0; j < numberClusters; j++) {
            			pointsInCluster[j] = 0;
            			for (k = 0; k < numberClusters; k++) {
            				centroidDistances[j][k] = 0.0;
            			}
            		}
                	if (equalScale) {
                		for (j = 0; j < numberClusters; j++) {
            				for (k = j+1; k < numberClusters; k++) {
            					for (m = 0; m < nDims; m++) {
            						diff = centroidPos[m][j] - centroidPos[m][k];
            						centroidDistances[j][k] += diff*diff;
            					} // for (m = 0; m < nDims; m++)
            					centroidDistances[j][k] /= 4.0;
            				} // for (k = j+1; k < numberClusters; k++)
            			} // for (j = 0; j < numberClusters; j++)
                		for (j = 0; j < subsampleSize; j++) {
	        	    	    minDistSquared = 0.0;
	        	    	    originalGroupNum = groupNum[j];
	        	    	    for (m = 0; m < nDims; m++) {
        	    	    		diff = subsamplePos[m][j] - centroidPos[m][0];
        	    	    	    minDistSquared = minDistSquared + diff*diff;
        	    	    	}
	        	    	    groupNum[j] = 0;
	        	    	    for (k = 1; k < numberClusters; k++) {
	        	    	    	// Use triangle inequality to avoid unnecessary calculations
	    		    	    	if (minDistSquared > centroidDistances[groupNum[j]][k]) {
		        	    	    	distSquared = 0.0;
		        	    	    	for (m = 0; m < nDims; m++) {
		        	    	    		diff = subsamplePos[m][j] - centroidPos[m][k];
		        	    	    	    distSquared = distSquared + diff*diff;
		        	    	    	}
		        	    	    	if (distSquared < minDistSquared) {
		        	    	    		minDistSquared = distSquared;
		        	    	    		groupNum[j] = k;
		        	    	    	}
	    		    	    	} // if (minDistSquared > centroidDistances[groupNum[j]][k]) {
	        	    	    } // for (k = 1; k < numberClusters; k++)
	        	    	    pointsInCluster[groupNum[j]]++;
	        	    	    if (originalGroupNum != groupNum[j]) {
	        	    	    	changeOccurred = true;
	        	    	    }
	        	    	} // for (j = 0; j < subsampleSize; j++)	
                	} // if (equalScale)
                	else { // not equal scale
                		for (j = 0; j < numberClusters; j++) {
            				for (k = j+1; k < numberClusters; k++) {
            					for (m = 0; m < nDims; m++) {
            						diff = centroidPos[m][j] - centroidPos[m][k];
            						centroidDistances[j][k] += scale2[m]*diff*diff;
            					} // for (m = 0; m < nDims; m++)
            					centroidDistances[j][k] /= 4.0;
            				} // for (k = j+1; k < numberClusters; k++)
            			} // for (j = 0; j < numberClusters; j++)
	        	    	for (j = 0; j < subsampleSize; j++) {
	        	    	    minDistSquared = 0.0;
	        	    	    originalGroupNum = groupNum[j];
	        	    	    for (m = 0; m < nDims; m++) {
        	    	    		diff = subsamplePos[m][j] - centroidPos[m][0];
        	    	    	    minDistSquared = minDistSquared + scale2[m]*diff*diff;
        	    	    	}
	        	    	    groupNum[j] = 0;
	        	    	    for (k = 1; k < numberClusters; k++) {
	        	    	    	// Use triangle inequality to avoid unnecessary calculations
	    		    	    	if (minDistSquared > centroidDistances[groupNum[j]][k]) {
		        	    	    	distSquared = 0.0;
		        	    	    	for (m = 0; m < nDims; m++) {
		        	    	    		diff = subsamplePos[m][j] - centroidPos[m][k];
		        	    	    	    distSquared = distSquared + scale2[m]*diff*diff;
		        	    	    	}
		        	    	    	if (distSquared < minDistSquared) {
		        	    	    		minDistSquared = distSquared;
		        	    	    		groupNum[j] = k;
		        	    	    	}
	    		    	    	} // if (minDistSquared > centroidDistances[groupNum[j]][k]) {
	        	    	    } // for (k = 1; k < numberClusters; k++)
	        	    	    pointsInCluster[groupNum[j]]++;
	        	    	    if (originalGroupNum != groupNum[j]) {
	        	    	    	changeOccurred = true;
	        	    	    }
	        	    	} // for (j = 0; j < subsampleSize; j++)
                	} // else not equal scale
        	    	for (j = 0; j < numberClusters; j++) {
        	    		for (k = 0; k < nDims; k++) {
        	    			centroidPos[k][j] = 0.0;
        	    		}
        	    	}
        	    	for (j = 0; j < subsampleSize; j++) {
        	    		for (k = 0; k < nDims; k++) {
        	    			centroidPos[k][groupNum[j]] += subsamplePos[k][j];
        	    		}
        	    	}
        	    	clustersWithoutPoints = 0;
        	    	for (j = 0; j < numberClusters; j++) {
        	    		if (pointsInCluster[j] == 0) {
        	    			Preferences.debug("Cluster centroid " + (j+1) + " has no points\n");
        	    			clustersWithoutPoints++;
        	    		}
        	    		else {
	        	    		Preferences.debug("Cluster centroid " + (j+1) + ":\n");
	        	    		for (k = 0; k < nDims; k++) {
	        	    			centroidPos[k][j] = centroidPos[k][j]/pointsInCluster[j];
	        	    			Preferences.debug("Dimension " + (k+1) + " at " + centroidPos[k][j] + "\n");
	        	    		}
        	    		} // else 
        	    	}
                } // while (changeOccurred)
                Preferences.debug("There are " + clustersWithoutPoints +
                		          " clusters without points on subsample number " + (i+1) + "\n");
                s = -1;
                for (j = 0; j < clustersWithoutPoints; j++) {
                	s++;
                	while (pointsInCluster[s] > 0) {
                	    s++;	
                	}
                	maxDistSquared = 0.0;
                	if (equalScale) {
                		for (k = 0; k < numberClusters; k++) {
	                    	if (pointsInCluster[k] > 1) {
	                    	    for (m = 0; m < subsampleSize; m++) {
	                    	    	if (groupNum[m] == k) {
	                    	    	    distSquared = 0.0;
	                    	    	    for (n = 0; n < nDims; n++) {
	                    	    	        diff = subsamplePos[n][m] - centroidPos[n][k];
	                    	    	        distSquared = distSquared + diff*diff;
	                    	    	    } // for (n = 0; n < nDims; n++)'
	                    	    	    if (distSquared > maxDistSquared) {
	                    	    	    	maxDistSquared = distSquared;
	                    	    	    	maxDistPoint = m;
	                    	    	    	clusterWithMaxDistPoint = k;
	                    	    	    }
	                    	    	} // if (groupNum[m] == k)
	                    	    } // for (m = 0; m < subsampleSize; m++)
	                    	} // if (pointsInCluster[k] > 1)
	                    } // for (k = 0; k < numberClusters; k++)	
                	} // if (equalScale)
                	else { // not equalScale
	                    for (k = 0; k < numberClusters; k++) {
	                    	if (pointsInCluster[k] > 1) {
	                    	    for (m = 0; m < subsampleSize; m++) {
	                    	    	if (groupNum[m] == k) {
	                    	    	    distSquared = 0.0;
	                    	    	    for (n = 0; n < nDims; n++) {
	                    	    	        diff = subsamplePos[n][m] - centroidPos[n][k];
	                    	    	        distSquared = distSquared + scale2[n]*diff*diff;
	                    	    	    } // for (n = 0; n < nDims; n++)'
	                    	    	    if (distSquared > maxDistSquared) {
	                    	    	    	maxDistSquared = distSquared;
	                    	    	    	maxDistPoint = m;
	                    	    	    	clusterWithMaxDistPoint = k;
	                    	    	    }
	                    	    	} // if (groupNum[m] == k)
	                    	    } // for (m = 0; m < subsampleSize; m++)
	                    	} // if (pointsInCluster[k] > 1)
	                    } // for (k = 0; k < numberClusters; k++)
                	} // else not equalScale
                    groupNum[maxDistPoint] = s;
                    pointsInCluster[clusterWithMaxDistPoint]--;
                    for (k = 0; k < nDims; k++) {
                    	centroidPos[k][s] = subsamplePos[k][maxDistPoint];
                    }
                    pointsInCluster[s] = 1;
                    for (k = 0; k < nDims; k++) {
                    	centroidPos[k][clusterWithMaxDistPoint] = 0.0;
                    }
                    for (k = 0; k < subsampleSize; k++) {
                    	if (groupNum[k] == clusterWithMaxDistPoint) {
	        	    		for (m = 0; m < nDims; m++) {
	        	    			centroidPos[m][clusterWithMaxDistPoint] += subsamplePos[m][k];
	        	    		}
                    	}
        	    	}
                    for (k = 0; k < nDims; k++) {
                    	centroidPos[k][clusterWithMaxDistPoint] = 
                    		centroidPos[k][clusterWithMaxDistPoint]/pointsInCluster[clusterWithMaxDistPoint];
                    }
                } // for (j = 0; j < clustersWithoutPoints; j++)
                if (clustersWithoutPoints > 0) {
                    	Preferences.debug("Redoing k means on subsample number " + (i+1) + "\n");
                    	changeOccurred = true;
                        iteration = 1;
                        while (changeOccurred){
                        	fireProgressStateChanged("Iteration = " + iteration + " on part 1 subsample number " + (i+1));
                        	Preferences.debug("Iteration = " + iteration + " on part 1 subsample number " + (i+1));
                        	iteration++;
                        	changeOccurred = false;
                        	for (j = 0; j < numberClusters; j++) {
                    			pointsInCluster[j] = 0;
                    			for (k = 0; k < numberClusters; k++) {
                    				centroidDistances[j][k] = 0.0;
                    			}
                    		}
                        	if (equalScale) {
                        		for (j = 0; j < numberClusters; j++) {
                    				for (k = j+1; k < numberClusters; k++) {
                    					for (m = 0; m < nDims; m++) {
                    						diff = centroidPos[m][j] - centroidPos[m][k];
                    						centroidDistances[j][k] += diff*diff;
                    					} // for (m = 0; m < nDims; m++)
                    					centroidDistances[j][k] /= 4.0;
                    				} // for (k = j+1; k < numberClusters; k++)
                    			} // for (j = 0; j < numberClusters; j++)
                        		for (j = 0; j < subsampleSize; j++) {
	                	    	    minDistSquared = 0.0;
	                	    	    originalGroupNum = groupNum[j];
	                	    	    for (m = 0; m < nDims; m++) {
                	    	    		diff = subsamplePos[m][j] - centroidPos[m][0];
                	    	    	    minDistSquared = minDistSquared + diff*diff;
                	    	    	}
	                	    	    groupNum[j] = 0;
	                	    	    for (k = 1; k < numberClusters; k++) {
	                	    	    	// Use triangle inequality to avoid unnecessary calculations
	    	    		    	    	if (minDistSquared > centroidDistances[groupNum[j]][k]) {
		                	    	    	distSquared = 0.0;
		                	    	    	for (m = 0; m < nDims; m++) {
		                	    	    		diff = subsamplePos[m][j] - centroidPos[m][k];
		                	    	    	    distSquared = distSquared + diff*diff;
		                	    	    	}
		                	    	    	if (distSquared < minDistSquared) {
		                	    	    		minDistSquared = distSquared;
		                	    	    		groupNum[j] = k;
		                	    	    	}
	    	    		    	    	} // if (minDistSquared > centroidDistances[groupNum[j]][k])
	                	    	    } // for (k = 1; k < numberClusters; k++)
	                	    	    pointsInCluster[groupNum[j]]++;
	                	    	    if (originalGroupNum != groupNum[j]) {
	                	    	    	changeOccurred = true;
	                	    	    }
	                	    	} // for (j = 0; j < subsampleSize; j++)	
                        	} // if (equalScale)
                        	else { // not equalScale
                        		for (j = 0; j < numberClusters; j++) {
                    				for (k = j+1; k < numberClusters; k++) {
                    					for (m = 0; m < nDims; m++) {
                    						diff = centroidPos[m][j] - centroidPos[m][k];
                    						centroidDistances[j][k] += scale2[m]*diff*diff;
                    					} // for (m = 0; m < nDims; m++)
                    					centroidDistances[j][k] /= 4.0;
                    				} // for (k = j+1; k < numberClusters; k++)
                    			} // for (j = 0; j < numberClusters; j++)
	                	    	for (j = 0; j < subsampleSize; j++) {
	                	    	    minDistSquared = 0.0;
	                	    	    originalGroupNum = groupNum[j];
	                	    	    for (m = 0; m < nDims; m++) {
                	    	    		diff = subsamplePos[m][j] - centroidPos[m][0];
                	    	    	    minDistSquared = minDistSquared + scale2[m]*diff*diff;
                	    	    	}
	                	    	    groupNum[j] = 0;
	                	    	    for (k = 1; k < numberClusters; k++) {
	                	    	    	// Use triangle inequality to avoid unnecessary calculations
	    	    		    	    	if (minDistSquared > centroidDistances[groupNum[j]][k]) {
		                	    	    	distSquared = 0.0;
		                	    	    	for (m = 0; m < nDims; m++) {
		                	    	    		diff = subsamplePos[m][j] - centroidPos[m][k];
		                	    	    	    distSquared = distSquared + scale2[m]*diff*diff;
		                	    	    	}
		                	    	    	if (distSquared < minDistSquared) {
		                	    	    		minDistSquared = distSquared;
		                	    	    		groupNum[j] = k;
		                	    	    	}
	    	    		    	    	} // if (minDistSquared > centroidDistances[groupNum[j]][k])
	                	    	    } // for (k = 1; k < numberClusters; k++)
	                	    	    pointsInCluster[groupNum[j]]++;
	                	    	    if (originalGroupNum != groupNum[j]) {
	                	    	    	changeOccurred = true;
	                	    	    }
	                	    	} // for (j = 0; j < subsampleSize; j++)
                        	} // else not equalScale
                	    	for (j = 0; j < numberClusters; j++) {
                	    		for (k = 0; k < nDims; k++) {
                	    			centroidPos[k][j] = 0.0;
                	    		}
                	    	}
                	    	for (j = 0; j < subsampleSize; j++) {
                	    		for (k = 0; k < nDims; k++) {
                	    			centroidPos[k][groupNum[j]] += subsamplePos[k][j];
                	    		}
                	    	}
                	    	clustersWithoutPoints = 0;
                	    	for (j = 0; j < numberClusters; j++) {
                	    		if (pointsInCluster[j] == 0) {
                	    			Preferences.debug("Cluster centroid " + (j+1) + " has no points\n");
                	    			clustersWithoutPoints++;
                	    		}
                	    		else {
        	        	    		Preferences.debug("Cluster centroid " + (j+1) + ":\n");
        	        	    		for (k = 0; k < nDims; k++) {
        	        	    			centroidPos[k][j] = centroidPos[k][j]/pointsInCluster[j];
        	        	    			Preferences.debug("Dimension " + (k+1) + " at " + centroidPos[k][j] + "\n");
        	        	    		}
                	    		} // else 
                	    	}
                        } // while (changeOccurred)
                        Preferences.debug("There are " + clustersWithoutPoints +
                        		          " clusters without points on subsample number " + (i+1) + "\n");
                } // if (clustersWithoutPoints > 0)
                for (j = 0; j < numberClusters; j++) {
                	for (k = 0; k < nDims; k++) {
                	    localCM[k][j][i] = centroidPos[k][j];
                	}
                }
                CMPointsAdded = 0;
                for (j = 0; j < numberClusters; j++) {
                	haveCMPoint = false;
                	for (m = 0; m < numberCMPoints && (!haveCMPoint); m++) {
                		haveCMPoint = true;
                		for (n = 0; n < nDims; n++) {
                			if (unionCM[n][m] != centroidPos[n][j]) {
                				haveCMPoint = false;
                			}
                		} // for (n = 0; n < nDims; n++)
                	} // for (m = 0; m < numberCMPoints && (!haveCMPoint); m++)
                	if (!haveCMPoint) {
                		CMPointsAdded++;
                		for (m = 0; m < nDims; m++) {
                			unionCM[m][numberCMPoints + CMPointsAdded - 1] = centroidPos[m][j];
                		} // for (m = 0; m < nDims; m++)
                	} // if (!haveCMPoint)
                } // for (j = 0; j < numberClusters; j++)
                numberCMPoints = numberCMPoints + CMPointsAdded;
    		} // for (i = 1; i <= subsampleNumber; i++)
    		subsampleSize = numberCMPoints;
    		localFM = new double[nDims][numberClusters][subsampleNumber];
    		
            for (i = 0; i < subsampleNumber; i++) {
            	for (j = 0; j < subsampleSize; j++) {
            		groupNum[j] = -1;
            	}
            	for (j = 0; j < numberClusters; j++) {
            		for (k = 0; k < nDims; k++) {
            			centroidPos[k][j] = localCM[k][j][i];
            		}
            	}
            	changeOccurred = true;
                iteration = 1;
                while (changeOccurred){
                	fireProgressStateChanged("Iteration = " + iteration + " on part 2 subsample number " + (i+1));
                	Preferences.debug("Iteration = " + iteration + " on part 2 subsample number " + (i+1));
                	iteration++;
                	changeOccurred = false;
                	for (j = 0; j < numberClusters; j++) {
            			pointsInCluster[j] = 0;
            		}
                	if (equalScale) {
                		for (j = 0; j < subsampleSize; j++) {
	        	    	    minDistSquared = Double.MAX_VALUE;
	        	    	    originalGroupNum = groupNum[j];
	        	    	    for (k = 0; k < numberClusters; k++) {
	        	    	    	distSquared = 0.0;
	        	    	    	for (m = 0; m < nDims; m++) {
	        	    	    		diff = unionCM[m][j] - centroidPos[m][k];
	        	    	    	    distSquared = distSquared + diff*diff;
	        	    	    	}
	        	    	    	if (distSquared < minDistSquared) {
	        	    	    		minDistSquared = distSquared;
	        	    	    		groupNum[j] = k;
	        	    	    	}
	        	    	    } // for (k = 0; k < numberClusters; k++)
	        	    	    pointsInCluster[groupNum[j]]++;
	        	    	    if (originalGroupNum != groupNum[j]) {
	        	    	    	changeOccurred = true;
	        	    	    }
	        	    	} // for (j = 0; j < subsampleSize; j++)	
                	} // if (equalScale)
                	else { // not equalScale
	        	    	for (j = 0; j < subsampleSize; j++) {
	        	    	    minDistSquared = Double.MAX_VALUE;
	        	    	    originalGroupNum = groupNum[j];
	        	    	    for (k = 0; k < numberClusters; k++) {
	        	    	    	distSquared = 0.0;
	        	    	    	for (m = 0; m < nDims; m++) {
	        	    	    		diff = unionCM[m][j] - centroidPos[m][k];
	        	    	    	    distSquared = distSquared + scale2[m]*diff*diff;
	        	    	    	}
	        	    	    	if (distSquared < minDistSquared) {
	        	    	    		minDistSquared = distSquared;
	        	    	    		groupNum[j] = k;
	        	    	    	}
	        	    	    } // for (k = 0; k < numberClusters; k++)
	        	    	    pointsInCluster[groupNum[j]]++;
	        	    	    if (originalGroupNum != groupNum[j]) {
	        	    	    	changeOccurred = true;
	        	    	    }
	        	    	} // for (j = 0; j < subsampleSize; j++)
                	} // else not equalScale
        	    	for (j = 0; j < numberClusters; j++) {
        	    		for (k = 0; k < nDims; k++) {
        	    			centroidPos[k][j] = 0.0;
        	    		}
        	    	}
        	    	for (j = 0; j < subsampleSize; j++) {
        	    		for (k = 0; k < nDims; k++) {
        	    			centroidPos[k][groupNum[j]] += unionCM[k][j];
        	    		}
        	    	}
        	    	clustersWithoutPoints = 0;
        	    	for (j = 0; j < numberClusters; j++) {
        	    		if (pointsInCluster[j] == 0) {
        	    			Preferences.debug("Cluster centroid " + (j+1) + " has no points\n");
        	    			clustersWithoutPoints++;
        	    		}
        	    		else {
	        	    		Preferences.debug("Cluster centroid " + (j+1) + ":\n");
	        	    		for (k = 0; k < nDims; k++) {
	        	    			centroidPos[k][j] = centroidPos[k][j]/pointsInCluster[j];
	        	    			Preferences.debug("Dimension " + (k+1) + " at " + centroidPos[k][j] + "\n");
	        	    		}
        	    		} // else 
        	    	}
                } // while (changeOccurred)
                Preferences.debug("There are " + clustersWithoutPoints +
                		          " clusters without points on subsample number " + (i+1) + "\n");	
                for (j = 0; j < numberClusters; j++) {
                	for (k = 0; k < nDims; k++) {
                	    localFM[k][j][i] = centroidPos[k][j];
                	}
                }
            } // for (i = 0; i < subsampleNumber; i++)
                
            // The refined initial point is chosen as the localFM[][][i] having minimal distortion over unionCM
            totalDistSquared = new double[subsampleNumber];
            minTotalDistSquared = Double.MAX_VALUE;
            if (equalScale) {
            	for (i = 0; i < subsampleNumber; i++) {
	                for (j = 0; j < subsampleSize; j++) {
	                    minDistSquared = Double.MAX_VALUE;
	                    for (k = 0; k < numberClusters; k++) {
	                        distSquared = 0.0;
	                        for (m = 0; m < nDims; m++) {
	                        	diff = unionCM[m][j] - localFM[m][k][i];
	                        	distSquared = distSquared + diff*diff;
	                        }
	                        if (distSquared < minDistSquared) {
	                        	minDistSquared = distSquared;
	                        }
	                    } // for (k = 0; k < numberClusters; k++)
	                    totalDistSquared[i] = totalDistSquared[i] + minDistSquared;
	                } // for (j = 0; j < subsampleSize; j++)
	                if (totalDistSquared[i] < minTotalDistSquared) {
	                	minTotalDistSquared = totalDistSquared[i];
	                	bestFMIndex = i;
	                }
	            } // for (i = 0; i < subsampleNumber; i++)	
            } // if (equalScale)
            else { // not equalScale
	            for (i = 0; i < subsampleNumber; i++) {
	                for (j = 0; j < subsampleSize; j++) {
	                    minDistSquared = Double.MAX_VALUE;
	                    for (k = 0; k < numberClusters; k++) {
	                        distSquared = 0.0;
	                        for (m = 0; m < nDims; m++) {
	                        	diff = unionCM[m][j] - localFM[m][k][i];
	                        	distSquared = distSquared + scale2[m]*diff*diff;
	                        }
	                        if (distSquared < minDistSquared) {
	                        	minDistSquared = distSquared;
	                        }
	                    } // for (k = 0; k < numberClusters; k++)
	                    totalDistSquared[i] = totalDistSquared[i] + minDistSquared;
	                } // for (j = 0; j < subsampleSize; j++)
	                if (totalDistSquared[i] < minTotalDistSquared) {
	                	minTotalDistSquared = totalDistSquared[i];
	                	bestFMIndex = i;
	                }
	            } // for (i = 0; i < subsampleNumber; i++)
            } // else not equalScale
            Preferences.debug("Refinement algorithm returns inital centroids at:\n");
            
            for (i = 0; i < numberClusters; i++) {
            	Preferences.debug("Initial centroid " + (i+1) + "\n");
            	for (j = 0; j < nDims; j++) {
            		centroidPos[j][i] = localFM[j][i][bestFMIndex];
            		Preferences.debug("Dimension " + (j+1) + " at " + centroidPos[j][i] + "\n");
            	}
            }
               
    		break;
    	case HIERARCHICAL_GROUPING_INIT:
    		pointsInGroup = new int[nPoints];
    		for (i = 0; i < nPoints; i++) {
    			pointsInGroup[i] = 1;
    		}
    		highestGroupPresent = nPoints - 1;
    		hierGroup = new int[nPoints][];
    		for (i = 0; i < nPoints; i++) {
    			hierGroup[i] = new int[nPoints-i];
    			hierGroup[i][0] = i;
    		}
    		essGroup = new double[nPoints];
    		for (i = 0; i < nPoints; i++) {
    			essGroup[i] = 0.0;
    		}
    		if (equalScale) {
    			for (groupsPresent = nPoints; groupsPresent > numberClusters; groupsPresent--) {
	    			minessIncrease = Double.MAX_VALUE;
		    		for (i = 0; i < highestGroupPresent; i++) {
		    			if (pointsInGroup[i] > 0) {
		    				for (j = i+1; j <= highestGroupPresent; j++) {
		    					if (pointsInGroup[j] > 0) {
		    					    newPointsInGroup = pointsInGroup[i] + pointsInGroup[j];
		    					    newess = 0.0;
		    					    for (m = 0; m < nDims; m++) {
		    					        sum = 0.0;
		    					        sumSq = 0.0;
		    					        for (n = 0; n < pointsInGroup[i]; n++) {
		    					        	sum += pos[m][hierGroup[i][n]];
		    					        	sumSq += pos[m][hierGroup[i][n]]*pos[m][hierGroup[i][n]];
		    					        }
		    					        for (n = 0; n < pointsInGroup[j]; n++) {
		    					        	sum += pos[m][hierGroup[j][n]];
		    					        	sumSq += pos[m][hierGroup[j][n]]*pos[m][hierGroup[j][n]];
		    					        }
		    					        newess += (sumSq - sum*sum/newPointsInGroup);
		    					    } // for (m = 0; m < nDims; m++)
		    					    essIncrease = newess - (essGroup[i] + essGroup[j]);
		    					    if (essIncrease < minessIncrease) {
		    					    	minessIncrease = essIncrease;
		    					    	bestFirstIndex = i;
		    					    	bestSecondIndex = j;
		    					    	bestnewess = newess;
		    					    	bestNewPointsInGroup = newPointsInGroup;
		    					    } // if (essIncrease < minessIncrease)
		    					} // if (pointsInGroup[j] > 0)
		    				} // for (j = i+1; j <= highestGroupPresent; j++)
		    			} // if (pointsInGroup[i] > 0)
		    		} // for (i = 0; i < highestGroupPresent; i++)
		    		for (i = pointsInGroup[bestFirstIndex]; i < bestNewPointsInGroup; i++) {
		    	        hierGroup[bestFirstIndex][i] = hierGroup[bestSecondIndex][i-pointsInGroup[bestFirstIndex]];
		    		}
		    		pointsInGroup[bestFirstIndex] = bestNewPointsInGroup;
		    		pointsInGroup[bestSecondIndex] = 0;
		    		essGroup[bestFirstIndex] = bestnewess;
		    		found = false;
		    		for (i = highestGroupPresent; (i >= 0) && (!found); i--) {
		    			if (pointsInGroup[i] > 0) {
		    			    highestGroupPresent = i;
		    			    found = true;
		    			}
		    		}
	    		} // for (groupsPresent = nPoints; groupsPresent > numberClusters; groupsPresent--)	
    		} // if (equalScale)
    		else { // not equalScale
	    		for (groupsPresent = nPoints; groupsPresent > numberClusters; groupsPresent--) {
	    			minessIncrease = Double.MAX_VALUE;
		    		for (i = 0; i < highestGroupPresent; i++) {
		    			if (pointsInGroup[i] > 0) {
		    				for (j = i+1; j <= highestGroupPresent; j++) {
		    					if (pointsInGroup[j] > 0) {
		    					    newPointsInGroup = pointsInGroup[i] + pointsInGroup[j];
		    					    newess = 0.0;
		    					    for (m = 0; m < nDims; m++) {
		    					        sum = 0.0;
		    					        sumSq = 0.0;
		    					        for (n = 0; n < pointsInGroup[i]; n++) {
		    					        	sum += pos[m][hierGroup[i][n]];
		    					        	sumSq += pos[m][hierGroup[i][n]]*pos[m][hierGroup[i][n]];
		    					        }
		    					        for (n = 0; n < pointsInGroup[j]; n++) {
		    					        	sum += pos[m][hierGroup[j][n]];
		    					        	sumSq += pos[m][hierGroup[j][n]]*pos[m][hierGroup[j][n]];
		    					        }
		    					        newess += scale2[m]*(sumSq - sum*sum/newPointsInGroup);
		    					    } // for (m = 0; m < nDims; m++)
		    					    essIncrease = newess - (essGroup[i] + essGroup[j]);
		    					    if (essIncrease < minessIncrease) {
		    					    	minessIncrease = essIncrease;
		    					    	bestFirstIndex = i;
		    					    	bestSecondIndex = j;
		    					    	bestnewess = newess;
		    					    	bestNewPointsInGroup = newPointsInGroup;
		    					    } // if (essIncrease < minessIncrease)
		    					} // if (pointsInGroup[j] > 0)
		    				} // for (j = i+1; j <= highestGroupPresent; j++)
		    			} // if (pointsInGroup[i] > 0)
		    		} // for (i = 0; i < highestGroupPresent; i++)
		    		for (i = pointsInGroup[bestFirstIndex]; i < bestNewPointsInGroup; i++) {
		    	        hierGroup[bestFirstIndex][i] = hierGroup[bestSecondIndex][i-pointsInGroup[bestFirstIndex]];
		    		}
		    		pointsInGroup[bestFirstIndex] = bestNewPointsInGroup;
		    		pointsInGroup[bestSecondIndex] = 0;
		    		essGroup[bestFirstIndex] = bestnewess;
		    		found = false;
		    		for (i = highestGroupPresent; (i >= 0) && (!found); i--) {
		    			if (pointsInGroup[i] > 0) {
		    			    highestGroupPresent = i;
		    			    found = true;
		    			}
		    		}
	    		} // for (groupsPresent = nPoints; groupsPresent > numberClusters; groupsPresent--)
    		} // else not equalScale
    		groupIndex = -1;
    		for (i = 0; i < numberClusters; i++) {
    			for (j = 0; j < nDims; j++) {
    				centroidPos[j][i] = 0.0;
    			}
    		}
    		for (i = 0; (i < highestGroupPresent) && (groupIndex < numberClusters-1); i++) {
    			if (pointsInGroup[i] > 0) {
    			    groupIndex++;
    			    for (j = 0; j < pointsInGroup[i]; j++) {
    			    	groupNum[hierGroup[i][j]] = groupIndex;
    			    	for (k = 0; k < nDims; k++) {
    			    	    centroidPos[k][groupIndex] += pos[k][hierGroup[i][j]];	
    			    	}
    			    } // for (j = 0; j < pointsInGroup[i]; j++)
    			    for (j = 0; j < nDims; j++) {
    			    	centroidPos[j][groupIndex] = centroidPos[j][groupIndex]/pointsInGroup[i];
    			    }
    			} // if (pointsInGroun[i] > 0)
    		}
    		break;
    	case MAXMIN_INIT:
    		for (i = 0; i < nPoints; i++) {
    			groupNum[i] = -1;
    		}
    		// Obtain the 2 point furtherest apart as seeds
    		maxDistSquared = 0.0;
    		if (equalScale) {
    			for (i = 0; i < nPoints-1; i++) {
	    			for (j = i+1; j <= nPoints-1; j++) {
	    			    distSquared = 0.0;
	    			    for (k = 0; k < nDims; k++) {
	    			    	diff = pos[k][i] - pos[k][j];
	    			    	distSquared += diff*diff;
	    			    } // for (k = 0; k < nDims; k++)
	    			    if (distSquared > maxDistSquared) {
	    			    	maxDistSquared = distSquared;
	    			    	bestFirstIndex = i;
	    			    	bestSecondIndex = j;
	    			    }
	    			} // for (j = i+1; j <= nPoints-1; j++)
	    		} // for (i = 0; i < nPoints-1; i++)	
    		} // if (equalScale)
    		else { // not equalScale
	    		for (i = 0; i < nPoints-1; i++) {
	    			for (j = i+1; j <= nPoints-1; j++) {
	    			    distSquared = 0.0;
	    			    for (k = 0; k < nDims; k++) {
	    			    	diff = pos[k][i] - pos[k][j];
	    			    	distSquared += scale2[k]*diff*diff;
	    			    } // for (k = 0; k < nDims; k++)
	    			    if (distSquared > maxDistSquared) {
	    			    	maxDistSquared = distSquared;
	    			    	bestFirstIndex = i;
	    			    	bestSecondIndex = j;
	    			    }
	    			} // for (j = i+1; j <= nPoints-1; j++)
	    		} // for (i = 0; i < nPoints-1; i++)
    		} // else not equalScale
    		for (i = 0; i < nDims; i++) {
    			centroidPos[i][0] = pos[i][bestFirstIndex];
    			centroidPos[i][1] = pos[i][bestSecondIndex];
    		}
    		groupNum[bestFirstIndex] = 0;
    		groupNum[bestSecondIndex] = 1;
    		if (numberClusters > 2) {
    		    minDistSquaredSet = new double[numberClusters - 1];
    		    minIndexSet = new int[numberClusters - 1];
    		}
    		if (equalScale) {
    			for (currentClusters = 2; currentClusters < numberClusters; currentClusters++) {
	    			for (i = 0; i < nPoints; i++) {
	    			    if (groupNum[i] == -1) {
	    			    	for (j = 0; j < currentClusters; j++) {
	    			    		minDistSquaredSet[j] = Double.MAX_VALUE;
	    			    	}
	    			    	for (j = 0; j < currentClusters; j++) {
	    			    	    distSquared = 0.0;
	    			    	    for (k = 0; k < nDims; k++) {
	    			    	    	diff = pos[k][i] - centroidPos[k][j];
	    			    	    	distSquared += diff*diff;
	    			    	    } // for (k = 0; k < nDims; k++)
	    			    	    if (distSquared < minDistSquaredSet[j]) {
	    			    	    	minDistSquaredSet[j] = distSquared;
	    			    	    	minIndexSet[j] = i;
	    			    	    }
	    			    	} // for (j = 0; j < currentClusters; j++)
	    			    } // if (groupNum[i] == -1)
	    			} // for (i = 0; i < nPoints; i++)
	    			maxDistSquared = 0.0;
	    			for (i = 0; i < currentClusters; i++) {
	    			    if (minDistSquaredSet[i] > maxDistSquared) {
	    			    	maxDistSquared = minDistSquaredSet[i];
	    			    	newIndex = minIndexSet[i];
	    			    }
	    			} // for (i = 0; i < currentClusters; i++)
	    			for (i = 0; i < nDims; i++) {
	    				centroidPos[i][currentClusters] = pos[i][newIndex];
	    			}
	    			groupNum[newIndex] = currentClusters;
	    		} // for (currentClusters = 2; currentClusters < numberClusters; currentClusters++)	
    		} // if (equalScale)
    		else { // not equalScale
	    		for (currentClusters = 2; currentClusters < numberClusters; currentClusters++) {
	    			for (i = 0; i < nPoints; i++) {
	    			    if (groupNum[i] == -1) {
	    			    	for (j = 0; j < currentClusters; j++) {
	    			    		minDistSquaredSet[j] = Double.MAX_VALUE;
	    			    	}
	    			    	for (j = 0; j < currentClusters; j++) {
	    			    	    distSquared = 0.0;
	    			    	    for (k = 0; k < nDims; k++) {
	    			    	    	diff = pos[k][i] - centroidPos[k][j];
	    			    	    	distSquared += scale2[k]*diff*diff;
	    			    	    } // for (k = 0; k < nDims; k++)
	    			    	    if (distSquared < minDistSquaredSet[j]) {
	    			    	    	minDistSquaredSet[j] = distSquared;
	    			    	    	minIndexSet[j] = i;
	    			    	    }
	    			    	} // for (j = 0; j < currentClusters; j++)
	    			    } // if (groupNum[i] == -1)
	    			} // for (i = 0; i < nPoints; i++)
	    			maxDistSquared = 0.0;
	    			for (i = 0; i < currentClusters; i++) {
	    			    if (minDistSquaredSet[i] > maxDistSquared) {
	    			    	maxDistSquared = minDistSquaredSet[i];
	    			    	newIndex = minIndexSet[i];
	    			    }
	    			} // for (i = 0; i < currentClusters; i++)
	    			for (i = 0; i < nDims; i++) {
	    				centroidPos[i][currentClusters] = pos[i][newIndex];
	    			}
	    			groupNum[newIndex] = currentClusters;
	    		} // for (currentClusters = 2; currentClusters < numberClusters; currentClusters++)
    		} // else not equalScale
    		break;
    	} // switch(initSelection)
    	
    	changeOccurred = true;
    	iteration = 1;
    	Preferences.debug("\n");
    	while (changeOccurred) {
    		fireProgressStateChanged("Iteration = " + iteration);
    		Preferences.debug("Iteration = " + iteration + "\n");
    		iteration++;
    		changeOccurred = false;
    		for (i = 0; i < numberClusters; i++) {
    			pointsInCluster[i] = 0;
    			for (j = 0; j < numberClusters; j++) {
    				centroidDistances[i][j] = 0.0;
    			}
    		}
    		if (equalScale) {
    			for (i = 0; i < numberClusters; i++) {
    				for (j = i+1; j < numberClusters; j++) {
    					for (k = 0; k < nDims; k++) {
    						diff = centroidPos[k][i] - centroidPos[k][j];
    						centroidDistances[i][j] += diff*diff;
    					} // for (k = 0; k < nDims; k++)
    					centroidDistances[i][j] /= 4.0;
    				} // for (j = i+1; j < numberClusters; j++)
    			} // for (i = 0; i < numberClusters; i++)
    			for (i = 0; i < nPoints; i++) {
		    	    minDistSquared = 0.0;
		    	    originalGroupNum = groupNum[i];
	    	    	for (k = 0; k < nDims; k++) {
	    	    		diff = pos[k][i] - centroidPos[k][0];
	    	    	    minDistSquared = minDistSquared + diff*diff;
	    	    	}
	    	    	groupNum[i] = 0;
		    	    for (j = 1; j < numberClusters; j++) {
		    	    	// Use triangle inequality to avoid unnecessary calculations
		    	    	if (minDistSquared > centroidDistances[groupNum[i]][j]) {
			    	    	distSquared = 0.0;
			    	    	for (k = 0; k < nDims; k++) {
			    	    		diff = pos[k][i] - centroidPos[k][j];
			    	    	    distSquared = distSquared + diff*diff;
			    	    	}
			    	    	if (distSquared < minDistSquared) {
			    	    		minDistSquared = distSquared;
			    	    		groupNum[i] = j;
			    	    	}
		    	    	} // if (minDistSquared > centroidDistances[groupNum[i]][j]))
		    	    } // for (j = 1; j < numberClusters; j++)
		    	    pointsInCluster[groupNum[i]]++;
		    	    if (originalGroupNum != groupNum[i]) {
		    	    	changeOccurred = true;
		    	    }
		    	} // for (i = 0; i < nPoints; i++)	
    		} // if (equalScale)
    		else { // not equalScale
    			for (i = 0; i < numberClusters; i++) {
    				for (j = i+1; j < numberClusters; j++) {
    					for (k = 0; k < nDims; k++) {
    						diff = centroidPos[k][i] - centroidPos[k][j];
    						centroidDistances[i][j] += scale2[k]*diff*diff;
    					} // for (k = 0; k < nDims; k++)
    					centroidDistances[i][j] /= 4.0;
    				} // for (j = i+1; j < numberClusters; j++)
    			} // for (i = 0; i < numberClusters; i++)
		    	for (i = 0; i < nPoints; i++) {
		    		minDistSquared = 0.0;
		    	    originalGroupNum = groupNum[i];
	    	    	for (k = 0; k < nDims; k++) {
	    	    		diff = pos[k][i] - centroidPos[k][0];
	    	    	    minDistSquared = minDistSquared + scale2[k]*diff*diff;
	    	    	}
	    	    	groupNum[i] = 0;
		    	    for (j = 1; j < numberClusters; j++) {
		    	    	// Use triangle inequality to avoid unnecessary calculations
		    	    	if (minDistSquared > centroidDistances[groupNum[i]][j]) {
			    	    	distSquared = 0.0;
			    	    	for (k = 0; k < nDims; k++) {
			    	    		diff = pos[k][i] - centroidPos[k][j];
			    	    	    distSquared = distSquared + scale2[k]*diff*diff;
			    	    	}
			    	    	if (distSquared < minDistSquared) {
			    	    		minDistSquared = distSquared;
			    	    		groupNum[i] = j;
			    	    	}
		    	    	} // if (minDistSquared > centroidDistances[groupNum[i]][j]))
		    	    } // for (j = 1; j < numberClusters; j++)
		    	    pointsInCluster[groupNum[i]]++;
		    	    if (originalGroupNum != groupNum[i]) {
		    	    	changeOccurred = true;
		    	    }
		    	} // for (i = 0; i < nPoints; i++)
    		} // else not equalScale
	    	for (i = 0; i < numberClusters; i++) {
	    		for (j = 0; j < nDims; j++) {
	    			centroidPos[j][i] = 0.0;
	    		}
	    	}
	    	if (useColorHistogram) {
	    	    for (i = 0; i < numberClusters; i++) {
	    	    	totalWeight[i] = 0.0;
	    	    }
	    		for (i = 0; i < nPoints; i++) {
	    			totalWeight[groupNum[i]] += weight[i];
		    		for (j = 0; j < nDims; j++) {
		    			centroidPos[j][groupNum[i]] += pos[j][i]*weight[i];
		    		}
		    	}
		    	clustersWithoutPoints = 0;
		    	for (i = 0; i < numberClusters; i++) {
		    		if (pointsInCluster[i] == 0) {
		    			Preferences.debug("Cluster centroid " + (i+1) + " has no points\n");
		    			clustersWithoutPoints++;
		    		}
		    		else {
			    		Preferences.debug("Cluster centroid " + (i+1) + ":\n");
			    		for (j = 0; j < nDims; j++) {
			    			centroidPos[j][i] = centroidPos[j][i]/totalWeight[i];
			    			Preferences.debug("Dimension " + (j+1) + " at " + centroidPos[j][i] + "\n");
			    		}
		    		} // else
		    	}	
	    	} // if (useColorHistogram)
	    	else { // no color histogram
		    	for (i = 0; i < nPoints; i++) {
		    		for (j = 0; j < nDims; j++) {
		    			centroidPos[j][groupNum[i]] += pos[j][i];
		    		}
		    	}
		    	clustersWithoutPoints = 0;
		    	for (i = 0; i < numberClusters; i++) {
		    		if (pointsInCluster[i] == 0) {
		    			Preferences.debug("Cluster centroid " + (i+1) + " has no points\n");
		    			clustersWithoutPoints++;
		    		}
		    		else {
			    		Preferences.debug("Cluster centroid " + (i+1) + ":\n");
			    		for (j = 0; j < nDims; j++) {
			    			centroidPos[j][i] = centroidPos[j][i]/pointsInCluster[i];
			    			Preferences.debug("Dimension " + (j+1) + " at " + centroidPos[j][i] + "\n");
			    		}
		    		} // else
		    	}
	    	} // else no colorHistogram
    	} // while (changeOccurred)
    	Preferences.debug("There are " + clustersWithoutPoints + " clusters without points\n");
    	break;
    	case GLOBAL_K_MEANS:
    		Preferences.debug("Finding centroid for 1 cluster\n");
    		for (i = 0; i < nPoints; i++) {
	    		groupNum[i] = 0;
	    	}
    		centroidPosStart = new double[nDims][numberClusters];
    		pointsInCluster[0] = nPoints;
    		for (j = 0; j < nDims; j++) {
    			centroidPosStart[j][0] = 0.0;
    		}
	    	if (useColorHistogram) {
	    		totalWeight[0] = 0.0;
	    		for (i = 0; i < nPoints; i++) {
	    			totalWeight[0] += weight[i];
		    		for (j = 0; j < nDims; j++) {
		    			centroidPosStart[j][0] += pos[j][i]*weight[i];
		    		}
		    	}
	    		for (j = 0; j < nDims; j++) {
	    			centroidPosStart[j][0] = centroidPosStart[j][0]/totalWeight[0];
	    		}
	    	} // if (useColorHistogram)
	    	else { // no color histogram
		    	for (i = 0; i < nPoints; i++) {
		    		for (j = 0; j < nDims; j++) {
		    			centroidPosStart[j][0] += pos[j][i];
		    		}
		    	}
	    		for (j = 0; j < nDims; j++) {
	    			centroidPosStart[j][0] = centroidPosStart[j][0]/pointsInCluster[0];
	    		}
	    	} // else no colorHistogram
	    	for (presentClusters = 2; presentClusters <= numberClusters; presentClusters++) {
	    		Preferences.debug("Present cluster number = " + presentClusters + "\n");
	    		fireProgressStateChanged("Present cluster number = " + presentClusters);
	    		bestTotalMinDistSquared = Double.MAX_VALUE;
	    	    for (initialClusterLocation = 0; initialClusterLocation < nPoints; initialClusterLocation++) {
	    	    	changeOccurred = true;
	    	    	iteration = 1;
	    	    	for (i = 0; i < nDims; i++) {
	    	    		for (j = 0; j < presentClusters-1; j++) {
	    	    			centroidPos[i][j] = centroidPosStart[i][j];
	    	    		}
	    	    		centroidPos[i][presentClusters-1] = pos[i][initialClusterLocation];
	    	    	}
	    	    	while (changeOccurred) {
	    	    		iteration++;
	    	    		changeOccurred = false;
	    	    		totalMinDistSquared = 0.0;
	    	    		for (i = 0; i < presentClusters; i++) {
	    	    			pointsInCluster[i] = 0;
	    	    			for (j = 0; j < presentClusters; j++) {
	    	    				centroidDistances[i][j] = 0.0;
	    	    			}
	    	    		}
	    	    		if (equalScale) {
	    	    			for (i = 0; i < presentClusters; i++) {
	    	    				for (j = i+1; j < presentClusters; j++) {
	    	    					for (k = 0; k < nDims; k++) {
	    	    						diff = centroidPos[k][i] - centroidPos[k][j];
	    	    						centroidDistances[i][j] += diff*diff;
	    	    					} // for (k = 0; k < nDims; k++)
	    	    					centroidDistances[i][j] /= 4.0;
	    	    				} // for (j = i+1; j < presentClusters; j++)
	    	    			} // for (i = 0; i < presentClusters; i++)
	    	    			for (i = 0; i < nPoints; i++) {
	    			    	    minDistSquared = 0.0;
	    			    	    originalGroupNum = groupNum[i];
	    		    	    	for (k = 0; k < nDims; k++) {
	    		    	    		diff = pos[k][i] - centroidPos[k][0];
	    		    	    	    minDistSquared = minDistSquared + diff*diff;
	    		    	    	}
	    		    	    	groupNum[i] = 0;
	    			    	    for (j = 1; j < presentClusters; j++) {
	    			    	    	// Use triangle inequality to avoid unnecessary calculations
	    			    	    	if (minDistSquared > centroidDistances[groupNum[i]][j]) {
	    				    	    	distSquared = 0.0;
	    				    	    	for (k = 0; k < nDims; k++) {
	    				    	    		diff = pos[k][i] - centroidPos[k][j];
	    				    	    	    distSquared = distSquared + diff*diff;
	    				    	    	}
	    				    	    	if (distSquared < minDistSquared) {
	    				    	    		minDistSquared = distSquared;
	    				    	    		groupNum[i] = j;
	    				    	    	}
	    			    	    	} // if (minDistSquared > centroidDistances[groupNum[i]][j]))
	    			    	    } // for (j = 1; j < presentClusters; j++)
	    			    	    pointsInCluster[groupNum[i]]++;
	    			    	    if (originalGroupNum != groupNum[i]) {
	    			    	    	changeOccurred = true;
	    			    	    }
	    			    	    totalMinDistSquared += minDistSquared;
	    			    	} // for (i = 0; i < nPoints; i++)	
	    	    		} // if (equalScale)
	    	    		else { // not equalScale
	    	    			for (i = 0; i < presentClusters; i++) {
	    	    				for (j = i+1; j < presentClusters; j++) {
	    	    					for (k = 0; k < nDims; k++) {
	    	    						diff = centroidPos[k][i] - centroidPos[k][j];
	    	    						centroidDistances[i][j] += scale2[k]*diff*diff;
	    	    					} // for (k = 0; k < nDims; k++)
	    	    					centroidDistances[i][j] /= 4.0;
	    	    				} // for (j = i+1; j < presentClusters; j++)
	    	    			} // for (i = 0; i < presentClusters; i++)
	    			    	for (i = 0; i < nPoints; i++) {
	    			    		minDistSquared = 0.0;
	    			    	    originalGroupNum = groupNum[i];
	    		    	    	for (k = 0; k < nDims; k++) {
	    		    	    		diff = pos[k][i] - centroidPos[k][0];
	    		    	    	    minDistSquared = minDistSquared + scale2[k]*diff*diff;
	    		    	    	}
	    		    	    	groupNum[i] = 0;
	    			    	    for (j = 1; j < presentClusters; j++) {
	    			    	    	// Use triangle inequality to avoid unnecessary calculations
	    			    	    	if (minDistSquared > centroidDistances[groupNum[i]][j]) {
	    				    	    	distSquared = 0.0;
	    				    	    	for (k = 0; k < nDims; k++) {
	    				    	    		diff = pos[k][i] - centroidPos[k][j];
	    				    	    	    distSquared = distSquared + scale2[k]*diff*diff;
	    				    	    	}
	    				    	    	if (distSquared < minDistSquared) {
	    				    	    		minDistSquared = distSquared;
	    				    	    		groupNum[i] = j;
	    				    	    	}
	    			    	    	} // if (minDistSquared > centroidDistances[groupNum[i]][j]))
	    			    	    } // for (j = 1; j < presentClusters; j++)
	    			    	    pointsInCluster[groupNum[i]]++;
	    			    	    if (originalGroupNum != groupNum[i]) {
	    			    	    	changeOccurred = true;
	    			    	    }
	    			    	    totalMinDistSquared += minDistSquared;
	    			    	} // for (i = 0; i < nPoints; i++)
	    	    		} // else not equalScale
	    		    	for (i = 0; i < presentClusters; i++) {
	    		    		for (j = 0; j < nDims; j++) {
	    		    			centroidPos[j][i] = 0.0;
	    		    		}
	    		    	}
	    		    	if (useColorHistogram) {
	    		    	    for (i = 0; i < presentClusters; i++) {
	    		    	    	totalWeight[i] = 0.0;
	    		    	    }
	    		    		for (i = 0; i < nPoints; i++) {
	    		    			totalWeight[groupNum[i]] += weight[i];
	    			    		for (j = 0; j < nDims; j++) {
	    			    			centroidPos[j][groupNum[i]] += pos[j][i]*weight[i];
	    			    		}
	    			    	}
	    			    	clustersWithoutPoints = 0;
	    			    	for (i = 0; i < presentClusters; i++) {
	    			    		if (pointsInCluster[i] == 0) {
	    			    			Preferences.debug("Cluster centroid " + (i+1) + " has no points\n");
	    			    			clustersWithoutPoints++;
	    			    		}
	    			    		else {
	    				    		for (j = 0; j < nDims; j++) {
	    				    			centroidPos[j][i] = centroidPos[j][i]/totalWeight[i];
	    				    		}
	    			    		} // else
	    			    	}	
	    		    	} // if (useColorHistogram)
	    		    	else { // no color histogram
	    			    	for (i = 0; i < nPoints; i++) {
	    			    		for (j = 0; j < nDims; j++) {
	    			    			centroidPos[j][groupNum[i]] += pos[j][i];
	    			    		}
	    			    	}
	    			    	clustersWithoutPoints = 0;
	    			    	for (i = 0; i < presentClusters; i++) {
	    			    		if (pointsInCluster[i] == 0) {
	    			    			Preferences.debug("Cluster centroid " + (i+1) + " has no points\n");
	    			    			clustersWithoutPoints++;
	    			    		}
	    			    		else {
	    				    		for (j = 0; j < nDims; j++) {
	    				    			centroidPos[j][i] = centroidPos[j][i]/pointsInCluster[i];
	    				    		}
	    			    		} // else
	    			    	}
	    		    	} // else no colorHistogram
	    	    	} // while (changeOccurred)
	    	    	Preferences.debug("There are " + clustersWithoutPoints + " clusters without points\n");	
	    	    	if (totalMinDistSquared < bestTotalMinDistSquared) {
	    	    	    bestTotalMinDistSquared = totalMinDistSquared;
	    	    	    for (i = 0; i < nDims; i++) {
	    	    	    	for (j = 0; j < presentClusters; j++) {
	    	    	    		centroidPosStart[i][j] = centroidPos[i][j];
	    	    	    	}
	    	    	    }
	    	    	}
	    	    } // for (initialClusterLocation = 0; initialClusterLocation < nPoints; initialClusterLocation++)
	    	} // for (presentClusters = 2; presentClusters <= numberClusters; presentClusters++)
	    	for (i = 0; i < nDims; i++) {
    	    	for (j = 0; j < numberClusters; j++) {
    	    		centroidPos[i][j] = centroidPosStart[i][j];
    	    	}
    	    }
    	break;
    	case FAST_GLOBAL_K_MEANS:
    	break;
    	} // switch(algoSelection)
    	
    	if ((redBuffer != null) || (greenBuffer != null) || (blueBuffer != null)) {
    		buffer = new byte[length];
    		for(i = 0; i < length; i++) {
    		    buffer[i] = (byte)groupNum[indexTable[i]];	
    		}
    		try {
    			image.importData(0, buffer, true);
    		}
    		catch (IOException e) {
    			MipavUtil.displayError("IOException on image.importData(0, buffer, true)");
    			setCompleted(false);
    			return;
    		}
    	    new ViewJFrameImage(image);
    	    
    	    file = new File(resultsFileName);
        	try {
        	raFile = new RandomAccessFile( file, "rw" );
        	}
        	catch(FileNotFoundException e) {
        		MipavUtil.displayError("new RandomAccessFile gave FileNotFoundException " + e);
        		setCompleted(false);
        		return;
        	}
    	    
    	    // Necessary so that if this is an overwritten file there isn't any junk at the end
        	try {
        	    raFile.setLength( 0 );
        	}
        	catch (IOException e) {
        		MipavUtil.displayError("raFile.setLength(0) gave IOException " + e);
        		setCompleted(false);
        		return;	
        	}
        	for (i = 0; i < numberClusters; i++) {
        		dataString += "Cluster centroid " + (i+1) + ":\n";
        		for (j = 0; j < nDims; j++) {
        			dataString += "Dimension " + (j+1) + " = " + centroidPos[j][i] + "\n";
        		}
        		dataString += "\n";
        	}
        	
        	try {
        	    raFile.write(dataString.getBytes());
        	}
        	catch (IOException e) {
        		MipavUtil.displayError("raFile.write gave IOException " + e);
        		setCompleted(false);
        		return;		
        	}
        	try {
        	    raFile.close();
        	}
        	catch (IOException e) {
        		MipavUtil.displayError("raFile.close gave IOException " + e);
        		setCompleted(false);
        		return;		
        	}
        	
    	    setCompleted(true);
    	    return;
    	} // if ((redBuffer != null) || (greenBuffer != null) || (blueBuffer != null))
 
    	if ((image != null) && (nDims >= 2) && (nDims <= 3) && (numberClusters <= 9)) {
    		color = new Color[numberClusters];
    		color[0] = Color.red;
    		color[1] = Color.green;
    		if (numberClusters >= 3) {
    			color[2] = Color.blue;
    			if (numberClusters >= 4) {
    				color[3] = Color.cyan;
    				if (numberClusters >= 5) {
    					color[4] = Color.magenta;
    					if (numberClusters >= 6) {
    						color[5] = Color.orange;
    						if (numberClusters >= 7) {
    							color[6] = Color.pink;
    							if (numberClusters >= 8) {
    								color[7] = Color.yellow;
    								if (numberClusters >= 9) {
    									color[8] = Color.gray;
    								}
    							}
    						}
    					}
    				}
    			}
    		}
	    	for (i = 0; i < nPoints; i++) {
	    		newPtVOI = new VOI((short) (i), "", VOI.POINT, -1.0f);
	    		newPtVOI.setColor(color[groupNum[i]]);
	    		xArr[0] = (float)pos[0][i];
	    		yArr[0] = (float)pos[1][i];
	    		if (nDims == 2) {
	    			zArr[0] = 0.0f;
	    		}
	    		else {
	    			zArr[0] = (float)pos[2][i];
	    		}
	    		newPtVOI.importCurve(xArr, yArr, zArr);
	            ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setFixed(true);
	            image.registerVOI(newPtVOI);
	    	}
	    	for (i = 0; i < numberClusters; i++) {
	    		newPtVOI = new VOI((short) (i), "cluster" + (i+1) + ".voi", VOI.POINT, -1.0f);
	    		newPtVOI.setColor(Color.white);
	    		xArr[0] = (float)centroidPos[0][i];
	    		yArr[0] = (float)centroidPos[1][i];
	    		if (nDims == 2) {
	    			zArr[0] = 0.0f;
	    		}
	    		else {
	    			zArr[0] = (float)centroidPos[2][i];
	    		}
	    		newPtVOI.importCurve(xArr, yArr, zArr);
	            ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setFixed(true);
	            ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setLabel("C" + (i + 1));
	            image.registerVOI(newPtVOI);	
	    	}
	    	new ViewJFrameImage(image);
    	} // if ((image != null) && (nDims >= 2) && (nDims <= 3) && (numberClusters <= 9)
 
    	
    	file = new File(resultsFileName);
    	try {
    	raFile = new RandomAccessFile( file, "rw" );
    	}
    	catch(FileNotFoundException e) {
    		MipavUtil.displayError("new RandomAccessFile gave FileNotFoundException " + e);
    		setCompleted(false);
    		return;
    	}
    	// Necessary so that if this is an overwritten file there isn't any junk at the end
    	try {
    	    raFile.setLength( 0 );
    	}
    	catch (IOException e) {
    		MipavUtil.displayError("raFile.setLength(0) gave IOException " + e);
    		setCompleted(false);
    		return;	
    	}
    	for (i = 0; i < numberClusters; i++) {
    		dataString += "Cluster centroid " + (i+1) + ":\n";
    		for (j = 0; j < nDims; j++) {
    			dataString += "Dimension " + (j+1) + " = " + centroidPos[j][i] + "\n";
    		}
    		dataString += "\n";
    	}
    	
    	for (i = 0; i < nPoints; i++) {
    		dataString += "Point number " + String.valueOf(i+1) + "  Location  ";
    		for (j = 0; j < nDims; j++) {
    		    dataString += String.valueOf(pos[j][i]) + "  ";	
    		}
    		dataString += "Cluster  " + String.valueOf(groupNum[i]+1) + "\n";
    	}
    	try {
    	    raFile.write(dataString.getBytes());
    	}
    	catch (IOException e) {
    		MipavUtil.displayError("raFile.write gave IOException " + e);
    		setCompleted(false);
    		return;		
    	}
    	try {
    	    raFile.close();
    	}
    	catch (IOException e) {
    		MipavUtil.displayError("raFile.close gave IOException " + e);
    		setCompleted(false);
    		return;		
    	}
    	
        setCompleted(true);
        return;
    }
	
}
