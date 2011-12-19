package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.Color;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;


/**
 * With a small number of records, it is feasible to use RANDOM_INIT to perform multiple restarts efficiently.
 * With a small sample size, the subsampling for initialization is not effective in the Bradley-Fayyad
 * initialization.  The Bradley-Fayyad method is best suited for large-scale data.  By initializing a general
 * clustering algorithm near the nodes, not only are the true clusters found more often, but it follows that
 * the clustering algorithm will iterate fewer times prior to convergence.
 * 
 * The k-means algorithm will run differently with each run when random initialization and Bradley
 * Fayyad initialization are used since random number generation is used in selection.  The k-means
 * algorithm will run the same with each run when Hierarchical grouping initialization and maxmin
 * initialization are used.  Global k-means and fast global k-means will run the same with each run.
 * 
 * Initialization methods are only chosen with k-means; initialization methods are not chosen with 
 * global K-means and with fast global k-means.
 * 
 * In the maxmin initialization first select the 2 points that are farthest apart as seeds.  For each
 * additional seed find the minimum distance from each nonseed point to any of the seed points.  The new
 * seed point will be the point that yields the maximum of these minimum distances.
 * 
 * In hierarchical grouping all n points are initially considered as n groups of 1 member.  The number of groups is
 * decreased by 1 with each iteration from n to n-1 to n-2 until the desired number of groups has been obtained.
 * At each step the sum over all groups of the error sum of squares of each group is minimized.  The error sum of
 * squares of a group of n members is 
 * (sum from i = 1 to n of (xi - xaverage)**2 + (yi - yaverage)**2 + (zi - zaverage)**2)
 * where xaverage, yaverage, and zaverage are for all members of 1 group.
 * With n groups n*(n-1)/2 possible groupings to go from n to n-1 groups are tried to find the n-1 grouping with the lowest
 * sum over all groups of the error sum of squares.
 * 
 * In Bradley-Fayyad we perform k-means on 10 subsamples, each with 1/10 of all the points, chosen at random.  The initial
 * centroids are chosen at random and then 1/10 of the data points are selected at random.  When a
 * subsample k-means finishes, if a centroid does not have any members, then the initial centroid is reassigned 
 * to the point which is farthest from its assigned cluster centroid, and k-means is run again from these new initial
 * centroids.  The final centroids cmi from each of the 10 subsample runs are grouped into a common set cm.  10 k-means runs are
 * performed on this common cm centroid set, each starting with a cmi from a subsample run.  Let the final resulting centroids
 * from each of these 10 runs be fmi.  Then the fmi which has the lowest sum over all groups of the error sum of squares of
 * each group will be chosen as the initial starting centroids for the full sample run.
 * 
 * The global k-means problem solves a problem with m clusters by sequentially solving all intermediate
 * problems with 1,2, ..., m-1 clusters.  At the start of a run with m clusters the first m-1 clusters
 * are put at positions corresponding to their final locations in the m-1 cluster problem.  For global
 * k-means at every intermediate stage the one unassigned cluster is started at locations corresponding
 * to every data point in the image and the clustering error is calculated for a full k-means run.  
 * The run with the lowest clustering error is taken as the solution. For fast global k-means at each
 * intermediate step only 1 full k-means run is performed instead of having the number of k-means runs
 * equal the number of data points in the image.  The point which provides the largest guaranteed 
 * reduction in the error measure is used as the new cluster starting location. 
 * 
 * In fast global k-means we wish to find the point xn that maximizes bn, the guaranteed reduction in clustering
 * error obtained by inserting a new cluster center at position xn.  Let there be N points.  Then 
 * bn = sum from j = 1 to N of the max(d(j,k-1)**2 - ||xn -xj||**2,0)
 * where d(j,k-1) is the distance between xj and the closest center among the k-1 cluster centers obtained so far.
 * Fast global k-means is not used for the city-block measure.
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
 5.) "The global k-means clustering algorithm" by Aristidis Likas, Nikos Vlassis, and
     Jakob J. Verbeek, Pattern Recognition, Volume 36, 2003, pp. 451-461.
 */
public class AlgorithmKMeans extends AlgorithmBase {
	
	private static final int RANDOM_INIT = 0;
	
	private static final int BRADLEY_FAYYAD_INIT = 1;
	
	private static final int HIERARCHICAL_GROUPING_INIT = 2;
	
	private static final int MAXMIN_INIT = 3;
	
    private static final int K_MEANS = 0;
	
	private static final int GLOBAL_K_MEANS = 1;
	
	private static final int FAST_GLOBAL_K_MEANS = 2;
	
	private static final int EUCLIDEAN_SQUARED = 0;
	
	private static final int CITY_BLOCK = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------
	
	private ModelImage image;
	
	// K-means, global k-means, or fast global k-means
	private int algoSelection;
	
	// Take resolutions from any black and white image.
    // Use 1.0 in every dimension if not scaled or if colored image.
    // Subscript goes from 0 to nDims - 1 for black and white
	// and for 0 to 1 for color.
    private double scale[];
    
    // First subscript x = 0, y = 1, z = 2, t = 3
    // Second subscript 0 to nPoints-1 for each point
    // Value is the point position
	private double[][] pos;
	
	// subscript goes from 0 to nPoints-1 for each point
    // Value is the cluster number from 0 to numberClusters-1.
	private int[] groupNum;
	
    // subscript goes form 0 to nPoints-1 for each point
	// Value is the weight or number of occurrences of each point
	private double[] weight;
	
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
    
    // Euclidean squared using means or city-block using median
    private int distanceMeasure;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     
     */
    public AlgorithmKMeans(ModelImage image, int algoSelection, int distanceMeasure,
    		               double[][] pos, double[] scale, int groupNum[], double weight[],
    		               double[][] centroidPos, String resultsFileName, int initSelection, float[] redBuffer,
    		               float[] greenBuffer, float[] blueBuffer, double scaleMax, boolean useColorHistogram) {

        this.image = image;
        this.algoSelection = algoSelection;
        this.distanceMeasure = distanceMeasure;
        this.pos = pos;
        this.scale = scale;
        this.groupNum = groupNum;
        this.weight = weight;
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
        double subsampleWeight[];
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
        double totalDistSquared;
        double minTotalDistSquared;
        int bestFMIndex = 0;
        double centroidStartPos[][];
        int groupsPresent;
        int pointsInGroup[];
        double weightInGroup[];
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
        double newWeightInGroup;
        int bestNewPointsInGroup = 0;
        double bestNewWeightInGroup = 0.0;
        boolean found;
        int groupIndex;
        int currentClusters;
        int length = 0;
        int colors[] = null;
		int[] indexTable = null;
		int colorsFound;
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
        double distSquaredToNearestCluster[];
        double bn = 0.0;
        double bnMax;
        double minClusterDistance;
        double maxClusterDistance;
        int index = 0;
        double minDistCityBlock;
        double maxDistCityBlock;
        double distCityBlock;
        ArrayList<positionWeightItem> medianList[][];
        double preMedianPos;
        double preMedianWeight;
        double postMedianPos;
        double postMedianWeight;
        double weightFraction;
        double totalDistCityBlock;
        double minTotalDistCityBlock;
        double totalMinDistCityBlock = 0.0;
        double bestTotalMinDistCityBlock;
        
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
        totalWeight = new double[numberClusters];
    	
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
		    Preferences.debug("Colors found = " + colorsFound + "\n", Preferences.DEBUG_ALGORITHM);
		    pos = new double[2][colorsFound];
		    groupNum = new int[colorsFound];
		    
	    	weight = new double[colorsFound];
	    	totalWeight = new double[numberClusters];
	    	if (useColorHistogram) {
		    	for (i = 0; i < colorsFound; i++) {
		    		weight[i] = ((double)instances[i])/((double)length);
		    	}
	    	}
	    	else {
	    		for (i = 0; i < colorsFound; i++) {
	    		    weight[i] = 1.0;
	    		}
	    	}
	    	instances = null;
		    
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
    	switch(distanceMeasure) {
    	case EUCLIDEAN_SQUARED:
    
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
	    	Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
	    	for (i = 0; i < numberClusters; i++) {
	    		Preferences.debug("Starting centroid " + (i+1) + "\n", Preferences.DEBUG_ALGORITHM);
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
	    		    Preferences.debug("Dimension " + (j+1) + " at " + centroidPos[j][i] + "\n", Preferences.DEBUG_ALGORITHM);
	    		}
	    	} // for (i = 0; i < numberClusters; i++)
	    	startingPointIndex = null;
	    	break;
    	case BRADLEY_FAYYAD_INIT:
    		subsampleNumber = 10;
    		subsampleSize = nPoints/10;
    		subsampleIndex = new int[subsampleSize];
    		startingPointIndex = new int[numberClusters];
    		subsampleWeight = new double[subsampleSize];
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
	    	Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
	    	for (i = 0; i < numberClusters; i++) {
	    		Preferences.debug("Starting centroid " + (i+1) + "\n", Preferences.DEBUG_ALGORITHM);
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
	    		    Preferences.debug("Dimension " + (j+1) + " at " + centroidStartPos[j][i] + "\n", 
	    		    		Preferences.DEBUG_ALGORITHM);
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
                	subsampleWeight[j] = weight[possibleSample];
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
                	Preferences.debug("Iteration = " + iteration + " on part 1 subsample number " + (i+1), 
                			Preferences.DEBUG_ALGORITHM);
                	iteration++;
                	changeOccurred = false;
                	for (j = 0; j < numberClusters; j++) {
            			totalWeight[j] = 0.0;
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
	        	    	    totalWeight[groupNum[j]] += subsampleWeight[j];
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
	        	    	    totalWeight[groupNum[j]] += subsampleWeight[j];
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
        	    			centroidPos[k][groupNum[j]] += subsamplePos[k][j]*subsampleWeight[j];
        	    		}
        	    	}
        	    	clustersWithoutPoints = 0;
        	    	for (j = 0; j < numberClusters; j++) {
        	    		if (totalWeight[j] <= 1.0E-10) {
        	    			Preferences.debug("Cluster centroid " + (j+1) + " has no points\n", Preferences.DEBUG_ALGORITHM);
        	    			clustersWithoutPoints++;
        	    		}
        	    		else {
	        	    		Preferences.debug("Cluster centroid " + (j+1) + ":\n", Preferences.DEBUG_ALGORITHM);
	        	    		for (k = 0; k < nDims; k++) {
	        	    			centroidPos[k][j] = centroidPos[k][j]/totalWeight[j];
	        	    			Preferences.debug("Dimension " + (k+1) + " at " + centroidPos[k][j] + "\n", 
	        	    					Preferences.DEBUG_ALGORITHM);
	        	    		}
        	    		} // else 
        	    	}
                } // while (changeOccurred)
                Preferences.debug("There are " + clustersWithoutPoints +
                		          " clusters without points on subsample number " + (i+1) + "\n", Preferences.DEBUG_ALGORITHM);
                s = -1;
                for (j = 0; j < clustersWithoutPoints; j++) {
                	s++;
                	while (totalWeight[s] > 1.0E-10) {
                	    s++;	
                	}
                	maxDistSquared = 0.0;
                	if (equalScale) {
                		for (k = 0; k < numberClusters; k++) {
	                    	if (totalWeight[k] > 1.0E-10) {
	                    	    for (m = 0; m < subsampleSize; m++) {
	                    	    	if (groupNum[m] == k) {
	                    	    	    distSquared = 0.0;
	                    	    	    for (n = 0; n < nDims; n++) {
	                    	    	        diff = subsamplePos[n][m] - centroidPos[n][k];
	                    	    	        distSquared = distSquared + diff*diff;
	                    	    	    } // for (n = 0; n < nDims; n++)
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
	                    	if (totalWeight[k] > 1.0E-10) {
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
                    totalWeight[clusterWithMaxDistPoint] -= subsampleWeight[maxDistPoint];
                    for (k = 0; k < nDims; k++) {
                    	// No subsampleWeight because initializing with 1 point
                    	centroidPos[k][s] = subsamplePos[k][maxDistPoint];
                    }
                    totalWeight[s] = subsampleWeight[maxDistPoint];
                    for (k = 0; k < nDims; k++) {
                    	centroidPos[k][clusterWithMaxDistPoint] = 0.0;
                    }
                    for (k = 0; k < subsampleSize; k++) {
                    	if (groupNum[k] == clusterWithMaxDistPoint) {
	        	    		for (m = 0; m < nDims; m++) {
	        	    			centroidPos[m][clusterWithMaxDistPoint] += subsamplePos[m][k]*subsampleWeight[k];
	        	    		}
                    	}
        	    	}
                    for (k = 0; k < nDims; k++) {
                    	centroidPos[k][clusterWithMaxDistPoint] = 
                    		centroidPos[k][clusterWithMaxDistPoint]/totalWeight[clusterWithMaxDistPoint];
                    }
                } // for (j = 0; j < clustersWithoutPoints; j++)
                if (clustersWithoutPoints > 0) {
                    	Preferences.debug("Redoing k means on subsample number " + (i+1) + "\n", Preferences.DEBUG_ALGORITHM);
                    	changeOccurred = true;
                        iteration = 1;
                        while (changeOccurred){
                        	fireProgressStateChanged("Iteration = " + iteration + " on part 1 subsample number " + (i+1));
                        	Preferences.debug("Iteration = " + iteration + " on part 1 subsample number " + (i+1), 
                        			Preferences.DEBUG_ALGORITHM);
                        	iteration++;
                        	changeOccurred = false;
                        	for (j = 0; j < numberClusters; j++) {
                    			totalWeight[j] = 0.0;
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
	                	    	    totalWeight[groupNum[j]] += subsampleWeight[j];
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
	                	    	    totalWeight[groupNum[j]] += subsampleWeight[j];
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
                	    			centroidPos[k][groupNum[j]] += subsamplePos[k][j]*subsampleWeight[j];
                	    		}
                	    	}
                	    	clustersWithoutPoints = 0;
                	    	for (j = 0; j < numberClusters; j++) {
                	    		if (totalWeight[j] <= 1.0E-10) {
                	    			Preferences.debug("Cluster centroid " + (j+1) + " has no points\n", 
                	    					Preferences.DEBUG_ALGORITHM);
                	    			clustersWithoutPoints++;
                	    		}
                	    		else {
        	        	    		Preferences.debug("Cluster centroid " + (j+1) + ":\n", Preferences.DEBUG_ALGORITHM);
        	        	    		for (k = 0; k < nDims; k++) {
        	        	    			centroidPos[k][j] = centroidPos[k][j]/totalWeight[j];
        	        	    			Preferences.debug("Dimension " + (k+1) + " at " + centroidPos[k][j] + "\n", 
        	        	    					Preferences.DEBUG_ALGORITHM);
        	        	    		}
                	    		} // else 
                	    	}
                        } // while (changeOccurred)
                        Preferences.debug("There are " + clustersWithoutPoints +
                        		          " clusters without points on subsample number " + (i+1) + "\n", 
                        		          Preferences.DEBUG_ALGORITHM);
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
                	Preferences.debug("Iteration = " + iteration + " on part 2 subsample number " + (i+1), 
                			Preferences.DEBUG_ALGORITHM);
                	iteration++;
                	changeOccurred = false;
                	for (j = 0; j < numberClusters; j++) {
            			totalWeight[j] = 0.0;
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
	        	    	    totalWeight[groupNum[j]] += 1.0;
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
	        	    	    totalWeight[groupNum[j]] += 1.0;
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
        	    		if (totalWeight[j] <= 1.0E-10) {
        	    			Preferences.debug("Cluster centroid " + (j+1) + " has no points\n", Preferences.DEBUG_ALGORITHM);
        	    			clustersWithoutPoints++;
        	    		}
        	    		else {
	        	    		Preferences.debug("Cluster centroid " + (j+1) + ":\n", Preferences.DEBUG_ALGORITHM);
	        	    		for (k = 0; k < nDims; k++) {
	        	    			centroidPos[k][j] = centroidPos[k][j]/totalWeight[j];
	        	    			Preferences.debug("Dimension " + (k+1) + " at " + centroidPos[k][j] + "\n", 
	        	    					Preferences.DEBUG_ALGORITHM);
	        	    		}
        	    		} // else 
        	    	}
                } // while (changeOccurred)
                Preferences.debug("There are " + clustersWithoutPoints +
                		          " clusters without points on subsample number " + (i+1) + "\n", 
                		          Preferences.DEBUG_ALGORITHM);	
                for (j = 0; j < numberClusters; j++) {
                	for (k = 0; k < nDims; k++) {
                	    localFM[k][j][i] = centroidPos[k][j];
                	}
                }
            } // for (i = 0; i < subsampleNumber; i++)
                
            // The refined initial point is chosen as the localFM[][][i] having minimal distortion over unionCM
            minTotalDistSquared = Double.MAX_VALUE;
            if (equalScale) {
            	for (i = 0; i < subsampleNumber; i++) {
            		totalDistSquared = 0.0;
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
	                    totalDistSquared = totalDistSquared + minDistSquared;
	                } // for (j = 0; j < subsampleSize; j++)
	                if (totalDistSquared < minTotalDistSquared) {
	                	minTotalDistSquared = totalDistSquared;
	                	bestFMIndex = i;
	                }
	            } // for (i = 0; i < subsampleNumber; i++)	
            } // if (equalScale)
            else { // not equalScale
	            for (i = 0; i < subsampleNumber; i++) {
	            	totalDistSquared = 0.0;
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
	                    totalDistSquared = totalDistSquared + minDistSquared;
	                } // for (j = 0; j < subsampleSize; j++)
	                if (totalDistSquared < minTotalDistSquared) {
	                	minTotalDistSquared = totalDistSquared;
	                	bestFMIndex = i;
	                }
	            } // for (i = 0; i < subsampleNumber; i++)
            } // else not equalScale
            Preferences.debug("Refinement algorithm returns inital centroids at:\n", Preferences.DEBUG_ALGORITHM);
            
            for (i = 0; i < numberClusters; i++) {
            	Preferences.debug("Initial centroid " + (i+1) + "\n", Preferences.DEBUG_ALGORITHM);
            	for (j = 0; j < nDims; j++) {
            		centroidPos[j][i] = localFM[j][i][bestFMIndex];
            		Preferences.debug("Dimension " + (j+1) + " at " + centroidPos[j][i] + "\n", Preferences.DEBUG_ALGORITHM);
            	}
            }
               
    		break;
    	case HIERARCHICAL_GROUPING_INIT:
    		pointsInGroup = new int[nPoints];
    		weightInGroup = new double[nPoints];
    		for (i = 0; i < nPoints; i++) {
    			pointsInGroup[i] = 1;
    			weightInGroup[i] = weight[i];
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
		    					    newWeightInGroup = weightInGroup[i] + weightInGroup[j];
		    					    newess = 0.0;
		    					    for (m = 0; m < nDims; m++) {
		    					        sum = 0.0;
		    					        sumSq = 0.0;
		    					        for (n = 0; n < pointsInGroup[i]; n++) {
		    					        	sum += pos[m][hierGroup[i][n]]*weight[hierGroup[i][n]];
		    					        	sumSq += pos[m][hierGroup[i][n]]*pos[m][hierGroup[i][n]]*weight[hierGroup[i][n]];
		    					        }
		    					        for (n = 0; n < pointsInGroup[j]; n++) {
		    					        	sum += pos[m][hierGroup[j][n]]*weight[hierGroup[j][n]];
		    					        	sumSq += pos[m][hierGroup[j][n]]*pos[m][hierGroup[j][n]]*weight[hierGroup[j][n]];
		    					        }
		    					        newess += (sumSq - sum*sum/newWeightInGroup);
		    					    } // for (m = 0; m < nDims; m++)
		    					    essIncrease = newess - (essGroup[i] + essGroup[j]);
		    					    if (essIncrease < minessIncrease) {
		    					    	minessIncrease = essIncrease;
		    					    	bestFirstIndex = i;
		    					    	bestSecondIndex = j;
		    					    	bestnewess = newess;
		    					    	bestNewPointsInGroup = newPointsInGroup;
		    					    	bestNewWeightInGroup = newWeightInGroup;
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
		    		weightInGroup[bestFirstIndex] = bestNewWeightInGroup;
		    		weightInGroup[bestSecondIndex] = 0.0;
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
		    					    newWeightInGroup = weightInGroup[i] + weightInGroup[j];
		    					    newess = 0.0;
		    					    for (m = 0; m < nDims; m++) {
		    					        sum = 0.0;
		    					        sumSq = 0.0;
		    					        for (n = 0; n < pointsInGroup[i]; n++) {
		    					        	sum += pos[m][hierGroup[i][n]]*weight[hierGroup[i][n]];
		    					        	sumSq += pos[m][hierGroup[i][n]]*pos[m][hierGroup[i][n]]*weight[hierGroup[i][n]];
		    					        }
		    					        for (n = 0; n < pointsInGroup[j]; n++) {
		    					        	sum += pos[m][hierGroup[j][n]]*weight[hierGroup[j][n]];
		    					        	sumSq += pos[m][hierGroup[j][n]]*pos[m][hierGroup[j][n]]*weight[hierGroup[j][n]];
		    					        }
		    					        newess += scale2[m]*(sumSq - sum*sum/newWeightInGroup);
		    					    } // for (m = 0; m < nDims; m++)
		    					    essIncrease = newess - (essGroup[i] + essGroup[j]);
		    					    if (essIncrease < minessIncrease) {
		    					    	minessIncrease = essIncrease;
		    					    	bestFirstIndex = i;
		    					    	bestSecondIndex = j;
		    					    	bestnewess = newess;
		    					    	bestNewPointsInGroup = newPointsInGroup;
		    					    	bestNewWeightInGroup = newWeightInGroup;
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
		    		weightInGroup[bestFirstIndex] = bestNewWeightInGroup;
		    		weightInGroup[bestSecondIndex] = 0.0;
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
    		Preferences.debug("Hierarchical grouping returns inital centroids at:\n", Preferences.DEBUG_ALGORITHM);
    		for (i = 0; (i <= highestGroupPresent) && (groupIndex < numberClusters-1); i++) {
    			if (pointsInGroup[i] > 0) {
    			    groupIndex++;
    			    Preferences.debug("Initial centroid " + (groupIndex+1) + "\n", Preferences.DEBUG_ALGORITHM);
    			    totalWeight[groupIndex] = 0.0;
    			    for (j = 0; j < pointsInGroup[i]; j++) {
    			    	totalWeight[groupIndex] += weight[hierGroup[i][j]];
    			    	groupNum[hierGroup[i][j]] = groupIndex;
    			    	for (k = 0; k < nDims; k++) {
    			    	    centroidPos[k][groupIndex] += pos[k][hierGroup[i][j]]*weight[hierGroup[i][j]];	
    			    	}
    			    } // for (j = 0; j < pointsInGroup[i]; j++)
    			    for (j = 0; j < nDims; j++) {
    			    	centroidPos[j][groupIndex] = centroidPos[j][groupIndex]/totalWeight[groupIndex];
    			    	Preferences.debug("Dimension " + (j+1) + " at " + centroidPos[j][groupIndex] + "\n", Preferences.DEBUG_ALGORITHM);
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
    		if (equalScale) {
    			for (currentClusters = 2; currentClusters < numberClusters; currentClusters++) {
    				maxClusterDistance = 0.0;
	    			for (i = 0; i < nPoints; i++) {
	    			    if (groupNum[i] == -1) {
	    			    	minClusterDistance = Double.MAX_VALUE;
	    			    	for (j = 0; j < currentClusters; j++) {
	    			    	    distSquared = 0.0;
	    			    	    for (k = 0; k < nDims; k++) {
	    			    	    	diff = pos[k][i] - centroidPos[k][j];
	    			    	    	distSquared += diff*diff;
	    			    	    } // for (k = 0; k < nDims; k++)
	    			    	    if (distSquared < minClusterDistance) {
	    			    	    	minClusterDistance = distSquared;
	    			    	    }
	    			    	} // for (j = 0; j < currentClusters; j++)
	    			    	if (minClusterDistance > maxClusterDistance) {
	    			    		maxClusterDistance = minClusterDistance;
	    			    		index = i;
	    			    		for (j = 0; j < nDims; j++) {
	    			    			centroidPos[j][currentClusters] = pos[j][i];
	    			    		}
	    			    	}
	    			    } // if (groupNum[i] == -1)
	    			} // for (i = 0; i < nPoints; i++)
	    			groupNum[index] = currentClusters;
	    		} // for (currentClusters = 2; currentClusters < numberClusters; currentClusters++)	
    		} // if (equalScale)
    		else { // not equalScale
    			for (currentClusters = 2; currentClusters < numberClusters; currentClusters++) {
    				maxClusterDistance = 0.0;
	    			for (i = 0; i < nPoints; i++) {
	    			    if (groupNum[i] == -1) {
	    			    	minClusterDistance = Double.MAX_VALUE;
	    			    	for (j = 0; j < currentClusters; j++) {
	    			    	    distSquared = 0.0;
	    			    	    for (k = 0; k < nDims; k++) {
	    			    	    	diff = pos[k][i] - centroidPos[k][j];
	    			    	    	distSquared += scale2[k]*diff*diff;
	    			    	    } // for (k = 0; k < nDims; k++)
	    			    	    if (distSquared < minClusterDistance) {
	    			    	    	minClusterDistance = distSquared;
	    			    	    }
	    			    	} // for (j = 0; j < currentClusters; j++)
	    			    	if (minClusterDistance > maxClusterDistance) {
	    			    		maxClusterDistance = minClusterDistance;
	    			    		index = i;
	    			    		for (j = 0; j < nDims; j++) {
	    			    			centroidPos[j][currentClusters] = pos[j][i];
	    			    		}
	    			    	}
	    			    } // if (groupNum[i] == -1)
	    			} // for (i = 0; i < nPoints; i++)
	    			groupNum[index] = currentClusters;
	    		} // for (currentClusters = 2; currentClusters < numberClusters; currentClusters++)		
    		} // else not equalScale
            Preferences.debug("Maxmin initialization returns inital centroids at:\n", Preferences.DEBUG_ALGORITHM);
            
            for (i = 0; i < numberClusters; i++) {
            	Preferences.debug("Initial centroid " + (i+1) + "\n", Preferences.DEBUG_ALGORITHM);
            	for (j = 0; j < nDims; j++) {
            		Preferences.debug("Dimension " + (j+1) + " at " + centroidPos[j][i] + "\n", Preferences.DEBUG_ALGORITHM);
            	}
            }
    		break;
    	} // switch(initSelection)
    	
    	changeOccurred = true;
    	iteration = 1;
    	Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
    	while (changeOccurred) {
    		fireProgressStateChanged("Iteration = " + iteration);
    		Preferences.debug("Iteration = " + iteration + "\n", Preferences.DEBUG_ALGORITHM);
    		iteration++;
    		changeOccurred = false;
    		for (i = 0; i < numberClusters; i++) {
    			totalWeight[i] = 0.0;
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
		    	    totalWeight[groupNum[i]] += weight[i];
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
		    	    totalWeight[groupNum[i]] += weight[i];
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
	    		if (totalWeight[i] <= 1.0E-10) {
	    			Preferences.debug("Cluster centroid " + (i+1) + " has no points\n", Preferences.DEBUG_ALGORITHM);
	    			clustersWithoutPoints++;
	    		}
	    		else {
		    		Preferences.debug("Cluster centroid " + (i+1) + ":\n", Preferences.DEBUG_ALGORITHM);
		    		for (j = 0; j < nDims; j++) {
		    			centroidPos[j][i] = centroidPos[j][i]/totalWeight[i];
		    			Preferences.debug("Dimension " + (j+1) + " at " + centroidPos[j][i] + "\n", 
		    					Preferences.DEBUG_ALGORITHM);
		    		}
	    		} // else
	    	}	
    	} // while (changeOccurred)
    	Preferences.debug("There are " + clustersWithoutPoints + " clusters without points\n", Preferences.DEBUG_ALGORITHM);
    	break;
    	case GLOBAL_K_MEANS:
    		Preferences.debug("Finding centroid for 1 cluster\n", Preferences.DEBUG_ALGORITHM);
    		for (i = 0; i < nPoints; i++) {
	    		groupNum[i] = 0;
	    	}
    		centroidPosStart = new double[nDims][numberClusters];
    		totalWeight[0] = 0;
    		for (i = 0; i < nPoints; i++) {
    		    totalWeight[0] += weight[i];
    		}
    		for (j = 0; j < nDims; j++) {
    			centroidPosStart[j][0] = 0.0;
    		}
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
	    	for (presentClusters = 2; presentClusters <= numberClusters; presentClusters++) {
	    		Preferences.debug("Present cluster number = " + presentClusters + "\n", Preferences.DEBUG_ALGORITHM);
	    		fireProgressStateChanged("Present cluster number = " + presentClusters);
	    		bestTotalMinDistSquared = Double.MAX_VALUE;
	    	    for (initialClusterLocation = 0; initialClusterLocation < nPoints; initialClusterLocation++) {
	    	    	changeOccurred = true;
	    	    	iteration = 1;
	    	    	for (i = 0; i < nDims; i++) {
	    	    		for (j = 0; j < presentClusters-1; j++) {
	    	    			centroidPos[i][j] = centroidPosStart[i][j];
	    	    		}
	    	    		// No weight since just one point is being assigned
	    	    		centroidPos[i][presentClusters-1] = pos[i][initialClusterLocation];
	    	    	}
	    	    	while (changeOccurred) {
	    	    		iteration++;
	    	    		changeOccurred = false;
	    	    		totalMinDistSquared = 0.0;
	    	    		for (i = 0; i < presentClusters; i++) {
	    	    			totalWeight[i] = 0.0;
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
	    			    	    totalWeight[groupNum[i]] += weight[i];
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
	    			    	    totalWeight[groupNum[i]] += weight[i];
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
    			    		if (totalWeight[i] <= 1.0E-10) {
    			    			Preferences.debug("Cluster centroid " + (i+1) + " has no points\n", 
    			    					Preferences.DEBUG_ALGORITHM);
    			    			clustersWithoutPoints++;
    			    		}
    			    		else {
    				    		for (j = 0; j < nDims; j++) {
    				    			centroidPos[j][i] = centroidPos[j][i]/totalWeight[i];
    				    		}
    			    		} // else
    			    	}	
	    	    	} // while (changeOccurred)
	    	    	Preferences.debug("There are " + clustersWithoutPoints + " clusters without points\n", 
	    	    			Preferences.DEBUG_ALGORITHM);	
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
    		Preferences.debug("Finding centroid for 1 cluster\n", Preferences.DEBUG_ALGORITHM);
    		for (i = 0; i < nPoints; i++) {
	    		groupNum[i] = 0;
	    	}
    		totalWeight[0] = 0.0;
    		for (i = 0; i < nPoints; i++) {
    			totalWeight[0] += weight[i];
    		}
    		distSquaredToNearestCluster = new double[nPoints];
    		for (j = 0; j < nDims; j++) {
    			centroidPos[j][0] = 0.0;
    		}
    		totalWeight[0] = 0.0;
    		for (i = 0; i < nPoints; i++) {
    			totalWeight[0] += weight[i];
	    		for (j = 0; j < nDims; j++) {
	    			centroidPos[j][0] += pos[j][i]*weight[i];
	    		}
	    	}
    		for (j = 0; j < nDims; j++) {
    			centroidPos[j][0] = centroidPos[j][0]/totalWeight[0];
    		}
	    	if (equalScale) {
		    	for (i = 0; i < nPoints; i++) {
		    		for (j = 0; j < nDims; j++) {
		    			diff = pos[j][i] - centroidPos[j][0];
		    			distSquaredToNearestCluster[i] += diff * diff;
		    		}
		    	} // for (i = 0; i < nPoints; i++)
	    	} // if (equalScale)
	    	else {
	    		for (i = 0; i < nPoints; i++) {
		    		for (j = 0; j < nDims; j++) {
		    			diff = pos[j][i] - centroidPos[j][0];
		    			distSquaredToNearestCluster[i] += diff * diff * scale2[j];
		    		}
		    	} // for (i = 0; i < nPoints; i++)	
	    	}
	    	for (presentClusters = 2; presentClusters <= numberClusters; presentClusters++) {
	    		Preferences.debug("Present cluster number = " + presentClusters + "\n", Preferences.DEBUG_ALGORITHM);
	    		fireProgressStateChanged("Present cluster number = " + presentClusters);
	    		bnMax = -Double.MAX_VALUE;
	    	    for (initialClusterLocation = 0; initialClusterLocation < nPoints; initialClusterLocation++) {
	    	        bn = 0.0;
	    	        if (equalScale) {
		    	        for (i = 0; i < nPoints; i++) {
		    	        	distSquared = 0.0;
		    	        	for (j = 0; j < nDims; j++) {
		    	        		diff = pos[j][initialClusterLocation] - pos[j][i];
		    	        		distSquared += diff * diff;
		    	        	}
		    	        	bn += Math.max(distSquaredToNearestCluster[i] - distSquared, 0.0);
		    	        } // for (i = 0; i < nPoints; i++)
	    	        }
	    	        else { // not equal scale
	    	        	for (i = 0; i < nPoints; i++) {
		    	        	distSquared = 0.0;
		    	        	for (j = 0; j < nDims; j++) {
		    	        		diff = pos[j][initialClusterLocation] - pos[j][i];
		    	        		distSquared += diff * diff * scale2[j];
		    	        	}
		    	        	bn += Math.max(distSquaredToNearestCluster[i] - distSquared, 0.0);
		    	        } // for (i = 0; i < nPoints; i++)	
	    	        } // not equalScale
	    	        if (bn > bnMax) {
		    	    	bnMax = bn;
		    	    	for (i = 0; i < nDims; i++) {
		    	    		centroidPos[i][presentClusters-1] = pos[i][initialClusterLocation];
		    	    	}
		    	    }
	    	    } // for (initialClusterLocation = 0; initialClusterLocation < nPoints; initialClusterLocation++)
	    	    changeOccurred = true;
	        	iteration = 1;
	        	while (changeOccurred) {
	        		iteration++;
	        		changeOccurred = false;
	        		for (i = 0; i < presentClusters; i++) {
	        			totalWeight[i] = 0.0;
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
	    		    	    totalWeight[groupNum[i]] += weight[i];
	    		    	    if (originalGroupNum != groupNum[i]) {
	    		    	    	changeOccurred = true;
	    		    	    }
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
	    		    	    totalWeight[groupNum[i]] += weight[i];
	    		    	    if (originalGroupNum != groupNum[i]) {
	    		    	    	changeOccurred = true;
	    		    	    }
	    		    	} // for (i = 0; i < nPoints; i++)
	        		} // else not equalScale
	    	    	for (i = 0; i < presentClusters; i++) {
	    	    		for (j = 0; j < nDims; j++) {
	    	    			centroidPos[j][i] = 0.0;
	    	    		}
	    	    	}
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
    		    		if (totalWeight[i] <= 1.0E-10) {
    		    			Preferences.debug("Cluster centroid " + (i+1) + " has no points\n", 
    		    					Preferences.DEBUG_ALGORITHM);
    		    			clustersWithoutPoints++;
    		    		}
    		    		else {
    			    		for (j = 0; j < nDims; j++) {
    			    			centroidPos[j][i] = centroidPos[j][i]/totalWeight[i];
    			    		}
    		    		} // else
    		    	}	
	    	    	if (equalScale) {
		    	    	for (i = 0; i < nPoints; i++) {
		    	    		distSquaredToNearestCluster[i] = 0.0;
		    	    		for (j = 0; j < nDims; j++) {
		    	    			diff = pos[j][i] - centroidPos[j][groupNum[i]];
		    	    			distSquaredToNearestCluster[i] += diff * diff;
		    	    		}
		    	    	}
	    	    	} // if (equalScale)
	    	    	else {
	    	    		for (i = 0; i < nPoints; i++) {
		    	    		distSquaredToNearestCluster[i] = 0.0;
		    	    		for (j = 0; j < nDims; j++) {
		    	    			diff = pos[j][i] - centroidPos[j][groupNum[i]];
		    	    			distSquaredToNearestCluster[i] += diff * diff * scale2[j];
		    	    		}
		    	    	}	
	    	    	}
	        	} // while (changeOccurred)
	        	Preferences.debug("There are " + clustersWithoutPoints + " clusters without points\n", 
	        			Preferences.DEBUG_ALGORITHM);
	    	    
	    	} // for (presentClusters = 2; presentClusters <= numberClusters; presentClusters++)
	    	
    	break;
    	} // switch(algoSelection)
    	break;
    	
    	case CITY_BLOCK:
    		medianList = new ArrayList[nDims][numberClusters];
	    	for (j = 0; j < numberClusters; j++) {
	    		for (k = 0; k < nDims; k++) {
	    		    medianList[k][j] = new ArrayList<positionWeightItem>();
	    		}
	    	}
	  
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
    	    	Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
    	    	for (i = 0; i < numberClusters; i++) {
    	    		Preferences.debug("Starting centroid " + (i+1) + "\n", Preferences.DEBUG_ALGORITHM);
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
    	    		    Preferences.debug("Dimension " + (j+1) + " at " + centroidPos[j][i] + "\n", Preferences.DEBUG_ALGORITHM);
    	    		}
    	    	} // for (i = 0; i < numberClusters; i++)
    	    	startingPointIndex = null;
    	    	break;
        	case BRADLEY_FAYYAD_INIT:
        		subsampleNumber = 10;
        		subsampleSize = nPoints/10;
        		subsampleIndex = new int[subsampleSize];
        		startingPointIndex = new int[numberClusters];
        		subsampleWeight = new double[subsampleSize];
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
    	    	Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
    	    	for (i = 0; i < numberClusters; i++) {
    	    		Preferences.debug("Starting centroid " + (i+1) + "\n", Preferences.DEBUG_ALGORITHM);
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
    	    		    Preferences.debug("Dimension " + (j+1) + " at " + centroidStartPos[j][i] + "\n", 
    	    		    		Preferences.DEBUG_ALGORITHM);
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
                    	subsampleWeight[j] = weight[possibleSample];
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
                    	Preferences.debug("Iteration = " + iteration + " on part 1 subsample number " + (i+1), 
                    			Preferences.DEBUG_ALGORITHM);
                    	iteration++;
                    	changeOccurred = false;
                    	for (j = 0; j < numberClusters; j++) {
                			totalWeight[j] = 0.0;
                		}
                    	if (equalScale) {
                    		for (j = 0; j < subsampleSize; j++) {
    	        	    	    minDistCityBlock = 0.0;
    	        	    	    originalGroupNum = groupNum[j];
    	        	    	    for (m = 0; m < nDims; m++) {
            	    	    		diff = Math.abs(subsamplePos[m][j] - centroidPos[m][0]);
            	    	    	    minDistCityBlock = minDistCityBlock + diff;
            	    	    	}
    	        	    	    groupNum[j] = 0;
    	        	    	    for (k = 1; k < numberClusters; k++) {
		        	    	    	distCityBlock = 0.0;
		        	    	    	for (m = 0; m < nDims; m++) {
		        	    	    		diff = Math.abs(subsamplePos[m][j] - centroidPos[m][k]);
		        	    	    	    distCityBlock = distCityBlock + diff;
		        	    	    	}
		        	    	    	if (distCityBlock < minDistCityBlock) {
		        	    	    		minDistCityBlock = distCityBlock;
		        	    	    		groupNum[j] = k;
		        	    	    	}
    	        	    	    } // for (k = 1; k < numberClusters; k++)
    	        	    	    totalWeight[groupNum[j]] += subsampleWeight[j];
    	        	    	    if (originalGroupNum != groupNum[j]) {
    	        	    	    	changeOccurred = true;
    	        	    	    }
    	        	    	} // for (j = 0; j < subsampleSize; j++)	
                    	} // if (equalScale)
                    	else { // not equal scale
    	        	    	for (j = 0; j < subsampleSize; j++) {
    	        	    	    minDistCityBlock = 0.0;
    	        	    	    originalGroupNum = groupNum[j];
    	        	    	    for (m = 0; m < nDims; m++) {
            	    	    		diff = Math.abs(subsamplePos[m][j] - centroidPos[m][0]);
            	    	    	    minDistCityBlock = minDistCityBlock + scale[m]*diff;
            	    	    	}
    	        	    	    groupNum[j] = 0;
    	        	    	    for (k = 1; k < numberClusters; k++) {
		        	    	    	distCityBlock = 0.0;
		        	    	    	for (m = 0; m < nDims; m++) {
		        	    	    		diff = Math.abs(subsamplePos[m][j] - centroidPos[m][k]);
		        	    	    	    distCityBlock = distCityBlock + scale[m]*diff;
		        	    	    	}
		        	    	    	if (distCityBlock < minDistCityBlock) {
		        	    	    		minDistCityBlock = distCityBlock;
		        	    	    		groupNum[j] = k;
		        	    	    	}
    	        	    	    } // for (k = 1; k < numberClusters; k++)
    	        	    	    totalWeight[groupNum[j]] += subsampleWeight[j];
    	        	    	    if (originalGroupNum != groupNum[j]) {
    	        	    	    	changeOccurred = true;
    	        	    	    }
    	        	    	} // for (j = 0; j < subsampleSize; j++)
                    	} // else not equal scale
            	    	for (j = 0; j < numberClusters; j++) {
            	    		for (k = 0; k < nDims; k++) {
            	    		    medianList[k][j].clear();
            	    		}
            	    	}
            	    	for (j = 0; j < subsampleSize; j++) {
            	    		for (k = 0; k < nDims; k++) {
            	    			medianList[k][groupNum[j]].add(new positionWeightItem(subsamplePos[k][j],subsampleWeight[j]));
            	    		}
            	    	}
            	    	for (j = 0; j <numberClusters; j++) {
            	    		for (k = 0; k < nDims; k++) {
            	    			Collections.sort(medianList[k][j], new positionWeightComparator());
            	    		}
            	    	}
            	    			
            	    	clustersWithoutPoints = 0;
            	    	for (j = 0; j < numberClusters; j++) {
            	    		if (totalWeight[j] <= 1.0E-10) {
            	    			Preferences.debug("Cluster centroid " + (j+1) + " has no points\n", Preferences.DEBUG_ALGORITHM);
            	    			clustersWithoutPoints++;
            	    		}
            	    		else {
    	        	    		Preferences.debug("Cluster centroid " + (j+1) + ":\n", Preferences.DEBUG_ALGORITHM);
    	        	    		for (k = 0; k < nDims; k++) {
    	        	    			m = 0;
                	                preMedianWeight = 0.0;
                	                postMedianWeight = 0.0;
                	    			while (postMedianWeight <= totalWeight[j]/2.0) {
                	    				preMedianWeight = postMedianWeight;
                	    				postMedianWeight += medianList[k][j].get(m++).getWeight();
                	    			}
                	    			if (m < 2) {
                	    			    centroidPos[k][j] = medianList[k][j].get(0).getPosition();	
                	    			}
                	    			else {
                	    				preMedianPos = medianList[k][j].get(m-2).getPosition();
                	    				postMedianPos = medianList[k][j].get(m-1).getPosition();
                	    				weightFraction = (totalWeight[j]/2.0 - preMedianWeight)/(postMedianWeight - preMedianWeight);
                	    				centroidPos[k][j] = preMedianPos + (postMedianPos - preMedianPos)*weightFraction;
                	    			}
    	        	    			Preferences.debug("Dimension " + (k+1) + " at " + centroidPos[k][j] + "\n", 
    	        	    					Preferences.DEBUG_ALGORITHM);
    	        	    		}
            	    		} // else 
            	    	}
                    } // while (changeOccurred)
                    Preferences.debug("There are " + clustersWithoutPoints +
                    		          " clusters without points on subsample number " + (i+1) + "\n", Preferences.DEBUG_ALGORITHM);
                    s = -1;
                    for (j = 0; j < clustersWithoutPoints; j++) {
                    	s++;
                    	while (totalWeight[s] > 1.0E-10) {
                    	    s++;	
                    	}
                    	maxDistCityBlock = 0.0;
                    	if (equalScale) {
                    		for (k = 0; k < numberClusters; k++) {
    	                    	if (totalWeight[k] > 1.0E-10) {
    	                    	    for (m = 0; m < subsampleSize; m++) {
    	                    	    	if (groupNum[m] == k) {
    	                    	    	    distCityBlock = 0.0;
    	                    	    	    for (n = 0; n < nDims; n++) {
    	                    	    	        diff = Math.abs(subsamplePos[n][m] - centroidPos[n][k]);
    	                    	    	        distCityBlock = distCityBlock + diff;
    	                    	    	    } // for (n = 0; n < nDims; n++)
    	                    	    	    if (distCityBlock > maxDistCityBlock) {
    	                    	    	    	maxDistCityBlock = distCityBlock;
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
    	                    	if (totalWeight[k] > 1.0E-10) {
    	                    	    for (m = 0; m < subsampleSize; m++) {
    	                    	    	if (groupNum[m] == k) {
    	                    	    	    distCityBlock = 0.0;
    	                    	    	    for (n = 0; n < nDims; n++) {
    	                    	    	        diff = Math.abs(subsamplePos[n][m] - centroidPos[n][k]);
    	                    	    	        distCityBlock = distCityBlock + scale[n]*diff;
    	                    	    	    } // for (n = 0; n < nDims; n++)'
    	                    	    	    if (distCityBlock > maxDistCityBlock) {
    	                    	    	    	maxDistCityBlock = distCityBlock;
    	                    	    	    	maxDistPoint = m;
    	                    	    	    	clusterWithMaxDistPoint = k;
    	                    	    	    }
    	                    	    	} // if (groupNum[m] == k)
    	                    	    } // for (m = 0; m < subsampleSize; m++)
    	                    	} // if (pointsInCluster[k] > 1)
    	                    } // for (k = 0; k < numberClusters; k++)
                    	} // else not equalScale
                        groupNum[maxDistPoint] = s;
                        totalWeight[clusterWithMaxDistPoint] -= subsampleWeight[maxDistPoint];
                        for (k = 0; k < nDims; k++) {
                        	// No subsampleWeight because initializing with 1 point
                        	centroidPos[k][s] = subsamplePos[k][maxDistPoint];
                        }
                        totalWeight[s] = subsampleWeight[maxDistPoint];
        	    		for (k = 0; k < nDims; k++) {
        	    		    medianList[k][clusterWithMaxDistPoint].clear();
        	    		}
                        for (k = 0; k < subsampleSize; k++) {
                        	if (groupNum[k] == clusterWithMaxDistPoint) {
    	        	    		for (m = 0; m < nDims; m++) {
    	        	    			medianList[m][clusterWithMaxDistPoint].add(new positionWeightItem(subsamplePos[m][k],subsampleWeight[k]));
    	        	    		}
                        	}
            	    	}
                        for (k = 0; k < nDims; k++) {
        	    			Collections.sort(medianList[k][clusterWithMaxDistPoint], new positionWeightComparator());
        	    		}
                        for (k = 0; k < nDims; k++) {
                        	m = 0;
        	                preMedianWeight = 0.0;
        	                postMedianWeight = 0.0;
        	    			while (postMedianWeight <= totalWeight[clusterWithMaxDistPoint]/2.0) {
        	    				preMedianWeight = postMedianWeight;
        	    				postMedianWeight += medianList[k][clusterWithMaxDistPoint].get(m++).getWeight();
        	    			}
        	    			if (m < 2) {
        	    			    centroidPos[k][clusterWithMaxDistPoint] = medianList[k][clusterWithMaxDistPoint].get(0).getPosition();	
        	    			}
        	    			else {
        	    				preMedianPos = medianList[k][clusterWithMaxDistPoint].get(m-2).getPosition();
        	    				postMedianPos = medianList[k][clusterWithMaxDistPoint].get(m-1).getPosition();
        	    				weightFraction = 
        	    			    (totalWeight[clusterWithMaxDistPoint]/2.0 - preMedianWeight)/(postMedianWeight - preMedianWeight);
        	    				centroidPos[k][clusterWithMaxDistPoint] = preMedianPos + (postMedianPos - preMedianPos)*weightFraction;
        	    			}
                        }
                    } // for (j = 0; j < clustersWithoutPoints; j++)
                    if (clustersWithoutPoints > 0) {
                        	Preferences.debug("Redoing k means on subsample number " + (i+1) + "\n", Preferences.DEBUG_ALGORITHM);
                        	changeOccurred = true;
                            iteration = 1;
                            while (changeOccurred){
                            	fireProgressStateChanged("Iteration = " + iteration + " on part 1 subsample number " + (i+1));
                            	Preferences.debug("Iteration = " + iteration + " on part 1 subsample number " + (i+1), 
                            			Preferences.DEBUG_ALGORITHM);
                            	iteration++;
                            	changeOccurred = false;
                            	for (j = 0; j < numberClusters; j++) {
                        			totalWeight[j] = 0.0;
                        		}
                            	if (equalScale) {
                            		for (j = 0; j < subsampleSize; j++) {
    	                	    	    minDistCityBlock = 0.0;
    	                	    	    originalGroupNum = groupNum[j];
    	                	    	    for (m = 0; m < nDims; m++) {
                    	    	    		diff = Math.abs(subsamplePos[m][j] - centroidPos[m][0]);
                    	    	    	    minDistCityBlock = minDistCityBlock + diff;
                    	    	    	}
    	                	    	    groupNum[j] = 0;
    	                	    	    for (k = 1; k < numberClusters; k++) {
		                	    	    	distCityBlock = 0.0;
		                	    	    	for (m = 0; m < nDims; m++) {
		                	    	    		diff = Math.abs(subsamplePos[m][j] - centroidPos[m][k]);
		                	    	    	    distCityBlock = distCityBlock + diff;
		                	    	    	}
		                	    	    	if (distCityBlock < minDistCityBlock) {
		                	    	    		minDistCityBlock = distCityBlock;
		                	    	    		groupNum[j] = k;
		                	    	    	}
    	                	    	    } // for (k = 1; k < numberClusters; k++)
    	                	    	    totalWeight[groupNum[j]] += subsampleWeight[j];
    	                	    	    if (originalGroupNum != groupNum[j]) {
    	                	    	    	changeOccurred = true;
    	                	    	    }
    	                	    	} // for (j = 0; j < subsampleSize; j++)	
                            	} // if (equalScale)
                            	else { // not equalScale
    	                	    	for (j = 0; j < subsampleSize; j++) {
    	                	    	    minDistCityBlock = 0.0;
    	                	    	    originalGroupNum = groupNum[j];
    	                	    	    for (m = 0; m < nDims; m++) {
                    	    	    		diff = Math.abs(subsamplePos[m][j] - centroidPos[m][0]);
                    	    	    	    minDistCityBlock = minDistCityBlock + scale[m]*diff;
                    	    	    	}
    	                	    	    groupNum[j] = 0;
    	                	    	    for (k = 1; k < numberClusters; k++) {
		                	    	    	distCityBlock = 0.0;
		                	    	    	for (m = 0; m < nDims; m++) {
		                	    	    		diff = Math.abs(subsamplePos[m][j] - centroidPos[m][k]);
		                	    	    	    distCityBlock = distCityBlock + scale[m]*diff;
		                	    	    	}
		                	    	    	if (distCityBlock < minDistCityBlock) {
		                	    	    		minDistCityBlock = distCityBlock;
		                	    	    		groupNum[j] = k;
		                	    	    	}
    	                	    	    } // for (k = 1; k < numberClusters; k++)
    	                	    	    totalWeight[groupNum[j]] += subsampleWeight[j];
    	                	    	    if (originalGroupNum != groupNum[j]) {
    	                	    	    	changeOccurred = true;
    	                	    	    }
    	                	    	} // for (j = 0; j < subsampleSize; j++)
                            	} // else not equalScale
                    	    	for (j = 0; j < numberClusters; j++) {
                    	    		for (k = 0; k < nDims; k++) {
                    	    		    medianList[k][j].clear();
                    	    		}
                    	    	}
                    	    	for (j = 0; j < subsampleSize; j++) {
                    	    		for (k = 0; k < nDims; k++) {
                    	    			medianList[k][groupNum[j]].add(new positionWeightItem(subsamplePos[k][j],subsampleWeight[j]));
                    	    		}
                    	    	}
                    	    	for (j = 0; j <numberClusters; j++) {
                    	    		for (k = 0; k < nDims; k++) {
                    	    			Collections.sort(medianList[k][j], new positionWeightComparator());
                    	    		}
                    	    	}
                    	    	clustersWithoutPoints = 0;
                    	    	for (j = 0; j < numberClusters; j++) {
                    	    		if (totalWeight[j] <= 1.0E-10) {
                    	    			Preferences.debug("Cluster centroid " + (j+1) + " has no points\n", 
                    	    					Preferences.DEBUG_ALGORITHM);
                    	    			clustersWithoutPoints++;
                    	    		}
                    	    		else {
            	        	    		Preferences.debug("Cluster centroid " + (j+1) + ":\n", Preferences.DEBUG_ALGORITHM);
            	        	    		for (k = 0; k < nDims; k++) {
            	        	    			m = 0;
                        	                preMedianWeight = 0.0;
                        	                postMedianWeight = 0.0;
                        	    			while (postMedianWeight <= totalWeight[j]/2.0) {
                        	    				preMedianWeight = postMedianWeight;
                        	    				postMedianWeight += medianList[k][j].get(m++).getWeight();
                        	    			}
                        	    			if (m < 2) {
                        	    			    centroidPos[k][j] = medianList[k][j].get(0).getPosition();	
                        	    			}
                        	    			else {
                        	    				preMedianPos = medianList[k][j].get(m-2).getPosition();
                        	    				postMedianPos = medianList[k][j].get(m-1).getPosition();
                        	    				weightFraction = (totalWeight[j]/2.0 - preMedianWeight)/(postMedianWeight - preMedianWeight);
                        	    				centroidPos[k][j] = preMedianPos + (postMedianPos - preMedianPos)*weightFraction;
                        	    			}
            	        	    			Preferences.debug("Dimension " + (k+1) + " at " + centroidPos[k][j] + "\n", 
            	        	    					Preferences.DEBUG_ALGORITHM);
            	        	    		}
                    	    		} // else 
                    	    	}
                            } // while (changeOccurred)
                            Preferences.debug("There are " + clustersWithoutPoints +
                            		          " clusters without points on subsample number " + (i+1) + "\n", 
                            		          Preferences.DEBUG_ALGORITHM);
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
                    	Preferences.debug("Iteration = " + iteration + " on part 2 subsample number " + (i+1), 
                    			Preferences.DEBUG_ALGORITHM);
                    	iteration++;
                    	changeOccurred = false;
                    	for (j = 0; j < numberClusters; j++) {
                			totalWeight[j] = 0.0;
                		}
                    	if (equalScale) {
                    		for (j = 0; j < subsampleSize; j++) {
    	        	    	    minDistCityBlock = Double.MAX_VALUE;
    	        	    	    originalGroupNum = groupNum[j];
    	        	    	    for (k = 0; k < numberClusters; k++) {
    	        	    	    	distCityBlock = 0.0;
    	        	    	    	for (m = 0; m < nDims; m++) {
    	        	    	    		diff = Math.abs(unionCM[m][j] - centroidPos[m][k]);
    	        	    	    	    distCityBlock = distCityBlock + diff;
    	        	    	    	}
    	        	    	    	if (distCityBlock < minDistCityBlock) {
    	        	    	    		minDistCityBlock = distCityBlock;
    	        	    	    		groupNum[j] = k;
    	        	    	    	}
    	        	    	    } // for (k = 0; k < numberClusters; k++)
    	        	    	    totalWeight[groupNum[j]] += 1.0;
    	        	    	    if (originalGroupNum != groupNum[j]) {
    	        	    	    	changeOccurred = true;
    	        	    	    }
    	        	    	} // for (j = 0; j < subsampleSize; j++)	
                    	} // if (equalScale)
                    	else { // not equalScale
    	        	    	for (j = 0; j < subsampleSize; j++) {
    	        	    	    minDistCityBlock = Double.MAX_VALUE;
    	        	    	    originalGroupNum = groupNum[j];
    	        	    	    for (k = 0; k < numberClusters; k++) {
    	        	    	    	distCityBlock = 0.0;
    	        	    	    	for (m = 0; m < nDims; m++) {
    	        	    	    		diff = Math.abs(unionCM[m][j] - centroidPos[m][k]);
    	        	    	    	    distCityBlock = distCityBlock + scale[m]*diff;
    	        	    	    	}
    	        	    	    	if (distCityBlock < minDistCityBlock) {
    	        	    	    		minDistCityBlock = distCityBlock;
    	        	    	    		groupNum[j] = k;
    	        	    	    	}
    	        	    	    } // for (k = 0; k < numberClusters; k++)
    	        	    	    totalWeight[groupNum[j]] += 1.0;
    	        	    	    if (originalGroupNum != groupNum[j]) {
    	        	    	    	changeOccurred = true;
    	        	    	    }
    	        	    	} // for (j = 0; j < subsampleSize; j++)
                    	} // else not equalScale
            	    	for (j = 0; j < numberClusters; j++) {
            	    		for (k = 0; k < nDims; k++) {
            	    		    medianList[k][j].clear();
            	    		}
            	    	}
            	    	for (j = 0; j < subsampleSize; j++) {
            	    		for (k = 0; k < nDims; k++) {
            	    			medianList[k][groupNum[j]].add(new positionWeightItem(unionCM[k][j],1.0));
            	    		}
            	    	}
            	    	for (j = 0; j <numberClusters; j++) {
            	    		for (k = 0; k < nDims; k++) {
            	    			Collections.sort(medianList[k][j], new positionWeightComparator());
            	    		}
            	    	}
            	    	clustersWithoutPoints = 0;
            	    	for (j = 0; j < numberClusters; j++) {
            	    		if (totalWeight[j] <= 1.0E-10) {
            	    			Preferences.debug("Cluster centroid " + (j+1) + " has no points\n", Preferences.DEBUG_ALGORITHM);
            	    			clustersWithoutPoints++;
            	    		}
            	    		else {
    	        	    		Preferences.debug("Cluster centroid " + (j+1) + ":\n", Preferences.DEBUG_ALGORITHM);
    	        	    		for (k = 0; k < nDims; k++) {
    	        	    			m = 0;
                	                preMedianWeight = 0.0;
                	                postMedianWeight = 0.0;
                	    			while (postMedianWeight <= totalWeight[j]/2.0) {
                	    				preMedianWeight = postMedianWeight;
                	    				postMedianWeight += medianList[k][j].get(m++).getWeight();
                	    			}
                	    			if (m < 2) {
                	    			    centroidPos[k][j] = medianList[k][j].get(0).getPosition();	
                	    			}
                	    			else {
                	    				preMedianPos = medianList[k][j].get(m-2).getPosition();
                	    				postMedianPos = medianList[k][j].get(m-1).getPosition();
                	    				weightFraction = (totalWeight[j]/2.0 - preMedianWeight)/(postMedianWeight - preMedianWeight);
                	    				centroidPos[k][j] = preMedianPos + (postMedianPos - preMedianPos)*weightFraction;
                	    			}
    	        	    			Preferences.debug("Dimension " + (k+1) + " at " + centroidPos[k][j] + "\n", 
    	        	    					Preferences.DEBUG_ALGORITHM);
    	        	    		}
            	    		} // else 
            	    	}
                    } // while (changeOccurred)
                    Preferences.debug("There are " + clustersWithoutPoints +
                    		          " clusters without points on subsample number " + (i+1) + "\n", 
                    		          Preferences.DEBUG_ALGORITHM);	
                    for (j = 0; j < numberClusters; j++) {
                    	for (k = 0; k < nDims; k++) {
                    	    localFM[k][j][i] = centroidPos[k][j];
                    	}
                    }
                } // for (i = 0; i < subsampleNumber; i++)
                    
                // The refined initial point is chosen as the localFM[][][i] having minimal distortion over unionCM
                minTotalDistCityBlock = Double.MAX_VALUE;
                if (equalScale) {
                	for (i = 0; i < subsampleNumber; i++) {
                		totalDistCityBlock = 0.0;
    	                for (j = 0; j < subsampleSize; j++) {
    	                    minDistCityBlock = Double.MAX_VALUE;
    	                    for (k = 0; k < numberClusters; k++) {
    	                        distCityBlock = 0.0;
    	                        for (m = 0; m < nDims; m++) {
    	                        	diff = Math.abs(unionCM[m][j] - localFM[m][k][i]);
    	                        	distCityBlock = distCityBlock + diff;
    	                        }
    	                        if (distCityBlock < minDistCityBlock) {
    	                        	minDistCityBlock = distCityBlock;
    	                        }
    	                    } // for (k = 0; k < numberClusters; k++)
    	                    totalDistCityBlock = totalDistCityBlock + minDistCityBlock;
    	                } // for (j = 0; j < subsampleSize; j++)
    	                if (totalDistCityBlock < minTotalDistCityBlock) {
    	                	minTotalDistCityBlock = totalDistCityBlock;
    	                	bestFMIndex = i;
    	                }
    	            } // for (i = 0; i < subsampleNumber; i++)	
                } // if (equalScale)
                else { // not equalScale
    	            for (i = 0; i < subsampleNumber; i++) {
    	            	totalDistCityBlock = 0.0;
    	                for (j = 0; j < subsampleSize; j++) {
    	                    minDistCityBlock = Double.MAX_VALUE;
    	                    for (k = 0; k < numberClusters; k++) {
    	                        distCityBlock = 0.0;
    	                        for (m = 0; m < nDims; m++) {
    	                        	diff = Math.abs(unionCM[m][j] - localFM[m][k][i]);
    	                        	distCityBlock = distCityBlock + scale[m]*diff;
    	                        }
    	                        if (distCityBlock < minDistCityBlock) {
    	                        	minDistCityBlock = distCityBlock;
    	                        }
    	                    } // for (k = 0; k < numberClusters; k++)
    	                    totalDistCityBlock = totalDistCityBlock + minDistCityBlock;
    	                } // for (j = 0; j < subsampleSize; j++)
    	                if (totalDistCityBlock < minTotalDistCityBlock) {
    	                	minTotalDistCityBlock = totalDistCityBlock;
    	                	bestFMIndex = i;
    	                }
    	            } // for (i = 0; i < subsampleNumber; i++)
                } // else not equalScale
                Preferences.debug("Refinement algorithm returns inital centroids at:\n", Preferences.DEBUG_ALGORITHM);
                
                for (i = 0; i < numberClusters; i++) {
                	Preferences.debug("Initial centroid " + (i+1) + "\n", Preferences.DEBUG_ALGORITHM);
                	for (j = 0; j < nDims; j++) {
                		centroidPos[j][i] = localFM[j][i][bestFMIndex];
                		Preferences.debug("Dimension " + (j+1) + " at " + centroidPos[j][i] + "\n", Preferences.DEBUG_ALGORITHM);
                	}
                }
                   
        		break;
        	case HIERARCHICAL_GROUPING_INIT:
        		pointsInGroup = new int[nPoints];
        		weightInGroup = new double[nPoints];
        		for (i = 0; i < nPoints; i++) {
        			pointsInGroup[i] = 1;
        			weightInGroup[i] = weight[i];
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
    		    					    newWeightInGroup = weightInGroup[i] + weightInGroup[j];
    		    					    newess = 0.0;
    		    					    for (m = 0; m < nDims; m++) {
    		    					        sum = 0.0;
    		    					        sumSq = 0.0;
    		    					        for (n = 0; n < pointsInGroup[i]; n++) {
    		    					        	sum += pos[m][hierGroup[i][n]]*weight[hierGroup[i][n]];
    		    					        	sumSq += pos[m][hierGroup[i][n]]*pos[m][hierGroup[i][n]]*weight[hierGroup[i][n]];
    		    					        }
    		    					        for (n = 0; n < pointsInGroup[j]; n++) {
    		    					        	sum += pos[m][hierGroup[j][n]]*weight[hierGroup[j][n]];
    		    					        	sumSq += pos[m][hierGroup[j][n]]*pos[m][hierGroup[j][n]]*weight[hierGroup[j][n]];
    		    					        }
    		    					        newess += (sumSq - sum*sum/newWeightInGroup);
    		    					    } // for (m = 0; m < nDims; m++)
    		    					    essIncrease = newess - (essGroup[i] + essGroup[j]);
    		    					    if (essIncrease < minessIncrease) {
    		    					    	minessIncrease = essIncrease;
    		    					    	bestFirstIndex = i;
    		    					    	bestSecondIndex = j;
    		    					    	bestnewess = newess;
    		    					    	bestNewPointsInGroup = newPointsInGroup;
    		    					    	bestNewWeightInGroup = newWeightInGroup;
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
    		    		weightInGroup[bestFirstIndex] = bestNewWeightInGroup;
    		    		weightInGroup[bestSecondIndex] = 0.0;
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
    		    					    newWeightInGroup = weightInGroup[i] + weightInGroup[j];
    		    					    newess = 0.0;
    		    					    for (m = 0; m < nDims; m++) {
    		    					        sum = 0.0;
    		    					        sumSq = 0.0;
    		    					        for (n = 0; n < pointsInGroup[i]; n++) {
    		    					        	sum += pos[m][hierGroup[i][n]]*weight[hierGroup[i][n]];
    		    					        	sumSq += pos[m][hierGroup[i][n]]*pos[m][hierGroup[i][n]]*weight[hierGroup[i][n]];
    		    					        }
    		    					        for (n = 0; n < pointsInGroup[j]; n++) {
    		    					        	sum += pos[m][hierGroup[j][n]]*weight[hierGroup[j][n]];
    		    					        	sumSq += pos[m][hierGroup[j][n]]*pos[m][hierGroup[j][n]]*weight[hierGroup[j][n]];
    		    					        }
    		    					        newess += scale2[m]*(sumSq - sum*sum/newWeightInGroup);
    		    					    } // for (m = 0; m < nDims; m++)
    		    					    essIncrease = newess - (essGroup[i] + essGroup[j]);
    		    					    if (essIncrease < minessIncrease) {
    		    					    	minessIncrease = essIncrease;
    		    					    	bestFirstIndex = i;
    		    					    	bestSecondIndex = j;
    		    					    	bestnewess = newess;
    		    					    	bestNewPointsInGroup = newPointsInGroup;
    		    					    	bestNewWeightInGroup = newWeightInGroup;
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
    		    		weightInGroup[bestFirstIndex] = bestNewWeightInGroup;
    		    		weightInGroup[bestSecondIndex] = 0.0;
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
        		Preferences.debug("Hierarchical grouping returns inital centroids at:\n", Preferences.DEBUG_ALGORITHM);
        		for (i = 0; (i <= highestGroupPresent) && (groupIndex < numberClusters-1); i++) {
        			if (pointsInGroup[i] > 0) {
        			    groupIndex++;
        			    Preferences.debug("Initial centroid " + (groupIndex+1) + "\n", Preferences.DEBUG_ALGORITHM);
        			    totalWeight[groupIndex] = 0.0;
        			    for (k = 0; k < nDims; k++) {
        	    		    medianList[k][groupIndex].clear();
        	    		}
        			    for (j = 0; j < pointsInGroup[i]; j++) {
        			    	totalWeight[groupIndex] += weight[hierGroup[i][j]];
        			    	groupNum[hierGroup[i][j]] = groupIndex;
	        	    		for (k = 0; k < nDims; k++) {
	        	    			medianList[k][groupIndex].add(new positionWeightItem(pos[k][hierGroup[i][j]],weight[hierGroup[i][j]]));
	        	    		}
	        	    		for (k = 0; k < nDims; k++) {
	        	    			Collections.sort(medianList[k][groupIndex], new positionWeightComparator());
	        	    		}
        			    } // for (j = 0; j < pointsInGroup[i]; j++)
        			    for (j = 0; j < nDims; j++) {
        			    	m = 0;
        	                preMedianWeight = 0.0;
        	                postMedianWeight = 0.0;
        	    			while (postMedianWeight <= totalWeight[groupIndex]/2.0) {
        	    				preMedianWeight = postMedianWeight;
        	    				postMedianWeight += medianList[j][groupIndex].get(m++).getWeight();
        	    			}
        	    			if (m < 2) {
        	    			    centroidPos[j][groupIndex] = medianList[j][groupIndex].get(0).getPosition();	
        	    			}
        	    			else {
        	    				preMedianPos = medianList[j][groupIndex].get(m-2).getPosition();
        	    				postMedianPos = medianList[j][groupIndex].get(m-1).getPosition();
        	    				weightFraction = (totalWeight[groupIndex]/2.0 - preMedianWeight)/(postMedianWeight - preMedianWeight);
        	    				centroidPos[j][groupIndex] = preMedianPos + (postMedianPos - preMedianPos)*weightFraction;
        	    			}
        			    	Preferences.debug("Dimension " + (j+1) + " at " + centroidPos[j][groupIndex] + "\n", Preferences.DEBUG_ALGORITHM);
        			    }
        			} // if (pointsInGroun[i] > 0)
        		}
        		break;
        	case MAXMIN_INIT:
        		for (i = 0; i < nPoints; i++) {
        			groupNum[i] = -1;
        		}
        		// Obtain the 2 point furtherest apart as seeds
        		maxDistCityBlock = 0.0;
        		if (equalScale) {
        			for (i = 0; i < nPoints-1; i++) {
    	    			for (j = i+1; j <= nPoints-1; j++) {
    	    			    distCityBlock = 0.0;
    	    			    for (k = 0; k < nDims; k++) {
    	    			    	diff = Math.abs(pos[k][i] - pos[k][j]);
    	    			    	distCityBlock += diff;
    	    			    } // for (k = 0; k < nDims; k++)
    	    			    if (distCityBlock > maxDistCityBlock) {
    	    			    	maxDistCityBlock = distCityBlock;
    	    			    	bestFirstIndex = i;
    	    			    	bestSecondIndex = j;
    	    			    }
    	    			} // for (j = i+1; j <= nPoints-1; j++)
    	    		} // for (i = 0; i < nPoints-1; i++)	
        		} // if (equalScale)
        		else { // not equalScale
    	    		for (i = 0; i < nPoints-1; i++) {
    	    			for (j = i+1; j <= nPoints-1; j++) {
    	    			    distCityBlock = 0.0;
    	    			    for (k = 0; k < nDims; k++) {
    	    			    	diff = Math.abs(pos[k][i] - pos[k][j]);
    	    			    	distCityBlock += scale[k]*diff;
    	    			    } // for (k = 0; k < nDims; k++)
    	    			    if (distCityBlock > maxDistCityBlock) {
    	    			    	maxDistCityBlock = distCityBlock;
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
        		if (equalScale) {
        			for (currentClusters = 2; currentClusters < numberClusters; currentClusters++) {
        				maxClusterDistance = 0.0;
    	    			for (i = 0; i < nPoints; i++) {
    	    			    if (groupNum[i] == -1) {
    	    			    	minClusterDistance = Double.MAX_VALUE;
    	    			    	for (j = 0; j < currentClusters; j++) {
    	    			    	    distCityBlock = 0.0;
    	    			    	    for (k = 0; k < nDims; k++) {
    	    			    	    	diff = Math.abs(pos[k][i] - centroidPos[k][j]);
    	    			    	    	distCityBlock += diff;
    	    			    	    } // for (k = 0; k < nDims; k++)
    	    			    	    if (distCityBlock < minClusterDistance) {
    	    			    	    	minClusterDistance = distCityBlock;
    	    			    	    }
    	    			    	} // for (j = 0; j < currentClusters; j++)
    	    			    	if (minClusterDistance > maxClusterDistance) {
    	    			    		maxClusterDistance = minClusterDistance;
    	    			    		index = i;
    	    			    		for (j = 0; j < nDims; j++) {
    	    			    			centroidPos[j][currentClusters] = pos[j][i];
    	    			    		}
    	    			    	}
    	    			    } // if (groupNum[i] == -1)
    	    			} // for (i = 0; i < nPoints; i++)
    	    			groupNum[index] = currentClusters;
    	    		} // for (currentClusters = 2; currentClusters < numberClusters; currentClusters++)	
        		} // if (equalScale)
        		else { // not equalScale
        			for (currentClusters = 2; currentClusters < numberClusters; currentClusters++) {
        				maxClusterDistance = 0.0;
    	    			for (i = 0; i < nPoints; i++) {
    	    			    if (groupNum[i] == -1) {
    	    			    	minClusterDistance = Double.MAX_VALUE;
    	    			    	for (j = 0; j < currentClusters; j++) {
    	    			    	    distCityBlock = 0.0;
    	    			    	    for (k = 0; k < nDims; k++) {
    	    			    	    	diff = Math.abs(pos[k][i] - centroidPos[k][j]);
    	    			    	    	distCityBlock += scale[k]*diff;
    	    			    	    } // for (k = 0; k < nDims; k++)
    	    			    	    if (distCityBlock < minClusterDistance) {
    	    			    	    	minClusterDistance = distCityBlock;
    	    			    	    }
    	    			    	} // for (j = 0; j < currentClusters; j++)
    	    			    	if (minClusterDistance > maxClusterDistance) {
    	    			    		maxClusterDistance = minClusterDistance;
    	    			    		index = i;
    	    			    		for (j = 0; j < nDims; j++) {
    	    			    			centroidPos[j][currentClusters] = pos[j][i];
    	    			    		}
    	    			    	}
    	    			    } // if (groupNum[i] == -1)
    	    			} // for (i = 0; i < nPoints; i++)
    	    			groupNum[index] = currentClusters;
    	    		} // for (currentClusters = 2; currentClusters < numberClusters; currentClusters++)		
        		} // else not equalScale
                Preferences.debug("Maxmin initialization returns inital centroids at:\n", Preferences.DEBUG_ALGORITHM);
                
                for (i = 0; i < numberClusters; i++) {
                	Preferences.debug("Initial centroid " + (i+1) + "\n", Preferences.DEBUG_ALGORITHM);
                	for (j = 0; j < nDims; j++) {
                		Preferences.debug("Dimension " + (j+1) + " at " + centroidPos[j][i] + "\n", Preferences.DEBUG_ALGORITHM);
                	}
                }
        		break;
        	} // switch(initSelection)
        	
        	changeOccurred = true;
        	iteration = 1;
        	Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        	while (changeOccurred) {
        		fireProgressStateChanged("Iteration = " + iteration);
        		Preferences.debug("Iteration = " + iteration + "\n", Preferences.DEBUG_ALGORITHM);
        		iteration++;
        		changeOccurred = false;
        		for (i = 0; i < numberClusters; i++) {
        			totalWeight[i] = 0.0;
        		}
        		if (equalScale) {
        			for (i = 0; i < nPoints; i++) {
    		    	    minDistCityBlock = 0.0;
    		    	    originalGroupNum = groupNum[i];
    	    	    	for (k = 0; k < nDims; k++) {
    	    	    		diff = Math.abs(pos[k][i] - centroidPos[k][0]);
    	    	    	    minDistCityBlock = minDistCityBlock + diff;
    	    	    	}
    	    	    	groupNum[i] = 0;
    		    	    for (j = 1; j < numberClusters; j++) {
			    	    	distCityBlock = 0.0;
			    	    	for (k = 0; k < nDims; k++) {
			    	    		diff = Math.abs(pos[k][i] - centroidPos[k][j]);
			    	    	    distCityBlock = distCityBlock + diff;
			    	    	}
			    	    	if (distCityBlock < minDistCityBlock) {
			    	    		minDistCityBlock = distCityBlock;
			    	    		groupNum[i] = j;
			    	    	}
    		    	    } // for (j = 1; j < numberClusters; j++)
    		    	    totalWeight[groupNum[i]] += weight[i];
    		    	    if (originalGroupNum != groupNum[i]) {
    		    	    	changeOccurred = true;
    		    	    }
    		    	} // for (i = 0; i < nPoints; i++)	
        		} // if (equalScale)
        		else { // not equalScale
    		    	for (i = 0; i < nPoints; i++) {
    		    		minDistCityBlock = 0.0;
    		    	    originalGroupNum = groupNum[i];
    	    	    	for (k = 0; k < nDims; k++) {
    	    	    		diff = Math.abs(pos[k][i] - centroidPos[k][0]);
    	    	    	    minDistCityBlock = minDistCityBlock + scale[k]*diff;
    	    	    	}
    	    	    	groupNum[i] = 0;
    		    	    for (j = 1; j < numberClusters; j++) {
			    	    	distCityBlock = 0.0;
			    	    	for (k = 0; k < nDims; k++) {
			    	    		diff = Math.abs(pos[k][i] - centroidPos[k][j]);
			    	    	    distCityBlock = distCityBlock + scale[k]*diff;
			    	    	}
			    	    	if (distCityBlock < minDistCityBlock) {
			    	    		minDistCityBlock = distCityBlock;
			    	    		groupNum[i] = j;
			    	    	}
    		    	    } // for (j = 1; j < numberClusters; j++)
    		    	    totalWeight[groupNum[i]] += weight[i];
    		    	    if (originalGroupNum != groupNum[i]) {
    		    	    	changeOccurred = true;
    		    	    }
    		    	} // for (i = 0; i < nPoints; i++)
        		} // else not equalScale
        	    for (i = 0; i < numberClusters; i++) {
        	    	totalWeight[i] = 0.0;
        	    }
        	    for (i = 0; i < numberClusters; i++) {
    	    		for (j = 0; j < nDims; j++) {
    	    		    medianList[j][i].clear();
    	    		}
    	    	}
    	    	for (i = 0; i < nPoints; i++) {
    	    		for (j = 0; j < nDims; j++) {
    	    			medianList[j][groupNum[i]].add(new positionWeightItem(pos[j][i],weight[i]));
    	    		}
    	    	}
    	    	for (i = 0; i <numberClusters; i++) {
    	    		for (j = 0; j < nDims; j++) {
    	    			Collections.sort(medianList[j][i], new positionWeightComparator());
    	    		}
    	    	}
    	    	clustersWithoutPoints = 0;
    	    	for (i = 0; i < numberClusters; i++) {
    	    		if (totalWeight[i] <= 1.0E-10) {
    	    			Preferences.debug("Cluster centroid " + (i+1) + " has no points\n", Preferences.DEBUG_ALGORITHM);
    	    			clustersWithoutPoints++;
    	    		}
    	    		else {
    		    		Preferences.debug("Cluster centroid " + (i+1) + ":\n", Preferences.DEBUG_ALGORITHM);
    		    		for (j = 0; j < nDims; j++) {
    		    			m = 0;
        	                preMedianWeight = 0.0;
        	                postMedianWeight = 0.0;
        	    			while (postMedianWeight <= totalWeight[i]/2.0) {
        	    				preMedianWeight = postMedianWeight;
        	    				postMedianWeight += medianList[j][i].get(m++).getWeight();
        	    			}
        	    			if (m < 2) {
        	    			    centroidPos[j][i] = medianList[j][i].get(0).getPosition();	
        	    			}
        	    			else {
        	    				preMedianPos = medianList[j][i].get(m-2).getPosition();
        	    				postMedianPos = medianList[j][i].get(m-1).getPosition();
        	    				weightFraction = (totalWeight[i]/2.0 - preMedianWeight)/(postMedianWeight - preMedianWeight);
        	    				centroidPos[j][i] = preMedianPos + (postMedianPos - preMedianPos)*weightFraction;
        	    			}
    		    			Preferences.debug("Dimension " + (j+1) + " at " + centroidPos[j][i] + "\n", 
    		    					Preferences.DEBUG_ALGORITHM);
    		    		}
    	    		} // else
    	    	}	
        	} // while (changeOccurred)
        	Preferences.debug("There are " + clustersWithoutPoints + " clusters without points\n", Preferences.DEBUG_ALGORITHM);
        	break;
        	case GLOBAL_K_MEANS:
        		Preferences.debug("Finding centroid for 1 cluster\n", Preferences.DEBUG_ALGORITHM);
        		for (i = 0; i < nPoints; i++) {
    	    		groupNum[i] = 0;
    	    	}
        		centroidPosStart = new double[nDims][numberClusters];
        		totalWeight[0] = 0;
        		for (i = 0; i < nPoints; i++) {
        		    totalWeight[0] += weight[i];
        		}
        		totalWeight[0] = 0.0;
        		for (i = 0; i < nPoints; i++) {
        			totalWeight[0] += weight[i];
    	    	}
        		for (i = 0; i < numberClusters; i++) {
    	    		for (j = 0; j < nDims; j++) {
    	    		    medianList[j][i].clear();
    	    		}
    	    	}
    	    	for (i = 0; i < nPoints; i++) {
    	    		for (j = 0; j < nDims; j++) {
    	    			medianList[j][0].add(new positionWeightItem(pos[j][i],weight[i]));
    	    		}
    	    	}
	    		for (j = 0; j < nDims; j++) {
	    			Collections.sort(medianList[j][0], new positionWeightComparator());
	    		}
        		for (j = 0; j < nDims; j++) {
        			m = 0;
	                preMedianWeight = 0.0;
	                postMedianWeight = 0.0;
	    			while (postMedianWeight <= totalWeight[0]/2.0) {
	    				preMedianWeight = postMedianWeight;
	    				postMedianWeight += medianList[j][0].get(m++).getWeight();
	    			}
	    			if (m < 2) {
	    			    centroidPosStart[j][0] = medianList[j][0].get(0).getPosition();	
	    			}
	    			else {
	    				preMedianPos = medianList[j][0].get(m-2).getPosition();
	    				postMedianPos = medianList[j][0].get(m-1).getPosition();
	    				weightFraction = (totalWeight[0]/2.0 - preMedianWeight)/(postMedianWeight - preMedianWeight);
	    				centroidPosStart[j][0] = preMedianPos + (postMedianPos - preMedianPos)*weightFraction;
	    			}
        		}
    	    	for (presentClusters = 2; presentClusters <= numberClusters; presentClusters++) {
    	    		Preferences.debug("Present cluster number = " + presentClusters + "\n", Preferences.DEBUG_ALGORITHM);
    	    		fireProgressStateChanged("Present cluster number = " + presentClusters);
    	    		bestTotalMinDistCityBlock = Double.MAX_VALUE;
    	    	    for (initialClusterLocation = 0; initialClusterLocation < nPoints; initialClusterLocation++) {
    	    	    	changeOccurred = true;
    	    	    	iteration = 1;
    	    	    	for (i = 0; i < nDims; i++) {
    	    	    		for (j = 0; j < presentClusters-1; j++) {
    	    	    			centroidPos[i][j] = centroidPosStart[i][j];
    	    	    		}
    	    	    		// No weight since just one point is being assigned
    	    	    		centroidPos[i][presentClusters-1] = pos[i][initialClusterLocation];
    	    	    	}
    	    	    	while (changeOccurred) {
    	    	    		iteration++;
    	    	    		changeOccurred = false;
    	    	    		totalMinDistCityBlock = 0.0;
    	    	    		for (i = 0; i < presentClusters; i++) {
    	    	    			totalWeight[i] = 0.0;
    	    	    		}
    	    	    		if (equalScale) {
    	    	    			for (i = 0; i < nPoints; i++) {
    	    			    	    minDistCityBlock = 0.0;
    	    			    	    originalGroupNum = groupNum[i];
    	    		    	    	for (k = 0; k < nDims; k++) {
    	    		    	    		diff = Math.abs(pos[k][i] - centroidPos[k][0]);
    	    		    	    	    minDistCityBlock = minDistCityBlock + diff;
    	    		    	    	}
    	    		    	    	groupNum[i] = 0;
    	    			    	    for (j = 1; j < presentClusters; j++) {
	    				    	    	distCityBlock = 0.0;
	    				    	    	for (k = 0; k < nDims; k++) {
	    				    	    		diff = Math.abs(pos[k][i] - centroidPos[k][j]);
	    				    	    	    distCityBlock = distCityBlock + diff;
	    				    	    	}
	    				    	    	if (distCityBlock < minDistCityBlock) {
	    				    	    		minDistCityBlock = distCityBlock;
	    				    	    		groupNum[i] = j;
	    				    	    	}
    	    			    	    } // for (j = 1; j < presentClusters; j++)
    	    			    	    totalWeight[groupNum[i]] += weight[i];
    	    			    	    if (originalGroupNum != groupNum[i]) {
    	    			    	    	changeOccurred = true;
    	    			    	    }
    	    			    	    totalMinDistCityBlock += minDistCityBlock;
    	    			    	} // for (i = 0; i < nPoints; i++)	
    	    	    		} // if (equalScale)
    	    	    		else { // not equalScale
    	    			    	for (i = 0; i < nPoints; i++) {
    	    			    		minDistCityBlock = 0.0;
    	    			    	    originalGroupNum = groupNum[i];
    	    		    	    	for (k = 0; k < nDims; k++) {
    	    		    	    		diff = Math.abs(pos[k][i] - centroidPos[k][0]);
    	    		    	    	    minDistCityBlock = minDistCityBlock + scale[k]*diff;
    	    		    	    	}
    	    		    	    	groupNum[i] = 0;
    	    			    	    for (j = 1; j < presentClusters; j++) {
	    				    	    	distCityBlock = 0.0;
	    				    	    	for (k = 0; k < nDims; k++) {
	    				    	    		diff = Math.abs(pos[k][i] - centroidPos[k][j]);
	    				    	    	    distCityBlock = distCityBlock + scale[k]*diff;
	    				    	    	}
	    				    	    	if (distCityBlock < minDistCityBlock) {
	    				    	    		minDistCityBlock = distCityBlock;
	    				    	    		groupNum[i] = j;
	    				    	    	}
    	    			    	    } // for (j = 1; j < presentClusters; j++)
    	    			    	    totalWeight[groupNum[i]] += weight[i];
    	    			    	    if (originalGroupNum != groupNum[i]) {
    	    			    	    	changeOccurred = true;
    	    			    	    }
    	    			    	    totalMinDistCityBlock += minDistCityBlock;
    	    			    	} // for (i = 0; i < nPoints; i++)
    	    	    		} // else not equalScale
        		    	    for (i = 0; i < presentClusters; i++) {
        		    	    	totalWeight[i] = 0.0;
        		    	    }
        		    	    for (i = 0; i < numberClusters; i++) {
        	    	    		for (j = 0; j < nDims; j++) {
        	    	    		    medianList[j][i].clear();
        	    	    		}
        	    	    	}
        	    	    	for (i = 0; i < nPoints; i++) {
        	    	    		for (j = 0; j < nDims; j++) {
        	    	    			medianList[j][groupNum[i]].add(new positionWeightItem(pos[j][i],weight[i]));
        	    	    		}
        	    	    	}
        	    	    	for (i = 0; i <numberClusters; i++) {
        	    	    		for (j = 0; j < nDims; j++) {
        	    	    			Collections.sort(medianList[j][i], new positionWeightComparator());
        	    	    		}
        	    	    	}
        			    	clustersWithoutPoints = 0;
        			    	for (i = 0; i < presentClusters; i++) {
        			    		if (totalWeight[i] <= 1.0E-10) {
        			    			Preferences.debug("Cluster centroid " + (i+1) + " has no points\n", 
        			    					Preferences.DEBUG_ALGORITHM);
        			    			clustersWithoutPoints++;
        			    		}
        			    		else {
        				    		for (j = 0; j < nDims; j++) {
        				    			m = 0;
        	        	                preMedianWeight = 0.0;
        	        	                postMedianWeight = 0.0;
        	        	    			while (postMedianWeight <= totalWeight[i]/2.0) {
        	        	    				preMedianWeight = postMedianWeight;
        	        	    				postMedianWeight += medianList[j][i].get(m++).getWeight();
        	        	    			}
        	        	    			if (m < 2) {
        	        	    			    centroidPos[j][i] = medianList[j][i].get(0).getPosition();	
        	        	    			}
        	        	    			else {
        	        	    				preMedianPos = medianList[j][i].get(m-2).getPosition();
        	        	    				postMedianPos = medianList[j][i].get(m-1).getPosition();
        	        	    				weightFraction = (totalWeight[i]/2.0 - preMedianWeight)/(postMedianWeight - preMedianWeight);
        	        	    				centroidPos[j][i] = preMedianPos + (postMedianPos - preMedianPos)*weightFraction;
        	        	    			}
        				    		}
        			    		} // else
        			    	}	
    	    	    	} // while (changeOccurred)
    	    	    	Preferences.debug("There are " + clustersWithoutPoints + " clusters without points\n", 
    	    	    			Preferences.DEBUG_ALGORITHM);	
    	    	    	if (totalMinDistCityBlock < bestTotalMinDistCityBlock) {
    	    	    	    bestTotalMinDistCityBlock = totalMinDistCityBlock;
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
        	
        	} // switch(algoSelection)	
    	break;
    	} // switch(distanceMeasure)
    	
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
    	    
    	    Preferences.debug("Putting results in " + resultsFileName + "\n", Preferences.DEBUG_ALGORITHM);
        	System.out.println("Putting results in " + resultsFileName);
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
 
    	
    	Preferences.debug("Putting results in " + resultsFileName + "\n", Preferences.DEBUG_ALGORITHM);
    	System.out.println("Putting results in " + resultsFileName);
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
	
	private class positionWeightComparator implements Comparator<positionWeightItem> {

        /**
         * DOCUMENT ME!
         * 
         * @param o1 DOCUMENT ME!
         * @param o2 DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int compare(final positionWeightItem o1, final positionWeightItem o2) {
            final double a = o1.getPosition();
            final double b = o2.getPosition();

            if (a < b) {
                return -1;
            } else if (a > b) {
                return 1;
            } else {
                return 0;
            }
        }

    }
	
	private class positionWeightItem {

        /** DOCUMENT ME! */
        private final double position;

        /** DOCUMENT ME! */
        private final double weight;

        /**
         * Creates a new positionWieghtItem object.
         * 
         * @param position
         * @param weight
         */
        public positionWeightItem(final double position, final double weight) {
            this.position = position;
            this.weight = weight;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public double getPosition() {
            return position;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public double getWeight() {
            return weight;
        }

    }
	
}
