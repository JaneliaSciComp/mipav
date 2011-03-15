package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.Color;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;


/**
 
 */
public class AlgorithmKMeans extends AlgorithmBase {
	
	private static final int RANDOM_INIT = 0;
	
	private static final int BRADLEY_FAYYAD_INIT = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------
	
	private ModelImage image;
	
	// Take resolutions from the image
    // Use 1.0 in every dimension if not scaled.
    // Subscript goes from 0 to nDims - 1
    private double scale[];
    
    // First subscript x = 0, y = 1, z = 2, t = 3
    // Second subscript 0 to nPoints-1 for each point
    // Value is the point position
	private int[][] pos;
	
	// subscript goes from 0 to nPoints-1 for each point
    // Value is the cluster number from 0 to numberClusters-1.
	private int[] groupNum;
	
	// First subscript x = 0, y = 1, z = 2, t = 3
    // Second subscript 0 to numberClusters-1 for each cluster
    // Value is the cluster position
	private double[][] centroidPos;
	
	private String resultsFileName;
	
	private int initSelection;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     
     */
    public AlgorithmKMeans(ModelImage image, int[][] pos, double[] scale, int groupNum[], double[][] centroidPos,
    		               String resultsFileName, int initSelection) {

        this.image = image;
        this.pos = pos;
        this.scale = scale;
        this.groupNum = groupNum;
        this.centroidPos = centroidPos;
        this.resultsFileName = resultsFileName;
        this.initSelection = initSelection;
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
    	
    	nDims = pos.length;
    	nPoints = pos[0].length;
    	numberClusters = centroidPos[0].length;
    	pointsInCluster = new int[numberClusters];
    	
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
	    		    Preferences.debug("Dimension " + (j+1) + "  " + centroidPos[j][i] + "\n");
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
    		randomGen = new RandomNumberGen();
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
                // Randomly select starting points in the subsample
                for (j = 0; j < numberClusters; j++) {
                	startingPointIndex[j] = -1;
                }
                for (j = 0; j < numberClusters; j++) {
                    do {
                        alreadyUsed = false;
                        possibleStart = randomGen.genUniformRandomNum(0, subsampleSize - 1);
                        for (k = 0; k < j; k++) {
                            if (startingPointIndex[k] == possibleStart) {
                            	alreadyUsed = true;
                            }
                        } // for (k = 0; k < j; k++)
                    } while(alreadyUsed);
                    startingPointIndex[j] = possibleStart;
                    groupNum[possibleStart] = j;
                    for (k = 0; k < nDims; k++) {
                    	centroidPos[k][j] = subsamplePos[k][possibleStart];
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
            		}
        	    	for (j = 0; j < subsampleSize; j++) {
        	    	    minDistSquared = Double.MAX_VALUE;
        	    	    originalGroupNum = groupNum[j];
        	    	    for (k = 0; k < numberClusters; k++) {
        	    	    	distSquared = 0.0;
        	    	    	for (m = 0; m < nDims; m++) {
        	    	    		diff = subsamplePos[m][j] - centroidPos[m][k];
        	    	    	    distSquared = distSquared + scale[m]*scale[m]*diff*diff;
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
	        	    			Preferences.debug("Dimension " + (k+1) + " = " + centroidPos[k][j] + "\n");
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
                    for (k = 0; k < numberClusters; k++) {
                    	if (pointsInCluster[k] > 1) {
                    	    for (m = 0; m < subsampleSize; m++) {
                    	    	if (groupNum[m] == k) {
                    	    	    distSquared = 0.0;
                    	    	    for (n = 0; n < nDims; n++) {
                    	    	        diff = subsamplePos[n][m] - centroidPos[n][k];
                    	    	        distSquared = distSquared + scale[n]*scale[n]*diff*diff;
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
                        	fireProgressStateChanged("Iteration = " + iteration + " on subsample number " + (i+1));
                        	Preferences.debug("Iteration = " + iteration + " on subsample number " + (i+1));
                        	iteration++;
                        	changeOccurred = false;
                        	for (j = 0; j < numberClusters; j++) {
                    			pointsInCluster[j] = 0;
                    		}
                	    	for (j = 0; j < subsampleSize; j++) {
                	    	    minDistSquared = Double.MAX_VALUE;
                	    	    originalGroupNum = groupNum[j];
                	    	    for (k = 0; k < numberClusters; k++) {
                	    	    	distSquared = 0.0;
                	    	    	for (m = 0; m < nDims; m++) {
                	    	    		diff = subsamplePos[m][j] - centroidPos[m][k];
                	    	    	    distSquared = distSquared + scale[m]*scale[m]*diff*diff;
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
        	        	    			Preferences.debug("Dimension " + (k+1) + " = " + centroidPos[k][j] + "\n");
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
        	    	for (j = 0; j < subsampleSize; j++) {
        	    	    minDistSquared = Double.MAX_VALUE;
        	    	    originalGroupNum = groupNum[j];
        	    	    for (k = 0; k < numberClusters; k++) {
        	    	    	distSquared = 0.0;
        	    	    	for (m = 0; m < nDims; m++) {
        	    	    		diff = unionCM[m][j] - centroidPos[m][k];
        	    	    	    distSquared = distSquared + scale[m]*scale[m]*diff*diff;
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
        	    	} // for (j = 0; j < subsampleSIze; j++)
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
	        	    			Preferences.debug("Dimension " + (k+1) + " = " + centroidPos[k][j] + "\n");
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
            for (i = 0; i < subsampleNumber; i++) {
                for (j = 0; j < subsampleSize; j++) {
                    minDistSquared = Double.MAX_VALUE;
                    for (k = 0; k < numberClusters; k++) {
                        distSquared = 0.0;
                        for (m = 0; m < nDims; m++) {
                        	diff = unionCM[m][j] - localFM[m][k][i];
                        	distSquared = distSquared + scale[m]*scale[m]*diff*diff;
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
            Preferences.debug("Refinement algorithm returns inital centroids at:\n");
            
            for (i = 0; i < numberClusters; i++) {
            	Preferences.debug("Initial centroid " + (i+1) + "\n");
            	for (j = 0; j < nDims; j++) {
            		centroidPos[j][i] = localFM[j][i][bestFMIndex];
            		Preferences.debug("Dimension " + (j+1) + "  " + centroidPos[j][i] + "\n");
            	}
            }
               
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
    		}
	    	for (i = 0; i < nPoints; i++) {
	    	    minDistSquared = Double.MAX_VALUE;
	    	    originalGroupNum = groupNum[i];
	    	    for (j = 0; j < numberClusters; j++) {
	    	    	distSquared = 0.0;
	    	    	for (k = 0; k < nDims; k++) {
	    	    		diff = pos[k][i] - centroidPos[k][j];
	    	    	    distSquared = distSquared + scale[k]*scale[k]*diff*diff;
	    	    	}
	    	    	if (distSquared < minDistSquared) {
	    	    		minDistSquared = distSquared;
	    	    		groupNum[i] = j;
	    	    	}
	    	    } // for (j = 0; j < numberClusters; j++)
	    	    pointsInCluster[groupNum[i]]++;
	    	    if (originalGroupNum != groupNum[i]) {
	    	    	changeOccurred = true;
	    	    }
	    	} // for (i = 0; i < nPoints; i++)
	    	for (i = 0; i < numberClusters; i++) {
	    		for (j = 0; j < nDims; j++) {
	    			centroidPos[j][i] = 0.0;
	    		}
	    	}
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
		    			Preferences.debug("Dimension " + (j+1) + " = " + centroidPos[j][i] + "\n");
		    		}
	    		} // else
	    	}
    	} // while (changeOccurred)
    	Preferences.debug("There are " + clustersWithoutPoints + " clusters without points\n");
    	
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
	    		xArr[0] = pos[0][i];
	    		yArr[0] = pos[1][i];
	    		if (nDims == 2) {
	    			zArr[0] = 0.0f;
	    		}
	    		else {
	    			zArr[0] = pos[2][i];
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
