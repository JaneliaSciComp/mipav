package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.Color;


/**
 
 */
public class AlgorithmKMeans extends AlgorithmBase {

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

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     
     */
    public AlgorithmKMeans(ModelImage image, int[][] pos, double[] scale, int groupNum[], double[][] centroidPos) {

        this.image = image;
        this.pos = pos;
        this.scale = scale;
        this.groupNum = groupNum;
        this.centroidPos = centroidPos;

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
    	int i, j, k;
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
    	
    	nDims = pos.length;
    	nPoints = pos[0].length;
    	numberClusters = centroidPos[0].length;
    	pointsInCluster = new int[numberClusters];
    	
    	// Randomly choose one point as the starting centroid of each cluster
    	startingPointIndex = new int[numberClusters];
    	randomGen = new RandomNumberGen();
    	for (i = 0; i < numberClusters; i++) {
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
    		    centroidPos[j][i] = pos[j][possibleStart];	
    		}
    	} // for (i = 0; i < numberClusters; i++)
    	startingPointIndex = null;
    	
    	changeOccurred = true;
    	iteration = 1;
    	while (changeOccurred) {
    		fireProgressStateChanged("Iteration = " + iteration);
    		Preferences.debug("Iteration = " + iteration + "\n");
    		iteration++;
    		changeOccurred = false;
    		for (i = 0; i < numberClusters; i++) {
    			pointsInCluster[i] = 0;
    		}
	    	for (i = 0; i < nPoints; i++) {
	    	    distSquared = 0.0;
	    	    minDistSquared = Double.MAX_VALUE;
	    	    originalGroupNum = groupNum[i];
	    	    for (j = 0; j < numberClusters; j++) {
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
	    	for (i = 0; i < numberClusters; i++) {
	    		Preferences.debug("Cluster " + (i+1) + ":\n");
	    		for (j = 0; j < nDims; j++) {
	    			centroidPos[j][i] = centroidPos[j][i]/pointsInCluster[i];
	    			Preferences.debug("Dimension " + (j+1) + " = " + centroidPos[j][i] + "\n");
	    		}
	    	}
    	} // while (changeOccurred)
    	
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
	    		newPtVOI = new VOI((short) (i), "point" + i + ".voi", VOI.POINT, -1.0f);
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
	    		newPtVOI = new VOI((short) (i), "cluster" + i + ".voi", VOI.POINT, -1.0f);
	    		newPtVOI.setColor(Color.black);
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
    	} // if ((image != null) && (nDims >= 2) && (nDims <= 3) && (numberClusters <= 9)
        setCompleted(true);
        return;
    }


    
}
