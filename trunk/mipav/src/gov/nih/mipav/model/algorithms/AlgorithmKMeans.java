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

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     
     */
    public AlgorithmKMeans(ModelImage image, int[][] pos, double[] scale, int groupNum[], double[][] centroidPos,
    		               String resultsFileName) {

        this.image = image;
        this.pos = pos;
        this.scale = scale;
        this.groupNum = groupNum;
        this.centroidPos = centroidPos;
        this.resultsFileName = resultsFileName;

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
    @SuppressWarnings("null")
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
        File file;
        RandomAccessFile raFile;
        String dataString = "";
    	
    	nDims = pos.length;
    	nPoints = pos[0].length;
    	numberClusters = centroidPos[0].length;
    	pointsInCluster = new int[numberClusters];
    	
    	// Randomly choose one point as the starting centroid of each cluster
    	startingPointIndex = new int[numberClusters];
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
	    	for (i = 0; i < numberClusters; i++) {
	    		Preferences.debug("Cluster centroid " + (i+1) + ":\n");
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
