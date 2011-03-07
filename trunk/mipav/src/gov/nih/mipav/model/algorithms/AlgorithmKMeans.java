package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 
 */
public class AlgorithmKMeans extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------
	
	private ModelImage image;
	
	private int[] groupNum;
	
	private int[] xPos;
	
	private int[] yPos;
	
	private int[] zPos;
	
	private int[] tPos;
	
	private int numClusters;
	
	private double[] xValueArray;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     
     */
    public AlgorithmKMeans(ModelImage image, int[] groupNum, int[] xPos,
    		                        int[] yPos, int[] zPos, int[] tPos, int numClusters) {

        this.image = image;
        this.groupNum = groupNum;
        this.xPos = xPos;
        this.yPos = yPos;
        this.zPos = zPos;
        this.tPos = tPos;
        this.numClusters = numClusters;

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
        
        setCompleted(true);
        return;
    }


    
}
