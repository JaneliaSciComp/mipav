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
	
	private int[][] pos;
	
	private int numClusters;
	
	private double[] xValueArray;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     
     */
    public AlgorithmKMeans(ModelImage image, int[] groupNum, int[][] pos, int numClusters) {

        this.image = image;
        this.groupNum = groupNum;
        this.pos = pos;
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
