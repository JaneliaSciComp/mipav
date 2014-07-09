package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;

public class AlgorithmLawsTexture extends AlgorithmBase {
	
	/** Level 3 */
	private static final int L3[] = new int[]{1, 2, 1};
	
	/** Edge 3 */
	private static final int E3[] = new int[]{-1, 0, 1};
	
	/** Spot 3 */
	private static final int S3[] = new int[]{-1, 2, -1};
	
	/** Level 5 */
	private static final int L5[] = new int[]{1, 4, 6, 4, 1};
	
	/** Edge 5 */
	private static final int E5[] = new int[]{-1, -2, 0 , 2, 1};
	
	/** Spot 5 */
	private static final int S5[] = new int[]{-1, 0, 2, 0, -1};
	
	/** Wave 5 */
	private static final int W5[] = new int[]{-1, 2, 0, -2, 1};
	
	/** Ripple 5 */
	private static final int R5[] = new int[]{1, -4, 6, -4, 1};
	
	/** Level 7 */
	private static final int L7[] = new int[]{1, 6, 15, 20, 15, 6, 1};
	
	/** Edge 7 */
	private static final int E7[] = new int[]{-1, -4, -5, 0, 5, 4, 1};
	
	/** Spot 7 */
	private static final int S7[] = new int[]{-1, -2, 1, 4, 1, -2, -1};
	
	/** Wave 7 */
	private static final int W7[] = new int[]{-1, 0, 3, 0, -3, 0, 1};
	
	/** Ripple 7 */
	private static final int R7[] = new int[]{1, -2, -1, 4, -1, -2, 1};
	
	/** Oscillation 7 */
	private static final int O7[] = new int[]{-1, 6, -15, 20, -15, 6, -1};
	
	private ModelImage[] destImage = null;
	
	/** Size of square window must be 3, 5, or 7 */
	private int windowSize = 5;
	
	public AlgorithmLawsTexture(ModelImage[] destImg, ModelImage srcImg, int windowSize) {
		super(null, srcImg);
        destImage = destImg;
        this.windowSize = windowSize;
		
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

        if (threadStopped) {
            finalize();

            return;
        }

        

        fireProgressStateChanged(0, null, "Running Laws textures ...");
        
        calculateLaws();
    }

    /**
     * DOCUMENT ME!
     */
    private void calculateLaws() {
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;
        int zDim;
        int E3E3[][] = null;
        int E3L3[][] = null;
        int E3S3[][] = null;
        int L3E3[][] = null;
        int L3L3[][] = null;
        int L3S3[][] = null;
        int S3E3[][] = null;
        int S3L3[][] = null;
        int S3S3[][] = null;
        int E5E5[][] = null;
        int E5L5[][] = null;
        int E5R5[][] = null;
        int E5S5[][] = null;
        int E5W5[][] = null;
        int L5E5[][] = null;
        int L5L5[][] = null;
        int L5R5[][] = null;
        int L5S5[][] = null;
        int L5W5[][] = null;
        int R5E5[][] = null;
        int R5L5[][] = null;
        int R5R5[][] = null;
        int R5S5[][] = null;
        int R5W5[][] = null;
    }
	
}