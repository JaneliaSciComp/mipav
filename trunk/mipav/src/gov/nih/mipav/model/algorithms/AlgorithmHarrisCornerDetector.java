package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.filters.AlgorithmGradientMagnitudeSep;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;

public class AlgorithmHarrisCornerDetector extends AlgorithmBase implements AlgorithmInterface{
    
    // References: 1.) Feature Extraction & Image Processing for Computer Vision Third Edition
	// by Mark S. Nixon and Alberto S. Aguado, Section 4.4.1.4 Moravec and Harris detectors,
	// pp. 188 - 192.
	// 2.) "A Combined Corner and Edge Detector" by Chris Harris & Mike Stephens, 1988.
	
    private static final int xOp = 1;
    
    private static final int yOp = 2;
    
    private float sigmas[];
    
    // Seen values of 0.04, 0.06, and 0.1 used
    private double k = 0.1;
    
    // No ideal what this value should be
    private double pointThreshold = 1.0;
    
    private float GxData[];
    
    private float GyData[];
    
    private int kExtents[];
    
 // Buffer to receive result of convolution operation
    private float[] Ix;
    
    private float[] Iy;
    
    private int operationType = xOp;
    
    
  //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmHarrisCornerDetector - default constructor.
     */
    public AlgorithmHarrisCornerDetector() { }
    
    /**
     * AlgorithmHarrisCornerDetector.
     *
     * @param  destImg  DOCUMENT ME!
     * @param  srcImg   DOCUMENT ME!
     * @param  highThreshold
     * @param  lowThreshold
     * @param  sigma
     */
    public AlgorithmHarrisCornerDetector(ModelImage destImg, ModelImage srcImg, float[] sigmas, double k,
    		double pointThreshold) {
        super(destImg, srcImg);
        this.sigmas = sigmas; 
        this.k = k;
        this.pointThreshold = pointThreshold;
    }


    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * finalize -
     */
    public void finalize() {
        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        int xDim;
        int yDim;
        int sliceSize;
        AlgorithmGradientMagnitudeSep gradMagAlgo = null;
        boolean entireImage = true;
        boolean image25D = true;
        int i;
        AlgorithmConvolver convolver;
        double IxIx;
        double IxIy;
        double IyIy;
        
        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
        
        fireProgressStateChanged(srcImage.getImageName(), "Running Harris Corner Detector ...");
        kExtents = new int[2];
        kExtents[0] = 3;
        kExtents[1] = 1;
        GxData = new float[]{-1, 0, 1};
        convolver = new AlgorithmConvolver(srcImage, GxData, kExtents,entireImage, image25D);
        convolver.setMinProgressValue(0);
        convolver.setMaxProgressValue(20);
        linkProgressToAlgorithm(convolver);
        convolver.addListener(this);
        if (!entireImage) {
            convolver.setMask(mask);
        }
        operationType = xOp;
        convolver.run();
        
        kExtents[0] = 1;
        kExtents[1] = 3;
        GyData = new float[]{-1, 0, 1};
        convolver = new AlgorithmConvolver(srcImage, GyData, kExtents,entireImage, image25D);
        convolver.setMinProgressValue(21);
        convolver.setMaxProgressValue(40);
        linkProgressToAlgorithm(convolver);
        convolver.addListener(this);
        if (!entireImage) {
            convolver.setMask(mask);
        }
        operationType = yOp;
        convolver.run();

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
       
        
        setCompleted(true);
        return;
    }
    
    public void algorithmPerformed(AlgorithmBase algorithm){
        if(!algorithm.isCompleted()){
            finalize();
            return;
        }
        if (algorithm instanceof AlgorithmConvolver) {
            AlgorithmConvolver convolver = (AlgorithmConvolver) algorithm;
            if (operationType == xOp) {
                Ix = convolver.getOutputBuffer();
            }
            else {
                Iy = convolver.getOutputBuffer();
            }
        }
    }
    
    
}