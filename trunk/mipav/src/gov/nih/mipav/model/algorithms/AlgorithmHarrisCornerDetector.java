package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;

public class AlgorithmHarrisCornerDetector extends AlgorithmBase implements AlgorithmInterface{
    
    // References: 1.) Feature Extraction & Image Processing for Computer Vision Third Edition
	// by Mark S. Nixon and Alberto S. Aguado, Section 4.4.1.4 Moravec and Harris detectors,
	// pp. 188 - 192.
	// 2.) "A Combined Corner and Edge Detector" by Chris Harris & Mike Stephens, 1988.
	
	// For the matrix gaussianBlur((Ix*Ix), sigma)   gaussianBlur((Ix*Iy), sigma)
	//                gaussianBlur((Ix*Iy), sigma)   gaussianBlur((Iy*Iy), sigma)
	// =
	//                Fx2         Fxy
	//                Fxy         Fy2
	// the product of the eigenvalues is Fx2*Fy2 - Fxy*Fxy = lambda1 * lambda2
	// and the sum of the eigenvalues is Fx2 + Fy2 = lambda1 + lambda2
	// Harris corner detector = (Fx2*Fy2 - Fxy*Fxy) - k * (Fx2 + Fy2)**2
	// Harris corner detector = (lambda1 * lambda2) - k (lambda1 + lambda2)**2
	// Set hcd to 0 if less than threshold.
	// Set hcd to 0 if its value is less than the value of an 8-connected neighbor.
	
    private static final int xOp = 1;
    
    private static final int yOp = 2;
    
    private static final int xxOp = 3;
    
    private static final int xyOp = 4;
    
    private static final int yyOp = 5;
    
    private float sigma;
    
    // Seen values of 0.04, 0.06, and 0.1 used
    private double k = 0.1;
    
    // No idea what this value should be
    private double pointThreshold = 1.0;
    
    // Buffer to receive result of convolution operation
    private float[] Ix;
    
    private float[] Iy;
    
    private float[] Fx2;
    
    private float[] Fxy;
    
    private float[] Fy2;
    
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
    public AlgorithmHarrisCornerDetector(ModelImage destImg, ModelImage srcImg, float sigma, double k,
    		double pointThreshold) {
        super(destImg, srcImg);
        this.sigma = sigma; 
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
        boolean entireImage = true;
        boolean image25D = true;
        int i;
        AlgorithmConvolver convolver;
        int xkDim, ykDim;
        int[] derivOrder = new int[2];
        float GxData[];
        float GyData[];
        int kExtents[];
        float[] GData;
        float IxIx[];
        float IxIy[];
        float IyIy[];
        float sigmaArray[] = new float[]{sigma, sigma};
        ModelImage IxIxImage;
        ModelImage IxIyImage;
        ModelImage IyIyImage;
        float sum;
        double hcd[];
        byte c8Max[];
        int x;
        int y;
        int index;
        
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
        convolver.setMaxProgressValue(10);
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
        convolver.setMinProgressValue(11);
        convolver.setMaxProgressValue(20);
        linkProgressToAlgorithm(convolver);
        convolver.addListener(this);
        if (!entireImage) {
            convolver.setMask(mask);
        }
        operationType = yOp;
        convolver.run();
        
        derivOrder[0] = 0;
        derivOrder[1] = 0;
        xkDim = Math.round(11 * sigma);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        if (xkDim < 3) {
            xkDim = 3;
        }

        kExtents[0] = xkDim;

        ykDim = Math.round(11 * sigma);

        if ((ykDim % 2) == 0) {
            ykDim++;
        }

        if (ykDim < 3) {
            ykDim = 3;
        }

        kExtents[1] = ykDim;

        GData = new float[xkDim * ykDim];
        GenerateGaussian G = new GenerateGaussian(GData, kExtents, sigmaArray, derivOrder);
        G.calc(false);


        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        IxIx = new float[sliceSize];
        IxIy = new float[sliceSize];
        IyIy = new float[sliceSize];
        for (i = 0; i < sliceSize; i++) {
        	IxIx[i] = Ix[i] * Ix[i];
        	IxIy[i] = Ix[i] * Iy[i];
        	IyIy[i] = Iy[i] * Iy[i];
        }
        
        IxIxImage = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), "IxIxImage");
        IxIyImage = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), "IxIyImage");
        IyIyImage = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), "IyIyImage");
        try {
        	IxIxImage.importData(0, IxIx, true);
        }
        catch(IOException e) {
        	MipavUtil.displayError("IOException " + e + " on IxIxImage.importData(0, IxIx, true");
        	setCompleted(false);
        	return;
        }
        IxIx = null;
        try {
        	IxIyImage.importData(0, IxIy, true);
        }
        catch(IOException e) {
        	MipavUtil.displayError("IOException " + e + " on IxIyImage.importData(0, IxIy, true");
        	setCompleted(false);
        	return;
        }
        IxIy = null;
        try {
        	IyIyImage.importData(0, IyIy, true);
        }
        catch(IOException e) {
        	MipavUtil.displayError("IOException " + e + " on IyIyImage.importData(0, IyIy, true");
        	setCompleted(false);
        	return;
        }
        IyIy = null;
        
        convolver = new AlgorithmConvolver(IxIxImage, GData, kExtents,entireImage, image25D);
        convolver.setMinProgressValue(21);
        convolver.setMaxProgressValue(30);
        linkProgressToAlgorithm(convolver);
        convolver.addListener(this);
        if (!entireImage) {
            convolver.setMask(mask);
        }
        operationType = xxOp;
        convolver.run();
        
        convolver = new AlgorithmConvolver(IxIyImage, GData, kExtents,entireImage, image25D);
        convolver.setMinProgressValue(31);
        convolver.setMaxProgressValue(40);
        linkProgressToAlgorithm(convolver);
        convolver.addListener(this);
        if (!entireImage) {
            convolver.setMask(mask);
        }
        operationType = xyOp;
        convolver.run();
        
        convolver = new AlgorithmConvolver(IyIyImage, GData, kExtents,entireImage, image25D);
        convolver.setMinProgressValue(41);
        convolver.setMaxProgressValue(50);
        linkProgressToAlgorithm(convolver);
        convolver.addListener(this);
        if (!entireImage) {
            convolver.setMask(mask);
        }
        operationType = yyOp;
        convolver.run();
        
        IxIxImage.disposeLocal();
        IxIyImage.disposeLocal();
        IyIyImage.disposeLocal();
        
        hcd = new double[sliceSize];
        for (i = 0; i < sliceSize; i++) {
        	if (entireImage || mask.get(i)) {
	        	sum = Fx2[i] + Fy2[i];
	        	hcd[i] = (Fx2[i]*Fy2[i] - Fxy[i]*Fxy[i]) - k * sum *sum;
	        	if (hcd[i] < pointThreshold) {
	        		hcd[i] = 0;
	        	}
        	}
        }
        c8Max = new byte[sliceSize];
        for (i = 0; i < sliceSize; i++) {
        	c8Max[i] = 1;
        }
        for (y = 0; y < yDim; y++) {
        	for (x = 0; x < xDim; x++) {
        		index = x + y * xDim;
        		if (entireImage || mask.get(index)) {
        		    if ((x > 1) && (hcd[index] < hcd[index-1])) {
        		        c8Max[index] = 0;
        		        continue;
        		    }
        		    if ((x < xDim - 1) && (hcd[index] < hcd[index+1])) {
        		    	c8Max[index] = 0;
        		    	continue;
        		    }
        		    if ((y > 1) && (hcd[index] < hcd[index-xDim])) {
        		        c8Max[index] = 0;
        		        continue;
        		    }
        		    if ((y < yDim - 1) && (hcd[index] < hcd[index+xDim])) {
        		    	c8Max[index] = 0;
        		    	continue;
        		    }
        		    if ((x > 1) && (y > 1) && (hcd[index] < hcd[index-xDim-1])) {
        		    	c8Max[index] = 0;
        		    	continue;
        		    }
        		    if ((x > 1) && (y < yDim - 1) && (hcd[index] < hcd[index+xDim-1])) {
        		    	c8Max[index] = 0;
        		    	continue;
        		    }
        		    if ((x < xDim -1) && (y > 1) && (hcd[index] < hcd[index-xDim+1])) {
        		    	c8Max[index] = 0;
        		    	continue;
        		    }
        		    if ((x < xDim-1) && (y < yDim-1) && (hcd[index] < hcd[index+xDim+1])) {
        		    	c8Max[index] = 0;
        		    }
        		}
        	}
        }
        
        
        for (y = 0; y < yDim; y++) {
        	for (x = 0; x < xDim; x++) {
        		index = x + y * xDim;
        		if (entireImage || mask.get(index)) {
        			if (c8Max[index] == 0) {
        				hcd[index] = 0;
        			}
        		}
            }
        }
        
        try {
        	destImage.importData(0, hcd, true);
        }
        catch(IOException e) {
        	MipavUtil.displayError("IOException " + e + " on destImage.importData, hcd, true");
        	setCompleted(false);
        	return;
        }
        
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
            else if (operationType == yOp){
                Iy = convolver.getOutputBuffer();
            }
            else if (operationType == xxOp) {
            	Fx2 = convolver.getOutputBuffer();
            }
            else if (operationType == xyOp) {
            	Fxy = convolver.getOutputBuffer();
            }
            else {
            	Fy2 = convolver.getOutputBuffer();
            }
        }
    }
    
    
}