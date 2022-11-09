package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;

public class AlgorithmHarrisCornerDetector extends AlgorithmBase implements AlgorithmInterface{
	
	// Copyright (c) 2002-2010 Peter Kovesi
	// Centre for Exploration Targeting
	// The University of Western Australia
	// http://www.csse.uwa.edu.au/~pk/research/matlabfns/
	// 
	// Permission is hereby granted, free of charge, to any person obtaining a copy
	// of this software and associated documentation files (the "Software"), to deal
	// in the Software without restriction, subject to the following conditions:
	// 
	// The above copyright notice and this permission notice shall be included in 
	// all copies or substantial portions of the Software.
	//
	// The Software is provided "as is", without warranty of any kind.

	// March    2002 - Original version
	// December 2002 - Updated comments
	// August   2005 - Changed so that code calls nonmaxsuppts
    // August   2010 - Changed to use Farid and Simoncelli's derivative filters

    // Portions of the original code have been ported to Java
    // References: 1.) Feature Extraction & Image Processing for Computer Vision Third Edition
	// by Mark S. Nixon and Alberto S. Aguado, Section 4.4.1.4 Moravec and Harris detectors,
	// pp. 188 - 192.
	// 2.) "A Combined Corner and Edge Detector" by Chris Harris & Mike Stephens, 1988.
	// 3.) Alison Noble, "Descriptions of Image Surfaces", PhD thesis, Department of Engineering
	// Oxford University, 1989, p.45.
	
	// Calculate first derivatives using the 5-tap coefficients given by
	// Farid and Simoncelli.  For the x derivative first use the prefilter
	// interpolating coefficents in the y direction and then use the
	// d1 derivative coefficients in the x direction.
	// For the matrix gaussianBlur((Ix*Ix), sigma)   gaussianBlur((Ix*Iy), sigma)
	//                gaussianBlur((Ix*Iy), sigma)   gaussianBlur((Iy*Iy), sigma)
	// =
	//                Fx2         Fxy
	//                Fxy         Fy2
	// the product of the eigenvalues is Fx2*Fy2 - Fxy*Fxy = lambda1 * lambda2
	// and the sum of the eigenvalues is Fx2 + Fy2 = lambda1 + lambda2
	// Original Harris corner detector = (Fx2*Fy2 - Fxy*Fxy) - k * (Fx2 + Fy2)**2
	// Original Harris corner detector = (lambda1 * lambda2) - k (lambda1 + lambda2)**2
	// Nobel modified corner detector = (Fx2*Fy2 - Fxy*Fxy)/(Fx2 + Fy2 + epsilon)
	// Set some values to zero with nonmaximum suppression.
	
    private static final int xOp = 1;
    
    private static final int yOp = 2;
    
    private static final int xxOp = 3;
    
    private static final int xyOp = 4;
    
    private static final int yyOp = 5;
    
    // epsilon = D1MACH(4)
    // Machine epsilon is the smallest positive epsilon such that
    // (1.0 + epsilon) != 1.0.
    // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
    // epsilon = 2.2204460e-16
    // epsilon is called the largest relative spacing
    private double epsilon = Math.pow(2, -52);
    
    private float sigma;
    
    private int radius = 1;   // Radius of the region considered in non-maximal suppression.
	                          // Typical values to use might be 1-3 pixels.
    // Seen values of 0.04, 0.06, and 0.1 used
    //private double k = 0.1;
    
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
     * @param  sigma
     * @param  radius
     * @param  pointThreshold
     */
    public AlgorithmHarrisCornerDetector(ModelImage destImg, ModelImage srcImg, float sigma, int radius,
    		double pointThreshold) {
        super(destImg, srcImg);
        this.sigma = sigma; 
        this.radius = radius;
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
        double cim[];
        AlgorithmNonMaxSuppts nonMaxAlgo;
        ModelImage cimImage;
        final int useSimpleDerivatives = 1;
        final int useFaridAndSimoncelli = 2;
        int derivativeMethod = useFaridAndSimoncelli;
        float p[];
        float d1[];
        ModelImage IxImage;
        ModelImage IyImage;
        VOIVector VOIs;
        
        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
        
        fireProgressStateChanged(srcImage.getImageName(), "Running Harris Corner Detector ...");
        kExtents = new int[2];
        switch (derivativeMethod) {
        case useSimpleDerivatives:
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
        break;
        case useFaridAndSimoncelli:
        	p = new float[]{0.037659f, 0.249153f, 0.426375f, 0.249153f, 0.037659f};
        	d1 = new float[]{-0.109604f, -0.276691f, 0.0f, 0.276691f, 0.109604f};
        	kExtents[0] = 1;
        	kExtents[1] = 5;
        	convolver = new AlgorithmConvolver(srcImage, p, kExtents,entireImage, image25D);
 	        convolver.setMinProgressValue(0);
 	        convolver.setMaxProgressValue(10);
 	        linkProgressToAlgorithm(convolver);
 	        convolver.addListener(this);
 	        if (!entireImage) {
 	            convolver.setMask(mask);
 	        }
 	        operationType = xOp;
 	        convolver.run();
 	        IxImage = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), "IxImage");
 	        
 	        try {
 	        	IxImage.importData(0, Ix, true);
 	        }
 	        catch(IOException e) {
 	        	MipavUtil.displayError("IOException " + e + " on IxImage.importData(0, Ix, true");
 	        	setCompleted(false);
 	        	return;	
 	        }
 	        
 	        kExtents[0] = 5;
       	    kExtents[1] = 1;
       	    convolver = new AlgorithmConvolver(IxImage, d1, kExtents,entireImage, image25D);
	        convolver.setMinProgressValue(11);
	        convolver.setMaxProgressValue(20);
	        linkProgressToAlgorithm(convolver);
	        convolver.addListener(this);
	        if (!entireImage) {
	            convolver.setMask(mask);
	        }
	        operationType = xOp;
	        convolver.run();
	        
	        kExtents[0] = 5;
        	kExtents[1] = 1;
        	convolver = new AlgorithmConvolver(srcImage, p, kExtents,entireImage, image25D);
 	        convolver.setMinProgressValue(21);
 	        convolver.setMaxProgressValue(30);
 	        linkProgressToAlgorithm(convolver);
 	        convolver.addListener(this);
 	        if (!entireImage) {
 	            convolver.setMask(mask);
 	        }
 	        operationType = yOp;
 	        convolver.run();
 	        IyImage = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(), "IyImage");
 	        
 	        try {
 	        	IyImage.importData(0, Iy, true);
 	        }
 	        catch(IOException e) {
 	        	MipavUtil.displayError("IOException " + e + " on IyImage.importData(0, Iy, true");
 	        	setCompleted(false);
 	        	return;	
 	        }
 	        
 	        kExtents[0] = 1;
       	    kExtents[1] = 5;
       	    convolver = new AlgorithmConvolver(IyImage, d1, kExtents, entireImage, image25D);
	        convolver.setMinProgressValue(31);
	        convolver.setMaxProgressValue(40);
	        linkProgressToAlgorithm(convolver);
	        convolver.addListener(this);
	        if (!entireImage) {
	            convolver.setMask(mask);
	        }
	        operationType = yOp;
	        convolver.run();
	        
	        IxImage.disposeLocal();
	        IyImage.disposeLocal();
        break;
        } // switch(derivativeMethod)
        
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
        convolver.setMinProgressValue(41);
        convolver.setMaxProgressValue(50);
        linkProgressToAlgorithm(convolver);
        convolver.addListener(this);
        if (!entireImage) {
            convolver.setMask(mask);
        }
        operationType = xxOp;
        convolver.run();
        
        convolver = new AlgorithmConvolver(IxIyImage, GData, kExtents,entireImage, image25D);
        convolver.setMinProgressValue(51);
        convolver.setMaxProgressValue(60);
        linkProgressToAlgorithm(convolver);
        convolver.addListener(this);
        if (!entireImage) {
            convolver.setMask(mask);
        }
        operationType = xyOp;
        convolver.run();
        
        convolver = new AlgorithmConvolver(IyIyImage, GData, kExtents,entireImage, image25D);
        convolver.setMinProgressValue(61);
        convolver.setMaxProgressValue(70);
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
        
        cim = new double[sliceSize];
        for (i = 0; i < sliceSize; i++) {
        	if (entireImage || mask.get(i)) {
	        	sum = Fx2[i] + Fy2[i];
	        	//cim[i] = (Fx2[i]*Fy2[i] - Fxy[i]*Fxy[i]) - k * sum *sum;
	        	cim[i] = (Fx2[i]*Fy2[i] - Fxy[i]*Fxy[i])/(sum + epsilon);
        	}
        }
        
        cimImage = new ModelImage(ModelStorageBase.DOUBLE, srcImage.getExtents(), "cimImage");
        try {
        	cimImage.importData(0, cim, true);
        }
        catch(IOException e) {
        	MipavUtil.displayError("IOException " + e + " on cimImage.importData(0, cim, true");
        	setCompleted(false);
        	return;
        }
        
        VOIs = srcImage.getVOIs();
        nonMaxAlgo = new AlgorithmNonMaxSuppts(destImage, cimImage, VOIs, radius, pointThreshold);
        nonMaxAlgo.run();
        srcImage.setVOIs(VOIs);
        srcImage.notifyImageDisplayListeners();
        
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