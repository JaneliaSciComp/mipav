package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;
import java.util.Vector;

public class AlgorithmHarrisLaplace extends AlgorithmBase implements AlgorithmInterface{
	
	/*
	 * The author of the original kp_harrislaplace.m MATLAB code is Vincent Garcia.
	 * The date of the original code is 05/12/2007.
	 * This code extracts keypoints, also called feature points, corners, or interest points
	 * from an input gray level image.
	 * 
	 * Vincent Garica has kindly given MIPAV permission to use and modify his code.
			
	 Reference: K. Mikolajczyk and C. Schmid.  Scale & affine interest point detectors.
	            International Journal of Computer Vision, 2004.
	 * 
	 */
	
    private static final int xOp = 1;
    
    private static final int yOp = 2;
    
    private static final int xxOp = 3;
    
    private static final int xyOp = 4;
    
    private static final int yyOp = 5;
    
    private static final int logOp = 6;
    
    // Buffer to receive result of convolution operation
    private float[] Ix;
    
    private float[] Iy;
    
    private float[] Ix2;
    
    private float[] Ixy;
    
    private float[] Iy2;
    
    private float[] log;
    
    private int operationType = xOp;
    
    private ModelImage destImage[];
    
 // epsilon = D1MACH(4)
    // Machine epsilon is the smallest positive epsilon such that
    // (1.0 + epsilon) != 1.0.
    // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
    // epsilon = 2.2204460e-16
    // epsilon is called the largest relative spacing
    private double epsilon = Math.pow(2, -52);
    
    
  //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmHarrisLaplace - default constructor.
     */
    public AlgorithmHarrisLaplace() { }
    
    /**
     * AlgorithmHarrisLaplace.
     *
     * @param  destImg  DOCUMENT ME!
     * @param  srcImg   DOCUMENT ME!
     */
    public AlgorithmHarrisLaplace(ModelImage destImage[], ModelImage srcImg) {
        super(null, srcImg);
        this.destImage = destImage;
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
        ModelImage IxIxImage;
        ModelImage IxIyImage;
        ModelImage IyIyImage;
        // Scale parameters
        double sigmaBegin = 1.5;
        double sigmaStep = 1.2;
        int sigmaNB = 13;
        double sigmaArray[] = new double[sigmaNB];
        Vector<Integer> harrisPtsX = new Vector<Integer>();
        Vector<Integer> harrisPtsY = new Vector<Integer>();
        Vector<Integer> harrisPtsScale = new Vector<Integer>();
        // Integration scale
        double sI;
        // Derivative scale
        double sD;
        int xk;
        int xm[];
        int j;
        double denom;
        double denom2;
        float sigma2D[] = new float[2];
        double cim[];
        double sum;
        // Factor in original Harris measure
        double k = 0.06;
        int x;
        int y;
        double distSquared;
        double radiusSquared;
        int nb;
        double maxLocal[];
        byte localMask[][];
        int x2;
        int y2;
        double neighborhoodMax;
        boolean found;
        int index;
        double globalMax;
        double threshold;
        double sL;
        int halfMask;
        float srcBuffer[];
        float expandedBuffer[];
        int expandedSize;
        ModelImage expandedImage;
        double kf;
        int loopVal;
        float laplace_snlo[][];
        double total;
        double mean;
        
        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
        
        fireProgressStateChanged(srcImage.getImageName(), "Running Harris Laplace...");
        
        for (i = 0; i < sigmaNB; i++) {
        	sigmaArray[i] = sigmaBegin*Math.pow(sigmaStep, i);
        }
        
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        kExtents = new int[2];
        
        // Part 1: HARRIS
        for (i = 0; i < sigmaNB; i++) {
        	loopVal = (50 *i)/sigmaNB;
        	fireProgressStateChanged(loopVal);
        	// scale (standard deviation)
        	sI = sigmaArray[i]; // Integration scale
        	sD = 0.7 * sI;  // Derivative mask
        	
        	// Derivative mask
        	xk = (int)Math.round(3*sD);
        	xm = new int[2*xk + 1];
        	for (j = 0; j <= 2*xk; j++) {
        		xm[j] = j - xk; 
        	}
        	
        	kExtents[0] = 2*xk + 1;
        	kExtents[1] = 1;
        	GxData = new float[2*xk + 1];
        	denom = 2.0 * sD * sD;
        	denom2 = sD * sD * sD * Math.sqrt(2.0 * Math.PI);
        	for (j = 0; j <= 2*xk; j++) {
        		GxData[j] = (float)(xm[j]* Math.exp(-xm[j]*xm[j]/denom)/denom2);
        	}
        	convolver = new AlgorithmConvolver(srcImage, GxData, kExtents,entireImage, image25D);
	        //convolver.setMinProgressValue(0);
	        //convolver.setMaxProgressValue(10);
	        //linkProgressToAlgorithm(convolver);
	        convolver.addListener(this);
	        if (!entireImage) {
	            convolver.setMask(mask);
	        }
	        operationType = xOp;
	        convolver.run();
	        
	        kExtents[0] = 1;
	        kExtents[1] = 2*xk + 1;
	        GyData = new float[2*xk + 1];
	        for (j = 0; j <= 2*xk; j++) {
	        	GyData[j] = GxData[j];
	        }
	        convolver = new AlgorithmConvolver(srcImage, GyData, kExtents,entireImage, image25D);
	        //convolver.setMinProgressValue(11);
	        //convolver.setMaxProgressValue(20);
	        //linkProgressToAlgorithm(convolver);
	        convolver.addListener(this);
	        if (!entireImage) {
	            convolver.setMask(mask);
	        }
	        operationType = yOp;
	        convolver.run();
	        
	        // Auto-correlation matrix
	        derivOrder[0] = 0;
	        derivOrder[1] = 0;
	        xkDim = (int)Math.round(11 * sI);

	        if ((xkDim % 2) == 0) {
	            xkDim++;
	        }

	        if (xkDim < 3) {
	            xkDim = 3;
	        }

	        kExtents[0] = xkDim;

	        ykDim = (int)Math.round(11 * sI);

	        if ((ykDim % 2) == 0) {
	            ykDim++;
	        }

	        if (ykDim < 3) {
	            ykDim = 3;
	        }

	        kExtents[1] = ykDim;

	        GData = new float[xkDim * ykDim];
	        sigma2D[0] = (float)sI;
	        sigma2D[1] = (float)sI;
	        GenerateGaussian G = new GenerateGaussian(GData, kExtents, sigma2D, derivOrder);
	        G.calc(false);

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
	        //convolver.setMinProgressValue(41);
	        //convolver.setMaxProgressValue(50);
	        //linkProgressToAlgorithm(convolver);
	        convolver.addListener(this);
	        if (!entireImage) {
	            convolver.setMask(mask);
	        }
	        operationType = xxOp;
	        convolver.run();
	        
	        convolver = new AlgorithmConvolver(IxIyImage, GData, kExtents,entireImage, image25D);
	        //convolver.setMinProgressValue(51);
	        //convolver.setMaxProgressValue(60);
	        //linkProgressToAlgorithm(convolver);
	        convolver.addListener(this);
	        if (!entireImage) {
	            convolver.setMask(mask);
	        }
	        operationType = xyOp;
	        convolver.run();
	        
	        convolver = new AlgorithmConvolver(IyIyImage, GData, kExtents,entireImage, image25D);
	        //convolver.setMinProgressValue(61);
	        //convolver.setMaxProgressValue(70);
	        //linkProgressToAlgorithm(convolver);
	        convolver.addListener(this);
	        if (!entireImage) {
	            convolver.setMask(mask);
	        }
	        operationType = yyOp;
	        convolver.run();
	        
	        IxIxImage.disposeLocal();
	        IxIyImage.disposeLocal();
	        IyIyImage.disposeLocal();
	        
	        // Interest point response
	        cim = new double[sliceSize];
	        for (i = 0; i < sliceSize; i++) {
	        	if (entireImage || mask.get(i)) {
		        	sum = Ix2[i] + Iy2[i];
		        	// Original Harris measure
		        	//cim[i] = (Ix2[i]*Iy2[i] - Ixy[i]*Ixy[i]) - k * sum *sum;
		        	// Alison Noble measure
		        	cim[i] = (Ix2[i]*Iy2[i] - Ixy[i]*Ixy[i])/(sum + epsilon);
	        	}
	        }
	        
	        // Find local maxima on neighborhood
	        // Find unique local maxima using filtering fast
	        halfMask = (int)Math.ceil(3 * sI);
	        radiusSquared = 9.0 * sI * sI;
	        nb = 0;
	        localMask = new byte[2*halfMask + 1][2*halfMask+1];
	        maxLocal = new double[sliceSize];
	        for (y = -halfMask; y <= halfMask; y++) {
	        	for (x = -halfMask; x <= halfMask; x++) {
	        	    distSquared = x*x + y*y;
	        	    if (distSquared <= radiusSquared) {
	        	        nb++;
	        	        localMask[y + halfMask][x + halfMask] = 1;
	        	    }
	        	}
	        }
	        localMask[halfMask][halfMask] = 0;
	        globalMax = -Double.MAX_VALUE;
	        for (y = halfMask; y <= yDim - 1 - halfMask; y++) {
	        	for (x = halfMask; x <= xDim - 1 - halfMask; x++) {
	        		index = x + y * xDim;
	        	    neighborhoodMax = cim[index];
	        	    found = true;
	        	    for (y2 = -halfMask; (y2 <= halfMask) && found; y2++) {
	        	    	for (x2 = -halfMask; (x2 <= halfMask) && found; x2++) {
	        	    	    if (localMask[y2 + halfMask][x2 + halfMask] == 1) {
	        	    	       if (cim[(y + y2)*xDim + x + x2] >= neighborhoodMax) {
	        	    	    	   found = false;
	        	    	       }
	        	    	    }
	        	    	}
	        	    } // for (y2 = -halfMask; (y2 <= halfMask) && found; y2++)
	        	    if (found) {
	        	    	maxLocal[index] = cim[index];
	        	    	if (maxLocal[index] > globalMax) {
	        	    		globalMax = maxLocal[index];
	        	    	}
	        	    }
	        	} // for (x = halfMask; x <= xDim - 1 - halfMask; x++)
	        } // for (y = halfMask; y <= yDim - 1 - halfMask; y++)
	        
	        threshold = 0.2 * globalMax;
	        // Find local maxima greater than threshold
	        // Build interest points
	        for (y = 0; y < yDim; y++) {
	        	for (x = 0; x < xDim; x++) {
	        		index = x + y * xDim;
	        		if (maxLocal[index] >= threshold) {
	        			harrisPtsX.add(x);
	        			harrisPtsY.add(y);
	        			harrisPtsScale.add(i+1);
	        		}
	        	}
	        }
        } // for (i = 0; i < sigmaNB; i++)
        
        // PART 2: LAPLACE
        // Compute scale-normalized laplacian operator
        laplace_snlo = new float[sigmaNB][sliceSize];
        srcBuffer = new float[sliceSize];
        try {
        	srcImage.exportData(0, sliceSize, srcBuffer);
        }
        catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on srcImage.exportData(0, sliceSize, srcBuffer");
            setCompleted(false);
            return;
        }
        for (i = 0; i < sigmaNB; i++) {
        	sL =  sigmaArray[i]; // scale
        	halfMask = (int)Math.floor(6*sL+1);
        	//To eliminate the zero-padding artifacts around the edge of the image, use an alternative boundary
        	// padding method called border replication. In border replication, the value of any pixel outside
        	// the image is determined by replicating the value from the nearest border pixel. 
        	expandedSize = (xDim + 2 * halfMask) * (yDim + 2 * halfMask);
        	expandedBuffer = new float[expandedSize];
        	for (y = 0; y < yDim; y++) {
        		for (x = 0; x < xDim; x++) {
        			expandedBuffer[x + halfMask + (y + halfMask) * (xDim + 2 * halfMask)] = srcBuffer[x + y * xDim];
        		}
        	}
        	for (x = 0; x < halfMask; x++) {
        		for (y = 0; y < halfMask; y++) {
        			expandedBuffer[x + y * (xDim + 2 * halfMask)] = srcBuffer[0];	
        		}
        		
        		for (y = halfMask; y <= yDim + halfMask - 1; y++) {
        			expandedBuffer[x + y * (xDim + 2 * halfMask)] = srcBuffer[(y - halfMask)* xDim];	
        		}
        		
        		for (y = yDim + halfMask; y <= yDim + 2*halfMask - 1; y++) {
        			expandedBuffer[x + y * (xDim + 2 * halfMask)] = srcBuffer[(yDim - 1)* xDim];	
        		}
        	} // for (x = 0; x < halfMask; x++)
        	
        	for (x = xDim + halfMask; x < xDim + 2 * halfMask; x++) {
        		for (y = 0; y < halfMask; y++) {
        			expandedBuffer[x + y * (xDim + 2 * halfMask)] = srcBuffer[xDim-1];	
        		}
        		
        		for (y = halfMask; y <= yDim + halfMask - 1; y++) {
        			expandedBuffer[x + y * (xDim + 2 * halfMask)] = srcBuffer[(y - halfMask)* xDim + xDim - 1];	
        		}
        		
        		for (y = yDim + halfMask; y <= yDim + 2*halfMask - 1; y++) {
        			expandedBuffer[x + y * (xDim + 2 * halfMask)] = srcBuffer[(yDim - 1)* xDim + xDim - 1];	
        		}	
        	} // for (x = xDim + halfMask; x < xDim + 2 * halfMask; x++)
        	
        	for (y = 0; y < halfMask; y++) {
        		for (x = halfMask; x <= xDim + halfMask - 1; x++) {
        			expandedBuffer[x + y * (xDim + 2 * halfMask)] = srcBuffer[x - halfMask];		
        		}
        	}
        	
        	for (y = yDim + halfMask; y < yDim + 2*halfMask; y++) {
        		for (x = halfMask; x <= xDim + halfMask - 1; x++) {
        			expandedBuffer[x + y * (xDim + 2 * halfMask)] = srcBuffer[(yDim - 1)* xDim + x - halfMask];		
        		}
        	}
        	int extents[] = new int[2];
        	extents[0] = xDim + 2 * halfMask;
        	extents[1] = yDim + 2 * halfMask;
        	expandedImage = new ModelImage(ModelStorageBase.FLOAT, extents, "expandedImage");
        	try {
        		expandedImage.importData(0, expandedBuffer, true);
        	}
        	catch(IOException e) {
        		MipavUtil.displayError("IOException " + e + " on expandedImage.importData(0, expandedBuffer, true)");
        		setCompleted(false);
        		return;
        	}
        	kExtents[0] = 2*halfMask + 1;
        	kExtents[1] = 2*halfMask + 1;
        	GData = new float[kExtents[0] * kExtents[1]];
        	denom = 2.0 * sL * sL;
        	kf = -1.0/(Math.PI * sL * sL);
        	for (y = -halfMask; y <= halfMask; y++) {
        		for (x = -halfMask; x <= halfMask; x++) {
        		    distSquared = x * x + y * y;
        		    GData[(x + halfMask) + (y + halfMask) * kExtents[0]] = (float)(kf * (1.0 - distSquared/denom) *
        		    		         Math.exp(-distSquared/denom));
        		}
        	} // for (y = -halfMask; y <= halfMask; y++)
        	
        	convolver = new AlgorithmConvolver(srcImage, GData, kExtents,entireImage, image25D);
	        //convolver.setMinProgressValue(11);
	        //convolver.setMaxProgressValue(20);
	        //linkProgressToAlgorithm(convolver);
	        convolver.addListener(this);
	        if (!entireImage) {
	            convolver.setMask(mask);
	        }
	        operationType = logOp;
	        convolver.run();
	        // Ensure 0 DC
	        total = 0;
	        for (y = halfMask; y <= yDim + halfMask -1; y++) {
	        	for (x = halfMask; x <= xDim + halfMask - 1; x++) {
	        	    total += log[x + y * (xDim + 2 * halfMask)];	
	        	}
	        }
	        mean = total/sliceSize;
	        for (y = halfMask; y <= yDim + halfMask -1; y++) {
	        	for (x = halfMask; x <= xDim + halfMask - 1; x++) {
	        	    laplace_snlo[i][x- halfMask + (y - halfMask)* xDim] = (float)(log[x + y * (xDim + 2 * halfMask)] - mean);	
	        	}
	        }
        } // for (i = 0; i < sigmaNB; i++)
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
            	Ix2 = convolver.getOutputBuffer();
            }
            else if (operationType == xyOp) {
            	Ixy = convolver.getOutputBuffer();
            }
            else if (operationType == yyOp){
            	Iy2 = convolver.getOutputBuffer();
            }
            else {
            	log = convolver.getOutputBuffer();
            }
        }
    }
}