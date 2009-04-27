package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
   This is a port of MATLAB routines contained in kernelRegressionBasedImageProcessingToolBox_ver1-2beta written by
   Hiroyuki Takeda, Sina Farsiu, and Peyman Milanfar.
   References are:
   1.) Takeda, H., Farsiu, S., and Milanfar, P., "Kernel Regression for Image Processing and Reconstruction",
   IEEE Transactions on Image Processing, Vol. 16, No. 2, February, 2007, pp. 349-366.
   2.) Takeda, H., Farsiu, S., and Milanfar, P., "Robust Kernel Regression for Restoration and Reconstruction of
   Images from Sparse Noisy Data", Proceedings of the International Conference on Image Processing (ICIP), 
   Atlanta, Georgia, October, 2006.
   3.) Takeda, H., M.S. Thesis, "Kernel Regression for Image Processing and Reconstruction",
   Electrical Engineering, UC Santa Cruz, March, 2006.
 */
public class AlgorithmKernelRegression extends AlgorithmBase {
    public static final int REGULARLY_SAMPLED_SECOND_ORDER_CLASSIC = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    private int method;
    
    private float globalSmoothing;
    
    private int upscale;
    
    /** The kernel must be kernelSize by kernelSize */
    private int kernelSize;

    /** In 3D if do25D == true, process each slice separately. */
    private boolean do25D = true;

    private int nDims;
    
    private ModelImage horizontalGradientImage;
    
    private ModelImage verticalGradientImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmKernelRegression object.
     *
     * @param  destImage         denoised image
     * @param  srcImg            2D or 3D source image
     * @param  globalSmoothing
     * @param  upscale
     * @param  kernelSize
     * @param  do25D             If true, do slice by slice filtering
     */
    public AlgorithmKernelRegression(ModelImage destImage, ModelImage srcImg, int method,
            float globalSmoothing, int upscale, int kernelSize, boolean do25D) {
        super(destImage, srcImg);
        this.method = method;
        this.globalSmoothing = globalSmoothing;
        this.upscale = upscale;
        this.kernelSize = kernelSize;
        this.do25D = do25D;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImage = null;
        destImage = null;
       
        super.finalize();
    }


    /**
     * Starts the nonlocal means filter algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }
        
        nDims = srcImage.getNDims();
        
        if (method == REGULARLY_SAMPLED_SECOND_ORDER_CLASSIC) {
            if ((nDims == 2) || do25D) {
                ckr2Regular();   
            }
        }
        
        
    }
    
    /*  2D processing on a second order classic kernel regression function for regularly sampled data */
    private void ckr2Regular() {
        long time;
        int xDim;
        int yDim;
        int zDim;
        int length;
        float input[];
        float output[];
        float horizontalGradient[];
        float verticalGradient[];
        int upscaleSquared = upscale * upscale;
        int radius;
        float x1[];
        float x2[];
        int x;
        int y;
        int upKernelSize;
        float start;
        float increment;
        float A[][][][];
        float xx1[];
        float xx2[];
        int i;
        int j;
        int xDimDown;
        int yDimDown;
        int kernelSizeSquared = kernelSize * kernelSize;
        int kernelSize5SquaredPlus = 5 * kernelSizeSquared + 1;
        float Xx[];
        float tt[];
        float W[];
        double escale = -0.5/(globalSmoothing * globalSmoothing);
        
        time = System.currentTimeMillis();
        fireProgressStateChanged(0, srcImage.getImageName(), "Kernel regression filter");
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        length = xDim * yDim;
        input = new float[length];
        if (nDims == 3) {
            zDim = srcImage.getExtents()[2];
        }
        else {
            zDim = 1;
        } 
        output = new float[upscaleSquared * length];
        horizontalGradient = new float[upscaleSquared * length];
        verticalGradient = new float[upscaleSquared * length];
        int extents[];
        if (nDims == 3) {
            extents = new int[3];
            extents[0] = xDim * upscale;
            extents[1] = yDim * upscale;
            extents[2] = zDim;
        }
        else {
            extents = new int[2];
            extents[0] = xDim * upscale;
            extents[1] = yDim * upscale;
        }
        horizontalGradientImage = new ModelImage(ModelStorageBase.FLOAT, extents,
                                  srcImage.getImageName() + "_horizontalGradient");
        verticalGradientImage = new ModelImage(ModelStorageBase.FLOAT, extents,
                                srcImage.getImageName() + "_verticalGradient");
        
        // Create the equivalent kernels
        radius = (kernelSize - 1)/2;
        upKernelSize = upscale * kernelSize;
        x1 = new float[upKernelSize * upKernelSize];
        x2 = new float[upKernelSize * upKernelSize];
        start = -radius - (float)(upscale - 1)/(float)(upscale);
        increment = 1.0f/(float)(upscale);
        for (y = 0; y < upKernelSize; y++) {
            for (x = 0; x < upKernelSize; x++) {
                x2[x + y * kernelSize] = start + x * increment;
                x1[x + y * kernelSize] = start + y * increment;
            }
        }
        A = new float[6][kernelSizeSquared][upscale][upscale];
        xx1 = new float[kernelSizeSquared*kernelSizeSquared];
        xx2 = new float[kernelSizeSquared*kernelSizeSquared];
        tt = new float[kernelSizeSquared * kernelSizeSquared];
        W = new float[kernelSizeSquared * kernelSizeSquared];
        for (i = 1; i <= upscale; i++) {
            yDimDown = (upKernelSize - upscale + i)/upscale;
            for (j = 1; j <= upscale; j++) {
                xDimDown = (upKernelSize - upscale + j)/upscale;
                for (y = 0; y < yDimDown; y++) {
                    for (x = 0; x < xDimDown; x++) {
                        xx1[x + y * xDimDown] = 
                        x1[upscale - j + x * upscale + xDimDown * (upscale - i + y * upscale)];
                        xx2[x + y * xDimDown] =
                        x2[upscale - j + x * upscale + xDimDown * (upscale - i + y * upscale)];
                    }
                }
                Xx = new float[kernelSize5SquaredPlus*kernelSizeSquared];
                for (y = 0; y < kernelSizeSquared; y++) {
                    Xx[y * kernelSize5SquaredPlus] = 1.0f;
                    for (x = 0; x < kernelSizeSquared; x++) {
                        Xx[1 + y * kernelSize5SquaredPlus] = xx1[x + y * kernelSizeSquared];
                        Xx[1 + kernelSizeSquared + x + y * kernelSize5SquaredPlus] =
                        xx2[x + y * kernelSizeSquared];
                        Xx[1 + 2*kernelSizeSquared + x + y * kernelSize5SquaredPlus] =
                        xx1[x + y * kernelSizeSquared] * xx1[x + y * kernelSizeSquared];
                        Xx[1 + 3*kernelSizeSquared + x + y * kernelSize5SquaredPlus] =
                        xx1[x + y * kernelSizeSquared] * xx2[x + y * kernelSizeSquared];
                        Xx[1 + 4*kernelSizeSquared + x + y * kernelSize5SquaredPlus] =
                        xx2[x + y * kernelSizeSquared] * xx2[x + y * kernelSizeSquared];
                        // The weight matrix (Gaussian kernel function)
                        tt[x + y * kernelSizeSquared] = xx1[x + y * kernelSizeSquared] +
                                                        xx2[x + y * kernelSizeSquared];
                        W[x + y * kernelSizeSquared] = (float)Math.exp(escale * tt[x + y * kernelSizeSquared]);
                    }
                }
            }
        }
    } // ckr2Regular


}
