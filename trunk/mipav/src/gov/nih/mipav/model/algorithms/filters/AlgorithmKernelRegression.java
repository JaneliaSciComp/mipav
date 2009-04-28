package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import Jama.*;


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
    
    public static final int ITERATIVE_STEERING_KERNEL_SECOND_ORDER = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    private int method;
    
    private float initialGlobalSmoothing;
    
    private float iterativeGlobalSmoothing;
    
    private int upscale;
    
    /** The kernel must be initialKernelSize by initialKernelSize */
    private int initialKernelSize;
    
    private int iterativeKernelSize;
    
    /** The total number of iterations */
    private int iterations;
    
    /** The size of the local orientation analysis window */
    private int windowSize;
    
    /** The regularization for the elongation parameter */
    private float lambda;
    
    /** The structure sensitive parameter */
    private float alpha;

    /** In 3D if do25D == true, process each slice separately. */
    private boolean do25D = true;

    private int nDims;
    
    private ModelImage horizontalGradientImage;
    
    private ModelImage verticalGradientImage;
    
    private float output[];
    
    private float horizontalGradient[];
    
    private float verticalGradient[];
    
    private int extents[];

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmKernelRegression object.
     *
     * @param  destImage         denoised image
     * @param  srcImg            2D or 3D source image
     * @param  initialGlobalSmoothing
     * @param  iterativeGlobalSmoothing
     * @param  upscale           Upscaling factor
     * @param  initialKernelSize
     * @param  iterativeKernelSize
     * @param  iterations        Total number of iterations
     * @param  windowSize        Size of the local orientation analysis window
     * @param  lambda            Regularization for the elongation parameter
     * @param  alpha             Structure sensitive parameter
     * @param  do25D             If true, do slice by slice filtering
     */
    public AlgorithmKernelRegression(ModelImage destImage, ModelImage srcImg, int method,
            float initialGlobalSmoothing, float iterativeGlobalSmoothing,
            int upscale, int initialKernelSize, int iterativeKernelSize, int iterations,
            int windowSize, float lambda, float alpha, boolean do25D) {
        super(destImage, srcImg);
        this.method = method;
        this.initialGlobalSmoothing = initialGlobalSmoothing;
        this.iterativeGlobalSmoothing = iterativeGlobalSmoothing;
        this.upscale = upscale;
        this.initialKernelSize = initialKernelSize;
        this.iterativeKernelSize = iterativeKernelSize;
        this.iterations = iterations;
        this.windowSize = windowSize;
        this.lambda = lambda;
        this.alpha = alpha;
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
        long time;

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }
        
        time = System.currentTimeMillis();
        fireProgressStateChanged(0, srcImage.getImageName(), "Kernel regression filter");
        
        nDims = srcImage.getNDims();
        
        if (method == REGULARLY_SAMPLED_SECOND_ORDER_CLASSIC) {
            if ((nDims == 2) || do25D) {
                ckr2Regular();
                try {
                    if (destImage != null) {
                        destImage.importData(0, output, true);
                    }
                    else {
                        if (upscale != 1) {
                            srcImage.reallocate(srcImage.getType(), extents);
                        }
                        srcImage.importData(0, output, true);
                    }
                }
                catch (IOException e) {
                    MipavUtil.displayError("IOException on importData");
                    setCompleted(false);
                    return;
                }
            } // if ((nDims == 2) || do25D)
        } // if (method == REGULARLY_SAMPLED_SECOND_ORDER_CLASSIC)
        else if (method == ITERATIVE_STEERING_KERNEL_SECOND_ORDER) {
            if ((nDims == 2) || do25D) {
                ckr2Regular();   
            } // if ((nDims == 2) || do25D)
        } // else if (method == ITERATIVE_STEERING_KERNEL_SECOND_ORDER)
        
        fireProgressStateChanged(100);
        time = System.currentTimeMillis() - time;
        Preferences.debug("Seconds elapsed in AlgorithmKernelRegression = " + (time/1000.0) + "\n");
        setCompleted(true);
        return;
        
        
    }
    
    /*  2D processing on a second order classic kernel regression function for regularly sampled data */
    private void ckr2Regular() {
        int xDim;
        int yDim;
        int zDim;
        int length;
        float input[];
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
        int initialKernelSizeSquared = initialKernelSize * initialKernelSize;
        double Xx[][];
        double tt[];
        double W[];
        double escale = -0.5/(initialGlobalSmoothing * initialGlobalSmoothing);
        double Xw[][];
        Matrix matXx;
        Matrix matXw;
        double prod[][];
        int z;
        int padXDim;
        int padYDim;
        float input2[];
        float inputp[];
        int xp;
        int yp;
        int xx;
        int yy;
        int k;
        
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
        output = new float[upscaleSquared * length * zDim];
        horizontalGradient = new float[upscaleSquared * length * zDim];
        verticalGradient = new float[upscaleSquared * length * zDim];
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
        
        // Create the equivalent kernels
        radius = (initialKernelSize - 1)/2;
        upKernelSize = upscale * initialKernelSize;
        x1 = new float[upKernelSize * upKernelSize];
        x2 = new float[upKernelSize * upKernelSize];
        start = -radius - (float)(upscale - 1)/(float)(upscale);
        increment = 1.0f/(float)(upscale);
        for (y = 0; y < upKernelSize; y++) {
            for (x = 0; x < upKernelSize; x++) {
                x2[x + y * upKernelSize] = start + x * increment;
                x1[x + y * upKernelSize] = start + y * increment;
            }
        }
        A = new float[6][initialKernelSizeSquared][upscale][upscale];
        xx1 = new float[initialKernelSizeSquared];
        xx2 = new float[initialKernelSizeSquared];
        tt = new double[initialKernelSizeSquared];
        W = new double[initialKernelSizeSquared];
        Xx = new double[initialKernelSizeSquared][6];
        Xw = new double[initialKernelSizeSquared][6];
        for (i = 1; i <= upscale; i++) {
            for (j = 1; j <= upscale; j++) {
                for (y = 0; y < initialKernelSize; y++) {
                    for (x = 0; x < initialKernelSize; x++) {
                        // Store in xx1 and xx2 as one long column formed from the columns of the
                        // 2D xx1 and xx2
                        xx1[x * initialKernelSize + y] = 
                        x1[upscale - j + x * upscale + upKernelSize * (upscale - i + y * upscale)];
                        xx2[x * initialKernelSize + y] =
                        x2[upscale - j + x * upscale + upKernelSize * (upscale - i + y * upscale)];
                    } // for (x = 0; x < initialKernelSize; x++)
                } // for (y = 0; y < initialKernelSize; y++)
                for (y = 0; y < initialKernelSizeSquared; y++) {
                    Xx[y][0] = 1.0;
                    Xx[y][1] = xx1[y];
                    Xx[y][2] = xx2[y];
                    Xx[y][3] = xx1[y] * xx1[y];
                    Xx[y][4] = xx1[y] * xx2[y];
                    Xx[y][5] = xx2[y] * xx2[y];
                    // The weight matrix (Gaussian kernel function)
                    tt[y] = xx1[y] * xx1[y] + xx2[y] * xx2[y];
                    W[y] = Math.exp(escale * tt[y]);
                    // Equivalent kernel
                    Xw[y][0] = W[y];
                    Xw[y][1] = xx1[y] * W[y];
                    Xw[y][2] = xx2[y] * W[y];
                    Xw[y][3] = Xx[y][3] * W[y];
                    Xw[y][4] = Xx[y][4] * W[y];
                    Xw[y][5] = Xx[y][5] * W[y];
                } // for (y = 0; y < initialKernelSizeSquared; y++)
                matXx = new Matrix(Xx);
                matXw = new Matrix(Xw);
                prod = ((((matXx.transpose()).times(matXw)).inverse()).times(matXw.transpose())).getArray();
                for (y = 0; y < 6; y++) {
                    for (x = 0; x < initialKernelSizeSquared; x++) {
                        A[y][x][i][j] = (float)prod[y][x];
                    }
                }
            } // for (j = 1; j <= upscale; j++)
        } // for (i = 1; i <= upscale; i++)
        
        padXDim = xDim + 2 * radius;
        padYDim = yDim + 2 * radius;
        input2 = new float[padXDim * padYDim];
        inputp = new float[initialKernelSizeSquared];
        for (z = 0; z < zDim; z++) {
            try {
                srcImage.exportData(z*length, length, input);
            }
            catch (IOException e) {
                MipavUtil.displayError("IOException on srcImage.exportData(z*length, length, input)");
                setCompleted(false);
                return;
            }
            
            // Mirror the input image
            for (y = 0; y < yDim; y++) {
                for (x = 0; x < xDim; x++) {
                    input2[x + radius + padXDim*(y + radius)] = input[x + y * xDim];
                }
            }
            
            for (y = 0; y < yDim; y++) {
                for (x = 0; x < radius; x++) {
                    // left side mirror reflection
                    input2[x + padXDim*(y + radius)] = input[(radius - x) + xDim * y];
                    // right side mirror reflection
                    input2[x + xDim + radius + padXDim * (y + radius)] =
                    input[xDim - 2 - x + xDim * y];
                }
            }
            for (y = 0; y < radius; y++) {
                for (x = 0; x < xDim; x++) {
                    // top side mirror reflection
                    input2[x + radius + padXDim * y] = input[x + xDim * (radius - y)];
                    // bottom side mirror reflection
                    input2[x + radius + padXDim * (y + yDim + radius)] =
                    input[x + xDim * (yDim - 2 - y)];
                }
            }
            for (y = 0; y < radius; y++) {
                for (x = 0; x < radius; x++) {
                    // left top mirror reflection
                    input2[x + padXDim * y] = input[(radius - x) + xDim * (radius - y)];
                    // left bottom mirror reflection
                    input2[x + padXDim * (y + yDim + radius)] = 
                    input[(radius - x) + xDim * (yDim - 2 - y)];
                    // right top mirror reflection
                    input2[x + xDim + radius + padXDim * y] =
                    input[xDim - 2 - x + xDim * (radius - y)];
                    // right bottom mirror reflection
                    input2[x + xDim + radius + padXDim * (y + yDim + radius)] =
                    input[xDim - 2 - x + xDim * (yDim - 2 - y)];
                }
            }
            
            // Estimate the image and its first gradients
            for (y = 1; y <= yDim; y++) {
                for (x = 1; x <= xDim; x++) {
                    for (yp = 0; yp < initialKernelSize; yp++) {
                        for (xp = 0; xp < initialKernelSize; xp++) {
                            // Store inputp in one long column
                            inputp[yp + xp * initialKernelSize] = input2[x - 1 + xp + padXDim * (y - 1 + yp)];
                        }
                    }
                    
                    for (i = 1; i <= upscale; i++) {
                        yy = (y - 1) * upscale + i;
                        for (j = 1; j <= upscale; j++) {
                            xx = (x - 1) * upscale + j;
                            for (k = 0; k < initialKernelSizeSquared; k++) {
                                output[xx - 1 + extents[0] * (yy - 1) + extents[0]*extents[1]*z] =
                                A[0][k][i-1][j-1] * inputp[k];
                                verticalGradient[xx - 1 + extents[0] * (yy - 1) + extents[0]*extents[1]*z] =
                                A[1][k][i-1][j-1] * inputp[k];
                                horizontalGradient[xx - 1 + extents[0] * (yy - 1) + extents[0]*extents[1]*z] =
                                A[2][k][i-1][j-1] * inputp[k];
                            }
                        } // for (j = 1; j <= upscale; j++)
                    } // for (i = 1; i <= upscale; i++)
                } // for (x = 1; x <= xDim; x++)
            } // for (y = 1; y <= yDim; y++)
        } // for (z = 0; z < zDim; z++)
        
    } // ckr2Regular


}
