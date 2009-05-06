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
    
    public static final int STEERING_KERNEL_SECOND_ORDER_L1_NORM = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    private int method;
    
    private float initialGlobalSmoothing;
    
    private float iterativeGlobalSmoothing;
    
    /** Global smoothing parameter in skr2L1_regular in STEERING_KERNEL_SECOND_ORDER_L1_NORM */
    private float iterativeGlobalSmoothing2;
    
    private int upscale;
    
    /** The kernel must be initialKernelSize by initialKernelSize */
    private int initialKernelSize;
    
    private int iterativeKernelSize;
    
    /** The total number of iterations */
    private int iterations;
    
    /** Iterations of the steepest descent method in L1 steering kernel regression
     *  in skr2L1_regular in STEERING_KERNEL_SECOND_ORDER_L1_NORM
     */
    private int iterations2;
    
    /** The size of the local orientation analysis window */
    private int windowSize;
    
    /** The regularization for the elongation parameter */
    private float lambda;
    
    /** The structure sensitive parameter */
    private float alpha;
    
    private float classicStepSize;
    
    private float steeringStepSize;

    /** In 3D if do25D == true, process each slice separately. */
    private boolean do25D = true;

    private int nDims;
    
    private float output[];
    
    private float horizontalGradient[];
    
    private float verticalGradient[];
    
    private int extents[];
    
    private int xDim;
    
    private int yDim;
    
    private int zDim;
    
    private float C[][][][][];
    
    private boolean I[];
    
    private float input[];
    
    private ModelImage medianImage;
    
    private float zInit[];
    
    //  Scale factor used in RGB-CIELab conversions.  255 for ARGB, could be higher for ARGB_USHORT.
    private double scaleMax = 255.0; 

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmKernelRegression object.
     *
     * @param  destImage         denoised image
     * @param  srcImg            2D or 3D source image
     * @param  initialGlobalSmoothing
     * @param  iterativeGlobalSmoothing
     * @param  iterativeGlobalSmoothing2 Global smoothing in L1 steering kernel regression
     * @param  upscale           Upscaling factor
     * @param  initialKernelSize
     * @param  iterativeKernelSize
     * @param  iterations        Total number of iterations
     * @param  iterations2       Iterations in L1 steering kernel regression
     * @param  windowSize        Size of the local orientation analysis window
     * @param  lambda            Regularization for the elongation parameter
     * @param  alpha             Structure sensitive parameter
     * @param  classicStepSize
     * @param  steeringStepSize
     * @param  do25D             If true, do slice by slice filtering
     */
    public AlgorithmKernelRegression(ModelImage destImage, ModelImage srcImg, int method,
            float initialGlobalSmoothing, float iterativeGlobalSmoothing, float iterativeGlobalSmoothing2, 
            int upscale, int initialKernelSize, int iterativeKernelSize, int iterations, int iterations2, 
            int windowSize, float lambda, float alpha, float classicStepSize, float steeringStepSize, boolean do25D) {
        super(destImage, srcImg);
        this.method = method;
        this.initialGlobalSmoothing = initialGlobalSmoothing;
        this.iterativeGlobalSmoothing = iterativeGlobalSmoothing;
        this.iterativeGlobalSmoothing2 = iterativeGlobalSmoothing2;
        this.upscale = upscale;
        this.initialKernelSize = initialKernelSize;
        this.iterativeKernelSize = iterativeKernelSize;
        this.iterations = iterations;
        this.iterations2 = iterations2;
        this.windowSize = windowSize;
        this.lambda = lambda;
        this.alpha = alpha;
        this.classicStepSize = classicStepSize;
        this.steeringStepSize = steeringStepSize;
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
        int i;
        int j;
        float min;
        float max;
        double imageMax;
        AlgorithmMedian algoMedian;
        int medianIters;
        int kernelSize;
        int kernelShape;
        float stdDev;
        boolean adaptiveSize;
        int maximumSize;
        boolean wholeImage;
        boolean sliceBySlice;

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }
        
        time = System.currentTimeMillis();
        fireProgressStateChanged(0, srcImage.getImageName(), "Kernel regression filter");
        
        nDims = srcImage.getNDims();
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        if (nDims == 3) {
            zDim = srcImage.getExtents()[2];
        }
        else {
            zDim = 1;
        } 
        
        if (method == REGULARLY_SAMPLED_SECOND_ORDER_CLASSIC) {
            if ((nDims == 2) || do25D) {
                if (srcImage.isColorImage()) {
                    srcImage.calcMinMax();
                    imageMax = srcImage.getMax(); 
                    scaleMax = Math.max(255.0, imageMax);
                    convertRGBtoCIELab();
                    ckr2RegularCIELab();
                } // if (srcImage.isColorImage())
                else {
                    ckr2Regular();
                }
            } // if ((nDims == 2) || do25D)
        } // if (method == REGULARLY_SAMPLED_SECOND_ORDER_CLASSIC)
        else if (method == ITERATIVE_STEERING_KERNEL_SECOND_ORDER) {
            if ((nDims == 2) || do25D) {
                if (srcImage.isColorImage()) {
                    srcImage.calcMinMax();
                    imageMax = srcImage.getMax();
                    scaleMax = Math.max(255.0, imageMax);
                    convertRGBtoCIELab();
                    ckr2RegularCIELab();
                    // output and input are upscaled if upscale > 1.
                    input = new float[output.length];
                    for (i = 0; i < output.length; i++) {
                        input[i] = output[i];
                    }
                    // Set upscaling to 1 so upscaling no longer occurs anymore
                    // Otherwise you would upscale with each iteration
                    upscale = 1;
                    I = new boolean[horizontalGradient.length];
                    for (i = 0; i < I.length; i++) {
                        I[i] = true;
                    }
                    for (i = 1; i <= iterations; i++) {
                        fireProgressStateChanged((i - 1) * 100/iterations);
                        // Compute steering matrix
                        steering();
                        skr2RegularCIELab();
                        for (j = 0; j < output.length; j++) {
                            input[j] = output[j];
                        }
                    } // for (i = 1; i <= iterations; i++)
                } // if (srcImage.isColorImage())
                else { // not color image
                    ckr2Regular();
                    // output and input are upscaled if upscale > 1.
                    input = new float[output.length];
                    for (i = 0; i < output.length; i++) {
                        input[i] = output[i];
                    }
                    // Set upscaling to 1 so upscaling no longer occurs
                    // Otherwise you would upscale with each iteration
                    upscale = 1;
                    I = new boolean[horizontalGradient.length];
                    for (i = 0; i < I.length; i++) {
                        I[i] = true;
                    }
                    for (i = 1; i <= iterations; i++) {
                        fireProgressStateChanged((i - 1) * 100/iterations);
                        // Compute steering matrix
                        steering();
                        skr2Regular();
                        for (j = 0; j < output.length; j++) {
                            input[j] = output[j];
                        }
                    } // for (i = 1; i <= iterations; i++)
                } // else not color image
            } // if ((nDims == 2) || do25D)
        } // else if (method == ITERATIVE_STEERING_KERNEL_SECOND_ORDER)
        else if (method == STEERING_KERNEL_SECOND_ORDER_L1_NORM) {
            medianImage = (ModelImage)srcImage.clone();
            medianIters = 1; // Iterations of the median filter
            kernelSize = 3;  // Kernel size is 3 X 3.
            kernelShape = AlgorithmMedian.SQUARE_KERNEL;
            stdDev = 0.0f; // Inner-bounds by which to process pixels (pixel values outside this bound will be median
                           // filtered).
            adaptiveSize = false; // do not adaptively change size of the kernel mask
            maximumSize = 0; // If adaptiveSize is true, the maximum size the kernel mask can be increased to.
            wholeImage = true; // Median filtering will be performed on the whole image
            Preferences.debug("\nEntering AlgorithmMedian\n");
            if (nDims == 2) {
                algoMedian = new AlgorithmMedian(medianImage, srcImage, medianIters, kernelSize, kernelShape, stdDev, adaptiveSize,
                                                 maximumSize, wholeImage);
            }
            else {
                sliceBySlice = true;
                algoMedian = new AlgorithmMedian(medianImage, srcImage, medianIters, kernelSize, kernelShape, stdDev, adaptiveSize,
                        maximumSize, sliceBySlice, wholeImage);    
            }
            algoMedian.run();
            algoMedian.finalize();
            algoMedian = null;
            
            Preferences.debug("Entering ckr2allRegular\n");
            ckr2allRegular();
            zInit = new float[output.length];
            for (i = 0; i < output.length; i++) {
                zInit[i] = output[i];
            }
            Preferences.debug("Entering ckr2L1Regular\n");
            ckr2L1Regular();
            zInit = new float[output.length];
            for (i = 0; i < output.length; i++) {
                zInit[i] = output[i];
            }
            I = new boolean[horizontalGradient.length];
            for (i = 0; i < I.length; i++) {
                I[i] = true;
            }
            Preferences.debug("Entering steering\n");
            steering();
            Preferences.debug("Entering skr2L1Regular\n");
            skr2L1Regular();
        } // else if (method == STEERING_KERNEL_SECOND_ORDER_L1_NORM)
        
        try {
            if (srcImage.isColorImage()) {
                convertCIELabtoRGB(output);
            }
            if (destImage != null) {
                destImage.importData(0, output, true);
            }
            else {
                if (upscale != 1) {
                    srcImage.reallocate(srcImage.getType(), extents);
                }
                if (!srcImage.isColorImage()) {
                    switch(srcImage.getType()) {
                        case ModelStorageBase.BOOLEAN:
                            min = 0;
                            max = 1;
                            break;
                        case ModelStorageBase.BYTE:
                            min = -128;
                            max = 127;
                            break;
                        case ModelStorageBase.UBYTE:
                            min = 0;
                            max = 255;
                            break;
                        case ModelStorageBase.SHORT:
                            min = -32768;
                            max = 32767;
                            break;
                        case ModelStorageBase.USHORT:
                            min = 0;
                            max = 65535;
                            break;
                        case ModelStorageBase.INTEGER:
                            min = Integer.MIN_VALUE;
                            max = Integer.MAX_VALUE;
                            break;
                        case ModelStorageBase.UINTEGER:
                            min = 0;
                            max = 4294967295L;
                            break;
                        case ModelStorageBase.LONG:
                            min = Long.MIN_VALUE;
                            max = Long.MAX_VALUE;
                            break;
                        case ModelStorageBase.FLOAT:
                        case ModelStorageBase.DOUBLE:
                        default:
                            min = -Float.MAX_VALUE;
                            max = Float.MAX_VALUE;
                            break;
                            
                    }
                    for (i = 0; i < output.length; i++) {
                        if (output[i] < min) {
                            output[i] = min;
                        }
                        else if (output[i] > max) {
                            output[i] = max;
                        }
                    }
                } // if (!srcImage.isColorImage())
                srcImage.importData(0, output, true);
            }
        }
        catch (IOException e) {
            MipavUtil.displayError("IOException on importData");
            setCompleted(false);
            return;
        }
        
        fireProgressStateChanged(100);
        time = System.currentTimeMillis() - time;
        Preferences.debug("Seconds elapsed in AlgorithmKernelRegression = " + (time/1000.0) + "\n");
        setCompleted(true);
        return;
        
        
    }
    
    /*  2D processing on a second order classic kernel regression function for regularly sampled data */
    private void ckr2Regular() {
        int length;
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
        
        length = xDim * yDim;
        input = new float[length];
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
                        A[y][x][i-1][j-1] = (float)prod[y][x];
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
                            output[xx - 1 + extents[0] * (yy - 1) + extents[0]*extents[1]*z] = 0;
                            verticalGradient[xx - 1 + extents[0] * (yy - 1) + extents[0]*extents[1]*z] = 0;
                            horizontalGradient[xx - 1 + extents[0] * (yy - 1) + extents[0]*extents[1]*z] = 0;
                            for (k = 0; k < initialKernelSizeSquared; k++) {
                                output[xx - 1 + extents[0] * (yy - 1) + extents[0]*extents[1]*z] +=
                                A[0][k][i-1][j-1] * inputp[k];
                                verticalGradient[xx - 1 + extents[0] * (yy - 1) + extents[0]*extents[1]*z] +=
                                A[1][k][i-1][j-1] * inputp[k];
                                horizontalGradient[xx - 1 + extents[0] * (yy - 1) + extents[0]*extents[1]*z] +=
                                A[2][k][i-1][j-1] * inputp[k];
                            }
                        } // for (j = 1; j <= upscale; j++)
                    } // for (i = 1; i <= upscale; i++)
                } // for (x = 1; x <= xDim; x++)
            } // for (y = 1; y <= yDim; y++)
        } // for (z = 0; z < zDim; z++)
        
    } // ckr2Regular
    
    /** Second order classic kernel regression function with L1-norm for regularly sampled data */
    private void ckr2L1Regular() {
        int length;
        int radius;
        float x1[];
        float x2[];
        int x;
        int y;
        float xx1[];
        float xx2[];
        int i;
        int j;
        int iterativeKernelSizeSquared = iterativeKernelSize * iterativeKernelSize;
        double Xx[][];
        double W[];
        double escale = -0.5/(iterativeGlobalSmoothing * iterativeGlobalSmoothing);
        double XwT[][];
        int z;
        int padXDim;
        int padYDim;
        float input2[];
        float inputp[];
        int xp;
        int yp;
        int k;
        float b[];
        int it;
        float Xxb[];
        double XwT6[];
        byte sg[];
        int upscaleSquared = upscale * upscale;
        int upKernelSize;
        float start;
        float increment;
        int xx;
        int yy;
        int jj;
        
        length = xDim * yDim;
        input = new float[length];
        output = new float[6 * upscaleSquared * length * zDim];
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
        radius = (iterativeKernelSize - 1)/2;
        upKernelSize = upscale * iterativeKernelSize;
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
      
        xx1 = new float[iterativeKernelSizeSquared];
        xx2 = new float[iterativeKernelSizeSquared];
        W = new double[iterativeKernelSizeSquared];
        Xx = new double[iterativeKernelSizeSquared][6];
        XwT = new double[6][iterativeKernelSizeSquared];
        padXDim = xDim + 2 * radius;
        padYDim = yDim + 2 * radius;
        input2 = new float[padXDim * padYDim];
        inputp = new float[iterativeKernelSizeSquared];
        b = new float[6];
        Xxb = new float[iterativeKernelSizeSquared];
        XwT6 = new double[6];
        sg = new byte[iterativeKernelSizeSquared];
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
            for (i = 1; i <= upscale; i++) {
                for (j = 1; j <= upscale; j++) {
                    for (y = 0; y < iterativeKernelSize; y++) {
                        for (x = 0; x < iterativeKernelSize; x++) {
                            // Store in xx1 and xx2 as one long column formed from the columns of the
                            // 2D xx1 and xx2
                            xx1[x * iterativeKernelSize + y] = 
                            x1[upscale - j + x * upscale + upKernelSize * (upscale - i + y * upscale)];
                            xx2[x * iterativeKernelSize + y] =
                            x2[upscale - j + x * upscale + upKernelSize * (upscale - i + y * upscale)];
                        } // for (x = 0; x < iterativeKernelSize; x++)
                    } // for (y = 0; y < iterativeKernelSize; y++)
                    for (y = 0; y < iterativeKernelSizeSquared; y++) {
                        Xx[y][0] = 1.0;
                        Xx[y][1] = xx1[y];
                        Xx[y][2] = xx2[y];
                        Xx[y][3] = xx1[y] * xx1[y];
                        Xx[y][4] = xx1[y] * xx2[y];
                        Xx[y][5] = xx2[y] * xx2[y];
                        // The weight matrix
                        W[y] = Math.exp(escale * (xx1[y] * xx1[y] + xx2[y] * xx2[y]));
                        // Equivalent kernel
                        XwT[0][y] = W[y];
                        XwT[1][y] = xx1[y] * W[y];
                        XwT[2][y] = xx2[y] * W[y];
                        XwT[3][y] = Xx[y][3] * W[y];
                        XwT[4][y] = Xx[y][4] * W[y];
                        XwT[5][y] = Xx[y][5] * W[y];
                    } // for (y = 0; y < iterativeKernelSizeSquared; y++)
                    
                    for (y = 1; y <= yDim; y++) {
                        yy = (y - 1) * upscale + i;
                        for (x = 1; x <= xDim; x++) {
                            xx = (x - 1) * upscale + j;
                            for (yp = 0; yp < iterativeKernelSize; yp++) {
                                for (xp = 0; xp < iterativeKernelSize; xp++) {
                                    // Store inputp in one long column
                                    inputp[yp + xp * iterativeKernelSize] = input2[x - 1+ xp + padXDim * (y - 1 + yp)];
                                }
                            } 
                            
                            // Steepest descent iterations
                            for (k = 0; k < 6; k++) {
                                b[k] = zInit[6*(xx - 1 + extents[0] * (yy - 1) + extents[0]*extents[1]*z) + k];
                            }
                            
                            for (it = 1; it <= iterations; it++) {
                                for (jj = 0; jj < iterativeKernelSizeSquared; jj++) {
                                    Xxb[jj] = 0.0f;
                                    for (k = 0; k < 6; k++) {
                                        Xxb[jj] += Xx[jj][k] * b[k];    
                                    }
                                    if (inputp[jj] > Xxb[jj]) {
                                        sg[jj] = 1;
                                    }
                                    else if (inputp[jj] == Xxb[jj]) {
                                        sg[jj] = 0;
                                    }
                                    else {
                                        sg[jj] = -1;
                                    }
                                } // for (jj = 0; jj < iterativeKernelSizeSquared; jj++)
                                
                                for (jj = 0; jj < 6; jj++) {
                                    XwT6[jj] = 0;
                                    for (k = 0; k < iterativeKernelSizeSquared; k++) {
                                        XwT6[jj] += XwT[jj][k] * sg[k];
                                    }
                                    b[jj] += classicStepSize * XwT6[jj];
                                } // for (jj = 0; jj < 6; jj++)
                            } // for (it = 1; it <= iterations; it++)
                            for (k = 0; k < 6; k++) {
                                output[6*(xx - 1 + extents[0] * (yy - 1) + extents[0]*extents[1]*z) + k] = b[k];
                            }
                            verticalGradient[xx - 1  + extents[0] * (yy - 1) + extents[0] * extents[1] * z] = b[1];
                            horizontalGradient[xx - 1 + extents[0] * (yy - 1) + extents[0] * extents[1]* z] = b[2];
                        } // for (x = 1; x <= xDim; x++)
                    } // for (y = 1; y <= yDim; y++)
                } // for (j = 1; j <= upscale; j++)
            } // for (i = 1; i <= upscale; i++) 
        } // for (z = 0; z < zDim; z++)  
    } // ckr2L1Regular
    
    /** Second order classic kernel regression function for regularly sampled data */
    private void ckr2allRegular() {
        int length;
        int radius;
        float x1[];
        float x2[];
        int x;
        int y;
        float A[][][][];
        float xx1[];
        float xx2[];
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
        int k;
        int upscaleSquared = upscale * upscale;
        int upKernelSize;
        float start;
        float increment;
        int i;
        int j;
        int xx;
        int yy;
        
        length = xDim * yDim;
        input = new float[length];
        output = new float[6 * upscaleSquared * length * zDim];
        
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
                        A[y][x][i-1][j-1] = (float)prod[y][x];
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
                medianImage.exportData(z*length, length, input);
            }
            catch (IOException e) {
                MipavUtil.displayError("IOException on medianImage.exportData(z*length, length, input)");
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
                            output[6*(xx - 1 + extents[0] * (yy - 1) + extents[0]*extents[1]*z)] = 0;
                            output[6*(xx - 1 + extents[0] * (yy - 1) + extents[0]*extents[1]*z) + 1] = 0;
                            output[6*(xx - 1 + extents[0] * (yy - 1) + extents[0]*extents[1]*z) + 2] = 0;
                            output[6*(xx - 1 + extents[0] * (yy - 1) + extents[0]*extents[1]*z) + 3] = 0;
                            output[6*(xx - 1 + extents[0] * (yy - 1) + extents[0]*extents[1]*z) + 4] = 0;
                            output[6*(xx - 1 + extents[0] * (yy - 1) + extents[0]*extents[1]*z) + 5] = 0;
                            for (k = 0; k < initialKernelSizeSquared; k++) {
                                output[6*(xx - 1 + extents[0] * (yy - 1) + extents[0]*extents[1]*z)] += 
                                A[0][k][i-1][j-1] * inputp[k]; // z(x)
                                output[6*(xx - 1 + extents[0] * (yy - 1) + extents[0]*extents[1]*z) + 1] += 
                                A[1][k][i-1][j-1] * inputp[k]; // z_x1(x)
                                output[6*(xx - 1 + extents[0] * (yy - 1) + extents[0]*extents[1]*z) + 2] += 
                                A[2][k][i-1][j-1] * inputp[k]; // z_x2(x)
                                output[6*(xx - 1 + extents[0] * (yy - 1) + extents[0]*extents[1]*z) + 3] += 
                                A[3][k][i-1][j-1] * inputp[k]; // z_x1x1(x)
                                output[6*(xx - 1 + extents[0] * (yy - 1) + extents[0]*extents[1]*z) + 4] += 
                                A[4][k][i-1][j-1] * inputp[k]; // z_x1x2(x)
                                output[6*(xx - 1 + extents[0] * (yy - 1) + extents[0]*extents[1]*z) + 5] += 
                                A[5][k][i-1][j-1] * inputp[k]; // z_x2x2(x)
                            }
                        } // for (j = 1; j <= upscale; j++)
                    } // for (i = 1; i <= upscale; i++)     
                } // for (x = 1; x <= xDim; x++)
            } // for (y = 1; y <= yDim; y++)
        } // for (z = 0; z < zDim; z++)    
    } // ckr2allRegular
    
    /*  2D processing on a second order classic kernel regression function for regularly sampled  CIELab data */
    private void ckr2RegularCIELab() {
        int length;
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
        int c;
        
        length = xDim * yDim;
        output = new float[4 * upscaleSquared * length * zDim];
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
                        A[y][x][i-1][j-1] = (float)prod[y][x];
                    }
                }
            } // for (j = 1; j <= upscale; j++)
        } // for (i = 1; i <= upscale; i++)
        
        padXDim = xDim + 2 * radius;
        padYDim = yDim + 2 * radius;
        input2 = new float[padXDim * padYDim];
        inputp = new float[initialKernelSizeSquared];
        for (c = 1; c <= 3; c++) {
            for (z = 0; z < zDim; z++) {
                
                // Mirror the input image
                for (y = 0; y < yDim; y++) {
                    for (x = 0; x < xDim; x++) {
                        input2[x + radius + padXDim*(y + radius)] = input[4*(x + y * xDim + z * length) + c];
                    }
                }
                
                for (y = 0; y < yDim; y++) {
                    for (x = 0; x < radius; x++) {
                        // left side mirror reflection
                        input2[x + padXDim*(y + radius)] = input[4*((radius - x) + xDim * y + z * length) + c];
                        // right side mirror reflection
                        input2[x + xDim + radius + padXDim * (y + radius)] =
                        input[4 *(xDim - 2 - x + xDim * y + z * length) + c];
                    }
                }
                for (y = 0; y < radius; y++) {
                    for (x = 0; x < xDim; x++) {
                        // top side mirror reflection
                        input2[x + radius + padXDim * y] = input[4*(x + xDim * (radius - y) + z * length) + c];
                        // bottom side mirror reflection
                        input2[x + radius + padXDim * (y + yDim + radius)] =
                        input[4*(x + xDim * (yDim - 2 - y) + z * length) + c];
                    }
                }
                for (y = 0; y < radius; y++) {
                    for (x = 0; x < radius; x++) {
                        // left top mirror reflection
                        input2[x + padXDim * y] = input[4*((radius - x) + xDim * (radius - y) + z * length) + c];
                        // left bottom mirror reflection
                        input2[x + padXDim * (y + yDim + radius)] = 
                        input[4*((radius - x) + xDim * (yDim - 2 - y) + z * length) + c];
                        // right top mirror reflection
                        input2[x + xDim + radius + padXDim * y] =
                        input[4*(xDim - 2 - x + xDim * (radius - y) + z * length) + c];
                        // right bottom mirror reflection
                        input2[x + xDim + radius + padXDim * (y + yDim + radius)] =
                        input[4*(xDim - 2 - x + xDim * (yDim - 2 - y) + z * length) + c];
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
                                output[4*(xx - 1 + extents[0] * (yy - 1) + extents[0]*extents[1]*z) + c] = 0;
                                if (c == 1) {
                                    verticalGradient[xx - 1 + extents[0] * (yy - 1) + extents[0]*extents[1]*z] = 0;
                                    horizontalGradient[xx - 1 + extents[0] * (yy - 1) + extents[0]*extents[1]*z] = 0;
                                }
                                for (k = 0; k < initialKernelSizeSquared; k++) {
                                    output[4*(xx - 1 + extents[0] * (yy - 1) + extents[0]*extents[1]*z) + c] +=
                                    A[0][k][i-1][j-1] * inputp[k];
                                    if (c == 1) {
                                        verticalGradient[xx - 1 + extents[0] * (yy - 1) + extents[0]*extents[1]*z] +=
                                        A[1][k][i-1][j-1] * inputp[k];
                                        horizontalGradient[xx - 1 + extents[0] * (yy - 1) + extents[0]*extents[1]*z] +=
                                        A[2][k][i-1][j-1] * inputp[k];
                                    }
                                }
                            } // for (j = 1; j <= upscale; j++)
                        } // for (i = 1; i <= upscale; i++)
                    } // for (x = 1; x <= xDim; x++)
                } // for (y = 1; y <= yDim; y++)
            } // for (z = 0; z < zDim; z++)
        } // for (c = 1; c <= 3; c++)
        
    } // ckr2RegularCIELab
    
    private void steering() {
        int win;
        float K[];
        int y;
        int x;
        int winSquared;
        int delX;
        int delY;
        int delYSquared;
        float zx[];
        float zy[];
        int z;
        int padXDim;
        int padYDim;
        int zPos;
        float gx[];
        float gy[];
        int i;
        int j;
        double G[][];
        int len;
        Matrix matG;
        SingularValueDecomposition SVD;
        double s[][];
        double v[][];
        double S1;
        double S2;
        double v1[][] = new double[2][2];
        double v2[][] = new double[2][2];
        double con;
        
        C = new float[2][2][extents[1]][extents[0]][zDim];
        
        if ((windowSize % 2) == 0) {
            windowSize++;
        }
        win = (windowSize - 1)/2;
        winSquared = win * win;
        // Create a 2D circular averaging filter (pillbox) within the square matrix of size 2*win +1 = windowSize
        K = new float[windowSize * windowSize];
        for (y = 0; y < windowSize; y++) {
            delY = y - win;
            delYSquared = delY * delY;
            for (x = 0; x < windowSize; x++) {
                delX = x - win; 
                if ((delX * delX + delYSquared) <= winSquared) {
                    K[x + windowSize * y] = 1.0f;
                }
            }
        }
        
        padXDim = extents[0] + 2 * win;
        padYDim = extents[1] + 2 * win;
        zx = new float[padXDim * padYDim];
        zy = new float[padXDim * padYDim];
        gx = new float[windowSize * windowSize];
        gy = new float[windowSize * windowSize];
        G = new double[windowSize * windowSize][2];
        for (z = 0; z < zDim; z++) {
            // Mirror the horizontal and vertical gradients
            zPos = z * extents[0] * extents[1];
            for (y = 0; y < extents[1]; y++) {
                for (x = 0; x < extents[0]; x++) {
                    zx[x + win + padXDim*(y + win)] = 
                    horizontalGradient[x + y * extents[0] + zPos];
                    zy[x + win + padXDim*(y + win)] = 
                    verticalGradient[x + y * extents[0] + zPos];
                }
            }
            
            for (y = 0; y < extents[1]; y++) {
                for (x = 0; x < win; x++) {
                    // left side mirror reflection
                    zx[x + padXDim*(y + win)] =
                    horizontalGradient[(win - x) + extents[0] * y + zPos];
                    zy[x + padXDim*(y + win)] =
                    verticalGradient[(win - x) + extents[0] * y + zPos];
                    // right side mirror reflection
                    zx[x + extents[0] + win + padXDim * (y + win)] =
                    horizontalGradient[extents[0] - 2 - x + extents[0] * y + zPos];
                    zy[x + extents[0] + win + padXDim * (y + win)] =
                    verticalGradient[extents[0] - 2 - x + extents[0] * y + zPos];
                }
            }
            for (y = 0; y < win; y++) {
                for (x = 0; x < extents[0]; x++) {
                    // top side mirror reflection
                    zx[x + win + padXDim * y] = 
                    horizontalGradient[x + extents[0] * (win - y) + zPos];
                    zy[x + win + padXDim * y] = 
                    verticalGradient[x + extents[0] * (win - y) + zPos];
                    // bottom side mirror reflection
                    zx[x + win + padXDim * (y + extents[1] + win)] =
                    horizontalGradient[x + extents[0] * (extents[1] - 2 - y) + zPos];
                    zy[x + win + padXDim * (y + extents[1] + win)] =
                    verticalGradient[x + extents[0] * (extents[1] - 2 - y) + zPos];
                }
            }
            for (y = 0; y < win; y++) {
                for (x = 0; x < win; x++) {
                    // left top mirror reflection
                    zx[x + padXDim * y] = 
                    horizontalGradient[(win - x) + extents[0] * (win - y) + zPos];
                    zy[x + padXDim * y] = 
                    verticalGradient[(win - x) + extents[0] * (win - y) + zPos];
                    // left bottom mirror reflection
                    zx[x + padXDim * (y + extents[1] + win)] = 
                    horizontalGradient[(win - x) + extents[0] * (extents[1] - 2 - y) + zPos];
                    zy[x + padXDim * (y + extents[1] + win)] = 
                    verticalGradient[(win - x) + extents[0] * (extents[1] - 2 - y) + zPos];
                    // right top mirror reflection
                    zx[x + extents[0] + win + padXDim * y] =
                    horizontalGradient[extents[0] - 2 - x + extents[0] * (win - y) + zPos];
                    zy[x + extents[0] + win + padXDim * y] =
                    verticalGradient[extents[0] - 2 - x + extents[0] * (win - y) + zPos];
                    // right bottom mirror reflection
                    zx[x + extents[0] + win + padXDim * (y + extents[1] + win)] =
                    horizontalGradient[extents[0] - 2 - x + extents[0] * (extents[1] - 2 - y) + zPos];
                    zy[x + extents[0] + win + padXDim * (y + extents[1] + win)] =
                    verticalGradient[extents[0] - 2 - x + extents[0] * (extents[1] - 2 - y) + zPos];
                }
            } 
            
            for (y = 1; y <= extents[1]; y++) {
                for (x = 1; x <= extents[0]; x++) {
                    if (!I[x - 1 + extents[0] * (y - 1)]) {
                        continue;
                    }
                    
                    // Put all the columns of gx and gy into a single column
                    for (i = 0; i < windowSize; i++) {
                        for (j = 0; j < windowSize; j++) {
                            gx[i + j * windowSize] = zx[x - 1 + j + padXDim * (y - 1 + i)] *
                                                      K[j + i * windowSize];
                            gy[i + j * windowSize] = zy[x - 1 + j + padXDim * (y - 1 + i)] *
                                                      K[j + i * windowSize];
                        }
                    }
                    
                    for (i = 0; i < windowSize*windowSize; i++) {
                        G[i][0] = gx[i];
                        G[i][1] = gy[i];
                    }
                    
                    len = 0;
                    for (i = 0; i < K.length; i++) {
                        len += K[i];
                    }
                    matG = new Matrix(G);
                    SVD = matG.svd();
                    s = SVD.getS().getArray();
                    v = SVD.getV().getArray();
                    S1 = (s[0][0] + lambda)/(s[1][1] + lambda);
                    S2 = (s[1][1] + lambda)/(s[0][0] + lambda);
                    con = Math.pow(((s[0][0] * s[1][1] + 1.0E-7)/len), alpha);
                    v1[0][0] = S1*v[0][0]*v[0][0];
                    v1[0][1] = S1*v[0][0]*v[1][0];
                    v1[1][0] = S1*v[1][0]*v[0][0];
                    v1[1][1] = S1*v[1][0]*v[1][0];
                    v2[0][0] = S2*v[0][1]*v[0][1];
                    v2[0][1] = S2*v[0][1]*v[1][1];
                    v2[1][0] = S2*v[1][1]*v[0][1];
                    v2[1][1] = S2*v[1][1]*v[1][1];
                    for (j = 0; j < 2; j++) {
                        for (i = 0; i < 2; i++) {
                            C[j][i][y-1][x-1][z] = (float)((v1[j][i] + v2[j][i])*con);   
                        }
                    }
                    
                } // for (x = 1; x <= extents[0]; x++)
            } // for (y = 1; y <= extents[1]; y++)
        } // for (z = 0; z < zDim; z++)
    } // steering

    /*  2D processing on a second order classic kernel regression function for regularly sampled data */
    private void skr2Regular() {
        int length;
        int radius;
        float x1[];
        float x2[];
        int x;
        int y;
        double C11[];
        double C12[];
        double C22[];
        double CC[][];
        Matrix matCC;
        double sqrtDetC[];
        float inputM[];
        double C11M[];
        double C12M[];
        double C22M[];
        double sqrtDetCM[];
        double A[][];
        float x1Col[];
        float x2Col[];
        int i;
        int iterativeKernelSizeSquared = iterativeKernelSize * iterativeKernelSize;
        double Xx[][];
        double tt[];
        double W[];
        double escale = -0.5/(iterativeGlobalSmoothing * iterativeGlobalSmoothing);
        double Xw[][];
        Matrix matXx;
        Matrix matXw;
        double prod[][];
        Matrix matProd;
        int z;
        int padXDim;
        int padYDim;
        float inputp[];
        int xp;
        int yp;
        int k;
        
        length = extents[0] * extents[1];
        output = new float[length * zDim];
        horizontalGradient = new float[length * zDim];
        verticalGradient = new float[length * zDim];
        
        // Create the equivalent kernels
        radius = (iterativeKernelSize - 1)/2;
        x1 = new float[iterativeKernelSizeSquared];
        x2 = new float[iterativeKernelSizeSquared];
        x1Col = new float[iterativeKernelSizeSquared];
        x2Col = new float[iterativeKernelSizeSquared];
        for (y = 0; y < iterativeKernelSize; y++) {
            for (x = 0; x < iterativeKernelSize; x++) {
                x2[x + y * iterativeKernelSize] = -radius + x;
                x2Col[y + x * iterativeKernelSize] = -radius + x;
                x1[x + y * iterativeKernelSize] = -radius + y;
                x1Col[y + x * iterativeKernelSize] = -radius + y;
            }
        }
        
        C11 = new double[length];
        C12 = new double[length];
        C22 = new double[length];
        CC = new double[2][2];
        sqrtDetC = new double[length];
        padXDim = extents[0] + 2 * radius;
        padYDim = extents[1] + 2 * radius;
        inputM = new float[padXDim * padYDim];
        C11M = new double[padXDim * padYDim];
        C12M = new double[padXDim * padYDim];
        C22M = new double[padXDim * padYDim];
        sqrtDetCM = new double[padXDim * padYDim];
        inputp = new float[iterativeKernelSizeSquared];
        tt = new double[iterativeKernelSizeSquared];
        W = new double[iterativeKernelSizeSquared];
        Xw = new double[iterativeKernelSizeSquared][6];
        A = new double[6][iterativeKernelSizeSquared];
        for (z = 0; z < zDim; z++) {
            for (y = 0; y < extents[1]; y++) {
                for (x = 0; x < extents[0]; x++) {
                    // precalculation for covariance matrices
                    C11[x + y * extents[0]] = C[0][0][y][x][z];
                    C12[x + y * extents[0]] = C[0][1][y][x][z];
                    C22[x + y * extents[0]] = C[1][1][y][x][z];
                    CC[0][0] = C[0][0][y][x][z];
                    CC[0][1] = C[0][1][y][x][z];
                    CC[1][0] = C[1][0][y][x][z];
                    CC[1][1] = C[1][1][y][x][z];
                    matCC = new Matrix(CC);
                    sqrtDetC[x + y * extents[0]] = Math.sqrt(matCC.det());
                } // for (x = 0; x < extents[0]; x++)
            } // for (y = 0; y < extents[1]; y++)
            
            // Mirroring
            for (y = 0; y < extents[1]; y++) {
                for (x = 0; x < extents[0]; x++) {
                    inputM[x + radius + padXDim*(y + radius)] = input[x + y * extents[0] + z * length];
                    C11M[x + radius + padXDim*(y + radius)] = C11[x + y * extents[0]];
                    C12M[x + radius + padXDim*(y + radius)] = C12[x + y * extents[0]];
                    C22M[x + radius + padXDim*(y + radius)] = C22[x + y * extents[0]];
                    sqrtDetCM[x + radius + padXDim*(y + radius)] = sqrtDetC[x + y * extents[0]];
                }
            }
            
            for (y = 0; y < extents[1]; y++) {
                for (x = 0; x < radius; x++) {
                    // left side mirror reflection
                    inputM[x + padXDim*(y + radius)] = input[(radius - x) + extents[0] * y + z * length];
                    C11M[x + padXDim*(y + radius)] = C11[(radius - x) + extents[0] * y];
                    C12M[x + padXDim*(y + radius)] = C12[(radius - x) + extents[0] * y];
                    C22M[x + padXDim*(y + radius)] = C22[(radius - x) + extents[0] * y];
                    sqrtDetCM[x + padXDim*(y + radius)] = sqrtDetC[(radius - x) + extents[0] * y];
                    // right side mirror reflection
                    inputM[x + extents[0] + radius + padXDim * (y + radius)] =
                    input[extents[0] - 2 - x + extents[0] * y + z * length];
                    C11M[x + extents[0] + radius + padXDim * (y + radius)] =
                    C11[extents[0] - 2 - x + extents[0] * y];
                    C12M[x + extents[0] + radius + padXDim * (y + radius)] =
                    C12[extents[0] - 2 - x + extents[0] * y];
                    C22M[x + extents[0] + radius + padXDim * (y + radius)] =
                    C22[extents[0] - 2 - x + extents[0] * y];
                    sqrtDetCM[x + extents[0] + radius + padXDim * (y + radius)] =
                    sqrtDetC[extents[0] - 2 - x + extents[0] * y];
                }
            }
            for (y = 0; y < radius; y++) {
                for (x = 0; x < extents[0]; x++) {
                    // top side mirror reflection
                    inputM[x + radius + padXDim * y] = input[x + extents[0] * (radius - y) + z * length];
                    C11M[x + radius + padXDim * y] = C11[x + extents[0] * (radius - y)];
                    C12M[x + radius + padXDim * y] = C12[x + extents[0] * (radius - y)];
                    C22M[x + radius + padXDim * y] = C22[x + extents[0] * (radius - y)];
                    sqrtDetCM[x + radius + padXDim * y] = sqrtDetC[x + extents[0] * (radius - y)];
                    // bottom side mirror reflection
                    inputM[x + radius + padXDim * (y + extents[1] + radius)] =
                    input[x + extents[0] * (extents[1] - 2 - y) + z * length];
                    C11M[x + radius + padXDim * (y + extents[1] + radius)] =
                    C11[x + extents[0] * (extents[1] - 2 - y)];
                    C12M[x + radius + padXDim * (y + extents[1] + radius)] =
                    C12[x + extents[0] * (extents[1] - 2 - y)];
                    C22M[x + radius + padXDim * (y + extents[1] + radius)] =
                    C22[x + extents[0] * (extents[1] - 2 - y)];
                    sqrtDetCM[x + radius + padXDim * (y + extents[1] + radius)] =
                    sqrtDetC[x + extents[0] * (extents[1] - 2 - y)];
                }
            }
            for (y = 0; y < radius; y++) {
                for (x = 0; x < radius; x++) {
                    // left top mirror reflection
                    inputM[x + padXDim * y] = input[(radius - x) + extents[0] * (radius - y) + z * length];
                    C11M[x + padXDim * y] = C11[(radius - x) + extents[0] * (radius - y)];
                    C12M[x + padXDim * y] = C12[(radius - x) + extents[0] * (radius - y)];
                    C22M[x + padXDim * y] = C22[(radius - x) + extents[0] * (radius - y)];
                    sqrtDetCM[x + padXDim * y] = sqrtDetC[(radius - x) + extents[0] * (radius - y)];
                    // left bottom mirror reflection
                    inputM[x + padXDim * (y + extents[1] + radius)] = 
                    input[(radius - x) + extents[0] * (extents[1] - 2 - y) + z * length];
                    C11M[x + padXDim * (y + extents[1] + radius)] = 
                    C11[(radius - x) + extents[0] * (extents[1] - 2 - y)];
                    C12M[x + padXDim * (y + extents[1] + radius)] = 
                    C12[(radius - x) + extents[0] * (extents[1] - 2 - y)];
                    C22M[x + padXDim * (y + extents[1] + radius)] = 
                    C22[(radius - x) + extents[0] * (extents[1] - 2 - y)];
                    sqrtDetCM[x + padXDim * (y + extents[1] + radius)] = 
                    sqrtDetC[(radius - x) + extents[0] * (extents[1] - 2 - y)];
                    // right top mirror reflection
                    inputM[x + extents[0] + radius + padXDim * y] =
                    input[extents[0] - 2 - x + extents[0] * (radius - y) + z * length];
                    C11M[x + extents[0] + radius + padXDim * y] =
                    C11[extents[0] - 2 - x + extents[0] * (radius - y)];
                    C12M[x + extents[0] + radius + padXDim * y] =
                    C12[extents[0] - 2 - x + extents[0] * (radius - y)];
                    C22M[x + extents[0] + radius + padXDim * y] =
                    C22[extents[0] - 2 - x + extents[0] * (radius - y)];
                    sqrtDetCM[x + extents[0] + radius + padXDim * y] =
                    sqrtDetC[extents[0] - 2 - x + extents[0] * (radius - y)];
                    // right bottom mirror reflection
                    inputM[x + extents[0] + radius + padXDim * (y + extents[1] + radius)] =
                    input[extents[0] - 2 - x + extents[0] * (extents[1] - 2 - y) + z * length];
                    C11M[x + extents[0] + radius + padXDim * (y + extents[1] + radius)] =
                    C11[extents[0] - 2 - x + extents[0] * (extents[1] - 2 - y)];
                    C12M[x + extents[0] + radius + padXDim * (y + extents[1] + radius)] =
                    C12[extents[0] - 2 - x + extents[0] * (extents[1] - 2 - y)];
                    C22M[x + extents[0] + radius + padXDim * (y + extents[1] + radius)] =
                    C22[extents[0] - 2 - x + extents[0] * (extents[1] - 2 - y)];
                    sqrtDetCM[x + extents[0] + radius + padXDim * (y + extents[1] + radius)] =
                    sqrtDetC[extents[0] - 2 - x + extents[0] * (extents[1] - 2 - y)];
                }
            }
            
            Xx = new double[iterativeKernelSizeSquared][6];
            // Estimate an image and its first gradients
            for (i = 0; i < iterativeKernelSizeSquared; i++) {
                Xx[i][0] = 1.0;
                Xx[i][1] = x1Col[i];
                Xx[i][2] = x2Col[i];
                Xx[i][3] = x1Col[i] * x1Col[i];
                Xx[i][4] = x1Col[i] * x2Col[i];
                Xx[i][5] = x2Col[i] * x2Col[i];
            }
            
            for (y = 0; y < extents[1]; y++) {
                for (x = 0; x < extents[0]; x++) {
                    for (yp = 0; yp < iterativeKernelSize; yp++) {
                        for (xp = 0; xp < iterativeKernelSize; xp++) {
                            // Store inputp in one long column
                            inputp[yp + xp * iterativeKernelSize] = inputM[x + xp + padXDim * (y + yp)];
                            tt[xp + yp * iterativeKernelSize] = x1[xp + yp * iterativeKernelSize] *
                            (C11M[x + xp + padXDim * (y + yp)] * x1[xp + yp * iterativeKernelSize] +
                             C12M[x + xp + padXDim * (y + yp)] * x2[xp + yp * iterativeKernelSize]) +
                            x2[xp + yp * iterativeKernelSize] * 
                            (C12M[x + xp + padXDim * (y + yp)] * x1[xp + yp * iterativeKernelSize] +
                             C22M[x + xp + padXDim * (y + yp)] * x2[xp + yp * iterativeKernelSize]);
                            // Store W in one long column
                            W[yp + xp * iterativeKernelSize] = Math.exp(escale * tt[xp + yp * iterativeKernelSize]) *
                            sqrtDetCM[x + xp + padXDim * (y + yp)];
                        }
                    }
                    
                    for (i = 0; i < iterativeKernelSizeSquared; i++) {
                        // Equivalent kernel
                        Xw[i][0] = W[i];
                        Xw[i][1] = x1Col[i] * W[i];
                        Xw[i][2] = x2Col[i] * W[i];
                        Xw[i][3] = Xx[i][3] * W[i];
                        Xw[i][4] = Xx[i][4] * W[i];
                        Xw[i][5] = Xx[i][5] * W[i];
                    } // for (i = 0; i < iterativeKernelSizeSquared; i++)
                    matXx = new Matrix(Xx);
                    matXw = new Matrix(Xw);
                    prod = ((matXx.transpose()).times(matXw)).getArray();
                    for (i = 0; i < 6; i++) {
                        prod[i][i] += 1.0E-7;
                    }
                    matProd = new Matrix(prod);
                    A = ((matProd.inverse()).times(matXw.transpose())).getArray();
                    
                    output[x + y * extents[0] + z * length] = 0;
                    verticalGradient[x + y * extents[0] + z * length] = 0;
                    horizontalGradient[x + y * extents[0] + z * length] = 0;
                    for (k = 0; k < iterativeKernelSizeSquared; k++) {
                        output[x + y * extents[0] + z * length] += A[0][k] * inputp[k];
                        verticalGradient[x + y * extents[0] + z * length] += A[1][k] * inputp[k];
                        horizontalGradient[x + y * extents[0] + z * length] += A[2][k] * inputp[k];    
                    }
                } // for (x = 0; x < extents[0]; x++)
            } // for (y = 0; y < extents[1]; y++)
        } // for (z = 0; z < zDim; z++)
        
    } // skr2Regular
    
    /*  2D processing on a second order classic kernel regression function with L1 norm for regularly sampled data */
    private void skr2L1Regular() {
        int length;
        int radius;
        float x1[];
        float x2[];
        int x;
        int y;
        double C11[];
        double C12[];
        double C22[];
        double CC[][];
        Matrix matCC;
        double sqrtDetC[];
        float inputM[];
        double C11M[];
        double C12M[];
        double C22M[];
        double sqrtDetCM[];
        int i;
        int j;
        int iterativeKernelSizeSquared = iterativeKernelSize * iterativeKernelSize;
        double Xx[][];
        double tt[];
        double W[];
        double escale = -0.5/(iterativeGlobalSmoothing2 * iterativeGlobalSmoothing2);
        int z;
        int padXDim;
        int padYDim;
        float inputp[];
        int xp;
        int yp;
        int k;
        int upKernelSize;
        int upscaleSquared = upscale * upscale;
        float start;
        float increment;
        float xx1[];
        float xx2[];
        float xx1Col[];
        float xx2Col[];
        int xx;
        int yy;
        double XwT[][];
        float b[];
        float Xxb[];
        double XwT6[];
        byte sg[];
        int it;
        int jj;
        
        length = xDim * yDim;
        input = new float[length];
        output = new float[upscaleSquared * length * zDim];
        
        
        // Create the equivalent kernels
        radius = (iterativeKernelSize - 1)/2;
        upKernelSize = upscale * iterativeKernelSize;
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
        
        C11 = new double[length];
        C12 = new double[length];
        C22 = new double[length];
        CC = new double[2][2];
        sqrtDetC = new double[length];
        padXDim = xDim + 2 * radius;
        padYDim = yDim + 2 * radius;
        inputM = new float[padXDim * padYDim];
        C11M = new double[padXDim * padYDim];
        C12M = new double[padXDim * padYDim];
        C22M = new double[padXDim * padYDim];
        sqrtDetCM = new double[padXDim * padYDim];
        inputp = new float[iterativeKernelSizeSquared];
        tt = new double[iterativeKernelSizeSquared];
        W = new double[iterativeKernelSizeSquared];
        Xx = new double[iterativeKernelSizeSquared][6];
        xx1 = new float[iterativeKernelSizeSquared];
        xx2 = new float[iterativeKernelSizeSquared];
        xx1Col = new float[iterativeKernelSizeSquared];
        xx2Col = new float[iterativeKernelSizeSquared];
        XwT = new double[6][iterativeKernelSizeSquared];
        b = new float[6];
        Xxb = new float[iterativeKernelSizeSquared];
        XwT6 = new double[6];
        sg = new byte[iterativeKernelSizeSquared];
        for (z = 0; z < zDim; z++) {
            try {
                srcImage.exportData(z*length, length, input);
            }
            catch (IOException e) {
                MipavUtil.displayError("IOException on srcImage.exportData(z*length, length, input)");
                setCompleted(false);
                return;
            }
            for (y = 0; y < yDim; y++) {
                for (x = 0; x < xDim; x++) {
                    // precalculation for covariance matrices
                    C11[x + y * xDim] = C[0][0][y][x][z];
                    C12[x + y * xDim] = C[0][1][y][x][z];
                    C22[x + y * xDim] = C[1][1][y][x][z];
                    CC[0][0] = C[0][0][y][x][z];
                    CC[0][1] = C[0][1][y][x][z];
                    CC[1][0] = C[1][0][y][x][z];
                    CC[1][1] = C[1][1][y][x][z];
                    matCC = new Matrix(CC);
                    sqrtDetC[x + y * xDim] = Math.sqrt(matCC.det());
                } // for (x = 0; x < extents[0]; x++)
            } // for (y = 0; y < extents[1]; y++)
            
            // Mirroring
            for (y = 0; y < yDim; y++) {
                for (x = 0; x < xDim; x++) {
                    inputM[x + radius + padXDim*(y + radius)] = input[x + y * xDim + z * length];
                    C11M[x + radius + padXDim*(y + radius)] = C11[x + y * xDim];
                    C12M[x + radius + padXDim*(y + radius)] = C12[x + y * xDim];
                    C22M[x + radius + padXDim*(y + radius)] = C22[x + y * xDim];
                    sqrtDetCM[x + radius + padXDim*(y + radius)] = sqrtDetC[x + y * xDim];
                }
            }
            
            for (y = 0; y < yDim; y++) {
                for (x = 0; x < radius; x++) {
                    // left side mirror reflection
                    inputM[x + padXDim*(y + radius)] = input[(radius - x) + xDim * y + z * length];
                    C11M[x + padXDim*(y + radius)] = C11[(radius - x) + xDim * y];
                    C12M[x + padXDim*(y + radius)] = C12[(radius - x) + xDim * y];
                    C22M[x + padXDim*(y + radius)] = C22[(radius - x) + xDim * y];
                    sqrtDetCM[x + padXDim*(y + radius)] = sqrtDetC[(radius - x) + xDim * y];
                    // right side mirror reflection
                    inputM[x + xDim + radius + padXDim * (y + radius)] =
                    input[xDim - 2 - x + xDim * y + z * length];
                    C11M[x + xDim + radius + padXDim * (y + radius)] =
                    C11[xDim - 2 - x + xDim * y];
                    C12M[x + xDim + radius + padXDim * (y + radius)] =
                    C12[xDim - 2 - x + xDim * y];
                    C22M[x + xDim + radius + padXDim * (y + radius)] =
                    C22[xDim - 2 - x + xDim * y];
                    sqrtDetCM[x + xDim + radius + padXDim * (y + radius)] =
                    sqrtDetC[xDim - 2 - x + xDim * y];
                }
            }
            for (y = 0; y < radius; y++) {
                for (x = 0; x < xDim; x++) {
                    // top side mirror reflection
                    inputM[x + radius + padXDim * y] = input[x + xDim * (radius - y) + z * length];
                    C11M[x + radius + padXDim * y] = C11[x + xDim * (radius - y)];
                    C12M[x + radius + padXDim * y] = C12[x + xDim * (radius - y)];
                    C22M[x + radius + padXDim * y] = C22[x + xDim * (radius - y)];
                    sqrtDetCM[x + radius + padXDim * y] = sqrtDetC[x + xDim * (radius - y)];
                    // bottom side mirror reflection
                    inputM[x + radius + padXDim * (y + yDim + radius)] =
                    input[x + xDim * (yDim - 2 - y) + z * length];
                    C11M[x + radius + padXDim * (y + yDim + radius)] =
                    C11[x + xDim * (yDim - 2 - y)];
                    C12M[x + radius + padXDim * (y + yDim + radius)] =
                    C12[x + xDim * (yDim - 2 - y)];
                    C22M[x + radius + padXDim * (y + yDim + radius)] =
                    C22[x + xDim * (yDim - 2 - y)];
                    sqrtDetCM[x + radius + padXDim * (y + yDim + radius)] =
                    sqrtDetC[x + xDim * (yDim - 2 - y)];
                }
            }
            for (y = 0; y < radius; y++) {
                for (x = 0; x < radius; x++) {
                    // left top mirror reflection
                    inputM[x + padXDim * y] = input[(radius - x) + xDim * (radius - y) + z * length];
                    C11M[x + padXDim * y] = C11[(radius - x) + xDim * (radius - y)];
                    C12M[x + padXDim * y] = C12[(radius - x) + xDim * (radius - y)];
                    C22M[x + padXDim * y] = C22[(radius - x) + xDim * (radius - y)];
                    sqrtDetCM[x + padXDim * y] = sqrtDetC[(radius - x) + xDim * (radius - y)];
                    // left bottom mirror reflection
                    inputM[x + padXDim * (y + yDim + radius)] = 
                    input[(radius - x) + xDim * (yDim - 2 - y) + z * length];
                    C11M[x + padXDim * (y + yDim + radius)] = 
                    C11[(radius - x) + xDim * (yDim - 2 - y)];
                    C12M[x + padXDim * (y + yDim + radius)] = 
                    C12[(radius - x) + xDim * (yDim - 2 - y)];
                    C22M[x + padXDim * (y + yDim + radius)] = 
                    C22[(radius - x) + xDim * (yDim - 2 - y)];
                    sqrtDetCM[x + padXDim * (y + yDim + radius)] = 
                    sqrtDetC[(radius - x) + xDim * (yDim - 2 - y)];
                    // right top mirror reflection
                    inputM[x + xDim + radius + padXDim * y] =
                    input[xDim - 2 - x + xDim * (radius - y) + z * length];
                    C11M[x + xDim + radius + padXDim * y] =
                    C11[xDim - 2 - x + xDim * (radius - y)];
                    C12M[x + xDim + radius + padXDim * y] =
                    C12[xDim - 2 - x + xDim * (radius - y)];
                    C22M[x + xDim + radius + padXDim * y] =
                    C22[xDim - 2 - x + xDim * (radius - y)];
                    sqrtDetCM[x + xDim + radius + padXDim * y] =
                    sqrtDetC[xDim - 2 - x + xDim * (radius - y)];
                    // right bottom mirror reflection
                    inputM[x + xDim + radius + padXDim * (y + yDim + radius)] =
                    input[xDim - 2 - x + xDim * (yDim - 2 - y) + z * length];
                    C11M[x + xDim + radius + padXDim * (y + yDim + radius)] =
                    C11[xDim - 2 - x + xDim * (yDim - 2 - y)];
                    C12M[x + xDim + radius + padXDim * (y + yDim + radius)] =
                    C12[xDim - 2 - x + xDim * (yDim - 2 - y)];
                    C22M[x + xDim + radius + padXDim * (y + yDim + radius)] =
                    C22[xDim - 2 - x + xDim * (yDim - 2 - y)];
                    sqrtDetCM[x + xDim + radius + padXDim * (y + yDim + radius)] =
                    sqrtDetC[xDim - 2 - x + xDim * (yDim - 2 - y)];
                }
            }
            
            
            // Estimate an image and its first gradients
            for (i = 1; i <= upscale; i++) {
                for (j = 1; j <= upscale; j++) {
                    for (y = 0; y < iterativeKernelSize; y++) {
                        for (x = 0; x < iterativeKernelSize; x++) {
                            xx1Col[x * iterativeKernelSize + y] = 
                            x1[upscale - j + x * upscale + upKernelSize * (upscale - i + y * upscale)];
                            xx1[y * iterativeKernelSize + x] = 
                                x1[upscale - j + x * upscale + upKernelSize * (upscale - i + y * upscale)];
                            xx2Col[x * iterativeKernelSize + y] =
                            x2[upscale - j + x * upscale + upKernelSize * (upscale - i + y * upscale)];
                            xx2[y * iterativeKernelSize + x] =
                                x2[upscale - j + x * upscale + upKernelSize * (upscale - i + y * upscale)];
                        } // for (x = 0; x < iterativeKernelSize; x++)
                    } // for (y = 0; y < iterativeKernelSize; y++)
                    for (y = 0; y < iterativeKernelSizeSquared; y++) {
                        Xx[y][0] = 1.0;
                        Xx[y][1] = xx1Col[y];
                        Xx[y][2] = xx2Col[y];
                        Xx[y][3] = xx1Col[y] * xx1Col[y];
                        Xx[y][4] = xx1Col[y] * xx2Col[y];
                        Xx[y][5] = xx2Col[y] * xx2Col[y];
                    } // for (y = 0; y < initialKernelSizeSquared; y++)
                    for (y = 1; y <= yDim; y++) {
                        yy = (y - 1) * upscale + i;
                        for (x = 1; x <= xDim; x++) {
                            xx = (x - 1) * upscale + j;
                            for (yp = 0; yp < iterativeKernelSize; yp++) {
                                for (xp = 0; xp < iterativeKernelSize; xp++) {
                                    // Store inputp in one long column
                                    inputp[yp + xp * iterativeKernelSize] = inputM[x - 1 + xp + padXDim * (y - 1 + yp)];
                                    tt[xp + yp * iterativeKernelSize] = xx1[xp + yp * iterativeKernelSize] *
                                    (C11M[x - 1 + xp + padXDim * (y - 1 + yp)] * xx1[xp + yp * iterativeKernelSize] +
                                     C12M[x - 1 + xp + padXDim * (y - 1 + yp)] * xx2[xp + yp * iterativeKernelSize]) +
                                    xx2[xp + yp * iterativeKernelSize] * 
                                    (C12M[x - 1 + xp + padXDim * (y - 1 + yp)] * xx1[xp + yp * iterativeKernelSize] +
                                     C22M[x - 1 + xp + padXDim * (y - 1 + yp)] * xx2[xp + yp * iterativeKernelSize]);
                                    // Store W in one long column
                                    W[yp + xp * iterativeKernelSize] = Math.exp(escale * tt[xp + yp * iterativeKernelSize]) *
                                    sqrtDetCM[x - 1 + xp + padXDim * (y - 1 + yp)];
                                } // for (xp = 0; xp < iterativeKernelSize; xp++)
                            } // for (yp = 0; yp < iterativeKernelSize; yp++)
                            
                            for (k = 0; k < iterativeKernelSizeSquared; k++) {
                                XwT[0][k] = W[k];
                                XwT[1][k] = Xx[k][1] * W[k];
                                XwT[2][k] = Xx[k][2] * W[k];
                                XwT[3][k] = Xx[k][3] * W[k];
                                XwT[4][k] = Xx[k][4] * W[k];
                                XwT[5][k] = Xx[k][5] * W[k];
                            } // for (k = 0; k < iterativeKernelSizeSquared; k++)
                            
                            // Steepest descent iterations
                            for (k = 0; k < 6; k++) {
                                b[k] = zInit[6*(xx - 1 + extents[0] * (yy - 1) + extents[0]*extents[1]*z) + k];
                            }
                            
                            for (it = 1; it <= iterations2; it++) {
                                for (jj = 0; jj < iterativeKernelSizeSquared; jj++) {
                                    Xxb[jj] = 0.0f;
                                    for (k = 0; k < 6; k++) {
                                        Xxb[jj] += Xx[jj][k] * b[k];    
                                    }
                                    if (inputp[jj] > Xxb[jj]) {
                                        sg[jj] = 1;
                                    }
                                    else if (inputp[jj] == Xxb[jj]) {
                                        sg[jj] = 0;
                                    }
                                    else {
                                        sg[jj] = -1;
                                    }
                                } // for (jj = 0; jj < iterativeKernelSizeSquared; jj++)
                                
                                for (jj = 0; jj < 6; jj++) {
                                    XwT6[jj] = 0;
                                    for (k = 0; k < iterativeKernelSizeSquared; k++) {
                                        XwT6[jj] += XwT[jj][k] * sg[k];
                                    }
                                    b[jj] += steeringStepSize * XwT6[jj];
                                } // for (jj = 0; jj < 6; j++)
                            } // for (it = 1; it <= iterations2; it++)
                            output[(xx - 1) + extents[0] * (yy - 1) + upscaleSquared * length * z] = b[0];
                            //for (k = 0; k < 6; k++) {
                                //output[6*(x + xDim * y + length*z) + k] = b[k];
                            //}
                        } // for (x = 1; x <= xDim; x++)
                    } // for (y = 1; y <= yDim; y++)
                } // for (j = 1; j <= upscale; j++)
            } // for (i = 1; i <= upscale; i++)
        } // for (z = 0; z < zDim; z++)  
            
            
    } // skr2L1Regular
    
    /*  2D processing on a second order classic kernel regression function for regularly sampled CIELab data */
    private void skr2RegularCIELab() {
        int length;
        int radius;
        float x1[];
        float x2[];
        int x;
        int y;
        double C11[];
        double C12[];
        double C22[];
        double CC[][];
        Matrix matCC;
        double sqrtDetC[];
        float inputM[];
        double C11M[];
        double C12M[];
        double C22M[];
        double sqrtDetCM[];
        double A[][];
        float x1Col[];
        float x2Col[];
        int i;
        int iterativeKernelSizeSquared = iterativeKernelSize * iterativeKernelSize;
        double Xx[][];
        double tt[];
        double W[];
        double escale = -0.5/(iterativeGlobalSmoothing * iterativeGlobalSmoothing);
        double Xw[][];
        Matrix matXx;
        Matrix matXw;
        double prod[][];
        Matrix matProd;
        int z;
        int padXDim;
        int padYDim;
        float inputp[];
        int xp;
        int yp;
        int k;
        int c;
        
        length = extents[0] * extents[1];
        output = new float[4 * length * zDim];
        horizontalGradient = new float[length * zDim];
        verticalGradient = new float[length * zDim];
        
        // Create the equivalent kernels
        radius = (iterativeKernelSize - 1)/2;
        x1 = new float[iterativeKernelSizeSquared];
        x2 = new float[iterativeKernelSizeSquared];
        x1Col = new float[iterativeKernelSizeSquared];
        x2Col = new float[iterativeKernelSizeSquared];
        for (y = 0; y < iterativeKernelSize; y++) {
            for (x = 0; x < iterativeKernelSize; x++) {
                x2[x + y * iterativeKernelSize] = -radius + x;
                x2Col[y + x * iterativeKernelSize] = -radius + x;
                x1[x + y * iterativeKernelSize] = -radius + y;
                x1Col[y + x * iterativeKernelSize] = -radius + y;
            }
        }
        
        C11 = new double[length];
        C12 = new double[length];
        C22 = new double[length];
        CC = new double[2][2];
        sqrtDetC = new double[length];
        padXDim = extents[0] + 2 * radius;
        padYDim = extents[1] + 2 * radius;
        inputM = new float[padXDim * padYDim];
        C11M = new double[padXDim * padYDim];
        C12M = new double[padXDim * padYDim];
        C22M = new double[padXDim * padYDim];
        sqrtDetCM = new double[padXDim * padYDim];
        inputp = new float[iterativeKernelSizeSquared];
        tt = new double[iterativeKernelSizeSquared];
        W = new double[iterativeKernelSizeSquared];
        Xw = new double[iterativeKernelSizeSquared][6];
        A = new double[6][iterativeKernelSizeSquared];
        Xx = new double[iterativeKernelSizeSquared][6];
        for (c = 1; c <= 3; c++) {
            for (z = 0; z < zDim; z++) {
                for (y = 0; y < extents[1]; y++) {
                    for (x = 0; x < extents[0]; x++) {
                        // precalculation for covariance matrices
                        C11[x + y * extents[0]] = C[0][0][y][x][z];
                        C12[x + y * extents[0]] = C[0][1][y][x][z];
                        C22[x + y * extents[0]] = C[1][1][y][x][z];
                        CC[0][0] = C[0][0][y][x][z];
                        CC[0][1] = C[0][1][y][x][z];
                        CC[1][0] = C[1][0][y][x][z];
                        CC[1][1] = C[1][1][y][x][z];
                        matCC = new Matrix(CC);
                        sqrtDetC[x + y * extents[0]] = Math.sqrt(matCC.det());
                    } // for (x = 0; x < extents[0]; x++)
                } // for (y = 0; y < extents[1]; y++)
                
                // Mirroring
                for (y = 0; y < extents[1]; y++) {
                    for (x = 0; x < extents[0]; x++) {
                        inputM[x + radius + padXDim*(y + radius)] = input[4*(x + y * extents[0] + z * length) + c];
                        C11M[x + radius + padXDim*(y + radius)] = C11[x + y * extents[0]];
                        C12M[x + radius + padXDim*(y + radius)] = C12[x + y * extents[0]];
                        C22M[x + radius + padXDim*(y + radius)] = C22[x + y * extents[0]];
                        sqrtDetCM[x + radius + padXDim*(y + radius)] = sqrtDetC[x + y * extents[0]];
                    }
                }
                
                for (y = 0; y < extents[1]; y++) {
                    for (x = 0; x < radius; x++) {
                        // left side mirror reflection
                        inputM[x + padXDim*(y + radius)] = input[4*((radius - x) + extents[0] * z * length) + c];
                        C11M[x + padXDim*(y + radius)] = C11[(radius - x) + extents[0] * y];
                        C12M[x + padXDim*(y + radius)] = C12[(radius - x) + extents[0] * y];
                        C22M[x + padXDim*(y + radius)] = C22[(radius - x) + extents[0] * y];
                        sqrtDetCM[x + padXDim*(y + radius)] = sqrtDetC[(radius - x) + extents[0] * y];
                        // right side mirror reflection
                        inputM[x + extents[0] + radius + padXDim * (y + radius)] =
                        input[4*(extents[0] - 2 - x + extents[0] * y + z * length) + c];
                        C11M[x + extents[0] + radius + padXDim * (y + radius)] =
                        C11[extents[0] - 2 - x + extents[0] * y];
                        C12M[x + extents[0] + radius + padXDim * (y + radius)] =
                        C12[extents[0] - 2 - x + extents[0] * y];
                        C22M[x + extents[0] + radius + padXDim * (y + radius)] =
                        C22[extents[0] - 2 - x + extents[0] * y];
                        sqrtDetCM[x + extents[0] + radius + padXDim * (y + radius)] =
                        sqrtDetC[extents[0] - 2 - x + extents[0] * y];
                    }
                }
                for (y = 0; y < radius; y++) {
                    for (x = 0; x < extents[0]; x++) {
                        // top side mirror reflection
                        inputM[x + radius + padXDim * y] = input[4*(x + extents[0] * (radius - y) + z * length) + c];
                        C11M[x + radius + padXDim * y] = C11[x + extents[0] * (radius - y)];
                        C12M[x + radius + padXDim * y] = C12[x + extents[0] * (radius - y)];
                        C22M[x + radius + padXDim * y] = C22[x + extents[0] * (radius - y)];
                        sqrtDetCM[x + radius + padXDim * y] = sqrtDetC[x + extents[0] * (radius - y)];
                        // bottom side mirror reflection
                        inputM[x + radius + padXDim * (y + extents[1] + radius)] =
                        input[4*(x + extents[0] * (extents[1] - 2 - y) + z * length) + c];
                        C11M[x + radius + padXDim * (y + extents[1] + radius)] =
                        C11[x + extents[0] * (extents[1] - 2 - y)];
                        C12M[x + radius + padXDim * (y + extents[1] + radius)] =
                        C12[x + extents[0] * (extents[1] - 2 - y)];
                        C22M[x + radius + padXDim * (y + extents[1] + radius)] =
                        C22[x + extents[0] * (extents[1] - 2 - y)];
                        sqrtDetCM[x + radius + padXDim * (y + extents[1] + radius)] =
                        sqrtDetC[x + extents[0] * (extents[1] - 2 - y)];
                    }
                }
                for (y = 0; y < radius; y++) {
                    for (x = 0; x < radius; x++) {
                        // left top mirror reflection
                        inputM[x + padXDim * y] = input[4*((radius - x) + extents[0] * (radius - y) + z * length) + c];
                        C11M[x + padXDim * y] = C11[(radius - x) + extents[0] * (radius - y)];
                        C12M[x + padXDim * y] = C12[(radius - x) + extents[0] * (radius - y)];
                        C22M[x + padXDim * y] = C22[(radius - x) + extents[0] * (radius - y)];
                        sqrtDetCM[x + padXDim * y] = sqrtDetC[(radius - x) + extents[0] * (radius - y)];
                        // left bottom mirror reflection
                        inputM[x + padXDim * (y + extents[1] + radius)] = 
                        input[4*((radius - x) + extents[0] * (extents[1] - 2 - y) + z * length) + c];
                        C11M[x + padXDim * (y + extents[1] + radius)] = 
                        C11[(radius - x) + extents[0] * (extents[1] - 2 - y)];
                        C12M[x + padXDim * (y + extents[1] + radius)] = 
                        C12[(radius - x) + extents[0] * (extents[1] - 2 - y)];
                        C22M[x + padXDim * (y + extents[1] + radius)] = 
                        C22[(radius - x) + extents[0] * (extents[1] - 2 - y)];
                        sqrtDetCM[x + padXDim * (y + extents[1] + radius)] = 
                        sqrtDetC[(radius - x) + extents[0] * (extents[1] - 2 - y)];
                        // right top mirror reflection
                        inputM[x + extents[0] + radius + padXDim * y] =
                        input[4*(extents[0] - 2 - x + extents[0] * (radius - y) + z * length) + c];
                        C11M[x + extents[0] + radius + padXDim * y] =
                        C11[extents[0] - 2 - x + extents[0] * (radius - y)];
                        C12M[x + extents[0] + radius + padXDim * y] =
                        C12[extents[0] - 2 - x + extents[0] * (radius - y)];
                        C22M[x + extents[0] + radius + padXDim * y] =
                        C22[extents[0] - 2 - x + extents[0] * (radius - y)];
                        sqrtDetCM[x + extents[0] + radius + padXDim * y] =
                        sqrtDetC[extents[0] - 2 - x + extents[0] * (radius - y)];
                        // right bottom mirror reflection
                        inputM[x + extents[0] + radius + padXDim * (y + extents[1] + radius)] =
                        input[4*(extents[0] - 2 - x + extents[0] * (extents[1] - 2 - y) + z * length) + c];
                        C11M[x + extents[0] + radius + padXDim * (y + extents[1] + radius)] =
                        C11[extents[0] - 2 - x + extents[0] * (extents[1] - 2 - y)];
                        C12M[x + extents[0] + radius + padXDim * (y + extents[1] + radius)] =
                        C12[extents[0] - 2 - x + extents[0] * (extents[1] - 2 - y)];
                        C22M[x + extents[0] + radius + padXDim * (y + extents[1] + radius)] =
                        C22[extents[0] - 2 - x + extents[0] * (extents[1] - 2 - y)];
                        sqrtDetCM[x + extents[0] + radius + padXDim * (y + extents[1] + radius)] =
                        sqrtDetC[extents[0] - 2 - x + extents[0] * (extents[1] - 2 - y)];
                    }
                }
                
                // Estimate an image and its first gradients
                for (i = 0; i < iterativeKernelSizeSquared; i++) {
                    Xx[i][0] = 1.0;
                    Xx[i][1] = x1Col[i];
                    Xx[i][2] = x2Col[i];
                    Xx[i][3] = x1Col[i] * x1Col[i];
                    Xx[i][4] = x1Col[i] * x2Col[i];
                    Xx[i][5] = x2Col[i] * x2Col[i];
                }
                
                for (y = 0; y < extents[1]; y++) {
                    for (x = 0; x < extents[0]; x++) {
                        for (yp = 0; yp < iterativeKernelSize; yp++) {
                            for (xp = 0; xp < iterativeKernelSize; xp++) {
                                // Store inputp in one long column
                                inputp[yp + xp * iterativeKernelSize] = inputM[x + xp + padXDim * (y + yp)];
                                tt[xp + yp * iterativeKernelSize] = x1[xp + yp * iterativeKernelSize] *
                                (C11M[x + xp + padXDim * (y + yp)] * x1[xp + yp * iterativeKernelSize] +
                                 C12M[x + xp + padXDim * (y + yp)] * x2[xp + yp * iterativeKernelSize]) +
                                x2[xp + yp * iterativeKernelSize] * 
                                (C12M[x + xp + padXDim * (y + yp)] * x1[xp + yp * iterativeKernelSize] +
                                 C22M[x + xp + padXDim * (y + yp)] * x2[xp + yp * iterativeKernelSize]);
                                // Store W in one long column
                                W[yp + xp * iterativeKernelSize] = Math.exp(escale * tt[xp + yp * iterativeKernelSize]) *
                                sqrtDetCM[x + xp + padXDim * (y + yp)];
                            }
                        }
                        
                        for (i = 0; i < iterativeKernelSizeSquared; i++) {
                            // Equivalent kernel
                            Xw[i][0] = W[i];
                            Xw[i][1] = x1Col[i] * W[i];
                            Xw[i][2] = x2Col[i] * W[i];
                            Xw[i][3] = Xx[i][3] * W[i];
                            Xw[i][4] = Xx[i][4] * W[i];
                            Xw[i][5] = Xx[i][5] * W[i];
                        } // for (i = 0; i < iterativeKernelSizeSquared; i++)
                        matXx = new Matrix(Xx);
                        matXw = new Matrix(Xw);
                        prod = ((matXx.transpose()).times(matXw)).getArray();
                        for (i = 0; i < 6; i++) {
                            prod[i][i] += 1.0E-7;
                        }
                        matProd = new Matrix(prod);
                        A = ((matProd.inverse()).times(matXw.transpose())).getArray();
                        
                        output[4*(x + y * extents[0] + z * length) + c] = 0;
                        if (c == 1) {
                            verticalGradient[x + y * extents[0] + z * length] = 0;
                            horizontalGradient[x + y * extents[0] + z * length] = 0;
                        }
                        for (k = 0; k < iterativeKernelSizeSquared; k++) {
                            output[4*(x + y * extents[0] + z * length) + c] += A[0][k] * inputp[k];
                            if (c == 1) {
                                verticalGradient[x + y * extents[0] + z * length] += A[1][k] * inputp[k];
                                horizontalGradient[x + y * extents[0] + z * length] += A[2][k] * inputp[k];
                            }
                        }
                    } // for (x = 0; x < extents[0]; x++)
                } // for (y = 0; y < extents[1]; y++)
            } // for (z = 0; z < zDim; z++)
        } // for (c = 1; c <= 3; c++)
        
    } // skr2RegularCIELab
    
    private void convertRGBtoCIELab() {
        // Observer = 2 degrees, Illuminant = D65
        double XN = 95.047;
        double YN = 100.000;
        double ZN = 108.883;
        int i;
        double varR, varG, varB;
        double X, Y, Z;
        double varX, varY, varZ;
        float L, a, b;
        int length = 4 * xDim * yDim * zDim;
        
        input = new float[length];
        
        try {
            srcImage.exportData(0, length, input); // locks and releases lock
        } catch (IOException error) {
            displayError("Algorithm Kernel Regression: Image(s) locked");
            setCompleted(false);
            fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);
            srcImage.releaseLock();

            return;
        }
        for (i = 0; i < input.length; i += 4) {
            varR = input[i+1]/scaleMax;
            varG = input[i+2]/scaleMax;
            varB = input[i+3]/scaleMax;
            
            if (varR <= 0.04045) {
                varR = varR/12.92;
            }
            else {
                varR = Math.pow((varR + 0.055)/1.055, 2.4);
            }
            if (varG <= 0.04045) {
                varG = varG/12.92;
            }
            else {
                varG = Math.pow((varG + 0.055)/1.055, 2.4);
            }
            if (varB <= 0.04045) {
                varB = varB/12.92;
            }
            else {
                varB = Math.pow((varB + 0.055)/1.055, 2.4);
            }
            
            varR = 100.0 * varR;
            varG = 100.0 * varG;
            varB = 100.0 * varB;
            
            // Observer = 2 degrees, Illuminant = D65
            X = 0.4124*varR + 0.3576*varG + 0.1805*varB;
            Y = 0.2126*varR + 0.7152*varG + 0.0722*varB;
            Z = 0.0193*varR + 0.1192*varG + 0.9505*varB;
            
            varX = X/ XN;
            varY = Y/ YN;
            varZ = Z/ ZN;
            
            if (varX > 0.008856) {
                varX = Math.pow(varX, 1.0/3.0);
            }
            else {
                varX = (7.787 * varX) + (16.0/116.0);
            }
            if (varY > 0.008856) {
                varY = Math.pow(varY, 1.0/3.0);
            }
            else {
                varY = (7.787 * varY) + (16.0/116.0);
            }
            if (varZ > 0.008856) {
                varZ = Math.pow(varZ, 1.0/3.0);
            }
            else {
                varZ = (7.787 * varZ) + (16.0/116.0);
            }
            
            L = (float)((116.0 * varY) - 16.0);
            a = (float)(500.0 * (varX - varY));
            b = (float)(200.0 * (varY - varZ));
            
            input[i+1] = L;
            input[i+2] = a;
            input[i+3] = b;
        } // for (i = 0; i < buffer.length; i += 4)   
    } // private void convertRGBtoCIELab()
    
    private void convertCIELabtoRGB(float buffer[]) {
        // Observer = 2 degrees, Illuminant = D65
        double XN = 95.047;
        double YN = 100.000;
        double ZN = 108.883;
        int i;
        double varX, varY, varZ;
        double L, a, b;
        double varX3, varY3, varZ3;
        double X, Y, Z;
        double varR, varG, varB;
        double R, G, B;
        for (i = 0; i < buffer.length; i += 4) {
            L = (double)buffer[i+1];
            a = (double)buffer[i+2];
            b = (double)buffer[i+3];
            
            varY = (L + 16.0)/116.0;
            varX = a/500.0 + varY;
            varZ = varY - b/200.0;
            
            varX3 = Math.pow(varX, 3.0);
            if (varX3 > 0.008856) {
                varX = varX3;
            }
            else {
                varX = (varX - 16.0/116.0)/7.787;
            }
            varY3 = Math.pow(varY, 3.0);
            if (varY3 > 0.008856) {
                varY = varY3;
            }
            else {
                varY = (varY - 16.0/116.0)/7.787;
            }
            varZ3 = Math.pow(varZ, 3.0);
            if (varZ3 > 0.008856) {
                varZ = varZ3;
            }
            else {
                varZ = (varZ - 16.0/116.0)/7.787;
            }
            
            X = XN * varX;
            Y = YN * varY;
            Z = ZN * varZ;
            
            varX = X / 100.0;
            varY = Y / 100.0;
            varZ = Z / 100.0;
            
            varR = 3.2406 * varX - 1.5372 * varY - 0.4986 * varZ;
            varG = -0.9689 * varX + 1.8758 * varY + 0.0415 * varZ;
            varB = 0.0557 * varX - 0.2040 * varY + 1.0570 * varZ;
            
            if (varR > 0.0031308) {
                varR = 1.055 * (Math.pow(varR, 1.0/2.4)) - 0.055;
            }
            else {
                varR = 12.92 * varR;
            }
            if (varG > 0.0031308) {
                varG = 1.055 * (Math.pow(varG, 1.0/2.4)) - 0.055;
            }
            else {
                varG = 12.92 * varG;
            }
            if (varB > 0.0031308) {
                varB = 1.055 * (Math.pow(varB, 1.0/2.4)) - 0.055;
            }
            else {
                varB = 12.92 * varB;
            }
            
            R = scaleMax * varR;
            if (R < 0) {
                R = 0;
            }
            else if (R > scaleMax) {
                R = scaleMax;
            }
            G = scaleMax * varG;
            if (G < 0) {
                G = 0;
            }
            else if (G > scaleMax) {
                G = scaleMax;
            }
            B = scaleMax * varB;
            if (B < 0) {
                B = 0;
            }
            else if (B > scaleMax) {
                B = scaleMax;
            }
            
            buffer[i+1] = (float)R;
            buffer[i+2] = (float)G;
            buffer[i+3] = (float)B;
        } // for (i = 0; i < buffer.length; i += 4)
    } // private void convertCIELabtoRGB(float buffer[])
}
