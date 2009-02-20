package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmConvolver;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.GenerateGaussian;
import gov.nih.mipav.model.structures.ModelImage;

import java.io.IOException;
import gov.nih.mipav.view.ViewJProgressBar;


/**
 * This code is ported from 2 files written by Prasun Choudhury and Jack Tumblin, Trilateral2003.h and 
 * Trilateral2003.cpp.  This code filters a 2D black and white image.
 * Trilateral filtering smooths an image or VOI region of the image towards a sharply bounded, piecewise linear
 * approximation with 2 bilateral stages and a min-max stack.  A bilateral stage has 2 Gaussians.
 * One is a Gaussian at a user defined spatial scale (sigma - standard deviation) and the second is a Gaussian at a
 * user defined intensity scale.  The spatial Gaussian weighs pixels with smaller spatial separations more heavily and 
 * the intensity Gaussian weighs pixels with similar intensities more heavily. 
 * --Quick overview:
        1) Compute the X and Y gradients of the input image 
            and store it in xGradient and yGradient.

        2) Compute the min-gradient stack and max-gradient stack from the 
            gradients computed in step 1; store result in minGradientStack 
            and maxGradientStack respectively,

        3) Bilaterally filter the gradients (xGradient and yGradient) of the 
            input image; store results in xSmoothGradient and ySmoothGradient.
            (equations 4 and 5, Section 3.1)

        4) Compute the adaptive neighborhood 'fTheta' from the min and max 
        stack of image gradients. (equation 10, Section 3.2)

        5) Tilt the filter kernel based on bilaterally smoothed image gradients
        (xSmoothGradient snd ySmoothGradient) and filter the detail signal. 
        Then we add filtered detail to the original signal 
        (equations 6, 7, 8, 9; Section 3.1). 
 * 
 * 
 * References:
 * 1.) "Trilateral Filter for HDR Imaes and 3D Meshes", Prasun Choudhury and Jack Tumblin,
 * Proc. Eurographics Symposium on Rendering, Per. H. Christensen and Daniel Cohen eds., pp. 186-196, 2003.
 * @version  0.1 February 20, 2009
 * @author   William Gandler
 * @see      GenerateGaussian
 * @see      AlgorithmConvolver
 */
public class AlgorithmTrilateralFilter extends AlgorithmBase implements AlgorithmInterface{

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Flag, if true, indicates that the whole image should be processed. If false on process the image over the mask
     * areas.
     */
    private boolean entireImage;
    
    /** Variance of the domain for the gradient and the detail filter */
    private float sigmaC;

    /** Storage location of the Gaussian kernel. */
    private float[] GaussData;

    /** Dimensionality of the kernel. */
    private int[] kExtents;

    /** Standard deviations of the gaussian used to calculate the kernels. */
    private float[] sigmas;
    
    /** units of intensity range; it is multiplied by the intensity range to create intensitySigma */
    private float intensityFraction;
    
    // intensityGaussianDenom = 2.0 * intensitySigma * intensitySigma
    private double intensityGaussianDenom;
    
    /* Assigned to srcImage if replace image, assigned to destImage if new image */
    private ModelImage targetImage = null;
    
    private ModelImage cieLabImage = null;
    
    private double imageMax;
    
    // Scale factor used in RGB-CIELab conversions.  255 for ARGB, could be higher for ARGB_USHORT.
    private double scaleMax = 255.0;
    private float sigmaCTheta;
    private float beta;
    private int filterSize;
    private int levX;
    private int levY;
    private int levelMax;
    private int xDim;
    private int yDim;
    private int sliceSize;
    private float minGradientStack[][][] = null;
    private float maxGradientStack[][][] = null;
    private float xGradient[] = null;
    private float yGradient[] = null;
    private float xSmoothGradient[] = null;
    private float ySmoothGradient[] = null;
    private float fTheta[] = null;
    private float srcBuffer[] = null;
    private float sigmaR;
    private float R;
    private float sigmaRTheta;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmTrilateralFilter object.
     *
     * @param  srcImg    the srcImge
     * @param  sigmaC    variance of the domain for the gradient and the detail filter
     * @param  maskFlag  the mask flag
     * @param  img25D    the 2.5D indicator
     */
    public AlgorithmTrilateralFilter(ModelImage srcImg, float sigmaC, boolean maskFlag, boolean img25D) {
        this(null, srcImg, sigmaC, maskFlag, img25D);
    }

    /**
     * Constructor
     *
     * @param  destImg   the destination image
     * @param  srcImg    the source image
     * @param  sigmaC    variance of the domain for the gradient and the detail filter
     * @param  maskFlag  the mask flag
     * @param  img25D    the 2.5D indicator
     */
    public AlgorithmTrilateralFilter(ModelImage destImg, ModelImage srcImg, float sigmaC, boolean maskFlag,
                                 boolean img25D) {
        super(destImg, srcImg);

        this.sigmaC = sigmaC;
        entireImage = maskFlag;
        image25D = img25D;

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        GaussData = null;
        destImage = null;
        srcImage = null;
        kExtents = null;
        sigmas = null;
        super.finalize();
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
        

        
        long startTime = System.nanoTime();
        fireProgressStateChanged(0, srcImage.getImageName(), "Trilateral filter on image ...");
        
        // Default internal parameters
        sigmaCTheta = sigmaC; // Variance of the Domain Filter, the only user set parameter
        beta = 0.15f;
        filterSize = (int)sigmaC;
        
        // Compute the image stack height
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        levX = (int)(Math.log(xDim)/Math.log(2.0));
        levY = (int)(Math.log(yDim)/Math.log(2.0));
        if (levX < levY) {
            levelMax = levX + 1;
        }
        else {
            levelMax = levY + 1;
        }
        
        try {
            srcImage.exportData(0, sliceSize, srcBuffer);
        }
        catch (IOException e) {
            errorCleanUp("IOException on srcImage.exportData(0, sliceSize, srcBuffer)", false);
            return;
        }
        
        // Allocate memory for the Min-Max Image Stack
        minGradientStack = new float[xDim][yDim][levelMax];
        maxGradientStack = new float[xDim][yDim][levelMax];
        
        // Allocate memory for the gradient vectors and the output image
        xGradient = new float[sliceSize];
        yGradient = new float[sliceSize];
        xSmoothGradient = new float[sliceSize];
        ySmoothGradient = new float[sliceSize];
        fTheta = new float[sliceSize];
        
        /**
         * Compute Gradients using Forward Difference (Step 1)
         * X gradient is stored in xGradient
         * Y gradient is stored in yGradient
         **/
        
        computeGradients(xGradient, yGradient);
        
        /**
         * Builds the Min-Max Image Stack consisting of Image Gradients (Step 2).
         * Also computes sigmaR, the range variance, (see equation 11 in the paper for further details)>
         * min and max gradients are stored in two separate stacks.
         **/
        
        sigmaR = buildMinMaxImageStack(xGradient, yGradient, minGradientStack, maxGradientStack, levelMax, beta);
        
        if (destImage == null) {
            targetImage = srcImage;
        }
        else {
            targetImage = destImage;
        }
        
        srcImage.calcMinMax();
        imageMax = (float)srcImage.getMax();
        
        setCompleted(true);
        System.out.println("Time consumed GB: " + (System.nanoTime()-startTime));
    }
    
    /**
     * Computes the X and Y gradients using forward difference
     * @param px  X gradient is stored in px
     * @param py  Y gradient is stored in py
     */
    private void computeGradients(float px[], float py[]) {
        int i;
        int j;
        int jS;
        int iE;
        float cval;
        float eval;
        float sval;
        float gE;
        float gS;
        int index;
        
        for (j = 0; j < yDim; j++) { // for each scanline
            jS = j + 1; // addres of south neighbor
            if (jS > yDim - 1) {
                jS = yDim - 1;
            }
            for (i = 0; i < xDim; i++) { // and each pixel on a scanline
                index = i + j * xDim;
                iE = i + 1; // address of east neighbor
                if (iE > xDim - 1) {
                    iE = xDim - 1;
                }
                cval = srcBuffer[index];
                eval = srcBuffer[iE + j * xDim];
                sval = srcBuffer[i + jS * xDim];
                gE = eval - cval; // gradient computation with forward difference
                gS = sval - cval;
                px[index] = gE; // copy the gradient values to px and py
                py[index] = gS;
            } // for (i = 0; i < xDim; i++)
        } // for (j = 0; j < yDim; j++)
    }
    
    /**
     * 
     * @param px X gradient of original image
     * @param py Y gradient of original image
     * @param pMinStack output min stack of image gradients
     * @param pMaxStack output max stack of image gradients
     * @param levelMax height of the image stack
     * @param beta user specified parameter used to compute the range variance
     * @return range variance (sigmaR), equation 11
     */
    private float buildMinMaxImageStack(float px[], float py[], float pMinStack[][][], float pMaxStack[][][], int levelMax,
                                        float beta) {
        int i;
        int j;
        int lev;
        int m;
        int n;
        float min;
        float max;
        float minGrad = Float.MAX_VALUE;
        float maxGrad = -Float.MAX_VALUE;
        float tmp;
        float tmpMin;
        float tmpMax;
        float rangeVariance;
        int index;
        
        for (lev = 0; lev < levelMax; lev++) {
            for (j = 0; j < yDim; j++) {
                for (i = 0; i < xDim; i++) {
                    if (lev == 0) { // stack level 0 is the magnitude of the gradients of the original image
                        index = i + j * xDim;
                        tmp = (float)Math.sqrt(px[index]*px[index] + py[index]*py[index]);
                        if (maxGrad < tmp) {
                            maxGrad = tmp;
                        }
                        if (minGrad > tmp) {
                            minGrad = tmp;
                        }
                        max = min = tmp;
                        pMinStack[i][j][0] = min;
                        pMaxStack[i][j][0] = max;
                    } // if (lev == 0)
                    else { // lev > 0 Gradients at each level of the image stack are computed form the level below
                        min = pMinStack[i][j][lev-1];
                        max = pMaxStack[i][j][lev-1];
                        
                        for (m = -1; m <= 1; m++) {
                            for (n = -1; n <= 1; n++) {
                                // Computes the minimum and maximum gradient value in the neighborhood
                                if ((i+m) >= 0 && (i+m) < xDim && (j+n) >= 0 && (j+n) < yDim) {
                                    tmpMin = pMinStack[i+m][j+n][lev-1];
                                    tmpMax = pMaxStack[i+m][j+n][lev-1];
                                    if (min > tmpMin) {
                                        min = tmpMin;
                                    }
                                    if (max < tmpMax) {
                                        max = tmpMax;
                                    }
                                } // if ((i+m) >= 0 && (i+m) < xDim && (j+n) >= 0 && (j+n) < yDim)
                            } // for (n = -1; n <= 1; n++)
                        } // for (m = -1; m <= 1; m++)
                        pMinStack[i][j][lev] = min;
                        pMaxStack[i][j][lev] = max;
                    } // else lev > 0
                } // for (i = 0; i < xDim; i++)
            } // for (j = 0; j < yDim; j++)
        } // for (lev = 0; lev < levelmax; lev++)
        
        // range variance is computed as a ratio of difference between max and min gradient
        rangeVariance = beta * (maxGrad - minGrad);
        return rangeVariance;
    }
    
    /**
     * Creates 2D Gaussian kernels for the blurring process. The kernel size is always odd and proportional (8X) to the
     * standard deviation of the Gaussian.
     */
    private void makeKernels2D() {
        int xkDim, ykDim;
        int[] derivOrder = new int[2];

        kExtents = new int[2];
        derivOrder[0] = 0;
        derivOrder[1] = 0;

        xkDim = Math.round(8 * sigmas[0]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        kExtents[0] = xkDim;

        ykDim = Math.round(8 * sigmas[1]);

        if ((ykDim % 2) == 0) {
            ykDim++;
        }

        kExtents[1] = ykDim;

        GaussData = new float[xkDim * ykDim];

        GenerateGaussian Gauss = new GenerateGaussian(GaussData, kExtents, sigmas, derivOrder);
        Gauss.calc(false);
        Gauss.finalize();
        Gauss = null;
    }

    /**
     * Creates 3D Gaussian kernels for the blurring process. The kernel size is always odd and proportional (8X) to the
     * standard deviation of the Gaussian.
     */
    private void makeKernels3D() {
        int xkDim, ykDim, zkDim;
        int[] derivOrder = new int[3];

        kExtents = new int[3];
        derivOrder[0] = 0;
        derivOrder[1] = 0;
        derivOrder[2] = 0;

        xkDim = Math.round(8 * sigmas[0]);
        // System.out.println("Sigma 0 = " + sigmas[0]);
        // System.out.println("Sigma 1 = " + sigmas[1]);
        // System.out.println("Sigma 2 = " + sigmas[2]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        kExtents[0] = xkDim;

        ykDim = Math.round(8 * sigmas[1]);

        if ((ykDim % 2) == 0) {
            ykDim++;
        }

        kExtents[1] = ykDim;

        zkDim = Math.round(8 * sigmas[2]);

        if ((zkDim % 2) == 0) {
            zkDim++;
        }

        kExtents[2] = zkDim;

        GaussData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gauss = new GenerateGaussian(GaussData, kExtents, sigmas, derivOrder);
        Gauss.calc(false);
        Gauss.finalize();
        Gauss = null;
    }

    public void algorithmPerformed(AlgorithmBase algorithm){
    	if(!algorithm.isCompleted()){
    		finalize();
    		return;
    	}
    	if (algorithm instanceof AlgorithmConvolver) {
			AlgorithmConvolver convolver = (AlgorithmConvolver) algorithm;
            if (srcImage.isColorImage()) {
                convertCIELabtoRGB(convolver.getOutputBuffer());
            }
			try {
				targetImage.importData(0, convolver.getOutputBuffer(), true);
			} catch (IOException error) {
				errorCleanUp("Algorithm Gaussian Blur: Image(s) locked",
						false);

				return;
			}
			this.setCompleted(true);
		}
    }
    
    
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
        int length = 4 * targetImage.getSliceSize();
        float buffer[] = new float[length];
        int t;
        int z;
        int tDim = 1;
        int zDim = 1;
        float minL = Float.MAX_VALUE;
        float maxL = -Float.MAX_VALUE;
        float mina = Float.MAX_VALUE;
        float maxa = -Float.MAX_VALUE;
        float minb = Float.MAX_VALUE;
        float maxb = -Float.MAX_VALUE;
        float LRange;
        float aRange;
        float bRange;
        double maxDistance;
        double intensitySigma;
        if (srcImage.getNDims() >= 3) {
            tDim = srcImage.getExtents()[2];
        }
        if (srcImage.getNDims() >= 4) {
            tDim = srcImage.getExtents()[3];
        }
        
        for (t = 0; (t < tDim) && !threadStopped; t++) {
            for (z = 0; (z < zDim) && !threadStopped; z++) {
                try {
                    srcImage.exportData((t * zDim + z) * length, length, buffer); // locks and releases lock
                } catch (IOException error) {
                    displayError("Algorithm Bilateral Filter: Image(s) locked");
                    setCompleted(false);
                    fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);
                    srcImage.releaseLock();

                    return;
                }
                for (i = 0; i < buffer.length; i += 4) {
                    varR = buffer[i+1]/scaleMax;
                    varG = buffer[i+2]/scaleMax;
                    varB = buffer[i+3]/scaleMax;
                    
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
                    
                    if (L < minL) {
                        minL = L;
                    }
                    if (L > maxL) {
                        maxL = L;
                    }
                    if (a < mina) {
                        mina = a;
                    }
                    if (a > maxa) {
                        maxa = a;
                    }
                    if (b < minb) {
                        minb = b;
                    }
                    if (b > maxb) {
                        maxb = b;
                    }
                    
                    buffer[i+1] = L;
                    buffer[i+2] = a;
                    buffer[i+3] = b;
                } // for (i = 0; i < buffer.length; i += 4)
                try {
                    cieLabImage.importData((t * zDim + z) * length, buffer, false);
                } catch (IOException error) {
                    errorCleanUp("Algorithm Bilateral Filter: Image(s) locked", false);

                    return;
                }
            } // for (z = 0; (z < zDim) && !threadStopped; z++)
        } // for (t = 0; (t < tDim) && !threadStopped; t++)
        LRange = maxL - minL;
        aRange = maxa - mina;
        bRange = maxb - minb;
        maxDistance = (float)Math.sqrt(LRange*LRange + aRange*aRange + bRange*bRange);
        intensitySigma = intensityFraction * maxDistance;
        intensityGaussianDenom = (2.0 * intensitySigma * intensitySigma);
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
            G = scaleMax * varG;
            B = scaleMax * varB;
            
            buffer[i+1] = (float)R;
            buffer[i+2] = (float)G;
            buffer[i+3] = (float)B;
        } // for (i = 0; i < buffer.length; i += 4)
    } // private void convertCIELabtoRGB(float buffer[])
}

