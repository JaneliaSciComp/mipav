package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.structures.ModelImage;

import java.io.IOException;


/**
 * This code is ported from 2 files written by Prasun Choudhury and Jack Tumblin, Trilateral2003.h and 
 * Trilateral2003.cpp.  This code filters a 2D black and white image and the lumninance component of a
 * 2D color image.
 * 
 * Trilateral filtering smooths an image towards a sharply bounded, piecewise linear
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
 * @version  0.1 February 23, 2009
 * @author   William Gandler
 */
public class AlgorithmTrilateralFilter extends AlgorithmBase implements AlgorithmInterface{

    //~ Instance fields -----------------------------------------------------------------------------------------------
    
    /** Standard deviation of the domain for the gradient and the detail filter */
    private float sigmaC;
    
    // Scale factor used in RGB-CIELab conversions.  255 for ARGB, could be higher for ARGB_USHORT.
    private double scaleMax = 255.0;
    private int xDim;
    private int yDim;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmTrilateralFilter object.
     *
     * @param  srcImg    the srcImge
     * @param  sigmaC    standard deviation of the domain for the gradient and the detail filter
     */
    public AlgorithmTrilateralFilter(ModelImage srcImg, float sigmaC) {
        this(null, srcImg, sigmaC);
    }

    /**
     * Constructor
     *
     * @param  destImg   the destination image
     * @param  srcImg    the source image
     * @param  sigmaC    standard deviation of the domain for the gradient and the detail filter
     */
    public AlgorithmTrilateralFilter(ModelImage destImg, ModelImage srcImg, float sigmaC) {
        super(destImg, srcImg);

        this.sigmaC = sigmaC;

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {
        int i;
        float sigmaCTheta;
        float beta;
        int filterSize;
        int levX;
        int levY;
        int levelMax;
        int sliceSize;
        float minGradientStack[][][] = null;
        float maxGradientStack[][][] = null;
        float xGradient[] = null;
        float yGradient[] = null;
        float xSmoothGradient[] = null;
        float ySmoothGradient[] = null;
        float fTheta[] = null;  // stores adaptive neighborhood size for each pixel
        float result[] = null;
        float srcBuffer[] = null;
        float cieBuffer[] = null;
        float sigmaR;
        float R;
        float sigmaRTheta;
        /* Assigned to srcImage if replace image, assigned to destImage if new image */
        ModelImage targetImage = null;
        double imageMax;

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
        

        
        long startTime = System.nanoTime();
        fireProgressStateChanged(0, srcImage.getImageName(), "Trilateral filter on image ...");
        
        // Default internal parameters
        sigmaCTheta = sigmaC; // Standard deviation of the Domain Filter, the only user set parameter
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
        
        srcImage.calcMinMax();
        imageMax = srcImage.getMax();
        srcBuffer = new float[sliceSize];
        if (srcImage.isColorImage()) {
            scaleMax = Math.max(255.0, imageMax);
            cieBuffer = new float[4 * sliceSize];
            try {
                srcImage.exportData(0, 4 * sliceSize, cieBuffer);
            }
            catch (IOException e) {
                errorCleanUp("IOException on srcImage.exportData(0, 4 * sliceSize, cieBuffer)", false);
                return;
            }
            // Put Lab in cieBuffer and L in srcBuffer
            convertRGBtoCIELab(cieBuffer, srcBuffer); 
        }
        else {
            try {
                srcImage.exportData(0, sliceSize, srcBuffer);
            }
            catch (IOException e) {
                errorCleanUp("IOException on srcImage.exportData(0, sliceSize, srcBuffer)", false);
                return;
            }
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
        result = new float[sliceSize];
        
        /**
         * Compute Gradients using Forward Difference (Step 1)
         * X gradient is stored in xGradient
         * Y gradient is stored in yGradient
         **/
        
        computeGradients(srcBuffer, xGradient, yGradient);
        
        /**
         * Builds the Min-Max Image Stack consisting of Image Gradients (Step 2).
         * Also computes sigmaR, the range standard deviation, (see equation 11 in the paper for further details)>
         * min and max gradients are stored in two separate stacks.
         **/
        
        sigmaR = buildMinMaxImageStack(xGradient, yGradient, minGradientStack, maxGradientStack, levelMax, beta);
        
        // Set the remaining internal parameters required for trilateral filter
        sigmaRTheta = R = sigmaR;
        
        /**
         * Bilaterally filter the X and Y gradients of the input image (Step 3, equation 4 and 5)
         * to produce xSmoothGradient and ySmoothGradient
         */
        
        bilateralGradientFilter(xGradient, yGradient, xSmoothGradient, ySmoothGradient, sigmaC, sigmaR, filterSize);
        
        /**
         * Find the adaptive neighborhood fTheta for each pixel location (Step 4).  fTheta size is 
         * given by stack level.  The min-max gradient stacks and range threshold "R" are used for this calculation.
         * (see equation 10 in paper for details).
         */
        
        findAdaptiveRegion(minGradientStack, maxGradientStack, R, levelMax, fTheta);
        
        /**
         * Performs bilateral filter on the detail signal (Step 5).
         * See equations 6, 7, 8, and 9.
         * Output is stored in destImage or srcImage (end result of equation 8, Section 3.1)
         */
        detailBilateralFilter(srcBuffer, xSmoothGradient, ySmoothGradient, fTheta, sigmaCTheta, sigmaRTheta, result);
        
        if (srcImage.isColorImage()) {
            for (i = 0; i < sliceSize; i++) {
                cieBuffer[4*i+1] = result[i];
            }
            convertCIELabtoRGB(cieBuffer);
        }
        
        if (destImage == null) {
            targetImage = srcImage;
        }
        else {                                                                                                                                                                             
            targetImage = destImage;
        }
        
        if (srcImage.isColorImage()) {
            try {
                targetImage.importData(0, cieBuffer, true);
            }
            catch (IOException e) {
                errorCleanUp("IOException on targetImage.importData(0, cieBuffer, true)", false);
                return;    
            }    
        }
        else {
            try {
                targetImage.importData(0, result, true);
            }
            catch (IOException e) {
                errorCleanUp("IOException on targetImage.importData(0, result, true)", false);
                return;    
            }
        }
        
        fireProgressStateChanged(100);
        setCompleted(true);
        System.out.println("Time consumed: " + ((System.nanoTime()-startTime)* 1.0E-9) + " seconds");
    }
    
    /**
     * Computes the X and Y gradients using forward difference
     * @param srcBuffer
     * @param px  X gradient is stored in px
     * @param py  Y gradient is stored in py
     */
    private void computeGradients(float srcBuffer[], float px[], float py[]) {
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
            jS = j + 1; // address of south neighbor
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
     * @param beta user specified parameter used to compute the range standard deviation
     * @return range standard deviation (sigmaR), equation 11
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
        float rangeStandardDeviation;
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
                    else { // lev > 0 Gradients at each level of the image stack are computed from the level below
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
        
        // range standard deviation is computed as a ratio of difference between max and min gradient
        rangeStandardDeviation = beta * (maxGrad - minGrad);
        return rangeStandardDeviation;
    }
    
    /**
     * Bilaterally filters the X and Y gradients of the input image.
     * @param px  X gradient of the input image
     * @param py  Y gradient of the input image
     * @param pSmoothX  output bilaterally filtered X gradient
     * @param pSmoothY  output bilaterally filtered Y gradient
     * @param sigmaC  domain standard deviation of the bilateral filter
     * @param sigmaR  range standard deviation of the bilateral filter
     * @param filterSize  size of the filter kernel
     */
    private void bilateralGradientFilter(float px[], float py[], float pSmoothX[], float pSmoothY[], float sigmaC,
                                         float sigmaR, int filterSize) {
        int i;
        int j;
        int m;
        int n;
        int halfSize;
        double tmpX;
        double tmpY;
        double posDiff;
        double gradDiff;
        double domainWeight;
        double rangeWeight;
        double normFactor;
        double g1;
        double g2;
        int index;
        int index2;
        double dr;
        
        halfSize = (filterSize - 1)/2; // size of the filter kernel
        
        for (i = 0; i < xDim; i++) { // X scanline
            for (j = 0; j < yDim; j++) { // Y scanline
                index = i + j * xDim;
                normFactor = 0.0;
                tmpX = 0.0;
                tmpY = 0.0;
                for (m = -halfSize; m <= halfSize; m++) {
                    for (n = -halfSize; n <= halfSize; n++) {
                        posDiff = (double)(m*m + n*n);
                        // Compute the weight for the domain filter (domainWeight).  The domain filter
                        // is a Gaussian lowpass filter.
                        domainWeight = Math.exp(-posDiff/(2.0 * sigmaC * sigmaC));
                        if ((i+m) >= 0 && (i+m) < xDim && (j+n) >= 0 && (j+n) < yDim) {
                            index2 = (i+m) + (j+n) * xDim;
                            g1 = px[index2]*px[index2] + py[index2]*py[index2];
                            g2 = px[index]*px[index] + py[index]*py[index];
                            // Compute the gradient difference between a pixel and its neighborhood pixel
                            gradDiff = (Math.sqrt(g1) - Math.sqrt(g2));
                            // Compute the weight for the range filter (rangeWeight).  The range filter
                            // is a Gaussian filter defined by the difference in gradient magnitude.
                            rangeWeight = Math.exp(-(gradDiff * gradDiff)/(2.0 * sigmaR * sigmaR));
                            dr = domainWeight * rangeWeight;
                            tmpX += (px[index2]*dr);
                            tmpY += (py[index2]*dr);
                            // Bilateral filter normalized by normFactor (eq. 5, Section 3.1)
                            normFactor += dr;
                        } // if ((i+m) >= 0 && (i+m) < xDim && (j+n) >= 0 && (j+n) < yDim)
                    } // for (n = -halfSize; n <= halfSize; n++)
                } // for (m = -halfSize; m <= halfSize; m++)
                tmpX = (tmpX/normFactor);
                tmpY = (tmpY/normFactor);
                pSmoothX[index] = (float)tmpX;
                pSmoothY[index] = (float)tmpY;
            } // for (j = 0; j < yDim; j++)
        } // for (i = 0; i < xDim; i++)
    }
    
    /**
     * Finds adaptive neighborhood for every pixel
     * @param pMinStack Min gradient stack
     * @param pMaxStack Max gradient stack
     * @param R threshold value for computing fTheta
     * @param levelMax maximum level of the image stack 
     * @param fTheta output stack level that satisfies equation 10
     */
    private void findAdaptiveRegion(float pMinStack[][][], float pMaxStack[][][], float R, int levelMax, float fTheta[]) {
        int i;
        int j;
        int lev;
        int index;
        
        for (j = 0; j < yDim; j++) {
            for (i = 0; i < xDim; i++) {
                for (lev = 0; lev < levelMax; lev++) {
                    // Compute the adaptive neighborhood based on the similarity of 
                    // the neighborhood gradients, equation 10, Section 3.2.
                    if (pMaxStack[i][j][lev] > (pMaxStack[i][j][0] + R) ||
                        pMinStack[i][j][lev] < (pMaxStack[i][j][0] - R)) {
                        break;
                    }
                } // for (lev = 0; lev < levelMax; lev++)
                index = i + j * xDim;
                fTheta[index] = (float)(lev - 1);
            } // for (i = 0; i < xDim; i++)
        } // for (j = 0; j < yDim; j++)
    }
    
    /**
     * Filters the detail signal and computes the output (2nd filtering pass for trilateral
     * filter).
     * @param srcBuffer input image filter
     * @param pSmoothX  bilaterally filtered X gradient of srcImage
     * @param pSmoothY  bilaterally filtered Y gradient of srcImage
     * @param fTheta adaptive neighborhood for each pixel of srcImage    
     * @param sigmaCTheta  domain standard deviation of the bilateral filter
     * @param sigmaRTheta  range standard deviation of the bilateral filter
     * @param result  trilaterally filtered output
     */
    private void detailBilateralFilter(float srcBuffer[], float pSmoothX[], float pSmoothY[],
                                       float fTheta[], float sigmaCTheta, float sigmaRTheta, float result[]) {
        int i;
        int j;
        int m;
        int n;
        int halfSize;
        double tmp;
        double diff;
        double detail;
        double domainWeight;
        double rangeWeight;
        double normFactor;
        double coeffA; // coeffA = dI/dx
        double coeffB; // coeffB = dI/dy
        double coeffC; // coeffc = I at center pixel of the filter kernel
        int index;
        int index2;
        double dr;
        
        for (i = 0; i < xDim; i++) { // X scanline
            fireProgressStateChanged(i * 100/xDim);
            for (j = 0; j < yDim; j++) { // Y scanline
                index = i + j * xDim;
                normFactor = 0.0;
                tmp = 0.0;
                // filter window width is calculated from fTheta
                // halfSize is half of the filter window width
                halfSize = (int)fTheta[index];
                halfSize = (int)(Math.pow(2.0, halfSize)/2.0);
                
                // Coefficients defining the centerplane (equation 6, section 3.1) are calculated
                // from the smoothed image gradients
                coeffA = pSmoothX[index];
                coeffB = pSmoothY[index];
                coeffC = srcBuffer[index];
                for (m = -halfSize; m <= halfSize; m++) {
                    for (n = -halfSize; n <= halfSize; n++) {
                        diff = (double)(m*m + n*n);
                        // Compute the weight for the domain filter (domainWeight).  The domain filter
                        // is a Gaussian lowpass filter
                        domainWeight = Math.exp(-diff/(2.0 * sigmaCTheta * sigmaCTheta));
                        if ((i+m) >= 0 && (i+m) < xDim && (j+n) >= 0 && (j+n) < yDim) {
                            // Compute the detail signal (detail) based on the difference between a
                            // neighborhood pixel and the centerplane passing through the center-pixel
                            // of the filter window.  See equation 7, section 3.1 for details
                            index2 = (i+m) + (j+n) * xDim;
                            detail = srcBuffer[index2] - coeffA * m - coeffB * n - coeffC;
                            // Compute the weight for the range filter (rangeWeight).  The range filter
                            // is a Gaussian filter defined by the detail signal.
                            rangeWeight = Math.exp(-(detail*detail)/(2.0 * sigmaRTheta * sigmaRTheta));
                            dr = domainWeight * rangeWeight;
                            tmp += detail * dr;
                            // Detail bilateral filter normalized by normFactor (eq. 9, Section 3.1)
                            normFactor += dr;
                        } // if ((i+m) >= 0 && (i+m) < xDim && (j+n) >= 0 && (j+n) < yDim)
                    } // for (n = -halfSize; n <= halfSize; n++)
                } // for (m = -halfSize; m <= halfSize; m++)
                tmp = tmp/normFactor;
                tmp += coeffC;
                result[index] = (float)tmp;
            } // for (j = 0; j < yDim; j++)
        } // for (i = 0; i < xDim; i++)
        
    }
    
    public void algorithmPerformed(AlgorithmBase algorithm){
        
    }
    
    
    private void convertRGBtoCIELab(float cieBuffer[], float srcBuffer[]) {
        // Observer = 2 degrees, Illuminant = D65
        double XN = 95.047;
        double YN = 100.000;
        double ZN = 108.883;
        int i;
        double varR, varG, varB;
        double X, Y, Z;
        double varX, varY, varZ;
        float L, a, b;
        
        for (i = 0; i < cieBuffer.length; i += 4) {
            varR = cieBuffer[i+1]/scaleMax;
            varG = cieBuffer[i+2]/scaleMax;
            varB = cieBuffer[i+3]/scaleMax;
            
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
            
            cieBuffer[i+1] = L;
            srcBuffer[i >> 2] = L;
            cieBuffer[i+2] = a;
            cieBuffer[i+3] = b;
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
            G = scaleMax * varG;
            B = scaleMax * varB;
            
            buffer[i+1] = (float)R;
            buffer[i+2] = (float)G;
            buffer[i+3] = (float)B;
        } // for (i = 0; i < buffer.length; i += 4)
    } // private void convertCIELabtoRGB(float buffer[])
}

