package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.util.ThreadUtil;
import gov.nih.mipav.view.ViewJProgressBar;

import java.io.*;
import java.util.concurrent.CountDownLatch;


/**
 * Convolves kernel with a 2D or 3D image - only pixels where the kernel is completely contained in the image are
 * convolved, otherwise they are set to zero. This is reasonable since data at the edges of images is rarely used and
 * large kernels should not be used since it is much faster to perform FFT, filter, and IFFT. The break even point is
 * probably around a kernel size of 11 or so.
 *
 * <p>Since this class extends the AlgorithmBase class that extends the Thread class it can be run in its own thread by
 * invoking algoConvolver3DObj.start(); It can also be invoked without a new thread by calling the the run() method
 * directly (ie. algoConvolver3DObj.run()).</p>
 *
 * <ol>
 *   <li>Source image is exported (locked and unlocked by export)</li>
 *   <li>Kernel is exported</li>
 *   <li>Image is convolved with kernel</li>
 *   <li>Return</li>
 * </ol>
 *
 * @version  0.1 Aug 1, 1997
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class AlgorithmConvolver extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The image data to convolve. */

    private int[] kExtents;
    
    private float[] kernelBuffer;
    
    private float[] kernelBufferX;
    
    private float[] kernelBufferY;
    
    private float[] kernelBufferZ;
    
    private float[] kernelBufferX2;
    
    private float[] kernelBufferY2;
    
    private float[] kernelBufferXX;
    
    private float[] kernelBufferXY;
    
    private float[] kernelBufferYY;
    
    private float[] kernelBufferXZ;
    
    private float[] kernelBufferYZ;
    
    private float[] kernelBufferZZ;
    
    private float[] kernelBufferXXX;
    
    private float[] kernelBufferXXY;
    
    private float[] kernelBufferXYY;
    
    private float[] kernelBufferYYY;
    
    private float[] kernelBufferXXZ;
    
    private float[] kernelBufferXZZ;
    
    private float[] kernelBufferXYZ;
    
    private float[] kernelBufferYYZ;
    
    private float[] kernelBufferYZZ;
    
    private float[] kernelBufferZZZ;
    
    private boolean red, blue, green;
    
    private float[] outputBuffer;
    
    private boolean entireImage;
    
    private boolean doXY = false;
    
    // Used with 2D AlgorithmAnistropicDiffusion
    private boolean sqrtXY = false;
    
    // used with 3D AlgorithmAnistropicDiffusion
    private boolean sqrtXYZ = false;
    
    // Used with 2D AlgorithmNMSuppression
    private boolean nms2 = false;
    
    // Used with 2D AlgorithmEdgeNMSuppression
    private boolean nms2e = false;
    
    // Used with 3D AlgorithmNMSuppression
    private boolean nms3 = false;
    
    // Used with 3D AlgorithmEdgeNMSuppression
    private boolean nms3e = false;
    
    // Used with 3D and 4D AlgorithmGradientMagnitude
    // 3D kernels used on slices where whole kernels fit, 2D kernels used on other slices
    private boolean combined2D3D = false;
    
    private double intensityGaussianDenom = 1.0;
    
    // Used with AlgorithmBilateralFilter
    private boolean bilateral = false;
    //~ Constructors ---------------------------------------------------------------------------------------------------

    public boolean isRed() {
		return red;
	}

	public void setRed(boolean red) {
		this.red = red;
	}

	public boolean isBlue() {
		return blue;
	}

	public void setBlue(boolean blue) {
		this.blue = blue;
	}

	public boolean isGreen() {
		return green;
	}

	public void setGreen(boolean green) {
		this.green = green;
	}

	public void setColorChannels(boolean red, boolean green, boolean blue){
		this.red = red;
		this.green = green;
		this.blue = blue;
	}
    

	/**
	 * 
     * Sets the source and kernel images and calls the appropriate method based on image dimensionality.
     * 
	 * @param srcImage
	 * @param kernel
	 * @param kExtents
	 * @param entireImage
	 * @param image25D
	 */
    
    public AlgorithmConvolver(ModelImage srcImage, float[] kernel, int[] kExtents, boolean entireImage, boolean image25D){
    	super(null, srcImage);
    	kernelBuffer = kernel;
    	this.kExtents = kExtents;
    	this.entireImage = entireImage;
    	this.image25D = image25D;
    }
    
    public AlgorithmConvolver(ModelImage srcImage, float[] kernel, int[] kExtents, boolean entireImage, boolean image25D,
            double intensityGaussianDenom){
        super(null, srcImage);
        kernelBuffer = kernel;
        this.kExtents = kExtents;
        this.entireImage = entireImage;
        this.image25D = image25D;
        this.intensityGaussianDenom = intensityGaussianDenom;
        bilateral = true;
    }
    
    public AlgorithmConvolver(ModelImage srcImage, float[] kernelX, float[] kernelY,
                              int[] kExtents, boolean entireImage, boolean sqrtXY){
        super(null, srcImage);
        kernelBufferX = kernelX;
        kernelBufferY = kernelY;
        this.kExtents = kExtents;
        this.entireImage = entireImage;
        image25D = true;
        this.sqrtXY = sqrtXY;
        doXY = true;
    }
    
    
    public AlgorithmConvolver(ModelImage srcImage, float[] kernelX, float[] kernelY, float[] kernelZ,
            int[] kExtents, boolean entireImage){
        super(null, srcImage);
        kernelBufferX = kernelX;
        kernelBufferY = kernelY;
        kernelBufferZ = kernelZ;
        this.kExtents = kExtents;
        this.entireImage = entireImage;
        image25D = false;
        sqrtXYZ = true;
    }
    
    public AlgorithmConvolver(ModelImage srcImage, float[] kernelX, float[] kernelY, float[] kernelXX,
                              float[] kernelXY, float[] kernelYY, int[] kExtents, boolean entireImage) {
        super(null, srcImage);
        kernelBufferX = kernelX;
        kernelBufferY = kernelY;
        kernelBufferXX = kernelXX;
        kernelBufferXY = kernelXY;
        kernelBufferYY = kernelYY;
        this.kExtents = kExtents;
        this.entireImage = entireImage;
        image25D = true;
        nms2 = true;
    }
    
    public AlgorithmConvolver(ModelImage srcImage, float[] kernelX, float[] kernelY, float[] kernelXX,
            float[] kernelXY, float[] kernelYY, float[] kernelXXX, float[] kernelXXY,
            float[] kernelXYY, float[] kernelYYY, int[] kExtents, boolean entireImage) {
        super(null, srcImage);
        kernelBufferX = kernelX;
        kernelBufferY = kernelY;
        kernelBufferXX = kernelXX;
        kernelBufferXY = kernelXY;
        kernelBufferYY = kernelYY;
        kernelBufferXXX = kernelXXX;
        kernelBufferXXY = kernelXXY;
        kernelBufferXYY = kernelXYY;
        kernelBufferYYY = kernelYYY;
        this.kExtents = kExtents;
        this.entireImage = entireImage;
        image25D = true;
        nms2e = true;
    }
    
    public AlgorithmConvolver(ModelImage srcImage, float[] kernelX, float[] kernelY, float[] kernelZ, float[] kernelXX,
            float[] kernelXY, float[] kernelYY, float[] kernelXZ, float[] kernelYZ, float[] kernelZZ,
            boolean entireImage, int[] kExtents) {
        // Note that last 2 arguments are reversed from previous order to distinguish constructors
        super(null, srcImage);
        kernelBufferX = kernelX;
        kernelBufferY = kernelY;
        kernelBufferZ = kernelZ;
        kernelBufferXX = kernelXX;
        kernelBufferXY = kernelXY;
        kernelBufferYY = kernelYY;
        kernelBufferXZ = kernelXZ;
        kernelBufferYZ = kernelYZ;
        kernelBufferZZ = kernelZZ;
        this.kExtents = kExtents;
        this.entireImage = entireImage;
        image25D = false;
        nms3 = true;
    }
    
    public AlgorithmConvolver(ModelImage srcImage, float[] kernelX, float[] kernelY, float[] kernelZ, float[] kernelXX,
            float[] kernelXY, float[] kernelYY, float[] kernelXZ, float[] kernelYZ, float[] kernelZZ,
            float[] kernelXXX, float[] kernelXXY, float[] kernelXYY, float[] kernelYYY, float[] kernelXXZ,
            float[] kernelXZZ, float[] kernelXYZ, float[] kernelYYZ, float[] kernelYZZ, float[] kernelZZZ,
            int[] kExtents, boolean entireImage) {
        super(null, srcImage);
        kernelBufferX = kernelX;
        kernelBufferY = kernelY;
        kernelBufferZ = kernelZ;
        kernelBufferXX = kernelXX;
        kernelBufferXY = kernelXY;
        kernelBufferYY = kernelYY;
        kernelBufferXZ = kernelXZ;
        kernelBufferYZ = kernelYZ;
        kernelBufferZZ = kernelZZ;
        kernelBufferXXX = kernelXXX;
        kernelBufferXXY = kernelXXY;
        kernelBufferXYY = kernelXYY;
        kernelBufferYYY = kernelYYY;
        kernelBufferXXZ = kernelXXZ;
        kernelBufferXZZ = kernelXZZ;
        kernelBufferXYZ = kernelXYZ;
        kernelBufferYYZ = kernelYYZ;
        kernelBufferYZZ = kernelYZZ;
        kernelBufferZZZ = kernelZZZ;
        this.kExtents = kExtents;
        this.entireImage = entireImage;
        image25D = false;
        nms3e = true;
    }
    
    public AlgorithmConvolver(ModelImage srcImage, float kernelX[], float kernelY[], float kernelZ[], float kernelX2[],
                              float kernelY2[], int kExtents[], boolean entireImage, boolean combined2D3D) {
        super(null, srcImage);
        kernelBufferX = kernelX;
        kernelBufferY = kernelY;
        kernelBufferZ = kernelZ;
        kernelBufferX2 = kernelX2;
        kernelBufferY2 = kernelY2;
        this.kExtents = kExtents;
        this.entireImage = entireImage;
        this.combined2D3D = combined2D3D;
    }
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * A static function that convolves a kernel with an image at a position.
     *
     * @param   pix       index indicating location of convolution
     * @param   iExtents  image dimensions
     * @param   image     image data
     * @param   kExtents  kernel dimensions
     * @param   kernel    kernel data
     *
     * @return  the value of the pixel after convolution with the kernel
     */
    public static final synchronized float convolve2DPt(int pix, int[] iExtents, float[] image, int[] kExtents,
                                                        float[] kernel) {

        int i, j;
        int offsetX, offsetY;
        int xDim = iExtents[0];
        int yDim = iExtents[1];
        int xKDim = kExtents[0];
        int yKDim = kExtents[1];
        int yLimit = xDim * yDim;

        int startX, startY;
        int endX, endY;
        int count;
        double sum;
        double norm = 0;

        offsetX = (pix % xDim) - (xKDim / 2);
        offsetY = (pix / xDim) - (yKDim / 2);
        sum = 0;
        count = 0;
        startY = offsetY * xDim;
        endY = startY + (yKDim * xDim);

        for (j = startY; j < endY; j += xDim) {
            startX = j + offsetX;
            endX = startX + xKDim;

            for (i = startX; i < endX; i++) {

                if ((j >= 0) && (j < yLimit) && ((i - j) >= 0) && ((i - j) < xDim)) {

                    // Needed for compiler bug
                    // Run same image twice from AlgorithmLevelSetDiffusion and
                    // array index out of bounds exception shows up
                    if (count >= kernel.length) {
                        break;
                    }

                    sum += kernel[count] * image[i];

                    if (kernel[count] >= 0) {
                        norm += kernel[count];
                    } else {
                        norm += -kernel[count];
                    }
                }

                count++;
            }
        }

        if (norm > 0) {
            return (float)(sum / norm);
        } else {
            return 0;
        }
    }
    
    /**
     * A static function that convolves a kernel with an image at a position.  The intensity difference between the 
     * center pixel and the indexed pixel is used in a Gaussian in the weighing.
     * The intensity Gaussian is (1.0/(Math.sqrt(2.0 * Math.PI) * intensitySigma))* exp(-intensityDifference**2/intenDenom)
     * Since the same (1.0/(Math.sqrt(2.0 * Math.PI) * intensitySigma) term would appear in every weight term, it would cancel
     * out by appearing in both the numerator and denominator in (sum / norm) and so can be omitted.
     *
     * @param   pix       index indicating location of convolution
     * @param   iExtents  image dimensions
     * @param   image     image data
     * @param   kExtents  kernel dimensions
     * @param   kernel    kernel data
     * @param   intensityGaussianDenom
     *
     * @return  the value of the pixel after convolution with the kernel
     */
    public static final synchronized float convolveBilateral2DPt(int pix, int[] iExtents, float[] image, int[] kExtents,
                                                        float[] kernel, double intensityGaussianDenom) {

        int i, j;
        int offsetX, offsetY;
        int xDim = iExtents[0];
        int yDim = iExtents[1];
        int xKDim = kExtents[0];
        int yKDim = kExtents[1];
        int yLimit = xDim * yDim;

        int startX, startY;
        int endX, endY;
        int count;
        double sum;
        double norm = 0;

        offsetX = (pix % xDim) - (xKDim / 2);
        offsetY = (pix / xDim) - (yKDim / 2);
        sum = 0;
        count = 0;
        startY = offsetY * xDim;
        endY = startY + (yKDim * xDim);
        float centerIntensityValue = image[pix];
        float intensityValue;
        double intensityGaussian;
        float intensityDifference;
        double weight;

        for (j = startY; j < endY; j += xDim) {
            startX = j + offsetX;
            endX = startX + xKDim;

            for (i = startX; i < endX; i++) {

                if ((j >= 0) && (j < yLimit) && ((i - j) >= 0) && ((i - j) < xDim)) {

                    // Needed for compiler bug
                    // Run same image twice from AlgorithmLevelSetDiffusion and
                    // array index out of bounds exception shows up
                    if (count >= kernel.length) {
                        break;
                    }
                    
                    intensityValue = image[i];
                    intensityDifference = intensityValue - centerIntensityValue;
                    intensityGaussian = Math.exp(-intensityDifference*intensityDifference/intensityGaussianDenom);
                    weight = kernel[count] * intensityGaussian;
                    sum += weight * intensityValue;

                    if (weight >= 0) {
                        norm += weight;
                    } else {
                        norm += -weight;
                    }
                }

                count++;
            }
        }

        if (norm > 0) {
            return (float)(sum / norm);
        } else {
            return 0;
        }
    }
    
    /**
     * A static function that convolves a kernel with an image at a position.
     *
     * @param   pix       index indicating location of convolution
     * @param   iExtents  image dimensions
     * @param   image     image data
     * @param   kExtents  kernel dimensions
     * @param   kernelX   kernel data
     * @param   kernelY   kernel data
     *
     * @return  the value of the pixel after convolution with the kernel
     */
    public static final synchronized float convolve2DPtSqrtXY(int pix, int[] iExtents, float[] image, int[] kExtents,
                                                        float[] kernelX, float[] kernelY) {

        int i, j;
        int offsetX, offsetY;
        int xDim = iExtents[0];
        int yDim = iExtents[1];
        int xKDim = kExtents[0];
        int yKDim = kExtents[1];
        int yLimit = xDim * yDim;

        int startX, startY;
        int endX, endY;
        int count;
        double sumX;
        double sumY;
        double normX = 0;
        double normY = 0;
        double ptX;
        double ptY;

        offsetX = (pix % xDim) - (xKDim / 2);
        offsetY = (pix / xDim) - (yKDim / 2);
        sumX = 0;
        sumY = 0;
        count = 0;
        startY = offsetY * xDim;
        endY = startY + (yKDim * xDim);

        for (j = startY; j < endY; j += xDim) {
            startX = j + offsetX;
            endX = startX + xKDim;

            for (i = startX; i < endX; i++) {

                if ((j >= 0) && (j < yLimit) && ((i - j) >= 0) && ((i - j) < xDim)) {

                    // Needed for compiler bug
                    // Run same image twice from AlgorithmLevelSetDiffusion and
                    // array index out of bounds exception shows up
                    if (count >= kernelX.length) {
                        break;
                    }

                    sumX += kernelX[count] * image[i];
                    sumY += kernelY[count] * image[i];

                    if (kernelX[count] >= 0) {
                        normX += kernelX[count];
                    } else {
                        normX += -kernelX[count];
                    }
                    
                    if (kernelY[count] >= 0) {
                        normY += kernelY[count];
                    } else {
                        normY += -kernelY[count];
                    }
                }

                count++;
            }
        }

        if ((normX > 0) && (normY > 0)) {
            ptX = sumX/ normX;
            ptY = sumY/ normY;
            return (float)Math.sqrt(ptX*ptX + ptY*ptY);
        } else {
            return 0;
        }
    }
    
    /**
     * A static function that convolves a kernel with an image at a position.
     *
     * @param   pix       index indicating location of convolution
     * @param   iExtents  image dimensions
     * @param   image     image data
     * @param   kExtents  kernel dimensions
     * @param   kernelX   kernel data
     * @param   kernelY   kernel data
     * @param   kernelXX  kernel data
     * @param   kernelXY  kernel data
     * @param   kernelYY  kernel data
     *
     * @return  the value of the pixel after convolution with the kernel
     */
    public static final synchronized float convolve2DPtNMS(int pix, int[] iExtents, float[] image, int[] kExtents,
                                                        float[] kernelX, float[] kernelY, float[] kernelXX,
                                                        float[] kernelXY, float[] kernelYY) {

        int i, j;
        int offsetX, offsetY;
        int xDim = iExtents[0];
        int yDim = iExtents[1];
        int xKDim = kExtents[0];
        int yKDim = kExtents[1];
        int yLimit = xDim * yDim;

        int startX, startY;
        int endX, endY;
        int count;
        double sumX;
        double sumY;
        double sumXX;
        double sumXY;
        double sumYY;
        double normX = 0;
        double normY = 0;
        double normXX = 0;
        double normXY = 0;
        double normYY = 0;
        double ptX;
        double ptY;
        double ptXX;
        double ptXY;
        double ptYY;

        offsetX = (pix % xDim) - (xKDim / 2);
        offsetY = (pix / xDim) - (yKDim / 2);
        sumX = 0;
        sumY = 0;
        sumXX = 0;
        sumXY = 0;
        sumYY = 0;
        count = 0;
        startY = offsetY * xDim;
        endY = startY + (yKDim * xDim);

        for (j = startY; j < endY; j += xDim) {
            startX = j + offsetX;
            endX = startX + xKDim;

            for (i = startX; i < endX; i++) {

                if ((j >= 0) && (j < yLimit) && ((i - j) >= 0) && ((i - j) < xDim)) {

                    // Needed for compiler bug
                    // Run same image twice from AlgorithmLevelSetDiffusion and
                    // array index out of bounds exception shows up
                    if (count >= kernelX.length) {
                        break;
                    }

                    sumX += kernelX[count] * image[i];
                    sumY += kernelY[count] * image[i];
                    sumXX += kernelXX[count] * image[i];
                    sumXY += kernelXY[count] * image[i];
                    sumYY += kernelYY[count] * image[i];

                    if (kernelX[count] >= 0) {
                        normX += kernelX[count];
                    } else {
                        normX += -kernelX[count];
                    }
                    
                    if (kernelY[count] >= 0) {
                        normY += kernelY[count];
                    } else {
                        normY += -kernelY[count];
                    }
                    
                    if (kernelXX[count] >= 0) {
                        normXX += kernelXX[count];
                    } else {
                        normXX += -kernelXX[count];
                    }
                    
                    if (kernelXY[count] >= 0) {
                        normXY += kernelXY[count];
                    } else {
                        normXY += -kernelXY[count];
                    }
                    
                    if (kernelYY[count] >= 0) {
                        normYY += kernelYY[count];
                    } else {
                        normYY += -kernelYY[count];
                    }
                }

                count++;
            }
        }

        if ((normX > 0) && (normY > 0) && (normXX > 0) && (normXY > 0) && (normYY > 0)) {
            ptX = sumX/ normX;
            ptY = sumY/ normY;
            ptXX = sumXX/ normXX;
            ptXY = sumXY/ normXY;
            ptYY = sumYY/ normYY;
            return (float)((ptX * ptX * ptXX) + (2.0 * ptX * ptY * ptXY) + (ptY * ptY * ptYY));
        } else {
            return 0;
        }
    }
    
    /**
     * A static function that convolves a kernel with an image at a position.
     *
     * @param   pix       index indicating location of convolution
     * @param   iExtents  image dimensions
     * @param   image     image data
     * @param   kExtents  kernel dimensions
     * @param   kernelX   kernel data
     * @param   kernelY   kernel data
     * @param   kernelXXX  kernel data
     * @param   kernelXXY  kernel data
     * @param   kernelXYY  kernel data
     * @param   kernelYYY  kernel data
     *
     * @return  the value of the pixel after convolution with the kernel
     */
    public static final synchronized float convolve2DPtNMSE(int pix, int[] iExtents, float[] image, int[] kExtents,
                                                        float[] kernelX, float[] kernelY, float[] kernelXXX,
                                                        float[] kernelXXY, float[] kernelXYY, float[] kernelYYY) {

        int i, j;
        int offsetX, offsetY;
        int xDim = iExtents[0];
        int yDim = iExtents[1];
        int xKDim = kExtents[0];
        int yKDim = kExtents[1];
        int yLimit = xDim * yDim;

        int startX, startY;
        int endX, endY;
        int count;
        double sumX;
        double sumY;
        double sumXXX;
        double sumXXY;
        double sumXYY;
        double sumYYY;
        double normX = 0;
        double normY = 0;
        double normXXX = 0;
        double normXXY = 0;
        double normXYY = 0;
        double normYYY = 0;
        double ptX;
        double ptY;
        double ptXXX;
        double ptXXY;
        double ptXYY;
        double ptYYY;

        offsetX = (pix % xDim) - (xKDim / 2);
        offsetY = (pix / xDim) - (yKDim / 2);
        sumX = 0;
        sumY = 0;
        sumXXX = 0;
        sumXXY = 0;
        sumXYY = 0;
        sumYYY = 0;
        count = 0;
        startY = offsetY * xDim;
        endY = startY + (yKDim * xDim);

        for (j = startY; j < endY; j += xDim) {
            startX = j + offsetX;
            endX = startX + xKDim;

            for (i = startX; i < endX; i++) {

                if ((j >= 0) && (j < yLimit) && ((i - j) >= 0) && ((i - j) < xDim)) {

                    // Needed for compiler bug
                    // Run same image twice from AlgorithmLevelSetDiffusion and
                    // array index out of bounds exception shows up
                    if (count >= kernelX.length) {
                        break;
                    }

                    sumX += kernelX[count] * image[i];
                    sumY += kernelY[count] * image[i];
                    sumXXX += kernelXXX[count] * image[i];
                    sumXXY += kernelXXY[count] * image[i];
                    sumXYY += kernelXYY[count] * image[i];
                    sumYYY += kernelYYY[count] * image[i];

                    if (kernelX[count] >= 0) {
                        normX += kernelX[count];
                    } else {
                        normX += -kernelX[count];
                    }
                    
                    if (kernelY[count] >= 0) {
                        normY += kernelY[count];
                    } else {
                        normY += -kernelY[count];
                    }
                    
                    if (kernelXXX[count] >= 0) {
                        normXXX += kernelXXX[count];
                    } else {
                        normXXX += -kernelXXX[count];
                    }
                    
                    if (kernelXXY[count] >= 0) {
                        normXXY += kernelXXY[count];
                    } else {
                        normXXY += -kernelXXY[count];
                    }
                    
                    if (kernelXYY[count] >= 0) {
                        normXYY += kernelXYY[count];
                    } else {
                        normXYY += -kernelXYY[count];
                    }
                    
                    if (kernelYYY[count] >= 0) {
                        normYYY += kernelYYY[count];
                    } else {
                        normYYY += -kernelYYY[count];
                    }
                }

                count++;
            }
        }

        if ((normX > 0) && (normY > 0) && (normXXX > 0) && (normXXY > 0) && (normXYY > 0) && (normYYY > 0)) {
            ptX = sumX/ normX;
            ptY = sumY/ normY;
            ptXXX = sumXXX/ normXXX;
            ptXXY = sumXXY/ normXXY;
            ptXYY = sumXYY/ normXYY;
            ptYYY = sumYYY/ normYYY;
            return (float)((ptX * ptX * ptX * ptXXX) + (3.0 * ptX * ptX * ptY * ptXXY) +
                    (3.0 * ptX * ptY * ptY * ptXYY) + (ptY * ptY * ptY * ptYYY));
        } else {
            return 0;
        }
    }

    /**
     * A static function that convolves a kernel with an image at a position.
     *
     * @param   pt        floating point indicating location of convolution
     * @param   iExtents  image dimensions
     * @param   image     image data
     * @param   kExtents  kernel dimensions
     * @param   kernel    kernel data
     *
     * @return  the value of the pixel after convolution with the kernel
     */
    public static final float convolve2DPt(Vector2f pt, int[] iExtents, float[] image, int[] kExtents, float[] kernel) {

        int i, j;
        float dx, dy;
        int offsetX, offsetY;
        int xDim = iExtents[0];
        int yDim = iExtents[1];
        int yLimit = xDim * yDim;
        int xKDim = kExtents[0];
        int yKDim = kExtents[1];

        int startX, startY;
        int endX, endY;
        int count;
        double sum;
        double norm = 0;

        dx = pt.X - (int) pt.X;
        dy = pt.Y - (int) pt.Y;
        offsetX = (int) pt.X - (xKDim / 2);
        offsetY = (int) pt.Y - (yKDim / 2);
        sum = 0;
        count = 0;
        startY = offsetY * xDim;
        endY = startY + (yKDim * xDim);

        for (j = startY; j < endY; j += xDim) {
            startX = j + offsetX;
            endX = startX + xKDim;

            for (i = startX; i < endX; i++) {

                if ((j >= 0) && (j < yLimit) && ((i - j) >= 0) && ((i - j) < xDim)) {
                    sum += kernel[count] * getBilinear(i, dx, dy, iExtents, image);

                    if (kernel[count] >= 0) {
                        norm += kernel[count];
                    } else {
                        norm += -kernel[count];
                    }
                }

                count++;
            }
        }

        if (norm > 0) {
            return (float)(sum / norm);
        } else {
            return 0;
        }
    }

    /**
     * A static function that convolves a kernel with an image at a position. This version seems to just be used by
     * AlgorithmLapMedianess.
     *
     * @see     gov.nih.mipav.model.algorithms.AlgorithmLapMedianess
     *
     * @param   pix       index indicating location of convolution
     * @param   iExtents  image dimensions
     * @param   image     image data
     * @param   kExtents  kernel dimensions
     * @param   kernel    kernel data
     *
     * @return  the value of the pixel after convolution with the kernel
     */
    public static final float convolve2DPtMed(int pix, int[] iExtents, float[] image, int[] kExtents, float[] kernel) {

        int i, j;
        int offsetX, offsetY;
        int xDim = iExtents[0];
        int yDim = iExtents[1];
        int xKDim = kExtents[0];
        int yKDim = kExtents[1];
        int yLimit = xDim * yDim;

        int startX, startY;
        int endX, endY;
        int count;
        double sum;

        offsetX = (pix % xDim) - (xKDim / 2);
        offsetY = (pix / xDim) - (yKDim / 2);
        

        sum = 0;
        count = 0;
        startY = offsetY * xDim;
        endY = startY + (yKDim * xDim);

        for (j = startY; j < endY; j += xDim) {
            startX = j + offsetX;
            endX = startX + xKDim;

            for (i = startX; i < endX; i++) {

                if ((j >= 0) && (j < yLimit) && ((i - j) >= 0) && ((i - j) < xDim)) {
                    sum += kernel[count] * image[i];
                }

                count++;
            }
        }

        return (float)sum;
    }

    /**
     * A static function that convolves a kernel with an RGB image at a position.
     *
     * @param   pix       index indicating location of convolution
     * @param   iExtents  image dimensions
     * @param   image     image data
     * @param   kExtents  kernel dimensions
     * @param   kernel    kernel data
     *
     * @return  the value of the pixel after convolution with the kernel
     */
    public static final float convolve2DRGBPt(int pix, int[] iExtents, float[] image, int[] kExtents, float[] kernel) {

        int i, j;
        int offsetX, offsetY;
        int xDim = iExtents[0];
        int yDim = iExtents[1];
        int yLimit = 4 * xDim * yDim;
        int xKDim = kExtents[0];
        int yKDim = kExtents[1];
        int offset = 4 * xDim;

        int startX, startY;
        int endX, endY;
        int count;
        double sum;
        double norm = 0;

        offsetX = (pix % (offset)) - ((xKDim) / 2 * 4);
        offsetY = (pix / (offset)) - ((yKDim) / 2);

        sum = 0;
        count = 0;
        startY = offsetY * offset;
        endY = startY + (yKDim * offset);

        for (j = startY; j < endY; j += offset) {
            startX = j + offsetX;
            endX = startX + (xKDim * 4);

            for (i = startX; i < endX; i += 4) {

                if ((j >= 0) && (j < yLimit) && ((i - j) >= 0) && ((i - j) < offset)) {
                    sum += kernel[count] * image[i];

                    if (kernel[count] >= 0) {
                        norm += kernel[count];
                    } else {
                        norm += -kernel[count];
                    }
                }

                count++;
            }
        }

        if (norm > 0) {
            return (float)(sum / norm);
        } else {
            return 0;
        }
    }
    
    /**
     * A static function that convolves a kernel with an CIELab image at a position.  The CIE76 intensity difference between the
     * center pixel and the indexed pixel is used in a Gaussian in the weighing.
     * The intensity Gaussian is (1.0/(Math.sqrt(2.0 * Math.PI) * intensitySigma))* exp(-intensityDifference**2/intenDenom)
     * Since the same (1.0/(Math.sqrt(2.0 * Math.PI) * intensitySigma) term would appear in every weight term, it would cancel
     * out by appearing in both the numerator and denominator in (sum / norm) and so can be omitted.
     *
     * @param   pix       index indicating location of convolution
     * @param   iExtents  image dimensions
     * @param   image     image data
     * @param   kExtents  kernel dimensions
     * @param   kernel    kernel data
     * @param   intensityGaussianDenom
     *
     * @return  the L*,a*,b* of the pixel after convolution with the kernel
     */
    public static final float[] convolveBilateral2DCIELabPt(int pix, int[] iExtents, float[] image, int[] kExtents, float[] kernel,
                                                          double intensityGaussianDenom) {

        int i, j;
        int offsetX, offsetY;
        int xDim = iExtents[0];
        int yDim = iExtents[1];
        int yLimit = 4 * xDim * yDim;
        int xKDim = kExtents[0];
        int yKDim = kExtents[1];
        int offset = 4 * xDim;

        int startX, startY;
        int endX, endY;
        int count;
        double sumL;
        double suma;
        double sumb;
        double norm = 0;

        offsetX = (pix % (offset)) - ((xKDim) / 2 * 4);
        offsetY = (pix / (offset)) - ((yKDim) / 2);

        sumL = 0;
        suma = 0;
        sumb = 0;
        count = 0;
        startY = offsetY * offset;
        endY = startY + (yKDim * offset);
        float centerLValue = image[pix+1];
        float centeraValue = image[pix+2];
        float centerbValue = image[pix+3];
        float LValue;
        float aValue;
        float bValue;
        double intensityGaussian;
        float LDifference;
        float aDifference;
        float bDifference;
        double differenceSquared;
        double weight;
        float buf[] = new float[3];

        for (j = startY; j < endY; j += offset) {
            startX = j + offsetX;
            endX = startX + (xKDim * 4);

            for (i = startX; i < endX; i += 4) {

                if ((j >= 0) && (j < yLimit) && ((i - j) >= 0) && ((i - j) < offset)) {
                    LValue = image[i+1];
                    aValue = image[i+2];
                    bValue = image[i+3];
                    LDifference = LValue - centerLValue;
                    aDifference = aValue - centeraValue;
                    bDifference = bValue - centerbValue;
                    differenceSquared = (LDifference*LDifference + aDifference*aDifference + bDifference*bDifference);
                    intensityGaussian = Math.exp(-differenceSquared/intensityGaussianDenom);
                    weight = kernel[count] * intensityGaussian;
                    sumL += weight * LValue;
                    suma += weight * aValue;
                    sumb += weight * bValue;

                    if (weight >= 0) {
                        norm += weight;
                    } else {
                        norm += -weight;
                    }
                }

                count++;
            }
        }

        if (norm > 0) {
            buf[0] = (float)(sumL / norm);
            buf[1] = (float)(suma / norm);
            buf[2] = (float)(sumb / norm);
        }
        return buf;
    }
    
    /**
     * A static function that convolves a kernel with an RGB image at a position.
     *
     * @param   pix       index indicating location of convolution
     * @param   iExtents  image dimensions
     * @param   image     image data
     * @param   kExtents  kernel dimensions
     * @param   kernelX   kernel data
     * @param   kernelY   kernel data
     *
     * @return  the value of the pixel after convolution with the kernel
     */
    public static final float convolve2DRGBPtSqrtXY(int pix, int[] iExtents, float[] image, int[] kExtents,
                                                float[] kernelX, float[] kernelY) {

        int i, j;
        int offsetX, offsetY;
        int xDim = iExtents[0];
        int yDim = iExtents[1];
        int yLimit = 4 * xDim * yDim;
        int xKDim = kExtents[0];
        int yKDim = kExtents[1];
        int offset = 4 * xDim;

        int startX, startY;
        int endX, endY;
        int count;
        double sumX;
        double normX = 0;
        double sumY;
        double normY = 0;
        double xPt;
        double yPt;

        offsetX = (pix % (offset)) - ((xKDim) / 2 * 4);
        offsetY = (pix / (offset)) - ((yKDim) / 2);

        sumX = 0;
        sumY = 0;
        count = 0;
        startY = offsetY * offset;
        endY = startY + (yKDim * offset);

        for (j = startY; j < endY; j += offset) {
            startX = j + offsetX;
            endX = startX + (xKDim * 4);

            for (i = startX; i < endX; i += 4) {

                if ((j >= 0) && (j < yLimit) && ((i - j) >= 0) && ((i - j) < offset)) {
                    sumX += kernelX[count] * image[i];
                    sumY += kernelY[count] * image[i];

                    if (kernelX[count] >= 0) {
                        normX += kernelX[count];
                    } else {
                        normX += -kernelX[count];
                    }
                    
                    if (kernelY[count] >= 0) {
                        normY += kernelY[count];
                    } else {
                        normY += -kernelY[count];
                    }
                }

                count++;
            }
        }

        if ((normX > 0) && (normY > 0)) {
            xPt = sumX/ normX;
            yPt = sumY/ normY;
            return (float)Math.sqrt(xPt*xPt + yPt*yPt);
        } else {
            return 0;
        }
    }

    /**
     * A static function that convolves a kernel with an image at a position.
     *
     * @param   pix       index indicating location of convolution
     * @param   iExtents  image dimensions
     * @param   image     image data
     * @param   kExtents  kernel dimensions
     * @param   kernel    kernel data
     *
     * @return  the value of the pixel after convolution with the kernel
     */
    public static final float convolve3DPt(int pix, int[] iExtents, float[] image, int[] kExtents, float[] kernel) {

        int i, j, k;
        int offsetX, offsetY, offsetZ;
        int sliceSize = iExtents[0] * iExtents[1];
        int volSize = sliceSize * iExtents[2];
        int xDim = iExtents[0];
        int xKDim = kExtents[0];
        int indexY;
        int stepY, stepZ;
        int startX, startY, startZ;
        int endX, endY, endZ;
        int count;
        double sum;
        double norm = 0;

        offsetX = (pix % xDim) - (kExtents[0] / 2);
        offsetY = ((pix % sliceSize) / xDim) - (kExtents[1] / 2);
        offsetZ = (pix / (sliceSize)) - (kExtents[2] / 2);

        count = 0;
        sum = 0;
        indexY = offsetY * xDim;
        stepY = kExtents[1] * xDim;
        stepZ = kExtents[2] * sliceSize;
        startZ = offsetZ * sliceSize;
        endZ = startZ + stepZ;

        for (k = startZ; k < endZ; k += sliceSize) {

            if ((k >= 0) && (k < volSize)) {
                startY = k + indexY;
                endY = startY + stepY;

                for (j = startY; j < endY; j += xDim) {

                    if (((j - k) >= 0) && ((j - k) < sliceSize)) {
                        startX = j + offsetX;
                        endX = startX + xKDim;

                        for (i = startX; i < endX; i++) {

                            if (((i - j) >= 0) && ((i - j) < xDim)) {
                                sum += kernel[count] * image[i];

                                if (kernel[count] >= 0) {
                                    norm += kernel[count];
                                } else {
                                    norm += -kernel[count];
                                }
                            }

                            count++;
                        }
                    }else{
                    	count += kExtents[0];
                    }
                }
            }else{
            	count += kExtents[0]*kExtents[1];
            }
        }

        if (norm > 0) {
            return (float)(sum / norm);
        } else {
            return 0;
        }
    }
    
    /**
     * A static function that convolves a kernel with an image at a position.  The intensity difference between the 
     * center pixel and the indexed pixel is used in a Gaussian in the weighing.
     * The intensity Gaussian is (1.0/(Math.sqrt(2.0 * Math.PI) * intensitySigma))* exp(-intensityDifference**2/intenDenom)
     * Since the same (1.0/(Math.sqrt(2.0 * Math.PI) * intensitySigma) term would appear in every weight term, it would cancel
     * out by appearing in both the numerator and denominator in (sum / norm) and so can be omitted.
     *
     * @param   pix       index indicating location of convolution
     * @param   iExtents  image dimensions
     * @param   image     image data
     * @param   kExtents  kernel dimensions
     * @param   kernel    kernel data
     * @param   intensityGaussianDenom
     *
     * @return  the value of the pixel after convolution with the kernel
     */
    public static final float convolveBilateral3DPt(int pix, int[] iExtents, float[] image, int[] kExtents, float[] kernel,
                                                    double intensityGaussianDenom) {

        int i, j, k;
        int offsetX, offsetY, offsetZ;
        int sliceSize = iExtents[0] * iExtents[1];
        int volSize = sliceSize * iExtents[2];
        int xDim = iExtents[0];
        int xKDim = kExtents[0];
        int indexY;
        int stepY, stepZ;
        int startX, startY, startZ;
        int endX, endY, endZ;
        int count;
        double sum;
        double norm = 0;

        offsetX = (pix % xDim) - (kExtents[0] / 2);
        offsetY = ((pix % sliceSize) / xDim) - (kExtents[1] / 2);
        offsetZ = (pix / (sliceSize)) - (kExtents[2] / 2);

        count = 0;
        sum = 0;
        indexY = offsetY * xDim;
        stepY = kExtents[1] * xDim;
        stepZ = kExtents[2] * sliceSize;
        startZ = offsetZ * sliceSize;
        endZ = startZ + stepZ;
        float centerIntensityValue = image[pix]; 
        float intensityValue;
        double intensityGaussian;
        float intensityDifference;
        double weight;

        for (k = startZ; k < endZ; k += sliceSize) {

            if ((k >= 0) && (k < volSize)) {
                startY = k + indexY;
                endY = startY + stepY;

                for (j = startY; j < endY; j += xDim) {

                    if (((j - k) >= 0) && ((j - k) < sliceSize)) {
                        startX = j + offsetX;
                        endX = startX + xKDim;

                        for (i = startX; i < endX; i++) {

                            if (((i - j) >= 0) && ((i - j) < xDim)) {
                                intensityValue = image[i];
                                intensityDifference = intensityValue - centerIntensityValue;
                                intensityGaussian = Math.exp(-intensityDifference*intensityDifference/intensityGaussianDenom);
                                weight = kernel[count] * intensityGaussian;
                                sum += weight * intensityValue;

                                if (weight >= 0) {
                                    norm += weight;
                                } else {
                                    norm += -weight;
                                }
                                
                            }

                            count++;
                        }
                    }else{
                        count += kExtents[0];
                    }
                }
            }else{
                count += kExtents[0]*kExtents[1];
            }
        }

        if (norm > 0) {
            return (float)(sum / norm);
        } else {
            return 0;
        }
    }
    
    /**
     * A static function that convolves a kernel with an image at a position.
     *
     * @param   pix       index indicating location of convolution
     * @param   iExtents  image dimensions
     * @param   image     image data
     * @param   kExtents  kernel dimensions
     * @param   kernelX   kernel data
     * @param   kernelY   kernel data
     * @param   kernelZ   kernel data
     *
     * @return  the value of the pixel after convolution with the kernel
     */
    public static final float convolve3DPtXYZ(int pix, int[] iExtents, float[] image, int[] kExtents,
                                              float[] kernelX, float[] kernelY, float[] kernelZ) {

        int i, j, k;
        int offsetX, offsetY, offsetZ;
        int sliceSize = iExtents[0] * iExtents[1];
        int volSize = sliceSize * iExtents[2];
        int xDim = iExtents[0];
        int xKDim = kExtents[0];
        int indexY;
        int stepY, stepZ;
        int startX, startY, startZ;
        int endX, endY, endZ;
        int count;
        double sumX;
        double sumY;
        double sumZ;
        double normX = 0;
        double normY = 0;
        double normZ = 0;
        double ptX;
        double ptY;
        double ptZ;

        offsetX = (pix % xDim) - (kExtents[0] / 2);
        offsetY = ((pix % sliceSize) / xDim) - (kExtents[1] / 2);
        offsetZ = (pix / (sliceSize)) - (kExtents[2] / 2);

        count = 0;
        sumX = 0;
        sumY = 0;
        sumZ = 0;
        indexY = offsetY * xDim;
        stepY = kExtents[1] * xDim;
        stepZ = kExtents[2] * sliceSize;
        startZ = offsetZ * sliceSize;
        endZ = startZ + stepZ;

        for (k = startZ; k < endZ; k += sliceSize) {

            if ((k >= 0) && (k < volSize)) {
                startY = k + indexY;
                endY = startY + stepY;

                for (j = startY; j < endY; j += xDim) {

                    if (((j - k) >= 0) && ((j - k) < sliceSize)) {
                        startX = j + offsetX;
                        endX = startX + xKDim;

                        for (i = startX; i < endX; i++) {

                            if (((i - j) >= 0) && ((i - j) < xDim)) {
                                sumX += kernelX[count] * image[i];
                                sumY += kernelY[count] * image[i];
                                sumZ += kernelZ[count] * image[i];

                                if (kernelX[count] >= 0) {
                                    normX += kernelX[count];
                                } else {
                                    normX += -kernelX[count];
                                }
                                
                                if (kernelY[count] >= 0) {
                                    normY += kernelY[count];
                                } else {
                                    normY += -kernelY[count];
                                }
                                
                                if (kernelZ[count] >= 0) {
                                    normZ += kernelZ[count];
                                } else {
                                    normZ += -kernelZ[count];
                                }
                            }

                            count++;
                        }
                    }else{
                        count += kExtents[0];
                    }
                }
            }else{
                count += kExtents[0]*kExtents[1];
            }
        }

        if ((normX > 0) && (normY > 0) && (normZ > 0)) {
            ptX = sumX/normX;
            ptY = sumY/normY;
            ptZ = sumZ/normZ;
            return (float)Math.sqrt(ptX*ptX + ptY*ptY + ptZ*ptZ);
        } else {
            return 0;
        }
    }
    
    /**
     * A static function that convolves a kernel with an image at a position.
     *
     * @param   pix       index indicating location of convolution
     * @param   iExtents  image dimensions
     * @param   image     image data
     * @param   kExtents  kernel dimensions
     * @param   kernelX   kernel data
     * @param   kernelY   kernel data
     * @param   kernelZ   kernel data
     * @param   kernelXX  kernel data
     * @param   kernelXY  kernel data
     * @param   kernelYY  kernel data
     * @param   kernelXZ  kernel data
     * @param   kernelYZ  kernel data
     * @param   kernelZZ  kernel data
     *
     * @return  the value of the pixel after convolution with the kernel
     */
    public static final float convolve3DPtNMS(int pix, int[] iExtents, float[] image, int[] kExtents,
                                              float[] kernelX, float[] kernelY, float[] kernelZ,
                                              float[] kernelXX, float[] kernelXY, float[] kernelYY,
                                              float[] kernelXZ, float[] kernelYZ, float[] kernelZZ) {

        int i, j, k;
        int offsetX, offsetY, offsetZ;
        int sliceSize = iExtents[0] * iExtents[1];
        int volSize = sliceSize * iExtents[2];
        int xDim = iExtents[0];
        int xKDim = kExtents[0];
        int indexY;
        int stepY, stepZ;
        int startX, startY, startZ;
        int endX, endY, endZ;
        int count;
        double sumX;
        double sumY;
        double sumZ;
        double sumXX;
        double sumXY;
        double sumYY;
        double sumXZ;
        double sumYZ;
        double sumZZ;
        double normX = 0;
        double normY = 0;
        double normZ = 0;
        double normXX = 0;
        double normXY = 0;
        double normYY = 0;
        double normXZ = 0;
        double normYZ = 0;
        double normZZ = 0;
        double ptX;
        double ptY;
        double ptZ;
        double ptXX;
        double ptXY;
        double ptYY;
        double ptXZ;
        double ptYZ;
        double ptZZ;

        offsetX = (pix % xDim) - (kExtents[0] / 2);
        offsetY = ((pix % sliceSize) / xDim) - (kExtents[1] / 2);
        offsetZ = (pix / (sliceSize)) - (kExtents[2] / 2);

        count = 0;
        sumX = 0;
        sumY = 0;
        sumZ = 0;
        sumXX = 0;
        sumXY = 0;
        sumYY = 0;
        sumXZ = 0;
        sumYZ = 0;
        sumZZ = 0;
        indexY = offsetY * xDim;
        stepY = kExtents[1] * xDim;
        stepZ = kExtents[2] * sliceSize;
        startZ = offsetZ * sliceSize;
        endZ = startZ + stepZ;

        for (k = startZ; k < endZ; k += sliceSize) {

            if ((k >= 0) && (k < volSize)) {
                startY = k + indexY;
                endY = startY + stepY;

                for (j = startY; j < endY; j += xDim) {

                    if (((j - k) >= 0) && ((j - k) < sliceSize)) {
                        startX = j + offsetX;
                        endX = startX + xKDim;

                        for (i = startX; i < endX; i++) {

                            if (((i - j) >= 0) && ((i - j) < xDim)) {
                                sumX += kernelX[count] * image[i];
                                sumY += kernelY[count] * image[i];
                                sumZ += kernelZ[count] * image[i];
                                sumXX += kernelXX[count] * image[i];
                                sumXY += kernelXY[count] * image[i];
                                sumYY += kernelYY[count] * image[i];
                                sumXZ += kernelXZ[count] * image[i];
                                sumYZ += kernelYZ[count] * image[i];
                                sumZZ += kernelZZ[count] * image[i];

                                if (kernelX[count] >= 0) {
                                    normX += kernelX[count];
                                } else {
                                    normX += -kernelX[count];
                                }
                                
                                if (kernelY[count] >= 0) {
                                    normY += kernelY[count];
                                } else {
                                    normY += -kernelY[count];
                                }
                                
                                if (kernelZ[count] >= 0) {
                                    normZ += kernelZ[count];
                                } else {
                                    normZ += -kernelZ[count];
                                }
                                
                                if (kernelXX[count] >= 0) {
                                    normXX += kernelXX[count];
                                } else {
                                    normXX += -kernelXX[count];
                                }
                                
                                if (kernelXY[count] >= 0) {
                                    normXY += kernelXY[count];
                                } else {
                                    normXY += -kernelXY[count];
                                }
                                
                                if (kernelYY[count] >= 0) {
                                    normYY += kernelYY[count];
                                } else {
                                    normYY += -kernelYY[count];
                                }
                                
                                if (kernelXZ[count] >= 0) {
                                    normXZ += kernelXZ[count];
                                } else {
                                    normXZ += -kernelXZ[count];
                                }
                                
                                if (kernelYZ[count] >= 0) {
                                    normYZ += kernelYZ[count];
                                } else {
                                    normYZ += -kernelYZ[count];
                                }
                                
                                if (kernelZZ[count] >= 0) {
                                    normZZ += kernelZZ[count];
                                } else {
                                    normZZ += -kernelZZ[count];
                                }
                            }

                            count++;
                        }
                    }else{
                        count += kExtents[0];
                    }
                }
            }else{
                count += kExtents[0]*kExtents[1];
            }
        }

        if ((normX > 0) && (normY > 0) && (normZ > 0) && (normXX > 0) && (normXY > 0) && (normYY > 0) &&
            (normXZ > 0) && (normYZ > 0) && (normZZ > 0)) {
            ptX = sumX/normX;
            ptY = sumY/normY;
            ptZ = sumZ/normZ;
            ptXX = sumXX/normXX;
            ptXY = sumXY/normXY;
            ptYY = sumYY/normYY;
            ptXZ = sumXZ/normXZ;
            ptYZ = sumYZ/normYZ;
            ptZZ = sumZZ/normZZ;
            return (float)((ptX * ptX * ptXX) + (2.0 * ptX * ptY * ptXY) + (ptY * ptY * ptYY) +
                    (2.0 * ptX * ptZ * ptXZ) + (2.0 * ptY * ptZ * ptYZ) + (ptZ * ptZ * ptZZ));
        } else {
            return 0;
        }
    }
    
    /**
     * A static function that convolves a kernel with an image at a position.
     *
     * @param   pix       index indicating location of convolution
     * @param   iExtents  image dimensions
     * @param   image     image data
     * @param   kExtents  kernel dimensions
     * @param   kernelX   kernel data
     * @param   kernelY   kernel data
     * @param   kernelZ   kernel data
     * @param   kernelXXX kernel data
     * @param   kernelXXY kernel data
     * @param   kernelXYY kernel data
     * @param   kernelYYY kernel data
     * @param   kernelXXZ kernel data
     * @param   kernelXZZ kernel data
     * @param   kernelXYZ kernel data
     * @parma   kernelYYZ kernel data
     * @param   kernelYZZ kernel data
     * @param   kernelZZZ kernel data
     *
     * @return  the value of the pixel after convolution with the kernel
     */
    public static final float convolve3DPtNMSE(int pix, int[] iExtents, float[] image, int[] kExtents,
                                              float[] kernelX, float[] kernelY, float[] kernelZ,
                                              float[] kernelXXX, float[] kernelXXY, float[] kernelXYY,
                                              float[] kernelYYY, float[] kernelXXZ, float[] kernelXZZ,
                                              float[] kernelXYZ, float[] kernelYYZ, float[] kernelYZZ,
                                              float[] kernelZZZ) {

        int i, j, k;
        int offsetX, offsetY, offsetZ;
        int sliceSize = iExtents[0] * iExtents[1];
        int volSize = sliceSize * iExtents[2];
        int xDim = iExtents[0];
        int xKDim = kExtents[0];
        int indexY;
        int stepY, stepZ;
        int startX, startY, startZ;
        int endX, endY, endZ;
        int count;
        double sumX;
        double sumY;
        double sumZ;
        double sumXXX;
        double sumXXY;
        double sumXYY;
        double sumYYY;
        double sumXXZ;
        double sumXZZ;
        double sumXYZ;
        double sumYYZ;
        double sumYZZ;
        double sumZZZ;
        double normX = 0;
        double normY = 0;
        double normZ = 0;
        double normXXX = 0;
        double normXXY = 0;
        double normXYY = 0;
        double normYYY = 0;
        double normXXZ = 0;
        double normXZZ = 0;
        double normXYZ = 0;
        double normYYZ = 0;
        double normYZZ = 0;
        double normZZZ = 0;
        double ptX;
        double ptY;
        double ptZ;
        double ptXXX;
        double ptXXY;
        double ptXYY;
        double ptYYY;
        double ptXXZ;
        double ptXZZ;
        double ptXYZ;
        double ptYYZ;
        double ptYZZ;
        double ptZZZ;

        offsetX = (pix % xDim) - (kExtents[0] / 2);
        offsetY = ((pix % sliceSize) / xDim) - (kExtents[1] / 2);
        offsetZ = (pix / (sliceSize)) - (kExtents[2] / 2);

        count = 0;
        sumX = 0;
        sumY = 0;
        sumZ = 0;
        sumXXX = 0;
        sumXXY = 0;
        sumXYY = 0;
        sumYYY = 0;
        sumXXZ = 0;
        sumXZZ = 0;
        sumXYZ = 0;
        sumYYZ = 0;
        sumYZZ = 0;
        sumZZZ = 0;
        indexY = offsetY * xDim;
        stepY = kExtents[1] * xDim;
        stepZ = kExtents[2] * sliceSize;
        startZ = offsetZ * sliceSize;
        endZ = startZ + stepZ;

        for (k = startZ; k < endZ; k += sliceSize) {

            if ((k >= 0) && (k < volSize)) {
                startY = k + indexY;
                endY = startY + stepY;

                for (j = startY; j < endY; j += xDim) {

                    if (((j - k) >= 0) && ((j - k) < sliceSize)) {
                        startX = j + offsetX;
                        endX = startX + xKDim;

                        for (i = startX; i < endX; i++) {

                            if (((i - j) >= 0) && ((i - j) < xDim)) {
                                sumX += kernelX[count] * image[i];
                                sumY += kernelY[count] * image[i];
                                sumZ += kernelZ[count] * image[i];
                                sumXXX += kernelXXX[count] * image[i];
                                sumXXY += kernelXXY[count] * image[i];
                                sumXYY += kernelXYY[count] * image[i];
                                sumYYY += kernelYYY[count] * image[i];
                                sumXXZ += kernelXXZ[count] * image[i];
                                sumXZZ += kernelXZZ[count] * image[i];
                                sumXYZ += kernelXYZ[count] * image[i];
                                sumYYZ += kernelYYZ[count] * image[i];
                                sumYZZ += kernelYZZ[count] * image[i];
                                sumZZZ += kernelZZZ[count] * image[i];

                                if (kernelX[count] >= 0) {
                                    normX += kernelX[count];
                                } else {
                                    normX += -kernelX[count];
                                }
                                
                                if (kernelY[count] >= 0) {
                                    normY += kernelY[count];
                                } else {
                                    normY += -kernelY[count];
                                }
                                
                                if (kernelZ[count] >= 0) {
                                    normZ += kernelZ[count];
                                } else {
                                    normZ += -kernelZ[count];
                                }
                                
                                if (kernelXXX[count] >= 0) {
                                    normXXX += kernelXXX[count];
                                } else {
                                    normXXX += -kernelXXX[count];
                                }
                                
                                if (kernelXXY[count] >= 0) {
                                    normXXY += kernelXXY[count];
                                } else {
                                    normXXY += -kernelXXY[count];
                                }
                                
                                if (kernelXYY[count] >= 0) {
                                    normXYY += kernelXYY[count];
                                } else {
                                    normXYY += -kernelXYY[count];
                                }
                                
                                if (kernelYYY[count] >= 0) {
                                    normYYY += kernelYYY[count];
                                } else {
                                    normYYY += -kernelYYY[count];
                                }
                                
                                if (kernelXXZ[count] >= 0) {
                                    normXXZ += kernelXXZ[count];
                                } else {
                                    normXXZ += -kernelXXZ[count];
                                }
                                
                                if (kernelXZZ[count] >= 0) {
                                    normXZZ += kernelXZZ[count];
                                } else {
                                    normXZZ += -kernelXZZ[count];
                                }
                                
                                if (kernelXYZ[count] >= 0) {
                                    normXYZ += kernelXYZ[count];
                                } else {
                                    normXYZ += -kernelXYZ[count];
                                }
                                
                                if (kernelYYZ[count] >= 0) {
                                    normYYZ += kernelYYZ[count];
                                } else {
                                    normYYZ += -kernelYYZ[count];
                                }
                                
                                if (kernelYZZ[count] >= 0) {
                                    normYZZ += kernelYZZ[count];
                                } else {
                                    normYZZ += -kernelYZZ[count];
                                }
                                
                                if (kernelZZZ[count] >= 0) {
                                    normZZZ += kernelZZZ[count];
                                } else {
                                    normZZZ += -kernelZZZ[count];
                                }
                            }

                            count++;
                        }
                    }else{
                        count += kExtents[0];
                    }
                }
            }else{
                count += kExtents[0]*kExtents[1];
            }
        }

        if ((normX > 0) && (normY > 0) && (normZ > 0) && (normXXX > 0) && (normXXY > 0) && (normXYY > 0) &&
            (normYYY > 0) && (normXXZ > 0) && (normXZZ > 0) && (normXYZ > 0) && (normYYZ > 0) && (normYZZ > 0) &&
            (normZZZ > 0)) {
            ptX = sumX/normX;
            ptY = sumY/normY;
            ptZ = sumZ/normZ;
            ptXXX = sumXXX/normXXX;
            ptXXY = sumXXY/normXXY;
            ptXYY = sumXYY/normXYY;
            ptYYY = sumYYY/normYYY;
            ptXXZ = sumXXZ/normXXZ;
            ptXZZ = sumXZZ/normXZZ;
            ptXYZ = sumXYZ/normXYZ;
            ptYYZ = sumYYZ/normYYZ;
            ptYZZ = sumYZZ/normYZZ;
            ptZZZ = sumZZZ/normZZZ;
            return (float)((ptX * ptX * ptX * ptXXX) + (3.0 * ptX * ptX * ptY * ptXXY) +
                    (3.0 * ptX * ptY * ptY * ptXYY) + (ptY * ptY * ptY * ptYYY) +
                    (3.0 * ptX * ptX * ptZ * ptXXZ) + (3.0 * ptX * ptZ * ptZ * ptXZZ) +
                    (6.0 * ptX * ptY * ptZ * ptXYZ) + (3.0 * ptY * ptY * ptZ * ptYYZ) +
                    (3.0 * ptY * ptZ * ptZ * ptYZZ) + (ptZ * ptZ * ptZ * ptZZZ));
        } else {
            return 0;
        }
    }

    /**
     * 
     * @param start
     * @param end
     * @param lpp		length per voxel, for example 4 for RGB image. 
     * @param iImage
     * @param iExtents
     * @param kExtents
     * @param kernel
     * @param oImage
     */
    public final static void convolveBlock3D(int start, int end, int lpv, float[] iImage, 
    		int[] iExtents, int[] kExtents, float[] kernel, ModelImage oImage){
    	int sliceSize = iExtents[0]*iExtents[1]*lpv;
    	int volumeSize = sliceSize * iExtents[2];
    	int offset = iExtents[0]*lpv;
    	int kSliceSize = kExtents[0]*kExtents[1];
    	for(int pix = start; pix < end; pix++){
            if((lpv > 1) && ((pix % offset)%lpv) == 0){
            	oImage.set(pix, iImage[pix]);
            	continue;
            }
            int offsetZ = (pix / (sliceSize))-kExtents[2]/2;
            int offsetY = (pix % sliceSize)/offset - kExtents[1]/2;
            int offsetX = (pix % offset) - lpv*kExtents[0]/2;
            int startZ = offsetZ * sliceSize;
            int endZ = startZ + sliceSize*kExtents[2];
            int indexY = offsetY*offset;
            int stepY = kExtents[1]*offset;
//            for(;ic < lpv; ic++){
            	int count = 0;
            	double sum = 0;
            	double norm = 0;
            	for(int ik = startZ; ik < endZ; ik+=sliceSize){
            		if(ik >= 0 && ik < volumeSize){
                    	int startY = ik + indexY;
                    	int endY = startY + stepY;
            			for(int ij = startY; ij < endY; ij+=offset){
            				if((ij-ik) >= 0 && (ij-ik) < sliceSize){
            					int startX = ij + offsetX;
            					int endX = startX + kExtents[0]*lpv;
            					for(int ii = startX; ii < endX; ii+=lpv){
            						if((ii-ij) >= 0 && (ii-ij) < offset){
            							sum += iImage[ii]*kernel[count];
            							norm += kernel[count];
            						}
            						count++;
            					}
            				}else{
            					count += kExtents[0];
            				}
            			}
            		}else{
            			count += kSliceSize;
            		}
            	}
            	if(norm != 0){
            		oImage.set(pix, sum/norm);
            	}else{
            		oImage.set(pix, 0);
            	}
            }
//    	}
    }
    
    /**
     * A static function that convolves a kernel with an image at a position.
     *
     * @param   pt        floating point indicating location of convolution
     * @param   iExtents  image dimensions
     * @param   image     image data
     * @param   kExtents  kernel dimensions
     * @param   kernel    kernel data
     *
     * @return  the value of the pixel after convolution with the kernel
     */
    public static final float convolve3DPt(float[] pt, int[] iExtents, float[] image, int[] kExtents, float[] kernel) {

        int i, j, k;
        int offsetX, offsetY, offsetZ;
        float dx, dy, dz;
        int sliceSize = iExtents[0] * iExtents[1];
        int volSize = iExtents[2] * sliceSize;
        int xDim = iExtents[0];
        int xKDim = kExtents[0];
        int indexY;
        int stepY, stepZ;
        int startX, startY, startZ;
        int endX, endY, endZ;
        int count;
        double sum;
        double norm = 0;

        dx = pt[0] - (int) pt[0];
        dy = pt[1] - (int) pt[1];
        dz = pt[2] - (int) pt[2];

        offsetX = (int) pt[0] - (kExtents[0] / 2);
        offsetY = (int) pt[1] - (kExtents[1] / 2);
        offsetZ = (int) pt[2] - (kExtents[2] / 2);

        count = 0;
        sum = 0;
        indexY = offsetY * xDim;
        stepY = kExtents[1] * xDim;
        stepZ = kExtents[2] * sliceSize;
        startZ = offsetZ * sliceSize;
        endZ = startZ + stepZ;

        for (k = startZ; k < endZ; k += sliceSize) {
            startY = k + indexY;
            endY = startY + stepY;

            for (j = startY; j < endY; j += xDim) {
                startX = j + offsetX;
                endX = startX + xKDim;

                for (i = startX; i < endX; i++) {

                    if ((k >= 0) && (k < volSize) && ((j - k) >= 0) && ((j - k) < sliceSize) && ((i - j) >= 0) &&
                            ((i - j) < xDim)) {
                        
                        sum += kernel[count] * getTrilinear(i, dx, dy, dz, iExtents, image);

                        if (kernel[count] >= 0) {
                            norm += kernel[count];
                        } else {
                            norm += -kernel[count];
                        }
                    }
                    count++;
                }
            }
        }

        if (norm > 0) {
            return (float)(sum / norm);
        } else {
            return 0;
        }
    }

    /**
     * A static function that convolves a kernel with an image at a position. This version seems to just be used by
     * AlgorithmLapMedianess.
     *
     * @see     gov.nih.mipav.model.algorithms.AlgorithmLapMedianess
     *
     * @param   pix       index indicating location of convolution
     * @param   iExtents  image dimensions
     * @param   image     image data
     * @param   kExtents  kernel dimensions
     * @param   kernel    kernel data
     *
     * @return  the value of the pixel after convolution with the kernel
     */
    public static final float convolve3DPtMed(int pix, int[] iExtents, float[] image, int[] kExtents, float[] kernel) {

        int i, j, k;
        int offsetX, offsetY, offsetZ;
        int xDim = iExtents[0];
        int yDim = iExtents[1];
        int zDim = iExtents[2];
        int xKDim = kExtents[0];
        int yKDim = kExtents[1];
        int zKDim = kExtents[2];
        int sliceSize = xDim * yDim;
        int volSize = sliceSize * zDim;

        int startX, startY, startZ;
        int endX, endY, endZ;
        int count;
        double sum;
        int remainder;

        remainder = pix % sliceSize;
        offsetX = (remainder % xDim) - (xKDim / 2);
        offsetY = (remainder / xDim) - (yKDim / 2);
        offsetZ = (pix / sliceSize) - (zKDim / 2);

        sum = 0;
        count = 0;
        startZ = offsetZ * sliceSize;
        endZ = startZ + (zKDim * sliceSize);

        for (k = startZ; k < endZ; k += sliceSize) {
            startY = k + (offsetY * xDim);
            endY = startY + (yKDim * xDim);

            for (j = startY; j < endY; j += xDim) {
                startX = j + offsetX;
                endX = startX + xKDim;

                for (i = startX; i < endX; i++) {

                    if ((k >= 0) && (k < volSize) && ((j - k) >= 0) && ((j - k) < sliceSize) && ((i - j) >= 0) &&
                            ((i - j) < xDim)) {
                        sum += kernel[count] * image[i];
                    }

                    count++;
                }
            }
        }

        return (float)sum;
    }

    /**
     * A static function that convolves a kernel with an RGB image at a position.
     *
     * @param   pix       index indicating location of convolution
     * @param   iExtents  image dimensions
     * @param   image     image data
     * @param   kExtents  kernel dimensions
     * @param   kernel    kernel data
     *
     * @return  the value of the pixel after convolution with the kernel
     */
    public static final float convolve3DRGBPt(int pix, int[] iExtents, float[] image, int[] kExtents, float[] kernel) {

        int i, j, k;
        int offsetX, offsetY, offsetZ;

        int sliceSize = iExtents[0] * iExtents[1] * 4;
        int volSize = iExtents[2] * sliceSize;
        int xDim = iExtents[0];
        int xKDim = kExtents[0];
        int indexY;
        int stepY, stepZ;
        int startX, startY, startZ;
        int endX, endY, endZ;
        int count;
        double sum;
        double norm = 0;
        int offset = 4 * xDim;

        offsetX = (pix % offset) - (kExtents[0] / 2 * 4);
        offsetY = ((pix % sliceSize) / offset) - (kExtents[1] / 2);
        offsetZ = (pix / (sliceSize)) - (kExtents[2] / 2);

        count = 0;
        sum = 0;
        indexY = offsetY * offset;
        stepY = kExtents[1] * offset;
        stepZ = kExtents[2] * sliceSize;
        startZ = offsetZ * sliceSize;
        endZ = startZ + stepZ;

        for (k = startZ; k < endZ; k += sliceSize) {
            startY = k + indexY;
            endY = startY + stepY;

            for (j = startY; j < endY; j += offset) {
                startX = j + offsetX;
                endX = startX + (xKDim * 4);

                for (i = startX; i < endX; i += 4) {

                    if ((k >= 0) && (k < volSize) && ((j - k) >= 0) && ((j - k) < sliceSize) && ((i - j) >= 0) &&
                            ((i - j) < offset)) {
                        sum += kernel[count] * image[i];

                        if (kernel[count] >= 0) {
                            norm += kernel[count];
                        } else {
                            norm += -kernel[count];
                        }
                    }

                    count++;
                }
            }
        }

        if (norm > 0) {
            return (float)(sum / norm);
        } else {
            return 0;
        }
    }
    
    /**
     * A static function that convolves a kernel with a CIELab image at a position.   The CIE76 intensity difference between the
     * center pixel and the indexed pixel is used in a Gaussian in the weighing.
     * The intensity Gaussian is (1.0/(Math.sqrt(2.0 * Math.PI) * intensitySigma))* exp(-intensityDifference**2/intenDenom)
     * Since the same (1.0/(Math.sqrt(2.0 * Math.PI) * intensitySigma) term would appear in every weight term, it would cancel
     * out by appearing in both the numerator and denominator in (sum / norm) and so can be omitted.
     *
     * @param   pix       index indicating location of convolution
     * @param   iExtents  image dimensions
     * @param   image     image data
     * @param   kExtents  kernel dimensions
     * @param   kernel    kernel data
     * @param   intensityGaussianDenom
     *
     * @return  the L*,a*,b* of the pixel after convolution with the kernel
     */
    public static final float[] convolveBilateral3DCIELabPt(int pix, int[] iExtents, float[] image, int[] kExtents, float[] kernel,
                                                            double intensityGaussianDenom) {

        int i, j, k;
        int offsetX, offsetY, offsetZ;

        int sliceSize = iExtents[0] * iExtents[1] * 4;
        int volSize = iExtents[2] * sliceSize;
        int xDim = iExtents[0];
        int xKDim = kExtents[0];
        int indexY;
        int stepY, stepZ;
        int startX, startY, startZ;
        int endX, endY, endZ;
        int count;
        double sumL;
        double suma;
        double sumb;
        double norm = 0;
        int offset = 4 * xDim;

        offsetX = (pix % offset) - (kExtents[0] / 2 * 4);
        offsetY = ((pix % sliceSize) / offset) - (kExtents[1] / 2);
        offsetZ = (pix / (sliceSize)) - (kExtents[2] / 2);

        count = 0;
        sumL = 0;
        suma = 0;
        sumb = 0;
        indexY = offsetY * offset;
        stepY = kExtents[1] * offset;
        stepZ = kExtents[2] * sliceSize;
        startZ = offsetZ * sliceSize;
        endZ = startZ + stepZ;
        float centerLValue = image[pix+1];
        float centeraValue = image[pix+2];
        float centerbValue = image[pix+3];
        float LValue;
        float aValue;
        float bValue;
        double intensityGaussian;
        float LDifference;
        float aDifference;
        float bDifference;
        double differenceSquared;
        double weight;
        float buf[] = new float[3];

        for (k = startZ; k < endZ; k += sliceSize) {
            startY = k + indexY;
            endY = startY + stepY;

            for (j = startY; j < endY; j += offset) {
                startX = j + offsetX;
                endX = startX + (xKDim * 4);

                for (i = startX; i < endX; i += 4) {

                    if ((k >= 0) && (k < volSize) && ((j - k) >= 0) && ((j - k) < sliceSize) && ((i - j) >= 0) &&
                            ((i - j) < offset)) {
                        LValue = image[i+1];
                        aValue = image[i+2];
                        bValue = image[i+3];
                        LDifference = LValue - centerLValue;
                        aDifference = aValue - centeraValue;
                        bDifference = bValue - centerbValue;
                        differenceSquared = (LDifference*LDifference + aDifference*aDifference + bDifference*bDifference);
                        intensityGaussian = Math.exp(-differenceSquared/intensityGaussianDenom);
                        weight = kernel[count] * intensityGaussian;
                        sumL += weight * LValue;
                        suma += weight * aValue;
                        sumb += weight * bValue;

                        if (weight >= 0) {
                            norm += weight;
                        } else {
                            norm += -weight;
                        }
                        
                    }

                    count++;
                }
            }
        }

        if (norm > 0) {
            buf[0] = (float)(sumL/norm);
            buf[1] = (float)(suma/norm);
            buf[2] = (float)(sumb/norm);
        } 
        return buf;
    }
    
    /**
     * A static function that convolves a kernel with an RGB image at a position.
     *
     * @param   pix       index indicating location of convolution
     * @param   iExtents  image dimensions
     * @param   image     image data
     * @param   kExtents  kernel dimensions
     * @param   kernelX   kernel data
     * @param   kernelY   kernel data
     * @param   kernelZ   kernel data
     *
     * @return  the value of the pixel after convolution with the kernel
     */
    public static final float convolve3DRGBPtXYZ(int pix, int[] iExtents, float[] image, int[] kExtents,
                                                 float[] kernelX, float[] kernelY, float[] kernelZ) {

        int i, j, k;
        int offsetX, offsetY, offsetZ;

        int sliceSize = iExtents[0] * iExtents[1] * 4;
        int volSize = iExtents[2] * sliceSize;
        int xDim = iExtents[0];
        int xKDim = kExtents[0];
        int indexY;
        int stepY, stepZ;
        int startX, startY, startZ;
        int endX, endY, endZ;
        int count;
        double sumX;
        double sumY;
        double sumZ;
        double normX = 0;
        double normY = 0;
        double normZ = 0;
        double ptX;
        double ptY;
        double ptZ;
        int offset = 4 * xDim;

        offsetX = (pix % offset) - (kExtents[0] / 2 * 4);
        offsetY = ((pix % sliceSize) / offset) - (kExtents[1] / 2);
        offsetZ = (pix / (sliceSize)) - (kExtents[2] / 2);

        count = 0;
        sumX = 0;
        sumY = 0;
        sumZ = 0;
        indexY = offsetY * offset;
        stepY = kExtents[1] * offset;
        stepZ = kExtents[2] * sliceSize;
        startZ = offsetZ * sliceSize;
        endZ = startZ + stepZ;

        for (k = startZ; k < endZ; k += sliceSize) {
            startY = k + indexY;
            endY = startY + stepY;

            for (j = startY; j < endY; j += offset) {
                startX = j + offsetX;
                endX = startX + (xKDim * 4);

                for (i = startX; i < endX; i += 4) {

                    if ((k >= 0) && (k < volSize) && ((j - k) >= 0) && ((j - k) < sliceSize) && ((i - j) >= 0) &&
                            ((i - j) < offset)) {
                        sumX += kernelX[count] * image[i];
                        sumY += kernelY[count] * image[i];
                        sumZ += kernelZ[count] * image[i];

                        if (kernelX[count] >= 0) {
                            normX += kernelX[count];
                        } else {
                            normX += -kernelX[count];
                        }
                        
                        if (kernelY[count] >= 0) {
                            normY += kernelY[count];
                        } else {
                            normY += -kernelY[count];
                        }
                        
                        if (kernelZ[count] >= 0) {
                            normZ += kernelZ[count];
                        } else {
                            normZ += -kernelZ[count];
                        }
                    }

                    count++;
                }
            }
        }

        if ((normX > 0) && (normY > 0) && (normZ > 0)) {
            ptX = sumX/normX;
            ptY = sumY/normY;
            ptZ = sumZ/normZ;
            return (float)Math.sqrt(ptX*ptX + ptY*ptY + ptZ*ptZ);
        } else {
            return 0;
        }
    }

    /**
     * A static function that convolves a kernel with an image at a position. The convolution is performed only if the
     * whole kernel fits inside the image.
     *
     * @param   pix       index indicating location of convolution
     * @param   iExtents  image dimensions
     * @param   image     image data
     * @param   kExtents  kernel dimensions
     * @param   kernel    kernel data
     *
     * @return  the value of the pixel after convolution with the kernel or 0 if the kernel does not fit wholly within
     *          the image
     */
    public static final float convolveWhole2DPt(int pix, int[] iExtents, float[] image, int[] kExtents,
                                                float[] kernel) {

        int i, j;
        int offsetX, offsetY;
        int xDim = iExtents[0];
        int yDim = iExtents[1];
        int xKDim = kExtents[0];
        int yKDim = kExtents[1];

        int startX, startY;
        int endX, endY;
        int count;
        double sum;
        double norm = 0;

        offsetX = (pix % xDim) - (xKDim / 2);
        offsetY = (pix / xDim) - (yKDim / 2);

        for (int p = 0; p < kernel.length; p++) {
            norm += Math.abs(kernel[p]);
        }

        if ((offsetX >= 0) && ((offsetX + xKDim) <= xDim) && (offsetY >= 0) && ((offsetY + yKDim) <= yDim)) {
            sum = 0;
            count = 0;
            startY = offsetY * xDim;
            endY = startY + (yKDim * xDim);

            for (j = startY; j < endY; j += xDim) {
                startX = j + offsetX;
                endX = startX + xKDim;

                for (i = startX; i < endX; i++) {
                    sum += kernel[count] * image[i];
                    count++;
                }
            }

            if (norm > 0) {
                return (float)(sum / norm);
            } else {
                return 0;
            }
        } else {
            return 0;
        }
    }

    /**
     * A static function that convolves a kernel with an image at a position The convolution is performed only if the
     * whole kernel fits inside the image.
     *
     * @param   pix       index indicating location of convolution
     * @param   iExtents  image dimensions
     * @param   image     image data
     * @param   kExtents  kernel dimensions
     * @param   kernel    kernel data
     *
     * @return  the value of the pixel after convolution with the kernel or 0 if the kernel does not fit wholly within
     *          the image
     */
    public static final float convolveWhole3DPt(int pix, int[] iExtents, float[] image, int[] kExtents,
                                                float[] kernel) {

        int i, j, k;
        int offsetX, offsetY, offsetZ;
        int xDim = iExtents[0];
        int yDim = iExtents[1];
        int zDim = iExtents[2];
        int xKDim = kExtents[0];
        int yKDim = kExtents[1];
        int zKDim = kExtents[2];
        int sliceSize = xDim * yDim;

        int startX, startY, startZ;
        int endX, endY, endZ;
        int count;
        double sum;
        double norm = 0;
        int remainder;

        remainder = pix % sliceSize;
        offsetX = (remainder % xDim) - (xKDim / 2);
        offsetY = (remainder / xDim) - (yKDim / 2);
        offsetZ = (pix / sliceSize) - (zKDim / 2);

        for (int p = 0; p < kernel.length; p++) {
            norm += Math.abs(kernel[p]);
        }

        if ((offsetX >= 0) && ((offsetX + xKDim) <= xDim) && (offsetY >= 0) && ((offsetY + yKDim) <= yDim) &&
                (offsetZ >= 0) && ((offsetZ + zKDim) <= zDim)) {
            sum = 0;
            count = 0;
            startZ = offsetZ * sliceSize;
            endZ = startZ + (zKDim * sliceSize);

            for (k = startZ; k < endZ; k += sliceSize) {
                startY = k + (offsetY * xDim);
                endY = startY + (yKDim * xDim);

                for (j = startY; j < endY; j += xDim) {
                    startX = j + offsetX;
                    endX = startX + xKDim;

                    for (i = startX; i < endX; i++) {
                        sum += kernel[count] * image[i];
                        count++;
                    }
                }
            }

            if (norm > 0) {
                return (float)(sum / norm);
            } else {
                return 0;
            }
        } else {
            return 0;
        }
    }
    
    /**
     * A static function that convolves a kernel with an image at a position.
     *
     * @param   kExtents  kernel dimensions
     * @param   kernel    kernel data
     *
     * @return  the value of the pixel after convolution with the kernel
     */
    public static final float convolve3DPt(Vector3f kPt, ModelImage kImage, int[] kExtents, float[] kernel) {
        int xKDim = kExtents.length > 0 ? kExtents[0] : 1;
        int yKDim = kExtents.length > 1 ? kExtents[1] : 1;
        int zKDim = kExtents.length > 2 ? kExtents[2] : 1;
        
        int offsetX = (xKDim / 2);
        int offsetY = (yKDim / 2);
        int offsetZ = (zKDim / 2);

        int startX = (int)(kPt.X - offsetX);
        int endX = (int)(kPt.X + offsetX);
        int startY = (int)(kPt.Y - offsetY);
        int endY = (int)(kPt.Y + offsetY);
        int startZ = (int)(kPt.Z - offsetZ);
        int endZ = (int)(kPt.Z + offsetZ);

        int xDim = kImage.getExtents().length > 0 ? kImage.getExtents()[0] : 1;
        int yDim = kImage.getExtents().length > 1 ? kImage.getExtents()[1] : 1;
        int zDim = kImage.getExtents().length > 2 ? kImage.getExtents()[2] : 1;

        double sum = 0;
        double norm = 0;
        int index;
        for (int z = startZ; z <= endZ; z++ ) {
            for ( int y = startY; y <= endY; y++ ) {
                for ( int x = startX; x <= endX; x++) {
                    if ( (x >= 0) && (x < xDim) && (y >= 0) && (y < yDim) && (z >= 0) && (z < zDim) )
                    {
                        index = (z - startZ) * (xKDim * yKDim) +
                        (y - startY) * xKDim +
                        (x - startX);
                        sum += kernel[index] * kImage.getFloat(x,y,z);

                        norm += Math.abs( kernel[index] );
                    }
                }
            }
        }

        if (norm > 0) {
            return (float)(sum / norm);
        }
        return 0;
    }


    

    private final void convolve2D(int startSlice, int endSlice){
    	boolean color = srcImage.isColorImage();
    	int cFactor = (color)?4:1;
    	int length = cFactor * srcImage.getSliceSize();
    	for (int s = startSlice; s < endSlice; s++) {
			int start = s * length;
			float[] buffer = new float[length];
			try {
				srcImage.exportData(start, length, buffer); // locks and
															// releases lock
			} catch (IOException error) {
				errorCleanUp("Algorithm Convolver: Image(s) locked", false);

				return;
			}

			if (color == true) {

				for (int i = 0, idx = start; (i < length) && !threadStopped; i += 4, idx += 4) {
					progress += 4;
					if ((progress % progressModulus) == 0) {
						fireProgressStateChanged(minProgressValue + (int)(progress/progressModulus));
					}

					if ((entireImage == true) || mask.get(i / 4)) {
						outputBuffer[idx] = buffer[i]; // alpha

						if (red) {
							outputBuffer[idx + 1] = AlgorithmConvolver
									.convolve2DRGBPt(i + 1, srcImage
											.getExtents(), buffer, kExtents,
											kernelBuffer);
						} else {
							outputBuffer[idx + 1] = buffer[i + 1];
						}

						if (green) {
							outputBuffer[idx + 2] = AlgorithmConvolver
									.convolve2DRGBPt(i + 2, srcImage
											.getExtents(), buffer, kExtents,
											kernelBuffer);
						} else {
							outputBuffer[idx + 2] = buffer[i + 2];
						}

						if (blue) {
							outputBuffer[idx + 3] = AlgorithmConvolver
									.convolve2DRGBPt(i + 3, srcImage
											.getExtents(), buffer, kExtents,
											kernelBuffer);
						} else {
							outputBuffer[idx + 3] = buffer[i + 3];
						}
					} else {
						outputBuffer[idx] = buffer[i];
						outputBuffer[idx + 1] = buffer[i + 1];
						outputBuffer[idx + 2] = buffer[i + 2];
						outputBuffer[idx + 3] = buffer[i + 3];
					}
				}
			} else {

				for (int i = 0, idx = start; (i < length) && !threadStopped; i++, idx++) {
					progress++;
					if ((progress % progressModulus) == 0) {
						fireProgressStateChanged(minProgressValue + (int)(progress/progressModulus));
					}

					if ((entireImage == true) || mask.get(i)) {
						outputBuffer[idx] = AlgorithmConvolver.convolve2DPt(i,
								srcImage.getExtents(), buffer, kExtents,
								kernelBuffer);
					} else {
						outputBuffer[idx] = buffer[i];
					}
				}
			}
		}
    }
    
    private final void convolve2DBilateral(int startSlice, int endSlice){
        boolean color = srcImage.isColorImage();
        int cFactor = (color)?4:1;
        int length = cFactor * srcImage.getSliceSize();
        for (int s = startSlice; s < endSlice; s++) {
            int start = s * length;
            float[] buffer = new float[length];
            float resultBuffer[] = null;
            try {
                srcImage.exportData(start, length, buffer); // locks and
                                                            // releases lock
            } catch (IOException error) {
                errorCleanUp("Algorithm Convolver: Image(s) locked", false);

                return;
            }

            if (color == true) {
                for (int i = 0, idx = start; (i < length) && !threadStopped; i += 4, idx += 4) {
                    progress += 4;
                    if ((progress % progressModulus) == 0) {
                        fireProgressStateChanged(minProgressValue + (int)(progress/progressModulus));
                    }

                    if ((entireImage == true) || mask.get(i / 4)) {
                        outputBuffer[idx] = buffer[i]; // alpha
                        resultBuffer = AlgorithmConvolver.convolveBilateral2DCIELabPt(i,
                                srcImage.getExtents(), buffer, kExtents, kernelBuffer, intensityGaussianDenom);
                        outputBuffer[idx+1] = resultBuffer[0];
                        outputBuffer[idx+2] = resultBuffer[1];
                        outputBuffer[idx+3] = resultBuffer[2];
                    } else {
                        outputBuffer[idx] = buffer[i];
                        outputBuffer[idx + 1] = buffer[i + 1];
                        outputBuffer[idx + 2] = buffer[i + 2];
                        outputBuffer[idx + 3] = buffer[i + 3];
                    }
                }
            } else {

                for (int i = 0, idx = start; (i < length) && !threadStopped; i++, idx++) {
                    progress++;
                    if ((progress % progressModulus) == 0) {
                        fireProgressStateChanged(minProgressValue + (int)(progress/progressModulus));
                    }

                    if ((entireImage == true) || mask.get(i)) {
                        outputBuffer[idx] = AlgorithmConvolver.convolveBilateral2DPt(i,
                                srcImage.getExtents(), buffer, kExtents,
                                kernelBuffer, intensityGaussianDenom);
                    } else {
                        outputBuffer[idx] = buffer[i];
                    }
                }
            }
        }
    }
    
    private final void convolve2DXY(int startSlice, int endSlice){
        // Does x and y kernels separately
        boolean color = srcImage.isColorImage();
        int cFactor = (color)?4:1;
        int length = cFactor * srcImage.getSliceSize();
        for (int s = startSlice; s < endSlice; s++) {
            int start = s * length;
            float[] buffer = new float[length];
            try {
                srcImage.exportData(start, length, buffer); // locks and
                                                            // releases lock
            } catch (IOException error) {
                errorCleanUp("Algorithm Convolver: Image(s) locked", false);

                return;
            }

            if (color == true) {

                for (int i = 0, idx = start; (i < length) && !threadStopped; i += 4, idx += 4) {
                    progress += 4;
                    if ((progress % progressModulus) == 0) {
                        fireProgressStateChanged(minProgressValue + (int)(progress/progressModulus));
                    }

                    if ((entireImage == true) || mask.get(i / 4)) {
                        outputBuffer[2*idx] = buffer[i]; // alpha
                        outputBuffer[2*idx+1] = buffer[i];

                        if (red) {
                            outputBuffer[2*idx + 2] = AlgorithmConvolver
                                    .convolve2DRGBPt(i + 1, srcImage
                                            .getExtents(), buffer, kExtents,
                                            kernelBufferX);
                            outputBuffer[2*idx + 3] = AlgorithmConvolver
                            .convolve2DRGBPt(i + 1, srcImage
                                    .getExtents(), buffer, kExtents,
                                    kernelBufferY);
                        } else {
                            outputBuffer[2*idx + 2] = buffer[i + 1];
                            outputBuffer[2*idx + 3] = buffer[i + 1];
                        }

                        if (green) {
                            outputBuffer[2*idx + 4] = AlgorithmConvolver
                                    .convolve2DRGBPt(i + 2, srcImage
                                            .getExtents(), buffer, kExtents,
                                            kernelBufferX);
                            outputBuffer[2*idx + 5] = AlgorithmConvolver
                            .convolve2DRGBPt(i + 2, srcImage
                                    .getExtents(), buffer, kExtents,
                                    kernelBufferY);
                        } else {
                            outputBuffer[2*idx + 4] = buffer[i + 2];
                            outputBuffer[2*idx + 5] = buffer[i + 2];
                        }

                        if (blue) {
                            outputBuffer[2*idx + 6] = AlgorithmConvolver
                                    .convolve2DRGBPt(i + 3, srcImage
                                            .getExtents(), buffer, kExtents,
                                            kernelBufferX);
                            outputBuffer[2*idx + 7] = AlgorithmConvolver
                            .convolve2DRGBPt(i + 3, srcImage
                                    .getExtents(), buffer, kExtents,
                                    kernelBufferY);
                        } else {
                            outputBuffer[2*idx + 6] = buffer[i + 3];
                            outputBuffer[2*idx + 7] = buffer[i + 3];
                        }
                    } else {
                        outputBuffer[2*idx] = buffer[i];
                        outputBuffer[2*idx+1] = buffer[i];
                        outputBuffer[2*idx + 2] = buffer[i + 1];
                        outputBuffer[2*idx + 3] = buffer[i + 1];
                        outputBuffer[2*idx + 4] = buffer[i + 2];
                        outputBuffer[2*idx + 5] = buffer[i + 2];
                        outputBuffer[2*idx + 6] = buffer[i + 3];
                        outputBuffer[2*idx + 7] = buffer[i + 3];
                    }
                }
            } else {

                for (int i = 0, idx = start; (i < length) && !threadStopped; i++, idx++) {
                    progress++;
                    if ((progress % progressModulus) == 0) {
                        fireProgressStateChanged(minProgressValue + (int)(progress/progressModulus));
                    }

                    if ((entireImage == true) || mask.get(i)) {
                        outputBuffer[2*idx] = AlgorithmConvolver.convolve2DPt(i,
                                srcImage.getExtents(), buffer, kExtents,
                                kernelBufferX);
                        outputBuffer[2*idx+1] = AlgorithmConvolver.convolve2DPt(i,
                                srcImage.getExtents(), buffer, kExtents,
                                kernelBufferY);
                    } else {
                        outputBuffer[2*idx] = buffer[i];
                        outputBuffer[2*idx+1] = buffer[i];
                    }
                }
            }
        }
    }
    
    private final void convolve2DSqrtXY(int startSlice, int endSlice){
        boolean color = srcImage.isColorImage();
        int cFactor = (color)?4:1;
        int length = cFactor * srcImage.getSliceSize();
        for (int s = startSlice; s < endSlice; s++) {
            int start = s * length;
            float[] buffer = new float[length];
            try {
                srcImage.exportData(start, length, buffer); // locks and
                                                            // releases lock
            } catch (IOException error) {
                errorCleanUp("Algorithm Convolver: Image(s) locked", false);

                return;
            }

            if (color == true) {

                for (int i = 0, idx = start; (i < length) && !threadStopped; i += 4, idx += 4) {
                    progress += 4;
                    if ((progress % progressModulus) == 0) {
                        fireProgressStateChanged(minProgressValue + (int)(progress/progressModulus));
                    }

                    if ((entireImage == true) || mask.get(i / 4)) {
                        outputBuffer[idx] = buffer[i]; // alpha

                        if (red) {
                            outputBuffer[idx + 1] = AlgorithmConvolver
                                    .convolve2DRGBPtSqrtXY(i + 1, srcImage
                                            .getExtents(), buffer, kExtents,
                                            kernelBufferX, kernelBufferY);
                        } else {
                            outputBuffer[idx + 1] = buffer[i + 1];
                        }

                        if (green) {
                            outputBuffer[idx + 2] = AlgorithmConvolver
                                    .convolve2DRGBPtSqrtXY(i + 2, srcImage
                                            .getExtents(), buffer, kExtents,
                                            kernelBufferX, kernelBufferY);
                        } else {
                            outputBuffer[idx + 2] = buffer[i + 2];
                        }

                        if (blue) {
                            outputBuffer[idx + 3] = AlgorithmConvolver
                                    .convolve2DRGBPtSqrtXY(i + 3, srcImage
                                            .getExtents(), buffer, kExtents,
                                            kernelBufferX, kernelBufferY);
                        } else {
                            outputBuffer[idx + 3] = buffer[i + 3];
                        }
                    } else {
                        outputBuffer[idx] = buffer[i];
                        outputBuffer[idx + 1] = buffer[i + 1];
                        outputBuffer[idx + 2] = buffer[i + 2];
                        outputBuffer[idx + 3] = buffer[i + 3];
                    }
                }
            } else {

                for (int i = 0, idx = start; (i < length) && !threadStopped; i++, idx++) {
                    progress++;
                    if ((progress % progressModulus) == 0) {
                        fireProgressStateChanged(minProgressValue + (int)(progress/progressModulus));
                    }

                    if ((entireImage == true) || mask.get(i)) {
                        outputBuffer[idx] = AlgorithmConvolver.convolve2DPtSqrtXY(i,
                                srcImage.getExtents(), buffer, kExtents,
                                kernelBufferX, kernelBufferY);
                    } else {
                        outputBuffer[idx] = buffer[i];
                    }
                }
            }
        }
    }
    
    private final void convolve2DNMS(int startSlice, int endSlice){
        int length = srcImage.getSliceSize();
        for (int s = startSlice; s < endSlice; s++) {
            int start = s * length;
            float[] buffer = new float[length];
            try {
                srcImage.exportData(start, length, buffer); // locks and
                                                            // releases lock
            } catch (IOException error) {
                errorCleanUp("Algorithm Convolver: Image(s) locked", false);

                return;
            }

            for (int i = 0, idx = start; (i < length) && !threadStopped; i++, idx++) {
                progress++;
                if ((progress % progressModulus) == 0) {
                    fireProgressStateChanged(minProgressValue + (int)(progress/progressModulus));
                }

                if ((entireImage == true) || mask.get(i)) {
                    outputBuffer[idx] = AlgorithmConvolver.convolve2DPtNMS(i,
                            srcImage.getExtents(), buffer, kExtents,
                            kernelBufferX, kernelBufferY, kernelBufferXX,
                            kernelBufferXY, kernelBufferYY);
                } else {
                    outputBuffer[idx] = buffer[i];
                }
            }
        }
    }
    
    private final void convolve2DNMSE(int startSlice, int endSlice){
        int length = srcImage.getSliceSize();
        for (int s = startSlice; s < endSlice; s++) {
            int start = s * length;
            float[] buffer = new float[length];
            try {
                srcImage.exportData(start, length, buffer); // locks and
                                                            // releases lock
            } catch (IOException error) {
                errorCleanUp("Algorithm Convolver: Image(s) locked", false);

                return;
            }

            for (int i = 0, idx = start; (i < length) && !threadStopped; i++, idx++) {
                progress++;
                if ((progress % progressModulus) == 0) {
                    fireProgressStateChanged(minProgressValue + (int)(progress/progressModulus));
                }

                if ((entireImage == true) || mask.get(i)) {
                    outputBuffer[2*idx] = AlgorithmConvolver.convolve2DPtNMS(i,
                            srcImage.getExtents(), buffer, kExtents,
                            kernelBufferX, kernelBufferY, kernelBufferXX,
                            kernelBufferXY, kernelBufferYY);
                    outputBuffer[2*idx+1] = AlgorithmConvolver.convolve2DPtNMSE(i,
                            srcImage.getExtents(), buffer, kExtents,
                            kernelBufferX, kernelBufferY, kernelBufferXXX,
                            kernelBufferXXY, kernelBufferXYY, kernelBufferYYY);
                } else {
                    outputBuffer[2*idx] = buffer[i];
                    outputBuffer[2*idx+1] = buffer[i];
                }
            }
        }
    }
    
    private final void convolve3D(int start, int end, float[] iImage, int index){
    	if(srcImage.isColorImage()){
    		for (int i = start; (i < end) && !threadStopped; i += 4) {
    			progress += 4;
    			if ((progress % progressModulus) == 0) {
    				fireProgressStateChanged(minProgressValue + (int)(progress/progressModulus));
    			}

    			if ((entireImage == true) || mask.get(i / 4)) {
    				outputBuffer[i+index] = iImage[i]; // alpha

    				if (red) {
    					outputBuffer[i+index + 1] = AlgorithmConvolver
    							.convolve3DRGBPt(i + 1, srcImage
    									.getExtents(), iImage, kExtents,
    									kernelBuffer);
    				} else {
    					outputBuffer[i+index + 1] = iImage[i + 1];
    				}

    				if (green) {
    					outputBuffer[i+index + 2] = AlgorithmConvolver
    							.convolve3DRGBPt(i + 2, srcImage
    									.getExtents(), iImage, kExtents,
    									kernelBuffer);
    				} else {
    					outputBuffer[i+index + 2] = iImage[i + 2];
    				}

    				if (blue) {
    					outputBuffer[i+index + 3] = AlgorithmConvolver
    							.convolve3DRGBPt(i + 3, srcImage
    									.getExtents(), iImage, kExtents,
    									kernelBuffer);
    				} else {
    					outputBuffer[i+index + 3] = iImage[i + 3];
    				}
    			} else {
    				outputBuffer[i+index] = iImage[i];
    				outputBuffer[i+index + 1] = iImage[i + 1];
    				outputBuffer[i+index + 2] = iImage[i + 2];
    				outputBuffer[i+index + 3] = iImage[i + 3];
    			}
    		}

    	} else {
			for (int i = start; (i < end) && !threadStopped; i++) {
				progress++;
				if ((progress % progressModulus) == 0) {
					fireProgressStateChanged(minProgressValue + (int) (progress / progressModulus));
					// System.out.println("Entire = " + entireImage);
				}

				if ((entireImage == true) || mask.get(i)) {
					outputBuffer[i + index] = AlgorithmConvolver.convolve3DPt(i,
							srcImage.getExtents(), iImage, kExtents,
							kernelBuffer);
				} else {
					outputBuffer[i + index] = iImage[i];
				}
			}
		}

    }
    
    private final void convolve3DBilateral(int start, int end, float[] iImage, int index){
        float resultBuffer[] = null;
        if(srcImage.isColorImage()){
            for (int i = start; (i < end) && !threadStopped; i += 4) {
                progress += 4;
                if ((progress % progressModulus) == 0) {
                    fireProgressStateChanged(minProgressValue + (int)(progress/progressModulus));
                }

                if ((entireImage == true) || mask.get(i / 4)) {
                    outputBuffer[i+index] = iImage[i]; // alpha
                    resultBuffer = AlgorithmConvolver.convolveBilateral3DCIELabPt(i,
                            srcImage.getExtents(), iImage, kExtents, kernelBuffer, intensityGaussianDenom);
                    outputBuffer[i + index + 1] = resultBuffer[0];
                    outputBuffer[i + index + 2] = resultBuffer[1];
                    outputBuffer[i + index + 3] = resultBuffer[2];
                } else {
                    outputBuffer[i+index] = iImage[i];
                    outputBuffer[i+index + 1] = iImage[i + 1];
                    outputBuffer[i+index + 2] = iImage[i + 2];
                    outputBuffer[i+index + 3] = iImage[i + 3];
                }
            }

        } else {
            for (int i = start; (i < end) && !threadStopped; i++) {
                progress++;
                if ((progress % progressModulus) == 0) {
                    fireProgressStateChanged(minProgressValue + (int) (progress / progressModulus));
                    // System.out.println("Entire = " + entireImage);
                }

                if ((entireImage == true) || mask.get(i)) {
                    outputBuffer[i + index] = AlgorithmConvolver.convolveBilateral3DPt(i,
                            srcImage.getExtents(), iImage, kExtents,
                            kernelBuffer, intensityGaussianDenom);
                } else {
                    outputBuffer[i + index] = iImage[i];
                }
            }
        }

    }
    
    private final void convolve2D3D(int start, int end, float[] iImage, int index,
                                    int min3DLength, int max3DLength){
        int sliceMod;
        int i1, i2, i3 = 0;
        if (srcImage.isColorImage()) {
            sliceMod = 4 * srcImage.getExtents()[0] * srcImage.getExtents()[1];
        }
        else {
            sliceMod = srcImage.getExtents()[0] * srcImage.getExtents()[1];    
        }
        float[] iImage2 = new float[sliceMod];
        if(srcImage.isColorImage()){
            for (int i = start; (i < end) && !threadStopped; i += 4) {
                progress += 4;
                if ((progress % progressModulus) == 0) {
                    fireProgressStateChanged(minProgressValue + (int)(progress/progressModulus));
                }

                if ((entireImage == true) || mask.get(i / 4)) {
                    outputBuffer[i+index] = iImage[i]; // alpha
                    if ((i >= min3DLength) && (i < max3DLength)) {
                        // 3D convolution
                        if (red) {
                            outputBuffer[i+index + 1] = AlgorithmConvolver
                                    .convolve3DRGBPtXYZ(i + 1, srcImage
                                            .getExtents(), iImage, kExtents,
                                            kernelBufferX, kernelBufferY, kernelBufferZ);
                        } else {
                            outputBuffer[i+index + 1] = iImage[i + 1];
                        }

                        if (green) {
                            outputBuffer[i+index + 2] = AlgorithmConvolver
                                    .convolve3DRGBPtXYZ(i + 2, srcImage
                                            .getExtents(), iImage, kExtents,
                                            kernelBufferX, kernelBufferY, kernelBufferZ);
                        } else {
                            outputBuffer[i+index + 2] = iImage[i + 2];
                        }

                        if (blue) {
                            outputBuffer[i+index + 3] = AlgorithmConvolver
                                    .convolve3DRGBPtXYZ(i + 3, srcImage
                                            .getExtents(), iImage, kExtents,
                                            kernelBufferX, kernelBufferY, kernelBufferZ);
                        } else {
                            outputBuffer[i+index + 3] = iImage[i + 3];
                        }    
                    }
                    else { // cannot fit full 3D kernel on this slice - do 2D convolution
                        if ((i % sliceMod) == 0) {
                            for (i1 = 0, i2 = i, i3 = i; i1 < sliceMod; i1++, i2++) {
                                iImage2[i1] = iImage[i2];
                            }
                        }
                        if (red) {
                            outputBuffer[i+index + 1] = AlgorithmConvolver
                                    .convolve2DRGBPtSqrtXY(i + 1 - i3, srcImage
                                            .getExtents(), iImage2, kExtents,
                                            kernelBufferX2, kernelBufferY2);
                        } else {
                            outputBuffer[i+index + 1] = iImage[i + 1];
                        }

                        if (green) {
                            outputBuffer[i+index + 2] = AlgorithmConvolver
                                    .convolve2DRGBPtSqrtXY(i + 2 - i3, srcImage
                                            .getExtents(), iImage2, kExtents,
                                            kernelBufferX2, kernelBufferY2);
                        } else {
                            outputBuffer[i+index + 2] = iImage[i + 2];
                        }

                        if (blue) {
                            outputBuffer[i+index + 3] = AlgorithmConvolver
                                    .convolve2DRGBPtSqrtXY(i + 3 - i3, srcImage
                                            .getExtents(), iImage2, kExtents,
                                            kernelBufferX2, kernelBufferY2);
                        } else {
                            outputBuffer[i+index + 3] = iImage[i + 3];
                        }    
                    }
                } else {
                    outputBuffer[i+index] = iImage[i];
                    outputBuffer[i+index+1] = iImage[i + 1];
                    outputBuffer[i+index+2] = iImage[i + 2];
                    outputBuffer[i+index+3] = iImage[i + 3];
                }
            }

        } else {
            for (int i = start; (i < end) && !threadStopped; i++) {
                progress++;
                if ((progress % progressModulus) == 0) {
                    fireProgressStateChanged(minProgressValue + (int) (progress / progressModulus));
                    // System.out.println("Entire = " + entireImage);
                }

                if ((i >= min3DLength) && (i < max3DLength)) {
                    if ((entireImage == true) || mask.get(i)) {
                        outputBuffer[i + index] = AlgorithmConvolver.convolve3DPtXYZ(i,
                                srcImage.getExtents(), iImage, kExtents,
                                kernelBufferX, kernelBufferY, kernelBufferZ);
                    } else {
                        outputBuffer[i + index] = iImage[i];
                    }    
                } // if ((i >= min3DLength) && (i < max3DLength))
                else { // cannot fit full 3D kernel on this slice - do 2D convolution
                    if ((i % sliceMod) == 0) {
                        for (i1 = 0, i2 = i, i3 = i; i1 < sliceMod; i1++, i2++) {
                            iImage2[i1] = iImage[i2];
                        }
                    }
                    if ((entireImage == true) || mask.get(i)) {
                        outputBuffer[i+index] = AlgorithmConvolver.convolve2DPtSqrtXY(i - i3,
                                srcImage.getExtents(), iImage2, kExtents,
                                kernelBufferX2, kernelBufferY2);
                    } else {
                        outputBuffer[i + index] = iImage[i];
                    }    
                }
            }
        }

    }
    
    
    private final void convolve3DXYZ(int start, int end, float[] iImage, int index){
        if(srcImage.isColorImage()){
            for (int i = start; (i < end) && !threadStopped; i += 4) {
                progress += 4;
                if ((progress % progressModulus) == 0) {
                    fireProgressStateChanged(minProgressValue + (int)(progress/progressModulus));
                }

                if ((entireImage == true) || mask.get(i / 4)) {
                    outputBuffer[i+index] = iImage[i]; // alpha

                    if (red) {
                        outputBuffer[i+index + 1] = AlgorithmConvolver
                                .convolve3DRGBPtXYZ(i + 1, srcImage
                                        .getExtents(), iImage, kExtents,
                                        kernelBufferX, kernelBufferY, kernelBufferZ);
                    } else {
                        outputBuffer[i+index + 1] = iImage[i + 1];
                    }

                    if (green) {
                        outputBuffer[i+index + 2] = AlgorithmConvolver
                                .convolve3DRGBPtXYZ(i + 2, srcImage
                                        .getExtents(), iImage, kExtents,
                                        kernelBufferX, kernelBufferY, kernelBufferZ);
                    } else {
                        outputBuffer[i+index + 2] = iImage[i + 2];
                    }

                    if (blue) {
                        outputBuffer[i+index + 3] = AlgorithmConvolver
                                .convolve3DRGBPtXYZ(i + 3, srcImage
                                        .getExtents(), iImage, kExtents,
                                        kernelBufferX, kernelBufferY, kernelBufferZ);
                    } else {
                        outputBuffer[i+index + 3] = iImage[i + 3];
                    }
                } else {
                    outputBuffer[i+index] = iImage[i];
                    outputBuffer[i+index + 1] = iImage[i + 1];
                    outputBuffer[i+index + 2] = iImage[i + 2];
                    outputBuffer[i+index + 3] = iImage[i + 3];
                }
            }

        } else {
            for (int i = start; (i < end) && !threadStopped; i++) {
                progress++;
                if ((progress % progressModulus) == 0) {
                    fireProgressStateChanged(minProgressValue + (int) (progress / progressModulus));
                    // System.out.println("Entire = " + entireImage);
                }

                if ((entireImage == true) || mask.get(i)) {
                    outputBuffer[i + index] = AlgorithmConvolver.convolve3DPtXYZ(i,
                            srcImage.getExtents(), iImage, kExtents,
                            kernelBufferX, kernelBufferY, kernelBufferZ);
                } else {
                    outputBuffer[i + index] = iImage[i];
                }
            }
        }

    }
    
    private final void convolve3DNMS(int start, int end, float[] iImage, int index){
        for (int i = start; (i < end) && !threadStopped; i++) {
            progress++;
            if ((progress % progressModulus) == 0) {
                fireProgressStateChanged(minProgressValue + (int) (progress / progressModulus));
                // System.out.println("Entire = " + entireImage);
            }

            if ((entireImage == true) || mask.get(i)) {
                outputBuffer[i + index] = AlgorithmConvolver.convolve3DPtNMS(i,
                        srcImage.getExtents(), iImage, kExtents,
                        kernelBufferX, kernelBufferY, kernelBufferZ, kernelBufferXX, kernelBufferXY,
                        kernelBufferYY, kernelBufferXZ, kernelBufferYZ, kernelBufferZZ);
            } else {
                outputBuffer[i + index] = iImage[i];
            }
        }
    }
    
    private final void convolve3DNMSE(int start, int end, float[] iImage, int index){
        for (int i = start; (i < end) && !threadStopped; i++) {
            progress++;
            if ((progress % progressModulus) == 0) {
                fireProgressStateChanged(minProgressValue + (int) (progress / progressModulus));
                // System.out.println("Entire = " + entireImage);
            }

            if ((entireImage == true) || mask.get(i)) {
                outputBuffer[2*(i + index)] = AlgorithmConvolver.convolve3DPtNMS(i,
                        srcImage.getExtents(), iImage, kExtents,
                        kernelBufferX, kernelBufferY, kernelBufferZ, kernelBufferXX, kernelBufferXY,
                        kernelBufferYY, kernelBufferXZ, kernelBufferYZ, kernelBufferZZ);
                outputBuffer[2*(i + index)+1] = AlgorithmConvolver.convolve3DPtNMSE(i,
                        srcImage.getExtents(), iImage, kExtents,
                        kernelBufferX, kernelBufferY, kernelBufferZ, kernelBufferXXX, kernelBufferXXY,
                        kernelBufferXYY, kernelBufferYYY, kernelBufferXXZ, kernelBufferXZZ, kernelBufferXYZ,
                        kernelBufferYYZ, kernelBufferYZZ, kernelBufferZZZ);
            } else {
                outputBuffer[2*(i + index)] = iImage[i];
                outputBuffer[2*(i + index)+1] = iImage[i];
            }
        }
    }
    
    /**
	 * Prepares this class for destruction.
	 */
    public void finalize() {

        srcImage = null;
        outputBuffer = null;
        kExtents = null;
        kernelBuffer = null;
        kernelBufferX = null;
        kernelBufferY = null;
        kernelBufferZ = null;
        kernelBufferX2 = null;
        kernelBufferY2 = null;
        kernelBufferXX = null;
        kernelBufferXY = null;
        kernelBufferYY = null;
        kernelBufferXZ = null;
        kernelBufferYZ = null;
        kernelBufferZZ = null;
        kernelBufferXXX = null;
        kernelBufferXXY = null;
        kernelBufferXYY = null;
        kernelBufferYYY = null;
        kernelBufferXXZ = null;
        kernelBufferXZZ = null;
        kernelBufferXYZ = null;
        kernelBufferYYZ = null;
        kernelBufferYZZ = null;
        kernelBufferZZZ = null;
        
        super.finalize();
    }

    /**
     * Begins the execution of the 2D convolver.
     */
    public void run2D() {
        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        if (kExtents.length != 2) {
            displayError("Kernel is not 2D");

            return;
        }
        if (srcImage.getNDims() != 2) {
            displayError("Source Image is not 2D");

            return;
        }

        int cFactor = 1;

        if (srcImage.isColorImage()) {
            cFactor = 4;
        }

        int length = cFactor * srcImage.getSliceSize();
        if (nms2e) {
            outputBuffer = new float[2*length];
        }
        else if (doXY && (!sqrtXY)) {
            outputBuffer = new float[2*length];
        }
        else {
            outputBuffer = new float[length];
        }
        progress = 0;
        progressModulus = length / (maxProgressValue-minProgressValue);

        if (sqrtXY) {
            convolve2DSqrtXY(0, 1);
        }
        else if (doXY && (!sqrtXY)) {
            convolve2DXY(0, 1);
        }
        else if (nms2) {
            convolve2DNMS(0,1);
        }
        else if (nms2e) {
            convolve2DNMSE(0,1);
        }
        else if (bilateral) {
            convolve2DBilateral(0,1);
        }
        else {
            convolve2D(0, 1);
        }

        fireProgressStateChanged(maxProgressValue);

        if (threadStopped) {
            finalize();
            return;
        }
        setCompleted(true);
    }

    /**
     * Begins the execution of the 3D convolver.
     */
    public void run3D() {
        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }
        if (srcImage.getNDims() != 3) {
            displayError("Source Image is not 3D");

            return;
        }

        int cFactor = 1;

        if (srcImage.isColorImage()) {
            cFactor = 4;
        }
       
        progress = 0;
        int length;
        if(this.image25D){
            if(kExtents.length != 2){
                displayError("Kernel is not 2D");
                return;
            }
            length = cFactor * srcImage.getSliceSize();
            int nSlices = srcImage.getExtents()[2];
            int totalLength = length * srcImage.getExtents()[2];
            if (nms2e) {
                outputBuffer = new float[2*totalLength];
            }
            else if (doXY && (!sqrtXY)) {
                outputBuffer = new float[2*totalLength];
            }
            else {
                outputBuffer = new float[totalLength];
            }
            progressModulus = totalLength / (maxProgressValue-minProgressValue);

            if(this.multiThreadingEnabled){
    			final CountDownLatch doneSignal = new CountDownLatch(nthreads);
    			final float step = nSlices / nthreads;
    			for (int j = 0; j < nthreads; j++) {
    				final int fstart = (int)(j * step);
    				final int fend = (j == (nthreads-1))?nSlices:(int)(step * (j + 1));
    				Runnable task = new Runnable() {
    					public void run() {
                            if (sqrtXY) {
                                convolve2DSqrtXY(fstart, fend);    
                            }
                            else if (doXY && (!sqrtXY)) {
                                convolve2DXY(fstart, fend);
                            }
                            else if (nms2) {
                                convolve2DNMS(fstart, fend);
                            }
                            else if (nms2e) {
                                convolve2DNMSE(fstart, fend);
                            } 
                            else if (bilateral) {
                                convolve2DBilateral(fstart, fend);
                            }
                            else {
    						    convolve2D(fstart, fend);
                            }
    						doneSignal.countDown();
    					}
    				};
    				ThreadUtil.mipavThreadPool.execute(task);

    			}
    			try {
    				doneSignal.await();
    			} catch (InterruptedException e) {
    				e.printStackTrace();
    			}
            }else{
                if (sqrtXY) {
                    convolve2DSqrtXY(0, nSlices);
                }
                else if (doXY && (!sqrtXY)) {
                    convolve2DXY(0, nSlices);
                }
                else if (nms2) {
                    convolve2DNMS(0, nSlices);
                }
                else if (nms2e) {
                    convolve2DNMSE(0, nSlices);
                }
                else if (bilateral) {
                    convolve2DBilateral(0, nSlices);
                }
                else {
            	    convolve2D(0, nSlices);
                }
            }

            fireProgressStateChanged(maxProgressValue);

            if (threadStopped) {
                finalize();

                return;
            }
        	
        }else{
            // Start of slice at which full 3D kernels fit  
            final int min3DLength = cFactor * (kExtents[2]/2)* srcImage.getSliceSize();
            // Start of slice at which full 3D kernels no longer fit
            final int max3DLength = cFactor * (srcImage.getExtents()[2] - kExtents[2] + kExtents[2]/2 + 1) * srcImage.getSliceSize();
            if(kExtents.length != 3){
                displayError("Kernel is not 3D");
                return;
            }
        	float[] buffer = null;
            try {
                length = cFactor * srcImage.getSliceSize() * srcImage.getExtents()[2];

                // System.out.println ("sliceSize = " + srcImage.getSliceSize() + "  Length = " + length);
                buffer = new float[length];
                if (nms3e) {
                    outputBuffer = new float[2*length];
                }
                else {
                    outputBuffer = new float[length];
                }
                srcImage.exportData(0, length, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                errorCleanUp("Algorithm Convolver: Image(s) locked", true);

                return;
            } catch (OutOfMemoryError e) {
                buffer = null;
                errorCleanUp("Algorithm Convolver: Out of memory", true);

                return;
            }

            progressModulus = length / (maxProgressValue - minProgressValue);


        	if(multiThreadingEnabled){
				final CountDownLatch doneSignal = new CountDownLatch(nthreads);
				final int step = (int)(length / (cFactor*nthreads))*cFactor;
				for (int j = 0; j < nthreads; j++) {
					final int start = j * step;
					final int end = (j == (nthreads-1))?length:step * (j + 1);
					final float[] iImage = buffer;
					Runnable task = new Runnable() {
						public void run() {
                            if (sqrtXYZ) {
                                convolve3DXYZ(start, end, iImage, 0);    
                            }
                            else if (nms3) {
                                convolve3DNMS(start, end, iImage, 0);
                            }
                            else if (nms3e) {
                                convolve3DNMSE(start, end, iImage, 0);
                            }
                            else if (combined2D3D) {
                                convolve2D3D(start, end, iImage, 0, min3DLength, max3DLength);
                            }
                            else if (bilateral) {
                                convolve3DBilateral(start, end, iImage, 0);
                            }
                            else {
							    convolve3D(start, end, iImage, 0);
                            }
							doneSignal.countDown();
						}
					};
					ThreadUtil.mipavThreadPool.execute(task);

				}
				try {
					doneSignal.await();
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
        		
        	} else {
                if (sqrtXYZ) {
                    convolve3DXYZ(0, length, buffer, 0);
                }
                else if (nms3) {
                    convolve3DNMS(0, length, buffer, 0);
                }
                else if (nms3e) {
                    convolve3DNMSE(0, length, buffer, 0);
                }
                else if (combined2D3D) {
                    convolve2D3D(0, length, buffer, 0, min3DLength, max3DLength);
                }
                else if (bilateral) {
                    convolve3DBilateral(0, length, buffer, 0);
                }
                else {
        		    convolve3D(0, length, buffer, 0);
                }
        	}
        
            fireProgressStateChanged(maxProgressValue);

            if (threadStopped) {
                finalize();
                return;
            }
        }
        setCompleted(true);
    }

    public void run4D(){
        
        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }
        if (srcImage.getNDims() != 4) {
            displayError("Source Image is not 4D");

            return;
        }
        
        if(kExtents.length != 3){
            displayError("Kernel is not 3D");
            return;
        }
        int length;
        float[] buffer;
        int cFactor = 1;

        if (srcImage.isColorImage()) {
            cFactor = 4;
        }
        
        // Start of slice at which full 3D kernels fit  
        final int min3DLength = cFactor * (kExtents[2]/2)* srcImage.getSliceSize();
        // Start of slice at which full 3D kernels no longer fit
        final int max3DLength = cFactor * (srcImage.getExtents()[2] - kExtents[2] + kExtents[2]/2 + 1) * srcImage.getSliceSize();

        try {
            length = cFactor * srcImage.getSliceSize() * srcImage.getExtents()[2];
            buffer = new float[length];
            outputBuffer = new float[length*srcImage.getExtents()[3]];
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Convolver: Out of memory", true);

            return;
        }

        int index;
        int end = srcImage.getExtents()[3];

        progress = 0;
        progressModulus = length * end / (maxProgressValue-minProgressValue);

        for (int t = 0; (t < end) && !threadStopped; t++) {

			try {
				srcImage.exportData(t * length, length, buffer); 
			} catch (IOException error) {
				displayError("Algorithm Convolver: Image(s) locked");
				setCompleted(false);
				fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);

				return;
			}

			index = t * length;

			if (this.multiThreadingEnabled) {
				final CountDownLatch doneSignal = new CountDownLatch(nthreads);
				final float step = ((float) length) / nthreads;
				for (int j = 0; j < nthreads; j++) {
					final int start = (int) (j * step);
					final int fend = (int) (step * (j + 1));
					final float[] iImage = buffer;
					final int findex = index;
					Runnable task = new Runnable() {
						public void run() {
                            if (sqrtXYZ) {
                                convolve3DXYZ(start, fend, iImage, findex);
                            }
                            else if (combined2D3D) {
                                convolve2D3D(start, fend, iImage, findex, min3DLength, max3DLength);
                            }
                            else if (bilateral) {
                                convolve3DBilateral(start, fend, iImage, findex);
                            }
                            else {
							    convolve3D(start, fend, iImage, findex);
                            }
							doneSignal.countDown();
						}
					};
					ThreadUtil.mipavThreadPool.execute(task);

				}
				try {
					doneSignal.await();
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			} else {
                if (sqrtXYZ) {
                    convolve3DXYZ(0, length, buffer, index);    
                }
                else if (combined2D3D) {
                    convolve2D3D(0, length, buffer, index, min3DLength, max3DLength);
                }
                else if (bilateral) {
                    convolve3DBilateral(0, length, buffer, index);
                }
                else {
				    convolve3D(0, length, buffer, index);
                }
			}

		}

        fireProgressStateChanged(maxProgressValue);

        if (threadStopped) {
            finalize();
            return;
        }

        setCompleted(true);
    
    }
    /**
     * Begins execution of the convolver.
     */
    public void runAlgorithm() {
        if (srcImage.getNDims() == 2) {
            run2D();
        } else if (srcImage.getNDims() == 3) {
            run3D();
        }else if(srcImage.getNDims() == 4){
        	run4D();
        }else{
            displayError("The dimension of image exceeds 4d, is not supported yet.");
        }
    }

    /**
     * Performs bilinear interpolation of image data.
     *
     * @param   i         index into image
     * @param   dx        change in x from integer
     * @param   dy        change in y from integer
     * @param   iExtents  dimensions of image
     * @param   image     image data
     *
     * @return  the bilinearly interpolated value
     */
    private static float getBilinear(int i, float dx, float dy, int[] iExtents, float[] image) {

        int xDim = iExtents[0];
        int yDim = iExtents[1];
        float x1, x2;
        int ix, iy;
        int xIndex;
        int yIndex;
        
        xIndex = i % xDim;
        yIndex = i / xDim;

        if ((dx == 0.0f) || (xIndex == (xDim - 1))) {
            ix = i;
        } else {
            ix = i + 1;
        }

        if ((dy == 0.0f) || (yIndex == (yDim - 1))) {
            iy = 0;
        } else {
            iy = xDim;
        }

        x1 = ((1 - dx) * image[i]) + (dx * image[ix]);
        x2 = ((1 - dx) * image[i + iy]) + (dx * image[ix + iy]);

        return (float) (((1 - dy) * x1) + (dy * x2));
    }

    /**
     * Performs trilinear interpolation of image data.
     *
     * @param   i1        index into image
     * @param   dx        change in x from integer
     * @param   dy        change in y from integer
     * @param   dz        change in z from integer
     * @param   iExtents  dimensions of image
     * @param   image     image data
     *
     * @return  the trilinearly interpolated data value
     */
    private static float getTrilinear(int i1, float dx, float dy, float dz, int[] iExtents, float[] image) {

        int xDim = iExtents[0];
        int yDim = iExtents[1];
        int zDim = iExtents[2];
        int sliceSize = xDim * yDim;
        int i2, ix1, ix2, iy;
        int sliceIndex;
        int xIndex;
        int yIndex;
        int zIndex;
        
        sliceIndex = (i1 % sliceSize);
        xIndex = sliceIndex % xDim;
        yIndex = sliceIndex / xDim;
        zIndex = i1 / sliceSize;

        float a1, a2;
        float b1, b2;

        
        if ((dz == 0.0f) || (zIndex == (zDim - 1))) {
            i2 = i1;
        } else {
            i2 = i1 + sliceSize;
        }

        if ((dx == 0.0f) || (xIndex == (xDim - 1))) {
            ix1 = i1;
            ix2 = i2;
        } else {
            ix1 = i1 + 1;
            ix2 = i2 + 1;
        }

        if ((dy == 0.0f) || (yIndex == (yDim - 1))) {
            iy = 0;
        } else {
            iy = xDim;
        }

        a1 = ((1 - dx) * image[i1]) + (dx * image[ix1]);
        a2 = ((1 - dx) * image[i1 + iy]) + (dx * image[ix1 + iy]);
        b1 = ((1 - dy) * a1) + (dy * a2);

        a1 = ((1 - dx) * image[i2]) + (dx * image[ix2]);
        a2 = ((1 - dx) * image[i2 + iy]) + (dx * image[ix2 + iy]);
        b2 = ((1 - dy) * a1) + (dy * a2);

        return (float) (((1 - dz) * b1) + (dz * b2));
    }
    
    
    
    
    /**
     * Performs trilinear interpolation of image data.
     *
     * @param   i1        index into image
     * @param   dx        change in x from integer
     * @param   dy        change in y from integer
     * @param   dz        change in z from integer
     * @param   iExtents  dimensions of image
     * @param   image     image data
     *
     * @return  the trilinearly interpolated data value
     */
    public static byte[] getTrilinearC(int i1, float dx, float dy, float dz, int[] iExtents, byte[] image) {

        int xDim = iExtents[0];
        int yDim = iExtents[1];
        int zDim = iExtents[2];
        int sliceSize = xDim * yDim * 4;
        int i2, ix1, ix2, iy;
        int sliceIndex;
        int xIndex;
        int yIndex;
        int zIndex;
        
        sliceIndex = (i1 % sliceSize);
        xIndex = sliceIndex % (4*xDim);
        yIndex = sliceIndex / (4*xDim);
        zIndex = i1 / sliceSize;

        float a1, a2;
        float b1, b2;
        
        i1 = i1 + 1; //get on red channel
        

        
        if ((dz == 0.0f) || (zIndex == (zDim - 1))) {
            i2 = i1;
        } else {
            i2 = i1 + sliceSize;
        }

        if ((dx == 0.0f) || (xIndex == (xDim - 1))) {
            ix1 = i1;
            ix2 = i2;
        } else {
            ix1 = i1 + 4;
            ix2 = i2 + 4;
        }

        if ((dy == 0.0f) || (yIndex == (yDim - 1))) {
            iy = 0;
        } else {
            iy = xDim * 4;
        }

        
        a1 = ((1 - dx) * (image[i1] & 0xff)) + (dx * (image[ix1] & 0xff));
        a2 = ((1 - dx) * (image[i1 + iy] & 0xff)) + (dx * (image[ix1 + iy] & 0xff));
        b1 = ((1 - dy) * a1) + (dy * a2);

        a1 = ((1 - dx) * (image[i2] & 0xff)) + (dx * (image[ix2] & 0xff));
        a2 = ((1 - dx) * (image[i2 + iy] & 0xff)) + (dx * (image[ix2 + iy] & 0xff));
        b2 = ((1 - dy) * a1) + (dy * a2);
        
        
        byte r = (byte) (((1 - dz) * b1) + (dz * b2));
        

        a1 = ((1 - dx) * (image[i1 + 1] & 0xff)) + (dx * (image[ix1 + 1] & 0xff));
        a2 = ((1 - dx) * (image[i1 + iy + 1] & 0xff)) + (dx * (image[ix1 + iy + 1] & 0xff));
        b1 = ((1 - dy) * a1) + (dy * a2);

        a1 = ((1 - dx) * (image[i2 + 1] & 0xff)) + (dx * (image[ix2 + 1] & 0xff));
        a2 = ((1 - dx) * (image[i2 + iy + 1] & 0xff)) + (dx * (image[ix2 + iy + 1] & 0xff));
        b2 = ((1 - dy) * a1) + (dy * a2);
        
        byte g = (byte) (((1 - dz) * b1) + (dz * b2));
        

        a1 = ((1 - dx) * (image[i1 + 2] & 0xff)) + (dx * (image[ix1 + 2] & 0xff));
        a2 = ((1 - dx) * (image[i1 + iy + 2] & 0xff)) + (dx * (image[ix1 + iy + 2] & 0xff));
        b1 = ((1 - dy) * a1) + (dy * a2);

        a1 = ((1 - dx) * (image[i2 + 2] & 0xff)) + (dx * (image[ix2 + 2] & 0xff));
        a2 = ((1 - dx) * (image[i2 + iy + 2] & 0xff)) + (dx * (image[ix2 + iy + 2] & 0xff));
        b2 = ((1 - dy) * a1) + (dy * a2);
        
        byte b = (byte) (((1 - dz) * b1) + (dz * b2));

        byte[] rgb = {r,g,b};
        
        return rgb;
    }
    

    /**
     * Returns the output buffer
     * @return buffer that contains the data after convolution
     */
	public float[] getOutputBuffer() {
		return outputBuffer;
	}
}
