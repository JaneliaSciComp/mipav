package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.util.MipavUtil;
import gov.nih.mipav.view.ViewJProgressBar;

import java.io.*;
import java.util.concurrent.CountDownLatch;


/**
 * Convolves kernel with a 2D or 3D image - only pixels where the kernel is completely contained in the image are
 * convolved, otherwise they are set to zero. This is reasonable since data at the edges of images is rarely used and
 * large kernels should not be used since it is much faster to perform FFT, filter, and IFFT. The break even point is
 * probably around a kernel size of 11 or so. To replace the image model with a convolved version simply construct this
 * object with the source image as both the srcImg and the destImg.
 *
 * <p>Since this class extends the AlgorithmBase class that extends the Thread class it can be run in its own thread by
 * invoking algoConvolver3DObj.start(); It can also be invoked without a new thread by calling the the run() method
 * directly (ie. algoConvolver3DObj.run()).</p>
 *
 * <ol>
 *   <li>Source image is exported (locked and unlocked by export)</li>
 *   <li>Kernel is exported</li>
 *   <li>Destination image is locked</li>
 *   <li>Image is convolved with kernel</li>
 *   <li>Destination image is unlocked</li>
 *   <li>Return</li>
 * </ol>
 *
 * @version  0.1 Aug 1, 1997
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class AlgorithmConvolver extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The image data to convolve. */
    private float[] imgBuffer;

    /** The convolution kernel. */
    private ModelImage kernel;

    private int[] kExtents;
    
    private float[] kernelBuffer;
    
    private float[] kernelBufferX;
    
    private float[] kernelBufferY;
    
    private float[] kernelBufferZ;
    
    private boolean red, blue, green;
    
    private float[] outputBuffer;
    
    private boolean entireImage;
    
    private boolean sqrtXY = false;
    
    private boolean sqrtXYZ = false;
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
     * Sets the source and destination images and calls the appropriate method based on image dimensionality.
     *
     * @param  destImg  Destination image of result.
     * @param  srcImg   Source image to be convolved with kernel.
     * @param  kern     Kernel image.
     */
    public AlgorithmConvolver(ModelImage destImg, ModelImage srcImg, ModelImage kern) {
        super(destImg, srcImg);
        kernel = kern;

        init();
    }

    public AlgorithmConvolver(ModelImage destImage, ModelImage srcImage, float[] kernel, int[] kExtents, boolean entireImage, boolean image25D){
    	super(destImage, srcImage);
    	kernelBuffer = kernel;
    	this.kExtents = kExtents;
    	this.entireImage = entireImage;
    	this.image25D = image25D;
    }
    
    public AlgorithmConvolver(ModelImage destImage, ModelImage srcImage, float[] kernelX, float[] kernelY,
                              int[] kExtents, boolean entireImage){
        super(destImage, srcImage);
        kernelBufferX = kernelX;
        kernelBufferY = kernelY;
        this.kExtents = kExtents;
        this.entireImage = entireImage;
        image25D = true;
        sqrtXY = true;
    }
    
    
    public AlgorithmConvolver(ModelImage destImage, ModelImage srcImage, float[] kernelX, float[] kernelY, float[] kernelZ,
            int[] kExtents, boolean entireImage){
        super(destImage, srcImage);
        kernelBufferX = kernelX;
        kernelBufferY = kernelY;
        kernelBufferZ = kernelZ;
        this.kExtents = kExtents;
        this.entireImage = entireImage;
        image25D = false;
        sqrtXYZ = true;
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
    public static final synchronized float convolve2DPtXY(int pix, int[] iExtents, float[] image, int[] kExtents,
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
     * @param   pt        floating point indicating location of convolution
     * @param   iExtents  image dimensions
     * @param   image     image data
     * @param   kExtents  kernel dimensions
     * @param   kernel    kernel data
     *
     * @return  the value of the pixel after convolution with the kernel
     */
    public static final float convolve2DPt(Point2Df pt, int[] iExtents, float[] image, int[] kExtents, float[] kernel) {

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

        dx = pt.x - (int) pt.x;
        dy = pt.y - (int) pt.y;
        offsetX = (int) pt.x - (xKDim / 2);
        offsetY = (int) pt.y - (yKDim / 2);
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
    public static final float convolve2DRGBPtXY(int pix, int[] iExtents, float[] image, int[] kExtents,
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
				errorCleanUp("Algorithm Gaussian Blur: Image(s) locked", false);

				return;
			}

			if (color == true) {

				for (int i = 0, idx = start; (i < length) && !threadStopped; i += 4, idx += 4) {
					progress++;
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
    
    private final void convolve2DXY(int startSlice, int endSlice){
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
                errorCleanUp("Algorithm Gaussian Blur: Image(s) locked", false);

                return;
            }

            if (color == true) {

                for (int i = 0, idx = start; (i < length) && !threadStopped; i += 4, idx += 4) {
                    progress++;
                    if ((progress % progressModulus) == 0) {
                        fireProgressStateChanged(minProgressValue + (int)(progress/progressModulus));
                    }

                    if ((entireImage == true) || mask.get(i / 4)) {
                        outputBuffer[idx] = buffer[i]; // alpha

                        if (red) {
                            outputBuffer[idx + 1] = AlgorithmConvolver
                                    .convolve2DRGBPtXY(i + 1, srcImage
                                            .getExtents(), buffer, kExtents,
                                            kernelBufferX, kernelBufferY);
                        } else {
                            outputBuffer[idx + 1] = buffer[i + 1];
                        }

                        if (green) {
                            outputBuffer[idx + 2] = AlgorithmConvolver
                                    .convolve2DRGBPtXY(i + 2, srcImage
                                            .getExtents(), buffer, kExtents,
                                            kernelBufferX, kernelBufferY);
                        } else {
                            outputBuffer[idx + 2] = buffer[i + 2];
                        }

                        if (blue) {
                            outputBuffer[idx + 3] = AlgorithmConvolver
                                    .convolve2DRGBPtXY(i + 3, srcImage
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
                        outputBuffer[idx] = AlgorithmConvolver.convolve2DPtXY(i,
                                srcImage.getExtents(), buffer, kExtents,
                                kernelBufferX, kernelBufferY);
                    } else {
                        outputBuffer[idx] = buffer[i];
                    }
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
    
    /**
	 * Prepares this class for destruction.
	 */
    public void finalize() {

        kernel = null;
        destImage = null;
        srcImage = null;
        imgBuffer = null;
        super.finalize();
    }

    /**
     * Begins the excution of the 2D convolver.
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
        outputBuffer = new float[length];
        progress = 0;
        progressModulus = length / (maxProgressValue-minProgressValue);

        if (sqrtXY) {
            convolve2DXY(0, 1);
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
     * Begins the excution of the 3D convolver.
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
            outputBuffer = new float[totalLength];
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
                                convolve2DXY(fstart, fend);    
                            }
                            else {
    						    convolve2D(fstart, fend);
                            }
    						doneSignal.countDown();
    					}
    				};
    				MipavUtil.mipavThreadPool.execute(task);

    			}
    			try {
    				doneSignal.await();
    			} catch (InterruptedException e) {
    				e.printStackTrace();
    			}
            }else{
                if (sqrtXY) {
                    convolve2DXY(0, nSlices);
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
            if(kExtents.length != 3){
                displayError("Kernel is not 3D");
                return;
            }
        	float[] buffer = null;
            try {
                length = cFactor * srcImage.getSliceSize() * srcImage.getExtents()[2];

                // System.out.println ("sliceSize = " + srcImage.getSliceSize() + "  Length = " + length);
                buffer = new float[length];
                outputBuffer = new float[length];
                srcImage.exportData(0, length, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                errorCleanUp("Algorithm Gaussian Blur: Image(s) locked", true);

                return;
            } catch (OutOfMemoryError e) {
                buffer = null;
                errorCleanUp("Algorithm Gaussian Blur: Out of memory", true);

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
                            else {
							    convolve3D(start, end, iImage, 0);
                            }
							doneSignal.countDown();
						}
					};
					MipavUtil.mipavThreadPool.execute(task);

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

        try {
            length = cFactor * srcImage.getSliceSize() * srcImage.getExtents()[2];
            buffer = new float[length];
            outputBuffer = new float[length*srcImage.getExtents()[3]];
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Gaussian Blur: Out of memory", true);

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
				displayError("Algorithm Gaussian Blur: Image(s) locked");
				setCompleted(false);
				fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);
				destImage.releaseLock();

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
                            else {
							    convolve3D(start, fend, iImage, findex);
                            }
							doneSignal.countDown();
						}
					};
					MipavUtil.mipavThreadPool.execute(task);

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
     * Accessor that sets the destination and kernel images.
     *
     * @param  destImg  Destination image.
     * @param  kern     Kernel image.
     */
    public void setImages(ModelImage destImg, ModelImage kern) {
        destImage = destImg;
        kernel = kern;
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
        float x1, x2;
        int ix, iy;

        // The below code prevents an out of bounds index being used for image
        // when the y coordinate is exactly equal to ydim - 1.
        if (dx == 0.0f) {
            ix = i;
        } else {
            ix = i + 1;
        }

        if (dy == 0.0f) {
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
        int imageSize = xDim * yDim;
        int i2, ix1, ix2, iy;

        float a1, a2;
        float b1, b2;

        // The following code prevents an out of bounds array index from occurring
        // in the case when the z coordinate exactly equals zdim - 1.
        if (dz == 0.0f) {
            i2 = i1;
        } else {
            i2 = i1 + imageSize;
        }

        if (dx == 0.0f) {
            ix1 = i1;
            ix2 = i2;
        } else {
            ix1 = i1 + 1;
            ix2 = i2 + 1;
        }

        if (dy == 0.0f) {
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
     * Initializes the convolver.
     */
    private void init() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        try {
            int length = srcImage.getSliceSize();
            if(srcImage.getNDims() == 3){
            	length *= srcImage.getExtents()[2];
            }

            imgBuffer = new float[length];
            srcImage.exportData(0, length, imgBuffer); // locks and releases lock
        } catch (IOException error) {
            displayError("Algorithm Convolver: Image(s) locked");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Convolver: Out of memory");
            setCompleted(false);

            return;
        }
    }

	public float[] getOutputBuffer() {
		return outputBuffer;
	}
}
