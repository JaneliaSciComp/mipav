package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;


/**
 * Calculates the gradient magnitude of an image at a scale defined by the user.
 *
 * @version  0.1 Feb 11, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class AlgorithmGradientMagnitude extends AlgorithmBase implements AlgorithmInterface {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean blue = true;

    /**
     * Flag, if true, indicates that the whole image should be processed. If false only process the image over the mask
     * areas.
     */
    private boolean entireImage;

    /** DOCUMENT ME! */
    private boolean green = true;


    /** Storage location of the first derivative of the Gaussian in the X direction for 2D images; */
    private float[] Gx2Data;

    /** Storage location of the first derivative of the Gaussian in the X direction. */
    private float[] GxData;

    /** Storage location of the first derivative of the Gaussian in the Y direction for 2D images; */
    private float[] Gy2Data;

    /** Storage location of the first derivative of the Gaussian in the Y direction. */
    private float[] GyData;

    /** Storage location of the first derivative of the Gaussian in the Z direction. */
    private float[] GzData;


    /** Extents of the kernel. */
    private int[] kExtents;

    /** Flags indicate which color channel to process. True indicates the channel should be processed. */
    private boolean red = true;

    /** Standard deviations of the gaussian used to calculate the kernels. */
    private float[] sigmas;
    
    /** Receives output of AlgorithmConvolver */
    private float[] outputBuffer = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmGradientMagnitude object.
     *
     * @param  srcImg    source image model
     * @param  sigmas    Gaussian standard deviations in each dimension
     * @param  maskFlag  Flag, if true, indicates that the gradient magnitude will be calculated for the whole image
     * @param  img25D    Flag, if true, indicates that each slice of the 3D volume should be processed independently. 2D
     *                   images disregard this flag.
     */
    public AlgorithmGradientMagnitude(ModelImage srcImg, float[] sigmas, boolean maskFlag, boolean img25D) {
        super(null, srcImg);

        this.sigmas = sigmas;
        image25D = img25D;
        entireImage = maskFlag;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }
    }

    /**
     * Creates a new AlgorithmGradientMagnitude object.
     *
     * @param  destImg   image model where result image is to stored
     * @param  srcImg    source image model
     * @param  sigmas    Gaussian standard deviations in each dimension
     * @param  maskFlag  Flag, if true, indicates that the gradient magnitude will be calculated for the whole image
     * @param  img25D    Flag, if true, indicates that each slice of the 3D volume should be processed independently. 2D
     *                   images disregard this flag.
     */
    public AlgorithmGradientMagnitude(ModelImage destImg, ModelImage srcImg, float[] sigmas, boolean maskFlag,
                                      boolean img25D) {
        super(destImg, srcImg);

        this.sigmas = sigmas;
        entireImage = maskFlag;
        image25D = img25D;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Calculates the gradient magnitude of a 2D image and returns it as a float buffer.
     *
     * @param   buffer   DOCUMENT ME!
     * @param   extents  DOCUMENT ME!
     * @param   color    DOCUMENT ME!
     * @param   xDirs    DOCUMENT ME!
     * @param   yDirs    DOCUMENT ME!
     *
     * @return  Buffer with the gradient magnitude of the image.
     */
    public float[] calcInBuffer2D(float[] buffer, int[] extents, boolean color, float[] xDirs, float[] yDirs) {
        makeKernels2D();

        return calcInPlace2DBuffer(1, buffer, extents, color, xDirs, yDirs);
    }

    /**
     * Calculates the gradient magnitude of a 2D image and returns it as a float buffer.
     *
     * @param   buffer   DOCUMENT ME!
     * @param   extents  DOCUMENT ME!
     * @param   color    DOCUMENT ME!
     * @param   xDirs    DOCUMENT ME!
     * @param   yDirs    DOCUMENT ME!
     *
     * @return  Buffer with the gradient magnitude of the image.
     */
    public float[] calcInBuffer2DUnnormalized(float[] buffer, int[] extents, boolean color, float[] xDirs,
                                              float[] yDirs) {
        makeKernels2D();

        return calcInPlace2DBufferUnnormalized(1, buffer, extents, color, xDirs, yDirs);
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        kExtents = null;
        sigmas = null;
        GxData = null;
        GyData = null;
        GzData = null;
        Gx2Data = null;
        Gy2Data = null;
        destImage = null;
        srcImage = null;
        super.finalize();
    }


    /**
     * Starts the program.
     */
    public void runAlgorithm() {
        AlgorithmConvolver convolver;

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }
        final long startTime = System.currentTimeMillis();

        if (srcImage.getNDims() == 2) {
            makeKernels2D();
        } else if ((srcImage.getNDims() == 3) && (image25D == false)) {
            makeKernels2D();
            makeKernels3D();
        } else if ((srcImage.getNDims() == 3) && (image25D == true)) {
            makeKernels2D();
        } else if (srcImage.getNDims() == 4) {
            makeKernels2D();
            makeKernels3D();
        }

        if (threadStopped) {
            finalize();

            return;
        }

        

        fireProgressStateChanged(0, null, "Calculating gradient magnitude ...");
        
        if ((srcImage.getNDims() == 2) || image25D) {
            boolean sqrtXY = false;
            convolver = new AlgorithmConvolver(srcImage, Gx2Data, Gy2Data, kExtents, entireImage, sqrtXY);
            convolver.setMinProgressValue(0);
            convolver.setMaxProgressValue(20);
        }
        else {
            boolean combined2D3D = true;
            convolver = new AlgorithmConvolver(srcImage, GxData, GyData, GzData, Gx2Data, Gy2Data, kExtents, entireImage, combined2D3D);
            convolver.setMinProgressValue(0);
            convolver.setMaxProgressValue(100);
        }  
        linkProgressToAlgorithm(convolver);
        convolver.addListener(this);
        if (!entireImage) {
            convolver.setMask(mask);
        }
        if (srcImage.isColorImage()) {
            convolver.setColorChannels(red, green, blue);
        }
        convolver.run();
        convolver.finalize();    
        
        if (destImage != null) {

            if (srcImage.getNDims() == 2) {
                calcStoreInDest2D(1);
            } else if ((srcImage.getNDims() == 3) && (image25D == false)) {
                calcStoreInDest34D();
            } else if ((srcImage.getNDims() == 3) && (image25D == true)) {
                calcStoreInDest2D(srcImage.getExtents()[2]);
            } else if (srcImage.getNDims() == 4) {
                calcStoreInDest34D();
            }
        } else {

            if (srcImage.getNDims() == 2) {
                calcInPlace2D(1);
            } else if ((srcImage.getNDims() == 3) && (image25D == false)) {
                calcInPlace34D();
            } else if ((srcImage.getNDims() == 3) && (image25D == true)) {
                calcInPlace2D(srcImage.getExtents()[2]);
            } else if (srcImage.getNDims() == 4) {
                calcInPlace34D();
            }
        }
        System.out.println("Time Consumed : " + (System.currentTimeMillis() - startTime));
    }

    /**
     * Sets the flag for the blue channel.
     *
     * @param  flag  if set to true then the blue channel is processed.
     */
    public void setBlue(boolean flag) {
        blue = flag;
    }

    /**
     * Sets the flag for the green channel.
     *
     * @param  flag  if set to true then the green channel is processed.
     */
    public void setGreen(boolean flag) {
        green = flag;
    }

    /**
     * Sets the flag for the red channel.
     *
     * @param  flag  if set to true then the red channel is processed.
     */
    public void setRed(boolean flag) {
        red = flag;
    }


    /**
     * Calculates the gradient image and replaces the source image with the new image.
     *
     * @param  nImages  number of images on which to calculate the gradient magnitude. If 2D image then nImage = 1. If
     *                  3D image where each image is to processed independently then nImages equals the number of images
     *                  in the volume.
     */
    private void calcInPlace2D(int nImages) {
        boolean color = false;

        if (srcImage != null) {

            if (srcImage.isColorImage() == true) {
                color = true;
            }
        }

        float[] buffer = calcInPlace2DBuffer(nImages, null, srcImage.getExtents(), color, null, null);

        if (threadStopped || (buffer == null)) {
            finalize();

            return;
        }

        try {
            srcImage.importData(0, buffer, true);
        } catch (IOException error) {
            errorCleanUp("Algorithm Gradient Magnitude importData: Image(s) locked", false);

            return;
        }

        
        setCompleted(true);
    }

    /**
     * Calculates the gradient image and returns a float buffer with the new values.
     *
     * @param   nImages      number of images on which to calculate the gradient magnitude. If 2D image then nImage = 1.
     *                       If 3D image where each image is to processed independently then nImages equals the number
     *                       of images in the volume.
     * @param   buffer       DOCUMENT ME!
     * @param   extents      DOCUMENT ME!
     * @param   color        DOCUMENT ME!
     * @param   xDirections  DOCUMENT ME!
     * @param   yDirections  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private float[] calcInPlace2DBuffer(int nImages, float[] buffer, int[] extents, boolean color, float[] xDirections,
                                        float[] yDirections) {

        int i, s;
        int length, totalLength;
        int start;
        float[] resultBuffer;
        float ix, iy;
        int cFactor;

        if (color) {
            cFactor = 4;
        } else {
            cFactor = 1;
        }

        try {

            if (buffer == null) {
                length = cFactor * srcImage.getSliceSize();
            } else {
                length = buffer.length;
            }

            totalLength = length * nImages;
            resultBuffer = new float[length * nImages];

            if (srcImage != null) {
                fireProgressStateChanged(srcImage.getImageName(), "Calculating the gradient magnitude ...");
            }
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Gradient Magnitude: Out of memory", true);

            return null;
        }

        if (buffer != null) {
            fireProgressStateChanged(0, null, null);
            int mod = totalLength / 100; // mod is 1 percent of length
            for (s = 0; s < nImages; s++) {
                start = s * length;
    
                if (color == true) {
    
                    for (i = 0; (i < length) && !threadStopped; i += 4) {
    
                        if ((((start + i) % mod) == 0)) {
                            //fireProgressStateChanged(Math.round((float) (start + i) / (totalLength - 1) * 100));
                            fireProgressStateChanged( ((float) (start + i) / (totalLength - 1) ), null, null);
                        }
    
                        if (entireImage || mask.get(i / 4)) {
                            resultBuffer[start + i] = buffer[i];
    
                            if (red) {
                                ix = AlgorithmConvolver.convolve2DRGBPt(i + 1, extents, buffer, kExtents, Gx2Data);
                                iy = AlgorithmConvolver.convolve2DRGBPt(i + 1, extents, buffer, kExtents, Gy2Data);
                                resultBuffer[start + i + 1] = (float) Math.sqrt((ix * ix) + (iy * iy));
    
                                if (xDirections != null) {
                                    xDirections[start + i + 1] = ix / resultBuffer[start + i + 1];
                                    yDirections[start + i + 1] = iy / resultBuffer[start + i + 1];
                                }
                            } else {
                                resultBuffer[start + i + 1] = buffer[i + 1];
                            }
    
                            if (green) {
                                ix = AlgorithmConvolver.convolve2DRGBPt(i + 2, extents, buffer, kExtents, Gx2Data);
                                iy = AlgorithmConvolver.convolve2DRGBPt(i + 2, extents, buffer, kExtents, Gy2Data);
                                resultBuffer[start + i + 2] = (float) Math.sqrt((ix * ix) + (iy * iy));
    
                                if (xDirections != null) {
                                    xDirections[start + i + 2] = ix / resultBuffer[start + i + 2];
                                    yDirections[start + i + 2] = iy / resultBuffer[start + i + 2];
                                }
                            } else {
                                resultBuffer[start + i + 2] = buffer[i + 2];
                            }
    
                            if (blue) {
                                ix = AlgorithmConvolver.convolve2DRGBPt(i + 3, extents, buffer, kExtents, Gx2Data);
                                iy = AlgorithmConvolver.convolve2DRGBPt(i + 3, extents, buffer, kExtents, Gy2Data);
                                resultBuffer[start + i + 3] = (float) Math.sqrt((ix * ix) + (iy * iy));
    
                                if (xDirections != null) {
                                    xDirections[start + i + 3] = ix / resultBuffer[start + i + 3];
                                    yDirections[start + i + 3] = iy / resultBuffer[start + i + 3];
                                }
                            } else {
                                resultBuffer[start + i + 3] = buffer[i + 3];
                            }
                        } else {
                            resultBuffer[start + i] = buffer[i];
                            resultBuffer[start + i + 1] = buffer[i + 1];
                            resultBuffer[start + i + 2] = buffer[i + 2];
                            resultBuffer[start + i + 3] = buffer[i + 3];
    
                            if (xDirections != null) {
                                xDirections[start + i] = 0;
                                yDirections[start + i] = 0;
                                xDirections[start + i + 1] = 0;
                                yDirections[start + i + 1] = 0;
                                xDirections[start + i + 2] = 0;
                                yDirections[start + i + 2] = 0;
                                xDirections[start + i + 3] = 0;
                                yDirections[start + i + 3] = 0;
                            }
                        }
                    }
                } else {
    
                    for (i = 0; (i < length) && !threadStopped; i++) {
    
                        if ((((start + i) % mod) == 0)) {
                            fireProgressStateChanged(((float) (start + i) / (totalLength - 1)) , null, null);
                         //   fireProgressStateChanged(Math.round((float) (start + i) / (totalLength - 1) * 100));
                        }
    
                        if (entireImage || mask.get(i)) {
                            // System.out.println("buffer length = " + buffer.length + " Gx2Data length = " +
                            // Gx2Data.length); System.out.println("buffer length = " + "buffer length = " + " Gx2Data
                            // length = " + "buffer length = " + "buffer length = "); System.out.println("buffer length = "
                            // + "buffer length = " + " Gx2Data length = " + "buffer length = " + "buffer length = ");
    
                            ix = AlgorithmConvolver.convolve2DPt(i, extents, buffer, kExtents, Gx2Data);
                            iy = AlgorithmConvolver.convolve2DPt(i, extents, buffer, kExtents, Gy2Data);
                            resultBuffer[start + i] = (float) Math.sqrt((ix * ix) + (iy * iy));
    
                            if (xDirections != null) {
    
                                if (resultBuffer[start + i] == 0) {
                                    xDirections[start + i] = ix;
                                    yDirections[start + i] = iy;
                                } else {
                                    xDirections[start + i] = ix / resultBuffer[start + i];
                                    yDirections[start + i] = iy / resultBuffer[start + i];
                                }
                            }
                        } else {
                            resultBuffer[start + i] = buffer[i];
    
                            if (xDirections != null) {
                                xDirections[start + i] = 0;
                                yDirections[start + i] = 0;
                            }
                        }
                    }
                }
            }
        } // if (buffer != null)
        else { // buffer == null
            fireProgressStateChanged(20, null, null);
            int mod = totalLength / 80; // mod is 1 percent of length
            for (s = 0; s < nImages; s++) {
                start = s * length;
    
                if (color == true) {
    
                    for (i = 0; (i < length) && !threadStopped; i += 4) {
    
                        if ((((start + i) % mod) == 0)) {
                            fireProgressStateChanged(20 + (80 * (start + i))/(totalLength - 1), null, null);
                        }
    
                        if (entireImage || mask.get(i / 4)) {
                            resultBuffer[start + i] = outputBuffer[2*start + 2*i];
    
                            if (red) {
                                ix = outputBuffer[2*start + 2*i + 2];
                                iy = outputBuffer[2*start + 2*i + 3];
                                resultBuffer[start + i + 1] = (float) Math.sqrt((ix * ix) + (iy * iy));
    
                                if (xDirections != null) {
                                    xDirections[start + i + 1] = ix / resultBuffer[start + i + 1];
                                    yDirections[start + i + 1] = iy / resultBuffer[start + i + 1];
                                }
                            } else {
                                resultBuffer[start + i + 1] = outputBuffer[2*start + 2*i + 2];
                            }
    
                            if (green) {
                                ix = outputBuffer[2*start + 2*i + 4];
                                iy = outputBuffer[2*start + 2*i + 5];
                                resultBuffer[start + i + 2] = (float) Math.sqrt((ix * ix) + (iy * iy));
    
                                if (xDirections != null) {
                                    xDirections[start + i + 2] = ix / resultBuffer[start + i + 2];
                                    yDirections[start + i + 2] = iy / resultBuffer[start + i + 2];
                                }
                            } else {
                                resultBuffer[start + i + 2] = outputBuffer[2*start + 2*i + 4];
                            }
    
                            if (blue) {
                                ix = outputBuffer[2*start + 2*i + 6];
                                iy = outputBuffer[2*start + 2*i + 7];
                                resultBuffer[start + i + 3] = (float) Math.sqrt((ix * ix) + (iy * iy));
    
                                if (xDirections != null) {
                                    xDirections[start + i + 3] = ix / resultBuffer[start + i + 3];
                                    yDirections[start + i + 3] = iy / resultBuffer[start + i + 3];
                                }
                            } else {
                                resultBuffer[start + i + 3] = outputBuffer[2*start + 2*i + 6];
                            }
                        } else {
                            resultBuffer[start + i] = outputBuffer[2*start + 2*i];
                            resultBuffer[start + i + 1] = outputBuffer[2*start + 2*i + 2];
                            resultBuffer[start + i + 2] = outputBuffer[2*start + 2*i + 4];
                            resultBuffer[start + i + 3] = outputBuffer[2*start + 2*i + 6];
    
                            if (xDirections != null) {
                                xDirections[start + i] = 0;
                                yDirections[start + i] = 0;
                                xDirections[start + i + 1] = 0;
                                yDirections[start + i + 1] = 0;
                                xDirections[start + i + 2] = 0;
                                yDirections[start + i + 2] = 0;
                                xDirections[start + i + 3] = 0;
                                yDirections[start + i + 3] = 0;
                            }
                        }
                    }
                } else {
    
                    for (i = 0; (i < length) && !threadStopped; i++) {
    
                        if ((((start + i) % mod) == 0)) {
                            fireProgressStateChanged(20 + (80 * (start + i))/(totalLength - 1), null, null);
                        }
    
                        if (entireImage || mask.get(i)) {
                            // System.out.println("buffer length = " + buffer.length + " Gx2Data length = " +
                            // Gx2Data.length); System.out.println("buffer length = " + "buffer length = " + " Gx2Data
                            // length = " + "buffer length = " + "buffer length = "); System.out.println("buffer length = "
                            // + "buffer length = " + " Gx2Data length = " + "buffer length = " + "buffer length = ");
    
                            ix = outputBuffer[2*start + 2*i];
                            iy = outputBuffer[2*start + 2*i + 1];
                            resultBuffer[start + i] = (float) Math.sqrt((ix * ix) + (iy * iy));
    
                            if (xDirections != null) {
    
                                if (resultBuffer[start + i] == 0) {
                                    xDirections[start + i] = ix;
                                    yDirections[start + i] = iy;
                                } else {
                                    xDirections[start + i] = ix / resultBuffer[start + i];
                                    yDirections[start + i] = iy / resultBuffer[start + i];
                                }
                            }
                        } else {
                            resultBuffer[start + i] = outputBuffer[2*start + 2*i];
    
                            if (xDirections != null) {
                                xDirections[start + i] = 0;
                                yDirections[start + i] = 0;
                            }
                        }
                    }
                }
            }    
        } // else buffer == null

        return resultBuffer;
    }

    /**
     * Calculates the gradient image and returns a float buffer with the new values.
     *
     * @param   nImages      number of images on which to calculate the gradient magnitude. If 2D image then nImage = 1.
     *                       If 3D image where each image is to processed independently then nImages equals the number
     *                       of images in the volume.
     * @param   buffer       DOCUMENT ME!
     * @param   extents      DOCUMENT ME!
     * @param   color        DOCUMENT ME!
     * @param   xDirections  DOCUMENT ME!
     * @param   yDirections  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private float[] calcInPlace2DBufferUnnormalized(int nImages, float[] buffer, int[] extents, boolean color,
                                                    float[] xDirections, float[] yDirections) {

        int i, s;
        int length, totalLength;
        int start;
        float[] resultBuffer;
        float ix, iy;
        int cFactor;

        if (color) {
            cFactor = 4;
        } else {
            cFactor = 1;
        }

        try {

            if (buffer == null) {
                length = cFactor * srcImage.getSliceSize();
            } else {
                length = buffer.length;
            }

            totalLength = length * nImages;
            resultBuffer = new float[length * nImages];

            if (srcImage != null) {
                fireProgressStateChanged(srcImage.getImageName(), "Calculating the gradient magnitude ...");
            }
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Gradient Magnitude exportData: Out of memory", true);

            return null;
        }

        int mod = totalLength / 100; // mod is 1 percent of length
        fireProgressStateChanged(0, null, null);

        if (buffer != null) {
            for (s = 0; s < nImages; s++) {
                start = s * length;
    
                if (color == true) {
    
                    for (i = 0; (i < length) && !threadStopped; i += 4) {
    
                        if ((((start + i) % mod) == 0)) {
                            fireProgressStateChanged( ((float) (start + i) / (totalLength - 1)), null, null);
                            //fireProgressStateChanged(Math.round((float) (start + i) / (totalLength - 1) * 100));
                        }
    
                        if ((entireImage == true) || mask.get(i / 4)) {
                            resultBuffer[start + i] = buffer[i];
    
                            if (red) {
                                ix = AlgorithmConvolver.convolve2DRGBPt(i + 1, extents, buffer, kExtents, Gx2Data);
                                iy = AlgorithmConvolver.convolve2DRGBPt(i + 1, extents, buffer, kExtents, Gy2Data);
                                resultBuffer[start + i + 1] = (float) Math.sqrt((ix * ix) + (iy * iy));
    
                                if (xDirections != null) {
                                    xDirections[start + i + 1] = ix;
                                    yDirections[start + i + 1] = iy;
                                }
                            } else {
                                resultBuffer[start + i + 1] = buffer[i + 1];
                            }
    
                            if (green) {
                                ix = AlgorithmConvolver.convolve2DRGBPt(i + 2, extents, buffer, kExtents, Gx2Data);
                                iy = AlgorithmConvolver.convolve2DRGBPt(i + 2, extents, buffer, kExtents, Gy2Data);
                                resultBuffer[start + i + 2] = (float) Math.sqrt((ix * ix) + (iy * iy));
    
                                if (xDirections != null) {
                                    xDirections[start + i + 2] = ix;
                                    yDirections[start + i + 2] = iy;
                                }
                            } else {
                                resultBuffer[start + i + 2] = buffer[i + 2];
                            }
    
                            if (blue) {
                                ix = AlgorithmConvolver.convolve2DRGBPt(i + 3, extents, buffer, kExtents, Gx2Data);
                                iy = AlgorithmConvolver.convolve2DRGBPt(i + 3, extents, buffer, kExtents, Gy2Data);
                                resultBuffer[start + i + 3] = (float) Math.sqrt((ix * ix) + (iy * iy));
    
                                if (xDirections != null) {
                                    xDirections[start + i + 3] = ix;
                                    yDirections[start + i + 3] = iy;
                                }
                            } else {
                                resultBuffer[start + i + 3] = buffer[i + 3];
                            }
                        } else {
                            resultBuffer[start + i] = buffer[i];
                            resultBuffer[start + i + 1] = buffer[i + 1];
                            resultBuffer[start + i + 2] = buffer[i + 2];
                            resultBuffer[start + i + 3] = buffer[i + 3];
    
                            if (xDirections != null) {
                                xDirections[start + i] = 0;
                                yDirections[start + i] = 0;
                                xDirections[start + i + 1] = 0;
                                yDirections[start + i + 1] = 0;
                                xDirections[start + i + 2] = 0;
                                yDirections[start + i + 2] = 0;
                                xDirections[start + i + 3] = 0;
                                yDirections[start + i + 3] = 0;
                            }
                        }
                    }
                } else {
    
                    for (i = 0; (i < length) && !threadStopped; i++) {
    
                        if ((((start + i) % mod) == 0)) {
                            fireProgressStateChanged( ((float) (start + i) / (totalLength - 1)), null, null);
                          //  fireProgressStateChanged(Math.round((float) (start + i) / (totalLength - 1) * 100));
                        }
    
                        if ((entireImage == true) || mask.get(i)) {
                            ix = AlgorithmConvolver.convolve2DPt(i, extents, buffer, kExtents, Gx2Data);
                            iy = AlgorithmConvolver.convolve2DPt(i, extents, buffer, kExtents, Gy2Data);
                            resultBuffer[start + i] = (float) Math.sqrt((ix * ix) + (iy * iy));
    
                            if (xDirections != null) {
    
                                if (resultBuffer[start + i] == 0) {
                                    xDirections[start + i] = ix;
                                    yDirections[start + i] = iy;
                                } else {
                                    xDirections[start + i] = ix;
                                    yDirections[start + i] = iy;
                                }
                            }
                        } else {
                            resultBuffer[start + i] = buffer[i];
    
                            if (xDirections != null) {
                                xDirections[start + i] = 0;
                                yDirections[start + i] = 0;
                            }
                        }
                    }
                }
            }
        } // if (buffer != null)
        else { // buffer == null
            for (s = 0; s < nImages; s++) {
                start = s * length;
                
                if (color == true) {
    
                    for (i = 0; (i < length) && !threadStopped; i += 4) {
    
                        if ((((start + i) % mod) == 0)) {
                            fireProgressStateChanged( ((float) (start + i) / (totalLength - 1)), null, null);
                            //fireProgressStateChanged(Math.round((float) (start + i) / (totalLength - 1) * 100));
                        }
    
                        if ((entireImage == true) || mask.get(i / 4)) {
                            resultBuffer[start + i] = outputBuffer[2*start + 2*i];
    
                            if (red) {
                                ix = outputBuffer[2*start + 2*i + 2];
                                iy = outputBuffer[2*start + 2*i + 3];
                                resultBuffer[start + i + 1] = (float) Math.sqrt((ix * ix) + (iy * iy));
    
                                if (xDirections != null) {
                                    xDirections[start + i + 1] = ix;
                                    yDirections[start + i + 1] = iy;
                                }
                            } else {
                                resultBuffer[start + i + 1] = outputBuffer[2*start + 2*i + 2];
                            }
    
                            if (green) {
                                ix = outputBuffer[2*start + 2*i + 4];
                                iy = outputBuffer[2*start + 2*i + 5];
                                resultBuffer[start + i + 2] = (float) Math.sqrt((ix * ix) + (iy * iy));
    
                                if (xDirections != null) {
                                    xDirections[start + i + 2] = ix;
                                    yDirections[start + i + 2] = iy;
                                }
                            } else {
                                resultBuffer[start + i + 2] = outputBuffer[2*start + 2*i + 4];
                            }
    
                            if (blue) {
                                ix = outputBuffer[2*start + 2*i + 6];
                                iy = outputBuffer[2*start + 2*i + 7];
                                resultBuffer[start + i + 3] = (float) Math.sqrt((ix * ix) + (iy * iy));
    
                                if (xDirections != null) {
                                    xDirections[start + i + 3] = ix;
                                    yDirections[start + i + 3] = iy;
                                }
                            } else {
                                resultBuffer[start + i + 3] = outputBuffer[2*start + 2*i + 6];
                            }
                        } else {
                            resultBuffer[start + i] = outputBuffer[2*start + 2*i];
                            resultBuffer[start + i + 1] = outputBuffer[2*start + 2*i + 2];
                            resultBuffer[start + i + 2] = outputBuffer[2*start + 2*i + 4];
                            resultBuffer[start + i + 3] = outputBuffer[2*start + 2*i + 6];
    
                            if (xDirections != null) {
                                xDirections[start + i] = 0;
                                yDirections[start + i] = 0;
                                xDirections[start + i + 1] = 0;
                                yDirections[start + i + 1] = 0;
                                xDirections[start + i + 2] = 0;
                                yDirections[start + i + 2] = 0;
                                xDirections[start + i + 3] = 0;
                                yDirections[start + i + 3] = 0;
                            }
                        }
                    }
                } else {
    
                    for (i = 0; (i < length) && !threadStopped; i++) {
    
                        if ((((start + i) % mod) == 0)) {
                            fireProgressStateChanged( ((float) (start + i) / (totalLength - 1)), null, null);
                          //  fireProgressStateChanged(Math.round((float) (start + i) / (totalLength - 1) * 100));
                        }
    
                        if ((entireImage == true) || mask.get(i)) {
                            ix = outputBuffer[2*start + 2*i];
                            iy = outputBuffer[2*start + 2*i + 1];
                            resultBuffer[start + i] = (float) Math.sqrt((ix * ix) + (iy * iy));
    
                            if (xDirections != null) {
    
                                if (resultBuffer[start + i] == 0) {
                                    xDirections[start + i] = ix;
                                    yDirections[start + i] = iy;
                                } else {
                                    xDirections[start + i] = ix;
                                    yDirections[start + i] = iy;
                                }
                            }
                        } else {
                            resultBuffer[start + i] = outputBuffer[2*start + 2*i];
    
                            if (xDirections != null) {
                                xDirections[start + i] = 0;
                                yDirections[start + i] = 0;
                            }
                        }
                    }
                }
            }
        } // else buffer == null

        return resultBuffer;
    }

    /**
     * Calculates the gradient magnitude image and replaces the source image with the new image.
     */
    private void calcInPlace34D() {       

        try {
            srcImage.importData(0, outputBuffer, true);
        } catch (IOException error) {
            errorCleanUp("Algorithm Gradient Magnitude importData: Image(s) locked", false);

            return;
        }

        
        setCompleted(true);
    }

    /**
     * This function produces the gradient magnitude of input image.
     *
     * @param  nImages  number of images on which to calculate the gradient magnitude. If 2D image then nImage = 1. If
     *                  3D image where each image is to processed independently then nImages equals the number of images
     *                  in the volume.
     */
    private void calcStoreInDest2D(int nImages) {

        int i, s, idx;
        int length, totalLength;
        int start;
        float ix, iy;
        boolean color = false;
        int cFactor = 1;

        if (srcImage.isColorImage()) {
            color = true;
            cFactor = 4;
        }

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp("Algorithm Gradient Magnitude: Image(s) locked", false);

            return;
        }

        try {
            length = cFactor * srcImage.getSliceSize();
            totalLength = length * nImages;
            fireProgressStateChanged(srcImage.getImageName(), "Calculating the gradient magnitude ...");
        } catch (OutOfMemoryError e) {
            System.gc();
            displayError("Algorithm Gradient Magnitude: Out of memory");
            setCompleted(false);
            
            destImage.releaseLock();

            return;
        }

        int mod = totalLength / 80; // mod is 1 percent of length
        fireProgressStateChanged(20, null, null);

        for (s = 0; s < nImages; s++) {
            start = s * length;

            if (color == true) {

                for (i = 0, idx = start; (i < length) && !threadStopped; i += 4, idx += 4) {

                    if ((((start + i) % mod) == 0)) {
                        fireProgressStateChanged(20 + (80 * (start + i))/ (totalLength - 1), null, null);
                    }

                    if ((entireImage == true) || mask.get(i / 4)) {
                        destImage.set(idx, outputBuffer[2*idx]); // alpha

                        if (red) {
                            ix = outputBuffer[2*idx + 2];
                            iy = outputBuffer[2*idx + 3];
                            destImage.set(idx + 1, (float) Math.sqrt((ix * ix) + (iy * iy)));
                        } else {
                            destImage.set(idx + 1, outputBuffer[2*idx + 2]);
                        }

                        if (green) {
                            ix = outputBuffer[2*idx + 4];
                            iy = outputBuffer[2*idx + 5];
                            destImage.set(idx + 2, (float) Math.sqrt((ix * ix) + (iy * iy)));
                        } else {
                            destImage.set(idx + 2, outputBuffer[2*idx + 4]);
                        }

                        if (blue) {
                            ix = outputBuffer[2*idx + 6];
                            iy = outputBuffer[2*idx + 7];
                            destImage.set(idx + 3, (float) Math.sqrt((ix * ix) + (iy * iy)));
                        } else {
                            destImage.set(idx + 3, outputBuffer[2*idx + 6]);
                        }
                    } else {
                        destImage.set(idx, outputBuffer[2*idx]);
                        destImage.set(idx + 1, outputBuffer[2*idx + 2]);
                        destImage.set(idx + 2, outputBuffer[2*idx + 4]);
                        destImage.set(idx + 3, outputBuffer[2*idx + 6]);
                    }
                }
            } else {

                for (i = 0, idx = start; (i < length) && !threadStopped; i++, idx++) {

                    if ((((start + i) % mod) == 0)) {
                        fireProgressStateChanged(20 + (80 * (start + i))/ (totalLength - 1), null, null);
                    }

                    if ((entireImage == true) || mask.get(i)) {
                        ix = outputBuffer[2*idx];
                        iy = outputBuffer[2*idx+1];
                        destImage.set(idx, (float) Math.sqrt((ix * ix) + (iy * iy)));
                        // destImage.set(idx, ix + iy);
                    } else {
                        destImage.set(idx, outputBuffer[2*idx]);
                    }
                }
            }
        }

        if (threadStopped) {
            finalize();

            return;
        }

        destImage.calcMinMax();
        destImage.releaseLock();
        
        setCompleted(true);
    }

    /**
     * This function produces the gradient magnitude of input image.
     */
    private void calcStoreInDest34D() {      

        try {
            destImage.importData(0, outputBuffer, true);
        } catch (IOException error) {
            errorCleanUp("Algorithm Gradient Magnitude importData: Image(s) locked", false);

            return;
        }

        
        setCompleted(true);
        
    }

    /**
     * Creates Gaussian derivative kernels.
     */
    private void makeKernels2D() {
        int xkDim, ykDim;
        int[] derivOrder = new int[2];

        kExtents = new int[2];
        derivOrder[0] = 1;
        derivOrder[1] = 0;

        xkDim = Math.round(8 * sigmas[0]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        if (xkDim < 3) {
            xkDim = 3;
        }

        kExtents[0] = xkDim;

        ykDim = Math.round(8 * sigmas[1]);

        if ((ykDim % 2) == 0) {
            ykDim++;
        }

        if (ykDim < 3) {
            ykDim = 3;
        }

        kExtents[1] = ykDim;

        Gx2Data = new float[xkDim * ykDim];

        GenerateGaussian Gx = new GenerateGaussian(Gx2Data, kExtents, sigmas, derivOrder);
        Gx.calc(false);

        derivOrder[0] = 0;
        derivOrder[1] = 1;
        Gy2Data = new float[xkDim * ykDim];

        GenerateGaussian Gy = new GenerateGaussian(Gy2Data, kExtents, sigmas, derivOrder);
        Gy.calc(true);
    }

    /**
     * Creates Gaussian derivative kernels.
     */
    private void makeKernels3D() {
        int xkDim, ykDim, zkDim;
        int[] derivOrder = new int[3];

        kExtents = new int[3];
        derivOrder[0] = 1;
        derivOrder[1] = 0;
        derivOrder[2] = 0;

        xkDim = Math.round(8 * sigmas[0]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        if (xkDim < 3) {
            xkDim = 3;
        }

        kExtents[0] = xkDim;

        ykDim = Math.round(8 * sigmas[1]);

        if ((ykDim % 2) == 0) {
            ykDim++;
        }

        if (ykDim < 3) {
            ykDim = 3;
        }

        kExtents[1] = ykDim;

        zkDim = Math.round(8 * sigmas[2]);

        if ((zkDim % 2) == 0) {
            zkDim++;
        }

        if (zkDim < 3) {
            zkDim = 3;
        }

        kExtents[2] = zkDim;


        GxData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gx = new GenerateGaussian(GxData, kExtents, sigmas, derivOrder);
        Gx.calc(false);

        derivOrder[0] = 0;
        derivOrder[1] = 1;
        derivOrder[2] = 0;
        GyData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gy = new GenerateGaussian(GyData, kExtents, sigmas, derivOrder);
        Gy.calc(true);

        derivOrder[0] = 0;
        derivOrder[1] = 0;
        derivOrder[2] = 1;
        GzData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gz = new GenerateGaussian(GzData, kExtents, sigmas, derivOrder);
        Gz.calc(true);
    }
    
    public void algorithmPerformed(AlgorithmBase algorithm){
        if(!algorithm.isCompleted()){
            finalize();
            return;
        }
        if (algorithm instanceof AlgorithmConvolver) {
            AlgorithmConvolver convolver = (AlgorithmConvolver) algorithm;
            outputBuffer = convolver.getOutputBuffer();
        }
    }

}
