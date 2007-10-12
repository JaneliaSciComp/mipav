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
public class AlgorithmGradientMagnitude extends AlgorithmBase {

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

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

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
        
        if (destImage != null) {

            if (srcImage.getNDims() == 2) {
                calcStoreInDest2D(1);
            } else if ((srcImage.getNDims() == 3) && (image25D == false)) {
                calcStoreInDest3D();
            } else if ((srcImage.getNDims() == 3) && (image25D == true)) {
                calcStoreInDest2D(srcImage.getExtents()[2]);
            } else if (srcImage.getNDims() == 4) {
                calcStoreInDest4D();
            }
        } else {

            if (srcImage.getNDims() == 2) {
                calcInPlace2D(1);
            } else if ((srcImage.getNDims() == 3) && (image25D == false)) {
                calcInPlace3D();
            } else if ((srcImage.getNDims() == 3) && (image25D == true)) {
                calcInPlace2D(srcImage.getExtents()[2]);
            } else if (srcImage.getNDims() == 4) {
                calcInPlace4D();
            }
        }
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
                buffer = new float[length];
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

        fireProgressStateChanged(0, null, null);

        
        int mod = totalLength / 100; // mod is 1 percent of length
        //fireProgressStateChanged(0, null, null);

        for (s = 0; s < nImages; s++) {
            start = s * length;

            try {

                if (srcImage != null) {
                    srcImage.exportData(start, length, buffer); // locks and releases lock
                }
            } catch (IOException error) {
                errorCleanUp("Algorithm Gradient Magnitude: " + error, false);

                return null;
            }

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
                buffer = new float[length];
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

        for (s = 0; s < nImages; s++) {
            start = s * length;

            try {

                if (srcImage != null) {
                    srcImage.exportData(start, length, buffer); // locks and releases lock
                }
            } catch (IOException error) {
                errorCleanUp("Algorithm Gradient Magnitude: " + error, false);

                return null;
            }

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

        return resultBuffer;
    }

    /**
     * Calculates the gradient magnitude image and replaces the source image with the new image.
     */
    private void calcInPlace3D() {

        int i, j;
        int length;
        float[] buffer, buffer2D;
        float[] resultBuffer;
        float ix, iy, iz;
        int offsetZ;
        boolean color = false;
        int cFactor = 1;
        int curSlice, cur2DBufferSlice = -1;

        if (srcImage.isColorImage() == true) {
            color = true;
            cFactor = 4;
        }

        try {
            length = cFactor * srcImage.getSliceSize() * srcImage.getExtents()[2];
            buffer = new float[length];
            resultBuffer = new float[length];
            buffer2D = new float[cFactor * srcImage.getSliceSize()];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            fireProgressStateChanged(srcImage.getImageName(), "Calculating the gradient magnitude ...");
        } catch (IOException error) {
            errorCleanUp("Algorithm Gradient Magnitude exportData: Image(s) locked", false);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Gradient Magnitude exportData: Out of memory", true);

            return;
        }

        fireProgressStateChanged(0, null, null);

        int mod = length / 100; // mod is 1 percent of length

        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int sliceSize = cFactor * xDim * yDim;

        if (color == true) {

            for (i = 0; (i < length) && !threadStopped; i += 4) {

                if ((((i) % mod) == 0)) {
                    fireProgressStateChanged( ((float) i / (length - 1)), null, null);
                  //  fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
                }

                curSlice = i / sliceSize;
                offsetZ = curSlice - (kExtents[2] / 2);

                if ((offsetZ >= 0) && ((offsetZ + kExtents[2] - 1) < zDim)) {

                    if ((entireImage == true) || mask.get(i / 4)) {
                        resultBuffer[i] = buffer[i];

                        if (red) {
                            ix = AlgorithmConvolver.convolve3DRGBPt(i + 1, srcImage.getExtents(), buffer, kExtents,
                                                                    GxData);
                            iy = AlgorithmConvolver.convolve3DRGBPt(i + 1, srcImage.getExtents(), buffer, kExtents,
                                                                    GyData);
                            iz = AlgorithmConvolver.convolve3DRGBPt(i + 1, srcImage.getExtents(), buffer, kExtents,
                                                                    GzData);

                            resultBuffer[i + 1] = (float) Math.sqrt((ix * ix) + (iy * iy) + (iz * iz));
                        } else {
                            resultBuffer[i + 1] = buffer[i + 1];
                        }

                        if (green) {
                            ix = AlgorithmConvolver.convolve3DRGBPt(i + 2, srcImage.getExtents(), buffer, kExtents,
                                                                    GxData);
                            iy = AlgorithmConvolver.convolve3DRGBPt(i + 2, srcImage.getExtents(), buffer, kExtents,
                                                                    GyData);
                            iz = AlgorithmConvolver.convolve3DRGBPt(i + 2, srcImage.getExtents(), buffer, kExtents,
                                                                    GzData);

                            resultBuffer[i + 2] = (float) Math.sqrt((ix * ix) + (iy * iy) + (iz * iz));
                        } else {
                            resultBuffer[i + 2] = buffer[i + 2];
                        }

                        if (blue) {
                            ix = AlgorithmConvolver.convolve3DRGBPt(i + 3, srcImage.getExtents(), buffer, kExtents,
                                                                    GxData);
                            iy = AlgorithmConvolver.convolve3DRGBPt(i + 3, srcImage.getExtents(), buffer, kExtents,
                                                                    GyData);
                            iz = AlgorithmConvolver.convolve3DRGBPt(i + 3, srcImage.getExtents(), buffer, kExtents,
                                                                    GzData);

                            resultBuffer[i + 3] = (float) Math.sqrt((ix * ix) + (iy * iy) + (iz * iz));
                        } else {
                            resultBuffer[i + 3] = buffer[i + 3];
                        }
                    }
                } else {

                    if ((entireImage == true) || mask.get(i / 4)) {

                        if (cur2DBufferSlice != curSlice) {

                            // we haven't calculated the 2D buffers for this slice, so we make them now
                            System.arraycopy(buffer, curSlice * sliceSize, buffer2D, 0, sliceSize);

                            cur2DBufferSlice = curSlice;
                        }

                        j = i % sliceSize;

                        resultBuffer[i] = buffer[i];

                        if (red) {
                            ix = AlgorithmConvolver.convolve2DRGBPt(j + 1, srcImage.getExtents(), buffer2D, kExtents,
                                                                    Gx2Data);
                            iy = AlgorithmConvolver.convolve2DRGBPt(j + 1, srcImage.getExtents(), buffer2D, kExtents,
                                                                    Gy2Data);
                            resultBuffer[i + 1] = (float) Math.sqrt((ix * ix) + (iy * iy));
                        }

                        if (green) {
                            ix = AlgorithmConvolver.convolve2DRGBPt(j + 2, srcImage.getExtents(), buffer2D, kExtents,
                                                                    Gx2Data);
                            iy = AlgorithmConvolver.convolve2DRGBPt(j + 2, srcImage.getExtents(), buffer2D, kExtents,
                                                                    Gy2Data);
                            resultBuffer[i + 2] = (float) Math.sqrt((ix * ix) + (iy * iy));
                        }

                        if (blue) {
                            ix = AlgorithmConvolver.convolve2DRGBPt(j + 3, srcImage.getExtents(), buffer2D, kExtents,
                                                                    Gx2Data);
                            iy = AlgorithmConvolver.convolve2DRGBPt(j + 3, srcImage.getExtents(), buffer2D, kExtents,
                                                                    Gy2Data);
                            resultBuffer[i + 3] = (float) Math.sqrt((ix * ix) + (iy * iy));
                        }
                    } else {
                        resultBuffer[i] = buffer[i];
                        resultBuffer[i + 1] = buffer[i + 1];
                        resultBuffer[i + 2] = buffer[i + 2];
                        resultBuffer[i + 3] = buffer[i + 3];
                    }
                }
            }
        } else {

            for (i = 0; (i < length) && !threadStopped; i++) {

                if (((i % mod) == 0)) {
                    fireProgressStateChanged( ((float) i / (length - 1)), null, null);
                    //fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
                }

                curSlice = i / sliceSize;
                offsetZ = curSlice - (kExtents[2] / 2);

                if ((offsetZ >= 0) && ((offsetZ + kExtents[2] - 1) < zDim)) {

                    if ((entireImage == true) || mask.get(i)) {
                        ix = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GxData);
                        iy = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GyData);
                        iz = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GzData);
                        resultBuffer[i] = (float) Math.sqrt((ix * ix) + (iy * iy) + (iz * iz));
                    } else {
                        resultBuffer[i] = buffer[i];
                    }
                } else {

                    if ((entireImage == true) || mask.get(i)) {

                        if (cur2DBufferSlice != curSlice) {

                            // we haven't calculated the 2D buffers for this slice, so we make them now
                            System.arraycopy(buffer, curSlice * sliceSize, buffer2D, 0, sliceSize);

                            cur2DBufferSlice = curSlice;
                        }

                        j = i % sliceSize;

                        ix = AlgorithmConvolver.convolve2DPt(j, srcImage.getExtents(), buffer2D, kExtents, Gx2Data);
                        iy = AlgorithmConvolver.convolve2DPt(j, srcImage.getExtents(), buffer2D, kExtents, Gy2Data);
                        resultBuffer[i] = (float) Math.sqrt((ix * ix) + (iy * iy));
                    } else {
                        resultBuffer[i] = buffer[i];
                    }
                }
            }
        }

        if (threadStopped) {
            finalize();

            return;
        }

        buffer = null;
        System.gc();

        try {
            srcImage.importData(0, resultBuffer, true);
        } catch (IOException error) {
            errorCleanUp("Algorithm Gradient Magnitude importData: Image(s) locked", false);

            return;
        }

        
        setCompleted(true);
    }

    /**
     * Calculates the gradient magnitude image and replaces the source image with the new image.
     */
    private void calcInPlace4D() {

        int i, t;
        int length;
        float[] buffer;
        float[] resultBuffer;
        float ix, iy, iz;
        int offsetZ;
        boolean color = false;
        int cFactor = 1;

        if (srcImage.isColorImage() == true) {
            color = true;
            cFactor = 4;
        }

        try {
            length = cFactor * srcImage.getSliceSize() * srcImage.getExtents()[2];
            buffer = new float[length];
            resultBuffer = new float[length];
            fireProgressStateChanged(srcImage.getImageName(), "Calculating the gradient magnitude ...");
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Gradient Magnitude exportData: Out of memory", true);

            return;
        }

        fireProgressStateChanged(0, null, null);

        int mod = length / 100; // mod is 1 percent of length

        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int sliceSize = cFactor * xDim * yDim;
        int index;
        int end = srcImage.getExtents()[3];

        for (t = 0; t < end; t++) {

            try {
                srcImage.exportData(t * length, length, buffer); // locks and releases lock
            } catch (IOException error) {
                displayError("Algorithm Gradient Magnitude exportData: Image(s) locked");
                setCompleted(false);
                

                return;
            }

           // fireProgressStateChanged(Math.round((float) t / end * 100));
            fireProgressStateChanged( ((float) t / end), null, null);

            index = t * length;

            if (color == true) {

                for (i = 0; (i < length) && !threadStopped; i += 4) {
                    offsetZ = (i / sliceSize) - (kExtents[2] / 2);

                    if ((offsetZ >= 0) && ((offsetZ + kExtents[2] - 1) < zDim)) {

                        if ((entireImage == true) || mask.get(i / 4)) {
                            resultBuffer[i] = buffer[i];

                            if (red) {
                                ix = AlgorithmConvolver.convolve3DRGBPt(i + 1, srcImage.getExtents(), buffer, kExtents,
                                                                        GxData);
                                iy = AlgorithmConvolver.convolve3DRGBPt(i + 1, srcImage.getExtents(), buffer, kExtents,
                                                                        GyData);
                                iz = AlgorithmConvolver.convolve3DRGBPt(i + 1, srcImage.getExtents(), buffer, kExtents,
                                                                        GzData);

                                resultBuffer[i + 1] = (float) Math.sqrt((ix * ix) + (iy * iy) + (iz * iz));
                            } else {
                                resultBuffer[i + 1] = buffer[i + 1];
                            }

                            if (green) {
                                ix = AlgorithmConvolver.convolve3DRGBPt(i + 2, srcImage.getExtents(), buffer, kExtents,
                                                                        GxData);
                                iy = AlgorithmConvolver.convolve3DRGBPt(i + 2, srcImage.getExtents(), buffer, kExtents,
                                                                        GyData);
                                iz = AlgorithmConvolver.convolve3DRGBPt(i + 2, srcImage.getExtents(), buffer, kExtents,
                                                                        GzData);

                                resultBuffer[i + 2] = (float) Math.sqrt((ix * ix) + (iy * iy) + (iz * iz));
                            } else {
                                resultBuffer[i + 2] = buffer[i + 2];
                            }

                            if (blue) {
                                ix = AlgorithmConvolver.convolve3DRGBPt(i + 3, srcImage.getExtents(), buffer, kExtents,
                                                                        GxData);
                                iy = AlgorithmConvolver.convolve3DRGBPt(i + 3, srcImage.getExtents(), buffer, kExtents,
                                                                        GyData);
                                iz = AlgorithmConvolver.convolve3DRGBPt(i + 3, srcImage.getExtents(), buffer, kExtents,
                                                                        GzData);

                                resultBuffer[i + 3] = (float) Math.sqrt((ix * ix) + (iy * iy) + (iz * iz));
                            } else {
                                resultBuffer[i + 3] = buffer[i + 3];
                            }
                        }
                    } else {

                        if ((entireImage == true) || mask.get(i / 4)) {
                            resultBuffer[i] = buffer[i];

                            if (red) {
                                ix = AlgorithmConvolver.convolve2DRGBPt(i + 1, srcImage.getExtents(), buffer, kExtents,
                                                                        Gx2Data);
                                iy = AlgorithmConvolver.convolve2DRGBPt(i + 1, srcImage.getExtents(), buffer, kExtents,
                                                                        Gy2Data);
                                resultBuffer[i + 1] = (float) Math.sqrt((ix * ix) + (iy * iy));
                            }

                            if (green) {
                                ix = AlgorithmConvolver.convolve2DRGBPt(i + 2, srcImage.getExtents(), buffer, kExtents,
                                                                        Gx2Data);
                                iy = AlgorithmConvolver.convolve2DRGBPt(i + 2, srcImage.getExtents(), buffer, kExtents,
                                                                        Gy2Data);
                                resultBuffer[i + 2] = (float) Math.sqrt((ix * ix) + (iy * iy));
                            }

                            if (blue) {
                                ix = AlgorithmConvolver.convolve2DRGBPt(i + 3, srcImage.getExtents(), buffer, kExtents,
                                                                        Gx2Data);
                                iy = AlgorithmConvolver.convolve2DRGBPt(i + 3, srcImage.getExtents(), buffer, kExtents,
                                                                        Gy2Data);
                                resultBuffer[i + 3] = (float) Math.sqrt((ix * ix) + (iy * iy));
                            }
                        } else {
                            resultBuffer[i] = buffer[i];
                            resultBuffer[i + 1] = buffer[i + 1];
                            resultBuffer[i + 2] = buffer[i + 2];
                            resultBuffer[i + 3] = buffer[i + 3];
                        }
                    }
                }
            } else {

                for (i = 0; (i < length) && !threadStopped; i++) {

                    if (((i % mod) == 0)) {
                        fireProgressStateChanged( ((float) i / (length - 1)), null, null);
                       // fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
                    }

                    offsetZ = (i / sliceSize) - (kExtents[2] / 2);

                    if ((offsetZ >= 0) && ((offsetZ + kExtents[2] - 1) < zDim)) {

                        if ((entireImage == true) || mask.get(i)) {
                            ix = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GxData);
                            iy = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GyData);
                            iz = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GzData);
                            resultBuffer[i] = (float) Math.sqrt((ix * ix) + (iy * iy) + (iz * iz));
                        } else {
                            resultBuffer[i] = buffer[i];
                        }
                    } else {

                        if ((entireImage == true) || mask.get(i)) {
                            ix = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), buffer, kExtents, Gx2Data);
                            iy = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), buffer, kExtents, Gy2Data);
                            resultBuffer[i] = (float) Math.sqrt((ix * ix) + (iy * iy));
                        } else {
                            resultBuffer[i] = buffer[i];
                        }
                    }
                }
            }

            try {
                srcImage.importData(index, resultBuffer, true);
            } catch (IOException error) {
                displayError("Algorithm Gradient Magnitude importData: Image(s) locked");
                setCompleted(false);
                

                return;
            }
        }

        if (threadStopped) {
            finalize();

            return;
        }

        resultBuffer = null;
        buffer = null;
        System.gc();
        
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
        float[] buffer;
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
            buffer = new float[length];
            fireProgressStateChanged(srcImage.getImageName(), "Calculating the gradient magnitude ...");
        } catch (OutOfMemoryError e) {
            buffer = null;
            System.gc();
            displayError("Algorithm Gradient Magnitude: Out of memory");
            setCompleted(false);
            
            destImage.releaseLock();

            return;
        }

        int mod = totalLength / 100; // mod is 1 percent of length
        fireProgressStateChanged(0, null, null);

        for (s = 0; s < nImages; s++) {
            start = s * length;

            try {
                srcImage.exportData(start, length, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                errorCleanUp("Algorithm Gradient Magnitude: Image(s) locked", true);

                return;
            }

            if (color == true) {

                for (i = 0, idx = start; (i < length) && !threadStopped; i += 4, idx += 4) {

                    if ((((start + i) % mod) == 0)) {
                        fireProgressStateChanged( ((float) (start + i) / (totalLength - 1)), null, null);
                      //  fireProgressStateChanged(Math.round((float) (start + i) / (totalLength - 1) * 100));
                    }

                    if ((entireImage == true) || mask.get(i / 4)) {
                        destImage.set(idx, buffer[i]); // alpha

                        if (red) {
                            ix = AlgorithmConvolver.convolve2DRGBPt(i + 1, srcImage.getExtents(), buffer, kExtents,
                                                                    Gx2Data);
                            iy = AlgorithmConvolver.convolve2DRGBPt(i + 1, srcImage.getExtents(), buffer, kExtents,
                                                                    Gy2Data);
                            destImage.set(idx + 1, (float) Math.sqrt((ix * ix) + (iy * iy)));
                        } else {
                            destImage.set(idx + 1, buffer[i + 1]);
                        }

                        if (green) {
                            ix = AlgorithmConvolver.convolve2DRGBPt(i + 2, srcImage.getExtents(), buffer, kExtents,
                                                                    Gx2Data);
                            iy = AlgorithmConvolver.convolve2DRGBPt(i + 2, srcImage.getExtents(), buffer, kExtents,
                                                                    Gy2Data);
                            destImage.set(idx + 2, (float) Math.sqrt((ix * ix) + (iy * iy)));
                        } else {
                            destImage.set(idx + 2, buffer[i + 2]);
                        }

                        if (blue) {
                            ix = AlgorithmConvolver.convolve2DRGBPt(i + 3, srcImage.getExtents(), buffer, kExtents,
                                                                    Gx2Data);
                            iy = AlgorithmConvolver.convolve2DRGBPt(i + 3, srcImage.getExtents(), buffer, kExtents,
                                                                    Gy2Data);
                            destImage.set(idx + 3, (float) Math.sqrt((ix * ix) + (iy * iy)));
                        } else {
                            destImage.set(idx + 3, buffer[i + 3]);
                        }
                    } else {
                        destImage.set(idx, buffer[i]);
                        destImage.set(idx + 1, buffer[i + 1]);
                        destImage.set(idx + 2, buffer[i + 2]);
                        destImage.set(idx + 3, buffer[i + 3]);
                    }
                }
            } else {

                for (i = 0, idx = start; (i < length) && !threadStopped; i++, idx++) {

                    if ((((start + i) % mod) == 0)) {
                        fireProgressStateChanged( ((float) (start + i) / (totalLength - 1)), null, null);
                       // fireProgressStateChanged(Math.round((float) (start + i) / (totalLength - 1) * 100));
                    }

                    if ((entireImage == true) || mask.get(i)) {
                        ix = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), buffer, kExtents, Gx2Data);
                        iy = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), buffer, kExtents, Gy2Data);
                        destImage.set(idx, (float) Math.sqrt((ix * ix) + (iy * iy)));
                        // destImage.set(idx, ix + iy);
                    } else {
                        destImage.set(idx, buffer[i]);
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
    private void calcStoreInDest3D() {

        int i, j;
        int length;
        float[] buffer2D;
        float[] buffer;
        float ix, iy, iz;
        int offsetZ;
        boolean color = false;
        int cFactor = 1;
        int cur2DBufferSlice = -1, curSlice;


        if (srcImage.isColorImage()) {
            color = true;
            cFactor = 4;
        }

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp(" GradientMagnitude: Image(s) locked", false);

            return;
        }

        try {
            length = cFactor * srcImage.getSliceSize() * srcImage.getExtents()[2];
            buffer = new float[length];
            buffer2D = new float[srcImage.getSliceSize() * cFactor];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            fireProgressStateChanged(srcImage.getImageName(), "Calculating the gradient magnitude ...");
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm Gradient Magnitude exportData: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            System.gc();
            displayError("Algorithm Gradient Magnitude exportData: Out of memory");

            return;
        }

        fireProgressStateChanged(0, null, null);

        int mod = length / 100; // mod is 1 percent of length
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int sliceSize = cFactor * xDim * yDim;

        if (color == true) {

            for (i = 0; (i < length) && !threadStopped; i += 4) {

                if ((((i) % mod) == 0)) {
                    fireProgressStateChanged( ((float) (i) / (length - 1)), null, null);
                  //  fireProgressStateChanged(Math.round((float) (i) / (length - 1) * 100));
                }

                curSlice = i / sliceSize;
                offsetZ = curSlice - (kExtents[2] / 2);

                if ((offsetZ >= 0) && ((offsetZ + kExtents[2] - 1) < zDim)) {

                    if ((entireImage == true) || mask.get(i / 4)) {
                        destImage.set(i, buffer[i]);

                        if (red) {
                            ix = AlgorithmConvolver.convolve3DRGBPt(i + 1, srcImage.getExtents(), buffer, kExtents,
                                                                    GxData);
                            iy = AlgorithmConvolver.convolve3DRGBPt(i + 1, srcImage.getExtents(), buffer, kExtents,
                                                                    GyData);
                            iz = AlgorithmConvolver.convolve3DRGBPt(i + 1, srcImage.getExtents(), buffer, kExtents,
                                                                    GzData);
                            destImage.set(i + 1, (float) Math.sqrt((ix * ix) + (iy * iy) + (iz * iz)));
                        } else {
                            destImage.set(i + 1, buffer[i + 1]);
                        }

                        if (green) {
                            ix = AlgorithmConvolver.convolve3DRGBPt(i + 2, srcImage.getExtents(), buffer, kExtents,
                                                                    GxData);
                            iy = AlgorithmConvolver.convolve3DRGBPt(i + 2, srcImage.getExtents(), buffer, kExtents,
                                                                    GyData);
                            iz = AlgorithmConvolver.convolve3DRGBPt(i + 2, srcImage.getExtents(), buffer, kExtents,
                                                                    GzData);
                            destImage.set(i + 2, (float) Math.sqrt((ix * ix) + (iy * iy) + (iz * iz)));
                        } else {
                            destImage.set(i + 2, buffer[i + 2]);
                        }

                        if (blue) {
                            ix = AlgorithmConvolver.convolve3DRGBPt(i + 3, srcImage.getExtents(), buffer, kExtents,
                                                                    GxData);
                            iy = AlgorithmConvolver.convolve3DRGBPt(i + 3, srcImage.getExtents(), buffer, kExtents,
                                                                    GyData);
                            iz = AlgorithmConvolver.convolve3DRGBPt(i + 3, srcImage.getExtents(), buffer, kExtents,
                                                                    GzData);
                            destImage.set(i + 3, (float) Math.sqrt((ix * ix) + (iy * iy) + (iz * iz)));
                        } else {
                            destImage.set(i + 3, buffer[i + 3]);
                        }
                    }
                } else {

                    if ((entireImage == true) || mask.get(i / 4)) {

                        if (cur2DBufferSlice != curSlice) {

                            // we haven't calculated the 2D buffers for this slice, so we make them now
                            System.arraycopy(buffer, curSlice * sliceSize, buffer2D, 0, sliceSize);

                            cur2DBufferSlice = curSlice;
                        }

                        j = i % sliceSize;
                        destImage.set(i, buffer[i]);

                        if (red) {
                            ix = AlgorithmConvolver.convolve2DRGBPt(j + 1, srcImage.getExtents(), buffer2D, kExtents,
                                                                    Gx2Data);
                            iy = AlgorithmConvolver.convolve2DRGBPt(j + 1, srcImage.getExtents(), buffer2D, kExtents,
                                                                    Gy2Data);
                            destImage.set(i + 1, (float) Math.sqrt((ix * ix) + (iy * iy)));
                        } else {
                            destImage.set(i + 1, buffer[i + 1]);
                        }

                        if (green) {
                            ix = AlgorithmConvolver.convolve2DRGBPt(j + 2, srcImage.getExtents(), buffer2D, kExtents,
                                                                    Gx2Data);
                            iy = AlgorithmConvolver.convolve2DRGBPt(j + 2, srcImage.getExtents(), buffer2D, kExtents,
                                                                    Gy2Data);
                            destImage.set(i + 2, (float) Math.sqrt((ix * ix) + (iy * iy)));
                        } else {
                            destImage.set(i + 2, buffer[i + 2]);
                        }

                        if (blue) {
                            ix = AlgorithmConvolver.convolve2DRGBPt(j + 3, srcImage.getExtents(), buffer2D, kExtents,
                                                                    Gx2Data);
                            iy = AlgorithmConvolver.convolve2DRGBPt(j + 3, srcImage.getExtents(), buffer2D, kExtents,
                                                                    Gy2Data);
                            destImage.set(i + 3, (float) Math.sqrt((ix * ix) + (iy * iy)));
                        } else {
                            destImage.set(i + 3, buffer[i + 3]);
                        }
                    } else {
                        destImage.set(i, buffer[i]);
                        destImage.set(i + 1, buffer[i + 1]);
                        destImage.set(i + 2, buffer[i + 2]);
                        destImage.set(i + 3, buffer[i + 3]);
                    }
                }
            }
        } else {

            for (i = 0; (i < length) && !threadStopped; i++) {

                if (((i % mod) == 0)) {
                    fireProgressStateChanged( ((float) i / (length - 1)), null, null);
                  //  fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
                }

                curSlice = i / sliceSize;
                offsetZ = curSlice - (kExtents[2] / 2);

                if ((offsetZ >= 0) && ((offsetZ + kExtents[2] - 1) < zDim)) {

                    if ((entireImage == true) || mask.get(i)) {
                        ix = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GxData);
                        iy = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GyData);
                        iz = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GzData);
                        destImage.set(i, (float) Math.sqrt((ix * ix) + (iy * iy) + (iz * iz)));
                    } else {
                        destImage.set(i, buffer[i]);
                    }
                } else {

                    if ((entireImage == true) || mask.get(i)) {

                        if (cur2DBufferSlice != curSlice) {

                            // we haven't calculated the 2D buffers for this slice, so we make them now
                            System.arraycopy(buffer, curSlice * sliceSize, buffer2D, 0, sliceSize);

                            cur2DBufferSlice = curSlice;
                        }

                        j = i % sliceSize;
                        ix = AlgorithmConvolver.convolve2DPt(j, srcImage.getExtents(), buffer2D, kExtents, Gx2Data);
                        iy = AlgorithmConvolver.convolve2DPt(j, srcImage.getExtents(), buffer2D, kExtents, Gy2Data);
                        destImage.set(i, (float) Math.sqrt((ix * ix) + (iy * iy)));
                    } else {
                        destImage.set(i, buffer[i]);
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
    private void calcStoreInDest4D() {

        int i, t;
        int length;
        float[] buffer;
        float ix, iy, iz;
        int offsetZ;
        boolean color = false;
        int cFactor = 1;

        if (srcImage.isColorImage() == true) {
            color = true;
            cFactor = 4;
        }

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp(" GradientMagnitude: Image(s) locked", false);

            return;
        }

        try {
            length = cFactor * srcImage.getSliceSize() * srcImage.getExtents()[2];
            buffer = new float[length];
            fireProgressStateChanged(srcImage.getImageName(), "Calculating the gradient magnitude ...");
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Gradient Magnitude exportData: Out of memory", true);

            return;
        }

        fireProgressStateChanged(0, null, null);

        int mod = length / 100; // mod is 1 percent of length
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int sliceSize = cFactor * xDim * yDim;
        int end = srcImage.getExtents()[3];
        int index;

        for (t = 0; t < end; t++) {

            try {
                srcImage.exportData(t * length, length, buffer); // locks and releases lock
            } catch (IOException error) {
                displayError("Algorithm Gradient Magnitude exportData: Image(s) locked");
                setCompleted(false);
                
                destImage.releaseLock();

                return;
            }

               // fireProgressStateChanged(Math.round((float) t / end * 100));
            fireProgressStateChanged(((float) t / end), null, null);

            index = t * length;

            if (color == true) {

                for (i = 0; (i < length) && !threadStopped; i += 4) {
                    offsetZ = (i / sliceSize) - (kExtents[2] / 2);

                    if ((offsetZ >= 0) && ((offsetZ + kExtents[2] - 1) < zDim)) {

                        if ((entireImage == true) || mask.get(i / 4)) {
                            destImage.set(i + index, buffer[i]);

                            if (red) {
                                ix = AlgorithmConvolver.convolve3DRGBPt(i + 1, srcImage.getExtents(), buffer, kExtents,
                                                                        GxData);
                                iy = AlgorithmConvolver.convolve3DRGBPt(i + 1, srcImage.getExtents(), buffer, kExtents,
                                                                        GyData);
                                iz = AlgorithmConvolver.convolve3DRGBPt(i + 1, srcImage.getExtents(), buffer, kExtents,
                                                                        GzData);
                                destImage.set(i + index + 1, (float) Math.sqrt((ix * ix) + (iy * iy) + (iz * iz)));
                            } else {
                                destImage.set(i + index + 1, buffer[i + 1]);
                            }

                            if (green) {
                                ix = AlgorithmConvolver.convolve3DRGBPt(i + 2, srcImage.getExtents(), buffer, kExtents,
                                                                        GxData);
                                iy = AlgorithmConvolver.convolve3DRGBPt(i + 2, srcImage.getExtents(), buffer, kExtents,
                                                                        GyData);
                                iz = AlgorithmConvolver.convolve3DRGBPt(i + 2, srcImage.getExtents(), buffer, kExtents,
                                                                        GzData);
                                destImage.set(i + index + 2, (float) Math.sqrt((ix * ix) + (iy * iy) + (iz * iz)));
                            } else {
                                destImage.set(i + index + 2, buffer[i + 2]);
                            }

                            if (blue) {
                                ix = AlgorithmConvolver.convolve3DRGBPt(i + 3, srcImage.getExtents(), buffer, kExtents,
                                                                        GxData);
                                iy = AlgorithmConvolver.convolve3DRGBPt(i + 3, srcImage.getExtents(), buffer, kExtents,
                                                                        GyData);
                                iz = AlgorithmConvolver.convolve3DRGBPt(i + 3, srcImage.getExtents(), buffer, kExtents,
                                                                        GzData);
                                destImage.set(i + index + 3, (float) Math.sqrt((ix * ix) + (iy * iy) + (iz * iz)));
                            } else {
                                destImage.set(i + index + 3, buffer[i + 3]);
                            }
                        }
                    } else {

                        if ((entireImage == true) || mask.get(i / 4)) {
                            destImage.set(i, buffer[i]);

                            if (red) {
                                ix = AlgorithmConvolver.convolve2DRGBPt(i + 1, srcImage.getExtents(), buffer, kExtents,
                                                                        Gx2Data);
                                iy = AlgorithmConvolver.convolve2DRGBPt(i + 1, srcImage.getExtents(), buffer, kExtents,
                                                                        Gy2Data);
                                destImage.set(i + index + 1, (float) Math.sqrt((ix * ix) + (iy * iy)));
                            } else {
                                destImage.set(i + index + 1, buffer[i + 1]);
                            }

                            if (green) {
                                ix = AlgorithmConvolver.convolve2DRGBPt(i + 2, srcImage.getExtents(), buffer, kExtents,
                                                                        Gx2Data);
                                iy = AlgorithmConvolver.convolve2DRGBPt(i + 2, srcImage.getExtents(), buffer, kExtents,
                                                                        Gy2Data);
                                destImage.set(i + index + 2, (float) Math.sqrt((ix * ix) + (iy * iy)));
                            } else {
                                destImage.set(i + index + 2, buffer[i + 2]);
                            }

                            if (blue) {
                                ix = AlgorithmConvolver.convolve2DRGBPt(i + 3, srcImage.getExtents(), buffer, kExtents,
                                                                        Gx2Data);
                                iy = AlgorithmConvolver.convolve2DRGBPt(i + 3, srcImage.getExtents(), buffer, kExtents,
                                                                        Gy2Data);
                                destImage.set(i + index + 3, (float) Math.sqrt((ix * ix) + (iy * iy)));
                            } else {
                                destImage.set(i + index + 3, buffer[i + 3]);
                            }
                        } else {
                            destImage.set(i + index, buffer[i]);
                            destImage.set(i + index + 1, buffer[i + 1]);
                            destImage.set(i + index + 2, buffer[i + 2]);
                            destImage.set(i + index + 3, buffer[i + 3]);
                        }
                    }
                }
            } else {

                for (i = 0; (i < length) && !threadStopped; i++) {

                    if (((i % mod) == 0)) {
                        fireProgressStateChanged( ((float) i / (length - 1)), null, null);
                     //   fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
                    }

                    offsetZ = (i / sliceSize) - (kExtents[2] / 2);

                    if ((offsetZ >= 0) && ((offsetZ + kExtents[2] - 1) < zDim)) {

                        if ((entireImage == true) || mask.get(i)) {
                            ix = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GxData);
                            iy = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GyData);
                            iz = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GzData);
                            destImage.set(i + index, (float) Math.sqrt((ix * ix) + (iy * iy) + (iz * iz)));
                        } else {
                            destImage.set(i + index, buffer[i]);
                        }
                    } else {

                        if ((entireImage == true) || mask.get(i)) {
                            ix = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), buffer, kExtents, Gx2Data);
                            iy = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), buffer, kExtents, Gy2Data);
                            destImage.set(i + index, (float) Math.sqrt((ix * ix) + (iy * iy)));
                        } else {
                            destImage.set(i + index, buffer[i]);
                        }
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

}
