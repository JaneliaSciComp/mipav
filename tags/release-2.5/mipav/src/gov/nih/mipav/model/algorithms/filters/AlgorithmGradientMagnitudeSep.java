package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;


/**
 * Calculates the gradient magnitude of an image at a scale defined by the user (using separable convolutions). Adapted
 * from AlgorithmGradientMagnitude. This version should be faster but uses significantly more memory.
 *
 * <p>Produces equivalent result images to AlgorithmGradientMagnitude (aside from some rounding error that seems to be
 * at most 10^-4)</p>
 *
 * @version  0.1 July 31, 2003
 * @author   Evan McCreedy
 * @see      AlgorithmGradientMagnitude
 */
public class AlgorithmGradientMagnitudeSep extends AlgorithmBase {

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

    /** Derivative and rounding 1D kernels. */
    private float[] GxDataDerivative;

    /** DOCUMENT ME! */
    private float[] GxDataRound;

    /** Storage location of the first derivative of the Gaussian in the Y direction for 2D images; */
    private float[] Gy2Data;

    /** Storage location of the first derivative of the Gaussian in the Y direction. */
    private float[] GyData;

    /** DOCUMENT ME! */
    private float[] GyDataDerivative;

    /** DOCUMENT ME! */
    private float[] GyDataRound;

    /** Storage location of the first derivative of the Gaussian in the Z direction. */
    private float[] GzData;

    /** DOCUMENT ME! */
    private float[] GzDataDerivative;

    /** DOCUMENT ME! */
    private float[] GzDataRound;

    /** Extents of the kernel. */
    private int[] kExtents;

    /** Flags indicate which color channel to process. True indicates the channel should be processed. */
    private boolean red = true;

    /** Standard deviations of the gaussian used to calculate the kernels. */
    private float[] sigmas;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmGradientMagnitudeSep object.
     *
     * @param  srcImg    source image model
     * @param  sigmas    Gaussian standard deviations in each dimension
     * @param  maskFlag  Flag, if true, indicates that the gradient magnitude will be calculated for the whole image
     * @param  img25D    Flag, if true, indicates that each slice of the 3D volume should be processed independently. 2D
     *                   images disregard this flag.
     */
    public AlgorithmGradientMagnitudeSep(ModelImage srcImg, float[] sigmas, boolean maskFlag, boolean img25D) {
        super(null, srcImg);

        this.sigmas = sigmas;
        image25D = img25D;
        entireImage = maskFlag;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }
    }

    /**
     * Creates a new AlgorithmGradientMagnitudeSep object.
     *
     * @param  destImg   image model where result image is to stored
     * @param  srcImg    source image model
     * @param  sigmas    Gaussian standard deviations in each dimension
     * @param  maskFlag  Flag, if true, indicates that the gradient magnitude will be calculated for the whole image
     * @param  img25D    Flag, if true, indicates that each slice of the 3D volume should be processed independently. 2D
     *                   images disregard this flag.
     */
    public AlgorithmGradientMagnitudeSep(ModelImage destImg, ModelImage srcImg, float[] sigmas, boolean maskFlag,
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
        makeKernels1D(false);

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
        makeKernels1D(false);

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
        GxDataDerivative = null;
        GxDataRound = null;
        GyDataDerivative = null;
        GyDataRound = null;
        GzDataDerivative = null;
        GzDataRound = null;
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

        if (srcImage.getNDims() == 4) {
            makeKernels1D(true);
        } else if ((srcImage.getNDims() == 3) && (image25D == false)) {
            makeKernels1D(true);
        } else {
            makeKernels1D(false);
        }

        if (threadStopped) {
            finalize();

            return;
        }

        constructLog();

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

        disposeProgressBar();
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
        int length;
        int start;
        float[] resultBuffer, xBuffer, yBuffer;
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

            resultBuffer = new float[length * nImages];
            xBuffer = new float[length];
            yBuffer = new float[length];

            if (srcImage != null) {
                buildProgressBar(srcImage.getImageName(), "Calculating the gradient magnitude ...", 0, 100);
            }
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Gradient Magnitude: Out of memory", true);

            return null;
        }

        float inc = 100.0f / nImages;
        initProgressBar();

        for (s = 0; s < nImages; s++) {

            if (isProgressBarVisible()) {
                progressBar.setMessage("Calculating gradient magnitude of slice " + (s + 1) + "...");
                progressBar.updateValue(Math.round(s * inc), activeImage);
            }

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

                AlgorithmSeparableConvolver xConvolver = new AlgorithmSeparableConvolver(xBuffer, buffer,
                                                                                         new int[] {
                                                                                             extents[0], extents[1]
                                                                                         }, GxDataDerivative,
                                                                                         GyDataRound, color);
                xConvolver.setActiveImage(activeImage);

                if (!entireImage) {
                    xConvolver.setMask(mask);
                }

                xConvolver.setColorChannels(red, green, blue);
                xConvolver.run();
                xConvolver.finalize();
                xConvolver = null;

                AlgorithmSeparableConvolver yConvolver = new AlgorithmSeparableConvolver(yBuffer, buffer,
                                                                                         new int[] {
                                                                                             extents[0], extents[1]
                                                                                         }, GxDataRound,
                                                                                         GyDataDerivative, color);
                yConvolver.setActiveImage(activeImage);

                if (!entireImage) {
                    yConvolver.setMask(mask);
                }

                yConvolver.setColorChannels(red, green, blue);
                yConvolver.run();
                yConvolver.finalize();
                yConvolver = null;

                for (i = 0; (i < length) && !threadStopped; i += 4) {

                    if (entireImage || mask.get(i / 4)) {
                        resultBuffer[start + i] = buffer[i];

                        if (red) {
                            resultBuffer[start + i + 1] = (float) Math.sqrt((xBuffer[i + 1] * xBuffer[i + 1]) +
                                                                            (yBuffer[i + 1] * yBuffer[i + 1]));

                            if (xDirections != null) {
                                xDirections[start + i + 1] = xBuffer[i + 1] / resultBuffer[start + i + 1];
                                yDirections[start + i + 1] = yBuffer[i + 1] / resultBuffer[start + i + 1];
                            }
                        } else {
                            resultBuffer[start + i + 1] = buffer[i + 1];
                        }

                        if (green) {
                            resultBuffer[start + i + 2] = (float) Math.sqrt((xBuffer[i + 2] * xBuffer[i + 2]) +
                                                                            (yBuffer[i + 2] * yBuffer[i + 2]));

                            if (xDirections != null) {
                                xDirections[start + i + 2] = xBuffer[i + 2] / resultBuffer[start + i + 2];
                                yDirections[start + i + 2] = yBuffer[i + 2] / resultBuffer[start + i + 2];
                            }
                        } else {
                            resultBuffer[start + i + 2] = buffer[i + 2];
                        }

                        if (blue) {
                            resultBuffer[start + i + 3] = (float) Math.sqrt((xBuffer[i + 3] * xBuffer[i + 3]) +
                                                                            (yBuffer[i + 3] * yBuffer[i + 3]));

                            if (xDirections != null) {
                                xDirections[start + i + 3] = xBuffer[i + 3] / resultBuffer[start + i + 3];
                                yDirections[start + i + 3] = yBuffer[i + 3] / resultBuffer[start + i + 3];
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
                AlgorithmSeparableConvolver xConvolver = new AlgorithmSeparableConvolver(xBuffer, buffer,
                                                                                         new int[] {
                                                                                             extents[0], extents[1]
                                                                                         }, GxDataDerivative,
                                                                                         GyDataRound, color);
                xConvolver.setActiveImage(activeImage);

                if (!entireImage) {
                    xConvolver.setMask(mask);
                }

                xConvolver.run();
                xConvolver.finalize();
                xConvolver = null;

                AlgorithmSeparableConvolver yConvolver = new AlgorithmSeparableConvolver(yBuffer, buffer,
                                                                                         new int[] {
                                                                                             extents[0], extents[1]
                                                                                         }, GxDataRound,
                                                                                         GyDataDerivative, color);
                yConvolver.setActiveImage(activeImage);

                if (!entireImage) {
                    yConvolver.setMask(mask);
                }

                yConvolver.run();
                yConvolver.finalize();
                yConvolver = null;

                for (i = 0; (i < length) && !threadStopped; i++) {

                    if (entireImage || mask.get(i)) {
                        resultBuffer[start + i] = (float) Math.sqrt((xBuffer[i] * xBuffer[i]) +
                                                                    (yBuffer[i] * yBuffer[i]));

                        if (xDirections != null) {

                            if (resultBuffer[start + i] == 0) {
                                xDirections[start + i] = xBuffer[i];
                                yDirections[start + i] = yBuffer[i];
                            } else {
                                xDirections[start + i] = xBuffer[i] / resultBuffer[start + i];
                                yDirections[start + i] = yBuffer[i] / resultBuffer[start + i];
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
        int length;
        int start;
        float[] resultBuffer, xBuffer, yBuffer;
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

            resultBuffer = new float[length * nImages];
            xBuffer = new float[length];
            yBuffer = new float[length];

            if (srcImage != null) {
                buildProgressBar(srcImage.getImageName(), "Calculating the gradient magnitude ...", 0, 100);
            }
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Gradient Magnitude: Out of memory", true);

            return null;
        }

        float inc = 100.0f / nImages;
        initProgressBar();

        for (s = 0; s < nImages; s++) {

            if (isProgressBarVisible()) {
                progressBar.setMessage("Calculating gradient magnitude of slice " + (s + 1) + "...");
                progressBar.updateValue(Math.round(s * inc), activeImage);
            }

            start = s * length;

            try {

                if (srcImage != null) {
                    srcImage.exportData(start, length, buffer); // locks and releases lock
                }
            } catch (IOException error) {
                errorCleanUp("Algorithm Gradient Magnitude: " + error, false);

                return null;
            }

            if (color) {
                AlgorithmSeparableConvolver xConvolver = new AlgorithmSeparableConvolver(xBuffer, buffer,
                                                                                         new int[] {
                                                                                             extents[0], extents[1]
                                                                                         }, GxDataDerivative,
                                                                                         GyDataRound, color);
                xConvolver.setActiveImage(activeImage);

                if (!entireImage) {
                    xConvolver.setMask(mask);
                }

                xConvolver.setColorChannels(red, green, blue);
                xConvolver.run();
                xConvolver.finalize();
                xConvolver = null;

                AlgorithmSeparableConvolver yConvolver = new AlgorithmSeparableConvolver(yBuffer, buffer,
                                                                                         new int[] {
                                                                                             extents[0], extents[1]
                                                                                         }, GxDataRound,
                                                                                         GyDataDerivative, color);
                yConvolver.setActiveImage(activeImage);

                if (!entireImage) {
                    yConvolver.setMask(mask);
                }

                yConvolver.setColorChannels(red, green, blue);
                yConvolver.run();
                yConvolver.finalize();
                yConvolver = null;

                for (i = 0; (i < length) && !threadStopped; i += 4) {

                    if (entireImage || mask.get(i / 4)) {
                        resultBuffer[start + i] = buffer[i];

                        if (red) {
                            resultBuffer[start + i + 1] = (float) Math.sqrt((xBuffer[i + 1] * xBuffer[i + 1]) +
                                                                            (yBuffer[i + 1] * yBuffer[i + 1]));

                            if (xDirections != null) {
                                xDirections[start + i + 1] = xBuffer[i + 1];
                                yDirections[start + i + 1] = yBuffer[i + 1];
                            }
                        } else {
                            resultBuffer[start + i + 1] = buffer[i + 1];
                        }

                        if (green) {
                            resultBuffer[start + i + 2] = (float) Math.sqrt((xBuffer[i + 2] * xBuffer[i + 2]) +
                                                                            (yBuffer[i + 2] * yBuffer[i + 2]));

                            if (xDirections != null) {
                                xDirections[start + i + 2] = xBuffer[i + 2];
                                yDirections[start + i + 2] = yBuffer[i + 2];
                            }
                        } else {
                            resultBuffer[start + i + 2] = buffer[i + 2];
                        }

                        if (blue) {
                            resultBuffer[start + i + 3] = (float) Math.sqrt((xBuffer[i + 3] * xBuffer[i + 3]) +
                                                                            (yBuffer[i + 3] * yBuffer[i + 3]));

                            if (xDirections != null) {
                                xDirections[start + i + 3] = xBuffer[i + 3];
                                yDirections[start + i + 3] = yBuffer[i + 3];
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
                AlgorithmSeparableConvolver xConvolver = new AlgorithmSeparableConvolver(xBuffer, buffer,
                                                                                         new int[] {
                                                                                             extents[0], extents[1]
                                                                                         }, GxDataDerivative,
                                                                                         GyDataRound, color);
                xConvolver.setActiveImage(activeImage);

                if (!entireImage) {
                    xConvolver.setMask(mask);
                }

                xConvolver.run();
                xConvolver.finalize();
                xConvolver = null;

                AlgorithmSeparableConvolver yConvolver = new AlgorithmSeparableConvolver(yBuffer, buffer,
                                                                                         new int[] {
                                                                                             extents[0], extents[1]
                                                                                         }, GxDataRound,
                                                                                         GyDataDerivative, color);
                yConvolver.setActiveImage(activeImage);

                if (!entireImage) {
                    yConvolver.setMask(mask);
                }

                yConvolver.run();
                yConvolver.finalize();
                yConvolver = null;

                for (i = 0; (i < length) && !threadStopped; i++) {

                    if (entireImage || mask.get(i)) {
                        resultBuffer[start + i] = (float) Math.sqrt((xBuffer[i] * xBuffer[i]) +
                                                                    (yBuffer[i] * yBuffer[i]));

                        if (xDirections != null) {
                            xDirections[start + i] = xBuffer[i];
                            yDirections[start + i] = yBuffer[i];
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
        float[] buffer;
        float[] resultBuffer, xBuffer, yBuffer, zBuffer, x2Buffer, y2Buffer, sliceBuffer;
        int offsetZ;
        boolean color = false;
        int cFactor = 1;
        int curPercent = 0, maxPercent = 100;
        int curSlice, cur2DBufferSlice = -1;

        if (srcImage.isColorImage() == true) {
            color = true;
            cFactor = 4;
        }

        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int sliceSize = cFactor * xDim * yDim;

        try {
            length = cFactor * srcImage.getSliceSize() * srcImage.getExtents()[2];
            buffer = new float[length];
            resultBuffer = new float[length];
            xBuffer = new float[length];
            yBuffer = new float[length];
            zBuffer = new float[length];
            x2Buffer = new float[cFactor * srcImage.getSliceSize()];
            y2Buffer = new float[cFactor * srcImage.getSliceSize()];
            sliceBuffer = new float[cFactor * srcImage.getSliceSize()];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            buildProgressBar(srcImage.getImageName(), "Calculating the gradient magnitude ...", 0, 100);
        } catch (IOException error) {
            errorCleanUp("Algorithm Gradient Magnitude exportData: Image(s) locked", false);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Gradient Magnitude: Out of memory", true);

            return;
        }

        initProgressBar();

        int mod; // mod is 1 percent of length

        if (color) {
            curPercent = 5;
            maxPercent = 20;

            AlgorithmSeparableConvolver xConvolver = new AlgorithmSeparableConvolver(xBuffer, buffer,
                                                                                     srcImage.getExtents(),
                                                                                     GxDataDerivative, GyDataRound,
                                                                                     GzDataRound, color);
            xConvolver.setActiveImage(activeImage);

            if (!entireImage) {
                xConvolver.setMask(mask);
            }

            xConvolver.setColorChannels(red, green, blue);

            if (isProgressBarVisible()) {
                xConvolver.setProgressBar(progressBar, curPercent, maxPercent, true);
            }

            xConvolver.run();
            xConvolver.finalize();
            xConvolver = null;

            curPercent = maxPercent;
            maxPercent += 20;

            AlgorithmSeparableConvolver yConvolver = new AlgorithmSeparableConvolver(yBuffer, buffer,
                                                                                     srcImage.getExtents(), GxDataRound,
                                                                                     GyDataDerivative, GzDataRound,
                                                                                     color);
            yConvolver.setActiveImage(activeImage);

            if (!entireImage) {
                yConvolver.setMask(mask);
            }

            yConvolver.setColorChannels(red, green, blue);

            if (isProgressBarVisible()) {
                yConvolver.setProgressBar(progressBar, curPercent, maxPercent, true);
            }

            yConvolver.run();
            yConvolver.finalize();
            yConvolver = null;

            curPercent = maxPercent;
            maxPercent += 20;

            AlgorithmSeparableConvolver zConvolver = new AlgorithmSeparableConvolver(zBuffer, buffer,
                                                                                     srcImage.getExtents(), GxDataRound,
                                                                                     GyDataRound, GzDataDerivative,
                                                                                     color);
            zConvolver.setActiveImage(activeImage);

            if (!entireImage) {
                zConvolver.setMask(mask);
            }

            zConvolver.setColorChannels(red, green, blue);

            if (isProgressBarVisible()) {
                zConvolver.setProgressBar(progressBar, curPercent, maxPercent, true);
            }

            zConvolver.run();
            zConvolver.finalize();
            zConvolver = null;

            mod = length / (100 - maxPercent);

            for (i = 0; (i < length) && !threadStopped; i += 4) {

                if (((i % mod) == 0) && isProgressBarVisible()) {
                    maxPercent++;
                    progressBar.updateValue(maxPercent, activeImage);
                }

                curSlice = i / sliceSize;
                offsetZ = curSlice - (GzDataDerivative.length / 2);

                if ((offsetZ >= 0) && ((offsetZ + GzDataDerivative.length - 1) < zDim)) {

                    if (entireImage || mask.get(i / 4)) {
                        resultBuffer[i] = buffer[i];

                        if (red) {
                            resultBuffer[i + 1] = (float) Math.sqrt((xBuffer[i + 1] * xBuffer[i + 1]) +
                                                                    (yBuffer[i + 1] * yBuffer[i + 1]) +
                                                                    (zBuffer[i + 1] * zBuffer[i + 1]));
                        } else {
                            resultBuffer[i + 1] = buffer[i + 1];
                        }

                        if (green) {
                            resultBuffer[i + 2] = (float) Math.sqrt((xBuffer[i + 2] * xBuffer[i + 2]) +
                                                                    (yBuffer[i + 2] * yBuffer[i + 2]) +
                                                                    (zBuffer[i + 2] * zBuffer[i + 2]));
                        } else {
                            resultBuffer[i + 2] = buffer[i + 2];
                        }

                        if (blue) {
                            resultBuffer[i + 3] = (float) Math.sqrt((xBuffer[i + 3] * xBuffer[i + 3]) +
                                                                    (yBuffer[i + 3] * yBuffer[i + 3]) +
                                                                    (zBuffer[i + 3] * zBuffer[i + 3]));
                        } else {
                            resultBuffer[i + 3] = buffer[i + 3];
                        }
                    }
                } else {

                    if (entireImage || mask.get(i / 4)) {

                        if (cur2DBufferSlice != curSlice) {

                            // we haven't calculated the 2D buffers for this slice, so we make them now
                            try {
                                srcImage.exportData(curSlice * sliceSize, sliceSize, sliceBuffer);
                            } catch (IOException ioe) {
                                sliceBuffer = null;
                                errorCleanUp("Algorithm Gradient Magnitude exportData: Image(s) locked", true);

                                return;
                            }

                            AlgorithmSeparableConvolver x2Convolver = new AlgorithmSeparableConvolver(x2Buffer,
                                                                                                      sliceBuffer,
                                                                                                      new int[] {
                                                                                                          srcImage.getExtents()[0],
                                                                                                          srcImage.getExtents()[1]
                                                                                                      },
                                                                                                      GxDataDerivative,
                                                                                                      GyDataRound,
                                                                                                      color);
                            x2Convolver.setActiveImage(activeImage);

                            if (!entireImage) {
                                x2Convolver.setMask(mask);
                            }

                            x2Convolver.run();
                            x2Convolver.finalize();
                            x2Convolver = null;

                            AlgorithmSeparableConvolver y2Convolver = new AlgorithmSeparableConvolver(y2Buffer,
                                                                                                      sliceBuffer,
                                                                                                      new int[] {
                                                                                                          srcImage.getExtents()[0],
                                                                                                          srcImage.getExtents()[1]
                                                                                                      }, GxDataRound,
                                                                                                      GyDataDerivative,
                                                                                                      color);
                            y2Convolver.setActiveImage(activeImage);

                            if (!entireImage) {
                                y2Convolver.setMask(mask);
                            }

                            y2Convolver.run();
                            y2Convolver.finalize();
                            y2Convolver = null;

                            cur2DBufferSlice = curSlice;
                        }

                        resultBuffer[i] = buffer[i];
                        j = i % sliceSize; // index into 2D x2 and y2Buffer

                        if (red) {
                            resultBuffer[i + 1] = (float) Math.sqrt((x2Buffer[j + 1] * x2Buffer[j + 1]) +
                                                                    (y2Buffer[j + 1] * y2Buffer[j + 1]));
                        }

                        if (red) {
                            resultBuffer[i + 2] = (float) Math.sqrt((x2Buffer[j + 2] * x2Buffer[j + 2]) +
                                                                    (y2Buffer[j + 2] * y2Buffer[j + 2]));
                        }

                        if (blue) {
                            resultBuffer[i + 3] = (float) Math.sqrt((x2Buffer[j + 3] * x2Buffer[j + 3]) +
                                                                    (y2Buffer[j + 3] * y2Buffer[j + 3]));
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
            curPercent = 5;
            maxPercent = 20;

            AlgorithmSeparableConvolver xConvolver = new AlgorithmSeparableConvolver(xBuffer, buffer,
                                                                                     srcImage.getExtents(),
                                                                                     GxDataDerivative, GyDataRound,
                                                                                     GzDataRound, color);
            xConvolver.setActiveImage(activeImage);

            if (!entireImage) {
                xConvolver.setMask(mask);
            }

            if (isProgressBarVisible()) {
                xConvolver.setProgressBar(progressBar, curPercent, maxPercent, true);
            }

            xConvolver.run();
            xConvolver.finalize();
            xConvolver = null;

            curPercent = maxPercent;
            maxPercent += 20;

            AlgorithmSeparableConvolver yConvolver = new AlgorithmSeparableConvolver(yBuffer, buffer,
                                                                                     srcImage.getExtents(), GxDataRound,
                                                                                     GyDataDerivative, GzDataRound,
                                                                                     color);
            yConvolver.setActiveImage(activeImage);

            if (!entireImage) {
                yConvolver.setMask(mask);
            }

            if (isProgressBarVisible()) {
                yConvolver.setProgressBar(progressBar, curPercent, maxPercent, true);
            }

            yConvolver.run();
            yConvolver.finalize();
            yConvolver = null;

            curPercent = maxPercent;
            maxPercent += 20;

            AlgorithmSeparableConvolver zConvolver = new AlgorithmSeparableConvolver(zBuffer, buffer,
                                                                                     srcImage.getExtents(), GxDataRound,
                                                                                     GyDataRound, GzDataDerivative,
                                                                                     color);
            zConvolver.setActiveImage(activeImage);

            if (!entireImage) {
                zConvolver.setMask(mask);
            }

            if (isProgressBarVisible()) {
                zConvolver.setProgressBar(progressBar, curPercent, maxPercent, true);
            }

            zConvolver.run();
            zConvolver.finalize();
            zConvolver = null;

            mod = length / (100 - maxPercent);

            for (i = 0; (i < length) && !threadStopped; i++) {

                if (((i % mod) == 0) && isProgressBarVisible()) {
                    maxPercent++;
                    progressBar.updateValue(maxPercent, activeImage);
                }

                curSlice = i / sliceSize;
                offsetZ = curSlice - (GzDataDerivative.length / 2);

                if ((offsetZ >= 0) && ((offsetZ + GzDataDerivative.length - 1) < zDim)) {

                    if ((entireImage == true) || mask.get(i)) {
                        resultBuffer[i] = (float) Math.sqrt((xBuffer[i] * xBuffer[i]) + (yBuffer[i] * yBuffer[i]) +
                                                            (zBuffer[i] * zBuffer[i]));
                    } else {
                        resultBuffer[i] = buffer[i];
                    }
                } else {

                    if (entireImage || mask.get(i)) {

                        if (cur2DBufferSlice != curSlice) {

                            // we haven't calculated the 2D buffers for this slice, so we make them now
                            try {
                                srcImage.exportData(curSlice * sliceSize, sliceSize, sliceBuffer);
                            } catch (IOException ioe) {
                                sliceBuffer = null;
                                errorCleanUp("Algorithm Gradient Magnitude exportData: Image(s) locked", true);

                                return;
                            }

                            AlgorithmSeparableConvolver x2Convolver = new AlgorithmSeparableConvolver(x2Buffer,
                                                                                                      sliceBuffer,
                                                                                                      new int[] {
                                                                                                          srcImage.getExtents()[0],
                                                                                                          srcImage.getExtents()[1]
                                                                                                      },
                                                                                                      GxDataDerivative,
                                                                                                      GyDataRound,
                                                                                                      color);
                            x2Convolver.setActiveImage(activeImage);

                            if (!entireImage) {
                                x2Convolver.setMask(mask);
                            }

                            x2Convolver.run();
                            x2Convolver.finalize();
                            x2Convolver = null;

                            AlgorithmSeparableConvolver y2Convolver = new AlgorithmSeparableConvolver(y2Buffer,
                                                                                                      sliceBuffer,
                                                                                                      new int[] {
                                                                                                          srcImage.getExtents()[0],
                                                                                                          srcImage.getExtents()[1]
                                                                                                      }, GxDataRound,
                                                                                                      GyDataDerivative,
                                                                                                      color);
                            y2Convolver.setActiveImage(activeImage);

                            if (!entireImage) {
                                y2Convolver.setMask(mask);
                            }

                            y2Convolver.run();
                            y2Convolver.finalize();
                            y2Convolver = null;

                            cur2DBufferSlice = curSlice;
                        }

                        j = i % sliceSize; // index into 2D x2 and y2Buffer
                        resultBuffer[i] = (float) Math.sqrt((x2Buffer[j] * x2Buffer[j]) + (y2Buffer[j] * y2Buffer[j]));
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

        disposeProgressBar();
        setCompleted(true);
    }

    /// not changing the 4D stuff yet.. 4D convolution is commented out anyway (in this algo and AlgorithmGraientMag)
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
            buildProgressBar(srcImage.getImageName(), "Calculating the gradient magnitude ...", 0, 100);
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Gradient Magnitude exportData: Out of memory", true);

            return;
        }

        initProgressBar();

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
                disposeProgressBar();

                return;
            }

            if (isProgressBarVisible()) {
                progressBar.updateValue(Math.round((float) t / end * 100), activeImage);
            }

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

                    if (((i % mod) == 0) && isProgressBarVisible()) {
                        progressBar.updateValue(Math.round((float) i / (length - 1) * 100), activeImage);
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
                disposeProgressBar();

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
        disposeProgressBar();
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
        int length;
        int start;
        float[] buffer, xBuffer, yBuffer;
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
            buffer = new float[length];
            xBuffer = new float[length];
            yBuffer = new float[length];
            buildProgressBar(srcImage.getImageName(), "Calculating the gradient magnitude ...", 0, 100);
        } catch (OutOfMemoryError e) {
            buffer = null;
            System.gc();
            displayError("Algorithm Gradient Magnitude: Out of memory");
            setCompleted(false);
            disposeProgressBar();
            destImage.releaseLock();

            return;
        }

        float inc = 100.0f / nImages;
        initProgressBar();

        for (s = 0; s < nImages; s++) {

            if (isProgressBarVisible()) {
                progressBar.setMessage("Calculating gradient magnitude of slice " + (s + 1) + "...");
                progressBar.updateValue(Math.round(s * inc), activeImage);
            }

            start = s * length;

            try {
                srcImage.exportData(start, length, buffer); // locks and releases lock
            } catch (IOException error) {
                errorCleanUp("Algorithm Gradient Magnitude: " + error, false);

                return;
            }

            if (color) {
                AlgorithmSeparableConvolver xConvolver = new AlgorithmSeparableConvolver(xBuffer, buffer,
                                                                                         new int[] {
                                                                                             srcImage.getExtents()[0],
                                                                                             srcImage.getExtents()[1]
                                                                                         }, GxDataDerivative,
                                                                                         GyDataRound, color);
                xConvolver.setActiveImage(activeImage);

                if (!entireImage) {
                    xConvolver.setMask(mask);
                }

                xConvolver.setColorChannels(red, green, blue);
                xConvolver.run();
                xConvolver.finalize();
                xConvolver = null;

                AlgorithmSeparableConvolver yConvolver = new AlgorithmSeparableConvolver(yBuffer, buffer,
                                                                                         new int[] {
                                                                                             srcImage.getExtents()[0],
                                                                                             srcImage.getExtents()[1]
                                                                                         }, GxDataRound,
                                                                                         GyDataDerivative, color);
                yConvolver.setActiveImage(activeImage);

                if (!entireImage) {
                    yConvolver.setMask(mask);
                }

                yConvolver.setColorChannels(red, green, blue);
                yConvolver.run();
                yConvolver.finalize();
                yConvolver = null;

                for (i = 0, idx = start; (i < length) && !threadStopped; i += 4, idx += 4) {

                    if (entireImage || mask.get(i / 4)) {
                        destImage.set(idx, buffer[i]); // alpha

                        if (red) {
                            destImage.set(idx + 1,
                                          (float) Math.sqrt((xBuffer[i + 1] * xBuffer[i + 1]) +
                                                            (yBuffer[i + 1] * yBuffer[i + 1])));
                        } else {
                            destImage.set(idx + 1, buffer[i + 1]);
                        }

                        if (green) {
                            destImage.set(idx + 2,
                                          (float) Math.sqrt((xBuffer[i + 2] * xBuffer[i + 2]) +
                                                            (yBuffer[i + 2] * yBuffer[i + 2])));
                        } else {
                            destImage.set(idx + 2, buffer[i + 2]);
                        }

                        if (blue) {
                            destImage.set(idx + 3,
                                          (float) Math.sqrt((xBuffer[i + 3] * xBuffer[i + 3]) +
                                                            (yBuffer[i + 3] * yBuffer[i + 3])));
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
                AlgorithmSeparableConvolver xConvolver = new AlgorithmSeparableConvolver(xBuffer, buffer,
                                                                                         new int[] {
                                                                                             srcImage.getExtents()[0],
                                                                                             srcImage.getExtents()[1]
                                                                                         }, GxDataDerivative,
                                                                                         GyDataRound, color);
                xConvolver.setActiveImage(activeImage);

                if (!entireImage) {
                    xConvolver.setMask(mask);
                }

                xConvolver.run();
                xConvolver.finalize();
                xConvolver = null;

                AlgorithmSeparableConvolver yConvolver = new AlgorithmSeparableConvolver(yBuffer, buffer,
                                                                                         new int[] {
                                                                                             srcImage.getExtents()[0],
                                                                                             srcImage.getExtents()[1]
                                                                                         }, GxDataRound,
                                                                                         GyDataDerivative, color);
                yConvolver.setActiveImage(activeImage);

                if (!entireImage) {
                    yConvolver.setMask(mask);
                }

                yConvolver.run();
                yConvolver.finalize();
                yConvolver = null;

                for (i = 0, idx = start; (i < length) && !threadStopped; i++, idx++) {

                    if (entireImage || mask.get(i)) {
                        destImage.set(idx, (float) Math.sqrt((xBuffer[i] * xBuffer[i]) + (yBuffer[i] * yBuffer[i])));
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
        disposeProgressBar();
        setCompleted(true);
    }

    /**
     * This function produces the gradient magnitude of input image.
     */
    private void calcStoreInDest3D() {

        int i, j;
        int length;
        float[] buffer, xBuffer, yBuffer, zBuffer, x2Buffer, y2Buffer, sliceBuffer;
        int offsetZ;
        boolean color = false;
        int cFactor = 1;
        int curPercent = 0, maxPercent = 100;
        int curSlice, cur2DBufferSlice = -1;

        if (srcImage.isColorImage()) {
            color = true;
            cFactor = 4;
        }

        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int sliceSize = cFactor * xDim * yDim;

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp(" GradientMagnitude: Image(s) locked", false);

            return;
        }

        try {
            length = cFactor * srcImage.getSliceSize() * srcImage.getExtents()[2];
            buffer = new float[length];
            xBuffer = new float[length];
            yBuffer = new float[length];
            zBuffer = new float[length];
            x2Buffer = new float[cFactor * srcImage.getSliceSize()];
            y2Buffer = new float[cFactor * srcImage.getSliceSize()];
            sliceBuffer = new float[cFactor * srcImage.getSliceSize()];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            buildProgressBar(srcImage.getImageName(), "Calculating the gradient magnitude ...", 0, 100);
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

        initProgressBar();

        int mod; // mod is 1 percent of length

        if (color) {
            curPercent = 5;
            maxPercent = 20;

            AlgorithmSeparableConvolver xConvolver = new AlgorithmSeparableConvolver(xBuffer, buffer,
                                                                                     srcImage.getExtents(),
                                                                                     GxDataDerivative, GyDataRound,
                                                                                     GzDataRound, color);
            xConvolver.setActiveImage(activeImage);

            if (!entireImage) {
                xConvolver.setMask(mask);
            }

            xConvolver.setColorChannels(red, green, blue);

            if (isProgressBarVisible()) {
                xConvolver.setProgressBar(progressBar, curPercent, maxPercent, true);
            }

            xConvolver.run();
            xConvolver.finalize();
            xConvolver = null;

            curPercent = maxPercent;
            maxPercent += 20;

            AlgorithmSeparableConvolver yConvolver = new AlgorithmSeparableConvolver(yBuffer, buffer,
                                                                                     srcImage.getExtents(), GxDataRound,
                                                                                     GyDataDerivative, GzDataRound,
                                                                                     color);
            yConvolver.setActiveImage(activeImage);

            if (!entireImage) {
                yConvolver.setMask(mask);
            }

            yConvolver.setColorChannels(red, green, blue);

            if (isProgressBarVisible()) {
                yConvolver.setProgressBar(progressBar, curPercent, maxPercent, true);
            }

            yConvolver.run();
            yConvolver.finalize();
            yConvolver = null;

            curPercent = maxPercent;
            maxPercent += 20;

            AlgorithmSeparableConvolver zConvolver = new AlgorithmSeparableConvolver(zBuffer, buffer,
                                                                                     srcImage.getExtents(), GxDataRound,
                                                                                     GyDataRound, GzDataDerivative,
                                                                                     color);
            zConvolver.setActiveImage(activeImage);

            if (!entireImage) {
                zConvolver.setMask(mask);
            }

            zConvolver.setColorChannels(red, green, blue);

            if (isProgressBarVisible()) {
                zConvolver.setProgressBar(progressBar, curPercent, maxPercent, true);
            }

            zConvolver.run();
            zConvolver.finalize();
            zConvolver = null;

            mod = length / (100 - maxPercent);

            for (i = 0; (i < length) && !threadStopped; i += 4) {

                if (((i % mod) == 0) && isProgressBarVisible()) {
                    maxPercent++;
                    progressBar.updateValue(maxPercent, activeImage);
                }

                curSlice = i / sliceSize;

                offsetZ = (i / sliceSize) - (GzDataDerivative.length / 2);

                if ((offsetZ >= 0) && ((offsetZ + GzDataDerivative.length - 1) < zDim)) {

                    if (entireImage || mask.get(i / 4)) {
                        destImage.set(i, buffer[i]); // alpha

                        if (red) {
                            destImage.set(i + 1,
                                          (float) Math.sqrt((xBuffer[i + 1] * xBuffer[i + 1]) +
                                                            (yBuffer[i + 1] * yBuffer[i + 1]) +
                                                            (zBuffer[i + 1] * zBuffer[i + 1])));
                        } else {
                            destImage.set(i + 1, buffer[i + 1]);
                        }

                        if (green) {
                            destImage.set(i + 2,
                                          (float) Math.sqrt((xBuffer[i + 2] * xBuffer[i + 2]) +
                                                            (yBuffer[i + 2] * yBuffer[i + 2]) +
                                                            (zBuffer[i + 2] * zBuffer[i + 2])));
                        } else {
                            destImage.set(i + 2, buffer[i + 2]);
                        }

                        if (blue) {
                            destImage.set(i + 3,
                                          (float) Math.sqrt((xBuffer[i + 3] * xBuffer[i + 3]) +
                                                            (yBuffer[i + 3] * yBuffer[i + 3]) +
                                                            (zBuffer[i + 3] * zBuffer[i + 3])));
                        } else {
                            destImage.set(i + 3, buffer[i + 3]);
                        }
                    }
                } else {

                    if (entireImage || mask.get(i / 4)) {

                        if (cur2DBufferSlice != curSlice) {

                            // we haven't calculated the 2D buffers for this slice, so we make them now
                            try {
                                srcImage.exportData(curSlice * sliceSize, sliceSize, sliceBuffer);
                            } catch (IOException ioe) {
                                sliceBuffer = null;
                                errorCleanUp("Algorithm Gradient Magnitude exportData: Image(s) locked", true);

                                return;
                            }

                            AlgorithmSeparableConvolver x2Convolver = new AlgorithmSeparableConvolver(x2Buffer,
                                                                                                      sliceBuffer,
                                                                                                      new int[] {
                                                                                                          srcImage.getExtents()[0],
                                                                                                          srcImage.getExtents()[1]
                                                                                                      },
                                                                                                      GxDataDerivative,
                                                                                                      GyDataRound,
                                                                                                      color);
                            x2Convolver.setActiveImage(activeImage);

                            if (!entireImage) {
                                x2Convolver.setMask(mask);
                            }

                            x2Convolver.run();
                            x2Convolver.finalize();
                            x2Convolver = null;

                            AlgorithmSeparableConvolver y2Convolver = new AlgorithmSeparableConvolver(y2Buffer,
                                                                                                      sliceBuffer,
                                                                                                      new int[] {
                                                                                                          srcImage.getExtents()[0],
                                                                                                          srcImage.getExtents()[1]
                                                                                                      }, GxDataRound,
                                                                                                      GyDataDerivative,
                                                                                                      color);
                            y2Convolver.setActiveImage(activeImage);

                            if (!entireImage) {
                                y2Convolver.setMask(mask);
                            }

                            y2Convolver.run();
                            y2Convolver.finalize();
                            y2Convolver = null;

                            cur2DBufferSlice = curSlice;
                        }

                        destImage.set(i, buffer[i]); // alpha
                        j = i % sliceSize; // index into 2D x2 and y2Buffer

                        if (red) {
                            destImage.set(i + 1,
                                          (float) Math.sqrt((x2Buffer[j + 1] * x2Buffer[j + 1]) +
                                                            (y2Buffer[j + 1] * y2Buffer[j + 1])));
                        }

                        if (red) {
                            destImage.set(i + 2,
                                          (float) Math.sqrt((x2Buffer[j + 2] * x2Buffer[j + 2]) +
                                                            (y2Buffer[j + 2] * y2Buffer[j + 2])));
                        }

                        if (blue) {
                            destImage.set(i + 3,
                                          (float) Math.sqrt((x2Buffer[j + 3] * x2Buffer[j + 3]) +
                                                            (y2Buffer[j + 3] * y2Buffer[j + 3])));
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
            curPercent = 5;
            maxPercent = 20;

            AlgorithmSeparableConvolver xConvolver = new AlgorithmSeparableConvolver(xBuffer, buffer,
                                                                                     srcImage.getExtents(),
                                                                                     GxDataDerivative, GyDataRound,
                                                                                     GzDataRound, color);
            xConvolver.setActiveImage(activeImage);

            if (!entireImage) {
                xConvolver.setMask(mask);
            }

            if (isProgressBarVisible()) {
                xConvolver.setProgressBar(progressBar, curPercent, maxPercent, true);
            }

            xConvolver.run();
            xConvolver.finalize();
            xConvolver = null;

            curPercent = maxPercent;
            maxPercent += 20;

            AlgorithmSeparableConvolver yConvolver = new AlgorithmSeparableConvolver(yBuffer, buffer,
                                                                                     srcImage.getExtents(), GxDataRound,
                                                                                     GyDataDerivative, GzDataRound,
                                                                                     color);
            yConvolver.setActiveImage(activeImage);

            if (!entireImage) {
                yConvolver.setMask(mask);
            }

            if (isProgressBarVisible()) {
                yConvolver.setProgressBar(progressBar, curPercent, maxPercent, true);
            }

            yConvolver.run();
            yConvolver.finalize();
            yConvolver = null;

            curPercent = maxPercent;
            maxPercent += 20;

            AlgorithmSeparableConvolver zConvolver = new AlgorithmSeparableConvolver(zBuffer, buffer,
                                                                                     srcImage.getExtents(), GxDataRound,
                                                                                     GyDataRound, GzDataDerivative,
                                                                                     color);
            zConvolver.setActiveImage(activeImage);

            if (!entireImage) {
                zConvolver.setMask(mask);
            }

            if (isProgressBarVisible()) {
                zConvolver.setProgressBar(progressBar, curPercent, maxPercent, true);
            }

            zConvolver.run();
            zConvolver.finalize();
            zConvolver = null;

            mod = length / (100 - maxPercent);

            for (i = 0; (i < length) && !threadStopped; i++) {

                if (((i % mod) == 0) && isProgressBarVisible()) {
                    maxPercent++;
                    progressBar.updateValue(maxPercent, activeImage);
                }

                curSlice = i / sliceSize;

                offsetZ = curSlice - (GzDataDerivative.length / 2);

                if ((offsetZ >= 0) && ((offsetZ + GzDataDerivative.length - 1) < zDim)) {

                    if (entireImage || mask.get(i)) {
                        destImage.set(i,
                                      (float) Math.sqrt((xBuffer[i] * xBuffer[i]) + (yBuffer[i] * yBuffer[i]) +
                                                        (zBuffer[i] * zBuffer[i])));
                    } else {
                        destImage.set(i, buffer[i]);
                    }
                } else {

                    if (entireImage || mask.get(i)) {

                        if (cur2DBufferSlice != curSlice) {

                            // we haven't calculated the 2D buffers for this slice, so we make them now
                            try {
                                srcImage.exportData(curSlice * sliceSize, sliceSize, sliceBuffer);
                            } catch (IOException ioe) {
                                sliceBuffer = null;
                                errorCleanUp("Algorithm Gradient Magnitude exportData: Image(s) locked", true);

                                return;
                            }

                            AlgorithmSeparableConvolver x2Convolver = new AlgorithmSeparableConvolver(x2Buffer,
                                                                                                      sliceBuffer,
                                                                                                      new int[] {
                                                                                                          srcImage.getExtents()[0],
                                                                                                          srcImage.getExtents()[1]
                                                                                                      },
                                                                                                      GxDataDerivative,
                                                                                                      GyDataRound,
                                                                                                      color);
                            x2Convolver.setActiveImage(activeImage);

                            if (!entireImage) {
                                x2Convolver.setMask(mask);
                            }

                            x2Convolver.run();
                            x2Convolver.finalize();
                            x2Convolver = null;

                            AlgorithmSeparableConvolver y2Convolver = new AlgorithmSeparableConvolver(y2Buffer,
                                                                                                      sliceBuffer,
                                                                                                      new int[] {
                                                                                                          srcImage.getExtents()[0],
                                                                                                          srcImage.getExtents()[1]
                                                                                                      }, GxDataRound,
                                                                                                      GyDataDerivative,
                                                                                                      color);
                            y2Convolver.setActiveImage(activeImage);

                            if (!entireImage) {
                                y2Convolver.setMask(mask);
                            }

                            y2Convolver.run();
                            y2Convolver.finalize();
                            y2Convolver = null;

                            cur2DBufferSlice = curSlice;
                        }

                        j = i % sliceSize; // index into 2D x2 and y2Buffer
                        destImage.set(i, (float) Math.sqrt((x2Buffer[j] * x2Buffer[j]) + (y2Buffer[j] * y2Buffer[j])));
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
        disposeProgressBar();
        setCompleted(true);
    }

    /// not changing the 4D stuff yet.. 4D convolution is commented out anyway (in this algo and AlgorithmGraientMag)
    /**
     * This function produces the gradient magnitude of input image.
     */
    private void calcStoreInDest4D() {

        int i, j;
        int length;
        float[] buffer, xBuffer, yBuffer, zBuffer, x2Buffer, y2Buffer, sliceBuffer;
        int offsetZ;
        boolean color = false;
        int cFactor = 1;
        int curPercent = 0, maxPercent = 100;
        int curSlice, cur2DBufferSlice = -1;

        if (srcImage.isColorImage()) {
            color = true;
            cFactor = 4;
        }

        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int sliceSize = cFactor * xDim * yDim;

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp(" GradientMagnitude: Image(s) locked", false);

            return;
        }

        try {
            length = cFactor * srcImage.getSliceSize() * srcImage.getExtents()[2];
            buffer = new float[length];
            xBuffer = new float[length];
            yBuffer = new float[length];
            zBuffer = new float[length];
            x2Buffer = new float[cFactor * srcImage.getSliceSize()];
            y2Buffer = new float[cFactor * srcImage.getSliceSize()];
            sliceBuffer = new float[cFactor * srcImage.getSliceSize()];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            buildProgressBar(srcImage.getImageName(), "Calculating the gradient magnitude ...", 0, 100);
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

        initProgressBar();

        int mod; // mod is 1 percent of length

        if (color) {
            curPercent = 5;
            maxPercent = 20;

            AlgorithmSeparableConvolver xConvolver = new AlgorithmSeparableConvolver(xBuffer, buffer,
                                                                                     srcImage.getExtents(),
                                                                                     GxDataDerivative, GyDataRound,
                                                                                     GzDataRound, color);
            xConvolver.setActiveImage(activeImage);

            if (!entireImage) {
                xConvolver.setMask(mask);
            }

            xConvolver.setColorChannels(red, green, blue);

            if (isProgressBarVisible()) {
                xConvolver.setProgressBar(progressBar, curPercent, maxPercent, true);
            }

            xConvolver.run();
            xConvolver.finalize();
            xConvolver = null;

            curPercent = maxPercent;
            maxPercent += 20;

            AlgorithmSeparableConvolver yConvolver = new AlgorithmSeparableConvolver(yBuffer, buffer,
                                                                                     srcImage.getExtents(), GxDataRound,
                                                                                     GyDataDerivative, GzDataRound,
                                                                                     color);
            yConvolver.setActiveImage(activeImage);

            if (!entireImage) {
                yConvolver.setMask(mask);
            }

            yConvolver.setColorChannels(red, green, blue);

            if (isProgressBarVisible()) {
                yConvolver.setProgressBar(progressBar, curPercent, maxPercent, true);
            }

            yConvolver.run();
            yConvolver.finalize();
            yConvolver = null;

            curPercent = maxPercent;
            maxPercent += 20;

            AlgorithmSeparableConvolver zConvolver = new AlgorithmSeparableConvolver(zBuffer, buffer,
                                                                                     srcImage.getExtents(), GxDataRound,
                                                                                     GyDataRound, GzDataDerivative,
                                                                                     color);
            zConvolver.setActiveImage(activeImage);

            if (!entireImage) {
                zConvolver.setMask(mask);
            }

            zConvolver.setColorChannels(red, green, blue);

            if (isProgressBarVisible()) {
                zConvolver.setProgressBar(progressBar, curPercent, maxPercent, true);
            }

            zConvolver.run();
            zConvolver.finalize();
            zConvolver = null;

            mod = length / (100 - maxPercent);

            for (i = 0; (i < length) && !threadStopped; i += 4) {

                if (((i % mod) == 0) && isProgressBarVisible()) {
                    maxPercent++;
                    progressBar.updateValue(maxPercent, activeImage);
                }

                curSlice = i / sliceSize;

                offsetZ = (i / sliceSize) - (GzDataDerivative.length / 2);

                if ((offsetZ >= 0) && ((offsetZ + GzDataDerivative.length - 1) < zDim)) {

                    if (entireImage || mask.get(i / 4)) {
                        destImage.set(i, buffer[i]); // alpha

                        if (red) {
                            destImage.set(i + 1,
                                          (float) Math.sqrt((xBuffer[i + 1] * xBuffer[i + 1]) +
                                                            (yBuffer[i + 1] * yBuffer[i + 1]) +
                                                            (zBuffer[i + 1] * zBuffer[i + 1])));
                        } else {
                            destImage.set(i + 1, buffer[i + 1]);
                        }

                        if (green) {
                            destImage.set(i + 2,
                                          (float) Math.sqrt((xBuffer[i + 2] * xBuffer[i + 2]) +
                                                            (yBuffer[i + 2] * yBuffer[i + 2]) +
                                                            (zBuffer[i + 2] * zBuffer[i + 2])));
                        } else {
                            destImage.set(i + 2, buffer[i + 2]);
                        }

                        if (blue) {
                            destImage.set(i + 3,
                                          (float) Math.sqrt((xBuffer[i + 3] * xBuffer[i + 3]) +
                                                            (yBuffer[i + 3] * yBuffer[i + 3]) +
                                                            (zBuffer[i + 3] * zBuffer[i + 3])));
                        } else {
                            destImage.set(i + 3, buffer[i + 3]);
                        }
                    }
                } else {

                    if (entireImage || mask.get(i / 4)) {

                        if (cur2DBufferSlice != curSlice) {

                            // we haven't calculated the 2D buffers for this slice, so we make them now
                            try {
                                srcImage.exportData(curSlice * sliceSize, sliceSize, sliceBuffer);
                            } catch (IOException ioe) {
                                sliceBuffer = null;
                                errorCleanUp("Algorithm Gradient Magnitude exportData: Image(s) locked", true);

                                return;
                            }

                            AlgorithmSeparableConvolver x2Convolver = new AlgorithmSeparableConvolver(x2Buffer,
                                                                                                      sliceBuffer,
                                                                                                      new int[] {
                                                                                                          srcImage.getExtents()[0],
                                                                                                          srcImage.getExtents()[1]
                                                                                                      },
                                                                                                      GxDataDerivative,
                                                                                                      GyDataRound,
                                                                                                      color);
                            x2Convolver.setActiveImage(activeImage);

                            if (!entireImage) {
                                x2Convolver.setMask(mask);
                            }

                            x2Convolver.run();
                            x2Convolver.finalize();
                            x2Convolver = null;

                            AlgorithmSeparableConvolver y2Convolver = new AlgorithmSeparableConvolver(y2Buffer,
                                                                                                      sliceBuffer,
                                                                                                      new int[] {
                                                                                                          srcImage.getExtents()[0],
                                                                                                          srcImage.getExtents()[1]
                                                                                                      }, GxDataRound,
                                                                                                      GyDataDerivative,
                                                                                                      color);
                            y2Convolver.setActiveImage(activeImage);

                            if (!entireImage) {
                                y2Convolver.setMask(mask);
                            }

                            y2Convolver.run();
                            y2Convolver.finalize();
                            y2Convolver = null;

                            cur2DBufferSlice = curSlice;
                        }

                        destImage.set(i, buffer[i]); // alpha
                        j = i % sliceSize; // index into 2D x2 and y2Buffer

                        if (red) {
                            destImage.set(i + 1,
                                          (float) Math.sqrt((x2Buffer[j + 1] * x2Buffer[j + 1]) +
                                                            (y2Buffer[j + 1] * y2Buffer[j + 1])));
                        }

                        if (red) {
                            destImage.set(i + 2,
                                          (float) Math.sqrt((x2Buffer[j + 2] * x2Buffer[j + 2]) +
                                                            (y2Buffer[j + 2] * y2Buffer[j + 2])));
                        }

                        if (blue) {
                            destImage.set(i + 3,
                                          (float) Math.sqrt((x2Buffer[j + 3] * x2Buffer[j + 3]) +
                                                            (y2Buffer[j + 3] * y2Buffer[j + 3])));
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
            curPercent = 5;
            maxPercent = 20;

            AlgorithmSeparableConvolver xConvolver = new AlgorithmSeparableConvolver(xBuffer, buffer,
                                                                                     srcImage.getExtents(),
                                                                                     GxDataDerivative, GyDataRound,
                                                                                     GzDataRound, color);
            xConvolver.setActiveImage(activeImage);

            if (!entireImage) {
                xConvolver.setMask(mask);
            }

            if (isProgressBarVisible()) {
                xConvolver.setProgressBar(progressBar, curPercent, maxPercent, true);
            }

            xConvolver.run();
            xConvolver.finalize();
            xConvolver = null;

            curPercent = maxPercent;
            maxPercent += 20;

            AlgorithmSeparableConvolver yConvolver = new AlgorithmSeparableConvolver(yBuffer, buffer,
                                                                                     srcImage.getExtents(), GxDataRound,
                                                                                     GyDataDerivative, GzDataRound,
                                                                                     color);
            yConvolver.setActiveImage(activeImage);

            if (!entireImage) {
                yConvolver.setMask(mask);
            }

            if (isProgressBarVisible()) {
                yConvolver.setProgressBar(progressBar, curPercent, maxPercent, true);
            }

            yConvolver.run();
            yConvolver.finalize();
            yConvolver = null;

            curPercent = maxPercent;
            maxPercent += 20;

            AlgorithmSeparableConvolver zConvolver = new AlgorithmSeparableConvolver(zBuffer, buffer,
                                                                                     srcImage.getExtents(), GxDataRound,
                                                                                     GyDataRound, GzDataDerivative,
                                                                                     color);
            zConvolver.setActiveImage(activeImage);

            if (!entireImage) {
                zConvolver.setMask(mask);
            }

            if (isProgressBarVisible()) {
                zConvolver.setProgressBar(progressBar, curPercent, maxPercent, true);
            }

            zConvolver.run();
            zConvolver.finalize();
            zConvolver = null;

            mod = length / (100 - maxPercent);

            for (i = 0; (i < length) && !threadStopped; i++) {

                if (((i % mod) == 0) && isProgressBarVisible()) {
                    maxPercent++;
                    progressBar.updateValue(maxPercent, activeImage);
                }

                curSlice = i / sliceSize;

                offsetZ = curSlice - (GzDataDerivative.length / 2);

                if ((offsetZ >= 0) && ((offsetZ + GzDataDerivative.length - 1) < zDim)) {

                    if (entireImage || mask.get(i)) {
                        destImage.set(i,
                                      (float) Math.sqrt((xBuffer[i] * xBuffer[i]) + (yBuffer[i] * yBuffer[i]) +
                                                        (zBuffer[i] * zBuffer[i])));
                    } else {
                        destImage.set(i, buffer[i]);
                    }
                } else {

                    if (entireImage || mask.get(i)) {

                        if (cur2DBufferSlice != curSlice) {

                            // we haven't calculated the 2D buffers for this slice, so we make them now
                            try {
                                srcImage.exportData(curSlice * sliceSize, sliceSize, sliceBuffer);
                            } catch (IOException ioe) {
                                sliceBuffer = null;
                                errorCleanUp("Algorithm Gradient Magnitude exportData: Image(s) locked", true);

                                return;
                            }

                            AlgorithmSeparableConvolver x2Convolver = new AlgorithmSeparableConvolver(x2Buffer,
                                                                                                      sliceBuffer,
                                                                                                      new int[] {
                                                                                                          srcImage.getExtents()[0],
                                                                                                          srcImage.getExtents()[1]
                                                                                                      },
                                                                                                      GxDataDerivative,
                                                                                                      GyDataRound,
                                                                                                      color);
                            x2Convolver.setActiveImage(activeImage);

                            if (!entireImage) {
                                x2Convolver.setMask(mask);
                            }

                            x2Convolver.run();
                            x2Convolver.finalize();
                            x2Convolver = null;

                            AlgorithmSeparableConvolver y2Convolver = new AlgorithmSeparableConvolver(y2Buffer,
                                                                                                      sliceBuffer,
                                                                                                      new int[] {
                                                                                                          srcImage.getExtents()[0],
                                                                                                          srcImage.getExtents()[1]
                                                                                                      }, GxDataRound,
                                                                                                      GyDataDerivative,
                                                                                                      color);
                            y2Convolver.setActiveImage(activeImage);

                            if (!entireImage) {
                                y2Convolver.setMask(mask);
                            }

                            y2Convolver.run();
                            y2Convolver.finalize();
                            y2Convolver = null;

                            cur2DBufferSlice = curSlice;
                        }

                        j = i % sliceSize; // index into 2D x2 and y2Buffer
                        destImage.set(i, (float) Math.sqrt((x2Buffer[j] * x2Buffer[j]) + (y2Buffer[j] * y2Buffer[j])));
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
        disposeProgressBar();
        setCompleted(true);
    }

    /**
     * Constructs a string of the contruction parameters and outputs the string to the messsage frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {
        String sigmaStr = new String();

        for (int i = 0; i < sigmas.length; i++) {
            sigmaStr += (" " + String.valueOf(sigmas[i]) + ", ");
        }

        if (srcImage.isColorImage() == true) {
            historyString = new String("GradientMagnitude(" + sigmaStr + String.valueOf(entireImage) + ", " +
                                       String.valueOf(image25D) + ", " + red + ", " + green + ", " + blue + ")\n");
        } else {
            historyString = new String("GradientMagnitude(" + sigmaStr + String.valueOf(entireImage) + ", " +
                                       String.valueOf(image25D) + ")\n");
        }
    }


    /**
     * DOCUMENT ME!
     *
     * @param  do3D  DOCUMENT ME!
     */
    private void makeKernels1D(boolean do3D) {
        int xkDim, ykDim, zkDim;
        int[] derivOrder = new int[1];
        kExtents = new int[1];

        xkDim = Math.round(5 * sigmas[0]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        if (xkDim < 3) {
            xkDim = 3;
        }

        ykDim = Math.round(5 * sigmas[1]);

        if ((ykDim % 2) == 0) {
            ykDim++;
        }

        if (ykDim < 3) {
            ykDim = 3;
        }

        kExtents[0] = xkDim;
        derivOrder[0] = 1;
        GxDataDerivative = new float[xkDim];

        float[] sigmasX = new float[1];
        sigmasX[0] = sigmas[0];

        GenerateGaussian Gx = new GenerateGaussian(GxDataDerivative, kExtents, sigmasX, derivOrder);
        Gx.calc(false);
        Gx.finalize();
        Gx = null;

        derivOrder[0] = 0;
        GxDataRound = new float[xkDim];
        Gx = new GenerateGaussian(GxDataRound, kExtents, sigmasX, derivOrder);
        Gx.calc(false);
        Gx.finalize();
        Gx = null;

        kExtents[0] = ykDim;
        derivOrder[0] = 1;

        float[] sigmasY = new float[1];
        sigmasY[0] = sigmas[1];
        GyDataDerivative = new float[ykDim];

        GenerateGaussian Gy = new GenerateGaussian(GyDataDerivative, kExtents, sigmasY, derivOrder);
        Gy.calc(false);
        Gy.finalize();
        Gy = null;

        derivOrder[0] = 0;
        GyDataRound = new float[ykDim];
        Gy = new GenerateGaussian(GyDataRound, kExtents, sigmasY, derivOrder);
        Gy.calc(false);
        Gy.finalize();
        Gy = null;

        if (do3D) {
            zkDim = Math.round(5 * sigmas[2]);

            if ((zkDim % 2) == 0) {
                zkDim++;
            }

            if (zkDim < 3) {
                zkDim = 3;
            }

            kExtents[0] = zkDim;
            derivOrder[0] = 1;
            GzDataDerivative = new float[zkDim];

            float[] sigmasZ = new float[1];
            sigmasZ[0] = sigmas[2];

            GenerateGaussian Gz = new GenerateGaussian(GzDataDerivative, kExtents, sigmasZ, derivOrder);
            Gz.calc(false);
            Gz.finalize();
            Gz = null;

            derivOrder[0] = 0;
            GzDataRound = new float[zkDim];
            Gz = new GenerateGaussian(GzDataRound, kExtents, sigmasZ, derivOrder);
            Gz.calc(false);
            Gz.finalize();
            Gz = null;
        }

    }

}
