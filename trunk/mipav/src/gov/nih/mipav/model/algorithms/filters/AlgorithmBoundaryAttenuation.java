package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;


/**
 * Attenuate around the boundary of an object defined by a VOI in an image volume.
 *
 * @author  Evan McCreedy
 */
public class AlgorithmBoundaryAttenuation extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The attenuated VOI mask buffer. */
    private float[] attenuationBuffer;

    /** The erosion kernel x and y dims. */
    private int kDimXY;

    /** The erosion kernel z dimension. */
    private int kDimZ;

    /** The erosion kernel. */
    private BitSet kernel;

    /** The mask image generated from the VOIs within the image. */
    private ModelImage maskImage;

    /** The maximum amount of attenuation to perform (between 0 and 1). */
    private float maxAttenuation;

    /** The number of erosions to perform (and the number of levels of attenuation to do). */
    private int numErosions;

    /** The image's x dimension. */
    private int xDim;

    /** The image's x resolution. */
    private float xRes;

    /** The image's y dimension. */
    private int yDim;

    /** The image's z dimension. */
    private int zDim;

    /** The image's z resolution. */
    private float zRes;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Set up the algorithm. Extract a mask to use in the attenuation using VOIs within the image and set up the kernel.
     *
     * @param  srcImg          the image to attenuate (must have a VOI in it to define the area to attenuate)
     * @param  numErosions     the number of erosions to do
     * @param  maxAttenuation  the maximum amount to reduce the object intensity by (0,1)
     */
    public AlgorithmBoundaryAttenuation(ModelImage srcImg, int numErosions, float maxAttenuation) {
        srcImage = srcImg;

        // find vois, extract to mask
        maskImage = new ModelImage(ModelStorageBase.SHORT, srcImg.getExtents(), srcImg.getImageName() + "_attenu_mask");

        AlgorithmMask maskAlgo = new AlgorithmMask(maskImage, srcImg, 1, true, true);
        maskAlgo.setProgressBarVisible(false);
        maskAlgo.run();

        this.numErosions = numErosions;
        this.maxAttenuation = maxAttenuation;

        xDim = srcImg.getExtents()[0];
        yDim = srcImg.getExtents()[1];
        zDim = srcImg.getExtents()[2];

        xRes = srcImg.getFileInfo(0).getResolutions()[0];
        zRes = srcImg.getFileInfo(0).getResolutions()[2];

        makeKernel(AlgorithmMorphology3D.CONNECTED6);

        attenuationBuffer = new float[xDim * yDim * zDim];
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Clean up memory used by the algorithm.
     */
    public void finalize() {
        maskImage.disposeLocal();
        kernel = null;
        attenuationBuffer = null;

        super.finalize();
    }

    /**
     * Return the attenuated image.
     *
     * @return  the attenuated image
     */
    public ModelImage getResultImage() {
        return destImage;
    }

    /**
     * Start the algorithm.
     */
    public void runAlgorithm() {
        buildProgressBar(srcImage.getImageName(), "Boundary Attenuation...", 0, 100);
        initProgressBar();

        erode(maskImage, numErosions);

        if (threadStopped) {
            setCompleted(false);
            disposeProgressBar();
            finalize();

            return;
        }

        if (isProgressBarVisible()) {
            progressBar.setMessage("Filling object interior...");
        }

        int totalPercent = 10;
        int mod = attenuationBuffer.length / totalPercent;

        for (int i = 0; i < attenuationBuffer.length; i++) {

            if (isProgressBarVisible() && ((i % mod) == 0)) {
                progressBar.updateValue(progressBar.getValue() + 1, activeImage);
            }

            if ((attenuationBuffer[i] == 0) && (maskImage.getFloat(i) == 1)) {
                attenuationBuffer[i] = 1;
            }
        }

        if (threadStopped) {
            setCompleted(false);
            disposeProgressBar();
            finalize();

            return;
        }

        if (isProgressBarVisible()) {
            progressBar.setMessage("Blurring attenuation mask...");
        }

        // blur attenuation image
        ModelImage tmpImg = new ModelImage(ModelStorageBase.FLOAT, srcImage.getExtents(),
                                           srcImage.getImageName() + "_attenuation", srcImage.getUserInterface());

        try {
            tmpImg.importData(0, attenuationBuffer, true);
        } catch (IOException ioe) {
            MipavUtil.displayError("Error importing attenuation buffer");

            return;
        }

        float[] sigmas = new float[] { 2.0f, 2.0f, 2.0f * (xRes / zRes) };
        AlgorithmGaussianBlurSep blurAlgo = new AlgorithmGaussianBlurSep(tmpImg, sigmas, false, false);
        blurAlgo.setProgressBarVisible(false);
        blurAlgo.setMask(srcImage.generateVOIMask());
        blurAlgo.run();

        // new ViewJFrameImage( tmpImg, null, srcImage.getUserInterface().getNewFrameLocation(),
        // srcImage.getUserInterface(), false );

        if (threadStopped) {
            setCompleted(false);
            disposeProgressBar();
            finalize();

            return;
        }

        if (isProgressBarVisible()) {
            progressBar.setMessage("Attenuating image...");
            progressBar.updateValue(progressBar.getValue() + 40, activeImage);
        }

        // combine attenuation buffer with srcImage and put into destImage
        try {
            tmpImg.exportData(0, attenuationBuffer.length, attenuationBuffer);
        } catch (IOException ioe) {
            MipavUtil.displayError("Error exporting blurred attenuation buffer");

            return;
        }

        tmpImg.disposeLocal();

        if (threadStopped) {
            setCompleted(false);
            disposeProgressBar();
            finalize();

            return;
        }

        if (isProgressBarVisible()) {
            progressBar.updateValue(progressBar.getValue() + 5, activeImage);
        }

        totalPercent = 10;
        mod = attenuationBuffer.length / totalPercent;

        for (int i = 0; i < attenuationBuffer.length; i++) {

            if (isProgressBarVisible() && ((i % mod) == 0)) {
                progressBar.updateValue(progressBar.getValue() + 1, activeImage);
            }

            if (attenuationBuffer[i] == 0) {
                attenuationBuffer[i] = srcImage.getFloat(i);
            } else {
                attenuationBuffer[i] = srcImage.getFloat(i) * attenuationBuffer[i];
            }
        }

        if (threadStopped) {
            setCompleted(false);
            disposeProgressBar();
            finalize();

            return;
        }

        if (isProgressBarVisible()) {
            progressBar.updateValue(95, activeImage);
        }

        destImage = new ModelImage(srcImage.getType(), srcImage.getExtents(), srcImage.getImageName() + "_attenuated");

        try {
            destImage.importData(0, attenuationBuffer, true);
        } catch (IOException ioe) {
            MipavUtil.displayError("Error importing final attenuated image buffer");

            return;
        }

        destImage.copyFileTypeInfo(srcImage);

        if (isProgressBarVisible()) {
            progressBar.updateValue(100, activeImage);
        }

        disposeProgressBar();
        setCompleted(true);
    }

    /**
     * Erode an image and mark the pixels we erode for attenuation later.
     *
     * @param  img         the image to erode
     * @param  iterations  the number of erosions to perform
     */
    private void erode(ModelImage img, int iterations) {

        // if thread has already been stopped, dump out
        if (threadStopped) {
            setCompleted(false);
            disposeProgressBar();
            finalize();

            return;
        }

        boolean clear;
        boolean attenuatePixel;
        short value = 0;
        int curIter;
        int i, j, k, pix, count;
        int indexY;
        int indexYU;
        int offsetX, offsetY, offsetZ;
        int offsetXU;
        int startX, startY, startZ;
        int endX, endY, endZ;
        short[] tempBuffer;

        int halfKDim = kDimXY / 2;
        int halfKDimZ = kDimZ / 2;

        int sliceSize = xDim * yDim;
        int imgSize = sliceSize * zDim;
        int stepZ = kDimZ * sliceSize;
        int stepY = kDimXY * xDim;

        int totalSize = imgSize * iterations;
        int tmpSize = 0;
        int mod = totalSize / 30;

        short[] processBuffer;
        short[] imgBuffer;

        try {
            processBuffer = new short[imgSize];
            imgBuffer = new short[imgSize];
            img.exportData(0, imgSize, imgBuffer); // locks and releases lock
        } catch (IOException error) {
            displayError("BoundaryAttenuation: Image(s) locked");
            setCompleted(false);
            disposeProgressBar();

            return;
        } catch (OutOfMemoryError error) {
            displayError("BoundaryAttenuation: Out of memory");
            setCompleted(false);
            disposeProgressBar();

            return;
        }

        try {

            if (progressBar != null) {
                progressBar.setMessage("Eroding image ...");
            }

            if (progressBar != null) {
                progressBar.updateValue(0, activeImage);
            }
        } catch (NullPointerException npe) {

            if (threadStopped) {
                Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                  Preferences.DEBUG_ALGORITHM);
            }
        }

        float linearStep = (1.0f - maxAttenuation) / iterations;
        float linearAttenuation;

        for (curIter = 0; (curIter < iterations) && !threadStopped; curIter++) {
            linearAttenuation = maxAttenuation + (linearStep * curIter);

            tmpSize = curIter * imgSize;

            for (pix = 0; (pix < imgSize) && !threadStopped; pix++) {

                try {

                    if ((((tmpSize + pix) % mod) == 0) && isProgressBarVisible()) {
                        progressBar.updateValue(Math.round((float) (tmpSize + pix) / totalSize * 30), activeImage);
                    }
                } catch (NullPointerException npe) {

                    if (threadStopped) {
                        Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                          Preferences.DEBUG_ALGORITHM);
                    }
                }

                value = imgBuffer[pix];

                if (imgBuffer[pix] == 0) {
                    clear = true;
                    attenuatePixel = false;
                } else {
                    clear = false;
                    attenuatePixel = false;
                    offsetX = (pix % xDim) - halfKDim;
                    offsetXU = offsetX + kDimXY;
                    offsetY = ((pix / xDim) % yDim) - halfKDim;
                    offsetZ = (pix / (sliceSize)) - halfKDimZ;

                    count = 0;
                    indexY = offsetY * xDim;
                    indexYU = indexY + stepY;
                    startZ = offsetZ * sliceSize;
                    endZ = startZ + stepZ;

                    if (startZ < 0) {
                        startZ = 0;
                    }

                    if (endZ > imgSize) {
                        endZ = imgSize;
                    }

                    if (indexY < 0) {
                        indexY = 0;
                    }

                    if (indexYU > sliceSize) {
                        indexYU = sliceSize;
                    }

                    if (offsetX < 0) {
                        offsetX = 0;
                    }

                    if (offsetXU > xDim) {
                        offsetXU = xDim;
                    }


kernelLoop:
                    for (k = startZ; k < endZ; k += sliceSize) {
                        startY = k + indexY;
                        endY = k + indexYU;

                        for (j = startY; j < endY; j += xDim) {
                            startX = j + offsetX;
                            endX = j + offsetXU;

                            for (i = startX; i < endX; i++) {

                                if ((kernel.get(count) == true) && (imgBuffer[i] == 0)) {
                                    clear = true;
                                    attenuatePixel = true;

                                    break kernelLoop;
                                }

                                count++;
                            }
                        }
                    }
                }

                if ((clear == true) && attenuatePixel) {
                    processBuffer[pix] = 0;
                    attenuationBuffer[pix] = linearAttenuation;
                } else {
                    processBuffer[pix] = value;
                }
            }

            tempBuffer = imgBuffer;
            imgBuffer = processBuffer;
            processBuffer = tempBuffer;
        }
    }

    /**
     * Generates a kernel of the indicated type.
     *
     * @param  kernelType  type of kernel to be generated
     */
    private void makeKernel(int kernelType) {

        switch (kernelType) {

            case AlgorithmMorphology3D.CONNECTED6:
                kDimXY = 3;
                kDimZ = 3;
                kernel = new BitSet(27);
                kernel.clear(0);
                kernel.clear(1);
                kernel.clear(2);
                kernel.clear(3);
                kernel.set(4);
                kernel.clear(5);
                kernel.clear(6);
                kernel.clear(7);
                kernel.clear(8);
                kernel.clear(9);
                kernel.set(10);
                kernel.clear(11);
                kernel.set(12);
                kernel.set(13);
                kernel.set(14);
                kernel.clear(15);
                kernel.set(16);
                kernel.clear(17);
                kernel.clear(18);
                kernel.clear(19);
                kernel.clear(20);
                kernel.clear(21);
                kernel.set(22);
                kernel.clear(23);
                kernel.clear(24);
                kernel.clear(25);
                kernel.clear(26);
                break;

            case AlgorithmMorphology3D.CONNECTED24:
                kDimXY = 5;
                kDimZ = 5;
                kernel = new BitSet(125);

                for (int i = 0; i < 125; i++) {
                    kernel.clear(i);
                }

                kernel.set(12);

                kernel.set(32);
                kernel.set(36);
                kernel.set(37);
                kernel.set(38);
                kernel.set(42);

                kernel.set(52);
                kernel.set(56);
                kernel.set(57);
                kernel.set(58);
                kernel.set(60);
                kernel.set(61);
                kernel.set(62);
                kernel.set(63);
                kernel.set(64);
                kernel.set(66);
                kernel.set(67);
                kernel.set(68);
                kernel.set(72);

                kernel.set(82);
                kernel.set(86);
                kernel.set(87);
                kernel.set(88);
                kernel.set(92);

                kernel.set(112);
                break;

            default:
        }
    }
}
