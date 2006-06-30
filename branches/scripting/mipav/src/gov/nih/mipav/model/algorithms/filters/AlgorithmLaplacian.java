package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;

import java.util.*;


/**
 * Calculates the Laplacian of the gaussian of an image at a scale defined by the user.
 *
 * @version  0.1 Feb 11, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class AlgorithmLaplacian extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** An amplification factor greater than 1.0 causes this filter to act like a highpass filter. */
    private float amplificationFactor = 1.0f;

    /** If true an edge map image should be produced. */
    private boolean edgeImage;

    /**
     * Flag, if true, indicates that the whole image should be processed. If false on process the image over the mask
     * areas.
     */
    private boolean entireImage;

    /** Storage location of the second derivative of the Gaussian in the X direction. */
    private float[] GxxData;

    /** Storage location of the second derivative of the Gaussian in the Y direction. */
    private float[] GyyData;

    /** Storage location of the second derivative of the Gaussian in the Z direction. */
    private float[] GzzData;

    /** Dimensionality of the kernel. */
    private int[] kExtents;

    /** Lower and upper threshold values used in the generation of the edge map. */
    private float loThres, hiThres;

    /** Standard deviations of the gaussian used to calculate the kernels. */
    private float[] sigmas;


    /**
     * Mask(unsigned byte) of the zero-crossings of the Laplacian of the gaussian. Non-zero value indicates edge. Zero
     * in the mask image is background.
     */
    private ModelImage zXMask;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs a Laplacian algorithm object.
     *
     * @param  srcImg        source image model
     * @param  sigmas        Gaussian's standard deviations in the each dimension
     * @param  objectBuffer  array with true for object, false for background
     * @param  img25D        Flag, if true, indicates that each slice of the 3D volume should be processed
     *                       independently. 2D images disregard this flag.
     * @param  ampFactor     An amplification factor greater than 1.0 causes this filter to act like a highpass filter.
     */
    public AlgorithmLaplacian(ModelImage srcImg, float[] sigmas, boolean[] objectBuffer, boolean img25D,
                              float ampFactor) {
        super(null, srcImg);

        destImage = null; // Put results in destination image.
        srcImage = srcImg;
        this.sigmas = sigmas;
        edgeImage = false;
        entireImage = false;
        image25D = img25D;
        amplificationFactor = ampFactor;
        mask = new BitSet(objectBuffer.length);

        for (int i = 0; i < objectBuffer.length; i++) {

            if (objectBuffer[i]) {
                mask.set(i);
            } else {
                mask.clear(i);
            }
        } // for (int i = 0; i < objectBuffer.length; i++)
    }


    /**
     * Constructs a Laplacian algorithm object.
     *
     * @param  srcImg     source image model
     * @param  sigmas     Gaussian's standard deviations in the each dimension
     * @param  maskFlag   Flag that indicates that the Laplacian will be calculated for the whole image if equal to true
     * @param  img25D     Flag, if true, indicates that each slice of the 3D volume should be processed independently.
     *                    2D images disregard this flag.
     * @param  ampFactor  An amplification factor greater than 1.0 causes this filter to act like a highpass filter.
     */
    public AlgorithmLaplacian(ModelImage srcImg, float[] sigmas, boolean maskFlag, boolean img25D, float ampFactor) {
        super(null, srcImg);

        destImage = null; // Calc in place
        srcImage = srcImg;
        this.sigmas = sigmas;
        entireImage = maskFlag;
        edgeImage = false;
        image25D = img25D;
        amplificationFactor = ampFactor;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }
    }


    /**
     * Constructs a Laplacian algorithm object.
     *
     * @param  destImg    image model where result image is to stored
     * @param  srcImg     source image model
     * @param  sigmas     Gaussian's standard deviations in the each dimension
     * @param  maskFlag   Flag that indicates that the Laplacian will be calculated for the whole image if equal to true
     * @param  img25D     Flag, if true, indicates that each slice of the 3D volume should be processed independently.
     *                    2D images disregard this flag.
     * @param  ampFactor  An amplification factor greater than 1.0 causes this filter to act like a highpass filter.
     */
    public AlgorithmLaplacian(ModelImage destImg, ModelImage srcImg, float[] sigmas, boolean maskFlag, boolean img25D,
                              float ampFactor) {
        super(destImg, srcImg);

        destImage = destImg; // Put results in destination image.
        srcImage = srcImg;
        this.sigmas = sigmas;
        edgeImage = false;
        entireImage = maskFlag;
        image25D = img25D;
        amplificationFactor = ampFactor;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Generates a zero crossing mask for a 2D function sets a Bitset object to 1 is a zero crossing is detected.
     *
     * @param  xDim       X dimension length
     * @param  yDim       Y dimension length
     * @param  buffer     array of data in which to find level crossing
     * @param  edgeImage  edge map of level crossings
     * @param  level      level of crossings to find (e.g. zero crossing of the Laplacian)
     */
    public static void genLevelMask(int xDim, int yDim, float[] buffer, BitSet edgeImage, float level) {

        float x0, x1, x2, x3;
        int i, j, index;
        int indexY;

        int xxDim = xDim - 1;
        int yyDim = yDim - 1;

        edgeImage = new BitSet(xDim * yDim);

        x0 = buffer[0];
        x2 = buffer[xDim];

        for (j = 0; j < yyDim; j++) {
            indexY = j * xDim;

            for (i = 0; i < xxDim; i++) {
                index = indexY + i;
                x1 = buffer[index + 1];
                x3 = buffer[index + 1 + xDim];

                if ((x0 >= level) && (x1 >= level) && (x2 >= level) && (x3 >= level)) {
                    edgeImage.clear(index);
                } else if ((x0 <= level) && (x1 <= level) && (x2 <= level) && (x3 <= level)) {
                    edgeImage.clear(index);
                } else {
                    edgeImage.set(index);
                }

                x0 = x1;
                x2 = x3;
            }
        }
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        sigmas = null;
        kExtents = null;
        GxxData = null;
        GyyData = null;
        GzzData = null;
        destImage = null;
        srcImage = null;
        super.finalize();
    }

    /**
     * Accessor to return mask indicating zero crossings.
     *
     * @return  ModelImage of zero crossings ( 2D function 1 = indicates zero crossing
     */
    public ModelImage getZeroXMask() {
        return zXMask;
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
            makeKernels3D();
        } else if ((srcImage.getNDims() == 3) && (image25D == true)) {
            makeKernels2D();
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
            }
        } else {

            if (srcImage.getNDims() == 2) {
                calcInPlace2D(1);
            } else if ((srcImage.getNDims() == 3) && (image25D == false)) {
                calcInPlace3D();
            } else if ((srcImage.getNDims() == 3) && (image25D == true)) {
                calcInPlace2D(srcImage.getExtents()[2]);
            }
        }
    }

    /**
     * Call from dialog if you wish to produce edge image.
     *
     * @param  loThres  DOCUMENT ME!
     * @param  hiThres  DOCUMENT ME!
     */
    public void setEdgeOptions(float loThres, float hiThres) {
        this.edgeImage = true;
        this.loThres = loThres;
        this.hiThres = hiThres;
    }

    /**
     * Calculates the Laplacian image and replaces the source image with the new image.
     *
     * @param  nImages  number of images to be blurred. If 2D image then nImage = 1, if 3D image where each image is to
     *                  processed independently then nImages equals the number of images in the volume.
     */
    private void calcInPlace2D(int nImages) {

        int i, s;
        int length, totalLength;
        int start;
        float[] buffer;
        float[] resultBuffer;
        float lap;

        try {
            length = srcImage.getSliceSize();
            totalLength = length * nImages;
            buffer = new float[length];
            resultBuffer = new float[length * nImages];
            buildProgressBar(srcImage.getImageName(), "Calculating the Laplacian ...", 0, 100);
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Laplacian exportData: Out of memory", true);

            return;
        }

        int mod = totalLength / 100; // mod is 1 percent of length
        initProgressBar();

        for (s = 0; (s < nImages) && !threadStopped; s++) {
            start = s * length;

            try {
                srcImage.exportData(start, length, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                resultBuffer = null;
                System.gc();
                displayError("Algorithm Laplacian: Image(s) locked");
                setCompleted(false);
                disposeProgressBar();

                return;
            }

            for (i = 0; (i < length) && !threadStopped; i++) {

                if ((((start + i) % mod) == 0) && isProgressBarVisible()) {
                    progressBar.updateValue(Math.round((float) (start + i) / (totalLength - 1) * 100), runningInSeparateThread);
                }

                if ((entireImage == true) || mask.get(i)) {
                    lap = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), buffer, kExtents, GxxData);
                    resultBuffer[start + i] = lap;
                } else {
                    resultBuffer[start + i] = buffer[i];
                    // resultBuffer[i] = 0;
                }
            }
        }

        if (threadStopped) {
            finalize();

            return;
        }

        try {
            srcImage.importData(0, resultBuffer, true);

            if ((edgeImage == true) && (nImages == 1)) {
                genZeroXMask(resultBuffer);
            }
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Laplacian importData: Image(s) locked", true);

            return;
        }

        disposeProgressBar();
        setCompleted(true);
    }

    /**
     * Calculates the Laplacian and replaces the source image with the new image.
     */
    private void calcInPlace3D() {

        int i;
        int length;
        float[] buffer;
        float[] resultBuffer;
        float lap;

        try {
            length = srcImage.getSliceSize() * srcImage.getExtents()[2];
            buffer = new float[length];
            resultBuffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            buildProgressBar(srcImage.getImageName(), "Calculating the Laplacian ...", 0, 100);
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Laplacian exportData: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            errorCleanUp("Algorithm Laplacian exportData: Out of memory", true);

            return;
        }

        initProgressBar();

        int mod = length / 100; // mod is 1 percent of length

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0) && isProgressBarVisible()) {
                progressBar.updateValue(Math.round((float) i / (length - 1) * 100), runningInSeparateThread);
            }

            if ((entireImage == true) || mask.get(i)) {
                lap = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GxxData);
                resultBuffer[i] = lap;
            } else {
                resultBuffer[i] = buffer[i];
                // resultBuffer[i] = 0;
            }
        }

        buffer = null;
        System.gc();

        if (threadStopped) {
            finalize();

            return;
        }

        try {
            srcImage.importData(0, resultBuffer, true);
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Laplacian importData: Image(s) locked", true);

            return;
        }

        setCompleted(true);
        disposeProgressBar();
    }

    /**
     * This function produces the Laplacian of input image.
     *
     * @param  nImages  number of images to be blurred. If 2D image then nImage = 1, if 3D image where each image is to
     *                  processed independently then nImages equals the number of images in the volume.
     */
    private void calcStoreInDest2D(int nImages) {
        int i, s, idx;
        int length, totalLength;
        int start;
        float[] buffer;
        float lap;

        try {
            destImage.setLock();
        } catch (IOException error) {
            errorCleanUp("Algorithm Laplacian: Image(s) locked", false);

            return;
        }

        try {
            length = srcImage.getSliceSize();
            totalLength = length * nImages;
            buffer = new float[length];
            buildProgressBar(srcImage.getImageName(), "Calculating the Laplacian ...", 0, 100);
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Laplacian exportData: Out of memory", true);

            return;
        }

        int mod = totalLength / 100; // mod is 1 percent of length
        initProgressBar();

        for (s = 0; (s < nImages) && !threadStopped; s++) {
            start = s * length;

            try {
                srcImage.exportData(start, length, buffer); // locks and releases lock
            } catch (IOException error) {
                displayError("Algorithm Gaussian Blur: Image(s) locked");
                setCompleted(false);
                disposeProgressBar();
                destImage.releaseLock();

                return;
            }

            float min, max;
            min = Float.MAX_VALUE;
            max = -Float.MAX_VALUE;

            float minL, maxL;
            minL = Float.MAX_VALUE;
            maxL = -Float.MAX_VALUE;

            if (entireImage == false) {

                for (i = 0; i < length; i++) {

                    if (mask.get(i)) {

                        if (buffer[i] > max) {
                            max = buffer[i];
                        } else if (buffer[i] < min) {
                            min = buffer[i];
                        }
                    }
                }
            }

            for (i = 0, idx = start; (i < length) && !threadStopped; i++, idx++) {

                if ((((start + i) % mod) == 0) && isProgressBarVisible()) {
                    progressBar.updateValue(Math.round((float) (start + i) / (totalLength - 1) * 100), runningInSeparateThread);
                }

                if ((entireImage == true) || mask.get(i)) {

                    lap = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), buffer, kExtents, GxxData);

                    if (entireImage == false) {

                        if (mask.get(i)) {

                            if (lap > maxL) {
                                maxL = lap;
                            } else if (lap < minL) {
                                minL = lap;
                            }
                        }
                    }

                    destImage.set(idx, lap);
                } else {
                    destImage.set(idx, buffer[i]);
                }
            }

            if (entireImage == false) {

                for (i = 0; i < length; i++) {

                    if (mask.get(i)) {
                        lap = destImage.getFloat(i);
                        lap = (((lap - minL) / (maxL - minL)) * (max - min)) + min;
                        destImage.set(i, lap);
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


        if (image25D == false) { // Image is 2D  and not 2.5D

            try {
                destImage.exportData(0, length, buffer);
            } catch (IOException error) {
                buffer = null;
                errorCleanUp("Algorithm Laplacian exportData: " + error, true);

                return;
            }
        }

        if ((edgeImage == true) && (nImages == 1)) {
            genZeroXMask(buffer);
        }

        disposeProgressBar();
        setCompleted(true);
    }


    /**
     * This function produces the Laplacian of input image.
     */
    private void calcStoreInDest3D() {

        int i;
        int length;
        float[] buffer;
        float lap;

        try {
            destImage.setLock();
        } catch (IOException error) {
            displayError("Algorithm Laplacian: Image(s) locked");
            setCompleted(false);
            destImage.releaseLock();

            return;
        }

        try {
            length = srcImage.getSliceSize() * srcImage.getExtents()[2];
            buffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            buildProgressBar(srcImage.getImageName(), "Calculating the Laplacian ...", 0, 100);
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm Laplacian exportData: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Laplacian exportData: Out of memory", true);

            return;
        }

        initProgressBar();

        int mod = length / 100; // mod is 1 percent of length

        float min, max;
        min = Float.MAX_VALUE;
        max = -Float.MAX_VALUE;

        float minL, maxL;
        minL = Float.MAX_VALUE;
        maxL = -Float.MAX_VALUE;

        if (entireImage == false) {

            for (i = 0; i < length; i++) {

                if (mask.get(i)) {

                    if (buffer[i] > max) {
                        max = buffer[i];
                    } else if (buffer[i] < min) {
                        min = buffer[i];
                    }
                }
            }
        }

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0) && isProgressBarVisible()) {
                progressBar.updateValue(Math.round((float) i / (length - 1) * 100), runningInSeparateThread);
            }

            if ((entireImage == true) || mask.get(i)) {
                lap = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GxxData);

                if (entireImage == false) {

                    if (mask.get(i)) {

                        if (lap > maxL) {
                            maxL = lap;
                        } else if (lap < minL) {
                            minL = lap;
                        }
                    }
                }

                destImage.set(i, lap);
            } else {
                destImage.set(i, buffer[i]);
            }
        }

        if (entireImage == false) {

            for (i = 0; i < length; i++) {

                if (mask.get(i)) {
                    lap = destImage.getFloat(i);
                    lap = (((lap - minL) / (maxL - minL)) * (max - min)) + min;
                    destImage.set(i, lap);
                }
            }
        }

        if (threadStopped) {
            finalize();

            return;
        }

        buffer = null;
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

        historyString = new String("Laplacian(" + sigmaStr + String.valueOf(entireImage) + ", " +
                                   String.valueOf(image25D) + ", " + String.valueOf(amplificationFactor) + ")\n");

    }

    /**
     * Generates a zero crossing mask for a 2D function sets a ModelImage to 1 if a zero crossing is detected.
     *
     * @param  buffer  array in which to find zero crossing
     */
    private void genZeroXMask(float[] buffer) {

        float x0, x1, x2, x3;
        int i, j, index;
        int indexY;

        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];

        int xxDim = xDim - 1;
        int yyDim = yDim - 1;
        int level = 0;

        int[] destExtents = new int[2];
        destExtents[0] = srcImage.getExtents()[0]; // X dim
        destExtents[1] = srcImage.getExtents()[1]; // Y dim

        zXMask = new ModelImage(ModelImage.UBYTE, destExtents, " Edges", srcImage.getUserInterface());

        x0 = buffer[0];
        x2 = buffer[xDim];

        if ((x0 >= loThres) && (x0 <= hiThres)) {
            x0 = 0;
        }

        if ((x2 >= loThres) && (x2 <= hiThres)) {
            x2 = 0;
        }

        for (j = 0; j < yyDim; j++) {
            indexY = j * xDim;

            for (i = 0; i < xxDim; i++) {
                index = indexY + i;
                x1 = buffer[index + 1];
                x3 = buffer[index + 1 + xDim];

                if ((x1 >= loThres) && (x1 <= hiThres)) {
                    x1 = 0;
                }

                if ((x3 >= loThres) && (x3 <= hiThres)) {
                    x3 = 0;
                }

                if ((x0 >= level) && (x1 >= level) && (x2 >= level) && (x3 >= level)) {
                    zXMask.set(index, 0);
                } else if ((x0 <= level) && (x1 <= level) && (x2 <= level) && (x3 <= level)) {
                    zXMask.set(index, 0);
                } else {
                    zXMask.set(index, 255);
                }

                x0 = x1;
                x2 = x3;
            }
        }

        zXMask.calcMinMax();

        FileInfoBase[] fileInfo;
        fileInfo = zXMask.getFileInfo();
        fileInfo[0].setModality(srcImage.getFileInfo()[0].getModality());
        fileInfo[0].setFileDirectory(srcImage.getFileInfo()[0].getFileDirectory());
        fileInfo[0].setEndianess(srcImage.getFileInfo()[0].getEndianess());
        fileInfo[0].setUnitsOfMeasure(srcImage.getUnitsOfMeasure());
        fileInfo[0].setResolutions(srcImage.getResolutions(0));
        fileInfo[0].setExtents(zXMask.getExtents());
        fileInfo[0].setMax(zXMask.getMax());
        fileInfo[0].setMin(zXMask.getMin());
        fileInfo[0].setPixelPadValue(srcImage.getFileInfo()[0].getPixelPadValue());
        fileInfo[0].setPhotometric(srcImage.getFileInfo()[0].getPhotometric());
    }

    /**
     * Creates Gaussian derivative kernels.
     */
    private void makeKernels2D() {
        int xkDim, ykDim;
        int[] derivOrder = new int[2];

        kExtents = new int[2];
        derivOrder[0] = 2;
        derivOrder[1] = 0;

        xkDim = Math.round(11 * sigmas[0]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        if (xkDim < 3) {
            xkDim = 3;
        }

        kExtents[0] = xkDim;

        ykDim = Math.round(11 * sigmas[1]);

        if ((ykDim % 2) == 0) {
            ykDim++;
        }

        if (ykDim < 3) {
            ykDim = 3;
        }

        kExtents[1] = ykDim;

        GxxData = new float[xkDim * ykDim];

        GenerateGaussian Gxx = new GenerateGaussian(GxxData, kExtents, sigmas, derivOrder);
        Gxx.calc(false);

        derivOrder[0] = 0;
        derivOrder[1] = 2;
        GyyData = new float[xkDim * ykDim];

        GenerateGaussian Gyy = new GenerateGaussian(GyyData, kExtents, sigmas, derivOrder);
        Gyy.calc(false);

        float tmp;

        for (int i = 0; i < GyyData.length; i++) {
            tmp = -(GxxData[i] + GyyData[i]);

            if (tmp > 0) {
                tmp *= amplificationFactor;
            }

            GxxData[i] = tmp;
        }
    }


    /**
     * Creates Gaussian derivative kernels.
     */
    private void makeKernels3D() {
        int xkDim, ykDim, zkDim;
        int[] derivOrder = new int[3];

        kExtents = new int[3];
        derivOrder[0] = 2;
        derivOrder[1] = 0;
        derivOrder[2] = 0;

        xkDim = Math.round(11 * sigmas[0]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        if (xkDim < 3) {
            xkDim = 3;
        }

        kExtents[0] = xkDim;

        ykDim = Math.round(11 * sigmas[1]);

        if ((ykDim % 2) == 0) {
            ykDim++;
        }

        if (ykDim < 3) {
            ykDim = 3;
        }

        kExtents[1] = ykDim;

        float scaleFactor = sigmas[2];
        sigmas[2] = sigmas[1];
        zkDim = Math.round(11 * sigmas[2]);

        if ((zkDim % 2) == 0) {
            zkDim++;
        }

        if (zkDim < 3) {
            zkDim = 3;
        }

        kExtents[2] = zkDim;


        GxxData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gxx = new GenerateGaussian(GxxData, kExtents, sigmas, derivOrder);
        Gxx.calc(false);

        derivOrder[0] = 0;
        derivOrder[1] = 2;
        derivOrder[2] = 0;
        GyyData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gyy = new GenerateGaussian(GyyData, kExtents, sigmas, derivOrder);
        Gyy.calc(false);

        derivOrder[0] = 0;
        derivOrder[1] = 0;
        derivOrder[2] = 2;
        GzzData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gzz = new GenerateGaussian(GzzData, kExtents, sigmas, derivOrder);
        Gzz.calc(false);

        float tmp;

        for (int i = 0; i < GyyData.length; i++) {
            tmp = -(GxxData[i] + GyyData[i] + (GzzData[i] * scaleFactor));

            if (tmp > 0) {
                tmp *= amplificationFactor;
            }

            GxxData[i] = tmp;
        }

        // for (int i = 0; i < GyyData.length; i++){
        // GxxData[i] = -(GxxData[i]+GyyData[i]+GzzData[i]*scaleFactor);
        // }
    }

}
