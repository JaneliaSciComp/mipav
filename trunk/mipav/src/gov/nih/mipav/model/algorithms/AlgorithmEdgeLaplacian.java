package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;


/**
 * Calculates the EdgeLap of an image at a scale defined by the user. This algorithm produces an edge map of the zero
 * crossings of the laplacian of the gaussian for 2D images and 2.5D images.
 *
 * @version  0.1 Feb 11, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class AlgorithmEdgeLaplacian extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Perform zero crossing detection using the marching squares method. */
    public static final int MARCHING_SQUARES = 0;

    /** Perform zero crossing detection using Matt's old method. */
    public static final int OLD_DETECTION = 1;

    /** Mark negative areas of the laplacian image as edges (makes thicker edges - used in BSE). */
    public static final int NEGATIVE_EDGES = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Flag, if true, indicates that the whole image should be processed. If false only process the image over the mask
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

    /** The type of zero crossing detection to use. */
    private int zeroDetectionType = MARCHING_SQUARES;

    /**
     * Mask(unsigned byte) of the zero-crossings of the Laplacian of the gaussian. Non-zero value indicates edge. Zero
     * in the mask image is background.
     */
    private ModelImage zXMask;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmEdgeLaplacian object.
     *
     * @param  destImg   image model where result image is to stored
     * @param  srcImg    source image model
     * @param  sigmas    Gaussian's standard deviations in the each dimension
     * @param  maskFlag  Flag that indicates that the EdgeLap will be calculated for the whole image if equal to true
     * @param  img25D    Flag, if true, indicates that each slice of the 3D volume should be processed independently. 2D
     *                   images disregard this flag.
     */
    public AlgorithmEdgeLaplacian(ModelImage destImg, ModelImage srcImg, float[] sigmas, boolean maskFlag,
                                  boolean img25D) {
        super(destImg, srcImg);

        this.sigmas = sigmas;
        image25D = img25D;

        entireImage = maskFlag;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }
    }

    /**
     * Creates a new AlgorithmEdgeLaplacian object.
     *
     * @param  destImg   image model where result image is to stored
     * @param  srcImg    source image model
     * @param  sigmas    Gaussian's standard deviations in the each dimension
     * @param  maskFlag  Flag that indicates that the EdgeLap will be calculated for the whole image if equal to true
     * @param  img25D    Flag, if true, indicates that each slice of the 3D volume should be processed independently. 2D
     *                   images disregard this flag.
     * @param  loThres   used to define the lower threshold to reduce noise (edges) in the laplacian image
     * @param  hiThres   used to define the upper threshold to reduce noise (edges) in the laplacian image
     */
    public AlgorithmEdgeLaplacian(ModelImage destImg, ModelImage srcImg, float[] sigmas, boolean maskFlag,
                                  boolean img25D, float loThres, float hiThres) {
        super(destImg, srcImg);

        this.loThres = loThres;
        this.hiThres = hiThres;
        this.sigmas = sigmas;
        image25D = img25D;

        entireImage = maskFlag;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Generates a zero crossing mask for a 2D function sets a Bitset object to 1 is a zero crossing is detected.
     *
     * @param   xDim           x dimension of image
     * @param   yDim           y dimension of image
     * @param   buffer         array in which to find zero crossing
     * @param   level          level to generate zero crossings at
     * @param   detectionType  the type of zero crossing method to use
     * @param   loThres        low threshold
     * @param   hiThres        high threshold
     *
     * @return  Bitset representing zero crossings
     */
    public static BitSet genLevelMask(int xDim, int yDim, float[] buffer, float level, int detectionType, float loThres,
                                      float hiThres) {
        int i0, i1, i2, i3;
        float x0, x1, x2, x3;
        int i, j;
        int indexY;

        int xxDim = xDim - 1;
        int yyDim = yDim - 1;

        BitSet edgeImage = new BitSet(xDim * yDim);

        for (j = 0; j < yyDim; j++) {
            indexY = j * xDim;

            for (i = 0; i < xxDim; i++) {
                i0 = indexY + i;
                i1 = i0 + 1;
                i2 = i0 + xDim;
                i3 = i0 + 1 + xDim;

                x0 = buffer[i0];
                x1 = buffer[i1];
                x2 = buffer[i2];
                x3 = buffer[i3];

                if (detectionType == MARCHING_SQUARES) {

                    if ((x0 >= level) && (x1 >= level) && (x2 >= level) && (x3 >= level)) { // case 0 - no edge
                    } else if ((x0 >= level) && (x1 >= level) && (x2 < level) && (x3 >= level)) {

                        // case 1 - edge in the lower left
                        edgeImage.set(i2);
                    } else if ((x0 >= level) && (x1 >= level) && (x2 >= level) && (x3 < level)) {

                        // case 2 - edge in the lower right
                        edgeImage.set(i3);
                    } else if ((x0 >= level) && (x1 >= level) && (x2 < level) && (x3 < level)) {

                        // case 3 - edge horizontally
                        edgeImage.set(i2);
                        edgeImage.set(i3);
                    } else if ((x0 >= level) && (x1 < level) && (x2 >= level) && (x3 >= level)) {

                        // case 4 - edge in the upper right
                        edgeImage.set(i1);
                    } else if ((x0 >= level) && (x1 < level) && (x2 < level) && (x3 >= level)) {

                        // case 5 - ambiguous case; either edge in upper right and lower left or
                        // edge that goes from the upper right to the lower left
                        edgeImage.set(i1);
                        edgeImage.set(i2);
                    } else if ((x0 >= level) && (x1 < level) && (x2 >= level) && (x3 < level)) {

                        // case 6 - edge going vertically along the right
                        edgeImage.set(i1);
                        edgeImage.set(i3);
                    } else if ((x0 >= level) && (x1 < level) && (x2 < level) && (x3 < level)) {

                        // case 7 - edge in the upper left
                        edgeImage.set(i0);
                    } else if ((x0 < level) && (x1 >= level) && (x2 >= level) && (x3 >= level)) {

                        // case 8 - edge in the upper left
                        edgeImage.set(i0);
                    } else if ((x0 < level) && (x1 >= level) && (x2 < level) && (x3 >= level)) {

                        // case 9 - edge going vertically along the left
                        edgeImage.set(i0);
                        edgeImage.set(i2);
                    } else if ((x0 < level) && (x1 >= level) && (x2 >= level) && (x3 < level)) {

                        // case 10 - ambiguous case; either edge in upper left and lower right or
                        // edge that goes from the upper left to the lower right
                        edgeImage.set(i0);
                        edgeImage.set(i3);
                    } else if ((x0 < level) && (x1 >= level) && (x2 < level) && (x3 < level)) {

                        // case 11 - edge in the upper right
                        edgeImage.set(i1);
                    } else if ((x0 < level) && (x1 < level) && (x2 >= level) && (x3 >= level)) {

                        // case 12 - edge going horizontally along the top
                        edgeImage.set(i0);
                        edgeImage.set(i1);
                    } else if ((x0 < level) && (x1 < level) && (x2 < level) && (x3 >= level)) {

                        // case 13 - edge in the lower right
                        edgeImage.set(i3);
                    } else if ((x0 < level) && (x1 < level) && (x2 >= level) && (x3 < level)) {

                        // case 14 - edge in the lower left
                        edgeImage.set(i2);
                    } else if ((x0 < level) && (x1 < level) && (x2 < level) && (x3 < level)) { // case 15 - no edge
                    }
                } else if (detectionType == NEGATIVE_EDGES) {

                    if (buffer[i0] <= 0) {
                        edgeImage.set(i0);
                    }
                } else if (detectionType == OLD_DETECTION) {

                    if ((x0 >= level) && (x1 >= level) && (x2 >= level) && (x3 >= level)) {
                        edgeImage.clear(i0);
                    } else if ((x0 <= level) && (x1 <= level) && (x2 <= level) && (x3 <= level)) {
                        edgeImage.clear(i0);
                    } else {
                        edgeImage.set(i0);
                    }
                }
            }
        }

        // System.out.println("Cardinality = " + edgeImage.cardinality());
        return edgeImage;
    }

    /**
     * Calculates the zero crossing mask of a 2D image and returns it as a BitSet buffer.
     *
     * @param   buffer   the buffer to find the edges in
     * @param   extents  the extents of the image the buffer came from
     *
     * @return  BitSet buffer with mask set when a zero crossing is detected
     */
    public BitSet calcZeroXMaskBitset(float[] buffer, int[] extents) {
        setStartTime();
        makeKernels2D();

        float[] resultBuffer = new float[buffer.length];

        for (int i = 0; i < buffer.length; i++) {
            resultBuffer[i] = AlgorithmConvolver.convolve2DPt(i, extents, buffer, kExtents, GxxData);
        }

        computeElapsedTime();

        return genLevelMask(extents[0], extents[1], resultBuffer, 0, zeroDetectionType, loThres, hiThres);
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        GxxData = null;
        GyyData = null;
        GzzData = null;
        destImage = null;
        srcImage = null;
        zXMask = null;
        kExtents = null;
        sigmas = null;
        super.finalize();
    }

    /**
     * Generates a zero crossing mask for a 2D function. Sets a ModelImage to 255 if a zero crossing is detected.
     *
     * @param  slice          the slice of the volume which we are working on (0 if from 2D image)
     * @param  buffer         array in which to find zero crossing
     * @param  detectionType  the type of zero crossing detection to perform
     */
    public void genZeroXMask(int slice, float[] buffer, int detectionType) {
        int i0, i1, i2, i3;
        float x0, x1, x2, x3;
        int i, j;
        int indexY;
        int length;

        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];

        length = xDim * yDim;

        int xxDim = xDim - 1;
        int yyDim = yDim - 1;
        float level = 0;
        int offset = slice * length;

        for (j = 0; j < yyDim; j++) {
            indexY = j * xDim;

            for (i = 0; i < xxDim; i++) {
                i0 = indexY + i;
                i1 = i0 + 1;
                i2 = i0 + xDim;
                i3 = i0 + 1 + xDim;

                x0 = buffer[i0];
                x1 = buffer[i1];
                x2 = buffer[i2];
                x3 = buffer[i3];

                if (detectionType == MARCHING_SQUARES) {

                    if ((x0 >= level) && (x1 >= level) && (x2 >= level) && (x3 >= level)) { // case 0 - no edge
                    } else if ((x0 >= level) && (x1 >= level) && (x2 < level) && (x3 >= level)) {

                        // case 1 - edge in the lower left
                        zXMask.set(offset + i2, 255);
                    } else if ((x0 >= level) && (x1 >= level) && (x2 >= level) && (x3 < level)) {

                        // case 2 - edge in the lower right
                        zXMask.set(offset + i3, 255);
                    } else if ((x0 >= level) && (x1 >= level) && (x2 < level) && (x3 < level)) {

                        // case 3 - edge horizontally
                        zXMask.set(offset + i2, 255);
                        zXMask.set(offset + i3, 255);
                    } else if ((x0 >= level) && (x1 < level) && (x2 >= level) && (x3 >= level)) {

                        // case 4 - edge in the upper right
                        zXMask.set(offset + i1, 255);
                    } else if ((x0 >= level) && (x1 < level) && (x2 < level) && (x3 >= level)) {

                        // case 5 - ambiguous case; either edge in upper right and lower left or
                        // edge that goes from the upper right to the lower left
                        zXMask.set(offset + i1, 255);
                        zXMask.set(offset + i2, 255);
                    } else if ((x0 >= level) && (x1 < level) && (x2 >= level) && (x3 < level)) {

                        // case 6 - edge going vertically along the right
                        zXMask.set(offset + i1, 255);
                        zXMask.set(offset + i3, 255);
                    } else if ((x0 >= level) && (x1 < level) && (x2 < level) && (x3 < level)) {

                        // case 7 - edge in the upper left
                        zXMask.set(offset + i0, 255);
                    } else if ((x0 < level) && (x1 >= level) && (x2 >= level) && (x3 >= level)) {

                        // case 8 - edge in the upper left
                        zXMask.set(offset + i0, 255);
                    } else if ((x0 < level) && (x1 >= level) && (x2 < level) && (x3 >= level)) {

                        // case 9 - edge going vertially along the left
                        zXMask.set(offset + i0, 255);
                        zXMask.set(offset + i2, 255);
                    } else if ((x0 < level) && (x1 >= level) && (x2 >= level) && (x3 < level)) {

                        // case 10 - ambiguous case; either edge in upper left and lower right or
                        // edge that goes from the upper left to the lower right
                        zXMask.set(offset + i0, 255);
                        zXMask.set(offset + i3, 255);
                    } else if ((x0 < level) && (x1 >= level) && (x2 < level) && (x3 < level)) {

                        // case 11 - edge in the upper right
                        zXMask.set(offset + i1, 255);
                    } else if ((x0 < level) && (x1 < level) && (x2 >= level) && (x3 >= level)) {

                        // case 12 - edge going horizontally along the top
                        zXMask.set(offset + i0, 255);
                        zXMask.set(offset + i1, 255);
                    } else if ((x0 < level) && (x1 < level) && (x2 < level) && (x3 >= level)) {

                        // case 13 - edge in the lower right
                        zXMask.set(offset + i3, 255);
                    } else if ((x0 < level) && (x1 < level) && (x2 >= level) && (x3 < level)) {

                        // case 14 - edge in the lower left
                        zXMask.set(offset + i2, 255);
                    } else if ((x0 < level) && (x1 < level) && (x2 < level) && (x3 < level)) { // case 15 - no edge
                    }
                } else if (detectionType == NEGATIVE_EDGES) {

                    if (buffer[i0] <= 1) {
                        zXMask.set(offset + i0, 255);
                    }
                } else if (detectionType == OLD_DETECTION) {

                    if ((x0 > level) && (x1 > level) && (x2 > level) && (x3 > level)) {
                        zXMask.set(offset + i0, 0);
                    } else if ((x0 < level) && (x1 < level) && (x2 < level) && (x3 < level)) {
                        zXMask.set(offset + i0, 0);
                    } else {
                        zXMask.set(offset + i0, 255);
                    }
                }
            }
        }

        FileInfoBase[] fileInfo = zXMask.getFileInfo();

        fileInfo[slice].setModality(srcImage.getFileInfo()[slice].getModality());
        fileInfo[slice].setFileDirectory(srcImage.getFileInfo()[slice].getFileDirectory());
        fileInfo[slice].setEndianess(srcImage.getFileInfo()[slice].getEndianess());
        fileInfo[slice].setUnitsOfMeasure(srcImage.getFileInfo()[slice].getUnitsOfMeasure());
        fileInfo[slice].setResolutions(srcImage.getFileInfo()[slice].getResolutions());
        fileInfo[slice].setExtents(zXMask.getExtents());
        fileInfo[slice].setMax(255);
        fileInfo[slice].setMin(0);
        fileInfo[slice].setPixelPadValue(srcImage.getFileInfo()[slice].getPixelPadValue());
        fileInfo[slice].setPhotometric(srcImage.getFileInfo()[slice].getPhotometric());
    }

    /**
     * Accessor to return mask indicating zero crossings.
     *
     * @return  ModelImage of zero crossings (2D function); 255 = indicates zero crossing
     */
    public ModelImage getZeroXMask() {
        return zXMask;
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {
        int[] destExtents = null;

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

        try {

            if (srcImage.getNDims() == 2) {
                destExtents = new int[2];
                destExtents[0] = srcImage.getExtents()[0]; // X dim
                destExtents[1] = srcImage.getExtents()[1]; // Y dim
            } else if (srcImage.getNDims() == 3) {
                destExtents = new int[3];
                destExtents[0] = srcImage.getExtents()[0]; // X dim
                destExtents[1] = srcImage.getExtents()[1]; // Y dim
                destExtents[2] = srcImage.getExtents()[2]; // Z or T dim
            }

            zXMask = new ModelImage(ModelImage.UBYTE, destExtents, " Edges");
        } catch (OutOfMemoryError e) {
            destImage = null;
            srcImage = null;
            zXMask.disposeLocal();
            zXMask = null;
            errorCleanUp("Algorithm EdgeLap exportData: Out of memory", true);

            return;
        }

        if (destImage != null) { // NEW
            constructLog();

            if (srcImage.getNDims() == 2) {
                calcStoreInDest2D(1, zeroDetectionType);
            } else if ((srcImage.getNDims() == 3) && (image25D == false)) {
                calcStoreInDest3D(zeroDetectionType);
            } else if ((srcImage.getNDims() == 3) && (image25D == true)) {
                calcStoreInDest2D(srcImage.getExtents()[2], zeroDetectionType);
            }
        }

        if (threadStopped) {
            finalize();

            return;
        }
    }

    /**
     * Changes the type of zero crossing detection method used.
     *
     * @param  type  the zero crossing method to use
     */
    public void setZeroDetectionType(int type) {
        zeroDetectionType = type;
    }

    /**
     * This function produces the EdgeLap of input image.
     *
     * @param  nImages        number of images on which to find zero crossings. If 2D image then nImage = 1. If 3D image
     *                        where each image is to processed independently then nImages equals the number of images in
     *                        the volume.
     * @param  detectionType  the type of zero crossing detection to perform
     */
    private void calcStoreInDest2D(int nImages, int detectionType) {
        int i, s, idx;
        int length, totalLength;
        int start;
        float[] buffer;
        float lap;

        try {
            destImage.setLock();
        } catch (IOException error) {
            errorCleanUp("Algorithm Edge Lap: Image(s) locked", false);

            return;
        }

        try {
            length = srcImage.getSliceSize();
            totalLength = length * nImages;
            buffer = new float[length];
            // fireProgressStateChanged(srcImage.getImageName(), "Calculating the Edge ...");
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Edge Lap: Out of memory", true);

            return;
        }

        int mod = totalLength / 100; // mod is 1 percent of length

        // initProgressBar();
        fireProgressStateChanged(0, srcImage.getImageName(), "Calculating the Edge ...");

        for (s = 0; (s < nImages) && !threadStopped; s++) {
            start = s * length;

            try {
                srcImage.exportData(start, length, buffer); // locks and releases lock
            } catch (IOException error) {
                errorCleanUp("Algorithm EdgeLaplacian: Image(s) locked", false);

                return;
            }

            for (i = 0, idx = start; (i < length) && !threadStopped; i++, idx++) {

                if (((start + i) % mod) == 0) {
                    fireProgressStateChanged(Math.round((float) (start + i) / (totalLength - 1) * 100));
                }

                if ((entireImage == true) || mask.get(i)) {
                    lap = AlgorithmConvolver.convolve2DPt(i, srcImage.getExtents(), buffer, kExtents, GxxData);
                    destImage.set(idx, lap);
                } else {
                    destImage.set(idx, buffer[i]);
                }
            }

            try {
                destImage.exportDataNoLock(start, length, buffer);
            } catch (IOException error) {
                errorCleanUp("Algorithm EdgeLaplacian exportData: " + error, false);

                return;
            }

            genZeroXMask(s, buffer, detectionType);
        }

        fireProgressStateChanged(100);

        if (threadStopped) {
            return;
        }

        zXMask.calcMinMax();
        destImage.calcMinMax();
        destImage.releaseLock();

        setCompleted(true);
    }

    /**
     * This function produces the Laplacian of input image.
     *
     * @param  detectionType  the type of zero crossing detection to perform
     */
    private void calcStoreInDest3D(int detectionType) {
        int i, nImages, s;
        int length, totalLength;
        float[] buffer;
        int start;
        float[] sliceBuffer;
        float lap;

        try {
            destImage.setLock();
        } catch (IOException error) {
            errorCleanUp("Algorithm EdgeLaplacian: Image(s) locked", false);

            return;
        }

        try {
            length = srcImage.getSliceSize();
            totalLength = srcImage.getSliceSize() * srcImage.getExtents()[2];
            nImages = srcImage.getExtents()[2];
            buffer = new float[totalLength];
            sliceBuffer = new float[length];
            srcImage.exportData(0, totalLength, buffer); // locks and releases lock

            // fireProgressStateChanged(srcImage.getImageName(), "Calculating Zero X-ings ...");
        } catch (IOException error) {
            buffer = null;
            sliceBuffer = null;
            errorCleanUp("Algorithm EdgeLaplacian exportData: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            sliceBuffer = null;
            errorCleanUp("Algorithm EdgeLaplacian exportData: Out of memory", true);

            return;
        }

        // initProgressBar();
        fireProgressStateChanged(0, srcImage.getImageName(), "Calculating Zero X-ings ...");

        int mod = totalLength / 100; // mod is 1 percent of length

        for (s = 0; (s < nImages) && !threadStopped; s++) {
            start = s * length;

            for (i = start; (i < (start + length)) && !threadStopped; i++) {

                if ((i % mod) == 0) {
                    fireProgressStateChanged(Math.round((float) i / (totalLength - 1) * 100));
                }

                if ((entireImage == true) || mask.get(i)) {
                    lap = AlgorithmConvolver.convolve3DPt(i, srcImage.getExtents(), buffer, kExtents, GxxData);
                    destImage.set(i, lap);
                } else {
                    destImage.set(i, buffer[i]);
                }
            }

            try {
                destImage.exportDataNoLock(start, length, sliceBuffer);
            } catch (IOException error) {
                buffer = null;
                sliceBuffer = null;
                errorCleanUp("Algorithm EdgeLaplacian exportData: " + error, true);

                return;
            }

            genZeroXMask(s, sliceBuffer, detectionType);
        }

        fireProgressStateChanged(100);

        if (threadStopped) {
            return;
        }

        zXMask.calcMinMax();
        destImage.calcMinMax();
        destImage.releaseLock();

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

        historyString = new String("EdgeLap(" + sigmaStr + String.valueOf(entireImage) + ", " +
                                   String.valueOf(image25D) + ", " + loThres + ", " + hiThres + ")\n");

        if (Preferences.is(Preferences.PREF_LOG) && (zXMask.getHistoryArea() != null)) {
            zXMask.getHistoryArea().append(historyString);
        }
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

        GxxData = new float[xkDim * ykDim];

        GenerateGaussian Gxx = new GenerateGaussian(GxxData, kExtents, sigmas, derivOrder);

        Gxx.calc(false);
        Gxx.finalize();
        Gxx = null;

        derivOrder[0] = 0;
        derivOrder[1] = 2;
        GyyData = new float[xkDim * ykDim];

        GenerateGaussian Gyy = new GenerateGaussian(GyyData, kExtents, sigmas, derivOrder);

        Gyy.calc(false);
        Gyy.finalize();
        Gyy = null;

        for (int i = 0; i < GyyData.length; i++) {
            GxxData[i] = -(GxxData[i] + GyyData[i]);
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

        GxxData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gxx = new GenerateGaussian(GxxData, kExtents, sigmas, derivOrder);

        Gxx.calc(false);
        Gxx.finalize();
        Gxx = null;

        derivOrder[0] = 0;
        derivOrder[1] = 2;
        derivOrder[2] = 0;
        GyyData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gyy = new GenerateGaussian(GyyData, kExtents, sigmas, derivOrder);

        Gyy.calc(false);
        Gyy.finalize();
        Gyy = null;

        derivOrder[0] = 0;
        derivOrder[1] = 0;
        derivOrder[2] = 2;
        GzzData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gzz = new GenerateGaussian(GzzData, kExtents, sigmas, derivOrder);

        Gzz.calc(false);
        Gzz.finalize();
        Gzz = null;

        for (int i = 0; i < GyyData.length; i++) {
            GxxData[i] = -(GxxData[i] + GyyData[i] + GzzData[i]);
        }
    }
}
