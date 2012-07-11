package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.Dimension;
import java.io.IOException;
import java.util.*;

import WildMagic.LibFoundation.Mathematics.*;


/**
 * Three-Dimensional mathmatical morphology class. Methods include:
 * 
 * <ul>
 * <li>Background Distance Map</li>
 * <li>close</li>
 * <li>Delete objects</li>
 * <li>dilate</li>
 * <li>Distance Map</li>
 * <li>erode</li>
 * <li>fill holes</li>
 * <li>find edges</li>
 * <li>Identify objects</li>
 * <li>Morphological gradient</li>
 * <li>open</li>
 * <li>Particle Analysis</li>
 * <li>Skeletonize with pruning option</li>
 * <li>ultimate erode</li>
 * </ul>
 * 
 * @version 1.0 Aug 1999
 * @author Matthew J. McAuliffe, Ph.D.
 */
public class AlgorithmMorphology3D extends AlgorithmBase {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public static final int ERODE = 0; // algorithm types

    /** DOCUMENT ME! */
    public static final int DILATE = 1;

    /** DOCUMENT ME! */
    public static final int CLOSE = 2;

    /** DOCUMENT ME! */
    public static final int OPEN = 3;

    /** DOCUMENT ME! */
    public static final int ID_OBJECTS = 4;

    /** DOCUMENT ME! */
    public static final int DELETE_OBJECTS = 5;

    /** DOCUMENT ME! */
    public static final int DISTANCE_MAP = 6;

    /** DOCUMENT ME! */
    public static final int BG_DISTANCE_MAP = 7;

    /** DOCUMENT ME! */
    public static final int ULTIMATE_ERODE = 8;

    /** DOCUMENT ME! */
    public static final int PARTICLE_ANALYSIS = 9;

    /** DOCUMENT ME! */
    public static final int SKELETONIZE = 10;

    /** DOCUMENT ME! */
    public static final int FIND_EDGES = 11;

    /** DOCUMENT ME! */
    public static final int FILL_HOLES = 12;

    /** DOCUMENT ME! */
    public static final int DISTANCE_MAP_FOR_SHAPE_INTERPOLATION = 13;

    public static final int MORPHOLOGICAL_GRADIENT = 14;

    /** DOCUMENT ME! */
    public static final int SIZED_SPHERE = 0;

    /** DOCUMENT ME! */
    public static final int CONNECTED6 = 1;

    /** DOCUMENT ME! */
    public static final int CONNECTED24 = 2;

    public static final int CONNECTED26 = 3;

    /**
     * Parameter used in Find_Edges. Indicates that the edge extracted is formed from the voxels that are directly
     * adjacient to the ojbect.
     */
    public static final int OUTER_EDGING = 0;

    /**
     * Parameter used in Find_Edges. Indicates that the edge extracted is formed from the voxels that overlay the edge
     * voxels of the object.
     */
    public static final int INNER_EDGING = 1;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** algorithm type (i.e. erode, dilate) */
    private int algorithm;

    /** DOCUMENT ME! */
    private float[] distanceMap = null;

    /** Edge type. */
    private final int edgingType;

    /** if true, indicates that the VOIs should NOT be used and that entire image should be processed. */
    private boolean entireImage;

    /** imgBuffer that hold voxel value for the 3D slices. */
    private short[] imgBuffer;

    private short[] imgBuffer2;

    /** Dilation iteration times. */
    private final int iterationsD;

    /** Erosion iteration times. */
    private final int iterationsE;

    /** Kernel dimension. */
    private int kDimXY;

    /** DOCUMENT ME! */
    private int kDimZ;

    /** Kernel for both erosion and dilation. */
    private BitSet kernel;

    /** kernel size (i.e. connectedness) */
    @SuppressWarnings("unused")
    private final int kernelType;

    /** maximum, minimum size of objects. */
    private int min = 1, max = 20000000;

    /** Number pixels to prune. */
    private final int numPruningPixels;

    /** Vector that holding the current available objects in the 3D image. */
    private Vector<intObject> objects = new Vector<intObject>();

    /** DOCUMENT ME! */
    private float pixDist = 0;

    /** intermediate processing buffer, same size with imgBuffer. */
    private short[] processBuffer;

    /** Not used now. Flag to show frame during each algorithm method call. */
    private final boolean showFrame = false;

    /** kernel diameter. */
    private float sphereDiameter;

    /** DOCUMENT ME! */
    private Vector3f[] ultErodeObjects = null;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmMorphology3D object.
     * 
     * @param srcImg source image model
     * @param kernelType kernel size (i.e. connectedness)
     * @param sphereDiameter only valid if kernelType == SIZED_SPHERE and represents the width of a circle in the
     *            resolution of the image
     * @param method setup the algorithm method (i.e. erode)
     * @param iterD number of times to dilate
     * @param iterE number of times to erode
     * @param pruningPix the number of pixels to prune
     * @param edType the type of edging to perform (inner or outer)
     * @param entireImage flag that indicates if the VOIs should NOT be used and entire image should be processed
     */
    public AlgorithmMorphology3D(final ModelImage srcImg, final int kernelType, final float sphereDiameter,
            final int method, final int iterD, final int iterE, final int pruningPix, final int edType,
            final boolean entireImage) {
        this(srcImg, null, kernelType, sphereDiameter, method, iterD, iterE, pruningPix, edType, entireImage);
    }

    /**
     * Creates a new AlgorithmMorphology3D object.
     * 
     * @param srcImg source image model
     * @param destImg destination image model
     * @param kernelType kernel size (i.e. connectedness)
     * @param sphereDiameter only valid if kernelType == SIZED_SPHERE and represents the width of a circle in the
     *            resolution of the image
     * @param method setup the algorithm method (i.e. erode)
     * @param iterD number of times to dilate
     * @param iterE number of times to erode
     * @param pruningPix the number of pixels to prune
     * @param edType the type of edging to perform (inner or outer)
     * @param entireImage flag that indicates if the VOIs should NOT be used and entire image should be processed
     */
    public AlgorithmMorphology3D(final ModelImage srcImg, final ModelImage destImg, final int kernelType,
            final float sphereDiameter, final int method, final int iterD, final int iterE, final int pruningPix,
            final int edType, final boolean entireImage) {
        super(destImg, srcImg);
        makeKernel(kernelType);
        setAlgorithm(method);
        iterationsD = iterD;
        iterationsE = iterE;
        numPruningPixels = pruningPix;
        edgingType = edType;
        this.entireImage = entireImage;
        this.kernelType = kernelType;

        if (kernelType == AlgorithmMorphology3D.SIZED_SPHERE) {
            this.sphereDiameter = sphereDiameter;
            makeSphericalKernel();
        } else {
            makeKernel(kernelType);
        }
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        kernel = null;
        processBuffer = null;
        imgBuffer = null;
        srcImage = null;

        if (objects != null) {
            objects.removeAllElements();
            objects = null;
        }

        super.finalize();
    }

    /**
     * Finds the edges of objects.
     * 
     * @param edgeType Indicates that the edge extracted is formed from the voxels that are directly adjacient to the
     *            ojbect or from the voxels that overlay the edge voxels of the object.
     * @param returnFlag if true then this operation is a step in the morph process (i.e. close) see OUTER_EDGING see
     *            INNER_EDGE
     */
    public void findEdges(final int edgeType, final boolean returnFlag) {

        // if thread has already been stopped, dump out
        if (threadStopped) {
            setCompleted(false);

            finalize();

            return;
        }

        boolean clear;
        int i, j, pix;
        int startX, startY;
        int endX, endY;
        short[] tempBuffer;
        short value;

        final int xDim = srcImage.getExtents()[0];
        final int yDim = srcImage.getExtents()[1];
        final int zDim = srcImage.getExtents()[2];

        final int sliceSize = xDim * yDim;
        final int imgSize = sliceSize * zDim;

        fireProgressStateChanged(0, srcImage.getImageName(), "Finding Edges ...");

        try {
            tempBuffer = new short[imgBuffer.length];
        } catch (final OutOfMemoryError e) {
            displayError("Algorithm Morphology3D: Out of memory");
            setCompleted(false);

            return;
        }

        for (pix = 0; pix < imgSize; pix++) {
            processBuffer[pix] = 0;
            tempBuffer[pix] = 0;
        }

        final float pixelsPerProgressValue = 100f / imgSize;

        if (edgingType == AlgorithmMorphology3D.OUTER_EDGING) {

            // dilate image and then XOR with orignal image
            for (int s = 0; (s < (zDim * sliceSize)) && !threadStopped; s += sliceSize) {

                if ( (s % pixelsPerProgressValue) == 0) {
                    fireProgressStateChanged(MipavMath.round( (s / pixelsPerProgressValue)));
                }

                try {
                    fireProgressStateChanged(Math.round( ((float) s) / ((float) (imgSize - 1)) * 100));
                } catch (final NullPointerException npe) {

                    if (threadStopped) {
                        Preferences
                                .debug(
                                        "somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                        Preferences.DEBUG_ALGORITHM);
                    }
                }

                for (pix = s; pix < (s + sliceSize); pix++) {

                    if (entireImage || mask.get(pix)) {
                        value = imgBuffer[pix];

                        if (value != 0) {
                            startY = pix - xDim - 1;
                            endY = pix + xDim - 1;

                            for (j = startY; j <= endY; j += xDim) {
                                startX = j;
                                endX = startX + 3;

                                for (i = startX; i < endX; i++) {

                                    if ( (i >= 0) && (i < (s + sliceSize))) {
                                        processBuffer[i] = value;
                                    }
                                }
                            }
                        }
                    } else {
                        tempBuffer[pix] = imgBuffer[pix];
                    }
                }
            }

            for (pix = 0; (pix < imgSize) && !threadStopped; pix++) {

                if (entireImage || mask.get(pix)) {

                    if ( (imgBuffer[pix] == 0) && (processBuffer[pix] != 0)) {
                        tempBuffer[pix] = processBuffer[pix];
                    }
                }
            }
        } else {

            for (int s = 0; (s < (zDim * sliceSize)) && !threadStopped; s += sliceSize) {

                // try {
                //
                // if (isProgressBarVisible()) { fireProgressStateChanged(Math.round(((float) s) /
                // ((float) (imgSize - 1)) * 100)); } } catch (NullPointerException npe) {

                // if (threadStopped) { Preferences.debug("somehow you managed
                // to cancel the algorithm and dispose the progressbar between checking for threadStopping and using
                // it.", Preferences.DEBUG_ALGORITHM); }
                // }

                for (pix = s; pix < (s + sliceSize); pix++) {

                    if (entireImage || mask.get(pix)) {
                        value = imgBuffer[pix];

                        if (value != 0) {
                            startY = pix - xDim - 1;
                            endY = pix + xDim - 1;
                            clear = false;

                            for (j = startY; j <= endY; j += xDim) {
                                startX = j;
                                endX = startX + 3;

                                for (i = startX; i < endX; i++) {

                                    if ( (i >= 0) && (i < (s + sliceSize))) {

                                        if (imgBuffer[i] == 0) {
                                            clear = true;
                                        }
                                    }
                                }
                            }

                            if (clear) {
                                processBuffer[pix] = 0;
                            } else {
                                processBuffer[pix] = value;
                            }
                        }
                    } else {
                        tempBuffer[pix] = imgBuffer[pix];
                    }
                }
            }

            for (pix = 0; (pix < imgSize) && !threadStopped; pix++) {

                if (entireImage || mask.get(pix)) {

                    if ( (imgBuffer[pix] != 0) && (processBuffer[pix] == 0)) {
                        tempBuffer[pix] = imgBuffer[pix];
                    }
                }
            }
        }

        imgBuffer = tempBuffer;

        if (returnFlag == true) {
            return;
        }

        try {

            if (threadStopped) {
                setCompleted(false);

                finalize();

                return;
            }

            srcImage.importData(0, imgBuffer, true);
        } catch (final IOException error) {
            displayError("Algorithm Morphology3D: Image(s) locked");
            setCompleted(false);

            return;
        }

        setCompleted(true);

    }

    /**
     * Labels each object in an image with a different integer value.
     * 
     * @param returnFlag if true then this operation is a step in the morph process (i.e. close)
     */
    public void identifyObjects(final boolean returnFlag) {

        // if thread has already been stopped, dump out
        if (threadStopped) {
            setCompleted(false);

            finalize();

            return;
        }

        int pix;
        final int xDim = srcImage.getExtents()[0];
        final int yDim = srcImage.getExtents()[1];
        final int zDim = srcImage.getExtents()[2];
        final int imageLength = xDim * yDim * zDim;
        short floodValue = 1;
        int count = 0;

        final int[] progressValues = getProgressValues();

        this.setMaxProgressValue(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 95));
        deleteObjects(min, max, true);

        this.setProgressValues(progressValues);

        fireProgressStateChanged("Identifying objects ...");

        final int mod = imageLength / 50;

        // objects.removeAllElements();
        for (pix = 0; (pix < imageLength) && !threadStopped; pix++) {

            if ( ( (pix % mod) == 0)) {
                fireProgressStateChanged(95 + Math.round( (pix + 1) / ((float) imageLength) * 5));
            }

            if (entireImage == true) {

                if (imgBuffer[pix] > 0) {
                    count = floodFill(processBuffer, pix, floodValue, imgBuffer[pix]);
                    objects.addElement(new intObject(pix, floodValue, count));
                    floodValue++;
                }
            } else if (mask.get(pix) == true) {

                if (imgBuffer[pix] > 0) {
                    count = floodFill(processBuffer, pix, floodValue, imgBuffer[pix]);
                    objects.addElement(new intObject(pix, floodValue, count));
                    floodValue++;
                }
            }
        }

        if (threadStopped) {
            setCompleted(false);
            finalize();

            return;
        }

        String mStr;
        float vol;

        mStr = srcImage.getFileInfo(0).getVolumeUnitsOfMeasureStr();
        ViewUserInterface.getReference().getMessageFrame().getData().append(
                " Object \t# of pixels\tVolume(" + mStr + ")\n");

        for (int i = 0; i < objects.size(); i++) {
            vol = ((intObject) (objects.elementAt(i))).size * srcImage.getFileInfo(0).getResolutions()[0]
                    * srcImage.getFileInfo(0).getResolutions()[1] * srcImage.getFileInfo(0).getResolutions()[2];

            // UI.setDataText(
            ViewUserInterface.getReference().getMessageFrame().getData().append(
                    "    " + (i + 1) + "\t" + + ((intObject) (objects.elementAt(i))).size + "\t" + vol + "\n");
        }

        if (returnFlag == true) {
            return;
        }

        try {

            if (threadStopped) {
                setCompleted(false);

                finalize();

                return;
            }

            if ( (srcImage.getType() == ModelStorageBase.BOOLEAN) || (srcImage.getType() == ModelStorageBase.BYTE)
                    || (srcImage.getType() == ModelStorageBase.UBYTE) || (srcImage.getType() == ModelStorageBase.SHORT)) {
                srcImage.reallocate(ModelStorageBase.USHORT);
            }

            srcImage.importData(0, processBuffer, true);
        } catch (final IOException error) {
            displayError("Algorithm Morphology3D: Image(s) locked");
            setCompleted(false);

            return;
        } catch (final OutOfMemoryError e) {
            displayError("Algorithm Morphology3D: Out of memory");
            setCompleted(false);

            return;
        }

        setCompleted(true);
    }

    /**
     * This method is to be applied on skeletonized images: it removes all branches which are iter or less pixels in
     * length.
     * 
     * @param iter the threshold number of pixels for the maximum length of a removed branch to be
     * @param returnFlag if true then this operation is a step in the morph process (i.e. close)
     */
    public void prune(final int iter, final boolean returnFlag) {

        // if thread has already been stopped, dump out
        if (threadStopped) {
            return;
        }

        short p1, p2, p3, p4, p5, p6, p7, p8, p9;
        final short bgColor = 0;
        short value;
        int i, slice, pix;
        final int xDim = srcImage.getExtents()[0];
        final int yDim = srcImage.getExtents()[1];
        final int zDim = srcImage.getExtents()[2];
        final int sliceSize = xDim * yDim;
        Vector<Integer> endpoints;
        Vector<Integer> branch;
        Vector<Vector<Integer>> branchesVector;
        short[] tempBuffer;

        if (iter < 1) {
            return;
        }

        try {
            tempBuffer = new short[imgBuffer.length];
            endpoints = new Vector<Integer>();
            branchesVector = new Vector<Vector<Integer>>();
        } catch (final OutOfMemoryError e) {
            displayError("Algorithm Morphology3D: Out of memory");
            setCompleted(false);

            return;
        }

        /*
         * try {
         * 
         * if { progressBar.setMessage("Pruning image ..."); }
         * 
         * if { fireProgressStateChanged(0); } } catch (NullPointerException npe) {
         * 
         * if (threadStopped) { Preferences.debug("somehow you managed to cancel the algorithm and dispose the
         * progressbar between checking for threadStopping and using it.", Preferences.DEBUG_ALGORITHM); } }
         */
        for (slice = 0; slice < zDim; slice++) {

            /*
             * try { progressBar.setMessage("Pruning Slice " + (slice + 1));
             * fireProgressStateChanged(Math.round(((float) slice) / ((float) zDim) * 100)); } catch
             * (NullPointerException npe) {
             * 
             * if (threadStopped) { Preferences.debug("somehow you managed to cancel the algorithm and dispose the
             * progressbar between checking for threadStopping and using it.", Preferences.DEBUG_ALGORITHM); } }
             */
            // sets the intensity of border points to 0
            for (pix = (slice * sliceSize); pix < ( (slice * sliceSize) + xDim); pix++) {
                imgBuffer[pix] = 0;
            }

            for (pix = ( (slice * sliceSize) + (xDim * (yDim - 1))); pix < ( (slice + 1) * sliceSize); pix++) {
                imgBuffer[pix] = 0;
            }

            for (pix = (slice * sliceSize); pix <= ( (slice * sliceSize) + (xDim * (yDim - 1))); pix += xDim) {
                imgBuffer[pix] = 0;
            }

            for (pix = ( (slice * sliceSize) + (xDim - 1)); pix < ( (slice + 1) * sliceSize); pix += xDim) {
                imgBuffer[pix] = 0;
            }

            for (pix = (slice * sliceSize); pix < ( (slice + 1) * sliceSize); pix++) {
                processBuffer[pix] = 0;
                tempBuffer[pix] = imgBuffer[pix];

                if (imgBuffer[pix] != bgColor) {
                    tempBuffer[pix] = -1; // used to represent non-zero pixels which are deeper than iter + 1 pixels
                    // from an endpoint
                }
            }

            // store in processBuffer a an image in which each branch's pixels
            // has intensities according to how deep within the branch they are
            // (goes (iter + 1) deep only)
            for (i = 1; i <= (iter + 1); i++) {

                for (int y = 1; y < (yDim - 1); y++) {

                    for (int x = 1; x < (xDim - 1); x++) {
                        pix = (slice * sliceSize) + (y * xDim) + x;

                        if (entireImage || mask.get(pix)) {
                            p5 = tempBuffer[pix];
                            value = p5;

                            if (p5 != bgColor) {

                                if (isEndpoint(pix, tempBuffer)) {
                                    value = (byte) i;
                                    endpoints.addElement(new Integer(pix));
                                }

                                processBuffer[pix] = value;
                            }
                        }
                    }
                }

                while ( !endpoints.isEmpty()) {
                    tempBuffer[ ((Integer) (endpoints.firstElement())).intValue()] = bgColor;
                    endpoints.removeElementAt(0);
                }
            }

            endpoints.removeAllElements();

            // for each occurrence of 1 (the endpoint of a branch), a new element is added to branchesVector
            for (int y = 1; y < (yDim - 1); y++) {

                for (int x = 1; x < (xDim - 1); x++) {
                    pix = (slice * sliceSize) + (y * xDim) + x;

                    if (entireImage || mask.get(pix)) {
                        p5 = processBuffer[pix];

                        if (p5 == 1) {
                            branch = new Vector<Integer>();
                            branch.addElement(new Integer(pix));
                            branchesVector.addElement(branch);
                        }
                    }
                }
            }

            // goes through all branches (by iterating through branchesVector), and removes all branches from
            // the vector which have a length of iter or less
            int currentBranch = 0;
            int branchesDone = 0;
            final int branchesSize = branchesVector.size();

            while (branchesDone != branchesSize) {
                branch = (Vector<Integer>) branchesVector.elementAt(currentBranch);

                for (i = 2; i <= (iter + 1); i++) {
                    pix = ((Integer) branch.elementAt(i - 2)).intValue();
                    p1 = processBuffer[pix - xDim - 1];
                    p2 = processBuffer[pix - xDim];
                    p3 = processBuffer[pix - xDim + 1];
                    p4 = processBuffer[pix - 1];
                    p6 = processBuffer[pix + 1];
                    p7 = processBuffer[pix + xDim - 1];
                    p8 = processBuffer[pix + xDim];
                    p9 = processBuffer[pix + xDim + 1];

                    if (p1 == (byte) i) {
                        branch.addElement(new Integer(pix - xDim - 1));
                    } else if (p2 == (byte) i) {
                        branch.addElement(new Integer(pix - xDim));
                    } else if (p3 == (byte) i) {
                        branch.addElement(new Integer(pix - xDim + 1));
                    } else if (p4 == (byte) i) {
                        branch.addElement(new Integer(pix - 1));
                    } else if (p6 == (byte) i) {
                        branch.addElement(new Integer(pix + 1));
                    } else if (p7 == (byte) i) {
                        branch.addElement(new Integer(pix + xDim - 1));
                    } else if (p8 == (byte) i) {
                        branch.addElement(new Integer(pix + xDim));
                    } else if (p9 == (byte) i) {
                        branch.addElement(new Integer(pix + xDim + 1));
                    } else {
                        break;
                    }
                }

                if (i < (iter + 1)) {
                    branchesVector.removeElementAt(currentBranch);
                } else {
                    currentBranch++;
                }

                branchesDone++;
            }

            for (pix = (slice * sliceSize); pix < ( (slice + 1) * sliceSize); pix++) {

                if (processBuffer[pix] == -1) {
                    tempBuffer[pix] = imgBuffer[pix];
                } else {
                    tempBuffer[pix] = 0;
                }
            }

            // creates final image which contains only branches of more than iter in length
            while ( !branchesVector.isEmpty()) {
                branch = (Vector<Integer>) branchesVector.firstElement();
                branchesVector.removeElementAt(0);

                while ( !branch.isEmpty()) {
                    pix = ((Integer) branch.firstElement()).intValue();
                    branch.removeElementAt(0);
                    tempBuffer[pix] = imgBuffer[pix];
                }
            }

            for (pix = (slice * sliceSize); pix < ( (slice + 1) * sliceSize); pix++) {
                imgBuffer[pix] = tempBuffer[pix];
            }

        }

        if (returnFlag == true) {
            return;
        }

        try {

            if (threadStopped) {
                setCompleted(false);

                finalize();

                return;
            }

            srcImage.importData(0, imgBuffer, true);
        } catch (final IOException error) {
            displayError("Algorithm Morphology3D: Image(s) locked");
            setCompleted(false);

            return;
        }

        setCompleted(true);
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");
            setCompleted(false);

            return;
        }

        srcImage.calcMinMax();
        final double minValue = srcImage.getMin();
        if (minValue < 0) {
            displayError("Source Image cannot have negative minimum");
            setCompleted(false);
            return;
        }
        if ( (srcImage.getType() != ModelStorageBase.BOOLEAN) && (srcImage.getType() != ModelStorageBase.BYTE)
                && (srcImage.getType() != ModelStorageBase.UBYTE) && (srcImage.getType() != ModelStorageBase.SHORT)
                && (srcImage.getType() != ModelStorageBase.USHORT)) {
            displayError("Source Image must be Boolean, Byte, UByte, Short or UShort");
            setCompleted(false);

            return;
        }

        if (srcImage.getNDims() != 3) {
            displayError("Source Image is not 3D");
            setCompleted(false);

            return;
        }

        try {
            final int length = srcImage.getSliceSize() * srcImage.getExtents()[2];

            imgBuffer = new short[length];

            if (algorithm != AlgorithmMorphology3D.FILL_HOLES) {
                processBuffer = new short[length];
            }

            srcImage.exportData(0, length, imgBuffer); // locks and releases lock

            // fireProgressStateChanged(srcImage.getImageName(), "Morph 3D ...");
        } catch (final IOException error) {
            displayError("Algorithm Morphology3D: Image(s) locked");
            setCompleted(false);

            return;
        } catch (final OutOfMemoryError e) {
            displayError("Algorithm Morphology3D: Out of memory");
            setCompleted(false);

            return;
        }

        // initProgressBar();

        int[] progressValues; // used to temporarily store the max progress value (for algorithms doing multiple
        // functions)

        switch (algorithm) {

            case ERODE:
                erode(false);
                break;

            case DILATE:
                dilate(false);
                break;

            case CLOSE:

                progressValues = getProgressValues();
                setMaxProgressValue(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 50));
                dilate(true);

                setProgressValues(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 50),
                        ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 100));
                erode(false);
                break;

            case OPEN:
                progressValues = getProgressValues();
                setMaxProgressValue(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 50));
                erode(true);

                setProgressValues(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 50),
                        ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 100));
                dilate(false);
                break;

            case MORPHOLOGICAL_GRADIENT:
                progressValues = getProgressValues();
                setMaxProgressValue(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 50));
                dilate(true);
                imgBuffer2 = new short[imgBuffer.length];
                System.arraycopy(imgBuffer, 0, imgBuffer2, 0, imgBuffer.length);
                try {
                    srcImage.exportData(0, imgBuffer.length, imgBuffer); // locks and releases lock
                } catch (final IOException error) {
                    displayError("Algorithm GrayScaleMorphology2D: Image(s) locked");
                    setCompleted(false);

                    return;
                }
                setProgressValues(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 50),
                        ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 100));
                erode(true);
                for (int i = 0; i < imgBuffer.length; i++) {
                    imgBuffer2[i] -= imgBuffer[i];
                }
                try {
                    srcImage.importData(0, imgBuffer2, true);
                } catch (final IOException error) {
                    displayError("Algorithm GrayScaleMorphology2D: Image(s) locked");
                    setCompleted(false);

                    return;
                }

                setCompleted(true);
                break;

            case FILL_HOLES:
                fillHoles(false);
                break;

            case DISTANCE_MAP_FOR_SHAPE_INTERPOLATION:
                distanceMapForShapeInterpolation(false);
                break;

            case ID_OBJECTS:
                identifyObjects(false);
                break;

            case DELETE_OBJECTS:
                deleteObjects(min, max, false);
                break;

            case DISTANCE_MAP:
                distanceMap(false);
                break;

            case BG_DISTANCE_MAP:
                backgroundDistanceMap(false);
                break;

            case ULTIMATE_ERODE:
                ultimateErode(false);
                break;

            case PARTICLE_ANALYSIS:
                particleAnalysis();
                break;

            case SKELETONIZE:
                skeletonize(numPruningPixels, false);
                break;

            case FIND_EDGES:
                findEdges(edgingType, false);
                break;

            default:
                break;
        }
    }

    /**
     * Sets the algorithm type (i.e. erode, dilate)
     * 
     * @param method algorithm type
     */
    public void setAlgorithm(final int method) {
        this.algorithm = method;
    }

    /**
     * Sets new source image.
     * 
     * @param img image model
     */
    public void setImage(final ModelImage img) {
        srcImage = img;
    }

    /**
     * Sets user defined square kernels.
     * 
     * @param kernel user defined kernel
     * @param dimXY length of x and y dimensions
     * @param dimZ length of z dimension
     */
    public void setKernel(final BitSet kernel, final int dimXY, final int dimZ) {
        this.kernel = kernel;
        kDimXY = dimXY;
        kDimZ = dimZ;
    }

    /**
     * Sets the bounds of object size used to delete objects.
     * 
     * @param _min minimum size of objects
     * @param _max maximum size of objects
     */
    public void setMinMax(final int _min, final int _max) {
        min = _min;
        max = _max;
    }

    /**
     * Used in the ultimate erode function to remove all eroded points less than the distance specified in pixel units.
     * 
     * @param dist distance in pixels.
     */
    public void setPixDistance(final float dist) {
        pixDist = dist;
    }

    /**
     * Display kernel as 1 or 0 in the debug window.
     */
    public void showKernel() {
        int x, y, z;

        try {
            String str = new String();

            Preferences.debug("\n Morphology3D - structuring element. \n", Preferences.DEBUG_ALGORITHM);

            for (z = 0; z < kDimZ; z++) {

                for (y = 0; y < kDimXY; y++) {

                    for (x = 0; x < kDimXY; x++) {

                        if (kernel.get( (z * kDimXY * kDimXY) + (y * kDimXY) + x) == true) {
                            str = str + "1   ";
                        } else {
                            str = str + "0   ";
                        }
                    }

                    Preferences.debug(str + "\n", Preferences.DEBUG_ALGORITHM);
                    str = new String();
                }

                Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
            }
        } catch (final OutOfMemoryError e) {
            displayError("Algorithm Morphology3D: Out of memory");
            setCompleted(false);

            return;
        }
    }

    /**
     * Skeletonizes the image by using a lookup table to repeatedly remove pixels from the edges of objects in a binary
     * image, reducing them to single pixel wide skeletons. Based on a thinning algorithm by Zhang and Suen. There is an
     * entry in the table for each of the 256 possible 3x3 neighborhood configurations. An entry of '1' signifies to
     * delete the indicated pixel on the first pass, '2' means to delete the indicated pixel on the second pass, and '3'
     * indicates to delete the pixel on either pass.
     * 
     * @param pruningPixels the number of pixels to prune after skeletonizing. (should be 0 if no pruning is to be done)
     * @param returnFlag if true then this operation is a step in the morph process (i.e. close)
     */
    public void skeletonize(final int pruningPixels, final boolean returnFlag) {

        // if thread has already been stopped, dump out
        if (threadStopped) {
            setCompleted(false);

            finalize();

            return;
        }

        final int[] table = // 0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1
        {0, 0, 0, 1, 0, 0, 1, 3, 0, 0, 3, 1, 1, 0, 1, 3, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 2, 0, 3, 0, 3, 3, 0, 0, 0, 0, 0,
                0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 3, 0, 2, 2, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 2,
                0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 3, 0, 2, 0, 0, 1, 3, 1, 0, 0, 1, 3, 0, 0, 0,
                0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 3, 1, 3, 0, 0, 1, 3, 0, 0, 0, 0, 0, 0, 0,
                1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 3, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 3,
                3, 0, 1, 0, 0, 0, 0, 2, 2, 0, 0, 2, 0, 0, 0};

        int pass = 0;
        int pixelsRemoved;
        int pix;
        final int xDim = srcImage.getExtents()[0];
        final int yDim = srcImage.getExtents()[1];
        final int zDim = srcImage.getExtents()[2];
        final int imageLength = xDim * yDim * zDim;

        short[] tempBuffer;

        for (pix = 0; pix < imageLength; pix++) {
            processBuffer[pix] = 0;
        }

        for (int i = 0; (i < zDim) && !threadStopped; i++) {

            fireProgressStateChanged("Skeletonizing Slice " + (i + 1));
            fireProgressStateChanged(Math.round( ((float) i) / ((float) zDim) * 100));

            do {
                pixelsRemoved = thin(pass++, i, table);
                tempBuffer = imgBuffer;
                imgBuffer = processBuffer;
                processBuffer = tempBuffer;

                pixelsRemoved = thin(pass++, i, table);
                tempBuffer = imgBuffer;
                imgBuffer = processBuffer;
                processBuffer = tempBuffer;
            } while (pixelsRemoved > 0);
        }

        if (pruningPixels > 0) {
            prune(pruningPixels, true);
        }

        if (returnFlag == true) {
            return;
        }

        try {

            if (threadStopped) {
                setCompleted(false);

                finalize();

                return;
            }

            srcImage.importData(0, imgBuffer, true);
        } catch (final IOException error) {
            displayError("Algorithm Morphology3D: Image(s) locked");
            setCompleted(false);

            return;
        }

        setCompleted(true);
    }

    private void distanceMapForShapeInterpolation(final boolean returnFlag) {

        // if thread has already been stopped, dump out
        if (threadStopped) {
            setCompleted(false);

            finalize();

            return;
        }

        int pix, i;
        final int xDim = srcImage.getExtents()[0];
        final int yDim = srcImage.getExtents()[1];
        final int zDim = srcImage.getExtents()[2];
        final int sliceSize = xDim * yDim;
        final int volSize = xDim * yDim * zDim;
        float distance;
        float dist;
        Point3D pt;
        int diffx, diffy, diffz;
        int x, y, z;

        float[] minDistanceBuffer;
        final Vector<Point3D> edgePoints = new Vector<Point3D>();

        fireProgressStateChanged("Bg. distance image ...");
        fireProgressStateChanged(0);

        try {
            minDistanceBuffer = new float[volSize];
        } catch (final OutOfMemoryError e) {
            displayError("Algorithm Morphology3D.distanceMap: Out of memory");
            setCompleted(false);

            return;
        }

        // Save original in processBuffer and invert image in imgBuffer
        for (pix = 0; pix < volSize; pix++) {
            processBuffer[pix] = imgBuffer[pix];
            if (imgBuffer[pix] > 0) {
                imgBuffer[pix] = 0;
            } else {
                imgBuffer[pix] = 1;
            }
        }

        // Find all edge pixels and put the index of their location in a integer vector
        final int end = volSize - sliceSize - xDim - 1;

        for (pix = sliceSize + xDim + 1; (pix < end) && !threadStopped; pix++) {

            if ( (imgBuffer[pix] == 0)
                    && ( (imgBuffer[pix - xDim] != 0) || (imgBuffer[pix + 1] != 0) || (imgBuffer[pix + xDim] != 0)
                            || (imgBuffer[pix - 1] != 0) || (imgBuffer[pix - sliceSize] != 0) || (imgBuffer[pix
                            + sliceSize] != 0))) {
                edgePoints.addElement(new Point3D(pix % xDim, (pix % sliceSize) / xDim, pix / sliceSize));
            }
        }

        if (threadStopped) {
            setCompleted(false);

            finalize();

            return;
        }

        final int edgeLength = edgePoints.size();

        final int mod = volSize / 50; // mod is 2 percent of length

        final float xRes = srcImage.getFileInfo(0).getResolutions()[0];
        final float xResSquared = xRes * xRes;
        final float yRes = srcImage.getFileInfo(0).getResolutions()[1];
        final float yResSquared = yRes * yRes;
        final float zRes = srcImage.getFileInfo(0).getResolutions()[2];
        final float zResSquared = zRes * zRes;

        pix = 0;

        for (z = 0; (z < zDim) && !threadStopped; z++) {

            for (y = 0; (y < yDim) && !threadStopped; y++) {

                for (x = 0; (x < xDim) && !threadStopped; x++) {

                    try {

                        if ( ( (pix % mod) == 0)) {
                            fireProgressStateChanged(Math.round( (pix + 1) / ((float) volSize) * 100));
                        }
                    } catch (final NullPointerException npe) {

                        if (threadStopped) {
                            Preferences
                                    .debug(
                                            "somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                            Preferences.DEBUG_ALGORITHM);
                        }
                    }

                    if (entireImage || mask.get(pix)) {

                        if (imgBuffer[pix] > 0) {
                            distance = 10000000;

                            // Test the distace from point(x1, y1, z1) to all edge points and
                            // put the min. distance into the distance buffer
                            for (i = 0; i < edgeLength; i++) {
                                pt = (Point3D) (edgePoints.elementAt(i));

                                diffx = pt.x - x;
                                diffy = pt.y - y;
                                diffz = pt.z - z;

                                dist = (diffx * diffx * xResSquared) + (diffy * diffy * yResSquared)
                                        + (diffz * diffz * zResSquared);

                                if (dist < distance) {
                                    distance = dist;
                                }
                            }

                            minDistanceBuffer[pix] = -(float) Math.sqrt(distance);
                        }
                    } // if (entireImage || mask.get(pix))
                    else {
                        distance = 10000000;

                        // Test the distace from point(x1, y1, z1) to all edge points and
                        // put the min. distance into the distance buffer
                        for (i = 0; i < edgeLength; i++) {
                            pt = (Point3D) (edgePoints.elementAt(i));

                            diffx = pt.x - x;
                            diffy = pt.y - y;
                            diffz = pt.z - z;

                            dist = (diffx * diffx * xResSquared) + (diffy * diffy * yResSquared)
                                    + (diffz * diffz * zResSquared);

                            if (dist < distance) {
                                distance = dist;
                            }
                        } // for (i = 0; i < edgeLength; i++)

                        minDistanceBuffer[pix] = (float) Math.sqrt(distance);
                    }

                    pix++;
                }
            }

        }

        if (returnFlag == true) {
            distanceMap = minDistanceBuffer;

            return;
        }

        try {

            if (threadStopped) {
                setCompleted(false);

                finalize();

                return;
            }

            srcImage.reallocate(ModelStorageBase.FLOAT);
            srcImage.importData(0, minDistanceBuffer, true);
        } catch (final IOException error) {
            displayError("Algorithm Morphology3D.distanceMap: Image(s) locked");
            setCompleted(false);

            return;
        } catch (final OutOfMemoryError e) {
            displayError("Algorithm Morphology3D.distanceMap: Out of memory");
            setCompleted(false);

            return;
        }

        setCompleted(true);
    }

    /**
     * Generates a Euclidian distance map of the background.
     * 
     * @param returnFlag if true then this operation is a step in the morph process
     */
    private void backgroundDistanceMap(final boolean returnFlag) {

        // if thread has already been stopped, dump out
        if (threadStopped) {
            setCompleted(false);

            finalize();

            return;
        }

        int pix, i;
        final int xDim = srcImage.getExtents()[0];
        final int yDim = srcImage.getExtents()[1];
        final int zDim = srcImage.getExtents()[2];
        final int sliceSize = xDim * yDim;
        final int volSize = xDim * yDim * zDim;
        float distance;
        float dist;
        Point3D pt;
        int diffx, diffy, diffz;
        int x, y, z;

        float[] minDistanceBuffer;
        final Vector<Point3D> edgePoints = new Vector<Point3D>();

        fireProgressStateChanged("Bg. distance image ...");
        fireProgressStateChanged(0);

        try {
            minDistanceBuffer = new float[volSize];
        } catch (final OutOfMemoryError e) {
            displayError("Algorithm Morphology3D.distanceMap: Out of memory");
            setCompleted(false);

            return;
        }

        // Save original in processBuffer and invert image in imgBuffer
        for (pix = 0; pix < volSize; pix++) {
            processBuffer[pix] = imgBuffer[pix];
            if (imgBuffer[pix] > 0) {
                imgBuffer[pix] = 0;
            } else {
                imgBuffer[pix] = 1;
            }
        }

        // Find all edge pixels and put the index of their location in a integer vector
        final int end = volSize - sliceSize - xDim - 1;

        for (pix = sliceSize + xDim + 1; (pix < end) && !threadStopped; pix++) {

            if ( (imgBuffer[pix] == 0)
                    && ( (imgBuffer[pix - xDim] != 0) || (imgBuffer[pix + 1] != 0) || (imgBuffer[pix + xDim] != 0)
                            || (imgBuffer[pix - 1] != 0) || (imgBuffer[pix - sliceSize] != 0) || (imgBuffer[pix
                            + sliceSize] != 0))) {
                edgePoints.addElement(new Point3D(pix % xDim, (pix % sliceSize) / xDim, pix / sliceSize));
            }
        }

        if (threadStopped) {
            setCompleted(false);

            finalize();

            return;
        }

        final int edgeLength = edgePoints.size();

        final int mod = volSize / 50; // mod is 2 percent of length

        final float xRes = srcImage.getFileInfo(0).getResolutions()[0];
        final float xResSquared = xRes * xRes;
        final float yRes = srcImage.getFileInfo(0).getResolutions()[1];
        final float yResSquared = yRes * yRes;
        final float zRes = srcImage.getFileInfo(0).getResolutions()[2];
        final float zResSquared = zRes * zRes;

        pix = 0;

        for (z = 0; (z < zDim) && !threadStopped; z++) {

            for (y = 0; (y < yDim) && !threadStopped; y++) {

                for (x = 0; (x < xDim) && !threadStopped; x++) {

                    try {

                        if ( ( (pix % mod) == 0)) {
                            fireProgressStateChanged(Math.round( (pix + 1) / ((float) volSize) * 100));
                        }
                    } catch (final NullPointerException npe) {

                        if (threadStopped) {
                            Preferences
                                    .debug(
                                            "somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                            Preferences.DEBUG_ALGORITHM);
                        }
                    }

                    if (entireImage || mask.get(pix)) {

                        if (imgBuffer[pix] > 0) {
                            distance = 10000000;

                            // Test the distace from point(x1, y1, z1) to all edge points and
                            // put the min. distance into the distance buffer
                            for (i = 0; i < edgeLength; i++) {
                                pt = (Point3D) (edgePoints.elementAt(i));

                                diffx = pt.x - x;
                                diffy = pt.y - y;
                                diffz = pt.z - z;

                                dist = (diffx * diffx * xResSquared) + (diffy * diffy * yResSquared)
                                        + (diffz * diffz * zResSquared);

                                if (dist < distance) {
                                    distance = dist;
                                }
                            }

                            minDistanceBuffer[pix] = (float) Math.sqrt(distance);
                        }
                    } // if (entireImage || mask.get(pix))
                    else {
                        minDistanceBuffer[pix] = processBuffer[pix];
                    }

                    pix++;
                }
            }

        }

        if (returnFlag == true) {
            distanceMap = minDistanceBuffer;

            return;
        }

        try {

            if (threadStopped) {
                setCompleted(false);

                finalize();

                return;
            }

            srcImage.reallocate(ModelStorageBase.FLOAT);
            srcImage.importData(0, minDistanceBuffer, true);
        } catch (final IOException error) {
            displayError("Algorithm Morphology3D.distanceMap: Image(s) locked");
            setCompleted(false);

            return;
        } catch (final OutOfMemoryError e) {
            displayError("Algorithm Morphology3D.distanceMap: Out of memory");
            setCompleted(false);

            return;
        }

        setCompleted(true);
    }

    /**
     * Deletes objects larger than maximum size indicated and objects smaller than the indicated minimum size.
     * 
     * @param min delete all objects smaller than the minimum value (pixels)
     * @param max delete all objects greater than the maximum value (pixels)
     * @param returnFlag if true then this operation is a step in the morph process (i.e. close)
     */
    private void deleteObjects(final int min, final int max, final boolean returnFlag) {

        // if thread has already been stopped, dump out
        if (threadStopped) {
            setCompleted(false);
            finalize();

            return;
        }

        int i, pix;
        int count;
        final int xDim = srcImage.getExtents()[0];
        final int yDim = srcImage.getExtents()[1];
        final int zDim = srcImage.getExtents()[2];
        final int volumeLength = xDim * yDim * zDim;
        short floodValue = 1;
        short[] tmpBuffer;

        fireProgressStateChanged("Removing objects ...");

        objects.removeAllElements();

        for (pix = 0; (pix < volumeLength) && !threadStopped; pix++) {

            if (entireImage == true) {

                if (imgBuffer[pix] > 0) {
                    count = floodFill(processBuffer, pix, floodValue, imgBuffer[pix]);
                    objects.addElement(new intObject(pix, floodValue, count));
                    floodValue++;
                }
            } else if (mask.get(pix) == true) {

                if (imgBuffer[pix] > 0) {
                    count = floodFill(processBuffer, pix, floodValue, imgBuffer[pix]);
                    objects.addElement(new intObject(pix, floodValue, count));
                    floodValue++;
                }
            }
        }

        tmpBuffer = imgBuffer;
        imgBuffer = processBuffer;
        processBuffer = tmpBuffer;

        if (threadStopped) {
            setCompleted(false);

            finalize();

            return;
        }

        fireProgressStateChanged(10);

        for (i = 0; i < objects.size(); i++) {
            fireProgressStateChanged(Math.round(10 + ( (i + 1) / ((float) objects.size()) * 45)));

            if (entireImage == true) {

                if ( ( ((intObject) (objects.elementAt(i))).size < min)
                        || ( ((intObject) (objects.elementAt(i))).size > max)) {
                    floodFill(processBuffer, ((intObject) (objects.elementAt(i))).index, (short) 0,
                            ((intObject) (objects.elementAt(i))).id);
                    objects.removeElementAt(i);
                    i--;
                }
            } else if (mask.get( ((intObject) (objects.elementAt(i))).index) == true) {

                if ( ( ((intObject) (objects.elementAt(i))).size < min)
                        || ( ((intObject) (objects.elementAt(i))).size > max)) {
                    floodFill(processBuffer, ((intObject) (objects.elementAt(i))).index, (short) 0,
                            ((intObject) (objects.elementAt(i))).id);
                    objects.removeElementAt(i);
                    i--;
                }
            }
        }

        if (threadStopped) {
            setCompleted(false);

            finalize();

            return;
        }

        // relabel objects in order
        for (i = 0; i < objects.size(); i++) {
            fireProgressStateChanged(Math.round(55 + ( (i + 1) / ((float) objects.size()) * 45)));

            if (entireImage == true) {
                floodFill(processBuffer, ((intObject) (objects.elementAt(i))).index, (short) (i + 1),
                        ((intObject) (objects.elementAt(i))).id);
            } else if (mask.get( ((intObject) (objects.elementAt(i))).index) == true) {
                floodFill(processBuffer, ((intObject) (objects.elementAt(i))).index, (short) (i + 1),
                        ((intObject) (objects.elementAt(i))).id);
            }
        }

        if (threadStopped) {
            setCompleted(false);

            finalize();

            return;
        }

        try {
            ViewUserInterface.getReference().setDataText(
                    "\nDeleted objects outside range (" + min + "," + max + "). \n");

            for (i = 0; i < objects.size(); i++) {
                ViewUserInterface.getReference().setDataText(
                        "  Object " + (i + 1) + " = " + ((intObject) (objects.elementAt(i))).size + "\n");
            }
        } catch (final NullPointerException npe) {

            if (threadStopped) {
                Preferences
                        .debug(
                                "somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                Preferences.DEBUG_ALGORITHM);
            }
        }

        if (returnFlag == true) {
            return;
        }

        try {

            if (threadStopped) {
                setCompleted(false);

                finalize();

                return;
            }

            if ( (srcImage.getType() == ModelStorageBase.BOOLEAN) || (srcImage.getType() == ModelStorageBase.BYTE)
                    || (srcImage.getType() == ModelStorageBase.UBYTE) || (srcImage.getType() == ModelStorageBase.SHORT)) {
                srcImage.reallocate(ModelStorageBase.USHORT);
            }

            srcImage.importData(0, processBuffer, true);
        } catch (final IOException error) {
            displayError("Algorithm Morphology3D: Image(s) locked");
            setCompleted(false);

            return;
        } catch (final OutOfMemoryError e) {
            displayError("Algorithm Morphology3D: Out of memory");
            setCompleted(false);

            return;
        }

        setCompleted(true);
    }

    /**
     * Dilates a boolean, unsigned byte, or unsigned short image using the indicated kernel and the indicated number of
     * iterations.
     * 
     * @param returnFlag if true then this operation is a step in the morph process (i.e. close)
     */
    private void dilate(final boolean returnFlag) {

        // if thread has already been stopped, dump out
        if (threadStopped) {
            setCompleted(false);

            finalize();

            return;
        }

        int c;
        int i, j, k, pix, count;
        int indexY;
        int indexYU;
        int offsetX, offsetY, offsetZ;
        int offsetXU;
        int startX, startY, startZ;
        int endX, endY, endZ;
        short[] tempBuffer;
        short value;

        final int xDim = srcImage.getExtents()[0];
        final int yDim = srcImage.getExtents()[1];
        final int zDim = srcImage.getExtents()[2];

        final int halfKDim = kDimXY / 2;
        final int halfKDimZ = kDimZ / 2;
        final int sliceSize = xDim * yDim;
        final int imgSize = sliceSize * zDim;
        final int stepZ = kDimZ * sliceSize;
        final int stepY = kDimXY * xDim;

        final int totalSize = imgSize * iterationsD;
        int tmpSize = 0;
        final int mod = totalSize / 100;

        fireProgressStateChanged("Dilating image ...");

        final int length = xDim * yDim * zDim;

        for (pix = 0; pix < length; pix++) {
            processBuffer[pix] = 0;
        }

        for (c = 0; (c < iterationsD) && !threadStopped; c++) {
            tmpSize = c * imgSize;

            for (pix = 0; (pix < imgSize) && !threadStopped; pix++) {

                if ( ( (tmpSize + pix) % mod) == 0) {
                    fireProgressStateChanged(Math.round((float) (tmpSize + pix) / totalSize * 100));
                }

                if (entireImage || mask.get(pix)) {
                    value = imgBuffer[pix];

                    if (value != 0) {
                        offsetX = (pix % xDim) - halfKDim;
                        offsetXU = offsetX + kDimXY;
                        offsetY = ( (pix / xDim) % yDim) - halfKDim;
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

                        for (k = startZ; k < endZ; k += sliceSize) {

                            // only work on valid pixels
                            // essentially process only the overlap between
                            // valid image and kernel slices
                            if (k >= 0) {
                                startY = k + indexY;
                                endY = k + indexYU;

                                for (j = startY; j < endY; j += xDim) {
                                    startX = j + offsetX;
                                    endX = j + offsetXU;

                                    for (i = startX; i < endX; i++) {

                                        if (kernel.get(count) == true) {
                                            processBuffer[i] = value;
                                        }

                                        count++;
                                    }
                                }
                            } else {

                                // jump to the next kernel slice as the current slice
                                // overlaps invalid image slices
                                count += kDimXY * kDimXY;
                            } // end if (k > 0) {} else {}
                        } // end for (k = startZ; ...)
                    }
                } else {
                    processBuffer[pix] = imgBuffer[pix];
                }
            }

            tempBuffer = imgBuffer;
            imgBuffer = processBuffer;
            processBuffer = tempBuffer;
        }

        if (returnFlag == true) {
            return;
        }

        try {

            if (threadStopped) {
                setCompleted(false);

                finalize();

                return;
            }

            srcImage.importData(0, imgBuffer, true);
        } catch (final IOException error) {
            displayError("Algorithm Morphology3D: Image(s) locked");
            setCompleted(false);

            return;
        }

        setCompleted(true);
    }

    /**
     * Generates a Euclidian distance map.
     * 
     * @param returnFlag if true then this operation is a step in the morph process
     */
    private void distanceMap(final boolean returnFlag) {

        // if thread has already been stopped, dump out
        if (threadStopped) {
            setCompleted(false);

            finalize();

            return;
        }

        int pix, i;
        final int xDim = srcImage.getExtents()[0];
        final int yDim = srcImage.getExtents()[1];
        final int zDim = srcImage.getExtents()[2];
        final int sliceSize = xDim * yDim;
        final int volSize = xDim * yDim * zDim;
        float distance;
        float dist;
        int diffx, diffy, diffz;
        int x, y, z;
        Point3D pt;

        float[] minDistanceBuffer;
        final Vector<Point3D> edgePoints = new Vector<Point3D>();
        fireProgressStateChanged("Distance image ...");

        try {
            minDistanceBuffer = new float[volSize];
        } catch (final OutOfMemoryError e) {
            displayError("Algorithm Morphology3D.distanceMap: Out of memory");
            setCompleted(false);

            return;
        }

        // Find all edge pixels and put the index of their location in a integer vector
        final int end = volSize - sliceSize - xDim - 1;

        for (pix = 0; (pix < volSize) && !threadStopped; pix++) {

            if ( (pix < (sliceSize + xDim)) && (imgBuffer[pix] > 0)) {
                edgePoints.addElement(new Point3D(pix % xDim, (pix % sliceSize) / xDim, pix / sliceSize));
            } else if ( ( (pix % xDim) == (xDim - 1)) && (imgBuffer[pix] > 0)) {
                edgePoints.addElement(new Point3D(pix % xDim, (pix % sliceSize) / xDim, pix / sliceSize));
            } else if ( ( (pix % xDim) == 0) && (imgBuffer[pix] > 0)) {
                edgePoints.addElement(new Point3D(pix % xDim, (pix % sliceSize) / xDim, pix / sliceSize));
            } else if ( ( (pix % sliceSize) < xDim) && (imgBuffer[pix] > 0)) {
                edgePoints.addElement(new Point3D(pix % xDim, (pix % sliceSize) / xDim, pix / sliceSize));
            } else if ( ( (pix % sliceSize) > (sliceSize - xDim)) && (imgBuffer[pix] > 0)) {
                edgePoints.addElement(new Point3D(pix % xDim, (pix % sliceSize) / xDim, pix / sliceSize));
            } else if ( (pix > (volSize - sliceSize - 1)) && (imgBuffer[pix] > 0)) {
                edgePoints.addElement(new Point3D(pix % xDim, (pix % sliceSize) / xDim, pix / sliceSize));
            } else if ( (pix > (sliceSize + 1))
                    && (pix < end)
                    && (imgBuffer[pix] == 0)
                    && ( (imgBuffer[pix - xDim] != 0) || (imgBuffer[pix + 1] != 0) || (imgBuffer[pix + xDim] != 0)
                            || (imgBuffer[pix - 1] != 0) || (imgBuffer[pix - sliceSize] != 0) || (imgBuffer[pix
                            + sliceSize] != 0))) {
                edgePoints.addElement(new Point3D(pix % xDim, (pix % sliceSize) / xDim, pix / sliceSize));
            }
        }

        if (threadStopped) {
            setCompleted(false);

            finalize();

            return;
        }

        final int edgeLength = edgePoints.size();

        final int mod = volSize / 50; // mod is 2 percent of length

        final float xRes = srcImage.getFileInfo(0).getResolutions()[0];
        final float xResSquared = xRes * xRes;
        final float yRes = srcImage.getFileInfo(0).getResolutions()[1];
        final float yResSquared = yRes * yRes;
        final float zRes = srcImage.getFileInfo(0).getResolutions()[2];
        final float zResSquared = zRes * zRes;

        pix = 0;

        for (z = 0; (z < zDim) && !threadStopped; z++) {

            for (y = 0; (y < yDim) && !threadStopped; y++) {

                for (x = 0; (x < xDim) && !threadStopped; x++) {

                    if ( ( (pix % mod) == 0)) {
                        fireProgressStateChanged(Math.round( (pix + 1) / ((float) volSize) * 100));
                    }

                    /*
                     * try {
                     * 
                     * if (((pix % mod) == 0)) { fireProgressStateChanged(Math.round((pix + 1) / ((float) volSize) *
                     * 100)); } } catch (NullPointerException npe) {
                     * 
                     * if (threadStopped) { Preferences.debug("somehow you managed to cancel the algorithm and dispose
                     * the progressbar between checking for threadStopping and using it.", Preferences.DEBUG_ALGORITHM); } }
                     */
                    if (entireImage || mask.get(pix)) {

                        if (imgBuffer[pix] > 0) {
                            distance = 10000000;

                            // Test the distace from point(x1, y1, z1) to all edge points and
                            // put the min. distance into the distance buffer
                            for (i = 0; i < edgeLength; i++) {
                                pt = (Point3D) (edgePoints.elementAt(i));

                                diffx = pt.x - x;
                                diffy = pt.y - y;
                                diffz = pt.z - z;

                                dist = (diffx * diffx * xResSquared) + (diffy * diffy * yResSquared)
                                        + (diffz * diffz * zResSquared);

                                if (dist < distance) {
                                    distance = dist;
                                }
                            } // for (i = 0; i < edgeLength; i++)

                            minDistanceBuffer[pix] = (float) Math.sqrt(distance);
                        } // if (imgBuffer[pix] > 0)
                    } // if (entireImage || mask.get(pix))
                    else {
                        minDistanceBuffer[pix] = imgBuffer[pix];
                    }

                    pix++;
                }
            }
        }

        if (returnFlag == true) {
            distanceMap = minDistanceBuffer;

            return;
        }

        try {

            if (threadStopped) {
                setCompleted(false);

                finalize();

                return;
            }

            srcImage.reallocate(ModelStorageBase.FLOAT);
            srcImage.importData(0, minDistanceBuffer, true);
        } catch (final IOException error) {
            displayError("Algorithm Morphology3D.distanceMap: Image(s) locked");
            setCompleted(false);

            return;
        } catch (final OutOfMemoryError e) {
            displayError("Algorithm Morphology3D.distanceMap: Out of memory");
            setCompleted(false);

            return;
        }

        setCompleted(true);
    }

    /**
     * Erodes a boolean, unsigned byte, or unsigned short image using the indicated kernel and the indicated number of
     * iterations.
     * 
     * @param returnFlag if true then this operation is a step in the morph process (i.e. open)
     */
    private void erode(final boolean returnFlag) {

        // if thread has already been stopped, dump out
        if (threadStopped) {
            setCompleted(false);

            finalize();

            return;
        }

        boolean clear;
        short value = 0;
        int c;
        int i, j, k, pix, count;
        int indexY;
        int indexYU;
        int offsetX, offsetY, offsetZ;
        int offsetXU;
        int startX, startY, startZ;
        int endX, endY, endZ;
        short[] tempBuffer;

        final int xDim = srcImage.getExtents()[0];
        final int yDim = srcImage.getExtents()[1];
        final int zDim = srcImage.getExtents()[2];

        final int halfKDim = kDimXY / 2;
        final int halfKDimZ = kDimZ / 2;

        final int sliceSize = xDim * yDim;
        final int imgSize = sliceSize * zDim;
        final int stepZ = kDimZ * sliceSize;
        final int stepY = kDimXY * xDim;

        final int totalSize = imgSize * iterationsE;
        int tmpSize = 0;
        final int mod = totalSize / 100;
        fireProgressStateChanged("Eroding image ...");

        /*
         * try {
         * 
         * if { fireProgressStateChanged("Eroding image ..."); }
         * 
         * if { fireProgressStateChanged(0); } } catch (NullPointerException npe) {
         * 
         * if (threadStopped) { Preferences.debug("somehow you managed to cancel the algorithm and dispose the
         * progressbar between checking for threadStopping and using it.", Preferences.DEBUG_ALGORITHM); } }
         */
        for (pix = 0; pix < imgSize; pix++) {
            processBuffer[pix] = 0;
        }

        for (c = 0; (c < iterationsE) && !threadStopped; c++) {
            tmpSize = c * imgSize;

            for (pix = 0; (pix < imgSize) && !threadStopped; pix++) {

                if ( ( ( (tmpSize + pix) % mod) == 0)) {
                    fireProgressStateChanged(Math.round((float) (tmpSize + pix) / totalSize * 100));
                }

                /*
                 * try {
                 * 
                 * if ((((tmpSize + pix) % mod) == 0)) { } } catch (NullPointerException npe) {
                 * 
                 * if (threadStopped) { Preferences.debug("somehow you managed to cancel the algorithm and dispose the
                 * progressbar between checking for threadStopping and using it.", Preferences.DEBUG_ALGORITHM); } }
                 */
                if (entireImage || mask.get(pix)) {
                    value = imgBuffer[pix];

                    if (imgBuffer[pix] == 0) {
                        clear = true;
                    } else {
                        clear = false;
                        offsetX = (pix % xDim) - halfKDim;
                        offsetXU = offsetX + kDimXY;
                        offsetY = ( (pix / xDim) % yDim) - halfKDim;
                        offsetZ = (pix / (sliceSize)) - halfKDimZ;

                        count = 0;
                        indexY = offsetY * xDim;
                        indexYU = indexY + stepY;
                        startZ = offsetZ * sliceSize;
                        endZ = startZ + stepZ;

                        // Took this out and check it later. This caused the a subtle error by setting
                        // a pixel on an incorrect slice
                        // if (startZ < 0) {
                        // startZ = 0;
                        // }

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

                        kernelLoop: for (k = startZ; k < endZ; k += sliceSize) {

                            // only process on valid image slices
                            // essentially process only the overlap between
                            // valid image and kernel slices
                            if (k >= 0) {
                                startY = k + indexY;
                                endY = k + indexYU;

                                for (j = startY; j < endY; j += xDim) {
                                    startX = j + offsetX;
                                    endX = j + offsetXU;

                                    for (i = startX; i < endX; i++) {

                                        if ( (kernel.get(count) == true) && (imgBuffer[i] == 0)) {
                                            clear = true;

                                            break kernelLoop;
                                        }

                                        count++;
                                    }
                                }
                            } else {

                                // jump to the next kernel slice as the current
                                // image slice overlaps invalid image slices
                                count += kDimXY * kDimXY;
                            } // end if (k > 0) {} else {}
                        }
                    }

                    if (clear == true) {
                        processBuffer[pix] = 0;
                    } else {
                        processBuffer[pix] = value;
                    }
                } else {
                    processBuffer[pix] = imgBuffer[pix];
                }
            }

            tempBuffer = imgBuffer;
            imgBuffer = processBuffer;
            processBuffer = tempBuffer;
        }

        if (returnFlag == true) {
            return;
        }

        try {

            if (threadStopped) {
                setCompleted(false);

                finalize();

                return;
            }

            srcImage.importData(0, imgBuffer, true);
        } catch (final IOException error) {
            displayError("Algorithm Morphology3D: Image(s) locked");
            setCompleted(false);

            return;
        }

        setCompleted(true);
    }

    /**
     * Fill the holes inside the cell region blocks.
     * 
     * @param returnFlag if true then this operation is a step in the morph process (i.e. close)
     */
    private void fillHoles(final boolean returnFlag) {

        if (threadStopped) {
            finalize();

            return;
        }

        final int xDim = srcImage.getExtents()[0];
        final int yDim = srcImage.getExtents()[1];
        final int zDim = srcImage.getExtents()[2];

        // int imageLength = xDim * yDim * zDim;
        int offset;

        final int sliceSize = xDim * yDim;
        final short floodValue = 2;

        // Fill the boundary of each the image slice with value 0, which represent the seed value
        int i = 0;

        for (int z = 0; z < zDim; z++) {

            // slice size accumulator
            offset = sliceSize * z;

            // top boundary
            for (i = offset; i < (xDim + offset); i++) {
                imgBuffer[i] = 0;
            }

            // bottom boundary
            for (i = ( (yDim - 1) * xDim) + offset; i < ( (yDim * xDim) + offset); i++) {
                imgBuffer[i] = 0;
            }

            // left boundary
            for (i = offset; i < (sliceSize + offset); i = i + xDim) {
                imgBuffer[i] = 0;
            }

            // right boundary
            for (i = xDim + offset; i < (sliceSize + offset); i = i + xDim) {
                imgBuffer[i - 1] = 0;
            }
        }

        // region grow to fill the holes inside the cell region block.
        fillHolesRegion(0, floodValue, imgBuffer[0]);

        // if THREAD stopped already, then dump out!
        if (threadStopped) {
            finalize();

            return;
        }

        try {

            if ( (srcImage.getType() == ModelStorageBase.BOOLEAN) || (srcImage.getType() == ModelStorageBase.BYTE)
                    || (srcImage.getType() == ModelStorageBase.UBYTE) || (srcImage.getType() == ModelStorageBase.SHORT)) {
                srcImage.reallocate(ModelStorageBase.USHORT);
            }

            srcImage.importData(0, imgBuffer, true);
        } catch (final IOException error) {
            displayError("Algorithm Morphology3D: Image(s) locked");
            setCompleted(false);

            return;
        } catch (final OutOfMemoryError e) {
            displayError("Algorithm Morphology3D: Out of memory");
            setCompleted(false);

            return;
        }

        if (showFrame) {
            final ModelImage tempImage = new ModelImage(ModelStorageBase.USHORT, srcImage.getExtents(), "Fill Objects");

            try {
                tempImage.importData(0, imgBuffer, true);
            } catch (final IOException error) {
                displayError("Algorithm Morphology3D: Image(s) locked in Fill Objects");
                setCompleted(false);

                return;
            }

            new ViewJFrameImage(tempImage, null, new Dimension(300, 300));
        }

        if (returnFlag == true) {
            return;
        }

        setCompleted(true);

    }

    /**
     * 3D flood fill that fill the holes insize the cell region block.
     * 
     * @param stIndex the starting index of the seed point
     * @param floodValue the value to flood the region with
     * @param objValue object ID value that idenditifies the flood region.
     */
    private void fillHolesRegion(final int stIndex, final short floodValue, final short objValue) {
        final int xDim = srcImage.getExtents()[0];
        final int yDim = srcImage.getExtents()[1];
        final int zDim = srcImage.getExtents()[2];
        final int sliceSize = xDim * yDim;
        int x, y, z;
        int indexZ, indexY;
        int pixCount = 0;

        Point3D pt;
        Point3D tempPt;
        final Point3D seed3DPt = new Point3D( (stIndex % sliceSize) % xDim, (stIndex % sliceSize) / xDim,
                (stIndex / sliceSize));
        final Stack<Point3D> stack = new Stack<Point3D>();

        if (imgBuffer[ (seed3DPt.z * sliceSize) + (seed3DPt.y * xDim) + seed3DPt.x] == objValue) {
            stack.push(seed3DPt);
            imgBuffer[ (seed3DPt.z * sliceSize) + (seed3DPt.y * xDim) + seed3DPt.x] = floodValue;

            // While loop mark the back ground region with value 2.
            while ( !stack.empty()) {
                pt = (Point3D) stack.pop();
                x = pt.x;
                y = pt.y;
                z = pt.z;

                indexZ = z * sliceSize;
                indexY = y * xDim;

                // voxel itself
                if (imgBuffer[indexZ + indexY + x] == objValue) {
                    imgBuffer[indexZ + indexY + x] = floodValue;
                }

                pixCount++;

                // checking on the voxel's six neighbors
                if ( (x + 1) < xDim) {

                    if (imgBuffer[indexZ + indexY + x + 1] == objValue) {
                        tempPt = new Point3D(x + 1, y, z);
                        stack.push(tempPt);
                    }
                }

                if ( (x - 1) >= 0) {

                    if (imgBuffer[indexZ + indexY + x - 1] == objValue) {
                        tempPt = new Point3D(x - 1, y, z);
                        stack.push(tempPt);
                    }
                }

                if ( (y + 1) < yDim) {

                    if (imgBuffer[indexZ + ( (y + 1) * xDim) + x] == objValue) {
                        tempPt = new Point3D(x, y + 1, z);
                        stack.push(tempPt);
                    }
                }

                if ( (y - 1) >= 0) {

                    if (imgBuffer[indexZ + ( (y - 1) * xDim) + x] == objValue) {
                        tempPt = new Point3D(x, y - 1, z);
                        stack.push(tempPt);
                    }
                }

                if ( (z + 1) < zDim) {

                    if (imgBuffer[ ( (z + 1) * sliceSize) + indexY + x] == objValue) {
                        tempPt = new Point3D(x, y, z + 1);
                        stack.push(tempPt);
                    }
                }

                if ( (z - 1) >= 0) {

                    if (imgBuffer[ ( (z - 1) * sliceSize) + indexY + x] == objValue) {
                        tempPt = new Point3D(x, y, z - 1);
                        stack.push(tempPt);
                    }
                }
            }
        }

        // Fill the pixels with value 2 to 0, else to 1. Fill holes.
        for (int i = 0; i < (xDim * yDim * zDim); i++) {

            if (imgBuffer[i] == 2) {
                imgBuffer[i] = 0;
            } else {
                imgBuffer[i] = 1;
            }
        }
    }

    /**
     * 3D flood fill that forms a short mask.
     * 
     * @param idBuffer buffer to store flooding results
     * @param stIndex starting index indicating the starting location of the flood fill
     * @param floodValue the value to flood the area with
     * @param objValue DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    private int floodFill(final short[] idBuffer, final int stIndex, final short floodValue, final short objValue) {
        final int xDim = srcImage.getExtents()[0];
        final int yDim = srcImage.getExtents()[1];
        final int zDim = srcImage.getExtents()[2];
        final int sliceSize = xDim * yDim;
        int x, y, z;
        int indexZ, indexY;
        int pixCount = 0;

        Point3D pt;
        Point3D tempPt;
        final Point3D seed3DPt = new Point3D( (stIndex % sliceSize) % xDim, (stIndex % sliceSize) / xDim,
                (stIndex / sliceSize));
        final Stack<Point3D> stack = new Stack<Point3D>();

        if (imgBuffer[ (seed3DPt.z * sliceSize) + (seed3DPt.y * xDim) + seed3DPt.x] > 0) {
            stack.push(seed3DPt);
            imgBuffer[ (seed3DPt.z * sliceSize) + (seed3DPt.y * xDim) + seed3DPt.x] = 0;

            while ( !stack.empty()) {
                pt = (Point3D) stack.pop();
                x = pt.x;
                y = pt.y;
                z = pt.z;

                indexZ = z * sliceSize;
                indexY = y * xDim;
                idBuffer[indexZ + indexY + x] = floodValue;
                pixCount++;

                if ( (x + 1) < xDim) {

                    if (imgBuffer[indexZ + indexY + x + 1] == objValue) {
                        tempPt = new Point3D(x + 1, y, z);
                        stack.push(tempPt);
                        imgBuffer[indexZ + indexY + tempPt.x] = 0;
                    }
                }

                if ( (x - 1) >= 0) {

                    if (imgBuffer[indexZ + indexY + x - 1] == objValue) {
                        tempPt = new Point3D(x - 1, y, z);
                        stack.push(tempPt);
                        imgBuffer[indexZ + indexY + tempPt.x] = 0;
                    }
                }

                if ( (y + 1) < yDim) {

                    if (imgBuffer[indexZ + ( (y + 1) * xDim) + x] == objValue) {
                        tempPt = new Point3D(x, y + 1, z);
                        stack.push(tempPt);
                        imgBuffer[indexZ + (tempPt.y * xDim) + x] = 0;
                    }
                }

                if ( (y - 1) >= 0) {

                    if (imgBuffer[indexZ + ( (y - 1) * xDim) + x] == objValue) {
                        tempPt = new Point3D(x, y - 1, z);
                        stack.push(tempPt);
                        imgBuffer[indexZ + (tempPt.y * xDim) + x] = 0;
                    }
                }

                if ( (z + 1) < zDim) {

                    if (imgBuffer[ ( (z + 1) * sliceSize) + indexY + x] == objValue) {
                        tempPt = new Point3D(x, y, z + 1);
                        stack.push(tempPt);
                        imgBuffer[ (tempPt.z * sliceSize) + indexY + x] = 0;
                    }
                }

                if ( (z - 1) >= 0) {

                    if (imgBuffer[ ( (z - 1) * sliceSize) + indexY + x] == objValue) {
                        tempPt = new Point3D(x, y, z - 1);
                        stack.push(tempPt);
                        imgBuffer[ (tempPt.z * sliceSize) + indexY + x] = 0;
                    }
                }
            }
        }

        return pixCount;
    }

    /**
     * This method returns whether or not pix is the index of an endpoint in tmpBuffer (it is assumed that location pix
     * is not the intensity of the background in tmpBuffer).
     * 
     * @param pix the index of a non-zero pixel in tmpBuffer
     * @param tmpBuffer the image
     * 
     * @return DOCUMENT ME!
     */
    private boolean isEndpoint(final int pix, final short[] tmpBuffer) {
        short p1, p2, p3, p4, p6, p7, p8, p9;
        final int xDim = srcImage.getExtents()[0];
        final short bgColor = 0;

        p1 = tmpBuffer[pix - xDim - 1];
        p2 = tmpBuffer[pix - xDim];
        p3 = tmpBuffer[pix - xDim + 1];
        p4 = tmpBuffer[pix - 1];
        p6 = tmpBuffer[pix + 1];
        p7 = tmpBuffer[pix + xDim - 1];
        p8 = tmpBuffer[pix + xDim];
        p9 = tmpBuffer[pix + xDim + 1];

        if ( (p2 == bgColor) && (p3 == bgColor) && (p4 != bgColor) && (p6 == bgColor) && (p8 == bgColor)
                && (p9 == bgColor)) {
            return true;
        } else if ( (p2 != bgColor) && (p4 == bgColor) && (p6 == bgColor) && (p7 == bgColor) && (p8 == bgColor)
                && (p9 == bgColor)) {
            return true;
        } else if ( (p1 == bgColor) && (p2 == bgColor) && (p4 == bgColor) && (p6 != bgColor) && (p7 == bgColor)
                && (p8 == bgColor)) {
            return true;
        } else if ( (p1 == bgColor) && (p2 == bgColor) && (p3 == bgColor) && (p4 == bgColor) && (p6 == bgColor)
                && (p8 != bgColor)) {
            return true;
        } else if ( (p1 != bgColor) && (p2 == bgColor) && (p3 == bgColor) && (p4 == bgColor) && (p6 == bgColor)
                && (p7 == bgColor) && (p8 == bgColor) && (p9 == bgColor)) {
            return true;
        } else if ( (p1 == bgColor) && (p2 == bgColor) && (p3 != bgColor) && (p4 == bgColor) && (p6 == bgColor)
                && (p7 == bgColor) && (p8 == bgColor) && (p9 == bgColor)) {
            return true;
        } else if ( (p1 == bgColor) && (p2 == bgColor) && (p3 == bgColor) && (p4 == bgColor) && (p6 == bgColor)
                && (p7 != bgColor) && (p8 == bgColor) && (p9 == bgColor)) {
            return true;
        } else if ( (p1 == bgColor) && (p2 == bgColor) && (p3 == bgColor) && (p4 == bgColor) && (p6 == bgColor)
                && (p7 == bgColor) && (p8 == bgColor) && (p9 != bgColor)) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Generates a kernel of the indicated type.
     * 
     * @param kernelType type of kernel to be generated.
     */
    private void makeKernel(final int kernelType) {

        switch (kernelType) {

            case CONNECTED6:
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

            case CONNECTED24:
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

            case CONNECTED26:
                kDimXY = 3;
                kDimZ = 3;
                kernel = new BitSet(27);
                kernel.set(0);
                kernel.set(1);
                kernel.set(2);
                kernel.set(3);
                kernel.set(4);
                kernel.set(5);
                kernel.set(6);
                kernel.set(7);
                kernel.set(8);
                kernel.set(9);
                kernel.set(10);
                kernel.set(11);
                kernel.set(12);
                kernel.set(13);
                kernel.set(14);
                kernel.set(15);
                kernel.set(16);
                kernel.set(17);
                kernel.set(18);
                kernel.set(19);
                kernel.set(20);
                kernel.set(21);
                kernel.set(22);
                kernel.set(23);
                kernel.set(24);
                kernel.set(25);
                kernel.set(26);
                break;

            default:
        }
    }

    /**
     * Generates a spherical kernel of a specific diameter. Most importantly it normalizes the kernel based on the
     * resolution in the z-dimension. This in effect accounts for the anisotrophy in the z-dimension of typical 3D
     * medical images. The resultant data is stored in the kernel class variable.
     */
    private void makeSphericalKernel() {
        int x, y, z;
        int length;
        int halfKDim, halfKDimZ;
        double width, radius;
        double thickness;
        double distance;
        final float[] resolutions = srcImage.getFileInfo()[0].getResolutions();

        width = sphereDiameter / resolutions[0];
        kDimXY = (int) Math.round(width + 1);

        if ( (kDimXY % 2) == 0) {
            kDimXY++;
        }

        if (kDimXY < 3) {
            kDimXY = 3;
        }

        Preferences.debug("# Morph3d.makeSphericalKernel: kernel size = " + kDimXY + "\n", Preferences.DEBUG_ALGORITHM);

        thickness = resolutions[2] / resolutions[0];
        kDimZ = (int) Math.round( (sphereDiameter / thickness) + 1);

        if ( (kDimZ % 2) == 0) {
            kDimZ++;
        }

        if (kDimZ < 3) {
            kDimZ = 3;
        }

        length = kDimXY * kDimXY * kDimZ;
        kernel = new BitSet(length);
        halfKDim = kDimXY / 2;
        halfKDimZ = kDimZ / 2;

        radius = width / 2.0;

        for (z = 0; z < kDimZ; z++) {

            for (y = 0; y < kDimXY; y++) {

                for (x = 0; x < kDimXY; x++) {
                    distance = Math.sqrt( ( (x - halfKDim) * (x - halfKDim)) + ( (y - halfKDim) * (y - halfKDim))
                            + ( (z - halfKDimZ) * thickness * (z - halfKDimZ) * thickness));

                    if (distance < radius) {
                        kernel.set( (z * kDimXY * kDimXY) + (y * kDimXY) + x);
                    }
                }
            }
        }

        if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM)) {
            showKernel();
        }
    }

    /**
     * Used by ultimate erode to simplify code a little. Checks to find a single pixels in the 27 neighborhood.
     * 
     * @param buffer input data buffer
     * @param index index of interest
     * @param sliceSize number of voxels in the image plane
     * @param xDim dimension of the image in the x direction
     * 
     * @return true if single pixel, false if not single pixel.
     */
    @SuppressWarnings("unused")
    private boolean onePixel(final short[] buffer, final int index, final int sliceSize, final int xDim) {

        if ( (buffer[index] > 0) && (buffer[index - sliceSize] == 0) && (buffer[index - sliceSize - xDim] == 0)
                && (buffer[index - sliceSize - xDim + 1] == 0) && (buffer[index - sliceSize + 1] == 0)
                && (buffer[index - sliceSize + xDim + 1] == 0) && (buffer[index - sliceSize + xDim] == 0)
                && (buffer[index - sliceSize + xDim - 1] == 0) && (buffer[index - sliceSize - 1] == 0)
                && (buffer[index - sliceSize - xDim - 1] == 0) && (buffer[index - xDim] == 0)
                && (buffer[index - xDim + 1] == 0) && (buffer[index + 1] == 0) && (buffer[index + xDim + 1] == 0)
                && (buffer[index + xDim] == 0) && (buffer[index + xDim - 1] == 0) && (buffer[index - 1] == 0)
                && (buffer[index - xDim - 1] == 0) && (buffer[index + sliceSize] == 0)
                && (buffer[index + sliceSize - xDim] == 0) && (buffer[index + sliceSize - xDim + 1] == 0)
                && (buffer[index + sliceSize + 1] == 0) && (buffer[index + sliceSize + xDim + 1] == 0)
                && (buffer[index + sliceSize + xDim] == 0) && (buffer[index + sliceSize + xDim - 1] == 0)
                && (buffer[index + sliceSize - 1] == 0) && (buffer[index + sliceSize - xDim - 1] == 0)) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Take a mask like image and performs an analysis.
     * 
     * <p>
     * 1. Deletes objects that are too small (noise) and objects that are too big 2. Ultimate erode and remove points
     * that are too close 3. Watershed using ultimate eroded points as seed points and distance map. 4. Delete objects
     * that are too small (noise) and objects that are too big
     * </p>
     * 
     * <p>
     * Ult erode & (bg dist map AND orig image) => watershed(ultErodePts, ANDED Bg Dist) => IDobjects
     * </p>
     */
    private void particleAnalysis() {

        // if thread has already been stopped, dump out
        if (threadStopped) {
            setCompleted(false);
            finalize();

            return;
        }

        final int xDim = srcImage.getExtents()[0];
        final int yDim = srcImage.getExtents()[1];
        final int zDim = srcImage.getExtents()[2];
        final int sliceSize = xDim * yDim;
        final int volSize = xDim * yDim * zDim;
        int pix;
        Vector3f[] seeds;
        int[] destExtents = null;
        ModelImage wsImage = null;
        ModelImage distanceImage = null;
        AlgorithmWatershed ws = null;
        short imgBufferOrg[] = null;
        short imgBuffer2[];
        ModelImage srcImage2 = null;
        int i;

        final int[] progressValues = getProgressValues();
        setMaxProgressValue(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 45));

        try {
            deleteObjects(min, max, true);

            if (threadStopped) {
                setCompleted(false);
                finalize();

                return;
            }

            srcImage.exportData(0, volSize, imgBuffer);
        } catch (final IOException error) {
            displayError("Algorithm Morphology3D.particleAnalysis: Image(s) locked");
            setCompleted(false);

            return;
        }

        setProgressValues(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 45),
                ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 90));

        ultimateErode(true); // forms list of points (one point per object, hopefully)

        if (threadStopped) {
            setCompleted(false);
            finalize();

            return;
        }

        // reset progress min & max
        setProgressValues(progressValues);

        try {
            srcImage.exportData(0, volSize, imgBuffer);
            destExtents = new int[3];
        } catch (final IOException error) {
            displayError("Algorithm Morphology3D.particleAnalysis: Image(s) locked");
            setCompleted(false);

            return;
        } catch (final OutOfMemoryError e) {
            displayError("Algorithm Morphology3D.particleAnalysis: Out of memory");
            setCompleted(false);

            return;
        }

        // calc min max of distanceMap (calculated in Ultimate erode !!)
        float minDist = Float.MAX_VALUE;
        float maxDist = -Float.MAX_VALUE;

        for (pix = 0; pix < volSize; pix++) {
            if ( (entireImage) || (mask.get(pix))) {
                if (distanceMap[pix] < minDist) {
                    minDist = distanceMap[pix];
                }

                if (distanceMap[pix] > maxDist) {
                    maxDist = distanceMap[pix];
                }
            }
        }

        for (pix = 0; pix < volSize; pix++) {
            if ( (entireImage) || (mask.get(pix))) {
                if (imgBuffer[pix] > 0) {
                    distanceMap[pix] = maxDist + minDist - distanceMap[pix];
                } else {
                    distanceMap[pix] = 0;
                }
            }
        }

        if (threadStopped) {
            setCompleted(false);

            finalize();

            return;
        }

        for (pix = sliceSize; pix < (volSize - sliceSize); pix++) {
            if ( (entireImage) || (mask.get(pix))) {
                if ( (distanceMap[pix] == 0)
                        && ( ( (distanceMap[pix - 1] <= maxDist) && (distanceMap[pix - 1] > 0))
                                || ( (distanceMap[pix + 1] <= maxDist) && (distanceMap[pix + 1] > 0))
                                || ( (distanceMap[pix - xDim] <= maxDist) && (distanceMap[pix - xDim] > 0))
                                || ( (distanceMap[pix + xDim] <= maxDist) && (distanceMap[pix + xDim] > 0))
                                || ( (distanceMap[pix - sliceSize] <= maxDist) && (distanceMap[pix - sliceSize] > 0)) || ( (distanceMap[pix
                                + sliceSize] <= maxDist) && (distanceMap[pix + sliceSize] > 0)))) {

                    distanceMap[pix] = maxDist + 10;
                }
            }
        }

        for (pix = 0; pix < volSize; pix++) {
            if ( (entireImage) || (mask.get(pix))) {
                if (distanceMap[pix] == 0) {
                    distanceMap[pix] = maxDist + 5;
                }
            }
        }

        fireProgressStateChanged("Watershed stage ...");

        destExtents[0] = xDim;
        destExtents[1] = yDim;
        destExtents[2] = zDim;

        // Make result image of float type
        try {

            // form vector of seeds from ultimate erode points + point for background(should be first in list!)
            seeds = new Vector3f[ultErodeObjects.length + 1];
            seeds[0] = new Vector3f(1, 1, 1);

            for (i = 0; i < ultErodeObjects.length; i++) {
                seeds[i + 1] = new Vector3f(ultErodeObjects[i].X, ultErodeObjects[i].Y, ultErodeObjects[i].Z);
            }

            for (i = 0; i < seeds.length; i++) {

                if (seeds[i].X == 0) {
                    seeds[i].X++;
                } else if (seeds[i].X == (xDim - 1)) {
                    seeds[i].X--;
                }

                if (seeds[i].Y == 0) {
                    seeds[i].Y++;
                } else if (seeds[i].Y == (yDim - 1)) {
                    seeds[i].Y--;
                }

                if (seeds[i].Z == 0) {
                    seeds[i].Z++;
                } else if (seeds[i].Z == (zDim - 1)) {
                    seeds[i].Z--;
                }

                Preferences.debug("Seed " + i + " = " + seeds[i].X + "," + seeds[i].Y + "," + seeds[i].Z, 
                		Preferences.DEBUG_ALGORITHM);
            }

            wsImage = new ModelImage(ModelStorageBase.USHORT, destExtents, "Watershed");

            distanceImage = new ModelImage(ModelStorageBase.FLOAT, destExtents, "Distance");

            distanceImage.importData(0, distanceMap, true);
            // srcImage.importData(0, distanceMap, true);
            if ( !entireImage) {
                imgBufferOrg = new short[volSize];
                imgBuffer2 = new short[volSize];
                try {
                    srcImage.exportData(0, volSize, imgBufferOrg);
                } catch (final IOException e) {
                    displayError("Algorithm Morphology2D: Image(s) locked");
                    setCompleted(false);
                    return;
                }
                for (i = 0; i < volSize; i++) {
                    if (mask.get(i)) {
                        imgBuffer2[i] = imgBufferOrg[i];
                    }
                }
                srcImage2 = (ModelImage) srcImage.clone();
                try {
                    srcImage2.importData(0, imgBuffer2, true);
                } catch (final IOException e) {
                    displayError("Algorithm Morphology2D: Image(s) locked");
                    setCompleted(false);
                    return;
                }
                ws = new AlgorithmWatershed(wsImage, srcImage2, null, null, null, true);
                imgBuffer2 = null;
            } // if (!entireImage)
            else {
                ws = new AlgorithmWatershed(wsImage, srcImage, null, null, null, true);
            }
        } catch (final IOException error) {
            displayError("Algorithm Morphology3Db: Image(s) locked");
            setCompleted(false);

            return;
        } catch (final OutOfMemoryError e) {
            displayError("Algorithm Morphology3D: Out of memory");
            setCompleted(false);

            return;
        }

        linkProgressToAlgorithm(ws);
        ws.setProgressValues(generateProgressValues(90, 95));

        ws.setSeedVector(seeds);
        ws.setEnergyImage(distanceImage);
        ws.run();

        if ( !ws.isCompleted()) { // if the (sub) algo is cancelled, this is too

            setCompleted(false);
            finalize();

            return;
        }

        if ( !entireImage) {
            srcImage2.disposeLocal();
            srcImage2 = null;
        }

        try {
            wsImage.exportData(0, xDim * yDim * zDim, imgBuffer);

            // once the data from the watershed has been exported to imgBuffer, the
            // 2 temp images (wsImage, distanceImage) can be cleaned up
            wsImage.disposeLocal();
            distanceImage.disposeLocal();
            wsImage = null;
            distanceImage = null;

        } catch (final IOException error) {
            displayError("Algorithm Morphology3Dc: Image(s) locked");
            setCompleted(false);

            return;
        }

        fireProgressStateChanged("Deleting objects ...");
        setProgressValues(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 95),
                ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 100));
        deleteObjects(min, max, false);

        if ( !entireImage) {
            try {
                srcImage.exportData(0, volSize, imgBuffer);
            } catch (final IOException e) {
                displayError("Algorithm Morphology2D: Image(s) locked");
                setCompleted(false);
                return;
            }
            for (i = 0; i < volSize; i++) {
                if ( !mask.get(i)) {
                    imgBuffer[i] = imgBufferOrg[i];
                }
            }
            try {
                srcImage.importData(0, imgBuffer, true);
            } catch (final IOException e) {
                displayError("Algorithm Morphology2D: Image(s) locked");
                setCompleted(false);
                return;
            }
        }

        if (threadStopped) {
            setCompleted(false);
            finalize();

            return;
        }

        setCompleted(true);

    }

    /**
     * This a thinning algorithm used to do half of one layer of thinning (which layer is dictated by whether pass is
     * even or odd).
     * 
     * @param pass the number pass this execution is on the image
     * @param slice the slice number being worked on (starting with 0);
     * @param table the table to lookup whether to delete the pixel or let it stay.
     * 
     * @return DOCUMENT ME!
     */
    private int thin(final int pass, final int slice, final int[] table) {
        int p1, p2, p3, p4, p5, p6, p7, p8, p9;
        final int bgColor = 0;
        int pix;
        final int xDim = srcImage.getExtents()[0];
        final int yDim = srcImage.getExtents()[1];
        final int sliceSize = xDim * yDim;

        int v, index, code;
        int pixelsRemoved = 0;
        int pixInSlice;

        for (pix = slice * sliceSize; pix < ( (slice + 1) * sliceSize); pix++) {

            if (entireImage || mask.get(pix)) {
                pixInSlice = pix % sliceSize;
                p5 = imgBuffer[pix] & 0xff;
                v = p5;

                if (v != bgColor) {

                    if ( (pixInSlice - xDim - 1) < 0) {
                        p1 = bgColor;
                    } else {
                        p1 = imgBuffer[pix - xDim - 1] & 0xff;
                    }

                    if ( (pixInSlice - xDim) < 0) {
                        p2 = bgColor;
                    } else {
                        p2 = imgBuffer[pix - xDim] & 0xff;
                    }

                    if ( ( (pixInSlice - xDim + 1) < 0) || ( (pixInSlice - xDim + 1) >= sliceSize)) {
                        p3 = bgColor;
                    } else {
                        p3 = imgBuffer[pix - xDim + 1] & 0xff;
                    }

                    if ( (pixInSlice - 1) < 0) {
                        p4 = bgColor;
                    } else {
                        p4 = imgBuffer[pix - 1] & 0xff;
                    }

                    if ( (pixInSlice + 1) >= sliceSize) {
                        p6 = bgColor;
                    } else {
                        p6 = imgBuffer[pix + 1] & 0xff;
                    }

                    if ( ( (pixInSlice + xDim - 1) < 0) || ( (pixInSlice + xDim - 1) >= sliceSize)) {
                        p7 = bgColor;
                    } else {
                        p7 = imgBuffer[pix + xDim - 1] & 0xff;
                    }

                    if ( (pixInSlice + xDim) >= sliceSize) {
                        p8 = bgColor;
                    } else {
                        p8 = imgBuffer[pix + xDim] & 0xff;
                    }

                    if ( (pixInSlice + xDim + 1) >= sliceSize) {
                        p9 = bgColor;
                    } else {
                        p9 = imgBuffer[pix + xDim + 1] & 0xff;
                    }

                    index = 0;

                    if (p1 != bgColor) {
                        index |= 1;
                    }

                    if (p2 != bgColor) {
                        index |= 2;
                    }

                    if (p3 != bgColor) {
                        index |= 4;
                    }

                    if (p6 != bgColor) {
                        index |= 8;
                    }

                    if (p9 != bgColor) {
                        index |= 16;
                    }

                    if (p8 != bgColor) {
                        index |= 32;
                    }

                    if (p7 != bgColor) {
                        index |= 64;
                    }

                    if (p4 != bgColor) {
                        index |= 128;
                    }

                    code = table[index];

                    if ( (pass & 1) == 1) { // odd pass

                        if ( (code == 2) || (code == 3)) {
                            v = bgColor;
                            pixelsRemoved++;
                        }
                    } else { // even pass

                        if ( (code == 1) || (code == 3)) {
                            v = bgColor;
                            pixelsRemoved++;
                        }
                    }
                }

                processBuffer[pix] = (byte) v;
            } else {
                processBuffer[pix] = imgBuffer[pix];
            }
        }

        return pixelsRemoved;
    }

    /**
     * Erodes to a single point.
     * 
     * @param returnFlag if true then this operation is a step in the morph process (i.e. open)
     */
    private void ultimateErode(final boolean returnFlag) {

        // if thread has already been stopped, dump out
        if (threadStopped) {
            setCompleted(false);

            finalize();

            return;
        }

        int vox, voxel, i, j;
        final int xDim = srcImage.getExtents()[0];
        final int yDim = srcImage.getExtents()[1];
        final int zDim = srcImage.getExtents()[2];

        final int sliceSize = xDim * yDim;
        final int volSize = xDim * yDim * zDim;
        float distance;
        float dist;
        float diffx, diffy;
        int z;
        int indexMax = 0;
        Vector3f pt = null;
        Vector3f maxPt = null;
        Vector4f[] erodeObjs;
        Vector3f[] erodeObjsOrdered;
        float max;
        float cPt;
        float xRes, xResSquared, yRes, yResSquared;
        Vector<Vector3f> edgePointsSlice = null;
        Vector<Vector3f> uPointsSlice = null;
        Vector<Vector3f> uPointsSliceOrdered = null;

        float[] minDistanceBuffer;
        short imgBuffer2[];

        try {
            minDistanceBuffer = new float[sliceSize];
            edgePointsSlice = new Vector<Vector3f>();
            uPointsSlice = new Vector<Vector3f>();
            uPointsSliceOrdered = new Vector<Vector3f>();
            maxPt = new Vector3f();
        } catch (final OutOfMemoryError e) {
            displayError("Algorithm Morphology3D.distanceMap: Out of memory");
            setCompleted(false);

            return;
        }

        xRes = srcImage.getFileInfo(0).getResolutions()[0];
        xResSquared = xRes * xRes;
        yRes = srcImage.getFileInfo(0).getResolutions()[1];
        yResSquared = yRes * yRes;

        for (vox = 0; vox < sliceSize; vox++) {
            imgBuffer[vox] = 0;
        }

        for (vox = volSize - sliceSize; vox < volSize; vox++) {
            imgBuffer[vox] = 0;
        }

        for (vox = sliceSize; vox < (volSize - sliceSize); vox++) {

            // top edge
            if ( (vox % sliceSize) < xDim) {
                imgBuffer[vox] = 0;
            } // bottom edge
            else if ( (vox % sliceSize) > (sliceSize - xDim)) {
                imgBuffer[vox] = 0;
            } // left edge
            else if ( (vox % xDim) == 0) {
                imgBuffer[vox] = 0;
            } // right edge
            else if ( (vox % xDim) == (xDim - 1)) {
                imgBuffer[vox] = 0;
            }
        }

        final int[] progressValues = getProgressValues();

        this.setMaxProgressValue(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 30));

        distanceMap(true);

        this.setProgressValues(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 30),
                ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 95));

        identifyObjects(true); // results are in destImage

        // reset the min/max progress values to original state...no longer calling functions
        setProgressValues(progressValues);

        if (threadStopped) {
            setCompleted(false);

            finalize();

            return;
        }

        try {

            // go and get source image and put it in imgBuffer
            srcImage.exportData(0, volSize, imgBuffer); // locks and releases lock
        } catch (final IOException error) {
            displayError("Algorithm Morphology3D: Image(s) locked");
            setCompleted(false);

            return;
        }

        fireProgressStateChanged(95, null, "Eroding ...");

        // We have choosen to find ult. points on a slice by slice basis
        for (z = 0; (z < zDim) && !threadStopped; z++) {
            edgePointsSlice.removeAllElements();

            for (vox = z * sliceSize; vox < ( (z + 1) * sliceSize); vox++) {

                if ( ( (vox % xDim) == 0) && (imgBuffer[vox] > 0)) {
                    edgePointsSlice.addElement(new Vector3f(vox % xDim, (vox % sliceSize) / xDim, zDim));
                } else if ( ( (vox % xDim) == (xDim - 1)) && (imgBuffer[vox] > 0)) {
                    edgePointsSlice.addElement(new Vector3f(vox % xDim, (vox % sliceSize) / xDim, zDim));
                } else if ( ( (vox % sliceSize) == (sliceSize - 1)) && (imgBuffer[vox] > 0)) {
                    edgePointsSlice.addElement(new Vector3f(vox % xDim, (vox % sliceSize) / xDim, zDim));
                } else if ( ( (vox % sliceSize) < xDim) && (imgBuffer[vox] > 0)) {
                    edgePointsSlice.addElement(new Vector3f(vox % xDim, (vox % sliceSize) / xDim, zDim));
                } else if ( ( (vox % sliceSize) > (sliceSize - xDim)) && (imgBuffer[vox] > 0)) {
                    edgePointsSlice.addElement(new Vector3f(vox % xDim, (vox % sliceSize) / xDim, zDim));
                } else if ( (vox > xDim)
                        && (vox < (volSize - sliceSize - xDim))
                        && (imgBuffer[vox] == 0)
                        && ( (imgBuffer[vox - xDim] != 0) || (imgBuffer[vox + 1] != 0) || (imgBuffer[vox + xDim] != 0) || (imgBuffer[vox - 1] != 0))) {
                    edgePointsSlice.addElement(new Vector3f(vox % xDim, (vox % sliceSize) / xDim, zDim));
                }
            }

            // calculate distance image of objects based on edge pixels
            for (voxel = z * sliceSize, vox = 0; vox < sliceSize; voxel++, vox++) {
                minDistanceBuffer[vox] = 0;

                if (entireImage || mask.get(vox)) {

                    if (imgBuffer[voxel] > 0) {
                        distance = 10000000;

                        // Test the distace from point(x1, y1) to all edge points and
                        // put the min. distance into the distance buffer
                        for (i = 0; i < edgePointsSlice.size(); i++) {
                            pt = (Vector3f) (edgePointsSlice.elementAt(i));

                            diffx = pt.X - (vox % xDim);
                            diffy = pt.Y - (vox / xDim);

                            dist = (diffx * diffx * xResSquared) + (diffy * diffy * yResSquared);

                            if (dist < distance) {
                                distance = dist;
                            }
                        }

                        minDistanceBuffer[vox] = (float) Math.sqrt(distance);
                    }
                }
            }

            if (threadStopped) {
                setCompleted(false);

                finalize();

                return;
            }

            // Find all local maximum
            uPointsSlice.removeAllElements();

            for (vox = xDim + 1, voxel = (z * sliceSize) + vox; vox < (sliceSize - xDim - 1); vox++, voxel++) {

                if (imgBuffer[voxel] > 0) {
                    cPt = minDistanceBuffer[vox];

                    if ( (cPt >= minDistanceBuffer[vox - xDim]) && (cPt >= minDistanceBuffer[vox - xDim + 1])
                            && (cPt >= minDistanceBuffer[vox + 1]) && (cPt >= minDistanceBuffer[vox + xDim + 1])
                            && (cPt >= minDistanceBuffer[vox + xDim]) && (cPt >= minDistanceBuffer[vox + xDim - 1])
                            && (cPt >= minDistanceBuffer[vox - 1]) && (cPt >= minDistanceBuffer[vox - xDim - 1])) {

                        uPointsSlice.addElement(new Vector3f(voxel, imgBuffer[voxel], minDistanceBuffer[vox]));
                    }
                }
            }

            // Order ult. points - not effient but number points should be small
            uPointsSliceOrdered.removeAllElements();

            final int size = uPointsSlice.size();

            for (j = 0; j < size; j++) {
                max = -1;
                indexMax = j;
                pt = (Vector3f) (uPointsSlice.elementAt(j));
                maxPt.X = pt.X;
                maxPt.Y = pt.Y;
                maxPt.Z = pt.Z;

                for (i = 0; i < size; i++) {
                    pt = (Vector3f) (uPointsSlice.elementAt(i));

                    if (pt.Z > max) {
                        max = pt.Z;
                        maxPt.X = pt.X;
                        maxPt.Y = pt.Y;
                        maxPt.Z = pt.Z;
                        indexMax = i;
                    }
                }

                ((Vector3f) (uPointsSlice.elementAt(indexMax))).Z = -2;
                uPointsSliceOrdered.addElement(new Vector3f(maxPt.X, maxPt.Y, maxPt.Z));
            }

            // find points with dist <= max with in the same object and delete them
            // ie. one point per object on a slice
            for (j = 0; j < uPointsSliceOrdered.size(); j++) {
                pt = (Vector3f) (uPointsSliceOrdered.elementAt(j));

                for (i = j + 1; i < uPointsSliceOrdered.size(); i++) {

                    if (pt.Y == ((Vector3f) (uPointsSliceOrdered.elementAt(i))).Y) {
                        uPointsSliceOrdered.removeElementAt(i);
                        i--;
                    }
                }
            }

            for (int m = 0; m < sliceSize; m++) {
                imgBuffer[ (z * sliceSize) + m] = 0;
            }

            for (i = 0; i < uPointsSliceOrdered.size(); i++) {
                pt = (Vector3f) (uPointsSliceOrdered.elementAt(i));
                imgBuffer[(int) pt.X] = (short) pt.Z;
            }
        }

        fireProgressStateChanged(98);

        if (threadStopped) {
            setCompleted(false);

            finalize();

            return;
        }

        int nErodeObj = 0;

        for (vox = 0; vox < volSize; vox++) {

            if (imgBuffer[vox] > 0) {
                nErodeObj++;
            }
        }

        // add code to remove points that are too close !
        xRes = srcImage.getFileInfo(0).getResolutions()[0];
        xResSquared = xRes * xRes;
        yRes = srcImage.getFileInfo(0).getResolutions()[1];
        yResSquared = yRes * yRes;

        try {
            erodeObjs = new Vector4f[nErodeObj];
            erodeObjsOrdered = new Vector3f[nErodeObj];

            for (vox = 0, i = 0; vox < volSize; vox++) {

                if (imgBuffer[vox] > 0) {
                    erodeObjs[i] = new Vector4f(vox % xDim, vox / xDim % yDim, vox / sliceSize, distanceMap[vox]);
                    i++;
                }
            }

            // order points
            for (j = 0; j < erodeObjs.length; j++) {
                max = -1;
                indexMax = j;
                maxPt.X = erodeObjs[j].X;
                maxPt.Y = erodeObjs[j].Y;
                maxPt.Z = erodeObjs[j].Z;

                for (i = 0; i < erodeObjs.length; i++) {

                    if (erodeObjs[i].W > max) {
                        max = erodeObjs[i].W;
                        maxPt.X = erodeObjs[i].X;
                        maxPt.Y = erodeObjs[i].Y;
                        maxPt.Z = erodeObjs[i].Z;
                        indexMax = i;
                    }
                }

                erodeObjs[indexMax].W = -2;
                erodeObjsOrdered[j] = new Vector3f(maxPt.X, maxPt.Y, maxPt.Z);
            }

            for (i = 0; i < (erodeObjsOrdered.length - 1); i++) {

                if (erodeObjsOrdered[i].X != -1) {

                    for (j = i + 1; j < erodeObjsOrdered.length; j++) {

                        if (erodeObjsOrdered[j].X != -1) {

                            if (Math
                                    .sqrt( ( (erodeObjsOrdered[i].X - erodeObjsOrdered[j].X) * (erodeObjsOrdered[i].X - erodeObjsOrdered[j].X))
                                            + ( (erodeObjsOrdered[i].Y - erodeObjsOrdered[j].Y) * (erodeObjsOrdered[i].Y - erodeObjsOrdered[j].Y))
                                            + ( (erodeObjsOrdered[i].Z - erodeObjsOrdered[j].Z) * (erodeObjsOrdered[i].Z - erodeObjsOrdered[j].Z))) < pixDist) {

                                imgBuffer[(int) ( (erodeObjsOrdered[j].Z * sliceSize) + (erodeObjsOrdered[j].Y * xDim) + erodeObjsOrdered[j].X)] = 0;
                                erodeObjsOrdered[j].X = -1;
                            }
                        }
                    }
                }
            }

            nErodeObj = 0;

            for (i = 0; i < erodeObjsOrdered.length; i++) {

                if (erodeObjsOrdered[i].X != -1) {
                    nErodeObj++;
                }
            }

            ultErodeObjects = new Vector3f[nErodeObj];

            for (i = 0, j = 0; i < erodeObjsOrdered.length; i++) {

                if (erodeObjsOrdered[i].X != -1) {
                    ultErodeObjects[j] = erodeObjsOrdered[i];
                    j++;
                }
            }

            if ( !entireImage) {
                imgBuffer2 = new short[volSize];
                try {
                    srcImage.exportData(0, volSize, imgBuffer2);
                } catch (final IOException e) {
                    displayError("Algorithm Morphology3D: Image(s) locked");
                    setCompleted(false);

                    return;
                }
                for (i = 0; i < volSize; i++) {
                    if ( !mask.get(i)) {
                        imgBuffer[i] = imgBuffer2[i];
                    }
                }
                imgBuffer2 = null;
            } // if (!entireImage)
        } catch (final OutOfMemoryError e) {
            displayError("Algorithm Morphology3D: Out of memory");
            setCompleted(false);

            return;
        }

        fireProgressStateChanged(100);

        if (returnFlag == true) {
            return;
        }

        try {

            if (threadStopped) {
                setCompleted(false);

                finalize();

                return;
            }

            srcImage.importData(0, imgBuffer, true);
        } catch (final IOException error) {
            displayError("Algorithm Morphology3D: Image(s) locked");
            setCompleted(false);

            return;
        }

        setCompleted(true);
    }

    // ~ Inner Classes
    // --------------------------------------------------------------------------------------------------

    /**
     * Simple class to temporarily store the object's size, ID and seed index value. This class is used by the
     * identifyObjects and deleteObjects methods of AlgorithmMorphology3D class.
     */
    private class intObject {

        /** DOCUMENT ME! */
        public short id = 0;

        /** DOCUMENT ME! */
        public int index = 0;

        /** DOCUMENT ME! */
        public int size = 0;

        /**
         * Creates a new intObject object.
         * 
         * @param idx seed index. Index is the location in the image
         * @param objectID the flood seed having a value >= 0.
         * @param objectSize the number of voxels in the object
         */
        public intObject(final int idx, final short objectID, final int objectSize) {
            index = idx;
            id = objectID;
            size = objectSize;
        }
    }

}
