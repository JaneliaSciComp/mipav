package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;
import java.util.*;


/**
 * Three-Dimensional mathematical morphology class applied to Gray scale images.
 *
 * @version  1.0 Aug 1999
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class AlgorithmGrayScaleMorphology3D extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

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
    
    public static final int TOP_HAT = 15;
    
    public static final int BOTTOM_HAT = 16;
    
    public static final int MORPHOLOGICAL_LAPLACIAN = 17;

    /** DOCUMENT ME! */
    public static final int SIZED_SPHERE = AlgorithmMorphology3D.SIZED_SPHERE;

    /** DOCUMENT ME! */
    public static final int CONNECTED6 = AlgorithmMorphology3D.CONNECTED6;

    /** DOCUMENT ME! */
    public static final int CONNECTED24 = AlgorithmMorphology3D.CONNECTED24;
    
    public static final int CONNECTED26 = AlgorithmMorphology3D.CONNECTED26;

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

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** algorithm type (i.e. erode, dilate) */
    private int algorithm;


    /** Edge type. */
    @SuppressWarnings("unused")
    private int edgingType;

    /** if true, indicates that the VOIs should NOT be used and that entire image should be processed. */
    private boolean entireImage;

    /** imgBuffer that hold voxel value for the 3D slices. */
    private double[] imgBuffer;
    
    private double[] imgBuffer2;

    /** Dilation iteration times. */
    private int iterationsD;

    /** Erosion iteration times. */
    private int iterationsE;

    /** Kernel dimension. */
    private int kDimXY;

    /** DOCUMENT ME! */
    private int kDimZ;

    /** Kernel for both erosion and dilation. */
    private BitSet kernel;

    /** kernel size (i.e. connectedness) */
    @SuppressWarnings("unused")
    private int kernelType;

    /** maximum, minimum size of objects. */
    @SuppressWarnings("unused")
    private int min = 1, max = 20000000;

    /** Number pixels to prune. */
    @SuppressWarnings("unused")
    private int numPruningPixels;

    @SuppressWarnings("unused")
    private float pixDist = 0;

    /** intermediate processing buffer, same size with imgBuffer. */
    private double[] processBuffer;

    /** kernel diameter. */
    private float sphereDiameter;


    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmMorphology3D object.
     *
     * @param  srcImg          source image model
     * @param  kernelType      kernel size (i.e. connectedness)
     * @param  sphereDiameter  only valid if kernelType == SIZED_SPHERE and represents the width of a circle in the
     *                         resolution of the image
     * @param  method          setup the algorithm method (i.e. erode)
     * @param  iterD           number of times to dilate
     * @param  iterE           number of times to erode
     * @param  pruningPix      the number of pixels to prune
     * @param  edType          the type of edging to perform (inner or outer)
     * @param  entireImage     flag that indicates if the VOIs should NOT be used and entire image should be processed
     */
    public AlgorithmGrayScaleMorphology3D(ModelImage srcImg, int kernelType, float sphereDiameter, int method, int iterD,
                                 int iterE, int pruningPix, int edType, boolean entireImage) {
        this(srcImg, null, kernelType, sphereDiameter, method, iterD, iterE, pruningPix, edType, entireImage);
    }

    /**
     * Creates a new AlgorithmMorphology3D object.
     *
     * @param  srcImg          source image model
     * @param  destImg         destination image model
     * @param  kernelType      kernel size (i.e. connectedness)
     * @param  sphereDiameter  only valid if kernelType == SIZED_SPHERE and represents the width of a circle in the
     *                         resolution of the image
     * @param  method          setup the algorithm method (i.e. erode)
     * @param  iterD           number of times to dilate
     * @param  iterE           number of times to erode
     * @param  pruningPix      the number of pixels to prune
     * @param  edType          the type of edging to perform (inner or outer)
     * @param  entireImage     flag that indicates if the VOIs should NOT be used and entire image should be processed
     */
    public AlgorithmGrayScaleMorphology3D(ModelImage srcImg, ModelImage destImg, int kernelType, float sphereDiameter,
                                 int method, int iterD, int iterE, int pruningPix, int edType, boolean entireImage) {
        super(destImg, srcImg);
        makeKernel(kernelType);
        setAlgorithm(method);
        iterationsD = iterD;
        iterationsE = iterE;
        numPruningPixels = pruningPix;
        edgingType = edType;
        this.entireImage = entireImage;
        this.kernelType = kernelType;

        if (kernelType == SIZED_SPHERE) {
            this.sphereDiameter = sphereDiameter;
            makeSphericalKernel();
        } else {
            makeKernel(kernelType);
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        kernel = null;
        processBuffer = null;
        imgBuffer = null;
        srcImage = null;

        super.finalize();
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
        if (srcImage.getType() == ModelImage.BOOLEAN) {
            displayError("Source Image cannot be Boolean");
        }
        else if (srcImage.isColorImage()) {
                
            displayError("Source Image cannot be color");
            setCompleted(false);

            return;
        }

        if (srcImage.getNDims() != 3) {
            displayError("Source Image is not 3D");
            setCompleted(false);

            return;
        }

        try {
            int length = srcImage.getSliceSize() * srcImage.getExtents()[2];

            imgBuffer = new double[length];

            if (algorithm != FILL_HOLES) {
                processBuffer = new double[length];
            }

            srcImage.exportData(0, length, imgBuffer); // locks and releases lock

            // fireProgressStateChanged(srcImage.getImageName(), "Morph 3D ...");
        } catch (IOException error) {
            displayError("Algorithm GrayScaleMorphology3D: Image(s) locked");
            setCompleted(false);


            return;
        } catch (OutOfMemoryError e) {
            displayError("Algorithm GrayScaleMorphology3D: Out of memory");
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
                imgBuffer2 = new double[imgBuffer.length];
                System.arraycopy(imgBuffer, 0, imgBuffer2, 0, imgBuffer.length);
                try { 
                    srcImage.exportData(0, imgBuffer.length, imgBuffer); // locks and releases lock
                } catch (IOException error) {
                    displayError("Algorithm GrayScaleMorphology3D: Image(s) locked");
                    setCompleted(false);

                    return;
                }
                setProgressValues(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 50),
                        ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 100));
                erode(true);
                for (int i = 0; i < imgBuffer.length; i++) {
                    imgBuffer2[i] = imgBuffer2[i] - imgBuffer[i];
                }
                try {
                    srcImage.importData(0, imgBuffer2, true);
                } catch (IOException error) {
                    displayError("Algorithm GrayScaleMorphology3D: Image(s) locked");
                    setCompleted(false);


                    return;
                }

                setCompleted(true);
                break;
                
            case TOP_HAT:
                // image - open of image
                progressValues = getProgressValues();
                setMaxProgressValue(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 50));
                erode(true);
                setProgressValues(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 50),
                                  ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 100));
                dilate(true);
                imgBuffer2 = new double[imgBuffer.length];
                System.arraycopy(imgBuffer, 0, imgBuffer2, 0, imgBuffer.length);
                try { 
                    srcImage.exportData(0, imgBuffer.length, imgBuffer); // locks and releases lock
                } catch (IOException error) {
                    displayError("Algorithm GrayScaleMorphology3D: Image(s) locked");
                    setCompleted(false);

                    return;
                }
                for (int i = 0; i < imgBuffer.length; i++) {
                    imgBuffer[i] = imgBuffer[i] - imgBuffer2[i];
                }
                try {
                    srcImage.importData(0, imgBuffer, true);
                } catch (IOException error) {
                    displayError("Algorithm GrayScaleMorphology3D: Image(s) locked");
                    setCompleted(false);


                    return;
                }

                setCompleted(true);
                break;
                
            case BOTTOM_HAT:
                // close of image - image
                progressValues = getProgressValues();
                setMaxProgressValue(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 50));
                dilate(true);
                setProgressValues(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 50),
                                  ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 100));
                erode(true);
                imgBuffer2 = new double[imgBuffer.length];
                System.arraycopy(imgBuffer, 0, imgBuffer2, 0, imgBuffer.length);
                try { 
                    srcImage.exportData(0, imgBuffer.length, imgBuffer); // locks and releases lock
                } catch (IOException error) {
                    displayError("Algorithm GrayScaleMorphology3D: Image(s) locked");
                    setCompleted(false);

                    return;
                }
                for (int i = 0; i < imgBuffer.length; i++) {
                    imgBuffer2[i] = imgBuffer2[i] - imgBuffer[i];
                }
                try {
                    srcImage.importData(0, imgBuffer2, true);
                } catch (IOException error) {
                    displayError("Algorithm GrayScaleMorphology3D: Image(s) locked");
                    setCompleted(false);


                    return;
                }

                setCompleted(true);
                break;
                
            case MORPHOLOGICAL_LAPLACIAN:
                // (dilation of image - image) - (image - erosion of image)
                progressValues = getProgressValues();
                setMaxProgressValue(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 50));
                dilate(true);
                imgBuffer2 = new double[imgBuffer.length];
                System.arraycopy(imgBuffer, 0, imgBuffer2, 0, imgBuffer.length);
                try { 
                    srcImage.exportData(0, imgBuffer.length, imgBuffer); // locks and releases lock
                } catch (IOException error) {
                    displayError("Algorithm GrayScaleMorphology3D: Image(s) locked");
                    setCompleted(false);

                    return;
                }
                setProgressValues(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 50),
                        ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 100));
                erode(true);
                for (int i = 0; i < imgBuffer.length; i++) {
                    imgBuffer2[i] = imgBuffer2[i] + imgBuffer[i];
                }
                try { 
                    srcImage.exportData(0, imgBuffer.length, imgBuffer); // locks and releases lock
                } catch (IOException error) {
                    displayError("Algorithm GrayScaleMorphology3D: Image(s) locked");
                    setCompleted(false);

                    return;
                }
                for (int i = 0; i < imgBuffer.length; i++) {
                    imgBuffer2[i] = imgBuffer2[i] - 2 * imgBuffer[i];
                }
                try {
                    srcImage.importData(0, imgBuffer2, true);
                } catch (IOException error) {
                    displayError("Algorithm GrayScaleMorphology3D: Image(s) locked");
                    setCompleted(false);


                    return;
                }

                setCompleted(true);
                break;

            /*case FILL_HOLES:
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
                break;*/

            default:
                break;
        }
    }

    /**
     * Sets the algorithm type (i.e. erode, dilate)
     *
     * @param  method  algorithm type
     */
    public void setAlgorithm(int method) {
        this.algorithm = method;
    }

    /**
     * Sets new source image.
     *
     * @param  img  image model
     */
    public void setImage(ModelImage img) {
        srcImage = img;
    }

    /**
     * Sets user defined square kernels.
     *
     * @param  kernel  user defined kernel
     * @param  dimXY   length of x and y dimensions
     * @param  dimZ    length of z dimension
     */
    public void setKernel(BitSet kernel, int dimXY, int dimZ) {
        this.kernel = kernel;
        kDimXY = dimXY;
        kDimZ = dimZ;
    }

    /**
     * Sets the bounds of object size used to delete objects.
     *
     * @param  _min  minimum size of objects
     * @param  _max  maximum size of objects
     */
    public void setMinMax(int _min, int _max) {
        min = _min;
        max = _max;
    }

    /**
     * Used in the ultimate erode function to remove all eroded points less than the distance specified in pixel units.
     *
     * @param  dist  distance in pixels.
     */
    public void setPixDistance(float dist) {
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

                        if (kernel.get((z * kDimXY * kDimXY) + (y * kDimXY) + x) == true) {
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
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Morphology3D: Out of memory");
            setCompleted(false);


            return;
        }
    }

    
    
    

    /**
     * Dilates an image using the indicated kernel and the indicated number of
     * iterations.  The grayscale dilation is the maximum value over the reflected kernel region.
     * For symmetric kernels the reflected kernel is the same as the kernel.
     *
     * @param  returnFlag  if true then this operation is a step in the morph process (i.e. close)
     */
    private void dilate(boolean returnFlag) {

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
        double[] tempBuffer;

        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];

        int halfKDim = kDimXY / 2;
        int halfKDimZ = kDimZ / 2;
        int sliceSize = xDim * yDim;
        int imgSize = sliceSize * zDim;
        int stepZ = kDimZ * sliceSize;
        int stepY = kDimXY * xDim;

        int totalSize = imgSize * iterationsD;
        int tmpSize = 0;
        int mod = totalSize / 100;

        fireProgressStateChanged("Dilating image ...");

        int length = xDim * yDim * zDim;

        for (pix = 0; pix < length; pix++) {
            processBuffer[pix] = 0;
        }

        for (c = 0; (c < iterationsD) && !threadStopped; c++) {
            tmpSize = c * imgSize;

            for (pix = 0; (pix < imgSize) && !threadStopped; pix++) {

                if (((tmpSize + pix) % mod) == 0) {
                    fireProgressStateChanged(Math.round((float) (tmpSize + pix) / totalSize * 100));
                }

                if (entireImage || mask.get(pix)) {
                    processBuffer[pix] = imgBuffer[pix];

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

                                    if (kernel.get(count) && (imgBuffer[i] > processBuffer[pix])) {
                                        processBuffer[pix] = imgBuffer[i];
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
        } catch (IOException error) {
            displayError("Algorithm GrayScaleMorphology3D: Image(s) locked");
            setCompleted(false);


            return;
        }

        setCompleted(true);
    }

 
    /**
     * Erodes an image using the indicated kernel and the indicated number of
     * iterations.  The grayscale erosion is the minimum value over the kernel region.
     *
     * @param  returnFlag  if true then this operation is a step in the morph process (i.e. open)
     */
    private void erode(boolean returnFlag) {

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
        double[] tempBuffer;

        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];

        int halfKDim = kDimXY / 2;
        int halfKDimZ = kDimZ / 2;

        int sliceSize = xDim * yDim;
        int imgSize = sliceSize * zDim;
        int stepZ = kDimZ * sliceSize;
        int stepY = kDimXY * xDim;

        int totalSize = imgSize * iterationsE;
        int tmpSize = 0;
        int mod = totalSize / 100;
        fireProgressStateChanged("Eroding image ...");

        for (c = 0; (c < iterationsE) && !threadStopped; c++) {
            tmpSize = c * imgSize;

            for (pix = 0; (pix < imgSize) && !threadStopped; pix++) {

                if ((((tmpSize + pix) % mod) == 0)) {
                    fireProgressStateChanged(Math.round((float) (tmpSize + pix) / totalSize * 100));
                }

                
                if (entireImage || mask.get(pix)) {
                    processBuffer[pix] = imgBuffer[pix];
                    
                    offsetX = (pix % xDim) - halfKDim;
                    offsetXU = offsetX + kDimXY;
                    offsetY = ((pix / xDim) % yDim) - halfKDim;
                    offsetZ = (pix / (sliceSize)) - halfKDimZ;

                    count = 0;
                    indexY = offsetY * xDim;
                    indexYU = indexY + stepY;
                    startZ = offsetZ * sliceSize;
                    endZ = startZ + stepZ;

                    // Took this out and check it later.  This caused the a subtle error by setting
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

                    for (k = startZ; k < endZ; k += sliceSize) {

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

                                    if ((kernel.get(count) == true) && (imgBuffer[i] < processBuffer[pix])) {
                                        processBuffer[pix] = imgBuffer[i];
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
        } catch (IOException error) {
            displayError("Algorithm GrayScaleMorphology3D: Image(s) locked");
            setCompleted(false);


            return;
        }

        setCompleted(true);
    }

   

   

    /**
     * Generates a kernel of the indicated type.
     *
     * @param  kernelType  type of kernel to be generated.
     */
    private void makeKernel(int kernelType) {

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
        float[] resolutions = srcImage.getFileInfo()[0].getResolutions();

        width = sphereDiameter / resolutions[0];
        kDimXY = (int) Math.round(width + 1);

        if ((kDimXY % 2) == 0) {
            kDimXY++;
        }

        if (kDimXY < 3) {
            kDimXY = 3;
        }

        Preferences.debug("# Morph3d.makeSphericalKernel: kernel size = " + kDimXY + "\n", Preferences.DEBUG_ALGORITHM);

        thickness = resolutions[2] / resolutions[0];
        kDimZ = (int) Math.round((sphereDiameter / thickness) + 1);

        if ((kDimZ % 2) == 0) {
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
                    distance = Math.sqrt(((x - halfKDim) * (x - halfKDim)) + ((y - halfKDim) * (y - halfKDim)) +
                                         ((z - halfKDimZ) * thickness * (z - halfKDimZ) * thickness));

                    if (distance < radius) {
                        kernel.set((z * kDimXY * kDimXY) + (y * kDimXY) + x);
                    }
                }
            }
        }

        if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM)) {
            showKernel();
        }
    }


}
