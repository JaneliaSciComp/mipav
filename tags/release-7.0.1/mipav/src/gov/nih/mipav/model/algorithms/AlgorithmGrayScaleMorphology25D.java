package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;
import java.util.*;


/**
 * 2.5D mathematical morphology class applied to Gray scale images. (2D morphology on 3D volumes). Kernels of 3x3 (4 or 8 connected) and 5x5 (12
 * connected) are available or user defined kernels can be supplied.
 *
 * <p>Supported methods include:</p>
 *
 * <ul>
 *   <li>Bottom hat</li>
 *   <li>close</li>
 *   <li>dilate</li>
 *   <li>erode</li>
 *   <li>Morphological gradient</li>
 *   <li>Morphological laplacian</li>
 *   <li>open</li>
 *   <li>Top hat</li>
 * </ul>
 *
 * <p>Adapted from AlgoritmMorphology2D by Matthew J. McAuliffe, Ph.D.</p>
 *
 * @version  1.0 June 8, 2004
 * @author   Evan McCreedy
 */
public class AlgorithmGrayScaleMorphology25D extends AlgorithmBase {

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
    public static final int PARTICLE_ANALYSIS_NEW = 12;

    /** DOCUMENT ME! */
    public static final int FILL_HOLES = 13;
    
    public static final int MORPHOLOGICAL_GRADIENT = 14;
    
    public static final int TOP_HAT = 15;
    
    public static final int BOTTOM_HAT = 16;
    
    public static final int MORPHOLOGICAL_LAPLACIAN = 17;

    /** DOCUMENT ME! */
    public static final int SIZED_CIRCLE = AlgorithmMorphology25D.SIZED_CIRCLE;

    /** DOCUMENT ME! */
    public static final int CONNECTED4 = AlgorithmMorphology25D.CONNECTED4;

    /** DOCUMENT ME! */
    public static final int CONNECTED8 = AlgorithmMorphology25D.CONNECTED8;

    /** DOCUMENT ME! */
    public static final int CONNECTED12 = AlgorithmMorphology25D.CONNECTED12;

    /** DOCUMENT ME! */
    public static final int OUTER_EDGING = 0;

    /** DOCUMENT ME! */
    public static final int INNER_EDGING = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int algorithm;

    /** DOCUMENT ME! */
    private float circleDiameter;

    /** DOCUMENT ME! */
    private boolean entireImage;

    /** DOCUMENT ME! */
    private double[] imgBuffer;
    
    private double[] imgBuffer2;

    /** DOCUMENT ME! */
    private int imgLength;

    /** DOCUMENT ME! */
    private int iterationsD;

    /** DOCUMENT ME! */
    private int iterationsE;

    /** DOCUMENT ME! */
    private int kDim;

    /** DOCUMENT ME! */
    private BitSet kernel;

    @SuppressWarnings("unused")
    private int kernelType;

    /** Number pixels to prune. */
    @SuppressWarnings("unused")
    private int numPruningPixels;

    /** DOCUMENT ME! */
    private double[] processBuffer;

    /** DOCUMENT ME! */
    private int sliceLength;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmMorphology25D object.
     *
     * @param  srcImg          source image model
     * @param  kernelType      kernel size (i.e. connectedness)
     * @param  circleDiameter  only valid if kernelType == SIZED_CIRCLE and represents the width of a circle in the
     *                         resolution of the image
     * @param  method          setup the algorithm method (i.e. erode, dilate)
     * @param  iterD           number of times to dilate
     * @param  iterE           number of times to erode
     * @param  pruningPix      the number of pixels to prune
     * @param  edType          the type of edging to perform (inner or outer)
     * @param  entireImage     if true, indicates that the VOIs should NOT be used and that entire image should be
     *                         processed
     */
    public AlgorithmGrayScaleMorphology25D(ModelImage srcImg, int kernelType, float circleDiameter, int method, int iterD,
                                  int iterE, int pruningPix, int edType, boolean entireImage) {
        super(null, srcImg);
        setAlgorithm(method);

        iterationsD = iterD;
        iterationsE = iterE;
        numPruningPixels = pruningPix;
        this.entireImage = entireImage;
        this.kernelType = kernelType;

        if ((kernelType == SIZED_CIRCLE) && (srcImage != null)) {
            this.circleDiameter = circleDiameter;
            makeCircularKernel(circleDiameter);
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

        // do all source image verification before logging:
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
        
        // verify using a 3d image for this algorithm.
        if (srcImage.getNDims() != 3) {
            displayError("Source Image is not 3D");
            setCompleted(false);

            return;
        }

        try {
            imgLength = srcImage.getSliceSize() * srcImage.getExtents()[2];
            sliceLength = srcImage.getSliceSize();

            imgBuffer = new double[imgLength];
            processBuffer = new double[imgLength];
            srcImage.exportData(0, imgLength, imgBuffer); // locks and releases lock
            // fireProgressStateChanged(srcImage.getImageName(), "Morph25D ...");
        } catch (IOException error) {
            displayError("Algorithm GrayScaleMorphology25D: Image(s) locked");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            displayError("Algorithm GrayScaleMorphology25D: Out of memory");
            setCompleted(false);

            return;
        }

        // initProgressBar();

        

        int [] progressValues = getProgressValues();
        
        switch (algorithm) {

            case ERODE:
                erode(false, iterationsE);
                break;

            case DILATE:
                dilate(false, iterationsD);
                break;

            case CLOSE:
            	setMaxProgressValue(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 50));
                dilate(true, iterationsD);
                setProgressValues(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 50),
                		ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 100));
                erode(false, iterationsE);
                break;

            case OPEN:
            	setMaxProgressValue(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 50));
                erode(true, iterationsE);
                setProgressValues(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 50),
                		ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 100));
                dilate(false, iterationsD);
                break;
                
            case MORPHOLOGICAL_GRADIENT:
                setMaxProgressValue(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 50));
                dilate(true, 1);
                imgBuffer2 = new double[imgBuffer.length];
                System.arraycopy(imgBuffer, 0, imgBuffer2, 0, imgBuffer.length);
                try { 
                    srcImage.exportData(0, imgBuffer.length, imgBuffer); // locks and releases lock
                } catch (IOException error) {
                    displayError("Algorithm GrayScaleMorphology25D: Image(s) locked");
                    setCompleted(false);

                    return;
                }
                setProgressValues(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 50),
                        ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 100));
                erode(true, 1);
                for (int i = 0; i < imgBuffer.length; i++) {
                    imgBuffer2[i] = imgBuffer2[i] - imgBuffer[i];
                }
                try {
                    srcImage.importData(0, imgBuffer2, true);
                } catch (IOException error) {
                    displayError("Algorithm GrayScaleMorphology25D: Image(s) locked");
                    setCompleted(false);


                    return;
                }

                setCompleted(true);
                break;
                
            case TOP_HAT:
                // image - open of image
                setMaxProgressValue(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 50));
                erode(true, iterationsE);
                setProgressValues(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 50),
                                  ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 100));
                dilate(true, iterationsD);
                imgBuffer2 = new double[imgBuffer.length];
                System.arraycopy(imgBuffer, 0, imgBuffer2, 0, imgBuffer.length);
                try { 
                    srcImage.exportData(0, imgBuffer.length, imgBuffer); // locks and releases lock
                } catch (IOException error) {
                    displayError("Algorithm GrayScaleMorphology25D: Image(s) locked");
                    setCompleted(false);

                    return;
                }
                for (int i = 0; i < imgBuffer.length; i++) {
                    imgBuffer[i] = imgBuffer[i] - imgBuffer2[i];
                }
                try {
                    srcImage.importData(0, imgBuffer, true);
                } catch (IOException error) {
                    displayError("Algorithm GrayScaleMorphology25D: Image(s) locked");
                    setCompleted(false);


                    return;
                }

                setCompleted(true);
                break;
                
            case BOTTOM_HAT:
                // close of image - image
                setMaxProgressValue(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 50));
                dilate(true, iterationsD);
                setProgressValues(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 50),
                                  ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 100));
                erode(true, iterationsE);
                imgBuffer2 = new double[imgBuffer.length];
                System.arraycopy(imgBuffer, 0, imgBuffer2, 0, imgBuffer.length);
                try { 
                    srcImage.exportData(0, imgBuffer.length, imgBuffer); // locks and releases lock
                } catch (IOException error) {
                    displayError("Algorithm GrayScaleMorphology25D: Image(s) locked");
                    setCompleted(false);

                    return;
                }
                for (int i = 0; i < imgBuffer.length; i++) {
                    imgBuffer2[i] = imgBuffer2[i] - imgBuffer[i];
                }
                try {
                    srcImage.importData(0, imgBuffer2, true);
                } catch (IOException error) {
                    displayError("Algorithm GrayScaleMorphology25D: Image(s) locked");
                    setCompleted(false);


                    return;
                }

                setCompleted(true);
                break;
                
            case MORPHOLOGICAL_LAPLACIAN:
                // (dilation of image - image) - (image - erosion of image)
                setMaxProgressValue(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 50));
                dilate(true, 1);
                imgBuffer2 = new double[imgBuffer.length];
                System.arraycopy(imgBuffer, 0, imgBuffer2, 0, imgBuffer.length);
                try { 
                    srcImage.exportData(0, imgBuffer.length, imgBuffer); // locks and releases lock
                } catch (IOException error) {
                    displayError("Algorithm GrayScaleMorphology25D: Image(s) locked");
                    setCompleted(false);

                    return;
                }
                setProgressValues(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 50),
                        ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 100));
                erode(true, 1);
                for (int i = 0; i < imgBuffer.length; i++) {
                    imgBuffer2[i] = imgBuffer2[i] + imgBuffer[i];
                }
                try { 
                    srcImage.exportData(0, imgBuffer.length, imgBuffer); // locks and releases lock
                } catch (IOException error) {
                    displayError("Algorithm GrayScaleMorphology25D: Image(s) locked");
                    setCompleted(false);

                    return;
                }
                for (int i = 0; i < imgBuffer.length; i++) {
                    imgBuffer2[i] = imgBuffer2[i] - 2 * imgBuffer[i];
                }
                try {
                    srcImage.importData(0, imgBuffer2, true);
                } catch (IOException error) {
                    displayError("Algorithm GrayScaleMorphology25D: Image(s) locked");
                    setCompleted(false);


                    return;
                }

                setCompleted(true);
                break;

            /*case ID_OBJECTS:
                MipavUtil.displayError("2.5D identify objects not supported.");

                // identifyObjects( false );
                break;

            case DELETE_OBJECTS:
                MipavUtil.displayError("2.5D delete objects not supported.");

                // deleteObjects( min, max, false );
                break;

            case DISTANCE_MAP:
                MipavUtil.displayError("2.5D distance map not supported.");

                // distanceMap( false );
                break;

            case BG_DISTANCE_MAP:
                MipavUtil.displayError("2.5D background distance map not supported.");

                // backgroundDistanceMap( false );
                break;

            case ULTIMATE_ERODE:
                MipavUtil.displayError("2.5D ultimate erode not supported.");

                // ultimateErode( false );
                break;

            case PARTICLE_ANALYSIS:
                MipavUtil.displayError("2.5D particle analysis not supported.");

                // particleAnalysis();
                break;

            case SKELETONIZE:
                skeletonize(numPruningPixels, false);
                break;

            case FIND_EDGES:
                MipavUtil.displayError("2.5D find edges not supported.");

                // findEdges( edgingType, false );
                break;
            case FILL_HOLES:
                fillHoles(false);
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
     * Sets the algorithm to new image of boolean type.
     *
     * @param  img  image model of boolean type
     */
    public void setImage(ModelImage img) {
        srcImage = img;
    }

    /**
     * Sets number of iterations for closing or opening.
     *
     * @param  itersD  number of dilations
     * @param  itersE  number of erosions
     */
    public void setIterations(int itersD, int itersE) {
        iterationsD = itersD;
        iterationsE = itersE;
    }


    /**
     * Sets user defined square kernels.
     *
     * @param  kernel  user defined kernel
     * @param  dim     length of one dimension (kernel should be square)
     */
    public void setKernel(BitSet kernel, int dim) {
        this.kernel = kernel;
        kDim = dim;
    }

    /**
     * display kernel as 1 or 0 in the debug window.
     */
    public void showKernel() {
        int x, y;
        double width;

        width = circleDiameter / srcImage.getFileInfo()[0].getResolutions()[0];

        int kDimXY = (int) Math.round(width);

        if ((kDimXY % 2) == 0) {
            kDimXY++;
        }

        if (kDimXY < 3) {
            kDimXY = 3;
        }

        try {
            String str = new String();

            Preferences.debug("\n Morphology25D - structuring element. \n", Preferences.DEBUG_ALGORITHM);

            for (y = 0; y < kDimXY; y++) {

                for (x = 0; x < kDimXY; x++) {

                    if (kernel.get((y * kDimXY) + x) == true) {
                        str = str + "1   ";
                    } else {
                        str = str + "0   ";
                    }
                }

                Preferences.debug(str + "\n", Preferences.DEBUG_ALGORITHM);
                str = new String();
            }

        } catch (OutOfMemoryError e) {
            displayError("Algorithm Morphology25D: Out of memory");
            setCompleted(false);

            return;
        }
    }

    /**
     * Dilates an image using the indicated kernel and the indicated number of
     * executions.  The grayscale dilation is the maximum value over the reflected kernel region.
     * For symmetric kernels the reflected kernel is the same as the kernel.
     *
     * @param  returnFlag  if true then this operation is a step in the morph process (i.e. close)
     * @param  iters       number of dilations
     */
    private void dilate(boolean returnFlag, int iters) {

        // if THREAD stopped already, then dump out!
        if (threadStopped) {
            finalize();

            return;
        }

        int c;
        int i, j, pix, count;
        int offsetX, offsetY, offsetZ;
        int offsetXU;
        int startX, startY;
        int endX, endY;

        int xDim = srcImage.getExtents()[0];
        int zDim = srcImage.getExtents()[2];

        int halfKDim = kDim / 2;
        int stepY = kDim * xDim;
        double[] tempBuffer;
        
        int mod = (iters * sliceLength * zDim) / 20; // mod is 5 percent of length

        for (c = 0; (c < iters) && !threadStopped; c++) {

            for (int curSlice = 0; curSlice < zDim; curSlice++) {
                offsetZ = curSlice * sliceLength;

                for (pix = offsetZ; (pix < (offsetZ + sliceLength)) && !threadStopped; pix++) {
                    
                    if (((((c * offsetZ) + pix) % mod) == 0)) {
                        fireProgressStateChanged(Math.round((pix + 1 + (c * sliceLength * zDim)) /
                                                               (iters * (float) sliceLength * zDim) * 100));
                    }

                    if (entireImage || mask.get(pix)) {
                        processBuffer[pix] = imgBuffer[pix];

                        offsetX = (pix % xDim) - halfKDim;
                        offsetXU = offsetX + kDim;
                        offsetY = ((pix % sliceLength) / xDim) - halfKDim;

                        count = 0;
                        startY = (offsetY * xDim) + offsetZ;
                        endY = startY + stepY;

                        if (startY < offsetZ) {
                            startY = offsetZ;
                        }

                        if (endY > (offsetZ + sliceLength)) {
                            endY = offsetZ + sliceLength;
                        }

                        if (offsetX < 0) {
                            offsetX = 0;
                        }

                        if (offsetXU > xDim) {
                            offsetXU = xDim;
                        }

                        for (j = startY; j < endY; j += xDim) {
                            startX = j + offsetX;
                            endX = j + offsetXU;

                            for (i = startX; i < endX; i++) {

                                if (kernel.get(count)  && imgBuffer[i] > processBuffer[pix]) {
                                    processBuffer[pix] = imgBuffer[i];
                                }

                                count++;
                            }
                        }
                     
                    } else {
                        processBuffer[pix] = imgBuffer[pix];
                    }
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
                finalize();

                return;
            }

            srcImage.importData(0, imgBuffer, true);
        } catch (IOException error) {
            displayError("Algorithm Morphology25D: Image(s) locked");
            setCompleted(false);
            // 
            

            return;
        }

     
        setCompleted(true);
    }

    /**
     * Erodes an image using the indicated kernel and the indicated number of
     * executions.  The grayscale erosion is the minimum value over the kernel region.
     *
     * @param  returnFlag  if true then this operation is a step in the morph process (i.e. open)
     * @param  iters       number of erosion iterations.
     */
    private void erode(boolean returnFlag, int iters) {

        // if THREAD stopped already, then dump out!
        if (threadStopped) {
            finalize();

            return;
        }

        int c;
        int i, j, pix, count;
        int offsetX, offsetY, offsetZ;
        int offsetXU;
        int startX, startY;
        int endX, endY;

        int xDim = srcImage.getExtents()[0];
        int zDim = srcImage.getExtents()[2];

        int halfKDim = kDim / 2;
        int stepY = kDim * xDim;
        double[] tempBuffer;

        fireProgressStateChanged("Eroding image ...");
        
        int mod = (iters * sliceLength * zDim) / 20; // mod is 5 percent of length

        for (c = 0; (c < iters) && !threadStopped; c++) {

            for (int curSlice = 0; curSlice < zDim; curSlice++) {
                offsetZ = curSlice * sliceLength;

                for (pix = offsetZ; (pix < (offsetZ + sliceLength)) && !threadStopped; pix++) {

                        if (((((c * offsetZ) + pix) % mod) == 0)) {
                            fireProgressStateChanged(Math.round((pix + 1 + (c * sliceLength * zDim)) /
                                                                   (iters * (float) sliceLength * zDim) * 100));
                        }

                    if (entireImage || mask.get(pix)) {
                        processBuffer[pix] = imgBuffer[pix];

                        
                        offsetX = (pix % xDim) - halfKDim;
                        offsetXU = offsetX + kDim;
                        offsetY = ((pix % sliceLength) / xDim) - halfKDim;

                        count = 0;
                        startY = (offsetY * xDim) + offsetZ;
                        endY = startY + stepY;

                        if (startY < offsetZ) {
                            startY = offsetZ;
                        }

                        if (endY > (offsetZ + sliceLength)) {
                            endY = offsetZ + sliceLength;
                        }

                        if (offsetX < 0) {
                            offsetX = 0;
                        }

                        if (offsetXU > xDim) {
                            offsetXU = xDim;
                        }

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
                        processBuffer[pix] = imgBuffer[pix];
                    }
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
                finalize();

                return;
            }

            srcImage.importData(0, imgBuffer, true);
        } catch (IOException error) {
            displayError("Algorithm GrayScaleMorphology25D: Image(s) locked");
            setCompleted(false);
            // 
            

            return;
        }

       
        setCompleted(true);
    }
    
 

    /**
     * Generates a circular kernel of a specific diameter.
     *
     * @param  circDiameter  represents the width of a circle in the resolution of the image
     */
    private void makeCircularKernel(float circDiameter) {
        int x, y;
        int length;
        int halfKDim;
        double width, radius;
        double distance;
        float[] resolutions = srcImage.getFileInfo()[0].getResolutions();

        width = circDiameter / resolutions[0];
        kDim = (int) Math.round(width);

        if ((kDim % 2) == 0) {
            kDim++;
        }

        if (kDim < 3) {
            kDim = 3;
        }

        Preferences.debug("# Morph25d.makeCircularKernel: kernel size = " + kDim + "\n", Preferences.DEBUG_ALGORITHM);

        length = kDim * kDim;
        kernel = new BitSet(length);
        halfKDim = kDim / 2;
        radius = width / 2.0;

        for (y = 0; y < kDim; y++) {

            for (x = 0; x < kDim; x++) {
                distance = Math.sqrt(((x - halfKDim) * (x - halfKDim)) + ((y - halfKDim) * (y - halfKDim)));

                if (distance < radius) {
                    kernel.set((y * kDim) + x);
                }
            }
        }

        if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM)) {
            showKernel();
        }
    }

    /**
     * Generates a kernel of the indicated type.
     *
     * @param  kernelType  type of kernel to be generated.
     */
    private void makeKernel(int kernelType) {

        switch (kernelType) {

            case CONNECTED4:
                kDim = 3;
                kernel = new BitSet(9);
                kernel.set(1);
                kernel.set(3);
                kernel.set(4);
                kernel.set(5);
                kernel.set(7);
                break;

            case CONNECTED8:
                kDim = 3;
                kernel = new BitSet(9);
                kernel.set(0);
                kernel.set(1);
                kernel.set(2);
                kernel.set(3);
                kernel.set(4);
                kernel.set(5);
                kernel.set(6);
                kernel.set(7);
                kernel.set(8);
                break;

            case CONNECTED12:
                kDim = 5;
                kernel = new BitSet(25);
                kernel.set(2);
                kernel.set(6);
                kernel.set(7);
                kernel.set(8);
                kernel.set(10);
                kernel.set(11);
                kernel.set(12);
                kernel.set(13);
                kernel.set(14);
                kernel.set(16);
                kernel.set(17);
                kernel.set(18);
                kernel.set(22);
                break;

            default:
        }
    }

}
