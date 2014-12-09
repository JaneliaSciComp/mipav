package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.Point;
import java.io.*;

import java.util.*;


/**
 * 2.5D mathmatical morphology class (2D morphology on 3D volumes). Kernels of 3x3 (4 or 8 connected) and 5x5 (12
 * connected) are available or user defined kernels can be supplied.
 *
 * <p>Supported methods include:</p>
 *
 * <ul>
 *   <li>erode</li>
 *   <li>dilate</li>
 *   <li>open</li>
 *   <li>close</li>
 *   <li>Morphological gradient
 * </ul>
 *
 * <p>Adapted from AlgoritmMorphology2D by Matthew J. McAuliffe, Ph.D.</p>
 *
 * @version  1.0 June 8, 2004
 * @author   Evan McCreedy
 */
public class AlgorithmMorphology25D extends AlgorithmBase {

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

    /** DOCUMENT ME! */
    public static final int SIZED_CIRCLE = 0;

    /** DOCUMENT ME! */
    public static final int CONNECTED4 = 1;

    /** DOCUMENT ME! */
    public static final int CONNECTED8 = 2;

    /** DOCUMENT ME! */
    public static final int CONNECTED12 = 3;

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
    private short[] imgBuffer;
    
    private short[] imgBuffer2;

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
    private int numPruningPixels;

    /** DOCUMENT ME! */
    private short[] processBuffer;

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
    public AlgorithmMorphology25D(ModelImage srcImg, int kernelType, float circleDiameter, int method, int iterD,
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
     * This method is to be applied on skeletonized images: it removes all branches which are iter or less pixels in
     * length.
     *
     * @param  iter        the threshold number of pixels for the maximum length of a removed branch to be
     * @param  returnFlag  if true then this operation is a step in the morph process (i.e. close)
     */
    public void prune(int iter, boolean returnFlag) {

        // if thread has already been stopped, dump out
        if (threadStopped) {
            return;
        }

        short p1, p2, p3, p4, p5, p6, p7, p8, p9;
        short bgColor = 0;
        short value;
        int i, slice, pix;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int sliceSize = xDim * yDim;
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
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Morphology25D: Out of memory");
            setCompleted(false);
            // 
            

            return;
        }
/*
        try {

            if  {
                fireProgressStateChanged("Pruning image ...");
            }

            if  {
                fireProgressStateChanged(0);
            }
        } catch (NullPointerException npe) {

            if (threadStopped) {
                Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                  Preferences.DEBUG_ALGORITHM);
            }
        }
*/
        for (slice = 0; slice < zDim; slice++) {
/*
            try {
                fireProgressStateChanged("Pruning Slice " + (slice + 1));
                fireProgressStateChanged(Math.round(((float) slice) / ((float) zDim) * 100));
            } catch (NullPointerException npe) {

                if (threadStopped) {
                    Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                      Preferences.DEBUG_ALGORITHM);
                }
            }
*/
            // sets the intensity of border points to 0
            for (pix = (slice * sliceSize); pix < ((slice * sliceSize) + xDim); pix++) {
                imgBuffer[pix] = 0;
            }

            for (pix = ((slice * sliceSize) + (xDim * (yDim - 1))); pix < ((slice + 1) * sliceSize); pix++) {
                imgBuffer[pix] = 0;
            }

            for (pix = (slice * sliceSize); pix <= ((slice * sliceSize) + (xDim * (yDim - 1))); pix += xDim) {
                imgBuffer[pix] = 0;
            }

            for (pix = ((slice * sliceSize) + (xDim - 1)); pix < ((slice + 1) * sliceSize); pix += xDim) {
                imgBuffer[pix] = 0;
            }

            for (pix = (slice * sliceSize); pix < ((slice + 1) * sliceSize); pix++) {
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

                while (!endpoints.isEmpty()) {
                    tempBuffer[((Integer) (endpoints.firstElement())).intValue()] = bgColor;
                    endpoints.removeElementAt(0);
                }
            }

            endpoints.removeAllElements();

            // for each occurance of 1 (the endpoint of a branch), a new element is added to branchesVector
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
            int branchesSize = branchesVector.size();

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

            for (pix = (slice * sliceSize); pix < ((slice + 1) * sliceSize); pix++) {

                if (processBuffer[pix] == -1) {
                    tempBuffer[pix] = imgBuffer[pix];
                } else {
                    tempBuffer[pix] = 0;
                }
            }

            // creates final image which contains only branches of more than iter in length
            while (!branchesVector.isEmpty()) {
                branch = (Vector<Integer>) branchesVector.firstElement();
                branchesVector.removeElementAt(0);

                while (!branch.isEmpty()) {
                    pix = ((Integer) branch.firstElement()).intValue();
                    branch.removeElementAt(0);
                    tempBuffer[pix] = imgBuffer[pix];
                }
            }

            for (pix = (slice * sliceSize); pix < ((slice + 1) * sliceSize); pix++) {
                imgBuffer[pix] = tempBuffer[pix];
            }

        }

        if (returnFlag == true) {
            return;
        }

        try {

            if (threadStopped) {
                setCompleted(false);
                // 
                
                
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
        double minValue = srcImage.getMin();
        if (minValue < 0) {
            displayError("Source Image cannot have negative minimum");
            setCompleted(false);
            return;
        }
        if ((srcImage.getType() != ModelImage.BOOLEAN) && (srcImage.getType() != ModelImage.BYTE) &&
                (srcImage.getType() != ModelImage.UBYTE) && (srcImage.getType() != ModelImage.SHORT) &&
                (srcImage.getType() != ModelImage.USHORT)) {
            displayError("Source Image must be Boolean, Byte, UByte, Short or UShort");
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

            imgBuffer = new short[imgLength];
            processBuffer = new short[imgLength];
            srcImage.exportData(0, imgLength, imgBuffer); // locks and releases lock
            // fireProgressStateChanged(srcImage.getImageName(), "Morph25D ...");
        } catch (IOException error) {
            displayError("Algorithm Morphology25D: Image(s) locked");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Morphology25D: Out of memory");
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
                imgBuffer2 = new short[imgBuffer.length];
                System.arraycopy(imgBuffer, 0, imgBuffer2, 0, imgBuffer.length);
                try { 
                    srcImage.exportData(0, imgBuffer.length, imgBuffer); // locks and releases lock
                } catch (IOException error) {
                    displayError("Algorithm GrayScaleMorphology2D: Image(s) locked");
                    setCompleted(false);

                    return;
                }
                setProgressValues(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 50),
                        ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 100));
                erode(true, 1);
                for (int i = 0; i < imgBuffer.length; i++) {
                    imgBuffer2[i] -= imgBuffer[i];
                }
                try {
                    srcImage.importData(0, imgBuffer2, true);
                } catch (IOException error) {
                    displayError("Algorithm GrayScaleMorphology2D: Image(s) locked");
                    setCompleted(false);


                    return;
                }

                setCompleted(true);
                break;

            case ID_OBJECTS:
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
                break;

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
     * Skeletonizes the image by using a lookup table to repeatedly remove pixels from the edges of objects in a binary
     * image, reducing them to single pixel wide skeletons. Based on a thinning algorithm by Zhang and Suen. There is an
     * entry in the table for each of the 256 possible 3x3 neighborhood configurations. An entry of '1' signifies to
     * delete the indicated pixel on the first pass, '2' means to delete the indicated pixel on the second pass, and '3'
     * indicates to delete the pixel on either pass.
     *
     * @param  pruningPixels  the number of pixels to prune after skeletonizing. (should be 0 if no pruning is to be
     *                        done)
     * @param  returnFlag     if true then this operation is a step in the morph process (i.e. close)
     */
    public void skeletonize(int pruningPixels, boolean returnFlag) {

        // if thread has already been stopped, dump out
        if (threadStopped) {
            setCompleted(false);
            // 
            
            
            finalize();

            return;
        }

        int[] table = // 0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0,1
        {
            0, 0, 0, 1, 0, 0, 1, 3, 0, 0, 3, 1, 1, 0, 1, 3, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 2, 0, 3, 0, 3, 3, 0, 0, 0, 0,
            0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 3, 0, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0,
            2, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 3, 0, 2, 0, 0, 1, 3, 1, 0, 0, 1, 3, 0, 0, 0, 0, 0, 0, 0, 1,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 3, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 3, 1, 3, 0, 0, 1, 3, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 2, 3, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 0, 1, 0, 0, 0, 0, 2, 2, 0, 0,
            2, 0, 0, 0
        };

        int pass = 0;
        int pixelsRemoved;
        int pix;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int imageLength = xDim * yDim * zDim;

        short[] tempBuffer;
        

        for (pix = 0; pix < imageLength; pix++) {
            processBuffer[pix] = 0;
        }

        for (int i = 0; (i < zDim) && !threadStopped; i++) {
        	fireProgressStateChanged(Math.round(((float) i) / ((float) zDim) * 100), null, 
        			"Skeletonizing Slice " + (i + 1));
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
                // 
                
                
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
     * Dilates a boolean, unsigned byte or unsigned short image using the indicated kernel and the indicated number of
     * executions.
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
        short value = 0;
        int i, j, pix, count;
        int offsetX, offsetY, offsetZ;
        int offsetXU;
        int startX, startY;
        int endX, endY;

        int xDim = srcImage.getExtents()[0];
        int zDim = srcImage.getExtents()[2];

        int halfKDim = kDim / 2;
        int stepY = kDim * xDim;
        short[] tempBuffer;
/*
        if  {
            fireProgressStateChanged("Dilating image ...");
        }

        if  {
            fireProgressStateChanged(0);
        }
*/
        for (pix = 0; pix < imgLength; pix++) {
            processBuffer[pix] = 0;
        }

        for (c = 0; (c < iters) && !threadStopped; c++) {

            for (int curSlice = 0; curSlice < zDim; curSlice++) {
                offsetZ = curSlice * sliceLength;

                for (pix = offsetZ; (pix < (offsetZ + sliceLength)) && !threadStopped; pix++) {
/*
                    try {

                        if (((((c * offsetZ) + pix) % mod) == 0)) {
                            fireProgressStateChanged(Math.round((pix + 1 + (c * offsetZ)) /
                                                                   (iters * (float) sliceLength * zDim) * 100),
                                                    runningInSeparateThread);
                        }
                    } catch (NullPointerException npe) {

                        if (threadStopped) {
                            Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                              Preferences.DEBUG_ALGORITHM);
                        }
                    }
*/
                    if (entireImage || mask.get(pix)) {
                        value = imgBuffer[pix];

                        if (value != 0) {
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

                                    if (kernel.get(count) == true) {
                                        processBuffer[i] = value;
                                    }

                                    count++;
                                }
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
     * Erodes a boolean or unsigned byte or unsigned short image using the indicated kernel and the indicated number of
     * executions.
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

        boolean clear;
        int c;
        short value = 0;
        int i, j, pix, count;
        int offsetX, offsetY, offsetZ;
        int offsetXU;
        int startX, startY;
        int endX, endY;

        int xDim = srcImage.getExtents()[0];
        int zDim = srcImage.getExtents()[2];

        int halfKDim = kDim / 2;
        int stepY = kDim * xDim;
        short[] tempBuffer;

        fireProgressStateChanged("Eroding image ...");
        
        int mod = (iters * sliceLength * zDim) / 20; // mod is 5 percent of length

        for (pix = 0; pix < imgLength; pix++) {
            processBuffer[pix] = 0;
        }

        for (c = 0; (c < iters) && !threadStopped; c++) {

            for (int curSlice = 0; curSlice < zDim; curSlice++) {
                offsetZ = curSlice * sliceLength;

                for (pix = offsetZ; (pix < (offsetZ + sliceLength)) && !threadStopped; pix++) {

                        if (((((c * offsetZ) + pix) % mod) == 0)) {
                            fireProgressStateChanged(Math.round((pix + 1 + (c * sliceLength * zDim)) /
                                                                   (iters * (float) sliceLength * zDim) * 100));
                        }

                    if (entireImage || mask.get(pix)) {
                        value = imgBuffer[pix];

                        if (imgBuffer[pix] == 0) {
                            clear = true;
                        } else {
                            clear = false;
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

kernelLoop:
                            for (j = startY; j < endY; j += xDim) {
                                startX = j + offsetX;
                                endX = j + offsetXU;

                                for (i = startX; i < endX; i++) {

                                    if ((kernel.get(count) == true) && (imgBuffer[i] == 0)) {
                                        clear = true;

                                        break kernelLoop;
                                    }

                                    count++;
                                }
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
     * Fill the holes inside the cell region blocks.
     *
     * @param  returnFlag  if true then this operation is a step in the morph process (i.e. close)
     */
    private void fillHoles(boolean returnFlag) {
        if (threadStopped) {
            finalize();

            return;
        }

        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int sliceSize = xDim * yDim;
        int offset;
        short floodValue = 2;
        int z;

        fireProgressStateChanged("Filling holes in image ...");

        for (z = 0; z < zDim; z++) {
            fireProgressStateChanged(z * 100/zDim);
            offset = z * sliceSize;
            // Fill the boundary of the image with value 0, which represent the seed value
            int i = 0;

            // top boundary
            for (i = 0; i < xDim; i++) {
                imgBuffer[offset + i] = 0;
            }
    
            // bottom boundary
            for (i = (yDim - 1) * xDim; i < (yDim * xDim); i++) {
                imgBuffer[offset + i] = 0;
            }
    
            // left boundary
            for (i = 0; i < sliceSize; i = i + xDim) {
                imgBuffer[offset + i] = 0;
            }
    
            // right boundary
            for (i = xDim; i < sliceSize; i = i + xDim) {
                imgBuffer[offset + i - 1] = 0;
            }
    
            // region grow to fill the holes inside the cell region block.
            fillHolesRegion(offset, floodValue, imgBuffer[offset]);
    
            // System.arraycopy(imgBuffer,0, processBuffer, 0, imgBuffer.length);
            // if THREAD stopped already, then dump out!
            if (threadStopped) {
                finalize();
    
                return;
            }
        } // for (z = 0; z < zDim; z++)

        try {

            if ((srcImage.getType() == ModelStorageBase.BOOLEAN) || (srcImage.getType() == ModelStorageBase.BYTE) ||
                    (srcImage.getType() == ModelStorageBase.UBYTE) || (srcImage.getType() == ModelStorageBase.SHORT)) {
                    srcImage.reallocate(ModelImage.USHORT);
                }

            srcImage.importData(0, imgBuffer, true);
        } catch (IOException error) {
            displayError("Algorithm Morphology25D: Image(s) locked");
            setCompleted(false);
            

            return;
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Morphology25D: Out of memory");
            setCompleted(false);
            

            return;
        }

        if (returnFlag == true) {
            return;
        }

        
        setCompleted(true);

    }
    
    /**
     * 2D flood fill that fill the holes insize the cell region block.
     *
     * @param  stIndex     the starting index of the seed point
     * @param  floodValue  the value to flood the region with
     * @param  objValue    object ID value that idenditifies the flood region.
     */
    private void fillHolesRegion(int stIndex, short floodValue, short objValue) {
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;
        int x, y, z;
        int indexZ, indexY;
        int pixCount = 0;

        Point pt;
        Point tempPt;
        z = stIndex/sliceSize;
        indexZ = z * sliceSize;
        Point seedPt = new Point((stIndex % sliceSize) % xDim, (stIndex % sliceSize) / xDim);
        Stack<Point> stack = new Stack<Point>();
        

        if (imgBuffer[indexZ + (seedPt.y * xDim) + seedPt.x] == objValue) {
            stack.push(seedPt);
            imgBuffer[indexZ + (seedPt.y * xDim) + seedPt.x] = floodValue;

            // While loop mark the back ground region with value 2.
            while (!stack.empty()) {
                pt = (Point) stack.pop();
                x = pt.x;
                y = pt.y;

                indexY = y * xDim;

                // voxel itself
                if (imgBuffer[indexZ + indexY + x] == objValue) {
                    imgBuffer[indexZ + indexY + x] = floodValue;
                }

                pixCount++;

                // checking on the voxel's six neighbors
                if ((x + 1) < xDim) {

                    if (imgBuffer[indexZ + indexY + x + 1] == objValue) {
                        tempPt = new Point(x + 1, y);
                        stack.push(tempPt);
                    }
                }

                if ((x - 1) >= 0) {

                    if (imgBuffer[indexZ + indexY + x - 1] == objValue) {
                        tempPt = new Point(x - 1, y);
                        stack.push(tempPt);
                    }
                }

                if ((y + 1) < yDim) {

                    if (imgBuffer[indexZ + ((y + 1) * xDim) + x] == objValue) {
                        tempPt = new Point(x, y + 1);
                        stack.push(tempPt);
                    }
                }

                if ((y - 1) >= 0) {

                    if (imgBuffer[indexZ + ((y - 1) * xDim) + x] == objValue) {
                        tempPt = new Point(x, y - 1);
                        stack.push(tempPt);
                    }
                }
            }
        }

        // Fill the pixels with value 2 to 0, else to 1. Fill holes.
        for (int i = indexZ; i < indexZ + sliceSize; i++) {

            if (imgBuffer[i] == 2) {
                imgBuffer[i] = 0;
            } else {
                imgBuffer[i] = 1;
            }
        }
    }



    /**
     * This method returns whether or not pix is the index of an endpoint in tmpBuffer (it is assumed that location pix
     * is not the intensity of the background in tmpBuffer).
     *
     * @param   pix        the index of a non-zero pixel in tmpBuffer
     * @param   tmpBuffer  the image
     *
     * @return  DOCUMENT ME!
     */
    private boolean isEndpoint(int pix, short[] tmpBuffer) {
        short p1, p2, p3, p4, p6, p7, p8, p9;
        int xDim = srcImage.getExtents()[0];
        short bgColor = 0;

        p1 = tmpBuffer[pix - xDim - 1];
        p2 = tmpBuffer[pix - xDim];
        p3 = tmpBuffer[pix - xDim + 1];
        p4 = tmpBuffer[pix - 1];
        p6 = tmpBuffer[pix + 1];
        p7 = tmpBuffer[pix + xDim - 1];
        p8 = tmpBuffer[pix + xDim];
        p9 = tmpBuffer[pix + xDim + 1];

        if ((p2 == bgColor) && (p3 == bgColor) && (p4 != bgColor) && (p6 == bgColor) && (p8 == bgColor) &&
                (p9 == bgColor)) {
            return true;
        } else if ((p2 != bgColor) && (p4 == bgColor) && (p6 == bgColor) && (p7 == bgColor) && (p8 == bgColor) &&
                       (p9 == bgColor)) {
            return true;
        } else if ((p1 == bgColor) && (p2 == bgColor) && (p4 == bgColor) && (p6 != bgColor) && (p7 == bgColor) &&
                       (p8 == bgColor)) {
            return true;
        } else if ((p1 == bgColor) && (p2 == bgColor) && (p3 == bgColor) && (p4 == bgColor) && (p6 == bgColor) &&
                       (p8 != bgColor)) {
            return true;
        } else if ((p1 != bgColor) && (p2 == bgColor) && (p3 == bgColor) && (p4 == bgColor) && (p6 == bgColor) &&
                       (p7 == bgColor) && (p8 == bgColor) && (p9 == bgColor)) {
            return true;
        } else if ((p1 == bgColor) && (p2 == bgColor) && (p3 != bgColor) && (p4 == bgColor) && (p6 == bgColor) &&
                       (p7 == bgColor) && (p8 == bgColor) && (p9 == bgColor)) {
            return true;
        } else if ((p1 == bgColor) && (p2 == bgColor) && (p3 == bgColor) && (p4 == bgColor) && (p6 == bgColor) &&
                       (p7 != bgColor) && (p8 == bgColor) && (p9 == bgColor)) {
            return true;
        } else if ((p1 == bgColor) && (p2 == bgColor) && (p3 == bgColor) && (p4 == bgColor) && (p6 == bgColor) &&
                       (p7 == bgColor) && (p8 == bgColor) && (p9 != bgColor)) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Generates a cicular kernel of a specific diameter.
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

    /**
     * This a thinning algorithm used to do half of one layer of thinning (which layer is dictated by whether pass is
     * even or odd).
     *
     * @param   pass   the number pass this execution is on the image
     * @param   slice  the slice number being worked on (starting with 0);
     * @param   table  the table to lookup whether to delete the pixel or let it stay.
     *
     * @return  DOCUMENT ME!
     */
    private int thin(int pass, int slice, int[] table) {
        int p1, p2, p3, p4, p5, p6, p7, p8, p9;
        int bgColor = 0;
        int pix;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;

        int v, index, code;
        int pixelsRemoved = 0;
        int pixInSlice;

        for (pix = slice * sliceSize; pix < ((slice + 1) * sliceSize); pix++) {

            if (entireImage || mask.get(pix)) {
                pixInSlice = pix % sliceSize;
                p5 = imgBuffer[pix] & 0xff;
                v = p5;

                if (v != bgColor) {

                    if ((pixInSlice - xDim - 1) < 0) {
                        p1 = bgColor;
                    } else {
                        p1 = imgBuffer[pix - xDim - 1] & 0xff;
                    }

                    if ((pixInSlice - xDim) < 0) {
                        p2 = bgColor;
                    } else {
                        p2 = imgBuffer[pix - xDim] & 0xff;
                    }

                    if (((pixInSlice - xDim + 1) < 0) || ((pixInSlice - xDim + 1) >= sliceSize)) {
                        p3 = bgColor;
                    } else {
                        p3 = imgBuffer[pix - xDim + 1] & 0xff;
                    }

                    if ((pixInSlice - 1) < 0) {
                        p4 = bgColor;
                    } else {
                        p4 = imgBuffer[pix - 1] & 0xff;
                    }

                    if ((pixInSlice + 1) >= sliceSize) {
                        p6 = bgColor;
                    } else {
                        p6 = imgBuffer[pix + 1] & 0xff;
                    }

                    if (((pixInSlice + xDim - 1) < 0) || ((pixInSlice + xDim - 1) >= sliceSize)) {
                        p7 = bgColor;
                    } else {
                        p7 = imgBuffer[pix + xDim - 1] & 0xff;
                    }

                    if ((pixInSlice + xDim) >= sliceSize) {
                        p8 = bgColor;
                    } else {
                        p8 = imgBuffer[pix + xDim] & 0xff;
                    }

                    if ((pixInSlice + xDim + 1) >= sliceSize) {
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

                    if ((pass & 1) == 1) { // odd pass

                        if ((code == 2) || (code == 3)) {
                            v = bgColor;
                            pixelsRemoved++;
                        }
                    } else { // even pass

                        if ((code == 1) || (code == 3)) {
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

}
