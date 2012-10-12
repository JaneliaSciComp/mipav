package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;

import java.util.*;


/**
 * Two-Dimensional mathmatical morphology class. Kernels of 3x3 (4 or 8 connected) and 5x5 (12 connected) are available
 * or user defined kernels can be supplied.
 *
 * <p>Methods include:</p>
 *
 * <ul>
 *   <li>Background Distance Map</li>
 *   <li>close</li>
 *   <li>Delete objects</li>
 *   <li>dilate</li>
 *   <li>Distance Map</li>
 *   <li>erode</li>
 *   <li>fill holes</li>
 *   <li>find edges</li>
 *   <li>Identify objects</li>
 *   <li>Morphological gradient</li>
 *   <li>open</li>
 *   <li>Particle Analysis</li>
 *   <li>Skeletonize with pruning option</li>
 *   <li>ultimate erode</li>
 * </ul>
 *
 * @version  1.0 March 15, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class AlgorithmMorphology2D extends AlgorithmBase {

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
    
    /** DOCUMENT ME! */
    public static final int DISTANCE_MAP_FOR_SHAPE_INTERPOLATION = 14;
    
    public static final int MORPHOLOGICAL_GRADIENT = 15;

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

    /** algorithm type (i.e. erode, dilate) */
    private int algorithm;

    /** kernel diameter. */
    private float circleDiameter;

    /** Erosion kernel diameter. */
    private float circleDiameterErode;

    /** DOCUMENT ME! */
    private float[] distanceMap = null;

    /** Edge type. */
    private int edgingType;

    /** if true, indicates that the VOIs should NOT be used and that entire image should be processed. */
    private boolean entireImage;

    /** imgBuffer that hold pixel value for the 2D slice. */
    private short[] imgBuffer;
    
    private short[] imgBuffer2;

    /** Dilation iteration times. */
    private int iterationsD;

    /** Erosion iteration times. */
    private int iterationsE;

    /** DOCUMENT ME! */
    private int iterationsOpen;

    /** Kernel dimension. */
    private int kDim;

    /** Kernel for both erosion and dilation. */
    private BitSet kernel;

    /** kernel size (i.e. connectedness) */
    @SuppressWarnings("unused")
    private int kernelType;

    /** Erosion kernel type. */
    private int kernelTypeErode;

    /** maximum, minimum size of objects. */
    private int min = 1, max = 1000000;

    /** Number pixels to prune. */
    private int numPruningPixels;

    /** Vector that holding the current available objects in the 2D image. */
    private Vector<intObject> objects = new Vector<intObject>();

    /** DOCUMENT ME! */
    private float pixDist = 1;

    /** intermediate processing buffer, same size with imgBuffer. */
    private short[] processBuffer;

    /** Vector that hold the prune seeding pixels. */
    private Vector<Integer> pruneSeeds = new Vector<Integer>();

    /** Flag to show frame during each algorithm method call. */
    private boolean showFrame = false;

    /** DOCUMENT ME! */
    private Point[] ultErodeObjects = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmMorphology2D object.
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
    public AlgorithmMorphology2D(ModelImage srcImg, int kernelType, float circleDiameter, int method, int iterD,
                                 int iterE, int pruningPix, int edType, boolean entireImage) {
        super(null, srcImg);
        setAlgorithm(method);

        iterationsD = iterD;
        iterationsE = iterE;

        numPruningPixels = pruningPix;
        edgingType = edType;
        this.entireImage = entireImage;
        this.kernelType = kernelType;

        if (kernelType == SIZED_CIRCLE) {
            this.circleDiameter = circleDiameter;
            makeCircularKernel(circleDiameter);
        } else {
            makeKernel(kernelType);
        }
    }

    /**
     * Creates a new AlgorithmMorphology2D object.
     *
     * @param  srcImg               source image model
     * @param  kernelType           dilation kernel size (i.e. connectedness)
     * @param  circleDiameter       dilation only valid if kernelType == SIZED_CIRCLE and represents the width of a
     *                              circle in the resolution of the image
     * @param  kernelTypeErode      kernel size (i.e. connectedness) of erosion
     * @param  circleDiameterErode  Erosion only valid if kernelType == SIZED_CIRCLE and represents the width of a
     *                              circle in the resolution of the image
     * @param  method               setup the algorithm method (i.e. erode, dilate)
     * @param  iterOpen             number of times to dilate
     * @param  iterE                number of times to erode
     * @param  pruningPix           the number of pixels to prune
     * @param  edType               the type of edging to perform (inner or outer)
     * @param  entireImage          if true, indicates that the VOIs should NOT be used and that entire image should be
     *                              processed
     * @param  _showFrame           if true, indicates that show image frame after each algorithm be processed
     */
    public AlgorithmMorphology2D(ModelImage srcImg, int kernelType, float circleDiameter, int kernelTypeErode,
                                 float circleDiameterErode, int method, int iterOpen, int iterE, int pruningPix,
                                 int edType, boolean entireImage, boolean _showFrame) {
        super(null, srcImg);
        setAlgorithm(method);

        iterationsOpen = iterOpen;
        iterationsE = iterE;
        numPruningPixels = pruningPix;
        edgingType = edType;
        this.entireImage = entireImage;
        this.kernelType = kernelType;
        this.showFrame = _showFrame;

        if (kernelType == SIZED_CIRCLE) {
            this.circleDiameter = circleDiameter;
            makeCircularKernel(circleDiameter);
        } else {
            makeKernel(kernelType);
        }

        this.kernelTypeErode = kernelTypeErode;
        this.circleDiameterErode = circleDiameterErode;

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Static method that generates a boundary of a binary object. (aka. turtle algorithm: if 1 turn left, step if 0
     * turn right, step)
     *
     * @param   image    input image where binary object is located.
     * @param   xDim     the x dimension of the image
     * @param   yDim     the y dimension of the image
     * @param   startPt  starting point
     *
     * @return  returns boundary of the object as Polygon
     */
    public static final Polygon genContour(BitSet image, int xDim, int yDim, Point startPt) {

        int N = 0, E = 1, S = 2, W = 3;
        int dir = E;
        int index = (startPt.y * xDim) + startPt.x;
        int xCur = startPt.x;
        int yCur = startPt.y;

        // Without xInc and yInc the lower and right boundaries are 1 pixel too far inside
        // the region.
        int xInc = 0;
        int yInc = 0;
        int startIndex = index;
        Polygon resultGon = new Polygon();

        do {

            if ((xCur >= 0) && (xCur < xDim) && (yCur >= 0) && (yCur < yDim) && (image.get(index))) {
                resultGon.addPoint(xCur + xInc, yCur + yInc);

                if (dir == N) {
                    xCur--;
                    dir = W;

                    if ((xCur - 1) < xDim) {
                        xInc = 1;
                    } else {
                        xInc = 0;
                    }
                } else if (dir == E) {
                    yCur--;
                    dir = N;

                    if ((yCur - 1) < yDim) {
                        yInc = 1;
                    } else {
                        yInc = 0;
                    }
                } else if (dir == S) {
                    xCur++;
                    ;
                    dir = E;
                    xInc = 0;
                } else {
                    yCur++;
                    dir = S;
                    yInc = 0;
                }
            } else {

                if (dir == N) {
                    xCur++;
                    dir = E;
                    xInc = 0;
                } else if (dir == E) {
                    yCur++;
                    dir = S;
                    yInc = 0;
                } else if (dir == S) {
                    xCur--;
                    dir = W;

                    if ((xCur - 1) < xDim) {
                        xInc = 1;
                    } else {
                        xInc = 0;
                    }
                } else {
                    yCur--;
                    dir = N;

                    if ((yCur - 1) < yDim) {
                        yInc = 1;
                    } else {
                        yInc = 0;
                    }
                }
            }

            index = xCur + (yCur * xDim);
        } while (((index != startIndex) || (resultGon.npoints < 4)) && (resultGon.npoints < 10000));

        if (resultGon.npoints == 10000) {
            Preferences.debug("Error - genContour has not completed at resultGon.npoints = 10,000\n", 
            		Preferences.DEBUG_ALGORITHM);
        }

        return resultGon;
    }

    /**
     * Static method that generates a boundary of a binary object. (aka. turtle algorithm: if 1 turn left, step if 0
     * turn right, step)
     *
     * @param   image    input image (Boolean) where binary object is located.
     * @param   xDim     the x dimension of the image
     * @param   yDim     the y dimension of the image
     * @param   startPt  starting point
     *
     * @return  returns boundary of the object as Polygon
     */
    public static final Polygon genContour(ModelImage image, int xDim, int yDim, Point startPt) {

        int N = 0, E = 1, S = 2, W = 3;
        int dir = E;
        int index = (startPt.y * xDim) + startPt.x;
        int xCur = startPt.x;
        int yCur = startPt.y;

        // Without xInc and yInc the lower and right boundaries are 1 pixel too far inside
        // the region.
        int xInc = 0;
        int yInc = 0;
        int startIndex = index;
        Polygon resultGon = new Polygon();

        do {

            if ((xCur >= 0) && (xCur < xDim) && (yCur >= 0) && (yCur < yDim) && (image.getBoolean(index))) {
                resultGon.addPoint(xCur + xInc, yCur + yInc);

                if (dir == N) {
                    xCur--;
                    dir = W;

                    if ((xCur - 1) < xDim) {
                        xInc = 1;
                    } else {
                        xInc = 0;
                    }
                } else if (dir == E) {
                    yCur--;
                    dir = N;

                    if ((yCur - 1) < yDim) {
                        yInc = 1;
                    } else {
                        yInc = 0;
                    }
                } else if (dir == S) {
                    xCur++;
                    ;
                    dir = E;
                    xInc = 0;
                } else {
                    yCur++;
                    dir = S;
                    yInc = 0;
                }
            } else {

                if (dir == N) {
                    xCur++;
                    dir = E;
                    xInc = 0;
                } else if (dir == E) {
                    yCur++;
                    dir = S;
                    yInc = 0;
                } else if (dir == S) {
                    xCur--;
                    dir = W;

                    if ((xCur - 1) < xDim) {
                        xInc = 1;
                    } else {
                        xInc = 0;
                    }
                } else {
                    yCur--;
                    dir = N;

                    if ((yCur - 1) < yDim) {
                        yInc = 1;
                    } else {
                        yInc = 0;
                    }
                }
            }

            index = xCur + (yCur * xDim);
        } while (((index != startIndex) || (resultGon.npoints < 4)) && (resultGon.npoints < 10000));

        if (resultGon.npoints == 10000) {
            Preferences.debug("Error - genContour has not completed at resultGon.npoints = 10,000\n", 
            		Preferences.DEBUG_ALGORITHM);
        }

        return resultGon;
    }

    /**
     * Static method that generates a boundary of a general object. (aka. turtle algorithm: if 1 turn left, step if 0
     * turn right, step)
     *
     * @param   xDimE    the x dimension of the image
     * @param   yDimE    the y dimension of the image
     * @param   startPt  starting point
     * @param   maskA    mask of the object
     *
     * @return  returns boundary of the object as Polygon
     */
    public static final Polygon genContour(int xDimE, int yDimE, Point startPt, BitSet maskA) {

        int N = 0, E = 1, S = 2, W = 3;
        int dir = E;
        int index = (startPt.y * xDimE) + startPt.x;
        int xCur = startPt.x;
        int yCur = startPt.y;
        int xLast = startPt.x;
        int yLast = startPt.y;

        int startIndex = index;
        Polygon resultGon = new Polygon();
        int xDim = xDimE / 2;
        int yDim = yDimE / 2;
        int smallLength = xDim * yDim;
        int backAtStart = 0;
        BitSet mask;
        Polygon expandedPolygon;
        int i;
        int xpos;
        int ypos;

        try {
            mask = new BitSet(smallLength);
        } catch (OutOfMemoryError e) {
            return null;
        }

        mask.clear();

        do {

            if ((xCur >= 0) && (xCur < xDimE) && (yCur >= 0) && (yCur < yDimE) && (maskA.get(index))) {

                if (((xCur % 2) == 0) && ((yCur % 2) == 0) && (xCur != xLast) && (yCur != yLast)) {

                    if ((maskA.get(xLast + (yCur * xDimE))) && (!mask.get((xLast / 2) + (xDim * (yCur / 2))))) {
                        resultGon.addPoint(xLast / 2, yCur / 2);
                        mask.set((xLast / 2) + (xDim * (yCur / 2)));
                    } else if ((maskA.get(xCur + (yLast * xDimE))) && (!mask.get((xCur / 2) + (xDim * (yLast / 2))))) {
                        resultGon.addPoint(xCur / 2, yLast / 2);
                        mask.set((xCur / 2) + (xDim * (yLast / 2)));
                    }
                }

                if (((xCur % 2) == 0) && ((yCur % 2) == 0)) {
                    xLast = xCur;
                    yLast = yCur;

                    if (!mask.get((xCur / 2) + (xDim * (yCur / 2)))) {
                        resultGon.addPoint(xCur / 2, yCur / 2);
                        mask.set((xCur / 2) + (xDim * (yCur / 2)));
                    }
                }

                if (dir == N) {
                    xCur--;
                    dir = W;
                } else if (dir == E) {
                    yCur--;
                    dir = N;
                } else if (dir == S) {
                    xCur++;
                    ;
                    dir = E;
                } else {
                    yCur++;
                    dir = S;
                }
            } else {

                if (dir == N) {
                    xCur++;
                    dir = E;
                } else if (dir == E) {
                    yCur++;
                    dir = S;
                } else if (dir == S) {
                    xCur--;
                    dir = W;
                } else {
                    yCur--;
                    dir = N;
                }
            }

            index = xCur + (yCur * xDimE);

            if (index == startIndex) {

                // Allow once at beginning and N, E, S, and W reentries
                backAtStart++;
            }
        } while (((index != startIndex) || (resultGon.npoints < 4)) && (resultGon.npoints < 10000) && (backAtStart < 6));

        if (((xCur % 2) == 0) && ((yCur % 2) == 0) && (xCur != xLast) && (yCur != yLast)) {

            if ((maskA.get(xLast + (yCur * xDimE))) && (!mask.get((xLast / 2) + (xDim * (yCur / 2))))) {
                resultGon.addPoint(xLast / 2, yCur / 2);
                mask.set((xLast / 2) + (xDim * (yCur / 2)));
            } else if ((maskA.get(xCur + (yLast * xDimE))) && (!mask.get((xCur / 2) + (xDim * (yLast / 2))))) {
                resultGon.addPoint(xCur / 2, yLast / 2);
                mask.set((xCur / 2) + (xDim * (yLast / 2)));
            }
        }

        if (((xCur % 2) == 0) && ((yCur % 2) == 0) && (!mask.get((xCur / 2) + (xDim * (yCur / 2))))) {
            resultGon.addPoint(xCur / 2, yCur / 2);
        }

        if (resultGon.npoints == 10000) {
            Preferences.debug("Error - genContour has not completed at resultGon.npoints = 10,000", 
            		Preferences.DEBUG_ALGORITHM);
        }
        
        expandedPolygon = new Polygon();
        for (i = 0; i < resultGon.npoints; i++) {
            xpos = resultGon.xpoints[i];
            ypos = resultGon.ypoints[i];
            if ((xpos > 0) && (ypos > 0) && (resultGon.contains(xpos-1,ypos-1)) &&
                    (!resultGon.contains(xpos-1,ypos)) &&
                    (!resultGon.contains(xpos,ypos-1))) {
                expandedPolygon.addPoint(xpos+1,ypos+1);
            }
            else if ((xpos > 0) && (resultGon.contains(xpos-1,ypos))) {
                expandedPolygon.addPoint(xpos+1, ypos);
            }
            else if ((ypos > 0) && (resultGon.contains(xpos, ypos-1))) {
                expandedPolygon.addPoint(xpos, ypos+1);
            }
            else {
                expandedPolygon.addPoint(xpos, ypos);
            }
        }

        return expandedPolygon;
    }

    /**
     * Static method that generates a boundary of a general object. (aka. turtle algorithm: if 1 turn left, step if 0
     * turn right, step)
     *
     * @param   image        input image where <<short>> object is located.
     * @param   xDim         the x dimension of the image
     * @param   yDim         the y dimension of the image
     * @param   startPt      starting point
     * @param   objectValue  value of the object
     *
     * @return  returns boundary of the object as Polygon
     */
    public static final Polygon genContour(short[] image, int xDim, int yDim, Point startPt, short objectValue) {

        int N = 0, E = 1, S = 2, W = 3;
        int dir = E;
        int index = (startPt.y * xDim) + startPt.x;
        int xCur = startPt.x;
        int yCur = startPt.y;

        // Without xInc and yInc the lower and right boundaries are 1 pixel too far inside
        // the region.
        int xInc = 0;
        int yInc = 0;
        int startIndex = index;
        Polygon resultGon = new Polygon();

        do {

            if ((xCur >= 0) && (xCur < xDim) && (yCur >= 0) && (yCur < yDim) && (image[index] == objectValue)) {
                resultGon.addPoint(xCur + xInc, yCur + yInc);

                if (dir == N) {
                    xCur--;
                    dir = W;

                    if ((xCur - 1) < xDim) {
                        xInc = 1;
                    } else {
                        xInc = 0;
                    }
                } else if (dir == E) {
                    yCur--;
                    dir = N;

                    if ((yCur - 1) < yDim) {
                        yInc = 1;
                    } else {
                        yInc = 0;
                    }
                } else if (dir == S) {
                    xCur++;
                    ;
                    dir = E;
                    xInc = 0;
                } else {
                    yCur++;
                    dir = S;
                    yInc = 0;
                }
            } else {

                if (dir == N) {
                    xCur++;
                    dir = E;
                    xInc = 0;
                } else if (dir == E) {
                    yCur++;
                    dir = S;
                    yInc = 0;
                } else if (dir == S) {
                    xCur--;
                    dir = W;

                    if ((xCur - 1) < xDim) {
                        xInc = 1;
                    } else {
                        xInc = 0;
                    }
                } else {
                    yCur--;
                    dir = N;

                    if ((yCur - 1) < yDim) {
                        yInc = 1;
                    } else {
                        yInc = 0;
                    }
                }
            }

            index = xCur + (yCur * xDim);
        } while (((index != startIndex) || (resultGon.npoints < 4)) && (resultGon.npoints < 10000));

        if (resultGon.npoints == 10000) {
            Preferences.debug("Error - genContour has not completed at resultGon.npoints = 10,000\n", 
            		Preferences.DEBUG_ALGORITHM);
        }

        return resultGon;
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        int i;
        kernel = null;
        processBuffer = null;
        imgBuffer = null;
        srcImage = null;

        if (objects != null) {
            objects.removeAllElements();
            objects = null;
        }

        if (ultErodeObjects != null) {

            for (i = 0; i < ultErodeObjects.length; i++) {
                ultErodeObjects[i] = null;
            }

            ultErodeObjects = null;
        }

        distanceMap = null;
        super.finalize();
    }

    /**
     * Labels each object in an image with a different integer value.
     *
     * @param  returnFlag  if true then this operation is a step in the morph process (i.e. close)
     */
    public void identifyObjects(boolean returnFlag) {

        // if THREAD stopped already, then dump out!
        if (threadStopped) {
            finalize();

            return;
        }

        int pix;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;
        short floodValue = 1;
        int count = 0;

        deleteObjects(min, max, true);

        fireProgressStateChanged(0, null, "Identifing objects ...");


        int mod = sliceSize / 50; // mod is 2 percent of length

        for (pix = 0; (pix < sliceSize) && !threadStopped; pix++) {

            try {

                if (((pix % mod) == 0)) {
                    fireProgressStateChanged(Math.round((pix + 1) / ((float) sliceSize) * 100));
                }
            } catch (NullPointerException npe) {

                if (threadStopped) {
                    Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                      Preferences.DEBUG_ALGORITHM);
                }
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

        // if THREAD stopped already, then dump out!
        if (threadStopped) {
            finalize();

            return;
        }

        ViewUserInterface.getReference().getMessageFrame().getData().append("\nIdentified " + objects.size() +
                                                                            " objects. \n");

        String mStr;
        int measure;
        float area;

        measure = srcImage.getFileInfo(0).getUnitsOfMeasure(0);

        if (measure == Unit.INCHES.getLegacyNum()) {
            mStr = " inches^2";
        } else if (measure == Unit.MILS.getLegacyNum()) {
            mStr = " mil^2";
        } else if (measure == Unit.ANGSTROMS.getLegacyNum()) {
            mStr = " A^2";
        } else if (measure == Unit.NANOMETERS.getLegacyNum()) {
            mStr = " nm^2";
        } else if (measure == Unit.MICROMETERS.getLegacyNum()) {
            mStr = " um^2";
        } else if (measure == Unit.MILLIMETERS.getLegacyNum()) {
            mStr = " mm^2";
        } else if (measure == Unit.CENTIMETERS.getLegacyNum()) {
            mStr = " cm^2";
        } else if (measure == Unit.METERS.getLegacyNum()) {
            mStr = " m^2";
        } else if (measure == Unit.KILOMETERS.getLegacyNum()) {
            mStr = " km^2";
        } else if (measure == Unit.MILES.getLegacyNum()) {
            mStr = " miles^2";
        } else {
            mStr = "Unknown";
        }

        ViewUserInterface.getReference().getMessageFrame().getData().append(" Object \t# of pixels\tArea(" + mStr +
                                                                            ")\n");

        for (int i = 0; i < objects.size(); i++) {
            area = ((intObject) (objects.elementAt(i))).size * srcImage.getFileInfo(0).getResolutions()[0] *
                       srcImage.getFileInfo(0).getResolutions()[1];
            ViewUserInterface.getReference().getMessageFrame().getData().append("    " + (i + 1) + "\t" + +((intObject)
                                                                                                                (objects.elementAt(i))).size +
                                                                                "\t" + area + "\n");
        }

        if (returnFlag == true) {
            return;
        }

        try {

            if (threadStopped) {
                finalize();

                return;
            }

            if ((srcImage.getType() == ModelStorageBase.BOOLEAN) || (srcImage.getType() == ModelStorageBase.BYTE) ||
                (srcImage.getType() == ModelStorageBase.UBYTE) || (srcImage.getType() == ModelStorageBase.SHORT)) {
                srcImage.reallocate(ModelImage.USHORT);
            }

            srcImage.importData(0, processBuffer, true);
        } catch (IOException error) {
            displayError("Algorithm Morphology2D: Image(s) locked");
            setCompleted(false);


            return;
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Morphology2D: Out of memory");
            setCompleted(false);


            return;
        }

        setCompleted(true);
    }

    /**
     * This method is to be applied on skeletonized images: it removes all branches which are iter or less pixels in
     * length.
     *
     * @param  iter        the threshold number of pixels for the maximum length of a removed branch to be
     * @param  returnFlag  if true then this operation is a step in the morph process (i.e. close)
     */
    public void prune(int iter, boolean returnFlag) {

        // if THREAD stopped already, then dump out!
        if (threadStopped) {
            return;
        }

        short p1, p2, p3, p4, p5, p6, p7, p8, p9;
        short bgColor = 0;
        short value;
        int i, pix;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;
        Vector<Integer> endpoints;
        Vector<Integer> branch;
        Vector <Vector<Integer>>branchesVector;
        short[] tempBuffer;

        if (iter < 1) {
            return;
        }

        try {
            tempBuffer = new short[imgBuffer.length];
            endpoints = new Vector<Integer>();
            branchesVector = new Vector<Vector<Integer>>();
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Morphology2D: Out of memory");
            setCompleted(false);


            return;
        }

        try {

            fireProgressStateChanged("Pruning image ...");


            fireProgressStateChanged(0);

        } catch (NullPointerException npe) {

            if (threadStopped) {
                Preferences.debug("Killed progressbar, in between checking threadstopped.",
                                  Preferences.DEBUG_ALGORITHM);
            }
        }

        // sets the intensity of border points to 0
        for (pix = 0; pix < xDim; pix++) {
            imgBuffer[pix] = 0;
        }

        for (pix = (xDim * (yDim - 1)); pix < sliceSize; pix++) {
            imgBuffer[pix] = 0;
        }

        for (pix = 0; pix <= (xDim * (yDim - 1)); pix += xDim) {
            imgBuffer[pix] = 0;
        }

        for (pix = (xDim - 1); pix < sliceSize; pix += xDim) {
            imgBuffer[pix] = 0;
        }

        for (pix = 0; pix < sliceSize; pix++) {
            processBuffer[pix] = 0;
            tempBuffer[pix] = imgBuffer[pix];

            if (imgBuffer[pix] != bgColor) {
                tempBuffer[pix] = -1; // used to represent non-zero pixels which are deeper than iter + 1 pixels from
                                      // an endpoint
            }
        }

        // if THREAD stopped already, then dump out!
        if (threadStopped) {
            setCompleted(false);


            return;
        }

        // store in processBuffer an image in which each branch's pixels
        // has intensities according to how deep within the branch they are
        // (goes (iter + 1) deep only)
        for (i = 1; (i <= (iter + 1)) && !threadStopped; i++) {

            for (int y = 1; (y < (yDim - 1)) && !threadStopped; y++) {

                for (int x = 1; (x < (xDim - 1)) && !threadStopped; x++) {
                    pix = (y * xDim) + x;

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

        // for each occurrence of 1 (the endpoint of a branch),
        // a new element is added to branchesVector
        for (int y = 1; (y < (yDim - 1)) && !threadStopped; y++) {

            for (int x = 1; (x < (xDim - 1)) && !threadStopped; x++) {
                pix = (y * xDim) + x;

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

        // if THREAD stopped already, then dump out!
        if (threadStopped) {
            return;
        }

        // goes through all branches (by iterating through branchesVector),
        // and removes all branches from
        // the vector which have a length of iter or less
        int currentBranch = 0;
        int branchesDone = 0;
        int branchesSize = branchesVector.size();

        while ((branchesDone != branchesSize) && !threadStopped) {
            branch = (Vector<Integer>) branchesVector.elementAt(currentBranch);

            for (i = 2; (i <= (iter + 1)) && !threadStopped; i++) {
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

        // if THREAD stopped already, then dump out!
        if (threadStopped) {
            return;
        }

        for (pix = 0; pix < sliceSize; pix++) {

            if (processBuffer[pix] == -1) {
                tempBuffer[pix] = imgBuffer[pix];
            } else {
                tempBuffer[pix] = 0;
            }
        }

        // creates final image which contains only branches of more than iter in length
        while (!branchesVector.isEmpty() && !threadStopped) {
            branch = (Vector<Integer>) branchesVector.firstElement();
            branchesVector.removeElementAt(0);

            while (!branch.isEmpty() && !threadStopped) {
                pix = ((Integer) branch.firstElement()).intValue();
                branch.removeElementAt(0);
                tempBuffer[pix] = imgBuffer[pix];
            }
        }

        // if THREAD stopped already, then dump out!
        if (threadStopped) {
            return;
        }

        imgBuffer = tempBuffer;

        if (returnFlag == true) {
            return;
        }

        try {

            // if THREAD stopped already, then dump out!
            if (threadStopped) {
                return;
            }

            srcImage.importData(0, imgBuffer, true);
        } catch (IOException error) {
            displayError("Algorithm Morphology2D: Image(s) locked");
            setCompleted(false);


            return;
        }

        setCompleted(true);
    }

    /**
     * This method is to be applied on skeletonized images: it skeletonized each branches into a point.
     *
     * @param  returnFlag  if true then this operation is a step in the morph process (i.e. close)
     */
    public void pruneAuto(boolean returnFlag) {

        // if THREAD stopped already, then dump out!
        if (threadStopped) {
            return;
        }

        short p1, p2, p3, p4, p5, p6, p7, p8, p9;
        short bgColor = 0;
        short value;
        int i, pix;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;
        Vector<Integer> endpoints;
        Vector<Integer> branch;
        Vector<Vector<Integer>> branchesVector;
        short[] tempBuffer;

        try {
            tempBuffer = new short[imgBuffer.length];
            endpoints = new Vector<Integer>();
            branchesVector = new Vector<Vector<Integer>>();
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Morphology2D: Out of memory");
            setCompleted(false);


            return;
        }

        try {

            fireProgressStateChanged("Pruning image ...");


            fireProgressStateChanged(0);
        } catch (NullPointerException npe) {

            if (threadStopped) {
                Preferences.debug("Killed progressbar, in between checking threadstopped.",
                                  Preferences.DEBUG_ALGORITHM);
            }
        }

        // sets the intensity of border points to 0
        for (pix = 0; pix < xDim; pix++) {
            imgBuffer[pix] = 0;
        }

        for (pix = (xDim * (yDim - 1)); pix < sliceSize; pix++) {
            imgBuffer[pix] = 0;
        }

        for (pix = 0; pix <= (xDim * (yDim - 1)); pix += xDim) {
            imgBuffer[pix] = 0;
        }

        for (pix = (xDim - 1); pix < sliceSize; pix += xDim) {
            imgBuffer[pix] = 0;
        }

        for (pix = 0; pix < sliceSize; pix++) {
            processBuffer[pix] = 0;
            tempBuffer[pix] = imgBuffer[pix];

            if (imgBuffer[pix] != bgColor) {
                tempBuffer[pix] = -1; // used to represent non-zero pixels which are deeper than iter + 1 pixels from
                                      // an endpoint
            }
        }

        // if THREAD stopped already, then dump out!
        if (threadStopped) {
            setCompleted(false);


            return;
        }

        boolean prune = true;

        // store in processBuffer an image in which each branch's pixels
        // has intensities according to how deep within the branch they are
        // (goes (iter + 1) deep only)
        i = 1;

        // The deepest branch length
        int iter = 0;

        while (prune && !threadStopped) {
            prune = false;

            for (int y = 1; (y < (yDim - 1)) && !threadStopped; y++) {

                for (int x = 1; (x < (xDim - 1)) && !threadStopped; x++) {
                    pix = (y * xDim) + x;

                    if (entireImage || mask.get(pix)) {
                        p5 = tempBuffer[pix];
                        value = p5;

                        if (p5 != bgColor) {

                            if (isEndpoint(pix, tempBuffer)) {
                                value = (byte) i;
                                endpoints.addElement(new Integer(pix));
                                prune = true;
                            } else if (isSinglePoint(pix, tempBuffer)) {
                                value = (byte) i;
                                endpoints.addElement(new Integer(pix));
                                prune = true;
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

            i++;
        }

        if (showFrame) {

            ModelImage tImage = new ModelImage(ModelImage.USHORT, srcImage.getExtents(), "Check");

            try {
                tImage.importData(0, processBuffer, true);
            } catch (IOException error) {
                displayError("Algorithm Morphology2D: Image(s) locked in Dilate");
                setCompleted(false);


                return;
            }

            new ViewJFrameImage(tImage, null, null, false);
        }

        iter = i;

        // for each occurrence of 1 (the endpoint of a branch),
        // a new element is added to branchesVector
        for (int y = 1; (y < (yDim - 1)) && !threadStopped; y++) {

            for (int x = 1; (x < (xDim - 1)) && !threadStopped; x++) {
                pix = (y * xDim) + x;

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

        // if THREAD stopped already, then dump out!
        if (threadStopped) {
            return;
        }

        // goes through all branches (by iterating through branchesVector),
        // and removes all branches from
        // the vector which have a length of iter or less
        int currentBranch = 0;
        int branchesDone = 0;
        int branchesSize = branchesVector.size();
        int index;

        while ((branchesDone != branchesSize) && !threadStopped) {
            branch = (Vector<Integer>) branchesVector.elementAt(currentBranch);

            for (i = 2; (i <= (iter)) && !threadStopped; i++) {
                index = ((Integer) branch.elementAt(i - 2)).intValue();
                p1 = processBuffer[index - xDim - 1];
                p2 = processBuffer[index - xDim];
                p3 = processBuffer[index - xDim + 1];
                p4 = processBuffer[index - 1];
                p6 = processBuffer[index + 1];
                p7 = processBuffer[index + xDim - 1];
                p8 = processBuffer[index + xDim];
                p9 = processBuffer[index + xDim + 1];

                if (p1 >= (byte) i) {
                    branch.addElement(new Integer(index - xDim - 1));
                } else if (p2 >= (byte) i) {
                    branch.addElement(new Integer(index - xDim));
                } else if (p3 >= (byte) i) {
                    branch.addElement(new Integer(index - xDim + 1));
                } else if (p4 >= (byte) i) {
                    branch.addElement(new Integer(index - 1));
                } else if (p6 >= (byte) i) {
                    branch.addElement(new Integer(index + 1));
                } else if (p7 >= (byte) i) {
                    branch.addElement(new Integer(index + xDim - 1));
                } else if (p8 >= (byte) i) {
                    branch.addElement(new Integer(index + xDim));
                } else if (p9 >= (byte) i) {
                    branch.addElement(new Integer(index + xDim + 1));
                } else {
                    break;
                }
            }

            currentBranch++;
            branchesDone++;
        }

        // Find the max valued label
        int maxPix, maxIndex = 0;
        int[] pixArray = new int[branchesVector.size()];

        for (i = 0; i < branchesVector.size(); i++) {
            branch = (Vector<Integer>) branchesVector.elementAt(i);
            maxPix = 0;
            maxIndex = 0;

            for (int j = 0; j < branch.size(); j++) {
                index = ((Integer) branch.elementAt(j)).intValue();
                pix = processBuffer[index];

                if (pix > maxPix) {
                    maxPix = pix;
                    maxIndex = index;
                }
            }

            pixArray[i] = maxIndex;

        }

        // mark the neighbor
        int val, t, neighbor;
        boolean found = false;

        for (int k = 0; k < pixArray.length; k++) {
            val = pixArray[k];
            found = false;

            for (t = k + 1; t < pixArray.length; t++) {
                neighbor = pixArray[t];

                if (isNeighbor(val, neighbor) || (val == neighbor)) {
                    found = true;
                }
            }

            if (found) {
                pixArray[k] = -1;
            }
        }

        /*
         *      Preferences.debug( "pixArray : ", Preferences.DEBUG_ALGORITHM );     for ( int k = 0; k < pixArray.length; k++ ) { Preferences.debug(
         * k + " = " + pixArray[k], Preferences.DEBUG_ALGORITHM );     }
         */
        // if THREAD stopped already, then dump out!
        if (threadStopped) {
            return;
        }

        for (pix = 0; pix < sliceSize; pix++) {
            if ((entireImage) || (mask.get(pix))) {
                tempBuffer[pix] = 0;
            }
            else {
                tempBuffer[pix] = imgBuffer[pix];
            }
        }

        for (i = 0; i < pixArray.length; i++) {

            if (pixArray[i] != -1) {
                pix = pixArray[i];
                tempBuffer[pix] = imgBuffer[pix];
                pruneSeeds.add(new Integer(pix));
            }
        }

        // if THREAD stopped already, then dump out!
        if (threadStopped) {
            return;
        }

        imgBuffer = tempBuffer;

        if (showFrame) {
            ModelImage tempImage = new ModelImage(ModelImage.USHORT, srcImage.getExtents(), "PruneAuto");

            try {
                tempImage.importData(0, imgBuffer, true);
            } catch (IOException error) {
                displayError("Algorithm Morphology2D: Image(s) locked in PruneAuto");
                setCompleted(false);


                return;
            }

            new ViewJFrameImage(tempImage, null, null, false);
        }

        if (returnFlag == true) {
            return;
        }

        try {

            // if THREAD stopped already, then dump out!
            if (threadStopped) {
                return;
            }

            srcImage.importData(0, imgBuffer, true);
        } catch (IOException error) {
            displayError("Algorithm Morphology2D: Image(s) locked");
            setCompleted(false);


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

        // Source Image must be Boolean, Byte, UByte, Short or UShort
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

        // verify using a 2d image for this algorithm.
        if (srcImage.getNDims() != 2) {
            displayError("Source Image is not 2D");
            setCompleted(false);

            return;
        }

        try {
            int length = srcImage.getSliceSize();

            imgBuffer = new short[length];
            processBuffer = new short[length];
            srcImage.exportData(0, length, imgBuffer); // locks and releases lock
            fireProgressStateChanged(srcImage.getImageName(), "Morph2D ...");
        } catch (IOException error) {
            displayError("Algorithm Morphology2D: Image(s) locked");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Morphology2D: Out of memory");
            setCompleted(false);

            return;
        }

        

        int[] progressValues = getProgressValues();

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


            case PARTICLE_ANALYSIS_NEW:
                // open
                setMaxProgressValue(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 15));
                fillHoles(true);

                setProgressValues(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 15),
                                  ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 30));
                erode(true, iterationsOpen);
                setProgressValues(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 30),
                                  ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 45));
                dilate(true, iterationsOpen);

                if (kernelTypeErode == SIZED_CIRCLE) {
                    makeCircularKernel(circleDiameterErode);
                } else {
                    makeKernel(kernelTypeErode);
                }

                makeKernel(CONNECTED4);
                setProgressValues(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 45),
                                  ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 60));
                erode(true, iterationsE);

                setProgressValues(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 60),
                                  ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 75));
                skeletonize(numPruningPixels, true);
                setProgressValues(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 75),
                                  ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 80));
                pruneAuto(true);
                setProgressValues(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 80),
                                  ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 95));
                particleAnalysisNew(true);
                setMinMax(0, 1000000);
                setProgressValues(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 95),
                                  ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 100));
                identifyObjects(false);
                break;

            case FILL_HOLES:
                fillHoles(false);
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
                
            case DISTANCE_MAP_FOR_SHAPE_INTERPOLATION:
                distanceMapForShapeInterpolation(false);
                break;

            case ULTIMATE_ERODE:
                setMaxProgressValue(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 50));
                skeletonize(numPruningPixels, true);
                setProgressValues(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 50),
                                  ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 100));
                pruneAuto(false);
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
     * Sets bounds of object size used to delete objects.
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
     * @param  dist  distance in pixels. Default = 0 (i.e. do not remove any points)
     */
    public void setPixDistance(float dist) {
        pixDist = dist;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  showFrame  DOCUMENT ME!
     */
    public void setShowFrame(boolean showFrame) {
        this.showFrame = showFrame;
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

            Preferences.debug("\n Morphology2D - structuring element. \n", Preferences.DEBUG_ALGORITHM);

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
            displayError("Algorithm Morphology2D: Out of memory");
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

        // if THREAD stopped already, then dump out!
        if (threadStopped) {
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
        int sliceSize = xDim * yDim;
        short[] tempBuffer;


        fireProgressStateChanged("Skeletonize image ...");

        fireProgressStateChanged(0);


        for (pix = 0; pix < sliceSize; pix++) {
            processBuffer[pix] = 0;
        }

        do {
            pixelsRemoved = thin(pass++, table);
            tempBuffer = imgBuffer;
            imgBuffer = processBuffer;
            processBuffer = tempBuffer;

            pixelsRemoved = thin(pass++, table);
            tempBuffer = imgBuffer;
            imgBuffer = processBuffer;
            processBuffer = tempBuffer;
        } while (pixelsRemoved > 0);

        if (pruningPixels > 0) {
            prune(pruningPixels, true);
        }

        if (showFrame) {
            ModelImage tempImage = new ModelImage(ModelImage.USHORT, srcImage.getExtents(), "skeletonize");

            try {
                tempImage.importData(0, imgBuffer, true);
            } catch (IOException error) {
                displayError("Algorithm Morphology2D: Image(s) locked in skeletonize");
                setCompleted(false);


                return;
            }

            new ViewJFrameImage(tempImage, null, null, false);
        }

        if (returnFlag == true) {
            return;
        }

        try {

            // if THREAD stopped already, then dump out!
            if (threadStopped) {

                finalize();

                return;
            }

            srcImage.importData(0, imgBuffer, true);
        } catch (IOException error) {
            displayError("Algorithm Morphology2D: Image(s) locked");
            setCompleted(false);


            return;
        }

        setCompleted(true);
    }
    
    
    
    private void distanceMapForShapeInterpolation(boolean returnFlag) {
        // if thread has already been stopped, dump out
        if (threadStopped) {
            finalize();

            return;
        }

        int pix, i;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;
        int x1, x2, y1, y2;
        float distance;
        float dist;

        float[] distBuffer;
        int[] pointArray;
        int pointIndex = 0;


        fireProgressStateChanged("Distance image ...");

        fireProgressStateChanged(0);


        try {
            pointArray = new int[sliceSize / 2];
            distBuffer = new float[sliceSize];
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Morphology2D.distanceMap: Out of memory");
            setCompleted(false);

            return;
        }

        // Save original in processBuffer and invert image in imgBuffer
        for (pix = 0; pix < sliceSize; pix++) {
            processBuffer[pix] = imgBuffer[pix];
            if (imgBuffer[pix] > 0) {
                imgBuffer[pix] = 0;
            } else {
                imgBuffer[pix] = 1;
            }
        }

        if (threadStopped) {
            finalize();

            return;
        }

        // Find all edge pixels.
        int end = sliceSize - xDim - 1;

        for (pix = xDim; pix < end; pix++) {

            if ((imgBuffer[pix] == 0) &&
                    ((imgBuffer[pix - xDim] != 0) || (imgBuffer[pix + 1] != 0) || (imgBuffer[pix + xDim] != 0) ||
                         (imgBuffer[pix - 1] != 0))) {
                pointArray[pointIndex] = pix;
                pointIndex++;
            }
        }

        int mod = sliceSize / 50; // mod is 2 percent of length

        float xRes = srcImage.getFileInfo(0).getResolutions()[0];
        float xResSquared = xRes * xRes;
        float yRes = srcImage.getFileInfo(0).getResolutions()[1];
        float yResSquared = yRes * yRes;

        if (threadStopped) {
            finalize();

            return;
        }

        for (pix = 0; (pix < sliceSize) && !threadStopped; pix++) {

            try {

                if (((pix % mod) == 0)) {
                    fireProgressStateChanged(Math.round((pix + 1) / ((float) sliceSize) * 100));
                }
            } catch (NullPointerException npe) {

                if (threadStopped) {
                    Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                      Preferences.DEBUG_ALGORITHM);
                }
            }

            if (entireImage || mask.get(pix)) {
                if (imgBuffer[pix] > 0) {
                    distance = 100000;
                    x1 = pix % xDim;
                    y1 = pix / xDim;

                    for (i = 0; i < pointIndex; i++) {
                        x2 = pointArray[i] % xDim;
                        y2 = pointArray[i] / xDim;
                        dist = ((x2 - x1) * (x2 - x1) * xResSquared) + ((y2 - y1) * (y2 - y1) * yResSquared);

                        if (dist < distance) {
                            distance = dist;
                        }
                    }

                    distBuffer[pix] = -(float) Math.sqrt(distance);
                }else {
                    //distBuffer[pix] = processBuffer[pix];
                	distance = 100000;
                    x1 = pix % xDim;
                    y1 = pix / xDim;

                    for (i = 0; i < pointIndex; i++) {
                        x2 = pointArray[i] % xDim;
                        y2 = pointArray[i] / xDim;
                        dist = ((x2 - x1) * (x2 - x1) * xResSquared) + ((y2 - y1) * (y2 - y1) * yResSquared);

                        if (dist < distance) {
                            distance = dist;
                        }
                    }

                    distBuffer[pix] = (float) Math.sqrt(distance);
                }
            }else {
                distBuffer[pix] = imgBuffer[pix];
            }
            
        }

        if (returnFlag == true) {
            distanceMap = distBuffer;

            return;
        }

        try {

            if (threadStopped) {
                finalize();

                return;
            }
            srcImage.reallocate(ModelStorageBase.FLOAT);
            srcImage.importData(0, distBuffer, true);
        } catch (IOException error) {
            displayError("Algorithm Morphology2D: Image(s) locked");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Morphology2D: Out of memory");
            setCompleted(false);

            return;
        }

        setCompleted(true);
    }

    /**
     * Euclidian distance map of the background.
     *
     * @param  returnFlag  if true then this operation is a step in the morph process
     */
    private void backgroundDistanceMap(boolean returnFlag) {
        
        // if thread has already been stopped, dump out
        if (threadStopped) {
            finalize();

            return;
        }

        int pix, i;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;
        int x1, x2, y1, y2;
        float distance;
        float dist;

        float[] distBuffer;
        int[] pointArray;
        int pointIndex = 0;


        fireProgressStateChanged("Distance image ...");

        fireProgressStateChanged(0);


        try {
            pointArray = new int[sliceSize / 2];
            distBuffer = new float[sliceSize];
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Morphology2D.distanceMap: Out of memory");
            setCompleted(false);

            return;
        }

        // Save original in processBuffer and invert image in imgBuffer
        for (pix = 0; pix < sliceSize; pix++) {
            processBuffer[pix] = imgBuffer[pix];
            if (imgBuffer[pix] > 0) {
                imgBuffer[pix] = 0;
            } else {
                imgBuffer[pix] = 1;
            }
        }

        if (threadStopped) {
            finalize();

            return;
        }

        // Find all edge pixels.
        int end = sliceSize - xDim - 1;

        for (pix = xDim; pix < end; pix++) {

            if ((imgBuffer[pix] == 0) &&
                    ((imgBuffer[pix - xDim] != 0) || (imgBuffer[pix + 1] != 0) || (imgBuffer[pix + xDim] != 0) ||
                         (imgBuffer[pix - 1] != 0))) {
                pointArray[pointIndex] = pix;
                pointIndex++;
            }
        }

        int mod = sliceSize / 50; // mod is 2 percent of length

        float xRes = srcImage.getFileInfo(0).getResolutions()[0];
        float xResSquared = xRes * xRes;
        float yRes = srcImage.getFileInfo(0).getResolutions()[1];
        float yResSquared = yRes * yRes;

        if (threadStopped) {
            finalize();

            return;
        }

        for (pix = 0; (pix < sliceSize) && !threadStopped; pix++) {

            try {

                if (((pix % mod) == 0)) {
                    fireProgressStateChanged(Math.round((pix + 1) / ((float) sliceSize) * 100));
                }
            } catch (NullPointerException npe) {

                if (threadStopped) {
                    Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                      Preferences.DEBUG_ALGORITHM);
                }
            }

            if (entireImage || mask.get(pix)) {

                if (imgBuffer[pix] > 0) {
                    distance = 100000;
                    x1 = pix % xDim;
                    y1 = pix / xDim;

                    for (i = 0; i < pointIndex; i++) {
                        x2 = pointArray[i] % xDim;
                        y2 = pointArray[i] / xDim;
                        dist = ((x2 - x1) * (x2 - x1) * xResSquared) + ((y2 - y1) * (y2 - y1) * yResSquared);

                        if (dist < distance) {
                            distance = dist;
                        }
                    }

                    distBuffer[pix] = (float) Math.sqrt(distance);
                }
            }
            else {
                distBuffer[pix] = processBuffer[pix];
            }
        }

        if (returnFlag == true) {
            distanceMap = distBuffer;

            return;
        }

        try {

            if (threadStopped) {
                finalize();

                return;
            }
            srcImage.reallocate(ModelStorageBase.FLOAT);
            srcImage.importData(0, distBuffer, true);
        } catch (IOException error) {
            displayError("Algorithm Morphology2D: Image(s) locked");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Morphology2D: Out of memory");
            setCompleted(false);

            return;
        }

        setCompleted(true);
    }

    /**
     * Deletes objects larger than maximum size indicated and objects smaller than the indicated minimum size.
     *
     * @param  min         delete all objects smaller than the minimum value (pixels)
     * @param  max         delete all objects greater than the maximum value (pixels)
     * @param  returnFlag  if true then this operation is a step in the morph process (i.e. close)
     */
    private void deleteObjects(int min, int max, boolean returnFlag) {

        if (threadStopped) {
            setCompleted(false);

            finalize();

            return;
        }

        int i, pix;
        int count;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;
        short floodValue = 1;
        short[] tmpBuffer;


        fireProgressStateChanged("Removing objects ...");

        fireProgressStateChanged(0);


        objects.removeAllElements();

        for (pix = 0; (pix < sliceSize) && !threadStopped; pix++) {

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

        tmpBuffer = imgBuffer;
        imgBuffer = processBuffer;
        processBuffer = tmpBuffer;

        try {
            fireProgressStateChanged(33);
        } catch (NullPointerException npe) {

            if (threadStopped) {
                Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                  Preferences.DEBUG_ALGORITHM);
            }
        }

        for (i = 0; i < objects.size(); i++) {

            try {
                fireProgressStateChanged(Math.round(33 + ((i + 1) / ((float) objects.size()) * 33)));
            } catch (NullPointerException npe) {

                if (threadStopped) {
                    Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                      Preferences.DEBUG_ALGORITHM);
                }
            }

            if (entireImage == true) {

                if ((((intObject) (objects.elementAt(i))).size < min) ||
                        (((intObject) (objects.elementAt(i))).size > max)) {
                    floodFill(processBuffer, ((intObject) (objects.elementAt(i))).index, (short) 0,
                              ((intObject) (objects.elementAt(i))).id);
                    objects.removeElementAt(i);
                    i--;
                }
            } else if (mask.get(((intObject) (objects.elementAt(i))).index) == true) {

                if ((((intObject) (objects.elementAt(i))).size < min) ||
                        (((intObject) (objects.elementAt(i))).size > max)) {
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

        try {
            fireProgressStateChanged(66);
        } catch (NullPointerException npe) {

            if (threadStopped) {
                Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                  Preferences.DEBUG_ALGORITHM);
            }
        }

        // relabel objects in order
        for (i = 0; (i < objects.size()) && !threadStopped; i++) {

            try {
                fireProgressStateChanged(Math.round(66 + ((i + 1) / ((float) objects.size()) * 34)));
            } catch (NullPointerException npe) {

                if (threadStopped) {
                    Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                      Preferences.DEBUG_ALGORITHM);
                }
            }

            if (entireImage == true) {
                floodFill(processBuffer, ((intObject) (objects.elementAt(i))).index, (short) (i + 1),
                          ((intObject) (objects.elementAt(i))).id);
            } else if (mask.get(((intObject) (objects.elementAt(i))).index) == true) {
                floodFill(processBuffer, ((intObject) (objects.elementAt(i))).index, (short) (i + 1),
                          ((intObject) (objects.elementAt(i))).id);
            }
        }

        try {
            fireProgressStateChanged(100);
        } catch (NullPointerException npe) {

            if (threadStopped) {
                Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                  Preferences.DEBUG_ALGORITHM);
            }
        }

        /*
         * srcImage.getUserInterface().getMessageFrame().getData().append( "\nDeleted objects outside range (" + min +
         * ", " + max + "). \n" ); for ( i = 0; i < objects.size(); i++ ) {
         * srcImage.getUserInterface().getMessageFrame().getData().append( "  Object " + ( i + 1 ) + " = " + (
         * (intObject) ( objects.elementAt( i ) ) ).size + "\n" ); }
         */
        if (returnFlag == true) {
            return;
        }

        try {

            if (threadStopped) {
                setCompleted(false);

                finalize();

                return;
            }

            if ((srcImage.getType() == ModelStorageBase.BOOLEAN) || (srcImage.getType() == ModelStorageBase.BYTE) ||
                (srcImage.getType() == ModelStorageBase.UBYTE) || (srcImage.getType() == ModelStorageBase.SHORT)) {
                srcImage.reallocate(ModelImage.USHORT);
            }

            srcImage.importData(0, processBuffer, true);
        } catch (IOException error) {
            displayError("Algorithm Morphology2D: Image(s) locked");
            setCompleted(false);


            return;
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Morphology2D: Out of memory");
            setCompleted(false);


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
        int offsetX, offsetY;
        int offsetXU;
        int startX, startY;
        int endX, endY;

        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];

        int halfKDim = kDim / 2;
        int sliceSize = xDim * yDim;
        int stepY = kDim * xDim;
        short[] tempBuffer;

        fireProgressStateChanged("Dilating image ...");
        fireProgressStateChanged(0);


        for (pix = 0; pix < sliceSize; pix++) {
            processBuffer[pix] = 0;
        }

        int mod = (iters * sliceSize) / 20; // mod is 5 percent of length

        for (c = 0; (c < iters) && !threadStopped; c++) {

            for (pix = 0; (pix < sliceSize) && !threadStopped; pix++) {

                try {

                    if (((((c * sliceSize) + pix) % mod) == 0)) {
                        fireProgressStateChanged(Math.round((pix + 1 + (c * sliceSize)) / (iters * (float) sliceSize) *
                                                                100));
                    }
                } catch (NullPointerException npe) {

                    if (threadStopped) {
                        Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                          Preferences.DEBUG_ALGORITHM);
                    }
                }

                if (entireImage || mask.get(pix)) {
                    value = imgBuffer[pix];

                    if (value != 0) {
                        offsetX = (pix % xDim) - halfKDim;
                        offsetXU = offsetX + kDim;
                        offsetY = (pix / xDim) - halfKDim;

                        count = 0;
                        startY = offsetY * xDim;
                        endY = startY + stepY;

                        if (startY < 0) {
                            startY = 0;
                        }

                        if (endY > sliceSize) {
                            endY = sliceSize;
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

            tempBuffer = imgBuffer;
            imgBuffer = processBuffer;
            processBuffer = tempBuffer;
        }

        if (showFrame) {
            ModelImage tempImage = new ModelImage(ModelImage.USHORT, srcImage.getExtents(), "Dilate");

            try {
                tempImage.importData(0, imgBuffer, true);
            } catch (IOException error) {
                displayError("Algorithm Morphology2D: Image(s) locked in Dilate");
                setCompleted(false);


                return;
            }

            new ViewJFrameImage(tempImage, null, null, false);
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
            displayError("Algorithm Morphology2D: Image(s) locked");
            setCompleted(false);


            return;
        }

        setCompleted(true);
    }

    /**
     * Euclidian distance map.
     *
     * @param  returnFlag  if true then this operation is a step in the morph process
     */
    private void distanceMap(boolean returnFlag) {

        // if thread has already been stopped, dump out
        if (threadStopped) {
            finalize();

            return;
        }

        int pix, i;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;
        int x1, x2, y1, y2;
        float distance;
        float dist;

        float[] distBuffer;
        int[] pointArray;
        int pointIndex = 0;

        fireProgressStateChanged("Distance image ...");
        fireProgressStateChanged(0);


        try {
            pointArray = new int[sliceSize / 2];
            distBuffer = new float[sliceSize];
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Morphology2D.distanceMap: Out of memory");
            setCompleted(false);

            return;
        }

        // Find all edge pixels.
        int end = sliceSize - xDim - 1;

        for (pix = 0; (pix < sliceSize) && !threadStopped; pix++) {

            if ((pix < xDim) && (imgBuffer[pix] > 0)) {
                pointArray[pointIndex] = pix;
                pointIndex++;
            } else if (((pix % xDim) == (xDim - 1)) && (imgBuffer[pix] > 0)) {
                pointArray[pointIndex] = pix;
                pointIndex++;
            } else if (((pix % xDim) == 0) && (imgBuffer[pix] > 0)) {
                pointArray[pointIndex] = pix;
                pointIndex++;
            } else if ((pix > (sliceSize - xDim)) && (imgBuffer[pix] > 0)) {
                pointArray[pointIndex] = pix;
                pointIndex++;
            } else if ((pix > (xDim + 1)) && (pix < end) && (imgBuffer[pix] == 0) &&
                           ((imgBuffer[pix - xDim] != 0) || (imgBuffer[pix + 1] != 0) || (imgBuffer[pix + xDim] != 0) ||
                                (imgBuffer[pix - 1] != 0))) {
                pointArray[pointIndex] = pix;
                pointIndex++;
            }
        }

        int mod = sliceSize / 50; // mod is 2 percent of length

        float xRes = srcImage.getFileInfo(0).getResolutions()[0];
        float xResSquared = xRes * xRes;
        float yRes = srcImage.getFileInfo(0).getResolutions()[1];
        float yResSquared = yRes * yRes;

        if (threadStopped) {
            finalize();

            return;
        }

        for (pix = 0; (pix < sliceSize) && !threadStopped; pix++) {

            try {

                if (((pix % mod) == 0)) {
                    fireProgressStateChanged(Math.round((pix + 1) / ((float) sliceSize) * 100));
                }
            } catch (NullPointerException npe) {

                if (threadStopped) {
                    Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                      Preferences.DEBUG_ALGORITHM);
                }
            }

            if (entireImage || mask.get(pix)) {

                if (imgBuffer[pix] > 0) {
                    distance = 100000;
                    x1 = pix % xDim;
                    y1 = pix / xDim;

                    for (i = 0; i < pointIndex; i++) {
                        x2 = pointArray[i] % xDim;
                        y2 = pointArray[i] / xDim;
                        dist = ((x2 - x1) * (x2 - x1) * xResSquared) + ((y2 - y1) * (y2 - y1) * yResSquared);

                        if (dist < distance) {
                            distance = dist;
                        }
                    }

                    distBuffer[pix] = (float) Math.sqrt(distance);
                }
            }
            else {
                distBuffer[pix] = imgBuffer[pix];
            }
        }

        if (returnFlag == true) {
            distanceMap = distBuffer;

            return;
        }

        try {

            if (threadStopped) {
                finalize();

                return;
            }

            srcImage.reallocate(ModelStorageBase.FLOAT);
            srcImage.importData(0, distBuffer, true);
        } catch (IOException error) {
            displayError("Algorithm Morphology2D: Image(s) locked");
            setCompleted(false);


            return;
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Morphology2D: Out of memory");
            setCompleted(false);


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
        int offsetX, offsetY;
        int offsetXU;
        int startX, startY;
        int endX, endY;

        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];

        int halfKDim = kDim / 2;
        int sliceSize = xDim * yDim;
        int stepY = kDim * xDim;
        short[] tempBuffer;

        fireProgressStateChanged("Eroding image ...");
        fireProgressStateChanged(0);


        int mod = (iters * sliceSize) / 20; // mod is 5 percent of length

        for (pix = 0; pix < sliceSize; pix++) {
            processBuffer[pix] = 0;
        }

        for (c = 0; (c < iters) && !threadStopped; c++) {

            for (pix = 0; (pix < sliceSize) && !threadStopped; pix++) {

                try {

                    if (((((c * sliceSize) + pix) % mod) == 0)) {
                        fireProgressStateChanged(Math.round((pix + 1 + (c * sliceSize)) / (iters * (float) sliceSize) *
                                                                100));
                    }
                } catch (NullPointerException npe) {

                    if (threadStopped) {
                        Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                          Preferences.DEBUG_ALGORITHM);
                    }
                }

                if (entireImage || mask.get(pix)) {
                    value = imgBuffer[pix];

                    if (imgBuffer[pix] == 0) {
                        clear = true;
                    } else {
                        clear = false;
                        offsetX = (pix % xDim) - halfKDim;
                        offsetXU = offsetX + kDim;
                        offsetY = (pix / xDim) - halfKDim;

                        count = 0;
                        startY = offsetY * xDim;
                        endY = startY + stepY;

                        if (startY < 0) {
                            startY = 0;
                        }

                        if (endY > sliceSize) {
                            endY = sliceSize;
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

            tempBuffer = imgBuffer;
            imgBuffer = processBuffer;
            processBuffer = tempBuffer;
        }

        if (showFrame) {
            ModelImage tempImage = new ModelImage(ModelImage.USHORT, srcImage.getExtents(), "Erode");

            try {
                tempImage.importData(0, imgBuffer, true);
            } catch (IOException error) {
                displayError("Algorithm Morphology2D: Image(s) locked in Erode");
                setCompleted(false);


                return;
            }

            new ViewJFrameImage(tempImage, null, null, false);
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
            displayError("Algorithm Morphology2D: Image(s) locked");
            setCompleted(false);


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
        int sliceSize = xDim * yDim;
        short floodValue = 2;

        // Fill the boundary of the image with value 0, which represent the seed value
        int i = 0;

        // top boundary
        for (i = 0; i < xDim; i++) {
            imgBuffer[i] = 0;
        }

        // bottom boundary
        for (i = (yDim - 1) * xDim; i < (yDim * xDim); i++) {
            imgBuffer[i] = 0;
        }

        // left boundary
        for (i = 0; i < sliceSize; i = i + xDim) {
            imgBuffer[i] = 0;
        }

        // right boundary
        for (i = xDim; i < sliceSize; i = i + xDim) {
            imgBuffer[i - 1] = 0;
        }

        // region grow to fill the holes inside the cell region block.
        fillHolesRegion(0, floodValue, imgBuffer[0]);

        // System.arraycopy(imgBuffer,0, processBuffer, 0, imgBuffer.length);
        // if THREAD stopped already, then dump out!
        if (threadStopped) {
            finalize();

            return;
        }

        try {

            if ((srcImage.getType() == ModelStorageBase.BOOLEAN) || (srcImage.getType() == ModelStorageBase.BYTE) ||
                (srcImage.getType() == ModelStorageBase.UBYTE) || (srcImage.getType() == ModelStorageBase.SHORT)) {
                srcImage.reallocate(ModelImage.USHORT);
            }

            srcImage.importData(0, imgBuffer, true);
        } catch (IOException error) {
            displayError("Algorithm Morphology2D: Image(s) locked");
            setCompleted(false);


            return;
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Morphology2D: Out of memory");
            setCompleted(false);


            return;
        }

        if (showFrame) {
            ModelImage tempImage = new ModelImage(ModelImage.USHORT, srcImage.getExtents(), "Fill Objects");

            try {
                tempImage.importData(0, imgBuffer, true);
            } catch (IOException error) {
                displayError("Algorithm Morphology2D: Image(s) locked in Fill Objects");
                setCompleted(false);


                return;
            }

            new ViewJFrameImage(tempImage, null, null, false);
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
        Point pt;
        Point tempPt;
        Stack<Point> stack = new Stack<Point>();
        int indexY;
        int x, y;

        Point seedPt = new Point(stIndex % xDim, stIndex / xDim);

        if (imgBuffer[(seedPt.y * xDim) + seedPt.x] == objValue) {
            stack.push(seedPt);
            imgBuffer[(seedPt.y * xDim) + seedPt.x] = floodValue;

            // While loop fill the background region with value 2.
            while (!stack.empty()) {
                pt = (Point) stack.pop();
                x = pt.x;
                y = pt.y;
                indexY = y * xDim;

                // pixel itself
                if (imgBuffer[indexY + x] == objValue) {
                    imgBuffer[indexY + x] = floodValue;
                }

                // checking on the pixel's four neighbors
                if ((x + 1) < xDim) {

                    if (imgBuffer[indexY + x + 1] == objValue) {
                        tempPt = new Point(x + 1, y);
                        stack.push(tempPt);
                    }
                }

                if ((x - 1) >= 0) {

                    if (imgBuffer[indexY + x - 1] == objValue) {
                        tempPt = new Point(x - 1, y);
                        stack.push(tempPt);
                    }
                }

                if ((y + 1) < yDim) {

                    if (imgBuffer[((y + 1) * xDim) + x] == objValue) {
                        tempPt = new Point(x, y + 1);
                        stack.push(tempPt);
                    }
                }

                if ((y - 1) >= 0) {

                    if (imgBuffer[((y - 1) * xDim) + x] == objValue) {
                        tempPt = new Point(x, y - 1);
                        stack.push(tempPt);
                    }
                }
            }
        }

        // Fill the pixels with value 2 to 0, else to 1.   Fill the holes
        for (int i = 0; i < (xDim * yDim); i++) {

            if (imgBuffer[i] == 2) {
                imgBuffer[i] = 0;
            } else {
                imgBuffer[i] = 1;
            }
        }
    }

    /**
     * Finds the edges of the objects in an image.
     *
     * @param  edgingType  DOCUMENT ME!
     * @param  returnFlag  if true then this operation is a step in the morph process (i.e. close)
     */
    private void findEdges(int edgingType, boolean returnFlag) {

        // if THREAD stopped already, then dump out!
        if (threadStopped) {
            finalize();

            return;
        }

        boolean clear;
        short value = 0;
        int i, j, pix, count, mod;
        int offsetX, offsetY;
        int offsetXU;
        int startX, startY;
        int endX, endY;

        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];

        int halfKDim = kDim / 2;
        int sliceSize = xDim * yDim;
        int stepY = kDim * xDim;
        short[] tempBuffer;

        fireProgressStateChanged("Finding Edges ...");
        fireProgressStateChanged(0);


        try {
            tempBuffer = new short[imgBuffer.length];
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Morphology2D: Out of memory");
            setCompleted(false);


            return;
        }

        for (pix = 0; pix < sliceSize; pix++) {
            processBuffer[pix] = 0;
            tempBuffer[pix] = 0;
        }

        mod = sliceSize / 20; // mod is 5 percent of length

        if (edgingType == OUTER_EDGING) {

            // create a dilated image and XOR it with the original
            for (pix = 0; (pix < sliceSize) && !threadStopped; pix++) {

                try {

                    if ((((sliceSize + pix) % mod) == 0)) {
                        fireProgressStateChanged(Math.round((pix + 1 + sliceSize) / ((float) sliceSize) * 100));
                    }
                } catch (NullPointerException npe) {

                    if (threadStopped) {
                        Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                          Preferences.DEBUG_ALGORITHM);
                    }
                }

                if (entireImage || mask.get(pix)) {
                    value = imgBuffer[pix];

                    if (value != 0) {
                        offsetX = (pix % xDim) - halfKDim;
                        offsetXU = offsetX + kDim;
                        offsetY = (pix / xDim) - halfKDim;

                        count = 0;
                        startY = offsetY * xDim;
                        endY = startY + stepY;

                        if (startY < 0) {
                            startY = 0;
                        }

                        if (endY > sliceSize) {
                            endY = sliceSize;
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
                    tempBuffer[pix] = imgBuffer[pix];
                }
            }

            for (pix = 0; (pix < sliceSize) && !threadStopped; pix++) {

                if (entireImage || mask.get(pix)) {

                    if ((imgBuffer[pix] == 0) && (processBuffer[pix] != 0)) {
                        tempBuffer[pix] = processBuffer[pix];
                    }
                }
            }
        } else {

            // create an eroded image and XOR it with the original
            for (pix = 0; (pix < sliceSize) && !threadStopped; pix++) {

                try {

                    if ((((sliceSize + pix) % mod) == 0)) {
                        fireProgressStateChanged(Math.round((pix + 1 + sliceSize) / ((float) sliceSize) * 100));
                    }
                } catch (NullPointerException npe) {

                    if (threadStopped) {
                        Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                          Preferences.DEBUG_ALGORITHM);
                    }
                }

                if (entireImage || mask.get(pix)) {
                    value = imgBuffer[pix];

                    if (imgBuffer[pix] == 0) {
                        clear = true;
                    } else {
                        clear = false;
                        offsetX = (pix % xDim) - halfKDim;
                        offsetXU = offsetX + kDim;
                        offsetY = (pix / xDim) - halfKDim;

                        count = 0;
                        startY = offsetY * xDim;
                        endY = startY + stepY;

                        if (startY < 0) {
                            startY = 0;
                        }

                        if (endY > sliceSize) {
                            endY = sliceSize;
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

                    tempBuffer[pix] = imgBuffer[pix];
                }
            }

            // copy buffer
            for (pix = 0; (pix < sliceSize) && !threadStopped; pix++) {

                if (entireImage || mask.get(pix)) {

                    if ((imgBuffer[pix] != 0) && (processBuffer[pix] == 0)) {
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
                finalize();

                return;
            }

            srcImage.importData(0, imgBuffer, true);
        } catch (IOException error) {
            displayError("Algorithm Morphology2D: Image(s) locked");
            setCompleted(false);


            return;
        }

        setCompleted(true);

    }

    /**
     * 2D flood fill that forms a short mask.
     *
     * @param   idBuffer    buffer used to hold flood value
     * @param   stIndex     the starting index of the seed point
     * @param   floodValue  the value to flood the region with
     * @param   objValue    object ID value that idenditifies the flood region.
     *
     * @return  DOCUMENT ME!
     */
    private int floodFill(short[] idBuffer, int stIndex, short floodValue, short objValue) {
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        Point pt;
        Point tempPt;
        Stack<Point> stack = new Stack<Point>();
        int indexY;
        int x, y;
        int pixCount = 0;

        Point seedPt = new Point(stIndex % xDim, stIndex / xDim);

        if (imgBuffer[(seedPt.y * xDim) + seedPt.x] > 0) {
            stack.push(seedPt);
            imgBuffer[(seedPt.y * xDim) + seedPt.x] = 0;

            while (!stack.empty()) {
                pt = (Point) stack.pop();
                x = pt.x;
                y = pt.y;
                indexY = y * xDim;

                idBuffer[indexY + x] = floodValue;
                pixCount++;

                if ((x + 1) < xDim) {

                    if (imgBuffer[indexY + x + 1] == objValue) {
                        tempPt = new Point(x + 1, y);
                        stack.push(tempPt);
                        imgBuffer[indexY + tempPt.x] = 0;
                    }
                }

                if ((x - 1) >= 0) {

                    if (imgBuffer[indexY + x - 1] == objValue) {
                        tempPt = new Point(x - 1, y);
                        stack.push(tempPt);
                        imgBuffer[indexY + tempPt.x] = 0;
                    }
                }

                if ((y + 1) < yDim) {

                    if (imgBuffer[((y + 1) * xDim) + x] == objValue) {
                        tempPt = new Point(x, y + 1);
                        stack.push(tempPt);
                        imgBuffer[(tempPt.y * xDim) + x] = 0;
                    }
                }

                if ((y - 1) >= 0) {

                    if (imgBuffer[((y - 1) * xDim) + x] == objValue) {
                        tempPt = new Point(x, y - 1);
                        stack.push(tempPt);
                        imgBuffer[(tempPt.y * xDim) + x] = 0;
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
     * Check the neighbor pix.
     *
     * @param   index        pixel in center
     * @param   pixNeighbor  neighbor pix
     *
     * @return  boolean is connected neighbor or not
     */
    private boolean isNeighbor(int index, int pixNeighbor) {
        int p1, p2, p3, p4, p6, p7, p8, p9;
        int xDim = srcImage.getExtents()[0];

        p1 = index - xDim - 1;
        p2 = index - xDim;
        p3 = index - xDim + 1;
        p4 = index - 1;
        p6 = index + 1;
        p7 = index + xDim - 1;
        p8 = index + xDim;
        p9 = index + xDim + 1;

        if ((pixNeighbor == p1) || (pixNeighbor == p2) || (pixNeighbor == p3) || (pixNeighbor == p4) ||
                (pixNeighbor == p6) || (pixNeighbor == p7) || (pixNeighbor == p8) || (pixNeighbor == p9)) {
            return true;
        } else {
            return false;
        }

    }

    /**
     * Check if the given pix is a single point or not.
     *
     * @param   index      index
     * @param   tmpBuffer  short[]
     *
     * @return  boolean
     */
    private boolean isSinglePoint(int index, short[] tmpBuffer) {
        short p1, p2, p3, p4, p6, p7, p8, p9;
        int xDim = srcImage.getExtents()[0];

        short bgColor = 0;

        p1 = tmpBuffer[index - xDim - 1];
        p2 = tmpBuffer[index - xDim];
        p3 = tmpBuffer[index - xDim + 1];
        p4 = tmpBuffer[index - 1];
        p6 = tmpBuffer[index + 1];
        p7 = tmpBuffer[index + xDim - 1];
        p8 = tmpBuffer[index + xDim];
        p9 = tmpBuffer[index + xDim + 1];

        if ((p1 == bgColor) && (p2 == bgColor) && (p3 == bgColor) && (p4 == bgColor) && (p6 == bgColor) &&
                (p7 == bgColor) && (p8 == bgColor) && (p9 == bgColor)) {
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

        Preferences.debug("# Morph2d.makeCircularKernel: kernel size = " + kDim + "\n", Preferences.DEBUG_ALGORITHM);

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
     * Used by ultimate erode to simplify code a little.
     *
     * @param   buffer  reference to the mask data
     * @param   index   pointer into the array
     * @param   xDim    the width of image
     *
     * @return  DOCUMENT ME!
     */
    private boolean onePixel(short[] buffer, int index, int xDim) {

        if ((buffer[index] > 0) && (buffer[index - xDim] == 0) && (buffer[index - xDim + 1] == 0) &&
                (buffer[index + 1] == 0) && (buffer[index + xDim + 1] == 0) && (buffer[index + xDim] == 0) &&
                (buffer[index + xDim - 1] == 0) && (buffer[index - 1] == 0) && (buffer[index - xDim - 1] == 0)) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * DOCUMENT ME!
     */
    private void particleAnalysis() {

        // if thread has already been stopped, dump out
        if (threadStopped) {
            setCompleted(false);

            finalize();

            return;
        }

        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;
        int pix, i;
        Vector3f[] seeds;
        int[] destExtents = null;
        ModelImage wsImage = null;
        ModelImage distanceImage = null;
        AlgorithmWatershed ws = null;

        int[] progressValues = getProgressValues();

        fireProgressStateChanged("Particle analysis (PA) ...");
        fireProgressStateChanged(0);


        // add open and close to remove noise; Might not need because delete objects
        // of some size works.

        try {

            // add set minimum dist
            setProgressValues(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 0),
                              ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 25));
            ultimateErode(true); // forms list of points (one point per object)
            setProgressValues(progressValues);


            fireProgressStateChanged(25);

            fireProgressStateChanged("PA - distance map ...");


            srcImage.exportData(0, xDim * yDim, imgBuffer); // locks and releases lock
            destExtents = new int[2];
        } catch (NullPointerException npe) {

            if (threadStopped) {
                Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                  Preferences.DEBUG_ALGORITHM);
            }
        } catch (IOException error) {
            displayError("Algorithm Morphology2D..particleAnalysis: Image(s) locked");
            setCompleted(false);


            return;
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Morphology2D.particleAnalysis: Out of memory");
            setCompleted(false);


            return;
        }

        // calc min max of distanceMap
        float minDist = Float.MAX_VALUE;
        float maxDist = -Float.MAX_VALUE;

        for (pix = 0; pix < sliceSize; pix++) {

            if (distanceMap[pix] < minDist) {
                minDist = distanceMap[pix];
            }

            if (distanceMap[pix] > maxDist) {
                maxDist = distanceMap[pix];
            }
        }

        if (threadStopped) {
            setCompleted(false);

            finalize();

            return;
        }

        int bgIndex = -1;

        for (pix = 0; pix < sliceSize; pix++) {

            if (imgBuffer[pix] > 0) {
                distanceMap[pix] = maxDist + minDist - distanceMap[pix];
            } else {

                if ((bgIndex == -1) && (pix > xDim) && ((pix % xDim) != 0) && ((pix % xDim) != xDim) &&
                        (pix < (sliceSize - xDim))) {
                    bgIndex = pix;
                }

                distanceMap[pix] = 0;
            }
        }

        fireProgressStateChanged("Watershed stage ...");
        fireProgressStateChanged(70);

        destExtents[0] = xDim;
        destExtents[1] = yDim;

        // Make result image of float type
        try {

            // form vector of seeds from ultimate erode points + point for background(should be first in list!)
            seeds = new Vector3f[ultErodeObjects.length + 1];

            if (bgIndex > 0) {
                seeds[0] = new Vector3f(bgIndex / xDim, bgIndex % xDim, 1); // background seed
            } else {
                seeds[0] = new Vector3f(1, 1, 1);
            }

            for (i = 0; i < ultErodeObjects.length; i++) {
                seeds[i + 1] = new Vector3f(ultErodeObjects[i].x, ultErodeObjects[i].y, 1);
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

            }

            if (threadStopped) {
                setCompleted(false);

                finalize();

                return;
            }

            wsImage = new ModelImage(ModelImage.USHORT, destExtents, " Watershed");

            distanceImage = new ModelImage(ModelImage.FLOAT, destExtents, "Distance ");
            distanceImage.importData(0, distanceMap, true);
            ws = new AlgorithmWatershed(wsImage, srcImage, null, null, null);

        } catch (IOException error) {
            displayError("Algorithm Morphology2D: Image(s) locked");
            setCompleted(false);


            return;
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Morphology2D: Out of memory");
            setCompleted(false);


            return;
        }

        linkProgressToAlgorithm(ws);
        ws.setProgressValues(generateProgressValues(70, 90));

        ws.setSeedVector(seeds);
        ws.setEnergyImage(distanceImage);
        ws.run();

        // if the watershed algo is halted,
        if (!ws.isCompleted()) {
            setThreadStopped(true); // halt the rest of this processing.

            // and kill progressbar
            setCompleted(false);

            return;
        }

        try {
            fireProgressStateChanged(90);
            wsImage.exportData(0, xDim * yDim, imgBuffer); // locks and releases lock

            // once the data from the watershed has been exported to imgBuffer, the
            // 2 temp images (wsImage, distanceImage) can be cleaned up
            wsImage.disposeLocal();
            distanceImage.disposeLocal();
            wsImage = null;
            distanceImage = null;
        } catch (IOException error) {
            displayError("Algorithm Morphology2D: Image(s) locked");
            setCompleted(false);


            return;
        } catch (NullPointerException npe) {

            if (threadStopped) {
                Preferences.debug("somehow you mana	ged to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                  Preferences.DEBUG_ALGORITHM);
            }
        }

        fireProgressStateChanged("Deleting objects ...");
        fireProgressStateChanged(90);


        setProgressValues(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 90),
                          ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 100));
        deleteObjects(min, max, false);
        setProgressValues(progressValues);

    }

    /**
     * particleAnalysis = Ult erode & (bg dist map AND orig image) => watershed(ultErodePts, ANDED Bg Dist) =>
     * IDobjects.
     *
     * @param  returnFlag  DOCUMENT ME!
     */
    private void particleAnalysisNew(boolean returnFlag) {

        // if thread has already been stopped, dump out
        if (threadStopped) {
            setCompleted(false);

            finalize();

            return;
        }

        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;
        int pix, i;
        Vector3f[] seeds;
        int[] destExtents = null;
        ModelImage wsImage = null;
        ModelImage distanceImage = null;
        AlgorithmWatershed ws = null;

        int[] progressValues = getProgressValues();
        short imgBufferOrg[] = null;
        short imgBuffer2[];
        ModelImage srcImage2 = null;

        fireProgressStateChanged("Particle analysis (PA) ...");
        fireProgressStateChanged(0);


        try {

            // add set minimum dist

            srcImage.exportData(0, xDim * yDim, imgBuffer); // locks and releases lock
            setMaxProgressValue(ViewJProgressBar.getProgressFromInt(progressValues[0], progressValues[1], 60));
            distanceMap(true);
            setProgressValues(progressValues);
            destExtents = new int[2];
        } catch (IOException error) {
            displayError("Algorithm Morphology2D..particleAnalysis: Image(s) locked");
            setCompleted(false);


            return;
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Morphology2D.particleAnalysis: Out of memory");
            setCompleted(false);


            return;
        }

        // calc min max of distanceMap
        float minDist = Float.MAX_VALUE;
        float maxDist = -Float.MAX_VALUE;

        for (pix = 0; pix < sliceSize; pix++) {
            if ((entireImage) || (mask.get(pix))) {
                if (distanceMap[pix] < minDist) {
                    minDist = distanceMap[pix];
                }
    
                if (distanceMap[pix] > maxDist) {
                    maxDist = distanceMap[pix];
                }
            }
        }

        if (threadStopped) {
            setCompleted(false);

            finalize();

            return;
        }

        int bgIndex = -1;

        for (pix = 0; pix < sliceSize; pix++) {
            if ((entireImage) || (mask.get(pix))) {
                if (imgBuffer[pix] > 0) {
                    distanceMap[pix] = maxDist + minDist - distanceMap[pix];
                } else {
    
                    if ((bgIndex == -1) && (pix > xDim) && ((pix % xDim) != 0) && ((pix % xDim) != xDim) &&
                            (pix < (sliceSize - xDim))) {
                        bgIndex = pix;
                    }
    
                    distanceMap[pix] = 0;
                }
            }
        }

        fireProgressStateChanged(75, null, "Watershed stage ...");


        destExtents[0] = xDim;
        destExtents[1] = yDim;

        // Make result image of float type
        try {
            seeds = new Vector3f[pruneSeeds.size() + 1];

            if (bgIndex > 0) {
                seeds[0] = new Vector3f(bgIndex / xDim, bgIndex % xDim, 1); // background seed
            } else {
                seeds[0] = new Vector3f(1, 1, 1);
            }

            for (i = 0; i < pruneSeeds.size(); i++) {
                int val = ((Integer) (pruneSeeds.elementAt(i))).intValue();

                seeds[i + 1] = new Vector3f(val % xDim, val / xDim, 1);
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

            }

            if (threadStopped) {
                setCompleted(false);

                finalize();

                return;
            }

            wsImage = new ModelImage(ModelImage.USHORT, destExtents, " Watershed");
            distanceImage = new ModelImage(ModelImage.FLOAT, destExtents, "Distance");
            distanceImage.importData(0, distanceMap, true);
            if (!entireImage) {
                imgBufferOrg = new short[sliceSize];
                imgBuffer2 = new short[sliceSize];
                try {
                    srcImage.exportData(0, sliceSize, imgBufferOrg);
                }
                catch(IOException e) {
                    displayError("Algorithm Morphology2D: Image(s) locked");
                    setCompleted(false);
                    return;    
                }
                for (i = 0; i < sliceSize; i++) {
                    if (mask.get(i)) {
                        imgBuffer2[i] = imgBufferOrg[i];
                    }
                }
                srcImage2 = (ModelImage)srcImage.clone();
                try {
                    srcImage2.importData(0, imgBuffer2, true);
                }
                catch(IOException e) {
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
        } catch (IOException error) {
            displayError("Algorithm Morphology2D: Image(s) locked");
            setCompleted(false);


            return;
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Morphology2D: Out of memory");
            setCompleted(false);
            return;
        }

        ws.setSeedVector(seeds);
        linkProgressToAlgorithm(ws);
        ws.setProgressValues(generateProgressValues(80, 95));
        ws.setEnergyImage(distanceImage);
        ws.run();

        // if the watershed algo is halted,
        if (!ws.isCompleted()) {
            setThreadStopped(true); // halt the rest of this processing.

            // and kill progressbar
            setCompleted(false);

            return;
        }
        
        if (!entireImage) {
            srcImage2.disposeLocal();
            srcImage2 = null;
        }

        // new ViewJFrameImage( wsImage, null, new Dimension( 300, 300 ), srcImage.getUserInterface(), false );

        /*
         *  setCompleted( true ); returnFlag = true; if ( returnFlag == true ) { return; }
         */
        try {
            fireProgressStateChanged(95);
            wsImage.exportData(0, xDim * yDim, imgBuffer); // locks and releases lock
            if (!entireImage) {
                for (i = 0; i < sliceSize; i++) {
                    if (!mask.get(i)) {
                        imgBuffer[i] = imgBufferOrg[i];    
                    }
                }
            }
            srcImage.importData(0, imgBuffer, true);

            // once the data from the watershed has been exported to imgBuffer, the
            // 2 temp images (wsImage, distanceImage) can be cleaned up
            if (showFrame) {
                new ViewJFrameImage(wsImage, null, null, false);    
            }
            else {
                wsImage.disposeLocal();
                wsImage = null;
            }
            distanceImage.disposeLocal();
            distanceImage = null;
            fireProgressStateChanged(100);
        } catch (IOException error) {
            displayError("Algorithm Morphology2D: Image(s) locked");
            setCompleted(false);


            return;
        } catch (NullPointerException npe) {

            if (threadStopped) {
                Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                  Preferences.DEBUG_ALGORITHM);
            }
        }

        if (returnFlag == true) {
            return;
        }

        setCompleted(true);
    }

    /**
     * This a thinning algorithm used to do half of one layer of thinning (which layer is dictated by whether pass is
     * even or odd).
     *
     * @param   pass   the number pass this execution is on the image
     * @param   table  the table to lookup whether to delete the pixel or let it stay.
     *
     * @return  DOCUMENT ME!
     */
    private int thin(int pass, int[] table) {
        int p1, p2, p3, p4, p5, p6, p7, p8, p9;
        int bgColor = 0;
        int pix;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;

        int v, index, code;
        int pixelsRemoved = 0;

        for (pix = 0; (pix < sliceSize) && !threadStopped; pix++) {

            if (entireImage || mask.get(pix)) {
                p5 = imgBuffer[pix] & 0xff;
                v = p5;

                if (v != bgColor) {

                    if ((pix - xDim - 1) < 0) {
                        p1 = bgColor;
                    } else {
                        p1 = imgBuffer[pix - xDim - 1] & 0xff;
                    }

                    if ((pix - xDim) < 0) {
                        p2 = bgColor;
                    } else {
                        p2 = imgBuffer[pix - xDim] & 0xff;
                    }

                    if (((pix - xDim + 1) < 0) || ((pix - xDim + 1) >= sliceSize)) {
                        p3 = bgColor;
                    } else {
                        p3 = imgBuffer[pix - xDim + 1] & 0xff;
                    }

                    if ((pix - 1) < 0) {
                        p4 = bgColor;
                    } else {
                        p4 = imgBuffer[pix - 1] & 0xff;
                    }

                    if ((pix + 1) >= sliceSize) {
                        p6 = bgColor;
                    } else {
                        p6 = imgBuffer[pix + 1] & 0xff;
                    }

                    if (((pix + xDim - 1) < 0) || ((pix + xDim - 1) >= sliceSize)) {
                        p7 = bgColor;
                    } else {
                        p7 = imgBuffer[pix + xDim - 1] & 0xff;
                    }

                    if ((pix + xDim) >= sliceSize) {
                        p8 = bgColor;
                    } else {
                        p8 = imgBuffer[pix + xDim] & 0xff;
                    }

                    if ((pix + xDim + 1) >= sliceSize) {
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

    /**
     * Ultimate erodes objects down to a single pixel (almost). Sometimes depending on the shape of the object (bar
     * bell) more than a single point. Most likely a good situation.
     *
     * @param  returnFlag  if true then this operation is a step in the morph process
     */
    private void ultimateErode(boolean returnFlag) {

        // if thread has already been stopped, dump out
        if (threadStopped) {
            finalize();

            return;
        }

        int pix, i, j;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;

        distanceMap(true);

        short[] arrayBuffer = new short[processBuffer.length];

        float cPt;

        for (pix = xDim + 1; pix < (sliceSize - xDim - 1); pix++) {

            if (imgBuffer[pix] > 0) {
                cPt = distanceMap[pix];

                if ((cPt >= distanceMap[pix - xDim]) && (cPt >= distanceMap[pix - xDim + 1]) &&
                        (cPt >= distanceMap[pix + 1]) && (cPt >= distanceMap[pix + xDim + 1]) &&
                        (cPt >= distanceMap[pix + xDim]) && (cPt >= distanceMap[pix + xDim - 1]) &&
                        (cPt >= distanceMap[pix - 1]) && (cPt >= distanceMap[pix - xDim - 1]) && // not on edge of the
                                                                                                     // object problem
                        (distanceMap[pix - xDim] != 0) && (distanceMap[pix - xDim + 1] != 0) &&
                        (distanceMap[pix + 1] != 0) && (distanceMap[pix + xDim + 1] != 0) &&
                        (distanceMap[pix + xDim] != 0) && (distanceMap[pix + xDim - 1] != 0) &&
                        (distanceMap[pix - 1] != 0) && (distanceMap[pix - xDim - 1] != 0)) {
                    arrayBuffer[pix] = 1; // (short)distanceMap[pix];
                } else {
                    arrayBuffer[pix] = 0;
                }
            }
        }

        if (threadStopped) {
            finalize();

            return;
        }

        // remove any border
        for (pix = 0; pix < sliceSize; pix++) {

            if ((pix == 0) && (arrayBuffer[pix] > 0) &&
                    ((arrayBuffer[pix + 1] != 0) || (arrayBuffer[pix + xDim + 1] != 0) ||
                         (arrayBuffer[pix + xDim] != 0))) {
                arrayBuffer[pix] = 0;
            } else if ((pix == (xDim - 1)) && (arrayBuffer[pix] > 0) &&
                           ((arrayBuffer[pix - 1] != 0) || (arrayBuffer[pix + xDim - 1] != 0) ||
                                (arrayBuffer[pix + xDim] != 0))) {
                arrayBuffer[pix] = 0;
            } else if ((pix == (sliceSize - xDim)) && (arrayBuffer[pix] > 0) &&
                           ((arrayBuffer[pix - xDim] != 0) || (arrayBuffer[pix - xDim + 1] != 0) ||
                                (arrayBuffer[pix + 1] != 0))) {
                arrayBuffer[pix] = 0;
            } else if ((pix == (sliceSize - 1)) && (arrayBuffer[pix] > 0) &&
                           ((arrayBuffer[pix - xDim] != 0) || (arrayBuffer[pix - xDim - 1] != 0) ||
                                (arrayBuffer[pix - 1] != 0))) {
                arrayBuffer[pix] = 0;
            } else if ((pix < xDim) && (arrayBuffer[pix] > 0) &&
                           ((arrayBuffer[pix + 1] != 0) || (arrayBuffer[pix + xDim + 1] != 0) ||
                                (arrayBuffer[pix + xDim] != 0) || (arrayBuffer[pix + xDim - 1] != 0) ||
                                (arrayBuffer[pix - 1] != 0))) {
                arrayBuffer[pix] = 0;
            } else if (((pix % xDim) == (xDim - 1)) && (arrayBuffer[pix] > 0) &&
                           ((arrayBuffer[pix + xDim] != 0) || (arrayBuffer[pix + xDim - 1] != 0) ||
                                (arrayBuffer[pix - 1] != 0) || (arrayBuffer[pix - xDim - 1] != 0) ||
                                (arrayBuffer[pix - xDim] != 0))) {
                arrayBuffer[pix] = 0;
            } else if (((pix % xDim) == 0) && (arrayBuffer[pix] > 0) &&
                           ((arrayBuffer[pix - xDim] != 0) || (arrayBuffer[pix - xDim + 1] != 0) ||
                                (arrayBuffer[pix + 1] != 0) || (arrayBuffer[pix + xDim + 1] != 0) ||
                                (arrayBuffer[pix = xDim] != 0))) {
                arrayBuffer[pix] = 0;
            } else if ((pix > (sliceSize - xDim)) && (arrayBuffer[pix] > 0) &&
                           ((arrayBuffer[pix - xDim] != 0) || (arrayBuffer[pix - xDim + 1] != 0) ||
                                (arrayBuffer[pix + 1] != 0) || (arrayBuffer[pix - 1] != 0) ||
                                (arrayBuffer[pix - xDim - 1] != 0))) {
                arrayBuffer[pix] = 0;
            }
        }

        if (threadStopped) {
            finalize();

            return;
        }

        identifyObjects(true); // Leaves  the IDed objects in processBuffer

        for (pix = 0; pix < sliceSize; pix++) {
            imgBuffer[pix] = arrayBuffer[pix];
        }

        boolean trimming = true;

        while ((trimming == true) && !threadStopped) {
            trimming = false;

            // not checking for stoppedthread here
            for (pix = xDim + 1; pix < (sliceSize - xDim - 1); pix++) {
                cPt = imgBuffer[pix];

                if (cPt > 0) {

                    if ((imgBuffer[pix - xDim] != 0) && (imgBuffer[pix + 1] == 0) && (imgBuffer[pix + xDim + 1] == 0) &&
                            (imgBuffer[pix + xDim] == 0) && (imgBuffer[pix + xDim - 1] == 0) &&
                            (imgBuffer[pix - 1] == 0) && !onePixel(arrayBuffer, pix, xDim)) {
                        arrayBuffer[pix] = 0;
                        trimming = true;
                    } else if ((imgBuffer[pix - xDim + 1] != 0) && (imgBuffer[pix + xDim + 1] == 0) &&
                                   (imgBuffer[pix + xDim] == 0) && (imgBuffer[pix + xDim - 1] == 0) &&
                                   (imgBuffer[pix - 1] == 0) && (imgBuffer[pix - xDim - 1] == 0) &&
                                   !onePixel(arrayBuffer, pix, xDim)) {
                        arrayBuffer[pix] = 0;
                        trimming = true;
                    } else if ((imgBuffer[pix - xDim] == 0) && (imgBuffer[pix + 1] != 0) &&
                                   (imgBuffer[pix + xDim] == 0) && (imgBuffer[pix + xDim - 1] == 0) &&
                                   (imgBuffer[pix - 1] == 0) && (imgBuffer[pix - xDim - 1] == 0) &&
                                   !onePixel(arrayBuffer, pix, xDim)) {
                        arrayBuffer[pix] = 0;
                        trimming = true;
                    } else if ((imgBuffer[pix - xDim] == 0) && (imgBuffer[pix - xDim + 1] == 0) &&
                                   (imgBuffer[pix + xDim + 1] != 0) && (imgBuffer[pix + xDim - 1] == 0) &&
                                   (imgBuffer[pix - 1] == 0) && (imgBuffer[pix - xDim - 1] == 0) &&
                                   !onePixel(arrayBuffer, pix, xDim)) {
                        arrayBuffer[pix] = 0;
                        trimming = true;
                    } else if ((imgBuffer[pix - xDim] == 0) && (imgBuffer[pix - xDim + 1] == 0) &&
                                   (imgBuffer[pix + 1] == 0) && (imgBuffer[pix + xDim] != 0) &&
                                   (imgBuffer[pix - 1] == 0) && (imgBuffer[pix - xDim - 1] == 0) &&
                                   !onePixel(arrayBuffer, pix, xDim)) {
                        arrayBuffer[pix] = 0;
                        trimming = true;
                    } else if ((imgBuffer[pix - xDim] == 0) && (imgBuffer[pix - xDim + 1] == 0) &&
                                   (imgBuffer[pix + 1] == 0) && (imgBuffer[pix + xDim + 1] == 0) &&
                                   (imgBuffer[pix + xDim - 1] != 0) && (imgBuffer[pix - xDim - 1] == 0) &&
                                   !onePixel(arrayBuffer, pix, xDim)) {
                        arrayBuffer[pix] = 0;
                        trimming = true;
                    } else if ((imgBuffer[pix - xDim] == 0) && (imgBuffer[pix - xDim + 1] == 0) &&
                                   (imgBuffer[pix + 1] == 0) && (imgBuffer[pix + xDim + 1] == 0) &&
                                   (imgBuffer[pix + xDim] == 0) && (imgBuffer[pix - 1] != 0) &&
                                   !onePixel(arrayBuffer, pix, xDim)) {
                        arrayBuffer[pix] = 0;
                        trimming = true;
                    } else if ((imgBuffer[pix - xDim + 1] == 0) && (imgBuffer[pix + 1] == 0) &&
                                   (imgBuffer[pix + xDim + 1] == 0) && (imgBuffer[pix + xDim] == 0) &&
                                   (imgBuffer[pix + xDim - 1] == 0) && (imgBuffer[pix - xDim - 1] != 0) &&
                                   !onePixel(arrayBuffer, pix, xDim)) {
                        arrayBuffer[pix] = 0;
                        trimming = true;
                    }
                }
            }

            for (pix = 0; pix < sliceSize; pix++) {
                imgBuffer[pix] = arrayBuffer[pix];
            }
        }

        // count eroded objects
        int nErodeObj = 0;

        for (pix = 0; pix < sliceSize; pix++) {

            if (imgBuffer[pix] > 0) {
                nErodeObj++;
            }
        }

        if (threadStopped) {
            finalize();

            return;
        }

        float xRes = srcImage.getFileInfo(0).getResolutions()[0];
        float xResSquared = xRes * xRes;
        float yRes = srcImage.getFileInfo(0).getResolutions()[1];
        float yResSquared = yRes * yRes;
        Point[] erodeObjs;
        Point[] pts;

        try {

            // get eroded points
            pts = new Point[nErodeObj];

            for (pix = 0, i = 0; pix < sliceSize; pix++) {

                if (imgBuffer[pix] > 0) {
                    pts[i] = new Point(pix, imgBuffer[pix]);
                    i++;
                }
            }

            if (threadStopped) {
                finalize();

                return;
            }

            // order points by max. distance (pt.y = imgBuffer[pix]);
            erodeObjs = new Point[nErodeObj];

            int max = -1, maxIndex = 0, index = 0;

            for (j = 0; j < nErodeObj; j++) {
                max = -1;

                for (i = 0; i < nErodeObj; i++) {

                    if (pts[i].y > max) {
                        max = pts[i].y;
                        maxIndex = pts[i].x;
                        index = i;
                    }
                }

                pts[index].y = -2;
                erodeObjs[j] = new Point(maxIndex % xDim, maxIndex / xDim);
            }

            // remove points which are two close together (multiple maximum)
            for (i = 0; i < (erodeObjs.length - 1); i++) {

                if (erodeObjs[i].x != -1) {

                    for (j = i + 1; j < erodeObjs.length; j++) {

                        if (erodeObjs[j].x != -1) {

                            if (processBuffer[(erodeObjs[i].y * xDim) + erodeObjs[i].x] ==
                                    processBuffer[(erodeObjs[j].y * xDim) + erodeObjs[j].x]) {

                                if (Math.sqrt(((erodeObjs[i].x - erodeObjs[j].x) * (erodeObjs[i].x - erodeObjs[j].x) *
                                                   xResSquared) +
                                                  ((erodeObjs[i].y - erodeObjs[j].y) *
                                                       (erodeObjs[i].y - erodeObjs[j].y) * yResSquared)) < pixDist) {
                                    imgBuffer[(erodeObjs[j].y * xDim) + erodeObjs[j].x] = 0;
                                    erodeObjs[j].x = -1;
                                }
                            }
                        }
                    }
                }
            }

            // count left over points
            for (i = 0, nErodeObj = 0; i < erodeObjs.length; i++) {

                if (erodeObjs[i].x != -1) {
                    nErodeObj++;
                }
            }

            // get points and put them in ultErodeObjects - which is a class object
            ultErodeObjects = new Point[nErodeObj];

            for (i = 0, j = 0; i < erodeObjs.length; i++) {

                if (erodeObjs[i].x != -1) {
                    ultErodeObjects[j] = erodeObjs[i];
                    j++;
                }
            }
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Morphology2D: Out of memory");
            setCompleted(false);


            return;
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
            displayError("Algorithm Morphology2D: Image(s) locked");
            setCompleted(false);


            return;
        } catch (OutOfMemoryError e) {
            displayError("Algorithm Morphology2D: Out of memory");
            setCompleted(false);


            return;
        }

        setCompleted(true);
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Simple class to temporarily store the object's size, ID and seed index value. This class is used by the
     * identifyObjects and deleteObjects methods of AlgorithmMorphology2D class.
     */
    private class intObject {

        /** DOCUMENT ME! */
        public short id = 0;

        /** DOCUMENT ME! */
        public int index = 0;

        /** DOCUMENT ME! */
        public int size = 0;

        /**
         * Initializes object with supplied parameters.
         *
         * @param  idx         seed index. Index is the location in the image
         * @param  objectID    the flood seed having a value >= 0.
         * @param  objectSize  the number of voxels in the object
         */
        public intObject(int idx, short objectID, int objectSize) {
            index = idx;
            id = objectID;
            size = objectSize;
        }
    }
}
