package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.Color;
import java.io.IOException;
import java.util.LinkedList;


/**
 * Pixel neighbors:
 * 
 * <pre>
 *  0 1 2
 *  3   4
 *  5 6 7
 * </pre>
 * 
 * The algorithm works as follows: I have implemented the Generalized Hough transform scheme originally sketched out by
 * Dana H. Ballard. At every point on a curve the gradient angle omega, perpendicular to the tangent and going into the
 * object, is found. Then, the distance r and angle beta of the line segment from the center of mass of the object to
 * the point on the curve is found. An R-table of gradient angle bins, linked to lists of all possible r, beta values is
 * created. In matching the object searching must occur over different scale factors and rotations of the object.
 * 
 * <pre>
 *  for (i = 0; i &lt; numPoints; i++) {
 *     Get gradient angle omega for point
 *    for (j = 0; j &lt;  thetaBins; j++) {
 *         rotation angle = theta[j]
 *         Get list of all r, beta values at gradient angle bin for omega - rotation angle
 *         for (k = 0; k &lt; size of r, beta list; k++) {
 *             Get r, beta values
 *             for (m = 0; m &lt; scaleBins; m++) {
 *                 xCenter = x + r*scaleFactor*cos[beta - rotation angle];
 *                 yCenter = y + r*scaleFactor*sin[beta - rotation angle];
 * </pre>
 * 
 * so a bin in 4D space of xCenter, yCenter, rotation angle, scaleFactor is incremented.
 * 
 * The method seems to work well if only either scale factor or rotation angle is used, but often produces wrong answers
 * if both scale factor and rotation angle are used. One reason is that in 3D space I can keep the bin width small
 * enough for a thorough search, but in 4D memory restrictions force me to increase bin width.
 * 
 * References: 1.) Computer Vision by Dana H. Ballard and Christopher M. Brown, Prentice-Hall, Inc., 1982, Section 4.3.4
 * Generalizing the Hough Transform, pp. 128-131.
 */
public class AlgorithmFindRtableObject extends AlgorithmBase {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------
    /**
     * Number of bins covering gradient angle going from 0 to 2*PI Gradient is taken as going into the object
     */
    private int omegaBins = 90;

    /**
     * Number of points to take from each side of a point on a curve in determining a tangent If only 1 point is used on
     * each side, simply use average of slopes to each of the neigboring points.
     */
    private int sidePointsForTangent;

    /**
     * For each omega angle index, a linked list of R and B values is present omega is the gradient angle to the curve,
     * where the gradient is taken as going into the object, and R and B give the distance and angle from the center of
     * the VOI to the tangent point
     */
    private LinkedList omegaRBetaList[];

    /**
     * Desired maximum pixel bin width for x, y center value. If maxBufferSize is not large enough, this number is
     * increased.
     */
    private float maxPixelBinWidth;

    /** The maximum Hough transform size in megabytes - default is currently 256 */
    private int maxBufferSize;

    /** If true, allow rotation of R-table object */
    private boolean allowRotation;

    /**
     * Desired maximum bin width for R-table object rotation. If maxBufferSize is not large enough, this number is
     * increased.
     */
    private float maxDegreesBinWidth;

    /** If true, allow scaling of R-table object */
    private boolean allowScaling;

    private float minScaleFactor;

    private float maxScaleFactor;

    /**
     * Number of scaling bins if memory is not limited. If maxBufferSize is not large enough, this number is reduced.
     */
    private int scaleBins;

    /** Number of instances of R-table object to find in the image */
    private int objectsToFind;

    // VOI center of mass of creation contour
    private float creationCenterX;

    private float creationCenterY;

    private int creationXDim;

    private int creationNumPoints;

    // contains index = x + xDim*y of creation points
    private int creationIndexArray[];

    // Slope of creations points
    private float creationOmegaArray[];

    // Curvature of creation points
    private float creationCurvatureArray[];

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmFindRtableObject - default constructor.
     */
    public AlgorithmFindRtableObject() {}

    /**
     * AlgorithmFindRtableObject.
     * 
     * @param srcImg Binary image that has 1 or more instances of the R-table object
     * @param omegaBins Number of bins for gradient angle at point on curve going from 0 to 2*PI radians
     * @param sidePointsForTangent Maximum number of points to take from each side of a point on a curve in determining
     *            the tangent
     * @param omegaRBetaList For each omega angle index, a linked list of R and B values is present omega is the
     *            gradient angle to the curve, with the gradient defined as going into the object, and R and B give the
     *            distance and angle from the center of the VOI to the tangent point
     * @param maxPixelBinWidth Desired maximum pixel bin width for x, y center value. If maxBufferSize is not large
     *            enough, this number is increased.
     * @param maxBufferSize The maximum Hough transform size in megabytes - default is currently 256
     * @param allowRotation If true, allow rotation of R-table object
     * @param maxDegreesBinWidth Desired maximum angle bin width for object rotation If maxBufferSize is not large
     *            enough, this number is increased.
     * @param allowScaling If true, allow scaling of R-table object
     * @param minScaleFactor
     * @param maxScaleFactor
     * @param scaleBins Desired number of scaling bins of entry. If maxBufferSize is not large enough, this number is
     *            reduced.
     * @param objectsToFind Number of instances of R-table object to find in the image
     */
    public AlgorithmFindRtableObject(ModelImage srcImg, int omegaBins, int sidePointsForTangent,
            LinkedList omegaRBetaList[], float maxPixelBinWidth, int maxBufferSize, boolean allowRotation,
            float maxDegreesBinWidth, boolean allowScaling, float minScaleFactor, float maxScaleFactor, int scaleBins,
            int objectsToFind) {
        super(null, srcImg);
        this.omegaBins = omegaBins;
        this.sidePointsForTangent = sidePointsForTangent;
        this.omegaRBetaList = omegaRBetaList;
        this.maxPixelBinWidth = maxPixelBinWidth;
        this.maxBufferSize = maxBufferSize;
        this.allowRotation = allowRotation;
        this.maxDegreesBinWidth = maxDegreesBinWidth;
        this.allowScaling = allowScaling;
        this.minScaleFactor = minScaleFactor;
        this.maxScaleFactor = maxScaleFactor;
        this.scaleBins = scaleBins;
        this.objectsToFind = objectsToFind;
    }

    /**
     * Constructor for curvature method that works far more poorly than the traditional method without curvature
     * 
     * @param srcImg
     * @param creationCenterX
     * @param creationCenterY
     * @param sidePointsForTangent
     * @param creationXDim
     * @param creationIndexArray
     * @param creationOmegaArray
     * @param creationCurvatureArray
     * @param maxPixelBinWidth
     * @param allowRotation
     * @param maxDegreesBinWidth
     * @param allowScaling
     * @param minScaleFactor
     * @param maxScaleFactor
     * @param scaleBins
     * @param objectsToFind
     */
    public AlgorithmFindRtableObject(ModelImage srcImg, float creationCenterX, float creationCenterY,
            int sidePointsForTangent, int creationXDim, int creationIndexArray[], float creationOmegaArray[],
            float creationCurvatureArray[], float maxPixelBinWidth, boolean allowRotation, float maxDegreesBinWidth,
            boolean allowScaling, float minScaleFactor, float maxScaleFactor, int scaleBins, int objectsToFind) {
        super(null, srcImg);
        this.creationCenterX = creationCenterX;
        this.creationCenterY = creationCenterY;
        this.sidePointsForTangent = sidePointsForTangent;
        this.creationXDim = creationXDim;
        this.creationIndexArray = creationIndexArray;
        this.creationOmegaArray = creationOmegaArray;
        this.creationCurvatureArray = creationCurvatureArray;
        this.maxPixelBinWidth = maxPixelBinWidth;
        this.allowRotation = allowRotation;
        this.maxDegreesBinWidth = maxDegreesBinWidth;
        this.allowScaling = allowScaling;
        // Dialog assumes scale factor = object size/model size, but equations use
        // model size/object size
        this.maxScaleFactor = 1.0f / minScaleFactor;
        this.minScaleFactor = 1.0f / maxScaleFactor;
        this.scaleBins = scaleBins;
        this.objectsToFind = objectsToFind;
        creationNumPoints = creationIndexArray.length;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * finalize -
     */
    public void finalize() {
        super.finalize();
    }

    /**
     * Starts the program. * Uses curvature based on the linefit method, which is a very poor measurement curvature =
     * (difference in tangent angles/distance between tangent points) According to Marcel Worring and Arnold W. M.
     * Smeulders in "Digital Curvature Estimation", CVGIP: Image Understanding, Vol. 58, No. 3, November, 1993, pp.
     * 366-382: The Linefit method, "based on fitting two straight lines, has poor performance. Bias is -27% to +24%,
     * where deviation is 5.8%-68%. The errors are a direct consequence of the method of differential filtering and of
     * the arclength estimator used. Orientation estimation is reliable, however. Curvature estimates are improved by
     * using a Gaussian differential kernel and a better arclength estimator."
     * 
     * "Another source of inaccuracy is the fact that the estimated derivative is divided by the length between discrete
     * pixels. The distance between pixels is restricted to either 1 or sqrt(2), a poor estimate of predigitized
     * arclength."
     * 
     * This method does not work well with either only rotation or only scaling.
     * 
     * Find the tangent angles and curvatures at every point in both the model and the object under investigation.
     */
    public void runCurvatureAlgorithm() {
        int x, y;
        float creationX = 0.0f;
        float creationY = 0.0f;
        int offset;

        int xDim;

        int yDim;

        int sourceSlice;

        int numPoints;

        int i, j, k, n;
        int index;
        byte[] srcBuffer;
        boolean test = false;
        double xCenter;
        double yCenter;
        int indexArray[];
        int newIndexArray[];
        int neighbors;
        int endPoints;
        int numOpenCurves;
        int numClosedCurves;
        boolean foundArray[];
        int openStart[];
        int openLength[];
        int closedLength;
        int indexPtr = 0;
        int nextPoint;
        float xArray[];
        float yArray[];
        float tangentX;
        float tangentY;
        float normalX;
        float normalY;
        boolean openArray[];
        boolean newOpenArray[];
        float omegaArray[];
        float newOmegaArray[];
        float curvatureArray[];
        float newCurvatureArray[];
        int startPtr;
        int presentSidePoints;
        float xPoints[];
        float yPoints[];
        int endPtr;
        int startWrapPoints;
        int endWrapPoints;
        int neighbor1[];
        int neighbor2[];
        AlgorithmMorphology2D algoMorph2D;
        int neigh0;
        int neigh1;
        int neigh2;
        int neigh3;
        int neigh4;
        int neigh5;
        int neigh6;
        int neigh7;
        int pruningPix;
        boolean entireImage;
        double var;
        int xBins;
        int yBins;
        int thetaBins;
        double maxRadiansDiff;
        boolean foundPoint[];
        float xCenterTable[];
        float yCenterTable[];
        float thetaTable[] = null;
        float scaleTable[] = null;
        short countTable[];
        int pointsDeleted;
        int newNumPoints;
        float xpc;
        float ypc;
        float lastxpc = 0.0f;
        float lastypc = 0.0f;
        float xpc0 = 0.0f;
        float ypc0 = 0.0f;
        double xSqSum;
        double ySqSum;
        double xySum;
        double x1t;
        double x2t;
        double y1t;
        double y2t;
        double d1;
        double d2;
        double slope;
        int objectsFound = 0;
        float thetaArray[] = null;
        double maxToMinRatio;
        float scaleArray[] = null;
        int maxXIndex = 0;
        int maxYIndex = 0;
        int maxThetaIndex = 0;
        int maxScaleIndex = 0;
        VOI centerPtVOI;
        float xArr[] = new float[1];
        float yArr[] = new float[1];
        float zArr[] = new float[1];
        ModelImage maskImage;
        double omega;
        double lastOmega = 0.0;
        double omega0 = 0.0;
        double creationOmega;
        VOI closedVOI;
        int closedX[];
        int closedY[];
        int xVal[];
        int yVal[];
        int zVal[];
        double deltaOmega;
        double curveXDist;
        double curveYDist;
        double distance;
        float curvature = 0.0f;
        float creationCurvature;
        float rotationArray[][];
        float scalingArray[][];
        float xCenterArray[][];
        float yCenterArray[][];
        int rsCountArray[][];
        int xyCountArray[][];
        double theta = 0.0;
        float scaleFactor = 1.0f;
        double radiansPerBin = 0.0;
        int thetaIndex = 0;
        double betweenBinsScale;
        double logBetweenScale = 1.0;
        int scaleIndex = 0;
        double costheta = 0.0;
        double sintheta = 0.0;
        double tantheta = 0.0;
        float xPixelsPerBin;
        float yPixelsPerBin;
        int xIndex;
        int yIndex;
        int maxXYCount;
        int maxRSCount = 0;
        short VOIMask[];
        int xt;
        int yt;
        int indext;

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        fireProgressStateChanged(srcImage.getImageName(), "Find R-table object ...");

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sourceSlice = xDim * yDim;

        srcBuffer = new byte[sourceSlice];

        try {
            srcImage.exportData(0, sourceSlice, srcBuffer);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on srcImage.exportData");

            setCompleted(false);

            return;
        }

        maskImage = (ModelImage) srcImage.clone();

        // Skeletonize the binary image
        // Prune off branches with 2 or less pixels
        pruningPix = 2;
        entireImage = true;
        algoMorph2D = new AlgorithmMorphology2D(maskImage, 0, 0.0f, AlgorithmMorphology2D.SKELETONIZE, 0, 0,
                pruningPix, 0, entireImage);
        algoMorph2D.run();
        algoMorph2D.finalize();

        try {
            maskImage.exportData(0, sourceSlice, srcBuffer);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on destImage.exportData");

            setCompleted(false);

            return;
        }

        // When a diagonal neighbor is adjacent to a horizontal or vertical neighbor,
        // remove the horizontal or vertical neighbor
        for (y = 0; y < yDim; y++) {
            offset = y * xDim;
            for (x = 0; x < xDim; x++) {
                index = offset + x;
                if (srcBuffer[index] != 0) {
                    neighbors = 0;
                    neigh0 = -1;
                    neigh1 = -1;
                    neigh2 = -1;
                    neigh3 = -1;
                    neigh4 = -1;
                    neigh5 = -1;
                    neigh6 = -1;
                    neigh7 = -1;
                    if (y > 0) {
                        if (x > 0) {
                            if (srcBuffer[index - xDim - 1] != 0) {
                                neighbors++;
                                neigh0 = index - xDim - 1;
                            }
                        }
                        if (srcBuffer[index - xDim] != 0) {
                            neighbors++;
                            neigh1 = index - xDim;
                        }
                        if (x < xDim - 1) {
                            if (srcBuffer[index - xDim + 1] != 0) {
                                neighbors++;
                                neigh2 = index - xDim + 1;
                            }
                        }
                    } // if (y > 0)
                    if (x > 0) {
                        if (srcBuffer[index - 1] != 0) {
                            neighbors++;
                            neigh3 = index - 1;
                        }
                    } // if (x > 0)
                    if (x < xDim - 1) {
                        if (srcBuffer[index + 1] != 0) {
                            neighbors++;
                            neigh4 = index + 1;
                        }
                    } // if (x < xDim - 1)
                    if (y < yDim - 1) {
                        if (x > 0) {
                            if (srcBuffer[index + xDim - 1] != 0) {
                                neighbors++;
                                neigh5 = index + xDim - 1;
                            }
                        }
                        if (srcBuffer[index + xDim] != 0) {
                            neighbors++;
                            neigh6 = index + xDim;
                        }
                        if (x < xDim - 1) {
                            if (srcBuffer[index + xDim + 1] != 0) {
                                neighbors++;
                                neigh7 = index + xDim + 1;
                            }
                        }
                    } // if (y < yDim - 1)
                    if (neighbors > 2) {
                        // Could be 3 or 4
                        if ( (neigh0 >= 0) && (neigh1 >= 0)) {
                            srcBuffer[neigh1] = 0;
                            neigh1 = -1;
                        }
                        if ( (neigh1 >= 0) && (neigh2 >= 0)) {
                            srcBuffer[neigh1] = 0;
                            neigh1 = -1;
                        }
                        if ( (neigh0 >= 0) && (neigh3 >= 0)) {
                            srcBuffer[neigh3] = 0;
                            neigh3 = -1;
                        }
                        if ( (neigh3 >= 0) && (neigh5 >= 0)) {
                            srcBuffer[neigh3] = 0;
                            neigh3 = -1;
                        }
                        if ( (neigh2 >= 0) && (neigh4 >= 0)) {
                            srcBuffer[neigh4] = 0;
                            neigh4 = -1;
                        }
                        if ( (neigh4 >= 0) && (neigh7 >= 0)) {
                            srcBuffer[neigh4] = 0;
                            neigh4 = -1;
                        }
                        if ( (neigh5 >= 0) && (neigh6 >= 0)) {
                            srcBuffer[neigh6] = 0;
                            neigh6 = -1;
                        }
                        if ( (neigh6 >= 0) && (neigh7 >= 0)) {
                            srcBuffer[neigh6] = 0;
                            neigh6 = -1;
                        }
                    }
                } // if (srcBuffer[index] != 0)
            } // for (x = 0; x < xDim; x++)
        } // for (y = 0; y < yDim; y++)

        // Remove points with more than 2 neighbors
        for (y = 0; y < yDim; y++) {
            offset = y * xDim;
            for (x = 0; x < xDim; x++) {
                index = offset + x;
                if (srcBuffer[index] != 0) {
                    neighbors = 0;
                    if (y > 0) {
                        if (x > 0) {
                            if (srcBuffer[index - xDim - 1] != 0) {
                                neighbors++;
                            }
                        }
                        if (srcBuffer[index - xDim] != 0) {
                            neighbors++;
                        }
                        if (x < xDim - 1) {
                            if (srcBuffer[index - xDim + 1] != 0) {
                                neighbors++;
                            }
                        }
                    } // if (y > 0)
                    if (x > 0) {
                        if (srcBuffer[index - 1] != 0) {
                            neighbors++;
                        }
                    } // if (x > 0)
                    if (x < xDim - 1) {
                        if (srcBuffer[index + 1] != 0) {
                            neighbors++;
                        }
                    } // if (x < xDim - 1)
                    if (y < yDim - 1) {
                        if (x > 0) {
                            if (srcBuffer[index + xDim - 1] != 0) {
                                neighbors++;
                            }
                        }
                        if (srcBuffer[index + xDim] != 0) {
                            neighbors++;
                        }
                        if (x < xDim - 1) {
                            if (srcBuffer[index + xDim + 1] != 0) {
                                neighbors++;
                            }
                        }
                    } // if (y < yDim - 1)
                    if (neighbors > 2) {
                        srcBuffer[index] = 0;
                    }
                } // if (srcBuffer[index] != 0)
            } // for (x = 0; x < xDim; x++)
        } // for (y = 0; y < yDim; y++)

        // Find the 1 or 2 neighbors of every point
        // Find the number of end points, that is, points with only 1 neighbor
        // Delete isolated points with no neighbors
        numPoints = 0;
        endPoints = 0;
        neighbor1 = new int[sourceSlice];
        neighbor2 = new int[sourceSlice];
        for (i = 0; i < sourceSlice; i++) {
            neighbor1[i] = -1;
            neighbor2[i] = -1;
        }
        for (y = 0; y < yDim; y++) {
            offset = y * xDim;
            for (x = 0; x < xDim; x++) {
                index = offset + x;
                if (srcBuffer[index] != 0) {
                    neighbors = 0;
                    if (y > 0) {
                        if (x > 0) {
                            if (srcBuffer[index - xDim - 1] != 0) {
                                neighbors++;
                                neighbor1[index] = index - xDim - 1;
                            }
                        }
                        if (srcBuffer[index - xDim] != 0) {
                            neighbors++;
                            if (neighbor1[index] == -1) {
                                neighbor1[index] = index - xDim;
                            } else {
                                neighbor2[index] = index - xDim;
                            }
                        }
                        if (x < xDim - 1) {
                            if (srcBuffer[index - xDim + 1] != 0) {
                                neighbors++;
                                if (neighbor1[index] == -1) {
                                    neighbor1[index] = index - xDim + 1;
                                } else {
                                    neighbor2[index] = index - xDim + 1;
                                }
                            }
                        }
                    } // if (y > 0)
                    if (x > 0) {
                        if (srcBuffer[index - 1] != 0) {
                            neighbors++;
                            if (neighbor1[index] == -1) {
                                neighbor1[index] = index - 1;
                            } else {
                                neighbor2[index] = index - 1;
                            }
                        }
                    } // if (x > 0)
                    if (x < xDim - 1) {
                        if (srcBuffer[index + 1] != 0) {
                            neighbors++;
                            if (neighbor1[index] == -1) {
                                neighbor1[index] = index + 1;
                            } else {
                                neighbor2[index] = index + 1;
                            }
                        }
                    } // if (x < xDim - 1)
                    if (y < yDim - 1) {
                        if (x > 0) {
                            if (srcBuffer[index + xDim - 1] != 0) {
                                neighbors++;
                                if (neighbor1[index] == -1) {
                                    neighbor1[index] = index + xDim - 1;
                                } else {
                                    neighbor2[index] = index + xDim - 1;
                                }
                            }
                        }
                        if (srcBuffer[index + xDim] != 0) {
                            neighbors++;
                            if (neighbor1[index] == -1) {
                                neighbor1[index] = index + xDim;
                            } else {
                                neighbor2[index] = index + xDim;
                            }
                        }
                        if (x < xDim - 1) {
                            if (srcBuffer[index + xDim + 1] != 0) {
                                neighbors++;
                                if (neighbor1[index] == -1) {
                                    neighbor1[index] = index + xDim + 1;
                                } else {
                                    neighbor2[index] = index + xDim + 1;
                                }
                            }
                        }
                    } // if (y < yDim - 1)
                    if (neighbors == 0) {
                        srcBuffer[index] = 0;
                        neighbor1[index] = -1;
                        neighbor2[index] = -1;
                    } else {
                        numPoints++;
                        if (neighbors == 1) {
                            endPoints++;
                        }
                    }
                } // if (srcBuffer[index] != 0)
            } // for (x = 0; x < xDim; x++)
        } // for (y = 0; y < yDim; y++)

        numOpenCurves = endPoints / 2;
        openStart = new int[numOpenCurves];
        openLength = new int[numOpenCurves];
        foundArray = new boolean[sourceSlice];
        // Set foundArray to true at every zero location
        for (y = 0; y < yDim; y++) {
            offset = y * xDim;
            for (x = 0; x < xDim; x++) {
                index = offset + x;
                if (srcBuffer[index] == 0) {
                    foundArray[index] = true;
                }
            }
        }

        // Find the starting positions and lengths of the open curves
        i = 0;
        for (y = 0; y < yDim; y++) {
            offset = y * xDim;
            for (x = 0; x < xDim; x++) {
                index = offset + x;
                if ( (neighbor2[index] == -1) && ( !foundArray[index])) {
                    foundArray[index] = true;
                    openStart[i] = index;
                    openLength[i]++;
                    index = neighbor1[index];
                    foundArray[index] = true;
                    openLength[i]++;
                    while (neighbor2[index] != -1) {
                        if ( !foundArray[neighbor1[index]]) {
                            index = neighbor1[index];
                        } else {
                            index = neighbor2[index];
                        }
                        foundArray[index] = true;
                        openLength[i]++;
                    } // (while(neighbor2[index] != -1)
                    // Delete all open curves with only 2 points
                    // Also don't determine tangents of end points on longer curves,
                    // but use these 2 end points in determining tangents of more inner points
                    numPoints = numPoints - 2;
                    if (openLength[i] == 2) {
                        srcBuffer[openStart[i]] = 0;
                        srcBuffer[neighbor1[openStart[i]]] = 0;
                        numOpenCurves--;
                        openLength[i] = 0;
                    } else {
                        i++;
                    }
                }
            }
        }

        ViewUserInterface.getReference().setDataText("Number of open curves = " + numOpenCurves + "\n");

        // For the open curves find the tangent and normal to a point on a curve
        // With a user specified sidePointsForTangent on each side of a point find the tangent line that
        // minimizes the sum of the squared distances from these side points to the tangent line
        indexArray = new int[numPoints];
        openArray = new boolean[numPoints];
        omegaArray = new float[numPoints];
        curvatureArray = new float[numPoints];
        for (i = 0; i < sourceSlice; i++) {
            if (srcBuffer[i] != 0) {
                foundArray[i] = false;
            }
        }
        indexPtr = 0;
        for (i = 0; i < numOpenCurves; i++) {
            startPtr = indexPtr;
            xArray = new float[openLength[i]];
            yArray = new float[openLength[i]];
            nextPoint = openStart[i];
            xArray[0] = nextPoint % xDim;
            yArray[0] = nextPoint / xDim;
            foundArray[nextPoint] = true;
            for (n = 1; n <= openLength[i] - 1; n++) {
                if ( !foundArray[neighbor1[nextPoint]]) {
                    nextPoint = neighbor1[nextPoint];
                } else {
                    nextPoint = neighbor2[nextPoint];
                }
                if (n <= openLength[i] - 2) {
                    indexArray[indexPtr++] = nextPoint;
                }
                xArray[n] = nextPoint % xDim;
                yArray[n] = nextPoint / xDim;
                foundArray[nextPoint] = true;
            } // for (n = 1; n <= openLength[i] - 1; n++)
            indexPtr = startPtr;
            for (n = 1; n <= openLength[i] - 2; n++) {
                presentSidePoints = Math.min(sidePointsForTangent, n);
                presentSidePoints = Math.min(presentSidePoints, openLength[i] - 1 - n);
                if (presentSidePoints == 1) {
                    xpc = xArray[n];
                    ypc = yArray[n];
                    tangentX = (xArray[n + 1] - xArray[n - 1]) / 2.0f;
                    tangentY = (yArray[n + 1] - yArray[n - 1]) / 2.0f;
                    if (tangentX == 0) {
                        normalX = 1;
                        normalY = 0;
                    } else if (tangentY == 0) {
                        normalX = 0;
                        normalY = 1;
                    } else {
                        normalX = -tangentY / tangentX;
                        normalY = 1;
                    }
                } // if (presentSidePoints == 1)
                else {
                    xPoints = new float[2 * presentSidePoints + 1];
                    yPoints = new float[2 * presentSidePoints + 1];
                    for (k = 0, j = n - presentSidePoints; j <= n + presentSidePoints; j++, k++) {
                        xPoints[k] = xArray[j];
                        yPoints[k] = yArray[j];
                    }
                    // Center all points for tangent point touching curve at (0, 0)
                    // That is, use an x axis and a y axis going thru the tangent point
                    xpc = xPoints[sidePointsForTangent];
                    ypc = yPoints[sidePointsForTangent];
                    for (k = 0; k < xPoints.length; k++) {
                        xPoints[k] = xPoints[k] - xpc;
                        yPoints[k] = yPoints[k] - ypc;
                    }
                    xSqSum = 0.0;
                    ySqSum = 0.0;
                    xySum = 0.0;
                    for (k = 0; k < xPoints.length; k++) {
                        xSqSum += xPoints[k] * xPoints[k];
                        ySqSum += yPoints[k] * yPoints[k];
                        xySum += xPoints[k] * yPoints[k];
                    }
                    if (xySum != 0.0) {
                        var = Math
                                .sqrt(ySqSum * ySqSum - 2.0 * xSqSum * ySqSum + xSqSum * xSqSum + 4.0 * xySum * xySum);
                        x1t = 0.5 * ( ( -ySqSum + xSqSum + var) / xySum);
                        x2t = 0.5 * ( ( -ySqSum + xSqSum - var) / xySum);
                        y1t = 1.0;
                        y2t = 1.0;
                    } else {
                        // If all points are symmetric to either this new x axis or this new y axis, then
                        // their product sum is 0 and the tangentX, tangentY must be 1,0 or 0,1
                        x1t = 1.0;
                        x2t = 0.0;
                        y1t = 0.0;
                        y2t = 1.0;
                    }
                    // x1t, y1t and x2t, y2t are perpindicular. To find the solution, calculate the sum of
                    // distances from the curve points to the line for the 2 cases
                    // The shortest distance is the correct solution
                    // Distance from AX + BY + C = 0 to P1 is
                    // abs((A*x1 + B*y1 + C))/sqrt(A**2 + B**2)
                    // Here A = slope, B = -1, and C = 0.
                    d1 = 0.0;
                    for (k = 0; k < xPoints.length; k++) {
                        if (x1t == 0.0) {
                            // Infinite slope thru (0,0)
                            d1 += Math.abs(xPoints[k]);
                        } else if (y1t == 0.0) {
                            // Zero slope thru (0, 0)
                            d1 += Math.abs(yPoints[k]);
                        } else {
                            slope = y1t / x1t;
                            d1 += Math.abs( (slope * xPoints[k] - yPoints[k]) / Math.sqrt(slope * slope + 1));
                        }
                    }
                    d2 = 0.0;
                    for (k = 0; k < xPoints.length; k++) {
                        if (x2t == 0.0) {
                            // Infinite slope thru (0,0)
                            d2 += Math.abs(xPoints[k]);
                        } else if (y2t == 0.0) {
                            // Zero slope thru (0, 0)
                            d2 += Math.abs(yPoints[k]);
                        } else {
                            slope = y2t / x2t;
                            d2 += Math.abs( (slope * xPoints[k] - yPoints[k]) / Math.sqrt(slope * slope + 1));
                        }
                    }
                    if (d1 < d2) {
                        tangentX = (float) x1t;
                        tangentY = (float) y1t;
                        normalX = (float) x2t;
                        normalY = (float) y2t;
                    } else {
                        tangentX = (float) x2t;
                        tangentY = (float) y2t;
                        normalX = (float) x1t;
                        normalY = (float) y1t;
                    }
                }
                omega = Math.atan2(normalY, normalX);
                // Change omega range from -PI to PI to 0 to 2*PI
                if (omega < 0.0) {
                    omega = omega + 2.0 * Math.PI;
                }
                if (n >= 2) {
                    curveXDist = xpc - lastxpc;
                    curveYDist = ypc - lastypc;
                    distance = Math.sqrt(curveXDist * curveXDist + curveYDist * curveYDist);
                    deltaOmega = omega - lastOmega;
                    curvature = (float) (deltaOmega / distance);
                    omegaArray[indexPtr] = (float) omega;
                    curvatureArray[indexPtr] = curvature;
                    openArray[indexPtr++] = true;
                }

                if (n == openLength[i] - 2) {
                    omegaArray[indexPtr] = (float) omega;
                    curvatureArray[indexPtr] = curvature;
                    openArray[indexPtr++] = true;
                }
                lastxpc = xpc;
                lastypc = ypc;
                lastOmega = omega;
            } // for (n = 1; n <= openLength[i] - 2; n++)
        } // for (i = 0; i < numOpenCurves; i++)
        openStart = null;
        openLength = null;
        xArray = null;
        yArray = null;

        // Find a position and length of closed curve
        numClosedCurves = 0;
        xPoints = new float[2 * sidePointsForTangent + 1];
        yPoints = new float[2 * sidePointsForTangent + 1];
        closedX = new int[2 * (xDim + yDim)];
        closedY = new int[2 * (xDim + yDim)];
        VOIMask = new short[sourceSlice];
        for (y = 0; y < yDim; y++) {
            offset = y * xDim;
            for (x = 0; x < xDim; x++) {
                index = offset + x;
                if ( !foundArray[index]) {
                    startPtr = indexPtr;
                    foundArray[index] = true;
                    numClosedCurves++;
                    closedVOI = new VOI((short) numClosedCurves, "VOI" + numClosedCurves, 1, VOI.CONTOUR, -1.0f);
                    closedX[0] = x;
                    closedY[0] = y;
                    closedLength = 1;
                    indexArray[indexPtr++] = index;
                    while ( ( !foundArray[neighbor1[index]]) || ( !foundArray[neighbor2[index]])) {
                        if ( !foundArray[neighbor1[index]]) {
                            index = neighbor1[index];
                        } else {
                            index = neighbor2[index];
                        }
                        foundArray[index] = true;
                        closedX[closedLength] = index % xDim;
                        closedY[closedLength] = index / xDim;
                        closedLength++;
                        indexArray[indexPtr++] = index;
                    } // while ((!foundArray[neighbor1[index]]) || (!foundArray[neighbor2[index]]))
                    xVal = new int[closedLength];
                    yVal = new int[closedLength];
                    zVal = new int[closedLength];
                    for (i = 0; i < closedLength; i++) {
                        xVal[i] = closedX[i];
                        yVal[i] = closedY[i];
                    }
                    closedVOI.importCurve(xVal, yVal, zVal, 0);
                    VOIMask = closedVOI.createShortMask(xDim, yDim, VOIMask, false);
                    try {
                        closedVOI.finalize();
                    } catch (Throwable e) {
                        ViewUserInterface.getReference().setDataText("Exception on closedVOI.finalize()");
                    }
                    closedVOI = null;
                    endPtr = indexPtr - 1;
                    indexPtr = startPtr;
                    for (n = 0; n <= closedLength - 1; n++) {
                        // Put the tangent point at index sidePointsForTangent in the
                        // center of the xPoints and yPoints array with sidePointsForTangent points
                        // to each side.
                        startWrapPoints = Math.max(0, sidePointsForTangent - n);
                        endWrapPoints = Math.max(0, sidePointsForTangent - (closedLength - 1 - n));
                        for (k = 0; k < startWrapPoints; k++) {
                            xPoints[k] = indexArray[endPtr - (startWrapPoints - k)] % xDim;
                            yPoints[k] = indexArray[endPtr - (startWrapPoints - k)] / xDim;
                        }
                        for (k = startWrapPoints, j = indexPtr - sidePointsForTangent + startWrapPoints; k < 2
                                * sidePointsForTangent + 1 - endWrapPoints; j++, k++) {
                            xPoints[k] = indexArray[j] % xDim;
                            yPoints[k] = indexArray[j] / xDim;
                        }
                        for (j = 0, k = 2 * sidePointsForTangent + 1 - endWrapPoints; k < 2 * sidePointsForTangent + 1; j++, k++) {
                            xPoints[k] = indexArray[startPtr + j] % xDim;
                            yPoints[k] = indexArray[startPtr + j] / xDim;
                        }
                        // For the closed curve find the slope and y axis intercept of the tangent line to the curve at
                        // a point
                        // With a user specified sidePointsForTangent on each side of a point find the tangent line that
                        // minimizes the sum of the squared distances from these side points to the tangent line
                        if (sidePointsForTangent == 1) {
                            xpc = xPoints[1];
                            ypc = yPoints[1];
                            tangentX = (xPoints[2] - xPoints[0]) / 2.0f;
                            tangentY = (yPoints[2] - yPoints[0]) / 2.0f;
                            if (tangentX == 0) {
                                normalX = 1;
                                normalY = 0;
                            } else if (tangentY == 0) {
                                normalX = 0;
                                normalY = 1;
                            } else {
                                normalX = -tangentY / tangentX;
                                normalY = 1;
                            }
                        } // if (sidePointsForTangent == 1)
                        else {
                            // Center all points for tangent point touching curve at (0, 0)
                            // That is, use an x axis and a y axis going thru the tangent point
                            xpc = xPoints[sidePointsForTangent];
                            ypc = yPoints[sidePointsForTangent];
                            for (k = 0; k < xPoints.length; k++) {
                                xPoints[k] = xPoints[k] - xpc;
                                yPoints[k] = yPoints[k] - ypc;
                            }
                            xSqSum = 0.0;
                            ySqSum = 0.0;
                            xySum = 0.0;
                            for (k = 0; k < xPoints.length; k++) {
                                xSqSum += xPoints[k] * xPoints[k];
                                ySqSum += yPoints[k] * yPoints[k];
                                xySum += xPoints[k] * yPoints[k];
                            }
                            if (xySum != 0.0) {
                                var = Math.sqrt(ySqSum * ySqSum - 2.0 * xSqSum * ySqSum + xSqSum * xSqSum + 4.0 * xySum
                                        * xySum);
                                x1t = 0.5 * ( ( -ySqSum + xSqSum + var) / xySum);
                                x2t = 0.5 * ( ( -ySqSum + xSqSum - var) / xySum);
                                y1t = 1.0;
                                y2t = 1.0;
                            } else {
                                // If all points are symmetric to either this new x axis or this new y axis, then
                                // their product sum is 0 and the tangentX, tangentY must be 1,0 or 0,1
                                x1t = 1.0;
                                x2t = 0.0;
                                y1t = 0.0;
                                y2t = 1.0;
                            }
                            // x1t, y1t and x2t, y2t are perpindicular. To find the solution, calculate the sum of
                            // distances from the curve points to the line for the 2 cases
                            // The shortest distance is the correct solution
                            // Distance from AX + BY + C = 0 to P1 is
                            // abs((A*x1 + B*y1 + C))/sqrt(A**2 + B**2)
                            // Here A = slope, B = -1, and C = 0.
                            d1 = 0.0;
                            for (k = 0; k < xPoints.length; k++) {
                                if (x1t == 0.0) {
                                    // Infinite slope thru (0,0)
                                    d1 += Math.abs(xPoints[k]);
                                } else if (y1t == 0.0) {
                                    // Zero slope thru (0, 0)
                                    d1 += Math.abs(yPoints[k]);
                                } else {
                                    slope = y1t / x1t;
                                    d1 += Math.abs( (slope * xPoints[k] - yPoints[k]) / Math.sqrt(slope * slope + 1));
                                }
                            }
                            d2 = 0.0;
                            for (k = 0; k < xPoints.length; k++) {
                                if (x2t == 0.0) {
                                    // Infinite slope thru (0,0)
                                    d2 += Math.abs(xPoints[k]);
                                } else if (y2t == 0.0) {
                                    // Zero slope thru (0, 0)
                                    d2 += Math.abs(yPoints[k]);
                                } else {
                                    slope = y2t / x2t;
                                    d2 += Math.abs( (slope * xPoints[k] - yPoints[k]) / Math.sqrt(slope * slope + 1));
                                }
                            }
                            if (d1 < d2) {
                                tangentX = (float) x1t;
                                tangentY = (float) y1t;
                                normalX = (float) x2t;
                                normalY = (float) y2t;
                            } else {
                                tangentX = (float) x2t;
                                tangentY = (float) y2t;
                                normalX = (float) x1t;
                                normalY = (float) y1t;
                            }
                        }
                        omega = Math.atan2(normalY, normalX);
                        // Change omega range from -PI to PI to 0 to 2*PI
                        if (omega < 0.0) {
                            omega = omega + 2.0 * Math.PI;
                        }
                        xt = (int) Math.round(xpc + 2.0 * Math.cos(omega));
                        yt = (int) Math.round(ypc + 2.0 * Math.sin(omega));
                        indext = xt + xDim * yt;
                        if (VOIMask[indext] != numClosedCurves) {
                            // Not in VOI - take normal going the opposite way
                            if (omega < Math.PI) {
                                omega = omega + Math.PI;
                            } else {
                                omega = omega - Math.PI;
                            }
                        }
                        if (n >= 1) {
                            curveXDist = xpc - lastxpc;
                            curveYDist = ypc - lastypc;
                            distance = Math.sqrt(curveXDist * curveXDist + curveYDist * curveYDist);
                            deltaOmega = omega - lastOmega;
                            curvature = (float) (deltaOmega / distance);
                            omegaArray[indexPtr] = (float) omega;
                            curvatureArray[indexPtr] = curvature;
                        }

                        if (n == closedLength - 1) {
                            curveXDist = xpc0 - xpc;
                            curveYDist = ypc0 - ypc;
                            distance = Math.sqrt(curveXDist * curveXDist + curveYDist * curveYDist);
                            deltaOmega = omega0 - omega;
                            curvature = (float) (deltaOmega / distance);
                            omegaArray[startPtr] = (float) omega0;
                            curvatureArray[startPtr] = curvature;
                        }
                        lastxpc = xpc;
                        lastypc = ypc;
                        lastOmega = omega;
                        if (n == 0) {
                            xpc0 = xpc;
                            ypc0 = ypc;
                            omega0 = omega;
                        }
                        indexPtr++;
                    } // for (n = 0; n <= closedLength - 1; n++)
                }
            } // for (x = 0; x < xDim; x++)
        } // for (y = 0; y < yDim; y++)

        neighbor1 = null;
        neighbor2 = null;
        foundArray = null;
        xPoints = null;
        yPoints = null;

        ViewUserInterface.getReference().setDataText("Number of closed curves = " + numClosedCurves + "\n");
        // k = model/object scale factor, whereas the dialog requests the object/model scale factor
        // so the appropriate conversion is made in the constructor
        // Rotation and scaling:
        // (creationX - creationCenterX) = k*cos(theta)*(x - xCenter) - k*sin(theta)*(y - yCenter)
        // (creationY - creationCenterY) = k*sin(theta)*(x - xCenter) + k*cos(theta)*(y - yCenter)
        // If sin(theta) != 0:
        // tan(theta)*(creationY - creationCenterY) = k*sin(theta)*tan(theta)*(x - xCenter) + k*sin(theta)*(y - yCenter)
        // (creationX - creationCenterX) + tan(theta)*(creationY - creationCenterY) =
        // k*cos(theta)*(x - xCenter) + k*sin(theta)*tan(theta)*(x - xCenter)
        // (xCenter - x) = [(creationCenterX - creationX) + tan(theta)*(creationCenterY - creationY)]/
        // [k*(cos(theta) + sin(theta)*tan(theta))]
        // xCenter = x + [(creationCenterX - creationX) + tan(theta)*(creationCenterY - creationY)]/
        // [k*(cos(theta) + sin(theta)*tan(theta))]
        // k*sin(theta)*(y - ycenter) = k*cos(theta)*(x - xCenter) - (creationX - creationCenterX)
        // (yCenter - y) = [k*cos(theta)*(xCenter - x) + (creationX - creationCenterX)]/[k*sin(theta)]
        // yCenter = y + [k*cos(theta)*(xCenter - x) + (creationX - creationCenterX)]/[k*(sin(theta)]
        // If sin(theta) == 0:
        // k*cos(theta)*(xCenter - x) = (creationCenterX - creationX)
        // xCenter = x + (creationCenterX - creationX)/[k*cos(theta)]
        // k*cos(theta)*(yCenter - y) = (creationCenterY - creationY)
        // yCenter = y + (creationCenterY - creationY)/[k*cos(theta)]
        // Rotation:
        // (creationX - creationCenterX) = cos(theta)*(x - xCenter) - sin(theta)*(y - yCenter)
        // (creationY - creationCenterY) = sin(theta)*(x - xCenter) + cos(theta)*(y - yCenter)
        // If sin(theta) != 0:
        // tan(theta)*(creationY - creationCenterY) = sin(theta)*tan(theta)*(x - xCenter) + sin(theta)*(y - yCenter)
        // (creationX - creationCenterX) + tan(theta)*(creationY - creationCenterY) =
        // cos(theta)*(x - xCenter) + sin(theta)*tan(theta)*(x - xCenter)
        // (xCenter - x) = [(creationCenterX - creationX) + tan(theta)*(creationCenterY - creationY)]/
        // [(cos(theta) + sin(theta)*tan(theta))]
        // xCenter = x + [(creationCenterX - creationX) + tan(theta)*(creationCenterY - creationY)]/
        // [(cos(theta) + sin(theta)*tan(theta))]
        // sin(theta)*(y - ycenter) = cos(theta)*(x - xCenter) - (creationX - creationCenterX)
        // (yCenter - y) = [cos(theta)*(xCenter - x) + (creationX - creationCenterX)]/[sin(theta)]
        // yCenter = y + [cos(theta)*(xCenter - x) + (creationX - creationCenterX)]/[(sin(theta)]
        // If sin(theta) == 0:
        // cos(theta)*(xCenter - x) = (creationCenterX - creationX)
        // xCenter = x + (creationCenterX - creationX)/[cos(theta)]
        // cos(theta)*(yCenter - y) = (creationCenterY - creationY)
        // yCenter = y + (creationCenterY - creationY)/[cos(theta)]
        // Scaling:
        // (creationX - creationCenterX) = k*(x - xCenter)
        // (creationY - creationCenterY) = k*(y - yCenter)
        // xCenter = x + (creationCenterX - creationX) /k
        // yCenter = y + (creationCenterY - creationY)/k
        // No rotation or scaling:
        // (creationX - creationCenterX) = (x - xCenter)
        // (creationY - creationCenterY) = (y - yCenter)
        // xCenter = x + (creationCenterX - creationX)
        // yCenter = y + (creationCenterY - creationY)
        xBins = (int) Math.ceil( (xDim - 1.0) / maxPixelBinWidth);
        xPixelsPerBin = (xDim - 1.0f) / (xBins - 1);
        yBins = (int) Math.ceil( (yDim - 1.0) / maxPixelBinWidth);
        yPixelsPerBin = (yDim - 1.0f) / (yBins - 1);
        thetaBins = 1;
        maxRadiansDiff = maxDegreesBinWidth * Math.PI / 180.0;
        if (allowRotation) {
            thetaBins = (int) Math.ceil( (2.0 * Math.PI) / maxRadiansDiff);
        }
        if ( !allowScaling) {
            scaleBins = 1;
        }
        if (allowRotation) {
            thetaArray = new float[thetaBins];
            radiansPerBin = (2.0 * Math.PI) / thetaBins;
            for (i = 0; i < thetaBins; i++) {
                thetaArray[i] = (float) (i * 2.0 * Math.PI) / thetaBins;
            }
        }
        if (allowScaling) {
            maxToMinRatio = maxScaleFactor / minScaleFactor;
            scaleArray = new float[scaleBins];
            scaleArray[0] = minScaleFactor;
            scaleArray[scaleBins - 1] = maxScaleFactor;
            for (i = 1; i < scaleBins - 1; i++) {
                scaleArray[i] = (float) (minScaleFactor * Math
                        .pow(maxToMinRatio, (double) i / (double) (scaleBins - 1)));
            }
            betweenBinsScale = Math.pow(maxToMinRatio, 1.0 / (double) (scaleBins - 1));
            logBetweenScale = Math.log(betweenBinsScale);
            scaleIndex = (int) (Math.log(1.0 / minScaleFactor) / logBetweenScale);
        }
        ViewUserInterface.getReference().setDataText("xBins = " + xBins + "\n");
        ViewUserInterface.getReference().setDataText("yBins = " + yBins + "\n");
        ViewUserInterface.getReference().setDataText("thetaBins = " + thetaBins + "\n");
        ViewUserInterface.getReference().setDataText("scaleBins = " + scaleBins + "\n");
        rotationArray = new float[thetaBins][scaleBins];
        scalingArray = new float[thetaBins][scaleBins];
        rsCountArray = new int[thetaBins][scaleBins];
        xCenterArray = new float[xBins][yBins];
        yCenterArray = new float[xBins][yBins];
        xyCountArray = new int[xBins][yBins];
        foundPoint = new boolean[numPoints];
        xCenterTable = new float[objectsToFind];
        yCenterTable = new float[objectsToFind];
        if (allowRotation) {
            thetaTable = new float[objectsToFind];
        }
        if (allowScaling) {
            scaleTable = new float[objectsToFind];
        }

        countTable = new short[objectsToFind];
        while (objectsFound < objectsToFind) {
            for (i = 0; i < creationNumPoints; i++) {
                creationX = creationIndexArray[i] % creationXDim;
                creationY = creationIndexArray[i] / creationXDim;
                creationOmega = creationOmegaArray[i];
                creationCurvature = creationCurvatureArray[i];
                for (j = 0; j < numPoints; j++) {
                    x = indexArray[j] % xDim;
                    y = indexArray[j] / xDim;
                    omega = omegaArray[j];
                    curvature = curvatureArray[j];
                    if (allowRotation) {
                        theta = creationOmega - omega;
                        while (theta < 0.0) {
                            theta = theta + 2.0 * Math.PI;
                        }
                        while (theta >= 2.0 * Math.PI) {
                            theta = theta - 2.0 * Math.PI;
                        }
                        costheta = Math.cos(theta);
                        sintheta = Math.sin(theta);
                        tantheta = sintheta / costheta;
                        thetaIndex = (int) (theta / radiansPerBin);
                    } // if (allowRotation)
                    if (allowScaling) {
                        if ( (curvature == 0.0f) || (creationCurvature == 0.0f)) {
                            continue;
                        } else {
                            scaleFactor = curvature / creationCurvature;
                        }
                        if (openArray[j]) {
                            scaleFactor = Math.abs(scaleFactor);
                        }
                        // This will also remove cases where curvature and
                        // creationCurvature are of opposite sign for closed curves
                        // Since all closed curves are transited counterclockwise,
                        // the sign of curvature should be considered
                        if (scaleFactor < minScaleFactor) {
                            continue;
                        }
                        if (scaleFactor > maxScaleFactor) {
                            continue;
                        }
                        scaleIndex = (int) (Math.log(scaleFactor / minScaleFactor) / logBetweenScale);
                    } // if (allowScaling)
                    if (allowRotation && allowScaling) {
                        if (sintheta != 0.0) {
                            xCenter = x + ( (creationCenterX - creationX) + tantheta * (creationCenterY - creationY))
                                    / (scaleFactor * (costheta + sintheta * tantheta));
                            yCenter = y + (scaleFactor * costheta * (xCenter - x) + (creationX - creationCenterX))
                                    / (scaleFactor * sintheta);
                        } else {
                            xCenter = x + (creationCenterX - creationX) / (scaleFactor * costheta);
                            yCenter = y + (creationCenterY - creationY) / (scaleFactor * costheta);
                        }
                    } else if (allowRotation) {
                        if (sintheta != 0.0) {
                            xCenter = x + ( (creationCenterX - creationX) + tantheta * (creationCenterY - creationY))
                                    / (costheta + sintheta * tantheta);
                            yCenter = y + (costheta * (xCenter - x) + (creationX - creationCenterX)) / sintheta;
                        } else {
                            xCenter = x + (creationCenterX - creationX) / costheta;
                            yCenter = y + (creationCenterY - creationY) / costheta;
                        }
                    } else if (allowScaling) {
                        xCenter = x + (creationCenterX - creationX) / scaleFactor;
                        yCenter = y + (creationCenterY - creationY) / scaleFactor;
                    } else {
                        xCenter = x + (creationCenterX - creationX);
                        yCenter = y + (creationCenterY - creationY);
                    }
                    if ( (xCenter < 0.0f) || (xCenter > (xDim - 1)) || (yCenter < 0.0f) || (yCenter > (yDim - 1))) {
                        continue;
                    }

                    xIndex = (int) (xCenter / xPixelsPerBin);
                    yIndex = (int) (yCenter / yPixelsPerBin);
                    if (allowRotation) {
                        rotationArray[thetaIndex][scaleIndex] += theta;
                    }
                    if (allowScaling) {
                        scalingArray[thetaIndex][scaleIndex] += scaleFactor;
                    }
                    if (allowRotation || allowScaling) {
                        rsCountArray[thetaIndex][scaleIndex]++;
                    }
                    xCenterArray[xIndex][yIndex] += xCenter;
                    yCenterArray[xIndex][yIndex] += yCenter;
                    xyCountArray[xIndex][yIndex]++;
                    if (allowRotation && openArray[j]) {
                        if (theta < Math.PI) {
                            theta = theta + Math.PI;
                        } else {
                            theta = theta - Math.PI;
                        }
                        costheta = Math.cos(theta);
                        sintheta = Math.sin(theta);
                        tantheta = sintheta / costheta;
                        thetaIndex = (int) (theta / radiansPerBin);
                        if (allowScaling) {
                            if (sintheta != 0.0) {
                                xCenter = x
                                        + ( (creationCenterX - creationX) + tantheta * (creationCenterY - creationY))
                                        / (scaleFactor * (costheta + sintheta * tantheta));
                                yCenter = y + (scaleFactor * costheta * (xCenter - x) + (creationX - creationCenterX))
                                        / (scaleFactor * sintheta);
                            } else {
                                xCenter = x + (creationCenterX - creationX) / (scaleFactor * costheta);
                                yCenter = y + (creationCenterY - creationY) / (scaleFactor * costheta);
                            }
                        } else {
                            if (sintheta != 0.0) {
                                xCenter = x
                                        + ( (creationCenterX - creationX) + tantheta * (creationCenterY - creationY))
                                        / (costheta + sintheta * tantheta);
                                yCenter = y + (costheta * (xCenter - x) + (creationX - creationCenterX)) / sintheta;
                            } else {
                                xCenter = x + (creationCenterX - creationX) / costheta;
                                yCenter = y + (creationCenterY - creationY) / costheta;
                            }
                        }
                        if ( (xCenter < 0.0f) || (xCenter > (xDim - 1)) || (yCenter < 0.0f) || (yCenter > (yDim - 1))) {
                            continue;
                        }

                        xIndex = (int) (xCenter / xPixelsPerBin);
                        yIndex = (int) (yCenter / yPixelsPerBin);
                        rotationArray[thetaIndex][scaleIndex] += theta;
                        if (allowScaling) {
                            scalingArray[thetaIndex][scaleIndex] += scaleFactor;
                        }
                        rsCountArray[thetaIndex][scaleIndex]++;
                        xCenterArray[xIndex][yIndex] += xCenter;
                        yCenterArray[xIndex][yIndex] += yCenter;
                        xyCountArray[xIndex][yIndex]++;
                    } // if (allowRotation && openArray[j])

                } // for (j = 0; j < numPoints; j++)
            } // for (i = 0; i < creationNumPoints; i++)

            maxXIndex = -1;
            maxYIndex = -1;
            maxXYCount = 0;
            for (i = 0; i < xBins; i++) {
                for (j = 0; j < yBins; j++) {
                    if (xyCountArray[i][j] > maxXYCount) {
                        maxXIndex = i;
                        maxYIndex = j;
                        maxXYCount = xyCountArray[i][j];
                    }
                }
            }
            ViewUserInterface.getReference().setDataText(" maxXYCount = " + maxXYCount + "\n");
            if (maxXYCount < 2) {
                break;
            }

            if (allowRotation || allowScaling) {
                maxThetaIndex = -1;
                maxScaleIndex = -1;
                maxRSCount = 0;
                for (i = 0; i < thetaBins; i++) {
                    for (j = 0; j < scaleBins; j++) {
                        if (rsCountArray[i][j] > maxRSCount) {
                            maxThetaIndex = i;
                            maxScaleIndex = j;
                            maxRSCount = rsCountArray[i][j];
                        }
                    } // for (m = 0; m < scaleBins; m++)
                } // for (k = 0; k < thetaBins; k++)

                if (maxRSCount < 2) {
                    break;
                }
            } // if (allowRotation || allowScaling)

            xCenter = xCenterArray[maxXIndex][maxYIndex] / maxXYCount;
            yCenter = yCenterArray[maxXIndex][maxYIndex] / maxXYCount;
            if (allowRotation) {
                theta = rotationArray[maxThetaIndex][maxScaleIndex] / maxRSCount;
            }
            if (allowScaling) {
                scaleFactor = scalingArray[maxThetaIndex][maxScaleIndex] / maxRSCount;
            }

            if ( (xCenter >= 0.0f) && (xCenter <= xDim - 1) && (yCenter >= 0.0f) && (yCenter <= yDim - 1)) {
                ViewUserInterface.getReference().setDataText("xCenter = " + xCenter + " yCenter = " + yCenter + "\n");
                xCenterTable[objectsFound] = (float) xCenter;
                yCenterTable[objectsFound] = (float) yCenter;
                if (allowRotation) {
                    thetaTable[objectsFound] = (float) theta;
                }
                if (allowScaling) {
                    // Convert model/object to object/model
                    scaleTable[objectsFound] = 1.0f / scaleFactor;
                }
                countTable[objectsFound] = (short) maxXYCount;
                objectsFound++;
                ViewUserInterface.getReference().setDataText("Object # " + objectsFound + " found\n");
                ViewUserInterface.getReference().setDataText(" x center = " + xCenter + "\n");
                ViewUserInterface.getReference().setDataText(" y center = " + yCenter + "\n");
                if (allowRotation) {
                    ViewUserInterface.getReference().setDataText(
                            " theta = " + (thetaTable[objectsFound - 1] * 180 / Math.PI) + "\n");
                }
                if (allowScaling) {
                    ViewUserInterface.getReference().setDataText(
                            " scale factor = " + scaleTable[objectsFound - 1] + "\n");
                }
                centerPtVOI = new VOI((short) (objectsFound), "center" + objectsFound + ".voi", 1, VOI.POINT, -1.0f);
                centerPtVOI.setColor(Color.white);
                xArr[0] = (float) xCenter;
                yArr[0] = (float) yCenter;
                zArr[0] = 0.0f;
                centerPtVOI.importCurve(xArr, yArr, zArr, 0);
                ((VOIPoint) (centerPtVOI.getCurves()[0].elementAt(0))).setFixed(true);
                ((VOIPoint) (centerPtVOI.getCurves()[0].elementAt(0))).setLabel(String.valueOf(objectsFound));
                srcImage.registerVOI(centerPtVOI);

                if (objectsFound == objectsToFind) {
                    break;
                }
            } // if ((xCenter >= 0.0f) && (xCenter <= xDim - 1) && (yCenter >= 0.0f) && (yCenter <= yDim - 1))
            // If an object was found, then zero the Hough transform array before acquiring a
            // new set of xCenter, yCenter points.
            for (i = 0; i < xBins; i++) {
                for (j = 0; j < yBins; j++) {
                    xCenterArray[i][j] = 0.0f;
                    yCenterArray[i][j] = 0.0f;
                    xyCountArray[i][j] = 0;
                }
            }
            if (allowRotation) {
                for (i = 0; i < thetaBins; i++) {
                    for (j = 0; j < scaleBins; j++) {
                        rotationArray[i][j] = 0.0f;
                    }
                }
            }
            if (allowScaling) {
                for (i = 0; i < thetaBins; i++) {
                    for (j = 0; j < scaleBins; j++) {
                        scalingArray[i][j] = 0.0f;
                    }
                }
            }
            if (allowRotation || allowScaling) {
                for (i = 0; i < thetaBins; i++) {
                    for (j = 0; j < scaleBins; j++) {
                        rsCountArray[i][j] = 0;
                    }
                }
            }

            // If an object was found, then delete the points from the
            // from indexArray, omegaArray, and curvatureArray before running the
            // Hough transform again
            pointsDeleted = 0;
            for (i = 0; i < creationNumPoints; i++) {
                creationX = creationIndexArray[i] % creationXDim;
                creationY = creationIndexArray[i] / creationXDim;
                creationOmega = creationOmegaArray[i];
                creationCurvature = creationCurvatureArray[i];
                for (j = 0; j < numPoints; j++) {
                    x = indexArray[j] % xDim;
                    y = indexArray[j] / xDim;
                    omega = omegaArray[j];
                    curvature = curvatureArray[j];
                    if (allowRotation) {
                        theta = creationOmega - omega;
                        while (theta < 0.0) {
                            theta = theta + 2.0 * Math.PI;
                        }
                        while (theta >= 2.0 * Math.PI) {
                            theta = theta - 2.0 * Math.PI;
                        }
                        costheta = Math.cos(theta);
                        sintheta = Math.sin(theta);
                        tantheta = sintheta / costheta;
                        thetaIndex = (int) (theta / radiansPerBin);
                    } // if (allowRotation)
                    if (allowScaling) {
                        if ( (curvature == 0.0f) && (creationCurvature == 0.0f)) {
                            scaleFactor = 1.0f;
                        } else if (creationCurvature == 0.0f) {
                            continue;
                        }
                        scaleFactor = curvature / creationCurvature;
                        if (openArray[j]) {
                            scaleFactor = Math.abs(scaleFactor);
                        }
                        // This will also remove cases where curvature and
                        // creationCurvature are of opposite sign for closed curves
                        // Since all closed curves are transited counterclockwise,
                        // the sign of curvature should be considered
                        if (scaleFactor < minScaleFactor) {
                            continue;
                        }
                        if (scaleFactor > maxScaleFactor) {
                            continue;
                        }
                        scaleIndex = (int) (Math.log(scaleFactor / minScaleFactor) / logBetweenScale);
                    } // if (allowScaling)
                    if (allowRotation && allowScaling) {
                        if (sintheta != 0.0) {
                            xCenter = x + ( (creationCenterX - creationX) + tantheta * (creationCenterY - creationY))
                                    / (scaleFactor * (costheta + sintheta * tantheta));
                            yCenter = y + (scaleFactor * costheta * (xCenter - x) + (creationX - creationCenterX))
                                    / (scaleFactor * sintheta);
                        } else {
                            xCenter = x + (creationCenterX - creationX) / (scaleFactor * costheta);
                            yCenter = y + (creationCenterY - creationY) / (scaleFactor * costheta);
                        }
                    } else if (allowRotation) {
                        if (sintheta != 0.0) {
                            xCenter = x + ( (creationCenterX - creationX) + tantheta * (creationCenterY - creationY))
                                    / (costheta + sintheta * tantheta);
                            yCenter = y + (costheta * (xCenter - x) + (creationX - creationCenterX)) / sintheta;
                        } else {
                            xCenter = x + (creationCenterX - creationX) / costheta;
                            yCenter = y + (creationCenterY - creationY) / costheta;
                        }
                    } else if (allowScaling) {
                        xCenter = x + (creationCenterX - creationX) / scaleFactor;
                        yCenter = y + (creationCenterY - creationY) / scaleFactor;
                    } else {
                        xCenter = x + (creationCenterX - creationX);
                        yCenter = y + (creationCenterY - creationY);
                    }

                    if ( (xCenter < 0.0f) || (xCenter > (xDim - 1)) || (yCenter < 0.0f) || (yCenter > (yDim - 1))) {
                        continue;
                    }

                    xIndex = (int) (xCenter / xPixelsPerBin);
                    if ( (xIndex >= Math.max(0, maxXIndex - 1)) && (xIndex <= Math.min(xBins - 1, maxXIndex + 1))
                            && ( !foundPoint[j])) {
                        yIndex = (int) (yCenter / yPixelsPerBin);
                        if ( (yIndex >= Math.max(0, maxYIndex - 1)) && (yIndex <= Math.min(yBins - 1, maxYIndex + 1))) {
                            foundPoint[j] = true;
                            pointsDeleted++;
                        } // if ((yIndex >= Math.max(0, maxYIndex - 1)) && (yIndex <= Math.min(yBins-1, maxYIndex +
                        // 1)))
                    } // if ((xIndex >= Math.max(0, maxXIndex - 1)) && (xIndex <= Math.min(xBins-1, maxXIndex + 1)) &&
                    // (!foundPoint[j]))

                    if (allowRotation && openArray[j]) {
                        if (theta < Math.PI) {
                            theta = theta + Math.PI;
                        } else {
                            theta = theta - Math.PI;
                        }
                        costheta = Math.cos(theta);
                        sintheta = Math.sin(theta);
                        tantheta = sintheta / costheta;
                        thetaIndex = (int) (theta / radiansPerBin);
                        if (allowScaling) {
                            if (sintheta != 0.0) {
                                xCenter = x
                                        + ( (creationCenterX - creationX) + tantheta * (creationCenterY - creationY))
                                        / (scaleFactor * (costheta + sintheta * tantheta));
                                yCenter = y + (scaleFactor * costheta * (xCenter - x) + (creationX - creationCenterX))
                                        / (scaleFactor * sintheta);
                            } else {
                                xCenter = x + (creationCenterX - creationX) / (scaleFactor * costheta);
                                yCenter = y + (creationCenterY - creationY) / (scaleFactor * costheta);
                            }
                        } else {
                            if (sintheta != 0.0) {
                                xCenter = x
                                        + ( (creationCenterX - creationX) + tantheta * (creationCenterY - creationY))
                                        / (costheta + sintheta * tantheta);
                                yCenter = y + (costheta * (xCenter - x) + (creationX - creationCenterX)) / sintheta;
                            } else {
                                xCenter = x + (creationCenterX - creationX) / costheta;
                                yCenter = y + (creationCenterY - creationY) / costheta;
                            }
                        }

                        if ( (xCenter < 0.0f) || (xCenter > (xDim - 1)) || (yCenter < 0.0f) || (yCenter > (yDim - 1))) {
                            continue;
                        }

                        xIndex = (int) (xCenter / xPixelsPerBin);
                        if ( (xIndex >= Math.max(0, maxXIndex - 1)) && (xIndex <= Math.min(xBins - 1, maxXIndex + 1))
                                && ( !foundPoint[j])) {
                            yIndex = (int) (yCenter / yPixelsPerBin);
                            if ( (yIndex >= Math.max(0, maxYIndex - 1))
                                    && (yIndex <= Math.min(yBins - 1, maxYIndex + 1))) {
                                foundPoint[j] = true;
                                pointsDeleted++;
                            } // if ((yIndex >= Math.max(0, maxYIndex - 1)) && (yIndex <= Math.min(yBins-1, maxYIndex
                            // + 1)))
                        } // if ((xIndex >= Math.max(0, maxXIndex - 1)) && (xIndex <= Math.min(xBins-1, maxXIndex +
                        // 1)) && (!foundPoint[j]))
                    } // if (allowRotation && openArray[j])
                } // for (j = 0; j < numPoints; j++)
            } // for (i = 0; i < creationNumPoints; i++)

            if (pointsDeleted > 0) {
                newNumPoints = numPoints - pointsDeleted;
                if (newNumPoints == 0) {
                    break;
                }
                newIndexArray = new int[newNumPoints];
                newOmegaArray = new float[newNumPoints];
                newCurvatureArray = new float[newNumPoints];
                newOpenArray = new boolean[newNumPoints];
                for (i = 0, j = 0; i < numPoints; i++) {
                    if ( !foundPoint[i]) {
                        newIndexArray[j] = indexArray[i];
                        newOmegaArray[j] = omegaArray[i];
                        newCurvatureArray[j] = curvatureArray[i];
                        newOpenArray[j] = openArray[i];
                        j++;
                    }
                } // for (i = 0, j = 0; i < numPoints; i++)
                numPoints = newNumPoints;
                foundPoint = null;
                foundPoint = new boolean[numPoints];
                indexArray = null;
                indexArray = new int[numPoints];
                omegaArray = null;
                omegaArray = new float[numPoints];
                curvatureArray = null;
                curvatureArray = new float[numPoints];
                openArray = null;
                openArray = new boolean[numPoints];
                for (i = 0; i < numPoints; i++) {
                    indexArray[i] = newIndexArray[i];
                    omegaArray[i] = newOmegaArray[i];
                    curvatureArray[i] = newCurvatureArray[i];
                    openArray[i] = newOpenArray[i];
                }
                newIndexArray = null;
                newOmegaArray = null;
                newCurvatureArray = null;
                newOpenArray = null;
            } // if (pointsDeleted > 0)
        } // while (objectsFound < objectsToFind)

        srcImage.notifyImageDisplayListeners();

        // Restore original source values
        if ( !test) {
            try {
                srcImage.exportData(0, sourceSlice, srcBuffer);
            } catch (IOException e) {
                MipavUtil.displayError("IOException " + e + " on srcImage.exportData");

                setCompleted(false);

                return;
            }
        } // if (!test)

        setCompleted(true);
        return;
    }

    public void runAlgorithm() {
        int x, y;
        int offset;

        int xDim;

        int yDim;

        int sourceSlice;

        int numPoints;

        int i, j, k, m, n;
        int index;
        byte[] srcBuffer;
        boolean test = false;
        double xCenter;
        double yCenter;
        float xCenterArray[][][];
        float yCenterArray[][][];
        int countArray[][][];
        int indexArray[];
        int newIndexArray[];
        int neighbors;
        int endPoints;
        int numOpenCurves;
        int numClosedCurves;
        boolean foundArray[];
        int openStart[];
        int openLength[];
        int closedLength;
        int indexPtr = 0;
        int nextPoint;
        float xArray[];
        float yArray[];
        float tangentX;
        float tangentY;
        float normalX;
        float normalY;
        boolean openArray[];
        boolean newOpenArray[];
        short omegaArray[];
        short newOmegaArray[];
        int startPtr;
        int presentSidePoints;
        float xPoints[];
        float yPoints[];
        int endPtr;
        int startWrapPoints;
        int endWrapPoints;
        double r;
        double beta;
        int neighbor1[];
        int neighbor2[];
        AlgorithmMorphology2D algoMorph2D;
        int neigh0;
        int neigh1;
        int neigh2;
        int neigh3;
        int neigh4;
        int neigh5;
        int neigh6;
        int neigh7;
        int pruningPix;
        boolean entireImage;
        double var;
        float minXCenter;
        float maxXCenter;
        float minYCenter;
        float maxYCenter;
        int bytesPerCell;
        int xCenterBins;
        int yCenterBins;
        int thetaBins;
        double maxRadiansDiff;
        long desiredBytes;
        long actualBytesAvailable;
        double shrinkFactor = 1.0;
        long longNumBins;
        int numBins;
        int xCenterIndex;
        int yCenterIndex;
        int maxCount = 0;
        boolean foundPoint[];
        float xCenterTable[];
        float yCenterTable[];
        float thetaTable[] = null;
        float scaleTable[] = null;
        short countTable[];
        int pointsDeleted;
        int newNumPoints;
        float xpc;
        float ypc;
        double xSqSum;
        double ySqSum;
        double xySum;
        double x1t;
        double x2t;
        double y1t;
        double y2t;
        double d1;
        double d2;
        double slope;
        int objectsFound = 0;
        int omegaIndex;
        int rBetaLength;
        float floatArray[];
        float thetaArray[] = null;
        double maxToMinRatio;
        float scaleArray[] = null;
        int xyBins;
        int maxXYIndex = 0;
        int maxXIndex = 0;
        int maxYIndex = 0;
        int maxThetaIndex = 0;
        int maxScaleIndex = 0;
        VOI centerPtVOI;
        float xArr[] = new float[1];
        float yArr[] = new float[1];
        float zArr[] = new float[1];
        ModelImage maskImage;
        double omegaBinWidth = (2.0 * Math.PI) / omegaBins;
        double omega;
        VOI closedVOI;
        int closedX[];
        int closedY[];
        int xVal[];
        int yVal[];
        int zVal[];
        short VOIMask[];
        int xt;
        int yt;
        int indext;

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        fireProgressStateChanged(srcImage.getImageName(), "Find R-table object ...");

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sourceSlice = xDim * yDim;

        srcBuffer = new byte[sourceSlice];

        try {
            srcImage.exportData(0, sourceSlice, srcBuffer);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on srcImage.exportData");

            setCompleted(false);

            return;
        }

        maskImage = (ModelImage) srcImage.clone();

        // Skeletonize the binary image
        // Prune off branches with 2 or less pixels
        pruningPix = 2;
        entireImage = true;
        algoMorph2D = new AlgorithmMorphology2D(maskImage, 0, 0.0f, AlgorithmMorphology2D.SKELETONIZE, 0, 0,
                pruningPix, 0, entireImage);
        algoMorph2D.run();
        algoMorph2D.finalize();

        try {
            maskImage.exportData(0, sourceSlice, srcBuffer);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on destImage.exportData");

            setCompleted(false);

            return;
        }

        // When a diagonal neighbor is adjacent to a horizontal or vertical neighbor,
        // remove the horizontal or vertical neighbor
        for (y = 0; y < yDim; y++) {
            offset = y * xDim;
            for (x = 0; x < xDim; x++) {
                index = offset + x;
                if (srcBuffer[index] != 0) {
                    neighbors = 0;
                    neigh0 = -1;
                    neigh1 = -1;
                    neigh2 = -1;
                    neigh3 = -1;
                    neigh4 = -1;
                    neigh5 = -1;
                    neigh6 = -1;
                    neigh7 = -1;
                    if (y > 0) {
                        if (x > 0) {
                            if (srcBuffer[index - xDim - 1] != 0) {
                                neighbors++;
                                neigh0 = index - xDim - 1;
                            }
                        }
                        if (srcBuffer[index - xDim] != 0) {
                            neighbors++;
                            neigh1 = index - xDim;
                        }
                        if (x < xDim - 1) {
                            if (srcBuffer[index - xDim + 1] != 0) {
                                neighbors++;
                                neigh2 = index - xDim + 1;
                            }
                        }
                    } // if (y > 0)
                    if (x > 0) {
                        if (srcBuffer[index - 1] != 0) {
                            neighbors++;
                            neigh3 = index - 1;
                        }
                    } // if (x > 0)
                    if (x < xDim - 1) {
                        if (srcBuffer[index + 1] != 0) {
                            neighbors++;
                            neigh4 = index + 1;
                        }
                    } // if (x < xDim - 1)
                    if (y < yDim - 1) {
                        if (x > 0) {
                            if (srcBuffer[index + xDim - 1] != 0) {
                                neighbors++;
                                neigh5 = index + xDim - 1;
                            }
                        }
                        if (srcBuffer[index + xDim] != 0) {
                            neighbors++;
                            neigh6 = index + xDim;
                        }
                        if (x < xDim - 1) {
                            if (srcBuffer[index + xDim + 1] != 0) {
                                neighbors++;
                                neigh7 = index + xDim + 1;
                            }
                        }
                    } // if (y < yDim - 1)
                    if (neighbors > 2) {
                        // Could be 3 or 4
                        if ( (neigh0 >= 0) && (neigh1 >= 0)) {
                            srcBuffer[neigh1] = 0;
                            neigh1 = -1;
                        }
                        if ( (neigh1 >= 0) && (neigh2 >= 0)) {
                            srcBuffer[neigh1] = 0;
                            neigh1 = -1;
                        }
                        if ( (neigh0 >= 0) && (neigh3 >= 0)) {
                            srcBuffer[neigh3] = 0;
                            neigh3 = -1;
                        }
                        if ( (neigh3 >= 0) && (neigh5 >= 0)) {
                            srcBuffer[neigh3] = 0;
                            neigh3 = -1;
                        }
                        if ( (neigh2 >= 0) && (neigh4 >= 0)) {
                            srcBuffer[neigh4] = 0;
                            neigh4 = -1;
                        }
                        if ( (neigh4 >= 0) && (neigh7 >= 0)) {
                            srcBuffer[neigh4] = 0;
                            neigh4 = -1;
                        }
                        if ( (neigh5 >= 0) && (neigh6 >= 0)) {
                            srcBuffer[neigh6] = 0;
                            neigh6 = -1;
                        }
                        if ( (neigh6 >= 0) && (neigh7 >= 0)) {
                            srcBuffer[neigh6] = 0;
                            neigh6 = -1;
                        }
                    }
                } // if (srcBuffer[index] != 0)
            } // for (x = 0; x < xDim; x++)
        } // for (y = 0; y < yDim; y++)

        // Remove points with more than 2 neighbors
        for (y = 0; y < yDim; y++) {
            offset = y * xDim;
            for (x = 0; x < xDim; x++) {
                index = offset + x;
                if (srcBuffer[index] != 0) {
                    neighbors = 0;
                    if (y > 0) {
                        if (x > 0) {
                            if (srcBuffer[index - xDim - 1] != 0) {
                                neighbors++;
                            }
                        }
                        if (srcBuffer[index - xDim] != 0) {
                            neighbors++;
                        }
                        if (x < xDim - 1) {
                            if (srcBuffer[index - xDim + 1] != 0) {
                                neighbors++;
                            }
                        }
                    } // if (y > 0)
                    if (x > 0) {
                        if (srcBuffer[index - 1] != 0) {
                            neighbors++;
                        }
                    } // if (x > 0)
                    if (x < xDim - 1) {
                        if (srcBuffer[index + 1] != 0) {
                            neighbors++;
                        }
                    } // if (x < xDim - 1)
                    if (y < yDim - 1) {
                        if (x > 0) {
                            if (srcBuffer[index + xDim - 1] != 0) {
                                neighbors++;
                            }
                        }
                        if (srcBuffer[index + xDim] != 0) {
                            neighbors++;
                        }
                        if (x < xDim - 1) {
                            if (srcBuffer[index + xDim + 1] != 0) {
                                neighbors++;
                            }
                        }
                    } // if (y < yDim - 1)
                    if (neighbors > 2) {
                        srcBuffer[index] = 0;
                    }
                } // if (srcBuffer[index] != 0)
            } // for (x = 0; x < xDim; x++)
        } // for (y = 0; y < yDim; y++)

        // Find the 1 or 2 neighbors of every point
        // Find the number of end points, that is, points with only 1 neighbor
        // Delete isolated points with no neighbors
        numPoints = 0;
        endPoints = 0;
        neighbor1 = new int[sourceSlice];
        neighbor2 = new int[sourceSlice];
        for (i = 0; i < sourceSlice; i++) {
            neighbor1[i] = -1;
            neighbor2[i] = -1;
        }
        for (y = 0; y < yDim; y++) {
            offset = y * xDim;
            for (x = 0; x < xDim; x++) {
                index = offset + x;
                if (srcBuffer[index] != 0) {
                    neighbors = 0;
                    if (y > 0) {
                        if (x > 0) {
                            if (srcBuffer[index - xDim - 1] != 0) {
                                neighbors++;
                                neighbor1[index] = index - xDim - 1;
                            }
                        }
                        if (srcBuffer[index - xDim] != 0) {
                            neighbors++;
                            if (neighbor1[index] == -1) {
                                neighbor1[index] = index - xDim;
                            } else {
                                neighbor2[index] = index - xDim;
                            }
                        }
                        if (x < xDim - 1) {
                            if (srcBuffer[index - xDim + 1] != 0) {
                                neighbors++;
                                if (neighbor1[index] == -1) {
                                    neighbor1[index] = index - xDim + 1;
                                } else {
                                    neighbor2[index] = index - xDim + 1;
                                }
                            }
                        }
                    } // if (y > 0)
                    if (x > 0) {
                        if (srcBuffer[index - 1] != 0) {
                            neighbors++;
                            if (neighbor1[index] == -1) {
                                neighbor1[index] = index - 1;
                            } else {
                                neighbor2[index] = index - 1;
                            }
                        }
                    } // if (x > 0)
                    if (x < xDim - 1) {
                        if (srcBuffer[index + 1] != 0) {
                            neighbors++;
                            if (neighbor1[index] == -1) {
                                neighbor1[index] = index + 1;
                            } else {
                                neighbor2[index] = index + 1;
                            }
                        }
                    } // if (x < xDim - 1)
                    if (y < yDim - 1) {
                        if (x > 0) {
                            if (srcBuffer[index + xDim - 1] != 0) {
                                neighbors++;
                                if (neighbor1[index] == -1) {
                                    neighbor1[index] = index + xDim - 1;
                                } else {
                                    neighbor2[index] = index + xDim - 1;
                                }
                            }
                        }
                        if (srcBuffer[index + xDim] != 0) {
                            neighbors++;
                            if (neighbor1[index] == -1) {
                                neighbor1[index] = index + xDim;
                            } else {
                                neighbor2[index] = index + xDim;
                            }
                        }
                        if (x < xDim - 1) {
                            if (srcBuffer[index + xDim + 1] != 0) {
                                neighbors++;
                                if (neighbor1[index] == -1) {
                                    neighbor1[index] = index + xDim + 1;
                                } else {
                                    neighbor2[index] = index + xDim + 1;
                                }
                            }
                        }
                    } // if (y < yDim - 1)
                    if (neighbors == 0) {
                        srcBuffer[index] = 0;
                        neighbor1[index] = -1;
                        neighbor2[index] = -1;
                    } else {
                        numPoints++;
                        if (neighbors == 1) {
                            endPoints++;
                        }
                    }
                } // if (srcBuffer[index] != 0)
            } // for (x = 0; x < xDim; x++)
        } // for (y = 0; y < yDim; y++)

        numOpenCurves = endPoints / 2;
        openStart = new int[numOpenCurves];
        openLength = new int[numOpenCurves];
        foundArray = new boolean[sourceSlice];
        // Set foundArray to true at every zero location
        for (y = 0; y < yDim; y++) {
            offset = y * xDim;
            for (x = 0; x < xDim; x++) {
                index = offset + x;
                if (srcBuffer[index] == 0) {
                    foundArray[index] = true;
                }
            }
        }

        // Find the starting positions and lengths of the open curves
        i = 0;
        for (y = 0; y < yDim; y++) {
            offset = y * xDim;
            for (x = 0; x < xDim; x++) {
                index = offset + x;
                if ( (neighbor2[index] == -1) && ( !foundArray[index])) {
                    foundArray[index] = true;
                    openStart[i] = index;
                    openLength[i]++;
                    index = neighbor1[index];
                    foundArray[index] = true;
                    openLength[i]++;
                    while (neighbor2[index] != -1) {
                        if ( !foundArray[neighbor1[index]]) {
                            index = neighbor1[index];
                        } else {
                            index = neighbor2[index];
                        }
                        foundArray[index] = true;
                        openLength[i]++;
                    } // (while(neighbor2[index] != -1)
                    // Delete all open curves with only 2 points
                    // Also don't determine tangents of end points on longer curves,
                    // but use these 2 end points in determining tangents of more inner points
                    numPoints = numPoints - 2;
                    if (openLength[i] == 2) {
                        srcBuffer[openStart[i]] = 0;
                        srcBuffer[neighbor1[openStart[i]]] = 0;
                        numOpenCurves--;
                        openLength[i] = 0;
                    } else {
                        i++;
                    }
                }
            }
        }

        ViewUserInterface.getReference().setDataText("Number of open curves = " + numOpenCurves + "\n");

        // For the open curves find the tangent and normal to a point on a curve
        // With a user specified sidePointsForTangent on each side of a point find the tangent line that
        // minimizes the sum of the squared distances from these side points to the tangent line
        indexArray = new int[numPoints];
        openArray = new boolean[numPoints];
        omegaArray = new short[numPoints];
        for (i = 0; i < sourceSlice; i++) {
            if (srcBuffer[i] != 0) {
                foundArray[i] = false;
            }
        }
        indexPtr = 0;
        for (i = 0; i < numOpenCurves; i++) {
            startPtr = indexPtr;
            xArray = new float[openLength[i]];
            yArray = new float[openLength[i]];
            nextPoint = openStart[i];
            xArray[0] = nextPoint % xDim;
            yArray[0] = nextPoint / xDim;
            foundArray[nextPoint] = true;
            for (n = 1; n <= openLength[i] - 1; n++) {
                if ( !foundArray[neighbor1[nextPoint]]) {
                    nextPoint = neighbor1[nextPoint];
                } else {
                    nextPoint = neighbor2[nextPoint];
                }
                if (n <= openLength[i] - 2) {
                    indexArray[indexPtr++] = nextPoint;
                }
                xArray[n] = nextPoint % xDim;
                yArray[n] = nextPoint / xDim;
                foundArray[nextPoint] = true;
            } // for (n = 0; n <= openLength[i] - 1; n++)
            indexPtr = startPtr;
            for (n = 1; n <= openLength[i] - 2; n++) {
                presentSidePoints = Math.min(sidePointsForTangent, n);
                presentSidePoints = Math.min(presentSidePoints, openLength[i] - 1 - n);
                if (presentSidePoints == 1) {
                    tangentX = (xArray[n + 1] - xArray[n - 1]) / 2.0f;
                    tangentY = (yArray[n + 1] - yArray[n - 1]) / 2.0f;
                    if (tangentX == 0) {
                        normalX = 1;
                        normalY = 0;
                    } else if (tangentY == 0) {
                        normalX = 0;
                        normalY = 1;
                    } else {
                        normalX = -tangentY / tangentX;
                        normalY = 1;
                    }
                } // if (presentSidePoints == 1)
                else {
                    xPoints = new float[2 * presentSidePoints + 1];
                    yPoints = new float[2 * presentSidePoints + 1];
                    for (k = 0, j = n - presentSidePoints; j <= n + presentSidePoints; j++, k++) {
                        xPoints[k] = xArray[j];
                        yPoints[k] = yArray[j];
                    }
                    // Center all points for tangent point touching curve at (0, 0)
                    // That is, use an x axis and a y axis going thru the tangent point
                    xpc = xPoints[sidePointsForTangent];
                    ypc = yPoints[sidePointsForTangent];
                    for (k = 0; k < xPoints.length; k++) {
                        xPoints[k] = xPoints[k] - xpc;
                        yPoints[k] = yPoints[k] - ypc;
                    }
                    xSqSum = 0.0;
                    ySqSum = 0.0;
                    xySum = 0.0;
                    for (k = 0; k < xPoints.length; k++) {
                        xSqSum += xPoints[k] * xPoints[k];
                        ySqSum += yPoints[k] * yPoints[k];
                        xySum += xPoints[k] * yPoints[k];
                    }
                    if (xySum != 0.0) {
                        var = Math
                                .sqrt(ySqSum * ySqSum - 2.0 * xSqSum * ySqSum + xSqSum * xSqSum + 4.0 * xySum * xySum);
                        x1t = 0.5 * ( ( -ySqSum + xSqSum + var) / xySum);
                        x2t = 0.5 * ( ( -ySqSum + xSqSum - var) / xySum);
                        y1t = 1.0;
                        y2t = 1.0;
                    } else {
                        // If all points are symmetric to either this new x axis or this new y axis, then
                        // their product sum is 0 and the tangentX, tangentY must be 1,0 or 0,1
                        x1t = 1.0;
                        x2t = 0.0;
                        y1t = 0.0;
                        y2t = 1.0;
                    }
                    // x1t, y1t and x2t, y2t are perpindicular. To find the solution, calculate the sum of
                    // distances from the curve points to the line for the 2 cases
                    // The shortest distance is the correct solution
                    // Distance from AX + BY + C = 0 to P1 is
                    // abs((A*x1 + B*y1 + C))/sqrt(A**2 + B**2)
                    // Here A = slope, B = -1, and C = 0.
                    d1 = 0.0;
                    for (k = 0; k < xPoints.length; k++) {
                        if (x1t == 0.0) {
                            // Infinite slope thru (0,0)
                            d1 += Math.abs(xPoints[k]);
                        } else if (y1t == 0.0) {
                            // Zero slope thru (0, 0)
                            d1 += Math.abs(yPoints[k]);
                        } else {
                            slope = y1t / x1t;
                            d1 += Math.abs( (slope * xPoints[k] - yPoints[k]) / Math.sqrt(slope * slope + 1));
                        }
                    }
                    d2 = 0.0;
                    for (k = 0; k < xPoints.length; k++) {
                        if (x2t == 0.0) {
                            // Infinite slope thru (0,0)
                            d2 += Math.abs(xPoints[k]);
                        } else if (y2t == 0.0) {
                            // Zero slope thru (0, 0)
                            d2 += Math.abs(yPoints[k]);
                        } else {
                            slope = y2t / x2t;
                            d2 += Math.abs( (slope * xPoints[k] - yPoints[k]) / Math.sqrt(slope * slope + 1));
                        }
                    }
                    if (d1 < d2) {
                        tangentX = (float) x1t;
                        tangentY = (float) y1t;
                        normalX = (float) x2t;
                        normalY = (float) y2t;
                    } else {
                        tangentX = (float) x2t;
                        tangentY = (float) y2t;
                        normalX = (float) x1t;
                        normalY = (float) y1t;
                    }
                }
                omega = Math.atan2(normalY, normalX);
                // Change omega range from -PI to PI to 0 to 2*PI
                if (omega < 0.0) {
                    omega = omega + 2.0 * Math.PI;
                }
                omegaArray[indexPtr] = (short) (omega / omegaBinWidth);
                openArray[indexPtr++] = true;
            } // for (n = 1; n <= openLength[i] - 2; n++)
        } // for (i = 0; i < numOpenCurves; i++)
        openStart = null;
        openLength = null;
        xArray = null;
        yArray = null;

        // Find a position and length of closed curve
        numClosedCurves = 0;
        xPoints = new float[2 * sidePointsForTangent + 1];
        yPoints = new float[2 * sidePointsForTangent + 1];
        closedX = new int[2 * (xDim + yDim)];
        closedY = new int[2 * (xDim + yDim)];
        VOIMask = new short[sourceSlice];
        for (y = 0; y < yDim; y++) {
            offset = y * xDim;
            for (x = 0; x < xDim; x++) {
                index = offset + x;
                if ( !foundArray[index]) {
                    startPtr = indexPtr;
                    foundArray[index] = true;
                    numClosedCurves++;
                    closedVOI = new VOI((short) numClosedCurves, "VOI" + numClosedCurves, 1, VOI.CONTOUR, -1.0f);
                    closedX[0] = x;
                    closedY[0] = y;
                    closedLength = 1;
                    indexArray[indexPtr++] = index;
                    while ( ( !foundArray[neighbor1[index]]) || ( !foundArray[neighbor2[index]])) {
                        if ( !foundArray[neighbor1[index]]) {
                            index = neighbor1[index];
                        } else {
                            index = neighbor2[index];
                        }
                        foundArray[index] = true;
                        closedX[closedLength] = index % xDim;
                        closedY[closedLength] = index / xDim;
                        closedLength++;
                        indexArray[indexPtr++] = index;
                    } // while ((!foundArray[neighbor1[index]]) || (!foundArray[neighbor2[index]]))
                    xVal = new int[closedLength];
                    yVal = new int[closedLength];
                    zVal = new int[closedLength];
                    for (i = 0; i < closedLength; i++) {
                        xVal[i] = closedX[i];
                        yVal[i] = closedY[i];
                    }
                    closedVOI.importCurve(xVal, yVal, zVal, 0);
                    VOIMask = closedVOI.createShortMask(xDim, yDim, VOIMask, false);
                    try {
                        closedVOI.finalize();
                    } catch (Throwable e) {
                        ViewUserInterface.getReference().setDataText("Exception on closedVOI.finalize()");
                    }
                    closedVOI = null;
                    endPtr = indexPtr - 1;
                    indexPtr = startPtr;
                    for (n = 0; n <= closedLength - 1; n++) {
                        // Put the tangent point at index sidePointsForTangent in the
                        // center of the xPoints and yPoints array with sidePointsForTangent points
                        // to each side.
                        startWrapPoints = Math.max(0, sidePointsForTangent - n);
                        endWrapPoints = Math.max(0, sidePointsForTangent - (closedLength - 1 - n));
                        for (k = 0; k < startWrapPoints; k++) {
                            xPoints[k] = indexArray[endPtr - (startWrapPoints - k)] % xDim;
                            yPoints[k] = indexArray[endPtr - (startWrapPoints - k)] / xDim;
                        }
                        for (k = startWrapPoints, j = indexPtr - sidePointsForTangent + startWrapPoints; k < 2
                                * sidePointsForTangent + 1 - endWrapPoints; j++, k++) {
                            xPoints[k] = indexArray[j] % xDim;
                            yPoints[k] = indexArray[j] / xDim;
                        }
                        for (j = 0, k = 2 * sidePointsForTangent + 1 - endWrapPoints; k < 2 * sidePointsForTangent + 1; j++, k++) {
                            xPoints[k] = indexArray[startPtr + j] % xDim;
                            yPoints[k] = indexArray[startPtr + j] / xDim;
                        }
                        // For the closed curve find the slope and y axis intercept of the tangent line to the curve at
                        // a point
                        // With a user specified sidePointsForTangent on each side of a point find the tangent line that
                        // minimizes the sum of the squared distances from these side points to the tangent line
                        if (sidePointsForTangent == 1) {
                            xpc = xPoints[1];
                            ypc = yPoints[1];
                            tangentX = (xPoints[2] - xPoints[0]) / 2.0f;
                            tangentY = (yPoints[2] - yPoints[0]) / 2.0f;
                            if (tangentX == 0) {
                                normalX = 1;
                                normalY = 0;
                            } else if (tangentY == 0) {
                                normalX = 0;
                                normalY = 1;
                            } else {
                                normalX = -tangentY / tangentX;
                                normalY = 1;
                            }
                        } // if (sidePointsForTangent == 1)
                        else {
                            // Center all points for tangent point touching curve at (0, 0)
                            // That is, use an x axis and a y axis going thru the tangent point
                            xpc = xPoints[sidePointsForTangent];
                            ypc = yPoints[sidePointsForTangent];
                            for (k = 0; k < xPoints.length; k++) {
                                xPoints[k] = xPoints[k] - xpc;
                                yPoints[k] = yPoints[k] - ypc;
                            }
                            xSqSum = 0.0;
                            ySqSum = 0.0;
                            xySum = 0.0;
                            for (k = 0; k < xPoints.length; k++) {
                                xSqSum += xPoints[k] * xPoints[k];
                                ySqSum += yPoints[k] * yPoints[k];
                                xySum += xPoints[k] * yPoints[k];
                            }
                            if (xySum != 0.0) {
                                var = Math.sqrt(ySqSum * ySqSum - 2.0 * xSqSum * ySqSum + xSqSum * xSqSum + 4.0 * xySum
                                        * xySum);
                                x1t = 0.5 * ( ( -ySqSum + xSqSum + var) / xySum);
                                x2t = 0.5 * ( ( -ySqSum + xSqSum - var) / xySum);
                                y1t = 1.0;
                                y2t = 1.0;
                            } else {
                                // If all points are symmetric to either this new x axis or this new y axis, then
                                // their product sum is 0 and the tangentX, tangentY must be 1,0 or 0,1
                                x1t = 1.0;
                                x2t = 0.0;
                                y1t = 0.0;
                                y2t = 1.0;
                            }
                            // x1t, y1t and x2t, y2t are perpindicular. To find the solution, calculate the sum of
                            // distances from the curve points to the line for the 2 cases
                            // The shortest distance is the correct solution
                            // Distance from AX + BY + C = 0 to P1 is
                            // abs((A*x1 + B*y1 + C))/sqrt(A**2 + B**2)
                            // Here A = slope, B = -1, and C = 0.
                            d1 = 0.0;
                            for (k = 0; k < xPoints.length; k++) {
                                if (x1t == 0.0) {
                                    // Infinite slope thru (0,0)
                                    d1 += Math.abs(xPoints[k]);
                                } else if (y1t == 0.0) {
                                    // Zero slope thru (0, 0)
                                    d1 += Math.abs(yPoints[k]);
                                } else {
                                    slope = y1t / x1t;
                                    d1 += Math.abs( (slope * xPoints[k] - yPoints[k]) / Math.sqrt(slope * slope + 1));
                                }
                            }
                            d2 = 0.0;
                            for (k = 0; k < xPoints.length; k++) {
                                if (x2t == 0.0) {
                                    // Infinite slope thru (0,0)
                                    d2 += Math.abs(xPoints[k]);
                                } else if (y2t == 0.0) {
                                    // Zero slope thru (0, 0)
                                    d2 += Math.abs(yPoints[k]);
                                } else {
                                    slope = y2t / x2t;
                                    d2 += Math.abs( (slope * xPoints[k] - yPoints[k]) / Math.sqrt(slope * slope + 1));
                                }
                            }
                            if (d1 < d2) {
                                tangentX = (float) x1t;
                                tangentY = (float) y1t;
                                normalX = (float) x2t;
                                normalY = (float) y2t;
                            } else {
                                tangentX = (float) x2t;
                                tangentY = (float) y2t;
                                normalX = (float) x1t;
                                normalY = (float) y1t;
                            }
                        }
                        omega = Math.atan2(normalY, normalX);
                        // Change omega range from -PI to PI to 0 to 2*PI
                        if (omega < 0.0) {
                            omega = omega + 2.0 * Math.PI;
                        }
                        xt = (int) Math.round(xpc + 2.0 * Math.cos(omega));
                        yt = (int) Math.round(ypc + 2.0 * Math.sin(omega));
                        indext = xt + xDim * yt;
                        if (VOIMask[indext] != numClosedCurves) {
                            // Not in VOI - take normal going the opposite way
                            if (omega < Math.PI) {
                                omega = omega + Math.PI;
                            } else {
                                omega = omega - Math.PI;
                            }
                        }
                        omegaIndex = (int) (omega / omegaBinWidth);
                        omegaArray[indexPtr++] = (short) omegaIndex;
                    } // for (n = 0; n <= closedLength - 1; n++)
                }
            } // for (x = 0; x < xDim; x++)
        } // for (y = 0; y < yDim; y++)

        neighbor1 = null;
        neighbor2 = null;
        foundArray = null;
        xPoints = null;
        yPoints = null;

        ViewUserInterface.getReference().setDataText("Number of closed curves = " + numClosedCurves + "\n");

        // Calculate the desired number of bins that would be used for each parameter if memory were not a
        // limitation.
        minXCenter = 0.0f;
        maxXCenter = xDim - 1;
        minYCenter = 0.0f;
        maxYCenter = yDim - 1;
        bytesPerCell = 2 * 4 + 2; // xCenter float, yCenter float, and short count
        xCenterBins = (int) Math.ceil( (maxXCenter - minXCenter) / maxPixelBinWidth);
        yCenterBins = (int) Math.ceil( (maxYCenter - minYCenter) / maxPixelBinWidth);
        thetaBins = 1;
        maxRadiansDiff = maxDegreesBinWidth * Math.PI / 180.0;
        if (allowRotation) {
            thetaBins = (int) Math.ceil( (2.0 * Math.PI) / maxRadiansDiff);
        }
        if ( !allowScaling) {
            scaleBins = 1;
        }
        longNumBins = (long) xCenterBins * (long) yCenterBins * (long) thetaBins * (long) scaleBins;
        numBins = (int) longNumBins;
        desiredBytes = longNumBins * (long) bytesPerCell;
        actualBytesAvailable = (long) maxBufferSize * 1024L * 1024L;
        if (actualBytesAvailable < desiredBytes) {
            // Must shrink the number of each bins used for each parameter by the fourth, third, or second root of
            // deisredBytes/actualBytesAvailable to keep the buffer size down to actualBytesAvailable
            if (allowRotation && allowScaling) {
                shrinkFactor = Math.pow((double) desiredBytes / (double) actualBytesAvailable, 0.25);
            } else if (allowRotation || allowScaling) {
                shrinkFactor = Math.pow((double) desiredBytes / (double) actualBytesAvailable, 1.0 / 3.0);
            } else {
                shrinkFactor = Math.pow((double) desiredBytes / (double) actualBytesAvailable, 0.5);
            }
            maxPixelBinWidth = (float) (maxPixelBinWidth * shrinkFactor);
            xCenterBins = (int) Math.ceil( (maxXCenter - minXCenter) / maxPixelBinWidth);
            yCenterBins = (int) Math.ceil( (maxYCenter - minYCenter) / maxPixelBinWidth);
            if (allowRotation) {
                maxRadiansDiff = maxRadiansDiff * shrinkFactor;
                thetaBins = (int) Math.ceil( (2.0 * Math.PI) / maxRadiansDiff);
            }
            if (allowScaling) {
                scaleBins = (int) Math.ceil(scaleBins / shrinkFactor);
            }
            numBins = xCenterBins * yCenterBins * thetaBins * scaleBins;
        } // if (actualBytesAvailable < desiredBytes)
        if (allowRotation) {
            thetaArray = new float[thetaBins];
            for (i = 0; i < thetaBins; i++) {
                thetaArray[i] = (float) (i * 2.0 * Math.PI) / thetaBins;
            }
        }
        if (allowScaling) {
            maxToMinRatio = maxScaleFactor / minScaleFactor;
            scaleArray = new float[scaleBins];
            scaleArray[0] = minScaleFactor;
            scaleArray[scaleBins - 1] = maxScaleFactor;
            for (i = 1; i < scaleBins - 1; i++) {
                scaleArray[i] = (float) (minScaleFactor * Math
                        .pow(maxToMinRatio, (double) i / (double) (scaleBins - 1)));
            }
        }
        xyBins = xCenterBins * yCenterBins;
        ViewUserInterface.getReference().setDataText("xCenterBins = " + xCenterBins + "\n");
        ViewUserInterface.getReference().setDataText("yCenterBins = " + yCenterBins + "\n");
        ViewUserInterface.getReference().setDataText("thetaBins = " + thetaBins + "\n");
        ViewUserInterface.getReference().setDataText("scaleBins = " + scaleBins + "\n");
        ViewUserInterface.getReference().setDataText("numBins = " + numBins + "\n");
        xCenterArray = new float[xyBins][thetaBins][scaleBins];
        yCenterArray = new float[xyBins][thetaBins][scaleBins];
        countArray = new int[xyBins][thetaBins][scaleBins];
        foundPoint = new boolean[numPoints];
        xCenterTable = new float[objectsToFind];
        yCenterTable = new float[objectsToFind];
        if (allowRotation) {
            thetaTable = new float[objectsToFind];
        }
        if (allowScaling) {
            scaleTable = new float[objectsToFind];
        }
        countTable = new short[objectsToFind];

        while (objectsFound < objectsToFind) {
            for (i = 0; i < numPoints; i++) {
                x = indexArray[i] % xDim;
                y = indexArray[i] / xDim;
                omegaIndex = omegaArray[i];
                omega = omegaIndex * omegaBinWidth;
                for (j = 0; j < thetaBins; j++) {
                    if (allowRotation) {
                        omega = omega - thetaArray[j];
                        if (omega < 0.0) {
                            omega = omega + 2.0 * Math.PI;
                        } else if (omega >= 2.0 * Math.PI) {
                            omega = omega - 2.0 * Math.PI;
                        }
                        omegaIndex = (int) (omega / omegaBinWidth);
                    }
                    rBetaLength = omegaRBetaList[omegaIndex].size();
                    for (k = 0; k < rBetaLength; k++) {
                        floatArray = (float[]) omegaRBetaList[omegaIndex].get(k);
                        r = floatArray[0];
                        beta = floatArray[1];
                        if (allowRotation) {
                            beta = floatArray[1] - thetaArray[j];
                        }
                        for (m = 0; m < scaleBins; m++) {
                            if (allowScaling) {
                                r = scaleArray[m] * floatArray[0];
                            }
                            xCenter = x + r * Math.cos(beta);
                            xCenterIndex = (int) ( (xCenter - minXCenter) / maxPixelBinWidth);
                            if ( (xCenterIndex >= 0) && (xCenterIndex < xCenterBins)) {
                                yCenter = y + r * Math.sin(beta);
                                yCenterIndex = (int) ( (yCenter - minYCenter) / maxPixelBinWidth);
                                if ( (yCenterIndex >= 0) && (yCenterIndex < yCenterBins)) {
                                    index = xCenterIndex + xCenterBins * yCenterIndex;
                                    xCenterArray[index][j][m] += xCenter;
                                    yCenterArray[index][j][m] += yCenter;
                                    countArray[index][j][m]++;
                                } // if (yCenterIndex >= 0) && (yCenterIndex < yCenterBins))
                            } // if (xCenterIndex >= 0) && (xCenterIndex < xCenterBins))
                        } // for (m = 0; m < scaleBins; m+++)
                    } // for (k = 0; k < rBetaLength; k++)
                    if (openArray[i]) {
                        if (omega < Math.PI) {
                            omega = omega + Math.PI;
                        } else {
                            omega = omega - Math.PI;
                        }
                        omegaIndex = (int) (omega / omegaBinWidth);
                        rBetaLength = omegaRBetaList[omegaIndex].size();
                        for (k = 0; k < rBetaLength; k++) {
                            floatArray = (float[]) omegaRBetaList[omegaIndex].get(k);
                            r = floatArray[0];
                            beta = floatArray[1];
                            if (allowRotation) {
                                beta = floatArray[1] - thetaArray[j];
                            }
                            for (m = 0; m < scaleBins; m++) {
                                if (allowScaling) {
                                    r = scaleArray[m] * floatArray[0];
                                }
                                xCenter = x + r * Math.cos(beta);
                                xCenterIndex = (int) ( (xCenter - minXCenter) / maxPixelBinWidth);
                                if ( (xCenterIndex >= 0) && (xCenterIndex < xCenterBins)) {
                                    yCenter = y + r * Math.sin(beta);
                                    yCenterIndex = (int) ( (yCenter - minYCenter) / maxPixelBinWidth);
                                    if ( (yCenterIndex >= 0) && (yCenterIndex < yCenterBins)) {
                                        index = xCenterIndex + xCenterBins * yCenterIndex;
                                        xCenterArray[index][j][m] += xCenter;
                                        yCenterArray[index][j][m] += yCenter;
                                        countArray[index][j][m]++;
                                    } // if (yCenterIndex >= 0) && (yCenterIndex < yCenterBins))
                                } // if (xCenterIndex >= 0) && (xCenterIndex < xCenterBins))
                            } // for (m = 0; m < scaleBins; m+++)
                        } // for (k = 0; k < rBetaLength; k++)
                    } // if (openArray[i])
                } // for (j = 0; j < thetaBins; j++)
            } // for (i = 0; i < numPoints; i++)

            maxXYIndex = -1;
            maxXIndex = -1;
            maxYIndex = -1;
            maxThetaIndex = -1;
            maxScaleIndex = -1;
            maxCount = 0;
            for (i = 0; i < xyBins; i++) {
                for (k = 0; k < thetaBins; k++) {
                    for (m = 0; m < scaleBins; m++) {
                        if (countArray[i][k][m] > maxCount) {
                            maxXYIndex = i;
                            maxXIndex = i % xCenterBins;
                            maxYIndex = i / xCenterBins;
                            maxThetaIndex = k;
                            maxScaleIndex = m;
                            maxCount = countArray[i][k][m];
                        }
                    } // for (m = 0; m < scaleBins; m++)
                } // for (k = 0; k < thetaBins; k++)
            } // for (i = 0; i < xyBins; i++)
            if (maxCount < 2) {
                break;
            }

            xCenter = xCenterArray[maxXYIndex][maxThetaIndex][maxScaleIndex] / maxCount;
            yCenter = yCenterArray[maxXYIndex][maxThetaIndex][maxScaleIndex] / maxCount;
            xCenterTable[objectsFound] = (float) xCenter;
            yCenterTable[objectsFound] = (float) yCenter;
            if (allowRotation) {
                thetaTable[objectsFound] = thetaArray[maxThetaIndex];
            }
            if (allowScaling) {
                scaleTable[objectsFound] = scaleArray[maxScaleIndex];
            }
            countTable[objectsFound] = (short) maxCount;
            objectsFound++;
            ViewUserInterface.getReference().setDataText("Object # " + objectsFound + " found\n");
            ViewUserInterface.getReference().setDataText(" x center = " + xCenter + "\n");
            ViewUserInterface.getReference().setDataText(" y center = " + yCenter + "\n");
            if (allowRotation) {
                ViewUserInterface.getReference().setDataText(
                        " theta = " + (thetaTable[objectsFound - 1] * 180 / Math.PI) + "\n");
            }
            if (allowScaling) {
                ViewUserInterface.getReference().setDataText(" scale factor = " + scaleTable[objectsFound - 1] + "\n");
            }
            centerPtVOI = new VOI((short) (objectsFound), "center" + objectsFound + ".voi", 1, VOI.POINT, -1.0f);
            centerPtVOI.setColor(Color.white);
            xArr[0] = (float) xCenter;
            yArr[0] = (float) yCenter;
            zArr[0] = 0.0f;
            centerPtVOI.importCurve(xArr, yArr, zArr, 0);
            ((VOIPoint) (centerPtVOI.getCurves()[0].elementAt(0))).setFixed(true);
            ((VOIPoint) (centerPtVOI.getCurves()[0].elementAt(0))).setLabel(String.valueOf(objectsFound));
            srcImage.registerVOI(centerPtVOI);

            if (objectsFound == objectsToFind) {
                break;
            }
            // If an object was found, then zero the Hough transform array before acquiring a
            // new set of xCenter, yCenter points.
            for (i = 0; i < xyBins; i++) {
                for (k = 0; k < thetaBins; k++) {
                    for (m = 0; m < scaleBins; m++) {
                        xCenterArray[i][k][m] = 0.0f;
                        yCenterArray[i][k][m] = 0.0f;
                        countArray[i][k][m] = 0;
                    } // for (m = 0; m < scaleBins; m++)
                } // for (k = 0; k < thetaBins; k++)
            } // for (i = 0; i < xyBins; i++)
            // If an object was found, then delete the points from the
            // from indexArray and omega before running the
            // Hough transform again
            pointsDeleted = 0;
            for (i = 0; i < numPoints; i++) {
                x = indexArray[i] % xDim;
                y = indexArray[i] / xDim;
                omegaIndex = omegaArray[i];
                omega = omegaIndex * omegaBinWidth;
                if (allowRotation) {
                    omega = omega - thetaArray[maxThetaIndex];
                    if (omega < 0.0) {
                        omega = omega + 2.0 * Math.PI;
                    } else if (omega >= 2.0 * Math.PI) {
                        omega = omega - 2.0 * Math.PI;
                    }
                    omegaIndex = (int) (omega / omegaBinWidth);
                }
                rBetaLength = omegaRBetaList[omegaIndex].size();
                for (k = 0; k < rBetaLength; k++) {
                    floatArray = (float[]) omegaRBetaList[omegaIndex].get(k);
                    r = floatArray[0];
                    beta = floatArray[1];
                    if (allowRotation) {
                        beta = floatArray[1] - thetaArray[maxThetaIndex];
                    }
                    if (allowScaling) {
                        r = scaleArray[maxScaleIndex] * floatArray[0];
                    }
                    xCenter = x + r * Math.cos(beta);
                    xCenterIndex = (int) ( (xCenter - minXCenter) / maxPixelBinWidth);
                    if ( (xCenterIndex >= Math.max(0, maxXIndex - 1))
                            && (xCenterIndex <= Math.min(xCenterBins - 1, maxXIndex + 1))) {
                        yCenter = y + r * Math.sin(beta);
                        yCenterIndex = (int) ( (yCenter - minYCenter) / maxPixelBinWidth);
                        if ( (yCenterIndex >= Math.max(0, maxYIndex - 1))
                                && (yCenterIndex <= Math.min(yCenterBins - 1, maxYIndex + 1)) && ( !foundPoint[i])) {
                            foundPoint[i] = true;
                            pointsDeleted++;
                        } // if ((yCenterIndex >= Math.max(0, maxYIndex-1)) && (yCenterIndex <=
                        // Math.min(yCenterBins-1, maxYIndex+1)) && (!foundPoint[i]))
                    } // if ((xCenterIndex >= Math.max(0,maxXIndex-1)) && (xCenterIndex <= Math.min(xCenterBins-1,
                    // maxXIndex+1)))
                } // for (k = 0; k < rBetaLength; k++)
                if (openArray[i]) {
                    if (omega < Math.PI) {
                        omega = omega + Math.PI;
                    } else {
                        omega = omega - Math.PI;
                    }
                    omegaIndex = (int) (omega / omegaBinWidth);
                    rBetaLength = omegaRBetaList[omegaIndex].size();
                    for (k = 0; k < rBetaLength; k++) {
                        floatArray = (float[]) omegaRBetaList[omegaIndex].get(k);
                        r = floatArray[0];
                        beta = floatArray[1];
                        if (allowRotation) {
                            beta = floatArray[1] - thetaArray[maxThetaIndex];
                        }
                        if (allowScaling) {
                            r = scaleArray[maxScaleIndex] * floatArray[0];
                        }
                        xCenter = x + r * Math.cos(beta);
                        xCenterIndex = (int) ( (xCenter - minXCenter) / maxPixelBinWidth);
                        if ( (xCenterIndex >= Math.max(0, maxXIndex - 1))
                                && (xCenterIndex <= Math.min(xCenterBins - 1, maxXIndex + 1))) {
                            yCenter = y + r * Math.sin(beta);
                            yCenterIndex = (int) ( (yCenter - minYCenter) / maxPixelBinWidth);
                            if ( (yCenterIndex >= Math.max(0, maxYIndex - 1))
                                    && (yCenterIndex <= Math.min(yCenterBins - 1, maxYIndex + 1)) && ( !foundPoint[i])) {
                                foundPoint[i] = true;
                                pointsDeleted++;
                            } // if ((yCenterIndex >= Math.max(0, maxYIndex-1)) && (yCenterIndex <=
                            // Math.min(yCenterBins-1, maxYIndex+1)) && (!foundPoint[i]))
                        } // if ((xCenterIndex >= Math.max(0,maxXIndex-1)) && (xCenterIndex <= Math.min(xCenterBins-1,
                        // maxXIndex+1)))
                    } // for (k = 0; k < rBetaLength; k++)
                } // if (openArray[i])
            } // for (i = 0; i < numPoints; i++)

            if (pointsDeleted > 0) {
                newNumPoints = numPoints - pointsDeleted;
                if (newNumPoints == 0) {
                    break;
                }
                newIndexArray = new int[newNumPoints];
                newOmegaArray = new short[newNumPoints];
                newOpenArray = new boolean[newNumPoints];
                for (i = 0, j = 0; i < numPoints; i++) {
                    if ( !foundPoint[i]) {
                        newIndexArray[j] = indexArray[i];
                        newOmegaArray[j] = omegaArray[i];
                        newOpenArray[j] = openArray[i];
                        j++;
                    }
                } // for (i = 0, j = 0; i < numPoints; i++)
                numPoints = newNumPoints;
                foundPoint = null;
                foundPoint = new boolean[numPoints];
                indexArray = null;
                indexArray = new int[numPoints];
                omegaArray = null;
                omegaArray = new short[numPoints];
                openArray = null;
                openArray = new boolean[numPoints];
                for (i = 0; i < numPoints; i++) {
                    indexArray[i] = newIndexArray[i];
                    omegaArray[i] = newOmegaArray[i];
                    openArray[i] = newOpenArray[i];
                }
                newIndexArray = null;
                newOmegaArray = null;
                newOpenArray = null;
            } // if (pointsDeleted > 0)
        } // while (objectsFound < objectsToFind)

        srcImage.notifyImageDisplayListeners();

        // Restore original source values
        if ( !test) {
            try {
                srcImage.exportData(0, sourceSlice, srcBuffer);
            } catch (IOException e) {
                MipavUtil.displayError("IOException " + e + " on srcImage.exportData");

                setCompleted(false);

                return;
            }
        } // if (!test)

        setCompleted(true);
        return;
    }

}
