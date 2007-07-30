package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.Matrix;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogHoughEllipseChoice;

import java.io.*;
import java.util.*;

/**
 
 *  Pixel neighbors:
 *  0 1 2
 *  3   4
 *  5 6 7
 *  
 *  The algorithm works as follows:
 * 
 * References:
 * 1.) Computer Vision by Dana H. Ballard and Christopher M. Brown, Prentice-Hall, Inc., 1982,
 *     Section 4.3.4 Generalizing the Hough Transform, pp. 128-131.
 */
public class AlgorithmFindRtableObject extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------
    /** Number of bins covering tangent angle going from -PI/2 to PI/2 */
    private int omegaBins = 45;
    
    /** Number of points to take from each side of a point on a curve in determining a tangent
     * If only 1 point is used on each side, simply use avarage of slopes to each of the
     * neigboring points. */ 
    private int sidePointsForTangent;
    
    /** For each omega angle index, a linked list of R and B values is present
     *  omega is the tangent angle to the curve, and R and B give the distance
     *  and angle from the center of the VOI to the tangent point */
    private LinkedList omegaRBetaList[];
    
    /** Desired maximum pixel bin width for x, y center value.
     *  If maxBufferSize is not large enough, this number is increased. */
    private float maxPixelBinWidth;
    
    /** The maximum Hough transform size in megabytes - default is currently 256 */
    private int maxBufferSize;
    
    /** If true, allow rotation of R-table object */
    private boolean allowRotation;
    
    /** Desired maximum bin width for R-table object rotation.
     *  If maxBufferSize is not large enough, this number is increased.
     */
    private float maxDegreesBinWidth;
    
    /** If true, allow scaling of R-table object */
    private boolean allowScaling;
    
    private float minScaleFactor;
   
    private float maxScaleFactor;
    
    /** Desired number of scaling bins of entry.  If maxBufferSize is not large enough,
     *  this number is reduced.
     */
    private int scaleBins;
    
    /** Number of instances of R-table object to find in the image */
    private int objectsToFind;
    
    /** Minimum distances between instances of R-table objects */
    private float minDistance;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmFindRtableObject - default constructor.
     */
    public AlgorithmFindRtableObject() { }

    /**
     * AlgorithmFindRtableObject.
     *
     * @param  srcImg   Binary image that has 1 or more instances of the R-table object
     * @param  omegaBins Number of bins for tangent angle at point on curve going from -90 to +90
     * @param  sidePointsForTangent  Maximum number of points to take from each side of a point on a curve
     *                               in determining the tangent
     * @param  omegaRBetaList For each omega angle index, a linked list of R and B values is present
     *                        omega is the tangent angle to the curve, and R and B give the distance
     *                        and angle from the center of the VOI to the tangent point
     * @param maxPixelBinWidth Desired maximum pixel bin width for x, y center value.
     *                         If maxBufferSize is not large enough, this number is increased.
     * @param maxBufferSize The maximum Hough transform size in megabytes - default is currently 256
     * @param allowRotation If true, allow rotation of R-table object
     * @param maxDegreesBinWidth Desired maximum angle bin width for object rotation
     *                           If maxBufferSize is not large enough, this number is increased.
     * @param allowScaling If true, allow scaling of R-table object
     * @param minScaleFactor
     * @param maxScaleFactor
     * @param scaleBins Desired number of scaling bins of entry.  If maxBufferSize is not large enough,
     *                    this number is reduced.
     * @param objectsToFind Number of instances of R-table object to find in the image
     * @param minDistance Minimum distances between instances of R-table objects
     */
    public AlgorithmFindRtableObject(ModelImage srcImg, int omegaBins, int sidePointsForTangent, 
                                     LinkedList omegaRBetaList[], float maxPixelBinWidth, int maxBufferSize, 
                                     boolean allowRotation, float maxDegreesBinWidth,
                                     boolean allowScaling, float minScaleFactor, float maxScaleFactor,
                                     int scaleBins, int objectsToFind, float minDistance) {
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
        this.minDistance = minDistance;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * finalize -
     */
    public void finalize() {
        super.finalize();
    }
    
    /**
     * Starts the program.
     */
    public void runAlgorithm() {
        int x, y;
        int offset;

        int xDim;

        int yDim;

        int sourceSlice;
        
        int numPoints;

        int i, j, k, m, n;
        int index;
        double theta;
        double theta1;
        double theta2;
        byte[] srcBuffer;
        boolean test = false;
        double xCenter;
        double yCenter;
        float xCenterArray[][][];
        float yCenterArray[][][];
        int countArray[][][];
        boolean selectedEllipse[];
        JDialogHoughEllipseChoice choice;
        byte value = 0;
        int x1;
        int y1;
        int x2;
        int y2;
        int x3;
        int y3;
        double x1c;
        double y1c;
        double x2c;
        double y2c;
        double x3c;
        double y3c;
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
        short omegaArray[];
        short newOmegaArray[];
        float interceptArray[];
        float newInterceptArray[];
        int startPtr;
        int presentSidePoints;
        float xPoints[];
        float yPoints[];
        int endPtr;
        int startWrapPoints;
        int endWrapPoints;
        double a1;
        double b1;
        double alpha;
        double cosalpha;
        double sinalpha;
        double angle;
        double r;
        double beta;
        double cosbeta;
        double sinbeta;
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
        int ellipsesFound = 0;
        int ellipseFindCycles = 0;
        int pointSetsAcquired = 0;
        RandomNumberGen randomGen;
        int number1;
        int number2;
        int number3;
        double minDistanceSquared;
        double maxDistanceSquared;
        int distanceSquared;
        int xDiff;
        int yDiff;
        float slope1;
        float slope2;
        float slope3;
        float intercept1;
        float intercept2;
        float intercept3;
        float tx12;
        float ty12;
        float midx12;
        float midy12;
        float tangentx12;
        float tangenty12;
        float slope12;
        float intercept12 = 0.0f;
        float tx23;
        float ty23;
        float midx23;
        float midy23;
        float tangentx23;
        float tangenty23;
        float slope23;
        float intercept23 = 0.0f;
        Matrix A;
        Matrix B;
        Matrix X = null;
        double a = 1.0;
        double b = 0.0;
        double c = 1.0;
        double r1;
        double r2;
        double aminusc;
        double sqr2 = Math.sqrt(2.0);
        double twob;
        double var;
        double aplusc;
        float minXCenter;
        float maxXCenter;
        float minYCenter;
        float maxYCenter;
        float minR1;
        float maxR1;
        float minR2;
        float maxR2;
        float minTheta;
        int bytesPerCell;
        int xCenterBins;
        int yCenterBins;
        int r1Bins;
        int r2Bins;
        int thetaBins;
        double maxRadiansDiff;
        long desiredBytes;
        long actualBytesAvailable;
        double shrinkFactor = 1.0;
        long longNumBins;
        int numBins;
        int xCenterIndex;
        int yCenterIndex;
        int r1Index;
        int r2Index;
        int thetaIndex;
        int scale1;
        int scale12;
        int scale123;
        int scale1234;
        int maxIndex = -1;
        int maxCount = 0;
        double h;
        double perimiter;
        int minPixels;
        boolean foundPoint[];
        double xc;
        double yc;
        double costheta1;
        double costheta2;
        double sintheta1;
        double sintheta2;
        double r1Sq;
        double r2Sq;
        double var1;
        double var2;
        double checkVal;
        double upperLimit = 1.0 + 0.01;
        double lowerLimit = 1.0 - 0.01;
        short pointsOnEllipse;
        short pointsOnEllipse1;
        short pointsOnEllipse2;
        float xCenterTable[];
        float yCenterTable[];
        float thetaTable[] = null;
        float scaleTable[] = null;
        short countTable[];
        int pointsDeleted;
        int newNumPoints;
        boolean testedArray[];
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
        boolean ellipseDetected;
        int objectsFound = 0;
        int omegaIndex;
        int rBetaLength;
        float floatArray[];
        float thetaArray[] = null;
        double maxToMinRatio;
        float scaleArray[] = null;
        int xyBins;
        int maxXYIndex = 0;
        int maxThetaIndex = 0;
        int maxScaleIndex = 0;

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
        
        if (test) {
            for (y = 0; y < yDim; y++) {
                offset = y * xDim;
                for (x = 0; x < xDim; x++) {
                    index = offset + x; 
                    srcBuffer[index] = 0;
                } // for (x = 0; x < xDim; x++)
            } // for (y = 0; y < yDim; y++)
            
            xCenter = (xDim-1)/2.0;
            yCenter = (yDim-1)/2.0;
            a1 = 50.0;
            b1 = 25.0;
            for (i = 0; i < 360; i++) {
                alpha = i * Math.PI/180.0;
                cosalpha = Math.cos(alpha);
                sinalpha = Math.sin(alpha);
                x = (int)(xCenter + a1 * cosalpha);
                y = (int)(yCenter + b1 * sinalpha);
                index = x + (xDim*y);
                srcBuffer[index] = 1; 
            }
            
            a1 = 75.0;
            b1 = 37.5;
            angle = Math.PI/4.0;
            beta = -angle;
            cosbeta = Math.cos(beta);
            sinbeta = Math.sin(beta);
            for (i = 0; i < 720; i++) {
                alpha = i * Math.PI/360.0;
                cosalpha = Math.cos(alpha);
                sinalpha = Math.sin(alpha);
                x = (int)(xCenter + 50.0 + a1 * cosalpha * cosbeta - b1 * sinalpha * sinbeta);
                y = (int)(yCenter + 50.0 + a1 * cosalpha * sinbeta + b1 * sinalpha * cosbeta);
                index = x + (xDim*y);
                srcBuffer[index] = 1; 
            }
            /*for (i = 0; i < 90; i++) {
                alpha = i * Math.PI/360.0;
                cosalpha = Math.cos(alpha);
                sinalpha = Math.sin(alpha);
                x = (int)(xCenter + 50.0 + a1 * cosalpha * cosbeta - b1 * sinalpha * sinbeta);
                y = (int)(yCenter + 50.0 + a1 * cosalpha * sinbeta + b1 * sinalpha * cosbeta);
                index = x + (xDim*y);
                srcBuffer[index] = 1; 
            }
            
            for (i = 180; i < 270; i++) {
                alpha = i * Math.PI/360.0;
                cosalpha = Math.cos(alpha);
                sinalpha = Math.sin(alpha);
                x = (int)(xCenter + 50.0 + a1 * cosalpha * cosbeta - b1 * sinalpha * sinbeta);
                y = (int)(yCenter + 50.0 + a1 * cosalpha * sinbeta + b1 * sinalpha * cosbeta);
                index = x + (xDim*y);
                srcBuffer[index] = 1; 
            }
            
            for (i = 360; i < 450; i++) {
                alpha = i * Math.PI/360.0;
                cosalpha = Math.cos(alpha);
                sinalpha = Math.sin(alpha);
                x = (int)(xCenter + 50.0 + a1 * cosalpha * cosbeta - b1 * sinalpha * sinbeta);
                y = (int)(yCenter + 50.0 + a1 * cosalpha * sinbeta + b1 * sinalpha * cosbeta);
                index = x + (xDim*y);
                srcBuffer[index] = 1; 
            }
            
            for (i = 540; i < 630; i++) {
                alpha = i * Math.PI/360.0;
                cosalpha = Math.cos(alpha);
                sinalpha = Math.sin(alpha);
                x = (int)(xCenter + 50.0 + a1 * cosalpha * cosbeta - b1 * sinalpha * sinbeta);
                y = (int)(yCenter + 50.0 + a1 * cosalpha * sinbeta + b1 * sinalpha * cosbeta);
                index = x + (xDim*y);
                srcBuffer[index] = 1; 
            }*/
            
            a1 = 50.0;
            b1 = 25.0;
            /*angle = -Math.PI/4.0;
            beta = -angle;
            cosbeta = Math.cos(beta);
            sinbeta = Math.sin(beta);
            for (i = 0; i < 720; i++) {
                alpha = i * Math.PI/360.0;
                cosalpha = Math.cos(alpha);
                sinalpha = Math.sin(alpha);
                x = (int)(xCenter -50.0 + a1 * cosalpha * cosbeta - b1 * sinalpha * sinbeta);
                y = (int)(yCenter -50.0 + a1 * cosalpha * sinbeta + b1 * sinalpha * cosbeta);
                index = x + (xDim*y);
                srcBuffer[index] = 1; 
            }*/
            for (i = 0; i < 90; i++) {
                alpha = i * Math.PI/360.0;
                cosalpha = Math.cos(alpha);
                sinalpha = Math.sin(alpha);
                x = (int)(xCenter -50.0 + a1 * cosalpha * cosbeta - b1 * sinalpha * sinbeta);
                y = (int)(yCenter -50.0 + a1 * cosalpha * sinbeta + b1 * sinalpha * cosbeta);
                index = x + (xDim*y);
                srcBuffer[index] = 1; 
            }
            
            for (i = 180; i < 270; i++) {
                alpha = i * Math.PI/360.0;
                cosalpha = Math.cos(alpha);
                sinalpha = Math.sin(alpha);
                x = (int)(xCenter -50.0 + a1 * cosalpha * cosbeta - b1 * sinalpha * sinbeta);
                y = (int)(yCenter -50.0 + a1 * cosalpha * sinbeta + b1 * sinalpha * cosbeta);
                index = x + (xDim*y);
                srcBuffer[index] = 1; 
            }
            
            for (i = 360; i < 450; i++) {
                alpha = i * Math.PI/360.0;
                cosalpha = Math.cos(alpha);
                sinalpha = Math.sin(alpha);
                x = (int)(xCenter -50.0 + a1 * cosalpha * cosbeta - b1 * sinalpha * sinbeta);
                y = (int)(yCenter -50.0 + a1 * cosalpha * sinbeta + b1 * sinalpha * cosbeta);
                index = x + (xDim*y);
                srcBuffer[index] = 1; 
            }
            
            for (i = 540; i < 630; i++) {
                alpha = i * Math.PI/360.0;
                cosalpha = Math.cos(alpha);
                sinalpha = Math.sin(alpha);
                x = (int)(xCenter -50.0 + a1 * cosalpha * cosbeta - b1 * sinalpha * sinbeta);
                y = (int)(yCenter -50.0 + a1 * cosalpha * sinbeta + b1 * sinalpha * cosbeta);
                index = x + (xDim*y);
                srcBuffer[index] = 1; 
            }
   
            //setCompleted(true);
            //return;
        } // if (test)
        
        try {
            destImage.importData(0, srcBuffer, true);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on destImage.importData");

            setCompleted(true);

            return;
        }
        
        // Skeletonize the binary image
        // Prune off branches with 2 or less pixels
        pruningPix = 2;
        entireImage = true;
        algoMorph2D = new AlgorithmMorphology2D(destImage, 0, 0.0f, AlgorithmMorphology2D.SKELETONIZE, 0, 0, pruningPix, 0, entireImage);
        algoMorph2D.run();
        algoMorph2D.finalize();
        
        try {
            destImage.exportData(0, sourceSlice, srcBuffer);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on destImage.exportData");

            setCompleted(false);

            return;
        }
        
        for (i = 0; i < sourceSlice; i++) {
            if (srcBuffer[i] != 0) {
                value = srcBuffer[i];
                break;
            }
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
                        if ((neigh0 >= 0) && (neigh1 >= 0)) {
                            srcBuffer[neigh1] = 0;
                            neigh1 = -1;
                        }
                        if ((neigh1 >= 0) && (neigh2 >= 0)) {
                            srcBuffer[neigh1] = 0;
                            neigh1 = -1;
                        }
                        if ((neigh0 >= 0) && (neigh3 >= 0)) {
                            srcBuffer[neigh3] = 0;
                            neigh3 = -1;
                        }
                        if ((neigh3 >= 0) && (neigh5 >= 0)) {
                            srcBuffer[neigh3] = 0;
                            neigh3 = -1;
                        }
                        if ((neigh2 >= 0) && (neigh4 >= 0)) {
                            srcBuffer[neigh4] = 0;
                            neigh4 = -1;
                        }
                        if ((neigh4 >= 0) && (neigh7 >= 0)) {
                            srcBuffer[neigh4] = 0;
                            neigh4 = -1;
                        }
                        if ((neigh5 >= 0) && (neigh6 >= 0)) {
                            srcBuffer[neigh6] = 0;
                            neigh6 = -1;
                        }
                        if ((neigh6 >= 0) && (neigh7 >= 0)) {
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
                            }
                            else {
                                neighbor2[index] = index - xDim;
                            }
                        }
                        if (x < xDim - 1) {
                            if (srcBuffer[index - xDim + 1] != 0) {
                                neighbors++;
                                if (neighbor1[index] == -1) {
                                    neighbor1[index] = index - xDim + 1;
                                }
                                else {
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
                            }
                            else {
                                neighbor2[index] = index - 1;
                            }
                        }
                    } // if (x > 0)
                    if (x < xDim - 1) {
                        if (srcBuffer[index + 1] != 0) {
                            neighbors++;
                            if (neighbor1[index] == -1) {
                                neighbor1[index] = index + 1;
                            }
                            else {
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
                                }
                                else {
                                    neighbor2[index] = index + xDim - 1;
                                }
                            }
                        }
                        if (srcBuffer[index + xDim] != 0) {
                            neighbors++;
                            if (neighbor1[index] == -1) {
                                neighbor1[index] = index + xDim;
                            }
                            else {
                                neighbor2[index] = index + xDim;
                            }
                        }
                        if (x < xDim - 1) {
                            if (srcBuffer[index + xDim + 1] != 0) {
                                neighbors++;
                                if (neighbor1[index] == -1) {
                                    neighbor1[index] = index + xDim + 1;
                                }
                                else {
                                    neighbor2[index] = index + xDim + 1;
                                }
                            }
                        }    
                    } // if (y < yDim - 1)
                    if (neighbors == 0) {
                        srcBuffer[index] = 0;
                        neighbor1[index] = -1;
                        neighbor2[index] = -1;
                    }
                    else {
                        numPoints++;
                        if (neighbors == 1) {
                            endPoints++;
                        }
                    }
                } // if (srcBuffer[index] != 0)
            } // for (x = 0; x < xDim; x++)
        } // for (y = 0; y < yDim; y++)
        
        numOpenCurves = endPoints/2;
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
                if ((neighbor2[index] == -1) && (!foundArray[index])) {
                    foundArray[index] = true;
                    openStart[i] = index;
                    openLength[i]++;
                    index = neighbor1[index];
                    foundArray[index] = true;
                    openLength[i]++;
                    while(neighbor2[index] != -1) {
                        if (!foundArray[neighbor1[index]]) {
                            index = neighbor1[index];
                        }
                        else {
                            index = neighbor2[index];
                        }
                        foundArray[index] = true;
                        openLength[i]++;
                    } // (while(neighbor2[index] != -1)
                    // Delete all open curves with only 2 points
                    // Also don't determine tangents of end points on longer curves,
                    // but use these 2 end points in determining tangents of more inner points
                    // These 2 end points will not be used in generating random 3 point sets.
                    numPoints = numPoints - 2;
                    if (openLength[i] == 2) {
                        srcBuffer[openStart[i]] = 0;
                        srcBuffer[neighbor1[openStart[i]]] = 0;
                        numOpenCurves--;
                        openLength[i] = 0;
                    }
                    else {
                        i++;
                    }
                }
            }
        }
        
        ViewUserInterface.getReference().setDataText("Number of open curves = " + numOpenCurves + "\n");
        
        // For the open curves find the slope and y axis intercept of the tangent line to the curve at a point
        // With a user specified sidePointsForTangent on each side of a point find the tangent line that
        // minimizes the sum of the squared distances from these side points to the tangent line 
        indexArray = new int[numPoints];
        omegaArray = new short[numPoints];
        interceptArray = new float[numPoints];
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
              if (!foundArray[neighbor1[nextPoint]]) {
                  nextPoint = neighbor1[nextPoint];
              }
              else {
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
                    tangentX = (xArray[n+1] - xArray[n-1])/2.0f;
                    tangentY = (yArray[n+1] - yArray[n-1])/2.0f;
                    if (tangentX == 0.0f) {
                        omegaArray[indexPtr++] = (short)0;
                    }
                    else {
                        omegaArray[indexPtr++] = (short)((Math.atan(tangentY/tangentX) + Math.PI/2.0)/omegaBins);
                    }
                } // if (presentSidePoints == 1)
                else {
                    xPoints = new float[2*presentSidePoints+1];
                    yPoints = new float[2*presentSidePoints+1];
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
                        xSqSum += xPoints[k]*xPoints[k];
                        ySqSum += yPoints[k]*yPoints[k];
                        xySum += xPoints[k]*yPoints[k];
                    }
                    if (xySum != 0.0) {
                        var = Math.sqrt(ySqSum*ySqSum - 2.0 * xSqSum * ySqSum + xSqSum * xSqSum + 4.0 * xySum * xySum);
                        x1t = 0.5 * ((-ySqSum + xSqSum + var)/xySum);
                        x2t = 0.5 * ((-ySqSum + xSqSum - var)/xySum);
                        y1t = 1.0;
                        y2t = 1.0;
                    }
                    else {
                        // If all points are symmetric to either this new x axis or this new y axis, then
                        // their product sum is 0 and the tangentX, tangentY must be 1,0 or 0,1
                        x1t = 1.0;
                        x2t = 0.0;
                        y1t = 0.0;
                        y2t = 1.0;
                    }
                    // x1t, y1t and x2t, y2t are perpindicular.  To find the solution, calculate the sum of
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
                        }
                        else if (y1t == 0.0) {
                            // Zero slope thru (0, 0)
                            d1 += Math.abs(yPoints[k]);
                        }
                        else {
                            slope = y1t/x1t;
                            d1 += Math.abs((slope * xPoints[k] - yPoints[k])/Math.sqrt(slope*slope + 1));
                        }
                    }
                    d2 = 0.0;
                    for (k = 0; k < xPoints.length; k++) {
                        if (x2t == 0.0) {
                            // Infinite slope thru (0,0)
                            d2 += Math.abs(xPoints[k]);
                        }
                        else if (y2t == 0.0) {
                            // Zero slope thru (0, 0)
                            d2 += Math.abs(yPoints[k]);
                        }
                        else {
                            slope = y2t/x2t;
                            d2 += Math.abs((slope * xPoints[k] - yPoints[k])/Math.sqrt(slope*slope + 1));
                        }
                    }
                    if (d1 < d2) {
                        tangentX = (float)x1t;
                        tangentY = (float)y1t;
                    }
                    else {
                        tangentX = (float)x2t;
                        tangentY = (float)y2t;
                    }
                    if (tangentX == 0.0f) {
                        omegaArray[indexPtr++] = (short)0;
                        
                    }
                    else {
                        omegaArray[indexPtr++] = (short)((Math.atan(tangentY/tangentX) + Math.PI/2.0)/omegaBins);
                    }    
                }
            } // for (n = 2; n <= openLength[i] - 2; n++)
        } // for (i = 0; i < numOpenCurves; i++)
        openStart = null;
        openLength = null;
        xArray = null;
        yArray = null;
        
        // Find a position and length of closed curve
        numClosedCurves = 0;
        xPoints = new float[2*sidePointsForTangent + 1];
        yPoints = new float[2*sidePointsForTangent + 1];
        for (y = 0; y < yDim; y++) {
            offset = y * xDim;
            for (x = 0; x < xDim; x++) {
                index = offset + x;
                if (!foundArray[index]) {
                    startPtr = indexPtr;
                    foundArray[index] = true;
                    numClosedCurves++;
                    closedLength = 1;
                    indexArray[indexPtr++] = index;
                    while ((!foundArray[neighbor1[index]]) || (!foundArray[neighbor2[index]])) {
                        if (!foundArray[neighbor1[index]]) {
                            index = neighbor1[index];
                        }
                        else {
                            index = neighbor2[index];
                        }
                        foundArray[index] = true;
                        closedLength++;
                        indexArray[indexPtr++] = index;
                    } // while ((!foundArray[neighbor1[index]]) || (!foundArray[neighbor2[index]]))
                    endPtr = indexPtr - 1;
                    indexPtr = startPtr;
                    for (n = 0; n <= closedLength - 1; n++) {
                        // Put the tangent point at index sidePointsForTangent in the
                        // center of the xPoints and yPoints array with sidePointsForTangent points
                        // to each side.
                        startWrapPoints = Math.max(0, sidePointsForTangent - n);
                        endWrapPoints =  Math.max(0, sidePointsForTangent - (closedLength - 1 - n));
                        for (k = 0; k < startWrapPoints; k++) {
                            xPoints[k] = indexArray[endPtr - (startWrapPoints - k)] % xDim;
                            yPoints[k] = indexArray[endPtr - (startWrapPoints - k)] / xDim;
                        }
                        for (k = startWrapPoints, j = indexPtr - sidePointsForTangent + startWrapPoints;
                             k < 2*sidePointsForTangent + 1 - endWrapPoints; j++, k++) {
                            xPoints[k] = indexArray[j] % xDim;
                            yPoints[k] = indexArray[j] / xDim;
                        }
                        for (j = 0, k = 2*sidePointsForTangent + 1 - endWrapPoints; k < 2*sidePointsForTangent + 1; j++, k++) {
                            xPoints[k] = indexArray[startPtr + j] % xDim;
                            yPoints[k] = indexArray[startPtr + j] / xDim;
                        }
                        // For the closed curve find the slope and y axis intercept of the tangent line to the curve at a point
                        // With a user specified sidePointsForTangent on each side of a point find the tangent line that
                        // minimizes the sum of the squared distances from these side points to the tangent line 
                        if (sidePointsForTangent == 1) {
                            tangentX = (xPoints[2] - xPoints[0])/2.0f;
                            tangentY = (yPoints[2] - yPoints[0])/2.0f;
                            if (tangentX == 0.0f) {
                                omegaArray[indexPtr++] = (short)0;
                            }
                            else {
                                omegaArray[indexPtr++] = (short)((Math.atan(tangentY/tangentX) + Math.PI/2.0)/omegaBins);
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
                                xSqSum += xPoints[k]*xPoints[k];
                                ySqSum += yPoints[k]*yPoints[k];
                                xySum += xPoints[k]*yPoints[k];
                            }
                            if (xySum != 0.0) {
                                var = Math.sqrt(ySqSum*ySqSum - 2.0 * xSqSum * ySqSum + xSqSum * xSqSum + 4.0 * xySum * xySum);
                                x1t = 0.5 * ((-ySqSum + xSqSum + var)/xySum);
                                x2t = 0.5 * ((-ySqSum + xSqSum - var)/xySum);
                                y1t = 1.0;
                                y2t = 1.0;
                            }
                            else {
                                // If all points are symmetric to either this new x axis or this new y axis, then
                                // their product sum is 0 and the tangentX, tangentY must be 1,0 or 0,1
                                x1t = 1.0;
                                x2t = 0.0;
                                y1t = 0.0;
                                y2t = 1.0;
                            }
                            // x1t, y1t and x2t, y2t are perpindicular.  To find the solution, calculate the sum of
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
                                }
                                else if (y1t == 0.0) {
                                    // Zero slope thru (0, 0)
                                    d1 += Math.abs(yPoints[k]);
                                }
                                else {
                                    slope = y1t/x1t;
                                    d1 += Math.abs((slope * xPoints[k] - yPoints[k])/Math.sqrt(slope*slope + 1));
                                }
                            }
                            d2 = 0.0;
                            for (k = 0; k < xPoints.length; k++) {
                                if (x2t == 0.0) {
                                    // Infinite slope thru (0,0)
                                    d2 += Math.abs(xPoints[k]);
                                }
                                else if (y2t == 0.0) {
                                    // Zero slope thru (0, 0)
                                    d2 += Math.abs(yPoints[k]);
                                }
                                else {
                                    slope = y2t/x2t;
                                    d2 += Math.abs((slope * xPoints[k] - yPoints[k])/Math.sqrt(slope*slope + 1));
                                }
                            }
                            if (d1 < d2) {
                                tangentX = (float)x1t;
                                tangentY = (float)y1t;
                            }
                            else {
                                tangentX = (float)x2t;
                                tangentY = (float)y2t;
                            }
                            if (tangentX == 0.0f) {
                                omegaArray[indexPtr++] = (short)0;
                                
                            }
                            else {
                                omegaArray[indexPtr++] = (short)((Math.atan(tangentY/tangentX) + Math.PI/2.0)/omegaBins);
                            }    
                        }
                    } // for (n = 0; n <= closedLength[i] - 1; n++)
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
        minTheta = (float)(-Math.PI/2.0);
        bytesPerCell = 2 * 4 + 2; // xCenter float, yCenter float, and short count
        xCenterBins = (int)Math.ceil((maxXCenter - minXCenter)/maxPixelBinWidth);
        yCenterBins = (int)Math.ceil((maxYCenter - minYCenter)/maxPixelBinWidth);
        thetaBins = 1;
        maxRadiansDiff = maxDegreesBinWidth * Math.PI/180.0;
        if (allowRotation) {
            thetaBins = (int)Math.ceil((2.0 * Math.PI)/maxRadiansDiff);
        }
        if (!allowScaling) {
            scaleBins = 1;  
        }
        longNumBins = (long)xCenterBins * (long)yCenterBins * (long)thetaBins * (long)scaleBins;
        numBins = (int)longNumBins;
        desiredBytes = longNumBins * (long)bytesPerCell;
        actualBytesAvailable = (long)maxBufferSize * 1024L * 1024L;
        if (actualBytesAvailable < desiredBytes) {
            // Must shrink the number of each bins used for each parameter by the fourth, third, or second root of
            // deisredBytes/actualBytesAvailable to keep the buffer size down to actualBytesAvailable
            if (allowRotation && allowScaling) {
                shrinkFactor = Math.pow((double)desiredBytes/(double)actualBytesAvailable, 0.25);
            }
            else if (allowRotation || allowScaling) {
                shrinkFactor = Math.pow((double)desiredBytes/(double)actualBytesAvailable, 1.0/3.0);    
            }
            else {
                shrinkFactor = Math.pow((double)desiredBytes/(double)actualBytesAvailable, 0.5);
            }
            maxPixelBinWidth = (float)(maxPixelBinWidth * shrinkFactor);
            xCenterBins = (int)Math.ceil((maxXCenter - minXCenter)/maxPixelBinWidth);
            yCenterBins = (int)Math.ceil((maxYCenter - minYCenter)/maxPixelBinWidth);
            if (allowRotation) {
                maxRadiansDiff = maxRadiansDiff * shrinkFactor;
                thetaBins = (int)Math.ceil((2.0 * Math.PI)/maxRadiansDiff);
                thetaArray = new float[thetaBins];
                for (i = 0; i < thetaBins; i++) {
                    thetaArray[i] = (float)(i * 2.0 * Math.PI)/thetaBins;
                }
            }
            if (allowScaling) {
                scaleBins = (int)Math.ceil(scaleBins/shrinkFactor);
                maxToMinRatio = maxScaleFactor/minScaleFactor;
                scaleArray = new float[scaleBins];
                scaleArray[0] = minScaleFactor;
                scaleArray[scaleBins - 1] = maxScaleFactor;
                for (i = 1; i < scaleBins - 1; i++) {
                    scaleArray[i] = (float)(minScaleFactor * Math.pow(maxToMinRatio, (double)i/(double)(scaleBins - 1)));
                }
            }
            numBins = xCenterBins * yCenterBins * thetaBins * scaleBins;
        } // if (actualBytesAvailable < desiredBytes)
        xyBins = xCenterBins * yCenterBins;
        ViewUserInterface.getReference().setDataText("xCenterBins = " + xCenterBins + "\n");
        ViewUserInterface.getReference().setDataText("yCenterBins = " + yCenterBins + "\n");
        ViewUserInterface.getReference().setDataText("thetaBins = " + thetaBins + "\n");
        ViewUserInterface.getReference().setDataText("scaleBins = " + scaleBins + "\n");
        ViewUserInterface.getReference().setDataText("numBins = " + numBins + "\n");
        xCenterArray = new float[xyBins][thetaBins][scaleBins];
        yCenterArray = new float[xyBins][thetaBins][scaleBins];
        countArray = new int[xyBins][thetaBins][scaleBins];
        foundPoint = new boolean[xyBins];
        xCenterTable = new float[objectsToFind];
        yCenterTable = new float[objectsToFind];
        if (allowRotation) {
            thetaTable = new float[objectsToFind];
        }
        if (allowScaling) {
            scaleTable = new float[objectsToFind];
        }
        countTable = new short[objectsToFind];
        
        minDistanceSquared = minDistance * minDistance;
        while (objectsFound < objectsToFind) {
            for (i = 0; i < numPoints; i++) {
                omegaIndex = omegaArray[i];
                x = indexArray[i] % xDim;
                y = indexArray[i] / xDim;
                rBetaLength = omegaRBetaList[omegaIndex].size();
                for (j = 0; j < rBetaLength; j++) {
                    floatArray = (float[])omegaRBetaList[omegaIndex].get(j);
                    r = floatArray[0];
                    beta = floatArray[1];
                    for (k = 0; k < thetaBins; k++) {
                        if (allowRotation) {
                            beta = floatArray[1] + thetaArray[k];
                        }
                        for (m = 0; m < scaleBins; m++) {
                            if (allowScaling) {
                                r = scaleArray[m] * floatArray[0];
                            }
                            xCenter = x + r * Math.cos(beta);
                            yCenter = y + r * Math.sin(beta);
                            xCenterIndex = (int)((xCenter - minXCenter)/maxPixelBinWidth);
                            if ((xCenterIndex >= 0) && (xCenterIndex < xCenterBins)) {
                                yCenterIndex = (int)((yCenter - minYCenter)/maxPixelBinWidth);
                                if ((yCenterIndex >= 0) && (yCenterIndex < yCenterBins)) {
                                    index = xCenterIndex + xCenterBins * yCenterIndex;
                                    xCenterArray[index][k][m] += xCenter;
                                    yCenterArray[index][k][m] += yCenter;
                                    countArray[index][k][m]++;
                                } // if (yCenterIndex >= 0) && (yCenterIndex < yCenterBins))
                            }  // if (xCenterIndex >= 0) && (xCenterIndex < xCenterBins))
                        } // for (m = 0; m < scaleBins; m+++)
                    } // for (k = 0; k < thetaBins; k++)
                } // for (j = 0; j < rBetaLength; j++)
            } // for (i = 0; i < numPoints; i++)
            
            maxIndex = -1;
            maxCount = 0;
            for (i = 0; i < xyBins; i++) {
                for (k = 0; k < thetaBins; k++) {
                    for (m = 0; m < scaleBins; m++) {
                        if (countArray[i][k][m] > maxCount) {
                            maxXYIndex = i;
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
                
            xCenter = xCenterArray[maxXYIndex][maxThetaIndex][maxScaleIndex]/maxCount;
            yCenter = yCenterArray[maxXYIndex][maxThetaIndex][maxScaleIndex]/maxCount;
            xCenterTable[objectsFound] = (float)xCenter;
            yCenterTable[objectsFound] = (float)yCenter;
            if (allowRotation) {
                thetaTable[objectsFound] = thetaArray[maxThetaIndex];
            }
            if (allowScaling) {
                scaleTable[objectsFound] = scaleArray[maxScaleIndex];
            }
            countTable[objectsFound] = (short)maxCount;
            objectsFound++;
            ViewUserInterface.getReference().setDataText("Object # " + objectsFound + " found\n");
            ViewUserInterface.getReference().setDataText(" x center = " + xCenter + "\n");
            ViewUserInterface.getReference().setDataText(" y center = " + yCenter + "\n");
            if (allowRotation) {
                ViewUserInterface.getReference().setDataText(" theta = " + (thetaTable[objectsFound-1] * 180/Math.PI) + "\n");
            }
            if (allowScaling) {
                ViewUserInterface.getReference().setDataText(" scale factor = " + scaleTable[objectsFound-1] + "\n");
            }
                    
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
                if (foundPoint[i]) {
                    pointsDeleted++;
                }
            } // for (i = 0; i < numPoints; i++)
            if (pointsDeleted > 0) {
                newNumPoints = numPoints - pointsDeleted;
                if (newNumPoints == 0) {
                    break;
                }
                newIndexArray = new int[newNumPoints];
                newOmegaArray = new short[newNumPoints];
                newInterceptArray = new float[newNumPoints];
                for (i = 0, j = 0; i < numPoints; i++) {
                    if (!foundPoint[i]) {
                        newIndexArray[j] = indexArray[i];
                        newOmegaArray[j] = omegaArray[i];
                        newInterceptArray[j] = interceptArray[i];
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
                interceptArray = null;
                interceptArray = new float[numPoints];
                for (i = 0; i < numPoints; i++) {
                    indexArray[i] = newIndexArray[i];
                    omegaArray[i] = newOmegaArray[i];
                    interceptArray[i] = newInterceptArray[i];
                }
                newIndexArray = null;
                newOmegaArray = null;
                newInterceptArray = null;
            } // if (pointsDeleted > 0)
        } // while (objectsFound < objectsToFind)
        xCenterArray = null;
        yCenterArray = null;
        thetaArray = null;
        foundPoint = null;
        indexArray = null;
        omegaArray = null;
        interceptArray = null;
        testedArray = null;
        
        
        
        // Restore original source values
        if (!test) {
            try {
                srcImage.exportData(0, sourceSlice, srcBuffer);
            } catch (IOException e) {
                MipavUtil.displayError("IOException " + e + " on srcImage.exportData");
    
                setCompleted(false);
    
                return;
            }
        } // if (!test)
        
        // Create a dialog with numEllipsesFound xCenterTable[i], yCenterTable[i], r1Table[i],
        // r2Table[i], thetaTable[i], and countTable[i] values, where the user will select a check box
        // to have the selected ellipse drawn.
        selectedEllipse = new boolean[ellipsesFound];
        
        //choice = new JDialogHoughEllipseChoice(ViewUserInterface.getReference().getMainFrame(), xCenterTable,
                 //xDim, yCenterTable, yDim, r1Table, minR1, maxR1, r2Table, minR2, maxR2, thetaTable,
                 //countTable, selectedEllipse);
        
        //if (!choice.okayPressed() ) {
            //setCompleted(false);
            //return;
        //}
        
        // Draw selected elipses
        /*for (i = 0; i < ellipsesFound; i++) {
            if (selectedEllipse[i]) {
                beta = thetaTable[i];
                cosbeta = Math.cos(beta);
                sinbeta = Math.sin(beta);
                for (j = 0; j < 720; j++) {
                    alpha = j * Math.PI/360.0;
                    cosalpha = Math.cos(alpha);
                    sinalpha = Math.sin(alpha);
                    x = (int)(xCenterTable[i] + r1Table[i] * cosalpha * cosbeta - r2Table[i] * sinalpha * sinbeta);
                    if ((x >= 0) && (x < xDim)) {
                        y = (int)(yCenterTable[i] + r1Table[i] * cosalpha * sinbeta + r2Table[i] * sinalpha * cosbeta);
                        if ((y >= 0) && (y < yDim)) {
                            index = x + (xDim*y);
                            srcBuffer[index] = value;
                        }
                    }
                }
            } // if (selectedEllipse[i])
        } // for (i = 0; i < ellipsesFound; i++)*/
        
        try {
            destImage.importData(0, srcBuffer, true);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on destImage.importData");

            setCompleted(false);

            return;
        }
        
        setCompleted(true);
        return;
    }
    
    
    
    
    
    /**
     * Constructs a string of the contruction parameters and out puts the string to the messsage frame if the history
     * logging procedure is turned on.
     */
    private void constructLog() {
        String historyString;

        historyString = new String("FindRtableObject(" + String.valueOf(omegaBins) + ", " +
                                   String.valueOf(sidePointsForTangent) + ", " +
                                   String.valueOf(maxPixelBinWidth) + ", " +
                                   String.valueOf(maxBufferSize) + ", " +
                                   String.valueOf(allowRotation) + ", " +
                                   String.valueOf(maxDegreesBinWidth) + ", " +
                                   String.valueOf(allowScaling) + ", " + 
                                   String.valueOf(minScaleFactor) + ", " + 
                                   String.valueOf(maxScaleFactor) + ", " + 
                                   String.valueOf(scaleBins) + ", " +
                                   String.valueOf(objectsToFind) + 
                                   String.valueOf(minDistance) + ")\n");
    }
}
