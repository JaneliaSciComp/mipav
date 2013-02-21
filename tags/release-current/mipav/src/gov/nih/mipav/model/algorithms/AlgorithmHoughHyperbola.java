package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.JamaMatrix;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;
import java.awt.Color;

/**
 *  This work is made possible by the following mathematical theorem proved by Professor Alan Horwitz
 *  of Penn State University:
 *  
    Finding the Center of a Hyperbola, H, given three nonparallel tangent lines to H and
    the corresponding points of tangency.
    Theorem: Let P1,P2,P3 be any three distinct points on the hyperbola, H; Let L1,L2,L3 be the
    tangent lines to H at P1,P2,P3, and assume that no two of the Lj are parallel. Let t12 = Intersection
    Point of L1 & L2, t23 = Intersection Point of L2 & L3,
    m12 = midpoint of P1P2, m23 = midpoint of P2P3, J1 = line thru t12 & m12, J2 = line thru t23 & m23
    Then the Intersection Point of J1 & J2 is the center of H

 *  This Hough transform uses (xi, yi) points in the original image space to generate p, q, r1, r2, and theta points in the Hough
 *  transform.  This Hough transform module only works with binary images.   Before it is used the user must 
 *  compute the gradient of an image and threshold it to obtain a binary image.  Noise removal and thinning should also
 *  be performed, if necessary, before this program is run.
 *  
 *  The code for finding the best tangent for a point on a curve uses a port of findOptimalTangent.m from the Randomized Hough
 *  transform used for ellipse detection MATLAB code by Andrew Schuler.  The rest of the code is not ported, but is derived
 *  from the Hough ellipse writeups by Andrew Schuler and Robert A. Mclaughlin.  Robert A. Mclaughlin uses a linked list to save memory at
 *  the cost of slower speed.  Andrew Schuler uses an array to achieve higher speed at the cost of more memory usage.  In
 *  this application I use the array for higher speed and incur a higher memory cost.  If need be, memory can be saved by
 *  rewriting this application to used a linked list.
 *  
 *  Pixel neighbors:
 *  0 1 2
 *  3   4
 *  5 6 7
 *  
 *  The algorithm works as follows:
 *  1.) Skeletonization is performed.  Branches with 2 or less pixels are pruned off.
 *  2.) When a pixel has a diagonal neighbor adjacent to a horizontal or vertical neighbor, 
 *      the horizontal or vertical neighbor is deleted.
 *  3.) Remove points with 2 or more neighbors.
 *  4.) Find the 1 or 2 neighbors of every point
 *      Find the number of end points, that is, points with only 1 neighbor
 *      Delete isolated points with no neighbors
 *  5.) Find the starting positions and lengths of the open curves
 *      Delete all open curves with only 2 points
 *  6.) For the open curves find the slope and y axis intercept of the tangent line to the curve at a point
 *      With a user specified sidePointsForTangent on each side of a point find the tangent line that
 *      minimizes the sum of the squared distances from these side points to the tangent line
 *  7.) Find a position and length of closed curve 
 *  8.) For the closed curve find the slope and y axis intercept of the tangent line to the curve at a point
 *      With a user specified sidePointsForTangent on each side of a point find the tangent line that
 *      minimizes the sum of the squared distances from these side points to the tangent line 
 *  9.) Calculate the desired number of bins that would be used for each parameter if memory were not a
 *      limitation.  This desired bin number = (maximum parameter value - minimum parameter value)/maximum bin width
 *  10.) Shrink the number of each bins used for each parameter by the fifth root of desiredBytes/actualBytesAvailable
         to keep the buffer size down to user specified actualBytesAvailable.
 *  11.) User a random number generator to select 3 of the points.  If 2 of the 3 points are closer than
 *       the user specified minimum distance or farther than the user specified maximum distance, then
 *       run the random number generator again to find another point within the permissible distance
 *       range.  If 2 of the points have tangents with the same slope, then run the random number
 *       generator again.
 *  12.) Find the intersection of the tangent line for point 1 and the tangent line for point2, t12.
 *       Find the point midway between point 1 and point 2, point m12.
 *       Find the intersection of the tangent line for point 2 and the tangent line for point3, t23.
 *       Find the point midway between point 2 and point 3, point m23.
 *       The center is found at the intersection of t12m12 and t23m23.  If the center is not inside
 *       the bounds of the image, go back and run the random number generator for 3 new points.
 *  13.) Translate the center of the hyperbola to the origin by subtracting xCenter, yCenter from 
 *       point 1, point 2, and point 3.
 *  14.) Solve for the other 3 hyberbola parameters a, b, and c in a*x**2 + 2*b*x*y + c*y**2 = 1 by solving:
 *       [x1**2  2*x1*y1  y1**2] [a]   [1]
 *       [x2**2  2*x2*y2  y2**2] [b] = [1]
 *       [x3**2  2*x3*y3  y3**2] [c]   [1]
 *  15.) The inequality a*c - b**2 < 0 is true if the parameters represent a valid hyperbola.
 *       If this inequality is false, it implies that either the 3 pixels do not lie on the same hyperbola,
 *       or that the estimates of the tangents were inaccurate.  In either case, we discard the 
 *       parameters and run the random number generator to find a new pixel triplet.
 *  16.) Convert a, b, and c to semi major axis r1, semi minor axis r2, and orientation angle theta.
 *       Find the combined index value for these 5 parameters from the 5 individual index values and the bin
 *       numbers for the 5 parameters.  Add the respective parameters to xCenterArray[index],
 *       yCenterArray[index], r1Array[index], r2Array[index], and thetaArray[index].
 *       Increment countArray[index].
 *  17.) Keep collecting point triplets until the user specified pointSetsRequired have been collected.
 *  18.) Find the maxIndex yielding the largest value of countArray[index].  If countArray[index] > countThreshold,
 *       then proceed with step 19.  Otherwise zero the Hough buffer and start to collect a new set of
 *       pointSetsRequired point triplets.
 *  19.) Divide xCenterArray[maxIndex], yCenterArray[maxIndex], r1Array[maxIndex], r2Array[maxIndex],
 *       and thetaArray[maxIndex] by countArray[maxIndex].  For xCenter, yCenter, r1, r2, and theta,
 *       find the number of pixels near the perimiter of the hyperbola.  For xCenter, yCenter, r1, r2,
 *       and theta - 90 degrees, find the number of pixels near the perimiter of the hyperbola.  The
 *       correct one of these 2 cases has the largest number of pixels near the perimiter of the
 *       hyperbola. If the correct case finds more pixels than minPixels, then the hyberbola is considered as valid.
 *       If the correct case has fewer than minPixels, then this set of parameters is discarded and we return to step 18.
 *  20.) If an hyperbola has been found, but the algorithm still has not found the user specified numHyperbolas
 *       number of hyperbolas, then delete the points found along its perimiter from indexArray, slopeArray,
 *       and interceptArray before running the Hough transform again.  xCenterArray, yCenterArray,
 *       r1Array, r2Array, thetaArray, and countArray are zeroed before the next Hough transform
 *       is acquired.
 *  21.) Perform no more than maxHyperbolaFindCycles of pointSetsRequired triplet set acquisitions.  When
 *       this number of triplet set acquisitions has occurred, stop trying to find new hyperbolas.
 *  22.) Create a dialog with numHyperbolasFound xCenterArray[i], yCenterArray[i], r1Array[i], r2Array[i],
 *       thetaArray[i], and countArray[i] values, where the user will select a check box to have that
 *       hyperbola drawn.
 *  
 *  References:
 *  1.) Private communication from Professor Alan Horwitz of Penn State University
 *  
 *  2.) The Randomized Hough Transform used for ellipse detection by Andrew Schuler
 *  
 *  3.) Technical Report - Randomized Hough Transform: Improved Ellipse Detection with Comparison by
 *  Robert A. Mclaughlin
 *  
 *  4.) Digital Image Processing, Second Edition by Richard C. Gonzalez and Richard E. Woods, Section 10.2.2
 *  Global Processing via the Hough Transform, Prentice-Hall, Inc., 2002, pp. 587-591.
 *  
 *  5.) Shape Detection in Computer Vision Using the Hough Transform by V. F. Leavers, Springer-Verlag, 1992.
 * 
 */
public class AlgorithmHoughHyperbola extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------
    // Minimum number of points on a hyperbola for it to be valid
	private int minPixels;
	
    // Maximum number of points to take from each side of a point on a curve in determining a tangent
    // If only 1 point is available on each side, simply use avarage of slopes to each of the
    // neigboring points.  
    private int sidePointsForTangent;
    
    // xCenter, yCenter, r1, and r2 must have bin width <= maxPixelBinWidth
    private double maxPixelBinWidth;
    
    // theta must have bin width <= maxDegreesBinWidth
    private double maxDegreesBinWidth;
    
    // Smallest allowable distance between 2 of 3 picked points
    private double minPointDistance;
    
    // Largest allowable distance between 2 of 3 picked points
    private double maxPointDistance;
    
    // Number of point triplets required before each hyperbola find is performed
    private int pointSetsRequired;
    
    // number of hyperbolas to be found
    private int numHyperbolas;
    
    // Maximum number of pointSetsRequired triplet point acqusitions that is allowed to occur
    private int maxHyperbolaFindCycles = 80;
    
    // The maximum Hough transform size in megabytes - default is currently 256
    private int maxBufferSize;
    
    // Number of counts required to find a hyperbola
    private int countThreshold;
    
    // Maximum percent by which perimiter pixels can deviate from the hyperbola equation
    private double hyperbolaRangeTolerance;
    
    private ModelImage testImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmHoughHyperbola - default constructor.
     */
    public AlgorithmHoughHyperbola() { }

    /**
     * AlgorithmHoughHyperbola.
     *
     * @param  destImg  Image with hyperbolas filled in
     * @param  srcImg   Binary source image that has hyperbolas and near hyperbola shapes with gaps
     * @param  minPixels Minimum number of points on a found hyperbola for it to be valid.
     * @param  sidePointsForTangent  Maximum number of points to take from each side of a point on a curve
     *                               in determining the tangent
     * @param  maxPixelBinWidth  Maximum pixel difference allowed for xCenter, yCenter, r1, and r2 for 5 values
     *                           to be placed into an existing bin
     * @param  maxDegreesBinWidth Maximum degrees difference allowed for theta for 5 values to be placed into
     *                            an existing bin
     * @param  minPointDistance Smallest allowable distance between 2 of 3 picked points
     * @param  maxPointDistance Largest allowable distance between 2 of 3 picked points
     * @param  pointSetsRequired Number of point triplets acquired before each hyperbola find is performed
     * @param  countThreshold Number of counts required to find a hyperbola
     * @param  hyperbolaRangeTolerance Maximum percent by which perimiter pixels can deviate from the
     *                                hyperbola equation
     * @param  numHyperbolas number of hyperbolas to be found
     * @param  maxHyperbolaFindCycles Maximum number of pointSetsRequired triplet point acquisitions that
     *                              is allowed to occur
     * @param  maxBufferSize maximum Hough transform size in megabytes
     */
    public AlgorithmHoughHyperbola(ModelImage destImg, ModelImage srcImg, int minPixels, int sidePointsForTangent,
                                 double maxPixelBinWidth, double maxDegreesBinWidth, double minPointDistance,
                                 double maxPointDistance, int pointSetsRequired, int countThreshold, 
                                 double hyperbolaRangeTolerance, int numHyperbolas, 
                                 int maxHyperbolaFindCycles, int maxBufferSize) {
        super(destImg, srcImg);
        this.minPixels = minPixels;
        this.sidePointsForTangent = sidePointsForTangent;
        this.maxPixelBinWidth = maxPixelBinWidth;
        this.maxDegreesBinWidth = maxDegreesBinWidth;
        this.minPointDistance = minPointDistance;
        this.maxPointDistance = maxPointDistance;
        this.pointSetsRequired = pointSetsRequired;
        this.countThreshold = countThreshold;
        this.hyperbolaRangeTolerance = hyperbolaRangeTolerance;
        this.numHyperbolas = numHyperbolas;
        this.maxHyperbolaFindCycles = maxHyperbolaFindCycles;
        this.maxBufferSize = maxBufferSize;
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

        int i, j, k, n;
        int index;
        double theta;
        double theta1;
        double theta2;
        byte[] srcBuffer;
        boolean test = false;
        double xCenter;
        double yCenter;
        float xCenterArray[];
        float yCenterArray[];
        float r1Array[];
        float r2Array[];
        float thetaArray[];
        short countArray[];
        boolean selectedHyperbola[];
        JDialogHoughHyperbolaChoice choice;
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
        float slopeArray[];
        float newSlopeArray[];
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
        double secalpha;
        double tanalpha;
        double angle;
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
        int hyperbolasFound = 0;
        int hyperbolaFindCycles = 0;
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
        JamaMatrix A;
        JamaMatrix B;
        JamaMatrix X = null;
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
        byte foundPoint[];
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
        double upperLimit = 1.0 + 0.01 * hyperbolaRangeTolerance;
        double lowerLimit = 1.0 - 0.01 * hyperbolaRangeTolerance;
        short pointsOnHyperbola;
        short pointsOnHyperbola1;
        short pointsOnHyperbola2;
        float xCenterTable[];
        float yCenterTable[];
        float r1Table[];
        float r2Table[];
        float thetaTable[];
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
        boolean hyperbolaDetected;
        VOI hyperbolaVOI[] = null;
        int hyperbolasDrawn = 0;
        int hyperbolaPoints[];
        float xArr[];
        float yArr[];
        float zArr[];

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        

        fireProgressStateChanged(srcImage.getImageName(), "Hough hyperbola ...");

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
            for (i = -179; i <= 179; i++) {
                alpha = i * Math.PI/360.0;
                secalpha = 1.0/Math.cos(alpha);
                tanalpha = Math.tan(alpha);
                x = (int)(xCenter + a1 * secalpha);
                if ((x >= 0) && (x <= xDim - 1)) {
                    y = (int)(yCenter + b1 * tanalpha);
                    if ((y >= 0) && (y <= yDim - 1)) {
                        index = x + (xDim*y);
                        srcBuffer[index] = 1; 
                    }
                }
            }
            
            a1 = 75.0;
            b1 = 37.5;
            angle = Math.PI/4.0;
            beta = -angle;
            cosbeta = Math.cos(beta);
            sinbeta = Math.sin(beta);
            for (i = -359; i <= 359; i++) {
                alpha = i * Math.PI/720.0;
                secalpha = 1.0/Math.cos(alpha);
                tanalpha = Math.tan(alpha);
                x = (int)(xCenter + 50.0 + a1 * secalpha * cosbeta - b1 * tanalpha * sinbeta);
                if ((x >= 0) && (x <= xDim - 1)) {
                    y = (int)(yCenter + 50.0 + a1 * secalpha * sinbeta + b1 * tanalpha * cosbeta);
                    if ((y >= 0) && (y <= yDim - 1)) {
                        index = x + (xDim*y);
                        srcBuffer[index] = 1;
                    }
                }
            }
            /*for (i = -179; i <= -135; i++) {
                alpha = i * Math.PI/360.0;
                secalpha = 1.0/Math.cos(alpha);
                tanalpha = Math.tan(alpha);
                x = (int)(xCenter + 50.0 + a1 * secalpha * cosbeta - b1 * tanalpha * sinbeta);
                if ((x >= 0) && (x <= xDim - 1)) {
                    y = (int)(yCenter + 50.0 + a1 * secalpha * sinbeta + b1 * tanalpha * cosbeta);
                    if ((y >= 0) && (y <= yDim - 1)) {
                        index = x + (xDim*y);
                        srcBuffer[index] = 1;
                    }
                } 
            }
            
            for (i = -90; i <= -45; i++) {
                alpha = i * Math.PI/360.0;
                secalpha = 1.0/Math.cos(alpha);
                tanalpha = Math.tan(alpha);
                x = (int)(xCenter + 50.0 + a1 * secalpha * cosbeta - b1 * tanalpha * sinbeta);
                if ((x >= 0) && (x <= xDim - 1)) {
                    y = (int)(yCenter + 50.0 + a1 * secalpha * sinbeta + b1 * tanalpha * cosbeta);
                    if ((y >= 0) && (y <= yDim - 1)) {
                        index = x + (xDim*y);
                        srcBuffer[index] = 1; 
                    }
                }        
            }
            
            for (i = 0; i <= 45; i++) {
                alpha = i * Math.PI/360.0;
                secalpha = 1.0/Math.cos(alpha);
                tanalpha = Math.tan(alpha);
                x = (int)(xCenter + 50.0 + a1 * secalpha * cosbeta - b1 * tanalpha * sinbeta);
                if ((x >= 0) && (x <= xDim - 1)) {
                    y = (int)(yCenter + 50.0 + a1 * secalpha * sinbeta + b1 * tanalpha * cosbeta);
                    if ((y >= 0) && (y <= yDim - 1)) {
                        index = x + (xDim*y);
                        srcBuffer[index] = 1; 
                    }
                }
            }
            
            for (i = 90; i <= 135; i++) {
                alpha = i * Math.PI/360.0;
                secalpha = 1.0/Math.cos(alpha);
                tanalpha = Math.tan(alpha);
                x = (int)(xCenter + 50.0 + a1 * secalpha * cosbeta - b1 * tanalpha * sinbeta);
                if ((x >= 0) && (x <= xDim - 1)) {
                    y = (int)(yCenter + 50.0 + a1 * secalpha * sinbeta + b1 * tanalpha * cosbeta);
                    if ((y >= 0) && ( y <= yDim - 1)) {
                        index = x + (xDim*y);
                        srcBuffer[index] = 1;
                    }
                } 
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
            for (i = -179; i <= -135; i++) {
                alpha = i * Math.PI/360.0;
                secalpha = 1.0/Math.cos(alpha);
                tanalpha = Math.tan(alpha);
                x = (int)(xCenter -50.0 + a1 * secalpha * cosbeta - b1 * tanalpha * sinbeta);
                if ((x >= 0) && (x <= xDim - 1)) {
                    y = (int)(yCenter -50.0 + a1 * secalpha * sinbeta + b1 * tanalpha * cosbeta);
                    if ((y >= 0) && (y <= yDim - 1)) {
                        index = x + (xDim*y);
                        srcBuffer[index] = 1; 
                    }
                }
            }
            
            for (i = -90; i <= -45; i++) {
                alpha = i * Math.PI/360.0;
                secalpha = 1.0/Math.cos(alpha);
                tanalpha = Math.tan(alpha);
                x = (int)(xCenter -50.0 + a1 * secalpha * cosbeta - b1 * tanalpha * sinbeta);
                if ((x >= 0) && (x <= xDim - 1)) {
                    y = (int)(yCenter -50.0 + a1 * secalpha * sinbeta + b1 * tanalpha * cosbeta);
                    if ((y >= 0) && (y <= yDim - 1)) {
                        index = x + (xDim*y);
                        srcBuffer[index] = 1; 
                    }
                }
            }
            
            for (i = 0; i <= 45; i++) {
                alpha = i * Math.PI/360.0;
                secalpha = 1.0/Math.cos(alpha);
                tanalpha = Math.tan(alpha);
                x = (int)(xCenter -50.0 + a1 * secalpha * cosbeta - b1 * tanalpha * sinbeta);
                if ((x >= 0) && (x <= xDim - 1)) {
                    y = (int)(yCenter -50.0 + a1 * secalpha * sinbeta + b1 * tanalpha * cosbeta);
                    if ((y >= 0) && (y <= yDim - 1)) {
                        index = x + (xDim*y);
                        srcBuffer[index] = 1;
                    }
                }
            }
            
            for (i = 90; i <= 135; i++) {
                alpha = i * Math.PI/360.0;
                secalpha = 1.0/Math.cos(alpha);
                tanalpha = Math.tan(alpha);
                x = (int)(xCenter -50.0 + a1 * secalpha * cosbeta - b1 * tanalpha * sinbeta);
                if ((x >= 0) && (x <= xDim - 1)) {
                    y = (int)(yCenter -50.0 + a1 * secalpha * sinbeta + b1 * tanalpha * cosbeta);
                    if ((y >= 0) && (y <= yDim - 1)) {
                        index = x + (xDim*y);
                        srcBuffer[index] = 1; 
                    }
                }
            }
   
            testImage = new ModelImage(ModelStorageBase.BYTE, srcImage.getExtents(), "Hough Hyperbola Test Image");
            try {
                testImage.importData(0, srcBuffer, true);
            }
            catch (IOException e) {
                MipavUtil.displayError("IOException " + e + " on testImage.importData");

                setCompleted(false);

                return;    
            }
            new ViewJFrameImage(testImage);
        } // if (test)
        
        try {
            destImage.importData(0, srcBuffer, true);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on destImage.importData");

            setCompleted(false);

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
        slopeArray = new float[numPoints];
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
                        slopeArray[indexPtr++] = Float.POSITIVE_INFINITY;
                    }
                    else {
                        slopeArray[indexPtr] = tangentY/tangentX;
                        interceptArray[indexPtr] = yArray[n] - slopeArray[indexPtr] * xArray[n];
                        indexPtr++;
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
                    xpc = xPoints[presentSidePoints];
                    ypc = yPoints[presentSidePoints];
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
                            d1 += Math.abs(yPoints[k]);
                        }
                        else if (y1t == 0.0) {
                            // Zero slope thru (0, 0)
                            d1 += Math.abs(xPoints[k]);
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
                            d2 += Math.abs(yPoints[k]);
                        }
                        else if (y2t == 0.0) {
                            // Zero slope thru (0, 0)
                            d2 += Math.abs(xPoints[k]);
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
                        slopeArray[indexPtr++] = Float.POSITIVE_INFINITY;
                        
                    }
                    else {
                        slopeArray[indexPtr] = tangentY/tangentX;
                        interceptArray[indexPtr] = yArray[n] - slopeArray[indexPtr] * xArray[n];
                        indexPtr++;
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
                                slopeArray[indexPtr++] = Float.POSITIVE_INFINITY;
                            }
                            else {
                                slopeArray[indexPtr] = tangentY/tangentX;
                                interceptArray[indexPtr] = yPoints[1] - slopeArray[indexPtr] * xPoints[1];
                                indexPtr++;
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
                                    d1 += Math.abs(yPoints[k]);
                                }
                                else if (y1t == 0.0) {
                                    // Zero slope thru (0, 0)
                                    d1 += Math.abs(xPoints[k]);
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
                                    d2 += Math.abs(yPoints[k]);
                                }
                                else if (y2t == 0.0) {
                                    // Zero slope thru (0, 0)
                                    d2 += Math.abs(xPoints[k]);
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
                                slopeArray[indexPtr++] = Float.POSITIVE_INFINITY;
                                
                            }
                            else {
                                slopeArray[indexPtr] = tangentY/tangentX;
                                // Add xpc, ypc back in to return from tangent point (0, 0) origin to original origin
                                interceptArray[indexPtr] = (yPoints[sidePointsForTangent] + ypc)- slopeArray[indexPtr] * (xPoints[sidePointsForTangent] + xpc);
                                indexPtr++;
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
        minR1 = (float)minPointDistance;
        maxR1 = (float)Math.min(maxPointDistance, Math.sqrt((xDim-1)*(xDim-1) + (yDim-1)*(yDim-1)));
        minR2 = (float)minPointDistance;
        maxR2 = (float)Math.min(maxPointDistance, Math.sqrt((xDim-1)*(xDim-1) + (yDim-1)*(yDim-1)));
        minTheta = (float)(-Math.PI/2.0);
        bytesPerCell = 4 * 5 + 2; // float for each of 5 parameters and short for count;
        xCenterBins = (int)Math.ceil((maxXCenter - minXCenter)/maxPixelBinWidth);
        yCenterBins = (int)Math.ceil((maxYCenter - minYCenter)/maxPixelBinWidth);
        r1Bins = (int)Math.ceil((maxR1 - minR1)/maxPixelBinWidth);
        r2Bins = (int)Math.ceil((maxR2 - minR2)/maxPixelBinWidth);
        maxRadiansDiff = maxDegreesBinWidth * Math.PI/180.0;
        thetaBins = (int)Math.ceil(Math.PI/maxRadiansDiff);
        longNumBins = (long)xCenterBins * (long)yCenterBins * (long)r1Bins * (long)r2Bins * (long)thetaBins;
        numBins = (int)longNumBins;
        desiredBytes = longNumBins * (long)bytesPerCell;
        actualBytesAvailable = (long)maxBufferSize * 1024L * 1024L;
        if (actualBytesAvailable < desiredBytes) {
            // Must shrink the number of each bins used for each parameter by the fifth root of
            // deisredBytes/actualBytesAvailable to keep the buffer size down to actualBytesAvailable
            shrinkFactor = Math.pow((double)desiredBytes/(double)actualBytesAvailable, 0.2);
            maxPixelBinWidth = maxPixelBinWidth * shrinkFactor;
            maxRadiansDiff = maxRadiansDiff * shrinkFactor;
            xCenterBins = (int)Math.ceil((maxXCenter - minXCenter)/maxPixelBinWidth);
            yCenterBins = (int)Math.ceil((maxYCenter - minYCenter)/maxPixelBinWidth);
            r1Bins = (int)Math.ceil((maxR1 - minR1)/maxPixelBinWidth);
            r2Bins = (int)Math.ceil((maxR2 - minR2)/maxPixelBinWidth);
            thetaBins = (int)Math.ceil(Math.PI/maxRadiansDiff);
            numBins = xCenterBins * yCenterBins * r1Bins * r2Bins * thetaBins;
        } // if (actualBytesAvailable < desiredBytes)
        ViewUserInterface.getReference().setDataText("xCenterBins = " + xCenterBins + "\n");
        ViewUserInterface.getReference().setDataText("yCenterBins = " + yCenterBins + "\n");
        ViewUserInterface.getReference().setDataText("r1Bins = " + r1Bins + "\n");
        ViewUserInterface.getReference().setDataText("r2Bins = " + r2Bins + "\n");
        ViewUserInterface.getReference().setDataText("thetaBins = " + thetaBins + "\n");
        ViewUserInterface.getReference().setDataText("numBins = " + numBins + "\n");
        xCenterArray = new float[numBins];
        yCenterArray = new float[numBins];
        r1Array = new float[numBins];
        r2Array = new float[numBins];
        thetaArray = new float[numBins];
        countArray = new short[numBins];
        scale1 = xCenterBins;
        scale12 = scale1 * yCenterBins;
        scale123 = scale12 * r1Bins;
        scale1234 = scale123 * r2Bins;
        xCenterTable = new float[numHyperbolas];
        yCenterTable = new float[numHyperbolas];
        r1Table = new float[numHyperbolas];
        r2Table = new float[numHyperbolas];
        thetaTable = new float[numHyperbolas];
        countTable = new short[numHyperbolas];
        
        randomGen = new RandomNumberGen();
        minDistanceSquared = minPointDistance * minPointDistance;
        maxDistanceSquared = maxPointDistance * maxPointDistance;
        A = new JamaMatrix(3,3, 0.0);
        B = new JamaMatrix(3,1, 0.0);
        foundPoint = new byte[numPoints];
        testedArray = new boolean[numBins];
        while (hyperbolaFindCycles < maxHyperbolaFindCycles) {
            pointSetsAcquired = 0;
            while (pointSetsAcquired < pointSetsRequired) {
                // Generate 3 random numbers from 0 to numPoints - 1  
                number1 = randomGen.genUniformRandomNum(0, numPoints - 1);
                x1 = indexArray[number1] % xDim;
                y1 = indexArray[number1] / xDim;
                slope1 = slopeArray[number1];
                while (true) {
                    number2 = randomGen.genUniformRandomNum(0, numPoints - 1);
                    if (number1 == number2) {
                        continue;
                    }
                    x2 = indexArray[number2] % xDim;
                    y2 = indexArray[number2] / xDim;
                    xDiff = x1 - x2;
                    yDiff = y1 - y2;
                    distanceSquared = xDiff * xDiff + yDiff * yDiff;
                    // Don't use if points 1 and 2 are less than a minimum distance or greater than
                    // a maximum distance apart
                    if ((distanceSquared < minDistanceSquared) || (distanceSquared > maxDistanceSquared)) {
                        continue;
                    }
                    // Don't use if tangent lines 1 and 2 are parallel
                    slope2 = slopeArray[number2];
                    if (Float.isInfinite(slope1) && Float.isInfinite(slope2)) {
                        continue;
                    }
                    if (slope1 == slope2) {
                        continue;
                    }
                    break;
                }
                // Find intersection of tangent 1 and tangent2.
                intercept1 = interceptArray[number1];
                intercept2 = interceptArray[number2];
                if (Float.isInfinite(slope1)) {
                    tx12 = x1;
                    ty12 = slope2 * tx12 + intercept2;
                }
                else if (Float.isInfinite(slope2)) {
                    tx12 = x2;
                    ty12 = slope1 * tx12 + intercept1;
                }
                else {
                    tx12 = (intercept2 - intercept1)/(slope1 - slope2);
                    ty12 = slope1 * tx12 + intercept1;
                }
                // Find point midway between point 1 and point 2
                midx12 = (x1 + x2)/2.0f;
                midy12 = (y1 + y2)/2.0f;
                // Find slope and intercept of line connecting intersection point of tangent1 and tangent2 and midpoint of 1 and 2
                tangentx12 = tx12 - midx12;
                tangenty12 = ty12 - midy12;
                if (tangentx12 == 0.0f) {
                    slope12 = Float.POSITIVE_INFINITY;
                }
                else {
                    slope12 = tangenty12/tangentx12;
                    intercept12 = midy12 - slope12 * midx12;
                }
                while (true) {
                    number3 = randomGen.genUniformRandomNum(0, numPoints - 1);
                    if ((number1 == number3) || (number2 == number3)) {
                        continue;
                    }
                    x3 = indexArray[number3] % xDim;
                    y3 = indexArray[number3] / xDim;
                    xDiff = x1 - x3;
                    yDiff = y1 - y3;
                    distanceSquared = xDiff * xDiff + yDiff * yDiff;
                    // Don't use if points 1 and 3 are less than a minimum distance or greater than
                    // a maximum distance apart
                    if ((distanceSquared < minDistanceSquared) || (distanceSquared > maxDistanceSquared)) {
                        continue;
                    }
                    xDiff = x2 - x3;
                    yDiff = y2 - y3;
                    distanceSquared = xDiff * xDiff + yDiff * yDiff;
                    // Don't use if points 2 and 3 are less than a minimum distance or greater than
                    // a maximum distance apart
                    if ((distanceSquared < minDistanceSquared) || (distanceSquared > maxDistanceSquared)) {
                        continue;
                    }
                    // Don't use if tangent lines 2 and 3 are parallel
                    slope3 = slopeArray[number3];
                    if (Float.isInfinite(slope2) && Float.isInfinite(slope3)) {
                        continue;
                    }
                    if (slope2 == slope3) {
                        continue;
                    }
                    break;
                }
                // Find intersection of tangent 2 and tangent3.
                intercept3 = interceptArray[number3];
                if (Float.isInfinite(slope2)) {
                    tx23 = x2;
                    ty23 = slope3 * tx23 + intercept3;
                }
                else if (Float.isInfinite(slope3)) {
                    tx23 = x3;
                    ty23 = slope2 * tx23 + intercept2;
                }
                else {
                    tx23 = (intercept3 - intercept2)/(slope2 - slope3);
                    ty23 = slope2 * tx23 + intercept2;
                }
                // Find point midway between point 2 and point 3
                midx23 = (x2 + x3)/2.0f;
                midy23 = (y2 + y3)/2.0f;
                // Find slope and intercept of line connecting intersection point of tangent2 and tangent3 and midpoint of 2 and 3
                tangentx23 = tx23 - midx23;
                tangenty23 = ty23 - midy23;
                if (tangentx23 == 0.0f) {
                    slope23 = Float.POSITIVE_INFINITY;
                }
                else {
                    slope23 = tangenty23/tangentx23;
                    intercept23 = midy23 - slope23 * midx23;
                }
                // Don't use if lines are parallel
                if (Float.isInfinite(slope12) && Float.isInfinite(slope23)) {
                    continue;
                }
                if (slope12 == slope23) {
                    continue;
                }
                // The estimated center of the hyperbola is at the intersection of these 2 lines
                if (Float.isInfinite(slope12)) {
                    xCenter = midx12;
                    yCenter = slope23 * xCenter + intercept23;
                }
                else if (Float.isInfinite(slope23)) {
                    xCenter = midx23;
                    yCenter = slope12 * xCenter + intercept12;
                }
                else {
                    xCenter = (intercept23 - intercept12)/(slope12 - slope23);
                    yCenter = slope12 * xCenter + intercept12;
                }
                if ((xCenter < minXCenter) || (xCenter > maxXCenter)) {
                    continue;
                }
                if ((yCenter < minYCenter) || (yCenter > maxYCenter)) {
                    continue;
                }
                // Translate the hyperbola to be centered at the origin
                x1c = x1 - xCenter;
                x2c = x2 - xCenter;
                x3c = x3 - xCenter;
                y1c = y1 - yCenter;
                y2c = y2 - yCenter;
                y3c = y3 - yCenter;
                // Solve:
                // [x1c**2  2*x1c*y1c  y1c**2] [a]   [1]
                // [x2c**2  2*x2c*y2c  y2c**2] [b] = [1]
                // [x3c**2  2*x3c*y3c  y3c**2] [c]   [1]
                A.set(0, 0, x1c*x1c);
                A.set(0, 1, 2.0*x1c*y1c);
                A.set(0, 2, y1c*y1c);
                A.set(1, 0, x2c*x2c);
                A.set(1, 1, 2.0*x2c*y2c);
                A.set(1, 2, y2c*y2c);
                A.set(2, 0, x3c*x3c);
                A.set(2, 1, 2.0*x3c*y3c);
                A.set(2, 2, y3c*y3c);
                B.set(0, 0, 1.0);
                B.set(1, 0, 1.0);
                B.set(2, 0, 1.0);
                try {
                    X = A.solve(B);
                }
                catch (RuntimeException e) {
                    // Matrix is singular
                    continue;
                }
                if (X == null) continue;
                // Obtain the remaining 3 hyperbola parameters, a, b, and c
                // where a*x**2 + 2*b*x*y + c*y**2 = 1
                // in the now zero centered hyperbola
                a = X.get(0, 0);
                b = X.get(1, 0);
                c = X.get(2, 0);
                // The inequality a*c - b**2 < 0 is true if the parameters represent a valid hyperbola.
                // If this inequality is false, it implies that either the 3 pixels do not lie on the same hyperbola,
                // or that the estimates of the tangents were inaccurate.  In either case, we discard the 
                // parameters and choose a new pixel triplet.
                if ((a*c - b*b) >= 0.0) {
                    continue;
                }
                // Convert a, b, and c to r1, r2, and orientation angle theta
                // 2*r1 = transverse axis = distance between 2 vertices of hyperbola =
                //        difference of distances of any point on hyperbola from foci
                // 2*r2 = conjugate axis
                // sqrt(r1**2 + r2**2) = distance form center to either focus
                // Convert a*x**2 + 2*b*x*y + c*y**2 = 1 to
                // (y*sin(theta) + x*cos(theta))**2/r1**2 - (y*cos(theta) - x*sin(theta))**2/r2**2 = 1
                // y**2*(sin(theta))**2/r1**2 + 2*x*y*cos(theta)*sin(theta)/r1**2 + x**2*(cos(theta))**2/r1**2
                // - y**2*(cos(theta))**2/r2**2 + 2*x*y*cos(theta)*sin(theta)/r2**2 - x**2*(sin(theta))**2/r2**2 = 1
                // a = (cos(theta))**2/r1**2 - (sin(theta))**2/r2**2
                // c = (sin(theta))**2/r1**2 - (cos(theta))**2/r2**2
                // b = cos(theta)*sin(theta)*(1/r1**2 + 1/r2**2) = (1/2)*sin(2*theta)*(1/r1*2 + 1/r2**2)
                // a + c = 1/r1**2 - 1/r2**2
                // a - c = ((cos(theta))**2 - (sin(theta))**2)*(1/r1**2 + 1/r2**2)
                // a - c = (cos(2*theta)) * (1/r1*2 + 1/r2**2)
                // b/(a - c)= tan(2*theta)/2
                // tan(2*theta) = tan(2*theta + PI) = (2*b)/(a - c)
                // 2*theta = arctan((2*b)/(a - c)) and 2*theta + PI = arctan((2*b)/(a - c))
                // theta = 0.5 * arctan((2*b)/(a - c)) and theta = 0.5 * arctan((2*b)/(a - c)) - PI/2
                // sin(2*theta) = (2*b)/(1/r1**2 + 1/r2**2)
                // cos(2*theta) = (a - c)/(1/r1**2 + 1/r2**2)
                // 1 = (4*b**2 + (a - c)**2)/((1/r1**2 + 1/r2**2)**2)
                // 1/r1**2 + 1/r2**2 = sqrt(4*b**2 + (a - c)**2)
                // 1/r2**2 = -a - c + 1/r1**2
                // 2/r1**2 - a - c = sqrt(4*b**2 + (a - c)**2)
                // 2/r1**2 = a + c + sqrt(4*b**2 + (a - c)**2)
                // r1**2/2 = 1/(a + c + sqrt(4*b**2 + (a - c)**2))
                // r1 = sqrt(2)/sqrt(a + c + sqrt(4*b**2 + (a - c)**2))
                // 1/r2**2 = -a - c + 1/r1**2
                // 1/r2**2 = (-2*a - 2*c + (a + c + sqrt(4*b**2 + (a - c)**2)))/2
                // 1/r2**2 = (-a - c + sqrt(4*b**2 + (a - c)**2))/2
                // r2 = sqrt(2)/sqrt(-a - c + sqrt(4*b**2 + (a - c)**2))
                aminusc = a - c;
                twob = 2.0*b;
                var = Math.sqrt(twob*twob + aminusc*aminusc);
                aplusc = a + c;
                r1 = sqr2/Math.sqrt(aplusc + var);
                r2 = sqr2/Math.sqrt(-aplusc + var);
                theta = 0.5 * Math.atan2(twob, aminusc);
                if ((r1 < minR1) || (r1 > maxR1)) {
                    continue;
                }
                if ((r2 < minR2) || (r2 > maxR2)) {
                    continue;
                }
                pointSetsAcquired++;
                xCenterIndex = (int)((xCenter - minXCenter)/maxPixelBinWidth);
                yCenterIndex = (int)((yCenter - minYCenter)/maxPixelBinWidth);
                r1Index = (int)((r1 - minR1)/maxPixelBinWidth);
                r2Index = (int)((r2 - minR2)/maxPixelBinWidth);
                thetaIndex = (int)((theta - minTheta)/maxRadiansDiff);
                index = xCenterIndex + scale1 * yCenterIndex + scale12 * r1Index + scale123 * r2Index + scale1234 * thetaIndex;
                xCenterArray[index] += xCenter;
                yCenterArray[index] += yCenter;
                r1Array[index] += r1;
                r2Array[index] += r2;
                thetaArray[index] += theta;
                countArray[index]++;
            } // while (pointSetsAcquired < pointSetsRequired)
            for (i = 0; i < numBins; i++) {
                testedArray[i] = false;
            }
            hyperbolaDetected = false;
            while (!hyperbolaDetected) {
                // Keep checking all bins that meet the countThreshold until an hyperbola which
                // has minPixels along the perimiter is found
                maxIndex = -1;
                maxCount = 0;
                for (i = 0; i < numBins; i++) {
                    if ((!testedArray[i]) && (countArray[i] > maxCount)) {
                        maxIndex = i;
                        maxCount = countArray[i];
                    }
                } // for (i = 0; i < numBins; i++)
                testedArray[maxIndex] = true;
                if (maxCount < countThreshold) {
                    break;
                }
                
                xCenter = xCenterArray[maxIndex]/maxCount;
                yCenter = yCenterArray[maxIndex]/maxCount;
                r1 = r1Array[maxIndex]/maxCount;
                r2 = r2Array[maxIndex]/maxCount;
                theta1 = thetaArray[maxIndex]/maxCount;
                theta2 = theta1 - Math.PI/2.0;
                // 2 theta values separated by 90 degrees must both be examined to see which one
                // yields the largest number of pixels along the perimiter
                costheta1 = Math.cos(theta1);
                sintheta1 = Math.sin(theta1);
                costheta2 = Math.cos(theta2);
                sintheta2 = Math.sin(theta2);
                r1Sq = r1 * r1;
                r2Sq = r2 * r2;
                pointsOnHyperbola1 = 0;
                pointsOnHyperbola2 = 0;
                for (i = 0; i < numPoints; i++) {
                    x = indexArray[i] % xDim;
                    y = indexArray[i] / xDim;
                    // Check on a zero centered hyperbola
                    xc = x - xCenter;
                    yc = y - yCenter;
                    var1 = yc * sintheta1 + xc*costheta1;
                    var2 = yc * costheta1 - xc*sintheta1;
                    checkVal = var1*var1/r1Sq - var2*var2/r2Sq;
                    if ((checkVal >= lowerLimit) && (checkVal <= upperLimit)) {
                        pointsOnHyperbola1++;
                        if (foundPoint[i] == 0) {
                            foundPoint[i] = 1;
                        }
                    }
                    var1 = yc * sintheta2 + xc*costheta2;
                    var2 = yc * costheta2 - xc*sintheta2;
                    checkVal = var1*var1/r1Sq - var2*var2/r2Sq;
                    if ((checkVal >= lowerLimit) && (checkVal <= upperLimit)) {
                        pointsOnHyperbola2++;
                        if ((foundPoint[i] == 0) || (foundPoint[i] == 1)) {
                            foundPoint[i] += 2;
                        }
                    }
                } // for (i = 0; i < numPoints; i++)
                if (pointsOnHyperbola1 > pointsOnHyperbola2) {
                    pointsOnHyperbola = pointsOnHyperbola1;
                    theta = theta1;
                }
                else {
                    pointsOnHyperbola = pointsOnHyperbola2;
                    theta = theta2;
                }
                if (pointsOnHyperbola >= minPixels) {
                    xCenterTable[hyperbolasFound] = (float)xCenter;
                    yCenterTable[hyperbolasFound] = (float)yCenter;
                    r1Table[hyperbolasFound] = (float)r1;
                    r2Table[hyperbolasFound] = (float)r2;
                    thetaTable[hyperbolasFound] = (float)(theta);
                    countTable[hyperbolasFound] = pointsOnHyperbola;
                    hyperbolasFound++;
                    hyperbolaDetected = true;
                    ViewUserInterface.getReference().setDataText("Hyperbola # " + hyperbolasFound + " found\n");
                    ViewUserInterface.getReference().setDataText(" x center = " + xCenter + "\n");
                    ViewUserInterface.getReference().setDataText(" y center = " + yCenter + "\n");
                    ViewUserInterface.getReference().setDataText(" r1 = " + r1 + "\n");
                    ViewUserInterface.getReference().setDataText(" r2 = " + r2 + "\n");
                    ViewUserInterface.getReference().setDataText(" theta = " + (theta * 180/Math.PI) + "\n");
                    if (pointsOnHyperbola1 > pointsOnHyperbola2) {
                        for (i = 0; i < numPoints; i++) {
                            if ((foundPoint[i] == 1) || (foundPoint[i] == 3)) {
                                foundPoint[i] = 4;
                            }
                            else if (foundPoint[i] == 2) {
                                foundPoint[i] = 0;
                            }
                        }
                    } // if (pointsOnHyperbola1 > pointsOnHYperbola2)
                    else { // else pointsOnHyperbola1 <= pointsOnHyperbola2
                        for (i = 0; i < numPoints; i++) {
                            if ((foundPoint[i] == 2) || (foundPoint[i] == 3)) {
                                foundPoint[i] = 4;
                            }
                            else if (foundPoint[i] == 1) {
                                foundPoint[i] = 0;
                            }
                        }   
                    } // else pointsOnHyperbola
                }
                else { // pointsOnHyperbola < minPixels
                    for (i = 0; i < numPoints; i++) {
                        if ((foundPoint[i] >= 1) && (foundPoint[i] <= 3)) {
                            foundPoint[i] = 0;
                        }
                    }
                }
            } // while (!hyperbolaDetected)
            if (hyperbolasFound == numHyperbolas) {
                break;
            }
            // If a hyperbola was found, then zero the Hough transform array before acquiring a
            // new set of point triplets.
            for (i = 0; i < numBins; i++) {
                xCenterArray[i] = 0.0f;
                yCenterArray[i] = 0.0f;
                r1Array[i] = 0.0f;
                r2Array[i] = 0.0f;
                thetaArray[i] = 0.0f;
                countArray[i] = 0;
            }
            // If a hyperbola was found, then delete the points found along its perimiter
            // from indexArray, slopeArray, and interceptArray before running the
            // Hough transform again
            pointsDeleted = 0;
            for (i = 0; i < numPoints; i++) {
                if (foundPoint[i] != 0) {
                    pointsDeleted++;
                }
            } // for (i = 0; i < numPoints; i++)
            if (pointsDeleted > 0) {
                newNumPoints = numPoints - pointsDeleted;
                if (newNumPoints == 0) {
                    break;
                }
                newIndexArray = new int[newNumPoints];
                newSlopeArray = new float[newNumPoints];
                newInterceptArray = new float[newNumPoints];
                for (i = 0, j = 0; i < numPoints; i++) {
                    if (foundPoint[i] == 0) {
                        newIndexArray[j] = indexArray[i];
                        newSlopeArray[j] = slopeArray[i];
                        newInterceptArray[j] = interceptArray[i];
                        j++;
                    }
                } // for (i = 0, j = 0; i < numPoints; i++)
                numPoints = newNumPoints;
                foundPoint = null;
                foundPoint = new byte[numPoints];
                indexArray = null;
                indexArray = new int[numPoints];
                slopeArray = null;
                slopeArray = new float[numPoints];
                interceptArray = null;
                interceptArray = new float[numPoints];
                for (i = 0; i < numPoints; i++) {
                    indexArray[i] = newIndexArray[i];
                    slopeArray[i] = newSlopeArray[i];
                    interceptArray[i] = newInterceptArray[i];
                }
                newIndexArray = null;
                newSlopeArray = null;
                newInterceptArray = null;
            } // if (pointsDeleted > 0)
        } // while (hyperbolaFindCycles < maxHyperbolaFindCycles)
        xCenterArray = null;
        yCenterArray = null;
        r1Array = null;
        r2Array = null;
        thetaArray = null;
        foundPoint = null;
        indexArray = null;
        slopeArray = null;
        interceptArray = null;
        testedArray = null;
//        try {
//            A.finalize();
//            B.finalize();
//            if (X != null) {
//                X.finalize();
//            }
//        }
//        catch (Throwable e) {
//            
//        }
        A = null;
        B = null;
        X = null;
        randomGen = null;
        
        
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
        
        // Create a dialog with numhyperbolasFound xCenterTable[i], yCenterTable[i], r1Table[i],
        // r2Table[i], thetaTable[i], and countTable[i] values, where the user will select a check box
        // to have the selected hyperbola drawn.
        selectedHyperbola = new boolean[hyperbolasFound];
        
        choice = new JDialogHoughHyperbolaChoice(ViewUserInterface.getReference().getMainFrame(), xCenterTable,
                 xDim, yCenterTable, yDim, r1Table, minR1, maxR1, r2Table, minR2, maxR2, thetaTable,
                 countTable, selectedHyperbola);
        
        if (!choice.okayPressed() ) {
            setCompleted(false);
            return;
        }
        
        for (i = 0; i < hyperbolasFound; i++) {
            if (selectedHyperbola[i]) {
                hyperbolasDrawn++;
            }
        }
        
        // Draw selected hyperbolas
        hyperbolaPoints = new int[hyperbolasDrawn];
        for (i = 0, k = 0; i < hyperbolasFound; i++) {
            if (selectedHyperbola[i]) {
                beta = thetaTable[i];
                cosbeta = Math.cos(beta);
                sinbeta = Math.sin(beta);
                for (j = -359; j <= 359; j++) {
                    alpha = j * Math.PI/720.0;
                    secalpha = 1.0/Math.cos(alpha);
                    tanalpha = Math.tan(alpha);
                    x = (int)(xCenterTable[i] + r1Table[i] * secalpha * cosbeta - r2Table[i] * tanalpha * sinbeta);
                    if ((x >= 0) && (x < xDim)) {
                        y = (int)(yCenterTable[i] + r1Table[i] * secalpha * sinbeta + r2Table[i] * tanalpha * cosbeta);
                        if ((y >= 0) && (y < yDim)) {
                            index = x + (xDim*y);
                            srcBuffer[index] = value;
                            hyperbolaPoints[k]++;
                        }
                    }
                }
                k++;
            } // if (selectedHyperbola[i])
        } // for (i = 0; i < hyperbolasFound; i++)
        
        
        hyperbolaVOI = new VOI[hyperbolasDrawn];
        for (i = 0, k = 0; i < hyperbolasFound; i++) {
            if (selectedHyperbola[i]) {
                n = 0;
                hyperbolaVOI[k] = new VOI((short)k, "hyperbolaVOI" + Integer.toString(k), VOI.CONTOUR, -1.0f);
                hyperbolaVOI[k].setColor(Color.red);
                xArr = new float[hyperbolaPoints[k]];
                yArr = new float[hyperbolaPoints[k]];
                zArr = new float[hyperbolaPoints[k]];
                beta = thetaTable[i];
                cosbeta = Math.cos(beta);
                sinbeta = Math.sin(beta);
                for (j =-359; j <= 359; j++) {
                    alpha = j * Math.PI/720.0;
                    secalpha = 1.0/Math.cos(alpha);
                    tanalpha = Math.tan(alpha);
                    x = (int)(xCenterTable[i] + r1Table[i] * secalpha * cosbeta - r2Table[i] * tanalpha * sinbeta);
                    if ((x >= 0) && (x < xDim)) {
                        y = (int)(yCenterTable[i] + r1Table[i] * secalpha * sinbeta + r2Table[i] * tanalpha * cosbeta);
                        if ((y >= 0) && (y < yDim)) {
                            xArr[n] = x;
                            yArr[n++] = y;
                        }
                    }
                }
                hyperbolaVOI[k].importCurve(xArr, yArr, zArr);
                ((VOIContour)(hyperbolaVOI[k].getCurves().elementAt(0))).setFixed(true);
                destImage.registerVOI(hyperbolaVOI[k]);
                k++;
            } // if (selectedHyperbola[i])
        } // for (i = 0; i < hyperbolasFound; i++)
        
        
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
}
