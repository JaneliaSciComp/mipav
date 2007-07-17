package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;

/**
 *  This Hough transform uses (xi, yi) points in the original image space to generate p, q, r1, r2, and theta points in the Hough
 *  transform.  This Hough transform module only works with binary images.   Before it is used the user must 
 *  compute the gradient of an image and threshold it to obtain a binary image.  Noise removal and thinning should also
 *  be performed, if necessary, before this program is run. 
 *  
 *  Pixel neighbors:
 *  0 1 2
 *  3   4
 *  5 6 7
 *  
 *  If more ellipses are to be found, then zero the houghBuffer and run through the
 *  same Hough transform a second time, but on this second run instead of incrementing
 *  the Hough buffer, zero the values in the source buffer that contributed to the peak
 *  point in the Hough buffer. So on the next run of the Hough transform the source points that
 *  contributed to the Hough peak value just detected will not be present.
 *  
 *  Create a dialog with numEllipsesFound pArray[i], qArray[i], r1Array[i], r2Array[i], thetaArray[i], and
 *  countArray[i] values, where the user will select a check box to have that ellipse drawn.
 *  
 *  References: 1.) The Randomized Hough Transform used for ellipse detection by Andrew Schuler
 *  
 *  2.) Technical Report - Randomized Hough Transform: Improved Ellipse Detection with Comparison by
 *  Robert A. Mclaughlin
 *  
 *  3.) Digital Image Processing, Second Edition by Richard C. Gonzalez and Richard E. Woods, Section 10.2.2
 *  Global Processing via the Hough Transform, Prentice-Hall, Inc., 2002, pp. 587-591.
 *  
 *  4.) Shape Detection in Computer Vision Using the Hough Transform by V. F. Leavers, Springer-Verlag, 1992.
 * 
 */
public class AlgorithmHoughEllipse extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------
    // Minimum percentage of the perimiter of a found ellipse that must be covered by points for it to be
    // valid.  The perimiter of an ellipse = 4 * a * E(e), where E is the complete elliptic integral of
    // the second kind and e = the eccentricity = sqrt(r1**2 - r2**2)/r1
    // An approximation is perimiter equals approximately PI * sqrt(2*(r1**2 + r2**2))
    // A more exact approximation is perimiter equals approximately
    // PI * [3*(r1 + r2) - sqrt((r1 + 3*r2)*(3*r1 + r2))]
    // More exact still is perimiter equals approximately 
    // PI * (r1 + r2) * [1 + (3*h)/(10 + sqrt(4 - 3*h))], where h = ((r1 - r2)/(r1 + r2))**2,
    // where the last approximation has an  error of about 3 * 2**-17 * h**5 for small values of h.
    // Default value is 30.0.
    private double minCoverage;
    
    // Maximum number of points to take from each side of a point on a curve in determining a tangent
    // If only 1 point is available on each side, simply use avarage of slopes to each of the
    // neigboring points.  If at least 2 or more points are available on each side, use 
    // bspline.bSplineJetXY.
    private int sidePointsForTangent;
    
    // For p, q, r1, and r2 must have the abs(var(i) - var) <= maxPixelDiff if the set of 5 values is to
    // be placed into an existing bin.  If any of the 4 variables exceeds this difference, then a
    // new bin must be created.
    private double maxPixelDiff;
    
    // For theta must have abs(theta(i) - theta) <= maxDegreesDiff if the set of 5 values is to be
    // placed into an existing bin.  If the difference exceeds maxDegreesDiff, then a new bin
    // must be created.
    private double maxDegreesDiff;
    
    // Smallest allowable distance between 2 of 3 picked points
    private double minPointDistance;
    
    // Largest allowable distance between 2 of 3 picked points
    private double maxPointDistance;
    
    // Number of point triplets required before each ellipse find is performed
    private int pointSetsRequired;
    
    // Maximum ratio of major axis to minor axis - default is 2.0;
    private double maxAxesRatio;
    
    // number of ellipses to be found
    private int numEllipses;
    
    private int maxEllipseFindCycles = 80;
    
    // The maximum Hough transform size in megabytes - default is currently 256
    private int maxBufferSize;
    
    // Number of counts required to find an ellipse
    private int countThreshold;
    
    // Maximum pixel distance by which ellipse perimiter pixels can deviate from the calculated curve
    private double ellipseRangeTolerance;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmHoughEllipse - default constructor.
     */
    public AlgorithmHoughEllipse() { }

    /**
     * AlgorithmHoughEllipse.
     *
     * @param  destImg  Image with lines filled in
     * @param  srcImg   Binary source image that has lines with gaps
     * @param  minCoverage Minimum percentage of the perimiter of a found ellipse that must be covered by points
     *                     for it to be valid.
     * @param  sidePointsForTangent  Maximum number of points to take from each side of a point on a curve
     *                               in determining the tangent
     * @param  maxPixelDiff  Maximum pixel difference allowed for p, q, r1, and r2 for 5 values to be placed
     *                       into an existing bin
     * @param  maxDegreesDiff Maximum degrees difference allowed for theth for 5 values to be placed into
     *                        an existing bin
     * @param  minPointDistance Smallest allowable distance between 2 of 3 picked points
     * @param  maxPointDistance Largest allowable distance between 2 of 3 picked points
     * @param  pointSetsRequired Number of point triplets acquired before each ellipse find is performed
     * @param  countThreshold Number of counts required to find an ellipse
     * @param  ellipseRangeTolerance Maximum pixel distance by which ellipse perimiter pixels can deviate from
     *                               the calculated space
     * @param  maxAxesRatio  Maximum ratio of major axis to minor axis
     * @param  numEllipses number of ellipses to be found
     * @param  maxBufferSize maximum Hough transform size in megabytes
     */
    public AlgorithmHoughEllipse(ModelImage destImg, ModelImage srcImg, double minCoverage, int sidePointsForTangent,
                                 double maxPixelDiff, double maxDegreesDiff, double minPointDistance,
                                 double maxPointDistance, int pointSetsRequired, int countThreshold, 
                                 double ellipseRangeTolerance, double maxAxesRatio, int numEllipses,
                                 int maxBufferSize) {
        super(destImg, srcImg);
        this.minCoverage = minCoverage;
        this.sidePointsForTangent = sidePointsForTangent;
        this.maxPixelDiff = maxPixelDiff;
        this.maxDegreesDiff = maxDegreesDiff;
        this.minPointDistance = minPointDistance;
        this.maxPointDistance = maxPointDistance;
        this.pointSetsRequired = pointSetsRequired;
        this.countThreshold = countThreshold;
        this.ellipseRangeTolerance = ellipseRangeTolerance;
        this.maxAxesRatio = maxAxesRatio;
        this.numEllipses = numEllipses;
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

        int i, j, k, m, n;
        int index, indexDest;
        
        int houghSlice;
        byte[] srcBuffer;
        short[][][][][] houghBuffer;
        double theta;
        double d1Array[];
        double d2Array[];
        double d3Array[];
        double d3;
        double d3Scale;
        boolean test = true;
        double xCenter;
        double yCenter;
        double radius;
        double radius2;
        double radius3;
        int largestValue;
        int largestIndex;
        int numEllipsesFound = 0;
        double x0Array[];
        double y0Array[];
        double radArray[];
        int countArray[];
        boolean selectedEllipse[];
        //JDialogHoughEllipseChoice choice;
        byte value = 0;
        int maxEllipsePoints;
        double maxRad;
        int x0y0;
        double xSum;
        double xSum2;
        double xSum3;
        double ySum;
        double ySum2;
        double ySum3;
        double radSum;
        double maxDist[];
        double xpos[];
        double ypos[];
        double majorAxis[];
        double maxRoot;
        double x1min;
        double x1max;
        int x1num;
        double perx;
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
        double xpos1;
        double xdist;
        double maxyDist;
        double maxDistSq[];
        double y1min;
        double y1max;
        int y1num;
        double pery;
        int bufferBytes;
        float bufferMbytes;
        double invx;
        double invy;
        double invm;
        double invx1;
        int indexArray[];
        int curve1[];
        int curve2[];
        int numCurves;
        int curvesAtPoint;
        int neighbors;
        int endPoints;
        int numOpenCurves;
        int numClosedCurves;
        boolean foundArray[];
        boolean found;
        int curvePos;
        int openStart[];
        int openLength[];
        int nextPos;
        int closedStart;
        int closedLength;
        int indexPtr = 0;
        int firstPoint;
        int nextPoint;
        int lastPoint;
        float xArray[];
        float yArray[];
        float tangentX;
        float tangentY;
        float slopeArray[];
        float interceptArray[];;
        int startPtr;
        int presentSidePoints;
        float xPoints[];
        float yPoints[];
        AlgorithmBSpline bSpline = null;
        Point2Df tangentDir = null;
        int endPtr;
        int startWrapPoints;
        int endWrapPoints;
        double a1;
        double b1;
        double alpha;
        double cosalpha;
        double sinalpha;
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
        Matrix X;
        double a;
        double b;
        double c;
        double r1;
        double r2;
        double aminusc;
        double sqr2 = Math.sqrt(2.0);
        double twob;
        double var;
        double aplusc;

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        constructLog();

        fireProgressStateChanged(srcImage.getImageName(), "Hough ellipse ...");

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
            for (i = 0; i < 90; i++) {
                alpha = i * Math.PI/360.0;
                cosalpha = Math.cos(alpha);
                sinalpha = Math.sin(alpha);
                x = (int)(xCenter + a1 * cosalpha * cosbeta - b1 * sinalpha * sinbeta);
                y = (int)(yCenter + a1 * cosalpha * sinbeta + b1 * sinalpha * cosbeta);
                index = x + (xDim*y);
                srcBuffer[index] = 1; 
            }
            
            for (i = 180; i < 270; i++) {
                alpha = i * Math.PI/360.0;
                cosalpha = Math.cos(alpha);
                sinalpha = Math.sin(alpha);
                x = (int)(xCenter + a1 * cosalpha * cosbeta - b1 * sinalpha * sinbeta);
                y = (int)(yCenter + a1 * cosalpha * sinbeta + b1 * sinalpha * cosbeta);
                index = x + (xDim*y);
                srcBuffer[index] = 1; 
            }
            
            for (i = 360; i < 450; i++) {
                alpha = i * Math.PI/360.0;
                cosalpha = Math.cos(alpha);
                sinalpha = Math.sin(alpha);
                x = (int)(xCenter + a1 * cosalpha * cosbeta - b1 * sinalpha * sinbeta);
                y = (int)(yCenter + a1 * cosalpha * sinbeta + b1 * sinalpha * cosbeta);
                index = x + (xDim*y);
                srcBuffer[index] = 1; 
            }
            
            for (i = 540; i < 630; i++) {
                alpha = i * Math.PI/360.0;
                cosalpha = Math.cos(alpha);
                sinalpha = Math.sin(alpha);
                x = (int)(xCenter + a1 * cosalpha * cosbeta - b1 * sinalpha * sinbeta);
                y = (int)(yCenter + a1 * cosalpha * sinbeta + b1 * sinalpha * cosbeta);
                index = x + (xDim*y);
                srcBuffer[index] = 1; 
            }
            
            a1 = 100.0;
            b1 = 50.0;
            angle = -Math.PI/4.0;
            beta = -angle;
            cosbeta = Math.cos(beta);
            sinbeta = Math.sin(beta);
            for (i = 0; i < 90; i++) {
                alpha = i * Math.PI/360.0;
                cosalpha = Math.cos(alpha);
                sinalpha = Math.sin(alpha);
                x = (int)(xCenter + a1 * cosalpha * cosbeta - b1 * sinalpha * sinbeta);
                y = (int)(yCenter + a1 * cosalpha * sinbeta + b1 * sinalpha * cosbeta);
                index = x + (xDim*y);
                srcBuffer[index] = 1; 
            }
            
            for (i = 180; i < 270; i++) {
                alpha = i * Math.PI/360.0;
                cosalpha = Math.cos(alpha);
                sinalpha = Math.sin(alpha);
                x = (int)(xCenter + a1 * cosalpha * cosbeta - b1 * sinalpha * sinbeta);
                y = (int)(yCenter + a1 * cosalpha * sinbeta + b1 * sinalpha * cosbeta);
                index = x + (xDim*y);
                srcBuffer[index] = 1; 
            }
            
            for (i = 360; i < 450; i++) {
                alpha = i * Math.PI/360.0;
                cosalpha = Math.cos(alpha);
                sinalpha = Math.sin(alpha);
                x = (int)(xCenter + a1 * cosalpha * cosbeta - b1 * sinalpha * sinbeta);
                y = (int)(yCenter + a1 * cosalpha * sinbeta + b1 * sinalpha * cosbeta);
                index = x + (xDim*y);
                srcBuffer[index] = 1; 
            }
            
            for (i = 540; i < 630; i++) {
                alpha = i * Math.PI/360.0;
                cosalpha = Math.cos(alpha);
                sinalpha = Math.sin(alpha);
                x = (int)(xCenter + a1 * cosalpha * cosbeta - b1 * sinalpha * sinbeta);
                y = (int)(yCenter + a1 * cosalpha * sinbeta + b1 * sinalpha * cosbeta);
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
        
        // Reduce the number of 8-connected neighbors to 2
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
        i = 0;
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
                    } // while(neighbor2[index] != -1;
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
        
        indexArray = new int[numPoints];
        slopeArray = new float[numPoints];
        interceptArray = new float[numPoints];
        for (i = 0; i < sourceSlice; i++) {
            if (srcBuffer[i] != 0) {
                foundArray[i] = false;
            }
        }
        indexPtr = 0;
        if (sidePointsForTangent > 1) {
            bSpline = new AlgorithmBSpline();
            tangentDir = new Point2Df();
        }
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
                    tangentDir = bSpline.bSplineJetXY(1, presentSidePoints, xPoints, yPoints);
                    if (tangentDir.x == 0.0f) {
                        slopeArray[indexPtr++] = Float.POSITIVE_INFINITY;
                    }
                    else {
                        slopeArray[indexPtr] = tangentDir.y/tangentDir.x;
                        interceptArray[indexPtr] = yArray[n] - slopeArray[indexPtr] * xArray[n];
                        indexPtr++;
                    }
                }
            } // for (n = 2; n <= openLength[i] - 2; n++)
        } // for (i = 0; i < numOpenCurves; i++)      
        
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
                    closedStart = index;
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
                        startWrapPoints = Math.max(0, sidePointsForTangent - n);
                        endWrapPoints =  Math.max(0, sidePointsForTangent - (closedLength - 1 - n));
                        for (k = 0; k < startWrapPoints; k++) {
                            xPoints[k] = indexArray[endPtr - k] % xDim;
                            yPoints[k] = indexArray[endPtr - k] / xDim;
                        }
                        for (k = startWrapPoints, j = indexPtr - sidePointsForTangent + startWrapPoints;
                             k < 2*sidePointsForTangent + 1 - endWrapPoints; j++, k++) {
                            xPoints[k] = indexArray[j] % xDim;
                            yPoints[k] = indexArray[j] / xDim;
                        }
                        for (k = 2*sidePointsForTangent + 1 - endWrapPoints; k < 2*sidePointsForTangent + 1; k++) {
                            xPoints[k] = indexArray[startPtr + k] % xDim;
                            yPoints[k] = indexArray[startPtr + k] / xDim;
                        }
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
                            tangentDir = bSpline.bSplineJetXY(1, sidePointsForTangent, xPoints, yPoints);
                            if (tangentDir.x == 0.0f) {
                                slopeArray[indexPtr++] = Float.POSITIVE_INFINITY;
                            }
                            else {
                                slopeArray[indexPtr] = tangentDir.y/tangentDir.x;
                                interceptArray[indexPtr] = yPoints[sidePointsForTangent] - slopeArray[indexPtr] * xPoints[sidePointsForTangent];
                                indexPtr++;
                            }    
                        }
                    } // for (n = 0; n <= closedLength[i] - 1; n++)
                }
            } // for (x = 0; x < xDim; x++)
        } // for (y = 0; y < yDim; y++)
        
        ViewUserInterface.getReference().setDataText("Number of closed curves = " + numClosedCurves + "\n");
        
        randomGen = new RandomNumberGen();
        minDistanceSquared = minPointDistance * minPointDistance;
        maxDistanceSquared = maxPointDistance * maxPointDistance;
        while ((ellipsesFound < numEllipses) && (ellipseFindCycles < maxEllipseFindCycles)) {
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
                    if ((distanceSquared < minDistanceSquared) || (distanceSquared > maxDistanceSquared)) {
                        continue;
                    }
                    // Don't use if lines are parallel
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
                // Find slope and intercept of line connecting intsection point of tangent1 and tangent2 and midpoint of 1 and 2
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
                    if ((distanceSquared < minDistanceSquared) || (distanceSquared > maxDistanceSquared)) {
                        continue;
                    }
                    xDiff = x2 - x3;
                    yDiff = y2 - y3;
                    distanceSquared = xDiff * xDiff + yDiff * yDiff;
                    if ((distanceSquared < minDistanceSquared) || (distanceSquared > maxDistanceSquared)) {
                        continue;
                    }
                    // Don't use if lines are parallel
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
                // Find slope and intercept of line connecting intsection point of tangent2 and tangent3 and midpoint of 2 and 3
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
                // The estimated center of the ellipse is at the intersection of these 2 lines
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
                // Translate the ellipse to be centered at the origin
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
                A = new Matrix(3,3);
                A.set(0, 0, x1c*x1c);
                A.set(0, 1, 2.0*x1c*y1c);
                A.set(0, 2, y1c*y1c);
                A.set(1, 0, x2c*x2c);
                A.set(1, 1, 2.0*x2c*y2c);
                A.set(1, 2, y2c*y2c);
                A.set(2, 0, x3c*x3c);
                A.set(2, 1, 2.0*x3c*y3c);
                A.set(2, 2, y3c*y3c);
                B = new Matrix(3,1);
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
                // Obtain the remaining 3 ellipse parameters, a, b, and c
                // where a*x**2 + 2*b*x*y + c*y**2 = 1
                // in the now zero centered ellipse
                a = X.get(0, 0);
                b = X.get(1, 0);
                c = X.get(2, 0);
                // The inequality a*c - b**2 > 0 is true if the parameters represent a valid ellipse.
                // If this inequality is false, it implies that either the 3 pixels do not lie on the same ellipse,
                // or that the estimates of the tangents were inaccurate.  In either case, we discard the 
                // parameters and choose a new pixel triplet.
                if ((a*c - b*b) <= 0.0) {
                    continue;
                }
                // Convert a, b, and c to semi major axis r1, semi minor axis r2, and orientation angle theta
                // Convert a*x**2 + 2*b*x*y + c*y**2 = 1 to
                // (y*sin(theta) + x*cos(theta))**2/r1**2 + (y*cos(theta) - x*sin(theta))**2/r2**2 = 1
                // If a = c, this is a circle with r1 = a, r2 = a, and theta = 0.
                // y**2*(sin(theta))**2/r1**2 + 2*x*y*cos(theta)*sin(theta)/r1**2 + x**2*(cos(theta))**2/r1**2
                // + y**2*(cos(theta))**2/r2**2 - 2*x*y*cos(theta)*sin(theta)/r2**2 + x**2*(sin(theta))**2/r2**2 = 1
                // a = (cos(theta))**2/r1**2 + (sin(theta))**2/r2**2
                // c = (sin(theta))**2/r1**2 + (cos(theta))**2/r2**2
                // b = cos(theta)*sin(theta)*(1/r1**2 - 1/r2**2) = (1/2)*sin(2*theta)*(1/r1*2 - 1/r2**2)
                // a + c = 1/r1**2 + 1/r2**2
                // a - c = ((cos(theta))**2 - (sin(theta))**2)*(1/r1**2 - 1/r2**2)
                // a - c = (cos(2*theta)) * (1/r1*2 - 1/r2**2)
                // b/(a - c)= tan(2*theta)/2
                // tan(2*theta) = (2*b)/(a - c)
                // 2*theta = arctan((2*b)/(a - c))
                // theta = 0.5 * arctan((2*b)/(a - c))
                // sin(2*theta) = (2*b)/(1/r1**2 - 1/r2**2)
                // cos(2*theta) = (a - c)/(1/r1**2 - 1/r2**2)
                // 1 = (4*b**2 + (a - c)**2)/((1/r1**2 - 1/r2**2)**2)
                // If r1 > r2, then (1/r1**2 - 1/r2**2) < 0 so
                // 1/r1**2 - 1/r2**2 = -sqrt(4*b**2 + (a - c)**2)
                // 1/r2**2 = a + c - 1/r1**2
                // 2/r1**2 - a - c = -sqrt(4*b**2 + (a - c)**2)
                // 2/r1**2 = a + c - sqrt(4*b**2 + (a - c)**2)
                // r1**2/2 = 1/(a + c - sqrt(4*b**2 + (a - c)**2))
                // r1 = sqrt(2)/sqrt(a + c - sqrt(4*b**2 + (a - c)**2))
                // 1/r2**2 = a + c - 1/r1**2
                // 1/r2**2 = (2*a + 2*c - (a + c - sqrt(4*b**2 + (a - c)**2)))/2
                // 1/r2**2 = (a + c + sqrt(4*b**2 + (a - c)**2))/2
                // r2 = sqrt(2)/sqrt(a + c + sqrt(4*b**2 + (a - c)**2))
                if (a == c) {
                    // Circle
                    r1 = a;
                    r2 = a;
                    theta = 0.0;
                }
                else {
                    aminusc = a - c;
                    twob = 2.0*b;
                    var = Math.sqrt(twob*twob + aminusc*aminusc);
                    aplusc = a + c;
                    r1 = sqr2/Math.sqrt(aplusc - var);
                    r2 = sqr2/Math.sqrt(aplusc + var);
                    theta = 0.5 * Math.atan2(twob, aminusc);
                }
                pointSetsAcquired++;
            } // while (pointSetsAcquired < pointSetsRequired)
        } // while ((ellipsesFound < numEllipses) && (ellipseFindCycles < maxEllipseFindCycles))
        
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
        
        // Create a dialog with numEllipsesFound x0Array[i], y0Array[i], radArray[i] and
        // countArray[i] values, where the user will select a check box to have the selected circle drawn.
        selectedEllipse = new boolean[numEllipsesFound];
        
        //choice = new JDialogHoughCircleChoice(ViewUserInterface.getReference().getMainFrame(), x0Array,
                 //xDimSource, y0Array, yDimSource, radArray, maxRad, countArray, selectedCircle);
        
        //if (!choice.okayPressed() ) {
            //setCompleted(false);
            //return;
        //}
        
        // Draw selected elipses
        for (i = 0; i < numEllipsesFound; i++) {
            //if (selectedEllipse[i]) {
                
           // } // if (selectedEllipse[i])
        } // for (i = 0; i < numEllipsesFound; i++)
        
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


        historyString = new String("HoughEllipse(" + String.valueOf(minCoverage) + ", " +
                                   String.valueOf(sidePointsForTangent) + ", " +
                                   String.valueOf(maxPixelDiff) + ", " +
                                   String.valueOf(maxDegreesDiff) + ", " + 
                                   String.valueOf(minPointDistance) + ", " +
                                   String.valueOf(maxPointDistance) + ", " +
                                   String.valueOf(pointSetsRequired) + ", " +
                                   String.valueOf(countThreshold) + ", " +
                                   String.valueOf(ellipseRangeTolerance) + ", " +
                                   String.valueOf(maxAxesRatio) + ", " + 
                                   String.valueOf(maxBufferSize) + ")\n");
    }
}
