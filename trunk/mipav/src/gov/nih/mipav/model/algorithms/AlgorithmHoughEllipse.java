package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

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
    
    // Number of point triplets acquired before each ellipse find is performed
    private int pointSetsAcquired;
    
    // Maximum ratio of major axis to minor axis - default is 2.0;
    private double maxAxesRatio;
    
    // number of ellipses to be found
    private int numEllipses;
    
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
     * @param  pointSetsAcquired Number of point triplets acquired before each ellipse find is performed
     * @param  countThreshold Number of counts required to find an ellipse
     * @param  ellipseRangeTolerance Maximum pixel distance by which ellipse perimiter pixels can deviate from
     *                               the calculated space
     * @param  maxAxesRatio  Maximum ratio of major axis to minor axis
     * @param  numEllipses number of ellipses to be found
     * @param  maxBufferSize maximum Hough transform size in megabytes
     */
    public AlgorithmHoughEllipse(ModelImage destImg, ModelImage srcImg, double minCoverage, int sidePointsForTangent,
                                 double maxPixelDiff, double maxDegreesDiff, double minPointDistance,
                                 double maxPointDistance, int pointSetsAcquired, int countThreshold, 
                                 double ellipseRangeTolerance, double maxAxesRatio, int numEllipses,
                                 int maxBufferSize) {
        super(destImg, srcImg);
        this.minCoverage = minCoverage;
        this.sidePointsForTangent = sidePointsForTangent;
        this.maxPixelDiff = maxPixelDiff;
        this.maxDegreesDiff = maxDegreesDiff;
        this.minPointDistance = minPointDistance;
        this.maxPointDistance = maxPointDistance;
        this.pointSetsAcquired = pointSetsAcquired;
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

        int i, j, k, m, c;
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
        boolean test = false;
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
        int neighbor0[];
        int neighbor1[];
        int neighbor2[];
        int neighbor3[];
        int neighbor4[];
        int neighbor5[];
        int neighbor6[];
        int neighbor7[];
        int slope[];

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
            radius = 50.0;
            radius2 = 70.0;
            radius3 = 90.0;
            xSum = 0.0;
            ySum = 0.0;
            xSum2 = 0.0;
            ySum2 = 0.0;
            xSum3 = 0.0;
            ySum3 = 0.0;
            for (i = 0; i < 20; i++) {
                theta = i * Math.PI/10.0;
                x = (int)Math.round(xCenter + radius *Math.cos(theta));
                y = (int)Math.round(yCenter + radius * Math.sin(theta));
                index = x + y * xDim;
                srcBuffer[index] = 1;
                xSum = xSum + x;
                ySum = ySum + y;
                x = (int)Math.round(xCenter + radius2 *Math.cos(theta));
                y = (int)Math.round(yCenter + radius2 * Math.sin(theta));
                index = x + y * xDim;
                srcBuffer[index] = 1;
                xSum2 = xSum2 + x;
                ySum2 = ySum2 + y;
                x = (int)Math.round(xCenter + radius3 *Math.cos(theta));
                y = (int)Math.round(yCenter + radius3 * Math.sin(theta));
                index = x + y * xDim;
                srcBuffer[index] = 1;
                xSum3 = xSum3 + x;
                ySum3 = ySum3 + y;
            }
            xSum = xSum/20.0;
            ySum = ySum/20.0;
            radSum = 0.0;
            for (i = 0; i < 20; i++) {
                theta = i * Math.PI/10.0;
                x = (int)Math.round(xCenter + radius *Math.cos(theta));
                y = (int)Math.round(yCenter + radius * Math.sin(theta));
                radSum = radSum + Math.sqrt((x - xSum)*(x - xSum) + (y - ySum)*(y - ySum));
            }
            radSum = radSum/20.0;
            System.out.println(" x = " + xSum + " y = " + ySum + " radius = " + radSum);
            
            xSum2 = xSum2/20.0;
            ySum2 = ySum2/20.0;
            radSum = 0.0;
            for (i = 0; i < 20; i++) {
                theta = i * Math.PI/10.0;
                x = (int)Math.round(xCenter + radius2 *Math.cos(theta));
                y = (int)Math.round(yCenter + radius2 * Math.sin(theta));
                radSum = radSum + Math.sqrt((x - xSum2)*(x - xSum2) + (y - ySum2)*(y - ySum2));
            }
            radSum = radSum/20.0;
            System.out.println(" x2 = " + xSum2 + " y2 = " + ySum2 + " radius2 = " + radSum);
            
            xSum3 = xSum3/20.0;
            ySum3 = ySum3/20.0;
            radSum = 0.0;
            for (i = 0; i < 20; i++) {
                theta = i * Math.PI/10.0;
                x = (int)Math.round(xCenter + radius3 *Math.cos(theta));
                y = (int)Math.round(yCenter + radius3 * Math.sin(theta));
                radSum = radSum + Math.sqrt((x - xSum3)*(x - xSum3) + (y - ySum3)*(y - ySum3));
            }
            radSum = radSum/20.0;
            System.out.println(" x3 = " + xSum3 + " y3 = " + ySum3 + " radius3 = " + radSum);
        } // if (test)
        
        for (i = 0; i < sourceSlice; i++) {
            if (srcBuffer[i] != 0) {
                value = srcBuffer[i];
                break;
            }
        }
        
        numPoints = 0;
        for (i = 0; i < sourceSlice; i++) {
            if (srcBuffer[i] != 0) {
                numPoints++;
            }
        }
        
        indexArray = new int[numPoints];
        curve1 = new int[numPoints];
        curve2 = new int[numPoints];
        neighbor0 = new int[numPoints];
        neighbor1 = new int[numPoints];
        neighbor2 = new int[numPoints];
        neighbor3 = new int[numPoints];
        neighbor4 = new int[numPoints];
        neighbor5 = new int[numPoints];
        neighbor6 = new int[numPoints];
        neighbor7 = new int[numPoints];
        for (i = 0; i < numPoints; i++) {
            indexArray[i] = -1;
            curve1[i] = -1;
            curve2[i] = -1;
            neighbor0[i] = -1;
            neighbor1[i] = -1;
            neighbor2[i] = -1;
            neighbor3[i] = -1;
            neighbor4[i] = -1;
            neighbor5[i] = -1;
            neighbor6[i] = -1;
            neighbor7[i] = -1;
        }
        // Assign every nonzero point to a curve and find its tangent
        for (y = 0 ; y < yDim; y++) {
            offset = y * xDim;
            for (x = 0; x < xDim; x++) {
                index = offset + x;
                if (srcBuffer[index] != 0) {
                  indexArray[numPoints++] = index;
                  if (x > 0) {
                      if (srcBuffer[index-1] != 0) {
                          curve1[index] = curve1[index-1];
                          neighbor3[index] = index - 1;
                          neighbor4[index-1] = index;
                      }
                      else if (y > 0) {
                          if (srcBuffer[index - xDim - 1] != 0) {
                              curve1[index] = curve1[index - xDim - 1];
                              neighbor0[index] = index - xDim - 1;
                              neighbor7[index - xDim - 1] = index;
                          }
                      }
                  } // if (x > 0)
                  if (y > 0) {
                      if (srcBuffer[index - xDim] != 0) {
                          curve1[index] = curve1[index - xDim];
                          neighbor1[index] = index - xDim;
                          neighbor6[index - xDim] = index;
                      }
                      if (x < xDim - 1) {
                          if (srcBuffer[index - xDim + 1] != 0) {
                              if (curve1[index] == -1) {
                                  if (curve2[index - xDim + 1] != -1) {
                                      curve1[index] = curve2[index - xDim + 1];
                                  }
                                  else {
                                      curve1[index] = curve1[index - xDim + 1];
                                  }
                                  neighbor2[index] = index - xDim + 1;
                                  neighbor5[index - xDim + 1] = index;
                              }
                              else {
                                  // Two different curves intersect at this point
                                  curve2[index] = curve1[index - xDim +1];
                                  neighbor2[index] = index - xDim + 1;
                                  neighbor5[index - xDim + 1] = index;
                              }
                          }
                      }
                  } // if (y > 0)
               } // if (srcBuffer[index] != 0)
            } // for (x = 0; x < xDim; x++)
        } // for (y = 0; y < yDim; y++)
        
        
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
                                   String.valueOf(pointSetsAcquired) + ", " +
                                   String.valueOf(countThreshold) + ", " +
                                   String.valueOf(ellipseRangeTolerance) + ", " +
                                   String.valueOf(maxAxesRatio) + ", " + 
                                   String.valueOf(maxBufferSize) + ")\n");
    }
}
