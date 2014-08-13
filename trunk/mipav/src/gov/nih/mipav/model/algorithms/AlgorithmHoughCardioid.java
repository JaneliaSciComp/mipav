package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;

/**
 *  This Hough transform uses (xi, yi) points in the original image space to generate x0, y0, rad points in the Hough
 *  transform.  This Hough transform module only works with binary images.   Before it is used the user must 
 *  compute the gradient of an image and threshold it to obtain a binary image.  Noise removal and thinning should also
 *  be performed, if necessary, before this program is run. 
 *  
 *  The user is asked for the number of x0 bins, y0 bins, theta0 bins, rad bins, and number of cardioids.  The default size for x0 is 
 *  min(512, image.getExtents()[0]).  The default size for y0 is min(512, image.getExtents()[1]).
 *  The default size for rad is min(512, max(image.getExtents()[0], image.getExtents()[1]).
 *  The default number of cardioids is 1. The program generates a Hough transform of the source image using the basic
 *  equations:
 *  theta = atan2(y - d2, x - d1).
 *  Calculate d3 = sqrt((x - d1)**2 + (y - d2)**2)/(1 - cos(theta + theta0)) if theta != -theta0
 *  In general:
 *  sqrt((x - x0)**2 + (y - y0)**2) = rad*(1 - cos(theta + theta0))
 *  ((x-x0)**2 + (y-y0)**2 - rad*((x-x0)*cos*(theta0) - (y-y0)*sin(theta0)) = rad*sqrt((x-x0)**2 + (y-y0)**2)
 *  dy/dx = (-2*(x-x0) + rad*cos(theta0) + rad*(x-x0)/sqrt((x-x0)**2 + (y-y0)**2))/
 *          (2*(y-y0) - rad*sin(theta0) - rad*(y-y0)/sqrt((x-x0)**2 + (y-y0)**2))
 *  x = x0 + rad*cos(theta)*(1 - cos(theta + theta0))
 *    = x0 + rad(-0.5*cos(theta0) + cos(theta) - 0.5*cos(2*theta + theta0))
 *  y = y0 + rad*sin(theta)*(1 - cos(theta + theta0))
 *    = y0 + rad*(0.5*sin(theta0) + sin(theta) -0.5*sin(2*theta + theta0))
 *  dy/dx = dy/dtheta/dx/dtheta = (-cos(2*theta + theta0) + cos(theta))/(sin(2*theta + theta0) - sin(theta))
 *        = tan((1/2)*(3*theta + theta0))
 *  All cusp chords are of length 2 * rad.
 *  The tangents to the endpoints of a cusp chord are perpindicular.
 *  Every slope value occurs 3 times.
 *  If 3 points have parallel tangents, the lines from the cusp to these 3 points make equal angles
 *  of 2*PI/3 at the cusp.
 *  For cusp on the left:
 *  sqrt((x - x0)**2 + (y - y0)**2) = rad*(1 + cos(theta)).
 *  x = x0 + (rad/2)*(1 + 2*cos(theta) + cos(2*theta))) = x0 + rad*cos(theta)*(1 + cos(theta))
 *  y = y0 + (rad/2)*(2*sin(theta) + sin(2*theta)) = y0 + rad*sin(theta)*(1 + cos(theta))
 *  For cusp on the right:
 *  sqrt((x - x0)**2 + (y - y0)**2) = rad*(1 - cos(theta)).
 *  x = x0 + (rad/2)*(-1 + 2*cos(theta) - cos(2*theta)))  = x0 + rad*cos(theta)*(1 - cos(theta))
 *  y = y0 + (rad/2)*(2*sin(theta) - sin(2*theta)) = y0 + rad*sin(theta)*(1 - cos(theta))
 *  For cusp on top:
 *  sqrt((x - x0)**2 + (y - y0)**2) = rad*(1 + sin(theta)).
 *  x = x0 + (rad/2)*(2*cos(theta) + sin(2*theta))) = x0 + rad*cos(theta)*(1 + sin(theta))
 *  y = y0 + (rad/2)*(1 + 2*sin(theta) - cos(2*theta)) = y0 + rad*sin(theta)*(1 + sin(theta))
 *  For cusp on bottom:
 *  sqrt((x - x0)**2 + (y - y0)**2) = rad*(1 - sin(theta)).
 *  x = x0 + (rad/2)*(2*cos(theta) - sin(2*theta)))  = x0 + rad*cos(theta)*(1 - sin(theta))
 *  y = y0 + (rad/2)*(-1 + 2*sin(theta) + cos(2*theta)) = y0 + rad*sin(theta)*(1 - sin(theta))
 *  The program finds the cardioids containing the largest number of points.
 *  The program produces a dialog which allows the user to select which cardioids should be drawn.
 *  
 *  The Hough transform for the entire image is generated a separate time to find each cardioid.
 *  For each (xi, yi) point in the original image not having a value of zero, calculate the first dimension value d1 = 
 *  j * (xDim - 1)/(x0 - 1), with j = 0 to x0 - 1.  Calculate the second dimension value d2 = k * (yDim - 1)/(y0 - 1),
 *  with k = 0 to y0 - 1. 
 *  Calculate theta = atan2(y - d2, x - d1).
 *  Calculate d3 = sqrt((x - d1)**2 + (y - d2)**2)/(1 - cos(theta + theta0)) if theta != -theta0
 *  Don't calculate d3 if theta = -theta0.
 *  d3 goes from 0 to maxRad = max(xDim-1, yDim-1)/2.0.  s3 is the dimension 3 scaling factor.
 *  s3 * (rad - 1) = maxRad.
 *  s3 = maxRad/(rad - 1)
 *  n = d3*(rad - 1)/maxRad.
 *  Only calculate the Hough transform for d3 <= maxRad.
 *  
 *  Find the peak point in the x0, y0, rad Hough transform space.
 *  Put the values for this peak point in x0Array[c], y0Array[c], radArray[c], and
 *  countArray[c].
 *  
 *  If more cardioids are to be found, then zero the houghBuffer and run through the
 *  same Hough transform a second time, but on this second run instead of incrementing
 *  the Hough buffer, zero the values in the source buffer that contributed to the peak
 *  point in the Hough buffer. So on the next run of the Hough transform the source points that
 *  contributed to the Hough peak value just detected will not be present.
 *  
 *  Create a dialog with numCardioidsFound typeArray[i], x0Array[i], y0Array[i], radArray[i], and
 *  countArray[i] values, where the user will select a check box to have that cardioid drawn.
 *  
 *  References: 1.) Digital Image Processing, Second Edition by Richard C. Gonzalez and Richard E. Woods, Section 10.2.2
 *  Global Processing via the Hough Transform, Prentice-Hall, Inc., 2002, pp. 587-591.
 *  
 *  2.) Shape Detection in Computer Vision Using the Hough Transform by V. F. Leavers, Springer-Verlag, 1992.
 * 
 */
public class AlgorithmHoughCardioid extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------
    // Number of dimension 1 bins in Hough transform space
    private int x0;
    
    // Number of dimension 2 bins in Hough transform space
    private int y0;
    
    // number of dimension 3 bins in Hough transform space
    private int rad;
    
    // number of cardioids to be found
    private int numCardioids;
    
    private ModelImage testImage;
    
    // Maximum number of points to take from each side of a point on a curve in determining a tangent
    // If only 1 point is available on each side, simply use average of slopes to each of the
    // neigboring points.  
    private int sidePointsForTangent;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmHoughCardioid - default constructor.
     */
    public AlgorithmHoughCardioid() { }

    /**
     * AlgorithmHoughCardioid.
     *
     * @param  destImg  Image with lines filled in
     * @param  srcImg   Binary source image that has lines with gaps
     * @param  x0       number of dimension 1 bins in Hough transform space
     * @param  y0       number of dimension 2 bins in Hough transform space
     * @param  rad   number of dimension 3 bins in Hough transform space
     * @param  numCardioids number of cardioids to be found
     * @param  sidePointsForTangent  Maximum number of points to take from each side of a point on a curve
     *                               in determining the tangent
     */
    public AlgorithmHoughCardioid(ModelImage destImg, ModelImage srcImg,
    		                      int x0, int y0, 
    		                      int rad, int numCardioids, int sidePointsForTangent) {
        super(destImg, srcImg);
        this.x0 = x0;
        this.y0 = y0;
        this.rad = rad;
        this.numCardioids = numCardioids;
        this.sidePointsForTangent = sidePointsForTangent;
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

        int i, j, k, m, c;
        int index, indexDest;
        
        int houghSlice;
        byte[] srcBuffer;
        float radBuffer[];
        double theta0Buffer[];
        int[] houghBuffer;
        double theta;
        double theta0;
        double d1Array[];
        double d2Array[];
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
        int numCardioidsFound;
        double x0Array[];
        double y0Array[];
        double radArray[];
        double theta0Array[];
        int countArray[];
        boolean selectedCardioid[];
        JDialogHoughCardioidChoice choice;
        byte value = 0;
        int maxCardioidPoints;
        double maxRad;
        int x0y0;
        double theta1 = 7.0 * Math.PI/4.0;
        //double xSum;
        //double xSum2;
        //double xSum3;
        //double ySum;
        //double ySum2;
        //double ySum3;
        //int radCount;
        double num;
        int pruningPix;
        boolean entireImage;
        AlgorithmMorphology2D algoMorph2D;
        int neighbors;
        int neigh0;
        int neigh1;
        int neigh2;
        int neigh3;
        int neigh4;
        int neigh5;
        int neigh6;
        int neigh7;
        int numPoints;
        int endPoints;
        int neighbor1[];
        int neighbor2[];
        int numOpenCurves;
        boolean foundArray[];
        int openStart[];
        int openLength[];
        int indexArray[];
        float slopeArray[];
        float interceptArray[];
        int indexPtr = 0;
        int startPtr;
        int nextPoint;
        float xArray[];
        float yArray[];
        int n;
        int presentSidePoints;
        float tangentX;
        float tangentY;
        float xPoints[];
        float yPoints[];
        float xpc;
        float ypc;
        double xSqSum;
        double ySqSum;
        double xySum;
        double x1t;
        double x2t;
        double y1t;
        double y2t;
        double var;
        double d1;
        double d2;
        double slope;
        int numClosedCurves;
        int closedLength;
        int endPtr;
        int startWrapPoints;
        int endWrapPoints;

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        

        fireProgressStateChanged(srcImage.getImageName(), "Hough cardioid ...");

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sourceSlice = xDim * yDim; 
        maxRad = Math.max(xDim - 1, yDim - 1)/2.0;

        x0y0 = x0 * y0;
        houghSlice = x0y0 * rad;
        srcBuffer = new byte[sourceSlice];

        try {
            srcImage.exportData(0, sourceSlice, srcBuffer);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on srcImage.exportData");

            setCompleted(false);

            return;
        }
        
        for (i = 0; i < sourceSlice; i++) {
            if (srcBuffer[i] != 0) {
                value = srcBuffer[i];
                break;
            }
        }
        
        if (test) {
            for (y = 0; y < yDim; y++) {
                offset = y * xDim;
                for (x = 0; x < xDim; x++) {
                    index = offset + x; 
                    srcBuffer[index] = 0;
                } // for (x = 0; x < xDim; x++)
            } // for (y = 0; y < yDim; y++)
            
            // left
            //xCenter = (xDim-1)/4.0;
            //yCenter = (yDim-1)/2.0;
            // right 
            //xCenter = 3*(xDim-1)/4.0;
            //yCenter = (yDim-1)/2.0;
            // top
            //xCenter = (xDim-1)/2.0;
            //yCenter = (yDim-1)/4.0;
            // bottom
            //xCenter = (xDim-1)/2.0;
            //yCenter = 3*(yDim-1)/4.0;
            xCenter = 3*(xDim-1)/4.0;
            yCenter = 3*(yDim-1)/4.0;
            radius = 40.0;
            radius2 = 60.0;
            radius3 = 80.0;
            //xSum = 0.0;
            //ySum = 0.0;
            //xSum2 = 0.0;
            //ySum2 = 0.0;
            //xSum3 = 0.0;
            //ySum3 = 0.0;
            for (i = 0; i < 7200; i++) {
            	// left
                /*theta = i * Math.PI/10.0;
                x = (int)Math.round(xCenter + radius * (1.0 + 2.0 * Math.cos(theta) + Math.cos(2.0*theta)));
                y = (int)Math.round(yCenter + radius * (2.0 * Math.sin(theta) + Math.sin(2.0*theta)));
                index = x + y * xDim;
                srcBuffer[index] = 1;
                xSum = xSum + x;
                ySum = ySum + y;
                x = (int)Math.round(xCenter + radius2 * (1.0 + 2.0 * Math.cos(theta) + Math.cos(2.0*theta)));
                y = (int)Math.round(yCenter + radius2 * (2.0 * Math.sin(theta) + Math.sin(2.0*theta)));
                index = x + y * xDim;
                srcBuffer[index] = 1;
                xSum2 = xSum2 + x;
                ySum2 = ySum2 + y;
                x = (int)Math.round(xCenter + radius3 * (1.0 + 2.0 * Math.cos(theta) + Math.cos(2.0*theta)));
                y = (int)Math.round(yCenter + radius3 * (2.0 * Math.sin(theta) + Math.sin(2.0*theta)));
                index = x + y * xDim;
                srcBuffer[index] = 1;
                xSum3 = xSum3 + x;
                ySum3 = ySum3 + y;*/
                // right
                /*theta = i * Math.PI/10.0;
                x = (int)Math.round(xCenter + radius * (-1.0 + 2.0 * Math.cos(theta) - Math.cos(2.0*theta)));
                y = (int)Math.round(yCenter + radius * (2.0 * Math.sin(theta) - Math.sin(2.0*theta)));
                index = x + y * xDim;
                srcBuffer[index] = 1;
                x = (int)Math.round(xCenter + radius2 * (-1.0 + 2.0 * Math.cos(theta) - Math.cos(2.0*theta)));
                y = (int)Math.round(yCenter + radius2 * (2.0 * Math.sin(theta) - Math.sin(2.0*theta)));
                index = x + y * xDim;
                srcBuffer[index] = 1;
                x = (int)Math.round(xCenter + radius3 * (-1.0 + 2.0 * Math.cos(theta) - Math.cos(2.0*theta)));
                y = (int)Math.round(yCenter + radius3 * (2.0 * Math.sin(theta) - Math.sin(2.0*theta)));
                index = x + y * xDim;
                srcBuffer[index] = 1;*/
            	// top
                /*theta = i * Math.PI/10.0;
                x = (int)Math.round(xCenter + radius * (2.0 * Math.cos(theta) + Math.sin(2.0*theta)));
                y = (int)Math.round(yCenter + radius * (1.0 + 2.0 * Math.sin(theta) - Math.cos(2.0*theta)));
                index = x + y * xDim;
                srcBuffer[index] = 1;
                x = (int)Math.round(xCenter + radius2 * (2.0 * Math.cos(theta) + Math.sin(2.0*theta)));
                y = (int)Math.round(yCenter + radius2 * (1.0 + 2.0 * Math.sin(theta) - Math.cos(2.0*theta)));
                index = x + y * xDim;
                srcBuffer[index] = 1;
                x = (int)Math.round(xCenter + radius3 * (2.0 * Math.cos(theta) + Math.sin(2.0*theta)));
                y = (int)Math.round(yCenter + radius3 * (1.0 + 2.0 * Math.sin(theta) - Math.cos(2.0*theta)));
                index = x + y * xDim;
                srcBuffer[index] = 1;*/
                // bottom
                /*theta = i * Math.PI/10.0;
                x = (int)Math.round(xCenter + radius * (2.0 * Math.cos(theta) - Math.sin(2.0*theta)));
                y = (int)Math.round(yCenter + radius * (-1.0 + 2.0 * Math.sin(theta) + Math.cos(2.0*theta)));
                index = x + y * xDim;
                srcBuffer[index] = 1;
                x = (int)Math.round(xCenter + radius2 * (2.0 * Math.cos(theta) - Math.sin(2.0*theta)));
                y = (int)Math.round(yCenter + radius2 * (-1.0 + 2.0 * Math.sin(theta) + Math.cos(2.0*theta)));
                index = x + y * xDim;
                srcBuffer[index] = 1;
                x = (int)Math.round(xCenter + radius3 * (2.0 * Math.cos(theta) - Math.sin(2.0*theta)));
                y = (int)Math.round(yCenter + radius3 * (-1.0 + 2.0 * Math.sin(theta) + Math.cos(2.0*theta)));
                index = x + y * xDim;
                srcBuffer[index] = 1;*/
                theta = i * Math.PI/3600.0;
                /*x = (int)Math.round(xCenter + radius * Math.cos(theta)*(1 - Math.cos(theta+ theta1)));
                y = (int)Math.round(yCenter + radius * Math.sin(theta)*(1 - Math.cos(theta+ theta1)));
                index = x + y * xDim;
                srcBuffer[index] = 1;
                x = (int)Math.round(xCenter + radius2 * Math.cos(theta)*(1 - Math.cos(theta+ theta1)));
                y = (int)Math.round(yCenter + radius2 * Math.sin(theta)*(1 - Math.cos(theta+ theta1)));
                index = x + y * xDim;
                srcBuffer[index] = 1;*/
                x = (int)Math.round(xCenter + radius3 * Math.cos(theta)*(1 - Math.cos(theta+ theta1)));
                y = (int)Math.round(yCenter + radius3 * Math.sin(theta)*(1 - Math.cos(theta+ theta1)));
                index = x + y * xDim;
                srcBuffer[index] = 1;
            }
            testImage = new ModelImage(ModelStorageBase.BYTE, srcImage.getExtents(), "Hough Cardioid Test Image");
            try {
                testImage.importData(0, srcBuffer, true);
            }
            catch (IOException e) {
                MipavUtil.displayError("IOException " + e + " on testImage.importData");

                setCompleted(false);

                return;
            }
            new ViewJFrameImage(testImage);
            // left
            /*xSum = xSum/20.0;
            ySum = ySum/20.0;
            radSum = 0.0;
            radCount = 0;
            for (i = 0; i < 20; i++) {
                theta = i * Math.PI/10.0;
                x = (int)Math.round(xCenter + radius * (1.0 + 2.0 * Math.cos(theta) + Math.cos(2.0*theta)));
                y = (int)Math.round(yCenter + radius * (2.0 * Math.sin(theta) + Math.sin(2.0*(theta))));
                if (Math.abs(theta) != Math.PI) {
                    radSum = radSum + Math.sqrt((x - xSum)*(x - xSum) + (y - ySum)*(y - ySum))/(1.0 + Math.cos(theta));
                    radCount++;
                }
            }
            radSum = radSum/radCount;
            System.out.println(" x = " + xSum + " y = " + ySum + " theta0 = " + theta0 + " radius = " + radSum);
            
            xSum2 = xSum2/20.0;
            ySum2 = ySum2/20.0;
            radSum = 0.0;
            radCount = 0;
            for (i = 0; i < 20; i++) {
                theta = i * Math.PI/10.0;
                x = (int)Math.round(xCenter + radius2 * (1.0 + 2.0 * Math.cos(theta) + Math.cos(2.0*theta)));
                y = (int)Math.round(yCenter + radius2 * (2.0 * Math.sin(theta) + Math.sin(2.0*theta)));
                if (Math.abs(theta) != Math.PI) {
                    radSum = radSum + Math.sqrt((x - xSum2)*(x - xSum2) + (y - ySum2)*(y - ySum2))/(1.0 + Math.cos(theta));
                    radCount++;
                }
            }
            radSum = radSum/radCount;
            System.out.println(" x2 = " + xSum2 + " y2 = " + ySum2 + "theta0 = " + theta0 + " radius2 = " + radSum);
            
            xSum3 = xSum3/20.0;
            ySum3 = ySum3/20.0;
            radSum = 0.0;
            radCount = 0;
            for (i = 0; i < 20; i++) {
                theta = i * Math.PI/10.0;
                x = (int)Math.round(xCenter + radius3 * (1.0 + 2.0 * Math.cos(theta) + Math.cos(2.0*theta)));
                y = (int)Math.round(yCenter + radius3 * (2.0 * Math.sin(theta) + Math.sin(2.0*theta)));
                if (Math.abs(theta) != Math.PI) {
                    radSum = radSum + Math.sqrt((x - xSum3)*(x - xSum3) + (y - ySum3)*(y - ySum3))/(1.0 + Math.cos(theta));
                    radCount++;
                }
            }
            radSum = radSum/radCount;
            System.out.println(" x3 = " + xSum3 + " y3 = " + ySum3 + " theta0 = " + theta0 + " radius3 = " + radSum);*/
        }
        
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
        

        radBuffer = new float[houghSlice];
        houghBuffer = new int[houghSlice];
        theta0Buffer = new double[houghSlice];
        
        // Calculate d1Array and d2Array
        d1Array = new double[x0];
        d2Array = new double[y0];
        for (i = 0; i < x0; i++) {
            d1Array[i] = ((double)(i * (xDim - 1)))/((double)(x0 - 1));
        }
        for (i = 0; i < y0; i++) {
            d2Array[i] = ((double)(i * (yDim - 1)))/((double)(y0 - 1));
        }
        d3Scale = ((double)(rad - 1))/maxRad;
        maxCardioidPoints = (int)Math.ceil(2.0 * Math.PI * maxRad);
        
        x0Array = new double[numCardioids];
        y0Array = new double[numCardioids];
        radArray = new double[numCardioids];
        theta0Array = new double[numCardioids];
        countArray = new int[numCardioids];
        numCardioidsFound = 0;
        
        for (c = 0; c < numCardioids; c++) {
            // Calculate the Hough transform
            fireProgressStateChanged("Calculating Hough cardioid " + String.valueOf(c+1));
           
            for (i = 0; i < numPoints; i++) {
        		 if (indexArray[i] >= 0) {
	        		 y = indexArray[i] / xDim;
	        		 x = indexArray[i] % xDim;
	        		 slope = slopeArray[i];
	                 for (j = 0; j < x0; j++) {
	                    for (k = 0; k < y0; k++) {
	                    	theta = Math.atan2(y - d2Array[k], x - d1Array[j]);
	                        num = Math.sqrt((x - d1Array[j])*(x - d1Array[j]) + (y - d2Array[k])*(y - d2Array[k]));
	                        theta0 = 2.0 * Math.atan(slope) - 3.0 * theta;
	                        while (theta0 > Math.PI) {
	                        	theta0 = theta0 - 2.0 * Math.PI;
	                        }
	                        while (theta0 < -Math.PI) {
	                        	theta0 = theta0 + 2.0 * Math.PI;
	                        }
	                    	if (theta != -theta0) {
	                    	    d3 = num/(1.0 - Math.cos(theta + theta0));	
	                    	}
	                    	else {
	                    		d3 = Double.MAX_VALUE;
	                    	}
	                        if (d3 <= maxRad) {
	                            m = (int)Math.round(d3*d3Scale);
	                            indexDest = j + k * x0 + m * x0y0;
	                            houghBuffer[indexDest]++;
	                            radBuffer[indexDest] += d3;
	                            theta0Buffer[indexDest] += theta0;
	                        }
	                    } // for (k = 0; k < y0; k++)
	                } // for (j = 0; j < x0; j++ 
        		} // if (indexArray[i] >= 0)
        	} // for (i = 0; i < numPoints; i++)     
           
            // Find up to cell with the highest counts
            // Obtain the x0, y0, rad, and count values of this cardioid
            fireProgressStateChanged("Finding Hough peak cardioid " + String.valueOf(c+1));
            
            largestValue = 0;
            largestIndex = -1;
            for (j = 0; j < houghSlice; j++) {
                if (houghBuffer[j] > largestValue) {
                    largestValue = houghBuffer[j];
                    largestIndex = j;
                }
            } // for (j = 0; j < houghSlice; j++)
            if (largestIndex == -1) {
                break;
            }
            
            numCardioidsFound++;
            x0Array[c] = largestIndex % x0;
            x0Array[c] = x0Array[c] * ((double)(xDim - 1))/((double)(x0 - 1));
            y0Array[c] = (largestIndex % x0y0)/x0;
            y0Array[c] = y0Array[c] * ((double)(yDim - 1))/((double)(y0 - 1));
            radArray[c] = radBuffer[largestIndex]/largestValue;
            theta0Array[c] = theta0Buffer[largestIndex]/largestValue;
            countArray[c] = largestValue;
            
            if (c < numCardioids - 1) {
                // Zero hough buffer for next run
                for (i = 0; i < houghSlice; i++) {
                    houghBuffer[i] = 0;
                    radBuffer[i] = 0.0f;
                    theta0Buffer[i] = 0.0;
                }
                // zero all points in the source slice that contributed to this cardioid
                fireProgressStateChanged("Zeroing source cardioid " + String.valueOf(c+1));
               
	                
                for (i = 0; i < numPoints; i++) {
                	if (indexArray[i] >= 0) {
               		 y = indexArray[i] / xDim;
               		 x = indexArray[i] % xDim;
               		 slope = slopeArray[i];
                     for (j = 0; j < x0; j++) {
                        for (k = 0; k < y0; k++) {
                        	theta = Math.atan2(y - d2Array[k], x - d1Array[j]);
                            num = Math.sqrt((x - d1Array[j])*(x - d1Array[j]) + (y - d2Array[k])*(y - d2Array[k]));
                            theta0 = 2.0 * Math.atan(slope) - 3.0 * theta;
	                        while (theta0 > Math.PI) {
	                        	theta0 = theta0 - 2.0 * Math.PI;
	                        }
	                        while (theta0 < -Math.PI) {
	                        	theta0 = theta0 + 2.0 * Math.PI;
	                        }
                            if (theta != -theta0) {
                        	    d3 = num/(1.0 - Math.cos(theta + theta0));	
                        	}
                        	else {
                        		d3 = Double.MAX_VALUE;
                        	}
                            if (d3 <= maxRad) {
                                m = (int)Math.round(d3*d3Scale);
                                indexDest = j + k * x0 + m * x0y0;
                                if (indexDest == largestIndex) {
                                    indexArray[i] = -1;
                                }
                            }
                        } // for (k = 0; k < y0; k++)
                    } // for (j = 0; j < x0; j++)
                } // if (indexArray[i] >= 0)
            } // for (i = 0; i < numPoints; i++)     
                	
            } // if (c < numCardioids - 1)
        } // for (c = 0; c < numCardioids; c++)
        
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
        
        // Create a dialog with numCardioidFound x0Array[i], y0Array[i], radArray[i] and
        // countArray[i] values, where the user will select a check box to have the selected cardioid drawn.
        selectedCardioid = new boolean[numCardioidsFound];
        
        choice = new JDialogHoughCardioidChoice(ViewUserInterface.getReference().getMainFrame(), x0Array,
                 xDim, y0Array, yDim, radArray, maxRad, theta0Array, countArray, selectedCardioid);
        
        if (!choice.okayPressed() ) {
            setCompleted(false);
            return;
        }
        
        // Draw selected cardioids
        for (i = 0; i < numCardioidsFound; i++) {
            if (selectedCardioid[i]) {
                for (j = 0; j < maxCardioidPoints; j++) {
                    theta = j * 2.0 * Math.PI/maxCardioidPoints;
                    x = (int)Math.round(x0Array[i] + radArray[i] * Math.cos(theta)*(1 - Math.cos(theta + theta0Array[i])));
                    y = (int)Math.round(y0Array[i] + radArray[i] * Math.sin(theta)*(1 - Math.cos(theta + theta0Array[i])));
                    indexDest = x + y * xDim;
                    srcBuffer[indexDest] = value;
                }
            } // if (selectedCardioid[i])
        } // for (i = 0; i < numCardioidsFound; i++)
        
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
