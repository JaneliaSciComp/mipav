package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;

/**
 *  This Hough transform uses (xi, yi) points in the original image space to generate theta0, a0 points in the Hough
 *  transform.  xc and yc of the cusp are obtained from finding the point of maximum curvature.
 *  Hough space is used to check the (xcdim * ycdim) possibilities of xc-xchalf to xc + xchalf, yc - ychalf to yc + ychalf,
 *  where xchalf = (xcdim - 1)/2 and ychalf = (ycdim - 1)/2.  
 *  This Hough transform module only works with binary images.   Before it is used the user must 
 *  compute the gradient of an image and threshold it to obtain a binary image.  Noise removal and thinning should also
 *  be performed, if necessary, before this program is run. 
 *  
 *  The user is asked for the number of theta0 bins, a0 bins, side points for curvature, and number of cardioids.  
 *  The default size for theta0Num is 720 and the default size for a0Num is min(512, 2*max(image.getExtents()[0], image.getExtents()[1]).
 *  The default number of cardioids is 1. The program generates a Hough transform of the source image using the basic
 *  equations:
 *  theta = atan2(y - yc, x - xc) where xc and yc are the cusp points.
 *  Calculate d2 = sqrt((x - d1)**2 + (y - d2)**2)/(1 - cos(theta + theta0)) if theta != -theta0
 *  In general:
 *  sqrt((x - xc)**2 + (y - yc)**2) = a*(1 - cos(theta + theta0))
 *  ((x-xc)**2 + (y-yc)**2 - a*((x-xc)*cos*(theta0) - (y-yc)*sin(theta0)) = a*sqrt((x-xc)**2 + (y-yc)**2)
 *  dy/dx = (-2*(x-xc) + a*cos(theta0) + a*(x-xc)/sqrt((x-xc)**2 + (y-yc)**2))/
 *          (2*(y-yc) - a*sin(theta0) - a*(y-yc)/sqrt((x-xc)**2 + (y-yc)**2))
 *  x = xc + a*cos(theta)*(1 - cos(theta + theta0))
 *    = xc + a*(-0.5*cos(theta0) + cos(theta) - 0.5*cos(2*theta + theta0))
 *  y = yc + a*sin(theta)*(1 - cos(theta + theta0))
 *    = yc + a*(0.5*sin(theta0) + sin(theta) -0.5*sin(2*theta + theta0))
 *  dy/dx = dy/dtheta/dx/dtheta = (-cos(2*theta + theta0) + cos(theta))/(sin(2*theta + theta0) - sin(theta))
 *        = tan((1/2)*(3*theta + theta0))
 *  Every slope value occurs 3 times so we must use the second derivative to find which of the 3 theta values
 *  is correct.
 *  dy'/dtheta = -a*sin(theta) + 2a*sin(2*theta + theta0)
 *  d2y/dx2 = dy'/dtheta/dx/dtheta = 
 *  (-sin(theta) + 2*sin(2*theta + theta0))/(-sin(theta) + sin(2*theta + theta0))
 *  curvature(theta) = ((dx/dtheta)*(d2y/dtheta2) - (dy/dtheta)*(d2x/dtheta2))/((dx/dtheta)**2 + (dy/dtheta)**2)**1.5
 *  d2x/dtheta2 = -a*cos(theta) + 2*a*cos(2*theta + theta0)
 *  d2y/dtheta2 = -a*sin(theta) + 2*a*sin(2*theta + theta0)
 *  curvature(theta) = 3/(2*sqrt(2)*a*sqrt(1 - cos(theta+theta0))
 *  so the curvature is infinite at theta = -theta0.
 *  All cusp chords are of length 2 * a.
 *  The tangents to the endpoints of a cusp chord are perpindicular.
 *  If 3 points have parallel tangents, the lines from the cusp to these 3 points make equal angles
 *  of 2*PI/3 at the cusp.
 *  For cusp on the left:
 *  sqrt((x - xc)**2 + (y - yc)**2) = a*(1 + cos(theta)).
 *  x = xc + (a/2)*(1 + 2*cos(theta) + cos(2*theta))) = xc + a*cos(theta)*(1 + cos(theta))
 *  y = yc + (a/2)*(2*sin(theta) + sin(2*theta)) = yc + a*sin(theta)*(1 + cos(theta))
 *  For cusp on the right:
 *  sqrt((x - xc)**2 + (y - yc)**2) = a*(1 - cos(theta)).
 *  x = xc + (a/2)*(-1 + 2*cos(theta) - cos(2*theta)))  = xc + a*cos(theta)*(1 - cos(theta))
 *  y = yc + (a/2)*(2*sin(theta) - sin(2*theta)) = yc + a*sin(theta)*(1 - cos(theta))
 *  For cusp on top:
 *  sqrt((x - xc)**2 + (y - yc)**2) = a*(1 + sin(theta)).
 *  x = xc + (a/2)*(2*cos(theta) + sin(2*theta))) = xc + a*cos(theta)*(1 + sin(theta))
 *  y = yc + (a/2)*(1 + 2*sin(theta) - cos(2*theta)) = yc + a*sin(theta)*(1 + sin(theta))
 *  For cusp on bottom:
 *  sqrt((x - xc)**2 + (y - yc)**2) = a*(1 - sin(theta)).
 *  x = xc + (a/2)*(2*cos(theta) - sin(2*theta)))  = xc + a*cos(theta)*(1 - sin(theta))
 *  y = yc + (a/2)*(-1 + 2*sin(theta) + cos(2*theta)) = yc + a*sin(theta)*(1 - sin(theta))
 *  The program finds the cardioids containing the largest number of points.
 *  The program produces a dialog which allows the user to select which cardioids should be drawn.
 *  
 *  The Hough transform for the entire image is generated a separate time to find each cardioid.
 *  Calculate theta = atan2(y - yc, x - xc).
 *  Calculate d3 = sqrt((x - xc)**2 + (y - yc)**2)/(1 - cos(theta + theta0)) if theta != -theta0
 *  Don't calculate d2 if theta = -theta0.
 *  d2 goes from 0 to maxA = sqrt((xDim-1)**2 + (yDim-1)**2)/2.0.  s2 is the dimension 2 scaling factor.
 *  s2 * (a0 - 1) = maxA.
 *  s2 = maxA/(a0 - 1)
 *  m = d2*(a0 - 1)/maxA.
 *  Only calculate the Hough transform for d2 <= maxA.
 *  
 *  Find the peak point in the theta0, a0 Hough transform space.
 *  Put the values for this peak point in xcArray[c], ycArray[c], a0Array[c], and
 *  countArray[c].
 *  
 *  If more cardioids are to be found, then zero the houghBuffer and run through the
 *  same Hough transform a second time, but on this second run instead of incrementing
 *  the Hough buffer, zero the values in the source buffer that contributed to the peak
 *  point in the Hough buffer. So on the next run of the Hough transform the source points that
 *  contributed to the Hough peak value just detected will not be present.
 *  
 *  Create a dialog with numCardioidsFound xcArray[i], ycArray[i], a0Array[i], and
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
    private int theta0Num;
    
    // number of dimension 2 bins in Hough transform space
    private int a0Num;
    
    // number of cardioids to be found
    private int numCardioids;
    
    private ModelImage testImage;
    
    // Maximum number of points to take from each side of a point on a curve in determining a tangent
    // If only 1 point is available on each side, simply use average of slopes to each of the
    // neigboring points.  
    private int sidePointsForCurvature;


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
     * @param  theta0Num       number of dimension 1 bins in Hough transform space
     * @param  a0Num   number of dimension 2 bins in Hough transform space
     * @param  numCardioids number of cardioids to be found
     @param  sidePointsForCurvature  Maximum number of points to take from each side of a point on a curve
     *                               in determining the tangent

     */
    public AlgorithmHoughCardioid(ModelImage destImg, ModelImage srcImg,
    		                      int theta0Num, 
    		                      int a0Num, int numCardioids, int sidePointsForCurvature) {
        super(destImg, srcImg);
        this.theta0Num = theta0Num;
        this.a0Num = a0Num;
        this.numCardioids = numCardioids;
        this.sidePointsForCurvature = sidePointsForCurvature;
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
        int[] houghBuffer;
        double theta;
        double theta0;
        double d2;
        double d2Scale;
        boolean test = false;
        //double xCenter;
        //double yCenter;
        double xCenter2;
        double yCenter2;
        double xCenter3;
        double yCenter3;
        //double radius;
        double radius2;
        double radius3;
        int largestValue;
        int largestIndex;
        int numCardioidsFound;
        double xcArray[];
        double ycArray[];
        double a0Array[];
        double theta0Array[];
        int countArray[];
        boolean selectedCardioid[];
        JDialogHoughCardioidChoice choice;
        byte value = 0;
        int maxCardioidPoints;
        double maxA;
        double theta1 = 7.0 * Math.PI/4.0;
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
        float curvatureArray[];
        int indexPtr = 0;
        int startPtr;
        int nextPoint;
        float xArray[];
        float yArray[];
        int n;
        int presentSidePoints;
        float xPoints[];
        float yPoints[];
        int numClosedCurves;
        int closedLength;
        int endPtr;
        int startWrapPoints;
        int endWrapPoints;
        float xComponent;
        float yComponent;
        float maxCurvatureValue;
        int maxCurvatureIndex;
        int xc;
        int yc;
        int a0Index;
        int theta0Index;
        int xcdim;
        int ycdim;
        int xf;
        int yf;
        int xchalf;
        int ychalf;

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        

        fireProgressStateChanged(srcImage.getImageName(), "Hough cardioid ...");

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sourceSlice = xDim * yDim; 
        maxA = Math.sqrt((xDim - 1)*(xDim -1) + (yDim - 1)*(yDim - 1))/2.0;
        
        xcdim = 5;
        ycdim = 5;
        xchalf = (xcdim - 1)/2;
        ychalf = (ycdim - 1)/2;

        houghSlice = theta0Num * a0Num * xcdim * ycdim;
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
            xCenter2 = (xDim-1)/2.0;
            yCenter2 = (yDim-1)/2.0;
            xCenter3 = 3*(xDim-1)/4.0;
            yCenter3 = 3*(yDim-1)/4.0;
            //radius = 40.0;
            radius2 = 60.0;
            radius3 = 80.0;
            //xSum = 0.0;
            //ySum = 0.0;
            //xSum2 = 0.0;
            //ySum2 = 0.0;
            //xSum3 = 0.0;
            //ySum3 = 0.0;
            for (i = 0; i < 2000; i++) {
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
                theta = i * Math.PI/1000.0;
                /*x = (int)Math.round(xCenter + radius * Math.cos(theta)*(1 - Math.cos(theta+ theta1)));
                y = (int)Math.round(yCenter + radius * Math.sin(theta)*(1 - Math.cos(theta+ theta1)));
                index = x + y * xDim;
                srcBuffer[index] = 1;*/
                x = (int)Math.round(xCenter2 + radius2 * Math.cos(theta)*(1 - Math.cos(theta+ theta1)));
                y = (int)Math.round(yCenter2 + radius2 * Math.sin(theta)*(1 - Math.cos(theta+ theta1)));
                index = x + y * xDim;
                srcBuffer[index] = 1;
                x = (int)Math.round(xCenter3 + radius3 * Math.cos(theta)*(1 - Math.cos(theta+ theta1)));
                y = (int)Math.round(yCenter3 + radius3 * Math.sin(theta)*(1 - Math.cos(theta+ theta1)));
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
        
        // Skeletonize the binary image
        // Prune off branches with 2 or less pixels
        try {
        	destImage.importData(0, srcBuffer, true);
        }
        catch (IOException e) {
        	MipavUtil.displayError("IOException " + e + " on destImage.importData(0, srcBuffer, true)");
        	setCompleted(false);
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
        // With a user specified sidePointsForCurvature on each side of a point find the tangent line that
        // minimizes the sum of the squared distances from these side points to the tangent line 
        indexArray = new int[numPoints];
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
                presentSidePoints = Math.min(sidePointsForCurvature, n);
                presentSidePoints = Math.min(presentSidePoints, openLength[i] - 1 - n);
                xComponent = xArray[n+presentSidePoints] - 2 * xArray[n] + xArray[n - presentSidePoints];
                yComponent = yArray[n+presentSidePoints] - 2 * yArray[n] + yArray[n - presentSidePoints];
                curvatureArray[indexPtr++] = xComponent * xComponent + yComponent * yComponent;
            } // for (n = 1; n <= openLength[i] - 2; n++)
        } // for (i = 0; i < numOpenCurves; i++)
        openStart = null;
        openLength = null;
        xArray = null;
        yArray = null;
        
        // Find a position and length of closed curve
        numClosedCurves = 0;
        xPoints = new float[2*sidePointsForCurvature + 1];
        yPoints = new float[2*sidePointsForCurvature + 1];
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
                        // Put the tangent point at index sidePointsForCurvature in the
                        // center of the xPoints and yPoints array with sidePointsForCurvature points
                        // to each side.
                        startWrapPoints = Math.max(0, sidePointsForCurvature - n);
                        endWrapPoints =  Math.max(0, sidePointsForCurvature - (closedLength - 1 - n));
                        for (k = 0; k < startWrapPoints; k++) {
                            xPoints[k] = indexArray[endPtr - (startWrapPoints - k)] % xDim;
                            yPoints[k] = indexArray[endPtr - (startWrapPoints - k)] / xDim;
                        }
                        for (k = startWrapPoints, j = indexPtr - sidePointsForCurvature + startWrapPoints;
                             k < 2*sidePointsForCurvature + 1 - endWrapPoints; j++, k++) {
                            xPoints[k] = indexArray[j] % xDim;
                            yPoints[k] = indexArray[j] / xDim;
                        }
                        for (j = 0, k = 2*sidePointsForCurvature + 1 - endWrapPoints; k < 2*sidePointsForCurvature + 1; j++, k++) {
                            xPoints[k] = indexArray[startPtr + j] % xDim;
                            yPoints[k] = indexArray[startPtr + j] / xDim;
                        }
                        xComponent = xPoints[2*sidePointsForCurvature] - 2 * xPoints[sidePointsForCurvature] + xPoints[0];
                        yComponent = yPoints[2*sidePointsForCurvature] - 2 * yPoints[sidePointsForCurvature] + yPoints[0];
                        curvatureArray[indexPtr++] = xComponent * xComponent + yComponent * yComponent;
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
        
        houghBuffer = new int[houghSlice];
        
        d2Scale = ((double)(a0Num - 1))/maxA;
        maxCardioidPoints = (int)Math.ceil(2.0 * Math.PI * maxA);
        
        xcArray = new double[numCardioids];
        ycArray = new double[numCardioids];
        a0Array = new double[numCardioids];
        theta0Array = new double[numCardioids];
        countArray = new int[numCardioids];
        numCardioidsFound = 0;
        
        for (c = 0; c < numCardioids; c++) {
            // Calculate the Hough transform
            fireProgressStateChanged("Calculating Hough cardioid " + String.valueOf(c+1));
            maxCurvatureValue = -1.0f;
            maxCurvatureIndex = -1;
            for (i = 0; i < numPoints; i++) {
                if (indexArray[i] >= 0) {
                    if (curvatureArray[i] > maxCurvatureValue) {
                    	maxCurvatureValue = curvatureArray[i];
                    	maxCurvatureIndex = i;
                    }
                } // if (indexArray[i] >= 0)
            } // for (i = 0; i < numPoints; i++)
            yc = indexArray[maxCurvatureIndex] / xDim;
            xc = indexArray[maxCurvatureIndex] % xDim;
            
            for (i = 0; i < numPoints; i++) {
       		    if (indexArray[i] >= 0) {
	                y = indexArray[i] / xDim;
	        		x = indexArray[i] % xDim;
	        		for (yf = -ychalf; yf <= ychalf; yf++) {
		        		for (xf = -xchalf; xf <= xchalf; xf++) {
		                	theta = Math.atan2(y - (yc+yf), x - (xc+xf));
		                    num = Math.sqrt((x - (xc+xf))*(x - (xc+xf)) + (y - (yc+yf))*(y - (yc+yf)));
		                    for (j = 0; j < theta0Num; j++) {
		                    	theta0 = -Math.PI + j * 2 * Math.PI/theta0Num;
			                	if (theta != -theta0) {
			                	    d2 = num/(1.0 - Math.cos(theta + theta0));	
			                	}
			                	else {
			                		d2 = Double.MAX_VALUE;
			                	}
			                    if (d2 <= maxA) {
			                        m = (int)Math.round(d2*d2Scale);
			                        indexDest = j + m * theta0Num + (xf + xchalf) * theta0Num * a0Num +
			                        		(yf+ychalf) * theta0Num * a0Num * xcdim;
			                        houghBuffer[indexDest]++;
			                    }
		                    } // for (j = 0; j < theta0Num; j++) 
		        		} // for (xf = -xchalf; xf <= xchalf; xf++)
	        		} // for (yf = -ychalf; yf <= ychalf; yf++)
       		} // if (indexArray[i] >= 0)
       	} // for (i = 0; i < numPoints; i++)     

           
            // Find cell with the highest counts
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
            xf = (largestIndex % (theta0Num * a0Num * xcdim))/(theta0Num * a0Num) - xchalf;
            xcArray[c] = xc + xf;
            yf = largestIndex/(theta0Num * a0Num * xcdim) - ychalf;
            ycArray[c] = yc + yf;
            a0Index = (largestIndex % (theta0Num*a0Num))/theta0Num;
            a0Array[c] = a0Index *(maxA/(double)(a0Num -1));
            theta0Index = largestIndex % theta0Num;
            theta0Array[c] = -Math.PI + 2 * theta0Index * Math.PI/theta0Num;
            countArray[c] = largestValue;
            
            if (c < numCardioids - 1) {
                // Zero hough buffer for next run
                for (i = 0; i < houghSlice; i++) {
                    houghBuffer[i] = 0;
                }
                // zero all points in the source slice that contributed to this cardioid
                fireProgressStateChanged("Zeroing source cardioid " + String.valueOf(c+1));
               
                
                loop: for (i = 0; i < numPoints; i++) {
                	if (indexArray[i] >= 0) {
               		 y = indexArray[i] / xDim;
               		 x = indexArray[i] % xDim;
               		for (yf = -ychalf; yf <= ychalf; yf++) {
		        		for (xf = -xchalf; xf <= xchalf; xf++) {
		        			if ((x == xf + xc) && (y == yf + yc)) {
		        				indexArray[i] = -1;
		        				continue loop;
		        			}
		                	theta = Math.atan2(y - (yc+yf), x - (xc+xf));
		                    num = Math.sqrt((x - (xc+xf))*(x - (xc+xf)) + (y - (yc+yf))*(y - (yc+yf)));
		                    for (j = 0; j < theta0Num; j++) {
		                    	theta0 = -Math.PI + j * 2 * Math.PI/theta0Num;
			                	if (theta != -theta0) {
			                	    d2 = num/(1.0 - Math.cos(theta + theta0));	
			                	}
			                	else {
			                		d2 = Double.MAX_VALUE;
			                	}
			                    if (d2 <= maxA) {
			                        m = (int)Math.round(d2*d2Scale);
			                        indexDest = j + m * theta0Num + (xf + xchalf) * theta0Num * a0Num +
			                        		(yf+ychalf) * theta0Num * a0Num * xcdim;
			                        if (indexDest == largestIndex) {
			                        	indexArray[i] = -1;
			                        }
			                    }
		                    } // for (j = 0; j < theta0Num; j++) 
		        		} // for (xf = -xchalf; xf <= xchalf; xf++)
	        		} // for (yf = -ychalf; yf <= ychalf; yf++)
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
        
        // Create a dialog with numCardioidFound xcArray[i], ycArray[i], a0Array[i], theta0Array and
        // countArray[i] values, where the user will select a check box to have the selected cardioid drawn.
        selectedCardioid = new boolean[numCardioidsFound];
        
        choice = new JDialogHoughCardioidChoice(ViewUserInterface.getReference().getMainFrame(), xcArray,
                 xDim, ycArray, yDim, a0Array, maxA, theta0Array, countArray, selectedCardioid);
        
        if (!choice.okayPressed() ) {
            setCompleted(false);
            return;
        }
        
        // Draw selected cardioids
        for (i = 0; i < numCardioidsFound; i++) {
            if (selectedCardioid[i]) {
                for (j = 0; j < maxCardioidPoints; j++) {
                    theta = j * 2.0 * Math.PI/maxCardioidPoints;
                    x = (int)Math.round(xcArray[i] + a0Array[i] * Math.cos(theta)*(1 - Math.cos(theta + theta0Array[i])));
                    y = (int)Math.round(ycArray[i] + a0Array[i] * Math.sin(theta)*(1 - Math.cos(theta + theta0Array[i])));
                    if ((x >= 0) && (x < xDim) && (y >= 0) && (y < yDim)) {
                        indexDest = x + y * xDim;
                        srcBuffer[indexDest] = value;
                    }
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

