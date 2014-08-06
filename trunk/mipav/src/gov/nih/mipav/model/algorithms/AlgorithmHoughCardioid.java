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
 *  In general:
 *  sqrt((x - x0)**2 + (y - y0)**2) = rad*(1 - cos(theta + theta0))
 *  x = x0 + rad*cos(theta)*(1 - cos(theta + theta0))
 *  y = y0 + rad*sin(theta)*(1 - cos(theta + theta0))
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
 *  Calculate d3 = sqrt((x - d1)**2 + (y - d2)**2)/(1 + cos(theta)) if theta != PI and theta != -PI for type = left.
 *  Don't calculate d3 if theta = PI or theta = -PI for type = left.
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
    
    private double theta0;

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
     * @param  leftSelected
     * @param  rightSelected
     * @param  topSelected
     * @param  bottomSelected
     * @param  x0       number of dimension 1 bins in Hough transform space
     * @param  y0       number of dimension 2 bins in Hough transform space
     * @param  rad   number of dimension 3 bins in Hough transform space
     * @param  numCardioids number of cardioids to be found
     */
    public AlgorithmHoughCardioid(ModelImage destImg, ModelImage srcImg, double theta0,
    		                      int x0, int y0, 
    		                      int rad, int numCardioids) {
        super(destImg, srcImg);
        this.theta0 = theta0;
        this.x0 = x0;
        this.y0 = y0;
        this.rad = rad;
        this.numCardioids = numCardioids;
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

        int xDimSource;

        int yDimSource;

        int sourceSlice;

        int i, j, k, m, c;
        int index, indexDest;
        
        int houghSlice;
        byte[] srcBuffer;
        float radBuffer[];
        int[] houghBuffer;
        double theta;
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

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        

        fireProgressStateChanged(srcImage.getImageName(), "Hough cardioid ...");

        xDimSource = srcImage.getExtents()[0];
        yDimSource = srcImage.getExtents()[1];
        sourceSlice = xDimSource * yDimSource; 
        maxRad = Math.max(xDimSource - 1, yDimSource - 1)/2.0;

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
            for (y = 0; y < yDimSource; y++) {
                offset = y * xDimSource;
                for (x = 0; x < xDimSource; x++) {
                    index = offset + x; 
                    srcBuffer[index] = 0;
                } // for (x = 0; x < xDimSource; x++)
            } // for (y = 0; y < yDimSource; y++)
            
            // left
            //xCenter = (xDimSource-1)/4.0;
            //yCenter = (yDimSource-1)/2.0;
            // right 
            //xCenter = 3*(xDimSource-1)/4.0;
            //yCenter = (yDimSource-1)/2.0;
            // top
            //xCenter = (xDimSource-1)/2.0;
            //yCenter = (yDimSource-1)/4.0;
            // bottom
            //xCenter = (xDimSource-1)/2.0;
            //yCenter = 3*(yDimSource-1)/4.0;
            xCenter = 3*(xDimSource-1)/4.0;
            yCenter = 3*(yDimSource-1)/4.0;
            radius = 40.0;
            radius2 = 60.0;
            radius3 = 80.0;
            //xSum = 0.0;
            //ySum = 0.0;
            //xSum2 = 0.0;
            //ySum2 = 0.0;
            //xSum3 = 0.0;
            //ySum3 = 0.0;
            for (i = 0; i < 20; i++) {
            	// left
                /*theta = i * Math.PI/10.0;
                x = (int)Math.round(xCenter + radius * (1.0 + 2.0 * Math.cos(theta) + Math.cos(2.0*theta)));
                y = (int)Math.round(yCenter + radius * (2.0 * Math.sin(theta) + Math.sin(2.0*theta)));
                index = x + y * xDimSource;
                srcBuffer[index] = 1;
                xSum = xSum + x;
                ySum = ySum + y;
                x = (int)Math.round(xCenter + radius2 * (1.0 + 2.0 * Math.cos(theta) + Math.cos(2.0*theta)));
                y = (int)Math.round(yCenter + radius2 * (2.0 * Math.sin(theta) + Math.sin(2.0*theta)));
                index = x + y * xDimSource;
                srcBuffer[index] = 1;
                xSum2 = xSum2 + x;
                ySum2 = ySum2 + y;
                x = (int)Math.round(xCenter + radius3 * (1.0 + 2.0 * Math.cos(theta) + Math.cos(2.0*theta)));
                y = (int)Math.round(yCenter + radius3 * (2.0 * Math.sin(theta) + Math.sin(2.0*theta)));
                index = x + y * xDimSource;
                srcBuffer[index] = 1;
                xSum3 = xSum3 + x;
                ySum3 = ySum3 + y;*/
                // right
                /*theta = i * Math.PI/10.0;
                x = (int)Math.round(xCenter + radius * (-1.0 + 2.0 * Math.cos(theta) - Math.cos(2.0*theta)));
                y = (int)Math.round(yCenter + radius * (2.0 * Math.sin(theta) - Math.sin(2.0*theta)));
                index = x + y * xDimSource;
                srcBuffer[index] = 1;
                x = (int)Math.round(xCenter + radius2 * (-1.0 + 2.0 * Math.cos(theta) - Math.cos(2.0*theta)));
                y = (int)Math.round(yCenter + radius2 * (2.0 * Math.sin(theta) - Math.sin(2.0*theta)));
                index = x + y * xDimSource;
                srcBuffer[index] = 1;
                x = (int)Math.round(xCenter + radius3 * (-1.0 + 2.0 * Math.cos(theta) - Math.cos(2.0*theta)));
                y = (int)Math.round(yCenter + radius3 * (2.0 * Math.sin(theta) - Math.sin(2.0*theta)));
                index = x + y * xDimSource;
                srcBuffer[index] = 1;*/
            	// top
                /*theta = i * Math.PI/10.0;
                x = (int)Math.round(xCenter + radius * (2.0 * Math.cos(theta) + Math.sin(2.0*theta)));
                y = (int)Math.round(yCenter + radius * (1.0 + 2.0 * Math.sin(theta) - Math.cos(2.0*theta)));
                index = x + y * xDimSource;
                srcBuffer[index] = 1;
                x = (int)Math.round(xCenter + radius2 * (2.0 * Math.cos(theta) + Math.sin(2.0*theta)));
                y = (int)Math.round(yCenter + radius2 * (1.0 + 2.0 * Math.sin(theta) - Math.cos(2.0*theta)));
                index = x + y * xDimSource;
                srcBuffer[index] = 1;
                x = (int)Math.round(xCenter + radius3 * (2.0 * Math.cos(theta) + Math.sin(2.0*theta)));
                y = (int)Math.round(yCenter + radius3 * (1.0 + 2.0 * Math.sin(theta) - Math.cos(2.0*theta)));
                index = x + y * xDimSource;
                srcBuffer[index] = 1;*/
                // bottom
                /*theta = i * Math.PI/10.0;
                x = (int)Math.round(xCenter + radius * (2.0 * Math.cos(theta) - Math.sin(2.0*theta)));
                y = (int)Math.round(yCenter + radius * (-1.0 + 2.0 * Math.sin(theta) + Math.cos(2.0*theta)));
                index = x + y * xDimSource;
                srcBuffer[index] = 1;
                x = (int)Math.round(xCenter + radius2 * (2.0 * Math.cos(theta) - Math.sin(2.0*theta)));
                y = (int)Math.round(yCenter + radius2 * (-1.0 + 2.0 * Math.sin(theta) + Math.cos(2.0*theta)));
                index = x + y * xDimSource;
                srcBuffer[index] = 1;
                x = (int)Math.round(xCenter + radius3 * (2.0 * Math.cos(theta) - Math.sin(2.0*theta)));
                y = (int)Math.round(yCenter + radius3 * (-1.0 + 2.0 * Math.sin(theta) + Math.cos(2.0*theta)));
                index = x + y * xDimSource;
                srcBuffer[index] = 1;*/
                theta = i * Math.PI/10.0;
                x = (int)Math.round(xCenter + radius * Math.cos(theta)*(1 - Math.cos(theta+ theta1)));
                y = (int)Math.round(yCenter + radius * Math.sin(theta)*(1 - Math.cos(theta+ theta1)));
                index = x + y * xDimSource;
                srcBuffer[index] = 1;
                x = (int)Math.round(xCenter + radius2 * Math.cos(theta)*(1 - Math.cos(theta+ theta1)));
                y = (int)Math.round(yCenter + radius2 * Math.sin(theta)*(1 - Math.cos(theta+ theta1)));
                index = x + y * xDimSource;
                srcBuffer[index] = 1;
                x = (int)Math.round(xCenter + radius3 * Math.cos(theta)*(1 - Math.cos(theta+ theta1)));
                y = (int)Math.round(yCenter + radius3 * Math.sin(theta)*(1 - Math.cos(theta+ theta1)));
                index = x + y * xDimSource;
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

        radBuffer = new float[houghSlice];
        houghBuffer = new int[houghSlice];
        
        // Calculate d1Array and d2Array
        d1Array = new double[x0];
        d2Array = new double[y0];
        for (i = 0; i < x0; i++) {
            d1Array[i] = ((double)(i * (xDimSource - 1)))/((double)(x0 - 1));
        }
        for (i = 0; i < y0; i++) {
            d2Array[i] = ((double)(i * (yDimSource - 1)))/((double)(y0 - 1));
        }
        d3Scale = ((double)(rad - 1))/maxRad;
        maxCardioidPoints = (int)Math.ceil(2.0 * Math.PI * maxRad);
        
        x0Array = new double[numCardioids];
        y0Array = new double[numCardioids];
        radArray = new double[numCardioids];
        countArray = new int[numCardioids];
        numCardioidsFound = 0;
        
        for (c = 0; c < numCardioids; c++) {
            // Calculate the Hough transform
            fireProgressStateChanged("Calculating Hough cardioid " + String.valueOf(c+1));
           
            for (y = 0; y < yDimSource; y++) {
                offset = y * xDimSource;
                for (x = 0; x < xDimSource; x++) {
                    index = offset + x;
                    if (srcBuffer[index] != 0) {
                        for (j = 0; j < x0; j++) {
                            for (k = 0; k < y0; k++) {
                            	theta = Math.atan2(y - d2Array[k], x - d1Array[j]);
                                num = Math.sqrt((x - d1Array[j])*(x - d1Array[j]) + (y - d2Array[k])*(y - d2Array[k]));
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
                                }
                            } // for (k = 0; k < y0; k++)
                        } // for (j = 0; j < x0; j++)
                    } // if (srcBuffer[index] != 0)
                } // for (x = 0; x < xDimSource; x++)
            } // for (y = 0; y < yDimSource; y++)
            	
            
            
           
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
            x0Array[c] = x0Array[c] * ((double)(xDimSource - 1))/((double)(x0 - 1));
            y0Array[c] = (largestIndex % x0y0)/x0;
            y0Array[c] = y0Array[c] * ((double)(yDimSource - 1))/((double)(y0 - 1));
            radArray[c] = radBuffer[largestIndex]/largestValue;
            countArray[c] = largestValue;
            
            if (c < numCardioids - 1) {
                // Zero hough buffer for next run
                for (i = 0; i < houghSlice; i++) {
                    houghBuffer[i] = 0;
                    radBuffer[i] = 0.0f;
                }
                // zero all points in the source slice that contributed to this cardioid
                fireProgressStateChanged("Zeroing source cardioid " + String.valueOf(c+1));
               
	                for (y = 0; y < yDimSource; y++) {
	                    offset = y * xDimSource;
	                    for (x = 0; x < xDimSource; x++) {
	                        index = offset + x;
	                        if (srcBuffer[index] != 0) {
	                            for (j = 0; j < x0; j++) {
	                                for (k = 0; k < y0; k++) {
	                                	theta = Math.atan2(y - d2Array[k], x - d1Array[j]);
	                                    num = Math.sqrt((x - d1Array[j])*(x - d1Array[j]) + (y - d2Array[k])*(y - d2Array[k]));
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
	                                            srcBuffer[index] = 0;
	                                        }
	                                    }
	                                } // for (k = 0; k < y0; k++)
	                            } // for (j = 0; j < x0; j++)
	                        } // if (srcBuffer[index] != 0)
	                    } // for (x = 0; x < xDimSource; x++)
	                } // for (y = 0; y < yDimSource; y++)
                	
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
                 xDimSource, y0Array, yDimSource, radArray, maxRad, countArray, selectedCardioid);
        
        if (!choice.okayPressed() ) {
            setCompleted(false);
            return;
        }
        
        // Draw selected cardioids
        for (i = 0; i < numCardioidsFound; i++) {
            if (selectedCardioid[i]) {
                for (j = 0; j < maxCardioidPoints; j++) {
                    theta = j * 2.0 * Math.PI/maxCardioidPoints;
                    x = (int)Math.round(x0Array[i] + radArray[i] * Math.cos(theta)*(1 - Math.cos(theta + theta0)));
                    y = (int)Math.round(y0Array[i] + radArray[i] * Math.sin(theta)*(1 - Math.cos(theta + theta0)));
                    indexDest = x + y * xDimSource;
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
