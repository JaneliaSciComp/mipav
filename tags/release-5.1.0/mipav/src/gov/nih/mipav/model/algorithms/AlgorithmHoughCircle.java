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
 *  The user is asked for the number of x0 bins, y0 bins, rad bins, and number of circles.  The default size for x0 is 
 *  min(512, image.getExtents()[0]).  The default size for y0 is min(512, image.getExtents()[1]).
 *  The default size for rad is min(512, max(image.getExtents()[0], image.getExtents()[1]).
 *  The default number of circles is 1. The program generates a Hough transform of the source image using the basic
 *  equation (x - x0)**2 + (y - y0)**2 = rad**2.  The program finds the circles containing the largest number of points.
 *  The program produces a dialog which allows the user to select which circles should be drawn.
 *  
 *  The Hough transform for the entire image is generated a separate time to find each circle.
 *  For each (xi, yi) point in the original image not having a value of zero, calculate the first dimension value d1 = 
 *  j * (xDim - 1)/(x0 - 1), with j = 0 to x0 - 1.  Calculate the second dimension value d2 = k * (yDim - 1)/(y0 - 1),
 *  with k = 0 to y0 - 1.  Calculate d3 = sqrt((x - d1)**2 + (y - d2)**2).
 *  d3 goes from 0 to maxRad = max(xDim-1, yDim-1)/2.0.  s3 is the dimension 3 scaling factor.
 *  s3 * (rad - 1) = maxRad.
 *  s3 = maxRad/(rad - 1)
 *  m = d3*(rad - 1)/maxRad.
 *  Only calculate the Hough transform for d3 <= maxRad.
 *  
 *  Find the peak point in the x0, y0, rad Hough transform space.
 *  Put the values for this peak point in x0Array[c], y0Array[c], radArray[c], and
 *  countArray[c].
 *  
 *  If more circles are to be found, then zero the houghBuffer and run through the
 *  same Hough transform a second time, but on this second run instead of incrementing
 *  the Hough buffer, zero the values in the source buffer that contributed to the peak
 *  point in the Hough buffer. So on the next run of the Hough transform the source points that
 *  contributed to the Hough peak value just detected will not be present.
 *  
 *  Create a dialog with numCirclesFound x0Array[i], y0Array[i], radArray[i], and
 *  countArray[i] values, where the user will select a check box to have that circle drawn.
 *  
 *  References: 1.) Digital Image Processing, Second Edition by Richard C. Gonzalez and Richard E. Woods, Section 10.2.2
 *  Global Processing via the Hough Transform, Prentice-Hall, Inc., 2002, pp. 587-591.
 *  
 *  2.) Shape Detection in Computer Vision Using the Hough Transform by V. F. Leavers, Springer-Verlag, 1992.
 * 
 */
public class AlgorithmHoughCircle extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------
    // Number of dimension 1 bins in Hough transform space
    private int x0;
    
    // Number of dimension 2 bins in Hough transform space
    private int y0;
    
    // number of dimension 3 bins in Hough transform space
    private int rad;
    
    // number of circles to be found
    private int numCircles;
    
    private ModelImage testImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmHoughCircle - default constructor.
     */
    public AlgorithmHoughCircle() { }

    /**
     * AlgorithmHoughCircle.
     *
     * @param  destImg  Image with lines filled in
     * @param  srcImg   Binary source image that has lines with gaps
     * @param  x0       number of dimension 1 bins in Hough transform space
     * @param  y0       number of dimension 2 bins in Hough transform space
     * @param  rad      number of dimension 3 bins in Hough transform space
     * @param  numCircles number of circles to be found
     */
    public AlgorithmHoughCircle(ModelImage destImg, ModelImage srcImg, int x0, int y0, int rad, int numCircles) {
        super(destImg, srcImg);
        this.x0 = x0;
        this.y0 = y0;
        this.rad = rad;
        this.numCircles = numCircles;
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
        int numCirclesFound;
        double x0Array[];
        double y0Array[];
        double radArray[];
        int countArray[];
        boolean selectedCircle[];
        JDialogHoughCircleChoice choice;
        byte value = 0;
        int maxCirclePoints;
        double maxRad;
        int x0y0;
        double xSum;
        double xSum2;
        double xSum3;
        double ySum;
        double ySum2;
        double ySum3;
        double radSum;

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        

        fireProgressStateChanged(srcImage.getImageName(), "Hough circle ...");

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
            
            xCenter = (xDimSource-1)/2.0;
            yCenter = (yDimSource-1)/2.0;
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
                index = x + y * xDimSource;
                srcBuffer[index] = 1;
                xSum = xSum + x;
                ySum = ySum + y;
                x = (int)Math.round(xCenter + radius2 *Math.cos(theta));
                y = (int)Math.round(yCenter + radius2 * Math.sin(theta));
                index = x + y * xDimSource;
                srcBuffer[index] = 1;
                xSum2 = xSum2 + x;
                ySum2 = ySum2 + y;
                x = (int)Math.round(xCenter + radius3 *Math.cos(theta));
                y = (int)Math.round(yCenter + radius3 * Math.sin(theta));
                index = x + y * xDimSource;
                srcBuffer[index] = 1;
                xSum3 = xSum3 + x;
                ySum3 = ySum3 + y;
            }
            testImage = new ModelImage(ModelStorageBase.BYTE, srcImage.getExtents(), "Hough Circle Test Image");
            try {
                testImage.importData(0, srcBuffer, true);
            }
            catch (IOException e) {
                MipavUtil.displayError("IOException " + e + " on testImage.importData");

                setCompleted(false);

                return;
            }
            new ViewJFrameImage(testImage);
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
        }

        radBuffer = new float[houghSlice];
        houghBuffer = new int[houghSlice];
        
        // Calculate d1Array and d2Array values
        d1Array = new double[x0];
        d2Array = new double[y0];
        for (i = 0; i < x0; i++) {
            d1Array[i] = ((double)(i * (xDimSource - 1)))/((double)(x0 - 1));
        }
        for (i = 0; i < y0; i++) {
            d2Array[i] = ((double)(i * (yDimSource - 1)))/((double)(y0 - 1));
        }
        d3Scale = ((double)(rad - 1))/maxRad;
        maxCirclePoints = (int)Math.ceil(2.0 * Math.PI * maxRad);
        
        x0Array = new double[numCircles];
        y0Array = new double[numCircles];
        radArray = new double[numCircles];
        countArray = new int[numCircles];
        numCirclesFound = 0;
        
        for (c = 0; c < numCircles; c++) {
            // Calculate the Hough transform
            fireProgressStateChanged("Calculating Hough circle " + String.valueOf(c+1));
            for (y = 0; y < yDimSource; y++) {
                offset = y * xDimSource;
                for (x = 0; x < xDimSource; x++) {
                    index = offset + x;
                    if (srcBuffer[index] != 0) {
                        for (j = 0; j < x0; j++) {
                            for (k = 0; k < y0; k++) {
                                d3 = Math.sqrt((x - d1Array[j])*(x - d1Array[j]) + (y - d2Array[k])*(y - d2Array[k]));
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
            // Obtain the x0, y0, rad, and count values of this circle
            fireProgressStateChanged("Finding Hough peak circle " + String.valueOf(c+1));
            
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
            
            numCirclesFound++;
            x0Array[c] = largestIndex % x0;
            x0Array[c] = x0Array[c] * ((double)(xDimSource - 1))/((double)(x0 - 1));
            y0Array[c] = (largestIndex % x0y0)/x0;
            y0Array[c] = y0Array[c] * ((double)(yDimSource - 1))/((double)(y0 - 1));
            radArray[c] = radBuffer[largestIndex]/largestValue;
            countArray[c] = largestValue;
            
            if (c < numCircles - 1) {
                // Zero hough buffer for next run
                for (i = 0; i < houghSlice; i++) {
                    houghBuffer[i] = 0;
                    radBuffer[i] = 0.0f;
                }
                // zero all points in the source slice that contributed to this circle
                fireProgressStateChanged("Zeroing source circle " + String.valueOf(c+1));
                for (y = 0; y < yDimSource; y++) {
                    offset = y * xDimSource;
                    for (x = 0; x < xDimSource; x++) {
                        index = offset + x;
                        if (srcBuffer[index] != 0) {
                            for (j = 0; j < x0; j++) {
                                for (k = 0; k < y0; k++) {
                                    d3 = Math.sqrt((x - d1Array[j])*(x - d1Array[j]) + (y - d2Array[k])*(y - d2Array[k]));
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
            } // if (c < numCircles - 1)
        } // for (c = 0; c < numCircles; c++)
        
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
        
        // Create a dialog with numCirclesFound x0Array[i], y0Array[i], radArray[i] and
        // countArray[i] values, where the user will select a check box to have the selected circle drawn.
        selectedCircle = new boolean[numCirclesFound];
        
        choice = new JDialogHoughCircleChoice(ViewUserInterface.getReference().getMainFrame(), x0Array,
                 xDimSource, y0Array, yDimSource, radArray, maxRad, countArray, selectedCircle);
        
        if (!choice.okayPressed() ) {
            setCompleted(false);
            return;
        }
        
        // Draw selected circles
        for (i = 0; i < numCirclesFound; i++) {
            if (selectedCircle[i]) {
                for (j = 0; j < maxCirclePoints; j++) {
                    theta = j * 2.0 * Math.PI/maxCirclePoints;
                    x = (int)Math.round(x0Array[i] + radArray[i]*Math.cos(theta));
                    y = (int)Math.round(y0Array[i] + radArray[i]*Math.sin(theta));
                    indexDest = x + y * xDimSource;
                    srcBuffer[indexDest] = value;
                }
            } // if (selectedCircle[i])
        } // for (i = 0; i < numCirclesFound; i++)
        
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
