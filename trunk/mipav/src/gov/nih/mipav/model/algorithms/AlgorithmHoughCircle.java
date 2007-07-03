package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;

/**
 *  This Hough transform uses (xi, yi) points in the original image space to generate x0, y0, rad points in the Hough
 *  transform.  This Hough transform module only works with binary images.    Before it is used the user must 
 *  compute the gradient of an image and threshold it to obtain a binary image.  Noise removal and thinning should also
 *  be performed, if necessary, before this program is run. 
 *  
 *  The user is asked for the number of x0, y0, and rad bins.  The default size for x0 is 
 *  min(512, image.getExtents()[0]).  The default size for y0 is min(512, image.getExtents()[1]).
 *  The default size for rad is min(512, max(image.getExtents()[0], image.getExtents()[1]).
 *  The program generates a Hough transform of the source image using the basic equation (x - x0)**2 + (y - y0)**2 = rad**2.  
 *  The program selects the circles containing the largest number of points.  The program can select up to maxCircleNumber,
 *  which currently has a value of 10, circles. The program produces a dialog which allows the user to select which
 *  circles should be drawn.
 *  
 *  For each (xi, yi) point in the original image not having a value of zero, calculate the first dimension value d1 = 
 *  j * (xDim - 1)/(x0 - 1), with j = 0 to x0 - 1.  Calculate the second dimension value d2 = k * (yDim - 1)/(y0 - 1),
 *  with k = 0 to y0 - 1.  Calculate d3 = sqrt((x - d1)**2 + (y - d2)**2).
 *  d3 goes from 0 to max(xDim-1, yDim-1)/2.  s3 is the dimension 3 scaling factor.
 *  s3 * (rad - 1) = max(xDim-1, yDim-1)/2.
 *  s3 = max(xDim-1, yDim-1)/(2*(rad - 1))
 *  m = d3*2*(rad - 1)/max(xDim-1, yDim-1).  If m > rad -1, set m equal to rad - 1.
 *  For each of the points in the Hough transform make a list of the x values and y values
 *  that went to generate it.
 *  
 *  Find up to maxCircleNumber circles with the highest number of points by finding the points in the
 *  Hough transform with the highest number of counts.  Form x0, y0, rad and count arrays for these
 *  lines.
 *  
 *  Create a dialog with numCriclesFound x0Array[i], y0Array[i], radArray[i], and
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
     */
    public AlgorithmHoughCircle(ModelImage destImg, ModelImage srcImg, int x0, int y0, int rad) {
        super(destImg, srcImg);
        this.x0 = x0;
        this.y0 = y0;
        this.rad = rad;
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
        int x, y, r;
        int offset;

        int xDimSource;

        int yDimSource;

        int sourceSlice;

        int i, j, k, m;
        int index, indexDest;
        
        int houghSlice;
        byte[] srcBuffer;
        int[] houghBuffer;
        double theta;
        double d1Array[];
        double d2Array[];
        double d3;
        double d3Scale;
        boolean test = true;
        double xCenter;
        double yCenter;
        double radius;
        boolean foundIndex[];
        int largestValue;
        int largestIndex;
        int numCirclesFound;
        int indexArray[];
        double x0Array[];
        double y0Array[];
        double radArray[];
        int countArray[];
        int maxCircleNumber = 10;
        boolean selectedCircle[];
        JDialogHoughCircleChoice choice;
        byte value = 0;
        int maxCirclePoints;
        double maxRad;
        int x0y0;
        double xSum;
        double ySum;
        double radSum;
        int xa;
        int ya;
        int ra;
        int rStart;
        int rFinish;
        int yStart;
        int yFinish;
        int xStart;
        int xFinish;

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        constructLog();

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
            xSum = 0.0;
            ySum = 0.0;
            for (i = 0; i < 20; i++) {
                theta = i * Math.PI/10.0;
                x = (int)Math.round(xCenter + radius *Math.cos(theta));
                y = (int)Math.round(yCenter + radius * Math.sin(theta));
                index = x + y * xDimSource;
                srcBuffer[index] = 1;
                xSum = xSum + x;
                ySum = ySum + y;
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
        }

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
        
        // Calculate the Hough transform
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
                            }
                        } // for (k = 0; k < y0; k++)
                    } // for (j = 0; j < x0; j++)
                } // if (srcBuffer[index] != 0)
            } // for (x = 0; x < xDimSource; x++)
        } // for (y = 0; y < yDimSource; y++)
       
        // Find up to maxCircleNumber cells with the highest counts
        // Obtain the x0, y0, rad,  and count values of these circles
        foundIndex = new boolean[houghSlice];
        numCirclesFound = 0;
        indexArray = new int[maxCircleNumber];
        x0Array = new double[maxCircleNumber];
        y0Array = new double[maxCircleNumber];
        radArray = new double[maxCircleNumber];
        countArray = new int[maxCircleNumber];
        for (i = 0; i < maxCircleNumber; i++) {
            largestValue = 0;
            largestIndex = -1;
            for (j = 0; j < houghSlice; j++) {
                if (!foundIndex[j]) {
                    if (houghBuffer[j] > largestValue) {
                        largestValue = houghBuffer[j];
                        largestIndex = j;
                    }
                }
            } // for (j = 0; j < houghSlice; j++)
            if (largestIndex == -1) {
                break;
            }
            
            foundIndex[largestIndex] = true;
            numCirclesFound++;
            indexArray[i] = largestIndex;
            xa = largestIndex % x0;
            x0Array[i] = xa * ((double)(xDimSource - 1))/((double)(x0 - 1));
            ya = (largestIndex % x0y0)/x0;
            y0Array[i] = ya * ((double)(yDimSource - 1))/((double)(y0 - 1));
            ra = largestIndex/ x0y0;
            radArray[i] = ra/d3Scale;
            countArray[i] = largestValue;
            //  Remove points immediately surrounding maximum
            for (r = Math.max(0, ra - 4); r <= Math.min(rad - 1, ra + 4); r++) {
                for (y = Math.max(0, ya - 2); y <= Math.min(y0 - 1, ya + 2); y++) {
                    for (x = Math.max(0, xa - 2); x <= Math.min(x0 - 1, xa + 2); x++) {
                        indexDest = x + y * x0 + r * x0y0;
                        if (indexDest != largestIndex) {
                            houghBuffer[indexDest] = 0;
                        }
                    }
                }
            }
        } // for (i = 0; i < maxCircleNumber; i++)
        
        // Create a dialog with numLinesFound x0Array[i], y0Array[i], radArray[i] and
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
    
    
    
    /**
     * Constructs a string of the contruction parameters and out puts the string to the messsage frame if the history
     * logging procedure is turned on.
     */
    private void constructLog() {


        historyString = new String("HoughCircles(" + String.valueOf(x0) + ", " +
                                   String.valueOf(y0) + ", " +
                                   String.valueOf(rad)+ ")\n");
    }
}
