package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;
import java.util.*;
import java.awt.*;

/**
 *  This Hough transform uses (xi, yi) points in the original image space to generate rho, theta points in the Hough
 *  transform.  The Hough transform only works with binary images.  Compute the gradient of an image and threshold
 *  it to obtain a binary image.  Noise removal and thinning can then be used.  rho goes from
 *  -sqrt((xDim-1)*(xDim-1) + (yDim-1)*(yDim-1)) to +sqrt((xDim-1)*(xDim-1) + (yDim-1)*(yDim-1)).  theta goes from
 *  -PI/2 to +PI/2.  Let rho have n1 cells and let theta have n2 cells.  theta(k) = -PI/2 + k*PI/n2, where k goes from
 *  0 to (n2 - 1).  Let d = sqrt((xDim-1)*(xDim-1) + (yDim-1)*(yDim-1)). rho = x*cos(theta) + y*sin(theta)
 *  j = (rho + d)*n1/(2*d), with j going from 0 to n1-1.  If j = n1, set j equal to n1 - 1.
 * 
 */
public class AlgorithmHoughLine extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------
    // Number of rho bins
    private int n1;
    
    // Number of theta bins
    private int n2;
    
    // Hough transform of image with rho on x axis and theta on y axis
    private ModelImage houghImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmHoughLine - default constructor.
     */
    public AlgorithmHoughLine() { }

    /**
     * AlgorithmHoughLine.
     *
     * @param  destImg  DOCUMENT ME!
     * @param  srcImg   DOCUMENT ME!
     * @param  n1       number of rho bins
     * @param  n2       number of theta bins
     */
    public AlgorithmHoughLine(ModelImage destImg, ModelImage srcImg, int n1, int n2) {
        super(destImg, srcImg);
        this.n1 = n1;
        this.n2 = n2;
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
        double rho;
        double d;

        int xDimSource;

        int yDimSource;

        int sourceSlice;

        int i, j, k;
        int index, indexDest;

        
        int houghSlice;
        byte[] srcBuffer;
        int[] houghBuffer;
        double theta;
        double costh[];
        double sinth[];
        boolean test = true;
        boolean foundIndex[];
        int largestValue;
        int largestIndex;
        int numLinesFound;
        int indexArray[];
        int rhoArray[];
        int thetaArray[];
        int countArray[];
        int xArray[][];
        int yArray[][];
        int pointIndex;
        int maxLineNumber = 10;
        boolean selectedLine[];
        float maxDistance[];
        JDialogHoughLineChoice choice;
        int extents[];
        String name;
        double xDist;
        double yDist;
        double xAbs;
        double yAbs;
        double distance;
        double yPerX;
        double xPerY;
        byte value = 0;

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        constructLog();

        fireProgressStateChanged(srcImage.getImageName(), "Hough line ...");

        xDimSource = srcImage.getExtents()[0];
        yDimSource = srcImage.getExtents()[1];
        sourceSlice = xDimSource * yDimSource; 

        houghSlice = n1 * n2;
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
            
            srcBuffer[0] = 1;
            srcBuffer[(yDimSource-1)*xDimSource] = 1;
            srcBuffer[(xDimSource-1)/2 + ((yDimSource-1)/2)*xDimSource] = 1;
            srcBuffer[xDimSource - 1] = 1;
            srcBuffer[sourceSlice - 1] = 1;
        }

        houghBuffer = new int[houghSlice];
        
        costh = new double[n2];
        sinth = new double[n2];
        for (i = 0; i < n2; i++) {
            theta = -Math.PI/2.0 + i*Math.PI/n2;
            costh[i] = Math.cos(theta);
            sinth[i] = Math.sin(theta);
        }
        d = Math.sqrt((xDimSource - 1.0)*(xDimSource - 1.0) + (yDimSource - 1.0)*(yDimSource - 1.0));
        for (y = 0; y < yDimSource; y++) {
            offset = y * xDimSource;
            for (x = 0; x < xDimSource; x++) {
                index = offset + x;
                if (srcBuffer[index] != 0) {
                    for (k = 0; k < n2; k++) {
                        rho = x*costh[k] + y*sinth[k];
                        j = (int)((rho + d)*n1/(2.0*d));
                        if (j >= n1) {
                            j = n1 - 1;
                        }
                        indexDest = j + k*n1;
                        houghBuffer[indexDest]++;
                    } // for (k = 0; k < n2; k++)
                } // if (srcBuffer[index] != 0)
            } // for (x = 0; x < xDimSource; x++)
        } // for (y = 0; y < yDimSource; y++)
        extents = new int[2];
        extents[0] = n1;
        extents[1] = n2;
        name = srcImage.getImageName() +  "_hough_line";
        houghImage = new ModelImage(ModelStorageBase.INTEGER, extents, name);
        try {
            houghImage.importData(0, houghBuffer, true);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on houghImage.importData");

            setCompleted(false);

            return;
        }
        new ViewJFrameImage(houghImage);
       
        // Find the maxLineNumber cells with the highest counts
        foundIndex = new boolean[houghSlice];
        numLinesFound = 0;
        indexArray = new int[maxLineNumber];
        rhoArray = new int[maxLineNumber];
        thetaArray = new int[maxLineNumber];
        countArray = new int[maxLineNumber];
        for (i = 0; i < maxLineNumber; i++) {
            largestValue = 0;
            largestIndex = -1;
            for (j = 0; j < houghSlice; j++) {
                if (!foundIndex[j]) {
                    if (houghBuffer[j] > largestValue) {
                        largestValue = houghBuffer[j];
                        largestIndex = j;
                    }
                }
            } // for (j = 0; j < destSlice; j++)
            if (largestIndex == -1) {
                break;
            }
            foundIndex[largestIndex] = true;
            numLinesFound++;
            indexArray[i] = largestIndex;
            rhoArray[i] = largestIndex % n1;
            thetaArray[i] = largestIndex/n1;
            countArray[i] = largestValue;
        } // for (i = 0; i < maxLineNumber; i++)
        
        xArray = new int[numLinesFound][];
        yArray = new int[numLinesFound][];
        for (i = 0; i < numLinesFound; i++) {
            xArray[i] = new int[countArray[i]];
            yArray[i] = new int[countArray[i]];
            pointIndex = 0;
            for (y = 0; y < yDimSource; y++) {
                offset = y * xDimSource;
                for (x = 0; x < xDimSource; x++) {
                    index = offset + x;
                    if (srcBuffer[index] != 0) {
                        for (k = 0; k < n2; k++) {
                            rho = x*costh[k] + y*sinth[k];
                            j = (int)((rho + d)*n1/(2.0*d));
                            if (j >= n1) {
                                j = n1 - 1;
                            }
                            indexDest = j + k*n1;
                            if (indexDest == indexArray[i]) {
                                xArray[i][pointIndex] = x;
                                yArray[i][pointIndex++] = y;
                            }
                        } // for (k = 0; k < n2; k++)
                    } // if (srcBuffer[index] != 0)
                } // for (x = 0; x < xDimSource; x++)
            } // for (y = 0; y < yDimSource; y++)
        } // for (i = 0; i < numLinesFound; i++)
        
        // Create a dialog with numLinesFound(up to maxLineNumber) rhoArray[i], thetaArray[i], and
        // countArray[i], where the user will select a check box to have that line selected and
        // fill in a text field with the maximum gap length to be filled on that line.
        selectedLine = new boolean[numLinesFound];
        maxDistance = new float[numLinesFound];
        
        choice = new JDialogHoughLineChoice(ViewUserInterface.getReference().getMainFrame(), rhoArray,
                 n1, thetaArray, n2, countArray, selectedLine, maxDistance);
        
        if (!choice.okayPressed() ) {
            setCompleted(false);
            return;
        }
        
        for (i = 0; i < numLinesFound; i++) {
            if (selectedLine[i]) {
                for (j = 1; j < countArray[i]; j++) {
                    xDist = (double)(xArray[i][j] - xArray[i][j-1]);
                    xAbs = Math.abs(xDist);
                    yDist = (double)(yArray[i][j] - yArray[i][j-1]);
                    yAbs = Math.abs(yDist);
                    if ((xAbs > 1.0) || (yAbs > 1.0)) {
                        distance = Math.sqrt(xDist*xDist + yDist*yDist);
                        if (distance <= maxDistance[i]) {
                            if (xAbs >= yAbs) {
                                yPerX = yDist/xDist;
                                if (xDist > 0) {
                                    for (x = xArray[i][j-1] + 1; x < xArray[i][j]; x++) {
                                        y = (int)Math.round(yArray[i][j-1] + yPerX*(x - xArray[i][j-1]));
                                        offset = y * xDimSource + x;
                                        srcBuffer[offset] = value;
                                    }
                                }
                                else {
                                    for (x = xArray[i][j] + 1; x < xArray[i][j-1]; x++) {
                                        y = (int)Math.round(yArray[i][j] + yPerX*(x - xArray[i][j]));
                                        offset = y * xDimSource + x;
                                        srcBuffer[offset] = value;
                                    }
                                }
                            } // if (xAbs >= yAbs)
                            else { // xAbs < yAbs
                                xPerY = xDist/yDist;
                                if (yDist > 0) {
                                    for (y = yArray[i][j-1] + 1; y < yArray[i][j]; y++) {
                                        x = (int)Math.round(xArray[i][j-1] + xPerY*(y - yArray[i][j-1]));
                                        offset = y * xDimSource + x;
                                        srcBuffer[offset] = value;
                                    }
                                }
                                else {
                                    for (y = yArray[i][j] + 1; y < yArray[i][j-1]; y++) {
                                        x = (int)Math.round(xArray[i][j] + xPerY*(y - yArray[i][j]));
                                        offset = y * xDimSource + x;
                                        srcBuffer[offset] = value;
                                    }
                                }
                            } // else xAbs < yAbs
                        } // if (distance <= maxDistance[i])
                    } // if ((xAbs > 1.0) || (yAbs > 1.0))
                } // for (j = 1; j < countArray[i]; j++)
            } // if (selectedLine[i])
        } // for (i = 0; i < numLinesFound; i++)
        
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


        historyString = new String("HoughLine(" + String.valueOf(destImage.getExtents()[0]) + ", " +
                                   String.valueOf(destImage.getExtents()[1]) + ")\n");
    }
}
