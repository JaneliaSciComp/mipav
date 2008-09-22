package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;

/**
 *  This Hough transform uses (xi, yi) points in the original image space to generate rho, theta points in the Hough
 *  transform.  This Hough transform module only works with binary images.    Before it is used the user must 
 *  compute the gradient of an image and threshold it to obtain a binary image.  Noise removal and thinning should also
 *  be performed, if necessary, before this program is run. 
 *  
 *  The user is asked for the number of rho and theta bins.  The program generates a Hough transform of the source image
 *  using the basic equation rho = x*cos(theta) + y*sin(theta).  The program selects the lines containing the largest
 *  number of points.  The program can select up to maxLineNumber, which currently has a value of 10, lines. The program
 *  produces a dialog which allows the user to select which lines should be filled in and for selected lines the maximum
 *  length of the gaps to be filled.  By setting the maximum distance equal to 0.0, the user can have this module function
 *  solely for line detection and not perform any line filling.
 *  
 *  rho goes from -sqrt((xDim-1)*(xDim-1) + (yDim-1)*(yDim-1)) to +sqrt((xDim-1)*(xDim-1) + (yDim-1)*(yDim-1)).
 *  theta goes from -PI/2 to +PI/2.  rho has n1 cells and theta has n2 cells.  d = sqrt((xDim-1)*(xDim-1) + (yDim-1)*(yDim-1)).
 *  For each (xi, yi) point in the original image not having a value of zero, calculate n2 values of theta.
 *  Take k from 0 to (n2 - 1) to generate theta(k) = -PI/2 + k*PI/n2.
 *  rho = x*cos(theta) + y*sin(theta).  Then find which of n1 bins rho belongs to by calculating
 *  j = (rho + d)*n1/(2*d), with j going from 0 to n1-1.  If j = n1, set j equal to n1 - 1.
 *  For each of the points in the Hough transform make a list of the x values and y values
 *  that went to generate it.
 *  Generate the Hough transform image.
 *  
 *  Find which rho, theta points are peak points in the rho, theta plane.
 *  Find up to maxLineNumber lines with the highest number of points by finding the peaks in the
 *  Hough transform with the highest number of counts.  Note that only peaks in the rho, theta plane are
 *  considered so that we do not make selections from the spread around the peak.  Form rho, theta, and count arrays for these
 *  lines.
 *  
 *  Create a dialog with numLinesFound rhoArray[i], thetaArray[i], and
 *  countArray[i] values, where the user will select a check box to have that line selected for 
 *  gap filling and fill in a text field with the maximum gap length to be filled on that line.
 *  
 *  Fill in the gaps on the selected lines by examining distances between adjacent points.
 *  
 *  References: 1.) Digital Image Processing, Second Edition by Richard C. Gonzalez and Richard E. Woods, Section 10.2.2
 *  Global Processing via the Hough Transform, Prentice-Hall, Inc., 2002, pp. 587-591.
 *  
 *  2.) Shape Detection in Computer Vision Using the Hough Transform by V. F. Leavers, Springer-Verlag, 1992.
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
    
    private ModelImage testImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmHoughLine - default constructor.
     */
    public AlgorithmHoughLine() { }

    /**
     * AlgorithmHoughLine.
     *
     * @param  destImg  Image with lines filled in
     * @param  srcImg   Binary source image that has lines with gaps
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
        boolean test = false;
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
        int maxLinePoints;
        boolean peak[];
        int offset2;
        int index2;
        boolean found;
        boolean flat;
        boolean neighborNotPeak;

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        

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
            testImage = new ModelImage(ModelStorageBase.BYTE, srcImage.getExtents(),"Hough Line Test");
            try {
                testImage.importData(0, srcBuffer, true);
            }
            catch(IOException e) {
                MipavUtil.displayError("IOException " + e + " on testImage.importData");

                setCompleted(false);

                return;    
            }
            new ViewJFrameImage(testImage);
        }

        houghBuffer = new int[houghSlice];
        
        // Calculate values of cosine and sine arrays
        costh = new double[n2];
        sinth = new double[n2];
        for (i = 0; i < n2; i++) {
            theta = -Math.PI/2.0 + i*Math.PI/n2;
            costh[i] = Math.cos(theta);
            sinth[i] = Math.sin(theta);
        }
        d = Math.sqrt((xDimSource - 1.0)*(xDimSource - 1.0) + (yDimSource - 1.0)*(yDimSource - 1.0));
        maxLinePoints = (int)Math.ceil(Math.sqrt(xDimSource*xDimSource + yDimSource*yDimSource));
        xArray = new int[houghSlice][maxLinePoints];
        yArray = new int[houghSlice][maxLinePoints];
        
        // Calculate the Hough transform
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
                        xArray[indexDest][houghBuffer[indexDest]] = x;
                        yArray[indexDest][houghBuffer[indexDest]] = y;
                        houghBuffer[indexDest]++;
                    } // for (k = 0; k < n2; k++)
                } // if (srcBuffer[index] != 0)
            } // for (x = 0; x < xDimSource; x++)
        } // for (y = 0; y < yDimSource; y++)
        
        // Generate the hough transform image
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
        
        // Find which rho, theta points are peak points
        peak = new boolean[houghSlice];
        for (i = 0; i < houghSlice; i++) {
            peak[i] = true;
        }
        for (y = 0; y < n2; y++) {
            offset = y * n1;
            for (x = 0; x < n1; x++) {
                index = offset + x;
                for (k = Math.max(0, y-1); k <= Math.min(n2-1, y+1); k++) {
                    offset2 = k * n1;
                    for (j = Math.max(0, x-1); j <= Math.min(n1-1, x+1); j++) {
                        index2 = offset2 + j;
                        if (houghBuffer[index] < houghBuffer[index2]) {
                            peak[index] = false;
                        }
                    }
                }
            } // for (x = 0; x < n1; x++)
        } // for (y = 0; y < n2; y++)
        
        found = true;
        while(found) {
            found = false;
            for (y = 0; y < n2; y++) {
                offset = y * n1;
                for (x = 0; x < n1; x++) {
                    index = offset + x;
                    if (peak[index]) {
                        flat = true;
                        neighborNotPeak = false;
                        kloop:
                        for (k = Math.max(0, y-1); k <= Math.min(n2-1, y+1); k++) {
                            offset2 = k * n1;
                            for (j = Math.max(0, x-1); j <= Math.min(n1-1, x+1); j++) {
                                index2 = offset2 + j;
                                if (houghBuffer[index] != houghBuffer[index2]) {
                                    flat = false;
                                    break kloop;
                                }
                                if (!peak[index2]) {
                                    neighborNotPeak = true;
                                }
                            }
                        } // for (k = Math.max(0, y-1); k <= Math.min(n2-1, y+1); k++)
                        if (flat && neighborNotPeak) {
                            peak[index] = false;
                            found = true;
                        }
                    } // if (peak[index])
                } // for (x = 0; x < n1; x++)
            } // for (y = 0; y < n2; y++)   
        } // while(found)
       
        // Find up to maxLineNumber cells with the highest peak counts
        // Obtain the rho, theta, and count values of these lines
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
                if (peak[j] && (!foundIndex[j])) {
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
        
        // Create a dialog with numLinesFound rhoArray[i], thetaArray[i], and
        // countArray[i] values, where the user will select a check box to have that line selected for 
        // gap filling and fill in a text field with the maximum gap length to be filled on that line.
        selectedLine = new boolean[numLinesFound];
        maxDistance = new float[numLinesFound];
        
        choice = new JDialogHoughLineChoice(ViewUserInterface.getReference().getMainFrame(), rhoArray,
                 n1, thetaArray, n2, countArray, selectedLine, maxDistance);
        
        if (!choice.okayPressed() ) {
            setCompleted(false);
            return;
        }
        
        // Fill in the gaps on the selected lines by examining distances between adjacent points.
        for (i = 0; i < numLinesFound; i++) {
            if (selectedLine[i]) {
                for (j = 1; j < countArray[i]; j++) {
                    xDist = (double)(xArray[indexArray[i]][j] - xArray[indexArray[i]][j-1]);
                    xAbs = Math.abs(xDist);
                    yDist = (double)(yArray[indexArray[i]][j] - yArray[indexArray[i]][j-1]);
                    yAbs = Math.abs(yDist);
                    if ((xAbs > 1.0) || (yAbs > 1.0)) {
                        distance = Math.sqrt(xDist*xDist + yDist*yDist);
                        if (distance <= maxDistance[i]) {
                            if (xAbs >= yAbs) {
                                yPerX = yDist/xDist;
                                if (xDist > 0) {
                                    for (x = xArray[indexArray[i]][j-1] + 1; x < xArray[indexArray[i]][j]; x++) {
                                        y = (int)Math.round(yArray[indexArray[i]][j-1] + yPerX*(x - xArray[indexArray[i]][j-1]));
                                        offset = y * xDimSource + x;
                                        srcBuffer[offset] = value;
                                    }
                                }
                                else {
                                    for (x = xArray[indexArray[i]][j] + 1; x < xArray[indexArray[i]][j-1]; x++) {
                                        y = (int)Math.round(yArray[indexArray[i]][j] + yPerX*(x - xArray[indexArray[i]][j]));
                                        offset = y * xDimSource + x;
                                        srcBuffer[offset] = value;
                                    }
                                }
                            } // if (xAbs >= yAbs)
                            else { // xAbs < yAbs
                                xPerY = xDist/yDist;
                                if (yDist > 0) {
                                    for (y = yArray[indexArray[i]][j-1] + 1; y < yArray[indexArray[i]][j]; y++) {
                                        x = (int)Math.round(xArray[indexArray[i]][j-1] + xPerY*(y - yArray[indexArray[i]][j-1]));
                                        offset = y * xDimSource + x;
                                        srcBuffer[offset] = value;
                                    }
                                }
                                else {
                                    for (y = yArray[indexArray[i]][j] + 1; y < yArray[indexArray[i]][j-1]; y++) {
                                        x = (int)Math.round(xArray[indexArray[i]][j] + xPerY*(y - yArray[indexArray[i]][j]));
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
}
