package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;

/**
 * [(y - yv)*cos(phi) - (x - xv)*sin(phi)]**2 =
 * 4*p*[(y - vy)*sin(phi) + (x - vx)*cos(phi)]
 * where vx, vy are the coordinates of the parabola vertex
 * p is the distance between the vertex and focus of the parabola
 * 
 *  This Hough transform uses (xi, yi) points in the original image space to generate xv, yv, phi, p points in the Hough
 *  transform.  This Hough transform module only works with binary images.   Before it is used the user must 
 *  compute the gradient of an image and threshold it to obtain a binary image.  Noise removal and thinning should also
 *  be performed, if necessary, before this program is run. 
 *  
 *  The user is asked for the number of xv bins, yv bins, phi bins, the phi constant value for when phi bins == 1,
 *  p bins, pMin value, pMax value, maxBufferSize, and number of parabolas.  The desired size for xvBins is 
 *  min(512, image.getExtents()[0]).  The desired size for yvBins is min(512, image.getExtents()[1]).
 *  The desired size for phi is 360.  The default value for phiConstant is 90 degrees.  The default value for pBins
 *  is Math.min(512, Math.max(image.getExtents()[0], image.getExtents()[1])).  The default value for pMin is 1.0.
 *  The default value for pMax is Math.max(image.getExtents()[0], image.getExtents()[1]).  The default value for
 *  maxBufferSize is 256 megabytes.  The default number of parabolas is 1. The program generates a Hough transform
 *  of the source image using the basic equation [(y - yv)*cos(phi) - (x - xv)*sin(phi)]**2 =
 *  4*p*[(y - vy)*sin(phi) + (x - vx)*cos(phi)]
 *  The program finds the parabolas containing the largest number of points.
 *  The program produces a dialog which allows the user to select which parabolas should be drawn.
 *  
 *  The Hough transform for the entire image is generated a separate time to find each parabola.
 *  For each (xi, yi) point in the original image not having a value of zero, calculate the first dimension value xvArray[j] = 
 *  j * (xDim - 1)/(xvBins - 1), with j = 0 to xvBins - 1.  Calculate the second dimension value yvArray[k] = k * (yDim - 1)/(yvBins - 1),
 *  with k = 0 to yvBins - 1.  calculate the third dimension phiArray[m] = m * 2.0 * PI/phiBins, with m going from 0 to phiBins - 1
 *  Calculate p = [(y - yv)*cos(phi) - (x - xv)*sin(phi)]**2/[4*[(y - vy)*sin(phi) + (x - vx)*cos(phi)]]
 *  p goes from pMin to pMax
 *  pMin + s4 * (pBins - 1) = pMax.
 *  s4 = (pMax - pMin)/(pBins - 1)
 *  n = (p - pMin)*(pBins - 1)/(pMax - pMin).
 *  Only calculate the Hough transform for pMax >= p >= pMin.
 *  
 *  Find the peak point in the xv, yv, phi, p Hough transform space.
 *  Put the values for this peak point in xvArray[c], yvArray[c], phiArray[c], pArray[c], and
 *  countArray[c].
 *  
 *  If more parabolas are to be found, then zero the houghBuffer and run through the
 *  same Hough transform a second time, but on this second run instead of incrementing
 *  the Hough buffer, zero the values in the source buffer that contributed to the peak
 *  point in the Hough buffer. So on the next run of the Hough transform the source points that
 *  contributed to the Hough peak value just detected will not be present.
 *  
 *  Create a dialog with numParabolasFound xvArray[i], yvArray[i], phiArray[i], pArray[i], and
 *  countArray[i] values, where the user will select a check box to have that parabola drawn.
 *  
 *  References: 1.) Digital Image Processing, Second Edition by Richard C. Gonzalez and Richard E. Woods, Section 10.2.2
 *  Global Processing via the Hough Transform, Prentice-Hall, Inc., 2002, pp. 587-591.
 *  
 *  2.) Form of parabola equation using phi taken from MATLAB routine houghparabola by Clara Isabel Sanchez.
 * 
 */
public class AlgorithmHoughParabola extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------
    // Number of dimension xv bins in Hough transform space
    private int xvBins;
    
    // Number of dimension yv bins in Hough transform space
    private int yvBins;
    
    // number of dimension phi bins in Hough transform space
    private int phiBins;
    
    // Value of phi in radians if phiBins = 1
    private double phiConstant;
    
    // Number of dimension p bins in Hough transform space
    private int pBins;
    
    // Minimum p value
    private float pMin;
    
    // Maximum p value
    private float pMax;
    
    /** The maximum Hough transform size in megabytes - default is currently 256 */
    private int maxBufferSize;
    
    // number of parabolas to be found
    private int numParabolas;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmHoughParabola - default constructor.
     */
    public AlgorithmHoughParabola() { }

    /**
     * AlgorithmHoughParabola.
     *
     * @param  destImg  Image with lines filled in
     * @param  srcImg   Binary source image that has lines with gaps
     * @param  xvBins   number of dimension xv bins in Hough transform space
     * @param  yvBins   number of dimension yv bins in Hough transform space
     * @param  phiBins  number of dimension phi bins in Hough transform space
     * @param  phiConstant phi value in radians if phiBins == 1
     * @param  pBins    number of dimension p bins in Hough transform space
     * @param  pMin     minimum p value
     * @param  pMax     maximum p value
     * @param  maxBufferSize maximum Hough transform size in megabytes
     * @param  numParabolas number of parabolas to be found
     */
    public AlgorithmHoughParabola(ModelImage destImg, ModelImage srcImg, int xvBins, int yvBins, int phiBins, 
                                double phiConstant, int pBins, float pMin, float pMax, int maxBufferSize,
                                int numParabolas) {
        super(destImg, srcImg);
        this.xvBins = xvBins;
        this.yvBins = yvBins;
        this.phiBins = phiBins;
        this.phiConstant = phiConstant;
        this.pBins = pBins;
        this.pMin = pMin;
        this.pMax = pMax;
        this.maxBufferSize = maxBufferSize;
        this.numParabolas = numParabolas;
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

        int i, j, k, m, n, c;
        int index, indexDest;
        
        int houghSlice;
        byte[] srcBuffer;
        short[] countBuffer;
        float pBuffer[];
        boolean test = false;
        int largestValue;
        int largestIndex;
        int numParabolasFound;
        boolean selectedParabola[];
        JDialogHoughParabolaChoice choice;
        byte value = 0;
        long longNumBins;
        int bytesPerCell;
        int numBins;
        long desiredBytes;
        long actualBytesAvailable;
        double shrinkFactor = 1.0;
        double xVertex;
        double yVertex;
        double xvArray[];
        double yvArray[];
        double phiArray[];
        float pScale;
        int maxParabolaPoints;
        float xvTable[];
        float yvTable[];
        float phiTable[];
        float pTable[];
        int countTable[];
        double p;
        double numerator;
        double denominator;
        double xdel;
        double ydel;
        double sinArray[];
        double cosArray[];
        int xy;
        int xyp;
        // 0 for x - xVertex negative, y - yVertex negative
        // 1 for x - xVertex negative, y - yVertex zero or positive
        // 2 for x - xVertex zero or positive, y - yVertex negative
        // 3 for x - xVertex zero or positive, y - yVertex zero or positive
        double  maxDistance[];
        int xMax[];
        int yMax[];
        // 2 endpoints for each parabola
        int xEndPoint[][];
        int yEndPoint[][];
        int typeEndPoint[][];
        double distanceEndPoint[][];
        double distance;

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
        
        fireProgressStateChanged(srcImage.getImageName(), "Hough parabola ...");

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sourceSlice = xDim * yDim; 
        
        // Calculate the desired number of bins that would be used for each parameter if memory were not a
        // limitation.
        bytesPerCell = 4 + 2; // float for p parameter and short for count;
        longNumBins = (long)xvBins * (long)yvBins * (long)phiBins * (long)pBins;
        numBins = (int)longNumBins;
        desiredBytes = longNumBins * (long)bytesPerCell;
        actualBytesAvailable = (long)maxBufferSize * 1024L * 1024L;
        if (actualBytesAvailable < desiredBytes) {
            // Must shrink the number of each bins used for each parameter by the third or fourth root of
            // deisredBytes/actualBytesAvailable to keep the buffer size down to actualBytesAvailable
            if (phiBins > 1) {
                shrinkFactor = Math.pow((double)desiredBytes/(double)actualBytesAvailable, 0.25);
                phiBins = (int)Math.ceil(phiBins/shrinkFactor);
            }
            else {
                shrinkFactor = Math.pow((double)desiredBytes/(double)actualBytesAvailable,1.0/3.0);
            }
            xvBins = (int)Math.ceil(xvBins/shrinkFactor);
            yvBins = (int)Math.ceil(yvBins/shrinkFactor);
            pBins = (int)Math.ceil(pBins/shrinkFactor);
            numBins = xvBins * yvBins * phiBins * pBins;
        } // if (actualBytesAvailable < desiredBytes)
        xy = xvBins * yvBins;
        xyp = xy * phiBins;
        ViewUserInterface.getReference().setDataText("xvBins = " + xvBins + "\n");
        ViewUserInterface.getReference().setDataText("yvBins = " + yvBins + "\n");
        ViewUserInterface.getReference().setDataText("phiBins = " + phiBins + "\n");
        ViewUserInterface.getReference().setDataText("pBins = " + pBins + "\n");
        ViewUserInterface.getReference().setDataText("numBins = " + numBins + "\n");

        houghSlice =  numBins;
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
            
            xVertex = (xDim-1)/2.0;
            yVertex = (yDim-1)/2.0;
            for (i = -20; i <= 20 ; i++) {
                x = (int)(xVertex + i);
                y = (int)Math.round(yVertex + 0.25*(x - xVertex)*(x - xVertex));
                index = x + y * xDim;
                srcBuffer[index] = 1;
            }
        } // if (test)

        pBuffer = new float[numBins];
        countBuffer = new short[numBins];
        
        // Calculate xvArray, yvArray, phiArray, cosArray, sinArray values
        xvArray = new double[xvBins];
        yvArray = new double[yvBins];
        phiArray = new double[phiBins];
        cosArray = new double[phiBins];
        sinArray = new double[phiBins];
        for (i = 0; i < xvBins; i++) {
            xvArray[i] = ((double)(i * (xDim - 1)))/((double)(xvBins - 1));
        }
        for (i = 0; i < yvBins; i++) {
            yvArray[i] = ((double)(i * (yDim - 1)))/((double)(yvBins - 1));
        }
        if (phiBins > 1) {
            for (i = 0; i < phiBins; i++) {
                phiArray[i] = i * 2.0 * Math.PI/phiBins;
                cosArray[i] = Math.cos(phiArray[i]);
                sinArray[i] = Math.sin(phiArray[i]);
            }
        }
        else {
            phiArray[0] = phiConstant;
            cosArray[0] = Math.cos(phiConstant);
            sinArray[0] = Math.sin(phiConstant);
        }
        pScale = (pBins - 1)/(pMax - pMin);
        maxParabolaPoints = 2*Math.max(xDim, yDim) + Math.min(xDim, yDim);
        
        xvTable = new float[numParabolas];
        yvTable = new float[numParabolas];
        phiTable = new float[numParabolas];
        pTable = new float[numParabolas];
        countTable = new int[numParabolas];
        numParabolasFound = 0;
        
        maxDistance = new double[4];
        xMax = new int[4];
        yMax = new int[4];
        xEndPoint = new int[numParabolas][2];
        yEndPoint = new int[numParabolas][2];
        typeEndPoint = new int[numParabolas][2];
        distanceEndPoint = new double[numParabolas][2];
        for (c = 0; c < numParabolas; c++) {
            // Calculate the Hough transform
            fireProgressStateChanged("Calculating Hough parabola " + String.valueOf(c+1));
            for (y = 0; y < yDim; y++) {
                offset = y * xDim;
                for (x = 0; x < xDim; x++) {
                    index = offset + x;
                    if (srcBuffer[index] != 0) {
                        for (j = 0; j < xvBins; j++) {
                            xdel = x - xvArray[j];
                            for (k = 0; k < yvBins; k++) {
                                ydel = y - yvArray[k];
                                for (m = 0; m < phiBins; m++) {
                                    numerator = ydel*cosArray[m] - xdel*sinArray[m];
                                    numerator = numerator * numerator;
                                    denominator = 4*(ydel*sinArray[m] + xdel*cosArray[m]);
                                    if (denominator != 0.0) {
                                        p = numerator/denominator;
                                        if ((p >= pMin) && (p <= pMax)) {
                                            n = (int)((p - pMin)*pScale);
                                            indexDest = j + k * xvBins + m * xy + n * xyp;
                                            countBuffer[indexDest]++;
                                            pBuffer[indexDest] += p;
                                        } // if ((p >= pMin) && (p <= pMax)
                                    } // if (denominator != 0.0)
                                } // for (m = 0; m < phiBins; m++)
                            } // for (k = 0; k < yvBins; k++)
                        } // for (j = 0; j < xvBins; j++)
                    } // if (srcBuffer[index] != 0)
                } // for (x = 0; x < xDim; x++)
            } // for (y = 0; y < yDim; y++)
            
            
           
            // Find cell with the highest counts
            // Obtain the xv, yv, phi, p, and count values of this parabola
            fireProgressStateChanged("Finding Hough peak parabola " + String.valueOf(c+1));
            
            largestValue = 0;
            largestIndex = -1;
            for (j = 0; j < houghSlice; j++) {
                if (countBuffer[j] > largestValue) {
                    largestValue = countBuffer[j];
                    largestIndex = j;
                }
            } // for (j = 0; j < houghSlice; j++)
            if (largestIndex == -1) {
                break;
            }
            
            numParabolasFound++;
            xvTable[c] = largestIndex % xvBins;
            xvTable[c] = xvTable[c] * ((float)(xDim - 1))/((float)(xvBins - 1));
            yvTable[c] = (largestIndex % xy)/xvBins;
            yvTable[c] = yvTable[c] * ((float)(yDim - 1))/((float)(yvBins - 1));
            if (phiBins > 1) {
                phiTable[c] = (largestIndex % xyp)/xy;
                phiTable[c] = (float)(phiTable[c] * 2.0 * Math.PI/phiBins);
            }
            else {
                phiTable[c] = (float)phiConstant;
            }
            pTable[c] = pBuffer[largestIndex]/largestValue;
            countTable[c] = largestValue;
            
            // Zero hough buffer for next run
            for (i = 0; i < houghSlice; i++) {
                countBuffer[i] = 0;
                pBuffer[i] = 0.0f;
            }
            // zero all points in the source slice that contributed to this parabola
            // and find the endpoints of the parabola
            fireProgressStateChanged("Zeroing source parabola " + String.valueOf(c+1));
            for (i = 0; i < 4; i++) {
                maxDistance[i] = -1.0;
                xMax[i] = -1;
                yMax[i] = -1;
            }
            for (y = 0; y < yDim; y++) {
                offset = y * xDim;
                for (x = 0; x < xDim; x++) {
                    index = offset + x;
                    if (srcBuffer[index] != 0) {
                        for (j = 0; j < xvBins; j++) {
                            xdel = x - xvArray[j];
                            for (k = 0; k < yvBins; k++) {
                                ydel = y - yvArray[k];
                                for (m = 0; m < phiBins; m++) {
                                    numerator = ydel*cosArray[m] - xdel*sinArray[m];
                                    numerator = numerator * numerator;
                                    denominator = 4*(ydel*sinArray[m] + xdel*cosArray[m]);
                                    if (denominator != 0.0) {
                                        p = numerator/denominator;
                                        if ((p >= pMin) && (p <= pMax)) {
                                            n = (int)((p - pMin)*pScale);
                                            indexDest = j + k * xvBins + m * xy + n * xyp;
                                            if (indexDest == largestIndex) {
                                                srcBuffer[index] = 0;
                                                distance = Math.sqrt(xdel*xdel + ydel*ydel);
                                                if ((xdel < 0.0) && (ydel < 0.0)) {
                                                    if (distance > maxDistance[0]) {
                                                        maxDistance[0] = distance;
                                                        xMax[0] = x;
                                                        yMax[0] = y;
                                                    }
                                                } // if ((xdel < 0.0) && (ydel < 0.0))
                                                else if ((xdel < 0.0) && (ydel >= 0.0)) {
                                                    if (distance > maxDistance[1]) {
                                                        maxDistance[1] = distance;
                                                        xMax[1] = x;
                                                        yMax[1] = y;
                                                    }
                                                } // else if ((xdel < 0.0) && (ydel >= 0.0))
                                                else if ((xdel >= 0.0) && (ydel < 0.0)) {
                                                    if (distance > maxDistance[2]) {
                                                        maxDistance[2] = distance;
                                                        xMax[2] = x;
                                                        yMax[2] = y;
                                                    }
                                                } // else if ((xdel >= 0.0) && (ydel < 0.0))
                                                else {
                                                    if (distance > maxDistance[3]) {
                                                        maxDistance[3] = distance;
                                                        xMax[3] = x;
                                                        yMax[3] = y;
                                                    }
                                                }
                                            } // if (indexDest == largestIndex)
                                        } // if ((p >= pMin) && (p <= pMax)
                                    } // if (denominator != 0.0)
                                } // for (m = 0; m < phiBins; m++)
                            } // for (k = 0; k < yvBins; k++)
                        } // for (j = 0; j < xvBins; j++)
                    } // if (srcBuffer[index] != 0)
                } // for (x = 0; x < xDim; x++)
            } // for (y = 0; y < yDim; y++)
            // Find one end point
            distanceEndPoint[c][0] = -1.0;
            for (i = 0; i < 4; i++) {
                if (maxDistance[i] > distanceEndPoint[c][0]) {
                    distanceEndPoint[c][0] = maxDistance[i];
                    xEndPoint[c][0] = xMax[i];
                    yEndPoint[c][0] = yMax[i];
                    typeEndPoint[c][0] = i;
                }
            }
            distanceEndPoint[c][1] = -1.0;
            for (i = 0; i < 4; i++) {
                if (i != typeEndPoint[c][0]) {
                    if (maxDistance[i] > distanceEndPoint[c][1]) {
                        distanceEndPoint[c][1] = maxDistance[i];
                        xEndPoint[c][1] = xMax[i];
                        yEndPoint[c][1] = yMax[i];
                        typeEndPoint[c][1] = i;
                    }
                }
            }
        } // for (c = 0; c < numParabolas; c++)
        
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
        
        // Create a dialog with numParabolasFound xvTable[i], yvTable[i], phiTable[i], pTable[i], and
        // countTable[i] values, where the user will select a check box to have the selected parabola drawn.
        selectedParabola = new boolean[numParabolasFound];
        
        choice = new JDialogHoughParabolaChoice(ViewUserInterface.getReference().getMainFrame(), xvTable,
                 xDim, yvTable, yDim, phiTable, phiBins, pTable, pMin, pMax, countTable, selectedParabola);
        
        if (!choice.okayPressed() ) {
            setCompleted(false);
            return;
        }
        
        // Draw selected parabolas
        for (i = 0; i < numParabolasFound; i++) {
            if (selectedParabola[i]) {
                for (j = 0; j < maxParabolaPoints; j++) {
                    
                    //indexDest = x + y * xDim;
                    //srcBuffer[indexDest] = value;
                }
            } // if (selectedParabola[i])
        } // for (i = 0; i < numParabolaFound; i++)
        
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
