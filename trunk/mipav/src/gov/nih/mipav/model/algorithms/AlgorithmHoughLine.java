package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

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
     */
    public AlgorithmHoughLine(ModelImage destImg, ModelImage srcImg) {
        super(destImg, srcImg);
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
        int n1, n2;
        int x, y;
        int offset;
        double rho;
        double d;

        int xDimSource;

        int yDimSource;

        int sourceSlice;

        int i, j, k;
        int index, indexDest;

        
        int destSlice;
        byte[] srcBuffer;
        int[] destBuffer;
        double theta;
        double costh[];
        double sinth[];
        boolean test = false;

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

        n1 = destImage.getExtents()[0];
        n2 = destImage.getExtents()[1];
        destSlice = n1 * n2;
        srcBuffer = new byte[sourceSlice];

        try {
            srcImage.exportData(0, sourceSlice, srcBuffer);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on srcImage.exportData");

            setCompleted(false);

            return;
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

        destBuffer = new int[destSlice];
        
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
                        destBuffer[indexDest]++;
                    } // for (k = 0; k < n2; k++)
                } // if (srcBuffer[index] != 0)
            } // for (x = 0; x < xDimSource; x++)
        } // for (y = 0; y < yDimSource; y++)
        try {
            destImage.importData(0, destBuffer, true);
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
