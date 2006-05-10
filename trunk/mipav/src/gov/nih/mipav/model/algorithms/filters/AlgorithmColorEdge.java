package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 This algorithm uses a hypercomplex filter to find the edges between a region of two
 user specified colors.  This code is based on material in the article:
 Colour-Sensitive Edge Detection using Hypercomplex Filters by Carolyn J. Evans ahd
 Stephen J. Sangwine.  This filters on operates in 2D.
 */
public class AlgorithmColorEdge extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The ViewUserInterface is held for debugging purposes only. May be deprecated in the future. */
    ViewUserInterface userInterface; // for debug only
    
    private int red1;
    private int green1;
    private int blue1;
    private int red2;
    private int green2;
    private int blue2;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * This constructor initialises a Color Edge algorithm for a source and destination image, and ensures that
     * the destination image is <code>ModelStorageBase.UBYTE</code>.
     *
     * <p>Currently (9 May 2006), this algorithm does not support replacing the original data set with that of the
     * color edge image.</p>
     *
     * @param  dest      DOCUMENT ME!
     * @param  src       DOCUMENT ME!
     * @param  red1
     * @param  green1
     * @param  blue1
     * @param  red2
     * @param  green2
     * @param  blue2
     */
    public AlgorithmColorEdge(ModelImage dest, ModelImage src, int red1, int green1, int blue1,
                                   int red2, int green2, int blue2) {
        super(dest, src);

        this.red1 = red1;
        this.green1 = green1;
        this.blue1 = blue1;
        this.red2 = red2;
        this.green2 = green2;
        this.blue2 = blue2;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        super.finalize();

        
    }

    /**
     * Standard algorithm run method. It will not run if the source Image is <code>null</code>. The calculation is done
     * and placed in a separate destination image if it is to be stored there.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        constructLog();

        if (destImage != null) { // if there exists a destination image
            calcStoreInDest();
        } else { // there is no image but the original source.
            calcInPlace();
        }
    }

    
    /**
     * The ViewUserInterface is set for debugging purposes only. This method may be deprecated in the future.
     *
     * @param  vui  DOCUMENT ME!
     */
    public void setUserInterface(ViewUserInterface vui) {
        userInterface = vui;
    }

    /**
     * Filters the source image. Replaces the original image with the filtered image.
     *
     * <p><em>Does not currently work.</em></p>
     */
    private void calcInPlace() {
        errorCleanUp("AlgorithmColorEdge: " + "Replace Image not yet implemented", false);
        finalize();

        return;
    }

    /**
     * This function produces a color edged image into a ModelImage that does not replace the original image-data.
     */
    private void calcStoreInDest() {
        int dataLen = srcImage.getSliceSize();
        int colorLen = 4*dataLen;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        float[] imageData;
        byte[] edgeData;
        int sliceNum = 1;
        int z;
        int x, y;
        double absC1;
        double absC2;
        double C1DotC2;
        double C1CrossC2;
        
        absC1 = Math.sqrt(red1*red1 + green1*green1 + blue1*blue1);
        absC2 = Math.sqrt(red2*red2 + green2*green2 + blue2*blue2);

        if (srcImage.getNDims() == 3) {
            sliceNum *= srcImage.getExtents()[2];
        } else if (srcImage.getNDims() == 4) {
            sliceNum *= srcImage.getExtents()[2] * srcImage.getExtents()[3];
        }

        try {
            imageData = new float[colorLen];
        } catch (OutOfMemoryError oome) {
            errorCleanUp("AlgorithmColorEdge: " + "out of memory creating imageData", false);
            finalize();

            return;
        }
        
        try {
            edgeData = new byte[dataLen];
        } catch (OutOfMemoryError oome) {
            errorCleanUp("AlgorithmColorEdge: " + "out of memory creating edgeData", false);
            finalize();

            return;
        }

        
        for (z = 0; z < sliceNum; z++) {
            try {
                srcImage.exportData(z*colorLen, colorLen, imageData);
            } catch (IOException ioe) {
                errorCleanUp("AlgorithmColorEdge: " + "failure to export imageData", false);
                finalize();
    
                return;
            }
            
            for (y = 1; y <= yDim - 2; y++) {
                for (x = 1; x <= xDim - 2; x++) {
                    // Perform the hypercomplex filter designed to find all horizontal
                    // edges C1 -> C2 (from top to bottom)
                }
            } // for (y = 1; y <= yDim - 2; y++)
    
            
            if (threadStopped) {
                setCompleted(false);
                finalize();
    
                return;
            }
    
            try {
                destImage.importData(z*dataLen, edgeData, false);
            }
            catch (IOException ioe) {
                errorCleanUp("AlgorithmColorEdge: " + "failure to import edgeData", false);
                finalize();
    
                return;    
            }
        } // for (z = 0; z < sliceNum; z++)
        destImage.calcMinMax();

        setCompleted(true);

    } // end calcStoreInDest()

    /**
     * Constructs a string of the contruction parameters and outputs the string to the messsage frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {
        String str; 

        historyString = "color edge(" + String.valueOf(red1) + ", " + String.valueOf(green1) + ", " +
                        String.valueOf(blue1) + ", " + String.valueOf(red2) + ", " +
                        String.valueOf(green2) + ", " + String.valueOf(blue2) + ")\n";
    }


    
}
