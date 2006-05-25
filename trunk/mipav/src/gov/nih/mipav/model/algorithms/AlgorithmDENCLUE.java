package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

/**
  References: 1.) Data Mining Concepts and Techniques by Jiawei Han and Micheline Kamber
  Section 8.6.3 DENCLUE: Clustering Based on Density Distribution Functions, pp. 366-
  369, 2001.
  2.) An Efficient Approach to Clustering in Large Multimedia Databases with Noise by
  Alexander Hinneburg and Daniel A. Keim, Proc. 1998 Int. Conf. Knowledge Discovery and
  Data Mining, pages 58-65, New York, August, 1998.
 */
public class AlgorithmDENCLUE extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The ViewUserInterface is held for debugging purposes only. May be deprecated in the future. */
    ViewUserInterface userInterface; // for debug only
    
    // If true, use a Gaussian influence function
    // If false, use a square wave influence function
    private boolean isGaussian;
    // If isGaussian is true, this is the standard deviation in the Gaussian influence function
    // If a square wave function, all pixels sum out to this distance,
    // zero past this distance
    private float distance;
    // density function must equal or exceed this value to be part of a cluster
    private float threshold;
    // If isArbitrary is true, create arbitrary shaped clusters
    // If isArbitrary is false, create center defined clusters
    private boolean isArbitrary;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * This constructor initialises a Color Edge algorithm for a source and destination image, and ensures that
     * the destination image is <code>ModelStorageBase.UBYTE</code>.
     *
     * <p>Currently (25 May 2006), this algorithm does not support replacing the original data set with that of the
     * color edge image.</p>
     *
     * @param  dest      DOCUMENT ME!
     * @param  src       DOCUMENT ME!
     * @param  isGaussian If true, Gaussian influence function
     *                    If false, Square wave influence function
     * @param  distance  if isGaussian is true, the standard deviation in the Gaussian
     *                   influence function.  If a square wave function, all pixels sum
     *                   out to this distance, zero past this density
     * @param  threshold Density function must equal or exceed this value to be part
     *                   of a cluster
     * @param  isArbitrary  If isArbitrary is true, create arbitrary shaped clusters
     *                      If isArbitrary is false, create center defined clusters
     */
    public AlgorithmDENCLUE(ModelImage dest, ModelImage src, boolean isGaussian,
                            float distance, float threshold, boolean isArbitrary) {
        super(dest, src);

        this.isGaussian = isGaussian;
        this.distance = distance;
        this.threshold = threshold;
        this.isArbitrary = isArbitrary;
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
        errorCleanUp("AlgorithmDENCLUE: " + "Replace Image not yet implemented", false);
        finalize();

        return;
    }

    /**
     * This function produces a density based clustering image that does not replace the original image-data.
     */
    private void calcStoreInDest() {
        
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int length = xDim * yDim;
        float buffer[];
        float density[];
        int maxDistance;
        int x, y;
        int j, k;
        int index;
        
        buildProgressBar("Finding density cluster in " + srcImage.getImageName(), "DENCLUE..", 0, 100);
        initProgressBar();
        
        buffer = new float[length];
        density = new float[length];
        
        try {
            srcImage.exportData(0, length, buffer);
        }
        catch (IOException e) {
            MipavUtil.displayError("Error on srcImage.exportData");
            disposeProgressBar();
            setCompleted(false);
        }
        
        if (isGaussian) {
            maxDistance = Math.max(1, (int)(5.0 * distance + 0.5f));
            for (y = 0; y < yDim; y++) {
                for (x = 0; x < xDim; x++) {
                    index = y*xDim + x;
                    for (k = y - maxDistance; k <= y + maxDistance; k++) {
                        if ((k >= 0) && (k < yDim)) {
                            for (j = x - maxDistance; j <= x + maxDistance; j++) {
                                if ((j >= 0) && (j < xDim)) {
                                    density[index] += Math.exp
                                    (-((k-y)*(k-y) + (j-x)*(j-x))/(2.0*distance*distance));
                                }
                            }
                        }
                    }
                }
            }
        } // if (isGaussian)
        else { // Square wave
        } // else Square wave
        disposeProgressBar();

        setCompleted(true);

    } // end calcStoreInDest()

    /**
     * Constructs a string of the contruction parameters and outputs the string to the messsage frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {

        historyString = "DENCLUE(" + String.valueOf(isGaussian) + ", " + String.valueOf(distance) + ", " +
                        String.valueOf(threshold) + ", " + String.valueOf(isArbitrary) + ")\n";
    }


    
}
