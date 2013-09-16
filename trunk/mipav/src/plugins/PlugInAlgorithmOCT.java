import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.ViewUserInterface;

import java.io.*;


/**
 * DOCUMENT ME!
 */
public class PlugInAlgorithmOCT extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int noiseBound = 0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for 3D images in which changes are placed in a predetermined destination image.
     *
     * @param  srcImg    Source image model.
     * @param  noiseLen  the noise bound
     */
    public PlugInAlgorithmOCT(ModelImage srcImg, int noiseLen) {
        super(null, srcImg);
        noiseBound = noiseLen;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        super.finalize();
    }

    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        

        if (srcImage.getNDims() == 2) {
            calcStoreInDest2D();
        }
    } // end runAlgorithm()

    /**
     * This function produces a new image that has been median filtered and places filtered image in the destination
     * image.
     */
    private void calcStoreInDest2D() {
        int length; // total number of data-elements (pixels) in image
        float[] buffer; // data-buffer (for pixel data) which is the "heart" of the image

        // call algorithm median filter
        // call algorithm histogram and get threshold
        boolean adaptiveSize = false;
        boolean truncatedMedian = false;
        int maximumSize = 0;
        AlgorithmMedian algoMedian = new AlgorithmMedian(srcImage, 1, 3, AlgorithmMedian.SQUARE_KERNEL, 0, adaptiveSize,
                                                         truncatedMedian, maximumSize, true);
        algoMedian.run();

        int[] dimExtent = new int[1];
        dimExtent[0] = (int) Math.round(srcImage.getMax() - srcImage.getMin());

        ModelHistogram histogram = new ModelHistogram(ModelStorageBase.INTEGER, dimExtent);
        AlgorithmHistogram histo = new AlgorithmHistogram(histogram, srcImage, true);
        histo.run();

        try {

            // image length is length in 2 dims
            length = srcImage.getExtents()[0] * srcImage.getExtents()[1];
            buffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm OCT reports: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm OCT reports: out of memory", true);

            return;
        }

        // Threshold image
        int threshold = histo.getModelHistogram().getOtsuThreshold();
        ViewUserInterface.getReference().setGlobalDataText(srcImage.getImageName() + " Threshold = " + threshold +
                                                           "\n");

        for (int i = 0; i < buffer.length; i++) {

            if (buffer[i] < threshold) {
                buffer[i] = 0;
            } else {
                buffer[i] = 1;
            }
        }

        float avgLength = 0;

        for (int i = 0; i < srcImage.getExtents()[0]; i++) {
            int segLength = 0;
            int noiseLength = 0;
            boolean stCounting = false;

            for (int j = 0; j < srcImage.getExtents()[1]; j++) {

                if (buffer[i + (j * srcImage.getExtents()[0])] > 0) {
                    segLength++;
                    stCounting = true;
                    noiseLength = 0;
                } else if (stCounting == true) {

                    if (noiseLength > noiseBound) {
                        break;
                    }

                    noiseLength++;
                }
            }

            avgLength += segLength;
        }

        avgLength = avgLength / srcImage.getExtents()[0];

        // calc length in pixels of each vertical line
        // calc avg length of line and report it.
        // System.out.println( "Average length = " + avgLength );
        ViewUserInterface.getReference().setGlobalDataText(srcImage.getImageName() + " Average length = " + avgLength +
                                                           "\n");
        setCompleted(true);
    }

}
