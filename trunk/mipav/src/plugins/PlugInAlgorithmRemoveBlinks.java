import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.plugins.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;


/**
 * DOCUMENT ME!
 */
public class PlugInAlgorithmRemoveBlinks extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private double[] averages = null;

    /** DOCUMENT ME! */
    private double[] buffer = null;

    /** DOCUMENT ME! */
    private boolean calcInPlace = false;

    /** DOCUMENT ME! */
    private double percentage = 80;

    /** DOCUMENT ME! */
    private boolean[] removedSlices = null;

    /** DOCUMENT ME! */
    private double[] sliceMax = null;

    /** DOCUMENT ME! */
    private double thresholdDevsNorm = .6; // # of std deviations to allow for slices with a normal max

    /** DOCUMENT ME! */
    private double thresholdMax;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new PlugInAlgorithmRemoveBlinks object.
     *
     * @param  srcImg  image model where result image is to stored
     */
    public PlugInAlgorithmRemoveBlinks(ModelImage srcImg) {
        this.srcImage = srcImg;
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
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public ModelImage getResultImage() {
        return destImage;
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {
        constructLog();
        removeBlinks();
    }

    /**
     * Constructs a string of the contruction parameters and out puts the string to the messsage frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {
        historyString = new String("RemoveBlinks()\n");
    }

    /**
     * DOCUMENT ME!
     */
    private void removeBlinks() {

        int xDim, yDim;
        int numSlices;

        int length;
        int i, j;
        double totalMax;
        double totalMin;

        totalMin = srcImage.getMin();
        totalMax = srcImage.getMax();

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        numSlices = srcImage.getExtents()[2];

        thresholdMax = ((double) (percentage) / 100.0) * srcImage.getMax();

        float totalAverage = 0;

        try {
            length = xDim * yDim;
            buffer = new double[length];
            averages = new double[numSlices];
            sliceMax = new double[numSlices];
            removedSlices = new boolean[numSlices];
            buildProgressBar(srcImage.getImageName(), "Locating blinks ...", 0, 100);
        } catch (OutOfMemoryError e) {
            buffer = null;
            System.gc();
            displayError("Algorithm Blink Removal reports: Out of memory when creating image buffer");
            setCompleted(false);

            return;
        }

        initProgressBar();


        double mod = 100.0 / numSlices;
        int counter = 0;


        // export each slice into buffer and calculate the intensities
        for (i = 0; i < numSlices; i++) {

            try {
                srcImage.exportSliceXY(i, buffer);
                sliceMax[i] = buffer[0];

                for (j = 0; j < length; j++) {
                    averages[i] += buffer[j];

                    if (buffer[j] > sliceMax[i]) {
                        sliceMax[i] = buffer[j];
                    }
                }

                averages[i] /= length;
                progressBar.updateValue((int) ((i + 1) * mod));
            } catch (IOException ex) {
                System.err.println(ex.toString());
            }

            counter += length;
            totalAverage += averages[i];

        }

        // get the total average
        totalAverage /= numSlices;

        // find the sum of squares
        double sumOfSquares = 0;
        double deviation = 0;

        for (i = 0; i < numSlices; i++) {
            deviation = averages[i] - totalAverage;
            sumOfSquares += (deviation * deviation);
            counter++;

        }

        double standardDeviation = Math.sqrt(sumOfSquares / (numSlices - 1));

        int numNewSlices = 0;
        int numRemovedSlices = 0;

        boolean hitThreshold = false;

        for (i = 0; i < numSlices; i++) {
            counter++;

            // threshold has not been hit
            if (!hitThreshold) {

                // average is still below threshold
                if (averages[i] < (totalAverage * .75)) {

                    // if the slice max intensity is less than 50.. discard
                    if (sliceMax[i] < thresholdMax) {
                        removedSlices[i] = true;
                        numRemovedSlices++;

                        System.err.println("Removing slice (max inten of " + sliceMax[i] + " below " + thresholdMax +
                                           "): " + (i + 1));

                    } else {
                        removedSlices[i] = false;
                    }
                }
                // average is above threshold
                else {
                    hitThreshold = true;
                    removedSlices[i] = false;
                }
            }
            // threshold has been hit... will now use std deviation
            else {
                deviation = totalAverage - averages[i];

                // System.err.println((i + 1) + "Standard deviation (average): " + (deviation - standardDeviation));
                if ((deviation - standardDeviation) > thresholdDevsNorm) {
                    removedSlices[i] = true;
                    numRemovedSlices++;

                    // System.err.println("Removing slice: " + (i + 1) + "  Std devs away: " + (deviation -
                    // standardDeviation));

                } else {
                    removedSlices[i] = false;
                }
            }
        }

        numNewSlices = numSlices - numRemovedSlices;

        System.err.println("Removing " + numRemovedSlices + " slices from " + srcImage.getImageFileName());

        ViewUserInterface.getReference().getMessageFrame().append("Removing " + numRemovedSlices + " slices from " +
                                                                  srcImage.getImageFileName() + "\n",
                                                                  ViewJFrameMessage.DATA);

        for (j = 0; j < removedSlices.length; j++) {

            if (removedSlices[j]) {
                ViewUserInterface.getReference().getMessageFrame().append("\t" + (j + 1) + " slice removed\n",
                                                                          ViewJFrameMessage.DATA);
                System.err.println((j + 1) + " slice removed");
            }
        }

        disposeProgressBar();

        // determine the extents for the new movie System.err.println("Removing " + numRemovedSlices + " slices.  Result
        // image has " + numNewSlices + " slices");
        if ((numRemovedSlices > 0) && (numNewSlices > 0)) {

            if (!calcInPlace) {
                int[] extents = new int[3];
                extents[0] = srcImage.getExtents()[0];
                extents[1] = srcImage.getExtents()[1];
                extents[2] = numNewSlices;

                String name = JDialogBase.makeImageName(srcImage.getImageName(), "_no_blinks");
                destImage = new ModelImage(srcImage.getType(), extents, name);
            }

            AlgorithmRemoveSlices algoRS = new AlgorithmRemoveSlices(srcImage, destImage, removedSlices);
            algoRS.setRunningInSeparateThread(runningInSeparateThread);
            algoRS.run();

            if (algoRS.isCompleted()) {
                setCompleted(true);

                return;
            } else {
                setCompleted(false);

                return;
            }
        } else {

            if (numRemovedSlices == 0) {
                System.err.println("Algorithm will remove no slices");

            } else {
                System.err.println("Algorithm would remove all slices: aborting");
            }

            // because this can be used in a script, will just clone the image as the result
            String name = JDialogBase.makeImageName(srcImage.getImageName(), "_unchanged");
            destImage = new ModelImage(srcImage.getType(), srcImage.getExtents(), name);
        }

        setCompleted(true);
        notifyListeners(this);
    }

}
