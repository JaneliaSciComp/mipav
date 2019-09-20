package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;


/**
 * DOCUMENT ME!
 *
 * @author  pandyan
 *
 *          <p>This class is the main dialog for the CumulativeHistogram Algorithm The dialog is never visible</p>
 */
public class JDialogCumulativeHistogram extends JDialogBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2835835380784019616L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** MoelHistogram object.* */
    private ModelHistogram hist;

    /** int array of histogram values. */
    private int[] histoBuffer;

    /** Source Image. */
    private ModelImage image; // source image

    /** int vlaue indicating RGB channel. */
    private int RGBValue;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     *
     * @param  theParentFrame  DOCUMENT ME!
     * @param  im              DOCUMENT ME!
     */
    public JDialogCumulativeHistogram(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        callAlgorithm();

    }

    //~ Methods --------------------------------------------------------------------------------------------------------


    /**
     * method that is called after algorithm is finished.
     *
     * @param  algorithm  DOCUMENT ME!
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        int[] extents = { 256, 256 };
        ModelImage cumHistImage = null;

        if (image.isColorImage()) {
            String color = "";

            if (RGBValue == 1) {
                color = "Red";
            } else if (RGBValue == 2) {
                color = "Green";
            } else if (RGBValue == 3) {
                color = "Blue";
            }

            cumHistImage = new ModelImage(ModelStorageBase.INTEGER, extents,
                                          color + " Cumulative Histogram for " + image.getImageName());
        } else {
            cumHistImage = new ModelImage(ModelStorageBase.INTEGER, extents,
                                          "Cumulative Histogram for " + image.getImageName());
        }

        histoBuffer = new int[256];

        try {
            hist.exportData(0, 256, histoBuffer);
        } catch (IOException error) { }

        int min = histoBuffer[0];
        int max = histoBuffer[0];

        for (int i = 1; i < histoBuffer.length; i++) {

            if (histoBuffer[i] < min) {
                min = histoBuffer[i];
            }

            if (histoBuffer[i] > max) {
                max = histoBuffer[i];
            }
        }

        int range = max - min;

        for (int k = 0; k < histoBuffer.length; k++) {
            histoBuffer[k] = (int) (((float) (histoBuffer[k] - min) / range) * 255);

            int top = 255 - histoBuffer[k];

            for (int j = top; j < 256; j++) {
                cumHistImage.set(k, j, 255);
            }
        }

        cumHistImage.calcMinMax();

        try {
            new ViewJFrameImage(cumHistImage, null, new Dimension(610, 200));
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: unable to open new frame");
        } finally {
            hist = null;
            dispose();
        }
    }

    /**
     * Calls the CumulativeHistogram Algorithm.
     */
    private void callAlgorithm() {
        int[] dimExtentsA = new int[1];
        dimExtentsA[0] = 256;

        if (image.isColorImage()) {
            hist = new ModelHistogram(ModelStorageBase.INTEGER, dimExtentsA);

            AlgorithmCumulativeHistogram histoAlgoColorRed = new AlgorithmCumulativeHistogram(hist, 1, image);
            histoAlgoColorRed.setRunningInSeparateThread(false);
            histoAlgoColorRed.addListener(this);
            RGBValue = 1;
            histoAlgoColorRed.run();

            hist = new ModelHistogram(ModelStorageBase.INTEGER, dimExtentsA);

            AlgorithmCumulativeHistogram histoAlgoColorGreen = new AlgorithmCumulativeHistogram(hist, 2, image);
            histoAlgoColorGreen.setRunningInSeparateThread(false);
            histoAlgoColorGreen.addListener(this);
            RGBValue = 2;
            histoAlgoColorGreen.run();

            hist = new ModelHistogram(ModelStorageBase.INTEGER, dimExtentsA);

            AlgorithmCumulativeHistogram histoAlgoColorBlue = new AlgorithmCumulativeHistogram(hist, 3, image);
            histoAlgoColorBlue.setRunningInSeparateThread(false);
            histoAlgoColorBlue.addListener(this);
            RGBValue = 3;
            histoAlgoColorBlue.run();
        } else {
            hist = new ModelHistogram(ModelStorageBase.INTEGER, dimExtentsA);

            AlgorithmCumulativeHistogram histoAlgoA = new AlgorithmCumulativeHistogram(hist, image);
            histoAlgoA.setRunningInSeparateThread(false);
            histoAlgoA.addListener(this);
            histoAlgoA.run();
        }

    }


}
