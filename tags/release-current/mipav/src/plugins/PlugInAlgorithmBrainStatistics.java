import gov.nih.mipav.model.algorithms.*;

import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * Calculates statistics of classified areas of a brain segmentation (background, csf, grey, white).
 *
 * @author  Evan McCreedy
 */
public class PlugInAlgorithmBrainStatistics extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The image from which to gather statistics. */
    private ModelImage brainImage;

    /** The user interface. */
    private ViewUserInterface UI;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Only support grayscale images (for now at least).
     *
     * @param  img  DOCUMENT ME!
     */
    // private int RGBOffset = 1;

    /**
     * Initialize the brain statistics algorithm.
     *
     * @param  img  the image to gather statistics of
     */
    public PlugInAlgorithmBrainStatistics(ModelImage img) {
        brainImage = img;
        UI = ViewUserInterface.getReference();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Calculate statistics.
     */
    public void calcStats() {
        int bins = 256;
        int[] histoBuffer = new int[bins];
        double[] lowValue;
        double[] highValue;
        double imageMax, imageMin;
        float[] imgBuffer;
        int z, zStop;
        int value;
        int length;
        double divisor;
        double factor;
        int i;
        boolean calcLowHigh = false;

        switch (brainImage.getType()) {

            case ModelStorageBase.BYTE:
                imageMin = -128;
                imageMax = 127;
                break;

            case ModelStorageBase.UBYTE:
                imageMin = 0;
                imageMax = 255;
                break;

            case ModelStorageBase.SHORT:
                imageMin = (double) brainImage.getMin();
                imageMax = (double) brainImage.getMax();
                break;

            default: {
                imageMin = (double) brainImage.getMin();
                imageMax = (double) brainImage.getMax();
                break;
            }
        }

        length = brainImage.getSliceSize();
        imgBuffer = new float[length];
        lowValue = new double[bins];
        highValue = new double[bins];

        if ((brainImage.getType() != ModelStorageBase.BYTE) && (brainImage.getType() != ModelStorageBase.UBYTE) &&
                (brainImage.getType() != ModelStorageBase.ARGB)) {
            calcLowHigh = true;
        }

        if (brainImage.getNDims() > 2) {
            zStop = brainImage.getExtents()[2];
        } else {
            zStop = 1;
        }

        try {
            brainImage.setLock(ModelStorageBase.W_LOCKED);
        } catch (IOException error) {
            errorCleanUp("Algorithm Histogram: Image locked", false);

            return;
        }

        for (z = 0; z < zStop; z++) {

            try {

                if (brainImage.getType() == ModelStorageBase.COMPLEX) {
                    brainImage.exportMagData(2 * z * length, length, imgBuffer);
                } else {
                    brainImage.exportDataNoLock(z * length, length, imgBuffer);
                }
            } catch (IOException error) {
                errorCleanUp("Algorithm Histogram: image bounds exceeded", false);
                brainImage.releaseLock();

                return;
            }

            if ((brainImage.getType() == ModelStorageBase.COMPLEX) && (brainImage.getLogMagDisplay() == true)) {

                for (i = 0; i < length; i++) {
                    imgBuffer[i] = (float) (0.4342944819 * java.lang.Math.log(1 + imgBuffer[i]));
                }
            }

            divisor = imageMax - imageMin;

            if (divisor == 0) {
                divisor = 1;
            }

            factor = (bins - 1) / divisor;
            // This is the part that actually calculates the histogram

            // Calculate for the entire image
            for (i = 0; i < length; i++) {
                value = (int) (((imgBuffer[i] - imageMin) * factor) + 0.5f);
                histoBuffer[value]++;

                if (calcLowHigh) {

                    if (histoBuffer[value] == 1) {
                        lowValue[value] = imgBuffer[i];
                        highValue[value] = imgBuffer[i];
                    } // if histoBuffer[value] == 1)
                    else {

                        if (imgBuffer[i] < lowValue[value]) {
                            lowValue[value] = imgBuffer[i];
                        } else if (imgBuffer[i] > highValue[value]) {
                            highValue[value] = imgBuffer[i];
                        }
                    } // else
                } // if (calcLowHigh)
            }

            fireProgressStateChanged(Math.round((float) (z + 1) / zStop * 100));

        }

        brainImage.releaseLock();

        if (brainImage.getUnitsOfMeasure()[0] != Unit.MILLIMETERS.getLegacyNum()) {
            MipavUtil.displayError("Image units are not in millimeters.  It is in " + Unit.getUnitFromLegacyNum(brainImage.getUnitsOfMeasure()[0]).toString() + ".");

            return;
        }

        // UI.setDataText( "Image\tCSF\tGM\tWM\tBPF\t(units in cubic " + unitsStr + ")\n" );
        // UI.setGlobalDataText( "Image\tCSF\tGM\tWM\tBPF\t(units in cubic " + unitsStr + ")\n" );

        String dataString = brainImage.getImageName();
        float[] res = brainImage.getFileInfo()[0].getResolutions();
        float pixelSize = res[0] * res[1] * res[2];
        int curClass = 0;
        float matterTotal = 0;
        float brainTotal = 0;

        for (i = 0; i < bins; i++) {

            if (histoBuffer[i] >= 1) {

                // skip the background
                if (curClass > 0) {
                    dataString += "\t" + (pixelSize * histoBuffer[i]);

                    if (curClass > 1) {
                        matterTotal += pixelSize * histoBuffer[i];
                    }

                    brainTotal += pixelSize * histoBuffer[i];
                }

                curClass++;
            }
        }

        dataString += "\t" + (matterTotal / brainTotal);
        dataString += "\n";

        UI.setDataText(dataString);
        UI.setGlobalDataText(dataString);


        setCompleted(true);
        imgBuffer = null;
        histoBuffer = null;
        lowValue = null;
        highValue = null;
        System.gc();
    }

    /**
     * Clean up the algorithm's memory.
     */
    public void finalize() {
        super.finalize();

        brainImage = null;
        UI = null;
    }

    /**
     * Calculate the classification statistics.
     */
    public void runAlgorithm() {

        if (brainImage == null) {
            displayError("Brain image is null.");

            return;
        }

        calcStats();
    }
}
