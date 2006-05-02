import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;


/**
 * This shows how to extend the AlgorithmBase class.
 *
 * <p>Supports the segmentation CT scans: Fat: -190 to -30 Low density muscle: 0 to 30 High density muscle: 31 to 100 If
 * you have any questions, please drop me a line. ===== Matthew J. Delmonico, MS, MPH Graduate Research Assistant,
 * Exercise Physiology 2132 HHP Building University of Maryland College Park, MD 20742 (301) 405-2569 (301) 793-0567
 * (cell)</p>
 *
 * @version  July 12, 2002
 * @author   DOCUMENT ME!
 * @see      AlgorithmBase
 *
 *           <p>$Logfile: /mipav/src/plugins/PlugInAlgorithmCT_MD.java $ $Revision: 13 $ $Date: 11/16/05 5:11p $</p>
 */
public class PlugInAlgorithmCT_MD extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public int fatH = -30;

    /** DOCUMENT ME! */
    public int fatL = -190;

    /** DOCUMENT ME! */
    public int hdmH = 100;

    /** DOCUMENT ME! */
    public int hdmL = 31;

    /** DOCUMENT ME! */
    public int ldmH = 30;

    /** DOCUMENT ME! */
    public int ldmL = 0;


    /** DOCUMENT ME! */
    private boolean entireImage = true;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for 3D images in which changes are placed in a predetermined destination image.
     *
     * @param  destImg  Image model where result image is to stored.
     * @param  srcImg   Source image model.
     */
    public PlugInAlgorithmCT_MD(ModelImage destImg, ModelImage srcImg) {
        super(destImg, srcImg);
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

        if (destImage == null) {
            displayError("Source Image is null");

            return;
        }

        if (destImage != null) { // if there exists a destination image

            if (srcImage.getNDims() == 2) {
                calcStoreInDest2D();
            } else if (srcImage.getNDims() > 2) {
                calcStoreInDest3D();
            }
        }
    }

    /**
     * This function produces a new image that has been median filtered and places filtered image in the destination
     * image.
     */
    private void calcStoreInDest2D() {

        int length; // total number of data-elements (pixels) in image
        float[] buffer; // data-buffer (for pixel data) which is the "heart" of the image


        try {

            // image length is length in 2 dims
            length = srcImage.getExtents()[0] * srcImage.getExtents()[1];
            buffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm CT_MD reports: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm CT_MD reports: out of memory", true);

            return;
        }

        int mod = length / 100; // mod is 1 percent of length
        initProgressBar();

        // Fat:  -190 to -30
        // Low density muscle:  0 to 30
        // High density muscle:  31 to 100
        BitSet mask = null;

        if (srcImage.getVOIs().size() > 0) {
            mask = srcImage.generateVOIMask();
            entireImage = false;
        }

        int fat = 0;
        int ldMuscle = 0;
        int hdMuscle = 0;

        for (int i = 0; (i < length) && !threadStopped; i++) {

            if (isProgressBarVisible() && (((i) % mod) == 0)) {
                progressBar.updateValue(Math.round((float) (i) / (length - 1) * 100), activeImage);
            }

            if ((entireImage == true) || mask.get(i)) {

                if ((buffer[i] >= fatL) && (buffer[i] <= fatH)) {
                    destImage.set(i, 20);
                    fat++;
                } else if ((buffer[i] >= ldmL) && (buffer[i] <= ldmH)) {
                    destImage.set(i, 40);
                    ldMuscle++;
                } else if ((buffer[i] >= hdmL) && (buffer[i] <= hdmH)) {
                    destImage.set(i, 60);
                    hdMuscle++;
                } else {
                    destImage.set(i, 0);
                    // buffer[i] = (float)srcImage.getMin();
                }
            }
        }

        // destImage.releaseLock();

        if (threadStopped) {
            finalize();

            return;
        }

        float area = srcImage.getFileInfo()[0].getResolutions()[0] * srcImage.getFileInfo()[0].getResolutions()[1];

        destImage.getUserInterface().getMessageFrame().append("Number of Fat pixels = " + fat, ViewJFrameMessage.DATA);
        destImage.getUserInterface().getMessageFrame().append("  Area = " + (fat * area) + " mm^2\n",
                                                              ViewJFrameMessage.DATA);

        destImage.getUserInterface().getMessageFrame().append("Number of LDM pixels = " + ldMuscle,
                                                              ViewJFrameMessage.DATA);
        destImage.getUserInterface().getMessageFrame().append("  Area = " + (ldMuscle * area) + " mm^2\n",
                                                              ViewJFrameMessage.DATA);

        destImage.getUserInterface().getMessageFrame().append("Number of HDM pixels = " + hdMuscle,
                                                              ViewJFrameMessage.DATA);
        destImage.getUserInterface().getMessageFrame().append("  Area = " + (hdMuscle * area) + " mm^2\n",
                                                              ViewJFrameMessage.DATA);

        destImage.calcMinMax();
        setCompleted(true);
    }

    /**
     * This function produces a new volume image that has been median filtered. Image can be filtered by filtering each
     * slice individually, or by filtering using a kernel-volume.
     */
    private void calcStoreInDest3D() {

        int totLength, imgLength;
        float[] buffer;

        float vol = srcImage.getFileInfo()[0].getResolutions()[0] * srcImage.getFileInfo()[0].getResolutions()[1] *
                        srcImage.getFileInfo()[0].getResolutions()[2];

        try {

            // image totLength is totLength in 3 dims
            imgLength = srcImage.getSliceSize();
            totLength = srcImage.getSliceSize() * srcImage.getExtents()[2];
            buffer = new float[totLength];
            srcImage.exportData(0, totLength, buffer); // locks and releases lock
            buildProgressBar(srcImage.getImageName(), "Processing image ...", 0, 100);
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm CT_MD: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm CT_MD: Out of memory creating process buffer", true);

            return;
        }

        int totFat = 0;
        int totLdMuscle = 0;
        int totHdMuscle = 0;
        initProgressBar();

        for (int i = 0; (i < srcImage.getExtents()[2]) && !threadStopped; i++) {
            int fat = 0;
            int ldMuscle = 0;
            int hdMuscle = 0;

            if (isProgressBarVisible()) {
                progressBar.updateValue(Math.round((float) (i) / (srcImage.getExtents()[2] - 1) * 100), activeImage);
            }

            for (int j = 0; (j < imgLength) && !threadStopped; j++) {

                // System.out.println(" j = " + j);
                int index = (i * imgLength) + j;

                if ((buffer[index] >= fatL) && (buffer[index] <= fatH)) {
                    destImage.set(index, 60);
                    totFat++;
                    fat++;
                } else if ((buffer[index] >= ldmL) && (buffer[index] <= ldmH)) {
                    destImage.set(index, 120);
                    totLdMuscle++;
                    ldMuscle++;
                } else if ((buffer[index] >= hdmL) && (buffer[index] <= hdmH)) {
                    destImage.set(index, 200);
                    totHdMuscle++;
                    hdMuscle++;
                } else {
                    destImage.set(index, 0);
                    // buffer[i] = -1024;
                }
            }

            destImage.getUserInterface().getMessageFrame().append("\n\n ***************** Slice " + i +
                                                                  " totals ***************\n", ViewJFrameMessage.DATA);
            destImage.getUserInterface().getMessageFrame().append("Number of fat pixels = " + fat,
                                                                  ViewJFrameMessage.DATA);
            destImage.getUserInterface().getMessageFrame().append("  Volume = " + (fat * vol) + " mm^3\n",
                                                                  ViewJFrameMessage.DATA);

            destImage.getUserInterface().getMessageFrame().append("Number of LDM pixels = " + ldMuscle,
                                                                  ViewJFrameMessage.DATA);
            destImage.getUserInterface().getMessageFrame().append("  Volume = " + (ldMuscle * vol) + " mm^3\n",
                                                                  ViewJFrameMessage.DATA);

            destImage.getUserInterface().getMessageFrame().append("Number of HDM pixels = " + hdMuscle,
                                                                  ViewJFrameMessage.DATA);
            destImage.getUserInterface().getMessageFrame().append("  Volume = " + (hdMuscle * vol) + " mm^3\n",
                                                                  ViewJFrameMessage.DATA);
        }

        destImage.releaseLock();

        if (threadStopped) {
            finalize();

            return;
        }

        destImage.getUserInterface().getMessageFrame().append("\n ************************ Totals ********************\n",
                                                              ViewJFrameMessage.DATA);
        destImage.getUserInterface().getMessageFrame().append("Number of totFat pixels = " + totFat,
                                                              ViewJFrameMessage.DATA);
        destImage.getUserInterface().getMessageFrame().append("  Volume = " + (totFat * vol) + " mm^3\n",
                                                              ViewJFrameMessage.DATA);

        destImage.getUserInterface().getMessageFrame().append("Number of LDM pixels = " + totLdMuscle,
                                                              ViewJFrameMessage.DATA);
        destImage.getUserInterface().getMessageFrame().append("  Volume = " + (totLdMuscle * vol) + " mm^3\n",
                                                              ViewJFrameMessage.DATA);

        destImage.getUserInterface().getMessageFrame().append("Number of HDM pixels = " + totHdMuscle,
                                                              ViewJFrameMessage.DATA);
        destImage.getUserInterface().getMessageFrame().append("  Volume = " + (totHdMuscle * vol) + " mm^3\n",
                                                              ViewJFrameMessage.DATA);

        destImage.calcMinMax();
        progressBar.dispose();
        setCompleted(true);
    }

}
