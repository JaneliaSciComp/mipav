package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * Quantify image data based on a mask image. At present only calculates the the number of voxels, volume, and total
 * intensity for an object defined in a mask image.
 *
 * @version  0.1 Feb 11, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class AlgorithmQuantify extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * The mask image. The range of the image [0, positive integer value] where zero is the background and a positive
     * number indicates that is part of an object that has the same positive integer number.
     */
    private ModelImage maskImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmQuantify object.
     *
     * @param  srcImg   image model where result image is to stored
     * @param  maskImg  source image model
     */
    public AlgorithmQuantify(ModelImage srcImg, ModelImage maskImg) {

        super(null, srcImg);
        maskImage = maskImg; // Put results in destination image.
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        maskImage = null;
        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if ((srcImage == null) || (maskImage == null)) {
            displayError("Source and/or Mask Image is null");

            return;
        }

        // add test for mask image >= 0 and < 32K else fail

        if (srcImage.getNDims() == 2) {
            calc2D();
        } else if (srcImage.getNDims() == 3) {
            calc3D();
        }
    }

    /**
     * Calculates the information about the 2D mask.
     */
    private void calc2D() {

        int i;
        int length;
        float[] buffer;
        MaskObject[] objs = null;

        try {
            length = srcImage.getSliceSize();
            buffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            buildProgressBar(srcImage.getImageName(), "Quantify ...", 0, 100);
        } catch (IOException error) {
            buffer = null;
            System.gc();
            displayError("Algorithm Quantify: Image(s) locked");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            System.gc();
            displayError("Algorithm Quantify: Out of memory");
            setCompleted(false);

            return;
        }

        int mod = length / 25; // mod is 4 percent of length

        initProgressBar();

        objs = new MaskObject[(int) maskImage.getMax() + 1];

        for (i = 0; i < objs.length; i++) {
            objs[i] = new MaskObject();
        }

        int objID;

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0) && isProgressBarVisible()) {
                progressBar.updateValue(Math.round((float) i / (length - 1) * 100), runningInSeparateThread);
            }

            objID = maskImage.getShort(i);

            if (objID != 0) {
                objs[objID].nVoxels++;
                objs[objID].totalIntensity += buffer[i];
            }
        }

        if (threadStopped) {
            finalize();

            return;
        }

        String mStr = srcImage.getFileInfo(0).getVolumeUnitsOfMeasureStr();

        srcImage.getUserInterface().setDataText("\n Output from image quantify based on mask. ");
        srcImage.getUserInterface().setDataText("\n" + " Object \t# of pixles\tTotal Intensity\tArea(" + mStr + ")\n");

        for (i = 1; i < objs.length; i++) {
            objs[i].output(srcImage, i);
        }
        // output object data.

        disposeProgressBar();
        setCompleted(true);
    }

    /**
     * Calculates the information about the 3D mask.
     */
    private void calc3D() {

        int i, z;
        int objID;
        int length, offset;
        float[] buffer;
        MaskObject[] objs = null;

        try {
            length = srcImage.getSliceSize();
            buffer = new float[length];
            buildProgressBar(srcImage.getImageName(), "Quantify ...", 0, 100);
        } catch (OutOfMemoryError e) {
            buffer = null;
            System.gc();
            displayError("Algorithm Quantify: Out of memory");
            setCompleted(false);

            return;
        }

        initProgressBar();
        objs = new MaskObject[(int) maskImage.getMax() + 1];

        for (i = 0; i < objs.length; i++) {
            objs[i] = new MaskObject();
        }

        for (z = 0; (z < srcImage.getExtents()[2]) && !threadStopped; z++) {

            try {
                srcImage.exportData(z * length, length, buffer); // locks and releases lock
            } catch (IOException error) {
                displayError("Algorithm Quantify: Image(s) locked");
                setCompleted(false);

                return;
            }

            progressBar.updateValue(Math.round((float) z / (srcImage.getExtents()[2] - 1) * 100), runningInSeparateThread);
            offset = z * length;

            for (i = 0; i < length; i++) {
                objID = maskImage.getShort(offset + i);

                if (objID != 0) {
                    objs[objID].nVoxels++;
                    objs[objID].totalIntensity += buffer[i];
                }
            }
        }

        if (threadStopped) {
            finalize();

            return;
        }

        String mStr;
        mStr = srcImage.getFileInfo(0).getVolumeUnitsOfMeasureStr();
        srcImage.getUserInterface().setDataText("\n Output from image quantify based on mask. ");
        srcImage.getUserInterface().setDataText("\n" + " Object \t# of pixles\tTotal Intensity\tVolume(" + mStr +
                                                ")\n");

        for (i = 1; i < objs.length; i++) {
            objs[i].output(srcImage, i);
        }

        disposeProgressBar();
        setCompleted(true);
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Private class to AlgorithmQuantify used to store the number of voxels, volume, and total intensity for an object
     * defined in a mask image. This class also has a function to output the statistics calculated.
     */
    private class MaskObject {

        /** DOCUMENT ME! */
        public int nVoxels = 0;

        /** DOCUMENT ME! */
        public float totalIntensity = 0;

        /** DOCUMENT ME! */
        public float volume = 0;

        /**
         * Outputs the area (2D) or volume (3D) the user interface for the specificed image and object.
         *
         * @param  image  the source image for the calculation
         * @param  objNo  the object for which the information was calculated.
         */
        public void output(ModelImage image, int objNo) {
            float area, volume;

            if (image.getNDims() == 2) {
                area = (nVoxels * image.getFileInfo(0).getResolutions()[0] * image.getFileInfo(0).getResolutions()[1]);
                ViewUserInterface.getReference().setDataText("    " + objNo + "\t" + +nVoxels + "\t" + totalIntensity + "\t\t" + +area + "\n");
            } else {

                volume = (nVoxels * image.getFileInfo(0).getResolutions()[0] *
                              image.getFileInfo(0).getResolutions()[1] * image.getFileInfo(0).getResolutions()[2]);
                ViewUserInterface.getReference().setDataText("    " + objNo + "\t" + +nVoxels + "\t" + totalIntensity + "\t\t" + +volume + "\n");
            }
        }
    }


}
