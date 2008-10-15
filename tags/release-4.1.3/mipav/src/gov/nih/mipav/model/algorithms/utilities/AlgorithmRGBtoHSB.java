package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import java.awt.*;

import java.io.*;


/**
 * Simple algorithm that converts an RGB image to a hue, saturation and brightness greyscale images.
 *
 * @version  1.0 Dec 30, 1999
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class AlgorithmRGBtoHSB extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Destination image (gray type) to store the Brightness channel of the source image. */
    private ModelImage destImageB;

    /** Destination image (gray type) to store the Hue channel of the source image. */
    private ModelImage destImageH;

    /** Destination image (gray type) to store the Saturation channel of the source image. */
    private ModelImage destImageS;

    /** Source image RGB type image. */
    private ModelImage srcImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmRGBtoHSB object.
     *
     * @param  destImgH  image model where result image of the Red channel is to be stored
     * @param  destImgS  image model where result image of the Green channel is to be stored
     * @param  destImgB  image model where result image of the Blue channel is to be stored
     * @param  srcImg    source image model
     */
    public AlgorithmRGBtoHSB(ModelImage destImgH, ModelImage destImgS, ModelImage destImgB, ModelImage srcImg) {

        destImageH = destImgH; // Put results in red   destination image.
        destImageS = destImgS; // Put results in green destination image.
        destImageB = destImgB; // Put results in blue  destination image.
        srcImage = srcImg;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImageH = null;
        destImageS = null;
        destImageB = null;
        srcImage = null;
        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if ((srcImage == null) || (destImageH == null) || (destImageS == null) || (destImageB == null)) {
            displayError("RGBtoHSB.run(): Source  and/or Destination image is null");

            return;
        }

        if (srcImage.isColorImage() == false) {
            displayError("RGBtoHSB.run(): Source Image is not a RGB type");

            return;
        }

        
        calcStoreInDest();
    }

    /**
     * Calculates the new images.
     */
    private void calcStoreInDest() {

        int i, j, k, m;
        int id;
        int z, t, f;
        int offset, newOffset;
        int length, newImgLength; // total number of data-elements (pixels) in image
        float[] buffer; // data-buffer (for pixel data) which is the "heart" of the image
        float[] bufferDestH;
        float[] bufferDestS;
        float[] bufferDestB;

        try {
            length = 4 * srcImage.getSliceSize();
            buffer = new float[length];
            bufferDestH = new float[srcImage.getSliceSize()];
            bufferDestS = new float[srcImage.getSliceSize()];
            bufferDestB = new float[srcImage.getSliceSize()];
            fireProgressStateChanged(srcImage.getImageName(), "Forming new images ...");
        } catch (OutOfMemoryError e) {
            buffer = null;
            bufferDestH = null;
            bufferDestS = null;
            bufferDestB = null;
            System.gc();
            displayError("Algorithm RGBtoGrays reports: Out of memory when creating image buffer");
            setCompleted(false);

            return;
        }

        

        int mod = length / 20;

        if (srcImage.getNDims() == 5) {
            f = srcImage.getExtents()[4];
        } else {
            f = 1;
        }

        if (srcImage.getNDims() >= 4) {
            t = srcImage.getExtents()[3];
        } else {
            t = 1;
        }

        if (srcImage.getNDims() >= 3) {
            z = srcImage.getExtents()[2];
        } else {
            z = 1;
        }

        int totalLength = f * t * z * length;
        newImgLength = srcImage.getSliceSize();

        float[] hsb = new float[3];

        for (m = 0; (m < f) && !threadStopped; m++) {

            for (k = 0; (k < t) && !threadStopped; k++) {

                for (j = 0; (j < z) && !threadStopped; j++) {

                    try {
                        offset = (m * t * z * length) + (k * z * length) + (j * length);
                        newOffset = (m * t * z * newImgLength) + (k * z * newImgLength) + (j * newImgLength);
                        srcImage.exportData(offset, length, buffer); // locks and releases lock
                    } catch (IOException error) {
                        buffer = null;
                        bufferDestH = null;
                        bufferDestS = null;
                        bufferDestB = null;
                        displayError("Algorithm RGB to grays : Export image(s) locked");
                        setCompleted(false);

                        return;
                    }

                    for (i = 0, id = 0; (i < length) && !threadStopped; i += 4, id++) {

                        if (((i % mod) == 0)) {
                            fireProgressStateChanged(Math.round((float) (i + offset) / (totalLength - 1) * 100));
                        }

                        Color.RGBtoHSB((int) buffer[i + 1], (int) buffer[i + 2], (int) buffer[i + 3], hsb);

                        bufferDestH[id] = (1.0f - Math.abs(hsb[0] - .5f)) * 255f;
                        bufferDestS[id] = hsb[1] * 255f;
                        bufferDestB[id] = hsb[2] * 255f;
                    }

                    try {
                        destImageH.importData(newOffset, bufferDestH, false);
                        destImageS.importData(newOffset, bufferDestS, false);
                        destImageB.importData(newOffset, bufferDestB, false);
                    } catch (IOException error) {
                        displayError("Algorithm RGB to grays: Import image(s): " + error);
                        setCompleted(false);
                        

                        return;
                    }
                }
            } // k loop
        } // m loop

        if (threadStopped) {
            buffer = null;
            bufferDestH = null;
            bufferDestS = null;
            bufferDestB = null;
            finalize();

            return;
        }

        destImageH.calcMinMax();
        destImageS.calcMinMax();
        destImageB.calcMinMax();
        
        setCompleted(true);
    }
}
