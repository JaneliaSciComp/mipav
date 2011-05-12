package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;


/**
 * This algorithm averages together slices of a 3D image. The average used here is the mean - the sum of values divided
 * by the number of values. Under Slice Number 3, 5, 7, or All is chosen with All being the default. If All is chosen,
 * then a 2D image is created. If 3, 5, or 7 is chosen, then a 3D image is created. Only New image can be chosen if All
 * is selected since a 2D image cannot meaningfully replace a 3D image. If 3, 5, or 7 is chosen, New image is the
 * default, but either New image or Replace image can be chosen.
 *
 * <p>If All is chosen: newValue(x,y) = (sum over all slice of oldValue(x,y,slice))/number of slices</p>
 *
 * <p>If 3, 5, or 7 is chosen: offset = (averagingNumber - 1)/2 newValue(x,y,sliceNumber) = (sum from slice =
 * Maximum(first slice number,sliceNumber - offset) to slice = Minimum(last slice number,sliceNumber + offset) of
 * oldValue(x,y,slice))/ number of slices in summation The above equations prevent the inclusion of slice numbers less
 * than the first slice number or greater than the last slice number in the averaging equation. For example, when
 * averaging on the first slice with an averagingNumber = 7, only include the first slice and the three slices above it
 * in the averaging. When averaging on the last slice with averagingNumber = 5, only include the last slice and the two
 * slices below it in the averaging.</p>
 */

public class AlgorithmSliceAveraging extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int averagingNumber;


    /** DOCUMENT ME! */
    private boolean haveColor;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmSliceAveraging object.
     *
     * @param  srcImg           source image model
     * @param  averagingNumber  number of slices to average together, if 1 average all slices together
     */
    public AlgorithmSliceAveraging(ModelImage srcImg, int averagingNumber) {

        super(null, srcImg);
        this.averagingNumber = averagingNumber;
    }

    /**
     * Creates a new AlgorithmSliceAveraging object.
     *
     * @param  destImg          image model where result image is to stored
     * @param  srcImg           source image model
     * @param  averagingNumber  number of slices to average together, if 1 average all slices together
     */
    public AlgorithmSliceAveraging(ModelImage destImg, ModelImage srcImg, int averagingNumber) {

        super(destImg, srcImg);
        this.averagingNumber = averagingNumber;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {

        super.finalize();
    }


    /**
     * Start algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            errorCleanUp("PComponent: Source Image is null", false);

            return;
        }

        if (srcImage.isColorImage()) {
            haveColor = true;
        } else {
            haveColor = false;
        }

        if (srcImage.getNDims() != 3) {
            errorCleanUp("Slice averaging: Source image must be 3D", false);

            return;
        }

        
        fireProgressStateChanged(srcImage.getImageName(), "Slice averaging ...");
        

        sAveraging();

        if (threadStopped) {
            finalize();

            return;
        }
    }

    /**
     * Averages image slices together.
     */
    private void sAveraging() {
        float[] buffer;
        int offset;
        int includeNumber;
        int sliceSize;
        int i, j;
        int z;
        int zDim;
        float[] values;
        int totalLength;


        zDim = srcImage.getExtents()[2];

        sliceSize = srcImage.getExtents()[0] * srcImage.getExtents()[1];

        if (haveColor) {
            totalLength = 4 * sliceSize * zDim;
        } else {
            totalLength = sliceSize * zDim;
        }

        fireProgressStateChanged("Exporting source data");

        try {
            values = new float[totalLength];
            srcImage.exportData(0, totalLength, values); // locks and releases lock
        } catch (IOException error) {
            values = null;
            errorCleanUp("Algorithm SliceAveraging: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError error) {
            values = null;
            errorCleanUp("Algorithm SliceAveraging: Out of memory.", true);

            return;
        }

        fireProgressStateChanged(10);

        fireProgressStateChanged("Averaging data");

        try {

            if (averagingNumber == 1) { // average all slices into 1 2D image

                if (haveColor) {
                    buffer = new float[4 * sliceSize];

                    for (i = 0; i < buffer.length; i++) {
                        buffer[i] = 0.0f;
                    }

                    for (i = 0; (i < (4 * sliceSize)) && !threadStopped; i++) {

                        for (z = 0; z < zDim; z++) {
                            buffer[i] += values[(4 * z * sliceSize) + i];
                        }

                        buffer[i] /= zDim;
                    }
                } // if (haveColor)
                else { // not color
                    buffer = new float[sliceSize];

                    for (i = 0; i < buffer.length; i++) {
                        buffer[i] = 0.0f;
                    }

                    for (i = 0; (i < sliceSize) && !threadStopped; i++) {

                        for (z = 0; z < zDim; z++) {
                            buffer[i] += values[(z * sliceSize) + i];
                        }

                        buffer[i] /= zDim;
                    }
                } // else not color
            } // if (averagingNumber == 1)
            else { // averagingNumber != 1
                offset = (averagingNumber - 1) / 2;

                if (haveColor) {
                    buffer = new float[4 * sliceSize * zDim];

                    for (i = 0; i < buffer.length; i++) {
                        buffer[i] = 0.0f;
                    }

                    for (i = 0; (i < (4 * sliceSize)) && !threadStopped; i++) {

                        for (z = 0; z < zDim; z++) {
                            includeNumber = 0;

                            for (j = -offset; j <= offset; j++) {

                                if (((z + j) >= 0) && ((z + j) < zDim)) {
                                    includeNumber++;
                                    buffer[i + (4 * sliceSize * z)] += values[(4 * (z + j) * sliceSize) + i];
                                }
                            }

                            buffer[i + (4 * sliceSize * z)] /= includeNumber;
                        }
                    }
                } // if (haveColor)
                else { // not color
                    buffer = new float[sliceSize * zDim];

                    for (i = 0; i < buffer.length; i++) {
                        buffer[i] = 0.0f;
                    }

                    for (i = 0; (i < sliceSize) && !threadStopped; i++) {

                        for (z = 0; z < zDim; z++) {
                            includeNumber = 0;

                            for (j = -offset; j <= offset; j++) {

                                if (((z + j) >= 0) && ((z + j) < zDim)) {
                                    includeNumber++;
                                    buffer[i + (sliceSize * z)] += values[((z + j) * sliceSize) + i];
                                }
                            }

                            buffer[i + (sliceSize * z)] /= includeNumber;
                        }
                    }
                } // else not color
            } // else averagingNumber != 1
        } catch (OutOfMemoryError error) {
            buffer = null;
            values = null;
            errorCleanUp("Algorithm SliceAveraging: Out of memory.", true);

            return;
        }

        fireProgressStateChanged(90);

        fireProgressStateChanged("Importing averaged data");

        if (destImage != null) {

            try {
                destImage.importData(0, buffer, true);
            } catch (IOException error) {
                buffer = null;
                values = null;
                errorCleanUp("AlgorithmSliceAveraging: IOException on destination image import data", true);

                return;
            }
        } // if (destImage != null)
        else { // destImage == null

            try {
                srcImage.importData(0, buffer, true);
            } catch (IOException error) {
                buffer = null;
                values = null;
                errorCleanUp("AlgorithmSliceAveraging: IOException on source image import data", true);

                return;
            }
        } // else destImage == null

        
        setCompleted(true);
    }


}
