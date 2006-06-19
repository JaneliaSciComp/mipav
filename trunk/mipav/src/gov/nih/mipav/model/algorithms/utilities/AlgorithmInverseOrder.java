package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;


/**
 * Algorithm that inverses the order of 3D image dataset. That is the last image becomes the first the first image
 * becomes the last.
 *
 * @version  1.0 Dec 30, 1999
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class AlgorithmInverseOrder extends AlgorithmBase {

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs an algorithm object that inverses the order of 3D image dataset.
     *
     * @param  srcImg  source image model
     */
    public AlgorithmInverseOrder(ModelImage srcImg) {
        super(null, srcImg);
    }

    /**
     * Constructs an algorithm object that inverses the order of 3D image dataset.
     *
     * @param  srcImg    source image model
     * @param  progress  Progress mode - see AlgorithmBase.
     */
    public AlgorithmInverseOrder(ModelImage srcImg, int progress) {
        this(srcImg);
        progressMode = progress;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("InverseOrder.run(): Source Image is null");

            return;
        }

        if ((srcImage.getNDims() != 3) && (srcImage.getNDims() != 4)) {
            displayError("InverseOrder.run(): Source Image is not 3D or 4D");

            return;
        }

        constructLog();
        calcInPlace();
    }

    /**
     * Forms the reversed order image and places the result in original image.
     */
    private void calcInPlace() {

        int i, idx, s, t;
        int start;
        int length, totalLength, threeDLength;
        float[] buffer;
        float[] sliceBuffer;
        int sliceNumber;
        int tDim;

        try {

            if (srcImage.getNDims() == 4) {
                tDim = srcImage.getExtents()[3];
            } else {
                tDim = 1;
            }

            sliceNumber = srcImage.getExtents()[2];

            if (srcImage.isColorImage()) {
                length = 4 * srcImage.getSliceSize();
            } else {
                length = srcImage.getSliceSize();

            }

            threeDLength = length * srcImage.getExtents()[2];
            totalLength = threeDLength * tDim;
            buffer = new float[totalLength];
            sliceBuffer = new float[length];
            buildProgressBar(srcImage.getImageName(), "Reordering image ...", 0, 100);
        } catch (OutOfMemoryError e) {
            buffer = null;
            sliceBuffer = null;
            ;
            errorCleanUp("Algorithm Inverse order: Out of memory", true);

            return;
        }

        initProgressBar();

        for (t = 0; (t < tDim) && !threadStopped; t++) {

            for (s = 0; (s < sliceNumber) && !threadStopped; s++) {

                if (isProgressBarVisible()) {
                    progressBar.updateValue(Math.round((float) ((t * sliceNumber) + s) / ((tDim * sliceNumber) - 1) *
                                                           100), runningInSeparateThread);
                }

                start = s * length;

                try {
                    srcImage.exportData((t * threeDLength) + start, length, sliceBuffer); // locks and releases lock
                } catch (IOException error) {
                    buffer = null;
                    sliceBuffer = null;
                    errorCleanUp("Algorithm Inverse order: Image(s) locked", true);

                    return;
                }

                for (i = 0, idx = ((t + 1) * threeDLength) - length - start; (i < length) && !threadStopped;
                         i++, idx++) {
                    buffer[idx] = sliceBuffer[i];
                }
            } // for (s = 0; s < sliceNumber; s++)

        } // for (t = 0; t < tDim; t++)

        if (threadStopped) {
            buffer = null;
            sliceBuffer = null;
            finalize();

            return;
        }

        // Reorder fileinfo stuff
        FileInfoBase[] fileInfo;
        FileInfoBase[] fileInfoNew = new FileInfoBase[sliceNumber * tDim];
        fileInfo = srcImage.getFileInfo();

        int orient = FileInfoBase.oppositeOrient(fileInfo[0].getAxisOrientation(2));

        for (t = 0; (t < tDim) && !threadStopped; t++) {

            for (s = 0, idx = sliceNumber - 1; (s < sliceNumber) && !threadStopped; s++, idx--) {
                fileInfoNew[(t * sliceNumber) + s] = fileInfo[(t * sliceNumber) + idx];
                fileInfoNew[(t * sliceNumber) + s].setAxisOrientation(orient, 2);
            }
        }

        for (t = 0; (t < tDim) && !threadStopped; t++) {

            for (s = 0; (s < sliceNumber) && !threadStopped; s++) {
                srcImage.setFileInfo(fileInfoNew[(t * sliceNumber) + s], (t * sliceNumber) + s);
            }
        }

        if (threadStopped) {
            srcImage.setFileInfo(fileInfo);
            buffer = null;
            sliceBuffer = null;
            finalize();

            return;
        }

        try {
            srcImage.importData(0, buffer, true);
        } catch (IOException error) {
            buffer = null;
            sliceBuffer = null;
            errorCleanUp("Algorithm Inverse order: Image(s) locked", false);

            return;
        }

        if (threadStopped) {
            buffer = null;
            sliceBuffer = null;
            finalize();

            return;
        }

        srcImage.calcMinMax();
        disposeProgressBar();
        setCompleted(true);
    }

    /**
     * Constructs a string of the contruction parameters and outputs the string to the messsage frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {
        historyString = new String("InverseOrder()\n");
    }

}
