package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;


/**
 * Algorithm that subtracts the mean or median value of a VOI from the image. If only 1 contour is present, that contour
 * is used whether or not it is selected. If multiple contours are present, at least 1 contour must be selected, and
 * only selected contours are used. Note that under MIPAV all selected contours must belong to the same VOI. If the new
 * image exceeds the range that can be stored in an image of that type the data is either clipped and stored in the
 * original image. Or a new image of a type (int, float...) that can store the range of new data is generated. For color
 * the dialog currently disables promotion for color because MIPAV cannot handle negative color values.
 */
public class AlgorithmSubtractVOI extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public static final int MEAN = 0;

    /** DOCUMENT ME! */
    public static final int MEDIAN = 1;

    /** DOCUMENT ME! */
    public static final int CLIP = 0; // clamp result data to the bounds of the input image type

    /** DOCUMENT ME! */
    public static final int PROMOTE = 1; // promote image type so that the range of the result fits into

    // the new image type. ( ie. byte to short).

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private double average;

    /** DOCUMENT ME! */
    private float averageB;

    /** DOCUMENT ME! */
    private float averageG;

    /** DOCUMENT ME! */
    private int averageMode = MEDIAN;

    /** DOCUMENT ME! */
    private float averageR;

    /** Maximum clipping modes if data result value exceeds the capacity of image's data type. */
    private double clipMax;

    /** DOCUMENT ME! */
    private float clipMaxC;

    /** Minimum clipping modes if data result value exceeds the capacity of image's data type. */
    private double clipMin;

    /** DOCUMENT ME! */
    private float clipMinC;

    /**
     * Clipping mode.
     *
     * <pre>
                          CLIP          = 0;   clamp result data to the bounds of the input image type
                          PROMOTE       = 1;   promote image type so that the range of the result fits into
                                               the new image type. ( ie. byte to short).
     *                   </pre>
     */
    private int clipMode = PROMOTE;

    /** Used to store the image maximum. */
    private double max;

    /** Used to store the image minimum. */
    private double min;

    /** DOCUMENT ME! */
    private double minR, minG, minB;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmSubtractVOI object.
     *
     * @param  srcImg        source image model
     * @param  _averageMode  mean or median of VOI
     * @param  _clipMode     clamp data to image type range or promote image type to hold new data range.
     */
    public AlgorithmSubtractVOI(ModelImage srcImg, int _averageMode, int _clipMode) {
        super(null, srcImg);
        averageMode = _averageMode;
        clipMode = _clipMode;
        setClipValues();
    }

    /**
     * Creates a new AlgorithmSubtractVOI object.
     *
     * @param  destImg       image model where result image is to stored
     * @param  srcImg        source image model
     * @param  _averageMode  mean or median of VOI
     * @param  _clipMode     clamp data to image type range or promote image type to hold new data range.
     */
    public AlgorithmSubtractVOI(ModelImage destImg, ModelImage srcImg, int _averageMode, int _clipMode) {
        super(destImg, srcImg);
        averageMode = _averageMode;
        clipMode = _clipMode;
        setClipValues();
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
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("AlgorithmSubtractVOI.run(): Source Image is null");

            return;
        }

        fireProgressStateChanged(srcImage.getImageName(), "Calculating image ...");
        

        

        if (destFlag == true) { // use a destination image.
            getAverage();

            if (!Double.isInfinite(average) && !Double.isNaN(average)) {

                if (srcImage.isColorImage()) {

                    if (averageMode == MEDIAN) {
                        Preferences.debug("median red = " + averageR + " median green = " + averageG +
                                          " median blue = " + averageB, Preferences.DEBUG_ALGORITHM);
                    } else {
                        Preferences.debug("mean red = " + averageR + " mean green = " + averageG + " mean blue = " +
                                          averageB, Preferences.DEBUG_ALGORITHM);
                    }

                    calcStoreInDestColor();
                } else {

                    if (averageMode == MEDIAN) {
                        Preferences.debug("median = " + average, Preferences.DEBUG_ALGORITHM);
                    } else {
                        Preferences.debug("mean = " + average, Preferences.DEBUG_ALGORITHM);
                    }

                    calcStoreInDest();
                }
            }
        } else {
            getAverage();

            if (!Double.isInfinite(average) && !Double.isNaN(average)) {

                if (srcImage.isColorImage()) {

                    if (averageMode == MEDIAN) {
                        Preferences.debug("median red = " + averageR + " median green = " + averageG +
                                          " median blue = " + averageB, Preferences.DEBUG_ALGORITHM);
                    } else {
                        Preferences.debug("mean red = " + averageR + " mean green = " + averageG + " mean blue = " +
                                          averageB, Preferences.DEBUG_ALGORITHM);
                    }

                    calcInPlaceColor();
                } else {

                    if (averageMode == MEDIAN) {
                        Preferences.debug("median = " + average, Preferences.DEBUG_ALGORITHM);
                    } else {
                        Preferences.debug("mean = " + average, Preferences.DEBUG_ALGORITHM);
                    }

                    calcInPlace();
                }
            }
        }
    }

    /**
     * Generates the new data and places in the source image.
     *
     * <p><b>note</b> that if this method is canceled part-way through a multi-sliced image, it will almost certainly be
     * partially adjusted. This is because to save memory buffer-space, it returns an adjusted slice back to the source
     * image after processing it; it then takes out the next slice to alter it. Canceling a job causes the current slice
     * will not be affected, but all previous slices will be.</p>
     */
    private void calcInPlace() {

        int i, j, k, m;
        int z, t, f;
        int offset;
        int length; // total number of data-elements (pixels) in image
        double[] buffer; // data-buffer (for pixel data) which is the "heart" of the image

        try {
            length = srcImage.getSliceSize();
            buffer = new double[length];
        } catch (OutOfMemoryError e) {
            buffer = null;
            System.gc();
            displayError("Algorithm Subtract VOI reports: Out of memory when creating image buffer");
            setCompleted(false);

            return;
        }

        min = srcImage.getMin();
        max = srcImage.getMax();

        int newType = srcImage.getType();

        if (clipMode == PROMOTE) {

            if (((min - average) < clipMin) || ((max - average) > clipMax)) {
                newType = findType(srcImage.getType());
            }

            if (newType != srcImage.getType()) {
                AlgorithmChangeType changeTypeAlgo = new AlgorithmChangeType(srcImage, newType, min, max, min, max,
                                                                             false);

                changeTypeAlgo.setRunningInSeparateThread(runningInSeparateThread);
                changeTypeAlgo.run();

                // if the change algo is halted,
                if (!changeTypeAlgo.isCompleted()) {

                    // halt the rest of this processing.
                    setThreadStopped(true);
                }
            }
        }

        // if the thread has been stopped, then prevent algo from continuing.
        if (threadStopped) {
            setCompleted(false);
            finalize();

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

        for (m = 0; (m < f) && !threadStopped; m++) {

            for (k = 0; (k < t) && !threadStopped; k++) {

                for (j = 0; (j < z) && !threadStopped; j++) {

                    try {
                        offset = (m * k * j * length) + (k * j * length) + (j * length);
                        srcImage.exportData(offset, length, buffer); // locks and releases lock
                    } catch (IOException error) {
                        displayError("Algorithm Subtract VOI : Image(s) locked");
                        setCompleted(false);

                        return;
                    }

                    for (i = 0; (i < length) && !threadStopped; i++) {

                        try {

                            if (((i % mod) == 0)) {
                                fireProgressStateChanged(Math.round(50 +
                                                                   ((float) (i + offset) / (totalLength - 1) * 50)));
                            }
                        } catch (NullPointerException npe) {

                            if (threadStopped) {
                                Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                                  Preferences.DEBUG_ALGORITHM);
                            }
                        }

                        buffer[i] = buffer[i] - average;
                    }

                    // clip check
                    if (clipMode == CLIP) {

                        for (i = 0; i < length; i++) {

                            if (buffer[i] > clipMax) {
                                buffer[i] = clipMax;
                            } else if (buffer[i] < clipMin) {
                                buffer[i] = clipMin;
                            }
                        }
                    }

                    try {

                        // do BEFORE buffer has been exported to Image
                        if (threadStopped) {
                            finalize();

                            return;
                        }

                        srcImage.importData(offset, buffer, false);
                    } catch (IOException error) {
                        displayError("Algorithm Subtract VOI: Image(s) locked");
                        setCompleted(false);
                        

                        return;
                    }
                }
            } // t loop
        } // f loop

        srcImage.calcMinMax();
        
        setCompleted(true);
    }

    /**
     * Generates the new data and places in the source image.
     *
     * <p><b>note</b> that if this method is canceled part-way through a multi-sliced image, it will almost certainly be
     * partially adjusted. This is because to save memory buffer-space, it returns an adjusted slice back to the source
     * image after processing it; it then takes out the next slice to alter it. Canceling a job causes the current slice
     * will not be affected, but all previous slices will be.</p>
     */
    private void calcInPlaceColor() {

        int i, j, k, m;
        int z, t, f;
        int offset;
        int length; // total number of data-elements (pixels) in image
        float[] buffer; // data-buffer (for pixel data) which is the "heart" of the image
        float[] totalBuffer;
        int totalLength;

        totalLength = 4;

        for (i = 0; i < srcImage.getNDims(); i++) {
            totalLength *= srcImage.getExtents()[i];
        }

        minR = srcImage.getMinR();
        minG = srcImage.getMinG();
        minB = srcImage.getMinB();

        int newType = srcImage.getType();

        if (clipMode == PROMOTE) {

            if (srcImage.getType() == ModelStorageBase.ARGB) {

                if (((minR - averageR) < 0) || ((minG - averageG) < 0) || ((minB - averageB) < 0)) {
                    newType = ModelStorageBase.ARGB_FLOAT;
                }
            } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {

                if (((minR - averageR) < 0) || ((minG - averageG) < 0) || ((minB - averageB) < 0)) {
                    newType = ModelStorageBase.ARGB_FLOAT;
                }
            }

            if (newType != srcImage.getType()) {

                try {
                    totalBuffer = new float[totalLength];
                } catch (OutOfMemoryError e) {
                    totalBuffer = null;
                    System.gc();
                    displayError("Algorithm Subtract VOI reports: Out of memory when creating totalBuffer");
                    setCompleted(false);

                    return;
                }

                try {
                    srcImage.exportData(0, totalLength, totalBuffer); // locks and releases lock
                } catch (IOException error) {
                    totalBuffer = null;
                    System.gc();
                    MipavUtil.displayError("IOException on srcImage.exportData");
                    setCompleted(false);

                    return;
                }

                srcImage.reallocate(newType);

                try {
                    srcImage.importData(0, totalBuffer, true);
                } catch (IOException error) {
                    totalBuffer = null;
                    System.gc();
                    MipavUtil.displayError("IOException on srcImage.importData");
                    setCompleted(false);

                    return;
                }

                totalBuffer = null;
                System.gc();
            } // if (newType != srcImage.getType())
        } // if (clipMode == PROMOTE)

        // if the thread has been stopped, then prevent algo from continuing.
        if (threadStopped) {
            setCompleted(false);
            finalize();

            return;
        }

        try {
            length = 4 * srcImage.getSliceSize();
            buffer = new float[length];
        } catch (OutOfMemoryError e) {
            buffer = null;
            System.gc();
            displayError("Algorithm Subtract VOI reports: Out of memory when creating image buffer");
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

        for (m = 0; (m < f) && !threadStopped; m++) {

            for (k = 0; (k < t) && !threadStopped; k++) {

                for (j = 0; (j < z) && !threadStopped; j++) {

                    try {
                        offset = (m * k * j * length) + (k * j * length) + (j * length);
                        srcImage.exportData(offset, length, buffer); // locks and releases lock
                    } catch (IOException error) {
                        displayError("Algorithm Subtract VOI : Image(s) locked");
                        setCompleted(false);

                        return;
                    }

                    for (i = 0; (i < length) && !threadStopped; i += 4) {

                        try {

                            if (((i % mod) == 0)) {
                                fireProgressStateChanged(Math.round(50 +
                                                                   ((float) (i + offset) / (totalLength - 1) * 50)));
                            }
                        } catch (NullPointerException npe) {

                            if (threadStopped) {
                                Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                                  Preferences.DEBUG_ALGORITHM);
                            }
                        }

                        buffer[i + 1] = buffer[i + 1] - averageR;
                        buffer[i + 2] = buffer[i + 2] - averageG;
                        buffer[i + 3] = buffer[i + 3] - averageB;
                    }

                    // clip check
                    if (clipMode == CLIP) {

                        for (i = 0; i < length; i++) {

                            if (buffer[i] > clipMaxC) {
                                buffer[i] = clipMaxC;
                            } else if (buffer[i] < clipMinC) {
                                buffer[i] = clipMinC;
                            }
                        }
                    }

                    try {

                        // do BEFORE buffer has been exported to Image
                        if (threadStopped) {
                            finalize();

                            return;
                        }

                        srcImage.importData(offset, buffer, false);
                    } catch (IOException error) {
                        displayError("Algorithm Subtract VOI: Image(s) locked");
                        setCompleted(false);
                        

                        return;
                    }
                } // j loop
            } // k loop
        } // m loop

        srcImage.calcMinMax();
        
        setCompleted(true);
    }

    /**
     * Generates the new data and places in a new (destination) image.
     *
     * <p>This method checks for thread halting actions more infrequently than the in-place method.</p>
     */
    private void calcStoreInDest() {

        int i, j, k, m;
        int z, t, f;
        int offset;
        int length; // total number of data-elements (pixels) in image
        double[] buffer; // data-buffer (for pixel data) which is the "heart" of the image

        try {
            length = srcImage.getSliceSize();
            buffer = new double[length];
        } catch (OutOfMemoryError e) {
            buffer = null;
            System.gc();
            displayError("Algorithm Subtract VOI reports: Out of memory when creating image buffer");
            setCompleted(false);

            return;
        }

        min = srcImage.getMin();
        max = srcImage.getMax();

        int newType = srcImage.getType();

        if (clipMode == PROMOTE) {

            if (((min - average) < clipMin) || ((max - average) > clipMax)) {
                newType = findType(srcImage.getType());
            }

            if (newType > destImage.getType()) {
                destImage.reallocate(newType);
            }
        }

        // do BEFORE starting progress bar
        if (threadStopped) {
            setCompleted(false);
            finalize();

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

        for (m = 0; (m < f) && !threadStopped; m++) {

            for (k = 0; (k < t) && !threadStopped; k++) {

                for (j = 0; (j < z) && !threadStopped; j++) {

                    try {
                        offset = (m * k * j * length) + (k * j * length) + (j * length);
                        srcImage.exportData(offset, length, buffer); // locks and releases lock
                    } catch (IOException error) {
                        displayError("Algorithm Subtract VOI : Image(s) locked");
                        setCompleted(false);
                        

                        return;
                    }

                    for (i = 0; (i < length) && !threadStopped; i++) {

                        try {

                            if (((i % mod) == 0)) {
                                fireProgressStateChanged(Math.round(50 +
                                                                   ((float) (i + offset) / (totalLength - 1) * 50)));
                            }
                        } catch (NullPointerException npe) {

                            if (threadStopped) {
                                Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                                  Preferences.DEBUG_ALGORITHM);
                            }
                        }

                        buffer[i] = buffer[i] - average;
                    }

                    // clip check
                    if (clipMode == CLIP) {

                        for (i = 0; i < length; i++) {

                            if (buffer[i] > clipMax) {
                                buffer[i] = clipMax;
                            } else if (buffer[i] < clipMin) {
                                buffer[i] = clipMin;
                            }
                        }
                    }

                    try {
                        destImage.importData(offset, buffer, false);
                    } catch (IOException error) {
                        displayError("Algorithm Subtract VOI: Destination Image locked");
                        setCompleted(false);
                        

                        return;
                    }

                }
            } // t loop
        } // f loop

        if (threadStopped) {
            setCompleted(false);
            
            finalize();

            return;
        }

        destImage.calcMinMax();
        
        setCompleted(true);
    }

    /**
     * Generates the new data and places in a new (destination) image.
     *
     * <p>This method checks for thread halting actions more infrequently than the in-place method.</p>
     */
    private void calcStoreInDestColor() {

        int i, j, k, m;
        int z, t, f;
        int offset;
        int length; // total number of data-elements (pixels) in image
        float[] buffer; // data-buffer (for pixel data) which is the "heart" of the image
        int totalLength;

        totalLength = 4;

        for (i = 0; i < srcImage.getNDims(); i++) {
            totalLength *= srcImage.getExtents()[i];
        }

        minR = srcImage.getMinR();
        minG = srcImage.getMinG();
        minB = srcImage.getMinB();

        int newType = srcImage.getType();

        if (clipMode == PROMOTE) {

            if (srcImage.getType() == ModelStorageBase.ARGB) {

                if (((minR - averageR) < 0) || ((minG - averageG) < 0) || ((minB - averageB) < 0)) {
                    newType = ModelStorageBase.ARGB_FLOAT;
                }
            } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {

                if (((minR - averageR) < 0) || ((minG - averageG) < 0) || ((minB - averageB) < 0)) {
                    newType = ModelStorageBase.ARGB_FLOAT;
                }
            }

            if (newType != srcImage.getType()) {
                destImage.reallocate(newType);
            } // if (newType != srcImage.getType())
        } // if (clipMode == PROMOTE)

        // if the thread has been stopped, then prevent algo from continuing.
        if (threadStopped) {
            setCompleted(false);
            finalize();

            return;
        }

        try {
            length = 4 * srcImage.getSliceSize();
            buffer = new float[length];
        } catch (OutOfMemoryError e) {
            buffer = null;
            System.gc();
            displayError("Algorithm Subtract VOI reports: Out of memory when creating image buffer");
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

        for (m = 0; (m < f) && !threadStopped; m++) {

            for (k = 0; (k < t) && !threadStopped; k++) {

                for (j = 0; (j < z) && !threadStopped; j++) {

                    try {
                        offset = (m * k * j * length) + (k * j * length) + (j * length);
                        srcImage.exportData(offset, length, buffer); // locks and releases lock
                    } catch (IOException error) {
                        displayError("Algorithm Subtract VOI : Image(s) locked");
                        setCompleted(false);

                        return;
                    }

                    for (i = 0; (i < length) && !threadStopped; i += 4) {

                        try {

                            if (((i % mod) == 0)) {
                                fireProgressStateChanged(Math.round(50 +
                                                                   ((float) (i + offset) / (totalLength - 1) * 50)));
                            }
                        } catch (NullPointerException npe) {

                            if (threadStopped) {
                                Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                                  Preferences.DEBUG_ALGORITHM);
                            }
                        }

                        buffer[i + 1] = buffer[i + 1] - averageR;
                        buffer[i + 2] = buffer[i + 2] - averageG;
                        buffer[i + 3] = buffer[i + 3] - averageB;
                    }

                    // clip check
                    if (clipMode == CLIP) {

                        for (i = 0; i < length; i++) {

                            if (buffer[i] > clipMaxC) {
                                buffer[i] = clipMaxC;
                            } else if (buffer[i] < clipMinC) {
                                buffer[i] = clipMinC;
                            }
                        }
                    }

                    try {

                        // do BEFORE buffer has been exported to Image
                        if (threadStopped) {
                            finalize();

                            return;
                        }

                        destImage.importData(offset, buffer, false);
                    } catch (IOException error) {
                        displayError("Algorithm Subtract VOI: Image(s) locked");
                        setCompleted(false);
                        

                        return;
                    }
                } // j loop
            } // k loop
        } // m loop

        destImage.calcMinMax();
        
        setCompleted(true);
    }

    /**
     * Find the type able to contain the full range of the data.
     *
     * @param   stType  starting type of image. Image will be promoted above this type if needed.
     *
     * @return  type capable of storing full range of data. See ModelStorageBase for image types.
     */
    private int findType(int stType) {
        boolean loop = true;
        int endType;

        endType = stType;

        while (loop == true) {

            if (testType(endType, min - average, max - average) == false) {
                endType = promoteType(endType);

                if (endType == ModelStorageBase.DOUBLE) {
                    loop = false;
                }
            } else {
                loop = false;
            }
        }

        return endType;

    }

    /**
     * DOCUMENT ME!
     */
    private void getAverage() {
        ViewVOIVector VOIs;
        int nVOI;
        int i, j, k, m;
        int z, t, f;
        int length;
        int offset;
        int mod;
        int nVOIContour = 0;
        Vector<VOIBase> contours;
        int nContours;
        int totalContours = 0;
        int activeContours = 0;
        int totalLength = 1;
        int contourVoxels = 0;
        double[] buffer;
        double[] contourBuffer;
        float[] bufferC;
        float[] contourBufferR;
        float[] contourBufferG;
        float[] contourBufferB;
        int p;
        double sum;

        for (i = 0; i < srcImage.getNDims(); i++) {
            totalLength *= srcImage.getExtents()[i];
        }

        mask = new BitSet(totalLength);
        VOIs = srcImage.getVOIs();
        nVOI = VOIs.size();

        if (nVOI == 0) {
            MipavUtil.displayError("Error! The source image had no VOIs");
            setCompleted(false);
            average = Double.NaN;

            return;
        }

        for (i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                nVOIContour++;
                contours = VOIs.VOIAt(i).getCurves();
                nContours = contours.size();
                totalContours += nContours;

                for (j = 0; j < nContours; j++) {

                    if (((VOIContour) (contours.elementAt(j))).isActive()) {
                        activeContours++;
                    }
                }
            }
        }

        if (nVOIContour == 0) {
            MipavUtil.displayError("Error! The source image had no contour VOIs");
            setCompleted(false);
            average = Double.NaN;

            return;
        }

        if (totalContours == 0) {
            MipavUtil.displayError("Error! The source image had no contours");
            setCompleted(false);
            average = Double.NaN;

            return;
        }

        if ((totalContours > 1) && (activeContours == 0)) {
            MipavUtil.displayError("Error! At least 1 of the contours present must be selected");
            setCompleted(false);
            average = Double.NaN;

            return;
        }

        if (totalContours == 1) {

            for (i = 0; i < nVOI; i++) {

                if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                    contours = VOIs.VOIAt(i).getCurves();
                    nContours = contours.size();

                    if (nContours == 1) {
                        VOIs.VOIAt(i).createBinaryMask3D(mask, srcImage.getExtents()[0], srcImage.getExtents()[1], false, false);
                    }
                }
            }
        } // if (totalContours == 1)
        else if (totalContours > 1) {

            for (i = 0; i < nVOI; i++) {

                if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                    contours = VOIs.VOIAt(i).getCurves();
                    nContours = contours.size();

                    if (nContours >= 1) {
                        VOIs.VOIAt(i).createActiveContourBinaryMask(mask, srcImage.getExtents()[0],
                                                                    srcImage.getExtents()[1]);
                    }
                }
            }
        }

        for (i = 0; i < totalLength; i++) {

            if (mask.get(i)) {
                contourVoxels++;
            }
        }

        if (srcImage.isColorImage()) {

            try {
                contourBufferR = new float[contourVoxels];
                contourBufferG = new float[contourVoxels];
                contourBufferB = new float[contourVoxels];
            } catch (OutOfMemoryError e) {
                contourBufferR = null;
                contourBufferG = null;
                contourBufferB = null;
                System.gc();
                displayError("Algorithm Subtract VOI getAverage: Out of memroy creating contour buffers");
                setCompleted(false);
                average = Double.NaN;

                return;
            }

            length = 4 * srcImage.getSliceSize();
            totalLength = 4;

            for (i = 0; i < srcImage.getNDims(); i++) {
                totalLength *= srcImage.getExtents()[i];
            }

            try {
                bufferC = new float[length];
            } catch (OutOfMemoryError e) {
                bufferC = null;
                System.gc();
                displayError("Algorithm Subtract VOI reports: Out of memory when creating image buffer");
                setCompleted(false);
                average = Double.NaN;

                return;
            }

            mod = length / 20;

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

            for (m = 0, p = 0; (m < f) && !threadStopped; m++) {

                for (k = 0; (k < t) && !threadStopped; k++) {

                    for (j = 0; (j < z) && !threadStopped; j++) {

                        try {
                            offset = (m * k * j * length) + (k * j * length) + (j * length);
                            srcImage.exportData(offset, length, bufferC); // locks and releases lock
                        } catch (IOException error) {
                            displayError("Algorithm Subtract VOI : Image(s) locked");
                            setCompleted(false);
                            average = Double.NaN;

                            return;
                        }

                        for (i = 0; (i < length) && !threadStopped; i += 4) {

                            try {

                                if (((i % mod) == 0)) {
                                    fireProgressStateChanged(Math.round((float) (i + offset) / (totalLength - 1) * 50));
                                }
                            } catch (NullPointerException npe) {

                                if (threadStopped) {
                                    Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                                      Preferences.DEBUG_ALGORITHM);
                                }
                            }

                            if (mask.get((i + offset) / 4)) {
                                contourBufferR[p] = bufferC[i + 1];
                                contourBufferG[p] = bufferC[i + 2];
                                contourBufferB[p++] = bufferC[i + 3];
                            }
                        } // for ( i = 0; i < length && !threadStopped; i++)
                    } // for (j = 0; j < z && !threadStopped; j++)
                } // for (k = 0; k < t && !threadStopped; k++)
            } // for (k = 0; k < t && !threadStopped; k++)

            bufferC = null;
            System.gc();

            if (averageMode == MEAN) {
                sum = 0.0;

                for (i = 0; i < contourVoxels; i++) {
                    sum += contourBufferR[i];
                }

                averageR = (float) (sum / contourVoxels);
                sum = 0.0;

                for (i = 0; i < contourVoxels; i++) {
                    sum += contourBufferG[i];
                }

                averageG = (float) (sum / contourVoxels);
                sum = 0.0;

                for (i = 0; i < contourVoxels; i++) {
                    sum += contourBufferB[i];
                }

                averageB = (float) (sum / contourVoxels);
            } // if (averageMode == MEAN)
            else { // averageMode == MEDIAN
            	Arrays.sort(contourBufferR);
            	Arrays.sort(contourBufferG);
            	Arrays.sort(contourBufferB);

                if (((contourVoxels / 2) * 2) != contourVoxels) {
                    averageR = contourBufferR[contourVoxels / 2];
                    averageG = contourBufferG[contourVoxels / 2];
                    averageB = contourBufferB[contourVoxels / 2];
                } else {
                    averageR = (contourBufferR[contourVoxels / 2] + contourBufferR[(contourVoxels / 2) - 1]) / 2.0f;
                    averageG = (contourBufferG[contourVoxels / 2] + contourBufferG[(contourVoxels / 2) - 1]) / 2.0f;
                    averageB = (contourBufferB[contourVoxels / 2] + contourBufferB[(contourVoxels / 2) - 1]) / 2.0f;
                }
            } // averageMode == MEDIAN
        } else { // black and white

            try {
                contourBuffer = new double[contourVoxels];
            } catch (OutOfMemoryError e) {
                contourBuffer = null;
                System.gc();
                displayError("Algorithm Subtract VOI getAverage: Out of memory when creating contourBuffer");
                setCompleted(false);
                average = Double.NaN;

                return;
            }

            length = srcImage.getSliceSize();

            try {
                buffer = new double[length];
            } catch (OutOfMemoryError e) {
                buffer = null;
                System.gc();
                displayError("Algorithm Subtract VOI reports: Out of memory when creating image buffer");
                setCompleted(false);
                average = Double.NaN;

                return;
            }

            mod = length / 20;

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

            for (m = 0, p = 0; (m < f) && !threadStopped; m++) {

                for (k = 0; (k < t) && !threadStopped; k++) {

                    for (j = 0; (j < z) && !threadStopped; j++) {

                        try {
                            offset = (m * k * j * length) + (k * j * length) + (j * length);
                            srcImage.exportData(offset, length, buffer); // locks and releases lock
                        } catch (IOException error) {
                            displayError("Algorithm Subtract VOI : Image(s) locked");
                            setCompleted(false);
                            average = Double.NaN;

                            return;
                        }

                        for (i = 0; (i < length) && !threadStopped; i++) {

                            try {

                                if (((i % mod) == 0)) {
                                    fireProgressStateChanged(Math.round((float) (i + offset) / (totalLength - 1) * 50));
                                }
                            } catch (NullPointerException npe) {

                                if (threadStopped) {
                                    Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                                      Preferences.DEBUG_ALGORITHM);
                                }
                            }

                            if (mask.get(i + offset)) {
                                contourBuffer[p++] = buffer[i];
                            }
                        } // for ( i = 0; i < length && !threadStopped; i++)
                    } // for (j = 0; j < z && !threadStopped; j++)
                } // for (k = 0; k < t && !threadStopped; k++)
            } // for (k = 0; k < t && !threadStopped; k++)

            buffer = null;
            System.gc();

            if (averageMode == MEAN) {
                sum = 0.0;

                for (i = 0; i < contourVoxels; i++) {
                    sum += contourBuffer[i];
                }

                average = sum / contourVoxels;
            } // if (averageMode == MEAN)
            else { // averageMode == MEDIAN
            	Arrays.sort(contourBuffer);

                if (((contourVoxels / 2) * 2) != contourVoxels) {
                    average = contourBuffer[contourVoxels / 2];
                } else {
                    average = (contourBuffer[contourVoxels / 2] + contourBuffer[(contourVoxels / 2) - 1]) / 2.0;
                }
            } // averageMode == MEDIAN
        } // else black and white

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   presentType  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private int promoteType(int presentType) {

        switch (presentType) {

            case ModelStorageBase.BOOLEAN:
                return ModelStorageBase.BYTE;

            case ModelStorageBase.BYTE:
                return ModelStorageBase.UBYTE;

            case ModelStorageBase.UBYTE:
                return ModelStorageBase.SHORT;

            case ModelStorageBase.SHORT:
                return ModelStorageBase.USHORT;

            case ModelStorageBase.USHORT:
                return ModelStorageBase.INTEGER;

            case ModelStorageBase.INTEGER:
                return ModelStorageBase.UINTEGER;

            case ModelStorageBase.UINTEGER:
                return ModelStorageBase.LONG;

            case ModelStorageBase.LONG:
                return ModelStorageBase.FLOAT;

            case ModelStorageBase.FLOAT:
                return ModelStorageBase.DOUBLE;

            case ModelStorageBase.DOUBLE:
                return ModelStorageBase.DOUBLE;

            default:
                return ModelStorageBase.DOUBLE;
        }
    }

    /**
     * Sets clipMin and clipMax.
     */
    private void setClipValues() {

        if (srcImage.getType() == ModelStorageBase.BOOLEAN) {
            clipMin = 0;
            clipMax = 1;
        } else if (srcImage.getType() == ModelStorageBase.BYTE) {
            clipMin = -128;
            clipMax = 127;
        } else if (srcImage.getType() == ModelStorageBase.UBYTE) {
            clipMin = 0;
            clipMax = 255;
        } else if (srcImage.getType() == ModelStorageBase.SHORT) {
            clipMin = -32768;
            clipMax = 32767;
        } else if (srcImage.getType() == ModelStorageBase.USHORT) {
            clipMin = 0;
            clipMax = 65535;
        } else if (srcImage.getType() == ModelStorageBase.INTEGER) {
            clipMin = Integer.MIN_VALUE;
            clipMax = Integer.MAX_VALUE;
        } else if (srcImage.getType() == ModelStorageBase.UINTEGER) {
            clipMin = 0;
            clipMax = 4294967295L;
        } else if (srcImage.getType() == ModelStorageBase.LONG) {
            clipMin = Long.MIN_VALUE;
            clipMax = Long.MAX_VALUE;
        } else if (srcImage.getType() == ModelStorageBase.FLOAT) {
            clipMin = -Float.MAX_VALUE;
            clipMax = Float.MAX_VALUE;
        } else if (srcImage.getType() == ModelStorageBase.DOUBLE) {
            clipMin = -Double.MAX_VALUE;
            clipMax = Double.MAX_VALUE;
        } else if (srcImage.getType() == ModelStorageBase.ARGB) {
            clipMinC = 0;
            clipMaxC = 255;
        } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
            clipMinC = 0;
            clipMaxC = 65535;
        } else if (srcImage.getType() == ModelStorageBase.ARGB_FLOAT) {
            clipMinC = -Float.MAX_VALUE;
            clipMaxC = Float.MAX_VALUE;
        }
    }


    /**
     * Determine if the min and max values are in the image types range.
     *
     * @param   type    image type
     * @param   minVal  min value of the image
     * @param   maxVal  max value of the image
     *
     * @return  true if min and max are within the image type specified
     */
    private boolean testType(int type, double minVal, double maxVal) {

        if (type == ModelStorageBase.BOOLEAN) {

            if ((minVal < 0) || (maxVal > 1)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.BYTE) {

            if ((minVal < -128) || (maxVal > 127)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.UBYTE) {

            if ((minVal < 0) || (maxVal > 255)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.SHORT) {

            if ((minVal < -32768) || (maxVal > 32767)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.USHORT) {

            if ((minVal < 0) || (maxVal > 65535)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.INTEGER) {

            if ((minVal < Integer.MIN_VALUE) || (maxVal > Integer.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.UINTEGER) {

            if ((minVal < 0) || (maxVal > 4294967295L)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.LONG) {

            if ((minVal < Long.MIN_VALUE) || (maxVal > Long.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.FLOAT) {

            if ((minVal < -Float.MAX_VALUE) || (maxVal > Float.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.DOUBLE) {

            if ((minVal < -Double.MAX_VALUE) || (maxVal > Double.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else {
            return false;
        }
    }
}
