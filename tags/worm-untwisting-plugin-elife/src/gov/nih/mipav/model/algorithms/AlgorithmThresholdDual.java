package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.model.file.FileInfoBase.Unit;

import java.io.*;


/**
 * This algorithm has two modes of operation:
 *
 * <p>1. Generates binary image based on threshold values passed in by the constructor. Inside the volume of interest:
 * If the image intensity is greater than or equal to the threshold[0] and less than or equal to the threshold[1], the
 * binary image is set to one. Otherwise, it is set to zero. Outside the volume of interest: The binary image is set
 * equal to zero.</p>
 *
 * <p>2. Inside the volume of interest: Preserves all grey scale values from threshold[0] to threshold[1] and sets all
 * values less than the lower threshold and all values greater than the upper threshold to the fillValue supplied to
 * this algorithm via the constructor. Outside the volume of interest: Sets all gray scale values to the fill value.</p>
 *
 * @version  1.0 March 8, 2000
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class AlgorithmThresholdDual extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public static final int ORIGINAL_TYPE = 0;

    /** DOCUMENT ME! */
    public static final int BINARY_TYPE = 1;
    
    /** DOCUMENT ME! */
    public static final int UNSIGNED_BYTE_TYPE = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Flag, if true, indicates that the whole image should be processed. If false on process the image over the mask
     * areas.
     */
    private boolean entireImage;

    /** All values outside the thresholds are set to this value. */
    private float fillValue;

    /**
     * Inverse threshold: false means turn all pixels outside of the lower and upper thresholds to the given fill value,
     * true means turn all data within the lower and upper thresholds to the fill value (fill value can also be binary
     * (0)).
     */
    private boolean isInverse = false;

    /** Type of output (same type, binary, or unsigned byte). */
    private int outputType;

    /** Array of two thresholds. threshold[0] = Minimum threshold, threshold[1] = Maximum threshold. */
    private float[] threshold;
    
    /** Variable that will store the total number of pixels in thresholding range */
    private int pixelsInRange = 0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmThresholdDual object.
     *
     * @param  srcImg       source image model
     * @param  threshold    array of two thresholds
     * @param  fillValue    all values outside the thresholds are set to this value
     * @param  output_type  same type, binary, or unsigned byte
     * @param  maskFlag     true indicates that the whole image should be processed
     * @param  isInverse    false means turn all pixels outside of the lower and upper thresholds to the given fill
     *                      value, true means turn all data within the lower and upper thresholds to the fill value
     */
    public AlgorithmThresholdDual(ModelImage srcImg, float[] threshold, float fillValue, int output_type,
                                  boolean maskFlag, boolean isInverse) {

        super(null, srcImg);

        this.threshold = threshold;
        this.fillValue = fillValue;
        this.outputType = output_type;
        entireImage = maskFlag;
        this.isInverse = isInverse;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }
    }

    /**
     * Creates a new AlgorithmThresholdDual object.
     *
     * @param  destImg      image model where result image is to be stored
     * @param  srcImg       source image model
     * @param  threshold    array of two thresholds
     * @param  fillValue    all values outside the thresholds are set to this value
     * @param  output_type  same type, binary, or unsigned byte
     * @param  maskFlag     true indicates that the whole image should be processed
     * @param  isInverse    false means turn all pixels outside of the lower and upper thresholds to the given fill
     *                      value, true means turn all data within the lower and upper thresholds to the fill value
     */
    public AlgorithmThresholdDual(ModelImage destImg, ModelImage srcImg, float[] threshold, float fillValue,
                                  int output_type, boolean maskFlag, boolean isInverse) {

        super(destImg, srcImg);

        this.threshold = threshold;
        this.fillValue = fillValue;
        this.outputType = output_type;
        entireImage = maskFlag;
        this.isInverse = isInverse;

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        threshold = null;
        destImage = null;
        srcImage = null;
        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");
            setCompleted(false);

            return;
        }

        

        if (destImage != null) {

            if ((destImage.getType() != ModelImage.BOOLEAN) && (outputType == BINARY_TYPE)) {
                destImage.reallocate(ModelStorageBase.BOOLEAN);
            } else if ((destImage.getType() != ModelImage.UBYTE) && (outputType == UNSIGNED_BYTE_TYPE)) {
                destImage.reallocate(ModelStorageBase.UBYTE);
            }

            if (srcImage.getNDims() == 2) {
                calcStoreInDest2D();
            } else if (srcImage.getNDims() > 2) {
                calcStoreInDest34D();
            }
        } else {

            if (srcImage.getNDims() == 2) {
                calcInPlace2D();
            } else if (srcImage.getNDims() > 2) {
                calcInPlace34D();
            }
        }
    }

    /**
     * Replace 2D source image with the thresholded image.
     */
    private void calcInPlace2D() {

        int i;
        int length;
        float[] buffer;

        try {
            length = srcImage.getSliceSize();
            buffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            fireProgressStateChanged(srcImage.getImageName(), "Thresholding image ...");
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm Threshold: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Threshold: Out of Memory", true);

            return;
        }

        int mod = length / 100; // mod is 1 percent of length


        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
            }

            if (!isInverse) {

                if (((entireImage == true) || mask.get(i)) &&
                        ((buffer[i] >= threshold[0]) && (buffer[i] <= threshold[1]))) {
                    pixelsInRange++;

                    if ((outputType == BINARY_TYPE) || (outputType == UNSIGNED_BYTE_TYPE)) {
                        buffer[i] = 1;
                    }
                } else {

                    if ((outputType == BINARY_TYPE) || (outputType == UNSIGNED_BYTE_TYPE)) {
                        buffer[i] = 0;
                    } else {
                        buffer[i] = fillValue;
                    }
                }
            } else {

                if (((entireImage == true) || mask.get(i)) &&
                        ((buffer[i] < threshold[0]) || (buffer[i] > threshold[1]))) {
                    pixelsInRange++;

                    if ((outputType == BINARY_TYPE) || (outputType == UNSIGNED_BYTE_TYPE)) {
                        buffer[i] = 1;
                    }
                } else {

                    if ((outputType == BINARY_TYPE) || (outputType == UNSIGNED_BYTE_TYPE)) {
                        buffer[i] = 0;
                    } else {
                        buffer[i] = fillValue;
                    }
                }

            }
        }

        if (threadStopped) {
            finalize();

            return;
        }

        try {

            if ((outputType == BINARY_TYPE) && (srcImage.getType() != ModelImage.BOOLEAN)) {
                srcImage.reallocate(ModelImage.BOOLEAN);
            } else if ((outputType == UNSIGNED_BYTE_TYPE) && (srcImage.getType() != ModelImage.UBYTE)) {
                srcImage.reallocate(ModelImage.UBYTE);
            }

            srcImage.importData(0, buffer, true);
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm Threshold: Image(s) locked", true);

            return;
        }
        
        setThresholdStatistics();

        setCompleted(true);
    }

    /**
     * Replace 3D or 4D source image with the thresholded image.
     */
    private void calcInPlace34D() {

        int i;
        int length;
        float[] buffer;

        try {
            length = srcImage.getSliceSize() * srcImage.getExtents()[2];

            if (srcImage.getNDims() == 4) {
                length = length * srcImage.getExtents()[3];
            }

            buffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            fireProgressStateChanged(srcImage.getImageName(), "Thresholding image ...");
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm Threshold: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Threshold: Out of Memory", true);

            return;
        }

        int mod = length / 100; // mod is 1 percent of length


        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
            }

            if (!isInverse) {

                if (((entireImage == true) || mask.get(i)) &&
                        ((buffer[i] >= threshold[0]) && (buffer[i] <= threshold[1]))) {
                    pixelsInRange++;
                    if ((outputType == BINARY_TYPE) || (outputType == UNSIGNED_BYTE_TYPE)) {
                        buffer[i] = 1;
                    }
                } else {

                    if ((outputType == BINARY_TYPE) || (outputType == UNSIGNED_BYTE_TYPE)) {
                        buffer[i] = 0;
                    } else {
                        buffer[i] = fillValue;
                    }
                }
            } else {

                if (((entireImage == true) || mask.get(i)) &&
                        ((buffer[i] < threshold[0]) || (buffer[i] > threshold[1]))) {
                    pixelsInRange++;
                    if ((outputType == BINARY_TYPE) || (outputType == UNSIGNED_BYTE_TYPE)) {
                        buffer[i] = 1;
                    }
                } else {

                    if ((outputType == BINARY_TYPE) || (outputType == UNSIGNED_BYTE_TYPE)) {
                        buffer[i] = 0;
                    } else {
                        buffer[i] = fillValue;
                    }
                }

            }
        }

        if (threadStopped) {
            finalize();

            return;
        }

        try {

            if ((outputType == BINARY_TYPE) && (srcImage.getType() != ModelImage.BOOLEAN)) {
                srcImage.reallocate(ModelImage.BOOLEAN);
            } else if ((outputType == UNSIGNED_BYTE_TYPE) && (srcImage.getType() != ModelImage.UBYTE)) {
                srcImage.reallocate(ModelImage.UBYTE);
            }

            srcImage.importData(0, buffer, true);
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm Threshold: Image(s) locked", true);

            return;
        }

        setThresholdStatistics();
        setCompleted(true);

    }

    /**
     * Stores the thresholded image into the destination image.
     */
    private void calcStoreInDest2D() {

        int i;
        int length;
        float[] buffer;

        try {
            destImage.setLock();
        } catch (IOException error) {
            displayError("Algorithm Threshold: Image(s) locked");
            setCompleted(false);

            return;
        }

        try {
            length = srcImage.getSliceSize();
            buffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            fireProgressStateChanged(srcImage.getImageName(), "Thresholding image ...");
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm Threshold: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Threshold: Out of Memory", true);

            return;
        }

        int mod = length / 100; // mod is 1 percent of length


        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
            }

            if (!isInverse) {

                if (((entireImage == true) || mask.get(i)) &&
                        ((buffer[i] >= threshold[0]) && (buffer[i] <= threshold[1]))) {
                    pixelsInRange++;
                    if ((outputType == BINARY_TYPE) || (outputType == UNSIGNED_BYTE_TYPE)) {
                        destImage.set(i, 1);
                    } else {
                        destImage.set(i, buffer[i]);
                    }
                } else {

                    if ((outputType == BINARY_TYPE) || (outputType == UNSIGNED_BYTE_TYPE)) {
                        destImage.set(i, 0);
                    } else {
                        destImage.set(i, fillValue);
                    }
                }
            } else {

                if (((entireImage == true) || mask.get(i)) &&
                        ((buffer[i] < threshold[0]) || (buffer[i] > threshold[1]))) {
                    pixelsInRange++;
                    if ((outputType == BINARY_TYPE) || (outputType == UNSIGNED_BYTE_TYPE)) {
                        destImage.set(i, 1);
                    } else {
                        destImage.set(i, buffer[i]);
                    }
                } else {

                    if (outputType == BINARY_TYPE) {
                        destImage.set(i, 0);
                    } else {
                        destImage.set(i, fillValue);
                    }
                }

            }
        }

        if (threadStopped) {
            finalize();

            return;
        }

        destImage.calcMinMax();
        destImage.releaseLock();
        setThresholdStatistics();

        setCompleted(true);
    }

    /**
     * Stores the thresholded image into the destination image.
     */
    private void calcStoreInDest34D() {

        int i;
        int length;
        float[] buffer;

        try {
            destImage.setLock();
        } catch (IOException error) {
            displayError("Algorithm Threshold: Image(s) locked");
            setCompleted(false);

            return;
        }

        try {
            length = srcImage.getSliceSize() * srcImage.getExtents()[2];

            if (srcImage.getNDims() == 4) {
                length = length * srcImage.getExtents()[3];
            }

            buffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            fireProgressStateChanged(srcImage.getImageName(), "Thresholding image ...");
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm Threshold: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Threshold: Out of Memory", true);

            return;
        }

        int mod = length / 100; // mod is 1 percent of length


        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged(Math.round((float) i / (length - 1) * 100));
            }

            if (!isInverse) {

                if (((entireImage == true) || mask.get(i)) &&
                        ((buffer[i] >= threshold[0]) && (buffer[i] <= threshold[1]))) {
                    pixelsInRange++;
                    if ((outputType == BINARY_TYPE) || (outputType == UNSIGNED_BYTE_TYPE)) {
                        destImage.set(i, 1);
                    } else {
                        destImage.set(i, buffer[i]);
                    }
                } else {

                    if ((outputType == BINARY_TYPE) || (outputType == UNSIGNED_BYTE_TYPE)) {
                        destImage.set(i, 0);
                    } else {
                        destImage.set(i, fillValue);
                    }
                }
            } else {

                if (((entireImage == true) || mask.get(i)) &&
                        ((buffer[i] < threshold[0]) || (buffer[i] > threshold[1]))) {
                    pixelsInRange++;
                    if ((outputType == BINARY_TYPE) || (outputType == UNSIGNED_BYTE_TYPE)) {
                        destImage.set(i, 1);
                    } else {
                        destImage.set(i, buffer[i]);
                    }
                } else {

                    if ((outputType == BINARY_TYPE) || (outputType == UNSIGNED_BYTE_TYPE)) {
                        destImage.set(i, 0);
                    } else {
                        destImage.set(i, fillValue);
                    }
                }

            }
        }

        if (threadStopped) {
            finalize();

            return;
        }

        destImage.calcMinMax();
        destImage.releaseLock();
        setThresholdStatistics();

        setCompleted(true);
    }
    
    private void setThresholdStatistics() {
        float xRes, yRes, zRes;
        float area;
        float volume = 0.0f;
        int xUnits, yUnits;
        int zUnits = Unit.UNKNOWN_MEASURE.getLegacyNum();
        ViewUserInterface UI = ViewUserInterface.getReference();
        String units = "    ";
        xRes = srcImage.getFileInfo(0).getResolutions()[0];
        yRes = srcImage.getFileInfo(0).getResolutions()[1];
        area = pixelsInRange * xRes * yRes;
        xUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[0];
        yUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[1];
        if (srcImage.getNDims() > 2) {
            zRes = srcImage.getFileInfo(0).getResolutions()[2];
            volume = area * zRes;
            zUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[2];
        }
        UI.setDataText("            Image name:            " + srcImage.getImageName() + "\n");
        UI.setDataText("            Inverse Threshold:     " + isInverse + "\n");
        UI.setDataText("            Thresholded range:     " + threshold[0] + " - " + threshold[1] + "\n");
        UI.setDataText("            Number of pixels:     " + pixelsInRange + "\n");
        if (srcImage.getNDims() == 2) {
            if (xUnits == yUnits) {
                switch (Unit.getUnitFromLegacyNum(xUnits)) {
                    case INCHES:
                        units = "  in^2";
                        break;
                    case MILS:
                        units = "  mil^2";
                        break;
                    case CENTIMETERS:
                        units = "  cm^2";
                        break;
                    case ANGSTROMS:
                        units = "  A^2";
                        break;
                    case NANOMETERS:
                        units = "  nm^2";
                        break;
                    case MICROMETERS:
                        units = "  um^2";
                        break;
                    case MILLIMETERS:
                        units = "  mm^2";
                        break;
                    case METERS:
                        units = "  m^2";
                        break;
                } // switch (xUnits)
            } // if (xUnits == yUnits)
            UI.setDataText("            Area:     " + area + units + "\n");
        }
        else if (srcImage.getNDims() == 3) {
            if ((xUnits == yUnits) && (xUnits == zUnits)){
                switch (Unit.getUnitFromLegacyNum(xUnits)) {
                    case INCHES:
                        units = "  in^3";
                        break;
                    case MILS:
                        units = "  mil^3";
                        break;
                    case CENTIMETERS:
                        units = "  cm^3";
                        break;
                    case ANGSTROMS:
                        units = "  A^3";
                        break;
                    case NANOMETERS:
                        units = "  nm^3";
                        break;
                    case MICROMETERS:
                        units = "  um^3";
                        break;
                    case MILLIMETERS:
                        units = "  mm^3";
                        break;
                    case METERS:
                        units = "  m^3";
                        break;
                } // switch (xUnits)
            } // if ((xUnits == yUnits) && (xUnits == zUnits))
            UI.setDataText("            Volume:     " + volume + units + "\n");    
        }
    }
}
