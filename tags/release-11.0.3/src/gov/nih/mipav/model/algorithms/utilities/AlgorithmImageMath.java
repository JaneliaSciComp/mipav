package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * Algorithm that adds, subtracts, multiplies, or divides an image by some user specified value. In addition, the square
 * root or log of an image can be calculated. If the new image exceeds the range that can be stored in an image of that
 * type the data is either clipped and stored in the original image. Or a new image of a type (int, float...) that can
 * store the range of new data is generated.
 *
 * @version  1.0 Dec 30, 1999
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class AlgorithmImageMath extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    public enum Operator {
        ABSOLUTE_VALUE(0, "Absolute value", "abs"),
        ADD(1, "Add", "add"),
        AVERAGE(2, "Average", "avg"),
        CONSTANT(3, "Constant", "const"),
        DIVIDE(4, "Divide", "div"),
        LOG(5, "Log", "log"),
        MULTIPLY(6, "Multiply", "mult"),
        SQUARE(7, "Square", "sq"),
        SQUARE_ROOT(8, "Square root", "sqrt"),
        SUBTRACT(9, "Subtract", "sub"),
        SUM(10, "Sum", "sum"),
        INVERSE(11, "Inverse", "inv");
        
        private int legacyNum;
        private String name;
        private String shortOp;

        Operator(int legacyNum, String name, String shortOp) {
            this.legacyNum = legacyNum;
            this.name = name;
            this.shortOp = shortOp;
        }
        
        public String toString() {
            return name;
        }

        public int getLegacyNum() {
            return legacyNum;
        }

        public String getShortOp() {
            return shortOp;
        }
        
        public static Operator getOperatorFromLegacyNum(int legacyNum) {
            for(Operator op : Operator.values()) {
                if(op.getLegacyNum() == legacyNum) {
                    return op;
                }
            }
            return null;
        }
    }

    /** DOCUMENT ME! */
    public static final int ABSOLUTE_VALUE = 0;

    /** DOCUMENT ME! */
    public static final int ADD = 1; // algorithm functions

    /** DOCUMENT ME! */
    public static final int AVERAGE = 2;

    /** DOCUMENT ME! */
    public static final int CONSTANT = 3;

    /** DOCUMENT ME! */
    public static final int DIVIDE = 4;

    /** DOCUMENT ME! */
    public static final int LOG = 5;

    /** DOCUMENT ME! */
    public static final int MULTIPLY = 6;

    /** DOCUMENT ME! */
    public static final int SQUARE = 7;

    /** DOCUMENT ME! */
    public static final int SQUARE_ROOT = 8;

    /** DOCUMENT ME! */
    public static final int SUBTRACT = 9;

    /** DOCUMENT ME! */
    public static final int SUM = 10;
    
    /** DOCUMENT ME! */
    public static final int INVERSE = 11; 
    
    /** DOCUMENT ME! */
    public static final int CLIP = 0; // clamp result data to the bounds of the input image type

    /** DOCUMENT ME! */
    public static final int PROMOTE = 1; // promote image type so that the range of the result fits into
    									// the new image type. ( ie. byte to short).
    
    /** DOCUMENT ME! */
    public static final int CONVERT_FLOAT = 2; // Convert the result image to type float.

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Maximum clipping modes if data result value exceeds the capacity of image's data type. */
    private double clipMax;

    /** Minimum clipping modes if data result value exceeds the capacity of image's data type. */
    private double clipMin;

    /**
     * Clipping mode.
     *
     * <pre>
                          CLIP          = 0;   clamp result data to the bounds of the input image type
                          PROMOTE       = 1;   promote image type so that the range of the result fits into
                                               the new image type. ( ie. byte to short).
                          FLOAT         = 2;   Convert image to float                     
     *                   </pre>
     */
    private int clipMode = PROMOTE;

    /**
     * Flag, if true, indicates that the whole image should be processed. If false, process the image over the mask
     * areas.
     */
    private boolean entireImage;

    /** Used to store the image maximum. */
    private double max;

    /** Used to store the image minimum. */
    private double min;
    
    private double minR;
    private double maxR;
    private double minG;
    private double maxG;
    private double minB;
    private double maxB;

    /** Operation to be performed on the images (i.e. Add, ...) */
    private Operator opType;

    /** DOCUMENT ME! */
    private boolean useComplex = false;

    /** the value to modify the image with. */
    private double value;

    /** imaginary part of value to modify complex or dcomplex image with or green color of color image. */
    private double valueI;
    
    private double valueB;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmImageMath object.
     *
     * @param  srcImg     source image model
     * @param  type       operation type to be performed on the image
     * @param  val        value applied to the image
     * @param  valI       imaginary part of value applied if image is of complex type or 
     *                    green part of color if color image
     * @param  valB       blue part of color image
     * @param  _clipMode  clamp data to image type range or promote image type to hold new data range.
     * @param  maskFlag   Flag that indicates that the operator will be calculated for the whole image if equal to true
     */
    public AlgorithmImageMath(ModelImage srcImg, int type, double val, double valI, 
                              double valB, int _clipMode, boolean maskFlag) {
        this(null, srcImg, type, val, valI, valB, _clipMode, maskFlag);
    }

    /**
     * Creates a new AlgorithmImageMath object.
     *
     * @param  destImg    image model where result image is to stored
     * @param  srcImg     source image model
     * @param  type       operation type to be performed on the image
     * @param  val        value applied to the image
     * @param  valI       imaginary part of value applied if image is of complex type or
     *                    green part of color if color image
     * @param  valB       blue part of color image
     * @param  _clipMode  clamp data to image type range or promote image type to hold new data range.
     * @param  maskFlag   Flag that indicates that the operator will be calculated for the whole image if equal to true
     */
    public AlgorithmImageMath(ModelImage destImg, ModelImage srcImg, int type, double val, double valI, 
                              double valB, int _clipMode, boolean maskFlag) {
        super(destImg, srcImg);
        entireImage = maskFlag;
        opType = Operator.getOperatorFromLegacyNum(type);
        value = val;
        valueI = valI;
        valueB = valB;
        clipMode = _clipMode;
        setClipValues();

        if (entireImage == false) {
            mask = srcImage.generateVOIMask();
        }

        if ((srcImage.getType() == ModelStorageBase.COMPLEX) || (srcImage.getType() == ModelStorageBase.DCOMPLEX)) {
            useComplex = true;
        }
        // System.err.println("op type is: " + opType);
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
            displayError("ImageMath.run(): Source Image is null");

            return;
        }

        

        if (destFlag == true) { // use a destination image.

            if (useComplex) {
                calcStoreInDestComplex();
            } else if (srcImage.isColorImage()) {
                calcStoreInDestColor();
            } else {
                calcStoreInDest();
            }
        } else {

            if (useComplex) {
                calcInPlaceComplex();
            } else if (srcImage.isColorImage()) {
                calcInPlaceColor();
            } else {
                calcInPlace();
            }
        }
    }
    
    private void calcInPlaceColor() {
        int i, j, k, m;
        int z, t, f;
        int offset;
        int length; // total number of data-elements (pixels) in image
        double[] buffer; // data-buffer (for pixel data) which is the "heart" of the image
        double bestMin, bestMax;
        double bestMinR, bestMaxR, bestMinG, bestMaxG, bestMinB, bestMaxB;

        try {
            length = srcImage.getSliceSize();
            buffer = new double[4*length];
            fireProgressStateChanged(srcImage.getImageName(), "Calculating image ...");
        } catch (OutOfMemoryError e) {
            buffer = null;
            System.gc();
            displayError("Algorithm ImageMath reports: Out of memory when creating image buffer");
            setCompleted(false);

            return;
        }

        minR = srcImage.getMinR();
        maxR = srcImage.getMaxR();
        minG = srcImage.getMinG();
        maxG = srcImage.getMaxG();
        minB = srcImage.getMinB();
        maxB = srcImage.getMaxB();
        min = Math.min(minR, Math.min(minG, minB));
        max = Math.max(maxR, Math.max(maxG, maxB));

        if (opType == Operator.LOG) {

            if (min <= 0) {
                displayError("Algorithm ImageMath: Cannot do log on image data <= 0");
                setCompleted(false);

                return;
            } else if (srcImage.getType() != ModelStorageBase.ARGB_FLOAT) {
                AlgorithmChangeType changeTypeAlgo = new AlgorithmChangeType(srcImage, ModelStorageBase.ARGB_FLOAT,
                                                     min, max, min, max, false);
                changeTypeAlgo.setRunningInSeparateThread(runningInSeparateThread);
                changeTypeAlgo.run();

                // if the change algo was halted,
                if (!changeTypeAlgo.isCompleted()) {

                    // halt the rest of this processing.
                    setThreadStopped(true);
                }
            }
        } else if (opType == Operator.SQUARE_ROOT) {

            if (min < 0) {
                displayError("Algorithm ImageMath: Cannot do SQRT on image data < 0");
                setCompleted(false);

                return;
            } else if (srcImage.getType() != ModelStorageBase.ARGB_FLOAT) {
                AlgorithmChangeType changeTypeAlgo = new AlgorithmChangeType(srcImage, ModelStorageBase.ARGB_FLOAT,
                                                     min, max, min, max, false);
                changeTypeAlgo.setRunningInSeparateThread(runningInSeparateThread);
                changeTypeAlgo.run();

                // if the change algo is halted,
                if (!changeTypeAlgo.isCompleted()) {

                    // halt the rest of this processing.
                    setThreadStopped(true);
                }
            }
        } else if ((opType == Operator.DIVIDE) && ((value == 0.0) || (valueI == 0.0) || (valueB == 0.0))) {
            displayError("Algorithm ImageMath: Cannot divide image data by zero");
            setCompleted(false);

            return;
        } else if (clipMode == CONVERT_FLOAT) {
            
            int newType = ModelStorageBase.ARGB_FLOAT;
            bestMinR = minR;
            bestMaxR = maxR;
            bestMinG = minG;
            bestMaxG = maxG;
            bestMinB = minB;
            bestMaxB = maxB;
            bestMin = Math.min(bestMinR, Math.min(bestMinG, bestMinB));
            bestMax = Math.max(bestMaxR, Math.max(bestMaxG, bestMaxB));
            
            switch (opType) {
            
                case ADD:
                    bestMinR = minR + value;
                    bestMaxR = maxR + value;
                    bestMinG = minG + valueI;
                    bestMaxG = maxG + valueI;
                    bestMinB = minB + valueB;
                    bestMaxB = maxB + valueB;
                    bestMin = Math.min(bestMinR, Math.min(bestMinG, bestMinB));
                    bestMax = Math.max(bestMaxR, Math.max(bestMaxG, bestMaxB));
                    break;
                    
                case SUBTRACT:
                    bestMinR = minR - value;
                    bestMaxR = maxR - value;
                    bestMinG = minG - valueI;
                    bestMaxG = maxG - valueI;
                    bestMinB = minB - valueB;
                    bestMaxB = maxB - valueB;
                    bestMin = Math.min(bestMinR, Math.min(bestMinG, bestMinB));
                    bestMax = Math.max(bestMaxR, Math.max(bestMaxG, bestMaxB));
                    break;
                    
                case MULTIPLY:
                    bestMinR = java.lang.Math.min(minR * value, maxR * value);
                    bestMaxR = java.lang.Math.max(minR * value, maxR * value);
                    bestMinG = java.lang.Math.min(minG * valueI, maxG * valueI);
                    bestMaxG = java.lang.Math.max(minG * valueI, maxG * valueI);
                    bestMinB = java.lang.Math.min(minB * valueB, maxB * valueB);
                    bestMaxB = java.lang.Math.max(minB * valueB, maxB * valueB);
                    bestMin = Math.min(bestMinR, Math.min(bestMinG, bestMinB));
                    bestMax = Math.max(bestMaxR, Math.max(bestMaxG, bestMaxB));
                    break;
                    
                case DIVIDE:
                    bestMinR = java.lang.Math.min(minR / value, maxR / value);
                    bestMaxR = java.lang.Math.max(minR / value, maxR / value);
                    bestMinG = java.lang.Math.min(minG / valueI, maxG / valueI);
                    bestMaxG = java.lang.Math.max(minG / valueI, maxG / valueI);
                    bestMinB = java.lang.Math.min(minB / valueB, maxB / valueB);
                    bestMaxB = java.lang.Math.max(minB / valueB, maxB / valueB);
                    bestMin = Math.min(bestMinR, Math.min(bestMinG, bestMinB));
                    bestMax = Math.max(bestMaxR, Math.max(bestMaxG, bestMaxB));
                    break;
                    
                case SQUARE:
                    bestMinR = minR * minR;
                    bestMaxR = maxR * maxR;
                    bestMinG = minG * minG;
                    bestMaxG = maxG * maxG;
                    bestMinB = minB * minB;
                    bestMaxB = maxB * maxB;
                    bestMin = Math.min(bestMinR, Math.min(bestMinG, bestMinB));
                    bestMax = Math.max(bestMaxR, Math.max(bestMaxG, bestMaxB));
                    break;
                    
                case CONSTANT:
                    if (value < minR) {
                        bestMinR = value;
                        bestMaxR = maxR;
                    } else if (value > maxR) {
                        bestMinR = minR;
                        bestMaxR = maxR;
                    } else {
                        bestMinR = minR;
                        bestMaxR = maxR;
                    }
                    
                    if (valueI < minG) {
                        bestMinG = valueI;
                        bestMaxG = maxG;
                    } else if (valueI > maxG) {
                        bestMinG = minG;
                        bestMaxG = maxG;
                    } else {
                        bestMinG = minG;
                        bestMaxG = maxG;
                    }
                    
                    if (valueB < minB) {
                        bestMinB = valueB;
                        bestMaxB = maxB;
                    } else if (valueB > maxB) {
                        bestMinB = minB;
                        bestMaxB = maxB;
                    } else {
                        bestMinB = minB;
                        bestMaxB = maxB;
                    }
                    
                    bestMin = Math.min(bestMinR, Math.min(bestMinG, bestMinB));
                    bestMax = Math.max(bestMaxR, Math.max(bestMaxG, bestMaxB));
                    break;
                    
            }
            
            AlgorithmChangeType changeTypeAlgo = new AlgorithmChangeType(srcImage, newType, min, max, min, max,
                                                                        false);
            changeTypeAlgo.setRunningInSeparateThread(runningInSeparateThread);
            changeTypeAlgo.run();

            // if the change algo is halted,
            if (!changeTypeAlgo.isCompleted()) {

            // halt the rest of this processing.
            setThreadStopped(true);
            }
            
        } else {
            int newType = srcImage.getType();

            if (clipMode == PROMOTE) {

                switch (opType) {

                    case ADD:
                        if (((maxR + value) > clipMax) || ((minR + value) < clipMin) || 
                            ((maxG + valueI) > clipMax) || ((minG + valueI) < clipMin) ||
                            ((maxB + valueB) > clipMax) || ((minB + valueB) < clipMin)) {
                            newType = findType(srcImage.getType());
                        }

                        break;

                    case SUBTRACT:
                        if (((minR - value) < clipMin) || ((maxR - value) > clipMax) ||
                            ((minG - valueI) < clipMin) || ((maxG - valueI) > clipMax)||
                            ((minB - valueB) < clipMin) || ((maxB - valueB) > clipMax)) {
                            newType = findType(srcImage.getType());
                        }

                        break;

                    case MULTIPLY:
                        bestMinR = java.lang.Math.min(minR * value, maxR * value);
                        bestMaxR = java.lang.Math.max(minR * value, maxR * value);
                        bestMinG = java.lang.Math.min(minG * valueI, maxG * valueI);
                        bestMaxG = java.lang.Math.max(minG * valueI, maxG * valueI);
                        bestMinB = java.lang.Math.min(minB * valueB, maxB * valueB);
                        bestMaxB = java.lang.Math.max(minB * valueB, maxB * valueB);
                        bestMin = Math.min(bestMinR, Math.min(bestMinG, bestMinB));
                        bestMax = Math.max(bestMaxR, Math.max(bestMaxG, bestMaxB));
                        if ((bestMax > clipMax) || (bestMin < clipMin)) {
                            newType = findType(srcImage.getType());
                        }

                        break;

                    case DIVIDE:
                        bestMinR = java.lang.Math.min(minR / value, maxR / value);
                        bestMaxR = java.lang.Math.max(minR / value, maxR / value);
                        bestMinG = java.lang.Math.min(minG / valueI, maxG / valueI);
                        bestMaxG = java.lang.Math.max(minG / valueI, maxG / valueI);
                        bestMinB = java.lang.Math.min(minB / valueB, maxB / valueB);
                        bestMaxB = java.lang.Math.max(minB / valueB, maxB / valueB);
                        bestMin = Math.min(bestMinR, Math.min(bestMinG, bestMinB));
                        bestMax = Math.max(bestMaxR, Math.max(bestMaxG, bestMaxB));
                        if ((bestMin < clipMin) || (bestMax > clipMax)) {
                            newType = findType(srcImage.getType());
                        }

                        break;

                    case SQUARE:
                        if (((maxR * maxR) > clipMax) || ((minR * minR) > clipMax) ||
                            ((maxG * maxG) > clipMax) || ((minG * minG) > clipMax) ||
                            ((maxB * maxB) > clipMax) || ((minB * minB) > clipMax)) {
                            newType = findType(srcImage.getType());
                        }

                        break;

                    case CONSTANT:
                        if ((value < clipMin) || (value > clipMax) ||
                            (valueI < clipMin) || (valueI > clipMax) ||
                            (valueB < clipMin) || (valueB > clipMax)) {
                            newType = findType(srcImage.getType());
                        }

                        break;

                    default:
                        break;
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

        int volume = z * length;
        int space4D = t * volume;
        int totalLength = f * t * z * length;

        for (m = 0; (m < f) && !threadStopped; m++) {

            for (k = 0; (k < t) && !threadStopped; k++) {

                for (j = 0; (j < z) && !threadStopped; j++) {

                    try {
                        offset = (m * space4D) + (k * volume) + (j * length);
                        srcImage.exportData(4*offset, 4*length, buffer); // locks and releases lock
                    } catch (IOException error) {
                        displayError("Algorithm ImageMath : Image(s) locked");
                        setCompleted(false);

                        return;
                    }

                    for (i = 0; (i < length) && !threadStopped; i++) {

                        try {

                            if (((i % mod) == 0)) {
                                fireProgressStateChanged(Math.round((float) (i + offset) / (totalLength - 1) * 100));
                            }
                        } catch (NullPointerException npe) {

                            if (threadStopped) {
                                Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                                  Preferences.DEBUG_ALGORITHM);
                            }
                        }

                        // Get slice
                        if ((entireImage == true) || mask.get(i + offset)) {

                            switch (opType) {

                                case ADD:
                                    buffer[4*i+1] = buffer[4*i+1] + value;
                                    buffer[4*i+2] = buffer[4*i+2] + valueI;
                                    buffer[4*i+3] = buffer[4*i+3] + valueB;
                                    break;

                                case SUBTRACT:
                                    buffer[4*i+1] = buffer[4*i+1] - value;
                                    buffer[4*i+2] = buffer[4*i+2] - valueI;
                                    buffer[4*i+3] = buffer[4*i+3] - valueB;
                                    break;

                                case MULTIPLY:
                                    buffer[4*i+1] = buffer[4*i+1] * value;
                                    buffer[4*i+2] = buffer[4*i+2] * valueI;
                                    buffer[4*i+3] = buffer[4*i+3] * valueB;
                                    break;

                                case DIVIDE:
                                    buffer[4*i+1] = buffer[4*i+1] / value;
                                    buffer[4*i+2] = buffer[4*i+2] / valueI;
                                    buffer[4*i+3] = buffer[4*i+3] / valueB;
                                    break;

                                case SQUARE:
                                    buffer[4*i+1] = buffer[4*i+1] * buffer[4*i+1];
                                    buffer[4*i+2] = buffer[4*i+2] * buffer[4*i+2];
                                    buffer[4*i+3] = buffer[4*i+3] * buffer[4*i+3];
                                    break;

                                case SQUARE_ROOT:
                                    buffer[4*i+1] = Math.sqrt(buffer[4*i+1]);
                                    buffer[4*i+2] = Math.sqrt(buffer[4*i+2]);
                                    buffer[4*i+3] = Math.sqrt(buffer[4*i+3]);
                                    break;

                                case LOG:
                                    buffer[4*i+1] = Math.log(buffer[4*i+1]);
                                    buffer[4*i+2] = Math.log(buffer[4*i+2]);
                                    buffer[4*i+3] = Math.log(buffer[4*i+3]);
                                    break;

                                case CONSTANT:
                                    buffer[4*i+1] = value;
                                    buffer[4*i+2] = valueI;
                                    buffer[4*i+3] = valueB;
                                    break;
                                    
                                case ABSOLUTE_VALUE:
                                    buffer[4*i+1] = Math.abs(buffer[4*i+1]);
                                    buffer[4*i+2] = Math.abs(buffer[4*i+2]);
                                    buffer[4*i+3] = Math.abs(buffer[4*i+3]);
                                    break;

                                default:
                                    break;
                            }
                        }
                    }

                    // clip check
                    if (clipMode == CLIP) {

                        for (i = 0; i < 4*length; i++) {

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

                        srcImage.importData(4*offset, buffer, false);
                    } catch (IOException error) {
                        displayError("Algorithm ImageMath: Image(s) locked");
                        setCompleted(false);
                        

                        return;
                    }
                }
            } // t loop
        } // f loop

        srcImage.calcMinMax();
        
        setCompleted(true);    
    } // private void calcInPlaceColor()

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
        double bestMin, bestMax;

        try {
            length = srcImage.getSliceSize();
            buffer = new double[length];
            fireProgressStateChanged(srcImage.getImageName(), "Calculating image ...");
        } catch (OutOfMemoryError e) {
            buffer = null;
            System.gc();
            displayError("Algorithm ImageMath reports: Out of memory when creating image buffer");
            setCompleted(false);

            return;
        }

        min = srcImage.getMin();
        max = srcImage.getMax();

        if (opType == Operator.LOG) {

            if (min <= 0) {
                displayError("Algorithm ImageMath: Cannot do log on image data <= 0");
                setCompleted(false);

                return;
            } else if ((srcImage.getType() != ModelStorageBase.FLOAT) &&
                           (srcImage.getType() != ModelStorageBase.DOUBLE)) {
                AlgorithmChangeType changeTypeAlgo = new AlgorithmChangeType(srcImage, ModelStorageBase.FLOAT, min, max,
                                                                             min, max, false);
                changeTypeAlgo.setRunningInSeparateThread(runningInSeparateThread);
                changeTypeAlgo.run();

                // if the change algo was halted,
                if (!changeTypeAlgo.isCompleted()) {

                    // halt the rest of this processing.
                    setThreadStopped(true);
                }
            }
        } else if (opType == Operator.SQUARE_ROOT) {

            if (min < 0) {
                displayError("Algorithm ImageMath: Cannot do SQRT on image data < 0");
                setCompleted(false);

                return;
            } else if ((srcImage.getType() != ModelStorageBase.FLOAT) &&
                           (srcImage.getType() != ModelStorageBase.DOUBLE)) {
                AlgorithmChangeType changeTypeAlgo = new AlgorithmChangeType(srcImage, ModelStorageBase.FLOAT, min, max,
                                                                             min, max, false);
                changeTypeAlgo.setRunningInSeparateThread(runningInSeparateThread);
                changeTypeAlgo.run();

                // if the change algo is halted,
                if (!changeTypeAlgo.isCompleted()) {

                    // halt the rest of this processing.
                    setThreadStopped(true);
                }
            }
        } else if ((opType == Operator.DIVIDE) && (value == 0.0)) {
            displayError("Algorithm ImageMath: Cannot divide image data by zero");
            setCompleted(false);

            return;
        } else if (clipMode == CONVERT_FLOAT) {
        	
        	int newType = ModelStorageBase.FLOAT;
        	bestMin = min;
        	bestMax = max;
        	
        	switch (opType) {
        	
        		case ADD:
        			bestMin = min + value;
        			bestMax = max + value;
        			break;
        			
        		case SUBTRACT:
        			bestMin = min - value;
        			bestMax = max - value;
        			break;
        			
        		case MULTIPLY:
        			bestMin = java.lang.Math.min(min * value, max * value);
                    bestMax = java.lang.Math.max(min * value, max * value);
                    break;
                    
        		case DIVIDE:
        			bestMin = java.lang.Math.min(min / value, max / value);
                    bestMax = java.lang.Math.max(min / value, max / value);
                    break;
                    
        		case SQUARE:
        			bestMin = min * min;
        			bestMax = max * max;
        			break;
        			
        		case CONSTANT:
        			if (value < min) {
        				bestMin = value;
        				bestMax = max;
        			} else if (value > max) {
        				bestMin = min;
        				bestMax = max;
        			} else {
        				bestMin = min;
        				bestMax = max;
        			}
        			break;
        			
        	}
        	
        	AlgorithmChangeType changeTypeAlgo = new AlgorithmChangeType(srcImage, newType, min, max, min, max,
                    													false);
        	changeTypeAlgo.setRunningInSeparateThread(runningInSeparateThread);
        	changeTypeAlgo.run();

 			// if the change algo is halted,
        	if (!changeTypeAlgo.isCompleted()) {

 			// halt the rest of this processing.
 			setThreadStopped(true);
        	}
        	
        } else {
            int newType = srcImage.getType();

            if (clipMode == PROMOTE) {

                switch (opType) {

                    case ADD:
                        if (((max + value) > clipMax) || ((min + value) < clipMin)) {
                            newType = findType(srcImage.getType());
                        }

                        break;

                    case SUBTRACT:
                        if (((min - value) < clipMin) || ((max - value) > clipMax)) {
                            newType = findType(srcImage.getType());
                        }

                        break;

                    case MULTIPLY:
                        bestMin = java.lang.Math.min(min * value, max * value);
                        bestMax = java.lang.Math.max(min * value, max * value);
                        if ((bestMax > clipMax) || (bestMin < clipMin)) {
                            newType = findType(srcImage.getType());
                        }

                        break;

                    case DIVIDE:
                        bestMin = java.lang.Math.min(min / value, max / value);
                        bestMax = java.lang.Math.max(min / value, max / value);
                        if ((bestMin < clipMin) || (bestMax > clipMax)) {
                            newType = findType(srcImage.getType());
                        }

                        break;

                    case SQUARE:
                        if (((max * max) > clipMax) || ((min * min) > clipMax)) {
                            newType = findType(srcImage.getType());
                        }

                        break;

                    case CONSTANT:
                        if ((value < clipMin) || (value > clipMax)) {
                            newType = findType(srcImage.getType());
                        }

                        break;

                    default:
                        break;
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

        int volume = z * length;
        int space4D = t * volume;
        int totalLength = f * t * z * length;

        for (m = 0; (m < f) && !threadStopped; m++) {

            for (k = 0; (k < t) && !threadStopped; k++) {

                for (j = 0; (j < z) && !threadStopped; j++) {

                    try {
                        offset = (m * space4D) + (k * volume) + (j * length);
                        srcImage.exportData(offset, length, buffer); // locks and releases lock
                    } catch (IOException error) {
                        displayError("Algorithm ImageMath : Image(s) locked");
                        setCompleted(false);

                        return;
                    }

                    for (i = 0; (i < length) && !threadStopped; i++) {

                        try {

                            if (((i % mod) == 0)) {
                                fireProgressStateChanged(Math.round((float) (i + offset) / (totalLength - 1) * 100));
                            }
                        } catch (NullPointerException npe) {

                            if (threadStopped) {
                                Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                                  Preferences.DEBUG_ALGORITHM);
                            }
                        }

                        // Get slice
                        if ((entireImage == true) || mask.get(i + offset)) {

                            switch (opType) {

                                case ADD:
                                    buffer[i] = buffer[i] + value;
                                    break;

                                case SUBTRACT:
                                    buffer[i] = buffer[i] - value;
                                    break;

                                case MULTIPLY:
                                    buffer[i] = buffer[i] * value;
                                    break;

                                case DIVIDE:
                                    buffer[i] = buffer[i] / value;
                                    break;

                                case SQUARE:
                                    buffer[i] = buffer[i] * buffer[i];
                                    break;

                                case SQUARE_ROOT:
                                    buffer[i] = Math.sqrt(buffer[i]);
                                    break;

                                case LOG:
                                    buffer[i] = Math.log(buffer[i]);
                                    break;

                                case CONSTANT:
                                    buffer[i] = value;
                                    break;
                                    
                                case ABSOLUTE_VALUE:
                                    buffer[i] = Math.abs(buffer[i]);
                                    break;
                                    
                                case INVERSE:
                                	if(buffer[i] == 0) {
                                		buffer[i] = Float.NaN;
                                	}else {
                                		buffer[i] = 1 / buffer[i];
                                	}
                                	break;
                                	 

                                default:
                                    break;
                            }
                        }
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
                        displayError("Algorithm ImageMath: Image(s) locked");
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
    private void calcInPlaceComplex() {

        int i, j, k, m;
        int z, t, f;
        int offset;
        int length; // total number of data-elements (pixels) in image
        double[] buffer; // data-buffer (for pixel data) which is the "heart" of the image
        double[] bufferI;
        boolean needPromote = false;
        double[] totBuffer;
        double[] totBufferI;
        double temp;
        double denom;
        double mag;
        double ang;

        try {
            length = srcImage.getSliceSize();
            buffer = new double[length];
            bufferI = new double[length];
            fireProgressStateChanged(srcImage.getImageName(), "Calculating image ...");
        } catch (OutOfMemoryError e) {
            buffer = null;
            bufferI = null;
            System.gc();
            displayError("Algorithm ImageMath reports: Out of memory when creating image buffer");
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

        int volume = z * length;
        int space4D = t * volume;
        int totalLength = f * t * z * length;

        if ((opType == Operator.DIVIDE) && (value == 0.0) && (valueI == 0.0)) {
            displayError("Algorithm ImageMath: Cannot divide image data by zero");
            setCompleted(false);

            return;
        }

        for (m = 0; (m < f) && !threadStopped; m++) {

            for (k = 0; (k < t) && !threadStopped; k++) {

                for (j = 0; (j < z) && !threadStopped; j++) {

                    try {
                        offset = (m * space4D) + (k * volume) + (j * length);
                        srcImage.exportDComplexData(2 * offset, length, buffer, bufferI); // locks and releases lock
                    } catch (IOException error) {
                        displayError("Algorithm ImageMath : Image(s) locked");
                        setCompleted(false);

                        return;
                    }

                    for (i = 0; (i < length) && !threadStopped; i++) {

                        try {

                            if (((i % mod) == 0)) {
                                fireProgressStateChanged(Math.round((float) (i + offset) / (totalLength - 1) * 100));
                            }
                        } catch (NullPointerException npe) {

                            if (threadStopped) {
                                Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                                  Preferences.DEBUG_ALGORITHM);
                            }
                        }

                        // Get slice
                        if ((entireImage == true) || mask.get(i + offset)) {

                            switch (opType) {

                                case ADD:
                                    buffer[i] = buffer[i] + value;
                                    bufferI[i] = bufferI[i] + valueI;
                                    break;

                                case SUBTRACT:
                                    buffer[i] = buffer[i] - value;
                                    bufferI[i] = bufferI[i] - valueI;
                                    break;

                                case MULTIPLY:
                                    temp = (buffer[i] * value) - (bufferI[i] * valueI);
                                    bufferI[i] = (buffer[i] * valueI) + (bufferI[i] * value);
                                    buffer[i] = temp;
                                    break;

                                case DIVIDE:
                                    denom = (value * value) + (valueI * valueI);
                                    temp = ((buffer[i] * value) + (bufferI[i] * valueI)) / denom;
                                    bufferI[i] = ((value * bufferI[i]) - (valueI * buffer[i])) / denom;
                                    buffer[i] = temp;
                                    break;

                                case SQUARE:
                                    temp = (buffer[i] * buffer[i]) - (bufferI[i] * bufferI[i]);
                                    bufferI[i] = 2.0 * buffer[i] * bufferI[i];
                                    buffer[i] = temp;
                                    break;

                                case SQUARE_ROOT:
                                    mag = Math.sqrt(Math.sqrt((buffer[i] * buffer[i]) + (bufferI[i] * bufferI[i])));
                                    ang = 0.5 * Math.atan2(bufferI[i], buffer[i]);
                                    buffer[i] = Math.cos(ang) * mag;
                                    bufferI[i] = Math.sin(ang) * mag;
                                    break;

                                case LOG:
                                    mag = Math.sqrt((buffer[i] * buffer[i]) + (bufferI[i] * bufferI[i]));
                                    buffer[i] = Math.log(mag);
                                    bufferI[i] = Math.atan2(bufferI[i], buffer[i]);

                                    // The principal value has the 0 <= angle < 2*PI
                                    if (bufferI[i] < 0.0) {
                                        bufferI[i] = bufferI[i] + (2.0 * Math.PI);
                                    }

                                    break;

                                case CONSTANT:
                                    buffer[i] = value;
                                    bufferI[i] = valueI;
                                    break;

                                default:
                                    break;
                            }
                        }
                    }

                    // clip check
                    if (clipMode == CLIP) {

                        // Keep same phase in clipping
                        for (i = 0; i < length; i++) {

                            if ((buffer[i] > Float.MAX_VALUE) || (buffer[i] < -Float.MAX_VALUE) ||
                                    (bufferI[i] > Float.MAX_VALUE) || (bufferI[i] < -Float.MAX_VALUE)) {

                                if (Math.abs(buffer[i]) >= Math.abs(bufferI[i])) {

                                    if (buffer[i] > Float.MAX_VALUE) {
                                        bufferI[i] = bufferI[i] * (Float.MAX_VALUE / buffer[i]);
                                        buffer[i] = Float.MAX_VALUE;
                                    } else {
                                        bufferI[i] = bufferI[i] * (-Float.MAX_VALUE / buffer[i]);
                                        buffer[i] = -Float.MAX_VALUE;
                                    }
                                } else { // Math.abs(buffer[i]) < Math.abs(bufferI[i])

                                    if (bufferI[i] > Float.MAX_VALUE) {
                                        buffer[i] = buffer[i] * (Float.MAX_VALUE / bufferI[i]);
                                        bufferI[i] = Float.MAX_VALUE;
                                    } else {
                                        buffer[i] = buffer[i] * (-Float.MAX_VALUE / bufferI[i]);
                                        bufferI[i] = -Float.MAX_VALUE;
                                    }
                                }
                            }
                        }
                    } // if (clipMode == CLIP)
                    else if ((clipMode == PROMOTE) && (srcImage.getType() == ModelStorageBase.COMPLEX)) {

                        for (i = 0; (i < length) && !needPromote; i++) {

                            if ((buffer[i] > Float.MAX_VALUE) || (buffer[i] < -Float.MAX_VALUE) ||
                                    (bufferI[i] > Float.MAX_VALUE) || (bufferI[i] < -Float.MAX_VALUE)) {
                                needPromote = true;
                            }
                        }
                    }

                    if (needPromote) {
                        needPromote = false;

                        try {
                            totBuffer = new double[totalLength];
                            totBufferI = new double[totalLength];
                        } catch (OutOfMemoryError e) {
                            totBuffer = null;
                            totBufferI = null;
                            System.gc();
                            displayError("Algorithm ImageMath reports: Out of memory when creating total image buffer");
                            setCompleted(false);

                            return;
                        }

                        try {
                            srcImage.exportDComplexData(0, totalLength, totBuffer, totBufferI); // locks and releases
                                                                                                // lock
                        } catch (IOException error) {
                            displayError("Algorithm ImageMath : Image(s) locked");
                            setCompleted(false);

                            return;
                        }

                        srcImage.reallocate(ModelStorageBase.DCOMPLEX);

                        try {

                            // do BEFORE buffer has been exported to Image
                            if (threadStopped) {
                                finalize();

                                return;
                            }

                            srcImage.importDComplexData(0, totBuffer, totBufferI, false, true);
                        } catch (IOException error) {
                            displayError("Algorithm ImageMath: Image(s) locked");
                            setCompleted(false);
                            

                            return;
                        }

                        totBuffer = null;
                        totBufferI = null;
                    } // if (needPromote)

                    try {

                        // do BEFORE buffer has been exported to Image
                        if (threadStopped) {
                            finalize();

                            return;
                        }

                        srcImage.importDComplexData(2 * offset, buffer, bufferI, false, true);
                    } catch (IOException error) {
                        displayError("Algorithm ImageMath: Image(s) locked");
                        setCompleted(false);
                        

                        return;
                    }
                }
            } // t loop
        } // f loop

        srcImage.calcMinMax();
        
        setCompleted(true);
    }
    
    private void calcStoreInDestColor() {
        int i, j, k, m;
        int z, t, f;
        int offset;
        int length; // total number of data-elements (pixels) in image
        double[] buffer; // data-buffer (for pixel data) which is the "heart" of the image
        double bestMin, bestMax;
        double bestMinR, bestMaxR, bestMinG, bestMaxG, bestMinB, bestMaxB;
        int sumAverageLength;

        double[] sumAverageBuffer = null; // buffer needed to do summing/average
        int sumFactor = 1;

        try {
            length = srcImage.getSliceSize();
            buffer = new double[4*length];

            if ((opType == Operator.AVERAGE) || (opType == Operator.SUM)) {
                sumAverageLength = 4*length;
                if (srcImage.getNDims() >= 4) {
                    sumAverageLength *= srcImage.getExtents()[2];
                }
                if (srcImage.getNDims() == 5) {
                    sumAverageLength *= srcImage.getExtents()[3];
                }
                sumAverageBuffer = new double[sumAverageLength];
                if (srcImage.getNDims() == 5) {
                    sumFactor = srcImage.getExtents()[4];
                }
                else if (srcImage.getNDims() == 4) {
                    sumFactor = srcImage.getExtents()[3];
                }
                else if (srcImage.getNDims() == 3) {
                    sumFactor = srcImage.getExtents()[2];
                }
                else {
                    sumFactor = 1;
                }
                
            }

            fireProgressStateChanged(srcImage.getImageName(), "Calculating image ...");
        } catch (OutOfMemoryError e) {
            buffer = null;
            System.gc();
            displayError("Algorithm ImageMath reports: Out of memory when creating image buffer");
            setCompleted(false);

            return;
        }

        minR = srcImage.getMinR();
        maxR = srcImage.getMaxR();
        minG = srcImage.getMinG();
        maxG = srcImage.getMaxG();
        minB = srcImage.getMinB();
        maxB = srcImage.getMaxB();
        min = Math.min(minR, Math.min(minG, minB));
        max = Math.max(maxR, Math.max(maxG, maxB));
        
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

        int volume = z * length;
        int space4D = t * volume;
        int totalLength = f * t * z * length;

        if (opType == Operator.LOG) {

            if (min <= 0) {
                displayError("Cannot do LOG on image data <= 0");
                setCompleted(false);

                return;
            } else if (destImage.getType() != ModelStorageBase.ARGB_FLOAT) {
                destImage.reallocate(ModelStorageBase.ARGB_FLOAT);
            } 
        } else if (opType == Operator.SQUARE_ROOT) {

            if (min < 0) {
                displayError("Cannot do SQRT on image data < 0");
                setCompleted(false);

                return;
            } else if (destImage.getType() != ModelStorageBase.ARGB_FLOAT) {
                destImage.reallocate(ModelStorageBase.ARGB_FLOAT);
            } 
        } else if ((opType == Operator.DIVIDE) && (value == 0.0)) {
            displayError("cannot divide image data by zero");
            setCompleted(false);

            return;
        } else if (clipMode == CONVERT_FLOAT) {
            destImage.reallocate(ModelStorageBase.ARGB_FLOAT);
        } else {
            int newType = srcImage.getType();

            if (clipMode == PROMOTE) {

                switch (opType) {

                    case ADD:
                        if (((maxR + value) > clipMax) || ((minR + value) < clipMin) ||
                            ((maxG + valueI) > clipMax) || ((minG + valueI) < clipMin) ||
                            ((maxB + valueB) > clipMax) || ((minB + valueB) < clipMin)) {
                            newType = findType(srcImage.getType());
                        }

                        break;

                    case SUBTRACT:
                        if (((minR - value) < clipMin) || ((maxR - value) > clipMax) ||
                            ((minG - valueI) < clipMin) || ((maxG - valueI) > clipMax) ||
                            ((minB - valueB) < clipMin) || ((maxB - valueB) > clipMax)) {
                            newType = findType(srcImage.getType());
                        }

                        break;
                        
                    case MULTIPLY:
                        bestMinR = java.lang.Math.min(minR * value, maxR * value);
                        bestMaxR = java.lang.Math.max(minR * value, maxR * value);
                        bestMinG = java.lang.Math.min(minG * valueI, maxG * valueI);
                        bestMaxG = java.lang.Math.max(minG * valueI, maxG * valueI);
                        bestMinB = java.lang.Math.min(minB * valueB, maxB * valueB);
                        bestMaxB = java.lang.Math.max(minB * valueB, maxB * valueB);
                        bestMin = Math.min(bestMinR, Math.min(bestMinG, bestMinB));
                        bestMax = Math.max(bestMaxR, Math.max(bestMaxG, bestMaxB));
                        if ((bestMax > clipMax) || (bestMin < clipMin)) {
                            newType = findType(srcImage.getType());
                        }

                        break;

                    case DIVIDE:
                        bestMinR = java.lang.Math.min(minR / value, maxR / value);
                        bestMaxR = java.lang.Math.max(minR / value, maxR / value);
                        bestMinG = java.lang.Math.min(minG / valueI, maxG / valueI);
                        bestMaxG = java.lang.Math.max(minG / valueI, maxG / valueI);
                        bestMinB = java.lang.Math.min(minB / valueB, maxB / valueB);
                        bestMaxB = java.lang.Math.max(minB / valueB, maxB / valueB);
                        bestMin = Math.min(bestMinR, Math.min(bestMinG, bestMinB));
                        bestMax = Math.max(bestMaxR, Math.max(bestMaxG, bestMaxB));
                        if ((bestMin < clipMin) || (bestMax > clipMax)) {
                            newType = findType(srcImage.getType());
                        }

                        break;

                    case SQUARE:
                        if (((maxR * maxR) > clipMax) || ((minR * minR) > clipMax) ||
                            ((maxG * maxG) > clipMax) || ((minG * minG) > clipMax) ||
                            ((maxB * maxB) > clipMax) || ((minB * minB) > clipMax)) {
                            newType = findType(srcImage.getType());
                        }

                        break;

                    case CONSTANT:
                        if ((value < clipMin) || (value > clipMax) ||
                            (valueI < clipMin) || (valueI > clipMax) ||
                            (valueB < clipMin) || (valueB > clipMax)) {
                            newType = findType(srcImage.getType());
                        }

                        break;
                        
                    case SUM:
                        if (minR < 0) {
                            bestMinR = minR * sumFactor;
                        }
                        else {
                            bestMinR = 0;
                        }
                        if (maxR > 0) {
                            bestMaxR = maxR * sumFactor;
                        }
                        else {
                            bestMaxR = 0;
                        }
                        
                        if (minG < 0) {
                            bestMinG = minG * sumFactor;
                        }
                        else {
                            bestMinG = 0;
                        }
                        if (maxG > 0) {
                            bestMaxG = maxG * sumFactor;
                        }
                        else {
                            bestMaxG = 0;
                        }
                        
                        if (minB < 0) {
                            bestMinB = minB * sumFactor;
                        }
                        else {
                            bestMinB = 0;
                        }
                        if (maxB > 0) {
                            bestMaxB = maxB * sumFactor;
                        }
                        else {
                            bestMaxB = 0;
                        }
                        
                        bestMin = Math.min(bestMinR, Math.min(bestMinG, bestMinB));
                        bestMax = Math.max(bestMaxR, Math.max(bestMaxG, bestMaxB));
                        if ((bestMin < clipMin) || (bestMax > clipMax)) {
                            newType = findType(srcImage.getType());
                        }

                        break;

                    default:
                        break;
                }

                if (newType > destImage.getType()) {
                    destImage.reallocate(newType);
                }
            }
        }

        // do BEFORE starting progress bar
        if (threadStopped) {
            setCompleted(false);
            finalize();

            return;
        }

        for (m = 0; (m < f) && !threadStopped; m++) {

            for (k = 0; (k < t) && !threadStopped; k++) {

                for (j = 0; (j < z) && !threadStopped; j++) {

                    try {
                        offset = (m * space4D) + (k * volume) + (j * length);
                        srcImage.exportData(4*offset, 4*length, buffer); // locks and releases lock
                    } catch (IOException error) {
                        displayError("Algorithm ImageMath : Image(s) locked");
                        setCompleted(false);
                        

                        return;
                    }

                    for (i = 0; (i < length) && !threadStopped; i++) {

                        try {

                            if (((i % mod) == 0)) {
                                fireProgressStateChanged(Math.round((float) (i + offset) / (totalLength - 1) * 100));
                            }
                        } catch (NullPointerException npe) {

                            if (threadStopped) {
                                Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                                  Preferences.DEBUG_ALGORITHM);
                            }
                        }

                        // Get slice
                        if ((entireImage == true) || mask.get(i + offset)) {

                            switch (opType) {
         
                                case ADD:
                                    buffer[4*i+1] = buffer[4*i+1] + value;
                                    buffer[4*i+2] = buffer[4*i+2] + valueI;
                                    buffer[4*i+3] = buffer[4*i+3] + valueB;
                                    break;

                                case SUBTRACT:
                                    buffer[4*i+1] = buffer[4*i+1] - value;
                                    buffer[4*i+2] = buffer[4*i+2] - valueI;
                                    buffer[4*i+3] = buffer[4*i+3] - valueB;
                                    break;

                                case MULTIPLY:
                                    buffer[4*i+1] = buffer[4*i+1] * value;
                                    buffer[4*i+2] = buffer[4*i+2] * valueI;
                                    buffer[4*i+3] = buffer[4*i+3] * valueB;
                                    break;

                                case DIVIDE:
                                    buffer[4*i+1] = buffer[4*i+1] / value;
                                    buffer[4*i+2] = buffer[4*i+2] / valueI;
                                    buffer[4*i+3] = buffer[4*i+3] / valueB;
                                    break;

                                case SQUARE:
                                    buffer[4*i+1] = buffer[4*i+1] * buffer[4*i+1];
                                    buffer[4*i+2] = buffer[4*i+2] * buffer[4*i+2];
                                    buffer[4*i+3] = buffer[4*i+3] * buffer[4*i+3];
                                    break;

                                case SQUARE_ROOT:
                                    buffer[4*i+1] = Math.sqrt(buffer[4*i+1]);
                                    buffer[4*i+2] = Math.sqrt(buffer[4*i+2]);
                                    buffer[4*i+3] = Math.sqrt(buffer[4*i+3]);
                                    break;

                                case LOG:
                                    buffer[4*i+1] = Math.log(buffer[4*i+1]);
                                    buffer[4*i+2] = Math.log(buffer[4*i+2]);
                                    buffer[4*i+3] = Math.log(buffer[4*i+3]);
                                    break;

                                case CONSTANT:
                                    buffer[4*i+1] = value;
                                    buffer[4*i+2] = valueI;
                                    buffer[4*i+3] = valueB;
                                    break;
                                    
                                case ABSOLUTE_VALUE:
                                    buffer[4*i+1] = Math.abs(buffer[4*i+1]);
                                    buffer[4*i+2] = Math.abs(buffer[4*i+2]);
                                    buffer[4*i+3] = Math.abs(buffer[4*i+3]);
                                    break;

                                
                                case AVERAGE:
                                case SUM:
                                    if (srcImage.getNDims() <= 3) {
                                        sumAverageBuffer[4*i+1] += buffer[4*i+1];
                                        sumAverageBuffer[4*i+2] += buffer[4*i+2];
                                        sumAverageBuffer[4*i+3] += buffer[4*i+3];
                                    }
                                    else if (srcImage.getNDims() == 4) {
                                        sumAverageBuffer[4*j*length + 4*i + 1] += buffer[4*i+1];
                                        sumAverageBuffer[4*j*length + 4*i + 2] += buffer[4*i+2];
                                        sumAverageBuffer[4*j*length + 4*i + 3] += buffer[4*i+3];
                                    }
                                    else { // srcImage.getNDims() == 5
                                        sumAverageBuffer[4*k*volume + 4*j*length + 4*i + 1] += buffer[4*i+1];
                                        sumAverageBuffer[4*k*volume + 4*j*length + 4*i + 2] += buffer[4*i+2];
                                        sumAverageBuffer[4*k*volume + 4*j*length + 4*i + 3] += buffer[4*i+3];
                                    }
                                    break;

                                default:
                                    break;
                            }
                        }
                    }

                    // do not do the following segment for opType AVERAGE:
                    if ((opType != Operator.AVERAGE) && (opType != Operator.SUM)) {

                        // clip check
                        if (clipMode == CLIP) {

                            for (i = 0; i < 4*length; i++) {

                                if (buffer[i] > clipMax) {
                                    buffer[i] = clipMax;
                                } else if (buffer[i] < clipMin) {
                                    buffer[i] = clipMin;
                                }
                            }
                        }

                        try {
                            destImage.importData(4*offset, buffer, false);
                        } catch (IOException error) {
                            displayError("Algorithm ImageMath: Destination Image locked");
                            setCompleted(false);
                            

                            return;
                        }
                    }
                }
            } // t loop
        } // f loop

        if (threadStopped) {
            setCompleted(false);
            
            finalize();

            return;
        }

        if ((opType == Operator.AVERAGE) || (opType == Operator.SUM)) {

            if (opType == Operator.AVERAGE) {

                for (i = 0; i < sumAverageBuffer.length; i++) {
                    sumAverageBuffer[i] /= sumFactor;
                }
            }
            
            if ((opType == Operator.SUM) && (clipMode == CLIP)) {
                for (i = 0; i < sumAverageBuffer.length; i++) {

                    if (sumAverageBuffer[i] > clipMax) {
                        sumAverageBuffer[i] = clipMax;
                    } else if (sumAverageBuffer[i] < clipMin) {
                        sumAverageBuffer[i] = clipMin;
                    }
                }   
            }

            try {
                destImage.importData(0, sumAverageBuffer, false);
            } catch (Exception e) {
                displayError("Algorithm ImageMath: Destination Image locked");
                setCompleted(false);
                

                return;
            }
        }

        destImage.calcMinMax();
        
        setCompleted(true);    
    } // private void calcStoreInDestColor()

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
        double bestMin, bestMax;
        int sumAverageLength;
        int sumFactor = 1;

        double[] sumAverageBuffer = null; // buffer needed to do summing/average

        try {
            length = srcImage.getSliceSize();
            buffer = new double[length];

            if ((opType == Operator.AVERAGE) || (opType == Operator.SUM)) {
                sumAverageLength = length;
                if (srcImage.getNDims() >= 4) {
                    sumAverageLength *= srcImage.getExtents()[2];
                }
                if (srcImage.getNDims() == 5) {
                    sumAverageLength *= srcImage.getExtents()[3];
                }
                sumAverageBuffer = new double[sumAverageLength];
                if (srcImage.getNDims() == 5) {
                    sumFactor = srcImage.getExtents()[4];
                }
                else if (srcImage.getNDims() == 4) {
                    sumFactor = srcImage.getExtents()[3];
                }
                else if (srcImage.getNDims() == 3) {
                    sumFactor = srcImage.getExtents()[2];
                }
                else {
                    sumFactor = 1;
                }
            }

            fireProgressStateChanged(srcImage.getImageName(), "Calculating image ...");
        } catch (OutOfMemoryError e) {
            buffer = null;
            System.gc();
            displayError("Algorithm ImageMath reports: Out of memory when creating image buffer");
            setCompleted(false);

            return;
        }

        min = srcImage.getMin();
        max = srcImage.getMax();
        
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

        int volume = z * length;
        int space4D = t * volume;
        int totalLength = f * t * z * length;

        if (opType == Operator.LOG) {

            if (min <= 0) {
                displayError("Cannot do LOG on image data <= 0");
                setCompleted(false);

                return;
            } else if ((destImage.getType() != ModelStorageBase.FLOAT) &&
                           (destImage.getType() != ModelStorageBase.DOUBLE) && (max < (double) Float.MAX_VALUE)) {
                destImage.reallocate(ModelStorageBase.FLOAT);
            } else if ((destImage.getType() != ModelStorageBase.DOUBLE) && (max >= (double) Float.MAX_VALUE)) {
                destImage.reallocate(ModelStorageBase.DOUBLE);
            }
        } else if (opType == Operator.SQUARE_ROOT) {

            if (min < 0) {
                displayError("Cannot do SQRT on image data < 0");
                setCompleted(false);

                return;
            } else if ((destImage.getType() != ModelStorageBase.FLOAT) &&
                           (destImage.getType() != ModelStorageBase.DOUBLE) && (max < (double) Float.MAX_VALUE)) {
                destImage.reallocate(ModelStorageBase.FLOAT);
            } else if ((destImage.getType() != ModelStorageBase.DOUBLE) && (max >= (double) Float.MAX_VALUE)) {
                destImage.reallocate(ModelStorageBase.DOUBLE);
            }
        } else if ((opType == Operator.DIVIDE) && (value == 0.0)) {
            displayError("cannot divide image data by zero");
            setCompleted(false);

            return;
        } else if (clipMode == CONVERT_FLOAT) {
        	destImage.reallocate(ModelStorageBase.FLOAT);
        } else {
            int newType = srcImage.getType();

            if (clipMode == PROMOTE) {

                switch (opType) {

                    case ADD:
                        if (((max + value) > clipMax) || ((min + value) < clipMin)) {
                            newType = findType(srcImage.getType());
                        }

                        break;

                    case SUBTRACT:
                        if (((min - value) < clipMin) || ((max - value) > clipMax)) {
                            newType = findType(srcImage.getType());
                        }

                        break;

                    case MULTIPLY:
                        bestMin = java.lang.Math.min(min * value, max * value);
                        bestMax = java.lang.Math.max(min * value, max * value);
                        if ((bestMax > clipMax) || (bestMin < clipMin)) {
                            newType = findType(srcImage.getType());
                        }

                        break;

                    case DIVIDE:
                        bestMin = java.lang.Math.min(min / value, max / value);
                        bestMax = java.lang.Math.max(min / value, max / value);
                        if ((bestMin < clipMin) || (bestMax > clipMax)) {
                            newType = findType(srcImage.getType());
                        }

                        break;

                    case SQUARE:
                        if (((max * max) > clipMax) || ((min * min) > clipMax)) {
                            newType = findType(srcImage.getType());
                        }

                        break;

                    case CONSTANT:
                        if ((value < clipMin) || (value > clipMax)) {
                            newType = findType(srcImage.getType());
                        }

                        break;
                        
                    case SUM:
                        
                        if (min < 0) {
                            bestMin = min * sumFactor;
                        }
                        else {
                            bestMin = 0;
                        }
                        if (max > 0) {
                            bestMax = max * sumFactor;
                        }
                        else {
                            bestMax = 0;
                        }
                        
                        if ((bestMin < clipMin) || (bestMax > clipMax)) {
                            newType = findType(srcImage.getType());
                        }

                        break;

                    default:
                        break;
                }

                if (newType > destImage.getType()) {
                    destImage.reallocate(newType);
                }
            }
        }

        // do BEFORE starting progress bar
        if (threadStopped) {
            setCompleted(false);
            finalize();

            return;
        }

        for (m = 0; (m < f) && !threadStopped; m++) {

            for (k = 0; (k < t) && !threadStopped; k++) {

                for (j = 0; (j < z) && !threadStopped; j++) {

                    try {
                        offset = (m * space4D) + (k * volume) + (j * length);
                        srcImage.exportData(offset, length, buffer); // locks and releases lock
                    } catch (IOException error) {
                        displayError("Algorithm ImageMath : Image(s) locked");
                        setCompleted(false);
                        

                        return;
                    }

                    for (i = 0; (i < length) && !threadStopped; i++) {

                        try {

                            if (((i % mod) == 0)) {
                                fireProgressStateChanged(Math.round((float) (i + offset) / (totalLength - 1) * 100));
                            }
                        } catch (NullPointerException npe) {

                            if (threadStopped) {
                                Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                                  Preferences.DEBUG_ALGORITHM);
                            }
                        }

                        // Get slice
                        if ((entireImage == true) || mask.get(i + offset)) {

                            switch (opType) {

                                case ADD:
                                    buffer[i] = buffer[i] + value;
                                    break;

                                case SUBTRACT:
                                    buffer[i] = buffer[i] - value;
                                    break;

                                case MULTIPLY:
                                    buffer[i] = buffer[i] * value;
                                    break;

                                case DIVIDE:
                                    buffer[i] = buffer[i] / value;
                                    break;

                                case SQUARE:
                                    buffer[i] = buffer[i] * buffer[i];
                                    break;

                                case SQUARE_ROOT:
                                    buffer[i] = Math.sqrt(buffer[i]);
                                    break;

                                case LOG:
                                    buffer[i] = Math.log(buffer[i]);
                                    break;

                                case CONSTANT:
                                    buffer[i] = value;
                                    break;

                                case ABSOLUTE_VALUE:
                                    buffer[i] = Math.abs(buffer[i]);
                                    break;
                                    
                                case INVERSE:
                                	if(buffer[i] == 0) {
                                		buffer[i] = Float.NaN;
                                	}else {
                                		buffer[i] = 1 / buffer[i];
                                	}
                                	break;

                                case AVERAGE:
                                case SUM:
                                    if (srcImage.getNDims() <= 3) {
                                        sumAverageBuffer[i] += buffer[i];
                                    }
                                    else if (srcImage.getNDims() == 4) {
                                        sumAverageBuffer[j*length + i] += buffer[i];
                                    }
                                    else { // srcImage.getNDims() == 5
                                        sumAverageBuffer[k*volume + j*length + i] += buffer[i];
                                    }
                                    break;

                                default:
                                    break;
                            }
                        }
                    }

                    // do not do the following segment for opType AVERAGE:
                    if ((opType != Operator.AVERAGE) && (opType != Operator.SUM)) {

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
                            displayError("Algorithm ImageMath: Destination Image locked");
                            setCompleted(false);
                            

                            return;
                        }
                    }
                }
            } // t loop
        } // f loop

        if (threadStopped) {
            setCompleted(false);
            
            finalize();

            return;
        }

        if ((opType == Operator.AVERAGE) || (opType == Operator.SUM)) {

            if (opType == Operator.AVERAGE) {

                for (i = 0; i < sumAverageBuffer.length; i++) {
                    sumAverageBuffer[i] /= sumFactor;
                }
            }
            
            if ((opType == Operator.SUM) && (clipMode == CLIP)) {
                for (i = 0; i < sumAverageBuffer.length; i++) {

                    if (sumAverageBuffer[i] > clipMax) {
                        sumAverageBuffer[i] = clipMax;
                    } else if (sumAverageBuffer[i] < clipMin) {
                        sumAverageBuffer[i] = clipMin;
                    }
                }   
            }

            try {
                destImage.importData(0, sumAverageBuffer, false);
            } catch (Exception e) {
                displayError("Algorithm ImageMath: Destination Image locked");
                setCompleted(false);
                

                return;
            }
        }

        destImage.calcMinMax();
        
        setCompleted(true);
    }

    /**
     * Generates the new data and places in a new (destination) image.
     */
    private void calcStoreInDestComplex() {
        int i, j, k, m;
        int z, t, f;
        int offset;
        int length; // total number of data-elements (pixels) in image
        double[] buffer; // data-buffer (for pixel data) which is the "heart" of the image
        double[] bufferI;
        boolean needPromote = false;
        double[] totBuffer = null;
        double[] totBufferI = null;
        double temp;
        double denom;
        double mag;
        double ang;
        int buffersDone = 0;

        try {
            length = srcImage.getSliceSize();
            buffer = new double[length];
            bufferI = new double[length];
            fireProgressStateChanged(srcImage.getImageName(), "Calculating image ...");
        } catch (OutOfMemoryError e) {
            buffer = null;
            bufferI = null;
            System.gc();
            displayError("Algorithm ImageMath reports: Out of memory when creating image buffer");
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

        int volume = z * length;
        int space4D = t * volume;
        int totalLength = f * t * z * length;

        if ((opType == Operator.DIVIDE) && (value == 0.0) && (valueI == 0.0)) {
            displayError("Algorithm ImageMath: Cannot divide image data by zero");
            setCompleted(false);

            return;
        }

        for (m = 0; (m < f) && !threadStopped; m++) {

            for (k = 0; (k < t) && !threadStopped; k++) {

                for (j = 0; (j < z) && !threadStopped; j++) {

                    try {
                        offset = (m * space4D) + (k * volume) + (j * length);
                        srcImage.exportDComplexData(2 * offset, length, buffer, bufferI); // locks and releases lock
                    } catch (IOException error) {
                        displayError("Algorithm ImageMath : Image(s) locked");
                        setCompleted(false);

                        return;
                    }

                    for (i = 0; (i < length) && !threadStopped; i++) {

                        try {

                            if (((i % mod) == 0)) {
                                fireProgressStateChanged(Math.round((float) (i + offset) / (totalLength - 1) * 100));
                            }
                        } catch (NullPointerException npe) {

                            if (threadStopped) {
                                Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                                  Preferences.DEBUG_ALGORITHM);
                            }
                        }

                        // Get slice
                        if ((entireImage == true) || mask.get(i + offset)) {

                            switch (opType) {

                                case ADD:
                                    buffer[i] = buffer[i] + value;
                                    bufferI[i] = bufferI[i] + valueI;
                                    break;

                                case SUBTRACT:
                                    buffer[i] = buffer[i] - value;
                                    bufferI[i] = bufferI[i] - valueI;
                                    break;

                                case MULTIPLY:
                                    temp = (buffer[i] * value) - (bufferI[i] * valueI);
                                    bufferI[i] = (buffer[i] * valueI) + (bufferI[i] * value);
                                    buffer[i] = temp;
                                    break;

                                case DIVIDE:
                                    denom = (value * value) + (valueI * valueI);
                                    temp = ((buffer[i] * value) + (bufferI[i] * valueI)) / denom;
                                    bufferI[i] = ((value * bufferI[i]) - (valueI * buffer[i])) / denom;
                                    buffer[i] = temp;
                                    break;

                                case SQUARE:
                                    temp = (buffer[i] * buffer[i]) - (bufferI[i] * bufferI[i]);
                                    bufferI[i] = 2.0 * buffer[i] * bufferI[i];
                                    buffer[i] = temp;
                                    break;

                                case SQUARE_ROOT:
                                    mag = Math.sqrt(Math.sqrt((buffer[i] * buffer[i]) + (bufferI[i] * bufferI[i])));
                                    ang = 0.5 * Math.atan2(bufferI[i], buffer[i]);
                                    buffer[i] = Math.cos(ang) * mag;
                                    bufferI[i] = Math.sin(ang) * mag;
                                    break;

                                case LOG:
                                    mag = Math.sqrt((buffer[i] * buffer[i]) + (bufferI[i] * bufferI[i]));
                                    buffer[i] = Math.log(mag);
                                    bufferI[i] = Math.atan2(bufferI[i], buffer[i]);

                                    // The principal value has the 0 <= angle < 2*PI
                                    if (bufferI[i] < 0.0) {
                                        bufferI[i] = bufferI[i] + (2.0 * Math.PI);
                                    }

                                    break;

                                case CONSTANT:
                                    buffer[i] = value;
                                    bufferI[i] = valueI;
                                    break;

                                default:
                                    break;
                            }
                        }
                    }

                    // clip check
                    if (clipMode == CLIP) {

                        // Keep same phase in clipping
                        for (i = 0; i < length; i++) {

                            if ((buffer[i] > Float.MAX_VALUE) || (buffer[i] < -Float.MAX_VALUE) ||
                                    (bufferI[i] > Float.MAX_VALUE) || (bufferI[i] < -Float.MAX_VALUE)) {

                                if (Math.abs(buffer[i]) >= Math.abs(bufferI[i])) {

                                    if (buffer[i] > Float.MAX_VALUE) {
                                        bufferI[i] = bufferI[i] * (Float.MAX_VALUE / buffer[i]);
                                        buffer[i] = Float.MAX_VALUE;
                                    } else {
                                        bufferI[i] = bufferI[i] * (-Float.MAX_VALUE / buffer[i]);
                                        buffer[i] = -Float.MAX_VALUE;
                                    }
                                } else { // Math.abs(buffer[i]) < Math.abs(bufferI[i])

                                    if (bufferI[i] > Float.MAX_VALUE) {
                                        buffer[i] = buffer[i] * (Float.MAX_VALUE / bufferI[i]);
                                        bufferI[i] = Float.MAX_VALUE;
                                    } else {
                                        buffer[i] = buffer[i] * (-Float.MAX_VALUE / bufferI[i]);
                                        bufferI[i] = -Float.MAX_VALUE;
                                    }
                                }
                            }
                        }
                    } // if (clipMode == CLIP)
                    else if ((clipMode == PROMOTE) && (destImage.getType() == ModelStorageBase.COMPLEX)) {

                        for (i = 0; (i < length) && !needPromote; i++) {

                            if ((buffer[i] > Float.MAX_VALUE) || (buffer[i] < -Float.MAX_VALUE) ||
                                    (bufferI[i] > Float.MAX_VALUE) || (bufferI[i] < -Float.MAX_VALUE)) {
                                needPromote = true;
                            }
                        }
                    }

                    if (needPromote) {
                        needPromote = false;

                        if (buffersDone > 0) {

                            try {
                                totBuffer = new double[buffersDone * length];
                                totBufferI = new double[buffersDone * length];
                            } catch (OutOfMemoryError e) {
                                totBuffer = null;
                                totBufferI = null;
                                System.gc();
                                displayError("Algorithm ImageMath reports: Out of memory when creating total image buffer");
                                setCompleted(false);

                                return;
                            }

                            try {
                                destImage.exportDComplexData(0, buffersDone * length, totBuffer, totBufferI); // locks and releases lock
                            } catch (IOException error) {
                                displayError("Algorithm ImageMath : Image(s) locked");
                                setCompleted(false);

                                return;
                            }
                        } // if (buffersDone > 0)

                        destImage.reallocate(ModelStorageBase.DCOMPLEX);

                        if (buffersDone > 0) {

                            try {

                                // do BEFORE buffer has been exported to Image
                                if (threadStopped) {
                                    finalize();

                                    return;
                                }

                                destImage.importDComplexData(0, totBuffer, totBufferI, false, true);
                            } catch (IOException error) {
                                displayError("Algorithm ImageMath: Image(s) locked");
                                setCompleted(false);
                                

                                return;
                            }

                            totBuffer = null;
                            totBufferI = null;
                        } // if (buffersDone > 0)
                    } // if (needPromote)

                    try {

                        // do BEFORE buffer has been exported to Image
                        if (threadStopped) {
                            finalize();

                            return;
                        }

                        destImage.importDComplexData(2 * offset, buffer, bufferI, false, true);
                    } catch (IOException error) {
                        displayError("Algorithm ImageMath: Image(s) locked");
                        setCompleted(false);
                        

                        return;
                    }

                    buffersDone++;
                }
            } // t loop
        } // f loop

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
        double bestMin, bestMax;
        double bestMinR, bestMaxR, bestMinG, bestMaxG, bestMinB, bestMaxB;
        int sumFactor;

        endType = stType;

        if (srcImage.isColorImage()) {
            switch (opType) {
                
                case ADD:
                    while (loop == true) {
    
                        if ((testType(endType, minR + value, maxR + value) == false) ||
                            (testType(endType, minG + valueI, maxG + valueI) == false) ||
                            (testType(endType, minB + valueB, maxB + valueB) == false)) {
                            endType = promoteType(endType);
    
                            if (endType == ModelStorageBase.ARGB_FLOAT) {
                                loop = false;
                            }
                        } else {
                            loop = false;
                        }
                    }
    
                    break;
    
                case SUBTRACT:
                    while (loop == true) {
    
                        if ((testType(endType, minR - value, maxR - value) == false) ||
                            (testType(endType, minG - valueI, maxG - valueI) == false) ||
                            (testType(endType, minB - valueB, maxB - valueB) == false)) {
                            endType = promoteType(endType);
    
                            if (endType == ModelStorageBase.ARGB_FLOAT) {
                                loop = false;
                            }
                        } else {
                            loop = false;
                        }
                    }
    
                    break;
    
                case MULTIPLY:
                    bestMinR = java.lang.Math.min(minR * value, maxR * value);
                    bestMaxR = java.lang.Math.max(minR * value, maxR * value);
                    bestMinG = java.lang.Math.min(minG * valueI, maxG * valueI);
                    bestMaxG = java.lang.Math.max(minG * valueI, maxG * valueI);
                    bestMinB = java.lang.Math.min(minB * valueB, maxB * valueB);
                    bestMaxB = java.lang.Math.max(minB * valueB, maxB * valueB);
                    bestMin = Math.min(bestMinR, Math.min(bestMinG, bestMinB));
                    bestMax = Math.max(bestMaxR, Math.max(bestMaxG, bestMaxB));
                    while (loop == true) {
    
                        if (testType(endType, bestMin, bestMax) == false) {
                            endType = promoteType(endType);
    
                            if (endType == ModelStorageBase.ARGB_FLOAT) {
                                loop = false;
                            }
                        } else {
                            loop = false;
                        }
                    }
    
                    break;
    
                case DIVIDE:
                    bestMinR = java.lang.Math.min(minR / value, maxR / value);
                    bestMaxR = java.lang.Math.max(minR / value, maxR / value);
                    bestMinG = java.lang.Math.min(minG / valueI, maxG / valueI);
                    bestMaxG = java.lang.Math.max(minG / valueI, maxG / valueI);
                    bestMinB = java.lang.Math.min(minB / valueB, maxB / valueB);
                    bestMaxB = java.lang.Math.max(minB / valueB, maxB / valueB);
                    bestMin = Math.min(bestMinR, Math.min(bestMinG, bestMinB));
                    bestMax = Math.max(bestMaxR, Math.max(bestMaxG, bestMaxB));
                    while (loop == true) {
    
                        if (testType(endType, bestMin, bestMax) == false) {
                            endType = promoteType(endType);
    
                            if (endType == ModelStorageBase.ARGB_FLOAT) {
                                loop = false;
                            }
                        } else {
                            loop = false;
                        }
                    }
    
                    break;
    
                case SQUARE:
                    if ((minR <= 0.0) && (maxR >= 0.0)) {
                        bestMinR = 0.0;
                    } else {
                        bestMinR = java.lang.Math.min(minR * minR, maxR * maxR);
                    }
    
                    bestMaxR = java.lang.Math.max(minR * minR, maxR * maxR);
                    
                    if ((minG <= 0.0) && (maxG >= 0.0)) {
                        bestMinG = 0.0;
                    } else {
                        bestMinG = java.lang.Math.min(minG * minG, maxG * maxG);
                    }
    
                    bestMaxG = java.lang.Math.max(minG * minG, maxG * maxG);
                    
                    if ((minB <= 0.0) && (maxB >= 0.0)) {
                        bestMinB = 0.0;
                    } else {
                        bestMinB = java.lang.Math.min(minB * minB, maxB * maxB);
                    }
    
                    bestMaxB = java.lang.Math.max(minB * minB, maxB * maxB);
                    
                    bestMin = Math.min(bestMinR, Math.min(bestMinG, bestMinB));
                    bestMax = Math.max(bestMaxR, Math.max(bestMaxG, bestMaxB));
                    while (loop == true) {
    
                        if (testType(endType, bestMin, bestMax) == false) {
                            endType = promoteType(endType);
    
                            if (endType == ModelStorageBase.ARGB_FLOAT) {
                                loop = false;
                            }
                        } else {
                            loop = false;
                        }
                    }
    
                    break;
    
                case CONSTANT:
                    while (loop == true) {
    
                        if ((testType(endType, value, value) == false) ||
                            (testType(endType, valueI, valueI) == false) ||
                            (testType(endType, valueB, valueB) == false)) {
                            endType = promoteType(endType);
    
                            if (endType == ModelStorageBase.ARGB_FLOAT) {
                                loop = false;
                            }
                        } else {
                            loop = false;
                        }
                    }
    
                    break;
                    
                case SUM:
                    if (srcImage.getNDims() == 5) {
                        sumFactor = srcImage.getExtents()[4];
                    }
                    else if (srcImage.getNDims() == 4) {
                        sumFactor = srcImage.getExtents()[3];
                    }
                    else if (srcImage.getNDims() == 3) {
                        sumFactor = srcImage.getExtents()[2];
                    }
                    else {
                        sumFactor = 1;
                    }
                    
                    if (minR < 0) {
                        bestMinR = minR * sumFactor;
                    }
                    else {
                        bestMinR = 0;
                    }
                    if (maxR > 0) {
                        bestMaxR = maxR * sumFactor;
                    }
                    else {
                        bestMaxR = 0;
                    }
                    
                    if (minG < 0) {
                        bestMinG = minG * sumFactor;
                    }
                    else {
                        bestMinG = 0;
                    }
                    if (maxG > 0) {
                        bestMaxG = maxG * sumFactor;
                    }
                    else {
                        bestMaxG = 0;
                    }
                    
                    if (minB < 0) {
                        bestMinB = minB * sumFactor;
                    }
                    else {
                        bestMinB = 0;
                    }
                    if (maxB > 0) {
                        bestMaxB = maxB * sumFactor;
                    }
                    else {
                        bestMaxB = 0;
                    }
                    
                    bestMin = Math.min(bestMinR, Math.min(bestMinG, bestMinB));
                    bestMax = Math.max(bestMaxR, Math.max(bestMaxG, bestMaxB));
                    while (loop == true) {
    
                        if (testType(endType, bestMin, bestMax) == false) {
                            endType = promoteType(endType);
    
                            if (endType == ModelStorageBase.ARGB_FLOAT) {
                                loop = false;
                            }
                        } else {
                            loop = false;
                        }
                    }
    
                    break;
    
                default:
                    break;
            } // switch (opType)    
        } // if (srcImage.isColorImage())
        else { // black and white image
            switch (opType) {
    
                case ADD:
                    while (loop == true) {
    
                        if (testType(endType, min + value, max + value) == false) {
                            endType = promoteType(endType);
    
                            if (endType == ModelStorageBase.DOUBLE) {
                                loop = false;
                            }
                        } else {
                            loop = false;
                        }
                    }
    
                    break;
    
                case SUBTRACT:
                    while (loop == true) {
    
                        if (testType(endType, min - value, max - value) == false) {
                            endType = promoteType(endType);
    
                            if (endType == ModelStorageBase.DOUBLE) {
                                loop = false;
                            }
                        } else {
                            loop = false;
                        }
                    }
    
                    break;
    
                case MULTIPLY:
                    bestMin = java.lang.Math.min(min * value, max * value);
                    bestMax = java.lang.Math.max(min * value, max * value);
                    while (loop == true) {
    
                        if (testType(endType, bestMin, bestMax) == false) {
                            endType = promoteType(endType);
    
                            if (endType == ModelStorageBase.DOUBLE) {
                                loop = false;
                            }
                        } else {
                            loop = false;
                        }
                    }
    
                    break;
    
                case DIVIDE:
                    bestMin = java.lang.Math.min(min / value, max / value);
                    bestMax = java.lang.Math.max(min / value, max / value);
                    while (loop == true) {
    
                        if (testType(endType, bestMin, bestMax) == false) {
                            endType = promoteType(endType);
    
                            if (endType == ModelStorageBase.DOUBLE) {
                                loop = false;
                            }
                        } else {
                            loop = false;
                        }
                    }
    
                    break;
    
                case SQUARE:
                    if ((min <= 0.0) && (max >= 0.0)) {
                        bestMin = 0.0;
                    } else {
                        bestMin = java.lang.Math.min(min * min, max * max);
                    }
    
                    bestMax = java.lang.Math.max(min * min, max * max);
                    while (loop == true) {
    
                        if (testType(endType, bestMin, bestMax) == false) {
                            endType = promoteType(endType);
    
                            if (endType == ModelStorageBase.DOUBLE) {
                                loop = false;
                            }
                        } else {
                            loop = false;
                        }
                    }
    
                    break;
    
                case CONSTANT:
                    while (loop == true) {
    
                        if (testType(endType, value, value) == false) {
                            endType = promoteType(endType);
    
                            if (endType == ModelStorageBase.DOUBLE) {
                                loop = false;
                            }
                        } else {
                            loop = false;
                        }
                    }
    
                    break;
                    
                case SUM:
                    if (srcImage.getNDims() == 5) {
                        sumFactor = srcImage.getExtents()[4];
                    }
                    else if (srcImage.getNDims() == 4) {
                        sumFactor = srcImage.getExtents()[3];
                    }
                    else if (srcImage.getNDims() == 3) {
                        sumFactor = srcImage.getExtents()[2];
                    }
                    else {
                        sumFactor = 1;
                    }
                    
                    if (min < 0) {
                        bestMin = min * sumFactor;
                    }
                    else {
                        bestMin = 0;
                    }
                    if (max > 0) {
                        bestMax = max * sumFactor;
                    }
                    else {
                        bestMax = 0;
                    }
                    
                    while (loop == true) {
    
                        if (testType(endType, bestMin, bestMax) == false) {
                            endType = promoteType(endType);
    
                            if (endType == ModelStorageBase.DOUBLE) {
                                loop = false;
                            }
                        } else {
                            loop = false;
                        }
                    }
    
                    break;
    
                default:
                    break;
            } // switch (opType)
        } // else black and white image

        return endType;

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
                
            case ModelStorageBase.ARGB:
                return ModelStorageBase.ARGB_USHORT;
                
            case ModelStorageBase.ARGB_USHORT:
                return ModelStorageBase.ARGB_FLOAT;
                
            case ModelStorageBase.ARGB_FLOAT:
                return ModelStorageBase.ARGB_FLOAT;

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
        } else if ((srcImage.getType() == ModelStorageBase.UBYTE) ||
                   (srcImage.getType() == ModelStorageBase.ARGB)) {
            clipMin = 0;
            clipMax = 255;
        } else if (srcImage.getType() == ModelStorageBase.SHORT) {
            clipMin = -32768;
            clipMax = 32767;
        } else if ((srcImage.getType() == ModelStorageBase.USHORT) ||
                   (srcImage.getType() == ModelStorageBase.ARGB_USHORT)) {
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
        } else if (srcImage.getType() == ModelStorageBase.ARGB_FLOAT) {
            clipMin = -Float.MAX_VALUE;
            clipMax = Float.MAX_VALUE;
        } else if (srcImage.getType() == ModelStorageBase.DOUBLE) {
            clipMin = -Double.MAX_VALUE;
            clipMax = Double.MAX_VALUE;
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
        } else if ((type == ModelStorageBase.UBYTE) || (type == ModelStorageBase.ARGB)) {

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
        } else if ((type == ModelStorageBase.USHORT) || (type == ModelStorageBase.ARGB_USHORT)) {

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
        } else if (type == ModelStorageBase.ARGB_FLOAT) {

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
