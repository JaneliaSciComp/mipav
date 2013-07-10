package gov.nih.mipav.model.structures;


import gov.nih.mipav.util.MipavCoordinateSystems;

import gov.nih.mipav.model.file.FileInfoBase;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.Preferences.ComplexDisplay;

import java.io.IOException;
import java.lang.reflect.Array;
import java.util.*;

import WildMagic.LibFoundation.Mathematics.*;


/**
 * N-dimensional buffer (physically stored as a 1D buffer).
 * 
 * @version 1.0 Sept 1, 1998
 * @author Matthew J. McAuliffe
 */
/**
 * @author pandyan
 * 
 */
public class ModelStorageBase extends ModelSerialCloneable {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -3710345290762731636L;

    /** Used to indicate that the buffer is unlocked and can be writen to or read from. */
    public static final int UNLOCKED = 0;

    /** Used to indicate that the buffer is locked from reading. */
    public static final int RW_LOCKED = 1;

    /** Used to indicate that the buffer is locked from writing. */
    public static final int W_LOCKED = 2;

    /** Used to indicate nearest neighbor interpolation. */
    public static final int NEAREST = 0;

    /** Used to indicate linear (ie. bilinear, trilinear) interpolation. */
    public static final int LINEAR = 1;

    /** Used to indicate Taylor series interpolation. */
    public static final int TAYLOR = 2;
    
    public enum DataType {
        /** Data buffer is of type Boolean (1 bit per voxel). */
        BOOLEAN(0, "Boolean", 0, 1),
        /** Data buffer is of type signed byte (8 bits per voxel). */
        BYTE(1, "Byte", Byte.MIN_VALUE, Byte.MAX_VALUE),
        /** Data buffer is of type unsigned byte (8 bits per voxel). */
        UBYTE(2, "Unsigned Byte", 0, 255),
        /** Data buffer is of type signed short (16 bits per voxel). */
        SHORT(3, "Short", Short.MIN_VALUE, Short.MAX_VALUE),
        /** Data buffer is of type unsigned short (16 bits per voxal). */
        USHORT(4, "Unsigned Short", 0, 65535),
        /** Data buffer is of type signed integer (32 bits per voxel). */
        INTEGER(5, "Integer", Integer.MIN_VALUE, Integer.MAX_VALUE),
        /** Data buffer is of type signed long integer (64 bits per voxel). */
        LONG(6, "Long", Long.MIN_VALUE, Long.MAX_VALUE),
        /** Data buffer is of type float (32 bits per voxel). */
        FLOAT(7, "Float", -Float.MAX_VALUE, Float.MAX_VALUE),
        /** Data buffer is of type double (64 bits per voxel). */
        DOUBLE(8, "Double", -Double.MAX_VALUE, Double.MAX_VALUE),
        /** 
         * Data buffer is of type ARGB where each channel (A = alpha, R = red, G = green, B =
         * blue) is represented by a unsigned byte value. (4 * UBYTE(8 bits) = 4 bytes) 
         */
        ARGB(9, "ARGB", UBYTE.typeMin, UBYTE.typeMax),
        /** 
         * Data buffer is of type ARGB where each channel (A = alpha, R = red, G = green, B =
         * blue) is represented by a unsigned short value. (4 * USHORT(16 bits) = 8 bytes)
         */
        ARGB_USHORT(10, "ARGB Ushort", USHORT.typeMin, USHORT.typeMax),
        /** 
         * Data buffer is of type ARGB where each channel (A = alpha, R = red, G = green, B =
         * blue) is represented by a float value. (4 * FLOAT(32 bits) = 16 bytes)
         */
        ARGB_FLOAT(11, "ARGB Float", -Float.MAX_VALUE, Float.MAX_VALUE),
        /** Data buffer is of type complex type floats (2 x 64 bits per voxel). */
        COMPLEX(12, "Complex", -Float.MAX_VALUE, Float.MAX_VALUE),
        /** Data buffer is of type complex type of doubles (2 x 128 bits per voxel). */
        DCOMPLEX(13, "Complex Double", -Double.MAX_VALUE, Double.MAX_VALUE),
        /** Data buffer is of type unsigned integer (32 bits per voxel). */
        UINTEGER(14, "Unsigned Integer", 0, 4294967295L);
        
        /** These variables preserve the legacy ordering of these enums. */
        private int legacyNum;
        private String legacyName;
        /** Min value allowed of the type */
        private Number typeMin;
        /** Max value allowed of the type */
        private Number typeMax;

        DataType(int legacyNum, String legacyName, Number typeMin, Number typeMax) {
            this.legacyNum = legacyNum;
            this.legacyName = legacyName;
            this.typeMin = typeMin;
            this.typeMax = typeMax;
        }

        public int getLegacyNum() {
            return legacyNum;
        }

        /**
         * This method returns the min value allowed of the type.
         * 
         * @return A java.lang.Number
         */
        public Number getTypeMin() {
            return typeMin;
        }

        /**
         * This method return the max value allowed of the type.
         * 
         * @return A java.lang.Number
         */
        public Number getTypeMax() {
            return typeMax;
        }
        
        public String toString() {
            return legacyName;
        }
        
        public static DataType getDataType(int legacyNum) {
            for(DataType d : DataType.values()) {
                if(d.legacyNum == legacyNum) {
                    return d;
                }
            }
            return null;
        }
    }
    
    /** Used to indicate that the data buffer is of type Boolean (1 bit per voxel). */
    public static final int BOOLEAN = 0;

    /** Used to indicate that the data buffer is of type signed byte (8 bits per voxel). */
    public static final int BYTE = 1;

    /** Used to indicate that the data buffer is of type unsigned byte (8 bits per voxel). */
    public static final int UBYTE = 2;

    /** Used to indicate that the data buffer is of type signed short (16 bits per voxel). */
    public static final int SHORT = 3;

    /** Used to indicate that the data buffer is of type unsigned short (16 bits per voxal). */
    public static final int USHORT = 4;

    /** Used to indicate that the data buffer is of type signed integer (32 bits per voxel). */
    public static final int INTEGER = 5;

    /** Used to indicate that the data buffer is of type unsigned integer (32 bits per voxel). */
    public static final int UINTEGER = 14;

    /** Used to indicate that the data buffer is of type signed long integer (64 bits per voxel). */
    public static final int LONG = 6;

    /** Used to indicate that the data buffer is of type float (32 bits per voxel). */
    public static final int FLOAT = 7;

    /** Used to indicate that the data buffer is of type double (64 bits per voxel). */
    public static final int DOUBLE = 8;

    /**
     * Used to indicate that the data buffer is of type ARGB where each channel (A = alpha, R = red, G = green, B =
     * blue) is represented by a unsigned byte value. (4 * UBYTE(8 bits) = 4 bytes)
     */
    public static final int ARGB = 9;

    /**
     * Used to indicate that the data buffer is of type ARGB where each channel (A = alpha, R = red, G = green, B =
     * blue) is represented by a unsigned short value. (4 * USHORT(16 bits) = 8 bytes)
     */
    public static final int ARGB_USHORT = 10;

    /**
     * Used to indicate that the data buffer is of type ARGB where each channel (A = alpha, R = red, G = green, B =
     * blue) is represented by a float value. (4 * FLOAT(32 bits) = 16 bytes)
     */
    public static final int ARGB_FLOAT = 11;

    /** Used to indicate that the data buffer is of complex type floats (2 x 64 bits per voxel). */
    public static final int COMPLEX = 12;

    /** Used to indicate that the data buffer is of complex type of doubles (2 x 128 bits per voxel). */
    public static final int DCOMPLEX = 13;

    /** Used to indicate, as a String, that the data buffer is of type boolean. */
    public static final String BOOLEAN_STRING = "Boolean";

    /** Used to indicate, as a String, that the data buffer is of type byte. */
    public static final String BYTE_STRING = "Byte";

    /** Used to indicate, as a String, that the data buffer is of type unsigned byte. */
    public static final String UBYTE_STRING = "Unsigned Byte";

    /** Used to indicate, as a String, that the data buffer is of type signed short. */
    public static final String SHORT_STRING = "Short";

    /** Used to indicate, as a String, that the data buffer is of type unsigned short. */
    public static final String USHORT_STRING = "Unsigned Short";

    /** Used to indicate, as a String, that the data buffer is of type signed integer. */
    public static final String INTEGER_STRING = "Integer";

    /** Used to indicate, as a String, that the data buffer is of type signed long. */
    public static final String LONG_STRING = "Long";

    /** Used to indicate, as a String, that the data buffer is of type signed float. */
    public static final String FLOAT_STRING = "Float";

    /** Used to indicate, as a String, that the data buffer is of type signed double. */
    public static final String DOUBLE_STRING = "Double";

    /** Used to indicate, as a String, that the data buffer is of type color (ARGB - unsigned bytes). */
    public static final String ARGB_STRING = "ARGB";

    /** Used to indicate, as a String, that the data buffer is of type color (ARGB - unsigned shorts). */
    public static final String ARGB_USHORT_STRING = "ARGB Ushort";

    /** Used to indicate, as a String, that the data buffer is of type color (ARGB - floats). */
    public static final String ARGB_FLOAT_STRING = "ARGB Float"; // 4 * FLOAT(32 bits) = 16 bytes

    /** Used to indicate, as a String, that the data buffer is of type complex (float). */
    public static final String COMPLEX_STRING = "Complex"; // 2 * FLOAT(32 bits) = 8 bytes

    /** Used to indicate, as a String, that the data buffer is of type complex (double). */
    public static final String DCOMPLEX_STRING = "Complex Double"; // 2 * DOUBLE(64 bits) = 16 bytes

    /** Used to indicate, as a String, that the data buffer is of type unsigned integer. */
    public static final String UINTEGER_STRING = "Unsigned Integer";

    
    /** String representations of the data types supported by ModelStorageBase. 
     * @deprecated should use enums directly
     * */
    public static final String[] bufferTypeStr;
    
    static {
        bufferTypeStr = new String[DataType.values().length];
        for(int i=0; i<DataType.values().length; i++) {
            bufferTypeStr[DataType.values()[i].legacyNum] = DataType.values()[i].toString();
        }
    }

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** Each image slice has a information that describes aspects of each slice of an image. */
    protected FileInfoBase[] fileInfo;

    /**
     * lastMin and lastMax store the last min and max values for an image. these are used as a trigger for resetting the
     * transfer function.
     */
    protected double lastMin, lastMax;

    /** Type of image buffer (i.e. BOOLEAN, BYTE, UBYTE, SHORT ...) */
    private DataType bufferType;

    /** Storage location of image data. */
    private BufferBase data;

    /** Total buffer length. */
    private int dataSize;

    /**
     * Bounds of the image where.
     * 
     * <ul>
     * <li>dimExtents[0] = x dimension</li>
     * <li>dimExtents[1] = y dimension</li>
     * <li>dimExtents[2] = z dimension (typically)</li>
     * <li>dimExtents[3] = fourth dimension (ie time)...</li>
     * </ul>
     */
    private int[] dimExtents;

    /**
     * Bounds of the image where.
     * 
     * <ul>
     * <li>dimOriginalExtents[0] = x dimension</li>
     * <li>dimOriginalExtents[1] = y dimension</li>
     * <li>dimOriginalExtents[2] = z dimension (typically)</li>
     * <li>dimOriginalExtents[3] = fourth dimension (ie time)...</li>
     * </ul>
     */
    private int[] dimOriginalExtents;

    /** DOCUMENT ME! - TO BE MOVED - redesigned */
    private int filterType = 2;

    /** DOCUMENT ME! - TO BE MOVED - redesigned */
    private float freq1 = 0.4f;

    /** DOCUMENT ME! - TO BE MOVED - redesigned */
    private float freq2 = 0.7f;

    /** 5 variables used in Gabor transform. - should be removed - redesigned */
    private float freqU = 0.0f; // frequency along horizontal axis before rotation from -1 to 1

    /** DOCUMENT ME! - TO BE MOVED - redesigned */
    private float freqV = 0.0f; // frequency along vertical axis before rotation from -1 to 1

    /** DOCUMENT ME! - TO BE MOVED - redesigned */
    private boolean haveWindowed;

    /** Boolean telling if 3D images are processed 1 slice at a time. */
    private boolean image25D;

    /** Locking status of the image. Default = UNLOCKED */
    private int lockStatus = ModelStorageBase.UNLOCKED;

    /** Boolean telling if log magnitude display is used for the data buffer */
    protected boolean logMagDisp;

    /**
     * Set to true when the image is the product of the ffts of two images, so that on an inverse_fft this image will be
     * centered properly. Default is false.
     */
    private boolean m_bConvolve = false;

    /** When true, display the data in the Radiological View, when false display the Neurological View:. */
    private boolean m_bRadiologicalView = true;

    /** Surface color vector. - TO BE MOVED - redesigned */
    private final Vector<ColorRGBA> m_kColorVector = new Vector<ColorRGBA>();

    /** Surface mask color vector. - TO BE MOVED - redesigned */
    private final Vector<ColorRGBA[]> m_kMaskColorVector = new Vector<ColorRGBA[]>();

    /** Surface mask vector.- TO BE MOVED - redesigned. */
    private final Vector<BitSet> m_kMaskVector = new Vector<BitSet>();

    /** Minimum and maximum image intensity. */
    private double min, max;

    /** Minimum and maximum for a color image. */
    private double minA, maxA, minR, maxR, minG, maxG, minB, maxB;

    /** Number of dimensions of the data. */
    private int nDims;

    /** Minimum and maximum image intensity with log magnitude operation removed. - TO BE MOVED */
    private double noLogMin, noLogMax, noLogMinNonZero;

    /** Minimum and maximum nonzero intensity. - TO BE MOVED - redesigned */
    private double nonZeroMin, nonZeroMax;

    /** Minimum and maximum nonzero image RGB. - TO BE MOVED - redesigned */
    private double nonZeroMinR, nonZeroMaxR, nonZeroMinG, nonZeroMaxG, nonZeroMinB, nonZeroMaxB;

    /** DOCUMENT ME!- TO BE MOVED - redesigned. */
    private int originalButterworthOrder;

    /** DOCUMENT ME!- TO BE MOVED - redesigned. */
    private boolean originalCropCheckbox;

    /** DOCUMENT ME! - TO BE MOVED - redesigned */
    private boolean originalDoCrop;

    /** DOCUMENT ME!- TO BE MOVED - redesigned. */
    private int[] originalEnd;

    /** DOCUMENT ME! - TO BE MOVED- redesigned */
    private int originalFilterConstruction;

    /** This are variables used by the FFT filter algorithm. - should be removed - redesigned */
    private int originalKernelDimension;

    /** DOCUMENT ME! - TO BE MOVED - redesigned */
    private float originalMaximum;

    /** DOCUMENT ME! - TO BE MOVED - redesigned */
    private float originalMinimum;

    /** DOCUMENT ME! - TO BE MOVED - redesigned */
    private int[] originalStart;

    /** DOCUMENT ME! - TO BE MOVED - redesigned */
    private float sigmaU = 0.1f; // standard deviation along horizontal axis before rotation

    /** DOCUMENT ME! - should be removed - redesigned */
    private float sigmaV = 0.1f; // standard deviation along vertical axis before rotation

    /** smallest magnitude negative and positive. */
    private double smallestMagnitudeNegative, smallestMagnitudePositive;

    /** smallest magnitude negative and positive. */
    private double smallestMagnitudeNegativeR, smallestMagnitudePositiveR, smallestMagnitudeNegativeG,
            smallestMagnitudePositiveG, smallestMagnitudeNegativeB, smallestMagnitudePositiveB;

    /** DOCUMENT ME! - TO BE MOVED - redesigned */
    private float theta = 0.0f; // angle or rotation in radians

    /** Boolean telling if unequal dimensions are allowed in the FFT image. TO BE MOVED */
    private boolean unequalDim;

    /** Locking write count. */
    private int writeLockCount = 0;

    private float avgInten, avgIntenR, avgIntenG, avgIntenB, stdDeviation, stdDeviationR, stdDeviationG, stdDeviationB;

    private int numPixels;

    private double sumPixelInten, sumPixelIntenR, sumPixelIntenG, sumPixelIntenB;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Default constructor.
     */
    public ModelStorageBase() {
        data = null;
    }

    /**
     * Allocates buffer memory of the specified type.
     * 
     * @param type type of buffer to allocate
     * @param dimExtents extents of the buffer in each dimension (multipleid together produces the size of the buffer to
     *            be allocated
     */
    public ModelStorageBase(final int type, final int[] dimExtents) {
        this(DataType.getDataType(type), dimExtents);
    }
    
    /**
     * Allocates buffer memory of the specified type.
     * 
     * @param type type of buffer to allocate
     * @param dimExtents extents of the buffer in each dimension (multipleid together produces the size of the buffer to
     *            be allocated
     */
    public ModelStorageBase(final DataType type, final int[] dimExtents) {
        construct(type, dimExtents);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Returns the string for a particular buffer type.
     * 
     * @param type int representing the buffer type (see the static definitions)
     * 
     * @return string representing the buffer type
     */
    public static String getBufferTypeStr(final int type) {

        try {
            return ModelStorageBase.bufferTypeStr[type];
        } catch (final ArrayIndexOutOfBoundsException ae) {}

        return "";

    } // end getBufferTypeStr()

    /**
     * Adds a surface mask to this image.
     * 
     * @param index the index of the mask.
     * @param kMask the surface mask
     * @param kMaskColors the per-voxel colors for the mask
     * @param kColor the overall mask color if per-voxel mask colors are not defined.
     */
    public void addSurfaceMask(final int index, final BitSet kMask, final ColorRGBA[] kMaskColors,
            final ColorRGBA kColor) {

        if (kColor != null) {

            if (m_kColorVector.size() > index) {
                m_kColorVector.removeElementAt(index);
            }

            m_kColorVector.insertElementAt(kColor, index);
        }

        if (kMask != null) {

            if (m_kMaskVector.size() > index) {
                m_kMaskVector.removeElementAt(index);
            }

            m_kMaskVector.insertElementAt(kMask, index);
        }

        if (kMaskColors != null) {

            if (m_kMaskColorVector.size() > index) {
                m_kMaskColorVector.removeElementAt(index);
            }

            m_kMaskColorVector.insertElementAt(kMaskColors, index);
        }
    }

    /**
     * Calculates the min and max values for the image array.
     * 
     */
    public void calcMinMax() {
        calcMinMax(logMagDisp);
    }

    /**
     * @param logMagDisplay if true calculate min and max for log10 of 1 + magnitude Calculates the min and max values
     *            for the image array.
     */
    public void calcMinMax(final boolean logMagDisplay) {

        // before computing the new min, max, save the last values.
        lastMin = min;
        lastMax = max;

        min = Double.POSITIVE_INFINITY;
        max = Double.NEGATIVE_INFINITY;

        int i;
        double value;

        if ( (bufferType == DataType.COMPLEX) || (bufferType == DataType.DCOMPLEX)) {
            calcMinMaxMag(logMagDisplay);
        } else if ( (bufferType != DataType.ARGB) && (bufferType != DataType.ARGB_USHORT)
                && (bufferType != DataType.ARGB_FLOAT)) {

            /*
             * if (bufferType == SHORT) { // I thought this would be faster but it is not -- Matt 9/5/2008 short
             * minShort; short maxShort; short valueShort; minShort = Short.MAX_VALUE; maxShort = Short.MIN_VALUE; for
             * (i = 0; i < dataSize; i++) { valueShort = data.getShort(i);
             * 
             * if (valueShort > maxShort) { maxShort = valueShort; }
             * 
             * if (valueShort < minShort) { minShort = valueShort; } } min = minShort; max = maxShort; }
             */
            // else if (bufferType == DOUBLE) {
            for (i = 0; i < dataSize; i++) {
                value = data.getDouble(i);
                if (value > max) {
                    max = value;
                }

                if (value < min) {
                    min = value;
                }
            }
            // }
        } else { // color
            minA = Double.POSITIVE_INFINITY;
            maxA = Double.NEGATIVE_INFINITY;
            minR = Double.POSITIVE_INFINITY;
            maxR = Double.NEGATIVE_INFINITY;
            minG = Double.POSITIVE_INFINITY;
            maxG = Double.NEGATIVE_INFINITY;
            minB = Double.POSITIVE_INFINITY;
            maxB = Double.NEGATIVE_INFINITY;

            for (i = 0; i < dataSize; i += 4) {
                value = data.getDouble(i);

                if (value > maxA) {
                    maxA = value;
                }

                if (value < minA) {
                    minA = value;
                }
                
                value = data.getDouble(i + 1);

                if (value > maxR) {
                    maxR = value;
                }

                if (value < minR) {
                    minR = value;
                }

                value = data.getDouble(i + 2);

                if (value > maxG) {
                    maxG = value;
                }

                if (value < minG) {
                    minG = value;
                }

                value = data.getDouble(i + 3);

                if (value > maxB) {
                    maxB = value;
                }

                if (value < minB) {
                    minB = value;
                }
            } // for(i= 0; i < dataSize; i+=4)

            min = Math.min(minR, Math.min(minG, minB));
            max = Math.max(maxR, Math.max(maxG, maxB));
        } // else color

    }

    /**
     * Calculates the min and max magnitude values for the image array.
     * 
     * @param logMagDisplay if true calculate min and max for log10 of 1 + magnitude
     */
    public void calcMinMaxMag(final boolean logMagDisplay) {
        min = Double.POSITIVE_INFINITY;
        max = Double.NEGATIVE_INFINITY;
        noLogMin = Double.POSITIVE_INFINITY;
        noLogMinNonZero = Double.POSITIVE_INFINITY;
        noLogMax = Double.NEGATIVE_INFINITY;

        int i;
        double valuer, valuei, value;

        for (i = 0; i < dataSize; i = i + 2) {
            valuer = data.getDouble(i);
            valuei = data.getDouble(i + 1);
            value = java.lang.Math.sqrt( (valuer * valuer) + (valuei * valuei));

            if (logMagDisplay) {

                if (value > noLogMax) {
                    noLogMax = value;
                }

                if (value < noLogMin) {
                    noLogMin = value;
                }

                if ( (value > 0) && (value < noLogMinNonZero)) {
                    noLogMinNonZero = value;
                }

                value = 0.4342944819 * java.lang.Math.log(1 + value);
            }

            if (value > max) {
                max = value;
            }

            if (value < min) {
                min = value;
            }
        }

        for (i = 0; i < fileInfo.length; i++) {
            fileInfo[i].setMin(getMin());
            fileInfo[i].setMax(getMax());
        }

    }

    /**
     * Calculates the min and max nonzero values for the image array.
     */
    public void calcMinMaxNonZero() {
        boolean foundNonZeroMin;
        boolean foundNonZeroMax;
        boolean foundSmallestMagnitudeNegative;
        boolean foundSmallestMagnitudePositive;
        boolean foundNonZeroMinR;
        boolean foundNonZeroMaxR;
        boolean foundSmallestMagnitudeNegativeR;
        boolean foundSmallestMagnitudePositiveR;
        boolean foundNonZeroMinG;
        boolean foundNonZeroMaxG;
        boolean foundSmallestMagnitudeNegativeG;
        boolean foundSmallestMagnitudePositiveG;
        boolean foundNonZeroMinB;
        boolean foundNonZeroMaxB;
        boolean foundSmallestMagnitudeNegativeB;
        boolean foundSmallestMagnitudePositiveB;

        nonZeroMin = Double.POSITIVE_INFINITY;
        nonZeroMax = -Double.POSITIVE_INFINITY;
        smallestMagnitudeNegative = -Double.POSITIVE_INFINITY;
        smallestMagnitudePositive = Double.POSITIVE_INFINITY;
        nonZeroMinR = Double.POSITIVE_INFINITY;
        nonZeroMaxR = -Double.POSITIVE_INFINITY;
        smallestMagnitudeNegativeR = -Double.POSITIVE_INFINITY;
        smallestMagnitudePositiveR = Double.POSITIVE_INFINITY;
        nonZeroMinG = Double.POSITIVE_INFINITY;
        nonZeroMaxG = -Double.POSITIVE_INFINITY;
        smallestMagnitudeNegativeG = -Double.POSITIVE_INFINITY;
        smallestMagnitudePositiveG = Double.POSITIVE_INFINITY;
        nonZeroMinB = Double.POSITIVE_INFINITY;
        nonZeroMaxB = -Double.POSITIVE_INFINITY;
        smallestMagnitudeNegativeB = -Double.POSITIVE_INFINITY;
        smallestMagnitudePositiveB = Double.POSITIVE_INFINITY;

        int i;
        double value;

        if (bufferType == DataType.BOOLEAN) {
            nonZeroMin = 1;
            nonZeroMax = 1;
            smallestMagnitudeNegative = Double.NaN;
            smallestMagnitudePositive = 1;
        } else if ( (bufferType != DataType.ARGB) && (bufferType != DataType.ARGB_USHORT)
                && (bufferType != DataType.ARGB_FLOAT)) {
            foundNonZeroMin = false;
            foundNonZeroMax = false;
            foundSmallestMagnitudeNegative = false;
            foundSmallestMagnitudePositive = false;

            for (i = 0; i < dataSize; i++) {
                value = data.getDouble(i);

                if (value != 0.0) {

                    if (value > nonZeroMax) {
                        nonZeroMax = value;
                        foundNonZeroMax = true;
                    }

                    if (value < nonZeroMin) {
                        nonZeroMin = value;
                        foundNonZeroMin = true;
                    }

                    if ( (value > 0.0) && (value < smallestMagnitudePositive)) {
                        smallestMagnitudePositive = value;
                        foundSmallestMagnitudePositive = true;
                    }

                    if ( (value < 0.0) && (value > smallestMagnitudeNegative)) {
                        smallestMagnitudeNegative = value;
                        foundSmallestMagnitudeNegative = true;
                    }
                } // if (value != 0.0)
            } // for(i= 0; i < dataSize; i++)

            if ( !foundNonZeroMin) {
                nonZeroMin = Double.NaN;
            }

            if ( !foundNonZeroMax) {
                nonZeroMax = Double.NaN;
            }

            if ( !foundSmallestMagnitudePositive) {
                smallestMagnitudePositive = Double.NaN;
            }

            if ( !foundSmallestMagnitudeNegative) {
                smallestMagnitudeNegative = Double.NaN;
            }
        } // else if (bufferType != ARGB &&...)
        else { // color
            foundNonZeroMinR = false;
            foundNonZeroMaxR = false;
            foundSmallestMagnitudeNegativeR = false;
            foundSmallestMagnitudePositiveR = false;
            foundNonZeroMinG = false;
            foundNonZeroMaxG = false;
            foundSmallestMagnitudeNegativeG = false;
            foundSmallestMagnitudePositiveG = false;
            foundNonZeroMinB = false;
            foundNonZeroMaxB = false;
            foundSmallestMagnitudeNegativeB = false;
            foundSmallestMagnitudePositiveB = false;

            for (i = 0; i < dataSize; i += 4) {
                value = Math.abs(data.getDouble(i + 1));

                if (value != 0.0) {

                    if (value > nonZeroMaxR) {
                        nonZeroMaxR = value;
                        foundNonZeroMaxR = true;
                    }

                    if (value < nonZeroMinR) {
                        nonZeroMinR = value;
                        foundNonZeroMinR = true;
                    }

                    if ( (value > 0.0) && (value < smallestMagnitudePositiveR)) {
                        smallestMagnitudePositiveR = value;
                        foundSmallestMagnitudePositiveR = true;
                    }

                    if ( (value < 0.0) && (value > smallestMagnitudeNegativeR)) {
                        smallestMagnitudeNegativeR = value;
                        foundSmallestMagnitudeNegativeR = true;
                    }
                } // if (value != 0.0)

                value = Math.abs(data.getDouble(i + 2));

                if (value != 0.0) {

                    if (value > nonZeroMaxG) {
                        nonZeroMaxG = value;
                        foundNonZeroMaxG = true;
                    }

                    if (value < nonZeroMinG) {
                        nonZeroMinG = value;
                        foundNonZeroMinG = true;
                    }

                    if ( (value > 0.0) && (value < smallestMagnitudePositiveG)) {
                        smallestMagnitudePositiveG = value;
                        foundSmallestMagnitudePositiveG = true;
                    }

                    if ( (value < 0.0) && (value > smallestMagnitudeNegativeG)) {
                        smallestMagnitudeNegativeG = value;
                        foundSmallestMagnitudeNegativeG = true;
                    }
                } // if (value != 0.0)

                value = Math.abs(data.getDouble(i + 3));

                if (value != 0.0) {

                    if (value > nonZeroMaxB) {
                        nonZeroMaxB = value;
                        foundNonZeroMaxB = true;
                    }

                    if (value < nonZeroMinB) {
                        nonZeroMinB = value;
                        foundNonZeroMinB = true;
                    }

                    if ( (value > 0.0) && (value < smallestMagnitudePositiveB)) {
                        smallestMagnitudePositiveB = value;
                        foundSmallestMagnitudePositiveB = true;
                    }

                    if ( (value < 0.0) && (value > smallestMagnitudeNegativeB)) {
                        smallestMagnitudeNegativeB = value;
                        foundSmallestMagnitudeNegativeB = true;
                    }
                } // if (value != 0.0)
            } // for(i= 0; i < dataSize; i+=4)

            if ( !foundNonZeroMinR) {
                nonZeroMinR = Double.NaN;
            }

            if ( !foundNonZeroMaxR) {
                nonZeroMaxR = Double.NaN;
            }

            if ( !foundSmallestMagnitudePositiveR) {
                smallestMagnitudePositiveR = Double.NaN;
            }

            if ( !foundSmallestMagnitudeNegativeR) {
                smallestMagnitudeNegativeR = Double.NaN;
            }

            if ( !foundNonZeroMinG) {
                nonZeroMinG = Double.NaN;
            }

            if ( !foundNonZeroMaxG) {
                nonZeroMaxG = Double.NaN;
            }

            if ( !foundSmallestMagnitudePositiveG) {
                smallestMagnitudePositiveG = Double.NaN;
            }

            if ( !foundSmallestMagnitudeNegativeG) {
                smallestMagnitudeNegativeG = Double.NaN;
            }

            if ( !foundNonZeroMinB) {
                nonZeroMinB = Double.NaN;
            }

            if ( !foundNonZeroMaxB) {
                nonZeroMaxB = Double.NaN;
            }

            if ( !foundSmallestMagnitudePositiveB) {
                smallestMagnitudePositiveB = Double.NaN;
            }

            if ( !foundSmallestMagnitudeNegativeB) {
                smallestMagnitudeNegativeB = Double.NaN;
            }
        } // else color

    }

    /**
     * Copies the object.
     * 
     * @return Returns the cloned object.
     */
    public Object clone() {
        Object obj = null;

        try {
            setLock(ModelStorageBase.W_LOCKED);
            obj = super.clone();
        } catch (final IOException error) {
            MipavUtil.displayError("" + error);

            return null;
        } finally {
            releaseLock();
        }

        return (obj);

    }

    /**
     * Disposes of old data and constructs a new buffer of the user specific type if the image in NOT locked.
     * 
     * @exception IOException throws an exception if the image model is locked.
     */
    public synchronized void convertToFloat() throws IOException {

        if (lockStatus == ModelStorageBase.UNLOCKED) {

            // disposeLocal(); // delete old memory and reallocate
            // data = null;
            // System.gc();
            if (getType() != ModelStorageBase.FLOAT) {
                int i;

                for (this.dataSize = 1, i = 0; i < this.nDims; i++) {
                    this.dataSize *= dimExtents[i];
                }

                final float[] imgBuf = new float[dataSize];

                exportData(0, dataSize, imgBuf);

                data = null;
                System.gc();
                construct(DataType.FLOAT, dimExtents);
                importData(0, imgBuf, true);
            } else {
                return;
            }
        } else {
            throw new IOException("ModelStorageBase: convertToFloat: Image locked !");
        }
    }

    /**
     * Dispose of memory and call the garbage collector.
     */
    public void disposeLocal() {
        int i;

        if (data != null) {

            try {
                data.finalize();
            } catch (final Throwable t) {}
        }

        data = null;

        if (fileInfo != null) {

            for (i = 0; i < fileInfo.length; i++) {

                if (fileInfo[i] != null) {
                    fileInfo[i].finalize();
                    fileInfo[i] = null;
                }
            }
        }

        fileInfo = null;

        dimExtents = null;
        dimOriginalExtents = null;
    }

    /**
     * Exports data based on the mapping from ModelImage space to Patient space.
     * 
     * @param orientation The Patient Orientation of the slice to export
     * @param tSlice Index into the forth dimension
     * @param slice Indicates slice of data to be exported
     * @param values The array in which to write the data
     * 
     * @return A float array of the data mapped to patient space.
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized float[] export(final int orientation, final int tSlice, final int slice,
            final float[] values, boolean bMask) throws IOException {
        final int[] axisOrder = MipavCoordinateSystems.getAxisOrder(this, orientation);
        final boolean[] axisFlip = MipavCoordinateSystems.getAxisFlip(this, orientation);

        return export(axisOrder, axisFlip, tSlice, slice, values, bMask);
    }

    /**
     * Exports data based on the mapping from ModelImage space to Patient space. The mapping parameters are passed in as
     * the axisOrder and axisFlip arrays.
     * 
     * @param axisOrder The mapping of ModelImage space volume axes to Patient space axes
     * @param axisFlip The mapping of ModelImage space volume axes to Patient space axes (invert flags)
     * @param tSlice Index into the forth dimension
     * @param slice Indicates slice of data to be exported
     * @param values The array to write the data into
     * 
     * @return A float array of the data mapped to patient space.
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized float[] export(final int[] axisOrder, final boolean[] axisFlip, final int tSlice,
            final int slice, final float[] values, boolean bMask) throws IOException {
        float[] fReturn = null;

        try {
            setLock(ModelStorageBase.W_LOCKED);

            /* Get the loop bounds, based on the coordinate-systems: transformation: */
            final int iBound = (dimExtents.length > 0) ? dimExtents[axisOrder[0]] : 1;
            final int jBound = (dimExtents.length > 1) ? dimExtents[axisOrder[1]] : 1;
            final int kBound = (dimExtents.length > 2) ? dimExtents[axisOrder[2]] : 1;

            /*
             * Get the loop multiplication factors for indexing into the 1D array with 3 index variables: based on the
             * coordinate-systems: transformation:
             */
            final int[] aiFactors = new int[3];
            aiFactors[0] = 1;
            aiFactors[1] = (dimExtents.length > 1) ? dimExtents[0] : 1;
            aiFactors[2] = (dimExtents.length > 2) ? (dimExtents[0] * dimExtents[1]) : 1;

            final int iFactor = aiFactors[axisOrder[0]];
            final int jFactor = aiFactors[axisOrder[1]];
            final int kFactor = aiFactors[axisOrder[2]];

            int kIndex = slice;

            if (axisFlip[2]) {
                kIndex = (kBound - 1) - slice;
            }

            final int tFactor = (dimExtents.length > 2) ? (dimExtents[0] * dimExtents[1] * dimExtents[2])
                    : ( (dimExtents.length > 1) ? (dimExtents[0] * dimExtents[1])
                            : ( (dimExtents.length > 0) ? dimExtents[0] : 1));

            final boolean exportComplex = (values.length == (2 * iBound * jBound)) ? true : false;
            if(exportComplex && (Preferences.getComplexDisplay() != ComplexDisplay.APLUSBI || 
                                    Preferences.getComplexDisplay() != ComplexDisplay.REITHETA)) {
                Preferences.debug("MIPAV will only display the magnitude of this complex data because the buffer has not been set.", Preferences.DEBUG_FILEIO);
            }
            double real, imaginary, mag;

            /* loop over the 2D image (values) we're writing into */
            for (int j = 0; j < jBound; j++) {

                for (int i = 0; i < iBound; i++) {

                    /* calculate the ModelImage space index: */
                    int iIndex = i;
                    int jIndex = j;

                    if (axisFlip[0]) {
                        iIndex = (iBound - 1) - i;
                    }

                    if (axisFlip[1]) {
                        jIndex = (jBound - 1) - j;
                    }

                    final int index = (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor) + (tSlice * tFactor);

                    /* surface Mask? */
                    ColorRGBA kMaskColor = null;

                    if ( bMask && (m_kMaskVector != null) ) {

                        for (int iMask = 0; iMask < m_kMaskVector.size(); iMask++) {
                            final BitSet kMaskSet = (BitSet) m_kMaskVector.elementAt(iMask);

                            if ( (kMaskSet != null) && kMaskSet.get(index)) {

                                if (fReturn == null) {
                                    fReturn = new float[jBound * iBound * 4];
                                }

                                if (m_kColorVector.size() > iMask) {
                                    kMaskColor = ((ColorRGBA) m_kColorVector.elementAt(iMask));
                                }

                                if (m_kMaskColorVector.size() > iMask) {
                                    final ColorRGBA[] kMaskColors = (ColorRGBA[]) m_kMaskColorVector.elementAt(iMask);

                                    if (kMaskColors[index] != null) {
                                        kMaskColor = kMaskColors[index];
                                    }
                                }

                                if (kMaskColor != null) {
                                    fReturn[ ( ( (j * iBound) + i) * 4) + 0] = kMaskColor.A;
                                    fReturn[ ( ( (j * iBound) + i) * 4) + 1] = 255 * kMaskColor.R;
                                    fReturn[ ( ( (j * iBound) + i) * 4) + 2] = 255 * kMaskColor.G;
                                    fReturn[ ( ( (j * iBound) + i) * 4) + 3] = 255 * kMaskColor.B;
                                }
                            }
                        }
                    }

                    /* if color: */
                    if ( (bufferType == DataType.ARGB) || (bufferType == DataType.ARGB_USHORT)
                            || (bufferType == DataType.ARGB_FLOAT)) {
                        values[ ( ( (j * iBound) + i) * 4) + 0] = getFloat( (index * 4) + 0);
                        values[ ( ( (j * iBound) + i) * 4) + 1] = getFloat( (index * 4) + 1);
                        values[ ( ( (j * iBound) + i) * 4) + 2] = getFloat( (index * 4) + 2);
                        values[ ( ( (j * iBound) + i) * 4) + 3] = getFloat( (index * 4) + 3);
                    }
                    /* if complex: */
                    else if ( (bufferType == DataType.COMPLEX) || (bufferType == DataType.DCOMPLEX)) {

                        if (exportComplex) {
                            values[ ( ( (j * iBound) + i) * 2) + 0] = getFloat(index * 2);
                            values[ ( ( (j * iBound) + i) * 2) + 1] = getFloat( (index * 2) + 1);
                        } else {
                            real = getFloat(index * 2);
                            imaginary = getFloat( (index * 2) + 1);

                            if (logMagDisp == true) {
                                mag = Math.sqrt( (real * real) + (imaginary * imaginary));
                                values[ (j * iBound) + i] = (float) (0.4342944819 * Math.log( (1.0 + mag)));
                            } else {
                                values[ (j * iBound) + i] = (float) Math.sqrt( (real * real) + (imaginary * imaginary));
                            }
                        }
                    }
                    /* not color: */
                    else {
                        values[ (j * iBound) + i] = getFloat(index);
                    }
                }
            }
        } catch (final IOException error) {
            throw error;
        } finally {
            releaseLock();
        }

        return fReturn;
    }

    /**
     * Export data to the real values and the images values arrays.
     * 
     * @param start Indicates starting position in data array
     * @param length Length of complex data (in 2 float units) to be copied from data array
     * @param valuesR Array where real data is to be deposited
     * @param valuesI Array where imaginary data is to be deposited
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void exportComplexData(final int start, final int length, final float[] valuesR,
            final float[] valuesI) throws IOException {
        int i, j;

        if ( (start >= 0) && ( (start + (2 * length)) <= dataSize) && (length <= valuesR.length)
                && (length <= valuesI.length)) {

            try {
                setLock(ModelStorageBase.W_LOCKED);

                for (i = start, j = 0; j < length; i += 2, j++) {
                    valuesR[j] = data.getFloat(i);
                    valuesI[j] = data.getFloat(i + 1);
                }

            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Export data error - bounds incorrect");

    }

    /**
     * Export XY slice magnitude data into values array.
     * 
     * @param slice indicates slice of data to be exported
     * @param values array where data is to be deposited
     * @param logMagDisplay if true display log10 of 1 + data
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final void exportComplexSliceXY(final int slice, final float[] values, final boolean logMagDisplay)
            throws IOException {

        int i;
        final int length = dimExtents[0] * dimExtents[1];

        exportMagData(2 * slice * length, length, values);

        if (logMagDisplay) {

            for (i = 0; i < length; i++) {

                // log10(x) = loge(x)/loge(10)
                values[i] = (float) (0.4342944819 * java.lang.Math.log( (1.0 + values[i])));
            }
        }
    }

    /**
     * Export data into values array, in the native type of the model image.
     * 
     * @param start indicates starting position in data array
     * @param length length of data to be copied from data array
     * @return Object, new array where data has been deposited
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final Object exportData(final int start, final int length) throws IOException {
        int i, j;

        Object value_array = null;

        if ( (start >= 0) && ( (start + length) <= dataSize)) {

            try {
                setLock(ModelStorageBase.W_LOCKED);

                final Class<?> buffer_type = data.getType();
                if (buffer_type == null) {
                    return null;
                }

                value_array = Array.newInstance(buffer_type, length);

                for (i = start, j = 0; j < length; i++, j++) {
                    Array.set(value_array, j, data.get(i));
                }
            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return value_array;
        }

        throw new IOException("Export data error - bounds incorrect");
    }

    /**
     * Import Object. Imports Object array that is exported with the exportData function.
     * 
     * @param value_array Object storing data.
     * @throws IOException
     */
    public final void importData(final Object value_array) throws IOException {
        try {
            setLock(ModelStorageBase.W_LOCKED);
            for (int i = 0; i < Math.min(Array.getLength(value_array), dataSize); i++) {
                data.set(i, (Number) Array.get(value_array, i));
            }
        } catch (final IOException error) {
            throw error;
        } finally {
            releaseLock();
        }
    }

    /**
     * Export data into values array.
     * 
     * @param start indicates starting position in data array
     * @param length length of data to be copied from data array
     * @param values array where data is to be deposited
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final void exportData(final int start, final int length, final Number[] values) throws IOException {
        int i, j;

        if ( (start >= 0) && ( (start + length) <= dataSize) && (length <= values.length)) {

            try {
                setLock(ModelStorageBase.W_LOCKED);

                for (i = start, j = 0; j < length; i++, j++) {
                    values[j] = data.get(i);
                }
            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Export data error - bounds incorrect");
    }

    /**
     * Export data into values array.
     * 
     * @param start indicates starting position in data array
     * @param length length of data to be copied from data array
     * @param values array where data is to be deposited
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void exportData(final int start, final int length, final BitSet values)
            throws IOException {
        int i, j;

        if ( (start >= 0) && ( (start + length) <= dataSize) && (length <= values.size())) {

            try {
                setLock(ModelStorageBase.W_LOCKED);

                for (i = start, j = 0; j < length; i++, j++) {

                    if (data.getBoolean(i)) {
                        values.set(j);
                    } else {
                        values.clear(j);
                    }
                }
            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Export data error - bounds incorrect");
    }

    /**
     * Export data into values array.
     * 
     * @param start indicates starting position in data array
     * @param length length of data to be copied from data array
     * @param values array where data is to be deposited
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void exportData(final int start, final int length, final byte[] values)
            throws IOException {
        int i, j;

        if ( (start >= 0) && ( (start + length) <= dataSize) && (length <= values.length)) {

            try {
                setLock(ModelStorageBase.W_LOCKED);

                for (i = start, j = 0; j < length; i++, j++) {
                    values[j] = data.getByte(i);
                }

            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Export data error - bounds incorrect");
    }

    /**
     * Export data into values array.
     * 
     * @param start indicates starting position in data array
     * @param length length of data to be copied from data array
     * @param values array where data is to be deposited
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void exportData(final int start, final int length, final short[] values)
            throws IOException {
        int i, j;

        if ( (start >= 0) && ( (start + length) <= dataSize) && (length <= values.length)) {

            try {
                setLock(ModelStorageBase.W_LOCKED);

                for (i = start, j = 0; j < length; i++, j++) {
                    values[j] = data.getShort(i);
                }

            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Export data error - bounds incorrect");
    }

    /**
     * Export data into values array.
     * 
     * @param start indicates starting position in data array
     * @param length length of data to be copied from data array
     * @param values array where data is to be deposited
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void exportData(final int start, final int length, final int[] values) throws IOException {
        int i, j;

        if ( (start >= 0) && ( (start + length) <= dataSize) && (length <= values.length)) {

            try {
                setLock(ModelStorageBase.W_LOCKED);

                for (i = start, j = 0; j < length; i++, j++) {
                    values[j] = data.getInt(i);
                }

            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Export data error - bounds incorrect");
    }

    /**
     * Export data into values array.
     * 
     * @param start indicates starting position in data array
     * @param length length of data to be copied from data array
     * @param values array where data is to be deposited
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void exportData(final int start, final int length, final long[] values)
            throws IOException {
        int i, j;

        if ( (start >= 0) && ( (start + length) <= dataSize) && (length <= values.length)) {

            try {
                setLock(ModelStorageBase.W_LOCKED);

                for (i = start, j = 0; j < length; i++, j++) {
                    values[j] = data.getLong(i);
                }
            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Export data error - bounds incorrect");
    }

    /**
     * Export data to values array.
     * 
     * @param start indicates starting position in data array
     * @param length length of data to be copied from data array
     * @param values array where data is to be deposited
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void exportData(final int start, final int length, final float[] values)
            throws IOException {
        int i, j;

        if ( (start >= 0) && ( (start + length) <= dataSize) && (length <= values.length)) {

            try {
                setLock(ModelStorageBase.W_LOCKED);

                for (i = start, j = 0; j < length; i++, j++) {
                    values[j] = data.getFloat(i);
                }

            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Export data error - bounds incorrect");

    }

    /**
     * Export data in values array.
     * 
     * @param start indicates starting position in data array
     * @param length length of data to be copied from data array
     * @param values array where data is to be deposited
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void exportData(final int start, final int length, final double[] values)
            throws IOException {
        int i, j;

        if ( (start >= 0) && ( (start + length) <= dataSize) && (length <= values.length)) {

            try {
                setLock(ModelStorageBase.W_LOCKED);

                for (i = start, j = 0; j < length; i++, j++) {
                    values[j] = data.getDouble(i);
                }

            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Export data error - bounds incorrect");
    }

    /**
     * export data to values array.
     * 
     * @param start indicates starting position in data array
     * @param length0 length of first dimension of data to be copied from data array
     * @param length1 length of second dimension of data to be copied from data array
     * @param values array where data is to be deposited
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void exportData(final int start, final int length0, final int length1,
            final float[] values) throws IOException {
        int i, j;
        int x, y;
        final int length = length0 * length1;

        if ( (start >= 0) && ( (start + length) <= dataSize) && (length <= values.length)) {

            try {
                setLock(ModelStorageBase.W_LOCKED);

                for (y = 0, j = 0; y < length1; y++) {

                    for (x = 0; x <= (length0 - 1); x++, j++) {
                        i = start + x + (y * length0);
                        values[j] = data.getFloat(i);
                    }
                }
            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Export data error - bounds incorrect");

    }

    /**
     * export data into values array WITHOUT locking.
     * 
     * @param start indicates starting position in data array
     * @param length length of data to be copied from data array
     * @param values array where data is to be deposited
     * 
     * @throws IOException Throws an error when there is a bounds error.
     */
    public final void exportDataNoLock(final int start, final int length, final Number[] values) throws IOException {
        int i, j;

        if ( (start >= 0) && ( (start + length) <= dataSize) && (length <= values.length)) {

            for (i = start, j = 0; j < length; i++, j++) {
                values[j] = data.get(i);
            }

            return;
        }

        throw new IOException("Export data error - bounds incorrect");
    }

    /**
     * export data into values array WITHOUT locking.
     * 
     * @param start indicates starting position in data array
     * @param length length of data to be copied from data array
     * @param values array where data is to be deposited
     * 
     * @throws IOException Throws an error when there is a bounds error.
     */
    public final synchronized void exportDataNoLock(final int start, final int length, final BitSet values)
            throws IOException {
        int i, j;

        if ( (start >= 0) && ( (start + length) <= dataSize) && (length <= values.size())) {

            for (i = start, j = 0; j < length; i++, j++) {

                if (data.getBoolean(i)) {
                    values.set(j);
                } else {
                    values.clear(j);
                }
            }

            return;
        }

        throw new IOException("Export data error - bounds incorrect");
    }

    /**
     * Export data into values array WITHOUT locking.
     * 
     * @param start Indicates starting position in data array
     * @param length Length of data to be copied from data array
     * @param values Array where data is to be deposited
     * 
     * @throws IOException Throws an error when there is a bounds error.
     */
    public final synchronized void exportDataNoLock(final int start, final int length, final byte[] values)
            throws IOException {
        int i, j;

        if ( (start >= 0) && ( (start + length) <= dataSize) && (length <= values.length)) {

            for (i = start, j = 0; j < length; i++, j++) {
                values[j] = data.getByte(i);
            }

            return;
        }

        throw new IOException("Export data error - bounds incorrect");
    }

    /**
     * Export data into values array WITHOUT locking.
     * 
     * @param start Indicates starting position in data array
     * @param length Length of data to be copied from data array
     * @param values Array where data is to be deposited
     * 
     * @throws IOException Throws an error when there is a bounds error.
     */
    public final synchronized void exportDataNoLock(final int start, final int length, final short[] values)
            throws IOException {
        int i, j;

        if ( (start >= 0) && ( (start + length) <= dataSize) && (length <= values.length)) {

            for (i = start, j = 0; j < length; i++, j++) {
                values[j] = data.getShort(i);
            }

            return;
        }

        throw new IOException("Export data error - bounds incorrect");
    }

    /**
     * Export data into values array WITHOUT locking.
     * 
     * @param start Indicates starting position in data array
     * @param length Length of data to be copied from data array
     * @param values Array where data is to be deposited
     * 
     * @throws IOException Throws an error when there is a bounds error.
     */
    public final synchronized void exportDataNoLock(final int start, final int length, final int[] values)
            throws IOException {
        int i, j;

        if ( (start >= 0) && ( (start + length) <= dataSize) && (length <= values.length)) {

            for (i = start, j = 0; j < length; i++, j++) {
                values[j] = data.getInt(i);
            }

            return;
        }

        throw new IOException("Export data error - bounds incorrect");
    }

    /**
     * export data into values array WITHOUT locking.
     * 
     * @param start indicates starting position in data array
     * @param length length of data to be copied from data array
     * @param values array where data is to be deposited
     * 
     * @throws IOException Throws an error when there is a bounds error.
     */
    public final synchronized void exportDataNoLock(final int start, final int length, final long[] values)
            throws IOException {
        int i, j;

        if ( (start >= 0) && ( (start + length) <= dataSize) && (length <= values.length)) {

            for (i = start, j = 0; j < length; i++, j++) {
                values[j] = data.getLong(i);
            }

            return;
        }

        throw new IOException("Export data error - bounds incorrect");
    }

    /**
     * export data in values array WITHOUT using locking.
     * 
     * @param start indicates starting position in data array
     * @param length length of data to be copied from data array
     * @param values array where data is to be deposited
     * 
     * @throws IOException Throws an error when there is a bounds error.
     */
    public final synchronized void exportDataNoLock(final int start, final int length, final float[] values)
            throws IOException {
        int i, j;

        if ( (start >= 0) && ( (start + length) <= dataSize) && (length <= values.length)) {

            for (i = start, j = 0; j < length; i++, j++) {
                values[j] = data.getFloat(i);
            }

            return;
        }

        throw new IOException("Export data error - bounds incorrect");
    }

    /**
     * export data into values array WITHOUT locking.
     * 
     * @param start indicates starting position in data array
     * @param length length of data to be copied from data array
     * @param values array where data is to be deposited
     * 
     * @throws IOException Throws an error when there is a bounds error.
     */
    public final synchronized void exportDataNoLock(final int start, final int length, final double[] values)
            throws IOException {
        int i, j;

        if ( (start >= 0) && ( (start + length) <= dataSize) && (length <= values.length)) {

            for (i = start, j = 0; j < length; i++, j++) {
                values[j] = data.getDouble(i);
            }

            return;
        }

        throw new IOException("Export data error - bounds incorrect");
    }

    /**
     * Export data to the real values and the imaginary values arrays.
     * 
     * @param start indicates starting position in data array
     * @param length length of complex data (in 2 float units) to be copied from data array
     * @param valuesR array where real data is to be deposited
     * @param valuesI array where imaginary data is to be deposited
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void exportDComplexData(final int start, final int length, final double[] valuesR,
            final double[] valuesI) throws IOException {
        int i, j;

        if ( (start >= 0) && ( (start + (2 * length)) <= dataSize) && (length <= valuesR.length)
                && (length <= valuesI.length)) {

            try {
                setLock(ModelStorageBase.W_LOCKED);

                for (i = start, j = 0; j < length; i += 2, j++) {
                    valuesR[j] = data.getDouble(i);
                    valuesI[j] = data.getDouble(i + 1);
                }
            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Export data error - bounds incorrect");

    }

    /**
     * ShowDiagonal samples the ModelImage data along a non-axis aligned plane. The plane may intersect the ModelImage
     * volume, defined in x,y,z space, along a diagonal direction.
     * 
     * <p>
     * This function steps through the image, using the four transformed points to step through the ModelImage along the
     * diagonal directions, read the corresonding point in the ModelImage and write the value into the image array. If
     * bInterpolate is set to true, the ModelImage data for non-interger vertices is interpolated using tri-linear
     * interpolation. Note: there is one loop for steping though he data, no matter which type of plane this object
     * represents (XY, XZ, or ZY).
     * </p>
     * 
     * @param tSlice Index into the forth dimension
     * @param slice Indicates slice of data to be exported
     * @param extents Image extents in the local coordinate system.
     * @param verts The rotated non-axis aligned corners of the slice
     * @param values The array in which to write the data.
     * @param bInterpolate If true then use interpolation.
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void exportDiagonal(final int tSlice, final int slice, final int[] extents,
            final Vector3f[] verts, final float[] values, final boolean bInterpolate) throws IOException {

        try {
            setLock(ModelStorageBase.W_LOCKED);
        } catch (final IOException error) {
            releaseLock();
            throw error;
        }

        final int iBound = extents[0];
        final int jBound = extents[1];

        /*
         * Get the loop multiplication factors for indexing into the 1D array with 3 index variables: based on the
         * coordinate-systems: transformation:
         */
        final int iFactor = 1;
        final int jFactor = dimExtents[0];
        final int kFactor = dimExtents[0] * dimExtents[1];
        final int tFactor = dimExtents[0] * dimExtents[1] * dimExtents[2];

        int buffFactor = 1;

        if ( (bufferType == DataType.ARGB) || (bufferType == DataType.ARGB_USHORT)
                || (bufferType == DataType.ARGB_FLOAT)) {
            buffFactor = 4;
        }
        
        /* Calculate the slopes for traversing the data in x,y,z: */
        float xSlopeX = verts[1].X - verts[0].X;
        float ySlopeX = verts[1].Y - verts[0].Y;
        float zSlopeX = verts[1].Z - verts[0].Z;

        float xSlopeY = verts[3].X - verts[0].X;
        float ySlopeY = verts[3].Y - verts[0].Y;
        float zSlopeY = verts[3].Z - verts[0].Z;

        float x0 = verts[0].X;
        float y0 = verts[0].Y;
        float z0 = verts[0].Z;

        xSlopeX /= (iBound - 1);
        ySlopeX /= (iBound - 1);
        zSlopeX /= (iBound - 1);

        xSlopeY /= (jBound - 1);
        ySlopeY /= (jBound - 1);
        zSlopeY /= (jBound - 1);

        final boolean exportComplex = (values.length == (2 * iBound * jBound)) ? true : false;
        double real, imaginary, mag;

        /* loop over the 2D image (values) we're writing into */
        float x = x0;
        float y = y0;
        float z = z0;

        for (int j = 0; j < jBound; j++) {

            /* Initialize the first diagonal point(x,y,z): */
            x = x0;
            y = y0;
            z = z0;

            for (int i = 0; i < iBound; i++) {
                final int iIndex = (int) x;
                final int jIndex = (int) y;
                final int kIndex = (int) z;

                /* calculate the ModelImage space index: */
                final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor) + (tSlice * tFactor));

                /* Bounds checking, if out of bounds, set to zero: */
                if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
                        || ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > dataSize))) {

                    if ( (bufferType == DataType.ARGB) || (bufferType == DataType.ARGB_USHORT)
                            || (bufferType == DataType.ARGB_FLOAT)) {
                        values[ ( ( (j * iBound) + i) * 4) + 0] = 0;
                        values[ ( ( (j * iBound) + i) * 4) + 1] = 0;
                        values[ ( ( (j * iBound) + i) * 4) + 2] = 0;
                        values[ ( ( (j * iBound) + i) * 4) + 3] = 0;
                    }
                    /* not color: */
                    else {
                        values[ (j * iBound) + i] = (float) this.min;
                    }
                } else {

                    /* if color: */
                    if ( (bufferType == DataType.ARGB) || (bufferType == DataType.ARGB_USHORT)
                            || (bufferType == DataType.ARGB_FLOAT)) {
                        values[ ( ( (j * iBound) + i) * 4) + 0] = getFloat( (index * 4) + 0);
                        values[ ( ( (j * iBound) + i) * 4) + 1] = getFloat( (index * 4) + 1);
                        values[ ( ( (j * iBound) + i) * 4) + 2] = getFloat( (index * 4) + 2);
                        values[ ( ( (j * iBound) + i) * 4) + 3] = getFloat( (index * 4) + 3);
                    }
                    /* if complex: */
                    else if (bufferType == DataType.COMPLEX) {

                        if (exportComplex) {
                            values[ ( ( (j * iBound) + i) * 2) + 0] = getFloat(index * 2);
                            values[ ( ( (j * iBound) + i) * 2) + 1] = getFloat( (index * 2) + 1);
                        } else {
                            real = getFloat(index * 2);
                            imaginary = getFloat( (index * 2) + 1);

                            if (logMagDisp == true) {
                                mag = Math.sqrt( (real * real) + (imaginary * imaginary));
                                values[ (j * iBound) + i] = (float) (0.4342944819 * Math.log( (1.0 + mag)));
                            } else {
                                values[ (j * iBound) + i] = (float) Math.sqrt( (real * real) + (imaginary * imaginary));
                            }
                        }
                    }
                    /* not color: */
                    else {

                        if (bInterpolate) {
                            values[ (j * iBound) + i] = getFloatTriLinearBounds(x, y, z);
                        } else {
                            values[ (j * iBound) + i] = getFloat(index);
                        }
                    }
                }

                /*
                 * Inner loop: Move to the next diagonal point along the x-direction of the plane, using the xSlopeX,
                 * ySlopeX and zSlopeX values:
                 */
                x = x + xSlopeX;
                y = y + ySlopeX;
                z = z + zSlopeX;
            }

            /*
             * Outer loop: Move to the next diagonal point along the y-direction of the plane, using the xSlopeY,
             * ySlopeY and zSlopeY values:
             */
            x0 = x0 + xSlopeY;
            y0 = y0 + ySlopeY;
            z0 = z0 + zSlopeY;
        }
    }



    public final synchronized void exportDiagonal(final int tSlice, final int slice, final int[] extents,
            final Vector3f[] verts, final float[] values, final boolean bInterpolate, final BitSet imageMask,
            Vector<BitSet> surfaceMask, boolean[] surfaceValues) throws IOException {

        try {
            setLock(ModelStorageBase.W_LOCKED);
        } catch (final IOException error) {
            releaseLock();
            throw error;
        }

        final int iBound = extents[0];
        final int jBound = extents[1];

        /*
         * Get the loop multiplication factors for indexing into the 1D array with 3 index variables: based on the
         * coordinate-systems: transformation:
         */
        final int iFactor = 1;
        final int jFactor = dimExtents[0];
        final int kFactor = dimExtents[0] * dimExtents[1];
        final int tFactor = dimExtents[0] * dimExtents[1] * dimExtents[2];

        int buffFactor = 1;

        if ( (bufferType == DataType.ARGB) || (bufferType == DataType.ARGB_USHORT)
                || (bufferType == DataType.ARGB_FLOAT)) {
            buffFactor = 4;
        }
        
        /* Calculate the slopes for traversing the data in x,y,z: */
        float xSlopeX = verts[1].X - verts[0].X;
        float ySlopeX = verts[1].Y - verts[0].Y;
        float zSlopeX = verts[1].Z - verts[0].Z;

        float xSlopeY = verts[3].X - verts[0].X;
        float ySlopeY = verts[3].Y - verts[0].Y;
        float zSlopeY = verts[3].Z - verts[0].Z;

        float x0 = verts[0].X;
        float y0 = verts[0].Y;
        float z0 = verts[0].Z;

        xSlopeX /= (iBound - 1);
        ySlopeX /= (iBound - 1);
        zSlopeX /= (iBound - 1);

        xSlopeY /= (jBound - 1);
        ySlopeY /= (jBound - 1);
        zSlopeY /= (jBound - 1);

        final boolean exportComplex = (values.length == (2 * iBound * jBound)) ? true : false;
        double real, imaginary, mag;

        /* loop over the 2D image (values) we're writing into */
        float x = x0;
        float y = y0;
        float z = z0;

        for (int j = 0; j < jBound; j++) {

            /* Initialize the first diagonal point(x,y,z): */
            x = x0;
            y = y0;
            z = z0;

            for (int i = 0; i < iBound; i++) {
                final int iIndex = (int) x;
                final int jIndex = (int) y;
                final int kIndex = (int) z;

                /* calculate the ModelImage space index: */
                final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor) + (tSlice * tFactor));

                /* Bounds checking, if out of bounds, set to zero: */
                if (( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
                        || ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > dataSize))) {

                    if ( (bufferType == DataType.ARGB) || (bufferType == DataType.ARGB_USHORT)
                            || (bufferType == DataType.ARGB_FLOAT)) {
                        values[ ( ( (j * iBound) + i) * 4) + 0] = 0;
                        values[ ( ( (j * iBound) + i) * 4) + 1] = 0;
                        values[ ( ( (j * iBound) + i) * 4) + 2] = 0;
                        values[ ( ( (j * iBound) + i) * 4) + 3] = 0;
                    }
                    /* not color: */
                    else {
                        values[ (j * iBound) + i] = (float) this.min;
                    }
                    if ( surfaceValues != null )
                    {
                    	surfaceValues[ (j * iBound) + i] = false;
                    }
                } 
                else if ( (imageMask != null) && !imageMask.get(index) )
                {
                    if ( (bufferType == DataType.ARGB) || (bufferType == DataType.ARGB_USHORT)
                            || (bufferType == DataType.ARGB_FLOAT)) {
                        values[ ( ( (j * iBound) + i) * 4) + 0] = 0;
                        values[ ( ( (j * iBound) + i) * 4) + 1] = 0;
                        values[ ( ( (j * iBound) + i) * 4) + 2] = 0;
                        values[ ( ( (j * iBound) + i) * 4) + 3] = 0;
                    }
                    /* not color: */
                    else {
                        values[ (j * iBound) + i] = (float) this.min;
                    }   
                    if ( surfaceValues != null )
                    {
                    	surfaceValues[ (j * iBound) + i] = false;
                    }             	
                }                
                else {

                    /* if color: */
                    if ( (bufferType == DataType.ARGB) || (bufferType == DataType.ARGB_USHORT)
                            || (bufferType == DataType.ARGB_FLOAT)) {
                        values[ ( ( (j * iBound) + i) * 4) + 0] = getFloat( (index * 4) + 0);
                        values[ ( ( (j * iBound) + i) * 4) + 1] = getFloat( (index * 4) + 1);
                        values[ ( ( (j * iBound) + i) * 4) + 2] = getFloat( (index * 4) + 2);
                        values[ ( ( (j * iBound) + i) * 4) + 3] = getFloat( (index * 4) + 3);
                    }
                    /* if complex: */
                    else if (bufferType == DataType.COMPLEX) {

                        if (exportComplex) {
                            values[ ( ( (j * iBound) + i) * 2) + 0] = getFloat(index * 2);
                            values[ ( ( (j * iBound) + i) * 2) + 1] = getFloat( (index * 2) + 1);
                        } else {
                            real = getFloat(index * 2);
                            imaginary = getFloat( (index * 2) + 1);

                            if (logMagDisp == true) {
                                mag = Math.sqrt( (real * real) + (imaginary * imaginary));
                                values[ (j * iBound) + i] = (float) (0.4342944819 * Math.log( (1.0 + mag)));
                            } else {
                                values[ (j * iBound) + i] = (float) Math.sqrt( (real * real) + (imaginary * imaginary));
                            }
                        }
                    }
                    /* not color: */
                    else {

                        if (bInterpolate) {
                            values[ (j * iBound) + i] = getFloatTriLinearBounds(x, y, z);
                        } else {
                            values[ (j * iBound) + i] = getFloat(index);
                        }
                    }
                    if ( surfaceValues != null && surfaceMask != null )
                    {
                    	boolean isSurface = false;
                    	for ( int surface = 0; surface < surfaceMask.size(); surface++ )
                    	{
                    		isSurface |= surfaceMask.elementAt(surface).get(index);
                    	}
                    	surfaceValues[ (j * iBound) + i] = isSurface;
                    }
                }

                /*
                 * Inner loop: Move to the next diagonal point along the x-direction of the plane, using the xSlopeX,
                 * ySlopeX and zSlopeX values:
                 */
                x = x + xSlopeX;
                y = y + ySlopeX;
                z = z + zSlopeX;
            }

            /*
             * Outer loop: Move to the next diagonal point along the y-direction of the plane, using the xSlopeY,
             * ySlopeY and zSlopeY values:
             */
            x0 = x0 + xSlopeY;
            y0 = y0 + ySlopeY;
            z0 = z0 + zSlopeY;
        }
    }



    /**
     * ShowDiagonal samples the ModelImage data along a non-axis aligned plane. The plane may intersect the ModelImage
     * volume, defined in x,y,z space, along a diagonal direction.
     * 
     * <p>
     * This function steps through the image, using the four transformed points to step through the ModelImage along the
     * diagonal directions, read the corresonding point in the ModelImage and write the value into the image array. If
     * bInterpolate is set to true, the ModelImage data for non-interger vertices is interpolated using tri-linear
     * interpolation. Note: there is one loop for steping though he data, no matter which type of plane this object
     * represents (XY, XZ, or ZY).
     * </p>
     * 
     * @param tSlice Index into the forth dimension
     * @param slice Indicates slice of data to be exported
     * @param extents Image extents in the local coordinate system.
     * @param verts The rotated non-axis aligned corners of the slice
     * @param values The array in which to write the data.
     * @param bInterpolate If true then use interpolation.
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void exportDiagonal( BitSet duplicateMask, final int tSlice, final int slice, final int[] extents,
            final Vector3f[] verts, final float diameter, boolean bSetZero, final float[] values, final boolean bInterpolate) throws IOException {

        try {
            setLock(ModelStorageBase.W_LOCKED);
        } catch (final IOException error) {
            releaseLock();
            throw error;
        }

        final int iBound = extents[0];
        final int jBound = extents[1];

        /*
         * Get the loop multiplication factors for indexing into the 1D array with 3 index variables: based on the
         * coordinate-systems: transformation:
         */
        final int iFactor = 1;
        final int jFactor = dimExtents[0];
        final int kFactor = dimExtents[0] * dimExtents[1];
        final int tFactor = dimExtents[0] * dimExtents[1] * dimExtents[2];

        int buffFactor = 1;

        if ( (bufferType == DataType.ARGB) || (bufferType == DataType.ARGB_USHORT)
                || (bufferType == DataType.ARGB_FLOAT)) {
            buffFactor = 4;
        }
        
        Vector3f center = new Vector3f();
        for ( int i = 0; i < verts.length; i++ )
        {
        	center.add(verts[i]);
        }
        center.scale( 1f/(float)verts.length );
        
        /* Calculate the slopes for traversing the data in x,y,z: */
        float xSlopeX = verts[1].X - verts[0].X;
        float ySlopeX = verts[1].Y - verts[0].Y;
        float zSlopeX = verts[1].Z - verts[0].Z;

        float xSlopeY = verts[3].X - verts[0].X;
        float ySlopeY = verts[3].Y - verts[0].Y;
        float zSlopeY = verts[3].Z - verts[0].Z;

        float x0 = verts[0].X;
        float y0 = verts[0].Y;
        float z0 = verts[0].Z;

        xSlopeX /= (iBound - 1);
        ySlopeX /= (iBound - 1);
        zSlopeX /= (iBound - 1);

        xSlopeY /= (jBound - 1);
        ySlopeY /= (jBound - 1);
        zSlopeY /= (jBound - 1);

        final boolean exportComplex = (values.length == (2 * iBound * jBound)) ? true : false;
        double real, imaginary, mag;

        /* loop over the 2D image (values) we're writing into */
        float x = x0;
        float y = y0;
        float z = z0;
        
        Vector3f currentPoint = new Vector3f();

        Vector<Integer> maskBits = new Vector<Integer>();
        for (int j = 0; j < jBound; j++) {

            /* Initialize the first diagonal point(x,y,z): */
            x = x0;
            y = y0;
            z = z0;

            for (int i = 0; i < iBound; i++) {
                final int iIndex = (int) Math.round(x);
                final int jIndex = (int) Math.round(y);
                final int kIndex = (int) Math.round(z);

                /* calculate the ModelImage space index: */
                final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor) + (tSlice * tFactor));

                currentPoint.set(x, y, z);
                float distance = center.distance(currentPoint);
                
                /* Bounds checking, if out of bounds, set to zero: */
                if ( (distance > diameter ) ||
                		( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
                		|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > dataSize))) {

                	if ( (bufferType == DataType.ARGB) || (bufferType == DataType.ARGB_USHORT)
                			|| (bufferType == DataType.ARGB_FLOAT)) {
                		values[ ( ( (j * iBound) + i) * 4) + 0] = 0;
                		values[ ( ( (j * iBound) + i) * 4) + 1] = 0;
                		values[ ( ( (j * iBound) + i) * 4) + 2] = 0;
                		values[ ( ( (j * iBound) + i) * 4) + 3] = 0;
                	}
                	/* not color: */
                	else {
                		values[ (j * iBound) + i] = (float) this.min;
                	}
                } else {
                	if ( (duplicateMask != null) && duplicateMask.get(index) )
                	{
                		if ( bSetZero )
                		{
                			// set output to zero or min:
                			if ( (bufferType == DataType.ARGB) || (bufferType == DataType.ARGB_USHORT)
                					|| (bufferType == DataType.ARGB_FLOAT)) {
                				values[ ( ( (j * iBound) + i) * 4) + 0] = 0;
                				values[ ( ( (j * iBound) + i) * 4) + 1] = 0;
                				values[ ( ( (j * iBound) + i) * 4) + 2] = 0;
                				values[ ( ( (j * iBound) + i) * 4) + 3] = 0;
                			}
                			/* not color: */
                			else {
                				values[ (j * iBound) + i] = (float) this.min;
                			}
                		}
                	}                	
                	else
                	{
                		maskBits.add(index);
                		/* if color: */
                		if ( (bufferType == DataType.ARGB) || (bufferType == DataType.ARGB_USHORT)
                				|| (bufferType == DataType.ARGB_FLOAT)) {
                			values[ ( ( (j * iBound) + i) * 4) + 0] = getFloat( (index * 4) + 0);
                			values[ ( ( (j * iBound) + i) * 4) + 1] = getFloat( (index * 4) + 1);
                			values[ ( ( (j * iBound) + i) * 4) + 2] = getFloat( (index * 4) + 2);
                			values[ ( ( (j * iBound) + i) * 4) + 3] = getFloat( (index * 4) + 3);
                		}
                		/* if complex: */
                		else if (bufferType == DataType.COMPLEX) {

                			if (exportComplex) {
                				values[ ( ( (j * iBound) + i) * 2) + 0] = getFloat(index * 2);
                				values[ ( ( (j * iBound) + i) * 2) + 1] = getFloat( (index * 2) + 1);
                			} else {
                				real = getFloat(index * 2);
                				imaginary = getFloat( (index * 2) + 1);

                				if (logMagDisp == true) {
                					mag = Math.sqrt( (real * real) + (imaginary * imaginary));
                					values[ (j * iBound) + i] = (float) (0.4342944819 * Math.log( (1.0 + mag)));
                				} else {
                					values[ (j * iBound) + i] = (float) Math.sqrt( (real * real) + (imaginary * imaginary));
                				}
                			}
                		}
                		/* not color: */
                		else {

                			if (bInterpolate) {
                				values[ (j * iBound) + i] = getFloatTriLinearBounds(x, y, z);
                			} else {
                				values[ (j * iBound) + i] = getFloat(index);
                			}
                		}
                	}
                }

                /*
                 * Inner loop: Move to the next diagonal point along the x-direction of the plane, using the xSlopeX,
                 * ySlopeX and zSlopeX values:
                 */
                x = x + xSlopeX;
                y = y + ySlopeX;
                z = z + zSlopeX;
            }

            /*
             * Outer loop: Move to the next diagonal point along the y-direction of the plane, using the xSlopeY,
             * ySlopeY and zSlopeY values:
             */
            x0 = x0 + xSlopeY;
            y0 = y0 + ySlopeY;
            z0 = z0 + zSlopeY;
        }
        
        if ( duplicateMask != null )
        {
        	for ( int i = 0; i < maskBits.size(); i++ )
        	{
        		duplicateMask.set(maskBits.elementAt(i));
        	}
        }
        maskBits.clear();
        maskBits = null;
    }

    /**
     */
    public final synchronized Vector3f findMax( final int tSlice, final int slice,
            final Vector3f[] verts, final float diameter, final boolean bInterpolate) throws IOException {

        try {
            setLock(ModelStorageBase.W_LOCKED);
        } catch (final IOException error) {
            releaseLock();
            throw error;
        }

        final int iBound = (int)(Math.ceil(verts[1].X - verts[0].X));
        final int jBound = (int)(Math.ceil(verts[3].Y - verts[0].Y));

        /*
         * Get the loop multiplication factors for indexing into the 1D array with 3 index variables: based on the
         * coordinate-systems: transformation:
         */
        final int iFactor = 1;
        final int jFactor = dimExtents[0];
        final int kFactor = dimExtents[0] * dimExtents[1];
        final int tFactor = dimExtents[0] * dimExtents[1] * dimExtents[2];

        int buffFactor = 1;

        if ( (bufferType == DataType.ARGB) || (bufferType == DataType.ARGB_USHORT)
                || (bufferType == DataType.ARGB_FLOAT)) {
            buffFactor = 4;
        }
        
        Vector3f center = new Vector3f();
        for ( int i = 0; i < verts.length; i++ )
        {
        	center.add(verts[i]);
        }
        center.scale( 1f/(float)verts.length );
        
        /* Calculate the slopes for traversing the data in x,y,z: */
        float xSlopeX = verts[1].X - verts[0].X;
        float ySlopeX = verts[1].Y - verts[0].Y;
        float zSlopeX = verts[1].Z - verts[0].Z;

        float xSlopeY = verts[3].X - verts[0].X;
        float ySlopeY = verts[3].Y - verts[0].Y;
        float zSlopeY = verts[3].Z - verts[0].Z;

        float x0 = verts[0].X;
        float y0 = verts[0].Y;
        float z0 = verts[0].Z;

        xSlopeX /= (iBound - 1);
        ySlopeX /= (iBound - 1);
        zSlopeX /= (iBound - 1);

        xSlopeY /= (jBound - 1);
        ySlopeY /= (jBound - 1);
        zSlopeY /= (jBound - 1);

        /* loop over the 2D image (values) we're writing into */
        float x = x0;
        float y = y0;
        float z = z0;
        
        Vector3f currentPoint = new Vector3f();

        float maxValue = -Float.MAX_VALUE;
        float maxX = -1, maxY = -1, maxZ = -1;
        for (int j = 0; j < jBound; j++) {

            /* Initialize the first diagonal point(x,y,z): */
            x = x0;
            y = y0;
            z = z0;

            for (int i = 0; i < iBound; i++) {
                final int iIndex = (int) Math.round(x);
                final int jIndex = (int) Math.round(y);
                final int kIndex = (int) Math.round(z);

                /* calculate the ModelImage space index: */
                final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor) + (tSlice * tFactor));

                currentPoint.set(x, y, z);
                float distance = center.distance(currentPoint);
                
                /* Bounds checking, if out of bounds, set to zero: */
                if ( (distance > diameter ) ||
                		( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
                		|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > dataSize))) {

                } else {
                	float val;
                	if (bInterpolate) {
                		val = getFloatTriLinearBounds(x, y, z);
                	} else {
                		val = getFloat(index);
                	}
                	if ( val > maxValue )
                	{
                		maxValue = val;
                		maxX = x;
                		maxY = y;
                		maxZ = z;
                	}
                }

                /*
                 * Inner loop: Move to the next diagonal point along the x-direction of the plane, using the xSlopeX,
                 * ySlopeX and zSlopeX values:
                 */
                x = x + xSlopeX;
                y = y + ySlopeX;
                z = z + zSlopeX;
            }

            /*
             * Outer loop: Move to the next diagonal point along the y-direction of the plane, using the xSlopeY,
             * ySlopeY and zSlopeY values:
             */
            x0 = x0 + xSlopeY;
            y0 = y0 + ySlopeY;
            z0 = z0 + zSlopeY;
        }
        if ( maxX != -1 && maxY != -1 && maxZ != -1 )
        {
        	return new Vector3f( Math.round(maxX), Math.round(maxY), Math.round(maxZ) );
        }
        return null;
    }

    
    /**
     * Export magnitude data to values array.
     * 
     * @param start indicates starting position in data array
     * @param length length of magnitude data to be copied from data array
     * @param values array where magnitude data is to be deposited
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void exportDMagData(final int start, final int length, final double[] values)
            throws IOException {
        int i, j;
        double real, imaginary;

        if ( (start >= 0) && ( (start + (2 * length)) <= dataSize) && (length <= values.length)) {

            try {
                setLock(ModelStorageBase.W_LOCKED);

                for (i = start, j = 0; j < length; i += 2, j++) {
                    real = data.getDouble(i);
                    imaginary = data.getDouble(i + 1);
                    values[j] = Math.sqrt( (real * real) + (imaginary * imaginary));
                }
            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Export data error - bounds incorrect");
    }

    /**
     * export magnitude data to values array.
     * 
     * @param start indicates starting position in data array
     * @param length length of magnitude data to be copied from data array
     * @param values array where magnitude data is to be deposited
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void exportMagData(final int start, final int length, final float[] values)
            throws IOException {
        int i, j;
        double real, imaginary;

        if ( (start >= 0) && ( (start + (2 * length)) <= dataSize) && (length <= values.length)) {

            try {
                setLock(ModelStorageBase.W_LOCKED);

                for (i = start, j = 0; j < length; i += 2, j++) {
                    real = data.getFloat(i);
                    imaginary = data.getFloat(i + 1);
                    values[j] = (float) Math.sqrt( (real * real) + (imaginary * imaginary));
                }

            } catch (final IOException error) {
                releaseLock();
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Export data error - bounds incorrect");
    }
    
    /**
     * export magnitude data to values array.
     * 
     * @param start indicates starting position in data array
     * @param length length of magnitude data to be copied from data array
     * @param values array where magnitude data is to be deposited
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void exportMagData(final int start, final int length, final double[] values)
            throws IOException {
        int i, j;
        double real, imaginary;

        if ( (start >= 0) && ( (start + (2 * length)) <= dataSize) && (length <= values.length)) {

            try {
                setLock(ModelStorageBase.W_LOCKED);

                for (i = start, j = 0; j < length; i += 2, j++) {
                    real = data.getDouble(i);
                    imaginary = data.getDouble(i + 1);
                    values[j] = Math.sqrt( (real * real) + (imaginary * imaginary));
                }

            } catch (final IOException error) {
                releaseLock();
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Export data error - bounds incorrect");
    }

    /**
     * export data in values array.
     * 
     * @param offset correct offset for RED, GREEN, or BLUE component to be exported
     * @param start indicates starting position in data array
     * @param length length of data to be copied from data array
     * @return Object, new array where data has been deposited
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized Object exportRGBData(final int offset, final int start, final int length)
            throws IOException {
        int i, j;

        Object value_array = null;

        if ( (start >= 0) && ( (start + (4 * length)) <= dataSize)) {

            try {
                setLock(ModelStorageBase.W_LOCKED);

                final Class<?> buffer_type = data.getType();
                if (buffer_type == null) {
                    return null;
                }

                value_array = Array.newInstance(buffer_type, length);

                for (i = start + offset, j = 0; j < length; i += 4, j++) {
                    Array.set(value_array, j, data.get(i));
                }
            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return value_array;
        }

        throw new IOException("Export RGB data error - bounds incorrect");
    }

    /**
     * export data in values array.
     * 
     * @param offset correct offset for RED, GREEN, or BLUE component to be exported
     * @param start indicates starting position in data array
     * @param length length of data to be copied from data array
     * @param values array where data is to be deposited
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void exportRGBData(final int offset, final int start, final int length,
            final byte[] values) throws IOException {
        int i, j;

        if ( (start >= 0) && ( (start + (4 * length)) <= dataSize) && (length <= values.length)) {

            try {
                setLock(ModelStorageBase.W_LOCKED);

                for (i = start + offset, j = 0; j < length; i += 4, j++) {
                    values[j] = data.getByte(i);
                }
            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Export RGB data error - bounds incorrect");
    }

    /**
     * export data in values array.
     * 
     * @param offset correct offset for RED, GREEN, or BLUE component to be exported
     * @param start indicates starting position in data array
     * @param length length of data to be copied from data array
     * @param values array where data is to be deposited
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void exportRGBData(final int offset, final int start, final int length,
            final short[] values) throws IOException {
        int i, j;

        if ( (start >= 0) && ( (start + (4 * length)) <= dataSize) && (length <= values.length)) {

            try {
                setLock(ModelStorageBase.W_LOCKED);

                for (i = start + offset, j = 0; j < length; i += 4, j++) {
                    values[j] = data.getShort(i);
                }

            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Export RGB data error - bounds incorrect");
    }

    /**
     * export data in values array.
     * 
     * @param offset correct offset for RED, GREEN, or BLUE component to be exported
     * @param start indicates starting position in data array
     * @param length length of data to be copied from data array
     * @param values array where data is to be deposited
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void exportRGBData(final int offset, final int start, final int length,
            final float[] values) throws IOException {
        int i, j;

        if ( (start >= 0) && ( (start + (4 * length)) <= dataSize) && (length <= values.length)) {

            try {
                setLock(ModelStorageBase.W_LOCKED);

                for (i = start + offset, j = 0; j < length; i += 4, j++) {
                    values[j] = data.getFloat(i);
                }
            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Export RGB data error - bounds incorrect");
    }

    /**
     * Export data in values array WITHOUT using locking.
     * 
     * @param offset offset into the data array
     * @param start indicates starting position in data array
     * @param length length of data to be copied from data array
     * @param values array where data is to be deposited
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void exportRGBDataNoLock(final int offset, final int start, final int length,
            final float[] values) throws IOException {
        int i, j;

        if ( (start >= 0) && ( (start + (4 * length)) <= dataSize) && (length <= values.length)) {

            for (i = start, j = 0; j < length; i += 4, j++) {
                values[j] = data.getFloat(i + offset);
            }

            return;
        }

        throw new IOException("Export RGB data error - bounds incorrect");
    }
    
    /**
     * Export data in values array WITHOUT using locking.
     * 
     * @param offset offset into the data array
     * @param start indicates starting position in data array
     * @param length length of data to be copied from data array
     * @param values array where data is to be deposited
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void exportRGBDataNoLock(final int offset, final int start, final int length,
            final double[] values) throws IOException {
        int i, j;

        if ( (start >= 0) && ( (start + (4 * length)) <= dataSize) && (length <= values.length)) {

            for (i = start, j = 0; j < length; i += 4, j++) {
                values[j] = data.getDouble(i + offset);
            }

            return;
        }

        throw new IOException("Export RGB data error - bounds incorrect");
    }

    /**
     * Export XY slice into values array.
     * 
     * @param slice Indicates slice of data to be exported
     * @param values array where data is to be deposited
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final void exportSliceXY(final int slice, final byte[] values) throws IOException {
        final int length = dimExtents[0] * dimExtents[1];

        exportData(slice * length, length, values);
    }

    /**
     * Export XY slice into values array.
     * 
     * @param slice Indicates slice of data to be exported.
     * @param values Array where data is to be deposited.
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final void exportSliceXY(final int slice, final short[] values) throws IOException {
        final int length = dimExtents[0] * dimExtents[1];

        exportData(slice * length, length, values);
    }

    /**
     * Export XY slice into values array.
     * 
     * @param slice Indicates slice of data to be exported.
     * @param values Array where data is to be deposited.
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final void exportSliceXY(final int slice, final int[] values) throws IOException {
        final int length = dimExtents[0] * dimExtents[1];

        exportData(slice * length, length, values);
    }

    /**
     * export XY slice into values array.
     * 
     * @param slice indicates slice of data to be exported
     * @param values array where data is to be deposited
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final void exportSliceXY(final int slice, final long[] values) throws IOException {
        final int length = dimExtents[0] * dimExtents[1];

        exportData(slice * length, length, values);
    }

    /**
     * export XY slice into values array.
     * 
     * @param slice indicates slice of data to be exported
     * @param values array where data is to be deposited
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final void exportSliceXY(final int slice, final float[] values) throws IOException {
        final int length = dimExtents[0] * dimExtents[1];

        exportData(slice * length, length, values);
    }

    /**
     * export XY slice into values array.
     * 
     * @param slice indicates slice of data to be exported
     * @param values array where data is to be deposited
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final void exportSliceXY(final int slice, final double[] values) throws IOException {
        final int length = dimExtents[0] * dimExtents[1];

        exportData(slice * length, length, values);
    }

    /**
     * function to get data where bounds checking is performed.
     * 
     * @param position position in one dimensional array
     * 
     * @return returns value if position in data array range
     */
    public final Number get(final int position) {

        if ( (position >= 0) && (position < dataSize)) {
            return (data.get(position));
        }

        return (new Byte((byte) 0));
    }

    /**
     * nD get data fuction where bounds checking is performed.
     * 
     * @param position array of coordinate values
     * 
     * @return returns true if position in data array range
     */
    public final Number get(final int[] position) {
        int i;
        int location = 0;
        final int dimensions = position.length;

        if (nDims == dimensions) {

            for (i = dimensions - 1; i > 0; i--) {
                location += (position[i] * dimExtents[i]);
            }

            location += position[0];

            if ( (location >= 0) && (location < dataSize)) {
                return data.get(location);
            }

            return (new Byte((byte) 0));
        }

        return (new Byte((byte) 0));
    }

    /**
     * 2D get data fuction where bounds checking is performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * 
     * @return returns value if position in data array range
     */
    public final Number get(final int x, final int y) {
        int position;

        if (nDims == 2) {
            position = (y * dimExtents[0]) + x;

            if ( (position >= 0) && (position < dataSize)) {
                return data.get(position);
            }

            return (new Byte((byte) 0));
        }

        return (new Byte((byte) 0));
    }

    /**
     * 3D get data fuction where bounds checking is performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * 
     * @return returns value if position in data array range
     */
    public final Number get(final int x, final int y, final int z) {
        int position;

        if (nDims == 3) {
            position = (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x;

            if ( (position >= 0) && (position < dataSize)) {
                return data.get(position);
            }

            return (new Byte((byte) 0));
        }

        return (new Byte((byte) 0));
    }

    /**
     * 4D get data fuction where bounds checking is performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param b b coordinate (ie. multi-modality images or time)
     * 
     * @return returns true if position in data array range
     */
    public final Number get(final int x, final int y, final int z, final int b) {
        int position;

        if (nDims == 4) {
            position = (b * (dimExtents[0] * dimExtents[1] * dimExtents[2])) + (z * (dimExtents[0] * dimExtents[1]))
                    + (y * dimExtents[0]) + x;

            if ( (position >= 0) && (position < dataSize)) {
                return data.get(position);
            }

            return (new Byte((byte) 0));
        }

        return (new Byte((byte) 0));
    }

    /**
     * Returns the axis orientation of image.
     * 
     * @return the axis orientation of image
     */
    public int[] getAxisOrientation() {

        if (fileInfo == null) {
            return null;
        }

        return fileInfo[0].getAxisOrientation();
    }

    /**
     * version of get that performs bi-linear interpoloation. Note - does perform bounds checking
     * 
     * @param x x coordinate
     * @param y y coordinate
     * 
     * @return The Number of the bilinearly interpolated data.
     */
    public final Number getBiLinear(final float x, final float y) {

        final int xDim = dimExtents[0];
        int position;
        float dx, dy;
        float x1, x2;
        int intX, intY;

        intX = (int) x;
        intY = (int) y;

        dx = x - intX;
        dy = y - intY;
        position = (intY * xDim) + intX;

        if ( (position >= 0) && (position < (dataSize - xDim - 1))) {
            x1 = ( (1 - dx) * (data.get(position)).floatValue()) + (dx * (data.get(position + 1)).floatValue());

            x2 = ( (1 - dx) * (data.get(position + xDim)).floatValue())
                    + (dx * (data.get(position + xDim + 1)).floatValue());

            return (new Float( ( (1 - dy) * x1) + (dy * x2)));
        } else {
            return (new Byte((byte) 0));
        }
    }

    /**
     * Version of get that does NOT perform bounds checking.
     * 
     * @param position index into the data.
     * 
     * @return The boolean value from the data array.
     */
    public final boolean getBoolean(final int position) {
        return (data.getBoolean(position));
    }

    /**
     * n-Dimensional get data fuction where bounds checking is NOT performed.
     * 
     * @param position index into the data
     * 
     * @return returns true if position in data array range
     */
    public final boolean getBoolean(final int[] position) {
        int i;
        int location = 0;
        final int dimensions = position.length;

        for (i = dimensions - 1; i > 0; i--) {
            location += (position[i] * dimExtents[i]);
        }

        location += position[0];

        return (data.getBoolean(location));
    }

    /**
     * 2D get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * 
     * @return value
     */
    public final boolean getBoolean(final int x, final int y) {
        return (data.getBoolean( (y * dimExtents[0]) + x));
    }

    /**
     * 3D get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate*
     * 
     * @return The value at that position in the data array.
     */
    public final boolean getBoolean(final int x, final int y, final int z) {
        return (data.getBoolean( (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x));
    }

    /**
     * 4D get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param b 4th dimension coordinate.*
     * 
     * @return The value at that position in the data array.
     */
    public final boolean getBoolean(final int x, final int y, final int z, final int b) {
        return (data.getBoolean( (b * (dimExtents[0] * dimExtents[1] * dimExtents[2]))
                + (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x));
    }

    /**
     * Version of get that does NOT perform bounds checking
     * 
     * @param position position in one dimensional array.
     * 
     * @param position Indexe into the data array.
     * 
     * @return The value at that position in the data array.
     */
    public final byte getByte(final int position) {
        return (data.getByte(position));
    }

    /**
     * nD get data fuction where bounds checking is NOT performed.
     * 
     * @param position array of coordinate values
     * 
     * @return returns true if position in data array range
     */
    public final byte getByte(final int[] position) {
        int i;
        int location = 0;
        final int dimensions = position.length;

        for (i = dimensions - 1; i > 0; i--) {
            location += (position[i] * dimExtents[i]);
        }

        location += position[0];

        return (data.getByte(location));
    }

    /**
     * 2D get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * 
     * @return value at that position in the data array
     */
    public final byte getByte(final int x, final int y) {
        return (data.getByte( (y * dimExtents[0]) + x));
    }

    /**
     * 3D get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * 
     * @return value at that position in the data array
     */
    public final byte getByte(final int x, final int y, final int z) {
        return (data.getByte( (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x));
    }

    /**
     * 4D get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate (usually the slice plane index)
     * @param t t coordinate (usually the volume index)
     * 
     * @return value at that position in the data array
     */
    public final byte getByte(final int x, final int y, final int z, final int t) {
        return (data.getByte( (t * (dimExtents[0] * dimExtents[1] * dimExtents[2]))
                + (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x));
    }

    /**
     * version of get that performs bi-linear interpoloation. Note - does NOT perform bounds checking
     * 
     * @param x x coordinate
     * @param y y coordinate
     * 
     * @return The interpolated value from the data array.
     */
    public final byte getByteBiLinear(final float x, final float y) {

        final int xDim = dimExtents[0];
        int position;
        int intX, intY;
        float dx, dy;
        float x1, x2;

        intX = (int) x;
        intY = (int) y;

        dx = x - intX;
        dy = y - intY;
        position = (intY * xDim) + intX;

        // if ((position >= 0) && (position < size-xDim-1) ) {
        x1 = ( (1 - dx) * data.getByte(position)) + (dx * data.getByte(position + 1));
        x2 = ( (1 - dx) * data.getByte(position + xDim)) + (dx * data.getByte(position + xDim + 1));

        return (byte) ( ( (1 - dy) * x1) + (dy * x2));
    }

    /**
     * version of get that performs tri-linear interpoloation. <b>Note - does NOT perform bounds checking</b>
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * 
     * @return The interpolated value from the data array.
     */
    public final byte getByteTriLinear(final float x, final float y, final float z) {

        final int xDim = dimExtents[0];
        final int yDim = dimExtents[1];
        final int imageSize = xDim * yDim;
        int position1, position2;
        int intX, intY, intZ;
        float dx, dy, dz;
        float a1, a2;
        float b1, b2;

        intX = (int) x;
        intY = (int) y;
        intZ = (int) z;

        dx = x - intX;
        dy = y - intY;
        dz = z - intZ;

        position1 = (intZ * imageSize) + (intY * xDim) + intX;
        position2 = position1 + imageSize;

        a1 = ( (1 - dx) * data.getByte(position1)) + (dx * data.getByte(position1 + 1));
        a2 = ( (1 - dx) * data.getByte(position1 + xDim)) + (dx * data.getByte(position1 + xDim + 1));
        b1 = ( (1 - dy) * a1) + (dy * a2);

        a1 = ( (1 - dx) * data.getByte(position2)) + (dx * data.getByte(position2 + 1));
        a2 = ( (1 - dx) * data.getByte(position2 + xDim)) + (dx * data.getByte(position2 + xDim + 1));
        b2 = ( (1 - dy) * a1) + (dy * a2);

        return (byte) ( ( (1 - dz) * b1) + (dz * b2));
    }

    /**
     * Color function to get data where bounds checking is performed.
     * 
     * @param position position in one dimensional array
     * @param color DOCUMENT ME!
     * 
     * @return returns value if position in data array range
     */
    public final Number getC(final int position, final int color) {

        if ((position >= 0) && (((4 * position) + color) < dataSize)) {
            return data.get((4 * position) + color);
        }

        return (new Byte((byte) 0));
    }

    /**
     * Accessor method for the m_bConvolve data memeber.
     * 
     * @return m_bConvolve, true when this images is the product of fft( imageA ) fft( imageB )
     */
    public boolean getConvolve() {
        return m_bConvolve;
    }

    /**
     * The interpolated value from the data array.ersion of get that does NOT perform bounds checking.
     * 
     * @param position index into one dimensional array
     * 
     * @return The value from the data array.
     */
    public final double getDouble(final int position) {
        return (data.getDouble(position));
    }

    /**
     * nD get data fuction where bounds checking is NOT performed.
     * 
     * @param position Index into one dimensional array
     * 
     * @return The value from the data array.
     */
    public final double getDouble(final int[] position) {
        int i;
        int location = 0;
        final int dimensions = position.length;

        for (i = dimensions - 1; i > 0; i--) {
            location += (position[i] * dimExtents[i]);
        }

        location += position[0];

        return (data.getDouble(location));
    }

    /**
     * 2D get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * 
     * @return Value at that position in the data array
     */
    public final double getDouble(final int x, final int y) {
        return (data.getDouble( (y * dimExtents[0]) + x));
    }

    /**
     * 3D get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * 
     * @return Value at that position in the data array
     */
    public final double getDouble(final int x, final int y, final int z) {
        return (data.getDouble( (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x));
    }

    /**
     * 4D get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param t t coordinate (usually the volume index)*
     * 
     * @return The value at that position in the data array.
     */
    public final double getDouble(final int x, final int y, final int z, final int t) {
        return (data.getDouble( (t * (dimExtents[0] * dimExtents[1] * dimExtents[2]))
                + (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x));
    }

    /**
     * version of get that performs bi-linear interpoloation. Note - does NOT perform bounds checking
     * 
     * @param x x coordinate
     * @param y y coordinate
     * 
     * @return The interpolated value at that position in the data array.
     */
    public final double getDoubleBiLinear(final float x, final float y) {

        final int xDim = dimExtents[0];
        int position;
        int intX, intY;
        double dx, dy;
        double x1, x2;

        intX = (int) x;
        intY = (int) y;

        dx = x - intX;
        dy = y - intY;
        position = (intY * xDim) + intX;

        // if ((position >= 0) && (position < size-xDim-1) ) {
        x1 = ( (1 - dx) * data.getDouble(position)) + (dx * data.getDouble(position + 1));
        x2 = ( (1 - dx) * data.getDouble(position + xDim)) + (dx * data.getDouble(position + xDim + 1));

        return ( ( (1 - dy) * x1) + (dy * x2));
    }

    /**
     * version of get that performs tri-linear interpoloation. <b>Note - does NOT perform bounds checking</b>
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * 
     * @return The value at that position in the data array.
     */
    public final double getDoubleTriLinear(final float x, final float y, final float z) {

        final int xDim = dimExtents[0];
        final int yDim = dimExtents[1];
        final int imageSize = xDim * yDim;
        int position1, position2;
        int intX, intY, intZ;
        double dx, dy, dz;
        double a1, a2;
        double b1, b2;

        intX = (int) x;
        intY = (int) y;
        intZ = (int) z;

        dx = x - intX;
        dy = y - intY;
        dz = z - intZ;

        position1 = (intZ * imageSize) + (intY * xDim) + intX;
        position2 = position1 + imageSize;

        a1 = ( (1 - dx) * data.getDouble(position1)) + (dx * data.getDouble(position1 + 1));
        a2 = ( (1 - dx) * data.getDouble(position1 + xDim)) + (dx * data.getDouble(position1 + xDim + 1));
        b1 = ( (1 - dy) * a1) + (dy * a2);

        a1 = ( (1 - dx) * data.getDouble(position2)) + (dx * data.getDouble(position2 + 1));
        a2 = ( (1 - dx) * data.getDouble(position2 + xDim)) + (dx * data.getDouble(position2 + xDim + 1));
        b2 = ( (1 - dy) * a1) + (dy * a2);

        return ( ( (1 - dz) * b1) + (dz * b2));
    }

    /**
     * Accessor that returns the extents of the image.
     * 
     * @return array of ints indicating the extents in each dimension
     */
    public final int[] getExtents() {
        return dimExtents;
    }

    /**
     * Returns the image extents translated into the Patient-Coordinate systsm:
     * 
     * @param orientation the Patient-Coordinate view for which the extents are needed:
     * 
     * @return Extents for the image in Patient Coordinates
     */
    public final int[] getExtents(final int orientation) {

        /* Do not reorder the extents if this is less than a 3D image: */
        if (dimExtents.length < 3) {
            return dimExtents;
        }

        final int[] extentsReturn = new int[3];
        final int[] aiAxisOrder = MipavCoordinateSystems.getAxisOrder(this, orientation);

        for (int i = 0; i < 3; i++) {
            extentsReturn[i] = dimExtents[aiAxisOrder[i]];
        }

        return extentsReturn;
    }


    /**
     * Returns the image extents translated into the Patient-Coordinate system:
     * 
     * @param aiAxisOrder the Patient-Coordinate order of the original data for which the extents are needed:
     * 
     * @return Extents for the image in Patient Coordinates
     */
    public final int[] getExtents(final int[] aiAxisOrder) {

        /* Do not reorder the extents if this is less than a 3D image: */
        if (dimExtents.length < 3) {
            return dimExtents;
        }

        final int[] extentsReturn = new int[3];

        for (int i = 0; i < 3; i++) {
            extentsReturn[i] = dimExtents[aiAxisOrder[i]];
        }

        return extentsReturn;
    }

    public final int[] getExtentsSize(final int orientation) {

        final int[] aiSizes = new int[] {1, dimExtents[0], dimExtents[0] * dimExtents[1]};

        final int[] extentsSizesReturn = new int[3];
        final int[] aiAxisOrder = MipavCoordinateSystems.getAxisOrder(this, orientation);

        for (int i = 0; i < 3; i++) {
            extentsSizesReturn[i] = aiSizes[aiAxisOrder[i]];
        }

        return extentsSizesReturn;
    }

    /**
     * Accessor that returns the fileInfo array (one per slice).
     * 
     * @return fileInfo array structure
     */
    public FileInfoBase[] getFileInfo() {
        return fileInfo;
    }

    /**
     * Accessor that returns the fileInfo of a specific image slice.
     * 
     * @param i index that indicates image slice
     * 
     * @return fileInfo array structure for a specific image slice
     */
    public FileInfoBase getFileInfo(final int i) {
        return fileInfo[i];
    }

    /**
     * returns type of filter - low, high, bandpass, or bandstop. - TO BE MOVED
     * 
     * @return filterType
     */
    public final int getFilterType() {
        return filterType;
    }

    /**
     * Version of get that does NOT perform bounds checking.
     * 
     * @param position index into the one dimensional data array
     * 
     * @return The value at that position in the data array.
     */
    public final float getFloat(final int position) {
        return (data.getFloat(position));
    }

    /**
     * nD get data function where bounds checking is NOT performed.
     * 
     * @param position index into the one dimensional data array
     * 
     * @return The value at that position in the data array.
     */
    public final float getFloat(final int[] position) {
        int i;
        int location = 0;
        final int dimensions = position.length;

        for (i = dimensions - 1; i > 0; i--) {
            location += (position[i] * dimExtents[i]);
        }

        location += position[0];

        return (data.getFloat(location));
    }

    /**
     * 2D get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * 
     * @return The value at that position in the data array.
     */
    public final float getFloat(final int x, final int y) {
        return (data.getFloat( (y * dimExtents[0]) + x));
    }

    /**
     * 3D get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * 
     * @return The value at that position in the data array.
     */
    public final float getFloat(final int x, final int y, final int z) {
        return (data.getFloat( (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x));
    }

    /**
     * 4D get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param t t coordinate
     * 
     * @return The value at that position in the data array.
     */
    public final float getFloat(final int x, final int y, final int z, final int t) {
        return (data.getFloat( (t * (dimExtents[0] * dimExtents[1] * dimExtents[2]))
                + (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x));
    }

    /**
     * version of get that performs bi-linear interpoloation. Note - does NOT perform bounds checking
     * 
     * @param x x coordinate
     * @param y y coordinate
     * 
     * @return The interpolated value at that position in the data array.
     */
    public final float getFloatBiLinear(final float x, final float y) {

        final int xDim = dimExtents[0];
        int position;
        int intX, intY;
        float dx, dy;
        float x1, x2;

        intX = (int) x;
        intY = (int) y;

        dx = x - intX;
        dy = y - intY;
        position = (intY * xDim) + intX;

        // if ((position >= 0) && (position < size-xDim-1) ) {
        x1 = ( (1 - dx) * data.getFloat(position)) + (dx * data.getFloat(position + 1));
        x2 = ( (1 - dx) * data.getFloat(position + xDim)) + (dx * data.getFloat(position + xDim + 1));

        return ( ( (1 - dy) * x1) + (dy * x2));
    }

    /**
     * 2D color get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param c color channel index
     * 
     * @return value at that position in the data array
     */
    public final float getFloatC(int x, int y, int c) {
        return data.getFloat((4 * ((y * dimExtents[0]) + x)) + c);
    }

    /**
     * 3D color get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param c color channel index
     * 
     * @return value at that position in the data array
     */
    public final float getFloatC(int x, int y, int z, int c) {
        return data.getFloat(4 * ((z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x) + c);
    }

    /**
     * 4D get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param t t coordinate
     * @param c color channel index
     * 
     * @return The value at that position in the data array.
     */
    public final float getFloatC(int x, int y, int z, int t, int c) {
        return data.getFloat(4 *
                                  ((t * (dimExtents[0] * dimExtents[1] * dimExtents[2])) +
                                       (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x) + c);
    }

    /**
     * version of get that performs tri-linear interpoloation. Note - does NOT perform bounds checking
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * 
     * @return The value at that position in the data array.
     */
    public final float getFloatTriLinear(final float x, final float y, final float z) {

        final int xDim = dimExtents[0];
        final int yDim = dimExtents[1];
        final int imageSize = xDim * yDim;
        int position1, position2;
        int intX, intY, intZ;
        float dx, dy, dz;
        float a1, a2;
        float b1, b2;

        intX = (int) x;
        intY = (int) y;
        intZ = (int) z;

        dx = x - intX;
        dy = y - intY;
        dz = z - intZ;

        position1 = (intZ * imageSize) + (intY * xDim) + intX;
        position2 = position1 + imageSize;

        a1 = ( (1 - dx) * data.getFloat(position1)) + (dx * data.getFloat(position1 + 1));
        a2 = ( (1 - dx) * data.getFloat(position1 + xDim)) + (dx * data.getFloat(position1 + xDim + 1));
        b1 = ( (1 - dy) * a1) + (dy * a2);

        a1 = ( (1 - dx) * data.getFloat(position2)) + (dx * data.getFloat(position2 + 1));
        a2 = ( (1 - dx) * data.getFloat(position2 + xDim)) + (dx * data.getFloat(position2 + xDim + 1));
        b2 = ( (1 - dy) * a1) + (dy * a2);

        return ( ( (1 - dz) * b1) + (dz * b2));
    }

    /**
     * Get a value using tri-linear interpoloation. Note - DOES perform bounds checking (both that x,y,z are valid and
     * that the interp indicies don't cause exceptions).
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * 
     * @return interpolated value if in bounds, 0 if not.
     */
    public final float getFloatTriLinearBounds(final float x, final float y, final float z) {

        final int xDim = dimExtents[0];
        final int yDim = dimExtents[1];
        final int imageSize = xDim * yDim;
        int position1, position2;
        int intX, intY, intZ;
        float dx, dy, dz;
        float a1, a2;
        float b1, b2;

        intX = (int) x;
        intY = (int) y;
        intZ = (int) z;

        dx = x - intX;
        dy = y - intY;
        dz = z - intZ;

        position1 = (intZ * imageSize) + (intY * xDim) + intX;
        position2 = position1 + imageSize;

        if ( (x >= 0) && (y >= 0) && (z >= 0) && (x < xDim) && (y < yDim) && (z < dimExtents[2]) && (position1 >= 0)
                && (position1 < (dataSize - (xDim * yDim) - 1)) && (position2 >= 0)
                && (position2 < (dataSize - (xDim * yDim) - 1))) {
            a1 = ( (1 - dx) * data.getFloat(position1)) + (dx * data.getFloat(position1 + 1));
            a2 = ( (1 - dx) * data.getFloat(position1 + xDim)) + (dx * data.getFloat(position1 + xDim + 1));
            b1 = ( (1 - dy) * a1) + (dy * a2);

            a1 = ( (1 - dx) * data.getFloat(position2)) + (dx * data.getFloat(position2 + 1));
            a2 = ( (1 - dx) * data.getFloat(position2 + xDim)) + (dx * data.getFloat(position2 + xDim + 1));
            b2 = ( (1 - dy) * a1) + (dy * a2);

            return ( ( (1 - dz) * b1) + (dz * b2));
        } else {
            return (float) this.min;
        }
    }
    


    /**
     * Get a value using tri-linear interpoloation. Note - DOES perform bounds checking (both that x,y,z are valid and
     * that the interp indicies don't cause exceptions).
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param t t coordinate
     * 
     * @return interpolated value if in bounds, 0 if not.
     */
    public final float getFloatTriLinearBoundsTime(final float x, final float y, final float z, final int t) {

        final int xDim = dimExtents[0];
        final int yDim = dimExtents[1];
        final int zDim = dimExtents[2];
        final int sliceSize = xDim * yDim;
        final int imageSize = zDim * xDim * yDim;
        int position1, position2;
        int intX, intY, intZ;
        float dx, dy, dz;
        float a1, a2;
        float b1, b2;

        intX = (int) x;
        intY = (int) y;
        intZ = (int) z;

        dx = x - intX;
        dy = y - intY;
        dz = z - intZ;

        position1 = (t * imageSize) + (intZ * sliceSize) + (intY * xDim) + intX;
        position2 = (t * imageSize) + position1 + sliceSize;

        if ( (x >= 0) && (y >= 0) && (z >= 0) && (x < xDim) && (y < yDim) && (z < dimExtents[2]) && (position1 >= 0)
                && (position1 < (dataSize - (xDim * yDim) - 1)) && (position2 >= 0)
                && (position2 < (dataSize - (xDim * yDim) - 1))) {
            a1 = ( (1 - dx) * data.getFloat(position1)) + (dx * data.getFloat(position1 + 1));
            a2 = ( (1 - dx) * data.getFloat(position1 + xDim)) + (dx * data.getFloat(position1 + xDim + 1));
            b1 = ( (1 - dy) * a1) + (dy * a2);

            a1 = ( (1 - dx) * data.getFloat(position2)) + (dx * data.getFloat(position2 + 1));
            a2 = ( (1 - dx) * data.getFloat(position2 + xDim)) + (dx * data.getFloat(position2 + xDim + 1));
            b2 = ( (1 - dy) * a1) + (dy * a2);

            return ( ( (1 - dz) * b1) + (dz * b2));
        } else {
            return (float) this.min;
        }
    }

    /**
     * Get a value using tri-linear interpoloation. Note - DOES perform bounds checking (both that x,y,z are valid and
     * that the interp indicies don't cause exceptions).
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param c color-coordinate
     * 
     * @return interpolated value if in bounds, 0 if not.
     */
    public final float getFloatTriLinearBounds(final float x, final float y, final float z, final int c) {

        final int xDim = dimExtents[0];
        final int yDim = dimExtents[1];
        final int imageSize = xDim * yDim;
        int position1, position2;
        int intX, intY, intZ;
        float dx, dy, dz;
        float a1, a2;
        float b1, b2;

        intX = (int) x;
        intY = (int) y;
        intZ = (int) z;

        dx = x - intX;
        dy = y - intY;
        dz = z - intZ;

        position1 = (intZ * imageSize) + (intY * xDim) + intX;
        position2 = position1 + imageSize;

        if ( (x >= 0) && (y >= 0) && (z >= 0) && (x < xDim) && (y < yDim) && (z < dimExtents[2])
        		&& (position1 >= 0) && (position2 >= 0)
                && (((c + (4 * (position1))) < dataSize)
                && ((c + (4 * (position1 + 1))) < dataSize)
                && ((c + (4 * (position1 + xDim))) < dataSize)
                && (c + (4 * (position1 + xDim + 1))) < dataSize)
                && ((c + (4 * (position2 + 1))) < dataSize) 
                && ((c + (4 * (position2))) < dataSize) 
        	    && ((c + (4 * (position2 + xDim))) < dataSize)
        	    && ((c + (4 * (position2 + xDim + 1))) < dataSize) )
        {
            a1 = ( (1 - dx) * data.getFloat(c + (4 * (position1)))) + (dx * data.getFloat(c + (4 * (position1 + 1))));
            a2 = ( (1 - dx) * data.getFloat(c + (4 * (position1 + xDim))))
                    + (dx * data.getFloat(c + (4 * (position1 + xDim + 1))));
            b1 = ( (1 - dy) * a1) + (dy * a2);

            a1 = ( (1 - dx) * data.getFloat(c + (4 * (position2)))) + (dx * data.getFloat(c + (4 * (position2 + 1))));
            a2 = ( (1 - dx) * data.getFloat(c + (4 * (position2 + xDim))))
                    + (dx * data.getFloat(c + (4 * (position2 + xDim + 1))));
            b2 = ( (1 - dy) * a1) + (dy * a2);

            return ( ( (1 - dz) * b1) + (dz * b2));
        } else {
            return 0;
        }
    }

    /**
     * returns frequency 1 of filter.
     * 
     * @return freq1
     */
    public final float getFreq1() {
        return freq1;
    }

    /**
     * returns frequency 2 of filter.
     * 
     * @return freq2
     */
    public final float getFreq2() {
        return freq2;
    }

    /**
     * returns frequency U of filter.
     * 
     * @return freqU
     */
    public final float getFreqU() {
        return freqU;
    }

    /**
     * returns frequency V of filter.
     * 
     * @return freqV
     */
    public final float getFreqV() {
        return freqV;
    }

    /**
     * DOCUMENT ME! - TO BE MOVED
     * 
     * @return DOCUMENT ME!
     */
    public final boolean getHaveWindowed() {
        return haveWindowed;
    }

    /**
     * Returns the image height, based on the Patient Coordinates orientation from which the data will be viewed:
     * 
     * @param orientation the Patient-Viewing orientation
     * 
     * @return The image height for the viewing orientation
     */
    public int getHeight(final int orientation) {

        if (dimExtents.length < 3) {
            return dimExtents[1];
        }

        final int[] aiAxisOrder = MipavCoordinateSystems.getAxisOrder(this, orientation);

        return dimExtents[aiAxisOrder[1]];
    }

    /**
     * Accessor that returns the boolean indicating if 3D images are processed one slice at a time.
     * 
     * @return boolean telling if slice by slice processing occurs in 3D
     */
    public boolean getImage25D() {
        return image25D;
    }

    /**
     * Gets the image orientation (sagittal, axial, ...).
     * 
     * @return integer representing the orientation
     */
    public int getImageOrientation() {

        if (fileInfo != null) {
            return fileInfo[0].getImageOrientation();
        }
        return FileInfoBase.UNKNOWN_ORIENT;
    }

    /**
     * Version of get that does NOT perform bounds checking.
     * 
     * @param position position in one dimensional array
     * 
     * @return value at that position in the data array
     */
    public final int getInt(final int position) {
        return (data.getInt(position));
    }

    /**
     * nD get data fuction where bounds checking is NOT performed.
     * 
     * @param position array of coordinate values
     * 
     * @return returns true if position in data array range
     */
    public final int getInt(final int[] position) {
        int i;
        int location = 0;
        final int dimensions = position.length;

        for (i = dimensions - 1; i > 0; i--) {
            location += (position[i] * dimExtents[i]);
        }

        location += position[0];

        return (data.getInt(location));
    }

    /**
     * 2D get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * 
     * @return value at that position in the data array
     */
    public final int getInt(final int x, final int y) {
        return (data.getInt( (y * dimExtents[0]) + x));
    }

    /**
     * 3D get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * 
     * @return value at that position in the data array
     */
    public final int getInt(final int x, final int y, final int z) {
        return (data.getInt( (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x));
    }

    /**
     * 4D get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param t t coordinate
     * 
     * @return The value at that position in the data array
     */
    public final int getInt(final int x, final int y, final int z, final int t) {
        return (data.getInt( (t * (dimExtents[0] * dimExtents[1] * dimExtents[2]))
                + (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x));
    }

    /**
     * version of get that performs bi-linear interpoloation. Note - does NOT perform bounds checking
     * 
     * @param x x coordinate
     * @param y y coordinate
     * 
     * @return The interpolated value at that position in the data array
     */
    public final int getIntBiLinear(final float x, final float y) {

        final int xDim = dimExtents[0];
        int position;
        int intX, intY;
        float dx, dy;
        float x1, x2;

        intX = (int) x;
        intY = (int) y;

        dx = x - intX;
        dy = y - intY;
        position = (intY * xDim) + intX;

        // if ((position >= 0) && (position < size-xDim-1) ) {
        x1 = ( (1 - dx) * data.getInt(position)) + (dx * data.getInt(position + 1));
        x2 = ( (1 - dx) * data.getInt(position + xDim)) + (dx * data.getInt(position + xDim + 1));

        return (int) ( ( (1 - dy) * x1) + (dy * x2));
    }

    /**
     * Version of get that performs tri-linear interpoloation. Note - does NOT perform bounds checking
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * 
     * @return The value at that position in the data array
     */
    public final int getIntTriLinear(final float x, final float y, final float z) {

        final int xDim = dimExtents[0];
        final int yDim = dimExtents[1];
        final int imageSize = xDim * yDim;
        int position1, position2;
        int intX, intY, intZ;
        float dx, dy, dz;
        float a1, a2;
        float b1, b2;

        intX = (int) x;
        intY = (int) y;
        intZ = (int) z;

        dx = x - intX;
        dy = y - intY;
        dz = z - intZ;

        position1 = (intZ * imageSize) + (intY * xDim) + intX;
        position2 = position1 + imageSize;

        a1 = ( (1 - dx) * data.getInt(position1)) + (dx * data.getInt(position1 + 1));
        a2 = ( (1 - dx) * data.getInt(position1 + xDim)) + (dx * data.getInt(position1 + xDim + 1));
        b1 = ( (1 - dy) * a1) + (dy * a2);

        a1 = ( (1 - dx) * data.getShort(position2)) + (dx * data.getShort(position2 + 1));
        a2 = ( (1 - dx) * data.getShort(position2 + xDim)) + (dx * data.getShort(position2 + xDim + 1));
        b2 = ( (1 - dy) * a1) + (dy * a2);

        return (int) ( ( (1 - dz) * b1) + (dz * b2));
    }

    /**
     * Accessor that returns the lock status of the image.
     * 
     * @return lock status
     */
    public final int getLockStatus() {
        return lockStatus;
    }

    /**
     * Accessor that returns the boolean indicating if log magnitude displays are used in complex images.
     * 
     * @return boolean telling if log display is used
     */
    public boolean getLogMagDisplay() {
        return logMagDisp;
    }

    /**
     * Version of get that does NOT perform bounds checking.
     * 
     * @param position The index in one dimensional array
     * 
     * @return The value at that position in the data array
     */
    public final long getLong(final int position) {
        return (data.getLong(position));
    }

    /**
     * nD get data fuction where bounds checking is NOT performed.
     * 
     * @param position The index in one dimensional array
     * 
     * @return The value at that position in the data array
     */
    public final long getLong(final int[] position) {
        int i;
        int location = 0;
        final int dimensions = position.length;

        for (i = dimensions - 1; i > 0; i--) {
            location += (position[i] * dimExtents[i]);
        }

        location += position[0];

        return (data.getLong(location));
    }

    /**
     * 2D get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * 
     * @return value at that position in the data array
     */
    public final long getLong(final int x, final int y) {
        return (data.getLong( (y * dimExtents[0]) + x));
    }

    /**
     * 3D get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * 
     * @return value at that position in the data array
     */
    public final long getLong(final int x, final int y, final int z) {
        return (data.getLong( (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x));
    }

    /**
     * 4D get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param t t coordinate
     * 
     * @return value at that position in the data array
     */
    public final long getLong(final int x, final int y, final int z, final int t) {
        return (data.getLong( (t * (dimExtents[0] * dimExtents[1] * dimExtents[2]))
                + (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x));
    }

    /**
     * version of get that performs bi-linear interpoloation. <b>Note - does NOT perform bounds checking</b>
     * 
     * @param x x coordinate
     * @param y y coordinate
     * 
     * @return The interpolated value at that position in the data array
     */
    public final long getLongBiLinear(final float x, final float y) {

        final int xDim = dimExtents[0];
        int position;
        int intX, intY;
        float dx, dy;
        float x1, x2;

        intX = (int) x;
        intY = (int) y;

        dx = x - intX;
        dy = y - intY;
        position = (intY * xDim) + intX;

        // if ((position >= 0) && (position < size-xDim-1) ) {
        x1 = ( (1 - dx) * data.getLong(position)) + (dx * data.getLong(position + 1));
        x2 = ( (1 - dx) * data.getLong(position + xDim)) + (dx * data.getLong(position + xDim + 1));

        return (long) ( ( (1 - dy) * x1) + (dy * x2));
    }

    /**
     * Version of get that performs tri-linear interpoloation. <b>Note - does NOT perform bounds checking</b>
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * 
     * @return The interpolated value at that position in the data array
     */
    public final long getLongTriLinear(final float x, final float y, final float z) {

        final int xDim = dimExtents[0];
        final int yDim = dimExtents[1];
        final int imageSize = xDim * yDim;
        int position1, position2;
        int intX, intY, intZ;
        float dx, dy, dz;
        float a1, a2;
        float b1, b2;

        intX = (int) x;
        intY = (int) y;
        intZ = (int) z;

        dx = x - intX;
        dy = y - intY;
        dz = z - intZ;

        position1 = (intZ * imageSize) + (intY * xDim) + intX;
        position2 = position1 + imageSize;

        a1 = ( (1 - dx) * data.getLong(position1)) + (dx * data.getLong(position1 + 1));
        a2 = ( (1 - dx) * data.getLong(position1 + xDim)) + (dx * data.getLong(position1 + xDim + 1));
        b1 = ( (1 - dy) * a1) + (dy * a2);

        a1 = ( (1 - dx) * data.getLong(position2)) + (dx * data.getLong(position2 + 1));
        a2 = ( (1 - dx) * data.getLong(position2 + xDim)) + (dx * data.getLong(position2 + xDim + 1));
        b2 = ( (1 - dy) * a1) + (dy * a2);

        return (long) ( ( (1 - dz) * b1) + (dz * b2));
    }

    /**
     * Accessor that returns the maximum value in the dataArray.
     * 
     * @return double indicating maximum value in the dataArray
     */
    public double getMax() {
        return max;
    }

    /**
     * Accessor that returns the maximum alpha value in the dataArray.
     * 
     * @return double indicating maximum alpha value in the dataArray
     */
    public double getMaxA() {
        return maxA;
    }

    /**
     * Accessor that returns the maximum blue value in the dataArray.
     * 
     * @return double indicating maximum blue value in the dataArray
     */
    public double getMaxB() {
        return maxB;
    }

    /**
     * Accessor that returns the maximum green value in the dataArray.
     * 
     * @return double indicating maximum green value in the dataArray
     */
    public double getMaxG() {
        return maxG;
    }

    /**
     * Accessor that returns the maximum red value in the dataArray.
     * 
     * @return double indicating maximum red value in the dataArray
     */
    public double getMaxR() {
        return maxR;
    }

    /**
     * Accessor that returns the minimum value in the dataArray.
     * 
     * @return double indicating minimum value in the dataArray
     */
    public double getMin() {
        return min;
    }

    /**
     * Accessor that returns the minimum alpha value in the dataArray.
     * 
     * @return double indicating minimum alpha value in the dataArray
     */
    public double getMinA() {
        return minA;
    }

    /**
     * Accessor that returns the minimum blue value in the dataArray.
     * 
     * @return double indicating minimum blue value in the dataArray
     */
    public double getMinB() {
        return minB;
    }

    /**
     * getMinG - Accessor that returns the minimum green value in the dataArray.
     * 
     * @return double indicating minimum green value in the dataArray
     */
    public double getMinG() {
        return minG;
    }

    /**
     * Accessor that returns the minimum red value in the dataArray.
     * 
     * @return double indicating minimum red value in the dataArray
     */
    public double getMinR() {
        return minR;
    }

    /**
     * Accessor that returns the dimensionality of the image.
     * 
     * @return int indicating the number of dimensions
     */
    public final int getNDims() {
        return nDims;
    }

    /**
     * Accessor that returns the maximum value without log processing in the dataArray.
     * 
     * @return double indicating maximum value in the dataArray
     */
    public double getNoLogMax() {
        return noLogMax;
    }

    /**
     * Accessor that returns the minimum value without log processing in the dataArray.
     * 
     * @return double indicating minimum value in the dataArray
     */
    public double getNoLogMin() {
        return noLogMin;
    }

    /**
     * Accessor that returns the minimum nonzero value without log processing in the dataArray.
     * 
     * @return double indicating minimum value in the dataArray
     */
    public double getNoLogMinNonZero() {
        return noLogMinNonZero;
    }

    /**
     * Accessor that returns the smallest magnitude negative value in the dataArray.
     * 
     * @return double indicating maximum nonzero value in the dataArray NaN if no nonzero value present
     */
    public double getNonZeroMax() {
        return nonZeroMax;
    }

    /**
     * Accessor that returns the maximum nonzero blue value in the dataArray.
     * 
     * @return double indicating maximum nonzero blue value in the dataArray NaN if no nonzero blue value present
     */
    public double getNonZeroMaxB() {
        return nonZeroMaxB;
    }

    /**
     * Accessor that returns the maximum nonzero green value in the dataArray.
     * 
     * @return double indicating maximum nonzero green value in the dataArray NaN if no nonzero green value present
     */
    public double getNonZeroMaxG() {
        return nonZeroMaxG;
    }

    /**
     * Accessor that returns the maximum nonzero red value in the dataArray.
     * 
     * @return double indicating maximum nonzero red value in the dataArray NaN if no nonzero red value present
     */
    public double getNonZeroMaxR() {
        return nonZeroMaxR;
    }

    /**
     * Accessor that returns the minimum nonzero value in the dataArray.
     * 
     * @return double indicating minimum nonzero value in the dataArray NaN if no nonzero value present
     */
    public double getNonZeroMin() {
        return nonZeroMin;
    }

    /**
     * Accessor that returns the minimum nonzero blue value in the dataArray.
     * 
     * @return double indicating minimum nonzero blue value in the dataArray NaN if no nonzero blue value present
     */
    public double getNonZeroMinB() {
        return nonZeroMinB;
    }

    /**
     * Accessor that returns the minimum nonzero green value in the dataArray.
     * 
     * @return double indicating minimum nonzero green value in the dataArray NaN if no nonzero green value present
     */
    public double getNonZeroMinG() {
        return nonZeroMinG;
    }

    /**
     * Accessor that returns the minimum nonzero red value in the dataArray.
     * 
     * @return double indicating minimum nonzero red value in the dataArray NaN if no nonzero red value present
     */
    public double getNonZeroMinR() {
        return nonZeroMinR;
    }

    /**
     * Returns the origin of the image.
     * 
     * @return the origin of the image
     */
    public float[] getOrigin() {

        if (fileInfo == null) {
            return null;
        }

        return fileInfo[0].getOrigin();
    }

    /**
     * Returns the image origin for the image translated into the Patient-Coordinate systsm:
     * 
     * @param index the fileInfo index
     * @param orientation the Patient-Coordinate view for which the origin are needed:
     * 
     * @return the origin of the image in Patient Coordinates
     */
    public float[] getOrigin(final int index, final int orientation) {

        if (fileInfo == null) {
            return null;
        }

        final float[] originTemp = fileInfo[index].getOrigin();

        /* Do not reorder the origin if this is less than a 3D image: */
        if (dimExtents.length < 3) {
            return originTemp;
        }

        final float[] originReturn = new float[3];
        final int[] aiAxisOrder = MipavCoordinateSystems.getAxisOrder(this, orientation);

        for (int i = 0; i < 3; i++) {
            originReturn[i] = originTemp[aiAxisOrder[i]];
        }

        return originReturn;
    }

    /**
     * returns integer telling Butterworth order. - TO BE MOVED
     * 
     * @return value indicating the Butterworth filter order;
     */
    public final int getOriginalButterworthOrder() {
        return originalButterworthOrder;
    }

    /**
     * DOCUMENT ME! - TO BE MOVED
     * 
     * @return DOCUMENT ME!
     */
    public final boolean getOriginalCropCheckbox() {
        return originalCropCheckbox;
    }

    /**
     * DOCUMENT ME! - TO BE MOVED
     * 
     * @return DOCUMENT ME!
     */
    public final boolean getOriginalDoCrop() {
        return originalDoCrop;
    }

    /**
     * DOCUMENT ME! -TO BE MOVED
     * 
     * @return DOCUMENT ME!
     */
    public final int[] getOriginalEnd() {
        return originalEnd;
    }

    /**
     * Accessor that returns the original extents of the image.
     * 
     * @return array of ints indicating the original extents in each dimension
     */
    public final int[] getOriginalExtents() {
    	if ( dimOriginalExtents != null )
    		return dimOriginalExtents;
    	return dimExtents;
    }

    /**
     * returns integer telling filter construction method.
     * 
     * @return int showing original filter construction method
     */

    public final int getOriginalFilterConstruction() {
        return originalFilterConstruction;
    }

    /**
     * returns kernel diameter chosen on forward FFT.
     * 
     * @return int showing original kernel diameter chosen on forward FFT
     */
    public final int getOriginalKernelDimension() {
        return originalKernelDimension;
    }

    /**
     * DOCUMENT ME! - TO BE MOVED
     * 
     * @return DOCUMENT ME! - to be moved
     */
    public final float getOriginalMaximum() {
        return originalMaximum;
    }

    /**
     * DOCUMENT ME! - TO BE MOVED
     * 
     * @return DOCUMENT ME!
     */
    public final float getOriginalMinimum() {
        return originalMinimum;
    }

    /**
     * DOCUMENT ME! - TO BE MOVED
     * 
     * @return DOCUMENT ME!
     */
    public final int[] getOriginalStart() {
        return originalStart;
    }

    /**
     * Gets the radiological view flag:
     * 
     * @return the RadiologicalView on/off
     */
    public boolean getRadiologicalView() {
        return m_bRadiologicalView;
    }

    /**
     * Returns the resolutions for the image without regarding resolution difference between slices.
     * 
     * @param index The index indicating which data slice to exact the resolutions
     * 
     * @return the resolutions for the data slice
     */
    public float[] getResolutions(final int index) {

        if (fileInfo == null) {
            return null;
        }

        return fileInfo[index].getResolutions();
    }

    /**
     * Returns the resolutions for the image translated into the Patient-Coordinate systsm:
     * 
     * @param index the fileInfo index
     * @param orientation the Patient-Coordinate view for which the resolutions are needed:
     * 
     * @return the resolutions for the image in Patient Coordinates
     */
    public float[] getResolutions(final int index, final int orientation) {

        if (fileInfo == null) {
            return null;
        }

        final float[] resTemp = fileInfo[index].getResolutions();

        /* Do not reorder the resolutions if this is less than a 3D image: */
        if (dimExtents.length < 3) {
            return resTemp;
        }

        final float[] resReturn = new float[3];
        final int[] aiAxisOrder = MipavCoordinateSystems.getAxisOrder(this, orientation);

        for (int i = 0; i < 3; i++) {
            resReturn[i] = resTemp[aiAxisOrder[i]];
        }

        return resReturn;
    }

    /**
     * Returns the resolutions for the image translated into the Patient-Coordinate systsm:
     * 
     * @param index the fileInfo index
     * @param aiAxisOrder the Patient-Coordinate order of the original data for which the resolutions are needed:
     * 
     * @return the resolutions for the image in Patient Coordinates
     */
    public float[] getResolutions(final int index, final int[] aiAxisOrder) {

        if (fileInfo == null) {
            return null;
        }

        final float[] resTemp = fileInfo[index].getResolutions();

        /* Do not reorder the resolutions if this is less than a 3D image: */
        if (dimExtents.length < 3) {
            return resTemp;
        }

        final float[] resReturn = new float[3];

        for (int i = 0; i < 3; i++) {
            resReturn[i] = resTemp[aiAxisOrder[i]];
        }

        return resReturn;
    }

    /**
     * Version of get that does NOT perform bounds checking.
     * 
     * @param position The index into the one dimensional array
     * 
     * @return The value at that position in the data array
     */
    public final short getShort(final int position) {
        return (data.getShort(position));
    }

    /**
     * nD get data fuction where bounds checking is NOT performed.
     * 
     * @param position The index into the one dimensional array
     * 
     * @return The value at that position in the data array
     */
    public final short getShort(final int[] position) {
        int i;
        int location = 0;
        final int dimensions = position.length;

        for (i = dimensions - 1; i > 0; i--) {
            location += (position[i] * dimExtents[i]);
        }

        location += position[0];

        return (data.getShort(location));
    }

    /**
     * 2D get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * 
     * @return value at that position in the data array
     */
    public final short getShort(final int x, final int y) {
        return (data.getShort( (y * dimExtents[0]) + x));
    }

    /**
     * 3D get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * 
     * @return value at that position in the data array
     */
    public final short getShort(final int x, final int y, final int z) {
        return (data.getShort( (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x));
    }

    /**
     * 4D get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param t t coordinate
     * 
     * @return value at that position in the data array
     */
    public final short getShort(final int x, final int y, final int z, final int t) {
        return (data.getShort( (t * (dimExtents[0] * dimExtents[1] * dimExtents[2]))
                + (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x));
    }

    /**
     * version of get that performs bi-linear interpoloation. Note - does NOT perform bounds checking
     * 
     * @param x x coordinate
     * @param y y coordinate
     * 
     * @return The value at that position in the data array
     */
    public final short getShortBiLinear(final float x, final float y) {

        final int xDim = dimExtents[0];
        int position;
        int intX, intY;
        float dx, dy;
        float x1, x2;

        intX = (int) x;
        intY = (int) y;

        dx = x - intX;
        dy = y - intY;
        position = (intY * xDim) + intX;

        // if ((position >= 0) && (position < size-xDim-1) ) {
        x1 = ( (1 - dx) * data.getShort(position)) + (dx * data.getShort(position + 1));
        x2 = ( (1 - dx) * data.getShort(position + xDim)) + (dx * data.getShort(position + xDim + 1));

        return (short) ( ( (1 - dy) * x1) + (dy * x2));
    }

    /**
     * version of get that performs tri-linear interpoloation. <b>Note - does NOT perform bounds checking</b>
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * 
     * @return The intoplated value at that position in the data array
     */
    public final short getShortTriLinear(final float x, final float y, final float z) {

        final int xDim = dimExtents[0];
        final int yDim = dimExtents[1];
        final int imageSize = xDim * yDim;
        int position1, position2;
        int intX, intY, intZ;
        float dx, dy, dz;
        float a1, a2;
        float b1, b2;

        intX = (int) x;
        intY = (int) y;
        intZ = (int) z;

        dx = x - intX;
        dy = y - intY;
        dz = z - intZ;

        position1 = (intZ * imageSize) + (intY * xDim) + intX;
        position2 = position1 + imageSize;

        a1 = ( (1 - dx) * data.getShort(position1)) + (dx * data.getShort(position1 + 1));
        a2 = ( (1 - dx) * data.getShort(position1 + xDim)) + (dx * data.getShort(position1 + xDim + 1));
        b1 = ( (1 - dy) * a1) + (dy * a2);

        a1 = ( (1 - dx) * data.getShort(position2)) + (dx * data.getShort(position2 + 1));
        a2 = ( (1 - dx) * data.getShort(position2 + xDim)) + (dx * data.getShort(position2 + xDim + 1));
        b2 = ( (1 - dy) * a1) + (dy * a2);

        return (short) ( ( (1 - dz) * b1) + (dz * b2));
    }

    /**
     * returns standard deviation U of filter. TO BE MOVED
     * 
     * @return sigmaU
     */
    public final float getSigmaU() {
        return sigmaU;
    }

    /**
     * returns standard deviation V of filter. TO BE MOVED
     * 
     * @return sigmaV
     */
    public final float getSigmaV() {
        return sigmaV;
    }

    /**
     * Accessor that returns the total size(length) of the data array.
     * 
     * @return Value indicating the number of data points in the data array
     */
    public final int getSize() {
        return dataSize;
    }

    /**
     * Get the nuber of pixels in a slice of the image.
     * 
     * @return the number of pixels in a slice
     */
    public final int getSliceSize() {
        return dimExtents[0] * dimExtents[1];
    }

    /**
     * Accessor that returns the smallest magnitude negative value in the dataArray.
     * 
     * @return value indicating smallest magnitude negative value in the dataArray NaN if no negative value is present
     */
    public double getSmallestMagnitudeNegative() {
        return smallestMagnitudeNegative;
    }

    /**
     * Accessor that returns the smallest magnitude negative blue value in the dataArray.
     * 
     * @return value indicating smallest magnitude negative blue value in the dataArray NaN if no negative blue value is
     *         present
     */
    public double getSmallestMagnitudeNegativeB() {
        return smallestMagnitudeNegativeB;
    }

    /**
     * Accessor that returns the smallest magnitude negative green value in the dataArray.
     * 
     * @return value indicating smallest magnitude negative green value in the dataArray NaN if no negative green value
     *         is present
     */
    public double getSmallestMagnitudeNegativeG() {
        return smallestMagnitudeNegativeG;
    }

    /**
     * Accessor that returns the smallest magnitude negative red value in the dataArray.
     * 
     * @return value indicating smallest magnitude negative red value in the dataArray NaN if no negative red value is
     *         present
     */
    public double getSmallestMagnitudeNegativeR() {
        return smallestMagnitudeNegativeR;
    }

    /**
     * Accessor that returns the smallest magnitude positive value in the dataArray.
     * 
     * @return value indicating smallest magnitude positive value in the dataArray NaN if no positive value is present
     */
    public double getSmallestMagnitudePositive() {
        return smallestMagnitudePositive;
    }

    /**
     * Accessor that returns the smallest magnitude positive blue value in the dataArray.
     * 
     * @return value indicating smallest magnitude positive blue value in the dataArray NaN if no positive blue value is
     *         present
     */
    public double getSmallestMagnitudePositiveB() {
        return smallestMagnitudePositiveB;
    }

    /**
     * Accessor that returns the smallest magnitude positive green value in the dataArray.
     * 
     * @return value indicating smallest magnitude positive green value in the dataArray NaN if no positive green value
     *         is present
     */
    public double getSmallestMagnitudePositiveG() {
        return smallestMagnitudePositiveG;
    }

    /**
     * Accessor that returns the smallest magnitude positive red value in the dataArray.
     * 
     * @return value indicating smallest magnitude positive red value in the dataArray NaN if no positive red value is
     *         present
     */
    public double getSmallestMagnitudePositiveR() {
        return smallestMagnitudePositiveR;
    }

    /**
     * Returns the surface mask from this image.
     * 
     * @param index the index of the mask to remove.
     * 
     * @return the BitSet mask
     */
    public BitSet getSurfaceMask(final int index) {

        if (m_kMaskVector.size() > index) {
            return (BitSet) m_kMaskVector.get(index);
        }

        return null;
    }

    /**
     * Returns the surface mask from this image.
     * 
     * @param index the index of the mask to remove.
     * 
     * @return the BitSet mask
     */
    public ColorRGBA getSurfaceMaskColor(final int index) {

        if (m_kColorVector.size() > index) {
            return (ColorRGBA) m_kColorVector.get(index);
        }

        return null;
    }

    /**
     * Returns roation angle theta in radians of filter. TO BE MOVED
     * 
     * @return theta
     */
    public final float getTheta() {
        return theta;
    }

    /**
     * version of get that performs tri-linear interpoloation. <b>Note - does perform bounds checking</b>
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * 
     * @return The introplated value at that position in the data array
     */
    public final Number getTriLinear(final float x, final float y, final float z) {

        final int xDim = dimExtents[0];
        final int yDim = dimExtents[1];
        final int imageSize = xDim * yDim;
        int position1, position2;
        int intX, intY, intZ;
        float dx, dy, dz;
        float a1, a2;
        float b1, b2;

        intX = (int) x;
        intY = (int) y;
        intZ = (int) z;

        dx = x - intX;
        dy = y - intY;
        dz = z - intZ;

        position1 = (intZ * imageSize) + (intY * xDim) + intX;
        position2 = position1 + imageSize;

        if ( (position1 >= 0) && (position1 < (dataSize - (xDim * yDim) - 1)) && (position2 >= 0)
                && (position2 < (dataSize - (xDim * yDim) - 1))) {
            a1 = ( (1 - dx) * data.get(position1).floatValue()) + (dx * data.get(position1 + 1).floatValue());
            a2 = ( (1 - dx) * data.get(position1 + xDim).floatValue())
                    + (dx * data.get(position1 + xDim + 1).floatValue());
            b1 = ( (1 - dy) * a1) + (dy * a2);

            a1 = ( (1 - dx) * data.get(position2).floatValue()) + (dx * data.get(position2 + 1).floatValue());
            a2 = ( (1 - dx) * data.get(position2 + xDim).floatValue())
                    + (dx * data.get(position2 + xDim + 1).floatValue());
            b2 = ( (1 - dy) * a1) + (dy * a2);

            return (new Float( ( (1 - dz) * b1) + (dz * b2)));
        } else {
            return (new Byte((byte) 0));
        }
    }

    /**
     * Accessor that returns the image buffer type.
     * 
     * @return Indicates the data buffer type
     */
    public final int getType() {
        return bufferType.getLegacyNum();
    }
    
    /**
     * Accessor that returns the image buffer type as an enum.
     * 
     * @return The data buffer type
     */
    public final DataType getDataType() {
        return bufferType;
    }
    
    /**
     * Accessor that returns the image type.
     * 
     * @return The data buffer type as a String.
     */
    public final String getTypeString() {
        return bufferType.toString();
    }

    /**
     * version of get that does NOT perform bounds checking.
     * 
     * @param position The index into the one dimensional array
     * 
     * @return DOCUMENT ME!
     */
    public final short getUByte(final int position) {
        return (data.getUByte(position));
    }

    /**
     * nD get data fuction where bounds checking is NOT performed.
     * 
     * @param position The index into the one dimensional array
     * 
     * @return The value at that position in the data array
     */
    public final short getUByte(final int[] position) {
        int i;
        int location = 0;
        final int dimensions = position.length;

        for (i = dimensions - 1; i > 0; i--) {
            location += (position[i] * dimExtents[i]);
        }

        location += position[0];

        return (data.getUByte(location));
    }

    /**
     * 2D get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * 
     * @return The interpolated value at that position in the data array.
     */
    public final short getUByte(final int x, final int y) {
        return (data.getUByte( (y * dimExtents[0]) + x));
    }

    /**
     * 3D get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * 
     * @return The value at that position in the data array.
     */
    public final short getUByte(final int x, final int y, final int z) {
        return (data.getUByte( (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x));
    }

    /**
     * 4D get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param t t coordinate
     * 
     * @return The value at that position in the data array
     */
    public final short getUByte(final int x, final int y, final int z, final int t) {
        return (data.getUByte( (t * (dimExtents[0] * dimExtents[1] * dimExtents[2]))
                + (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x));
    }

    /**
     * Version of get that performs bi-linear interpoloation. Note - does NOT perform bounds checking
     * 
     * @param x x coordinate
     * @param y y coordinate
     * 
     * @return The interpolated value at that position in the data array.
     */
    public final short getUByteBiLinear(final float x, final float y) {

        final int xDim = dimExtents[0];
        int position;
        int intX, intY;
        float dx, dy;
        float x1, x2;

        intX = (int) x;
        intY = (int) y;

        dx = x - intX;
        dy = y - intY;
        position = (intY * xDim) + intX;

        // if ((position >= 0) && (position < size-xDim-1) ) {
        x1 = ( (1 - dx) * data.getUByte(position)) + (dx * data.getUByte(position + 1));
        x2 = ( (1 - dx) * data.getUByte(position + xDim)) + (dx * data.getUByte(position + xDim + 1));

        return (short) ( ( (1 - dy) * x1) + (dy * x2));
    }

    /**
     * Version of get that performs tri-linear interpoloation. <b>Note - does NOT perform bounds checking</b>
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * 
     * @return The interpolated value at that position in the data array.
     */
    public final short getUByteTriLinear(final float x, final float y, final float z) {

        final int xDim = dimExtents[0];
        final int yDim = dimExtents[1];
        final int imageSize = xDim * yDim;
        int position1, position2;
        int intX, intY, intZ;
        float dx, dy, dz;
        float a1, a2;
        float b1, b2;

        intX = (int) x;
        intY = (int) y;
        intZ = (int) z;

        dx = x - intX;
        dy = y - intY;
        dz = z - intZ;

        position1 = (intZ * imageSize) + (intY * xDim) + intX;
        position2 = position1 + imageSize;
        a1 = ( (1 - dx) * data.getByte(position1)) + (dx * data.getByte(position1 + 1));
        a2 = ( (1 - dx) * data.getByte(position1 + xDim)) + (dx * data.getByte(position1 + xDim + 1));
        b1 = ( (1 - dy) * a1) + (dy * a2);

        a1 = ( (1 - dx) * data.getByte(position2)) + (dx * data.getByte(position2 + 1));
        a2 = ( (1 - dx) * data.getByte(position2 + xDim)) + (dx * data.getByte(position2 + xDim + 1));
        b2 = ( (1 - dy) * a1) + (dy * a2);

        return (short) ( ( (1 - dz) * b1) + (dz * b2));
    }

    /**
     * version of get that does NOT perform bounds checking.
     * 
     * @param position position in one dimensional array
     * 
     * @return The value at that position in the data array.
     */
    public final long getUInt(final int position) {
        return (data.getUInt(position));
    }

    /**
     * nD get data fuction where bounds checking is NOT performed.
     * 
     * @param position The index into the array of data.
     * 
     * @return The value at that position in the data array.
     */
    public final long getUInt(final int[] position) {
        int i;
        int location = 0;
        final int dimensions = position.length;

        for (i = dimensions - 1; i > 0; i--) {
            location += (position[i] * dimExtents[i]);
        }

        location += position[0];

        return (data.getUInt(location));
    }

    /**
     * 2D get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * 
     * @return value at that position in the data array
     */
    public final long getUInt(final int x, final int y) {
        return (data.getUInt( (y * dimExtents[0]) + x));
    }

    /**
     * 3D get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * 
     * @return value at that position in the data array
     */
    public final long getUInt(final int x, final int y, final int z) {
        return (data.getUInt( (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x));
    }

    /**
     * 4D get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param t t coordinate
     * 
     * @return The value at that position in the data array.
     */
    public final long getUInt(final int x, final int y, final int z, final int t) {
        return (data.getUInt( (t * (dimExtents[0] * dimExtents[1] * dimExtents[2]))
                + (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x));
    }

    /**
     * version of get that performs bi-linear interpoloation. Note - does NOT perform bounds checking
     * 
     * @param x x coordinate
     * @param y y coordinate
     * 
     * @return The value at that position in the data array.
     */
    public final long getUIntBiLinear(final float x, final float y) {

        final int xDim = dimExtents[0];
        int position;
        int intX, intY;
        float dx, dy;
        float x1, x2;

        intX = (int) x;
        intY = (int) y;

        dx = x - intX;
        dy = y - intY;
        position = (intY * xDim) + intX;

        // if ((position >= 0) && (position < size-xDim-1) ) {
        x1 = ( (1 - dx) * data.getUInt(position)) + (dx * data.getUInt(position + 1));
        x2 = ( (1 - dx) * data.getUInt(position + xDim)) + (dx * data.getUInt(position + xDim + 1));

        return (long) ( ( (1 - dy) * x1) + (dy * x2));
    }

    /**
     * version of get that performs tri-linear interpoloation. Note - does NOT perform bounds checking
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * 
     * @return The interpolated value at that position in the data array.
     */
    public final long getUIntTriLinear(final float x, final float y, final float z) {

        final int xDim = dimExtents[0];
        final int yDim = dimExtents[1];
        final int imageSize = xDim * yDim;
        int position1, position2;
        int intX, intY, intZ;
        float dx, dy, dz;
        float a1, a2;
        float b1, b2;

        intX = (int) x;
        intY = (int) y;
        intZ = (int) z;

        dx = x - intX;
        dy = y - intY;
        dz = z - intZ;

        position1 = (intZ * imageSize) + (intY * xDim) + intX;
        position2 = position1 + imageSize;

        a1 = ( (1 - dx) * data.getUInt(position1)) + (dx * data.getUInt(position1 + 1));
        a2 = ( (1 - dx) * data.getUInt(position1 + xDim)) + (dx * data.getUInt(position1 + xDim + 1));
        b1 = ( (1 - dy) * a1) + (dy * a2);

        a1 = ( (1 - dx) * data.getUInt(position2)) + (dx * data.getUInt(position2 + 1));
        a2 = ( (1 - dx) * data.getUInt(position2 + xDim)) + (dx * data.getUInt(position2 + xDim + 1));
        b2 = ( (1 - dy) * a1) + (dy * a2);

        return (long) ( ( (1 - dz) * b1) + (dz * b2));
    }

    /**
     * Accessor that returns the boolean indicating if unequal dimensions are allowed in complex images. TO BE MOVED
     * 
     * @return Indicated if unequal dimensions are allowed
     */
    public boolean getUnequalDim() {
        return unequalDim;
    }

    /**
     * Returns the units used to measure all dimensions of the image.
     * 
     * @return The units used to measure all dimensions of the image.
     */
    public int[] getUnitsOfMeasure() {

        if (fileInfo == null) {
            return null;
        }

        return fileInfo[0].getUnitsOfMeasure();
    }

    /**
     * Returns the unit used to measure the specific dimension of image.
     * 
     * @param index The index of specific dimension
     * 
     * @return The unit used to measure the specific dimension
     */
    public int getUnitsOfMeasure(final int index) {

        if (fileInfo == null) {
            return -1;
        }

        return fileInfo[0].getUnitsOfMeasure(index);

    }

    /**
     * Returns the units of measure for the image translated into the Patient-Coordinate systsm:
     * 
     * @param index the fileInfo index
     * @param orientation the Patient-Coordinate view for which the units of measure are needed:
     * 
     * @return the units of measure for the image in Patient Coordinates
     */
    public int[] getUnitsOfMeasure(final int index, final int orientation) {

        if (fileInfo == null) {
            return null;
        }

        final int[] unitsTemp = fileInfo[index].getUnitsOfMeasure();

        /* Do not reorder the units if this is less than a 3D image: */
        if (dimExtents.length < 3) {
            return unitsTemp;
        }

        final int[] unitsReturn = new int[3];
        final int[] aiAxisOrder = MipavCoordinateSystems.getAxisOrder(this, orientation);

        for (int i = 0; i < 3; i++) {
            unitsReturn[i] = unitsTemp[aiAxisOrder[i]];
        }

        return unitsReturn;
    }

    /**
     * version of get that does NOT perform bounds checking.
     * 
     * @param position The index into the one dimensional array
     * 
     * @return The value at that position in the data array
     */
    public final int getUShort(final int position) {
        return (data.getUShort(position));
    }

    /**
     * nD get data fuction where bounds checking is NOT performed.
     * 
     * @param position The index into the one dimensional array
     * 
     * @return The value at that position in the data array
     */
    public final int getUShort(final int[] position) {
        int i;
        int location = 0;
        final int dimensions = position.length;

        for (i = dimensions - 1; i > 0; i--) {
            location += (position[i] * dimExtents[i]);
        }

        location += position[0];

        return (data.getUShort(location));
    }

    /**
     * 2D get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * 
     * @return The value at that position in the data array
     */
    public final int getUShort(final int x, final int y) {
        return (data.getUShort( (y * dimExtents[0]) + x));
    }

    /**
     * 3D get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * 
     * @return The value at that position in the data array
     */
    public final int getUShort(final int x, final int y, final int z) {
        return (data.getUShort( (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x));
    }

    /**
     * 4D get data fuction where bounds checking is NOT performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param t t coordinate
     * 
     * @return The value at that position in the data array
     */
    public final int getUShort(final int x, final int y, final int z, final int t) {
        return (data.getUShort( (t * (dimExtents[0] * dimExtents[1] * dimExtents[2]))
                + (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x));
    }

    /**
     * Version of get that performs bi-linear interpoloation. Note - does NOT perform bounds checking
     * 
     * @param x x coordinate
     * @param y y coordinate
     * 
     * @return The interpolated value at that position in the data array.
     */
    public final int getUShortBiLinear(final float x, final float y) {

        final int xDim = dimExtents[0];
        int position;
        int intX, intY;
        float dx, dy;
        float x1, x2;

        intX = (int) x;
        intY = (int) y;

        dx = x - intX;
        dy = y - intY;
        position = (intY * xDim) + intX;

        // if ((position >= 0) && (position < size-xDim-1) ) {
        x1 = ( (1 - dx) * data.getUShort(position)) + (dx * data.getUShort(position + 1));
        x2 = ( (1 - dx) * data.getUShort(position + xDim)) + (dx * data.getUShort(position + xDim + 1));

        return (int) ( ( (1 - dy) * x1) + (dy * x2));
    }

    /**
     * version of get that performs tri-linear interpoloation. Note - does NOT perform bounds checking
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * 
     * @return The interpolated value at that position in the data array.
     */
    public final int getUShortTriLinear(final float x, final float y, final float z) {

        final int xDim = dimExtents[0];
        final int yDim = dimExtents[1];
        final int imageSize = xDim * yDim;
        int position1, position2;
        int intX, intY, intZ;
        float dx, dy, dz;
        float a1, a2;
        float b1, b2;

        intX = (int) x;
        intY = (int) y;
        intZ = (int) z;

        dx = x - intX;
        dy = y - intY;
        dz = z - intZ;

        position1 = (intZ * imageSize) + (intY * xDim) + intX;
        position2 = position1 + imageSize;

        a1 = ( (1 - dx) * data.getUShort(position1)) + (dx * data.getUShort(position1 + 1));
        a2 = ( (1 - dx) * data.getUShort(position1 + xDim)) + (dx * data.getUShort(position1 + xDim + 1));
        b1 = ( (1 - dy) * a1) + (dy * a2);

        a1 = ( (1 - dx) * data.getUShort(position2)) + (dx * data.getUShort(position2 + 1));
        a2 = ( (1 - dx) * data.getUShort(position2 + xDim)) + (dx * data.getUShort(position2 + xDim + 1));
        b2 = ( (1 - dy) * a1) + (dy * a2);

        return (int) ( ( (1 - dz) * b1) + (dz * b2));
    }

    /**
     * Get the factors needed to iterate through the image volume. Can be multiplied against iterators to retreive the
     * index into the image volume data.
     * 
     * @return multiples of dimExtents[] for the image
     */
    public int[] getVolumeIterationFactors() {
        return new int[] {1, dimExtents[0], dimExtents[0] * dimExtents[1]};
    }
    
    /**
     * Gets the number of pixels in a volume of the image.
     */
    public int getVolumeSize() {
        return getSliceSize() * (getNDims() > 2 ? getExtents()[2] : 1);
    }

    /**
     * Returns the image width, based on the Patient Coordinates orientation from which the data will be viewed:
     * 
     * @param orientation the Patient-Viewing orientation
     * 
     * @return dimExtents representing the image width for the viewing orientation
     */
    public int getWidth(final int orientation) {

        if (dimExtents.length < 3) {
            return dimExtents[0];
        }

        final int[] aiAxisOrder = MipavCoordinateSystems.getAxisOrder(this, orientation);

        return dimExtents[aiAxisOrder[0]];
    }

    /**
     * import Complex data (in 2 float units) into data array.
     * 
     * @param start indicates starting position in data array
     * @param valuesR array where real data is to be acquired
     * @param valuesI array where imaginary data is to be acquired
     * @param mmFlag whether or not to calculate min and max magnitude values for the image
     * @param logMagDisplay whether or not min and max are calculated for log10 of 1 + magnitude array
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void importComplexData(final int start, final float[] valuesR, final float[] valuesI,
            final boolean mmFlag, final boolean logMagDisplay) throws IOException {
        final int length = valuesR.length;
        final int lengthi = valuesI.length;
        int ptr;

        if ( (length == lengthi) && (start >= 0) && ( (start + (2 * length)) <= dataSize)) {

            try {
                setLock(ModelStorageBase.RW_LOCKED);
                ptr = start;

                for (int i = 0; i < length; i++, ptr += 2) {
                    data.setFloat(ptr, valuesR[i]);
                    data.setFloat(ptr + 1, valuesI[i]);
                }

                setLogMagDisplay(logMagDisplay);

                if (mmFlag) {
                    calcMinMaxMag(logMagDisplay);
                }
            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Import data error: bounds incorrect");
    }

    /**
     * import Number data into data array.
     * 
     * @param start indicates starting position in data array
     * @param values array where data is to be acquired
     * @param mmFlag whether or not to calculate min and max values for the image array
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void importData(final int start, final Number[] values, final boolean mmFlag)
            throws IOException {
        final int length = values.length;
        int ptr;

        if ( (start >= 0) && ( (start + length) <= dataSize)) {

            try {
                setLock(ModelStorageBase.RW_LOCKED);
                ptr = start;

                for (int i = 0; i < length; i++, ptr++) {
                    data.set(ptr, values[i]);
                }

                if (mmFlag) {
                    calcMinMax();
                }

            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Import data error: bounds incorrect");
    }

    /**
     * Import boolean (BitSet) data into data array.
     * 
     * @param start indicates starting position in data array
     * @param values array where data is to be acquired
     * @param mmFlag whether or not to calculate min and max values for the image array
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void importData(final int start, final BitSet values, final boolean mmFlag)
            throws IOException {

        final int length = values.size();
        int ptr;

        if ( (start >= 0) && ( (start + length) <= dataSize)) {

            try {
                setLock(ModelStorageBase.RW_LOCKED);
                ptr = start;

                for (int i = 0; i < length; i++, ptr++) {
                    data.setBoolean(ptr, values.get(i));
                }

                if (mmFlag) {
                    calcMinMax();
                }
            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Import data error: bounds incorrect");
    }

    /**
     * Import boolean data into data array.
     * 
     * @param start indicates starting position in data array
     * @param values array where data is to be acquired
     * @param mmFlag whether or not to calculate min and max values for the image array
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void importData(final int start, final boolean[] values, final boolean mmFlag)
            throws IOException {
        final int length = values.length;
        int ptr;

        if ( (start >= 0) && ( (start + length) <= dataSize)) {

            try {
                setLock(ModelStorageBase.RW_LOCKED);
                ptr = start;

                for (int i = 0; i < length; i++, ptr++) {
                    data.setBoolean(ptr, values[i]);
                }

                if (mmFlag) {
                    calcMinMax();
                }
            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Import data error: bounds incorrect");
    }

    /**
     * Import byte data into data array.
     * 
     * @param start indicates starting position in data array
     * @param values array where data is to be acquired
     * @param mmFlag whether or not to calculate min and max values for the image array
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void importData(final int start, final byte[] values, final boolean mmFlag)
            throws IOException {
        final int length = values.length;
        int ptr;

        if ( (start >= 0) && ( (start + length) <= dataSize)) {

            try {
                setLock(ModelStorageBase.RW_LOCKED);
                ptr = start;

                for (int i = 0; i < length; i++, ptr++) {
                    data.setByte(ptr, values[i]);
                }

                if (mmFlag) {
                    calcMinMax();
                }
            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Import data error: bounds incorrect");
    }

    /**
     * import short data into data array.
     * 
     * @param start indicates starting position in data array
     * @param values array where data is to be acquired
     * @param mmFlag whether or not to calculate min and max values for the image array
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void importData(final int start, final short[] values, final boolean mmFlag)
            throws IOException {

        final int length = values.length;
        int ptr;

        if ( (start >= 0) && ( (start + length) <= dataSize)) {

            try {
                setLock(ModelStorageBase.RW_LOCKED);
                ptr = start;

                for (int i = 0; i < length; i++, ptr++) {
                    data.setShort(ptr, values[i]);
                }

                if (mmFlag) {
                    calcMinMax();
                }
            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Import data error: bounds incorrect");
    }

    /**
     * import integer data into data array.
     * 
     * @param start indicates starting position in data array
     * @param values array where data is to be acquired
     * @param mmFlag whether or not to calculate min and max values for the image array
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void importData(final int start, final int[] values, final boolean mmFlag)
            throws IOException {
        final int length = values.length;
        int ptr;

        if ( (start >= 0) && ( (start + length) <= dataSize)) {

            try {
                setLock(ModelStorageBase.RW_LOCKED);
                ptr = start;

                for (int i = 0; i < length; i++, ptr++) {
                    data.setInt(ptr, values[i]);
                }

                if (mmFlag) {
                    calcMinMax();
                }

            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Import data error: bounds incorrect");
    }

    /**
     * import long data into data array.
     * 
     * @param start indicates starting position in data array
     * @param values array where data is to be acquired
     * @param mmFlag whether or not to calculate min and max values for the image array
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void importData(final int start, final long[] values, final boolean mmFlag)
            throws IOException {
        final int length = values.length;
        int ptr;

        if ( (start >= 0) && ( (start + length) <= dataSize)) {

            try {
                setLock(ModelStorageBase.RW_LOCKED);
                ptr = start;

                for (int i = 0; i < length; i++, ptr++) {
                    data.setLong(ptr, values[i]);
                }

                if (mmFlag) {
                    calcMinMax();
                }
            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Import data error: bounds incorrect");
    }

    /**
     * import float data into data array.
     * 
     * @param start indicates starting position in data array
     * @param values array where data is to be acquired
     * @param mmFlag whether or not to calculate min and max values for the image array
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void importData(final int start, final float[] values, final boolean mmFlag)
            throws IOException {
        final int length = values.length;
        int ptr;

        if ( (start >= 0) && ( (start + length) <= dataSize)) {

            try {
                setLock(ModelStorageBase.RW_LOCKED);
                ptr = start;

                for (int i = 0; i < length; i++, ptr++) {
                    data.setFloat(ptr, values[i]);
                }

                if (mmFlag) {
                    calcMinMax();
                }
            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Import data error: bounds incorrect");
    }

    /**
     * import double data into data array.
     * 
     * @param start indicates starting position in data array
     * @param values array where data is to be acquired
     * @param mmFlag whether or not to calculate min and max values for the image array
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void importData(final int start, final double[] values, final boolean mmFlag)
            throws IOException {
        final int length = values.length;
        int ptr;

        if ( (start >= 0) && ( (start + length) <= dataSize)) {

            try {
                setLock(ModelStorageBase.RW_LOCKED);
                ptr = start;

                for (int i = 0; i < length; i++, ptr++) {
                    data.setDouble(ptr, values[i]);
                }

                if (mmFlag) {
                    calcMinMax();
                }
            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Import data error: bounds incorrect");
    }

    /**
     * import Complex data (in 2 double units) into data array.
     * 
     * @param start indicates starting position in data array
     * @param valuesR array where real data is to be acquired
     * @param valuesI array where imaginary data is to be acquired
     * @param mmFlag whether or not to calculate min and max magnitude values for the image
     * @param logMagDisplay whether or not min and max are calculated for log10 of 1 + magnitude array
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void importDComplexData(final int start, final double[] valuesR, final double[] valuesI,
            final boolean mmFlag, final boolean logMagDisplay) throws IOException {
        final int length = valuesR.length;
        final int lengthi = valuesI.length;
        int ptr;

        if ( (length == lengthi) && (start >= 0) && ( (start + (2 * length)) <= dataSize)) {

            try {
                setLock(ModelStorageBase.RW_LOCKED);
                ptr = start;

                for (int i = 0; i < length; i++, ptr += 2) {
                    data.setDouble(ptr, valuesR[i]);
                    data.setDouble(ptr + 1, valuesI[i]);
                }

                setLogMagDisplay(logMagDisplay);

                if (mmFlag) {
                    calcMinMaxMag(logMagDisplay);
                }
            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Import data error: bounds incorrect");
    }

    /**
     * Import byte data into data array.
     * 
     * @param color color planes are interleaved, so color offsets the appropriate interleave interval
     * @param alphaIndexStart indicates starting position in data array which points to the alpha value
     * @param values array where data is to be acquired
     * @param mmFlag whether or not to calculate min and max values for the image array
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void importRGBData(final int color, final int alphaIndexStart, final byte[] values,
            final boolean mmFlag) throws IOException {
        final int length = values.length;
        int ptr;

        if ( (bufferType != DataType.ARGB) && (bufferType != DataType.ARGB_USHORT)
                && (bufferType != DataType.ARGB_FLOAT)) { // not
            // a
            // color
            // image
            return; // so don't do anything
        }

        if ( (alphaIndexStart >= 0) && ( (alphaIndexStart + length) <= dataSize)) {

            try {
                setLock(ModelStorageBase.RW_LOCKED);
                ptr = alphaIndexStart + color;

                for (int i = 0; i < length; i++, ptr += 4) {
                    data.setByte(ptr, values[i]);
                }

                if (mmFlag) {
                    calcMinMax();
                }
            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Import data error: bounds incorrect");
    }
    
    /**
     * Import byte data into data array.
     * 
     * @param color color planes are interleaved, so color offsets the appropriate interleave interval
     * @param alphaIndexStart indicates starting position in data array which points to the alpha value
     * @param values array where data is to be acquired
     * @param mmFlag whether or not to calculate min and max values for the image array
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void importRGBData(final int color, final int alphaIndexStart, final short[] values,
            final boolean mmFlag) throws IOException {
        final int length = values.length;
        int ptr;

        if ( (bufferType != DataType.ARGB) && (bufferType != DataType.ARGB_USHORT)
                && (bufferType != DataType.ARGB_FLOAT)) { // not
            // a
            // color
            // image
            return; // so don't do anything
        }

        if ( (alphaIndexStart >= 0) && ( (alphaIndexStart + length) <= dataSize)) {

            try {
                setLock(ModelStorageBase.RW_LOCKED);
                ptr = alphaIndexStart + color;

                for (int i = 0; i < length; i++, ptr += 4) {
                    data.setShort(ptr, values[i]);
                }

                if (mmFlag) {
                    calcMinMax();
                }
            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Import data error: bounds incorrect");
    }

    /**
     * import float data into data array.
     * 
     * @param color color planes are interleaved, so color offsets the appropriate interleave interval
     * @param alphaIndexStart indicates starting position in data array which points to the alpha value
     * @param values array where data is to be acquired
     * @param mmFlag whether or not to calculate min and max values for the image array
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void importRGBData(final int color, final int alphaIndexStart, final float[] values,
            final boolean mmFlag) throws IOException {
        final int length = values.length;
        int ptr;

        if ( (bufferType != DataType.ARGB) && (bufferType != DataType.ARGB_USHORT)
                && (bufferType != DataType.ARGB_FLOAT)) { // not
            // a
            // color
            // image
            return; // so don't do anything
        }

        if ( (alphaIndexStart >= 0) && ( (alphaIndexStart + length) <= dataSize)) {

            try {
                setLock(ModelStorageBase.RW_LOCKED);
                ptr = alphaIndexStart + color;

                for (int i = 0; i < length; i++, ptr += 4) {
                    data.setFloat(ptr, values[i]);
                }

                if (mmFlag) {
                    calcMinMax();
                }
            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Import data error: bounds incorrect");
    }

    /**
     * import short data into data array.
     * 
     * @param start indicates starting position in data array
     * @param values array where data is to be acquired
     * @param mmFlag whether or not to calculate min and max values for the image array
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void importUData(final int start, final short[] values, final boolean mmFlag)
            throws IOException {
        final int length = values.length;
        int ptr;

        if ( (start >= 0) && ( (start + length) <= dataSize)) {

            try {
                setLock(ModelStorageBase.RW_LOCKED);
                ptr = start;

                for (int i = 0; i < length; i++, ptr++) {
                    data.setUByte(ptr, values[i]);
                }

                if (mmFlag) {
                    calcMinMax();
                }

            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Import data error: bounds incorrect");
    }

    /**
     * import int data into data array.
     * 
     * @param start indicates starting position in data array
     * @param values array where data is to be acquired
     * @param mmFlag whether or not to calculate min and max values for the image array
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void importUData(final int start, final int[] values, final boolean mmFlag)
            throws IOException {
        final int length = values.length;
        int ptr;

        if ( (start >= 0) && ( (start + length) <= dataSize)) {

            try {
                setLock(ModelStorageBase.RW_LOCKED);
                ptr = start;

                for (int i = 0; i < length; i++, ptr++) {
                    data.setUShort(ptr, values[i]);
                }

                if (mmFlag) {
                    calcMinMax();
                }
            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Import data error: bounds incorrect");
    }

    /**
     * import int data into data array.
     * 
     * @param start indicates starting position in data array
     * @param values array where data is to be acquired
     * @param mmFlag whether or not to calculate min and max values for the image array
     * 
     * @throws IOException Throws an error when there is a locking or bounds error.
     */
    public final synchronized void importUData(final int start, final long[] values, final boolean mmFlag)
            throws IOException {
        final int length = values.length;
        int ptr;

        if ( (start >= 0) && ( (start + length) <= dataSize)) {

            try {
                setLock(ModelStorageBase.RW_LOCKED);
                ptr = start;

                for (int i = 0; i < length; i++, ptr++) {
                    data.setUInt(ptr, values[i]);
                }

                if (mmFlag) {
                    calcMinMax();
                }

            } catch (final IOException error) {
                throw error;
            } finally {
                releaseLock();
            }

            return;
        }

        throw new IOException("Import data error: bounds incorrect");
    }

    /**
     * Disposes of old data and constructs a new buffer of the user specific type if the image in NOT locked.
     * 
     * @param type type of new buffer
     * 
     * @exception IOException throws an exception if the image model is locked.
     */
    public synchronized void reallocate(final int type) throws IOException {

        if (lockStatus == ModelStorageBase.UNLOCKED) {

            // disposeLocal(); // delete old memory and reallocate
            data = null;
            System.gc();
            construct(DataType.getDataType(type), dimExtents);
        } else {
            throw new IOException("ModelStorageBase: Reallocate: Image locked !");
        }
    }

    /**
     * Disposes of old data and constructs a new buffer of the user specific type if the image in NOT locked.
     * 
     * @param dimExtents extents of the buffer in each dimension (multipleid together produces the size of the buffer to
     *            be allocated
     * 
     * @exception IOException throws an exception if the image model is locked.
     */
    public synchronized void reallocate(final int[] dimExtents) throws IOException {

        if (lockStatus == ModelStorageBase.UNLOCKED) {

            // disposeLocal(); // delete old memory and reallocate
            data = null;
            System.gc();
            construct(bufferType, dimExtents);
        } else {
            throw new IOException("ModelStorageBase: Reallocate: Image locked !");
        }
    }

    /**
     * Disposes of old data and constructs a new buffer of the user specific type if the image in NOT locked.
     * 
     * @param type type of new buffer
     * @param dimExtents extents of the buffer in each dimension (multiplied together produces the size of the buffer to
     *            be allocated
     * 
     * @exception IOException throws an exception if the image model is locked.
     */
    public synchronized void reallocate(final int type, final int[] dimExtents) throws IOException {

        if (lockStatus == ModelStorageBase.UNLOCKED) {

            // disposeLocal(); // delete old memory and reallocate
            data = null;
            System.gc();
            construct(DataType.getDataType(type), dimExtents);
        } else {
            throw new IOException("ModelStorageBase: Reallocate: Image locked !");
        }
    }

    /**
     * Recomputes the datasize based on the type of buffer. If the datasize has changed, then the data array needs to be
     * reallocated. So this method will also reconstruct the data array in this case. This method must be called if the
     * extents of an buffer have been changed.
     * 
     * <p>
     * WARNING: This will clear any existing data if the dataSize is changed.
     * </p>
     */
    public void recomputeDataSize() {
        final int oldDataSize = this.dataSize;

        this.computeDataSize();

        if (this.dataSize != oldDataSize) {
            allocateData();
        }

    } // end recomputeDataSize()

    /**
     * Releases the lock so that other proceses can read or write the data.
     */
    public final synchronized void releaseLock() {

        if (lockStatus == ModelStorageBase.W_LOCKED) {
            writeLockCount--;

            if (writeLockCount <= 0) {
                lockStatus = ModelStorageBase.UNLOCKED;
                writeLockCount = 0; // Just make sure its zero.
            }
        } else if (lockStatus == ModelStorageBase.RW_LOCKED) {
            lockStatus = ModelStorageBase.UNLOCKED;
            writeLockCount = 0; // Just make sure its zero.
        } else {
            writeLockCount = 0; // Just make sure its zero.
        }

    }

    /**
     * Removes the surface mask from this image.
     * 
     * @param index The index of the mask to remove.
     */
    public void removeSurfaceMask(final int index) {

        if (m_kColorVector.size() > index) {
            m_kColorVector.removeElementAt(index);
        }

        if (m_kMaskVector.size() > index) {
            m_kMaskVector.removeElementAt(index);
        }

        if (m_kMaskColorVector.size() > index) {
            m_kMaskColorVector.removeElementAt(index);
        }
    }

    /**
     * Returns the array of BitSet masks for backup.
     * 
     * @return the BitSet mask array.
     */
    public BitSet[] removeSurfaceMasks() {
        final BitSet[] masks = new BitSet[m_kMaskVector.size()];

        for (int i = 0; i < m_kMaskVector.size(); i++) {
            masks[i] = (BitSet) m_kMaskVector.get(i);
        }

        m_kMaskVector.removeAllElements();

        return masks;
    }

    /**
     * Restores the mask list from the array.
     * 
     * @param masks array of BitSet masks
     */
    public void restoreSurfaceMasks(final BitSet[] masks) {
        m_kMaskVector.removeAllElements();

        for (int i = 0; i < masks.length; i++) {
            m_kMaskVector.insertElementAt(masks[i], i);
        }
    }

    /**
     * Set that does perform bounds checking.
     * 
     * @param position position in one dimensional array
     * @param value The value to stored in the data array.
     * 
     * @return returns value if position in data array range
     */
    public final boolean set(final int position, final Number value) {

        if ( (position >= 0) && (position < dataSize)) {
            data.set(position, value);

            return true;
        }

        return false;
    }

    /**
     * nD set data fuction where bounds checking is performed.
     * 
     * @param position The coordinate into the 1D data array
     * @param value The value to stored in the data array.
     * 
     * @return returns true if position in data array range
     */
    public final boolean set(final int[] position, final Number value) {
        int i;
        int j;
        int location = 0;
        int prod;
        final int dimensions = position.length;

        if (nDims == dimensions) {

            for (i = dimensions - 1; i > 0; i--) {
                prod = dimExtents[0];
                for (j = 1; j <= i-1; j++) {
                    prod *= dimExtents[j];
                }
                location += (position[i] * prod);
            }

            location += position[0];

            if ( (location >= 0) && (location < dataSize)) {
                data.set(location, value);

                return true;
            }

            return false;
        }

        return false;
    }

    /**
     * version of set that does NOT perform bounds checking
     * 
     * @param position position in one dimensional array.
     * 
     * @param position The index into the data array.
     * @param value The value to stored in the data array.
     */
    public final void set(final int position, final boolean value) {
        data.setBoolean(position, value);
    }

    /**
     * nD set data fuction where bounds checking is NOT performed.
     * 
     * @param position array of coordinate values
     * @param value The value to stored in the data array.
     */
    public final void set(final int[] position, final boolean value) {
        int i;
        int j;
        int prod;
        int location = 0;
        final int dimensions = position.length;
        
        for (i = dimensions - 1; i > 0; i--) {
            prod = dimExtents[0];
            for (j = 1; j <= i-1; j++) {
                prod *= dimExtents[j];
            }
            location += (position[i] * prod);
        }

        location += position[0];
        data.setBoolean(location, value);
    }

    /**
     * Version of set that does NOT perform bounds checking.
     * 
     * @param position The position in one dimensional array
     * @param value The value to stored in the data array.
     */
    public final void set(final int position, final byte value) {
        data.setByte(position, value);
    }

    /**
     * nD set data fuction where bounds checking is NOT performed.
     * 
     * @param position array of coordinate values
     * @param value data that will be stored in the data array
     */
    public final void set(final int[] position, final byte value) {
        int i;
        int j;
        int prod;
        int location = 0;
        final int dimensions = position.length;
        
        for (i = dimensions - 1; i > 0; i--) {
            prod = dimExtents[0];
            for (j = 1; j <= i-1; j++) {
                prod *= dimExtents[j];
            }
            location += (position[i] * prod);
        }

        location += position[0];
        data.setByte(location, value);
    }

    /**
     * nD set data fuction where bounds checking is NOT performed.
     * 
     * @param position array of coordinate values
     * @param value data that will be stored in the data array
     */
    public final void set(final int[] position, final short value) {
        int i;
        int j;
        int prod;
        int location = 0;
        final int dimensions = position.length;
        
        for (i = dimensions - 1; i > 0; i--) {
            prod = dimExtents[0];
            for (j = 1; j <= i-1; j++) {
                prod *= dimExtents[j];
            }
            location += (position[i] * prod);
        }

        location += position[0];
        data.setShort(location, value);
    }

    /**
     * Version of set that does NOT perform bounds checking.
     * 
     * @param position position in one dimensional array
     * @param value The value to stored in the data array.
     */
    public final void set(final int position, final int value) {
        data.setInt(position, value);
    }

    /**
     * nD set data fuction where bounds checking is NOT performed.
     * 
     * @param position array of coordinate values
     * @param value data that will be stored in the data array
     */
    public final void set(final int[] position, final int value) {
        int i;
        int j;
        int prod;
        int location = 0;
        final int dimensions = position.length;
        
        for (i = dimensions - 1; i > 0; i--) {
            prod = dimExtents[0];
            for (j = 1; j <= i-1; j++) {
                prod *= dimExtents[j];
            }
            location += (position[i] * prod);
        }

        location += position[0];
        data.setInt(location, value);
    }

    /**
     * version of set that does NOT perform bounds checking.
     * 
     * @param position position in one dimensional array
     * @param value The value to stored in the data array.
     */
    public final void set(final int position, final long value) {
        data.setLong(position, value);
    }

    /**
     * nD set data fuction where bounds checking is NOT performed.
     * 
     * @param position array of coordinate values
     * @param value data that will be stored in the data array
     */
    public final void set(final int[] position, final long value) {
        int i;
        int j;
        int prod;
        int location = 0;
        final int dimensions = position.length;
        
        for (i = dimensions - 1; i > 0; i--) {
            prod = dimExtents[0];
            for (j = 1; j <= i-1; j++) {
                prod *= dimExtents[j];
            }
            location += (position[i] * prod);
        }

        location += position[0];
        data.setLong(location, value);
    }

    /**
     * version of set that does NOT perform bounds checking.
     * 
     * @param position position in one dimensional array
     * @param value data that will be stored in the data array
     */
    public final void set(final int position, final float value) {
        data.setFloat(position, value);
    }

    /**
     * nD set data fuction where bounds checking is NOT performed.
     * 
     * @param position array of coordinate values
     * @param value data that will be stored in the data array
     */
    public final void set(final int[] position, final float value) {
        int i;
        int j;
        int prod;
        int location = 0;
        final int dimensions = position.length;
        
        for (i = dimensions - 1; i > 0; i--) {
            prod = dimExtents[0];
            for (j = 1; j <= i-1; j++) {
                prod *= dimExtents[j];
            }
            location += (position[i] * prod);
        }

        location += position[0];
        data.setFloat(location, value);
    }

    /**
     * version of set that does NOT perform bounds checking.
     * 
     * @param position position in one dimensional array
     * @param value The value to stored in the data array.
     */
    public final void set(final int position, final double value) {
        data.setDouble(position, value);
    }

    /**
     * nD set data fuction where bounds checking is NOT performed.
     * 
     * @param position array of coordinate values
     * @param value data that will be stored in the data array
     */
    public final void set(final int[] position, final double value) {
        int i;
        int j;
        int prod;
        int location = 0;
        final int dimensions = position.length;
        
        for (i = dimensions - 1; i > 0; i--) {
            prod = dimExtents[0];
            for (j = 1; j <= i-1; j++) {
                prod *= dimExtents[j];
            }
            location += (position[i] * prod);
        }

        location += position[0];
        data.setDouble(location, value);
    }

    /**
     * 2D get data fuction where bounds checking is performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param value set data array to this value
     * 
     * @return returns true if position in data array range
     */
    public final boolean set(final int x, final int y, final Number value) {
        int position;

        if (nDims == 2) {
            position = (y * dimExtents[0]) + x;

            if ( (position >= 0) && (position < dataSize)) {
                data.set(position, value);

                return true;
            }

            return false;
        }

        return false;
    }

    /**
     * 2D fast version of set that does NOT perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param value The value to stored in the data array.
     */
    public final void set(final int x, final int y, final boolean value) {
        data.setBoolean( (y * dimExtents[0]) + x, value);
    }

    /**
     * 2D fast version of set that does NOT perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param value The value to stored in the data array.
     */
    public final void set(final int x, final int y, final byte value) {
        data.setByte( (y * dimExtents[0]) + x, value);
    }

    /**
     * 2D fast version of set that does NOT perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param value The value to stored in the data array.
     */
    public final void set(final int x, final int y, final int value) {
        data.setInt( (y * dimExtents[0]) + x, value);
    }

    /**
     * 2D fast version of set that does NOT perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param value The value to stored in the data array.
     */
    public final void set(final int x, final int y, final long value) {
        data.setLong( (y * dimExtents[0]) + x, value);
    }

    /**
     * 2D fast version of set that does NOT perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param value The value to stored in the data array.
     */
    public final void set(final int x, final int y, final float value) {
        data.setFloat( (y * dimExtents[0]) + x, value);
    }

    /**
     * 2D fast version of set that does NOT perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param value The value to stored in the data array.
     */
    public final void set(final int x, final int y, final double value) {
        data.setDouble( (y * dimExtents[0]) + x, value);
    }

    /**
     * 3D set data fuction where bounds checking is performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param value set data array to this value
     * 
     * @return returns true if position in data array range
     */
    public final boolean set(final int x, final int y, final int z, final Number value) {
        int position;

        if (nDims == 3) {
            position = (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x;

            if ( (position >= 0) && (position < dataSize)) {
                data.set(position, value);

                return true;
            }

            return false;
        }

        return false;
    }

    /**
     * 3D fast version of set that does NOT perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param value The value to stored in the data array.
     */
    public final void set(final int x, final int y, final int z, final boolean value) {
        data.setBoolean( (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x, value);
    }

    /**
     * 3D fast version of set that does NOT perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param value The value to stored in the data array.
     */
    public final void set(final int x, final int y, final int z, final byte value) {
        data.setByte( (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x, value);
    }

    /**
     * 3D fast version of set that does NOT perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param value The value to stored in the data array.
     */
    public final void set(final int x, final int y, final int z, final short value) {
        data.setShort( (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x, value);
    }

    /**
     * 3D fast version of set that does NOT perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param value The value to stored in the data array.
     */
    public final void set(final int x, final int y, final int z, final int value) {
        data.setInt( (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x, value);
    }

    /**
     * 3D fast version of set that does NOT perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param value The value to stored in the data array.
     */
    public final void set(final int x, final int y, final int z, final long value) {
        data.setLong( (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x, value);
    }

    /**
     * 3D fast version of set that does NOT perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param value The value to stored in the data array.
     */
    public final void set(final int x, final int y, final int z, final float value) {
        data.setFloat( (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x, value);
    }

    /**
     * 3D fast version of set that does NOT perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param value The value to stored in the data array.
     */
    public final void set(final int x, final int y, final int z, final double value) {
        data.setDouble( (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x, value);
    }

    /**
     * 4D get data fuction where bounds checking is performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param b b coordinate (ie. multi modality images or time)
     * @param value The value to stored in the data array.
     * 
     * @return returns true if position in data array range
     */
    public final boolean set(final int x, final int y, final int z, final int b, final Number value) {
        int position;

        if (nDims == 4) {
            position = (b * (dimExtents[0] * dimExtents[1] * dimExtents[2])) + (z * (dimExtents[0] * dimExtents[1]))
                    + (y * dimExtents[0]) + x;

            if ( (position >= 0) && (position < dataSize)) {
                data.set(position, value);

                return true;
            }

            return false;
        }

        return false;
    }

    /**
     * 4D fast version of set that does NOT perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param b b coordinate
     * @param value set data array to this value
     */
    public final void set(final int x, final int y, final int z, final int b, final boolean value) {
        data.setBoolean( (b * (dimExtents[0] * dimExtents[1] * dimExtents[2])) + (z * (dimExtents[0] * dimExtents[1]))
                + (y * dimExtents[0]) + x, value);
    }

    /**
     * 4D fast version of set that does NOT perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param b b coordinate
     * @param value set data array to this value
     */
    public final void set(final int x, final int y, final int z, final int b, final byte value) {
        data.setByte( (b * (dimExtents[0] * dimExtents[1] * dimExtents[2])) + (z * (dimExtents[0] * dimExtents[1]))
                + (y * dimExtents[0]) + x, value);
    }

    /**
     * 4D fast version of set that does NOT perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param b b coordinate
     * @param value set data array to this value
     */
    public final void set(final int x, final int y, final int z, final int b, final short value) {
        data.setShort( (b * (dimExtents[0] * dimExtents[1] * dimExtents[2])) + (z * (dimExtents[0] * dimExtents[1]))
                + (y * dimExtents[0]) + x, value);
    }

    /**
     * 4D fast version of set that does NOT perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param b b coordinate
     * @param value set data array to this value
     */
    public final void set(final int x, final int y, final int z, final int b, final int value) {
        data.setInt( (b * (dimExtents[0] * dimExtents[1] * dimExtents[2])) + (z * (dimExtents[0] * dimExtents[1]))
                + (y * dimExtents[0]) + x, value);
    }

    /**
     * 4D fast version of set that does NOT perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param b b coordinate
     * @param value set data array to this value
     */
    public final void set(final int x, final int y, final int z, final int b, final long value) {
        data.setLong( (b * (dimExtents[0] * dimExtents[1] * dimExtents[2])) + (z * (dimExtents[0] * dimExtents[1]))
                + (y * dimExtents[0]) + x, value);
    }

    /**
     * 4D fast version of set that does NOT perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param b b coordinate
     * @param value set data array to this value
     */
    public final void set(final int x, final int y, final int z, final int b, final float value) {
        data.setFloat( (b * (dimExtents[0] * dimExtents[1] * dimExtents[2])) + (z * (dimExtents[0] * dimExtents[1]))
                + (y * dimExtents[0]) + x, value);
    }

    /**
     * 4D fast version of set that does NOT perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param b b coordinate
     * @param value set data array to this value
     */
    public final void set(final int x, final int y, final int z, final int b, final double value) {
        data.setDouble( (b * (dimExtents[0] * dimExtents[1] * dimExtents[2])) + (z * (dimExtents[0] * dimExtents[1]))
                + (y * dimExtents[0]) + x, value);
    }

    /**
     * Sets the entire buffer to value passed to this method.
     * 
     * @param value float data that will be stored in the data array
     */
    public final void setAll(final float value) {

        for (int i = 0; i < data.length(); i++) {
            data.setFloat(i, value);
        }

    }

    /**
     * Sets the entire buffer to value passed to this method.
     * 
     * @param value byte data that will be stored in the data array
     */

    public final void setAll(final byte value) {

        for (int i = 0; i < data.length(); i++) {
            data.setByte(i, value);
        }

    }

    /**
     * Sets the entire buffer to value passed to this method.
     * 
     * @param value short data that will be stored in the data array
     */

    public final void setAll(final short value) {

        for (int i = 0; i < data.length(); i++) {
            data.setShort(i, value);
        }

    }

    /**
     * Sets the entire buffer to value passed to this method.
     * 
     * @param value int data that will be stored in the data array
     */

    public final void setAll(final int value) {

        for (int i = 0; i < data.length(); i++) {
            data.setInt(i, value);
        }

    }

    /**
     * Sets the entire buffer to value passed to this method.
     * 
     * @param value double data that will be stored in the data array
     */

    public final void setAll(final double value) {

        for (int i = 0; i < data.length(); i++) {
            data.setDouble(i, value);
        }

    }

    /**
     * Sets the entire buffer to value passed to this method.
     * 
     * @param value UByte data that will be stored in the data array
     */

    public final void setAllUByte(final short value) {

        for (int i = 0; i < data.length(); i++) {
            data.setUByte(i, value);
        }

    }

    /**
     * Sets the entire buffer to value passed to this method.
     * 
     * @param value UShort data that will be stored in the data array
     */

    public final void setAllUShort(final int value) {

        for (int i = 0; i < data.length(); i++) {
            data.setUShort(i, value);
        }

    }

    /**
     * Sets the entire buffer to value passed to this method.
     * 
     * @param position position in one dimensional array
     * @param color The color channel to be modified
     * @param value set data array to this value
     * 
     * @return returns value if position in data array range
     */
    public final boolean setC(final int position, final int color, final Number value) {

        if ( (position >= 0) && ( ( (4 * position) + color) < dataSize)) {
            data.set( ( (4 * position) + color), value);

            return true;
        }

        return false;
    }

    /**
     * color version of setC that does NOT perform bounds checking.
     * 
     * @param position position in one dimensional array
     * @param c The color channel to be modified
     * @param value set data array to this value
     */
    public final void setC(final int position, final int c, final float value) {
        data.setFloat( (4 * position) + c, value);
    }

    /**
     * 2D fast color version of set that does NOT perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param c The color channel to be modified
     * @param value set data array to this value
     */
    public final void setC(final int x, final int y, final int c, final float value) {
        data.setFloat( (4 * ( (y * dimExtents[0]) + x)) + c, value);
    }

    /**
     * 3D color set data fuction where bounds checking is performed.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param c A,R,G, or B
     * @param value set data array to this value
     * 
     * @return returns true if position in data array range
     */
    public final boolean setC(final int x, final int y, final int z, final int c, final Number value) {
        int position;

        if (nDims == 3) {
            position = (4 * ( (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x)) + c;

            if ( (position >= 0) && (position < dataSize)) {
                data.set(position, value);

                return true;
            }

            return false;
        }

        return false;
    }

    /**
     * 3D fast color version of set that does NOT perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param c The color channel to be modified
     * @param value set data array to this value
     */
    public final void setC(final int x, final int y, final int z, final int c, final float value) {
        data.setFloat( (4 * ( (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x)) + c, value);
    }
    
    /**
     * 
     * @param x
     * @param y
     * @param z
     * @param part 0 for real 1 for imaginary
     * @param value
     */
    public final void setComplex(final int x, final int y, final int z, final int part, final float value) {
    	data.setFloat( (2 * ( (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x)) + part, value);	
    }

    /**
     * Accessor method for the m_bConvolve data memeber.
     * 
     * @param bConvolve true when this images is the product of fft( imageA ) fft( imageB )
     */
    public void setConvolve(final boolean bConvolve) {
        m_bConvolve = bConvolve;
    }

    /**
     * Sets the dimExtents for this structure.
     * 
     * @param dims The data's dimensionality.
     */
    public void setExtents(final int[] dims) {
        this.dimExtents = dims.clone();
        this.nDims = dims.length;
    } // end setExtents()

    /**
     * Accessor that sets the entire fileInfo array of the image.
     * 
     * @param fInfo structure
     */
    public void setFileInfo(final FileInfoBase[] fInfo) {

        if ( (fInfo != null) && (fileInfo != fInfo)) {
            // for(int i=0;i<fileInfo.length;i++) {
            // fileInfo[i].finalize();
            // }
            fileInfo = fInfo;
        }
    }

    /**
     * Accessor that sets the fileInfo class for the image.
     * 
     * @param fInfo fileInfo structure.
     * @param idx index that typically indicates image slice.
     */
    public void setFileInfo(final FileInfoBase fInfo, final int idx) {
        // if(fileInfo == null) {
        // return;
        // }
        // if((fileInfo[idx] != null) && (fileInfo[idx] != fInfo)) {
        // fileInfo[idx].finalize();
        // }
        fileInfo[idx] = fInfo;
    }

    /**
     * DOCUMENT ME! TO BE MOVED
     * 
     * @param _filterType int
     */
    public void setFilterType(final int _filterType) {
        filterType = _filterType;
    }

    /**
     * DOCUMENT ME! TO BE MOVED
     * 
     * @param _freq1 float
     */
    public void setFreq1(final float _freq1) {
        freq1 = _freq1;
    }

    /**
     * DOCUMENT ME! TO BE MOVED
     * 
     * @param _freq2 float
     */
    public void setFreq2(final float _freq2) {
        freq2 = _freq2;
    }

    /**
     * DOCUMENT ME! TO BE MOVED
     * 
     * @param _freqU float
     */
    public void setFreqU(final float _freqU) {
        freqU = _freqU;
    }

    /**
     * DOCUMENT ME! TO BE MOVED
     * 
     * @param _freqV float
     */
    public void setFreqV(final float _freqV) {
        freqV = _freqV;
    }

    /**
     * DOCUMENT ME! TO BE MOVED
     * 
     * @param window boolean
     */
    public void setHaveWindowed(final boolean window) {
        haveWindowed = window;
    }

    /**
     * Accessor that sets the boolean telling if 3D images are processed one slice at a time.
     * 
     * @param _image25D Set true if images are processed one slice at a time.
     */
    public void setImage25D(final boolean _image25D) {
        image25D = _image25D;
    }

    /**
     * Sets the lockFlag to protect data. When the flag is set no other processes can read or write the data
     * 
     * @throws IOException Throws an error if image is already locked.
     */
    public void setLock() throws IOException {

        if (lockStatus == ModelStorageBase.UNLOCKED) {
            lockStatus = ModelStorageBase.RW_LOCKED;
        } else {
            throw new IOException("ModelStorageBase: Image locked !");
        }
    }

    /**
     * Sets the lockFlag to protect data. When the flag is set no other processes can read or write the data
     * 
     * @param lockType The type of read/write lock that is to be set.
     * 
     * @throws IOException Throws an error if image is already locked.
     */
    public final synchronized void setLock(final int lockType) throws IOException {

        if (lockStatus == ModelStorageBase.RW_LOCKED) {
            throw new IOException("ModelStorageBase: Image locked !");
        } else if ( (lockStatus == ModelStorageBase.UNLOCKED) && (lockType == ModelStorageBase.W_LOCKED)) {
            lockStatus = lockType;
        } else if ( (lockStatus == ModelStorageBase.W_LOCKED) && (lockType == ModelStorageBase.W_LOCKED)) {} else if ( (lockStatus == ModelStorageBase.W_LOCKED)
                && (lockType == ModelStorageBase.RW_LOCKED)) {
            throw new IOException("ModelStorageBase: Image locked !");
        } else if ( (lockStatus == ModelStorageBase.UNLOCKED) && (lockType == ModelStorageBase.RW_LOCKED)) {
            lockStatus = lockType;
        }
    }

    /**
     * Accessor that sets the boolean telling if log magnitude display is used in a complex image. TO BE MOVED
     * 
     * @param logMagDisplay DOCUMENT ME!
     */
    public void setLogMagDisplay(final boolean logMagDisplay) {
        logMagDisp = logMagDisplay;
    }

    /**
     * Accessor that sets the maximum value in the dataArray.
     * 
     * @param _max Indicates the maximum value in the data array
     */
    public void setMax(final double _max) {
        max = _max;
    }

    /**
     * Accessor that sets the minimum value in the dataArray.
     * 
     * @param _min Indicates the minimum value in the data array
     */
    public void setMin(final double _min) {
        min = _min;
    }

    /**
     * DOCUMENT ME! TO BE MOVED
     * 
     * @param butterworthOrder DOCUMENT ME!
     */
    public void setOriginalButterworthOrder(final int butterworthOrder) {
        originalButterworthOrder = butterworthOrder;
    }

    /**
     * DOCUMENT ME! TO BE MOVED
     * 
     * @param cropCheckbox boolean
     */
    public void setOriginalCropCheckbox(final boolean cropCheckbox) {
        originalCropCheckbox = cropCheckbox;
    }

    /**
     * DOCUMENT ME! TO BE MOVED
     * 
     * @param doCrop boolean
     */
    public void setOriginalDoCrop(final boolean doCrop) {
        originalDoCrop = doCrop;
    }

    /**
     * DOCUMENT ME! TO BE MOVED
     * 
     * @param end int[]
     */
    public void setOriginalEnd(final int[] end) {
        originalEnd = end;
    }

    /**
     * Sets original dimensionality of the images. TO BE MOVED
     * 
     * @param dims int[] dimensionality for x,y, and z ... dimensions
     */
    public void setOriginalExtents(final int[] dims) {
        dimOriginalExtents = dims;
    }

    /**
     * DOCUMENT ME! TO BE MOVED
     * 
     * @param filterConstruction DOCUMENT ME!
     */
    public void setOriginalFilterConstruction(final int filterConstruction) {
        originalFilterConstruction = filterConstruction;
    }

    /**
     * DOCUMENT ME! TO BE MOVED
     * 
     * @param kDim DOCUMENT ME!
     */
    public void setOriginalKernelDimension(final int kDim) {
        originalKernelDimension = kDim;
    }

    /**
     * DOCUMENT ME! TO BE MOVED
     * 
     * @param maximum float
     */
    public void setOriginalMaximum(final float maximum) {
        originalMaximum = maximum;
    }

    /**
     * DOCUMENT ME! TO BE MOVED
     * 
     * @param minimum float
     */
    public void setOriginalMinimum(final float minimum) {
        originalMinimum = minimum;
    }

    /**
     * DOCUMENT ME! TO BE MOVED
     * 
     * @param start int[]
     */
    public void setOriginalStart(final int[] start) {
        originalStart = start;
    }

    /**
     * Sets the radiological view flag.
     * 
     * @param bEnabled when true the radiological view is displayed, when false the neurological view is displayed.
     */
    public void setRadiologicalView(final boolean bEnabled) {
        m_bRadiologicalView = bEnabled;
    }

    /**
     * Sets the resolutions to the specific value for a specific slice indicated by the index parameter.
     * 
     * @param index index that typically indicates image slice.
     * @param resolutions the image's voxel resolutions
     */
    public void setResolutions(final int index, final float[] resolutions) {

        if (fileInfo == null) {
            return;
        }

        fileInfo[index].setResolutions(resolutions);
    }

    /**
     * Sets the resolutions to the specific value for a specific slice indicated by the index parameter.
     * 
     * @param resolutions the image's voxel resolutions
     */
    public void setResolutions(final float[] resolutions) {

        if (fileInfo == null) {
            return;
        }
        for ( int i = 0; i < fileInfo.length; i++ )
        {
        	if ( fileInfo[i] != null )
        	{
        		fileInfo[i].setResolutions(resolutions);
        	}
        }
    }

    /**
     * Sets the image voxel at the specified position to the specified value.
     * 
     * @param position position in one dimensional array.
     * @param value the new voxel value.
     */
    public final void setShort(final int position, final short value) {
        data.setShort(position, value);
    }

    /**
     * 2D fast version of set that does NOT perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param value the new voxel value.
     */
    public final void setShort(final int x, final int y, final short value) {
        data.setShort( (y * dimExtents[0]) + x, value);
    }

    /**
     * DOCUMENT ME! TO BE MOVED
     * 
     * @param _sigmaU float
     */
    public void setSigmaU(final float _sigmaU) {
        sigmaU = _sigmaU;
    }

    /**
     * DOCUMENT ME! TO BE MOVED
     * 
     * @param _sigmaV float
     */
    public void setSigmaV(final float _sigmaV) {
        sigmaV = _sigmaV;
    }

    /**
     * DOCUMENT ME! TO BE MOVED
     * 
     * @param _theta float
     */
    public void setTheta(final float _theta) {
        theta = _theta;
    }

    /**
     * Sets the type of data.
     * 
     * @param type the type of data
     */
    public final void setType(final int type) {
        bufferType = DataType.getDataType(type);
    }
    
    /**
     * Sets the type of data.
     * 
     * @param type the type of data
     */
    public final void setType(final DataType type) {
        bufferType = type;
    }

    /**
     * Sets the image voxel at the specified position to the specified value.
     * 
     * @param position position in one dimensional array
     * @param value the new voxel value.
     */
    public final void setUByte(final int position, final short value) {
        data.setUByte(position, value);
    }

    /**
     * Sets the image voxel at the specified position to the specified value. nD set data fuction where bounds checking
     * is NOT performed.
     * 
     * @param position array of coordinate values
     * @param value data that will be stored in the data array
     */
    public final void setUByte(final int[] position, final short value) {
        int i;
        int location = 0;
        final int dimensions = position.length;

        for (i = dimensions - 1; i > 0; i--) {
            location += (position[i] * dimExtents[i]);
        }

        location += position[0];
        data.setUByte(location, value);
    }

    /**
     * Sets the image voxel at the specified position to the specified value. 2D fast version of set that does NOT
     * perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param value the new voxel value.
     */
    public final void setUByte(final int x, final int y, final short value) {
        data.setUByte( (y * dimExtents[0]) + x, value);
    }

    /**
     * Sets the image voxel at the specified position to the specified value. 3D fast version of set that does NOT
     * perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param value the new voxel value.
     */
    public final void setUByte(final int x, final int y, final int z, final short value) {
        data.setUByte( (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x, value);
    }

    /**
     * Sets the image voxel at the specified position to the specified value. 4D fast version of set that does NOT
     * perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param b b coordinate
     * @param value the new voxel value.
     */
    public final void setUByte(final int x, final int y, final int z, final int b, final short value) {
        data.setUByte( (b * (dimExtents[0] * dimExtents[1] * dimExtents[2])) + (z * (dimExtents[0] * dimExtents[1]))
                + (y * dimExtents[0]) + x, value);
    }

    /**
     * Sets the image voxel at the specified position to the specified value. version of set that does NOT perform
     * bounds checking.
     * 
     * @param position position in one dimensional array
     * @param value the new voxel value.
     */
    public final void setUInt(final int position, final long value) {
        data.setUInt(position, value);
    }

    /**
     * Sets the image voxel at the specified position to the specified value. nD set data fuction where bounds checking
     * is NOT performed.
     * 
     * @param position array of coordinate values
     * @param value data that will be stored in the data array
     */
    public final void setUInt(final int[] position, final long value) {
        int i;
        int location = 0;
        final int dimensions = position.length;

        for (i = dimensions - 1; i > 0; i--) {
            location += (position[i] * dimExtents[i]);
        }

        location += position[0];
        data.setUInt(location, value);
    }

    /**
     * Sets the image voxel at the specified position to the specified value. 2D fast version of set that does NOT
     * perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param value the new voxel value.
     */
    public final void setUInt(final int x, final int y, final long value) {
        data.setUInt( (y * dimExtents[0]) + x, value);
    }

    /**
     * Sets the image voxel at the specified position to the specified value. 3D fast version of set that does NOT
     * perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param value the new voxel value.
     */
    public final void setUInt(final int x, final int y, final int z, final long value) {
        data.setUInt( (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x, value);
    }

    /**
     * Sets the image voxel at the specified position to the specified value. 4D fast version of set that does NOT
     * perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param t t coordinate
     * @param value the new voxel value.
     */
    public final void setUInt(final int x, final int y, final int z, final int t, final long value) {
        data.setUInt( (t * (dimExtents[0] * dimExtents[1] * dimExtents[2])) + (z * (dimExtents[0] * dimExtents[1]))
                + (y * dimExtents[0]) + x, value);
    }

    /**
     * Accessor that sets the boolean telling if unequal dimesnions are allowed in a complex image. TO BE MOVED
     * 
     * @param unequalDimension DOCUMENT ME!
     */
    public void setUnequalDim(final boolean unequalDimension) {
        unequalDim = unequalDimension;
    }

    /**
     * Sets the image voxel at the specified position to the specified value.
     * 
     * @param position position in one dimensional array
     * @param value The value to stored in the data array.
     */
    public final void setUShort(final int position, final int value) {
        data.setUShort(position, value);
    }

    /**
     * Sets the image voxel at the specified position to the specified value. nD set data fuction where bounds checking
     * is NOT performed.
     * 
     * @param position array of coordinate values
     * @param value data that will be stored in the data array
     */
    public final void setUShort(final int[] position, final int value) {
        int i;
        int location = 0;
        final int dimensions = position.length;

        for (i = dimensions - 1; i > 0; i--) {
            location += (position[i] * dimExtents[i]);
        }

        location += position[0];
        data.setUShort(location, value);
    }

    /**
     * Sets the image voxel at the specified position to the specified value. 2D fast version of set that does NOT
     * perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param value the new voxel value.
     */
    public final void setUShort(final int x, final int y, final int value) {
        data.setUShort( (y * dimExtents[0]) + x, value);
    }

    /**
     * Sets the image voxel at the specified position to the specified value. 3D fast version of set that does NOT
     * perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param value the new voxel value.
     */
    public final void setUShort(final int x, final int y, final int z, final int value) {
        data.setUShort( (z * (dimExtents[0] * dimExtents[1])) + (y * dimExtents[0]) + x, value);
    }

    /**
     * Sets the image voxel at the specified position to the specified value. 4D fast version of set that does NOT
     * perform bounds checking.
     * 
     * @param x x coordinate
     * @param y y coordinate
     * @param z z coordinate
     * @param b b coordinate
     * @param value the new voxel value.
     */
    public final void setUShort(final int x, final int y, final int z, final int b, final int value) {
        data.setUShort( (b * (dimExtents[0] * dimExtents[1] * dimExtents[2])) + (z * (dimExtents[0] * dimExtents[1]))
                + (y * dimExtents[0]) + x, value);
    }

    /**
     * Allocates data based on the data type and dataSize. Since this can be used to reallocate data, first set the data
     * to null and do some garbage collecting.
     */
    protected void allocateData() {
        int i;

        // check to see if data is already allocated
        if (this.data != null) {
            this.data = null;
            System.gc();
        }

        // allocate memory in the data buffer and do some initialization
        // based on dataSize
        switch (this.bufferType) {

            case BOOLEAN:

                // since the dataSize computed for boolean may not actually
                // be the number used for allocation of the buffer,
                // go ahead and recompute the boolean datasize manually
                for (this.dataSize = 1, i = 0; i < this.nDims; i++) {
                    this.dataSize *= this.dimExtents[i];
                    if (i == 1) {
                        this.dataSize = 64 * ( (this.dataSize + 63) >> 6);
                    }
                }

                try {
                    this.data = new BufferBoolean(this.dataSize);
                    // Reset dataSize to reflect change in buffer size
                    // because BitSet structure may increase buffer.
                    this.dataSize = ((BufferBoolean) (this.data)).dataArray.size();
                } catch (final OutOfMemoryError error) {
                    disposeLocal();
                    MipavUtil.displayError("ImageModel: Unable to allocate boolean data");
                    throw (error);
                }

                break;

            case BYTE:
                try {
                    this.data = new BufferByte(this.dataSize);
                } catch (final OutOfMemoryError error) {
                    disposeLocal();
                    MipavUtil.displayError("ImageModel: Unable to allocate byte data");
                    throw (error);
                }

                break;

            case UBYTE:
                try {
                    this.data = new BufferUByte(this.dataSize);
                } catch (final OutOfMemoryError error) {
                    disposeLocal();
                    MipavUtil.displayError("ImageModel: Unable to allocate unsigned byte data");
                    throw (error);
                }

                break;

            case SHORT:
                try {
                    this.data = new BufferShort(this.dataSize);
                } catch (final OutOfMemoryError error) {
                    disposeLocal();
                    MipavUtil.displayError("ImageModel: Unable to allocate short data");
                    throw (error);
                }

                break;

            case USHORT:
                try {
                    this.data = new BufferUShort(this.dataSize);
                } catch (final OutOfMemoryError error) {
                    disposeLocal();
                    MipavUtil.displayError("ImageModel: Unable to allocate unsigned short data");
                    throw (error);
                }

                break;

            case INTEGER:
                try {
                    this.data = new BufferInt(this.dataSize);
                } catch (final OutOfMemoryError error) {
                    disposeLocal();
                    MipavUtil.displayError("ImageModel: Unable to allocate integer data buffer");
                    throw (error);
                }

                break;

            case UINTEGER:
                try {
                    this.data = new BufferUInt(this.dataSize);
                } catch (final OutOfMemoryError error) {
                    disposeLocal();
                    MipavUtil.displayError("ImageModel: Unable to allocate unsigned integer data buffer");
                    throw (error);
                }

                break;

            case LONG:
                try {
                    this.data = new BufferLong(this.dataSize);
                } catch (final OutOfMemoryError error) {
                    disposeLocal();
                    MipavUtil.displayError("ImageModel: Unable to allocate long data buffer");
                    throw (error);
                }

                break;

            case FLOAT:
                try {
                    this.data = new BufferFloat(this.dataSize);
                } catch (final OutOfMemoryError error) {
                    disposeLocal();
                    MipavUtil.displayError("ImageModel: Unable to allocate float data buffer");
                    throw (error);
                }

                break;

            case COMPLEX:
                try {
                    this.data = new BufferFloat(this.dataSize);
                } catch (final OutOfMemoryError error) {
                    disposeLocal();
                    MipavUtil.displayError("ImageModel: Unable to allocate complex data buffer");
                    throw (error);
                }

                break;

            case DOUBLE:
                try {
                    this.data = new BufferDouble(this.dataSize);
                } catch (final OutOfMemoryError error) {
                    disposeLocal();
                    MipavUtil.displayError("ImageModel: Unable to allocate double data buffer");
                    throw (error);
                }

                break;

            case DCOMPLEX:
                try {
                    this.data = new BufferDouble(this.dataSize);
                } catch (final OutOfMemoryError error) {
                    disposeLocal();
                    MipavUtil.displayError("ImageModel: Unable to allocate double complex data buffer");
                    throw (error);
                }

                break;

            case ARGB:
                try {
                    this.data = new BufferUByte(this.dataSize);
                } catch (final OutOfMemoryError error) {
                    disposeLocal();
                    MipavUtil.displayError("ImageModel: Unable to allocate ARGB-UByte data buffer");
                    throw (error);
                }

                // set alphas to 255 - full on
                for (i = 0; i < this.dataSize; i += 4) {
                    setUByte(i, (byte) 255);
                }

                break;

            case ARGB_USHORT:
                try {
                    this.data = new BufferUShort(this.dataSize);
                } catch (final OutOfMemoryError error) {
                    disposeLocal();
                    MipavUtil.displayError("ImageModel: Unable to allocate ARGB-UShort data buffer");
                    throw (error);
                }

                // set alphas to 65535 - full on
                for (i = 0; i < this.dataSize; i += 4) {
                    setUShort(i, 65535);
                }

                break;

            case ARGB_FLOAT:
                try {
                    this.data = new BufferFloat(this.dataSize);
                } catch (final OutOfMemoryError error) {
                    disposeLocal();
                    MipavUtil.displayError("ImageModel: Unable to allocate ARGB- float data buffer");
                    throw (error);
                }

                // set alphas to 1.0 - full on
                for (i = 0; i < this.dataSize; i += 4) {
                    set(i, 1.0f);
                }

                break;

            default: {
                MipavUtil.displayError("ModelStorageArray: Unknown data type - memory not allocated");
            }
        }

    } // end allocateData()

    /**
     * his method return the min value allowed of the type
     * 
     * @param type
     * @return
     */
    public static double getTypeMin(final int type) {
        if(DataType.getDataType(type) != null) {
            return DataType.getDataType(type).getTypeMin().doubleValue();
        } else {
            return 0;
        }
    }

    /**
     * This method return the max value allowed of the type
     * 
     * @param type
     * @return
     */
    public static double getTypeMax(final int type) {
        if(DataType.getDataType(type) != null) {
            return DataType.getDataType(type).getTypeMax().doubleValue();
        } else {
            return 0;
        }
    }

    /**
     * computes the avg intensity got 2d and 3d greyscale and color images
     * 
     * @return
     */
    public void calcAvgIntenStdDev() {
        if (getNDims() == 2) {
            final int numPixelsPerSlice = getSliceSize();
            if ( (bufferType != DataType.ARGB) && (bufferType != DataType.ARGB_USHORT)
                    && (bufferType != DataType.ARGB_FLOAT)) { // greyscale
                // image
                final float[] imageBuffer = new float[numPixelsPerSlice];

                try {
                    exportData(0, imageBuffer.length, imageBuffer);
                } catch (final Exception e) {
                    e.printStackTrace();
                }

                for (final float element : imageBuffer) {
                    numPixels = numPixels + 1;
                    sumPixelInten = sumPixelInten + element;
                }

                avgInten = (float) (sumPixelInten / numPixels);

                float dev;
                float devSqrd;
                double sumDevSqrd = 0;
                for (final float element : imageBuffer) {
                    dev = element - avgInten;
                    devSqrd = dev * dev;
                    sumDevSqrd = sumDevSqrd + devSqrd;
                }

                stdDeviation = (float) Math.sqrt(sumDevSqrd / (imageBuffer.length - 1));
            } else { // color image
                final float[] imageBuffer = new float[numPixelsPerSlice * 4];

                try {
                    exportData(0, imageBuffer.length, imageBuffer);
                } catch (final Exception e) {
                    e.printStackTrace();
                }

                for (int i = 0; i < imageBuffer.length; i = i + 4) {
                    numPixels = numPixels + 1;
                    sumPixelIntenR = sumPixelIntenR + imageBuffer[i + 1];
                    sumPixelIntenG = sumPixelIntenG + imageBuffer[i + 2];
                    sumPixelIntenB = sumPixelIntenB + imageBuffer[i + 3];
                }

                avgIntenR = (float) (sumPixelIntenR / numPixels);
                avgIntenG = (float) (sumPixelIntenG / numPixels);
                avgIntenB = (float) (sumPixelIntenB / numPixels);

                float devR, devG, devB;
                float devSqrdR, devSqrdG, devSqrdB;
                double sumDevSqrdR = 0, sumDevSqrdG = 0, sumDevSqrdB = 0;

                for (int i = 0; i < imageBuffer.length; i = i + 4) {
                    devR = imageBuffer[i + 1] - avgIntenR;
                    devSqrdR = devR * devR;
                    sumDevSqrdR = sumDevSqrdR + devSqrdR;

                    devG = imageBuffer[i + 2] - avgIntenG;
                    devSqrdG = devG * devG;
                    sumDevSqrdG = sumDevSqrdG + devSqrdG;

                    devB = imageBuffer[i + 3] - avgIntenB;
                    devSqrdB = devB * devB;
                    sumDevSqrdB = sumDevSqrdB + devSqrdB;
                }

                stdDeviationR = (float) Math.sqrt(sumDevSqrdR / (imageBuffer.length - 1));
                stdDeviationG = (float) Math.sqrt(sumDevSqrdG / (imageBuffer.length - 1));
                stdDeviationB = (float) Math.sqrt(sumDevSqrdB / (imageBuffer.length - 1));
            }
        } else if (getNDims() == 3) {
            final int numSlices = getExtents()[2];
            final int numPixelsPerSlice = getSliceSize();
            if ( (bufferType != DataType.ARGB) && (bufferType != DataType.ARGB_USHORT)
                    && (bufferType != DataType.ARGB_FLOAT)) { // greyscale
                // image
                final float[] imageBuffer = new float[numSlices * numPixelsPerSlice];

                try {
                    exportData(0, imageBuffer.length, imageBuffer);
                } catch (final Exception e) {
                    e.printStackTrace();
                }

                for (final float element : imageBuffer) {
                    numPixels = numPixels + 1;
                    sumPixelInten = sumPixelInten + element;
                }

                avgInten = (float) (sumPixelInten / numPixels);

                float dev;
                float devSqrd;
                double sumDevSqrd = 0;
                for (final float element : imageBuffer) {
                    dev = element - avgInten;
                    devSqrd = dev * dev;
                    sumDevSqrd = sumDevSqrd + devSqrd;
                }

                stdDeviation = (float) Math.sqrt(sumDevSqrd / (imageBuffer.length - 1));
            } else { // color image
                final float[] imageBuffer = new float[numSlices * numPixelsPerSlice * 4];

                try {
                    exportData(0, imageBuffer.length, imageBuffer);
                } catch (final Exception e) {
                    e.printStackTrace();
                }

                for (int i = 0; i < imageBuffer.length; i = i + 4) {
                    numPixels = numPixels + 1;
                    sumPixelIntenR = sumPixelIntenR + imageBuffer[i + 1];
                    sumPixelIntenG = sumPixelIntenG + imageBuffer[i + 2];
                    sumPixelIntenB = sumPixelIntenB + imageBuffer[i + 3];
                }

                avgIntenR = (float) (sumPixelIntenR / numPixels);
                avgIntenG = (float) (sumPixelIntenG / numPixels);
                avgIntenB = (float) (sumPixelIntenB / numPixels);

                float devR, devG, devB;
                float devSqrdR, devSqrdG, devSqrdB;
                double sumDevSqrdR = 0, sumDevSqrdG = 0, sumDevSqrdB = 0;

                for (int i = 0; i < imageBuffer.length; i = i + 4) {
                    devR = imageBuffer[i + 1] - avgIntenR;
                    devSqrdR = devR * devR;
                    sumDevSqrdR = sumDevSqrdR + devSqrdR;

                    devG = imageBuffer[i + 2] - avgIntenG;
                    devSqrdG = devG * devG;
                    sumDevSqrdG = sumDevSqrdG + devSqrdG;

                    devB = imageBuffer[i + 3] - avgIntenB;
                    devSqrdB = devB * devB;
                    sumDevSqrdB = sumDevSqrdB + devSqrdB;
                }
                stdDeviationR = (float) Math.sqrt(sumDevSqrdR / ( (imageBuffer.length / 4) - 1));
                stdDeviationG = (float) Math.sqrt(sumDevSqrdG / ( (imageBuffer.length / 4) - 1));
                stdDeviationB = (float) Math.sqrt(sumDevSqrdB / ( (imageBuffer.length / 4) - 1));

            }
        }
    }

    public float getStdDeviation() {
        return stdDeviation;
    }

    public float getAvgInten() {
        return avgInten;
    }

    public int getNumPixels() {
        return numPixels;
    }

    public double getSumPixelInten() {
        return sumPixelInten;
    }

    public float getAvgIntenB() {
        return avgIntenB;
    }

    public float getAvgIntenG() {
        return avgIntenG;
    }

    public float getAvgIntenR() {
        return avgIntenR;
    }

    public float getStdDeviationB() {
        return stdDeviationB;
    }

    public float getStdDeviationG() {
        return stdDeviationG;
    }

    public float getStdDeviationR() {
        return stdDeviationR;
    }

    public double getSumPixelIntenB() {
        return sumPixelIntenB;
    }

    public double getSumPixelIntenG() {
        return sumPixelIntenG;
    }

    public double getSumPixelIntenR() {
        return sumPixelIntenR;
    }

    /**
     * Computes the datasize based on the type of buffer.
     */
    protected void computeDataSize() {
        int i;
        long longSize;

        // base data size
        for (this.dataSize = 1, i = 0, longSize = 1; i < this.nDims; i++) {
            this.dataSize *= dimExtents[i];
            longSize *= dimExtents[i];
        }

        if (longSize > Integer.MAX_VALUE) {
            MipavUtil.displayError("Product of dimExtents exceeds maximum allowable array size = " + Integer.MAX_VALUE);
            return;
        }

        switch (bufferType) {

            case BOOLEAN:
                try {

                    // create a temporary buffer to get this dataSize)
                    BufferBase tmpdata = new BufferBoolean(this.dataSize);

                    // Reset dataSize to reflect change in buffer size
                    // because BitSet structure may increase buffer.
                    dataSize = ((BufferBoolean) (tmpdata)).dataArray.size();

                    // clean up the temporary buffer
                    tmpdata = null;
                    System.gc();
                } catch (final OutOfMemoryError error) {
                    disposeLocal();
                    MipavUtil.displayError("ImageModel: Unable to allocate boolean data");
                    throw (error);
                }

                break;

            case BYTE:
            case UBYTE:
            case SHORT:
            case USHORT:
            case INTEGER:
            case UINTEGER:
            case LONG:
            case FLOAT:
            case DOUBLE:
                // use base data size unchanged.
                break;

            case COMPLEX:
            case DCOMPLEX:

                this.dataSize *= 2;
                longSize *= 2;
                if (longSize > Integer.MAX_VALUE) {
                    MipavUtil.displayError("2 times product of dimExtents exceeds maximum allowable array size = "
                            + Integer.MAX_VALUE);
                    return;
                }
                break;

            case ARGB:
            case ARGB_USHORT:
            case ARGB_FLOAT:

                this.dataSize *= 4;
                longSize *= 4;
                if (longSize > Integer.MAX_VALUE) {
                    MipavUtil.displayError("4 times product of dimExtents exceeds maximum allowable array size = "
                            + Integer.MAX_VALUE);
                    return;
                }
                break;

            default: {
                MipavUtil.displayError("ModelStorageArray: Unknown data type - dataSize not computed");
                break;
            }
        } // end switch

    } // end computeDataSize()

    /**
     * Allocates buffer memory of the specified type.
     * 
     * @param type type of buffer to allocate
     * @param dimExtents extents of the buffer in each dimension (multiplied together produces the size of the buffer to
     *            be allocated
     */
    protected void construct(final DataType type, final int[] dimExtents) {

        this.bufferType = type;
        this.dimExtents = dimExtents.clone();
        this.nDims = dimExtents.length;

        // compute the dataSize based on the extents
        computeDataSize();

        // allocate memory in the data buffer and do some initialization
        // based on dataSize
        allocateData();

    } // end construct()

    /**
     * Calls disposeLocal of this class to ensure this class nulls the references to global class variables so that
     * memory will be recovered.
     * 
     * @throws Throwable Throws an error if the finalization failed.
     */
    protected void finalize() throws Throwable {
        disposeLocal();
        super.finalize();
    }

    /**
     * Get total buffer length.
     */
    public int getDataSize() {
        return dataSize;
    }

}
