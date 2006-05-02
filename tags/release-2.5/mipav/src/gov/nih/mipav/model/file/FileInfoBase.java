package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;


/**
 * This structure contains the basic information that describes how the image is stored on disk.
 *
 * <p>Subclasses add additional information which is particular to that image-format.</p>
 *
 * <p>This class needs work</p>
 *
 * <p>1. fixing (making consistance what to do when null pointers encountered. see getStartLocation and
 * getUnitsOfMeasure</p>
 *
 * @version  0.9 June 30, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      FileBase
 */
public abstract class FileInfoBase extends ModelSerialCloneable {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 6605143084958470864L;

    /** Unit of measurement unknown. */
    public static final int UNKNOWN_MEASURE = 1;

    /** Unit of measurement inches. */
    public static final int INCHES = 2;

    /** Unit of measurement centimeters. */
    public static final int CENTIMETERS = 3;

    /** Unit of measurement angstroms. */
    public static final int ANGSTROMS = 4;

    /** Unit of measurement nanometers. */
    public static final int NANOMETERS = 5;

    /** Unit of measurement micrometers. */
    public static final int MICROMETERS = 6;

    /** Unit of measurement millimeters. */
    public static final int MILLIMETERS = 7;

    /** Unit of measurement meters. */
    public static final int METERS = 8;

    /** Unit of measurement kilometers. */
    public static final int KILOMETERS = 9;

    /** Unit of measurement miles. */
    public static final int MILES = 10;

    /** Unit of measurement nanoseconds. */
    public static final int NANOSEC = 11;

    /** Unit of measurement microseconds. */
    public static final int MICROSEC = 12;

    /** Unit of measurement milliseconds. */
    public static final int MILLISEC = 13;

    /** Unit of measurement seconds. */
    public static final int SECONDS = 14;

    /** Unit of measurement minutes. */
    public static final int MINUTES = 15;

    /** Unit of measurement hours. */
    public static final int HOURS = 16;

    /** Unit of measurement hertz. */
    public static final int HZ = 17;

    /** Unit of measurement part-per-million. */
    public static final int PPM = 18;

    /** Radians per second. */
    public static final int RADS = 19;

    /** String version of units of measurement - unknown. */
    public static final String UNKNOWN_STRING = "Unknown";

    /** String version of units of measurement - inches. */
    public static final String INCHES_STRING = "Inches";

    /** String version of units of measurement - centimeters. */
    public static final String CENTIMETERS_STRING = "Centimeters";

    /** String version of units of measurement - angstroms. */
    public static final String ANGSTROMS_STRING = "Angstroms";

    /** String version of units of measurement - nanometers. */
    public static final String NANOMETERS_STRING = "Nanometers";

    /** String version of units of measurement - micrometers. */
    public static final String MICROMETERS_STRING = "Micrometers";

    /** String version of units of measurement - millimeters. */
    public static final String MILLIMETERS_STRING = "Millimeters";

    /** String version of units of measurement - meters. */
    public static final String METERS_STRING = "Meters";

    /** String version of units of measurement - kilometers. */
    public static final String KILOMETERS_STRING = "Kilometers";

    /** String version of units of measurement - miles. */
    public static final String MILES_STRING = "Miles";

    /** String version of units of measurement - nanoseconds. */
    public static final String NANOSEC_STRING = "Nanoseconds";

    /** String version of units of measurement - microseconds. */
    public static final String MICROSEC_STRING = "Microseconds";

    /** String version of units of measurement - milliseconds. */
    public static final String MILLISEC_STRING = "Milliseconds";

    /** String version of units of measurement - seconds. */
    public static final String SECONDS_STRING = "Seconds";

    /** String version of units of measurement - minutes. */
    public static final String MINUTES_STRING = "Minutes";

    /** String version of units of measurement - hours. */
    public static final String HOURS_STRING = "Hours";

    /** String version of units of measurement - hertz. */
    public static final String HZ_STRING = "Hertz";

    /** String version of units of measurement - part-per-million. */
    public static final String PPM_STRING = "Part_Per_Million";

    /** String version of units of measurement - radians per second. */
    public static final String RADS_STRING = "Radians_Per_Second";

    /**
     * Array of all units --- the first value is unknown since all of the* static definitions start at 1 instead of 0.
     */
    private static final String[] allUnits = {
        UNKNOWN_STRING, UNKNOWN_STRING, INCHES_STRING, CENTIMETERS_STRING, ANGSTROMS_STRING, NANOMETERS_STRING,
        MICROMETERS_STRING, MILLIMETERS_STRING, METERS_STRING, KILOMETERS_STRING, MILES_STRING, NANOSEC_STRING,
        MICROSEC_STRING, MILLISEC_STRING, SECONDS_STRING, MINUTES_STRING, HOURS_STRING, HZ_STRING, PPM_STRING,
        RADS_STRING
    };

    /**
     * Array of all abbreviated units --- the first value is unknown since all of the* static definitions start at 1
     * instead of 0. Each string* can be no more than 4 characters.
     */
    private static final String[] allAbbrevUnits = {
        "unk", "unk", "in", "cm", "A", "nm", "um", "mm", "m", "km", "mi", "nsec", "usec", "msec", "sec", "min", "hr",
        "hz", "ppm", "rads"
    };

    /** Array of space units: inches, mm, etc. */
    public static final String[] sUnits = {
        "Unknown", "Unknown", "Inches", "cm", "A", "nm", "um", "mm", "m", "km", "miles"
    };

    /** Array of time units: seconds, minutes, etc. */
    public static final String[] tUnits = {
        "nano seconds", "micro seconds", "milli seconds", "seconds", "minutes", "hours", "hertz", "part per million",
        "radians per second"
    };

    /** Image modality unknown. */
    public static final int UNKNOWN_MODALITY = 0;

    /** Image modality biomagnetic imaging. */
    public static final int BIOMAGENETIC_IMAGING = 1;

    /** Image modality color flow doppler. */
    public static final int COLOR_FLOW_DOPPLER = 2;

    /** Image modality CR. */
    public static final int COMPUTED_RADIOGRAPHY = 3;

    /** Image modality CT. */
    public static final int COMPUTED_TOMOGRAPHY = 4;

    /** Image modality duplex doppler. */
    public static final int DUPLEX_DOPPLER = 5;

    /** Image modality diaphanography. */
    public static final int DIAPHANOGRAPHY = 6;

    /** Image modality digital radiography. */
    public static final int DIGITAL_RADIOGRAPHY = 7;

    /** Image modality endoscopy. */
    public static final int ENDOSCOPY = 8;

    /** Image modality general microscopy. */
    public static final int GENERAL_MICROSCOPY = 9;

    /** Image modality hard copy. */
    public static final int HARDCODY = 10;

    /** Image modality intraoral radiography. */
    public static final int INTRAORAL_RADIOGRAPHY = 11;

    /** Image modality laser surface scan. */
    public static final int LASER_SURFACE_SCAN = 12;

    /** Image modality MR angiography. */
    public static final int MAGNETIC_RESONANCE_ANGIOGRAPHY = 13;

    /** Image modality mammography. */
    public static final int MAMMOGRAPHY = 14;

    /** Image modality MR. */
    public static final int MAGNETIC_RESONANCE = 15;

    /** Image modality MR SPECT. */
    public static final int MAGNETIC_RESONANCE_SPECTROSCOPY = 16;

    /** Image modality nuclear medicine. */
    public static final int NUCLEAR_MEDICINE = 17;

    /** Image modality other. */
    public static final int OTHER = 18;

    /** Image modality PET. */
    public static final int POSITRON_EMISSION_TOMOGRAPHY = 19;

    /** Image modality panoramtic X ray. */
    public static final int PANORAMIC_XRAY = 20;

    /** Image modality radio fluoroscopy. */
    public static final int RADIO_FLUOROSCOPY = 21;

    /** Image modality radiographic imaging. */
    public static final int RADIOGRAPHIC_IMAGING = 22;

    /** Image modality radiotherapy dose. */
    public static final int RADIOTHERAPY_DOSE = 23;

    /** Image modality radiotherapy image. */
    public static final int RADIOTHERAPY_IMAGE = 24;

    /** Image modality radiotherapy plan. */
    public static final int RADIOTHERAPY_PLAN = 25;

    /** Image modality radiotherapy record. */
    public static final int RADIOTHERAPY_RECORD = 26;

    /** Image modality radiotherapy structure set. */
    public static final int RADIOTHERAPY_STRUCTURE_SET = 27;

    /** Image modality slide microscopy. */
    public static final int SLIDE_MICROSCOPY = 28;

    /** Image modality SPECT. */
    public static final int SINGLE_PHOTON_EMISSION_COMPUTED_TOMOGRAPHY = 29;

    /** Image modality thermography. */
    public static final int THERMOGRAPHY = 30;

    /** Image modality ultrasound. */
    public static final int ULTRASOUND = 31;

    /** Image modality X ray angiography. */
    public static final int XRAY_ANGIOGRAPHY = 32;

    /** Image modality external camera photography. */
    public static final int EXTERNAL_CAMERA_PHOTOGRAPHY = 33;

    /** Image modality Red Free. */
    public static final int RED_FREE = 34;

    /** Image modality Fluorescein Angiography. */
    public static final int FA = 35;

    /** Image modality IndoCyanine Green. */
    public static final int ICG = 36;

    /** Array of modality strings -- again, numbering starts at 1, not 0. */
    private static final String[] modalityStr = {
        "Unknown Modality", "Biomagenetic Imaging", "Color Flow Doppler", "Computed Radiography", "Computed Tomography",
        "Duplex Doppler", "Diaphanography", "Digital Radiography", "Endoscopy", "General Microscopy", "Hardcody",
        "Intraoral Radiography", "Laser Surface Scan", "Magnetic Resonance Angiography", "Mammography",
        "Magnetic Resonance", "Magnetic Resonance Spectroscopy", "Nuclear Medicine", "Other",
        "Positron Emission Tomography", "Panoramic XRay", "Radio Fluoroscopy", "Radiographic Imaging",
        "Radiotherapy Dose", "Radiotherapy Image", "Radiotherapy Plan", "Radiotherapy Record",
        "Radiotherapy Structure Set", "Slide Microscopy", "Single Photon Emission Computed Tomography", "Thermography",
        "Ultrasound", "XRay Angiography", "External Camera Photography", "Red Free", "FA", "ICG"
    };

    /** Axis orientation unknown. */
    public static final int ORI_UNKNOWN_TYPE = 0;

    /** Axis orientation Right to Left. */
    public static final int ORI_R2L_TYPE = 1;

    /** Axis orientation Left to Right. */
    public static final int ORI_L2R_TYPE = 2;

    /** Axis orientation Posterior to Anterior. */
    public static final int ORI_P2A_TYPE = 3;

    /** Axis orientation Anterior to Posterior. */
    public static final int ORI_A2P_TYPE = 4;

    /** Axis orientation Inferior to Superior. */
    public static final int ORI_I2S_TYPE = 5;

    /** Axis orientation Superior to Inferior. */
    public static final int ORI_S2I_TYPE = 6;

    /** Array of axis orientation strings. */
    private static final String[] axisOrientationStr = {
        "Unknown", "Right to Left", "Left to Right", "Posterior to Anterior", "Anterior to Posterior",
        "Inferior to Superior", "Superior to Inferior"
    };

    /** Axial orientation. */
    public static final int AXIAL = 0;

    /** Coronal orientation. */
    public static final int CORONAL = 1;

    /** Sagittal orientation. */
    public static final int SAGITTAL = 2;

    /** Unknown orientation. */
    public static final int UNKNOWN_ORIENT = 3;

    /** Array of image orientation strings. */
    private static final String[] imageOrientationStr = { "Axial", "Coronal", "Sagittal", "Unknown" };

    /** Unknown transform ID. */
    public static final int TRANSFORM_UNKNOWN = 0;

    /** Scanner Anatomical transform ID. */
    public static final int TRANSFORM_SCANNER_ANATOMICAL = 1;

    /** Another Dataset transform ID. */
    public static final int TRANSFORM_ANOTHER_DATASET = 2;

    /** Talairach Tournoux transform ID. */
    public static final int TRANSFORM_TALAIRACH_TOURNOUX = 3;

    /** MNI 152 transform ID. */
    public static final int TRANSFORM_MNI_152 = 4;

    /** Array of transform ID strings. */
    private static final String[] transformIDStr = {
        "Unknown", "Scanner Anatomical", "Another Dataset", "Talairach Tournoux", "MNI 152"
    };

    /** Indicates no compression. */
    public static final int COMPRESSION_NONE = 0;

    /** Indicates zip compression of an image. */
    public static final int COMPRESSION_ZIP = 1;

    /** Indicates gzip compression of an image. */
    public static final int COMPRESSION_GZIP = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * axis orientation used to support image ordering and display for medical images. We support the right hand rule
     * where the origin is the upper left hand of the image with the positive axis.
     *
     * <p>x - left to right y - top to botton z - into the screen</p>
     */
    protected int[] axisOrientation = { ORI_UNKNOWN_TYPE, ORI_UNKNOWN_TYPE, ORI_UNKNOWN_TYPE };

    /** File directory where the image is located. */
    protected String fileDir;

    /** File name the the image was read from (image extension included - foo.img, foo.dcm ). */
    protected String fileName;

    /** File suffix (ex. "jpg") */
    protected String fileSuffix;

    /** Indicates the image orientation (i.e. Axial, Coronal, ...) */
    protected int imageOrientation = UNKNOWN_ORIENT;

    /** Indicates the modality (medical image type) of the dataset. */
    protected int modality = UNKNOWN_MODALITY;

    /**
     * the origin to support image locations (ie. DICOM, MINC ...) it is relative to the image origin. the positive axis
     * are (right hand rule.
     *
     * <p>x - left to right y - top to botton z - into the screen</p>
     */
    protected float[] origin = new float[4]; // { 0, 0, 0, 0};

    /** Used to indicate if the raw data was also compression (0 = no, 1 = zip). */
    private int compressionType = 0;

    /** The data type of the data (i.e. byte, short, float ... */
    private int dataType;

    /** Pixel or voxel resolutions for each dimension - default = 1.0. */
    private float[] dimResolutions = { (float) 1.0, (float) 1.0, (float) 1.0, (float) 1.0, (float) 1.0 };

    /**
     * The Endianess of the data. Intel, DEC Alpha ***** LSB first byte LITTLE_ENDIAN (false) Motorola (MAC), SPARC
     * (SUN), SGI IRIX MSB first byte BIG_ENDIAN (true)
     */
    private boolean endianess = FileBase.LITTLE_ENDIAN;

    /** Image extents as decribed by the image file format. */
    private int[] extents = new int[5];

    /** File format as defined in the Filebase. */
    private int fileFormat;

    /** If the image is 2.5D. */
    private boolean is2_5D = false;

    /** Image maximum intensity for single channel image. */
    private double max;

    /** Image maximum intensity for the blue channel of an RGB image. */
    private double maxB;

    /** Image maximum intensity for the green channel of an RGB image. */
    private double maxG;

    /** Image maximum intensity for the red channel of an RGB image. */
    private double maxR;

    /** Image minimum intensity for single channel image. */
    private double min;

    /** Image minimum intensity for the blue channel of an RGB image. */
    private double minB;

    /** Image minimum intensity for the green channel of an RGB image. */
    private double minG;

    /** Image minimum intensity for the red channel of an RGB image. */
    private double minR;

    // 0 indicates  0 is white
    // 2 RGB
    // 3 indexed color LUT is saved with image

    /** Flag that indicates whether or not the image is in multiple files (tiff). */
    private boolean multiFile = false;

    /** Number of bytes to the start the image data - ie. the header length */
    private int offset;

    /** Image minimum intensity for single channel image. */
    private short photometric = 1; // 1 indicates  0 is black

    /** Some file formats have a pad value for pixels outside the acquisition domain. */
    private Short pixelPadValue;

    /** DICOM images have a rescale y-intercept value that we have also kept in the base. */
    private double rescaleIntercept = 0.0;

    /** DICOM images have a rescale slope value that we have also kept in the base. */
    private double rescaleSlope = 1.0;

    /** The spacing between slices. Only appears in DICOM (TAG = 0018, 0088) and MIPAV (XML) file formats. */
    private float sliceSpacing = 0;

    /** Transform ID associated with the matrix. */
    private int transformID = TRANSFORM_UNKNOWN;

    /** Describes the units of measure for the dataset. */
    private int[] unitsOfMeasure = { MILLIMETERS, MILLIMETERS, MILLIMETERS, SECONDS, UNKNOWN_MEASURE };

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * fileInfo constructor.
     *
     * @param  name       name of file
     * @param  directory  file directory
     * @param  format     file storage format -- see FileBase.java
     */
    public FileInfoBase(String name, String directory, int format) {

        fileName = name;
        fileDir = directory;
        fileFormat = format;
        fileSuffix = FileIO.getSuffixFrom(name);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Abstract method which is used by the extending class to display information about the window.
     *
     * @param  dialog  Area where image information is to be displayed.
     * @param  matrix  Transformation matrix
     */
    public abstract void displayAboutInfo(JDialogBase dialog, TransMatrix matrix);

    /**
     * Helper method to copy important file info type to another file info type.
     *
     * @param  originalInfo  source file info.
     * @param  newInfo       destination file info.
     */
    public static void copyCoreInfo(FileInfoBase[] originalInfo, FileInfoBase[] newInfo) {

        for (int i = 0; i < originalInfo.length; i++) {
            newInfo[i].setAxisOrientation(originalInfo[i].getAxisOrientation());
            newInfo[i].setDataType(originalInfo[i].getDataType());
            newInfo[i].setEndianess(originalInfo[i].getEndianess());
            newInfo[i].setExtents(originalInfo[i].getExtents());
            newInfo[i].setImageOrientation(originalInfo[i].getImageOrientation());

            if (ModelImage.isColorImage(originalInfo[i].getDataType())) {
                newInfo[i].setMinR(originalInfo[i].getMinR());
                newInfo[i].setMaxR(originalInfo[i].getMaxR());
                newInfo[i].setMinG(originalInfo[i].getMinG());
                newInfo[i].setMaxG(originalInfo[i].getMaxG());
                newInfo[i].setMinB(originalInfo[i].getMinB());
                newInfo[i].setMaxB(originalInfo[i].getMaxB());
            } else {
                newInfo[i].setMin(originalInfo[i].getMin());
                newInfo[i].setMax(originalInfo[i].getMax());
            }

            newInfo[i].setModality(originalInfo[i].getModality());
            newInfo[i].setOrigin(originalInfo[i].getOrigin());
            newInfo[i].setResolutions(originalInfo[i].getResolutions()); // Added 10/23/2002
        }
    }

    /**
     * Helper method to copy core information from one fileinfo into another, this method also has a list of fileinfos
     * NOT to copy (used by JDialogRemoveSlices).
     *
     * @param  originalInfo  FileInfoBase[] original file infos (longer list)
     * @param  newInfo       FileInfoBase[] new file infos (shorter list)
     * @param  listNoCopy    boolean[] boolean array of indices into the original fileinfos that should not be copied
     */
    public static void copyCoreInfo(FileInfoBase[] originalInfo, FileInfoBase[] newInfo, boolean[] listNoCopy) {

        for (int i = 0, j = 0; i < originalInfo.length; i++) {

            if (!listNoCopy[i % listNoCopy.length]) {
                newInfo[j].setAxisOrientation(originalInfo[i].getAxisOrientation());
                newInfo[j].setDataType(originalInfo[i].getDataType());
                newInfo[j].setEndianess(originalInfo[i].getEndianess());
                newInfo[j].setExtents(originalInfo[i].getExtents());
                newInfo[j].setImageOrientation(originalInfo[i].getImageOrientation());

                if (ModelImage.isColorImage(originalInfo[i].getDataType())) {
                    newInfo[j].setMinR(originalInfo[i].getMinR());
                    newInfo[j].setMaxR(originalInfo[i].getMaxR());
                    newInfo[j].setMinG(originalInfo[i].getMinG());
                    newInfo[j].setMaxG(originalInfo[i].getMaxG());
                    newInfo[j].setMinB(originalInfo[i].getMinB());
                    newInfo[j].setMaxB(originalInfo[i].getMaxB());
                } else {
                    newInfo[j].setMin(originalInfo[i].getMin());
                    newInfo[j].setMax(originalInfo[i].getMax());
                }

                newInfo[j].setModality(originalInfo[i].getModality());
                newInfo[j].setOrigin(originalInfo[i].getOrigin());
                newInfo[j].setResolutions(originalInfo[i].getResolutions()); // Added 10/23/2002
                j++; // increment the index into the newInfo
            }
        }
    }

    /**
     * Returns the axis orientation associated with a string.
     *
     * @param   s  String to test
     *
     * @return  axis orientation
     */
    public static int getAxisOrientationFromStr(String s) {

        // look through the array of strings to see if there's a match.
        try {

            for (int i = 0; i < axisOrientationStr.length; i++) {

                if (FileInfoBase.getAxisOrientationStr(i).regionMatches(true, 0, s, 0,
                                                                            FileInfoBase.getAxisOrientationStr(i).length())) {
                    return i;
                }
            }
        } catch (ArrayIndexOutOfBoundsException aie) {
            return FileInfoBase.ORI_UNKNOWN_TYPE;
        }

        return FileInfoBase.ORI_UNKNOWN_TYPE;

    } // end getModalityFromStr()

    /**
     * Return the string associated with an axis orientation.
     *
     * @param   m  int representing the axis orientation (see the static definitions)
     *
     * @return  String representing the string associated with the axis orientation.
     */
    public static String getAxisOrientationStr(int m) {

        try {
            return FileInfoBase.axisOrientationStr[m];
        } catch (ArrayIndexOutOfBoundsException aie) { }

        return "";

    } // end getAxisOrientationStr()

    /**
     * Returns the image data type associated with a string.
     *
     * @param   s  String to test
     *
     * @return  data type
     */
    public static int getDataTypeFromStr(String s) {

        for (int i = 0; i < ModelStorageBase.bufferTypeStr.length; i++) {

            if (ModelStorageBase.bufferTypeStr[i].regionMatches(true, 0, s, 0, s.length())) {
                return i;
            }
        }

        return ModelStorageBase.SHORT;
    } // end getDataTypeFromStr()

    /**
     * Returns the endianess associated with a string.
     *
     * @param   s  String to test
     *
     * @return  Big endian or little endian
     */
    public static boolean getEndianessFromStr(String s) {

        if ((s.indexOf("Big") != -1) || (s.indexOf("big") != -1) || (s.indexOf("BIG") != -1)) {
            return FileBase.BIG_ENDIAN;
        } else if ((s.indexOf("Little") != -1) || (s.indexOf("little") != -1) || (s.indexOf("LITTLE") != -1)) {
            return FileBase.LITTLE_ENDIAN;
        }

        return FileBase.LITTLE_ENDIAN;
    } // end getEndianessFromStr()

    /**
     * Returns the image orientation associated with a string.
     *
     * @param   s  String to test
     *
     * @return  image orientation
     */
    public static int getImageOrientationFromStr(String s) {

        // look through the array of strings to see if there's a match.
        try {

            for (int i = 0; i < 3; i++) {

                if (FileInfoBase.getImageOrientationStr(i).regionMatches(true, 0, s, 0,
                                                                             FileInfoBase.getImageOrientationStr(i).length())) {
                    return i;
                }
            }
        } catch (ArrayIndexOutOfBoundsException aie) {
            return FileInfoBase.UNKNOWN_ORIENT;
        }

        return FileInfoBase.UNKNOWN_ORIENT;

    } // end getImageOrientationFromStr()

    /**
     * Return the string associated with an image orientation.
     *
     * @param   m  the orientation (see the static definitions)
     *
     * @return  the string associated with the orientation.
     */
    public static String getImageOrientationStr(int m) {

        try {
            return FileInfoBase.imageOrientationStr[m];
        } catch (ArrayIndexOutOfBoundsException aie) { }

        return "";

    } // end getImageOrientationStr()

    /**
     * Returns the modality associated with a string.
     *
     * @param   s  String to test
     *
     * @return  modality
     */
    public static int getModalityFromStr(String s) {

        // look through the array of strings to see if there's a match.
        try {

            for (int i = 0; i < modalityStr.length; i++) {

                if (FileInfoBase.getModalityStr(i).regionMatches(true, 0, s, 0,
                                                                     FileInfoBase.getModalityStr(i).length())) {
                    return i;
                }
            }
        } catch (ArrayIndexOutOfBoundsException aie) {
            return FileInfoBase.UNKNOWN_MODALITY;
        }

        return FileInfoBase.UNKNOWN_MODALITY;

    } // end getModalityFromStr()

    /**
     * Return all the modality strings as an array.
     *
     * @return  String[] - array containing the strings associated with modalities.
     */
    public static String[] getModalityStr() {

        return modalityStr;

    } // end getModalityStr()

    /**
     * Return the string associated with a modality.
     *
     * @param   m  the modality (see the static definitions)
     *
     * @return  the string associated with the modality.
     */
    public static String getModalityStr(int m) {

        try {
            return FileInfoBase.modalityStr[m];
        } catch (ArrayIndexOutOfBoundsException aie) { }

        return "";

    } // end getModalityStr()

    /**
     * Returns the transform ID associated with a string.
     *
     * @param   s  String to test
     *
     * @return  data type
     */
    public static int getTransformIDFromStr(String s) {

        // look through the array of strings to see if there's a match.
        try {

            for (int i = 0; i < transformIDStr.length; i++) {

                if (FileInfoBase.getTransformIDStr(i).regionMatches(true, 0, s, 0,
                                                                        FileInfoBase.getTransformIDStr(i).length())) {
                    return i;
                }
            }
        } catch (ArrayIndexOutOfBoundsException aie) {
            return FileInfoBase.TRANSFORM_UNKNOWN;
        }

        return FileInfoBase.TRANSFORM_UNKNOWN;

    } // getTransformIDFromStr()

    /**
     * Return the list of transform ID strings (for edit attributes combo box.
     *
     * @return  string [] of transform ID
     */
    public static String[] getTransformIDStr() {
        return transformIDStr;
    }

    /**
     * Return the string associated with the matrix transform ID.
     *
     * @param   m  transform ID
     *
     * @return  the string associated with the transform ID
     */
    public static String getTransformIDStr(int m) {

        try {
            return FileInfoBase.transformIDStr[m];
        } catch (ArrayIndexOutOfBoundsException aie) { }

        return "";
    }

    /**
     * Return all the abbreviated units of measure strings as an array.
     *
     * @return  String[] - array containing the abbreviated strings associated with units of measure.
     */
    public static String[] getUnitsOfMeasureAbbrevStr() {

        return allAbbrevUnits;

    } // end getUnitsOfMeasureAbbrevStr()


    /**
     * Return the abbreviated string associated with a units of measure.
     *
     * @param   units  the units of measure (see the static definitions)
     *
     * @return  the abbreviated string associated with the units.
     */
    public static String getUnitsOfMeasureAbbrevStr(int units) {

        try {
            return FileInfoBase.allAbbrevUnits[units];
        } catch (ArrayIndexOutOfBoundsException aie) { }

        return "";

    } // end getUnitsOfMeasureAbbrevStr()

    /**
     * Returns the units of measure.
     *
     * @param   s  DOCUMENT ME!
     *
     * @return  int units (Inches or millimeters);
     */
    public static int getUnitsOfMeasureFromStr(String s) {

        // look through both the long and abbreviated arrays
        // of strings to see if there's a match.
        try {

            for (int i = 0; i < allUnits.length; i++) {

                if (FileInfoBase.getUnitsOfMeasureStr(i).regionMatches(true, 0, s, 0,
                                                                           FileInfoBase.getUnitsOfMeasureStr(i).length())) {
                    return i;
                } else if (FileInfoBase.getUnitsOfMeasureAbbrevStr(i).regionMatches(true, 0, s, 0,
                                                                                        FileInfoBase.getUnitsOfMeasureAbbrevStr(i).length())) {
                    return i;
                }
            }
        } catch (ArrayIndexOutOfBoundsException aie) {
            return FileInfoBase.UNKNOWN_MEASURE;
        }

        return FileInfoBase.UNKNOWN_MEASURE;

    } // end getUnitsOfMeasureFromStr()

    /**
     * Return all the units of measure strings as an array.
     *
     * @return  String[] - array containing the strings associated with units of measure.
     */
    public static String[] getUnitsOfMeasureStr() {

        return allUnits;

    } // end getUnitsOfMeasureStr()

    /**
     * Return the string associated with a units of measure.
     *
     * @param   units  the units of measure (see the static definitions)
     *
     * @return  the string associated with the units.
     */
    public static String getUnitsOfMeasureStr(int units) {

        try {
            return FileInfoBase.allUnits[units];
        } catch (ArrayIndexOutOfBoundsException aie) { }

        return "";

    } // end getUnitsOfMeasureStr()

    /**
     * Helper method that returns the opposite axis orientation of the one sent in; that is, R2L for L2R, A2P for P2A,
     * etc.
     *
     * @param   orient  DOCUMENT ME!
     *
     * @return  int Opposite image orientation
     */
    public static int oppositeOrient(int orient) {
        int neworient = -1;

        switch (orient) {

            case FileInfoBase.ORI_A2P_TYPE:
                neworient = FileInfoBase.ORI_P2A_TYPE;
                break;

            case FileInfoBase.ORI_P2A_TYPE:
                neworient = FileInfoBase.ORI_A2P_TYPE;
                break;

            case FileInfoBase.ORI_R2L_TYPE:
                neworient = FileInfoBase.ORI_L2R_TYPE;
                break;

            case FileInfoBase.ORI_L2R_TYPE:
                neworient = FileInfoBase.ORI_R2L_TYPE;
                break;

            case FileInfoBase.ORI_I2S_TYPE:
                neworient = FileInfoBase.ORI_S2I_TYPE;
                break;

            case FileInfoBase.ORI_S2I_TYPE:
                neworient = FileInfoBase.ORI_I2S_TYPE;
                break;
        }

        return neworient;
    }

    /**
     * Helper method to determine if axis A and axis B are the same axis: that is, if both are the patient x-axis, they
     * will be either R2L or L2R.
     *
     * @param   axisA  Axis A: one of the defined ORI_ types.
     * @param   axisB  Axis B: one of the defined ORI_ types.
     *
     * @return  boolean <code>true</code> if axis A and axis B are the same axis
     */
    public static boolean sameAxis(int axisA, int axisB) {

        if ((axisA == ORI_R2L_TYPE) || (axisA == ORI_L2R_TYPE)) {

            if ((axisB == ORI_R2L_TYPE) || (axisB == ORI_L2R_TYPE)) {
                return true;
            } else {
                return false;
            }
        } else if ((axisA == ORI_A2P_TYPE) || (axisA == ORI_P2A_TYPE)) {

            if ((axisB == ORI_A2P_TYPE) || (axisB == ORI_P2A_TYPE)) {
                return true;
            } else {
                return false;
            }
        } else if ((axisA == ORI_S2I_TYPE) || (axisA == ORI_I2S_TYPE)) {

            if ((axisB == ORI_S2I_TYPE) || (axisB == ORI_I2S_TYPE)) {
                return true;
            } else {
                return false;
            }
        } else {
            return false;
        }
    }

    /**
     * Copies the object.
     *
     * @return  Object A copy of the file info.
     */
    public Object clone() {
        Object base = super.clone();

        return (base);
    }


    /**
     * Copies the object.
     *
     * @return  Object A copy of the file info.
     */
    public Object cloneItself() {
        FileInfoBase cloned = (FileInfoBase) super.cloneItself();
        cloned.modality = modality;
        cloned.imageOrientation = imageOrientation;
        cloned.fileName = fileName;
        cloned.fileDir = fileDir;
        cloned.fileSuffix = fileSuffix;
        cloned.fileFormat = fileFormat;
        cloned.dataType = dataType;

        cloned.offset = offset;
        cloned.endianess = endianess;
        cloned.sliceSpacing = sliceSpacing;
        cloned.min = min;
        cloned.max = max;
        cloned.minR = minR;
        cloned.maxR = maxR;
        cloned.minG = minG;
        cloned.maxG = maxG;
        cloned.minB = minB;
        cloned.maxB = maxB;
        cloned.pixelPadValue = pixelPadValue;
        cloned.rescaleIntercept = rescaleIntercept;
        cloned.rescaleSlope = rescaleSlope;
        cloned.photometric = photometric;
        cloned.multiFile = multiFile;
        cloned.is2_5D = is2_5D;
        cloned.compressionType = compressionType;
        cloned.transformID = transformID;

        cloned.origin = (float[]) (origin.clone());
        cloned.axisOrientation = (int[]) (axisOrientation.clone());
        cloned.extents = (int[]) (extents.clone());
        cloned.unitsOfMeasure = (int[]) (unitsOfMeasure.clone());
        cloned.dimResolutions = (float[]) (dimResolutions.clone());

        return cloned;

    }

    /**
     * Displays the file information.
     *
     * @param  dialog  dialog box that is written to
     */
    public void displayAboutInfo(JDialogBase dialog) {
        displayAboutInfo(dialog, null);
    }

    /**
     * Method called by many extending classes to display basic information in the dialog common to all images.
     *
     * @param  dialog  Area where image information is to be displayed.
     * @param  matrix  Transformation matrix
     */
    public void displayPrimaryInfo(JDialogText dialog, TransMatrix matrix) {
        dialog.setMessage("\n                     Image information\n\n");

        for (int i = 0; i < extents.length; i++) {
            dialog.append("Dimension " + i + ":          " + extents[i] + "\n");
        }

        dialog.append("Type:                 " + ModelStorageBase.getBufferTypeStr(dataType) + "\n");

        if (!ModelImage.isColorImage(getDataType())) {
            dialog.append("Min:                  " + min + "\n");
            dialog.append("Max:                  " + max + "\n");
        } else {
            dialog.append("Min red:              " + minR + "\n");
            dialog.append("Max red:              " + maxR + "\n");
            dialog.append("Min green:            " + minG + "\n");
            dialog.append("Max green:            " + maxG + "\n");
            dialog.append("Min blue:             " + minB + "\n");
            dialog.append("Max blue:             " + maxB + "\n");
        }

        dialog.append("Modality:             " + modalityStr[modality] + "\n");
        dialog.append("Image origin upper left corner of image - right hand rule\n");

        for (int i = 0; i < origin.length; i++) {

            switch (i) {

                case 0:
                    dialog.append(" Origin X (left to right) :         " + origin[0] + "\n");
                    break;

                case 1:
                    dialog.append(" Origin Y (top to bottom) :         " + origin[1] + "\n");
                    break;

                case 2:
                    dialog.append(" Origin Z:(into the screen):        " + origin[2] + "\n");
                    break;

                case 3:
                    dialog.append(" Origin T:(time):                   " + origin[3] + "\n");
                    break;
            }
        }

        dialog.append("Orientation:          ");

        switch (imageOrientation) {

            case AXIAL:
                dialog.append("Axial \n");
                break;

            case CORONAL:
                dialog.append("Coronal \n");
                break;

            case SAGITTAL:
                dialog.append("Sagittal \n");
                break;

            default:
                dialog.append("Unknown \n");
        }

        dialog.append("X axis orientation:   ");

        switch (axisOrientation[0]) {

            case ORI_R2L_TYPE:
                dialog.append("right to left \n");
                break;

            case ORI_L2R_TYPE:
                dialog.append("left to right \n");
                break;

            case ORI_A2P_TYPE:
                dialog.append("anterior to posterior \n");
                break;

            case ORI_P2A_TYPE:
                dialog.append("posterior to anterior \n");
                break;

            case ORI_I2S_TYPE:
                dialog.append("inferior to superior \n");
                break;

            case ORI_S2I_TYPE:
                dialog.append("superior to inferior \n");
                break;

            default:
                dialog.append("unknown to unknown \n");
        }

        dialog.append("Y axis orientation:   ");

        switch (axisOrientation[1]) {

            case ORI_R2L_TYPE:
                dialog.append("right to left \n");
                break;

            case ORI_L2R_TYPE:
                dialog.append("left to right \n");
                break;

            case ORI_A2P_TYPE:
                dialog.append("anterior to posterior \n");
                break;

            case ORI_P2A_TYPE:
                dialog.append("posterior to anterior \n");
                break;

            case ORI_I2S_TYPE:
                dialog.append("inferior to superior \n");
                break;

            case ORI_S2I_TYPE:
                dialog.append("superior to inferior \n");
                break;

            default:
                dialog.append("unknown to unknown \n");
        }

        dialog.append("Z axis orientation:   ");

        switch (axisOrientation[2]) {

            case ORI_R2L_TYPE:
                dialog.append("right to left \n");
                break;

            case ORI_L2R_TYPE:
                dialog.append("left to right \n");
                break;

            case ORI_A2P_TYPE:
                dialog.append("anterior to posterior \n");
                break;

            case ORI_P2A_TYPE:
                dialog.append("posterior to anterior \n");
                break;

            case ORI_I2S_TYPE:
                dialog.append("inferior to superior \n");
                break;

            case ORI_S2I_TYPE:
                dialog.append("superior to inferior \n");
                break;

            default:
                dialog.append("unknown to unknown \n");
        }

        for (int i = 0; i < extents.length; i++) {

            if (dimResolutions[i] > 0.0) {
                dialog.append("Pixel resolution " + i + ":  " + dimResolutions[i] + "  ");

                switch (unitsOfMeasure[i]) {

                    case INCHES:
                        dialog.append("Inches \n");
                        break;

                    case MILLIMETERS:
                        dialog.append("Millimeters \n");
                        break;

                    case CENTIMETERS:
                        dialog.append("Centimeters \n");
                        break;

                    case METERS:
                        dialog.append("Meters \n");
                        break;

                    case KILOMETERS:
                        dialog.append("Kilometers \n");
                        break;

                    case MILES:
                        dialog.append("Miles \n");
                        break;

                    case ANGSTROMS:
                        dialog.append("Angstroms \n");
                        break;

                    case NANOMETERS:
                        dialog.append("Nanometers \n");
                        break;

                    case MICROMETERS:
                        dialog.append("Micrometers \n");
                        break;

                    case NANOSEC:
                        dialog.append("Nanoseconds \n");
                        break;

                    case MICROSEC:
                        dialog.append("Microseconds \n");
                        break;

                    case MILLISEC:
                        dialog.append("Milliseconds \n");
                        break;

                    case SECONDS:
                        dialog.append("Seconds \n");
                        break;

                    case MINUTES:
                        dialog.append("Minutes \n");
                        break;

                    case HOURS:
                        dialog.append("Hours \n");
                        break;

                    default:
                        dialog.append("Unknown \n");
                        break;
                } // end of switch(measure[i])
            } // end of if (resolutions[i] > 0.0)
        } // for (int i=0; i < 5; i++)

        if (extents.length >= 3) {
            dialog.append("Slice spacing:       " + sliceSpacing + "\n");
        }

        if (endianess == FileBase.LITTLE_ENDIAN) {
            dialog.append("Endianess: Little Endian \n");
        } else {
            dialog.append("Endianess: Big Endian \n");
        }

        if (matrix != null) {

            // when using displayAboutInfo(dialog) this doesn't appear
            // calling prg might use an editing panel to adjust this matrix
            dialog.append("Matrix: \n" + matrix.matrixToString(10, 4) + "\n");
        }
    }

    /**
     * Prepares this class for cleanup.
     */
    public void finalize() {
        axisOrientation = null;
        origin = null;

        unitsOfMeasure = null;
        dimResolutions = null;

        fileName = null;
        fileDir = null;
        fileSuffix = null;

        try {
            super.finalize();
        } catch (Throwable er) { }
    }

    /* ********************************************************************** */

    /* ****************************** Accessors ***************************** */

    /* ********************************************************************** */

    /**
     * Returns the area unit for the data. Assumes both dimensions are the same units.
     *
     * @return  String associated volume unit of measure.
     */
    public String getAreaUnitsOfMeasureStr() {
        String mStr = new String();
        int measure;

        measure = getUnitsOfMeasure(0);

        if (measure == FileInfoBase.INCHES) {
            mStr = " inches^2";
        } else if (measure == FileInfoBase.ANGSTROMS) {
            mStr = " A^2";
        } else if (measure == FileInfoBase.NANOMETERS) {
            mStr = " nm^2";
        } else if (measure == FileInfoBase.MICROMETERS) {
            mStr = " um^2";
        } else if (measure == FileInfoBase.MILLIMETERS) {
            mStr = " mm^2";
        } else if (measure == FileInfoBase.CENTIMETERS) {
            mStr = " cm^2";
        } else if (measure == FileInfoBase.METERS) {
            mStr = " m^2";
        } else if (measure == FileInfoBase.KILOMETERS) {
            mStr = " km^2";
        } else if (measure == FileInfoBase.MILES) {
            mStr = " miles^2";
        } else {
            mStr = "Unknown";
        }

        return mStr;
    }

    /**
     * Get the direction for accessing each axis of data. This is based on the values in the axisOrientation array.
     *
     * @return  int[] Array of +/-1 values with one entry for each axis. A -1 indicates that the direction is reversed.
     */
    public int[] getAxisDirection() {

        int[] axisOrient = getAxisOrientation();
        int[] direction = new int[axisOrient.length];

        for (int i = 0; i < axisOrient.length; i++) {

            if ((axisOrient[i] == ORI_R2L_TYPE) || (axisOrient[i] == ORI_A2P_TYPE) || (axisOrient[i] == ORI_I2S_TYPE)) {
                direction[i] = 1;
            } else {
                direction[i] = -1;
            }
        }

        return direction;
    }

    /**
     * Returns orientation of each axis.
     *
     * @return  int[] orientation of each axis
     *
     * @see     #setAxisOrientation(int[])
     */
    public int[] getAxisOrientation() {
        return axisOrientation;
    }

    /**
     * Returns orientation of entered axis.
     *
     * @param   axis  Axis to get orientation for
     *
     * @return  int orientation of specified axis
     *
     * @see     #setAxisOrientation(int, int)
     */
    public int getAxisOrientation(int axis) {
        return axisOrientation[axis];
    }

    /**
     * Gets the compression type.
     *
     * @return  the compression type
     */
    public int getCompressionType() {
        return this.compressionType;
    }

    /**
     * Returns data type.
     *
     * @return  int type of data in file
     */
    public final int getDataType() {
        return dataType;
    }

    /**
     * Descibes file endianess.
     *
     * @return  boolean <code>false</code> = litteEndian format <code>true</code> = bigEndian format
     */
    public final boolean getEndianess() {
        return endianess;
    }

    /**
     * Returns the dimensionality of the image.
     *
     * @return  int[] units (Inches or millimeters);
     */
    public final int[] getExtents() {
        return extents;
    }

    /**
     * Returns counter to start of image data.
     *
     * @return  String that indicates location of the file
     */
    public final String getFileDirectory() {
        return fileDir;
    }

    /**
     * Returns file format.
     *
     * @return  int file format (TIFF, raw, Analyze...)
     */
    public final int getFileFormat() {
        return fileFormat;
    }

    /**
     * Returns the file name.
     *
     * @return  String indicating file name
     */
    public final String getFileName() {
        return fileName;
    }

    /**
     * Returns the file suffix.
     *
     * @return  String representing the filename suffix
     */
    public final String getFileSuffix() {
        return fileSuffix;
    }

    /**
     * Returns the image orientation.
     *
     * @return  int representing orientation
     */
    public final int getImageOrientation() {
        return imageOrientation;
    }

    /**
     * Return whether or not the image is 2.5D (Z resolution).
     *
     * @return  boolean is 2.5 D
     */
    public final boolean getIs2_5D() {

        if ((unitsOfMeasure.length > 2) && ((unitsOfMeasure[2] > 10) && (unitsOfMeasure[2] < 17))) {
            return true;
        }

        return false;
    }

    /**
     * Returns max pixel value of the image.
     *
     * @return  double Returns double max pixel value of the image
     */
    public final double getMax() {
        return max;
    }

    /**
     * Returns max blue pixel value of the image.
     *
     * @return  double Returns double blue max pixel value of the image
     */
    public final double getMaxB() {
        return maxB;
    }

    /**
     * Returns max green pixel value of the image.
     *
     * @return  double Returns double green max pixel value of the image
     */
    public final double getMaxG() {
        return maxG;
    }

    /**
     * Returns max red pixel value of the image.
     *
     * @return  double Returns double red max pixel value of the image
     */
    public final double getMaxR() {
        return maxR;
    }

    /**
     * Returns min pixel value of the image.
     *
     * @return  double Returns double min pixel value of the image
     */
    public final double getMin() {
        return min;
    }

    /**
     * Returns min blue pixel value of the image.
     *
     * @return  double Returns double blue min pixel value of the image
     */
    public final double getMinB() {
        return minB;
    }

    /**
     * Returns min green pixel value of the image.
     *
     * @return  couble Returns double green min pixel value of the image
     */
    public final double getMinG() {
        return minG;
    }

    /**
     * Returns min red pixel value of the image.
     *
     * @return  couble Returns double red min pixel value of the image
     */
    public final double getMinR() {
        return minR;
    }

    /**
     * Returns the modality.
     *
     * @return  int indicating modality
     */
    public final int getModality() {
        return modality;
    }

    /**
     * Returns whether or not the image is in multiple files (tiff).
     *
     * @return  boolean true indicates multiple files, false o.w.
     */
    public final boolean getMultiFile() {
        return multiFile;
    }

    /**
     * Returns the header offset.
     *
     * @return  int header offset
     */
    public final int getOffset() {
        return offset;
    }

    /**
     * Returns the origin.
     *
     * @return  float[] the origin
     */
    public float[] getOrigin() {
        return origin;
    }

    /**
     * Returns the origin value of the requested axis.
     *
     * @param   axis  requested axis; x is 0, y is 1, z is 2, and t is 3.
     *
     * @return  float orientation of axis
     */
    public float getOrigin(int axis) {

        try {
            return origin[axis];
        } catch (ArrayIndexOutOfBoundsException aiiobe) {
            throw aiiobe;
        } catch (NullPointerException npe) {
            throw npe;
        }
    }

    /**
     * Gets the origin of a particular slice; resets for the z dimension.
     *
     * @param   slice  Z-dimension slice.
     *
     * @return  float[] New start locations
     */
    public float[] getOriginAtSlice(int slice) {
        float[] newOrigin = new float[4];

        for (int i = 0; i < 3; i++) {
            newOrigin[i] = origin[i];
        }

        int direction = 1;

        if ((axisOrientation[2] == ORI_L2R_TYPE) || (axisOrientation[2] == ORI_P2A_TYPE) ||
                (axisOrientation[2] == ORI_S2I_TYPE)) {
            direction = -1;
        }

        newOrigin[2] = origin[2] + (direction * dimResolutions[2] * slice);

        return newOrigin;
    }

    /**
     * Photometric interpretion.
     *
     * <table border=true>
     *   <tr>
     *     <td>1 indicates</td>
     *     <td>0 is black</td>
     *   </tr>
     *   <tr>
     *     <td>0 indicates</td>
     *     <td>0 is white</td>
     *   </tr>
     *   <tr>
     *     <td>2</td>
     *     <td>RGB</td>
     *   </tr>
     *   <tr>
     *     <td>3</td>
     *     <td>indexed color LUT is saved with image</td>
     *   </tr>
     *   <tr>
     *     <td>4</td>
     *     <td>Transparency Mask</td>
     *   </tr>
     * </table>
     *
     * @return  short Returns interpretation
     */
    public final short getPhotometric() {
        return photometric;
    }

    /**
     * Returns pixel pad value.
     *
     * @return  Short Returns pixel pad value
     */
    public final Short getPixelPadValue() {
        return pixelPadValue;
    }

    /**
     * Returns the intercept.
     *
     * @return  double rescale intercept
     */
    public final double getRescaleIntercept() {
        return rescaleIntercept;
    }

    /**
     * Returns the slope.
     *
     * @return  double rescale slope
     */
    public final double getRescaleSlope() {
        return rescaleSlope;
    }

    /**
     * Returns each dimension's resolution.
     *
     * @return  float[] dimension resolutions
     */
    public final float[] getResolutions() {
        return dimResolutions;
    }

    /**
     * Returns the space between neighboring slices.
     *
     * @return  float slice spacing
     */
    public final float getSliceSpacing() {
        return sliceSpacing;
    }

    /**
     * Returns the transform ID associated with the matrix.
     *
     * @return  int transform ID
     */
    public final int getTransformID() {
        return transformID;
    }

    /**
     * Returns the units of measure.
     *
     * @return  int[] units (Inches or millimeters);
     */
    public final int[] getUnitsOfMeasure() {
        return unitsOfMeasure;
    }

    /**
     * Returns the units of measure.
     *
     * @param   dim  dimension index
     *
     * @return  int units (Inches or millimeters);
     */
    public int getUnitsOfMeasure(int dim) {

        // could try catch array out of bounds ...
        if ((unitsOfMeasure != null) && (dim < unitsOfMeasure.length) && (dim >= 0)) {
            return unitsOfMeasure[dim];
        } else {
            Preferences.debug("Units of measure array is null.\n");

            return UNKNOWN_MEASURE;
        }
    }


    /**
     * Returns the volume unit for the data. Assumes all three dimensions are the same units.
     *
     * @return  String associated volume unit of measure.
     */
    public String getVolumeUnitsOfMeasureStr() {
        String mStr = new String();
        int measure;

        measure = getUnitsOfMeasure(0);

        if (measure == FileInfoBase.INCHES) {
            mStr = " inches^3";
        } else if (measure == FileInfoBase.ANGSTROMS) {
            mStr = " A^3";
        } else if (measure == FileInfoBase.NANOMETERS) {
            mStr = " nm^3";
        } else if (measure == FileInfoBase.MICROMETERS) {
            mStr = " um^3";
        } else if (measure == FileInfoBase.MILLIMETERS) {
            mStr = " mm^3";
        } else if (measure == FileInfoBase.CENTIMETERS) {
            mStr = " cm^3";
        } else if (measure == FileInfoBase.METERS) {
            mStr = " m^3";
        } else if (measure == FileInfoBase.KILOMETERS) {
            mStr = " km^3";
        } else if (measure == FileInfoBase.MILES) {
            mStr = " miles^3";
        } else {
            mStr = "Unknown";
        }

        return mStr;
    }

    /**
     * Sets (copies) orientation of each axis.
     *
     * @param  axOrient  axis orientation array
     *
     * @see    #getAxisOrientation()
     */
    public void setAxisOrientation(int[] axOrient) {

        if ((axOrient == null) || (axOrient.length != 3)) {
            Preferences.debug("Axis orientations array must be of length 3.\n");

            return;
        }

        axisOrientation[0] = axOrient[0];
        axisOrientation[1] = axOrient[1];
        axisOrientation[2] = axOrient[2];
    }

    /**
     * Sets the image orientation in the specified axis. Creates the axisOrientation if the array has not yet been
     * created.
     *
     * @param  axOrient  orientation
     * @param  axis      axis of orientation; x is 0, y is 1, z is 2.
     */
    public void setAxisOrientation(int axOrient, int axis) {

        // System.out.println("axis orient is " + axOrient);
        if ((axis < 0) || (axis > 2)) {
            Preferences.debug("Error: Axis must be 0, 1, or 2.\n");

            return;
        }

        if ((axOrient == ORI_UNKNOWN_TYPE) || (axOrient == ORI_A2P_TYPE) || (axOrient == ORI_P2A_TYPE) ||
                (axOrient == ORI_R2L_TYPE) || (axOrient == ORI_L2R_TYPE) || (axOrient == ORI_S2I_TYPE) ||
                (axOrient == ORI_I2S_TYPE)) {
            axisOrientation[axis] = axOrient;
        } else {
            axisOrientation[axis] = ORI_UNKNOWN_TYPE;
            Preferences.debug(axOrient + " is an invalid axis orientation.\n");
        }
    }

    /**
     * Sets the compression type.
     *
     * @param  type  compression type
     */
    public void setCompressionType(int type) {
        this.compressionType = type;
    }

    /**
     * Sets format of image data.
     *
     * @param  type  data type defined in ModelStorageBase
     */
    public final void setDataType(int type) {
        dataType = type;
    }

    /**
     * Describes file endianess.
     *
     * @param  endness  endianess of the file format
     */
    public void setEndianess(boolean endness) {
        endianess = endness;
    }

    /**
     * Sets dimensionality of the images.
     *
     * @param  dims  dimensionality for x,y, and z ... dimensions
     */
    public final void setExtents(int[] dims) {

        if (dims != null) {
            extents = (int[]) dims.clone();
        }
    }

    /**
     * Sets dimensionality for image, on a per dimension basis.
     *
     * @param  extent  Extent of this dimension
     * @param  dim     Dimension to set extent in
     */
    public void setExtents(int extent, int dim) {
        extents[dim] = extent;
    }

    /**
     * Sets the file directory.
     *
     * @param  fDir  file directory
     */
    public final void setFileDirectory(String fDir) {
        fileDir = fDir;
    }

    /**
     * Sets the file format.
     *
     * @param  format  File format
     */
    public final void setFileFormat(int format) {
        fileFormat = format;
    }

    /**
     * Sets the file name.
     *
     * @param  fname  image file name
     */
    public void setFileName(String fname) {
        fileName = fname;
    }

    /**
     * Sets the file suffix.
     *
     * @param  suffix  file suffix
     */
    public final void setFileSuffix(String suffix) {
        fileSuffix = suffix;
    }

    /**
     * Sets the image orientation.
     *
     * @param  orient  Orientation.
     */
    public void setImageOrientation(int orient) {
        imageOrientation = orient;
    }

    /**
     * Sets max pixel value of image.
     *
     * @param  Max  max pixel value
     */
    public void setMax(double Max) {
        max = Max;
    }

    /**
     * Sets max blue pixel value of image.
     *
     * @param  Max  max blue pixel value
     */
    public final void setMaxB(double Max) {
        maxB = Max;
    }

    /**
     * Sets max green pixel value of image.
     *
     * @param  Max  max green pixel value
     */
    public void setMaxG(double Max) {
        maxG = Max;
    }

    /**
     * Sets max red pixel value of image.
     *
     * @param  Max  max red pixel value
     */
    public void setMaxR(double Max) {
        maxR = Max;
    }

    /**
     * Sets min pixel value of image.
     *
     * @param  Min  Min pixel value
     */
    public final void setMin(double Min) {
        min = Min;
    }

    /**
     * Sets min blue pixel value of image.
     *
     * @param  Min  min blue pixel value
     */
    public final void setMinB(double Min) {
        minB = Min;
    }

    /**
     * Sets min green pixel value of image.
     *
     * @param  Min  min green pixel value
     */
    public final void setMinG(double Min) {
        minG = Min;
    }

    /**
     * Sets min red pixel value of image.
     *
     * @param  Min  min red pixel value
     */
    public final void setMinR(double Min) {
        minR = Min;
    }

    /**
     * Sets the modality.
     *
     * @param  mod  modality
     */
    public final void setModality(int mod) {
        modality = mod;
    }

    /**
     * Sets the flag for multiple files.
     *
     * @param  flag  <code>true</code> indicates multiple files for image, <code>false</code> o.w.
     */
    public final void setMultiFile(boolean flag) {
        multiFile = flag;
    }

    /**
     * Sets the header offset.
     *
     * @param  off  the header offset
     */
    public void setOffset(int off) {
        offset = off;
    }

    /**
     * Sets the origin.
     *
     * @param  originlocat  origin location array
     *
     * @see    #getStartLocations()
     */
    public void setOrigin(float[] originlocat) {

        if ((originlocat == null) || (originlocat.length > 4)) {
            Preferences.debug("Start locations array must be of length less than or equal to 4.\n");

            return;
        }

        origin = (float[]) originlocat.clone();
    }

    /**
     * Sets the start location in the specified axis. creates the startLocations if the arrray has not yet been created.
     *
     * @param  originCoord  start location
     * @param  axis         axis of orientation; x is 0, y is 1, z is 2.
     *
     * @see    #getStartLocations(int)
     */
    public void setOrigin(float originCoord, int axis) {

        if ((axis < 0) || (axis > 3)) {
            Preferences.debug("Error: Axis must be 0, 1, 2, or 3.\n");

            return;
        }

        origin[axis] = originCoord;
    }

    /**
     * Sets photometric interpretation.
     *
     * <table border=true>
     *   <tr>
     *     <td>1 indicates</td>
     *     <td>0 is black</td>
     *   </tr>
     *   <tr>
     *     <td>0 indicates</td>
     *     <td>0 is white</td>
     *   </tr>
     *   <tr>
     *     <td>2</td>
     *     <td>RGB</td>
     *   </tr>
     *   <tr>
     *     <td>3</td>
     *     <td>indexed color LUT is saved with image</td>
     *   </tr>
     * </table>
     *
     * @param  value  photometric value
     */
    public void setPhotometric(short value) {
        photometric = value;
    }

    /**
     * Sets pixel pad value: used in some Dicom images.
     *
     * @param  value  pixel pad value
     */
    public final void setPixelPadValue(Short value) {
        pixelPadValue = value;
    }

    /**
     * Sets the rescale intercept.
     *
     * @param  intercept  the intercept
     */
    public final void setRescaleIntercept(double intercept) {
        rescaleIntercept = intercept;
    }

    /**
     * Sets the rescale slope.
     *
     * @param  slope  the slope
     */
    public final void setRescaleSlope(double slope) {
        rescaleSlope = slope;
    }

    /**
     * Sets the resolutions of the image.
     *
     * @param  resolutions  resolution object
     */
    public final void setResolutions(float[] resolutions) {

        if (resolutions != null) {
            dimResolutions = (float[]) resolutions.clone();
        }
    }

    /**
     * Sets the resolutions of the image, on a per dimension basis.
     *
     * @param  resolution  Resolution for the dimension
     * @param  dim         Dimension to set resolution in
     */
    public void setResolutions(float resolution, int dim) {
        dimResolutions[dim] = resolution;
    }

    /**
     * Sets the slice spacing for the image.
     *
     * @param  spacing  Spacing between slices
     */
    public void setSliceSpacing(float spacing) {
        sliceSpacing = spacing;
    }

    /**
     * Sets the transform ID for the matrix.
     *
     * @param  t_id  transform ID
     */
    public void setTransformID(int t_id) {
        transformID = t_id;
    }

    /**
     * Sets (copies) units of measure for image.
     *
     * @param  unitMeasure  unit of measure for a specified dimension
     */
    public final void setUnitsOfMeasure(int[] unitMeasure) {

        if (unitMeasure != null) {
            unitsOfMeasure = (int[]) unitMeasure.clone();
        }
    }

    /**
     * Sets units of measure for image, on a per dimension basis.
     *
     * @param  unitMeasure  Unit of measure for the dimension
     * @param  dim          Dimension to set unit of measure in
     */
    public final void setUnitsOfMeasure(int unitMeasure, int dim) {
        unitsOfMeasure[dim] = unitMeasure;
    }

    /**
     * Gives the information contained in FileInfo in a string.
     *
     * @return  String information contained in the FileInfo object
     */
    public String toString() {
        String s = "";

        s += "File info:\n";
        s += "Modality: ";
        s += FileInfoBase.getModalityStr(modality) + "\n";
        s += "File directory: " + fileDir + "\nFile name: " + fileName + "\n";
        s += "File suffix: " + fileSuffix + "\n";
        s += "File format: ";
        s += FileBase.getFileFormatStr(fileFormat) + "\n";
        s += "Data type: ";
        s += ModelStorageBase.getBufferTypeStr(dataType) + "\n";
        s += "Offset: " + offset + "\n";
        s += "Endianess: ";

        if (endianess == FileBase.LITTLE_ENDIAN) {
            s += "LITTLE_ENDIAN\n";
        } else {
            s += "BIG_ENDIAN\n";
        }

        s += "Extents: ";

        for (int i = 0; i < extents.length; i++) {
            s += extents[i] + " ";
        }

        s += "\nResolutions: ";

        for (int i = 0; i < dimResolutions.length; i++) {
            s += dimResolutions[i] + " ";
        }

        s += "\nUnits of measure: ";

        for (int i = 0; i < unitsOfMeasure.length; i++) {
            s += FileInfoBase.getUnitsOfMeasureStr(unitsOfMeasure[i]) + " ";
        }

        s += "\nImage orientation: ";
        s += FileInfoBase.getImageOrientationStr(imageOrientation);

        s += "\nAxis orientations: ";

        for (int i = 0; i < axisOrientation.length; i++) {
            s += FileInfoBase.getAxisOrientationStr(axisOrientation[i]) + " ";
        }

        s += "\nImage origin locations: ";

        for (int i = 0; i < origin.length; i++) {
            s += origin[i] + " ";
        }

        s += "\nMin: " + min + "\nMax: " + max + "\n";

        return s;
    }


}
