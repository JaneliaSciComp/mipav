package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.util.*;


/**
 * This structures contains the information that describes how an analyze image is stored on disk. Analyze Version 7.5.
 * We have extended this format to store image orientation and origin. We have used unused variables to store these
 * data. Almost all programs ignore these variables and should not have any problems reading images saved with MIPAV,
 * except SPM. A new format for MIPAV is now XML based.
 *
 * <p>Unused1 - X-axis orientation @see FileInfoBase for static variables that are set. Unused2 - Y-axis orientation //
 * All three unused variables are of short type. Unused3 - Z-axis orientation</p>
 *
 * <p>Funused1 - X-axis starting location (origin from upper left-hand corner) Funused2 - Y-axis starting location
 * (origin from upper left-hand corner) Funused3 - Z-axis starting location (origin is the first image slice)</p>
 *
 * <p>RGB analyze images are store in chunky format rgb, rgb, rgb ......</p>
 *
 * <p>Note that there is a char data_type[10] field and a short datatype field.</p>
 *
 * @version  1.0 July, 2002
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      FileAnalyze
 */
public class FileInfoAnalyze extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7319414471012796383L;

    /** Analyze file format define use to indicate no image data type. */
    public static final short DT_NONE = 0;

    /** AAnalyze file format define use to indicate undefined image data type. */
    public static final short DT_UNKNOWN = 0;

    /** Analyze file format define use to indicate binary (1 bit) image data type. */
    public static final short DT_BINARY = 1;

    /** Analyze file format define use to indicate unsigned byte (char) (8 bit) image data type. */
    public static final short DT_UNSIGNED_CHAR = 2;

    /** Analyze file format define use to indicate short (16 bit) image data type. */
    public static final short DT_SIGNED_SHORT = 4;

    /** Analyze file format define use to indicate short (16 bit) image data type. */
    public static final short DT_UNSIGNED_SHORT = 4;

    /** Analyze file format define use to indicate integer (32 bit) image data type. */
    public static final short DT_SIGNED_INT = 8;

    /** Analyze file format define use to indicate float (32 bit) image data type. */
    public static final short DT_FLOAT = 16;

    /** Analyze file format define use to indicate complex (64 bit) image data type. */
    public static final short DT_COMPLEX = 32;

    /** Analyze file format define use to indicate double (64 bit) image data type. */
    public static final short DT_DOUBLE = 64;

    /** Analyze file format define use to indicate color (RGB - 24 bits) image data type. */
    public static final short DT_RGB = 128;

    /** Analyze file format define use to indicate _ALL_ image data type. */
    public static final short DT_ALL = 255;

    /**
     * The data_history substructure is not required, but the orient element is used to indicate individual slice
     * orientation and determines whetther the Analyze Movie program will attempt to flip the images before displaying a
     * movie sequence. 0 - transverse unflipped 1 - coronal unflipped 2 - sagitttal unflipped 3 - transverse flipped 4 -
     * coronal flipped 5 - sagitttal flipped
     */
    public static final byte UNKNOWN_ORIENT = -1;

    /** DOCUMENT ME! */
    public static final byte TRANSVERSE_UNFLIPPED = 0;

    /** DOCUMENT ME! */
    public static final byte CORONAL_UNFLIPPED = 1;

    /** DOCUMENT ME! */
    public static final byte SAGITTAL_UNFLIPPED = 2;

    /** DOCUMENT ME! */
    public static final byte TRANSVERSE_FLIPPED = 3;

    /** DOCUMENT ME! */
    public static final byte CORONAL_FLIPPED = 4;

    /** DOCUMENT ME! */
    public static final byte SAGITTAL_FLIPPED = 5;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private String aux_file = null;

    /** Bits per pixel. Valid values include: 1, 8, 16, 32, 64, 24(rgb). */
    private short bitpix = -1;

    /** Range of calibration values. */
    private float cal_max = 0;

    /** Values of 0.0 for both fields imply that no calibration min and max values are used ! */
    private float cal_min = 0;

    /** Specifies the name of the calibration unit. */
    private String cal_units = null;

    /** Indicates if compression is used. Default is -1 indicating no compression */
    private float compressed = -1;

    /** Unknown use 10 chars. */
    private String data_type = null;

    /**
     * Stores the image data type DT_NONE = 0; DT_UNKNOWN = 0; DT_BINARY = 1; DT_UNSIGNED_CHAR = 2; DT_SIGNED_SHORT = 4;
     * DT_UNSIGNED_SHORT = 4; DT_SIGNED_INT = 8; DT_FLOAT = 16; DT_COMPLEX = 32; DT_DOUBLE = 64; DT_RGB = 128; DT_ALL =
     * 255;
     */
    private short datatype = -1;

    /** Name of file with a length of 18 chars. */
    private String db_name = null;

    /** String with a max character length of 80 used to describe image. */
    private String descrip = null;

    /** Unused. */
    private short dim_un0 = -1;
    // public     float   pixdim               = new float[8]; // image resolutions info mm or ms  stored in
    // FileInfoBase  pixdim[0] = number of dimensions  pixdim[1] = voxel width  pixdim[2] = voxel height  pixdim[3] =
    // voxel thickness  pixdim[4] = time

    /** String with a max character length of 10 used to indicate the experiment date. */
    private String exp_date = null;

    /** String with a max character length of 10 used to indicate the experiment time. */
    private String exp_time = null;

    /** Should be 16384. */
    private int extents = 0;

    /** Unknown length. */
    private int field_skip = -1;

    /** MIPAV uses this variable to define the X-axis origin. */
    private float funused1 = -1;

    /** MIPAV uses this variable to define the Y-axis origin. */
    private float funused2 = -1;

    /** MIPAV uses this variable to define the Z-axis origin. */
    private float funused3 = -1;

    /** Unknown use. Length of 10. */
    private String generated = null;

    /** Maximum pixel values for the entire dataset. */
    private int glmax = -99999;

    /** Minimum pixel values for the entire database. */
    private int glmin = -99999;

    /** Unknown use. Length of 3. */
    private String hist_un0 = null;

    /** Unknown use. Length of 1. */
    private char hkey_un0;

    /** Unknown use. */
    private int omax = -1;

    /** Unknown use. */
    private int omin = -1;

    /** DOCUMENT ME! */
    private byte orient = -1;

    /** Unknown use. Length of 10. */
    private String originator = null;

    /** Patient's ID. Length of 10. */
    private String patient_id = null;

    /** I think it means data has uniform spacing (resolutions). */
    private char regular = 'r';

    /** Scan ID Length of 10. */
    private String scannum = null;

    /** Unknown use. */
    private short session_error = -1;

    /** Always of length 348. */
    private int sizeof_hdr = -1;

    /** Unknown use. */
    private int smax = -1;

    /** Unknown use. */
    private int smin = -1;

    /** Unknown use. */
    private int start_field = -1;

    /** MIPAV uses this variable to define the X-axis orientation. */
    private short unused1 = -1;

    /** MIPAV uses this variable to define the Y-axis orientation. */
    private short unused2 = -1;

    /** MIPAV uses this variable to define the Z-axis orientation. */
    private short unused3 = -1;

    /** Unknown use. */
    private float verified = -1;

    /** Unknown use. */
    private int views = -1;

    /** Unknown use. */
    private int vols_added = -1;

    /**
     * Byte offset in the ".img" file at which voxels start This value can be negative to specify that the absolute
     * value is applied for every image in the file.
     */
    // private float vox_offset = -1;

    // public     short   dim[]  = new short[8]; // image dimension data
    // stored in FileInfoBase
    // dim[0] = number of dimensions; usually 4
    // dim[1] = image width
    // dim[2] = image height
    // dim[3] = image depth (# of slices)
    // dim[4] = volumes in image  --- must be one for 3D image

    /** specifies the spatial units of measure for a voxel. */
    private String vox_units = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * File info storage constructor.
     *
     * @param  name       file name
     * @param  directory  directory
     * @param  format     file format
     */
    public FileInfoAnalyze(String name, String directory, int format) {
        super(name, directory, format);
        setOffset(-1);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Displays the file information.
     *
     * @param  dlog    dialog box that is written to
     * @param  matrix  transformation matrix
     */
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix) {
        JDialogFileInfo dialog = (JDialogFileInfo) dlog;
        int[] extents;
        int i;
        int[] editorChoice = new int[1];
        editorChoice[0] = JDialogEditor.STRING;

        dialog.displayAboutInfo(this); // setup layout in the dialog

        extents = super.getExtents();

        for (i = 0; i < extents.length; i++) {
            dialog.appendPrimaryData("Dimension " + i, Integer.toString(extents[i]));
        }

        dialog.appendPrimaryData("Type", ModelStorageBase.getBufferTypeStr(getDataType()));

        if (ModelImage.isColorImage(getDataType())) {
            dialog.appendPrimaryData("Min red", Double.toString(getMinR()));
            dialog.appendPrimaryData("Max red", Double.toString(getMaxR()));
            dialog.appendPrimaryData("Min green", Double.toString(getMinG()));
            dialog.appendPrimaryData("Max green", Double.toString(getMaxG()));
            dialog.appendPrimaryData("Min blue", Double.toString(getMinB()));
            dialog.appendPrimaryData("Max blue", Double.toString(getMaxB()));

        } else {
            dialog.appendPrimaryData("Min", Double.toString(getMin()));
            dialog.appendPrimaryData("Max", Double.toString(getMax()));
        }

        dialog.appendPrimaryData("Modality", FileInfoBase.getModalityStr(getModality()));

        dialog.appendPrimaryData("Orientation", getImageOrientationStr(getImageOrientation()));

        float[] resolutions; // = new float[5];
        resolutions = getResolutions();

        int[] measure; // = new int[5];
        measure = getUnitsOfMeasure();

        for (i = 0; i < extents.length; i++) {

            if (resolutions[i] > 0.0) {
                String pixelRes = "Pixel resolution " + i;
                dialog.appendPrimaryData(pixelRes,
                                         Float.toString(resolutions[i]) + " " + (Unit.getUnitFromLegacyNum(measure[i])).toString());
            } // end of if (resolutions[i] > 0.0)
        } // for (i=0; i < 5; i++)

        if (getEndianess() == FileBase.LITTLE_ENDIAN) {
            dialog.appendPrimaryData("Endianess", "Little Endian");
        } else {
            dialog.appendPrimaryData("Endianess", "Big Endian");
        }

        if (matrix != null) {

            // when using displayAboutInfo(dialog) this doesn't appear
            // calling prg might use an editing panel to adjust this matrix
            dialog.appendPrimaryData("Matrix", matrix.matrixToString(10, 4));
        }

        try { // DB name (usually filename)
            editorChoice[0] = JDialogEditor.ANALYZE_DBNAME;
            dialog.appendSecondaryData("DB Name", db_name.trim(), editorChoice);
        } catch (NullPointerException npe) {
            editorChoice[0] = JDialogEditor.ANALYZE_DBNAME;
            dialog.appendSecondaryData("DB Name", "", editorChoice);
        }

        // description
        try {
            editorChoice[0] = JDialogEditor.ANALYZE_DESCRIPTION;
            dialog.appendSecondaryData("Description", descrip.trim(), editorChoice);
        } catch (NullPointerException npe) {
            editorChoice[0] = JDialogEditor.ANALYZE_DESCRIPTION;
            dialog.appendSecondaryData("Description", "", editorChoice);
        }

        try { // data type
            editorChoice[0] = JDialogEditor.ANALYZE_ORIGINATOR;
            dialog.appendSecondaryData("Data Type", data_type.trim(), editorChoice);
        } catch (NullPointerException npe) {
            editorChoice[0] = JDialogEditor.ANALYZE_ORIGINATOR;
            dialog.appendSecondaryData("Data Type", "", editorChoice);
        }

        if (getOffset() != -1) { // vox offset

            // dialog.append("voxel offset: " + vox_offset + "\n");
            editorChoice[0] = JDialogEditor.FLOAT_STRING;
            dialog.appendSecondaryData("Voxel Offset", Float.toString(getOffset()), editorChoice);
        }

        try { // vox units
            editorChoice[0] = JDialogEditor.ANALYZE_VOX;
            dialog.appendSecondaryData("vox units", vox_units.trim(), editorChoice);
        } catch (NullPointerException npe) {
            editorChoice[0] = JDialogEditor.ANALYZE_VOX;
            dialog.appendSecondaryData("vox units", "", editorChoice);
        }

        try { // cal units
            editorChoice[0] = JDialogEditor.ANALYZE_CAL;
            dialog.appendSecondaryData("cal units", cal_units.trim(), editorChoice);
        } catch (NullPointerException npe) {
            editorChoice[0] = JDialogEditor.ANALYZE_CAL;
            dialog.appendSecondaryData("cal units", "", editorChoice);
        }

        // hack to use the unused portion of the analyze image as image information
        editorChoice[0] = JDialogEditor.ANALYZE_AXIS_ORIENTATION;
        dialog.appendSecondaryData("Axis: x-orientation (Unused1)", getAxisOrientationStr(getUnused1()), editorChoice);
        dialog.appendSecondaryData("Axis: y-orientation (Unused2)", getAxisOrientationStr(getUnused2()), editorChoice);
        dialog.appendSecondaryData("Axis: z-orientation (Unused3)", getAxisOrientationStr(getUnused3()), editorChoice);


        editorChoice[0] = JDialogEditor.FLOAT_STRING;
        dialog.appendSecondaryData("X-origin if saved to MIPAV SCALE if SPM", Float.toString(getFunused1()), editorChoice);
        dialog.appendSecondaryData("Y-origin (Funused2)", Float.toString(getFunused2()), editorChoice);
        dialog.appendSecondaryData("Z-origin (Funused3)", Float.toString(getFunused3()), editorChoice);

        if (cal_min != -1) {
            editorChoice[0] = JDialogEditor.FLOAT_STRING;
            dialog.appendSecondaryData("cal_min", Float.toString(cal_min), editorChoice);
        }

        if (cal_max != -1) {
            editorChoice[0] = JDialogEditor.FLOAT_STRING;
            dialog.appendSecondaryData("cal_max", Float.toString(cal_max), editorChoice);
        }

        if ((glmin != -99999) && (glmax != -99999)) {

            // dialog.append("glmin: " + glmin + "  glmax: " + glmax + "\n");
            editorChoice[0] = JDialogEditor.INT_STRING;
            dialog.appendSecondaryData("glmin", Integer.toString(glmin), editorChoice);
            dialog.appendSecondaryData("glmax", Integer.toString(glmax), editorChoice);
        }

        if (bitpix != -1) {
            dialog.appendSecondaryData("Bits per Pixel", Integer.toString(bitpix));
        }

        if (aux_file != null) {

            if (aux_file.trim().length() > 0) {
                dialog.appendSecondaryData("aux", aux_file.trim());
            }
        }

        editorChoice[0] = JDialogEditor.ANALYZE_ORIENTATION;
        dialog.appendSecondaryData("Orientation", selectOrientationText(getOrientation()), editorChoice);

        try { // originator
            editorChoice[0] = JDialogEditor.ANALYZE_ORIGINATOR;
            dialog.appendSecondaryData("Originator", originator.trim(), editorChoice);
        } catch (NullPointerException npe) {
            editorChoice[0] = JDialogEditor.ANALYZE_ORIGINATOR;
            dialog.appendSecondaryData("Originator", "", editorChoice);
        }

        try { // generated
            editorChoice[0] = JDialogEditor.ANALYZE_ORIGINATOR;
            dialog.appendSecondaryData("Generated", generated.trim(), editorChoice);
        } catch (NullPointerException npe) {
            editorChoice[0] = JDialogEditor.ANALYZE_ORIGINATOR;
            dialog.appendSecondaryData("Generated", "", editorChoice);
        }

        try { // scan number
            editorChoice[0] = JDialogEditor.ANALYZE_ORIGINATOR;
            dialog.appendSecondaryData("Scan Number", scannum.trim(), editorChoice);
        } catch (NullPointerException npe) {
            editorChoice[0] = JDialogEditor.ANALYZE_ORIGINATOR;
            dialog.appendSecondaryData("Scan Number", "", editorChoice);
        }

        try { // patient ID
            editorChoice[0] = JDialogEditor.ANALYZE_ORIGINATOR;
            dialog.appendSecondaryData("Patient ID", patient_id.trim(), editorChoice);
        } catch (NullPointerException npe) {
            editorChoice[0] = JDialogEditor.ANALYZE_ORIGINATOR;
            dialog.appendSecondaryData("Patient ID", "", editorChoice);
        }

        try { // experiment date
            editorChoice[0] = JDialogEditor.ANALYZE_ORIGINATOR;
            dialog.appendSecondaryData("Experiment Date", exp_date.trim(), editorChoice);
        } catch (NullPointerException npe) {
            editorChoice[0] = JDialogEditor.ANALYZE_ORIGINATOR;
            dialog.appendSecondaryData("Experiment Date", "", editorChoice);
        }

        try { // experiment time
            editorChoice[0] = JDialogEditor.ANALYZE_ORIGINATOR;
            dialog.appendSecondaryData("Experiment Time", exp_time.trim(), editorChoice);
        } catch (NullPointerException npe) {
            editorChoice[0] = JDialogEditor.ANALYZE_ORIGINATOR;
            dialog.appendSecondaryData("Experiment Time", "", editorChoice);
        }

        if (hist_un0 != null) {

            if (hist_un0.trim().length() > 0) {
                dialog.appendSecondaryData("History:", hist_un0.trim());
            }
        }

        if (views != -1) {
            editorChoice[0] = JDialogEditor.INT_STRING;
            dialog.appendSecondaryData("Views", Integer.toString(views), editorChoice);
        }

        if (vols_added != -1) {
            editorChoice[0] = JDialogEditor.INT_STRING;
            dialog.appendSecondaryData("Volume Added", Integer.toString(vols_added), editorChoice);
        }

        if (start_field != -1) {
            editorChoice[0] = JDialogEditor.INT_STRING;
            dialog.appendSecondaryData("Start Field", Integer.toString(start_field), editorChoice);
        }

        if (field_skip != -1) {
            editorChoice[0] = JDialogEditor.INT_STRING;
            dialog.appendSecondaryData("Field Skip", Integer.toString(field_skip), editorChoice);
        }

        if (omax != -1) {
            editorChoice[0] = JDialogEditor.INT_STRING;
            dialog.appendSecondaryData("omin", Integer.toString(omin), editorChoice);
            dialog.appendSecondaryData("omax", Integer.toString(omax), editorChoice);
        }

        if (smax != -1) {
            editorChoice[0] = JDialogEditor.INT_STRING;
            dialog.appendSecondaryData("smin", Integer.toString(smin), editorChoice);
            dialog.appendSecondaryData("smax", Integer.toString(smax), editorChoice);
        }

    }

    /**
     * Accessor to the aux_file string.
     *
     * @return  String aux_file
     */
    public String getAuxFile() {
        return aux_file;
    }

    /**
     * Accessor to the bitpix value.
     *
     * @return  short the bitpix value.
     */
    public short getBitPix() {
        return bitpix;
    }

    /**
     * Accessor to cal-max.
     *
     * @return  float cal_max
     */
    public float getCalMax() {
        return cal_max;
    }

    /**
     * Accessor to cal_min.
     *
     * @return  float cal_min
     */
    public float getCalMin() {
        return cal_min;
    }

    /**
     * Provides the string for cal units.
     *
     * @return  String string for cal_units
     */
    public String getCalUnits() {
        return cal_units;
    }

    /**
     * Provides the compressed value.
     *
     * @return  float compressed
     */
    public float getCompressed() {
        return compressed;
    }

    /**
     * Accessor to coded datatype value.
     *
     * @return  short datatype
     */
    public short getDataTypeCode() {
        return datatype;
    }

    /**
     * Accessor to the 10 character string of data-type.
     *
     * @return  String returns data_type
     */
    public String getDataTypeName() {
        return data_type;
    }

    /**
     * Accessor to DB_name.
     *
     * @return  String database name
     */
    public String getDBname() {
        return db_name;
    }

    /**
     * Accessor to the current analyze-image description.
     *
     * @return  String description
     */
    public String getDescription() {
        return descrip;
    }

    /**
     * Accessor to the dim_un0.
     *
     * @return  Returns the dim_un0 variable.
     */
    public short getDim() {
        return dim_un0;
    }

    /**
     * Provides the current experiment date string.
     *
     * @return  String the experiment date.
     */
    public String getExperimentDate() {
        return exp_date;
    }

    /**
     * Provides the current experiment time string.
     *
     * @return  String the experiment time
     */
    public String getExperimentTime() {
        return exp_time;
    }

    /**
     * Provides the current field_skip value.
     *
     * @return  int the field_skip value
     */
    public int getFieldSkip() {
        return field_skip;
    }

    /**
     * Analyze _extents_ value of 16,384.
     *
     * @return  The extents value which always seems to be 16,384.
     */
    public int getFileExtents() {
        return extents;
    }

    /**
     * MIPAV hack to the ANALYZE standard. Retrieves start locaiton from x-axis. To be stored as Funused1.
     *
     * @return  float funused1
     */
    public float getFunused1() {
        return funused1;
    }

    /**
     * MIPAV hack to the ANALYZE standard. Retrieves start locaiton from y-axis. To be stored as Funused2.
     *
     * @return  float funused2
     */
    public float getFunused2() {
        return funused2;
    }

    /**
     * MIPAV hack to the ANALYZE standard. Retrieves start location from z-axis. To be stored as Funused3.
     *
     * @return  float funused3
     */
    public float getFunused3() {
        return funused3;
    }

    /**
     * Provides the current generated string.
     *
     * @return  String generated string
     */
    public String getGenerated() {
        return generated;
    }

    /**
     * Provides the value of glmax.
     *
     * @return  int glmax
     */
    public int getGLmax() {
        return glmax;
    }

    /**
     * Provides the value of glmin.
     *
     * @return  int glmin
     */
    public int getGLmin() {
        return glmin;
    }

    /**
     * Accessor to get the Analyze's hist_un0 string.
     *
     * @return  Returns the hist_un0 string.
     */
    public String getHist() {
        return hist_un0;
    }

    /**
     * Accessor to get the Analyze's hkey_un0 string.
     *
     * @return  Returns the hkey_un0 string.
     */
    public char getHkey() {
        return hkey_un0;
    }

    /**
     * Provides current omax value.
     *
     * @return  int the omax value
     */
    public int getOmax() {
        return omax;
    }

    /**
     * Provides current omin value.
     *
     * @return  int the omin value
     */
    public int getOmin() {
        return omin;
    }

    /**
     * Provides the current orientation value.
     *
     * @return  byte orientation value
     */
    public byte getOrientation() {
        return orient;
    }

    /**
     * Provides the current originator string.
     *
     * @return  String originator string
     */
    public String getOriginator() {
        return originator;
    }

    /**
     * Provides the current patient id.
     *
     * @return  String the patient id
     */
    public String getPatientID() {
        return patient_id;
    }

    /**
     * Provides the value of regular character.
     *
     * @return  char value of regular character
     */
    public char getRegular() {
        return regular;
    }

    /**
     * Provides the current scannum string.
     *
     * @return  String current scannum string
     */
    public String getScanNum() {
        return scannum;
    }

    /**
     * Provide the value of session err.
     *
     * @return  short session_error
     */
    public short getSessionErr() {
        return session_error;
    }

    /**
     * Provides the current scannum string.
     *
     * @return  String current scannum string
     */
    public int getSizeOfHeader() {
        return sizeof_hdr;
    }

    /**
     * Provides current smax value.
     *
     * @return  int the smax value
     */
    public int getSmax() {
        return smax;
    }

    /**
     * Provides current smin value.
     *
     * @return  int the smin value
     */
    public int getSmin() {
        return smin;
    }

    /**
     * Provides the current value for the start_field.
     *
     * @return  int the start_field
     */
    public int getStartField() {
        return start_field;
    }

    /**
     * provides current unused1 value.
     *
     * @return  short unused1 value
     */
    public short getUnused1() {
        return unused1;
    }

    /**
     * Provides current unused2 value.
     *
     * @return  short unused2 value
     */
    public short getUnused2() {
        return unused2;
    }

    /**
     * Provides current unused3 value.
     *
     * @return  short unused3 value
     */
    public short getUnused3() {
        return unused3;
    }

    /**
     * Provides the verified value.
     *
     * @return  The Analyze verified parameter.
     */
    public float getVerified() {
        return verified;
    }

    /**
     * Provides current views value.
     *
     * @return  The view parameter.
     */
    public int getViews() {
        return views;
    }

    /**
     * Provides the current vols_added value.
     *
     * @return  The vols_added
     */
    public int getVolsAdded() {
        return vols_added;
    }

    /**
     * Provides the string for vox-units.
     *
     * @return  The vox_units parameter
     */
    public String getVoxUnits() {
        return vox_units;
    }

    /**
     * Supplies auxiliary-file string; permits no more than 24 characters.
     *
     * @param  auxFile  T
     */
    public void setAuxFile(String auxFile) {
        aux_file = setString(auxFile, 24);
    }

    /**
     * Sets orientation of each axis.
     *
     * @param  axOrient  axis orientation array
     *
     * @see    FileInfoBase#getAxisOrientation()
     */
    public void setAxisOrientation(int[] axOrient) {

        if (axOrient.length != 3) {
            Preferences.debug("Axis orientations array must be of length 3.\n", Preferences.DEBUG_FILEIO);

            return;
        }

        for (int i = 0; i < axOrient.length; i++) {
            setAxisOrientation(axOrient[i], i);
        }
    }

    /**
     * Sets the image orientation in the specified axis. Creates the axisOrientation if the array has not yet been
     * created.
     *
     * @param  axOrient  orientation
     * @param  axis      axis of orientation; x is 0, y is 1, z is 2.
     */
    public void setAxisOrientation(int axOrient, int axis) {

        if ((axis < 0) || (axis > 2)) {
            Preferences.debug("Error: Axis must be 0, 1, or 2.\n", Preferences.DEBUG_FILEIO);

            return;
        }

        if (axis == 0) { // x-axis
            setUnused1(axOrient);
        } else if (axis == 1) { // y-axis
            setUnused2(axOrient);
        } else if (axis == 2) { // z-axis
            setUnused3(axOrient);
        }
    }

    /**
     * Sets bitpix; any value other than 1, 8, 16, 32, 64, or 24 gets set to the dissalowed trap value, -1.
     *
     * @param  bp  DOCUMENT ME!
     */
    public void setBitPix(short bp) {

        if ((bp == 1) || (bp == 8) || (bp == 16) || (bp == 32) || (bp == 64) || (bp == 24)) {
            bitpix = bp;
        } else {
            bitpix = -1;
        } // a disallowed trap value
    }

    /**
     * sets cal-max. if supplied value is less than cal-min, the cal-min gets reset to the supplied value as well, so
     * that cal-min is still no greater than cal-max.
     *
     * @param  cal  DOCUMENT ME!
     */
    public void setCalMax(float cal) {
        cal_max = cal;

        if (cal_max < cal_min) {
            cal_min = cal_max;
        }
    }

    /**
     * Sets cal-min. if supplied value is greater than cal-max, the cal-max gets reset to the supplied value as well, so
     * that cal-max is still no less than cal-min.
     *
     * @param  cal  DOCUMENT ME!
     */
    public void setCalMin(float cal) {
        cal_min = cal;

        if (cal_min > cal_max) {
            cal_max = cal_min;
        }
    }

    /**
     * Allows no more than 8 characters for the string describing the cal units.
     *
     * @param  cal  DOCUMENT ME!
     */
    public void setCalUnits(String cal) {
        cal_units = setString(cal, 8);
    }

    /**
     * sets the compressed variable.
     *
     * @param  comp  DOCUMENT ME!
     */
    public void setCompressed(float comp) {
        compressed = comp;
    }

    /**
     * Permits 10 charactar large string for data-type.
     *
     * @param  dtype  DOCUMENT ME!
     */
    public void setDataType(String dtype) {
        data_type = setString(dtype, 10);
    }

    /**
     * Accessor to supply coded datatype.
     *
     * @param  dtype  DOCUMENT ME!
     */
    public void setDataType(short dtype) {

        if ((dtype == DT_UNSIGNED_SHORT) || (dtype == DT_NONE) || (dtype == DT_UNKNOWN) ||
                (dtype == DT_BINARY) || (dtype == DT_UNSIGNED_CHAR) || (dtype == DT_SIGNED_SHORT) ||
                (dtype == DT_SIGNED_INT) || (dtype == DT_FLOAT) || (dtype == DT_COMPLEX) ||
                (dtype == DT_RGB) || (dtype == DT_ALL)) {
            datatype = dtype;
        } else if (dtype == 6) { // Old illegal label for DT_UNSIGNED_SHORT
            datatype = DT_SIGNED_SHORT;
        } else {
            datatype = -1;
        } // a disallowed trap value
    }

    /**
     * Set the database name - limit 18 character max to set the DB_NAME value.
     *
     * @param  dbname  DOCUMENT ME!
     */
    public void setDBname(String dbname) {
        db_name = setString(dbname, 18);
    }

    /**
     * Allows no more than 80 characters to fill in the analyze-image description.
     *
     * @param  description  DOCUMENT ME!
     */
    public void setDescription(String description) {
        descrip = setString(description, 80);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  un0  DOCUMENT ME!
     */
    public void setDim(short un0) {
        dim_un0 = un0;
    }

    /**
     * Allows a maximum of 10 characters to set the experiment date string.
     *
     * @param  date  DOCUMENT ME!
     */
    public void setExperimentDate(String date) {
        exp_date = setString(date, 10);
    }

    /**
     * Allows a maximum of 10 characters to set the experiment time string.
     *
     * @param  time  DOCUMENT ME!
     */
    public void setExperimentTime(String time) {
        exp_time = setString(time, 10);
    }

    /**
     * Supplies the submitted value to the field_sip variable.
     *
     * @param  field  DOCUMENT ME!
     */
    public void setFieldSkip(int field) {
        field_skip = field;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  ext  DOCUMENT ME!
     */
    public void setFileExtents(int ext) {
        extents = ext;
    }

    /**
     * Sets the start locations in the x-axis in fileInfoBase. This is a hack into the ANALYZE standard to compensate
     * for start locations not being known. To be Stored in the Funused1 variable.
     *
     * @param  fun  x start location
     *
     * @see    FileInfoBase#setStartLocations(float, int)
     */
    public void setFunused1(float fun) {
        funused1 = fun;
        super.setOrigin(fun, 0);
    }

    /**
     * Sets the start locations in the y-axis in fileInfoBase. This is a hack into the ANALYZE standard to compensate
     * for start locations not being known. To be Stored in the Funused2 variable.
     *
     * @param  fun  y start location
     *
     * @see    FileInfoBase#setStartLocations(float, int)
     */
    public void setFunused2(float fun) {
        funused2 = fun;
        super.setOrigin(fun, 1);
    }

    /**
     * Sets the start locations in the z-axis in fileInfoBase. This is a hack into the ANALYZE standard to compensate
     * for start locations not being known. To be Stored in the Funused3 variable.
     *
     * @param  fun  z start location
     */
    public void setFunused3(float fun) {
        funused3 = fun;
        super.setOrigin(fun, 2);
    }

    /**
     * Allows a maximum of 10 characters to set the generated string.
     *
     * @param  gen  DOCUMENT ME!
     */
    public void setGenerated(String gen) {
        generated = setString(gen, 10);
    }

    /**
     * Sets glmax. if supplied value is less than glmin, the glmin gets reset to the supplied value as well, so that
     * glmin is still no greater than glmax.
     *
     * @param  gl  DOCUMENT ME!
     */
    public void setGLmax(int gl) {
        glmax = gl;

        if (glmax < glmin) {
            glmin = glmax;
        }
    }

    /**
     * Sets glmin. if supplied value is greater than glmax, the glmax gets reset to the supplied value as well, so that
     * glmax is still no less than glmin.
     *
     * @param  gl  DOCUMENT ME!
     */
    public void setGLmin(int gl) {
        glmin = gl;

        if (glmin > glmax) {
            glmax = glmin;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  hist  DOCUMENT ME!
     */
    public void setHist(String hist) {
        hist_un0 = setString(hist, 3);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  hk  DOCUMENT ME!
     */
    public void setHkey(char hk) {
        hkey_un0 = hk;
    }


    /**
     * Sets omax. If supplied value is less than omin, the omin gets reset to the supplied value as well, so that omin
     * is still no greater than omax.
     *
     * @param  oMax  Value that omax is set to.
     */
    public void setOmax(int oMax) {
        omax = oMax;

        if (omax < omin) {
            omin = omax;
        }
    }

    /**
     * Sets omin. if supplied value is greater than omax, the omax gets reset to the supplied value as well, so that
     * omax is still no less than omin.
     *
     * @param  oMin  Value that omin is set to.
     */
    public void setOmin(int oMin) {
        omin = oMin;

        if (omin > omax) {
            omax = omin;
        }
    }

    /**
     * Sets the image orientation converting from analyze specific orientation to general orientation. Value may be
     * transverse, coronal, or sagittal.
     *
     * <p>see the top of this document for all possible values. supplying a value other than one of these 6 possiblities
     * sets the orientation to an impossible 'flag' value.</p>
     *
     * @see  FileInfoBase#setImageOrientation(int)
     */
    public void setOrientation(byte orientation) {
        int axisX, axisY, axisZ;
        orient = orientation;
        // Remember the y axis is flipped
        if (orientation == FileInfoSPM.TRANSVERSE_UNFLIPPED) {
            super.setImageOrientation(FileInfoBase.AXIAL);
            axisX = FileInfoBase.ORI_R2L_TYPE;
            axisY = FileInfoBase.ORI_A2P_TYPE;
            axisZ = FileInfoBase.ORI_I2S_TYPE;
        } else if (orientation == FileInfoSPM.TRANSVERSE_FLIPPED) {
            super.setImageOrientation(FileInfoBase.AXIAL);
            axisX = FileInfoBase.ORI_R2L_TYPE;
            axisY = FileInfoBase.ORI_P2A_TYPE;
            axisZ = FileInfoBase.ORI_I2S_TYPE;
        } else if (orientation == FileInfoSPM.CORONAL_UNFLIPPED) {
            super.setImageOrientation(FileInfoBase.CORONAL);
            axisX = FileInfoBase.ORI_R2L_TYPE;
            axisY = FileInfoBase.ORI_S2I_TYPE;
            axisZ = FileInfoBase.ORI_P2A_TYPE; 
        } else if (orientation == FileInfoSPM.CORONAL_FLIPPED) {
            super.setImageOrientation(FileInfoBase.CORONAL);
            axisX = FileInfoBase.ORI_R2L_TYPE;
            axisY = FileInfoBase.ORI_I2S_TYPE;
            axisZ = FileInfoBase.ORI_P2A_TYPE; 
        } else if (orientation == FileInfoSPM.SAGITTAL_UNFLIPPED) {
            super.setImageOrientation(FileInfoBase.SAGITTAL); 
            axisX = FileInfoBase.ORI_P2A_TYPE;
            axisY = FileInfoBase.ORI_S2I_TYPE;
            axisZ = FileInfoBase.ORI_R2L_TYPE;
        } else if (orientation == FileInfoSPM.SAGITTAL_FLIPPED) {
            super.setImageOrientation(FileInfoBase.SAGITTAL);
            axisX = FileInfoBase.ORI_P2A_TYPE;
            axisY = FileInfoBase.ORI_I2S_TYPE;
            axisZ = FileInfoBase.ORI_R2L_TYPE;
        } else {
            super.setImageOrientation(FileInfoBase.UNKNOWN_ORIENT);
            axisX = FileInfoBase.ORI_UNKNOWN_TYPE;
            axisY = FileInfoBase.ORI_UNKNOWN_TYPE;
            axisZ = FileInfoBase.ORI_UNKNOWN_TYPE;
        }
        super.setAxisOrientation(axisX, 0);
        super.setAxisOrientation(axisY, 1);
        super.setAxisOrientation(axisZ, 2);

    }

    /**
     * Sets start locations of each axis.
     *
     * @param  stlocat  axis start location array
     *
     * @see    FileInfoBase#getStartLocations()
     */
    public void setOrigin(float[] stlocat) {

        for (int i = 0; i < Math.min(4,stlocat.length); i++) {
            setOrigin(stlocat[i], i);
        }
    }

    /**
     * Sets the start location of the specified axis.
     *
     * @param  fun   origin coord.
     * @param  axis  axis of orientation; x is 0, y is 1, z is 2.
     */
    public void setOrigin(float fun, int axis) {

        if ((axis < 0) || (axis > 3)) {
            Preferences.debug("Error: Axis must be 0, 1, 2, or 3.\n", Preferences.DEBUG_FILEIO);

            return;
        }

        if (axis == 0) { // x-axis
            setFunused1(fun);
        } else if (axis == 1) { // y-axis
            setFunused2(fun);
        } else if (axis == 2) { // z-axis
            setFunused3(fun);
        } else if (axis == 3) {
            super.setOrigin(fun, 3);
        }
    }

    /**
     * Allows a maximum of 10 characters to set the originator string.
     *
     * @param  originator  Originator string.
     */
    public void setOriginator(String originator) {
        this.originator = setString(originator, 10);
    }

    /**
     * Allows a maximum of 10 characters to set the patient ID string.
     *
     * @param  patIDnum  Patient ID string.
     */
    public void setPatientID(String patIDnum) {
        patient_id = setString(patIDnum, 10);
    }

    /**
     * Supply the value of regular character.
     *
     * @param  ch  value of r indicates all images are of the same size which seems to always be the case.
     */
    public void setRegular(char ch) {
        regular = ch;
    }

    /**
     * Allows a maximum of 10 characters to set the scannum string.
     *
     * @param  scanNum  Scan number of the image.
     */
    public void setScanNum(String scanNum) {
        scannum = setString(scanNum, 10);
    }

    /**
     * Supply the value of session error.
     *
     * @param  sessionError  The session error.
     */
    public void setSessionErr(short sessionError) {
        session_error = sessionError;
    }

    /**
     * Sets the size of the header which is always 348.
     *
     * @param  size  The size of the header which is always 348 for Analyze images.
     */
    public void setSizeOfHeader(int size) {
        sizeof_hdr = size;
    }

    /**
     * Sets smax. If supplied value is less than smin, the smin gets reset to the supplied value as well, so that smin
     * is still no greater than smax.
     *
     * @param  sMax  DOCUMENT ME!
     */
    public void setSmax(int sMax) {
        smax = sMax;

        if (smax < smin) {
            smin = smax;
        }
    }

    /**
     * Sets smin. If supplied value is greater than smax, the smax gets reset to the supplied value as well, so that
     * smax is still no less than smin.
     *
     * @param  sMin  The sMin value.
     */
    public void setSmin(int sMin) {
        smin = sMin;

        if (smin > smax) {
            smax = smin;
        }
    }

    /**
     * Supplies the submitted value to the start_field variable.
     *
     * @param  startField  The start field value.
     */
    public void setStartField(int startField) {
        start_field = startField;
    }


    /**
     * Sets unused 1.
     *
     * @param  unused  The unused value.
     */
    public void setUnused1(int unused) {
        unused1 = (short) unused;
        super.setAxisOrientation(unused, 0);
    }

    /**
     * Sets unused 2.
     *
     * @param  unused  The unused value.
     */
    public void setUnused2(int unused) {
        unused2 = (short) unused;
        super.setAxisOrientation(unused, 1);
    }

    /**
     * Sets unused 3.
     *
     * @param  unused  The unused value.
     */
    public void setUnused3(int unused) {
        unused3 = (short) unused;
        super.setAxisOrientation(unused, 2);
    }

    /**
     * Supply the value to verified.
     *
     * @param  verified  The verified value
     */
    public void setVerified(float verified) {
        this.verified = verified;
    }

    // public void setHistUn0            /* thinks this is unused !! */
    /**
     * Sets the views variable.
     *
     * @param  view  The view value.
     */
    public void setViews(int view) {
        views = view;
    }

    /**
     * Supplies a value to the vols_added valiable.
     *
     * @param  vols  DOCUMENT ME!
     */
    public void setVolsAdded(int vols) {
        vols_added = vols;
    }


    /**
     * Allows no more than 4 characters for the string describing the vox units.
     *
     * @param  voxUnits  DOCUMENT ME!
     */
    public void setVoxUnits(String voxUnits) {
        vox_units = setString(voxUnits, 4);
    }

    /**
     * .
     *
     * <table>
     *   <tr>
     *     <td>ce[0] = table</td>
     *     <td>0 = primary, 1 = secondary, etC</td>
     *   </tr>
     *   <tr>
     *     <td>ce[1] = line of table</td>
     *     <td></td>
     *   </tr>
     *   <tr>
     *     <td>ce[2] = string name</td>
     *     <td>eg, "Type"</td>
     *   </tr>
     *   <tr>
     *     <td>ce[3] = Vector codeValue</td>
     *     <td>eg, "B"</td>
     *   </tr>
     *   <tr>
     *     <td>ce[4] = string value</td>
     *     <td>eg, "Big"</td>
     *   </tr>
     * </table>
     *
     * <p>"ce" comes from ChangeEvent upon which this is based. care to make our own ChangeEvent to store and handle
     * this?</p>
     *
     * @param  ce  DOCUMENT ME!
     */
    @SuppressWarnings("unchecked")
    public void stateChanged(Vector ce) {
        String tname = (String) ce.elementAt(2); // [t]able [name]
        Vector tcvalue = (Vector) ce.elementAt(3); // [t]able [c]ode [value]
        String tvalue = (String) ce.elementAt(4); // [t]able [value]

        if (tname.equalsIgnoreCase("Description")) {
            setDescription(tvalue);
        } else if (tname.equalsIgnoreCase("data type")) {
            setDataType(tvalue);
        } else if (tname.equalsIgnoreCase("db name")) {
            setDBname(tvalue);
        } else if (tname.equalsIgnoreCase("voxel units")) {
            setVoxUnits(tvalue);
        } else if (tname.equalsIgnoreCase("cal units")) {
            setCalUnits(tvalue);
        } else if (tname.equalsIgnoreCase("voxel offset")) {
            setOffset((int) (Float.parseFloat((String) tcvalue.elementAt(0))));
        } else if (tname.equalsIgnoreCase("cal_min")) {
            setCalMin(Float.parseFloat((String) tcvalue.elementAt(0)));
        } else if (tname.equalsIgnoreCase("cal_max")) {
            setCalMax(Float.parseFloat((String) tcvalue.elementAt(0)));
        } else if (tname.equalsIgnoreCase("glmax")) {
            setGLmax(Integer.parseInt((String) tcvalue.elementAt(0)));
        } else if (tname.equalsIgnoreCase("glmin")) {
            setGLmin(Integer.parseInt((String) tcvalue.elementAt(0)));
        } else if (tname.equalsIgnoreCase("Orientation")) {
            setOrientation(((Byte) tcvalue.elementAt(0)).byteValue());
            // setImageOrientation(((Byte) tcvalue.elementAt(0)).byteValue());
        } else if (tname.equalsIgnoreCase("Originator")) {
            setOriginator(tvalue);
        } else if (tname.equalsIgnoreCase("generated")) {
            setGenerated(tvalue);
        } else if (tname.equalsIgnoreCase("scan number")) {
            setScanNum(tvalue);
        } else if (tname.equalsIgnoreCase("patient id")) {
            setPatientID(tvalue);
        } else if (tname.equalsIgnoreCase("experiment date")) {
            setExperimentDate(tvalue);
        } else if (tname.equalsIgnoreCase("experiment time")) {
            setExperimentTime(tvalue);
        } else if (tname.equalsIgnoreCase("views")) {
            setViews(Integer.parseInt((String) tcvalue.elementAt(0)));
        } else if (tname.equalsIgnoreCase("volume added")) {
            setVolsAdded(Integer.parseInt((String) tcvalue.elementAt(0)));
        } else if (tname.equalsIgnoreCase("start field")) {
            setStartField(Integer.parseInt((String) tcvalue.elementAt(0)));
        } else if (tname.equalsIgnoreCase("field skip")) {
            setFieldSkip(Integer.parseInt((String) tcvalue.elementAt(0)));
        } else if (tname.equalsIgnoreCase("omax")) {
            setOmax(Integer.parseInt((String) tcvalue.elementAt(0)));
        } else if (tname.equalsIgnoreCase("omin")) {
            setOmin(Integer.parseInt((String) tcvalue.elementAt(0)));
        } else if (tname.equalsIgnoreCase("smax")) {
            setSmax(Integer.parseInt((String) tcvalue.elementAt(0)));
        } else if (tname.equalsIgnoreCase("smin")) {
            setSmin(Integer.parseInt((String) tcvalue.elementAt(0)));
        } else if (tname.startsWith("Axis: x-orientation")) {
            setUnused1(((Integer) tcvalue.elementAt(0)).intValue());
        } else if (tname.startsWith("Axis: y-orientation")) {
            setUnused2(((Integer) tcvalue.elementAt(0)).intValue());
        } else if (tname.startsWith("Axis: z-orientation")) {
            setUnused3(((Integer) tcvalue.elementAt(0)).intValue());
        } else if (tname.startsWith("X-origin: ")) {
            setOrigin(Float.parseFloat((String) tcvalue.elementAt(0)), 0);
        } else if (tname.startsWith("Y-origin: ")) {
            setOrigin(Float.parseFloat((String) tcvalue.elementAt(0)), 1);
        } else if (tname.startsWith("Z-origin: ")) {
            setOrigin(Float.parseFloat((String) tcvalue.elementAt(0)), 2);
        } else if (tname.equalsIgnoreCase("Orientation")) {
            setImageOrientation(((Integer) tcvalue.elementAt(0)).intValue());
            // setOrientation(((Byte)tcvalue.elementAt(0)).byteValue());

        } else {
            Preferences.debug("tname: " + tname + ", not found.", Preferences.DEBUG_FILEIO);
        }
    }

    /**
     * Propogates the current file info to another FileInfoAnalyze except for the 3 start locations kept in funused1,
     * funused2, funused3.
     *
     * <p>It does not copy over the datatypeCode. (though, aside from, "it isn't in the about table", I can't think of a
     * reason why it shouldn't. but it doesn't.) Also, copied over is bitPix, aux_file.</p>
     *
     * <p>Some variables, such as the unused1, unused2, unused3, are really taken from unused, rather than from the
     * start locations as we <b>are</b> re-using the funused variables. This means that if the unused variables are not
     * right to begin with, this certainly won't correct them!</p>
     *
     * @param  fInfo  File info. object used to update values in this object.
     */
    public void updateFileInfos(FileInfoAnalyze fInfo) {

        if (this == fInfo) {
            return;
        }

        // fInfo.setAuxFile            (this.getAuxFile());// not editable by the table!!
        // fInfo.setBitPix             (this.getBitPix()); // not editable by the table!!
        fInfo.setCalMin(this.getCalMin());
        fInfo.setCalMax(this.getCalMax());
        fInfo.setCalUnits(this.getCalUnits());
        fInfo.setCompressed(this.getCompressed());

        // fInfo.setDataTypeCode       (this.getDataTypeCode());//not edited by the table!!
        fInfo.setDataType(this.getDataTypeName());
        fInfo.setDBname(this.getDBname());
        fInfo.setDescription(this.getDescription());
        fInfo.setDim(this.getDim());
        fInfo.setExperimentDate(this.getExperimentDate());
        fInfo.setExperimentTime(this.getExperimentTime());
        fInfo.setFieldSkip(this.getFieldSkip());
        fInfo.setGenerated(this.getGenerated());
        fInfo.setGLmin(this.getGLmin());
        fInfo.setGLmax(this.getGLmax());
        fInfo.setHist(this.getHist());
        fInfo.setHkey(this.getHkey());
        fInfo.setOmin(this.getOmin());
        fInfo.setOmax(this.getOmax());
        fInfo.setOrientation(this.getOrientation());
        fInfo.setOriginator(this.getOriginator());
        fInfo.setPatientID(this.getPatientID());
        fInfo.setRegular(this.getRegular());
        fInfo.setScanNum(this.getScanNum());
        fInfo.setSessionErr(this.getSessionErr());
        fInfo.setSmax(this.getSmax());
        fInfo.setSmin(this.getSmin());
        fInfo.setStartField(this.getStartField());
        fInfo.setUnused1(this.getUnused1()); // acutally gets unused --
        fInfo.setUnused2(this.getUnused2()); // if the unused are not right,
        fInfo.setUnused3(this.getUnused3()); // this won't correct them!
        fInfo.setVerified(this.getVerified());
        fInfo.setViews(this.getViews());
        fInfo.setVolsAdded(this.getVolsAdded());
        fInfo.setOffset(this.getOffset());
        fInfo.setVoxUnits(this.getVoxUnits());
    }


    /**
     * Verifies string is not larger than length; strings larger than length, are clipped before being returned.
     *
     * @param   str     The input string.
     * @param   length  The maximum length of the string.
     *
     * @return  String new substring
     */
    protected String setString(String str, int length) {

        if (str.length() <= length) {
            return str;
        } else {
            return str.substring(0, length);
        }
    }

    /**
     * Selection of text for the table and the editpanel.
     *
     * @param   orientation  value indicating the orientation.
     *
     * @return  String the orientation string
     */
    private String selectOrientationText(int orientation) {
        String orientat;

        switch (orientation) {

            case TRANSVERSE_UNFLIPPED:
                orientat = "transverse unflipped";
                break;

            case CORONAL_UNFLIPPED:
                orientat = "coronal unflipped";
                break;

            case SAGITTAL_UNFLIPPED:
                orientat = "sagittal unflipped";
                break;

            case TRANSVERSE_FLIPPED:
                orientat = "transverse flipped";
                break;

            case CORONAL_FLIPPED:
                orientat = "coronal flipped";
                break;

            case SAGITTAL_FLIPPED:
                orientat = "sagittal flipped";
                break;

            default:
                orientat = "undefined";
                break;
        }

        return orientat;
    }
}
