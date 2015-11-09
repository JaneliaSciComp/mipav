package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;


/**
 * @see      FileSiemensText
 */
public class FileInfoSiemensText extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7319414471012796383L;
    
    /** SiemensText file format define use to indicate undefined image data type. */
    public static final short DT_UNKNOWN = 0;

    /** SiemensText file format define use to indicate Byte (8-bits) data type */
    public static final short DT_BYTE = 1;

    /** SiemensText file format define use to indicate 2-byte integer - Intel style. */
    public static final short DT_2INTEGERI = 2;
    
    /** SiemensText file format define use to indicate 4-byte integer - Intel style. */
    public static final short DT_4INTEGERI = 3;

    /** SiemensText file format define use to indicate 4-byte float - Intel style. */
    public static final short DT_4FLOATI = 4;
    
    /** SiemensText file format define use to indicate 4-byte float - Sun style. */
    public static final short DT_4FLOATS = 5;

    /** SiemensText file format define use to indicate 2-byte integer - Sun style. */
    public static final short DT_2INTEGERS = 6;
    
    /** SiemensText file format define use to indicate 4-byte integer - Sun style. */
    public static final short DT_4INTEGERS = 7;
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private String aux_file = null;

    /** Bits per pixel. Valid values include: 1, 8, 16, 32, 64, 24(rgb). */
    private short bitpix = -1;

    /** Range of calibration values. */
    private float cal_max = 0;

    /** Values of 0.0 for both fields imply that no calibration min and max values are used ! */
    private float cal_min = 0;

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

    /** specifies the spatial units of measure for a voxel. */
    private String vox_units = null;

	private String name;

	private String dir;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * File info storage constructor.
     *
     * @param  name       file name
     * @param  directory  directory
     * @param  format     file format
     */
    public FileInfoSiemensText(String name, String directory, int format) {
        super(name, directory, format);
        this.name = name;
        this.dir = directory;
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
        JDialogText dialog = (JDialogText) dlog;
        displayPrimaryInfo(dialog, matrix);
        int index = name.lastIndexOf('.');
        RandomAccessFile raFile;
        String fileHeaderName = name.substring(0, index) + ".IMG.HDR";
        File fileHeader = new File(dir + fileHeaderName);
        try {
			raFile = new RandomAccessFile(fileHeader, "r");
		String text = raFile.readLine();
		while (text != null){
			dialog.append(text + "\n");
			text = raFile.readLine();
		}
		
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
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
     * Accessor to the current SiemensText-image description.
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
     * SiemensText _extents_ value of 16,384.
     *
     * @return  The extents value which always seems to be 16,384.
     */
    public int getFileExtents() {
        return extents;
    }

    /**
     * MIPAV hack to the SiemensText standard. Retrieves start locaiton from x-axis. To be stored as Funused1.
     *
     * @return  float funused1
     */
    public float getFunused1() {
        return funused1;
    }

    /**
     * MIPAV hack to the SiemensText standard. Retrieves start locaiton from y-axis. To be stored as Funused2.
     *
     * @return  float funused2
     */
    public float getFunused2() {
        return funused2;
    }

    /**
     * MIPAV hack to the SiemensText standard. Retrieves start location from z-axis. To be stored as Funused3.
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
     * Accessor to get the SiemensText's hist_un0 string.
     *
     * @return  Returns the hist_un0 string.
     */
    public String getHist() {
        return hist_un0;
    }

    /**
     * Accessor to get the SiemensText's hkey_un0 string.
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
     * Provides the verified value.
     *
     * @return  The SiemensText verified parameter.
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
     * sets the compressed variable.
     *
     * @param  comp  DOCUMENT ME!
     */
    public void setCompressed(float comp) {
        compressed = comp;
    }

    /**
     * Accessor to supply coded datatype.
     *
     * @param  dtype  DOCUMENT ME!
     */
    public void setDataType(short dtype) {


            datatype = dtype;

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
     * Allows no more than 80 characters to fill in the SiemensText-image description.
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
    public void setExperimentDateandTime(String date) {
        exp_date = date.substring(0,date.indexOf(':')-3) + date.substring(date.lastIndexOf(' '));
        exp_time = date.substring(date.indexOf(':')-3,date.lastIndexOf(' '));
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
     * Sets the start locations in the x-axis in fileInfoBase. This is a hack into the SiemensText standard to compensate
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
     * Sets the start locations in the y-axis in fileInfoBase. This is a hack into the SiemensText standard to compensate
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
     * Sets the start locations in the z-axis in fileInfoBase. This is a hack into the SiemensText standard to compensate
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
     * Sets the image orientation converting from SiemensText specific orientation to general orientation. Value may be
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

        if (stlocat.length > 4) {
            Preferences.debug("Start locations array must be of length less than 4.\n", Preferences.DEBUG_FILEIO);

            return;
        }

        for (int i = 0; i < stlocat.length; i++) {
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
     * @param  size  The size of the header which is always 348 for SiemensText images.
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

    /**
     * Propogates the current file info to another FileInfoSiemensText except for the 3 start locations kept in funused1,
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
    public void updateFileInfos(FileInfoSiemensText fInfo) {

        if (this == fInfo) {
            return;
        }

        // fInfo.setAuxFile            (this.getAuxFile());// not editable by the table!!
        // fInfo.setBitPix             (this.getBitPix()); // not editable by the table!!
        fInfo.setCalMin(this.getCalMin());
        fInfo.setCalMax(this.getCalMax());
        fInfo.setCompressed(this.getCompressed());

        // fInfo.setDataTypeCode       (this.getDataTypeCode());//not edited by the table!!
        fInfo.setDBname(this.getDBname());
        fInfo.setDescription(this.getDescription());
        fInfo.setDim(this.getDim());
        fInfo.setExperimentDateandTime(this.getExperimentDate());
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
}