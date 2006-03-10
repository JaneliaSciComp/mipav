package gov.nih.mipav.model.file;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.util.Vector;

/**
*   This structures contains the information that describes how
*   an SPM image is stored on disk. We have
*   extended this format to store image orientation and start locations. We have used
*   unused variables to store these data. Almost all programs ignore these
*   variables and should not have any problems reading images saved with MIPAV.
*
*   Differences between MIPAV ANALYZE and SPM:
*   In MIPAV ANALYZE we have
*   location 60 cal_units 4 unsigned characters
*   location 64 - X-axis orientation     @see FileInfoBase for static variables that are set.
*   location 66 - Y-axis orientation     // All three unused variables are of short type.
*   location 68 - Z-axis orientation
*   In SPM we have
*   location 60 cal_units 8 unsigned characters
*   location 68 unused1
*
*   In MIPAV ANALYZE we have
*   location 112 Funused1 - X-axis starting location (origin from upper left-hand corner)
*   location 116 Funused2 - Y-axis starting location (origin from upper left-hand corner)
*   location 120 Funused3 - Z-axis starting location (origin is the first image slice)
*   In SPM we have
*   location 112 Funused1 scale a floating point scale factor applied during memory
*                               mapping
*   location 116 Funused2
*   location 120 Funused3
*
*   In MIPAV ANALYZE we have
*   location 253 originator with 10 characters
*   In SPM we have
*   location 253 origin with 5 shorts.  If the first 3 shorts of a 3D image are set
*   to 0, 0, 0, the origin is assumed to be at the center of the volume, since in SPM
*   the corner voxel is at 1, 1, 1.  The position(x,y,z) in mm. is determined by the
*   distance(x,y,z) from the origin multiplied by the vox_units(x,y,z).
*
*   In SPM the signed byte datatype was added with DT_BYTE = 130.
*   MIPAV ANALYZE uses UNSIGNED_SHORT = 6 while SPM uses DT_UNSIGNED_SHORT = 132.
*   The SPM standard also provides for an unsigned int = 136, but
*   MIPAV does not used the unsigned int data type.  Note that in SPM while
*   DATA = datatype * 256 for swapped bytes, only datatype and not DATA is
*   written to the file, so in this program we need never consider the multiplicative
*   factors of 256.
*
*   RGB SPM images are store in chunky format rgb, rgb, rgb ......
*
*		@version    1.0 July, 2002
*		@author     Matthew J. McAuliffe, Ph.D.
*       @see        FileSPM
*
*/

public class FileInfoSPM extends FileInfoBase
{

    private     int     sizeof_hdr           = -1;       /* 348 */
    private     String  data_type            = null;     /* 10 chars */
    private     String  db_name              = null;     /* name of file 18 chars */
    private     int     extents              = 0;        /* should be 16384 */
    private     short   session_error        = -1;
    private     char    regular              = 'r';      /* I think it means data
                                                           has uniform spacing (resolutions)*/
    private     char    hkey_un0;

   //public     short   dim[]  = new short[8]; // image dimension data
                              //  stored in FileInfoBase
                              //  dim[0] = number of dimensions; usally 4
                              //  dim[1] = image width
                              //  dim[2] = image height
                              //  dim[3] = image depth (# of slices)
                              //  dim[4] = volumes in image  --- must be one for 3D image

    private     String  vox_units            = null; // specifies the spatial units of measure for a voxel
    private     String  cal_units            = null; // specifies the name of the calibration unit

    private     short   datatype             = -1;

    private final  static int  DT_NONE              =  0;
    private final  static int  DT_UNKNOWN           =  0;
    private final  static int  DT_BINARY            =  1;
    private final  static int  DT_BYTE              =130;
    private final  static int  DT_UNSIGNED_CHAR     =  2;
    private final  static int  DT_SIGNED_SHORT      =  4;
    private final  static int  DT_UNSIGNED_SHORT    =132;
    private final  static int  DT_SIGNED_INT        =  8;
    private final  static int  DT_UNSIGNED_INT      =136;
    private final  static int  DT_FLOAT             = 16;
    private final  static int  DT_COMPLEX           = 32;
    private final  static int  DT_DOUBLE            = 64;
    private final  static int  DT_RGB               =128;
    private final  static int  DT_ALL               =255;

    private     short   bitpix               = -1;   // bits per pixel : 1,8,16,32,64, 24(rgb)
    private     short   dim_un0              = -1;   // unused

    //public     float   pixdim               = new float[8]; // image resolutions info mm or ms
                              //  stored in FileInfoBase
                              //  pixdim[0] = number of dimensions
                              //  pixdim[1] = voxel width
                              //  pixdim[2] = voxel height
                              //  pixdim[3] = voxel thickness
                              //  pixdim[4] = time
    private     float   vox_offset           = -1;  /* byte offset in the ".img" file at which voxels start
                                                      This value can be negative to specify that the absolute
                                                      value is applied for every image in the file */

    // a floating point scale factor used during memory mapping
    private     float   scale             = 1.0f;

    private     float   cal_max              =  0;   // range of calibration values
    private     float   cal_min              =  0;   // values of 0.0 for both fields imply that no
                                                    //  calibration min and max values are used !
    private     float   compressed           = -1;
    private     float   verified             = -1;
    private     int     glmax                = -99999; // maximum pixel values for the entire database
    private     int     glmin                = -99999; // minimum pixel values for the entire database

    private     String  descrip              = null;
    private     String  aux_file             = null;
    /*
      The data_history substructure is not required, but the orient element is used to indicate
      individual slice orientation and determines whetther the SPM Movie program will attempt to
      flip the images before displaying a movie sequence.
                0 - transverse unflipped
                1 - coronal unflipped
                2 - sagitttal unflipped
                3 - transverse flipped
                4 - coronal flipped
                5 - sagitttal flipped
    */
    public static final int  UNKNOWN_ORIENT       = -1;
    public static final int  TRANSVERSE_UNFLIPPED =  0;
    public static final int  CORONAL_UNFLIPPED    =  1;
    public static final int  SAGITTAL_UNFLIPPED   =  2;
    public static final int  TRANSVERSE_FLIPPED   =  3;
    public static final int  CORONAL_FLIPPED      =  4;
    public static final int  SAGITTAL_FLIPPED     =  5;


    private     byte    orient               = -1;
    private     short   origin[]             = new short[]{0,0,0,0,0};
    private     String  generated            = null;
    private     String  scannum              = null;
    private     String  patient_id           = null;
    private     String  exp_date             = null;
    private     String  exp_time             = null;
    private     String  hist_un0             = null;
    private     int     views                = -1;
    private     int     vols_added           = -1;
    private     int     start_field          = -1;
    private     int     field_skip           = -1;
    private     int     omax = -1, omin      = -1;
    private     int     smax = -1, smin      = -1;

    /**
    *  file info storage constructor
    *  @param name        file name
    *  @param directory   directory
    *  @param format      file format
    */
    public FileInfoSPM(String name, String directory, int format) {
        super(name, directory, format);
    }

    /**
    *  displays the file information
    *  @param dlog    dialog box that is written to
    *  @param matrix  transformation matrix
    */
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix){
        JDialogFileInfo dialog = (JDialogFileInfo) dlog;
        int[] extents;
        int i;
        int[] editorChoice = new int[1];
        editorChoice[0] = JDialogEditor.STRING;

        dialog.displayAboutInfo(this);  // setup layout in the dialog

        extents = super.getExtents();
        for(i = 0; i < extents.length; i++){
            dialog.appendPrimaryData("Dimension " + i, Integer.toString(extents[i]));
        }

        dialog.appendPrimaryData("Type", ModelStorageBase.getBufferTypeStr(getDataType()));
        if (ModelImage.isColorImage(getDataType())) {
            dialog.appendPrimaryData("Min red",   Double.toString(getMinR()));
            dialog.appendPrimaryData("Max red",   Double.toString(getMaxR()));
            dialog.appendPrimaryData("Min green", Double.toString(getMinG()));
            dialog.appendPrimaryData("Max green", Double.toString(getMaxG()));
            dialog.appendPrimaryData("Min blue",  Double.toString(getMinB()));
            dialog.appendPrimaryData("Max blue",  Double.toString(getMaxB()));

        }
        else {
            dialog.appendPrimaryData("Min", Double.toString(getMin()));
            dialog.appendPrimaryData("Max", Double.toString(getMax()));
        }
        dialog.appendPrimaryData("Modality", FileInfoBase.getModalityStr(getModality()));

        dialog.appendPrimaryData("Orientation", getImageOrientationStr(getImageOrientation()));

        float[] resolutions;// = new float[5];
        resolutions = getResolutions();
        int[] measure;// = new int[5];
        measure = getUnitsOfMeasure();
        for (i=0; i < extents.length; i++) {
          if (resolutions[i] > 0.0) {
            String pixelRes = "Pixel resolution " + i;
            dialog.appendPrimaryData(pixelRes, Float.toString(resolutions[i]) +" "+ getUnitsOfMeasureStr(measure[i]));
          }  // end of if (resolutions[i] > 0.0)
        }  // for (i=0; i < 5; i++)

        if ( getEndianess() == FileBase.LITTLE_ENDIAN) {
            dialog.appendPrimaryData("Endianess", "Little Endian");
        }
        else {
            dialog.appendPrimaryData("Endianess", "Big Endian");
        }

        if (matrix != null) {
            // when using displayAboutInfo(dialog) this doesn't appear
            // calling prg might use an editing panel to adjust this matrix
            dialog.appendPrimaryData("Matrix", matrix.matrixToString(10,4));
        }



        try {   // DB name (usually filename)
            editorChoice[0] = JDialogEditor.ANALYZE_DBNAME;
            dialog.appendSecondaryData("DB Name", db_name.trim(), editorChoice);
        }
        catch (NullPointerException npe) {
            editorChoice[0] = JDialogEditor.ANALYZE_DBNAME;
            dialog.appendSecondaryData("DB Name", "", editorChoice);
        }
        // description
        try {
            editorChoice[0] = JDialogEditor.ANALYZE_DESCRIPTION;
            dialog.appendSecondaryData("Description", descrip.trim(), editorChoice);
        }
        catch (NullPointerException npe) {
            editorChoice[0] = JDialogEditor.ANALYZE_DESCRIPTION;
            dialog.appendSecondaryData("Description", "", editorChoice);
        }

        for(i = 0; i < extents.length; i++){
            dialog.appendSecondaryData("origin " + i, Integer.toString(origin[i]));
        }

        try {   // data type
            editorChoice[0] = JDialogEditor.ANALYZE_ORIGINATOR;
            dialog.appendSecondaryData("Data Type", data_type.trim(), editorChoice);
        }
        catch (NullPointerException npe) {
            editorChoice[0] = JDialogEditor.ANALYZE_ORIGINATOR;
            dialog.appendSecondaryData("Data Type", "", editorChoice);
        }

        if (vox_offset != -1) {     // vox offset
            //dialog.append("voxel offset: " + vox_offset + "\n");
            editorChoice[0] = JDialogEditor.FLOAT_STRING;
            dialog.appendSecondaryData("Voxel Offset", Float.toString(vox_offset), editorChoice);
        }

        try {   // vox units
            editorChoice[0] = JDialogEditor.ANALYZE_VOX;
            dialog.appendSecondaryData("vox units", vox_units.trim(), editorChoice);
        }
        catch (NullPointerException npe) {
            editorChoice[0] = JDialogEditor.ANALYZE_VOX;
            dialog.appendSecondaryData("vox units", "", editorChoice);
        }

        try {   // cal units
            editorChoice[0] = JDialogEditor.ANALYZE_CAL;
            dialog.appendSecondaryData("cal units", cal_units.trim(), editorChoice);
        }
        catch (NullPointerException npe) {
            editorChoice[0] = JDialogEditor.ANALYZE_CAL;
            dialog.appendSecondaryData("cal units", "", editorChoice);
        }

        dialog.appendSecondaryData("Axis: x-orientation ", getAxisOrientationStr(getAxisOrientation()[0]));
        dialog.appendSecondaryData("Axis: y-orientation ", getAxisOrientationStr(getAxisOrientation()[1]));
        dialog.appendSecondaryData("Axis: z-orientation ", getAxisOrientationStr(getAxisOrientation()[2]));


        editorChoice[0] = JDialogEditor.FLOAT_STRING;
        dialog.appendSecondaryData("Scale factor: ", Float.toString(getScale()), editorChoice);

        if (cal_min != -1) {
            editorChoice[0] = JDialogEditor.FLOAT_STRING;
            dialog.appendSecondaryData("cal_min", Float.toString(cal_min), editorChoice);
        }

        if (cal_max != -1) {
            editorChoice[0] = JDialogEditor.FLOAT_STRING;
            dialog.appendSecondaryData("cal_max", Float.toString(cal_max), editorChoice);
        }

        if (glmin != -99999 && glmax != -99999)  {
            //dialog.append("glmin: " + glmin + "  glmax: " + glmax + "\n");
            editorChoice[0] = JDialogEditor.INT_STRING;
            dialog.appendSecondaryData("glmin", Integer.toString(glmin), editorChoice);
            dialog.appendSecondaryData("glmax", Integer.toString(glmax), editorChoice);
        }

        if (bitpix != -1) {
            dialog.appendSecondaryData("Bits per Pixel", Integer.toString(bitpix));
        }

        if (aux_file != null)   {
            if (aux_file.trim().length() > 0)
                dialog.appendSecondaryData("aux", aux_file.trim());
        }

        editorChoice[0] = JDialogEditor.ANALYZE_ORIENTATION;
        dialog.appendSecondaryData("Orientation", selectOrientationText(getOrientation()), editorChoice);

        try {   // generated
            editorChoice[0] = JDialogEditor.ANALYZE_ORIGINATOR;
            dialog.appendSecondaryData("Generated", generated.trim(), editorChoice);
            //dialog.append("Generated: " + generated.trim() + "\n");
        }
        catch (NullPointerException npe) {
            editorChoice[0] = JDialogEditor.ANALYZE_ORIGINATOR;
            dialog.appendSecondaryData("Generated", "", editorChoice);
        }

        try {   // scan number
            editorChoice[0] = JDialogEditor.ANALYZE_ORIGINATOR;
            dialog.appendSecondaryData("Scan Number", scannum.trim(), editorChoice);
            //dialog.append("Generated: " + generated.trim() + "\n");
        }
        catch (NullPointerException npe) {
            editorChoice[0] = JDialogEditor.ANALYZE_ORIGINATOR;
            dialog.appendSecondaryData("Scan Number", "", editorChoice);
        }

        try {   // patient ID
            editorChoice[0] = JDialogEditor.ANALYZE_ORIGINATOR;
            dialog.appendSecondaryData("Patient ID", patient_id.trim(), editorChoice);
            //dialog.append("Generated: " + generated.trim() + "\n");
        }
        catch (NullPointerException npe) {
            editorChoice[0] = JDialogEditor.ANALYZE_ORIGINATOR;
            dialog.appendSecondaryData("Patient ID", "", editorChoice);
        }

        try {   // experiment date
            editorChoice[0] = JDialogEditor.ANALYZE_ORIGINATOR;
            dialog.appendSecondaryData("Experiment Date", exp_date.trim(), editorChoice);
            //dialog.append("Generated: " + generated.trim() + "\n");
        }
        catch (NullPointerException npe) {
            editorChoice[0] = JDialogEditor.ANALYZE_ORIGINATOR;
            dialog.appendSecondaryData("Experiment Date", "", editorChoice);
        }

        try {   // experiment time
            editorChoice[0] = JDialogEditor.ANALYZE_ORIGINATOR;
            dialog.appendSecondaryData("Experiment Time", exp_time.trim(), editorChoice);
        }
        catch (NullPointerException npe) {
            editorChoice[0] = JDialogEditor.ANALYZE_ORIGINATOR;
            dialog.appendSecondaryData("Experiment Time", "", editorChoice);
        }

        if (hist_un0 != null)  {
            if (hist_un0.trim().length() > 0)
                dialog.appendSecondaryData("History:", hist_un0.trim());
        }

        if (views != -1)  {
            editorChoice[0] = JDialogEditor.INT_STRING;
            dialog.appendSecondaryData("Views", Integer.toString(views), editorChoice);
        }
        if (vols_added != -1)  {
            editorChoice[0] = JDialogEditor.INT_STRING;
            dialog.appendSecondaryData("Volume Added", Integer.toString(vols_added), editorChoice);
        }
        if (start_field != -1)  {
            editorChoice[0] = JDialogEditor.INT_STRING;
            dialog.appendSecondaryData("Start Field", Integer.toString(start_field), editorChoice);
            //dialog.append("Start field: " + start_field + "\n");
        }
        if (field_skip != -1)  {
            editorChoice[0] = JDialogEditor.INT_STRING;
            dialog.appendSecondaryData("Field Skip", Integer.toString(field_skip), editorChoice);
            //dialog.append("Field skip: " + field_skip + "\n");
        }
        if (omax != -1)  {
            editorChoice[0] = JDialogEditor.INT_STRING;
            dialog.appendSecondaryData("omin", Integer.toString(omin), editorChoice);
            dialog.appendSecondaryData("omax", Integer.toString(omax), editorChoice);
            //dialog.append("omax: " + omax + "  omin: " + omin + "\n");
        }
        if (smax != -1)  {
            editorChoice[0] = JDialogEditor.INT_STRING;
            dialog.appendSecondaryData("smin", Integer.toString(smin), editorChoice);
            dialog.appendSecondaryData("smax", Integer.toString(smax), editorChoice);
            //dialog.append("smax: " + smax + "  smin: " + smin + "\n");
        }

    }


    /** verifies string is not larger than len length;
    *   strings larger than len, are clipped before being returned
    *   @see String#substring(int, int)
    *   @return String new substring
    */
    protected String setString(String str, int len) {
        if (str.length() < len) {return str;}
        else {return str.substring(0, len);}
    }

    /** permits 10 charactar large string for data-type. */
    public void setDataType(String dtype) {
        data_type = setString(dtype, 10);
    }
    /** Accessor to the 10 character string of data-type.
    *   @return String returns data_type
    */
    public String getDataTypeName() {return data_type;}

    /** accessor to supply coded datatype. */
    public void setDataType(short dtype) {
        if ((dtype == (short)this.DT_UNSIGNED_SHORT)   || // mipav specific MODE
            (dtype == (short)this.DT_NONE)          ||
            (dtype == (short)this.DT_UNKNOWN)       ||
            (dtype == (short)this.DT_BINARY)        ||
            (dtype == (short)this.DT_BYTE)          ||
            (dtype == (short)this.DT_UNSIGNED_CHAR) ||
            (dtype == (short)this.DT_SIGNED_SHORT)  ||
            (dtype == (short)this.DT_SIGNED_INT)    ||
            (dtype == (short)this.DT_FLOAT)         ||
            (dtype == (short)this.DT_COMPLEX)       ||
            (dtype == (short)this.DT_RGB)           ||
            (dtype == (short)this.DT_ALL)) {
                datatype = dtype;
        }
        else {  datatype = -1;} // a disallowed trap value
    }

    /** accessor to coded datatype value.
    *   @return short datatype
    */
    public short getDataTypeCode() {return datatype;}

    /** 18 character max to set the DB_NAME value */
    public void setDBname(String dbname) {
        db_name = setString(dbname, 18);
    }

    /** accessor to DB_name
    *   @return String database name
    */
    public String getDBname() {return db_name;}

    /** allows no more than 80 characters to fill in the SPM-image description. */
    public void setDescription(String description) {
        descrip = setString(description, 80);
    }

    /** accessor to the current SPM-image description.
    *   @return String description
    */
    public String getDescription() {return descrip;}

    /** supplies auxiliary-file string; permits no more than 24 characters. */
    public void setAuxFile(String aux) {
        aux_file = setString(aux, 24);
    }

    /** accessor to the aux_file string.
    *   @return String aux_file
    */
    public String getAuxFile() {return aux_file;}

    /** sets bitpix; any value other than 1, 8, 16, 32, 64, or 24
    *   gets set to the dissalowed trap value, -1.
    */
    public void setBitPix(short bp) {
        if ((bp == 1) ||
            (bp == 8) ||
            (bp == 16)||
            (bp == 32)||
            (bp == 64)||
            (bp == 24)) {
                bitpix  = bp;
        }
        else {  bitpix = -1;}   // a disallowed trap value
    }

    /** accessor to the bitpix value.
    *   @return short the bitpix value.
    */
    public short getBitPix() {return bitpix;}

    /** sets vox offset value. */
    public void setVoxOffset(float vox) {
        vox_offset = vox;
    }

    /** accessor to the vox offset value
    *   @return float vox_offset
    */
    public float getVoxOffset() {return vox_offset;}

    /** sets cal-max.  if supplied value is less than
    *   cal-min, the cal-min gets reset to the supplied
    *   value as well, so that cal-min is still no
    *   greater than cal-max.
    */
    public void setCalMax(float cal) {
        cal_max = cal;
        if (cal_max < cal_min) {
            cal_min = cal_max;
        }
    }

    /** accessor to cal-max
    *   @return float cal_max
    */
    public float getCalMax() {return cal_max;}

    /** sets cal-min.  if supplied value is greater than
    *   cal-max, the cal-max gets reset to the supplied
    *   value as well, so that cal-max is still no
    *   less than cal-min.
    */
    public void setCalMin(float cal) {
        cal_min = cal;
        if (cal_min > cal_max) {
            cal_max = cal_min;
        }
    }

    /** accessor to cal-min
    *   @return float cal_min
    */
    public float getCalMin() {return cal_min;}

    /** sets the compressed variable */
    public void setCompressed(float comp) {
        compressed = comp;
    }

    /** provides the compressed value
    *   @return float compressed
    */
    public float getCompressed() {return compressed;}

    /** supply the value to verified. */
    public void setVerified(float veri) {
        verified = veri;
    }

    /** provides the verified value
    *   @return float verified
    */
    public float getVerified() {return verified;}

    /** sets glmax.  if supplied value is less than
    *   glmin, the glmin gets reset to the supplied
    *   value as well, so that glmin is still no
    *   greater than glmax.
    */
    public void setGLmax(int gl) {
        glmax = gl;
        if (glmax < glmin) {
            glmin = glmax;
        }
    }

    /** provides the value of glmax
    *   @return int glmax
    */
    public int getGLmax() {return glmax;}

    /** sets glmin.  if supplied value is greater than
    *   glmax, the glmax gets reset to the supplied
    *   value as well, so that glmax is still no
    *   less than glmin.
    */
    public void setGLmin(int gl) {
        glmin = gl;
        if (glmin > glmax) {
            glmax = glmin;
        }
    }

    /** provides the value of glmin
    *   @return int glmin
    */
    public int getGLmin() {return glmin;}

    /** supply the value of session error */
    public void setSessionErr(short s) {
        session_error = s;
    }

    /** provide the value of session err
    *   @return short session_error
    */
    public short getSessionErr() {return session_error;}

    /** supply the value of regular character */
    public void setRegular(char ch) {
        regular = ch;
    }

    /** provides the value of regular character
    *   @return char value of regular character
    */
    public char getRegular() {return regular;}

    /** allows no more than 4 characters for the string
    *   describing the vox units
    */
    public void setVoxUnits(String vox) {
        vox_units = setString(vox, 4);
    }

    /** provides the string for vox-units
    *   @return String string for vox_units
    */
    public String getVoxUnits() {return vox_units;}

    /** allows no more than 8 characters for the string
    *   describing the cal units.
    */
    public void setCalUnits(String cal) {
        cal_units = setString(cal, 8);
    }

    /** provides the string for cal units.
    *   @return String string for cal_units
    */
    public String getCalUnits() {return cal_units;}

    /** Sets the orientation using MIPAV specific orientation.
    *   (AXIAL, SAGITTAL, CORONAL, UNKNOWN)
    *   @param orientation 1 these options (AXIAL, SAGITTAL, CORONAL, UNKNOWN)
    *   @see FileInfoBase
    */
    public void setImageOrientation(int orientation) {
        if (orientation == FileInfoBase.AXIAL) {
            orient = (byte)FileInfoSPM.TRANSVERSE_UNFLIPPED;
        }
        else if (orientation == FileInfoBase.CORONAL) {
            orient = (byte)FileInfoSPM.CORONAL_UNFLIPPED;
        }
        else if (orientation == FileInfoBase.SAGITTAL) {
            orient = (byte)FileInfoSPM.SAGITTAL_UNFLIPPED;
        }
        else {
            orient = FileInfoSPM.UNKNOWN_ORIENT;
        }

        super.setImageOrientation(orientation);
        setAxisOrientation(orientation);
    }

    /** Sets the image orientation converting from SPM specific
    *   orientation to general orientation.
    *   Value may be transverse, coronal, or sagittal.
    *   <p>
    *   see the top of this document for all possible values.
    *   supplying a value other than one of these 6 possiblities
    *   sets the orientation to an impossible 'flag' value.
    *   @see FileInfoBase#setImageOrientation(int)
    */
    public void setOrientation(byte orientation) {
        if ((orientation == (byte)FileInfoSPM.TRANSVERSE_UNFLIPPED)||
            (orientation == (byte)FileInfoSPM.CORONAL_UNFLIPPED)   ||
            (orientation == (byte)FileInfoSPM.SAGITTAL_UNFLIPPED)  ||
            (orientation == (byte)FileInfoSPM.TRANSVERSE_FLIPPED)  ||
            (orientation == (byte)FileInfoSPM.CORONAL_FLIPPED)     ||
            (orientation == (byte)FileInfoSPM.SAGITTAL_FLIPPED)) {

                orient = orientation;

            if ( (orientation == (byte)FileInfoSPM.TRANSVERSE_UNFLIPPED)    ||
                 (orientation == (byte)FileInfoSPM.TRANSVERSE_FLIPPED) )      {
                super.setImageOrientation(FileInfoBase.AXIAL);
                setAxisOrientation(FileInfoBase.AXIAL);
            }
            else if ( (orientation == (byte)FileInfoSPM.CORONAL_UNFLIPPED)  ||
                      (orientation == (byte)FileInfoSPM.CORONAL_FLIPPED) )    {
                super.setImageOrientation(FileInfoBase.CORONAL);
                setAxisOrientation(FileInfoBase.CORONAL);
            }
            else if ( (orientation == (byte)FileInfoSPM.SAGITTAL_UNFLIPPED) ||
                      (orientation == (byte)FileInfoSPM.SAGITTAL_FLIPPED) )   {
                super.setImageOrientation(FileInfoBase.SAGITTAL);
                setAxisOrientation(FileInfoBase.SAGITTAL);
            }
            else {
                super.setImageOrientation(FileInfoBase.UNKNOWN_ORIENT);
            }
        }
        else {
            super.setImageOrientation(FileInfoBase.UNKNOWN_ORIENT);
            orient = FileInfoSPM.UNKNOWN_ORIENT;
            setAxisOrientation(orient);
        }

    }

    /** provides the current orientation value.
    *   @return byte orientation value
    */
    public byte getOrientation() {return orient;}

    public void setOrigin(short origin[]) {
        this.origin = origin;
    }

    /**
    *   @return origin
    */
    public short[] getOriginLoc() {return origin;}

    /** allows a maximum of 10 characters to set the generated string. */
    public void setGenerated(String gen) {
        generated = setString(gen, 10);
    }

    /** provides the current generated string.
    *   @return String generated string
    */
    public String getGenerated() {return generated;}

    /** allows a maximum of 10 characters to set the scannum string. */
    public void setScanNum(String num) {
        scannum = setString(num, 10);
    }

    /** provides the current scannum string.
    *   @return String current scannum string
    */
    public String getScanNum() {return scannum;}

    /** allows a maximum of 10 characters to set the patient ID string. */
    public void setPatientID(String IDnum) {
        patient_id = setString(IDnum, 10);
    }

    /** provides the current patient id.
    *   @return String the patient id
    */
    public String getPatientID() {return patient_id;}

    /** allows a maximum of 10 characters to set the
    *   experiment date string.
    */
    public void setExperimentDate(String date) {
        exp_date = setString(date, 10);
    }

    /** provides the current experiment date string.
    *   @return String  the experiment date.
    */
    public String getExperimentDate() {return exp_date;}

    /** allows a maximum of 10 characters to set the
    *   experiment time string.
    */
    public void setExperimentTime(String time) {
        exp_time = setString(time, 10);
    }

    /** provides the current experiment time string.
    *   @return String  the experiment time
    */
    public String getExperimentTime() {return exp_time;}

    //public void setHistUn0            /* thinks this is unused !! */
    /** sets the views variable. */
    public void setViews(int view) {
        views = view;
    }

    /** provides current views value.
    *   @return int the view
    */
    public int getViews() {return views;}

    /** supplies a value to the vols_added valiable. */
    public void setVolsAdded(int vols) {vols_added = vols;}

    /** provides the current vols_added value.
    *   @return int the vols_added
    */
    public int getVolsAdded() {return vols_added;}

    /** supplies the submitted value to the start_field variable. */
    public void setStartField(int field) {start_field = field;}

    /** provides the current value for the start_field.
    *   @return int the start_field
    */
    public int getStartField() {return start_field;}

    /** supplies the submitted value to the field_sip variable. */
    public void setFieldSkip(int field) {
        field_skip = field;
    }

    /** provides the current field_skip value
    *   @return int the field_skip value
    */
    public int getFieldSkip() {return field_skip;}

    /** sets omax.  if supplied value is less than
    *   omin, the omin gets reset to the supplied
    *   value as well, so that omin is still no
    *   greater than omax.
    */
    public void setOmax(int o) {
        omax = o;
        if (omax < omin) {
            omin = omax;
        }
    }

    /** provides current omax value
    *   @return int the omax value
    */
    public int getOmax() { return omax;}

    /** sets omin.  if supplied value is greater than
    *   omax, the omax gets reset to the supplied
    *   value as well, so that omax is still no
    *   less than omin.
    */
    public void setOmin(int o) {
        omin = o;
        if (omin > omax) {
            omax = omin;
        }
    }

    /** provides current omin value
    *   @return int the omin value
    */
    public int getOmin() {return omin;}

    /** sets smax.  if supplied value is less than
    *   smin, the smin gets reset to the supplied
    *   value as well, so that smin is still no
    *   greater than smax.
    */
    public void setSmax(int s) {
        smax = s;
        if (smax < smin) {
            smin = smax;
        }
    }

    /** provides current smax value
    *   @return int the smax value
    */
    public int getSmax() {return smax;}

    /** sets smin.  if supplied value is greater than
    *   smax, the smax gets reset to the supplied
    *   value as well, so that smax is still no
    *   less than smin.
    */
    public void setSmin(int s) {
        smin = s;
        if (smin > smax) {
            smax = smin;
        }
    }

    /** provides current smin value
    *   @return int the smin value
    */
    public int getSmin() {return smin;}

    /**
    *   Sets the axis orientation based on the image orientation.
    *   @param imageOrient the image's orienation (Axial, Sagittal, Coronal)
    */
    public void setAxisOrientation(int imageOrient) {
        int axisX, axisY, axisZ;
        if (imageOrient == FileInfoBase.AXIAL) {
            axisX = FileInfoBase.ORI_R2L_TYPE;
            axisY = FileInfoBase.ORI_A2P_TYPE;
            axisZ = FileInfoBase.ORI_I2S_TYPE;
        }
        else if (imageOrient == FileInfoBase.SAGITTAL) {
            axisX = FileInfoBase.ORI_P2A_TYPE;
            axisY = FileInfoBase.ORI_S2I_TYPE;
            axisZ = FileInfoBase.ORI_R2L_TYPE;
        }
        else if (imageOrient == FileInfoBase.CORONAL) {
            axisX = FileInfoBase.ORI_R2L_TYPE;
            axisY = FileInfoBase.ORI_S2I_TYPE;
            axisZ = FileInfoBase.ORI_P2A_TYPE;
        }
        else {
            axisX = (short)FileInfoBase.ORI_UNKNOWN_TYPE;
            axisY = (short)FileInfoBase.ORI_UNKNOWN_TYPE;
            axisZ = (short)FileInfoBase.ORI_UNKNOWN_TYPE;
        }
        super.setAxisOrientation(axisX, 0);
        super.setAxisOrientation(axisY, 1);
        super.setAxisOrientation(axisZ, 2);
    }



    /**
    *   Sets orientation of each axis
    *   @param axOrient  axis orientation array
    *   @see FileInfoBase#getAxisOrientation()
    */
    public void setAxisOrientation(int[] axOrient) {
        if (axOrient.length != 3) {
            Preferences.debug("Axis orientations array must be of length 3.\n");
            return;
        }
        for (int i=0; i<axOrient.length; i++)
            setAxisOrientation(axOrient[i], i);
    }




    /**
    *   @param scale
    */
    public void setScale(float scale) {
        if (scale == 0.0f) scale = 1.0f;
       this.scale = scale;
    }

    /**
    *   @return float scale
    */
    public float getScale() {
        return scale;
    }


    public void setDim(short un0) {dim_un0 = un0;}
    public short getDim() {return dim_un0;}

    public void setHkey(char hk) {hkey_un0 = hk;}
    public char getHkey() {return hkey_un0;}

    public void setSizeOfHeader(int size) {sizeof_hdr = size;}
    public int getSizeOfHeader() {return sizeof_hdr;}

    public void setHist(String hist) {hist_un0 = setString(hist, 3);}
    public String getHist() {return hist_un0;}

    public void setFileExtents(int ext) {extents = ext;}
    public int getFileExtents() {return extents;}

    /** <table>
    *   <tr><td>ce[0] = table           </td><td>0 = primary, 1 = secondary, etC</td></tr>
    *   <tr><td>ce[1] = line of table   </td><td>          </td></tr>
    *   <tr><td>ce[2] = string name     </td><td>eg, "Type"</td></tr>
    *   <tr><td>ce[3] = Vector codeValue</td><td>eg, "B"   </td></tr>
    *   <tr><td>ce[4] = string value    </td><td>eg, "Big" </td></tr>
    *   </table>
    *   "ce" comes from ChangeEvent upon which this is based.  care to
    *   make our own ChangeEvent to store and handle this?
    */
    public void stateChanged(Vector ce) {
        String tname = (String) ce.elementAt(2);  // [t]able [name]
        Vector tcvalue = (Vector) ce.elementAt(3);// [t]able [c]ode [value]
        String tvalue= (String) ce.elementAt(4);  // [t]able [value]

        if (tname.equalsIgnoreCase ("Description")) {
            setDescription(tvalue);
        }
        else if (tname.equalsIgnoreCase("data type")) {
            setDataType(tvalue);
        }
        else if (tname.equalsIgnoreCase("db name")) {
            setDBname(tvalue);
        }
        else if (tname.equalsIgnoreCase("voxel units")) {
            setVoxUnits(tvalue);
        }
        else if (tname.equalsIgnoreCase("cal units")) {
            setCalUnits(tvalue);
        }
        else if (tname.equalsIgnoreCase("voxel offset")) {
            setVoxOffset( Float.parseFloat((String) tcvalue.elementAt(0)));
        }
        else if (tname.equalsIgnoreCase("cal_min")) {
            setCalMin( Float.parseFloat( (String)tcvalue.elementAt(0)));
        }
        else if (tname.equalsIgnoreCase("cal_max")) {
            setCalMax(Float.parseFloat((String) tcvalue.elementAt(0)));
        }
        else if (tname.equalsIgnoreCase("glmax")) {
            setGLmax(Integer.parseInt((String) tcvalue.elementAt(0)));
        }
        else if (tname.equalsIgnoreCase("glmin")) {
            setGLmin(Integer.parseInt((String) tcvalue.elementAt(0)));
        }
        else if (tname.equalsIgnoreCase("Orientation")) {
            setOrientation(((Byte)tcvalue.elementAt(0)).byteValue());
            //setImageOrientation(((Byte) tcvalue.elementAt(0)).byteValue());
        }
        else if (tname.equalsIgnoreCase("generated")) {
            setGenerated(tvalue);
        }
        else if (tname.equalsIgnoreCase("scan number")) {
            setScanNum(tvalue);
        }
        else if (tname.equalsIgnoreCase("patient id")) {
            setPatientID(tvalue);
        }
        else if (tname.equalsIgnoreCase("experiment date")) {
            setExperimentDate(tvalue);
        }
        else if (tname.equalsIgnoreCase("experiment time")) {
            setExperimentTime(tvalue);
        }
        else if (tname.equalsIgnoreCase("views")) {
            setViews(Integer.parseInt((String) tcvalue.elementAt(0)));
        }
        else if (tname.equalsIgnoreCase("volume added")) {
            setVolsAdded(Integer.parseInt((String) tcvalue.elementAt(0)));
        }
        else if (tname.equalsIgnoreCase("start field")) {
            setStartField(Integer.parseInt((String) tcvalue.elementAt(0)));
        }
        else if (tname.equalsIgnoreCase("field skip")) {
            setFieldSkip(Integer.parseInt((String) tcvalue.elementAt(0)));
        }
        else if (tname.equalsIgnoreCase("omax")) {
            setOmax(Integer.parseInt((String) tcvalue.elementAt(0)));
        }
        else if (tname.equalsIgnoreCase("omin")) {
            setOmin(Integer.parseInt((String) tcvalue.elementAt(0)));
        }
        else if (tname.equalsIgnoreCase("smax")) {
            setSmax(Integer.parseInt((String) tcvalue.elementAt(0)));
        }
        else if (tname.equalsIgnoreCase("smin")) {
            setSmin(Integer.parseInt((String) tcvalue.elementAt(0)));
        }
        else if (tname.startsWith("Start Location: x-axis")) {
            setOrigin(Float.parseFloat((String) tcvalue.elementAt(0)), 0);
        }
        else if (tname.startsWith("Start Location: y-axis")) {
            setOrigin(Float.parseFloat((String) tcvalue.elementAt(0)), 1);
        }
        else if (tname.startsWith("Start Location: z-axis")) {
            setOrigin(Float.parseFloat((String) tcvalue.elementAt(0)), 2);
        }
        else if (tname.equalsIgnoreCase("Orientation")) {
            setImageOrientation( ((Integer)tcvalue.elementAt(0)).intValue());
            //setOrientation(((Byte)tcvalue.elementAt(0)).byteValue());

        }
        else {Preferences.debug("tname: "+tname + ", not found.");}
    }

    /** selection of text for the table and the editpanel.
    *   @return String the orientation string
    */
    private String selectOrientationText(int or) {
        String orientat;
        switch (or) {
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

    /** Propogates the current file info to another FileInfoSPM
    *   except for the 3 start locations kept in funused1, funused2, funused3.
    *   <p>
    *   It does not copy over the datatypeCode. (though, aside from, "it isn't
    *   in the about table", I can't think of a reason why it shouldn't.
    *   but it doesn't.)  Also, copied over is bitPix, aux_file.
    *
    *   <p>
    *   Some variables, such as the unused1, unused2, unused3, are really taken
    *   from unused, rather than from the start locations as we <b>are</b>
    *   re-using the funused variables.  This means that if the unused
    *   variables are not right to begin with, this certainly won't correct them!
    *
    */
    public void updateFileInfos(FileInfoSPM fInfo) {
        if (this == fInfo) {
            return;
        }
        //fInfo.setAuxFile            (this.getAuxFile());// not editable by the table!!
        //fInfo.setBitPix             (this.getBitPix()); // not editable by the table!!
        fInfo.setCalMin             (this.getCalMin());
        fInfo.setCalMax             (this.getCalMax());
        fInfo.setCalUnits           (this.getCalUnits());
        fInfo.setCompressed         (this.getCompressed());
        //fInfo.setDataTypeCode       (this.getDataTypeCode());//not edited by the table!!
        fInfo.setDataType           (this.getDataTypeName());
        fInfo.setDBname             (this.getDBname());
        fInfo.setDescription        (this.getDescription());
        fInfo.setDim                (this.getDim());
        fInfo.setExperimentDate     (this.getExperimentDate());
        fInfo.setExperimentTime     (this.getExperimentTime());
        fInfo.setFieldSkip          (this.getFieldSkip());
        fInfo.setGenerated          (this.getGenerated());
        fInfo.setGLmin              (this.getGLmin());
        fInfo.setGLmax              (this.getGLmax());
        fInfo.setHist               (this.getHist());
        fInfo.setHkey               (this.getHkey());
        fInfo.setOmin               (this.getOmin());
        fInfo.setOmax               (this.getOmax());
        fInfo.setOrientation        (this.getOrientation());
        fInfo.setOrigin             (this.getOrigin());
        fInfo.setPatientID          (this.getPatientID());
        fInfo.setRegular            (this.getRegular());
        fInfo.setScale              (this.getScale());
        fInfo.setScanNum            (this.getScanNum());
        fInfo.setSessionErr         (this.getSessionErr());
        fInfo.setSmax               (this.getSmax());
        fInfo.setSmin               (this.getSmin());
        fInfo.setStartField         (this.getStartField());
        fInfo.setVerified           (this.getVerified());
        fInfo.setViews              (this.getViews());
        fInfo.setVolsAdded          (this.getVolsAdded());
        fInfo.setVoxOffset          (this.getVoxOffset());
        fInfo.setVoxUnits           (this.getVoxUnits());
    }
}
