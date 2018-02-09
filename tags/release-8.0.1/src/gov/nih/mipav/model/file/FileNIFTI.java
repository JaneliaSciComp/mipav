package gov.nih.mipav.model.file;


import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;
import Jama.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;
import java.util.zip.GZIPInputStream;
import java.util.zip.ZipInputStream;

import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;
import org.codehaus.jettison.json.JSONArray;

import java.text.*;


/**
 * The class reads and writes NIFTI files. The header is intended to be "mostly compatible" with the ANALYZE (TM) 7.5
 * file format. Most of the "unused" fields in that format have been taken, and some of the lesser-used fields have been
 * co-opted for other purposes.
 * 
 * NIFTI origin
Author: William Gandler (---.cit.nih.gov)
Date:   03-13-09 18:31

Are qoffset_x and srow_x[3] always meant to be an origin in the RL axis, qoffset_y and srow_y[3] always meant to be an origin in the AP axis, and are qoffset_z and srow_z[3] always meant to be an origin in the IS axis?

 *
 *Re: NIFTI origin
Author: rick reynolds (---.nimh.nih.gov)
Date:   03-16-09 11:02

Hi William,

Yes. The qoffset_{x,y,z} values are explicitly LR, PA, IS, as the
transformations always describe resulting coordinates in LPI (sign
and order) orientation. The sign of those coordinates corresponds
to LPI being the negative directions.

This is much like Dicom images, which give coordinates in RAI,
regardless of the actual orientation of the data on disk. There,
RAI are the negative directions (2 of them opposite that of NIfTI).

These offset coordinates are applied after the transformation
matrix is applied, and so they must be LR, PA and IS, respectively.

The same applies for the sform matrix.

See the section on "ORIENTATION AND LOCATION IN SPACE" from the
nifti.h standard for details:

http://nifti.nimh.nih.gov/pub/dist/src/niftilib/nifti1.h

- rick
 * <p>NIFTI can have 2 different transformation matrices
 * associated with an image - one stored in the qform_code parameters and one stored in the sform_code parameters. While
 * MIPAV separately stores axis orientation and matrix information, NIFTI does not store axis orientation information.
 * NIFTI uses a routine to derive axis orientations from the upper 3 by 3 parameters of the 4 by 4 matrix. The 4 by 4
 * matrix in NIFTI transforms x,y,z indexes to (right, anterior, superior) coordinates where +x = Right, +y = Anterior,
 * +z = Superior. In MIPAV the 4 by 4 matrix does not imply the axis orientations.</p>
 *
 * <p>For qform_code > 0, which should be the normal case the NIFTI definition is: [right] [R11 R12 R13] [ pixdim[1] *
 * i] [qoffset_right] [anterior] = [R21 R22 R23] [ pixdim[2] * j] + [qoffset_anterior] [superior] [R31 R32 R33] [qfac *
 * pixdim[3] * k] [qoffset_superior] Now in going to MIPAV 2 changes must occur. 1.) NIFTI is L->R and P->A while MIPAV
 * is R->L and A->P, so this would cause R11, R12, R13, qoffset_right, R21, R22, R23, and qoffset_anterior to be
 * multiplied by -1.  2.) R13, R23, and R33 are multiplied by qfac. So we in going to MIPAV we use
 * -R11, -R12, -R13*qfac, -qoffset_right, -R21, -R22, -R23*qfac, -qoffset_anterior, and R33*qfac. 
 * If qform_code == 0 and sform_code > 0:
 *  x = srow_x[0]* i + srow_x[1] * j + srow_x[2] * k + srow_x[3]
 *  y = srow_y[0]* i + srow_y[1] * j + srow_y[2] * k + srow_y[3]
 *  z = srow_z[0]* i + srow_z[1] * j + srow_z[2] * k + srow_z[3] 
 * In going to MIPAV we use -srow_x[0], -srow_x[1] -srow_x[2],
 * -srow_x[3], -srow_y[0], -srow_y[1] -srow_y[2], and -srow_y[3].</p>
 *
 * <p>MIPAV ANALYZE uses 6 for 16 bit unsigned short in the datatype field while NIFTI uses 512 for 16 bit unsigned
 * short in the datatype field. NIFTI also has a signed char at 256, an unsigned int at 768, a LONG at 1024, an unsigned
 * long at 1280, an 128 bit float at 1536, a 128 bit COMPLEX at 1792, and a 256 bit COMPLEX at 2048 which are not
 * present in ANALYZE. MIPAV cannot presently handle a 128 bit float or a 256 bit COMPLEX. If scl_slope != 1.0 or
 * scl_inter != 0.0, then source data types will be promoted to higher data types if necessary.</p>
 *
 * <p>At location 56 our hacked anlayze has a 4 byte string for voxel units, while NIFTI has the float intent_p1. At
 * location 60 our hacked analyze has a 4 byte string for cal units, while NIFTI has the float intent_p2. At locations
 * 64 and 66 our hacked analyze has shorts for axis orientations 0 and 1, while NIFTI has the float intent_p3. At
 * location 68 our hacked analyze has the short for axis orientation 2, while NIFTI has the short for intent_code.</p>
 *
 * <p>At location 76 both ANALYZE and NIFTI have 8 floats for resolutions. In ANALYZE pixdim[0] at location 76 is
 * unused, but in NIFTI if (pixdim[0] >= 0) qfac is taken as 1. Otherwise, if (pixdim[0] < 0) in NIFTI qfac is taken as
 * -1. qfac is used in determining the transformation matrix in method 2. In both ANALYZE and NIFTI the x resolution
 * pixdim[1] is found at location 80, the y resolution pixdim[2] is found at location 84, the z resolution pixdim[3] is
 * found at location 88, and the t resolution pixdim[4] is found at location 92. In NIFTI L to R, P to A, and I to S are
 * defined as positive directions. This differs from AFNI and MIPAV where R to L, A to P, and I to S are positive.
 * ANALYZE 7.5 has R to L, P to A, and I to S positive. Thus, NIFTI, AFNI, and MIPAV have right handed coordinate
 * systems, while ANALYZE has a left handed coordinate system.</p>
 *
 * <p>At location 112 our hacked analyze has a float for origin[0], while NIFTI has the float scl_slope. At location 116
 * our hacked analyze has a float for origin[1], while NIFTI has the float scl_inter. The data is scaled according to:
 * scaled_data[i] = scl_slope * unscaled_data[i] + scl_inter At location 120 our hacked analyze has a float for
 * origin[2], while the NIFTI location 120 has short slice_end, location 122 has char slice_code, and location 123 has
 * char xyzt_units. NIFTI has qoffset_x, qoffset_y, and qoffset_z in locations 268, 272, and 276.</p>
 *
 * @see  FileIO
 * @see  FileInfoNIFTI
 * @see  FileRaw
 */

public class FileNIFTI extends FileBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** R2L, L2R, A2P, P2A, I2S, and S2I orientations of x, y, and z axes. */
    private int[] axisOrientation;

    /** When both qform_code > 0 and sform_code > 0, the axis orientation information corresponding to sform_code > 0
     *  is placed in axisOrientation2 */
    private int[] axisOrientation2;

    /** A byte array of the size of the NIFTI header + 4 extension bytes. */
    private byte[] bufferByte = null;

    /** If qform_code > 0, coord_code = qform_code.  If qform_code <= 0 and sform_code > 0, coord_code = sform_code.
     *  coord_code has values for "Arbitrary X,Y,Z coordinate system", "Scanner based anatomical coordinates",
     *  "Coordinates aligned to another file's or to anatomical truth", "Talairach X,Y,Z coordinate system", and
     *  "MNI 152 normalized X,Y,Z coordinates".   */
    private short coord_code;

    /** DOCUMENT ME! */
    private String fileDir;

    /** DOCUMENT ME! */
    private File fileHeader;

    /** DOCUMENT ME! */
    private FileInfoNIFTI fileInfo = null;

    /** DOCUMENT ME! */
    private String fileName;

    /** Bits 0 and 1 of the dim_info character contain the freq_dim information.
     *  0 for "No frequency encoding direction is present", 1 for "Frequency encoding in the x direction",
     *  2 for "Frequency encoding in the y direction", and 3 for "Frequency encoding in the z direction". */
    private int freq_dim = 0;

    /** The size of the NIFTI header must be set to 348 bytes. */
    private int headerSize = 348;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private double imageMax;

    /** DOCUMENT ME! */
    private double imageMin;

    /** The intent_name field provides space for a 15 character (plus 0 byte)
        name string for the type of data stored. Examples:
        - intent_code = NIFTI_INTENT_ESTIMATE; intent_name = "T1";
        could be used to signify that the voxel values are estimates of the
        NMR parameter T1.
        - intent_code = NIFTI_INTENT_TTEST; intent_name = "House";
        could be used to signify that the voxel values are t-statistics
        for the significance of activation response to a House stimulus.
        - intent_code = NIFTI_INTENT_DISPVECT; intent_name = "ToMNI152";
        could be used to signify that the voxel values are a displacement
        vector that transforms each voxel (x,y,z) location to the
        corresponding location in the MNI152 standard brain.
        - intent_code = NIFTI_INTENT_SYMMATRIX; intent_name = "DTI";
        could be used to signify that the voxel values comprise a diffusion
        tensor image. */
    private String intentName;

    /** The MIPAV origin value which is obtained from qoffset_x, qoffset_y, and qoffset_z
     *  when qform_code > 0 or from srow_x[3], srow_y[3], and srow_z[3] when sform_code > 0.
     *  The MIPAV value will equal the NIFTI value or the negative of the NIFTI value.
     *  The MIPAV value is stored using fileInfo.setOrigin(LPSOrigin) and  
     *  matrix.setMatrix((double) LPSOrigin[0], 0, 3);
        matrix.setMatrix((double) LPSOrigin[1], 1, 3);
        matrix.setMatrix((double) LPSOrigin[2], 2, 3);*/
    private float[] LPSOrigin;

    /** When qform_code > 0 and sform_code > 0, LPSOrigin derives the MIPAV origin from 
     *  the qform information and LPSOrigin2 derives MIPAV origin from the sform 
     *  information.  LPSOrigin2 is derived from srow_x[3], srow_y[3], and srow_z[3].
     *  The MIPAV value will equal the NIFTI value or the negative of the NIFTI value.
     *  MIPAV information is stored using
     *  matrix2.setMatrix((double) LPSOrigin2[0], 0, 3);
        matrix2.setMatrix((double) LPSOrigin2[1], 1, 3);
        matrix2.setMatrix((double) LPSOrigin2[2], 2, 3); */
    private float[] LPSOrigin2;

    /** MIPAV matrix used for storing qform or sform transformation information. */
    private TransMatrix matrix = new TransMatrix(4);
    
    /** MIPAV matrix used for storing qform or sform transformation information
     * for 2D images.
     */
    private TransMatrix matrixTwoDim = new TransMatrix(3);

    /** When qform_code > 0 and sform_code > 0, the qform transformation information
     *  is stored in matrix and the sform transformation information is stored in
     *  matrix2. */
    private TransMatrix matrix2 = null;

    /** When the image data is rescaled by
     *  y = scl_slope * x + scl_inter,
     *  the image minimum or image maximum is rescaled to newMax. */
    private double newMax;

    /** When the image data is rescaled by
     *  y = scl_slope * x + scl_inter,
     *  the image minimum or image maximum is rescaled to newMin. */
    private double newMin;

    /** If true, header and data both stored in .nii file.
     *  If false, header stored in filename.hdr and data
     *  stored in filename.img. */
    private boolean oneFile;

    /** DOCUMENT ME! */
    private float[] origin;

    /** Bits 2 and 3 of the dim_info character contain the phase_dim information.
     *  0 for "No phase encoding direction is present", 1 for "Phase encoding in the x direction",
     *  2 for "Phase encoding in the y direction", and 3 for "Phase encoding in the z direction". */
    private int phase_dim = 0;

    /** qfac is stored in the otherwise unused pixdim[0].
     *  pixdim[i] = voxel width along dimension #i, i=1..dim[0] (positive)
     *  the units of pixdim can be specified with the xyzt_units field */
    private float[] pixdim;

    /**  The scaling factor qfac is either 1 or -1. The rotation matrix R
         defined by the quaternion parameters is "proper" (has determinant 1).
         This may not fit the needs of the data; for example, if the image
         grid is
         i increases from Left-to-Right
         j increases from Anterior-to-Posterior
         k increases from Inferior-to-Superior
         Then (i,j,k) is a left-handed triple. In this example, if qfac=1,
         the R matrix would have to be
        
         [ 1 0 0 ]
         [ 0 -1 0 ] which is "improper" (determinant = -1).
         [ 0 0 1 ]
        
         If we set qfac=-1, then the R matrix would be
        
         [ 1 0 0 ]
         [ 0 -1 0 ] which is proper.
         [ 0 0 -1 ]
         qfac is stored in the otherwise unused pixdim[0]. */
    private float qfac;

    /** When qform_code > 0 the (x,y,z) coordinates are given by:
     *  [ x ]   [ R11 R12 R13 ] [        pixdim[1] * i ]   [ qoffset_x ]
        [ y ] = [ R21 R22 R23 ] [        pixdim[2] * j ] + [ qoffset_y ]
        [ z ]   [ R31 R32 R33 ] [ qfac * pixdim[3] * k ]   [ qoffset_z ] 
        where the R rotation parameters are calculated from the 
        quaternion parameters.
        qform_code has values for "Arbitrary X,Y,Z coordinate system", "Scanner based anatomical coordinates",
     *  "Coordinates aligned to another file's or to anatomical truth", "Talairach X,Y,Z coordinate system", and
     *  "MNI 152 normalized X,Y,Z coordinates".   */
    private short qform_code;

    /** Quaternion x shift */
    private float qoffset_x;

    /** Quaternion y shift */
    private float qoffset_y;

    /** Quaternion z shift */
    private float qoffset_z;

    /** The orientation of the (x,y,z) axes relative to the (i,j,k) axes
   in 3D space is specified using a unit quaternion [a,b,c,d], where
   a*a+b*b+c*c+d*d=1.  The (b,c,d) values are all that is needed, since
   we require that a = sqrt(1.0-(b*b+c*c+d*d)) be nonnegative. */
    private float quatern_a;

    /** Quaternion b parameter */
    private float quatern_b;

    /** Quaternion c parameter */
    private float quatern_c;

    /** Quaternion d parameter */
    private float quatern_d;

    /** The (proper) 3x3 rotation matrix that
        corresponds to quaternion [a,b,c,d] is

         [ a*a+b*b-c*c-d*d   2*b*c-2*a*d       2*b*d+2*a*c     ]
     R = [ 2*b*c+2*a*d       a*a+c*c-b*b-d*d   2*c*d-2*a*b     ]
         [ 2*b*d-2*a*c       2*c*d+2*a*b       a*a+d*d-c*c-b*b ] */
    private double r00, r01, r02;
    private double r10, r11, r12;
    private double r20, r21, r22;
    // Used in writing the DICOM (0020, 0037) patient orientation tag
    private String patientOrientationString = null;

    /** NIFTI pixdim information is converted into MIPAV resolutions information
     *  Only those pixdim[i] for which the niftiExtents[i] > 1 are passed into
     *  a resolutions[j] value. */
    private float[] resolutions;

    /** If the scl_slope field is nonzero, then each voxel value in the dataset
        should be scaled as
        y = scl_slope * x + scl_inter
        where x = voxel value stored
        y = "true" voxel value
        Normally, we would expect this scaling to be used to store "true" floating
        values in a smaller integer datatype, but that is not required.  That is,
        it is legal to use scaling even if the datatype is a float type (crazy,
        perhaps, but legal).
        - However, the scaling is to be ignored if datatype is DT_RGB24.
        - If datatype is a complex type, then the scaling is to be
        applied to both the real and imaginary parts. */
    private float scl_slope;
    private float scl_inter;   

    /** When sform_code > 0,
     *  The (x,y,z) coordinates are given by a general affine transformation
        of the (i,j,k) indexes:

        x = srow_x[0] * i + srow_x[1] * j + srow_x[2] * k + srow_x[3]
        y = srow_y[0] * i + srow_y[1] * j + srow_y[2] * k + srow_y[3]
        z = srow_z[0] * i + srow_z[1] * j + srow_z[2] * k + srow_z[3]
        sform_code has values for "Arbitrary X,Y,Z coordinate system", "Scanner based anatomical coordinates",
     *  "Coordinates aligned to another file's or to anatomical truth", "Talairach X,Y,Z coordinate system", and
     *  "MNI 152 normalized X,Y,Z coordinates".   */
    private short sform_code;

    /** Bits 4 and 5 of the dim_info character contain the slice_dim information.
     *  0 for "No slice acquisition direction is present", 1 for "Slice acquisition in the x direction",
     *  2 for "Slice acquisition in the y direction", and 3 for "Slice acquisition in the z direction". */
    private int slice_dim = 0;

    /** If this is nonzero, AND if slice_dim is nonzero, AND
        if slice_duration is positive, indicates the timing
        pattern of the slice acquisition.  The following codes
        are defined:
        "Slice timing order is sequentially increasing",
        "Slice timing order is sequentially decreasing",
        "Slice timing order is alternately increasing",
        "Slice timing order is alternately decreasing",
        "Slice timing order is alternately increasing #2",
        "Slice timing order is alternately decreasing #2". */
    private byte sliceCode;

    /** Time used to acquire 1 slice. */
    private float sliceDuration;

    /** Slice timing pattern ends with slice = (sliceEnd + 1) */
    private short sliceEnd;

    /** Slice timing pattern starts with slice = (sliceStart + 1) */
    private short sliceStart;

    /** Source bits per pixel = sourceBitPix */
    private short sourceBitPix;

    /** Original unscaled source data type */
    private short sourceType;

    /** Bits 0, 1, and 2 of xyzt_units specify the units of pixdim[1..3],
     *  that is the spatial units of the nifti x, y, and z axes.
     *  0 means "Spatial units are unknown",
     *  1 means "Spatial units are meters",
     *  2 means "Spatial units are millimeters",
     *  3 means "Spatial units are micrometers". */
    private int spaceUnits = FileInfoNIFTI.NIFTI_UNITS_UNKNOWN;

    /** 1st row affine transform */
    private float[] srow_x;

    /** 2nd row affine transform */
    private float[] srow_y;

    /** 3rd row affine transform */
    private float[] srow_z;

    /** Bits 3, 4, and 5 of xyzt_units specify the units of pixdim[4],
     *  that is the temporal units of the nifti time axis.
        Temporal units are multiples of 8.
        0 means "Temporal units are unknown",
        8 means "Temporal units are seconds",
        16 means "Temporal units are milliseconds",
        24 means "Temporal units are microseconds",
        32 means "Temporal units are Hertz",
        40 means "Temporal units are parts per million",
        48 means "Temporal units are Radians per second." */
    private int timeUnits = FileInfoNIFTI.NIFTI_UNITS_UNKNOWN;

    /** The toffset field can be used to indicate a nonzero start point for
        the time axis.  That is, time point #m is at t=toffset+m*pixdim[4]
        for m=0..dim[4]-1. */
    private float tOffset;

    /** If the magic field is "n+1", then the voxel data is stored in the same file as the header.
     *  In this case, the voxel data starts at offset (int)vox_offset into the header file. Thus,
     *  vox_offset=352.0 means that the data starts immediately after the NIFTI-1 header. If vox_offset
     *  is greater than 352, the NIFTI-1 format does not say much about the contents of the dataset
     *  file between the end of the header and the start of the data.

        If the magic field is "ni1", then the voxel data is stored in the associated ".img" file,
        starting at offset 0 (i.e., vox_offset is not used in this case, and should be set to 0.0).

        In a .nii file, the vox_offset field value is interpreted as the start location of the image
        data bytes in that file. In a .hdr/.img file pair, the vox_offset field value is the start
        location of the image data bytes in the .img file. If vox_offset is less than 352 in a .nii
        file, it is equivalent to 352 (i.e., image data never starts before byte #352 in a .nii file).
        The default value for vox_offset in a .nii file is 352. In a .hdr file, the default value for
        vox_offset is 0. * vox_offset should be an integer multiple of 16; otherwise, some programs
        may not work properly (e.g., SPM). This is to allow memory-mapped input to be properly byte-aligned. */
    private float vox_offset = 0.0f;
    
    // Number of bytes in the extended header including 8 bytes of esize and eocde themselves
    // esize must be a positive integral multiple of 16.
    private int esize = 0;
    
    // A non-negative integer that indicates the format of the extended header data that follows
    // Different ecode values are assigned to different developer groups
    // At present, the "registered" values for the code are
    // = 0 = unknown private format (not recommended!)
    // = 2 = DICOM format (i.e., attribute tags and values)
    // = 4 = AFNI group(i.e., ASCII XML-ish elements)
    private int ecode;
    
    private int esizeArray[] = null;
    
    private int ecodeArray[] = null;
    
    private String mindIdentArray[] = null;
    
    private float bValueArray[] = null;
    
    private float azimuthArray[] = null;
    
    private float zenithArray[] = null;
    
    private int dtComponentArray[][] = null;
    
    private int degreeArray[] = null;
    
    private int orderArray[] = null;
    
    private String afniGroupArray[] = null;
    
    private String asciiTextArray[] = null;
    
    private String caretArray[] = null;
    
    private String jsonArray[] = null;
    
    /**
     * File object and input streams 
     * needed for NIFTI compressed files
     */
    private File file;
    private FileInputStream fis;
    private ZipInputStream zin;
    private GZIPInputStream gzin;
    private CBZip2InputStream bz2in;
    private boolean noReadPrivateTags;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new file object.
     *
     * @param  fName  File name.
     * @param  fDir   File directory.
     */
    public FileNIFTI(String fName, String fDir) {
        String fileDataName;
        File fileData;
        int index = fName.length();

        for (int i = fName.length() - 1; i >= 0; i--) {

            if (fName.charAt(i) == '.') {
                index = i;

                break;
            }
        }
        if (fName.substring(index).equalsIgnoreCase(".HDR")) {
            fileDataName = fName.substring(0, index) + ".img";
            fileData = new File(fDir + fileDataName);
            if (fileData.exists()) {
                fName = fileDataName;
            }
            else {
                fileDataName = fName.substring(0, index) + ".IMG";
                fileData = new File(fDir + fileDataName); 
                if (fileData.exists()) {
                    fName = fileDataName;
                }
            }
        }
        fileName = fName;
        fileDir = fDir;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Method determines if the image format is NIFTI, so FileIO will call the appropriate FileNIFTI read method.
     *
     * @param   fName  File name of image.
     * @param   fDir   Directory.
     *
     * @return  <code>true</code> if NIFTI file, <code>false</code> otherwise.
     */
    public static boolean isNIFTI(String fName, String fDir) {

        try {
            RandomAccessFile raFile;
            int index = fName.length();

            for (int i = fName.length() - 1; i >= 0; i--) {

                if (fName.charAt(i) == '.') {
                    index = i;

                    break;
                }
            }

            String fileHeaderName = fName.substring(0, index) + ".hdr";
            File fileHeader = new File(fDir + fileHeaderName);

            if (fileHeader.exists() == false) {
                fileHeaderName = fName.substring(0, index) + ".HDR";
                fileHeader = new File(fDir + fileHeaderName);

                if (fileHeader.exists() == false) {
                    fileHeaderName = fName.substring(0, index) + ".nii";
                    fileHeader = new File(fDir + fileHeaderName);

                    if (fileHeader.exists() == false) {
                        return false;
                    }
                }
            }

            raFile = new RandomAccessFile(fileHeader, "r");
            raFile.seek(344L);

            // Check that this is a NIFTI header file.
            // The NIFTI header has a 348 byte single structure.
            // To flag such a struc as being conformant to the NIFTI-1 spec,
            // the last 4 bytes of the header must be the C String "ni1" or "n+1";
            // in hexadecimal, the 4 bytes 6E 69 31 00 or 6E 2B 31 00.  Normally,
            // such a "magic number" or flag goes at the start of the file,
            // but trying to avoid clobbering widely-used ANALYZE-7.5 fields
            // led to putting this marker last.
            byte[] b = new byte[4];
            raFile.read(b);

            String tmpString = new String(b);

            raFile.close();

            if ((!tmpString.equals("ni1\0")) && (!tmpString.equals("n+1\0"))) {
                return false;
            } else {
                return true;
            }
        } catch (FileNotFoundException e) {
            MipavUtil.displayError("FileNIFTI: Error reading file.");
        } catch (IOException e) {
            MipavUtil.displayError("FileNIFTI: Error reading file.");
        }

        return false;

    }

    /**
     * Absolute value of image.
     *
     * @param   image  Image to take absolute value of.
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public void absoluteValue(ModelImage image) throws IOException {

        try {
            int nBuffers;
            int bufferSize;
            float[] buffer = null;

            if (image.getNDims() > 1) {
                bufferSize = image.getSliceSize();
            } else {
                bufferSize = image.getExtents()[0];
            }

            if (image.getNDims() == 5) {
                nBuffers = image.getExtents()[4] * image.getExtents()[3] * image.getExtents()[2];

            } else if (image.getNDims() == 4) {
                nBuffers = image.getExtents()[3] * image.getExtents()[2];
            } else if (image.getNDims() == 3) {
                nBuffers = image.getExtents()[2];
            } else {
                nBuffers = 1;
            }

            if (!image.isColorImage()) {
                buffer = new float[bufferSize];

                int i, j, k;
                int xDim = image.getExtents()[0];
                int yDim = image.getExtents()[1];

                for (k = 0; k < nBuffers; k++) {
                    image.exportData(k * bufferSize, bufferSize, buffer);

                    for (j = 0; j < yDim; j++) {

                        for (i = 0; i < xDim; i++) {
                            buffer[(j * xDim) + i] = Math.abs(buffer[(j * xDim) + i]);
                        }
                    }

                    image.importData(k * bufferSize, buffer, false);
                }
            }
        } catch (IOException error) {
            throw new IOException("FileNIFTI.absoluteValue: " + error);
        } catch (OutOfMemoryError error) {
            throw (error);
        }
    }

    /**
     * Flips image. NIFTI stores its data "upside down".
     *
     * @param   image  Image to flip.
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public void flipTopBottom(ModelImage image) throws IOException {

        try {
            int nBuffers;
            int bufferSize;
            float[] buffer = null;
            float[] resultBuffer = null;

            if (image.getNDims() > 1) {
                bufferSize = image.getSliceSize();
            } else {
                bufferSize = image.getExtents()[0];
            }

            if (image.getNDims() == 5) {
                nBuffers = image.getExtents()[4] * image.getExtents()[3] * image.getExtents()[2];

            } else if (image.getNDims() == 4) {
                nBuffers = image.getExtents()[3] * image.getExtents()[2];
            } else if (image.getNDims() == 3) {
                nBuffers = image.getExtents()[2];
            } else {
                nBuffers = 1;
            }

            if (image.isColorImage()) {

                buffer = new float[bufferSize * 4];
                resultBuffer = new float[bufferSize * 4];
                bufferSize = bufferSize * 4;

                int i, j, k;
                int xDim = image.getExtents()[0] * 4;
                int yDim = image.getExtents()[1];

                for (k = 0; k < nBuffers; k++) {
                    image.exportData(k * bufferSize, bufferSize, buffer);

                    for (j = 0; j < yDim; j++) {

                        for (i = 0; i < xDim; i += 4) {
                            resultBuffer[(j * xDim) + i] = 255;
                            resultBuffer[(j * xDim) + i + 1] = buffer[((yDim - 1 - j) * xDim) + i + 1];
                            resultBuffer[(j * xDim) + i + 2] = buffer[((yDim - 1 - j) * xDim) + i + 2];
                            resultBuffer[(j * xDim) + i + 3] = buffer[((yDim - 1 - j) * xDim) + i + 3];
                        }
                    }

                    image.importData(k * bufferSize, resultBuffer, false);
                }
            } else {
                buffer = new float[bufferSize];
                resultBuffer = new float[bufferSize];

                int i, j, k;
                int xDim = image.getExtents()[0];
                int yDim = image.getExtents()[1];

                for (k = 0; k < nBuffers; k++) {
                    image.exportData(k * bufferSize, bufferSize, buffer);

                    for (j = 0; j < yDim; j++) {

                        for (i = 0; i < xDim; i++) {
                            resultBuffer[(j * xDim) + i] = buffer[((yDim - 1 - j) * xDim) + i];
                        }
                    }

                    image.importData(k * bufferSize, resultBuffer, false);
                }
            }
        } catch (IOException error) {
            throw new IOException("FileNIFTI.flipTopBottom: " + error);
        } catch (OutOfMemoryError error) {
            throw (error);
        }
    }

    /**
     * Flips image. NIFTI stores its data "upside down".
     *
     * @param   buffer    Buffer holding image to flip.
     * @param   fileInfo  File info structure for image to flip.
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public void flipTopBottom(float[] buffer, FileInfoNIFTI fileInfo) throws IOException {
        int nBuffers;
        int bufferSize;
        float[] resultBuffer = null;

        try {

            if ((fileInfo.getExtents().length - 1) > 1) {
                bufferSize = fileInfo.getExtents()[0] * fileInfo.getExtents()[1];
            } else {
                bufferSize = fileInfo.getExtents()[0];
            }

            if ((fileInfo.getExtents().length - 1) == 5) {
                nBuffers = fileInfo.getExtents()[4] * fileInfo.getExtents()[3] * fileInfo.getExtents()[2];

            } else if ((fileInfo.getExtents().length - 1) == 4) {
                nBuffers = fileInfo.getExtents()[3] * fileInfo.getExtents()[2];
            } else if ((fileInfo.getExtents().length - 1) == 3) {
                nBuffers = fileInfo.getExtents()[2];
            } else {
                nBuffers = 1;
            }

            if (fileInfo.getDataType() == ModelStorageBase.ARGB) {

                resultBuffer = new float[bufferSize * 4];
                bufferSize = bufferSize * 4;

                int i, j, k;
                int xDim = image.getExtents()[0] * 4;
                int yDim = image.getExtents()[1];

                for (k = 0; k < nBuffers; k++) {

                    for (j = 0; j < yDim; j++) {

                        for (i = 0; i < xDim; i += 4) {
                            resultBuffer[(j * xDim) + i] = 255;
                            resultBuffer[(k * bufferSize) + (j * xDim) + i + 1] = buffer[(k * bufferSize) +
                                                                                         ((yDim - 1 - j) * xDim) + i + 1];
                            resultBuffer[(k * bufferSize) + (j * xDim) + i + 2] = buffer[(k * bufferSize) +
                                                                                         ((yDim - 1 - j) * xDim) + i + 2];
                            resultBuffer[(k * bufferSize) + (j * xDim) + i + 3] = buffer[(k * bufferSize) +
                                                                                         ((yDim - 1 - j) * xDim) + i + 3];
                        }
                    }
                }
            } else {
                resultBuffer = new float[buffer.length];

                int i, j, k;
                int xDim = fileInfo.getExtents()[0];
                int yDim = fileInfo.getExtents()[1];

                for (k = 0; k < nBuffers; k++) {

                    for (j = 0; j < yDim; j++) {

                        for (i = 0; i < xDim; i++) {
                            resultBuffer[(k * bufferSize) + (j * xDim) + i] = buffer[(k * bufferSize) +
                                                                                     ((yDim - 1 - j) * xDim) + i];
                        }
                    }
                }
            }
        } catch (OutOfMemoryError error) {
            throw (error);
        }

        System.arraycopy(resultBuffer, 0, buffer, 0, buffer.length); // buffer = resultBuffer;
    }
    
    /**
     * Prepares this class for cleanup. Calls the <code>finalize</code> method for existing elements, closes any open
     * files and sets other elements to <code>null</code>.
     */
    public void finalize() {

        axisOrientation = null;
        axisOrientation2 = null;
        bufferByte = null;
        LPSOrigin = null;
        LPSOrigin2 = null;
        matrix = null;
        matrixTwoDim = null;
        matrix2 = null;
        origin = null;
        pixdim = null;
        resolutions = null;
        srow_x = null;
        srow_y = null;
        srow_z = null;
        intentName = null;
        fileDir = null;
        fileName = null;
        fileInfo = null;
        image = null;

        try {
            super.finalize();
        } catch (Throwable er) { }
    }

    /**
     * Returns the FileInfoNIFTI read from the file.
     *
     * @return  File info read from file, or null if it has not been read.
     */
    public FileInfoNIFTI getFileInfo() {
        return fileInfo;
    }
    
    /**
     * 
     * @return
     */
    public TransMatrix getMatrix() {
        return matrix;
    }
    
    
    public TransMatrix getMatrix2() {
        return matrix2;
    }
    

	/**
     * Reads the NIFTI header and stores the information in fileInfo.
     *
     * @param      imageFileName  File name of image.
     * @param      fileDir        Directory.
     * @param      niftiCompressed boolean indicating if file is a NIFTI compressed type
     * @return     Flag to confirm a successful read.
     *
     * @exception  IOException  if there is an error reading the header
     *
     * @see        FileInfoNIFTI
     */
    public boolean readHeader(String imageFileName, String fileDir, boolean niftiCompressed, boolean noReadPrivateTags) throws IOException {
        int i, j;
        int index;
        String fileHeaderName;
        long fileLength = 348;
        boolean endianess = BIG_ENDIAN;
        int[] niftiExtents = new int[5];
        int numDims = 0;
        float intentP1;
        float intentP2;
        float intentP3;
        short intentCode;
        int unitMeasure;
        int spatialDims;
        double a, b, c, d;
        boolean isQform = true;
        byte extension0 = 0;
        DecimalFormat nf;
        int extendedHeaderStart;
        int currentAddress;
        int ecodeNumber = 0;
        int mindIdentNumber = 0;
        int mindIdentIndex = 0;
        int bValueNumber = 0;
        int bValueIndex = 0;
        int sphericalDirectionNumber = 0;
        int sphericalDirectionIndex = 0;
        int dtComponentNumber = 0;
        int dtComponentIndex = 0;
        int dtComponents;
        int sphericalHarmonicNumber = 0;
        int sphericalHarmonicIndex = 0;
        int afniGroupNumber = 0;
        int afniGroupIndex = 0;
        int asciiTextNumber = 0;
        int asciiTextIndex = 0;
        int caretNumber = 0;
        int caretIndex = 0;
        int jsonNumber = 0;
        int jsonIndex = 0;

        bufferByte = new byte[headerSize];

        
        
        if(niftiCompressed) {
        	oneFile = true;
        	byte[] buffer = new byte[headerSize];
        	int bytesRead;
        	int dataStart;
        	byte[] buffer2 = null;
        	int bytesRead2 = 0;
        	if (fileName.endsWith("zip")) {
        		zin.getNextEntry();
        		 bytesRead = zin.read(buffer);
                 if(bytesRead != headerSize) {
                	 buffer = getFullBuffer(zin,buffer,bytesRead,headerSize); 
                 }
                 // Set the endianess based on header size = 348 Big Endian or 1,543,569,408 Little endian
     	        endianess = BIG_ENDIAN;
                 if (headerSize != (getBufferInt(buffer, 0, BIG_ENDIAN))) {
                	    endianess = LITTLE_ENDIAN;
                	    if (headerSize != getBufferInt(buffer, 0, LITTLE_ENDIAN)) {
                	    	Preferences.debug("FileNIFTI:readHeader NIFTI header length != 348.\n",
                	    			Preferences.DEBUG_FILEIO);
                         return false; 
                	    }
                 }
                 else {
                 	Preferences.debug("FileNIFTI:readHeader Endianess = Big endian.\n", Preferences.DEBUG_FILEIO);	
                 }
                 fileInfo.setEndianess(endianess);
                 fileInfo.setSizeOfHeader(headerSize);
                 vox_offset = getBufferFloat(buffer, 108, endianess);
                 fileInfo.setVoxOffset(vox_offset);
                 Preferences.debug("vox_offset = " + vox_offset + "\n", Preferences.DEBUG_FILEIO);
                 dataStart = Math.round(vox_offset);
                 if (dataStart > headerSize) {
                	 buffer2 = new byte[dataStart-headerSize];
                	 bytesRead2 = zin.read(buffer2);
                	 if (bytesRead2 != dataStart - headerSize) {
                		 buffer2 = getFullBuffer(zin, buffer2, bytesRead2, dataStart - headerSize);
                	 }
                 }
                 else {
                	 dataStart = headerSize;
                 }
                 bufferByte = new byte[dataStart];
                 for (i = 0; i < headerSize; i++) {
                     bufferByte[i] = buffer[i];
                 }
                 for (i = 0; i < dataStart-headerSize; i++) {
                	 bufferByte[i+headerSize] = buffer2[i];
                 }
        	}else if(fileName.endsWith("gz")) {
        		bytesRead = gzin.read(buffer);
                if(bytesRead != headerSize) {
               	 buffer = getFullBuffer(gzin,buffer,bytesRead,headerSize); 
                }
                // Set the endianess based on header size = 348 Big Endian or 1,543,569,408 Little endian
    	        endianess = BIG_ENDIAN;
                if (headerSize != (getBufferInt(buffer, 0, BIG_ENDIAN))) {
               	    endianess = LITTLE_ENDIAN;
               	    if (headerSize != getBufferInt(buffer, 0, LITTLE_ENDIAN)) {
               	    	Preferences.debug("FileNIFTI:readHeader NIFTI header length != 348.\n", 
               	    			Preferences.DEBUG_FILEIO);
                        return false; 
               	    }
                }
                else {
                	Preferences.debug("FileNIFTI:readHeader Endianess = Big endian.\n", 
                			Preferences.DEBUG_FILEIO);	
                }
                fileInfo.setEndianess(endianess);
                fileInfo.setSizeOfHeader(headerSize);
                vox_offset = getBufferFloat(buffer, 108, endianess);
                fileInfo.setVoxOffset(vox_offset);
                Preferences.debug("vox_offset = " + vox_offset + "\n", Preferences.DEBUG_FILEIO);
                dataStart = Math.round(vox_offset);
                if (dataStart > headerSize) {
               	 buffer2 = new byte[dataStart-headerSize];
               	 bytesRead2 = gzin.read(buffer2);
               	 if (bytesRead2 != dataStart - headerSize) {
               		 buffer2 = getFullBuffer(gzin, buffer2, bytesRead2, dataStart - headerSize);
               	 }
                }
                else {
               	 dataStart = headerSize;
                }
                bufferByte = new byte[dataStart];
                for (i = 0; i < headerSize; i++) {
                    bufferByte[i] = buffer[i];
                }
                for (i = 0; i < dataStart-headerSize; i++) {
               	 bufferByte[i+headerSize] = buffer2[i];
                }
        	}else if(fileName.endsWith("bz2")) {
        		bytesRead = bz2in.read(buffer);
                if(bytesRead != headerSize) {
               	 buffer = getFullBuffer(bz2in,buffer,bytesRead,headerSize); 
                }
                // Set the endianess based on header size = 348 Big Endian or 1,543,569,408 Little endian
    	        endianess = BIG_ENDIAN;
                if (headerSize != (getBufferInt(buffer, 0, BIG_ENDIAN))) {
               	    endianess = LITTLE_ENDIAN;
               	    if (headerSize != getBufferInt(buffer, 0, LITTLE_ENDIAN)) {
               	    	Preferences.debug("FileNIFTI:readHeader NIFTI header length != 348.\n", 
               	    			Preferences.DEBUG_FILEIO);
                        return false; 
               	    }
                }
                else {
                	Preferences.debug("FileNIFTI:readHeader Endianess = Big endian.\n", 
                			Preferences.DEBUG_FILEIO);	
                }
                fileInfo.setEndianess(endianess);
                fileInfo.setSizeOfHeader(headerSize);
                vox_offset = getBufferFloat(buffer, 108, endianess);
                fileInfo.setVoxOffset(vox_offset);
                Preferences.debug("vox_offset = " + vox_offset + "\n", Preferences.DEBUG_FILEIO);
                dataStart = Math.round(vox_offset);
                if (dataStart > headerSize) {
               	 buffer2 = new byte[dataStart-headerSize];
               	 bytesRead2 = bz2in.read(buffer2);
               	 if (bytesRead2 != dataStart - headerSize) {
               		 buffer2 = getFullBuffer(bz2in, buffer2, bytesRead2, dataStart - headerSize);
               	 }
                }
                else {
               	 dataStart = headerSize;
                }
                bufferByte = new byte[dataStart];
                for (i = 0; i < headerSize; i++) {
                    bufferByte[i] = buffer[i];
                }
                for (i = 0; i < dataStart-headerSize; i++) {
               	 bufferByte[i+headerSize] = buffer2[i];
                }
        	}
           
        }else {
        	// index         = fileName.toLowerCase().indexOf(".img");
            index = fileName.lastIndexOf(".");

            if (index == -1) {
                oneFile = false;
                fileHeaderName = fileName + ".hdr";
                fileHeader = new File(fileDir + fileHeaderName);

                if (fileHeader.exists() == false) {
                    fileHeaderName = fileName + ".HDR";
                    fileHeader = new File(fileDir + fileHeaderName);
                }

                if (fileHeader.exists() == false) {
                    return false;
                }

            } else {

                if (fileName.substring(index + 1).equalsIgnoreCase("nii")) {
                    oneFile = true;
                    fileHeaderName = fileName;
                } else {
                    oneFile = false;
                    fileHeaderName = fileName.substring(0, index) + ".hdr";
                }

                fileHeader = new File(fileDir + fileHeaderName);

                if (fileHeader.exists() == false) {
                    fileHeaderName = fileName.substring(0, index) + ".HDR";
                    fileHeader = new File(fileDir + fileHeaderName);

                    if (fileHeader.exists() == false) {
                        return false;
                    }
                }

            }

            //for multi-file
            if (fileInfo == null) { // if the file info does not yet exist: make it
            	fileInfo = new FileInfoNIFTI(imageFileName, fileDir, FileUtility.NIFTI);
            }
        


	        raFile = new RandomAccessFile(fileHeader, "r");
	        byte buffer[] = new byte[headerSize];
	        raFile.read(buffer);
	        // Set the endianess based on header size = 348 Big Endian or 1,543,569,408 Little endian
	        endianess = BIG_ENDIAN;
            if (headerSize != (getBufferInt(buffer, 0, BIG_ENDIAN))) {
           	    endianess = LITTLE_ENDIAN;
           	    if (headerSize != getBufferInt(buffer, 0, LITTLE_ENDIAN)) {
           	    	Preferences.debug("FileNIFTI:readHeader NIFTI header length != 348.\n", 
           	    			Preferences.DEBUG_FILEIO);
                    return false; 
           	    }
            }
            else {
            	Preferences.debug("FileNIFTI:readHeader Endianess = Big endian.\n", Preferences.DEBUG_FILEIO);	
            }
            fileInfo.setEndianess(endianess);
            fileInfo.setSizeOfHeader(headerSize);
            vox_offset = getBufferFloat(buffer, 108, endianess);
            fileInfo.setVoxOffset(vox_offset);
            Preferences.debug("vox_offset = " + vox_offset + "\n", Preferences.DEBUG_FILEIO);
            int dataStart = Math.round(vox_offset);
            if (dataStart < headerSize) {
            	dataStart = headerSize;
            }
            bufferByte = new byte[dataStart];
            raFile.seek(0L);
            raFile.read(bufferByte);
	        fileLength = raFile.length();
	        Preferences.debug("\nThe size of the file with the header information = " + fileLength + "\n",
	        		Preferences.DEBUG_FILEIO);
        
        }  

        // bufferByte[39] is the dim_info byte
        freq_dim = (int) (bufferByte[39] & 0x03);

        switch (freq_dim) {

            case 0:
                Preferences.debug("No frequency encoding direction is present\n", Preferences.DEBUG_FILEIO);
                break;

            case 1:
                Preferences.debug("Frequency encoding in the x direction\n", Preferences.DEBUG_FILEIO);
                break;

            case 2:
                Preferences.debug("Frequency encoding in the y direction\n", Preferences.DEBUG_FILEIO);
                break;

            case 3:
                Preferences.debug("Frequency encoding in the z direction\n", Preferences.DEBUG_FILEIO);
                break;
        }

        fileInfo.setFreqDim(freq_dim);
        phase_dim = (int) ((bufferByte[39] >> 2) & 0x03);

        switch (phase_dim) {

            case 0:
                Preferences.debug("No phase encoding direction is present\n", Preferences.DEBUG_FILEIO);
                break;

            case 1:
                Preferences.debug("Phase encoding in the x direction\n", Preferences.DEBUG_FILEIO);
                break;

            case 2:
                Preferences.debug("Phase encoding in the y direction\n", Preferences.DEBUG_FILEIO);
                break;

            case 3:
                Preferences.debug("Phase encoding in the z direction\n", Preferences.DEBUG_FILEIO);
                break;
        }

        fileInfo.setPhaseDim(phase_dim);
        slice_dim = (int) ((bufferByte[39] >> 4) & 0x03);

        switch (slice_dim) {

            case 0:
                Preferences.debug("No slice acquisition direction is present\n", Preferences.DEBUG_FILEIO);
                break;

            case 1:
                Preferences.debug("Slice acquisition in the x direction\n", Preferences.DEBUG_FILEIO);
                break;

            case 2:
                Preferences.debug("Slice acquisition in the y direction\n", Preferences.DEBUG_FILEIO);
                break;

            case 3:
                Preferences.debug("Slice acquisition in the z direction\n", Preferences.DEBUG_FILEIO);
                break;
        }

        fileInfo.setSliceDim(slice_dim);

        // In NIFTI always have x,y,z as dimensions 1, 2, and 3, t as dimension 4,
        // and any other dimensions as 5, 6, and 7
        // so that a x, y, t image would have dim[3] = 1
        int dims = getBufferShort(bufferByte, 40, endianess);
        Preferences.debug("FileNIFTI:readHeader. Number of dimensions = " + dims + "\n", Preferences.DEBUG_FILEIO);

        for (i = 0; i < dims; i++) {
            niftiExtents[i] = getBufferShort(bufferByte, 42 + (2 * i), endianess);
            Preferences.debug("FileNIFTI:readHeader. Dimension " + (i + 1) + " = " + niftiExtents[i] + "\n",
            		Preferences.DEBUG_FILEIO);

            if (niftiExtents[i] > 1) {
                numDims++;
            }
        }

        spatialDims = 0;

        for (i = 0; i < Math.min(dims, 3); i++) {

            if (niftiExtents[i] > 1) {
                spatialDims++;
            }
        }

        int[] extents = new int[numDims];

        for (i = 0, j = 0; i < dims; i++) {

            if (niftiExtents[i] > 1) {
                extents[j++] = niftiExtents[i];
            }
        }

        fileInfo.setExtents(extents);
        intentP1 = getBufferFloat(bufferByte, 56, endianess);
        fileInfo.setIntentP1(intentP1);
        Preferences.debug("FileNIFTI:readHeader. intentP1 = " + fileInfo.getIntentP1() + "\n", Preferences.DEBUG_FILEIO);
        intentP2 = getBufferFloat(bufferByte, 60, endianess);
        fileInfo.setIntentP2(intentP2);
        Preferences.debug("FileNIFTI:readHeader. statPar2 = " + fileInfo.getIntentP2() + "\n", Preferences.DEBUG_FILEIO);
        intentP3 = getBufferFloat(bufferByte, 64, endianess);
        fileInfo.setIntentP3(intentP3);
        Preferences.debug("FileNIFTI:readHeader. intentP3 = " + fileInfo.getIntentP3() + "\n", Preferences.DEBUG_FILEIO);
        intentCode = getBufferShort(bufferByte, 68, endianess);
        fileInfo.setIntentCode(intentCode);
        Preferences.debug("FileNIFTI:readHeader. intentCode = " + intentCode + "\n", Preferences.DEBUG_FILEIO);

        switch (intentCode) {

            case FileInfoNIFTI.NIFTI_INTENT_NONE:
                Preferences.debug("No intention\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_CORREL:
                Preferences.debug("Correlation coefficient R\n");
                Preferences.debug("Degrees of freedom = " + Math.round(intentP1) + "\n", Preferences.DEBUG_FILEIO);
                if ((dims == 5) && (extents[numDims - 1] == 2)) {
                    Preferences.debug("Dimension " + numDims + " has the Correlation Coefficient R\n",
                    		Preferences.DEBUG_FILEIO);
                    Preferences.debug("in the first data plane and degrees of freedom in the\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("second data plane\n", Preferences.DEBUG_FILEIO);
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_TTEST:
                Preferences.debug("Student t statistic\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Degress of freedom = " + Math.round(intentP1) + "\n", Preferences.DEBUG_FILEIO);
                if ((dims == 5) && (extents[numDims - 1] == 2)) {
                    Preferences.debug("Dimension " + numDims + " has the Student t statistic\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("in the first data plane and degrees of freedom in the\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("second data plane\n", Preferences.DEBUG_FILEIO);
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_FTEST:
                Preferences.debug("Fisher F statistic\n");
                Preferences.debug("Numerator degrees of freedom = " + Math.round(intentP1) + "\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Denominator degrees of freedom = " + Math.round(intentP2) + "\n",
                		Preferences.DEBUG_FILEIO);
                if ((dims == 5) && (extents[numDims - 1] == 3)) {
                    Preferences.debug("Dimension " + numDims + " has the Fisher F statistic\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("in the first data plane, numerator degrees of freedom in the\n", 
                    		Preferences.DEBUG_FILEIO);
                    Preferences.debug("second data plane, and denominator degrees of freedom in the\n",
                    		Preferences.DEBUG_FILEIO);
                    Preferences.debug("third data plane\n", Preferences.DEBUG_FILEIO);
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_ZSCORE:
                Preferences.debug("Standard normal - N(0,1) distributed\n", Preferences.DEBUG_FILEIO);
                break;

            case FileInfoNIFTI.NIFTI_INTENT_CHISQ:
                Preferences.debug("Chi - squared\n");
                Preferences.debug("Degrees of freedom = " + Math.round(intentP1) + "\n", Preferences.DEBUG_FILEIO);
                if ((dims == 5) && (extents[numDims - 1] == 2)) {
                    Preferences.debug("Dimension " + numDims + " has Chi-squared\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("in the first data plane and degrees of freedom in the\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("second data plane\n", Preferences.DEBUG_FILEIO);
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_BETA:
                Preferences.debug("Beta distribution\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("a parameter = " + intentP1 + "\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("b parameter = " + intentP2 + "\n", Preferences.DEBUG_FILEIO);
                if ((dims == 5) && (extents[numDims - 1] == 3)) {
                    Preferences.debug("Dimension " + numDims + " has the Beta distribution\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("in the first data plane, the a parameter in the\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("second data plane, and the b parameter in the third\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("third data plane\n", Preferences.DEBUG_FILEIO);
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_BINOM:
                Preferences.debug("Binomial distribution\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Number of trials = " + Math.round(intentP1) + "\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Probability per trial = " + intentP2 + "\n", Preferences.DEBUG_FILEIO);
                if ((dims == 5) && (extents[numDims - 1] == 3)) {
                    Preferences.debug("Dimension " + numDims + " has the Binomial distribution\n",
                    		Preferences.DEBUG_FILEIO);
                    Preferences.debug("in the first data plane, the number of trials in the\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("second data plane, and the probability per trial in the\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("third data plane\n", Preferences.DEBUG_FILEIO);
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_GAMMA:
                Preferences.debug("Gamma with PDF = x^(shape-1) * exp(-Scale*x)\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("for x >= 0\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Shape = " + intentP1 + "\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Scale = " + intentP2 + "\n", Preferences.DEBUG_FILEIO);
                if ((dims == 5) && (extents[numDims - 1] == 3)) {
                    Preferences.debug("Dimension " + numDims + " has Gamma\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("in the first data plane, shape in the\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("second data plane, and scale in the third\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("data plane\n", Preferences.DEBUG_FILEIO);
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_POISSON:
                Preferences.debug("Poisson distribution\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Mean = " + intentP1 + "\n", Preferences.DEBUG_FILEIO);
                if ((dims == 5) && (extents[numDims - 1] == 2)) {
                    Preferences.debug("Dimension " + numDims + " has the Poisson distribution\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("in the first data plane and the mean in the\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("second data plane\n", Preferences.DEBUG_FILEIO);
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_NORMAL:
                Preferences.debug("Normal distribution\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Mean = " + intentP1 + "\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Standard deviation = " + intentP2 + "\n", Preferences.DEBUG_FILEIO);
                if ((dims == 5) && (extents[numDims - 1] == 3)) {
                    Preferences.debug("Dimension " + numDims + " has the Normal distribution\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("in the first data plane, the mean in the\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("second data plane, and the standard deviation\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("in the third data plane\n", Preferences.DEBUG_FILEIO);
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_FTEST_NONC:
                Preferences.debug("Noncentral F statistic\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Numerator degrees of freedom = " + Math.round(intentP1) + "\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Denominator degrees of freedom = " + Math.round(intentP2) + "\n", 
                		Preferences.DEBUG_FILEIO);
                Preferences.debug("Numerator noncentrality parameter= " + intentP3 + "\n", Preferences.DEBUG_FILEIO);
                if ((dims == 5) && (extents[numDims - 1] == 4)) {
                    Preferences.debug("Dimension " + numDims + " has the Noncentral F statistic\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("in the first data plane, numerator degrees of freedom in the\n",
                    		Preferences.DEBUG_FILEIO);
                    Preferences.debug("second data plane, denominator degrees of freedom in the\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("third data plane, and the numerator noncentrality parameter\n",
                    		Preferences.DEBUG_FILEIO);
                    Preferences.debug("in the fourth data plane\n", Preferences.DEBUG_FILEIO);
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_CHISQ_NONC:
                Preferences.debug("Noncentral chi-squared statistic\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Degrees of freedom = " + Math.round(intentP1) + "\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Noncentrality parameter = " + intentP2 + "\n", Preferences.DEBUG_FILEIO);
                if ((dims == 5) && (extents[numDims - 1] == 3)) {
                    Preferences.debug("Dimension " + numDims + " has the Noncentral chi-squared\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("statistic in the first data plane, degrees of freedom in the\n",
                    		Preferences.DEBUG_FILEIO);
                    Preferences.debug("second data plane, and the noncentrality parameter in the\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("third data plane\n", Preferences.DEBUG_FILEIO);
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_LOGISTIC:
                Preferences.debug("Logistic distribution\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Location = " + intentP1 + "\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Scale = " + intentP2 + "\n", Preferences.DEBUG_FILEIO);
                if ((dims == 5) && (extents[numDims - 1] == 3)) {
                    Preferences.debug("Dimension " + numDims + " has the Logistic distribution\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("in the first data plane, location in the second\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("data plane, and scale in the third data plane\n", Preferences.DEBUG_FILEIO);
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_LAPLACE:
                Preferences.debug("Laplace distribution\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Location = " + intentP1 + "\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Scale = " + intentP2 + "\n", Preferences.DEBUG_FILEIO);
                if ((dims == 5) && (extents[numDims - 1] == 3)) {
                    Preferences.debug("Dimension " + numDims + " has the Laplace distribution\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("in the first data plane, location in the second\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("data plane, and scale in the third data plane\n", Preferences.DEBUG_FILEIO);
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_UNIFORM:
                Preferences.debug("Uniform distribution\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Start = " + intentP1 + "\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("End = " + intentP2 + "\n", Preferences.DEBUG_FILEIO);
                if ((dims == 5) && (extents[numDims - 1] == 3)) {
                    Preferences.debug("Dimension " + numDims + " has the Uniform distribution\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("in the first data plane, start in the second data\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("plane, and end in the third data plane\n", Preferences.DEBUG_FILEIO);
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_TTEST_NONC:
                Preferences.debug("Noncentral t statistic\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Degrees of freedom = " + Math.round(intentP1) + "\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Noncentrality parameter = " + intentP2 + "\n", Preferences.DEBUG_FILEIO);
                if ((dims == 5) && (extents[numDims - 1] == 3)) {
                    Preferences.debug("Dimension " + numDims + " has the Noncentral t statistic\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("in the first data plane, degrees of freedom in the\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("second data plane, and the noncentrality parameter in\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("third data plane\n", Preferences.DEBUG_FILEIO);
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_WEIBULL:
                Preferences.debug("Weibull distribution\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Location = " + intentP1 + "\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Scale = " + intentP2 + "\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Power = " + intentP3 + "\n", Preferences.DEBUG_FILEIO);
                if ((dims == 5) && (extents[numDims - 1] == 4)) {
                    Preferences.debug("Dimension " + numDims + " has the Weibull distribution\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("in the first data plane, location in the second\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("data plane, scale in the third data plane, and power\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("in the fourth data plane\n", Preferences.DEBUG_FILEIO);
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_CHI:
                Preferences.debug("Chi distribution\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Degrees of freedom = " + Math.round(intentP1) + "\n", Preferences.DEBUG_FILEIO);

                int p1 = Math.round(intentP1);
                if (p1 == 1) {
                    Preferences.debug("dof = 1 = half normal distribution\n", Preferences.DEBUG_FILEIO);
                } else if (p1 == 2) {
                    Preferences.debug("dof = 2 = Rayleigh distribution\n", Preferences.DEBUG_FILEIO);
                } else if (p1 == 3) {
                    Preferences.debug("dof = 3 = Maxwell-Boltzmann distribution\n", Preferences.DEBUG_FILEIO);
                }

                if ((dims == 5) && (extents[numDims - 1] == 2)) {
                    Preferences.debug("Dimension " + numDims + " has the Chi distribution\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("in the first data plane and degrees of freedom in the\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("second data plane\n", Preferences.DEBUG_FILEIO);
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_INVGAUSS:
                Preferences.debug("Inverse Gaussian\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Mu = " + intentP1 + "\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Lambda = " + intentP2 + "\n", Preferences.DEBUG_FILEIO);
                if ((dims == 5) && (extents[numDims - 1] == 3)) {
                    Preferences.debug("Dimension " + numDims + " has the Inverse Gaussian\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("in the first data plane, mu in the second data\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("plane, and lambda in the third data plane\n", Preferences.DEBUG_FILEIO);
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_EXTVAL:
                Preferences.debug("Extreme value type 1\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Location = " + intentP1 + "\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("Scale = " + intentP2 + "\n", Preferences.DEBUG_FILEIO);
                if ((dims == 5) && (extents[numDims - 1] == 3)) {
                    Preferences.debug("Dimension " + numDims + " has Extreme value type 1\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("in the first data plane, location in the second\n", Preferences.DEBUG_FILEIO);
                    Preferences.debug("data plane, and scale in the third data plane\n", Preferences.DEBUG_FILEIO);
                }
                break;

            case FileInfoNIFTI.NIFTI_INTENT_PVAL:
                Preferences.debug("Data is a p-value\n", Preferences.DEBUG_FILEIO);
                break;

            case FileInfoNIFTI.NIFTI_INTENT_LOGPVAL:
                Preferences.debug("Data is ln(p-value)\n", Preferences.DEBUG_FILEIO);
                break;

            case FileInfoNIFTI.NIFTI_INTENT_LOG10PVAL:
                Preferences.debug("Data is log10(p-value)\n", Preferences.DEBUG_FILEIO);
                break;

            case FileInfoNIFTI.NIFTI_INTENT_ESTIMATE:
                Preferences.debug("Each voxel is an estimate of some parameter\n", Preferences.DEBUG_FILEIO);
                break;

            case FileInfoNIFTI.NIFTI_INTENT_LABEL:
                Preferences.debug("Each voxel is an index into some set of labels\n", Preferences.DEBUG_FILEIO);
                break;

            case FileInfoNIFTI.NIFTI_INTENT_NEURONAME:
                Preferences.debug("Each voxel is an index into the NeuroNames label set\n", Preferences.DEBUG_FILEIO);
                break;

            case FileInfoNIFTI.NIFTI_INTENT_GENMATRIX:
                Preferences.debug("Each voxel has a M x N matrix\n", Preferences.DEBUG_FILEIO);
                break;

            case FileInfoNIFTI.NIFTI_INTENT_SYMMATRIX:
                Preferences.debug("Each voxel has a NxN symmetric matrix\n", Preferences.DEBUG_FILEIO);
                break;

            case FileInfoNIFTI.NIFTI_INTENT_DISPVECT:
                Preferences.debug("Each voxel has a displacement vector\n", Preferences.DEBUG_FILEIO);
                break;

            case FileInfoNIFTI.NIFTI_INTENT_VECTOR:
                Preferences.debug("Each voxel has a vector\n", Preferences.DEBUG_FILEIO);
                break;

            case FileInfoNIFTI.NIFTI_INTENT_POINTSET:
                Preferences.debug("Each voxel has a spatial coordinate\n", Preferences.DEBUG_FILEIO);
                break;

            case FileInfoNIFTI.NIFTI_INTENT_TRIANGLE:
                Preferences.debug("Each voxel has a triple of indexes\n", Preferences.DEBUG_FILEIO);
                break;

            case FileInfoNIFTI.NIFTI_INTENT_QUATERNION:
                Preferences.debug("Each voxel has a quarternion\n", Preferences.DEBUG_FILEIO);
                break;

            case FileInfoNIFTI.NIFTI_INTENT_DIMLESS:
                Preferences.debug("Each voxel is a dimensionless value\n", Preferences.DEBUG_FILEIO);
                break;

            default:
                Preferences.debug("intentCode = " + intentCode + " is not a recognized value\n", Preferences.DEBUG_FILEIO);
        }

        sourceType = getBufferShort(bufferByte, 70, endianess);
        fileInfo.setSourceType(sourceType);
        Preferences.debug("Original unscaled source data type:\n", Preferences.DEBUG_FILEIO);

        switch (sourceType) {

            case FileInfoNIFTI.DT_UNKNOWN:
                Preferences.debug("Unknown data type\n", Preferences.DEBUG_FILEIO);
                MipavUtil.displayError("Mipav cannot handle data type DT_UNKNOWN");

                return false;

            case FileInfoNIFTI.DT_BINARY:
                Preferences.debug("Binary data\n", Preferences.DEBUG_FILEIO);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_INT8:
                Preferences.debug("Signed byte data\n", Preferences.DEBUG_FILEIO);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_UINT8:
                Preferences.debug("Unsigned byte data\n", Preferences.DEBUG_FILEIO);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_INT16:
                Preferences.debug("Signed short data\n", Preferences.DEBUG_FILEIO);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_UINT16:
                Preferences.debug("Unsigned short data\n", Preferences.DEBUG_FILEIO);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_INT32:
                Preferences.debug("Signed integer data\n", Preferences.DEBUG_FILEIO);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_UINT32:
                Preferences.debug("Unsigned integer data\n", Preferences.DEBUG_FILEIO);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_INT64:
                Preferences.debug("Signed long data\n", Preferences.DEBUG_FILEIO);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_UINT64:
                Preferences.debug("Unsigned long data\n", Preferences.DEBUG_FILEIO);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_FLOAT32:
                Preferences.debug("32 bit float data\n", Preferences.DEBUG_FILEIO);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_FLOAT64:
                Preferences.debug("64 bit double data\n", Preferences.DEBUG_FILEIO);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_FLOAT128:
                Preferences.debug("128 bit float data\n");
                MipavUtil.displayError("MIPAV cannot handle 128 bit floating point data");

                return false;

            case FileInfoNIFTI.NIFTI_TYPE_RGB24:
                Preferences.debug("RGB 24 bit data\n", Preferences.DEBUG_FILEIO);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_COMPLEX64:
                Preferences.debug("64 bit complex data\n", Preferences.DEBUG_FILEIO);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_COMPLEX128:
                Preferences.debug("128 bit DCOMPLEX data\n", Preferences.DEBUG_FILEIO);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_COMPLEX256:
                Preferences.debug("256 bit complex data\n", Preferences.DEBUG_FILEIO);
                MipavUtil.displayError("MIPAV cannot handle 256 bit complex data");

                return false;

            default:
                Preferences.debug("Unknown datatype code = " + sourceType + "\n", Preferences.DEBUG_FILEIO);
                MipavUtil.displayError("Unknown datatype code = " + sourceType);

                return false;
        }

        sourceBitPix = getBufferShort(bufferByte, 72, endianess);
        fileInfo.setSourceBitPix(sourceBitPix);
        Preferences.debug("FileNIFTI:readHeader. source bits per pixel = " + sourceBitPix + "\n", Preferences.DEBUG_FILEIO);

        sliceStart = getBufferShort(bufferByte, 74, endianess);

        pixdim = new float[dims + 1];
        resolutions = new float[Math.max(3, numDims)];

        for (i = 0, j = 0; i < (dims + 1); i++) {
            pixdim[i] = getBufferFloat(bufferByte, 76 + (4 * i), endianess);

            if ((i >= 1) && (niftiExtents[i - 1] > 1)) {
                resolutions[j] = Math.abs(pixdim[i]);
                Preferences.debug("FileNIFTI:readHeader. Resolutions " + (j + 1) + " = " + resolutions[j] + "\n", 
                		Preferences.DEBUG_FILEIO);
                j++;
            }
        }

        fileInfo.setResolutions(resolutions);

        scl_slope = getBufferFloat(bufferByte, 112, endianess);
        fileInfo.setSclSlope(scl_slope);
        Preferences.debug("Data scaling slope = " + scl_slope + "\n", Preferences.DEBUG_FILEIO);
        scl_inter = getBufferFloat(bufferByte, 116, endianess);
        fileInfo.setSclInter(scl_inter);
        Preferences.debug("Data offset = " + scl_inter + "\n", Preferences.DEBUG_FILEIO);

        sliceEnd = getBufferShort(bufferByte, 120, endianess);

        sliceCode = bufferByte[122];

        if ((sliceCode > 0) && (sliceStart > 0)) {
            fileInfo.setSliceStart(sliceStart);
            Preferences.debug("Slice timing pattern starts with slice = " + (sliceStart + 1) + "\n", Preferences.DEBUG_FILEIO);
        }

        if ((sliceCode > 0) && (sliceEnd > sliceStart)) {
            fileInfo.setSliceEnd(sliceEnd);
            Preferences.debug("Slice timing pattern ends with slice = " + (sliceEnd + 1) + "\n", Preferences.DEBUG_FILEIO);
        }

        if (spatialDims == 0) {
            Preferences.debug("No x, y, or z dimensions are present\n", Preferences.DEBUG_FILEIO);
        } else {
            spaceUnits = (int) (bufferByte[123] & 0x07);

            switch (spaceUnits) {

                case FileInfoNIFTI.NIFTI_UNITS_UNKNOWN:
                    Preferences.debug("Spatial units are unknown\n", Preferences.DEBUG_FILEIO);
                    unitMeasure = Unit.UNKNOWN_MEASURE.getLegacyNum();
                    break;

                case FileInfoNIFTI.NIFTI_UNITS_METER:
                    Preferences.debug("Spatial units are meters\n", Preferences.DEBUG_FILEIO);
                    unitMeasure = Unit.METERS.getLegacyNum();
                    break;

                case FileInfoNIFTI.NIFTI_UNITS_MM:
                    Preferences.debug("Spatial units are millimeters\n", Preferences.DEBUG_FILEIO);
                    unitMeasure = Unit.MILLIMETERS.getLegacyNum();
                    break;

                case FileInfoNIFTI.NIFTI_UNITS_MICRON:
                    Preferences.debug("Spatial units are micrometers\n", Preferences.DEBUG_FILEIO);
                    unitMeasure = Unit.MICROMETERS.getLegacyNum();
                    break;

                default:
                    Preferences.debug("Spatial units are an illegal " + spaceUnits + "\n", Preferences.DEBUG_FILEIO);
                    unitMeasure = Unit.UNKNOWN_MEASURE.getLegacyNum();
                    break;
            }

            for (i = 0; i < spatialDims; i++) {
                fileInfo.setUnitsOfMeasure(unitMeasure, i);
            }
        }

        if ((dims >= 4) && (niftiExtents[3] > 1)) {
            timeUnits = bufferByte[123] & 0x38;

            switch (timeUnits) {

                case FileInfoNIFTI.NIFTI_UNITS_UNKNOWN:
                    Preferences.debug("Time units are unknown\n", Preferences.DEBUG_FILEIO);
                    unitMeasure = Unit.UNKNOWN_MEASURE.getLegacyNum();
                    break;

                case FileInfoNIFTI.NIFTI_UNITS_SEC:
                    Preferences.debug("Time units are seconds\n", Preferences.DEBUG_FILEIO);
                    unitMeasure = Unit.SECONDS.getLegacyNum();
                    break;

                case FileInfoNIFTI.NIFTI_UNITS_MSEC:
                    Preferences.debug("Time units are milliseconds\n", Preferences.DEBUG_FILEIO);
                    unitMeasure = Unit.MILLISEC.getLegacyNum();
                    break;

                case FileInfoNIFTI.NIFTI_UNITS_USEC:
                    Preferences.debug("Time units are microseconds\n", Preferences.DEBUG_FILEIO);
                    unitMeasure = Unit.MICROSEC.getLegacyNum();
                    break;

                case FileInfoNIFTI.NIFTI_UNITS_HZ:
                    Preferences.debug("Time units are hertz\n", Preferences.DEBUG_FILEIO);
                    unitMeasure = Unit.HZ.getLegacyNum();
                    break;

                case FileInfoNIFTI.NIFTI_UNITS_PPM:
                    Preferences.debug("Time units are parts per million\n", Preferences.DEBUG_FILEIO);
                    unitMeasure = Unit.PPM.getLegacyNum();
                    break;

                case FileInfoNIFTI.NIFTI_UNITS_RADS:
                    Preferences.debug("Time units are radians per second\n", Preferences.DEBUG_FILEIO);
                    unitMeasure = Unit.RADS.getLegacyNum();
                    break;

                default:
                    Preferences.debug("Time units are an illegal = " + timeUnits + "\n", Preferences.DEBUG_FILEIO);
                    unitMeasure = Unit.UNKNOWN_MEASURE.getLegacyNum();
            }

            fileInfo.setUnitsOfMeasure(unitMeasure, spatialDims);
        }

        fileInfo.setCalMax(getBufferFloat(bufferByte, 124, endianess));
        fileInfo.setCalMin(getBufferFloat(bufferByte, 128, endianess));
        sliceDuration = getBufferFloat(bufferByte, 132, endianess);

        if ((sliceDuration > 0) && (slice_dim > 0)) {
            fileInfo.setSliceDuration(sliceDuration);
            Preferences.debug("Time used to acquire 1 slice = " + sliceDuration + "\n", Preferences.DEBUG_FILEIO);
        }

        if ((sliceCode > 0) && (slice_dim > 0) && (sliceDuration > 0)) {

            if (sliceCode == FileInfoNIFTI.NIFTI_SLICE_SEQ_INC) {
                Preferences.debug("Slice timing order is sequentially increasing\n", Preferences.DEBUG_FILEIO);
            } else if (sliceCode == FileInfoNIFTI.NIFTI_SLICE_SEQ_DEC) {
                Preferences.debug("Slice timing order is sequentially decreasing\n", Preferences.DEBUG_FILEIO);
            } else if (sliceCode == FileInfoNIFTI.NIFTI_SLICE_ALT_INC) {
                Preferences.debug("Slice timing order is alternately increasing\n", Preferences.DEBUG_FILEIO);
            } else if (sliceCode == FileInfoNIFTI.NIFTI_SLICE_ALT_DEC) {
                Preferences.debug("Slice timing order is alternately decreasing\n", Preferences.DEBUG_FILEIO);
            } else if (sliceCode == FileInfoNIFTI.NIFTI_SLICE_ALT_INC2) {
                Preferences.debug("Slice timing order is alternately increasing #2\n", Preferences.DEBUG_FILEIO);
            } else if (sliceCode == FileInfoNIFTI.NIFTI_SLICE_ALT_DEC2) {
                Preferences.debug("Slice timing order is alternately decreasing #2\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("slice code has an illegal value = " + sliceCode + "\n", Preferences.DEBUG_FILEIO);
            }
        } else {
            Preferences.debug("Slice timing order is not specified\n", Preferences.DEBUG_FILEIO);
        }

        tOffset = getBufferFloat(bufferByte, 136, endianess);
        fileInfo.setOrigin(tOffset, 3);
        Preferences.debug("tOffset = " + tOffset + "\n", Preferences.DEBUG_FILEIO);


        switch (sourceType) {

            case FileInfoNIFTI.DT_NONE:
                return false;

            case FileInfoNIFTI.DT_BINARY:
                fileInfo.setDataType(ModelStorageBase.BOOLEAN);
                fileInfo.setBitPix((short) 1);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_UINT8:
                fileInfo.setDataType(ModelStorageBase.UBYTE);
                fileInfo.setBitPix((short) 8);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_INT16:
                fileInfo.setDataType(ModelStorageBase.SHORT);
                fileInfo.setBitPix((short) 16);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_INT32:
                fileInfo.setDataType(ModelStorageBase.INTEGER);
                fileInfo.setBitPix((short) 32);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_FLOAT32:
                fileInfo.setDataType(ModelStorageBase.FLOAT);
                fileInfo.setBitPix((short) 32);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_COMPLEX64:
                fileInfo.setDataType(ModelStorageBase.COMPLEX);
                fileInfo.setBitPix((short) 64);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_FLOAT64:
                fileInfo.setDataType(ModelStorageBase.DOUBLE);
                fileInfo.setBitPix((short) 64);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_RGB24:
                fileInfo.setDataType(ModelStorageBase.ARGB);
                fileInfo.setBitPix((short) 24);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_INT8:
                fileInfo.setDataType(ModelStorageBase.BYTE);
                fileInfo.setBitPix((short) 8);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_UINT16:
                fileInfo.setDataType(ModelStorageBase.USHORT);
                fileInfo.setBitPix((short) 16);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_UINT32:
                fileInfo.setDataType(ModelStorageBase.UINTEGER);
                fileInfo.setBitPix((short) 32);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_INT64:
                fileInfo.setDataType(ModelStorageBase.LONG);
                fileInfo.setBitPix((short) 64);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_UINT64:
                fileInfo.setDataType(ModelStorageBase.LONG);
                fileInfo.setBitPix((short) 64);
                break;

            case FileInfoNIFTI.NIFTI_TYPE_COMPLEX128:
                fileInfo.setDataType(ModelStorageBase.DCOMPLEX);
                fileInfo.setBitPix((short) 128);
                break;

            default:
                return false;
        }

        fileInfo.setDescription(new String(bufferByte, 148, 80));

        // update the fileInfo modality based on the description
        // if the description contains something other than modality, then
        // the modality will be set to unknown.
        fileInfo.setModality(FileInfoBase.getModalityFromStr(fileInfo.getDescription()));

        fileInfo.setAuxFile(new String(bufferByte, 228, 24));

        qform_code = getBufferShort(bufferByte, 252, endianess);
        sform_code = getBufferShort(bufferByte, 254, endianess);

        if (pixdim[0] >= 0.0f) {
            qfac = 1.0f;
        } else {
            qfac = -1.0f;
        }

        if ((qform_code == 0) && (sform_code == 0)) {

            // No particular spatial orientation is assigned
            fileInfo.setImageOrientation(FileInfoBase.UNKNOWN_ORIENT);
            axisOrientation = new int[3];
            axisOrientation[0] = FileInfoBase.ORI_UNKNOWN_TYPE;
            axisOrientation[1] = FileInfoBase.ORI_UNKNOWN_TYPE;
            axisOrientation[2] = FileInfoBase.ORI_UNKNOWN_TYPE;
            fileInfo.setAxisOrientation(axisOrientation);
            matrix.set(0, 0, resolutions[0]);
            matrix.set(1, 1, resolutions[1]);
            matrix.set(2, 2, resolutions[2]);
        }

        // Both methods 2 and 3 could be present
        // MIPAV can handle 2 different transformation matrices
        // for the same image.
        Preferences.debug("qform_code = " + qform_code + "\n", Preferences.DEBUG_FILEIO);
        Preferences.debug("sform_code = " + sform_code + "\n", Preferences.DEBUG_FILEIO);

        if (qform_code > 0) {
            coord_code = qform_code;
            isQform = true;
        } else if (sform_code > 0) {
            coord_code = sform_code;
            isQform = false;
        }

        fileInfo.setCoordCode(coord_code);
        
        if ((qform_code > 0) && (sform_code > 0)) {
            fileInfo.setCoordCode2(sform_code);
        }

        if (coord_code > 0) {

            matrix.setIsNIFTI(true);
            matrix.setIsQform(isQform);

            switch (coord_code) {

                case FileInfoNIFTI.NIFTI_XFORM_UNKNOWN:
                    Preferences.debug("Arbitrary X,Y,Z coordinate system\n", Preferences.DEBUG_FILEIO);
                    matrix.setTransformID(TransMatrix.TRANSFORM_UNKNOWN);
                    break;

                case FileInfoNIFTI.NIFTI_XFORM_SCANNER_ANAT:
                    Preferences.debug("Scanner based anatomical coordinates\n", Preferences.DEBUG_FILEIO);
                    matrix.setTransformID(TransMatrix.TRANSFORM_NIFTI_SCANNER_ANATOMICAL);
                    break;

                case FileInfoNIFTI.NIFTI_XFORM_ALIGNED_ANAT:
                    Preferences.debug("Coordinates aligned to another file's or to anatomical truth\n",
                    		Preferences.DEBUG_FILEIO);
                    matrix.setTransformID(TransMatrix.TRANSFORM_ANOTHER_DATASET);
                    break;

                case FileInfoNIFTI.NIFTI_XFORM_TALAIRACH:
                    Preferences.debug("Talairach X,Y,Z coordinate system\n", Preferences.DEBUG_FILEIO);
                    matrix.setTransformID(TransMatrix.TRANSFORM_TALAIRACH_TOURNOUX);
                    break;

                case FileInfoNIFTI.NIFTI_XFORM_MNI_152:
                    matrix.setTransformID(TransMatrix.TRANSFORM_MNI_152);
                    Preferences.debug("MNI 152 normalized X,Y,Z coordinates\n", Preferences.DEBUG_FILEIO);
                    break;

                default:
                    matrix.setTransformID(TransMatrix.TRANSFORM_UNKNOWN);
                    Preferences.debug("Unknown coord_code = " + coord_code + "\n", Preferences.DEBUG_FILEIO);
            }
        } // if (coord_code > 0)

        if ((qform_code > 0) && (sform_code > 0)) {
            matrix2 = new TransMatrix(4);
            matrix2.setIsNIFTI(true);
            matrix2.setIsQform(false);

            switch (sform_code) {

                case FileInfoNIFTI.NIFTI_XFORM_UNKNOWN:
                    Preferences.debug("Matrix 2 arbitrary X,Y,Z coordinate system\n", Preferences.DEBUG_FILEIO);
                    matrix2.setTransformID(TransMatrix.TRANSFORM_UNKNOWN);
                    break;

                case FileInfoNIFTI.NIFTI_XFORM_SCANNER_ANAT:
                    Preferences.debug("Matrix 2 scanner based anatomical coordinates\n", Preferences.DEBUG_FILEIO);
                    matrix2.setTransformID(TransMatrix.TRANSFORM_NIFTI_SCANNER_ANATOMICAL);
                    break;

                case FileInfoNIFTI.NIFTI_XFORM_ALIGNED_ANAT:
                    Preferences.debug("Matrix 2 coordinates aligned to another file's or to anatomical truth\n",
                    		Preferences.DEBUG_FILEIO);
                    matrix2.setTransformID(TransMatrix.TRANSFORM_ANOTHER_DATASET);
                    break;

                case FileInfoNIFTI.NIFTI_XFORM_TALAIRACH:
                    Preferences.debug("Matrix 2 Talairach X,Y,Z coordinate system\n", Preferences.DEBUG_FILEIO);
                    matrix2.setTransformID(TransMatrix.TRANSFORM_TALAIRACH_TOURNOUX);
                    break;

                case FileInfoNIFTI.NIFTI_XFORM_MNI_152:
                    matrix2.setTransformID(TransMatrix.TRANSFORM_MNI_152);
                    Preferences.debug("Matrix 2 MNI 152 normalized X,Y,Z coordinates\n", Preferences.DEBUG_FILEIO);
                    break;

                default:
                    matrix2.setTransformID(TransMatrix.TRANSFORM_UNKNOWN);
                    Preferences.debug("Unknown sform_code = " + sform_code + "\n", Preferences.DEBUG_FILEIO);
            }
        }

        if (qform_code > 0) {
            quatern_b = getBufferFloat(bufferByte, 256, endianess);
            b = quatern_b;
            quatern_c = getBufferFloat(bufferByte, 260, endianess);
            c = quatern_c;
            quatern_d = getBufferFloat(bufferByte, 264, endianess);
            d = quatern_d;
            a = 1.0 - (b * b) - (c * c) - (d * d);

            if (a < 1.0e-7) {

                // special case
                a = 1.0 / Math.sqrt((b * b) + (c * c) + (d * d));

                // normalize b,c,d vector;
                b *= a;
                c *= a;
                d *= a;
                a = 0.0;
            } else {
                a = Math.sqrt(a);
            }

            r00 = (a * a) + (b * b) - (c * c) - (d * d);
            matrix.set(0, 0, -r00 * resolutions[0]);
            r01 = 2.0 * ((b * c) - (a * d));
            matrix.set(0, 1, -r01 * resolutions[1]);
            r02 = 2.0 * ((b * d) + (a * c));
            matrix.set(0, 2, -r02 * qfac * resolutions[2]);
            r10 = 2.0 * ((b * c) + (a * d));
            matrix.set(1, 0, -r10 * resolutions[0]);
            r11 = (a * a) + (c * c) - (b * b) - (d * d);
            matrix.set(1, 1, -r11 * resolutions[1]);
            r12 = 2.0 * ((c * d) - (a * b));
            r20 = 2.0 * ((b * d) - (a * c));
            matrix.set(2, 0, r20 * resolutions[0]);
            r21 = 2.0 * ((c * d) + (a * b));
            matrix.set(2, 1, r21 * resolutions[1]);
            r22 = (a * a) + (d * d) - (c * c) - (b * b);
            matrix.set(2, 2, r22 * qfac * resolutions[2]);
            patientOrientationString = new String();
            nf = new DecimalFormat("##0.0000000");

            patientOrientationString = nf.format(-r00) + "\\" + nf.format(-r10) + "\\" + nf.format(r20) +
                        "\\" + nf.format(-r01) + "\\" + nf.format(-r11) + "\\" + nf.format(r21);
            fileInfo.setPatientOrientationString(patientOrientationString);
            matrix.set(1, 2, -r12 * qfac * resolutions[2]);
            qoffset_x = getBufferFloat(bufferByte, 268, endianess);
            qoffset_y = getBufferFloat(bufferByte, 272, endianess);
            qoffset_z = getBufferFloat(bufferByte, 276, endianess);
            LPSOrigin = new float[3];

            axisOrientation = getAxisOrientation(matrix);
            Preferences.debug("axisOrientation = " + axisOrientation[0] + "  " + axisOrientation[1] + "  " +
                              axisOrientation[2] + "\n", Preferences.DEBUG_FILEIO);
            fileInfo.setAxisOrientation(axisOrientation);

            for (j = 0; j < 3; j++) {

                if (axisOrientation[j] == FileInfoBase.ORI_R2L_TYPE) {
                    LPSOrigin[j] = -Math.abs(qoffset_x);
                } else if (axisOrientation[j] == FileInfoBase.ORI_L2R_TYPE) {
                    LPSOrigin[j] = Math.abs(qoffset_x);
                } else if (axisOrientation[j] == FileInfoBase.ORI_A2P_TYPE) {
                    LPSOrigin[j] = -Math.abs(qoffset_y);
                } else if (axisOrientation[j] == FileInfoBase.ORI_P2A_TYPE) {
                    LPSOrigin[j] = Math.abs(qoffset_y);
                } else if (axisOrientation[j] == FileInfoBase.ORI_I2S_TYPE) {
                    LPSOrigin[j] = -Math.abs(qoffset_z);
                } else if (axisOrientation[j] == FileInfoBase.ORI_S2I_TYPE) {
                    LPSOrigin[j] = Math.abs(qoffset_z);
                }
            }

            //fileInfo.setOrigin(LPSOrigin);
            fileInfo.setOrigin(LPSOrigin[0], 0);
            fileInfo.setOrigin(LPSOrigin[1], 1);
            fileInfo.setOrigin(LPSOrigin[2], 2);
            matrix.set(0, 3, (double) LPSOrigin[0]);
            matrix.set(1, 3, (double) LPSOrigin[1]);
            matrix.set(2, 3, (double) LPSOrigin[2]);

            if ((axisOrientation[2] == FileInfoBase.ORI_R2L_TYPE) ||
                    (axisOrientation[2] == FileInfoBase.ORI_L2R_TYPE)) {
                fileInfo.setImageOrientation(FileInfoBase.SAGITTAL);
            } else if ((axisOrientation[2] == FileInfoBase.ORI_A2P_TYPE) ||
                           (axisOrientation[2] == FileInfoBase.ORI_P2A_TYPE)) {
                fileInfo.setImageOrientation(FileInfoBase.CORONAL);
            } else {
                fileInfo.setImageOrientation(FileInfoBase.AXIAL);
            }

            Preferences.debug("matrix = \n" + matrix + "\n", Preferences.DEBUG_FILEIO);

            Preferences.debug("quatern_a = " + quatern_a + "\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("quatern_b = " + quatern_b + "\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("quatern_c = " + quatern_c + "\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("quatern_d = " + quatern_d + "\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("qoffset_x = " + qoffset_x + "\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("qoffset_y = " + qoffset_y + "\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("qoffset_z = " + qoffset_z + "\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("qfac = " + qfac + "\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("r00 = " + r00 + "\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("r01 = " + r01 + "\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("r02 = " + r02 + "\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("r10 = " + r10 + "\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("r11 = " + r11 + "\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("r12 = " + r12 + "\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("r20 = " + r20 + "\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("r21 = " + r21 + "\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("r22 = " + r22 + "\n", Preferences.DEBUG_FILEIO);
        } // if (qform_code > 0)
        else if (sform_code > 0) { // qform_code = 0, so only 1 matrix
            srow_x = new float[4];
            srow_y = new float[4];
            srow_z = new float[4];
            srow_x[0] = getBufferFloat(bufferByte, 280, endianess);
            srow_x[1] = getBufferFloat(bufferByte, 284, endianess);
            srow_x[2] = getBufferFloat(bufferByte, 288, endianess);
            srow_x[3] = getBufferFloat(bufferByte, 292, endianess);
            srow_y[0] = getBufferFloat(bufferByte, 296, endianess);
            srow_y[1] = getBufferFloat(bufferByte, 300, endianess);
            srow_y[2] = getBufferFloat(bufferByte, 304, endianess);
            srow_y[3] = getBufferFloat(bufferByte, 308, endianess);
            srow_z[0] = getBufferFloat(bufferByte, 312, endianess);
            srow_z[1] = getBufferFloat(bufferByte, 316, endianess);
            srow_z[2] = getBufferFloat(bufferByte, 320, endianess);
            srow_z[3] = getBufferFloat(bufferByte, 324, endianess);
            matrix.set(0, 0, (double) -srow_x[0]);
            matrix.set(0, 1, (double) -srow_x[1]);
            matrix.set(0, 2, (double) -srow_x[2]);
            matrix.set(1, 0, (double) -srow_y[0]);
            matrix.set(1, 1, (double) -srow_y[1]);
            matrix.set(1, 2, (double) -srow_y[2]);
            matrix.set(2, 0, (double) srow_z[0]);
            matrix.set(2, 1, (double) srow_z[1]);
            matrix.set(2, 2, (double) srow_z[2]);
            r00 = -matrix.get(0,0)/resolutions[0];
            r10 = -matrix.get(1,0)/resolutions[0];
            r20 = matrix.get(2,0)/resolutions[0];
            r01 = -matrix.get(0, 1)/resolutions[1];
            r11 = -matrix.get(1, 1)/resolutions[1];
            r21 = matrix.get(2,1)/resolutions[1];
            patientOrientationString = new String();
            nf = new DecimalFormat("##0.0000000");
          
            patientOrientationString = nf.format(-r00) + "\\" + nf.format(-r10) + "\\" + nf.format(r20) +
                        "\\" + nf.format(-r01) + "\\" + nf.format(-r11) + "\\" + nf.format(r21);
            fileInfo.setPatientOrientationString(patientOrientationString);
            
            LPSOrigin = new float[3];

            axisOrientation = getAxisOrientation(matrix);
            Preferences.debug("axisOrientation = " + axisOrientation[0] + "  " + axisOrientation[1] + "  " +
                              axisOrientation[2] + "\n", Preferences.DEBUG_FILEIO);
            fileInfo.setAxisOrientation(axisOrientation);

            for (j = 0; j < 3; j++) {

                if (axisOrientation[j] == FileInfoBase.ORI_R2L_TYPE) {
                    LPSOrigin[j] = -Math.abs(srow_x[3]);
                } else if (axisOrientation[j] == FileInfoBase.ORI_L2R_TYPE) {
                    LPSOrigin[j] = Math.abs(srow_x[3]);
                } else if (axisOrientation[j] == FileInfoBase.ORI_A2P_TYPE) {
                    LPSOrigin[j] = -Math.abs(srow_y[3]);
                } else if (axisOrientation[j] == FileInfoBase.ORI_P2A_TYPE) {
                    LPSOrigin[j] = Math.abs(srow_y[3]);
                } else if (axisOrientation[j] == FileInfoBase.ORI_I2S_TYPE) {
                    LPSOrigin[j] = -Math.abs(srow_z[3]);
                } else if (axisOrientation[j] == FileInfoBase.ORI_S2I_TYPE) {
                    LPSOrigin[j] = Math.abs(srow_z[3]);
                }
            }

            //fileInfo.setOrigin(LPSOrigin);
            fileInfo.setOrigin(LPSOrigin[0], 0);
            fileInfo.setOrigin(LPSOrigin[1], 1);
            fileInfo.setOrigin(LPSOrigin[2], 2);
            matrix.set(0, 3, (double) LPSOrigin[0]);
            matrix.set(1, 3, (double) LPSOrigin[1]);
            matrix.set(2, 3, (double) LPSOrigin[2]);

            if ((axisOrientation[2] == FileInfoBase.ORI_R2L_TYPE) ||
                    (axisOrientation[2] == FileInfoBase.ORI_L2R_TYPE)) {
                fileInfo.setImageOrientation(FileInfoBase.SAGITTAL);
            } else if ((axisOrientation[2] == FileInfoBase.ORI_A2P_TYPE) ||
                           (axisOrientation[2] == FileInfoBase.ORI_P2A_TYPE)) {
                fileInfo.setImageOrientation(FileInfoBase.CORONAL);
            } else {
                fileInfo.setImageOrientation(FileInfoBase.AXIAL);
            }

            Preferences.debug("matrix = \n" + matrix + "\n", Preferences.DEBUG_FILEIO);

            Preferences.debug("srow_x = " + srow_x[0] + "  " + srow_x[1] + "  " + srow_x[2] + "  " + srow_x[3] +
                              "\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("srow_y = " + srow_y[0] + "  " + srow_y[1] + "  " + srow_y[2] + "  " + srow_y[3] +
                              "\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("srow_z = " + srow_z[0] + "  " + srow_z[1] + "  " + srow_z[2] + "  " + srow_z[3] +
                              "\n", Preferences.DEBUG_FILEIO);
        } // else if (sform_code > 0)

        if (matrix2 != null) { // sform_code > 0 and 2 matrices
            srow_x = new float[4];
            srow_y = new float[4];
            srow_z = new float[4];
            srow_x[0] = getBufferFloat(bufferByte, 280, endianess);
            srow_x[1] = getBufferFloat(bufferByte, 284, endianess);
            srow_x[2] = getBufferFloat(bufferByte, 288, endianess);
            srow_x[3] = getBufferFloat(bufferByte, 292, endianess);
            srow_y[0] = getBufferFloat(bufferByte, 296, endianess);
            srow_y[1] = getBufferFloat(bufferByte, 300, endianess);
            srow_y[2] = getBufferFloat(bufferByte, 304, endianess);
            srow_y[3] = getBufferFloat(bufferByte, 308, endianess);
            srow_z[0] = getBufferFloat(bufferByte, 312, endianess);
            srow_z[1] = getBufferFloat(bufferByte, 316, endianess);
            srow_z[2] = getBufferFloat(bufferByte, 320, endianess);
            srow_z[3] = getBufferFloat(bufferByte, 324, endianess);
            matrix2.set(0, 0, (double) -srow_x[0]);
            matrix2.set(0, 1, (double) -srow_x[1]);
            matrix2.set(0, 2, (double) -srow_x[2]);
            matrix2.set(1, 0, (double) -srow_y[0]);
            matrix2.set(1, 1, (double) -srow_y[1]);
            matrix2.set(1, 2, (double) -srow_y[2]);
            matrix2.set(2, 0, (double) srow_z[0]);
            matrix2.set(2, 1, (double) srow_z[1]);
            matrix2.set(2, 2, (double) srow_z[2]);
            LPSOrigin2 = new float[3];

            axisOrientation2 = getAxisOrientation(matrix2);
            Preferences.debug("axisOrientation2 = " + axisOrientation2[0] + "  " + axisOrientation2[1] + "  " +
                              axisOrientation2[2] + "\n", Preferences.DEBUG_FILEIO);

            for (j = 0; j < 3; j++) {
                if (axisOrientation2[j] == FileInfoBase.ORI_R2L_TYPE) {
                    LPSOrigin2[j] = -Math.abs(srow_x[3]);
                } else if (axisOrientation2[j] == FileInfoBase.ORI_L2R_TYPE) {
                    LPSOrigin2[j] = Math.abs(srow_x[3]);
                } else if (axisOrientation2[j] == FileInfoBase.ORI_A2P_TYPE) {
                    LPSOrigin2[j] = -Math.abs(srow_y[3]);
                } else if (axisOrientation2[j] == FileInfoBase.ORI_P2A_TYPE) {
                    LPSOrigin2[j] = Math.abs(srow_y[3]);
                } else if (axisOrientation2[j] == FileInfoBase.ORI_I2S_TYPE) {
                    LPSOrigin2[j] = -Math.abs(srow_z[3]);
                } else if (axisOrientation2[j] == FileInfoBase.ORI_S2I_TYPE) {
                    LPSOrigin2[j] = Math.abs(srow_z[3]);
                }
            }

            matrix2.set(0, 3, (double) LPSOrigin2[0]);
            matrix2.set(1, 3, (double) LPSOrigin2[1]);
            matrix2.set(2, 3, (double) LPSOrigin2[2]);


            Preferences.debug("matrix2 = \n" + matrix2 + "\n", Preferences.DEBUG_FILEIO);

            Preferences.debug("srow_x = " + srow_x[0] + "  " + srow_x[1] + "  " + srow_x[2] + "  " + srow_x[3] +
                              "\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("srow_y = " + srow_y[0] + "  " + srow_y[1] + "  " + srow_y[2] + "  " + srow_y[3] +
                              "\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("srow_z = " + srow_z[0] + "  " + srow_z[1] + "  " + srow_z[2] + "  " + srow_z[3] +
                              "\n", Preferences.DEBUG_FILEIO);
        } // if (matrix2 != null)
        
        if (numDims == 2) {
            if ((axisOrientation[0] == FileInfoBase.ORI_L2R_TYPE) ||
                (axisOrientation[0] == FileInfoBase.ORI_P2A_TYPE) ||
                (axisOrientation[0] == FileInfoBase.ORI_S2I_TYPE)) {
                matrixTwoDim.set(0, 0, (double)-resolutions[0]);
            }
            else {
                matrixTwoDim.set(0, 0, (double)resolutions[0]);
            }
            if ((axisOrientation[1] == FileInfoBase.ORI_L2R_TYPE) ||
                (axisOrientation[1] == FileInfoBase.ORI_P2A_TYPE) ||
                (axisOrientation[1] == FileInfoBase.ORI_S2I_TYPE)) {
                matrixTwoDim.set(1, 1, (double)-resolutions[1]);
            }
            else {
                matrixTwoDim.set(1, 1, (double)resolutions[1]);
            }
            if (LPSOrigin != null) {
                matrixTwoDim.set(0, 2, (double)LPSOrigin[0]);
                matrixTwoDim.set(1, 2, (double)LPSOrigin[1]);
            }
        } // if (numDims == 2)
        
        if ((qform_code > 0) && (sform_code > 0)) {
            fileInfo.setMatrixQ(matrix);
            fileInfo.setMatrixS(matrix2);
        }
        else if (qform_code > 0) {
            fileInfo.setMatrixQ(matrix);
        }
        else if (sform_code > 0) {
            fileInfo.setMatrixS(matrix);
        }
        

        intentName = (new String(bufferByte, 328, 16));
        Preferences.debug("Name or meaning of data = " + intentName + "\n", Preferences.DEBUG_FILEIO);
        fileInfo.setIntentName(intentName.trim());
        if ((bufferByte.length > 348) && (vox_offset > 352)) {
            // 4 byte extension array is present with only the first byte extension[0] defined
            // If extension[0] is nonzero, it indicates that extended header information is
            // present in the bytes following the extension array.
            extension0 = bufferByte[348];
            Preferences.debug("First byte in extension array = " + extension0 + "\n", Preferences.DEBUG_FILEIO);
        }
        if (extension0 == 0) {
            Preferences.debug("No extended header information is present\n", Preferences.DEBUG_FILEIO);
        }
        else {
            Preferences.debug("This indicates a header extension follows the extension array\n", Preferences.DEBUG_FILEIO);
            // Read past the 3 unused bytes in the extension array
            // The size of the extended header in bytes including the 8 bytes for esize and ecode
            // esize must be a positive integral multiple of 16
            extendedHeaderStart = 352;
            currentAddress = extendedHeaderStart;
            esize = 8;
            while ((bufferByte.length >= currentAddress + esize) && ((!oneFile) || (vox_offset >= currentAddress + esize))) {
                esize = getBufferInt(bufferByte, currentAddress, endianess);
                ecode = getBufferInt(bufferByte, currentAddress+4, endianess);
                if (ecode == 0) {
                    String openBracket = new String(bufferByte, currentAddress+8, 1);
                    if (openBracket.equals("{")) {
                    	// Padded to 16 bytes, so could be a }, }<sp>, }<sp><sp>, or }<sp><sp><sp>, etc.
                        String closeBracket = new String(bufferByte, currentAddress+esize-17, 16);
                        if (closeBracket.trim().endsWith("}")) {
                        	jsonNumber++;
                        }
                    }
                }
                else if (ecode == 4) {
                	afniGroupNumber++;
                }
                else if (ecode == 6) {
                	asciiTextNumber++;
                }
                else if (ecode == 18) {
                	mindIdentNumber++;
                }
                else if (ecode == 20) {
                	bValueNumber++;
                }
                else if (ecode == 22) {
                	sphericalDirectionNumber++;
                }
                else if (ecode == 24) {
                	dtComponentNumber++;
                }
                else if (ecode == 26) {
                	sphericalHarmonicNumber++;
                }
                else if (ecode == 30) {
                	caretNumber++;
                }
                ecodeNumber++;
                currentAddress = currentAddress + esize;
            } // while ((fileLength >= currentAddress + 8) && ((!oneFile) || (vox_offset >= currentAddress + 8)))
            Preferences.debug("The number of header fields = " + ecodeNumber + "\n", Preferences.DEBUG_FILEIO);
            if (ecodeNumber >= 1) {
                esizeArray = new int[ecodeNumber];
                esizeArray[0] = 8;
                ecodeArray = new int[ecodeNumber];
                mindIdentArray = new String[mindIdentNumber];
                bValueArray = new float[bValueNumber];
                azimuthArray = new float[sphericalDirectionNumber];
                zenithArray = new float[sphericalDirectionNumber];
                dtComponentArray = new int[dtComponentNumber][];
                degreeArray = new int[sphericalHarmonicNumber];
                orderArray = new int[sphericalHarmonicNumber];
                afniGroupArray = new String[afniGroupNumber];
                asciiTextArray = new String[asciiTextNumber];
                caretArray = new String[caretNumber];
                jsonArray = new String[jsonNumber];
                currentAddress = extendedHeaderStart;
                ecodeNumber = 0;
                while ((bufferByte.length >= currentAddress + esizeArray[Math.max(0, ecodeNumber-1)]) && ((!oneFile) || (vox_offset >= currentAddress + esizeArray[Math.max(0, ecodeNumber-1)]))) {
                    esizeArray[ecodeNumber] = getBufferInt(bufferByte, currentAddress, endianess);
                    Preferences.debug("Header field number " + (ecodeNumber+1) + " size in bytes = " + esizeArray[ecodeNumber]
                                      + "\n", Preferences.DEBUG_FILEIO);
                    ecodeArray[ecodeNumber] = getBufferInt(bufferByte, currentAddress+4, endianess);
                    Preferences.debug("Header field number " + (ecodeNumber+1) + " has ", Preferences.DEBUG_FILEIO);
                    switch(ecodeArray[ecodeNumber]) {
                    case 0:
                    	if (jsonNumber == 0) {
                    	     Preferences.debug("ecode = 0 for an unknown private format\n", Preferences.DEBUG_FILEIO);
                    	}
                    	else {
                    		Preferences.debug("ecode = 0 used for DcmMeta encoded with JSON\n", Preferences.DEBUG_FILEIO);
                    		jsonArray[jsonIndex] = new String(bufferByte, currentAddress+8, esizeArray[ecodeNumber]-8);
                    		jsonIndex++;
                    	}
                    	break;
                    case 2:
                    	Preferences.debug("ecode = 2 for DICOM format (i.e., attribute tags and values)\n",
                    			Preferences.DEBUG_FILEIO);
                    	break;
                    case 4:
                    	Preferences.debug("ecode = 4 for AFNI group (i.e., ASCII XML-ish elements)\n",
                    			Preferences.DEBUG_FILEIO);
                    	afniGroupArray[afniGroupIndex] = new String(bufferByte, currentAddress+8, esizeArray[ecodeNumber]-8);
                    	afniGroupIndex++;
                    	break;
                    case 6:
                    	Preferences.debug("ecode = 6 for comment: arbitrary non-NUL ASCII text\n", Preferences.DEBUG_FILEIO);
                    	asciiTextArray[asciiTextIndex] = new String(bufferByte, currentAddress+8, esizeArray[ecodeNumber]-8);
                    	asciiTextIndex++;
                    	break;
                    case 8:
                    	Preferences.debug("ecode = 8 for XCEDE metadata\n", Preferences.DEBUG_FILEIO);
                    	break;
                    case 10:
                        Preferences.debug("ecode = 10 for dimensional information for JIM software(XML format)\n",
                        		Preferences.DEBUG_FILEIO);
                        break;
                    case 12:
                    	Preferences.debug("ecode = 12 for Fiswidget XML pipeline descriptions\n", Preferences.DEBUG_FILEIO);
                    	break;
                    case 18:
                    	Preferences.debug("ecode = 18 for MIND_IDENT field with character data\n", Preferences.DEBUG_FILEIO);
                    	mindIdentArray[mindIdentIndex] = new String(bufferByte, currentAddress+8, esizeArray[ecodeNumber]-8);
                    	Preferences.debug("Mind Ident field = " + mindIdentArray[mindIdentIndex] + "\n",
                    			Preferences.DEBUG_FILEIO);
                    	mindIdentIndex++;
                    	break;
                    case 20:
                        Preferences.debug("ecode = 20 for B_VALUE for b-value in units of s/mm-squared\n",
                        		Preferences.DEBUG_FILEIO);
                        bValueArray[bValueIndex] = getBufferFloat(bufferByte, currentAddress+8, endianess);
                        Preferences.debug("b-value = " + bValueArray[bValueIndex] + " s/(mm*mm)\n", Preferences.DEBUG_FILEIO);
                        bValueIndex++;
                        break;
                    case 22:
                    	Preferences.debug("ecode = 22 for SPHERICAL_DIRECTION with spherical coordinates\n",
                    			Preferences.DEBUG_FILEIO);
                    	azimuthArray[sphericalDirectionIndex] = getBufferFloat(bufferByte, currentAddress+8, endianess);
                    	Preferences.debug("Azimuthal angle = " + azimuthArray[sphericalDirectionIndex] + " radians\n",
                    			Preferences.DEBUG_FILEIO);
                    	zenithArray[sphericalDirectionIndex] = getBufferFloat(bufferByte, currentAddress+12, endianess);
                    	Preferences.debug("Zenith angle = " + zenithArray[sphericalDirectionIndex] + " radians\n",
                    			Preferences.DEBUG_FILEIO);
                    	sphericalDirectionIndex++;
                    	break;
                    case 24:
                    	Preferences.debug("ecode = 24 for DT_COMPONENT specifying the indicies of a single diffusion tensor component\n",
                    			Preferences.DEBUG_FILEIO);
                    	dtComponents = (esizeArray[ecodeNumber] - 8)/4;
                    	dtComponentArray[dtComponentIndex] = new int[dtComponents];
                    	for (i = 0; i < dtComponents; i++) {
                    		dtComponentArray[dtComponentIndex][i] = getBufferInt(bufferByte, currentAddress + 8 + 4*i, endianess);
                    		Preferences.debug("DT component index " + (i+1) + " = " + dtComponentArray[dtComponentIndex][i]
                    		                  + "\n", Preferences.DEBUG_FILEIO);
                    	}
                    	dtComponentIndex++;
                    	break;
                    case 26:
                    	Preferences.debug("ecode = 26 for SHC_DEGREEORDER specifying degree and order of a spherical harmonic basis function\n",
                    			Preferences.DEBUG_FILEIO);
                    	degreeArray[sphericalHarmonicIndex] = getBufferInt(bufferByte, currentAddress+8, endianess);
                    	Preferences.debug("Degree = " + degreeArray[sphericalHarmonicIndex] + "\n", Preferences.DEBUG_FILEIO);
                    	orderArray[sphericalHarmonicIndex] = getBufferInt(bufferByte, currentAddress+12, endianess);
                    	Preferences.debug("Order = " + orderArray[sphericalHarmonicIndex] + "\n", Preferences.DEBUG_FILEIO);
                    	sphericalHarmonicIndex++;
                    	break;
                    case 30:
                    	Preferences.debug("ecode = 30 for CARET an XML extension\n", Preferences.DEBUG_FILEIO);
                    	caretArray[caretIndex] = new String(bufferByte, currentAddress+8, esizeArray[ecodeNumber]-8);
                    	Preferences.debug("Caret field = " + caretArray[caretIndex] + "\n", Preferences.DEBUG_FILEIO);
                    	caretIndex++;
                    	break;
                    default:
                        Preferences.debug("ecode = " + ecodeArray[ecodeNumber] + " an unspecified ecode value\n",
                        		Preferences.DEBUG_FILEIO);
                    } // switch(ecodeArray[ecodeNumber])
                    currentAddress = currentAddress + esizeArray[ecodeNumber];
                    ecodeNumber++;
                } // while ((fileLength >= currentAddress + 8) && ((!oneFile) || (vox_offset >= currentAddress + 8)))
                fileInfo.setEsize(esizeArray);
                fileInfo.setEcode(ecodeArray);
                fileInfo.setMindIdent(mindIdentArray);
                fileInfo.setBValue(bValueArray);
                fileInfo.setAzimuth(azimuthArray);
                fileInfo.setZenith(zenithArray);
                fileInfo.setDTComponent(dtComponentArray);
                fileInfo.setDegree(degreeArray);
                fileInfo.setOrder(orderArray);
                fileInfo.setAfniGroup(afniGroupArray);
                fileInfo.setAsciiText(asciiTextArray);
                fileInfo.setCaret(caretArray);
                if (jsonNumber >= 1) {
                	fileInfo.setHaveDcmMeta(true);
                	processJson(jsonArray[0], noReadPrivateTags);
                }
            } // if (ecodeNumber >= 1)
        } // else 
        

        
        
        if(raFile != null) {
        	raFile.close();
        }
        return true; // If it got this far, it has successfully read in the header
    }
    
    private void processJson(String json, boolean noReadPrivateTags) {
    	JSONObject jsonObject = null;
    	JSONObject global = null;
    	JSONObject global_const = null;
    	String specificCharacterSet = null;
    	JSONArray imageType = null;
    	String imageTypeString[] = null;
    	int i;
    	int j;
    	String studyTime = null;
    	String seriesTime = null;
    	String accessionNumber = null;
    	String modalityString = null;
    	String manufacturer = null;
    	String manufacturerModelName = null;
    	String scanningSequence = null;
    	String sequenceVariant = null;
    	String scanOptions = null;
    	String MRAcquisitionType = null;
    	String sequenceName = null;
    	String angioFlag = null;
    	float sliceThickness = Float.NaN;
    	double repetitionTime = Double.NaN;
    	double echoTime = Double.NaN;
    	double numberOfAverages = Double.NaN;
    	double imagingFrequency = Double.NaN;
    	String imagedNucleus = null;
    	int echoNumbers = Integer.MIN_VALUE;
    	double magneticFieldStrength = Double.NaN;
    	double spacingBetweenSlices = Double.NaN;
    	int numberOfPhaseEncodingSteps = Integer.MIN_VALUE;
    	int echoTrainLength = Integer.MIN_VALUE;
    	double percentSampling = Double.NaN;
    	double percentPhaseFieldOfView = Double.NaN;
    	double pixelBandwidth = Double.NaN;
    	String softwareVersions = null;
    	String transmitCoilName = null;
    	JSONArray acquisitionMatrix = null;
    	int acquisitionMatrixInt[] = null;
    	String inPlanePhaseEncodingDirection = null;
    	double flipAngle = Double.NaN;
    	String variableFlipAngleFlag = null;
    	double SAR = Double.NaN;
    	double dBdt = Double.NaN;
    	int seriesNumber = Integer.MIN_VALUE;
    	JSONArray imagePositionPatient = null;
    	double imagePositionPatientDouble[] = null;
    	JSONArray imageOrientationPatient = null;
    	double imageOrientationPatientDouble[] = null;
    	double sliceLocation = Double.NaN;
    	int samplesPerPixel = Integer.MIN_VALUE;
    	String photometricInterpretation = null;
    	int rows = Integer.MIN_VALUE;
    	int columns = Integer.MIN_VALUE;
    	JSONArray pixelSpacing = null;
    	double pixelSpacingDouble[] = null;
    	int bitsAllocated = Integer.MIN_VALUE;
    	int bitsStored = Integer.MIN_VALUE;
    	int highBit = Integer.MIN_VALUE;
    	int pixelRepresentation = Integer.MIN_VALUE;
    	int smallestImagePixelValue = Integer.MAX_VALUE;
    	int largestImagePixelValue = Integer.MIN_VALUE;
    	String windowCenterWidthExplanation = null;
    	String performedProcedureStepStartTime = null;
    	
    	try {
    	    jsonObject = new JSONObject(json);
    	}
    	catch (JSONException e) {
    		MipavUtil.displayError("JSONException " + e + " on new JSONObject(json)");
    	}
    	try {
    		global = jsonObject.getJSONObject("global");
    	}
    	catch (JSONException e) {
    	    Preferences.debug("global not found\n", Preferences.DEBUG_FILEIO);	
    	}
    	if (global != null) {
    	    try {
    	    	global_const = global.getJSONObject("const");
    	    }
    	    catch (JSONException e) {
    	        Preferences.debug("const not found\n", Preferences.DEBUG_FILEIO);	
    	    }
    	    if (global_const != null) {
    	    	try {
    	    		specificCharacterSet = global_const.getString("SpecificCharacterSet");
    	    	}
    	    	catch (JSONException e) {
    	    		Preferences.debug("SpecificCharacterSet not found\n", Preferences.DEBUG_FILEIO);
    	    	}
    	    	if (specificCharacterSet != null) {
    	    		fileInfo.setSpecificCharacterSet(specificCharacterSet);
    	    	}
    	    	try {
    	    		imageType = global_const.getJSONArray("ImageType");
    	    	}
    	    	catch(JSONException e) {
    	    		Preferences.debug("JSONArray ImageType not found\n", Preferences.DEBUG_FILEIO);
    	    	}
    	    	if (imageType != null) {
    	    		imageTypeString = new String[imageType.length()];
    	    		for (i = 0; i < imageType.length(); i++) {
    	    			try {
    	    			    imageTypeString[i] = imageType.getString(i);
    	    			}
    	    			catch (JSONException e) {
    	    				Preferences.debug("imageType.getString("+i+") not found\n", Preferences.DEBUG_FILEIO);
    	    			}
    	    		}
    	    		fileInfo.setImageType(imageTypeString);
    	    	}
    	        try {
    	        	studyTime = global_const.getString("StudyTime");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("StudyTime not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (studyTime != null) {
    	        	fileInfo.setStudyTime(studyTime);
    	        }
    	        try {
    	        	seriesTime = global_const.getString("SeriesTime");
    	        }
    	        catch(JSONException e) {
    	        	Preferences.debug("SeriesTime not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (seriesTime != null) {
    	        	fileInfo.setSeriesTime(seriesTime);
    	        }
    	        try {
    	        	accessionNumber = global_const.getString("AccessionNumber");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("AccessionNumber not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (accessionNumber != null) {
    	        	fileInfo.setAccessionNumber(accessionNumber);
    	        }
    	        try {
    	        	modalityString = global_const.getString("Modality");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("Modality not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (modalityString != null) {
    	        	fileInfo.setModalityString(modalityString);
    	        }
    	        try {
    	        	manufacturer = global_const.getString("Manufacturer");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("Manufacturer not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (manufacturer != null) {
    	        	fileInfo.setManufacturer(manufacturer);
    	        }
    	        try {
    	        	manufacturerModelName = global_const.getString("ManufacturerModelName");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("ManufacturerModelName not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (manufacturerModelName != null) {
    	        	fileInfo.setManufacturerModelName(manufacturerModelName);
    	        }
    	        try {
    	        	scanningSequence = global_const.getString("ScanningSequence");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("ScanningSequence not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (scanningSequence != null) {
    	        	fileInfo.setScanningSequence(scanningSequence);
    	        }
    	        try {
    	        	sequenceVariant = global_const.getString("SequenceVariant");
    	        }
    	        catch(JSONException e) {
    	        	Preferences.debug("SequenceVariant not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (sequenceVariant != null) {
    	        	fileInfo.setSequenceVariant(sequenceVariant);
    	        }
    	        try {
    	        	scanOptions = global_const.getString("ScanOptions");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("ScanOptions not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (scanOptions != null) {
    	        	fileInfo.setScanOptions(scanOptions);
    	        }
    	        try {
    	        	MRAcquisitionType = global_const.getString("MRAcquisitionType");
    	        }
    	        catch(JSONException e) {
    	        	Preferences.debug("MRAcquisitionType not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (MRAcquisitionType != null) {
    	        	fileInfo.setMRAcquisitionType(MRAcquisitionType);
    	        }
    	        try {
    	        	sequenceName = global_const.getString("SequenceName");
    	        }
    	        catch(JSONException e) {
    	        	Preferences.debug("SequenceName not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (sequenceName != null) {
    	        	fileInfo.setSequenceName(sequenceName);
    	        }
    	        try {
    	        	angioFlag = global_const.getString("AngioFlag");
    	        }
    	        catch(JSONException e) {
    	        	Preferences.debug("AngioFlag not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (angioFlag != null) {
    	        	fileInfo.setAngioFlag(angioFlag);
    	        }
    	        try {
    	        	sliceThickness = (float)global_const.getDouble("SliceThickness");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("SliceThickness not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (!Float.isNaN(sliceThickness)) {
    	        	fileInfo.setSliceThickness(sliceThickness);
    	        }
    	        try {
    	        	repetitionTime = global_const.getDouble("RepetitionTime");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("RepetitionTime not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (!Double.isNaN(repetitionTime)) {
    	        	fileInfo.setRepetitionTime(repetitionTime);
    	        }
    	        try {
    	        	echoTime = global_const.getDouble("EchoTime");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("EchoTime not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (!Double.isNaN(echoTime)) {
    	        	fileInfo.setEchoTime(echoTime);
    	        }
    	        try {
    	        	numberOfAverages = global_const.getDouble("NumberOfAverages");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("NumberOfAverages  not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (!Double.isNaN(numberOfAverages)) {
    	        	fileInfo.setNumberOfAverages(numberOfAverages);
    	        }
    	        try {
    	        	imagingFrequency = global_const.getDouble("ImagingFrequency");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("ImagingFrequency not found\n");
    	        }
    	        if (!Double.isNaN(imagingFrequency)) {
    	        	fileInfo.setImagingFrequency(imagingFrequency);
    	        }
    	        try {
    	        	imagedNucleus = global_const.getString("ImagedNucleus");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("ImagedNucleus not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (imagedNucleus != null) {
    	        	fileInfo.setImagedNucleus(imagedNucleus);
    	        }
    	        try {
    	        	echoNumbers = global_const.getInt("EchoNumbers");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("EchoNumbers not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (echoNumbers != Integer.MIN_VALUE) {
    	        	fileInfo.setEchoNumbers(echoNumbers);
    	        }
    	        try {
    	            magneticFieldStrength = global_const.getDouble("MagneticFieldStrength");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("MagneticFieldStrength not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (!Double.isNaN(magneticFieldStrength)) {
    	        	fileInfo.setMagneticFieldStrength(magneticFieldStrength);
    	        }
    	        try {
    	        	spacingBetweenSlices = global_const.getDouble("SpacingBetweenSlices");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("SpacingBetweenSlices not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (!Double.isNaN(spacingBetweenSlices)) {
    	        	fileInfo.setSpacingBetweenSlices(spacingBetweenSlices);
    	        }
    	        try {
    	        	numberOfPhaseEncodingSteps = global_const.getInt("NumberOfPhaseEncodingSteps");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("NumberOfPhaseEncodingSteps not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (numberOfPhaseEncodingSteps != Integer.MIN_VALUE) {
    	        	fileInfo.setNumberOfPhaseEncodingSteps(numberOfPhaseEncodingSteps);
    	        }
    	        try {
    	        	echoTrainLength = global_const.getInt("EchoTrainLength");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("EchoTrainLength not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (echoTrainLength != Integer.MIN_VALUE) {
    	        	fileInfo.setEchoTrainLength(echoTrainLength);
    	        }
    	        try {
    	        	percentSampling = global_const.getDouble("PercentSampling");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("PercentSampling not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (!Double.isNaN(percentSampling)) {
    	        	fileInfo.setPercentSampling(percentSampling);
    	        }
    	        try {
    	        	percentPhaseFieldOfView = global_const.getDouble("PercentPhaseFieldOfView");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("PercentPhaseFieldOfView not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (!Double.isNaN(percentPhaseFieldOfView)) {
    	        	fileInfo.setPercentPhaseFieldOfView(percentPhaseFieldOfView);
    	        }
    	        try {
    	        	pixelBandwidth = global_const.getDouble("PixelBandwidth");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("PixelBandwidth not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (!Double.isNaN(pixelBandwidth)) {
    	        	fileInfo.setPixelBandwidth(pixelBandwidth);
    	        }
    	        try {
    	        	softwareVersions = global_const.getString("SoftwareVersions");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("SoftwareVersions not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (softwareVersions != null) {
    	        	fileInfo.setSoftwareVersions(softwareVersions);
    	        }
    	        try {
    	        	transmitCoilName = global_const.getString("TransmitCoilName");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("TransmitCoilName not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (transmitCoilName != null) {
    	        	fileInfo.setTransmitCoilName(transmitCoilName);
    	        }
    	        try {
    	        	acquisitionMatrix = global_const.getJSONArray("AcquisitionMatrix");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("JSONArray AcquisitionMatrix not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (acquisitionMatrix != null) {
    	            acquisitionMatrixInt = new int[acquisitionMatrix.length()];
    	            for (i = 0; i < acquisitionMatrix.length(); i++) {
    	            	try {
    	            		acquisitionMatrixInt[i] = acquisitionMatrix.getInt(i);
    	            	}
    	            	catch (JSONException e) {
    	            		Preferences.debug("acquisitionMatrix.getInt("+i+") not found\n", Preferences.DEBUG_FILEIO);
    	            	}
    	            }
    	            if (acquisitionMatrixInt != null) {
    	            	fileInfo.setAcquisitionMatrix(acquisitionMatrixInt);
    	            }
    	        } // if (acquisitionMatrix != null) 
    	        try {
    	        	inPlanePhaseEncodingDirection = global_const.getString("InPlanePhaseEncodingDirection");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("InPlanePhaseEncodingDirection not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (inPlanePhaseEncodingDirection != null) {
    	        	fileInfo.setInPlanePhaseEncodingDirection(inPlanePhaseEncodingDirection);
    	        }
    	        try {
    	        	flipAngle = global_const.getDouble("FlipAngle");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("FlipAngle not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (!Double.isNaN(flipAngle)) {
    	        	fileInfo.setFlipAngle(flipAngle);
    	        }
    	        try {
    	        	variableFlipAngleFlag = global_const.getString("VariableFlipAngleFlag");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("VariableFlipAngleFlag not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (variableFlipAngleFlag !=  null) {
    	        	fileInfo.setVariableFlipAngleFlag(variableFlipAngleFlag);
    	        }
    	        try {
    	        	SAR = global_const.getDouble("SAR");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("SAR not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (!Double.isNaN(SAR)) {
    	        	fileInfo.setSAR(SAR);
    	        }
    	        try {
    	        	dBdt = global_const.getDouble("dBdt");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("dBdt not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (!Double.isNaN(dBdt)) {
    	        	fileInfo.setDBdt(dBdt);
    	        }
    	        try {
    	        	seriesNumber = global_const.getInt("SeriesNumber");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("SeriesNumber not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (seriesNumber != Integer.MIN_VALUE) {
    	        	fileInfo.setSeriesNumber(seriesNumber);
    	        }
    	        try {
    	        	imagePositionPatient = global_const.getJSONArray("ImagePositionPatient");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("JSONArray ImagePositionPatient not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (imagePositionPatient != null) {
    	        	imagePositionPatientDouble = new double[imagePositionPatient.length()];
    	        	for (i = 0; i < imagePositionPatient.length(); i++) {
    	        		try {
    	        			imagePositionPatientDouble[i] = imagePositionPatient.getDouble(i);
    	        		}
    	        		catch (JSONException e) {
    	        			Preferences.debug("imagePositionPatient.getDouble("+i+") not found\n", Preferences.DEBUG_FILEIO);
    	        		}
    	        	}
    	        	if (imagePositionPatientDouble != null) {
    	        		fileInfo.setImagePositionPatient(imagePositionPatientDouble);
    	        	}
    	        } // if (imagePositionPatient != null)
    	        try {
    	        	imageOrientationPatient = global_const.getJSONArray("ImageOrientationPatient");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("JSONArray ImageOrientationPatient not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (imageOrientationPatient != null) {
    	        	imageOrientationPatientDouble = new double[imageOrientationPatient.length()];
    	        	for (i = 0; i < imageOrientationPatient.length(); i++) {
    	        		try {
    	        			imageOrientationPatientDouble[i] = imageOrientationPatient.getDouble(i);
    	        		}
    	        		catch (JSONException e) {
    	        			Preferences.debug("imageOrientationPatient.getDouble("+i+") not found\n", Preferences.DEBUG_FILEIO);
    	        		}
    	        	}
    	        	if (imageOrientationPatientDouble != null) {
    	        		fileInfo.setImageOrientationPatient(imageOrientationPatientDouble);
    	        	}
    	        } // if (imageOrientationPatient != null)
    	        try {
    	        	sliceLocation = global_const.getDouble("SliceLocation");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("SliceLocation not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (!Double.isNaN(sliceLocation)) {
    	        	fileInfo.setSliceLocation(sliceLocation);
    	        }
    	        try {
    	        	samplesPerPixel = global_const.getInt("SamplesPerPixel");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("SamplesPerPixel not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (samplesPerPixel != Integer.MIN_VALUE) {
    	        	fileInfo.setSamplesPerPixel(samplesPerPixel);
    	        }
    	        try {
    	        	photometricInterpretation = global_const.getString("PhotometricInterpretation");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("PhotometricInterpretation not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (photometricInterpretation != null) {
    	        	fileInfo.setPhotometricInterpretation(photometricInterpretation);
    	        }
    	        try {
    	        	rows = global_const.getInt("Rows");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("Rows not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (rows != Integer.MIN_VALUE) {
    	        	fileInfo.setRows(rows);
    	        }
    	        try {
    	        	columns = global_const.getInt("Columns");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("Columns not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (columns != Integer.MIN_VALUE) {
    	        	fileInfo.setColumns(columns);
    	        }
    	        try {
    	        	pixelSpacing = global_const.getJSONArray("PixelSpacing");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("JSONArray PixelSpacing not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (pixelSpacing != null) {
    	        	pixelSpacingDouble = new double[pixelSpacing.length()];
    	        	for (i = 0; i < pixelSpacing.length(); i++) {
    	        		try {
    	        			pixelSpacingDouble[i] = pixelSpacing.getDouble(i);
    	        		}
    	        		catch (JSONException e) {
    	        			Preferences.debug("pixelSpacing.getDouble("+i+") not found\n", Preferences.DEBUG_FILEIO);
    	        		}
    	        	}
    	        	if (pixelSpacingDouble != null) {
    	        		fileInfo.setPixelSpacing(pixelSpacingDouble);
    	        	}
    	        } // if (pixelSpacing != null)
    	        try {
    	        	bitsAllocated = global_const.getInt("BitsAllocated");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("BitsAllocated not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (bitsAllocated != Integer.MIN_VALUE) {
    	        	fileInfo.setBitsAllocated(bitsAllocated);
    	        }
    	        try {
    	        	bitsStored = global_const.getInt("BitsStored");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("BitsStored not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (bitsStored != Integer.MIN_VALUE) {
    	        	fileInfo.setBitsStored(bitsStored);
    	        }
    	        try {
    	        	highBit = global_const.getInt("HighBit");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("HighBit not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (highBit != Integer.MIN_VALUE) {
    	        	fileInfo.setHighBit(highBit);
    	        }
    	        try {
    	        	pixelRepresentation = global_const.getInt("PixelRepresentation");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("PixelRepresentation not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (pixelRepresentation != Integer.MIN_VALUE) {
    	        	fileInfo.setPixelRepresentation(pixelRepresentation);
    	        }
    	        try {
    	        	smallestImagePixelValue = global_const.getInt("SmallestImagePixelValue");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("SmallestImagePixelValue not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (smallestImagePixelValue != Integer.MAX_VALUE) {
    	        	fileInfo.setSmallestImagePixelValue(smallestImagePixelValue);
    	        }
    	        try {
    	        	largestImagePixelValue = global_const.getInt("LargestImagePixelValue");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("LargestImagePixelValue not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (largestImagePixelValue != Integer.MIN_VALUE) {
    	        	fileInfo.setLargestImagePixelValue(largestImagePixelValue);
    	        }
    	        try {
    	        	windowCenterWidthExplanation = global_const.getString("WindowCenterWidthExplanation");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("WindowCenterWidthExplanation not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (windowCenterWidthExplanation != null) {
    	        	fileInfo.setWindowCenterWidthExplanation(windowCenterWidthExplanation);
    	        }
    	        try {
    	        	performedProcedureStepStartTime = global_const.getString("PerformedProcedureStepStartTime");
    	        }
    	        catch (JSONException e) {
    	        	Preferences.debug("PerformedProcedureStepStartTime not found\n", Preferences.DEBUG_FILEIO);
    	        }
    	        if (performedProcedureStepStartTime != null) {
    	        	fileInfo.setPerformedProcedureStepStartTime(performedProcedureStepStartTime);
    	        }
    	        if (noReadPrivateTags) {
    	        	return;
    	        }
    	        if (!noReadPrivateTags) {
    	        	int CsaImageEchoLinePosition = Integer.MIN_VALUE;
    	        	int CsaImageProtocolSliceNumber = Integer.MIN_VALUE;
    	        	int CsaImageUsedChannelMask = Integer.MIN_VALUE;
    	        	double CsaImageBandwidthPerPixelPhaseEncode = Double.NaN;
    	        	int CsaImageMeasuredFourierLines = Integer.MIN_VALUE;
    	        	int CsaImageSequenceMask = Integer.MIN_VALUE;
    	        	String CsaImageRFSWDDataType = null;
    	        	String CsaImageImaPATModeText = null;
    	        	int CsaImageRealDwellTime = Integer.MIN_VALUE;
    	        	String CsaImageImaCoilString = null;
    	        	int CsaImageEchoColumnPosition = Integer.MIN_VALUE;
    	        	int CsaImagePhaseEncodingDirectionPositive = Integer.MIN_VALUE;
    	        	JSONArray CsaImageSlicePosition_PCS = null;
    	        	double CsaImageSlicePosition_PCSDouble[] = null;
    	        	JSONArray CsaImageSliceNormalVector = null;
    	        	double CsaImageSliceNormalVectorDouble[] = null;
    	        	String CsaImageGSWDDataType = null;
    	        	int CsaImageMultistepIndex = Integer.MIN_VALUE;
    	        	JSONArray CsaImageImaRelTablePosition = null;
    	        	int CsaImageImaRelTablePositionInt[] = null;
    	        	int CsaImageNumberOfImagesInMosaic = Integer.MIN_VALUE;
    	        	int CsaImageNonPlanarImage = Integer.MIN_VALUE;
    	        	int CsaImageEchoPartitionPosition = Integer.MIN_VALUE;
    	        	String CsaImageAcquisitionMatrixText = null;
    	        	JSONArray CsaImageImaAbsTablePosition = null;
    	        	int CsaImageImaAbsTablePositionInt[] = null;
    	        	double CsaSeriesTalesReferencePower = Double.NaN;
    	        	int CsaSeriesOperation_mode_flag = Integer.MIN_VALUE;
    	        	double CsaSeriesdBdt_thresh = Double.NaN;
    	        	int CsaSeriesProtocolChangeHistory = Integer.MIN_VALUE;
    	        	JSONArray CsaSeriesGradientDelayTime = null;
    	        	double CsaSeriesGradientDelayTimeDouble[] = null;
    	        	JSONArray CsaSeriesSARMostCriticalAspect = null;
    	        	double CsaSeriesSARMostCriticalAspectDouble[] = null;
    	        	JSONArray CsaSeriesB1rms = null;
    	        	double CsaSeriesB1rmsDouble[] = null;
    	        	String CsaSeriesPATModeText = null;
    	        	JSONArray CsaSeriesRelTablePosition = null;
    	        	int CsaSeriesRelTablePositionInt[] = null;
    	        	int CsaSeriesNumberOfPrescans = Integer.MIN_VALUE;
    	        	double CsaSeriesdBdt_limit = Double.NaN;
    	        	JSONArray CsaSeriesStim_lim = null;
    	        	double CsaSeriesStim_limDouble[] = null;
    	        	String CsaSeriesPatReinPattern = null;
    	        	String CsaSeriesB1rmsSupervision = null;
    	        	double CsaSeriesReadoutGradientAmplitude = Double.NaN;
    	        	int CsaSeriesMrProtocolVersion = Integer.MIN_VALUE;
    	        	String CsaSeriesRFSWDMostCriticalAspect = null;
    	        	String CsaSeriesSequenceFileOwner = null;
    	        	String CsaSeriesGradientMode = null;
    	        	int CsaSeriesSliceArrayConcatenations = Integer.MIN_VALUE;
    	        	String CsaSeriesFlowCompensation = null;
    	        	double CsaSeriesTransmitterCalibration = Double.NaN;
    	        	int CsaSeriesIsocentered = Integer.MIN_VALUE;
    	        	int CsaSeriesAbsTablePosition = Integer.MIN_VALUE;
    	        	double CsaSeriesReadoutOS = Double.NaN;
    	        	double CsaSeriesdBdt_max = Double.NaN;
    	        	int CsaSeriesRFSWDOperationMode = Integer.MIN_VALUE;
    	        	double CsaSeriesSelectionGradientAmplitude = Double.NaN;
    	        	double CsaSeriesPhaseGradientAmplitude = Double.NaN;
    	        	int CsaSeriesRfWatchdogMask = Integer.MIN_VALUE;
    	        	String CsaSeriesCoilForGradient2 = null;
    	        	int CsaSeriesStim_mon_mode = Integer.MIN_VALUE;
    	        	JSONArray CsaSeriesCoilId = null;
    	        	int CsaSeriesCoilIdInt[] = null;
    	        	double CsaSeriesStim_max_ges_norm_online = Double.NaN;
    	        	String CsaSeriesCoilString = null;
    	        	String CsaSeriesCoilForGradient = null;
    	        	JSONArray CsaSeriesTablePositionOrigin = null;
    	        	int CsaSeriesTablePositionOriginInt[] = null;
    	        	JSONArray CsaSeriesMiscSequenceParam = null;
    	        	int CsaSeriesMiscSequenceParamInt[] = null;
    	        	String CsaSeriesLongModelName = null;
    	        	double CsaSeriesStim_faktor = Double.NaN;
    	        	double CsaSeriesSW_korr_faktor = Double.NaN;
    	        	JSONArray CsaSeriesSed = null;
    	        	double CsaSeriesSedDouble[] = null;
    	        	String CsaSeriesPositivePCSDirections = null;
    	        	double CsaSeriesSliceResolution = Double.NaN;
    	        	JSONArray CsaSeriesStim_max_online = null;
    	        	double CsaSeriesStim_max_onlineDouble[] = null;
    	        	double CsaSeriest_puls_max = Double.NaN;
    	        	int CsaSeriesMrPhoenixProtocolulVersion = Integer.MIN_VALUE;
    	        	String CsaSeriesMrPhoenixProtocoltSequenceFileName = null;
    	        	String CsaSeriesMrPhoenixProtocoltProtocolName = null;
    	        	String CsaSeriesMrPhoenixProtocoltReferenceImage0 = null;
    	        	String CsaSeriesMrPhoenixProtocoltReferenceImage1 = null;
    	        	String CsaSeriesMrPhoenixProtocoltReferenceImage2 = null;
    	        	int CsaSeriesMrPhoenixProtocolucScanRegionPosValid = Integer.MIN_VALUE;
    	        	int CsaSeriesMrPhoenixProtocolucTablePositioningMode = Integer.MIN_VALUE;
    	        	String CsaSeriesMrPhoenixProtocolsProtConsistencyInfotBaselineString = null;
    	        	String CsaSeriesMrPhoenixProtocolsProtConsistencyInfotSystemType = null;
    	        	double CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflNominalB0 = Double.NaN;
    	        	double CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflGMax = Double.NaN;
    	        	double CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflRiseTime = Double.NaN;
    	        	int CsaSeriesMrPhoenixProtocolsProtConsistencyInfolMaximumNofRxReceiverChannels = Integer.MIN_VALUE;
    	        	double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude0 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude1 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude2 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude3 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude4 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant0 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant1 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant2 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant3 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant4 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude0 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude1 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude2 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude3 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude4 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant0 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant1 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant2 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant3 = Double.NaN; 
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant4 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude0 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude1 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude2 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude3 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude4 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant0 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant1 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant2 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant3 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant4 = Double.NaN;
    	            int CsaSeriesMrPhoenixProtocolsGRADSPECbEddyCompensationValid = Integer.MIN_VALUE;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude0 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude1 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude2 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant0 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant1 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant2 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude0 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude1 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude2 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant0 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant1 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant2 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude0 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude1 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude2 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant0 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant1 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant2 = Double.NaN;
    	            int CsaSeriesMrPhoenixProtocolsGRADSPECbB0CompensationValid = Integer.MIN_VALUE;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXYaflAmplitude0 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXYaflTimeConstant0 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXZaflAmplitude0 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXZaflTimeConstant0 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYXaflAmplitude0 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYXaflTimeConstant0 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYZaflAmplitude0 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYZaflTimeConstant0 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZXaflAmplitude0 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZXaflTimeConstant0 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZYaflAmplitude0 = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZYaflTimeConstant0 = Double.NaN;
    	            int CsaSeriesMrPhoenixProtocolsGRADSPECbCrossTermCompensationValid = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetX = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetY = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetZ = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsGRADSPECbOffsetValid = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsGRADSPEClDelayX = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsGRADSPEClDelayY = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsGRADSPEClDelayZ = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsGRADSPECbDelayValid = Integer.MIN_VALUE;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityX = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityY = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityZ = Double.NaN;
    	            int CsaSeriesMrPhoenixProtocolsGRADSPECbSensitivityValid = Integer.MIN_VALUE;
    	            double CsaSeriesMrPhoenixProtocolsGRADSPECflGSWDMinRiseTime = Double.NaN;
    	            int CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent0 = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent1 = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent2 = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent3 = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent4 = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsGRADSPECbShimCurrentValid = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsGRADSPECucMode = Integer.MIN_VALUE;
    	            String CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0tNucleus = null;
    	            int CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0lFrequency = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bFrequencyValid = Integer.MIN_VALUE;
    	            double CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0flReferenceAmplitude = Double.NaN;
    	            int CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bReferenceAmplitudeValid = Integer.MIN_VALUE;
    	            double CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0flAmplitudeCorrection = Double.NaN;
    	            int CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bAmplitudeCorrectionValid = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bRFPAIndexValid = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bFrequencyValid = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bReferenceAmplitudeValid = Integer.MIN_VALUE;
    	            double CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1flAmplitudeCorrection = Double.NaN;
    	            int CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bAmplitudeCorrectionValid = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1lRFPAIndex = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bRFPAIndexValid = Integer.MIN_VALUE;
    	            String CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEntName[] = new String[2];
    	            int CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnbAmplitudeValid[] = new int[2];
    	            double CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnflAmplitude[] = new double[2];
    	            int CsaSeriesMrPhoenixProtocolsTXSPEClNoOfTraPulses = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsTXSPEClBCExcitationMode = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsTXSPEClBCSeqExcitationMode = Integer.MIN_VALUE;
    	            double CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeMin = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeMax = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeClipLow = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeClipHigh = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsTXSPECflKDynPhaseMax = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsTXSPECflKDynPhaseClip = Double.NaN;
    	            int CsaSeriesMrPhoenixProtocolsTXSPECbKDynValid = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsTXSPECucRFPulseType = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsTXSPECucExcitMode = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsTXSPECucSimultaneousExcitation = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsTXSPECucBCExcitationModeValid = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsRXSPEClGain = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsRXSPECbGainValid = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsRXSPECalDwellTime0 = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsAdjDatauiAdjFreMode = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsAdjDatauiAdjShimMode = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsAdjDatauiAdjWatSupMode = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsAdjDatauiAdjRFMapMode = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsAdjDatauiAdjMDSMode = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsAdjDatauiAdjTableTolerance = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsAdjDatauiAdjProtID = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsAdjDatauiAdjFreProtRelated = Integer.MIN_VALUE;
    	            double CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondSag = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondCor = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondTra = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldSag = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldCor = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldTra = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedThickness = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedPhaseFOV = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedReadoutFOV = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedInPlaneRot = Double.NaN;
    	            int CsaSeriesMrPhoenixProtocolsAdjDatauiAdjVolumeValid = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolucEnableNoiseAdjust = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolalTR0 = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocollContrasts = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolalTE0 = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolacFlowComp0 = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocollCombinedEchoes = Integer.MIN_VALUE;
    	            double CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondSag[] = new double[fileInfo.getExtents()[2]];
    	            double CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondCor[] = new double[fileInfo.getExtents()[2]];
    	            double CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondTra[] = new double[fileInfo.getExtents()[2]];
    	            double CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldSag[] = new double[fileInfo.getExtents()[2]];
    	            double CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldCor[] = new double[fileInfo.getExtents()[2]];
    	            double CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldTra[] = new double[fileInfo.getExtents()[2]];
    	            double CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdThickness[] = new double[fileInfo.getExtents()[2]];
    	            double CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdPhaseFOV[] = new double[fileInfo.getExtents()[2]];
    	            double CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdReadoutFOV[] = new double[fileInfo.getExtents()[2]];        
    	            double CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdInPlaneRot[] = new double[fileInfo.getExtents()[2]];
    	            int CsaSeriesMrPhoenixProtocolsSliceArrayanAsc[] = new int[fileInfo.getExtents()[2]-1];
    	            int CsaSeriesMrPhoenixProtocolsSliceArrayanPos[] = new int[fileInfo.getExtents()[2]-1];
    	            int CsaSeriesMrPhoenixProtocolsSliceArraylSize = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsSliceArraylConc = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsSliceArrayucMode = Integer.MIN_VALUE;
    	            double CsaSeriesMrPhoenixProtocolsSliceArraysTSatdThickness = Double.NaN;
    	            int CsaSeriesMrPhoenixProtocolsGroupArrayasGroup0nSize = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsGroupArrayanMember[] = new int[fileInfo.getExtents()[2]];
    	            int CsaSeriesMrPhoenixProtocolsGroupArraylSize = Integer.MIN_VALUE;
    	            double CsaSeriesMrPhoenixProtocolsGroupArraysPSatdThickness = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsGroupArraysPSatdGap = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondSag = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondCor = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondTra = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldSag = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldCor = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldTra = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsRSatArrayasElm0dThickness = Double.NaN;
    	            int CsaSeriesMrPhoenixProtocolsRSatArraylSize = Integer.MIN_VALUE;
    	            double CsaSeriesMrPhoenixProtocolsAutoAligndAAMatrix[] = new double[16];
    	            int CsaSeriesMrPhoenixProtocolsNavigatorParalBreathHoldMeas = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsNavigatorParalRespComp = Integer.MIN_VALUE;
    	            double CsaSeriesMrPhoenixProtocolsBladeParadBladeCoverage = Double.NaN;
    	            int CsaSeriesMrPhoenixProtocolsBladeParaucMotionCorr = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPrepPulsesucFatSat = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPrepPulsesucWaterSat = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPrepPulsesucInversion = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPrepPulsesucSatRecovery = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPrepPulsesucT2Prep = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPrepPulsesucTIScout = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPrepPulsesucFatSatMode = Integer.MIN_VALUE;
    	            double CsaSeriesMrPhoenixProtocolsPrepPulsesdDarkBloodThickness = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsPrepPulsesdDarkBloodFlipAngle = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsPrepPulsesdT2PrepDuration = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsPrepPulsesdIRPulseThicknessFactor = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsKSpacedPhaseResolution = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsKSpacedSliceResolution = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsKSpacedAngioDynCentralRegionA = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocolsKSpacedAngioDynSamplingDensityB = Double.NaN;
    	            int CsaSeriesMrPhoenixProtocolsKSpacelBaseResolution = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsKSpacelPhaseEncodingLines = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsKSpacelPartitions = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsKSpacelImagesPerSlab = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsKSpacelRadialViews = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsKSpacelRadialInterleavesPerImage = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsKSpacelLinesPerShot = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsKSpaceunReordering = Integer.MIN_VALUE;
    	            double CsaSeriesMrPhoenixProtocolsKSpacedSeqPhasePartialFourierForSNR = Double.NaN;
    	            int CsaSeriesMrPhoenixProtocolsKSpaceucPhasePartialFourier = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsKSpaceucSlicePartialFourier = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsKSpaceucAveragingMode = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsKSpaceucMultiSliceMode = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsKSpaceucDimension = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsKSpaceucTrajectory = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsKSpaceucViewSharing = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsKSpaceucAsymmetricEchoMode = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsKSpaceucPOCS = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsFastImaginglEPIFactor = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsFastImaginglTurboFactor = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsFastImaginglSliceTurboFactor = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsFastImaginglSegments = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsFastImagingucSegmentationMode = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsFastImaginglShots = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsFastImaginglEchoTrainDuration = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPhysioImaginglSignal1 = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPhysioImaginglMethod1 = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPhysioImaginglSignal2 = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPhysioImaginglMethod2 = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPhysioImaginglPhases = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPhysioImaginglRetroGatedImages = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerPulses = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerWindow = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlArrhythmiaDetection = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlCardiacGateOnThreshold = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlCardiacGateOffThreshold = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerIntervals = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerPulses = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerWindow = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselArrhythmiaDetection = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselCardiacGateOnThreshold = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselCardiacGateOffThreshold = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerIntervals = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerPulses = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerWindow = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlArrhythmiaDetection = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlCardiacGateOnThreshold = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlCardiacGateOffThreshold = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerIntervals = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioResplRespGateThreshold = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioResplRespGatePhase = Integer.MIN_VALUE;
    	            double CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioRespdGatingRatio = Double.NaN;
    	            int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioNativeucMode = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioNativeucFlowSenMode = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsSpecParalPhaseCyclingType = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsSpecParalPhaseEncodingType = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsSpecParalRFExcitationBandwidth = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsSpecParaucRemoveOversampling = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsSpecParalAutoRefScanNo = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsSpecParalDecouplingType = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsSpecParalNOEType = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsSpecParalExcitationType = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsSpecParalSpecAppl = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsSpecParalSpectralSuppression = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsDiffusionulMode = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsAngioucPCFlowMode = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsAngioucTOFInflow = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsAngiolDynamicReconMode = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsAngiolTemporalInterpolation = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsRawFilterlSlope_256 = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsRawFilterucOn = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsRawFilterucMode = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsDistortionCorrFilterucMode = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPatlAccelFactPE = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPatlAccelFact3D = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPatlRefLinesPE = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPatucPATMode = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPatucRefScanMode = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsPatucTPatAverageAllFrames = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsMDSulMdsModeMask = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsMDSulMdsVariableResolution = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsMDSlTableSpeedNumerator = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsMDSlmdsLinesPerSegment = Integer.MIN_VALUE;
    	            double CsaSeriesMrPhoenixProtocolsMDSsMdsEndPosSBCS_mmdTra = Double.NaN;
    	            int CsaSeriesMrPhoenixProtocolsMDSulMdsReconMode = Integer.MIN_VALUE;
    	            double CsaSeriesMrPhoenixProtocolsMDSdMdsRangeExtension = Double.NaN;
    	            int CsaSeriesMrPhoenixProtocolucEnableIntro = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolucDisableChangeStoreImages = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolucAAMode = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolucAARegionMode = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolucAARefMode = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolucReconstructionMode = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolucOneSeriesForAllMeas = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolucPHAPSMode = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolucDixon = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolucDixonSaveOriginal = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolucWaitForPrepareCompletion = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocollAverages = Integer.MIN_VALUE;
    	            double CsaSeriesMrPhoenixProtocoldAveragesDouble = Double.NaN;
    	            int CsaSeriesMrPhoenixProtocollRepetitions = Integer.MIN_VALUE;
    	            double CsaSeriesMrPhoenixProtocoladFlipAngleDegree0 = Double.NaN;
    	            int CsaSeriesMrPhoenixProtocollScanTimeSec = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocollTotalScanTimeSec = Integer.MIN_VALUE;
    	            double CsaSeriesMrPhoenixProtocoldRefSNR = Double.NaN;
    	            double CsaSeriesMrPhoenixProtocoldRefSNR_VOI = Double.NaN;
    	            String CsaSeriesMrPhoenixProtocoltdefaultEVAProt = null;
    	            String CsaSeriesMrPhoenixProtocolasCoilSelectMeas0tNucleus = null;
    	            int CsaSeriesMrPhoenixProtocolasCoilSelectMeas0iUsedRFactor = Integer.MIN_VALUE;
    	            String CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtCoilID[] = new String[24];
    	            int CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDlCoilCopy[] = new int[24];
    	            String CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtElement[] = new String[24];
    	            int CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlElementSelected[] = new int[24];
    	            int CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlRxChannelConnected[] = new int[24];
    	            int CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSaulPlugId[] = new int[6];
    	            int CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSauiNmbrOfNibbles[] = new int[6];
    	            double CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnflFactor[] = new double[24];
    	            int CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnbValid[] = new int[24];
    	            int CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnlRxChannel[] = new int[24];
    	            int CsaSeriesMrPhoenixProtocolsEFISPECbEFIDataValid = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsWiPMemBlockalFree[] = new int[17];
    	            double CsaSeriesMrPhoenixProtocolsWiPMemBlockadFree[] = new double[12];
    	            String CsaSeriesMrPhoenixProtocolsWiPMemBlocktFree = null;
    	            int CsaSeriesMrPhoenixProtocolucBOLDParadigmArray[] = new int[75];
    	            int CsaSeriesMrPhoenixProtocollParadigmPeriodicity = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolucCineMode = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolucSequenceType = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolucCoilCombineMode = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolucFlipAngleMode = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocollTOM = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocollProtID = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolucReadOutMode = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolucBold3dPace = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolucForcePositioningOnNDIS = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolucInteractiveRealtime = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolucInternalTablePosValid = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsParametricMappingucParametricMap = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolsAslulMode = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolWaitForUserStart = Integer.MIN_VALUE;
    	            int CsaSeriesMrPhoenixProtocolucAutoAlignInit = Integer.MIN_VALUE;
    	            
	    	        try {
	    	        	CsaImageEchoLinePosition = global_const.getInt("CsaImage.EchoLinePosition");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaImage.EchoLinePosition not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaImageEchoLinePosition != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaImageEchoLinePosition(CsaImageEchoLinePosition);
	    	        }
	    	        try {
	    	        	CsaImageProtocolSliceNumber = global_const.getInt("CsaImage.ProtocolSliceNumber");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaImage.ProtocolSliceNumber not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaImageProtocolSliceNumber != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaImageProtocolSliceNumber(CsaImageProtocolSliceNumber);
	    	        }
	    	        try {
	    	        	CsaImageUsedChannelMask = global_const.getInt("CsaImage.UsedChannelMask");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaImage.UsedChannelMask not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaImageUsedChannelMask != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaImageUsedChannelMask(CsaImageUsedChannelMask);
	    	        }
	    	        try {
	    	        	CsaImageBandwidthPerPixelPhaseEncode = global_const.getDouble("CsaImage.BandwidthPerPixelPhaseEncode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaImage.BandwidthPerPixelPhaseEncode not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaImageBandwidthPerPixelPhaseEncode)) {
	    	        	fileInfo.setCsaImageBandwidthPerPixelPhaseEncode(CsaImageBandwidthPerPixelPhaseEncode);
	    	        }
	    	        try {
	    	        	CsaImageMeasuredFourierLines = global_const.getInt("CsaImage.MeasuredFourierLines");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaImage.MeasuredFourierLines not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaImageMeasuredFourierLines != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaImageMeasuredFourierLines(CsaImageMeasuredFourierLines);
	    	        }
	    	        try {
	    	        	CsaImageSequenceMask = global_const.getInt("CsaImage.SequenceMask");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaImage.SequenceMask not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaImageSequenceMask != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaImageSequenceMask(CsaImageSequenceMask);
	    	        }
	    	        try {
	    	        	CsaImageRFSWDDataType = global_const.getString("CsaImage.RFSWDDataType");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaImage.RFSWDDataType not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaImageRFSWDDataType != null) {
	    	        	fileInfo.setCsaImageRFSWDDataType(CsaImageRFSWDDataType);
	    	        }
	    	        try {
	    	        	CsaImageImaPATModeText = global_const.getString("CsaImage.ImaPATModeText");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaImage.ImaPATModeText not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaImageImaPATModeText != null) {
	    	        	fileInfo.setCsaImageImaPATModeText(CsaImageImaPATModeText);
	    	        }
	    	        try {
	    	        	CsaImageRealDwellTime = global_const.getInt("CsaImage.RealDwellTime");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaImage.RealDwellTime not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaImageRealDwellTime != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaImageRealDwellTime(CsaImageRealDwellTime);
	    	        }
	    	        try {
	    	        	CsaImageImaCoilString = global_const.getString("CsaImage.ImaCoilString");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaImage.ImaCoilString not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaImageImaCoilString != null) {
	    	        	fileInfo.setCsaImageImaCoilString(CsaImageImaCoilString);
	    	        }
	    	        try {
	    	        	CsaImageEchoColumnPosition = global_const.getInt("CsaImage.EchoColumnPosition");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaImage.EchoColumnPosition not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaImageEchoColumnPosition != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaImageEchoColumnPosition(CsaImageEchoColumnPosition);
	    	        }
	    	        try {
	    	        	CsaImagePhaseEncodingDirectionPositive = global_const.getInt("CsaImage.PhaseEncodingDirectionPositive");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaImage.PhaseEncodingDirectionPositive not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaImagePhaseEncodingDirectionPositive != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaImagePhaseEncodingDirectionPositive(CsaImagePhaseEncodingDirectionPositive);
	    	        }
	    	        try {
	    	        	CsaImageSlicePosition_PCS = global_const.getJSONArray("CsaImage.SlicePosition_PCS");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("JSONArray CsaImage.SlicePosition_PCS not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaImageSlicePosition_PCS != null) {
	    	            CsaImageSlicePosition_PCSDouble = new double[CsaImageSlicePosition_PCS.length()];
	    	            for (i = 0; i < CsaImageSlicePosition_PCS.length(); i++) {
	    	            	try {
	    	            		CsaImageSlicePosition_PCSDouble[i] = CsaImageSlicePosition_PCS.getDouble(i);
	    	            	}
	    	            	catch (JSONException e) {
	    	            		Preferences.debug("CsaImageSlicePosition_PCS.getDouble("+i+") not found\n", Preferences.DEBUG_FILEIO);
	    	            	}
	    	            }
	    	            if (CsaImageSlicePosition_PCSDouble != null) {
	    	            	fileInfo.setCsaImageSlicePosition_PCS(CsaImageSlicePosition_PCSDouble);
	    	            }
	    	        } // if (CsaImageSlicePosition_PCS != null) 
	    	        try {
	    	        	CsaImageSliceNormalVector = global_const.getJSONArray("CsaImage.SliceNormalVector");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("JSONArray CsaImage.SliceNormalVector not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaImageSliceNormalVector != null) {
	    	            CsaImageSliceNormalVectorDouble = new double[CsaImageSliceNormalVector.length()];
	    	            for (i = 0; i < CsaImageSliceNormalVector.length(); i++) {
	    	            	try {
	    	            		CsaImageSliceNormalVectorDouble[i] = CsaImageSliceNormalVector.getDouble(i);
	    	            	}
	    	            	catch (JSONException e) {
	    	            		Preferences.debug("CsaImageSliceNormalVector.getDouble("+i+") not found\n", Preferences.DEBUG_FILEIO);
	    	            	}
	    	            }
	    	            if (CsaImageSliceNormalVectorDouble != null) {
	    	            	fileInfo.setCsaImageSliceNormalVector(CsaImageSliceNormalVectorDouble);
	    	            }
	    	        } // if (CsaImageSliceNormalVector != null) 
	    	        try {
	    	        	CsaImageGSWDDataType = global_const.getString("CsaImage.GSWDDataType");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaImage.GSWDDataType not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaImageGSWDDataType != null) {
	    	        	fileInfo.setCsaImageGSWDDataType(CsaImageGSWDDataType);
	    	        }
	    	        try {
	    	        	CsaImageMultistepIndex = global_const.getInt("CsaImage.MultistepIndex");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaImage.MultistepIndex not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaImageMultistepIndex != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaImageMultistepIndex(CsaImageMultistepIndex);
	    	        }
	    	        try {
	    	        	CsaImageImaRelTablePosition = global_const.getJSONArray("CsaImage.ImaRelTablePosition");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("JSONArray CsaImage.ImaRelTablePosition not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaImageImaRelTablePosition != null) {
	    	            CsaImageImaRelTablePositionInt = new int[CsaImageImaRelTablePosition.length()];
	    	            for (i = 0; i < CsaImageImaRelTablePosition.length(); i++) {
	    	            	try {
	    	            		CsaImageImaRelTablePositionInt[i] = CsaImageImaRelTablePosition.getInt(i);
	    	            	}
	    	            	catch (JSONException e) {
	    	            		Preferences.debug("CsaImageImaRelTablePosition.getInt("+i+") not found\n", Preferences.DEBUG_FILEIO);
	    	            	}
	    	            }
	    	            if (CsaImageImaRelTablePositionInt != null) {
	    	            	fileInfo.setCsaImageImaRelTablePosition(CsaImageImaRelTablePositionInt);
	    	            }
	    	        } // if (CsaImageImaRelTablePosition != null)
	    	        try {
	    	        	CsaImageNumberOfImagesInMosaic = global_const.getInt("CsaImage.NumberOfImagesInMosaic");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaImage.NumberOfImagesInMosaic not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaImageNumberOfImagesInMosaic != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaImageNumberOfImagesInMosaic(CsaImageNumberOfImagesInMosaic);
	    	        }
	    	        try {
	    	        	CsaImageNonPlanarImage = global_const.getInt("CsaImage.NonPlanarImage");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaImage.NonPlanarImage not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaImageNonPlanarImage != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaImageNonPlanarImage(CsaImageNonPlanarImage);
	    	        }
	    	        try {
	    	        	CsaImageEchoPartitionPosition = global_const.getInt("CsaImage.EchoPartitionPosition");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaImage.EchoPartitionPosition not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaImageEchoPartitionPosition != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaImageEchoPartitionPosition(CsaImageEchoPartitionPosition);
	    	        }
	    	        try {
	    	        	CsaImageAcquisitionMatrixText = global_const.getString("CsaImage.AcquisitionMatrixText");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaImage.AcquisitionMatrixText not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaImageAcquisitionMatrixText != null) {
	    	        	fileInfo.setCsaImageAcquisitionMatrixText(CsaImageAcquisitionMatrixText);
	    	        }
	    	        try {
	    	        	CsaImageImaAbsTablePosition = global_const.getJSONArray("CsaImage.ImaAbsTablePosition");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("JSONArray CsaImage.ImaAbsTablePosition not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaImageImaAbsTablePosition != null) {
	    	            CsaImageImaAbsTablePositionInt = new int[CsaImageImaAbsTablePosition.length()];
	    	            for (i = 0; i < CsaImageImaAbsTablePosition.length(); i++) {
	    	            	try {
	    	            		CsaImageImaAbsTablePositionInt[i] = CsaImageImaAbsTablePosition.getInt(i);
	    	            	}
	    	            	catch (JSONException e) {
	    	            		Preferences.debug("CsaImageImaAbsTablePosition.getInt("+i+") not found\n", Preferences.DEBUG_FILEIO);
	    	            	}
	    	            }
	    	            if (CsaImageImaAbsTablePositionInt != null) {
	    	            	fileInfo.setCsaImageImaAbsTablePosition(CsaImageImaAbsTablePositionInt);
	    	            }
	    	        } // if (CsaImageImaAbsTablePosition != null)
	    	        try {
	    	        	CsaSeriesTalesReferencePower = global_const.getDouble("CsaSeries.TalesReferencePower");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.TalesReferencePower not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesTalesReferencePower)) {
	    	        	fileInfo.setCsaSeriesTalesReferencePower(CsaSeriesTalesReferencePower);
	    	        }
	    	        try {
	    	        	CsaSeriesOperation_mode_flag = global_const.getInt("CsaSeries.Operation_mode_flag");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.Operation_mode_flag not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesOperation_mode_flag != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesOperation_mode_flag(CsaSeriesOperation_mode_flag);
	    	        }
	    	        try {
	    	        	CsaSeriesdBdt_thresh = global_const.getDouble("CsaSeries.dBdt_thresh");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.dBdt_thresh not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesdBdt_thresh)) {
	    	        	fileInfo.setCsaSeriesdBdt_thresh(CsaSeriesdBdt_thresh);
	    	        }
	    	        try {
	    	        	CsaSeriesProtocolChangeHistory = global_const.getInt("CsaSeries.ProtocolChangeHistory");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.ProtocolChangeHistory not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesProtocolChangeHistory != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesProtocolChangeHistory(CsaSeriesProtocolChangeHistory);
	    	        }
	    	        try {
	    	        	CsaSeriesGradientDelayTime = global_const.getJSONArray("CsaSeries.GradientDelayTime");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("JSONArray CsaSeries.GradientDelayTime not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesGradientDelayTime != null) {
	    	            CsaSeriesGradientDelayTimeDouble = new double[CsaSeriesGradientDelayTime.length()];
	    	            for (i = 0; i < CsaSeriesGradientDelayTime.length(); i++) {
	    	            	try {
	    	            		CsaSeriesGradientDelayTimeDouble[i] = CsaSeriesGradientDelayTime.getDouble(i);
	    	            	}
	    	            	catch (JSONException e) {
	    	            		Preferences.debug("CsaSeriesGradientDelayTime.getDouble("+i+") not found\n", Preferences.DEBUG_FILEIO);
	    	            	}
	    	            }
	    	            if (CsaSeriesGradientDelayTimeDouble != null) {
	    	            	fileInfo.setCsaSeriesGradientDelayTime(CsaSeriesGradientDelayTimeDouble);
	    	            }
	    	        } // if (CsaSeriesGradientDelayTime != null)
	    	        try {
	    	        	CsaSeriesSARMostCriticalAspect = global_const.getJSONArray("CsaSeries.SARMostCriticalAspect");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("JSONArray CsaSeries.SARMostCriticalAspect not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesSARMostCriticalAspect != null) {
	    	            CsaSeriesSARMostCriticalAspectDouble = new double[CsaSeriesSARMostCriticalAspect.length()];
	    	            for (i = 0; i < CsaSeriesSARMostCriticalAspect.length(); i++) {
	    	            	try {
	    	            		CsaSeriesSARMostCriticalAspectDouble[i] = CsaSeriesSARMostCriticalAspect.getDouble(i);
	    	            	}
	    	            	catch (JSONException e) {
	    	            		Preferences.debug("CsaSeriesSARMostCriticalAspect.getDouble("+i+") not found\n", Preferences.DEBUG_FILEIO);
	    	            	}
	    	            }
	    	            if (CsaSeriesSARMostCriticalAspectDouble != null) {
	    	            	fileInfo.setCsaSeriesSARMostCriticalAspect(CsaSeriesSARMostCriticalAspectDouble);
	    	            }
	    	        } // if (CsaSeriesSARMostCriticalAspect != null) 
	    	        try {
	    	        	CsaSeriesB1rms = global_const.getJSONArray("CsaSeries.B1rms");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("JSONArray CsaSeries.B1rms not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesB1rms != null) {
	    	            CsaSeriesB1rmsDouble = new double[CsaSeriesB1rms.length()];
	    	            for (i = 0; i < CsaSeriesB1rms.length(); i++) {
	    	            	try {
	    	            		CsaSeriesB1rmsDouble[i] = CsaSeriesB1rms.getDouble(i);
	    	            	}
	    	            	catch (JSONException e) {
	    	            		Preferences.debug("CsaSeriesB1rms.getDouble("+i+") not found\n", Preferences.DEBUG_FILEIO);
	    	            	}
	    	            }
	    	            if (CsaSeriesB1rmsDouble != null) {
	    	            	fileInfo.setCsaSeriesB1rms(CsaSeriesB1rmsDouble);
	    	            }
	    	        } // if (CsaSeriesB1rms != null) 
	    	        try {
	    	        	CsaSeriesPATModeText = global_const.getString("CsaSeries.PATModeText");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.PATModeText not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesPATModeText != null) {
	    	        	fileInfo.setCsaSeriesPATModeText(CsaSeriesPATModeText);
	    	        }
	    	        try {
	    	        	CsaSeriesRelTablePosition = global_const.getJSONArray("CsaSeries.RelTablePosition");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("JSONArray CsaSeries.RelTablePosition not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesRelTablePosition != null) {
	    	            CsaSeriesRelTablePositionInt = new int[CsaSeriesRelTablePosition.length()];
	    	            for (i = 0; i < CsaSeriesRelTablePosition.length(); i++) {
	    	            	try {
	    	            		CsaSeriesRelTablePositionInt[i] = CsaSeriesRelTablePosition.getInt(i);
	    	            	}
	    	            	catch (JSONException e) {
	    	            		Preferences.debug("CsaSeriesRelTablePosition.getInt("+i+") not found\n", Preferences.DEBUG_FILEIO);
	    	            	}
	    	            }
	    	            if (CsaSeriesRelTablePositionInt != null) {
	    	            	fileInfo.setCsaSeriesRelTablePosition(CsaSeriesRelTablePositionInt);
	    	            }
	    	        } // if (CsaSeriesRelTablePosition != null)
	    	        try {
	    	        	CsaSeriesNumberOfPrescans = global_const.getInt("CsaSeries.NumberOfPrescans");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.NumberOfPrescans not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesNumberOfPrescans != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesNumberOfPrescans(CsaSeriesNumberOfPrescans);
	    	        }
	    	        try {
	    	        	CsaSeriesdBdt_limit = global_const.getDouble("CsaSeries.dBdt_limit");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.dBdt_limit not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesdBdt_limit)) {
	    	        	fileInfo.setCsaSeriesdBdt_limit(CsaSeriesdBdt_limit);
	    	        }
	    	        try {
	    	        	CsaSeriesStim_lim = global_const.getJSONArray("CsaSeries.Stim_lim");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("JSONArray CsaSeries.Stim_lim not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesStim_lim != null) {
	    	            CsaSeriesStim_limDouble = new double[CsaSeriesStim_lim.length()];
	    	            for (i = 0; i < CsaSeriesStim_lim.length(); i++) {
	    	            	try {
	    	            		CsaSeriesStim_limDouble[i] = CsaSeriesStim_lim.getDouble(i);
	    	            	}
	    	            	catch (JSONException e) {
	    	            		Preferences.debug("CsaSeriesStim_lim.getDouble("+i+") not found\n", Preferences.DEBUG_FILEIO);
	    	            	}
	    	            }
	    	            if (CsaSeriesStim_limDouble != null) {
	    	            	fileInfo.setCsaSeriesStim_lim(CsaSeriesStim_limDouble);
	    	            }
	    	        } // if (CsaSeriesStim_lim != null)
	    	        try {
	    	        	CsaSeriesPatReinPattern = global_const.getString("CsaSeries.PatReinPattern");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.PatReinPattern not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesPatReinPattern != null) {
	    	        	fileInfo.setCsaSeriesPatReinPattern(CsaSeriesPatReinPattern);
	    	        }
	    	        try {
	    	        	CsaSeriesB1rmsSupervision = global_const.getString("CsaSeries.B1rmsSupervision");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.B1rmsSupervision not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesB1rmsSupervision != null) {
	    	        	fileInfo.setCsaSeriesB1rmsSupervision(CsaSeriesB1rmsSupervision);
	    	        }
	    	        try {
	    	        	CsaSeriesReadoutGradientAmplitude = global_const.getDouble("CsaSeries.ReadoutGradientAmplitude");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.ReadoutGradientAmplitude not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesReadoutGradientAmplitude)) {
	    	        	fileInfo.setCsaSeriesReadoutGradientAmplitude(CsaSeriesReadoutGradientAmplitude);
	    	        }
	    	        try {
	    	        	CsaSeriesMrProtocolVersion = global_const.getInt("CsaSeries.MrProtocolVersion");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrProtocolVersion not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrProtocolVersion != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrProtocolVersion(CsaSeriesMrProtocolVersion);
	    	        }
	    	        try {
	    	        	CsaSeriesRFSWDMostCriticalAspect = global_const.getString("CsaSeries.RFSWDMostCriticalAspect");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.RFSWDMostCriticalAspect not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesRFSWDMostCriticalAspect != null) {
	    	        	fileInfo.setCsaSeriesRFSWDMostCriticalAspect(CsaSeriesRFSWDMostCriticalAspect);
	    	        }
	    	        try {
	    	        	CsaSeriesSequenceFileOwner = global_const.getString("CsaSeries.SequenceFileOwner");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.SequenceFileOwner not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesSequenceFileOwner != null) {
	    	        	fileInfo.setCsaSeriesSequenceFileOwner(CsaSeriesSequenceFileOwner);
	    	        }
	    	        try {
	    	        	CsaSeriesGradientMode = global_const.getString("CsaSeries.GradientMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.GradientMode not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesGradientMode != null) {
	    	        	fileInfo.setCsaSeriesGradientMode(CsaSeriesGradientMode);
	    	        }
	    	        try {
	    	        	CsaSeriesSliceArrayConcatenations = global_const.getInt("CsaSeries.SliceArrayConcatenations");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.SliceArrayConcatenations not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesSliceArrayConcatenations != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesSliceArrayConcatenations(CsaSeriesSliceArrayConcatenations);
	    	        }
	    	        try {
	    	        	CsaSeriesFlowCompensation = global_const.getString("CsaSeries.FlowCompensation");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.FlowCompensation not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesFlowCompensation != null) {
	    	        	fileInfo.setCsaSeriesFlowCompensation(CsaSeriesFlowCompensation);
	    	        }
	    	        try {
	    	        	CsaSeriesTransmitterCalibration = global_const.getDouble("CsaSeries.TransmitterCalibration");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.TransmitterCalibration not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesTransmitterCalibration)) {
	    	        	fileInfo.setCsaSeriesTransmitterCalibration(CsaSeriesTransmitterCalibration);
	    	        }
	    	        try {
	    	        	CsaSeriesIsocentered = global_const.getInt("CsaSeries.Isocentered");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.Isocentered not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesIsocentered != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesIsocentered(CsaSeriesIsocentered);
	    	        }
	    	        try {
	    	        	CsaSeriesAbsTablePosition = global_const.getInt("CsaSeries.AbsTablePosition");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.AbsTablePosition not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesAbsTablePosition != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesAbsTablePosition(CsaSeriesAbsTablePosition);
	    	        }
	    	        try {
	    	        	CsaSeriesReadoutOS = global_const.getDouble("CsaSeries.ReadoutOS");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.ReadoutOS not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesReadoutOS)) {
	    	        	fileInfo.setCsaSeriesReadoutOS(CsaSeriesReadoutOS);
	    	        }
	    	        try {
	    	        	CsaSeriesdBdt_max = global_const.getDouble("CsaSeries.dBdt_max");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.dBdt_max not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesdBdt_max)) {
	    	        	fileInfo.setCsaSeriesdBdt_max(CsaSeriesdBdt_max);
	    	        }
	    	        try {
	    	        	CsaSeriesRFSWDOperationMode = global_const.getInt("CsaSeries.RFSWDOperationMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.RFSWDOperationMode not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesRFSWDOperationMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesRFSWDOperationMode(CsaSeriesRFSWDOperationMode);
	    	        }
	    	        try {
	    	        	CsaSeriesSelectionGradientAmplitude = global_const.getDouble("CsaSeries.SelectionGradientAmplitude");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.SelectionGradientAmplitude not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesSelectionGradientAmplitude)) {
	    	        	fileInfo.setCsaSeriesSelectionGradientAmplitude(CsaSeriesSelectionGradientAmplitude);
	    	        }
	    	        try {
	    	        	CsaSeriesPhaseGradientAmplitude = global_const.getDouble("CsaSeries.PhaseGradientAmplitude");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.PhaseGradientAmplitude not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesPhaseGradientAmplitude)) {
	    	        	fileInfo.setCsaSeriesPhaseGradientAmplitude(CsaSeriesPhaseGradientAmplitude);
	    	        }
	    	        try {
	    	        	CsaSeriesRfWatchdogMask = global_const.getInt("CsaSeries.RfWatchdogMask");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.RfWatchdogMask not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesRfWatchdogMask != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesRfWatchdogMask(CsaSeriesRfWatchdogMask);
	    	        }
	    	        try {
	    	        	CsaSeriesCoilForGradient2 = global_const.getString("CsaSeries.CoilForGradient2");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.CoilForGradient2 not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesCoilForGradient2 != null) {
	    	        	fileInfo.setCsaSeriesCoilForGradient2(CsaSeriesCoilForGradient2);
	    	        }
	    	        try {
	    	        	CsaSeriesStim_mon_mode = global_const.getInt("CsaSeries.Stim_mon_mode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.Stim_mon_mode not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesStim_mon_mode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesStim_mon_mode(CsaSeriesStim_mon_mode);
	    	        }
	    	        try {
	    	        	CsaSeriesCoilId = global_const.getJSONArray("CsaSeries.CoilId");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("JSONArray CsaSeries.CoilId not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesCoilId != null) {
	    	            CsaSeriesCoilIdInt = new int[CsaSeriesCoilId.length()];
	    	            for (i = 0; i < CsaSeriesCoilId.length(); i++) {
	    	            	try {
	    	            		CsaSeriesCoilIdInt[i] = CsaSeriesCoilId.getInt(i);
	    	            	}
	    	            	catch (JSONException e) {
	    	            		Preferences.debug("CsaSeriesCoilId.getInt("+i+") not found\n", Preferences.DEBUG_FILEIO);
	    	            	}
	    	            }
	    	            if (CsaSeriesCoilIdInt != null) {
	    	            	fileInfo.setCsaSeriesCoilId(CsaSeriesCoilIdInt);
	    	            }
	    	        } // if (CsaSeriesCoilId != null)
	    	        try {
	    	        	CsaSeriesStim_max_ges_norm_online = global_const.getDouble("CsaSeries.Stim_max_ges_norm_online");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.Stim_max_ges_norm_online not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesStim_max_ges_norm_online)) {
	    	        	fileInfo.setCsaSeriesStim_max_ges_norm_online(CsaSeriesStim_max_ges_norm_online);
	    	        }
	    	        try {
	    	        	CsaSeriesCoilString = global_const.getString("CsaSeries.CoilString");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.CoilString not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesCoilString != null) {
	    	        	fileInfo.setCsaSeriesCoilString(CsaSeriesCoilString);
	    	        }
	    	        try {
	    	        	CsaSeriesCoilForGradient = global_const.getString("CsaSeries.CoilForGradient");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.CoilForGradient not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesCoilForGradient != null) {
	    	        	fileInfo.setCsaSeriesCoilForGradient(CsaSeriesCoilForGradient);
	    	        }
	    	        try {
	    	        	CsaSeriesTablePositionOrigin = global_const.getJSONArray("CsaSeries.TablePositionOrigin");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("JSONArray CsaSeries.TablePositionOrigin not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesTablePositionOrigin != null) {
	    	            CsaSeriesTablePositionOriginInt = new int[CsaSeriesTablePositionOrigin.length()];
	    	            for (i = 0; i < CsaSeriesTablePositionOrigin.length(); i++) {
	    	            	try {
	    	            		CsaSeriesTablePositionOriginInt[i] = CsaSeriesTablePositionOrigin.getInt(i);
	    	            	}
	    	            	catch (JSONException e) {
	    	            		Preferences.debug("CsaSeriesTablePositionOrigin.getInt("+i+") not found\n", Preferences.DEBUG_FILEIO);
	    	            	}
	    	            }
	    	            if (CsaSeriesTablePositionOriginInt != null) {
	    	            	fileInfo.setCsaSeriesTablePositionOrigin(CsaSeriesTablePositionOriginInt);
	    	            }
	    	        } // if (CsaSeriesTablePositionOrigin != null)
	    	        try {
	    	        	CsaSeriesMiscSequenceParam = global_const.getJSONArray("CsaSeries.MiscSequenceParam");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("JSONArray CsaSeries.MiscSequenceParam not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMiscSequenceParam != null) {
	    	            CsaSeriesMiscSequenceParamInt = new int[CsaSeriesMiscSequenceParam.length()];
	    	            for (i = 0; i < CsaSeriesMiscSequenceParam.length(); i++) {
	    	            	try {
	    	            		CsaSeriesMiscSequenceParamInt[i] = CsaSeriesMiscSequenceParam.getInt(i);
	    	            	}
	    	            	catch (JSONException e) {
	    	            		Preferences.debug("CsaSeriesMiscSequenceParam.getInt("+i+") not found\n", Preferences.DEBUG_FILEIO);
	    	            	}
	    	            }
	    	            if (CsaSeriesMiscSequenceParamInt != null) {
	    	            	fileInfo.setCsaSeriesMiscSequenceParam(CsaSeriesMiscSequenceParamInt);
	    	            }
	    	        } // if (CsaSeriesMiscSequenceParam != null)
	    	        try {
	    	        	CsaSeriesLongModelName = global_const.getString("CsaSeries.LongModelName");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.LongModelName not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesLongModelName != null) {
	    	        	fileInfo.setCsaSeriesLongModelName(CsaSeriesLongModelName);
	    	        }
	    	        try {
	    	        	CsaSeriesStim_faktor = global_const.getDouble("CsaSeries.Stim_faktor");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.Stim_faktor not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesStim_faktor)) {
	    	        	fileInfo.setCsaSeriesStim_faktor(CsaSeriesStim_faktor);
	    	        }
			        try {
			        	CsaSeriesSW_korr_faktor = global_const.getDouble("CsaSeries.SW_korr_faktor");
			        }
			        catch (JSONException e) {
			        	Preferences.debug("CsaSeries.SW_korr_faktor not found\n", Preferences.DEBUG_FILEIO);
			        }
			        if (!Double.isNaN(CsaSeriesSW_korr_faktor)) {
			        	fileInfo.setCsaSeriesSW_korr_faktor(CsaSeriesSW_korr_faktor);
			        }
			        try {
	    	        	CsaSeriesSed = global_const.getJSONArray("CsaSeries.Sed");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("JSONArray CsaSeries.Sed not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesSed != null) {
	    	            CsaSeriesSedDouble = new double[CsaSeriesSed.length()];
	    	            for (i = 0; i < CsaSeriesSed.length(); i++) {
	    	            	try {
	    	            		CsaSeriesSedDouble[i] = CsaSeriesSed.getDouble(i);
	    	            	}
	    	            	catch (JSONException e) {
	    	            		Preferences.debug("CsaSeriesSed.getDouble("+i+") not found\n", Preferences.DEBUG_FILEIO);
	    	            	}
	    	            }
	    	            if (CsaSeriesSedDouble != null) {
	    	            	fileInfo.setCsaSeriesSed(CsaSeriesSedDouble);
	    	            }
	    	        } // if (CsaSeriesSed != null)
	    	        try {
	    	        	CsaSeriesPositivePCSDirections = global_const.getString("CsaSeries.PositivePCSDirections");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.PositivePCSDirections not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesPositivePCSDirections != null) {
	    	        	fileInfo.setCsaSeriesPositivePCSDirections(CsaSeriesPositivePCSDirections);
	    	        }
	    	        try {
	    	        	CsaSeriesSliceResolution = global_const.getDouble("CsaSeries.SliceResolution");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.SliceResolution not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesSliceResolution)) {
	    	        	fileInfo.setCsaSeriesSliceResolution(CsaSeriesSliceResolution);
	    	        }
	    	        try {
	    	        	CsaSeriesStim_max_online = global_const.getJSONArray("CsaSeries.Stim_max_online");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("JSONArray CsaSeries.Stim_max_online not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesStim_max_online != null) {
	    	            CsaSeriesStim_max_onlineDouble = new double[CsaSeriesStim_max_online.length()];
	    	            for (i = 0; i < CsaSeriesStim_max_online.length(); i++) {
	    	            	try {
	    	            		CsaSeriesStim_max_onlineDouble[i] = CsaSeriesStim_max_online.getDouble(i);
	    	            	}
	    	            	catch (JSONException e) {
	    	            		Preferences.debug("CsaSeriesStim_max_online.getDouble("+i+") not found\n", Preferences.DEBUG_FILEIO);
	    	            	}
	    	            }
	    	            if (CsaSeriesStim_max_onlineDouble != null) {
	    	            	fileInfo.setCsaSeriesStim_max_online(CsaSeriesStim_max_onlineDouble);
	    	            }
	    	        } // if (CsaSeriesStim_max_online != null)
	    	        if (!Double.isNaN(CsaSeriest_puls_max)) {
	    	        	fileInfo.setCsaSeriest_puls_max(CsaSeriest_puls_max);
	    	        }
	    	        try {
	    	        	CsaSeriest_puls_max = global_const.getDouble("CsaSeries.t_puls_max");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.t_puls_max not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriest_puls_max)) {
	    	        	fileInfo.setCsaSeriest_puls_max(CsaSeriest_puls_max);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolulVersion = global_const.getInt("CsaSeries.MrPhoenixProtocol.ulVersion");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.ulVersion not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolulVersion != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolulVersion(CsaSeriesMrPhoenixProtocolulVersion);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocoltSequenceFileName = global_const.getString("CsaSeries.MrPhoenixProtocol.tSequenceFileName");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.tSequenceFileName not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocoltSequenceFileName != null) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocoltSequenceFileName(CsaSeriesMrPhoenixProtocoltSequenceFileName);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocoltProtocolName = global_const.getString("CsaSeries.MrPhoenixProtocol.tProtocolName");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.tProtocolName not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocoltProtocolName != null) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocoltProtocolName(CsaSeriesMrPhoenixProtocoltProtocolName);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocoltReferenceImage0 = global_const.getString("CsaSeries.MrPhoenixProtocol.tReferenceImage0");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.tReferenceImage0 not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocoltReferenceImage0 != null) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocoltReferenceImage0(CsaSeriesMrPhoenixProtocoltReferenceImage0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocoltReferenceImage1 = global_const.getString("CsaSeries.MrPhoenixProtocol.tReferenceImage1");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.tReferenceImage1 not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocoltReferenceImage1 != null) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocoltReferenceImage1(CsaSeriesMrPhoenixProtocoltReferenceImage1);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocoltReferenceImage2 = global_const.getString("CsaSeries.MrPhoenixProtocol.tReferenceImage2");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.tReferenceImage2 not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocoltReferenceImage2 != null) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocoltReferenceImage2(CsaSeriesMrPhoenixProtocoltReferenceImage2);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolucScanRegionPosValid = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.ucScanRegionPosValid");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.ucScanRegionPosValid not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolucScanRegionPosValid != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolucScanRegionPosValid(CsaSeriesMrPhoenixProtocolucScanRegionPosValid);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolucTablePositioningMode = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.ucTablePositioningMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.ucTablePositioningMode not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolucTablePositioningMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolucTablePositioningMode(CsaSeriesMrPhoenixProtocolucTablePositioningMode);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsProtConsistencyInfotBaselineString = 
	    	        			global_const.getString("CsaSeries.MrPhoenixProtocol.sProtConsistencyInfo.tBaselineString");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sProtConsistencyInfo.tBaselineString not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsProtConsistencyInfotBaselineString != null) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsProtConsistencyInfotBaselineString(
	    	        			CsaSeriesMrPhoenixProtocolsProtConsistencyInfotBaselineString);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsProtConsistencyInfotSystemType = 
	    	        			global_const.getString("CsaSeries.MrPhoenixProtocol.sProtConsistencyInfo.tSystemType");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sProtConsistencyInfo.tSystemType not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsProtConsistencyInfotSystemType != null) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsProtConsistencyInfotSystemType(
	    	        			CsaSeriesMrPhoenixProtocolsProtConsistencyInfotSystemType);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflNominalB0 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sProtConsistencyInfo.flNominalB0");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sProtConsistencyInfo.flNominalB0 not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflNominalB0)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsProtConsistencyInfoflNominalB0(
	    	        			CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflNominalB0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflGMax = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sProtConsistencyInfo.flGMax");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sProtConsistencyInfo.flGMax not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflGMax)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsProtConsistencyInfoflGMax(
	    	        			CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflGMax);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflRiseTime = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sProtConsistencyInfo.flRiseTime");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sProtConsistencyInfo.flRiseTime not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflRiseTime)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsProtConsistencyInfoflRiseTime(
	    	        			CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflRiseTime);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsProtConsistencyInfolMaximumNofRxReceiverChannels = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sProtConsistencyInfo.lMaximumNofRxReceiverChannels");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sProtConsistencyInfo.lMaximumNofRxReceiverChannels not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsProtConsistencyInfolMaximumNofRxReceiverChannels != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsProtConsistencyInfolMaximumNofRxReceiverChannels(
	    	        			CsaSeriesMrPhoenixProtocolsProtConsistencyInfolMaximumNofRxReceiverChannels);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude0 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationX.aflAmplitude[0]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationX.aflAmplitude[0] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude0)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude0(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude1 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationX.aflAmplitude[1]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationX.aflAmplitude[1] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude1)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude1(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude1);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude2 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationX.aflAmplitude[2]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationX.aflAmplitude[2] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude2)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude2(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude2);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude3 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationX.aflAmplitude[3]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationX.aflAmplitude[3] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude3)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude3(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude3);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude4 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationX.aflAmplitude[4]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationX.aflAmplitude[4] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude4)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude4(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude4);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant0 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationX.aflTimeConstant[0]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationX.aflTimeConstant[0] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant0)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant0(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant1 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationX.aflTimeConstant[1]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationX.aflTimeConstant[1] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant1)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant1(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant1);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant2 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationX.aflTimeConstant[2]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationX.aflTimeConstant[2] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant2)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant2(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant2);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant3 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationX.aflTimeConstant[3]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationX.aflTimeConstant[3] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant3)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant3(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant3);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant4 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationX.aflTimeConstant[4]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationX.aflTimeConstant[4] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant4)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant4(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant4);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude0 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationY.aflAmplitude[0]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationY.aflAmplitude[0] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude0)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude0(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude1 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationY.aflAmplitude[1]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationY.aflAmplitude[1] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude1)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude1(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude1);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude2 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationY.aflAmplitude[2]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationY.aflAmplitude[2] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude2)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude2(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude2);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude3 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationY.aflAmplitude[3]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationY.aflAmplitude[3] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude3)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude3(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude3);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude4 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationY.aflAmplitude[4]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationY.aflAmplitude[4] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude4)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude4(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude4);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant0 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationY.aflTimeConstant[0]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationY.aflTimeConstant[0] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant0)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant0(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant1 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationY.aflTimeConstant[1]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationY.aflTimeConstant[1] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant1)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant1(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant1);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant2 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationY.aflTimeConstant[2]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationY.aflTimeConstant[2] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant2)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant2(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant2);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant3 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationY.aflTimeConstant[3]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationY.aflTimeConstant[3] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant3)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant3(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant3);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant4 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationY.aflTimeConstant[4]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationY.aflTimeConstant[4] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant4)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant4(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant4);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude0 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationZ.aflAmplitude[0]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationZ.aflAmplitude[0] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude0)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude0(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude1 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationZ.aflAmplitude[1]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationZ.aflAmplitude[1] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude1)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude1(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude1);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude2 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationZ.aflAmplitude[2]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationZ.aflAmplitude[2] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude2)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude2(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude2);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude3 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationZ.aflAmplitude[3]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationZ.aflAmplitude[3] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude3)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude3(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude3);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude4 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationZ.aflAmplitude[4]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationZ.aflAmplitude[4] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude4)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude4(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude4);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant0 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationZ.aflTimeConstant[0]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationZ.aflTimeConstant[0] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant0)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant0(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant1 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationZ.aflTimeConstant[1]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationZ.aflTimeConstant[1] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant1)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant1(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant1);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant2 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationZ.aflTimeConstant[2]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationZ.aflTimeConstant[2] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant2)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant2(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant2);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant3 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationZ.aflTimeConstant[3]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationZ.aflTimeConstant[3] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant3)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant3(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant3);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant4 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationZ.aflTimeConstant[4]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationZ.aflTimeConstant[4] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant4)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant4(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant4);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECbEddyCompensationValid = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sGRADSPEC.bEddyCompensationValid");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.bEddyCompensationValid not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsGRADSPECbEddyCompensationValid != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECbEddyCompensationValid(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECbEddyCompensationValid);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude0 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationX.aflAmplitude[0]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationX.aflAmplitude[0] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude0)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude0(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude1 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationX.aflAmplitude[1]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationX.aflAmplitude[1] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude1)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude1(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude1);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude2 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationX.aflAmplitude[2]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationX.aflAmplitude[2] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude2)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude2(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude2);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant0 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationX.aflTimeConstant[0]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationX.aflTimeConstant[0] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant0)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant0(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant1 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationX.aflTimeConstant[1]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationX.aflTimeConstant[1] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant1)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant1(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant1);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant2 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationX.aflTimeConstant[2]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationX.aflTimeConstant[2] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant2)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant2(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant2);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude0 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationY.aflAmplitude[0]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationY.aflAmplitude[0] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude0)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude0(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude1 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationY.aflAmplitude[1]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationY.aflAmplitude[1] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude1)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude1(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude1);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude2 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationY.aflAmplitude[2]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationY.aflAmplitude[2] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude2)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude2(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude2);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant0 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationY.aflTimeConstant[0]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationY.aflTimeConstant[0] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant0)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant0(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant1 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationY.aflTimeConstant[1]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationY.aflTimeConstant[1] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant1)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant1(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant1);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant2 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationY.aflTimeConstant[2]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationY.aflTimeConstant[2] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant2)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant2(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant2);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude0 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationZ.aflAmplitude[0]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationZ.aflAmplitude[0] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude0)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude0(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude1 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationZ.aflAmplitude[1]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationZ.aflAmplitude[1] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude1)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude1(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude1);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude2 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationZ.aflAmplitude[2]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationZ.aflAmplitude[2] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude2)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude2(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude2);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant0 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationZ.aflTimeConstant[0]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationZ.aflTimeConstant[0] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant0)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant0(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant1 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationZ.aflTimeConstant[1]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationZ.aflTimeConstant[1] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant1)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant1(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant1);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant2 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationZ.aflTimeConstant[2]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationZ.aflTimeConstant[2] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant2)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant2(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant2);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECbB0CompensationValid = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sGRADSPEC.bB0CompensationValid");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.bB0CompensationValid not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsGRADSPECbB0CompensationValid != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECbB0CompensationValid(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECbB0CompensationValid);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXYaflAmplitude0 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationXY.aflAmplitude[0]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationXY.aflAmplitude[0] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXYaflAmplitude0)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXYaflAmplitude0(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXYaflAmplitude0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXYaflTimeConstant0 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationXY.aflTimeConstant[0]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationXY.aflTimeConstant[0] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXYaflTimeConstant0)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXYaflTimeConstant0(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXYaflTimeConstant0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXZaflAmplitude0 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationXZ.aflAmplitude[0]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationXZ.aflAmplitude[0] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXZaflAmplitude0)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXZaflAmplitude0(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXZaflAmplitude0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXZaflTimeConstant0 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationXZ.aflTimeConstant[0]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationXZ.aflTimeConstant[0] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXZaflTimeConstant0)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXZaflTimeConstant0(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXZaflTimeConstant0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYXaflAmplitude0 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationYX.aflAmplitude[0]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationYX.aflAmplitude[0] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYXaflAmplitude0)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYXaflAmplitude0(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYXaflAmplitude0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYXaflTimeConstant0 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationYX.aflTimeConstant[0]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationYX.aflTimeConstant[0] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYXaflTimeConstant0)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYXaflTimeConstant0(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYXaflTimeConstant0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYZaflAmplitude0 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationYZ.aflAmplitude[0]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationYZ.aflAmplitude[0] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYZaflAmplitude0)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYZaflAmplitude0(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYZaflAmplitude0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYZaflTimeConstant0 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationYZ.aflTimeConstant[0]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationYZ.aflTimeConstant[0] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYZaflTimeConstant0)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYZaflTimeConstant0(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYZaflTimeConstant0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZXaflAmplitude0 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationZX.aflAmplitude[0]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationZX.aflAmplitude[0] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZXaflAmplitude0)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZXaflAmplitude0(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZXaflAmplitude0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZXaflTimeConstant0 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationZX.aflTimeConstant[0]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationZX.aflTimeConstant[0] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZXaflTimeConstant0)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZXaflTimeConstant0(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZXaflTimeConstant0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZYaflAmplitude0 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationZY.aflAmplitude[0]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationZY.aflAmplitude[0] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZYaflAmplitude0)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZYaflAmplitude0(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZYaflAmplitude0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZYaflTimeConstant0 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationZY.aflTimeConstant[0]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationZY.aflTimeConstant[0] not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZYaflTimeConstant0)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZYaflTimeConstant0(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZYaflTimeConstant0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECbCrossTermCompensationValid = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sGRADSPEC.bCrossTermCompensationValid");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.bCrossTermCompensationValid not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsGRADSPECbCrossTermCompensationValid != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECbCrossTermCompensationValid(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECbCrossTermCompensationValid);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetX = global_const.getInt("CsaSeries.MrPhoenixProtocol.sGRADSPEC.lOffsetX");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.lOffsetX not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetX != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPEClOffsetX(CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetX);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetY = global_const.getInt("CsaSeries.MrPhoenixProtocol.sGRADSPEC.lOffsetY");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.lOffsetY not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetY != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPEClOffsetY(CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetY);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetZ = global_const.getInt("CsaSeries.MrPhoenixProtocol.sGRADSPEC.lOffsetZ");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.lOffsetZ not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetZ != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPEClOffsetZ(CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetZ);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECbOffsetValid = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sGRADSPEC.bOffsetValid");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.bOffsetValid not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsGRADSPECbOffsetValid != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECbOffsetValid(CsaSeriesMrPhoenixProtocolsGRADSPECbOffsetValid);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPEClDelayX = global_const.getInt("CsaSeries.MrPhoenixProtocol.sGRADSPEC.lDelayX");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.lDelayX not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsGRADSPEClDelayX != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPEClDelayX(CsaSeriesMrPhoenixProtocolsGRADSPEClDelayX);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPEClDelayY = global_const.getInt("CsaSeries.MrPhoenixProtocol.sGRADSPEC.lDelayY");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.lDelayY not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsGRADSPEClDelayY != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPEClDelayY(CsaSeriesMrPhoenixProtocolsGRADSPEClDelayY);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPEClDelayZ = global_const.getInt("CsaSeries.MrPhoenixProtocol.sGRADSPEC.lDelayZ");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.lDelayZ not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsGRADSPEClDelayZ != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPEClDelayZ(CsaSeriesMrPhoenixProtocolsGRADSPEClDelayZ);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECbDelayValid = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sGRADSPEC.bDelayValid");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.bDelayValid not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsGRADSPECbDelayValid != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECbDelayValid(CsaSeriesMrPhoenixProtocolsGRADSPECbDelayValid);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityX = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.flSensitivityX");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.flSensitivityX not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityX)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityX(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityX);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityY = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.flSensitivityY");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.flSensitivityY not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityY)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityY(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityY);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityZ = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.flSensitivityZ");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.flSensitivityZ not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityZ)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityZ(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityZ);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECbSensitivityValid = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sGRADSPEC.bSensitivityValid");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.bSensitivityValid not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsGRADSPECbSensitivityValid != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECbSensitivityValid(CsaSeriesMrPhoenixProtocolsGRADSPECbSensitivityValid);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECflGSWDMinRiseTime =
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGRADSPEC.flGSWDMinRiseTime");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.flGSWDMinRiseTime not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECflGSWDMinRiseTime)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECflGSWDMinRiseTime(
	    	        			CsaSeriesMrPhoenixProtocolsGRADSPECflGSWDMinRiseTime);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent0 = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sGRADSPEC.alShimCurrent[0]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.alShimCurrent[0] not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent0 != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent0(CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent1 = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sGRADSPEC.alShimCurrent[1]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.alShimCurrent[1] not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent1 != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent1(CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent1);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent2 = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sGRADSPEC.alShimCurrent[2]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.alShimCurrent[2] not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent2 != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent2(CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent2);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent3 = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sGRADSPEC.alShimCurrent[3]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.alShimCurrent[3] not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent3 != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent3(CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent3);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent4 = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sGRADSPEC.alShimCurrent[4]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.alShimCurrent[4] not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent4 != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent4(CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent4);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECbShimCurrentValid = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sGRADSPEC.bShimCurrentValid");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.bShimCurrentValid not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsGRADSPECbShimCurrentValid != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECbShimCurrentValid(CsaSeriesMrPhoenixProtocolsGRADSPECbShimCurrentValid);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGRADSPECucMode = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sGRADSPEC.ucMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGRADSPEC.ucMode not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsGRADSPECucMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGRADSPECucMode(CsaSeriesMrPhoenixProtocolsGRADSPECucMode);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0tNucleus = 
	    	        			global_const.getString("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[0].tNucleus");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[0].tNucleus not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0tNucleus != null) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0tNucleus(
	    	        			CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0tNucleus);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0lFrequency = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[0].lFrequency");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[0].lFrequency not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0lFrequency != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0lFrequency(
	    	        			CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0lFrequency);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bFrequencyValid = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[0].bFrequencyValid");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[0].bFrequencyValid not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bFrequencyValid != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bFrequencyValid(
	    	        			CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bFrequencyValid);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0flReferenceAmplitude = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[0].flReferenceAmplitude");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[0].flReferenceAmplitude not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0flReferenceAmplitude)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0flReferenceAmplitude(
	    	        			CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0flReferenceAmplitude);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bReferenceAmplitudeValid = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[0].bReferenceAmplitudeValid");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[0].bReferenceAmplitudeValid not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bReferenceAmplitudeValid != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bReferenceAmplitudeValid(
	    	        			CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bReferenceAmplitudeValid);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0flAmplitudeCorrection = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[0].flAmplitudeCorrection");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[0].flAmplitudeCorrection not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0flAmplitudeCorrection)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0flAmplitudeCorrection(
	    	        			CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0flAmplitudeCorrection);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bAmplitudeCorrectionValid = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[0].bAmplitudeCorrectionValid");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[0].bAmplitudeCorrectionValid not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bAmplitudeCorrectionValid != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bAmplitudeCorrectionValid(
	    	        			CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bAmplitudeCorrectionValid);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bRFPAIndexValid = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[0].bRFPAIndexValid");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[0].bRFPAIndexValid not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bRFPAIndexValid != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bRFPAIndexValid(
	    	        			CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bRFPAIndexValid);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bFrequencyValid = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[1].bFrequencyValid");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[1].bFrequencyValid not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bFrequencyValid != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bFrequencyValid(
	    	        			CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bFrequencyValid);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bReferenceAmplitudeValid = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[1].bReferenceAmplitudeValid");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[1].bReferenceAmplitudeValid not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bReferenceAmplitudeValid != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bReferenceAmplitudeValid(
	    	        			CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bReferenceAmplitudeValid);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1flAmplitudeCorrection = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[1].flAmplitudeCorrection");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[1].flAmplitudeCorrection not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1flAmplitudeCorrection)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1flAmplitudeCorrection(
	    	        			CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1flAmplitudeCorrection);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bAmplitudeCorrectionValid = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[1].bAmplitudeCorrectionValid");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[1].bAmplitudeCorrectionValid not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bAmplitudeCorrectionValid != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bAmplitudeCorrectionValid(
	    	        			CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bAmplitudeCorrectionValid);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1lRFPAIndex = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[1].lRFPAIndex");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[1].lRFPAIndex not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1lRFPAIndex != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1lRFPAIndex(
	    	        			CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1lRFPAIndex);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bRFPAIndexValid = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[1].bRFPAIndexValid");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[1].bRFPAIndexValid not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bRFPAIndexValid != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bRFPAIndexValid(
	    	        			CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bRFPAIndexValid);
	    	        }
	    	        for (i = 0; i < 2; i++) {
	    	        	CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEntName[i] = null;
		    	        try {
		    	        	CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEntName[i] = 
		    	        			global_const.getString("CsaSeries.MrPhoenixProtocol.sTXSPEC.aRFPULSE["+i+"].tName");
		    	        }
		    	        catch (JSONException e) {
		    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sTXSPEC.aRFPULSE["+i+"].tName not found\n",
		    	        			Preferences.DEBUG_FILEIO);
		    	        }
		    	        CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnbAmplitudeValid[i] = Integer.MIN_VALUE; 
		    	        try {
		    	        	CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnbAmplitudeValid[i] = 
		    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sTXSPEC.aRFPULSE["+i+"].bAmplitudeValid");
		    	        }
		    	        catch (JSONException e) {
		    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sTXSPEC.aRFPULSE["+i+"].bAmplitudeValid not found\n",
		    	        			Preferences.DEBUG_FILEIO);
		    	        }
		    	        CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnflAmplitude[i] = Double.NaN;
		    	        try {
		    	        	CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnflAmplitude[i] = 
		    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sTXSPEC.aRFPULSE["+i+"].flAmplitude");
		    	        }
		    	        catch (JSONException e) {
		    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sTXSPEC.aRFPULSE["+i+"].flAmplitude not found\n",
		    	        			Preferences.DEBUG_FILEIO);
		    	        }
	    	        } // for (i = 0; i < 2; i++)
	    	        if (CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEntName != null) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEntName(
	    	        			CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEntName);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnbAmplitudeValid != null) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnbAmplitudeValid(
	    	        			CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnbAmplitudeValid);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnflAmplitude != null) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnflAmplitude(
	    	        			CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnflAmplitude);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsTXSPEClNoOfTraPulses = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sTXSPEC.lNoOfTraPulses");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sTXSPEC.lNoOfTraPulses not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsTXSPEClNoOfTraPulses != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsTXSPEClNoOfTraPulses(CsaSeriesMrPhoenixProtocolsTXSPEClNoOfTraPulses);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsTXSPEClBCExcitationMode = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sTXSPEC.lBCExcitationMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sTXSPEC.lBCExcitationMode not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsTXSPEClBCExcitationMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsTXSPEClBCExcitationMode(CsaSeriesMrPhoenixProtocolsTXSPEClBCExcitationMode);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsTXSPEClBCSeqExcitationMode = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sTXSPEC.lBCSeqExcitationMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sTXSPEC.lBCSeqExcitationMode not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsTXSPEClBCSeqExcitationMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsTXSPEClBCSeqExcitationMode(
	    	        			CsaSeriesMrPhoenixProtocolsTXSPEClBCSeqExcitationMode);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeMin = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sTXSPEC.flKDynMagnitudeMin");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sTXSPEC.flKDynMagnitudeMin not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeMin)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeMin(
	    	        			CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeMin);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeMax = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sTXSPEC.flKDynMagnitudeMax");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sTXSPEC.flKDynMagnitudeMax not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeMax)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeMax(
	    	        			CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeMax);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeClipLow = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sTXSPEC.flKDynMagnitudeClipLow");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sTXSPEC.flKDynMagnitudeClipLow not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeClipLow)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeClipLow(
	    	        			CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeClipLow);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeClipHigh = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sTXSPEC.flKDynMagnitudeClipHigh");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sTXSPEC.flKDynMagnitudeClipHigh not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeClipHigh)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeClipHigh(
	    	        			CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeClipHigh);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsTXSPECflKDynPhaseMax = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sTXSPEC.flKDynPhaseMax");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sTXSPEC.flKDynPhaseMax not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsTXSPECflKDynPhaseMax)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsTXSPECflKDynPhaseMax(
	    	        			CsaSeriesMrPhoenixProtocolsTXSPECflKDynPhaseMax);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsTXSPECflKDynPhaseClip = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sTXSPEC.flKDynPhaseClip");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sTXSPEC.flKDynPhaseClip not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsTXSPECflKDynPhaseClip)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsTXSPECflKDynPhaseClip(
	    	        			CsaSeriesMrPhoenixProtocolsTXSPECflKDynPhaseClip);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsTXSPECbKDynValid = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sTXSPEC.bKDynValid");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sTXSPEC.bKDynValid not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsTXSPECbKDynValid != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsTXSPECbKDynValid(
	    	        			CsaSeriesMrPhoenixProtocolsTXSPECbKDynValid);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsTXSPECucRFPulseType = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sTXSPEC.ucRFPulseType");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sTXSPEC.ucRFPulseType not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsTXSPECucRFPulseType != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsTXSPECucRFPulseType(
	    	        			CsaSeriesMrPhoenixProtocolsTXSPECucRFPulseType);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsTXSPECucExcitMode = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sTXSPEC.ucExcitMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sTXSPEC.ucExcitMode not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsTXSPECucExcitMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsTXSPECucExcitMode(
	    	        			CsaSeriesMrPhoenixProtocolsTXSPECucExcitMode);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsTXSPECucSimultaneousExcitation = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sTXSPEC.ucSimultaneousExcitation");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sTXSPEC.ucSimultaneousExcitation not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsTXSPECucSimultaneousExcitation != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsTXSPECucSimultaneousExcitation(
	    	        			CsaSeriesMrPhoenixProtocolsTXSPECucSimultaneousExcitation);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsTXSPECucBCExcitationModeValid = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sTXSPEC.ucBCExcitationModeValid");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sTXSPEC.ucBCExcitationModeValid not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsTXSPECucBCExcitationModeValid != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsTXSPECucBCExcitationModeValid(
	    	        			CsaSeriesMrPhoenixProtocolsTXSPECucBCExcitationModeValid);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsRXSPEClGain = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sRXSPEC.lGain");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sRXSPEC.lGain not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsRXSPEClGain != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsRXSPEClGain(CsaSeriesMrPhoenixProtocolsRXSPEClGain);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsRXSPECbGainValid = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sRXSPEC.bGainValid");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sRXSPEC.bGainValid not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsRXSPECbGainValid != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsRXSPECbGainValid(CsaSeriesMrPhoenixProtocolsRXSPECbGainValid);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsRXSPECalDwellTime0 = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sRXSPEC.alDwellTime[0]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sRXSPEC.alDwellTime[0] not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsRXSPECalDwellTime0 != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsRXSPECalDwellTime0(CsaSeriesMrPhoenixProtocolsRXSPECalDwellTime0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsAdjDatauiAdjFreMode = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sAdjData.uiAdjFreMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sAdjData.uiAdjFreMode not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsAdjDatauiAdjFreMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsAdjDatauiAdjFreMode(CsaSeriesMrPhoenixProtocolsAdjDatauiAdjFreMode);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsAdjDatauiAdjShimMode = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sAdjData.uiAdjShimMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sAdjData.uiAdjShimMode not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsAdjDatauiAdjShimMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsAdjDatauiAdjShimMode(CsaSeriesMrPhoenixProtocolsAdjDatauiAdjShimMode);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsAdjDatauiAdjWatSupMode = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sAdjData.uiAdjWatSupMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sAdjData.uiAdjWatSupMode not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsAdjDatauiAdjWatSupMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsAdjDatauiAdjWatSupMode(CsaSeriesMrPhoenixProtocolsAdjDatauiAdjWatSupMode);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsAdjDatauiAdjRFMapMode = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sAdjData.uiAdjRFMapMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sAdjData.uiAdjRFMapMode not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsAdjDatauiAdjRFMapMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsAdjDatauiAdjRFMapMode(CsaSeriesMrPhoenixProtocolsAdjDatauiAdjRFMapMode);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsAdjDatauiAdjMDSMode = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sAdjData.uiAdjMDSMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sAdjData.uiAdjMDSMode not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsAdjDatauiAdjMDSMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsAdjDatauiAdjMDSMode(CsaSeriesMrPhoenixProtocolsAdjDatauiAdjMDSMode);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsAdjDatauiAdjTableTolerance = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sAdjData.uiAdjTableTolerance");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sAdjData.uiAdjTableTolerance not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsAdjDatauiAdjTableTolerance != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsAdjDatauiAdjTableTolerance(
	    	        			CsaSeriesMrPhoenixProtocolsAdjDatauiAdjTableTolerance);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsAdjDatauiAdjProtID = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sAdjData.uiAdjProtID");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sAdjData.uiAdjProtID not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsAdjDatauiAdjProtID != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsAdjDatauiAdjProtID(CsaSeriesMrPhoenixProtocolsAdjDatauiAdjProtID);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsAdjDatauiAdjFreProtRelated = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sAdjData.uiAdjFreProtRelated");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sAdjData.uiAdjFreProtRelated not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsAdjDatauiAdjFreProtRelated != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsAdjDatauiAdjFreProtRelated(
	    	        			CsaSeriesMrPhoenixProtocolsAdjDatauiAdjFreProtRelated);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondSag = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sAdjData.sAdjVolume.sPosition.dSag");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sAdjData.sAdjVolume.sPosition.dSag not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondSag)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondSag(
	    	        			CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondSag);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondCor = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sAdjData.sAdjVolume.sPosition.dCor");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sAdjData.sAdjVolume.sPosition.dCor not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondCor)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondCor(
	    	        			CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondCor);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondTra = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sAdjData.sAdjVolume.sPosition.dTra");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sAdjData.sAdjVolume.sPosition.dTra not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondTra)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondTra(
	    	        			CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondTra);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldSag = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sAdjData.sAdjVolume.sNormal.dSag");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sAdjData.sAdjVolume.sNormal.dSag not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldSag)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldSag(
	    	        			CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldSag);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldCor = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sAdjData.sAdjVolume.sNormal.dCor");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sAdjData.sAdjVolume.sNormal.dCor not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldCor)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldCor(
	    	        			CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldCor);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldTra = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sAdjData.sAdjVolume.sNormal.dTra");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sAdjData.sAdjVolume.sNormal.dTra not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldTra)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldTra(
	    	        			CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldTra);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedThickness = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sAdjData.sAdjVolume.dThickness");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sAdjData.sAdjVolume.dThickness not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedThickness)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedThickness(
	    	        			CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedThickness);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedPhaseFOV = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sAdjData.sAdjVolume.dPhaseFOV");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sAdjData.sAdjVolume.dPhaseFOV not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedPhaseFOV)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedPhaseFOV(
	    	        			CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedPhaseFOV);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedReadoutFOV = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sAdjData.sAdjVolume.dReadoutFOV");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sAdjData.sAdjVolume.dReadoutFOV not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedReadoutFOV)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedReadoutFOV(
	    	        			CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedReadoutFOV);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedInPlaneRot = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sAdjData.sAdjVolume.dInPlaneRot");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sAdjData.sAdjVolume.dInPlaneRot not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedInPlaneRot)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedInPlaneRot(
	    	        			CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedInPlaneRot);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsAdjDatauiAdjVolumeValid = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sAdjData.uiAdjVolumeValid");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sAdjData.uiAdjVolumeValid not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsAdjDatauiAdjVolumeValid != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsAdjDatauiAdjVolumeValid(
	    	        			CsaSeriesMrPhoenixProtocolsAdjDatauiAdjVolumeValid);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolucEnableNoiseAdjust = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.ucEnableNoiseAdjust");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.ucEnableNoiseAdjust not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolucEnableNoiseAdjust != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolucEnableNoiseAdjust(
	    	        			CsaSeriesMrPhoenixProtocolucEnableNoiseAdjust);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolalTR0 = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.alTR[0]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.alTR[0] not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolalTR0 != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolalTR0(CsaSeriesMrPhoenixProtocolalTR0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocollContrasts = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.lContrasts");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.lContrasts not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocollContrasts != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocollContrasts(CsaSeriesMrPhoenixProtocollContrasts);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolalTE0 = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.alTE[0]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.alTE[0] not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolalTE0 != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolalTE0(CsaSeriesMrPhoenixProtocolalTE0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolacFlowComp0 = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.acFlowComp[0]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.acFlowComp[0] not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolacFlowComp0 != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolacFlowComp0(CsaSeriesMrPhoenixProtocolacFlowComp0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocollCombinedEchoes = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.lCombinedEchoes");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.lCombinedEchoes not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocollCombinedEchoes != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocollCombinedEchoes(CsaSeriesMrPhoenixProtocollCombinedEchoes);
	    	        }
	    	        for (i = 0; i < fileInfo.getExtents()[2]; i++) {
	    	        	CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondSag[i] = Double.NaN;
	    	            CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondCor[i] = Double.NaN;
	    	            CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondTra[i] = Double.NaN;
	    	            CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldSag[i] = Double.NaN;
	    	            CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldCor[i] = Double.NaN;
	    	            CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldTra[i] = Double.NaN;
	    	            CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdThickness[i] = Double.NaN;
	    	            CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdPhaseFOV[i] = Double.NaN;
	    	            CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdReadoutFOV[i] = Double.NaN;        
	    	            CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdInPlaneRot[i] = Double.NaN;		
	    	        } // for (i = 0; i < fileInfo.getExtents()[2]; i++)
	    	        for (i = 0; i < fileInfo.getExtents()[2]; i++) {
	    	        	try {
	        	        	CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondSag[i] = 
	        	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sSliceArray.asSlice["+i+"].sPosition.dSag");
	        	        }
	        	        catch (JSONException e) {
	        	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sSliceArray.asSlice["+i+"].sPosition.dSag not found\n", 
	        	        			Preferences.DEBUG_FILEIO);
	        	        }
	    	        	try {
	        	        	CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondCor[i] = 
	        	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sSliceArray.asSlice["+i+"].sPosition.dCor");
	        	        }
	        	        catch (JSONException e) {
	        	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sSliceArray.asSlice["+i+"].sPosition.dCor not found\n", 
	        	        			Preferences.DEBUG_FILEIO);
	        	        }
	    	        	try {
	        	        	CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondTra[i] = 
	        	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sSliceArray.asSlice["+i+"].sPosition.dTra");
	        	        }
	        	        catch (JSONException e) {
	        	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sSliceArray.asSlice["+i+"].sPosition.dTra not found\n", 
	        	        			Preferences.DEBUG_FILEIO);
	        	        }
	    	        	try {
	        	        	CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldSag[i] = 
	        	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sSliceArray.asSlice["+i+"].sNormal.dSag");
	        	        }
	        	        catch (JSONException e) {
	        	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sSliceArray.asSlice["+i+"].sNormal.dSag not found\n", 
	        	        			Preferences.DEBUG_FILEIO);
	        	        }
	    	        	try {
	        	        	CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldCor[i] = 
	        	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sSliceArray.asSlice["+i+"].sNormal.dCor");
	        	        }
	        	        catch (JSONException e) {
	        	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sSliceArray.asSlice["+i+"].sNormal.dCor not found\n", 
	        	        			Preferences.DEBUG_FILEIO);
	        	        }
	    	        	try {
	        	        	CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldTra[i] = 
	        	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sSliceArray.asSlice["+i+"].sNormal.dTra");
	        	        }
	        	        catch (JSONException e) {
	        	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sSliceArray.asSlice["+i+"].sNormal.dTra not found\n", 
	        	        			Preferences.DEBUG_FILEIO);
	        	        }
	    	        	try {
	        	        	CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdThickness[i] = 
	        	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sSliceArray.asSlice["+i+"].dThickness");
	        	        }
	        	        catch (JSONException e) {
	        	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sSliceArray.asSlice["+i+"].dThickness not found\n", 
	        	        			Preferences.DEBUG_FILEIO);
	        	        }
	    	        	try {
	        	        	CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdPhaseFOV[i] = 
	        	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sSliceArray.asSlice["+i+"].dPhaseFOV");
	        	        }
	        	        catch (JSONException e) {
	        	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sSliceArray.asSlice["+i+"].dPhaseFOV not found\n", 
	        	        			Preferences.DEBUG_FILEIO);
	        	        }
	    	        	try {
	        	        	CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdReadoutFOV[i] = 
	        	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sSliceArray.asSlice["+i+"].dReadoutFOV");
	        	        }
	        	        catch (JSONException e) {
	        	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sSliceArray.asSlice["+i+"].dReadoutFOV not found\n", 
	        	        			Preferences.DEBUG_FILEIO);
	        	        }
	    	        	try {
	        	        	CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdInPlaneRot[i] = 
	        	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sSliceArray.asSlice["+i+"].dInPlaneRot");
	        	        }
	        	        catch (JSONException e) {
	        	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sSliceArray.asSlice["+i+"].dInPlaneRot not found\n", 
	        	        			Preferences.DEBUG_FILEIO);
	        	        }
	    	        } // // for (i = 0; i < fileInfo.getExtents()[2]; i++)
		        	fileInfo.setCsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondSag(
		        			CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondSag);
		        	fileInfo.setCsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondCor(
		        			CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondCor);
		        	fileInfo.setCsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondTra(
		        			CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondTra);
		        	fileInfo.setCsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldSag(
		        			CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldSag);
		        	fileInfo.setCsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldCor(
		        			CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldCor);
		        	fileInfo.setCsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldTra(
		        			CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldTra);
		        	fileInfo.setCsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdThickness(
		        			CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdThickness);
		        	fileInfo.setCsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdPhaseFOV(
		        			CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdPhaseFOV);
		        	fileInfo.setCsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdReadoutFOV(
		        			CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdReadoutFOV);
		        	fileInfo.setCsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdInPlaneRot(
		        			CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdInPlaneRot);
		        	for (i = 0; i < fileInfo.getExtents()[2]-1; i++) {
		        		CsaSeriesMrPhoenixProtocolsSliceArrayanAsc[i] = Integer.MIN_VALUE;	
		        	}
		        	for (i = 0; i < fileInfo.getExtents()[2]-1; i++) {
		        		try {
	        	        	CsaSeriesMrPhoenixProtocolsSliceArrayanAsc[i] = 
	        	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sSliceArray.anAsc["+(i+1)+"]");
	        	        }
	        	        catch (JSONException e) {
	        	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sSliceArray.anAsc["+(i+1)+"] not found\n", 
	        	        			Preferences.DEBUG_FILEIO);
	        	        }	
		        	} // for (i = 0; i < fileInfo.getExtents()[2]-1; i++)
		        	fileInfo.setCsaSeriesMrPhoenixProtocolsSliceArrayanAsc(
		        			CsaSeriesMrPhoenixProtocolsSliceArrayanAsc);
		        	for (i = 0; i < fileInfo.getExtents()[2]-1; i++) {
		        		CsaSeriesMrPhoenixProtocolsSliceArrayanPos[i] = Integer.MIN_VALUE;	
		        	}
		        	for (i = 0; i < fileInfo.getExtents()[2]-1; i++) {
		        		try {
	        	        	CsaSeriesMrPhoenixProtocolsSliceArrayanPos[i] = 
	        	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sSliceArray.anPos["+(i+1)+"]");
	        	        }
	        	        catch (JSONException e) {
	        	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sSliceArray.anPos["+(i+1)+"] not found\n", 
	        	        			Preferences.DEBUG_FILEIO);
	        	        }	
		        	} // for (i = 0; i < fileInfo.getExtents()[2]-1; i++)
		        	fileInfo.setCsaSeriesMrPhoenixProtocolsSliceArrayanPos(
		        			CsaSeriesMrPhoenixProtocolsSliceArrayanPos);
		        	try {
	    	        	CsaSeriesMrPhoenixProtocolsSliceArraylSize = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sSliceArray.lSize");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sSliceArray.lSize not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsSliceArraylSize != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsSliceArraylSize(CsaSeriesMrPhoenixProtocolsSliceArraylSize);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsSliceArraylConc = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sSliceArray.lConc");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sSliceArray.lConc not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsSliceArraylConc != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsSliceArraylConc(CsaSeriesMrPhoenixProtocolsSliceArraylConc);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsSliceArrayucMode = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sSliceArray.ucMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sSliceArray.ucMode not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsSliceArrayucMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsSliceArrayucMode(CsaSeriesMrPhoenixProtocolsSliceArrayucMode);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsSliceArraysTSatdThickness = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sSliceArray.sTSat.dThickness");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sSliceArray.sTSat.dThickness not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsSliceArraysTSatdThickness)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsSliceArraysTSatdThickness(CsaSeriesMrPhoenixProtocolsSliceArraysTSatdThickness);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGroupArrayasGroup0nSize = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sGroupArray.asGroup[0].nSize");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGroupArray.asGroup[0].nSize not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsGroupArrayasGroup0nSize != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGroupArrayasGroup0nSize(CsaSeriesMrPhoenixProtocolsGroupArrayasGroup0nSize);
	    	        }
	    	        for (i = 0; i < fileInfo.getExtents()[2]; i++) {
	    	        	CsaSeriesMrPhoenixProtocolsGroupArrayanMember[i] = Integer.MIN_VALUE;
	    	        }
	    	        for (i = 0; i < fileInfo.getExtents()[2]; i++) {
	    	        	try {
	        	        	CsaSeriesMrPhoenixProtocolsGroupArrayanMember[i] = 
	        	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sGroupArray.anMember["+(i+1)+"]");
	        	        }
	        	        catch (JSONException e) {
	        	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGroupArray.anMember["+(i+1)+"] not found\n", 
	        	        			Preferences.DEBUG_FILEIO);
	        	        }		
	    	        }
	    	        fileInfo.setCsaSeriesMrPhoenixProtocolsGroupArrayanMember(
		        			CsaSeriesMrPhoenixProtocolsGroupArrayanMember);
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGroupArraylSize = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sGroupArray.lSize");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGroupArray.lSize not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsGroupArraylSize != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGroupArraylSize(CsaSeriesMrPhoenixProtocolsGroupArraylSize);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGroupArraysPSatdThickness = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGroupArray.sPSat.dThickness");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGroupArray.sPSat.dThickness not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGroupArraysPSatdThickness)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGroupArraysPSatdThickness(CsaSeriesMrPhoenixProtocolsGroupArraysPSatdThickness);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsGroupArraysPSatdGap = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sGroupArray.sPSat.dGap");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sGroupArray.sPSat.dGap not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGroupArraysPSatdGap)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsGroupArraysPSatdGap(CsaSeriesMrPhoenixProtocolsGroupArraysPSatdGap);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondSag =
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sRSatArray.asElm[0].sPosition.dSag");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sRSatArray.asElm[0].sPosition.dSag not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondSag)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondSag(
	    	        			CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondSag);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondCor =
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sRSatArray.asElm[0].sPosition.dCor");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sRSatArray.asElm[0].sPosition.dCor not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondCor)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondCor(
	    	        			CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondCor);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondTra =
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sRSatArray.asElm[0].sPosition.dTra");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sRSatArray.asElm[0].sPosition.dTra not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondTra)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondTra(
	    	        			CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondTra);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldSag =
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sRSatArray.asElm[0].sNormal.dSag");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sRSatArray.asElm[0].sNormal.dSag not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldSag)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldSag(
	    	        			CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldSag);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldCor =
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sRSatArray.asElm[0].sNormal.dCor");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sRSatArray.asElm[0].sNormal.dCor not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldCor)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldCor(
	    	        			CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldCor);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldTra =
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sRSatArray.asElm[0].sNormal.dTra");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sRSatArray.asElm[0].sNormal.dTra not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldTra)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldTra(
	    	        			CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldTra);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondSag =
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sRSatArray.asElm[0].sPosition.dSag");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sRSatArray.asElm[0].sPosition.dSag not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondSag)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondSag(
	    	        			CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondSag);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondCor =
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sRSatArray.asElm[0].sPosition.dCor");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sRSatArray.asElm[0].sPosition.dCor not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondCor)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondCor(
	    	        			CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondCor);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsRSatArrayasElm0dThickness =
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sRSatArray.asElm[0].dThickness");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sRSatArray.asElm[0].dThickness not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsRSatArrayasElm0dThickness)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsRSatArrayasElm0dThickness(
	    	        			CsaSeriesMrPhoenixProtocolsRSatArrayasElm0dThickness);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsRSatArraylSize =
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sRSatArray.lSize");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sRSatArray.lSize not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsRSatArraylSize != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsRSatArraylSize(CsaSeriesMrPhoenixProtocolsRSatArraylSize);
	    	        }
	    	        for (i = 0; i < CsaSeriesMrPhoenixProtocolsAutoAligndAAMatrix.length; i++) {
	    	        	CsaSeriesMrPhoenixProtocolsAutoAligndAAMatrix[i] = Double.NaN;
	    	        }
	    	        for (i = 0; i < CsaSeriesMrPhoenixProtocolsAutoAligndAAMatrix.length; i++) {
	    	        	try {
	        	        	CsaSeriesMrPhoenixProtocolsAutoAligndAAMatrix[i] = 
	        	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sAutoAlign.dAAMatrix["+i+"]");
	        	        }
	        	        catch (JSONException e) {
	        	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sAutoAlign.dAAMatrix["+i+"] not found\n", 
	        	        			Preferences.DEBUG_FILEIO);
	        	        }	
	    	        }
	    	        fileInfo.setCsaSeriesMrPhoenixProtocolsAutoAligndAAMatrix(
		        			CsaSeriesMrPhoenixProtocolsAutoAligndAAMatrix);
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsNavigatorParalBreathHoldMeas = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sNavigatorPara.lBreathHoldMeas");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sNavigatorPara.lBreathHoldMeas not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsNavigatorParalBreathHoldMeas != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsNavigatorParalBreathHoldMeas(
	    	        			CsaSeriesMrPhoenixProtocolsNavigatorParalBreathHoldMeas);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsNavigatorParalRespComp = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sNavigatorPara.lRespComp");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sNavigatorPara.lRespComp not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsNavigatorParalRespComp != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsNavigatorParalRespComp(
	    	        			CsaSeriesMrPhoenixProtocolsNavigatorParalRespComp);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsBladeParadBladeCoverage = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sBladePara.dBladeCoverage");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sBladePara.dBladeCoverage not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsBladeParadBladeCoverage)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsBladeParadBladeCoverage(
	    	        			CsaSeriesMrPhoenixProtocolsBladeParadBladeCoverage);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsBladeParaucMotionCorr = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sBladePara.ucMotionCorr");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sBladePara.ucMotionCorr not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsBladeParaucMotionCorr != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsBladeParaucMotionCorr(
	    	        			CsaSeriesMrPhoenixProtocolsBladeParaucMotionCorr);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPrepPulsesucFatSat = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPrepPulses.ucFatSat");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPrepPulses.ucFatSat not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPrepPulsesucFatSat != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPrepPulsesucFatSat(CsaSeriesMrPhoenixProtocolsPrepPulsesucFatSat);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPrepPulsesucWaterSat = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPrepPulses.ucWaterSat");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPrepPulses.ucWaterSat not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPrepPulsesucWaterSat != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPrepPulsesucWaterSat(CsaSeriesMrPhoenixProtocolsPrepPulsesucWaterSat);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPrepPulsesucInversion = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPrepPulses.ucInversion");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPrepPulses.ucInversion not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPrepPulsesucInversion != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPrepPulsesucInversion(CsaSeriesMrPhoenixProtocolsPrepPulsesucInversion);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPrepPulsesucSatRecovery = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPrepPulses.ucSatRecovery");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPrepPulses.ucSatRecovery not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPrepPulsesucSatRecovery != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPrepPulsesucSatRecovery(CsaSeriesMrPhoenixProtocolsPrepPulsesucSatRecovery);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPrepPulsesucT2Prep = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPrepPulses.ucT2Prep");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPrepPulses.ucT2Prep not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPrepPulsesucT2Prep != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPrepPulsesucT2Prep(CsaSeriesMrPhoenixProtocolsPrepPulsesucT2Prep);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPrepPulsesucTIScout = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPrepPulses.ucTIScout");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPrepPulses.ucTIScout not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPrepPulsesucTIScout != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPrepPulsesucTIScout(CsaSeriesMrPhoenixProtocolsPrepPulsesucTIScout);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPrepPulsesucFatSatMode = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPrepPulses.ucFatSatMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPrepPulses.ucFatSatMode not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPrepPulsesucFatSatMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPrepPulsesucFatSatMode(CsaSeriesMrPhoenixProtocolsPrepPulsesucFatSatMode);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPrepPulsesdDarkBloodThickness = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sPrepPulses.dDarkBloodThickness");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPrepPulses.dDarkBloodThickness not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsPrepPulsesdDarkBloodThickness)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPrepPulsesdDarkBloodThickness(
	    	        			CsaSeriesMrPhoenixProtocolsPrepPulsesdDarkBloodThickness);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPrepPulsesdDarkBloodFlipAngle = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sPrepPulses.dDarkBloodFlipAngle");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPrepPulses.dDarkBloodFlipAngle not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsPrepPulsesdDarkBloodFlipAngle)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPrepPulsesdDarkBloodFlipAngle(
	    	        			CsaSeriesMrPhoenixProtocolsPrepPulsesdDarkBloodFlipAngle);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPrepPulsesdT2PrepDuration = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sPrepPulses.dT2PrepDuration");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocolsPrepPulses.dT2PrepDuration not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsPrepPulsesdT2PrepDuration)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPrepPulsesdT2PrepDuration(CsaSeriesMrPhoenixProtocolsPrepPulsesdT2PrepDuration);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPrepPulsesdIRPulseThicknessFactor = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sPrepPulses.dIRPulseThicknessFactor");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPrepPulses.dIRPulseThicknessFactor not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsPrepPulsesdT2PrepDuration)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPrepPulsesdIRPulseThicknessFactor(
	    	        			CsaSeriesMrPhoenixProtocolsPrepPulsesdIRPulseThicknessFactor);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsKSpacedPhaseResolution = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sKSpace.dPhaseResolution");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sKSpace.dPhaseResolution not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsKSpacedPhaseResolution)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsKSpacedPhaseResolution(CsaSeriesMrPhoenixProtocolsKSpacedPhaseResolution);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsKSpacedSliceResolution = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sKSpace.dSliceResolution");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sKSpace.dSliceResolution not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsKSpacedSliceResolution)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsKSpacedSliceResolution(CsaSeriesMrPhoenixProtocolsKSpacedSliceResolution);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsKSpacedAngioDynCentralRegionA = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sKSpace.dAngioDynCentralRegionA");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sKSpace.dAngioDynCentralRegionA not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsKSpacedAngioDynCentralRegionA)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsKSpacedAngioDynCentralRegionA(
	    	        			CsaSeriesMrPhoenixProtocolsKSpacedAngioDynCentralRegionA);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsKSpacedAngioDynSamplingDensityB = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sKSpace.dAngioDynSamplingDensityB");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sKSpace.dAngioDynSamplingDensityB not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsKSpacedAngioDynSamplingDensityB)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsKSpacedAngioDynSamplingDensityB(
	    	        			CsaSeriesMrPhoenixProtocolsKSpacedAngioDynSamplingDensityB);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsKSpacelBaseResolution = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sKSpace.lBaseResolution");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sKSpace.lBaseResolution not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsKSpacelBaseResolution != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsKSpacelBaseResolution(CsaSeriesMrPhoenixProtocolsKSpacelBaseResolution);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsKSpacelPhaseEncodingLines = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sKSpace.lPhaseEncodingLines");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sKSpace.lPhaseEncodingLines not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsKSpacelPhaseEncodingLines != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsKSpacelPhaseEncodingLines(CsaSeriesMrPhoenixProtocolsKSpacelPhaseEncodingLines);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsKSpacelPartitions = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sKSpace.lPartitions");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sKSpace.lPartitions not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsKSpacelPartitions != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsKSpacelPartitions(CsaSeriesMrPhoenixProtocolsKSpacelPartitions);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsKSpacelImagesPerSlab = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sKSpace.lImagesPerSlab");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sKSpace.lImagesPerSlab not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsKSpacelImagesPerSlab != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsKSpacelImagesPerSlab(CsaSeriesMrPhoenixProtocolsKSpacelImagesPerSlab);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsKSpacelRadialViews = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sKSpace.lRadialViews");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sKSpace.lRadialViews not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsKSpacelRadialViews != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsKSpacelRadialViews(CsaSeriesMrPhoenixProtocolsKSpacelRadialViews);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsKSpacelRadialInterleavesPerImage = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sKSpace.lRadialInterleavesPerImage");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sKSpace.lRadialInterleavesPerImage not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsKSpacelRadialInterleavesPerImage != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsKSpacelRadialInterleavesPerImage(
	    	        			CsaSeriesMrPhoenixProtocolsKSpacelRadialInterleavesPerImage);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsKSpacelLinesPerShot = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sKSpace.lLinesPerShot");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sKSpace.lLinesPerShot not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsKSpacelLinesPerShot != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsKSpacelLinesPerShot(CsaSeriesMrPhoenixProtocolsKSpacelLinesPerShot);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsKSpaceunReordering = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sKSpace.unReordering");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sKSpace.unReordering not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsKSpaceunReordering != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsKSpaceunReordering(CsaSeriesMrPhoenixProtocolsKSpaceunReordering);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsKSpacedSeqPhasePartialFourierForSNR = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sKSpace.dSeqPhasePartialFourierForSNR");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sKSpace.dSeqPhasePartialFourierForSNR not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsKSpacedSeqPhasePartialFourierForSNR)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsKSpacedSeqPhasePartialFourierForSNR(
	    	        			CsaSeriesMrPhoenixProtocolsKSpacedSeqPhasePartialFourierForSNR);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsKSpaceucPhasePartialFourier = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sKSpace.ucPhasePartialFourier");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sKSpace.ucPhasePartialFourier not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsKSpaceucPhasePartialFourier != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsKSpaceucPhasePartialFourier(
	    	        			CsaSeriesMrPhoenixProtocolsKSpaceucPhasePartialFourier);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsKSpaceucSlicePartialFourier = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sKSpace.ucSlicePartialFourier");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sKSpace.ucSlicePartialFourier not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsKSpaceucSlicePartialFourier != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsKSpaceucSlicePartialFourier(
	    	        			CsaSeriesMrPhoenixProtocolsKSpaceucSlicePartialFourier);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsKSpaceucAveragingMode = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sKSpace.ucAveragingMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sKSpace.ucAveragingMode not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsKSpaceucAveragingMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsKSpaceucAveragingMode(CsaSeriesMrPhoenixProtocolsKSpaceucAveragingMode);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsKSpaceucMultiSliceMode = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sKSpace.ucMultiSliceMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sKSpace.ucMultiSliceMode not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsKSpaceucMultiSliceMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsKSpaceucMultiSliceMode(CsaSeriesMrPhoenixProtocolsKSpaceucMultiSliceMode);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsKSpaceucDimension = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sKSpace.ucDimension");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sKSpace.ucDimension not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsKSpaceucDimension != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsKSpaceucDimension(CsaSeriesMrPhoenixProtocolsKSpaceucDimension);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsKSpaceucTrajectory = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sKSpace.ucTrajectory");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sKSpace.ucTrajectory not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsKSpaceucTrajectory != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsKSpaceucTrajectory(CsaSeriesMrPhoenixProtocolsKSpaceucTrajectory);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsKSpaceucViewSharing = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sKSpace.ucViewSharing");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sKSpace.ucViewSharing not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsKSpaceucViewSharing != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsKSpaceucViewSharing(CsaSeriesMrPhoenixProtocolsKSpaceucViewSharing);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsKSpaceucAsymmetricEchoMode = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sKSpace.ucAsymmetricEchoMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sKSpace.ucAsymmetricEchoMode not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsKSpaceucAsymmetricEchoMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsKSpaceucAsymmetricEchoMode(
	    	        			CsaSeriesMrPhoenixProtocolsKSpaceucAsymmetricEchoMode);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsKSpaceucPOCS = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sKSpace.ucPOCS");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sKSpace.ucPOCS not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsKSpaceucPOCS != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsKSpaceucPOCS(CsaSeriesMrPhoenixProtocolsKSpaceucPOCS);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsFastImaginglEPIFactor = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sFastImaging.lEPIFactor");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sFastImaging.lEPIFactor not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsFastImaginglEPIFactor != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsFastImaginglEPIFactor(CsaSeriesMrPhoenixProtocolsFastImaginglEPIFactor);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsFastImaginglTurboFactor = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sFastImaging.lTurboFactor");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sFastImaging.lTurboFactor not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsFastImaginglTurboFactor != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsFastImaginglTurboFactor(CsaSeriesMrPhoenixProtocolsFastImaginglTurboFactor);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsFastImaginglSliceTurboFactor = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sFastImaging.lSliceTurboFactor");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sFastImaging.lSliceTurboFactor not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsFastImaginglSliceTurboFactor != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsFastImaginglSliceTurboFactor(
	    	        			CsaSeriesMrPhoenixProtocolsFastImaginglSliceTurboFactor);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsFastImaginglSegments = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sFastImaging.lSegments");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sFastImaging.lSegments not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsFastImaginglSegments != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsFastImaginglSegments(CsaSeriesMrPhoenixProtocolsFastImaginglSegments);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsFastImagingucSegmentationMode = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sFastImaging.ucSegmentationMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sFastImaging.ucSegmentationMode not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsFastImagingucSegmentationMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsFastImagingucSegmentationMode(
	    	        			CsaSeriesMrPhoenixProtocolsFastImagingucSegmentationMode);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsFastImaginglShots = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sFastImaging.lShots");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sFastImaging.lShots not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsFastImaginglShots != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsFastImaginglShots(CsaSeriesMrPhoenixProtocolsFastImaginglShots);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsFastImaginglEchoTrainDuration = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sFastImaging.lEchoTrainDuration");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sFastImaging.lEchoTrainDuration not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsFastImaginglEchoTrainDuration != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsFastImaginglEchoTrainDuration(
	    	        			CsaSeriesMrPhoenixProtocolsFastImaginglEchoTrainDuration);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPhysioImaginglSignal1 = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPhysioImaging.lSignal1");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPhysioImaging.lSignal1 not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPhysioImaginglSignal1 != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPhysioImaginglSignal1(CsaSeriesMrPhoenixProtocolsPhysioImaginglSignal1);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPhysioImaginglMethod1 = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPhysioImaging.lMethod1");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPhysioImaging.lMethod1 not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPhysioImaginglMethod1 != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPhysioImaginglMethod1(CsaSeriesMrPhoenixProtocolsPhysioImaginglMethod1);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPhysioImaginglSignal2 = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPhysioImaging.lSignal2");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPhysioImaging.lSignal2 not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPhysioImaginglSignal2 != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPhysioImaginglSignal2(CsaSeriesMrPhoenixProtocolsPhysioImaginglSignal2);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPhysioImaginglMethod2 = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPhysioImaging.lMethod2");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPhysioImaging.lMethod2 not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPhysioImaginglMethod2 != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPhysioImaginglMethod2(CsaSeriesMrPhoenixProtocolsPhysioImaginglMethod2);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPhysioImaginglPhases = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPhysioImaging.lPhases");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPhysioImaging.lPhases not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPhysioImaginglPhases != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPhysioImaginglPhases(CsaSeriesMrPhoenixProtocolsPhysioImaginglPhases);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPhysioImaginglRetroGatedImages = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPhysioImaging.lRetroGatedImages");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPhysioImaging.lRetroGatedImages not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPhysioImaginglRetroGatedImages != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPhysioImaginglRetroGatedImages(
	    	        			CsaSeriesMrPhoenixProtocolsPhysioImaginglRetroGatedImages);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerPulses = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioECG.lTriggerPulses");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioECG.lTriggerPulses not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerPulses != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerPulses(
	    	        			CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerPulses);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerWindow = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioECG.lTriggerWindow");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioECG.lTriggerWindow not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerWindow != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerWindow(
	    	        			CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerWindow);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlArrhythmiaDetection = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioECG.lArrhythmiaDetection");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioECG.lArrhythmiaDetection not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlArrhythmiaDetection != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlArrhythmiaDetection(
	    	        			CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlArrhythmiaDetection);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlCardiacGateOnThreshold = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioECG.lCardiacGateOnThreshold");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioECG.lCardiacGateOnThreshold not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlCardiacGateOnThreshold != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlCardiacGateOnThreshold(
	    	        			CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlCardiacGateOnThreshold);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlCardiacGateOffThreshold = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioECG.lCardiacGateOffThreshold");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioECG.lCardiacGateOffThreshold" +
	    	                                " not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlCardiacGateOffThreshold != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlCardiacGateOffThreshold(
	    	        			CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlCardiacGateOffThreshold);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerIntervals = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioECG.lTriggerIntervals");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioECG.lTriggerIntervals not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerIntervals != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerIntervals(
	    	        			CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerIntervals);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerPulses = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioPulse.lTriggerPulses");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioPulse.lTriggerPulses not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerPulses != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerPulses(
	    	        			CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerPulses);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerWindow = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioPulse.lTriggerWindow");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioPulse.lTriggerWindow not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerWindow != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerWindow(
	    	        			CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerWindow);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselArrhythmiaDetection = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioPulse.lArrhythmiaDetection");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioPulse.lArrhythmiaDetection not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselArrhythmiaDetection != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselArrhythmiaDetection(
	    	        			CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselArrhythmiaDetection);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselCardiacGateOnThreshold = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioPulse.lCardiacGateOnThreshold");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioPulse.lCardiacGateOnThreshold not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselCardiacGateOnThreshold != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselCardiacGateOnThreshold(
	    	        			CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselCardiacGateOnThreshold);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselCardiacGateOffThreshold = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioPulse.lCardiacGateOffThreshold");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioPulse.lCardiacGateOffThreshold not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselCardiacGateOffThreshold != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselCardiacGateOffThreshold(
	    	        			CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselCardiacGateOffThreshold);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerIntervals = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioPulse.lTriggerIntervals");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioPulse.lTriggerIntervals not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerIntervals != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerIntervals(
	    	        			CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerIntervals);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerPulses = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioExt.lTriggerPulses");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioExt.lTriggerPulses not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerPulses != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerPulses(
	    	        			CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerPulses);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerWindow = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioExt.lTriggerWindow");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioExt.lTriggerWindow not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerWindow != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerWindow(
	    	        			CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerWindow);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlArrhythmiaDetection = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioExt.lArrhythmiaDetection");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioExt.lArrhythmiaDetection not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlArrhythmiaDetection != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlArrhythmiaDetection(
	    	        			CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlArrhythmiaDetection);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlCardiacGateOnThreshold = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioExt.lCardiacGateOnThreshold");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioExt.lCardiacGateOnThreshold not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlCardiacGateOnThreshold != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlCardiacGateOnThreshold(
	    	        			CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlCardiacGateOnThreshold);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlCardiacGateOffThreshold = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioExt.lCardiacGateOffThreshold");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioExt.lCardiacGateOffThresold not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlCardiacGateOffThreshold != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlCardiacGateOffThreshold(
	    	        			CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlCardiacGateOffThreshold);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerIntervals = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioExt.lTriggerIntervals");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioExt.lTriggerIntervals not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerIntervals != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerIntervals(
	    	        			CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerIntervals);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioResplRespGateThreshold = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioResp.lRespGateThreshold");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioResp.lRespGateThreshold not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioResplRespGateThreshold != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioResplRespGateThreshold(
	    	        			CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioResplRespGateThreshold);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioResplRespGatePhase = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioResp.lRespGatePhase");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioResp.lRespGatePhase not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioResplRespGatePhase != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioResplRespGatePhase(
	    	        			CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioResplRespGatePhase);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioRespdGatingRatio = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioResp.dGatingRatio");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioResp.dGatingRatio not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioRespdGatingRatio)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioRespdGatingRatio(
	    	        			CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioRespdGatingRatio);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioNativeucMode = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioNative.ucMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioNative.ucMode not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioNativeucMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioNativeucMode(
	    	        			CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioNativeucMode);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioNativeucFlowSenMode = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioNative.ucFlowSenMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioNative.ucFlowSenMode not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioNativeucFlowSenMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioNativeucFlowSenMode(
	    	        			CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioNativeucFlowSenMode);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsSpecParalPhaseCyclingType = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sSpecPara.lPhaseCyclingType");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sSpecPara.lPhaseCyclingType not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsSpecParalPhaseCyclingType != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsSpecParalPhaseCyclingType(
	    	        			CsaSeriesMrPhoenixProtocolsSpecParalPhaseCyclingType);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsSpecParalPhaseEncodingType = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sSpecPara.lPhaseEncodingType");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sSpecPara.lPhaseEncodingType not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsSpecParalPhaseEncodingType != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsSpecParalPhaseEncodingType(
	    	        			CsaSeriesMrPhoenixProtocolsSpecParalPhaseEncodingType);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsSpecParalRFExcitationBandwidth = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sSpecPara.lRFExcitationBandwidth");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sSpecPara.lRFExcitationBandwidth not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsSpecParalRFExcitationBandwidth != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsSpecParalRFExcitationBandwidth(
	    	        			CsaSeriesMrPhoenixProtocolsSpecParalRFExcitationBandwidth);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsSpecParaucRemoveOversampling = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sSpecPara.ucRemoveOversampling");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sSpecPara.ucRemoveOversampling not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsSpecParaucRemoveOversampling != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsSpecParaucRemoveOversampling(
	    	        			CsaSeriesMrPhoenixProtocolsSpecParaucRemoveOversampling);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsSpecParalAutoRefScanNo = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sSpecPara.lAutoRefScanNo");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sSpecPara.lAutoRefScanNo not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsSpecParalAutoRefScanNo != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsSpecParalAutoRefScanNo(
	    	        			CsaSeriesMrPhoenixProtocolsSpecParalAutoRefScanNo);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsSpecParalDecouplingType = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sSpecPara.lDecouplingType");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sSpecPara.lDecouplingType not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsSpecParalDecouplingType != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsSpecParalDecouplingType(CsaSeriesMrPhoenixProtocolsSpecParalDecouplingType);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsSpecParalNOEType = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sSpecPara.lNOEType");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sSpecPara.lNOEType not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsSpecParalNOEType != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsSpecParalNOEType(CsaSeriesMrPhoenixProtocolsSpecParalNOEType);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsSpecParalExcitationType = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sSpecPara.lExcitationType");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sSpecPara.lExcitationType not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsSpecParalExcitationType != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsSpecParalExcitationType(
	    	        			CsaSeriesMrPhoenixProtocolsSpecParalExcitationType);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsSpecParalSpecAppl = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sSpecPara.lSpecAppl");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sSpecPara.lSpecAppl not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsSpecParalSpecAppl != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsSpecParalSpecAppl(CsaSeriesMrPhoenixProtocolsSpecParalSpecAppl);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsSpecParalSpectralSuppression = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sSpecPara.lSpectralSuppression");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sSpecPara.lSpectralSuppression not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsSpecParalSpectralSuppression != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsSpecParalSpectralSuppression(
	    	        			CsaSeriesMrPhoenixProtocolsSpecParalSpectralSuppression);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsDiffusionulMode = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sDiffusion.ulMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sDiffusion.ulMode not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsDiffusionulMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsDiffusionulMode(CsaSeriesMrPhoenixProtocolsDiffusionulMode);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsAngioucPCFlowMode = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sAngio.ucPCFlowMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sAngio.ucPCFlowMode not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsAngioucPCFlowMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsAngioucPCFlowMode(CsaSeriesMrPhoenixProtocolsAngioucPCFlowMode);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsAngioucTOFInflow = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sAngio.ucTOFInflow");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sAngio.ucTOFInflow not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsAngioucTOFInflow != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsAngioucTOFInflow(CsaSeriesMrPhoenixProtocolsAngioucTOFInflow);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsAngiolDynamicReconMode = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sAngio.lDynamicReconMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sAngio.lDynamicReconMode not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsAngiolDynamicReconMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsAngiolDynamicReconMode(CsaSeriesMrPhoenixProtocolsAngiolDynamicReconMode);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsAngiolTemporalInterpolation = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sAngio.lTemporalInterpolation");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sAngio.lTemporalInterpolation not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsAngiolTemporalInterpolation != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsAngiolTemporalInterpolation(
	    	        			CsaSeriesMrPhoenixProtocolsAngiolTemporalInterpolation);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsRawFilterlSlope_256 = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sRawFilter.lSlope_256");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sRawFilter.lSlope_256 not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsRawFilterlSlope_256 != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsRawFilterlSlope_256(CsaSeriesMrPhoenixProtocolsRawFilterlSlope_256);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsRawFilterucOn = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sRawFilter.ucOn");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sRawFilter.ucOn not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsRawFilterucOn != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsRawFilterucOn(CsaSeriesMrPhoenixProtocolsRawFilterucOn);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsRawFilterucMode = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sRawFilter.ucMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sRawFilter.ucMode not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsRawFilterucMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsRawFilterucMode(CsaSeriesMrPhoenixProtocolsRawFilterucMode);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsDistortionCorrFilterucMode = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sDistortionCorrFilter.ucMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sDistortionCorrFilter.ucMode not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsDistortionCorrFilterucMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsDistortionCorrFilterucMode(
	    	        			CsaSeriesMrPhoenixProtocolsDistortionCorrFilterucMode);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPatlAccelFactPE = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPat.lAccelFactPE");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPat.lAccelFactPE not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPatlAccelFactPE != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPatlAccelFactPE(CsaSeriesMrPhoenixProtocolsPatlAccelFactPE);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPatlAccelFact3D = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPat.lAccelFact3D");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPat.lAccelFact3D not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPatlAccelFact3D != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPatlAccelFact3D(CsaSeriesMrPhoenixProtocolsPatlAccelFact3D);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPatlRefLinesPE = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPat.lRefLinesPE");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPat.lRefLinesPE not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPatlRefLinesPE != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPatlRefLinesPE(CsaSeriesMrPhoenixProtocolsPatlRefLinesPE);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPatucPATMode = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPat.ucPATMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPat.ucPATMode not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPatucPATMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPatucPATMode(CsaSeriesMrPhoenixProtocolsPatucPATMode);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPatucRefScanMode = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPat.ucRefScanMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPat.ucRefScanMode not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPatucRefScanMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPatucRefScanMode(CsaSeriesMrPhoenixProtocolsPatucRefScanMode);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsPatucTPatAverageAllFrames = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sPat.ucTPatAverageAllFrames");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sPat.ucTPatAverageAllFrames not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsPatucTPatAverageAllFrames != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsPatucTPatAverageAllFrames(CsaSeriesMrPhoenixProtocolsPatucTPatAverageAllFrames);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsMDSulMdsModeMask = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sMDS.ulMdsModeMask");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sMDS.ulMdsModeMask not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsMDSulMdsModeMask != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsMDSulMdsModeMask(CsaSeriesMrPhoenixProtocolsMDSulMdsModeMask);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsMDSulMdsVariableResolution = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sMDS.ulMdsVariableResolution");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sMDS.ulMdsVariableResolution not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsMDSulMdsVariableResolution != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsMDSulMdsVariableResolution(
	    	        			CsaSeriesMrPhoenixProtocolsMDSulMdsVariableResolution);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsMDSlTableSpeedNumerator = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sMDS.lTableSpeedNumerator");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sMDS.lTableSpeedNumerator not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsMDSlTableSpeedNumerator != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsMDSlTableSpeedNumerator(CsaSeriesMrPhoenixProtocolsMDSlTableSpeedNumerator);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsMDSlmdsLinesPerSegment = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sMDS.lmdsLinesPerSegment");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sMDS.lmdsLinesPerSegment not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsMDSlmdsLinesPerSegment != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsMDSlmdsLinesPerSegment(CsaSeriesMrPhoenixProtocolsMDSlmdsLinesPerSegment);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsMDSsMdsEndPosSBCS_mmdTra = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sMDS.sMdsEndPosSBCS_mm.dTra");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sMDS.sMdsEndPosSBCS_mm.dTra not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsMDSsMdsEndPosSBCS_mmdTra)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsMDSsMdsEndPosSBCS_mmdTra(CsaSeriesMrPhoenixProtocolsMDSsMdsEndPosSBCS_mmdTra);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsMDSulMdsReconMode = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sMDS.ulMdsReconMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sMDS.ulMdsReconMode not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsMDSulMdsReconMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsMDSulMdsReconMode(CsaSeriesMrPhoenixProtocolsMDSulMdsReconMode);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsMDSdMdsRangeExtension = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.sMDS.dMdsRangeExtension");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sMDS.dMdsRangeExtension not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsMDSdMdsRangeExtension)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsMDSdMdsRangeExtension(CsaSeriesMrPhoenixProtocolsMDSdMdsRangeExtension);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolucEnableIntro = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.ucEnableIntro");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.ucEnableIntro not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolucEnableIntro != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolucEnableIntro(CsaSeriesMrPhoenixProtocolucEnableIntro);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolucDisableChangeStoreImages = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.ucDisableChangeStoreImages");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.ucDisableChangeStoreImages not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolucDisableChangeStoreImages != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolucDisableChangeStoreImages(CsaSeriesMrPhoenixProtocolucDisableChangeStoreImages);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolucAAMode = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.ucAAMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.ucAAMode not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolucAAMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolucAAMode(CsaSeriesMrPhoenixProtocolucAAMode);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolucAARegionMode = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.ucAARegionMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.ucAARegionMode not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolucAARegionMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolucAARegionMode(CsaSeriesMrPhoenixProtocolucAARegionMode);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolucAARefMode = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.ucAARefMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.ucAARefMode not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolucAARefMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolucAARefMode(CsaSeriesMrPhoenixProtocolucAARefMode);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolucReconstructionMode = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.ucReconstructionMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.ucReconstructionMode not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolucReconstructionMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolucReconstructionMode(CsaSeriesMrPhoenixProtocolucReconstructionMode);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolucOneSeriesForAllMeas = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.ucOneSeriesForAllMeas");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.ucOneSeriesForAllMeas not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolucOneSeriesForAllMeas != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolucOneSeriesForAllMeas(CsaSeriesMrPhoenixProtocolucOneSeriesForAllMeas);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolucPHAPSMode = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.ucPHAPSMode");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.ucPHAPSMode not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolucPHAPSMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolucPHAPSMode(CsaSeriesMrPhoenixProtocolucPHAPSMode);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolucDixon = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.ucDixon");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.ucDixon not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolucDixon != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolucDixon(CsaSeriesMrPhoenixProtocolucDixon);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolucDixonSaveOriginal = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.ucDixonSaveOriginal");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.ucDixonSaveOriginal not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolucDixonSaveOriginal != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolucDixonSaveOriginal(CsaSeriesMrPhoenixProtocolucDixonSaveOriginal);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolucWaitForPrepareCompletion = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.ucWaitForPrepareCompletion");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.ucWaitForPrepareCompletion not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolucWaitForPrepareCompletion != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolucWaitForPrepareCompletion(CsaSeriesMrPhoenixProtocolucWaitForPrepareCompletion);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocollAverages = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.lAverages");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.lAverages not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocollAverages != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocollAverages(CsaSeriesMrPhoenixProtocollAverages);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocoldAveragesDouble = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.dAveragesDouble");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.dAveragesDouble not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocoldAveragesDouble)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocoldAveragesDouble(CsaSeriesMrPhoenixProtocoldAveragesDouble);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocollRepetitions = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.lRepetitions");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.lRepetitions not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocollRepetitions != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocollRepetitions(CsaSeriesMrPhoenixProtocollRepetitions);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocoladFlipAngleDegree0 = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.adFlipAngleDegree[0]");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.adFlipAngleDegree[0] not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocoladFlipAngleDegree0)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocoladFlipAngleDegree0(CsaSeriesMrPhoenixProtocoladFlipAngleDegree0);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocollScanTimeSec = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.lScanTimeSec");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.lScanTimeSec not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocollScanTimeSec != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocollScanTimeSec(CsaSeriesMrPhoenixProtocollScanTimeSec);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocollTotalScanTimeSec = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.lTotalScanTimeSec");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.lTotalScanTimeSec not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocollTotalScanTimeSec != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocollTotalScanTimeSec(CsaSeriesMrPhoenixProtocollTotalScanTimeSec);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocoldRefSNR = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.dRefSNR");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.dRefSNR not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocoldRefSNR)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocoldRefSNR(CsaSeriesMrPhoenixProtocoldRefSNR);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocoldRefSNR_VOI = 
	    	        			global_const.getDouble("CsaSeries.MrPhoenixProtocol.dRefSNR_VOI");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.dRefSNR_VOI not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocoldRefSNR_VOI)) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocoldRefSNR_VOI(CsaSeriesMrPhoenixProtocoldRefSNR_VOI);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocoltdefaultEVAProt = 
	    	        			global_const.getString("CsaSeries.MrPhoenixProtocol.tdefaultEVAProt");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.tdefaultEVAProt not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocoltdefaultEVAProt != null) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocoltdefaultEVAProt(CsaSeriesMrPhoenixProtocoltdefaultEVAProt);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolasCoilSelectMeas0tNucleus = 
	    	        			global_const.getString("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].tNucleus");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].tNucleus not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0tNucleus != null) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolasCoilSelectMeas0tNucleus(CsaSeriesMrPhoenixProtocolasCoilSelectMeas0tNucleus);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolasCoilSelectMeas0iUsedRFactor = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].iUsedRFactor");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].iUsedRFactor not found\n",
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0iUsedRFactor != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolasCoilSelectMeas0iUsedRFactor(
	    	        			CsaSeriesMrPhoenixProtocolasCoilSelectMeas0iUsedRFactor);
	    	        }
	    	        for (i = 0; i < 24; i++) {
	    	        	CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtCoilID[i] = null;
	    	            try {
	    	            	CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtCoilID[i] =
	    	            			global_const.getString("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].asList["+i+
	    	            					"].sCoilElementID.tCoilID");
	    	            }
	    	            catch (JSONException e) {
	    	            	Preferences.debug("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].asList["+i+
	    	            			"].sCoilElementID.tCoilID not found\n", Preferences.DEBUG_FILEIO);
	    	            }
	    	            CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDlCoilCopy[i] = Integer.MIN_VALUE;
	    	            try {
	    	            	CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDlCoilCopy[i] = 
	    	            			global_const.getInt("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].asList["+i+
	    	            					"].sCoilElementID.lCoilCopy");
	    	            }
	    	            catch (JSONException e) {
	    	            	Preferences.debug("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].asList["+i+
	    	            			"].sCoilElementID.lCoilCopy not found\n", Preferences.DEBUG_FILEIO);
	    	            }
	    	            CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtElement[i] = null;
	    	            try {
	    	            	CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtElement[i] =
	    	            			global_const.getString("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].asList["+i+
	    	            					"].sCoilElementID.tElement");
	    	            }
	    	            catch (JSONException e) {
	    	            	Preferences.debug("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].asList["+i+
	    	            			"].sCoilElementID.tElement not found\n", Preferences.DEBUG_FILEIO);
	    	            }
	    	            CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlElementSelected[i] = Integer.MIN_VALUE;
	    	            try {
	    	            	CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlElementSelected[i] = 
	    	            			global_const.getInt("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].asList["+i+
	    	            					"].lElementSelected");
	    	            }
	    	            catch (JSONException e) {
	    	            	Preferences.debug("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].asList["+i+
	    	            			"].lElementSelected not found\n", Preferences.DEBUG_FILEIO);
	    	            }
	    	            CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlRxChannelConnected[i] = Integer.MIN_VALUE;
	    	            try {
	    	            	CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlRxChannelConnected[i] = 
	    	            			global_const.getInt("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].asList["+i+
	    	            					"].lRxChannelConnected");
	    	            }
	    	            catch (JSONException e) {
	    	            	Preferences.debug("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].asList["+i+
	    	            			"].lRxChannelConnected not found\n", Preferences.DEBUG_FILEIO);
	    	            }
	    	        } // for (i = 0; i < 24; i++)
	    	        if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtCoilID != null) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtCoilID(
	    	        			CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtCoilID);	
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDlCoilCopy != null) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDlCoilCopy(
	    	        			CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDlCoilCopy);	
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtElement != null) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtElement(
	    	        			CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtElement);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlElementSelected != null) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlElementSelected(
	    	        			CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlElementSelected);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlRxChannelConnected != null) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlRxChannelConnected(
	    	        			CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlRxChannelConnected);
	    	        }
	    	        for (i = 0; i < CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSaulPlugId.length; i++) {
	    	        	CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSaulPlugId[i] = Integer.MIN_VALUE;
	    	            try {
	    	            	CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSaulPlugId[i] = 
	    	            			global_const.getInt("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].sCOILPLUGS.aulPlugId["+i+"]");
	    	            }
	    	            catch (JSONException e) {
	    	            	Preferences.debug("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0]asCoilSelectMeas[0].sCOILPLUGS.aulPlugId["+
	    	            i+"] not found\n", Preferences.DEBUG_FILEIO);
	    	            }	
	    	        } // for (i = 0; i < CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSaulPlugId.length; i++)
	    	        if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSaulPlugId != null) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSaulPlugId(
	    	        			CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSaulPlugId);
	    	        }
	    	        for (i = 0; i < CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSauiNmbrOfNibbles.length; i++) {
	    	        	CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSauiNmbrOfNibbles[i] = Integer.MIN_VALUE;
	    	            try {
	    	            	CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSauiNmbrOfNibbles[i] = 
	    	            			global_const.getInt("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].sCOILPLUGS.auiNmbrOfNibbles["+i+"]");
	    	            }
	    	            catch (JSONException e) {
	    	            	Preferences.debug("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0]asCoilSelectMeas[0].sCOILPLUGS.auiNmbrOfNibbles["
	    	            +i+"] not found\n", Preferences.DEBUG_FILEIO);
	    	            }	
	    	        } // for (i = 0; i < CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSauiNmbrOfNibbles.length; i++)
	    	        if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSauiNmbrOfNibbles != null) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSauiNmbrOfNibbles(
	    	        			CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSauiNmbrOfNibbles);
	    	        }
	    	        for (i = 0; i < 24; i++) {
	    	        	CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnflFactor[i] = Double.NaN;
	    	            try {
	    	            	CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnflFactor[i] = 
	    	            			global_const.getDouble("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].aFFT_SCALE["+i+"].flFactor");
	    	            }
	    	            catch (JSONException e) {
	    	            	Preferences.debug("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].aFFT_SCALE["+i+
	    	            			"].flFactor not found\n", Preferences.DEBUG_FILEIO);
	    	            }
	    	            CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnbValid[i] = Integer.MIN_VALUE;
	    	            try {
	    	            	CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnbValid[i] = 
	    	            			global_const.getInt("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].aFFT_SCALE["+i+"].bValid");
	    	            }
	    	            catch (JSONException e) {
	    	            	Preferences.debug("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].aFFT_SCALE["+i+
	    	            			"].bValid not found\n", Preferences.DEBUG_FILEIO);
	    	            }
	    	            CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnlRxChannel[i] = Integer.MIN_VALUE;
	    	            try {
	    	            	CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnlRxChannel[i] = 
	    	            			global_const.getInt("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].aFFT_SCALE["+i+"].lRxChannel");
	    	            }
	    	            catch (JSONException e) {
	    	            	Preferences.debug("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].aFFT_SCALE["+i+
	    	            			"].lRxChannel not found\n", Preferences.DEBUG_FILEIO);
	    	            }
	    	        } // for (i = 0; i < 24; i++)
	    	        if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnflFactor != null) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnflFactor(
	    	        			CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnflFactor);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnbValid != null) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnbValid(
	    	        			CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnbValid);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnlRxChannel != null) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnlRxChannel(
	    	        			CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnlRxChannel);
	    	        }
	    	        try {
	    	        	CsaSeriesMrPhoenixProtocolsEFISPECbEFIDataValid = 
	    	        			global_const.getInt("CsaSeries.MrPhoenixProtocol.sEFISPEC.bEFIDataValid");
	    	        }
	    	        catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sEFISPEC.bEFIDataValid not found\n", Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsEFISPECbEFIDataValid != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsEFISPECbEFIDataValid(CsaSeriesMrPhoenixProtocolsEFISPECbEFIDataValid);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsWiPMemBlockalFree != null) {
	    	        	for (i = 0; i < CsaSeriesMrPhoenixProtocolsWiPMemBlockalFree.length; i++) {
	    	        		CsaSeriesMrPhoenixProtocolsWiPMemBlockalFree[i] = Integer.MIN_VALUE;
	    	        		try {
	    	        			CsaSeriesMrPhoenixProtocolsWiPMemBlockalFree[i] = 
	    	        					global_const.getInt("CsaSeries.MrPhoenixProtocol.sWiPMemBlock.alFree["+i+"]");
	    	        		}
	    	        		catch (JSONException e) {
	    	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sWiPMemBlock.alFree["+i+"] not found\n", 
	    	    	        			Preferences.DEBUG_FILEIO);
	    	    	        }
	    	        	}
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsWiPMemBlockalFree(CsaSeriesMrPhoenixProtocolsWiPMemBlockalFree);
	    	        } // if (CsaSeriesMrPhoenixProtocolsWiPMemBlockalFree != null)
	    	        if (CsaSeriesMrPhoenixProtocolsWiPMemBlockadFree != null) {
	    	        	for (i = 0; i < CsaSeriesMrPhoenixProtocolsWiPMemBlockadFree.length; i++) {
	    	        		CsaSeriesMrPhoenixProtocolsWiPMemBlockadFree[i] = Double.NaN;
	    	        		try {
	    	        			CsaSeriesMrPhoenixProtocolsWiPMemBlockadFree[i] = 
	    	        					global_const.getDouble("CsaSeries.MrPhoenixProtocol.sWiPMemBlock.adFree["+i+"]");
	    	        		}
	    	        		catch (JSONException e) {
	    	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sWiPMemBlock.adFree["+i+"] not found\n", 
	    	    	        			Preferences.DEBUG_FILEIO);
	    	    	        }
	    	        	}
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsWiPMemBlockadFree(CsaSeriesMrPhoenixProtocolsWiPMemBlockadFree);
	    	        } // if (CsaSeriesMrPhoenixProtocolsWiPMemBlockadFree != null)
	    	        try {
	        			CsaSeriesMrPhoenixProtocolsWiPMemBlocktFree = 
	        					global_const.getString("CsaSeries.MrPhoenixProtocol.sWiPMemBlock.tFree");
	        		}
	        		catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sWiPMemBlock.tFree not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsWiPMemBlocktFree != null) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsWiPMemBlocktFree(CsaSeriesMrPhoenixProtocolsWiPMemBlocktFree);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolucBOLDParadigmArray != null) {
	    	            for (i = 0; i < CsaSeriesMrPhoenixProtocolucBOLDParadigmArray.length; i++) {
	    	            	CsaSeriesMrPhoenixProtocolucBOLDParadigmArray[i] = Integer.MIN_VALUE;
	    	            	try {
	    	            		CsaSeriesMrPhoenixProtocolucBOLDParadigmArray[i] =
	    	            				global_const.getInt("CsaSeries.MrPhoenixProtocol.ucBOLDParadigmArray["+i+"]");
	    	            	}
	    	            	catch (JSONException e) {
	    	            		Preferences.debug("CsaSeries.MrPhoenixProtocol.ucBOLDParadigmArray["+i+"] not found\n", 
	    	    	        			Preferences.DEBUG_FILEIO);	
	    	            	}
	    	            }
	    	            fileInfo.setCsaSeriesMrPhoenixProtocolucBOLDParadigmArray(CsaSeriesMrPhoenixProtocolucBOLDParadigmArray);
	    	        } // if (CsaSeriesMrPhoenixProtocolucBOLDParadigmArray != null)
	    	        try {
	        			CsaSeriesMrPhoenixProtocollParadigmPeriodicity = 
	        					global_const.getInt("CsaSeries.MrPhoenixProtocol.lParadigmPeriodicity");
	        		}
	        		catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.lParadigmPeriodicity not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocollParadigmPeriodicity != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocollParadigmPeriodicity(CsaSeriesMrPhoenixProtocollParadigmPeriodicity);
	    	        }
	    	        try {
	        			CsaSeriesMrPhoenixProtocolucCineMode = 
	        					global_const.getInt("CsaSeries.MrPhoenixProtocol.ucCineMode");
	        		}
	        		catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.ucCineMode not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolucCineMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolucCineMode(CsaSeriesMrPhoenixProtocolucCineMode);
	    	        }
	    	        try {
	        			CsaSeriesMrPhoenixProtocolucSequenceType = 
	        					global_const.getInt("CsaSeries.MrPhoenixProtocol.ucSequenceType");
	        		}
	        		catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.ucSequenceType not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolucSequenceType != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolucSequenceType(CsaSeriesMrPhoenixProtocolucSequenceType);
	    	        }
	    	        try {
	        			CsaSeriesMrPhoenixProtocolucCoilCombineMode = 
	        					global_const.getInt("CsaSeries.MrPhoenixProtocol.ucCoilCombineMode");
	        		}
	        		catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.ucCoilCombineMode not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolucCoilCombineMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolucCoilCombineMode(CsaSeriesMrPhoenixProtocolucCoilCombineMode);
	    	        }
	    	        try {
	        			CsaSeriesMrPhoenixProtocolucFlipAngleMode = 
	        					global_const.getInt("CsaSeries.MrPhoenixProtocol.ucFlipAngleMode");
	        		}
	        		catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.ucFlipAngleMode not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolucFlipAngleMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolucFlipAngleMode(CsaSeriesMrPhoenixProtocolucFlipAngleMode);
	    	        }
	    	        try {
	        			CsaSeriesMrPhoenixProtocollTOM = 
	        					global_const.getInt("CsaSeries.MrPhoenixProtocol.lTOM");
	        		}
	        		catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.lTOM not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocollTOM != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocollTOM(CsaSeriesMrPhoenixProtocollTOM);
	    	        }
	    	        try {
	        			CsaSeriesMrPhoenixProtocollProtID = 
	        					global_const.getInt("CsaSeries.MrPhoenixProtocol.lProtID");
	        		}
	        		catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.lProtID not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocollProtID != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocollProtID(CsaSeriesMrPhoenixProtocollProtID);
	    	        }
	    	        try {
	        			CsaSeriesMrPhoenixProtocolucReadOutMode = 
	        					global_const.getInt("CsaSeries.MrPhoenixProtocol.ucReadOutMode");
	        		}
	        		catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.ucReadOutMode not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolucReadOutMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolucReadOutMode(CsaSeriesMrPhoenixProtocolucReadOutMode);
	    	        }
	    	        try {
	        			CsaSeriesMrPhoenixProtocolucBold3dPace = 
	        					global_const.getInt("CsaSeries.MrPhoenixProtocol.ucBold3dPace");
	        		}
	        		catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.ucBold3dPace not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolucBold3dPace != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolucBold3dPace(CsaSeriesMrPhoenixProtocolucBold3dPace);
	    	        }
	    	        try {
	        			CsaSeriesMrPhoenixProtocolucForcePositioningOnNDIS = 
	        					global_const.getInt("CsaSeries.MrPhoenixProtocol.ucForcePositioningOnNDIS");
	        		}
	        		catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.ucForcePositioningOnNDIS not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolucForcePositioningOnNDIS != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolucForcePositioningOnNDIS(CsaSeriesMrPhoenixProtocolucForcePositioningOnNDIS);
	    	        }
	    	        try {
	        			CsaSeriesMrPhoenixProtocolucInteractiveRealtime = 
	        					global_const.getInt("CsaSeries.MrPhoenixProtocol.ucInteractiveRealtime");
	        		}
	        		catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.ucInteractiveRealtime not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolucInteractiveRealtime != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolucInteractiveRealtime(CsaSeriesMrPhoenixProtocolucInteractiveRealtime);
	    	        }
	    	        try {
	        			CsaSeriesMrPhoenixProtocolucInternalTablePosValid = 
	        					global_const.getInt("CsaSeries.MrPhoenixProtocol.ucInternalTablePosValid");
	        		}
	        		catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.ucInternalTablePosValid not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolucInternalTablePosValid != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolucInternalTablePosValid(CsaSeriesMrPhoenixProtocolucInternalTablePosValid);
	    	        }
	    	        try {
	        			CsaSeriesMrPhoenixProtocolsParametricMappingucParametricMap = 
	        					global_const.getInt("CsaSeries.MrPhoenixProtocol.sParametricMapping.ucParametricMap");
	        		}
	        		catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sParametricMapping.ucParametricMap not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsParametricMappingucParametricMap != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsParametricMappingucParametricMap(
	    	        			CsaSeriesMrPhoenixProtocolsParametricMappingucParametricMap);
	    	        }
	    	        try {
	        			CsaSeriesMrPhoenixProtocolsAslulMode = 
	        					global_const.getInt("CsaSeries.MrPhoenixProtocol.sAsl.ulMode");
	        		}
	        		catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.sAsl.ulMode not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolsAslulMode != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolsAslulMode(CsaSeriesMrPhoenixProtocolsAslulMode);
	    	        }
	    	        try {
	        			CsaSeriesMrPhoenixProtocolWaitForUserStart = 
	        					global_const.getInt("CsaSeries.MrPhoenixProtocol.WaitForUserStart");
	        		}
	        		catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.WaitForUserStart not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolWaitForUserStart != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolWaitForUserStart(CsaSeriesMrPhoenixProtocolWaitForUserStart);
	    	        }
	    	        try {
	        			CsaSeriesMrPhoenixProtocolucAutoAlignInit = 
	        					global_const.getInt("CsaSeries.MrPhoenixProtocol.ucAutoAlignInit");
	        		}
	        		catch (JSONException e) {
	    	        	Preferences.debug("CsaSeries.MrPhoenixProtocol.ucAutoAlignInit not found\n", 
	    	        			Preferences.DEBUG_FILEIO);
	    	        }
	    	        if (CsaSeriesMrPhoenixProtocolucAutoAlignInit != Integer.MIN_VALUE) {
	    	        	fileInfo.setCsaSeriesMrPhoenixProtocolucAutoAlignInit(CsaSeriesMrPhoenixProtocolucAutoAlignInit);
	    	        }
    	        } // if (!noReadPrivateTags)
    	    } // if (global_const != null)
    	} // if (global != null)
    	if (!noReadPrivateTags) {
    		JSONObject time = null;
            JSONObject samples = null;
            JSONArray AcquisitionTime = null;
            double AcquisitionTimeDouble[] = null;
            JSONArray AcquisitionNumber = null;
            int AcquisitionNumberInt[] = null;
            JSONArray InstanceNumber = null;
            int InstanceNumberInt[] = null;
            JSONArray WindowCenter = null;
            double WindowCenterDouble[] = null;
            JSONArray WindowWidth = null;
            double WindowWidthDouble[] = null;
            JSONArray CsaImageTimeAfterStart = null;
            double CsaImageTimeAfterStartDouble[] = null;
            JSONArray CsaImageMosaicRefAcqTimes = null;
            JSONArray CsaImageMosaicRefAcqTimes_inner = null;
            double CsaImageMosaicRefAcqTimesDouble[][] = null;
            JSONArray CsaImageICE_Dims = null;
            String CsaImageICE_DimsString[] = null;
            JSONArray CsaImageSliceMeasurementDuration = null;
            double CsaImageSliceMeasurementDurationDouble[] = null;
            JSONArray InstanceCreationTime = null;
            double InstanceCreationTimeDouble[] = null;
            JSONArray ContentTime = null;
            double ContentTimeDouble[] = null;
            JSONArray dcmmeta_affine = null;
            JSONArray dcmmeta_affine_inner = null;
            double dcmmeta_affineDouble[][] = null;
            JSONArray dcmmeta_reorient_transform = null;
            JSONArray dcmmeta_reorient_transform_inner = null;
            double dcmmeta_reorient_transformDouble[][] = null;
            int dcmmeta_slice_dim = Integer.MIN_VALUE;
            double dcmmeta_version = Double.NaN;
            JSONArray dcmmeta_shape = null;
            int dcmmeta_shapeInt[] = null;
	    	try {
	    		time = jsonObject.getJSONObject("time");
	    	}
	    	catch (JSONException e) {
	    	    Preferences.debug("time not found\n", Preferences.DEBUG_FILEIO);	
	    	}
	    	if (time != null) {
	    	    try {
	    	    	samples = time.getJSONObject("samples");
	    	    }
	    	    catch (JSONException e) {
	    	    	Preferences.debug("samples not found\n", Preferences.DEBUG_FILEIO);
	    	    }
	    	    if (samples != null) {
	    	    	try {
	    	            AcquisitionTime = samples.getJSONArray("AcquisitionTime");
	    	    	}
	    	    	catch (JSONException e) {
	    	    		Preferences.debug("JSONArray AcquisitionTime not found\n", Preferences.DEBUG_FILEIO);
	    	    	}
	    	    	if (AcquisitionTime != null) {
	    	    		AcquisitionTimeDouble = new double[AcquisitionTime.length()];
	    	    		for (i = 0; i < AcquisitionTime.length(); i++) {
	    	    			AcquisitionTimeDouble[i] = Double.NaN;
	    	    			try {
	    	    				AcquisitionTimeDouble[i] = AcquisitionTime.getDouble(i);
	    	    			}
	    	    			catch (JSONException e) {
	    	    				Preferences.debug("AcquisitionTime.getDouble("+i+") not found\n", Preferences.DEBUG_FILEIO);
	    	    			}
	    	    		}
	    	    		fileInfo.setAcquisitionTime(AcquisitionTimeDouble);
	    	    	} // if (AcquisitionTime != null)
	    	    	try {
	    	            AcquisitionNumber = samples.getJSONArray("AcquisitionNumber");
	    	    	}
	    	    	catch (JSONException e) {
	    	    		Preferences.debug("JSONArray AcquisitionNumber not found\n", Preferences.DEBUG_FILEIO);
	    	    	}
	    	    	if (AcquisitionNumber != null) {
	    	    		AcquisitionNumberInt = new int[AcquisitionNumber.length()];
	    	    		for (i = 0; i < AcquisitionNumber.length(); i++) {
	    	    			AcquisitionNumberInt[i] = Integer.MIN_VALUE;
	    	    			try {
	    	    				AcquisitionNumberInt[i] = AcquisitionNumber.getInt(i);
	    	    			}
	    	    			catch (JSONException e) {
	    	    				Preferences.debug("AcquisitionNumber.getInt("+i+") not found\n", Preferences.DEBUG_FILEIO);
	    	    			}
	    	    		}
	    	    		fileInfo.setAcquisitionNumber(AcquisitionNumberInt);
	    	    	} // if (AcquisitionNumber != null)
	    	    	try {
	    	            InstanceNumber = samples.getJSONArray("InstanceNumber");
	    	    	}
	    	    	catch (JSONException e) {
	    	    		Preferences.debug("JSONArray InstanceNumber not found\n", Preferences.DEBUG_FILEIO);
	    	    	}
	    	    	if (InstanceNumber != null) {
	    	    		InstanceNumberInt = new int[InstanceNumber.length()];
	    	    		for (i = 0; i < InstanceNumber.length(); i++) {
	    	    			InstanceNumberInt[i] = Integer.MIN_VALUE;
	    	    			try {
	    	    				InstanceNumberInt[i] = InstanceNumber.getInt(i);
	    	    			}
	    	    			catch (JSONException e) {
	    	    				Preferences.debug("InstanceNumber.getInt("+i+") not found\n", Preferences.DEBUG_FILEIO);
	    	    			}
	    	    		}
	    	    		fileInfo.setInstanceNumber(InstanceNumberInt);
	    	    	} // if (InstanceNumber != null)
	    	    	try {
	    	    		WindowCenter = samples.getJSONArray("WindowCenter");
	    	    	}
	    	    	catch (JSONException e) {
	    	    		Preferences.debug("JSONArray WindowCenter not found\n", Preferences.DEBUG_FILEIO);	
	    	    	}
	    	    	if (WindowCenter != null) {
	    	    		WindowCenterDouble = new double[WindowCenter.length()];
	    	    		for (i = 0; i < WindowCenter.length(); i++) {
	    	    			WindowCenterDouble[i] = Double.NaN;
	    	    			try {
	    	    				WindowCenterDouble[i] = WindowCenter.getDouble(i);
	    	    			}
	    	    			catch (JSONException e) {
	    	    				Preferences.debug("WindowCenter.getDouble("+i+") not found\n", Preferences.DEBUG_FILEIO);
	    	    			}
	    	    		}
	    	    		fileInfo.setWindowCenter(WindowCenterDouble);
	    	    	} // if (WindowCenter != null)
	    	    	try {
	    	    		WindowWidth = samples.getJSONArray("WindowWidth");
	    	    	}
	    	    	catch (JSONException e) {
	    	    		Preferences.debug("JSONArray WindowWidth not found\n", Preferences.DEBUG_FILEIO);	
	    	    	}
	    	    	if (WindowWidth != null) {
	    	    		WindowWidthDouble = new double[WindowWidth.length()];
	    	    		for (i = 0; i < WindowWidth.length(); i++) {
	    	    			WindowWidthDouble[i] = Double.NaN;
	    	    			try {
	    	    				WindowWidthDouble[i] = WindowWidth.getDouble(i);
	    	    			}
	    	    			catch (JSONException e) {
	    	    				Preferences.debug("WindowWidth.getDouble("+i+") not found\n", Preferences.DEBUG_FILEIO);
	    	    			}
	    	    		}
	    	    		fileInfo.setWindowWidth(WindowWidthDouble);
	    	    	} // if (WindowWidth != null)
	    	    	try {
	    	    		CsaImageTimeAfterStart = samples.getJSONArray("CsaImage.TimeAfterStart");
	    	    	}
	    	    	catch (JSONException e) {
	    	    		Preferences.debug("JSONArray CsaImage.TimeAfterStart not found\n", Preferences.DEBUG_FILEIO);
	    	    	}
	    	    	if (CsaImageTimeAfterStart != null) {
	    	    		CsaImageTimeAfterStartDouble = new double[CsaImageTimeAfterStart.length()];
	    	    	    for (i = 0; i < CsaImageTimeAfterStart.length(); i++) {
	    	    	    	CsaImageTimeAfterStartDouble[i] = Double.NaN;
	    	    	    	try {
	    	    	    		CsaImageTimeAfterStartDouble[i] = CsaImageTimeAfterStart.getDouble(i);
	    	    	    	}
	    	    	    	catch (JSONException e) {
	    	    	    		Preferences.debug("CsaImageTimeAfterStart.getDouble("+i+") not found\n", Preferences.DEBUG_FILEIO);
	    	    	    	}
	    	    	    }
	    	    	    fileInfo.setCsaImageTimeAfterStart(CsaImageTimeAfterStartDouble);
	    	    	} // if (CsaImageTimeAfterStart != null)
	    	    	try {
	    	    		CsaImageMosaicRefAcqTimes = samples.getJSONArray("CsaImage.MosaicRefAcqTimes");
	    	    	}
	    	    	catch (JSONException e) {
	    	    		Preferences.debug("CsaImage.MosaicRefAcqTimes not found\n", Preferences.DEBUG_FILEIO);
	    	    	}
	    	    	if (CsaImageMosaicRefAcqTimes != null) {
	    	    	    CsaImageMosaicRefAcqTimesDouble = new double[CsaImageMosaicRefAcqTimes.length()][];
	    	    	    for (i = 0; i < CsaImageMosaicRefAcqTimes.length(); i++) {
	    	    	    	try {
	    	    	    		CsaImageMosaicRefAcqTimes_inner = CsaImageMosaicRefAcqTimes.getJSONArray(i);
	    	    	    	}
	    	    	    	catch (JSONException e) {
	    	    	    		Preferences.debug("CsaImageMosaicRefAcqTimes.getJSONArray("+i+") not found\n", Preferences.DEBUG_FILEIO);
	    	    	    	}
	    	    	    	if (CsaImageMosaicRefAcqTimes_inner != null) {
	    	    	    		CsaImageMosaicRefAcqTimesDouble[i] = new double[CsaImageMosaicRefAcqTimes_inner.length()];
	    	    	    		for (j = 0; j < CsaImageMosaicRefAcqTimes_inner.length(); j++) {
	    	    	    			CsaImageMosaicRefAcqTimesDouble[i][j] = Double.NaN;
	    	    	    			try {
	    	    	    				CsaImageMosaicRefAcqTimesDouble[i][j] = CsaImageMosaicRefAcqTimes_inner.getDouble(j);
	    	    	    			}
	    	    	    			catch (JSONException e) {
	    	    	    				Preferences.debug("CsaImageMosaicRefAcqTimesDouble["+i+"]["+j+"] not found\n",
	    	    	    						Preferences.DEBUG_FILEIO);
	    	    	    			}
	    	    	    		}
	    	    	    	}
	    	    	    }
	    	    	    fileInfo.setCsaImageMosaicRefAcqTimes(CsaImageMosaicRefAcqTimesDouble);
	    	    	} // if (CsaImageMosaicRefAcqTimes != null)
	    	    	try {
	    	    		CsaImageICE_Dims = samples.getJSONArray("CsaImage.ICE_Dims");
	    	    	}
	    	    	catch (JSONException e) {
	    	    		Preferences.debug("JSONArray.CsaImageICE_Dims not found\n", Preferences.DEBUG_FILEIO);
	    	    	}
	    	    	if (CsaImageICE_Dims != null) {
	    	    		CsaImageICE_DimsString = new String[CsaImageICE_Dims.length()];
	    	    		for (i = 0; i < CsaImageICE_Dims.length(); i++) {
	    	    			CsaImageICE_DimsString[i] = null;
	    	    			try {
	    	    				CsaImageICE_DimsString[i] = CsaImageICE_Dims.getString(i);	
	    	    			}
	    	    			catch (JSONException e) {
	    	    				Preferences.debug("CsaImageICE_Dims.getString("+i+") not found\n", Preferences.DEBUG_FILEIO);
	    	    			}
	    	    		}
	    	    		fileInfo.setCsaImageICE_Dims(CsaImageICE_DimsString);
	    	    	} // if (CsaImageICE_Dims != null)
	    	    	try {
	    	    		CsaImageSliceMeasurementDuration = samples.getJSONArray("CsaImage.SliceMeasurementDuration");
	    	    	}
	    	    	catch (JSONException e) {
	    	    		Preferences.debug("CsaImage.SliceMeasurementDuration not found\n", Preferences.DEBUG_FILEIO);
	    	    	}
	    	    	if (CsaImageSliceMeasurementDuration != null) {
	    	    		CsaImageSliceMeasurementDurationDouble = new double[CsaImageSliceMeasurementDuration.length()];
	    	    		for (i = 0; i < CsaImageSliceMeasurementDuration.length(); i++) {
	    	    			CsaImageSliceMeasurementDurationDouble[i] = Double.NaN;
	    	    			try {
	    	    				CsaImageSliceMeasurementDurationDouble[i] = CsaImageSliceMeasurementDuration.getDouble(i);
	    	    			}
	    	    			catch(JSONException e) {
	    	    				Preferences.debug("CsaImageSliceMeasurementDurationDouble.get("+i+") not found\n", 
	    	    						Preferences.DEBUG_FILEIO);
	    	    			}
	    	    		}
	    	    		fileInfo.setCsaImageSliceMeasurementDuration(CsaImageSliceMeasurementDurationDouble);
	    	    	} // if (CsaImageSliceMeasurementDuration != null) 
	    	    	try {
	    	    		InstanceCreationTime = samples.getJSONArray("InstanceCreationTime");
	    	    	}
	    	    	catch (JSONException e) {
	    	    		Preferences.debug("InstanceCreationTime not found\n", Preferences.DEBUG_FILEIO);
	    	    	}
	    	    	if (InstanceCreationTime != null) {
	    	    		InstanceCreationTimeDouble = new double[InstanceCreationTime.length()];
	    	    		for (i = 0; i < InstanceCreationTime.length(); i++) {
	    	    			InstanceCreationTimeDouble[i] = Double.NaN;
	    	    			try {
	    	    				InstanceCreationTimeDouble[i] = InstanceCreationTime.getDouble(i);
	    	    			}
	    	    			catch (JSONException e) {
	    	    				Preferences.debug("InstanceCreationTime.getDouble("+i+") not found\n", Preferences.DEBUG_FILEIO);
	    	    			}
	    	    		}
	    	    		fileInfo.setInstanceCreationTime(InstanceCreationTimeDouble);
	    	    	} // if (InstanceCreationTime != null)
	    	    	try {
	    	    		ContentTime = samples.getJSONArray("ContentTime");
	    	    	}
	    	    	catch (JSONException e) {
	    	    		Preferences.debug("ContentTime not found\n", Preferences.DEBUG_FILEIO);
	    	    	}
	    	    	if (ContentTime != null) {
	    	    		ContentTimeDouble = new double[ContentTime.length()];
	    	    		for (i = 0; i < ContentTime.length(); i++) {
	    	    			ContentTimeDouble[i] = Double.NaN;
	    	    			try {
	    	    				ContentTimeDouble[i] = ContentTime.getDouble(i);
	    	    			}
	    	    			catch (JSONException e) {
	    	    				Preferences.debug("ContentTime.getDouble("+i+") not found\n", Preferences.DEBUG_FILEIO);
	    	    			}
	    	    		}
	    	    		fileInfo.setContentTime(ContentTimeDouble);
	    	    	} // if (ContentTime != null)
	    	    } // if (samples != null)
	    	} // if (time != null)
	        try {
	    		dcmmeta_shape = jsonObject.getJSONArray("dcmmeta_shape");
	    	}
	    	catch(JSONException e) {
	    		Preferences.debug("JSONArray dcmmeta_shape not found\n", Preferences.DEBUG_FILEIO);
	    	}
	        if (dcmmeta_shape != null) {
	        	dcmmeta_shapeInt = new int[dcmmeta_shape.length()];
	        	for (i = 0; i < dcmmeta_shape.length(); i++) {
	        		dcmmeta_shapeInt[i] = Integer.MIN_VALUE;
	        		try {
	        			dcmmeta_shapeInt[i] = dcmmeta_shape.getInt(i);
	        		}
	        		catch(JSONException e) {
	        			Preferences.debug("dcmmeta_shape.getInt("+i+") not found\n", Preferences.DEBUG_FILEIO);
	        		}
	        	}
	        	fileInfo.setDcmmeta_shape(dcmmeta_shapeInt);
	        } // if (dcmmeta_shape != null)
	        try {
	        	dcmmeta_affine = jsonObject.getJSONArray("dcmmeta_affine");
	        }
	        catch (JSONException e) {
	        	Preferences.debug("JSONArray dcmmeta_affine not found\n", Preferences.DEBUG_FILEIO);
	        }
	        if (dcmmeta_affine != null) {
	        	dcmmeta_affineDouble = new double[dcmmeta_affine.length()][];
	        	for (i = 0; i < dcmmeta_affine.length(); i++) {
	        		try {
	        			dcmmeta_affine_inner = dcmmeta_affine.getJSONArray(i);
	        		}
	        		catch (JSONException e) {
	        			Preferences.debug("dcmmeta_affine.getJSONArray("+i+") not found\n", Preferences.DEBUG_FILEIO);
	        		}
	        		if (dcmmeta_affine_inner != null) {
		        		dcmmeta_affineDouble[i] = new double[dcmmeta_affine_inner.length()];
		        		for (j = 0; j < dcmmeta_affine_inner.length(); j++) {
		        			dcmmeta_affineDouble[i][j] = Double.NaN;
		        		    try {
		        		    	dcmmeta_affineDouble[i][j] = dcmmeta_affine_inner.getDouble(j);
		        		    }
		        		    catch (JSONException e) {
		        		    	Preferences.debug("dcmmeta_affineDouble["+i+"]["+j+"] not found\n", Preferences.DEBUG_FILEIO);
		        		    }
		        		}
	        		}
	        	}
	        	fileInfo.setDcmmeta_affine(dcmmeta_affineDouble);
	        } // if (dcmmeta_affine != null)
	        try {
	        	dcmmeta_reorient_transform = jsonObject.getJSONArray("dcmmeta_reorient_transform");
	        }
	        catch (JSONException e) {
	        	Preferences.debug("JSONArray dcmmeta_reorient_transform not found\n", Preferences.DEBUG_FILEIO);
	        }
	        if (dcmmeta_reorient_transform != null) {
	        	dcmmeta_reorient_transformDouble = new double[dcmmeta_reorient_transform.length()][];
	        	for (i = 0; i < dcmmeta_reorient_transform.length(); i++) {
	        		try {
	        			dcmmeta_reorient_transform_inner = dcmmeta_reorient_transform.getJSONArray(i);
	        		}
	        		catch (JSONException e) {
	        		    Preferences.debug("dcmmeta_reorient_transform.getJSONArray("+i+") not found\n", Preferences.DEBUG_FILEIO);
	        		}
	        		if (dcmmeta_reorient_transform_inner != null) {
		        		dcmmeta_reorient_transformDouble[i] = new double[dcmmeta_reorient_transform_inner.length()];
		        		for (j = 0; j < dcmmeta_reorient_transform_inner.length(); j++) {
		        			dcmmeta_reorient_transformDouble[i][j] = Double.NaN;
		        			try {
		        				dcmmeta_reorient_transformDouble[i][j] = dcmmeta_reorient_transform_inner.getDouble(j);
		        			}
		        			catch (JSONException e) {
		        				Preferences.debug("dcmmeta_reorient_transformDouble["+i+"]["+j+"] not found\n", Preferences.DEBUG_FILEIO);
		        			}
		        		}
	        		}
	        	}
	        	fileInfo.setDcmmeta_reorient_transform(dcmmeta_reorient_transformDouble);
	        } // if (dcmmeta_reorient_transform != null)
	        try {
	    		dcmmeta_slice_dim = jsonObject.getInt("dcmmeta_slice_dim");
	    	}
	    	catch (JSONException e) {
	    		Preferences.debug("dcmmeta_slice_dim not found\n", Preferences.DEBUG_FILEIO);
	    	}
	    	if (dcmmeta_slice_dim != Integer.MIN_VALUE) {
	    		fileInfo.setDcmmeta_slice_dim(dcmmeta_slice_dim);
	    	}
	    	try {
	    		dcmmeta_version = jsonObject.getDouble("dcmmeta_version");
	    	}
	    	catch (JSONException e) {
	    		Preferences.debug("dcmmeta_version not found\n", Preferences.DEBUG_FILEIO);
	    	}
	    	if (!Double.isNaN(dcmmeta_version)) {
	    		fileInfo.setDcmmeta_version(dcmmeta_version);
	    	}
	    } // if (!noReadPrivateTags)
    }

    /**
     * Reads a NIFTI image file by reading the header then making a FileRaw to read the image for all filenames in the
     * file list. Only the one file directory (currently) supported.
     *
     * @param      one  flag indicating one image of a 3D dataset should be read in.
     * @param	   niftiCompressed  boolean indicating if file being read is a compressed nifti    .nii.gz, .nii.zip, or .nii.bz2
     *
     * @exception  IOException  if there is an error reading the file
     *
     * @return     The image.
     *
     * @see        FileRaw
     */
    public ModelImage readImage(boolean one, boolean niftiCompressed, boolean noImportData, 
    		boolean noReadPrivateTags) throws IOException, OutOfMemoryError {
        long offset;
        double m1, m2;
        boolean needFloat;
        int newType;
        boolean doChangeType;
        AlgorithmChangeType changeTypeAlgo;
        fileInfo = new FileInfoNIFTI(fileName, fileDir, FileUtility.NIFTI);
        boolean flip = false;
        int i;
        
        
        if(niftiCompressed) {
        	file = new File(fileDir + File.separator + fileName); 
        	String ext = fileName.substring(fileName.lastIndexOf(".")+1, fileName.length());


        	if (ext.equalsIgnoreCase("zip")) {

                try {
                    fis = new FileInputStream(file);
                } catch (FileNotFoundException e) {
                    MipavUtil.displayError("File not found exception on fis = new FileInputStream(file) for " +
                            (fileDir + File.separator + fileName));
                    return null;
                }
                try {
                    zin = new ZipInputStream(new BufferedInputStream(fis));
                } catch (Exception e) {
                    MipavUtil.displayError("Exception on ZipInputStream for " + fileName);
                    return null;
                }
                if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory(),true, noReadPrivateTags)) {
                    throw (new IOException(" NIFTI header file error"));
                }
        	}else if(ext.equalsIgnoreCase("gz")) {
        		try {
                    fis = new FileInputStream(file);
                } catch (FileNotFoundException e) {
                    MipavUtil.displayError("File not found exception on fis = new FileInputStream(file) for " +
                            (fileDir + File.separator + fileName));
                    return null;
                }
                try {
                    gzin = new GZIPInputStream(new BufferedInputStream(fis));
                } catch (IOException e) {
                    MipavUtil.displayError("IOException on GZIPInputStream for " + fileName);
                    return null;
                }
                if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory(),true, noReadPrivateTags)) {
                    throw (new IOException(" NIFTI header file error"));
                }
        	}else if(ext.equalsIgnoreCase("bz2")) {
        		try {
                    fis = new FileInputStream(file);
                } catch (FileNotFoundException e) {
                    MipavUtil.displayError("File not found exception on fis = new FileInputStream(file) for " + 
                            (fileDir + File.separator + fileName));
                    return null;
                }
                
                try {
                fis.read();
                }
                catch (IOException e) {
                    MipavUtil.displayError("IOException on fis.read() trying to read byte B");
                    return null;
                }
                
                try {
                    fis.read();
                    }
                    catch (IOException e) {
                        MipavUtil.displayError("IOException on fis.read() trying to read byte Z");
                        return null;
                    }

                try {
                    bz2in = new CBZip2InputStream(new BufferedInputStream(fis));
                } catch (Exception e) {
                    MipavUtil.displayError("Exception on CBZip2InputStream for " + fileName);
                    return null;
                }
                if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory(),true, noReadPrivateTags)) {
                    throw (new IOException(" NIFTI header file error"));
                }
        	}
        }else {
        	if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory(),false, noReadPrivateTags)) {
                throw (new IOException(" NIFTI header file error"));
            }
        }

        

        int[] extents = null;

        try {

            if (one) {
                extents = new int[fileInfo.getExtents().length];

                for (i = 0; i < extents.length; i++) {
                    extents[i] = fileInfo.getExtents()[i];
                }

                image = new ModelImage(fileInfo.getDataType(), new int[] { extents[0], extents[1] },
                                       fileInfo.getFileName());
            } else {
                image = new ModelImage(fileInfo.getDataType(), fileInfo.getExtents(), fileInfo.getFileName());
            }
        } catch (OutOfMemoryError error) {
            throw (error);
        }
        
        axisOrientation = fileInfo.getAxisOrientation();
        if ((Preferences.is(Preferences.PREF_FLIP_NIFTI_READ)) &&
           ((axisOrientation[1] == FileInfoBase.ORI_P2A_TYPE) || (axisOrientation[1] == FileInfoBase.ORI_I2S_TYPE))) {
        	int orient = axisOrientation[1];
            flip = true;
            if (axisOrientation[1] == FileInfoBase.ORI_P2A_TYPE) {
                axisOrientation[1] = FileInfoBase.ORI_A2P_TYPE;
            }
            else {
                axisOrientation[1] = FileInfoBase.ORI_S2I_TYPE;
            }
            fileInfo.setAxisOrientation(axisOrientation);
            LPSOrigin = fileInfo.getOrigin();

                if ((orient == FileInfoBase.ORI_R2L_TYPE) || 
                        (orient == FileInfoBase.ORI_A2P_TYPE) || 
                        (orient == FileInfoBase.ORI_I2S_TYPE)) {
                    LPSOrigin[1] = LPSOrigin[1] + ((fileInfo.getExtents()[1] - 1) * fileInfo.getResolutions()[1]);
                } else {
                    LPSOrigin[1] = LPSOrigin[1] - ((fileInfo.getExtents()[1] - 1) * fileInfo.getResolutions()[1]);
                }
            fileInfo.setOrigin(LPSOrigin);
            
            matrix.set(0, 1, -matrix.get(0, 1));
            matrix.set(1, 1, -matrix.get(1, 1));
            matrix.set(2, 1, -matrix.get(2, 1)); 
            matrix.set(0, 3, LPSOrigin[0]);
            matrix.set(1, 3, LPSOrigin[1]);
            matrix.set(2, 3, LPSOrigin[2]);
                          
            if (matrix2 != null) {
                matrix2.set(0, 1, -matrix2.get(0, 1));
                matrix2.set(1, 1, -matrix2.get(1, 1));
                matrix2.set(2, 1, -matrix2.get(2, 1)); 
                matrix2.set(0, 3, LPSOrigin[0]);
                matrix2.set(1, 3, LPSOrigin[1]); 
                matrix2.set(2, 3, LPSOrigin[2]);
            } // if (matrix2 != null)
                    
        } // if ((Preferences.is(Preferences.PREF_FLIP_NIFTI_READ)) &&

        extents = fileInfo.getExtents();
        if (image.getNDims() == 2) {
            image.setFileInfo(fileInfo, 0); // Otherwise just set the first fileInfo
        } else if (image.getNDims() == 3) { // If there is more than one image

            for (i = 0; i < extents[2]; i++) {
                FileInfoNIFTI newFileInfo = (FileInfoNIFTI) fileInfo.clone();
                newFileInfo.setOrigin(fileInfo.getOriginAtSlice(i));
                image.setFileInfo(newFileInfo, i); // Set the array of fileInfos in ModelImage
            }
        } else if (image.getNDims() == 4) { // If there is more than one image
            for (i = 0; i < (extents[2] * extents[3]); i++) {
                FileInfoNIFTI newFileInfo = (FileInfoNIFTI) fileInfo.clone();
                newFileInfo.setOrigin(fileInfo.getOriginAtSlice(i));
                image.setFileInfo(newFileInfo, i); // Set the array of fileInfos in ModelImage
            }
        }
        updateorigins(image.getFileInfo());
        if (image.getNDims() >= 3) {
            image.setMatrix(matrix);
        }
        else {
            image.setMatrix(matrixTwoDim);
        }

        if (matrix2 != null) {
            image.getMatrixHolder().addMatrix(matrix2);
        }
        
        if (noImportData) {
        	//close the compressed streams if open
            if(zin != null) {
            	zin.close();
            }
            if(gzin != null) {
            	gzin.close();
            }
            if(bz2in != null) {
            	bz2in.close();
            }	
            fireProgressStateChanged(100);
            return image;
        } // if (noImportData)

        try { // Construct a FileRaw to actually read the image.

            

            if (oneFile) {
                offset = (long) Math.abs(vox_offset);

                if (offset < headerSize) { // header length
                    offset = headerSize;
                }
            } else {
                offset = 0L;
            }

            if (one) {

                if (fileInfo.getExtents().length > 2) {
                    offset = offset + getOffset(fileInfo);
                }
            }
            
            
            if(niftiCompressed) {
            	byte[]buffer;
            	if(image.getType() == ModelStorageBase.ARGB) {
            		buffer = new byte[255];
            	}else if(image.getType() == ModelStorageBase.ARGB_USHORT) {
            		buffer = new byte[4];
            	}else if(image.getType() == ModelStorageBase.ARGB_FLOAT) {
            		buffer = new byte[8];
            	}else {
            		buffer = new byte[256];
            		
            	}	
            	
            	
            	int bytesRead;
            	String ext = fileName.substring(fileName.lastIndexOf(".")+1, fileName.length());
            	if (ext.equalsIgnoreCase("zip")) {
            		
                    int start = 0;
                    boolean endianness = fileInfo.getEndianess();
                    int type = image.getType();
                    
                    while (true) {
                        try {
                            bytesRead = zin.read(buffer);
                            if(bytesRead == -1) {
                           	 break;
                            }
                            
                            
                            
                            
                            if(image.getType() == ModelStorageBase.ARGB) {
                            	if(bytesRead != 255) {
                                 	 buffer = getFullBuffer(zin,buffer,bytesRead,255); 
                                  }
                        	}else if(image.getType() == ModelStorageBase.ARGB_USHORT) {
                        		if(bytesRead != 4) {
                                 	 buffer = getFullBuffer(zin,buffer,bytesRead,4); 
                                  }
                        	}else if(image.getType() == ModelStorageBase.ARGB_FLOAT) {
                        		if(bytesRead != 8) {
                                 	 buffer = getFullBuffer(zin,buffer,bytesRead,8); 
                                 }
                        	}else {
                        		if(bytesRead != 256) {
                                 	 buffer = getFullBuffer(zin,buffer,bytesRead,256); 
                                 }
                        	}


                        	if(type == ModelStorageBase.BYTE || type == ModelStorageBase.UBYTE || type == ModelStorageBase.BOOLEAN) {
                        		image.importData(start, buffer, false);
                        		if(start < image.getDataSize()) {
                        			start = start + buffer.length;
                        		}
                        	}else if(type == ModelStorageBase.SHORT || type == ModelStorageBase.USHORT ) {
                        		short[] shortBuff = new short[buffer.length/2];
                        		for(int m=0,k=0;m<buffer.length;m=m+2,k++) {
                        			byte[] b = {buffer[m],buffer[m+1]};
                        			shortBuff[k] = FileBase.bytesToShort(endianness, 0, b);
                        		}
                        		if(start < image.getDataSize()) {
                        			image.importData(start, shortBuff, false);
                        		}
                        		start = start + shortBuff.length;
                        	}else if(type == ModelStorageBase.INTEGER || type == ModelStorageBase.UINTEGER) {
                        		int[] intBuff = new int[buffer.length/4];
                        		for(int m=0,k=0;m<buffer.length;m=m+4,k++) {
                        			byte[] b = {buffer[m],buffer[m+1],buffer[m+2],buffer[m+3]};
                        			intBuff[k] = FileBase.bytesToInt(endianness, 0, b);
                        		}
                        		if(start < image.getDataSize()) {
                        			image.importData(start, intBuff, false);
                        		}
                        		start = start + intBuff.length;
                        	}else if(type == ModelStorageBase.FLOAT) {
                        		float[] floatBuff = new float[buffer.length/4];
                        		for(int m=0,k=0;m<buffer.length;m=m+4,k++) {
                        			byte[] b = {buffer[m],buffer[m+1],buffer[m+2],buffer[m+3]};
                        			floatBuff[k] = FileBase.bytesToFloat(endianness, 0, b);
                        		}
                        		if(start < image.getDataSize()) {
                        			image.importData(start, floatBuff, false);
                        		}
                        		start = start + floatBuff.length;                             
                        		                              
                        	}else if(type == ModelStorageBase.DOUBLE) {
                        		double[] doubleBuff = new double[buffer.length/8];
                        		for(int m=0,k=0;m<buffer.length;m=m+8,k++) {
                        			byte[] b = {buffer[m],buffer[m+1],buffer[m+2],buffer[m+3],buffer[m+4],buffer[m+5],buffer[m+6],buffer[m+7]};
                        			doubleBuff[k] = FileBase.bytesToDouble(endianness, 0, b);
                        		}
                        		if(start < image.getDataSize()) {
                        			image.importData(start, doubleBuff, false);
                        		}
                        		start = start + doubleBuff.length;                             
                        		                              
                        	}else if(type == ModelStorageBase.ARGB) {
                        		byte[] buff2 = new byte[buffer.length + buffer.length/3];
                        		if(start < image.getDataSize()) {
                        			int counter = 0;
                        			for(int m=0;m<buffer.length;m=m+3) {
                            			buff2[counter] = 1;
                            			buff2[counter+1] = buffer[m];
                            			buff2[counter+2] = buffer[m+1];
                            			buff2[counter+3] = buffer[m+2];
                            			counter = counter+4;
                        			}
                        			image.importData(start, buff2, false);
                        		}
                        		start = start + buff2.length;
                        	}else if(type == ModelStorageBase.ARGB_USHORT) {
                        		//short[] shortBuff = new short[buffer.length/2];
                        		short[] shortBuff2 = new short[3];
                        		shortBuff2[0] = 1;
                        		
                        		if(start < image.getDataSize()) {
                        			for(int m=0,k=1;m<buffer.length;m=m+2,k++) {
                            			byte[] b = {buffer[m],buffer[m+1]};
                            			shortBuff2[k] = FileBase.bytesToShort(endianness, 0, b);
                            		}
                        			image.importData(start, shortBuff2, false);
                        		}
                        		start = start + shortBuff2.length;
                        	}else if(type == ModelStorageBase.ARGB_FLOAT) {
                        		//float[] floatBuff = new float[buffer.length/4];
                        		float[] floatBuff2 = new float[3];
                        		floatBuff2[0] = 1;
                        		
                        		if(start < image.getDataSize()) {
                        			for(int m=0,k=1;m<buffer.length;m=m+4,k++) {
                            			byte[] b = {buffer[m],buffer[m+1],buffer[m+2],buffer[m+3]};
                            			floatBuff2[k] = FileBase.bytesToFloat(endianness, 0, b);
                            		}
                        			image.importData(start, floatBuff2, false);
                        		}
                        		start = start + floatBuff2.length;
                        	}
                        } catch (IOException e) {
                        	e.printStackTrace();
                            MipavUtil.displayError("IOException on gzin.read(buffer) for " + fileName);
                            return null;
                        } 
                    }
            	}else if(ext.equalsIgnoreCase("gz")) {
            		

                    int start = 0;
                    boolean endianness = fileInfo.getEndianess();
                    int type = image.getType();
                    while (true) {
                        try {
                            bytesRead = gzin.read(buffer);
                            if(bytesRead == -1) {
                           	 break;
                            }
                            if(image.getType() == ModelStorageBase.ARGB) {
                            	if(bytesRead != 255) {
                                 	 buffer = getFullBuffer(gzin,buffer,bytesRead,255); 
                                  }
                        	}else if(image.getType() == ModelStorageBase.ARGB_USHORT) {
                        		if(bytesRead != 4) {
                                 	 buffer = getFullBuffer(gzin,buffer,bytesRead,4); 
                                  }
                        	}else if(image.getType() == ModelStorageBase.ARGB_FLOAT) {
                        		if(bytesRead != 8) {
                                 	 buffer = getFullBuffer(gzin,buffer,bytesRead,8); 
                                 }
                        	}else {
                        		if(bytesRead != 256) {
                                 	 buffer = getFullBuffer(gzin,buffer,bytesRead,256); 
                                 }
                        	}

                            


                        	if(type == ModelStorageBase.BYTE || type == ModelStorageBase.UBYTE  || type == ModelStorageBase.BOOLEAN) {
                        		if(start < image.getDataSize()) {
                        			image.importData(start, buffer, false);
                        		}
                        		start = start + buffer.length;
                        	}else if(type == ModelStorageBase.SHORT || type == ModelStorageBase.USHORT ) {
                        		short[] shortBuff = new short[buffer.length/2];
                        		for(int m=0,k=0;m<buffer.length;m=m+2,k++) {
                        			byte[] b = {buffer[m],buffer[m+1]};
                        			shortBuff[k] = FileBase.bytesToShort(endianness, 0, b);
                        		}
                        		if(start < image.getDataSize()) {
                        			image.importData(start, shortBuff, false);
                        		}
                        		
                        		start = start + shortBuff.length;
                        	}else if(type == ModelStorageBase.INTEGER || type == ModelStorageBase.UINTEGER) {
                        		int[] intBuff = new int[buffer.length/4];
                        		for(int m=0,k=0;m<buffer.length;m=m+4,k++) {
                        			byte[] b = {buffer[m],buffer[m+1],buffer[m+2],buffer[m+3]};
                        			intBuff[k] = FileBase.bytesToInt(endianness, 0, b);
                        		}
                        		if(start < image.getDataSize()) {
                        			image.importData(start, intBuff, false);
                        		}
                        		start = start + intBuff.length;
                        	}else if(type == ModelStorageBase.FLOAT) {
                        		float[] floatBuff = new float[buffer.length/4];
                        		for(int m=0,k=0;m<buffer.length;m=m+4,k++) {
                        			byte[] b = {buffer[m],buffer[m+1],buffer[m+2],buffer[m+3]};
                        			floatBuff[k] = FileBase.bytesToFloat(endianness, 0, b);
                        		}
                        		if(start < image.getDataSize()) {
                        			image.importData(start, floatBuff, false);
                        		}
                        		start = start + floatBuff.length;                             
                        		                              
                        	}else if(type == ModelStorageBase.DOUBLE) {
                        		double[] doubleBuff = new double[buffer.length/8];
                        		for(int m=0,k=0;m<buffer.length;m=m+8,k++) {
                        			byte[] b = {buffer[m],buffer[m+1],buffer[m+2],buffer[m+3],buffer[m+4],buffer[m+5],buffer[m+6],buffer[m+7]};
                        			doubleBuff[k] = FileBase.bytesToDouble(endianness, 0, b);
                        		}
                        		if(start < image.getDataSize()) {
                        			image.importData(start, doubleBuff, false);
                        		}
                        		start = start + doubleBuff.length;                             
                        		                              
                        	}else if(type == ModelStorageBase.ARGB) {
                        		byte[] buff2 = new byte[buffer.length + buffer.length/3];
                        		if(start < image.getDataSize()) {
                        			int counter = 0;
                        			for(int m=0;m<buffer.length;m=m+3) {
                            			buff2[counter] = 1;
                            			buff2[counter+1] = buffer[m];
                            			buff2[counter+2] = buffer[m+1];
                            			buff2[counter+3] = buffer[m+2];
                            			counter = counter+4;
                        			}
                        			image.importData(start, buff2, false);
                        		}
                        		start = start + buff2.length;
                        	}else if(type == ModelStorageBase.ARGB_USHORT) {
                        		//short[] shortBuff = new short[buffer.length/2];
                        		short[] shortBuff2 = new short[3];
                        		shortBuff2[0] = 1;
                        		
                        		if(start < image.getDataSize()) {
                        			for(int m=0,k=1;m<buffer.length;m=m+2,k++) {
                            			byte[] b = {buffer[m],buffer[m+1]};
                            			shortBuff2[k] = FileBase.bytesToShort(endianness, 0, b);
                            		}
                        			image.importData(start, shortBuff2, false);
                        		}
                        		start = start + shortBuff2.length;
                        	}else if(type == ModelStorageBase.ARGB_FLOAT) {
                        		//float[] floatBuff = new float[buffer.length/4];
                        		float[] floatBuff2 = new float[3];
                        		floatBuff2[0] = 1;
                        		
                        		if(start < image.getDataSize()) {
                        			for(int m=0,k=1;m<buffer.length;m=m+4,k++) {
                            			byte[] b = {buffer[m],buffer[m+1],buffer[m+2],buffer[m+3]};
                            			floatBuff2[k] = FileBase.bytesToFloat(endianness, 0, b);
                            		}
                        			image.importData(start, floatBuff2, false);
                        		}
                        		start = start + floatBuff2.length;
                        	}
                        } catch (IOException e) {
                            Preferences.debug("IOException on gzin.read(buffer) for " + fileName, Preferences.DEBUG_FILEIO);
                            throw e;
                        } 
                    }
            	}else if(ext.equalsIgnoreCase("bz2")) {
            		
            		
                    int start = 0;
                    boolean endianness = fileInfo.getEndianess();
                    int type = image.getType();
                    while (true) {
                        try {
                            bytesRead = bz2in.read(buffer);
                            if(bytesRead == -1) {
                           	 break;
                            }

                            if(image.getType() == ModelStorageBase.ARGB) {
                            	if(bytesRead != 255) {
                                 	 buffer = getFullBuffer(bz2in,buffer,bytesRead,255); 
                                  }
                        	}else if(image.getType() == ModelStorageBase.ARGB_USHORT) {
                        		if(bytesRead != 4) {
                                 	 buffer = getFullBuffer(bz2in,buffer,bytesRead,4); 
                                  }
                        	}else if(image.getType() == ModelStorageBase.ARGB_FLOAT) {
                        		if(bytesRead != 8) {
                                 	 buffer = getFullBuffer(bz2in,buffer,bytesRead,8); 
                                 }
                        	}else {
                        		if(bytesRead != 256) {
                                 	 buffer = getFullBuffer(bz2in,buffer,bytesRead,256); 
                                 }
                        	}


                        	if(type == ModelStorageBase.BYTE || type == ModelStorageBase.UBYTE  || type == ModelStorageBase.BOOLEAN) {
                        		if(start < image.getDataSize()) {
                        			image.importData(start, buffer, false);
                        		}
                        		start = start + buffer.length;
                        	}else if(type == ModelStorageBase.SHORT || type == ModelStorageBase.USHORT ) {
                        		short[] shortBuff = new short[buffer.length/2];
                        		for(int m=0,k=0;m<buffer.length;m=m+2,k++) {
                        			byte[] b = {buffer[m],buffer[m+1]};
                        			shortBuff[k] = FileBase.bytesToShort(endianness, 0, b);
                        		}
                        		if(start < image.getDataSize()) {
                        			image.importData(start, shortBuff, false);
                        		}
                        		start = start + shortBuff.length;
                        	}else if(type == ModelStorageBase.INTEGER || type == ModelStorageBase.UINTEGER) {
                        		int[] intBuff = new int[buffer.length/4];
                        		for(int m=0,k=0;m<buffer.length;m=m+4,k++) {
                        			byte[] b = {buffer[m],buffer[m+1],buffer[m+2],buffer[m+3]};
                        			intBuff[k] = FileBase.bytesToInt(endianness, 0, b);
                        		}
                        		if(start < image.getDataSize()) {
                        			image.importData(start, intBuff, false);
                        		}
                        		start = start + intBuff.length;
                        	}else if(type == ModelStorageBase.FLOAT) {
                        		float[] floatBuff = new float[buffer.length/4];
                        		for(int m=0,k=0;m<buffer.length;m=m+4,k++) {
                        			byte[] b = {buffer[m],buffer[m+1],buffer[m+2],buffer[m+3]};
                        			floatBuff[k] = FileBase.bytesToFloat(endianness, 0, b);
                        		}
                        		if(start < image.getDataSize()) {
                        			image.importData(start, floatBuff, false);
                        		}
                        		start = start + floatBuff.length;                             
                        		                              
                        	}else if(type == ModelStorageBase.DOUBLE) {
                        		double[] doubleBuff = new double[buffer.length/8];
                        		for(int m=0,k=0;m<buffer.length;m=m+8,k++) {
                        			byte[] b = {buffer[m],buffer[m+1],buffer[m+2],buffer[m+3],buffer[m+4],buffer[m+5],buffer[m+6],buffer[m+7]};
                        			doubleBuff[k] = FileBase.bytesToDouble(endianness, 0, b);
                        		}
                        		if(start < image.getDataSize()) {
                        			image.importData(start, doubleBuff, false);
                        		}
                        		start = start + doubleBuff.length;                             
                        		                              
                        	}else if(type == ModelStorageBase.ARGB) {
                        		byte[] buff2 = new byte[buffer.length + buffer.length/3];
                        		if(start < image.getDataSize()) {
                        			int counter = 0;
                        			for(int m=0;m<buffer.length;m=m+3) {
                            			buff2[counter] = 1;
                            			buff2[counter+1] = buffer[m];
                            			buff2[counter+2] = buffer[m+1];
                            			buff2[counter+3] = buffer[m+2];
                            			counter = counter+4;
                        			}
                        			image.importData(start, buff2, false);
                        		}
                        		start = start + buff2.length;
                        	}else if(type == ModelStorageBase.ARGB_USHORT) {
                        		//short[] shortBuff = new short[buffer.length/2];
                        		short[] shortBuff2 = new short[3];
                        		shortBuff2[0] = 1;
                        		
                        		if(start < image.getDataSize()) {
                        			for(int m=0,k=1;m<buffer.length;m=m+2,k++) {
                            			byte[] b = {buffer[m],buffer[m+1]};
                            			shortBuff2[k] = FileBase.bytesToShort(endianness, 0, b);
                            		}
                        			image.importData(start, shortBuff2, false);
                        		}
                        		start = start + shortBuff2.length;
                        	}else if(type == ModelStorageBase.ARGB_FLOAT) {
                        		//float[] floatBuff = new float[buffer.length/4];
                        		float[] floatBuff2 = new float[3];
                        		floatBuff2[0] = 1;
                        		
                        		if(start < image.getDataSize()) {
                        			for(int m=0,k=1;m<buffer.length;m=m+4,k++) {
                            			byte[] b = {buffer[m],buffer[m+1],buffer[m+2],buffer[m+3]};
                            			floatBuff2[k] = FileBase.bytesToFloat(endianness, 0, b);
                            		}
                        			image.importData(start, floatBuff2, false);
                        		}
                        		start = start + floatBuff2.length;
                        	}
                        } catch (IOException e) {
                        	Preferences.debug("IOException on gzin.read(buffer) for " + fileName, Preferences.DEBUG_FILEIO);
                            throw e;
                        } 
                    }
            	}
            }else {
            	FileRaw rawFile;
                rawFile = new FileRaw(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, FileBase.READ);
                if (image.isColorImage()) {
                    rawFile.setNumChannels(3);
                }
                linkProgress(rawFile);
                rawFile.readImage(image, offset);
            }
            
            //close the compressed streams if open
            if(zin != null) {
            	zin.close();
            }
            if(gzin != null) {
            	gzin.close();
            }
            if(bz2in != null) {
            	bz2in.close();
            }

            if (vox_offset < 0.0f) {
                absoluteValue(image);
            }

            if ((scl_slope != 0.0) && ((scl_slope != 1.0f) || (scl_inter != 0.0f)) &&
                    (sourceType != FileInfoNIFTI.NIFTI_TYPE_COMPLEX64) &&
                    (sourceType != FileInfoNIFTI.NIFTI_TYPE_FLOAT64) &&
                    (sourceType != FileInfoNIFTI.NIFTI_TYPE_RGB24)) {
                if ( (image.getType() == ModelStorageBase.COMPLEX) || (image.getType() == ModelStorageBase.DCOMPLEX)) {
                    image.calcMinMaxMag(Preferences.is(Preferences.PREF_LOGMAG_DISPLAY));
                } else {
                    image.calcMinMax();
                }
                imageMin = image.getMin();
                imageMax = image.getMax();
                newType = image.getType();
                m1 = (scl_slope * imageMin) + scl_inter;
                m2 = (scl_slope * imageMax) + scl_inter;
                newMin = Math.min(m1, m2);
                newMax = Math.max(m1, m2);
                needFloat = false;
                doChangeType = false;

                if ((scl_slope != Math.round(scl_slope)) || (scl_inter != Math.round(scl_inter))) {
                    needFloat = true;
                }

                if (needFloat && (newMax <= Float.MAX_VALUE) && (newMin >= -Float.MAX_VALUE) &&
                        ((sourceType == FileInfoNIFTI.DT_BINARY) || (sourceType == FileInfoNIFTI.NIFTI_TYPE_INT8) ||
                             (sourceType == FileInfoNIFTI.NIFTI_TYPE_INT16) ||
                             (sourceType == FileInfoNIFTI.NIFTI_TYPE_INT32) ||
                             (sourceType == FileInfoNIFTI.NIFTI_TYPE_UINT8) ||
                             (sourceType == FileInfoNIFTI.NIFTI_TYPE_UINT16) ||
                             (sourceType == FileInfoNIFTI.NIFTI_TYPE_UINT32))) {
                    newType = ModelStorageBase.FLOAT;
                    doChangeType = true;
                    fileInfo.setBitPix((short) 32);
                } else if (needFloat && (newMax <= Float.MAX_VALUE) && (newMin >= -Float.MAX_VALUE) &&
                               (sourceType == FileInfoNIFTI.NIFTI_TYPE_FLOAT32)) {
                    // do nothing
                } else if (needFloat) {
                    newType = ModelStorageBase.DOUBLE;
                    doChangeType = true;
                    fileInfo.setBitPix((short) 64);
                } else if ((newMax > imageMax) || (newMin < imageMin)) {

                    if ((newMin >= -128) && (newMax <= 127)) {
                        newType = ModelStorageBase.BYTE;
                        doChangeType = true;
                        fileInfo.setBitPix((short) 8);
                    } else if ((newMin >= -32768) && (newMax <= 32767)) {
                        newType = ModelStorageBase.SHORT;
                        doChangeType = true;
                        fileInfo.setBitPix((short) 16);
                    } else if ((newMin >= Integer.MIN_VALUE) && (newMax <= Integer.MAX_VALUE)) {
                        newType = ModelStorageBase.INTEGER;
                        doChangeType = true;
                        fileInfo.setBitPix((short) 32);
                    } else if ((newMin >= Long.MIN_VALUE) && (newMax <= Long.MAX_VALUE)) {
                        newType = ModelStorageBase.LONG;
                        doChangeType = true;
                        fileInfo.setBitPix((short) 64);
                    } else {
                        newType = ModelStorageBase.DOUBLE;
                        doChangeType = true;
                        fileInfo.setBitPix((short) 64);
                    }
                }

                if (doChangeType) {

                    // Don't do scaling in changeTypeAlgo because scl_slope could be negative
                    changeTypeAlgo = new AlgorithmChangeType(image, newType, imageMin, imageMax, imageMin, imageMax,
                                                             false);
                    changeTypeAlgo.run();
                    changeTypeAlgo.finalize();
                    changeTypeAlgo = null;
                } // if (doChangeType)

                scale(image);
            } // if ((scl_slope != 0.0) && ((scl_slope != 1.0f) || (scl_inter != 0.0f)) &&
            
            if (flip) {
                 flipTopBottom(image);
            }
            
            if (one) {
                fileInfo.setExtents(extents);
            }

            fireProgressStateChanged(100);
        } catch (IOException error) {
            throw error;
        } catch (OutOfMemoryError e) {
            throw (e);
        }

        return image;
    }
    
    
    /**
     * 
     * @param in
     * @param buff
     * @param off
     * @param fullBufferSize
     * @return
     * @throws IOException 
     */
    private byte[] getFullBuffer(InputStream in, byte[] buff, int off, int fullBufferSize) throws IOException {
    	
    	int bytesRead = 0;
    	int offset = off;
    	while(offset != fullBufferSize || bytesRead != -1) {
    	
    		
    	 try {
    		 
             bytesRead = in.read(buff,offset,fullBufferSize-offset);

             if(bytesRead == -1) {
            	 break;
             }
             if(bytesRead == 0) {
            	 break;
             }
             offset = offset + bytesRead;

             if(offset == fullBufferSize) {
            	 break;
             }
           
         } catch (IOException e) {
        	 throw e;
         }
         
    	}
    	
         if(offset != fullBufferSize) {
        	 
        	 //this means that we reached the end of file...so lets return the appropriate sized buffer
        	 byte[] buffShortened = new byte[offset];
        	 for(int i=0;i<offset;i++) {
        		 buffShortened[i] = buff[i];
        	 }
        	 return buffShortened;
         }else {
        	 return buff;
         }
    	
    	
    }
    
    /**
     * Reads a NIFTI image file by reading the header then making a FileRaw to read the file. Image data is left in
     * buffer. If the fileInfo cannot be found, the header will be located and read first. Image is not 'flipped', and
     * neither units of measure nor orientation are set.
     *
     * @param      buffer  Image buffer to store image data into.
     *
     * @exception  IOException  if there is an error reading the file
     *
     * @see        FileRaw
     */
    public void readImage(float[] buffer) throws IOException, OutOfMemoryError {
    	readImage(buffer, 0l);
    }

    /**
     * Reads a NIFTI image file by reading the header then making a FileRaw to read the file. Image data is left in
     * buffer. If the fileInfo cannot be found, the header will be located and read first. Image is not 'flipped', and
     * neither units of measure nor orientation are set.
     *
     * @param      buffer  Image buffer to store image data into.
     * @param	   offset  Offset a which to start reaing image, can be used to read in a specific slice/subBrick
     *
     * @exception  IOException  if there is an error reading the file
     *
     * @see        FileRaw
     */
    public void readImage(float[] buffer, long userOffset) throws IOException, OutOfMemoryError {
        int i;
        long offset;

        if (fileInfo == null) { // if no file info yet, make it.
            fileInfo = new FileInfoNIFTI(fileName, fileDir, FileUtility.NIFTI);

            if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory(),false, noReadPrivateTags)) {
                throw (new IOException("Cannot read image because of NIFTI header file error"));
            }
        }

        try { // Construct a FileRaw to actually read the image.

            FileRaw rawFile;
            rawFile = new FileRaw(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, FileBase.READ);
            linkProgress(rawFile);

            if (oneFile) {
                offset = userOffset + (long) Math.abs(vox_offset);

                if (offset < headerSize) { // header length
                    offset = userOffset + headerSize;
                }
            } else {
            	offset = userOffset;
            }

            rawFile.readImage(buffer, offset, fileInfo.getDataType());

            if (vox_offset < 0.0f) {

                for (i = 0; i < buffer.length; i++) {
                    buffer[i] = Math.abs(buffer[i]);
                }
            }

            if ((scl_slope != 0.0) && ((scl_slope != 1.0f) || (scl_inter != 0.0f))) {

                for (i = 0; i < buffer.length; i++) {
                    buffer[i] = (buffer[i] * scl_slope) + scl_inter;
                }
            } // if ((scl_slope != 0.0) && ((scl_slope != 1.0f) || (scl_inter != 0.0f)))
            axisOrientation = fileInfo.getAxisOrientation();
            if ((Preferences.is(Preferences.PREF_FLIP_NIFTI_READ)) &&
               ((axisOrientation[1] == FileInfoBase.ORI_P2A_TYPE) || (axisOrientation[1] == FileInfoBase.ORI_I2S_TYPE))) {
            	int orient = axisOrientation[1];
                if (axisOrientation[1] == FileInfoBase.ORI_P2A_TYPE) {
                    axisOrientation[1] = FileInfoBase.ORI_A2P_TYPE;
                }
                else {
                    axisOrientation[1] = FileInfoBase.ORI_S2I_TYPE;
                }
                fileInfo.setAxisOrientation(axisOrientation);
                LPSOrigin = fileInfo.getOrigin();

                    if ((orient == FileInfoBase.ORI_R2L_TYPE) || 
                            (orient == FileInfoBase.ORI_A2P_TYPE) || 
                            (orient == FileInfoBase.ORI_I2S_TYPE)) {
                        LPSOrigin[1] = LPSOrigin[1] + ((fileInfo.getExtents()[1] - 1) * fileInfo.getResolutions()[1]);
                    } else {
                        LPSOrigin[1] = LPSOrigin[1] - ((fileInfo.getExtents()[1] - 1) * fileInfo.getResolutions()[1]);
                    }
                fileInfo.setOrigin(LPSOrigin);
                
                
                matrix.set(0, 1, -matrix.get(0, 1));
                matrix.set(1, 1, -matrix.get(1, 1));
                matrix.set(2, 1, -matrix.get(2, 1)); 
                matrix.set(0, 3, LPSOrigin[0]);
                matrix.set(1, 3, LPSOrigin[1]);
                matrix.set(2, 3, LPSOrigin[2]);
                              
                if (matrix2 != null) {
                    matrix2.set(0, 1, -matrix2.get(0, 1));
                    matrix2.set(1, 1, -matrix2.get(1, 1));
                    matrix2.set(2, 1, -matrix2.get(2, 1)); 
                    matrix2.set(0, 3, LPSOrigin[0]);
                    matrix2.set(1, 3, LPSOrigin[1]); 
                    matrix2.set(2, 3, LPSOrigin[2]);
                } // if (matrix2 != null)

                flipTopBottom(buffer, fileInfo);        
            } // if ((Preferences.is(Preferences.PREF_FLIP_NIFTI_READ)) &&
            
            rawFile.raFile.close();
            fireProgressStateChanged(100);
        } catch (IOException error) {
        	error.printStackTrace();
            throw new IOException("FileNIFTI: " + error);
            
        } catch (OutOfMemoryError e) {
            throw (e);
        }

        return;
    }

    /**
     * Scales image.
     *
     * @param   image  Image to scale.
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public void scale(ModelImage image) throws IOException {

        try {
            int nBuffers;
            int bufferSize;
            float[] buffer = null;

            if (image.getNDims() > 1) {
                bufferSize = image.getSliceSize();
            } else {
                bufferSize = image.getExtents()[0];
            }

            if (image.getNDims() == 5) {
                nBuffers = image.getExtents()[4] * image.getExtents()[3] * image.getExtents()[2];

            } else if (image.getNDims() == 4) {
                nBuffers = image.getExtents()[3] * image.getExtents()[2];
            } else if (image.getNDims() == 3) {
                nBuffers = image.getExtents()[2];
            } else {
                nBuffers = 1;
            }

            if (image.isColorImage()) {

                buffer = new float[bufferSize * 4];
                bufferSize = bufferSize * 4;

                int i, j, k;
                int xDim = image.getExtents()[0] * 4;
                int yDim = image.getExtents()[1];

                for (k = 0; k < nBuffers; k++) {
                    image.exportData(k * bufferSize, bufferSize, buffer);

                    for (j = 0; j < yDim; j++) {

                        for (i = 0; i < xDim; i += 4) {
                            buffer[(j * xDim) + i] = 255;
                            buffer[(j * xDim) + i + 1] = (scl_slope * buffer[(j * xDim) + i + 1]) + scl_inter;
                            buffer[(j * xDim) + i + 2] = (scl_slope * buffer[(j * xDim) + i + 2]) + scl_inter;
                            buffer[(j * xDim) + i + 3] = (scl_slope * buffer[(j * xDim) + i + 3]) + scl_inter;
                        }
                    }

                    image.importData(k * bufferSize, buffer, false);
                }
            } else {
                buffer = new float[bufferSize];

                int i, j, k;
                int xDim = image.getExtents()[0];
                int yDim = image.getExtents()[1];

                for (k = 0; k < nBuffers; k++) {
                    image.exportData(k * bufferSize, bufferSize, buffer);

                    for (j = 0; j < yDim; j++) {

                        for (i = 0; i < xDim; i++) {
                            buffer[(j * xDim) + i] = (scl_slope * buffer[(j * xDim) + i]) + scl_inter;
                        }
                    }

                    image.importData(k * bufferSize, buffer, false);
                }
            }
        } catch (IOException error) {
            throw new IOException("FileNIFTI.scale: " + error);
        } catch (OutOfMemoryError error) {
            throw (error);
        }
    }

    /**
     * Writes an NIFTI format type image.
     *
     * @param      image  Image model of data to write.
     *
     * @exception  IOException  if there is an error writing the file
     *
     * @see        FileInfoNIFTI
     * @see        FileRaw
     */
    public void writeImage(ModelImage image, FileWriteOptions options) throws IOException {
        String fhName;
        int index;
        int nImagesSaved;
        int nTimePeriodsSaved;
        String suffix;

        suffix = FileUtility.getExtension(fileName);

        if (suffix.equalsIgnoreCase(".nii")) {
            oneFile = true;
        } else if (suffix.equalsIgnoreCase(".img")) {
            oneFile = false;
        } else if (suffix.equalsIgnoreCase(".hdr")) {
            oneFile = false;
        } else {
            JDialogNIFTIChoice choice = new JDialogNIFTIChoice(ViewUserInterface.getReference().getMainFrame());

            if (!choice.okayPressed()) {
                throw new IOException("FileNIFTIWrite dialog error");
            }

            oneFile = choice.getOneFile();
        }

        index = fileName.lastIndexOf(".");

        if (index != -1) {
            fhName = fileName.substring(0, index);
            if (suffix.equalsIgnoreCase(".hdr")) {
                fileName = fhName + ".img";
            }
        } else {
            fhName = fileName.substring(0);
        }

        if (options.isMultiFile()) {
            FileRaw rawFile;
            rawFile = new FileRaw(image.getFileInfo(0));
            rawFile.setZeroLengthFlag(true);
            linkProgress(rawFile);

            if (oneFile) {
                rawFile.setStartPosition(352L);

                if (image.getNDims() == 3) {
                    rawFile.writeImage3DTo2D(image, options, ".nii");
                    writeHeader3DTo2D(image, fhName, fileDir, options, oneFile);
                } else if (image.getNDims() == 4) {
                    rawFile.writeImage4DTo3D(image, options, ".nii");
                    writeHeader4DTo3D(image, fhName, fileDir, options, oneFile);
                }
            } // if (oneFile)
            else { // 2 files
                rawFile.setStartPosition(0L);

                if (image.getNDims() == 3) {
                    rawFile.writeImage3DTo2D(image, options, ".img");
                    writeHeader3DTo2D(image, fhName, fileDir, options, oneFile);
                } else if (image.getNDims() == 4) {
                    rawFile.writeImage4DTo3D(image, options, ".img");
                    writeHeader4DTo3D(image, fhName, fileDir, options, oneFile);
                }
            } // else 2 files

        } else {

            try {
                FileRaw rawFile;
                rawFile = new FileRaw(fileName, fileDir, image.getFileInfo(0), FileBase.READ_WRITE);
                rawFile.setZeroLengthFlag(true);
                linkProgress(rawFile);

                if (oneFile) {
                    rawFile.setStartPosition(352L);
                } else {
                    rawFile.setStartPosition(0L);
                }

                rawFile.writeImage(image, options);
                nImagesSaved = rawFile.getNImages();
                nTimePeriodsSaved = rawFile.getNTimePeriods();

                if (nImagesSaved != 0) {
                    writeHeader(image, nImagesSaved, nTimePeriodsSaved, fhName, fileDir,false, oneFile);
                }

            } catch (IOException error) {
                throw new IOException("FileNIFTIWrite: " + error);
            } catch (OutOfMemoryError error) {
                throw (error);
            }
        }

        fireProgressStateChanged(100);

        // With extents from rawFile
    }


    /**
     * --------------------------------------------------------------------------- ! compute the (closest) orientation
     * from a 4x4 ijk->xyz tranformation matrix
     *
     * <pre>
       Input:  4x4 matrix that transforms (i,j,k) indexes to (x,y,z) coordinates,
               where +x=Left, +y=Posterior, +z=Superior.
               (Only the upper-left 3x3 corner of R is used herein.)
               Note that this routine uses the MIPAV LPS convention as
               opposed to the NIFTI RAS convention.
       Output: 3 orientation codes that correspond to the closest "standard"
               anatomical orientation of the (i,j,k) axes.
       Method: Find which permutation of (x,y,z) has the smallest angle to the
               (i,j,k) axes directions, which are the columns of the R matrix.
       Errors: The codes returned will be zero.

    
       </pre>
     *
     * <p>\see "QUATERNION REPRESENTATION OF ROTATION MATRIX" in nifti1.h \see nifti_quatern_to_mat44,
     * nifti_mat44_to_quatern, nifti_make_orthog_mat44
     * -------------------------------------------------------------------------</p>
     *
     * @param   mat  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private int[] getAxisOrientation(TransMatrix mat) {
        int[] axisOrientation = new int[3];
        //double[][] array;
        double xi, xj, xk, yi, yj, yk, zi, zj, zk, val, detQ, detP;
        Matrix P, Q, M;
        int i, j, k = 0, p, q, r, ibest, jbest, kbest, pbest, qbest, rbest;
        double vbest;

        /* load column vectors for each (i,j,k) direction from matrix */

        /*-- i axis --*/
        /*-- j axis --*/
        /*-- k axis --*/
        //array = mat.getMatrix(0, 2, 0, 2).getArray();
        xi = mat.get(0, 0);
        xj = mat.get(0, 1);
        xk = mat.get(0, 2);
        yi = mat.get(1, 0);
        yj = mat.get(1, 1);
        yk = mat.get(1, 2);
        zi = mat.get(2, 0);
        zj = mat.get(2, 1);
        zk = mat.get(2, 2);

        /* normalize column vectors to get unit vectors along each ijk-axis */

        /* normalize i axis */
        axisOrientation[0] = FileInfoBase.ORI_UNKNOWN_TYPE;
        val = Math.sqrt((xi * xi) + (yi * yi) + (zi * zi));

        if (val == 0.0) {
            return axisOrientation; /* stupid input */
        }

        xi /= val;
        yi /= val;
        zi /= val;

        /* normalize j axis */

        val = Math.sqrt((xj * xj) + (yj * yj) + (zj * zj));

        if (val == 0.0) {
            return axisOrientation; /* stupid input */
        }

        xj /= val;
        yj /= val;
        zj /= val;

        /* orthogonalize j axis to i axis, if needed */

        val = (xi * xj) + (yi * yj) + (zi * zj); /* dot product between i and j */

        if (Math.abs(val) > 1.e-4) {
            xj -= val * xi;
            yj -= val * yi;
            zj -= val * zi;
            val = Math.sqrt((xj * xj) + (yj * yj) + (zj * zj)); /* must renormalize */

            if (val == 0.0) {
                return axisOrientation; /* j was parallel to i? */
            }

            xj /= val;
            yj /= val;
            zj /= val;
        }

        /* normalize k axis; if it is zero, make it the cross product i x j */

        val = Math.sqrt((xk * xk) + (yk * yk) + (zk * zk));

        if (val == 0.0) {
            xk = (yi * zj) - (zi * yj);
            yk = (zi * xj) - (zj * xi);
            zk = (xi * yj) - (yi * xj);
        } else {
            xk /= val;
            yk /= val;
            zk /= val;
        }

        /* orthogonalize k to i */

        val = (xi * xk) + (yi * yk) + (zi * zk); /* dot product between i and k */

        if (Math.abs(val) > 1.e-4) {
            xk -= val * xi;
            yk -= val * yi;
            zk -= val * zi;
            val = Math.sqrt((xk * xk) + (yk * yk) + (zk * zk));

            if (val == 0.0) {
                return axisOrientation; /* bad */
            }

            xk /= val;
            yk /= val;
            zk /= val;
        }

        /* orthogonalize k to j */

        val = (xj * xk) + (yj * yk) + (zj * zk); /* dot product between j and k */

        if (Math.abs(val) > 1.e-4) {
            xk -= val * xj;
            yk -= val * yj;
            zk -= val * zj;
            val = Math.sqrt((xk * xk) + (yk * yk) + (zk * zk));

            if (val == 0.0) {
                return axisOrientation; /* bad */
            }

            xk /= val;
            yk /= val;
            zk /= val;
        }

        Q = new Matrix(3, 3);

        Q.set(0, 0, xi);
        Q.set(0, 1, xj);
        Q.set(0, 2, xk);
        Q.set(1, 0, yi);
        Q.set(1, 1, yj);
        Q.set(1, 2, yk);
        Q.set(2, 0, zi);
        Q.set(2, 1, zj);
        Q.set(2, 2, zk);

        /* at this point, Q is the rotation matrix from the (i,j,k) to (x,y,z) axes */

        detQ = Q.det();

        if (detQ == 0.0) {
            MipavUtil.displayError("detQ == 0.0 in getAxisOrientation");

            return axisOrientation;
        }

        P = new Matrix(3, 3);
        /* Build and test all possible +1/-1 coordinate permutation matrices P;
         * then find the P such that the rotation matrix M=PQ is closest to theidentity, in the sense of M having the
         * smallest total rotation angle. */

        /* Despite the formidable looking 6 nested loops, there are
         *only 3*3*3*2*2*2 = 216 passes, which will run very quickly. */

        vbest = -666.0;
        ibest = pbest = qbest = rbest = 1;
        jbest = 2;
        kbest = 3;

        for (i = 1; i <= 3; i++) { /* i = column number to use for row #1 */

            for (j = 1; j <= 3; j++) { /* j = column number to use for row #2 */

                if (i == j) {
                    continue;
                }

                for (k = 1; k <= 3; k++) { /* k = column number to use for row #3 */

                    if ((i == k) || (j == k)) {
                        continue;
                    }

                    P.set(0, 0, 0.0);
                    P.set(0, 1, 0.0);
                    P.set(0, 2, 0.0);
                    P.set(1, 0, 0.0);
                    P.set(1, 1, 0.0);
                    P.set(1, 2, 0.0);
                    P.set(2, 0, 0.0);
                    P.set(2, 1, 0.0);
                    P.set(2, 2, 0.0);

                    for (p = -1; p <= 1; p += 2) { /* p,q,r are -1 or +1      */

                        for (q = -1; q <= 1; q += 2) { /* and go into rows #1,2,3 */

                            for (r = -1; r <= 1; r += 2) {
                                P.set(0, i - 1, p);
                                P.set(1, j - 1, q);
                                P.set(2, k - 1, r);
                                detP = P.det(); /* sign of permutation */

                                if ((detP * detQ) <= 0.0) {
                                    continue; /* doesn't match sign of Q */
                                }

                                M = P.times(Q);

                                /* angle of M rotation = 2.0*acos(0.5*sqrt(1.0+trace(M)))       */
                                /* we want largest trace(M) == smallest angle == M nearest to I */

                                val = M.get(0, 0) + M.get(1, 1) + M.get(2, 2); /* trace */

                                if (val > vbest) {
                                    vbest = val;
                                    ibest = i;
                                    jbest = j;
                                    kbest = k;
                                    pbest = p;
                                    qbest = q;
                                    rbest = r;
                                }
                            }
                        }
                    }
                }
            }
        }

        /* At this point ibest is 1 or 2 or 3; pbest is -1 or +1; etc.
         *
         * The matrix P that corresponds is the best permutation approximation to Q-inverse; that is, P (approximately)
         * takes (x,y,z) coordinates to the (i,j,k) axes.
         *
         * For example, the first row of P (which contains pbest in column ibest) determines the way the i axis points
         * relative to the anatomical (x,y,z) axes.  If ibest is 2, then the i axis is along the y axis, which is
         * direction P2A (if pbest > 0) or A2P (if pbest < 0).
         *
         * So, using ibest and pbest, we can assign the output code forthe i axis.  Mutatis mutandis for the j and k axes,
         * of course. */

        switch (ibest * pbest) {

            case 1:
                i = FileInfoBase.ORI_R2L_TYPE;
                break;

            case -1:
                i = FileInfoBase.ORI_L2R_TYPE;
                break;

            case 2:
                i = FileInfoBase.ORI_A2P_TYPE;
                break;

            case -2:
                i = FileInfoBase.ORI_P2A_TYPE;
                break;

            case 3:
                i = FileInfoBase.ORI_I2S_TYPE;
                break;

            case -3:
                i = FileInfoBase.ORI_S2I_TYPE;
                break;
        }

        switch (jbest * qbest) {

            case 1:
                j = FileInfoBase.ORI_R2L_TYPE;
                break;

            case -1:
                j = FileInfoBase.ORI_L2R_TYPE;
                break;

            case 2:
                j = FileInfoBase.ORI_A2P_TYPE;
                break;

            case -2:
                j = FileInfoBase.ORI_P2A_TYPE;
                break;

            case 3:
                j = FileInfoBase.ORI_I2S_TYPE;
                break;

            case -3:
                j = FileInfoBase.ORI_S2I_TYPE;
                break;

            default:
                j = 1;
        }

        switch (kbest * rbest) {

            case 1:
                k = FileInfoBase.ORI_R2L_TYPE;
                break;

            case -1:
                k = FileInfoBase.ORI_L2R_TYPE;
                break;

            case 2:
                k = FileInfoBase.ORI_A2P_TYPE;
                break;

            case -2:
                k = FileInfoBase.ORI_P2A_TYPE;
                break;

            case 3:
                k = FileInfoBase.ORI_I2S_TYPE;
                break;

            case -3:
                k = FileInfoBase.ORI_S2I_TYPE;
                break;
        }

        axisOrientation[0] = i;
        axisOrientation[1] = j;
        axisOrientation[2] = k;

        return axisOrientation;
    }

    /**
     * Helper method to calculate the offset for getting only the middle NIFTI image slice from the 3D file.
     *
     * @param   fileInfo  File info.
     *
     * @return  offset
     */
    private int getOffset(FileInfoNIFTI fileInfo) {
        int offset = fileInfo.getExtents()[0] * fileInfo.getExtents()[1] * (fileInfo.getExtents()[2] / 2);

        switch (fileInfo.getSourceType()) {

            case FileInfoNIFTI.DT_BINARY:
            case FileInfoNIFTI.NIFTI_TYPE_UINT8:
                break;

            case FileInfoNIFTI.NIFTI_TYPE_INT16:
            case FileInfoNIFTI.NIFTI_TYPE_UINT16:
                offset *= 2;
                break;

            case FileInfoNIFTI.NIFTI_TYPE_FLOAT32:
            case FileInfoNIFTI.NIFTI_TYPE_INT32:
                offset *= 4;
                break;

            case FileInfoNIFTI.NIFTI_TYPE_INT64:
            case FileInfoNIFTI.NIFTI_TYPE_FLOAT64:
            case FileInfoNIFTI.NIFTI_TYPE_COMPLEX64:
                offset *= 8;
                break;

            case FileInfoNIFTI.NIFTI_TYPE_COMPLEX128:
                offset *= 16;
                break;

            case FileInfoNIFTI.NIFTI_TYPE_RGB24:
                offset *= 3;
                break;
        }

        return offset;
    }

    /**
     * max column norm of 3x3 matrix.
     *
     * @param   A  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double mat33_colnorm(Matrix A) {
        double r1, r2, r3;
        r1 = Math.abs(A.get(0, 0)) + Math.abs(A.get(1, 0)) + Math.abs(A.get(2, 0));
        r2 = Math.abs(A.get(0, 1)) + Math.abs(A.get(1, 1)) + Math.abs(A.get(2, 1));
        r3 = Math.abs(A.get(0, 2)) + Math.abs(A.get(1, 2)) + Math.abs(A.get(2, 2));

        if (r1 < r2) {
            r1 = r2;
        }

        if (r1 < r3) {
            r1 = r3;
        }

        return r1;
    }

    /**
     * Polar decomposition of a 3x3 matrix: finds the closest orthogonal matrix to input A (in both Frobenius and L2
     * norms). Algorithm is that from NJ Higham, SIAM JSci Stat Comput, 7:1160-1174.
     *
     * @param   A  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private Matrix mat33_polar(Matrix A) {
        Matrix X, Y, Z;
        double alp, bet, gam, gmi;
        double dif = 1.0;
        int k = 0;
        double val;

        X = A.copy();
        Z = new Matrix(3, 3);

        // force matrix to be nonsingular
        gam = X.det();

        while (gam == 0.0) { // perturb matrix
            gam = 0.00001 * (0.001 + mat33_rownorm(X));
            val = X.get(0, 0);
            X.set(0, 0, val + gam);
            val = X.get(1, 1);
            X.set(1, 1, val + gam);
            val = X.get(2, 2);
            X.set(2, 2, val + gam);
            gam = X.det();
        }

        while (true) {
            Y = X.inverse();

            if (dif > 0.3) { // far from convergence
                alp = Math.sqrt(mat33_rownorm(X) * mat33_colnorm(X));
                bet = Math.sqrt(mat33_rownorm(Y) * mat33_colnorm(Y));
                gam = Math.sqrt(bet / alp);
                gmi = 1.0 / gam;
            } else {
                gam = gmi = 1.0; // close to convergence
            }

            Z.set(0, 0, 0.5 * ((gam * X.get(0, 0)) + (gmi * Y.get(0, 0))));
            Z.set(0, 1, 0.5 * ((gam * X.get(0, 1)) + (gmi * Y.get(1, 0))));
            Z.set(0, 2, 0.5 * ((gam * X.get(0, 2)) + (gmi * Y.get(2, 0))));
            Z.set(1, 0, 0.5 * ((gam * X.get(1, 0)) + (gmi * Y.get(0, 1))));
            Z.set(1, 1, 0.5 * ((gam * X.get(1, 1)) + (gmi * Y.get(1, 1))));
            Z.set(1, 2, 0.5 * ((gam * X.get(1, 2)) + (gmi * Y.get(2, 1))));
            Z.set(2, 0, 0.5 * ((gam * X.get(2, 0)) + (gmi * Y.get(0, 2))));
            Z.set(2, 1, 0.5 * ((gam * X.get(2, 1)) + (gmi * Y.get(1, 2))));
            Z.set(2, 2, 0.5 * ((gam * X.get(2, 2)) + (gmi * Y.get(2, 2))));

            dif = Math.abs(Z.get(0, 0) - X.get(0, 0)) + Math.abs(Z.get(0, 1) - X.get(0, 1)) +
                  Math.abs(Z.get(0, 2) - X.get(0, 2)) + Math.abs(Z.get(1, 0) - X.get(1, 0)) +
                  Math.abs(Z.get(1, 1) - X.get(1, 1)) + Math.abs(Z.get(1, 2) - X.get(1, 2)) +
                  Math.abs(Z.get(2, 0) - X.get(2, 0)) + Math.abs(Z.get(2, 1) - X.get(2, 1)) +
                  Math.abs(Z.get(2, 2) - X.get(2, 2));

            k = k + 1;

            if ((k > 100) || (dif < 3.0e-6)) {
                break; // convergence or exhaustion
            }

            X = Z.copy();
        }

        return Z;
    }

    /**
     * max row norm of 3x3 matrix.
     *
     * @param   A  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private double mat33_rownorm(Matrix A) {
        double r1, r2, r3;
        r1 = Math.abs(A.get(0, 0)) + Math.abs(A.get(0, 1)) + Math.abs(A.get(0, 2));
        r2 = Math.abs(A.get(1, 0)) + Math.abs(A.get(1, 1)) + Math.abs(A.get(1, 2));
        r3 = Math.abs(A.get(2, 0)) + Math.abs(A.get(2, 1)) + Math.abs(A.get(2, 2));

        if (r1 < r2) {
            r1 = r2;
        }

        if (r1 < r3) {
            r1 = r3;
        }

        return r1;
    }

    /**
     * Updates the start locations. Each image has a fileinfo where the start locations are stored. Note that the start
     * location for the Z (3rd) dimension change with the change is the slice. The origin is in the upper left corner
     * and we are using the right hand rule. + x -> left to right; + y -> top to bottom and + z -> into screen.
     *
     * @param  fileInfo  DOCUMENT ME!
     */
    private void updateorigins(FileInfoBase[] fileInfo) {

        float[] origin = (float[]) (fileInfo[0].getOrigin().clone());
        float[] resolutions = fileInfo[0].getResolutions();

        if (image.getNDims() == 3) {

            for (int i = 0; i < image.getExtents()[2]; i++) {
                fileInfo[i].setOrigin(origin[0] + (matrix.get(0, 2) * i), 0);
                fileInfo[i].setOrigin(origin[1] + (matrix.get(1, 2) * i), 1);
                fileInfo[i].setOrigin(origin[2] + (matrix.get(2, 2) * i), 2);
            }
        } else if (image.getNDims() == 4) {

            for (int i = 0; i < image.getExtents()[3]; i++) {

                for (int j = 0; j < image.getExtents()[2]; j++) {
                    fileInfo[i * image.getExtents()[2] + j].setOrigin(origin[0] + (matrix.get(0, 2) * j), 0);
                    fileInfo[i * image.getExtents()[2] + j].setOrigin(origin[1] + (matrix.get(1, 2) * j), 1);
                    fileInfo[i * image.getExtents()[2] + j].setOrigin(origin[2] + (matrix.get(2, 2) * j), 2);
                    fileInfo[(i * image.getExtents()[2]) + j].setOrigin(origin[3] + i * resolutions[3], 3);
                    
                }
            }
        }
        /*else if (image.getNDims() == 5) {
         * fileInfo = image.getFileInfo(); for (int i = 0;    i <    image.getExtents()[2] * image.getExtents()[3] *
         * image.getExtents()[4];    i++) { fileInfo[i].setorigins(startLocs); startLocs[4] += resolutions[4]; }  }*/
    }

    

    /**
     * Writes a NIFTI header to a separate file.
     *
     * @param      image     Image model of data to write.
     * @param      fileName  File name.
     * @param      fileDir   File directory.
     *
     * @return     Flag to confirm a successful read.
     *
     * @exception  IOException  if there is an error
     *
     * @see        FileInfoNIFTI
     */
    public boolean writeHeader(ModelImage image, int nImagesSaved, int nTimeSaved, String fileName, String fileDir,
    		                   boolean doGzip, boolean oneFile)
            throws IOException {

        int i;
        boolean isNIFTI = true;
        boolean endianess;
        String fileHeaderName;
        int nDims;
        int[] extents;
        FileInfoBase myFileInfo;
        int[] niftiExtents;
        double r00, r01, r02, r10, r11, r12, r20, r21, r22;
        int imageMin, imageMax;
        byte dim_info;
        float[] resols;
        int[] unitsOfMeasure;
        boolean found;
        int firstSpatialDim;
        int firstTimeDim;
        int firstSpatialUnits = Unit.UNKNOWN_MEASURE.getLegacyNum();
        int firstTimeUnits = Unit.UNKNOWN_MEASURE.getLegacyNum();
        int niftiSpatialUnits = FileInfoNIFTI.NIFTI_UNITS_UNKNOWN;
        int niftiTimeUnits = FileInfoNIFTI.NIFTI_UNITS_UNKNOWN;
        int nDimsLength1;
        float[] niftiResols;
        double xd, yd, zd;
        Matrix Q;
        Matrix P;
        double a, b, c, d;
        float[] niftiOrigin = new float[3];
        float[] niftiOriginS = null;
        MatrixHolder matHolder = null;
        TransMatrix[] matrixArray = null;
        TransMatrix matrixQ = null;
        TransMatrix matrixS = null;
        int transformIDQ = TransMatrix.TRANSFORM_UNKNOWN;
        int transformIDS = TransMatrix.TRANSFORM_UNKNOWN;
        int qform_code = 0;
        int sform_code = 0;
        int j;
        FileInfoDicom fileInfoDicom;
        double xDel = 0.0;
        double yDel = 0.0;
        double zDel = 0.0;
        TransMatrix dicomMatrix = null;
        TransMatrix transposeMatrix;

        myFileInfo = image.getFileInfo(0); // A safeguard in case the file is not NIFTI
        endianess = myFileInfo.getEndianess();

        try { // In this case, the file must be NIFTI
            fileInfo = (FileInfoNIFTI) image.getFileInfo(0);
        } catch (ClassCastException e) { // If it isn't, catch the exception

            // and make a new fileInfo
            fileInfo = new FileInfoNIFTI(fileName, fileDir, FileUtility.NIFTI);
            isNIFTI = false; // Write the header without all the NIFTI info
            
            try { // see if it is DICOM
            	fileInfoDicom = (FileInfoDicom)image.getFileInfo(0);
            	dicomMatrix = fileInfoDicom.getPatientOrientation();
            	String orientation = (String) fileInfoDicom.getTagTable().getValue("0020,0032");

                if (orientation != null) {
                	
	                int index1 = -1, index2 = -1;
	
	                for (i = 0; i < orientation.length(); i++) {
	
	                    if (orientation.charAt(i) == '\\') {
	
	                        if (index1 == -1) {
	                            index1 = i;
	                        } else {
	                            index2 = i;
	                        }
	                    }
	                }
	                double coord[] = new double[3];
	                coord[0] = Double.valueOf(orientation.substring(0, index1)).doubleValue();
	                coord[1] = Double.valueOf(orientation.substring(index1 + 1, index2)).doubleValue();
	                coord[2] = Double.valueOf(orientation.substring(index2 + 1)).doubleValue();
	                
	                FileInfoDicom fileInfoDicom1 = (FileInfoDicom)image.getFileInfo(1);
	                String orientation1 = (String) fileInfoDicom1.getTagTable().getValue("0020,0032");

	                if (orientation1 != null) {
	                	
		                index1 = -1;
		                index2 = -1;
		
		                for (i = 0; i < orientation1.length(); i++) {
		
		                    if (orientation1.charAt(i) == '\\') {
		
		                        if (index1 == -1) {
		                            index1 = i;
		                        } else {
		                            index2 = i;
		                        }
		                    }
		                }
		                double coord1[] = new double[3];
		                coord1[0] = Double.valueOf(orientation1.substring(0, index1)).doubleValue();
		                coord1[1] = Double.valueOf(orientation1.substring(index1 + 1, index2)).doubleValue();
		                coord1[2] = Double.valueOf(orientation1.substring(index2 + 1)).doubleValue();
		                xDel = coord1[0] - coord[0];
		                yDel = coord1[1] - coord[1];
		                zDel = coord1[2] - coord[2];
	                } // if (orientation1 != null)
                } // if (orientation != null)
            }
            catch (ClassCastException ed) {
            	
            }
        }

        if (oneFile) {
            fileHeaderName = fileName + ".nii";
        } else {
            fileHeaderName = fileName + ".hdr";
        }

        fileHeader = new File(fileDir + fileHeaderName);
        if(!doGzip) {
        	raFile = new RandomAccessFile(fileHeader, "rw");
        }

        // Don't do raFile.setLength(0) if only one file is present because the data is written to this file
        // before the header
        if (!oneFile) {
        	if(!doGzip) {
        		raFile.setLength(0);
        	}
        }
        // 4 extension bytes after 348 header bytes
        bufferByte = new byte[headerSize + 4];

        // Set certain neccessary information
        fileInfo.setSizeOfHeader(headerSize);

        extents = myFileInfo.getExtents();
        unitsOfMeasure = myFileInfo.getUnitsOfMeasure();
        resols = myFileInfo.getResolutions();
        origin = myFileInfo.getOrigin();
        axisOrientation = myFileInfo.getAxisOrientation();

        found = false;
        firstSpatialDim = -1;

        for (i = 0; (i < unitsOfMeasure.length) && (!found); i++) {

            if ((unitsOfMeasure[i] == Unit.UNKNOWN_MEASURE.getLegacyNum()) || (unitsOfMeasure[i] == Unit.INCHES.getLegacyNum()) ||
                    (unitsOfMeasure[i] == Unit.MILS.getLegacyNum()) ||
                    (unitsOfMeasure[i] == Unit.CENTIMETERS.getLegacyNum()) || (unitsOfMeasure[i] == Unit.ANGSTROMS.getLegacyNum()) ||
                    (unitsOfMeasure[i] == Unit.NANOMETERS.getLegacyNum()) || (unitsOfMeasure[i] == Unit.MICROMETERS.getLegacyNum()) ||
                    (unitsOfMeasure[i] == Unit.MILLIMETERS.getLegacyNum()) || (unitsOfMeasure[i] == Unit.METERS.getLegacyNum()) ||
                    (unitsOfMeasure[i] == Unit.KILOMETERS.getLegacyNum()) || (unitsOfMeasure[i] == Unit.MILES.getLegacyNum())) {
                found = true;
                firstSpatialDim = i;
                firstSpatialUnits = unitsOfMeasure[i];
            }
        }

        found = false;
        firstTimeDim = -1;

        for (i = 0; (i < unitsOfMeasure.length) && (!found); i++) {

            if ((unitsOfMeasure[i] == Unit.NANOSEC.getLegacyNum()) || (unitsOfMeasure[i] == Unit.MICROSEC.getLegacyNum()) ||
                    (unitsOfMeasure[i] == Unit.MILLISEC.getLegacyNum()) || (unitsOfMeasure[i] == Unit.SECONDS.getLegacyNum()) ||
                    (unitsOfMeasure[i] == Unit.MINUTES.getLegacyNum()) || (unitsOfMeasure[i] == Unit.HOURS.getLegacyNum()) ||
                    (unitsOfMeasure[i] == Unit.HZ.getLegacyNum()) || (unitsOfMeasure[i] == Unit.PPM.getLegacyNum()) ||
                    (unitsOfMeasure[i] == Unit.RADS.getLegacyNum())) {
                found = true;
                firstTimeDim = i;
                firstTimeUnits = unitsOfMeasure[i];
            }
        }

        if (firstSpatialDim >= 0) {

            switch (Unit.getUnitFromLegacyNum(firstSpatialUnits)) {

                case UNKNOWN_MEASURE:
                    niftiSpatialUnits = FileInfoNIFTI.NIFTI_UNITS_UNKNOWN;
                    break;

                case INCHES:
                case MILS:
                case CENTIMETERS:
                case MILLIMETERS:

                    // convert all spatial units to millimeters
                    niftiSpatialUnits = FileInfoNIFTI.NIFTI_UNITS_MM;
                    for (i = 0; i < extents.length; i++) {

                        if (unitsOfMeasure[i] == Unit.INCHES.getLegacyNum()) {
                            resols[i] = 25.4f * resols[i];
                            origin[i] = 25.4f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.MILS.getLegacyNum()) {
                            resols[i] = 2.54e-2f * resols[i];
                            origin[i] = 2.54e-2f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.CENTIMETERS.getLegacyNum()) {
                            resols[i] = 10.0f * resols[i];
                            origin[i] = 10.0f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.ANGSTROMS.getLegacyNum()) {
                            resols[i] = 1.0e-7f * resols[i];
                            origin[i] = 1.0e-7f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.NANOMETERS.getLegacyNum()) {
                            resols[i] = 1.0e-6f * resols[i];
                            origin[i] = 1.0e-6f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.MICROMETERS.getLegacyNum()) {
                            resols[i] = 1.0e-3f * resols[i];
                            origin[i] = 1.0e-3f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.METERS.getLegacyNum()) {
                            resols[i] = 1.0e3f * resols[i];
                            origin[i] = 1.0e3f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.KILOMETERS.getLegacyNum()) {
                            resols[i] = 1.0e6f * resols[i];
                            origin[i] = 1.0e6f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.MILES.getLegacyNum()) {
                            resols[i] = 1.6093e6f * resols[i];
                            origin[i] = 1.6093e6f * origin[i];
                        }
                    }

                    break;

                case ANGSTROMS:
                case NANOMETERS:
                case MICROMETERS:

                    // convert all spatial units to micrometers
                    niftiSpatialUnits = FileInfoNIFTI.NIFTI_UNITS_MICRON;
                    for (i = 0; i < extents.length; i++) {

                        if (unitsOfMeasure[i] == Unit.INCHES.getLegacyNum()) {
                            resols[i] = 2.54e5f * resols[i];
                            origin[i] = 2.54e5f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.MILS.getLegacyNum()) {
                            resols[i] = 2.54e2f * resols[i];
                            origin[i] = 2.54e2f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.CENTIMETERS.getLegacyNum()) {
                            resols[i] = 1.0e5f * resols[i];
                            origin[i] = 1.0e5f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.ANGSTROMS.getLegacyNum()) {
                            resols[i] = 1.0e-4f * resols[i];
                            origin[i] = 1.0e-4f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.NANOMETERS.getLegacyNum()) {
                            resols[i] = 1.0e-3f * resols[i];
                            origin[i] = 1.0e-3f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.MILLIMETERS.getLegacyNum()) {
                            resols[i] = 1.0e3f * resols[i];
                            origin[i] = 1.0e3f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.METERS.getLegacyNum()) {
                            resols[i] = 1.0e6f * resols[i];
                            origin[i] = 1.0e6f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.KILOMETERS.getLegacyNum()) {
                            resols[i] = 1.0e9f * resols[i];
                            origin[i] = 1.0e9f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.MILES.getLegacyNum()) {
                            resols[i] = 1.6093e9f * resols[i];
                            origin[i] = 1.6093e9f * origin[i];
                        }
                    }

                    break;

                case METERS:
                case KILOMETERS:
                case MILES:

                    // convert all spatial units to meters
                    niftiSpatialUnits = FileInfoNIFTI.NIFTI_UNITS_METER;
                    for (i = 0; i < extents.length; i++) {

                        if (unitsOfMeasure[i] == Unit.INCHES.getLegacyNum()) {
                            resols[i] = 2.54e-2f * resols[i];
                            origin[i] = 2.54e-2f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.MILS.getLegacyNum()) {
                            resols[i] = 2.54e-5f * resols[i];
                            origin[i] = 2.54e-5f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.CENTIMETERS.getLegacyNum()) {
                            resols[i] = 1.0e-2f * resols[i];
                            origin[i] = 1.0e-2f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.ANGSTROMS.getLegacyNum()) {
                            resols[i] = 1.0e-10f * resols[i];
                            origin[i] = 1.0e-10f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.NANOMETERS.getLegacyNum()) {
                            resols[i] = 1.0e-9f * resols[i];
                            origin[i] = 1.0e-9f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.MICROMETERS.getLegacyNum()) {
                            resols[i] = 1.0e-6f * resols[i];
                            origin[i] = 1.0e-6f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.MILLIMETERS.getLegacyNum()) {
                            resols[i] = 1.0e-3f * resols[i];
                            origin[i] = 1.0e-3f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.KILOMETERS.getLegacyNum()) {
                            resols[i] = 1.0e3f * resols[i];
                            origin[i] = 1.0e3f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.MILES.getLegacyNum()) {
                            resols[i] = 1.6093e3f * resols[i];
                            origin[i] = 1.6093e3f * origin[i];
                        }
                    }

                    break;
            }
        } // if (firstSpatialDim >= 0)

        if (firstTimeDim >= 0) {
            switch (Unit.getUnitFromLegacyNum(firstTimeUnits)) {

                case NANOSEC:
                case MICROSEC:

                    // convert all spatial units to microseconds
                    niftiTimeUnits = FileInfoNIFTI.NIFTI_UNITS_USEC;
                    for (i = 0; i < extents.length; i++) {

                        if (unitsOfMeasure[i] == Unit.NANOSEC.getLegacyNum()) {
                            resols[i] = 1.0e-3f * resols[i];
                            origin[i] = 1.0e-3f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.MILLISEC.getLegacyNum()) {
                            resols[i] = 1.0e3f * resols[i];
                            origin[i] = 1.0e3f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.SECONDS.getLegacyNum()) {
                            resols[i] = 1.0e6f * resols[i];
                            origin[i] = 1.0e6f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.MINUTES.getLegacyNum()) {
                            resols[i] = 6.0e7f * resols[i];
                            origin[i] = 6.0e7f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.HOURS.getLegacyNum()) {
                            resols[i] = 3.6e9f * resols[i];
                            origin[i] = 3.6e9f * origin[i];
                        }
                    }

                    break;

                case MILLISEC:

                    // convert all spatial units to milliseconds
                    niftiTimeUnits = FileInfoNIFTI.NIFTI_UNITS_MSEC;
                    for (i = 0; i < extents.length; i++) {

                        if (unitsOfMeasure[i] == Unit.NANOSEC.getLegacyNum()) {
                            resols[i] = 1.0e-6f * resols[i];
                            origin[i] = 1.0e-6f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.MICROSEC.getLegacyNum()) {
                            resols[i] = 1.0e-3f * resols[i];
                            origin[i] = 1.0e-3f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.SECONDS.getLegacyNum()) {
                            resols[i] = 1.0e3f * resols[i];
                            origin[i] = 1.0e3f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.MINUTES.getLegacyNum()) {
                            resols[i] = 6.0e4f * resols[i];
                            origin[i] = 6.0e4f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.HOURS.getLegacyNum()) {
                            resols[i] = 3.6e6f * resols[i];
                            origin[i] = 3.6e6f * origin[i];
                        }
                    }

                    break;

                case SECONDS:
                case MINUTES:
                case HOURS:

                    // convert all time units to seconds
                    niftiTimeUnits = FileInfoNIFTI.NIFTI_UNITS_SEC;
                    for (i = 0; i < extents.length; i++) {

                        if (unitsOfMeasure[i] == Unit.NANOSEC.getLegacyNum()) {
                            resols[i] = 1.0e-9f * resols[i];
                            origin[i] = 1.0e-9f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.MICROSEC.getLegacyNum()) {
                            resols[i] = 1.0e-6f * resols[i];
                            origin[i] = 1.0e-6f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.MILLISEC.getLegacyNum()) {
                            resols[i] = 1.0e-3f * resols[i];
                            origin[i] = 1.0e-3f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.MINUTES.getLegacyNum()) {
                            resols[i] = 6.0e1f * resols[i];
                            origin[i] = 6.0e1f * origin[i];
                        } else if (unitsOfMeasure[i] == Unit.HOURS.getLegacyNum()) {
                            resols[i] = 3.6e3f * resols[i];
                            origin[i] = 3.6e3f * origin[i];
                        }
                    }

                    break;

                case HZ:
                    niftiTimeUnits = FileInfoNIFTI.NIFTI_UNITS_HZ;
                    break;

                case PPM:
                    niftiTimeUnits = FileInfoNIFTI.NIFTI_UNITS_PPM;
                    break;

                case RADS:
                    niftiTimeUnits = FileInfoNIFTI.NIFTI_UNITS_RADS;
                    break;
            }
        } // if (firstTimeDim >= 0)

        if (firstTimeDim < 0) {
            nDims = extents.length;
            nDimsLength1 = 0;
        } else {
            nDims = extents.length + 3 - firstTimeDim;
            nDimsLength1 = 3 - firstTimeDim;
        }

        Preferences.debug("FileNIFTI:writeHeader - nImagesSaved = " + nImagesSaved + "\n", Preferences.DEBUG_FILEIO);
        Preferences.debug("FileNIFTI:writeHeader - nDims = " + nDims + "\n", Preferences.DEBUG_FILEIO);

        niftiExtents = new int[nDims + 1];
        niftiExtents[0] = nDims;

        for (i = 1; i <= nDimsLength1; i++) {
            niftiExtents[i] = 1;
        }

        for (i = 1; i <= (nDims - nDimsLength1); i++) {

            if (i == 3) {
                niftiExtents[i] = nImagesSaved;
            } else if (i == 4) {
                niftiExtents[i] = nTimeSaved;
            } else {
                niftiExtents[i] = extents[i - 1];
            }
        }

        for (i = 0; i < niftiExtents.length; i++) {
            Preferences.debug("FileNIFTI:writeHeader - i = " + i + " dim = " + niftiExtents[i] + "\n",
            		Preferences.DEBUG_FILEIO);
        }

        switch (image.getType()) {

            case ModelStorageBase.BOOLEAN:
                sourceType = FileInfoNIFTI.DT_BINARY;
                break;

            case ModelStorageBase.BYTE:
                sourceType = FileInfoNIFTI.NIFTI_TYPE_INT8;
                break;

            case ModelStorageBase.UBYTE:
                sourceType = FileInfoNIFTI.NIFTI_TYPE_UINT8;
                break;

            case ModelStorageBase.SHORT:
                sourceType = FileInfoNIFTI.NIFTI_TYPE_INT16;
                break;

            case ModelStorageBase.USHORT:
                sourceType = FileInfoNIFTI.NIFTI_TYPE_UINT16;
                break;

            case ModelStorageBase.INTEGER:
                sourceType = FileInfoNIFTI.NIFTI_TYPE_INT32;
                break;

            case ModelStorageBase.UINTEGER:
                sourceType = FileInfoNIFTI.NIFTI_TYPE_UINT32;
                break;

            case ModelStorageBase.LONG:
                sourceType = FileInfoNIFTI.NIFTI_TYPE_INT64;
                break;

            case ModelStorageBase.FLOAT:
                sourceType = FileInfoNIFTI.NIFTI_TYPE_FLOAT32;
                break;

            case ModelStorageBase.DOUBLE:
                sourceType = FileInfoNIFTI.NIFTI_TYPE_FLOAT64;
                break;

            case ModelStorageBase.ARGB: // only RGB for NIFTI images
                sourceType = FileInfoNIFTI.NIFTI_TYPE_RGB24;
                break;

            case ModelStorageBase.COMPLEX:
                sourceType = FileInfoNIFTI.NIFTI_TYPE_COMPLEX64;
                break;

            case ModelStorageBase.DCOMPLEX:
                sourceType = FileInfoNIFTI.NIFTI_TYPE_COMPLEX128;
                break;

            default:
                return false;
        }

        fileInfo.setSourceType(sourceType);

        matHolder = image.getMatrixHolder();

        if (matHolder != null) {
            matrixArray = matHolder.getNIFTICompositeMatrices();

            if (matrixArray != null) {

                if (matrixArray.length >= 1) {

                    if (matrixArray[0] != null) {

                        if (matrixArray[0].isQform()) {
                            matrixQ = matrixArray[0];
                            transformIDQ = matrixArray[0].getTransformID();
                        } else {
                            matrixS = matrixArray[0];
                            transformIDS = matrixArray[0].getTransformID();
                        }
                    } // if (matrixArray[0] != null)
                } // if (matrixArray.length >= 1)

                if (matrixArray.length >= 2) {

                    if (matrixArray[1] != null) {

                        if (matrixArray[1].isQform()) {
                            matrixQ = matrixArray[1];
                            transformIDQ = matrixArray[1].getTransformID();
                        } else {
                            matrixS = matrixArray[1];
                            transformIDS = matrixArray[1].getTransformID();
                        }
                    } // if (matrixArray[1] != null)
                } // if (matrixArray.length >= 2)
            } // if (matrixArray != null)
        } // if (matHolder != null)

        if ((matrixQ == null) && (matrixS == null) && isNIFTI) {
            matrixQ = image.getMatrix();
            transformIDQ = matrixQ.getTransformID();
        }

        if ((matrixQ != null) || ((matrixQ == null) && (matrixS == null))) {

            if (matrixQ != null) {
                Preferences.debug("matrixQ on write entry = " + matrixQ + "\n", Preferences.DEBUG_FILEIO);

                // To use matrixQ information must have qform_code > 0,
                // so cannot have qform_code equal to FileInfoNIFTI.NIFTI_XFORM_UNKNOWN.
                switch (transformIDQ) {

                    case TransMatrix.TRANSFORM_NIFTI_SCANNER_ANATOMICAL:
                        qform_code = FileInfoNIFTI.NIFTI_XFORM_SCANNER_ANAT;
                        break;

                    case TransMatrix.TRANSFORM_ANOTHER_DATASET:
                        qform_code = FileInfoNIFTI.NIFTI_XFORM_ALIGNED_ANAT;
                        break;

                    case TransMatrix.TRANSFORM_TALAIRACH_TOURNOUX:
                        qform_code = FileInfoNIFTI.NIFTI_XFORM_TALAIRACH;
                        break;

                    case TransMatrix.TRANSFORM_MNI_152:
                        qform_code = FileInfoNIFTI.NIFTI_XFORM_MNI_152;
                        break;

                    default:
                        qform_code = FileInfoNIFTI.NIFTI_XFORM_SCANNER_ANAT;
                }

                if (image.getNDims() >= 3) {
                    axisOrientation = getAxisOrientation(matrixQ);
                    r00 = -matrixQ.get(0, 0) / resols[0];
                    r01 = -matrixQ.get(0, 1) / resols[1];
                    r02 = -matrixQ.get(0, 2) / resols[2];
                    r10 = -matrixQ.get(1, 0) / resols[0];
                    r11 = -matrixQ.get(1, 1) / resols[1];
                    r12 = -matrixQ.get(1, 2) / resols[2];
                    r20 = matrixQ.get(2, 0) / resols[0];
                    r21 = matrixQ.get(2, 1) / resols[1];
                    r22 = matrixQ.get(2, 2) / resols[2];
                } // if (image.getNDims() >= 3)
                else {
                    if ((origin != null) && (origin.length > 2)) {
                        niftiOrigin[2] = origin[2];
                    }
                    if ((axisOrientation[0] != FileInfoBase.ORI_UNKNOWN_TYPE) &&
                        (axisOrientation[1] != FileInfoBase.ORI_UNKNOWN_TYPE) &&
                        (axisOrientation[2] == FileInfoBase.ORI_UNKNOWN_TYPE)) {
                        if ((axisOrientation[0] != FileInfoBase.ORI_R2L_TYPE) && 
                            (axisOrientation[0] != FileInfoBase.ORI_L2R_TYPE) &&
                            (axisOrientation[1] != FileInfoBase.ORI_R2L_TYPE) &&
                            (axisOrientation[1] != FileInfoBase.ORI_L2R_TYPE)) {
                            axisOrientation[2] = FileInfoBase.ORI_R2L_TYPE;
                        }
                        else if ((axisOrientation[0] != FileInfoBase.ORI_A2P_TYPE) && 
                                (axisOrientation[0] != FileInfoBase.ORI_P2A_TYPE) &&
                                (axisOrientation[1] != FileInfoBase.ORI_A2P_TYPE) &&
                                (axisOrientation[1] != FileInfoBase.ORI_P2A_TYPE)) {
                                axisOrientation[2] = FileInfoBase.ORI_A2P_TYPE; 
                        }
                        else {
                            axisOrientation[2] = FileInfoBase.ORI_I2S_TYPE;
                        }
                    }
                    if ((axisOrientation[0] == FileInfoBase.ORI_UNKNOWN_TYPE) ||
                        (axisOrientation[1] == FileInfoBase.ORI_UNKNOWN_TYPE)) {
                        r00 = -1.0;
                        r01 = 0.0;
                        r02 = 0.0;
                        r10 = 0.0;
                        r11 = -1.0;
                        r12 = 0.0;
                        r20 = 0.0;
                        r21 = 0.0;
                        r22 = 1.0;
                    }
                    else {
                        r00 = 0.0;
                        r01 = 0.0;
                        r02 = 0.0;
                        r10 = 0.0;
                        r11 = 0.0;
                        r12 = 0.0;
                        r20 = 0.0;
                        r21 = 0.0;
                        r22 = 0.0;
                        switch (axisOrientation[0]) {
                            case FileInfoBase.ORI_R2L_TYPE:
                                r00 = -1.0;
                                break;
                            case FileInfoBase.ORI_L2R_TYPE:
                                r00 = 1.0;
                                break;
                            case FileInfoBase.ORI_A2P_TYPE:
                                r10 = -1.0;
                                break;
                            case FileInfoBase.ORI_P2A_TYPE:
                                r10 = 1.0;
                                break;
                            case FileInfoBase.ORI_I2S_TYPE:
                                r20 = 1.0;
                                break;
                            case FileInfoBase.ORI_S2I_TYPE:
                                r20 = -1.0;
                        } // switch (axisOrientation[0])
                        
                        switch (axisOrientation[1]) {
                            case FileInfoBase.ORI_R2L_TYPE:
                                r01 = -1.0;
                                break;
                            case FileInfoBase.ORI_L2R_TYPE:
                                r01 = 1.0;
                                break;
                            case FileInfoBase.ORI_A2P_TYPE:
                                r11 = -1.0;
                                break;
                            case FileInfoBase.ORI_P2A_TYPE:
                                r11 = 1.0;
                                break;
                            case FileInfoBase.ORI_I2S_TYPE:
                                r21 = 1.0;
                                break;
                            case FileInfoBase.ORI_S2I_TYPE:
                                r21 = -1.0;
                        } // switch (axisOrientation[1])
                        
                        switch (axisOrientation[2]) {
                            case FileInfoBase.ORI_R2L_TYPE:
                                r02 = -1.0;
                                break;
                            case FileInfoBase.ORI_L2R_TYPE:
                                r02 = 1.0;
                                break;
                            case FileInfoBase.ORI_A2P_TYPE:
                                r12 = -1.0;
                                break;
                            case FileInfoBase.ORI_P2A_TYPE:
                                r12 = 1.0;
                                break;
                            case FileInfoBase.ORI_I2S_TYPE:
                                r22 = 1.0;
                                break;
                            case FileInfoBase.ORI_S2I_TYPE:
                                r22 = -1.0;
                        } // switch (axisOrientation[2])
                    }
                }

                for (j = 0; j < Math.min(3,image.getNDims()); j++) {
                    if (axisOrientation[j] == FileInfoBase.ORI_L2R_TYPE) {
                        niftiOrigin[0] = -Math.abs(matrixQ.get(j,3));
                    } else if (axisOrientation[j] == FileInfoBase.ORI_R2L_TYPE) {
                        niftiOrigin[0] = Math.abs(matrixQ.get(j,3));
                    } else if (axisOrientation[j] == FileInfoBase.ORI_P2A_TYPE) {
                        niftiOrigin[1] = -Math.abs(matrixQ.get(j,3));
                    } else if (axisOrientation[j] == FileInfoBase.ORI_A2P_TYPE) {
                        niftiOrigin[1] = Math.abs(matrixQ.get(j, 3));
                    } else if (axisOrientation[j] == FileInfoBase.ORI_I2S_TYPE) {
                        niftiOrigin[2] = -Math.abs(matrixQ.get(j, 3));
                    } else if (axisOrientation[j] == FileInfoBase.ORI_S2I_TYPE) {
                        niftiOrigin[2] = Math.abs(matrixQ.get(j, 3));
                    }
                }
            } else if (dicomMatrix != null) {
            	transposeMatrix = new TransMatrix(4);
            	for (i = 0; i < 4; i++) {
            		for (j = 0; j < 4; j ++) {
            			transposeMatrix.set(i, j, dicomMatrix.get(j, i));
            		}
            	}
            	if (((transposeMatrix.get(0,2) <= 0) && (xDel > 0)) ||
            	    ((transposeMatrix.get(0,2) >= 0) && (xDel < 0))) {
            	        transposeMatrix.set(0, 2, -transposeMatrix.get(0, 2));	
            	}
            	if (((transposeMatrix.get(1,2) <= 0) && (yDel > 0)) ||
                	    ((transposeMatrix.get(1,2) >= 0) && (yDel < 0))) {
                	        transposeMatrix.set(1, 2, -transposeMatrix.get(1, 2));	
                }
            	if (((transposeMatrix.get(2,2) <= 0) && (zDel > 0)) ||
                	    ((transposeMatrix.get(2,2) >= 0) && (zDel < 0))) {
                	        transposeMatrix.set(2, 2, -transposeMatrix.get(2, 2));	
                }
            	axisOrientation = getAxisOrientation(transposeMatrix);
            	qform_code = FileInfoNIFTI.NIFTI_XFORM_SCANNER_ANAT;
                r00 = -transposeMatrix.get(0, 0);
                r01 = -transposeMatrix.get(0, 1);
                r02 = -transposeMatrix.get(0, 2);
                r10 = -transposeMatrix.get(1, 0);
                r11 = -transposeMatrix.get(1, 1);
                r12 = -transposeMatrix.get(1, 2);
                r20 = transposeMatrix.get(2, 0);
                r21 = transposeMatrix.get(2, 1);
                r22 = transposeMatrix.get(2, 2);
                for (j = 0; j < 3; j++) {

                    if (axisOrientation[j] == FileInfoBase.ORI_L2R_TYPE) {
                        niftiOrigin[0] = -Math.abs(origin[j]);
                    } else if (axisOrientation[j] == FileInfoBase.ORI_R2L_TYPE) {
                        niftiOrigin[0] = Math.abs(origin[j]);
                    } else if (axisOrientation[j] == FileInfoBase.ORI_P2A_TYPE) {
                        niftiOrigin[1] = -Math.abs(origin[j]);
                    } else if (axisOrientation[j] == FileInfoBase.ORI_A2P_TYPE) {
                        niftiOrigin[1] = Math.abs(origin[j]);
                    } else if (axisOrientation[j] == FileInfoBase.ORI_I2S_TYPE) {
                        niftiOrigin[2] = -Math.abs(origin[j]);
                    } else if (axisOrientation[j] == FileInfoBase.ORI_S2I_TYPE) {
                        niftiOrigin[2] = Math.abs(origin[j]);
                    }
                }
            } else { // matrixQ == null
                qform_code = FileInfoNIFTI.NIFTI_XFORM_SCANNER_ANAT;
                if ((axisOrientation != null) && 
                    (axisOrientation[0] != FileInfoBase.ORI_UNKNOWN_TYPE) &&
                    (axisOrientation[1] != FileInfoBase.ORI_UNKNOWN_TYPE)) {
                    if (axisOrientation[2] == FileInfoBase.ORI_UNKNOWN_TYPE) {
                        if ((axisOrientation[0] != FileInfoBase.ORI_R2L_TYPE) && 
                                (axisOrientation[0] != FileInfoBase.ORI_L2R_TYPE) &&
                                (axisOrientation[1] != FileInfoBase.ORI_R2L_TYPE) &&
                                (axisOrientation[1] != FileInfoBase.ORI_L2R_TYPE)) {
                                axisOrientation[2] = FileInfoBase.ORI_R2L_TYPE;
                            }
                            else if ((axisOrientation[0] != FileInfoBase.ORI_A2P_TYPE) && 
                                    (axisOrientation[0] != FileInfoBase.ORI_P2A_TYPE) &&
                                    (axisOrientation[1] != FileInfoBase.ORI_A2P_TYPE) &&
                                    (axisOrientation[1] != FileInfoBase.ORI_P2A_TYPE)) {
                                    axisOrientation[2] = FileInfoBase.ORI_A2P_TYPE; 
                            }
                            else {
                                axisOrientation[2] = FileInfoBase.ORI_I2S_TYPE;
                            }    
                    }
                    r00 = 0.0;
                    r01 = 0.0;
                    r02 = 0.0;
                    r10 = 0.0;
                    r11 = 0.0;
                    r12 = 0.0;
                    r20 = 0.0;
                    r21 = 0.0;
                    r22 = 0.0;
                    switch (axisOrientation[0]) {
                        case FileInfoBase.ORI_R2L_TYPE:
                            r00 = -1.0;
                            break;
                        case FileInfoBase.ORI_L2R_TYPE:
                            r00 = 1.0;
                            break;
                        case FileInfoBase.ORI_A2P_TYPE:
                            r10 = -1.0;
                            break;
                        case FileInfoBase.ORI_P2A_TYPE:
                            r10 = 1.0;
                            break;
                        case FileInfoBase.ORI_I2S_TYPE:
                            r20 = 1.0;
                            break;
                        case FileInfoBase.ORI_S2I_TYPE:
                            r20 = -1.0;
                    } // switch (axisOrientation[0])
                    
                    switch (axisOrientation[1]) {
                        case FileInfoBase.ORI_R2L_TYPE:
                            r01 = -1.0;
                            break;
                        case FileInfoBase.ORI_L2R_TYPE:
                            r01 = 1.0;
                            break;
                        case FileInfoBase.ORI_A2P_TYPE:
                            r11 = -1.0;
                            break;
                        case FileInfoBase.ORI_P2A_TYPE:
                            r11 = 1.0;
                            break;
                        case FileInfoBase.ORI_I2S_TYPE:
                            r21 = 1.0;
                            break;
                        case FileInfoBase.ORI_S2I_TYPE:
                            r21 = -1.0;
                    } // switch (axisOrientation[1])
                    
                    switch (axisOrientation[2]) {
                        case FileInfoBase.ORI_R2L_TYPE:
                            r02 = -1.0;
                            break;
                        case FileInfoBase.ORI_L2R_TYPE:
                            r02 = 1.0;
                            break;
                        case FileInfoBase.ORI_A2P_TYPE:
                            r12 = -1.0;
                            break;
                        case FileInfoBase.ORI_P2A_TYPE:
                            r12 = 1.0;
                            break;
                        case FileInfoBase.ORI_I2S_TYPE:
                            r22 = 1.0;
                            break;
                        case FileInfoBase.ORI_S2I_TYPE:
                            r22 = -1.0;
                    } // switch (axisOrientation[2])
                    
                    for (j = 0; j < 3; j++) {

                        if (axisOrientation[j] == FileInfoBase.ORI_L2R_TYPE) {
                            niftiOrigin[0] = -Math.abs(origin[j]);
                        } else if (axisOrientation[j] == FileInfoBase.ORI_R2L_TYPE) {
                            niftiOrigin[0] = Math.abs(origin[j]);
                        } else if (axisOrientation[j] == FileInfoBase.ORI_P2A_TYPE) {
                            niftiOrigin[1] = -Math.abs(origin[j]);
                        } else if (axisOrientation[j] == FileInfoBase.ORI_A2P_TYPE) {
                            niftiOrigin[1] = Math.abs(origin[j]);
                        } else if (axisOrientation[j] == FileInfoBase.ORI_I2S_TYPE) {
                            niftiOrigin[2] = -Math.abs(origin[j]);
                        } else if (axisOrientation[j] == FileInfoBase.ORI_S2I_TYPE) {
                            niftiOrigin[2] = Math.abs(origin[j]);
                        }
                    }
                }
                else {
                    r00 = -1.0;
                    r01 = 0.0;
                    r02 = 0.0;
                    r10 = 0.0;
                    r11 = -1.0;
                    r12 = 0.0;
                    r20 = 0.0;
                    r21 = 0.0;
                    r22 = 1.0;
                }
            } // else matrixQ == null

            // Compute lengths of each column; these determine grid spacings
            xd = Math.sqrt((r00 * r00) + (r10 * r10) + (r20 * r20));
            yd = Math.sqrt((r01 * r01) + (r11 * r11) + (r21 * r21));
            zd = Math.sqrt((r02 * r02) + (r12 * r12) + (r22 * r22));

            // If a column length is zero, patch the trouble
            if (xd == 0.0) {
                r00 = 1.0;
                r10 = 0.0;
                r20 = 0.0;
                xd = 1.0;
            }

            if (yd == 0.0) {
                r01 = 0.0;
                r11 = 1.0;
                r21 = 0.0;
                yd = 1.0;
            }

            if (zd == 0.0) {
                r02 = 0.0;
                r12 = 0.0;
                r22 = 1.0;
                zd = 1.0;
            }

            // Normalize the columns
            r00 /= xd;
            r10 /= xd;
            r20 /= xd;
            r01 /= yd;
            r11 /= yd;
            r21 /= yd;
            r02 /= zd;
            r12 /= zd;
            r22 /= zd;

            // At this point the matrix has normal columns, but the matrix may not have
            // normal columns.  So find the orthogonal matrix closest to the current matrix

            // One reason for using the polar decomposition to get this orthogonal matrix,
            // rather than just directly orthogonalizing the columns, is so that inputting
            // the inverse matrix to R will result in the inverse orthogonal matrix at this
            // point.  If we just orhtogonalized the columns, this wouldn't necessarily hold.

            Q = new Matrix(3, 3);
            Q.set(0, 0, r00);
            Q.set(0, 1, r01);
            Q.set(0, 2, r02);
            Q.set(1, 0, r10);
            Q.set(1, 1, r11);
            Q.set(1, 2, r12);
            Q.set(2, 0, r20);
            Q.set(2, 1, r21);
            Q.set(2, 2, r22);

            P = mat33_polar(Q);

            // Now the matrix is orthogonal
            r00 = P.get(0, 0);
            r01 = P.get(0, 1);
            r02 = P.get(0, 2);
            r10 = P.get(1, 0);
            r11 = P.get(1, 1);
            r12 = P.get(1, 2);
            r20 = P.get(2, 0);
            r21 = P.get(2, 1);
            r22 = P.get(2, 2);

            // Compute the determinant to determine if it is proper
            zd = P.det();

            if (zd > 0) { // proper
                qfac = 1.0f;
            } else {
                qfac = -1.0f;
                r02 = -r02;
                r12 = -r12;
                r22 = -r22;
            }

            // Now compute the quaternion parameters
            a = r00 + r11 + r22 + 1.0;

            if (a > 0.5) {
                a = 0.5 * Math.sqrt(a);
                b = 0.25 * (r21 - r12) / a;
                c = 0.25 * (r02 - r20) / a;
                d = 0.25 * (r10 - r01) / a;
            } else {
                xd = 1.0 + r00 - (r11 + r22);
                yd = 1.0 + r11 - (r00 + r22);
                zd = 1.0 + r22 - (r00 + r11);

                if (xd > 1.0) {
                    b = 0.5 * Math.sqrt(xd);
                    c = 0.25 * (r01 + r10) / b;
                    d = 0.25 * (r02 + r20) / b;
                    a = 0.25 * (r21 - r12) / b;
                } else if (yd > 1.0) {
                    c = 0.5 * Math.sqrt(yd);
                    b = 0.25 * (r01 + r10) / c;
                    d = 0.25 * (r12 + r21) / c;
                    a = 0.25 * (r02 - r20) / c;
                } else {
                    d = 0.5 * Math.sqrt(zd);
                    b = 0.25 * (r02 + r20) / d;
                    c = 0.25 * (r12 + r21) / d;
                    a = 0.25 * (r10 - r01) / d;
                }

                if (a < 0.0) {
                    a = -a;
                    b = -b;
                    c = -c;
                    d = -d;
                }
            }

            quatern_a = (float) a;
            quatern_b = (float) b;
            quatern_c = (float) c;
            quatern_d = (float) d;
        } // if ((matrixQ != null) || ((matrixQ == null) && (matrixS == null)))

        if (matrixS != null) {
            Preferences.debug("matrixS on write entry = " + matrixS + "\n", Preferences.DEBUG_FILEIO);

            // To use matrixS information must have sform_code > 0,
            // so cannot have sform_code equal to FileInfoNIFTI.NIFTI_XFORM_UNKNOWN.
            switch (transformIDS) {

                case TransMatrix.TRANSFORM_NIFTI_SCANNER_ANATOMICAL:
                    sform_code = FileInfoNIFTI.NIFTI_XFORM_SCANNER_ANAT;
                    break;

                case TransMatrix.TRANSFORM_ANOTHER_DATASET:
                    sform_code = FileInfoNIFTI.NIFTI_XFORM_ALIGNED_ANAT;
                    break;

                case TransMatrix.TRANSFORM_TALAIRACH_TOURNOUX:
                    sform_code = FileInfoNIFTI.NIFTI_XFORM_TALAIRACH;
                    break;

                case TransMatrix.TRANSFORM_MNI_152:
                    sform_code = FileInfoNIFTI.NIFTI_XFORM_MNI_152;
                    break;

                default:
                    sform_code = FileInfoNIFTI.NIFTI_XFORM_SCANNER_ANAT;
            }

            niftiOriginS = new float[3];
            axisOrientation = getAxisOrientation(matrixS);
            
            for (j = 0; j < 3; j++) {
                if (axisOrientation[j] == FileInfoBase.ORI_L2R_TYPE) {
                    niftiOriginS[0] = -Math.abs(matrixS.get(j,3));
                } else if (axisOrientation[j] == FileInfoBase.ORI_R2L_TYPE) {
                    niftiOriginS[0] = Math.abs(matrixS.get(j, 3));
                } else if (axisOrientation[j] == FileInfoBase.ORI_P2A_TYPE) {
                    niftiOriginS[1] = -Math.abs(matrixS.get(j, 3));
                } else if (axisOrientation[j] == FileInfoBase.ORI_A2P_TYPE) {
                    niftiOriginS[1] = Math.abs(matrixS.get(j, 3));
                } else if (axisOrientation[j] == FileInfoBase.ORI_I2S_TYPE) {
                    niftiOriginS[2] = -Math.abs(matrixS.get(j, 3));
                } else if (axisOrientation[j] == FileInfoBase.ORI_S2I_TYPE) {
                    niftiOriginS[2] = Math.abs(matrixS.get(j, 3));
                }
            }
        } // if (matrixS != null)

        if (isNIFTI) { // Must be a NIFTI file, can set all NIFTI information based on fileInfo
            setBufferInt(bufferByte, fileInfo.getSizeOfHeader(), 0, endianess);

            // fileName not needed by NIFTI
            setBufferString(bufferByte, fileName, 14);

            // extents not needed by NIFTI
            setBufferInt(bufferByte, 0, 32, endianess);

            // regular - not needed by NIFTI
            bufferByte[38] = (byte) 'r';

            // dim_info
            freq_dim = fileInfo.getFreqDim();
            phase_dim = fileInfo.getPhaseDim();
            slice_dim = fileInfo.getSliceDim();
            dim_info = (byte) ((freq_dim) | (phase_dim << 2) | (slice_dim << 4));
            bufferByte[39] = dim_info;

            for (i = 0; i < niftiExtents.length; i++) {
                setBufferShort(bufferByte, (short) niftiExtents[i], 40 + (i * 2), endianess);
            }

            for (i = niftiExtents.length; i < 8; i++) {
                setBufferShort(bufferByte, (short) 1, 40 + (i * 2), endianess);
            }

            setBufferFloat(bufferByte, fileInfo.getIntentP1(), 56, endianess);
            setBufferFloat(bufferByte, fileInfo.getIntentP2(), 60, endianess);
            setBufferFloat(bufferByte, fileInfo.getIntentP3(), 64, endianess);
            setBufferShort(bufferByte, fileInfo.getIntentCode(), 68, endianess);

            setBufferShort(bufferByte, sourceType, 70, endianess);
            setBufferShort(bufferByte, fileInfo.getBitPix(), 72, endianess);
            setBufferShort(bufferByte, fileInfo.getSliceStart(), 74, endianess);

            // set pixdim[0] to the qfac used in method 2
            setBufferFloat(bufferByte, qfac, 76, endianess);

            niftiResols = new float[nDims];

            for (i = 0; i < nDimsLength1; i++) {
                niftiResols[i] = 1.0f;
            }

            for (i = 0; i < (nDims - nDimsLength1); i++) {
                niftiResols[i + nDimsLength1] = resols[i];
            }

            // Set pixdim[1] to pixdim[nDims]
            for (i = 0; i < nDims; i++) {
                setBufferFloat(bufferByte, niftiResols[i], 80 + (i * 4), endianess);
            }

            for (i = nDims; i < 7; i++) {
                setBufferFloat(bufferByte, 0.0f, 80 + (i * 4), endianess);
            }

            // If data is in the same .nii file as the header, use an offset = 352
            // If the data is in a separate .img file, use a 0 offset
            if (oneFile) {
                setBufferFloat(bufferByte, 352.0f, 108, endianess);
            } else {
                setBufferFloat(bufferByte, 0.0f, 108, endianess);
            }

            // scl_slope
            setBufferFloat(bufferByte, 1.0f, 112, endianess);

            // scl_inter
            setBufferFloat(bufferByte, 0.0f, 116, endianess);
            setBufferShort(bufferByte, fileInfo.getSliceEnd(), 120, endianess);
            bufferByte[122] = fileInfo.getSliceCode();

            // xyzt_units
            bufferByte[123] = (byte) (niftiSpatialUnits | niftiTimeUnits);

            setBufferFloat(bufferByte, fileInfo.getCalMax(), 124, endianess);
            setBufferFloat(bufferByte, fileInfo.getCalMin(), 128, endianess);
            setBufferFloat(bufferByte, fileInfo.getSliceDuration(), 132, endianess);

            if (origin.length >= 4) {

                // tOffset
                setBufferFloat(bufferByte, origin[3], 136, endianess);
            } else {
                setBufferFloat(bufferByte, 0.0f, 136, endianess);
            }

            if ( (image.getType() == ModelStorageBase.COMPLEX) || (image.getType() == ModelStorageBase.DCOMPLEX)) {
                image.calcMinMaxMag(Preferences.is(Preferences.PREF_LOGMAG_DISPLAY));
            } else {
                image.calcMinMax();
            }

            if (image.isColorImage()) {
                imageMax = (int) Math.max(image.getMaxR(), Math.max(image.getMaxG(), image.getMaxB()));
                imageMin = (int) Math.min(image.getMinR(), Math.min(image.getMinG(), image.getMinB()));
            } else {
                imageMax = (int) image.getMax();
                imageMin = (int) image.getMin();
            }

            // not needed for NIFTI
            setBufferInt(bufferByte, imageMax, 140, endianess);

            // not needed for NIFTI
            setBufferInt(bufferByte, imageMin, 144, endianess);

            // make sure that description has been updated to match the modality
            // in FileInfoBase. If the modality is unknown, then leave description alone.
            int modality = fileInfo.getModality();

            if (modality != FileInfoBase.UNKNOWN_MODALITY) {
                fileInfo.setDescription(FileInfoBase.getModalityStr(modality));
            }

            if (fileInfo.getDescription() != null) {
                setBufferString(bufferByte, fileInfo.getDescription(), 148);
            }

            if (fileInfo.getAuxFile() != null) {
                setBufferString(bufferByte, fileInfo.getAuxFile(), 228);
            }

            // Write out info for both method 2 and method 3
            // qform_code
            setBufferShort(bufferByte, (short) qform_code, 252, endianess);

            // sform_code
            setBufferShort(bufferByte, (short) sform_code, 254, endianess);

            if (qform_code > 0) {
                Preferences.debug("Writing quatern_b = " + quatern_b + "\n", Preferences.DEBUG_FILEIO);
                setBufferFloat(bufferByte, quatern_b, 256, endianess);
                Preferences.debug("Writing quatern_c = " + quatern_c + "\n", Preferences.DEBUG_FILEIO);
                setBufferFloat(bufferByte, quatern_c, 260, endianess);
                Preferences.debug("Writing quatern_d = " + quatern_d + "\n");
                setBufferFloat(bufferByte, quatern_d, 264, endianess);

                // qoffset_x
                setBufferFloat(bufferByte, niftiOrigin[0], 268, endianess);

                // qoffset_y
                setBufferFloat(bufferByte, niftiOrigin[1], 272, endianess);

                // qoffset_z
                setBufferFloat(bufferByte, niftiOrigin[2], 276, endianess);
            }

            if (matrixS != null) {

                // System.out.println("matrixS = " + matrixS.toString());
                // srow_x
                setBufferFloat(bufferByte, (float) (-matrixS.get(0, 0)), 280, endianess);
                setBufferFloat(bufferByte, (float) (-matrixS.get(0, 1)), 284, endianess);
                setBufferFloat(bufferByte, (float) (-matrixS.get(0, 2)), 288, endianess);
                setBufferFloat(bufferByte, niftiOriginS[0], 292, endianess);

                // srow_y
                setBufferFloat(bufferByte, (float) (-matrixS.get(1, 0)), 296, endianess);
                setBufferFloat(bufferByte, (float) (-matrixS.get(1, 1)), 300, endianess);
                setBufferFloat(bufferByte, (float) (-matrixS.get(1, 2)), 304, endianess);
                setBufferFloat(bufferByte, niftiOriginS[1], 308, endianess);

                // srow_z
                setBufferFloat(bufferByte, (float) (matrixS.get(2, 0)), 312, endianess);
                setBufferFloat(bufferByte, (float) (matrixS.get(2, 1)), 316, endianess);
                setBufferFloat(bufferByte, (float) (matrixS.get(2, 2)), 320, endianess);
                setBufferFloat(bufferByte, niftiOriginS[2], 324, endianess);
            }

            if (fileInfo.getIntentName() != null) {
                setBufferString(bufferByte, fileInfo.getIntentName(), 328);
            }
        } else { // Not a NIFTI file.  Pad the header with blanks and set all known info

            setBufferInt(bufferByte, fileInfo.getSizeOfHeader(), 0, endianess);
            setBufferString(bufferByte, "         \n", 4);

            // unused in NIFTI
            setBufferString(bufferByte, fileName + "\n", 14);

            // unused in NIFTI
            setBufferInt(bufferByte, 0, 32, endianess);

            // unused in NIFTI
            setBufferShort(bufferByte, (short) 0, 36, endianess);

            // regular unused in NIFTI
            bufferByte[38] = (byte) 'r';

            // dim_info
            bufferByte[39] = 0;

            for (i = 0; i < niftiExtents.length; i++) {
                setBufferShort(bufferByte, (short) niftiExtents[i], 40 + (i * 2), endianess);
            }

            for (i = niftiExtents.length; i < 8; i++) {
                setBufferShort(bufferByte, (short) 1, 40 + (i * 2), endianess);
            }

            // set the voxUnits based on the Units of Measure
            int[] units = myFileInfo.getUnitsOfMeasure();
            fileInfo.setUnitsOfMeasure(units);

            // intent_p1
            setBufferFloat(bufferByte, 0.0f, 56, endianess);

            // intent_p2
            setBufferFloat(bufferByte, 0.0f, 60, endianess);

            // intent_p33
            setBufferFloat(bufferByte, 0.0f, 64, endianess);

            // intent_code
            setBufferShort(bufferByte, (short) 0, 68, endianess);

            Preferences.debug("FileNIFTI:writeHeader(simple): data type = " + sourceType + "\n", Preferences.DEBUG_FILEIO);
            setBufferShort(bufferByte, sourceType, 70, endianess);

            switch (image.getType()) {

                case ModelStorageBase.BOOLEAN:
                    fileInfo.setBitPix((short) 1);
                    break;

                case ModelStorageBase.BYTE:
                    fileInfo.setBitPix((short) 8);
                    break;

                case ModelStorageBase.UBYTE:
                    fileInfo.setBitPix((short) 8);
                    break;

                case ModelStorageBase.SHORT:
                    fileInfo.setBitPix((short) 16);
                    break;

                case ModelStorageBase.USHORT:
                    fileInfo.setBitPix((short) 16);
                    break;

                case ModelStorageBase.INTEGER:
                    fileInfo.setBitPix((short) 32);
                    break;

                case ModelStorageBase.LONG:
                    fileInfo.setBitPix((short) 64);
                    break;

                case ModelStorageBase.FLOAT:
                    fileInfo.setBitPix((short) 32);
                    break;

                case ModelStorageBase.DOUBLE:
                    fileInfo.setBitPix((short) 64);
                    break;

                case ModelStorageBase.ARGB: // only RGB for NIFTI images
                    fileInfo.setBitPix((short) 24);
                    break;

                case ModelStorageBase.COMPLEX:
                    fileInfo.setBitPix((short) 64);
                    break;

                case ModelStorageBase.DCOMPLEX:
                    fileInfo.setBitPix((short) 128);
                    break;

                default:
                    return false;
            }

            Preferences.debug("FileNIFTI:writeHeader(simple): bits per pixel = " + fileInfo.getBitPix() + "\n",
            		Preferences.DEBUG_FILEIO);
            setBufferShort(bufferByte, (short) fileInfo.getBitPix(), 72, endianess);

            // slice_start
            setBufferShort(bufferByte, (short) 0, 74, endianess);

            // set pixdim[0] to the qfac used in method 2
            setBufferFloat(bufferByte, qfac, 76, endianess);

            niftiResols = new float[nDims];

            for (i = 0; i < nDimsLength1; i++) {
                niftiResols[i] = 1.0f;
            }

            for (i = 0; i < (nDims - nDimsLength1); i++) {
                niftiResols[i + nDimsLength1] = resols[i];
            }

            // Set pixdim[1] to pixdim[nDims]
            for (i = 0; i < nDims; i++) {
                setBufferFloat(bufferByte, niftiResols[i], 80 + (i * 4), endianess);
            }

            for (i = nDims; i < 7; i++) {
                setBufferFloat(bufferByte, 0.0f, 80 + (i * 4), endianess);
            }

            // vox_offset = 352 for data in same .nii file as header
            // vox_offset = 0 for data in separate .img file
            if (oneFile) {
                setBufferFloat(bufferByte, 352.0f, 108, endianess);
            } else {
                setBufferFloat(bufferByte, 0.0f, 108, endianess);
            }

            // scl_slope
            setBufferFloat(bufferByte, 1.0f, 112, endianess);

            // scl_inter
            setBufferFloat(bufferByte, 0.0f, 116, endianess);

            // slice_end
            setBufferShort(bufferByte, (short) 0, 120, endianess);

            // slice_code
            bufferByte[122] = 0;

            // xyzt_units
            bufferByte[123] = (byte) (niftiSpatialUnits | niftiTimeUnits);

            // cal_max
            setBufferFloat(bufferByte, (float) 0, 124, endianess);

            // cal_min
            setBufferFloat(bufferByte, (float) 0, 128, endianess);

            // slice_duration
            setBufferFloat(bufferByte, (float) 0, 132, endianess);

            if (origin.length >= 4) {

                // tOffset
                setBufferFloat(bufferByte, origin[3], 136, endianess);
            } else {
                setBufferFloat(bufferByte, 0.0f, 136, endianess);
            }

            if ( (image.getType() == ModelStorageBase.COMPLEX) || (image.getType() == ModelStorageBase.DCOMPLEX)) {
                image.calcMinMaxMag(Preferences.is(Preferences.PREF_LOGMAG_DISPLAY));
            } else {
                image.calcMinMax();
            }

            if (image.isColorImage()) {
                imageMax = (int) Math.max(image.getMaxR(), Math.max(image.getMaxG(), image.getMaxB()));
                imageMin = (int) Math.min(image.getMinR(), Math.min(image.getMinG(), image.getMinB()));
            } else {
                imageMax = (int) image.getMax();
                imageMin = (int) image.getMin();
            }

            setBufferInt(bufferByte, imageMax, 140, endianess);
            setBufferInt(bufferByte, imageMin, 144, endianess);

            int modality = myFileInfo.getModality();
            fileInfo.setModality(modality);
            setBufferString(bufferByte, FileInfoBase.getModalityStr(modality), 148);

            // aux_file
            setBufferString(bufferByte, " ", 228);

            // qform_code
            setBufferShort(bufferByte, (short) qform_code, 252, endianess);

            // sform_code
            setBufferShort(bufferByte, (short) sform_code, 254, endianess);

            Preferences.debug("Writing quatern_b = " + quatern_b + "\n", Preferences.DEBUG_FILEIO);
            setBufferFloat(bufferByte, quatern_b, 256, endianess);
            Preferences.debug("Writing quatern_c = " + quatern_c + "\n", Preferences.DEBUG_FILEIO);
            setBufferFloat(bufferByte, quatern_c, 260, endianess);
            Preferences.debug("Writing quatern_d = " + quatern_d + "\n", Preferences.DEBUG_FILEIO);
            setBufferFloat(bufferByte, quatern_d, 264, endianess);

            // qoffset_x
            setBufferFloat(bufferByte, niftiOrigin[0], 268, endianess);

            // qoffset_y
            setBufferFloat(bufferByte, niftiOrigin[1], 272, endianess);

            // qoffset_z
            setBufferFloat(bufferByte, niftiOrigin[2], 276, endianess);

            if (matrixS != null) {

                // System.out.println("matrix = " + matrix.toString());
                // srow_x
                setBufferFloat(bufferByte, (float) (-matrixS.get(0, 0)), 280, endianess);
                setBufferFloat(bufferByte, (float) (-matrixS.get(0, 1)), 284, endianess);
                setBufferFloat(bufferByte, (float) (-matrixS.get(0, 2)), 288, endianess);
                setBufferFloat(bufferByte, niftiOriginS[0], 292, endianess);

                // srow_y
                setBufferFloat(bufferByte, (float) (-matrixS.get(1, 0)), 296, endianess);
                setBufferFloat(bufferByte, (float) (-matrixS.get(1, 1)), 300, endianess);
                setBufferFloat(bufferByte, (float) (-matrixS.get(1, 2)), 304, endianess);
                setBufferFloat(bufferByte, niftiOriginS[1], 308, endianess);

                // srow_z
                setBufferFloat(bufferByte, (float) (matrixS.get(2, 0)), 312, endianess);
                setBufferFloat(bufferByte, (float) (matrixS.get(2, 1)), 316, endianess);
                setBufferFloat(bufferByte, (float) (matrixS.get(2, 2)), 320, endianess);
                setBufferFloat(bufferByte, niftiOriginS[2], 324, endianess);
            }


            // intent_name
            setBufferString(bufferByte, " ", 328);

        }

        // ni1\0 for data stored in the .img file
        // n+1\0 for data stored in the same .nii file as the header
        if (oneFile) {
            setBufferString(bufferByte, "n+1\0", 344);
        } else {
            setBufferString(bufferByte, "ni1\0", 344);
        }
        bufferByte[348] = 0;
        bufferByte[349] = 0;
        bufferByte[350] = 0;
        bufferByte[351] = 0;

        if(!doGzip) {
        	raFile.write(bufferByte);
        	raFile.close();
        }

        return true; // Successful write
    }

    /**
     * This method is used when saving a 3D image in an array of 2D files. The file name has numbers appended to
     * correctly order the images.
     *
     * @param   image     the image dataset to be saved
     * @param   fileName  the file name
     * @param   fileDir   the file directory
     * @param   options   file options indicate how to save the image
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void writeHeader3DTo2D(ModelImage image, String fileName, String fileDir, FileWriteOptions options,
    		                       boolean oneFile)
            throws IOException {
        int k, seq;
        int beginSlice = options.getBeginSlice();
        int endSlice = options.getEndSlice();
        String origName = new String(fileName);

        for (k = beginSlice, seq = options.getStartNumber(); k <= endSlice; k++, seq++) {
            fileName = origName;

            if (options.getDigitNumber() == 1) {
                fileName += Integer.toString(seq);
            } else if (options.getDigitNumber() == 2) {

                if (seq < 10) {
                    fileName += "0" + Integer.toString(seq);
                } else {
                    fileName += Integer.toString(seq);
                }
            } else if (options.getDigitNumber() == 3) {

                if (seq < 10) {
                    fileName += "00" + Integer.toString(seq);
                } else if (seq < 100) {
                    fileName += "0" + Integer.toString(seq);
                } else {
                    fileName += Integer.toString(seq);
                }
            } else if (options.getDigitNumber() == 4) {

                if (seq < 10) {
                    fileName += "000" + Integer.toString(seq);
                } else if (seq < 100) {
                    fileName += "00" + Integer.toString(seq);
                } else if (seq < 1000) {
                    fileName += "0" + Integer.toString(seq);
                } else {
                    fileName += Integer.toString(seq);
                }
            }

            writeHeader(image, 1, 1, fileName, fileDir,false, oneFile);

        } // end for loop

    }

    /**
     * This method is used when saving a 4D image in an array of 3D files. The file name has numbers appended to
     * correctly order the images.
     *
     * @param   image     the image dataset to be saved
     * @param   fileName  the file name
     * @param   fileDir   the file directory
     * @param   options   file options indicate how to save the image
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void writeHeader4DTo3D(ModelImage image, String fileName, String fileDir, FileWriteOptions options,
    		                       boolean oneFile)
            throws IOException {
        int k, seq;
        int beginTime = options.getBeginTime();
        int endTime = options.getEndTime();
        String origName = new String(fileName);

        for (k = beginTime, seq = options.getStartNumber(); k <= endTime; k++, seq++) {
            fileName = origName;

            if (options.getDigitNumber() == 1) {
                fileName += Integer.toString(seq);
            } else if (options.getDigitNumber() == 2) {

                if (seq < 10) {
                    fileName += "0" + Integer.toString(seq);
                } else {
                    fileName += Integer.toString(seq);
                }
            } else if (options.getDigitNumber() == 3) {

                if (seq < 10) {
                    fileName += "00" + Integer.toString(seq);
                } else if (seq < 100) {
                    fileName += "0" + Integer.toString(seq);
                } else {
                    fileName += Integer.toString(seq);
                }
            } else if (options.getDigitNumber() == 4) {

                if (seq < 10) {
                    fileName += "000" + Integer.toString(seq);
                } else if (seq < 100) {
                    fileName += "00" + Integer.toString(seq);
                } else if (seq < 1000) {
                    fileName += "0" + Integer.toString(seq);
                } else {
                    fileName += Integer.toString(seq);
                }
            }
            // write header with image, # of images per, and 1 time slice

            writeHeader(image, image.getExtents()[2], 1, fileName, fileDir,false, oneFile);

        } // end for loop

    }

	public byte[] getBufferByte() {
		return bufferByte;
	}
    
    
    

}
