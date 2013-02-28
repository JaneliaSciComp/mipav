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
 * <p>NIFTI can have 2 different transformaton matrices
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
    
    /**
     * File object and input streams 
     * needed for NIFTI compressed files
     */
    private File file;
    private FileInputStream fis;
    private ZipInputStream zin;
    private GZIPInputStream gzin;
    private CBZip2InputStream bz2in;

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
    public boolean readHeader(String imageFileName, String fileDir, boolean niftiCompressed) throws IOException {
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
                if (ecode == 4) {
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
                    	Preferences.debug("ecode = 0 for an unknown private format\n", Preferences.DEBUG_FILEIO);
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
            } // if (ecodeNumber >= 1)
        } // else 
        

        
        
        if(raFile != null) {
        	raFile.close();
        }
        return true; // If it got this far, it has successfully read in the header
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
    public ModelImage readImage(boolean one, boolean niftiCompressed) throws IOException, OutOfMemoryError {
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
                if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory(),true)) {
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
                if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory(),true)) {
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
                if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory(),true)) {
                    throw (new IOException(" NIFTI header file error"));
                }
        	}
        }else {
        	if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory(),false)) {
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
                        	e.printStackTrace();
                            MipavUtil.displayError("IOException on gzin.read(buffer) for " + fileName);
                            return null;
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
                        	e.printStackTrace();
                            MipavUtil.displayError("IOException on gzin.read(buffer) for " + fileName);
                            return null;
                        } 
                    }
            	}
            }else {
            	FileRaw rawFile;
                rawFile = new FileRaw(fileInfo.getFileName(), fileInfo.getFileDirectory(), fileInfo, FileBase.READ);
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
            throw new IOException("FileNIFTI: " + error);
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
     */
    private byte[] getFullBuffer(InputStream in, byte[] buff, int off, int fullBufferSize) {
    	
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
        	 e.printStackTrace();
             MipavUtil.displayError("IOException on gzin.read(buffer)");
             
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

            if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory(),false)) {
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
