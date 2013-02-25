package gov.nih.mipav.model.file;


import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.JamaMatrix;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;

import java.text.*;

import java.util.*;


/**
 * Afni file format.
 *
 * <p>In Afni 3 .HEAD files may be present with just 1 .BRIK file containing the original Data. The +orig.BRIK file will
 * contain the data, the +orig.HEAD file will describe transformation to create the original image, the +acpc.HEAD file
 * will describe the transformation needed to create the acpc image, and the +tlrc.HEAD file will describe the
 * transformation needed to create the Talairach image.</p>
 *
 * <p><b>Special notes:</b></p>
 *
 * <p>The document README.attributes specifies DICOM order for MARKS_XYZ, but the sample file anat+orig.HEAD has dataset
 * order. The sample file anat+ACPC.HEAD has DICOM order.</p>
 *
 * <p>The original data must be resampled to cubic voxels for +acpc and +tlrc space. In FAQ 67, How can I compare
 * 'small' and 'big' in Talairach box datasets, the text states:<br>
 * If you have used the default 1 mm. voxel size in +tlrc coordinates(as almost everyone does)...</p>
 *
 * <p>The function THD_3dmm_to_dicomm does changes the ordering of x, y, and z, but never inverts them. That is either
 * R-L or L-R could be present in AFNI's Dicom ordering, either S-I or I-S could be present in AFNI's dicom ordering,
 * and either A-P or P-A could be present in AFNI's dicom ordering. I perform inversions in my code so that I always
 * have x = R-L, Y = A-P, and Z = I-S, the standard dicom ordering.</p>
 *
 * <p>To obtain the afni rr, bvec, and svec values, I must not invert axes in dicom ordering, and I must not adjust the
 * marker coordinates for resolution and origin.</p>
 *
 * <p>The function of the SKIP fields is not defined in the AFNI documentation.</p>
 *
 * <p>Note that AFNI code uses mfor for forward transformation and mbac for backward transformation. Our interpolation
 * routines perform a output to input mapping so since I am skipping the transform() section of AlgorithmTransform which
 * has the line xfrm = matrixtoInverseArray(transMatrix);<br>
 * I wish to use mbac in FileAfni and ViewJFrameTriImage.<br>
 * For +orig to +acpc mapping mbac is simply the transpose(also the inverse) of mfor.<br>
 * For +acpc to +tlrc use 1 over the scale factors in mbac as opposed to the scale factors in mfor.<br>
 * </p>
 *
 * <p>Note that in the sample files func+orig.head and anat+orig.head the origin location in millimeters is chosen so
 * that 0,0,0 in millimeters is at the center of the image. In anat+acpc.head and anat+tlrc.head the origin location in
 * millimeters ensures that the Talairach center is at 0,0,0.</p>
 *
 * <p>AFNI:Software for Analysis and Visualization of Functional Magnetic Resonance Neuroimages by Robert W. Cox states:
 * "In some applications, gaps are present between the functional slices. For example, some investigators using sagittal
 * functional images do not collect data that spans the longitudinal fissure, but instead leave a 3-5 mm gap there. The
 * auxiliary program abut can provide zero-filled images to fill in the gaps, and can resample the nonzero slices to a
 * finer mesh if needed(e.g., to fill a 2 mm gap in a series of 4 mm functional slices). Resampling in the slice select
 * direction between contiguous input slices can be done using nearest- neighbor, linear, or cubic interpolation, at the
 * user's discretion. Interpolation is not done across the boundaries between the gap-filling zero images and the
 * nonzero user-supplied images; that is, interpolation is only done inside spatially contiguous blocks of actual input
 * data."</p>
 *
 * <p>A second special consideration exists in the interpolation of functional data. The MCW AFNI - User Manual states:
 * "In this version of AFNI, the threshold data (e.g., correlation coefficient) is always resampled using the nearest
 * neighbor method. This is because thresholding with an interpolated nonlinear statistic is a somewhat dubious
 * procedure." At another point the manual states: "Threshold data in functional datasets is always resampled using the
 * nearest neighbor mode. This is becasue it is somewhat unreasonable to interpolate a nonlinear statistic (such as
 * correlation coefficient) between voxels, and then to interpret this statistic using probabilistic models that assume
 * independence."</p>
 *
 * <p>v1:time+orig.HEAD has 1 = ORI_L2R_TYPE for the third ORIENT_SPECIFIC value, -53 for the third ORIGIN value, and 7
 * for the third delta value. However, delta should be negative for ORI_L2R_TYPE. I changed the values to those found in
 * v2:time+orig.HEAD, namely 1 for the third ORIENT_SPECIFIC value, 52 for the third ORIGIN value, and -7 for the third
 * delta value. The observed overlap was then excellent for v1_time+orig.HEAD and anat+orig.HEAD, anat+ACPC.HEAD, and
 * anat+TLRC.HEAD.</p>
 *
 * @author  William Gandler
 * @see     FileIO
 */

public class FileAfni extends FileBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** attribute types. */
    public static final int INTEGER_ATTRIBUTE = 1;

    /** DOCUMENT ME! */
    public static final int FLOAT_ATTRIBUTE = 2;

    /** DOCUMENT ME! */
    public static final int STRING_ATTRIBUTE = 3;

    /** funcType for anatType == true. */
    public static final int ANAT_SPGR_TYPE = 0;

    /** DOCUMENT ME! */
    public static final int ANAT_FSE_TYPE = 1;

    /** DOCUMENT ME! */
    public static final int ANAT_EPI_TYPE = 2;

    /** DOCUMENT ME! */
    public static final int ANAT_MRAN_TYPE = 3;

    /** DOCUMENT ME! */
    public static final int ANAT_CT_TYPE = 4;

    /** DOCUMENT ME! */
    public static final int ANAT_SPECT_TYPE = 5;

    /** DOCUMENT ME! */
    public static final int ANAT_PET_TYPE = 6;

    /** DOCUMENT ME! */
    public static final int ANAT_MRA_TYPE = 7;

    /** DOCUMENT ME! */
    public static final int ANAT_BMAP_TYPE = 8;

    /** DOCUMENT ME! */
    public static final int ANAT_DIFF_TYPE = 9;

    /** DOCUMENT ME! */
    public static final int ANAT_OMRI_TYPE = 10;

    /** DOCUMENT ME! */
    public static final int ANAT_BUCK_TYPE = 11;
    /* At this time, Anat codes 0..10 are treated identically by all AFNI programs.  Code 11
     * marks the dataset as a "bucket" type, which is treated differently in the display; a chooser allows you to
     * specify which sub-brick from the bucket should be used tomake the underlay image. */

    /** funcType for anatType == false. */
    public static final int FUNC_FIM_TYPE = 0; /* 1 value */

    /** DOCUMENT ME! */
    public static final int FUNC_THR_TYPE = 1; /* obsolete */

    /** DOCUMENT ME! */
    public static final int FUNC_COR_TYPE = 2; /* fico: correlation */

    /** DOCUMENT ME! */
    public static final int FUNC_TT_TYPE = 3; /* fitt: t-statistic */

    /** DOCUMENT ME! */
    public static final int FUNC_FT_TYPE = 4; /* fift: F-statistic */

    /** DOCUMENT ME! */
    public static final int FUNC_ZT_TYPE = 5; /* fizt: z-score */

    /** DOCUMENT ME! */
    public static final int FUNC_CT_TYPE = 6; /* fict: Chi squared */

    /** DOCUMENT ME! */
    public static final int FUNC_BT_TYPE = 7; /* fibt: Beta stat */

    /** DOCUMENT ME! */
    public static final int FUNC_BN_TYPE = 8; /* fibn: Binomial */

    /** DOCUMENT ME! */
    public static final int FUNC_GT_TYPE = 9; /* figt: Gamma */

    /** DOCUMENT ME! */
    public static final int FUNC_PT_TYPE = 10; /* fipt: Poisson */

    /** DOCUMENT ME! */
    public static final int FUNC_BUCK_TYPE = 11; /* fbuc: bucket */
    
    /**
     * Unfortunately, the func type codes overlap for Func and Anat datasets. This means that one cannot tell the
     * contents of a dataset from a single attribute. Default scale factors for functional data threshold values stored
     * as shorts If BRICK_FLOAT_FACS is not supplied and the threshold functional data is stored as shorts, then AFNI
     * will apply a default scale factor unless the data is FUNC_FIM_TYPE with only an intensity value and no threshold
     * value or FUNC_BUCK_TYPE which has a default scale factor of 1.
     */
    public static final float FUNC_THR_SCALE_SHORT = 0.0001f;

    /** DOCUMENT ME! */
    public static final float FUNC_COR_SCALE_SHORT = 0.0001f;

    /** DOCUMENT ME! */
    public static final float FUNC_TT_SCALE_SHORT = 0.001f;

    /** DOCUMENT ME! */
    public static final float FUNC_FT_SCALE_SHORT = 0.01f;

    /** DOCUMENT ME! */
    public static final float FUNC_ZT_SCALE_SHORT = 0.001f;

    /** DOCUMENT ME! */
    public static final float FUNC_CT_SCALE_SHORT = 0.01f;

    /** DOCUMENT ME! */
    public static final float FUNC_BT_SCALE_SHORT = 0.0001f;

    /** DOCUMENT ME! */
    public static final float FUNC_BN_SCALE_SHORT = 0.01f;

    /** DOCUMENT ME! */
    public static final float FUNC_GT_SCALE_SHORT = 0.001f;

    /** DOCUMENT ME! */
    public static final float FUNC_PT_SCALE_SHORT = 0.01f;

    /** 6 funcType of functional data have default scaling for byte data. */
    public static final float FUNC_THR_SCALE_BYTE = 0.01f;

    /** DOCUMENT ME! */
    public static final float FUNC_COR_SCALE_BYTE = 0.01f;

    /** DOCUMENT ME! */
    public static final float FUNC_TT_SCALE_BYTE = 0.1f;

    /** DOCUMENT ME! */
    public static final float FUNC_ZT_SCALE_BYTE = 0.1f;

    /** DOCUMENT ME! */
    public static final float FUNC_BT_SCALE_BYTE = 0.01f;

    /** DOCUMENT ME! */
    public static final float FUNC_GT_SCALE_BYTE = 0.1f;

    /** marker types. */
    public static final int MARKSET_ALIGN = 1; /* +orig to +acpc */

    /** DOCUMENT ME! */
    public static final int MARKSET_BOUNDING = 2; /* +acpc to +tlrc */

    /** warp types. */
    public static final int WARP_AFFINE_TYPE = 0;

    /** DOCUMENT ME! */
    public static final int WARP_TALAIRACH_12_TYPE = 1;

    /** time step units. */
    public static final int UNITS_MSEC_TYPE = 77001;

    /** DOCUMENT ME! */
    public static final int UNITS_SEC_TYPE = 77002;

    /** DOCUMENT ME! */
    public static final int UNITS_HZ_TYPE = 77003;

    /** DOCUMENT ME! */
    private static final float ATLAS_FRONT_TO_AC = 70.0f;

    /** DOCUMENT ME! */
    private static final float ATLAS_PC_TO_BACK = 79.0f;

    /** DOCUMENT ME! */
    private static final float ATLAS_BOT_TO_AC = 42.0f;

    /** DOCUMENT ME! */
    private static final float ATLAS_AC_TO_TOP = 74.0f;

    /** DOCUMENT ME! */
    private static final float ATLAS_AC_TO_LAT = 68.0f;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * FUNC_ZT_TYPE Standard Normal -- none -- FUNC_CT_TYPE Chi-squared DOF FUNC_BT_TYPE Incomplete Beta Parameters "a"
     * and "b".
     */
    //private float a, b;

    /** Anterior Comissure in acpc space. */
    private Vector3f acpcAC = new Vector3f(95.0f, 95.0f, 70.0f);

    /** DOCUMENT ME! */
    private Vector3f acpcMaxPt = new Vector3f();

    /** DOCUMENT ME! */
    private Vector3f acpcMinPt = new Vector3f();

    /** DOCUMENT ME! */
    private String acpcName;

    /** DOCUMENT ME! */
    private Vector3f acpcPC;

    /** DOCUMENT ME! */
    private float acpcRes;

    /** DOCUMENT ME! */
    private float acquisitionDuration; // Obtained form TAXIS_FLOATS[2]

    // Durartion of acquisition.  This is 0 in datasets created by
    // to3d (at present).

    /** DOCUMENT ME! */
    private boolean alsoAcpc = false;

    /** DOCUMENT ME! */
    private boolean alsoOrig = false;

    /** DOCUMENT ME! */
    private String anatomyParentnameString;

    /** DOCUMENT ME! */
    private boolean anatType; // true for Anat type, false for Func type

    /** DOCUMENT ME! */
    private Vector3f anotherPtDicom;

    /** DOCUMENT ME! */
    //private int botX, botY, botZ;

    /** DOCUMENT ME! */
    private float[] brickFloatFacs = null; // There should be subBrickNumber(DATASET_RANK[1] or nvals) values here. For
                                           // the p-th sub-brick, if f = BRICK_FLOAT_FACS[p] is positive, then the
                                           // values in .BRIK should be scaled by f to give their "true" values.
                                           // Normally, this would only be used with byte or short types (to save disk
                                           // space), but it is legal to use f > 0 for float sub-bricks as well
                                           // (although pointless and confusing).  If f == 0, then the values are
                                           // unscaled.  Possible uses for f < 0 are reserved for the future. If this
                                           // attribute is not present, then the intensity data is unscaled. If you
                                           // store functional threshold data as shorts, but do not supply a nonzero
                                           // scale factor, then a default one is supplied to the data unless the
                                           // functional data is FUNC_BUCK_TYPE for which FUNC_BUCK_SCALE_SHORT is 1.

    /** DOCUMENT ME! */
    private String brickKeywordsString; // List of keywords for each sub-brick of the dataset.

    /** Sub-brick names */
    private String[] brickLabsString; 

    /**
     * Each BLT is defined by a struct that contains two 3x3 matrices and four 3-vectors (2*3*3 + 4*3 = the 30 numbers).
     * These values are: [mfor] = 3x3 forward transformation matrix [0..8] 0,1,2 correspond to alpha.X, alpha.Y, alpha.Z
     * 3,4,5 correspond to beta.X, beta.Y, beta.Z 6,7,8 correspond to gamma.X, gamma.Y, gamma.Z [mbac] = 3x3 backward
     * transformation matrix [0..17] [bvec] = 3-vector for forward transformation [18..20] [svec] = 3-vector for
     * backward transformation [21..23] [bot] [24..26] [top] [27..29] The matrices are stored in row major order; e.g.,
     * [ 0 1 2] [mfor] = [ 3 4 5] [ 6 7 8] the indices of the [mfor] matrix The forward transformation is [x_map] =
     * [mfor][x_in] - [bvec] The backward transformation is [x_in] = [mbac][x_map] - [svec] which implies [svec] =
     * -[mbac][bvec] and [mbac] = Inverse{[mfor]} The forward transformation is the transformation of Dicom order
     * coordinates from the warp parent dataset (usually in the +orig view) to the warped dataset (usually +acpc or
     * +tlrc). The backward transformation is just the inverse of the forward transformation, and is stored for
     * convenience. It could be recomputed from the forward transformation whenever it was needed. The identity BLT
     * would be stored as these 30 numbers: 1 0 0 0 1 0 [mfor] = I 0 0 1 1 0 0 0 1 0 [mbac] = I 0 0 1 0 0 0 [bvec] = 0 0
     * 0 0 [svec] = 0 botx boty botz topx topy topz bot and top numbers depend on the application. If the transformation
     * is WARP_TALAIRACH_TYPE, then each BLT type only applies to a bounded region of 3-space. The [bot] and [top]
     * vectors define the limits for each BLT, in the warped [x_map] coordinates. These values are used in the function
     * AFNI_transform_vector() to compute the transformation of a 3-vector between +orig and +tlrc coordinates. For
     * example, to compute the transformation from +tlrc back to +orig of a vector [x_tlrc], the code must scan all 12
     * [bot]..[top] regions to see which BLT to use. Similarly, to transform [x_orig] from +orig to +tlrc, the vector
     * must be transformed with each BLT and the result tested to see if it lies within the BLT's [bot]..[top] region.
     * (if a lower bound is supposed to be -infinity, then that element of [bot] is -9999; if an upper bound is supposed
     * to be +infinity then that element of [top] is +9999. For the +orig to +acpc transformation (of WARP_AFFINE_TYPE),
     * the [bot] and [top] vectors store the bounding box of the transformed dataset. However, this fact isn't used
     * much.
     */
    private float[] brickStatAux = null; // This stores the auxiliary statistical information about sub-bricks

    // that contain statistical parameters.  Each unit of the array contains the
    // following:
    // iv = sub-brick index (0..nvals-1)
    // jv = statistical code (see below)
    // nv = number of parameters that follow (may be 0)
    // and then nv more numbers
    // That is, there are nv+3 numbers for each unit of this array, starting at
    // location [0].  After the first unit is read out (from BRICK_STSTAUX[0])
    // up to BRICK_STATAUX[2+BRICK_STATAUX[2]), then the next one starts
    // immediately with the next value of iv.  jv should be one of the 9
    // statistical types supported by AFNI, listed below.
    // Type Index=jv Distribution            Auxiliary parameters [stataux]
    // FUNC_COR_TYPE Correlation Coeff       # Samples, # FIT Param, # Orts

    /** DOCUMENT ME! */
    private float[] brickStats = null; // There should be 2*subbrickNumber = 2*DATASET_RANK[1] = 2*nvals here.

    // For the p-th sub-brick, BRICK_STATS[2*p] is the minimum value stored in
    // the brick, and BRICK_STATS[2*p+1] is the maximum value stored in the brick.
    // If the brick is scaled, then these values refer to the scaled values, NOT
    // to the actual values stored in the .BRIK file.

    /** DOCUMENT ME! */
    private int brickType; // BRICK_TYPES[0] data type stored in the .BRIK file such as UBYTE, SHORT, FLOAT.

    /** DOCUMENT ME! */
    private int brickTypeNumber; // Number of values in BRICK_TYPES.  This should equal DATASET_RANK[1].

    /** DOCUMENT ME! */
    private int brikDataType; // the data type in the .BRIK file

    /** DOCUMENT ME! */
    private String brikFileName; // name of file with data

    /** DOCUMENT ME! */
    private int bufferSize;

    /** DOCUMENT ME! */
    private int[] dataExtents = new int[3]; // dataset dimensions if +orig file

    /** DOCUMENT ME! */
    private int[] dataOrient = new int[3]; // dataset orientations in +orig

    /** DOCUMENT ME! */
    private String datasetKeywordsString; // List of keywords for this dataset.  By convention,

    // keywords are separated by ";".

    /** DOCUMENT ME! */
    private String datasetNameString; // a longer name desciribing the dataset contents

    /** DOCUMENT ME! */
    private int ddof;

    /** DOCUMENT ME! */
    private float[] delta = new float[3]; // Three numbers giving the (x,y,z) voxel sizes, with [0] =

    // x-delta, [1] = y-delta, and [2] = z-delta.
    // dataset order rather than DICOM Order.  These values may be
    // negative; in the example above, where the y axis P-A, then y-delta
    // would be negative.  The center of the (i,j,k) voxel is located at
    // xyz coordinates origin[0] + i*delta[0], origin[1] + j*delta[1],
    // origin[2] + k*delta[2]

    /** DOCUMENT ME! */
    private float[] dicomDelta = new float[3]; // The delta numbers arranged in dicom order

    /** DOCUMENT ME! */
    private float dicomHighestX, dicomHighestY, dicomHighestZ;

    /** Dicom ordered bounding box in mm. */
    private float dicomLowestX, dicomLowestY, dicomLowestZ;

    /** DOCUMENT ME! */
    private int[] dicomOrientation;

    /** DOCUMENT ME! */
    private float[] dicomOrigin = new float[3]; // The origin numbers arranged in dicom order

    /** DOCUMENT ME! */
    private int dicomSliceSize; // dicomXDim * dicomYDim

    /** DOCUMENT ME! */
    private int dicomXDim;

    /** DOCUMENT ME! */
    private int dicomYDim;

    /** DOCUMENT ME! */
    private int dicomZDim;

    /** DOCUMENT ME! */
    private float dist_ant, dist_med, dist_pos, dist_sup, dist_inf, dist_lef, dist_rig;

    /** DOCUMENT ME! */
    private boolean doDicom = false; // false for dataset order, true for dicom order

    /** FUNC_TT_TYPE Student t Degrees-of-Freedom (DOF). */
    private int dof;

    /** DOCUMENT ME! */
    private boolean endianess;

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private String fileDir;


    /** DOCUMENT ME! */
    private FileInfoAfni fileInfo;
    
    private FileInfoAfni fileInfoCopy;

    /** DOCUMENT ME! */
    private long fileLength;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private Vector3f firstPtDicom;

    /** DOCUMENT ME! */
    private int floatsPerTag; // number of floats stored per tag

    /** DOCUMENT ME! */
    private int funcType; // 12 types for anatType == true and
                          // 12 more different types for anatType == false

    /** DOCUMENT ME! */
    private boolean haveReadACPC = false;

    /** DOCUMENT ME! */
    private boolean haveReadOrig = false;

    /** DOCUMENT ME! */
    private String historyNoteString; // a multi-line string giving the history of the dataset

    /** DOCUMENT ME! */
    private String idcodeAnatParentString; // ID code for the "anatomy parent" of this dataset

    // (if it has one).

    /** DOCUMENT ME! */
    private String idcodeDateString; // Maximum of 47 characters giving the creation date for

    // the dataset.

    /** DOCUMENT ME! */
    private String idcodeString; // 15 character string (plus NUL) giving a (hopefully)

    // unique identifier for the dataset, independent of the
    // filename assigned by the user.  ID codes are used to provide
    // links between datasets.

    /** DOCUMENT ME! */
    private String idcodeWarpParentString; // ID code for the "warp parent" of this dataset

    // (if it has one).  This will normally be a dataset in the +orig
    // view, even fo rdatasets transformed from +acpc to +tlrc.  That
    // is, the transformation cain +orig to +acpc to +tlrc is symbolic;
    // when you transform a dataset from +acpc to +tlrc, AFNI catenates
    // that transformation onot the +orig to +acpc result and stores the
    // result, which is the direct transformation from +orig to +tlrc.

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private float[] img2Buffer = null;

    /** DOCUMENT ME! */
    private float[] imgBuffer = null;

    /** DOCUMENT ME! */
    private int[] imgExtents;

    /** DOCUMENT ME! */
    private float[] imgResols = new float[] { 1.0f, 1.0f, 1.0f, 1.0f, 1.0f };

    /** DOCUMENT ME! */
    private boolean invertX, invertY, invertZ; // tells if original image had its axes inverted
                                               // in going to dicom format

    /** DOCUMENT ME! */
    private String label1String; // a short label describing the dataset

    /** DOCUMENT ME! */
    private String label2String; // another short label describing the dataset

    /** DOCUMENT ME! */
    private boolean loadB = false; // tells whether or not an IMAGEB is being loaded

    /** Dataset ordered bounding box in mm. */
    private float lowestX, lowestY, lowestZ, highestX, highestY, highestZ;

    /** DOCUMENT ME! */
    private int marksFlag = -1; // Either MARKSET_ALIGN or MARKSET_BOUNDING
                                // or -1 for invalid

    /** DOCUMENT ME! */
    private int marksFlags = -1; // Either MARKSET_ALIGN or MARKSET_BOUNDING
                                 // or -1 for invalid

    /** DOCUMENT ME! */
    private String[] marksHelpString = new String[10];

    /** DOCUMENT ME! */
    private String[] marksLabString = new String[10];
    // 200 characters give the labels for the markers (20 chars per marker, EXACTLY, including the NULS).  A marker
    // whose string is empty (all NUL characters) will not be defined or shown by AFNI.

    /** DOCUMENT ME! */
    private float[] marksXYZ = new float[30];
    // 30 values giving the xyz-coordinates of the markers
    // for this dataset.  (A maximum of 10 markers can be defined for the
    // dataset.)  MARKS_XYZ[0] = x0, MARKS_XYZ[1] = y0, MARKS_XYZ[2] = z0,
    // MARKS_XYZ[3] = x1, etc.  If a marker's xyz-coordinates are outside
    // the bounding box of the dataset, it is considered not to be set.
    // For this purpose, the bounding box of the dataset extends to the
    // edges of the outermost voxels (not just their centers).
    // Discrpancy:
    // The document README.attributes specifies DICOM order, but the
    // sample file anat+orig.HEAD has dataset order.  The sample file
    // anat+ACPC.HEAD has DICOM order.

    /** FUNC_PT_TYPE Poisson Mean. */
    //private float mean;

    /** FUNC_FT_TYPE F ratio Numerator DOF, Denominator DOF. */
    private int ndof;

    /** DOCUMENT ME! */
    private String noteDate001String;

    /** DOCUMENT ME! */
    private String noteDate002String;

    /** DOCUMENT ME! */
    private String noteDate003String;

    /** DOCUMENT ME! */
    private String noteDate004String;

    /** DOCUMENT ME! */
    private String noteDate005String;

    /** DOCUMENT ME! */
    private String noteNumber001String; // the first auxiliary note attached to the dataset.

    /** DOCUMENT ME! */
    private String noteNumber002String;

    /** DOCUMENT ME! */
    private String noteNumber003String;

    /** DOCUMENT ME! */
    private String noteNumber004String;

    /** DOCUMENT ME! */
    private String noteNumber005String;

    /** DOCUMENT ME! */
    private int notesCount; // The number of auxiliary notes attached to the dataset (from 0 to 999).
                            // NOTE_NUMBER_001 is the first auxiliary note

    /** DOCUMENT ME! */
    private int numberDicomSlices;

    /** DOCUMENT ME! */
    private int numberSlices; // zDim for 3D and zDim * tDim for 4D

    /** DOCUMENT ME! */
    private int numFitParam;

    /** DOCUMENT ME! */
    private int numNuisanceParam;

    /** DOCUMENT ME! */
    private int numSamples;

    /** FUNC_BN_TYPE Binomial # Trials, Probability per trial. */
    private int numTrials;

    /** DOCUMENT ME! */
    private int[] orientSpecific = new int[3];

    /** DOCUMENT ME! */
    private Vector3f origAC;

    /** DOCUMENT ME! */
    private float[] origDelta = new float[3]; // delta read form +orig file in Dicom order, but not
                                              // absolute value

    /** DOCUMENT ME! */
    private boolean origDicom; // true if +orig data is in dicom order

    /** DOCUMENT ME! */
    private int[] origExtents = new int[3];

    /** DOCUMENT ME! */
    private float[] origin = new float[3]; // Three numbers giving the xyz-coordinates of the center of the

    // (0,0,0) voxel in the dataset.  The order of these numbers is the
    // same as the order of the xyz-axes.
    // dataset order rather than DICOM order.  However, the AFNI convention
    // is that R-L, A-P, and I-S are negative to positive.  Thus, if the
    // y-origin is likely to be positive (and the y-delta, below, would
    // be negative).  These numbers are usually computed from the centering
    // controls in to3d.

    /** DOCUMENT ME! */
    private String originalFileName;

    /** DOCUMENT ME! */
    private String origName;

    /** DOCUMENT ME! */
    private float[] origOrigin = new float[3]; // origin read from +orig header in dicom order but
                                               // not inverted

    /** DOCUMENT ME! */
    private int origXDim; // dicom xDim from +orig header

    /** DOCUMENT ME! */
    private int origYDim; // dicom yDim from +orig header

    /** DOCUMENT ME! */
    private int origZDim; // dicom zDim from +orig header

    /** DOCUMENT ME! */
    private Vector3f pcDicom; // pass +orig header info onto +acpc header read
                              // This is the coordinate transformed pc inferior edge

    /** DOCUMENT ME! */
    private Vector3f pointMarker; // For +orig to +ACPC transformations contains the marker xyz voxel numbers
                                  // in dataset order and for +ACPC to +tlrc transformations contains the
                                  // xyz voxel markers in dicom order.

    /** DOCUMENT ME! */
    private Vector3f posteriorMarginDicom; // pass +orig header info onto +acpc header read

    /** DOCUMENT ME! */
    private int presentViewType;

    /** DOCUMENT ME! */
    //private float prob;

    /** DOCUMENT ME! */
    private boolean readACPC = false;

    /** DOCUMENT ME! */
    private boolean readTLRC = false;

    /** DOCUMENT ME! */
    //private float resolution;

    /** DOCUMENT ME! */
    private float scale_A, scale_M, scale_P, scale_S, scale_I, scale_L, scale_R;

    /** FUNC_GT_TYPE Gamma Shape, Scale. */
    //private float shape, scale;

    /** DOCUMENT ME! */
    private float[] skip;

    /** DOCUMENT ME! */
    private int sliceSize; // xDim * yDim

    /** DOCUMENT ME! */
    private int slicesWithTimeOffsets = 0; // Obtained from TAXIS_NUMS[1]

    // Number of slices with time offsets.  If zero, then no slice-
    // dependent time offsets are present (all slices are presumed to
    // be acquired at the same time).  If positive, specifies the number
    // of values to read from TAXIS_OFFSETS.  Normally, this would either
    // be 0 or be equal to DATASET_DIMENSIONS[2] (zDim).

    /** # orts are nuisance parameters whose number is >= 1. */
    private float[] statAux = null; // The BRICK_STATAUX attribute allows you to attach statistical

    // distribution information to arbitrary sub-bricks of a bucket dataset.
    // The oler STAT_AUX attribute is for the Func type datasets of the following
    // types:
    // fico = FUNC_COR_TYPE  fitt = FUNC_TT_TYPE
    // fift = FUNC_FT_TYPE   fict = FUNC_CT_TYPE
    // fibt = FUNC_BT_TYPE   fibn = FUNC_BN_TYPE
    // figt = FUNC_GT_TYPE   fipt = FUNC_PT_TYPE
    // These parameters apply to the second sub-brick (#1) of the dataset.
    // (Datasets of these types must have exactly 2 sub-bricks.)  The number
    // and definition of these parameters is the same as the BRICK_STATAUX
    // cases, above.
    // fico: (correlation coefficient)**2 is incomplete beta distributed, so
    // the fibt type is somewhat redundant.
    // fizt: This is N(0,10 distributed, so there are no parameters.
    // fibn: The "p-value" computed and displayed by AFNI is the probability
    // that a binomial deviate will be larger than the threshold value.
    // figt:  The PDF of the gamma distribution is proportional to
    // x ** (Shape - 1) * exp(-Scale * x)
    // for (x >= 0)
    // fipt: "The p-value" is the probability that a Poisson deviate is larger
    // than the threshold value.

    /** DOCUMENT ME! */
    private int statCode;

    /** DOCUMENT ME! */
    private int subBrickIndex;

    /** DOCUMENT ME! */
    private int subBrickNumber; // DATASET_RANK[1] = Number of sub-bricks in the dataset
                                // in most AFNI programs this is called nvals

    /** DOCUMENT ME! */
    private Vector3f superiorEdgeDicom; // pass +orig header info onto +acpc header read

    /**
     * The "p-values" for fico, fitt, and fizt datasets are 2-sided: that is, the value displayed by AFNI (below the
     * slider) is the probability that the absolute value of such a deviate will exceed the threshold value on the
     * slider. The "p-values" for the other types are 1-sided: that is, the value displayed by AFNI is the probability
     * that the value of the deviate will exceed the threshold value. (Of course, these probabilities are computed under
     * the appropriate null hypothesis, and assuming that the distributional model holds exactly. The latter assumption,
     * in particular, is fairly dubious.) Registration attributes Note that the MATVEC attributes are transformations of
     * Dicom-ordered coordinates, and so have to be permuted to transform dataset-ordered xyz-coordinates. The MATVEC
     * attributes describe the transformation of coordinates from the input dataset to the output dataset in the form
     * [xyz_out] = [mat] ([xyz_in] - [xyz_cen]) + [vec] + [xyz_cen] where [mat] is a 3x3 orthogonal matrix [vec] is a
     * 3-vector [xyz_in] is the input vector [xyz_cen] is the center of rotation (usually the center of the dataset)
     * [xyz_out] is the output vector Dicom coordinate order is used for these matrices and vectors, which means that
     * they need to be permuted to dataset order for application
     */
    private float[] tagAlignMatvec = null; // 12 numbers giving the 3x3 matrix and 3-vector of the transformation

    // derived in 3dTagalign.  The matrix-vector are loaded from the
    // following elements of the attribute:
    // [ 0  1  2  ]          [  3 ]
    // [mat] = [ 4  5  6  ]  [vec] = [  7 ]
    // [ 8  9  10 ]          [ 11 ]

    /** DOCUMENT ME! */
    private int tagNumber; // number of tags defined in the dataset (max = 100)

    /** DOCUMENT ME! */
    private float[] tagsetFloats = null; // tagNumber * floatsPerTag values
                                         // for tag #i:
                                         // [floatsPerTag*i+0] = x-coordinate (Dicom order)
                                         // [floatsPerTag*i+1] = y-coordinate (Dicom order)
                                         // [floatsPerTag*i+2] = z-coordinate (Dicom order)
                                         // [floatsPerTag*i+3] = tag numerical value
                                         // [floatsPerTag*i+4] = sub-brick index of tag (if >= 0)
                                         // or "not set" flag if less than 0

    /** 2560 characters giving the help strings for the markers (256 chars per marker, EXACTLY, including the NULS). */
    private String tagsetLabelsString; // ntag sub-strings (separated by NULS) with the labels

    // for each tag.

    /** DOCUMENT ME! */
    private float[] tAxisOffsets = null; // if TAXIS_NUMS[1] > 0, then this array gives the time offsets of the

    // slices defined by TAXIS_FLOATS[3..4].  The time offset at
    // z = TAXIS_FLOATS[3] + k*TAXIS_FLOATS[4]
    // is TAXIS_OFFSETS[k], for k = 0..TAXIS_NUMS[1] - 1.
    // If TAXIS_NUMS[1] == 0, then this attribute is not used

    /** DOCUMENT ME! */
    private int tDim = 0; // Obtained from TAXIS_NUMS[0];
                          // For 3D + time datasets must equal DATASET_RANK[1] = subBrickNumber
                          // In AFNI programs each time point can only have a single numerical
                          // value per voxel

    /** DOCUMENT ME! */
    private float timeOrigin; // Obtained from TAXIS_FLOATS[0]
                              // Time Origin (in units given by TAXIS_NUMS[2])

    /** DOCUMENT ME! */
    private float timeStep; // Obtained from TAXIS_FLOATS[1]
                            // Time step (TR).

    /** DOCUMENT ME! */
    private int timeStepUnit; // Unit code for TAXIS_FLOATS[1]
                              // Either UNITS_MSEC_TYPE or UNITS_SEC_TYPE or UNITS_HZ_TYPE

    /** DOCUMENT ME! */
    private TalairachTransformInfo tInfo;

    /** DOCUMENT ME! */
    private float[] tlrcRes;

    /** DOCUMENT ME! */
    //private int topX, topY, topZ;

    /** DOCUMENT ME! */
    private String typeString; // one of 3DIM_FileInfoAfni.HEAD_ANAT or 3DIM_FileInfoAfni.HEAD_FUNC or
                               // 3DIM_FileInfoAfni.GEN_ANAT or 3DIM_FileInfoAfni.GEN_FUNC.  Determines if dataset if of
                               // Anat or Func type.  If Anat type, and if it is a _FileInfoAfni.HEAD_ dataset in the
                               // +orig view, then Talairach markers might be attached to it (if it was created by
                               // to3d).

    /** DOCUMENT ME! */
    private int typeStringType; // 1 of the 4 types mentioned in typeString

    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    /** DOCUMENT ME! */
    private int viewType;

    /** DOCUMENT ME! */
    private String volregBaseIdcodeString; // In the 3dvolreg ouput dataset header, this tells

    // which dataset was base for registration

    /** DOCUMENT ME! */
    private String volregBaseNameString; // The .HEAD fileame of the 3dvolreg base dataset.

    /** DOCUMENT ME! */
    private float[] volregCenterBase = null; // The xyz-coordinates (Dicom order) of the center of the base
                                             // dataset to 3dvolreg; this is written to 3dvolreg's output
                                             // dataset

    /**
     * For sub-brick #xxxxxx (so a max of 999,999 sub-bricks can be used), this stores 12 numbers for the matrix-vector
     * of transformation from 3dvolreg.
     */
    private float[] volregCenterOld = null; // The xyz-coordinates (Dicom order) of the center of the input
                                            // dataset to 3dvolreg; this is written to 3dvolreg's output
                                            // dataset.

    /** DOCUMENT ME! */
    private String volregGridparentIdcodeString; // if a 3dvolreg run uses the -gridparent

    // option, then this value in the header of the output
    // dataset tells which dataset was the gridparent.

    /** DOCUMENT ME! */
    private String volregGridparentNameString; // The .HEAD filename of hte -gridparent.

    /** DOCUMENT ME! */
    private String volregInputIdcodeString; // in the 3dvolreg output dataset header,

    // this tells which dataset was input to 3dvolreg.

    /** DOCUMENT ME! */
    private String volregInputNameString; // The .HEAD filename of hte 3dvolreg input

    // dataset.

    /** DOCUMENT ME! */
    private float[] volregMatvec000000 = null;

    /** DOCUMENT ME! */
    private float[] volregMatvec000001 = null;

    /** DOCUMENT ME! */
    private float[] volregMatvec000002 = null;

    /** DOCUMENT ME! */
    private float[] volregMatvec000003 = null;

    /** DOCUMENT ME! */
    private float[] volregMatvec000004 = null;

    /** DOCUMENT ME! */
    private float[] volregMatvec000005 = null;

    /** DOCUMENT ME! */
    private float[] volregMatvec000006 = null;

    /** DOCUMENT ME! */
    private float[] volregMatvec000007 = null;

    /** DOCUMENT ME! */
    private float[] volregMatvec000008 = null;

    /** DOCUMENT ME! */
    private float[] volregMatvec000009 = null;

    /** DOCUMENT ME! */
    private int volregRotcomNum; // The single value in here tells how many sub-bricks were

    // registered by 3dvolreg.  (The only reason this might be different
    // than nvals is that someone might later tack extra sub-bricks onto
    // this dataset using 3dTcat.)  This is how many VOLREG_MATVEC_xxxxx
    // and VOLREG_ROTCOM_xxxxxx attributes are present in the dataset.

    /** DOCUMENT ME! */
    private String volregRotparentIdcodeString; // if a 3dvolreg run uses the -rotparent option,

    // then this value in the header of the output dataset tells
    // which dataset was the rotparent.

    /** DOCUMENT ME! */
    private String volregRotparentNameString; // The .HEAD filename of the -rotparent.

    /** DOCUMENT ME! */
    private float[] warpData = null; // Data that define the transformation from the warp data to the current

    // dataset.  Each basic linear transformation (BLT) takes 30 numbers.  For
    // WARP_AFFINE_TYPE, there is one BLT per warp; for WARP_TALAIRACH_12_TYPE,
    // there are 12 BLTS per warp.  Thus, for WARP_AFFINE_TYPE there should be
    // 30 numbers in WARP_DATA, and for WARP_TALAIRACH_12_TYPE there should be
    // 360 numbers.  (WARP_AFFINE_TYPE is used for the +orig to +acpc
    // transformation; WARP_TALAIRACH_12_TYPE is used the the +orig to +tlrc
    // transformation.

    /** DOCUMENT ME! */
    private String warpParentnameString;

    /** DOCUMENT ME! */
    private int warpType; // Either WARP_AFFINE_TYPE or WARP_TALAIRACH_12_TYPE

    /** DOCUMENT ME! */
    private int xDim = 0;

    /** DOCUMENT ME! */
    private int yDim = 0;

    /** DOCUMENT ME! */
    private float zAxisOffset; // Obtained form TAXIS_FLOATS[3]
                               // If TAXIS_NUMS[1] > 0, then this is the z-axis offset for the
                               // slice-dependent time offsets.  This will be equal to ORIGIN[2]
                               // in datasets created by to3d.c

    /** DOCUMENT ME! */
    private float zAxisStep; // Obtained from TAXIS_FLOATS[4]
                             // If TAXIS_NUMS[1] > 0, then this is the z-axis step for slice-dependent
                             // time offsets.  This will be equal to DELTA[2] in datasets created
                             // by to3d.c

    /** DOCUMENT ME! */
    private int zDim = 0;

    /** DOCUMENT ME! */
    private int[] zeroPad; // 3 integers specifying how much zero-padding to3d applied when it
                           // created the dataset (x,y,z axes).

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Afni reader constructor.
     *
     * @param      fileName  file name
     * @param      fileDir   file directory
     * @param      loadB     flag indicating if this is a B image
     * @param      doRead    DOCUMENT ME!
     *
     * @exception  IOException  if there is an error making the file
     */
    public FileAfni(String fileName, String fileDir, boolean loadB, boolean doRead)
            throws IOException {
        int s, t;
        String viewString;
        String suffix;

        UI = ViewUserInterface.getReference();
        this.fileName = fileName;
        originalFileName = fileName;
        this.fileDir = fileDir;
        this.loadB = loadB;

        // get view type
        s = fileName.lastIndexOf("+");

        if (s == -1) {
            throw new IOException("Afni file name Error: + sign not found");
        }

        t = fileName.lastIndexOf(".");

        if (t != (s + 5)) {
            throw new IOException("Afni file name Error: . not found 5 characters after + sign");
        }

        viewString = fileName.substring(s + 1, t);

        if (doRead) {

            // If have a name+tlrc.HEAD and name+tlrc.BRIK or
            // a name+acpc.HEAD and name+acpc.BRIK, then open
            // without transformations.
            tInfo = new TalairachTransformInfo();

            boolean sameType = false;

            try {
                brikFileName = fileName.substring(0, t + 1) + "BRIK";
                file = new File(fileDir + brikFileName);
                raFile = new RandomAccessFile(file, "r");
                this.fileName = fileName.substring(0, t + 1) + "HEAD";
                sameType = true;
            } catch (FileNotFoundException e) {
                brikFileName = fileName.substring(0, s + 1) + "orig.BRIK";
                this.fileName = fileName.substring(0, s + 1) + "orig.HEAD";
            }

            if (sameType) {

                if (viewString.equals("orig")) {
                    viewType = FileInfoAfni.AFNI_ORIG;
                    presentViewType = FileInfoAfni.AFNI_ORIG;
                } else if (viewString.equals("acpc")) {
                    viewType = FileInfoAfni.AFNI_ACPC;
                    presentViewType = FileInfoAfni.AFNI_ACPC;
                } else if (viewString.equals("tlrc")) {
                    viewType = FileInfoAfni.AFNI_TLRC;
                    presentViewType = FileInfoAfni.AFNI_TLRC;
                }
            } else if (viewString.equalsIgnoreCase("acpc")) {
                viewType = FileInfoAfni.AFNI_ACPC;

                // Always read in the +orig.HEAD file before the +acpc.HEAD file
                readACPC = true;
                presentViewType = FileInfoAfni.AFNI_ORIG;
            } else if (viewString.equalsIgnoreCase("tlrc")) {
                viewType = FileInfoAfni.AFNI_TLRC;

                // Always read in the +orig.HEAD file before the +tlrc.HEAD file
                readTLRC = true;
                presentViewType = FileInfoAfni.AFNI_ORIG;
            } else {
                throw new IOException("Afni file name error: orig, acpc, or tlrc not found");
            }

            suffix = fileName.substring(t + 1);

            // Always read in the .HEAD file before the .BRIK file
            if (suffix.equalsIgnoreCase("BRIK")) {
                this.fileName = fileName.substring(0, t + 1) + "HEAD";
            }

            if ((viewType == FileInfoAfni.AFNI_ORIG) || (viewType == FileInfoAfni.AFNI_TLRC)) {

                // Reading an acpc file is not necessary for reading this particular image.
                // However, it must be done to fill in certain fields of the TalairachTransformInfo
                // structure - acpcRes, acpcPC, acpcMin, and acpcMax.
                try {
                    acpcName = fileName.substring(0, s + 1) + "acpc.HEAD";
                    file = new File(fileDir + acpcName);
                    raFile = new RandomAccessFile(file, "r");
                    alsoAcpc = true;
                } catch (FileNotFoundException e) { }
            } // if ((viewType == FileInfoAfni.AFNI_ORIG) || (viewType == FileInfoAfni.AFNI_TLRC))

            if ((sameType) && ((viewType == FileInfoAfni.AFNI_ACPC) || (viewType == FileInfoAfni.AFNI_TLRC))) {

                // Reading an orig file is not necessary for reading this particular image.
                // However, it must be done to fill in certain fields of the TalairachTransformInfo
                // structure - origAC, origPC, origDim, origOrigin, and origRes
                try {
                    origName = fileName.substring(0, s + 1) + "orig.HEAD";
                    file = new File(fileDir + origName);
                    raFile = new RandomAccessFile(file, "r");
                    alsoOrig = true;
                } catch (FileNotFoundException e) { }
            } // if ((sameType) &&
        } // if (doRead)
        else { // write

            if (viewString.equalsIgnoreCase("orig")) {
                viewType = FileInfoAfni.AFNI_ORIG;
                presentViewType = FileInfoAfni.AFNI_ORIG;
            } else if (viewString.equalsIgnoreCase("acpc")) {
                viewType = FileInfoAfni.AFNI_ACPC;

                // Always read in the +orig.HEAD file before the +acpc.HEAD file
                readACPC = true;
                presentViewType = FileInfoAfni.AFNI_ORIG;
                this.fileName = fileName.substring(0, s + 1) + "acpc.HEAD";
            } else if (viewString.equalsIgnoreCase("tlrc")) {
                viewType = FileInfoAfni.AFNI_TLRC;

                // Always read in the +orig.HEAD file before the +tlrc.HEAD file
                readTLRC = true;
                presentViewType = FileInfoAfni.AFNI_ORIG;
                this.fileName = fileName.substring(0, s + 1) + "tlrc.HEAD";
            } else {
                throw new IOException("Afni file name error: orig, acpc, or tlrc not found");
            }

            suffix = fileName.substring(t + 1);

            // Always read in the .HEAD file before the .BRIK file
            if (suffix.equalsIgnoreCase("BRIK")) {
                this.fileName = fileName.substring(0, t + 1) + "HEAD";
            }
        } // write

    }

    //~ Methods --------------------------------------------------------------------------------------------------------
	/**
     * Prepares this class for cleanup. Calls the <code>finalize</code> method for existing elements, closes any open
     * files and sets other elements to <code>null</code>.
     */
    public void finalize() {
        int i;
        acpcAC = null;
        acpcMaxPt = null;
        acpcMinPt = null;
        acpcName = null;
        acpcPC = null;
        anatomyParentnameString = null;
        anotherPtDicom = null;
        brickFloatFacs = null;
        brickKeywordsString = null;
        brickLabsString = null;
        brickStatAux = null;
        brickStats = null;
        brikFileName = null;
        dataExtents = null;
        dataOrient = null;
        datasetKeywordsString = null;
        datasetNameString = null;
        delta = null;
        dicomDelta = null;
        dicomOrientation = null;
        dicomOrigin = null;
        file = null;
        fileName = null;
        fileDir = null;
        fileInfo = null;
        fileInfoCopy = null;
        firstPtDicom = null;
        historyNoteString = null;
        idcodeAnatParentString = null;
        idcodeDateString = null;
        idcodeString = null;
        idcodeWarpParentString = null;
        img2Buffer = null;
        image = null;
        imgBuffer = null;
        imgExtents = null;
        imgResols = null;
        label1String = null;
        label2String = null;
        if (marksHelpString != null) {
            for (i = 0; i < marksHelpString.length; i++) {
                marksHelpString[i] = null;
            }
        }
        marksHelpString = null;
        if (marksLabString != null) {
            for (i = 0; i < marksLabString.length; i++) {
                marksLabString[i] = null;
            }
        }
        marksLabString = null;
        marksXYZ = null;
        noteDate001String = null;
        noteDate002String = null;
        noteDate003String = null;
        noteDate004String = null;
        noteDate005String = null;
        noteNumber001String = null;
        noteNumber002String = null;
        noteNumber003String = null;
        noteNumber004String = null;
        noteNumber005String = null;
        orientSpecific = null;
        origAC = null;
        origDelta = null; 
        origExtents = null;
        origin = null;
        originalFileName = null;
        origName = null;
        origOrigin = null;
        pcDicom = null; 
        pointMarker = null;
        posteriorMarginDicom = null;
        skip = null;
        statAux = null;
        superiorEdgeDicom = null;
        tagAlignMatvec = null;
        tagsetFloats = null;
        tagsetLabelsString = null;
        tAxisOffsets = null;
        // Do not do tInfo.finalize();
        // Do not do tInfo = null;
        // tInfo may be needed for transformation between ORIG, ACPC, and TLRC.
        tlrcRes = null;
        typeString = null;
        volregBaseIdcodeString = null;
        volregBaseNameString = null;
        volregCenterBase = null;
        volregCenterOld = null;
        volregGridparentIdcodeString = null;
        volregGridparentNameString = null;
        volregInputIdcodeString = null;
        volregInputNameString = null;
        volregMatvec000000 = null;
        volregMatvec000001 = null;
        volregMatvec000002 = null;
        volregMatvec000003 = null;
        volregMatvec000004 = null;
        volregMatvec000005 = null;
        volregMatvec000006 = null;
        volregMatvec000007 = null;
        volregMatvec000008 = null;
        volregMatvec000009 = null;
        volregRotparentIdcodeString = null;
        volregRotparentNameString = null; 
        warpData = null;
        warpParentnameString = null;
        zeroPad = null;
        try {
            super.finalize();
        } catch (Throwable er) { }
    }
    
    /**
     * getFileInfo - accessor that returns the file info.
     *
     * @return  FileInfoBase containing the file info
     */
    public FileInfoBase getFileInfo() {
        return fileInfo;
    }


    /**
     * getImageBuffer - accessor that returns the image buffer.
     *
     * @return  buffer of image.
     */
    public float[] getImageBuffer() {
        return imgBuffer;
    }


    // public void testme(int testNumber) {
    // this.testNumber = testNumber;
    // }

    /**
     * readImage.
     *
     * @return     returns the image
     *
     * @exception  IOException  if there is an error reading the file
     */
    public ModelImage readImage() throws IOException {
        int iXdim, iYdim, iZdim;
        float iXres, iYres, iZres;
        Vector3f rr = new Vector3f(0.0f, 0.0f, 0.0f);
        Vector3f alpha = new Vector3f(0.0f, 0.0f, 0.0f);
        Vector3f beta = new Vector3f(0.0f, 0.0f, 0.0f);
        Vector3f gamma = new Vector3f(0.0f, 0.0f, 0.0f);
        Vector3f center;
        TransMatrix xfrm;
        float oXres, oYres, oZres;
        int oXdim, oYdim, oZdim;

        // acpc
        Vector3f TCenter = new Vector3f(0.0f, 0.0f, 0.0f);

        // Talairach
        Vector3f TalCenter = new Vector3f(0.0f, 0.0f, 0.0f);
        int i, j;
        double Tx = 0.0;
        double Ty = 0.0;
        double Tz = 0.0;
        JamaMatrix A, b, X;
        double Tr03, Tr13, Tr23;
        float Talx, Taly, Talz;
        int[] AFNIOrigExtents = new int[3];
        float[] AFNIOrigResolutions = new float[3];
        Vector3f translation = new Vector3f(0.0f, 0.0f, 0.0f); // last column of transformation matrix
        String nextFileName;
        int nextViewType;
        Vector3f rr1, rr2, dif, acpos, acsup, alpha1, alpha2;

        if (alsoOrig) {
            nextFileName = fileName;
            nextViewType = presentViewType;
            fileName = origName;
            presentViewType = FileInfoAfni.AFNI_ORIG;
            readHeader();
            fileName = nextFileName;
            presentViewType = nextViewType;
        } // if (alsoOrig)

        readHeader();
        readImage2();

        if (readACPC) {

            fireProgressStateChanged(0);
            iXdim = dicomXDim;
            iYdim = dicomYDim;
            iZdim = dicomZDim;
            AFNIOrigExtents[0] = dicomXDim;
            AFNIOrigExtents[1] = dicomYDim;
            AFNIOrigExtents[2] = dicomZDim;
            iXres = dicomDelta[0];
            iYres = dicomDelta[1];
            iZres = dicomDelta[2];
            AFNIOrigResolutions[0] = dicomDelta[0];
            AFNIOrigResolutions[1] = dicomDelta[1];
            AFNIOrigResolutions[2] = dicomDelta[2];
            fileName = originalFileName;
            presentViewType = viewType;
            readHeader();

            // rr = -svec where rr is the Talairach center
            rr.X = -warpData[21];
            rr.Y = -warpData[22];
            rr.Z = -warpData[23];
            rr.X = (rr.X - origOrigin[0]) / origDelta[0];

            if (invertX) {
                rr.X = origXDim - 1 - rr.X;
            }

            rr.Y = (rr.Y - origOrigin[1]) / origDelta[1];

            if (invertY) {
                rr.Y = origYDim - 1 - rr.Y;
            }

            rr.Z = (rr.Z - origOrigin[2]) / origDelta[2];

            if (invertZ) {
                rr.Z = origZDim - 1 - rr.Z;
            }

            Preferences.debug("svec rr = " + rr.X + "," + rr.Y + "," + rr.Z + "\n", Preferences.DEBUG_FILEIO);

            xfrm = new TransMatrix(4);
            center = new Vector3f();
            center.X = (image.getExtents()[0] - 1) / 2.0f;
            center.Y = (image.getExtents()[1] - 1) / 2.0f;
            center.Z = (image.getExtents()[2] - 1) / 2.0f;
            bufferSize = iXdim * iYdim * iZdim;
            imgBuffer = new float[bufferSize];

            try {
                image.exportData(0, bufferSize, imgBuffer);
            } catch (IOException error) {
                MipavUtil.displayError("FileAfni: IOException error on exportData");
            }

            int type = image.getType();
            image.disposeLocal();
            image = null;
            image = new ModelImage(type, imgExtents, originalFileName);


            xfrm.setTranslate(center.X, center.Y, center.Z);
            Preferences.debug("center.X = " + center.X + " center.Y = " + center.Y + " center.Z = " + center.Z + "\n",
            		Preferences.DEBUG_FILEIO);

            // Since our interpolation routines are doing a output to input mapping, create the mbac
            // transformation matrix which is the transpose(also the inverse) of the mfor matrix.
            alpha.X = warpData[9];
            alpha.Y = warpData[10];
            alpha.Z = warpData[11];
            beta.X = warpData[12];
            beta.Y = warpData[13];
            beta.Z = warpData[14];
            gamma.X = warpData[15];
            gamma.Y = warpData[16];
            gamma.Z = warpData[17];
            Preferences.debug("mbac alpha = " + alpha.X + "," + alpha.Y + "," + alpha.Z + "\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("mbac beta = " + beta.X + "," + beta.Y + "," + beta.Z + "\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("mbac gamma = " + gamma.X + "," + gamma.Y + "," + gamma.Z + "\n", Preferences.DEBUG_FILEIO);
            xfrm.setRotate(alpha, beta, gamma);


            /* Code to make cubic voxels required in acpc and Talairach space */
            /* Almost everyone uses the default 1 mm. */
            oXres = imgResols[0];
            oYres = imgResols[1];
            oZres = imgResols[2];

            oXdim = dicomXDim;
            oYdim = dicomYDim;
            oZdim = dicomZDim;

            Talx = Math.abs(dicomOrigin[0]);
            Taly = Math.abs(dicomOrigin[1]);
            Talz = Math.abs(dicomOrigin[2]);
            Preferences.debug("Talx = " + Talx + " Taly = " + Taly + " Talz = " + Talz + "\n", Preferences.DEBUG_FILEIO);

            
            Tr03 = (iXres * rr.X) - (Talx * xfrm.get(0, 0)) - (Taly * xfrm.get(0, 1)) - (Talz * xfrm.get(0, 2));
            Tr13 = (iYres * rr.Y) - (Talx * xfrm.get(1, 0)) - (Taly * xfrm.get(1, 1)) - (Talz * xfrm.get(1, 2));
            Tr23 = (iZres * rr.Z) - (Talx * xfrm.get(2, 0)) - (Taly * xfrm.get(2, 1)) - (Talz * xfrm.get(2, 2));

            /*
             * Tr03 = xfrm.Get(0, 0) * Tx + xfrm.Get(0, 1) * Ty + xfrm.Get(0, 2) * Tz + xfrm.Get(0, 3) Tr13 = xfrm.Get(1, 0) * Tx + xfrm.Get(1, 1) * Ty + xfrm.Get(1, 2)
             * Tz + xfrm.Get(1, 3) Tr23 = xfrm.Get(2, 2) * Tx + xfrm.Get(2, 1) * Ty + xfrm.Get(2, 2) * Tz + xfrm.Get(2, 3)
             */
            A = new JamaMatrix(3, 3, 0.0);
            A.set(0, 0, xfrm.get(0, 0));
            A.set(0, 1, xfrm.get(0, 1));
            A.set(0, 2, xfrm.get(0, 2));
            A.set(1, 0, xfrm.get(1, 0));
            A.set(1, 1, xfrm.get(1, 1));
            A.set(1, 2, xfrm.get(1, 2));
            A.set(2, 0, xfrm.get(2, 0));
            A.set(2, 1, xfrm.get(2, 1));
            A.set(2, 2, xfrm.get(2, 2));
            b = new JamaMatrix(3, 1, 0.0);
            b.set(0, 0, Tr03 - xfrm.get(0, 3));
            b.set(1, 0, Tr13 - xfrm.get(1, 3));
            b.set(2, 0, Tr23 - xfrm.get(2, 3));
            X = A.solve(b);
            Tx = X.get(0, 0);
            Ty = X.get(1, 0);
            Tz = X.get(2, 0);
            xfrm.setTranslate(Tx, Ty, Tz);
            
            transformACPCTrilinear(imgBuffer, xfrm, iXres, iYres, iZres, iXdim, iYdim, iZdim, oXres, oYres, oZres, oXdim,
                                   oYdim, oZdim);

            /* Find the Talairach center, the new origin, in the newly transformed coordinates */
            /* X = (i*oXres*T00 + j*oYres*T01 + k*oZres*T02 + T03)/iXres
             * Y = (i*oXres*T10 + j*oYres*T11 + k*oZres*T12 + T13)/iYres Z = (i*oXres*T20 + j*oYres*T21 + k*oZres*T22 +
             * T23)/iZres Wish to find i,j,k from X,Y,ZThat is, wish to find TCenter.X,TCenter.Y,TCenter.Z from
             * rr.X,rr.Y,rr.Z */
            
            translation.X = (float) xfrm.get(0, 3);
            translation.Y = (float) xfrm.get(1, 3);
            translation.Z = (float) xfrm.get(2, 3);

            A = new JamaMatrix(3, 3, 0.0);
            A.set(0, 0, oXres * xfrm.get(0, 0) / iXres);
            A.set(0, 1, oYres * xfrm.get(0, 1) / iXres);
            A.set(0, 2, oZres * xfrm.get(0, 2) / iXres);
            A.set(1, 0, oXres * xfrm.get(1, 0) / iYres);
            A.set(1, 1, oYres * xfrm.get(1, 1) / iYres);
            A.set(1, 2, oZres * xfrm.get(1, 2) / iYres);
            A.set(2, 0, oXres * xfrm.get(2, 0) / iZres);
            A.set(2, 1, oYres * xfrm.get(2, 1) / iZres);
            A.set(2, 2, oZres * xfrm.get(2, 2) / iZres);
            b = new JamaMatrix(3, 1, 0.0);

            /* b.set(0,0,rr.X - xfrm.Get(0, 3)/iXres);
             * b.set(1,0,rr.Y - xfrm.Get(1, 3)/iYres); b.set(2,0,rr.Z - xfrm.Get(2, 3)/iZres); X = A.solve(b); TCenter.X =
             * (float)(X.get(0,0)); TCenter.Y = (float)(X.get(1,0)); TCenter.Z = (float)(X.get(2,0)); */
            TCenter.X = Talx / oXres;
            TCenter.Y = Taly / oYres;
            TCenter.Z = Talz / oZres;
            Preferences.debug("Transformed Talairach origin = " + TCenter.X + "  " + TCenter.Y + "  " + TCenter.Z +
                              "\n", Preferences.DEBUG_FILEIO);

            // Find the new pc inferior edge in the new coordinates
            b.set(0, 0, pcDicom.X - (xfrm.get(0, 3) / iXres));
            b.set(1, 0, pcDicom.Y - (xfrm.get(1, 3) / iYres));
            b.set(2, 0, pcDicom.Z - (xfrm.get(2, 3) / iZres));
            X = A.solve(b);
            pcDicom.X = (float) (X.get(0, 0));
            pcDicom.Y = (float) (X.get(1, 0));
            pcDicom.Z = (float) (X.get(2, 0));

            // Store the posterior commissure for the +acpc to +tlrc conversion
            fileInfo.setpcie(pcDicom);
            tInfo.setAcpcPC(pcDicom);
            acpcPC = pcDicom;

            float[][] alignment = new float[3][3];
            float[] offset = new float[3];

            for (i = 0; i < 3; i++) {

                switch (orientSpecific[i]) {

                    case FileInfoBase.ORI_R2L_TYPE:
                        alignment[i][0] = 1.0f;
                        alignment[i][1] = 0.0f;
                        alignment[i][2] = 0.0f;
                        offset[i] = 0.0f;
                        break;

                    case FileInfoBase.ORI_L2R_TYPE:
                        alignment[i][0] = -1.0f;
                        alignment[i][1] = 0.0f;
                        alignment[i][2] = 0.0f;
                        offset[i] = tInfo.getOrigDim()[i] - 1.0f;
                        break;

                    case FileInfoBase.ORI_A2P_TYPE:
                        alignment[i][0] = 0.0f;
                        alignment[i][1] = 1.0f;
                        alignment[i][2] = 0.0f;
                        offset[i] = 0.0f;
                        break;

                    case FileInfoBase.ORI_P2A_TYPE:
                        alignment[i][0] = 0.0f;
                        alignment[i][1] = -1.0f;
                        alignment[i][2] = 0.0f;
                        offset[i] = tInfo.getOrigDim()[i] - 1.0f;
                        break;

                    case FileInfoBase.ORI_I2S_TYPE:
                        alignment[i][0] = 0.0f;
                        alignment[i][1] = 0.0f;
                        alignment[i][2] = 1.0f;
                        offset[i] = 0.0f;
                        break;

                    case FileInfoBase.ORI_S2I_TYPE:
                        alignment[i][0] = 0.0f;
                        alignment[i][1] = 0.0f;
                        alignment[i][2] = -1.0f;
                        offset[i] = tInfo.getOrigDim()[i] - 1.0f;
                        break;
                }
            }

            float[][] rotation = new float[3][3];
            rotation[0][0] = alpha.X;
            rotation[1][0] = alpha.Y;
            rotation[2][0] = alpha.Z;
            rotation[0][1] = beta.X;
            rotation[1][1] = beta.Y;
            rotation[2][1] = beta.Z;
            rotation[0][2] = gamma.X;
            rotation[1][2] = gamma.Y;
            rotation[2][2] = gamma.Z;

            // combine alignment with other quantities
            float[][] trans = new float[3][3];

            for (i = 0; i < 3; i++) {

                for (j = 0; j < 3; j++) {
                    trans[i][j] = 0.0f;

                    for (int k = 0; k < 3; k++) {
                        trans[i][j] += rotation[i][k] * alignment[j][k];
                    }
                }
            }

            float[] absDelta = new float[3];
            absDelta[0] = Math.abs(origDelta[0]);
            absDelta[1] = Math.abs(origDelta[1]);
            absDelta[2] = Math.abs(origDelta[2]);

            /* compute the new y direction (beta) */
            beta = sub(pcDicom, superiorEdgeDicom);
            beta = makemmVector3f(beta, absDelta);
            beta = norm(beta);

            /* Compute the new x direction (alpha) */
            rr = sub(firstPtDicom, superiorEdgeDicom);
            rr = makemmVector3f(rr, absDelta);
            alpha1 = crossProduct(beta, rr);
            alpha1 = norm(alpha1);

            rr = sub(anotherPtDicom, superiorEdgeDicom);
            rr = makemmVector3f(rr, absDelta);
            alpha2 = crossProduct(beta, rr);
            alpha2 = norm(alpha2);

            float size = dotProduct(alpha1, alpha2); // angle < 5 degrees

            /*if (size < 0.99619) {                     // size = cos(angle)
             *  MipavUtil.displayError("Error! The AC + PC + mid-sag points do not form a good plane.");}*/

            size = (float) (Math.acos((double) size) * 180.0 / Math.PI); // report angle
            Preferences.debug("Angular deviation between AC+PC+mid-sag pts: " + size + " degrees\n", Preferences.DEBUG_FILEIO);

            alpha = sclAdd(0.5f, alpha1, 0.5f, alpha2);
            alpha = norm(alpha);

            // compute the new z direction (gamma)
            gamma = crossProduct(alpha, beta);
            gamma = norm(gamma);

            /* Now consider the ray from the AC posterior margin (posteriorMarginDicom)
             * in the gamma direction, and the ray from the AC superior edge (superiorEdgeDicom) in the beta direction.
             * Nominally, these rays should intersect.  Find their points of closest approach (rr1,rr2).  The average of
             * these is the Talairach centerof coordinates (rr). */

            dif = sub(superiorEdgeDicom, posteriorMarginDicom);
            dif = makemmVector3f(dif, absDelta);
            size = dotProduct(dif, gamma);
            acpos = makemmVector3f(posteriorMarginDicom, absDelta);
            rr1 = sclAdd(1.0f, acpos, size, gamma);

            size = dotProduct(dif, beta);
            acsup = makemmVector3f(superiorEdgeDicom, absDelta);
            rr2 = sclAdd(1.0f, acsup, -size, beta);

            size = dist(rr1, rr2, absDelta);
            /*if (size > 2.0) {
             *  MipavUtil.displayError("Error! AC Talairach origin mismatch more than 2 mm");}*/

            rr = sclAdd(0.5f, rr1, 0.5f, rr2);
            rr = makeVoxelCoord3Df(rr, absDelta);

            origAC = new Vector3f(rr.X, rr.Y, rr.Z);

            origAC.X = (alignment[0][0] * rr.X) + (alignment[0][1] * rr.Y) + (alignment[0][2] * rr.Z) + offset[0];
            origAC.Y = (alignment[1][0] * rr.X) + (alignment[1][1] * rr.Y) + (alignment[1][2] * rr.Z) + offset[1];
            origAC.Z = (alignment[2][0] * rr.X) + (alignment[2][1] * rr.Y) + (alignment[2][2] * rr.Z) + offset[2];

            tInfo.setOrigOrient(trans);
            tInfo.setOrigAC(origAC);


            fileInfo.setImageOrientation(FileInfoBase.AXIAL);
            fileInfo.setMax(image.getMax());
            fileInfo.setMin(image.getMin());
            fileInfo.setTalairachCenter(TCenter);
            fileInfo.setAlpha(alpha);
            fileInfo.setBeta(beta);
            fileInfo.setGamma(gamma);
            fileInfo.setTranslation(translation);
            fileInfo.setrr(rr);
            fileInfo.setAFNIOrigExtents(AFNIOrigExtents);
            fileInfo.setAFNIOrigResolutions(AFNIOrigResolutions);
            fileInfo.setLowXmm(dicomLowestX);
            fileInfo.setLowYmm(dicomLowestY);
            fileInfo.setLowZmm(dicomLowestZ);
            fileInfo.setHighXmm(dicomHighestX);
            fileInfo.setHighYmm(dicomHighestY);
            fileInfo.setHighZmm(dicomHighestZ);
            fileInfo.setOrigin(origin);

            image.setFileInfo(fileInfo, 0);
            for (i = 1; i < dicomZDim; i++) {
                fileInfoCopy = (FileInfoAfni)fileInfo.clone();
                image.setFileInfo(fileInfoCopy, i);
            }


        } // if (readACPC)


        if (alsoAcpc) {
            iXres = origDelta[0];
            iYres = origDelta[1];
            iZres = origDelta[2];
            fileName = acpcName;
            presentViewType = FileInfoAfni.AFNI_ACPC;
            readHeader();

            // rr = -svec where rr is the Talairach center
            if (warpData != null) {
                rr.X = -warpData[21];
                rr.Y = -warpData[22];
                rr.Z = -warpData[23];
                rr.X = (rr.X - origOrigin[0]) / origDelta[0];

                if (invertX) {
                    rr.X = origXDim - 1 - rr.X;
                }

                rr.Y = (rr.Y - origOrigin[1]) / origDelta[1];

                if (invertY) {
                    rr.Y = origYDim - 1 - rr.Y;
                }

                rr.Z = (rr.Z - origOrigin[2]) / origDelta[2];

                if (invertZ) {
                    rr.Z = origZDim - 1 - rr.Z;
                }

                Preferences.debug("svec rr = " + rr.X + "," + rr.Y + "," + rr.Z + "\n", Preferences.DEBUG_FILEIO);

                xfrm = new TransMatrix(4);
                center = new Vector3f();
                center.X = (origXDim - 1) / 2f;
                center.Y = (origYDim - 1) / 2f;
                center.Z = (origZDim - 1) / 2f;

                xfrm.setTranslate(center.X, center.Y, center.Z);
                Preferences.debug("center.X = " + center.X + " center.Y = " + center.Y + " center.Z = " + center.Z +
                                  "\n", Preferences.DEBUG_FILEIO);

                // Since our interpolation routines are doing a output to input mapping, create the mbac
                // transformation matrix which is the transpose(also the inverse) of the mfor matrix.
                alpha.X = warpData[9];
                alpha.Y = warpData[10];
                alpha.Z = warpData[11];
                beta.X = warpData[12];
                beta.Y = warpData[13];
                beta.Z = warpData[14];
                gamma.X = warpData[15];
                gamma.Y = warpData[16];
                gamma.Z = warpData[17];
                Preferences.debug("mbac alpha = " + alpha.X + "," + alpha.Y + "," + alpha.Z + "\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("mbac beta = " + beta.X + "," + beta.Y + "," + beta.Z + "\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("mbac gamma = " + gamma.X + "," + gamma.Y + "," + gamma.Z + "\n", Preferences.DEBUG_FILEIO);
                xfrm.setRotate(alpha, beta, gamma);

                /* Code to make cubic voxels required in acpc and Talairach space */
                /* Almost everyone uses the default 1 mm. */
                oXres = acpcRes;
                oYres = acpcRes;
                oZres = acpcRes;

                Talx = Math.abs(origin[0]);
                Taly = Math.abs(origin[1]);
                Talz = Math.abs(origin[2]);
                Preferences.debug("Talx = " + Talx + " Taly = " + Taly + " Talz = " + Talz + "\n", Preferences.DEBUG_FILEIO);

                
                Tr03 = (iXres * rr.X) - (Talx * xfrm.get(0, 0)) - (Taly * xfrm.get(0, 1)) - (Talz * xfrm.get(0, 2));
                Tr13 = (iYres * rr.Y) - (Talx * xfrm.get(1, 0)) - (Taly * xfrm.get(1, 1)) - (Talz * xfrm.get(1, 2));
                Tr23 = (iZres * rr.Z) - (Talx * xfrm.get(2, 0)) - (Taly * xfrm.get(2, 1)) - (Talz * xfrm.get(2, 2));

                /*
                 *       Tr03 = xfrm.Get(0, 0) * Tx + xfrm.Get(0, 1) * Ty + xfrm.Get(0, 2) * Tz + xfrm.Get(0, 3)      Tr13 = xfrm.Get(1, 0) * Tx + xfrm.Get(1, 1)
                 * Ty + xfrm.Get(1, 2) * Tz + xfrm.Get(1, 3)      Tr23 = xfrm.Get(2, 2) * Tx + xfrm.Get(2, 1) * Ty + xfrm.Get(2, 2) * Tz + xfrm.Get(2, 3)
                 */
                A = new JamaMatrix(3, 3, 0.0);
                A.set(0, 0, xfrm.get(0, 0));
                A.set(0, 1, xfrm.get(0, 1));
                A.set(0, 2, xfrm.get(0, 2));
                A.set(1, 0, xfrm.get(1, 0));
                A.set(1, 1, xfrm.get(1, 1));
                A.set(1, 2, xfrm.get(1, 2));
                A.set(2, 0, xfrm.get(2, 0));
                A.set(2, 1, xfrm.get(2, 1));
                A.set(2, 2, xfrm.get(2, 2));
                b = new JamaMatrix(3, 1, 0.0);

                /* b.set(0,0,rr.X - xfrm.Get(0, 3)/iXres);
                 *       b.set(1,0,rr.Y - xfrm.Get(1, 3)/iYres);      b.set(2,0,rr.Z - xfrm.Get(2, 3)/iZres);      X = A.solve(b);
                 * TCenter.X = (float)(X.get(0,0));      TCenter.Y = (float)(X.get(1,0));      TCenter.Z =
                 * (float)(X.get(2,0)); */
                TCenter.X = Talx / oXres;
                TCenter.Y = Taly / oYres;
                TCenter.Z = Talz / oZres;
                Preferences.debug("Transformed Talairach origin = " + TCenter.X + "  " + TCenter.Y + "  " + TCenter.Z +
                                  "\n", Preferences.DEBUG_FILEIO);

                b.set(0, 0, Tr03 - xfrm.get(0, 3));
                b.set(1, 0, Tr13 - xfrm.get(1, 3));
                b.set(2, 0, Tr23 - xfrm.get(2, 3));
                X = A.solve(b);
                Tx = X.get(0, 0);
                Ty = X.get(1, 0);
                Tz = X.get(2, 0);
                xfrm.setTranslate(Tx, Ty, Tz);

                /* Find the Talairach center, the new origin, in the newly transformed coordinates */
                /* X = (i*oXres*T00 + j*oYres*T01 + k*oZres*T02 + T03)/iXres
                 * Y = (i*oXres*T10 + j*oYres*T11 + k*oZres*T12 + T13)/iYres Z = (i*oXres*T20 + j*oYres*T21 +
                 * k*oZres*T22 + T23)/iZres Wish to find i,j,k from X,Y,Z That is, wish to find
                 * TCenter.X,TCenter.Y,TCenter.Z from rr.X,rr.Y,rr.Z */
                

                A = new JamaMatrix(3, 3, 0.0);
                A.set(0, 0, oXres * xfrm.get(0, 0) / iXres);
                A.set(0, 1, oYres * xfrm.get(0, 1) / iXres);
                A.set(0, 2, oZres * xfrm.get(0, 2) / iXres);
                A.set(1, 0, oXres * xfrm.get(1, 0) / iYres);
                A.set(1, 1, oYres * xfrm.get(1, 1) / iYres);
                A.set(1, 2, oZres * xfrm.get(1, 2) / iYres);
                A.set(2, 0, oXres * xfrm.get(2, 0) / iZres);
                A.set(2, 1, oYres * xfrm.get(2, 1) / iZres);
                A.set(2, 2, oZres * xfrm.get(2, 2) / iZres);
                b = new JamaMatrix(3, 1, 0.0);

                // Find the new pc inferior edge in the new coordinates
                b.set(0, 0, pcDicom.X - (xfrm.get(0, 3) / iXres));
                b.set(1, 0, pcDicom.Y - (xfrm.get(1, 3) / iYres));
                b.set(2, 0, pcDicom.Z - (xfrm.get(2, 3) / iZres));
                X = A.solve(b);
                pcDicom.X = (float) (X.get(0, 0));
                pcDicom.Y = (float) (X.get(1, 0));
                pcDicom.Z = (float) (X.get(2, 0));

                // Store the posterior commissure for the +acpc to +tlrc conversion
                fileInfo.setpcie(pcDicom);
                tInfo.setAcpcPC(pcDicom);
                acpcPC = pcDicom;

                float[][] alignment = new float[3][3];
                float[] offset = new float[3];

                for (i = 0; i < 3; i++) {

                    if (doDicom) {

                        switch (orientSpecific[i]) {

                            case FileInfoBase.ORI_R2L_TYPE:
                                alignment[i][0] = 1.0f;
                                alignment[i][1] = 0.0f;
                                alignment[i][2] = 0.0f;
                                offset[i] = 0.0f;
                                break;

                            case FileInfoBase.ORI_L2R_TYPE:
                                alignment[i][0] = -1.0f;
                                alignment[i][1] = 0.0f;
                                alignment[i][2] = 0.0f;
                                offset[i] = origExtents[i] - 1.0f;
                                break;

                            case FileInfoBase.ORI_A2P_TYPE:
                                alignment[i][0] = 0.0f;
                                alignment[i][1] = 1.0f;
                                alignment[i][2] = 0.0f;
                                offset[i] = 0.0f;
                                break;

                            case FileInfoBase.ORI_P2A_TYPE:
                                alignment[i][0] = 0.0f;
                                alignment[i][1] = -1.0f;
                                alignment[i][2] = 0.0f;
                                offset[i] = origExtents[i] - 1.0f;
                                break;

                            case FileInfoBase.ORI_I2S_TYPE:
                                alignment[i][0] = 0.0f;
                                alignment[i][1] = 0.0f;
                                alignment[i][2] = 1.0f;
                                offset[i] = 0.0f;
                                break;

                            case FileInfoBase.ORI_S2I_TYPE:
                                alignment[i][0] = 0.0f;
                                alignment[i][1] = 0.0f;
                                alignment[i][2] = -1.0f;
                                offset[i] = origExtents[i] - 1.0f;
                                break;
                        }
                    } else { // if (doDicom),  !doDicom

                        switch (dataOrient[i]) {

                            case FileInfoBase.ORI_R2L_TYPE:
                                alignment[i][0] = 1.0f;
                                alignment[i][1] = 0.0f;
                                alignment[i][2] = 0.0f;
                                offset[i] = 0.0f;
                                break;

                            case FileInfoBase.ORI_L2R_TYPE:
                                alignment[i][0] = -1.0f;
                                alignment[i][1] = 0.0f;
                                alignment[i][2] = 0.0f;
                                offset[i] = dataExtents[i] - 1.0f;
                                break;

                            case FileInfoBase.ORI_A2P_TYPE:
                                alignment[i][0] = 0.0f;
                                alignment[i][1] = 1.0f;
                                alignment[i][2] = 0.0f;
                                offset[i] = 0.0f;
                                break;

                            case FileInfoBase.ORI_P2A_TYPE:
                                alignment[i][0] = 0.0f;
                                alignment[i][1] = -1.0f;
                                alignment[i][2] = 0.0f;
                                offset[i] = dataExtents[i] - 1.0f;
                                break;

                            case FileInfoBase.ORI_I2S_TYPE:
                                alignment[i][0] = 0.0f;
                                alignment[i][1] = 0.0f;
                                alignment[i][2] = 1.0f;
                                offset[i] = 0.0f;
                                break;

                            case FileInfoBase.ORI_S2I_TYPE:
                                alignment[i][0] = 0.0f;
                                alignment[i][1] = 0.0f;
                                alignment[i][2] = -1.0f;
                                offset[i] = dataExtents[i] - 1.0f;
                                break;
                        }
                    } // else !doDicom
                }

                float[][] rotation = new float[3][3];
                rotation[0][0] = alpha.X;
                rotation[1][0] = alpha.Y;
                rotation[2][0] = alpha.Z;
                rotation[0][1] = beta.X;
                rotation[1][1] = beta.Y;
                rotation[2][1] = beta.Z;
                rotation[0][2] = gamma.X;
                rotation[1][2] = gamma.Y;
                rotation[2][2] = gamma.Z;

                // combine alignment with other quantities
                float[][] trans = new float[3][3];

                for (i = 0; i < 3; i++) {

                    for (j = 0; j < 3; j++) {
                        trans[i][j] = 0.0f;

                        for (int k = 0; k < 3; k++) {
                            trans[i][j] += rotation[i][k] * alignment[j][k];
                        }
                    }
                }

                float[] absDelta = new float[3];
                absDelta[0] = Math.abs(origDelta[0]);
                absDelta[1] = Math.abs(origDelta[1]);
                absDelta[2] = Math.abs(origDelta[2]);

                /* compute the new y direction (beta) */
                beta = sub(pcDicom, superiorEdgeDicom);
                beta = makemmVector3f(beta, absDelta);
                beta = norm(beta);

                /* Compute the new x direction (alpha) */
                rr = sub(firstPtDicom, superiorEdgeDicom);
                rr = makemmVector3f(rr, absDelta);
                alpha1 = crossProduct(beta, rr);
                alpha1 = norm(alpha1);

                rr = sub(anotherPtDicom, superiorEdgeDicom);
                rr = makemmVector3f(rr, absDelta);
                alpha2 = crossProduct(beta, rr);
                alpha2 = norm(alpha2);

                float size = dotProduct(alpha1, alpha2); // angle < 5 degrees

                /*if (size < 0.99619) {                     // size = cos(angle)
                 *  MipavUtil.displayError("Error! The AC + PC + mid-sag points do not form a good plane.");      }*/

                size = (float) (Math.acos((double) size) * 180.0 / Math.PI); // report angle
                Preferences.debug("Angular deviation between AC+PC+mid-sag pts: " + size + " degrees\n",
                		Preferences.DEBUG_FILEIO);

                alpha = sclAdd(0.5f, alpha1, 0.5f, alpha2);
                alpha = norm(alpha);

                // compute the new z direction (gamma)
                gamma = crossProduct(alpha, beta);
                gamma = norm(gamma);

                /* Now consider the ray from the AC posterior margin (posteriorMarginDicom)
                 * in the gamma direction, and the ray from the AC superior edge (superiorEdgeDicom) in the beta
                 * direction.  Nominally, these rays should intersect.  Find their points of closest approach (rr1,rr2).
                 *  The average of these is the Talairach centerof coordinates (rr). */

                dif = sub(superiorEdgeDicom, posteriorMarginDicom);
                dif = makemmVector3f(dif, absDelta);
                size = dotProduct(dif, gamma);
                acpos = makemmVector3f(posteriorMarginDicom, absDelta);
                rr1 = sclAdd(1.0f, acpos, size, gamma);

                size = dotProduct(dif, beta);
                acsup = makemmVector3f(superiorEdgeDicom, absDelta);
                rr2 = sclAdd(1.0f, acsup, -size, beta);

                size = dist(rr1, rr2, absDelta);
                /*if (size > 2.0) {
                 * MipavUtil.displayError("Error! AC Talairach origin mismatch more than 2 mm");      }*/

                rr = sclAdd(0.5f, rr1, 0.5f, rr2);
                rr = makeVoxelCoord3Df(rr, absDelta);
                origAC = new Vector3f(rr.X, rr.Y, rr.Z);

                origAC.X = (alignment[0][0] * rr.X) + (alignment[0][1] * rr.Y) + (alignment[0][2] * rr.Z) + offset[0];
                origAC.Y = (alignment[1][0] * rr.X) + (alignment[1][1] * rr.Y) + (alignment[1][2] * rr.Z) + offset[1];
                origAC.Z = (alignment[2][0] * rr.X) + (alignment[2][1] * rr.Y) + (alignment[2][2] * rr.Z) + offset[2];

                tInfo.setOrigOrient(trans);
                tInfo.setOrigAC(origAC);
            } // if (warpData != null)
        } // if (alsoAcpc)

        if (readTLRC) {
            Vector3f[] alphaArray = {
                new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f),
                new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f),
                new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f),
                new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f)
            };
            Vector3f[] betaArray = {
                new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f),
                new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f),
                new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f),
                new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f)
            };
            Vector3f[] gammaArray = {
                new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f),
                new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f),
                new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f),
                new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f)
            };
            Vector3f[] rrArray = {
                new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f),
                new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f),
                new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f),
                new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f), new Vector3f(0.0f, 0.0f, 0.0f)
            };
            int[] botX = new int[12];
            int[] botY = new int[12];
            int[] botZ = new int[12];
            int[] topX = new int[12];
            int[] topY = new int[12];
            int[] topZ = new int[12];
            iXdim = dicomXDim;
            iYdim = dicomYDim;
            iZdim = dicomZDim;
            AFNIOrigExtents[0] = dicomXDim;
            AFNIOrigExtents[1] = dicomYDim;
            AFNIOrigExtents[2] = dicomZDim;
            iXres = dicomDelta[0];
            iYres = dicomDelta[1];
            iZres = dicomDelta[2];
            AFNIOrigResolutions[0] = dicomDelta[0];
            AFNIOrigResolutions[1] = dicomDelta[1];
            AFNIOrigResolutions[2] = dicomDelta[2];

            fileName = originalFileName;
            presentViewType = viewType;
            readHeader();
            xfrm = new TransMatrix(4);
            center = new Vector3f();
            center.X = (image.getExtents()[0] - 1) / 2.0f;
            center.Y = (image.getExtents()[1] - 1) / 2.0f;
            center.Z = (image.getExtents()[2] - 1) / 2.0f;

            bufferSize = iXdim * iYdim * iZdim;
            imgBuffer = new float[bufferSize];

            try {
                image.exportData(0, bufferSize, imgBuffer);
            } catch (IOException error) {
                MipavUtil.displayError("FileAfni: IOException error on exportData");
            }

            int type = image.getType();
            image.disposeLocal();
            image = null;

            image = new ModelImage(type, imgExtents, originalFileName);
            fileInfo.setImageOrientation(FileInfoBase.AXIAL);

            image.setFileInfo(fileInfo, 0);
            for (i = 1; i < dicomZDim; i++) {
                fileInfoCopy = (FileInfoAfni)fileInfo.clone();
                image.setFileInfo(fileInfoCopy, i);
            }

            // Note that the 12 BLTs only have 2 distinct sets of svec.  If botY  <
            // ATLAS_AC_TO_PC, then the svec values are the same as in the .acpc
            // header file.  Then svec is -rr, the Talairach center.
            // If botY == ATLAS_AC_TO_PC, then the svec values are
            // a second separate distinct set.  Then svec is still -rr, the Talairach Center.
            // In the Talairach image pcie.Y = TCenter.Y + ATLAS_AC_TO_PC
            // Use the transformation matrix to determine TCenter in the ACPC image from a
            // posterior section in the Talairach section.  Call this TCenter calculated back
            // from the Talairach posterior BTCenter.
            // BTCenter.X = TCenter.X and BTCenter.Z = TCenter.Z
            // However, BTCenter.Y = pcie.Y -(ATLAS_AC_TO_PC)/scale_P
            // This follows from the fact that in the Talairach image:
            // TCenter.Y = pcie.Y - (ATLAS_AC_TO_PC)


            /* Code to make cubic voxels required in acpc and Talairach space */
            /* Almost everyone uses the default 1 mm. */
            oXres = imgResols[0];
            oYres = imgResols[1];
            oZres = imgResols[2];

            oXdim = dicomXDim;
            ;
            oYdim = dicomYDim;
            oZdim = dicomZDim;

            Talx = Math.abs(dicomOrigin[0]);
            Taly = Math.abs(dicomOrigin[1]);
            Talz = Math.abs(dicomOrigin[2]);

            fireProgressStateChanged(0);

            for (i = 0; i < 12; i++) {
                j = i + 1;
                fireProgressStateChanged("Transformation pass #" + j);
                rrArray[i].X = -warpData[(i * 30) + 21];
                rrArray[i].Y = -warpData[(i * 30) + 22];
                rrArray[i].Z = -warpData[(i * 30) + 23];
                rrArray[i].X = (rrArray[i].X - origOrigin[0]) / origDelta[0];

                if (invertX) {
                    rrArray[i].X = origXDim - 1 - rrArray[i].X;
                }

                rrArray[i].Y = (rrArray[i].Y - origOrigin[1]) / origDelta[1];

                if (invertY) {
                    rrArray[i].Y = origYDim - 1 - rrArray[i].Y;
                }

                rrArray[i].Z = (rrArray[i].Z - origOrigin[2]) / origDelta[2];

                if (invertZ) {
                    rrArray[i].Z = origZDim - 1 - rrArray[i].Z;
                }

                Preferences.debug("svec rr = " + rrArray[i].X + "," + rrArray[i].Y + "," + rrArray[i].Z + "\n",
                		Preferences.DEBUG_FILEIO);
                xfrm.identity();
                xfrm.setTranslate(center.X, center.Y, center.Z);

                // Since our interpolation routines are doing a output to input mapping, create the mbac
                // transformation matrix which is the transpose(also the inverse) of the mfor matrix.
                alphaArray[i].X = warpData[(i * 30) + 9];
                alphaArray[i].Y = warpData[(i * 30) + 10];
                alphaArray[i].Z = warpData[(i * 30) + 11];
                betaArray[i].X = warpData[(i * 30) + 12];
                betaArray[i].Y = warpData[(i * 30) + 13];
                betaArray[i].Z = warpData[(i * 30) + 14];
                gammaArray[i].X = warpData[(i * 30) + 15];
                gammaArray[i].Y = warpData[(i * 30) + 16];
                gammaArray[i].Z = warpData[(i * 30) + 17];
                xfrm.setRotate(alphaArray[i], betaArray[i], gammaArray[i]);

                // Bounding values in the output
                botX[i] = Math.max(0, Math.round((warpData[(i * 30) + 24] - origin[0]) / delta[0]));
                botY[i] = Math.max(0, Math.round((warpData[(i * 30) + 25] - origin[1]) / delta[1]));
                botZ[i] = Math.max(0, Math.round((warpData[(i * 30) + 26] - origin[2]) / delta[2]));
                topX[i] = Math.min(dicomXDim - 1, Math.round((warpData[(i * 30) + 27] - origin[0]) / delta[0]));
                topY[i] = Math.min(dicomYDim - 1, Math.round((warpData[(i * 30) + 28] - origin[1]) / delta[1]));
                topZ[i] = Math.min(dicomZDim - 1, Math.round((warpData[(i * 30) + 29] - origin[2]) / delta[2]));
                Preferences.debug("botX = " + botX[i] + " botY = " + botY[i] + " botZ = " + botZ[i] + "\n",
                		Preferences.DEBUG_FILEIO);
                Preferences.debug("topX = " + topX[i] + " topY = " + topY[i] + " topZ = " + topZ[i] + "\n",
                		Preferences.DEBUG_FILEIO);
                
                Tr03 = (iXres * rrArray[i].X) - (Talx * xfrm.get(0, 0)) - (Taly * xfrm.get(0, 1)) - (Talz * xfrm.get(0, 2));
                Tr13 = (iYres * rrArray[i].Y) - (Talx * xfrm.get(1, 0)) - (Taly * xfrm.get(1, 1)) - (Talz * xfrm.get(1, 2));
                Tr23 = (iZres * rrArray[i].Z) - (Talx * xfrm.get(2, 0)) - (Taly * xfrm.get(2, 1)) - (Talz * xfrm.get(2, 2));

                /*
                 * Tr03 = xfrm.Get(0, 0) * Tx + xfrm.Get(0, 1) * Ty + xfrm.Get(0, 2) * Tz + xfrm.Get(0, 3) Tr13 = xfrm.Get(1, 0) * Tx + xfrm.Get(1, 1) * Ty +
                 * xfrm.Get(1, 2) * Tz + xfrm.Get(1, 3) Tr23 = xfrm.Get(2, 2) * Tx + xfrm.Get(2, 1) * Ty + xfrm.Get(2, 2) * Tz + xfrm.Get(2, 3)
                 */
                A = new JamaMatrix(3, 3, 0.0);
                A.set(0, 0, xfrm.get(0, 0));
                A.set(0, 1, xfrm.get(0, 1));
                A.set(0, 2, xfrm.get(0, 2));
                A.set(1, 0, xfrm.get(1, 0));
                A.set(1, 1, xfrm.get(1, 1));
                A.set(1, 2, xfrm.get(1, 2));
                A.set(2, 0, xfrm.get(2, 0));
                A.set(2, 1, xfrm.get(2, 1));
                A.set(2, 2, xfrm.get(2, 2));
                b = new JamaMatrix(3, 1, 0.0);
                b.set(0, 0, Tr03 - xfrm.get(0, 3));
                b.set(1, 0, Tr13 - xfrm.get(1, 3));
                b.set(2, 0, Tr23 - xfrm.get(2, 3));
                X = A.solve(b);
                Tx = X.get(0, 0);
                Ty = X.get(1, 0);
                Tz = X.get(2, 0);
                xfrm.setTranslate(Tx, Ty, Tz);
                
                transformTalairachTrilinear(imgBuffer, xfrm, iXres, iYres, iZres, iXdim, iYdim, iZdim, oXres, oYres,
                                            oZres, oXdim, botX[i], botY[i], botZ[i], topX[i], topY[i], topZ[i]);
                
                A = new JamaMatrix(3, 3, 0.0);
                A.set(0, 0, oXres * xfrm.get(0, 0) / iXres);
                A.set(0, 1, oYres * xfrm.get(0, 1) / iXres);
                A.set(0, 2, oZres * xfrm.get(0, 2) / iXres);
                A.set(1, 0, oXres * xfrm.get(1, 0) / iYres);
                A.set(1, 1, oYres * xfrm.get(1, 1) / iYres);
                A.set(1, 2, oZres * xfrm.get(1, 2) / iYres);
                A.set(2, 0, oXres * xfrm.get(2, 0) / iZres);
                A.set(2, 1, oYres * xfrm.get(2, 1) / iZres);
                A.set(2, 2, oZres * xfrm.get(2, 2) / iZres);
                b = new JamaMatrix(3, 1, 0.0);

                /*b.set(0,0,rrArray[i].X - xfrm.Get(0, 3)/iXres);
                 * b.set(1,0,rrArray[i].Y - xfrm.Get(1, 3)/iYres); b.set(2,0,rrArray[i].Z - xfrm.Get(2, 3)/iZres);
                 *
                 * X = A.solve(b); TalCenter.X = (float)(X.get(0,0)); TalCenter.Y = (float)(X.get(1,0));TalCenter.Z =
                 * (float)(X.get(2,0)); */
                TalCenter.X = Talx / oXres;
                TalCenter.Y = Taly / oYres;
                TalCenter.Z = Talz / oZres;
                Preferences.debug("Transformed Talairach origin = " + TalCenter.X + "  " + TalCenter.Y + "  " +
                                  TalCenter.Z + "\n", Preferences.DEBUG_FILEIO);
            } // for (i = 0; i < 12; i++)

            image.calcMinMax();
            fileInfo.setMax(image.getMax());
            fileInfo.setMin(image.getMin());
            fileInfo.setAFNIOrigExtents(AFNIOrigExtents);
            fileInfo.setAFNIOrigResolutions(AFNIOrigResolutions);
            fileInfo.setAlphaArray(alphaArray);
            fileInfo.setBetaArray(betaArray);
            fileInfo.setGammaArray(gammaArray);
            fileInfo.setrrArray(rrArray);
            fileInfo.setBotX(botX);
            fileInfo.setBotY(botY);
            fileInfo.setBotZ(botZ);
            fileInfo.setTopX(topX);
            fileInfo.setTopY(topY);
            fileInfo.setTopZ(topZ);
            fileInfo.setLowXmm(dicomLowestX);
            fileInfo.setLowYmm(dicomLowestY);
            fileInfo.setLowZmm(dicomLowestZ);
            fileInfo.setHighXmm(dicomHighestX);
            fileInfo.setHighYmm(dicomHighestY);
            fileInfo.setHighZmm(dicomHighestZ);
            fileInfo.setOrigin(origin);

            image.setFileInfo(fileInfo, 0);
            for (i = 1; i < dicomZDim; i++) {
                fileInfoCopy = (FileInfoAfni)fileInfo.clone();
                image.setFileInfo(fileInfoCopy, i);
            }


        } // if (readTLRC)

        if (haveReadOrig && haveReadACPC && (acpcPC != null)) {
            dist_ant = (acpcAC.Y - acpcMinPt.Y) * acpcRes;
            dist_med = (acpcPC.Y - acpcAC.Y) * acpcRes;
            dist_pos = (acpcMaxPt.Y - acpcPC.Y) * acpcRes;

            dist_sup = (acpcMaxPt.Z - acpcAC.Z) * acpcRes;
            dist_inf = (acpcAC.Z - acpcMinPt.Z) * acpcRes;

            dist_lef = (acpcMaxPt.X - acpcAC.X) * acpcRes;
            dist_rig = (acpcAC.X - acpcMinPt.X) * acpcRes;

            scale_A = ATLAS_FRONT_TO_AC / dist_ant;
            scale_M = ViewJFrameTriImage.ATLAS_AC_TO_PC / dist_med;
            scale_P = ATLAS_PC_TO_BACK / dist_pos;
            scale_S = ATLAS_AC_TO_TOP / dist_sup;
            scale_I = ATLAS_BOT_TO_AC / dist_inf;
            scale_L = ATLAS_AC_TO_LAT / dist_lef;
            scale_R = ATLAS_AC_TO_LAT / dist_rig;

            tlrcRes = new float[7];
            tlrcRes[0] = acpcRes / scale_R;
            tlrcRes[1] = acpcRes / scale_L;
            tlrcRes[2] = acpcRes / scale_A;
            tlrcRes[3] = acpcRes / scale_M;
            tlrcRes[4] = acpcRes / scale_P;
            tlrcRes[5] = acpcRes / scale_I;
            tlrcRes[6] = acpcRes / scale_S;
            tInfo.setTlrcRes(tlrcRes);

            image.setTalairachTransformInfo(tInfo);
        }

        return image;
    }

    /**
     * setFileName - accessor to set the file name (used when reading TIFF multiFile).
     *
     * @param  fName  file name of image to read.
     */
    public void setFileName(String fName) {
        fileName = fName;
    }

    public ModelLUT getModelLUT() {
		if(fileInfo != null) {
			return fileInfo.getLUT();
		}
		return null;
	}

	/**
     * Writes a AFNI format type image.
     *
     * @param      image    Image model of data to write.
     * @param      options  options such as starting and ending slices and times
     *
     * @exception  IOException  if there is an error writing the file
     */
    public void writeImage(ModelImage image, FileWriteOptions options) throws IOException {
        String fileHeaderName;
        String fileDataName;
        int[] orient;
        float[] resols;
        int[] units;
        float[] origin = new float[3];
        int brickType;
        String lineString;
        String lineString1;
        byte[] line;
        int i, j;
        int lastPeriod;
        int t, z;
        int sliceSize;
        byte[] byteBuffer;
        byte[] byteBuffer2;
        short[] shortBuffer;
        int[] intBuffer;
        float[] floatBuffer;
        double[] doubleBuffer;
        int numberSlices;
        int count;
        int zBegin, zEnd;
        int tBegin, tEnd;
        int tmpInt;
        long tmpLong;
        FileInfoAfni fileInfoAfni = null;
        Vector3f superiorEdge;
        float superiorEdgeX = -999999;
        float superiorEdgeY = -999999;
        float superiorEdgeZ = -999999;
        Vector3f posteriorMargin;
        float posteriorMarginX = -999999;
        float posteriorMarginY = -999999;
        float posteriorMarginZ = -999999;
        Vector3f inferiorEdge;
        float inferiorEdgeX = -999999;
        float inferiorEdgeY = -999999;
        float inferiorEdgeZ = -999999;
        Vector3f firstPt;
        float firstPtX = -999999;
        float firstPtY = -999999;
        float firstPtZ = -999999;
        Vector3f anotherPt;
        float anotherPtX = -999999;
        float anotherPtY = -999999;
        float anotherPtZ = -999999;

        // AFNIViewType: 0 = +orig, 1 = +acpc; 2 = +tlrc
        int AFNIViewType = 0;
        DecimalFormat nf;
        int nSpaces;

        // 0 for func type = ANAT_SPGR_TYPE
        int funcType = 0;

        // 2 for AFNITypeString = 3DIM_GEN_ANAT
        int AFNITypeString = 2;
        Vector3f anteriorPt;
        float anteriorPtX = -999999;
        float anteriorPtY = -999999;
        float anteriorPtZ = -999999;
        Vector3f posteriorPt;
        float posteriorPtX = -999999;
        float posteriorPtY = -999999;
        float posteriorPtZ = -999999;
        Vector3f superiorPt;
        float superiorPtX = -999999;
        float superiorPtY = -999999;
        float superiorPtZ = -999999;
        Vector3f inferiorPt;
        float inferiorPtX = -999999;
        float inferiorPtY = -999999;
        float inferiorPtZ = -999999;
        Vector3f leftPt;
        float leftPtX = -999999;
        float leftPtY = -999999;
        float leftPtZ = -999999;
        Vector3f rightPt;
        float rightPtX = -999999;
        float rightPtY = -999999;
        float rightPtZ = -999999;
        int slicesWithTimeOffsets = 0;
        float timeStep = 0.0f;
        float[] tAxisOffsets = null;

        lastPeriod = fileName.lastIndexOf(".");
        fileHeaderName = fileName.substring(0, lastPeriod + 1) + "HEAD";
        fileDataName = fileName.substring(0, lastPeriod + 1) + "BRIK";


        zBegin = options.getBeginSlice();
        zEnd = options.getEndSlice();

        if (image.getNDims() == 4) {
            tBegin = options.getBeginTime();
            tEnd = options.getEndTime();
        } else {
            tBegin = 0;
            tEnd = 0;
        }

        file = new File(fileDir + fileHeaderName);
        raFile = new RandomAccessFile(file, "rw");

        // Necessary so that if this is an overwritten file there isn't any
        // junk at the end
        raFile.setLength(0);

        // 14 characters long = 8 + 1 + 5
        nf = new DecimalFormat("#######0.#####");

        if (image.getFileInfo()[0] instanceof FileInfoAfni) {
            fileInfoAfni = ((FileInfoAfni) image.getFileInfo()[0]);
            AFNIViewType = fileInfoAfni.getAFNIViewType();
            marksFlag = fileInfoAfni.getMarksFlag();
            marksFlags = fileInfoAfni.getMarksFlags();
            funcType = fileInfoAfni.getFuncType();
            slicesWithTimeOffsets = fileInfoAfni.getSlicesWithTimeOffsets();

            if (slicesWithTimeOffsets != image.getExtents()[2]) {
                slicesWithTimeOffsets = 0;
            }

            timeStep = fileInfoAfni.getTimeStep();
            tAxisOffsets = fileInfoAfni.getTAxisOffsets();
            AFNITypeString = fileInfoAfni.getAFNITypeString();

            if (AFNIViewType == 0) {

                // +orig
                superiorEdge = fileInfoAfni.getSuperiorEdge();
                superiorEdgeX = superiorEdge.X;

                if (Float.isInfinite(superiorEdgeX) || Float.isNaN(superiorEdgeX)) {
                    superiorEdgeX = -999999;
                }

                superiorEdgeY = superiorEdge.Y;

                if (Float.isInfinite(superiorEdgeY) || Float.isNaN(superiorEdgeY)) {
                    superiorEdgeY = -999999;
                }

                superiorEdgeZ = superiorEdge.Z;

                if (Float.isInfinite(superiorEdgeZ) || Float.isNaN(superiorEdgeZ)) {
                    superiorEdgeZ = -999999;
                }

                posteriorMargin = fileInfoAfni.getPosteriorMargin();
                posteriorMarginX = posteriorMargin.X;

                if (Float.isInfinite(posteriorMarginX) || Float.isNaN(posteriorMarginX)) {
                    posteriorMarginX = -999999;
                }

                posteriorMarginY = posteriorMargin.Y;

                if (Float.isInfinite(posteriorMarginY) || Float.isNaN(posteriorMarginY)) {
                    posteriorMarginY = -999999;
                }

                posteriorMarginZ = posteriorMargin.Z;

                if (Float.isInfinite(posteriorMarginZ) || Float.isNaN(posteriorMarginZ)) {
                    posteriorMarginZ = -999999;
                }

                inferiorEdge = fileInfoAfni.getInferiorEdge();
                inferiorEdgeX = inferiorEdge.X;

                if (Float.isInfinite(inferiorEdgeX) || Float.isNaN(inferiorEdgeX)) {
                    inferiorEdgeX = -999999;
                }

                inferiorEdgeY = inferiorEdge.Y;

                if (Float.isInfinite(inferiorEdgeY) || Float.isNaN(inferiorEdgeY)) {
                    inferiorEdgeY = -999999;
                }

                inferiorEdgeZ = inferiorEdge.Z;

                if (Float.isInfinite(inferiorEdgeZ) || Float.isNaN(inferiorEdgeZ)) {
                    inferiorEdgeZ = -999999;
                }

                firstPt = fileInfoAfni.getFirstPt();
                firstPtX = firstPt.X;

                if (Float.isInfinite(firstPtX) || Float.isNaN(firstPtX)) {
                    firstPtX = -999999;
                }

                firstPtY = firstPt.Y;

                if (Float.isInfinite(firstPtY) || Float.isNaN(firstPtY)) {
                    firstPtY = -999999;
                }

                firstPtZ = firstPt.Z;

                if (Float.isInfinite(firstPtZ) || Float.isNaN(firstPtZ)) {
                    firstPtZ = -999999;
                }

                anotherPt = fileInfoAfni.getAnotherPt();
                anotherPtX = anotherPt.X;

                if (Float.isInfinite(anotherPtX) || Float.isNaN(anotherPtX)) {
                    anotherPtX = -999999;
                }

                anotherPtY = anotherPt.Y;

                if (Float.isInfinite(anotherPtY) || Float.isNaN(anotherPtY)) {
                    anotherPtY = -999999;
                }

                anotherPtZ = anotherPt.Z;

                if (Float.isInfinite(anotherPtZ) || Float.isNaN(anotherPtZ)) {
                    anotherPtZ = -999999;
                }
            } // if (AFNIViewType == 0)
            else if (AFNIViewType == 1) {

                // 1 = +acpc
                anteriorPt = fileInfoAfni.getAnteriorPt();
                anteriorPtX = anteriorPt.X;

                if (Float.isInfinite(anteriorPtX) || Float.isNaN(anteriorPtX)) {
                    anteriorPtX = -999999;
                }

                anteriorPtY = anteriorPt.Y;

                if (Float.isInfinite(anteriorPtY) || Float.isNaN(anteriorPtY)) {
                    anteriorPtY = -999999;
                }

                anteriorPtZ = anteriorPt.Z;

                if (Float.isInfinite(anteriorPtZ) || Float.isNaN(anteriorPtZ)) {
                    anteriorPtZ = -999999;
                }

                posteriorPt = fileInfoAfni.getPosteriorPt();
                posteriorPtX = posteriorPt.X;

                if (Float.isInfinite(posteriorPtX) || Float.isNaN(posteriorPtX)) {
                    posteriorPtX = -999999;
                }

                posteriorPtY = posteriorPt.Y;

                if (Float.isInfinite(posteriorPtY) || Float.isNaN(posteriorPtY)) {
                    posteriorPtY = -999999;
                }

                posteriorPtZ = posteriorPt.Z;

                if (Float.isInfinite(posteriorPtZ) || Float.isNaN(posteriorPtZ)) {
                    posteriorPtZ = -999999;
                }

                superiorPt = fileInfoAfni.getSuperiorPt();
                superiorPtX = superiorPt.X;

                if (Float.isInfinite(superiorPtX) || Float.isNaN(superiorPtX)) {
                    superiorPtX = -999999;
                }

                superiorPtY = superiorPt.Y;

                if (Float.isInfinite(superiorPtY) || Float.isNaN(superiorPtY)) {
                    superiorPtY = -999999;
                }

                superiorPtZ = superiorPt.Z;

                if (Float.isInfinite(superiorPtZ) || Float.isNaN(superiorPtZ)) {
                    superiorPtZ = -999999;
                }

                inferiorPt = fileInfoAfni.getInferiorPt();
                inferiorPtX = inferiorPt.X;

                if (Float.isInfinite(inferiorPtX) || Float.isNaN(inferiorPtX)) {
                    inferiorPtX = -999999;
                }

                inferiorPtY = inferiorPt.Y;

                if (Float.isInfinite(inferiorPtY) || Float.isNaN(inferiorPtY)) {
                    inferiorPtY = -999999;
                }

                inferiorPtZ = inferiorPt.Z;

                if (Float.isInfinite(inferiorPtZ) || Float.isNaN(inferiorPtZ)) {
                    inferiorPtZ = -999999;
                }

                leftPt = fileInfoAfni.getLeftPt();
                leftPtX = leftPt.X;

                if (Float.isInfinite(leftPtX) || Float.isNaN(leftPtX)) {
                    leftPtX = -999999;
                }

                leftPtY = leftPt.Y;

                if (Float.isInfinite(leftPtY) || Float.isNaN(leftPtY)) {
                    leftPtY = -999999;
                }

                leftPtZ = leftPt.Z;

                if (Float.isInfinite(leftPtZ) || Float.isNaN(leftPtZ)) {
                    leftPtZ = -999999;
                }

                rightPt = fileInfoAfni.getRightPt();
                rightPtX = rightPt.X;

                if (Float.isInfinite(rightPtX) || Float.isNaN(rightPtX)) {
                    rightPtX = -999999;
                }

                rightPtY = rightPt.Y;

                if (Float.isInfinite(rightPtY) || Float.isNaN(rightPtY)) {
                    rightPtY = -999999;
                }

                rightPtZ = rightPt.Z;

                if (Float.isInfinite(rightPtZ) || Float.isNaN(rightPtZ)) {
                    rightPtZ = -999999;
                }
            } // else if (AFNIViewType == 1)
        } // if (image.getFileInfo()[0] instanceof FileInfoAfni)

        lineString = new String("type = string-attribute\n");
        line = lineString.getBytes();
        raFile.write(line);

        lineString = new String("name = TYPESTRING\n");
        line = lineString.getBytes();
        raFile.write(line);

        switch (AFNITypeString) {

            case 0:
            case 1:
                lineString = new String("count = 15\n");
                break;

            case 2:
            case 3:
                lineString = new String("count = 14\n");
        }

        line = lineString.getBytes();
        raFile.write(line);

        switch (AFNITypeString) {

            case 0:
                lineString = new String("'3DIM_HEAD_ANAT~\n\n");
                break;

            case 1:
                lineString = new String("'3DIM_HEAD_FUNC~\n\n");
                break;

            case 2:
                lineString = new String("'3DIM_GEN_ANAT~\n\n");
                break;

            case 3:
                lineString = new String("'3DIM_GEN_FUNC~\n\n");
        }

        line = lineString.getBytes();
        raFile.write(line);

        lineString = new String("type = integer-attribute\n");
        line = lineString.getBytes();
        raFile.write(line);

        lineString = new String("name = SCENE_DATA\n");
        line = lineString.getBytes();
        raFile.write(line);

        lineString = new String("count = 3\n");
        line = lineString.getBytes();
        raFile.write(line);

        // First 0 for view type = +orig, 1 for + ACPC, 2 for talairach
        // Second 0 for func type = ANAT_SPGR_TYPE
        // 2 for TYPESTRING = 3DIM_GEN_ANAT
        lineString = new String(String.valueOf(AFNIViewType) + " " + String.valueOf(funcType) + " " +
                                String.valueOf(AFNITypeString) + "\n\n");
        line = lineString.getBytes();
        raFile.write(line);

        lineString = new String("type = integer-attribute\n");
        line = lineString.getBytes();
        raFile.write(line);

        lineString = new String("name = ORIENT_SPECIFIC\n");
        line = lineString.getBytes();
        raFile.write(line);

        lineString = new String("count = 3\n");
        line = lineString.getBytes();
        raFile.write(line);

        orient = image.getFileInfo()[0].getAxisOrientation();

        // Note that for the 6 known orientations the MIPAV
        // numerical values are 1 greater than the AFNI numerical
        // values and that AFNI has no value for UNKNOWN_TYPE.
        if ((orient[0] == FileInfoBase.ORI_UNKNOWN_TYPE) || (orient[1] == FileInfoBase.ORI_UNKNOWN_TYPE) ||
                (orient[2] == FileInfoBase.ORI_UNKNOWN_TYPE)) {

            // If unknown assume dicom order
            // x is R-L = 0;
            // y is A-P = 3
            // z is I-S = 4;
            lineString = new String("0 3 4\n\n");
        } else {
            lineString = Integer.toString(orient[0] - 1);
            lineString = lineString + " " + Integer.toString(orient[1] - 1);
            lineString = lineString + " " + Integer.toString(orient[2] - 1);
            lineString = lineString + "\n\n";
        }

        line = lineString.getBytes();
        raFile.write(line);

        // Distances in AFNI are always in millimeters.
        resols = image.getFileInfo()[0].getResolutions();
        origin = image.getFileInfo()[0].getOrigin();
        units = image.getFileInfo()[0].getUnitsOfMeasure();

        for (i = 0; i <= 2; i++) {

            if (units[i] == Unit.MILLIMETERS.getLegacyNum()) { }
            else if (units[i] == Unit.INCHES.getLegacyNum()) {
                resols[i] = 25.4f * resols[i];
                origin[i] = 25.4f * origin[i];
            } else if (units[i] == Unit.MILS.getLegacyNum()) {
                resols[i] = 2.54e-2f * resols[i];
                origin[i] = 2.54e-2f * origin[i];
            } else if (units[i] == Unit.CENTIMETERS.getLegacyNum()) {
                resols[i] = 10.0f * resols[i];
                origin[i] = 10.0f * origin[i];
            } else if (units[i] == Unit.ANGSTROMS.getLegacyNum()) {
                resols[i] = 1.0e-7f * resols[i];
                origin[i] = 1.0e-7f * origin[i];
            } else if (units[i] == Unit.NANOMETERS.getLegacyNum()) {
                resols[i] = 1.0e-6f * resols[i];
                origin[i] = 1.0e-6f * origin[i];
            } else if (units[i] == Unit.MICROMETERS.getLegacyNum()) {
                resols[i] = 1.0e-3f * resols[i];
                origin[i] = 1.0e-3f * origin[i];
            } else if (units[i] == Unit.METERS.getLegacyNum()) {
                resols[i] = 1.0e3f * resols[i];
                origin[i] = 1.0e3f * origin[i];
            } else if (units[i] == Unit.KILOMETERS.getLegacyNum()) {
                resols[i] = 1.0e6f * resols[i];
                origin[i] = 1.0e6f * origin[i];
            } else if (units[i] == Unit.MILES.getLegacyNum()) {
                resols[i] = 1.6093e6f * resols[i];
                origin[i] = 1.6093e6f * origin[i];
            }
        } // for (i = 0; i <= 2; i++)

        // For axes going in the order L2R, P2A, or S2I the resolutions
        // are taken as negative.
        for (i = 0; i <= 2; i++) {

            if ((orient[i] == FileInfoBase.ORI_L2R_TYPE) || (orient[i] == FileInfoBase.ORI_P2A_TYPE) ||
                    (orient[i] == FileInfoBase.ORI_S2I_TYPE)) {
                resols[i] = -resols[i];
            }
        } // for (i = 0; i <= 2; i++)

        lineString = new String("type = float-attribute\n");
        line = lineString.getBytes();
        raFile.write(line);

        lineString = new String("name = DELTA\n");
        line = lineString.getBytes();
        raFile.write(line);

        lineString = new String("count = 3\n");
        line = lineString.getBytes();
        raFile.write(line);

        lineString = Float.toString(resols[0]) + "     ";
        lineString = lineString + Float.toString(resols[1]) + "     ";
        lineString = lineString + Float.toString(resols[2]) + "\n\n";
        line = lineString.getBytes();
        raFile.write(line);

        lineString = new String("type = float-attribute\n");
        line = lineString.getBytes();
        raFile.write(line);

        lineString = new String("name = ORIGIN\n");
        line = lineString.getBytes();
        raFile.write(line);

        lineString = new String("count = 3\n");
        line = lineString.getBytes();
        raFile.write(line);

        lineString = Float.toString(origin[0]) + "     ";
        lineString = lineString + Float.toString(origin[1]) + "     ";
        lineString = lineString + Float.toString(origin[2]) + "\n\n";
        line = lineString.getBytes();
        raFile.write(line);

        if (AFNIViewType == 0) {

            if (superiorEdgeX != -999999) {
                superiorEdgeX = (superiorEdgeX * resols[0]) + origin[0];
            }

            if (superiorEdgeY != -999999) {
                superiorEdgeY = (superiorEdgeY * resols[1]) + origin[1];
            }

            if (superiorEdgeZ != -999999) {
                superiorEdgeZ = (superiorEdgeZ * resols[2]) + origin[2];
            }

            if (posteriorMarginX != -999999) {
                posteriorMarginX = (posteriorMarginX * resols[0]) + origin[0];
            }

            if (posteriorMarginY != -999999) {
                posteriorMarginY = (posteriorMarginY * resols[1]) + origin[1];
            }

            if (posteriorMarginZ != -999999) {
                posteriorMarginZ = (posteriorMarginZ * resols[2]) + origin[2];
            }

            if (inferiorEdgeX != -999999) {
                inferiorEdgeX = (inferiorEdgeX * resols[0]) + origin[0];
            }

            if (inferiorEdgeY != -999999) {
                inferiorEdgeY = (inferiorEdgeY * resols[1]) + origin[1];
            }

            if (inferiorEdgeZ != -999999) {
                inferiorEdgeZ = (inferiorEdgeZ * resols[2]) + origin[2];
            }

            if (firstPtX != -999999) {
                firstPtX = (firstPtX * resols[0]) + origin[0];
            }

            if (firstPtY != -999999) {
                firstPtY = (firstPtY * resols[1]) + origin[1];
            }

            if (firstPtZ != -999999) {
                firstPtZ = (firstPtZ * resols[2]) + origin[2];
            }

            if (anotherPtX != -999999) {
                anotherPtX = (anotherPtX * resols[0]) + origin[0];
            }

            if (anotherPtY != -999999) {
                anotherPtY = (anotherPtY * resols[1]) + origin[1];
            }

            if (anotherPtZ != -999999) {
                anotherPtZ = (anotherPtZ * resols[2]) + origin[2];
            }

            lineString = new String("type = float-attribute\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("name = MARKS_XYZ\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("count = 30\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(superiorEdgeX);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString;
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(superiorEdgeY);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString;
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(superiorEdgeZ);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString;
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(posteriorMarginX);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString;
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(posteriorMarginY);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString + "\n";
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(posteriorMarginZ);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString;
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(inferiorEdgeX);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString;
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(inferiorEdgeY);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString;
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(inferiorEdgeZ);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString;
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(firstPtX);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString + "\n";
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(firstPtY);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString;
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(firstPtZ);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString;
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(anotherPtX);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString;
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(anotherPtY);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString;
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(anotherPtZ);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString + "\n";
            line = lineString.getBytes();
            raFile.write(line);

            lineString = "        -999999        -999999        -999999" + "        -999999        -999999\n" +
                         "        -999999        -999999        -999999" + "        -999999        -999999\n" +
                         "        -999999        -999999        -999999" + "        -999999        -999999\n\n";
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("type = string-attribute\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("name = MARKS_LAB\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("count = 200\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = "'AC superior edge~~~~AC posterior margin~" + "PC inferior edge~~~~First mid-sag pt~~~~" +
                         "Another mid-sag pt~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n";
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("type = string-attribute\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("name = MARKS_HELP\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("count = 2560\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = "'This is the uppermost point\n" + "on the anterior commisure,\n" +
                         "in the mid-sagittal plane." + "~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~~~~~~~~~~~~~~~~~~~~" + "This is the rearmost point\n" +
                         "on the anterior commisure,\n" + "in the mid-sagittal plane." + "~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "This is the bottommost point\n" + "on the posterior commissure,\n" +
                         "in the mid-sagittal plane." + "~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~~~~~~~~~~~~~~~~~" +
                         "You must also specify two other points in the\n" +
                         "mid-sagittal plane, ABOVE the corpus callosum\n" +
                         "(i.e., in the longitudinal fissure).  These\n" +
                         "points are needed to define the vertical plane." + "~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~~~~~~~~~~~~~~~~~~" +
                         "You must also specify two other points in the\n" +
                         "mid-sagittal plane, ABOVE the corpus callosum\n" +
                         "(i.e., in the longitudinal fissure).  These\n" +
                         "points are needed to define the vertical plane." + "~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~\n\n";
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("type = integer-attribute\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("name = MARKS_FLAGS\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("count = 8\n");
            line = lineString.getBytes();
            raFile.write(line);

            if ((marksFlags == MARKSET_ALIGN) || ((marksFlags == 0) && (marksFlag == MARKSET_ALIGN))) {
                lineString = new String("1 1 0 0 0\n0 0 0\n\n");
            } else {
                lineString = new String("-1 -1 -1 -1 -1\n-1 -1 -1 \n\n");
            }

            line = lineString.getBytes();
            raFile.write(line);
        } // if (AFNIViewType == 0)
        else if (AFNIViewType == 1) {

            if (anteriorPtX != -999999) {
                anteriorPtX = (anteriorPtX * resols[0]) + origin[0];
            }

            if (anteriorPtY != -999999) {
                anteriorPtY = (anteriorPtY * resols[1]) + origin[1];
            }

            if (anteriorPtZ != -999999) {
                anteriorPtZ = (anteriorPtZ * resols[2]) + origin[2];
            }

            if (posteriorPtX != -999999) {
                posteriorPtX = (posteriorPtX * resols[0]) + origin[0];
            }

            if (posteriorPtY != -999999) {
                posteriorPtY = (posteriorPtY * resols[1]) + origin[1];
            }

            if (posteriorPtZ != -999999) {
                posteriorPtZ = (posteriorPtZ * resols[2]) + origin[2];
            }

            if (superiorPtX != -999999) {
                superiorPtX = (superiorPtX * resols[0]) + origin[0];
            }

            if (superiorPtY != -999999) {
                superiorPtY = (superiorPtY * resols[1]) + origin[1];
            }

            if (superiorPtZ != -999999) {
                superiorPtZ = (superiorPtZ * resols[2]) + origin[2];
            }

            if (inferiorPtX != -999999) {
                inferiorPtX = (inferiorPtX * resols[0]) + origin[0];
            }

            if (inferiorPtY != -999999) {
                inferiorPtY = (inferiorPtY * resols[1]) + origin[1];
            }

            if (inferiorPtZ != -999999) {
                inferiorPtZ = (inferiorPtZ * resols[2]) + origin[2];
            }

            if (leftPtX != -999999) {
                leftPtX = (leftPtX * resols[0]) + origin[0];
            }

            if (leftPtY != -999999) {
                leftPtY = (leftPtY * resols[1]) + origin[1];
            }

            if (leftPtZ != -999999) {
                leftPtZ = (leftPtZ * resols[2]) + origin[2];
            }

            if (rightPtX != -999999) {
                rightPtX = (rightPtX * resols[0]) + origin[0];
            }

            if (rightPtY != -999999) {
                rightPtY = (rightPtY * resols[1]) + origin[1];
            }

            if (rightPtZ != -999999) {
                rightPtZ = (rightPtZ * resols[2]) + origin[2];
            }

            lineString = new String("type = float-attribute\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("name = MARKS_XYZ\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("count = 30\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(anteriorPtX);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString;
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(anteriorPtY);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString;
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(anteriorPtZ);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString;
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(posteriorPtX);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString;
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(posteriorPtY);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString + "\n";
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(posteriorPtZ);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString;
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(superiorPtX);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString;
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(superiorPtY);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString;
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(superiorPtZ);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString;
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(inferiorPtX);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString + "\n";
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(inferiorPtY);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString;
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(inferiorPtZ);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString;
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(leftPtX);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString;
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(leftPtY);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString;
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(leftPtZ);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString + "\n";
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(rightPtX);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString;
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(rightPtY);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString;
            line = lineString.getBytes();
            raFile.write(line);

            lineString = nf.format(rightPtZ);
            nSpaces = 15 - lineString.getBytes().length;
            lineString1 = "";

            for (i = 0; i < nSpaces; i++) {
                lineString1 = lineString1 + " ";
            }

            lineString = lineString1 + lineString;
            line = lineString.getBytes();
            raFile.write(line);

            lineString = "        -999999        -999999\n" + "        -999999        -999999        -999999" +
                         "        -999999        -999999\n" + "        -999999        -999999        -999999" +
                         "        -999999        -999999\n\n";
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("type = string-attribute\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("name = MARKS_LAB\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("count = 200\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = "'Most anterior point~Most posterior poin~" + "Most superior point~Most inferior point~" +
                         "Most left point~~~~~Most right point~~~~" + "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n";
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("type = string-attribute\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("name = MARKS_HELP\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("count = 2560\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = "'The frontmost point of\n" + "the frontal cortex;\n" + "needed for brain length" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" + "The hindmost point of\n" + "the occipital cortex;\n" +
                         "needed for brain length" + "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "The topmost point of\n" + "the parietal cortex;\n" + "needed for brain height" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" + "The lowest point of\n" + "the temporal cortex;\n" +
                         "needed for brain height" + "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "The most lateral (left) point of\n" + "the parietotemporal cortex;\n" +
                         "needed for brain width" + "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~~~~~~~~" +
                         "The most lateral (right) point of\n" + "the parietotemporal cortex;\n" +
                         "needed for brain width" + "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" +
                         "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" + "~~~~~~\n\n";
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("type = integer-attribute\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("name = MARKS_FLAG\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("count = 8\n");
            line = lineString.getBytes();
            raFile.write(line);

            if ((marksFlags == MARKSET_BOUNDING) || ((marksFlags == 0) && (marksFlag == MARKSET_BOUNDING))) {
                lineString = new String("2 1 0 0 0\n0 0 0\n\n");
            } else {
                lineString = new String("-1 -1 -1 -1 -1\n-1 -1 -1\n\n");
            }

            line = lineString.getBytes();
            raFile.write(line);
        } // else if (AFNIViewType == 1)

        lineString = new String("type = integer-attribute\n");
        line = lineString.getBytes();
        raFile.write(line);

        lineString = new String("name = DATASET_RANK\n");
        line = lineString.getBytes();
        raFile.write(line);

        lineString = new String("count = 2\n");
        line = lineString.getBytes();
        raFile.write(line);

        if (image.getNDims() == 3) {
            lineString = new String("3 1\n\n");
        } else { // image.getNDims() == 4
            lineString = new String("3 ");
            lineString = lineString + Integer.toString(image.getExtents()[3]) + "\n\n";
        }

        line = lineString.getBytes();
        raFile.write(line);

        lineString = new String("type = integer-attribute\n");
        line = lineString.getBytes();
        raFile.write(line);

        lineString = new String("name = DATASET_DIMENSIONS\n");
        line = lineString.getBytes();
        raFile.write(line);

        lineString = new String("count = 3\n");
        line = lineString.getBytes();
        raFile.write(line);

        lineString = Integer.toString(image.getExtents()[0]) + "     ";
        lineString = lineString + Integer.toString(image.getExtents()[1]) + "     ";
        lineString = lineString + Integer.toString(image.getExtents()[2]) + "\n\n";
        line = lineString.getBytes();
        raFile.write(line);

        switch (image.getFileInfo()[0].getDataType()) {

            case ModelStorageBase.UBYTE:
                brickType = 0;
                break;

            case ModelStorageBase.SHORT:
                brickType = 1;
                break;

            case ModelStorageBase.INTEGER:
                brickType = 2;
                break;

            case ModelStorageBase.FLOAT:
                brickType = 3;
                break;

            case ModelStorageBase.DOUBLE:
                brickType = 4;
                break;

            case ModelStorageBase.COMPLEX:
                brickType = 5;
                break;

            case ModelStorageBase.ARGB:
                brickType = 6;
                break;

            default:
                throw new IOException("AFNI cannot write data type = " + image.getFileInfo()[0].getDataType());
        }

        lineString = new String("type = integer-attribute\n");
        line = lineString.getBytes();
        raFile.write(line);

        if (image.getNDims() == 3) {
            tDim = 1;
        } else {
            tDim = image.getExtents()[3];
        }

        lineString = new String("name = BRICK_TYPES\n");
        line = lineString.getBytes();
        raFile.write(line);


        lineString = new String("count = ");
        lineString = lineString + Integer.toString(tDim) + "\n";
        line = lineString.getBytes();
        raFile.write(line);

        lineString = Integer.toString(brickType);

        for (i = 1; i < tDim; i++) {
            lineString = lineString + " " + Integer.toString(brickType);

            if ((i != (tDim - 1)) && ((i % 5) == 4)) {
                lineString = lineString + "\n";
            }
        }

        lineString = lineString + "\n\n";
        line = lineString.getBytes();
        raFile.write(line);

        lineString = new String("type = float-attribute\n");
        line = lineString.getBytes();
        raFile.write(line);

        lineString = new String("name = BRICK_FLOAT_FACS\n");
        line = lineString.getBytes();
        raFile.write(line);


        lineString = new String("count = ");
        lineString = lineString + Integer.toString(tDim) + "\n";
        line = lineString.getBytes();
        raFile.write(line);

        lineString = "0";

        for (i = 1; i < tDim; i++) {
            lineString = lineString + " 0";

            if ((i != (tDim - 1)) && ((i % 5) == 4)) {
                lineString = lineString + "\n";
            }
        }

        lineString = lineString + "\n\n";
        line = lineString.getBytes();
        raFile.write(line);

        // Data files are written BIG-ENDIAN
        lineString = new String("type = string-attribute\n");
        line = lineString.getBytes();
        raFile.write(line);

        lineString = new String("name = BYTEORDER_STRING\n");
        line = lineString.getBytes();
        raFile.write(line);

        lineString = new String("count = 10\n");
        line = lineString.getBytes();
        raFile.write(line);

        lineString = new String("'MSB_FIRST~\n\n");
        line = lineString.getBytes();
        raFile.write(line);

        if (image.getNDims() == 4) {
            lineString = new String("type = integer-attribute\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("name = TAXIS_NUMS\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("count = 3\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = Integer.toString(image.getExtents()[3]) + "  ";

            // 77001 means the units are in msec
            // 77002 means the units are in seconds
            // 77003 means the units are in Hz
            lineString = lineString + Integer.toString(slicesWithTimeOffsets) + "  ";

            if (units[3] == Unit.MILLISEC.getLegacyNum()) {
                lineString = lineString + "77001\n\n";
            } else if (units[3] == Unit.SECONDS.getLegacyNum()) {
                lineString = lineString + "77002\n\n";
            } else if (units[3] == Unit.HZ.getLegacyNum()) {
                lineString = lineString + "77003\n\n";
            } else {
                lineString = lineString + "77001\n\n";
            }

            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("type = float-attribute\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("name = TAXIS_FLOATS\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("count = 5\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("0.0  ");
            lineString = lineString + Float.toString(timeStep) + "   0.0  ";

            if (slicesWithTimeOffsets > 0) {
                lineString = lineString + Float.toString(origin[2]) + "  " + Float.toString(resols[2]) + "\n\n";
            } else {
                lineString = lineString + "0.0  0.0\n\n";
            }

            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("type = float-attribute\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("name = TAXIS_OFFSETS\n");
            line = lineString.getBytes();
            raFile.write(line);

            lineString = new String("count = ");
            lineString = lineString + Integer.toString(image.getExtents()[2]) + "\n";
            line = lineString.getBytes();
            raFile.write(line);

            if ((tAxisOffsets == null) || ((tAxisOffsets != null) && (tAxisOffsets.length != image.getExtents()[2]))) {
                tAxisOffsets = new float[image.getExtents()[2]];

                for (i = 0; i < tAxisOffsets.length; i++) {
                    tAxisOffsets[i] = 0.0f;
                }
            }

            lineString = Float.toString(tAxisOffsets[0]);

            for (i = 1; i < image.getExtents()[2]; i++) {
                lineString = lineString + "  " + Float.toString(tAxisOffsets[i]);

                if ((i != (image.getExtents()[2] - 1)) && ((i % 5) == 4)) {
                    lineString = lineString + "\n";
                }
            }

            lineString = lineString + "\n\n";
            line = lineString.getBytes();
            raFile.write(line);
        } // if (image.getNDims() == 4)

        raFile.close();

        fireProgressStateChanged("Writing data file");
        file = new File(fileDir + fileDataName);
        raFile = new RandomAccessFile(file, "rw");

        sliceSize = image.getExtents()[0] * image.getExtents()[1];
        zDim = image.getExtents()[2];
        numberSlices = (tEnd - tBegin + 1) * (zEnd - zBegin + 1);
        count = 0;

        switch (image.getFileInfo()[0].getDataType()) {

            case ModelStorageBase.UBYTE:
                byteBuffer = new byte[sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = zBegin; z <= zEnd; z++) {
                        fireProgressStateChanged((100 * count++) / numberSlices);
                        image.exportSliceXY((t * zDim) + z, byteBuffer);
                        raFile.write(byteBuffer);
                    }
                }

                break;

            case ModelStorageBase.SHORT:
                shortBuffer = new short[sliceSize];
                byteBuffer = new byte[2 * sliceSize];
                for (t = 0; t <= tEnd; t++) {

                    for (z = zBegin; z <= zEnd; z++) {
                        fireProgressStateChanged((100 * count++) / numberSlices);
                        image.exportSliceXY((t * zDim) + z, shortBuffer);

                        for (j = 0; j < sliceSize; j++) {
                            byteBuffer[2 * j] = (byte) (shortBuffer[j] >>> 8);
                            byteBuffer[(2 * j) + 1] = (byte) (shortBuffer[j]);
                        }

                        raFile.write(byteBuffer);
                    } // for (z = zBegin; z <= zEnd; z++)
                } // for (t = tBegin; t <= tEnd; t++)

                break;

            case ModelStorageBase.INTEGER:
                intBuffer = new int[sliceSize];
                byteBuffer = new byte[4 * sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = zBegin; z <= zEnd; z++) {
                        fireProgressStateChanged((100 * count++) / numberSlices);
                        image.exportSliceXY((t * zDim) + z, intBuffer);

                        for (j = 0; j < sliceSize; j++) {
                            byteBuffer[4 * j] = (byte) (intBuffer[j] >>> 24);
                            byteBuffer[(4 * j) + 1] = (byte) (intBuffer[j] >>> 16);
                            byteBuffer[(4 * j) + 2] = (byte) (intBuffer[j] >>> 8);
                            byteBuffer[(4 * j) + 3] = (byte) (intBuffer[j]);
                        }

                        raFile.write(byteBuffer);
                    } // for (z = zBegin; z <= zEnd; z++)
                } // for (t = tBegin; t <= tEnd; t++)

                break;

            case ModelStorageBase.FLOAT:
                floatBuffer = new float[sliceSize];
                byteBuffer = new byte[4 * sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = zBegin; z <= zEnd; z++) {
                        fireProgressStateChanged((100 * count++) / numberSlices);
                        image.exportSliceXY((t * zDim) + z, floatBuffer);

                        for (j = 0; j < sliceSize; j++) {
                            tmpInt = Float.floatToIntBits(floatBuffer[j]);
                            byteBuffer[4 * j] = (byte) (tmpInt >>> 24);
                            byteBuffer[(4 * j) + 1] = (byte) (tmpInt >>> 16);
                            byteBuffer[(4 * j) + 2] = (byte) (tmpInt >>> 8);
                            byteBuffer[(4 * j) + 3] = (byte) (tmpInt);
                        }

                        raFile.write(byteBuffer);
                    } // for (z = zBegin; z <= zEnd; z++)
                } // for (t = tBegin; t <= tEnd; t++)

                break;

            case ModelStorageBase.DOUBLE:
                doubleBuffer = new double[sliceSize];
                byteBuffer = new byte[8 * sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = zBegin; z <= zEnd; z++) {
                        fireProgressStateChanged((100 * count++) / numberSlices);
                        image.exportSliceXY((t * zDim) + z, doubleBuffer);

                        for (j = 0; j < sliceSize; j++) {
                            tmpLong = Double.doubleToLongBits(doubleBuffer[j]);
                            byteBuffer[8 * j] = (byte) (tmpLong >>> 56);
                            byteBuffer[(8 * j) + 1] = (byte) (tmpLong >>> 48);
                            byteBuffer[(8 * j) + 2] = (byte) (tmpLong >>> 40);
                            byteBuffer[(8 * j) + 3] = (byte) (tmpLong >>> 32);
                            byteBuffer[(8 * j) + 4] = (byte) (tmpLong >>> 24);
                            byteBuffer[(8 * j) + 5] = (byte) (tmpLong >>> 16);
                            byteBuffer[(8 * j) + 6] = (byte) (tmpLong >>> 8);
                            byteBuffer[(8 * j) + 7] = (byte) (tmpLong);
                        }

                        raFile.write(byteBuffer);
                    } // for (z = zBegin; z <= zEnd; z++)
                } // for (t = tBegin; t <= tEnd; t++)

                break;

            case ModelStorageBase.COMPLEX:
                floatBuffer = new float[2 * sliceSize];
                byteBuffer = new byte[8 * sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = zBegin; z <= zEnd; z++) {
                        fireProgressStateChanged((100 * count++) / numberSlices);
                        image.exportData(2 * ((t * zDim) + z) * sliceSize, 2 * sliceSize, floatBuffer);

                        for (j = 0; j < 2 * sliceSize; j++) {
                            tmpInt = Float.floatToIntBits(floatBuffer[j]);
                            byteBuffer[4 * j] = (byte) (tmpInt >>> 24);
                            byteBuffer[(4 * j) + 1] = (byte) (tmpInt >>> 16);
                            byteBuffer[(4 * j) + 2] = (byte) (tmpInt >>> 8);
                            byteBuffer[(4 * j) + 3] = (byte) (tmpInt);
                        }

                        raFile.write(byteBuffer);
                    } // for (z = zBegin; z <= zEnd; z++)
                } // for (t = tBegin; t <= tEnd; t++)
                break;

            case ModelStorageBase.ARGB:
                byteBuffer = new byte[4 * sliceSize];
                byteBuffer2 = new byte[3 * sliceSize];
                for (t = tBegin; t <= tEnd; t++) {

                    for (z = zBegin; z <= zEnd; z++) {
                        fireProgressStateChanged((100 * count++) / numberSlices);
                        image.exportData(4*((t * zDim) + z) * sliceSize, 4 * sliceSize, byteBuffer);
                        for (j = 0; j < sliceSize; j++) {
                            byteBuffer2[3*j] = byteBuffer[4*j + 1];
                            byteBuffer2[3*j + 1] = byteBuffer[4*j + 2];
                            byteBuffer2[3*j + 2] = byteBuffer[4*j + 3];
                        }
                        raFile.write(byteBuffer2);
                    }
                }
                break;
        } // switch(mage.getFileInfo()[0].getDataType())

        raFile.close();

    }

    /**
     * DOCUMENT ME!
     *
     * @param  markerNumber  DOCUMENT ME!
     */
    private void calcOriginalMarker(int markerNumber) {

        // This expects the marker to be in dataset ordering
        float[] originalMarker = new float[3]; // stores the +orig marker location with the original

        // numbers, just dicom reordered
        // This routine is used simply to obtain the same rr,
        // bvec, and svec values that afni.c does.
        originalMarker[0] = marksXYZ[markerNumber * 3];
        originalMarker[1] = marksXYZ[(markerNumber * 3) + 1];
        originalMarker[2] = marksXYZ[(markerNumber * 3) + 2];

        float[] reorderedMarker = new float[3];

        switch (orientSpecific[0]) {

            case FileInfoBase.ORI_R2L_TYPE:
            case FileInfoBase.ORI_L2R_TYPE:
                reorderedMarker[0] = originalMarker[0];
                break;

            case FileInfoBase.ORI_A2P_TYPE:
            case FileInfoBase.ORI_P2A_TYPE:
                reorderedMarker[1] = originalMarker[0];
                break;

            case FileInfoBase.ORI_I2S_TYPE:
            case FileInfoBase.ORI_S2I_TYPE:
                reorderedMarker[2] = originalMarker[0];
                break;
        }

        switch (orientSpecific[1]) {

            case FileInfoBase.ORI_R2L_TYPE:
            case FileInfoBase.ORI_L2R_TYPE:
                reorderedMarker[0] = originalMarker[1];
                break;

            case FileInfoBase.ORI_A2P_TYPE:
            case FileInfoBase.ORI_P2A_TYPE:
                reorderedMarker[1] = originalMarker[1];
                break;

            case FileInfoBase.ORI_I2S_TYPE:
            case FileInfoBase.ORI_S2I_TYPE:
                reorderedMarker[2] = originalMarker[1];
                break;
        }

        switch (orientSpecific[2]) {

            case FileInfoBase.ORI_R2L_TYPE:
            case FileInfoBase.ORI_L2R_TYPE:
                reorderedMarker[0] = originalMarker[2];
                break;

            case FileInfoBase.ORI_A2P_TYPE:
            case FileInfoBase.ORI_P2A_TYPE:
                reorderedMarker[1] = originalMarker[2];
                break;

            case FileInfoBase.ORI_I2S_TYPE:
            case FileInfoBase.ORI_S2I_TYPE:
                reorderedMarker[2] = originalMarker[2];
                break;
        }

        pointMarker = new Vector3f((float) reorderedMarker[0], (float) reorderedMarker[1], (float) reorderedMarker[2]);

    }

    /**
     * DOCUMENT ME!
     */
    private void createOrderDialog() {
        JDialogOrder order = new JDialogOrder(UI.getMainFrame(), orientSpecific);
        doDicom = order.doDicom();
    }

    /**
     * Finds crossproduct of two vectors.
     *
     * @param   pt1  First vector.
     * @param   pt2  Second vector
     *
     * @return  Cross product of pt1 and pt2.
     */
    private Vector3f crossProduct(Vector3f pt1, Vector3f pt2) {
        Vector3f crossPt = new Vector3f(0.0f, 0.0f, 0.0f);
        crossPt.X = (pt1.Y * pt2.Z) - (pt1.Z * pt2.Y);
        crossPt.Y = (pt1.Z * pt2.X) - (pt1.X * pt2.Z);
        crossPt.Z = (pt1.X * pt2.Y) - (pt1.Y * pt2.X);

        return crossPt;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   pMarker  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    /*private Vector3f dataMarker(Vector3f pMarker) {

        // This expects the marker to be in dicom ordering
        // Reorders to dataset order
        float[] reorderedMarker = new float[3];
        float[] originalMarker = new float[3];
        originalMarker[0] = pMarker.X;
        originalMarker[1] = pMarker.Y;
        originalMarker[2] = pMarker.Z;

        switch (dataOrient[0]) {

            case FileInfoBase.ORI_R2L_TYPE:
                reorderedMarker[0] = originalMarker[0];
                break;

            case FileInfoBase.ORI_L2R_TYPE:
                reorderedMarker[0] = origExtents[0] - 1 - originalMarker[0];
                break;

            case FileInfoBase.ORI_A2P_TYPE:
                reorderedMarker[0] = originalMarker[1];
                break;

            case FileInfoBase.ORI_P2A_TYPE:
                reorderedMarker[0] = origExtents[1] - 1 - originalMarker[1];
                break;

            case FileInfoBase.ORI_I2S_TYPE:
                reorderedMarker[0] = originalMarker[2];
                break;

            case FileInfoBase.ORI_S2I_TYPE:
                reorderedMarker[0] = origExtents[2] - 1 - originalMarker[2];
                break;
        }

        switch (dataOrient[1]) {

            case FileInfoBase.ORI_R2L_TYPE:
                reorderedMarker[1] = originalMarker[0];
                break;

            case FileInfoBase.ORI_L2R_TYPE:
                reorderedMarker[1] = origExtents[0] - 1 - originalMarker[0];
                break;

            case FileInfoBase.ORI_A2P_TYPE:
                reorderedMarker[1] = originalMarker[1];
                break;

            case FileInfoBase.ORI_P2A_TYPE:
                reorderedMarker[1] = origExtents[1] - 1 - originalMarker[1];
                break;

            case FileInfoBase.ORI_I2S_TYPE:
                reorderedMarker[1] = originalMarker[2];
                break;

            case FileInfoBase.ORI_S2I_TYPE:
                reorderedMarker[1] = origExtents[2] - 1 - originalMarker[2];
                break;
        }

        switch (dataOrient[2]) {

            case FileInfoBase.ORI_R2L_TYPE:
                reorderedMarker[2] = originalMarker[0];
                break;

            case FileInfoBase.ORI_L2R_TYPE:
                reorderedMarker[2] = origExtents[0] - 1 - originalMarker[0];
                break;

            case FileInfoBase.ORI_A2P_TYPE:
                reorderedMarker[2] = originalMarker[1];
                break;

            case FileInfoBase.ORI_P2A_TYPE:
                reorderedMarker[2] = origExtents[1] - 1 - originalMarker[1];
                break;

            case FileInfoBase.ORI_I2S_TYPE:
                reorderedMarker[2] = originalMarker[2];
                break;

            case FileInfoBase.ORI_S2I_TYPE:
                reorderedMarker[2] = origExtents[2] - 1 - originalMarker[2];
                break;
        }

        Vector3f dMarker = new Vector3f((float) reorderedMarker[0], (float) reorderedMarker[1],
                                        (float) reorderedMarker[2]);

        return dMarker;

    }*/

    /**
     * DOCUMENT ME!
     *
     * @param   pMarker  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private Vector3f dicomMarker(Vector3f pMarker) {

        // This expects the marker to be in dataset ordering
        // Reorders to dicom order
        int[] reorderedMarker = new int[3];
        int[] originalMarker = new int[3];
        originalMarker[0] = (int) (pMarker.X + 0.5f);
        originalMarker[1] = (int) (pMarker.Y + 0.5f);
        originalMarker[2] = (int) (pMarker.Z + 0.5f);

        switch (orientSpecific[0]) {

            case FileInfoBase.ORI_R2L_TYPE:
                reorderedMarker[0] = originalMarker[0];
                break;

            case FileInfoBase.ORI_L2R_TYPE:
                reorderedMarker[0] = xDim - 1 - originalMarker[0];
                break;

            case FileInfoBase.ORI_A2P_TYPE:
                reorderedMarker[1] = originalMarker[0];
                break;

            case FileInfoBase.ORI_P2A_TYPE:
                reorderedMarker[1] = xDim - 1 - originalMarker[0];
                break;

            case FileInfoBase.ORI_I2S_TYPE:
                reorderedMarker[2] = originalMarker[0];
                break;

            case FileInfoBase.ORI_S2I_TYPE:
                reorderedMarker[2] = xDim - 1 - originalMarker[0];
                break;
        }

        switch (orientSpecific[1]) {

            case FileInfoBase.ORI_R2L_TYPE:
                reorderedMarker[0] = originalMarker[1];
                break;

            case FileInfoBase.ORI_L2R_TYPE:
                reorderedMarker[0] = yDim - 1 - originalMarker[1];
                break;

            case FileInfoBase.ORI_A2P_TYPE:
                reorderedMarker[1] = originalMarker[1];
                break;

            case FileInfoBase.ORI_P2A_TYPE:
                reorderedMarker[1] = yDim - 1 - originalMarker[1];
                break;

            case FileInfoBase.ORI_I2S_TYPE:
                reorderedMarker[2] = originalMarker[1];
                break;

            case FileInfoBase.ORI_S2I_TYPE:
                reorderedMarker[2] = yDim - 1 - originalMarker[1];
                break;
        }

        switch (orientSpecific[2]) {

            case FileInfoBase.ORI_R2L_TYPE:
                reorderedMarker[0] = originalMarker[2];
                break;

            case FileInfoBase.ORI_L2R_TYPE:
                reorderedMarker[0] = zDim - 1 - originalMarker[2];
                break;

            case FileInfoBase.ORI_A2P_TYPE:
                reorderedMarker[1] = originalMarker[2];
                break;

            case FileInfoBase.ORI_P2A_TYPE:
                reorderedMarker[1] = zDim - 1 - originalMarker[2];
                break;

            case FileInfoBase.ORI_I2S_TYPE:
                reorderedMarker[2] = originalMarker[2];
                break;

            case FileInfoBase.ORI_S2I_TYPE:
                reorderedMarker[2] = zDim - 1 - originalMarker[2];
                break;
        }

        Vector3f dMarker = new Vector3f((float) reorderedMarker[0], (float) reorderedMarker[1],
                                        (float) reorderedMarker[2]);

        return dMarker;

    }

    /**
     * DOCUMENT ME!
     *
     * @param   pt1    DOCUMENT ME!
     * @param   pt2    DOCUMENT ME!
     * @param   resol  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private float dist(Vector3f pt1, Vector3f pt2, float[] resol) {
        float distX, distY, distZ;
        float length;
        distX = (pt1.X - pt2.X) * resol[0];
        distX = distX * distX;
        distY = (pt1.Y - pt2.Y) * resol[1];
        distY = distY * distY;
        distZ = (pt1.Z - pt2.Z) * resol[2];
        distZ = distZ * distZ;
        length = (float) Math.sqrt(distX + distY + distZ);

        return length;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   pt1  DOCUMENT ME!
     * @param   pt2  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private float dotProduct(Vector3f pt1, Vector3f pt2) {
        float dot;
        dot = (pt1.X * pt2.X) + (pt1.Y * pt2.Y) + (pt1.Z * pt2.Z);

        return dot;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   pt     DOCUMENT ME!
     * @param   resol  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private Vector3f makemmVector3f(Vector3f pt, float[] resol) {
        Vector3f mmPt = new Vector3f(0.0f, 0.0f, 0.0f);
        mmPt.X = resol[0] * pt.X;
        mmPt.Y = resol[1] * pt.Y;
        mmPt.Z = resol[2] * pt.Z;

        return mmPt;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   pt     DOCUMENT ME!
     * @param   resol  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private Vector3f makeVoxelCoord3Df(Vector3f pt, float[] resol) {
        Vector3f voxelPt = new Vector3f(0.0f, 0.0f, 0.0f);
        voxelPt.X = pt.X / resol[0];
        voxelPt.Y = pt.Y / resol[1];
        voxelPt.Z = pt.Z / resol[2];

        return voxelPt;
    }

    /**
     * Finds the normal to the vector.
     *
     * @param   pt  Vector to find normal to.
     *
     * @return  Normal of pt.
     */
    private Vector3f norm(Vector3f pt) {
        float scale;
        Vector3f normPt = new Vector3f(0.0f, 0.0f, 0.0f);
        scale = (pt.X * pt.X) + (pt.Y * pt.Y) + (pt.Z * pt.Z);
        scale = (float) ((scale > 0) ? (1.0 / Math.sqrt(scale)) : 0);
        normPt.X = pt.X * scale;
        normPt.Y = pt.Y * scale;
        normPt.Z = pt.Z * scale;

        return normPt;

    }

    /**
     * Reads a slice of data at a time and stores the results in the buffer.
     *
     * @param      slice      offset into the file stored in the dataOffset array
     * @param      buffer     buffer where the info is stored
     * @param      scaleFact  if zero data unscaled, if > 0 data is scaled by scaleFact
     * @param      numRead    DOCUMENT ME!
     *
     * @numRead    number of data values read
     *
     * @exception  IOException  if there is an error reading the file
     */
    private void readBuffer(int slice, float[] buffer, float scaleFact, int numRead) throws IOException {
        int i = 0;
        int j;
        int nBytes;
        int b1, b2, b3, b4;
        byte[] byteBuffer;
        int progress, progressLength, mod;
        int tmpInt;
  
        switch (brikDataType) {

            case ModelStorageBase.UBYTE:
                byteBuffer = new byte[buffer.length];
                nBytes = numRead;
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * numberSlices;
                mod = progressLength / 100;

                for (j = 0; j < nBytes; j++, i++) {

                    if (((i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    buffer[i] = byteBuffer[j] & 0xff;
                }

                break;

            case ModelStorageBase.SHORT:
                byteBuffer = new byte[2 * buffer.length];
                nBytes = 2 * numRead;
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * numberSlices;
                mod = progressLength / 10;

                for (j = 0; j < nBytes; j += 2, i++) {

                    if (((i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    b1 = getUnsignedByte(byteBuffer, j);
                    b2 = getUnsignedByte(byteBuffer, j + 1);

                    if (endianess) {
                        buffer[i] = (short) ((b1 << 8) + b2);
                    } else {
                        buffer[i] = (short) ((b2 << 8) + b1);
                    }

                } // for (j = 0; j < nBytes; j+=2, i++ )

                break;

            case ModelStorageBase.INTEGER:
                byteBuffer = new byte[4 * buffer.length];
                nBytes = 4 * numRead;
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * numberSlices;
                mod = progressLength / 10;

                for (j = 0; j < nBytes; j += 4, i++) {

                    if (((i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    b1 = getUnsignedByte(byteBuffer, j);
                    b2 = getUnsignedByte(byteBuffer, j + 1);
                    b3 = getUnsignedByte(byteBuffer, j + 2);
                    b4 = getUnsignedByte(byteBuffer, j + 3);

                    if (endianess) {
                        buffer[i] = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
                    } else {
                        buffer[i] = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little Endian
                    }
                } // for (j =0; j < nBytes; j+=4, i++ )

                break;

            case ModelStorageBase.FLOAT:
                byteBuffer = new byte[4 * buffer.length];
                nBytes = 4 * numRead;
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * numberSlices;
                mod = progressLength / 10;

                for (j = 0; j < nBytes; j += 4, i++) {

                    if (((i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    b1 = getUnsignedByte(byteBuffer, j);
                    b2 = getUnsignedByte(byteBuffer, j + 1);
                    b3 = getUnsignedByte(byteBuffer, j + 2);
                    b4 = getUnsignedByte(byteBuffer, j + 3);

                    if (endianess) {
                        tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
                    } else {
                        tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little Endian
                    }

                    buffer[i] = Float.intBitsToFloat(tmpInt);
                } // for (j =0; j < nBytes; j+=4, i++ )

                break;

            case ModelStorageBase.DOUBLE:
                break;

            case ModelStorageBase.COMPLEX:
                byteBuffer = new byte[4 * buffer.length];
                nBytes = 8 * numRead;
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * numberSlices;
                mod = progressLength / 10;

                for (j = 0; j < nBytes; j += 4, i++) {

                    if (((i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    b1 = getUnsignedByte(byteBuffer, j);
                    b2 = getUnsignedByte(byteBuffer, j + 1);
                    b3 = getUnsignedByte(byteBuffer, j + 2);
                    b4 = getUnsignedByte(byteBuffer, j + 3);

                    if (endianess) {
                        tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
                    } else {
                        tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little Endian
                    }

                    buffer[i] = Float.intBitsToFloat(tmpInt);
                } // for (j =0; j < nBytes; j+=4, i++ )
                break;

            case ModelStorageBase.ARGB:
                byteBuffer = new byte[3*buffer.length/4];
                nBytes = 3 * numRead;
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * numberSlices;
                mod = progressLength / 100;

                for (j = 0; j < nBytes; j+= 3, i+= 4) {

                    if (((i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    buffer[i] = 0;
                    buffer[i+1] = byteBuffer[j] & 0xff;
                    buffer[i+2] = byteBuffer[j+1] & 0xff;
                    buffer[i+3] = byteBuffer[j+2] & 0xff;
                }
                break;
        } // switch(brikDataType)

        if (scaleFact > 0) {

            for (i = 0; i < buffer.length; i++) {
                buffer[i] *= scaleFact;
            }
        } // if (scaleFact > 0)

    }


    /**
     * Reads AFNI header (.HEAD file)
     *
     * @exception  IOException  if there is an error reading the file
     */
    public boolean readHeader() throws IOException {
        boolean done, restart, countRead, exceptionOccurred, isNull;
        String lineString;
        String tmpString;
        String countString;
        String intString;
        String floatString;
        byte leadByte;
        byte[] buffer;
        int i = 0;
        int j = 0;
        int typeAttribute = STRING_ATTRIBUTE;
        String nameString = new String("TYPESTRING");
        int countEntries = 0;
        int[] intVar = new int[1000];
        float[] floatVar = new float[2000];
        StringTokenizer t;
        int lineCounts;
        String varString;
        int followingParms;
        int tagnum, index;

        try {
            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "r");
            fileLength = raFile.length();

            fileInfo = new FileInfoAfni(fileName, fileDir, FileUtility.AFNI); // dummy fileInfo

            if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                haveReadOrig = true;
            } else if (presentViewType == FileInfoAfni.AFNI_ACPC) {
                haveReadACPC = true;
            }

            if (haveReadOrig && haveReadACPC) {
                tInfo.isAcpc(true);
                tInfo.isTlrc(true);
            }

            done = false;

            while (!done) {
                restart = false;

                if (raFile.getFilePointer() >= (fileLength - 1)) {
                    done = true;

                    break;
                }

                lineString = readLine();
                lineString = removeWhiteSpace(lineString);

                if (lineString == null) {
                    restart = true;
                } else if (lineString.equalsIgnoreCase("type=integer-attribute")) {
                    typeAttribute = INTEGER_ATTRIBUTE;
                } else if (lineString.equalsIgnoreCase("type=float-attribute")) {
                    typeAttribute = FLOAT_ATTRIBUTE;
                } else if (lineString.equalsIgnoreCase("type=string-attribute")) {
                    typeAttribute = STRING_ATTRIBUTE;
                } else {
                    restart = true;
                }

                if (!restart) {

                    if (raFile.getFilePointer() >= (fileLength - 1)) {
                        done = true;

                        break;
                    }

                    nameString = readLine();
                    nameString = removeWhiteSpace(nameString);
                    tmpString = (nameString).substring(0, 5);

                    if (tmpString.equalsIgnoreCase("name=")) {
                        nameString = nameString.substring(5);
                    } else {
                        restart = true;
                    }
                } // if (!restart)

                if (!restart) {

                    if (raFile.getFilePointer() >= (fileLength - 1)) {
                        done = true;

                        break;
                    }

                    countString = readLine();
                    countString = removeWhiteSpace(countString);
                    tmpString = countString.substring(0, 6);

                    if (tmpString.equalsIgnoreCase("count=")) {
                        countString = countString.substring(6);
                        countEntries = Integer.valueOf(countString).intValue();

                        if (countEntries <= 0) {
                            restart = true;
                        }
                    } else {
                        restart = true;
                    }
                } // if (!restart)

                if (!restart) {

                    if (typeAttribute == STRING_ATTRIBUTE) {
                        leadByte = raFile.readByte();

                        if (leadByte != 39) { // apostrophe
                            done = true;

                            break;
                        }

                        buffer = new byte[countEntries];

                        for (i = 0; i < countEntries; i++) {
                            buffer[i] = raFile.readByte();
                        }

                        if (buffer[countEntries - 1] != '~') {
                            done = true;

                            break;
                        }

                        for (i = 0; i < countEntries; i++) {

                            if (buffer[i] == 126) {
                                buffer[i] = 0; // replace tilde with NUL
                            }
                        }

                        if ((nameString).equalsIgnoreCase("TYPESTRING")) {
                            typeString = new String(buffer, 0, countEntries - 1);
                            Preferences.debug("TYPESTRING = " + typeString + "\n", Preferences.DEBUG_FILEIO);

                            if (typeString.equalsIgnoreCase("3DIM_HEAD_ANAT")) {

                                anatType = true;
                            } else if (typeString.equalsIgnoreCase("3DIM_GEN_ANAT")) {
                                anatType = true;
                            } else if (typeString.equalsIgnoreCase("3DIM_HEAD_FUNC")) {
                                anatType = false;
                            } else if (typeString.equalsIgnoreCase("3DIM_GEN_FUNC")) {
                                anatType = false;
                            } else {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: TYPESTRING = " + typeString + "\n");
                            }

                        } else if (nameString.equalsIgnoreCase("IDCODE_STRING")) {
                            idcodeString = new String(buffer, 0, countEntries - 1);
                            Preferences.debug("IDCODE_STRING = " + idcodeString + "\n", Preferences.DEBUG_FILEIO);
                            fileInfo.setIDCodeString(idcodeString);
                        } else if (nameString.equalsIgnoreCase("IDCODE_DATE")) {
                            idcodeDateString = new String(buffer, 0, countEntries - 1);
                            Preferences.debug("IDCODE_DATE = " + idcodeDateString + "\n", Preferences.DEBUG_FILEIO);
                            fileInfo.setIDCodeDate(idcodeDateString);
                        } else if (nameString.equalsIgnoreCase("BYTEORDER_STRING")) {
                            tmpString = new String(buffer, 0, countEntries - 1);

                            if (tmpString.equalsIgnoreCase("MSB_FIRST")) {
                                endianess = FileBase.BIG_ENDIAN;
                            } else if (tmpString.equalsIgnoreCase("LSB_FIRST")) {
                                endianess = FileBase.LITTLE_ENDIAN;
                            } else {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: BYTEORDER_STRING has an illegal " +
                                                      tmpString);
                            }

                            Preferences.debug("BYTE_ORDER_STRING = " + tmpString + "\n", Preferences.DEBUG_FILEIO);
                            fileInfo.setEndianess(endianess);
                        } // else if (nameString.equalsIgnoreCase("BYTEORDER_STRING"))
                        else if (nameString.equalsIgnoreCase("BRICK_LABS")) {
                        	try {
	                        	Vector<Integer> zeroLoc = new Vector<Integer>();
	                        	zeroLoc.add(0);
	                        	for(int k=0; k<buffer.length; k++) {
	                        		if(buffer[k] == 0) {
	                        			zeroLoc.add(k);
	                        		}
	                        	}
	                        	
	                        	int size = buffer[buffer.length-1] == 0 ? zeroLoc.size()-1 : zeroLoc.size();
	                        	brickLabsString = new String[size];
	                        	for(int k=0; k<zeroLoc.size()-1; k++) {
	                        		brickLabsString[k] = new String(buffer, zeroLoc.get(k), zeroLoc.get(k+1)-zeroLoc.get(k));
	                        	}
	                        	
	                        	if(size > zeroLoc.size()) {
	                        		brickLabsString[brickLabsString.length-1] = new String(buffer, zeroLoc.get(zeroLoc.size()-1), buffer.length - zeroLoc.get(zeroLoc.size()-1) - 1);
	                        	}
                        	} catch(Exception e) {
                        		brickLabsString = new String[1];
                        		brickLabsString[0] = new String(buffer, 0, countEntries - 1);
                        		Preferences.debug("Parsng of sub-brick names from BRICK_LABS failed, storing in single sub-brick entry\n", Preferences.DEBUG_FILEIO);
                        	}

                            for(int k=0; k<brickLabsString.length; k++) {
                            	Preferences.debug("BRICK_LABS["+k+"] = " + brickLabsString[k] + "\n", Preferences.DEBUG_FILEIO);
                            }
                            fileInfo.setBrickLabsString(brickLabsString);
                        } else if (nameString.equalsIgnoreCase("HISTORY_NOTE")) {
                            historyNoteString = new String(buffer, 0, countEntries - 1);
                            Preferences.debug("HISTORY_NOTE = " + historyNoteString + "\n", Preferences.DEBUG_FILEIO);
                        } else if (nameString.substring(0, nameString.length() - 3).equalsIgnoreCase("NOTE_NUMBER_")) {
                            tmpString = nameString.substring(nameString.length() - 3);

                            if (tmpString.equals("001")) {
                                noteNumber001String = new String(buffer, 0, countEntries - 1);
                                Preferences.debug("NOTE_NUMBER_001 = " + noteNumber001String + "\n", Preferences.DEBUG_FILEIO);
                                fileInfo.setNoteNumber001(noteNumber001String);
                            } else if (tmpString.equals("002")) {
                                noteNumber002String = new String(buffer, 0, countEntries - 1);
                                Preferences.debug("NOTE_NUMBER_002 = " + noteNumber002String + "\n", Preferences.DEBUG_FILEIO);
                                fileInfo.setNoteNumber002(noteNumber002String);
                            } else if (tmpString.equals("003")) {
                                noteNumber003String = new String(buffer, 0, countEntries - 1);
                                Preferences.debug("NOTE_NUMBER_003 = " + noteNumber003String + "\n", Preferences.DEBUG_FILEIO);
                                fileInfo.setNoteNumber003(noteNumber003String);
                            } else if (tmpString.equals("004")) {
                                noteNumber004String = new String(buffer, 0, countEntries - 1);
                                Preferences.debug("NOTE_NUMBER_004 = " + noteNumber004String + "\n", Preferences.DEBUG_FILEIO);
                                fileInfo.setNoteNumber004(noteNumber004String);
                            } else if (tmpString.equals("005")) {
                                noteNumber005String = new String(buffer, 0, countEntries - 1);
                                Preferences.debug("NOTE_NUMBER_005 = " + noteNumber005String + "\n", Preferences.DEBUG_FILEIO);
                                fileInfo.setNoteNumber005(noteNumber005String);
                            } else {
                                tmpString = new String(buffer, 0, countEntries - 1);
                                Preferences.debug(nameString + " = " + tmpString, Preferences.DEBUG_FILEIO);
                            }
                        } else if (nameString.substring(0, nameString.length() - 3).equalsIgnoreCase("NOTE_DATE_")) {
                            tmpString = nameString.substring(nameString.length() - 3);

                            if (tmpString.equals("001")) {
                                noteDate001String = new String(buffer, 0, countEntries - 1);
                                Preferences.debug("NOTE_DATE_001 = " + noteDate001String + "\n", Preferences.DEBUG_FILEIO);
                                fileInfo.setNoteDate001(noteDate001String);
                            } else if (tmpString.equals("002")) {
                                noteDate002String = new String(buffer, 0, countEntries - 1);
                                Preferences.debug("NOTE_DATE_002 = " + noteDate002String + "\n", Preferences.DEBUG_FILEIO);
                                fileInfo.setNoteDate002(noteDate002String);
                            } else if (tmpString.equals("003")) {
                                noteDate003String = new String(buffer, 0, countEntries - 1);
                                Preferences.debug("NOTE_DATE_003 = " + noteDate003String + "\n", Preferences.DEBUG_FILEIO);
                                fileInfo.setNoteDate003(noteDate003String);
                            } else if (tmpString.equals("004")) {
                                noteDate004String = new String(buffer, 0, countEntries - 1);
                                Preferences.debug("NOTE_DATE_004 = " + noteDate004String + "\n", Preferences.DEBUG_FILEIO);
                                fileInfo.setNoteDate004(noteDate004String);
                            } else if (tmpString.equals("005")) {
                                noteDate005String = new String(buffer, 0, countEntries - 1);
                                Preferences.debug("NOTE_DATE_005 = " + noteDate005String + "\n", Preferences.DEBUG_FILEIO);
                                fileInfo.setNoteDate005(noteDate005String);
                            } else {
                                tmpString = new String(buffer, 0, countEntries - 1);
                                Preferences.debug(nameString + " = " + tmpString, Preferences.DEBUG_FILEIO);
                            }
                        } else if (nameString.equalsIgnoreCase("VOLREG_ROTPARENT_IDCODE")) {
                            volregRotparentIdcodeString = new String(buffer, 0, countEntries - 1);
                            Preferences.debug("VOLREG_ROTPARENT_IDCODE = " + volregRotparentIdcodeString + "\n", 
                            		Preferences.DEBUG_FILEIO);
                        } else if (nameString.equalsIgnoreCase("VOLREG_ROTPARENT_NAME")) {
                            volregRotparentNameString = new String(buffer, 0, countEntries - 1);
                            Preferences.debug("VOLREG_ROTPARENT_NAME = " + volregRotparentNameString + "\n",
                            		Preferences.DEBUG_FILEIO);
                        } else if (nameString.equalsIgnoreCase("VOLREG_GRIDPARENT_IDCODE")) {
                            volregGridparentIdcodeString = new String(buffer, 0, countEntries - 1);
                            Preferences.debug("VOLREG_GRIDPARENT_IDCODE = " + volregGridparentIdcodeString + "\n", 
                            		Preferences.DEBUG_FILEIO);
                        } else if (nameString.equalsIgnoreCase("VOLREG_GRIDPARENT_NAME")) {
                            volregGridparentNameString = new String(buffer, 0, countEntries - 1);
                            Preferences.debug("VOLREG_GRIDPARENT_NAME = " + volregGridparentNameString + "\n",
                            		Preferences.DEBUG_FILEIO);
                        } else if (nameString.equalsIgnoreCase("VOLREG_INPUT_IDCODE")) {
                            volregInputIdcodeString = new String(buffer, 0, countEntries - 1);
                            Preferences.debug("VOLREG_INPUT_IDCODE = " + volregInputIdcodeString + "\n",
                            		Preferences.DEBUG_FILEIO);
                        } else if (nameString.equalsIgnoreCase("VOLREG_INPUT_NAME")) {
                            volregInputNameString = new String(buffer, 0, countEntries - 1);
                            Preferences.debug("VOLREG_INPUT_NAME = " + volregInputNameString + "\n", Preferences.DEBUG_FILEIO);
                        } else if (nameString.equalsIgnoreCase("VOLREG_BASE_IDCODE")) {
                            volregBaseIdcodeString = new String(buffer, 0, countEntries - 1);
                            Preferences.debug("VOLREG_BASE_IDCODE = " + volregBaseIdcodeString + "\n",
                            		Preferences.DEBUG_FILEIO);
                        } else if (nameString.equalsIgnoreCase("VOLREG_BASE_NAME")) {
                            volregBaseNameString = new String(buffer, 0, countEntries - 1);
                            Preferences.debug("VOLREG_BASE_NAME = " + volregBaseNameString + "\n", Preferences.DEBUG_FILEIO);
                        } else if (nameString.equalsIgnoreCase("IDCODE_ANAT_PARENT")) {
                            idcodeAnatParentString = new String(buffer, 0, countEntries - 1);
                            Preferences.debug("IDCODE_ANAT_PARENT = " + idcodeAnatParentString + "\n",
                            		Preferences.DEBUG_FILEIO);
                        } else if (nameString.equalsIgnoreCase("IDCODE_WARP_PARENT")) {
                            idcodeWarpParentString = new String(buffer, 0, countEntries - 1);
                            Preferences.debug("IDCODE_WARP_PARENT = " + idcodeWarpParentString + "\n",
                            		Preferences.DEBUG_FILEIO);
                        } else if (nameString.equalsIgnoreCase("MARKS_LAB")) {

                            if (countEntries < 200) {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: MARKS_LAB has " + countEntries +
                                                      " characters instead of the expected 200");
                            }

                            tmpString = new String(buffer);

                            for (i = 0; i < 10; i++) {
                                marksLabString[i] = tmpString.substring(i * 20, (i * 20) + 20);
                                marksLabString[i] = marksLabString[i].trim();
                                isNull = true;

                                for (j = 0; (j < (marksLabString[i]).length()) && isNull; j++) {

                                    if ((marksLabString[i]).charAt(j) > 0x20) {
                                        isNull = false;
                                    }
                                }

                                if (isNull) {
                                    marksLabString[i] = null;
                                }
                            } // for (i = 0; i < 10; i++)
                        } // else if (nameString.equalsIgnoreCase("MARKS_LAB"))
                        else if (nameString.equalsIgnoreCase("MARKS_HELP")) {

                            if (countEntries < 2560) {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: MARKS_HELP has " + countEntries +
                                                      " characters instead of the expected 2560");
                            }

                            tmpString = new String(buffer);

                            for (i = 0; i < 10; i++) {
                                marksHelpString[i] = tmpString.substring(i * 256, (i * 256) + 256);
                                marksHelpString[i] = marksHelpString[i].trim();
                                isNull = true;

                                for (j = 0; (j < (marksHelpString[i]).length()) && isNull; j++) {

                                    if ((marksHelpString[i]).charAt(j) > 0x20) {
                                        isNull = false;
                                    }
                                }

                                if (isNull) {
                                    marksHelpString[i] = null;
                                }

                                if (marksHelpString[i] != null) {
                                    Preferences.debug("MARKS_HELP[" + i + "] = " + marksHelpString[i] + "\n",
                                    		Preferences.DEBUG_FILEIO);
                                }
                            } // for (i = 0; i < 10; i++)
                        } // else if (nameString.equalsIgnoreCase("MARKS_HELP"))
                        else if (nameString.equalsIgnoreCase("TAGSET_LABELS")) {
                            tagsetLabelsString = new String(buffer, 0, countEntries - 1);
                            Preferences.debug("TAGSET_LABELS = " + tagsetLabelsString + "\n",
                            		Preferences.DEBUG_FILEIO);
                        } else if (nameString.equalsIgnoreCase("LABEL_1")) {
                            label1String = new String(buffer, 0, countEntries - 1);
                            Preferences.debug("LABEL_1 = " + label1String + "\n", Preferences.DEBUG_FILEIO);
                            fileInfo.setLabel1(label1String);
                        } else if (nameString.equalsIgnoreCase("LABEL_2")) {
                            label2String = new String(buffer, 0, countEntries - 1);
                            Preferences.debug("LABEL_2 = " + label2String + "\n", Preferences.DEBUG_FILEIO);
                            fileInfo.setLabel2(label2String);
                        } else if (nameString.equalsIgnoreCase("DATASET_NAME")) {
                            datasetNameString = new String(buffer, 0, countEntries - 1);
                            Preferences.debug("DATASET_NAME = " + datasetNameString + "\n", Preferences.DEBUG_FILEIO);
                            fileInfo.setDatasetName(datasetNameString);
                        } else if (nameString.equalsIgnoreCase("DATASET_KEYWORDS")) {
                            datasetKeywordsString = new String(buffer, 0, countEntries - 1);
                            Preferences.debug("DATASET_KEYWORDS = " + datasetKeywordsString + "\n", Preferences.DEBUG_FILEIO);
                        } else if (nameString.equalsIgnoreCase("BRICK_KEYWORDS")) {
                            brickKeywordsString = new String(buffer, 0, countEntries - 1);
                            Preferences.debug("BRICK_KEYWORDS = " + brickKeywordsString + "\n", Preferences.DEBUG_FILEIO);
                        } else if (nameString.equalsIgnoreCase("WARP_PARENTNAME")) {
                            warpParentnameString = new String(buffer, 0, countEntries - 1);
                            Preferences.debug("WARP_PARENTNAME = " + warpParentnameString + "\n", Preferences.DEBUG_FILEIO);
                        } else if (nameString.equalsIgnoreCase("ANATOMY_PARENTNAME")) {
                            anatomyParentnameString = new String(buffer, 0, countEntries - 1);
                            Preferences.debug("ANATOMY_PARENTNAME = " + anatomyParentnameString + "\n",
                            		Preferences.DEBUG_FILEIO);
                        } else {
                            tmpString = new String(buffer, 0, countEntries - 1);
                            Preferences.debug(nameString + " = " + tmpString + "\n", Preferences.DEBUG_FILEIO);
                        }
                    } // if (typeAttribute == STRING_ATTRIBUTE)
                    else if (typeAttribute == INTEGER_ATTRIBUTE) {
                        countRead = false;
                        i = 0;
                        lineCounts = 1;

                        while ((!countRead) && (lineCounts > 0)) {
                            intString = readLine();
                            t = new StringTokenizer(intString);
                            exceptionOccurred = false;
                            lineCounts = 0;

                            while ((!exceptionOccurred) && (!countRead)) {

                                try {
                                    varString = t.nextToken();
                                    intVar[i++] = Integer.parseInt(varString);
                                    lineCounts++;

                                    if (i == countEntries) {
                                        countRead = true;
                                    }
                                } catch (NoSuchElementException e) {
                                    exceptionOccurred = true;
                                }
                            } // while ((!exceptionOcurred) && (!countRead))
                        } // while ((!countRead)&& (lineCounts > 0))

                        if (!countRead) {
                            raFile.close();
                            throw new IOException("AFNI Read Header Error: For " + nameString + " read " + i +
                                                  " out of " + countEntries + " integers");
                        } // if (!countRead)

                        if (nameString.equalsIgnoreCase("SCENE_DATA")) {

                            if (countEntries < 3) {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: SCENE_DATA has only " + countEntries +
                                                      " instead of the needed 3");
                            }

                            if (presentViewType != intVar[0]) {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: From SCENE_DATA view type = " +
                                                      intVar[0] + " but from filename presentViewType = " +
                                                      presentViewType);
                            } // if (presentViewType != intVar[0])

                            funcType = intVar[1];

                            if (funcType < 0) {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: funcType illegally = " + funcType);
                            }

                            fileInfo.setFuncType(funcType);
                            typeStringType = intVar[2];

                            if ((typeStringType < 0) || (typeStringType > 3)) {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: typeStringType illegally = " +
                                                      typeStringType);
                            }

                        } // if (nameString.equalsIgnoreCase("SCENE_DATA"))
                        else if (nameString.equalsIgnoreCase("ORIENT_SPECIFIC")) {

                            if (countEntries < 3) {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: ORIENT_SPECIFIC has only " +
                                                      countEntries + " instead of the needed 3");
                            }

                            orientSpecific[0] = intVar[0] + 1; // Add one to make our FileInfoBase.ORI_ ... constansts
                            orientSpecific[1] = intVar[1] + 1;
                            orientSpecific[2] = intVar[2] + 1;

                            if ((orientSpecific[0] != FileInfoBase.ORI_R2L_TYPE) &&
                                    (orientSpecific[0] != FileInfoBase.ORI_L2R_TYPE) &&
                                    (orientSpecific[1] != FileInfoBase.ORI_R2L_TYPE) &&
                                    (orientSpecific[1] != FileInfoBase.ORI_L2R_TYPE) &&
                                    (orientSpecific[2] != FileInfoBase.ORI_R2L_TYPE) &&
                                    (orientSpecific[2] != FileInfoBase.ORI_L2R_TYPE)) {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: ORIENT_SPECIFIC = " + orientSpecific[0] +
                                                      "  " + orientSpecific[1] + "  " + orientSpecific[2]);
                            }

                            if ((orientSpecific[0] != FileInfoBase.ORI_P2A_TYPE) &&
                                    (orientSpecific[0] != FileInfoBase.ORI_A2P_TYPE) &&
                                    (orientSpecific[1] != FileInfoBase.ORI_P2A_TYPE) &&
                                    (orientSpecific[1] != FileInfoBase.ORI_A2P_TYPE) &&
                                    (orientSpecific[2] != FileInfoBase.ORI_P2A_TYPE) &&
                                    (orientSpecific[2] != FileInfoBase.ORI_A2P_TYPE)) {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: ORIENT_SPECIFIC = " + orientSpecific[0] +
                                                      "  " + orientSpecific[1] + "  " + orientSpecific[2]);
                            }

                            if ((orientSpecific[0] != FileInfoBase.ORI_I2S_TYPE) &&
                                    (orientSpecific[0] != FileInfoBase.ORI_S2I_TYPE) &&
                                    (orientSpecific[1] != FileInfoBase.ORI_I2S_TYPE) &&
                                    (orientSpecific[1] != FileInfoBase.ORI_S2I_TYPE) &&
                                    (orientSpecific[2] != FileInfoBase.ORI_I2S_TYPE) &&
                                    (orientSpecific[2] != FileInfoBase.ORI_S2I_TYPE)) {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: ORIENT_SPECIFIC = " + orientSpecific[0] +
                                                      "  " + orientSpecific[1] + "  " + orientSpecific[2]);
                            }

                            if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                                dataOrient[0] = orientSpecific[0];
                                dataOrient[1] = orientSpecific[1];
                                dataOrient[2] = orientSpecific[2];
                            }

                            switch (orientSpecific[0]) {

                                case FileInfoBase.ORI_R2L_TYPE:
                                    Preferences.debug("ORIENT_SPECIFIC X = 0 for ORI_R2L_TYPE\n", Preferences.DEBUG_FILEIO);
                                    if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                                        invertX = false;
                                    }

                                    break;

                                case FileInfoBase.ORI_L2R_TYPE:
                                    Preferences.debug("ORIENT_SPECIFIC X = 1 for ORI_L2R_TYPE\n", Preferences.DEBUG_FILEIO);
                                    if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                                        invertX = true;
                                    }

                                    break;

                                case FileInfoBase.ORI_P2A_TYPE:
                                    Preferences.debug("ORIENT_SPECIFIC X = 2 for ORI_P2A_TYPE\n", Preferences.DEBUG_FILEIO);
                                    if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                                        invertY = true;
                                    }

                                    break;

                                case FileInfoBase.ORI_A2P_TYPE:
                                    Preferences.debug("ORIENT_SPECIFIC X = 3 for ORI_A2P_TYPE\n", Preferences.DEBUG_FILEIO);
                                    if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                                        invertY = false;
                                    }

                                    break;

                                case FileInfoBase.ORI_I2S_TYPE:
                                    Preferences.debug("ORIENT_SPECIFIC X = 4 for ORI_I2S_TYPE\n", Preferences.DEBUG_FILEIO);
                                    if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                                        invertZ = false;
                                    }

                                    break;

                                case FileInfoBase.ORI_S2I_TYPE:
                                    Preferences.debug("ORIENT_SPECIFIC X = 5 for ORI_S2I_TYPE\n", Preferences.DEBUG_FILEIO);
                                    if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                                        invertZ = true;
                                    }

                                    break;
                            }

                            switch (orientSpecific[1]) {

                                case FileInfoBase.ORI_R2L_TYPE:
                                    Preferences.debug("ORIENT_SPECIFIC Y = 0 for ORI_R2L_TYPE\n", Preferences.DEBUG_FILEIO);
                                    if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                                        invertX = false;
                                    }

                                    break;

                                case FileInfoBase.ORI_L2R_TYPE:
                                    Preferences.debug("ORIENT_SPECIFIC Y = 1 for ORI_L2R_TYPE\n", Preferences.DEBUG_FILEIO);
                                    if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                                        invertX = true;
                                    }

                                    break;

                                case FileInfoBase.ORI_P2A_TYPE:
                                    Preferences.debug("ORIENT_SPECIFIC Y = 2 for ORI_P2A_TYPE\n", Preferences.DEBUG_FILEIO);
                                    if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                                        invertY = true;
                                    }

                                    break;

                                case FileInfoBase.ORI_A2P_TYPE:
                                    Preferences.debug("ORIENT_SPECIFIC Y = 3 for ORI_A2P_TYPE\n", Preferences.DEBUG_FILEIO);
                                    if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                                        invertY = false;
                                    }

                                    break;

                                case FileInfoBase.ORI_I2S_TYPE:
                                    Preferences.debug("ORIENT_SPECIFIC Y = 4 for ORI_I2S_TYPE\n", Preferences.DEBUG_FILEIO);
                                    if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                                        invertZ = false;
                                    }

                                    break;

                                case FileInfoBase.ORI_S2I_TYPE:
                                    Preferences.debug("ORIENT_SPECIFIC Y = 5 for ORI_S2I_TYPE\n", Preferences.DEBUG_FILEIO);
                                    if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                                        invertZ = true;
                                    }

                                    break;
                            }

                            switch (orientSpecific[2]) {

                                case FileInfoBase.ORI_R2L_TYPE:
                                    Preferences.debug("ORIENT_SPECIFIC Z = 0 for ORI_R2L_TYPE\n", Preferences.DEBUG_FILEIO);
                                    if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                                        invertX = false;
                                    }

                                    break;

                                case FileInfoBase.ORI_L2R_TYPE:
                                    Preferences.debug("ORIENT_SPECIFIC Z = 1 for ORI_L2R_TYPE\n", Preferences.DEBUG_FILEIO);
                                    if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                                        invertX = true;
                                    }

                                    break;

                                case FileInfoBase.ORI_P2A_TYPE:
                                    Preferences.debug("ORIENT_SPECIFIC Z = 2 for ORI_P2A_TYPE\n", Preferences.DEBUG_FILEIO);
                                    if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                                        invertY = true;
                                    }

                                    break;

                                case FileInfoBase.ORI_A2P_TYPE:
                                    Preferences.debug("ORIENT_SPECIFIC Z = 3 for ORI_A2P_TYPE\n", Preferences.DEBUG_FILEIO);
                                    if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                                        invertY = false;
                                    }

                                    break;

                                case FileInfoBase.ORI_I2S_TYPE:
                                    Preferences.debug("ORIENT_SPECIFIC Z = 4 for ORI_I2S_TYPE\n", Preferences.DEBUG_FILEIO);
                                    if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                                        invertZ = false;
                                    }

                                    break;

                                case FileInfoBase.ORI_S2I_TYPE:
                                    Preferences.debug("ORIENT_SPECIFIC Z = 5 for ORI_S2I_TYPE\n", Preferences.DEBUG_FILEIO);
                                    if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                                        invertZ = true;
                                    }

                                    break;
                            }
                        } // else if (nameString.equalsIgnoreCase("ORIENT_SPECIFIC"))
                        else if (nameString.equalsIgnoreCase("MARKS_FLAG")) {

                            if (countEntries < 2) {
                                Preferences.debug("AFNI Read Header Error: MARKS_FLAG has only " + countEntries +
                                                  " instead of the needed 2\n", Preferences.DEBUG_FILEIO);
                            }

                            if (countEntries >= 1) {

                                switch (intVar[0]) {

                                    case MARKSET_ALIGN:
                                        Preferences.debug("MARKS_FLAG[0] = 1 for MARKSET_ALIGN\n", Preferences.DEBUG_FILEIO);
                                        marksFlag = intVar[0];
                                        break;

                                    case MARKSET_BOUNDING:
                                        Preferences.debug("MARKS_FLAG[0] = 2 for MARKSET_BOUNDING\n", Preferences.DEBUG_FILEIO);
                                        marksFlag = intVar[0];
                                        break;

                                    default:
                                        Preferences.debug("MARKS_FLAG[0] = " + intVar[0] + "\n", Preferences.DEBUG_FILEIO);
                                        marksFlag = -1;
                                }

                                fileInfo.setMarksFlag(marksFlag);
                            } // if (countEntries >= 1)

                            if (countEntries >= 2) {

                                if (intVar[1] != 1) { // value should always be 1
                                    Preferences.debug("MARKS_FLAG[1] = " + intVar[1] + "\n", Preferences.DEBUG_FILEIO);
                                }
                            } // if (countEntries >= 2)
                        } // else if (nameString.equalsIgnoreCase("MARKS_FLAG"))
                        else if (nameString.equalsIgnoreCase("MARKS_FLAGS")) {

                            if (countEntries < 2) {
                                Preferences.debug("AFNI Read Header Error: MARKS_FLAGS has only " + countEntries +
                                                  " instead of the needed 2\n", Preferences.DEBUG_FILEIO);
                            }

                            if (countEntries >= 1) {

                                switch (intVar[0]) {

                                    case MARKSET_ALIGN:
                                        Preferences.debug("MARKS_FLAGS[0] = 1 for MARKSET_ALIGN\n", Preferences.DEBUG_FILEIO);
                                        marksFlags = intVar[0];
                                        break;

                                    case MARKSET_BOUNDING:
                                        Preferences.debug("MARKS_FLAGS[0] = 2 for MARKSET_BOUNDING\n",
                                        		Preferences.DEBUG_FILEIO);
                                        marksFlags = intVar[0];
                                        break;

                                    default:
                                        Preferences.debug("MARKS_FLAGS[0] = " + intVar[0] + "\n", Preferences.DEBUG_FILEIO);
                                        marksFlags = -1;
                                }

                                fileInfo.setMarksFlags(marksFlags);
                            } // if (countEntries >= 1)

                            if (countEntries >= 2) {

                                if (intVar[1] != 1) { // value should always be 1
                                    Preferences.debug("MARKS_FLAGS[1] = " + intVar[1] + "\n", Preferences.DEBUG_FILEIO);
                                }
                            } // if (countEntries >= 2)
                        } // else if (nameString.equalsIgnoreCase("MARKS_FLAGS"))
                        else if (nameString.equalsIgnoreCase("DATASET_RANK")) {

                            if (countEntries < 2) {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: DATASET_RANK has only " + countEntries +
                                                      " instead of the needed 2");
                            }

                            if (intVar[0] != 3) { // number of spatial dimensions must be 3
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: DATASET_RANK[0] illegally = " +
                                                      intVar[0]);
                            }

                            subBrickNumber = intVar[1];
                            Preferences.debug("DATASET_RANK[1](sub-bricks or nval) = " + intVar[1] + "\n",
                            		Preferences.DEBUG_FILEIO);
                        } // else if (nameString.equalsIgnoreCase("DATASET_RANK"))
                        else if (nameString.equalsIgnoreCase("DATASET_DIMENSIONS")) {

                            if (countEntries < 3) {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: DATASET_DIMENSIONS has only " +
                                                      countEntries + " instead of the needed 3");
                            }

                            // Each axis must have at least 2 points
                            xDim = intVar[0];

                            if (xDim < 2) {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: xDim illegally = " + xDim);
                            }

                            Preferences.debug("xDim = " + xDim + "\n", Preferences.DEBUG_FILEIO);
                            yDim = intVar[1];

                            if (yDim < 2) {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: yDim illegally = " + yDim);
                            }

                            Preferences.debug("yDim = " + yDim + "\n", Preferences.DEBUG_FILEIO);
                            zDim = intVar[2];

                            if (zDim < 2) {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: zDim illegally = " + zDim);
                            }

                            Preferences.debug("zDim = " + zDim + "\n", Preferences.DEBUG_FILEIO);

                            if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                                dataExtents[0] = xDim;
                                dataExtents[1] = yDim;
                                dataExtents[2] = zDim;
                            }
                        } // else if (nameString.equalsIgnoreCase("DATASET_DIMENSIONS"))
                        else if (nameString.equalsIgnoreCase("BRICK_TYPES")) {

                            for (i = 0; i < (countEntries - 1); i++) {

                                if (intVar[i] != intVar[i + 1]) {
                                    raFile.close();
                                    throw new IOException("AFNI Read Header Error: Cannot currently handle AFNI files " +
                                                          "with sub-bricks of different data types");
                                }
                            }

                            brickTypeNumber = countEntries;
                            brickType = intVar[0];

                            switch (brickType) {

                                case 0:
                                    fileInfo.setDataType(ModelStorageBase.UBYTE);
                                    Preferences.debug("DataType = UBYTE\n", Preferences.DEBUG_FILEIO);
                                    break;

                                case 1:
                                    fileInfo.setDataType(ModelStorageBase.SHORT);
                                    Preferences.debug("DataType = SHORT\n", Preferences.DEBUG_FILEIO);
                                    break;

                                case 2:
                                    fileInfo.setDataType(ModelStorageBase.INTEGER);
                                    Preferences.debug("DataType = INTEGER\n", Preferences.DEBUG_FILEIO);
                                    break;

                                case 3:
                                    fileInfo.setDataType(ModelStorageBase.FLOAT);
                                    Preferences.debug("DataType = FLOAT\n", Preferences.DEBUG_FILEIO);
                                    break;

                                case 4:
                                    fileInfo.setDataType(ModelStorageBase.DOUBLE);
                                    Preferences.debug("DataType = DOUBLE\n", Preferences.DEBUG_FILEIO);
                                    break;

                                case 5:
                                    fileInfo.setDataType(ModelStorageBase.COMPLEX);
                                    Preferences.debug("DataType = COMPLEX\n", Preferences.DEBUG_FILEIO);
                                    break;

                                case 6:
                                    fileInfo.setDataType(ModelStorageBase.ARGB);
                                    Preferences.debug("DataType = ARGB\n", Preferences.DEBUG_FILEIO);
                                    break;

                                default:
                                    raFile.close();
                                    throw new IOException("AFNI Read Header Error: BRICK_TYPES[0] illegally = " +
                                                          brickType);
                            }
                        } // else if (nameString.equalsIgnoreCase("BRICK_TYPES"))
                        else if (nameString.equalsIgnoreCase("NOTES_COUNT")) {
                            notesCount = intVar[0];
                            Preferences.debug("NOTES_COUNT = " + notesCount + "\n", Preferences.DEBUG_FILEIO);
                        } // else if (nameString.equalsIgnoreCase("NOTES_COUNT"))
                        else if (nameString.equalsIgnoreCase("WARP_TYPE")) {
                            warpType = intVar[0];

                            if ((warpType != WARP_AFFINE_TYPE) && (warpType != WARP_TALAIRACH_12_TYPE)) {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: WARP_TYPE[0] illegally = " + intVar[0]);
                            }

                            switch (warpType) {

                                case 0:
                                    Preferences.debug("WARP_TYPE[0] = 0 for WARP_AFFINE_TYPE\n", Preferences.DEBUG_FILEIO);
                                    break;

                                case 1:
                                    Preferences.debug("WARP_TYPE[0] = 1 for WARP_TALAIRACH_12_TYPE\n", Preferences.DEBUG_FILEIO);
                                    break;
                            }
                        } // else if (nameString.equalsIgnoreCase("WARP_TYPE"))
                        else if (nameString.equalsIgnoreCase("TAXIS_NUMS")) {

                            if (countEntries < 3) {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: TAXIS_NUMS has only " + countEntries +
                                                      " instead of the needed 3");
                            }

                            tDim = intVar[0];

                            if (tDim < 1) {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error:TAXIS_NUMS[0] illegally = " + tDim);
                            }

                            Preferences.debug("tDim = " + tDim + "\n", Preferences.DEBUG_FILEIO);
                            slicesWithTimeOffsets = intVar[1];
                            Preferences.debug("Slices with time offsets = " + slicesWithTimeOffsets + "\n",
                            		Preferences.DEBUG_FILEIO);
                            fileInfo.setSlicesWithTimeOffsets(slicesWithTimeOffsets);
                            timeStepUnit = intVar[2];

                            if ((timeStepUnit != UNITS_MSEC_TYPE) && (timeStepUnit != UNITS_SEC_TYPE) &&
                                    (timeStepUnit != UNITS_HZ_TYPE)) {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: TAXIS_NUMS[2] illegally = " +
                                                      timeStepUnit);
                            }

                            switch (timeStepUnit) {

                                case UNITS_MSEC_TYPE:
                                    fileInfo.setUnitsOfMeasure(Unit.MILLISEC.getLegacyNum(), 3);
                                    Preferences.debug("TAXIS_NUMS[2] = 77001 for UNITS_MSEC_TYPE\n", Preferences.DEBUG_FILEIO);
                                    break;

                                case UNITS_SEC_TYPE:
                                    fileInfo.setUnitsOfMeasure(Unit.SECONDS.getLegacyNum(), 3);
                                    Preferences.debug("TAXIS_NUMS[2] = 77002 for UNITS_SEC_TYPE\n", Preferences.DEBUG_FILEIO);
                                    break;

                                case UNITS_HZ_TYPE:
                                    fileInfo.setUnitsOfMeasure(Unit.HZ.getLegacyNum(), 3);
                                    Preferences.debug("TAXIS_NUMS[2] = 77003 for UNITS_HZ_TYPE\n", Preferences.DEBUG_FILEIO);
                                    break;
                            }
                        } // else if (nameString.equalsIgnoreCase("TAXIS_NUMS"))
                        else if (nameString.equalsIgnoreCase("VOLREG_ROTCOM_NUM")) {
                            volregRotcomNum = intVar[0];
                            Preferences.debug("VOLREG_ROTCOM_NUM = " + volregRotcomNum + "\n", Preferences.DEBUG_FILEIO);
                        } // else if (nameString.equalsIgnoreCase("VOLREG_ROTCOM_NUM"))
                        else if (nameString.equalsIgnoreCase("TO3D_ZPAD")) {

                            if (countEntries < 3) {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: TO3D_ZPAD has only " + countEntries +
                                                      " instead of the needed 3");
                            }

                            zeroPad = new int[3];
                            zeroPad[0] = intVar[0];
                            zeroPad[1] = intVar[1];
                            zeroPad[2] = intVar[2];
                            Preferences.debug("Zero padding x = " + zeroPad[0] + " y = " + zeroPad[1] + " z = " +
                                              zeroPad[2], Preferences.DEBUG_FILEIO);
                        } // else if (nameString.equalsIgnoreCase("TO3D_ZPAD"))
                        else if (nameString.equalsIgnoreCase("TAGSET_NUM")) {

                            if (countEntries < 2) {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: TAGSET_NUM has only " + countEntries +
                                                      " instead of the needed 2");
                            }

                            tagNumber = intVar[0];
                            Preferences.debug("number of tags = " + tagNumber + "\n", Preferences.DEBUG_FILEIO);
                            floatsPerTag = intVar[1];
                            Preferences.debug("Floats per tag = " + floatsPerTag + "\n", Preferences.DEBUG_FILEIO);
                        } // else if (nameString.equalsIgnoreCase("TAGSET_NUM"))
                        else {
                            Preferences.debug(nameString + "\n", Preferences.DEBUG_FILEIO);

                            for (i = 0; i < countEntries; i++) {
                                Preferences.debug("Entry number[" + i + "] = " + intVar[i] + "\n", Preferences.DEBUG_FILEIO);
                            }
                        }
                    } // else if (typeAttribute == INTEGER_ATTRIBUTE)
                    else { // typeAttribute == FLOAT_NUMBER
                        countRead = false;
                        i = 0;
                        lineCounts = 1;

                        while ((!countRead) && (lineCounts > 0)) {
                            floatString = readLine();
                            t = new StringTokenizer(floatString);
                            exceptionOccurred = false;
                            lineCounts = 0;

                            while ((!exceptionOccurred) && (!countRead)) {

                                try {
                                    varString = t.nextToken();
                                    floatVar[i++] = Float.parseFloat(varString);
                                    lineCounts++;

                                    if (i == countEntries) {
                                        countRead = true;
                                    }
                                } catch (NoSuchElementException e) {
                                    exceptionOccurred = true;
                                }
                            } // while ((!exceptionOcurred) && (!countRead))
                        } // while ((!countRead)&& (lineCounts > 0))

                        if (!countRead) {
                            raFile.close();
                            throw new IOException("AFNI Read Header Error: For " + nameString + " read " + i +
                                                  " out of " + countEntries + " floats ");
                        } // if (!countRead)

                        if (nameString.equalsIgnoreCase("ORIGIN")) {

                            if (countEntries < 3) {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: ORIGIN has only " + countEntries +
                                                      " instead of the needed 3");
                            }

                            origin[0] = floatVar[0];
                            origin[1] = floatVar[1];
                            origin[2] = floatVar[2];
                            Preferences.debug("ORIGIN X = " + origin[0] + " Y = " + origin[1] + " Z = " + origin[2] +
                                              "\n", Preferences.DEBUG_FILEIO);
                        } // if (nameString.equalsIgnoreCase("ORIGIN"))
                        else if (nameString.equalsIgnoreCase("DELTA")) {

                            if (countEntries < 3) {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: DELTA has only " + countEntries +
                                                      " instead of the needed 3");
                            }

                            delta[0] = floatVar[0];
                            delta[1] = floatVar[1];
                            delta[2] = floatVar[2];
                            Preferences.debug("DELTA X = " + delta[0] + " Y = " + delta[1] + " Z = " + delta[2] + "\n",
                            		Preferences.DEBUG_FILEIO);
                            imgResols[0] = Math.abs(delta[0]);
                            imgResols[1] = Math.abs(delta[1]);
                            imgResols[2] = Math.abs(delta[2]);

                            if (presentViewType == FileInfoAfni.AFNI_ACPC) {
                                acpcRes = delta[0];
                                tInfo.setAcpcRes(acpcRes);
                            }
                        } // else if (nameString.equalsIgnoreCase("DELTA"))
                        else if (nameString.equalsIgnoreCase("SKIP")) {
                            skip = new float[countEntries];

                            for (i = 0; i < countEntries; i++) {
                                skip[i] = floatVar[i];
                                Preferences.debug("SKIP[" + i + "] = " + skip[i] + "\n", Preferences.DEBUG_FILEIO);
                            }
                        } // else if (nameString.equalsIgnoreCase("SKIP"))
                        else if (nameString.equalsIgnoreCase("MARKS_XYZ")) {

                            if (countEntries < 30) {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: MARKS_XYZ has only " + countEntries +
                                                      " instead of the needed 30");
                            }

                            for (i = 0; i < 30; i++) {
                                marksXYZ[i] = floatVar[i];
                            }
                        } // else if (nameString.equalsIgnoreCase("MARKS_XYZ"))
                        else if (nameString.equalsIgnoreCase("BRICK_FLOAT_FACS")) {
                            brickFloatFacs = new float[countEntries];

                            for (i = 0; i < countEntries; i++) {
                                brickFloatFacs[i] = floatVar[i];

                                if (brickFloatFacs[i] > 0) {
                                    Preferences.debug("BRICKS_FLOAT_FACS[" + i + "] = " + brickFloatFacs[i] +
                                                      " for scaling .BRIK values\n", Preferences.DEBUG_FILEIO);
                                } else if (brickFloatFacs[i] == 0) {
                                    Preferences.debug("BRICK_FLOAT_FACS[" + i + "] = " + brickFloatFacs[i] +
                                                      " indicates unscaled .BRIK values\n", Preferences.DEBUG_FILEIO);
                                } else {
                                    Preferences.debug("BRICK_FLOAT_FACS[" + i + "] = " + brickFloatFacs[i] +
                                                      " is an unexplained negative value\n", Preferences.DEBUG_FILEIO);
                                }
                            }
                        } // else if (nameString.equalsIgnoreCase("BRICK_FLOAT_FACS"))
                        else if (nameString.equalsIgnoreCase("BRICK_STATS")) {

                            if ((countEntries % 2) != 0) {
                                raFile.close();
                                throw new IOException("BRICK_STATS has " + countEntries +
                                                      " values instead of an even number");
                            }

                            brickStats = new float[countEntries];

                            for (i = 0; i < countEntries; i++) {
                                brickStats[i] = floatVar[i];
                            }

                            for (i = 0; i < (countEntries / 2); i++) {
                                Preferences.debug("Sub-brick[" + i + "] has minimum = " + brickStats[2 * i] +
                                                  "  maximum = " + brickStats[(2 * i) + 1] + "\n", Preferences.DEBUG_FILEIO);
                            }
                        } // else if (nameString.equalsIgnoreCase("BRICK_STATS"))
                        else if (nameString.equalsIgnoreCase("WARP_DATA")) {
                            if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                                raFile.close();
                                throw new IOException("ERROR! Should not have WARP_DATA with a +orig file");
                            } else if (presentViewType == FileInfoAfni.AFNI_ACPC) {

                                if (countEntries < 30) {
                                    raFile.close();
                                    throw new IOException("WARP_DATA has only " + countEntries +
                                                          " values instead of the 30 required with an +acpc .HEADER");
                                }
                            } else { // presentViewType == FileInfoAfni.AFNI_TLRC

                                if (countEntries < 360) {
                                    // there seem to be some files with TLRC indicators that have only 30 entries in WARP_DATA, so we won't error out here
                                    if (countEntries == 30) {
                                        //presentViewType = FileInfoAfni.AFNI_ACPC;
                                    } else {
                                        raFile.close();
                                        throw new IOException("WARP_DATA has only " + countEntries + " values instead of the 360 required with a +tlrc .HEADER");
                                    }
                                }
                            }

                            warpData = new float[countEntries];

                            for (i = 0; i < countEntries; i++) {
                                warpData[i] = floatVar[i];
                            }

                            Preferences.debug("WARP_DATA = \n", Preferences.DEBUG_FILEIO);

                            for (i = 0; i < (countEntries / 5); i++) {
                                Preferences.debug(warpData[i * 5] + "\t" + warpData[(i * 5) + 1] + "\t" +
                                                  warpData[(i * 5) + 2] + "\t" + warpData[(i * 5) + 3] + "\t" +
                                                  warpData[(i * 5) + 4] + "\n", Preferences.DEBUG_FILEIO);
                            }
                        } // else if (nameString.equalsIgnoreCase("WARP_DATA"))
                        else if (nameString.equalsIgnoreCase("TAXIS_FLOATS")) {

                            if (countEntries < 5) {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: TAXIS_FLOATS has only " + countEntries +
                                                      " instead of the needed 5");
                            }

                            timeOrigin = floatVar[0];
                            Preferences.debug("Time origin = " + timeOrigin + "\n", Preferences.DEBUG_FILEIO);
                            fileInfo.setTimeOrigin(timeOrigin);
                            timeStep = floatVar[1];
                            Preferences.debug("Time step = " + timeStep + "\n", Preferences.DEBUG_FILEIO);
                            fileInfo.setTimeStep(timeStep);
                            acquisitionDuration = floatVar[2];
                            Preferences.debug("Acqusition duration = " + acquisitionDuration + "\n", Preferences.DEBUG_FILEIO);
                            fileInfo.setAcquisitionDuration(acquisitionDuration);
                            zAxisOffset = floatVar[3];
                            Preferences.debug("z-axis offset for slice-dependent time offsets = " + zAxisOffset + "\n", 
                            		Preferences.DEBUG_FILEIO);
                            fileInfo.setZAxisOffset(zAxisOffset);
                            zAxisStep = floatVar[4];
                            Preferences.debug("z-axis step for slice-dependent time offsets = " + zAxisStep + "\n", Preferences.DEBUG_FILEIO);
                            fileInfo.setZAxisStep(zAxisStep);
                        } // else if (nameString.equalsIgnoreCase("TAXIS_FLOATS"))
                        else if (nameString.equalsIgnoreCase("TAXIS_OFFSETS")) {
                            tAxisOffsets = new float[countEntries];

                            for (i = 0; i < countEntries; i++) {
                                tAxisOffsets[i] = floatVar[i];
                                Preferences.debug("TAXIS_OFFSETS[" + i + "] = " + tAxisOffsets[i] + "\n", Preferences.DEBUG_FILEIO);
                            }

                            fileInfo.setTAxisOffsets(tAxisOffsets);
                        } // else if (nameString.equalsIgnoreCase("TAXIS_OFFSETS"))
                        else if (nameString.equalsIgnoreCase("BRICK_STATAUX")) {
                            brickStatAux = new float[countEntries];
                            i = 0;

                            while (i < countEntries) {
                                subBrickIndex = (int) floatVar[i];
                                brickStatAux[i] = floatVar[i];
                                Preferences.debug("BRICK_STATAUX[" + i + "] = " + subBrickIndex +
                                                  ", the sub-brick index\n", Preferences.DEBUG_FILEIO);
                                i++;
                                statCode = (int) floatVar[i];
                                brickStatAux[i] = floatVar[i];

                                switch (statCode) {

                                    case FUNC_COR_TYPE:
                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + statCode +
                                                          " for FUNC_COR_TYPE with Correlation coeff\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        brickStatAux[i] = floatVar[i];
                                        followingParms = (int) floatVar[i];
                                        if (followingParms != 3) {
                                            raFile.close();
                                            throw new IOException("AFNI Read Header Error: BRICK_STATAUX[ " + i +
                                                                  "] = " + followingParms +
                                                                  ", but for FUNC_COR_TYPE 3 parameters should follow\n");
                                        }

                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + followingParms +
                                                          " for number of parameters that follow\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        brickStatAux[i] = floatVar[i];
                                        numSamples = (int) floatVar[i];
                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + numSamples +
                                                          ", the number of samples\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        brickStatAux[i] = floatVar[i];
                                        numFitParam = (int) floatVar[i];
                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + numFitParam +
                                                          ", the number of fitting parameters\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        brickStatAux[i] = floatVar[i];
                                        numNuisanceParam = (int) floatVar[i];
                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + numNuisanceParam +
                                                          ", the number of nuisance parameters\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        break;

                                    case FUNC_TT_TYPE:
                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + statCode +
                                                          " for FUNC_TT_TYPE with Student t\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        brickStatAux[i] = floatVar[i];
                                        followingParms = (int) floatVar[i];
                                        if (followingParms != 1) {
                                            raFile.close();
                                            throw new IOException("AFNI Read Header Error: BRICK_STATAUX[" + i +
                                                                  "] = " + followingParms +
                                                                  ", but for FUNC_TT_TYPE 1 parameter should follow\n");
                                        }

                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + followingParms +
                                                          " for number of parameters that follow\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        brickStatAux[i] = floatVar[i];
                                        dof = (int) floatVar[i];
                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + dof +
                                                          " for degrees of freedom\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        break;

                                    case FUNC_FT_TYPE:
                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + statCode +
                                                          " for FUNC_FT_TYPE with F ratio\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        brickStatAux[i] = floatVar[i];
                                        followingParms = (int) floatVar[i];
                                        if (followingParms != 2) {
                                            raFile.close();
                                            throw new IOException("AFNI Read Header Error: BRICK_STATAUX[" + i +
                                                                  "] = " + followingParms +
                                                                  ", but for FUNC_FT_TYPE 2 parameters should follow\n");
                                        }

                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + followingParms +
                                                          " for number of parameters that follow\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        brickStatAux[i] = floatVar[i];
                                        ndof = (int) floatVar[i];
                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + ndof +
                                                          " for numerator degrees of freedom\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        brickStatAux[i] = floatVar[i];
                                        ddof = (int) floatVar[i];
                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + ddof +
                                                          " for denominator degrees of freedom\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        break;

                                    case FUNC_ZT_TYPE:
                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + statCode +
                                                          " for FUNC_ZT_TYPE with Standard Normal\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        brickStatAux[i] = floatVar[i];
                                        followingParms = (int) floatVar[i];
                                        if (followingParms != 0) {
                                            raFile.close();
                                            throw new IOException("AFNI Read Header Error: BRICK_STATAUX[" + i +
                                                                  "] = " + followingParms +
                                                                  ", but for FUNC_ZT_TYPE 0 parameters should follow\n");
                                        }

                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + followingParms +
                                                          " for number of parameters that follow\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        break;

                                    case FUNC_CT_TYPE:
                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + statCode +
                                                          " for FUNC_CT_TYPE with Chi-squared\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        brickStatAux[i] = floatVar[i];
                                        followingParms = (int) floatVar[i];
                                        if (followingParms != 1) {
                                            raFile.close();
                                            throw new IOException("AFNI Read Header Error: BRICK_STATAUX[" + i +
                                                                  "] = " + followingParms +
                                                                  ", but for FUNC_CT_TYPE 1 parameter should follow\n");
                                        }

                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + followingParms +
                                                          " for number of parameters that follow\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        brickStatAux[i] = floatVar[i];
                                        dof = (int) floatVar[i];
                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + dof +
                                                          " for degrees of freedom\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        break;

                                    case FUNC_BT_TYPE:
                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + statCode +
                                                          " for FUNC_BT_TYPE with Incomplete Beta\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        brickStatAux[i] = floatVar[i];
                                        followingParms = (int) floatVar[i];
                                        if (followingParms != 2) {
                                            raFile.close();
                                            throw new IOException("AFNI Read Header Error: BRICK_STATAUX[" + i +
                                                                  "] = " + followingParms +
                                                                  ", but for FUNC_BT_TYPE 2 parameters should follow\n");
                                        }

                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + followingParms +
                                                          " for number of parameters that follow\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        brickStatAux[i] = floatVar[i];
                                        //a = floatVar[i];
                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + brickStatAux[i] +
                                                          " for parameter a\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        brickStatAux[i] = floatVar[i];
                                        //b = floatVar[i];
                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + brickStatAux[i] +
                                                          " for parameter b\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        break;

                                    case FUNC_BN_TYPE:
                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + statCode +
                                                          " for FUNC_BN_TYPE with Binomial\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        brickStatAux[i] = floatVar[i];
                                        followingParms = (int) floatVar[i];
                                        if (followingParms != 2) {
                                            raFile.close();
                                            throw new IOException("AFNI Read Header Error: BRICK_STATAUX[" + i +
                                                                  "] = " + followingParms +
                                                                  ", but for FUNC_BN_TYPE 2 parameters should follow\n");
                                        }

                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + followingParms +
                                                          " for number of parameters that follow\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        brickStatAux[i] = floatVar[i];
                                        numTrials = (int) floatVar[i];
                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + numTrials +
                                                          " number of trials\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        brickStatAux[i] = floatVar[i];
                                        //prob = floatVar[i];
                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + brickStatAux[i] +
                                                          " for probability per trial\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        break;

                                    case FUNC_GT_TYPE:
                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + statCode +
                                                          " for FUNC_GT_TYPE with Gamma\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        brickStatAux[i] = floatVar[i];
                                        followingParms = (int) floatVar[i];
                                        if (followingParms != 2) {
                                            raFile.close();
                                            throw new IOException("AFNI Read Header Error: BRICK_STATAUX[" + i +
                                                                  "] = " + followingParms +
                                                                  ", but for FUNC_GT_TYPE 2 parameters should follow\n");
                                        }

                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + followingParms +
                                                          " for number of parameters that follow\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        brickStatAux[i] = floatVar[i];
                                        //shape = floatVar[i];
                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + brickStatAux[i] +
                                                          " for shape\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        brickStatAux[i] = floatVar[i];
                                        //scale = floatVar[i];
                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + brickStatAux[i] +
                                                          " for scale\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        break;

                                    case FUNC_PT_TYPE:
                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + statCode +
                                                          " for FUNC_PT_TYPE with Poisson\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        brickStatAux[i] = floatVar[i];
                                        followingParms = (int) floatVar[i];
                                        if (followingParms != 1) {
                                            raFile.close();
                                            throw new IOException("AFNI Read Header Error: BRICK_STATAUX[" + i +
                                                                  "] = " + followingParms +
                                                                  ", but for FUNC_PT_TYPE 1 parameter should follow\n");
                                        }

                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + followingParms +
                                                          " for number of parameters that follow\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        brickStatAux[i] = floatVar[i];
                                        //mean = floatVar[i];
                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + brickStatAux[i] +
                                                          " for mean\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        break;

                                    default:
                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + statCode +
                                                          " for unknown FUNC_TYPE\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        brickStatAux[i] = floatVar[i];
                                        followingParms = (int) floatVar[i];
                                        Preferences.debug("BRICK_STATAUX[" + i + "] = " + followingParms +
                                                          " for number of parameters that follow\n", Preferences.DEBUG_FILEIO);
                                        i++;
                                        for (j = 0; j < followingParms; j++) {
                                            brickStatAux[i] = floatVar[i];
                                            Preferences.debug("BRICK_STATAUX[" + i + "] = " + brickStatAux[i] + "\n", Preferences.DEBUG_FILEIO);
                                            i++;
                                        } // for (j = 0; j < followingParms; j++)

                                        break;
                                } // switch(brickStatAux[i])
                            } // while (i < countEntries)

                            fileInfo.setBrickStatAux(brickStatAux);
                        } // else if (nameString.equalsIgnoreCase("BRICK_STATAUX"))
                        else if (nameString.equalsIgnoreCase("STAT_AUX")) {
                            statAux = new float[countEntries];

                            for (i = 0; i < countEntries; i++) {
                                statAux[i] = floatVar[i];
                                Preferences.debug("statAux[" + i + "] = " + statAux[i] + "\n", Preferences.DEBUG_FILEIO);
                            } // for (i = 0; i < countEntries; i++)

                            fileInfo.setStatAux(statAux);
                        } // else if (nameString.equalsIgnoreCase("STAT_AUX"))
                        else if (nameString.equalsIgnoreCase("TAGALIGN_MATVEC")) {

                            if (countEntries < 12) {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: TAGALIGN_MATVEC has only " +
                                                      countEntries + " instead of the needed 12");
                            }

                            tagAlignMatvec = new float[12];

                            for (i = 0; i < 12; i++) {
                                tagAlignMatvec[i] = floatVar[i];
                            }

                            Preferences.debug("TAGALIGN_MATVEC = \n", Preferences.DEBUG_FILEIO);
                            Preferences.debug(tagAlignMatvec[0] + "\t" + tagAlignMatvec[1] + "\t" + tagAlignMatvec[2] +
                                              "\t" + tagAlignMatvec[3] + "\t" + tagAlignMatvec[4] + "\t" +
                                              tagAlignMatvec[5] + "n", Preferences.DEBUG_FILEIO);
                            Preferences.debug(tagAlignMatvec[6] + "\t" + tagAlignMatvec[7] + "\t" + tagAlignMatvec[8] +
                                              "\t" + tagAlignMatvec[9] + "\t" + tagAlignMatvec[10] + "\t" +
                                              tagAlignMatvec[11] + "n", Preferences.DEBUG_FILEIO);
                        } // else if (nameString.equalsIgnoreCase("TAGALIGN_MATVEC"))
                        else if (nameString.substring(0, nameString.length() - 6).equalsIgnoreCase("VOLREG_MATVEC_")) {

                            if (countEntries < 12) {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: " + nameString + " has only " +
                                                      countEntries + " instead of the needed 12");
                            }

                            tmpString = nameString.substring(nameString.length() - 6);

                            if (tmpString.equals("000000")) {
                                volregMatvec000000 = new float[12];

                                for (i = 0; i < 12; i++) {
                                    volregMatvec000000[i] = floatVar[i];
                                }
                            } // if (tmpString.equals("000000"))
                            else if (tmpString.equals("000001")) {
                                volregMatvec000001 = new float[12];

                                for (i = 0; i < 12; i++) {
                                    volregMatvec000001[i] = floatVar[i];
                                }
                            } // else if (tmpString.equals("000001"))
                            else if (tmpString.equals("000002")) {
                                volregMatvec000002 = new float[12];

                                for (i = 0; i < 12; i++) {
                                    volregMatvec000002[i] = floatVar[i];
                                }
                            } // else if (tmpString.equals("000002"))
                            else if (tmpString.equals("000003")) {
                                volregMatvec000003 = new float[12];

                                for (i = 0; i < 12; i++) {
                                    volregMatvec000003[i] = floatVar[i];
                                }
                            } // else if (tmpString.equals("000003"))
                            else if (tmpString.equals("000004")) {
                                volregMatvec000004 = new float[12];

                                for (i = 0; i < 12; i++) {
                                    volregMatvec000004[i] = floatVar[i];
                                }
                            } // else if (tmpString.equals("000004"))
                            else if (tmpString.equals("000005")) {
                                volregMatvec000005 = new float[12];

                                for (i = 0; i < 12; i++) {
                                    volregMatvec000005[i] = floatVar[i];
                                }
                            } // else if (tmpString.equals("000005"))
                            else if (tmpString.equals("000006")) {
                                volregMatvec000006 = new float[12];

                                for (i = 0; i < 12; i++) {
                                    volregMatvec000006[i] = floatVar[i];
                                }
                            } // else if (tmpString.equals("000006"))
                            else if (tmpString.equals("000007")) {
                                volregMatvec000007 = new float[12];

                                for (i = 0; i < 12; i++) {
                                    volregMatvec000007[i] = floatVar[i];
                                }
                            } // else if (tmpString.equals("000007"))
                            else if (tmpString.equals("000008")) {
                                volregMatvec000008 = new float[12];

                                for (i = 0; i < 12; i++) {
                                    volregMatvec000008[i] = floatVar[i];
                                }
                            } // else if (tmpString.equals("000008"))
                            else if (tmpString.equals("000009")) {
                                volregMatvec000009 = new float[12];

                                for (i = 0; i < 12; i++) {
                                    volregMatvec000009[i] = floatVar[i];
                                }
                            } // else if (tmpString.equals("000009"))

                            Preferences.debug(nameString + " = \n", Preferences.DEBUG_FILEIO);
                            Preferences.debug(floatVar[0] + "\t" + floatVar[1] + "\t" + floatVar[2] + "\t" +
                                              floatVar[3] + "\t" + floatVar[4] + "\t" + floatVar[5] + "n", Preferences.DEBUG_FILEIO);
                            Preferences.debug(floatVar[6] + "\t" + floatVar[7] + "\t" + floatVar[8] + "\t" +
                                              floatVar[9] + "\t" + floatVar[10] + "\t" + floatVar[11] + "n", Preferences.DEBUG_FILEIO);
                        } // else if (nameString.substring(0,nameString.length() - 6).equalsIgnoreCase
                          // ("VOLREG_MATVEC_"))
                        else if (nameString.equalsIgnoreCase("VOLREG_CENTER_OLD")) {

                            if (countEntries < 3) {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: VOLREG_CENTER_OLD has only " +
                                                      countEntries + " instead of the needed 3");
                            }

                            volregCenterOld = new float[3];

                            for (i = 0; i < 3; i++) {
                                volregCenterOld[i] = floatVar[i];
                            }

                            Preferences.debug("VOLREG_CENTER_OLD X = " + volregCenterOld[0] + "  Y = " +
                                              volregCenterOld[1] + "  Z = " + volregCenterOld[2] + "\n", Preferences.DEBUG_FILEIO);
                        } // else if (nameString.equalsIgnorecase("VOLREG_CENTER_OLD"))
                        else if (nameString.equalsIgnoreCase("VOLREG_CENTER_BASE")) {

                            if (countEntries < 3) {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: VOLREG_CENTER_BASE has only " +
                                                      countEntries + " instead of the needed 3");
                            }

                            volregCenterBase = new float[3];

                            for (i = 0; i < 3; i++) {
                                volregCenterBase[i] = floatVar[i];
                            }

                            Preferences.debug("VOLREG_CENTER_BASE X = " + volregCenterBase[0] + "  Y = " +
                                              volregCenterBase[1] + "  Z = " + volregCenterBase[2] + "\n", Preferences.DEBUG_FILEIO);
                        } // else if (nameString.equalsIgnorecase("VOLREG_CENTER_BASE"))
                        else if (nameString.equalsIgnoreCase("TAGSET_FLOATS")) {

                            if ((countEntries % 5) != 0) {
                                raFile.close();
                                throw new IOException("AFNI Read Header Error: TAGSET_FLOATS has " + countEntries +
                                                      " values, which is not a multiple of 5");
                            }

                            tagsetFloats = new float[countEntries];

                            for (i = 0; i < countEntries; i++) {
                                tagsetFloats[i] = floatVar[i];
                            }

                            Preferences.debug("TAGSET_FLOATS = \n", Preferences.DEBUG_FILEIO);

                            for (i = 0; i < (countEntries / 5); i++) {
                                tagnum = (int) tagsetFloats[(5 * i) + 3];
                                index = (int) tagsetFloats[(5 * i) + 4];

                                if (index >= 0) {
                                    Preferences.debug("x = " + tagsetFloats[5 * i] + "  y = " +
                                                      tagsetFloats[(5 * i) + 1] + "  z = " + tagsetFloats[(5 * i) + 2] +
                                                      "  tag numerical value = " + tagnum +
                                                      "  sub-brick index of tag = " + index + "\n", Preferences.DEBUG_FILEIO);
                                } else { // index < 0
                                    Preferences.debug("x = " + tagsetFloats[5 * i] + "  y = " +
                                                      tagsetFloats[(5 * i) + 1] + "  z = " + tagsetFloats[(5 * i) + 2] +
                                                      "  tag numerical value = " + tagnum + "  not set flag = " +
                                                      index + "\n", Preferences.DEBUG_FILEIO);
                                }
                            }
                        } // else if (nameString.equalsIgnoreCase("TAGSET_FLOATS"))
                        else {
                            Preferences.debug(nameString + " is an unknown float with arguments: \n",
                            		Preferences.DEBUG_FILEIO);

                            for (i = 0; i < countEntries; i++) {
                                Preferences.debug(floatVar[i] + "\n", Preferences.DEBUG_FILEIO);
                            }
                        }

                    } // typeAttribute == FLOAT_NUMBER
                } // if (!restart)
            } // while(!done)

            raFile.close();

            if ((typeString.equalsIgnoreCase("3DIM_HEAD_ANAT")) && (typeStringType != FileInfoAfni.HEAD_ANAT_TYPE)) {
                throw new IOException("AFNI Read Header Error: typeString = " + typeString + " but typeStringType = " +
                                      typeStringType);
            }

            if ((typeString.equalsIgnoreCase("3DIM_GEN_ANAT")) && (typeStringType != FileInfoAfni.GEN_ANAT_TYPE)) {
                throw new IOException("AFNI Read Header Error: typeString = " + typeString + " but typeStringType = " +
                                      typeStringType);
            }

            if ((typeString.equalsIgnoreCase("3DIM_HEAD_FUNC")) && (typeStringType != FileInfoAfni.HEAD_FUNC_TYPE)) {
                throw new IOException("AFNI Read Header Error: typeString = " + typeString + " but typeStringType = " +
                                      typeStringType);
            }

            if ((typeString.equalsIgnoreCase("3DIM_GEN_FUNC")) && (typeStringType != FileInfoAfni.GEN_FUNC_TYPE)) {
                throw new IOException("AFNI Read Header Error: typeString = " + typeString + " but typeStringType = " +
                                      typeStringType);
            }

            if (subBrickNumber != brickTypeNumber) {
                throw new IOException("AFNI Read Header Error: DATASET_RANK[1] = " + subBrickNumber +
                                      ", but BRICK_TYPES has " + brickTypeNumber + " values");
            }

            if (slicesWithTimeOffsets > 0) {

                if (tAxisOffsets == null) {
                    throw new IOException("AFNI Read Header Error: slicesWithTimeOffsets = " + slicesWithTimeOffsets +
                                          ", but tAxisOffsets is null");
                } else if (tAxisOffsets.length != slicesWithTimeOffsets) {
                    throw new IOException("AFNI Read Header Error: slicesWithTimeOffsets = " + slicesWithTimeOffsets +
                                          ", but tAxisOffsets.length = " + tAxisOffsets.length);
                }
            } // if (slicesWithTimeOffsets > 0)

            switch (viewType) {

                case 0:
                    Preferences.debug("SCENE_DATA[0] = 0 for +orig\n", Preferences.DEBUG_FILEIO);
                    break;

                case 1:
                    Preferences.debug("SCENE_DATA[0] = 1 for +acpc\n", Preferences.DEBUG_FILEIO);
                    break;

                case 2:
                    Preferences.debug("SCENE_DATA[0] = 2 for +tlrc\n", Preferences.DEBUG_FILEIO);
                    break;
            }

            if (anatType) {

                switch (funcType) {

                    case 0:
                        Preferences.debug("SCENCE_DATA[1] = 0 for ANAT_SPGR_TYPE\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 1:
                        Preferences.debug("SCENCE_DATA[1] = 1 for ANAT_FSE_TYPE\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 2:
                        Preferences.debug("SCENE_DATA[1] = 2 for ANAT_EPI_TYPE\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 3:
                        Preferences.debug("SCENE_DATA[1] = 3 for ANAT_MRAN_TYPE\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 4:
                        Preferences.debug("SCENE_DATA[1] = 4 for ANAT_CT_TYPE\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 5:
                        Preferences.debug("SCENE_DATA[1] = 5 for ANAT_SPECT_TYPE\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 6:
                        Preferences.debug("SCENE_DATA[1] = 6 for ANAT_PET_TYPE\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 7:
                        Preferences.debug("SCENE_DATA[1] = 7 for ANAT_MRA_TYPE\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 8:
                        Preferences.debug("SCENE_DATA[1] = 8 for ANAT_BMAP_TYPE\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 9:
                        Preferences.debug("SCENE_DATA[1] = 9 for ANAT_DIFF_TYPE\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 10:
                        Preferences.debug("SCENE_DATA[1] = 10 for ANAT_OMRI_TYPE\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 11:
                        Preferences.debug("SCENE_DATA[1] = 11 for ANAT_BUCK_TYPE\n", Preferences.DEBUG_FILEIO);
                        break;

                    default:
                        Preferences.debug("SCENE_DATA[1] = " + funcType + " for unknown ANAT_TYPE\n", Preferences.DEBUG_FILEIO);
                        break;
                }
            } // if (anatType)
            else { // not anatType

                switch (funcType) {

                    case 0:
                        Preferences.debug("SCENCE_DATA[1] = 0 for 1 valued FUNC_FIM_TYPE\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 1:
                        Preferences.debug("SCENCE_DATA[1] = 1 for obsolete FUNC_THR_TYPE\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 2:
                        Preferences.debug("SCENCE_DATA[1] = 2 for correlation FUNC_COR_TYPE\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 3:
                        Preferences.debug("SCENCE_DATA[1] = 3 for t-statistic FUNC_TT_TYPE\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 4:
                        Preferences.debug("SCENCE_DATA[1] = 4 for F-statistic FUNC_FT_TYPE\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 5:
                        Preferences.debug("SCENCE_DATA[1] = 5 for z-score FUNC_ZT_TYPE\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 6:
                        Preferences.debug("SCENE_DATA[1] = 6 for Chi-squared FUNC_CT_TYPE\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 7:
                        Preferences.debug("SCENC_DATA[1] = 7 for Beta stat FUNC_BT_TYPE\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 8:
                        Preferences.debug("SCENE_DATA[1] = 8 for Binomaial FUNC_BN_TYPE\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 9:
                        Preferences.debug("SCENE_DATA[1] = 9 for Gamma FUNC_GT_TYPE\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 10:
                        Preferences.debug("SCENE_DATA[1] = 10 for Poisson FUNC_PT_TYPE\n", Preferences.DEBUG_FILEIO);
                        break;

                    case 11:
                        Preferences.debug("SCENE_DATA[1] = 11 for bucket FUNC_BUCK_TYPE\n", Preferences.DEBUG_FILEIO);
                        break;

                    default:
                        Preferences.debug("SCENCE_DATA[1] = " + funcType + " for unknown FUNC_TYPE\n", 
                        		Preferences.DEBUG_FILEIO);
                        break;
                }
            } // not anatType

            switch (typeStringType) {

                case 0:
                    Preferences.debug("SCENCE_DATA[2] = 0 for FileInfoAfni.HEAD_ANAT_TYPE\n", Preferences.DEBUG_FILEIO);
                    break;

                case 1:
                    Preferences.debug("SCENE_DATA[2] = 1 for FileInfoAfni.HEAD_FUNC_TYPE\n", Preferences.DEBUG_FILEIO);
                    break;

                case 2:
                    Preferences.debug("SCENE_DATA[2] = 2 for FileInfoAfni.GEN_ANAT_TYPE\n", Preferences.DEBUG_FILEIO);
                    break;

                case 3:
                    Preferences.debug("SCENE_DATA[2] = 3 for FileInfoAfni.GEN_FUNC_TYPE\n", Preferences.DEBUG_FILEIO);
                    break;
            }

            if (!((presentViewType == FileInfoAfni.AFNI_ACPC) && (alsoAcpc))) {

                // The origin is in a voxel center but the legal dataset bounds extend to the voxel edge
                if ((orientSpecific[0] == FileInfoBase.ORI_R2L_TYPE) ||
                        (orientSpecific[0] == FileInfoBase.ORI_A2P_TYPE) ||
                        (orientSpecific[0] == FileInfoBase.ORI_I2S_TYPE)) {
                    lowestX = origin[0];
                    highestX = origin[0] + ((xDim - 1.0f) * delta[0]);
                } else {
                    highestX = origin[0];
                    lowestX = origin[0] + ((xDim - 1.0f) * delta[0]);
                }

                if ((orientSpecific[1] == FileInfoBase.ORI_R2L_TYPE) ||
                        (orientSpecific[1] == FileInfoBase.ORI_A2P_TYPE) ||
                        (orientSpecific[1] == FileInfoBase.ORI_I2S_TYPE)) {
                    lowestY = origin[1];
                    highestY = origin[1] + ((yDim - 1.0f) * delta[1]);
                } else {
                    highestY = origin[1];
                    lowestY = origin[1] + ((yDim - 1.0f) * delta[1]);
                }

                if ((orientSpecific[2] == FileInfoBase.ORI_R2L_TYPE) ||
                        (orientSpecific[2] == FileInfoBase.ORI_A2P_TYPE) ||
                        (orientSpecific[2] == FileInfoBase.ORI_I2S_TYPE)) {
                    lowestZ = origin[2];
                    highestZ = origin[2] + ((zDim - 1.0f) * delta[2]);
                } else {
                    highestZ = origin[2];
                    lowestZ = origin[2] + ((zDim - 1.0f) * delta[2]);
                }

                Preferences.debug("lowestX = " + lowestX + " highestX = " + highestX + "\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("lowestY = " + lowestY + " highestY = " + highestY + "\n", Preferences.DEBUG_FILEIO);
                Preferences.debug("lowestZ = " + lowestZ + " highestZ = " + highestZ + "\n", Preferences.DEBUG_FILEIO);

                // Check to see which of the marksXYZ are within the bounded box of the dataset
                // First obtain dicom ordered origin and delta
                switch (orientSpecific[0]) {

                    case FileInfoBase.ORI_R2L_TYPE:
                        dicomOrigin[0] = origin[0];
                        dicomDelta[0] = delta[0];
                        dicomXDim = xDim;
                        if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                            origOrigin[0] = origin[0];
                            origDelta[0] = delta[0];
                            origXDim = xDim;
                            dicomLowestX = lowestX;
                            dicomHighestX = highestX;
                        }

                        break;

                    case FileInfoBase.ORI_L2R_TYPE:
                        dicomOrigin[0] = origin[0] + ((xDim - 1) * delta[0]);
                        dicomDelta[0] = Math.abs(delta[0]);
                        dicomXDim = xDim;
                        if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                            origOrigin[0] = origin[0];
                            origDelta[0] = delta[0];
                            origXDim = xDim;
                            dicomLowestX = lowestX;
                            dicomHighestX = highestX;
                        }

                        break;

                    case FileInfoBase.ORI_A2P_TYPE:
                        dicomOrigin[1] = origin[0];
                        dicomDelta[1] = delta[0];
                        dicomYDim = xDim;
                        if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                            origOrigin[1] = origin[0];
                            origDelta[1] = delta[0];
                            origYDim = xDim;
                            dicomLowestY = lowestX;
                            dicomHighestY = highestX;
                        }

                        break;

                    case FileInfoBase.ORI_P2A_TYPE:
                        dicomOrigin[1] = origin[0] + ((xDim - 1) * delta[0]);
                        dicomDelta[1] = Math.abs(delta[0]);
                        dicomYDim = xDim;
                        if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                            origOrigin[1] = origin[0];
                            origDelta[1] = delta[0];
                            origYDim = xDim;
                            dicomLowestY = lowestX;
                            dicomHighestY = highestX;
                        }

                        break;

                    case FileInfoBase.ORI_I2S_TYPE:
                        dicomOrigin[2] = origin[0];
                        dicomDelta[2] = delta[0];
                        dicomZDim = xDim;
                        if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                            origOrigin[2] = origin[0];
                            origDelta[2] = delta[0];
                            origZDim = xDim;
                            dicomLowestZ = lowestX;
                            dicomHighestZ = highestX;
                        }

                        break;

                    case FileInfoBase.ORI_S2I_TYPE:
                        dicomOrigin[2] = origin[0] + ((xDim - 1) * delta[0]);
                        dicomDelta[2] = Math.abs(delta[0]);
                        dicomZDim = xDim;
                        if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                            origOrigin[2] = origin[0];
                            origDelta[2] = delta[0];
                            origZDim = xDim;
                            dicomLowestZ = lowestX;
                            dicomHighestZ = highestX;
                        }

                        break;
                }

                switch (orientSpecific[1]) {

                    case FileInfoBase.ORI_R2L_TYPE:
                        dicomOrigin[0] = origin[1];
                        dicomDelta[0] = delta[1];
                        dicomXDim = yDim;
                        if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                            origOrigin[0] = origin[1];
                            origDelta[0] = delta[1];
                            origXDim = yDim;
                            dicomLowestX = lowestY;
                            dicomHighestX = highestY;
                        }

                        break;

                    case FileInfoBase.ORI_L2R_TYPE:
                        dicomOrigin[0] = origin[1] + ((yDim - 1) * delta[1]);
                        dicomDelta[0] = Math.abs(delta[1]);
                        dicomXDim = yDim;
                        if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                            origOrigin[0] = origin[1];
                            origDelta[0] = delta[1];
                            origXDim = yDim;
                            dicomLowestX = lowestY;
                            dicomHighestX = highestY;
                        }

                        break;

                    case FileInfoBase.ORI_A2P_TYPE:
                        dicomOrigin[1] = origin[1];
                        dicomDelta[1] = delta[1];
                        dicomYDim = yDim;
                        if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                            origOrigin[1] = origin[1];
                            origDelta[1] = delta[1];
                            origYDim = yDim;
                            dicomLowestY = lowestY;
                            dicomHighestY = highestY;
                        }

                        break;

                    case FileInfoBase.ORI_P2A_TYPE:
                        dicomOrigin[1] = origin[1] + ((yDim - 1) * delta[1]);
                        dicomDelta[1] = Math.abs(delta[1]);
                        dicomYDim = yDim;
                        if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                            origOrigin[1] = origin[1];
                            origDelta[1] = delta[1];
                            origYDim = yDim;
                            dicomLowestY = lowestY;
                            dicomHighestY = highestY;
                        }

                        break;

                    case FileInfoBase.ORI_I2S_TYPE:
                        dicomOrigin[2] = origin[1];
                        dicomDelta[2] = delta[1];
                        dicomZDim = yDim;
                        if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                            origOrigin[2] = origin[1];
                            origDelta[2] = delta[1];
                            origZDim = yDim;
                            dicomLowestZ = lowestY;
                            dicomHighestZ = highestY;
                        }

                        break;

                    case FileInfoBase.ORI_S2I_TYPE:
                        dicomOrigin[2] = origin[1] + ((yDim - 1) * delta[1]);
                        dicomDelta[2] = Math.abs(delta[1]);
                        dicomZDim = yDim;
                        if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                            origOrigin[2] = origin[1];
                            origDelta[2] = delta[1];
                            origZDim = yDim;
                            dicomLowestZ = lowestY;
                            dicomHighestZ = highestY;
                        }

                        break;
                }

                switch (orientSpecific[2]) {

                    case FileInfoBase.ORI_R2L_TYPE:
                        dicomOrigin[0] = origin[2];
                        dicomDelta[0] = delta[2];
                        dicomXDim = zDim;
                        if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                            origOrigin[0] = origin[2];
                            origDelta[0] = delta[2];
                            origXDim = zDim;
                            dicomLowestX = lowestZ;
                            dicomHighestX = highestZ;
                        }

                        break;

                    case FileInfoBase.ORI_L2R_TYPE:
                        dicomOrigin[0] = origin[2] + ((zDim - 1) * delta[2]);
                        dicomDelta[0] = Math.abs(delta[2]);
                        dicomXDim = zDim;
                        if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                            origOrigin[0] = origin[2];
                            origDelta[0] = delta[2];
                            origXDim = zDim;
                            dicomLowestX = lowestZ;
                            dicomHighestX = highestZ;
                        }

                        break;

                    case FileInfoBase.ORI_A2P_TYPE:
                        dicomOrigin[1] = origin[2];
                        dicomDelta[1] = delta[2];
                        dicomYDim = zDim;
                        if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                            origOrigin[1] = origin[2];
                            origDelta[1] = delta[2];
                            origYDim = zDim;
                            dicomLowestY = lowestZ;
                            dicomHighestY = highestZ;
                        }

                        break;

                    case FileInfoBase.ORI_P2A_TYPE:
                        dicomOrigin[1] = origin[2] + ((zDim - 1) * delta[2]);
                        dicomDelta[1] = Math.abs(delta[2]);
                        dicomYDim = zDim;
                        if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                            origOrigin[1] = origin[2];
                            origDelta[1] = delta[2];
                            origYDim = zDim;
                            dicomLowestY = lowestZ;
                            dicomHighestY = highestZ;
                        }

                        break;

                    case FileInfoBase.ORI_I2S_TYPE:
                        dicomOrigin[2] = origin[2];
                        dicomDelta[2] = delta[2];
                        dicomZDim = zDim;
                        if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                            origOrigin[2] = origin[2];
                            origDelta[2] = delta[2];
                            origZDim = zDim;
                            dicomLowestZ = lowestZ;
                            dicomHighestZ = highestZ;
                        }

                        break;

                    case FileInfoBase.ORI_S2I_TYPE:
                        dicomOrigin[2] = origin[2] + ((zDim - 1) * delta[2]);
                        dicomDelta[2] = Math.abs(delta[2]);
                        dicomZDim = zDim;
                        if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                            origOrigin[2] = origin[2];
                            origDelta[2] = delta[2];
                            origZDim = zDim;
                            dicomLowestZ = lowestZ;
                            dicomHighestZ = highestZ;
                        }

                        break;
                }

                if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                    origExtents[0] = origXDim;
                    origExtents[1] = origYDim;
                    origExtents[2] = origZDim;
                }

                // A dialog has the user choose between dataset and dicom order
                if ((viewType == FileInfoAfni.AFNI_ACPC) || (viewType == FileInfoAfni.AFNI_TLRC)) {
                    doDicom = true;
                } else if (loadB) {
                    doDicom = false;
                } else {

                    if ((orientSpecific[0] != FileInfoBase.ORI_R2L_TYPE) ||
                            (orientSpecific[1] != FileInfoBase.ORI_A2P_TYPE) ||
                            (orientSpecific[2] != FileInfoBase.ORI_I2S_TYPE)) {
                        origDicom = false;
                        createOrderDialog();
                    } else {
                        origDicom = true;
                    }

                    fileInfo.setOrigDicom(origDicom);
                }
            } // if (!((presentViewType == FileInfoAfni.AFNI_ACPC) && (alsoAcpc)))


            for (i = 0; i < 10; i++) {

                if ((marksXYZ[3 * i] >= lowestX) && (marksXYZ[3 * i] <= highestX) &&
                        (marksXYZ[(3 * i) + 1] >= lowestY) && (marksXYZ[(3 * i) + 1] <= highestY) &&
                        (marksXYZ[(3 * i) + 2] >= lowestZ) && (marksXYZ[(3 * i) + 2] <= highestZ) &&
                        (marksLabString[i] != null)) { // marker within bounds

                    // points for +orig to +acpc transformation
                    if ((marksLabString[i]).equalsIgnoreCase("AC superior edge")) {

                        if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                            Preferences.debug(marksLabString[i] + " at X = " + marksXYZ[3 * i] + " Y = " +
                                              marksXYZ[(3 * i) + 1] + " Z = " + marksXYZ[(3 * i) + 2] + "\n",
                                              Preferences.DEBUG_FILEIO);
                            reposMarker(i);

                            if (doDicom) {
                                superiorEdgeDicom = pointMarker;
                            } else {
                                superiorEdgeDicom = dicomMarker(pointMarker);
                            }

                            if ((!readACPC) && (!readTLRC)) {
                                fileInfo.setSuperiorEdge(pointMarker);
                                calcOriginalMarker(i);
                                fileInfo.setOriginalSuperiorEdge(pointMarker);
                            }
                        }
                    } else if ((marksLabString[i]).equalsIgnoreCase("AC posterior margin")) {

                        if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                            Preferences.debug(marksLabString[i] + " at X = " + marksXYZ[3 * i] + " Y = " +
                                              marksXYZ[(3 * i) + 1] + " Z = " + marksXYZ[(3 * i) + 2] + "\n",
                                              Preferences.DEBUG_FILEIO);
                            reposMarker(i);

                            if (doDicom) {
                                posteriorMarginDicom = pointMarker;
                            } else {
                                posteriorMarginDicom = dicomMarker(pointMarker);
                            }

                            if ((!readACPC) && (!readTLRC)) {
                                fileInfo.setPosteriorMargin(pointMarker);
                                calcOriginalMarker(i);
                                fileInfo.setOriginalPosteriorMargin(pointMarker);
                            }
                        }
                    } else if ((marksLabString[i]).equalsIgnoreCase("PC inferior edge")) {

                        if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                            Preferences.debug(marksLabString[i] + " at X = " + marksXYZ[3 * i] + " Y = " +
                                              marksXYZ[(3 * i) + 1] + " Z = " + marksXYZ[(3 * i) + 2] + "\n",
                                              Preferences.DEBUG_FILEIO);
                            reposMarker(i);

                            if (doDicom) {
                                pcDicom = pointMarker;
                            } else {
                                pcDicom = dicomMarker(pointMarker);
                            }

                            if ((!readACPC) && (!readTLRC)) {
                                fileInfo.setInferiorEdge(pointMarker);
                            }

                            tInfo.setOrigPC(pointMarker);
                        }
                    } else if ((marksLabString[i]).equalsIgnoreCase("First mid-sag pt")) {

                        if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                            Preferences.debug(marksLabString[i] + " at X = " + marksXYZ[3 * i] + " Y = " +
                                              marksXYZ[(3 * i) + 1] + " Z = " + marksXYZ[(3 * i) + 2] + "\n",
                                              Preferences.DEBUG_FILEIO);
                            reposMarker(i);

                            if (doDicom) {
                                firstPtDicom = pointMarker;
                            } else {
                                firstPtDicom = dicomMarker(pointMarker);
                            }

                            if ((!readACPC) && (!readTLRC)) {
                                fileInfo.setFirstPt(pointMarker);
                            }
                        }
                    } else if ((marksLabString[i]).equalsIgnoreCase("Another mid-sag pt")) {

                        if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                            Preferences.debug(marksLabString[i] + " at X = " + marksXYZ[3 * i] + " Y = " +
                                              marksXYZ[(3 * i) + 1] + " Z = " + marksXYZ[(3 * i) + 2] + "\n",
                                              Preferences.DEBUG_FILEIO);
                            reposMarker(i);

                            if (doDicom) {
                                anotherPtDicom = pointMarker;
                            } else {
                                anotherPtDicom = dicomMarker(pointMarker);
                            }

                            if ((!readACPC) && (!readTLRC)) {
                                fileInfo.setAnotherPt(pointMarker);
                            }
                        }
                    }
                    // points for +acpc to +tlrc transformation
                    else if ((marksLabString[i]).equalsIgnoreCase("Most anterior point")) {

                        if (presentViewType == FileInfoAfni.AFNI_ACPC) {
                            Preferences.debug(marksLabString[i] + " at X = " + marksXYZ[3 * i] + " Y = " +
                                              marksXYZ[(3 * i) + 1] + " Z = " + marksXYZ[(3 * i) + 2] + "\n", 
                                              Preferences.DEBUG_FILEIO);
                            reposMarker(i);
                            fileInfo.setAnteriorPt(pointMarker);

                            if (tInfo.getAcpcMin() != null) {
                                acpcMinPt = tInfo.getAcpcMin();
                            }

                            acpcMinPt.Y = pointMarker.Y;
                            tInfo.setAcpcMin(acpcMinPt);
                        }
                    } else if ((marksLabString[i]).equalsIgnoreCase("Most posterior poin")) {

                        if (presentViewType == FileInfoAfni.AFNI_ACPC) {
                            Preferences.debug(marksLabString[i] + " at X = " + marksXYZ[3 * i] + " Y = " +
                                              marksXYZ[(3 * i) + 1] + " Z = " + marksXYZ[(3 * i) + 2] + "\n", 
                                              Preferences.DEBUG_FILEIO);
                            reposMarker(i);
                            fileInfo.setPosteriorPt(pointMarker);

                            if (tInfo.getAcpcMax() != null) {
                                acpcMaxPt = tInfo.getAcpcMax();
                            }

                            acpcMaxPt.Y = pointMarker.Y;
                            tInfo.setAcpcMax(acpcMaxPt);
                        }
                    } else if ((marksLabString[i]).equalsIgnoreCase("Most superior point")) {

                        if (presentViewType == FileInfoAfni.AFNI_ACPC) {
                            Preferences.debug(marksLabString[i] + " at X = " + marksXYZ[3 * i] + " Y = " +
                                              marksXYZ[(3 * i) + 1] + " Z = " + marksXYZ[(3 * i) + 2] + "\n",
                                              Preferences.DEBUG_FILEIO);
                            reposMarker(i);
                            fileInfo.setSuperiorPt(pointMarker);

                            if (tInfo.getAcpcMax() != null) {
                                acpcMaxPt = tInfo.getAcpcMax();
                            }

                            acpcMaxPt.Z = pointMarker.Z;
                            tInfo.setAcpcMax(acpcMaxPt);
                        }
                    } else if ((marksLabString[i]).equalsIgnoreCase("Most inferior point")) {

                        if (presentViewType == FileInfoAfni.AFNI_ACPC) {
                            Preferences.debug(marksLabString[i] + " at X = " + marksXYZ[3 * i] + " Y = " +
                                              marksXYZ[(3 * i) + 1] + " Z = " + marksXYZ[(3 * i) + 2] + "\n",
                                              Preferences.DEBUG_FILEIO);
                            reposMarker(i);
                            fileInfo.setInferiorPt(pointMarker);

                            if (tInfo.getAcpcMin() != null) {
                                acpcMinPt = tInfo.getAcpcMin();
                            }

                            acpcMinPt.Z = pointMarker.Z;
                            tInfo.setAcpcMin(acpcMinPt);
                        }
                    } else if ((marksLabString[i]).equalsIgnoreCase("Most left point")) {

                        if (presentViewType == FileInfoAfni.AFNI_ACPC) {
                            Preferences.debug(marksLabString[i] + " at X = " + marksXYZ[3 * i] + " Y = " +
                                              marksXYZ[(3 * i) + 1] + " Z = " + marksXYZ[(3 * i) + 2] + "\n", 
                                              Preferences.DEBUG_FILEIO);
                            reposMarker(i);
                            fileInfo.setLeftPt(pointMarker);

                            if (tInfo.getAcpcMax() != null) {
                                acpcMaxPt = tInfo.getAcpcMax();
                            }

                            acpcMaxPt.X = pointMarker.X;
                            tInfo.setAcpcMax(acpcMaxPt);
                        }
                    } else if ((marksLabString[i]).equalsIgnoreCase("Most right point")) {

                        if (presentViewType == FileInfoAfni.AFNI_ACPC) {
                            Preferences.debug(marksLabString[i] + " at X = " + marksXYZ[3 * i] + " Y = " +
                                              marksXYZ[(3 * i) + 1] + " Z = " + marksXYZ[(3 * i) + 2] + "\n",
                                              Preferences.DEBUG_FILEIO);
                            reposMarker(i);
                            fileInfo.setRightPt(pointMarker);

                            if (tInfo.getAcpcMin() != null) {
                                acpcMinPt = tInfo.getAcpcMin();
                            }

                            acpcMinPt.X = pointMarker.X;
                            tInfo.setAcpcMin(acpcMinPt);
                        }
                    } else {
                        Preferences.debug("Unrecognized marker " + marksLabString[i] + " at X = " + marksXYZ[3 * i] +
                                          " Y = " + marksXYZ[(3 * i) + 1] + " Z = " + marksXYZ[(3 * i) + 2] + "\n",
                                          Preferences.DEBUG_FILEIO);
                    }
                } // marker within bounds
                else if (marksLabString[i] != null) { // marker out of bounds

                    // points for +orig to +acpc transformation
                    if ((marksLabString[i]).equalsIgnoreCase("AC superior edge")) {
                        Preferences.debug("Out of bounds " + marksLabString[i] + " at X = " + marksXYZ[3 * i] +
                                          " Y = " + marksXYZ[(3 * i) + 1] + " Z = " + marksXYZ[(3 * i) + 2] + "\n", 
                                          Preferences.DEBUG_FILEIO);
                    } else if ((marksLabString[i]).equalsIgnoreCase("AC posterior margin")) {
                        Preferences.debug("Out of bounds " + marksLabString[i] + " at X = " + marksXYZ[3 * i] +
                                          " Y = " + marksXYZ[(3 * i) + 1] + " Z = " + marksXYZ[(3 * i) + 2] + "\n", 
                                          Preferences.DEBUG_FILEIO);
                    } else if ((marksLabString[i]).equalsIgnoreCase("PC inferior edge")) {
                        Preferences.debug("Out of bounds " + marksLabString[i] + " at X = " + marksXYZ[3 * i] +
                                          " Y = " + marksXYZ[(3 * i) + 1] + " Z = " + marksXYZ[(3 * i) + 2] + "\n",
                                          Preferences.DEBUG_FILEIO);
                    } else if ((marksLabString[i]).equalsIgnoreCase("First mid-sag pt")) {
                        Preferences.debug("Out of bounds " + marksLabString[i] + " at X = " + marksXYZ[3 * i] +
                                          " Y = " + marksXYZ[(3 * i) + 1] + " Z = " + marksXYZ[(3 * i) + 2] + "\n", 
                                          Preferences.DEBUG_FILEIO);
                    } else if ((marksLabString[i]).equalsIgnoreCase("Another mid-sag pt")) {
                        Preferences.debug("Out of bounds " + marksLabString[i] + " at X = " + marksXYZ[3 * i] +
                                          " Y = " + marksXYZ[(3 * i) + 1] + " Z = " + marksXYZ[(3 * i) + 2] + "\n", 
                                          Preferences.DEBUG_FILEIO);
                    }
                    // points for +acpc to +tlrc transformation
                    else if ((marksLabString[i]).equalsIgnoreCase("Most anterior point")) {
                        Preferences.debug("Out of bounds " + marksLabString[i] + " at X = " + marksXYZ[3 * i] +
                                          " Y = " + marksXYZ[(3 * i) + 1] + " Z = " + marksXYZ[(3 * i) + 2] + "\n", 
                                          Preferences.DEBUG_FILEIO);
                    } else if ((marksLabString[i]).equalsIgnoreCase("Most posterior point")) {
                        Preferences.debug("Out of bounds " + marksLabString[i] + " at X = " + marksXYZ[3 * i] +
                                          " Y = " + marksXYZ[(3 * i) + 1] + " Z = " + marksXYZ[(3 * i) + 2] + "\n", 
                                          Preferences.DEBUG_FILEIO);
                    } else if ((marksLabString[i]).equalsIgnoreCase("Most superior point")) {
                        Preferences.debug("Out of bounds " + marksLabString[i] + " at X = " + marksXYZ[3 * i] +
                                          " Y = " + marksXYZ[(3 * i) + 1] + " Z = " + marksXYZ[(3 * i) + 2] + "\n", 
                                          Preferences.DEBUG_FILEIO);
                    } else if ((marksLabString[i]).equalsIgnoreCase("Most inferior point")) {
                        Preferences.debug("Out of bounds " + marksLabString[i] + " at X = " + marksXYZ[3 * i] +
                                          " Y = " + marksXYZ[(3 * i) + 1] + " Z = " + marksXYZ[(3 * i) + 2] + "\n", 
                                          Preferences.DEBUG_FILEIO);
                    } else if ((marksLabString[i]).equalsIgnoreCase("Most left point")) {
                        Preferences.debug("Out of bounds " + marksLabString[i] + " at X = " + marksXYZ[3 * i] +
                                          " Y = " + marksXYZ[(3 * i) + 1] + " Z = " + marksXYZ[(3 * i) + 2] + "\n", 
                                          Preferences.DEBUG_FILEIO);
                    } else if ((marksLabString[i]).equalsIgnoreCase("Most right point")) {
                        Preferences.debug("Out of bounds " + marksLabString[i] + " at X = " + marksXYZ[3 * i] +
                                          " Y = " + marksXYZ[(3 * i) + 1] + " Z = " + marksXYZ[(3 * i) + 2] + "\n", 
                                          Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug("Out of bounds unrecognized marker " + marksLabString[i] + " at X = " +
                                          marksXYZ[3 * i] + " Y = " + marksXYZ[(3 * i) + 1] + " Z = " +
                                          marksXYZ[(3 * i) + 2] + "\n", Preferences.DEBUG_FILEIO);
                    }
                } // marker out of bounds
            } // for (i = 0; i < 10; i++)

            if (!((presentViewType == FileInfoAfni.AFNI_ACPC) && (alsoAcpc))) {

                if ((brickFloatFacs != null) && (brickFloatFacs.length != subBrickNumber)) {
                    throw new IOException("AFNI Read Header Error: DATASET_RANK[1] = " + subBrickNumber +
                                          ", but BRICK_FLOAT_FACS length = " + brickFloatFacs.length);
                }

                if ((brickStats != null) && (brickStats.length != (2 * subBrickNumber))) {
                    throw new IOException("AFNI Read Header Error: DATASET_RANK[1] = " + subBrickNumber +
                                          ", but BRICK_STATS length = " + brickStats.length);
                }

                j = tagNumber * floatsPerTag;

                if ((tagsetFloats != null) && (tagsetFloats.length != j)) {
                    throw new IOException("AFNI Read Header Error: tagsetFloats.length = " + tagsetFloats.length +
                                          ", but (tagNumber * floatsPerTag) = " + j);
                }

                if (subBrickNumber == 1) { // 3D case
                    imgExtents = new int[3];

                    if (doDicom) {
                        imgExtents[0] = dicomXDim;
                        imgExtents[1] = dicomYDim;
                        imgExtents[2] = dicomZDim;
                        numberDicomSlices = dicomZDim;
                    } else {
                        imgExtents[0] = xDim;
                        imgExtents[1] = yDim;
                        imgExtents[2] = zDim;
                    }

                    numberSlices = zDim;
                } else { // AFNI 3D + time dataset

                    if ((tDim > 0) && (subBrickNumber != tDim)) {
                        throw new IOException("AFNI Read Header Error: DATASET_RANK[1] = " + subBrickNumber +
                                              ", but TAXIS_NUMS[0] = " + tDim);
                    }

                    imgExtents = new int[4];

                    if (doDicom) {
                        imgExtents[0] = dicomXDim;
                        imgExtents[1] = dicomYDim;
                        imgExtents[2] = dicomZDim;
                        numberDicomSlices = dicomZDim * subBrickNumber;
                    } else {
                        imgExtents[0] = xDim;
                        imgExtents[1] = yDim;
                        imgExtents[2] = zDim;
                    }

                    imgExtents[3] = subBrickNumber;
                    numberSlices = zDim * subBrickNumber;
                } // else AFNI 3D + time dataset

                fileInfo.setExtents(imgExtents);

                if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                    tInfo.setOrigDim(imgExtents);
                }

                if (doDicom) {
                    imgResols[0] = dicomDelta[0];
                    imgResols[1] = dicomDelta[1];
                    imgResols[2] = dicomDelta[2];
                }

                fileInfo.setResolutions(imgResols);

                if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                    tInfo.setOrigRes(imgResols);
                }

                // x,y,z AFNI units are always in millimeters
                fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 0);
                fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 1);
                fileInfo.setUnitsOfMeasure(Unit.MILLIMETERS.getLegacyNum(), 2);

                if (doDicom) {
                    dicomOrientation = new int[3];
                    dicomOrientation[0] = FileInfoBase.ORI_R2L_TYPE;
                    dicomOrientation[1] = FileInfoBase.ORI_A2P_TYPE;
                    dicomOrientation[2] = FileInfoBase.ORI_I2S_TYPE;
                    fileInfo.setAxisOrientation(dicomOrientation);

                    if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                        fileInfo.setLowXmm(dicomLowestX);
                        fileInfo.setLowYmm(dicomLowestY);
                        fileInfo.setLowZmm(dicomLowestZ);
                        fileInfo.setHighXmm(dicomHighestX);
                        fileInfo.setHighYmm(dicomHighestY);
                        fileInfo.setHighZmm(dicomHighestZ);
                        fileInfo.setOrigin(dicomLowestX, 0);
                        fileInfo.setOrigin(dicomLowestY, 1);
                        fileInfo.setOrigin(dicomLowestZ, 2);
                    }
                } else {
                    fileInfo.setAxisOrientation(orientSpecific);

                    if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                        fileInfo.setLowXmm(lowestX);
                        fileInfo.setLowYmm(lowestY);
                        fileInfo.setLowZmm(lowestZ);
                        fileInfo.setHighXmm(highestX);
                        fileInfo.setHighYmm(highestY);
                        fileInfo.setHighZmm(highestZ);

                        if ((orientSpecific[0] == FileInfoBase.ORI_R2L_TYPE) ||
                                (orientSpecific[0] == FileInfoBase.ORI_A2P_TYPE) ||
                                (orientSpecific[0] == FileInfoBase.ORI_I2S_TYPE)) {
                            fileInfo.setOrigin(lowestX, 0);
                        } else {
                            fileInfo.setOrigin(highestX, 0);
                        }

                        if ((orientSpecific[1] == FileInfoBase.ORI_R2L_TYPE) ||
                                (orientSpecific[1] == FileInfoBase.ORI_A2P_TYPE) ||
                                (orientSpecific[1] == FileInfoBase.ORI_I2S_TYPE)) {
                            fileInfo.setOrigin(lowestY, 1);
                        } else {
                            fileInfo.setOrigin(highestY, 1);
                        }

                        if ((orientSpecific[2] == FileInfoBase.ORI_R2L_TYPE) ||
                                (orientSpecific[2] == FileInfoBase.ORI_A2P_TYPE) ||
                                (orientSpecific[2] == FileInfoBase.ORI_I2S_TYPE)) {
                            fileInfo.setOrigin(lowestZ, 2);
                        } else {
                            fileInfo.setOrigin(highestZ, 2);
                        }
                    }
                }
                
                if (presentViewType == FileInfoAfni.AFNI_ORIG) {
                    tInfo.setOrigOrigin(fileInfo.getOrigin());
                }

                fileInfo.setAFNIViewType(viewType);
                fileInfo.setAFNITypeString(typeStringType);
            } // if (!((presentViewType == FileInfoAfni.AFNI_ACPC) && (alsoAcpc)))
        } // try
        catch (OutOfMemoryError error) {

            if (image != null) {
                image.disposeLocal();
                image = null;
            }

            System.gc();
            throw error;
        }
        
        return true;
    }


    /**
     * Reads AFNI image (.BRIK file)
     *
     * @return  Generated modelImage
     *
     * @throws  IOException  data reading problem occured
     */
    private ModelImage readImage2() throws IOException {
        int i, j, index;
        int xy, z;
        boolean gap;
        boolean gapFound;
        int gapNumber;
        int[] tmpArray;
        int[] gapArray;
        int gapPlane;
        float[] xyztBuffer;
        int volSize;
        int numRead;
        boolean havePosFloatFacs = false;
        boolean havePosFloatFacs1 = false;
        int zAxisOrientation;

        brikDataType = fileInfo.getDataType();

        if (brickFloatFacs != null) {

            for (i = 0; i < brickFloatFacs.length; i++) {

                if (brickFloatFacs[i] > 0.0f) {
                    havePosFloatFacs = true;

                    if (i == 1) {
                        havePosFloatFacs1 = true;
                    }
                }
            }
        }

        if ((!anatType) && (brikDataType == ModelStorageBase.SHORT) && (funcType != FUNC_FIM_TYPE) &&
                (funcType != FUNC_BUCK_TYPE) && (!havePosFloatFacs1)) {
            havePosFloatFacs = true;

            if (brickFloatFacs == null) {
                brickFloatFacs = new float[2];
                brickFloatFacs[0] = 0.0f;
            }

            switch (funcType) {

                case FUNC_THR_TYPE:
                    brickFloatFacs[1] = FUNC_THR_SCALE_SHORT;
                    break;

                case FUNC_COR_TYPE:
                    brickFloatFacs[1] = FUNC_COR_SCALE_SHORT;
                    break;

                case FUNC_TT_TYPE:
                    brickFloatFacs[1] = FUNC_TT_SCALE_SHORT;
                    break;

                case FUNC_FT_TYPE:
                    brickFloatFacs[1] = FUNC_FT_SCALE_SHORT;
                    break;

                case FUNC_ZT_TYPE:
                    brickFloatFacs[1] = FUNC_ZT_SCALE_SHORT;
                    break;

                case FUNC_CT_TYPE:
                    brickFloatFacs[1] = FUNC_CT_SCALE_SHORT;
                    break;

                case FUNC_BT_TYPE:
                    brickFloatFacs[1] = FUNC_BT_SCALE_SHORT;
                    break;

                case FUNC_BN_TYPE:
                    brickFloatFacs[1] = FUNC_BN_SCALE_SHORT;
                    break;

                case FUNC_GT_TYPE:
                    brickFloatFacs[1] = FUNC_GT_SCALE_SHORT;
                    break;

                case FUNC_PT_TYPE:
                    brickFloatFacs[1] = FUNC_PT_SCALE_SHORT;
                    break;

                default:
                    brickFloatFacs[1] = 0.0f;
                    break;
            }

        }

        if ((!anatType) && (brikDataType == ModelStorageBase.UBYTE) &&
                ((funcType == FUNC_THR_TYPE) || (funcType == FUNC_COR_TYPE) || (funcType == FUNC_TT_TYPE) ||
                     (funcType == FUNC_ZT_TYPE) || (funcType == FUNC_BT_TYPE) || (funcType == FUNC_GT_TYPE)) &&
                (!havePosFloatFacs1)) {
            havePosFloatFacs = true;

            if (brickFloatFacs == null) {
                brickFloatFacs = new float[2];
                brickFloatFacs[0] = 0.0f;
            }

            switch (funcType) {

                case FUNC_THR_TYPE:
                    brickFloatFacs[1] = FUNC_THR_SCALE_BYTE;
                    break;

                case FUNC_COR_TYPE:
                    brickFloatFacs[1] = FUNC_COR_SCALE_BYTE;
                    break;

                case FUNC_TT_TYPE:
                    brickFloatFacs[1] = FUNC_TT_SCALE_BYTE;
                    break;

                case FUNC_ZT_TYPE:
                    brickFloatFacs[1] = FUNC_ZT_SCALE_BYTE;
                    break;

                case FUNC_BT_TYPE:
                    brickFloatFacs[1] = FUNC_BT_SCALE_BYTE;
                    break;

                case FUNC_GT_TYPE:
                    brickFloatFacs[1] = FUNC_GT_SCALE_BYTE;
                    break;

                default:
                    brickFloatFacs[1] = 0.0f;
                    break;
            }

        }

        if (image != null) {
            image.disposeLocal();
        }

        image = null;

        if ((havePosFloatFacs) && (brikDataType != ModelStorageBase.FLOAT) &&
                (brikDataType != ModelStorageBase.COMPLEX) && (brikDataType != ModelStorageBase.DOUBLE)) {

            // all values in .BRIK file are multiplied by brickFloatFacs[0]
            // UBYTE, SHORT, and INTEGER must be changed to FLOAT
            image = new ModelImage(ModelStorageBase.FLOAT, imgExtents, fileInfo.getFileName());
            fileInfo.setDataType(ModelStorageBase.FLOAT);
        } else if ((havePosFloatFacs) && (brikDataType == ModelStorageBase.ARGB)) {

            // all values in .BRIK file are multiplied by brickFloatsFacs[0]
            // ARGB must be changed to ARGB_FLOAT
            image = new ModelImage(ModelStorageBase.ARGB_FLOAT, imgExtents, fileInfo.getFileName());
            fileInfo.setDataType(ModelStorageBase.ARGB_FLOAT);
        } else {

            // original data type preserved
            image = new ModelImage(brikDataType, imgExtents, fileInfo.getFileName());
        }

        // Dicom is always axial orientation
        if (doDicom) {
            zAxisOrientation = dicomOrientation[2];
        } else {
            zAxisOrientation = orientSpecific[2];
        }

        if ((zAxisOrientation == FileInfoBase.ORI_R2L_TYPE) || (zAxisOrientation == FileInfoBase.ORI_L2R_TYPE)) {
            fileInfo.setImageOrientation(FileInfoBase.SAGITTAL);
        } else if ((zAxisOrientation == FileInfoBase.ORI_A2P_TYPE) || (zAxisOrientation == FileInfoBase.ORI_P2A_TYPE)) {
            fileInfo.setImageOrientation(FileInfoBase.CORONAL);
        } else {
            fileInfo.setImageOrientation(FileInfoBase.AXIAL);
        }

        // open .BRIK file
        file = new File(fileDir + brikFileName);
        raFile = new RandomAccessFile(file, "r");
        fileLength = raFile.length();

        if (doDicom) {

            if (ModelImage.isColorImage(fileInfo.getDataType())) {
                bufferSize = 4 * xDim * yDim * zDim;
                sliceSize = 4 * xDim * yDim;
            } else if (fileInfo.getDataType() == ModelStorageBase.COMPLEX) {
                bufferSize = 2 * xDim * yDim * zDim;
                sliceSize = 2 * xDim * yDim;
            } else {
                bufferSize = xDim * yDim * zDim;
                sliceSize = xDim * yDim;
            }

            imgBuffer = new float[bufferSize];
            img2Buffer = new float[bufferSize];
            dicomSliceSize = dicomXDim * dicomYDim;

            // for functional images check for gap filling zeroes
            if (!anatType) {
                xyztBuffer = new float[bufferSize * subBrickNumber];
                numRead = xDim * yDim * zDim * subBrickNumber;
                readBuffer(0, xyztBuffer, 0.0f, numRead);
                tmpArray = new int[zDim];
                gapNumber = 0;
                gapFound = false;

                for (z = 0; z < zDim; z++) {
                    gap = true;

                    for (i = 0; (i < subBrickNumber) && (gap); i++) {
                        index = (z * sliceSize) + (i * bufferSize);

                        for (xy = 0; (xy < sliceSize) && (gap); xy++) {

                            if (xyztBuffer[index + xy] != 0) {
                                gap = false;
                            }
                        } // for (xy = 0; (xy < bufferSize) && (gap); xy++)
                    } // for (i = 0; (i < subBrickNumber) && (gap); i++)

                    if (gap) {
                        tmpArray[gapNumber++] = z;
                        gapFound = true;
                    }
                } // for (z = 0; z < zDim; z++)

                if (gapFound) {
                    gapArray = new int[gapNumber];

                    for (i = 0; i < gapNumber; i++) {
                        gapArray[i] = tmpArray[i];
                    }

                    switch (orientSpecific[2]) {

                        case FileInfoBase.ORI_R2L_TYPE:
                        case FileInfoBase.ORI_L2R_TYPE:
                            gapPlane = 0;
                            break;

                        case FileInfoBase.ORI_A2P_TYPE:
                        case FileInfoBase.ORI_P2A_TYPE:
                            gapPlane = 1;
                            break;

                        case FileInfoBase.ORI_I2S_TYPE:
                        case FileInfoBase.ORI_S2I_TYPE:
                            gapPlane = 2;
                            break;

                        default:
                            gapPlane = -1;
                            break;
                    }

                    for (i = 0; i < gapNumber; i++) {

                        switch (orientSpecific[2]) {

                            case FileInfoBase.ORI_R2L_TYPE:
                            case FileInfoBase.ORI_A2P_TYPE:
                            case FileInfoBase.ORI_I2S_TYPE:
                                break;

                            case FileInfoBase.ORI_L2R_TYPE:
                            case FileInfoBase.ORI_P2A_TYPE:
                            case FileInfoBase.ORI_S2I_TYPE:
                                gapArray[i] = zDim - 1 - gapArray[i];
                                break;
                        }
                    } // for (i = 0; i < gapNumber; i++)

                    // 2 == z gap
                    fileInfo.setPlaneGap(gapPlane);
                    fileInfo.setGapArray(gapArray);
                } // if (gapFound)

                raFile.seek(0L);
                xyztBuffer = null;
            } // if (!anatType)

            for (i = 0; i < subBrickNumber; i++) {

                if (havePosFloatFacs) {
                    readXYZBuffer(i, imgBuffer, img2Buffer, brickFloatFacs[i]);
                } else {
                    readXYZBuffer(i, imgBuffer, img2Buffer, 0.0f);
                }

                image.importData(i * bufferSize, img2Buffer, false);
            }

            if ((!readACPC) && (!readTLRC)) {

                image.setFileInfo(fileInfo, 0);
                for (i = 1; i < numberDicomSlices; i++) {
                    fileInfoCopy = (FileInfoAfni)fileInfo.clone();
                    image.setFileInfo(fileInfoCopy, i);
                }
            } // if ((!readACPC) && (!readTLRC))
        } // if (doDicom)
        else {
      
            if (ModelImage.isColorImage(fileInfo.getDataType())) {
                bufferSize = 4 * xDim * yDim;
                volSize = 4 * xDim * yDim * zDim;
            } else if (fileInfo.getDataType() == ModelStorageBase.COMPLEX) {
                bufferSize = 2 * xDim * yDim;
                volSize = 2 * xDim * yDim * zDim;
            } else {
                bufferSize = xDim * yDim;
                volSize = xDim * yDim * zDim;
            }

            imgBuffer = new float[bufferSize];

            // for functional images check for gap filling zeroes
            if (!anatType) {
                tmpArray = new int[zDim];
                gapNumber = 0;
                gapFound = false;
                numRead = xDim * yDim * zDim * subBrickNumber;
                xyztBuffer = new float[volSize * subBrickNumber];
                readBuffer(0, xyztBuffer, 0.0f, numRead);

                for (z = 0; z < zDim; z++) {
                    gap = true;

                    for (i = 0; (i < subBrickNumber) && (gap); i++) {
                        index = (z * bufferSize) + (i * volSize);

                        for (xy = 0; (xy < bufferSize) && (gap); xy++) {

                            if (xyztBuffer[index + xy] != 0) {
                                gap = false;
                            }
                        } // for (xy = 0; (xy < bufferSize) && (gap); xy++)
                    } // for (i = 0; (i < subBrickNumber) && (gap); i++)

                    if (gap) {
                        tmpArray[gapNumber++] = z;
                        gapFound = true;
                    }
                } // for (z = 0; z < zDim; z++)

                if (gapFound) {
                    gapArray = new int[gapNumber];

                    for (i = 0; i < gapNumber; i++) {
                        gapArray[i] = tmpArray[i];
                    }

                    // 2 == z gap
                    gapPlane = 2;
                    fileInfo.setPlaneGap(gapPlane);
                    fileInfo.setGapArray(gapArray);
                } // if (gapFound)

                raFile.seek(0L);
                xyztBuffer = null;
            } // if (!anatType)

            numRead = xDim * yDim;

            for (i = 0; i < subBrickNumber; i++) {

            	
                for (j = 0; j < zDim; j++) {
                    index = j + (i * zDim);

                    if (havePosFloatFacs) {
                        readBuffer(index, imgBuffer, brickFloatFacs[i], numRead); // Slice at a time
                    } else {
                        readBuffer(index, imgBuffer, 0.0f, numRead);
                    }

                    image.importData(index * bufferSize, imgBuffer, false);
                }
            }
            
            image.calcMinMax();
            
            for (i = 0; i < subBrickNumber; i++) {
            	ModelLUT lut = null;
            	if(brickLabsString != null && brickLabsString[i].contains("Tstat")) {
            		lut = ModelLUT.buildTDistLUT(1, .9, image);
            	}
            	
            	for (j = 0; j < zDim; j++) {
                    index = j + (i * zDim);

                    if ((!readACPC) && (!readTLRC)) {
                        fileInfoCopy = (FileInfoAfni)fileInfo.clone();
                        fileInfoCopy.setSubBrickNumber(i);
                        if(lut != null) {
                        	fileInfoCopy.setLUT(lut);
                        }
                        image.setFileInfo(fileInfoCopy, index);
                    }
            	}
            }
            

        } // dataset ordering

        image.calcMinMax();

        raFile.close();


        return image;

    }

    /**
     * readLine() - reads a line of the file and strips comments indicated by the # symbol.
     *
     * @return     the line read in
     *
     * @exception  IOException  if there is an error reading the file
     */
    private String readLine() throws IOException {
        String tempString;
        String retString;
        int index;

        try {
            tempString = raFile.readLine();
        } catch (IOException error) {
            throw (error);
        }

        if (tempString == null) {
            return null;
        }

        index = tempString.indexOf("#");

        if (index != -1) {
            retString = tempString.substring(0, index);

            return retString.trim();
        } else {
            return tempString.trim();
        }
    }

    /**
     * Reads a slice of data at a time and stores the results in the buffer.
     *
     * @param      slice        offset into the file stored in the dataOffset array
     * @param      buffer       buffer where the info is read into
     * @param      dicomBuffer  buffer into which the data is reordered
     * @param      scaleFact    if zero data unscaled, if > 0 data is scaled by scaleFact
     *
     * @exception  IOException  if there is an error reading the file
     */
    private void readXYZBuffer(int slice, float[] buffer, float[] dicomBuffer, float scaleFact) throws IOException {
        int i = 0;
        int j;
        int nBytes;
        byte[] byteBuffer;
        int b1, b2, b3, b4;
        int x, y, z;
        int newX = 0;
        int newY = 0;
        int newZ = 0;
        int progress, progressLength, mod;
        int tmpInt;

        fireProgressStateChanged(ViewUserInterface.getReference().getProgressBarPrefix() + "AFNI...");

        switch (brikDataType) {

            case ModelStorageBase.UBYTE:
                byteBuffer = new byte[buffer.length];
                nBytes = xDim * yDim * zDim;
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * subBrickNumber;
                mod = progressLength / 100;

                for (j = 0; j < nBytes; j++, i++) {

                    if (((i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    buffer[i] = byteBuffer[j] & 0xff;
                }

                break;

            case ModelStorageBase.SHORT:
                byteBuffer = new byte[2 * buffer.length];
                nBytes = 2 * xDim * yDim * zDim;
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * subBrickNumber;
                mod = progressLength / 10;

                for (j = 0; j < nBytes; j += 2, i++) {

                    if (((i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    b1 = getUnsignedByte(byteBuffer, j);
                    b2 = getUnsignedByte(byteBuffer, j + 1);

                    if (endianess) {
                        buffer[i] = (short) ((b1 << 8) + b2);
                    } else {
                        buffer[i] = (short) ((b2 << 8) + b1);
                    }

                } // for (j = 0; j < nBytes; j+=2, i++ )

                break;

            case ModelStorageBase.INTEGER:
                byteBuffer = new byte[4 * buffer.length];
                nBytes = 4 * xDim * yDim * zDim;
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * subBrickNumber;
                mod = progressLength / 10;

                for (j = 0; j < nBytes; j += 4, i++) {

                    if (((i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    b1 = getUnsignedByte(byteBuffer, j);
                    b2 = getUnsignedByte(byteBuffer, j + 1);
                    b3 = getUnsignedByte(byteBuffer, j + 2);
                    b4 = getUnsignedByte(byteBuffer, j + 3);

                    if (endianess) {
                        buffer[i] = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
                    } else {
                        buffer[i] = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little Endian
                    }
                } // for (j =0; j < nBytes; j+=4, i++ )

                break;

            case ModelStorageBase.FLOAT:
                byteBuffer = new byte[4 * buffer.length];
                nBytes = 4 * xDim * yDim * zDim;
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * subBrickNumber;
                mod = progressLength / 10;

                for (j = 0; j < nBytes; j += 4, i++) {

                    if (((i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    b1 = getUnsignedByte(byteBuffer, j);
                    b2 = getUnsignedByte(byteBuffer, j + 1);
                    b3 = getUnsignedByte(byteBuffer, j + 2);
                    b4 = getUnsignedByte(byteBuffer, j + 3);

                    if (endianess) {
                        tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
                    } else {
                        tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little Endian
                    }
                    
                    buffer[i] = Float.intBitsToFloat(tmpInt);
                } // for (j =0; j < nBytes; j+=4, i++ )

                break;

            case ModelStorageBase.DOUBLE:
                break;

            case ModelStorageBase.COMPLEX:
                byteBuffer = new byte[4 * buffer.length];
                nBytes = 8 * xDim * yDim * zDim;
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * numberSlices;
                mod = progressLength / 10;

                for (j = 0; j < nBytes; j += 4, i++) {

                    if (((i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    b1 = getUnsignedByte(byteBuffer, j);
                    b2 = getUnsignedByte(byteBuffer, j + 1);
                    b3 = getUnsignedByte(byteBuffer, j + 2);
                    b4 = getUnsignedByte(byteBuffer, j + 3);

                    if (endianess) {
                        tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
                    } else {
                        tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little Endian
                    }

                    buffer[i] = Float.intBitsToFloat(tmpInt);
                } // for (j =0; j < nBytes; j+=4, i++ )
                break;

            case ModelStorageBase.ARGB:
                byteBuffer = new byte[3*buffer.length/4];
                nBytes = 3 * xDim * yDim * zDim;
                raFile.read(byteBuffer, 0, nBytes);
                progress = slice * buffer.length;
                progressLength = buffer.length * numberSlices;
                mod = progressLength / 100;

                for (j = 0; j < nBytes; j+= 3, i+= 4) {

                    if (((i + progress) % mod) == 0) {
                        fireProgressStateChanged(Math.round((float) (i + progress) / progressLength * 100));
                    }

                    buffer[i] = 0;
                    buffer[i+1] = byteBuffer[j] & 0xff;
                    buffer[i+2] = byteBuffer[j+1] & 0xff;
                    buffer[i+3] = byteBuffer[j+2] & 0xff;
                }
                break;
        } // switch(brikDataType)

        if (scaleFact > 0) {

            for (i = 0; i < buffer.length; i++) {
                buffer[i] *= scaleFact;
            }
        } // if (scaleFact > 0)

        fireProgressStateChanged("Putting AFNI image into Dicom order...");

        for (x = 0; x < xDim; x++) {
            progress = 100 * slice / subBrickNumber;
            fireProgressStateChanged(Math.round(progress + (100 * x / (subBrickNumber * (xDim - 1)))));

            switch (orientSpecific[0]) {

                case FileInfoBase.ORI_R2L_TYPE:
                    newX = x;
                    break;

                case FileInfoBase.ORI_L2R_TYPE:
                    newX = xDim - 1 - x;
                    break;

                case FileInfoBase.ORI_A2P_TYPE:
                    newY = x;
                    break;

                case FileInfoBase.ORI_P2A_TYPE:
                    newY = xDim - 1 - x;
                    break;

                case FileInfoBase.ORI_I2S_TYPE:
                    newZ = x;
                    break;

                case FileInfoBase.ORI_S2I_TYPE:
                    newZ = xDim - 1 - x;
                    break;
            }

            for (y = 0; y < yDim; y++) {

                switch (orientSpecific[1]) {

                    case FileInfoBase.ORI_R2L_TYPE:
                        newX = y;
                        break;

                    case FileInfoBase.ORI_L2R_TYPE:
                        newX = yDim - 1 - y;
                        break;

                    case FileInfoBase.ORI_A2P_TYPE:
                        newY = y;
                        break;

                    case FileInfoBase.ORI_P2A_TYPE:
                        newY = yDim - 1 - y;
                        break;

                    case FileInfoBase.ORI_I2S_TYPE:
                        newZ = y;
                        break;

                    case FileInfoBase.ORI_S2I_TYPE:
                        newZ = yDim - 1 - y;
                        break;
                }

                for (z = 0; z < zDim; z++) {

                    switch (orientSpecific[2]) {

                        case FileInfoBase.ORI_R2L_TYPE:
                            newX = z;
                            break;

                        case FileInfoBase.ORI_L2R_TYPE:
                            newX = zDim - 1 - z;
                            break;

                        case FileInfoBase.ORI_A2P_TYPE:
                            newY = z;
                            break;

                        case FileInfoBase.ORI_P2A_TYPE:
                            newY = zDim - 1 - z;
                            break;

                        case FileInfoBase.ORI_I2S_TYPE:
                            newZ = z;
                            break;

                        case FileInfoBase.ORI_S2I_TYPE:
                            newZ = zDim - 1 - z;
                            break;
                    }

                    if (brikDataType == ModelStorageBase.COMPLEX) {
                        // Note that sliceSize already has a factor of 2 multiplied in, but
                        // dicomSliceSize does not
                        dicomBuffer[2*(newX + (dicomXDim * newY) + (dicomSliceSize * newZ))] = buffer[2*x + (2* xDim * y) +
                                                                                                  (sliceSize * z)];
                        dicomBuffer[2*(newX + (dicomXDim * newY) + (dicomSliceSize * newZ))+1] = buffer[2*x + (2* xDim * y) +
                                (sliceSize * z)+1]; 
                    }
                    else if (brikDataType == ModelStorageBase.ARGB) {
                        // Note that sliceSize already has a factor of 3 multiplied in, but
                        // dicomSliceSize does not
                        dicomBuffer[3*(newX + (dicomXDim * newY) + (dicomSliceSize * newZ))] = buffer[3*x + (3*xDim * y) +
                                (sliceSize * z)];
                        dicomBuffer[3*(newX + (dicomXDim * newY) + (dicomSliceSize * newZ))+1] = buffer[3*x + (3*xDim * y) +
                                (sliceSize * z)+1]; 
                        dicomBuffer[3*(newX + (dicomXDim * newY) + (dicomSliceSize * newZ))+2] = buffer[3*x + (3*xDim * y) +
                                (sliceSize * z)+2];    
                    }
                    else {
                        dicomBuffer[newX + (dicomXDim * newY) + (dicomSliceSize * newZ)] = buffer[x + (xDim * y) +
                                                                                              (sliceSize * z)];
                    }
                } // for (z = 0; z < zDim; z++)
            } // for (y = 0; y < yDim; y++)
        } // for (x = 0; x < xDim; x++)

    }

    /**
     * DOCUMENT ME!
     *
     * @param   inString  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private String removeWhiteSpace(String inString) {
        String outString;
        int i, j;

        if (inString != null) {
            char[] val = new char[inString.length()];

            for (i = 0, j = 0; i < inString.length(); i++) {
                val[j] = inString.charAt(i);

                if (val[j] > 0x20) {
                    j++;
                }
            }

            if (j > 0) {
                outString = new String(val, 0, j);

                return outString;
            } // if (j > 0)
            else {
                return null;
            }
        } // if (inString != null)
        else {
            return null;
        }

    }

    /**
     * DOCUMENT ME!
     *
     * @param  markerNumber  DOCUMENT ME!
     */
    private void reposMarker(int markerNumber) {

        // This expects the marker to be in dataset ordering
        int[] originalMarker = new int[3]; // stores the +orig marker location in voxel(i,j,k) numbers
                                           // - not millimeters - in dataset ordering if doDicom is false
                                           // uses dicom ordering if doDicom is true
        originalMarker[0] = (int) (((marksXYZ[markerNumber * 3] - origin[0]) / delta[0]) + 0.5f);
        originalMarker[1] = (int) (((marksXYZ[(markerNumber * 3) + 1] - origin[1]) / delta[1]) + 0.5f);
        originalMarker[2] = (int) (((marksXYZ[(markerNumber * 3) + 2] - origin[2]) / delta[2]) + 0.5f);

        if (doDicom) {
            int[] reorderedMarker = new int[3];

            switch (orientSpecific[0]) {

                case FileInfoBase.ORI_R2L_TYPE:
                    reorderedMarker[0] = originalMarker[0];
                    break;

                case FileInfoBase.ORI_L2R_TYPE:
                    reorderedMarker[0] = xDim - 1 - originalMarker[0];
                    break;

                case FileInfoBase.ORI_A2P_TYPE:
                    reorderedMarker[1] = originalMarker[0];
                    break;

                case FileInfoBase.ORI_P2A_TYPE:
                    reorderedMarker[1] = xDim - 1 - originalMarker[0];
                    break;

                case FileInfoBase.ORI_I2S_TYPE:
                    reorderedMarker[2] = originalMarker[0];
                    break;

                case FileInfoBase.ORI_S2I_TYPE:
                    reorderedMarker[2] = xDim - 1 - originalMarker[0];
                    break;
            }

            switch (orientSpecific[1]) {

                case FileInfoBase.ORI_R2L_TYPE:
                    reorderedMarker[0] = originalMarker[1];
                    break;

                case FileInfoBase.ORI_L2R_TYPE:
                    reorderedMarker[0] = yDim - 1 - originalMarker[1];
                    break;

                case FileInfoBase.ORI_A2P_TYPE:
                    reorderedMarker[1] = originalMarker[1];
                    break;

                case FileInfoBase.ORI_P2A_TYPE:
                    reorderedMarker[1] = yDim - 1 - originalMarker[1];
                    break;

                case FileInfoBase.ORI_I2S_TYPE:
                    reorderedMarker[2] = originalMarker[1];
                    break;

                case FileInfoBase.ORI_S2I_TYPE:
                    reorderedMarker[2] = yDim - 1 - originalMarker[1];
                    break;
            }

            switch (orientSpecific[2]) {

                case FileInfoBase.ORI_R2L_TYPE:
                    reorderedMarker[0] = originalMarker[2];
                    break;

                case FileInfoBase.ORI_L2R_TYPE:
                    reorderedMarker[0] = zDim - 1 - originalMarker[2];
                    break;

                case FileInfoBase.ORI_A2P_TYPE:
                    reorderedMarker[1] = originalMarker[2];
                    break;

                case FileInfoBase.ORI_P2A_TYPE:
                    reorderedMarker[1] = zDim - 1 - originalMarker[2];
                    break;

                case FileInfoBase.ORI_I2S_TYPE:
                    reorderedMarker[2] = originalMarker[2];
                    break;

                case FileInfoBase.ORI_S2I_TYPE:
                    reorderedMarker[2] = zDim - 1 - originalMarker[2];
                    break;
            }

            pointMarker = new Vector3f((float) reorderedMarker[0], (float) reorderedMarker[1],
                                       (float) reorderedMarker[2]);
        } else {
            pointMarker = new Vector3f((float) originalMarker[0], (float) originalMarker[1], (float) originalMarker[2]);
        }
    }

    /**
     * scale and add two vectors.
     *
     * @param   fa  DOCUMENT ME!
     * @param   a   DOCUMENT ME!
     * @param   fb  DOCUMENT ME!
     * @param   b   DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private Vector3f sclAdd(float fa, Vector3f a, float fb, Vector3f b) {
        Vector3f pt = new Vector3f(0.0f, 0.0f, 0.0f);
        pt.X = (fa * a.X) + (fb * b.X);
        pt.Y = (fa * a.Y) + (fb * b.Y);
        pt.Z = (fa * a.Z) + (fb * b.Z);

        return pt;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   pt1  DOCUMENT ME!
     * @param   pt2  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private Vector3f sub(Vector3f pt1, Vector3f pt2) {
        Vector3f pt = new Vector3f(0.0f, 0.0f, 0.0f);
        pt.X = pt1.X - pt2.X;
        pt.Y = pt1.Y - pt2.Y;
        pt.Z = pt1.Z - pt2.Z;

        return pt;
    }

    /**
     * transformACPCTrilinear - transforms and resamples volume using trilinear interpolation.
     *
     * @param  imgBuffer  image array
     * @param  xfrm       transformation matrix to be applied
     * @param  iXres      DOCUMENT ME!
     * @param  iYres      DOCUMENT ME!
     * @param  iZres      DOCUMENT ME!
     * @param  iXdim      DOCUMENT ME!
     * @param  iYdim      DOCUMENT ME!
     * @param  iZdim      DOCUMENT ME!
     * @param  oXres      DOCUMENT ME!
     * @param  oYres      DOCUMENT ME!
     * @param  oZres      DOCUMENT ME!
     * @param  oXdim      DOCUMENT ME!
     * @param  oYdim      DOCUMENT ME!
     * @param  oZdim      DOCUMENT ME!
     */
    private void transformACPCTrilinear(float[] imgBuffer, TransMatrix xfrm, float iXres, float iYres, float iZres,
                                        int iXdim, int iYdim, int iZdim, float oXres, float oYres, float oZres,
                                        int oXdim, int oYdim, int oZdim) {
        int i, j, k;
        float X, Y, Z;
        int x0, y0, z0;
        float value;
        int sliceSize;
        float imm, jmm, kmm;
        float k1, k2, k3, j1, j2, j3;
        sliceSize = iXdim * iYdim;

        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        int mod = oXdim / 50;

        T00 = xfrm.get(0, 0);
        T01 = xfrm.get(0, 1);
        T02 = xfrm.get(0, 2);
        T03 = xfrm.get(0, 3);
        T10 = xfrm.get(1, 0);
        T11 = xfrm.get(1, 1);
        T12 = xfrm.get(1, 2);
        T13 = xfrm.get(1, 3);
        T20 = xfrm.get(2, 0);
        T21 = xfrm.get(2, 1);
        T22 = xfrm.get(2, 2);
        T23 = xfrm.get(2, 3);

        int position1, position2;
        float b1, b2;
        float dx, dy, dz, dx1, dy1;
        int iXdim1 = iXdim - 1;
        int iYdim1 = iYdim - 1;
        int iZdim1 = iZdim - 1;

        float invXRes = 1 / iXres;
        float invYRes = 1 / iYres;
        float invZRes = 1 / iZres;

        int index = 0;

        for (k = 0; k < oZdim; k++) {

            if (((k % mod) == 0)) {
                fireProgressStateChanged((int) (((float) k / oZdim * 100) + 0.5f));
            }

            kmm = k * oZres;
            k1 = (kmm * T02) + T03;
            k2 = (kmm * T12) + T13;
            k3 = (kmm * T22) + T23;

            for (j = 0; j < oYdim; j++) {
                jmm = j * oYres;
                j1 = (jmm * T01) + k1;
                j2 = (jmm * T11) + k2;
                j3 = (jmm * T21) + k3;

                for (i = 0; i < oXdim; i++) {

                    // transform i,j,k
                    value = 0; // remains zero if voxel is transformed out of bounds
                    imm = i * oXres;
                    X = (j1 + (imm * T00)) * invXRes;

                    if ((X >= 0) && (X < iXdim1)) {
                        Y = (j2 + (imm * T10)) * invYRes;

                        if ((Y >= 0) && (Y < iYdim1)) {
                            Z = (j3 + (imm * T20)) * invZRes;

                            if ((Z >= 0) && (Z < iZdim1)) {

                                x0 = (int) X;
                                y0 = (int) Y;
                                z0 = (int) Z;

                                dx = X - x0;
                                dy = Y - y0;
                                dz = Z - z0;

                                dx1 = 1 - dx;
                                dy1 = 1 - dy;

                                position1 = (z0 * sliceSize) + (y0 * iXdim) + x0;
                                position2 = position1 + sliceSize;

                                b1 = (dy1 * ((dx1 * imgBuffer[position1]) + (dx * imgBuffer[position1 + 1]))) +
                                     (dy *
                                          ((dx1 * imgBuffer[position1 + iXdim]) +
                                               (dx * imgBuffer[position1 + iXdim + 1])));

                                b2 = (dy1 * ((dx1 * imgBuffer[position2]) + (dx * imgBuffer[position2 + 1]))) +
                                     (dy *
                                          ((dx1 * imgBuffer[position2 + iXdim]) +
                                               (dx * imgBuffer[position2 + iXdim + 1])));

                                value = ((1 - dz) * b1) + (dz * b2);

                            } // end if Z in bounds
                        } // end if Y in bounds
                    } // end if X in bounds

                    image.set(index++, value);
                } // end for k
            } // end for j
        } // end for i
    }

    /**
     * transformTalairachTrilinear - transforms and resamples volume using trilinear interpolation.
     *
     * @param  imgBuffer  image array
     * @param  xfrm       transformation matrix to be applied
     * @param  iXres      DOCUMENT ME!
     * @param  iYres      DOCUMENT ME!
     * @param  iZres      DOCUMENT ME!
     * @param  iXdim      DOCUMENT ME!
     * @param  iYdim      DOCUMENT ME!
     * @param  iZdim      DOCUMENT ME!
     * @param  oXres      DOCUMENT ME!
     * @param  oYres      DOCUMENT ME!
     * @param  oZres      DOCUMENT ME!
     * @param  oXdim      DOCUMENT ME!
     * @param  botX       DOCUMENT ME!
     * @param  botY       DOCUMENT ME!
     * @param  botZ       DOCUMENT ME!
     * @param  topX       DOCUMENT ME!
     * @param  topY       DOCUMENT ME!
     * @param  topZ       DOCUMENT ME!
     */
    private void transformTalairachTrilinear(float[] imgBuffer, TransMatrix xfrm, float iXres, float iYres, float iZres,
                                             int iXdim, int iYdim, int iZdim, float oXres, float oYres, float oZres,
                                             int oXdim, int botX, int botY, int botZ, int topX, int topY, int topZ) {

        int i, j, k;
        float X, Y, Z;
        int x0, y0, z0;
        float value;
        int sliceSize;
        float imm, jmm, kmm;
        float k1, k2, k3, j1, j2, j3;
        sliceSize = iXdim * iYdim;

        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        boolean doTransform;

        int mod = oXdim / 50;

        T00 = xfrm.get(0, 0);
        T01 = xfrm.get(0, 1);
        T02 = xfrm.get(0, 2);
        T03 = xfrm.get(0, 3);
        T10 = xfrm.get(1, 0);
        T11 = xfrm.get(1, 1);
        T12 = xfrm.get(1, 2);
        T13 = xfrm.get(1, 3);
        T20 = xfrm.get(2, 0);
        T21 = xfrm.get(2, 1);
        T22 = xfrm.get(2, 2);
        T23 = xfrm.get(2, 3);

        int position1, position2;
        float b1, b2;
        float dx, dy, dz, dx1, dy1;
        int iXdim1 = iXdim - 1;
        int iYdim1 = iYdim - 1;
        int iZdim1 = iZdim - 1;

        float invXRes = 1 / iXres;
        float invYRes = 1 / iYres;
        float invZRes = 1 / iZres;

        for (k = botZ; k <= topZ; k++) {

            if (((k % mod) == 0)) {
                fireProgressStateChanged((int) (((float) (k - botZ) / (topZ - botZ) * 100) + .5));
            }

            kmm = k * oZres;
            k1 = (kmm * T02) + T03;
            k2 = (kmm * T12) + T13;
            k3 = (kmm * T22) + T23;

            for (j = botY; j <= topY; j++) {
                jmm = j * oYres;
                j1 = (jmm * T01) + k1;
                j2 = (jmm * T11) + k2;
                j3 = (jmm * T21) + k3;

                for (i = botX; i <= topX; i++) {

                    // transform i,j,k
                    value = 0; // remains zero if voxel is transformed out of bounds
                    doTransform = false;
                    imm = i * oXres;
                    X = (j1 + (imm * T00)) * invXRes;

                    if ((X >= 0) && (X < iXdim1)) {
                        Y = (j2 + (imm * T10)) * invYRes;

                        if ((Y >= 0) && (Y < iYdim1)) {
                            Z = (j3 + (imm * T20)) * invZRes;

                            if ((Z >= 0) && (Z < iZdim1)) {
                                x0 = (int) X;
                                y0 = (int) Y;
                                z0 = (int) Z;

                                dx = X - x0;
                                dy = Y - y0;
                                dz = Z - z0;

                                dx1 = 1 - dx;
                                dy1 = 1 - dy;

                                position1 = (z0 * sliceSize) + (y0 * iXdim) + x0;
                                position2 = position1 + sliceSize;

                                b1 = (dy1 * ((dx1 * imgBuffer[position1]) + (dx * imgBuffer[position1 + 1]))) +
                                     (dy *
                                          ((dx1 * imgBuffer[position1 + iXdim]) +
                                               (dx * imgBuffer[position1 + iXdim + 1])));

                                b2 = (dy1 * ((dx1 * imgBuffer[position2]) + (dx * imgBuffer[position2 + 1]))) +
                                     (dy *
                                          ((dx1 * imgBuffer[position2 + iXdim]) +
                                               (dx * imgBuffer[position2 + iXdim + 1])));

                                value = ((1 - dz) * b1) + (dz * b2);
                                doTransform = true;
                            } // end if Z in bounds
                        } // end if Y in bounds
                    } // end if X in bounds

                    if (doTransform) {
                        image.set(i, j, k, value);
                    }
                } // end for k
            } // end for j
        } // end for i
    }


}
