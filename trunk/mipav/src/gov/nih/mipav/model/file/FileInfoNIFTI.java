package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.TransMatrix;

import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogText;

import java.util.Vector;


/**
 * This structures contains the information that describes how a NIFTI image is stored on disk. NIFTI is intended to be
 * "mostly compatible" with the ANALYZE 7.5 file format. Most of the "unused" fields in that format have been taken, and
 * some of the lesser-used fields have been co-opted for other purposes. We have extended this format to store image
 * orientation and the origin. We have used unused variables to store these data. Almost all programs ignore these
 * variables and should not have any problems reading images saved with MIPAV, except SPM. A new format for MIPAV is now
 * XML based.
 * 
 * <p>
 * RGB NIFTI images are store in chunky format rgb, rgb, rgb ......
 * </p>
 * 
 * <p>
 * Note that there is a short datatype field.
 * </p>
 * 
 * @see FileNIFTI
 */

public class FileInfoNIFTI extends FileInfoBase {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5506021019885109431L;

    /** Default, no intent indicated. */
    public static final short NIFTI_INTENT_NONE = 0;

    /** Correlation coefficient R (1 param): p1 = degrees of freedom R/sqrt(1-R*R) is t-distributed with p1 DOF. */
    public static final short NIFTI_INTENT_CORREL = 2;

    /** Student t statistic (1 param): p1 = DOF. */
    public static final short NIFTI_INTENT_TTEST = 3;

    /** Fisher F statistic (2 params): p1 = numerator DOF, p2 = denominator DOF. */
    public static final short NIFTI_INTENT_FTEST = 4;

    /** Standard normal (0 params): Density = N(0,1). */
    public static final short NIFTI_INTENT_ZSCORE = 5;

    /** Chi-squared (1 param): p1 = DOF Density(x) proportional to exp(-x/2) * x^(p1/2 - 1). */
    public static final short NIFTI_INTENT_CHISQ = 6;

    /** Beta distribution (2 params): p1 = a, p2 = b Density (x) proportional to x^(a-1) * (1-x)^(b-1). */
    public static final short NIFTI_INTENT_BETA = 7;

    /**
     * Binomial distribution (2 params): p1 = number of trials, p2 = probability per trial Prob(x) = (p1 choose x) *
     * p2^x * (1-p2)^(p1-x), for x = 0,1,...p1.
     */
    public static final short NIFTI_INTENT_BINOM = 8;

    /** Gamma distribution (2 params): p1 = shape, p2 = scale Density (x) proportional to x^(p1-1) * exp(-p2*x). */
    public static final short NIFTI_INTENT_GAMMA = 9;

    /** Poisson distribution (1 param): p1 = mean Prob(x) = exp(-p1) * p1^x/x!, for x = 0, 1, 2, ... */
    public static final short NIFTI_INTENT_POISSON = 10;

    /** Normal distribution (2 params): p1 = mean, p2 = standard deviation. */
    public static final short NIFTI_INTENT_NORMAL = 11;

    /**
     * Noncentral F statistic (3 params): p1 = numerator DOF, p2 = denominator DOF, p3 = numerator noncentrality
     * parameter.
     */
    public static final short NIFTI_INTENT_FTEST_NONC = 12;

    /** Noncentral chi-squared statistic (2 params): p1 = DOF, p2 = noncentrality parameter. */
    public static final short NIFTI_INTENT_CHISQ_NONC = 13;

    /**
     * Logistic distribution (2 params): p1 = location, p2 = scale Density (x) proportional to sech^2((x-p1)/(2*p2)).
     */
    public static final short NIFTI_INTENT_LOGISTIC = 14;

    /** Laplace distribution (2 params): p1 = location, p2 = scale Density (x) proportional to exp(-abs(x-p1)/p2). */
    public static final short NIFTI_INTENT_LAPLACE = 15;

    /** Uniform distribution: p1 = lower end, p2 = upper end. */
    public static final short NIFTI_INTENT_UNIFORM = 16;

    /** Noncentral t statistic (2 params): p1 = DOF, p2 = noncentrality parameter. */
    public static final short NIFTI_INTENT_TTEST_NONC = 17;

    /**
     * Weibull distribution (3 params): p1 = location, p2 = scale, p3 = power Density (x) proportional to
     * ((x-p1)/p2)^(p3-1) * exp(-((x-p1)/p2)^p3) for x > p1.
     */
    public static final short NIFTI_INTENT_WEIBULL = 18;

    /**
     * Chi distribution (1 param): p1 = DOF Density (x) proportional to x^(p1-1) * exp(-x^2/2) for x > 0 p1 = 1 = 'half
     * normal distribution p1 = 2 = Rayleigh distribution p1 = 3 = Maxwell_Boltzmann distribution.
     */

    public static final short NIFTI_INTENT_CHI = 19;

    /**
     * Inverse Gaussian (2 params): p1 = mu, p2 = lambda Density (x) proportional to exp(-p2*(x-p1)^2/(2*p1^2*x)) / x^3
     * for x > 0.
     */
    public static final short NIFTI_INTENT_INVGAUSS = 20;

    /** Extreme value type I (2 params): p1 = location, p2 = scale cdf(x) = exp(-exp(-(x-p1)/p2)). */
    public static final short NIFTI_INTENT_EXTVAL = 21;

    /** Data is a 'p-value' (no params). */
    public static final short NIFTI_INTENT_PVAL = 22;

    /**
     * Data is ln(p-value) (no params). To be safe, a program should compute p = exp(-abs(this_value)). The
     * nifti_stats.c library returns this_value as positive, so that this_value = -log(p).
     */
    public static final short NIFTI_INTENT_LOGPVAL = 23;

    /**
     * Data is log10(p-value) (no params). To be safe, a program should compute p = pow(10.,-abs(this_value)). The
     * nifti_stats.c library returns this_value as positive, so that this_value = -log10(p).
     */
    public static final short NIFTI_INTENT_LOG10PVAL = 24;

    /** Smallest intent code that indicates a statistic. */
    public static final short NIFTI_FIRST_STATCODE = 2;

    /** Largest intent code that indicates a statistic. */
    public static final short NIFTI_LAST_STATCODE = 22;

    // The following intent code values are not for statistics
    /**
     * The value at each voxel is an estimate of some parameter The name of the parameter may be stored in intentName.
     */
    public static final short NIFTI_INTENT_ESTIMATE = 1001;

    /**
     * The value at each voxel is an index into some set of labels The filename with the labels may be stored in
     * auxFile.
     */
    public static final short NIFTI_INTENT_LABEL = 1002;

    /** The value at each voxel is an index into the NeuroNames labels set. */
    public static final short NIFTI_INTENT_NEURONAME = 1003;

    /**
     * To store an M x N matrix at each voxel Dataset must have a 5th dimension (dim[0] = 5 and dim[5] > 1) dim[5] must
     * be M*N intentP1 must be M (in float format) intentP2 must be N (in float format) the matrix values A[i][j] are
     * stored in row order: A[0][0] A[0][1] ... A[0][N-1] A[1][0] A[1][1] ... A[1][N-1] ... A[M-1][0] A[M-1][1] ...
     * A[M-1][N-1]
     */
    public static final short NIFTI_INTENT_GENMATRIX = 1004;

    /**
     * To store an NxN symmetric matrix at each voxel Dataset must have a 5th dimension dim[5] must be N*(N+1)/2
     * intentP1 must be N (in float format) The matrix values A[i][j] are stored in row order A[0][0] A[1][0] A[1][1]
     * A{2][0] A[2][1] A[2][2].
     */
    public static final short NIFTI_INTENT_SYMMATRIX = 1005;

    /**
     * To signify that the vector value at each voxel is to be taken as a displacement field or vector: Dataset must
     * have a 5th dimension dim[5] must be the dimensionality of the displacement vector (e.g., 3 for spatial
     * displacement, 2 for in-plane).
     */
    public static final short NIFTI_INTENT_DISPVECT = 1006; /* specifically for displacements */

    /** DOCUMENT ME! */
    public static final short NIFTI_INTENT_VECTOR = 1007; /* for any other type of vector */

    /**
     * To signify that the vector value at each voxel is really a spatial coordinate (e.g., the verticies or nodes of a
     * surface mesh): dim[0] = 5 dim[1] = number of points dim[2] = dim[3] = dim[4] = 1 dim[5] must be the
     * dimensionality of space (e.g., 3 => 3D space) intentName may describe the object these points come from (e.g.,
     * "pial", "gray/white", "EEG", "MEG").
     */
    public static final short NIFTI_INTENT_POINTSET = 1008;

    /**
     * To signify that the vector value at each voxel is really a triple of indexes (e.g., forming a triangle) from a
     * pointset dataset: Dataset must have a fifth dimension dim[0] = 5 dim[1] = number of triangles dim[2] = dim[3] =
     * dim[4] = 1 dim[5] = 3 dataType should be an integer type (preferably DT_INT32) The data values are indexes
     * (0,1,...) into a pointset dataset.
     */
    public static final short NIFTI_INTENT_TRIANGLE = 1009;

    /**
     * To signify that the vector value at each voxel is a quaternion: Dataset must have a 5th dimension dim[0] = 5
     * dim[5] = 4 dataType should be a floating point type.
     */
    public static final short NIFTI_INTENT_QUATERNION = 1010;

    /**
     * Dimensionless value - no params - although, as in _ESTIMATE the name of the parameter may be stored in
     * intent_name.
     */
    public static final short NIFTI_INTENT_DIMLESS = 1011;

    /** The values at 2001 to 2005 apply to GIFTI datasets */

    /**
     * To signify that each value at each location is from a time series.
     */
    public static final short NIFTI_INTENT_TIME_SERIES = 2001;

    /**
     * To signify that the value at each location is a node index, from a complete surface dataset.
     */
    public static final short NIFTI_INTENT_NODE_INDEX = 2002;

    /**
     * To signify that the vector value at each location is an RGB triplet, of whatever type. - dataset must have a 5th
     * dimension - dim[0] = 5 - dim[1] = number of nodes - dim[2] = dim[3] = dim[4] = 1 - dim[5] = 3
     */
    public static final short NIFTI_INTENT_RGB_VECTOR = 2003;

    /**
     * To signify that the vector value at each location is a 4 valued RGBA vector, of whatever type. - dataset must
     * have a 5th dimension - dim[0] = 5 - dim[1] = number of nodes - dim[2] = dim[3] = dim[4] = 1 - dim[5] = 4
     */
    public static final short NIFTI_INTENT_RGBA_VECTOR = 2004;

    /**
     * To signify that the value at each location is a shape value, such as the curvature.
     */
    public static final short NIFTI_INTENT_SHAPE = 2005;

    /** Unspecified original unscaled source data type */
    public static final short DT_NONE = 0;

    /** Unknown original unscaled source data type */
    public static final short DT_UNKNOWN = 0;

    /** Binary (1 bit/voxel) */
    public static final short DT_BINARY = 1;

    /** Unsigned character (8 bits/voxel) */
    public static final short NIFTI_TYPE_UINT8 = 2;

    /** Signed short (16 bits/voxel) */
    public static final short NIFTI_TYPE_INT16 = 4;

    /** Signed int (32 bits/voxel) */
    public static final short NIFTI_TYPE_INT32 = 8;

    /** Float (32 bits/voxel) */
    public static final short NIFTI_TYPE_FLOAT32 = 16;

    /** 64 bit COMPLEX = 2 32 bit floats. */
    public static final short NIFTI_TYPE_COMPLEX64 = 32;

    /** Double (64 bits/voxel) */
    public static final short NIFTI_TYPE_FLOAT64 = 64;

    /** RGB triple (24 bits/voxel) */
    public static final short NIFTI_TYPE_RGB24 = 128;

    /** Signed char (8 bits/voxel) */
    public static final short NIFTI_TYPE_INT8 = 256;

    /** Unsigned short (16 bits/voxel) */
    public static final short NIFTI_TYPE_UINT16 = 512;

    /** Unsigned integer (32 bits/voxel) */
    public static final short NIFTI_TYPE_UINT32 = 768;

    /** Signed long (64 bits/voxel) */
    public static final short NIFTI_TYPE_INT64 = 1024;

    /** Unsigned long (64 bits/voxel) */
    public static final short NIFTI_TYPE_UINT64 = 1280;

    /** 128 bit floating point number - MIPAV cannot handle */
    public static final short NIFTI_TYPE_FLOAT128 = 1536;

    /** 128 bit COMPLEX = 2 64 bit floats. */
    public static final short NIFTI_TYPE_COMPLEX128 = 1792;

    /** 256 bit COMPLEX = 2 128 bit floats. MIPAV cannot handle. */
    public static final short NIFTI_TYPE_COMPLEX256 = 2048;

    /** Unknown spatial or temporal units */
    public static final int NIFTI_UNITS_UNKNOWN = 0;

    /** Spatial units are in meters */
    public static final int NIFTI_UNITS_METER = 1;

    /** Spatial units are in millimeters */
    public static final int NIFTI_UNITS_MM = 2;

    /** Spatial units are in micrometers */
    public static final int NIFTI_UNITS_MICRON = 3;

    /** Temporal units are in seconds */
    public static final int NIFTI_UNITS_SEC = 8;

    /** Temporal units are in milliseconds */
    public static final int NIFTI_UNITS_MSEC = 16;

    /** Temporal units are in microseconds */
    public static final int NIFTI_UNITS_USEC = 24;

    /** Temporal units are in Hertz */
    public static final int NIFTI_UNITS_HZ = 32;

    /** Temporal units are in parts per million */
    public static final int NIFTI_UNITS_PPM = 40;

    /** Temporal units are in radians per second */
    public static final int NIFTI_UNITS_RADS = 48;

    /** Sequential increasing pattern of slice acquisition */
    public static final byte NIFTI_SLICE_SEQ_INC = 1;

    /** Sequential decreasing pattern of slice acquisition */
    public static final byte NIFTI_SLICE_SEQ_DEC = 2;

    /**
     * Alternating increasing pattern of slice acquisition Slice timing starts at slice_start
     */
    public static final byte NIFTI_SLICE_ALT_INC = 3;

    /**
     * Alternating decreasing pattern of slice acquisition slice timing starts at slice_end
     */
    public static final byte NIFTI_SLICE_ALT_DEC = 4;

    /**
     * Alternating increasing pattern of slice acquisition # 2 Slice timing starts at slice_start + 1
     */
    public static final byte NIFTI_SLICE_ALT_INC2 = 5;

    /**
     * Alternating decreasing pattern of slice acquisition # 2 Slice timing starts at slice_end - 1
     */
    public static final byte NIFTI_SLICE_ALT_DEC2 = 6;

    // Codes for type of X, Y, Z coordinate system.
    /** Arbitrary coordinates. */
    public static final short NIFTI_XFORM_UNKNOWN = 0;

    /** Scanner based anatomical coordinates. */
    public static final short NIFTI_XFORM_SCANNER_ANAT = 1;

    /** Coordinates aligned to another file's or to anatomical "truth". */
    public static final short NIFTI_XFORM_ALIGNED_ANAT = 2;

    /** Coordinates aligned to Talairach-Tournoux Atlas; (0,0,0) = AC, etc. */
    public static final short NIFTI_XFORM_TALAIRACH = 3;

    /** MNI 152 normalized coordiantes. */
    public static final short NIFTI_XFORM_MNI_152 = 4;

    /** ASCII XML-ish elements */
    private static final int NIFTI_ECODE_AFNI = 4;

    /** Plain ASCII text only */
    private static final int NIFTI_ECODE_COMMENT = 6;

    /**
     * MIND is an acronym for NIFTI for DWI (diffusion-weighted images) The 5 MIND extensions to the NIFTI-1.1 header
     * provide a standard specification for data sharing and interchange for diffusion-weighted MRI datasets at various
     * stages of processing.
     */

    /**
     * The contents of a MIND_IDENT field are character data which serve to identify the type of DWI data structure
     * represented by the MIND extended header fields whcih follow.
     */
    private static final int NIFTI_ECODE_MIND_IDENT = 18;

    /**
     * A B_Value field contains a single 32-bit floating point value representing a diffusion-weighing b-value in units
     * of s/mm-squared. In the q-space formalism, the b-value is the square of the magnitude of the diffusion wavevector
     * q.
     */
    private static final int NIFTI_ECODE_B_VALUE = 20;

    /**
     * A SPHERICAL_DIRECTION field contains two 32-bit floating point values which represent a direction in spherical
     * coordinates. The azimuthal angle(longitude) is represented first, in radians, followed by the zenith angle(polar
     * angle, elevation angle, or colatitude), in radians. In the mathematics convention, the ordering is denoted
     * (theta, phi); in the physics convention, the notation is reversed, (phi, theta). A radial coordinate is omitted
     * as this field specifies direction only, not position.
     */
    private static final int NIFTI_ECODE_SPHERICAL_DIRECTION = 22;

    /**
     * The contents of a DT_COMPONENT field are a set of 32-bit integers which specify the indices of a single diffusion
     * tensor component. The number of integers corresponds to the order of the tensor: e.g. a 2nd order tensor
     * component Dij has 2 integer indices, while a 4th order tensor component Dijkl has 4 indices. The integers are
     * given in the indexing order: i.e. i before j before k before l, etc. Furthermore, the indices are 1-based, so
     * that D11 represents the upper-left element of a 2nd order diffusion tensor.
     */
    private static final int NIFTI_ECODE_DT_COMPONENT = 24;

    /**
     * The SHC_DEGREEORDER field specifies the degree (l) and order (m) of a spherical harmonic basis function as a pair
     * of 32-bit integers, with the degree preceding the order. m can take values between -l and +l, inclusive.
     */
    private static final int NIFTI_ECODE_SHC_DEGREEORDER = 26;

    private static final int NIFTI_ECODE_CARET = 30;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** auxiliary file */
    private String aux_file = null;

    /** Bits per pixel */
    private short bitpix = -1;

    /**
     * The cal_min and cal_max fields (if nonzero) are used for mapping (possibly scaled) dataset values to display
     * colors: - Minimum display intensity (black) corresponds to dataset value cal_min. - Maximum display intensity
     * (white) corresponds to dataset value cal_max. - Dataset values below cal_min should display as black also, and
     * values above cal_max as white. - Colors "black" and "white", of course, may refer to any scalar display scheme
     * (e.g., a color lookup table specified via aux_file). - cal_min and cal_max only make sense when applied to
     * scalar-valued datasets (i.e., dim[0] < 5 or dim[5] = 1).
     */
    private float cal_max = 0;

    /** values of 0.0 for both fields imply that no calibration min and max values are used ! */
    private float cal_min = 0;

    /**
     * If qform_code > 0, coord_code = qform_code. If qform_code <= 0 and sform_code > 0, coord_code = sform_code.
     * coord_code has values for "Arbitrary X,Y,Z coordinate system", "Scanner based anatomical coordinates",
     * "Coordinates aligned to another file's or to anatomical truth", "Talairach X,Y,Z coordinate system", and
     * "MNI 152 normalized X,Y,Z coordinates".
     */
    private short coord_code = 0;

    /**
     * If qform_code > 0 and sform_code > 0, coord_code = qform_code and coord_code2 = sform_code. coord_code has values
     * for "Arbitrary X,Y,Z coordinate system", "Scanner based anatomical coordinates",
     * "Coordinates aligned to another file's or to anatomical truth", "Talairach X,Y,Z coordinate system", and
     * "MNI 152 normalized X,Y,Z coordinates".
     */
    private short coord_code2 = 0;

    /** Any text you like */
    private String descrip = null;

    /**
     * Bits 0 and 1 of the dim_info character contain the freq_dim information. 0 for
     * "No frequency encoding direction is present", 1 for "Frequency encoding in the x direction", 2 for
     * "Frequency encoding in the y direction", and 3 for "Frequency encoding in the z direction".
     */
    private int freq_dim = 0;

    /**
     * The intent_code field can be used to indicate that the voxel data has some particular meaning. In particular, a
     * large number of codes is given to indicate that the the voxel data should be interpreted as being drawn from a
     * given probability distribution.
     */
    private short intentCode = 0;

    /**
     * The intent_name field provides space for a 15 character (plus 0 byte) name string for the type of data stored.
     * Examples: - intent_code = NIFTI_INTENT_ESTIMATE; intent_name = "T1"; could be used to signify that the voxel
     * values are estimates of the NMR parameter T1. - intent_code = NIFTI_INTENT_TTEST; intent_name = "House"; could be
     * used to signify that the voxel values are t-statistics for the significance of activation response to a House
     * stimulus. - intent_code = NIFTI_INTENT_DISPVECT; intent_name = "ToMNI152"; could be used to signify that the
     * voxel values are a displacement vector that transforms each voxel (x,y,z) location to the corresponding location
     * in the MNI152 standard brain. - intent_code = NIFTI_INTENT_SYMMATRIX; intent_name = "DTI"; could be used to
     * signify that the voxel values comprise a diffusion tensor image.
     */
    private String intentName = null;

    /**
     * If present, first auxiliary parameter used with intentCode
     */
    private float intentP1;

    /** If present, second auxiliary parameter used with intentCode */
    private float intentP2;

    /** If present, third auxiliary parameter used with intentCode */
    private float intentP3;

    /**
     * Bits 2 and 3 of the dim_info character contain the phase_dim information. 0 for
     * "No phase encoding direction is present", 1 for "Phase encoding in the x direction", 2 for
     * "Phase encoding in the y direction", and 3 for "Phase encoding in the z direction".
     */
    private int phase_dim = 0;

    /**
     * If the scl_slope field is nonzero, then each voxel value in the dataset should be scaled as y = scl_slope * x +
     * scl_inter where x = voxel value stored y = "true" voxel value Normally, we would expect this scaling to be used
     * to store "true" floating values in a smaller integer datatype, but that is not required. That is, it is legal to
     * use scaling even if the datatype is a float type (crazy, perhaps, but legal). - However, the scaling is to be
     * ignored if datatype is DT_RGB24. - If datatype is a complex type, then the scaling is to be applied to both the
     * real and imaginary parts.
     */
    private float scl_slope = 1.0f;

    private float scl_inter = 0.0f;

    /** Should always be a length of 348. */
    private int sizeof_hdr = -1;

    /**
     * Bits 4 and 5 of the dim_info character contain the slice_dim information. 0 for
     * "No slice acquisition direction is present", 1 for "Slice acquisition in the x direction", 2 for
     * "Slice acquisition in the y direction", and 3 for "Slice acquisition in the z direction".
     */
    private int slice_dim = 0;

    /**
     * If this is nonzero, AND if slice_dim is nonzero, AND if slice_duration is positive, indicates the timing pattern
     * of the slice acquisition. The following codes are defined: "Slice timing order is sequentially increasing",
     * "Slice timing order is sequentially decreasing", "Slice timing order is alternately increasing",
     * "Slice timing order is alternately decreasing", "Slice timing order is alternately increasing #2",
     * "Slice timing order is alternately decreasing #2".
     */
    private byte sliceCode = 0;

    /** Time used to acquire 1 slice. */
    private float sliceDuration = 0.0f;

    /** Slice timing pattern ends with slice = (sliceEnd + 1) */
    private short sliceEnd = 0;

    /** Slice timing pattern starts with slice = (sliceStart + 1) */
    private short sliceStart = 0;

    /** Source bits per pixel : 1,8,16,32,64,128 24(rgb). */
    private short sourceBitPix = -1;

    /** Original unscaled source data type */
    private short sourceType = -1;

    /**
     * If the magic field is "n+1", then the voxel data is stored in the same file as the header. In this case, the
     * voxel data starts at offset (int)vox_offset into the header file. Thus, vox_offset=352.0 means that the data
     * starts immediately after the NIFTI-1 header. If vox_offset is greater than 352, the NIFTI-1 format does not say
     * much about the contents of the dataset file between the end of the header and the start of the data.
     * 
     * If the magic field is "ni1", then the voxel data is stored in the associated ".img" file, starting at offset 0
     * (i.e., vox_offset is not used in this case, and should be set to 0.0).
     * 
     * In a .nii file, the vox_offset field value is interpreted as the start location of the image data bytes in that
     * file. In a .hdr/.img file pair, the vox_offset field value is the start location of the image data bytes in the
     * .img file. If vox_offset is less than 352 in a .nii file, it is equivalent to 352 (i.e., image data never starts
     * before byte #352 in a .nii file). The default value for vox_offset in a .nii file is 352. In a .hdr file, the
     * default value for vox_offset is 0. * vox_offset should be an integer multiple of 16; otherwise, some programs may
     * not work properly (e.g., SPM). This is to allow memory-mapped input to be properly byte-aligned.
     */
    private float vox_offset = -1;

    // The number of bytes in the header extension including esize and ecode themselves
    // esize must be a positive integral multiple of 16
    private int esize[] = null;

    // ecode is a non-negative integer integer value that indicates the format of the extended header data that
    // follows. Different ecode values are assigned to different developer groups. At present, the
    // "registered" values for the code are:
    // 0 = unknown private format (not recommended)
    // 2 = DICOM format (i.e., attribute tags and values)
    // 4 = AFNI group (i.e., ASCII XML-ish elements)
    private int ecode[] = null;

    private String mindIdent[] = null;

    private float bValue[] = null;

    private float azimuth[] = null;

    private float zenith[] = null;

    private int dtComponent[][] = null;

    private int degree[] = null;

    private int order[] = null;

    private String afniGroup[] = null;

    private String asciiText[] = null;

    private String caret[] = null;

    private TransMatrix matrixQ = null;

    private TransMatrix matrixS = null;

    private String patientOrientationString = null;

    // Tells whether or not an extended DcmMeta header encoded with JavaScript Object Notation is present.
    private boolean haveDcmMeta = false;

    // The below fields are found in extended DcmMeta headers:

    // Required if an extended or replacement character set is used in one of the keys
    // DICOM:0008_0005 CS
    private String specificCharacterSet = null;

    // Image identification characteristics. The Image Type (0008,0008) Attribute identifies
    // important image identification characteristics.
    // These characteristics are: a. Pixel Data Characteristics. b. Patient Examination Characteristics.
    // c. Modality Specific Characteristics.
    // DICOM:0008_0008 CS
    private String imageType[] = null;

    // Time at which the acquisition of the study information was started.
    // DICOM:0008_0030 TM
    private String studyTime = null;

    // Time the Series started.
    // DICOM:0008_0031 TM
    private String seriesTime = null;

    // A departmental IS generated number that identifies the order for the Imaging Service Request.
    // DICOM:0008_0050 Imaging data attribute DICOM term SH
    private String accessionNumber = null;

    // Type of equipment that acquired the data used to create the images in this Study Component.
    // DICOM:0008_0060 CS
    private String modalityString = null;

    // Manufacturer of the equipment that produced the composite instances.
    // DICOM:0008_0070 LO
    private String manufacturer = null;

    // Manufacturers model name of the equipment that produced the composite instances.
    // DICOM:0008_1090 LO
    private String manufacturerModelName = null;

    // Description of the type of data taken. Enumerated Values: SE = Spin Echo IR = Inversion Recovery
    // GR = Gradient Recalled EP = Echo Planar RM = Research Mode Note: Multi-valued,
    // but not all combinations are valid (e.g. SE/GR, etc.).
    // DICOM:0018_0020 CS
    private String scanningSequence = null;

    // Variant of the Scanning Sequence. Defined Terms: SK = segmented k-space MTC = magnetization transfer contrast
    // SS = steady state TRSS = time reversed steady state SP = spoiled MP = MAG prepared OSP = oversampling phase
    // NONE = no sequence variant
    // DICOM:0018_0021 CS
    private String sequenceVariant = null;

    // Parameters of scanning sequence. Defined Terms: PER = Phase Encode Reordering
    // RG = Respiratory Gating CG = Cardiac Gating PPG = Peripheral Pulse Gating FC = Flow Compensation
    // PFF = Partial Fourier - Frequency PFP = Partial Fourier - Phase SP = Spatial Presaturation FS = Fat Saturation
    // MR IMAGE MODULE ATTRIBUTES
    // DICOM:0018_0022 CS
    private String scanOptions = null;

    // Identification of data encoding scheme. Enumerated Values: 2D = frequency x phase 3D = frequency x phase x phase
    // DICOM:0018_0023 CS
    private String MRAcquisitionType = null;

    // User defined name for the Scanning Sequence (0018,0020) and Sequence Variant (0018,0021) combination.
    // DICOM:0018_0024 SH
    private String sequenceName = null;

    // Angio Image Indicator. Primary image for Angio processing. Enumerated Values: Y = Image is Angio N = Image is not
    // Angio
    // DICOM:0018_0025 Imaging protocol attribute DICOM term CS
    private String angioFlag = null;

    // The period of time in msec between the beginning of a pulse sequence and the beginning of the
    // succeeding (essentially identical) pulse sequence. Required except when Scanning Sequence (0018,0020) is EP and
    // Sequence Variant (0018,0021) is not SK.
    // DICOM:0018_0080 DS
    private double repetitionTime = Double.NaN;

    // Time in ms between the middle of the excitation pulse and the peak of the echo produced (kx=0).
    // In the case of segmented k-space, the TE(eff) is the time between the middle of the excitation pulse to
    // the peak of the echo that is used to cover the center of the segment.
    // DICOM:0018_0081 Imaging protocol attribute DICOM term DS
    private double echoTime = Double.NaN;

    // Number of times a given pulse sequence is repeated before any parameter is changed.
    // DICOM:0018_0083 DS
    private double numberOfAverages = Double.NaN;

    // Precession frequency in MHz of the nucleus being imaged.
    // DICOM:0018_0084 DS
    private double imagingFrequency = Double.NaN;

    // Nucleus that is resonant at the imaging frequency. Examples: 31P, 1H.
    // DICOM:0018_0085 SH
    private String imagedNucleus = null;

    // The echo number used in generating this image. In the case of segmented k-space, it is the effective Echo Number.
    // DICOM:0018_0086 Imaging protocol attribute DICOM term IS
    private int echoNumbers = Integer.MIN_VALUE;

    // Nominal field strength of MR magnet in Tesla.
    // DICOM:0018_0087 DS
    private double magneticFieldStrength = Double.NaN;

    // Spacing between slices, in mm. The spacing is measured from the center-tocenter of each slice.
    // DICOM:0018_0088 DS
    private double spacingBetweenSlices = Double.NaN;

    // Total number of lines in k-space in the "y" direction collected during acquisition.
    // DICOM:0018_0089 IS
    private int numberOfPhaseEncodingSteps = Integer.MIN_VALUE;

    // Number of lines in k-space acquired per excitation per image.
    // DICOM:0018_0091 Imaging protocol attribute DICOM term IS
    private int echoTrainLength = Integer.MIN_VALUE;

    // Fraction of acquisition matrix lines acquired, expressed as a percent.
    // DICOM:0018_0093 DS
    private double percentSampling = Double.NaN;

    // Ratio of field of view dimension in phase direction to field of view dimension in frequency direction, expressed
    // as a percent.
    // DICOM:0018_0094 DS
    private double percentPhaseFieldOfView = Double.NaN;

    // Reciprocal of the total sampling period, in hertz per pixel.
    // DICOM:0018_0095 DS
    private double pixelBandwidth = Double.NaN;

    // Manufacturers designation of software version of the equipment that produced the composite instances.
    // DICOM:0018_1020 LO
    private String softwareVersions = null;

    // Transmit coil used.
    // DICOM:0018_1251 SH
    private String transmitCoilName = null;

    // Dimensions of the acquired frequency /phase data before reconstruction.
    // Multi-valued: frequency rowsfrequency columnsphase rowsphase columns.
    // DICOM:0018_1310 Imaging protocol attribute DICOM term US
    private int acquisitionMatrix[] = null;

    // The axis of phase encoding with respect to the image. Enumerated Values: ROW = phase encoded in rows.
    // COL = phase encoded in columns.
    // DICOM:0018_1312 CS
    private String inPlanePhaseEncodingDirection = null;

    // Steady state angle in degrees to which the magnetic vector is flipped from the magnetic vector of the primary
    // field.
    // DICOM:0018_1314 Imaging protocol attribute DICOM term DS
    private double flipAngle = Double.NaN;

    // Flip angle variation applied during image acquisition.
    // DICOM:0018_1315 CS
    private String variableFlipAngleFlag = null;

    // Calculated whole body Specific Absorption Rate in watts/kilogram.
    // DICOM:0018_1316 DS
    private double SAR = Double.NaN;

    // The rate of change of the gradient coil magnetic flux density with time (T/s).
    // DICOM:0018_1318 Imaging hardware attribute DICOM term DS
    private double dBdt = Double.NaN;

    // A number that identifies this Series.
    // DICOM:0020_0011 IS
    private int seriesNumber = Integer.MIN_VALUE;

    // The x, y, and z coordinates of the upper left hand corner (center of the first voxel transmitted) of the image,
    // in mm.
    // DICOM:0020_0032 DS
    private double imagePositionPatient[] = null;

    // The direction cosines of the first row and the first column with respect to the patient.
    // DICOM:0020_0037 DS
    private double imageOrientationPatient[] = null;

    // Relative position of exposure expressed in mm. The Slice Location (0020,1041) is defined as the
    // relative position of exposure expressed in mm. This information is relative to an unspecified implementation
    // specific reference point.
    // DICOM:0020_1041 DS
    private double sliceLocation = Double.NaN;

    // Number of samples (planes) in this image.
    // DICOM:0028_0002 US
    private int samplesPerPixel = Integer.MIN_VALUE;

    // Specifies the intended interpretation of the pixel data.
    // DICOM:0028_0004 CS
    private String photometricInterpretation = null;

    // Number of rows in the image
    // DICOM: 0028_0010 US
    private int rows = Integer.MIN_VALUE;

    // Number of columns in the image.
    // DICOM:0028_0011 US
    private int columns = Integer.MIN_VALUE;

    // Physical distance in the patient between the center of each pixel, specified by a numeric pair -
    // adjacent row spacing (delimiter) adjacent column spacing in mm.
    // DICOM:0028_0030 DS
    private double pixelSpacing[] = null;

    // Number of bits allocated for each pixel sample. Each sample shall have the same number of bits allocated.
    // See PS 3.5 for further explanation. DICOM:0028_0100 Imaging data attribute DICOM term US
    private int bitsAllocated = Integer.MIN_VALUE;

    // Number of bits stored for each pixel sample. Each sample shall have the same number of bits stored.
    // See PS 3.5 for further explanation. DICOM:0028_0101 Imaging data attribute DICOM term US
    private int bitsStored = Integer.MIN_VALUE;

    // Most significant bit for pixel sample data. Each sample shall have the same high bit.
    // DICOM:0028_0102 US
    private int highBit = Integer.MIN_VALUE;

    // Data representation of the pixel samples. Each sample shall have the same pixel representation.
    // DICOM:0028_0103 US
    private int pixelRepresentation = Integer.MIN_VALUE;

    // The minimum actual pixel value encountered in this image.
    // DICOM:0028_0106 US or SS
    private int smallestImagePixelValue = Integer.MAX_VALUE;

    // The maximum actual pixel value encountered in this image.
    // DICOM:0028_0107 US or SS
    private int largestImagePixelValue = Integer.MIN_VALUE;

    // Explanation of the Window Center and Width.
    // DICOM:0028_1055 LO
    private String windowCenterWidthExplanation = null;

    // Time at which the Performed Procedure Step started.
    // DICOM:0040_0245 TM
    private String performedProcedureStepStartTime = null;

    // Although some information overlaps between
    // the dicom standard tags and CSA headers, patient information is not included in the CSA. Therefore,
    // anonymization doesn't need to fiddle with the CSA headers

    // Fourier line position with the maximal echo for the performed acquisition
    private int CsaImageEchoLinePosition = Integer.MIN_VALUE;

    // Number of the slice beginning with 0
    private int CsaImageProtocolSliceNumber = Integer.MIN_VALUE;

    // 8 bit mask of the used receiver channels for the performed acquisition. Example: channel 0: 00000001 channel 3:
    // 00000111
    private int CsaImageUsedChannelMask = Integer.MIN_VALUE;

    private double CsaImageBandwidthPerPixelPhaseEncode = Double.NaN;

    // Number of performed fourier lines
    private int CsaImageMeasuredFourierLines = Integer.MIN_VALUE;

    // Parameters used for acquisition, e.g. door open, interpolation, raw filter, Siemens seqence .&#xB7;
    private int CsaImageSequenceMask = Integer.MIN_VALUE;

    private String CsaImageRFSWDDataType = null;

    private String CsaImageImaPATModeText = null;

    // The time in ns between the beginning of sampling one data point and the beginning of sampling of next data
    // point in the acquired signal. This means the dwell time is the sampling rate during digital conversion of an
    // acquired signal
    private int CsaImageRealDwellTime = Integer.MIN_VALUE;

    private String CsaImageImaCoilString = null;

    // Echo column position for the performed acquisition
    private int CsaImageEchoColumnPosition = Integer.MIN_VALUE;

    // Phase encoding direction: 0 = negative; 1 = positive
    private int CsaImagePhaseEncodingDirectionPositive = Integer.MIN_VALUE;

    private double CsaImageSlicePosition_PCS[] = null;

    private double CsaImageSliceNormalVector[] = null;

    private String CsaImageGSWDDataType = null;

    private int CsaImageMultistepIndex = Integer.MIN_VALUE;

    private int CsaImageImaRelTablePosition[] = null;

    // Number of slices in a mosaic image
    private int CsaImageNumberOfImagesInMosaic = Integer.MIN_VALUE;

    private int CsaImageNonPlanarImage = Integer.MIN_VALUE;

    // Echo partition position for the performed acquisition
    private int CsaImageEchoPartitionPosition = Integer.MIN_VALUE;

    // Used acquisition matrix description
    private String CsaImageAcquisitionMatrixText = null;

    private int CsaImageImaAbsTablePosition[] = null;

    private double CsaSeriesTalesReferencePower = Double.NaN;

    private int CsaSeriesOperation_mode_flag = Integer.MIN_VALUE;

    private double CsaSeriesdBdt_thresh = Double.NaN;

    private int CsaSeriesProtocolChangeHistory = Integer.MIN_VALUE;

    private double CsaSeriesGradientDelayTime[] = null;

    private double CsaSeriesSARMostCriticalAspect[] = null;

    private double CsaSeriesB1rms[] = null;

    private String CsaSeriesPATModeText = null;

    private int CsaSeriesRelTablePosition[] = null;

    private int CsaSeriesNumberOfPrescans = Integer.MIN_VALUE;

    private double CsaSeriesdBdt_limit = Double.NaN;

    private double CsaSeriesStim_lim[] = null;

    private String CsaSeriesPatReinPattern = null;

    private String CsaSeriesB1rmsSupervision = null;

    private double CsaSeriesReadoutGradientAmplitude = Double.NaN;

    private int CsaSeriesMrProtocolVersion = Integer.MIN_VALUE;

    private String CsaSeriesRFSWDMostCriticalAspect = null;

    private String CsaSeriesSequenceFileOwner = null;

    private String CsaSeriesGradientMode = null;

    private int CsaSeriesSliceArrayConcatenations = Integer.MIN_VALUE;

    private String CsaSeriesFlowCompensation = null;

    private double CsaSeriesTransmitterCalibration = Double.NaN;

    private int CsaSeriesIsocentered = Integer.MIN_VALUE;

    private int CsaSeriesAbsTablePosition = Integer.MIN_VALUE;

    private double CsaSeriesReadoutOS = Double.NaN;

    private double CsaSeriesdBdt_max = Double.NaN;

    private int CsaSeriesRFSWDOperationMode = Integer.MIN_VALUE;

    private double CsaSeriesSelectionGradientAmplitude = Double.NaN;

    private double CsaSeriesPhaseGradientAmplitude = Double.NaN;
    
    private int CsaSeriesRfWatchdogMask = Integer.MIN_VALUE;

    private String CsaSeriesCoilForGradient2 = null;

    private int CsaSeriesStim_mon_mode = Integer.MIN_VALUE;

    private int CsaSeriesCoilId[] = null;

    private double CsaSeriesStim_max_ges_norm_online = Double.NaN;

    private String CsaSeriesCoilString = null;

    private String CsaSeriesCoilForGradient = null;

    private int CsaSeriesTablePositionOrigin[] = null;

    private int CsaSeriesMiscSequenceParam[] = null;

    private String CsaSeriesLongModelName = null;

    private double CsaSeriesStim_faktor = Double.NaN;

    private double CsaSeriesSW_korr_faktor = Double.NaN;

    private double CsaSeriesSed[] = null;

    private String CsaSeriesPositivePCSDirections = null;

    private double CsaSeriesSliceResolution = Double.NaN;

    private double CsaSeriesStim_max_online[] = null;

    private double CsaSeriest_puls_max = Double.NaN;

    // Protocol version
    private int CsaSeriesMrPhoenixProtocolulVersion = Integer.MIN_VALUE;

    // Sequence file name for actual measurement protocol
    private String CsaSeriesMrPhoenixProtocoltSequenceFileName = null;

    // Name of actual measurement protocol
    private String CsaSeriesMrPhoenixProtocoltProtocolName = null;

    // Referenced image
    private String CsaSeriesMrPhoenixProtocoltReferenceImage0 = null;

    // Referenced image
    private String CsaSeriesMrPhoenixProtocoltReferenceImage1 = null;

    // Referenced image
    private String CsaSeriesMrPhoenixProtocoltReferenceImage2 = null;

    // Valid flag for desired table position in series block coordinate system
    private int CsaSeriesMrPhoenixProtocolucScanRegionPosValid = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolucTablePositioningMode = Integer.MIN_VALUE;

    // Baseline consistence info
    private String CsaSeriesMrPhoenixProtocolsProtConsistencyInfotBaselineString = null;

    private String CsaSeriesMrPhoenixProtocolsProtConsistencyInfotSystemType = null;

    // Nominal Bo compensation consistence
    private double CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflNominalB0 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflGMax = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflRiseTime = Double.NaN;

    private int CsaSeriesMrPhoenixProtocolsProtConsistencyInfolMaximumNofRxReceiverChannels = Integer.MIN_VALUE;

    // Eddy compensation x amplitude gradient system specification
    private double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude0 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude1 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude2 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude3 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude4 = Double.NaN;

    // Eddy compensation x time parameter gradient system specification
    private double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant0 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant1 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant2 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant3 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant4 = Double.NaN;

    // Eddy compensation y amplitude gradient system specification
    private double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude0 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude1 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude2 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude3 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude4 = Double.NaN;

    // Eddy compensation y time parameter gradient system specification
    private double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant0 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant1 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant2 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant3 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant4 = Double.NaN;

    // Eddy compensation z amplitude gradient system specification
    private double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude0 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude1 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude2 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude3 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude4 = Double.NaN;

    // Eddy compensation z time parameter gradient system specification
    private double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant0 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant1 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant2 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant3 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant4 = Double.NaN;

    private int CsaSeriesMrPhoenixProtocolsGRADSPECbEddyCompensationValid = Integer.MIN_VALUE;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude0 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude1 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude2 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant0 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant1 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant2 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude0 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude1 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude2 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant0 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant1 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant2 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude0 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude1 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude2 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant0 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant1 = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant2 = Double.NaN;

    // B0 compensation gradient system specification valid flag
    private int CsaSeriesMrPhoenixProtocolsGRADSPECbB0CompensationValid = Integer.MIN_VALUE;

    // Crossterm compensation xy amplitude gradient system specification
    private double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXYaflAmplitude0 = Double.NaN;

    // Crossterm compensation xy time parameter gradient system specification
    private double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXYaflTimeConstant0 = Double.NaN;

    // Crossterm compensation xz amplitude gradient system specification
    private double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXZaflAmplitude0 = Double.NaN;

    // Crossterm compensation xz time parameter gradient system specification
    private double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXZaflTimeConstant0 = Double.NaN;

    // Crossterm compensation yx amplitude gradient system specification
    private double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYXaflAmplitude0 = Double.NaN;

    // Crossterm compensation yx time parameter gradient system specification
    private double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYXaflTimeConstant0 = Double.NaN;

    // Crossterm compensation yz amplitude gradient system specification
    private double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYZaflAmplitude0 = Double.NaN;

    // Crossterm compensation yz time parameter gradient system specification
    private double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYZaflTimeConstant0 = Double.NaN;

    // Crossterm compensation zx amplitude gradient system specification
    private double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZXaflAmplitude0 = Double.NaN;

    // Crossterm compensation zx time parameter gradient system specification
    private double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZXaflTimeConstant0 = Double.NaN;

    // Crossterm compensation zy amplitude gradient system specification
    private double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZYaflAmplitude0 = Double.NaN;

    // Crossterm compensation zy time parameter gradient system specification
    private double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZYaflTimeConstant0 = Double.NaN;

    // "Crossterm compensation gradient system specification valid flag
    private int CsaSeriesMrPhoenixProtocolsGRADSPECbCrossTermCompensationValid = Integer.MIN_VALUE;

    // Gradient offset x direction [bit pattern]
    private int CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetX = Integer.MIN_VALUE;

    // Gradient offset y direction [bit pattern]
    private int CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetY = Integer.MIN_VALUE;

    // Gradient offset z direction [bit pattern]
    private int CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetZ = Integer.MIN_VALUE;

    // Gradient offsets valid flag
    private int CsaSeriesMrPhoenixProtocolsGRADSPECbOffsetValid = Integer.MIN_VALUE;

    // Gradient delay x direction
    private int CsaSeriesMrPhoenixProtocolsGRADSPEClDelayX = Integer.MIN_VALUE;

    // Gradient delay y direction
    private int CsaSeriesMrPhoenixProtocolsGRADSPEClDelayY = Integer.MIN_VALUE;

    // Gradient delay z direction
    private int CsaSeriesMrPhoenixProtocolsGRADSPEClDelayZ = Integer.MIN_VALUE;

    // Gradient delay valid flag
    private int CsaSeriesMrPhoenixProtocolsGRADSPECbDelayValid = Integer.MIN_VALUE;

    // Gradient sensitivity x direction [mT/m]
    private double CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityX = Double.NaN;

    // Gradient sensitivity y direction [mT/m]
    private double CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityY = Double.NaN;

    // Gradient sensitivity z direction [mT/m]
    private double CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityZ = Double.NaN;

    // Gradient sensitivity valid flag
    private int CsaSeriesMrPhoenixProtocolsGRADSPECbSensitivityValid = Integer.MIN_VALUE;

    private double CsaSeriesMrPhoenixProtocolsGRADSPECflGSWDMinRiseTime = Double.NaN;
    
    // Shim current parameter [mA]
    private int CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent0 = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent1 = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent2 = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent3 = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent4 = Integer.MIN_VALUE;

    // Shim current parameter valid flag
    private int CsaSeriesMrPhoenixProtocolsGRADSPECbShimCurrentValid = Integer.MIN_VALUE;

    // Gradient mode: fast, normal, whisper
    private int CsaSeriesMrPhoenixProtocolsGRADSPECucMode = Integer.MIN_VALUE;

    // Transmitter system nucleus
    private String CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0tNucleus = null;

    // Transmitter system frequency [Hz]
    private int CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0lFrequency = Integer.MIN_VALUE;

    // Frequency valid flag
    private int CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bFrequencyValid = Integer.MIN_VALUE;

    // Transmitter reference amplitude [V]
    private double CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0flReferenceAmplitude = Double.NaN;

    // Reference amplitude valid flag
    private int CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bReferenceAmplitudeValid = Integer.MIN_VALUE;

    // Transmitter amplitude correction factor, e.g. used for water suppression
    private double CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0flAmplitudeCorrection = Double.NaN;

    // Amplitude correction valid flag
    private int CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bAmplitudeCorrectionValid = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bRFPAIndexValid = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bFrequencyValid = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bReferenceAmplitudeValid = Integer.MIN_VALUE;

    private double CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1flAmplitudeCorrection = Double.NaN;

    private int CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bAmplitudeCorrectionValid = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1lRFPAIndex = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bRFPAIndexValid = Integer.MIN_VALUE;

    private String[] CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEntName = null;

    private int[] CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnbAmplitudeValid = null;

    private double[] CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnflAmplitude = null;

    private int CsaSeriesMrPhoenixProtocolsTXSPEClNoOfTraPulses = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsTXSPEClBCExcitationMode = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsTXSPEClBCSeqExcitationMode = Integer.MIN_VALUE;

    private double CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeMin = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeMax = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeClipLow = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeClipHigh = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsTXSPECflKDynPhaseMax = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsTXSPECflKDynPhaseClip = Double.NaN;

    private int CsaSeriesMrPhoenixProtocolsTXSPECbKDynValid = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsTXSPECucRFPulseType = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsTXSPECucExcitMode = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsTXSPECucSimultaneousExcitation = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsTXSPECucBCExcitationModeValid = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsRXSPEClGain = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsRXSPECbGainValid = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsRXSPECalDwellTime0 = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsAdjDatauiAdjFreMode = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsAdjDatauiAdjShimMode = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsAdjDatauiAdjWatSupMode = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsAdjDatauiAdjRFMapMode = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsAdjDatauiAdjMDSMode = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsAdjDatauiAdjTableTolerance = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsAdjDatauiAdjProtID = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsAdjDatauiAdjFreProtRelated = Integer.MIN_VALUE;

    private double CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondSag = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondCor = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondTra = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldSag = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldCor = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldTra = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedThickness = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedPhaseFOV = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedReadoutFOV = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedInPlaneRot = Double.NaN;

    private int CsaSeriesMrPhoenixProtocolsAdjDatauiAdjVolumeValid = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolucEnableNoiseAdjust = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolalTR0 = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocollContrasts = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolalTE0 = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolacFlowComp0 = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocollCombinedEchoes = Integer.MIN_VALUE;

    private double CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondSag[] = null;

    private double CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondCor[] = null;

    private double CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondTra[] = null;

    private double CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldSag[] = null;

    private double CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldCor[] = null;

    private double CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldTra[] = null;

    private double CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdThickness[] = null;

    private double CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdPhaseFOV[] = null;

    private double CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdReadoutFOV[] = null;

    private double CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdInPlaneRot[] = null;

    private int CsaSeriesMrPhoenixProtocolsSliceArrayanAsc[] = null;

    private int CsaSeriesMrPhoenixProtocolsSliceArrayanPos[] = null;

    private int CsaSeriesMrPhoenixProtocolsSliceArraylSize = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsSliceArraylConc = Integer.MIN_VALUE;

    // # CsaSeries.MrPhoenixProtocol.sSliceArray.ucMode indicates siemens slice order:
    // # Siemens; 1 Ascending, 2 Descending, and 4 Interleaved Ascending.
    // # NIFTI; 1 Ascending, 2 Descending, and 4 Interleaving Descending
    // # Siemens slice order 4 could be nifti slice order 3 or 5 depending on num_slices
    // # - nifti slice order 3: interleave_asc, odd first, odd num slices, interleave INC
    // # - nifti slice order 5: interleave_asc, even first, even num slices, interleave INC 2
    // self.slice_order = self.getelem(self._hdr, 'CsaSeries.MrPhoenixProtocol.sSliceArray.ucMode', None, 0)
    // if self.slice_order == 4: # don't try to guess if num_slices can't be determined
    // if self.num_slices % 2 != 0:
    // self.slice_order = 3 # interleaved ascending, odd first
    // else:
    // self.slice_order = 5 # interleaved ascending, even first
    private int CsaSeriesMrPhoenixProtocolsSliceArrayucMode = Integer.MIN_VALUE;

    private double CsaSeriesMrPhoenixProtocolsSliceArraysTSatdThickness = Double.NaN;

    private int CsaSeriesMrPhoenixProtocolsGroupArrayasGroup0nSize = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsGroupArrayanMember[] = null;

    private int CsaSeriesMrPhoenixProtocolsGroupArraylSize = Integer.MIN_VALUE;

    private double CsaSeriesMrPhoenixProtocolsGroupArraysPSatdThickness = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsGroupArraysPSatdGap = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondSag = Double.NaN;
    
    private double CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondCor = Double.NaN;
    
    private double CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondTra = Double.NaN;
    
    private double CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldSag = Double.NaN;
    
    private double CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldCor = Double.NaN;
    
    private double CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldTra = Double.NaN;
    
    private double CsaSeriesMrPhoenixProtocolsRSatArrayasElm0dThickness = Double.NaN;
    
    private int CsaSeriesMrPhoenixProtocolsRSatArraylSize = Integer.MIN_VALUE;
    
    private double CsaSeriesMrPhoenixProtocolsAutoAligndAAMatrix[] = null;

    private int CsaSeriesMrPhoenixProtocolsNavigatorParalBreathHoldMeas = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsNavigatorParalRespComp = Integer.MIN_VALUE;

    private double CsaSeriesMrPhoenixProtocolsBladeParadBladeCoverage = Double.NaN;

    private int CsaSeriesMrPhoenixProtocolsBladeParaucMotionCorr = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPrepPulsesucFatSat = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPrepPulsesucWaterSat = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPrepPulsesucInversion = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPrepPulsesucSatRecovery = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPrepPulsesucT2Prep = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPrepPulsesucTIScout = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPrepPulsesucFatSatMode = Integer.MIN_VALUE;

    private double CsaSeriesMrPhoenixProtocolsPrepPulsesdDarkBloodThickness = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsPrepPulsesdDarkBloodFlipAngle = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsPrepPulsesdT2PrepDuration = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsPrepPulsesdIRPulseThicknessFactor = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsKSpacedPhaseResolution = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsKSpacedSliceResolution = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsKSpacedAngioDynCentralRegionA = Double.NaN;

    private double CsaSeriesMrPhoenixProtocolsKSpacedAngioDynSamplingDensityB = Double.NaN;

    private int CsaSeriesMrPhoenixProtocolsKSpacelBaseResolution = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsKSpacelPhaseEncodingLines = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsKSpacelPartitions = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsKSpacelImagesPerSlab = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsKSpacelRadialViews = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsKSpacelRadialInterleavesPerImage = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsKSpacelLinesPerShot = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsKSpaceunReordering = Integer.MIN_VALUE;

    private double CsaSeriesMrPhoenixProtocolsKSpacedSeqPhasePartialFourierForSNR = Double.NaN;

    private int CsaSeriesMrPhoenixProtocolsKSpaceucPhasePartialFourier = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsKSpaceucSlicePartialFourier = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsKSpaceucAveragingMode = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsKSpaceucMultiSliceMode = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsKSpaceucDimension = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsKSpaceucTrajectory = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsKSpaceucViewSharing = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsKSpaceucAsymmetricEchoMode = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsKSpaceucPOCS = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsFastImaginglEPIFactor = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsFastImaginglTurboFactor = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsFastImaginglSliceTurboFactor = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsFastImaginglSegments = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsFastImagingucSegmentationMode = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsFastImaginglShots = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsFastImaginglEchoTrainDuration = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPhysioImaginglSignal1 = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPhysioImaginglMethod1 = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPhysioImaginglSignal2 = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPhysioImaginglMethod2 = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPhysioImaginglPhases = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPhysioImaginglRetroGatedImages = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerPulses = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerWindow = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlArrhythmiaDetection = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlCardiacGateOnThreshold = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlCardiacGateOffThreshold = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerIntervals = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerPulses = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerWindow = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselArrhythmiaDetection = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselCardiacGateOnThreshold = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselCardiacGateOffThreshold = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerIntervals = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerPulses = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerWindow = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlArrhythmiaDetection = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlCardiacGateOnThreshold = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlCardiacGateOffThreshold = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerIntervals = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioResplRespGateThreshold = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioResplRespGatePhase = Integer.MIN_VALUE;

    private double CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioRespdGatingRatio = Double.NaN;

    private int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioNativeucMode = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioNativeucFlowSenMode = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsSpecParalPhaseCyclingType = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsSpecParalPhaseEncodingType = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsSpecParalRFExcitationBandwidth = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsSpecParaucRemoveOversampling = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsSpecParalAutoRefScanNo = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsSpecParalDecouplingType = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsSpecParalNOEType = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsSpecParalExcitationType = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsSpecParalSpecAppl = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsSpecParalSpectralSuppression = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsDiffusionulMode = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsAngioucPCFlowMode = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsAngioucTOFInflow = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsAngiolDynamicReconMode = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsAngiolTemporalInterpolation = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsRawFilterlSlope_256 = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsRawFilterucOn = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsRawFilterucMode = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsDistortionCorrFilterucMode = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPatlAccelFactPE = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPatlAccelFact3D = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPatlRefLinesPE = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPatucPATMode = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPatucRefScanMode = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsPatucTPatAverageAllFrames = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsMDSulMdsModeMask = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsMDSulMdsVariableResolution = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsMDSlTableSpeedNumerator = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsMDSlmdsLinesPerSegment = Integer.MIN_VALUE;

    private double CsaSeriesMrPhoenixProtocolsMDSsMdsEndPosSBCS_mmdTra = Double.NaN;

    private int CsaSeriesMrPhoenixProtocolsMDSulMdsReconMode = Integer.MIN_VALUE;

    private double CsaSeriesMrPhoenixProtocolsMDSdMdsRangeExtension = Double.NaN;

    private int CsaSeriesMrPhoenixProtocolucEnableIntro = Integer.MIN_VALUE;
    
    private int CsaSeriesMrPhoenixProtocolucDisableChangeStoreImages = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolucAAMode = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolucAARegionMode = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolucAARefMode = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolucReconstructionMode = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolucOneSeriesForAllMeas = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolucPHAPSMode = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolucDixon = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolucDixonSaveOriginal = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolucWaitForPrepareCompletion = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocollAverages = Integer.MIN_VALUE;

    private double CsaSeriesMrPhoenixProtocoldAveragesDouble = Double.NaN;

    // = tDim-1
    private int CsaSeriesMrPhoenixProtocollRepetitions = Integer.MIN_VALUE;

    private double CsaSeriesMrPhoenixProtocoladFlipAngleDegree0 = Double.NaN;

    private int CsaSeriesMrPhoenixProtocollScanTimeSec = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocollTotalScanTimeSec = Integer.MIN_VALUE;

    private double CsaSeriesMrPhoenixProtocoldRefSNR = Double.NaN;

    private double CsaSeriesMrPhoenixProtocoldRefSNR_VOI = Double.NaN;

    private String CsaSeriesMrPhoenixProtocoltdefaultEVAProt = null;

    private String CsaSeriesMrPhoenixProtocolasCoilSelectMeas0tNucleus = null;

    private int CsaSeriesMrPhoenixProtocolasCoilSelectMeas0iUsedRFactor = Integer.MIN_VALUE;

    private String CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtCoilID[] = null;

    private int CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDlCoilCopy[] = null;

    private String CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtElement[] = null;

    private int CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlElementSelected[] = null;

    private int CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlRxChannelConnected[] = null;

    private int CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSaulPlugId[] = null;

    private int CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSauiNmbrOfNibbles[] = null;

    private double CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnflFactor[] = null;

    private int CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnbValid[] = null;

    private int CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnlRxChannel[] = null;

    private int CsaSeriesMrPhoenixProtocolsEFISPECbEFIDataValid = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsWiPMemBlockalFree[] = null;

    private double CsaSeriesMrPhoenixProtocolsWiPMemBlockadFree[] = null;

    private String CsaSeriesMrPhoenixProtocolsWiPMemBlocktFree = null;

    private int CsaSeriesMrPhoenixProtocolucBOLDParadigmArray[] = null;

    private int CsaSeriesMrPhoenixProtocollParadigmPeriodicity = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolucCineMode = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolucSequenceType = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolucCoilCombineMode = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolucFlipAngleMode = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocollTOM = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocollProtID = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolucReadOutMode = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolucBold3dPace = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolucForcePositioningOnNDIS = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolucInteractiveRealtime = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolucInternalTablePosValid = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsParametricMappingucParametricMap = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolsAslulMode = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolWaitForUserStart = Integer.MIN_VALUE;

    private int CsaSeriesMrPhoenixProtocolucAutoAlignInit = Integer.MIN_VALUE;

    // The time the acquisition of data started
    private double AcquisitionTime[] = null;

    // A number identifying the gathering of data over a period of time which resulted in this data set
    private int AcquisitionNumber[] = null;

    private int InstanceNumber[] = null;

    private double WindowCenter[] = null;
    
    private double WindowWidth[] = null;
    
    // Time delay after start of measurment
    private double CsaImageTimeAfterStart[] = null;

    // This array of arrays is too massive for display. Don't display unless requested.
    private double CsaImageMosaicRefAcqTimes[][] = null;

    // The 9 used ICE object dimensions of the performed acquisition. Combined/unset dimensions will be marked
    // with 'X'. E.g.: X_2_1_1_1_1_2_1_1"/>
    private String CsaImageICE_Dims[] = null;

    // Time duration between two slices of the performed acquisition
    private double CsaImageSliceMeasurementDuration[] = null;

    private double InstanceCreationTime[] = null;
    
    private double ContentTime[] = null;
    
    // The dictionaries of summarized meta data are encoded with JSON. A small amount of "meta meta" data that describes
    // the DcmMeta extension is also included. This includes the affine ('dcmmeta_affine'), shape ('dcmmeta_shape'),
    // any reorientation transform ('dcmmeta_reorient_transform'), and the slice dimension ('dcmmeta_slice_dim') of the
    // data described by the meta data. A version number for the DcmMeta extension ('dcmmeta_version') is also included.

    // The affine, shape, and slice dimension are used to determine if varying meta data is still valid. For example,
    // if the image affine no longer matches the meta data affine (i.e. the image has been coregistered) then we
    // cannot directly match the per-slice meta data values to slices of the data array.

    // The reorientation transform can be used to update directional meta data to match the image orientation. This
    // transform encodes any reordering of the voxel data that occured during conversion. If the image affine does
    // not match the meta data affine, then an additional transformation needs to be done after applying the
    // reorientation transform (from the meta data space to the image space).
    // Same as image.getExtents()
    private int dcmmeta_shape[] = null;

    private double dcmmeta_affine[][] = null;

    private double dcmmeta_reorient_transform[][] = null;

    private int dcmmeta_slice_dim = Integer.MIN_VALUE;

    private double dcmmeta_version = Double.NaN;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * file info storage constructor.
     * 
     * @param name file name
     * @param directory directory
     * @param format file format
     */
    public FileInfoNIFTI(final String name, final String directory, final int format) {
        super(name, directory, format);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Displays the file information.
     * 
     * @param dlog dialog box that is written to
     * @param matrix transformation matrix
     */
    @Override
    public void displayAboutInfo(final JDialogBase dlog, final TransMatrix matrix) {
        int i;
        int j;
        int mindIdentIndex = 0;
        int bValueIndex = 0;
        int sphericalDirectionIndex = 0;
        int dtComponentIndex = 0;
        int dtComponents;
        int sphericalHarmonicIndex = 0;
        int afniGroupIndex = 0;
        int asciiTextIndex = 0;
        int caretIndex = 0;
        final JDialogText dialog = (JDialogText) dlog;
        displayPrimaryInfo(dialog, matrix);
        dialog.append("\n\n                Other information\n\n");

        try {
            dialog.append("Description = " + descrip.trim() + "\n");
        } catch (final NullPointerException npe) {
            dialog.append("Description\n");
        }

        if (vox_offset != -1) { // vox offset

            dialog.append("Voxel Offset = " + Float.toString(vox_offset) + "\n");
        }

        switch (intentCode) {

            case FileInfoNIFTI.NIFTI_INTENT_NONE:
                dialog.append("Intent code = No intention\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_CORREL:
                dialog.append("Statistics code = Correlation coefficient R\n");
                dialog.append("Degrees of freedom = " + Integer.toString(Math.round(intentP1)) + "\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_TTEST:
                dialog.append("Statistics code = Student t test\n");
                dialog.append("Degrees of freedom = " + Integer.toString(Math.round(intentP1)) + "\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_FTEST:
                dialog.append("Statistics code = Fisher F statistic\n");
                dialog.append("Numerator degrees of freedom = " + Integer.toString(Math.round(intentP1)) + "\n");
                dialog.append("Denominator degrees of freedom = " + Integer.toString(Math.round(intentP2)) + "\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_ZSCORE:
                dialog.append("Statistics code = Standard normal - N(0,1) distributed\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_CHISQ:
                dialog.append("Statistics code = Chi - squared\n");
                dialog.append("Degrees of freedom = " + Integer.toString(Math.round(intentP1)) + "\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_BETA:
                dialog.append("Statistics code = Beta distribution\n");
                dialog.append("a parameter = " + Float.toString(intentP1) + "\n");
                dialog.append("b parameter = " + Float.toString(intentP2) + "\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_BINOM:
                dialog.append("Statistics code = Binomial distribution\n");
                dialog.append("Number of trials = " + Integer.toString(Math.round(intentP1)) + "\n");
                dialog.append("Probability per trial = " + Float.toString(intentP2) + "\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_GAMMA:
                dialog.append("Statistics code = Gamma with PDF = x**(shape-1) * exp(-Scale*x)\n");
                dialog.append("Shape = " + Float.toString(intentP1) + "\n");
                dialog.append("Scale = " + Float.toString(intentP2) + "\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_POISSON:
                dialog.append("Statistics code = Poisson distribution\n");
                dialog.append("Mean = " + Float.toString(intentP1) + "\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_NORMAL:
                dialog.append("Statistics code = Normal distribution\n");
                dialog.append("Mean = " + Float.toString(intentP1) + "\n");
                dialog.append("Standard deviation = " + Float.toString(intentP2) + "\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_FTEST_NONC:
                dialog.append("Statistics code = Nocentral F statistic\n");
                dialog.append("Numerator degrees of freedom = " + Integer.toString(Math.round(intentP1)) + "\n");
                dialog.append("Denominator degrees of freedom = " + Integer.toString(Math.round(intentP2)) + "\n");
                dialog.append("Numerator noncentrality parameter = " + Float.toString(intentP3) + "\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_CHISQ_NONC:
                dialog.append("Statistics code = Nocentral chi-squared statistic\n");
                dialog.append("Degrees of freedom = " + Integer.toString(Math.round(intentP1)) + "\n");
                dialog.append("Noncentrality parameter = " + Float.toString(intentP2) + "\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_LOGISTIC:
                dialog.append("Statistics code = Logistic distribution\n");
                dialog.append("Location = " + Float.toString(intentP1) + "\n");
                dialog.append("Scale = " + Float.toString(intentP2) + "\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_LAPLACE:
                dialog.append("Statistics code = Laplace distribution\n");
                dialog.append("Location = " + Float.toString(intentP1) + "\n");
                dialog.append("Scale = " + Float.toString(intentP2) + "\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_UNIFORM:
                dialog.append("Statistics code = Uniform distribution\n");
                dialog.append("Start = " + Float.toString(intentP1) + "\n");
                dialog.append("End = " + Float.toString(intentP2) + "\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_TTEST_NONC:
                dialog.append("Statistics code = Nocentral t statistic\n");
                dialog.append("Degrees of freedom = " + Integer.toString(Math.round(intentP1)) + "\n");
                dialog.append("Noncentrality parameter = " + Float.toString(intentP2) + "\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_WEIBULL:
                dialog.append("Statistics code = Weibull distribution\n");
                dialog.append("Location = " + Float.toString(intentP1) + "\n");
                dialog.append("Scale = " + Float.toString(intentP2) + "\n");
                dialog.append("Power = " + Float.toString(intentP3) + "\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_CHI:
                dialog.append("Statistics code = Chi distribution\n");

                final int p1 = Math.round(intentP1);
                dialog.append("Degrees of freedom = " + Integer.toString(p1) + "\n");
                if (p1 == 1) {
                    dialog.append("DOF = 1  Half normal distribution\n");
                } else if (p1 == 2) {
                    dialog.append("DOF = 2  Rayleigh distribution\n");
                } else if (p1 == 3) {
                    dialog.append("DOF = 3  Maxwell-Boltzmann distribution\n");
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_INVGAUSS:
                dialog.append("Statistics code = Inverse Gaussian\n");
                dialog.append("Mu = " + Float.toString(intentP1) + "\n");
                dialog.append("Lambda = " + Float.toString(intentP2) + "\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_EXTVAL:
                dialog.append("Statistics code = Extreme value type 1\n");
                dialog.append("Location = " + Float.toString(intentP1) + "\n");
                dialog.append("Scale = " + Float.toString(intentP2) + "\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_PVAL:
                dialog.append("Statistics code = Data is a p-value\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_LOGPVAL:
                dialog.append("Statistics code = Data is a ln(p-value)\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_LOG10PVAL:
                dialog.append("Statistics code = Data is a log10(p-value)\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_ESTIMATE:
                dialog.append("Intent code = Parameter estimate\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_LABEL:
                dialog.append("Intent code = Label index\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_NEURONAME:
                dialog.append("Intent code = NeuroNames labels index\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_GENMATRIX:
                dialog.append("Intent code = M x N matrix\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_SYMMATRIX:
                dialog.append("Intent code = N x N symmetric matrix\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_DISPVECT:
                dialog.append("Intent code = Displacement vector\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_VECTOR:
                dialog.append("Intent code = Vector\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_POINTSET:
                dialog.append("Intent code = Spatial coordinate\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_TRIANGLE:
                dialog.append("Intent code = Triple of indexes\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_QUATERNION:
                dialog.append("Intent code = Quaternion\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_DIMLESS:
                dialog.append("Intent code = Dimless\n");
                break;

            default:
                dialog.append("Unrecognized intent code = " + Short.toString(intentCode) + "\n");
        } // switch(intentCode)

        String coordString = null;

        switch (coord_code) {

            case NIFTI_XFORM_UNKNOWN:
                coordString = "Arbitrary";
                break;

            case NIFTI_XFORM_SCANNER_ANAT:
                coordString = "Scanner-based anatomical";
                break;

            case NIFTI_XFORM_ALIGNED_ANAT:
                coordString = "Aligned to another anatomy";
                break;

            case NIFTI_XFORM_TALAIRACH:
                coordString = "Talairach-Tournoux Atlas";
                break;

            case NIFTI_XFORM_MNI_152:
                coordString = "MNI 152 normalized coordinates";
                break;
        }

        dialog.append("X,Y,Z Coordinate system = " + coordString + "\n");

        if (coord_code2 > 0) {
            String coordString2 = null;

            switch (coord_code2) {

                case NIFTI_XFORM_UNKNOWN:
                    coordString2 = "Arbitrary";
                    break;

                case NIFTI_XFORM_SCANNER_ANAT:
                    coordString2 = "Scanner-based anatomical";
                    break;

                case NIFTI_XFORM_ALIGNED_ANAT:
                    coordString2 = "Aligned to another anatomy";
                    break;

                case NIFTI_XFORM_TALAIRACH:
                    coordString2 = "Talairach-Tournoux Atlas";
                    break;

                case NIFTI_XFORM_MNI_152:
                    coordString2 = "MNI 152 normalized coordinates";
                    break;
            }

            dialog.append("X,Y,Z Coordinate system 2 = " + coordString2 + "\n");
        } // if (coord_code2 > 0)

        String sourceTypeString = null;

        switch (sourceType) {

            case DT_UNKNOWN:
                sourceTypeString = "UNKNOWN";
                break;

            case DT_BINARY:
                sourceTypeString = "BOOLEAN";
                break;

            case NIFTI_TYPE_UINT8:
                sourceTypeString = "UNSIGNED BYTE";
                break;

            case NIFTI_TYPE_INT16:
                sourceTypeString = "SIGNED SHORT";
                break;

            case NIFTI_TYPE_INT32:
                sourceTypeString = "SIGNED INTEGER";
                break;

            case NIFTI_TYPE_FLOAT32:
                sourceTypeString = "FLOAT";
                break;

            case NIFTI_TYPE_COMPLEX64:
                sourceTypeString = "COMPLEX";
                break;

            case NIFTI_TYPE_FLOAT64:
                sourceTypeString = "DOUBLE";
                break;

            case NIFTI_TYPE_RGB24:
                sourceTypeString = "RGB";
                break;

            case NIFTI_TYPE_INT8:
                sourceTypeString = "SIGNED BYTE";
                break;

            case NIFTI_TYPE_UINT16:
                sourceTypeString = "UNSIGNED SHORT";
                break;

            case NIFTI_TYPE_INT64:
                sourceTypeString = "SIGNED LONG";
                break;

            case NIFTI_TYPE_UINT64:
                sourceTypeString = "UNSIGNED LONG";
                break;

            case NIFTI_TYPE_FLOAT128:
                sourceTypeString = "128 BIT FLOAT";
                break;

            case NIFTI_TYPE_COMPLEX128:
                sourceTypeString = "DCOMPLEX";
                break;

            case NIFTI_TYPE_COMPLEX256:
                sourceTypeString = "256 bit COMPLEX";
                break;
        }

        dialog.append("Source type = " + sourceTypeString + "\n");

        dialog.append("Slope scale = " + Float.toString(scl_slope) + "\n");

        dialog.append("Added offset = " + Float.toString(scl_inter) + "\n");

        switch (freq_dim) {

            case 0:
                dialog.append("Frequency encoding direction = none\n");
                break;

            case 1:
                dialog.append("Frequency encoding direction = x dimension\n");
                break;

            case 2:
                dialog.append("Frequency encoding direction = y dimension\n");
                break;

            case 3:
                dialog.append("Frequency encoding direction = z dimension\n");
                break;
        }

        switch (phase_dim) {

            case 0:
                dialog.append("Phase encoding direction = none\n");
                break;

            case 1:
                dialog.append("Phase encoding direction = x dimension\n");
                break;

            case 2:
                dialog.append("Phase encoding direction = y dimension\n");
                break;

            case 3:
                dialog.append("Phase encoding direction = z dimension\n");
                break;
        }

        switch (slice_dim) {

            case 0:
                dialog.append("Slice acquisition direction = none\n");
                break;

            case 1:
                dialog.append("Slice acquisition direction = x dimension\n");
                break;

            case 2:
                dialog.append("Slice acquisition direction = y dimension\n");
                break;

            case 3:
                dialog.append("Slice acquisition direction = z dimension\n");
                break;
        }

        if (sliceDuration > 0) {
            dialog.append("Time used to acquire 1 slice = " + String.valueOf(sliceDuration) + "\n");
        }

        if (sliceCode == NIFTI_SLICE_SEQ_INC) {
            dialog.append("Timing pattern of slice acquisition = sequentially increasing\n");
        } else if (sliceCode == NIFTI_SLICE_SEQ_DEC) {
            dialog.append("Timing pattern of slice acquisition = sequentially decreasing\n");
        } else if (sliceCode == NIFTI_SLICE_ALT_INC) {
            dialog.append("Timing pattern of slice acquisition = alternately increasing\n");
        } else if (sliceCode == NIFTI_SLICE_ALT_DEC) {
            dialog.append("Timing pattern of slice acquisition = alternately decreasing\n");
        } else if (sliceCode == NIFTI_SLICE_ALT_INC2) {
            dialog.append("Timing pattern of slice acquisition = alternately increasing #2\n");
        } else if (sliceCode == NIFTI_SLICE_ALT_DEC2) {
            dialog.append("Timing pattern of slice acquisition = alternately decreasing #2\n");
        }

        if (sliceStart > 0) {
            dialog.append("Timing pattern starts at slice " + String.valueOf(sliceStart + 1) + "\n");
        }

        if (sliceEnd > 0) {
            dialog.append("Timing pattern ends at slice " + String.valueOf(sliceEnd + 1) + "\n");
        }

        dialog.append("Axis: x-orientation = " + getAxisOrientationStr(super.getAxisOrientation(0)) + "\n");
        dialog.append("Axis: y-orientation = " + getAxisOrientationStr(super.getAxisOrientation(1)) + "\n");
        dialog.append("Axis: z-orientation = " + getAxisOrientationStr(super.getAxisOrientation(2)) + "\n");

        dialog.append("X Origin: " + Float.toString(super.getOrigin(0)) + "\n");
        dialog.append("Y Origin: " + Float.toString(super.getOrigin(1)) + "\n");
        dialog.append("Z Origin: " + Float.toString(super.getOrigin(2)) + "\n");

        if (cal_min != -1) {
            dialog.append("cal_min = " + Float.toString(cal_min) + "\n");
        }

        if (cal_max != -1) {
            dialog.append("cal_max = " + Float.toString(cal_max) + "\n");
        }

        if (bitpix != -1) {
            dialog.append("Bits per Pixel = " + Integer.toString(bitpix) + "\n");
        }

        if (aux_file != null) {

            if (aux_file.trim().length() > 0) {
                dialog.append("aux = " + aux_file.trim() + "\n");
            }
        }

        if (intentName != null) {
            dialog.append("Name or meaning of data = " + intentName + "\n");
        }

        if ( (esize != null) && (ecode != null)) {
            mindIdentIndex = 0;
            bValueIndex = 0;
            sphericalDirectionIndex = 0;
            dtComponentIndex = 0;
            afniGroupIndex = 0;
            asciiTextIndex = 0;
            caretIndex = 0;
            if (esize.length == 1) {
                dialog.append("Extended header has " + esize.length + " header field\n");
            } else {
                dialog.append("Extended header has " + esize.length + " header fields\n");
            }
            for (i = 0; i < esize.length; i++) {
                // dialog.append("Header field number " + (i+1) + " size in bytes = " + esize[i] + "\n");
                dialog.append("Header field number " + (i + 1) + " has " + ecodeIntToString(ecode[i]) + "\n");
                switch (ecode[i]) {
                    case NIFTI_ECODE_AFNI:
                        dialog.append("AFNI GROUP field number " + (afniGroupIndex + 1) + " has:\n");
                        dialog.append(afniGroup[afniGroupIndex++].trim() + "\n\n");
                        break;
                    case NIFTI_ECODE_COMMENT:
                        dialog.append("ASCII TEXT field number " + (asciiTextIndex + 1) + " has:\n");
                        dialog.append(asciiText[asciiTextIndex++].trim() + "\n\n");
                        break;
                    case NIFTI_ECODE_MIND_IDENT:
                        dialog.append("MIND_IDENT field number " + (mindIdentIndex + 1) + " = " + mindIdent[mindIdentIndex].trim() + "\n");
                        mindIdentIndex++;
                        break;
                    case NIFTI_ECODE_B_VALUE:
                        dialog.append("B_VALUE field number " + (bValueIndex + 1) + " = " + bValue[bValueIndex] + " s/(mm*mm)\n");
                        bValueIndex++;
                        break;
                    case NIFTI_ECODE_SPHERICAL_DIRECTION:
                        dialog.append("SPHERICAL DIRECTION field number " + (sphericalDirectionIndex + 1) + " has:\n");
                        dialog.append("Azimuthal angle = " + azimuth[sphericalDirectionIndex] + " radians\n");
                        dialog.append("Zenith angle = " + zenith[sphericalDirectionIndex] + " radians\n");
                        sphericalDirectionIndex++;
                        break;
                    case NIFTI_ECODE_DT_COMPONENT:
                        dialog.append("Diffusion Tensor field number " + (dtComponentIndex + 1) + " has:\n");
                        dtComponents = dtComponent[dtComponentIndex].length;
                        for (j = 0; j < dtComponents; j++) {
                            dialog.append("Component index " + (j + 1) + " = " + dtComponent[dtComponentIndex][j] + "\n");
                        }
                        dtComponentIndex++;
                        break;
                    case NIFTI_ECODE_SHC_DEGREEORDER:
                        dialog.append("Spherical harmonic basis function number " + (sphericalHarmonicIndex + 1) + " has:\n");
                        dialog.append("Degree = " + degree[sphericalHarmonicIndex] + "\n");
                        dialog.append("Order = " + order[sphericalHarmonicIndex] + "\n");
                        sphericalHarmonicIndex++;
                        break;
                    case NIFTI_ECODE_CARET:
                        dialog.append("Caret field number " + (caretIndex + 1) + " has:\n");
                        dialog.append(caret[caretIndex++].trim() + "\n\n");
                        break;
                }
            } // for (i = 0; i < esize.length; i++)
        } // if ((esize != null) && (ecode != null))
        else {
            dialog.append("No extended header is present\n");
        }

        if (haveDcmMeta) {
        	dialog.append("An extended DcmMeta Header encoded with JSON has:\n");
        	if (specificCharacterSet != null) {
        		dialog.append("Specific character set = " + specificCharacterSet + "\n");
        	}
        	if (imageType != null) {
        		for (i = 0; i < imageType.length; i++) {
        			dialog.append("Image type["+i+"] = " + imageType[i] + "\n");
        		}
        	}
        	if (studyTime != null) {
        		dialog.append("Study time = " + studyTime + "\n");
        	}
        	if (seriesTime != null) {
        		dialog.append("Series time = " + seriesTime + "\n");
        	}
        	if (accessionNumber != null) {
        		dialog.append("Accession number = " + accessionNumber + "\n");
        	}
        	if (modalityString != null) {
        		dialog.append("Modality = " + modalityString + "\n");
        	}
        	if (manufacturer != null) {
        		dialog.append("Manufacturer = " + manufacturer + "\n");
        	}
        	if (manufacturerModelName != null) {
        		dialog.append("Manufacturer model name = " + manufacturerModelName + "\n");
        	}
        	if (scanningSequence != null) {
        		if (scanningSequence.equalsIgnoreCase("SE")) {
        			dialog.append("Scanning sequence = Spin Echo\n");
        		}
        		else if (scanningSequence.equalsIgnoreCase("IR")) {
        			dialog.append("Scanning sequence = Inversion Recovery\n");
        		}
        		else if (scanningSequence.equalsIgnoreCase("GR")) {
        			dialog.append("Scanning sequence = Gradient Recalled\n");
        		}
        		else if (scanningSequence.equalsIgnoreCase("EP")) {
        			dialog.append("Scanning sequence = Echo Planar\n");
        		}
        		else if (scanningSequence.equalsIgnoreCase("RM")) {
        			dialog.append("Scanning sequence = Research Mode\n");
        		}
        		else {
        		    dialog.append("Scanning sequence = " + scanningSequence + "\n");
        		}
        	}
        	if (sequenceVariant != null) {
        		if (sequenceVariant.equalsIgnoreCase("SK")) {
        			dialog.append("Sequence variant = segmented k-space\n");
        		}
        		else if (sequenceVariant.equalsIgnoreCase("MTC")) {
        			dialog.append("Sequence variant = magnetization transfer constant\n");
        		}
        		else if (sequenceVariant.equalsIgnoreCase("SS")) {
        			dialog.append("Sequence variant = steady state\n");
        		}
        		else if (sequenceVariant.equalsIgnoreCase("TRSS")) {
        			dialog.append("Sequence variant = time reversed steady state\n");
        		}
        		else if (sequenceVariant.equalsIgnoreCase("SP")) {
        			dialog.append("Sequence variant = spoiled\n");
        		}
        		else if (sequenceVariant.equalsIgnoreCase("MAG")) {
        			dialog.append("Sequence variant = MAG prepared\n");
        		}
        		else if (sequenceVariant.equalsIgnoreCase("OSP")) {
        			dialog.append("Sequence variant = oversampling phase\n");
        		}
        		else if (sequenceVariant.equalsIgnoreCase("NONE")) {
        			dialog.append("Sequence variant = no sequence variant\n");
        		}
        		else {
        		    dialog.append("Sequence variant = " + sequenceVariant + "\n");
        		}
        	}
        	if (scanOptions != null) {
        		if (scanOptions.equalsIgnoreCase("RG")) {
        			dialog.append("Scan options = Respiratory Gating\n");
        		}
        		else if (scanOptions.equalsIgnoreCase("CG")) {
        			dialog.append("Scan options = Cardiac Gating\n");
        		}
        		else if (scanOptions.equalsIgnoreCase("PPG")) {
        			dialog.append("Scan options = Peripheral Pulse Gating\n");
        		}
        		else if (scanOptions.equalsIgnoreCase("FC")) {
        			dialog.append("Scan options = Flow Compensation\n");
        		}
        		else if (scanOptions.equalsIgnoreCase("PFF")) {
        			dialog.append("Scan options = Partial Fourier Frequency\n");
        		}
        		else if (scanOptions.equalsIgnoreCase("PFP")) {
        			dialog.append("Scan options = Partial Fourier Phase\n");
        		}
        		else if (scanOptions.equalsIgnoreCase("SP")) {
        			dialog.append("Scan options = Spatial Presaturation\n");
        		}
        		else if (scanOptions.equalsIgnoreCase("FS")) {
        			dialog.append("Scan options = Fat Saturation\n");
        		}
        		else {
        		    dialog.append("Scan options = " + scanOptions + "\n");
        		}
        	}
        	if (MRAcquisitionType != null) {
        		if (MRAcquisitionType.equalsIgnoreCase("2D")) {
        			dialog.append("MR acquisition type = frequency x phase\n");
        		}
        		else if (MRAcquisitionType.equalsIgnoreCase("3D")) {
        			dialog.append("MR acquisition type = frequency x phase x phase\n");
        		}
        		else {
        		    dialog.append("MR acquisition type = " + MRAcquisitionType + "\n");
        		}
        	}
        	if (sequenceName != null) {
        		dialog.append("Sequence name = " + sequenceName + "\n");
        	}
        	if (angioFlag != null) {
        		if (angioFlag.equalsIgnoreCase("Y")) {
        			dialog.append("Image is Angio\n");
        		}
        		else if (angioFlag.equalsIgnoreCase("N")) {
        			dialog.append("Image is not Angio\n");
        		}
        		else {
        		    dialog.append("Angio flag = " + angioFlag + "\n");
        		}
        	}
        	if (!Double.isNaN(repetitionTime)) {
        		dialog.append("Repetition time = " + repetitionTime + " msec\n");
        	}
        	if (!Double.isNaN(echoTime)) {
        		dialog.append("Echo time = " + echoTime + " msec\n");
        	}
        	if (!Double.isNaN(numberOfAverages)) {
        		dialog.append("Number of averages = " + numberOfAverages + "\n");
        	}
        	if (!Double.isNaN(imagingFrequency)) {
        		dialog.append("Imaging frequency = " + imagingFrequency + " MHz\n");
        	}
        	if (imagedNucleus != null) {
        		dialog.append("Imaged nucleus = " + imagedNucleus + "\n");
        	}
        	if (echoNumbers != Integer.MIN_VALUE) {
        		dialog.append("Echo numbers = " + echoNumbers + "\n");
        	}
        	if (!Double.isNaN(magneticFieldStrength)) {
        		dialog.append("Magnetic field strength = " + magneticFieldStrength + " Tesla\n");
        	}
        	if (!Double.isNaN(spacingBetweenSlices)) {
        		dialog.append("Spacing between slices = " + spacingBetweenSlices + " mm\n");
        	}
        	if (numberOfPhaseEncodingSteps != Integer.MIN_VALUE) {
        		dialog.append("Number of phase encoding steps = " + numberOfPhaseEncodingSteps + "\n");
        	}
        	if (echoTrainLength != Integer.MIN_VALUE) {
        		dialog.append("Echo train length = " + echoTrainLength + "\n");
        	}
        	if (!Double.isNaN(percentSampling)) {
        		dialog.append("Percent sampling = " + percentSampling + "\n");
        	}
        	if (!Double.isNaN(percentPhaseFieldOfView)) {
        		dialog.append("Percent phase field of view = " + percentPhaseFieldOfView  + "\n");
        	}
        	if (!Double.isNaN(pixelBandwidth)) {
        		dialog.append("Pixel bandwidth = " + pixelBandwidth + " hertz per pixel\n");
        	}
        	if (softwareVersions != null) {
        		dialog.append("Software versions = " + softwareVersions + "\n");
        	}
        	if (transmitCoilName != null) {
        		dialog.append("Transmit coil name = " + transmitCoilName + "\n");
        	}
        	if (acquisitionMatrix != null) {
        	    for (i = 0; i < acquisitionMatrix.length; i++) {
        	    	dialog.append("Acquisition matrix["+i+"] = " + acquisitionMatrix[i] + "\n");
        	    }
        	}
        	// ROW = phase encoded in rows. 
        		    // COL = phase encoded in columns.
        	if (inPlanePhaseEncodingDirection != null) {
        		if (inPlanePhaseEncodingDirection.equalsIgnoreCase("ROW")) {
        			dialog.append("In plane phase encoding direction has phase encoded in rows\n");
        		}
        		else if (inPlanePhaseEncodingDirection.equalsIgnoreCase("COL")) {
        			dialog.append("In plane phase encoding direction has phase encoded in columns\n");
        		}
        		else {
        		    dialog.append("In plane phase encoding direction = " + inPlanePhaseEncodingDirection + "\n");
        		}
        	}
        	if (!Double.isNaN(flipAngle)) {
        		dialog.append("Flip angle = " + flipAngle + " degrees\n");
        	}
        	if (variableFlipAngleFlag != null) {
        		if (variableFlipAngleFlag.equalsIgnoreCase("Y")) {
        			dialog.append("Flip angle variation applied during image acquisition\n");
        		}
        		else if (variableFlipAngleFlag.equalsIgnoreCase("N")) {
        			dialog.append("Flip angle variation not applied during image acquisition\n");
        		}
        		else {
        		    dialog.append("Variable flip angle flag = " + variableFlipAngleFlag + "\n");
        		}
        	}
        	if (!Double.isNaN(SAR)) {
        		dialog.append("Calculated whole body specific absorption rate = " + SAR + " watts/kilogram\n");
        	}
        	if (!Double.isNaN(dBdt)) {
        		dialog.append("Rate of change of magnetic coil flux density with time = " + dBdt + " T/s\n");
        	}
        	if (seriesNumber != Integer.MIN_VALUE) {
        		dialog.append("Series number = " + seriesNumber + "\n");
        	}
        	if (imagePositionPatient != null) {
        		for (i = 0; i < imagePositionPatient.length; i++) {
        			dialog.append("Image position patient["+i+"] = " + imagePositionPatient[i] + " mm\n");
        		}
        	}
        	if (imageOrientationPatient != null) {
        		for (i = 0; i < imageOrientationPatient.length; i++) {
        			dialog.append("Image orientation patient["+i+"] = " + imageOrientationPatient[i] + "\n");
        		}
        	}
        	if (!Double.isNaN(sliceLocation)) {
        		dialog.append("Slice location = " + sliceLocation + " mm\n");
        	}
        	if (samplesPerPixel != Integer.MIN_VALUE) {
        		dialog.append("Samples per pixel = " + samplesPerPixel + "\n");
        	}
        	if (photometricInterpretation != null) {
        		dialog.append("Photometric interpretation = " + photometricInterpretation + "\n");
        	}
        	if (rows != Integer.MIN_VALUE) {
        		dialog.append("Rows = " + rows + "\n");
        	}
        	if (columns != Integer.MIN_VALUE) {
        		dialog.append("Columns = " + columns + "\n");
        	}
        	if (pixelSpacing != null) {
        		for (i = 0; i < pixelSpacing.length; i++) {
        			dialog.append("Pixel spacing["+i+"] = " + pixelSpacing[i] + " mm\n");
        		}
        	}
        	if (bitsAllocated != Integer.MIN_VALUE) {
        		dialog.append("Bits allocated = " + bitsAllocated + "\n");
        	}
        	if (bitsStored != Integer.MIN_VALUE) {
        		dialog.append("Bits stored = " + bitsStored + "\n");
        	}
        	if (highBit != Integer.MIN_VALUE) {
        		dialog.append("High bit = " + highBit + "\n");
        	}
        	if (pixelRepresentation != Integer.MIN_VALUE) {
        		dialog.append("Pixel representation = " + pixelRepresentation + "\n");
        	}
        	if (smallestImagePixelValue != Integer.MAX_VALUE) {
        		dialog.append("Smallest image pixel value = " + smallestImagePixelValue + "\n");
        	}
        	if (largestImagePixelValue != Integer.MIN_VALUE) {
        		dialog.append("Largest image pixel value = "+ largestImagePixelValue + "\n");
        	}
        	if (windowCenterWidthExplanation != null) {
        		dialog.append("Window center width explanation = " + windowCenterWidthExplanation + "\n");
        	}
        	if (performedProcedureStepStartTime != null) {
        		dialog.append("Performed procedure step start time = " + performedProcedureStepStartTime + "\n");
        	}
        	if (CsaImageEchoLinePosition != Integer.MIN_VALUE) {
        		dialog.append("CsaImage.EchoLinePosition = " + CsaImageEchoLinePosition + "\n");
        	}
        	if (CsaImageProtocolSliceNumber != Integer.MIN_VALUE) {
        		dialog.append("CsaImage.ProtocolSliceNumber = " + CsaImageProtocolSliceNumber + "\n");
        	}
        	if (CsaImageUsedChannelMask != Integer.MIN_VALUE) {
        		dialog.append("CsaImage.UsedChannelMask = " + CsaImageUsedChannelMask + "\n");
        	}
        	if (!Double.isNaN(CsaImageBandwidthPerPixelPhaseEncode)) {
        		dialog.append("CsaImage.BandwidthPerPixelPhaseEncode = " + CsaImageBandwidthPerPixelPhaseEncode + "\n");
        	}
        	if (CsaImageMeasuredFourierLines != Integer.MIN_VALUE) {
        		dialog.append("CsaImage.MeasuredFourierLines = " + CsaImageMeasuredFourierLines + "\n");
        	}
        	if (CsaImageSequenceMask != Integer.MIN_VALUE) {
        		dialog.append("CsaImage.SequenceMask = " + CsaImageSequenceMask + "\n");
        	}
        	if (CsaImageRFSWDDataType != null) {
        		dialog.append("CsaImage.RFSWDDataType = " + CsaImageRFSWDDataType + "\n");
        	}
        	if (CsaImageImaPATModeText != null) {
        		dialog.append("CsaImage.ImaPATModeText = " + CsaImageImaPATModeText + "\n");
        	}
        	if (CsaImageRealDwellTime != Integer.MIN_VALUE) {
        		dialog.append("CsaImage.RealDwellTime = " + CsaImageRealDwellTime + "\n");
        	}
        	if (CsaImageImaCoilString != null) {
        		dialog.append("CsaImage.ImaCoilString = " + CsaImageImaCoilString + "\n");
        	}
        	if (CsaImageEchoColumnPosition != Integer.MIN_VALUE) {
        		dialog.append("CsaImage.EchoColumnPosition = " + CsaImageEchoColumnPosition + "\n");
        	}
        	if (CsaImagePhaseEncodingDirectionPositive != Integer.MIN_VALUE) {
        		dialog.append("CsaImage.PhaseEncodingDirectionPositive = " + CsaImagePhaseEncodingDirectionPositive + "\n");
        	}
        	if (CsaImageSlicePosition_PCS != null) {
        		for (i = 0; i < CsaImageSlicePosition_PCS.length; i++) {
        			dialog.append("CsaImage.SlicePosition_PCS["+i+"] = " + CsaImageSlicePosition_PCS[i] + "\n");
        		}
        	}
        	if (CsaImageSliceNormalVector != null) {
        		for (i = 0; i < CsaImageSliceNormalVector.length; i++) {
        			dialog.append("CsaImage.SliceNormalVector["+i+"] = " + CsaImageSliceNormalVector[i] + "\n");
        		}
        	}
        	if (CsaImageGSWDDataType != null) {
        		dialog.append("CsaImage.GSWDDataType = " + CsaImageGSWDDataType + "\n");
        	}
        	if (CsaImageMultistepIndex != Integer.MIN_VALUE) {
        		dialog.append("CsaImage.MultistepIndex = " + CsaImageMultistepIndex + "\n");
        	}
        	if (CsaImageImaRelTablePosition != null) {
        		for (i = 0; i < CsaImageImaRelTablePosition.length; i++) {
        			dialog.append("CsaImage.ImaRelTablePosition["+i+"] = " + CsaImageImaRelTablePosition[i] + "\n");
        		}
        	}
        	if (CsaImageNumberOfImagesInMosaic != Integer.MIN_VALUE) {
        		dialog.append("CsaImage.NumberOfImagesInMosaic = " + CsaImageNumberOfImagesInMosaic + "\n");
        	}
        	if (CsaImageNonPlanarImage != Integer.MIN_VALUE) {
        		dialog.append("CsaImage.NonPlanarImage = " + CsaImageNonPlanarImage + "\n");
        	}
        	if (CsaImageEchoPartitionPosition != Integer.MIN_VALUE) {
        		dialog.append("CsaImage.EchoPartitionPosition = " + CsaImageEchoPartitionPosition + "\n");
        	}
        	if (CsaImageAcquisitionMatrixText != null) {
        		dialog.append("CsaImage.AcquisitionMatrixText = " + CsaImageAcquisitionMatrixText + "\n");
        	}
        	if (CsaImageImaAbsTablePosition != null) {
        		for (i = 0; i < CsaImageImaAbsTablePosition.length; i++) {
        			dialog.append("CsaImage.ImaAbsTablePosition["+i+"] = " + CsaImageImaAbsTablePosition[i] + "\n");
        		}
        	}
        	if (!Double.isNaN(CsaSeriesTalesReferencePower)) {
        		dialog.append("CsaSeries.TalesReferencePower = " + CsaSeriesTalesReferencePower + "\n");
        	}
        	if (CsaSeriesOperation_mode_flag != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.Operation_mode_flag = " + CsaSeriesOperation_mode_flag + "\n");
        	}
        	if (!Double.isNaN(CsaSeriesdBdt_thresh)) {
        		dialog.append("CsaSeries.dBdt_thresh = " + CsaSeriesdBdt_thresh + "\n");
        	}
        	if (CsaSeriesProtocolChangeHistory != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.ProtocolChangeHistory = " + CsaSeriesProtocolChangeHistory + "\n");
        	}
        	if (CsaSeriesGradientDelayTime != null) {
        		for (i = 0; i < CsaSeriesGradientDelayTime.length; i++) {
        			dialog.append("CsaSeries.GradientDelayTime["+i+"] = " + CsaSeriesGradientDelayTime[i] + "\n");
        		}
        	}
        	if (CsaSeriesSARMostCriticalAspect != null) {
        		for (i = 0; i < CsaSeriesSARMostCriticalAspect.length; i++) {
        			dialog.append("CsaSeries.SARMostCriticalAspect["+i+"] = " + CsaSeriesSARMostCriticalAspect[i] + "\n");
        		}
        	}
        	if (CsaSeriesB1rms != null) {
        		for (i = 0; i < CsaSeriesB1rms.length; i++) {
        			dialog.append("CsaSeries.B1rms["+i+"] = " + CsaSeriesB1rms[i] + "\n");
        		}
        	}
        	if (CsaSeriesPATModeText != null) {
        		dialog.append("CsaSeries.PATModeText = " + CsaSeriesPATModeText + "\n");
        	}
        	if (CsaSeriesRelTablePosition != null) {
        		for (i = 0; i < CsaSeriesRelTablePosition.length; i++) {
        			dialog.append("CsaSeries.RelTablePosition["+i+"] = " + CsaSeriesRelTablePosition[i] + "\n");
        		}
        	}
        	if (CsaSeriesNumberOfPrescans != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.NumberOfPrescans = " + CsaSeriesNumberOfPrescans + "\n");
        	}
        	if (!Double.isNaN(CsaSeriesdBdt_limit)) {
        		dialog.append("CsaSeries.dBdt_limit = " + CsaSeriesdBdt_limit + "\n");
        	}
        	if (CsaSeriesStim_lim != null) {
        		for (i = 0; i < CsaSeriesStim_lim.length; i++) {
        			dialog.append("CsaSeries.Stim_lim["+i+"] = " + CsaSeriesStim_lim[i] + "\n");
        		}
        	}
        	if (CsaSeriesPatReinPattern != null) {
        		dialog.append("CsaSeries.PatReinPattern = " + CsaSeriesPatReinPattern + "\n");
        	}
        	if (CsaSeriesB1rmsSupervision != null) {
        		dialog.append("CsaSeries.B1rmsSupervision = " + CsaSeriesB1rmsSupervision + "\n");
        	}
        	if (!Double.isNaN(CsaSeriesReadoutGradientAmplitude)) {
        		dialog.append("CsaSeries.ReadoutGradientAmplitude = " + CsaSeriesReadoutGradientAmplitude + "\n");
        	}
        	if (CsaSeriesMrProtocolVersion != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrProtocolVersion = " + CsaSeriesMrProtocolVersion + "\n");
        	}
        	if (CsaSeriesRFSWDMostCriticalAspect != null) {
        		dialog.append("CsaSeries.RFSWDMostCriticalAspect = " + CsaSeriesRFSWDMostCriticalAspect + "\n");
        	}
        	if (CsaSeriesSequenceFileOwner != null) {
        		dialog.append("CsaSeries.SequenceFileOwner = " + CsaSeriesSequenceFileOwner + "\n");
        	}
        	if (CsaSeriesGradientMode != null) {
        		dialog.append("CsaSeries.GradientMode = " + CsaSeriesGradientMode + "\n");
        	}
        	if (CsaSeriesSliceArrayConcatenations != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.SliceArrayConcatenations = " + CsaSeriesSliceArrayConcatenations + "\n");
        	}
        	if (CsaSeriesFlowCompensation != null) {
        		dialog.append("CsaSeries.FlowCompensation = " + CsaSeriesFlowCompensation + "\n");
        	}
        	if (!Double.isNaN(CsaSeriesTransmitterCalibration)) {
        		dialog.append("CsaSeries.TransmitterCalibration = " + CsaSeriesTransmitterCalibration + "\n");
        	}
        	if (CsaSeriesIsocentered != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.Isocentered = " + CsaSeriesIsocentered + "\n");
        	}
        	if (CsaSeriesAbsTablePosition != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.AbsTablePosition = " + CsaSeriesAbsTablePosition + "\n");
        	}
        	if (!Double.isNaN(CsaSeriesReadoutOS)) {
        		dialog.append("CsaSeries.ReadoutOS = " + CsaSeriesReadoutOS + "\n");
        	}
        	if (!Double.isNaN(CsaSeriesdBdt_max)) {
        		dialog.append("CsaSeries.dBdt_max = " + CsaSeriesdBdt_max + "\n");
        	}
        	if (CsaSeriesRFSWDOperationMode != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.RFSWDOperationMode = " + CsaSeriesRFSWDOperationMode + "\n");
        	}
        	if (!Double.isNaN(CsaSeriesSelectionGradientAmplitude)) {
        		dialog.append("CsaSeries.SelectionGradientAmplitude = " + CsaSeriesSelectionGradientAmplitude + "\n");
        	}
        	if (!Double.isNaN(CsaSeriesPhaseGradientAmplitude)) {
        		dialog.append("CsaSeries.PhaseGradientAmplitude = " + CsaSeriesPhaseGradientAmplitude + "\n");
        	}
        	if (CsaSeriesRfWatchdogMask != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.RfWatchdogMask = " + CsaSeriesRfWatchdogMask + "\n");
        	}
        	if (CsaSeriesCoilForGradient2 != null) {
        		dialog.append("CsaSeries.CoilForGradient2 = " + CsaSeriesCoilForGradient2 + "\n");
        	}
        	if (CsaSeriesStim_mon_mode != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.Stim_mon_mode = " + CsaSeriesStim_mon_mode + "\n");
        	}
        	if (CsaSeriesCoilId != null) {
        		for (i = 0; i < CsaSeriesCoilId.length; i++) {
        			dialog.append("CsaSeries.CoilId["+i+"] = " + CsaSeriesCoilId[i] + "\n");
        		}
        	}
        	if (!Double.isNaN(CsaSeriesStim_max_ges_norm_online)) {
        		dialog.append("CsaSeries.Stim_max_ges_norm_online = " + CsaSeriesStim_max_ges_norm_online + "\n");
        	}
        	if (CsaSeriesCoilString != null) {
        		dialog.append("CsaSeries.CoilString = " + CsaSeriesCoilString + "\n");
        	}
        	if (CsaSeriesCoilForGradient != null) {
        		dialog.append("CsaSeries.CoilForGradient = " + CsaSeriesCoilForGradient + "\n");
        	}
        	if (CsaSeriesTablePositionOrigin != null) {
        		for (i = 0; i < CsaSeriesTablePositionOrigin.length; i++) {
        			dialog.append("CsaSeries.TablePositionOrigin["+i+"] = " + CsaSeriesTablePositionOrigin[i] + "\n");
        		}
        	}
        	if (CsaSeriesMiscSequenceParam != null) {
        		for (i = 0; i < CsaSeriesMiscSequenceParam.length; i++) {
        			dialog.append("CsaSeries.MiscSequenceParam["+i+"] = " + CsaSeriesMiscSequenceParam[i] + "\n");
        		}
        	}
        	if (CsaSeriesLongModelName != null) {
        		dialog.append("CsaSeries.LongModelName = " + CsaSeriesLongModelName + "\n");
        	}
	    	if (!Double.isNaN(CsaSeriesStim_faktor)) {
	    		dialog.append("CsaSeries.Stim_faktor = " + CsaSeriesStim_faktor + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesSW_korr_faktor)) {
	    		dialog.append("CsaSeries.SW_korr_faktor = " + CsaSeriesSW_korr_faktor + "\n");
	    	}
	    	if (CsaSeriesSed != null) {
        		for (i = 0; i < CsaSeriesSed.length; i++) {
        			dialog.append("CsaSeries.Sed["+i+"] = " + CsaSeriesSed[i] + "\n");
        		}
        	}
	    	if (CsaSeriesPositivePCSDirections != null) {
        		dialog.append("CsaSeries.PositivePCSDirections = " + CsaSeriesPositivePCSDirections + "\n");
        	}
	    	if (!Double.isNaN(CsaSeriesSliceResolution)) {
	    		dialog.append("CsaSeries.SliceResolution = " + CsaSeriesSliceResolution + "\n");
	    	}
	    	if (CsaSeriesStim_max_online != null) {
        		for (i = 0; i < CsaSeriesStim_max_online.length; i++) {
        			dialog.append("CsaSeries.Stim_max_online["+i+"] = " + CsaSeriesStim_max_online[i] + "\n");
        		}
        	}
	    	if (!Double.isNaN(CsaSeriest_puls_max)) {
	    		dialog.append("CsaSeries.t_puls_max = " + CsaSeriest_puls_max + "\n");
	    	}
	    	if (CsaSeriesMrPhoenixProtocolulVersion != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.ulVersion = " + CsaSeriesMrPhoenixProtocolulVersion + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocoltSequenceFileName != null) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.tSequenceFileName = " + CsaSeriesMrPhoenixProtocoltSequenceFileName + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocoltProtocolName != null) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.tProtocolName = " + CsaSeriesMrPhoenixProtocoltProtocolName + "\n");
	    	}
	    	if (CsaSeriesMrPhoenixProtocoltReferenceImage0 != null) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.tReferenceImage0 = " + CsaSeriesMrPhoenixProtocoltReferenceImage0 + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocoltReferenceImage1 != null) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.tReferenceImage1 = " + CsaSeriesMrPhoenixProtocoltReferenceImage1 + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocoltReferenceImage2 != null) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.tReferenceImage2 = " + CsaSeriesMrPhoenixProtocoltReferenceImage2 + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolucScanRegionPosValid != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.ucScanRegionPosValid = " + 
	    	                             CsaSeriesMrPhoenixProtocolucScanRegionPosValid + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolucTablePositioningMode != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.ucTablePositioningMode = " + 
	    	                             CsaSeriesMrPhoenixProtocolucTablePositioningMode + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsProtConsistencyInfotBaselineString != null) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sProtConsistencyInfo.tBaselineString = " + 
	    	           CsaSeriesMrPhoenixProtocolsProtConsistencyInfotBaselineString + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsProtConsistencyInfotSystemType != null) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sProtConsistencyInfo.tSystemType = " + 
	    	           CsaSeriesMrPhoenixProtocolsProtConsistencyInfotSystemType + "\n");
        	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflNominalB0)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sProtConsistencyInfo.flNominalB0 = " + 
	    	                   CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflNominalB0 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflGMax)) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sProtConsistencyInfo.flGMax = " + 
	    	                             CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflGMax + "\n");
        	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflRiseTime)) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sProtConsistencyInfo.flRiseTime = " + 
	    	                             CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflRiseTime + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsProtConsistencyInfolMaximumNofRxReceiverChannels != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sProtConsistencyInfo.lMaximumNofRxReceiverChannels = " + 
	    	                             CsaSeriesMrPhoenixProtocolsProtConsistencyInfolMaximumNofRxReceiverChannels + "\n");
        	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude0)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationX.aflAmplitude[0] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude0 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude1)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationX.aflAmplitude[1] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude1 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude2)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationX.aflAmplitude[2] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude2 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude3)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationX.aflAmplitude[3] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude3 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude4)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationX.aflAmplitude[4] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude4 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant0)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationX.aflTimeConstant[0] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant0 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant1)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationX.aflTimeConstant[1] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant1 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant2)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationX.aflTimeConstant[2] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant2 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant3)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationX.aflTimeConstant[3] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant3 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant4)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationX.aflTimeConstant[4] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant4 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude0)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationY.aflAmplitude[0] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude0 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude1)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationY.aflAmplitude[1] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude1 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude2)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationY.aflAmplitude[2] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude2 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude3)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationY.aflAmplitude[3] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude3 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude4)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationY.aflAmplitude[4] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude4 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant0)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationY.aflTimeConstant[0] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant0 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant1)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationY.aflTimeConstant[1] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant1 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant2)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationY.aflTimeConstant[2] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant2 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant3)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationY.aflTimeConstant[3] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant3 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant4)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationY.aflTimeConstant[4] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant4 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude0)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationZ.aflAmplitude[0] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude0 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude1)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationZ.aflAmplitude[1] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude1 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude2)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationZ.aflAmplitude[2] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude2 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude3)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationZ.aflAmplitude[3] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude3 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude4)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationZ.aflAmplitude[4] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude4 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant0)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationZ.aflTimeConstant[0] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant0 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant1)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationZ.aflTimeConstant[1] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant1 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant2)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationZ.aflTimeConstant[2] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant2 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant3)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationZ.aflTimeConstant[3] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant3 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant4)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sEddyCompensationZ.aflTimeConstant[4] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant4 + "\n");
	    	}
	    	if (CsaSeriesMrPhoenixProtocolsGRADSPECbEddyCompensationValid != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.bEddyCompensationValid = " + 
	    	                             CsaSeriesMrPhoenixProtocolsGRADSPECbEddyCompensationValid + "\n");
        	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude0)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationX.aflAmplitude[0] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude0 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude1)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationX.aflAmplitude[1] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude1 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude2)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationX.aflAmplitude[2] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude2 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant0)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationX.aflTimeConstant[0] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant0 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant1)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationX.aflTimeConstant[1] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant1 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant2)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationX.aflTimeConstant[2] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant2 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude0)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationY.aflAmplitude[0] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude0 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude1)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationY.aflAmplitude[1] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude1 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude2)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationY.aflAmplitude[2] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude2 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant0)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationY.aflTimeConstant[0] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant0 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant1)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationY.aflTimeConstant[1] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant1 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant2)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationY.aflTimeConstant[2] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant2 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude0)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationZ.aflAmplitude[0] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude0 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude1)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationZ.aflAmplitude[1] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude1 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude2)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationZ.aflAmplitude[2] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude2 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant0)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationZ.aflTimeConstant[0] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant0 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant1)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationZ.aflTimeConstant[1] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant1 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant2)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sB0CompensationZ.aflTimeConstant[2] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant2 + "\n");
	    	}
	    	if (CsaSeriesMrPhoenixProtocolsGRADSPECbB0CompensationValid != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.bB0CompensationValid = " + 
	    	                             CsaSeriesMrPhoenixProtocolsGRADSPECbB0CompensationValid + "\n");
        	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXYaflAmplitude0)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationXY.aflAmplitude[0] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXYaflAmplitude0 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXYaflTimeConstant0)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationXY.aflTimeConstant[0] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXYaflTimeConstant0 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXZaflAmplitude0)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationXZ.aflAmplitude[0] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXZaflAmplitude0 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXZaflTimeConstant0)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationXZ.aflTimeConstant[0] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXZaflTimeConstant0 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYXaflAmplitude0)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationYX.aflAmplitude[0] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYXaflAmplitude0 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYXaflTimeConstant0)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationYX.aflTimeConstant[0] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYXaflTimeConstant0 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYZaflAmplitude0)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationYZ.aflAmplitude[0] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYZaflAmplitude0 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYZaflTimeConstant0)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationYZ.aflTimeConstant[0] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYZaflTimeConstant0 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZXaflAmplitude0)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationZX.aflAmplitude[0] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZXaflAmplitude0 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZXaflTimeConstant0)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationZX.aflTimeConstant[0] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZXaflTimeConstant0 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZYaflAmplitude0)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationZY.aflAmplitude[0] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZYaflAmplitude0 + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZYaflTimeConstant0)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.sCrossTermCompensationZY.aflTimeConstant[0] = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZYaflTimeConstant0 + "\n");
	    	}
	    	if (CsaSeriesMrPhoenixProtocolsGRADSPECbCrossTermCompensationValid != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.bCrossTermCompensationValid = " + 
	    	                             CsaSeriesMrPhoenixProtocolsGRADSPECbCrossTermCompensationValid + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetX != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.lOffsetX = " + 
	    	                             CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetX + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetY != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.lOffsetY = " + 
	    	                             CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetY + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetZ != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.lOffsetZ = " + 
	    	                             CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetZ + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsGRADSPECbOffsetValid != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.bOffsetValid = " +  
	    	                             CsaSeriesMrPhoenixProtocolsGRADSPECbOffsetValid + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsGRADSPEClDelayX != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.lDelayX = " + 
	    	                             CsaSeriesMrPhoenixProtocolsGRADSPEClDelayX + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsGRADSPEClDelayY != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.lDelayY = " + 
	    	                             CsaSeriesMrPhoenixProtocolsGRADSPEClDelayY + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsGRADSPEClDelayZ != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.lDelayZ = " + 
	    	                             CsaSeriesMrPhoenixProtocolsGRADSPEClDelayZ + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsGRADSPECbDelayValid != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.bDelayValid = " + 
	    	                             CsaSeriesMrPhoenixProtocolsGRADSPECbDelayValid + "\n");
        	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityX)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.flSensitivityX = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityX + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityY)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.flSensitivityY = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityY + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityZ)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.flSensitivityZ = " + 
	    	                   CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityZ + "\n");
	    	}
	    	if (CsaSeriesMrPhoenixProtocolsGRADSPECbSensitivityValid != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.bSensitivityValid = " + 
	    	                             CsaSeriesMrPhoenixProtocolsGRADSPECbSensitivityValid + "\n");
        	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGRADSPECflGSWDMinRiseTime)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.flGSWDMinRiseTime = " +
	    				CsaSeriesMrPhoenixProtocolsGRADSPECflGSWDMinRiseTime + "\n");
	    	}
	    	if (CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent0 != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.alShimCurrent[0] = " + 
	    	                             CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent0 + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent1 != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.alShimCurrent[1] = " + 
	    	                             CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent1 + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent2 != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.alShimCurrent[2] = " + 
	    	                             CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent2 + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent3 != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.alShimCurrent[3] = " + 
	    	                             CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent3 + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent4 != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.alShimCurrent[4] = " + 
	    	                             CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent4 + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsGRADSPECbShimCurrentValid != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.bShimCurrentValid = " + 
	    	                             CsaSeriesMrPhoenixProtocolsGRADSPECbShimCurrentValid + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsGRADSPECucMode != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sGRADSPEC.ucMode = " + 
	    	                             CsaSeriesMrPhoenixProtocolsGRADSPECucMode + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0tNucleus != null) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[0].tNucleus = " + 
	    	           CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0tNucleus + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0lFrequency != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[0].lFrequency = " + 
	    	                             CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0lFrequency + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bFrequencyValid != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[0].bFrequencyValid = " + 
	    	                             CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bFrequencyValid + "\n");
        	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0flReferenceAmplitude)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[0].flReferenceAmplitude = " + 
	    		CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0flReferenceAmplitude + "\n");
	    	}
	    	if (CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bReferenceAmplitudeValid != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[0].bReferenceAmplitudeValid = " + 
	    	                             CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bReferenceAmplitudeValid + "\n");
        	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0flAmplitudeCorrection)) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[0].flAmplitudeCorrection = " + 
	    	                             CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0flAmplitudeCorrection + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bAmplitudeCorrectionValid != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[0].bAmplitudeCorrectionValid = " + 
	    	                             CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bAmplitudeCorrectionValid + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bRFPAIndexValid != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[0].bRFPAIndexValid = " + 
	    	                             CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bRFPAIndexValid + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bFrequencyValid != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[1].bFrequencyValid = " + 
	    	                             CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bFrequencyValid + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bReferenceAmplitudeValid != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[1].bReferenceAmplitudeValid = " + 
	    	                             CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bReferenceAmplitudeValid + "\n");
        	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1flAmplitudeCorrection)) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[1].flAmplitudeCorrection = " + 
	    	                             CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1flAmplitudeCorrection + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bAmplitudeCorrectionValid != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[1].bAmplitudeCorrectionValid = " + 
	    	                             CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bAmplitudeCorrectionValid + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1lRFPAIndex != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[1].lRFPAIndex = " + 
	    	                             CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1lRFPAIndex + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bRFPAIndexValid != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sTXSPEC.asNucleusInfo[1].bRFPAIndexValid = " + 
	    	                             CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bRFPAIndexValid + "\n");
        	}
	    	for (i = 0; i < 2; i++) {
		    	if (CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEntName != null) {
	    			if (CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEntName[i] != null) {
		        		dialog.append("CsaSeries.MrPhoenixProtocol.sTXSPEC.aRFPULSE["+i+"].tName = " + 
			    	           CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEntName[i] + "\n");
	    		    }
	        	}
		    	if (CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnbAmplitudeValid != null) {
			    	if (CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnbAmplitudeValid[i] != Integer.MIN_VALUE) {
		        		dialog.append("CsaSeries.MrPhoenixProtocol.sTXSPEC.aRFPULSE["+i+"].bAmplitudeValid = " + 
			    	                             CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnbAmplitudeValid[i] + "\n");
		        	}
		    	}
		    	if (CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnflAmplitude != null) {
			    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnflAmplitude[i])) {
			    		dialog.append("CsaSeries.MrPhoenixProtocol.sTXSPEC.aRFPULSE["+i+"].flAmplitude = " + 
			    		    CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnflAmplitude[i] + "\n");
			    	}
		    	}
	    	} // for (i = 0; i < 2; i++)
	    	if (CsaSeriesMrPhoenixProtocolsTXSPEClNoOfTraPulses != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sTXSPEC.lNoOfTraPulses = " + 
	    	                             CsaSeriesMrPhoenixProtocolsTXSPEClNoOfTraPulses + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsTXSPEClBCExcitationMode != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sTXSPEC.lBCExcitationMode = " + 
	    	                             CsaSeriesMrPhoenixProtocolsTXSPEClBCExcitationMode + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsTXSPEClBCSeqExcitationMode != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sTXSPEC.lBCSeqExcitationMode = " + 
	    	                             CsaSeriesMrPhoenixProtocolsTXSPEClBCSeqExcitationMode + "\n");
        	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeMin)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sTXSPEC.flKDynMagnitudeMin = " + 
	    		CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeMin + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeMax)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sTXSPEC.flKDynMagnitudeMax = " + 
	    		CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeMax + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeClipLow)) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sTXSPEC.flKDynMagnitudeClipLow = " + 
	    	                             CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeClipLow + "\n");
        	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeClipHigh)) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sTXSPEC.flKDynMagnitudeClipHigh = " + 
	    	                             CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeClipHigh + "\n");
        	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsTXSPECflKDynPhaseMax)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sTXSPEC.flKDynPhaseMax = " + 
	    		CsaSeriesMrPhoenixProtocolsTXSPECflKDynPhaseMax + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsTXSPECflKDynPhaseClip)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sTXSPEC.flKDynPhaseClip = " + 
	    		CsaSeriesMrPhoenixProtocolsTXSPECflKDynPhaseClip + "\n");
	    	}
	    	if (CsaSeriesMrPhoenixProtocolsTXSPECbKDynValid != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sTXSPEC.bKDynValid = " + 
	    	                             CsaSeriesMrPhoenixProtocolsTXSPECbKDynValid + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsTXSPECucRFPulseType != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sTXSPEC.ucRFPulseType = " + 
	    	                             CsaSeriesMrPhoenixProtocolsTXSPECucRFPulseType + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsTXSPECucExcitMode != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sTXSPEC.ucExcitMode = " + 
	    	                             CsaSeriesMrPhoenixProtocolsTXSPECucExcitMode + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsTXSPECucSimultaneousExcitation != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sTXSPEC.ucSimultaneousExcitation = " + 
	    	                             CsaSeriesMrPhoenixProtocolsTXSPECucSimultaneousExcitation + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsTXSPECucBCExcitationModeValid != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sTXSPEC.ucBCExcitationModeValid = " + 
	    	                             CsaSeriesMrPhoenixProtocolsTXSPECucBCExcitationModeValid + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsRXSPEClGain != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sRXSPEC.lGain = " + 
	    	                             CsaSeriesMrPhoenixProtocolsRXSPEClGain + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsRXSPECbGainValid != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sRXSPEC.bGainValid = " + 
	    	                             CsaSeriesMrPhoenixProtocolsRXSPECbGainValid + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsRXSPECalDwellTime0 != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sRXSPEC.alDwellTime[0] = " + 
	    	                             CsaSeriesMrPhoenixProtocolsRXSPECalDwellTime0 + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsAdjDatauiAdjFreMode != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sAdjData.uiAdjFreMode = " + 
	    	                             CsaSeriesMrPhoenixProtocolsAdjDatauiAdjFreMode + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsAdjDatauiAdjShimMode != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sAdjData.uiAdjShimMode = " + 
	    	                             CsaSeriesMrPhoenixProtocolsAdjDatauiAdjShimMode + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsAdjDatauiAdjWatSupMode != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sAdjData.uiAdjWatSupMode = " + 
	    	                             CsaSeriesMrPhoenixProtocolsAdjDatauiAdjWatSupMode + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsAdjDatauiAdjRFMapMode != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sAdjData.uiAdjRFMapMode = " + 
	    	                             CsaSeriesMrPhoenixProtocolsAdjDatauiAdjRFMapMode + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsAdjDatauiAdjMDSMode != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sAdjData.uiAdjMDSMode = " + 
	    	                             CsaSeriesMrPhoenixProtocolsAdjDatauiAdjMDSMode + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsAdjDatauiAdjTableTolerance != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sAdjData.uiAdjTableTolerance = " + 
	    	                             CsaSeriesMrPhoenixProtocolsAdjDatauiAdjTableTolerance + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsAdjDatauiAdjProtID != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sAdjData.uiAdjProtID = " + 
	    	                             CsaSeriesMrPhoenixProtocolsAdjDatauiAdjProtID + "\n");
        	}
	    	if (CsaSeriesMrPhoenixProtocolsAdjDatauiAdjFreProtRelated != Integer.MIN_VALUE) {
        		dialog.append("CsaSeries.MrPhoenixProtocol.sAdjData.uiAdjFreProtRelated = " + 
	    	                             CsaSeriesMrPhoenixProtocolsAdjDatauiAdjFreProtRelated + "\n");
        	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondSag)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sAdjData.sAdjVolume.sPosition.dSag = " +
	    				       CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondSag + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondCor)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sAdjData.sAdjVolume.sPosition.dCor = " +
	    				       CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondCor + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondTra)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sAdjData.sAdjVolume.sPosition.dTra = " +
	    				       CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondTra + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldSag)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sAdjData.sAdjVolume.sNormal.dSag = " +
	    				       CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldSag + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldCor)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sAdjData.sAdjVolume.sNormal.dCor = " +
	    				       CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldCor + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldTra)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sAdjData.sAdjVolume.sNormal.dTra = " +
	    				       CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldTra + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedThickness)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sAdjData.sAdjVolume.dThickness = " + 
	    				CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedThickness + "\n");
	    		
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedPhaseFOV)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sAdjData.sAdjVolume.dPhaseFOV = " + 
	    				CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedPhaseFOV + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedReadoutFOV)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sAdjData.sAdjVolume.dReadoutFOV = " + 
	    				CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedReadoutFOV + "\n");
	    	}
	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedInPlaneRot)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sAdjData.sAdjVolume.dInPlaneRot = " +
	    				       CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedInPlaneRot + "\n");
	    	}
	    	if (CsaSeriesMrPhoenixProtocolsAdjDatauiAdjVolumeValid != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sAdjData.uiAdjVolumeValid = " + 
	    				CsaSeriesMrPhoenixProtocolsAdjDatauiAdjVolumeValid + "\n");
	    	}
	    	if (CsaSeriesMrPhoenixProtocolucEnableNoiseAdjust != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.ucEnableNoiseAdjust = " + 
	    	                   CsaSeriesMrPhoenixProtocolucEnableNoiseAdjust + "\n");
	    	}
	    	if (CsaSeriesMrPhoenixProtocolalTR0 != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.alTR[0] = " + CsaSeriesMrPhoenixProtocolalTR0 + "\n");
	    	}
	    	if (CsaSeriesMrPhoenixProtocollContrasts != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.lContrasts = " + CsaSeriesMrPhoenixProtocollContrasts + "\n");
	    	}
	    	if (CsaSeriesMrPhoenixProtocolalTE0 != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.alTE[0] = " + CsaSeriesMrPhoenixProtocolalTE0 + "\n");
	    	}
	    	if (CsaSeriesMrPhoenixProtocolacFlowComp0 != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.acFlowComp[0] = " + CsaSeriesMrPhoenixProtocolacFlowComp0 + "\n");
	    	}
	    	if (CsaSeriesMrPhoenixProtocollCombinedEchoes != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.lCombinedEchoes = " + CsaSeriesMrPhoenixProtocollCombinedEchoes + "\n");
	    	}
	    	
	    	
	    	for (i = 0; i < getExtents()[2]; i++) {
	    	    if (CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondSag != null) {
	    	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondSag[i])) {
	    	    		dialog.append("CsaSeries.MrPhoenixProtocol.sSliceArray.asSlice["+i+"].sPosition.dSag = " +
	    	    				CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondSag[i] + "\n");
	    	    	}
	    	    } // if (CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondSag != null)
	    	    
	    	    if (CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondCor != null) {
	    	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondCor[i])) {
	    	    		dialog.append("CsaSeries.MrPhoenixProtocol.sSliceArray.asSlice["+i+"].sPosition.dCor = " +
	    	    				CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondCor[i] + "\n");
	    	    	}
	    	    } // if (CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondCor != null)
	    	    
	    	    if (CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondTra != null) {
	    	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondTra[i])) {
	    	    		dialog.append("CsaSeries.MrPhoenixProtocol.sSliceArray.asSlice["+i+"].sPosition.dTra = " +
	    	    				CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondTra[i] + "\n");
	    	    	}
	    	    } // if (CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondTra != null)
	    	    
	    	    if (CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldSag != null) {
	    	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldSag[i])) {
	    	    		dialog.append("CsaSeries.MrPhoenixProtocol.sSliceArray.asSlice["+i+"].sNormal.dSag = " +
	    	    				CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldSag[i] + "\n");
	    	    	}
	    	    } // if (CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldSag != null)
	    	    
	    	    if (CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldCor != null) {
	    	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldCor[i])) {
	    	    		dialog.append("CsaSeries.MrPhoenixProtocol.sSliceArray.asSlice["+i+"].sNormal.dCor = " +
	    	    				CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldCor[i] + "\n");
	    	    	}
	    	    } // if (CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldCor != null)
	    	    
	    	    if (CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldTra != null) {
	    	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldTra[i])) {
	    	    		dialog.append("CsaSeries.MrPhoenixProtocol.sSliceArray.asSlice["+i+"].sNormal.dTra = " +
	    	    				CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldTra[i] + "\n");
	    	    	}
	    	    } // if (CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldTra != null)
	    	    
	    	    if (CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdThickness != null) {
	    	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdThickness[i])) {
	    	    		dialog.append("CsaSeries.MrPhoenixProtocol.sSliceArray.asSlice["+i+"].dThickness = " +
	    	    				CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdThickness[i] + "\n");
	    	    	}
	    	    } // if (CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdThickness != null)
	    	    
	    	    if (CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdPhaseFOV != null) {
	    	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdPhaseFOV[i])) {
	    	    		dialog.append("CsaSeries.MrPhoenixProtocol.sSliceArray.asSlice["+i+"].dPhaseFOV = " +
	    	    				CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdPhaseFOV[i] + "\n");
	    	    	}
	    	    } // if (CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdPhaseFOV != null)
	    	    
	    	    if (CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdReadoutFOV != null) {
	    	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdReadoutFOV[i])) {
	    	    		dialog.append("CsaSeries.MrPhoenixProtocol.sSliceArray.asSlice["+i+"].dReadoutFOV = " +
	    	    				CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdReadoutFOV[i] + "\n");
	    	    	}
	    	    } // if (CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdReadoutFOV != null)
	    	    
	    	    if (CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdInPlaneRot != null) {
	    	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdInPlaneRot[i])) {
	    	    		dialog.append("CsaSeries.MrPhoenixProtocol.sSliceArray.asSlice["+i+"].dInPlaneRot = " +
	    	    				CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdInPlaneRot[i] + "\n");
	    	    	}
	    	    } // if (CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdInPlaneRot != null)
	    	    
	    	} // for (i = 0; i < getExtents()[2]; i++)
	        if (CsaSeriesMrPhoenixProtocolsSliceArrayanAsc != null) {
	        	for (i = 0; i < getExtents()[2]-1; i++) {
	        		if (CsaSeriesMrPhoenixProtocolsSliceArrayanAsc[i] != Integer.MIN_VALUE) {
	        			dialog.append("CsaSeries.MrPhoenixProtocol.sSliceArray.anAsc["+(i+1)+"] = " +
	        					CsaSeriesMrPhoenixProtocolsSliceArrayanAsc[i] + "\n");
	        		}
	        	}
	    	}
	        if (CsaSeriesMrPhoenixProtocolsSliceArrayanPos != null) {
	        	for (i = 0; i < getExtents()[2]-1; i++) {
	        		if (CsaSeriesMrPhoenixProtocolsSliceArrayanPos[i] != Integer.MIN_VALUE) {
	        			dialog.append("CsaSeries.MrPhoenixProtocol.sSliceArray.anPos["+(i+1)+"] = " +
	        					CsaSeriesMrPhoenixProtocolsSliceArrayanPos[i] + "\n");
	        		}
	        	}
	    	}
	        if (CsaSeriesMrPhoenixProtocolsSliceArraylSize != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sSliceArray.lSize = " + CsaSeriesMrPhoenixProtocolsSliceArraylSize + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsSliceArraylConc != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sSliceArray.lConc = " + CsaSeriesMrPhoenixProtocolsSliceArraylConc + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsSliceArrayucMode != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sSliceArray.ucMode = " + CsaSeriesMrPhoenixProtocolsSliceArrayucMode + "\n");
	    	}
	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsSliceArraysTSatdThickness)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sSliceArray.sTSat.dThickness = " + 
	                                CsaSeriesMrPhoenixProtocolsSliceArraysTSatdThickness + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsGroupArrayasGroup0nSize != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGroupArray.asGroup[0].nSize = " + 
	                                CsaSeriesMrPhoenixProtocolsGroupArrayasGroup0nSize + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsGroupArrayanMember != null) {
	        	for (i = 0; i < getExtents()[2]; i++) {
	        		if (CsaSeriesMrPhoenixProtocolsGroupArrayanMember[i] != Integer.MIN_VALUE) {
	        			dialog.append("CsaSeries.MrPhoenixProtocol.sGroupArray.anMember["+(i+1)+"] = " +
	        					CsaSeriesMrPhoenixProtocolsGroupArrayanMember[i] + "\n");
	        		}
	        	}
	    	}
	        if (CsaSeriesMrPhoenixProtocolsGroupArraylSize != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGroupArray.lSize = " + 
	                                CsaSeriesMrPhoenixProtocolsGroupArraylSize + "\n");
	    	}
	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGroupArraysPSatdThickness)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGroupArray.sPSat.dThickness = " + 
	                                CsaSeriesMrPhoenixProtocolsGroupArraysPSatdThickness + "\n");
	    	}
	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsGroupArraysPSatdGap)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sGroupArray.sPSat.dGap = " + 
	                                CsaSeriesMrPhoenixProtocolsGroupArraysPSatdGap + "\n");
	    	}
	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondSag)) {
	            dialog.append("CsaSeries.MrPhoenixProtocol.sRSatArray.asElm[0].sPosition.dSag = " +
	            		CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondSag + "\n");	
	        }
	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondCor)) {
	            dialog.append("CsaSeries.MrPhoenixProtocol.sRSatArray.asElm[0].sPosition.dCor = " +
	            		CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondCor + "\n");	
	        }
	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondTra)) {
	            dialog.append("CsaSeries.MrPhoenixProtocol.sRSatArray.asElm[0].sPosition.dTra = " +
	            		CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondTra + "\n");	
	        }
	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldSag)) {
	            dialog.append("CsaSeries.MrPhoenixProtocol.sRSatArray.asElm[0].sNormal.dSag = " +
	            		CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldSag + "\n");	
	        }
	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldCor)) {
	            dialog.append("CsaSeries.MrPhoenixProtocol.sRSatArray.asElm[0].sNormal.dCor = " +
	            		CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldCor + "\n");	
	        }
	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldTra)) {
	            dialog.append("CsaSeries.MrPhoenixProtocol.sRSatArray.asElm[0].sNormal.dTra = " +
	            		CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldTra + "\n");	
	        }
	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsRSatArrayasElm0dThickness)) {
	            dialog.append("CsaSeries.MrPhoenixProtocol.sRSatArray.asElm[0].dThickness = " +
	            		CsaSeriesMrPhoenixProtocolsRSatArrayasElm0dThickness + "\n");	
	        }
	        if (CsaSeriesMrPhoenixProtocolsRSatArraylSize != Integer.MIN_VALUE) {
	            dialog.append("CsaSeries.MrPhoenixProtocol.sRSatArray.lSize = " +
	            		CsaSeriesMrPhoenixProtocolsRSatArraylSize + "\n");	
	        }
	        if (CsaSeriesMrPhoenixProtocolsAutoAligndAAMatrix != null) {
	        	for (i = 0; i < CsaSeriesMrPhoenixProtocolsAutoAligndAAMatrix.length; i++) {
	    	    	if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsAutoAligndAAMatrix[i])) {
	    	    		dialog.append("CsaSeries.MrPhoenixProtocol.sAutoAlign.dAAMatrix["+i+"] = "  +
	    	    		    CsaSeriesMrPhoenixProtocolsAutoAligndAAMatrix[i] + "\n");
	    	    	}
	        	}
    	    } // if (CsaSeriesMrPhoenixProtocolsAutoAligndAAMatrix != null)
	        if (CsaSeriesMrPhoenixProtocolsNavigatorParalBreathHoldMeas != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sNavigatorPara.lBreathHoldMeas = " + 
	                                CsaSeriesMrPhoenixProtocolsNavigatorParalBreathHoldMeas + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsNavigatorParalRespComp != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sNavigatorPara.lRespComp = " + 
	                                CsaSeriesMrPhoenixProtocolsNavigatorParalRespComp + "\n");
	    	}
	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsBladeParadBladeCoverage)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sBladePara.dBladeCoverage = " + 
	                                CsaSeriesMrPhoenixProtocolsBladeParadBladeCoverage + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsBladeParaucMotionCorr != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sBladePara.ucMotionCorr = " + 
	                                CsaSeriesMrPhoenixProtocolsBladeParaucMotionCorr + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPrepPulsesucFatSat != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPrepPulses.ucFatSat = " + 
	                                CsaSeriesMrPhoenixProtocolsPrepPulsesucFatSat + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPrepPulsesucWaterSat != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPrepPulses.ucWaterSat = " + 
	                                CsaSeriesMrPhoenixProtocolsPrepPulsesucWaterSat + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPrepPulsesucInversion != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPrepPulses.ucInversion = " + 
	                                CsaSeriesMrPhoenixProtocolsPrepPulsesucInversion + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPrepPulsesucSatRecovery != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPrepPulses.ucSatRecovery = " + 
	                                CsaSeriesMrPhoenixProtocolsPrepPulsesucSatRecovery + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPrepPulsesucT2Prep != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPrepPulses.ucT2Prep = " + 
	                                CsaSeriesMrPhoenixProtocolsPrepPulsesucT2Prep + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPrepPulsesucTIScout != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPrepPulses.ucTIScout = " + 
	                                CsaSeriesMrPhoenixProtocolsPrepPulsesucTIScout + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPrepPulsesucFatSatMode != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPrepPulses.ucFatSatMode = " + 
	                                CsaSeriesMrPhoenixProtocolsPrepPulsesucFatSatMode + "\n");
	    	}
	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsPrepPulsesdDarkBloodThickness)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPrepPulses.dDarkBloodThickness = " + 
	                                CsaSeriesMrPhoenixProtocolsPrepPulsesdDarkBloodThickness + "\n");
	    	}
	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsPrepPulsesdDarkBloodFlipAngle)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPrepPulses.dDarkBloodFlipAngle = " + 
	                                CsaSeriesMrPhoenixProtocolsPrepPulsesdDarkBloodFlipAngle + "\n");
	    	}
	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsPrepPulsesdT2PrepDuration)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPrepPulses.dT2PrepDuration = " + 
	                                CsaSeriesMrPhoenixProtocolsPrepPulsesdT2PrepDuration + "\n");
	    	}
	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsPrepPulsesdIRPulseThicknessFactor)) {
	        	dialog.append("CsaSeries.MrPhoenixProtocol.sPrepPulses.dIRPulseThicknessFactor = " + 
	        			CsaSeriesMrPhoenixProtocolsPrepPulsesdIRPulseThicknessFactor + "\n");
	        }
	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsKSpacedPhaseResolution)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sKSpace.dPhaseResolution = " + 
	                                CsaSeriesMrPhoenixProtocolsKSpacedPhaseResolution + "\n");
	    	}
	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsKSpacedSliceResolution)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sKSpace.dSliceResolution = " + 
	                                CsaSeriesMrPhoenixProtocolsKSpacedSliceResolution + "\n");
	    	}
	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsKSpacedAngioDynCentralRegionA)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sKSpace.dAngioDynCentralRegionA = " + 
	                                CsaSeriesMrPhoenixProtocolsKSpacedAngioDynCentralRegionA + "\n");
	    	}
	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsKSpacedAngioDynSamplingDensityB)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sKSpace.dAngioDynSamplingDensityB = " + 
	                                CsaSeriesMrPhoenixProtocolsKSpacedAngioDynSamplingDensityB + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsKSpacelBaseResolution != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sKSpace.lBaseResolution = " + 
	                                CsaSeriesMrPhoenixProtocolsKSpacelBaseResolution + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsKSpacelPhaseEncodingLines != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sKSpace.lPhaseEncodingLines = " + 
	                                CsaSeriesMrPhoenixProtocolsKSpacelPhaseEncodingLines + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsKSpacelPartitions != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sKSpace.lPartitions = " + 
	                                CsaSeriesMrPhoenixProtocolsKSpacelPartitions + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsKSpacelImagesPerSlab != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sKSpace.lImagesPerSlab = " + 
	                                CsaSeriesMrPhoenixProtocolsKSpacelImagesPerSlab + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsKSpacelRadialViews != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sKSpace.lRadialViews = " + 
	                                CsaSeriesMrPhoenixProtocolsKSpacelRadialViews + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsKSpacelRadialInterleavesPerImage != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sKSpace.lRadialInterleavesPerImage = " + 
	                                CsaSeriesMrPhoenixProtocolsKSpacelRadialInterleavesPerImage + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsKSpacelLinesPerShot != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sKSpace.lLinesPerShot = " + 
	                                CsaSeriesMrPhoenixProtocolsKSpacelLinesPerShot + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsKSpaceunReordering != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sKSpace.unReordering = " + 
	                                CsaSeriesMrPhoenixProtocolsKSpaceunReordering + "\n");
	    	}
	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsKSpacedSeqPhasePartialFourierForSNR)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sKSpace.dSeqPhasePartialFourierForSNR = " + 
	                                CsaSeriesMrPhoenixProtocolsKSpacedSeqPhasePartialFourierForSNR + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsKSpaceucPhasePartialFourier != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sKSpace.ucPhasePartialFourier = " + 
	                                CsaSeriesMrPhoenixProtocolsKSpaceucPhasePartialFourier + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsKSpaceucSlicePartialFourier != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sKSpace.ucSlicePartialFourier = " + 
	                                CsaSeriesMrPhoenixProtocolsKSpaceucSlicePartialFourier + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsKSpaceucAveragingMode != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sKSpace.ucAveragingMode = " + 
	                                CsaSeriesMrPhoenixProtocolsKSpaceucAveragingMode + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsKSpaceucMultiSliceMode != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sKSpace.ucMultiSliceMode = " + 
	                                CsaSeriesMrPhoenixProtocolsKSpaceucMultiSliceMode + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsKSpaceucDimension != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sKSpace.ucDimension = " + 
	                                CsaSeriesMrPhoenixProtocolsKSpaceucDimension + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsKSpaceucTrajectory != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sKSpace.ucTrajectory = " + 
	                                CsaSeriesMrPhoenixProtocolsKSpaceucTrajectory + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsKSpaceucViewSharing != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sKSpace.ucViewSharing = " + 
	                                CsaSeriesMrPhoenixProtocolsKSpaceucViewSharing + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsKSpaceucAsymmetricEchoMode != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sKSpace.ucAsymmetricEchoMode = " + 
	                                CsaSeriesMrPhoenixProtocolsKSpaceucAsymmetricEchoMode + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsKSpaceucPOCS != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sKSpace.ucPOCS = " + 
	                                CsaSeriesMrPhoenixProtocolsKSpaceucPOCS + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsFastImaginglEPIFactor != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sFastImaging.lEPIFactor = " + 
	                                CsaSeriesMrPhoenixProtocolsFastImaginglEPIFactor + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsFastImaginglTurboFactor != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sFastImaging.lTurboFactor = " + 
	                                CsaSeriesMrPhoenixProtocolsFastImaginglTurboFactor + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsFastImaginglSliceTurboFactor != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sFastImaging.lSliceTurboFactor = " + 
	                                CsaSeriesMrPhoenixProtocolsFastImaginglSliceTurboFactor + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsFastImaginglSegments != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sFastImaging.lSegments = " + 
	                                CsaSeriesMrPhoenixProtocolsFastImaginglSegments + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsFastImagingucSegmentationMode != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sFastImaging.ucSegmentationMode = " + 
	                                CsaSeriesMrPhoenixProtocolsFastImagingucSegmentationMode + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsFastImaginglShots != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sFastImaging.lShots = " + 
	                                CsaSeriesMrPhoenixProtocolsFastImaginglShots + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsFastImaginglEchoTrainDuration != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sFastImaging.lEchoTrainDuration = " + 
	                                CsaSeriesMrPhoenixProtocolsFastImaginglEchoTrainDuration + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPhysioImaginglSignal1 != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPhysioImaging.lSignal1 = " + 
	                                CsaSeriesMrPhoenixProtocolsPhysioImaginglSignal1 + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPhysioImaginglMethod1 != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPhysioImaging.lMethod1 = " + 
	                                CsaSeriesMrPhoenixProtocolsPhysioImaginglMethod1 + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPhysioImaginglSignal2 != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPhysioImaging.lSignal2 = " + 
	                                CsaSeriesMrPhoenixProtocolsPhysioImaginglSignal2 + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPhysioImaginglMethod2 != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPhysioImaging.lMethod2 = " + 
	                                CsaSeriesMrPhoenixProtocolsPhysioImaginglMethod2 + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPhysioImaginglPhases != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPhysioImaging.lPhases = " + 
	                                CsaSeriesMrPhoenixProtocolsPhysioImaginglPhases + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPhysioImaginglRetroGatedImages != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPhysioImaging.lRetroGatedImages = " + 
	                                CsaSeriesMrPhoenixProtocolsPhysioImaginglRetroGatedImages + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerPulses != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioECG.lTriggerPulses = " + 
	                                CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerPulses + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerWindow != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioECG.lTriggerWindow = " + 
	                                CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerWindow + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlArrhythmiaDetection != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioECG.lArrhythmiaDetection = " + 
	                                CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlArrhythmiaDetection + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlCardiacGateOnThreshold != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioECG.lCardiacGateOnThreshold = " + 
	                                CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlCardiacGateOnThreshold + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlCardiacGateOffThreshold != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioECG.lCardiacGateOffThreshold = " + 
	                                CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlCardiacGateOffThreshold + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerIntervals != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioECG.lTriggerIntervals = " + 
	                                CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerIntervals + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerPulses != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioPulse.lTriggerPulses = " + 
	                                CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerPulses + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerWindow != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioPulse.lTriggerWindow = " + 
	                                CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerWindow + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselArrhythmiaDetection != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioPulse.lArrhythmiaDetection = " + 
	                                CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselArrhythmiaDetection + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselCardiacGateOnThreshold != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioPulse.lCardiacGateOnThreshold = " + 
	                                CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselCardiacGateOnThreshold + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselCardiacGateOffThreshold != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioPulse.lCardiacGateOffThreshold = " + 
	                                CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselCardiacGateOffThreshold + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerIntervals != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioPulse.lTriggerIntervals = " + 
	                                CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerIntervals + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerPulses != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioExt.lTriggerPulses = " + 
	                                CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerPulses + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerWindow != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioExt.lTriggerWindow = " + 
	                                CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerWindow + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlArrhythmiaDetection != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioExt.lArrhythmiaDetection = " + 
	                                CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlArrhythmiaDetection + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlCardiacGateOnThreshold != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioExt.lCardiacGateOnThreshold = " + 
	                                CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlCardiacGateOnThreshold + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlCardiacGateOffThreshold != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioExt.lCardiacGateOffThreshold = " + 
	                                CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlCardiacGateOffThreshold + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerIntervals != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioExt.lTriggerIntervals = " + 
	                                CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerIntervals + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioResplRespGateThreshold != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioResp.lRespGateThreshold = " + 
	                                CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioResplRespGateThreshold + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioResplRespGatePhase != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioResp.lRespGatePhase = " + 
	                                CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioResplRespGatePhase + "\n");
	    	}
	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioRespdGatingRatio)) {
	        	dialog.append("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioResp.dGatingRatio = " + 
	        			CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioRespdGatingRatio + "\n");
	        }
	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioNativeucMode != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioNative.ucMode = " + 
	                                CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioNativeucMode + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioNativeucFlowSenMode != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPhysioImaging.sPhysioNative.ucFlowSenMode = " + 
	                                CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioNativeucFlowSenMode + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsSpecParalPhaseCyclingType != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sSpecPara.lPhaseCyclingType = " + 
	                                CsaSeriesMrPhoenixProtocolsSpecParalPhaseCyclingType + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsSpecParalPhaseEncodingType != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sSpecPara.lPhaseEncodingType = " + 
	                                CsaSeriesMrPhoenixProtocolsSpecParalPhaseEncodingType + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsSpecParalRFExcitationBandwidth != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sSpecPara.lRFExcitationBandwidth = " + 
	                                CsaSeriesMrPhoenixProtocolsSpecParalRFExcitationBandwidth + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsSpecParaucRemoveOversampling != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sSpecPara.ucRemoveOversampling = " + 
	                                CsaSeriesMrPhoenixProtocolsSpecParaucRemoveOversampling + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsSpecParalAutoRefScanNo != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sSpecPara.lAutoRefScanNo = " + 
	                                CsaSeriesMrPhoenixProtocolsSpecParalAutoRefScanNo + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsSpecParalDecouplingType != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sSpecPara.lDecouplingType = " + 
	                                CsaSeriesMrPhoenixProtocolsSpecParalDecouplingType + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsSpecParalNOEType != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sSpecPara.lNOEType = " + 
	                                CsaSeriesMrPhoenixProtocolsSpecParalNOEType + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsSpecParalExcitationType != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sSpecPara.lExcitationType = " + 
	                                CsaSeriesMrPhoenixProtocolsSpecParalExcitationType + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsSpecParalSpecAppl != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sSpecPara.lSpecAppl = " + 
	                                CsaSeriesMrPhoenixProtocolsSpecParalSpecAppl + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsSpecParalSpectralSuppression != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sSpecPara.lSpectralSuppression = " + 
	                                CsaSeriesMrPhoenixProtocolsSpecParalSpectralSuppression + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsDiffusionulMode != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sDiffusion.ulMode = " + 
	                                CsaSeriesMrPhoenixProtocolsDiffusionulMode + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsAngioucPCFlowMode != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sAngio.ucPCFlowMode = " + 
	                                CsaSeriesMrPhoenixProtocolsAngioucPCFlowMode + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsAngioucTOFInflow != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sAngio.ucTOFInflow = " + 
	                                CsaSeriesMrPhoenixProtocolsAngioucTOFInflow + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsAngiolDynamicReconMode != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sAngio.lDynamicReconMode = " + 
	                                CsaSeriesMrPhoenixProtocolsAngiolDynamicReconMode + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsAngiolTemporalInterpolation != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sAngio.lTemporalInterpolation = " + 
	                                CsaSeriesMrPhoenixProtocolsAngiolTemporalInterpolation + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsRawFilterlSlope_256 != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sRawFilter.lSlope_256 = " + 
	                                CsaSeriesMrPhoenixProtocolsRawFilterlSlope_256  + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsRawFilterucOn != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sRawFilter.ucOn = " + 
	                                CsaSeriesMrPhoenixProtocolsRawFilterucOn  + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsRawFilterucMode != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sRawFilter.ucMode = " + 
	                                CsaSeriesMrPhoenixProtocolsRawFilterucMode  + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsDistortionCorrFilterucMode != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sDistortionCorrFilter.ucMode  = " + 
	                                CsaSeriesMrPhoenixProtocolsDistortionCorrFilterucMode  + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPatlAccelFactPE != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPat.lAccelFactPE = " + 
	                                CsaSeriesMrPhoenixProtocolsPatlAccelFactPE  + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPatlAccelFact3D != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPat.lAccelFact3D = " + 
	                                CsaSeriesMrPhoenixProtocolsPatlAccelFact3D  + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPatlRefLinesPE != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPat.lRefLinesPE = " + 
	                                CsaSeriesMrPhoenixProtocolsPatlRefLinesPE  + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPatucPATMode != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPat.ucPATMode = " + 
	                                CsaSeriesMrPhoenixProtocolsPatucPATMode  + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPatucRefScanMode != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPat.ucRefScanMode = " + 
	                                CsaSeriesMrPhoenixProtocolsPatucRefScanMode  + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsPatucTPatAverageAllFrames != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sPat.ucTPatAverageAllFrames = " + 
	                                CsaSeriesMrPhoenixProtocolsPatucTPatAverageAllFrames  + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsMDSulMdsModeMask != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sMDS.ulMdsModeMask = " + 
	                                CsaSeriesMrPhoenixProtocolsMDSulMdsModeMask  + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsMDSulMdsVariableResolution != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sMDS.ulMdsVariableResolution = " + 
	                                CsaSeriesMrPhoenixProtocolsMDSulMdsVariableResolution  + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsMDSlTableSpeedNumerator != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sMDS.lTableSpeedNumerator = " + 
	                                CsaSeriesMrPhoenixProtocolsMDSlTableSpeedNumerator  + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsMDSlmdsLinesPerSegment != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sMDS.lmdsLinesPerSegment = " + 
	                                CsaSeriesMrPhoenixProtocolsMDSlmdsLinesPerSegment  + "\n");
	    	}
	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsMDSsMdsEndPosSBCS_mmdTra)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sMDS.sMdsEndPosSBCS_mmd.Tra = " + 
	                                CsaSeriesMrPhoenixProtocolsMDSsMdsEndPosSBCS_mmdTra  + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolsMDSulMdsReconMode != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sMDS.ulMdsReconMode = " + 
	                                CsaSeriesMrPhoenixProtocolsMDSulMdsReconMode  + "\n");
	    	}
	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsMDSdMdsRangeExtension)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.sMDS.dMdsRangeExtension = " + 
	                                CsaSeriesMrPhoenixProtocolsMDSdMdsRangeExtension  + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolucEnableIntro != Integer.MIN_VALUE) {
	        	dialog.append("CsaSeries.MrPhoenixProtocol.ucEnableIntro = " + CsaSeriesMrPhoenixProtocolucEnableIntro + "\n");
	        }
	        if (CsaSeriesMrPhoenixProtocolucDisableChangeStoreImages != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.ucDisableChangeStoreImages = " + 
	                                CsaSeriesMrPhoenixProtocolucDisableChangeStoreImages  + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolucAAMode != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.ucAAMode = " + 
	                                CsaSeriesMrPhoenixProtocolucAAMode  + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolucAARegionMode != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.ucAARegionMode = " + 
	                                CsaSeriesMrPhoenixProtocolucAARegionMode  + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolucAARefMode != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.ucAARefMode = " + 
	                                CsaSeriesMrPhoenixProtocolucAARefMode  + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolucReconstructionMode != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.ucReconstructionMode = " + 
	                                CsaSeriesMrPhoenixProtocolucReconstructionMode  + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolucOneSeriesForAllMeas != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.ucOneSeriesForAllMeas = " + 
	                                CsaSeriesMrPhoenixProtocolucOneSeriesForAllMeas  + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolucPHAPSMode != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.ucPHAPSMode = " + 
	                                CsaSeriesMrPhoenixProtocolucPHAPSMode  + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolucDixon != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.ucDixon = " + 
	                                CsaSeriesMrPhoenixProtocolucDixon  + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolucDixonSaveOriginal != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.ucDixonSaveOriginal = " + 
	                                CsaSeriesMrPhoenixProtocolucDixonSaveOriginal  + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolucWaitForPrepareCompletion != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.ucWaitForPrepareCompletion = " + 
	                                CsaSeriesMrPhoenixProtocolucWaitForPrepareCompletion  + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocollAverages != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.lAverages = " + 
	                                CsaSeriesMrPhoenixProtocollAverages  + "\n");
	    	}
	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocoldAveragesDouble)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.dAveragesDouble = " + 
	                                CsaSeriesMrPhoenixProtocoldAveragesDouble  + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocollRepetitions != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.lRepetitions = " + 
	                                CsaSeriesMrPhoenixProtocollRepetitions  + "\n");
	    	}
	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocoladFlipAngleDegree0)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.adFlipAngleDegree[0] = " + 
	                                CsaSeriesMrPhoenixProtocoladFlipAngleDegree0  + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocollScanTimeSec != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.lScanTimeSec = " + 
	                                CsaSeriesMrPhoenixProtocollScanTimeSec  + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocollTotalScanTimeSec != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.lTotalScanTimeSec = " + 
	                                CsaSeriesMrPhoenixProtocollTotalScanTimeSec  + "\n");
	    	}
	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocoldRefSNR)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.dRefSNR = " + 
	                                CsaSeriesMrPhoenixProtocoldRefSNR  + "\n");
	    	}
	        if (!Double.isNaN(CsaSeriesMrPhoenixProtocoldRefSNR_VOI)) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.dRefSNR_VOI = " + 
	                                CsaSeriesMrPhoenixProtocoldRefSNR_VOI  + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocoltdefaultEVAProt != null) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.tdefaultEVAProt = " + 
	                                CsaSeriesMrPhoenixProtocoltdefaultEVAProt  + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0tNucleus != null) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].tNucleus = " + 
	    				CsaSeriesMrPhoenixProtocolasCoilSelectMeas0tNucleus  + "\n");
	    	}
	        if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0iUsedRFactor != Integer.MIN_VALUE) {
	    		dialog.append("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].iUsedRFactor = " + 
	    				CsaSeriesMrPhoenixProtocolasCoilSelectMeas0iUsedRFactor  + "\n");
	    	}
	        for (i = 0; i < 24; i++) {
		        if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtCoilID != null) {
		        		if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtCoilID[i] != null) {
		        			dialog.append("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].asList["+i+"].sCoilElementID.tCoilID = " +
		        					CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtCoilID[i] + "\n");
		        		}
		        }
		        if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDlCoilCopy != null) {
		        	if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDlCoilCopy[i] != Integer.MIN_VALUE) {
		        		dialog.append("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].asList["+i+"].sCoilElementID.lCoilCopy = " +
		        				CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDlCoilCopy[i] + "\n");
		        	}
		        }
		        if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtElement != null) {
	        		if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtElement[i] != null) {
	        			dialog.append("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].asList["+i+"].sCoilElementID.tElement = " +
	        					CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtElement[i] + "\n");
	        		}
	            }
		        if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlElementSelected != null) {
		        	if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlElementSelected[i] != Integer.MIN_VALUE) {
		        		dialog.append("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].asList["+i+"].lElementSelected = " +
		        				CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlElementSelected[i] + "\n");
		        	}
		        }
		        if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlRxChannelConnected != null) {
		        	if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlRxChannelConnected[i] != Integer.MIN_VALUE) {
		        		dialog.append("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].asList["+i+"].lRxChannelConnected = " +
		        				CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlRxChannelConnected[i] + "\n");
		        	}
		        }
	        } // for (i = 0; i < 24; i++)
	        
	        if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSaulPlugId != null) {
	        	for (i = 0; i < CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSaulPlugId.length; i++) {
	        		if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSaulPlugId[i] != Integer.MIN_VALUE) {
	        			dialog.append("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].sCOILPLUGS.aulPlugId["+i+"] = " +
	        					CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSaulPlugId[i] + "\n");
	        		}
	        	}
	        }
	        
	        if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSauiNmbrOfNibbles != null) {
	        	for (i = 0; i < CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSauiNmbrOfNibbles.length; i++) {
	        		if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSauiNmbrOfNibbles[i] != Integer.MIN_VALUE) {
	        			dialog.append("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].sCOILPLUGS.auiNmbrOfNibbles["+i+"] = " +
	        					CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSauiNmbrOfNibbles[i] + "\n");
	        		}
	        	}
	        }
	        
	        for (i = 0; i < 24; i++) {
	        	if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnflFactor != null) {
	        		if (!Double.isNaN(CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnflFactor[i])) {
	        			dialog.append("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].aFFT_SCALE["+i+"].flFactor = " + 
	        					CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnflFactor[i] + "\n");
	        		}
	        	}
	        	if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnbValid != null) {
	        		if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnbValid[i] != Integer.MIN_VALUE) {
	        			dialog.append("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].aFFT_SCALE["+i+"].bValid = " +
	        					CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnbValid[i] + "\n");
	        		}
	        	}
	        	if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnlRxChannel != null) {
	        		if (CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnlRxChannel[i] != Integer.MIN_VALUE) {
	        			dialog.append("CsaSeries.MrPhoenixProtocol.asCoilSelectMeas[0].aFFT_SCALE["+i+"].lRxChannel = " +
	        					CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnlRxChannel[i] + "\n");
	        		}
	        	}
	        } // for (i = 0; i < 24; i++)
	        if (CsaSeriesMrPhoenixProtocolsEFISPECbEFIDataValid != Integer.MIN_VALUE) {
	        	dialog.append("CsaSeries.MrPhoenixProtocol.sEFISPEC.bEFIDataValid = " +
	        			CsaSeriesMrPhoenixProtocolsEFISPECbEFIDataValid + "\n");
	        }
	        if (CsaSeriesMrPhoenixProtocolsWiPMemBlockalFree != null) {
	        	for (i = 0; i < CsaSeriesMrPhoenixProtocolsWiPMemBlockalFree.length; i++) {
	        		if (CsaSeriesMrPhoenixProtocolsWiPMemBlockalFree[i] != Integer.MIN_VALUE) {
	        			dialog.append("CsaSeries.MrPhoenixProtocol.sWiPMemBlock.alFree["+i+"] = " +
	        			CsaSeriesMrPhoenixProtocolsWiPMemBlockalFree[i] + "\n");
	        		}
	        	}
	        }
	        if (CsaSeriesMrPhoenixProtocolsWiPMemBlockadFree != null) {
	        	for (i = 0; i < CsaSeriesMrPhoenixProtocolsWiPMemBlockadFree.length; i++) {
	        		if (!Double.isNaN(CsaSeriesMrPhoenixProtocolsWiPMemBlockadFree[i])) {
	        			dialog.append("CsaSeries.MrPhoenixProtocol.sWiPMemBlock.adFree["+i+"] = " +
	        			CsaSeriesMrPhoenixProtocolsWiPMemBlockadFree[i] + "\n");
	        		}
	        	}
	        }
	        if (CsaSeriesMrPhoenixProtocolsWiPMemBlocktFree != null) {
	        	dialog.append("CsaSeries.MrPhoenixProtocol.sWiPMemBlock.tFree = " + 
	                                  CsaSeriesMrPhoenixProtocolsWiPMemBlocktFree + "\n");
	        }
	        if (CsaSeriesMrPhoenixProtocolucBOLDParadigmArray != null) {
	        	for (i = 0; i < CsaSeriesMrPhoenixProtocolucBOLDParadigmArray.length; i++) {
	        		if (CsaSeriesMrPhoenixProtocolucBOLDParadigmArray[i] != Integer.MIN_VALUE) {
		        		dialog.append("CsaSeries.MrPhoenixProtocol.ucBOLDParadigmArray["+i+"] = " +
		        				CsaSeriesMrPhoenixProtocolucBOLDParadigmArray[i] + "\n");
	        		}
	        	}
	        }
	        if (CsaSeriesMrPhoenixProtocollParadigmPeriodicity != Integer.MIN_VALUE) {
	        	dialog.append("CsaSeries.MrPhoenixProtocol.lParadigmPeriodicity = " +
	        			CsaSeriesMrPhoenixProtocollParadigmPeriodicity + "\n");
	        }
	        if (CsaSeriesMrPhoenixProtocolucCineMode != Integer.MIN_VALUE) {
	        	dialog.append("CsaSeries.MrPhoenixProtocol.ucCineMode = " + 
	        			CsaSeriesMrPhoenixProtocolucCineMode + "\n");
	        }
	        if (CsaSeriesMrPhoenixProtocolucSequenceType != Integer.MIN_VALUE) {
	            dialog.append("CsaSeries.MrPhoenixProtocol.ucSequenceType = " + 
	            		CsaSeriesMrPhoenixProtocolucSequenceType + "\n");	
	        }
	        if (CsaSeriesMrPhoenixProtocolucCoilCombineMode != Integer.MIN_VALUE) {
	        	dialog.append("CsaSeries.MrPhoenixProtocol.ucCoilCombineMode = " +
	        			CsaSeriesMrPhoenixProtocolucCoilCombineMode + "\n");
	        }
	        if (CsaSeriesMrPhoenixProtocolucFlipAngleMode != Integer.MIN_VALUE) {
	        	dialog.append("CsaSeries.MrPhoenixProtocol.ucFlipAngleMode = " +
	        			CsaSeriesMrPhoenixProtocolucFlipAngleMode + "\n");
	        }
	        if (CsaSeriesMrPhoenixProtocollTOM != Integer.MIN_VALUE) {
	        	dialog.append("CsaSeries.MrPhoenixProtocol.lTOM = " + CsaSeriesMrPhoenixProtocollTOM + "\n");
	        }
	        if (CsaSeriesMrPhoenixProtocollProtID != Integer.MIN_VALUE) {
	        	dialog.append("CsaSeries.MrPhoenixProtocol.lProtID = " + CsaSeriesMrPhoenixProtocollProtID + "\n");
	        }
	        if (CsaSeriesMrPhoenixProtocolucReadOutMode != Integer.MIN_VALUE) {
	        	dialog.append("CsaSeries.MrPhoenixProtocol.ucReadOutMode = " +
	        			CsaSeriesMrPhoenixProtocolucReadOutMode + "\n");
	        }
	        if (CsaSeriesMrPhoenixProtocolucBold3dPace != Integer.MIN_VALUE) {
	        	dialog.append("CsaSeries.MrPhoenixProtocol.ucBold3dPace = " +
	        			CsaSeriesMrPhoenixProtocolucBold3dPace + "\n");
	        }
	        if (CsaSeriesMrPhoenixProtocolucForcePositioningOnNDIS != Integer.MIN_VALUE) {
	        	dialog.append("CsaSeries.MrPhoenixProtocol.ucForcePositioningOnNDIS = " +
	        			CsaSeriesMrPhoenixProtocolucForcePositioningOnNDIS + "\n");
	        }
	        if (CsaSeriesMrPhoenixProtocolucInteractiveRealtime != Integer.MIN_VALUE) {
	        	dialog.append("CsaSeries.MrPhoenixProtocol.ucInteractiveRealtime = " +
	        			CsaSeriesMrPhoenixProtocolucInteractiveRealtime + "\n");
	        }
	        if (CsaSeriesMrPhoenixProtocolucInternalTablePosValid != Integer.MIN_VALUE) {
	        	dialog.append("CsaSeries.MrPhoenixProtocol.ucInternalTablePosValid = " + 
	        			CsaSeriesMrPhoenixProtocolucInternalTablePosValid + "\n");
	        }
	        if (CsaSeriesMrPhoenixProtocolsParametricMappingucParametricMap != Integer.MIN_VALUE) {
	        	dialog.append("CsaSeries.MrPhoenixProtocol.sParametricMapping.ucParametricMap = " +
	        			CsaSeriesMrPhoenixProtocolsParametricMappingucParametricMap + "\n");
	        }
	        if (CsaSeriesMrPhoenixProtocolsAslulMode != Integer.MIN_VALUE) {
	        	dialog.append("CsaSeries.MrPhoenixProtocol.sAsl.ulMode = " + CsaSeriesMrPhoenixProtocolsAslulMode + "\n");
	        }
	        if (CsaSeriesMrPhoenixProtocolWaitForUserStart != Integer.MIN_VALUE) {
	        	dialog.append("CsaSeries.MrPhoenixProtocol.WaitForUserStart = " +
	        			CsaSeriesMrPhoenixProtocolWaitForUserStart + "\n");
	        }
	        if (CsaSeriesMrPhoenixProtocolucAutoAlignInit != Integer.MIN_VALUE) {
	        	dialog.append("CsaSeries.MrPhoenixProtocol.ucAutoAlignInit = " +
	        			CsaSeriesMrPhoenixProtocolucAutoAlignInit + "\n");
	        }
	        if (AcquisitionTime != null) {
	        	for (i = 0; i < AcquisitionTime.length; i++) {
	        		if (!Double.isNaN(AcquisitionTime[i])) {
	        			dialog.append("AcquisitionTime["+i+"] = " + AcquisitionTime[i] + "\n");
	        		}
	        	}
	        }
	        if (AcquisitionNumber != null) {
	        	for (i = 0; i < AcquisitionNumber.length; i++) {
	        		if (AcquisitionNumber[i] != Integer.MIN_VALUE) {
	        			dialog.append("AcquisitionNumber["+i+"] = " + AcquisitionNumber[i] + "\n");
	        		}
	        	}
	        }
	        if (InstanceNumber != null) {
	        	for (i = 0; i < InstanceNumber.length; i++) {
	        		if (InstanceNumber[i] != Integer.MIN_VALUE) {
	        			dialog.append("InstanceNumber["+i+"] = " + InstanceNumber[i] + "\n");
	        		}
	        	}
	        }
	        if (WindowCenter != null) {
	        	for (i = 0; i < WindowCenter.length; i++) {
	        		if (!Double.isNaN(WindowCenter[i])) {
	        			dialog.append("WindowCenter["+i+"] = " + WindowCenter[i] + "\n");
	        		}
	        	}
	        }
	        if (WindowWidth != null) {
	        	for (i = 0; i < WindowWidth.length; i++) {
	        		if (!Double.isNaN(WindowWidth[i])) {
	        			dialog.append("WindowWidth["+i+"] = " + WindowWidth[i] + "\n");
	        		}
	        	}
	        }
	        if (CsaImageTimeAfterStart != null) {
	        	for (i = 0; i < CsaImageTimeAfterStart.length; i++) {
	        		if (!Double.isNaN(CsaImageTimeAfterStart[i])) {
	        			dialog.append("CsaImage.TimeAfterStart["+i+"] = " + CsaImageTimeAfterStart[i] + "\n");
	        		}
	        	}
	        }
	        if (CsaImageICE_Dims != null) {
	        	for (i = 0; i < CsaImageICE_Dims.length; i++) {
	        		if (CsaImageICE_Dims[i] != null) {
	        			dialog.append("CsaImage.ICE_Dims["+i+"] = " + CsaImageICE_Dims[i] + "\n");
	        		}
	        	}
	        }
	        if (CsaImageSliceMeasurementDuration != null) {
	        	for (i = 0; i < CsaImageSliceMeasurementDuration.length; i++) {
	        		if (!Double.isNaN(CsaImageSliceMeasurementDuration[i])) {
	        			dialog.append("CsaImage.SliceMeasurementDuration["+i+"] = " +
	        		           CsaImageSliceMeasurementDuration[i] + "\n");
	        		}
	        	}
	        }
	        if (InstanceCreationTime != null) {
	        	for (i = 0; i < InstanceCreationTime.length; i++) {
	        		if (!Double.isNaN(InstanceCreationTime[i])) {
	        			dialog.append("InstanceCreationTime["+i+"] = " + InstanceCreationTime[i] + "\n");
	        		}
	        	}
	        }
	        if (ContentTime != null) {
	        	for (i = 0; i < ContentTime.length; i++) {
	        		if (!Double.isNaN(ContentTime[i])) {
	        			dialog.append("ContentTime["+i+"] = " + ContentTime[i] + "\n");
	        		}
	        	}
	        }
	        if (dcmmeta_shape != null) {
	        	for (i = 0; i < dcmmeta_shape.length; i++) {
	        		if (dcmmeta_shape[i] != Integer.MIN_VALUE) {
	        			dialog.append("dcmmeta_shape["+i+"] = " + dcmmeta_shape[i] + "\n");
	        		}
	        	}
	        }
	        if (dcmmeta_affine != null) {
	        	for (i = 0; i < dcmmeta_affine.length; i++) {
	        		if (dcmmeta_affine[i] != null) {
	        			for (j = 0; j < dcmmeta_affine[i].length; j++) {
	        				if (!Double.isNaN(dcmmeta_affine[i][j])) {
	        					dialog.append("dcmmeta_affine["+i+"]["+j+"] = " + dcmmeta_affine[i][j] + "\n");
	        				}
	        			}
	        		}
	        	}
	        }
	        if (dcmmeta_reorient_transform != null) {
	        	for (i = 0; i < dcmmeta_reorient_transform.length; i++) {
	        		if (dcmmeta_reorient_transform[i] != null) {
	        			for (j = 0; j < dcmmeta_reorient_transform[i].length; j++) {
	        				if (!Double.isNaN(dcmmeta_reorient_transform[i][j])) {
	        					dialog.append("dcmmeta_reorient_transform["+i+"]["+j+"] = " + 
	        							dcmmeta_reorient_transform[i][j] + "\n");
	        				}
	        			}
	        		}
	        	}
	        }
	        if (dcmmeta_slice_dim != Integer.MIN_VALUE) {
	        	dialog.append("dcmmeta_slice_dim = " + dcmmeta_slice_dim + "\n");
	        }
	        if (!Double.isNaN(dcmmeta_version)) {
	        	dialog.append("dcmmeta_version = " + dcmmeta_version + "\n");
	        }
        } // if (haveJson)

        if (matrixQ != null) {
            dialog.append("Qform Matrix = \n" + matrixQ.matrixToString(10, 4));
        }

        if (matrixS != null) {
            dialog.append("Sform Matrix = \n" + matrixS.matrixToString(10, 4));
        }

    }

    private String ecodeIntToString(final int ecode) {
        String ecodeStr = null;
        switch (ecode) {
            case 0:
                if (haveDcmMeta) {
                    ecodeStr = "DcmMeta encoded with JSON";
                } else {
                    ecodeStr = "Unknown private format";
                }
                break;
            case 2:
                ecodeStr = "DICOM format (attribute tags and values)";
                break;
            case 4:
                ecodeStr = "AFNI group (ASCII XML-ish elements)";
                break;
            case 6:
                ecodeStr = "Comment: arbitrary non-NUL ASCII text";
                break;
            case 8:
                ecodeStr = "XCEDE metadata";
                break;
            case 10:
                ecodeStr = "Dimensional information for the JIM software (XML format)";
                break;
            case 12:
                ecodeStr = "Fiswidget XML pipeline descriptions";
                break;
            case 18:
                ecodeStr = "MIND_IDENT field with character data";
                break;
            case 20:
                ecodeStr = "B_VALUE for b-value in units of s/mm-squared";
                break;
            case 22:
                ecodeStr = "SPHERICAL_DIRECTION with spherical coordinates";
                break;
            case 24:
                ecodeStr = "DT_COMPONENT specifying the indicies of a single diffusion tensor component";
                break;
            case 26:
                ecodeStr = "SHC_DEGREEORDER specifying degree and order of a spherical harmonic basis function";
                break;
            case 30:
                ecodeStr = "CARET with XML extension\n";
                break;
            default:
                ecodeStr = "Unrecognized ecode value";
        }
        return ecodeStr;
    }

    /**
     * accessor to the aux_file string.
     * 
     * @return String aux_file
     */
    public String getAuxFile() {
        return aux_file;
    }

    /**
     * accessor to the bitpix value.
     * 
     * @return short the bitpix value.
     */
    public short getBitPix() {
        return bitpix;
    }

    /**
     * accessor to cal-max.
     * 
     * @return float cal_max
     */
    public float getCalMax() {
        return cal_max;
    }

    /**
     * accessor to cal-min.
     * 
     * @return float cal_min
     */
    public float getCalMin() {
        return cal_min;
    }

    /**
     * Returns type of x, y, z coordinates.
     * 
     * @return coord_code
     */
    public short getCoordCode() {
        return coord_code;
    }

    /**
     * Returns type of x, y, z coordinates 2.
     * 
     * @return coord_code2
     */
    public short getCoordCode2() {
        return coord_code2;
    }

    /**
     * accessor to the current analyze-image description.
     * 
     * @return String description
     */
    public String getDescription() {
        return descrip;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public int getFreqDim() {
        return freq_dim;
    }

    /**
     * Accessor that returns the intent code.
     * 
     * @return intentCode
     */
    public short getIntentCode() {
        return intentCode;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public String getIntentName() {
        return intentName;
    }

    /**
     * Accessor that returns first statistical parameter.
     * 
     * @return intentP1
     */
    public float getIntentP1() {
        return intentP1;
    }

    /**
     * Accessor that returns second statistical parameter.
     * 
     * @return intentP2
     */
    public float getIntentP2() {
        return intentP2;
    }

    /**
     * Accessor that returns third statistical parameter.
     * 
     * @return intentP3
     */
    public float getIntentP3() {
        return intentP3;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public int getPhaseDim() {
        return phase_dim;
    }

    /**
     * Gets the data additive factor.
     * 
     * @return scl_inter
     */
    public float getSclInter() {
        return scl_inter;
    }

    /**
     * Gets the data scaling multiplicative factor.
     * 
     * @return scl_slope
     */
    public float getSclSlope() {
        return scl_slope;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public int getSizeOfHeader() {
        return sizeof_hdr;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public byte getSliceCode() {
        return sliceCode;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public int getSliceDim() {
        return slice_dim;
    }

    /**
     * provides the sliceDuration value.
     * 
     * @return float sliceDuration
     */
    public float getSliceDuration() {
        return sliceDuration;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public short getSliceEnd() {
        return sliceEnd;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public short getSliceStart() {
        return sliceStart;
    }

    /**
     * accessor to the sourceBitPix value.
     * 
     * @return short the sourceBitPix value.
     */
    public short getSourceBitPix() {
        return sourceBitPix;
    }

    /**
     * accessor to coded datatype value.
     * 
     * @return short datatype
     */
    public short getSourceType() {
        return sourceType;
    }

    /**
     * accessor to the vox offset value.
     * 
     * @return float vox_offset
     */
    public float getVoxOffset() {
        return vox_offset;
    }

    /**
     * Return String put in DICOM 0020,0037 field
     * 
     * @return
     */
    public String getPatientOrientationString() {
        return patientOrientationString;
    }

    /**
     * supplies auxiliary-file string; permits no more than 24 characters.
     * 
     * @param aux DOCUMENT ME!
     */
    public void setAuxFile(final String aux) {
        aux_file = setString(aux, 24);
    }

    /**
     * sets bitpix; any value other than 1, 8, 16, 32, 64, 128, or 24 gets set to the dissalowed trap value, -1.
     * 
     * @param bp DOCUMENT ME!
     */
    public void setBitPix(final short bp) {

        if ( (bp == 1) || (bp == 8) || (bp == 16) || (bp == 32) || (bp == 64) || (bp == 128) || (bp == 24)) {
            bitpix = bp;
        } else {
            bitpix = -1;
        } // a disallowed trap value
    }

    /**
     * sets cal-max. if supplied value is less than cal-min, the cal-min gets reset to the supplied value as well, so
     * that cal-min is still no greater than cal-max.
     * 
     * @param cal DOCUMENT ME!
     */
    public void setCalMax(final float cal) {
        cal_max = cal;

        if (cal_max < cal_min) {
            cal_min = cal_max;
        }
    }

    /**
     * sets cal-min. if supplied value is greater than cal-max, the cal-max gets reset to the supplied value as well, so
     * that cal-max is still no less than cal-min.
     * 
     * @param cal DOCUMENT ME!
     */
    public void setCalMin(final float cal) {
        cal_min = cal;

        if (cal_min > cal_max) {
            cal_max = cal_min;
        }
    }

    /**
     * Sets type of xyz coordinates.
     * 
     * @param coord_code DOCUMENT ME!
     */
    public void setCoordCode(final short coord_code) {
        this.coord_code = coord_code;
    }

    /**
     * Sets type of xyz coordinates 2.
     * 
     * @param coord_code2 DOCUMENT ME!
     */
    public void setCoordCode2(final short coord_code2) {
        this.coord_code2 = coord_code2;
    }

    /**
     * allows no more than 80 characters to fill in the analyze-image description.
     * 
     * @param description DOCUMENT ME!
     */
    public void setDescription(final String description) {
        descrip = setString(description, 80);
    }

    /**
     * DOCUMENT ME!
     * 
     * @param freq_dim DOCUMENT ME!
     */
    public void setFreqDim(final int freq_dim) {
        this.freq_dim = freq_dim;
    }

    /**
     * Accessor that sets the stat code.
     * 
     * @param intentCode DOCUMENT ME!
     */
    public void setIntentCode(final short intentCode) {
        this.intentCode = intentCode;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param intentName DOCUMENT ME!
     */
    public void setIntentName(final String intentName) {
        this.intentName = intentName;
    }

    /**
     * Accessor that sets first statistical parameter.
     * 
     * @param intentP1 DOCUMENT ME!
     */
    public void setIntentP1(final float intentP1) {
        this.intentP1 = intentP1;
    }

    /**
     * Accessor that sets second statistical parameter.
     * 
     * @param intentP2 DOCUMENT ME!
     */
    public void setIntentP2(final float intentP2) {
        this.intentP2 = intentP2;
    }

    /**
     * Accessor that sets third statistical parameter.
     * 
     * @param intentP3 DOCUMENT ME!
     */
    public void setIntentP3(final float intentP3) {
        this.intentP3 = intentP3;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param phase_dim DOCUMENT ME!
     */
    public void setPhaseDim(final int phase_dim) {
        this.phase_dim = phase_dim;
    }

    /**
     * Sets the data additive factor.
     * 
     * @param scl_inter DOCUMENT ME!
     */
    public void setSclInter(final float scl_inter) {
        this.scl_inter = scl_inter;
    }

    /**
     * Sets the data scaling multiplicative factor.
     * 
     * @param scl_slope DOCUMENT ME!
     */
    public void setSclSlope(final float scl_slope) {
        this.scl_slope = scl_slope;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param size DOCUMENT ME!
     */
    public void setSizeOfHeader(final int size) {
        sizeof_hdr = size;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param sliceCode DOCUMENT ME!
     */
    public void setSliceCode(final byte sliceCode) {
        this.sliceCode = sliceCode;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param slice_dim DOCUMENT ME!
     */
    public void setSliceDim(final int slice_dim) {
        this.slice_dim = slice_dim;
    }

    /**
     * sets the sliceDuration variable.
     * 
     * @param sliceDuration DOCUMENT ME!
     */
    public void setSliceDuration(final float sliceDuration) {
        this.sliceDuration = sliceDuration;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param sliceEnd DOCUMENT ME!
     */
    public void setSliceEnd(final short sliceEnd) {
        this.sliceEnd = sliceEnd;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param sliceStart DOCUMENT ME!
     */
    public void setSliceStart(final short sliceStart) {
        this.sliceStart = sliceStart;
    }

    /**
     * sets sourceBitPix; any value other than 1, 8, 16, 32, 64, 128, or 24 gets set to the disallowed trap value, -1.
     * 
     * @param bp DOCUMENT ME!
     */
    public void setSourceBitPix(final short bp) {

        if ( (bp == 1) || (bp == 8) || (bp == 16) || (bp == 32) || (bp == 64) || (bp == 128) || (bp == 24)) {
            sourceBitPix = bp;
        } else {
            sourceBitPix = -1;
        } // a disallowed trap value
    }

    /**
     * accessor to supply coded datatype.
     * 
     * @param dtype DOCUMENT ME!
     */
    // Data type before conversion for scl_slope and scl_offset
    public void setSourceType(final short dtype) {
        sourceType = dtype;
    }

    /**
     * sets vox offset value.
     * 
     * @param vox DOCUMENT ME!
     */
    public void setVoxOffset(final float vox) {
        vox_offset = vox;
    }

    /**
     * Sets esize array, the number of bytes in each header field in the header extension
     * 
     * @param esize
     */
    public void setEsize(final int esize[]) {
        this.esize = esize;
    }

    public int[] getEsize() {
        return esize;
    }

    /**
     * Sets ecode array, the data format of the header field in the header extension
     * 
     * @param ecode
     */
    public void setEcode(final int ecode[]) {
        this.ecode = ecode;
    }

    /**
     * Sets mindIdent array, character data which serve to identify the type of DWI data structure represented by the
     * MIND extended header fields which follow
     * 
     * @param mindIdent
     */
    public void setMindIdent(final String mindIdent[]) {
        this.mindIdent = mindIdent;
    }

    /**
     * Sets floating point array with diffusion-weighting b-values in units of s/mm-squared.
     * 
     * @param bValue
     */
    public void setBValue(final float bValue[]) {
        this.bValue = bValue;
    }

    /**
     * Sets azimuthal angle array for spherical direction
     * 
     * @param azimuth
     */
    public void setAzimuth(final float azimuth[]) {
        this.azimuth = azimuth;
    }

    /**
     * Sets zenith angle array for spherical direction
     * 
     * @param zenith
     */
    public void setZenith(final float zenith[]) {
        this.zenith = zenith;
    }

    /**
     * Sets degree array for set of spherical harmonic basis functions
     * 
     * @param degree
     */
    public void setDegree(final int degree[]) {
        this.degree = degree;
    }

    /**
     * Sets order array for set of spherical harmonic basis functions
     * 
     * @param order
     */
    public void setOrder(final int order[]) {
        this.order = order;
    }

    /**
     * Sets array of afni group xml inclusions
     * 
     * @param afniGroup
     */
    public void setAfniGroup(final String afniGroup[]) {
        this.afniGroup = afniGroup;
    }

    /**
     * Sets ascii text fields of header extension
     * 
     * @param asciiText
     */
    public void setAsciiText(final String asciiText[]) {
        this.asciiText = asciiText;
    }

    /**
     * Sets dt component array
     * 
     * @param dtComponent
     */
    public void setDTComponent(final int dtComponent[][]) {
        this.dtComponent = dtComponent;
    }

    /**
     * Sets caret array for extension header
     * 
     * @param caret
     */
    public void setCaret(final String caret[]) {
        this.caret = caret;
    }

    public void setMatrixQ(final TransMatrix matrixQ) {
        this.matrixQ = matrixQ;
    }

    public void setMatrixS(final TransMatrix matrixS) {
        this.matrixS = matrixS;
    }

    /**
     * Set String put in DICOM 0020, 0037
     * 
     * @param patientOrientationString
     */
    public void setPatientOrientationString(final String patientOrientationString) {
        this.patientOrientationString = patientOrientationString;
    }

    /**
     * .
     * 
     * <table>
     * <tr>
     * <td>ce[0] = table</td>
     * <td>0 = primary, 1 = secondary, etC</td>
     * </tr>
     * <tr>
     * <td>ce[1] = line of table</td>
     * <td></td>
     * </tr>
     * <tr>
     * <td>ce[2] = string name</td>
     * <td>eg, "Type"</td>
     * </tr>
     * <tr>
     * <td>ce[3] = Vector codeValue</td>
     * <td>eg, "B"</td>
     * </tr>
     * <tr>
     * <td>ce[4] = string value</td>
     * <td>eg, "Big"</td>
     * </tr>
     * </table>
     * 
     * "ce" comes from ChangeEvent upon which this is based. care to make our own ChangeEvent to store and handle this?
     * 
     * @param ce DOCUMENT ME!
     */
    @SuppressWarnings("unchecked")
    public void stateChanged(final Vector ce) {
        final String tname = (String) ce.elementAt(2); // [t]able [name]
        final Vector tcvalue = (Vector) ce.elementAt(3); // [t]able [c]ode [value]
        final String tvalue = (String) ce.elementAt(4); // [t]able [value]

        if (tname.equalsIgnoreCase("Description")) {
            setDescription(tvalue);
        } else if (tname.equalsIgnoreCase("voxel offset")) {
            setVoxOffset(Float.parseFloat((String) tcvalue.elementAt(0)));
        } else if (tname.equalsIgnoreCase("cal_min")) {
            setCalMin(Float.parseFloat((String) tcvalue.elementAt(0)));
        } else if (tname.equalsIgnoreCase("cal_max")) {
            setCalMax(Float.parseFloat((String) tcvalue.elementAt(0)));
        } else if (tname.equalsIgnoreCase("Orientation")) {
            super.setImageOrientation( ((Integer) tcvalue.elementAt(0)).intValue());
            // setImageOrientation(((Byte) tcvalue.elementAt(0)).byteValue());
        } else if (tname.startsWith("Axis: x-orientation")) {
            super.setAxisOrientation( ((Integer) tcvalue.elementAt(0)).intValue(), 0);
        } else if (tname.startsWith("Axis: y-orientation")) {
            super.setAxisOrientation( ((Integer) tcvalue.elementAt(0)).intValue(), 1);
        } else if (tname.startsWith("Axis: z-orientation")) {
            super.setAxisOrientation( ((Integer) tcvalue.elementAt(0)).intValue(), 2);
        } else if (tname.startsWith("Start Location: x-axis")) {
            super.setOrigin(Float.parseFloat((String) tcvalue.elementAt(0)), 0);

        } else if (tname.startsWith("Start Location: y-axis")) {
            super.setOrigin(Float.parseFloat((String) tcvalue.elementAt(0)), 1);
        } else if (tname.startsWith("Start Location: z-axis")) {
            super.setOrigin(Float.parseFloat((String) tcvalue.elementAt(0)), 2);
        } else if (tname.equalsIgnoreCase("Orientation")) {
            setImageOrientation( ((Integer) tcvalue.elementAt(0)).intValue());
            // setOrientation(((Byte)tcvalue.elementAt(0)).byteValue());

        } else {
            Preferences.debug("tname: " + tname + ", not found.", Preferences.DEBUG_FILEIO);
        }
    }

    /**
     * Propogates the current file info to another FileInfoNIFTI.
     * 
     * <p>
     * It does not copy over the datatypeCode. (though, aside from, "it isn't in the about table", I can't think of a
     * reason why it shouldn't. but it doesn't.) Also, copied over is bitPix, aux_file.
     * </p>
     * 
     * @param fInfo DOCUMENT ME!
     */
    public void updateFileInfos(final FileInfoNIFTI fInfo) {

        if (this == fInfo) {
            return;
        }

        // fInfo.setAuxFile (this.getAuxFile());// not editable by the table!!
        // fInfo.setBitPix (this.getBitPix()); // not editable by the table!!
        fInfo.setIntentCode(this.getIntentCode());
        fInfo.setIntentP1(this.getIntentP1());
        fInfo.setIntentP2(this.getIntentP2());
        fInfo.setIntentP3(this.getIntentP3());
        fInfo.setCoordCode(this.getCoordCode());
        fInfo.setCoordCode2(this.getCoordCode2());
        fInfo.setCalMin(this.getCalMin());
        fInfo.setCalMax(this.getCalMax());
        fInfo.setSliceDuration(this.getSliceDuration());

        // fInfo.setDataTypeCode (this.getDataTypeCode());//not edited by the table!!
        fInfo.setDescription(this.getDescription());
        fInfo.setSliceStart(this.getSliceStart());
        fInfo.setSliceEnd(this.getSliceEnd());
        fInfo.setFreqDim(this.getFreqDim());
        fInfo.setPhaseDim(this.getPhaseDim());
        fInfo.setSliceDim(this.getSliceDim());
        fInfo.setImageOrientation(this.getImageOrientation());
        fInfo.setVoxOffset(this.getVoxOffset());
        fInfo.setIntentName(this.getIntentName());
    }

    /**
     * verifies string is not larger than len length; strings larger than len, are clipped before being returned.
     * 
     * @see String#substring(int, int)
     * 
     * @return String new substring
     */
    protected String setString(final String str, final int len) {

        if (str.length() < len) {
            return str;
        } else {
            return str.substring(0, len);
        }
    }

    public void setHaveDcmMeta(final boolean haveDcmMeta) {
        this.haveDcmMeta = haveDcmMeta;
    }

    public boolean getHaveDcmMeta() {
        return haveDcmMeta;
    }

    public void setSpecificCharacterSet(final String specificCharacterSet) {
        this.specificCharacterSet = specificCharacterSet;
    }

    public String getSpecificCharacterSet() {
        return specificCharacterSet;
    }

    public void setImageType(final String imageType[]) {
        this.imageType = imageType;
    }

    public String[] getImageType() {
        return imageType;
    }

    public void setStudyTime(final String studyTime) {
        this.studyTime = studyTime;
    }

    public String getStudyTime() {
        return studyTime;
    }

    public void setSeriesTime(final String seriesTime) {
        this.seriesTime = seriesTime;
    }

    public String getSeriesTime() {
        return seriesTime;
    }

    public void setAccessionNumber(final String accessionNumber) {
        this.accessionNumber = accessionNumber;
    }

    public String getAccessionNumber() {
        return accessionNumber;
    }

    public void setModalityString(final String modalityString) {
        this.modalityString = modalityString;
    }

    public String getModalityString() {
        return modalityString;
    }

    public void setManufacturer(final String manufacturer) {
        this.manufacturer = manufacturer;
    }

    public String getManufacturer() {
        return manufacturer;
    }

    public void setManufacturerModelName(final String manufacturerModelName) {
        this.manufacturerModelName = manufacturerModelName;
    }

    public String getManufacturerModelName() {
        return manufacturerModelName;
    }

    public void setScanningSequence(final String scanningSequence) {
        this.scanningSequence = scanningSequence;
    }

    public String getScanningSequence() {
        return scanningSequence;
    }

    public void setSequenceVariant(final String sequenceVariant) {
        this.sequenceVariant = sequenceVariant;
    }

    public String getSequenceVariant() {
        return sequenceVariant;
    }

    public void setScanOptions(final String scanOptions) {
        this.scanOptions = scanOptions;
    }

    public String getScanOptions() {
        return scanOptions;
    }

    public void setMRAcquisitionType(final String MRAcquisitionType) {
        this.MRAcquisitionType = MRAcquisitionType;
    }

    public String getMRAcquisitionType() {
        return MRAcquisitionType;
    }

    public void setSequenceName(final String sequenceName) {
        this.sequenceName = sequenceName;
    }

    public String getSequenceName() {
        return sequenceName;
    }

    public void setAngioFlag(final String angioFlag) {
        this.angioFlag = angioFlag;
    }

    public String getAngioFlag() {
        return angioFlag;
    }

    public void setRepetitionTime(final double repetitionTime) {
        this.repetitionTime = repetitionTime;
    }

    public double getRepetitionTime() {
        return repetitionTime;
    }

    public void setEchoTime(final double echoTime) {
        this.echoTime = echoTime;
    }

    public double getEchoTime() {
        return echoTime;
    }

    public void setNumberOfAverages(final double numberOfAverages) {
        this.numberOfAverages = numberOfAverages;
    }

    public double getNumberOfAverages() {
        return numberOfAverages;
    }

    public void setImagingFrequency(final double imagingFrequency) {
        this.imagingFrequency = imagingFrequency;
    }

    public double getImagingFrequency() {
        return imagingFrequency;
    }

    public void setImagedNucleus(final String imagedNucleus) {
        this.imagedNucleus = imagedNucleus;
    }

    public String getImagedNucleus() {
        return imagedNucleus;
    }

    public void setEchoNumbers(final int echoNumbers) {
        this.echoNumbers = echoNumbers;
    }

    public int getEchoNumbers() {
        return echoNumbers;
    }

    public void setMagneticFieldStrength(final double magneticFieldStrength) {
        this.magneticFieldStrength = magneticFieldStrength;
    }

    public double getMagneticFieldStrength() {
        return magneticFieldStrength;
    }

    public void setSpacingBetweenSlices(final double spacingBetweenSlices) {
        this.spacingBetweenSlices = spacingBetweenSlices;
    }

    public double getSpacingBetweenSlices() {
        return spacingBetweenSlices;
    }

    public void setNumberOfPhaseEncodingSteps(final int numberOfPhaseEncodingSteps) {
        this.numberOfPhaseEncodingSteps = numberOfPhaseEncodingSteps;
    }

    public int getNumberOfPhaseEncodingSteps() {
        return numberOfPhaseEncodingSteps;
    }

    public void setEchoTrainLength(final int echoTrainLength) {
        this.echoTrainLength = echoTrainLength;
    }

    public int getEchoTrainLength() {
        return echoTrainLength;
    }

    public void setPercentSampling(final double percentSampling) {
        this.percentSampling = percentSampling;
    }

    public double getPercentSampling() {
        return percentSampling;
    }

    public void setPercentPhaseFieldOfView(final double percentPhaseFieldOfView) {
        this.percentPhaseFieldOfView = percentPhaseFieldOfView;
    }

    public double getPercentPhaseFieldOfView() {
        return percentPhaseFieldOfView;
    }

    public void setPixelBandwidth(final double pixelBandwidth) {
        this.pixelBandwidth = pixelBandwidth;
    }

    public double getPixelBandwidth() {
        return pixelBandwidth;
    }

    public void setSoftwareVersions(final String softwareVersions) {
        this.softwareVersions = softwareVersions;
    }

    public String getSoftwareVersions() {
        return softwareVersions;
    }

    public void setTransmitCoilName(final String transmitCoilName) {
        this.transmitCoilName = transmitCoilName;
    }

    public String getTransmitCoilName() {
        return transmitCoilName;
    }

    public void setAcquisitionMatrix(final int acquisitionMatrix[]) {
        this.acquisitionMatrix = acquisitionMatrix;
    }

    public int[] getAcquisitionMatrix() {
        return acquisitionMatrix;
    }

    public void setInPlanePhaseEncodingDirection(final String inPlanePhaseEncodingDirection) {
        this.inPlanePhaseEncodingDirection = inPlanePhaseEncodingDirection;
    }

    public String getInPlanePhaseEncodingDirection() {
        return inPlanePhaseEncodingDirection;
    }

    public void setFlipAngle(final double flipAngle) {
        this.flipAngle = flipAngle;
    }

    public double getFlipAngle() {
        return flipAngle;
    }

    public void setVariableFlipAngleFlag(final String variableFlipAngleFlag) {
        this.variableFlipAngleFlag = variableFlipAngleFlag;
    }

    public String getVariableFlipAngleFlag() {
        return variableFlipAngleFlag;
    }

    public void setSAR(final double SAR) {
        this.SAR = SAR;
    }

    public double getSAR() {
        return SAR;
    }

    public void setDBdt(final double dBdt) {
        this.dBdt = dBdt;
    }

    public double getDBdt() {
        return dBdt;
    }

    public void setSeriesNumber(final int seriesNumber) {
        this.seriesNumber = seriesNumber;
    }

    public int getSeriesNumber() {
        return seriesNumber;
    }

    public void setImagePositionPatient(final double imagePositionPatient[]) {
        this.imagePositionPatient = imagePositionPatient;
    }

    public double[] getImagePositionPatient() {
        return imagePositionPatient;
    }

    public void setImageOrientationPatient(final double imageOrientationPatient[]) {
        this.imageOrientationPatient = imageOrientationPatient;
    }

    public double[] getImageOrientationPatient() {
        return imageOrientationPatient;
    }

    public void setSliceLocation(final double sliceLocation) {
        this.sliceLocation = sliceLocation;
    }

    public double getSliceLocation() {
        return sliceLocation;
    }

    public void setSamplesPerPixel(final int samplesPerPixel) {
        this.samplesPerPixel = samplesPerPixel;
    }

    public int getSamplesPerPixel() {
        return samplesPerPixel;
    }

    public void setPhotometricInterpretation(final String photometricInterpretation) {
        this.photometricInterpretation = photometricInterpretation;
    }

    public String getPhotometricInterpretation() {
        return photometricInterpretation;
    }

    public void setRows(final int rows) {
        this.rows = rows;
    }

    public int getRows() {
        return rows;
    }

    public void setColumns(final int columns) {
        this.columns = columns;
    }

    public int getColumns() {
        return columns;
    }

    public void setPixelSpacing(final double pixelSpacing[]) {
        this.pixelSpacing = pixelSpacing;
    }

    public double[] getPixelSpacing() {
        return pixelSpacing;
    }

    public void setBitsAllocated(final int bitsAllocated) {
        this.bitsAllocated = bitsAllocated;
    }

    public int getBitsAllocated() {
        return bitsAllocated;
    }

    public void setBitsStored(final int bitsStored) {
        this.bitsStored = bitsStored;
    }

    public int getBitsStored() {
        return bitsStored;
    }

    public void setHighBit(final int highBit) {
        this.highBit = highBit;
    }

    public int getHighBit() {
        return highBit;
    }

    public void setPixelRepresentation(final int pixelRepresentation) {
        this.pixelRepresentation = pixelRepresentation;
    }

    public int getPixelRepresentation() {
        return pixelRepresentation;
    }

    public void setSmallestImagePixelValue(final int smallestImagePixelValue) {
        this.smallestImagePixelValue = smallestImagePixelValue;
    }

    public int getSmallestImagePixelValue() {
        return smallestImagePixelValue;
    }

    public void setLargestImagePixelValue(final int largestImagePixelValue) {
        this.largestImagePixelValue = largestImagePixelValue;
    }

    public int getLargestImagePixelValue() {
        return largestImagePixelValue;
    }

    public void setWindowCenterWidthExplanation(final String windowCenterWidthExplanation) {
        this.windowCenterWidthExplanation = windowCenterWidthExplanation;
    }

    public String getWindowCenterWidthExplanation() {
        return windowCenterWidthExplanation;
    }

    public void setPerformedProcedureStepStartTime(final String performedProcedureStepStartTime) {
        this.performedProcedureStepStartTime = performedProcedureStepStartTime;
    }

    public String getPerformedProcedureStepStartTime() {
        return performedProcedureStepStartTime;
    }

    public void setCsaImageEchoLinePosition(final int CsaImageEchoLinePosition) {
        this.CsaImageEchoLinePosition = CsaImageEchoLinePosition;
    }

    public int getCsaImageEchoLinePosition() {
        return CsaImageEchoLinePosition;
    }

    public void setCsaImageProtocolSliceNumber(final int CsaImageProtocolSliceNumber) {
        this.CsaImageProtocolSliceNumber = CsaImageProtocolSliceNumber;
    }

    public int getCsaImageProtocolSliceNumber() {
        return CsaImageProtocolSliceNumber;
    }

    public void setCsaImageUsedChannelMask(final int CsaImageUsedChannelMask) {
        this.CsaImageUsedChannelMask = CsaImageUsedChannelMask;
    }

    public int getCsaImageUsedChannelMask() {
        return CsaImageUsedChannelMask;
    }

    public void setCsaImageBandwidthPerPixelPhaseEncode(final double CsaImageBandwidthPerPixelPhaseEncode) {
        this.CsaImageBandwidthPerPixelPhaseEncode = CsaImageBandwidthPerPixelPhaseEncode;
    }

    public double getCsaImageBandwidthPerPixelPhaseEncode() {
        return CsaImageBandwidthPerPixelPhaseEncode;
    }

    public void setCsaImageMeasuredFourierLines(final int CsaImageMeasuredFourierLines) {
        this.CsaImageMeasuredFourierLines = CsaImageMeasuredFourierLines;
    }

    public int getCsaImageMeasuredFourierLines() {
        return CsaImageMeasuredFourierLines;
    }

    public void setCsaImageSequenceMask(final int CsaImageSequenceMask) {
        this.CsaImageSequenceMask = CsaImageSequenceMask;
    }

    public int getCsaImageSequenceMask() {
        return CsaImageSequenceMask;
    }

    public void setCsaImageRFSWDDataType(final String CsaImageRFSWDDataType) {
        this.CsaImageRFSWDDataType = CsaImageRFSWDDataType;
    }

    public String getCsaImageRFSWDDataType() {
        return CsaImageRFSWDDataType;
    }

    public void setCsaImageImaPATModeText(final String CsaImageImaPATModeText) {
        this.CsaImageImaPATModeText = CsaImageImaPATModeText;
    }

    public String getCsaImageImaPATModeText() {
        return CsaImageImaPATModeText;
    }

    public void setCsaImageRealDwellTime(final int CsaImageRealDwellTime) {
        this.CsaImageRealDwellTime = CsaImageRealDwellTime;
    }

    public int getCsaImageRealDwellTime() {
        return CsaImageRealDwellTime;
    }

    public void setCsaImageImaCoilString(final String CsaImageImaCoilString) {
        this.CsaImageImaCoilString = CsaImageImaCoilString;
    }

    public String getCsaImageImaCoilString() {
        return CsaImageImaCoilString;
    }

    public void setCsaImageEchoColumnPosition(final int CsaImageEchoColumnPosition) {
        this.CsaImageEchoColumnPosition = CsaImageEchoColumnPosition;
    }

    public int getCsaImageEchoColumnPosition() {
        return CsaImageEchoColumnPosition;
    }

    public void setCsaImagePhaseEncodingDirectionPositive(final int CsaImagePhaseEncodingDirectionPositive) {
        this.CsaImagePhaseEncodingDirectionPositive = CsaImagePhaseEncodingDirectionPositive;
    }

    public int getCsaImagePhaseEncodingDirectionPositive() {
        return CsaImagePhaseEncodingDirectionPositive;
    }

    public void setCsaImageSlicePosition_PCS(final double CsaImageSlicePosition_PCS[]) {
        this.CsaImageSlicePosition_PCS = CsaImageSlicePosition_PCS;
    }

    public double[] getCsaImageSlicePosition_PCS() {
        return CsaImageSlicePosition_PCS;
    }

    public void setCsaImageSliceNormalVector(final double CsaImageSliceNormalVector[]) {
        this.CsaImageSliceNormalVector = CsaImageSliceNormalVector;
    }

    public double[] getCsaImageSliceNormalVector() {
        return CsaImageSliceNormalVector;
    }

    public void setCsaImageGSWDDataType(final String CsaImageGSWDDataType) {
        this.CsaImageGSWDDataType = CsaImageGSWDDataType;
    }

    public String getCsaImageGSWDDataType() {
        return CsaImageGSWDDataType;
    }

    public void setCsaImageMultistepIndex(final int CsaImageMultistepIndex) {
        this.CsaImageMultistepIndex = CsaImageMultistepIndex;
    }

    public int getCsaImageMultistepIndex() {
        return CsaImageMultistepIndex;
    }

    public void setCsaImageImaRelTablePosition(final int CsaImageImaRelTablePosition[]) {
        this.CsaImageImaRelTablePosition = CsaImageImaRelTablePosition;
    }

    public int[] getCsaImageImaRelTablePosition() {
        return CsaImageImaRelTablePosition;
    }

    public void setCsaImageNumberOfImagesInMosaic(final int CsaImageNumberOfImagesInMosaic) {
        this.CsaImageNumberOfImagesInMosaic = CsaImageNumberOfImagesInMosaic;
    }

    public int getCsaImageNumberOfImagesInMosaic() {
        return CsaImageNumberOfImagesInMosaic;
    }

    public void setCsaImageNonPlanarImage(final int CsaImageNonPlanarImage) {
        this.CsaImageNonPlanarImage = CsaImageNonPlanarImage;
    }

    public int getCsaImageNonPlanarImage() {
        return CsaImageNonPlanarImage;
    }

    public void setCsaImageEchoPartitionPosition(final int CsaImageEchoPartitionPosition) {
        this.CsaImageEchoPartitionPosition = CsaImageEchoPartitionPosition;
    }

    public int getCsaImageEchoPartitionPosition() {
        return CsaImageEchoPartitionPosition;
    }

    public void setCsaImageAcquisitionMatrixText(final String CsaImageAcquisitionMatrixText) {
        this.CsaImageAcquisitionMatrixText = CsaImageAcquisitionMatrixText;
    }

    public String getCsaImageAcquisitionMatrixText() {
        return CsaImageAcquisitionMatrixText;
    }

    public void setCsaImageImaAbsTablePosition(final int CsaImageImaAbsTablePosition[]) {
        this.CsaImageImaAbsTablePosition = CsaImageImaAbsTablePosition;
    }

    public int[] getCsaImageImaAbsTablePosition() {
        return CsaImageImaAbsTablePosition;
    }

    public void setCsaSeriesTalesReferencePower(final double CsaSeriesTalesReferencePower) {
        this.CsaSeriesTalesReferencePower = CsaSeriesTalesReferencePower;
    }

    public double getCsaSeriesTalesReferencePower() {
        return CsaSeriesTalesReferencePower;
    }

    public void setCsaSeriesOperation_mode_flag(final int CsaSeriesOperation_mode_flag) {
        this.CsaSeriesOperation_mode_flag = CsaSeriesOperation_mode_flag;
    }

    public int getCsaSeriesOperation_mode_flag() {
        return CsaSeriesOperation_mode_flag;
    }

    public void setCsaSeriesdBdt_thresh(final double CsaSeriesdBdt_thresh) {
        this.CsaSeriesdBdt_thresh = CsaSeriesdBdt_thresh;
    }

    public double getCsaSeriesdBdt_thresh() {
        return CsaSeriesdBdt_thresh;
    }

    public void setCsaSeriesProtocolChangeHistory(final int CsaSeriesProtocolChangeHistory) {
        this.CsaSeriesProtocolChangeHistory = CsaSeriesProtocolChangeHistory;
    }

    public int getCsaSeriesProtocolChangeHistory() {
        return CsaSeriesProtocolChangeHistory;
    }

    public void setCsaSeriesGradientDelayTime(final double CsaSeriesGradientDelayTime[]) {
        this.CsaSeriesGradientDelayTime = CsaSeriesGradientDelayTime;
    }

    public double[] getCsaSeriesGradientDelayTime() {
        return CsaSeriesGradientDelayTime;
    }

    public void setCsaSeriesSARMostCriticalAspect(final double CsaSeriesSARMostCriticalAspect[]) {
        this.CsaSeriesSARMostCriticalAspect = CsaSeriesSARMostCriticalAspect;
    }

    public double[] getCsaSeriesSARMostCriticalAspect() {
        return CsaSeriesSARMostCriticalAspect;
    }

    public void setCsaSeriesB1rms(final double CsaSeriesB1rms[]) {
        this.CsaSeriesB1rms = CsaSeriesB1rms;
    }

    public double[] getCsaSeriesB1rms() {
        return CsaSeriesB1rms;
    }

    public void setCsaSeriesPATModeText(final String CsaSeriesPATModeText) {
        this.CsaSeriesPATModeText = CsaSeriesPATModeText;
    }

    public String getCsaSeriesPATModeText() {
        return CsaSeriesPATModeText;
    }

    public void setCsaSeriesRelTablePosition(final int CsaSeriesRelTablePosition[]) {
        this.CsaSeriesRelTablePosition = CsaSeriesRelTablePosition;
    }

    public int[] getCsaSeriesRelTablePosition() {
        return CsaSeriesRelTablePosition;
    }

    public void setCsaSeriesNumberOfPrescans(final int CsaSeriesNumberOfPrescans) {
        this.CsaSeriesNumberOfPrescans = CsaSeriesNumberOfPrescans;
    }

    public int getCsaSeriesNumberOfPrescans() {
        return CsaSeriesNumberOfPrescans;
    }

    public void setCsaSeriesdBdt_limit(final double CsaSeriesdBdt_limit) {
        this.CsaSeriesdBdt_limit = CsaSeriesdBdt_limit;
    }

    public double getCsaSeriesdBdt_limit() {
        return CsaSeriesdBdt_limit;
    }

    public void setCsaSeriesStim_lim(final double CsaSeriesStim_lim[]) {
        this.CsaSeriesStim_lim = CsaSeriesStim_lim;
    }

    public double[] getCsaSeriesStim_lim() {
        return CsaSeriesStim_lim;
    }

    public void setCsaSeriesPatReinPattern(final String CsaSeriesPatReinPattern) {
        this.CsaSeriesPatReinPattern = CsaSeriesPatReinPattern;
    }

    public String getCsaSeriesPatReinPattern() {
        return CsaSeriesPatReinPattern;
    }

    public void setCsaSeriesB1rmsSupervision(final String CsaSeriesB1rmsSupervision) {
        this.CsaSeriesB1rmsSupervision = CsaSeriesB1rmsSupervision;
    }

    public String getCsaSeriesB1rmsSupervision() {
        return CsaSeriesB1rmsSupervision;
    }

    public void setCsaSeriesReadoutGradientAmplitude(final double CsaSeriesReadoutGradientAmplitude) {
        this.CsaSeriesReadoutGradientAmplitude = CsaSeriesReadoutGradientAmplitude;
    }

    public double getCsaSeriesReadoutGradientAmplitude() {
        return CsaSeriesReadoutGradientAmplitude;
    }

    public void setCsaSeriesMrProtocolVersion(final int CsaSeriesMrProtocolVersion) {
        this.CsaSeriesMrProtocolVersion = CsaSeriesMrProtocolVersion;
    }

    public int getCsaSeriesMrProtocolVersion() {
        return CsaSeriesMrProtocolVersion;
    }

    public void setCsaSeriesRFSWDMostCriticalAspect(final String CsaSeriesRFSWDMostCriticalAspect) {
        this.CsaSeriesRFSWDMostCriticalAspect = CsaSeriesRFSWDMostCriticalAspect;
    }

    public String getCsaSeriesRFSWDMostCriticalAspect() {
        return CsaSeriesRFSWDMostCriticalAspect;
    }

    public void setCsaSeriesSequenceFileOwner(final String CsaSeriesSequenceFileOwner) {
        this.CsaSeriesSequenceFileOwner = CsaSeriesSequenceFileOwner;
    }

    public String getCsaSeriesSequenceFileOwner() {
        return CsaSeriesSequenceFileOwner;
    }

    public void setCsaSeriesGradientMode(final String CsaSeriesGradientMode) {
        this.CsaSeriesGradientMode = CsaSeriesGradientMode;
    }

    public String getCsaSeriesGradientMode() {
        return CsaSeriesGradientMode;
    }

    public void setCsaSeriesSliceArrayConcatenations(final int CsaSeriesSliceArrayConcatenations) {
        this.CsaSeriesSliceArrayConcatenations = CsaSeriesSliceArrayConcatenations;
    }

    public int getCsaSeriesSliceArrayConcatenations() {
        return CsaSeriesSliceArrayConcatenations;
    }

    public void setCsaSeriesFlowCompensation(final String CsaSeriesFlowCompensation) {
        this.CsaSeriesFlowCompensation = CsaSeriesFlowCompensation;
    }

    public String getCsaSeriesFlowCompensation() {
        return CsaSeriesFlowCompensation;
    }

    public void setCsaSeriesTransmitterCalibration(final double CsaSeriesTransmitterCalibration) {
        this.CsaSeriesTransmitterCalibration = CsaSeriesTransmitterCalibration;
    }

    public double getCsaSeriesTransmitterCalibration() {
        return CsaSeriesTransmitterCalibration;
    }

    public void setCsaSeriesIsocentered(final int CsaSeriesIsocentered) {
        this.CsaSeriesIsocentered = CsaSeriesIsocentered;
    }

    public int getCsaSeriesIsocentered() {
        return CsaSeriesIsocentered;
    }

    public void setCsaSeriesAbsTablePosition(final int CsaSeriesAbsTablePosition) {
        this.CsaSeriesAbsTablePosition = CsaSeriesAbsTablePosition;
    }

    public int getCsaSeriesAbsTablePosition() {
        return CsaSeriesAbsTablePosition;
    }

    public void setCsaSeriesReadoutOS(final double CsaSeriesReadoutOS) {
        this.CsaSeriesReadoutOS = CsaSeriesReadoutOS;
    }

    public double getCsaSeriesReadoutOS() {
        return CsaSeriesReadoutOS;
    }

    public void setCsaSeriesdBdt_max(final double CsaSeriesdBdt_max) {
        this.CsaSeriesdBdt_max = CsaSeriesdBdt_max;
    }

    public double getCsaSeriesdBdt_max() {
        return CsaSeriesdBdt_max;
    }

    public void setCsaSeriesRFSWDOperationMode(final int CsaSeriesRFSWDOperationMode) {
        this.CsaSeriesRFSWDOperationMode = CsaSeriesRFSWDOperationMode;
    }

    public int getCsaSeriesRFSWDOperationMode() {
        return CsaSeriesRFSWDOperationMode;
    }

    public void setCsaSeriesSelectionGradientAmplitude(final double CsaSeriesSelectionGradientAmplitude) {
        this.CsaSeriesSelectionGradientAmplitude = CsaSeriesSelectionGradientAmplitude;
    }

    public double getCsaSeriesSelectionGradientAmplitude() {
        return CsaSeriesSelectionGradientAmplitude;
    }
    
    public void setCsaSeriesPhaseGradientAmplitude(double CsaSeriesPhaseGradientAmplitude) {
    	this.CsaSeriesPhaseGradientAmplitude = CsaSeriesPhaseGradientAmplitude;
    }
    
    public double getCsaSeriesPhaseGradientAmplitude() {
    	return CsaSeriesPhaseGradientAmplitude;
    }
    
    public void setCsaSeriesRfWatchdogMask(int CsaSeriesRfWatchdogMask) {
    	this.CsaSeriesRfWatchdogMask = CsaSeriesRfWatchdogMask;
    }
    

    public int getCsaSeriesRfWatchdogMask() {
        return CsaSeriesRfWatchdogMask;
    }

    public void setCsaSeriesCoilForGradient2(final String CsaSeriesCoilForGradient2) {
        this.CsaSeriesCoilForGradient2 = CsaSeriesCoilForGradient2;
    }

    public String getCsaSeriesCoilForGradient2() {
        return CsaSeriesCoilForGradient2;
    }

    public void setCsaSeriesStim_mon_mode(final int CsaSeriesStim_mon_mode) {
        this.CsaSeriesStim_mon_mode = CsaSeriesStim_mon_mode;
    }

    public int getCsaSeriesStim_mon_mode() {
        return CsaSeriesStim_mon_mode;
    }

    public void setCsaSeriesCoilId(final int CsaSeriesCoilId[]) {
        this.CsaSeriesCoilId = CsaSeriesCoilId;
    }

    public int[] getCsaSeriesCoilId() {
        return CsaSeriesCoilId;
    }

    public void setCsaSeriesStim_max_ges_norm_online(final double CsaSeriesStim_max_ges_norm_online) {
        this.CsaSeriesStim_max_ges_norm_online = CsaSeriesStim_max_ges_norm_online;
    }

    public double getCsaStim_max_ges_norm_online() {
        return CsaSeriesStim_max_ges_norm_online;
    }

    public void setCsaSeriesCoilString(final String CsaSeriesCoilString) {
        this.CsaSeriesCoilString = CsaSeriesCoilString;
    }

    public String getCsaSeriesCoilString() {
        return CsaSeriesCoilString;
    }

    public void setCsaSeriesCoilForGradient(final String CsaSeriesCoilForGradient) {
        this.CsaSeriesCoilForGradient = CsaSeriesCoilForGradient;
    }

    public String getCsaSeriesCoilForGradient() {
        return CsaSeriesCoilForGradient;
    }

    public void setCsaSeriesTablePositionOrigin(final int CsaSeriesTablePositionOrigin[]) {
        this.CsaSeriesTablePositionOrigin = CsaSeriesTablePositionOrigin;
    }

    public int[] getCsaSeriesTablePositionOrigin() {
        return CsaSeriesTablePositionOrigin;
    }

    public void setCsaSeriesMiscSequenceParam(final int CsaSeriesMiscSequenceParam[]) {
        this.CsaSeriesMiscSequenceParam = CsaSeriesMiscSequenceParam;
    }

    public int[] getCsaSeriesMiscSequenceParam() {
        return CsaSeriesMiscSequenceParam;
    }

    public void setCsaSeriesLongModelName(final String CsaSeriesLongModelName) {
        this.CsaSeriesLongModelName = CsaSeriesLongModelName;
    }

    public String getCsaSeriesLongModelName() {
        return CsaSeriesLongModelName;
    }

    public void setCsaSeriesStim_faktor(final double CsaSeriesStim_faktor) {
        this.CsaSeriesStim_faktor = CsaSeriesStim_faktor;
    }

    public double getCsaSeriesStim_faktor() {
        return CsaSeriesStim_faktor;
    }

    public void setCsaSeriesSW_korr_faktor(final double CsaSeriesSW_korr_faktor) {
        this.CsaSeriesSW_korr_faktor = CsaSeriesSW_korr_faktor;
    }

    public double getCsaSeriesSW_korr_faktor() {
        return CsaSeriesSW_korr_faktor;
    }

    public void setCsaSeriesSed(final double CsaSeriesSed[]) {
        this.CsaSeriesSed = CsaSeriesSed;
    }

    public double[] getCsaSeriesSed() {
        return CsaSeriesSed;
    }

    public void setCsaSeriesPositivePCSDirections(final String CsaSeriesPositivePCSDirections) {
        this.CsaSeriesPositivePCSDirections = CsaSeriesPositivePCSDirections;
    }

    public String getCsaSeriesPositivePCSDirections() {
        return CsaSeriesPositivePCSDirections;
    }

    public void setCsaSeriesSliceResolution(final double CsaSeriesSliceResolution) {
        this.CsaSeriesSliceResolution = CsaSeriesSliceResolution;
    }

    public double getCsaSeriesSliceResolution() {
        return CsaSeriesSliceResolution;
    }

    public void setCsaSeriesStim_max_online(final double CsaSeriesStim_max_online[]) {
        this.CsaSeriesStim_max_online = CsaSeriesStim_max_online;
    }

    public double[] getCsaSeriesStim_max_online() {
        return CsaSeriesStim_max_online;
    }

    public void setCsaSeriest_puls_max(final double CsaSeriest_puls_max) {
        this.CsaSeriest_puls_max = CsaSeriest_puls_max;
    }

    public double getCsaSeriest_puls_max() {
        return CsaSeriest_puls_max;
    }

    public void setCsaSeriesMrPhoenixProtocolulVersion(final int CsaSeriesMrPhoenixProtocolulVersion) {
        this.CsaSeriesMrPhoenixProtocolulVersion = CsaSeriesMrPhoenixProtocolulVersion;
    }

    public int getCsaSeriesMrPhoenixProtocolulVersionRfWatchdogMask() {
        return CsaSeriesMrPhoenixProtocolulVersion;
    }

    public void setCsaSeriesMrPhoenixProtocoltSequenceFileName(final String CsaSeriesMrPhoenixProtocoltSequenceFileName) {
        this.CsaSeriesMrPhoenixProtocoltSequenceFileName = CsaSeriesMrPhoenixProtocoltSequenceFileName;
    }

    public String getCsaSeriesMrPhoenixProtocoltSequenceFileName() {
        return CsaSeriesMrPhoenixProtocoltSequenceFileName;
    }

    public void setCsaSeriesMrPhoenixProtocoltProtocolName(final String CsaSeriesMrPhoenixProtocoltProtocolName) {
        this.CsaSeriesMrPhoenixProtocoltProtocolName = CsaSeriesMrPhoenixProtocoltProtocolName;
    }

    public String getCsaSeriesMrPhoenixProtocoltProtocolName() {
        return CsaSeriesMrPhoenixProtocoltProtocolName;
    }

    public void setCsaSeriesMrPhoenixProtocoltReferenceImage0(final String CsaSeriesMrPhoenixProtocoltReferenceImage0) {
        this.CsaSeriesMrPhoenixProtocoltReferenceImage0 = CsaSeriesMrPhoenixProtocoltReferenceImage0;
    }

    public String getCsaSeriesMrPhoenixProtocoltReferenceImage0() {
        return CsaSeriesMrPhoenixProtocoltReferenceImage0;
    }

    public void setCsaSeriesMrPhoenixProtocoltReferenceImage1(final String CsaSeriesMrPhoenixProtocoltReferenceImage1) {
        this.CsaSeriesMrPhoenixProtocoltReferenceImage1 = CsaSeriesMrPhoenixProtocoltReferenceImage1;
    }

    public String getCsaSeriesMrPhoenixProtocoltReferenceImage1() {
        return CsaSeriesMrPhoenixProtocoltReferenceImage1;
    }

    public void setCsaSeriesMrPhoenixProtocoltReferenceImage2(final String CsaSeriesMrPhoenixProtocoltReferenceImage2) {
        this.CsaSeriesMrPhoenixProtocoltReferenceImage2 = CsaSeriesMrPhoenixProtocoltReferenceImage2;
    }

    public String getCsaSeriesMrPhoenixProtocoltReferenceImage2() {
        return CsaSeriesMrPhoenixProtocoltReferenceImage2;
    }

    public void setCsaSeriesMrPhoenixProtocolucScanRegionPosValid(final int CsaSeriesMrPhoenixProtocolucScanRegionPosValid) {
        this.CsaSeriesMrPhoenixProtocolucScanRegionPosValid = CsaSeriesMrPhoenixProtocolucScanRegionPosValid;
    }

    public int getCsaSeriesMrPhoenixProtocolucScanRegionPosValid() {
        return CsaSeriesMrPhoenixProtocolucScanRegionPosValid;
    }

    public void setCsaSeriesMrPhoenixProtocolucTablePositioningMode(final int CsaSeriesMrPhoenixProtocolucTablePositioningMode) {
        this.CsaSeriesMrPhoenixProtocolucTablePositioningMode = CsaSeriesMrPhoenixProtocolucTablePositioningMode;
    }

    public int getCsaSeriesMrPhoenixProtocolucTablePositioningMode() {
        return CsaSeriesMrPhoenixProtocolucTablePositioningMode;
    }

    public void setCsaSeriesMrPhoenixProtocolsProtConsistencyInfotBaselineString(final String CsaSeriesMrPhoenixProtocolsProtConsistencyInfotBaselineString) {
        this.CsaSeriesMrPhoenixProtocolsProtConsistencyInfotBaselineString = CsaSeriesMrPhoenixProtocolsProtConsistencyInfotBaselineString;
    }

    public String getCsaSeriesMrPhoenixProtocolsProtConsistencyInfotBaselineString() {
        return CsaSeriesMrPhoenixProtocolsProtConsistencyInfotBaselineString;
    }

    public void setCsaSeriesMrPhoenixProtocolsProtConsistencyInfotSystemType(final String CsaSeriesMrPhoenixProtocolsProtConsistencyInfotSystemType) {
        this.CsaSeriesMrPhoenixProtocolsProtConsistencyInfotSystemType = CsaSeriesMrPhoenixProtocolsProtConsistencyInfotSystemType;
    }

    public String getCsaSeriesMrPhoenixProtocolsProtConsistencyInfotSystemType() {
        return CsaSeriesMrPhoenixProtocolsProtConsistencyInfotSystemType;
    }

    public void setCsaSeriesMrPhoenixProtocolsProtConsistencyInfoflNominalB0(final double CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflNominalB0) {
        this.CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflNominalB0 = CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflNominalB0;
    }

    public double getCsaSeriesMrPhoenixProtocolsProtConsistencyInfoflNominalB0() {
        return CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflNominalB0;
    }

    public void setCsaSeriesMrPhoenixProtocolsProtConsistencyInfoflGMax(final double CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflGMax) {
        this.CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflGMax = CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflGMax;
    }

    public double getCsaSeriesMrPhoenixProtocolsProtConsistencyInfoflGMax() {
        return CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflGMax;
    }

    public void setCsaSeriesMrPhoenixProtocolsProtConsistencyInfoflRiseTime(final double CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflRiseTime) {
        this.CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflRiseTime = CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflRiseTime;
    }

    public double getCsaSeriesMrPhoenixProtocolsProtConsistencyInfoflRiseTime() {
        return CsaSeriesMrPhoenixProtocolsProtConsistencyInfoflRiseTime;
    }

    public void setCsaSeriesMrPhoenixProtocolsProtConsistencyInfolMaximumNofRxReceiverChannels(
            final int CsaSeriesMrPhoenixProtocolsProtConsistencyInfolMaximumNofRxReceiverChannels) {
        this.CsaSeriesMrPhoenixProtocolsProtConsistencyInfolMaximumNofRxReceiverChannels = CsaSeriesMrPhoenixProtocolsProtConsistencyInfolMaximumNofRxReceiverChannels;
    }

    public int getCsaSeriesMrPhoenixProtocolsProtConsistencyInfolMaximumNofRxReceiverChannels() {
        return CsaSeriesMrPhoenixProtocolsProtConsistencyInfolMaximumNofRxReceiverChannels;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude0(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude0) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude0 = CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude0;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude0() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude0;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude1(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude1) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude1 = CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude1;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude1() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude1;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude2(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude2) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude2 = CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude2;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude2() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude2;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude3(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude3) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude3 = CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude3;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude3() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude3;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude4(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude4) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude4 = CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude4;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude4() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflAmplitude4;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant0(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant0) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant0 = CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant0;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant0() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant0;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant1(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant1) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant1 = CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant1;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant1() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant1;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant2(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant2) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant2 = CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant2;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant2() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant2;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant3(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant3) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant3 = CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant3;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant3() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant3;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant4(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant4) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant4 = CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant4;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant4() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationXaflTimeConstant4;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude0(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude0) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude0 = CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude0;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude0() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude0;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude1(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude1) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude1 = CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude1;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude1() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude1;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude2(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude2) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude2 = CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude2;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude2() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude2;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude3(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude3) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude3 = CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude3;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude3() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude3;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude4(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude4) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude4 = CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude4;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude4() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflAmplitude4;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant0(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant0) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant0 = CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant0;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant0() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant0;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant1(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant1) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant1 = CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant1;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant1() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant1;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant2(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant2) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant2 = CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant2;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant2() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant2;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant3(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant3) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant3 = CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant3;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant3() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant3;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant4(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant4) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant4 = CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant4;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant4() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationYaflTimeConstant4;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude0(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude0) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude0 = CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude0;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude0() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude0;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude1(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude1) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude1 = CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude1;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude1() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude1;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude2(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude2) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude2 = CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude2;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude2() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude2;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude3(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude3) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude3 = CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude3;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude3() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude3;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude4(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude4) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude4 = CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude4;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude4() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflAmplitude4;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant0(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant0) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant0 = CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant0;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant0() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant0;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant1(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant1) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant1 = CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant1;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant1() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant1;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant2(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant2) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant2 = CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant2;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant2() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant2;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant3(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant3) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant3 = CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant3;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant3() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant3;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant4(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant4) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant4 = CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant4;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant4() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsEddyCompensationZaflTimeConstant4;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECbEddyCompensationValid(final int CsaSeriesMrPhoenixProtocolsGRADSPECbEddyCompensationValid) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECbEddyCompensationValid = CsaSeriesMrPhoenixProtocolsGRADSPECbEddyCompensationValid;
    }

    public int getCsaSeriesMrPhoenixProtocolsGRADSPECbEddyCompensationValid() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECbEddyCompensationValid;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude0(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude0) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude0 = CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude0;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude0() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude0;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude1(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude1) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude1 = CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude1;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude1() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude1;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude2(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude2) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude2 = CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude2;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude2() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflAmplitude2;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant0(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant0) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant0 = CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant0;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant0() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant0;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant1(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant1) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant1 = CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant1;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant1() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant1;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant2(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant2) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant2 = CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant2;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant2() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationXaflTimeConstant2;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude0(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude0) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude0 = CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude0;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude0() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude0;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude1(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude1) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude1 = CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude1;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude1() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude1;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude2(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude2) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude2 = CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude2;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude2() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflAmplitude2;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant0(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant0) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant0 = CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant0;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant0() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant0;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant1(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant1) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant1 = CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant1;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant1() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant1;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant2(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant2) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant2 = CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant2;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant2() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationYaflTimeConstant2;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude0(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude0) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude0 = CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude0;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude0() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude0;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude1(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude1) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude1 = CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude1;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude1() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude1;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude2(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude2) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude2 = CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude2;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude2() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflAmplitude2;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant0(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant0) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant0 = CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant0;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant0() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant0;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant1(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant1) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant1 = CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant1;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant1() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant1;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant2(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant2) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant2 = CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant2;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant2() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsB0CompensationZaflTimeConstant2;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECbB0CompensationValid(final int CsaSeriesMrPhoenixProtocolsGRADSPECbB0CompensationValid) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECbB0CompensationValid = CsaSeriesMrPhoenixProtocolsGRADSPECbB0CompensationValid;
    }

    public int getCsaSeriesMrPhoenixProtocolsGRADSPECbB0CompensationValid() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECbB0CompensationValid;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXYaflAmplitude0(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXYaflAmplitude0) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXYaflAmplitude0 = CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXYaflAmplitude0;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXYaflAmplitude0() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXYaflAmplitude0;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXYaflTimeConstant0(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXYaflTimeConstant0) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXYaflTimeConstant0 = CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXYaflTimeConstant0;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXYaflTimeConstant0() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXYaflTimeConstant0;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXZaflAmplitude0(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXZaflAmplitude0) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXZaflAmplitude0 = CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXZaflAmplitude0;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXZaflAmplitude0() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXZaflAmplitude0;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXZaflTimeConstant0(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXZaflTimeConstant0) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXZaflTimeConstant0 = CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXZaflTimeConstant0;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXZaflTimeConstant0() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationXZaflTimeConstant0;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYXaflAmplitude0(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYXaflAmplitude0) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYXaflAmplitude0 = CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYXaflAmplitude0;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYXaflAmplitude0() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYXaflAmplitude0;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYXaflTimeConstant0(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYXaflTimeConstant0) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYXaflTimeConstant0 = CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYXaflTimeConstant0;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYXaflTimeConstant0() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYXaflTimeConstant0;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYZaflAmplitude0(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYZaflAmplitude0) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYZaflAmplitude0 = CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYZaflAmplitude0;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYZaflAmplitude0() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYZaflAmplitude0;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYZaflTimeConstant0(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYZaflTimeConstant0) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYZaflTimeConstant0 = CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYZaflTimeConstant0;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYZaflTimeConstant0() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationYZaflTimeConstant0;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZXaflAmplitude0(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZXaflAmplitude0) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZXaflAmplitude0 = CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZXaflAmplitude0;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZXaflAmplitude0() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZXaflAmplitude0;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZXaflTimeConstant0(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZXaflTimeConstant0) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZXaflTimeConstant0 = CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZXaflTimeConstant0;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZXaflTimeConstant0() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZXaflTimeConstant0;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZYaflAmplitude0(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZYaflAmplitude0) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZYaflAmplitude0 = CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZYaflAmplitude0;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZYaflAmplitude0() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZYaflAmplitude0;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZYaflTimeConstant0(
            final double CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZYaflTimeConstant0) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZYaflTimeConstant0 = CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZYaflTimeConstant0;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZYaflTimeConstant0() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECsCrossTermCompensationZYaflTimeConstant0;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECbCrossTermCompensationValid(final int CsaSeriesMrPhoenixProtocolsGRADSPECbCrossTermCompensationValid) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECbCrossTermCompensationValid = CsaSeriesMrPhoenixProtocolsGRADSPECbCrossTermCompensationValid;
    }

    public int getCsaSeriesMrPhoenixProtocolsGRADSPECbCrossTermCompensationValid() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECbCrossTermCompensationValid;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPEClOffsetX(final int CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetX) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetX = CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetX;
    }

    public int getCsaSeriesMrPhoenixProtocolsGRADSPEClOffsetX() {
        return CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetX;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPEClOffsetY(final int CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetY) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetY = CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetY;
    }

    public int getCsaSeriesMrPhoenixProtocolsGRADSPEClOffsetY() {
        return CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetY;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPEClOffsetZ(final int CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetZ) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetZ = CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetZ;
    }

    public int getCsaSeriesMrPhoenixProtocolsGRADSPEClOffsetZ() {
        return CsaSeriesMrPhoenixProtocolsGRADSPEClOffsetZ;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECbOffsetValid(final int CsaSeriesMrPhoenixProtocolsGRADSPECbOffsetValid) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECbOffsetValid = CsaSeriesMrPhoenixProtocolsGRADSPECbOffsetValid;
    }

    public int getCsaSeriesMrPhoenixProtocolsGRADSPECbOffsetValid() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECbOffsetValid;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPEClDelayX(final int CsaSeriesMrPhoenixProtocolsGRADSPEClDelayX) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPEClDelayX = CsaSeriesMrPhoenixProtocolsGRADSPEClDelayX;
    }

    public int getCsaSeriesMrPhoenixProtocolsGRADSPEClDelayX() {
        return CsaSeriesMrPhoenixProtocolsGRADSPEClDelayX;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPEClDelayY(final int CsaSeriesMrPhoenixProtocolsGRADSPEClDelayY) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPEClDelayY = CsaSeriesMrPhoenixProtocolsGRADSPEClDelayY;
    }

    public int getCsaSeriesMrPhoenixProtocolsGRADSPEClDelayY() {
        return CsaSeriesMrPhoenixProtocolsGRADSPEClDelayY;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPEClDelayZ(final int CsaSeriesMrPhoenixProtocolsGRADSPEClDelayZ) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPEClDelayZ = CsaSeriesMrPhoenixProtocolsGRADSPEClDelayZ;
    }

    public int getCsaSeriesMrPhoenixProtocolsGRADSPEClDelayZ() {
        return CsaSeriesMrPhoenixProtocolsGRADSPEClDelayZ;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECbDelayValid(final int CsaSeriesMrPhoenixProtocolsGRADSPECbDelayValid) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECbDelayValid = CsaSeriesMrPhoenixProtocolsGRADSPECbDelayValid;
    }

    public int getCsaSeriesMrPhoenixProtocolsGRADSPECbDelayValid() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECbDelayValid;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityX(final double CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityX) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityX = CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityX;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityX() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityX;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityY(final double CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityY) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityY = CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityY;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityY() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityY;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityZ(final double CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityZ) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityZ = CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityZ;
    }

    public double getCsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityZ() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECflSensitivityZ;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECbSensitivityValid(final int CsaSeriesMrPhoenixProtocolsGRADSPECbSensitivityValid) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECbSensitivityValid = CsaSeriesMrPhoenixProtocolsGRADSPECbSensitivityValid;
    }

    public int getCsaSeriesMrPhoenixProtocolsGRADSPECbSensitivityValid() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECbSensitivityValid;
    }
    
    public void setCsaSeriesMrPhoenixProtocolsGRADSPECflGSWDMinRiseTime(double CsaSeriesMrPhoenixProtocolsGRADSPECflGSWDMinRiseTime) {
    	this.CsaSeriesMrPhoenixProtocolsGRADSPECflGSWDMinRiseTime = CsaSeriesMrPhoenixProtocolsGRADSPECflGSWDMinRiseTime;
    }
    
    public double getCsaSeriesMrPhoenixProtocolsGRADSPECflGSWDMinRiseTime() {
    	return CsaSeriesMrPhoenixProtocolsGRADSPECflGSWDMinRiseTime;
    }
    
    public void setCsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent0(int CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent0) {
    	this.CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent0 = CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent0;
    }

    public int getCsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent0() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent0;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent1(final int CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent1) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent1 = CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent1;
    }

    public int getCsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent1() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent1;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent2(final int CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent2) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent2 = CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent2;
    }

    public int getCsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent2() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent2;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent3(final int CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent3) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent3 = CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent3;
    }

    public int getCsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent3() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent3;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent4(final int CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent4) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent4 = CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent4;
    }

    public int getCsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent4() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECalShimCurrent4;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECbShimCurrentValid(final int CsaSeriesMrPhoenixProtocolsGRADSPECbShimCurrentValid) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECbShimCurrentValid = CsaSeriesMrPhoenixProtocolsGRADSPECbShimCurrentValid;
    }

    public int getCsaSeriesMrPhoenixProtocolsGRADSPECbShimCurrentValid() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECbShimCurrentValid;
    }

    public void setCsaSeriesMrPhoenixProtocolsGRADSPECucMode(final int CsaSeriesMrPhoenixProtocolsGRADSPECucMode) {
        this.CsaSeriesMrPhoenixProtocolsGRADSPECucMode = CsaSeriesMrPhoenixProtocolsGRADSPECucMode;
    }

    public int getCsaSeriesMrPhoenixProtocolsGRADSPECucMode() {
        return CsaSeriesMrPhoenixProtocolsGRADSPECucMode;
    }

    public void setCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0tNucleus(final String CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0tNucleus) {
        this.CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0tNucleus = CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0tNucleus;
    }

    public String getCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0tNucleus() {
        return CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0tNucleus;
    }

    public void setCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0lFrequency(final int CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0lFrequency) {
        this.CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0lFrequency = CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0lFrequency;
    }

    public int getCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0lFrequency() {
        return CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0lFrequency;
    }

    public void setCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bFrequencyValid(final int CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bFrequencyValid) {
        this.CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bFrequencyValid = CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bFrequencyValid;
    }

    public int getCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bFrequencyValid() {
        return CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bFrequencyValid;
    }

    public void setCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0flReferenceAmplitude(
            final double CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0flReferenceAmplitude) {
        this.CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0flReferenceAmplitude = CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0flReferenceAmplitude;
    }

    public double getCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0flReferenceAmplitude() {
        return CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0flReferenceAmplitude;
    }

    public void setCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bReferenceAmplitudeValid(
            final int CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bReferenceAmplitudeValid) {
        this.CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bReferenceAmplitudeValid = CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bReferenceAmplitudeValid;
    }

    public int getCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bReferenceAmplitudeValid() {
        return CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bReferenceAmplitudeValid;
    }

    public void setCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0flAmplitudeCorrection(
            final double CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0flAmplitudeCorrection) {
        this.CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0flAmplitudeCorrection = CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0flAmplitudeCorrection;
    }

    public double getCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0flAmplitudeCorrection() {
        return CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0flAmplitudeCorrection;
    }

    public void setCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bAmplitudeCorrectionValid(
            final int CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bAmplitudeCorrectionValid) {
        this.CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bAmplitudeCorrectionValid = CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bAmplitudeCorrectionValid;
    }

    public int getCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bAmplitudeCorrectionValid() {
        return CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bAmplitudeCorrectionValid;
    }

    public void setCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bRFPAIndexValid(final int CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bRFPAIndexValid) {
        this.CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bRFPAIndexValid = CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bRFPAIndexValid;
    }

    public int getCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bRFPAIndexValid() {
        return CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo0bRFPAIndexValid;
    }

    public void setCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bFrequencyValid(final int CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bFrequencyValid) {
        this.CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bFrequencyValid = CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bFrequencyValid;
    }

    public int getCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bFrequencyValid() {
        return CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bFrequencyValid;
    }

    public void setCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bReferenceAmplitudeValid(
            final int CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bReferenceAmplitudeValid) {
        this.CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bReferenceAmplitudeValid = CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bReferenceAmplitudeValid;
    }

    public int getCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bReferenceAmplitudeValid() {
        return CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bReferenceAmplitudeValid;
    }

    public void setCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1flAmplitudeCorrection(
            final double CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1flAmplitudeCorrection) {
        this.CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1flAmplitudeCorrection = CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1flAmplitudeCorrection;
    }

    public double getCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1flAmplitudeCorrection() {
        return CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1flAmplitudeCorrection;
    }

    public void setCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bAmplitudeCorrectionValid(
            final int CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bAmplitudeCorrectionValid) {
        this.CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bAmplitudeCorrectionValid = CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bAmplitudeCorrectionValid;
    }

    public int getCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bAmplitudeCorrectionValid() {
        return CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bAmplitudeCorrectionValid;
    }

    public void setCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1lRFPAIndex(final int CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1lRFPAIndex) {
        this.CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1lRFPAIndex = CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1lRFPAIndex;
    }

    public int getCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1lRFPAIndex() {
        return CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1lRFPAIndex;
    }

    public void setCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bRFPAIndexValid(final int CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bRFPAIndexValid) {
        this.CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bRFPAIndexValid = CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bRFPAIndexValid;
    }

    public int getCsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bRFPAIndexValid() {
        return CsaSeriesMrPhoenixProtocolsTXSPECasNucleusInfo1bRFPAIndexValid;
    }
    
    public void setCsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEntName(
    		String CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEntName[]) {
    	this.CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEntName = CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEntName;

    }

    public String[] getCsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEntName() {
    	return CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEntName;
    }
    
    public void setCsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnbAmplitudeValid(int 
    		CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnbAmplitudeValid[]) {
    	this.CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnbAmplitudeValid = 
    			CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnbAmplitudeValid;
    }

    public int[] getCsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnbAmplitudeValid() {
    	return CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnbAmplitudeValid;
    }
    
    public void setCsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnflAmplitude(double 
    		CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnflAmplitude[]) {
    	this.CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnflAmplitude = 
    			CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnflAmplitude;
    }

    public double[] getCsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnflAmplitude() {
    	return CsaSeriesMrPhoenixProtocolsTXSPECaRFPULSEnflAmplitude;
    }

    public void setCsaSeriesMrPhoenixProtocolsTXSPEClNoOfTraPulses(final int CsaSeriesMrPhoenixProtocolsTXSPEClNoOfTraPulses) {
        this.CsaSeriesMrPhoenixProtocolsTXSPEClNoOfTraPulses = CsaSeriesMrPhoenixProtocolsTXSPEClNoOfTraPulses;
    }

    public int getCsaSeriesMrPhoenixProtocolsTXSPEClNoOfTraPulses() {
        return CsaSeriesMrPhoenixProtocolsTXSPEClNoOfTraPulses;
    }

    public void setCsaSeriesMrPhoenixProtocolsTXSPEClBCExcitationMode(final int CsaSeriesMrPhoenixProtocolsTXSPEClBCExcitationMode) {
        this.CsaSeriesMrPhoenixProtocolsTXSPEClBCExcitationMode = CsaSeriesMrPhoenixProtocolsTXSPEClBCExcitationMode;
    }

    public int getCsaSeriesMrPhoenixProtocolsTXSPEClBCExcitationMode() {
        return CsaSeriesMrPhoenixProtocolsTXSPEClBCExcitationMode;
    }

    public void setCsaSeriesMrPhoenixProtocolsTXSPEClBCSeqExcitationMode(final int CsaSeriesMrPhoenixProtocolsTXSPEClBCSeqExcitationMode) {
        this.CsaSeriesMrPhoenixProtocolsTXSPEClBCSeqExcitationMode = CsaSeriesMrPhoenixProtocolsTXSPEClBCSeqExcitationMode;
    }

    public int getCsaSeriesMrPhoenixProtocolsTXSPEClBCSeqExcitationMode() {
        return CsaSeriesMrPhoenixProtocolsTXSPEClBCSeqExcitationMode;
    }

    public void setCsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeMin(final double CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeMin) {
        this.CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeMin = CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeMin;
    }

    public double getCsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeMin() {
        return CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeMin;
    }

    public void setCsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeMax(final double CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeMax) {
        this.CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeMax = CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeMax;
    }

    public double getCsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeMax() {
        return CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeMax;
    }

    public void setCsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeClipLow(final double CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeClipLow) {
        this.CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeClipLow = CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeClipLow;
    }

    public double getCsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeClipLow() {
        return CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeClipLow;
    }

    public void setCsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeClipHigh(final double CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeClipHigh) {
        this.CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeClipHigh = CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeClipHigh;
    }

    public double getCsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeClipHigh() {
        return CsaSeriesMrPhoenixProtocolsTXSPECflKDynMagnitudeClipHigh;
    }

    public void setCsaSeriesMrPhoenixProtocolsTXSPECflKDynPhaseMax(final double CsaSeriesMrPhoenixProtocolsTXSPECflKDynPhaseMax) {
        this.CsaSeriesMrPhoenixProtocolsTXSPECflKDynPhaseMax = CsaSeriesMrPhoenixProtocolsTXSPECflKDynPhaseMax;
    }

    public double getCsaSeriesMrPhoenixProtocolsTXSPECflKDynPhaseMax() {
        return CsaSeriesMrPhoenixProtocolsTXSPECflKDynPhaseMax;
    }

    public void setCsaSeriesMrPhoenixProtocolsTXSPECflKDynPhaseClip(final double CsaSeriesMrPhoenixProtocolsTXSPECflKDynPhaseClip) {
        this.CsaSeriesMrPhoenixProtocolsTXSPECflKDynPhaseClip = CsaSeriesMrPhoenixProtocolsTXSPECflKDynPhaseClip;
    }

    public double getCsaSeriesMrPhoenixProtocolsTXSPECflKDynPhaseClip() {
        return CsaSeriesMrPhoenixProtocolsTXSPECflKDynPhaseClip;
    }

    public void setCsaSeriesMrPhoenixProtocolsTXSPECbKDynValid(final int CsaSeriesMrPhoenixProtocolsTXSPECbKDynValid) {
        this.CsaSeriesMrPhoenixProtocolsTXSPECbKDynValid = CsaSeriesMrPhoenixProtocolsTXSPECbKDynValid;
    }

    public int getCsaSeriesMrPhoenixProtocolsTXSPECbKDynValid() {
        return CsaSeriesMrPhoenixProtocolsTXSPECbKDynValid;
    }

    public void setCsaSeriesMrPhoenixProtocolsTXSPECucRFPulseType(final int CsaSeriesMrPhoenixProtocolsTXSPECucRFPulseType) {
        this.CsaSeriesMrPhoenixProtocolsTXSPECucRFPulseType = CsaSeriesMrPhoenixProtocolsTXSPECucRFPulseType;
    }

    public int getCsaSeriesMrPhoenixProtocolsTXSPECucRFPulseType() {
        return CsaSeriesMrPhoenixProtocolsTXSPECucRFPulseType;
    }

    public void setCsaSeriesMrPhoenixProtocolsTXSPECucExcitMode(final int CsaSeriesMrPhoenixProtocolsTXSPECucExcitMode) {
        this.CsaSeriesMrPhoenixProtocolsTXSPECucExcitMode = CsaSeriesMrPhoenixProtocolsTXSPECucExcitMode;
    }

    public int getCsaSeriesMrPhoenixProtocolsTXSPECucExcitMode() {
        return CsaSeriesMrPhoenixProtocolsTXSPECucExcitMode;
    }

    public void setCsaSeriesMrPhoenixProtocolsTXSPECucSimultaneousExcitation(final int CsaSeriesMrPhoenixProtocolsTXSPECucSimultaneousExcitation) {
        this.CsaSeriesMrPhoenixProtocolsTXSPECucSimultaneousExcitation = CsaSeriesMrPhoenixProtocolsTXSPECucSimultaneousExcitation;
    }

    public int getCsaSeriesMrPhoenixProtocolsTXSPECucSimultaneousExcitation() {
        return CsaSeriesMrPhoenixProtocolsTXSPECucSimultaneousExcitation;
    }

    public void setCsaSeriesMrPhoenixProtocolsTXSPECucBCExcitationModeValid(final int CsaSeriesMrPhoenixProtocolsTXSPECucBCExcitationModeValid) {
        this.CsaSeriesMrPhoenixProtocolsTXSPECucBCExcitationModeValid = CsaSeriesMrPhoenixProtocolsTXSPECucBCExcitationModeValid;
    }

    public int getCsaSeriesMrPhoenixProtocolsTXSPECucBCExcitationModeValid() {
        return CsaSeriesMrPhoenixProtocolsTXSPECucBCExcitationModeValid;
    }

    public void setCsaSeriesMrPhoenixProtocolsRXSPEClGain(final int CsaSeriesMrPhoenixProtocolsRXSPEClGain) {
        this.CsaSeriesMrPhoenixProtocolsRXSPEClGain = CsaSeriesMrPhoenixProtocolsRXSPEClGain;
    }

    public int getCsaSeriesMrPhoenixProtocolsRXSPEClGain() {
        return CsaSeriesMrPhoenixProtocolsRXSPEClGain;
    }

    public void setCsaSeriesMrPhoenixProtocolsRXSPECbGainValid(final int CsaSeriesMrPhoenixProtocolsRXSPECbGainValid) {
        this.CsaSeriesMrPhoenixProtocolsRXSPECbGainValid = CsaSeriesMrPhoenixProtocolsRXSPECbGainValid;
    }

    public int getCsaSeriesMrPhoenixProtocolsRXSPECbGainValid() {
        return CsaSeriesMrPhoenixProtocolsRXSPECbGainValid;
    }

    public void setCsaSeriesMrPhoenixProtocolsRXSPECalDwellTime0(final int CsaSeriesMrPhoenixProtocolsRXSPECalDwellTime0) {
        this.CsaSeriesMrPhoenixProtocolsRXSPECalDwellTime0 = CsaSeriesMrPhoenixProtocolsRXSPECalDwellTime0;
    }

    public int getCsaSeriesMrPhoenixProtocolsRXSPECalDwellTime0() {
        return CsaSeriesMrPhoenixProtocolsRXSPECalDwellTime0;
    }

    public void setCsaSeriesMrPhoenixProtocolsAdjDatauiAdjFreMode(final int CsaSeriesMrPhoenixProtocolsAdjDatauiAdjFreMode) {
        this.CsaSeriesMrPhoenixProtocolsAdjDatauiAdjFreMode = CsaSeriesMrPhoenixProtocolsAdjDatauiAdjFreMode;
    }

    public int getCsaSeriesMrPhoenixProtocolsAdjDatauiAdjFreMode() {
        return CsaSeriesMrPhoenixProtocolsAdjDatauiAdjFreMode;
    }

    public void setCsaSeriesMrPhoenixProtocolsAdjDatauiAdjShimMode(final int CsaSeriesMrPhoenixProtocolsAdjDatauiAdjShimMode) {
        this.CsaSeriesMrPhoenixProtocolsAdjDatauiAdjShimMode = CsaSeriesMrPhoenixProtocolsAdjDatauiAdjShimMode;
    }

    public int getCsaSeriesMrPhoenixProtocolsAdjDatauiAdjShimMode() {
        return CsaSeriesMrPhoenixProtocolsAdjDatauiAdjShimMode;
    }

    public void setCsaSeriesMrPhoenixProtocolsAdjDatauiAdjWatSupMode(final int CsaSeriesMrPhoenixProtocolsAdjDatauiAdjWatSupMode) {
        this.CsaSeriesMrPhoenixProtocolsAdjDatauiAdjWatSupMode = CsaSeriesMrPhoenixProtocolsAdjDatauiAdjWatSupMode;
    }

    public int getCsaSeriesMrPhoenixProtocolsAdjDatauiAdjWatSupMode() {
        return CsaSeriesMrPhoenixProtocolsAdjDatauiAdjWatSupMode;
    }

    public void setCsaSeriesMrPhoenixProtocolsAdjDatauiAdjRFMapMode(final int CsaSeriesMrPhoenixProtocolsAdjDatauiAdjRFMapMode) {
        this.CsaSeriesMrPhoenixProtocolsAdjDatauiAdjRFMapMode = CsaSeriesMrPhoenixProtocolsAdjDatauiAdjRFMapMode;
    }

    public int getCsaSeriesMrPhoenixProtocolsAdjDatauiAdjRFMapMode() {
        return CsaSeriesMrPhoenixProtocolsAdjDatauiAdjRFMapMode;
    }

    public void setCsaSeriesMrPhoenixProtocolsAdjDatauiAdjMDSMode(final int CsaSeriesMrPhoenixProtocolsAdjDatauiAdjMDSMode) {
        this.CsaSeriesMrPhoenixProtocolsAdjDatauiAdjMDSMode = CsaSeriesMrPhoenixProtocolsAdjDatauiAdjMDSMode;
    }

    public int getCsaSeriesMrPhoenixProtocolsAdjDatauiAdjMDSMode() {
        return CsaSeriesMrPhoenixProtocolsAdjDatauiAdjMDSMode;
    }

    public void setCsaSeriesMrPhoenixProtocolsAdjDatauiAdjTableTolerance(final int CsaSeriesMrPhoenixProtocolsAdjDatauiAdjTableTolerance) {
        this.CsaSeriesMrPhoenixProtocolsAdjDatauiAdjTableTolerance = CsaSeriesMrPhoenixProtocolsAdjDatauiAdjTableTolerance;
    }

    public int getCsaSeriesMrPhoenixProtocolsAdjDatauiAdjTableTolerance() {
        return CsaSeriesMrPhoenixProtocolsAdjDatauiAdjTableTolerance;
    }

    public void setCsaSeriesMrPhoenixProtocolsAdjDatauiAdjProtID(final int CsaSeriesMrPhoenixProtocolsAdjDatauiAdjProtID) {
        this.CsaSeriesMrPhoenixProtocolsAdjDatauiAdjProtID = CsaSeriesMrPhoenixProtocolsAdjDatauiAdjProtID;
    }

    public int getCsaSeriesMrPhoenixProtocolsAdjDatauiAdjProtID() {
        return CsaSeriesMrPhoenixProtocolsAdjDatauiAdjProtID;
    }

    public void setCsaSeriesMrPhoenixProtocolsAdjDatauiAdjFreProtRelated(final int CsaSeriesMrPhoenixProtocolsAdjDatauiAdjFreProtRelated) {
        this.CsaSeriesMrPhoenixProtocolsAdjDatauiAdjFreProtRelated = CsaSeriesMrPhoenixProtocolsAdjDatauiAdjFreProtRelated;
    }

    public int getCsaSeriesMrPhoenixProtocolsAdjDatauiAdjFreProtRelated() {
        return CsaSeriesMrPhoenixProtocolsAdjDatauiAdjFreProtRelated;
    }

    public void setCsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondSag(final double CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondSag) {
        this.CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondSag = CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondSag;
    }

    public double getCsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondSag() {
        return CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondSag;
    }

    public void setCsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondCor(final double CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondCor) {
        this.CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondCor = CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondCor;
    }

    public double getCsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondCor() {
        return CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondCor;
    }

    public void setCsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondTra(final double CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondTra) {
        this.CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondTra = CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondTra;
    }

    public double getCsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondTra() {
        return CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesPositiondTra;
    }

    public void setCsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldSag(final double CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldSag) {
        this.CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldSag = CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldSag;
    }

    public double getCsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldSag() {
        return CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldSag;
    }

    public void setCsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldCor(final double CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldCor) {
        this.CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldCor = CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldCor;
    }

    public double getCsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldCor() {
        return CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldCor;
    }

    public void setCsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldTra(final double CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldTra) {
        this.CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldTra = CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldTra;
    }

    public double getCsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldTra() {
        return CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumesNormaldTra;
    }

    public void setCsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedThickness(final double CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedThickness) {
        this.CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedThickness = CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedThickness;
    }

    public double getCsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedThickness() {
        return CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedThickness;
    }

    public void setCsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedPhaseFOV(final double CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedPhaseFOV) {
        this.CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedPhaseFOV = CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedPhaseFOV;
    }

    public double getCsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedPhaseFOV() {
        return CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedPhaseFOV;
    }

    public void setCsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedReadoutFOV(final double CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedReadoutFOV) {
        this.CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedReadoutFOV = CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedReadoutFOV;
    }

    public double getCsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedReadoutFOV() {
        return CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedReadoutFOV;
    }

    public void setCsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedInPlaneRot(final double CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedInPlaneRot) {
        this.CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedInPlaneRot = CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedInPlaneRot;
    }

    public double getCsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedInPlaneRot() {
        return CsaSeriesMrPhoenixProtocolsAdjDatasAdjVolumedInPlaneRot;
    }

    public void setCsaSeriesMrPhoenixProtocolsAdjDatauiAdjVolumeValid(final int CsaSeriesMrPhoenixProtocolsAdjDatauiAdjVolumeValid) {
        this.CsaSeriesMrPhoenixProtocolsAdjDatauiAdjVolumeValid = CsaSeriesMrPhoenixProtocolsAdjDatauiAdjVolumeValid;
    }

    public int getCsaSeriesMrPhoenixProtocolsAdjDatauiAdjVolumeValid() {
        return CsaSeriesMrPhoenixProtocolsAdjDatauiAdjVolumeValid;
    }

    public void setCsaSeriesMrPhoenixProtocolucEnableNoiseAdjust(final int CsaSeriesMrPhoenixProtocolucEnableNoiseAdjust) {
        this.CsaSeriesMrPhoenixProtocolucEnableNoiseAdjust = CsaSeriesMrPhoenixProtocolucEnableNoiseAdjust;
    }

    public int getCsaSeriesMrPhoenixProtocolucEnableNoiseAdjust() {
        return CsaSeriesMrPhoenixProtocolucEnableNoiseAdjust;
    }

    public void setCsaSeriesMrPhoenixProtocolalTR0(final int CsaSeriesMrPhoenixProtocolalTR0) {
        this.CsaSeriesMrPhoenixProtocolalTR0 = CsaSeriesMrPhoenixProtocolalTR0;
    }

    public int getCsaSeriesMrPhoenixProtocolalTR0() {
        return CsaSeriesMrPhoenixProtocolalTR0;
    }

    public void setCsaSeriesMrPhoenixProtocollContrasts(final int CsaSeriesMrPhoenixProtocollContrasts) {
        this.CsaSeriesMrPhoenixProtocollContrasts = CsaSeriesMrPhoenixProtocollContrasts;
    }

    public int getCsaSeriesMrPhoenixProtocollContrasts() {
        return CsaSeriesMrPhoenixProtocollContrasts;
    }

    public void setCsaSeriesMrPhoenixProtocolalTE0(final int CsaSeriesMrPhoenixProtocolalTE0) {
        this.CsaSeriesMrPhoenixProtocolalTE0 = CsaSeriesMrPhoenixProtocolalTE0;
    }

    public int getCsaSeriesMrPhoenixProtocolalTE0() {
        return CsaSeriesMrPhoenixProtocolalTE0;
    }

    public void setCsaSeriesMrPhoenixProtocolacFlowComp0(final int CsaSeriesMrPhoenixProtocolacFlowComp0) {
        this.CsaSeriesMrPhoenixProtocolacFlowComp0 = CsaSeriesMrPhoenixProtocolacFlowComp0;
    }

    public int getCsaSeriesMrPhoenixProtocolacFlowComp0() {
        return CsaSeriesMrPhoenixProtocolacFlowComp0;
    }

    public void setCsaSeriesMrPhoenixProtocollCombinedEchoes(final int CsaSeriesMrPhoenixProtocollCombinedEchoes) {
        this.CsaSeriesMrPhoenixProtocollCombinedEchoes = CsaSeriesMrPhoenixProtocollCombinedEchoes;
    }

    public int getCsaSeriesMrPhoenixProtocollCombinedEchoes() {
        return CsaSeriesMrPhoenixProtocollCombinedEchoes;
    }

    public void setCsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondSag(final double CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondSag[]) {
        this.CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondSag = CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondSag;
    }

    public double[] getCsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondSag() {
        return CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondSag;
    }

    public void setCsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondCor(final double CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondCor[]) {
        this.CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondCor = CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondCor;
    }

    public double[] getCsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondCor() {
        return CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondCor;
    }

    public void setCsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondTra(final double CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondTra[]) {
        this.CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondTra = CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondTra;
    }

    public double[] getCsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondTra() {
        return CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsPositiondTra;
    }

    public void setCsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldSag(final double CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldSag[]) {
        this.CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldSag = CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldSag;
    }

    public double[] getCsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldSag() {
        return CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldSag;
    }

    public void setCsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldCor(final double CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldCor[]) {
        this.CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldCor = CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldCor;
    }

    public double[] getCsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldCor() {
        return CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldCor;
    }

    public void setCsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldTra(final double CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldTra[]) {
        this.CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldTra = CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldTra;
    }

    public double[] getCsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldTra() {
        return CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezsNormaldTra;
    }

    public void setCsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdThickness(final double CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdThickness[]) {
        this.CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdThickness = CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdThickness;
    }

    public double[] getCsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdThickness() {
        return CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdThickness;
    }

    public void setCsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdPhaseFOV(final double CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdPhaseFOV[]) {
        this.CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdPhaseFOV = CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdPhaseFOV;
    }

    public double[] getCsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdPhaseFOV() {
        return CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdPhaseFOV;
    }

    public void setCsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdReadoutFOV(final double CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdReadoutFOV[]) {
        this.CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdReadoutFOV = CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdReadoutFOV;
    }

    public double[] getCsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdReadoutFOV() {
        return CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdReadoutFOV;
    }

    public void setCsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdInPlaneRot(final double CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdInPlaneRot[]) {
        this.CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdInPlaneRot = CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdInPlaneRot;
    }

    public double[] getCsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdInPlaneRot() {
        return CsaSeriesMrPhoenixProtocolsSliceArrayasSlicezdInPlaneRot;
    }

    public void setCsaSeriesMrPhoenixProtocolsSliceArrayanAsc(final int CsaSeriesMrPhoenixProtocolsSliceArrayanAsc[]) {
        this.CsaSeriesMrPhoenixProtocolsSliceArrayanAsc = CsaSeriesMrPhoenixProtocolsSliceArrayanAsc;
    }

    public int[] getCsaSeriesMrPhoenixProtocolsSliceArrayanAsc() {
        return CsaSeriesMrPhoenixProtocolsSliceArrayanAsc;
    }

    public void setCsaSeriesMrPhoenixProtocolsSliceArrayanPos(final int CsaSeriesMrPhoenixProtocolsSliceArrayanPos[]) {
        this.CsaSeriesMrPhoenixProtocolsSliceArrayanPos = CsaSeriesMrPhoenixProtocolsSliceArrayanPos;
    }

    public int[] getCsaSeriesMrPhoenixProtocolsSliceArrayanPos() {
        return CsaSeriesMrPhoenixProtocolsSliceArrayanPos;
    }

    public void setCsaSeriesMrPhoenixProtocolsSliceArraylSize(final int CsaSeriesMrPhoenixProtocolsSliceArraylSize) {
        this.CsaSeriesMrPhoenixProtocolsSliceArraylSize = CsaSeriesMrPhoenixProtocolsSliceArraylSize;
    }

    public int getCsaSeriesMrPhoenixProtocolsSliceArraylSize() {
        return CsaSeriesMrPhoenixProtocolsSliceArraylSize;
    }

    public void setCsaSeriesMrPhoenixProtocolsSliceArraylConc(final int CsaSeriesMrPhoenixProtocolsSliceArraylConc) {
        this.CsaSeriesMrPhoenixProtocolsSliceArraylConc = CsaSeriesMrPhoenixProtocolsSliceArraylConc;
    }

    public int getCsaSeriesMrPhoenixProtocolsSliceArraylConc() {
        return CsaSeriesMrPhoenixProtocolsSliceArraylConc;
    }

    public void setCsaSeriesMrPhoenixProtocolsSliceArrayucMode(final int CsaSeriesMrPhoenixProtocolsSliceArrayucMode) {
        this.CsaSeriesMrPhoenixProtocolsSliceArrayucMode = CsaSeriesMrPhoenixProtocolsSliceArrayucMode;
    }

    public int getCsaSeriesMrPhoenixProtocolsSliceArrayucMode() {
        return CsaSeriesMrPhoenixProtocolsSliceArrayucMode;
    }

    public void setCsaSeriesMrPhoenixProtocolsSliceArraysTSatdThickness(final double CsaSeriesMrPhoenixProtocolsSliceArraysTSatdThickness) {
        this.CsaSeriesMrPhoenixProtocolsSliceArraysTSatdThickness = CsaSeriesMrPhoenixProtocolsSliceArraysTSatdThickness;
    }

    public double getCsaSeriesMrPhoenixProtocolsSliceArraysTSatdThickness() {
        return CsaSeriesMrPhoenixProtocolsSliceArraysTSatdThickness;
    }

    public void setCsaSeriesMrPhoenixProtocolsGroupArrayasGroup0nSize(final int CsaSeriesMrPhoenixProtocolsGroupArrayasGroup0nSize) {
        this.CsaSeriesMrPhoenixProtocolsGroupArrayasGroup0nSize = CsaSeriesMrPhoenixProtocolsGroupArrayasGroup0nSize;
    }

    public int getCsaSeriesMrPhoenixProtocolsGroupArrayasGroup0nSize() {
        return CsaSeriesMrPhoenixProtocolsGroupArrayasGroup0nSize;
    }

    public void setCsaSeriesMrPhoenixProtocolsGroupArrayanMember(final int CsaSeriesMrPhoenixProtocolsGroupArrayanMember[]) {
        this.CsaSeriesMrPhoenixProtocolsGroupArrayanMember = CsaSeriesMrPhoenixProtocolsGroupArrayanMember;
    }

    public int[] getCsaSeriesMrPhoenixProtocolsGroupArrayanMember() {
        return CsaSeriesMrPhoenixProtocolsGroupArrayanMember;
    }

    public void setCsaSeriesMrPhoenixProtocolsGroupArraylSize(final int CsaSeriesMrPhoenixProtocolsGroupArraylSize) {
        this.CsaSeriesMrPhoenixProtocolsGroupArraylSize = CsaSeriesMrPhoenixProtocolsGroupArraylSize;
    }

    public int getCsaSeriesMrPhoenixProtocolsGroupArraylSize() {
        return CsaSeriesMrPhoenixProtocolsGroupArraylSize;
    }

    public void setCsaSeriesMrPhoenixProtocolsGroupArraysPSatdThickness(final double CsaSeriesMrPhoenixProtocolsGroupArraysPSatdThickness) {
        this.CsaSeriesMrPhoenixProtocolsGroupArraysPSatdThickness = CsaSeriesMrPhoenixProtocolsGroupArraysPSatdThickness;
    }

    public double getCsaSeriesMrPhoenixProtocolsGroupArraysPSatdThickness() {
        return CsaSeriesMrPhoenixProtocolsGroupArraysPSatdThickness;
    }

    public void setCsaSeriesMrPhoenixProtocolsGroupArraysPSatdGap(final double CsaSeriesMrPhoenixProtocolsGroupArraysPSatdGap) {
        this.CsaSeriesMrPhoenixProtocolsGroupArraysPSatdGap = CsaSeriesMrPhoenixProtocolsGroupArraysPSatdGap;
    }

    public double getCsaSeriesMrPhoenixProtocolsGroupArraysPSatdGap() {
        return CsaSeriesMrPhoenixProtocolsGroupArraysPSatdGap;
    }
    
    public void setCsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondSag(double CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondSag) {
    	this.CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondSag = CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondSag;
    }
    
    public double getCsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondSag() {
    	return CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondSag;
    }
    
    public void setCsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondCor(double CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondCor) {
    	this.CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondCor = CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondCor;
    }
    
    public double getCsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondCor() {
    	return CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondCor;
    }
    
    public void setCsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondTra(double CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondTra) {
    	this.CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondTra = CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondTra;
    }
    
    public double getCsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondTra() {
    	return CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sPositiondTra;
    }
    
    public void setCsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldSag(double CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldSag) {
    	this.CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldSag = CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldSag;
    }
    
    public double getCsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldSag() {
    	return CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldSag;
    }
    
    public void setCsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldCor(double CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldCor) {
    	this.CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldCor = CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldCor;
    }
    
    public double getCsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldCor() {
    	return CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldCor;
    }
    
    public void setCsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldTra(double CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldTra) {
    	this.CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldTra = CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldTra;
    }
    
    public double getCsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldTra() {
    	return CsaSeriesMrPhoenixProtocolsRSatArrayasElm0sNormaldTra;
    }
    
    public void setCsaSeriesMrPhoenixProtocolsRSatArrayasElm0dThickness(double CsaSeriesMrPhoenixProtocolsRSatArrayasElm0dThickness) {
    	this.CsaSeriesMrPhoenixProtocolsRSatArrayasElm0dThickness = CsaSeriesMrPhoenixProtocolsRSatArrayasElm0dThickness;
    }
    
    public double getCsaSeriesMrPhoenixProtocolsRSatArrayasElm0dThickness() {
    	return CsaSeriesMrPhoenixProtocolsRSatArrayasElm0dThickness;
    }
    
    public void setCsaSeriesMrPhoenixProtocolsRSatArraylSize(int CsaSeriesMrPhoenixProtocolsRSatArraylSize) {
    	this.CsaSeriesMrPhoenixProtocolsRSatArraylSize = CsaSeriesMrPhoenixProtocolsRSatArraylSize;
    }
    
    public int getCsaSeriesMrPhoenixProtocolsRSatArraylSize() {
    	return CsaSeriesMrPhoenixProtocolsRSatArraylSize;
    }
    
    public void setCsaSeriesMrPhoenixProtocolsAutoAligndAAMatrix(double 
    		CsaSeriesMrPhoenixProtocolsAutoAligndAAMatrix[]) {
    	this.CsaSeriesMrPhoenixProtocolsAutoAligndAAMatrix = CsaSeriesMrPhoenixProtocolsAutoAligndAAMatrix;
    }

    public double[] getCsaSeriesMrPhoenixProtocolsAutoAligndAAMatrix() {
        return CsaSeriesMrPhoenixProtocolsAutoAligndAAMatrix;
    }

    public void setCsaSeriesMrPhoenixProtocolsNavigatorParalBreathHoldMeas(final int CsaSeriesMrPhoenixProtocolsNavigatorParalBreathHoldMeas) {
        this.CsaSeriesMrPhoenixProtocolsNavigatorParalBreathHoldMeas = CsaSeriesMrPhoenixProtocolsNavigatorParalBreathHoldMeas;
    }

    public int getCsaSeriesMrPhoenixProtocolsNavigatorParalBreathHoldMeas() {
        return CsaSeriesMrPhoenixProtocolsNavigatorParalBreathHoldMeas;
    }

    public void setCsaSeriesMrPhoenixProtocolsNavigatorParalRespComp(final int CsaSeriesMrPhoenixProtocolsNavigatorParalRespComp) {
        this.CsaSeriesMrPhoenixProtocolsNavigatorParalRespComp = CsaSeriesMrPhoenixProtocolsNavigatorParalRespComp;
    }

    public int getCsaSeriesMrPhoenixProtocolsNavigatorParalRespComp() {
        return CsaSeriesMrPhoenixProtocolsNavigatorParalRespComp;
    }

    public void setCsaSeriesMrPhoenixProtocolsBladeParadBladeCoverage(final double CsaSeriesMrPhoenixProtocolsBladeParadBladeCoverage) {
        this.CsaSeriesMrPhoenixProtocolsBladeParadBladeCoverage = CsaSeriesMrPhoenixProtocolsBladeParadBladeCoverage;
    }

    public double getCsaSeriesMrPhoenixProtocolsBladeParadBladeCoverage() {
        return CsaSeriesMrPhoenixProtocolsBladeParadBladeCoverage;
    }

    public void setCsaSeriesMrPhoenixProtocolsBladeParaucMotionCorr(final int CsaSeriesMrPhoenixProtocolsBladeParaucMotionCorr) {
        this.CsaSeriesMrPhoenixProtocolsBladeParaucMotionCorr = CsaSeriesMrPhoenixProtocolsBladeParaucMotionCorr;
    }

    public int getCsaSeriesMrPhoenixProtocolsBladeParaucMotionCorr() {
        return CsaSeriesMrPhoenixProtocolsBladeParaucMotionCorr;
    }

    public void setCsaSeriesMrPhoenixProtocolsPrepPulsesucFatSat(final int CsaSeriesMrPhoenixProtocolsPrepPulsesucFatSat) {
        this.CsaSeriesMrPhoenixProtocolsPrepPulsesucFatSat = CsaSeriesMrPhoenixProtocolsPrepPulsesucFatSat;
    }

    public int getCsaSeriesMrPhoenixProtocolsPrepPulsesucFatSat() {
        return CsaSeriesMrPhoenixProtocolsPrepPulsesucFatSat;
    }

    public void setCsaSeriesMrPhoenixProtocolsPrepPulsesucWaterSat(final int CsaSeriesMrPhoenixProtocolsPrepPulsesucWaterSat) {
        this.CsaSeriesMrPhoenixProtocolsPrepPulsesucWaterSat = CsaSeriesMrPhoenixProtocolsPrepPulsesucWaterSat;
    }

    public int getCsaSeriesMrPhoenixProtocolsPrepPulsesucWaterSat() {
        return CsaSeriesMrPhoenixProtocolsPrepPulsesucWaterSat;
    }

    public void setCsaSeriesMrPhoenixProtocolsPrepPulsesucInversion(final int CsaSeriesMrPhoenixProtocolsPrepPulsesucInversion) {
        this.CsaSeriesMrPhoenixProtocolsPrepPulsesucInversion = CsaSeriesMrPhoenixProtocolsPrepPulsesucInversion;
    }

    public int getCsaSeriesMrPhoenixProtocolsPrepPulsesucInversion() {
        return CsaSeriesMrPhoenixProtocolsPrepPulsesucInversion;
    }

    public void setCsaSeriesMrPhoenixProtocolsPrepPulsesucSatRecovery(final int CsaSeriesMrPhoenixProtocolsPrepPulsesucSatRecovery) {
        this.CsaSeriesMrPhoenixProtocolsPrepPulsesucSatRecovery = CsaSeriesMrPhoenixProtocolsPrepPulsesucSatRecovery;
    }

    public int getCsaSeriesMrPhoenixProtocolsPrepPulsesucSatRecovery() {
        return CsaSeriesMrPhoenixProtocolsPrepPulsesucSatRecovery;
    }

    public void setCsaSeriesMrPhoenixProtocolsPrepPulsesucT2Prep(final int CsaSeriesMrPhoenixProtocolsPrepPulsesucT2Prep) {
        this.CsaSeriesMrPhoenixProtocolsPrepPulsesucT2Prep = CsaSeriesMrPhoenixProtocolsPrepPulsesucT2Prep;
    }

    public int getCsaSeriesMrPhoenixProtocolsPrepPulsesucT2Prep() {
        return CsaSeriesMrPhoenixProtocolsPrepPulsesucT2Prep;
    }

    public void setCsaSeriesMrPhoenixProtocolsPrepPulsesucTIScout(final int CsaSeriesMrPhoenixProtocolsPrepPulsesucTIScout) {
        this.CsaSeriesMrPhoenixProtocolsPrepPulsesucTIScout = CsaSeriesMrPhoenixProtocolsPrepPulsesucTIScout;
    }

    public int getCsaSeriesMrPhoenixProtocolsPrepPulsesucTIScout() {
        return CsaSeriesMrPhoenixProtocolsPrepPulsesucTIScout;
    }

    public void setCsaSeriesMrPhoenixProtocolsPrepPulsesucFatSatMode(final int CsaSeriesMrPhoenixProtocolsPrepPulsesucFatSatMode) {
        this.CsaSeriesMrPhoenixProtocolsPrepPulsesucFatSatMode = CsaSeriesMrPhoenixProtocolsPrepPulsesucFatSatMode;
    }

    public int getCsaSeriesMrPhoenixProtocolsPrepPulsesucFatSatMode() {
        return CsaSeriesMrPhoenixProtocolsPrepPulsesucFatSatMode;
    }

    public void setCsaSeriesMrPhoenixProtocolsPrepPulsesdDarkBloodThickness(final double CsaSeriesMrPhoenixProtocolsPrepPulsesdDarkBloodThickness) {
        this.CsaSeriesMrPhoenixProtocolsPrepPulsesdDarkBloodThickness = CsaSeriesMrPhoenixProtocolsPrepPulsesdDarkBloodThickness;
    }

    public double getCsaSeriesMrPhoenixProtocolsPrepPulsesdDarkBloodThickness() {
        return CsaSeriesMrPhoenixProtocolsPrepPulsesdDarkBloodThickness;
    }

    public void setCsaSeriesMrPhoenixProtocolsPrepPulsesdDarkBloodFlipAngle(final double CsaSeriesMrPhoenixProtocolsPrepPulsesdDarkBloodFlipAngle) {
        this.CsaSeriesMrPhoenixProtocolsPrepPulsesdDarkBloodFlipAngle = CsaSeriesMrPhoenixProtocolsPrepPulsesdDarkBloodFlipAngle;
    }

    public double getCsaSeriesMrPhoenixProtocolsPrepPulsesdDarkBloodFlipAngle() {
        return CsaSeriesMrPhoenixProtocolsPrepPulsesdDarkBloodFlipAngle;
    }

    public void setCsaSeriesMrPhoenixProtocolsPrepPulsesdT2PrepDuration(final double CsaSeriesMrPhoenixProtocolsPrepPulsesdT2PrepDuration) {
        this.CsaSeriesMrPhoenixProtocolsPrepPulsesdT2PrepDuration = CsaSeriesMrPhoenixProtocolsPrepPulsesdT2PrepDuration;
    }

    public double getCsaSeriesMrPhoenixProtocolsPrepPulsesdT2PrepDuration() {
        return CsaSeriesMrPhoenixProtocolsPrepPulsesdT2PrepDuration;
    }

    public void setCsaSeriesMrPhoenixProtocolsPrepPulsesdIRPulseThicknessFactor(final double CsaSeriesMrPhoenixProtocolsPrepPulsesdIRPulseThicknessFactor) {
        this.CsaSeriesMrPhoenixProtocolsPrepPulsesdIRPulseThicknessFactor = CsaSeriesMrPhoenixProtocolsPrepPulsesdIRPulseThicknessFactor;
    }

    public double getCsaSeriesMrPhoenixProtocolsPrepPulsesdIRPulseThicknessFactor() {
        return CsaSeriesMrPhoenixProtocolsPrepPulsesdIRPulseThicknessFactor;
    }

    public void setCsaSeriesMrPhoenixProtocolsKSpacedPhaseResolution(final double CsaSeriesMrPhoenixProtocolsKSpacedPhaseResolution) {
        this.CsaSeriesMrPhoenixProtocolsKSpacedPhaseResolution = CsaSeriesMrPhoenixProtocolsKSpacedPhaseResolution;
    }

    public double getCsaSeriesMrPhoenixProtocolsKSpacedPhaseResolution() {
        return CsaSeriesMrPhoenixProtocolsKSpacedPhaseResolution;
    }

    public void setCsaSeriesMrPhoenixProtocolsKSpacedSliceResolution(final double CsaSeriesMrPhoenixProtocolsKSpacedSliceResolution) {
        this.CsaSeriesMrPhoenixProtocolsKSpacedSliceResolution = CsaSeriesMrPhoenixProtocolsKSpacedSliceResolution;
    }

    public double getCsaSeriesMrPhoenixProtocolsKSpacedSliceResolution() {
        return CsaSeriesMrPhoenixProtocolsKSpacedSliceResolution;
    }

    public void setCsaSeriesMrPhoenixProtocolsKSpacedAngioDynCentralRegionA(final double CsaSeriesMrPhoenixProtocolsKSpacedAngioDynCentralRegionA) {
        this.CsaSeriesMrPhoenixProtocolsKSpacedAngioDynCentralRegionA = CsaSeriesMrPhoenixProtocolsKSpacedAngioDynCentralRegionA;
    }

    public double getCsaSeriesMrPhoenixProtocolsKSpacedAngioDynCentralRegionA() {
        return CsaSeriesMrPhoenixProtocolsKSpacedAngioDynCentralRegionA;
    }

    public void setCsaSeriesMrPhoenixProtocolsKSpacedAngioDynSamplingDensityB(final double CsaSeriesMrPhoenixProtocolsKSpacedAngioDynSamplingDensityB) {
        this.CsaSeriesMrPhoenixProtocolsKSpacedAngioDynSamplingDensityB = CsaSeriesMrPhoenixProtocolsKSpacedAngioDynSamplingDensityB;
    }

    public double getCsaSeriesMrPhoenixProtocolsKSpacedAngioDynSamplingDensityB() {
        return CsaSeriesMrPhoenixProtocolsKSpacedAngioDynSamplingDensityB;
    }

    public void setCsaSeriesMrPhoenixProtocolsKSpacelBaseResolution(final int CsaSeriesMrPhoenixProtocolsKSpacelBaseResolution) {
        this.CsaSeriesMrPhoenixProtocolsKSpacelBaseResolution = CsaSeriesMrPhoenixProtocolsKSpacelBaseResolution;
    }

    public int getCsaSeriesMrPhoenixProtocolsKSpacelBaseResolution() {
        return CsaSeriesMrPhoenixProtocolsKSpacelBaseResolution;
    }

    public void setCsaSeriesMrPhoenixProtocolsKSpacelPhaseEncodingLines(final int CsaSeriesMrPhoenixProtocolsKSpacelPhaseEncodingLines) {
        this.CsaSeriesMrPhoenixProtocolsKSpacelPhaseEncodingLines = CsaSeriesMrPhoenixProtocolsKSpacelPhaseEncodingLines;
    }

    public int getCsaSeriesMrPhoenixProtocolsKSpacelPhaseEncodingLines() {
        return CsaSeriesMrPhoenixProtocolsKSpacelPhaseEncodingLines;
    }

    public void setCsaSeriesMrPhoenixProtocolsKSpacelPartitions(final int CsaSeriesMrPhoenixProtocolsKSpacelPartitions) {
        this.CsaSeriesMrPhoenixProtocolsKSpacelPartitions = CsaSeriesMrPhoenixProtocolsKSpacelPartitions;
    }

    public int getCsaSeriesMrPhoenixProtocolsKSpacelPartitions() {
        return CsaSeriesMrPhoenixProtocolsKSpacelPartitions;
    }

    public void setCsaSeriesMrPhoenixProtocolsKSpacelImagesPerSlab(final int CsaSeriesMrPhoenixProtocolsKSpacelImagesPerSlab) {
        this.CsaSeriesMrPhoenixProtocolsKSpacelImagesPerSlab = CsaSeriesMrPhoenixProtocolsKSpacelImagesPerSlab;
    }

    public int getCsaSeriesMrPhoenixProtocolsKSpacelImagesPerSlab() {
        return CsaSeriesMrPhoenixProtocolsKSpacelImagesPerSlab;
    }

    public void setCsaSeriesMrPhoenixProtocolsKSpacelRadialViews(final int CsaSeriesMrPhoenixProtocolsKSpacelRadialViews) {
        this.CsaSeriesMrPhoenixProtocolsKSpacelRadialViews = CsaSeriesMrPhoenixProtocolsKSpacelRadialViews;
    }

    public int getCsaSeriesMrPhoenixProtocolsKSpacelRadialViews() {
        return CsaSeriesMrPhoenixProtocolsKSpacelRadialViews;
    }

    public void setCsaSeriesMrPhoenixProtocolsKSpacelRadialInterleavesPerImage(final int CsaSeriesMrPhoenixProtocolsKSpacelRadialInterleavesPerImage) {
        this.CsaSeriesMrPhoenixProtocolsKSpacelRadialInterleavesPerImage = CsaSeriesMrPhoenixProtocolsKSpacelRadialInterleavesPerImage;
    }

    public int getCsaSeriesMrPhoenixProtocolsKSpacelRadialInterleavesPerImage() {
        return CsaSeriesMrPhoenixProtocolsKSpacelRadialInterleavesPerImage;
    }

    public void setCsaSeriesMrPhoenixProtocolsKSpacelLinesPerShot(final int CsaSeriesMrPhoenixProtocolsKSpacelLinesPerShot) {
        this.CsaSeriesMrPhoenixProtocolsKSpacelLinesPerShot = CsaSeriesMrPhoenixProtocolsKSpacelLinesPerShot;
    }

    public int getCsaSeriesMrPhoenixProtocolsKSpacelLinesPerShot() {
        return CsaSeriesMrPhoenixProtocolsKSpacelLinesPerShot;
    }

    public void setCsaSeriesMrPhoenixProtocolsKSpaceunReordering(final int CsaSeriesMrPhoenixProtocolsKSpaceunReordering) {
        this.CsaSeriesMrPhoenixProtocolsKSpaceunReordering = CsaSeriesMrPhoenixProtocolsKSpaceunReordering;
    }

    public int getCsaSeriesMrPhoenixProtocolsKSpaceunReordering() {
        return CsaSeriesMrPhoenixProtocolsKSpaceunReordering;
    }

    public void setCsaSeriesMrPhoenixProtocolsKSpacedSeqPhasePartialFourierForSNR(final double CsaSeriesMrPhoenixProtocolsKSpacedSeqPhasePartialFourierForSNR) {
        this.CsaSeriesMrPhoenixProtocolsKSpacedSeqPhasePartialFourierForSNR = CsaSeriesMrPhoenixProtocolsKSpacedSeqPhasePartialFourierForSNR;
    }

    public double getCsaSeriesMrPhoenixProtocolsKSpacedSeqPhasePartialFourierForSNR() {
        return CsaSeriesMrPhoenixProtocolsKSpacedSeqPhasePartialFourierForSNR;
    }

    public void setCsaSeriesMrPhoenixProtocolsKSpaceucPhasePartialFourier(final int CsaSeriesMrPhoenixProtocolsKSpaceucPhasePartialFourier) {
        this.CsaSeriesMrPhoenixProtocolsKSpaceucPhasePartialFourier = CsaSeriesMrPhoenixProtocolsKSpaceucPhasePartialFourier;
    }

    public int getCsaSeriesMrPhoenixProtocolsKSpaceucPhasePartialFourier() {
        return CsaSeriesMrPhoenixProtocolsKSpaceucPhasePartialFourier;
    }

    public void setCsaSeriesMrPhoenixProtocolsKSpaceucSlicePartialFourier(final int CsaSeriesMrPhoenixProtocolsKSpaceucSlicePartialFourier) {
        this.CsaSeriesMrPhoenixProtocolsKSpaceucSlicePartialFourier = CsaSeriesMrPhoenixProtocolsKSpaceucSlicePartialFourier;
    }

    public int getCsaSeriesMrPhoenixProtocolsKSpaceucSlicePartialFourier() {
        return CsaSeriesMrPhoenixProtocolsKSpaceucSlicePartialFourier;
    }

    public void setCsaSeriesMrPhoenixProtocolsKSpaceucAveragingMode(final int CsaSeriesMrPhoenixProtocolsKSpaceucAveragingMode) {
        this.CsaSeriesMrPhoenixProtocolsKSpaceucAveragingMode = CsaSeriesMrPhoenixProtocolsKSpaceucAveragingMode;
    }

    public int getCsaSeriesMrPhoenixProtocolsKSpaceucAveragingMode() {
        return CsaSeriesMrPhoenixProtocolsKSpaceucAveragingMode;
    }

    public void setCsaSeriesMrPhoenixProtocolsKSpaceucMultiSliceMode(final int CsaSeriesMrPhoenixProtocolsKSpaceucMultiSliceMode) {
        this.CsaSeriesMrPhoenixProtocolsKSpaceucMultiSliceMode = CsaSeriesMrPhoenixProtocolsKSpaceucMultiSliceMode;
    }

    public int getCsaSeriesMrPhoenixProtocolsKSpaceucMultiSliceMode() {
        return CsaSeriesMrPhoenixProtocolsKSpaceucMultiSliceMode;
    }

    public void setCsaSeriesMrPhoenixProtocolsKSpaceucDimension(final int CsaSeriesMrPhoenixProtocolsKSpaceucDimension) {
        this.CsaSeriesMrPhoenixProtocolsKSpaceucDimension = CsaSeriesMrPhoenixProtocolsKSpaceucDimension;
    }

    public int getCsaSeriesMrPhoenixProtocolsKSpaceucDimension() {
        return CsaSeriesMrPhoenixProtocolsKSpaceucDimension;
    }

    public void setCsaSeriesMrPhoenixProtocolsKSpaceucTrajectory(final int CsaSeriesMrPhoenixProtocolsKSpaceucTrajectory) {
        this.CsaSeriesMrPhoenixProtocolsKSpaceucTrajectory = CsaSeriesMrPhoenixProtocolsKSpaceucTrajectory;
    }

    public int getCsaSeriesMrPhoenixProtocolsKSpaceucTrajectory() {
        return CsaSeriesMrPhoenixProtocolsKSpaceucTrajectory;
    }

    public void setCsaSeriesMrPhoenixProtocolsKSpaceucViewSharing(final int CsaSeriesMrPhoenixProtocolsKSpaceucViewSharing) {
        this.CsaSeriesMrPhoenixProtocolsKSpaceucViewSharing = CsaSeriesMrPhoenixProtocolsKSpaceucViewSharing;
    }

    public int getCsaSeriesMrPhoenixProtocolsKSpaceucViewSharing() {
        return CsaSeriesMrPhoenixProtocolsKSpaceucViewSharing;
    }

    public void setCsaSeriesMrPhoenixProtocolsKSpaceucAsymmetricEchoMode(final int CsaSeriesMrPhoenixProtocolsKSpaceucAsymmetricEchoMode) {
        this.CsaSeriesMrPhoenixProtocolsKSpaceucAsymmetricEchoMode = CsaSeriesMrPhoenixProtocolsKSpaceucAsymmetricEchoMode;
    }

    public int getCsaSeriesMrPhoenixProtocolsKSpaceucAsymmetricEchoMode() {
        return CsaSeriesMrPhoenixProtocolsKSpaceucAsymmetricEchoMode;
    }

    public void setCsaSeriesMrPhoenixProtocolsKSpaceucPOCS(final int CsaSeriesMrPhoenixProtocolsKSpaceucPOCS) {
        this.CsaSeriesMrPhoenixProtocolsKSpaceucPOCS = CsaSeriesMrPhoenixProtocolsKSpaceucPOCS;
    }

    public int getCsaSeriesMrPhoenixProtocolsKSpaceucPOCS() {
        return CsaSeriesMrPhoenixProtocolsKSpaceucPOCS;
    }

    public void setCsaSeriesMrPhoenixProtocolsFastImaginglEPIFactor(final int CsaSeriesMrPhoenixProtocolsFastImaginglEPIFactor) {
        this.CsaSeriesMrPhoenixProtocolsFastImaginglEPIFactor = CsaSeriesMrPhoenixProtocolsFastImaginglEPIFactor;
    }

    public int getCsaSeriesMrPhoenixProtocolsFastImaginglEPIFactor() {
        return CsaSeriesMrPhoenixProtocolsFastImaginglEPIFactor;
    }

    public void setCsaSeriesMrPhoenixProtocolsFastImaginglTurboFactor(final int CsaSeriesMrPhoenixProtocolsFastImaginglTurboFactor) {
        this.CsaSeriesMrPhoenixProtocolsFastImaginglTurboFactor = CsaSeriesMrPhoenixProtocolsFastImaginglTurboFactor;
    }

    public int getCsaSeriesMrPhoenixProtocolsFastImaginglTurboFactor() {
        return CsaSeriesMrPhoenixProtocolsFastImaginglTurboFactor;
    }

    public void setCsaSeriesMrPhoenixProtocolsFastImaginglSliceTurboFactor(final int CsaSeriesMrPhoenixProtocolsFastImaginglSliceTurboFactor) {
        this.CsaSeriesMrPhoenixProtocolsFastImaginglSliceTurboFactor = CsaSeriesMrPhoenixProtocolsFastImaginglSliceTurboFactor;
    }

    public int getCsaSeriesMrPhoenixProtocolsFastImaginglSliceTurboFactor() {
        return CsaSeriesMrPhoenixProtocolsFastImaginglSliceTurboFactor;
    }

    public void setCsaSeriesMrPhoenixProtocolsFastImaginglSegments(final int CsaSeriesMrPhoenixProtocolsFastImaginglSegments) {
        this.CsaSeriesMrPhoenixProtocolsFastImaginglSegments = CsaSeriesMrPhoenixProtocolsFastImaginglSegments;
    }

    public int getCsaSeriesMrPhoenixProtocolsFastImaginglSegments() {
        return CsaSeriesMrPhoenixProtocolsFastImaginglSegments;
    }

    public void setCsaSeriesMrPhoenixProtocolsFastImagingucSegmentationMode(final int CsaSeriesMrPhoenixProtocolsFastImagingucSegmentationMode) {
        this.CsaSeriesMrPhoenixProtocolsFastImagingucSegmentationMode = CsaSeriesMrPhoenixProtocolsFastImagingucSegmentationMode;
    }

    public int getCsaSeriesMrPhoenixProtocolsFastImagingucSegmentationMode() {
        return CsaSeriesMrPhoenixProtocolsFastImagingucSegmentationMode;
    }

    public void setCsaSeriesMrPhoenixProtocolsFastImaginglShots(final int CsaSeriesMrPhoenixProtocolsFastImaginglShots) {
        this.CsaSeriesMrPhoenixProtocolsFastImaginglShots = CsaSeriesMrPhoenixProtocolsFastImaginglShots;
    }

    public int getCsaSeriesMrPhoenixProtocolsFastImaginglShots() {
        return CsaSeriesMrPhoenixProtocolsFastImaginglShots;
    }

    public void setCsaSeriesMrPhoenixProtocolsFastImaginglEchoTrainDuration(final int CsaSeriesMrPhoenixProtocolsFastImaginglEchoTrainDuration) {
        this.CsaSeriesMrPhoenixProtocolsFastImaginglEchoTrainDuration = CsaSeriesMrPhoenixProtocolsFastImaginglEchoTrainDuration;
    }

    public int getCsaSeriesMrPhoenixProtocolsFastImaginglEchoTrainDuration() {
        return CsaSeriesMrPhoenixProtocolsFastImaginglEchoTrainDuration;
    }

    public void setCsaSeriesMrPhoenixProtocolsPhysioImaginglSignal1(final int CsaSeriesMrPhoenixProtocolsPhysioImaginglSignal1) {
        this.CsaSeriesMrPhoenixProtocolsPhysioImaginglSignal1 = CsaSeriesMrPhoenixProtocolsPhysioImaginglSignal1;
    }

    public int getCsaSeriesMrPhoenixProtocolsPhysioImaginglSignal1() {
        return CsaSeriesMrPhoenixProtocolsPhysioImaginglSignal1;
    }

    public void setCsaSeriesMrPhoenixProtocolsPhysioImaginglMethod1(final int CsaSeriesMrPhoenixProtocolsPhysioImaginglMethod1) {
        this.CsaSeriesMrPhoenixProtocolsPhysioImaginglMethod1 = CsaSeriesMrPhoenixProtocolsPhysioImaginglMethod1;
    }

    public int getCsaSeriesMrPhoenixProtocolsPhysioImaginglMethod1() {
        return CsaSeriesMrPhoenixProtocolsPhysioImaginglMethod1;
    }

    public void setCsaSeriesMrPhoenixProtocolsPhysioImaginglSignal2(final int CsaSeriesMrPhoenixProtocolsPhysioImaginglSignal2) {
        this.CsaSeriesMrPhoenixProtocolsPhysioImaginglSignal2 = CsaSeriesMrPhoenixProtocolsPhysioImaginglSignal2;
    }

    public int getCsaSeriesMrPhoenixProtocolsPhysioImaginglSignal2() {
        return CsaSeriesMrPhoenixProtocolsPhysioImaginglSignal2;
    }

    public void setCsaSeriesMrPhoenixProtocolsPhysioImaginglMethod2(final int CsaSeriesMrPhoenixProtocolsPhysioImaginglMethod2) {
        this.CsaSeriesMrPhoenixProtocolsPhysioImaginglMethod2 = CsaSeriesMrPhoenixProtocolsPhysioImaginglMethod2;
    }

    public int getCsaSeriesMrPhoenixProtocolsPhysioImaginglMethod2() {
        return CsaSeriesMrPhoenixProtocolsPhysioImaginglMethod2;
    }

    public void setCsaSeriesMrPhoenixProtocolsPhysioImaginglPhases(final int CsaSeriesMrPhoenixProtocolsPhysioImaginglPhases) {
        this.CsaSeriesMrPhoenixProtocolsPhysioImaginglPhases = CsaSeriesMrPhoenixProtocolsPhysioImaginglPhases;
    }

    public int getCsaSeriesMrPhoenixProtocolsPhysioImaginglPhases() {
        return CsaSeriesMrPhoenixProtocolsPhysioImaginglPhases;
    }

    public void setCsaSeriesMrPhoenixProtocolsPhysioImaginglRetroGatedImages(final int CsaSeriesMrPhoenixProtocolsPhysioImaginglRetroGatedImages) {
        this.CsaSeriesMrPhoenixProtocolsPhysioImaginglRetroGatedImages = CsaSeriesMrPhoenixProtocolsPhysioImaginglRetroGatedImages;
    }

    public int getCsaSeriesMrPhoenixProtocolsPhysioImaginglRetroGatedImages() {
        return CsaSeriesMrPhoenixProtocolsPhysioImaginglRetroGatedImages;
    }

    public void setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerPulses(final int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerPulses) {
        this.CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerPulses = CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerPulses;
    }

    public int getCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerPulses() {
        return CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerPulses;
    }

    public void setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerWindow(final int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerWindow) {
        this.CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerWindow = CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerWindow;
    }

    public int getCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerWindow() {
        return CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerWindow;
    }

    public void setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlArrhythmiaDetection(
            final int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlArrhythmiaDetection) {
        this.CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlArrhythmiaDetection = CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlArrhythmiaDetection;
    }

    public int getCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlArryhythmiaDetection() {
        return CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlArrhythmiaDetection;
    }

    public void setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlCardiacGateOnThreshold(
            final int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlCardiacGateOnThreshold) {
        this.CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlCardiacGateOnThreshold = CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlCardiacGateOnThreshold;
    }

    public int getCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlCardiacGateOnThreshold() {
        return CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlCardiacGateOnThreshold;
    }

    public void setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlCardiacGateOffThreshold(
            final int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlCardiacGateOffThreshold) {
        this.CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlCardiacGateOffThreshold = CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlCardiacGateOffThreshold;
    }

    public int getCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlCardiacGateOffThreshold() {
        return CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlCardiacGateOffThreshold;
    }

    public void setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerIntervals(
            final int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerIntervals) {
        this.CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerIntervals = CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerIntervals;
    }

    public int getCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerIntervals() {
        return CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioECGlTriggerIntervals;
    }

    public void setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerPulses(
            final int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerPulses) {
        this.CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerPulses = CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerPulses;
    }

    public int getCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerPulses() {
        return CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerPulses;
    }

    public void setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerWindow(
            final int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerWindow) {
        this.CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerWindow = CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerWindow;
    }

    public int getCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerWindow() {
        return CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerWindow;
    }

    public void setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselArrhythmiaDetection(
            final int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselArrhythmiaDetection) {
        this.CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselArrhythmiaDetection = CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselArrhythmiaDetection;
    }

    public int getCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselArryhythmiaDetection() {
        return CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselArrhythmiaDetection;
    }

    public void setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselCardiacGateOnThreshold(
            final int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselCardiacGateOnThreshold) {
        this.CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselCardiacGateOnThreshold = CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselCardiacGateOnThreshold;
    }

    public int getCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselCardiacGateOnThreshold() {
        return CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselCardiacGateOnThreshold;
    }

    public void setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselCardiacGateOffThreshold(
            final int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselCardiacGateOffThreshold) {
        this.CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselCardiacGateOffThreshold = CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselCardiacGateOffThreshold;
    }

    public int getCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselCardiacGateOffThreshold() {
        return CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselCardiacGateOffThreshold;
    }

    public void setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerIntervals(
            final int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerIntervals) {
        this.CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerIntervals = CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerIntervals;
    }

    public int getCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerIntervals() {
        return CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioPulselTriggerIntervals;
    }

    public void setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerPulses(final int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerPulses) {
        this.CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerPulses = CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerPulses;
    }

    public int getCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerPulses() {
        return CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerPulses;
    }

    public void setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerWindow(final int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerWindow) {
        this.CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerWindow = CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerWindow;
    }

    public int getCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerWindow() {
        return CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerWindow;
    }

    public void setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlArrhythmiaDetection(
            final int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlArrhythmiaDetection) {
        this.CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlArrhythmiaDetection = CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlArrhythmiaDetection;
    }

    public int getCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlArryhythmiaDetection() {
        return CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlArrhythmiaDetection;
    }

    public void setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlCardiacGateOnThreshold(
            final int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlCardiacGateOnThreshold) {
        this.CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlCardiacGateOnThreshold = CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlCardiacGateOnThreshold;
    }

    public int getCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlCardiacGateOnThreshold() {
        return CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlCardiacGateOnThreshold;
    }

    public void setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlCardiacGateOffThreshold(
            final int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlCardiacGateOffThreshold) {
        this.CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlCardiacGateOffThreshold = CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlCardiacGateOffThreshold;
    }

    public int getCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlCardiacGateOffThreshold() {
        return CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlCardiacGateOffThreshold;
    }

    public void setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerIntervals(
            final int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerIntervals) {
        this.CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerIntervals = CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerIntervals;
    }

    public int getCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerIntervals() {
        return CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioExtlTriggerIntervals;
    }

    public void setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioResplRespGateThreshold(
            final int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioResplRespGateThreshold) {
        this.CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioResplRespGateThreshold = CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioResplRespGateThreshold;
    }

    public int getCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioResplRespGateThreshold() {
        return CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioResplRespGateThreshold;
    }

    public void setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioResplRespGatePhase(final int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioResplRespGatePhase) {
        this.CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioResplRespGatePhase = CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioResplRespGatePhase;
    }

    public int getCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioResplRespGatePhase() {
        return CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioResplRespGatePhase;
    }

    public void setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioRespdGatingRatio(final double CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioRespdGatingRatio) {
        this.CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioRespdGatingRatio = CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioRespdGatingRatio;
    }

    public double getCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioRespdGatingRatio() {
        return CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioRespdGatingRatio;
    }

    public void setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioNativeucMode(final int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioNativeucMode) {
        this.CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioNativeucMode = CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioNativeucMode;
    }

    public int getCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioNativeucMode() {
        return CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioNativeucMode;
    }

    public void setCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioNativeucFlowSenMode(
            final int CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioNativeucFlowSenMode) {
        this.CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioNativeucFlowSenMode = CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioNativeucFlowSenMode;
    }

    public int getCsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioNativeucFlowSenMode() {
        return CsaSeriesMrPhoenixProtocolsPhysioImagingsPhysioNativeucFlowSenMode;
    }

    public void setCsaSeriesMrPhoenixProtocolsSpecParalPhaseCyclingType(final int CsaSeriesMrPhoenixProtocolsSpecParalPhaseCyclingType) {
        this.CsaSeriesMrPhoenixProtocolsSpecParalPhaseCyclingType = CsaSeriesMrPhoenixProtocolsSpecParalPhaseCyclingType;
    }

    public int getCsaSeriesMrPhoenixProtocolsSpecParalPhaseCyclingType() {
        return CsaSeriesMrPhoenixProtocolsSpecParalPhaseCyclingType;
    }

    public void setCsaSeriesMrPhoenixProtocolsSpecParalPhaseEncodingType(final int CsaSeriesMrPhoenixProtocolsSpecParalPhaseEncodingType) {
        this.CsaSeriesMrPhoenixProtocolsSpecParalPhaseEncodingType = CsaSeriesMrPhoenixProtocolsSpecParalPhaseEncodingType;
    }

    public int getCsaSeriesMrPhoenixProtocolsSpecParalPhaseEncodingType() {
        return CsaSeriesMrPhoenixProtocolsSpecParalPhaseEncodingType;
    }

    public void setCsaSeriesMrPhoenixProtocolsSpecParalRFExcitationBandwidth(final int CsaSeriesMrPhoenixProtocolsSpecParalRFExcitationBandwidth) {
        this.CsaSeriesMrPhoenixProtocolsSpecParalRFExcitationBandwidth = CsaSeriesMrPhoenixProtocolsSpecParalRFExcitationBandwidth;
    }

    public int getCsaSeriesMrPhoenixProtocolsSpecParalRFExcitationBandwidth() {
        return CsaSeriesMrPhoenixProtocolsSpecParalRFExcitationBandwidth;
    }

    public void setCsaSeriesMrPhoenixProtocolsSpecParaucRemoveOversampling(final int CsaSeriesMrPhoenixProtocolsSpecParaucRemoveOversampling) {
        this.CsaSeriesMrPhoenixProtocolsSpecParaucRemoveOversampling = CsaSeriesMrPhoenixProtocolsSpecParaucRemoveOversampling;
    }

    public int getCsaSeriesMrPhoenixProtocolsSpecParaucRemoveOversampling() {
        return CsaSeriesMrPhoenixProtocolsSpecParaucRemoveOversampling;
    }

    public void setCsaSeriesMrPhoenixProtocolsSpecParalAutoRefScanNo(final int CsaSeriesMrPhoenixProtocolsSpecParalAutoRefScanNo) {
        this.CsaSeriesMrPhoenixProtocolsSpecParalAutoRefScanNo = CsaSeriesMrPhoenixProtocolsSpecParalAutoRefScanNo;
    }

    public int getCsaSeriesMrPhoenixProtocolsSpecParalAutoRefScanNo() {
        return CsaSeriesMrPhoenixProtocolsSpecParalAutoRefScanNo;
    }

    public void setCsaSeriesMrPhoenixProtocolsSpecParalDecouplingType(final int CsaSeriesMrPhoenixProtocolsSpecParalDecouplingType) {
        this.CsaSeriesMrPhoenixProtocolsSpecParalDecouplingType = CsaSeriesMrPhoenixProtocolsSpecParalDecouplingType;
    }

    public int getCsaSeriesMrPhoenixProtocolsSpecParalDecouplingType() {
        return CsaSeriesMrPhoenixProtocolsSpecParalDecouplingType;
    }

    public void setCsaSeriesMrPhoenixProtocolsSpecParalNOEType(final int CsaSeriesMrPhoenixProtocolsSpecParalNOEType) {
        this.CsaSeriesMrPhoenixProtocolsSpecParalNOEType = CsaSeriesMrPhoenixProtocolsSpecParalNOEType;
    }

    public int getCsaSeriesMrPhoenixProtocolsSpecParalNOEType() {
        return CsaSeriesMrPhoenixProtocolsSpecParalNOEType;
    }

    public void setCsaSeriesMrPhoenixProtocolsSpecParalExcitationType(final int CsaSeriesMrPhoenixProtocolsSpecParalExcitationType) {
        this.CsaSeriesMrPhoenixProtocolsSpecParalExcitationType = CsaSeriesMrPhoenixProtocolsSpecParalExcitationType;
    }

    public int getCsaSeriesMrPhoenixProtocolsSpecParalExcitationType() {
        return CsaSeriesMrPhoenixProtocolsSpecParalExcitationType;
    }

    public void setCsaSeriesMrPhoenixProtocolsSpecParalSpecAppl(final int CsaSeriesMrPhoenixProtocolsSpecParalSpecAppl) {
        this.CsaSeriesMrPhoenixProtocolsSpecParalSpecAppl = CsaSeriesMrPhoenixProtocolsSpecParalSpecAppl;
    }

    public int getCsaSeriesMrPhoenixProtocolsSpecParalSpecAppl() {
        return CsaSeriesMrPhoenixProtocolsSpecParalSpecAppl;
    }

    public void setCsaSeriesMrPhoenixProtocolsSpecParalSpectralSuppression(final int CsaSeriesMrPhoenixProtocolsSpecParalSpectralSuppression) {
        this.CsaSeriesMrPhoenixProtocolsSpecParalSpectralSuppression = CsaSeriesMrPhoenixProtocolsSpecParalSpectralSuppression;
    }

    public int getCsaSeriesMrPhoenixProtocolsSpecParalSpectralSuppression() {
        return CsaSeriesMrPhoenixProtocolsSpecParalSpectralSuppression;
    }

    public void setCsaSeriesMrPhoenixProtocolsDiffusionulMode(final int CsaSeriesMrPhoenixProtocolsDiffusionulMode) {
        this.CsaSeriesMrPhoenixProtocolsDiffusionulMode = CsaSeriesMrPhoenixProtocolsDiffusionulMode;
    }

    public int getCsaSeriesMrPhoenixProtocolsDiffusionulMode() {
        return CsaSeriesMrPhoenixProtocolsDiffusionulMode;
    }

    public void setCsaSeriesMrPhoenixProtocolsAngioucPCFlowMode(final int CsaSeriesMrPhoenixProtocolsAngioucPCFlowMode) {
        this.CsaSeriesMrPhoenixProtocolsAngioucPCFlowMode = CsaSeriesMrPhoenixProtocolsAngioucPCFlowMode;
    }

    public int getCsaSeriesMrPhoenixProtocolsAngioucPCFlowMode() {
        return CsaSeriesMrPhoenixProtocolsAngioucPCFlowMode;
    }

    public void setCsaSeriesMrPhoenixProtocolsAngioucTOFInflow(final int CsaSeriesMrPhoenixProtocolsAngioucTOFInflow) {
        this.CsaSeriesMrPhoenixProtocolsAngioucTOFInflow = CsaSeriesMrPhoenixProtocolsAngioucTOFInflow;
    }

    public int getCsaSeriesMrPhoenixProtocolsAngioucTOFInflow() {
        return CsaSeriesMrPhoenixProtocolsAngioucTOFInflow;
    }

    public void setCsaSeriesMrPhoenixProtocolsAngiolDynamicReconMode(final int CsaSeriesMrPhoenixProtocolsAngiolDynamicReconMode) {
        this.CsaSeriesMrPhoenixProtocolsAngiolDynamicReconMode = CsaSeriesMrPhoenixProtocolsAngiolDynamicReconMode;
    }

    public int getCsaSeriesMrPhoenixProtocolsAngiolDynamicReconMode() {
        return CsaSeriesMrPhoenixProtocolsAngiolDynamicReconMode;
    }

    public void setCsaSeriesMrPhoenixProtocolsAngiolTemporalInterpolation(final int CsaSeriesMrPhoenixProtocolsAngiolTemporalInterpolation) {
        this.CsaSeriesMrPhoenixProtocolsAngiolTemporalInterpolation = CsaSeriesMrPhoenixProtocolsAngiolTemporalInterpolation;
    }

    public int getCsaSeriesMrPhoenixProtocolsAngiolTemporalInterpolation() {
        return CsaSeriesMrPhoenixProtocolsAngiolTemporalInterpolation;
    }

    public void setCsaSeriesMrPhoenixProtocolsRawFilterlSlope_256(final int CsaSeriesMrPhoenixProtocolsRawFilterlSlope_256) {
        this.CsaSeriesMrPhoenixProtocolsRawFilterlSlope_256 = CsaSeriesMrPhoenixProtocolsRawFilterlSlope_256;
    }

    public int getCsaSeriesMrPhoenixProtocolsRawFilterlSlope_256() {
        return CsaSeriesMrPhoenixProtocolsRawFilterlSlope_256;
    }

    public void setCsaSeriesMrPhoenixProtocolsRawFilterucOn(final int CsaSeriesMrPhoenixProtocolsRawFilterucOn) {
        this.CsaSeriesMrPhoenixProtocolsRawFilterucOn = CsaSeriesMrPhoenixProtocolsRawFilterucOn;
    }

    public int getCsaSeriesMrPhoenixProtocolsRawFilterucOn() {
        return CsaSeriesMrPhoenixProtocolsRawFilterucOn;
    }

    public void setCsaSeriesMrPhoenixProtocolsRawFilterucMode(final int CsaSeriesMrPhoenixProtocolsRawFilterucMode) {
        this.CsaSeriesMrPhoenixProtocolsRawFilterucMode = CsaSeriesMrPhoenixProtocolsRawFilterucMode;
    }

    public int getCsaSeriesMrPhoenixProtocolsRawFilterucMode() {
        return CsaSeriesMrPhoenixProtocolsRawFilterucMode;
    }

    public void setCsaSeriesMrPhoenixProtocolsDistortionCorrFilterucMode(final int CsaSeriesMrPhoenixProtocolsDistortionCorrFilterucMode) {
        this.CsaSeriesMrPhoenixProtocolsDistortionCorrFilterucMode = CsaSeriesMrPhoenixProtocolsDistortionCorrFilterucMode;
    }

    public int getCsaSeriesMrPhoenixProtocolsDistortionCorrFilterucMode() {
        return CsaSeriesMrPhoenixProtocolsDistortionCorrFilterucMode;
    }

    public void setCsaSeriesMrPhoenixProtocolsPatlAccelFactPE(final int CsaSeriesMrPhoenixProtocolsPatlAccelFactPE) {
        this.CsaSeriesMrPhoenixProtocolsPatlAccelFactPE = CsaSeriesMrPhoenixProtocolsPatlAccelFactPE;
    }

    public int getCsaSeriesMrPhoenixProtocolsPatlAccelFactPE() {
        return CsaSeriesMrPhoenixProtocolsPatlAccelFactPE;
    }

    public void setCsaSeriesMrPhoenixProtocolsPatlAccelFact3D(final int CsaSeriesMrPhoenixProtocolsPatlAccelFact3D) {
        this.CsaSeriesMrPhoenixProtocolsPatlAccelFact3D = CsaSeriesMrPhoenixProtocolsPatlAccelFact3D;
    }

    public int getCsaSeriesMrPhoenixProtocolsPatlAccelFact3D() {
        return CsaSeriesMrPhoenixProtocolsPatlAccelFact3D;
    }

    public void setCsaSeriesMrPhoenixProtocolsPatlRefLinesPE(final int CsaSeriesMrPhoenixProtocolsPatlRefLinesPE) {
        this.CsaSeriesMrPhoenixProtocolsPatlRefLinesPE = CsaSeriesMrPhoenixProtocolsPatlRefLinesPE;
    }

    public int getCsaSeriesMrPhoenixProtocolsPatlRefLinesPE() {
        return CsaSeriesMrPhoenixProtocolsPatlRefLinesPE;
    }

    public void setCsaSeriesMrPhoenixProtocolsPatucPATMode(final int CsaSeriesMrPhoenixProtocolsPatucPATMode) {
        this.CsaSeriesMrPhoenixProtocolsPatucPATMode = CsaSeriesMrPhoenixProtocolsPatucPATMode;
    }

    public int getCsaSeriesMrPhoenixProtocolsPatucPATMode() {
        return CsaSeriesMrPhoenixProtocolsPatucPATMode;
    }

    public void setCsaSeriesMrPhoenixProtocolsPatucRefScanMode(final int CsaSeriesMrPhoenixProtocolsPatucRefScanMode) {
        this.CsaSeriesMrPhoenixProtocolsPatucRefScanMode = CsaSeriesMrPhoenixProtocolsPatucRefScanMode;
    }

    public int getCsaSeriesMrPhoenixProtocolsPatucRefScanMode() {
        return CsaSeriesMrPhoenixProtocolsPatucRefScanMode;
    }

    public void setCsaSeriesMrPhoenixProtocolsPatucTPatAverageAllFrames(final int CsaSeriesMrPhoenixProtocolsPatucTPatAverageAllFrames) {
        this.CsaSeriesMrPhoenixProtocolsPatucTPatAverageAllFrames = CsaSeriesMrPhoenixProtocolsPatucTPatAverageAllFrames;
    }

    public int getCsaSeriesMrPhoenixProtocolsPatucTPatAverageAllFrames() {
        return CsaSeriesMrPhoenixProtocolsPatucTPatAverageAllFrames;
    }

    public void setCsaSeriesMrPhoenixProtocolsMDSulMdsModeMask(final int CsaSeriesMrPhoenixProtocolsMDSulMdsModeMask) {
        this.CsaSeriesMrPhoenixProtocolsMDSulMdsModeMask = CsaSeriesMrPhoenixProtocolsMDSulMdsModeMask;
    }

    public int getCsaSeriesMrPhoenixProtocolsMDSulMdsModeMask() {
        return CsaSeriesMrPhoenixProtocolsMDSulMdsModeMask;
    }

    public void setCsaSeriesMrPhoenixProtocolsMDSulMdsVariableResolution(final int CsaSeriesMrPhoenixProtocolsMDSulMdsVariableResolution) {
        this.CsaSeriesMrPhoenixProtocolsMDSulMdsVariableResolution = CsaSeriesMrPhoenixProtocolsMDSulMdsVariableResolution;
    }

    public int getCsaSeriesMrPhoenixProtocolsMDSulMdsVariableResolution() {
        return CsaSeriesMrPhoenixProtocolsMDSulMdsVariableResolution;
    }

    public void setCsaSeriesMrPhoenixProtocolsMDSlTableSpeedNumerator(final int CsaSeriesMrPhoenixProtocolsMDSlTableSpeedNumerator) {
        this.CsaSeriesMrPhoenixProtocolsMDSlTableSpeedNumerator = CsaSeriesMrPhoenixProtocolsMDSlTableSpeedNumerator;
    }

    public int getCsaSeriesMrPhoenixProtocolsMDSlTableSpeedNumerator() {
        return CsaSeriesMrPhoenixProtocolsMDSlTableSpeedNumerator;
    }

    public void setCsaSeriesMrPhoenixProtocolsMDSlmdsLinesPerSegment(final int CsaSeriesMrPhoenixProtocolsMDSlmdsLinesPerSegment) {
        this.CsaSeriesMrPhoenixProtocolsMDSlmdsLinesPerSegment = CsaSeriesMrPhoenixProtocolsMDSlmdsLinesPerSegment;
    }

    public int getCsaSeriesMrPhoenixProtocolsMDSlmdsLinesPerSegment() {
        return CsaSeriesMrPhoenixProtocolsMDSlmdsLinesPerSegment;
    }

    public void setCsaSeriesMrPhoenixProtocolsMDSsMdsEndPosSBCS_mmdTra(final double CsaSeriesMrPhoenixProtocolsMDSsMdsEndPosSBCS_mmdTra) {
        this.CsaSeriesMrPhoenixProtocolsMDSsMdsEndPosSBCS_mmdTra = CsaSeriesMrPhoenixProtocolsMDSsMdsEndPosSBCS_mmdTra;
    }

    public double getCsaSeriesMrPhoenixProtocolsMDSsMdsEndPosSBCS_mmdTra() {
        return CsaSeriesMrPhoenixProtocolsMDSsMdsEndPosSBCS_mmdTra;
    }

    public void setCsaSeriesMrPhoenixProtocolsMDSulMdsReconMode(final int CsaSeriesMrPhoenixProtocolsMDSulMdsReconMode) {
        this.CsaSeriesMrPhoenixProtocolsMDSulMdsReconMode = CsaSeriesMrPhoenixProtocolsMDSulMdsReconMode;
    }

    public int getCsaSeriesMrPhoenixProtocolsMDSulMdsReconMode() {
        return CsaSeriesMrPhoenixProtocolsMDSulMdsReconMode;
    }

    public void setCsaSeriesMrPhoenixProtocolsMDSdMdsRangeExtension(final double CsaSeriesMrPhoenixProtocolsMDSdMdsRangeExtension) {
        this.CsaSeriesMrPhoenixProtocolsMDSdMdsRangeExtension = CsaSeriesMrPhoenixProtocolsMDSdMdsRangeExtension;
    }

    public double getCsaSeriesMrPhoenixProtocolsMDSdMdsRangeExtension() {
        return CsaSeriesMrPhoenixProtocolsMDSdMdsRangeExtension;
    }
    
    public void setCsaSeriesMrPhoenixProtocolucEnableIntro(int CsaSeriesMrPhoenixProtocolucEnableIntro) {
    	this.CsaSeriesMrPhoenixProtocolucEnableIntro = CsaSeriesMrPhoenixProtocolucEnableIntro;
    }
    
    public int getCsaSeriesMrPhoenixProtocolucEnableIntro() {
    	return CsaSeriesMrPhoenixProtocolucEnableIntro;
    }
    
    public void setCsaSeriesMrPhoenixProtocolucDisableChangeStoreImages(int CsaSeriesMrPhoenixProtocolucDisableChangeStoreImages) {
    	this.CsaSeriesMrPhoenixProtocolucDisableChangeStoreImages = CsaSeriesMrPhoenixProtocolucDisableChangeStoreImages;
    }

    public int getCsaSeriesMrPhoenixProtocolucDisableChangeStoreImages() {
        return CsaSeriesMrPhoenixProtocolucDisableChangeStoreImages;
    }

    public void setCsaSeriesMrPhoenixProtocolucAAMode(final int CsaSeriesMrPhoenixProtocolucAAMode) {
        this.CsaSeriesMrPhoenixProtocolucAAMode = CsaSeriesMrPhoenixProtocolucAAMode;
    }

    public int getCsaSeriesMrPhoenixProtocolucAAMode() {
        return CsaSeriesMrPhoenixProtocolucAAMode;
    }

    public void setCsaSeriesMrPhoenixProtocolucAARegionMode(final int CsaSeriesMrPhoenixProtocolucAARegionMode) {
        this.CsaSeriesMrPhoenixProtocolucAARegionMode = CsaSeriesMrPhoenixProtocolucAARegionMode;
    }

    public int getCsaSeriesMrPhoenixProtocolucAARegionMode() {
        return CsaSeriesMrPhoenixProtocolucAARegionMode;
    }

    public void setCsaSeriesMrPhoenixProtocolucAARefMode(final int CsaSeriesMrPhoenixProtocolucAARefMode) {
        this.CsaSeriesMrPhoenixProtocolucAARefMode = CsaSeriesMrPhoenixProtocolucAARefMode;
    }

    public int getCsaSeriesMrPhoenixProtocolucAARefMode() {
        return CsaSeriesMrPhoenixProtocolucAARefMode;
    }

    public void setCsaSeriesMrPhoenixProtocolucReconstructionMode(final int CsaSeriesMrPhoenixProtocolucReconstructionMode) {
        this.CsaSeriesMrPhoenixProtocolucReconstructionMode = CsaSeriesMrPhoenixProtocolucReconstructionMode;
    }

    public int getCsaSeriesMrPhoenixProtocolucReconstructionMode() {
        return CsaSeriesMrPhoenixProtocolucReconstructionMode;
    }

    public void setCsaSeriesMrPhoenixProtocolucOneSeriesForAllMeas(final int CsaSeriesMrPhoenixProtocolucOneSeriesForAllMeas) {
        this.CsaSeriesMrPhoenixProtocolucOneSeriesForAllMeas = CsaSeriesMrPhoenixProtocolucOneSeriesForAllMeas;
    }

    public int getCsaSeriesMrPhoenixProtocolucOneSeriesForAllMeas() {
        return CsaSeriesMrPhoenixProtocolucOneSeriesForAllMeas;
    }

    public void setCsaSeriesMrPhoenixProtocolucPHAPSMode(final int CsaSeriesMrPhoenixProtocolucPHAPSMode) {
        this.CsaSeriesMrPhoenixProtocolucPHAPSMode = CsaSeriesMrPhoenixProtocolucPHAPSMode;
    }

    public int getCsaSeriesMrPhoenixProtocolucPHAPSMode() {
        return CsaSeriesMrPhoenixProtocolucPHAPSMode;
    }

    public void setCsaSeriesMrPhoenixProtocolucDixon(final int CsaSeriesMrPhoenixProtocolucDixon) {
        this.CsaSeriesMrPhoenixProtocolucDixon = CsaSeriesMrPhoenixProtocolucDixon;
    }

    public int getCsaSeriesMrPhoenixProtocolucDixon() {
        return CsaSeriesMrPhoenixProtocolucDixon;
    }

    public void setCsaSeriesMrPhoenixProtocolucDixonSaveOriginal(final int CsaSeriesMrPhoenixProtocolucDixonSaveOriginal) {
        this.CsaSeriesMrPhoenixProtocolucDixonSaveOriginal = CsaSeriesMrPhoenixProtocolucDixonSaveOriginal;
    }

    public int getCsaSeriesMrPhoenixProtocolucDixonSaveOriginal() {
        return CsaSeriesMrPhoenixProtocolucDixonSaveOriginal;
    }

    public void setCsaSeriesMrPhoenixProtocolucWaitForPrepareCompletion(final int CsaSeriesMrPhoenixProtocolucWaitForPrepareCompletion) {
        this.CsaSeriesMrPhoenixProtocolucWaitForPrepareCompletion = CsaSeriesMrPhoenixProtocolucWaitForPrepareCompletion;
    }

    public int getCsaSeriesMrPhoenixProtocolucWaitForPrepareCompletion() {
        return CsaSeriesMrPhoenixProtocolucWaitForPrepareCompletion;
    }

    public void setCsaSeriesMrPhoenixProtocollAverages(final int CsaSeriesMrPhoenixProtocollAverages) {
        this.CsaSeriesMrPhoenixProtocollAverages = CsaSeriesMrPhoenixProtocollAverages;
    }

    public int getCsaSeriesMrPhoenixProtocollAverages() {
        return CsaSeriesMrPhoenixProtocollAverages;
    }

    public void setCsaSeriesMrPhoenixProtocoldAveragesDouble(final double CsaSeriesMrPhoenixProtocoldAveragesDouble) {
        this.CsaSeriesMrPhoenixProtocoldAveragesDouble = CsaSeriesMrPhoenixProtocoldAveragesDouble;
    }

    public double getCsaSeriesMrPhoenixProtocoldAveragesDouble() {
        return CsaSeriesMrPhoenixProtocoldAveragesDouble;
    }

    public void setCsaSeriesMrPhoenixProtocollRepetitions(final int CsaSeriesMrPhoenixProtocollRepetitions) {
        this.CsaSeriesMrPhoenixProtocollRepetitions = CsaSeriesMrPhoenixProtocollRepetitions;
    }

    public int getCsaSeriesMrPhoenixProtocollRepetitions() {
        return CsaSeriesMrPhoenixProtocollRepetitions;
    }

    public void setCsaSeriesMrPhoenixProtocoladFlipAngleDegree0(final double CsaSeriesMrPhoenixProtocoladFlipAngleDegree0) {
        this.CsaSeriesMrPhoenixProtocoladFlipAngleDegree0 = CsaSeriesMrPhoenixProtocoladFlipAngleDegree0;
    }

    public double getCsaSeriesMrPhoenixProtocoladFlipAngleDegree0() {
        return CsaSeriesMrPhoenixProtocoladFlipAngleDegree0;
    }

    public void setCsaSeriesMrPhoenixProtocollScanTimeSec(final int CsaSeriesMrPhoenixProtocollScanTimeSec) {
        this.CsaSeriesMrPhoenixProtocollScanTimeSec = CsaSeriesMrPhoenixProtocollScanTimeSec;
    }

    public int getCsaSeriesMrPhoenixProtocollScanTimeSec() {
        return CsaSeriesMrPhoenixProtocollScanTimeSec;
    }

    public void setCsaSeriesMrPhoenixProtocollTotalScanTimeSec(final int CsaSeriesMrPhoenixProtocollTotalScanTimeSec) {
        this.CsaSeriesMrPhoenixProtocollTotalScanTimeSec = CsaSeriesMrPhoenixProtocollTotalScanTimeSec;
    }

    public int getCsaSeriesMrPhoenixProtocollTotalScanTimeSec() {
        return CsaSeriesMrPhoenixProtocollTotalScanTimeSec;
    }

    public void setCsaSeriesMrPhoenixProtocoldRefSNR(final double CsaSeriesMrPhoenixProtocoldRefSNR) {
        this.CsaSeriesMrPhoenixProtocoldRefSNR = CsaSeriesMrPhoenixProtocoldRefSNR;
    }

    public double getCsaSeriesMrPhoenixProtocoldRefSNR() {
        return CsaSeriesMrPhoenixProtocoldRefSNR;
    }

    public void setCsaSeriesMrPhoenixProtocoldRefSNR_VOI(final double CsaSeriesMrPhoenixProtocoldRefSNR_VOI) {
        this.CsaSeriesMrPhoenixProtocoldRefSNR_VOI = CsaSeriesMrPhoenixProtocoldRefSNR_VOI;
    }

    public double getCsaSeriesMrPhoenixProtocoldRefSNR_VOI() {
        return CsaSeriesMrPhoenixProtocoldRefSNR_VOI;
    }

    public void setCsaSeriesMrPhoenixProtocoltdefaultEVAProt(final String CsaSeriesMrPhoenixProtocoltdefaultEVAProt) {
        this.CsaSeriesMrPhoenixProtocoltdefaultEVAProt = CsaSeriesMrPhoenixProtocoltdefaultEVAProt;
    }

    public String getCsaSeriesMrPhoenixProtocoltdefaultEVAProt() {
        return CsaSeriesMrPhoenixProtocoltdefaultEVAProt;
    }

    public void setCsaSeriesMrPhoenixProtocolasCoilSelectMeas0tNucleus(final String CsaSeriesMrPhoenixProtocolasCoilSelectMeas0tNucleus) {
        this.CsaSeriesMrPhoenixProtocolasCoilSelectMeas0tNucleus = CsaSeriesMrPhoenixProtocolasCoilSelectMeas0tNucleus;
    }

    public String getCsaSeriesMrPhoenixProtocolasCoilSelectMeas0tNucleus() {
        return CsaSeriesMrPhoenixProtocolasCoilSelectMeas0tNucleus;
    }

    public void setCsaSeriesMrPhoenixProtocolasCoilSelectMeas0iUsedRFactor(final int CsaSeriesMrPhoenixProtocolasCoilSelectMeas0iUsedRFactor) {
        this.CsaSeriesMrPhoenixProtocolasCoilSelectMeas0iUsedRFactor = CsaSeriesMrPhoenixProtocolasCoilSelectMeas0iUsedRFactor;
    }

    public int getCsaSeriesMrPhoenixProtocolasCoilSelectMeas0iUsedRFactor() {
        return CsaSeriesMrPhoenixProtocolasCoilSelectMeas0iUsedRFactor;
    }

    public void setCsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtCoilID(
            final String CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtCoilID[]) {
        this.CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtCoilID = CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtCoilID;
    }

    public String[] getCsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtCoilID() {
        return CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtCoilID;
    }

    public void setCsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDlCoilCopy(
            final int CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDlCoilCopy[]) {
        this.CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDlCoilCopy = CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDlCoilCopy;
    }

    public int[] getCsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDlCoilCopy() {
        return CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDlCoilCopy;
    }

    public void setCsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtElement(
            final String CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtElement[]) {
        this.CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtElement = CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtElement;
    }

    public String[] getCsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtElement() {
        return CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnsCoilElementIDtElement;
    }

    public void setCsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlElementSelected(
            final int CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlElementSelected[]) {
        this.CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlElementSelected = CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlElementSelected;
    }

    public int[] getCsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlElementSelected() {
        return CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlElementSelected;
    }

    public void setCsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlRxChannelConnected(
            final int CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlRxChannelConnected[]) {
        this.CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlRxChannelConnected = CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlRxChannelConnected;
    }

    public int[] getCsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlRxChannelConnected() {
        return CsaSeriesMrPhoenixProtocolasCoilSelectMeas0asListnlRxChannelConnected;
    }

    public void setCsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSaulPlugId(final int CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSaulPlugId[]) {
        this.CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSaulPlugId = CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSaulPlugId;
    }

    public int[] getCsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSaulPlugId() {
        return CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSaulPlugId;
    }

    public void setCsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSauiNmbrOfNibbles(
            final int CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSauiNmbrOfNibbles[]) {
        this.CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSauiNmbrOfNibbles = CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSauiNmbrOfNibbles;
    }

    public int[] getCsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSauiNmbrOfNibbles() {
        return CsaSeriesMrPhoenixProtocolasCoilSelectMeas0sCOILPLUGSauiNmbrOfNibbles;
    }

    public void setCsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnflFactor(final double CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnflFactor[]) {
        this.CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnflFactor = CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnflFactor;
    }

    public double[] getCsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnflFactor() {
        return CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnflFactor;
    }

    public void setCsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnbValid(final int CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnbValid[]) {
        this.CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnbValid = CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnbValid;
    }

    public int[] getCsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnbValid() {
        return CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnbValid;
    }

    public void setCsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnlRxChannel(
            final int CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnlRxChannel[]) {
        this.CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnlRxChannel = CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnlRxChannel;
    }

    public int[] getCsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnlRxChannel() {
        return CsaSeriesMrPhoenixProtocolasCoilSelectMeas0aFFT_SCALEnlRxChannel;
    }

    public void setCsaSeriesMrPhoenixProtocolsEFISPECbEFIDataValid(final int CsaSeriesMrPhoenixProtocolsEFISPECbEFIDataValid) {
        this.CsaSeriesMrPhoenixProtocolsEFISPECbEFIDataValid = CsaSeriesMrPhoenixProtocolsEFISPECbEFIDataValid;
    }

    public int getCsaSeriesMrPhoenixProtocolsEFISPECbEFIDataValid() {
        return CsaSeriesMrPhoenixProtocolsEFISPECbEFIDataValid;
    }

    public void setCsaSeriesMrPhoenixProtocolsWiPMemBlockalFree(final int CsaSeriesMrPhoenixProtocolsWiPMemBlockalFree[]) {
        this.CsaSeriesMrPhoenixProtocolsWiPMemBlockalFree = CsaSeriesMrPhoenixProtocolsWiPMemBlockalFree;
    }

    public int[] getCsaSeriesMrPhoenixProtocolsWiPMemBlockalFree() {
        return CsaSeriesMrPhoenixProtocolsWiPMemBlockalFree;
    }

    public void setCsaSeriesMrPhoenixProtocolsWiPMemBlockadFree(final double CsaSeriesMrPhoenixProtocolsWiPMemBlockadFree[]) {
        this.CsaSeriesMrPhoenixProtocolsWiPMemBlockadFree = CsaSeriesMrPhoenixProtocolsWiPMemBlockadFree;
    }

    public double[] getCsaSeriesMrPhoenixProtocolsWiPMemBlockadFree() {
        return CsaSeriesMrPhoenixProtocolsWiPMemBlockadFree;
    }

    public void setCsaSeriesMrPhoenixProtocolsWiPMemBlocktFree(final String CsaSeriesMrPhoenixProtocolsWiPMemBlocktFree) {
        this.CsaSeriesMrPhoenixProtocolsWiPMemBlocktFree = CsaSeriesMrPhoenixProtocolsWiPMemBlocktFree;
    }

    public String getCsaSeriesMrPhoenixProtocolsWiPMemBlocktFree() {
        return CsaSeriesMrPhoenixProtocolsWiPMemBlocktFree;
    }

    public void setCsaSeriesMrPhoenixProtocolucBOLDParadigmArray(final int CsaSeriesMrPhoenixProtocolucBOLDParadigmArray[]) {
        this.CsaSeriesMrPhoenixProtocolucBOLDParadigmArray = CsaSeriesMrPhoenixProtocolucBOLDParadigmArray;
    }

    public int[] getCsaSeriesMrPhoenixProtocolucBOLDParadigmArray() {
        return CsaSeriesMrPhoenixProtocolucBOLDParadigmArray;
    }

    public void setCsaSeriesMrPhoenixProtocollParadigmPeriodicity(final int CsaSeriesMrPhoenixProtocollParadigmPeriodicity) {
        this.CsaSeriesMrPhoenixProtocollParadigmPeriodicity = CsaSeriesMrPhoenixProtocollParadigmPeriodicity;
    }

    public int getCsaSeriesMrPhoenixProtocollParadigmPeriodicity() {
        return CsaSeriesMrPhoenixProtocollParadigmPeriodicity;
    }

    public void setCsaSeriesMrPhoenixProtocolucCineMode(final int CsaSeriesMrPhoenixProtocolucCineMode) {
        this.CsaSeriesMrPhoenixProtocolucCineMode = CsaSeriesMrPhoenixProtocolucCineMode;
    }

    public int getCsaSeriesMrPhoenixProtocolucCineMode() {
        return CsaSeriesMrPhoenixProtocolucCineMode;
    }

    public void setCsaSeriesMrPhoenixProtocolucSequenceType(final int CsaSeriesMrPhoenixProtocolucSequenceType) {
        this.CsaSeriesMrPhoenixProtocolucSequenceType = CsaSeriesMrPhoenixProtocolucSequenceType;
    }

    public int getCsaSeriesMrPhoenixProtocolucSequenceType() {
        return CsaSeriesMrPhoenixProtocolucSequenceType;
    }

    public void setCsaSeriesMrPhoenixProtocolucCoilCombineMode(final int CsaSeriesMrPhoenixProtocolucCoilCombineMode) {
        this.CsaSeriesMrPhoenixProtocolucCoilCombineMode = CsaSeriesMrPhoenixProtocolucCoilCombineMode;
    }

    public int getCsaSeriesMrPhoenixProtocolucCoilCombineMode() {
        return CsaSeriesMrPhoenixProtocolucCoilCombineMode;
    }

    public void setCsaSeriesMrPhoenixProtocolucFlipAngleMode(final int CsaSeriesMrPhoenixProtocolucFlipAngleMode) {
        this.CsaSeriesMrPhoenixProtocolucFlipAngleMode = CsaSeriesMrPhoenixProtocolucFlipAngleMode;
    }

    public int getCsaSeriesMrPhoenixProtocolucFlipAngleMode() {
        return CsaSeriesMrPhoenixProtocolucFlipAngleMode;
    }

    public void setCsaSeriesMrPhoenixProtocollTOM(final int CsaSeriesMrPhoenixProtocollTOM) {
        this.CsaSeriesMrPhoenixProtocollTOM = CsaSeriesMrPhoenixProtocollTOM;
    }

    public int getCsaSeriesMrPhoenixProtocollTOM() {
        return CsaSeriesMrPhoenixProtocollTOM;
    }

    public void setCsaSeriesMrPhoenixProtocollProtID(final int CsaSeriesMrPhoenixProtocollProtID) {
        this.CsaSeriesMrPhoenixProtocollProtID = CsaSeriesMrPhoenixProtocollProtID;
    }

    public int getCsaSeriesMrPhoenixProtocollProtID() {
        return CsaSeriesMrPhoenixProtocollProtID;
    }

    public void setCsaSeriesMrPhoenixProtocolucReadOutMode(final int CsaSeriesMrPhoenixProtocolucReadOutMode) {
        this.CsaSeriesMrPhoenixProtocolucReadOutMode = CsaSeriesMrPhoenixProtocolucReadOutMode;
    }

    public int getCsaSeriesMrPhoenixProtocolucReadOutMode() {
        return CsaSeriesMrPhoenixProtocolucReadOutMode;
    }

    public void setCsaSeriesMrPhoenixProtocolucBold3dPace(final int CsaSeriesMrPhoenixProtocolucBold3dPace) {
        this.CsaSeriesMrPhoenixProtocolucBold3dPace = CsaSeriesMrPhoenixProtocolucBold3dPace;
    }

    public int getCsaSeriesMrPhoenixProtocolucBold3dPace() {
        return CsaSeriesMrPhoenixProtocolucBold3dPace;
    }

    public void setCsaSeriesMrPhoenixProtocolucForcePositioningOnNDIS(final int CsaSeriesMrPhoenixProtocolucForcePositioningOnNDIS) {
        this.CsaSeriesMrPhoenixProtocolucForcePositioningOnNDIS = CsaSeriesMrPhoenixProtocolucForcePositioningOnNDIS;
    }

    public int getCsaSeriesMrPhoenixProtocolucForcePositioningOnNDIS() {
        return CsaSeriesMrPhoenixProtocolucForcePositioningOnNDIS;
    }

    public void setCsaSeriesMrPhoenixProtocolucInteractiveRealtime(final int CsaSeriesMrPhoenixProtocolucInteractiveRealtime) {
        this.CsaSeriesMrPhoenixProtocolucInteractiveRealtime = CsaSeriesMrPhoenixProtocolucInteractiveRealtime;
    }

    public int getCsaSeriesMrPhoenixProtocolucInteractiveRealtime() {
        return CsaSeriesMrPhoenixProtocolucInteractiveRealtime;
    }

    public void setCsaSeriesMrPhoenixProtocolucInternalTablePosValid(final int CsaSeriesMrPhoenixProtocolucInternalTablePosValid) {
        this.CsaSeriesMrPhoenixProtocolucInternalTablePosValid = CsaSeriesMrPhoenixProtocolucInternalTablePosValid;
    }

    public int getCsaSeriesMrPhoenixProtocolucInternalTablePosValid() {
        return CsaSeriesMrPhoenixProtocolucInternalTablePosValid;
    }

    public void setCsaSeriesMrPhoenixProtocolsParametricMappingucParametricMap(final int CsaSeriesMrPhoenixProtocolsParametricMappingucParametricMap) {
        this.CsaSeriesMrPhoenixProtocolsParametricMappingucParametricMap = CsaSeriesMrPhoenixProtocolsParametricMappingucParametricMap;
    }

    public int getCsaSeriesMrPhoenixProtocolsParametricMappingucParametricMap() {
        return CsaSeriesMrPhoenixProtocolsParametricMappingucParametricMap;
    }

    public void setCsaSeriesMrPhoenixProtocolsAslulMode(final int CsaSeriesMrPhoenixProtocolsAslulMode) {
        this.CsaSeriesMrPhoenixProtocolsAslulMode = CsaSeriesMrPhoenixProtocolsAslulMode;
    }

    public int getCsaSeriesMrPhoenixProtocolsAslulMode() {
        return CsaSeriesMrPhoenixProtocolsAslulMode;
    }

    public void setCsaSeriesMrPhoenixProtocolWaitForUserStart(final int CsaSeriesMrPhoenixProtocolWaitForUserStart) {
        this.CsaSeriesMrPhoenixProtocolWaitForUserStart = CsaSeriesMrPhoenixProtocolWaitForUserStart;
    }

    public int getCsaSeriesMrPhoenixProtocolWaitForUserStart() {
        return CsaSeriesMrPhoenixProtocolWaitForUserStart;
    }

    public void setCsaSeriesMrPhoenixProtocolucAutoAlignInit(final int CsaSeriesMrPhoenixProtocolucAutoAlignInit) {
        this.CsaSeriesMrPhoenixProtocolucAutoAlignInit = CsaSeriesMrPhoenixProtocolucAutoAlignInit;
    }

    public int getCsaSeriesMrPhoenixProtocolucAutoAlignInit() {
        return CsaSeriesMrPhoenixProtocolucAutoAlignInit;
    }

    public void setAcquisitionTime(final double AcquisitionTime[]) {
        this.AcquisitionTime = AcquisitionTime;
    }

    public double[] getAcquisitionTime() {
        return AcquisitionTime;
    }

    public void setAcquisitionNumber(final int AcquisitionNumber[]) {
        this.AcquisitionNumber = AcquisitionNumber;
    }

    public int[] getAcquisitionNumber() {
        return AcquisitionNumber;
    }

    public void setInstanceNumber(final int InstanceNumber[]) {
        this.InstanceNumber = InstanceNumber;
    }

    public int[] getInstanceNumber() {
        return InstanceNumber;
    }
    
    public void setWindowCenter(double WindowCenter[]) {
    	this.WindowCenter = WindowCenter;
    }
    
    public double[] getWindowCenter() {
    	return WindowCenter;
    }
    
    public void setWindowWidth(double WindowWidth[]) {
    	this.WindowWidth = WindowWidth;
    }
    
    public double[] getWindowWidth() {
    	return WindowWidth;
    } 

    public void setCsaImageTimeAfterStart(final double CsaImageTimeAfterStart[]) {
        this.CsaImageTimeAfterStart = CsaImageTimeAfterStart;
    }

    public double[] getCsaImageTimeAfterStart() {
        return CsaImageTimeAfterStart;
    }

    public void setCsaImageMosaicRefAcqTimes(final double CsaImageMosaicRefAcqTimes[][]) {
        this.CsaImageMosaicRefAcqTimes = CsaImageMosaicRefAcqTimes;
    }

    public double[][] getCsaImageMosaicRefAcqTimes() {
        return CsaImageMosaicRefAcqTimes;
    }

    public void setCsaImageICE_Dims(final String CsaImageICE_Dims[]) {
        this.CsaImageICE_Dims = CsaImageICE_Dims;
    }

    public String[] getCsaImageICE_Dims() {
        return CsaImageICE_Dims;
    }

    public void setCsaImageSliceMeasurementDuration(final double CsaImageSliceMeasurementDuration[]) {
        this.CsaImageSliceMeasurementDuration = CsaImageSliceMeasurementDuration;
    }

    public double[] getCsaImageSliceMeasurementDuration() {
        return CsaImageSliceMeasurementDuration;
    }

    public void setInstanceCreationTime(final double InstanceCreationTime[]) {
        this.InstanceCreationTime = InstanceCreationTime;
    }

    public double[] getInstanceCreationTime() {
        return InstanceCreationTime;
    }
    
    public void setContentTime(double ContentTime[]) {
    	this.ContentTime = ContentTime;
    }
    
    public double[] getContentTime() {
    	return ContentTime;
    }
    
    public void setDcmmeta_shape(int dcmmeta_shape[]) {
    	this.dcmmeta_shape = dcmmeta_shape;
    }

    public int[] getDcmmeta_shape() {
        return dcmmeta_shape;
    }

    public void setDcmmeta_affine(final double dcmmeta_affine[][]) {
        this.dcmmeta_affine = dcmmeta_affine;
    }

    public double[][] getDcmmeta_affine() {
        return dcmmeta_affine;
    }

    public void setDcmmeta_reorient_transform(final double dcmmeta_reorient_transform[][]) {
        this.dcmmeta_reorient_transform = dcmmeta_reorient_transform;
    }

    public double[][] getDcmmeta_reorient_transform() {
        return dcmmeta_reorient_transform;
    }

    public void setDcmmeta_slice_dim(final int dcmmeta_slice_dim) {
        this.dcmmeta_slice_dim = dcmmeta_slice_dim;
    }

    public int getDcmmeta_slice_dim() {
        return dcmmeta_slice_dim;
    }

    public void setDcmmeta_version(final double dcmmeta_version) {
        this.dcmmeta_version = dcmmeta_version;
    }

    public double getDcmmeta_version() {
        return dcmmeta_version;
    }
}
