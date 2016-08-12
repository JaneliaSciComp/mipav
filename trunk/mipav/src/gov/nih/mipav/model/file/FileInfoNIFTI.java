package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.dialogs.*;

import java.util.Vector;


/**
 * This structures contains the information that describes how a NIFTI image is stored on disk. NIFTI is intended to be
 * "mostly compatible" with the ANALYZE 7.5 file format. Most of the "unused" fields in that format have been taken, and
 * some of the lesser-used fields have been co-opted for other purposes. We have extended this format to store image
 * orientation and the origin. We have used unused variables to store these data. Almost all programs ignore these
 * variables and should not have any problems reading images saved with MIPAV, except SPM. A new format for MIPAV is now
 * XML based.
 *
 * <p>RGB NIFTI images are store in chunky format rgb, rgb, rgb ......</p>
 *
 * <p>Note that there is a short datatype field.</p>
 *
 * @see  FileNIFTI
 */

public class FileInfoNIFTI extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

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
     * To signify that the vector value at each location is an RGB triplet, of whatever type.
     * - dataset must have a 5th dimension 
     * - dim[0] = 5
     * - dim[1] = number of nodes
     * - dim[2] = dim[3] = dim[4] = 1
     * - dim[5] = 3
     */
    public static final short NIFTI_INTENT_RGB_VECTOR = 2003;
    
    /**
     * To signify that the vector value at each location is a 4 valued RGBA vector, of whatever type.
     * - dataset must have a 5th dimension
     * - dim[0] = 5
     * - dim[1] = number of nodes
     * - dim[2] = dim[3] = dim[4] = 1
     * - dim[5] = 4
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

    /** Alternating increasing pattern of slice acquisition
     *  Slice timing starts at slice_start */
    public static final byte NIFTI_SLICE_ALT_INC = 3;

    /** Alternating decreasing pattern of slice acquisition
     *  slice timing starts at slice_end */
    public static final byte NIFTI_SLICE_ALT_DEC = 4;

    /** Alternating increasing pattern of slice acquisition # 2
     * Slice timing starts at slice_start + 1 */
    public static final byte NIFTI_SLICE_ALT_INC2 = 5;

    /** Alternating decreasing pattern of slice acquisition # 2
     * Slice timing starts at slice_end - 1 */
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
    
    /** MIND is an acronym for NIFTI for DWI (diffusion-weighted images)
     * The 5 MIND extensions to the NIFTI-1.1 header provide a standard
     * specification for data sharing and interchange for diffusion-weighted
     * MRI datasets at various stages of processing.
     */
    
    /** The contents of a MIND_IDENT field are character data which serve to
     * identify the type of DWI data structure represented by the MIND extended
     * header fields whcih follow.
     */
    private static final int NIFTI_ECODE_MIND_IDENT = 18;
    
    
    /** A B_Value field contains a single 32-bit floating point value representing a
     * diffusion-weighing b-value in units of s/mm-squared.  In the q-space formalism,
     * the b-value is the square of the magnitude of the diffusion wavevector q.
     */
    private static final int NIFTI_ECODE_B_VALUE = 20;
    
    /**
     * A SPHERICAL_DIRECTION field contains two 32-bit floating point values which
     * represent a direction in spherical coordinates.  The azimuthal angle(longitude) is 
     * represented first, in radians, followed by the zenith angle(polar angle, elevation angle, or 
     * colatitude), in radians.  In the mathematics convention, the ordering is denoted
     * (theta, phi); in the physics convention, the notation is reversed,
     * (phi, theta).  A radial coordinate is omitted as this field specifies 
     * direction only, not position.
     */
    private static final int NIFTI_ECODE_SPHERICAL_DIRECTION = 22;
    
    /**
     * The contents of a DT_COMPONENT field are a set of 32-bit integers which specify the
     * indices of a single diffusion tensor component.  The number of integers corresponds
     * to the order of the tensor: e.g. a 2nd order tensor component Dij has 2 integer 
     * indices, while a 4th order tensor component Dijkl has 4 indices.  The integers are
     * given in the indexing order: i.e. i before j before k before l, etc.  Furthermore,
     * the indices are 1-based, so that D11 represents the upper-left element of a 2nd
     * order diffusion tensor.
     */
    private static final int NIFTI_ECODE_DT_COMPONENT = 24;
    
    /**
     * The SHC_DEGREEORDER field specifies the degree (l) and order (m) of a spherical
     * harmonic basis function as a pair of 32-bit integers, with the degree preceding
     * the order.  m can take values between -l and +l, inclusive. 
     */
    private static final int NIFTI_ECODE_SHC_DEGREEORDER = 26;
    
    private static final int NIFTI_ECODE_CARET = 30;
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** auxiliary file */
    private String aux_file = null;

    /** Bits per pixel */
    private short bitpix = -1;

    /** The cal_min and cal_max fields (if nonzero) are used for mapping (possibly
        scaled) dataset values to display colors:
        - Minimum display intensity (black) corresponds to dataset value cal_min.
        - Maximum display intensity (white) corresponds to dataset value cal_max.
        - Dataset values below cal_min should display as black also, and values
        above cal_max as white.
        - Colors "black" and "white", of course, may refer to any scalar display
        scheme (e.g., a color lookup table specified via aux_file).
        - cal_min and cal_max only make sense when applied to scalar-valued
        datasets (i.e., dim[0] < 5 or dim[5] = 1). */
    private float cal_max = 0;

    /** values of 0.0 for both fields imply that no calibration min and max values are used ! */
    private float cal_min = 0;

    /** If qform_code > 0, coord_code = qform_code.  If qform_code <= 0 and sform_code > 0, coord_code = sform_code.
     *  coord_code has values for "Arbitrary X,Y,Z coordinate system", "Scanner based anatomical coordinates",
     *  "Coordinates aligned to another file's or to anatomical truth", "Talairach X,Y,Z coordinate system", and
     *  "MNI 152 normalized X,Y,Z coordinates".   */
    private short coord_code = 0;
    
    /** If qform_code > 0 and sform_code > 0, coord_code = qform_code and coord_code2 = sform_code.
     *  coord_code has values for "Arbitrary X,Y,Z coordinate system", "Scanner based anatomical coordinates",
     *  "Coordinates aligned to another file's or to anatomical truth", "Talairach X,Y,Z coordinate system", and
     *  "MNI 152 normalized X,Y,Z coordinates".   */
    private short coord_code2 = 0;

    /** Any text you like */
    private String descrip = null;

    /** Bits 0 and 1 of the dim_info character contain the freq_dim information.
     *  0 for "No frequency encoding direction is present", 1 for "Frequency encoding in the x direction",
     *  2 for "Frequency encoding in the y direction", and 3 for "Frequency encoding in the z direction". */
    private int freq_dim = 0;

    /** The intent_code field can be used to indicate that the voxel data has
        some particular meaning.  In particular, a large number of codes is
        given to indicate that the the voxel data should be interpreted as
        being drawn from a given probability distribution. */
    private short intentCode = 0;

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
    private String intentName = null;

    /**
     * If present, first auxiliary parameter used with intentCode
     */
    private float intentP1;

    /** If present, second auxiliary parameter used with intentCode */
    private float intentP2;

    /** If present, third auxiliary parameter used with intentCode */
    private float intentP3;

    /** Bits 2 and 3 of the dim_info character contain the phase_dim information.
     *  0 for "No phase encoding direction is present", 1 for "Phase encoding in the x direction",
     *  2 for "Phase encoding in the y direction", and 3 for "Phase encoding in the z direction". */
    private int phase_dim = 0;

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
    private float scl_slope = 1.0f;
    private float scl_inter = 0.0f;

    /** Should always be a length of 348. */
    private int sizeof_hdr = -1;

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
    private float vox_offset = -1;
    
    // The number of bytes in the header extension including esize and ecode themselves
    // esize must be a positive integral multiple of 16
    private int esize[] = null;
    
    // ecode is a non-negative integer integer value that indicates the format of the extended header data that
    // follows.  Different ecode values are assigned to different developer groups.  At present, the
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
    // DICOM:0008_0050 		Imaging data attribute 	DICOM term 	SH
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
    
    // Angio Image Indicator. Primary image for Angio processing. Enumerated Values: Y = Image is Angio N = Image is not Angio
    // DICOM:0018_0025 		Imaging protocol attribute 	DICOM term 	CS
    private String angioFlag = null;
    
    // The period of time in msec between the beginning of a pulse sequence and the beginning of the 
    // succeeding (essentially identical) pulse sequence. Required except when Scanning Sequence (0018,0020) is EP and 
    // Sequence Variant (0018,0021) is not SK.
    // DICOM:0018_0080 DS
    private double repetitionTime = Double.NaN;
    
    // Time in ms between the middle of the excitation pulse and the peak of the echo produced (kx=0). 
    // In the case of segmented k-space, the TE(eff) is the time between the middle of the excitation pulse to
    //the peak of the echo that is used to cover the center of the segment. 	
    // DICOM:0018_0081 		Imaging protocol attribute 	DICOM term 	DS
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
    // DICOM:0018_0086 		Imaging protocol attribute 	DICOM term 	IS
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
    // DICOM:0018_0091 		Imaging protocol attribute 	DICOM term 	IS
    private int echoTrainLength = Integer.MIN_VALUE;
    
    // Fraction of acquisition matrix lines acquired, expressed as a percent.
    // DICOM:0018_0093 DS
    private double percentSampling = Double.NaN;
    
    // Ratio of field of view dimension in phase direction to field of view dimension in frequency direction, expressed as a percent.
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
    // DICOM:0018_1310 		Imaging protocol attribute 	DICOM term 	US
    private int acquisitionMatrix[] =  null;
    
    // The axis of phase encoding with respect to the image. Enumerated Values: ROW = phase encoded in rows. 
    // COL = phase encoded in columns.
    // DICOM:0018_1312 CS
    private String inPlanePhaseEncodingDirection = null;
    
    // Steady state angle in degrees to which the magnetic vector is flipped from the magnetic vector of the primary field.
    // DICOM:0018_1314 		Imaging protocol attribute 	DICOM term 	DS
    private double flipAngle = Double.NaN;
    
    // Flip angle variation applied during image acquisition.
    // DICOM:0018_1315 CS
    private String variableFlipAngleFlag = null;
    
    // Calculated whole body Specific Absorption Rate in watts/kilogram.
    // DICOM:0018_1316 DS
    private double SAR = Double.NaN;
    
    // The rate of change of the gradient coil magnetic flux density with time (T/s). 	
    // DICOM:0018_1318 		Imaging hardware attribute 	DICOM term 	DS
    private double dBdt = Double.NaN;
    
    // A number that identifies this Series.
    // DICOM:0020_0011 IS
    private int seriesNumber = Integer.MIN_VALUE;
    
    // The x, y, and z coordinates of the upper left hand corner (center of the first voxel transmitted) of the image, in mm.
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
    // See PS 3.5 for further explanation. 	DICOM:0028_0100 		Imaging data attribute 	DICOM term 	US
    private int bitsAllocated = Integer.MIN_VALUE;
    
    // Number of bits stored for each pixel sample. Each sample shall have the same number of bits stored. 
    // See PS 3.5 for further explanation. 	DICOM:0028_0101 		Imaging data attribute 	DICOM term 	US
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
    
    private int CsaImageEchoLinePosition = Integer.MIN_VALUE;
    
    private int CsaImageProtocolSliceNumber = Integer.MIN_VALUE;
    
    private int CsaImageUsedChannelMask = Integer.MIN_VALUE;
    
    private double CsaImageBandwidthPerPixelPhaseEncode = Double.NaN;
    
    private int CsaImageMeasuredFourierLines = Integer.MIN_VALUE;
    
    private int CsaImageSequenceMask = Integer.MIN_VALUE;
    
    private String CsaImageRFSWDDataType = null;
    
    private String CsaImageImaPATModeText = null;
    
    private int CsaImageRealDwellTime= Integer.MIN_VALUE;
    
    private String CsaImageImaCoilString = null;
    
    private int CsaImageEchoColumnPosition = Integer.MIN_VALUE;
    
    private int CsaImagePhaseEncodingDirectionPositive = Integer.MIN_VALUE;
    
    private double CsaImageSlicePosition_PCS[] = null;
    
    private double CsaImageSliceNormalVector[] = null;
    
    private String CsaImageGSWDDataType = null;
    
    private int CsaImageMultistepIndex = Integer.MIN_VALUE;
    
    private int CsaImageImaRelTablePosition[] = null;
    
    private int CsaImageNumberOfImagesInMosaic = Integer.MIN_VALUE;
    
    private int CsaImageNonPlanarImage = Integer.MIN_VALUE;
    
    private int CsaImageEchoPartitionPosition = Integer.MIN_VALUE;
    
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

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * file info storage constructor.
     *
     * @param  name       file name
     * @param  directory  directory
     * @param  format     file format
     */
    public FileInfoNIFTI(String name, String directory, int format) {
        super(name, directory, format);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Displays the file information.
     *
     * @param  dlog    dialog box that is written to
     * @param  matrix  transformation matrix
     */
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix) {
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
        JDialogText dialog = (JDialogText) dlog;
        displayPrimaryInfo(dialog, matrix);
        dialog.append("\n\n                Other information\n\n");

        
        try {
            dialog.append("Description = " + descrip.trim() + "\n");
        } catch (NullPointerException npe) {
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
                dialog.append("Degrees of freedom = " +  Integer.toString(Math.round(intentP1)) + "\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_TTEST:
                dialog.append("Statistics code = Student t test\n");
                dialog.append("Degrees of freedom = " +  Integer.toString(Math.round(intentP1)) + "\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_FTEST:
                dialog.append("Statistics code = Fisher F statistic\n");
                dialog.append("Numerator degrees of freedom = " +  Integer.toString(Math.round(intentP1)) + "\n");
                dialog.append("Denominator degrees of freedom = " + Integer.toString(Math.round(intentP2)) + "\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_ZSCORE:
                dialog.append("Statistics code = Standard normal - N(0,1) distributed\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_CHISQ:
                dialog.append("Statistics code = Chi - squared\n");
                dialog.append("Degrees of freedom = " +  Integer.toString(Math.round(intentP1)) + "\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_BETA:
                dialog.append("Statistics code = Beta distribution\n");
                dialog.append("a parameter = " + Float.toString(intentP1) + "\n");
                dialog.append("b parameter = " + Float.toString(intentP2) + "\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_BINOM:
                dialog.append("Statistics code = Binomial distribution\n");
                dialog.append("Number of trials = " +  Integer.toString(Math.round(intentP1)) + "\n");
                dialog.append("Probability per trial = " +  Float.toString(intentP2) + "\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_GAMMA:
                dialog.append("Statistics code = Gamma with PDF = x**(shape-1) * exp(-Scale*x)\n");
                dialog.append("Shape = " +  Float.toString(intentP1) + "\n");
                dialog.append("Scale = " + Float.toString(intentP2) + "\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_POISSON:
                dialog.append("Statistics code = Poisson distribution\n");
                dialog.append("Mean = " +  Float.toString(intentP1) + "\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_NORMAL:
                dialog.append("Statistics code = Normal distribution\n");
                dialog.append("Mean = " +  Float.toString(intentP1) + "\n");
                dialog.append("Standard deviation = " + Float.toString(intentP2) + "\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_FTEST_NONC:
                dialog.append("Statistics code = Nocentral F statistic\n");
                dialog.append("Numerator degrees of freedom = " +  Integer.toString(Math.round(intentP1)) + "\n");
                dialog.append("Denominator degrees of freedom = " + Integer.toString(Math.round(intentP2)) + "\n");
                dialog.append("Numerator noncentrality parameter = " +  Float.toString(intentP3) + "\n");
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
                dialog.append("Start = " +  Float.toString(intentP1) + "\n");
                dialog.append("End = " +  Float.toString(intentP2) + "\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_TTEST_NONC:
                dialog.append("Statistics code = Nocentral t statistic\n");
                dialog.append("Degrees of freedom = " +  Integer.toString(Math.round(intentP1)) + "\n");
                dialog.append("Noncentrality parameter = " +  Float.toString(intentP2) + "\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_WEIBULL:
                dialog.append("Statistics code = Weibull distribution\n");
                dialog.append("Location = " + Float.toString(intentP1) + "\n");
                dialog.append("Scale = " + Float.toString(intentP2) + "\n");
                dialog.append("Power = " +  Float.toString(intentP3) + "\n");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_CHI:
                dialog.append("Statistics code = Chi distribution\n");

                int p1 = Math.round(intentP1);
                dialog.append("Degrees of freedom = " +  Integer.toString(p1) + "\n");
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
                dialog.append("Mu = " +  Float.toString(intentP1) + "\n");
                dialog.append("Lambda = " +  Float.toString(intentP2) + "\n");
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

        dialog.append("X,Y,Z Coordinate system = " +  coordString + "\n");
        
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

        dialog.append("Added offset = " +  Float.toString(scl_inter) + "\n");

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


        dialog.append("X Origin: " +  Float.toString(super.getOrigin(0)) + "\n");
        dialog.append("Y Origin: " +  Float.toString(super.getOrigin(1)) + "\n");
        dialog.append("Z Origin: " +  Float.toString(super.getOrigin(2)) + "\n");

        if (cal_min != -1) {
            dialog.append("cal_min = " +  Float.toString(cal_min) + "\n");
        }

        if (cal_max != -1) {
            dialog.append("cal_max = " +  Float.toString(cal_max) + "\n");
        }

        if (bitpix != -1) {
            dialog.append("Bits per Pixel = " +  Integer.toString(bitpix) + "\n");
        }

        if (aux_file != null) {

            if (aux_file.trim().length() > 0) {
                dialog.append("aux = " + aux_file.trim() + "\n");
            }
        }

        if (intentName != null) {
            dialog.append("Name or meaning of data = " + intentName + "\n");
        }
        
        if ((esize != null) && (ecode != null)) {
        	mindIdentIndex = 0;
        	bValueIndex = 0;
        	sphericalDirectionIndex = 0;
        	dtComponentIndex = 0;
        	afniGroupIndex = 0;
        	asciiTextIndex = 0;
        	caretIndex = 0;
        	if (esize.length == 1) {
        		dialog.append("Extended header has " + esize.length + " header field\n");
        	}
        	else {
               dialog.append("Extended header has " + esize.length + " header fields\n");
        	}
            for (i = 0; i < esize.length; i++) {
            	//dialog.append("Header field number " + (i+1) + " size in bytes = " + esize[i] + "\n");
            	dialog.append("Header field number " + (i+1) + " has " + ecodeIntToString(ecode[i]) + "\n");
            	switch(ecode[i]) {
            	case NIFTI_ECODE_AFNI:
            		dialog.append("AFNI GROUP field number " + (afniGroupIndex+1) + " has:\n");
            		dialog.append(afniGroup[afniGroupIndex++].trim() + "\n\n");
            		break;
            	case NIFTI_ECODE_COMMENT:
            		dialog.append("ASCII TEXT field number " + (asciiTextIndex+1) + " has:\n");
            		dialog.append(asciiText[asciiTextIndex++].trim() + "\n\n");
            		break;
            	case NIFTI_ECODE_MIND_IDENT:
            		dialog.append("MIND_IDENT field number " + (mindIdentIndex+1) + " = " + mindIdent[mindIdentIndex].trim() + "\n");
            		mindIdentIndex++;
            		break;
            	case NIFTI_ECODE_B_VALUE:
            		dialog.append("B_VALUE field number " + (bValueIndex+1) + " = " + bValue[bValueIndex] + " s/(mm*mm)\n");
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
        	        	dialog.append("Component index " + (j+1) + " = " + dtComponent[dtComponentIndex][j] + "\n");
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
        		dialog.append("CsaImage.MeasuredSequenceMask = " + CsaImageSequenceMask + "\n");
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
        } // if (haveJson)
        
        
        if (matrixQ != null) {
            dialog.append("Qform Matrix = \n" + matrixQ.matrixToString(10, 4));
        }
        
        if (matrixS != null) {
            dialog.append("Sform Matrix = \n" + matrixS.matrixToString(10, 4));
        }

    }
    
    private String ecodeIntToString(int ecode) {
        String ecodeStr = null;
        switch(ecode) {
            case 0:
            	if (haveDcmMeta) {
            		ecodeStr = "DcmMeta encoded with JSON";
            	}
            	else {
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
     * @return  String aux_file
     */
    public String getAuxFile() {
        return aux_file;
    }

    /**
     * accessor to the bitpix value.
     *
     * @return  short the bitpix value.
     */
    public short getBitPix() {
        return bitpix;
    }

    /**
     * accessor to cal-max.
     *
     * @return  float cal_max
     */
    public float getCalMax() {
        return cal_max;
    }

    /**
     * accessor to cal-min.
     *
     * @return  float cal_min
     */
    public float getCalMin() {
        return cal_min;
    }

    /**
     * Returns type of x, y, z coordinates.
     *
     * @return  coord_code
     */
    public short getCoordCode() {
        return coord_code;
    }
    
    /**
     * Returns type of x, y, z coordinates 2.
     *
     * @return  coord_code2
     */
    public short getCoordCode2() {
        return coord_code2;
    }

    /**
     * accessor to the current analyze-image description.
     *
     * @return  String description
     */
    public String getDescription() {
        return descrip;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getFreqDim() {
        return freq_dim;
    }

    /**
     * Accessor that returns the intent code.
     *
     * @return  intentCode
     */
    public short getIntentCode() {
        return intentCode;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getIntentName() {
        return intentName;
    }

    /**
     * Accessor that returns first statistical parameter.
     *
     * @return  intentP1
     */
    public float getIntentP1() {
        return intentP1;
    }

    /**
     * Accessor that returns second statistical parameter.
     *
     * @return  intentP2
     */
    public float getIntentP2() {
        return intentP2;
    }

    /**
     * Accessor that returns third statistical parameter.
     *
     * @return  intentP3
     */
    public float getIntentP3() {
        return intentP3;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getPhaseDim() {
        return phase_dim;
    }

    /**
     * Gets the data additive factor.
     *
     * @return  scl_inter
     */
    public float getSclInter() {
        return scl_inter;
    }

    /**
     * Gets the data scaling multiplicative factor.
     *
     * @return  scl_slope
     */
    public float getSclSlope() {
        return scl_slope;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getSizeOfHeader() {
        return sizeof_hdr;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public byte getSliceCode() {
        return sliceCode;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getSliceDim() {
        return slice_dim;
    }

    /**
     * provides the sliceDuration value.
     *
     * @return  float sliceDuration
     */
    public float getSliceDuration() {
        return sliceDuration;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public short getSliceEnd() {
        return sliceEnd;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public short getSliceStart() {
        return sliceStart;
    }

    /**
     * accessor to the sourceBitPix value.
     *
     * @return  short the sourceBitPix value.
     */
    public short getSourceBitPix() {
        return sourceBitPix;
    }

    /**
     * accessor to coded datatype value.
     *
     * @return  short datatype
     */
    public short getSourceType() {
        return sourceType;
    }

    /**
     * accessor to the vox offset value.
     *
     * @return  float vox_offset
     */
    public float getVoxOffset() {
        return vox_offset;
    }
    
    /**
     * Return String put in DICOM 0020,0037 field
     * @return
     */
    public String getPatientOrientationString() {
    	return patientOrientationString;
    }

    /**
     * supplies auxiliary-file string; permits no more than 24 characters.
     *
     * @param  aux  DOCUMENT ME!
     */
    public void setAuxFile(String aux) {
        aux_file = setString(aux, 24);
    }

    /**
     * sets bitpix; any value other than 1, 8, 16, 32, 64, 128, or 24 gets set to the dissalowed trap value, -1.
     *
     * @param  bp  DOCUMENT ME!
     */
    public void setBitPix(short bp) {

        if ((bp == 1) || (bp == 8) || (bp == 16) || (bp == 32) || (bp == 64) || (bp == 128) || (bp == 24)) {
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
     * sets cal-min. if supplied value is greater than cal-max, the cal-max gets reset to the supplied value as well, so
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
     * Sets type of xyz coordinates.
     *
     * @param  coord_code  DOCUMENT ME!
     */
    public void setCoordCode(short coord_code) {
        this.coord_code = coord_code;
    }
    
    /**
     * Sets type of xyz coordinates 2.
     *
     * @param  coord_code2  DOCUMENT ME!
     */
    public void setCoordCode2(short coord_code2) {
        this.coord_code2 = coord_code2;
    }


    /**
     * allows no more than 80 characters to fill in the analyze-image description.
     *
     * @param  description  DOCUMENT ME!
     */
    public void setDescription(String description) {
        descrip = setString(description, 80);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  freq_dim  DOCUMENT ME!
     */
    public void setFreqDim(int freq_dim) {
        this.freq_dim = freq_dim;
    }

    /**
     * Accessor that sets the stat code.
     *
     * @param  intentCode  DOCUMENT ME!
     */
    public void setIntentCode(short intentCode) {
        this.intentCode = intentCode;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  intentName  DOCUMENT ME!
     */
    public void setIntentName(String intentName) {
        this.intentName = intentName;
    }


    /**
     * Accessor that sets first statistical parameter.
     *
     * @param  intentP1  DOCUMENT ME!
     */
    public void setIntentP1(float intentP1) {
        this.intentP1 = intentP1;
    }

    /**
     * Accessor that sets second statistical parameter.
     *
     * @param  intentP2  DOCUMENT ME!
     */
    public void setIntentP2(float intentP2) {
        this.intentP2 = intentP2;
    }

    /**
     * Accessor that sets third statistical parameter.
     *
     * @param  intentP3  DOCUMENT ME!
     */
    public void setIntentP3(float intentP3) {
        this.intentP3 = intentP3;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  phase_dim  DOCUMENT ME!
     */
    public void setPhaseDim(int phase_dim) {
        this.phase_dim = phase_dim;
    }

    /**
     * Sets the data additive factor.
     *
     * @param  scl_inter  DOCUMENT ME!
     */
    public void setSclInter(float scl_inter) {
        this.scl_inter = scl_inter;
    }

    /**
     * Sets the data scaling multiplicative factor.
     *
     * @param  scl_slope  DOCUMENT ME!
     */
    public void setSclSlope(float scl_slope) {
        this.scl_slope = scl_slope;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  size  DOCUMENT ME!
     */
    public void setSizeOfHeader(int size) {
        sizeof_hdr = size;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  sliceCode  DOCUMENT ME!
     */
    public void setSliceCode(byte sliceCode) {
        this.sliceCode = sliceCode;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  slice_dim  DOCUMENT ME!
     */
    public void setSliceDim(int slice_dim) {
        this.slice_dim = slice_dim;
    }

    /**
     * sets the sliceDuration variable.
     *
     * @param  sliceDuration  DOCUMENT ME!
     */
    public void setSliceDuration(float sliceDuration) {
        this.sliceDuration = sliceDuration;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  sliceEnd  DOCUMENT ME!
     */
    public void setSliceEnd(short sliceEnd) {
        this.sliceEnd = sliceEnd;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  sliceStart  DOCUMENT ME!
     */
    public void setSliceStart(short sliceStart) {
        this.sliceStart = sliceStart;
    }

    /**
     * sets sourceBitPix; any value other than 1, 8, 16, 32, 64, 128, or 24 gets set to the disallowed trap value, -1.
     *
     * @param  bp  DOCUMENT ME!
     */
    public void setSourceBitPix(short bp) {

        if ((bp == 1) || (bp == 8) || (bp == 16) || (bp == 32) || (bp == 64) || (bp == 128) || (bp == 24)) {
            sourceBitPix = bp;
        } else {
            sourceBitPix = -1;
        } // a disallowed trap value
    }

    /**
     * accessor to supply coded datatype.
     *
     * @param  dtype  DOCUMENT ME!
     */
    // Data type before conversion for scl_slope and scl_offset
    public void setSourceType(short dtype) {
        sourceType = dtype;
    }

    /**
     * sets vox offset value.
     *
     * @param  vox  DOCUMENT ME!
     */
    public void setVoxOffset(float vox) {
        vox_offset = vox;
    }
    
    /**
     * Sets esize array, the number of bytes in each header field in the header extension
     * @param esize
     */
    public void setEsize(int esize[]) {
        this.esize = esize;
    }
    
    
    
    public int[] getEsize() {
		return esize;
	}

	/**
     * Sets ecode array, the data format of the header field in the header extension
     * @param ecode
     */
    public void setEcode(int ecode[]) {
        this.ecode = ecode;
    }
    
    /**
     * Sets mindIdent array, character data which serve to identify the type of
     * DWI data structure represented by the MIND extended header fields which follow
     * @param mindIdent
     */
    public void setMindIdent(String mindIdent[]) {
    	this.mindIdent = mindIdent;
    }
    
    /**
     * Sets floating point array with diffusion-weighting b-values in units of s/mm-squared.
     * @param bValue
     */
    public void setBValue(float bValue[]) {
    	this.bValue = bValue;
    }
    
    /** 
     * Sets azimuthal angle array for spherical direction
     * @param azimuth
     */
    public void setAzimuth(float azimuth[]) {
    	this.azimuth = azimuth;
    }
    
    /**
     * Sets zenith angle array for spherical direction
     * @param zenith
     */
    public void setZenith(float zenith[]) {
    	this.zenith = zenith;
    }
    
    /**
     * Sets degree array for set of spherical harmonic basis functions
     * @param degree
     */
    public void setDegree(int degree[]) {
    	this.degree = degree;
    }
    
    /**
     * Sets order array for set of spherical harmonic basis functions
     * @param order
     */
    public void setOrder(int order[]) {
    	this.order = order;
    }
    
    /**
     * Sets array of afni group xml inclusions
     * @param afniGroup
     */
    public void setAfniGroup(String afniGroup[]) {
    	this.afniGroup = afniGroup;
    }
    
    /**
     * Sets ascii text fields of header extension
     * @param asciiText
     */
    public void setAsciiText(String asciiText[]) {
    	this.asciiText = asciiText;
    }
    
    /**
     * Sets dt component array
     * @param dtComponent
     */
    public void setDTComponent(int dtComponent[][]) {
    	this.dtComponent = dtComponent;
    }
    
    
    /**
     * Sets caret array for extension header
     * @param caret
     */
    public void setCaret(String caret[]) {
    	this.caret = caret;
    }
    
    public void setMatrixQ(TransMatrix matrixQ) {
        this.matrixQ = matrixQ;
    }
    
    public void setMatrixS(TransMatrix matrixS) {
        this.matrixS = matrixS;
    }
    
    /**
     * Set String put in DICOM 0020, 0037
     * @param patientOrientationString
     */
    public void setPatientOrientationString(String patientOrientationString) {
    	this.patientOrientationString = patientOrientationString;
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
     * "ce" comes from ChangeEvent upon which this is based. care to make our own ChangeEvent to store and handle this?
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
        } else if (tname.equalsIgnoreCase("voxel offset")) {
            setVoxOffset(Float.parseFloat((String) tcvalue.elementAt(0)));
        } else if (tname.equalsIgnoreCase("cal_min")) {
            setCalMin(Float.parseFloat((String) tcvalue.elementAt(0)));
        } else if (tname.equalsIgnoreCase("cal_max")) {
            setCalMax(Float.parseFloat((String) tcvalue.elementAt(0)));
        } else if (tname.equalsIgnoreCase("Orientation")) {
            super.setImageOrientation(((Integer) tcvalue.elementAt(0)).intValue());
            // setImageOrientation(((Byte) tcvalue.elementAt(0)).byteValue());
        } else if (tname.startsWith("Axis: x-orientation")) {
            super.setAxisOrientation(((Integer) tcvalue.elementAt(0)).intValue(), 0);
        } else if (tname.startsWith("Axis: y-orientation")) {
            super.setAxisOrientation(((Integer) tcvalue.elementAt(0)).intValue(), 1);
        } else if (tname.startsWith("Axis: z-orientation")) {
            super.setAxisOrientation(((Integer) tcvalue.elementAt(0)).intValue(), 2);
        } else if (tname.startsWith("Start Location: x-axis")) {
            super.setOrigin(Float.parseFloat((String) tcvalue.elementAt(0)), 0);

        } else if (tname.startsWith("Start Location: y-axis")) {
            super.setOrigin(Float.parseFloat((String) tcvalue.elementAt(0)), 1);
        } else if (tname.startsWith("Start Location: z-axis")) {
            super.setOrigin(Float.parseFloat((String) tcvalue.elementAt(0)), 2);
        } else if (tname.equalsIgnoreCase("Orientation")) {
            setImageOrientation(((Integer) tcvalue.elementAt(0)).intValue());
            // setOrientation(((Byte)tcvalue.elementAt(0)).byteValue());

        } else {
            Preferences.debug("tname: " + tname + ", not found.", Preferences.DEBUG_FILEIO);
        }
    }


    /**
     * Propogates the current file info to another FileInfoNIFTI.
     *
     * <p>It does not copy over the datatypeCode. (though, aside from, "it isn't in the about table", I can't think of a
     * reason why it shouldn't. but it doesn't.) Also, copied over is bitPix, aux_file.</p>
     *
     * @param  fInfo  DOCUMENT ME!
     */
    public void updateFileInfos(FileInfoNIFTI fInfo) {

        if (this == fInfo) {
            return;
        }

        // fInfo.setAuxFile            (this.getAuxFile());// not editable by the table!!
        // fInfo.setBitPix             (this.getBitPix()); // not editable by the table!!
        fInfo.setIntentCode(this.getIntentCode());
        fInfo.setIntentP1(this.getIntentP1());
        fInfo.setIntentP2(this.getIntentP2());
        fInfo.setIntentP3(this.getIntentP3());
        fInfo.setCoordCode(this.getCoordCode());
        fInfo.setCoordCode2(this.getCoordCode2());
        fInfo.setCalMin(this.getCalMin());
        fInfo.setCalMax(this.getCalMax());
        fInfo.setSliceDuration(this.getSliceDuration());

        // fInfo.setDataTypeCode       (this.getDataTypeCode());//not edited by the table!!
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
     * @see     String#substring(int, int)
     *
     * @return  String new substring
     */
    protected String setString(String str, int len) {

        if (str.length() < len) {
            return str;
        } else {
            return str.substring(0, len);
        }
    }
    
    public void setHaveDcmMeta(boolean haveDcmMeta) {
    	this.haveDcmMeta = haveDcmMeta;
    }
  
    public boolean getHaveDcmMeta() {
    	return haveDcmMeta;
    }
    
    public void setSpecificCharacterSet(String specificCharacterSet) {
    	this.specificCharacterSet = specificCharacterSet;
    }
    
    public String getSpecificCharacterSet() {
    	return specificCharacterSet;
    }
    
    public void setImageType(String imageType[]) {
    	this.imageType = imageType;
    }
    
    public String[] getImageType() {
    	return imageType;
    }
    
    public void setStudyTime(String studyTime) {
    	this.studyTime = studyTime;
    }
    
    public String getStudyTime() {
    	return studyTime;
    }
    
    public void setSeriesTime(String seriesTime) {
    	this.seriesTime = seriesTime;
    }
    
    public String getSeriesTime() {
    	return seriesTime;
    }
    
    public void setAccessionNumber(String accessionNumber) {
    	this.accessionNumber = accessionNumber;
    }
    
    public String getAccessionNumber() {
    	return accessionNumber;
    }
    
    public void setModalityString(String modalityString) {
    	this.modalityString = modalityString;
    }
    
    public String getModalityString() {
    	return modalityString;
    }
    
    public void setManufacturer(String manufacturer) {
    	this.manufacturer = manufacturer;
    }
    
    public String getManufacturer() {
    	return manufacturer;
    }
    
    public void setManufacturerModelName(String manufacturerModelName) {
    	this.manufacturerModelName = manufacturerModelName;
    }
    
    public String getManufacturerModelName() {
    	return manufacturerModelName;
    }
    
    public void setScanningSequence(String scanningSequence) {
    	this.scanningSequence = scanningSequence;
    }
    
    public String getScanningSequence() {
    	return scanningSequence;
    }
    
    public void setSequenceVariant(String sequenceVariant) {
    	this.sequenceVariant = sequenceVariant;
    }
    
    public String getSequenceVariant() {
    	return sequenceVariant;
    }
    
    public void setScanOptions(String scanOptions) {
    	this.scanOptions = scanOptions;
    }
    
    public String getScanOptions() {
    	return scanOptions;
    }
    
    public void setMRAcquisitionType(String MRAcquisitionType) {
    	this.MRAcquisitionType = MRAcquisitionType;
    }
    
    public String getMRAcquisitionType() {
    	return MRAcquisitionType;
    }
    
    public void setSequenceName(String sequenceName) {
    	this.sequenceName = sequenceName;
    }
    
    public String getSequenceName() {
    	return sequenceName;
    }
    
    public void setAngioFlag(String angioFlag) {
    	this.angioFlag = angioFlag;
    }
    
    public String getAngioFlag() {
    	return angioFlag;
    }
    
    public void setRepetitionTime(double repetitionTime) {
    	this.repetitionTime= repetitionTime;
    }
    
    public double getRepetitionTime() {
    	return repetitionTime;
    }
    
    public void setEchoTime(double echoTime) {
    	this.echoTime = echoTime;
    }
    
    public double getEchoTime() {
    	return echoTime;
    }
    
    public void setNumberOfAverages(double numberOfAverages) {
    	this.numberOfAverages = numberOfAverages;
    }
    
    public double getNumberOfAverages() {
    	return numberOfAverages;
    }
    
    public void setImagingFrequency(double imagingFrequency) {
    	this.imagingFrequency = imagingFrequency;
    }
    
    public double getImagingFrequency() {
    	return imagingFrequency;
    }
    
    public void setImagedNucleus(String imagedNucleus) {
    	this.imagedNucleus = imagedNucleus;
    }
    
    public String getImagedNucleus() {
    	return imagedNucleus;
    }
    
    public void setEchoNumbers(int echoNumbers) {
    	this.echoNumbers = echoNumbers;
    }
    
    public int getEchoNumbers() {
    	return echoNumbers;
    }
    
    public void setMagneticFieldStrength(double magneticFieldStrength) {
    	this.magneticFieldStrength = magneticFieldStrength;
    }
    
    public double getMagneticFieldStrength() {
    	return magneticFieldStrength;
    }
    
    public void setSpacingBetweenSlices(double spacingBetweenSlices) {
    	this.spacingBetweenSlices = spacingBetweenSlices;
    }
    
    public double getSpacingBetweenSlices() {
    	return spacingBetweenSlices;
    }
    
    public void setNumberOfPhaseEncodingSteps(int numberOfPhaseEncodingSteps) {
    	this.numberOfPhaseEncodingSteps = numberOfPhaseEncodingSteps;
    }
    
    public int getNumberOfPhaseEncodingSteps() {
    	return numberOfPhaseEncodingSteps;
    }
    
    public void setEchoTrainLength(int echoTrainLength) {
    	this.echoTrainLength = echoTrainLength;
    }
    
    public int getEchoTrainLength() {
    	return echoTrainLength;
    }
    
    public void setPercentSampling(double percentSampling) {
    	this.percentSampling = percentSampling;
    }
    
    public double getPercentSampling() {
    	return percentSampling;
    }
    
    public void setPercentPhaseFieldOfView(double percentPhaseFieldOfView) {
    	this.percentPhaseFieldOfView = percentPhaseFieldOfView;
    }
    
    public double getPercentPhaseFieldOfView() {
    	return percentPhaseFieldOfView;
    }
    
    public void setPixelBandwidth(double pixelBandwidth) {
    	this.pixelBandwidth = pixelBandwidth;
    }
    
    public double getPixelBandwidth() {
    	return pixelBandwidth;
    }
    
    public void setSoftwareVersions(String softwareVersions) {
    	this.softwareVersions = softwareVersions;
    }
    
    public String getSoftwareVersions() {
    	return softwareVersions;
    }
    
    public void setTransmitCoilName(String transmitCoilName) {
    	this.transmitCoilName = transmitCoilName;
    }
    
    public String getTransmitCoilName() {
    	return transmitCoilName;
    }
    
    public void setAcquisitionMatrix(int acquisitionMatrix[]) {
    	this.acquisitionMatrix = acquisitionMatrix;
    }
    
    public int[] getAcquisitionMatrix() {
    	return acquisitionMatrix;
    }
    
    public void setInPlanePhaseEncodingDirection(String inPlanePhaseEncodingDirection) {
    	this.inPlanePhaseEncodingDirection = inPlanePhaseEncodingDirection;
    }
    
    public String getInPlanePhaseEncodingDirection() {
    	return inPlanePhaseEncodingDirection;
    }
    
    public void setFlipAngle(double flipAngle) {
    	this.flipAngle = flipAngle;
    }
    
    public double getFlipAngle() {
    	return flipAngle;
    }
    
    public void setVariableFlipAngleFlag(String variableFlipAngleFlag) {
    	this.variableFlipAngleFlag = variableFlipAngleFlag;
    }
    
    public String getVariableFlipAngleFlag() {
    	return variableFlipAngleFlag;
    }
    
    public void setSAR(double SAR) {
    	this.SAR = SAR;
    }
    
    public double getSAR() {
    	return SAR;
    }
    
    public void setDBdt(double dBdt) {
    	this.dBdt = dBdt;
    }
    
    public double getDBdt() {
    	return dBdt;
    }
    
    public void setSeriesNumber(int seriesNumber) {
    	this.seriesNumber = seriesNumber;
    }
    
    public int getSeriesNumber() {
    	return seriesNumber;
    }
    
    public void setImagePositionPatient(double imagePositionPatient[]) {
    	this.imagePositionPatient = imagePositionPatient;
    }
    
    public double[] getImagePositionPatient() {
    	return imagePositionPatient;
    }
    
    public void setImageOrientationPatient(double imageOrientationPatient[]) {
    	this.imageOrientationPatient = imageOrientationPatient;
    }
    
    public double[] getImageOrientationPatient() {
    	return imageOrientationPatient;
    }
    
    public void setSliceLocation(double sliceLocation) {
    	this.sliceLocation = sliceLocation;
    }
    
    public double getSliceLocation() {
    	return sliceLocation;
    }
    
    public void setSamplesPerPixel(int samplesPerPixel) {
    	this.samplesPerPixel = samplesPerPixel;
    }
    
    public int getSamplesPerPixel() {
    	return samplesPerPixel;
    }
    
    public void setPhotometricInterpretation(String photometricInterpretation) {
    	this.photometricInterpretation = photometricInterpretation;
    }
    
    public String getPhotometricInterpretation() {
    	return photometricInterpretation;
    }
    
    public void setRows(int rows) {
    	this.rows = rows;
    }
    
    public int getRows() {
    	return rows;
    }
    
    public void setColumns(int columns) {
    	this.columns = columns;
    }
    
    public int getColumns() {
    	return columns;
    }
    
    public void setPixelSpacing(double pixelSpacing[]) {
    	this.pixelSpacing = pixelSpacing;
    }
    
    public double[] getPixelSpacing() {
    	return pixelSpacing;
    }
    
    public void setBitsAllocated(int bitsAllocated) {
    	this.bitsAllocated = bitsAllocated;
    }
    
    public int getBitsAllocated() {
    	return bitsAllocated;
    }
    
    public void setBitsStored(int bitsStored) {
    	this.bitsStored = bitsStored;
    }
    
    public int getBitsStored() {
    	return bitsStored;
    }
    
    public void setHighBit(int highBit) {
    	this.highBit = highBit;
    }
    
    public int getHighBit() {
    	return highBit;
    }
    
    public void setPixelRepresentation(int pixelRepresentation) {
    	this.pixelRepresentation = pixelRepresentation;
    }
    
    public int getPixelRepresentation() {
    	return pixelRepresentation;
    }
    
    public void setSmallestImagePixelValue(int smallestImagePixelValue) {
    	this.smallestImagePixelValue = smallestImagePixelValue;
    }
    
    public int getSmallestImagePixelValue() {
    	return smallestImagePixelValue;
    }
    
    public void setLargestImagePixelValue(int largestImagePixelValue) {
    	this.largestImagePixelValue = largestImagePixelValue;
    }
    
    public int getLargestImagePixelValue() {
    	return largestImagePixelValue;
    }
    
    public void setWindowCenterWidthExplanation(String windowCenterWidthExplanation) {
    	this.windowCenterWidthExplanation = windowCenterWidthExplanation;
    }
    
    public String getWindowCenterWidthExplanation() {
    	return windowCenterWidthExplanation;
    }
    
    public void setPerformedProcedureStepStartTime(String performedProcedureStepStartTime) {
    	this.performedProcedureStepStartTime = performedProcedureStepStartTime;
    }
    
    public String getPerformedProcedureStepStartTime() {
    	return performedProcedureStepStartTime;
    }
    
    public void setCsaImageEchoLinePosition(int CsaImageEchoLinePosition) {
    	this.CsaImageEchoLinePosition = CsaImageEchoLinePosition;
    }
    
    public int getCsaImageEchoLinePosition() {
    	return CsaImageEchoLinePosition;
    }
    
    public void setCsaImageProtocolSliceNumber(int CsaImageProtocolSliceNumber) {
    	this.CsaImageProtocolSliceNumber = CsaImageProtocolSliceNumber;
    }
    
    public int getCsaImageProtocolSliceNumber() {
    	return CsaImageProtocolSliceNumber;
    }
    
    public void setCsaImageUsedChannelMask(int CsaImageUsedChannelMask) {
    	this.CsaImageUsedChannelMask = CsaImageUsedChannelMask;
    }
    
    public int getCsaImageUsedChannelMask() {
    	return CsaImageUsedChannelMask;
    }
    
    public void setCsaImageBandwidthPerPixelPhaseEncode(double CsaImageBandwidthPerPixelPhaseEncode) {
    	this.CsaImageBandwidthPerPixelPhaseEncode = CsaImageBandwidthPerPixelPhaseEncode;
    }
    
    public double getCsaImageBandwidthPerPixelPhaseEncode() {
    	return CsaImageBandwidthPerPixelPhaseEncode;
    }
    
    public void setCsaImageMeasuredFourierLines(int CsaImageMeasuredFourierLines) {
    	this.CsaImageMeasuredFourierLines = CsaImageMeasuredFourierLines;
    }
    
    public int getCsaImageMeasuredFourierLines() {
    	return CsaImageMeasuredFourierLines;
    }
    
    public void setCsaImageSequenceMask(int CsaImageSequenceMask) {
    	this.CsaImageSequenceMask = CsaImageSequenceMask;
    }
    
    public int getCsaImageSequenceMask() {
    	return CsaImageSequenceMask;
    }
    
    public void setCsaImageRFSWDDataType(String CsaImageRFSWDDataType) {
    	this.CsaImageRFSWDDataType = CsaImageRFSWDDataType;
    }
    
    public String getCsaImageRFSWDDataType() {
    	return CsaImageRFSWDDataType;
    }
    
    public void setCsaImageImaPATModeText(String CsaImageImaPATModeText) {
    	this.CsaImageImaPATModeText = CsaImageImaPATModeText;
    }
    
    public String getCsaImageImaPATModeText() {
    	return CsaImageImaPATModeText;
    }
    
    public void setCsaImageRealDwellTime(int CsaImageRealDwellTime) {
    	this.CsaImageRealDwellTime = CsaImageRealDwellTime;
    }
    
    public int getCsaImageRealDwellTime() {
    	return CsaImageRealDwellTime;
    }
    
    public void setCsaImageImaCoilString(String CsaImageImaCoilString) {
    	this.CsaImageImaCoilString = CsaImageImaCoilString;
    }
    
    public String getCsaImageImaCoilString() {
    	return CsaImageImaCoilString;
    }
    
    public void setCsaImageEchoColumnPosition(int CsaImageEchoColumnPosition) {
    	this.CsaImageEchoColumnPosition = CsaImageEchoColumnPosition;
    }
    
    public int getCsaImageEchoColumnPosition() {
    	return CsaImageEchoColumnPosition;
    }
    
    public void setCsaImagePhaseEncodingDirectionPositive(int CsaImagePhaseEncodingDirectionPositive) {
    	this.CsaImagePhaseEncodingDirectionPositive = CsaImagePhaseEncodingDirectionPositive;
    }
    
    public int getCsaImagePhaseEncodingDirectionPositive() {
    	return CsaImagePhaseEncodingDirectionPositive;
    }
    
    public void setCsaImageSlicePosition_PCS(double CsaImageSlicePosition_PCS[]) {
    	this.CsaImageSlicePosition_PCS = CsaImageSlicePosition_PCS;
    }
    
    public double[] getCsaImageSlicePosition_PCS() {
    	return CsaImageSlicePosition_PCS;
    }
    
    public void setCsaImageSliceNormalVector(double CsaImageSliceNormalVector[]) {
    	this.CsaImageSliceNormalVector = CsaImageSliceNormalVector;
    }
    
    public double[] getCsaImageSliceNormalVector() {
    	return CsaImageSliceNormalVector;
    }
    
    public void setCsaImageGSWDDataType(String CsaImageGSWDDataType) {
    	this.CsaImageGSWDDataType = CsaImageGSWDDataType;
    }
    
    public String getCsaImageGSWDDataType() {
    	return CsaImageGSWDDataType;
    }
    
    public void setCsaImageMultistepIndex(int CsaImageMultistepIndex) {
    	this.CsaImageMultistepIndex = CsaImageMultistepIndex;
    }
    
    public int getCsaImageMultistepIndex() {
    	return CsaImageMultistepIndex;
    }
    
    public void setCsaImageImaRelTablePosition(int CsaImageImaRelTablePosition[]) {
    	this.CsaImageImaRelTablePosition = CsaImageImaRelTablePosition;
    }
    
    public int[] getCsaImageImaRelTablePosition() {
    	return CsaImageImaRelTablePosition;
    }
    
    public void setCsaImageNumberOfImagesInMosaic(int CsaImageNumberOfImagesInMosaic) {
    	this.CsaImageNumberOfImagesInMosaic = CsaImageNumberOfImagesInMosaic;
    }
    
    public int getCsaImageNumberOfImagesInMosaic() {
    	return CsaImageNumberOfImagesInMosaic;
    }
    
    public void setCsaImageNonPlanarImage(int CsaImageNonPlanarImage) {
    	this.CsaImageNonPlanarImage = CsaImageNonPlanarImage;
    }
    
    public int getCsaImageNonPlanarImage() {
    	return CsaImageNonPlanarImage;
    }
    
    public void setCsaImageEchoPartitionPosition(int CsaImageEchoPartitionPosition) {
    	this.CsaImageEchoPartitionPosition = CsaImageEchoPartitionPosition;
    }
    
    public int getCsaImageEchoPartitionPosition() {
    	return CsaImageEchoPartitionPosition;
    }
    
    public void setCsaImageAcquisitionMatrixText(String CsaImageAcquisitionMatrixText) {
    	this.CsaImageAcquisitionMatrixText = CsaImageAcquisitionMatrixText;
    }
    
    public String getCsaImageAcquisitionMatrixText() {
    	return CsaImageAcquisitionMatrixText;
    }
    
    public void setCsaImageImaAbsTablePosition(int CsaImageImaAbsTablePosition[]) {
    	this.CsaImageImaAbsTablePosition = CsaImageImaAbsTablePosition;
    }
    
    public int[] getCsaImageImaAbsTablePosition() {
    	return CsaImageImaAbsTablePosition;
    }
    
    public void setCsaSeriesTalesReferencePower(double CsaSeriesTalesReferencePower) {
    	this.CsaSeriesTalesReferencePower = CsaSeriesTalesReferencePower;
    }
    
    public double getCsaSeriesTalesReferencePower() {
    	return CsaSeriesTalesReferencePower;
    }
    
    public void setCsaSeriesOperation_mode_flag(int CsaSeriesOperation_mode_flag) {
    	this.CsaSeriesOperation_mode_flag = CsaSeriesOperation_mode_flag;
    }
    
    public int getCsaSeriesOperation_mode_flag() {
    	return CsaSeriesOperation_mode_flag;
    }
    
    public void setCsaSeriesdBdt_thresh(double CsaSeriesdBdt_thresh) {
    	this.CsaSeriesdBdt_thresh = CsaSeriesdBdt_thresh;
    }
    
    public double getCsaSeriesdBdt_thresh() {
    	return CsaSeriesdBdt_thresh;
    }
    
    public void setCsaSeriesProtocolChangeHistory(int CsaSeriesProtocolChangeHistory) {
    	this.CsaSeriesProtocolChangeHistory = CsaSeriesProtocolChangeHistory;
    }
    
    public int getCsaSeriesProtocolChangeHistory() {
    	return CsaSeriesProtocolChangeHistory;
    }
    
    public void setCsaSeriesGradientDelayTime(double CsaSeriesGradientDelayTime[]) {
    	this.CsaSeriesGradientDelayTime = CsaSeriesGradientDelayTime;
    }
    
    public double[] getCsaSeriesGradientDelayTime() {
    	return CsaSeriesGradientDelayTime;
    }
    
    public void setCsaSeriesSARMostCriticalAspect(double CsaSeriesSARMostCriticalAspect[]) {
    	this.CsaSeriesSARMostCriticalAspect = CsaSeriesSARMostCriticalAspect;
    }
    
    public double[] getCsaSeriesSARMostCriticalAspect() {
    	return CsaSeriesSARMostCriticalAspect;
    }
    
    public void setCsaSeriesB1rms(double CsaSeriesB1rms[]) {
    	this.CsaSeriesB1rms = CsaSeriesB1rms;
    }
    
    public double[] getCsaSeriesB1rms() {
    	return CsaSeriesB1rms;
    }
    
    public void setCsaSeriesPATModeText(String CsaSeriesPATModeText) {
    	this.CsaSeriesPATModeText = CsaSeriesPATModeText;
    }
    
    public String getCsaSeriesPATModeText() {
    	return CsaSeriesPATModeText;
    }
    
    public void setCsaSeriesRelTablePosition(int CsaSeriesRelTablePosition[]) {
    	this.CsaSeriesRelTablePosition = CsaSeriesRelTablePosition;
    }
    
    public int[] getCsaSeriesRelTablePosition() {
    	return CsaSeriesRelTablePosition;
    }
    
    public void setCsaSeriesNumberOfPrescans(int CsaSeriesNumberOfPrescans) {
    	this.CsaSeriesNumberOfPrescans = CsaSeriesNumberOfPrescans;
    }
    
    public int getCsaSeriesNumberOfPrescans() {
    	return CsaSeriesNumberOfPrescans;
    }
    
    public void setCsaSeriesdBdt_limit(double CsaSeriesdBdt_limit) {
    	this.CsaSeriesdBdt_limit = CsaSeriesdBdt_limit;
    }
    
    public double getCsaSeriesdBdt_limit() {
    	return CsaSeriesdBdt_limit;
    }
    
    public void setCsaSeriesStim_lim(double CsaSeriesStim_lim[]) {
    	this.CsaSeriesStim_lim = CsaSeriesStim_lim;
    }
    
    public double[] getCsaSeriesStim_lim() {
    	return CsaSeriesStim_lim;
    }
    
    public void setCsaSeriesPatReinPattern(String CsaSeriesPatReinPattern) {
    	this.CsaSeriesPatReinPattern = CsaSeriesPatReinPattern;
    }
    
    public String getCsaSeriesPatReinPattern() {
    	return CsaSeriesPatReinPattern;
    }
    
    public void setCsaSeriesB1rmsSupervision(String CsaSeriesB1rmsSupervision) {
    	this.CsaSeriesB1rmsSupervision = CsaSeriesB1rmsSupervision;
    }
    
    public String getCsaSeriesB1rmsSupervision() {
    	return CsaSeriesB1rmsSupervision;
    }
    
    public void setCsaSeriesReadoutGradientAmplitude(double CsaSeriesReadoutGradientAmplitude) {
    	this.CsaSeriesReadoutGradientAmplitude = CsaSeriesReadoutGradientAmplitude;
    }
    
    public double getCsaSeriesReadoutGradientAmplitude() {
    	return CsaSeriesReadoutGradientAmplitude;
    }
    
    public void setCsaSeriesMrProtocolVersion(int CsaSeriesMrProtocolVersion) {
    	this.CsaSeriesMrProtocolVersion = CsaSeriesMrProtocolVersion;
    }
    
    public int getCsaSeriesMrProtocolVersion() {
    	return CsaSeriesMrProtocolVersion;
    }
    
    public void setCsaSeriesRFSWDMostCriticalAspect(String CsaSeriesRFSWDMostCriticalAspect) {
    	this.CsaSeriesRFSWDMostCriticalAspect = CsaSeriesRFSWDMostCriticalAspect;
    }
    
    public String getCsaSeriesRFSWDMostCriticalAspect() {
    	return CsaSeriesRFSWDMostCriticalAspect;
    }
    
    public void setCsaSeriesSequenceFileOwner(String CsaSeriesSequenceFileOwner) {
    	this.CsaSeriesSequenceFileOwner = CsaSeriesSequenceFileOwner;
    }
    
    public String getCsaSeriesSequenceFileOwner() {
    	return CsaSeriesSequenceFileOwner;
    }
    
    public void setCsaSeriesGradientMode(String CsaSeriesGradientMode) {
    	this.CsaSeriesGradientMode = CsaSeriesGradientMode;
    }
    
    public String getCsaSeriesGradientMode() {
    	return CsaSeriesGradientMode;
    }
}
