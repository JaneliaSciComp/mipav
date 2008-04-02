package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogEditor;
import gov.nih.mipav.view.dialogs.JDialogFileInfo;

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
    private float sliceDuration = -1.0f;

    /** Slice timing pattern ends with slice = (sliceEnd + 1) */
    private short sliceEnd = -1;

    /** Slice timing pattern starts with slice = (sliceStart + 1) */
    private short sliceStart = -1;


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
            dialog.appendPrimaryData("Min", Double.toString(getMin()) + ":" + getMinLoc().toString());
            dialog.appendPrimaryData("Max", Double.toString(getMax()) + ":" + getMaxLoc().toString());
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
                                         Float.toString(resolutions[i]) + " " + getUnitsOfMeasureStr(measure[i]));
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


        // description
        try {
            editorChoice[0] = JDialogEditor.ANALYZE_DESCRIPTION;
            dialog.appendSecondaryData("Description", descrip.trim(), editorChoice);
        } catch (NullPointerException npe) {
            editorChoice[0] = JDialogEditor.ANALYZE_DESCRIPTION;
            dialog.appendSecondaryData("Description", "", editorChoice);
        }


        if (vox_offset != -1) { // vox offset

            // dialog.append("voxel offset: " + vox_offset + "\n");
            editorChoice[0] = JDialogEditor.FLOAT_STRING;
            dialog.appendSecondaryData("Voxel Offset", Float.toString(vox_offset), editorChoice);
        }

        switch (intentCode) {

            case FileInfoNIFTI.NIFTI_INTENT_NONE:
                dialog.appendSecondaryData("Intent code", "No intention");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_CORREL:
                dialog.appendSecondaryData("Statistics code", "Correlation coefficient R");
                dialog.appendSecondaryData("Degrees of freedom", Integer.toString(Math.round(intentP1)));
                break;

            case FileInfoNIFTI.NIFTI_INTENT_TTEST:
                dialog.appendSecondaryData("Statistics code", "Student t test");
                dialog.appendSecondaryData("Degrees of freedom", Integer.toString(Math.round(intentP1)));
                break;

            case FileInfoNIFTI.NIFTI_INTENT_FTEST:
                dialog.appendSecondaryData("Statistics code", "Fisher F statistic");
                dialog.appendSecondaryData("Numerator degrees of freedom", Integer.toString(Math.round(intentP1)));
                dialog.appendSecondaryData("Denominator degrees of freedom", Integer.toString(Math.round(intentP2)));
                break;

            case FileInfoNIFTI.NIFTI_INTENT_ZSCORE:
                dialog.appendSecondaryData("Statistics code", "Standard normal - N(0,1) distributed");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_CHISQ:
                dialog.appendSecondaryData("Statistics code", "Chi - squared");
                dialog.appendSecondaryData("Degrees of freedom", Integer.toString(Math.round(intentP1)));
                break;

            case FileInfoNIFTI.NIFTI_INTENT_BETA:
                dialog.appendSecondaryData("Statistics code", "Beta distribution");
                dialog.appendSecondaryData("a parameter", Float.toString(intentP1));
                dialog.appendSecondaryData("b parameter", Float.toString(intentP2));
                break;

            case FileInfoNIFTI.NIFTI_INTENT_BINOM:
                dialog.appendSecondaryData("Statistics code", "Binomial distribution");
                dialog.appendSecondaryData("Number of trials", Integer.toString(Math.round(intentP1)));
                dialog.appendSecondaryData("Probability per trial", Float.toString(intentP2));
                break;

            case FileInfoNIFTI.NIFTI_INTENT_GAMMA:
                dialog.appendSecondaryData("Statistics code", "Gamma with PDF = x**(shape-1) * exp(-Scale*x)");
                dialog.appendSecondaryData("Shape", Float.toString(intentP1));
                dialog.appendSecondaryData("Scale", Float.toString(intentP2));
                break;

            case FileInfoNIFTI.NIFTI_INTENT_POISSON:
                dialog.appendSecondaryData("Statistics code", "Poisson distribution");
                dialog.appendSecondaryData("Mean", Float.toString(intentP1));
                break;

            case FileInfoNIFTI.NIFTI_INTENT_NORMAL:
                dialog.appendSecondaryData("Statistics code", "Normal distribution");
                dialog.appendSecondaryData("Mean", Float.toString(intentP1));
                dialog.appendSecondaryData("Standard deviation", Float.toString(intentP2));
                break;

            case FileInfoNIFTI.NIFTI_INTENT_FTEST_NONC:
                dialog.appendSecondaryData("Statistics code", "Nocentral F statistic");
                dialog.appendSecondaryData("Numerator degrees of freedom", Integer.toString(Math.round(intentP1)));
                dialog.appendSecondaryData("Denominator degrees of freedom", Integer.toString(Math.round(intentP2)));
                dialog.appendSecondaryData("Numerator noncentrality parameter", Float.toString(intentP3));
                break;

            case FileInfoNIFTI.NIFTI_INTENT_CHISQ_NONC:
                dialog.appendSecondaryData("Statistics code", "Nocentral chi-squared statistic");
                dialog.appendSecondaryData("Degrees of freedom", Integer.toString(Math.round(intentP1)));
                dialog.appendSecondaryData("Noncentrality parameter", Float.toString(intentP2));
                break;

            case FileInfoNIFTI.NIFTI_INTENT_LOGISTIC:
                dialog.appendSecondaryData("Statistics code", "Logistic distribution");
                dialog.appendSecondaryData("Location", Float.toString(intentP1));
                dialog.appendSecondaryData("Scale", Float.toString(intentP2));
                break;

            case FileInfoNIFTI.NIFTI_INTENT_LAPLACE:
                dialog.appendSecondaryData("Statistics code", "Laplace distribution");
                dialog.appendSecondaryData("Location", Float.toString(intentP1));
                dialog.appendSecondaryData("Scale", Float.toString(intentP2));
                break;

            case FileInfoNIFTI.NIFTI_INTENT_UNIFORM:
                dialog.appendSecondaryData("Statistics code", "Uniform distribution");
                dialog.appendSecondaryData("Start", Float.toString(intentP1));
                dialog.appendSecondaryData("End", Float.toString(intentP2));
                break;

            case FileInfoNIFTI.NIFTI_INTENT_TTEST_NONC:
                dialog.appendSecondaryData("Statistics code", "Nocentral t statistic");
                dialog.appendSecondaryData("Degrees of freedom", Integer.toString(Math.round(intentP1)));
                dialog.appendSecondaryData("Noncentrality parameter", Float.toString(intentP2));
                break;

            case FileInfoNIFTI.NIFTI_INTENT_WEIBULL:
                dialog.appendSecondaryData("Statistics code", "Weibull distribution");
                dialog.appendSecondaryData("Location", Float.toString(intentP1));
                dialog.appendSecondaryData("Scale", Float.toString(intentP2));
                dialog.appendSecondaryData("Power", Float.toString(intentP3));
                break;

            case FileInfoNIFTI.NIFTI_INTENT_CHI:
                dialog.appendSecondaryData("Statistics code", "Chi distribution");

                int p1 = Math.round(intentP1);
                dialog.appendSecondaryData("Degrees of freedom", Integer.toString(p1));
                if (p1 == 1) {
                    dialog.appendSecondaryData("DOF = 1", "Half normal distribution");
                } else if (p1 == 2) {
                    dialog.appendSecondaryData("DOF = 2", "Rayleigh distribution");
                } else if (p1 == 3) {
                    dialog.appendSecondaryData("DOF = 3", "Maxwell-Boltzmann distribution");
                }

                break;

            case FileInfoNIFTI.NIFTI_INTENT_INVGAUSS:
                dialog.appendSecondaryData("Statistics code", "Inverse Gaussian");
                dialog.appendSecondaryData("Mu", Float.toString(intentP1));
                dialog.appendSecondaryData("Lambda", Float.toString(intentP2));
                break;

            case FileInfoNIFTI.NIFTI_INTENT_EXTVAL:
                dialog.appendSecondaryData("Statistics code", "Extreme value type 1");
                dialog.appendSecondaryData("Location", Float.toString(intentP1));
                dialog.appendSecondaryData("Scale", Float.toString(intentP2));
                break;

            case FileInfoNIFTI.NIFTI_INTENT_PVAL:
                dialog.appendSecondaryData("Statistics code", "Data is a p-value");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_LOGPVAL:
                dialog.appendSecondaryData("Statistics code", "Data is a ln(p-value)");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_LOG10PVAL:
                dialog.appendSecondaryData("Statistics code", "Data is a log10(p-value)");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_ESTIMATE:
                dialog.appendSecondaryData("Intent code", "Parameter estimate");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_LABEL:
                dialog.appendSecondaryData("Intent code", "Label index");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_NEURONAME:
                dialog.appendSecondaryData("Intent code", "NeuroNames labels index");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_GENMATRIX:
                dialog.appendSecondaryData("Intent code", "M x N matrix");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_SYMMATRIX:
                dialog.appendSecondaryData("Intent code", "N x N symmetric matrix");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_DISPVECT:
                dialog.appendSecondaryData("Intent code", "Displacement vector");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_VECTOR:
                dialog.appendSecondaryData("Intent code", "Vector");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_POINTSET:
                dialog.appendSecondaryData("Intent code", "Spatial coordinate");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_TRIANGLE:
                dialog.appendSecondaryData("Intent code", "Triple of indexes");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_QUATERNION:
                dialog.appendSecondaryData("Intent code", "Quaternion");
                break;

            case FileInfoNIFTI.NIFTI_INTENT_DIMLESS:
                dialog.appendSecondaryData("Intent code", "Dimless");

            default:
                dialog.appendSecondaryData("Unrecognized intent code", Short.toString(intentCode));
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

            case NIFTI_XFORM_TALAIRACH:
                coordString = "Talairach-Tournoux Atlas";
                break;

            case NIFTI_XFORM_MNI_152:
                coordString = "MNI 152 normalized coordinates";
                break;
        }

        dialog.appendSecondaryData("X,Y,Z Coordinate system", coordString);
        
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
    
                case NIFTI_XFORM_TALAIRACH:
                    coordString2 = "Talairach-Tournoux Atlas";
                    break;
    
                case NIFTI_XFORM_MNI_152:
                    coordString2 = "MNI 152 normalized coordinates";
                    break;
            }
    
            dialog.appendSecondaryData("X,Y,Z Coordinate system 2", coordString2);
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

        dialog.appendSecondaryData("Source type", sourceTypeString);

        dialog.appendSecondaryData("Slope scale", Float.toString(scl_slope));

        dialog.appendSecondaryData("Added offset", Float.toString(scl_inter));

        switch (freq_dim) {

            case 0:
                dialog.appendSecondaryData("Frequency encoding direction", "none");
                break;

            case 1:
                dialog.appendSecondaryData("Frequency encoding direction", "x dimension");
                break;

            case 2:
                dialog.appendSecondaryData("Frequency encoding direction", "y dimension");
                break;

            case 3:
                dialog.appendSecondaryData("Frequency encoding direction", "z dimension");
                break;
        }

        switch (phase_dim) {

            case 0:
                dialog.appendSecondaryData("Phase encoding direction", "none");
                break;

            case 1:
                dialog.appendSecondaryData("Phase encoding direction", "x dimension");
                break;

            case 2:
                dialog.appendSecondaryData("Phase encoding direction", "y dimension");
                break;

            case 3:
                dialog.appendSecondaryData("Phase encoding direction", "z dimension");
                break;
        }

        switch (slice_dim) {

            case 0:
                dialog.appendSecondaryData("Slice acquisition direction", "none");
                break;

            case 1:
                dialog.appendSecondaryData("Slice acquisition direction", "x dimension");
                break;

            case 2:
                dialog.appendSecondaryData("Slice acquisition direction", "y dimension");
                break;

            case 3:
                dialog.appendSecondaryData("Slice acquisition direction", "z dimension");
                break;
        }

        if (sliceDuration > 0) {
            dialog.appendSecondaryData("Time used to acquire 1 slice", String.valueOf(sliceDuration));
        }

        if (sliceCode == NIFTI_SLICE_SEQ_INC) {
            dialog.appendSecondaryData("Timing pattern of slice acquisition", "sequentially increasing");
        } else if (sliceCode == NIFTI_SLICE_SEQ_DEC) {
            dialog.appendSecondaryData("Timing pattern of slice acquisition", "sequentially decreasing");
        } else if (sliceCode == NIFTI_SLICE_ALT_INC) {
            dialog.appendSecondaryData("Timing pattern of slice acquisition", "alternately increasing");
        } else if (sliceCode == NIFTI_SLICE_ALT_DEC) {
            dialog.appendSecondaryData("Timing pattern of slice acquisition", "alternately decreasing");
        } else if (sliceCode == NIFTI_SLICE_ALT_INC2) {
            dialog.appendSecondaryData("Timing pattern of slice acquisition", "alternately increasing #2");
        } else if (sliceCode == NIFTI_SLICE_ALT_DEC2) {
            dialog.appendSecondaryData("Timing pattern of slice acquisition", "alternately decreasing #2");
        }

        if (sliceStart > 0) {
            dialog.appendSecondaryData("Timing pattern starts", "slice " + String.valueOf(sliceStart + 1));
        }

        if (sliceEnd > 0) {
            dialog.appendSecondaryData("Timing pattern ends", "slice " + String.valueOf(sliceEnd + 1));
        }

        editorChoice[0] = JDialogEditor.ANALYZE_AXIS_ORIENTATION;
        dialog.appendSecondaryData("Axis: x-orientation", getAxisOrientationStr(super.getAxisOrientation(0)),
                                   editorChoice);
        dialog.appendSecondaryData("Axis: y-orientation", getAxisOrientationStr(super.getAxisOrientation(1)),
                                   editorChoice);
        dialog.appendSecondaryData("Axis: z-orientation", getAxisOrientationStr(super.getAxisOrientation(2)),
                                   editorChoice);


        editorChoice[0] = JDialogEditor.FLOAT_STRING;
        dialog.appendSecondaryData("X Origin: ", Float.toString(super.getOrigin(0)), editorChoice);
        dialog.appendSecondaryData("Y Origin: ", Float.toString(super.getOrigin(1)), editorChoice);
        dialog.appendSecondaryData("Z Origin: ", Float.toString(super.getOrigin(2)), editorChoice);

        if (cal_min != -1) {
            editorChoice[0] = JDialogEditor.FLOAT_STRING;
            dialog.appendSecondaryData("cal_min", Float.toString(cal_min), editorChoice);
        }

        if (cal_max != -1) {
            editorChoice[0] = JDialogEditor.FLOAT_STRING;
            dialog.appendSecondaryData("cal_max", Float.toString(cal_max), editorChoice);
        }

        if (bitpix != -1) {
            dialog.appendSecondaryData("Bits per Pixel", Integer.toString(bitpix));
        }

        if (aux_file != null) {

            if (aux_file.trim().length() > 0) {
                dialog.appendSecondaryData("aux", aux_file.trim());
            }
        }

        if (intentName != null) {
            dialog.appendSecondaryData("Name or meaning of data", intentName);
        }

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
            Preferences.debug("tname: " + tname + ", not found.");
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
}
