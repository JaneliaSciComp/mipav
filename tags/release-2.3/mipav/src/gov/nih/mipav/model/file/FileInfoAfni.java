package gov.nih.mipav.model.file;

import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.model.structures.*;

/**
*   This structures contains the information that describes how
*   an Afni image is stored on disk.
*
*       @see        FileAfni
*/
public class FileInfoAfni extends FileInfoBase {

    /** AFNI view type Original     */
    public static final int AFNI_ORIG = 0;
    /** AFNI view type ACPC         */
    public static final int AFNI_ACPC = 1;
    /** AFNI view type Talairach    */
    public static final int AFNI_TLRC = 2;

    /** AFNI typeString head anatomical     */
    public static final int HEAD_ANAT_TYPE = 0;
    /** AFNI typeString head functional     */
    public static final int HEAD_FUNC_TYPE = 1;
    /** AFNI typeString general anatomical  */
    public static final int GEN_ANAT_TYPE  = 2;
    /** AFNI typeString general functional  */
    public static final int GEN_FUNC_TYPE  = 3;

    /**
        funcType for  anatType == true <p>
        At this time, Anat codes 0..10 are treated identically by all AFNI programs.  Code 11
        marks the dataset as a "bucket" type, which is treated differently in the display;
        a chooser allows you to specify which sub-brick from the bucket should be used to
        make the underlay image.
    */
    public static final int ANAT_SPGR_TYPE = 0;
    public static final int ANAT_FSE_TYPE = 1;
    public static final int ANAT_EPI_TYPE = 2;
    public static final int ANAT_MRAN_TYPE = 3;
    public static final int ANAT_CT_TYPE = 4;
    public static final int ANAT_SPECT_TYPE = 5;
    public static final int ANAT_PET_TYPE = 6;
    public static final int ANAT_MRA_TYPE = 7;
    public static final int ANAT_BMAP_TYPE = 8;
    public static final int ANAT_DIFF_TYPE = 9;
    public static final int ANAT_OMRI_TYPE = 10;
    public static final int ANAT_BUCK_TYPE = 11;

    /** funcType for anatType == false */
    /** 1 value */
    public static final int FUNC_FIM_TYPE = 0;
    public static final int FUNC_THR_TYPE = 1;   /* obsolete */
    public static final int FUNC_COR_TYPE = 2;   /* fico: correlation */
    public static final int FUNC_TT_TYPE = 3;    /* fitt: t-statistic */
    public static final int FUNC_FT_TYPE = 4;    /* fift: F-statistic */
    public static final int FUNC_ZT_TYPE = 5;    /* fizt: z-score */
    public static final int FUNC_CT_TYPE = 6;    /* fict: Chi squared */
    public static final int FUNC_BT_TYPE = 7;    /* fibt: Beta stat */
    public static final int FUNC_BN_TYPE = 8;    /* fibn: Binomial */
    public static final int FUNC_GT_TYPE = 9;    /* figt: Gamma */
    public static final int FUNC_PT_TYPE = 10;   /* fipt: Poisson */
    public static final int FUNC_BUCK_TYPE = 11; /* fbuc: bucket */
    /* Unfortunately, the func type codes overlap for Func and Anat datasets.  This means
    that one cannot tell the contents of a dataset from a single attribute. */

    private int funcType = 0;
    private int marksFlag = 0;
    private int marksFlags = 0;
    private String label1 = null;
    private String label2 = null;
    private String datasetName = null;
    private String idcodeString = null;
    private String idcodeDate = null;
    private String noteNumber001 = null;
    private String noteNumber002 = null;
    private String noteNumber003 = null;
    private String noteNumber004 = null;
    private String noteNumber005 = null;
    private String noteDate001 = null;
    private String noteDate002 = null;
    private String noteDate003 = null;
    private String noteDate004 = null;
    private String noteDate005 = null;
    private float brickStatAux[] = null;
    private float statAux[] = null;
    private int slicesWithTimeOffsets = 0;
    private float timeOrigin;
    private float timeStep = 0.0f;
    private float acquisitionDuration;
    private float zAxisOffset;
    private float zAxisStep;
    private boolean origDicom; // true if original is in dicom order
    private boolean isDicom; // true if current is in dicom order
    private float[] tAxisOffsets = null; // if TAXIS_NUMS[1] > 0, then this array gives the time offsets of the
                                       // slices defined by TAXIS_FLOATS[3..4].  The time offset at
                                       // z = TAXIS_FLOATS[3] + k*TAXIS_FLOATS[4]
                                       // is TAXIS_OFFSETS[k], for k = 0..TAXIS_NUMS[1] - 1.
                                       // If TAXIS_NUMS[1] == 0, then this attribute is not used
    // markers used for +orig to +ACPC transformation
    private Point3Df superiorEdge           = new Point3Df(Float.POSITIVE_INFINITY,Float.POSITIVE_INFINITY,
                                                           Float.POSITIVE_INFINITY);
    private Point3Df OriginalSuperiorEdge   = new Point3Df(Float.POSITIVE_INFINITY, Float.POSITIVE_INFINITY,
                                                           Float.POSITIVE_INFINITY);
    private Point3Df posteriorMargin        = new Point3Df(Float.POSITIVE_INFINITY,Float.POSITIVE_INFINITY,
                                                           Float.POSITIVE_INFINITY);
    private Point3Df OriginalPosteriorMargin = new Point3Df(Float.POSITIVE_INFINITY,
                                                            Float.POSITIVE_INFINITY,Float.POSITIVE_INFINITY);

    private Point3Df inferiorEdge           = new Point3Df(Float.POSITIVE_INFINITY,Float.POSITIVE_INFINITY,
                                                           Float.POSITIVE_INFINITY);
    private Point3Df firstPt                = new Point3Df(Float.POSITIVE_INFINITY,Float.POSITIVE_INFINITY,
                                                           Float.POSITIVE_INFINITY);
    private Point3Df anotherPt              = new Point3Df(Float.POSITIVE_INFINITY,Float.POSITIVE_INFINITY,
                                                           Float.POSITIVE_INFINITY);
    // markers used for +ACPC to +tlrc transformation
    private Point3Df anteriorPt             = new Point3Df(Float.POSITIVE_INFINITY,Float.POSITIVE_INFINITY,
                                                           Float.POSITIVE_INFINITY);
    private Point3Df posteriorPt            = new Point3Df(Float.POSITIVE_INFINITY,Float.POSITIVE_INFINITY,
                                                           Float.POSITIVE_INFINITY);
    private Point3Df superiorPt             = new Point3Df(Float.POSITIVE_INFINITY,Float.POSITIVE_INFINITY,
                                                           Float.POSITIVE_INFINITY);
    private Point3Df inferiorPt             = new Point3Df(Float.POSITIVE_INFINITY,Float.POSITIVE_INFINITY,
                                                           Float.POSITIVE_INFINITY);
    private Point3Df leftPt                 = new Point3Df(Float.POSITIVE_INFINITY,Float.POSITIVE_INFINITY,
                                                           Float.POSITIVE_INFINITY);
    private Point3Df rightPt                = new Point3Df(Float.POSITIVE_INFINITY,Float.POSITIVE_INFINITY,
                                                           Float.POSITIVE_INFINITY);
    // The posterior commisure inferior edge.  pcie is the coordinated transformed inferiorEdge
    private Point3Df pcie                   = new Point3Df(Float.POSITIVE_INFINITY, Float.POSITIVE_INFINITY,
                                                           Float.POSITIVE_INFINITY);
    private Point3Df TalairachCenter        = new Point3Df(Float.POSITIVE_INFINITY,Float.POSITIVE_INFINITY,
                                                           Float.POSITIVE_INFINITY);
    private float lowXmm;
    private float lowYmm;
    private float lowZmm;
    private float highXmm;
    private float highYmm;
    private float highZmm;
    private int AFNIViewType = -1;
    private int AFNITypeString = -1;
    private Point3Df alpha                  = new Point3Df(0.0f,0.0f,0.0f);
    private Point3Df beta                   = new Point3Df(0.0f,0.0f,0.0f);
    private Point3Df gamma                  = new Point3Df(0.0f,0.0f,0.0f);
    private Point3Df translation            = new Point3Df(0.0f,0.0f,0.0f);

    /**
     * rr is the dicom ordered original image Talairach center location
     */
    private Point3Df rr                     = new Point3Df(0.0f,0.0f,0.0f);
    private int AFNIOrigExtents[]; // dimensions of dicom ordered +orig image
    private float AFNIOrigResolutions[]; // resolutions of dicom ordered +orig image
    // arrays of 12 values used for Talairach conversions
    private Point3Df alphaArray[];
    private Point3Df betaArray[];
    private Point3Df gammaArray[];
    private Point3Df rrArray[];
    private int botX[];
    private int botY[];
    private int botZ[];
    private int topX[];
    private int topY[];
    private int topZ[];

    /**
     * Used to show a zero filling gap in functional AFNI images
     * -1 if no gap is present, 0 = x gap, 1 = y gap, 2 = z gap
     * Interpolation is not performed across zero filling gaps
     */
    private int planeGap = -1;

    /**
     * If planeGap is not -1, then gapArray contains the values at
     * which zero filling gaps are present
     */
    private int gapArray[] = null;



    /**
    *  File info storage constructor
    *  @param name        file name
    *  @param directory   directory
    *  @param format      file format
    */
    public FileInfoAfni(String name, String directory, int format) {
        super(name, directory, format);
    }

    /**
    *   Sets the function type
    *   @param funcType Function type to set.
    */
    public void setFuncType(int funcType) {this.funcType = funcType;}

    /**
    *  Gets the function type
    *  @return funcType
    */
    public int getFuncType() {
        return funcType;
    }

    /**
     * @param marksFlag int
     */
    public void setMarksFlag(int marksFlag) {
      this.marksFlag = marksFlag;
    }

    /**
    * @return marksFlag
    */
    public int getMarksFlag() {
      return marksFlag;
    }


    /**
     * @param marksFlags int
     */
    public void setMarksFlags(int marksFlags) {
      this.marksFlags = marksFlags;
    }

    /**
    * @return marksFlags
    */
    public int getMarksFlags() {
      return marksFlags;
    }

    /**
    *   Sets label1
    *   @param label1   Label 1 to set.
    */
    public void setLabel1(String label1) {this.label1 = label1;}

    /**
    *   Sets label2
    *   @param label2   Label 2 to set.
    */
    public void setLabel2(String label2) {this.label2 = label2;}

    /**
    *   Sets datasetName
    *   @param datasetName  Dataset name to set.
    */
    public void setDatasetName(String datasetName) {this.datasetName = datasetName;}

    /***
    *   Sets IDCodeString
    *   @param idcodeString Code string to set.
    */
    public void setIDCodeString(String idcodeString) {this.idcodeString = idcodeString;}

    /**
    *   Sets IDCodeDate
    *   @param idcodeDate   Code date to set.
    */
    public void setIDCodeDate(String idcodeDate) {this.idcodeDate = idcodeDate;}

    /**
    *   Sets noteNumber001
    *   @param noteNumber001    Value to set.
    */
    public void setNoteNumber001(String noteNumber001) {
        this.noteNumber001 = noteNumber001;
    }

    /**
    *   Sets noteNumber002
    *   @param noteNumber002    Value to set.
    */
    public void setNoteNumber002(String noteNumber002) {
        this.noteNumber002 = noteNumber002;
    }

    /**
    *   Sets noteNumber003
    *   @param noteNumber003    Value to set.
    */
    public void setNoteNumber003(String noteNumber003) {
        this.noteNumber003 = noteNumber003;
    }

    /**
    *   Sets noteNumber004
    *   @param noteNumber004    Value to set.
    */
    public void setNoteNumber004(String noteNumber004) {
        this.noteNumber004 = noteNumber004;
    }

    /**
    *   Sets noteNumber005
    *   @param noteNumber005    Value to set.
    */
    public void setNoteNumber005(String noteNumber005) {
        this.noteNumber005 = noteNumber005;
    }

    /**
    *   Sets noteDate001
    *   @param noteDate001      Value to set.
    */
    public void setNoteDate001(String noteDate001) {
        this.noteDate001 = noteDate001;
    }

    /**
    *   Sets noteDate002
    *   @param noteDate002      Value to set.
    */
    public void setNoteDate002(String noteDate002) {
        this.noteDate002 = noteDate002;
    }

    /**
    *   Sets noteDate003
    *   @param noteDate003      Value to set.
    */
    public void setNoteDate003(String noteDate003) {
        this.noteDate003 = noteDate003;
    }

    /**
    *   Sets noteDate004
    *   @param noteDate004      Value to set.
    */
    public void setNoteDate004(String noteDate004) {
        this.noteDate004 = noteDate004;
    }

    /**
    *   Sets noteDate005
    *   @param noteDate005      Value to set.
    */
    public void setNoteDate005(String noteDate005) {
        this.noteDate005 = noteDate005;
    }

    /**
    *   Sets brickStatAux
    *   The BRICK_STATAUX attribute allows you to attach statistical distribution
    *   information to arbitrary sub-bricks of a bucket dataset.
    *   @param brickStatAux auxiliary statistical information about sub-bricks
    */
    public void setBrickStatAux(float[] brickStatAux) {
        this.brickStatAux = brickStatAux;
    }

    /**
    *   Sets statAux.  Stat aux applies statistical information to the second sub-brick of a func
    *   dataset of type FUNC_COR_TYPE, FUNC_TT_TYPE, FUNC_FT_TYPE, FUNC_CT_TYPE, FUNC_BT_TYPE,
    *   FUNC_BN_TYPE, FUNC_GT_TYPE, or FUNC_PT_TYPE
    *   @param statAux  Value to set.
    */
    public void setStatAux(float[] statAux) {
        this.statAux = statAux;
    }

    /**
    *   Sets slicesWithTimeOffsets
    *   @param slicesWithTimeOffsets    Value to set.
    */
    public void setSlicesWithTimeOffsets(int slicesWithTimeOffsets) {
        this.slicesWithTimeOffsets = slicesWithTimeOffsets;
    }

    /**
    *   Gets slicesWithTimeOffsets
    *   @return slicesWithTimeOffsets
    */
    public int getSlicesWithTimeOffsets() {
        return slicesWithTimeOffsets;
    }

    /**
    *   Sets timeOrigin
    *   @param timeOrigin   Value to set.
    */
    public void setTimeOrigin(float timeOrigin) {
        this.timeOrigin = timeOrigin;
    }

    /**
    *   Sets timeStep
    *   @param timeStep Value to set.
    */
    public void setTimeStep(float timeStep) {
        this.timeStep = timeStep;
    }

    /**
    *   Gets timeStep
    *   @return timeStep
    */
    public float getTimeStep() {
        return timeStep;
    }

    /**
    *   Sets acquisition duration
    *   @param acquisitionDuration  Value to set.
    */
    public void setAcquisitionDuration(float acquisitionDuration) {
        this.acquisitionDuration = acquisitionDuration;
    }

    /**
    *   Sets z axis offset
    *   @param zAxisOffset  Value to set.
    */
    public void setZAxisOffset(float zAxisOffset) {
        this.zAxisOffset = zAxisOffset;
    }

    /**
    *   Sets z axis step
    *   @param zAxisStep    Value to set.
    */
    public void setZAxisStep(float zAxisStep) {
        this.zAxisStep = zAxisStep;
    }

    /**
    *   Sets origDicom
    *   @param origDicom    Value to set.
    */
    public void setOrigDicom(boolean origDicom) {
        this.origDicom = origDicom;
    }

    /**
    *   Sets time axis offsets
    *   @param tAxisOffsets     Time axis offsets to set.
    */
    public void setTAxisOffsets(float[] tAxisOffsets) {
        this.tAxisOffsets = tAxisOffsets;
    }

    /**
    *   Gets time axis offsets
    *   @return tAxisOffsets
    */
    public float[] getTAxisOffsets() {
        return tAxisOffsets;
    }

    /**
    *   Returns the lowest x location in millimeters
    *   @return     lowest x location in millimeters
    */
    public float getLowXmm() {return lowXmm;}

    /**
    *   Returns the lowest Y location in millimeters
    *   @return     lowest Y location in millimeters
    */
    public float getLowYmm() {return lowYmm;}

    /**
    *   Returns the lowest Z location in millimeters
    *   @return     lowest Z location in millimeters
    */
    public float getLowZmm() {return lowZmm;}

    /**
    *   Returns the highest x location in millimeters
    *   @return     highest x location in millimeters
    */
    public float getHighXmm() {return highXmm;}

    /**
    *   Returns the highest y location in millimeters
    *   @return     highest y location in millimeters
    */
    public float getHighYmm() {return highYmm;}

    /**
    *   Returns the highest z location in millimeters
    *   @return     highest z location in millimeters
    */
    public float getHighZmm() {return highZmm;}

    /**
    *   Returns another mid-sag marker point for +orig to +acpc
    *   transformation in dataset ordering
    *   @return                returns marker point
    */
    public Point3Df getAnotherPt() {return anotherPt;}

    /**
    *   Returns most anterior point marker for +acpc to +tlrc
    *   transformation in dicom ordering
    *   @return                returns marker point
    */
    public Point3Df getAnteriorPt() {return anteriorPt;}

    /**
    *   Returns Talairach center for +acpc to +tlrc
    *   transformation in dicom ordering
    *   @return                returns TalairachCenter
    */
    public Point3Df getTalairachCenter() {return TalairachCenter;}

    /**
    *   Returns T00, T01, and T02 in the transformation matrix
    *   @return               returns alpha
    */
    public Point3Df getAlpha() {return alpha;}

    /**
    *   Returns T10, T11, and T12 in the transformation matrix
    *  @return               returns beta
    */
    public Point3Df getBeta() {return beta;}

    /**
    *   Returns T20, T21, and T22 in the transformation matrix
    *   @return               returns gamma
    */
    public Point3Df getGamma() {return gamma;}

    /**
    *   Returns T03, T13, and T23 in the transformationmatrix
    *   @return               returns translation
    */
    public Point3Df getTranslation() {return translation;}

    /**
    *   Returns the location of the Talairach center in the dicom
    *   order +orig anatomical image in voxel indices
    *   @return               returns rr
    */
    public Point3Df getrr() {return rr;}

    /**
    *   Returns array of T00, T01, and T02 in the transformation matrix
    *   @return               returns alphaArray
    */
    public Point3Df[] getAlphaArray() {return alphaArray;}

    /**
    *   Returns array of T10, T11, and T12 in the transformation matrix
    *   @return               returns betaArray
    */
    public Point3Df[] getBetaArray() {return betaArray;}

    /**
    *   Returns array of T20, T21, and T22 in the transformation matrix
    *   @return               returns gammaArray
    */
    public Point3Df[] getGammaArray() {return gammaArray;}

    /**
    *   Returns array of the location of the Talairach center in the dicom
    *   order +orig anatomical image in voxel indices
    *   @return               returns rrArray
    */
    public Point3Df[] getrrArray() {return rrArray;}

    /**
    *   Returns array of bottom X boundaries for conversion to
    *   Talairach space
    *   @return               returns botX
    */
    public int[] getBotX() {return botX;}

    /**
    *   Returns array of bottom Y boundaries for conversion to
    *   Talairach space
    *   @return               returns botY
    */
    public int[] getBotY() {return botY;}

    /**
    *   Returns array of bottom Z boundaries for conversion to
    *   Talairach space
    *   @return               returns botZ
    */
    public int[] getBotZ() {return botZ;}

    /**
    *   Returns array of top X boundaries for conversion to
    *   Talairach space
    *   @return               returns topX
    */
    public int[] getTopX() {return topX;}

    /**
    *   Returns array of top Y boundaries for conversion to
    *   Talairach space
    *   @return               returns topY
    */
    public int[] getTopY() {return topY;}

    /**
    *   Returns array of top Z boundaries for conversion to
    *   Talairach space
    *   @return               returns topZ
    */
    public int[] getTopZ() {return topZ;}

    /**
    *   Returns the dimensions of the dicom ordered +orig AFNI image
    *   @return               the array contain dicom ordered +orig image dimensions
    */
    public int[] getAFNIOrigExtents() {return AFNIOrigExtents;}

    /**
    *   Return the resolutions of the dicom ordered +orig AFNI image
    *   return                 the array containing the dicom ordered +orig image resolutions
    */
    public float[] getAFNIOrigResolutions() {return AFNIOrigResolutions;}


    /**
    *   Returns first mid-sag point marker point for +orig to +acpc
    *   transformation in dataset ordering
    *   @return                returns marker point
    */
    public Point3Df getFirstPt() {return firstPt;}

    /**
    *   Returns PC inferior edge marker point for +orig to +acpc
    *   transformation in dataset ordering
    *   @return                returns marker point
    */
    public Point3Df getInferiorEdge() {return inferiorEdge;}

    /**
    *   Returns most inferior point marker for +acpc to +tlrc
    *   transformation in dicom ordering
    *   @return                 returns marker point
    */
    public Point3Df getInferiorPt() {return inferiorPt;}

    /**
    *   Returns most left point marker for +acpc to +tlrc
    *   transformation in dicom ordering
    *   @return                returns marker point
    */
    public Point3Df getLeftPt() {return leftPt;}

    /**
    *   Returns posterior commissure inferior edge for +acpc to +tlrc
    *   transformation in dicom ordering
    *   @return                returns marker point
    */
    public Point3Df getpcie() {return pcie;}

    /**
    *   Returns -1 if no plane gap is present, 0 = x gap, 1 = y gap, 2 = z gap
    *   @return       returns planeGap
    */
    public int getPlaneGap() {return planeGap;}

    /**
    *   Returns array of coordinate values at which gap is present
    *   @return       returns gapArray
    */
    public int[] getGapArray() {return gapArray;}

    /**
    *   Returns AC posterior margin marker point for +orig to +acpc
    *   transformation in dataset ordering
    *   @return                returns marker point
    */
    public Point3Df getPosteriorMargin() {return posteriorMargin;}

    /**
    *   Returns AC posterior margin marker point for +orig to +acpc
    *   transformation in dicom ordering using numbers not adjusted
    *   for resolution and origin
    *   @return                returns marker point
    */
    public Point3Df getOriginalPosteriorMargin() {return OriginalPosteriorMargin;}

    /**
    *   Returns most posterior point marker for +acpc to +tlrc
    *   transformation in dicom ordering
    *   @return                returns marker point
    */
    public Point3Df getPosteriorPt() {return posteriorPt;}

    /**
    *   Returns most right point marker for +acpc to +tlrc
    *   transformation in dicom ordering
    *   @return                returns marker point
    */
    public Point3Df getRightPt() {return rightPt;}

    /**
    *   Returns AC superior edge marker point for +orig to +acpc
    *   transformation in dataset ordering
    *   @return                returns marker point
    */
    public Point3Df getSuperiorEdge() {return superiorEdge;}

    /**
    *   Returns AC superior edge marker point for +orig to +acpc
    *   transformation in dicom ordering using numbers not adjusted
    *   for resolution and origin
    *   @return                returns marker point
    */
    public Point3Df getOriginalSuperiorEdge() {return OriginalSuperiorEdge;}

    /**
    *   Returns most superior point marker for +acpc to +tlrc
    *    transformation in dicom ordering
    *   @return                returns marker point
    */
    public Point3Df getSuperiorPt() {return superiorPt;}

    /**
    *   Returns integer telling if file is AFNI +orig,
    *   +acpc, or +tlrc
    *   @return           returns AFNIViewType
    */
    public int getAFNIViewType() {return AFNIViewType;}

    /**
    *   Returns integer telling if file is AFNI HEAD_ANAT_TYPE,
    *   HEAD_FUNC_TYPE, GEN_ANAT_TYPE, or GEN_FUNC_TYPE
    *   @return             returns AFNITypeString
    */
    public int getAFNITypeString() {return AFNITypeString;}

    /**
    *   Returns the tResolution
    *   @return          Duration in msec per phase
    */
    //public double getTResolution() { return tResolution;}

    /**
    *   Sets the value of the lowest x location in millimeters
    *   @param lowXmm
    */
    public void setLowXmm(float lowXmm) {
        this.lowXmm = lowXmm;
    }

    /**
    *   Sets the value of the lowest y location in millimeters
    *   @param lowYmm
    */
    public void setLowYmm(float lowYmm) {
        this.lowYmm = lowYmm;
    }

    /**
    *   Sets the value of the lowest z location in millimeters
    *   @param lowZmm
    */
    public void setLowZmm(float lowZmm) {
        this.lowZmm = lowZmm;
    }

    /**
    *   Sets the value of the highest x location in millimeters
    *   @param highXmm
    */
    public void setHighXmm(float highXmm) {
        this.highXmm = highXmm;
    }

    /**
    *   Sets the value of the highest y location in millimeters
    *   @param highYmm
    */
    public void setHighYmm(float highYmm) {
        this.highYmm = highYmm;
    }

    /**
    *   Sets the value of the highest z location in millimeters
    *   @param highZmm
    */
    public void setHighZmm(float highZmm) {
        this.highZmm = highZmm;
    }

    /**
    *   Sets another mid-sag point marker for orig
    *   to ACPC transformation in dataset ordering
    *   @param pt - the marker point coordinates
    */
    public void setAnotherPt(Point3Df pt) {
        anotherPt = pt;
    }

    /**
    *   Sets the most anterior point marker for +acpc
    *   to +tlrc transformation in dicom ordering
    *   @param pt - the marker point coordinates
    */
    public void setAnteriorPt(Point3Df pt) {
        anteriorPt = pt;
    }

    /**
    *   Sets the Talairach center point for +acpc
    *    to +tlrc transformation in dicom ordering
    *   @param pt - the Talairach center coordinates
    */
    public void setTalairachCenter(Point3Df pt) {
        TalairachCenter = pt;
    }

    /**
    *   Sets T00, T01, and T02 in the transformation matrix
    *   @param pt - the alpha point coordinates
    */
    public void setAlpha(Point3Df pt) {
        alpha = pt;
    }

    /**
    *   Sets T10, T11, and T12 in the transformation matrix
    *   @param pt -the beta point coordinates
    */
    public void setBeta(Point3Df pt) {
        beta = pt;
    }

    /**
    *   Sets T20, T21, and T22 in the transformation matrix
    *   @param pt -the gamma point coordinates
    */
    public void setGamma(Point3Df pt) {
        gamma = pt;
    }

    /**
    *   Sets T03, T13 and T23 in the transformation matrix
    *   @param pt - the translation point coordinates
    */
    public void setTranslation(Point3Df pt) {
        translation = pt;
    }

    /**
    *   Sets the Talairach center voxel inidces in the dicom ordered original image
    *   @param pt - the rr point coordinates
    */
    public void setrr(Point3Df pt) {
        rr = pt;
    }

    /**
    *   Sets array of T00, T01, and T02 in the transformation matrix
    *   @param pt - the array of alpha point coordinates
    */
    public void setAlphaArray(Point3Df[] pt) {
        alphaArray = pt;
    }

    /**
    *   Sets array of T10, T11, and T12 in the transformation matrix
    *   @param pt -the array of beta point coordinates
    */
    public void setBetaArray(Point3Df[] pt) {
        betaArray = pt;
    }

    /**
    *   Sets array of T20, T21, and T22 in the transformation matrix
    *   @param pt - the array of gamma point coordinates
    */
    public void setGammaArray(Point3Df[] pt) {
        gammaArray = pt;
    }

    /**
    *   Sets array of the Talairach center voxel inidces in the dicom ordered original image
    *   @param pt - the array of rr point coordinates
    */
    public void setrrArray(Point3Df[] pt) {
        rrArray = pt;
    }

    /**
    *   Sets array of the bottom x bounding box boundaries in Talairach conversion
    *   @param botX - the array of bottom x boundaries
    */
    public void setBotX(int[] botX) {
        this.botX = botX;
    }

    /**
    *   Sets array of the bottom y bounding box boundaries in Talairach conversion
    *   @param botY - the array of bottom y boundaries
    */
    public void setBotY(int[] botY) {
        this.botY = botY;
    }

    /**
    *   Sets array of the bottom z bounding box boundaries in Talairach conversion
    *   @param botZ - the array of bottom z boundaries
    */
    public void setBotZ(int[] botZ) {
        this.botZ = botZ;
    }

    /**
    *   Sets array of the top x bounding box boundaries in Talairach conversion
    *   @param topX - the array of top x boundaries
    */
    public void setTopX(int[] topX) {
        this.topX = topX;
    }

    /**
    *   Sets array of the top y bounding box boundaries in Talairach conversion
    *   @param topY - the array of top y boundaries
    */
    public void setTopY(int[] topY) {
        this.topY = topY;
    }

    /**
    *   Sets array of the top z bounding box boundaries in Talairach conversion
    *   @param topZ - the array of top z boundaries
    */
    public void setTopZ(int[] topZ) {
        this.topZ = topZ;
    }

    /**
    *   Sets the dimensions of the dicom ordered +orig image
    *   @param extents the array with dimensions of the dicom ordered +orig image
    */
    public void setAFNIOrigExtents(int[] extents) {
        AFNIOrigExtents = extents;
    }

    /**
    *   Sets the resolutions of the dicom ordered +orig image
    *   @param resolutions  the array with resolutions of the dicom ordered +orig image
    */
    public void setAFNIOrigResolutions(float[] resolutions) {
        AFNIOrigResolutions = resolutions;
    }

    /**
    *   Sets the first mid-sag marker for orig
    *   to ACPC transformation in dataset ordering
    *   @param pt - the marker point coordinates
    */
    public void setFirstPt(Point3Df pt) {
        firstPt = pt;
    }

    /**
    *   Sets the PC superior edge marker for orig
    *   to ACPC transformation in dataset ordering
    *   @param pt  the marker point coordinates
    */
    public void setInferiorEdge(Point3Df pt) {
        inferiorEdge = pt;
    }

    /**
    *   Sets the most inferior point marker for +acpc
    *   to +tlrc transformation in dicom ordering
    *   @param pt  the marker point coordinates
    */
    public void setInferiorPt(Point3Df pt) {
        inferiorPt = pt;
    }

    /**
    *   Sets the most left point marker for +acpc
    *   to +tlrc transformation in dicom ordering
    *   @param pt  the marker point coordinates
    */
    public void setLeftPt(Point3Df pt) {
        leftPt = pt;
    }

    /**
    *   Sets the AC posterior margin marker for orig
    *   to ACPC transformation in dataset ordering
    *   @param pt  the marker point coordinates
    */
    public void setPosteriorMargin(Point3Df pt) {
        posteriorMargin = pt;
    }

    /**
    *   Sets the AC posterior margin marker for orig
    *   to ACPC transformation in dicom ordering
    *   using numbers not adjusted for origin and resolution
    *   @param pt  the marker point coordinates
    */
    public void setOriginalPosteriorMargin(Point3Df pt) {
        OriginalPosteriorMargin = pt;
    }

    /**
    *   Sets the most posterior point marker for +acpc
    *   to +tlrc transformation in dicom ordering
    *   @param pt - the marker point coordinates
    */
    public void setPosteriorPt(Point3Df pt) {
        posteriorPt = pt;
    }

    /**
    *   Sets the posterior commissure inferior edge for +acpc
    *   to +tlrc transformation in dicom ordering
    *   @param pt  the marker point coordinates
    */
    public void setpcie(Point3Df pt) {
        pcie = pt;
    }

    /**
    *   Sets planeGap to -1 for no gap, 0 = x gap, 1 = y gap, 2 = z gap
    *   @param planeGap The plane gap.
    */
    public void setPlaneGap(int planeGap) {
        this.planeGap = planeGap;
    }

    /**
    *   Sets array of gap values for plane gap not equal to -1
    *   @param gapArray
    */
    public void setGapArray(int[] gapArray) {
        this.gapArray = gapArray;
    }

    /**
    *   Sets the AFNIViewType to AFNI_ORIG, AFNI_APCPC, or AFNI_TLRC
    *   @param AFNIViewType The view type.
    */
    public void setAFNIViewType(int AFNIViewType) {
        this.AFNIViewType = AFNIViewType;
    }

    /**
    *   Sets the AFNITypeString to HEAD_ANAT_TYPE, HEAD_FUNC_TYPE,
    *   GEN_ANAT_TYPE, or GEN_FUNC_TYPE
    *   @param AFNITypeString The type.
    */
    public void setAFNITypeString(int AFNITypeString) {
        this.AFNITypeString = AFNITypeString;
    }

    /**
    *   Sets the most right point marker for +acpc
    *   to +tlrc transformation in dicom ordering
    *   @param pt - the marker point coordinates
    */
    public void setRightPt(Point3Df pt) {
        rightPt = pt;
    }

    /**
    *   Sets the AC superior edge marker for orig
    *   to ACPC transformation in dataset ordering
    *   @param pt - the marker point coordinates
    */
    public void setSuperiorEdge(Point3Df pt) {
        superiorEdge = pt;
    }

    /**
    *   Sets the AC superior edge marker for orig
    *   to ACPC transformation in dicom ordering
    *   using numbers not adjusted for origin and resolution
    *   @param pt - the marker point coordinates
    */
    public void setOriginalSuperiorEdge(Point3Df pt) {
        OriginalSuperiorEdge = pt;
    }

    /**
    *   Sets the most superior point marker for +acpc
    *   to +tlrc transformation in dicom ordering
    *   @param pt - the marker point coordinates
    */
    public void setSuperiorPt(Point3Df pt) {
        superiorPt = pt;
    }

    /**
    *  Displays the file information
    *  @param dlog    dialog box that is written to
    *  @param matrix  transformation matrix
    */
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix){
        JDialogText dialog = (JDialogText) dlog;
        int[] extents;
        int i,j;
        int AFNITypeString;
        int AFNIViewType;
        int axisOrientation[];
        int subBrickNumber;
        int subBrickIndex;
        int statCode;
        int followingParms;
        int numSamples;
        int numFitParam;
        int numNuisanceParam;
        int dof,ndof,ddof;
        float a,b;
        int numTrials;
        float prob;
        float shape;
        float mean;
        float scale;

        displayPrimaryInfo(dialog, matrix);
        dialog.append("\n\n                Other information\n\n");

        axisOrientation = getAxisOrientation();
        if ((axisOrientation[0] != ORI_R2L_TYPE) || (axisOrientation[1] != ORI_A2P_TYPE) ||
                (axisOrientation[2] != ORI_I2S_TYPE)) {
            isDicom = false;
        }
        else {
            isDicom = true;
        }

        AFNITypeString = getAFNITypeString();

        if (getExtents().length == 4) {
            subBrickNumber = getExtents()[3];
        }
        else {
            subBrickNumber = 1;
        }
        if ((AFNITypeString == HEAD_FUNC_TYPE) || (AFNITypeString == GEN_FUNC_TYPE)) {
            dialog.append("Number of sub-bricks: " + subBrickNumber + "\n");
        }

        dialog.append("AFNI view:            ");
        AFNIViewType = getAFNIViewType();
        switch(AFNIViewType) {
            case AFNI_ORIG:
                dialog.append("Original \n");
                break;
            case AFNI_ACPC:
                dialog.append("ACPC \n");
                break;
            case AFNI_TLRC:
                dialog.append("Talairach \n");
                break;
            default:
                dialog.append("Unknown \n");
        }


        dialog.append("AFNI type:            ");
        switch(AFNITypeString) {
            case HEAD_ANAT_TYPE:
                dialog.append("HEAD_ANAT_TYPE \n");
                break;
            case HEAD_FUNC_TYPE:
                dialog.append("HEAD_FUNC_TYPE \n");
                break;
            case GEN_ANAT_TYPE:
                dialog.append("GEN_ANAT_TYPE \n");
                break;
            case GEN_FUNC_TYPE:
                dialog.append("GEN_FUNC_TYPE \n");
                break;
            default:
                dialog.append("Unknown \n");
        }

        dialog.append("AFNI func type:       ");
        if ((AFNITypeString == HEAD_ANAT_TYPE) || (AFNITypeString == GEN_ANAT_TYPE)) {
            switch(funcType) {
                case ANAT_SPGR_TYPE:
                    dialog.append("ANAT_SPGR_TYPE \n");
                    break;
                case ANAT_FSE_TYPE:
                    dialog.append("ANAT_FSE_TYPE \n");
                    break;
                case ANAT_EPI_TYPE:
                    dialog.append("ANAT_EPI_TYPE \n");
                    break;
                case ANAT_MRAN_TYPE:
                    dialog.append("ANAT_MRAN_TYPE \n");
                    break;
                case ANAT_CT_TYPE:
                    dialog.append("ANAT_CT_TYPE \n");
                    break;
                case ANAT_SPECT_TYPE:
                    dialog.append("ANAT_SPECT_TYPE \n");
                    break;
                case ANAT_PET_TYPE:
                    dialog.append("ANAT_PET_TYPE \n");
                    break;
                case ANAT_MRA_TYPE:
                    dialog.append("ANAT_MRA_TYPE \n");
                    break;
                case ANAT_BMAP_TYPE:
                    dialog.append("ANAT_BMAP_TYPE \n");
                    break;
                case ANAT_DIFF_TYPE:
                    dialog.append("ANAT_DIFF_TYPE \n");
                    break;
                case ANAT_OMRI_TYPE:
                    dialog.append("ANAT_OMRI_TYPE \n");
                    break;
                case ANAT_BUCK_TYPE:
                    dialog.append("ANAT_BUCK_TYPE \n");
                    break;
                default:
                    dialog.append("Unknown type");
            } // switch(funcType)
        } // if ((AFNITypeString == HEAD_ANAT_TYPE) || (AFNITypeString == GEN_ANAT_TYPE))
        else { // HEAD_FUNC_TYPE or GEN_FUNC_TYPE
            switch(funcType) {
                case FUNC_FIM_TYPE:
                    dialog.append("FUNC_FIM_TYPE 1 value \n");
                    break;
                case FUNC_THR_TYPE:
                    dialog.append("FUNC_THR_TYPE \n");
                    break;
                case FUNC_COR_TYPE:
                    dialog.append("FUNC_COR_TYPE correlation \n");
                    break;
                case FUNC_TT_TYPE:
                    dialog.append("FUNC_TT_TYPE t-statistic \n");
                    break;
                case FUNC_FT_TYPE:
                    dialog.append("FUNC_FT_TYPE F-statistic \n");
                    break;
                case FUNC_ZT_TYPE:
                    dialog.append("FUNC_ZT_TYPE z-score \n");
                    break;
                case FUNC_CT_TYPE:
                    dialog.append("FUNC_CT_TYPE Chi squared \n");
                    break;
                case FUNC_BT_TYPE:
                    dialog.append("FUNC_BT_TYPE Beta stat \n");
                    break;
                case FUNC_BN_TYPE:
                    dialog.append("FUNC_BN_TYPE Binomial \n");
                    break;
                case FUNC_GT_TYPE:
                    dialog.append("FUNC_GT_TYPE Gamma \n");
                    break;
                case FUNC_PT_TYPE:
                    dialog.append("FUNC_PT_TYPE Poisson \n");
                    break;
                case FUNC_BUCK_TYPE:
                    dialog.append("FUNC_BUCK_TYPE bucket \n");
                    break;
                default:
                    dialog.append("Unknown type \n");
            } // switchType
        } // HEAD_FUNC_TYPE or GEN_FUNC_TYPE

        if (brickStatAux != null) {
            i = 0;
            while (i < brickStatAux.length) {
                subBrickIndex = (int)brickStatAux[i];
                dialog.append("BRICK_STATAUX[" + i + "]: " + subBrickIndex +
                              ", the sub-brick index\n");
                i++;
                statCode = (int)brickStatAux[i];
                switch(statCode) {
                    case FUNC_COR_TYPE:
                        dialog.append("BRICK_STATAUX[" + i + "]: " + statCode +
                                    " for FUNC_COR_TYPE with Correlation coeff\n");
                        i++;
                        followingParms = (int)brickStatAux[i];
                        dialog.append("BRICK_STATAUX[" + i + "]: " + followingParms +
                                    " for number of parameters that follow\n");
                        i++;
                        numSamples = (int)brickStatAux[i];
                        dialog.append("BRICK_STATAUX[" + i + "]: " + numSamples +
                                    ", the number of samples\n");
                        i++;
                        numFitParam = (int)brickStatAux[i];
                        dialog.append("BRICK_STATAUX[" + i + "]: " + numFitParam +
                                    ", the number of fitting parameters\n");
                        i++;
                        numNuisanceParam = (int)brickStatAux[i];
                        dialog.append("BRICK_STATAUX[" + i + "]: " + numNuisanceParam +
                                    ", the number of nuisance parameters\n");
                        i++;
                        break;
                    case FUNC_TT_TYPE:
                        dialog.append("BRICK_STATAUX[" + i + "]: " + statCode +
                                    " for FUNC_TT_TYPE with Student t\n");
                        i++;
                        followingParms = (int)brickStatAux[i];
                        dialog.append("BRICK_STATAUX[" + i + "]: " + followingParms +
                                    " for number of parameters that follow\n");
                        i++;
                        dof = (int)brickStatAux[i];
                        dialog.append("BRICK_STATAUX[" + i + "]: " + dof +
                                    " for degrees of freedom\n");
                        i++;
                        break;
                    case FUNC_FT_TYPE:
                        dialog.append("BRICK_STATAUX[" + i + "]: " + statCode +
                                    " for FUNC_FT_TYPE with F ratio\n");
                        i++;
                        followingParms = (int)brickStatAux[i];
                        dialog.append("BRICK_STATAUX[" + i + "]: " + followingParms +
                                    " for number of parameters that follow\n");
                        i++;
                        ndof = (int)brickStatAux[i];
                        dialog.append("BRICK_STATAUX[" + i + "]: " + ndof +
                                    " for numerator degrees of freedom\n");
                        i++;
                        ddof = (int)brickStatAux[i];
                        dialog.append("BRICK_STATAUX[" + i + "]: " + ddof +
                                    " for denominator degrees of freedom\n");
                        i++;
                        break;
                    case FUNC_ZT_TYPE:
                        dialog.append("BRICK_STATAUX[" + i + "]: " + statCode +
                                    " for FUNC_ZT_TYPE with Standard Normal\n");
                        i++;
                        followingParms = (int)brickStatAux[i];
                        dialog.append("BRICK_STATAUX[" + i + "]: " + followingParms +
                                    " for number of parameters that follow\n");
                        i++;
                        break;
                    case FUNC_CT_TYPE:
                        dialog.append("BRICK_STATAUX[" + i + "]: " + statCode +
                                    " for FUNC_CT_TYPE with Chi-squared\n");
                        i++;
                        followingParms = (int)brickStatAux[i];
                        dialog.append("BRICK_STATAUX[" + i + "]: " + followingParms +
                                    " for number of parameters that follow\n");
                        i++;
                        dof = (int)brickStatAux[i];
                        dialog.append("BRICK_STATAUX[" + i + "]: " + dof +
                                    " for degrees of freedom\n");
                        i++;
                        break;
                    case FUNC_BT_TYPE:
                        dialog.append("BRICK_STATAUX[" + i + "]: " + statCode +
                                    " for FUNC_BT_TYPE with Inomplete Beta\n");
                        i++;
                        followingParms = (int)brickStatAux[i];
                        dialog.append("BRICK_STATAUX[" + i + "]: " + followingParms +
                                    " for number of parameters that follow\n");
                        i++;
                        a = brickStatAux[i];
                        dialog.append("BRICK_STATAUX[" + i + "]: " + brickStatAux[i] +
                                    " for parameter a\n");
                        i++;
                        b = brickStatAux[i];
                        dialog.append("BRICK_STATAUX[" + i + "]: " + brickStatAux[i] +
                                    " for parameter b\n");
                        i++;
                        break;
                    case FUNC_BN_TYPE:
                        dialog.append("BRICK_STATAUX[" + i + "]: " + statCode +
                                    " for FUNC_BN_TYPE with Binomial\n");
                        i++;
                        followingParms = (int)brickStatAux[i];
                        dialog.append("BRICK_STATAUX[" + i + "]: " + followingParms +
                                    " for number of parameters that follow\n");
                                    i++;
                        numTrials = (int)brickStatAux[i];
                        dialog.append("BRICK_STATAUX[" + i + "]: " + numTrials +
                                    " number of trials\n");
                        i++;
                        prob = brickStatAux[i];
                        dialog.append("BRICK_STATAUX[" + i + "]: " + brickStatAux[i] +
                                    " for probability per trial\n");
                        i++;
                        break;
                    case FUNC_GT_TYPE:
                        dialog.append("BRICK_STATAUX[" + i + "]: " + statCode +
                                    " for FUNC_GT_TYPE with Gamma\n");
                        i++;
                        followingParms = (int)brickStatAux[i];
                        dialog.append("BRICK_STATAUX[" + i + "]: " + followingParms +
                                    " for number of parameters that follow\n");
                        i++;
                        shape = brickStatAux[i];
                        dialog.append("BRICK_STATAUX[" + i + "]: " + brickStatAux[i] +
                                    " for shape\n");
                        i++;
                        scale = brickStatAux[i];
                        dialog.append("BRICK_STATAUX[" + i + "]: " + brickStatAux[i] +
                                    " for scale\n");
                        i++;
                        break;
                    case FUNC_PT_TYPE:
                        dialog.append("BRICK_STATAUX[" + i + "]: " + statCode +
                                    " for FUNC_PT_TYPE with Poisson\n");
                        i++;
                        followingParms = (int)brickStatAux[i];
                        dialog.append("BRICK_STATAUX[" + i + "]: " + followingParms +
                                    " for number of parameters that follow\n");
                        i++;
                        mean = brickStatAux[i];
                        dialog.append("BRICK_STATAUX[" + i + "]: " + brickStatAux[i] +
                                    " for mean\n");
                        i++;
                        break;
                    default:
                        dialog.append("BRICK_STATAUX[" + i + "]: " + statCode +
                                    " for unknown FUNC_TYPE\n");
                        i++;
                        followingParms = (int)brickStatAux[i];
                        dialog.append("BRICK_STATAUX[" + i + "]: " + followingParms +
                                    " for number of parameters that follow\n");
                        i++;
                        for (j = 0; j < followingParms; j++) {
                            dialog.append("BRICK_STATAUX[" + i + "]: " + brickStatAux[i] +
                                                          "\n");
                            i++;
                        } // for (j = 0; j < followingParms; j++)
                        break;
                } // switch(brickStatAux[i])
            } // while (i < countEntries)
        } // if (brickStatAux != null)

        if (statAux != null) {
            i = 0;
            switch(funcType) {
                case FUNC_COR_TYPE:
                    numSamples = (int)statAux[i];
                    dialog.append("STAT_AUX[" + i + "]: " + numSamples +
                                ", the number of samples\n");
                    i++;
                    numFitParam = (int)statAux[i];
                    dialog.append("STAT_AUX[" + i + "]: " + numFitParam +
                                ", the number of fitting parameters\n");
                    i++;
                    numNuisanceParam = (int)statAux[i];
                    dialog.append("STAT_AUX[" + i + "]: " + numNuisanceParam +
                                ", the number of nuisance parameters\n");
                    break;
                case FUNC_TT_TYPE:
                    dof = (int)statAux[i];
                    dialog.append("STAT_AUX[" + i + "]: " + dof +
                                " for degrees of freedom\n");
                    break;
                case FUNC_FT_TYPE:
                    ndof = (int)statAux[i];
                    dialog.append("STAT_AUX[" + i + "]: " + ndof +
                                " for numerator degrees of freedom\n");
                    i++;
                    ddof = (int)statAux[i];
                    dialog.append("STAT_AUX[" + i + "]: " + ddof +
                                " for denominator degrees of freedom\n");
                    break;
                case FUNC_CT_TYPE:
                    dof = (int)statAux[i];
                    dialog.append("STAT_AUX[" + i + "]: " + dof +
                                " for degrees of freedom\n");
                    break;
                case FUNC_BT_TYPE:
                    a = statAux[i];
                    dialog.append("STAT_AUX[" + i + "]: " + statAux[i] +
                                " for parameter a\n");
                    i++;
                    b = statAux[i];
                    dialog.append("STAT_AUX[" + i + "]: " + statAux[i] +
                                " for parameter b\n");
                    break;
                case FUNC_BN_TYPE:
                    numTrials = (int)statAux[i];
                    dialog.append("STAT_AUX[" + i + "]: " + numTrials +
                                " number of trials\n");
                    i++;
                    prob = statAux[i];
                    dialog.append("STAT_AUX[" + i + "]: " + statAux[i] +
                                " for probability per trial\n");
                    break;
                case FUNC_GT_TYPE:
                    shape = statAux[i];
                    dialog.append("STAT_AUX[" + i + "]: " + statAux[i] +
                                " for shape\n");
                    i++;
                    scale = statAux[i];
                    dialog.append("STAT_AUX[" + i + "]: " + statAux[i] +
                                " for scale\n");
                    break;
                case FUNC_PT_TYPE:
                    mean = statAux[i];
                    dialog.append("STAT_AUX[" + i + "]: " + statAux[i] +
                                " for mean\n");
                    break;
                default:
                    for (i = 0; i < statAux.length; i++) {
                        dialog.append("STAT_AUX[" + i + "]: " + statAux[i] +
                                      "\n");
                    }
                    break;
            } // switch(statAux[i])
        } // if (statAux != null)

        if ((getExtents().length == 4) && ((AFNITypeString == HEAD_ANAT_TYPE) || (AFNITypeString == GEN_ANAT_TYPE))) {
        // 3D + time file
            if ((isDicom == false) || ((isDicom == true) && (origDicom == true))) {
                dialog.append("Number of slices with time offsets: " + slicesWithTimeOffsets +
                               "\n");
            }
            dialog.append("Time origin: " + timeOrigin + "\n");
            dialog.append("Time step: " + timeStep + "\n");
            dialog.append("Acquisition duration: " + acquisitionDuration + "\n");
            if ((slicesWithTimeOffsets > 0) &&
            ((isDicom == false) || ((isDicom == true) && (origDicom == true)))) {
                dialog.append("Z axis offset for slice-dependent time offsets: " +
                              zAxisOffset + "\n");
                dialog.append("Z axis step for slice-dependent time offsets: " +
                              zAxisStep + "\n");
                for (j = 0; j <tAxisOffsets.length; j++) {
                    dialog.append("Time axis offset[" + j + "]: " + tAxisOffsets[j] + "\n");
                }
            }
        } // 3D + time file

        if (label1 != null) {
            dialog.append("LABEL_1: " + label1 + "\n");
        }

        if (label2 != null) {
            dialog.append("LABEL_2: " + label2 + "\n");
        }

        if (datasetName != null) {
            dialog.append("DATASET_NAME: " + datasetName + "\n");
        }

        if (idcodeString != null) {
            dialog.append("IDCODE_STRING: " + idcodeString + "\n");
        }

        if (idcodeDate != null) {
            dialog.append("IDCODE_DATE: " + idcodeDate + "\n");
        }

        if (noteNumber001 != null) {
            dialog.append("NOTE_NUMBER_001: " + noteNumber001 + "\n");
        }

        if (noteDate001 != null) {
            dialog.append("NOTE_DATE_001: " + noteDate001 + "\n");
        }

        if (noteNumber002 != null) {
            dialog.append("NOTE_NUMBER_002: " + noteNumber002 + "\n");
        }

        if (noteDate002 != null) {
            dialog.append("NOTE_DATE_002: " + noteDate002 + "\n");
        }

        if (noteNumber003 != null) {
            dialog.append("NOTE_NUMBER_003: " + noteNumber003 + "\n");
        }

        if (noteDate003 != null) {
            dialog.append("NOTE_DATE_003: " + noteDate003 + "\n");
        }

        if (noteNumber004 != null) {
            dialog.append("NOTE_NUMBER_004: " + noteNumber004 + "\n");
        }

        if (noteDate004 != null) {
            dialog.append("NOTE_DATE_004: " + noteDate004 + "\n");
        }

        if (noteNumber005 != null) {
            dialog.append("NOTE_NUMBER_005: " + noteNumber005 + "\n");
        }

        if (noteDate005 != null) {
            dialog.append("NOTE_DATE_005: " + noteDate005 + "\n");
        }
    }
}
