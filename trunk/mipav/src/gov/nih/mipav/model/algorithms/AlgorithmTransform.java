package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.*;

import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;


/**
 * Transforms Volume by resampling using transformation matrix and the choice of nearest-neighbor, trilinear
 * interpolation, 3rd order Bspline, 4th order Bspline, cubic Lagrangian, quintic Lagrangian, heptic Lagrangian, or
 * windowed sinc. Must indicate output volume's desired resolutions and dimensions.
 *
 * <p>Also includes static methods to transform images. Once caution - these static methods are NOT run in a separate
 * thread. Consequently, if a progress bar is sent into the method to update progress, it should not include a cancel
 * button because the user is unable to cancel a static transformation. If we want a transformation to have the ability
 * to be cancelled, we should NOT use the static method, but rather construct a new AlgorithmTransform and run the
 * algorithm using the standard .run() method.</p>
 *
 * <p>NOTE for possible improvements in the future. To move images into "mm" space we presently multiple by voxel
 * resolutions as we loop through the dimensions. A more efficent method is to modify the scale values of the the
 * transformation matrix (i.e. the diagonals). This would speed the process by reducing the number of mults and
 * divisions. Not sure how much but faster is better. Change should happen here and in the JDialogTransform interface.
 * </p>
 *
 * <p>This version of AlgorithmTransform includes a flag (passed as the last parameter) indicating whether to pad the
 * image volume.</p>
 *
 * @version  0.1 Nov, 1999
 * @author   Delia McGarry
 * @author   William Gandler
 * @author   Matthew McAuliffe
 * @author   Zohara Cohen
 */
public class AlgorithmTransform extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Trilinear interpolation. */
    public static final int TRILINEAR = 0;

    /** Biilinear interpolation. */
    public static final int BILINEAR = 1;

    /** Nearest neighbor interpolation. */
    public static final int NEAREST_NEIGHBOR = 2;

    /** Cubic bspline interpolation. */
    public static final int BSPLINE3 = 3;

    /** Quadratic bspline interpolation. */
    public static final int BSPLINE4 = 4;

    /** Cubic lagrangian interpolation. */
    public static final int CUBIC_LAGRANGIAN = 5;

    /** Quintic lagrangian interpolation. */
    public static final int QUINTIC_LAGRANGIAN = 6;

    /** Heptic lagrangian interpolation. */
    public static final int HEPTIC_LAGRANGIAN = 7;

    /** Windowed sinc interpolation. */
    public static final int WSINC = 8;

    /** DOCUMENT ME! */
    private static boolean updateOrigin = false;

    /** DOCUMENT ME! */
    private static float[] imgOrigin = new float[3];

    /** DOCUMENT ME! */
    private static int imgOrient;

    /** DOCUMENT ME! */
    private static int[] axisOrient = new int[3];

    /** DOCUMENT ME! */
    private static int[] direct = new int[3];

    /** DOCUMENT ME! */
    private static int[] margins = new int[6];

    /** DOCUMENT ME! */
    private static float startPos;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int bufferFactor;

    /** DOCUMENT ME! */
    private boolean canPad = true;

    /** DOCUMENT ME! */
    private Vector3f center = null;

    /** DOCUMENT ME! */
    private boolean clip = true;

    /** DOCUMENT ME! */
    private float[] destResolutions;

    /** DOCUMENT ME! */
    private int DIM;

    /** DOCUMENT ME! */
    private boolean do25D;

    /** DOCUMENT ME! */
    private boolean doCenter = false;

    /** DOCUMENT ME! */
    private float[] imgBuf = null;

    /** DOCUMENT ME! */
    private float[] imgBuf2 = null;

    /** DOCUMENT ME! */
    private int imgLength, imgLength2; // length of buffers used for import and export Data

    /** DOCUMENT ME! */
    private int interp;

    /** flag for determining if the transform is for scanner anatomical (->AXIAL). */
    private boolean isSATransform = false;

    /** DOCUMENT ME! */
    private int iXdim, iYdim, iZdim, iTdim;

    /** DOCUMENT ME! */
    private float iXres, iYres, iZres;

    /** DOCUMENT ME! */
    private int[] oUnits;

    /** DOCUMENT ME! */
    private int oXdim, oYdim, oZdim, oTdim;

    /** DOCUMENT ME! */
    private float oXres, oYres, oZres;

    /** DOCUMENT ME! */
    private boolean pad = true;

    /** DOCUMENT ME! */
    private int padVal = 0;

    /** DOCUMENT ME! */
    private ModelImage srcImage, destImage, maskImage;

    /** DOCUMENT ME! */
    private boolean transformVOI = false;

    /** DOCUMENT ME! */
    private TransMatrix transMatrix;
    
    private boolean haveCentered = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * 2D constructor for transformation algorithm. Also used for 2.5D algorithms on 3D and 4D images.
     *
     * @param  srcImage  ModelImage to be transformed
     * @param  xfrm      Transformation matrix to be applied
     * @param  interp    Type of interpolation (NEAREST_NEIGHBOR, BILINEAR, BSPLINE3, BSPLINE4, etc)
     * @param  oXres     X resolution of output image
     * @param  oYres     Y resolution of output image
     * @param  oXdim     X dimension of output image
     * @param  oYdim     Y dimension of output image
     * @param  tVOI      if <code>true</code> the VOI should be transformed with the volume
     * @param  clip      if <code>true</code> output range is clipped to input range
     * @param  pad       if <code>true</code> output image is padded so that none of the image is clipped
     */
    public AlgorithmTransform(ModelImage srcImage, TransMatrix xfrm, int interp, float oXres, float oYres, int oXdim,
                              int oYdim, boolean tVOI, boolean clip, boolean pad) {
        this(srcImage, xfrm, interp, oXres, oYres, oXdim, oYdim,
             new int[] { srcImage.getUnitsOfMeasure(0), srcImage.getUnitsOfMeasure(1) }, tVOI, clip, pad);
    }

    /**
     * Creates a new AlgorithmTransform object.
     *
     * @param  srcImage  DOCUMENT ME!
     * @param  xfrm      DOCUMENT ME!
     * @param  interp    DOCUMENT ME!
     * @param  oXres     DOCUMENT ME!
     * @param  oYres     DOCUMENT ME!
     * @param  oXdim     DOCUMENT ME!
     * @param  oYdim     DOCUMENT ME!
     * @param  units     DOCUMENT ME!
     * @param  tVOI      DOCUMENT ME!
     * @param  clip      DOCUMENT ME!
     * @param  pad       DOCUMENT ME!
     */
    public AlgorithmTransform(ModelImage srcImage, TransMatrix xfrm, int interp, float oXres, float oYres, int oXdim,
                              int oYdim, int[] units, boolean tVOI, boolean clip, boolean pad) {
        super(null, srcImage);
        transformVOI = tVOI;
        this.srcImage = srcImage;
        this.clip = clip;
        this.pad = pad;

        int[] extents;
        String name = JDialogBase.makeImageName(srcImage.getImageName(), "_transform");

        imgOrient = srcImage.getFileInfo(0).getImageOrientation();

        axisOrient = new int[srcImage.getFileInfo(0).getAxisOrientation().length];

        for (int i = 0; i < axisOrient.length; i++) {
            axisOrient[i] = srcImage.getFileInfo(0).getAxisOrientation()[i];
        }

        imgOrigin = (float[]) srcImage.getFileInfo(0).getOrigin().clone();

        int type = srcImage.getType();

        /* Read the direction vector from the MipavCoordinateSystems class: */
        direct = MipavCoordinateSystems.getModelDirections(srcImage);
        DIM = srcImage.getNDims();

        if (pad) {

            if (interp != BILINEAR) {
                canPad = false;
            }

            margins = getImageMargins(srcImage, xfrm, oXres, oYres);
            Preferences.debug("Padding is " + margins[0] + ", " + margins[1] + ".\n");
            updateOriginMargins2D();

            // System.out.println("Image origin with padding: " +imgOrigin[0] +" " +imgOrigin[1]);
            oXdim = oXdim + margins[0] + margins[2];
            oYdim = oYdim + margins[1] + margins[3];
        } else {

            for (int m = 0; m < 4; m++) {
                margins[m] = 0;
            }
        }

        if (srcImage.getNDims() == 2) {
            DIM = 2;
            do25D = false;
            extents = new int[] { oXdim, oYdim };
            destResolutions = new float[] { oXres, oYres };

            if (((interp == WSINC) || (interp == CUBIC_LAGRANGIAN) || (interp == QUINTIC_LAGRANGIAN) ||
                     (interp == HEPTIC_LAGRANGIAN)) && (!clip) &&
                    ((type == ModelStorageBase.BYTE) || (type == ModelStorageBase.UBYTE))) {
                destImage = new ModelImage(ModelStorageBase.SHORT, extents, name);
            } else if (((interp == WSINC) || (interp == CUBIC_LAGRANGIAN) || (interp == QUINTIC_LAGRANGIAN) ||
                            (interp == HEPTIC_LAGRANGIAN)) && (!clip) && (type == ModelStorageBase.USHORT)) {
                destImage = new ModelImage(ModelStorageBase.INTEGER, extents, name);
            } else if (((interp == WSINC) || (interp == CUBIC_LAGRANGIAN) || (interp == QUINTIC_LAGRANGIAN) ||
                            (interp == HEPTIC_LAGRANGIAN)) && (!clip) && (type == ModelStorageBase.UINTEGER)) {
                destImage = new ModelImage(ModelStorageBase.LONG, extents, name);
            } else {
                destImage = new ModelImage(type, extents, name);
            }

        } // end of if (srcImage.getNDims == 2)
        else if (srcImage.getNDims() == 3) {

            DIM = 3;
            do25D = true;
            iZdim = srcImage.getExtents()[2];
            oZdim = iZdim;
            startPos = imgOrigin[2]; // temporarily set origin for all slices as origin of first slice
            extents = new int[] { oXdim, oYdim, oZdim };

            destResolutions = new float[] { oXres, oYres, srcImage.getFileInfo(0).getResolutions()[2] };

            if (((interp == WSINC) || (interp == CUBIC_LAGRANGIAN) || (interp == QUINTIC_LAGRANGIAN) ||
                     (interp == HEPTIC_LAGRANGIAN)) && (!clip) &&
                    ((type == ModelStorageBase.BYTE) || (type == ModelStorageBase.UBYTE))) {
                destImage = new ModelImage(ModelStorageBase.SHORT, extents, name);
            } else if (((interp == WSINC) || (interp == CUBIC_LAGRANGIAN) || (interp == QUINTIC_LAGRANGIAN) ||
                            (interp == HEPTIC_LAGRANGIAN)) && (!clip) && (type == ModelStorageBase.USHORT)) {
                destImage = new ModelImage(ModelStorageBase.INTEGER, extents, name);
            } else if (((interp == WSINC) || (interp == CUBIC_LAGRANGIAN) || (interp == QUINTIC_LAGRANGIAN) ||
                            (interp == HEPTIC_LAGRANGIAN)) && (!clip) && (type == ModelStorageBase.UINTEGER)) {
                destImage = new ModelImage(ModelStorageBase.LONG, extents, name);
            } else {
                destImage = new ModelImage(type, extents, name);
            }
        } // end of else if (srcImage.getNDims() == 3)
        else { // (srcImage.getNDims() == 4) {
            DIM = 4;
            do25D = true;
            iZdim = srcImage.getExtents()[2];
            oZdim = iZdim;
            iTdim = srcImage.getExtents()[3];
            oTdim = iTdim;
            startPos = imgOrigin[2]; // temporarily set origin for all slices as origin of first slice
            extents = new int[] { oXdim, oYdim, oZdim, oTdim };
            destResolutions = new float[] {
                                  oXres, oYres, srcImage.getFileInfo(0).getResolutions()[2],
                                  srcImage.getFileInfo(0).getResolutions()[3]
                              };

            if (((interp == WSINC) || (interp == CUBIC_LAGRANGIAN) || (interp == QUINTIC_LAGRANGIAN) ||
                     (interp == HEPTIC_LAGRANGIAN)) && (!clip) &&
                    ((type == ModelStorageBase.BYTE) || (type == ModelStorageBase.UBYTE))) {
                destImage = new ModelImage(ModelStorageBase.SHORT, extents, name);
            } else if (((interp == WSINC) || (interp == CUBIC_LAGRANGIAN) || (interp == QUINTIC_LAGRANGIAN) ||
                            (interp == HEPTIC_LAGRANGIAN)) && (!clip) && (type == ModelStorageBase.USHORT)) {
                destImage = new ModelImage(ModelStorageBase.INTEGER, extents, name);
            } else if (((interp == WSINC) || (interp == CUBIC_LAGRANGIAN) || (interp == QUINTIC_LAGRANGIAN) ||
                            (interp == HEPTIC_LAGRANGIAN)) && (!clip) && (type == ModelStorageBase.UINTEGER)) {
                destImage = new ModelImage(ModelStorageBase.LONG, extents, name);
            } else {
                destImage = new ModelImage(type, extents, name);
            }
        } // end of else for srcImage.getNDims() == 4

        transMatrix = xfrm;
        iXres = srcImage.getFileInfo(0).getResolutions()[0];
        iYres = srcImage.getFileInfo(0).getResolutions()[1];
        iXdim = srcImage.getExtents()[0];
        iYdim = srcImage.getExtents()[1];

        this.oXres = oXres;
        this.oYres = oYres;
        this.oXdim = oXdim;
        this.oYdim = oYdim;
        this.oUnits = units;
        this.interp = interp;
    }
    
    /**
     * Creates a new AlgorithmTransform object.
     *
     * @param  srcImage  DOCUMENT ME!
     * @param  xfrm      DOCUMENT ME!
     * @param  interp    DOCUMENT ME!
     * @param  oXres     DOCUMENT ME!
     * @param  oYres     DOCUMENT ME!
     * @param  oXdim     DOCUMENT ME!
     * @param  oYdim     DOCUMENT ME!
     * @param  units     DOCUMENT ME!
     * @param  tVOI      DOCUMENT ME!
     * @param  clip      DOCUMENT ME!
     * @param  pad       DOCUMENT ME!
     * @param  doCenter
     * @param  center
     */
    public AlgorithmTransform(ModelImage srcImage, TransMatrix xfrm, int interp, float oXres, float oYres, int oXdim,
                              int oYdim, int[] units, boolean tVOI, boolean clip, boolean pad, boolean doCenter, Vector3f center) {
        super(null, srcImage);
        transformVOI = tVOI;
        this.srcImage = srcImage;
        this.clip = clip;
        this.pad = pad;
        this.doCenter = doCenter;
        this.center = center;

        int[] extents;
        TransMatrix trans;
        TransMatrix xfrmC;
        String name = JDialogBase.makeImageName(srcImage.getImageName(), "_transform");

        imgOrient = srcImage.getFileInfo(0).getImageOrientation();

        axisOrient = new int[srcImage.getFileInfo(0).getAxisOrientation().length];

        for (int i = 0; i < axisOrient.length; i++) {
            axisOrient[i] = srcImage.getFileInfo(0).getAxisOrientation()[i];
        }

        imgOrigin = (float[]) srcImage.getFileInfo(0).getOrigin().clone();

        int type = srcImage.getType();

        /* Read the direction vector from the MipavCoordinateSystems class: */
        direct = MipavCoordinateSystems.getModelDirections(srcImage);
        DIM = srcImage.getNDims();

        if (pad) {

            if (interp != BILINEAR) {
                canPad = false;
            }
            
            if (doCenter) {
                xfrmC = new TransMatrix(3);
                //xfrmC.identity();
                xfrmC.setTranslate(center.X, center.Y);
                xfrm.MultLeft(xfrmC);
                xfrm.setTranslate(-center.X, -center.Y);
                haveCentered = true;
            }

            margins = getImageMargins(srcImage, xfrm, oXres, oYres);
            Preferences.debug("Padding is " + margins[0] + ", " + margins[1] + ".\n");
            updateOriginMargins2D();

            // System.out.println("Image origin with padding: " +imgOrigin[0] +" " +imgOrigin[1]);
            oXdim = oXdim + margins[0] + margins[2];
            oYdim = oYdim + margins[1] + margins[3];
        } else {

            for (int m = 0; m < 4; m++) {
                margins[m] = 0;
            }
        }

        if (srcImage.getNDims() == 2) {
            DIM = 2;
            do25D = false;
            extents = new int[] { oXdim, oYdim };
            destResolutions = new float[] { oXres, oYres };

            if (((interp == WSINC) || (interp == CUBIC_LAGRANGIAN) || (interp == QUINTIC_LAGRANGIAN) ||
                     (interp == HEPTIC_LAGRANGIAN)) && (!clip) &&
                    ((type == ModelStorageBase.BYTE) || (type == ModelStorageBase.UBYTE))) {
                destImage = new ModelImage(ModelStorageBase.SHORT, extents, name);
            } else if (((interp == WSINC) || (interp == CUBIC_LAGRANGIAN) || (interp == QUINTIC_LAGRANGIAN) ||
                            (interp == HEPTIC_LAGRANGIAN)) && (!clip) && (type == ModelStorageBase.USHORT)) {
                destImage = new ModelImage(ModelStorageBase.INTEGER, extents, name);
            } else if (((interp == WSINC) || (interp == CUBIC_LAGRANGIAN) || (interp == QUINTIC_LAGRANGIAN) ||
                            (interp == HEPTIC_LAGRANGIAN)) && (!clip) && (type == ModelStorageBase.UINTEGER)) {
                destImage = new ModelImage(ModelStorageBase.LONG, extents, name);
            } else {
                destImage = new ModelImage(type, extents, name);
            }

        } // end of if (srcImage.getNDims == 2)
        else if (srcImage.getNDims() == 3) {

            DIM = 3;
            do25D = true;
            iZdim = srcImage.getExtents()[2];
            oZdim = iZdim;
            startPos = imgOrigin[2]; // temporarily set origin for all slices as origin of first slice
            extents = new int[] { oXdim, oYdim, oZdim };

            destResolutions = new float[] { oXres, oYres, srcImage.getFileInfo(0).getResolutions()[2] };

            if (((interp == WSINC) || (interp == CUBIC_LAGRANGIAN) || (interp == QUINTIC_LAGRANGIAN) ||
                     (interp == HEPTIC_LAGRANGIAN)) && (!clip) &&
                    ((type == ModelStorageBase.BYTE) || (type == ModelStorageBase.UBYTE))) {
                destImage = new ModelImage(ModelStorageBase.SHORT, extents, name);
            } else if (((interp == WSINC) || (interp == CUBIC_LAGRANGIAN) || (interp == QUINTIC_LAGRANGIAN) ||
                            (interp == HEPTIC_LAGRANGIAN)) && (!clip) && (type == ModelStorageBase.USHORT)) {
                destImage = new ModelImage(ModelStorageBase.INTEGER, extents, name);
            } else if (((interp == WSINC) || (interp == CUBIC_LAGRANGIAN) || (interp == QUINTIC_LAGRANGIAN) ||
                            (interp == HEPTIC_LAGRANGIAN)) && (!clip) && (type == ModelStorageBase.UINTEGER)) {
                destImage = new ModelImage(ModelStorageBase.LONG, extents, name);
            } else {
                destImage = new ModelImage(type, extents, name);
            }
        } // end of else if (srcImage.getNDims() == 3)
        else { // (srcImage.getNDims() == 4) {
            DIM = 4;
            do25D = true;
            iZdim = srcImage.getExtents()[2];
            oZdim = iZdim;
            iTdim = srcImage.getExtents()[3];
            oTdim = iTdim;
            startPos = imgOrigin[2]; // temporarily set origin for all slices as origin of first slice
            extents = new int[] { oXdim, oYdim, oZdim, oTdim };
            destResolutions = new float[] {
                                  oXres, oYres, srcImage.getFileInfo(0).getResolutions()[2],
                                  srcImage.getFileInfo(0).getResolutions()[3]
                              };

            if (((interp == WSINC) || (interp == CUBIC_LAGRANGIAN) || (interp == QUINTIC_LAGRANGIAN) ||
                     (interp == HEPTIC_LAGRANGIAN)) && (!clip) &&
                    ((type == ModelStorageBase.BYTE) || (type == ModelStorageBase.UBYTE))) {
                destImage = new ModelImage(ModelStorageBase.SHORT, extents, name);
            } else if (((interp == WSINC) || (interp == CUBIC_LAGRANGIAN) || (interp == QUINTIC_LAGRANGIAN) ||
                            (interp == HEPTIC_LAGRANGIAN)) && (!clip) && (type == ModelStorageBase.USHORT)) {
                destImage = new ModelImage(ModelStorageBase.INTEGER, extents, name);
            } else if (((interp == WSINC) || (interp == CUBIC_LAGRANGIAN) || (interp == QUINTIC_LAGRANGIAN) ||
                            (interp == HEPTIC_LAGRANGIAN)) && (!clip) && (type == ModelStorageBase.UINTEGER)) {
                destImage = new ModelImage(ModelStorageBase.LONG, extents, name);
            } else {
                destImage = new ModelImage(type, extents, name);
            }
        } // end of else for srcImage.getNDims() == 4

        transMatrix = xfrm;
        iXres = srcImage.getFileInfo(0).getResolutions()[0];
        iYres = srcImage.getFileInfo(0).getResolutions()[1];
        iXdim = srcImage.getExtents()[0];
        iYdim = srcImage.getExtents()[1];

        this.oXres = oXres;
        this.oYres = oYres;
        this.oXdim = oXdim;
        this.oYdim = oYdim;
        this.oUnits = units;
        this.interp = interp;
    }

    /**
     * 3D constructor for transformation algorithm. Also used for 3D algorithms on 4D images.
     *
     * @param  _srcImage  ModelImage to be transformed
     * @param  xfrm       Transformation matrix to be applied
     * @param  interp     Type of interpolation (NEAREST_NEIGHBOR, TRILINEAR, BSPLINE3, BSPLINE4, etc)
     * @param  _oXres     X resolution of output image
     * @param  _oYres     Y resolution of output image
     * @param  _oZres     Z resolution of output image
     * @param  _oXdim     X dimension of output image
     * @param  _oYdim     Y dimension of output image
     * @param  _oZdim     Z dimension of output image
     * @param  tVOI       if <code>true</code> the VOI should be transformed with the volume
     * @param  clip       if <code>true</code> output range is clipped to input range
     * @param  pad        if <code>true</code> output image is padded so that none of the image is clipped
     */
    public AlgorithmTransform(ModelImage _srcImage, TransMatrix xfrm, int interp, float _oXres, float _oYres,
                              float _oZres, int _oXdim, int _oYdim, int _oZdim, boolean tVOI, boolean clip,
                              boolean pad) {
        this(_srcImage, xfrm, interp, _oXres, _oYres, _oZres, _oXdim, _oYdim, _oZdim,
             new int[] {
                 _srcImage.getUnitsOfMeasure(0), _srcImage.getUnitsOfMeasure(1), _srcImage.getUnitsOfMeasure(2)
             }, tVOI, clip, pad);


    }

    /**
     * Creates a new $class.name$ object.
     *
     * @param  _srcImage  DOCUMENT ME!
     * @param  xfrm       DOCUMENT ME!
     * @param  interp     DOCUMENT ME!
     * @param  _oXres     DOCUMENT ME!
     * @param  _oYres     DOCUMENT ME!
     * @param  _oZres     DOCUMENT ME!
     * @param  _oXdim     DOCUMENT ME!
     * @param  _oYdim     DOCUMENT ME!
     * @param  _oZdim     DOCUMENT ME!
     * @param  units      DOCUMENT ME!
     * @param  tVOI       DOCUMENT ME!
     * @param  clip       DOCUMENT ME!
     * @param  pad        DOCUMENT ME!
     */
    public AlgorithmTransform(ModelImage _srcImage, TransMatrix xfrm, int interp, float _oXres, float _oYres,
                              float _oZres, int _oXdim, int _oYdim, int _oZdim, int[] units, boolean tVOI, boolean clip,
                              boolean pad) {
        super(null, _srcImage);
        this.interp = interp;
        this.srcImage = _srcImage;
        this.transMatrix = xfrm;
        transformVOI = tVOI;
        this.clip = clip;
        this.pad = pad;


        this.oXres = _oXres;
        this.oYres = _oYres;
        this.oZres = _oZres;
        this.oXdim = _oXdim;
        this.oYdim = _oYdim;
        this.oZdim = _oZdim;

        this.oUnits = units;

        DIM = srcImage.getNDims();
        imgOrigin = (float[]) srcImage.getFileInfo(0).getOrigin().clone();
        axisOrient = new int[srcImage.getFileInfo(0).getAxisOrientation().length];

        for (int i = 0; i < axisOrient.length; i++) {
            axisOrient[i] = srcImage.getFileInfo(0).getAxisOrientation()[i];
        }

        imgOrient = srcImage.getFileInfo(0).getImageOrientation();
        iXres = srcImage.getFileInfo(0).getResolutions()[0];
        iYres = srcImage.getFileInfo(0).getResolutions()[1];
        iZres = srcImage.getFileInfo(0).getResolutions()[2];
        iXdim = srcImage.getExtents()[0];
        iYdim = srcImage.getExtents()[1];
        iZdim = srcImage.getExtents()[2];

        /* Read the direction vector from the MipavCoordinateSystems class: */
        direct = MipavCoordinateSystems.getModelDirections(srcImage);

        // System.out.println("Directions are " +direct[0] +", " +direct[1] +" and " +direct[2]);
        if (pad) {

            if ((interp != TRILINEAR) || (DIM != 3)) {
                canPad = false;
            }

            margins = getImageMargins(srcImage, xfrm, oXres, oYres, oZres);
            Preferences.debug("Padding is " + margins[0] + ", " + margins[1] + " and " + margins[2] + ".\n");
            updateOriginMargins();

            // System.out.println("Image origin with padding: " +imgOrigin[0] +" " +imgOrigin[1] +" " +imgOrigin[2]);
            oXdim = oXdim + margins[0] + margins[3];
            oYdim = oYdim + margins[1] + margins[4];
            oZdim = oZdim + margins[2] + margins[5];
        } else {

            for (int m = 0; m < 6; m++) {
                margins[m] = 0;
            }
        }

        startPos = imgOrigin[2];

        int[] extents;

        if (DIM == 3) {
            destResolutions = new float[3];
            extents = new int[3];

            if (pad) {
                do25D = false;
            }

            if (do25D) {
                oZres = iZres;
            }
        } else { // DIM ==4
            do25D = false;
            destResolutions = new float[4];
            extents = new int[4];
        }

        destResolutions[0] = oXres;
        destResolutions[1] = oYres;
        destResolutions[2] = oZres;

        extents[0] = oXdim;
        extents[1] = oYdim;
        extents[2] = oZdim;


        if (DIM == 4) {
            iTdim = srcImage.getExtents()[3];
            oTdim = iTdim;
            extents[3] = oTdim;
            destResolutions[3] = srcImage.getFileInfo(0).getResolutions()[3];
        }

        String name = JDialogBase.makeImageName(srcImage.getImageName(), "_transform");
        int type = srcImage.getType();

        if (DIM == 3) {

            if (((interp == WSINC) || (interp == CUBIC_LAGRANGIAN) || (interp == QUINTIC_LAGRANGIAN) ||
                     (interp == HEPTIC_LAGRANGIAN)) && (!clip) &&
                    ((type == ModelStorageBase.BYTE) || (type == ModelStorageBase.UBYTE))) {
                type = ModelStorageBase.SHORT;
            } else if (((interp == WSINC) || (interp == CUBIC_LAGRANGIAN) || (interp == QUINTIC_LAGRANGIAN) ||
                            (interp == HEPTIC_LAGRANGIAN)) && (!clip) && (type == ModelStorageBase.USHORT)) {
                type = ModelStorageBase.INTEGER;
            } else if (((interp == WSINC) || (interp == CUBIC_LAGRANGIAN) || (interp == QUINTIC_LAGRANGIAN) ||
                            (interp == HEPTIC_LAGRANGIAN)) && (!clip) && (type == ModelStorageBase.UINTEGER)) {
                type = ModelStorageBase.LONG;
            }
        } // end of if DIM == 3
        else { // DIM == 4

            if (((interp == WSINC) || (interp == CUBIC_LAGRANGIAN) || (interp == QUINTIC_LAGRANGIAN) ||
                     (interp == HEPTIC_LAGRANGIAN)) && (!clip) &&
                    ((type == ModelStorageBase.BYTE) || (type == ModelStorageBase.UBYTE))) {
                type = ModelStorageBase.SHORT;
            } else if (((interp == WSINC) || (interp == CUBIC_LAGRANGIAN) || (interp == QUINTIC_LAGRANGIAN) ||
                            (interp == HEPTIC_LAGRANGIAN)) && (!clip) && (type == ModelStorageBase.USHORT)) {
                type = ModelStorageBase.INTEGER;
            } else if (((interp == WSINC) || (interp == CUBIC_LAGRANGIAN) || (interp == QUINTIC_LAGRANGIAN) ||
                            (interp == HEPTIC_LAGRANGIAN)) && (!clip) && (type == ModelStorageBase.UINTEGER)) {
                type = ModelStorageBase.LONG;
            }
        } // end of DIM == 4

        destImage = new ModelImage(type, extents, name);
    }
    
    /**
     * Creates a new $class.name$ object.
     *
     * @param  _srcImage  DOCUMENT ME!
     * @param  xfrm       DOCUMENT ME!
     * @param  interp     DOCUMENT ME!
     * @param  _oXres     DOCUMENT ME!
     * @param  _oYres     DOCUMENT ME!
     * @param  _oZres     DOCUMENT ME!
     * @param  _oXdim     DOCUMENT ME!
     * @param  _oYdim     DOCUMENT ME!
     * @param  _oZdim     DOCUMENT ME!
     * @param  units      DOCUMENT ME!
     * @param  tVOI       DOCUMENT ME!
     * @param  clip       DOCUMENT ME!
     * @param  pad        DOCUMENT ME!
     * @param  doCenter
     * @param  center
     */
    public AlgorithmTransform(ModelImage _srcImage, TransMatrix xfrm, int interp, float _oXres, float _oYres,
                              float _oZres, int _oXdim, int _oYdim, int _oZdim, int[] units, boolean tVOI, boolean clip,
                              boolean pad, boolean doCenter, Vector3f center) {
        super(null, _srcImage);
        this.interp = interp;
        this.srcImage = _srcImage;
        this.transMatrix = xfrm;
        transformVOI = tVOI;
        this.clip = clip;
        this.pad = pad;
        this.doCenter = doCenter;
        this.center = center;

        this.oXres = _oXres;
        this.oYres = _oYres;
        this.oZres = _oZres;
        this.oXdim = _oXdim;
        this.oYdim = _oYdim;
        this.oZdim = _oZdim;

        this.oUnits = units;

        TransMatrix xfrmC;
        DIM = srcImage.getNDims();
        imgOrigin = (float[]) srcImage.getFileInfo(0).getOrigin().clone();
        axisOrient = new int[srcImage.getFileInfo(0).getAxisOrientation().length];

        for (int i = 0; i < axisOrient.length; i++) {
            axisOrient[i] = srcImage.getFileInfo(0).getAxisOrientation()[i];
        }

        imgOrient = srcImage.getFileInfo(0).getImageOrientation();
        iXres = srcImage.getFileInfo(0).getResolutions()[0];
        iYres = srcImage.getFileInfo(0).getResolutions()[1];
        iZres = srcImage.getFileInfo(0).getResolutions()[2];
        iXdim = srcImage.getExtents()[0];
        iYdim = srcImage.getExtents()[1];
        iZdim = srcImage.getExtents()[2];

        /* Read the direction vector from the MipavCoordinateSystems class: */
        direct = MipavCoordinateSystems.getModelDirections(srcImage);

        // System.out.println("Directions are " +direct[0] +", " +direct[1] +" and " +direct[2]);
        if (pad) {

            if ((interp != TRILINEAR) || (DIM != 3)) {
                canPad = false;
            }
            
            if (doCenter) {
                xfrmC = new TransMatrix(4);
                //xfrmC.identity();
                xfrmC.setTranslate(center.X, center.Y, center.Z);
                xfrm.MultLeft(xfrmC);
                xfrm.setTranslate(-center.X, -center.Y, -center.Z);
                haveCentered = true;
            }

            margins = getImageMargins(srcImage, xfrm, oXres, oYres, oZres);
            Preferences.debug("Padding is " + margins[0] + ", " + margins[1] + " and " + margins[2] + ".\n");
            updateOriginMargins();

            // System.out.println("Image origin with padding: " +imgOrigin[0] +" " +imgOrigin[1] +" " +imgOrigin[2]);
            oXdim = oXdim + margins[0] + margins[3];
            oYdim = oYdim + margins[1] + margins[4];
            oZdim = oZdim + margins[2] + margins[5];
        } else {

            for (int m = 0; m < 6; m++) {
                margins[m] = 0;
            }
        }

        startPos = imgOrigin[2];

        int[] extents;

        if (DIM == 3) {
            destResolutions = new float[3];
            extents = new int[3];

            if (pad) {
                do25D = false;
            }

            if (do25D) {
                oZres = iZres;
            }
        } else { // DIM ==4
            do25D = false;
            destResolutions = new float[4];
            extents = new int[4];
        }

        destResolutions[0] = oXres;
        destResolutions[1] = oYres;
        destResolutions[2] = oZres;

        extents[0] = oXdim;
        extents[1] = oYdim;
        extents[2] = oZdim;


        if (DIM == 4) {
            iTdim = srcImage.getExtents()[3];
            oTdim = iTdim;
            extents[3] = oTdim;
            destResolutions[3] = srcImage.getFileInfo(0).getResolutions()[3];
        }

        String name = JDialogBase.makeImageName(srcImage.getImageName(), "_transform");
        int type = srcImage.getType();

        if (DIM == 3) {

            if (((interp == WSINC) || (interp == CUBIC_LAGRANGIAN) || (interp == QUINTIC_LAGRANGIAN) ||
                     (interp == HEPTIC_LAGRANGIAN)) && (!clip) &&
                    ((type == ModelStorageBase.BYTE) || (type == ModelStorageBase.UBYTE))) {
                type = ModelStorageBase.SHORT;
            } else if (((interp == WSINC) || (interp == CUBIC_LAGRANGIAN) || (interp == QUINTIC_LAGRANGIAN) ||
                            (interp == HEPTIC_LAGRANGIAN)) && (!clip) && (type == ModelStorageBase.USHORT)) {
                type = ModelStorageBase.INTEGER;
            } else if (((interp == WSINC) || (interp == CUBIC_LAGRANGIAN) || (interp == QUINTIC_LAGRANGIAN) ||
                            (interp == HEPTIC_LAGRANGIAN)) && (!clip) && (type == ModelStorageBase.UINTEGER)) {
                type = ModelStorageBase.LONG;
            }
        } // end of if DIM == 3
        else { // DIM == 4

            if (((interp == WSINC) || (interp == CUBIC_LAGRANGIAN) || (interp == QUINTIC_LAGRANGIAN) ||
                     (interp == HEPTIC_LAGRANGIAN)) && (!clip) &&
                    ((type == ModelStorageBase.BYTE) || (type == ModelStorageBase.UBYTE))) {
                type = ModelStorageBase.SHORT;
            } else if (((interp == WSINC) || (interp == CUBIC_LAGRANGIAN) || (interp == QUINTIC_LAGRANGIAN) ||
                            (interp == HEPTIC_LAGRANGIAN)) && (!clip) && (type == ModelStorageBase.USHORT)) {
                type = ModelStorageBase.INTEGER;
            } else if (((interp == WSINC) || (interp == CUBIC_LAGRANGIAN) || (interp == QUINTIC_LAGRANGIAN) ||
                            (interp == HEPTIC_LAGRANGIAN)) && (!clip) && (type == ModelStorageBase.UINTEGER)) {
                type = ModelStorageBase.LONG;
            }
        } // end of DIM == 4

        destImage = new ModelImage(type, extents, name);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Performs bspline interpolation on black and white image data.
     *
     * @param   image        Image from which the data is derived
     * @param   resultImage  Image to put result in; can be null.
     * @param   degree       Degree of the spline algorithm, either 3 or 4.
     * @param   xfrm         Transformation to apply.
     * @param   progressBar  Buffer containing image data.
     *
     * @return  DOCUMENT ME!
     */
    public static ModelImage bspline(ModelImage image, ModelImage resultImage, int degree, TransMatrix xfrm,
                                     ViewJProgressBar progressBar) {

        try {
            AlgorithmBSpline Bspline = new AlgorithmBSpline();
            int i, j, k;
            float X, Y, Z;
            float value;
            float imm, jmm, kmm;
            int xDim = image.getExtents()[0];
            int yDim = image.getExtents()[1];
            int zDim = image.getExtents()[2];
            float[] resols = new float[3];
            int[] inVolExtents = { xDim, yDim, zDim };
            int length = xDim * yDim * zDim;
            int mod = length / 100; // mod is 1 percent of length
            int counter = 0; // used for progress bar
            float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

            resols[0] = image.getFileInfo()[0].getResolutions()[0];
            resols[1] = image.getFileInfo()[0].getResolutions()[1];
            resols[2] = image.getFileInfo()[0].getResolutions()[2];

            int bufferSize = image.getSliceSize() * image.getExtents()[2];
            float[] imgBuffer = new float[bufferSize];

            try {

                if (resultImage != null) {
                    resultImage.exportData(0, bufferSize, imgBuffer);
                } else {
                    image.exportData(0, bufferSize, imgBuffer);
                }
            } catch (IOException error) {
                MipavUtil.displayError("ViewJFrameTriImage: IOException error on exportData");
            }

            TransMatrix kTM = matrixtoInverseArray(xfrm);
            T00 = kTM.M00;
            T01 = kTM.M01;
            T02 = kTM.M02;
            T03 = kTM.M03;
            T10 = kTM.M10;
            T11 = kTM.M11;
            T12 = kTM.M12;
            T13 = kTM.M13;
            T20 = kTM.M20;
            T21 = kTM.M21;
            T22 = kTM.M22;
            T23 = kTM.M23;

            Bspline.setup3DBSpline(imgBuffer, inVolExtents, degree);

            for (i = 0; i < xDim; i++) {

                for (j = 0; j < yDim; j++) {

                    for (k = 0; k < zDim; k++) {

                        if ((progressBar != null) && ((counter % mod) == 0)) {
                            progressBar.updateValueImmed(Math.round((float) counter / (length - 1) * 100));
                        }

                        value = 0; // will remain zero if boundary conditions not met
                        imm = (float) i * resols[0];
                        jmm = (float) j * resols[1];
                        kmm = (float) k * resols[2];

                        // transform i,j,k
                        X = (imm * T00) + (jmm * T01) + (kmm * T02) + T03;
                        X = X / resols[0];

                        if ((X >= 0) && (X < xDim)) { // check bounds
                            Y = (imm * T10) + (jmm * T11) + (kmm * T12) + T13;
                            Y = Y / resols[1];

                            if ((Y >= 0) && (Y < yDim)) {
                                Z = (imm * T20) + (jmm * T21) + (kmm * T22) + T23;
                                Z = Z / resols[2];

                                if ((Z >= 0) && (Z < zDim)) {
                                    value = Bspline.bSpline3D(0, 0, 0, X, Y, Z);
                                }
                            }
                        }

                        if (resultImage != null) {
                            resultImage.set(i, j, k, value);
                        } else {
                            image.set(i, j, k, value);
                        }

                        counter++;
                    }
                }
            }

            if (resultImage != null) {
                resultImage.calcMinMax();

                return resultImage;
            } else {
                image.calcMinMax();

                return image;
            }
        } finally { }
    }

    /**
     * Performs bspline interpolation on black and white image data in 4D image. Works time slice by time slice.
     *
     * @param   image        Image from which the data is derived
     * @param   resultImage  Image to put result in; can be null.
     * @param   degree       Degree of the spline algorithm, either 3 or 4.
     * @param   xfrm         Transformation to apply.
     * @param   progressBar  Buffer containing image data.
     *
     * @return  DOCUMENT ME!
     */
    public static ModelImage bspline4D(ModelImage image, ModelImage resultImage, int degree, TransMatrix xfrm,
                                       ViewJProgressBar progressBar) {
        AlgorithmBSpline Bspline = new AlgorithmBSpline();
        int i, j, k, l;
        float X, Y, Z;
        float value;
        float imm, jmm, kmm;
        int xDim = image.getExtents()[0];
        int yDim = image.getExtents()[1];
        int zDim = image.getExtents()[2];
        float[] resols = new float[3];
        int[] inVolExtents = { xDim, yDim, zDim };
        int length;
        int mod; // mod is 1 percent of length
        int tDim;
        int counter = 0; // used for progress bar
        tDim = image.getExtents()[3];
        length = xDim * yDim * zDim * tDim;
        mod = length / 100;

        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        int bufferSize = image.getSliceSize() * image.getExtents()[2];

        float[] imgBuffer = new float[bufferSize];

        try {

            if (resultImage != null) {
                resultImage.exportData(0, bufferSize, imgBuffer);
            } else {
                image.exportData(0, bufferSize, imgBuffer);
            }
        } catch (IOException error) {
            MipavUtil.displayError("ViewJFrameTriImage: IOException error on exportData");
        }

        resols[0] = image.getFileInfo()[0].getResolutions()[0];
        resols[1] = image.getFileInfo()[0].getResolutions()[1];
        resols[2] = image.getFileInfo()[0].getResolutions()[2];

        TransMatrix kTM = matrixtoInverseArray(xfrm);
        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;

        for (l = 0; l < tDim; l++) {
            Bspline.setup3DBSpline(imgBuffer, inVolExtents, degree);

            for (i = 0; i < xDim; i++) {

                for (j = 0; j < yDim; j++) {

                    for (k = 0; k < zDim; k++) {

                        if ((progressBar != null) && ((counter % mod) == 0)) {
                            progressBar.updateValue(Math.round((float) counter / (length - 1) * 100), false);
                        }

                        value = 0; // will remain zero if boundary conditions not met
                        imm = (float) i * resols[0];
                        jmm = (float) j * resols[1];
                        kmm = (float) k * resols[2];

                        // transform i,j,k
                        X = (imm * T00) + (jmm * T01) + (kmm * T02) + T03;
                        X = X / resols[0];

                        if ((X >= 0) && (X < xDim)) { // check bounds
                            Y = (imm * T10) + (jmm * T11) + (kmm * T12) + T13;
                            Y = Y / resols[1];

                            if ((Y >= 0) && (Y < yDim)) {
                                Z = (imm * T20) + (jmm * T21) + (kmm * T22) + T23;
                                Z = Z / resols[2];

                                if ((Z >= 0) && (Z < zDim)) {
                                    value = Bspline.bSpline3D(0, 0, 0, X, Y, Z);
                                }
                            }
                        }

                        if (resultImage != null) {
                            resultImage.set(i, j, k, l, value);
                        } else {
                            image.set(i, j, k, l, value);
                        }

                        counter++;
                    } // end for k
                } // end for j
            } // end for i

            if (l < (tDim - 1)) {

                try {

                    if (resultImage != null) {
                        resultImage.exportData((l + 1) * imgBuffer.length, imgBuffer.length, imgBuffer);
                    } else {
                        image.exportData((l + 1) * imgBuffer.length, imgBuffer.length, imgBuffer);
                    }
                } catch (IOException error) {
                    MipavUtil.displayError("ViewJFrameTriImage: IOException error on exportData");
                }
            } // if (l < (tDim - 1))
        } // end for l

        if (resultImage != null) {
            resultImage.calcMinMax();

            return resultImage;
        } else {
            image.calcMinMax();

            return image;
        }
    }


    /**
     * Performs bspline interpolation on color image data.
     *
     * @param   image        Image from which the data is derived
     * @param   resultImage  Image to put result in; can be null.
     * @param   degree       Degree of the spline algorithm, either 3 or 4.
     * @param   xfrm         Transformation to apply.
     * @param   progressBar  Buffer containing image data.
     *
     * @return  DOCUMENT ME!
     */
    public static ModelImage bsplineC(ModelImage image, ModelImage resultImage, int degree, TransMatrix xfrm,
                                      ViewJProgressBar progressBar) {

        try {
            AlgorithmBSpline Bspline = new AlgorithmBSpline();
            int i, j, k;
            float X, Y, Z;
            float[] value = new float[4];
            int sliceSize;
            float imm, jmm, kmm;
            int xDim = image.getExtents()[0];
            int yDim = image.getExtents()[1];
            int zDim = image.getExtents()[2];
            float[] resols = new float[3];
            int[] inVolExtents = { xDim, yDim, zDim };
            int length = xDim * yDim * zDim;
            int mod = length / 100; // mod is 1 percent of length
            int counter = 0; // used for progress bar
            sliceSize = xDim * yDim;

            float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
            int index;
            int bufferSize = 4 * image.getSliceSize() * image.getExtents()[2];
            float[] imgBuffer = new float[bufferSize];
            float[] imgBuffer2 = new float[imgBuffer.length];

            try {

                if (resultImage != null) {
                    resultImage.exportData(0, bufferSize, imgBuffer);
                } else {
                    image.exportData(0, bufferSize, imgBuffer);
                }
            } catch (IOException error) {
                MipavUtil.displayError("ViewJFrameTriImage: IOException error on exportData");
            }

            resols[0] = image.getFileInfo()[0].getResolutions()[0];
            resols[1] = image.getFileInfo()[0].getResolutions()[1];
            resols[2] = image.getFileInfo()[0].getResolutions()[2];

            TransMatrix kTM = matrixtoInverseArray(xfrm);
            T00 = kTM.M00;
            T01 = kTM.M01;
            T02 = kTM.M02;
            T03 = kTM.M03;
            T10 = kTM.M10;
            T11 = kTM.M11;
            T12 = kTM.M12;
            T13 = kTM.M13;
            T20 = kTM.M20;
            T21 = kTM.M21;
            T22 = kTM.M22;
            T23 = kTM.M23;

            Bspline.setup3DBSplineC(imgBuffer, inVolExtents, degree);

            for (i = 0; i < xDim; i++) {

                for (j = 0; j < yDim; j++) {

                    for (k = 0; k < zDim; k++) {

                        if ((progressBar != null) && ((counter % mod) == 0)) {
                            progressBar.updateValueImmed(Math.round((float) counter / (length - 1) * 100));
                        }

                        value[0] = value[1] = value[2] = value[3] = 0; // will remain zero if boundary conditions not
                                                                       // met
                        imm = (float) i * resols[0];
                        jmm = (float) j * resols[1];
                        kmm = (float) k * resols[2];

                        // transform i,j,k
                        X = (imm * T00) + (jmm * T01) + (kmm * T02) + T03;
                        X = X / resols[0];

                        if ((X >= 0) && (X < xDim)) { // check bounds
                            Y = (imm * T10) + (jmm * T11) + (kmm * T12) + T13;
                            Y = Y / resols[1];

                            if ((Y >= 0) && (Y < yDim)) {
                                Z = (imm * T20) + (jmm * T21) + (kmm * T22) + T23;
                                Z = Z / resols[2];

                                if ((Z >= 0) && (Z < zDim)) {
                                    value = Bspline.bSpline3DC(0, 0, 0, X, Y, Z);
                                }
                            }
                        }

                        index = 4 * (i + (j * xDim) + (k * sliceSize));
                        imgBuffer2[index] = value[0];
                        imgBuffer2[index + 1] = value[1];
                        imgBuffer2[index + 2] = value[2];
                        imgBuffer2[index + 3] = value[3];

                        counter++;
                    } // end for k
                } // end for j
            } // end for i

            try {

                if (resultImage != null) {
                    resultImage.importData(0, imgBuffer2, true);

                    return resultImage;
                } else {
                    image.importData(0, imgBuffer2, true);

                    return image;
                }
            } catch (IOException error) {
                MipavUtil.displayError("ViewJFrameTriImage: IOException Error on importData");

                return null;
            }
        } finally { }
    }

    /**
     * Performs bspline interpolation on color image data in 4D image. Works time slice by time slice.
     *
     * @param   image        Image from which the data is derived
     * @param   resultImage  Image to put result in; can be null.
     * @param   degree       Degree of the spline algorithm, either 3 or 4.
     * @param   xfrm         Transformation to apply.
     * @param   progressBar  Buffer containing image data.
     *
     * @return  DOCUMENT ME!
     */
    public static ModelImage bsplineC4D(ModelImage image, ModelImage resultImage, int degree, TransMatrix xfrm,
                                        ViewJProgressBar progressBar) {
        AlgorithmBSpline Bspline = new AlgorithmBSpline();
        int i, j, k, l;
        float X, Y, Z;
        float[] value = new float[4];
        int sliceSize;
        float imm, jmm, kmm;
        int xDim = image.getExtents()[0];
        int yDim = image.getExtents()[1];
        int zDim = image.getExtents()[2];
        float[] resols = new float[3];
        int[] inVolExtents = { xDim, yDim, zDim };
        int length;
        int mod; // mod is 1 percent of length
        int tDim;
        int counter = 0; // used for progress bar
        tDim = image.getExtents()[3];
        length = xDim * yDim * zDim * tDim;
        mod = length / 100;
        sliceSize = xDim * yDim;

        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        int index;

        int bufferSize = image.getSliceSize() * image.getExtents()[2];

        float[] imgBuffer = new float[bufferSize];

        try {

            if (resultImage != null) {
                resultImage.exportData(0, bufferSize, imgBuffer);
            } else {
                image.exportData(0, bufferSize, imgBuffer);
            }
        } catch (IOException error) {
            MipavUtil.displayError("ViewJFrameTriImage: IOException error on exportData");
        }

        float[] imgBuffer2 = new float[imgBuffer.length];

        resols[0] = image.getFileInfo()[0].getResolutions()[0];
        resols[1] = image.getFileInfo()[0].getResolutions()[1];
        resols[2] = image.getFileInfo()[0].getResolutions()[2];

        TransMatrix kTM = matrixtoInverseArray(xfrm);
        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;

        for (l = 0; l < tDim; l++) {
            Bspline.setup3DBSplineC(imgBuffer, inVolExtents, degree);

            for (i = 0; i < xDim; i++) {

                for (j = 0; j < yDim; j++) {

                    for (k = 0; k < zDim; k++) {

                        if ((progressBar != null) && ((counter % mod) == 0)) {
                            progressBar.updateValue(Math.round((float) counter / (length - 1) * 100), false);
                        }

                        value[0] = value[1] = value[2] = value[3] = 0; // will remain zero if boundary conditions not
                                                                       // met
                        imm = (float) i * resols[0];
                        jmm = (float) j * resols[1];
                        kmm = (float) k * resols[2];

                        // transform i,j,k
                        X = (imm * T00) + (jmm * T01) + (kmm * T02) + T03;
                        X = X / resols[0];

                        if ((X >= 0) && (X < xDim)) { // check bounds
                            Y = (imm * T10) + (jmm * T11) + (kmm * T12) + T13;
                            Y = Y / resols[1];

                            if ((Y >= 0) && (Y < yDim)) {
                                Z = (imm * T20) + (jmm * T21) + (kmm * T22) + T23;
                                Z = Z / resols[2];

                                if ((Z >= 0) && (Z < zDim)) {
                                    value = Bspline.bSpline3DC(0, 0, 0, X, Y, Z);
                                }
                            }
                        }

                        index = 4 * (i + (j * xDim) + (k * sliceSize));
                        imgBuffer2[index] = value[0];
                        imgBuffer2[index + 1] = value[1];
                        imgBuffer2[index + 2] = value[2];
                        imgBuffer2[index + 3] = value[3];

                        counter++;
                    } // end for k
                } // end for j
            } // end for i

            try {

                if (resultImage != null) {
                    resultImage.importData(l * imgBuffer2.length, imgBuffer2, false);
                } else {
                    image.importData(l * imgBuffer2.length, imgBuffer2, false);
                }
            } catch (IOException error) {
                MipavUtil.displayError("ViewJFrameTriImage: IOException Error on importData");
            }

            if (l < (tDim - 1)) {

                try {

                    if (resultImage != null) {
                        resultImage.exportData((l + 1) * imgBuffer.length, imgBuffer.length, imgBuffer);
                    } else {
                        image.exportData((l + 1) * imgBuffer.length, imgBuffer.length, imgBuffer);
                    }
                } catch (IOException error) {
                    MipavUtil.displayError("ViewJFrameTriImage: IOException error on exportData");
                }
            } // if (l < (tDim - 1))
        } // end for l

        if (resultImage != null) {
            resultImage.calcMinMax();

            return resultImage;
        } else {
            image.calcMinMax();

            return image;
        }
    }

    /**
     * Calculate necessary padding for image given applied transform.
     *
     * @param   srcImage     DOCUMENT ME!
     * @param   transMatrix  array with transformation matrix
     * @param   dxOut        DOCUMENT ME!
     * @param   dyOut        DOCUMENT ME!
     * @param   dzOut        DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public static int[] getImageMargins(ModelImage srcImage, TransMatrix transMatrix, float dxOut, float dyOut,
                                        float dzOut) {
        int i;
        float xi = 0.f;
        float yi = 0.f;
        float zi = 0.f;
        float dx = srcImage.getFileInfo(0).getResolutions()[0];
        float dy = srcImage.getFileInfo(0).getResolutions()[1];
        float dz = srcImage.getFileInfo(0).getResolutions()[2];
        int nx = srcImage.getExtents()[0];
        int ny = srcImage.getExtents()[1];
        int nz = srcImage.getExtents()[2];
        int[] destExtents = new int[3];
        float xf = 0.f, yf = 0.f, zf = 0.f;
        float minx, miny, maxx, maxy, minz, maxz;
        int leftPad = 0, rightPad = 0, topPad = 0, bottomPad = 0, front = 0, back = 0;
        Vector3f[] ptsi3, ptsf3;

        /* Set the far corner of the image volume in mm (but relative to image origin, in image coordinates). */
        xf = xi + (dx * (nx - 1));
        minx = xi;
        maxx = xf;
        yf = yi + (dy * (ny - 1));
        miny = yi;
        maxy = yf;
        zf = zi + (dz * (nz - 1));
        minz = zi;
        maxz = zf;
        // System.out.println("Far corner: " +(int)xf +", " +(int)yf +", " +(int)zf);

        /* Set up array of 8 points representing the corners of the image volume and then transform them with
         * transMatrix. */
        ptsi3 = new Vector3f[8];
        ptsf3 = new Vector3f[8];

        for (i = 1; i <= 8; i++) {
            ptsi3[i - 1] = new Vector3f();
            ptsf3[i - 1] = new Vector3f();

            if ((i == 1) || (i == 4) || (i == 5) || (i == 8)) {
                ptsi3[i - 1].X = xi;
            } else {
                ptsi3[i - 1].X = xf;
            }

            if ((i == 1) || (i == 2) || (i == 5) || (i == 6)) {
                ptsi3[i - 1].Y = yi;
            } else {
                ptsi3[i - 1].Y = yf;
            }

            if ((i == 1) || (i == 2) || (i == 3) || (i == 4)) {
                ptsi3[i - 1].Z = zi;
            } else {
                ptsi3[i - 1].Z = zf;
            }
            //System.out.println("Initial point " +i +": " +(int)ptsi3[i-1].X +", " +(int)ptsi3[i-1].Y +", "
             //+(int)ptsi3[i-1].Z);
        }

        /* Transform corner points, ptsi3, to get transformed points, ptsf3. */
        for (i = 1; i <= 8; i ++) {
            transMatrix.transformAsPoint3Df(ptsi3[i-1], ptsf3[i-1]);
        }

        /* Find new min and max values for the transformed point. */
        for (i = 1; i <= 8; i++) {

            //System.out.println("Transformed point " +i +": " +(int)ptsf3[i-1].X +", " +(int)ptsf3[i-1].Y +", "
            //+(int)ptsf3[i-1].Z);
            if (ptsf3[i - 1].X < minx) {
                minx = ptsf3[i - 1].X;
            }

            if (ptsf3[i - 1].X > maxx) {
                maxx = ptsf3[i - 1].X;
            }

            if (ptsf3[i - 1].Y < miny) {
                miny = ptsf3[i - 1].Y;
            }

            if (ptsf3[i - 1].Y > maxy) {
                maxy = ptsf3[i - 1].Y;
            }

            if (ptsf3[i - 1].Z < minz) {
                minz = ptsf3[i - 1].Z;
            }

            if (ptsf3[i - 1].Z > maxz) {
                maxz = ptsf3[i - 1].Z;
            }
        }
        // System.out.println("Bounding box first corner: " +(int)minx +", " +(int)miny +", " +(int)minz);
        // System.out.println("Bounding box far corner: " +(int)maxx +", " +(int)maxy +", " +(int)maxz);

        /* Calculate padding. */
        leftPad = (int) (((xi - minx) / dxOut) + 0.5);
        rightPad = (int) (((maxx - xf) / dxOut) + 0.5);

        // System.out.println("Padding in x is: " + leftPad +" and " +rightPad);
        topPad = (int) (((yi - miny) / dyOut) + 0.5);
        bottomPad = (int) (((maxy - yf) / dyOut) + 0.5);

        // System.out.println("Padding in y is: " + topPad+" and " +bottomPad);
        front = (int) (((zi - minz) / dzOut) + 0.5);
        back = (int) (((maxz - zf) / dzOut) + 0.5);
        // System.out.println("Padding in z is: " + front + " and " + back);

        margins[0] = leftPad;
        margins[1] = topPad;
        margins[2] = front;
        margins[3] = rightPad;
        margins[4] = bottomPad;
        margins[5] = back;

        return margins;
    }

    /**
     * Converts matrix to inverse array.
     *
     * @param   transMatrix  Matrix to convert.
     *
     * @return  The inverted array.
     */
    public static final TransMatrix matrixtoInverseArray(TransMatrix transMatrix) {

        if (transMatrix.isIdentity()) {

            // Added explicit handling of identity matrix - or else the new matrix is other than
            // identity because of round-off error.  This situation (of identity) matrix
            // occurs frequently -- any time there is resampling without transformation.
            return new TransMatrix(transMatrix);
        } else {
        	TransMatrix kTM = new TransMatrix(transMatrix);
        	kTM.Inverse();
            return kTM;
        }

    }

    /**
     * Transforms using bilinear interpolation.
     *
     * @param  image           Image to be transformed
     * @param  transformedImg  Transformed image
     * @param  trans           Transformation matrix to be applied
     * @param  progressBar     Progress bar to update. Can be null. Should NOT have cancel button.
     */
    public static final void transformBilinear(ModelImage image, ModelImage transformedImg, TransMatrix trans,
                                               ViewJProgressBar progressBar) {
        transformBilinear(image, transformedImg, trans, progressBar, true);
    }

    /**
     * Transforms using bilinear interpolation.
     *
     * @param  image           Image to be transformed
     * @param  transformedImg  Transformed image
     * @param  trans           Transformation matrix to be applied
     * @param  progressBar     Progress bar to update. Can be null. Should NOT have cancel button.
     * @param  activeImage     true if the algorithm is being run in a separate thread, false otherwise, to control
     *                         progress bar repainting
     */
    public static final void transformBilinear(ModelImage image, ModelImage transformedImg, TransMatrix trans,
                                               ViewJProgressBar progressBar, boolean activeImage) {
        int i, j;
        float X, Y;
        int x0, y0;
        float j1, j2;
        float imm, jmm;
        float value;

        imgOrigin = (float[]) image.getFileInfo(0).getOrigin().clone();

        if (updateOrigin) {
            updateOrigin(trans);
        }

        

        int iXdim, iYdim;
        float iXres, iYres;
        int oXdim, oYdim;
        float oXres, oYres;

        iXdim = image.getExtents()[0];
        iYdim = image.getExtents()[1];
        iXres = image.getFileInfo(0).getResolutions()[0];
        iYres = image.getFileInfo(0).getResolutions()[1];

        oXdim = transformedImg.getExtents()[0];
        oYdim = transformedImg.getExtents()[1];
        oXres = transformedImg.getFileInfo(0).getResolutions()[0];
        oYres = transformedImg.getFileInfo(0).getResolutions()[1];

        updateFileInfo(image, transformedImg, transformedImg.getFileInfo(0).getResolutions(),
                       image.getFileInfo()[0].getUnitsOfMeasure(), trans, false);

        int mod = Math.max(1, oYdim / 50);
        int imgLength = iXdim * iYdim;
        float[] imgBuf;

        try {
            imgBuf = new float[imgLength];
            image.exportData(0, imgLength, imgBuf);
        } catch (IOException error) {
            imgBuf = null;
            MipavUtil.displayError("Algorithm Transform: Image(s) locked");

            return;
        } catch (OutOfMemoryError error) {
            imgBuf = null;
            System.gc();
            MipavUtil.displayError("Algorithm Transform: Out of memory");

            return;
        }

        float T00, T01, T02, T10, T11, T12;

        TransMatrix kTM = matrixtoInverseArray(trans);
        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        int position;
        float dx, dy, dx1, dy1;

        float invXRes = 1 / iXres;
        float invYRes = 1 / iYres;

        int index = 0;
        int deltaX, deltaY;

        for (j = 0; j < oYdim; j++) {

            if ((progressBar != null) && ((j % mod) == 0)) {
                progressBar.updateValue((int) (((float) j / oYdim * 100) + 0.5f), activeImage);
            }

            jmm = j * oYres;
            j1 = (jmm * T01) + T02;
            j2 = (jmm * T11) + T12;

            for (i = 0; i < oXdim; i++) {

                // transform i,j,k
                value = (float) image.getMin(); // remains zero if voxel is transformed out of bounds
                imm = i * oXres;
                X = (j1 + (imm * T00)) * invXRes;

                if ((X > -0.5f) && (X < iXdim)) {
                    Y = (j2 + (imm * T10)) * invYRes;

                    if ((Y > -0.5f) && (Y < iYdim)) {

                        if (X <= 0) {
                            x0 = 0;
                            dx = 0;
                            deltaX = 0;
                        } else if (X >= (iXdim - 1)) {
                            x0 = iXdim - 1;
                            dx = 0;
                            deltaX = 0;
                        } else {
                            x0 = (int) X;
                            dx = X - x0;
                            deltaX = 1;
                        }

                        if (Y <= 0) {
                            y0 = 0;
                            dy = 0;
                            deltaY = 0;
                        } else if (Y >= (iYdim - 1)) {
                            y0 = iYdim - 1;
                            dy = 0;
                            deltaY = 0;
                        } else {
                            y0 = (int) Y;
                            dy = Y - y0;
                            deltaY = iXdim;
                        }

                        dx1 = 1 - dx;
                        dy1 = 1 - dy;

                        position = (y0 * iXdim) + x0;

                        value = (dy1 * ((dx1 * imgBuf[position]) + (dx * imgBuf[position + deltaX]))) +
                                (dy * ((dx1 * imgBuf[position + deltaY]) + (dx * imgBuf[position + deltaY + deltaX])));
                    } // end if Y in bounds
                } // end if X in bounds

                transformedImg.set(index++, value);
            } // end for i
        } // end for j
    }

    /**
     * Transforms using bilinear interpolation.
     *
     * @param  imgBuf          Image buffer to be transformed
     * @param  transformedImg  Transformed image
     * @param  trans           Transformation matrix to be applied
     * @param  iXdim           X dimension of input image
     * @param  iYdim           Y dimension of input image
     * @param  iXres           X resolution of input image
     * @param  iYres           Y resolution of input image
     * @param  progressBar     Progress bar to update. Can be null. Should NOT have cancel button.
     */
    public static final void transformBilinear(float[] imgBuf, ModelImage transformedImg, TransMatrix trans, int iXdim,
                                               int iYdim, float iXres, float iYres, ViewJProgressBar progressBar) {
        transformBilinear(imgBuf, transformedImg, trans, iXdim, iYdim, iXres, iYres, progressBar, true);
    }

    /**
     * Transforms using bilinear interpolation.
     *
     * @param  imgBuf          Image buffer to be transformed
     * @param  transformedImg  Transformed image
     * @param  trans           Transformation matrix to be applied
     * @param  iXdim           X dimension of input image
     * @param  iYdim           Y dimension of input image
     * @param  iXres           X resolution of input image
     * @param  iYres           Y resolution of input image
     * @param  progressBar     Progress bar to update. Can be null. Should NOT have cancel button.
     * @param  activeImage     true if the algorithm is being run in a separate thread, false otherwise, to control
     *                         progress bar repainting
     */
    public static final void transformBilinear(float[] imgBuf, ModelImage transformedImg, TransMatrix trans, int iXdim,
                                               int iYdim, float iXres, float iYres, ViewJProgressBar progressBar,
                                               boolean activeImage) {
        int i, j;
        float X, Y;
        int x0, y0;
        float value;
        float imm, jmm;
        int oXdim, oYdim;
        float oXres, oYres;
        float j1, j2;
        int deltaX, deltaY;

        oXdim = transformedImg.getExtents()[0];
        oYdim = transformedImg.getExtents()[1];
        oXres = transformedImg.getFileInfo(0).getResolutions()[0];
        oYres = transformedImg.getFileInfo(0).getResolutions()[1];

        imgOrigin = (float[]) transformedImg.getFileInfo(0).getOrigin().clone();

        if (updateOrigin) {
            updateOrigin(trans);
        }

        int mod = Math.max(1, oYdim / 50);


        float T00, T01, T02, T10, T11, T12;

        TransMatrix kTM = matrixtoInverseArray(trans);
        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        int position;
        float dx, dy, dx1, dy1;

        float invXRes = 1 / iXres;
        float invYRes = 1 / iYres;

        float min = Float.MAX_VALUE;

        for (int run = 0; run < imgBuf.length; run++) {

            if (min > imgBuf[run]) {
                min = imgBuf[run];
            }
        }

        int index = 0;

        for (j = 0; j < oYdim; j++) {

            if ((progressBar != null) && ((j % mod) == 0)) {
                progressBar.updateValue((int) (((float) j / oYdim * 100) + 0.5f), activeImage);
            }

            jmm = j * oYres;
            j1 = (jmm * T01) + T02;
            j2 = (jmm * T11) + T12;

            for (i = 0; i < oXdim; i++) {

                // transform i,j,k
                value = min; // remains zero if voxel is transformed out of bounds
                imm = i * oXres;
                X = (j1 + (imm * T00)) * invXRes;

                if ((X > -0.5f) && (X < iXdim)) {
                    Y = (j2 + (imm * T10)) * invYRes;

                    if ((Y > -0.5f) && (Y < iYdim)) {

                        if (X <= 0) {
                            x0 = 0;
                            dx = 0;
                            deltaX = 0;
                        } else if (X >= (iXdim - 1)) {
                            x0 = iXdim - 1;
                            dx = 0;
                            deltaX = 0;
                        } else {
                            x0 = (int) X;
                            dx = X - x0;
                            deltaX = 1;
                        }

                        if (Y <= 0) {
                            y0 = 0;
                            dy = 0;
                            deltaY = 0;
                        } else if (Y >= (iYdim - 1)) {
                            y0 = iYdim - 1;
                            dy = 0;
                            deltaY = 0;
                        } else {
                            y0 = (int) Y;
                            dy = Y - y0;
                            deltaY = iXdim;
                        }

                        dx1 = 1 - dx;
                        dy1 = 1 - dy;

                        position = (y0 * iXdim) + x0;

                        value = (dy1 * ((dx1 * imgBuf[position]) + (dx * imgBuf[position + deltaX]))) +
                                (dy * ((dx1 * imgBuf[position + deltaY]) + (dx * imgBuf[position + deltaY + deltaX])));
                    } // end if Y in bounds
                } // end if X in bounds

                transformedImg.set(index++, value);
            } // end for i
        } // end for j
    }

    /**
     * Transforms using bilinear interpolation.
     *
     * @param  imgBuf       Image buffer to be transformed
     * @param  tImgBuf      Transformed image
     * @param  trans        Transformation matrix to be applied
     * @param  iXdim        X dimension of input image
     * @param  iYdim        Y dimension of input image
     * @param  iXres        X resolution of input image
     * @param  iYres        Y resolution of input image
     * @param  oXdim        X dimension of output image
     * @param  oYdim        Y dimension of output image
     * @param  oXres        X resolution of output image
     * @param  oYres        Y resolution of output image
     * @param  progressBar  Progress bar to be updated. Can be null. Should NOT have cancel button.
     */
    public static final void transformBilinear(float[] imgBuf, float[] tImgBuf, TransMatrix trans, int iXdim, int iYdim,
                                               float iXres, float iYres, int oXdim, int oYdim, float oXres, float oYres,
                                               ViewJProgressBar progressBar) {
        transformBilinear(imgBuf, tImgBuf, trans, iXdim, iYdim, iXres, iYres, oXdim, oYdim, oXres, oYres, progressBar,
                          true);
    }

    /**
     * Transforms using bilinear interpolation.
     *
     * @param  imgBuf       Image buffer to be transformed
     * @param  tImgBuf      Transformed image
     * @param  trans        Transformation matrix to be applied
     * @param  iXdim        X dimension of input image
     * @param  iYdim        Y dimension of input image
     * @param  iXres        X resolution of input image
     * @param  iYres        Y resolution of input image
     * @param  oXdim        X dimension of output image
     * @param  oYdim        Y dimension of output image
     * @param  oXres        X resolution of output image
     * @param  oYres        Y resolution of output image
     * @param  progressBar  Progress bar to be updated. Can be null. Should NOT have cancel button.
     * @param  activeImage  true if the algorithm is being run in a separate thread, false otherwise, to control
     *                      progress bar repainting
     */
    public static final void transformBilinear(float[] imgBuf, float[] tImgBuf, TransMatrix trans, int iXdim, int iYdim,
                                               float iXres, float iYres, int oXdim, int oYdim, float oXres, float oYres,
                                               ViewJProgressBar progressBar, boolean activeImage) {
        int i, j;
        float X, Y;
        int x0, y0;
        float j1, j2;
        float value;
        float imm, jmm;

        float T00, T01, T02, T10, T11, T12;
        int mod = Math.max(1, oYdim / 50);
        int deltaX, deltaY;

        TransMatrix kTM = matrixtoInverseArray(trans);
        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        int position;
        float dx, dy, dx1, dy1;

        float invXRes = 1 / iXres;
        float invYRes = 1 / iYres;

        float min = Float.MAX_VALUE;

        for (int run = 0; run < imgBuf.length; run++) {

            if (min > imgBuf[run]) {
                min = imgBuf[run];
            }
        }

        int index = 0;

        for (j = 0; j < oYdim; j++) {

            if ((progressBar != null) && ((j % mod) == 0)) {
                progressBar.updateValue((int) (((float) j / oYdim * 100) + 0.5f), activeImage);
            }

            jmm = j * oYres;
            j1 = (jmm * T01) + T02;
            j2 = (jmm * T11) + T12;

            for (i = 0; i < oXdim; i++) {

                // transform i,j,k
                value = min; // remains zero if voxel is transformed out of bounds
                imm = i * oXres;
                X = (j1 + (imm * T00)) * invXRes;

                if ((X > -0.5f) && (X < iXdim)) {
                    Y = (j2 + (imm * T10)) * invYRes;

                    if ((Y > -0.5f) && (Y < iYdim)) {

                        if (X <= 0) {
                            x0 = 0;
                            dx = 0;
                            deltaX = 0;
                        } else if (X >= (iXdim - 1)) {
                            x0 = iXdim - 1;
                            dx = 0;
                            deltaX = 0;
                        } else {
                            x0 = (int) X;
                            dx = X - x0;
                            deltaX = 1;
                        }

                        if (Y <= 0) {
                            y0 = 0;
                            dy = 0;
                            deltaY = 0;
                        } else if (Y >= (iYdim - 1)) {
                            y0 = iYdim - 1;
                            dy = 0;
                            deltaY = 0;
                        } else {
                            y0 = (int) Y;
                            dy = Y - y0;
                            deltaY = iXdim;
                        }

                        dx1 = 1 - dx;
                        dy1 = 1 - dy;

                        position = (y0 * iXdim) + x0;

                        value = (dy1 * ((dx1 * imgBuf[position]) + (dx * imgBuf[position + deltaX]))) +
                                (dy * ((dx1 * imgBuf[position + deltaY]) + (dx * imgBuf[position + deltaY + deltaX])));

                    } // end if Y in bounds
                } // end if X in bounds

                tImgBuf[index++] = value;
            } // end for i
        } // end for j
    }

    /**
     * Used on color images. USE THIS IF OUTPUT IMAGE HAS DIFFERENT DIM/RES THAN INPUT IMAGE
     *
     * @param  image           Input image to be transformed
     * @param  transformedImg  Transformed image
     * @param  trans           Transformation matrix to be applied
     * @param  oXdim           Dimensions of output image
     * @param  oYdim           Dimensions of output image
     * @param  oXres           Resolutions of output image
     * @param  oYres           Resolutions of output image
     */
    public static final void transformBilinearC(ModelImage image, ModelImage transformedImg, TransMatrix trans,
                                                int oXdim, int oYdim, float oXres, float oYres) {
        int i, j;
        int X0pos, Y0pos;
        int X1pos, Y1pos;
        float X, Y;
        float x0, y0;
        float x1, y1;
        float imm, jmm;

        imgOrigin = (float[]) image.getFileInfo(0).getOrigin().clone();

        if (updateOrigin) {
            updateOrigin(trans);
        }

        int iXdim, iYdim;
        float iXres, iYres;

        iXdim = image.getExtents()[0];
        iYdim = image.getExtents()[1];
        iXres = image.getFileInfo(0).getResolutions()[0];
        iYres = image.getFileInfo(0).getResolutions()[1];

        int imgLength = 4 * iXdim * iYdim;
        float[] imgBuf = new float[imgLength];
        float[] imgBuf2 = new float[imgLength];
        int index, indexDest;
        int index00, index01, index10, index11;

        try {
            image.exportData(0, imgLength, imgBuf);
        } catch (IOException error) {
            MipavUtil.displayError("Algorithm Transform: Image(s) locked");

            return;
        }

        float[] resolutions = new float[] { oXres, oYres };

        updateFileInfo(image, transformedImg, resolutions, image.getFileInfo()[0].getUnitsOfMeasure(), trans, false);

        int roundX, roundY;
        float temp1, temp2;
        float T00, T01, T02, T10, T11, T12;

        TransMatrix kTM = matrixtoInverseArray(trans);
        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        for (i = 0; i < oXdim; i++) {
            imm = (float) i * oXres;
            temp1 = (imm * T00) + T02;
            temp2 = (imm * T10) + T12;

            for (j = 0; j < oYdim; j++) {

                // transform i,j
                indexDest = 4 * (i + (j * oXdim));
                imgBuf2[indexDest] = 255;
                imgBuf2[indexDest + 1] = 0; // R,G, and B remain zero if pixel is transformed out of bounds
                imgBuf2[indexDest + 2] = 0;
                imgBuf2[indexDest + 3] = 0;
                jmm = (float) j * oYres;
                X = (temp1 + (jmm * T01)) / iXres;

                if ((X >= 0) && (X < iXdim)) {
                    Y = (temp2 + (jmm * T11)) / iYres;

                    if ((Y >= 0) && (Y < iYdim)) {

                        if ((X >= (iXdim - 1)) || (Y >= (iYdim - 1))) { // cannot interpolate on last X or Y
                            X0pos = Math.min((int)(X + 0.5f),(iXdim - 1));
                            Y0pos = Math.min((int)(Y + 0.5f),(iYdim - 1)) * iXdim;
                            index = 4 * (Y0pos + X0pos);

                            // imgBuf2[indexDest] = imgBuf[index];
                            imgBuf2[indexDest + 1] = imgBuf[index + 1];
                            imgBuf2[indexDest + 2] = imgBuf[index + 2];
                            imgBuf2[indexDest + 3] = imgBuf[index + 3];
                        } else {

                            // set intensity of i,j,k to new transformed coordinate if
                            // x,y,z is w/in dimensions of image
                            x0 = X - (int) X;
                            y0 = Y - (int) Y;
                            x1 = 1 - x0;
                            y1 = 1 - y0;
                            X0pos = (int) X;
                            Y0pos = (int) Y * iXdim;
                            X1pos = X0pos + 1;
                            Y1pos = Y0pos + iXdim;
                            index00 = 4 * (Y0pos + X0pos);
                            index01 = 4 * (Y0pos + X1pos);
                            index10 = 4 * (Y1pos + X0pos);
                            index11 = 4 * (Y1pos + X1pos);

                            // imgBuf2[indexDest] = x1*y1*imgBuf[index00] +
                            // x0*y1*imgBuf[index01] +
                            // x1*y0*imgBuf[index10] +
                            // x0*y0*imgBuf[index11];
                            imgBuf2[indexDest + 1] = (x1 * y1 * imgBuf[index00 + 1]) + (x0 * y1 * imgBuf[index01 + 1]) +
                                                     (x1 * y0 * imgBuf[index10 + 1]) + (x0 * y0 * imgBuf[index11 + 1]);
                            imgBuf2[indexDest + 2] = (x1 * y1 * imgBuf[index00 + 2]) + (x0 * y1 * imgBuf[index01 + 2]) +
                                                     (x1 * y0 * imgBuf[index10 + 2]) + (x0 * y0 * imgBuf[index11 + 2]);
                            imgBuf2[indexDest + 3] = (x1 * y1 * imgBuf[index00 + 3]) + (x0 * y1 * imgBuf[index01 + 3]) +
                                                     (x1 * y0 * imgBuf[index10 + 3]) + (x0 * y0 * imgBuf[index11 + 3]);
                        } // end else
                    } // end if y in bounds
                } // end if x in bounds
            } // end for j
        } // end for i

        try {
            transformedImg.importData(0, imgBuf2, true);
        } catch (IOException error) {
            MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
        }

        return;
    }

    /**
     * Transforms using Nearest neighbor interpolation.
     *
     * @param  imgBuf   Image buffer to be transformed
     * @param  tImgBuf  Transformed image buffer
     * @param  trans    Transformation matrix to be applied
     * @param  xdim     X dimension of input AND output image
     * @param  ydim     Y dimension of input AND output image
     */
    public static final void transformNearestNeighbor2D(float[] imgBuf, float[] tImgBuf, TransMatrix trans, int xdim,
                                                        int ydim) {
        int i, j;
        float X, Y;
        int xOffset, yOffset;
        float value;

        if (updateOrigin) {
            updateOrigin(trans);
        }

        float T00, T01, T02, T10, T11, T12;
        float i1, i2;
        int roundX, roundY;

        TransMatrix kTM = matrixtoInverseArray(trans);
        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        float min = Float.MAX_VALUE;

        for (int run = 0; run < imgBuf.length; run++) {

            if (min > imgBuf[run]) {
                min = imgBuf[run];
            }
        }

        for (i = 0; i < xdim; i++) {
            i1 = ((float) i * T00) + T02;
            i2 = ((float) i * T10) + T12;

            for (j = 0; j < ydim; j++) {

                // transform i,j
                X = i1 + ((float) j * T01);
                Y = i2 + ((float) j * T11);

                // set intensity of i,j,k to new transformed coordinate if
                // x,y,z is w/in dimensions of image
                roundX = (int) (X + 0.5f);
                roundY = (int) (Y + 0.5f);

                if ((X < -0.5) || (X >= xdim) || (Y < -0.5) || (Y >= ydim)) {
                    value = min;
                } else {
                    xOffset = Math.min(roundX, xdim - 1);
                    yOffset = Math.min(roundY, ydim - 1) * xdim;
                    value = imgBuf[xOffset + yOffset];
                }

                tImgBuf[i + (j * xdim)] = value;
            }
        }

        imgBuf = tImgBuf;

        return;

    }

    /**
     * Transforms using Nearest neighbor interpolation.
     *
     * @param  imgBuf   Image buffer to be transformed
     * @param  tImgBuf  Transformed image buffer
     * @param  trans    Transformation matrix to be applied
     * @param  iXdim    X dimension of input image
     * @param  iYdim    Y dimension of input image
     * @param  iXres    X resolution of input image
     * @param  iYres    Y resolution of input image
     * @param  oXdim    X dimension of output image
     * @param  oYdim    Y dimension of output image
     * @param  oXres    X resolution of output image
     * @param  oYres    Y resolution of output image
     */
    public static final void transformNearestNeighbor2D(float[] imgBuf, float[] tImgBuf, TransMatrix trans, int iXdim,
                                                        int iYdim, float iXres, float iYres, int oXdim, int oYdim,
                                                        float oXres, float oYres) {
        int i, j;
        float X, Y;
        int xOffset, yOffset;
        float value;
        float imm, jmm;

        int roundX, roundY;

        float T00, T01, T02, T10, T11, T12;
        float i1, i2;

        TransMatrix kTM = matrixtoInverseArray(trans);
        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        float min = Float.MAX_VALUE;

        for (int run = 0; run < imgBuf.length; run++) {

            if (min > imgBuf[run]) {
                min = imgBuf[run];
            }
        }

        for (i = 0; i < oXdim; i++) {
            imm = (float) i * oXres;
            i1 = (imm * T00) + T02;
            i2 = (imm * T10) + T12;

            for (j = 0; j < oYdim; j++) {

                // transform i,j
                jmm = (float) j * oYres;
                X = i1 + (jmm * T01);
                Y = i2 + (jmm * T11);

                // set intensity of i,j,k to new transformed coordinate if
                // x,y,z is w/in dimensions of image
                X = X / iXres;
                Y = Y / iYres;
                roundX = (int) (X + 0.5f);
                roundY = (int) (Y + 0.5f);

                if ((X < -0.5) || (X >= iXdim) || (Y < -0.5) || (Y >= iYdim)) {
                    value = min;
                } else {
                    xOffset = Math.min(roundX, iXdim - 1);
                    yOffset = Math.min(roundY, iYdim - 1) * iXdim;
                    value = imgBuf[xOffset + yOffset];
                }

                tImgBuf[i + (j * oXdim)] = value;
            }
        }
    }


    /**
     * Transforms and resamples volume using nearest neighbor interpolation.
     *
     * @param  imgBuf          Image array to transform
     * @param  transformedImg  Transformed image.
     * @param  Xdim            X dimension of input image
     * @param  Ydim            Y dimension of input image
     * @param  Zdim            Z dimension of input image
     * @param  trans           Transformation matrix to be applied
     */
    public static final void transformNearestNeighbor3D(float[] imgBuf, ModelImage transformedImg, int Xdim, int Ydim,
                                                        int Zdim, TransMatrix trans) {
        int i, j, k;
        float X, Y, Z;
        int xOffset, yOffset, zOffset;
        float value;
        int sliceSize;

        sliceSize = Xdim * Ydim;

        int roundX, roundY, roundZ;
        float i1, i2, i3, j1, j2, j3;

        imgOrigin = (float[]) transformedImg.getFileInfo(0).getOrigin().clone();

        if (updateOrigin) {
            updateOrigin(trans);
        }

        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        TransMatrix kTM = matrixtoInverseArray(trans);
        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;

        float min = Float.MAX_VALUE;

        for (int run = 0; run < imgBuf.length; run++) {

            if (min > imgBuf[run]) {
                min = imgBuf[run];
            }
        }

        for (i = 0; i < Xdim; i++) {
            i1 = ((float) i * T00) + T03;
            i2 = ((float) i * T10) + T13;
            i3 = ((float) i * T20) + T23;

            for (j = 0; j < Ydim; j++) {
                j1 = (float) j * T01;
                j2 = (float) j * T11;
                j3 = (float) j * T21;

                for (k = 0; k < Zdim; k++) {
                    X = i1 + j1 + ((float) k * T02);
                    Y = i2 + j2 + ((float) k * T12);
                    Z = i3 + j3 + ((float) k * T22);
                    roundX = (int) (X + 0.5f);
                    roundY = (int) (Y + 0.5f);
                    roundZ = (int) (Z + 0.5f);

                    if ((X < -0.5) || (X >= Xdim) || (Y < -0.5) || (Y >= Ydim) ||
                            (Z < -0.5) || (Z >= Zdim)) {
                        value = min;
                    } else {
                        xOffset = Math.min(roundX, Xdim - 1);
                        yOffset = Math.min(roundY, Ydim - 1) * Xdim;
                        zOffset = Math.min(roundZ, Zdim - 1) * sliceSize;
                        value = imgBuf[xOffset + yOffset + zOffset];

                    }

                    transformedImg.set(i, j, k, value);
                }
            }
        }
    }

    /**
     * Transforms and resamples volume using trilinear interpolation.
     *
     * @param  image           Image to transform
     * @param  transformedImg  Transformed image.
     * @param  trans           Transformation matrix to be applied
     * @param  progressBar     The progress bar. Can be null. Should NOT have a cancel button.
     */
    public static final void transformTrilinear(ModelImage image, ModelImage transformedImg, TransMatrix trans,
                                                ViewJProgressBar progressBar) {
        transformTrilinear(image, transformedImg, trans, progressBar, true);
    }

    /**
     * Transforms and resamples volume using trilinear interpolation.
     *
     * @param  image           Image to transform
     * @param  transformedImg  Transformed image.
     * @param  trans           Transformation matrix to be applied
     * @param  progressBar     The progress bar. Can be null. Should NOT have a cancel button.
     * @param  activeImage     true if the algorithm is being run in a separate thread, false otherwise, to control
     *                         progress bar repainting
     */
    public static final void transformTrilinear(ModelImage image, ModelImage transformedImg, TransMatrix trans,
                                                ViewJProgressBar progressBar, boolean activeImage) {
        int i, j, k;
        float X, Y, Z;
        int x0, y0, z0;
        float value;
        int sliceSize;
        float imm, jmm, kmm;
        float k1, k2, k3, j1, j2, j3;
        int position1, position2;
        float b1, b2;
        float dx, dy, dz, dx1, dy1;
        int iXdim, iYdim, iZdim;
        float iXres, iYres, iZres;
        int oXdim, oYdim, oZdim;
        float oXres, oYres, oZres;
        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        int mod;
        int deltaX, deltaY, deltaZ;

        iXdim = image.getExtents()[0];
        iYdim = image.getExtents()[1];
        iZdim = image.getExtents()[2];
        iXres = image.getFileInfo(0).getResolutions()[0];
        iYres = image.getFileInfo(0).getResolutions()[1];
        iZres = image.getFileInfo(0).getResolutions()[2];

        oXdim = transformedImg.getExtents()[0];
        oYdim = transformedImg.getExtents()[1];
        oZdim = transformedImg.getExtents()[2];
        oXres = transformedImg.getFileInfo(0).getResolutions()[0];
        oYres = transformedImg.getFileInfo(0).getResolutions()[1];
        oZres = transformedImg.getFileInfo(0).getResolutions()[2];

        sliceSize = iXdim * iYdim;

        imgOrigin = (float[]) image.getFileInfo(0).getOrigin().clone();

        if (updateOrigin) {
            updateOrigin(trans);
        }

        TransMatrix kTM = matrixtoInverseArray(trans);
        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;

        int imgLength = iXdim * iYdim * iZdim;
        float[] imgBuffer = new float[imgLength];

        float min = Float.MAX_VALUE;

        for (int run = 0; run < imgBuffer.length; run++) {

            if (min > imgBuffer[run]) {
                min = imgBuffer[run];
            }
        }

        try {
            image.exportData(0, imgLength, imgBuffer);
        } catch (IOException error) {
            MipavUtil.displayError("Algorithm Transform: Image(s) locked");

            return;
        }

        mod = Math.max(1, oZdim / 50);

        float invXRes = 1 / iXres;
        float invYRes = 1 / iYres;
        float invZRes = 1 / iZres;

        int index = 0;

        for (k = 0; k < oZdim; k++) {

            if ((progressBar != null) && ((k % mod) == 0)) {
                progressBar.updateValue((int) (((float) k / oZdim * 100) + 0.5f), activeImage);
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
                    value = min; // remains zero if voxel is transformed out of bounds
                    imm = i * oXres;
                    X = (j1 + (imm * T00)) * invXRes;

                    if ((X > -0.5f) && (X < iXdim)) {
                        Y = (j2 + (imm * T10)) * invYRes;

                        if ((Y > -0.5f) && (Y < iYdim)) {
                            Z = (j3 + (imm * T20)) * invZRes;

                            if ((Z > -0.5f) && (Z < iZdim)) {

                                if (X <= 0) {
                                    x0 = 0;
                                    dx = 0;
                                    deltaX = 0;
                                } else if (X >= (iXdim - 1)) {
                                    x0 = iXdim - 1;
                                    dx = 0;
                                    deltaX = 0;
                                } else {
                                    x0 = (int) X;
                                    dx = X - x0;
                                    deltaX = 1;
                                }

                                if (Y <= 0) {
                                    y0 = 0;
                                    dy = 0;
                                    deltaY = 0;
                                } else if (Y >= (iYdim - 1)) {
                                    y0 = iYdim - 1;
                                    dy = 0;
                                    deltaY = 0;
                                } else {
                                    y0 = (int) Y;
                                    dy = Y - y0;
                                    deltaY = iXdim;
                                }

                                if (Z <= 0) {
                                    z0 = 0;
                                    dz = 0;
                                    deltaZ = 0;
                                } else if (Z >= (iZdim - 1)) {
                                    z0 = iZdim - 1;
                                    dz = 0;
                                    deltaZ = 0;
                                } else {
                                    z0 = (int) Z;
                                    dz = Z - z0;
                                    deltaZ = sliceSize;
                                }

                                dx1 = 1 - dx;
                                dy1 = 1 - dy;

                                position1 = (z0 * sliceSize) + (y0 * iXdim) + x0;
                                position2 = position1 + deltaZ;

                                b1 = (dy1 * ((dx1 * imgBuffer[position1]) + (dx * imgBuffer[position1 + deltaX]))) +
                                     (dy *
                                          ((dx1 * imgBuffer[position1 + deltaY]) +
                                               (dx * imgBuffer[position1 + deltaY + deltaX])));

                                b2 = (dy1 * ((dx1 * imgBuffer[position2]) + (dx * imgBuffer[position2 + deltaX]))) +
                                     (dy *
                                          ((dx1 * imgBuffer[position2 + deltaY]) +
                                               (dx * imgBuffer[position2 + deltaY + deltaX])));

                                value = ((1 - dz) * b1) + (dz * b2);

                            } // end if Z in bounds
                        } // end if Y in bounds
                    } // end if X in bounds

                    transformedImg.set(index++, value);
                } // end for i
            } // end for j
        } // end for k

        transformedImg.calcMinMax();
    }

    /**
     * Transforms and resamples volume using trilinear interpolation.
     *
     * @param  imgBuffer       Image array
     * @param  transformedImg  Image after transform
     * @param  trans           Transformation matrix to be applied
     * @param  iXdim           X dimension of input image
     * @param  iYdim           Y dimension of input image
     * @param  iZdim           Z dimension of input image
     * @param  iXres           X resolution of input image
     * @param  iYres           Y resolution of input image
     * @param  iZres           Z resolution of input image
     * @param  progressBar     Progress bar to update. Can be null. Should NOT have a cancel button.
     */
    public static final void transformTrilinear(float[] imgBuffer, ModelImage transformedImg, TransMatrix trans,
                                                int iXdim, int iYdim, int iZdim, float iXres, float iYres, float iZres,
                                                ViewJProgressBar progressBar) {
        transformTrilinear(imgBuffer, transformedImg, trans, iXdim, iYdim, iZdim, iXres, iYres, iZres, progressBar,
                           true);
    }

    /**
     * Transforms and resamples volume using trilinear interpolation.
     *
     * @param  imgBuffer       Image array
     * @param  transformedImg  Image after transform
     * @param  trans           Transformation matrix to be applied
     * @param  iXdim           X dimension of input image
     * @param  iYdim           Y dimension of input image
     * @param  iZdim           Z dimension of input image
     * @param  iXres           X resolution of input image
     * @param  iYres           Y resolution of input image
     * @param  iZres           Z resolution of input image
     * @param  progressBar     Progress bar to update. Can be null. Should NOT have a cancel button.
     * @param  activeImage     true if the algorithm is being run in a separate thread, false otherwise, to control
     *                         progress bar repainting
     */
    public static final void transformTrilinear(float[] imgBuffer, ModelImage transformedImg, TransMatrix trans,
                                                int iXdim, int iYdim, int iZdim, float iXres, float iYres, float iZres,
                                                ViewJProgressBar progressBar, boolean activeImage) {

        try {
            int i, j, k;
            float X, Y, Z;
            int x0, y0, z0;
            float k1, k2, k3, j1, j2, j3;
            float value;
            int sliceSize;
            float imm, jmm, kmm;
            int position1, position2;
            float b1, b2;
            float dx, dy, dz, dx1, dy1;
            float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
            int oXdim, oYdim, oZdim;
            float oXres, oYres, oZres;
            int deltaX, deltaY, deltaZ;

            oXdim = transformedImg.getFileInfo()[0].getExtents()[0];
            oYdim = transformedImg.getFileInfo()[0].getExtents()[1];
            oZdim = transformedImg.getFileInfo()[0].getExtents()[2];

            oXres = transformedImg.getFileInfo()[0].getResolutions()[0];
            oYres = transformedImg.getFileInfo()[0].getResolutions()[1];
            oZres = transformedImg.getFileInfo()[0].getResolutions()[2];
            sliceSize = iXdim * iYdim;

            imgOrigin = (float[]) transformedImg.getFileInfo(0).getOrigin().clone();

            if (updateOrigin) {
                updateOrigin(trans);
            }

            TransMatrix kTM = matrixtoInverseArray(trans);
            T00 = kTM.M00;
            T01 = kTM.M01;
            T02 = kTM.M02;
            T03 = kTM.M03;
            T10 = kTM.M10;
            T11 = kTM.M11;
            T12 = kTM.M12;
            T13 = kTM.M13;
            T20 = kTM.M20;
            T21 = kTM.M21;
            T22 = kTM.M22;
            T23 = kTM.M23;

            float invXRes = 1 / iXres;
            float invYRes = 1 / iYres;
            float invZRes = 1 / iZres;

            float min = Float.MAX_VALUE;

            for (int run = 0; run < imgBuffer.length; run++) {

                if (min > imgBuffer[run]) {
                    min = imgBuffer[run];
                }
            }

            int mod = Math.max(1, oZdim / 50);
            int index = 0;

            for (k = 0; k < oZdim; k++) {

                if ((progressBar != null) && ((k % mod) == 0)) {
                    progressBar.updateValue((int) (((float) k / oZdim * 100) + 0.5f), activeImage);
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

                        value = min; // remains zero if voxel is transformed out of bounds
                        imm = i * oXres;
                        X = (j1 + (imm * T00)) * invXRes;

                        if ((X > -0.5f) && (X < iXdim)) {
                            Y = (j2 + (imm * T10)) * invYRes;

                            if ((Y > -0.5f) && (Y < iYdim)) {
                                Z = (j3 + (imm * T20)) * invZRes;

                                if ((Z > -0.5f) && (Z < iZdim)) {

                                    if (X <= 0) {
                                        x0 = 0;
                                        dx = 0;
                                        deltaX = 0;
                                    } else if (X >= (iXdim - 1)) {
                                        x0 = iXdim - 1;
                                        dx = 0;
                                        deltaX = 0;
                                    } else {
                                        x0 = (int) X;
                                        dx = X - x0;
                                        deltaX = 1;
                                    }

                                    if (Y <= 0) {
                                        y0 = 0;
                                        dy = 0;
                                        deltaY = 0;
                                    } else if (Y >= (iYdim - 1)) {
                                        y0 = iYdim - 1;
                                        dy = 0;
                                        deltaY = 0;
                                    } else {
                                        y0 = (int) Y;
                                        dy = Y - y0;
                                        deltaY = iXdim;
                                    }

                                    if (Z <= 0) {
                                        z0 = 0;
                                        dz = 0;
                                        deltaZ = 0;
                                    } else if (Z >= (iZdim - 1)) {
                                        z0 = iZdim - 1;
                                        dz = 0;
                                        deltaZ = 0;
                                    } else {
                                        z0 = (int) Z;
                                        dz = Z - z0;
                                        deltaZ = sliceSize;
                                    }

                                    dx1 = 1 - dx;
                                    dy1 = 1 - dy;

                                    position1 = (z0 * sliceSize) + (y0 * iXdim) + x0;
                                    position2 = position1 + deltaZ;

                                    b1 = (dy1 * ((dx1 * imgBuffer[position1]) + (dx * imgBuffer[position1 + deltaX]))) +
                                         (dy *
                                              ((dx1 * imgBuffer[position1 + deltaY]) +
                                                   (dx * imgBuffer[position1 + deltaY + deltaX])));

                                    b2 = (dy1 * ((dx1 * imgBuffer[position2]) + (dx * imgBuffer[position2 + deltaX]))) +
                                         (dy *
                                              ((dx1 * imgBuffer[position2 + deltaY]) +
                                                   (dx * imgBuffer[position2 + deltaY + deltaX])));

                                    value = ((1 - dz) * b1) + (dz * b2);
                                } // end if Z in bounds
                            } // end if Y in bounds
                        } // end if X in bounds

                        transformedImg.set(index++, value);
                    } // end for k
                } // end for j
            } // end for i
        } finally { }
    }

    /**
     * Transforms and resamples volume using trilinear interpolation.
     *
     * @param  imgBuffer    Image buffer to be transformed
     * @param  tImgBuf      Transformed image
     * @param  trans        Transformation matrix to be applied
     * @param  iXdim        X dimension of input image
     * @param  iYdim        Y dimension of input image
     * @param  iZdim        Z dimension of input image
     * @param  iXres        X resolution of input image
     * @param  iYres        Y resolution of input image
     * @param  iZres        Z resolution of input image
     * @param  oXdim        X dimension of output image
     * @param  oYdim        Y dimension of output image
     * @param  oZdim        Z dimension of output image
     * @param  oXres        X resolution of output image
     * @param  oYres        Y resolution of output image
     * @param  oZres        Z resolution of output image
     * @param  progressBar  Progress bar. Can be null. Should NOT have cancel button.
     */
    public static final void transformTrilinear(float[] imgBuffer, float[] tImgBuf, TransMatrix trans, int iXdim,
                                                int iYdim, int iZdim, float iXres, float iYres, float iZres, int oXdim,
                                                int oYdim, int oZdim, float oXres, float oYres, float oZres,
                                                ViewJProgressBar progressBar) {
        transformTrilinear(imgBuffer, tImgBuf, trans, iXdim, iYdim, iZdim, iXres, iYres, iZres, oXdim, oYdim, oZdim,
                           oXres, oYres, oZres, progressBar, true);
    }

    /**
     * Transforms and resamples volume using trilinear interpolation.
     *
     * @param  imgBuffer    Image buffer to be transformed
     * @param  tImgBuf      Transformed image
     * @param  trans        Transformation matrix to be applied
     * @param  iXdim        X dimension of input image
     * @param  iYdim        Y dimension of input image
     * @param  iZdim        Z dimension of input image
     * @param  iXres        X resolution of input image
     * @param  iYres        Y resolution of input image
     * @param  iZres        Z resolution of input image
     * @param  oXdim        X dimension of output image
     * @param  oYdim        Y dimension of output image
     * @param  oZdim        Z dimension of output image
     * @param  oXres        X resolution of output image
     * @param  oYres        Y resolution of output image
     * @param  oZres        Z resolution of output image
     * @param  progressBar  Progress bar. Can be null. Should NOT have cancel button.
     * @param  activeImage  true if the algorithm is being run in a separate thread, false otherwise, to control
     *                      progress bar repainting
     */
    public static final void transformTrilinear(float[] imgBuffer, float[] tImgBuf, TransMatrix trans, int iXdim,
                                                int iYdim, int iZdim, float iXres, float iYres, float iZres, int oXdim,
                                                int oYdim, int oZdim, float oXres, float oYres, float oZres,
                                                ViewJProgressBar progressBar, boolean activeImage) {

        int i, j, k;
        float X, Y, Z;
        int x0, y0, z0;
        float value;
        int sliceSize;
        float imm, jmm, kmm;
        float k1, k2, k3, j1, j2, j3;

        sliceSize = iXdim * iYdim;

        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        int deltaX, deltaY, deltaZ;

        int mod = Math.max(1, oZdim / 50);

        TransMatrix kTM = matrixtoInverseArray(trans);

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;

        int position1, position2;
        float b1, b2;
        float dx, dy, dz, dx1, dy1;

        float invXRes = 1 / iXres;
        float invYRes = 1 / iYres;
        float invZRes = 1 / iZres;

        float min = Float.MAX_VALUE;

        for (int run = 0; run < imgBuffer.length; run++) {

            if (min > imgBuffer[run]) {
                min = imgBuffer[run];
            }
        }

        int index = 0;

        for (k = 0; k < oZdim; k++) {

            if ((progressBar != null) && ((k % mod) == 0)) {
                progressBar.updateValue((int) (((float) k / oZdim * 100) + 0.5f), activeImage);
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
                    value = min; // remains zero if voxel is transformed out of bounds
                    imm = i * oXres;
                    X = (j1 + (imm * T00)) * invXRes;

                    if ((X > -0.5f) && (X < iXdim)) {
                        Y = (j2 + (imm * T10)) * invYRes;

                        if ((Y > -0.5f) && (Y < iYdim)) {
                            Z = (j3 + (imm * T20)) * invZRes;

                            if ((Z > -0.5f) && (Z < iZdim)) {

                                if (X <= 0) {
                                    x0 = 0;
                                    dx = 0;
                                    deltaX = 0;
                                } else if (X >= (iXdim - 1)) {
                                    x0 = iXdim - 1;
                                    dx = 0;
                                    deltaX = 0;
                                } else {
                                    x0 = (int) X;
                                    dx = X - x0;
                                    deltaX = 1;
                                }

                                if (Y <= 0) {
                                    y0 = 0;
                                    dy = 0;
                                    deltaY = 0;
                                } else if (Y >= (iYdim - 1)) {
                                    y0 = iYdim - 1;
                                    dy = 0;
                                    deltaY = 0;
                                } else {
                                    y0 = (int) Y;
                                    dy = Y - y0;
                                    deltaY = iXdim;
                                }

                                if (Z <= 0) {
                                    z0 = 0;
                                    dz = 0;
                                    deltaZ = 0;
                                } else if (Z >= (iZdim - 1)) {
                                    z0 = iZdim - 1;
                                    dz = 0;
                                    deltaZ = 0;
                                } else {
                                    z0 = (int) Z;
                                    dz = Z - z0;
                                    deltaZ = sliceSize;
                                }

                                dx1 = 1 - dx;
                                dy1 = 1 - dy;

                                position1 = (z0 * sliceSize) + (y0 * iXdim) + x0;
                                position2 = position1 + deltaZ;

                                b1 = (dy1 * ((dx1 * imgBuffer[position1]) + (dx * imgBuffer[position1 + deltaX]))) +
                                     (dy *
                                          ((dx1 * imgBuffer[position1 + deltaY]) +
                                               (dx * imgBuffer[position1 + deltaY + deltaX])));

                                b2 = (dy1 * ((dx1 * imgBuffer[position2]) + (dx * imgBuffer[position2 + deltaX]))) +
                                     (dy *
                                          ((dx1 * imgBuffer[position2 + deltaY]) +
                                               (dx * imgBuffer[position2 + deltaY + deltaX])));

                                value = ((1 - dz) * b1) + (dz * b2);
                            } // end if Z in bounds
                        } // end if Y in bounds
                    } // end if X in bounds

                    tImgBuf[index++] = value;
                } // end for k
            } // end for j
        } // end for i
    }

    /**
     * Performs trilinear interpolation on black and white image data in a 4D image. Works time slice by time slice
     *
     * @param   image        Image from which the data is derived
     * @param   resultImage  Image to put result in; can be null.
     * @param   xfrm         Transformation to apply.
     * @param   progressBar  Buffer containing image data.
     *
     * @return  DOCUMENT ME!
     */
    public static ModelImage transformTrilinear4D(ModelImage image, ModelImage resultImage, TransMatrix xfrm,
                                                  ViewJProgressBar progressBar) {

        try {
            float X, Y, Z;
            int x0, y0, z0;
            float value;
            int sliceSize;
            float imm, jmm, kmm;
            int counter = 0;
            int position1, position2;
            float b1, b2;
            float dx, dy, dz, dx1, dy1;
            float k1, k2, k3, j1, j2, j3;
            float[] resols = new float[3];

            int xDim = image.getExtents()[0];
            int yDim = image.getExtents()[1];
            int zDim = image.getExtents()[2];
            float xRes = image.getFileInfo(0).getResolutions()[0];
            float yRes = image.getFileInfo(0).getResolutions()[1];
            float zRes = image.getFileInfo(0).getResolutions()[2];

            int bufferSize = image.getSliceSize() * image.getExtents()[2];
            float[] imgBuffer = new float[bufferSize];

            int length;
            int tDim;
            int mod; // mod is 1 percent of length
            float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

            resols[0] = image.getFileInfo()[0].getResolutions()[0];
            resols[1] = image.getFileInfo()[0].getResolutions()[1];
            resols[2] = image.getFileInfo()[0].getResolutions()[2];
            sliceSize = xDim * yDim;
            tDim = image.getExtents()[3];
            length = xDim * yDim * zDim * tDim;
            mod = length / 100;

            TransMatrix kTM = matrixtoInverseArray(xfrm);
            T00 = kTM.M00;
            T01 = kTM.M01;
            T02 = kTM.M02;
            T03 = kTM.M03;
            T10 = kTM.M10;
            T11 = kTM.M11;
            T12 = kTM.M12;
            T13 = kTM.M13;
            T20 = kTM.M20;
            T21 = kTM.M21;
            T22 = kTM.M22;
            T23 = kTM.M23;

            float invXRes = 1 / xRes;
            float invYRes = 1 / yRes;
            float invZRes = 1 / zRes;

            int index = 0;

            for (int l = 0; l < tDim; l++) {

                if ((progressBar != null) && ((counter % mod) == 0)) {
                    progressBar.updateValue(Math.round((float) l / (tDim - 1) * 100));
                }

                for (int k = 0; k < zDim; k++) {
                    kmm = k * zRes;
                    k1 = (kmm * T02) + T03;
                    k2 = (kmm * T12) + T13;
                    k3 = (kmm * T22) + T23;

                    for (int j = 0; j < yDim; j++) {
                        jmm = j * yRes;
                        j1 = (jmm * T01) + k1;
                        j2 = (jmm * T11) + k2;
                        j3 = (jmm * T21) + k3;

                        for (int i = 0; i < xDim; i++) {

                            // transform i,j,k
                            value = 0; // remains zero if voxel is transformed out of bounds
                            imm = i * xRes;
                            X = (j1 + (imm * T00)) * invXRes;

                            if ((X >= 0) && (X < xDim)) {
                                Y = (j2 + (imm * T10)) * invYRes;

                                if ((Y >= 0) && (Y < yDim)) {
                                    Z = (j3 + (imm * T20)) * invZRes;

                                    if ((Z >= 0) && (Z < zDim)) {

                                        x0 = (int) X;
                                        y0 = (int) Y;
                                        z0 = (int) Z;

                                        dx = X - x0;
                                        dy = Y - y0;
                                        dz = Z - z0;

                                        dx1 = 1 - dx;
                                        dy1 = 1 - dy;

                                        position1 = (z0 * sliceSize) + (y0 * xDim) + x0;
                                        position2 = position1 + sliceSize;

                                        b1 = (dy1 * ((dx1 * imgBuffer[position1]) + (dx * imgBuffer[position1 + 1]))) +
                                             (dy *
                                                  ((dx1 * imgBuffer[position1 + xDim]) +
                                                       (dx * imgBuffer[position1 + xDim + 1])));

                                        b2 = (dy1 * ((dx1 * imgBuffer[position2]) + (dx * imgBuffer[position2 + 1]))) +
                                             (dy *
                                                  ((dx1 * imgBuffer[position2 + xDim]) +
                                                       (dx * imgBuffer[position2 + xDim + 1])));

                                        value = ((1 - dz) * b1) + (dz * b2);

                                    } // end if Z in bounds
                                } // end if Y in bounds
                            } // end if X in bounds

                            if (resultImage != null) {
                                resultImage.set(index++, value);
                            } else {
                                image.set(index++, value);
                            }
                        } // end for k
                    } // end for j
                } // end for i

                if (l < (tDim - 1)) {

                    try {

                        if (resultImage != null) {
                            resultImage.exportData((l + 1) * imgBuffer.length, imgBuffer.length, imgBuffer);
                        } else {
                            image.exportData((l + 1) * imgBuffer.length, imgBuffer.length, imgBuffer);
                        }
                    } catch (IOException error) {
                        MipavUtil.displayError("ViewJFrameTriImage: IOException error on exportData");
                    }
                } // if (l < (tDim - 1))
            } // end for l

            if (resultImage != null) {
                resultImage.calcMinMax();

                return resultImage;
            } else {
                image.calcMinMax();

                return image;
            }
        } finally { }
    }

    /**
     * Performs trilinear interpolation on color image data.
     *
     * @param   image        Image from which the data is derived
     * @param   resultImage  Image to put result in; can be null.
     * @param   imgBuffer    Buffer containing image data.
     * @param   xfrm         Transformation to apply.
     * @param   progressBar  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public static ModelImage transformTrilinearC(ModelImage image, ModelImage resultImage, float[] imgBuffer,
                                                 TransMatrix xfrm, ViewJProgressBar progressBar) {

        try {
            int X0pos, Y0pos, Z0pos;
            int X1pos, Y1pos, Z1pos;
            int roundX, roundY, roundZ;
            float X, Y, Z;
            float x0, y0, z0;
            float x1, y1, z1;
            float valueA, valueR, valueG, valueB;
            int sliceSize;
            float imm, jmm, kmm;
            int counter = 0;
            int xDim = image.getExtents()[0];
            int yDim = image.getExtents()[1];
            int zDim = image.getExtents()[2];
            float[] resols = new float[3];
            int length = xDim * yDim * zDim;
            int mod = length / 100; // mod is 1 percent of length
            float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
            int index;
            int index000, index001, index010, index011, index100, index101, index110, index111;
            float w000, w001, w010, w011, w100, w101, w110, w111;

            float[] imgBuffer2 = new float[imgBuffer.length];

            resols[0] = image.getFileInfo()[0].getResolutions()[0];
            resols[1] = image.getFileInfo()[0].getResolutions()[1];
            resols[2] = image.getFileInfo()[0].getResolutions()[2];
            sliceSize = xDim * yDim;
            TransMatrix kTM = matrixtoInverseArray(xfrm);
            T00 = kTM.M00;
            T01 = kTM.M01;
            T02 = kTM.M02;
            T03 = kTM.M03;
            T10 = kTM.M10;
            T11 = kTM.M11;
            T12 = kTM.M12;
            T13 = kTM.M13;
            T20 = kTM.M20;
            T21 = kTM.M21;
            T22 = kTM.M22;
            T23 = kTM.M23;

            for (int i = 0; i < xDim; i++) {

                for (int j = 0; j < yDim; j++) {

                    for (int k = 0; k < zDim; k++) {

                        if ((progressBar != null) && ((counter % mod) == 0)) {
                            progressBar.updateValue(Math.round((float) counter / (length - 1) * 100), true);
                        }

                        // transform i,j,k
                        valueA = 0; // remains zero if voxel is transformed out of bounds
                        valueR = 0;
                        valueG = 0;
                        valueB = 0;
                        imm = (float) i * resols[0];
                        jmm = (float) j * resols[1];
                        kmm = (float) k * resols[2];

                        X = (imm * T00) + (jmm * T01) + (kmm * T02) + T03;
                        X = X / resols[0];
                        roundX = (int) (X + 0.5f);

                        if ((X >= 0) && (X < xDim)) {
                            Y = (imm * T10) + (jmm * T11) + (kmm * T12) + T13;
                            Y = Y / resols[1];
                            roundY = (int) (Y + 0.5f);

                            if ((Y >= 0) && (Y < yDim)) {
                                Z = (imm * T20) + (jmm * T21) + (kmm * T22) + T23;
                                Z = Z / resols[2];
                                roundZ = (int) (Z + 0.5f);

                                if ((Z >= 0) && (Z < zDim)) {

                                    if ((X >= (xDim - 1)) || (Y >= (yDim - 1)) || (Z >= (zDim - 1))) { // cannot interpolate on last X, Y, or Z
                                        X0pos = Math.min(roundX, xDim - 1);
                                        Y0pos = Math.min(roundY, yDim - 1) * xDim;
                                        Z0pos = Math.min(roundZ, zDim - 1) * sliceSize;
                                        index = 4 * (Z0pos + Y0pos + X0pos);
                                        valueA = imgBuffer[index];
                                        valueR = imgBuffer[index + 1];
                                        valueG = imgBuffer[index + 2];
                                        valueB = imgBuffer[index + 3];
                                    } else {

                                        // set intensity of i,j,k to new transformed coordinate if
                                        // x,y,z is w/in dimensions of image
                                        x0 = X - (int) X;
                                        y0 = Y - (int) Y;
                                        z0 = Z - (int) Z;
                                        x1 = 1 - x0;
                                        y1 = 1 - y0;
                                        z1 = 1 - z0;
                                        X0pos = (int) X;
                                        Y0pos = (int) Y * xDim;
                                        Z0pos = (int) Z * sliceSize;
                                        X1pos = X0pos + 1;
                                        Y1pos = Y0pos + xDim;
                                        Z1pos = Z0pos + sliceSize;
                                        index000 = 4 * (Z0pos + Y0pos + X0pos);
                                        index001 = 4 * (Z0pos + Y0pos + X1pos);
                                        index010 = 4 * (Z0pos + Y1pos + X0pos);
                                        index011 = 4 * (Z0pos + Y1pos + X1pos);
                                        index100 = 4 * (Z1pos + Y0pos + X0pos);
                                        index101 = 4 * (Z1pos + Y0pos + X1pos);
                                        index110 = 4 * (Z1pos + Y1pos + X0pos);
                                        index111 = 4 * (Z1pos + Y1pos + X1pos);
                                        w000 = x1 * y1 * z1;
                                        w001 = x0 * y1 * z1;
                                        w010 = x1 * y0 * z1;
                                        w011 = x0 * y0 * z1;
                                        w100 = x1 * y1 * z0;
                                        w101 = x0 * y1 * z0;
                                        w110 = x1 * y0 * z0;
                                        w111 = x0 * y0 * z0;

                                        valueA = (w000 * imgBuffer[index000]) + (w001 * imgBuffer[index001]) +
                                                 (w010 * imgBuffer[index010]) + (w011 * imgBuffer[index011]) +
                                                 (w100 * imgBuffer[index100]) + (w101 * imgBuffer[index101]) +
                                                 (w110 * imgBuffer[index110]) + (w111 * imgBuffer[index111]);
                                        valueR = (w000 * imgBuffer[index000 + 1]) + (w001 * imgBuffer[index001 + 1]) +
                                                 (w010 * imgBuffer[index010 + 1]) + (w011 * imgBuffer[index011 + 1]) +
                                                 (w100 * imgBuffer[index100 + 1]) + (w101 * imgBuffer[index101 + 1]) +
                                                 (w110 * imgBuffer[index110 + 1]) + (w111 * imgBuffer[index111 + 1]);
                                        valueG = (w000 * imgBuffer[index000 + 2]) + (w001 * imgBuffer[index001 + 2]) +
                                                 (w010 * imgBuffer[index010 + 2]) + (w011 * imgBuffer[index011 + 2]) +
                                                 (w100 * imgBuffer[index100 + 2]) + (w101 * imgBuffer[index101 + 2]) +
                                                 (w110 * imgBuffer[index110 + 2]) + (w111 * imgBuffer[index111 + 2]);
                                        valueB = (w000 * imgBuffer[index000 + 3]) + (w001 * imgBuffer[index001 + 3]) +
                                                 (w010 * imgBuffer[index010 + 3]) + (w011 * imgBuffer[index011 + 3]) +
                                                 (w100 * imgBuffer[index100 + 3]) + (w101 * imgBuffer[index101 + 3]) +
                                                 (w110 * imgBuffer[index110 + 3]) + (w111 * imgBuffer[index111 + 3]);
                                    }
                                } // end if Z in bounds
                            } // end if Y in bounds
                        } // end if X in bounds

                        index = 4 * (i + (j * xDim) + (k * sliceSize));
                        imgBuffer2[index] = valueA;
                        imgBuffer2[index + 1] = valueR;
                        imgBuffer2[index + 2] = valueG;
                        imgBuffer2[index + 3] = valueB;
                        counter++;
                    } // end for k
                } // end for j
            } // end for i

            try {

                if (resultImage != null) {
                    resultImage.importData(0, imgBuffer2, true);

                    return resultImage;
                } else {
                    image.importData(0, imgBuffer2, true);

                    return image;
                }
            } catch (IOException error) {
                MipavUtil.displayError("ViewJFrameTriImage: IOException Error on importData");

                return null;
            }
        } finally { }
    }

    /**
     * Transforms and resamples volume using trilinear interpolation Use on color images USE THIS IF OUTPUT IMAGE HAS
     * DIFFERENT DIM/RES THAN INPUT IMAGE.
     *
     * @param  image           Image to transform
     * @param  transformedImg  Result image.
     * @param  trans           Transformation matrix to be applied
     * @param  oXdim           DOCUMENT ME!
     * @param  oYdim           DOCUMENT ME!
     * @param  oZdim           DOCUMENT ME!
     * @param  oXres           DOCUMENT ME!
     * @param  oYres           DOCUMENT ME!
     * @param  oZres           DOCUMENT ME!
     */
    public static final void transformTrilinearC(ModelImage image, ModelImage transformedImg, TransMatrix trans,
                                                 int oXdim, int oYdim, int oZdim, float oXres, float oYres,
                                                 float oZres) {
        int i, j, k;
        int X0pos, Y0pos, Z0pos;
        int X1pos, Y1pos, Z1pos;
        float X, Y, Z;
        float x0, y0, z0;
        float x1, y1, z1;
        int sliceSize;
        float imm, jmm, kmm;
        int roundX, roundY, roundZ;
        float temp1, temp2, temp3, temp4, temp5, temp6, temp7;

        int iXdim, iYdim, iZdim;
        float iXres, iYres, iZres;

        iXdim = image.getExtents()[0];
        iYdim = image.getExtents()[1];
        iZdim = image.getExtents()[2];
        iXres = image.getFileInfo(0).getResolutions()[0];
        iYres = image.getFileInfo(0).getResolutions()[1];
        iZres = image.getFileInfo(0).getResolutions()[2];

        sliceSize = iXdim * iYdim;
        imgOrigin = (float[]) image.getFileInfo(0).getOrigin().clone();

        if (updateOrigin) {
            updateOrigin(trans);
        }

        TransMatrix kTM = matrixtoInverseArray(trans);

        float i1, i2, i3, j1, j2, j3;
        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;

        int imgLength = 4 * iXdim * iYdim * iZdim;
        float[] imgBuffer = new float[imgLength];
        float[] imgBuffer2 = new float[imgLength];
        int indexDest;
        int index;
        int index000, index001, index010, index011, index100, index101, index110, index111;

        try {
            image.exportData(0, imgLength, imgBuffer);
        } catch (IOException error) {
            MipavUtil.displayError("Algorithm Transform: Image(s) locked");

            return;
        }

        // int   [] extents      = new int   [] {oXdim, oYdim, oZdim};
        float[] resolutions = new float[] { oXres, oYres, oZres };

        updateFileInfo(image, transformedImg, resolutions, image.getFileInfo()[0].getUnitsOfMeasure(), trans, false);

        for (i = 0; i < oXdim; i++) {
            imm = (float) i * oXres;
            i1 = (imm * T00) + T03;
            i2 = (imm * T10) + T13;
            i3 = (imm * T20) + T23;

            for (j = 0; j < oYdim; j++) {
                jmm = (float) j * oYres;
                j1 = jmm * T01;
                j2 = jmm * T11;
                j3 = jmm * T21;
                temp1 = i3 + j3;
                temp2 = i2 + j2;
                temp3 = i1 + j1;

                for (k = 0; k < oZdim; k++) {

                    // transform i,j,k
                    indexDest = 4 * (i + (j * oXdim) + (k * oXdim * oYdim));
                    imgBuffer2[indexDest] = 255; // alpha default
                    imgBuffer2[indexDest + 1] = 0; // R, G, and B remain zero if voxel is transformed out of bounds
                    imgBuffer2[indexDest + 2] = 0;
                    imgBuffer2[indexDest + 3] = 0;
                    kmm = (float) k * oZres;
                    X = (temp3 + (kmm * T02)) / iXres;
                    roundX = (int) (X + 0.5f);

                    if ((X >= 0) && (X < iXdim)) {
                        Y = temp2 + (kmm * T12);
                        Y = Y / iYres;
                        roundY = (int) (Y + 0.5f);

                        if ((Y >= 0) && (Y < iYdim)) {
                            Z = temp1 + (kmm * T22);
                            Z = Z / iZres;
                            roundZ = (int) (Z + 0.5f);

                            if ((Z >= 0) && (Z < iZdim)) {

                                if ((X >= (iXdim - 1)) || (Y >= (iYdim - 1)) || (Z >= (iZdim - 1))) { // cannot interpolate on last X, Y, or Z
                                    X0pos = Math.min(roundX, iXdim - 1);
                                    Y0pos = Math.min(roundY, iYdim - 1) * iXdim;
                                    Z0pos = Math.min(roundZ, iZdim - 1) * sliceSize;
                                    index = 4 * (Z0pos + Y0pos + X0pos);

                                    // imgBuffer2[indexDest] = imgBuffer[index];
                                    imgBuffer2[indexDest + 1] = imgBuffer[index + 1];
                                    imgBuffer2[indexDest + 2] = imgBuffer[index + 2];
                                    imgBuffer2[indexDest + 3] = imgBuffer[index + 3];
                                } else {

                                    // set intensity of i,j,k to new transformed coordinate if
                                    // x,y,z is w/in dimensions of image
                                    x0 = X - (int) (X);
                                    y0 = Y - (int) (Y);
                                    z0 = Z - (int) (Z);
                                    x1 = 1 - x0;
                                    y1 = 1 - y0;
                                    z1 = 1 - z0;
                                    X0pos = (int) (X);
                                    Y0pos = (int) (Y) * iXdim;
                                    Z0pos = (int) (Z) * sliceSize;
                                    X1pos = X0pos + 1;
                                    Y1pos = Y0pos + iXdim;
                                    Z1pos = Z0pos + sliceSize;
                                    temp4 = y1 * z1;
                                    temp5 = y0 * z1;
                                    temp6 = y1 * z0;
                                    temp7 = y0 * z0;
                                    index000 = 4 * (Z0pos + Y0pos + X0pos);
                                    index001 = 4 * (Z0pos + Y0pos + X1pos);
                                    index010 = 4 * (Z0pos + Y1pos + X0pos);
                                    index011 = 4 * (Z0pos + Y1pos + X1pos);
                                    index100 = 4 * (Z1pos + Y0pos + X0pos);
                                    index101 = 4 * (Z1pos + Y0pos + X1pos);
                                    index110 = 4 * (Z1pos + Y1pos + X0pos);
                                    index111 = 4 * (Z1pos + Y1pos + X1pos);

                                    imgBuffer2[indexDest + 1] = (x1 * temp4 * imgBuffer[index000 + 1]) +
                                                                (x0 * temp4 * imgBuffer[index001 + 1]) +
                                                                (x1 * temp5 * imgBuffer[index010 + 1]) +
                                                                (x0 * temp5 * imgBuffer[index011 + 1]) +
                                                                (x1 * temp6 * imgBuffer[index100 + 1]) +
                                                                (x0 * temp6 * imgBuffer[index101 + 1]) +
                                                                (x1 * temp7 * imgBuffer[index110 + 1]) +
                                                                (x0 * temp7 * imgBuffer[index111 + 1]);
                                    imgBuffer2[indexDest + 2] = (x1 * temp4 * imgBuffer[index000 + 2]) +
                                                                (x0 * temp4 * imgBuffer[index001 + 2]) +
                                                                (x1 * temp5 * imgBuffer[index010 + 2]) +
                                                                (x0 * temp5 * imgBuffer[index011 + 2]) +
                                                                (x1 * temp6 * imgBuffer[index100 + 2]) +
                                                                (x0 * temp6 * imgBuffer[index101 + 2]) +
                                                                (x1 * temp7 * imgBuffer[index110 + 2]) +
                                                                (x0 * temp7 * imgBuffer[index111 + 2]);
                                    imgBuffer2[indexDest + 3] = (x1 * temp4 * imgBuffer[index000 + 3]) +
                                                                (x0 * temp4 * imgBuffer[index001 + 3]) +
                                                                (x1 * temp5 * imgBuffer[index010 + 3]) +
                                                                (x0 * temp5 * imgBuffer[index011 + 3]) +
                                                                (x1 * temp6 * imgBuffer[index100 + 3]) +
                                                                (x0 * temp6 * imgBuffer[index101 + 3]) +
                                                                (x1 * temp7 * imgBuffer[index110 + 3]) +
                                                                (x0 * temp7 * imgBuffer[index111 + 3]);

                                }
                            } // end if Z in bounds
                        } // end if Y in bounds
                    } // end if X in bounds
                } // end for k
            } // end for j
        } // end for i

        try {
            transformedImg.importData(0, imgBuffer2, true);
        } catch (IOException error) {
            MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
        }
    }

    /**
     * Performs trilinear interpolation on color image data in a 4D image. Works time slice by time slice.
     *
     * @param   image        Image from which the data is derived
     * @param   resultImage  Image to put result in; can be null.
     * @param   xfrm         Transformation to apply.
     * @param   progressBar  Buffer containing image data.
     *
     * @return  DOCUMENT ME!
     */
    public static ModelImage transformTrilinearC4D(ModelImage image, ModelImage resultImage, TransMatrix xfrm,
                                                   ViewJProgressBar progressBar) {

        try {
            int X0pos, Y0pos, Z0pos;
            int X1pos, Y1pos, Z1pos;
            int roundX, roundY, roundZ;
            float X, Y, Z;
            float x0, y0, z0;
            float x1, y1, z1;
            float valueA, valueR, valueG, valueB;
            int sliceSize;
            float imm, jmm, kmm;
            int counter = 0;
            int length;
            int mod; // mod is 1 percent of length
            int tDim;
            float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
            int index;
            int index000, index001, index010, index011, index100, index101, index110, index111;
            float w000, w001, w010, w011, w100, w101, w110, w111;
            int xDim = image.getExtents()[0];
            int yDim = image.getExtents()[1];
            int zDim = image.getExtents()[2];
            float[] resols = new float[3];

            int bufferSize = image.getSliceSize() * image.getExtents()[2];

            float[] imgBuffer = new float[bufferSize];

            try {

                if (resultImage != null) {
                    resultImage.exportData(0, bufferSize, imgBuffer);
                } else {
                    image.exportData(0, bufferSize, imgBuffer);
                }
            } catch (IOException error) {
                MipavUtil.displayError("ViewJFrameTriImage: IOException error on exportData");
            }

            float[] imgBuffer2 = new float[imgBuffer.length];
            resols[0] = image.getFileInfo()[0].getResolutions()[0];
            resols[1] = image.getFileInfo()[0].getResolutions()[1];
            resols[2] = image.getFileInfo()[0].getResolutions()[2];
            sliceSize = xDim * yDim;
            tDim = image.getExtents()[3];
            length = xDim * yDim * zDim * tDim;
            mod = length / 100;

            TransMatrix kTM = matrixtoInverseArray(xfrm);
            T00 = kTM.M00;
            T01 = kTM.M01;
            T02 = kTM.M02;
            T03 = kTM.M03;
            T10 = kTM.M10;
            T11 = kTM.M11;
            T12 = kTM.M12;
            T13 = kTM.M13;
            T20 = kTM.M20;
            T21 = kTM.M21;
            T22 = kTM.M22;
            T23 = kTM.M23;

            for (int l = 0; l < tDim; l++) {

                for (int i = 0; i < xDim; i++) {

                    for (int j = 0; j < yDim; j++) {

                        for (int k = 0; k < zDim; k++) {

                            if ((progressBar != null) && ((counter % mod) == 0)) {
                                progressBar.updateValue(Math.round((float) counter / (length - 1) * 100), true);
                            }

                            // transform i,j,k
                            valueA = 0; // remains zero if voxel is transformed out of bounds
                            valueR = 0;
                            valueG = 0;
                            valueB = 0;
                            imm = (float) i * resols[0];
                            jmm = (float) j * resols[1];
                            kmm = (float) k * resols[2];

                            X = (imm * T00) + (jmm * T01) + (kmm * T02) + T03;
                            X = X / resols[0];
                            roundX = (int) (X + 0.5f);

                            if ((X >= 0) && (X < xDim)) {
                                Y = (imm * T10) + (jmm * T11) + (kmm * T12) + T13;
                                Y = Y / resols[1];
                                roundY = (int) (Y + 0.5f);

                                if ((Y >= 0) && (Y < yDim)) {
                                    Z = (imm * T20) + (jmm * T21) + (kmm * T22) + T23;
                                    Z = Z / resols[2];
                                    roundZ = (int) (Z + 0.5f);

                                    if ((Z >= 0) && (Z < zDim)) {

                                        if ((X >= (xDim - 1)) || (Y >= (yDim - 1)) ||
                                                (Z >= (zDim - 1))) { // cannot interpolate on last X, Y, or Z
                                            X0pos = Math.min(roundX, xDim - 1);
                                            Y0pos = Math.min(roundY, yDim - 1) * xDim;
                                            Z0pos = Math.min(roundZ, zDim - 1) * sliceSize;
                                            index = 4 * (Z0pos + Y0pos + X0pos);
                                            valueA = imgBuffer[index];
                                            valueR = imgBuffer[index + 1];
                                            valueG = imgBuffer[index + 2];
                                            valueB = imgBuffer[index + 3];
                                        } else {

                                            // set intensity of i,j,k to new transformed coordinate if
                                            // x,y,z is w/in dimensions of image
                                            x0 = X - (int) X;
                                            y0 = Y - (int) Y;
                                            z0 = Z - (int) Z;
                                            x1 = 1 - x0;
                                            y1 = 1 - y0;
                                            z1 = 1 - z0;
                                            X0pos = (int) X;
                                            Y0pos = (int) Y * xDim;
                                            Z0pos = (int) Z * sliceSize;
                                            X1pos = X0pos + 1;
                                            Y1pos = Y0pos + xDim;
                                            Z1pos = Z0pos + sliceSize;
                                            index000 = 4 * (Z0pos + Y0pos + X0pos);
                                            index001 = 4 * (Z0pos + Y0pos + X1pos);
                                            index010 = 4 * (Z0pos + Y1pos + X0pos);
                                            index011 = 4 * (Z0pos + Y1pos + X1pos);
                                            index100 = 4 * (Z1pos + Y0pos + X0pos);
                                            index101 = 4 * (Z1pos + Y0pos + X1pos);
                                            index110 = 4 * (Z1pos + Y1pos + X0pos);
                                            index111 = 4 * (Z1pos + Y1pos + X1pos);
                                            w000 = x1 * y1 * z1;
                                            w001 = x0 * y1 * z1;
                                            w010 = x1 * y0 * z1;
                                            w011 = x0 * y0 * z1;
                                            w100 = x1 * y1 * z0;
                                            w101 = x0 * y1 * z0;
                                            w110 = x1 * y0 * z0;
                                            w111 = x0 * y0 * z0;

                                            valueA = (w000 * imgBuffer[index000]) + (w001 * imgBuffer[index001]) +
                                                     (w010 * imgBuffer[index010]) + (w011 * imgBuffer[index011]) +
                                                     (w100 * imgBuffer[index100]) + (w101 * imgBuffer[index101]) +
                                                     (w110 * imgBuffer[index110]) + (w111 * imgBuffer[index111]);
                                            valueR = (w000 * imgBuffer[index000 + 1]) +
                                                     (w001 * imgBuffer[index001 + 1]) +
                                                     (w010 * imgBuffer[index010 + 1]) +
                                                     (w011 * imgBuffer[index011 + 1]) +
                                                     (w100 * imgBuffer[index100 + 1]) +
                                                     (w101 * imgBuffer[index101 + 1]) +
                                                     (w110 * imgBuffer[index110 + 1]) +
                                                     (w111 * imgBuffer[index111 + 1]);
                                            valueG = (w000 * imgBuffer[index000 + 2]) +
                                                     (w001 * imgBuffer[index001 + 2]) +
                                                     (w010 * imgBuffer[index010 + 2]) +
                                                     (w011 * imgBuffer[index011 + 2]) +
                                                     (w100 * imgBuffer[index100 + 2]) +
                                                     (w101 * imgBuffer[index101 + 2]) +
                                                     (w110 * imgBuffer[index110 + 2]) +
                                                     (w111 * imgBuffer[index111 + 2]);
                                            valueB = (w000 * imgBuffer[index000 + 3]) +
                                                     (w001 * imgBuffer[index001 + 3]) +
                                                     (w010 * imgBuffer[index010 + 3]) +
                                                     (w011 * imgBuffer[index011 + 3]) +
                                                     (w100 * imgBuffer[index100 + 3]) +
                                                     (w101 * imgBuffer[index101 + 3]) +
                                                     (w110 * imgBuffer[index110 + 3]) +
                                                     (w111 * imgBuffer[index111 + 3]);
                                        }
                                    } // end if Z in bounds
                                } // end if Y in bounds
                            } // end if X in bounds

                            index = 4 * (i + (j * xDim) + (k * sliceSize));
                            imgBuffer2[index] = valueA;
                            imgBuffer2[index + 1] = valueR;
                            imgBuffer2[index + 2] = valueG;
                            imgBuffer2[index + 3] = valueB;
                            counter++;
                        } // end for k
                    } // end for j
                } // end for i

                try {

                    if (resultImage != null) {
                        resultImage.importData(l * imgBuffer2.length, imgBuffer2, false);
                    } else {
                        image.importData(l * imgBuffer2.length, imgBuffer2, false);
                    }
                } catch (IOException error) {
                    MipavUtil.displayError("ViewJFrameTriImage: IOException Error on importData");
                }

                if (l < (tDim - 1)) {

                    try {

                        if (resultImage != null) {
                            resultImage.exportData((l + 1) * imgBuffer.length, imgBuffer.length, imgBuffer);
                        } else {
                            image.exportData((l + 1) * imgBuffer.length, imgBuffer.length, imgBuffer);
                        }
                    } catch (IOException error) {
                        MipavUtil.displayError("ViewJFrameTriImage: IOException error on exportData");
                    }
                } // if (l < (tDim - 1))
            } // end for l

            if (resultImage != null) {
                resultImage.calcMinMax();

                return resultImage;
            } else {
                image.calcMinMax();

                return image;
            }
        } finally { }
    }

    /**
     * Dispose of local variables that may be taking up lots of room.
     */
    public void disposeLocal() {

        // System.err.println("Calling disposeLocal in algo transform");
        srcImage = null;
        destImage = null;
        maskImage = null;
        imgBuf = null;
        imgBuf2 = null;
        System.gc();
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        disposeLocal();
        super.finalize();
    }

    /**
     * Calculate necessary padding for image given applied transform.
     *
     * @param   srcImage     DOCUMENT ME!
     * @param   transMatrix  array with transformation matrix
     * @param   dxOut        DOCUMENT ME!
     * @param   dyOut        DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private int[] getImageMargins(ModelImage srcImage, TransMatrix transMatrix, float dxOut, float dyOut) {
        int i;
        float xi = 0.f;
        float yi = 0.f;
        float dx = srcImage.getFileInfo(0).getResolutions()[0];
        float dy = srcImage.getFileInfo(0).getResolutions()[1];
        int nx = srcImage.getExtents()[0];
        int ny = srcImage.getExtents()[1];
        int[] destExtents = new int[2];
        float xf = 0.f, yf = 0.f;
        float minx, miny, maxx, maxy;
        int leftPad = 0, rightPad = 0, topPad = 0, bottomPad = 0;
        Vector2f[] ptsi2, ptsf2;

        /* Set the far corner of the image volume in mm (but relative to image origin, in image coordinates). */
        xf = xi + (dx * (nx - 1));
        minx = xi;
        maxx = xf;
        yf = yi + (dy * (ny - 1));
        miny = yi;
        maxy = yf;
        // System.out.println("Far corner: " +(int)xf +", " +(int)yf +", " +(int)zf);

        /* Set up array of 4 points representing the corners of the image volume and then transform them with
         * transMatrix. */
        ptsi2 = new Vector2f[4];
        ptsf2 = new Vector2f[4];

        for (i = 1; i <= 4; i++) {
            ptsi2[i - 1] = new Vector2f();
            ptsf2[i - 1] = new Vector2f();

            if ((i == 1) || (i == 4)) {
                ptsi2[i - 1].X = xi;
            } else {
                ptsi2[i - 1].X = xf;
            }

            if ((i == 1) || (i == 2)) {
                ptsi2[i - 1].Y = yi;
            } else {
                ptsi2[i - 1].Y = yf;
            }

            //System.out.println("Initial point " +i +": " +(int)ptsi2[i-1].X +", " +(int)ptsi2[i-1].Y);
        }

        /* Transform corner points, ptsi2, to get transformed points, ptsf2. */
        transMatrix.transformAsPoint2Df(ptsi2, ptsf2);

        /* Find new min and max values for the transformed point. */
        for (i = 1; i <= 4; i++) {

            //System.out.println("Transformed point " +i +": " +(int)ptsf2[i-1].X +", " +(int)ptsf2[i-1].Y);
            if (ptsf2[i - 1].X < minx) {
                minx = ptsf2[i - 1].X;
            }

            if (ptsf2[i - 1].X > maxx) {
                maxx = ptsf2[i - 1].X;
            }

            if (ptsf2[i - 1].Y < miny) {
                miny = ptsf2[i - 1].Y;
            }

            if (ptsf2[i - 1].Y > maxy) {
                maxy = ptsf2[i - 1].Y;
            }
        }
        // System.out.println("Bounding box first corner: " +(int)minx +", " +(int)miny);
        // System.out.println("Bounding box far corner: " +(int)maxx +", " +(int)maxy);

        /* Calculate padding. */
        leftPad = (int) (((xi - minx) / dxOut) + 0.5);
        rightPad = (int) (((maxx - xf) / dxOut) + 0.5);

        //System.out.println("Padding in x is: " + leftPad +" and " +rightPad);
        topPad = (int) (((yi - miny) / dyOut) + 0.5);
        bottomPad = (int) (((maxy - yf) / dyOut) + 0.5);
        //System.out.println("Padding in y is: " + topPad+" and " +bottomPad);

        margins[0] = leftPad;
        margins[1] = topPad;
        margins[2] = rightPad;
        margins[3] = bottomPad;

        return margins;
    }


    /**
     * Returns transformed volume.
     *
     * @return  destImage
     */
    public ModelImage getTransformedImage() {
        return destImage;
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {
    	TransMatrix newMatrix;
        //TransMatrix newTMatrix;

        if (updateOrigin) {
            updateOrigin(this.transMatrix);
        }

        //System.err.println("MATRIX: " + transMatrix);

        // BEN: fix this so the origin is updated correctly
        updateFileInfo(srcImage, destImage, destResolutions, oUnits, this.transMatrix, isSATransform);

        if (pad && !canPad) {
            MipavUtil.displayError("For padding: interpolation linear and no resampling.");
            disposeLocal();
            setCompleted(false);
        } else {
            
            transform();

            // copy the src image's matrices into the destination image
            destImage.getMatrixHolder().replaceMatrices(srcImage.getMatrixHolder().getMatrices());

            // add the new transform matrix to the destination image
            transMatrix.setTransformID(TransMatrix.TRANSFORM_ANOTHER_DATASET);
            destImage.getMatrixHolder().addMatrix(transMatrix);

            if (transMatrix.isIdentity()) {
                // BEN: change
                // destImage.setMatrix(transMatrix);
                // destImage.getFileInfo(0).setTransformID(srcImage.getFileInfo(0).getTransformID());
            } else {

                // srcImage Matrix * transMatrix invert * [x y z]transpose
                // since (transMatrix invert * [x y z]transpose) takes the
                // destination image to the source image and srcImage Matrix
                // takes the source image to the axial image.
                // The translation to the center and away from the center are
                // not present since these translations are multiplied into
                // the transformation matrix in AlgorithmTransform.transform()
                // when rotation around the center is specified in JDialogTransform.
                transMatrix.Inverse();

                if (srcImage.getNDims() > 2) {

                    if (transMatrix.getDim() == 4) {
                        newMatrix = new TransMatrix(srcImage.getMatrix());
                        newMatrix.Mult(transMatrix);
                    } else { // 2.5D processing
                    	newMatrix = new TransMatrix(srcImage.getMatrix());
                        
                        TransMatrix mat3D = new TransMatrix(4);
                        mat3D.set(0, 0, transMatrix.M00);
                        mat3D.set(0, 1, transMatrix.M01);
                        mat3D.set(0, 2, 0.0);
                        mat3D.set(0, 3, transMatrix.M02);
                        mat3D.set(1, 0, transMatrix.M10);
                        mat3D.set(1, 1, transMatrix.M11);
                        mat3D.set(1, 2, 0.0);
                        mat3D.set(1, 3, transMatrix.M12);
                        mat3D.set(2, 0, transMatrix.M20);
                        mat3D.set(2, 1, transMatrix.M21);
                        mat3D.set(2, 2, transMatrix.M22);
                        mat3D.set(3, 3, 1.0);
                        newMatrix.Mult(mat3D);
                    }

                } else { // srcImage.getNDims() == 2
                    newMatrix = new TransMatrix(3);

                    // There is the posibility the for 2D DICOM that the matrix might  be 4x4
                    // If 3 x3 OK to load else the newMatrix is identity
                    if (srcImage.getMatrix().getDim() == 3) {
                        newMatrix.Copy(srcImage.getMatrix());
                    }

                    newMatrix.Mult(transMatrix);
                }


                // System.err.println("NEW MATRIX: " + newTMatrix);


                // replace the destination image's default (composite) matrix
                // newTMatrix.setTransformID(TransMatrix.TRANSFORM_COMPOSITE);
                // destImage.setMatrix(newTMatrix);

            }
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  center  DOCUMENT ME!
     */
    public void setCenter(Vector3f center) {
        this.center = center;
        if (pad) {
            
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  doCenter  DOCUMENT ME!
     */
    public void setDoCenter(boolean doCenter) {
        this.doCenter = doCenter;
    }

    /**
     * Set value for image padding during transformation. Not set in constructor because for most applications it will
     * be 0.
     *
     * @param  pad  DOCUMENT ME!
     */
    public void setPadValue(int pad) {
        padVal = pad;
    }

    /**
     * Sets the origin flag used indicated that origin should be changed based using the supplied transformation matrix.
     *
     * @param  originFlag  if true sets the updateOrigin flag to true
     */
    public void setUpdateOriginFlag(boolean originFlag) {
        updateOrigin = originFlag;
    }

    /**
     * Sets the tranform to set orientation to AXIAL (this is a scanner anatomical transform).
     *
     * @param  useSA  set to axial orientation
     */
    public void setUseScannerAnatomical(boolean useSA) {
        this.isSATransform = useSA;
    }

    /**
     * Copy important file information to resultant image structure.
     *
     * @param  image           Source image.
     * @param  resultImage     Resultant image.
     * @param  resolutions     DOCUMENT ME!
     * @param  units           DOCUMENT ME!
     * @param  matrix          DOCUMENT ME!
     * @param  useSATransform  DOCUMENT ME!
     */
    private static void updateFileInfo(ModelImage image, ModelImage resultImage, float[] resolutions, int[] units,
                                       TransMatrix matrix, boolean useSATransform) {
        FileInfoBase[] fileInfo = resultImage.getFileInfo();

        if (resultImage.getNDims() == 2) {
            fileInfo[0] = (FileInfoBase) image.getFileInfo(0).clone();
            fileInfo[0].setDataType(resultImage.getType());
            fileInfo[0].setModality(image.getFileInfo()[0].getModality());
            fileInfo[0].setFileDirectory(image.getFileInfo()[0].getFileDirectory());
            fileInfo[0].setEndianess(image.getFileInfo()[0].getEndianess());
            fileInfo[0].setUnitsOfMeasure(image.getFileInfo()[0].getUnitsOfMeasure());
            fileInfo[0].setResolutions(image.getFileInfo()[0].getResolutions());
            fileInfo[0].setExtents(resultImage.getExtents());
            fileInfo[0].setMax(resultImage.getMax());
            fileInfo[0].setMin(resultImage.getMin());
            fileInfo[0].setImageOrientation(image.getImageOrientation());
            fileInfo[0].setAxisOrientation(image.getFileInfo()[0].getAxisOrientation());
            fileInfo[0].setOrigin(image.getFileInfo()[0].getOrigin());
            fileInfo[0].setPixelPadValue(image.getFileInfo()[0].getPixelPadValue());
            fileInfo[0].setPhotometric(image.getFileInfo()[0].getPhotometric());
            fileInfo[0].setUnitsOfMeasure(units);
                  
        } else if (resultImage.getNDims() == 3) {
            float[] coord = new float[3];
            float[] tempPos = new float[3];
            String orientation;

            // if the transform was scanner anatomical, set to AXIAL
            if (useSATransform) {
                imgOrient = FileInfoBase.AXIAL;
                axisOrient[0] = FileInfoBase.ORI_R2L_TYPE;
                axisOrient[1] = FileInfoBase.ORI_A2P_TYPE;
                axisOrient[2] = FileInfoBase.ORI_I2S_TYPE;
            }

            if (image.getFileInfo(0).getFileFormat() == FileUtility.DICOM) {
                FileInfoDicom oldDicomInfo = (FileInfoDicom) image.getFileInfo(0);
                FileDicomTagTable[] childTagTables = new FileDicomTagTable[resultImage.getExtents()[2] - 1];

                // first create all of the new file infos (reference and children) and fill them with tags from the old
                // file info.  some of these tag values will be overridden in the next loop
                for (int i = 0; i < resultImage.getExtents()[2]; i++) {

                    if (i == 0) {

                        // create a new reference file info
                        fileInfo[0] = new FileInfoDicom(oldDicomInfo.getFileName(), oldDicomInfo.getFileDirectory(),
                                                        oldDicomInfo.getFileFormat());
                        ((FileInfoDicom)fileInfo[0]).vr_type = oldDicomInfo.vr_type;
                    } else {

                        // all other slices are children of the first file info..
                        fileInfo[i] = new FileInfoDicom(oldDicomInfo.getFileName(), oldDicomInfo.getFileDirectory(),
                                                        oldDicomInfo.getFileFormat(), (FileInfoDicom) fileInfo[0]);
                        ((FileInfoDicom)fileInfo[i]).vr_type = oldDicomInfo.vr_type;
                        childTagTables[i - 1] = ((FileInfoDicom) fileInfo[i]).getTagTable();
                    }

                    if (image.getExtents()[2] > i) {

                        // more correct information for a Z-axis rotation, so copy the file info on a slice basis
                        ((FileInfoDicom) fileInfo[i]).getTagTable().importTags((FileInfoDicom) image.getFileInfo(i));
                    } else {

                        // not possible for other rotations because the z-dimension is different
                        ((FileInfoDicom) fileInfo[i]).getTagTable().importTags(oldDicomInfo);
                    }
                }

                ((FileInfoDicom) fileInfo[0]).getTagTable().attachChildTagTables(childTagTables);
            } else {

                for (int i = 0; i < resultImage.getExtents()[2]; i++) {

                    if (image.getExtents()[2] > i) {
                        fileInfo[i] = (FileInfoBase) image.getFileInfo(i).clone();
                    } else {
                        fileInfo[i] = (FileInfoBase) image.getFileInfo(0).clone();
                    }
                }
            }

            for (int i = 0; i < resultImage.getExtents()[2]; i++) {

                fileInfo[i].setDataType(resultImage.getType());
                fileInfo[i].setResolutions(resolutions);
                fileInfo[i].setSliceThickness(resolutions[2]);
                fileInfo[i].setExtents(resultImage.getExtents());
                fileInfo[i].setMax(resultImage.getMax());
                fileInfo[i].setMin(resultImage.getMin());
                fileInfo[i].setImageOrientation(imgOrient);
                fileInfo[i].setAxisOrientation(axisOrient);
                fileInfo[i].setUnitsOfMeasure(units);

                // not sure why this was here, setting the origin per slice...should be the same
                // imgOrigin[2] = startPos + (direct[2] * i * resolutions[2]);
                fileInfo[i].setOrigin(imgOrigin);

                if (fileInfo[i].getFileFormat() == FileUtility.DICOM) {
                    orientation = (String) ((FileInfoDicom) fileInfo[i]).getTagTable().getValue("0020,0032");

                    if (orientation != null) {

                        int index1 = -1, index2 = -1;

                        for (int k = 0; k < orientation.length(); k++) {

                            if (orientation.charAt(k) == '\\') {

                                if (index1 == -1) {
                                    index1 = k;
                                } else {
                                    index2 = k;
                                }
                            }
                        }

                        coord[0] = Float.valueOf(orientation.substring(0, index1)).floatValue();
                        coord[1] = Float.valueOf(orientation.substring(index1 + 1, index2)).floatValue();
                        coord[2] = Float.valueOf(orientation.substring(index2 + 1)).floatValue();

                        matrix.transform(coord[0], coord[1], coord[2], tempPos);

                        // System.err.println("transformed " + orientation + " to: " +tempPos[0] + " " + tempPos[1] + "
                        // " + tempPos[2]);
                        orientation = tempPos[0] + "\\" + tempPos[1] + "\\" + tempPos[2];
                        ((FileInfoDicom) fileInfo[i]).getTagTable().setValue("0020,0032", orientation);
                    }
                }
            }
        } else if (resultImage.getNDims() == 4) {

            for (int i = 0; i < (resultImage.getExtents()[2] * resultImage.getExtents()[3]); i++) {
                fileInfo[i].setModality(image.getFileInfo()[0].getModality());
                fileInfo[i].setFileDirectory(image.getFileInfo()[0].getFileDirectory());
                fileInfo[i].setDataType(resultImage.getType());
                fileInfo[i].setEndianess(image.getFileInfo()[0].getEndianess());
                fileInfo[i].setUnitsOfMeasure(image.getFileInfo()[0].getUnitsOfMeasure());
                fileInfo[i].setResolutions(resolutions);
                fileInfo[i].setSliceThickness(resolutions[2]);
                fileInfo[i].setExtents(resultImage.getExtents());
                fileInfo[i].setMax(resultImage.getMax());
                fileInfo[i].setMin(resultImage.getMin());
                fileInfo[i].setImageOrientation(imgOrient);
                fileInfo[i].setAxisOrientation(axisOrient);
                imgOrigin[2] = startPos + (direct[2] * i * resolutions[2]);
                fileInfo[i].setOrigin(imgOrigin);
                fileInfo[i].setPixelPadValue(image.getFileInfo()[0].getPixelPadValue());
                fileInfo[i].setPhotometric(image.getFileInfo()[0].getPhotometric());
            }
        }

        resultImage.setFileInfo(fileInfo);
    }

    /**
     * Update origin.
     *
     * @param  xfrm  DOCUMENT ME!
     */
    private static void updateOrigin(TransMatrix xfrm) {

        if (xfrm.getDim() == 3) {
            float[] tempOrigin = new float[2];

            xfrm.transform(imgOrigin[0], imgOrigin[1], tempOrigin);
            imgOrigin[0] = tempOrigin[0];
            imgOrigin[1] = tempOrigin[1];
        } else {
            float[] tempOrigin = new float[3];

            xfrm.transform(imgOrigin[0], imgOrigin[1], imgOrigin[2], tempOrigin);
            imgOrigin[0] = tempOrigin[0];
            imgOrigin[1] = tempOrigin[1];
            imgOrigin[2] = tempOrigin[2];
        }
    }

    /**
     * Creates buffer for new image, prepares transformation matrix, and calls transform function for interpolation
     * specified.
     */
    private void transform() {

        // uses inverse transform to transform backwards
        TransMatrix xfrm = null;
        TransMatrix trans;
        TransMatrix xfrmC;

        if ((DIM >= 3) && (!do25D)) {
            imgLength = iXdim * iYdim * iZdim;
        } else {
            imgLength = iXdim * iYdim;
        }

        try {

            if ((do25D) || (DIM == 2)) {
                trans = new TransMatrix(3);
            } else { // (DIM >= 3) && (!do25D)
                trans = new TransMatrix(4);
            }

            trans.Copy(transMatrix);

            if ((doCenter) && (!haveCentered)) {

                if ((do25D) || (DIM == 2)) {
                    xfrmC = new TransMatrix(3);
                } else { // (DIM >= 3) && (!do25D)
                    xfrmC = new TransMatrix(4);
                }

                // by default: xfrmC.identity();

                if ((DIM >= 3) && (!do25D)) {
                    xfrmC.setTranslate(center.X, center.Y, center.Z);
                } else { // (DIM == 2) || do25D
                    xfrmC.setTranslate(center.X, center.Y);
                }

                trans.Copy(xfrmC);
                trans.Mult(transMatrix);


                if ((DIM >= 3) && (!do25D)) {
                    trans.setTranslate(-center.X, -center.Y, -center.Z);
                } else { // (DIM == 2) || do25D
                    trans.setTranslate(-center.X, -center.Y);
                }
            } // if ((doCenter) && (!haveCentered))

            xfrm = matrixtoInverseArray(trans);

            bufferFactor = 1;

            if (srcImage.isColorImage()) {
                bufferFactor = 4;
                imgLength = imgLength * 4;
            }

            imgBuf = new float[imgLength];
            srcImage.exportData(0, imgLength, imgBuf);

            if (bufferFactor == 4) {

                if ((DIM >= 3) && (!do25D)) {
                    imgBuf2 = new float[4 * oXdim * oYdim * oZdim];
                } else {
                    imgBuf2 = new float[4 * oXdim * oYdim];
                }
            }

            fireProgressStateChanged(srcImage.getImageName(), "Transforming image ...");

        } catch (IOException error) {
            displayError("Algorithm Transform: IOException on srcImage.exportData");
            setCompleted(false);

            disposeLocal();

            return;
        } catch (OutOfMemoryError e) {
            disposeLocal();
            System.gc();
            displayError("Algorithm Transform: ZZZ. Out of memory on srcImage.exportData");
            setCompleted(false);


            return;
        }

        if (bufferFactor == 1) { // black and white

            if ((do25D) && (DIM == 4)) {

                if (interp == NEAREST_NEIGHBOR) {
                    transformNearestNeighbor4Dim2D(imgBuf, xfrm);
                } else if (interp == BILINEAR) {
                    transformBilinear4D(imgBuf, xfrm);
                } else if (interp == BSPLINE3) {
                    transformBspline2D(imgBuf, xfrm, 3);
                } else if (interp == BSPLINE4) {
                    transformBspline2D(imgBuf, xfrm, 4);
                } else if (interp == CUBIC_LAGRANGIAN) {
                    transformCubicLagrangian4Dim2D(imgBuf, xfrm, clip);
                } else if (interp == QUINTIC_LAGRANGIAN) {
                    transformQuinticLagrangian4Dim2D(imgBuf, xfrm, clip);
                } else if (interp == HEPTIC_LAGRANGIAN) {
                    transformHepticLagrangian4Dim2D(imgBuf, xfrm, clip);
                } else if (interp == WSINC) {
                    transformWSinc4Dim2D(imgBuf, xfrm, clip);
                }
            } else if ((do25D) && (DIM == 3)) {

                if (interp == NEAREST_NEIGHBOR) {
                    transformNearestNeighbor3Dim2D(imgBuf, xfrm);
                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == BILINEAR) {
                    transformBilinear3D(imgBuf, xfrm);
                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == BSPLINE3) {
                    transformBspline2D(imgBuf, xfrm, 3);
                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == BSPLINE4) {
                    transformBspline2D(imgBuf, xfrm, 4);
                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == CUBIC_LAGRANGIAN) {
                    transformCubicLagrangian3Dim2D(imgBuf, xfrm, clip);
                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == QUINTIC_LAGRANGIAN) {
                    transformQuinticLagrangian3Dim2D(imgBuf, xfrm, clip);
                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == HEPTIC_LAGRANGIAN) {
                    transformHepticLagrangian3Dim2D(imgBuf, xfrm, clip);
                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == WSINC) {
                    transformWSinc3Dim2D(imgBuf, xfrm, clip);
                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                }
            } else if (DIM == 4) {

                if (interp == TRILINEAR) {
                    transformTrilinear4D(imgBuf, xfrm);
                } else if (interp == BSPLINE3) {
                    transformBspline4D(imgBuf, xfrm, 3);
                } else if (interp == BSPLINE4) {
                    transformBspline4D(imgBuf, xfrm, 4);
                } else if (interp == NEAREST_NEIGHBOR) {
                    transformNearestNeighbor4D(imgBuf, xfrm);
                } else if (interp == CUBIC_LAGRANGIAN) {
                    transformCubicLagrangian4D(imgBuf, xfrm, clip);
                } else if (interp == QUINTIC_LAGRANGIAN) {
                    transformQuinticLagrangian4D(imgBuf, xfrm, clip);
                } else if (interp == HEPTIC_LAGRANGIAN) {
                    transformHepticLagrangian4D(imgBuf, xfrm, clip);
                } else if (interp == WSINC) {
                    transformWSinc4D(imgBuf, xfrm, clip);
                } else if (interp == BILINEAR) {
                    displayError("Cannot select bilinear interpolation for 4D");

                    return;
                }
            } else if (DIM == 3) {

                if (interp == TRILINEAR) {
                    transformTrilinear(imgBuf, xfrm);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform3DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == BSPLINE3) {
                    transformBspline3D(imgBuf, xfrm, 3);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform3DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == BSPLINE4) {
                    transformBspline3D(imgBuf, xfrm, 4);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform3DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == NEAREST_NEIGHBOR) {
                    transformNearestNeighbor3D(imgBuf, xfrm);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform3DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == CUBIC_LAGRANGIAN) {
                    transformCubicLagrangian3D(imgBuf, xfrm, clip);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform3DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == QUINTIC_LAGRANGIAN) {
                    transformQuinticLagrangian3D(imgBuf, xfrm, clip);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform3DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == HEPTIC_LAGRANGIAN) {
                    transformHepticLagrangian3D(imgBuf, xfrm, clip);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform3DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == WSINC) {
                    transformWSinc3D(imgBuf, xfrm, clip);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform3DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == BILINEAR) {
                    displayError("Cannot select bilinear interpolation for 3D");

                    return;
                }
            } else if (DIM == 2) {

                if (interp == BILINEAR) {
                    transformBilinear(imgBuf, xfrm);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == NEAREST_NEIGHBOR) {
                    transformNearestNeighbor2D(imgBuf, xfrm);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == BSPLINE3) {
                    transformBspline2D(imgBuf, xfrm, 3);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == BSPLINE4) {
                    transformBspline2D(imgBuf, xfrm, 4);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == CUBIC_LAGRANGIAN) {
                    transformCubicLagrangian2D(imgBuf, xfrm, clip);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == QUINTIC_LAGRANGIAN) {
                    transformQuinticLagrangian2D(imgBuf, xfrm, clip);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == HEPTIC_LAGRANGIAN) {
                    transformHepticLagrangian2D(imgBuf, xfrm, clip);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == WSINC) {
                    transformWSinc2D(imgBuf, xfrm, clip);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == TRILINEAR) {
                    displayError("Cannot specify trilinear interpolation for 2D");

                    return;
                }
            }
        } // end of if (bufferFactor == 1)
        else { // color with bufferFactor == 4

            if ((do25D) && (DIM == 4)) {

                if (interp == NEAREST_NEIGHBOR) {
                    transformNearestNeighbor4Dim2DC(imgBuf, imgBuf2, xfrm);
                } else if (interp == BILINEAR) {
                    transformBilinear4DC(imgBuf, imgBuf2, xfrm);
                } else if (interp == BSPLINE3) {
                    transformBspline2DC(imgBuf, xfrm, 3);
                } else if (interp == BSPLINE4) {
                    transformBspline2DC(imgBuf, xfrm, 4);
                } else if (interp == CUBIC_LAGRANGIAN) {
                    transformCubicLagrangian4Dim2DC(imgBuf, imgBuf2, xfrm, clip);
                } else if (interp == QUINTIC_LAGRANGIAN) {
                    transformQuinticLagrangian4Dim2DC(imgBuf, imgBuf2, xfrm, clip);
                } else if (interp == HEPTIC_LAGRANGIAN) {
                    transformHepticLagrangian4Dim2DC(imgBuf, imgBuf2, xfrm, clip);
                } else if (interp == WSINC) {
                    transformWSinc4Dim2DC(imgBuf, imgBuf2, xfrm, clip);
                }
            } else if ((do25D) && (DIM == 3)) {

                if (interp == NEAREST_NEIGHBOR) {
                    transformNearestNeighbor3Dim2DC(imgBuf, imgBuf2, xfrm);
                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                }

                else if (interp == BILINEAR) {
                    transformBilinear3DC(imgBuf, imgBuf2, xfrm);
                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == BSPLINE3) {
                    transformBspline2DC(imgBuf, xfrm, 3);
                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == BSPLINE4) {
                    transformBspline2DC(imgBuf, xfrm, 4);
                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == CUBIC_LAGRANGIAN) {
                    transformCubicLagrangian3Dim2DC(imgBuf, imgBuf2, xfrm, clip);
                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == QUINTIC_LAGRANGIAN) {
                    transformQuinticLagrangian3Dim2DC(imgBuf, imgBuf2, xfrm, clip);
                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == HEPTIC_LAGRANGIAN) {
                    transformHepticLagrangian3Dim2DC(imgBuf, imgBuf2, xfrm, clip);
                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == WSINC) {
                    transformWSinc3Dim2DC(imgBuf, imgBuf2, xfrm, clip);
                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                }
            } else if (DIM == 4) {
                if (interp == TRILINEAR) {
                    transformTrilinear4DC(imgBuf, imgBuf2, xfrm);
                } else if (interp == BSPLINE3) {
                    transformBspline4DC(imgBuf, xfrm, 3);
                } else if (interp == BSPLINE4) {
                    transformBspline4DC(imgBuf, xfrm, 4);
                } else if (interp == NEAREST_NEIGHBOR) {
                    transformNearestNeighbor4DC(imgBuf, imgBuf2, xfrm);
                } else if (interp == CUBIC_LAGRANGIAN) {
                    transformCubicLagrangian4DC(imgBuf, imgBuf2, xfrm, clip);
                } else if (interp == QUINTIC_LAGRANGIAN) {
                    transformQuinticLagrangian4DC(imgBuf, imgBuf2, xfrm, clip);
                } else if (interp == HEPTIC_LAGRANGIAN) {
                    transformHepticLagrangian4DC(imgBuf, imgBuf2, xfrm, clip);
                } else if (interp == WSINC) {
                    transformWSinc4DC(imgBuf, imgBuf2, xfrm, clip);
                } else if (interp == BILINEAR) {
                    displayError("Cannot specify bilinear interpolation for 4D");

                    return;
                }
            } else if (DIM == 3) {

                if (interp == TRILINEAR) {
                    transformTrilinearC(imgBuf, imgBuf2, xfrm);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform3DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == BSPLINE3) {
                    transformBspline3DC(imgBuf, xfrm, 3);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform3DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == BSPLINE4) {
                    transformBspline3DC(imgBuf, xfrm, 4);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform3DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == NEAREST_NEIGHBOR) {
                    transformNearestNeighbor3DC(imgBuf, imgBuf2, xfrm);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform3DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == CUBIC_LAGRANGIAN) {
                    transformCubicLagrangian3DC(imgBuf, imgBuf2, xfrm, clip);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform3DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == QUINTIC_LAGRANGIAN) {
                    transformQuinticLagrangian3DC(imgBuf, imgBuf2, xfrm, clip);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform3DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == HEPTIC_LAGRANGIAN) {
                    transformHepticLagrangian3DC(imgBuf, imgBuf2, xfrm, clip);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform3DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == WSINC) {
                    transformWSinc3DC(imgBuf, imgBuf2, xfrm, clip);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform3DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == BILINEAR) {
                    displayError("Cannot specify bilinear interpolation for 3D");

                    return;
                }
            } else if (DIM == 2) {

                if (interp == BILINEAR) {
                    transformBilinearC(imgBuf, imgBuf2, xfrm);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == NEAREST_NEIGHBOR) {
                    transformNearestNeighbor2DC(imgBuf, imgBuf2, xfrm);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == BSPLINE3) {
                    transformBspline2DC(imgBuf, xfrm, 3);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == BSPLINE4) {
                    transformBspline2DC(imgBuf, xfrm, 4);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == CUBIC_LAGRANGIAN) {
                    transformCubicLagrangian2DC(imgBuf, imgBuf2, xfrm, clip);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == QUINTIC_LAGRANGIAN) {
                    transformQuinticLagrangian2DC(imgBuf, imgBuf2, xfrm, clip);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == HEPTIC_LAGRANGIAN) {
                    transformHepticLagrangian2DC(imgBuf, imgBuf2, xfrm, clip);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == WSINC) {
                    transformWSinc2DC(imgBuf, imgBuf2, xfrm, clip);

                    if ((transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == TRILINEAR) {
                    displayError("Cannot specify trilinear interpolation for 2D");

                    return;
                }
            }
        } // end of else for color

        if (threadStopped) {
            destImage.disposeLocal();
            finalize();

            return;
        }

        setCompleted(true);

    }

    /**
     * Transforms and resamples a 2D VOI using nearest neighbor interpolation.
     *
     * <ol>
     *   <li>Export VOIs as a mask image</li>
     *   <li>Transform mask</li>
     *   <li>Extract VOI contours from mask image and put in new image.</li>
     * </ol>
     *
     * @param  image      Image where VOIs are stored
     * @param  imgBuffer  Image array
     * @param  xfrm       Transformation matrix to be applied
     */
    private void transform2DVOI(ModelImage image, float[] imgBuffer, TransMatrix kTM) {

        int i, j;
        int iAdj, jAdj;
        int X0pos, Y0pos;
        float X, Y;
        float temp1, temp2;
        float value;
        float imm, jmm;
        int roundX, roundY;

        float T00, T01, T02, T10, T11, T12;
        ModelImage tmpMask;

        int mod = Math.max(1, oXdim / 50);

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        maskImage = image.generateShortImage(1);
        tmpMask = new ModelImage(ModelImage.SHORT, destImage.getExtents(), null);

        try {
            maskImage.exportData(0, iXdim * iYdim, imgBuffer); // locks and releases lock
        } catch (IOException error) {
            displayError("Algorithm VOI transform: Image(s) locked");
            setCompleted(false);

            return;
        }

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged((int) (((float) i / oXdim * 100) + .5));
            }

            if (pad) {
                iAdj = i - margins[0];
            } else {
                iAdj = i;
            }

            imm = iAdj * oXres;
            temp1 = (imm * T00) + T02;
            temp2 = (imm * T10) + T12;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                // transform i,j
                if (pad) {
                    jAdj = j - margins[1];
                } else {
                    jAdj = j;
                }

                jmm = jAdj * oYres;
                value = 0.0f; // remains zero if voxel is transformed out of bounds
                X = (temp1 + (jmm * T01)) / iXres;
                roundX = (int) (X + 0.5f);

                if ((X >= -0.5f) && (X < iXdim)) {
                    Y = (temp2 + (jmm * T11)) / iYres;
                    roundY = (int) (Y + 0.5f);

                    if ((Y >= -0.5f) && (Y < iYdim)) {
                        X0pos = Math.min(roundX, iXdim - 1);
                        Y0pos = Math.min(roundY, iYdim - 1) * iXdim;
                        value = imgBuffer[Y0pos + X0pos];
                    } // end if Y in bounds
                } // end if X in bounds

                tmpMask.set(i, j, value);
            } // end for j
        } // end for i

        if (threadStopped) {
            return;
        }

        // ******* Make algorithm for VOI extraction.
        tmpMask.calcMinMax();

        AlgorithmVOIExtraction VOIExtAlgo = new AlgorithmVOIExtraction(tmpMask);

        VOIExtAlgo.setRunningInSeparateThread(runningInSeparateThread);
        VOIExtAlgo.run();
        destImage.setVOIs(tmpMask.getVOIs());
        tmpMask.disposeLocal();
        maskImage.disposeLocal();
    }
    
    /**
     * Transforms and resamples a 3D VOI using nearest neighbor interpolation.
     *
     * <ol>
     *   <li>Export VOIs as a mask image</li>
     *   <li>Transform mask</li>
     *   <li>Extract VOI contours from mask image and put in new image.</li>
     * </ol>
     *
     * @param  image      Image where VOIs are stored
     * @param  imgBuffer  Image array
     * @param  kTM       Transformation matrix to be applied
     */
    private void transform25DVOI(ModelImage image, float[] imgBuffer, TransMatrix kTM) {

        int i, j;
        int iAdj, jAdj;
        int X0pos, Y0pos;
        float X, Y;
        float temp1, temp2;
        float value;
        float imm, jmm;
        int roundX, roundY;
        int z;

        float T00, T01, T02, T10, T11, T12;
        ModelImage tmpMask;

        int mod = Math.max(1, iZdim / 50);

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        int sliceSize = iXdim * iYdim;

        maskImage = image.generateShortImage(1);
        tmpMask = new ModelImage(ModelImage.SHORT, destImage.getExtents(), null);
        
        for (z = 0; z < iZdim; z++) {
            if (((z % mod) == 0)) {
                fireProgressStateChanged((int) (((float) z / iZdim * 100) + .5));
            }
            try {
                maskImage.exportData(z * sliceSize, sliceSize, imgBuffer); // locks and releases lock
            } catch (IOException error) {
                displayError("Algorithm VOI transform: Image(s) locked");
                setCompleted(false);
    
                return;
            }
    
            for (i = 0; (i < oXdim) && !threadStopped; i++) {
    
                if (pad) {
                    iAdj = i - margins[0];
                } else {
                    iAdj = i;
                }
    
                imm = iAdj * oXres;
                temp1 = (imm * T00) + T02;
                temp2 = (imm * T10) + T12;
    
                for (j = 0; (j < oYdim) && !threadStopped; j++) {
    
                    // transform i,j
                    if (pad) {
                        jAdj = j - margins[1];
                    } else {
                        jAdj = j;
                    }
    
                    jmm = jAdj * oYres;
                    value = 0.0f; // remains zero if voxel is transformed out of bounds
                    X = (temp1 + (jmm * T01)) / iXres;
                    roundX = (int) (X + 0.5f);
    
                    if ((X >= -0.5f) && (X < iXdim)) {
                        Y = (temp2 + (jmm * T11)) / iYres;
                        roundY = (int) (Y + 0.5f);
    
                        if ((Y >= -0.5f) && (Y < iYdim)) {
                            X0pos = Math.min(roundX, iXdim - 1);
                            Y0pos = Math.min(roundY, iYdim - 1) * iXdim;
                            value = imgBuffer[Y0pos + X0pos];
                        } // end if Y in bounds
                    } // end if X in bounds
    
                    tmpMask.set(i, j, z, value);
                } // end for j
            } // end for i
    
            if (threadStopped) {
                return;
            }
        } // for (z = 0; z < iZdim; z++)
        // ******* Make algorithm for VOI extraction.
        tmpMask.calcMinMax();
        
        AlgorithmVOIExtraction VOIExtAlgo = new AlgorithmVOIExtraction(tmpMask);

        VOIExtAlgo.setRunningInSeparateThread(runningInSeparateThread);
        VOIExtAlgo.run();
        destImage.setVOIs(tmpMask.getVOIs());
        tmpMask.disposeLocal();
        maskImage.disposeLocal();
    }

    /**
     * Transforms and resamples a 3D VOI using nearest neighbor interpolation.
     *
     * <ol>
     *   <li>Export VOIs as a mask image</li>
     *   <li>Transform mask</li>
     *   <li>Extract VOI contours from mask image and put in new image.</li>
     * </ol>
     *
     * @param  image      Image where VOIs are stored
     * @param  imgBuffer  Image array
     * @param  kTM       Transformation matrix to be applied
     */
    private void transform3DVOI(ModelImage image, float[] imgBuffer, TransMatrix kTM) {

        int i, j, k;
        int iAdj, jAdj, kAdj;
        int X0pos, Y0pos, Z0pos;
        float X, Y, Z;
        float value;
        int sliceSize;
        float imm, jmm, kmm;
        float k1, k2, k3, j1, j2, j3;

        sliceSize = iXdim * iYdim;

        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        ModelImage tmpMask;

        int mod = Math.max(1, oXdim / 50);

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;

        maskImage = image.generateShortImage(1);
        tmpMask = new ModelImage(ModelImage.SHORT, destImage.getExtents(), "VOI Mask");

        try {
            maskImage.exportData(0, iXdim * iYdim * iZdim, imgBuffer); // locks and releases lock
        } catch (IOException error) {
            displayError("Algorithm VOI transform: Image(s) locked");
            setCompleted(false);

            return;
        }

        float invXRes = 1 / iXres;
        float invYRes = 1 / iYres;
        float invZRes = 1 / iZres;

        for (k = 0; (k < oZdim) && !threadStopped; k++) {

            if (((k % mod) == 0)) {
                fireProgressStateChanged((int) (((float) k / oZdim * 100) + 0.5f));
            }

            if (pad) {
                kAdj = k - margins[2];
            } else {
                kAdj = k;
            }

            kmm = kAdj * oZres;
            k1 = (kmm * T02) + T03;
            k2 = (kmm * T12) + T13;
            k3 = (kmm * T22) + T23;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                if (pad) {
                    jAdj = j - margins[1];
                } else {
                    jAdj = j;
                }

                jmm = jAdj * oYres;
                j1 = (jmm * T01) + k1;
                j2 = (jmm * T11) + k2;
                j3 = (jmm * T21) + k3;

                for (i = 0; (i < oXdim) && !threadStopped; i++) {

                    // transform i,j,k
                    if (pad) {
                        iAdj = i - margins[0];
                    } else {
                        iAdj = i;
                    }

                    imm = iAdj * oXres;
                    value = 0.0f; // remains zero if voxel is transformed out of bounds
                    X = (j1 + (imm * T00)) * invXRes;

                    if ((X >= -0.5) && (X < iXdim)) {
                        Y = (j2 + (imm * T10)) * invYRes;

                        if ((Y >= -0.5) && (Y < iYdim)) {
                            Z = (j3 + (imm * T20)) * invZRes;

                            if ((Z >= -0.5) && (Z < iZdim)) {
                                X0pos = Math.min((int) (X + 0.5f), iXdim - 1);
                                Y0pos = Math.min((int) (Y + 0.5f), iYdim - 1) * iXdim;
                                Z0pos = Math.min((int) (Z + 0.5f), iZdim - 1) * sliceSize;
                                value = imgBuffer[Z0pos + Y0pos + X0pos];
                            } // end if Z in bounds
                        } // end if Y in bounds
                    } // end if X in bounds

                    tmpMask.set(i, j, k, value);
                } // end for k
            } // end for j
        } // end for i

        if (threadStopped) {
            return;
        }

        // ******* Make algorithm for VOI extraction.
        tmpMask.calcMinMax();

        AlgorithmVOIExtraction VOIExtAlgo = new AlgorithmVOIExtraction(tmpMask);

        VOIExtAlgo.setRunningInSeparateThread(runningInSeparateThread);
        VOIExtAlgo.run();

        VOIVector resultVOIs = tmpMask.getVOIs();
        VOIVector srcVOIs = image.getVOIs();

        for (int ii = 0; ii < resultVOIs.size(); ii++) {
            int id = ((VOI) (resultVOIs.elementAt(ii))).getID();

            for (int jj = 0; jj < srcVOIs.size(); jj++) {

                if (((VOI) (srcVOIs.elementAt(jj))).getID() == id) {
                    ((VOI) (resultVOIs.elementAt(ii))).setName(((VOI) (srcVOIs.elementAt(jj))).getName());
                }
            }
        }

        destImage.setVOIs(tmpMask.getVOIs());
        tmpMask.disposeLocal();
        maskImage.disposeLocal();
    }

    /**
     * Transforms and resamples volume using bilinear interpolation.
     *
     * @param  imgBuf  Image array
     * @param  kTM    Matrix to be applied
     */
    private void transformBilinear(float[] imgBuf, TransMatrix kTM) {
        int i, j;
        int iAdj, jAdj;
        float X, Y;
        int x0, y0;
        float j1, j2;
        float value;
        float imm, jmm;
        int deltaX, deltaY;

        int mod = Math.max(1, oYdim / 25);

        float T00, T01, T02, T10, T11, T12;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        int position;
        float dx, dy, dx1, dy1;

        float invXRes = 1 / iXres;
        float invYRes = 1 / iYres;

        int index = 0;

        for (j = 0; (j < oYdim) && !threadStopped; j++) {

            if (((j % mod) == 0)) {
                fireProgressStateChanged((int) (((float) j / oYdim * 100) + 0.5f));
            }

            if (pad) {
                jAdj = j - margins[1];
            } else {
                jAdj = j;
            }

            jmm = jAdj * oYres;
            j1 = (jmm * T01) + T02;
            j2 = (jmm * T11) + T12;

            for (i = 0; (i < oXdim) && !threadStopped; i++) {

                // transform i,j,k
                if (pad) {
                    value = (float) padVal;
                    iAdj = i - margins[0];
                } else {
                    value = (float) srcImage.getMin(); // remains zero if voxel is transformed out of bounds
                    iAdj = i;
                }

                imm = iAdj * oXres;
                X = (j1 + (imm * T00)) * invXRes;

                if ((X > -0.5f) && (X < iXdim)) {
                    Y = (j2 + (imm * T10)) * invYRes;

                    if ((Y > -0.5f) && (Y < iYdim)) {

                        if (X <= 0) {
                            x0 = 0;
                            dx = 0;
                            deltaX = 0;
                        } else if (X >= (iXdim - 1)) {
                            x0 = iXdim - 1;
                            dx = 0;
                            deltaX = 0;
                        } else {
                            x0 = (int) X;
                            dx = X - x0;
                            deltaX = 1;
                        }

                        if (Y <= 0) {
                            y0 = 0;
                            dy = 0;
                            deltaY = 0;
                        } else if (Y >= (iYdim - 1)) {
                            y0 = iYdim - 1;
                            dy = 0;
                            deltaY = 0;
                        } else {
                            y0 = (int) Y;
                            dy = Y - y0;
                            deltaY = iXdim;
                        }

                        dx1 = 1 - dx;
                        dy1 = 1 - dy;

                        position = (y0 * iXdim) + x0;

                        value = (dy1 * ((dx1 * imgBuf[position]) + (dx * imgBuf[position + deltaX]))) +
                                (dy * ((dx1 * imgBuf[position + deltaY]) + (dx * imgBuf[position + deltaY + deltaX])));
                    } // end if Y in bounds
                } // end if X in bounds

                destImage.set(index++, value);
            } // end for i
        } // end for j
    }

    /**
     * Transforms and resamples volume using bilinear interpolation. Used as a slice-only algorithm on 3D images.
     *
     * @param  imgBuf  Image array
     * @param  kTM    Matrix to be applied
     */
    private void transformBilinear3D(float[] imgBuf, TransMatrix kTM) {
        int i, j, k;
        int iAdj, jAdj;
        int X0pos, Y0pos;
        int X1pos, Y1pos;
        float X, Y;
        float x0, y0;
        float x1, y1;
        float value;
        float imm, jmm;
        int roundX, roundY;
        float temp1, temp2;
        float T00, T01, T02, T10, T11, T12;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        for (k = 0; (k < oZdim) && !threadStopped; k++) {
            fireProgressStateChanged(Math.round((float) k / oZdim * 100));

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                if (pad){
                    iAdj = i - margins[0];
                }
                else {
                    iAdj = i;
                }
                imm = (float) iAdj * oXres;
                temp1 = (imm * T00) + T02;
                temp2 = (imm * T10) + T12;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {

                    // transform i,j
                    if (pad) {
                        jAdj = j - margins[1];
                        value = (float)padVal;
                    } 
                    else {
                        jAdj = j;
                        value = (float) srcImage.getMin(); // remains zero if voxel is transformed out of bounds
                    }
                    jmm = (float) jAdj * oYres;
                    X = (temp1 + (jmm * T01)) / iXres;
                    roundX = (int) (X + 0.5f);

                    if ((X >= 0) && (X < iXdim)) {
                        Y = (temp2 + (jmm * T11)) / iYres;
                        roundY = (int) (Y + 0.5f);

                        if ((Y >= 0) && (Y < iYdim)) {

                            if ((X >= (iXdim - 1)) || (Y >= (iYdim - 1))) { // cannot interpolate on last X
                                                                                      // or Y
                                X0pos = Math.min(roundX, iXdim - 1);
                                Y0pos = Math.min(roundY, iYdim - 1) * iXdim;
                                value = imgBuf[Y0pos + X0pos];
                            } else {

                                // set intensity of i,j,k to new transformed coordinate if
                                // x,y,z is w/in dimensions of image
                                x0 = X - (int) X;
                                y0 = Y - (int) Y;
                                x1 = 1 - x0;
                                y1 = 1 - y0;
                                X0pos = (int) X;
                                Y0pos = (int) Y * iXdim;
                                X1pos = X0pos + 1;
                                Y1pos = Y0pos + iXdim;
                                value = (x1 * y1 * imgBuf[Y0pos + X0pos]) + (x0 * y1 * imgBuf[Y0pos + X1pos]) +
                                        (x1 * y0 * imgBuf[Y1pos + X0pos]) + (x0 * y0 * imgBuf[Y1pos + X1pos]);
                            }
                        } // end if Y in bounds
                    } // end if X in bounds

                    destImage.set(i, j, k, value);
                } // end for j
            } // end for i

            if (threadStopped) {
                return;
            }

            if (k < (oZdim - 1)) {

                try {
                    srcImage.exportData((k + 1) * imgLength, imgLength, imgBuf);
                } catch (IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);


                    return;
                }
            } // end if (k < (oZdim - 1))
        } // end for k
    }

    /**
     * Transforms and resamples volume using bilinear interpolation (2.5D) This version used with color images. This
     * alogorithm used on a slice by slice basis on 3D images.
     *
     * @param  imgBuf   Input image array
     * @param  imgBuf2  Output image array
     * @param  kTM     Matrix to be applied
     */
    private void transformBilinear3DC(float[] imgBuf, float[] imgBuf2, TransMatrix kTM) {
        int i, j, k;
        int iAdj, jAdj;
        int X0pos, Y0pos;
        int X1pos, Y1pos;
        float X, Y;
        float x0, y0;
        float x1, y1;
        float imm, jmm;

        int mod = Math.max(1, oXdim / 50);

        int counter = 0; // used for progress bar
        int roundX, roundY;
        float x1y1, x0y1, x1y0, x0y0;
        float temp1, temp2;
        int temp3, temp4, temp5, temp6, temp7, temp8;
        float T00, T01, T02, T10, T11, T12;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        for (k = 0; (k < oZdim) && !threadStopped; k++) {

            if (((k % mod) == 0)) {
                fireProgressStateChanged((int) (((float) k / oZdim * 100) + 0.5));
            }

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                if (pad) {
                    iAdj = i - margins[0];
                }
                else {
                    iAdj = i;
                }
                imm = (float) iAdj * oXres;
                temp1 = (imm * T00) + T02;
                temp2 = (imm * T10) + T12;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {

                    // transform i,j
                    if (pad) {
                        jAdj = j - margins[1];
                        temp3 = 4 * (i + (j * oXdim));
                        imgBuf2[temp3] = padVal; // remains padVal if voxel is transformed out of bounds
                        imgBuf2[temp3 + 1] = padVal;
                        imgBuf2[temp3 + 2] = padVal;
                        imgBuf2[temp3 + 3] = padVal;
                    }
                    else {
                        jAdj = j;
                        temp3 = 4 * (i + (j * oXdim));
                        imgBuf2[temp3] = 0; // remains zero if voxel is transformed out of bounds
                        imgBuf2[temp3 + 1] = 0;
                        imgBuf2[temp3 + 2] = 0;
                        imgBuf2[temp3 + 3] = 0;
                    }
                    

                    jmm = (float) jAdj * oYres;
                    X = (temp1 + (jmm * T01)) / iXres;
                    roundX = (int) (X + 0.5f);

                    if ((X >= 0) && (X < iXdim)) {
                        Y = (temp2 + (jmm * T11)) / iYres;
                        roundY = (int) (Y + 0.5);

                        if ((Y >= 0) && (Y < iYdim)) {

                            if ((X >= (iXdim - 1)) || (Y >= (iYdim - 1))) { // cannot interpolate on last X
                                                                                      // or Y
                                X0pos = Math.min(roundX, iXdim - 1);
                                Y0pos = Math.min(roundY, iYdim - 1) * iXdim;
                                temp4 = 4 * (Y0pos + X0pos);
                                imgBuf2[4 * (i + (j * oXdim))] = imgBuf[temp4];
                                imgBuf2[(4 * (i + (j * oXdim))) + 1] = imgBuf[temp4 + 1];
                                imgBuf2[(4 * (i + (j * oXdim))) + 2] = imgBuf[temp4 + 2];
                                imgBuf2[(4 * (i + (j * oXdim))) + 3] = imgBuf[temp4 + 3];
                            } else {

                                // set intensity of i,j,k to new transformed coordinate if
                                // x,y,z is w/in dimensions of image
                                x0 = X - (int) X;
                                y0 = Y - (int) Y;
                                x1 = 1 - x0;
                                y1 = 1 - y0;
                                X0pos = (int) X;
                                Y0pos = (int) Y * iXdim;
                                X1pos = X0pos + 1;
                                Y1pos = Y0pos + iXdim;
                                temp5 = 4 * (Y0pos + X0pos);
                                temp6 = 4 * (Y0pos + X1pos);
                                temp7 = 4 * (Y1pos + X0pos);
                                temp8 = 4 * (Y1pos + X1pos);
                                x1y1 = x1 * y1;
                                x0y1 = x0 * y1;
                                x1y0 = x1 * y0;
                                x0y0 = x0 * y0;
                                imgBuf2[temp3] = (x1y1 * imgBuf[temp5]) + (x0y1 * imgBuf[temp6]) +
                                                 (x1y0 * imgBuf[temp7]) + (x0y0 * imgBuf[temp8]);
                                imgBuf2[temp3 + 1] = (x1y1 * imgBuf[temp5 + 1]) + (x0y1 * imgBuf[temp6 + 1]) +
                                                     (x1y0 * imgBuf[temp7 + 1]) + (x0y0 * imgBuf[temp8 + 1]);
                                imgBuf2[temp3 + 2] = (x1y1 * imgBuf[temp5 + 2]) + (x0y1 * imgBuf[temp6 + 2]) +
                                                     (x1y0 * imgBuf[temp7 + 2]) + (x0y0 * imgBuf[temp8 + 2]);
                                imgBuf2[temp3 + 3] = (x1y1 * imgBuf[temp5 + 3]) + (x0y1 * imgBuf[temp6 + 3]) +
                                                     (x1y0 * imgBuf[temp7 + 3]) + (x0y0 * imgBuf[temp8 + 3]);
                            }
                        } // end if Y in bounds
                    } // end if X in bounds

                    counter++;
                } // end for j
            } // end for i

            try {
                destImage.importData(4 * k * oXdim * oYdim, imgBuf2, true);
            } catch (IOException error) {
                MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
            }

            if (k < (oZdim - 1)) {

                try {
                    srcImage.exportData((k + 1) * imgLength, imgLength, imgBuf);
                } catch (IOException error) {
                    displayError("Algorithm Transform: IOException Error on exportData");
                    setCompleted(false);


                    return;
                }
            } // end if (k < (oZdim - 1))
        } // end for k
    }

    /**
     * Transforms and resamples volume using bilinear interpolation (2.5D) Used as a slice only algorithm on 3D images.
     *
     * @param  imgBuf  Image array
     * @param  kTM    Transformation matrix to be applied
     */
    private void transformBilinear4D(float[] imgBuf, TransMatrix kTM) {
        int i, j, k, l;
        int X0pos, Y0pos;
        int X1pos, Y1pos;
        float X, Y;
        float x0, y0;
        float x1, y1;
        float value;
        float imm, jmm;
        int counter = 0; // used for progress bar
        int roundX, roundY;
        float temp1, temp2;
        float T00, T01, T02, T10, T11, T12;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged((int) (((float) l / oTdim * 100) + 0.5));

            for (k = 0; (k < oZdim) && !threadStopped; k++) {

                for (i = 0; (i < oXdim) && !threadStopped; i++) {
                    imm = (float) i * oXres;
                    temp1 = (imm * T00) + T02;
                    temp2 = (imm * T10) + T12;

                    for (j = 0; (j < oYdim) && !threadStopped; j++) {

                        // transform i,j
                        value = (float) srcImage.getMin(); // remains zero if voxel is transformed out of bounds
                        jmm = (float) j * oYres;
                        X = (temp1 + (jmm * T01)) / iXres;
                        roundX = (int) (X + 0.5f);

                        if ((X >= 0) && (X < iXdim)) {
                            Y = (temp2 + (jmm * T11)) / iYres;
                            roundY = (int) (Y + 0.5f);

                            if ((Y >= 0) && (Y < iYdim)) {

                                if ((X >= (iXdim - 1)) || (Y >= (iYdim - 1))) { // cannot interpolate on last
                                                                                          // X or Y
                                    X0pos = Math.min(roundX, iXdim - 1);
                                    Y0pos = Math.min(roundY, iYdim - 1) * iXdim;
                                    value = imgBuf[Y0pos + X0pos];
                                } else {

                                    // set intensity of i,j,k to new transformed coordinate if
                                    // x,y,z is w/in dimensions of image
                                    x0 = X - (int) X;
                                    y0 = Y - (int) Y;
                                    x1 = 1 - x0;
                                    y1 = 1 - y0;
                                    X0pos = (int) X;
                                    Y0pos = (int) Y * iXdim;
                                    X1pos = X0pos + 1;
                                    Y1pos = Y0pos + iXdim;
                                    value = (x1 * y1 * imgBuf[Y0pos + X0pos]) + (x0 * y1 * imgBuf[Y0pos + X1pos]) +
                                            (x1 * y0 * imgBuf[Y1pos + X0pos]) + (x0 * y0 * imgBuf[Y1pos + X1pos]);
                                }
                            } // end if Y in bounds
                        } // end if X in bounds

                        destImage.set(i, j, k, l, value);
                        counter++;
                    } // end for j
                } // end for i

                if ((k < (oZdim - 1)) || (l < (oTdim - 1))) {

                    try {
                        srcImage.exportData((l * oZdim * imgLength) + ((k + 1) * imgLength), imgLength, imgBuf);
                    } catch (IOException error) {
                        displayError("Algorithm Transform: Image(s) locked");
                        setCompleted(false);


                        return;
                    }
                } // end if ((k < (oZdim - 1))|| (l < (oTdim - 1)))
            } // end for k
        } // end for l
    }

    /**
     * Transforms and resamples volume using bilinear interpolation This version used with color images This alogorithm
     * used on a slice by slice basis on 4D images.
     *
     * @param  imgBuf   Input image array
     * @param  imgBuf2  Output image array
     * @param  kTM     Matrix to be applied
     */
    private void transformBilinear4DC(float[] imgBuf, float[] imgBuf2, TransMatrix kTM) {
        int i, j, k, l;
        int X0pos, Y0pos;
        int X1pos, Y1pos;
        float X, Y;
        float x0, y0;
        float x1, y1;
        float imm, jmm;
        int counter = 0; // used for progress bar
        int roundX, roundY;
        float x1y1, x0y1, x1y0, x0y0;
        float temp1, temp2;
        int temp3, temp4, temp5, temp6, temp7, temp8;
        float T00, T01, T02, T10, T11, T12;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged((int) (((float) l / oTdim * 100) + 0.5));

            for (k = 0; (k < oZdim) && !threadStopped; k++) {

                for (i = 0; (i < oXdim) && !threadStopped; i++) {
                    imm = (float) i * oXres;
                    temp1 = (imm * T00) + T02;
                    temp2 = (imm * T10) + T12;

                    for (j = 0; (j < oYdim) && !threadStopped; j++) {

                        // transform i,j
                        temp3 = 4 * (i + (j * oXdim));
                        imgBuf2[temp3] = 0; // remains zero if voxel is transformed out of bounds
                        imgBuf2[temp3 + 1] = 0;
                        imgBuf2[temp3 + 2] = 0;
                        imgBuf2[temp3 + 3] = 0;

                        jmm = (float) j * oYres;
                        X = (temp1 + (jmm * T01)) / iXres;
                        roundX = (int) (X + 0.5f);

                        if ((X >= 0) && (X < iXdim)) {
                            Y = (temp2 + (jmm * T11)) / iYres;
                            roundY = (int) (Y + 0.5f);

                            if ((Y >= 0) && (Y < iYdim)) {

                                if ((X >= (iXdim - 1)) || (Y >= (iYdim - 1))) { // cannot interpolate on last
                                                                                          // X or Y
                                    X0pos = Math.min(roundX, iXdim - 1);
                                    Y0pos = Math.min(roundY, iYdim - 1) * iXdim;
                                    temp4 = 4 * (Y0pos + X0pos);
                                    imgBuf2[temp3] = imgBuf[temp4];
                                    imgBuf2[temp3 + 1] = imgBuf[temp4 + 1];
                                    imgBuf2[temp3 + 2] = imgBuf[temp4 + 2];
                                    imgBuf2[temp3 + 3] = imgBuf[temp4 + 3];
                                } else {

                                    // set intensity of i,j,k to new transformed coordinate if
                                    // x,y,z is w/in dimensions of image
                                    x0 = X - (int) X;
                                    y0 = Y - (int) Y;
                                    x1 = 1 - x0;
                                    y1 = 1 - y0;
                                    X0pos = (int) X;
                                    Y0pos = (int) Y * iXdim;
                                    X1pos = X0pos + 1;
                                    Y1pos = Y0pos + iXdim;
                                    temp5 = 4 * (Y0pos + X0pos);
                                    temp6 = 4 * (Y0pos + X1pos);
                                    temp7 = 4 * (Y1pos + X0pos);
                                    temp8 = 4 * (Y1pos + X1pos);
                                    x1y1 = x1 * y1;
                                    x0y1 = x0 * y1;
                                    x1y0 = x1 * y0;
                                    x0y0 = x0 * y0;
                                    imgBuf2[temp3] = (x1y1 * imgBuf[temp5]) + (x0y1 * imgBuf[temp6]) +
                                                     (x1y0 * imgBuf[temp7]) + (x0y0 * imgBuf[temp8]);
                                    imgBuf2[temp3 + 1] = (x1y1 * imgBuf[temp5 + 1]) + (x0y1 * imgBuf[temp6 + 1]) +
                                                         (x1y0 * imgBuf[temp7 + 1]) + (x0y0 * imgBuf[temp8 + 1]);
                                    imgBuf2[temp3 + 2] = (x1y1 * imgBuf[temp5 + 2]) + (x0y1 * imgBuf[temp6 + 2]) +
                                                         (x1y0 * imgBuf[temp7 + 2]) + (x0y0 * imgBuf[temp8 + 2]);
                                    imgBuf2[temp3 + 3] = (x1y1 * imgBuf[temp5 + 3]) + (x0y1 * imgBuf[temp6 + 3]) +
                                                         (x1y0 * imgBuf[temp7 + 3]) + (x0y0 * imgBuf[temp8 + 3]);
                                }
                            } // end if Y in bounds
                        } // end if X in bounds

                        counter++;
                    } // end for j
                } // end for i

                try {
                    destImage.importData((4 * l * oXdim * oYdim * oZdim) + (4 * k * oXdim * oYdim), imgBuf2, true);
                } catch (IOException error) {
                    MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
                }

                if ((k < (oZdim - 1)) || (l < (oTdim - 1))) {

                    try {
                        srcImage.exportData((l * oZdim * imgLength) + ((k + 1) * imgLength), imgLength, imgBuf);
                    } catch (IOException error) {
                        displayError("Algorithm Transform: IOException Error on exportData");
                        setCompleted(false);


                        return;
                    }
                } // end if ((k < (oZdim - 1))|| (l < (oTdim - 1)))
            } // end for k
        } // end for l
    }

    /**
     * Transforms and resamples volume using bilinear interpolation. This version used with color images
     *
     * @param  imgBuf   Input image array
     * @param  imgBuf2  Output image array
     * @param  kTM     Matrix to be applied
     */
    private void transformBilinearC(float[] imgBuf, float[] imgBuf2, TransMatrix kTM) {
        int i, j;
        int iAdj, jAdj;
        int X0pos, Y0pos;
        int X1pos, Y1pos;
        float X, Y;
        float x0, y0;
        float x1, y1;
        float imm, jmm;

        int mod = Math.max(1, oXdim / 50);

        int counter = 0; // used for progress bar
        int roundX, roundY;
        float x1y1, x0y1, x1y0, x0y0;
        float temp1, temp2;
        int temp3, temp4, temp5, temp6, temp7, temp8;
        float T00, T01, T02, T10, T11, T12;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if (pad) {
                iAdj = i - margins[0];
            } else {
                iAdj = i;
            }

            imm = iAdj * oXres;
            temp1 = (imm * T00) + T02;
            temp2 = (imm * T10) + T12;

            if (((i % mod) == 0)) {
                fireProgressStateChanged((int) (((float) i / oXdim * 100) + 0.5));
            }

            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                // transform i,j
                temp3 = 4 * (i + (j * oXdim));

                if (pad) {
                    imgBuf2[temp3] = (float) padVal;
                    imgBuf2[temp3 + 1] = (float) padVal;
                    imgBuf2[temp3 + 2] = (float) padVal;
                    imgBuf2[temp3 + 3] = (float) padVal;
                    jAdj = j - margins[1];
                } else {
                    imgBuf2[temp3] = 0; // remains zero if voxel is transformed out of bounds
                    imgBuf2[temp3 + 1] = 0;
                    imgBuf2[temp3 + 2] = 0;
                    imgBuf2[temp3 + 3] = 0;
                    jAdj = j;
                }

                jmm = jAdj * oYres;

                X = (temp1 + (jmm * T01)) / iXres;
                roundX = (int) (X + 0.5f);

                if ((X >= 0) && (X < iXdim)) {
                    Y = (temp2 + (jmm * T11)) / iYres;
                    roundY = (int) (Y + 0.5f);

                    if ((Y >= 0) && (Y < iYdim)) {

                        if ((X >= (iXdim - 1)) || (Y >= (iYdim - 1))) { // cannot interpolate on last X or Y
                            X0pos = Math.min(roundX, iXdim - 1);
                            Y0pos = Math.min(roundY, iYdim - 1) * iXdim;
                            temp4 = 4 * (Y0pos + X0pos);
                            imgBuf2[temp3] = imgBuf[temp4];
                            imgBuf2[temp3 + 1] = imgBuf[temp4 + 1];
                            imgBuf2[temp3 + 2] = imgBuf[temp4 + 2];
                            imgBuf2[temp3 + 3] = imgBuf[temp4 + 3];
                        } else {

                            // set intensity of i,j,k to new transformed coordinate if
                            // x,y,z is w/in dimensions of image
                            x0 = X - (int) X;
                            y0 = Y - (int) Y;
                            x1 = 1 - x0;
                            y1 = 1 - y0;
                            X0pos = (int) X;
                            Y0pos = (int) Y * iXdim;
                            X1pos = X0pos + 1;
                            Y1pos = Y0pos + iXdim;
                            temp5 = 4 * (Y0pos + X0pos);
                            temp6 = 4 * (Y0pos + X1pos);
                            temp7 = 4 * (Y1pos + X0pos);
                            temp8 = 4 * (Y1pos + X1pos);
                            x1y1 = x1 * y1;
                            x0y1 = x0 * y1;
                            x1y0 = x1 * y0;
                            x0y0 = x0 * y0;
                            imgBuf2[temp3] = (x1y1 * imgBuf[temp5]) + (x0y1 * imgBuf[temp6]) + (x1y0 * imgBuf[temp7]) +
                                             (x0y0 * imgBuf[temp8]);
                            imgBuf2[temp3 + 1] = (x1y1 * imgBuf[temp5 + 1]) + (x0y1 * imgBuf[temp6 + 1]) +
                                                 (x1y0 * imgBuf[temp7 + 1]) + (x0y0 * imgBuf[temp8 + 1]);
                            imgBuf2[temp3 + 2] = (x1y1 * imgBuf[temp5 + 2]) + (x0y1 * imgBuf[temp6 + 2]) +
                                                 (x1y0 * imgBuf[temp7 + 2]) + (x0y0 * imgBuf[temp8 + 2]);
                            imgBuf2[temp3 + 3] = (x1y1 * imgBuf[temp5 + 3]) + (x0y1 * imgBuf[temp6 + 3]) +
                                                 (x1y0 * imgBuf[temp7 + 3]) + (x0y0 * imgBuf[temp8 + 3]);
                        }
                    } // end if Y in bounds
                } // end if X in bounds

                counter++;
            } // end for j
        } // end for i

        if (threadStopped) {
            return;
        }

        try {
            destImage.importData(0, imgBuf2, true);
        } catch (IOException error) {
            MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
        }
    }

    /**
     * Transforms and resamples volume using Bspline interpolation.
     *
     * @param  imgBuf  image array
     * @param  kTM    transformation matrix to be applied
     * @param  degree  degree of polynomial
     */
    private void transformBspline2D(float[] imgBuf, TransMatrix kTM, int degree) {
        int i, j;
        float X, Y;
        float value;
        float imm, jmm;
        int mod = Math.max(1, oYdim / 50);
        ;

        float j1, j2;
        float T00, T01, T02, T10, T11, T12;
        int nz;
        int x, y, z;
        BSplineProcessing splineAlg = null;
        float sliceMin;
        float sliceMax;
        float[][] img2D;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        if (srcImage.getNDims() == 2) {
            nz = 1;
        } else if (srcImage.getNDims() == 3) {
            nz = srcImage.getExtents()[2];
        } else {
            nz = srcImage.getExtents()[2] * srcImage.getExtents()[3];
        }

        splineAlg = new BSplineProcessing();
        img2D = new float[iXdim][iYdim];

        float invXRes = 1 / iXres;
        float invYRes = 1 / iYres;

        int index = 0;

        for (z = 0; z < nz; z++) {

            if (z >= 1) {

                try {
                    srcImage.exportData(z * imgLength, imgLength, imgBuf);
                } catch (IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);


                    return;
                }
            }

            sliceMin = Float.MAX_VALUE;
            sliceMax = -Float.MAX_VALUE;

            for (y = 0; y < iYdim; y++) {

                for (x = 0; x < iXdim; x++) {
                    img2D[x][y] = imgBuf[x + (iXdim * y)];

                    if (img2D[x][y] > sliceMax) {
                        sliceMax = img2D[x][y];
                    }

                    if (img2D[x][y] < sliceMin) {
                        sliceMin = img2D[x][y];
                    }
                }
            }

            splineAlg.samplesToCoefficients(img2D, iXdim, iYdim, degree);

            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                if (((j % mod) == 0)) {
                    fireProgressStateChanged((int) ((((float) z / nz * 100) + ((float) j / (oYdim * nz) * 100)) +
                                                    0.5f));
                }

                jmm = j * oYres;
                j1 = (jmm * T01) + T02;
                j2 = (jmm * T11) + T12;

                for (i = 0; (i < oXdim) && !threadStopped; i++) {

                    // transform i,j,z
                    value = sliceMin; // remains zero if voxel is transformed out of bounds
                    imm = i * oXres;
                    X = (j1 + (imm * T00)) * invXRes;

                    if ((X > -0.5f) && (X < iXdim)) {
                        Y = (j2 + (imm * T10)) * invYRes;

                        if ((Y > -0.5f) && (Y < iYdim)) {
                            value = (float) splineAlg.interpolatedValue(img2D, X, Y, iXdim, iYdim, degree);

                            if (value > sliceMax) {
                                value = sliceMax;
                            } else if (value < sliceMin) {
                                value = sliceMin;
                            }
                        }
                    }

                    destImage.set(index++, value);
                }
            }
        }

        Preferences.debug("finished Bspline");

    }

    /**
     * Transforms and resamples volume using Bspline interpolation.
     *
     * @param  imgBuf  image array
     * @param  kTM    transformation matrix to be applied
     * @param  degree  degree of polynomial
     */
    private void transformBspline2DC(float[] imgBuf, TransMatrix kTM, int degree) {
        int i, j;
        float X, Y;
        float value;
        float imm, jmm;
        int mod = Math.max(1, oYdim / 50);
        ;

        float j1, j2;
        float T00, T01, T02, T10, T11, T12;
        int nz;
        int x, y, z;
        BSplineProcessing splineAlg = null;
        float sliceMin;
        float sliceMax;
        float[][] img2D;
        int c;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        if (srcImage.getNDims() == 2) {
            nz = 1;
        } else if (srcImage.getNDims() == 3) {
            nz = srcImage.getExtents()[2];
        } else {
            nz = srcImage.getExtents()[2] * srcImage.getExtents()[3];
        }

        img2D = new float[iXdim][iYdim];
        splineAlg = new BSplineProcessing();

        float invXRes = 1 / iXres;
        float invYRes = 1 / iYres;

        int[] index = new int[4];
        index[0] = 0;
        index[1] = 1;
        index[2] = 2;
        index[3] = 3;

        for (z = 0; z < nz; z++) {

            if (z >= 1) {

                try {
                    srcImage.exportData(z * imgLength, imgLength, imgBuf);
                } catch (IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);


                    return;
                }
            }

            for (c = 0; c < 4; c++) {
                sliceMin = Float.MAX_VALUE;
                sliceMax = -Float.MAX_VALUE;

                for (y = 0; y < iYdim; y++) {

                    for (x = 0; x < iXdim; x++) {
                        img2D[x][y] = imgBuf[(4 * (x + (iXdim * y))) + c];

                        if (img2D[x][y] > sliceMax) {
                            sliceMax = img2D[x][y];
                        }

                        if (img2D[x][y] < sliceMin) {
                            sliceMin = img2D[x][y];
                        }
                    }
                }

                splineAlg.samplesToCoefficients(img2D, iXdim, iYdim, degree);

                for (j = 0; (j < oYdim) && !threadStopped; j++) {

                    if (((j % mod) == 0)) {
                        fireProgressStateChanged((int) ((((float) z / nz * 100) + ((float) c / (4 * nz) * 100) +
                                                         ((float) j / (4 * oYdim * nz) * 100)) + 0.5f));
                    }

                    jmm = j * oYres;
                    j1 = (jmm * T01) + T02;
                    j2 = (jmm * T11) + T12;

                    for (i = 0; (i < oXdim) && !threadStopped; i++) {

                        // transform i,j,z
                        value = sliceMin; // remains zero if voxel is transformed out of bounds
                        imm = i * oXres;
                        X = (j1 + (imm * T00)) * invXRes;

                        if ((X > -0.5f) && (X < iXdim)) {
                            Y = (j2 + (imm * T10)) * invYRes;

                            if ((Y > -0.5f) && (Y < iYdim)) {
                                value = (float) splineAlg.interpolatedValue(img2D, X, Y, iXdim, iYdim, degree);

                                if (value > sliceMax) {
                                    value = sliceMax;
                                } else if (value < sliceMin) {
                                    value = sliceMin;
                                }
                            }
                        }

                        destImage.set(index[c], value);
                        index[c] += 4;
                    }
                }
            }
        }

        Preferences.debug("finished Bspline");

    }

    /**
     * Transforms and resamples volume using Bspline interpolation.
     *
     * @param  imgBuf  image array
     * @param  kTM    transformation matrix to be applied
     * @param  degree  degree of polynomial
     */
    private void transformBspline3D(float[] imgBuf, TransMatrix kTM, int degree) {
        int i, j, k;
        float X, Y, Z;
        float value;
        float imm, jmm, kmm;
        int mod = Math.max(1, oZdim / 50);
        float[][][] image;
        int x, y, z;
        BSplineProcessing splineAlg;
        float imageMin;
        float imageMax;

        srcImage.calcMinMax();
        imageMin = (float) srcImage.getMin();
        imageMax = (float) srcImage.getMax();

        float k1, k2, k3, j1, j2, j3;

        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;

        image = new float[iXdim][iYdim][iZdim];

        for (z = 0; z < iZdim; z++) {

            for (y = 0; y < iYdim; y++) {

                for (x = 0; x < iXdim; x++) {
                    image[x][y][z] = imgBuf[x + (iXdim * y) + (iXdim * iYdim * z)];
                }
            }
        }

        splineAlg = new BSplineProcessing();
        splineAlg.samplesToCoefficients(image, iXdim, iYdim, iZdim, degree);

        float invXRes = 1 / iXres;
        float invYRes = 1 / iYres;
        float invZRes = 1 / iZres;

        int index = 0;

        for (k = 0; (k < oZdim) && !threadStopped; k++) {

            if (((k % mod) == 0)) {
                fireProgressStateChanged((int) (((float) k / oZdim * 100) + 0.5f));
            }

            kmm = k * oZres;
            k1 = (kmm * T02) + T03;
            k2 = (kmm * T12) + T13;
            k3 = (kmm * T22) + T23;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {
                jmm = j * oYres;
                j1 = (jmm * T01) + k1;
                j2 = (jmm * T11) + k2;
                j3 = (jmm * T21) + k3;

                for (i = 0; (i < oXdim) && !threadStopped; i++) {

                    // transform i,j,k
                    value = (float) srcImage.getMin(); // remains zero if voxel is transformed out of bounds
                    imm = i * oXres;
                    X = (j1 + (imm * T00)) * invXRes;

                    if ((X > -0.5f) && (X < iXdim)) {
                        Y = (j2 + (imm * T10)) * invYRes;

                        if ((Y > -0.5f) && (Y < iYdim)) {
                            Z = (j3 + (imm * T20)) * invZRes;

                            if ((Z > -0.5f) && (Z < iZdim)) {
                                value = (float) splineAlg.interpolatedValue(image, X, Y, Z, iXdim, iYdim, iZdim,
                                                                            degree);

                                if (value > imageMax) {
                                    value = imageMax;
                                } else if (value < imageMin) {
                                    value = imageMin;
                                }
                            }
                        }
                    }

                    destImage.set(index++, value);
                }
            }
        }

        Preferences.debug("finished Bspline");

    }

    /**
     * Transforms and resamples volume using Bspline interpolation.
     *
     * @param  imgBuf  image array
     * @param  kTM    transformation matrix to be applied
     * @param  degree  degree of polynomial
     */
    private void transformBspline3DC(float[] imgBuf, TransMatrix kTM, int degree) {
        int i, j, k;
        float X, Y, Z;
        float value;
        float imm, jmm, kmm;
        int mod = Math.max(1, oZdim / 50);
        float[][][] image;
        int x, y, z;
        BSplineProcessing splineAlg;
        float[] imageMin = new float[4];
        float[] imageMax = new float[4];
        int c;

        float k1, k2, k3, j1, j2, j3;

        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;

        float invXRes = 1 / iXres;
        float invYRes = 1 / iYres;
        float invZRes = 1 / iZres;

        int[] index = new int[4];
        index[0] = 0;
        index[1] = 1;
        index[2] = 2;
        index[3] = 3;

        splineAlg = new BSplineProcessing();

        image = new float[iXdim][iYdim][iZdim];

        for (c = 0; c < 4; c++) {
            imageMin[c] = Float.MAX_VALUE;
            imageMax[c] = -Float.MAX_VALUE;

            for (z = 0; z < iZdim; z++) {

                for (y = 0; y < iYdim; y++) {

                    for (x = 0; x < iXdim; x++) {
                        image[x][y][z] = imgBuf[(4 * (x + (iXdim * y) + (iXdim * iYdim * z))) + c];
                        
                        if (image[x][y][z] > imageMax[c]) {
                            imageMax[c] = image[x][y][z];
                        }
                        
                        if (image[x][y][z] < imageMin[c]) {
                            imageMin[c] = image[x][y][z];
                        }
                    }
                }
            }

            splineAlg.samplesToCoefficients(image, iXdim, iYdim, iZdim, degree);


            for (k = 0; (k < oZdim) && !threadStopped; k++) {

                if (((k % mod) == 0)) {
                    fireProgressStateChanged((int) (((float) k / (4 * oZdim) * 100) + ((float) c / 4 * 100) + 0.5f));
                }

                kmm = k * oZres;
                k1 = (kmm * T02) + T03;
                k2 = (kmm * T12) + T13;
                k3 = (kmm * T22) + T23;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {
                    jmm = j * oYres;
                    j1 = (jmm * T01) + k1;
                    j2 = (jmm * T11) + k2;
                    j3 = (jmm * T21) + k3;

                    for (i = 0; (i < oXdim) && !threadStopped; i++) {

                        // transform i,j,k
                        value = imageMin[c]; // if voxel is transformed out of bounds
                        imm = i * oXres;
                        X = (j1 + (imm * T00)) * invXRes;

                        if ((X > -0.5f) && (X < iXdim)) {
                            Y = (j2 + (imm * T10)) * invYRes;

                            if ((Y > -0.5f) && (Y < iYdim)) {
                                Z = (j3 + (imm * T20)) * invZRes;

                                if ((Z > -0.5f) && (Z < iZdim)) {
                                    value = (float) splineAlg.interpolatedValue(image, X, Y, Z, iXdim, iYdim, iZdim,
                                                                                degree);

                                    if (value > imageMax[c]) {
                                        value = imageMax[c];
                                    } else if (value < imageMin[c]) {
                                        value = imageMin[c];
                                    }
                                }
                            }
                        }

                        destImage.set(index[c], value);
                        index[c] += 4;
                    }
                }
            }
        }

        Preferences.debug("finished Bspline");

    }

    /**
     * Transforms and resamples volume using Bspline interpolation.
     *
     * @param  imgBuf  image array
     * @param  kTM    transformation matrix to be applied
     * @param  degree  degree of polynomial
     */
    private void transformBspline4D(float[] imgBuf, TransMatrix kTM, int degree) {
        int i, j, k;
        float X, Y, Z;
        float value;
        float imm, jmm, kmm;
        float[][][] image;
        int x, y, z;
        BSplineProcessing splineAlg;
        float volMin;
        float volMax;
        int t;

        float k1, k2, k3, j1, j2, j3;

        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;


        image = new float[iXdim][iYdim][iZdim];
        splineAlg = new BSplineProcessing();

        float invXRes = 1 / iXres;
        float invYRes = 1 / iYres;
        float invZRes = 1 / iZres;

        int index = 0;

        for (t = 0; t < iTdim; t++) {
            fireProgressStateChanged((int) (((float) t / iTdim * 100) + 0.5));

            if ((t >= 1)) {

                try {
                    srcImage.exportData(t * imgLength, imgLength, imgBuf);
                } catch (IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);


                    return;
                }
            }

            volMin = Float.MAX_VALUE;
            volMax = -Float.MAX_VALUE;

            for (z = 0; z < iZdim; z++) {

                for (y = 0; y < iYdim; y++) {

                    for (x = 0; x < iXdim; x++) {
                        image[x][y][z] = imgBuf[x + (iXdim * y) + (iXdim * iYdim * z)];

                        if (image[x][y][z] > volMax) {
                            volMax = image[x][y][z];
                        }

                        if (image[x][y][z] < volMin) {
                            volMin = image[x][y][z];
                        }
                    }
                }
            }

            splineAlg.samplesToCoefficients(image, iXdim, iYdim, iZdim, degree);

            for (k = 0; (k < oZdim) && !threadStopped; k++) {

                kmm = k * oZres;
                k1 = (kmm * T02) + T03;
                k2 = (kmm * T12) + T13;
                k3 = (kmm * T22) + T23;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {
                    jmm = j * oYres;
                    j1 = (jmm * T01) + k1;
                    j2 = (jmm * T11) + k2;
                    j3 = (jmm * T21) + k3;

                    for (i = 0; (i < oXdim) && !threadStopped; i++) {

                        // transform i,j,k
                        value = volMin; // if voxel is transformed out of bounds
                        imm = i * oXres;
                        X = (j1 + (imm * T00)) * invXRes;

                        if ((X > -0.5f) && (X < iXdim)) {
                            Y = (j2 + (imm * T10)) * invYRes;

                            if ((Y > -0.5f) && (Y < iYdim)) {
                                Z = (j3 + (imm * T20)) * invZRes;

                                if ((Z > -0.5f) && (Z < iZdim)) {
                                    value = (float) splineAlg.interpolatedValue(image, X, Y, Z, iXdim, iYdim, iZdim,
                                                                                degree);

                                    if (value > volMax) {
                                        value = volMax;
                                    } else if (value < volMin) {
                                        value = volMin;
                                    }
                                }
                            }
                        }

                        destImage.set(index++, value);
                    }
                }
            }
        }

        Preferences.debug("finished Bspline");

    }
    
    /**
     * Transforms and resamples color volume using Bspline interpolation.
     *
     * @param  imgBuf  image array
     * @param  kTM    transformation matrix to be applied
     * @param  degree  degree of polynomial
     */
    private void transformBspline4DC(float[] imgBuf, TransMatrix kTM, int degree) {
        int i, j, k;
        float X, Y, Z;
        float value;
        float imm, jmm, kmm;
        float[][][] image;
        int x, y, z, c;
        BSplineProcessing splineAlg;
        float volMin[] = new float[4];
        float volMax[] = new float[4];
        int t;

        float k1, k2, k3, j1, j2, j3;

        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;


        image = new float[iXdim][iYdim][iZdim];
        splineAlg = new BSplineProcessing();

        float invXRes = 1 / iXres;
        float invYRes = 1 / iYres;
        float invZRes = 1 / iZres;

        int index[] = new int[4];
        index[0] = 0;
        index[1] = 1;
        index[2] = 2;
        index[3] = 3;

        for (t = 0; t < iTdim; t++) {
            fireProgressStateChanged((int) (((float) t / iTdim * 100) + 0.5));

            if ((t >= 1)) {

                try {
                    srcImage.exportData(t * imgLength, imgLength, imgBuf);
                } catch (IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);


                    return;
                }
            }

            for (c = 0; c < 4; c++) {
                volMin[c] = Float.MAX_VALUE;
                volMax[c] = -Float.MAX_VALUE;
                for (z = 0; z < iZdim; z++) {
    
                    for (y = 0; y < iYdim; y++) {
    
                        for (x = 0; x < iXdim; x++) {
                            image[x][y][z] = imgBuf[4*(x + (iXdim * y) + (iXdim * iYdim * z)) + c];
    
                            if (image[x][y][z] > volMax[c]) {
                                volMax[c] = image[x][y][z];
                            }
    
                            if (image[x][y][z] < volMin[c]) {
                                volMin[c] = image[x][y][z];
                            }
                        }
                    }
                }
            

                splineAlg.samplesToCoefficients(image, iXdim, iYdim, iZdim, degree);
    
                for (k = 0; (k < oZdim) && !threadStopped; k++) {
    
                    kmm = k * oZres;
                    k1 = (kmm * T02) + T03;
                    k2 = (kmm * T12) + T13;
                    k3 = (kmm * T22) + T23;
    
                    for (j = 0; (j < oYdim) && !threadStopped; j++) {
                        jmm = j * oYres;
                        j1 = (jmm * T01) + k1;
                        j2 = (jmm * T11) + k2;
                        j3 = (jmm * T21) + k3;
    
                        for (i = 0; (i < oXdim) && !threadStopped; i++) {
    
                            // transform i,j,k
                            value = volMin[c]; // if voxel is transformed out of bounds
                            imm = i * oXres;
                            X = (j1 + (imm * T00)) * invXRes;
    
                            if ((X > -0.5f) && (X < iXdim)) {
                                Y = (j2 + (imm * T10)) * invYRes;
    
                                if ((Y > -0.5f) && (Y < iYdim)) {
                                    Z = (j3 + (imm * T20)) * invZRes;
    
                                    if ((Z > -0.5f) && (Z < iZdim)) {
                                        value = (float) splineAlg.interpolatedValue(image, X, Y, Z, iXdim, iYdim, iZdim,
                                                                                    degree);
    
                                        if (value > volMax[c]) {
                                            value = volMax[c];
                                        } else if (value < volMin[c]) {
                                            value = volMin[c];
                                        }
                                    }
                                }
                            }
    
                            destImage.set(index[c], value);
                            index[c] += 4;
                        }
                    }
                }
            }
        }

        Preferences.debug("finished Bspline");

    }


    /**
     * Transforms and resamples volume using Bspline interpolation.
     *
     * @param  imgBuf  image array
     * @param  kTM    transformation matrix to be applied
     * @param  clip    degree of polynomial
     */
    /*private void transformBspline2D(float[] imgBuf, TransMatrix kTM, int degree) {
     *  AlgorithmBSpline Bspline = new AlgorithmBSpline(); int i, j; float X, Y; float value; float imm, jmm; int[]
     * inVolExtents = { iXdim, iYdim }; int mod = Math.max(1, oYdim / 50); float j1, j2; float T00, T01, T02, T10, T11,
     * T12, T20, T21, T22;
     *
     * T00 = kTM.M00; T01 = kTM.M01; T02 = kTM.M02; T10 = kTM.M10; T11 =
     * kTM.M11; T12 = kTM.M12; T20 = kTM.M20; T21 = kTM.M21; T22 = (float)
     * xfrm[2][2];
     *
     * Bspline.setup2DBSpline(imgBuf, inVolExtents, degree);
     *
     * float invXRes = 1 / iXres; float invYRes = 1 / iYres;
     *
     * int index = 0;
     *
     * for (j = 0; (j < oYdim) && !threadStopped; j++) {
     *
     * if (((j % mod) == 0)) {         fireProgressStateChanged((int) (((float) j / oYdim * 100) + 0.5f));     }
     *
     * jmm = j * oYres;     j1 = (jmm * T01) + T02;     j2 = (jmm * T11) + T12;
     *
     * for (i = 0; (i < oXdim) && !threadStopped; i++) {
     *
     * // transform i,j,k         value = (float) srcImage.getMin(); // remains zero if voxel is transformed out of bounds
     *   imm = i * oXres;         X = (j1 + (imm * T00)) * invXRes;
     *
     * if ((X > -0.5f) && (X < (iXdim - 0.5f))) {             Y = (j2 + (imm * T10)) * invYRes;
     *
     * if ((Y > -0.5f) && (Y < (iYdim - 0.5f))) {                 value = Bspline.bSpline2D(0, 0, X, Y);      } }
     *
     * destImage.set(index++, value);     } }
     *
     * Bspline.finalize(); Preferences.debug("finished Bspline");
     *
     *}*/

    /**
     * Transforms and resamples volume using Bspline interpolation. This version used with color images.
     *
     * @param  imgBuf  output image array
     * @param  kTM    transformation matrix to be applied
     * @param  clip    degree of polynomial
     */
    /*private void transformBspline2DC(float[] imgBuf, float[] imgBuf2, TransMatrix kTM, int degree) {
     *  AlgorithmBSpline Bspline = new AlgorithmBSpline(); int i, j; float X, Y; float[] value = new float[4]; float
     * imm, jmm; int[] inVolExtents = { iXdim, iYdim }; int mod = Math.max(1, oXdim / 50); int counter = 0; // used for
     * progress bar float temp1, temp2; int temp3; float T00, T01, T02, T10, T11, T12;
     *
     * T00 = kTM.M00; T01 = kTM.M01; T02 = kTM.M02; T10 = kTM.M10; T11 =
     * kTM.M11; T12 = kTM.M12;
     *
     * Bspline.setup2DBSplineC(imgBuf, inVolExtents, degree);
     *
     * for (i = 0; (i < oXdim) && !threadStopped; i++) {
     *
     * if (((i % mod) == 0)) {         fireProgressStateChanged((int) (((float) i / oXdim * 100) + 0.5));     }
     *
     * imm = (float) i * oXres;     temp1 = (imm * T00) + T02;     temp2 = (imm * T10) + T12;
     *
     * for (j = 0; (j < oYdim) && !threadStopped; j++) {
     *
     * // convert to mm         value[0] = 0; // will remain zero if boundary conditions not met         value[1] = 0;
     * value[2] = 0;         value[3] = 0;
     *
     * jmm = (float) j * oYres;
     *
     * // transform i,j         X = (temp1 + (jmm * T01)) / iXres;
     *
     * if ((X >= 0) && (X < iXdim)) { // check bounds             Y = (temp2 + (jmm * T11)) / iYres;
     *
     * if ((Y >= 0) && (Y < iYdim)) {                 value = Bspline.bSpline2DC(0, 0, X, Y);             }     }
     *
     * temp3 = 4 * (i + (j * oXdim));         imgBuf2[temp3] = value[0];         imgBuf2[temp3 + 1] = value[1];
     * imgBuf2[temp3 + 2] = value[2];         imgBuf2[temp3 + 3] = value[3];         counter++;     } }
     *
     * if (threadStopped) {     return; }
     *
     * try {     destImage.importData(0, imgBuf2, true); } catch (IOException error) {
     * MipavUtil.displayError("AlgorithmTransform: IOException Error on importData"); }
     *
     * Bspline.finalize(); Preferences.debug("finished Bspline");
     *
     *}*/

    /**
     * Transforms and resamples volume using Bspline interpolation.
     *
     * @param  imgBuf  image array
     * @param  kTM    transformation matrix to be applied
     * @param  clip    degree of polynomial
     */
    /*private void transformBspline3D(float[] imgBuf, TransMatrix kTM, int degree) {
     *  AlgorithmBSpline Bspline = new AlgorithmBSpline(); int i, j, k; float X, Y, Z; float value; int sliceSize; float
     * imm, jmm, kmm; int[] inVolExtents = { iXdim, iYdim, iZdim }; int mod = Math.max(1, oZdim / 50);
     *
     * sliceSize = iXdim * iYdim;
     *
     * float k1, k2, k3, j1, j2, j3;
     *
     * float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
     *
     * T00 = kTM.M00; T01 = kTM.M01; T02 = kTM.M02; T03 = kTM.M03; T10 =
     * kTM.M10; T11 = kTM.M11; T12 = kTM.M12; T13 = kTM.M13; T20 = (float)
     * xfrm[2][0]; T21 = kTM.M21; T22 = kTM.M22; T23 = kTM.M23;
     *
     * Bspline.setup3DBSpline(imgBuf, inVolExtents, degree);
     *
     * float invXRes = 1 / iXres; float invYRes = 1 / iYres; float invZRes = 1 / iZres;
     *
     * int index = 0;
     *
     * for (k = 0; (k < oZdim) && !threadStopped; k++) {
     *
     * if (((k % mod) == 0)) {         fireProgressStateChanged((int) (((float) k / oZdim * 100) + 0.5f));     }
     *
     * kmm = k * oZres;     k1 = (kmm * T02) + T03;     k2 = (kmm * T12) + T13;     k3 = (kmm * T22) + T23;
     *
     * for (j = 0; (j < oYdim) && !threadStopped; j++) {         jmm = j * oYres;         j1 = (jmm * T01) + k1;  j2 =
     * (jmm * T11) + k2;         j3 = (jmm * T21) + k3;
     *
     * for (i = 0; (i < oXdim) && !threadStopped; i++) {
     *
     * // transform i,j,k             value = (float) srcImage.getMin(); // remains zero if voxel is transformed out of
     * bounds             imm = i * oXres;             X = (j1 + (imm * T00)) * invXRes;
     *
     * if ((X > -0.5f) && (X < (iXdim - 0.5f))) {                 Y = (j2 + (imm * T10)) * invYRes;
     *
     * if ((Y > -0.5f) && (Y < (iYdim - 0.5f))) {                     Z = (j3 + (imm * T20)) * invZRes;
     *
     * if ((Z > -0.5f) && (Z < (iZdim - 0.5f))) {                         value = Bspline.bSpline3D(0, 0, 0, X, Y, Z);
     *    }                 }             }
     *
     * destImage.set(index++, value);         }     } }
     *
     * Bspline.finalize(); Preferences.debug("finished Bspline");
     *
     *}*/

    /**
     * Transforms and resamples volume using Bspline interpolation. This version used with color images.
     *
     * @param  imgBuf  Output image array
     * @param  kTM    Transformation matrix to be applied
     * @param  clip    Degree of polynomial
     */
    /*private void transformBspline3DC(float[] imgBuf, float[] imgBuf2, TransMatrix kTM, int degree) {
     *  AlgorithmBSpline Bspline = new AlgorithmBSpline(); int i, j, k; float X, Y, Z; float[] value = new float[4];
     * float imm, jmm, kmm; int[] inVolExtents = { iXdim, iYdim, iZdim }; int mod = Math.max(1, oXdim / 50); int counter
     * = 0; // used for progress bar
     *
     * int osliceSize = oXdim * oYdim; float i1, i2, i3, j1, j2, j3; float temp1, temp2, temp3; int temp4; float T00, T01,
     * T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
     *
     * T00 = kTM.M00; T01 = kTM.M01; T02 = kTM.M02; T03 = kTM.M03; T10 =
     * kTM.M10; T11 = kTM.M11; T12 = kTM.M12; T13 = kTM.M13; T20 = (float)
     * xfrm[2][0]; T21 = kTM.M21; T22 = kTM.M22; T23 = kTM.M23;
     *
     * Bspline.setup3DBSplineC(imgBuf, inVolExtents, degree);
     *
     * for (i = 0; (i < oXdim) && !threadStopped; i++) {
     *
     * if (((i % mod) == 0)) {         fireProgressStateChanged((int) (((float) i / oXdim * 100) + 0.5));     }
     *
     * imm = (float) i * oXres;     i1 = (imm * T00) + T03;     i2 = (imm * T10) + T13;     i3 = (imm * T20) + T23;
     *
     * for (j = 0; (j < oYdim) && !threadStopped; j++) {         jmm = (float) j * oYres;         j1 = jmm * T01;   j2 =
     * jmm * T11;         j3 = jmm * T21;         temp1 = i3 + j3;         temp2 = i2 + j2;         temp3 = i1 + j1;
     *
     * for (k = 0; (k < oZdim) && !threadStopped; k++) {
     *
     * // convert to mm             value[0] = 0; // will remain zero if boundary conditions not met  value[1] = 0;
     * value[2] = 0;             value[3] = 0;             kmm = (float) k * oZres;
     *
     * // transform i,j,k             X = (temp3 + (kmm * T02)) / iXres;
     *
     * // convert back to pixels             if ((X >= 0) && (X < iXdim)) { // check bounds                 Y = (temp2 +
     * (kmm * T12)) / iYres;
     *
     * if ((Y >= 0) && (Y < iYdim)) {                     Z = (temp1 + (kmm * T22)) / iZres;
     *
     * if ((Z >= 0) && (Z < iZdim)) {                         value = Bspline.bSpline3DC(0, 0, 0, X, Y, Z);      }      }
     *        }
     *
     * temp4 = 4 * (i + (j * oXdim) + (k * osliceSize));             imgBuf2[temp4] = value[0]; imgBuf2[temp4 + 1] =
     * value[1];             imgBuf2[temp4 + 2] = value[2];             imgBuf2[temp4 + 3] = value[3]; counter++; } } }
     *
     * if (threadStopped) {     return; }
     *
     * try {     destImage.importData(0, imgBuf2, true); } catch (IOException error) {
     * MipavUtil.displayError("AlgorithmTransform: IOException Error on importData"); }
     *
     * Bspline.finalize(); Preferences.debug("finished Bspline");
     *
     *}*/

    /**
     * Transforms and resamples volume using Bspline interpolation Does a slice by slice bspline on a 3 dimensional
     * object.
     *
     * @param  imgBuf  image array
     * @param  kTM    transformation matrix to be applied
     * @param  clip    degree of polynomial
     */
    /*private void transformBspline3Dim2D(float[] imgBuf, TransMatrix kTM, int degree) {
     *  AlgorithmBSpline Bspline = new AlgorithmBSpline(); int i, j, k; float X, Y; float value; float imm, jmm; int[]
     * inVolExtents = { iXdim, iYdim };
     *
     * int mod = Math.max(1, oZdim / 50); int counter = 0; // used for progress bar float temp1, temp2; float T00, T01,
     * T02, T10, T11, T12, T20, T21, T22;
     *
     * T00 = kTM.M00; T01 = kTM.M01; T02 = kTM.M02; T10 = kTM.M10; T11 =
     * kTM.M11; T12 = kTM.M12; T20 = kTM.M20; T21 = kTM.M21; T22 = (float)
     * xfrm[2][2];
     *
     * // System.out.println("oZdim = "); for (k = 0; (k < oZdim) && !threadStopped; k++) {
     *
     * if (((k % mod) == 0)) {         fireProgressStateChanged((int) (((float) k / oZdim * 100) + 0.5));     }
     *
     * Bspline.setup2DBSpline(imgBuf, inVolExtents, degree);
     *
     * for (i = 0; (i < oXdim) && !threadStopped; i++) {         imm = (float) i * oXres;         temp1 = (imm * T00) +
     * T02;         temp2 = (imm * T10) + T12;
     *
     * for (j = 0; (j < oYdim) && !threadStopped; j++) {
     *
     * // convert to mm             value = (float) srcImage.getMin(); // will remain zero if boundary conditions not met
     *  jmm = (float) j * oYres;
     *
     * // transform i,j             X = (temp1 + (jmm * T01)) / iXres;
     *
     * if ((X >= 0) && (X < iXdim)) { // check bounds                 Y = (temp2 + (jmm * T11)) / iYres;
     *
     * if ((Y >= 0) && (Y < iYdim)) {                     value = Bspline.bSpline2D(0, 0, X, Y);      }      }
     *
     * destImage.set(i, j, k, value);             counter++;         }     }
     *
     * if (k < (oZdim - 1)) {
     *
     * try {             srcImage.exportData((k + 1) * imgLength, imgLength, imgBuf);         } catch (IOException error)
     * {             displayError("Algorithm Transform: Image(s) locked");             setCompleted(false);
     *
     *
     * return;         }     } // end if (k < (oZdim - 1)) } // end for k
     *
     * Bspline.finalize(); Preferences.debug("finished Bspline");}*/

    /**
     * This version used with color images This version performs a slice by slice algorithm on a 3 dimensional object
     * Transforms and resamples volume using Bspline interpolation.
     *
     * @param  imgBuf  output image array
     * @param  kTM    transformation matrix to be applied
     * @param  clip    degree of polynomial
     */
    /*private void transformBspline3Dim2DC(float[] imgBuf, float[] imgBuf2, TransMatrix kTM, int degree) {
     *  AlgorithmBSpline Bspline = new AlgorithmBSpline(); int i, j, k; float X, Y; float[] value = new float[4]; int
     * sliceSize; float imm, jmm; int[] inVolExtents = { iXdim, iYdim };
     *
     * sliceSize = iXdim * iYdim;
     *
     * int mod = Math.max(1, oZdim / 50); int counter = 0; // used for progress bar float temp1, temp2; int temp3; float
     * T00, T01, T02, T10, T11, T12;
     *
     * T00 = kTM.M00; T01 = kTM.M01; T02 = kTM.M02; T10 = kTM.M10; T11 =
     * kTM.M11; T12 = kTM.M12;
     *
     * for (k = 0; (k < oZdim) && !threadStopped; k++) {
     *
     * if (((k % mod) == 0)) {         fireProgressStateChanged((int) (((float) k / oZdim * 100) + 0.5));     }
     *
     * Bspline.setup2DBSplineC(imgBuf, inVolExtents, degree);
     *
     * for (i = 0; (i < oXdim) && !threadStopped; i++) {         imm = (float) i * oXres;         temp1 = (imm * T00) +
     * T02;         temp2 = (imm * T10) + T12;
     *
     * for (j = 0; (j < oYdim) && !threadStopped; j++) {
     *
     * // convert to mm             value[0] = 0; // will remain zero if boundary conditions not met  value[1] = 0;
     * value[2] = 0;             value[3] = 0;
     *
     * jmm = (float) j * oYres;
     *
     * // transform i,j             X = (temp1 + (jmm * T01)) / iXres;
     *
     * if ((X >= 0) && (X < iXdim)) { // check bounds                 Y = (temp2 + (jmm * T11)) / iYres;
     *
     * if ((Y >= 0) && (Y < iYdim)) {                     value = Bspline.bSpline2DC(0, 0, X, Y);       } }
     *
     * temp3 = 4 * (i + (j * oXdim));             imgBuf2[temp3] = value[0];             imgBuf2[temp3 + 1] = value[1];
     * imgBuf2[temp3 + 2] = value[2];             imgBuf2[temp3 + 3] = value[3]; counter++; } // for i     } // for j
     *
     * try {         destImage.importData(4 * k * oXdim * oYdim, imgBuf2, true);     } catch (IOException error) {
     * MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");     }
     *
     * if (k < (oZdim - 1)) {
     *
     * try {             srcImage.exportData((k + 1) * imgLength, imgLength, imgBuf);         } catch (IOException error)
     * {             displayError("Algorithm Transform: IOException Error on exportData"); setCompleted(false);
     *
     * return;         }     } // end if (k < (oZdim - 1)) } // end for k
     *
     * Bspline.finalize(); Preferences.debug("finished Bspline");}*/

    /**
     * Transforms and resamples 4 dimensional object using 3D Bspline interpolation.
     *
     * @param  imgBuf  image array
     * @param  kTM    transformation matrix to be applied
     * @param  clip    degree of polynomial
     */
    /*private void transformBspline4D(float[] imgBuf, TransMatrix kTM, int degree) {
     *  AlgorithmBSpline Bspline = new AlgorithmBSpline(); int i, j, k, l; float X, Y, Z; float value; float imm, jmm,
     * kmm; int[] inVolExtents = { iXdim, iYdim, iZdim }; int mod = Math.max(1, oTdim / 50); int counter = 0; // used
     * for progress bar
     *
     * float i1, i2, i3, j1, j2, j3; float temp1, temp2, temp3; float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21,
     * T22, T23;
     *
     * T00 = kTM.M00; T01 = kTM.M01; T02 = kTM.M02; T03 = kTM.M03; T10 =
     * kTM.M10; T11 = kTM.M11; T12 = kTM.M12; T13 = kTM.M13; T20 = (float)
     * xfrm[2][0]; T21 = kTM.M21; T22 = kTM.M22; T23 = kTM.M23;
     *
     * for (l = 0; (l < oTdim) && !threadStopped; l++) {
     *
     * if (((l % mod) == 0)) {         fireProgressStateChanged((int) (((float) l / oTdim * 100) + .5));     }
     *
     * Bspline.setup3DBSpline(imgBuf, inVolExtents, degree);
     *
     * for (i = 0; (i < oXdim) && !threadStopped; i++) {         imm = (float) i * oXres;         i1 = (imm * T00) + T03;
     * i2 = (imm * T10) + T13;         i3 = (imm * T20) + T23;
     *
     * for (j = 0; (j < oYdim) && !threadStopped; j++) {             jmm = (float) j * oYres;             j1 = jmm T01; j2
     * = jmm * T11;             j3 = jmm * T21;             temp1 = i3 + j3;             temp2 = i2 + j2; temp3 = i1 +
     * j1;
     *
     * for (k = 0; (k < oZdim) && !threadStopped; k++) {
     *
     * // convert to mm                 value = (float) srcImage.getMin(); // will remain zero if boundary conditions not
     * met                 kmm = (float) k * oZres;
     *
     * // transform i,j,k                 X = (temp3 + (kmm * T02)) / iXres;
     *
     * if ((X >= 0) && (X < iXdim)) { // check bounds                     Y = (temp2 + (kmm * T12)) / iYres;
     *
     * if ((Y >= 0) && (Y < iYdim)) {                         Z = (temp1 + (kmm * T22)) / iZres;
     *
     * if ((Z >= 0) && (Z < iZdim)) {                             value = Bspline.bSpline3D(0, 0, 0, X, Y, Z);   }
     *    }                 }
     *
     * destImage.set(i, j, k, l, value);                 counter++;             }         }     }
     *
     * if (l < (oTdim - 1)) {
     *
     * try {             srcImage.exportData((l + 1) * imgLength, imgLength, imgBuf);         } catch (IOException error)
     * {             displayError("Algorithm Transform: Image(s) locked");             setCompleted(false);
     *
     *
     * return;         }     } // end if (l < (oTdim - 1)) } // for l
     *
     * Bspline.finalize(); Preferences.debug("finished Bspline");}*/

    /**
     * Transforms and resamples volume using Bspline interpolation Does a slice by slice bspline on a 4 dimensional
     * object.
     *
     * @param  imgBuf  image array
     * @param  kTM    transformation matrix to be applied
     * @param  clip    degree of polynomial
     */
    /*private void transformBspline4Dim2D(float[] imgBuf, TransMatrix kTM, int degree) {
     *  AlgorithmBSpline Bspline = new AlgorithmBSpline(); int i, j, k, l; float X, Y; float value; int sliceSize; float
     * imm, jmm; int[] inVolExtents = { iXdim, iYdim };
     *
     * sliceSize = iXdim * iYdim;
     *
     * int counter = 0; // used for progress bar float temp1, temp2; float T00, T01, T02, T10, T11, T12, T20, T21, T22;
     *
     * T00 = kTM.M00; T01 = kTM.M01; T02 = kTM.M02; T10 = kTM.M10; T11 =
     * kTM.M11; T12 = kTM.M12; T20 = kTM.M20; T21 = kTM.M21; T22 = (float)
     * xfrm[2][2];
     *
     * for (l = 0; (l < oTdim) && !threadStopped; l++) {
     *
     * if (isProgressBarVisible()) {         fireProgressStateChanged((int) (((float) l / oTdim * 100) + 0.5));     }
     *
     * for (k = 0; (k < oZdim) && !threadStopped; k++) {         Bspline.setup2DBSpline(imgBuf, inVolExtents, degree);
     *
     * for (i = 0; (i < oXdim) && !threadStopped; i++) {             imm = (float) i * oXres;             temp1 = (imm *
     * T00) + T02;             temp2 = (imm * T10) + T12;
     *
     * for (j = 0; (j < oYdim) && !threadStopped; j++) {
     *
     * // convert to mm                 value = (float) srcImage.getMin(); // will remain zero if boundary conditions not
     * met                 jmm = (float) j * oYres;
     *
     * // transform i,j                 X = (temp1 + (jmm * T01)) / iXres;
     *
     * if ((X >= 0) && (X < iXdim)) { // check bounds                     Y = (temp2 + (jmm * T11)) / iYres;
     *
     * if ((Y >= 0) && (Y < iYdim)) {                         value = Bspline.bSpline2D(0, 0, X, Y);   }         }
     *
     * destImage.set(i, j, k, l, value);                 counter++;             }         }
     *
     * if ((k < (oZdim - 1)) || (l < (oTdim - 1))) {
     *
     * try {                 srcImage.exportData((l * oZdim * imgLength) + ((k + 1) * imgLength), imgLength, imgBuf);    }
     * catch (IOException error) {                 displayError("Algorithm Transform: Image(s) locked");
     * setCompleted(false);
     *
     * return;             }         } // end if ((k < (oZdim - 1))|| (l < (oTdim - 1)))     } // end for k } // end for l
     *
     * Bspline.finalize(); Preferences.debug("finished Bspline");}*/

    /**
     * Transforms and resamples volume using Bspline interpolation This version used with color images This version
     * performs a slice by slice algorithm on a 4 dimensional object.
     *
     * @param  imgBuf  output image array
     * @param  kTM    transformation matrix to be applied
     * @param  clip    degree of polynomial
     */
    /*private void transformBspline4Dim2DC(float[] imgBuf, float[] imgBuf2, TransMatrix kTM, int degree) {
     *  AlgorithmBSpline Bspline = new AlgorithmBSpline(); int i, j, k, l; float X, Y; float[] value = new float[4]; int
     * sliceSize; float imm, jmm; int[] inVolExtents = { iXdim, iYdim };
     *
     * sliceSize = iXdim * iYdim;
     *
     * int volSize = sliceSize * iZdim; int counter = 0; // used for progress bar float temp1, temp2; int temp3; float
     * T00, T01, T02, T10, T11, T12;
     *
     * T00 = kTM.M00; T01 = kTM.M01; T02 = kTM.M02; T10 = kTM.M10; T11 =
     * kTM.M11; T12 = kTM.M12;
     *
     * for (l = 0; (l < oTdim) && !threadStopped; l++) {
     *
     * if (isProgressBarVisible()) {         fireProgressStateChanged((int) (((float) l / oTdim * 100) + 0.5));     }
     *
     * for (k = 0; (k < oZdim) && !threadStopped; k++) {         Bspline.setup2DBSplineC(imgBuf, inVolExtents, degree);
     *
     * for (i = 0; (i < oXdim) && !threadStopped; i++) {             imm = (float) i * oXres;             temp1 = (imm *
     * T00) + T02;             temp2 = (imm * T10) + T12;
     *
     * for (j = 0; (j < oYdim) && !threadStopped; j++) {
     *
     * // convert to mm                 value[0] = 0; // will remain zero if boundary conditions not met value[1] = 0;
     * value[2] = 0;                 value[3] = 0;
     *
     * jmm = (float) j * oYres;
     *
     * // transform i,j                 X = (temp1 + (jmm * T01)) / iXres;
     *
     * if ((X >= 0) && (X < iXdim)) { // check bounds                     Y = (temp2 + (jmm * T11)) / iYres;
     *
     * if ((Y >= 0) && (Y < iYdim)) {                         value = Bspline.bSpline2DC(0, 0, X, Y);      }      }
     *
     * temp3 = 4 * (i + (j * oXdim));                 imgBuf2[temp3] = value[0]; imgBuf2[temp3 + 1] = value[1];
     * imgBuf2[temp3 + 2] = value[2];                 imgBuf2[temp3 + 3] = value[3];      counter++;       } // for j }
     * // for i
     *
     * try {             destImage.importData((4 * l * oXdim * oYdim * oZdim) + (4 * k * oXdim * oYdim), imgBuf2, true); }
     * catch (IOException error) {             MipavUtil.displayError("AlgorithmTransform: IOException Error on
     * importData");         }
     *
     * if ((k < (oZdim - 1)) || (l < (oTdim - 1))) {
     *
     * try {                 srcImage.exportData((l * oZdim * imgLength) + ((k + 1) * imgLength), imgLength, imgBuf);    }
     * catch (IOException error) {                 displayError("Algorithm Transform: IOException Error on exportData");
     *             setCompleted(false);
     *
     * return;             }         } // end if ((k < (oZdim - 1))|| (l < (oTdim - 1)))     } // end for k } // end for l
     *
     * Bspline.finalize(); Preferences.debug("finished Bspline");}*/

    /**
     * Transforms and resamples volume using cubic Lagrangian interpolation.
     *
     * @param  imgBuf  Image array
     * @param  kTM    Transformation matrix to be applied
     * @param  clip    if <code>true</code> clip output values to be within input range
     */
    private void transformCubicLagrangian2D(float[] imgBuf, TransMatrix kTM, boolean clip) {
        AlgorithmCubicLagrangian CLag = new AlgorithmCubicLagrangian();
        int i, j;
        float X, Y;
        float value;
        float imm, jmm;
        int[] inVolExtents = { iXdim, iYdim };
        int mod = Math.max(1, oXdim / 50);
        int counter = 0; // used for progress bar
        float temp1, temp2;
        float T00, T01, T02, T10, T11, T12;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        CLag.setup2DCubicLagrangian(imgBuf, inVolExtents, clip);

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged((int) (((float) i / oXdim * 100) + 0.5));
            }

            imm = (float) i * oXres;
            temp1 = (imm * T00) + T02;
            temp2 = (imm * T10) + T12;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                // convert to mm
                value = (float) srcImage.getMin(); // will remain zero if boundary conditions not met

                jmm = (float) j * oYres;

                // transform i,j
                X = (temp1 + (jmm * T01)) / iXres;

                if ((X >= 0) && (X < iXdim)) { // check bounds
                    Y = (temp2 + (jmm * T11)) / iYres;

                    if ((Y >= 0) && (Y < iYdim)) {
                        value = CLag.cubicLagrangian2D(X, Y);
                    }
                }

                destImage.set(i, j, value);
                counter++;
            }
        }

        CLag.finalize();
        CLag = null;
        Preferences.debug("finished cubic Lagrangian");
    }

    /**
     * Transforms and resamples volume using cubic Lagrangian interpolation. This version used with color images
     *
     * @param  imgBuf   Input image array
     * @param  imgBuf2  Output image array
     * @param  kTM     Transformation matrix to be applied
     * @param  clip     if true clip output values to be within input range
     */
    private void transformCubicLagrangian2DC(float[] imgBuf, float[] imgBuf2, TransMatrix kTM, boolean clip) {
        AlgorithmCubicLagrangian CLag = new AlgorithmCubicLagrangian();
        int i, j;
        float X, Y;
        float[] value = new float[4];
        float imm, jmm;
        int[] inVolExtents = { iXdim, iYdim };
        int mod = Math.max(1, oXdim / 50);
        int counter = 0; // used for progress bar
        float temp1, temp2;
        int temp3;
        float T00, T01, T02, T10, T11, T12;
        float argbMax = 255.0f;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        if (srcImage.getType() == ModelStorageBase.ARGB) {
            argbMax = 255.0f;
        } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
            argbMax = 65535.0f;
        }

        CLag.setup2DCubicLagrangianC(imgBuf, inVolExtents, argbMax, clip);

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged((int) (((float) i / oXdim * 100) + 0.5));
            }

            imm = (float) i * oXres;
            temp1 = (imm * T00) + T02;
            temp2 = (imm * T10) + T12;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                // convert to mm
                value[0] = 0; // will remain zero if boundary conditions not met
                value[1] = 0;
                value[2] = 0;
                value[3] = 0;

                jmm = (float) j * oYres;

                // transform i,j
                X = (temp1 + (jmm * T01)) / iXres;

                if ((X >= 0) && (X < iXdim)) { // check bounds
                    Y = (temp2 + (jmm * T11)) / iYres;

                    if ((Y >= 0) && (Y < iYdim)) {
                        value = CLag.cubicLagrangian2DC(X, Y);
                    }
                }

                temp3 = 4 * (i + (j * oXdim));
                imgBuf2[temp3] = value[0];
                imgBuf2[temp3 + 1] = value[1];
                imgBuf2[temp3 + 2] = value[2];
                imgBuf2[temp3 + 3] = value[3];
                counter++;
            }
        }

        if (threadStopped) {
            return;
        }

        try {
            destImage.importData(0, imgBuf2, true);
        } catch (IOException error) {
            MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
        }

        CLag.finalize();
        CLag = null;
        Preferences.debug("finished cubic Lagrangian");
    }

    /**
     * Transforms and resamples volume using cubic Lagrangian interpolation.
     *
     * @param  imgBuf  Image array
     * @param  kTM    Transformation matrix to be applied
     * @param  clip    If <code>true</code> clip output values to be within input range
     */
    private void transformCubicLagrangian3D(float[] imgBuf, TransMatrix kTM, boolean clip) {
        AlgorithmCubicLagrangian CLag = new AlgorithmCubicLagrangian();
        int i, j, k;
        float X, Y, Z;
        float value;
        int sliceSize;
        float imm, jmm, kmm;
        int[] inVolExtents = { iXdim, iYdim, iZdim };
        int mod = Math.max(1, oXdim / 50);

        sliceSize = iXdim * iYdim;

        float k1, k2, k3, j1, j2, j3;
        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;

        CLag.setup3DCubicLagrangian(imgBuf, inVolExtents, clip);

        float invXRes = 1 / iXres;
        float invYRes = 1 / iYres;
        float invZRes = 1 / iZres;

        int index = 0;

        for (k = 0; (k < oZdim) && !threadStopped; k++) {

            if (((k % mod) == 0)) {
                fireProgressStateChanged((int) (((float) k / oZdim * 100) + 0.5f));
            }

            kmm = k * oZres;
            k1 = (kmm * T02) + T03;
            k2 = (kmm * T12) + T13;
            k3 = (kmm * T22) + T23;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {
                jmm = j * oYres;
                j1 = (jmm * T01) + k1;
                j2 = (jmm * T11) + k2;
                j3 = (jmm * T21) + k3;

                for (i = 0; (i < oXdim) && !threadStopped; i++) {

                    // transform i,j,k
                    value = (float) srcImage.getMin(); // remains zero if voxel is transformed out of bounds
                    imm = i * oXres;
                    X = (j1 + (imm * T00)) * invXRes;

                    if ((X >= 0) && (X < iXdim)) {
                        Y = (j2 + (imm * T10)) * invYRes;

                        if ((Y >= 0) && (Y < iYdim)) {
                            Z = (j3 + (imm * T20)) * invZRes;

                            if ((Z >= 0) && (Z < iZdim)) {
                                value = CLag.cubicLagrangian3D(X, Y, Z);
                            }
                        }
                    }

                    destImage.set(index++, value);
                }
            }
        }

        CLag.finalize();
        CLag = null;
        Preferences.debug("finished cubic Lagrangian");
    }

    /**
     * Transforms and resamples volume using cubic Lagrangian interpolation. This version used with color images
     *
     * @param  imgBuf   Input image array
     * @param  imgBuf2  Output image array
     * @param  kTM     Transformation matrix to be applied
     * @param  clip     If <code>true</code> clip output values to be within input range
     */
    private void transformCubicLagrangian3DC(float[] imgBuf, float[] imgBuf2, TransMatrix kTM, boolean clip) {
        AlgorithmCubicLagrangian CLag = new AlgorithmCubicLagrangian();
        int i, j, k;
        float X, Y, Z;
        float[] value = new float[4];
        int sliceSize;
        float imm, jmm, kmm;
        int[] inVolExtents = { iXdim, iYdim, iZdim };
        int mod = Math.max(1, oXdim / 50);
        int counter = 0; // used for progress bar

        sliceSize = iXdim * iYdim;

        int osliceSize = oXdim * oYdim;
        float i1, i2, i3, j1, j2, j3;
        float temp1, temp2, temp3;
        int temp4;
        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        float argbMax = 255.0f;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;

        if (srcImage.getType() == ModelStorageBase.ARGB) {
            argbMax = 255.0f;
        } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
            argbMax = 65535.0f;
        }

        CLag.setup3DCubicLagrangianC(imgBuf, inVolExtents, argbMax, clip);

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged((int) (((float) i / oXdim * 100) + 0.5));
            }

            imm = (float) i * oXres;
            i1 = (imm * T00) + T03;
            i2 = (imm * T10) + T13;
            i3 = (imm * T20) + T23;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {
                jmm = (float) j * oYres;
                j1 = jmm * T01;
                j2 = jmm * T11;
                j3 = jmm * T21;
                temp1 = i3 + j3;
                temp2 = i2 + j2;
                temp3 = i1 + j1;

                for (k = 0; (k < oZdim) && !threadStopped; k++) {

                    // convert to mm
                    value[0] = 0; // will remain zero if boundary conditions not met
                    value[1] = 0;
                    value[2] = 0;
                    value[3] = 0;
                    kmm = (float) k * oZres;

                    // transform i,j,k
                    X = (temp3 + (kmm * T02)) / iXres;

                    // convert back to pixels
                    if ((X >= 0) && (X < iXdim)) { // check bounds
                        Y = (temp2 + (kmm * T12)) / iYres;

                        if ((Y >= 0) && (Y < iYdim)) {
                            Z = (temp1 + (kmm * T22)) / iZres;

                            if ((Z >= 0) && (Z < iZdim)) {
                                value = CLag.cubicLagrangian3DC(X, Y, Z);
                            }
                        }
                    }

                    temp4 = 4 * (i + (j * oXdim) + (k * osliceSize));
                    imgBuf2[temp4] = value[0];
                    imgBuf2[temp4 + 1] = value[1];
                    imgBuf2[temp4 + 2] = value[2];
                    imgBuf2[temp4 + 3] = value[3];
                    counter++;
                }
            }
        }

        if (threadStopped) {
            return;
        }

        try {
            destImage.importData(0, imgBuf2, true);
        } catch (IOException error) {
            MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
        }

        CLag.finalize();
        CLag = null;
        Preferences.debug("finished cubic Lagrangian");

    }

    /**
     * Transforms and resamples volume using cubic Lagrangian interpolation Does a slice by slice cubic Lagrangian
     * interpolation on a 3 dimensional object.
     *
     * @param  imgBuf  Image array
     * @param  kTM    Transformation matrix to be applied
     * @param  clip    If <code>true</code> clip output values to be within input range
     */
    private void transformCubicLagrangian3Dim2D(float[] imgBuf, TransMatrix kTM, boolean clip) {
        AlgorithmCubicLagrangian CLag = new AlgorithmCubicLagrangian();
        int i, j, k;
        float X, Y;
        float value;
        int sliceSize;
        float imm, jmm;
        int[] inVolExtents = { iXdim, iYdim };

        sliceSize = iXdim * iYdim;

        int mod = Math.max(1, oZdim / 50);
        int counter = 0; // used for progress bar
        float temp1, temp2;
        float T00, T01, T02, T10, T11, T12;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        for (k = 0; (k < oZdim) && !threadStopped; k++) {

            if (((k % mod) == 0)) {
                fireProgressStateChanged((int) (((float) k / oZdim * 100) + 0.5));
            }

            CLag.setup2DCubicLagrangian(imgBuf, inVolExtents, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                imm = (float) i * oXres;
                temp1 = (imm * T00) + T02;
                temp2 = (imm * T10) + T12;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {

                    // convert to mm
                    value = (float) srcImage.getMin(); // will remain zero if boundary conditions not met
                    jmm = (float) j * oYres;

                    // transform i,j
                    X = (temp1 + (jmm * T01)) / iXres;

                    if ((X >= 0) && (X < iXdim)) { // check bounds
                        Y = (temp2 + (jmm * T11)) / iYres;

                        if ((Y >= 0) && (Y < iYdim)) {
                            value = CLag.cubicLagrangian2D(X, Y);
                        }
                    }

                    destImage.set(i, j, k, value);
                    counter++;
                }
            }

            if (k < (oZdim - 1)) {

                try {
                    srcImage.exportData((k + 1) * imgLength, imgLength, imgBuf);
                } catch (IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);


                    return;
                }
            } // end if (k < (oZdim - 1))
        } // end for k

        CLag.finalize();
        CLag = null;
        Preferences.debug("finished cubic Lagrangian");

    }

    /**
     * Transforms and resamples volume using cubic Lagrangian interpolation This version used with color images This
     * version performs a slice by slice algorithm on a 3 dimensional object.
     *
     * @param  imgBuf   Input image array
     * @param  imgBuf2  Output image array
     * @param  kTM     Transformation matrix to be applied
     * @param  clip     If <code>true</code> clip output values to be within input range
     */
    private void transformCubicLagrangian3Dim2DC(float[] imgBuf, float[] imgBuf2, TransMatrix kTM, boolean clip) {
        AlgorithmCubicLagrangian CLag = new AlgorithmCubicLagrangian();
        int i, j, k;
        float X, Y;
        float[] value = new float[4];
        int sliceSize;
        float imm, jmm;
        int[] inVolExtents = { iXdim, iYdim };

        sliceSize = iXdim * iYdim;

        int mod = Math.max(1, oZdim / 50);
        int counter = 0; // used for progress bar
        float temp1, temp2;
        int temp3;
        float T00, T01, T02, T10, T11, T12;
        float argbMax = 255.0f;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        for (k = 0; (k < oZdim) && !threadStopped; k++) {

            if (((k % mod) == 0)) {
                fireProgressStateChanged((int) (((float) k / oZdim * 100) + 0.5));
            }

            if (srcImage.getType() == ModelStorageBase.ARGB) {
                argbMax = 255.0f;
            } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
                argbMax = 65535.0f;
            }

            CLag.setup2DCubicLagrangianC(imgBuf, inVolExtents, argbMax, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                imm = (float) i * oXres;
                temp1 = (imm * T00) + T02;
                temp2 = (imm * T10) + T12;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {

                    // convert to mm
                    value[0] = 0; // will remain zero if boundary conditions not met
                    value[1] = 0;
                    value[2] = 0;
                    value[3] = 0;

                    jmm = (float) j * oYres;

                    // transform i,j
                    X = (temp1 + (jmm * T01)) / iXres;

                    if ((X >= 0) && (X < iXdim)) { // check bounds
                        Y = (temp2 + (jmm * T11)) / iYres;

                        if ((Y >= 0) && (Y < iYdim)) {
                            value = CLag.cubicLagrangian2DC(X, Y);
                        }
                    }

                    temp3 = 4 * (i + (j * oXdim));
                    imgBuf2[temp3] = value[0];
                    imgBuf2[temp3 + 1] = value[1];
                    imgBuf2[temp3 + 2] = value[2];
                    imgBuf2[temp3 + 3] = value[3];
                    counter++;
                } // for i
            } // for j

            try {
                destImage.importData(4 * k * oXdim * oYdim, imgBuf2, true);
            } catch (IOException error) {
                MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
            }

            if (k < (oZdim - 1)) {

                try {
                    srcImage.exportData((k + 1) * imgLength, imgLength, imgBuf);
                } catch (IOException error) {
                    displayError("Algorithm Transform: IOException Error on importData");
                    setCompleted(false);


                    return;
                }
            } // end if (k < (oZdim - 1))
        } // end for k

        CLag.finalize();
        CLag = null;
        Preferences.debug("finished cubic Lagrangian");

    }

    /**
     * Transforms and resamples 4 dimensional object using 3D cubic Lagrangian interpolation.
     *
     * @param  imgBuf  Image array
     * @param  kTM    Transformation matrix to be applied
     * @param  clip    if <code>true</code> clip output values to be within input range
     */
    private void transformCubicLagrangian4D(float[] imgBuf, TransMatrix kTM, boolean clip) {
        AlgorithmCubicLagrangian CLag = new AlgorithmCubicLagrangian();
        int i, j, k, l;
        float X, Y, Z;
        float value;
        float imm, jmm, kmm;
        int[] inVolExtents = { iXdim, iYdim, iZdim };
        int mod = Math.max(1, oTdim / 50);
        int counter = 0; // used for progress bar

        float i1, i2, i3, j1, j2, j3;
        float temp1, temp2, temp3;
        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {

            if (((l % mod) == 0)) {
                fireProgressStateChanged((int) (((float) l / oTdim * 100) + .5));
            }

            CLag.setup3DCubicLagrangian(imgBuf, inVolExtents, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                imm = (float) i * oXres;
                i1 = (imm * T00) + T03;
                i2 = (imm * T10) + T13;
                i3 = (imm * T20) + T23;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {
                    jmm = (float) j * oYres;
                    j1 = jmm * T01;
                    j2 = jmm * T11;
                    j3 = jmm * T21;
                    temp1 = i3 + j3;
                    temp2 = i2 + j2;
                    temp3 = i1 + j1;

                    for (k = 0; (k < oZdim) && !threadStopped; k++) {

                        // convert to mm
                        value = (float) srcImage.getMin(); // will remain zero if boundary conditions not met
                        kmm = (float) k * oZres;

                        // transform i,j,k
                        X = (temp3 + (kmm * T02)) / iXres;

                        if ((X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (kmm * T12)) / iYres;

                            if ((Y >= 0) && (Y < iYdim)) {
                                Z = (temp1 + (kmm * T22)) / iZres;

                                if ((Z >= 0) && (Z < iZdim)) {
                                    value = CLag.cubicLagrangian3D(X, Y, Z);
                                }
                            }
                        }

                        destImage.set(i, j, k, l, value);
                        counter++;
                    }
                }
            }

            if (l < (oTdim - 1)) {

                try {
                    srcImage.exportData((l + 1) * imgLength, imgLength, imgBuf);
                } catch (IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);


                    return;
                }
            } // end if (l < (oTdim - 1))
        } // for l

        CLag.finalize();
        CLag = null;
        Preferences.debug("finished cubic Lagrangian");

    }
    
    /**
     * Transforms and resamples 4 dimensional object using 3D cubic Lagrangian interpolation.
     *
     * @param  imgBuf  Image array
     * @param  imgBuffer2
     * @param  kTM    Transformation matrix to be applied
     * @param  clip    if <code>true</code> clip output values to be within input range
     */
    private void transformCubicLagrangian4DC(float[] imgBuf, float imgBuffer2[], TransMatrix kTM, boolean clip) {
        AlgorithmCubicLagrangian CLag = new AlgorithmCubicLagrangian();
        int i, j, k, l;
        float X, Y, Z;
        float value[] = new float[4];
        int temp4;
        float imm, jmm, kmm;
        int[] inVolExtents = { iXdim, iYdim, iZdim };
        int oSliceSize;
        int oVolSize;
        int mod = Math.max(1, oTdim / 50);
        int counter = 0; // used for progress bar

        float i1, i2, i3, j1, j2, j3;
        float temp1, temp2, temp3;
        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        float argbMax = 255.0f;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;
        
        oSliceSize = oXdim * oYdim;
        oVolSize = oSliceSize * oZdim;
        
        if (srcImage.getType() == ModelStorageBase.ARGB) {
            argbMax = 255.0f;
        }
        else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
            argbMax = 65535.0f;
        }

        for (l = 0; (l < oTdim) && !threadStopped; l++) {

            if (((l % mod) == 0)) {
                fireProgressStateChanged((int) (((float) l / oTdim * 100) + .5));
            }

            CLag.setup3DCubicLagrangianC(imgBuf, inVolExtents, argbMax, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                imm = (float) i * oXres;
                i1 = (imm * T00) + T03;
                i2 = (imm * T10) + T13;
                i3 = (imm * T20) + T23;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {
                    jmm = (float) j * oYres;
                    j1 = jmm * T01;
                    j2 = jmm * T11;
                    j3 = jmm * T21;
                    temp1 = i3 + j3;
                    temp2 = i2 + j2;
                    temp3 = i1 + j1;

                    for (k = 0; (k < oZdim) && !threadStopped; k++) {

                        // convert to mm
                        value[0] = 0; // will remain zero if boundary conditions not met
                        value[1] = 0;
                        value[2] = 0;
                        value[3] = 0;
                        kmm = (float) k * oZres;

                        // transform i,j,k
                        X = (temp3 + (kmm * T02)) / iXres;

                        if ((X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (kmm * T12)) / iYres;

                            if ((Y >= 0) && (Y < iYdim)) {
                                Z = (temp1 + (kmm * T22)) / iZres;

                                if ((Z >= 0) && (Z < iZdim)) {
                                    value = CLag.cubicLagrangian3DC(X, Y, Z);
                                }
                            }
                        }

                        temp4 = 4 *(i + (j * oXdim) + (k * oSliceSize));
                        imgBuffer2[temp4] = value[0];
                        imgBuffer2[temp4 + 1] = value[1];
                        imgBuffer2[temp4 + 2] = value[2];
                        imgBuffer2[temp4 + 3] = value[3];
                        counter++;
                    }
                }
            }
            
            try {
                destImage.importData(4 * l * oVolSize, imgBuffer2, false);
            }
            catch (IOException error) {
                MipavUtil.displayError("AlgorithmTransform: IOException on destImage.importData");
            }

            if (l < (oTdim - 1)) {

                try {
                    srcImage.exportData((l + 1) * imgLength, imgLength, imgBuf);
                } catch (IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);


                    return;
                }
            } // end if (l < (oTdim - 1))
        } // for l

        destImage.calcMinMax();
        CLag.finalize();
        CLag = null;
        Preferences.debug("finished cubic Lagrangian");

    }

    /**
     * Transforms and resamples volume using cubic Lagrangian interpolation Does a slice by slice cubic Lagrangian
     * interpolation on a 4 dimensional object.
     *
     * @param  imgBuf  Image array
     * @param  kTM    Transformation matrix to be applied
     * @param  clip    If <code>true</code> clip output values to be within input range
     */
    private void transformCubicLagrangian4Dim2D(float[] imgBuf, TransMatrix kTM, boolean clip) {
        AlgorithmCubicLagrangian CLag = new AlgorithmCubicLagrangian();
        int i, j, k, l;
        float X, Y;
        float value;
        int sliceSize;
        float imm, jmm;
        int[] inVolExtents = { iXdim, iYdim };

        sliceSize = iXdim * iYdim;

        int volSize = sliceSize * iZdim;
        int counter = 0; // used for progress bar
        float temp1, temp2;
        float T00, T01, T02, T10, T11, T12;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged((int) (((float) l / oTdim * 100) + 0.5));

            for (k = 0; (k < oZdim) && !threadStopped; k++) {
                CLag.setup2DCubicLagrangian(imgBuf, inVolExtents, clip);

                for (i = 0; (i < oXdim) && !threadStopped; i++) {
                    imm = (float) i * oXres;
                    temp1 = (imm * T00) + T02;
                    temp2 = (imm * T10) + T12;

                    for (j = 0; (j < oYdim) && !threadStopped; j++) {

                        // convert to mm
                        value = (float) srcImage.getMin(); // will remain zero if boundary conditions not met
                        jmm = (float) j * oYres;

                        // transform i,j
                        X = (temp1 + (jmm * T01)) / iXres;

                        if ((X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (jmm * T11)) / iYres;

                            if ((Y >= 0) && (Y < iYdim)) {
                                value = CLag.cubicLagrangian2D(X, Y);
                            }
                        }

                        destImage.set(i, j, k, l, value);
                        counter++;
                    }
                }

                if ((k < (oZdim - 1)) || (l < (oTdim - 1))) {

                    try {
                        srcImage.exportData((l * oZdim * imgLength) + ((k + 1) * imgLength), imgLength, imgBuf);
                    } catch (IOException error) {
                        displayError("Algorithm Transform: Image(s) locked");
                        setCompleted(false);


                        return;
                    }
                } // end if ((k < (oZdim - 1))|| (l < (oTdim - 1)))
            } // end for k
        } // end for l

        CLag.finalize();
        CLag = null;
        Preferences.debug("finished cubic Lagrangian");
    }

    /**
     * Transforms and resamples volume using cubic Lagrangian interpolation This version used with color images This
     * version performs a slice by slice algorithm on a 4 dimensional object.
     *
     * @param  imgBuf   Input image array
     * @param  imgBuf2  Output image array
     * @param  kTM     Transformation matrix to be applied
     * @param  clip     If <code>true</code> clip output values to be within input range
     */
    private void transformCubicLagrangian4Dim2DC(float[] imgBuf, float[] imgBuf2, TransMatrix kTM, boolean clip) {
        AlgorithmCubicLagrangian CLag = new AlgorithmCubicLagrangian();
        int i, j, k, l;
        float X, Y;
        float[] value = new float[4];
        float imm, jmm;
        int[] inVolExtents = { iXdim, iYdim };

        int counter = 0; // used for progress bar
        float temp1, temp2;
        int temp3;
        float T00, T01, T02, T10, T11, T12;
        float argbMax = 255.0f;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged((int) (((float) l / oTdim * 100) + 0.5));

            for (k = 0; (k < oZdim) && !threadStopped; k++) {

                if (srcImage.getType() == ModelStorageBase.ARGB) {
                    argbMax = 255.0f;
                } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
                    argbMax = 65535.0f;
                }

                CLag.setup2DCubicLagrangianC(imgBuf, inVolExtents, argbMax, clip);

                for (i = 0; (i < oXdim) && !threadStopped; i++) {
                    imm = (float) i * oXres;
                    temp1 = (imm * T00) + T02;
                    temp2 = (imm * T10) + T12;

                    for (j = 0; (j < oYdim) && !threadStopped; j++) {

                        // convert to mm
                        value[0] = 0; // will remain zero if boundary conditions not met
                        value[1] = 0;
                        value[2] = 0;
                        value[3] = 0;

                        jmm = (float) j * oYres;

                        // transform i,j
                        X = (temp1 + (jmm * T01)) / iXres;

                        if ((X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (jmm * T11)) / iYres;

                            if ((Y >= 0) && (Y < iYdim)) {
                                value = CLag.cubicLagrangian2DC(X, Y);
                            }
                        }

                        temp3 = 4 * (i + (j * oXdim));
                        imgBuf2[temp3] = value[0];
                        imgBuf2[temp3 + 1] = value[1];
                        imgBuf2[temp3 + 2] = value[2];
                        imgBuf2[temp3 + 3] = value[3];
                        counter++;
                    } // for i
                } // for j

                try {
                    destImage.importData((4 * l * oXdim * oYdim * oZdim) + (4 * k * oXdim * oYdim), imgBuf2, true);
                } catch (IOException error) {
                    MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
                }

                if ((k < (oZdim - 1)) || (l < (oTdim - 1))) {

                    try {
                        srcImage.exportData((l * oZdim * imgLength) + ((k + 1) * imgLength), imgLength, imgBuf);
                    } catch (IOException error) {
                        displayError("Algorithm Transform: IOException Error on importData");
                        setCompleted(false);


                        return;
                    }
                } // end if ((k < (oZdim - 1))|| (l < (oTdim - 1)))
            } // end for k
        } // end for l

        CLag.finalize();
        CLag = null;
        Preferences.debug("finished cubic Lagrangian");
    }

    /**
     * Transforms and resamples volume using heptic Lagrangian interpolation.
     *
     * @param  imgBuf  Image array
     * @param  kTM    Transformation matrix to be applied
     * @param  clip    If <code>true</code> clip output values to be within input range
     */
    private void transformHepticLagrangian2D(float[] imgBuf, TransMatrix kTM, boolean clip) {
        AlgorithmHepticLagrangian HLag = new AlgorithmHepticLagrangian();
        int i, j;
        float X, Y;
        float value;
        float imm, jmm;
        int[] inVolExtents = { iXdim, iYdim };
        int mod = Math.max(1, oXdim / 50);
        int counter = 0; // used for progress bar
        float temp1, temp2;
        float T00, T01, T02, T10, T11, T12;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        HLag.setup2DHepticLagrangian(imgBuf, inVolExtents, clip);

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged((int) (((float) i / oXdim * 100) + 0.5));
            }

            imm = (float) i * oXres;
            temp1 = (imm * T00) + T02;
            temp2 = (imm * T10) + T12;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                // convert to mm
                value = (float) srcImage.getMin(); // will remain zero if boundary conditions not met

                jmm = (float) j * oYres;

                // transform i,j
                X = (temp1 + (jmm * T01)) / iXres;

                if ((X >= 0) && (X < iXdim)) { // check bounds
                    Y = (temp2 + (jmm * T11)) / iYres;

                    if ((Y >= 0) && (Y < iYdim)) {
                        value = HLag.hepticLagrangian2D(X, Y);
                    }
                }

                destImage.set(i, j, value);
                counter++;
            }
        }

        HLag.finalize();
        HLag = null;
        Preferences.debug("finished heptic Lagrangian");
    }

    /**
     * Transforms and resamples volume using heptic Lagrangian interpolation This version used with color images.
     *
     * @param  imgBuf   Input image array
     * @param  imgBuf2  Output image array
     * @param  kTM     Transformation matrix to be applied
     * @param  clip     If <code>true</code> clip output values to be within input range
     */
    private void transformHepticLagrangian2DC(float[] imgBuf, float[] imgBuf2, TransMatrix kTM, boolean clip) {
        AlgorithmHepticLagrangian HLag = new AlgorithmHepticLagrangian();
        int i, j;
        float X, Y;
        float[] value = new float[4];
        float imm, jmm;
        int[] inVolExtents = { iXdim, iYdim };
        int mod = Math.max(1, oXdim / 50);
        int counter = 0; // used for progress bar
        float temp1, temp2;
        int temp3;
        float T00, T01, T02, T10, T11, T12;
        float argbMax = 255.0f;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        if (srcImage.getType() == ModelStorageBase.ARGB) {
            argbMax = 255.0f;
        } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
            argbMax = 65535.0f;
        }

        HLag.setup2DHepticLagrangianC(imgBuf, inVolExtents, argbMax, clip);

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged((int) (((float) i / oXdim * 100) + 0.5));
            }

            imm = (float) i * oXres;
            temp1 = (imm * T00) + T02;
            temp2 = (imm * T10) + T12;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                // convert to mm
                value[0] = 0; // will remain zero if boundary conditions not met
                value[1] = 0;
                value[2] = 0;
                value[3] = 0;

                jmm = (float) j * oYres;

                // transform i,j
                X = (temp1 + (jmm * T01)) / iXres;

                if ((X >= 0) && (X < iXdim)) { // check bounds
                    Y = (temp2 + (jmm * T11)) / iYres;

                    if ((Y >= 0) && (Y < iYdim)) {
                        value = HLag.hepticLagrangian2DC(X, Y);
                    }
                }

                temp3 = 4 * (i + (j * oXdim));
                imgBuf2[temp3] = value[0];
                imgBuf2[temp3 + 1] = value[1];
                imgBuf2[temp3 + 2] = value[2];
                imgBuf2[temp3 + 3] = value[3];
                counter++;
            }
        }

        if (threadStopped) {
            return;
        }

        try {
            destImage.importData(0, imgBuf2, true);
        } catch (IOException error) {
            MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
        }

        HLag.finalize();
        HLag = null;
        Preferences.debug("finished heptic Lagrangian");

    }

    /**
     * Transforms and resamples volume using heptic Lagrangian interpolation.
     *
     * @param  imgBuf  Image array
     * @param  kTM    Transformation matrix to be applied
     * @param  clip    If <code>true</code> clip output values to be within input range
     */
    private void transformHepticLagrangian3D(float[] imgBuf, TransMatrix kTM, boolean clip) {
        AlgorithmHepticLagrangian HLag = new AlgorithmHepticLagrangian();
        int i, j, k;
        float X, Y, Z;
        float value;
        int sliceSize;
        float imm, jmm, kmm;
        int[] inVolExtents = { iXdim, iYdim, iZdim };
        int mod = Math.max(1, oXdim / 50);
        int counter = 0; // used for progress bar

        sliceSize = iXdim * iYdim;

        float i1, i2, i3, j1, j2, j3;
        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;

        HLag.setup3DHepticLagrangian(imgBuf, inVolExtents, clip);

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged((int) (((float) i / oXdim * 100) + 0.5));
            }

            imm = (float) i * oXres;
            i1 = (imm * T00) + T03;
            i2 = (imm * T10) + T13;
            i3 = (imm * T20) + T23;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {
                jmm = (float) j * oYres;
                j1 = (jmm * T01) + i1;
                j2 = (jmm * T11) + i2;
                j3 = (jmm * T21) + i3;

                for (k = 0; (k < oZdim) && !threadStopped; k++) {

                    // convert to mm
                    value = (float) srcImage.getMin(); // will remain zero if boundary conditions not met
                    kmm = (float) k * oZres;

                    // transform i,j,k
                    X = (j1 + (kmm * T02)) / iXres;

                    if ((X >= 0) && (X < iXdim)) { // check bounds
                        Y = (j2 + (kmm * T12)) / iYres;

                        if ((Y >= 0) && (Y < iYdim)) {
                            Z = (j3 + (kmm * T22)) / iZres;

                            if ((Z >= 0) && (Z < iZdim)) {
                                value = HLag.hepticLagrangian3D(X, Y, Z);
                            }
                        }
                    }

                    destImage.set(i, j, k, value);
                    counter++;
                }
            }
        }

        HLag.finalize();
        HLag = null;
        Preferences.debug("finished heptic Lagrangian");
    }

    /**
     * Transforms and resamples volume using heptic Lagrangian interpolation This version used with color images.
     *
     * @param  imgBuf   Input image array
     * @param  imgBuf2  Output image array
     * @param  kTM     Transformation matrix to be applied
     * @param  clip     If <code>true</code> clip output values to be within input range
     */
    private void transformHepticLagrangian3DC(float[] imgBuf, float[] imgBuf2, TransMatrix kTM, boolean clip) {
        AlgorithmHepticLagrangian HLag = new AlgorithmHepticLagrangian();
        int i, j, k;
        float X, Y, Z;
        float[] value = new float[4];
        int sliceSize;
        float imm, jmm, kmm;
        int[] inVolExtents = { iXdim, iYdim, iZdim };
        int mod = Math.max(1, oXdim / 50);
        int counter = 0; // used for progress bar

        sliceSize = iXdim * iYdim;

        int osliceSize = oXdim * oYdim;
        float i1, i2, i3, j1, j2, j3;
        float temp1, temp2, temp3;
        int temp4;
        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        float argbMax = 255.0f;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;

        if (srcImage.getType() == ModelStorageBase.ARGB) {
            argbMax = 255.0f;
        } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
            argbMax = 65535.0f;
        }

        HLag.setup3DHepticLagrangianC(imgBuf, inVolExtents, argbMax, clip);

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged((int) (((float) i / oXdim * 100) + 0.5));
            }

            imm = (float) i * oXres;
            i1 = (imm * T00) + T03;
            i2 = (imm * T10) + T13;
            i3 = (imm * T20) + T23;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {
                jmm = (float) j * oYres;
                j1 = jmm * T01;
                j2 = jmm * T11;
                j3 = jmm * T21;
                temp1 = i3 + j3;
                temp2 = i2 + j2;
                temp3 = i1 + j1;

                for (k = 0; (k < oZdim) && !threadStopped; k++) {

                    // convert to mm
                    value[0] = 0; // will remain zero if boundary conditions not met
                    value[1] = 0;
                    value[2] = 0;
                    value[3] = 0;
                    kmm = (float) k * oZres;

                    // transform i,j,k
                    X = (temp3 + (kmm * T02)) / iXres;

                    // convert back to pixels
                    if ((X >= 0) && (X < iXdim)) { // check bounds
                        Y = (temp2 + (kmm * T12)) / iYres;

                        if ((Y >= 0) && (Y < iYdim)) {
                            Z = (temp1 + (kmm * T22)) / iZres;

                            if ((Z >= 0) && (Z < iZdim)) {
                                value = HLag.hepticLagrangian3DC(X, Y, Z);
                            }
                        }
                    }

                    temp4 = 4 * (i + (j * oXdim) + (k * osliceSize));
                    imgBuf2[temp4] = value[0];
                    imgBuf2[temp4 + 1] = value[1];
                    imgBuf2[temp4 + 2] = value[2];
                    imgBuf2[temp4 + 3] = value[3];
                    counter++;
                }
            }
        }

        if (threadStopped) {
            return;
        }

        try {
            destImage.importData(0, imgBuf2, true);
        } catch (IOException error) {
            MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
        }

        HLag.finalize();
        HLag = null;
        Preferences.debug("finished heptic Lagrangian");

    }

    /**
     * Transforms and resamples volume using heptic Lagrangian interpolation Does a slice by slice heptic Lagrangian
     * interpolation on a 3 dimensional object.
     *
     * @param  imgBuf  Image array
     * @param  kTM    Transformation matrix to be applied
     * @param  clip    If <code>true</code> clip output values to be within input range
     */
    private void transformHepticLagrangian3Dim2D(float[] imgBuf, TransMatrix kTM, boolean clip) {
        AlgorithmHepticLagrangian HLag = new AlgorithmHepticLagrangian();
        int i, j, k;
        float X, Y;
        float value;
        int sliceSize;
        float imm, jmm;
        int[] inVolExtents = { iXdim, iYdim };

        sliceSize = iXdim * iYdim;

        int mod = Math.max(1, oZdim / 50);
        int counter = 0; // used for progress bar
        float temp1, temp2;
        float T00, T01, T02, T10, T11, T12;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        for (k = 0; (k < oZdim) && !threadStopped; k++) {

            if (((k % mod) == 0)) {
                fireProgressStateChanged((int) (((float) k / oZdim * 100) + 0.5));
            }

            HLag.setup2DHepticLagrangian(imgBuf, inVolExtents, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                imm = (float) i * oXres;
                temp1 = (imm * T00) + T02;
                temp2 = (imm * T10) + T12;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {

                    // convert to mm
                    value = (float) srcImage.getMin(); // will remain zero if boundary conditions not met
                    jmm = (float) j * oYres;

                    // transform i,j
                    X = (temp1 + (jmm * T01)) / iXres;

                    if ((X >= 0) && (X < iXdim)) { // check bounds
                        Y = (temp2 + (jmm * T11)) / iYres;

                        if ((Y >= 0) && (Y < iYdim)) {
                            value = HLag.hepticLagrangian2D(X, Y);
                        }
                    }

                    destImage.set(i, j, k, value);
                    counter++;
                }
            }

            if (k < (oZdim - 1)) {

                try {
                    srcImage.exportData((k + 1) * imgLength, imgLength, imgBuf);
                } catch (IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);


                    return;
                }
            } // end if (k < (oZdim - 1))
        } // end for k

        HLag.finalize();
        HLag = null;
        Preferences.debug("finished heptic Lagrangian");
    }

    /**
     * Transforms and resamples volume using heptic Lagrangian interpolation This version used with color images This
     * version performs a slice by slice algorithm on a 3 dimensional object.
     *
     * @param  imgBuf   Input image array
     * @param  imgBuf2  Output image array
     * @param  kTM     Transformation matrix to be applied
     * @param  clip     If <code>true</code> clip output values to be within input range
     */
    private void transformHepticLagrangian3Dim2DC(float[] imgBuf, float[] imgBuf2, TransMatrix kTM, boolean clip) {
        AlgorithmHepticLagrangian HLag = new AlgorithmHepticLagrangian();
        int i, j, k;
        float X, Y;
        float[] value = new float[4];
        int sliceSize;
        float imm, jmm;
        int[] inVolExtents = { iXdim, iYdim };

        sliceSize = iXdim * iYdim;

        int mod = Math.max(1, oZdim / 50);
        int counter = 0; // used for progress bar
        float temp1, temp2;
        int temp3;
        float T00, T01, T02, T10, T11, T12;
        float argbMax = 255.0f;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        for (k = 0; (k < oZdim) && !threadStopped; k++) {

            if (((k % mod) == 0)) {
                fireProgressStateChanged((int) (((float) k / oZdim * 100) + 0.5));
            }

            if (srcImage.getType() == ModelStorageBase.ARGB) {
                argbMax = 255.0f;
            } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
                argbMax = 65535.0f;
            }

            HLag.setup2DHepticLagrangianC(imgBuf, inVolExtents, argbMax, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                imm = (float) i * oXres;
                temp1 = (imm * T00) + T02;
                temp2 = (imm * T10) + T12;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {

                    // convert to mm
                    value[0] = 0; // will remain zero if boundary conditions not met
                    value[1] = 0;
                    value[2] = 0;
                    value[3] = 0;

                    jmm = (float) j * oYres;

                    // transform i,j
                    X = (temp1 + (jmm * T01)) / iXres;

                    if ((X >= 0) && (X < iXdim)) { // check bounds
                        Y = (temp2 + (jmm * T11)) / iYres;

                        if ((Y >= 0) && (Y < iYdim)) {
                            value = HLag.hepticLagrangian2DC(X, Y);
                        }
                    }

                    temp3 = 4 * (i + (j * oXdim));
                    imgBuf2[temp3] = value[0];
                    imgBuf2[temp3 + 1] = value[1];
                    imgBuf2[temp3 + 2] = value[2];
                    imgBuf2[temp3 + 3] = value[3];
                    counter++;
                } // for i
            } // for j

            try {
                destImage.importData(4 * k * oXdim * oYdim, imgBuf2, true);
            } catch (IOException error) {
                MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
            }

            if (k < (oZdim - 1)) {

                try {
                    srcImage.exportData((k + 1) * imgLength, imgLength, imgBuf);
                } catch (IOException error) {
                    displayError("Algorithm Transform: IOException Error on importData");
                    setCompleted(false);


                    return;
                }
            } // end if (k < (oZdim - 1))
        } // end for k

        HLag.finalize();
        HLag = null;
        Preferences.debug("finished heptic Lagrangian");
    }

    /**
     * transforms and resamples 4 dimensional object using 3D heptic Lagrangian interpolation.
     *
     * @param  imgBuf  image array
     * @param  kTM    transformation matrix to be applied
     * @param  clip    if true clip output values to be within input range
     */
    private void transformHepticLagrangian4D(float[] imgBuf, TransMatrix kTM, boolean clip) {
        AlgorithmHepticLagrangian HLag = new AlgorithmHepticLagrangian();
        int i, j, k, l;
        float X, Y, Z;
        float value;
        float imm, jmm, kmm;
        int[] inVolExtents = { iXdim, iYdim, iZdim };
        int mod = Math.max(1, oTdim / 50);
        int counter = 0; // used for progress bar

        float i1, i2, i3, j1, j2, j3;
        float temp1, temp2, temp3;
        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {

            if (((l % mod) == 0)) {
                fireProgressStateChanged((int) (((float) l / oTdim * 100) + .5));
            }

            HLag.setup3DHepticLagrangian(imgBuf, inVolExtents, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                imm = (float) i * oXres;
                i1 = (imm * T00) + T03;
                i2 = (imm * T10) + T13;
                i3 = (imm * T20) + T23;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {
                    jmm = (float) j * oYres;
                    j1 = jmm * T01;
                    j2 = jmm * T11;
                    j3 = jmm * T21;
                    temp1 = i3 + j3;
                    temp2 = i2 + j2;
                    temp3 = i1 + j1;

                    for (k = 0; (k < oZdim) && !threadStopped; k++) {

                        // convert to mm
                        value = (float) srcImage.getMin(); // will remain zero if boundary conditions not met
                        kmm = (float) k * oZres;

                        // transform i,j,k
                        X = (temp3 + (kmm * T02)) / iXres;

                        if ((X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (kmm * T12)) / iYres;

                            if ((Y >= 0) && (Y < iYdim)) {
                                Z = (temp1 + (kmm * T22)) / iZres;

                                if ((Z >= 0) && (Z < iZdim)) {
                                    value = HLag.hepticLagrangian3D(X, Y, Z);
                                }
                            }
                        }

                        destImage.set(i, j, k, l, value);
                        counter++;
                    }
                }
            }

            if (l < (oTdim - 1)) {

                try {
                    srcImage.exportData((l + 1) * imgLength, imgLength, imgBuf);
                } catch (IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);


                    return;
                }
            } // end if (l < (oTdim - 1))
        } // for l

        HLag.finalize();
        HLag = null;
        Preferences.debug("finished heptic Lagrangian");
    }
    
    /**
     * transforms and resamples 4 dimensional object using 3D heptic Lagrangian interpolation.
     *
     * @param  imgBuf  image array
     * @param  imgBuffer2
     * @param  kTM    transformation matrix to be applied
     * @param  clip    if true clip output values to be within input range
     */
    private void transformHepticLagrangian4DC(float[] imgBuf, float imgBuffer2[], TransMatrix kTM, boolean clip) {
        AlgorithmHepticLagrangian HLag = new AlgorithmHepticLagrangian();
        int i, j, k, l;
        float X, Y, Z;
        float value[] = new float[4];
        int temp4;
        float imm, jmm, kmm;
        int[] inVolExtents = { iXdim, iYdim, iZdim };
        int oSliceSize;
        int oVolSize;
        int mod = Math.max(1, oTdim / 50);
        int counter = 0; // used for progress bar

        float i1, i2, i3, j1, j2, j3;
        float temp1, temp2, temp3;
        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        
        float argbMax = 255.0f;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;
        
        oSliceSize = oXdim * oYdim;
        oVolSize = oSliceSize * oZdim;
        
        if (srcImage.getType() == ModelStorageBase.ARGB) {
            argbMax = 255.0f;
        }
        else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
            argbMax = 65535.0f;
        }

        for (l = 0; (l < oTdim) && !threadStopped; l++) {

            if (((l % mod) == 0)) {
                fireProgressStateChanged((int) (((float) l / oTdim * 100) + .5));
            }

            HLag.setup3DHepticLagrangianC(imgBuf, inVolExtents, argbMax, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                imm = (float) i * oXres;
                i1 = (imm * T00) + T03;
                i2 = (imm * T10) + T13;
                i3 = (imm * T20) + T23;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {
                    jmm = (float) j * oYres;
                    j1 = jmm * T01;
                    j2 = jmm * T11;
                    j3 = jmm * T21;
                    temp1 = i3 + j3;
                    temp2 = i2 + j2;
                    temp3 = i1 + j1;

                    for (k = 0; (k < oZdim) && !threadStopped; k++) {

                        // convert to mm
                        value[0] = 0; // will remain zero if boundary conditions not met
                        value[1] = 0;
                        value[2] = 0;
                        value[3] = 0;
                        kmm = (float) k * oZres;

                        // transform i,j,k
                        X = (temp3 + (kmm * T02)) / iXres;

                        if ((X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (kmm * T12)) / iYres;

                            if ((Y >= 0) && (Y < iYdim)) {
                                Z = (temp1 + (kmm * T22)) / iZres;

                                if ((Z >= 0) && (Z < iZdim)) {
                                    value = HLag.hepticLagrangian3DC(X, Y, Z);
                                }
                            }
                        }

                        temp4 = 4 *(i + (j * oXdim) + (k * oSliceSize));
                        imgBuffer2[temp4] = value[0];
                        imgBuffer2[temp4 + 1] = value[1];
                        imgBuffer2[temp4 + 2] = value[2];
                        imgBuffer2[temp4 + 3] = value[3];
                        counter++;
                    }
                }
            }
            
            try {
                destImage.importData(4 * l * oVolSize, imgBuffer2, false);
            }
            catch (IOException error) {
                MipavUtil.displayError("AlgorithmTransform: IOException on destImage.importData");
            }

            if (l < (oTdim - 1)) {

                try {
                    srcImage.exportData((l + 1) * imgLength, imgLength, imgBuf);
                } catch (IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);


                    return;
                }
            } // end if (l < (oTdim - 1))
        } // for l

        destImage.calcMinMax();
        HLag.finalize();
        HLag = null;
        Preferences.debug("finished heptic Lagrangian");
    }

    /**
     * Transforms and resamples volume using heptic Lagrangian interpolation Does a slice by slice heptic Lagrangian
     * interpolation on a 4 dimensional object.
     *
     * @param  imgBuf  Image array
     * @param  kTM    Transformation matrix to be applied
     * @param  clip    If <code>true</code> clip output values to be within input range
     */
    private void transformHepticLagrangian4Dim2D(float[] imgBuf, TransMatrix kTM, boolean clip) {
        AlgorithmHepticLagrangian HLag = new AlgorithmHepticLagrangian();
        int i, j, k, l;
        float X, Y;
        float value;
        int sliceSize;
        float imm, jmm;
        int[] inVolExtents = { iXdim, iYdim };

        sliceSize = iXdim * iYdim;

        int volSize = sliceSize * iZdim;
        int counter = 0; // used for progress bar
        float temp1, temp2;
        float T00, T01, T02, T10, T11, T12;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged((int) (((float) l / oTdim * 100) + 0.5));

            for (k = 0; (k < oZdim) && !threadStopped; k++) {
                HLag.setup2DHepticLagrangian(imgBuf, inVolExtents, clip);

                for (i = 0; (i < oXdim) && !threadStopped; i++) {
                    imm = (float) i * oXres;
                    temp1 = (imm * T00) + T02;
                    temp2 = (imm * T10) + T12;

                    for (j = 0; (j < oYdim) && !threadStopped; j++) {

                        // convert to mm
                        value = (float) srcImage.getMin(); // will remain zero if boundary conditions not met
                        jmm = (float) j * oYres;

                        // transform i,j
                        X = (temp1 + (jmm * T01)) / iXres;

                        if ((X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (jmm * T11)) / iYres;

                            if ((Y >= 0) && (Y < iYdim)) {
                                value = HLag.hepticLagrangian2D(X, Y);
                            }
                        }

                        destImage.set(i, j, k, l, value);
                        counter++;
                    }
                }

                if ((k < (oZdim - 1)) || (l < (oTdim - 1))) {

                    try {
                        srcImage.exportData((l * oZdim * imgLength) + ((k + 1) * imgLength), imgLength, imgBuf);
                    } catch (IOException error) {
                        displayError("Algorithm Transform: Image(s) locked");
                        setCompleted(false);


                        return;
                    }
                } // end if ((k < (oZdim - 1))|| (l < (oTdim - 1)))
            } // end for k
        } // end for l

        HLag.finalize();
        HLag = null;
        Preferences.debug("finished heptic Lagrangian");
    }

    /**
     * Transforms and resamples volume using heptic Lagrangian interpolation This version used with color images This
     * version performs a slice by slice algorithm on a 4 dimensional object.
     *
     * @param  imgBuf   Input image array
     * @param  imgBuf2  Output image array
     * @param  kTM     Transformation matrix to be applied
     * @param  clip     If <code>true</code> clip output values to be within input range
     */
    private void transformHepticLagrangian4Dim2DC(float[] imgBuf, float[] imgBuf2, TransMatrix kTM, boolean clip) {
        AlgorithmHepticLagrangian HLag = new AlgorithmHepticLagrangian();
        int i, j, k, l;
        float X, Y;
        float[] value = new float[4];
        float imm, jmm;
        int[] inVolExtents = { iXdim, iYdim };

        int counter = 0; // used for progress bar
        float temp1, temp2;
        int temp3;
        float T00, T01, T02, T10, T11, T12;
        float argbMax = 255.0f;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged((int) (((float) l / oTdim * 100) + 0.5));

            for (k = 0; (k < oZdim) && !threadStopped; k++) {

                if (srcImage.getType() == ModelStorageBase.ARGB) {
                    argbMax = 255.0f;
                } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
                    argbMax = 65535.0f;
                }

                HLag.setup2DHepticLagrangianC(imgBuf, inVolExtents, argbMax, clip);

                for (i = 0; (i < oXdim) && !threadStopped; i++) {
                    imm = (float) i * oXres;
                    temp1 = (imm * T00) + T02;
                    temp2 = (imm * T10) + T12;

                    for (j = 0; (j < oYdim) && !threadStopped; j++) {

                        // convert to mm
                        value[0] = 0; // will remain zero if boundary conditions not met
                        value[1] = 0;
                        value[2] = 0;
                        value[3] = 0;

                        jmm = (float) j * oYres;

                        // transform i,j
                        X = (temp1 + (jmm * T01)) / iXres;

                        if ((X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (jmm * T11)) / iYres;

                            if ((Y >= 0) && (Y < iYdim)) {
                                value = HLag.hepticLagrangian2DC(X, Y);
                            }
                        }

                        temp3 = 4 * (i + (j * oXdim));
                        imgBuf2[temp3] = value[0];
                        imgBuf2[temp3 + 1] = value[1];
                        imgBuf2[temp3 + 2] = value[2];
                        imgBuf2[temp3 + 3] = value[3];
                        counter++;
                    } // for i
                } // for j

                try {
                    destImage.importData((4 * l * oXdim * oYdim * oZdim) + (4 * k * oXdim * oYdim), imgBuf2, true);
                } catch (IOException error) {
                    MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
                }

                if ((k < (oZdim - 1)) || (l < (oTdim - 1))) {

                    try {
                        srcImage.exportData((l * oZdim * imgLength) + ((k + 1) * imgLength), imgLength, imgBuf);
                    } catch (IOException error) {
                        displayError("Algorithm Transform: IOException Error on importData");
                        setCompleted(false);


                        return;
                    }
                } // end if ((k < (oZdim - 1))|| (l < (oTdim - 1)))
            } // end for k
        } // end for l

        HLag.finalize();
        HLag = null;
        Preferences.debug("finished heptic Lagrangian");
    }

    /**
     * Transforms and resamples volume using nearest neighbor interpolation.
     *
     * @param  imgBuf  image array
     * @param  kTM    transformation matrix to be applied
     */
    private void transformNearestNeighbor2D(float[] imgBuf, TransMatrix kTM) {
        int i, j;
        float X, Y;
        int roundX, roundY;
        int xOffset, yOffset;
        float value;
        float imm, jmm;
        int mod = Math.max(1, oXdim / 50);
        float T00, T01, T02, T10, T11, T12;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged((int) (((float) i / oXdim * 100) + 0.5));
            }

            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                // transform i,j
                imm = (float) i * oXres;
                jmm = (float) j * oYres;
                X = (imm * T00) + (jmm * T01) + T02;
                Y = (imm * T10) + (jmm * T11) + T12;

                // set intensity of i,j to new transformed coordinate if
                // x,y is w/in dimensions of image
                X = X / iXres;
                Y = Y / iYres;

                roundX = (int) (X + 0.5f);
                roundY = (int) (Y + 0.5f);

                if ((X < -0.5) || (X >= iXdim) || (Y < -0.5) || (Y >= iYdim)) {
                    value = (float) srcImage.getMin();
                } else {
                    xOffset = Math.min(roundX, iXdim -1);
                    yOffset = Math.min(roundY, iYdim - 1) * iXdim;
                    value = imgBuf[xOffset + yOffset];
                }

                destImage.set(i, j, value);
            }
        }
    }


    /**
     * Transforms and resamples volume using nearest neighbor interpolation.
     *
     * @param  imgBuf   input image array
     * @param  imgBuf2  output image array
     * @param  kTM     transformation matrix to be applied
     */
    private void transformNearestNeighbor2DC(float[] imgBuf, float[] imgBuf2, TransMatrix kTM) {
        int i, j;
        float X, Y;
        int roundX, roundY;
        int xOffset, yOffset;
        float imm, jmm;
        int mod = Math.max(1, oXdim / 50);
        int counter = 0; // used for progress bar
        float T00, T01, T02, T10, T11, T12;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged((int) (((float) i / oXdim * 100) + 0.5));
            }

            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                // transform i,j
                imm = (float) i * oXres;
                jmm = (float) j * oYres;
                X = (imm * T00) + (jmm * T01) + T02;
                Y = (imm * T10) + (jmm * T11) + T12;

                // set intensity of i,j,k to new transformed coordinate if
                // x,y,z is w/in dimensions of image
                X = X / iXres;
                Y = Y / iYres;

                roundX = (int) (X + 0.5f);
                roundY = (int) (Y + 0.5f);

                if ((X < -0.5) || (X >= iXdim) || (Y < -0.5) || (Y >= iYdim)) {
                    imgBuf2[4 * (i + (oXdim * j))] = 0;
                    imgBuf2[(4 * (i + (oXdim * j))) + 1] = 0;
                    imgBuf2[(4 * (i + (oXdim * j))) + 2] = 0;
                    imgBuf2[(4 * (i + (oXdim * j))) + 3] = 0;
                } else {
                    xOffset = Math.min(roundX, iXdim - 1);
                    yOffset = Math.min(roundY, iYdim - 1) * iXdim;
                    imgBuf2[4 * (i + (oXdim * j))] = imgBuf[4 * (xOffset + yOffset)];
                    imgBuf2[(4 * (i + (oXdim * j))) + 1] = imgBuf[(4 * (xOffset + yOffset)) + 1];
                    imgBuf2[(4 * (i + (oXdim * j))) + 2] = imgBuf[(4 * (xOffset + yOffset)) + 2];
                    imgBuf2[(4 * (i + (oXdim * j))) + 3] = imgBuf[(4 * (xOffset + yOffset)) + 3];
                }

                counter++;
            }
        }

        if (threadStopped) {
            return;
        }

        try {
            destImage.importData(0, imgBuf2, true);
        } catch (IOException error) {
            MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
        }

    }

    /**
     * Transforms and resamples volume using nearest neighbor interpolation.
     *
     * @param  imgBuf  image array
     * @param  kTM    transformation matrix to be applied
     */
    private void transformNearestNeighbor3D(float[] imgBuf, TransMatrix kTM) {
        int i, j, k;
        float X, Y, Z;
        int xOffset, yOffset, zOffset;
        float value;
        int sliceSize;
        int roundX, roundY, roundZ;
        float imm, jmm, kmm;
        int mod = Math.max(1, oXdim / 50);
        int counter = 0; // used for progress bar

        sliceSize = iXdim * iYdim;

        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged((int) (((float) i / oXdim * 100) + 0.5));
            }

            imm = (float) i * oXres;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {
                jmm = (float) j * oYres;

                for (k = 0; (k < oZdim) && !threadStopped; k++) {

                    // transform i,j,k
                    kmm = (float) k * oZres;

                    X = (imm * T00) + (jmm * T01) + (kmm * T02) + T03;
                    Y = (imm * T10) + (jmm * T11) + (kmm * T12) + T13;
                    Z = (imm * T20) + (jmm * T21) + (kmm * T22) + T23;

                    // set intensity of i,j,k to new transformed coordinate if
                    // x,y,z is w/in dimensions of image
                    X = X / iXres;
                    Y = Y / iYres;
                    Z = Z / iZres;

                    roundX = (int) (X + 0.5f);
                    roundY = (int) (Y + 0.5f);
                    roundZ = (int) (Z + 0.5f);

                    if ((X < -0.5) || (X >= iXdim) || (Y < -0.5) || (Y >= iYdim) ||
                            (Z < -0.5) || (Z >= iZdim)) {
                        value = (float) srcImage.getMin();
                    } else {
                        xOffset = Math.min(roundX, iXdim - 1);
                        yOffset = Math.min(roundY, iYdim - 1) * iXdim;
                        zOffset = Math.min(roundZ, iZdim - 1) * sliceSize;
                        value = imgBuf[xOffset + yOffset + zOffset];
                    }

                    destImage.set(i, j, k, value);
                    counter++;
                }
            }
        }
    }

    /**
     * Transforms and resamples volume using nearest neighbor interpolation.
     *
     * @param  imgBuf   input image array
     * @param  imgBuf2  output image array
     * @param  kTM     transformation matrix to be applied
     */
    private void transformNearestNeighbor3DC(float[] imgBuf, float[] imgBuf2, TransMatrix kTM) {
        int i, j, k;
        float X, Y, Z;
        int roundX, roundY, roundZ;
        int xOffset, yOffset, zOffset;
        int sliceSize;
        float imm, jmm, kmm;
        int mod = Math.max(1, oXdim / 50);
        int counter = 0; // used for progress bar

        sliceSize = iXdim * iYdim;

        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged((int) (((float) i / oXdim * 100) + 0.5));
            }

            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                for (k = 0; (k < oZdim) && !threadStopped; k++) {

                    // transform i,j,k
                    imm = (float) i * oXres;
                    jmm = (float) j * oYres;
                    kmm = (float) k * oZres;

                    X = (imm * T00) + (jmm * T01) + (kmm * T02) + T03;
                    Y = (imm * T10) + (jmm * T11) + (kmm * T12) + T13;
                    Z = (imm * T20) + (jmm * T21) + (kmm * T22) + T23;

                    // set intensity of i,j,k to new transformed coordinate if
                    // x,y,z is w/in dimensions of image
                    X = X / iXres;
                    Y = Y / iYres;
                    Z = Z / iZres;

                    roundX = (int) (X + 0.5f);
                    roundY = (int) (Y + 0.5f);
                    roundZ = (int) (Z + 0.5f);

                    if ((X < -0.5) || (X >= iXdim) || (Y < -0.5) || (Y >= iYdim) ||
                            (Z < -0.5) || (Z >= iZdim)) {
                        imgBuf2[4 * (i + (oXdim * j) + (oXdim * oYdim * k))] = 0;
                        imgBuf2[(4 * (i + (oXdim * j) + (oXdim * oYdim * k))) + 1] = 0;
                        imgBuf2[(4 * (i + (oXdim * j) + (oXdim * oYdim * k))) + 2] = 0;
                        imgBuf2[(4 * (i + (oXdim * j) + (oXdim * oYdim * k))) + 3] = 0;
                    } else {
                        xOffset = Math.min(roundX, iXdim - 1);
                        yOffset = Math.min(roundY, iYdim - 1) * iXdim;
                        zOffset = Math.min(roundZ, iZdim - 1) * sliceSize;
                        imgBuf2[4 * (i + (oXdim * j) + (oXdim * oYdim * k))] = imgBuf[4 * (xOffset + yOffset + zOffset)];
                        imgBuf2[(4 * (i + (oXdim * j) + (oXdim * oYdim * k))) + 1] = imgBuf[(4 *
                                                                                                 (xOffset + yOffset +
                                                                                                      zOffset)) + 1];
                        imgBuf2[(4 * (i + (oXdim * j) + (oXdim * oYdim * k))) + 2] = imgBuf[(4 *
                                                                                                 (xOffset + yOffset +
                                                                                                      zOffset)) + 2];
                        imgBuf2[(4 * (i + (oXdim * j) + (oXdim * oYdim * k))) + 3] = imgBuf[(4 *
                                                                                                 (xOffset + yOffset +
                                                                                                      zOffset)) + 3];
                    }

                    counter++;
                }
            }
        }

        if (threadStopped) {
            return;
        }

        try {
            destImage.importData(0, imgBuf2, true);
        } catch (IOException error) {
            MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
        }

    }

    /**
     * Transforms and resamples volume using nearest neighbor interpolation.
     *
     * @param  imgBuf  image array
     * @param  kTM    transformation matrix to be applied
     */
    private void transformNearestNeighbor3Dim2D(float[] imgBuf, TransMatrix kTM) {
        int i, j, k;
        float X, Y;
        int roundX, roundY;
        int xOffset, yOffset;
        float value;
        float imm, jmm;
        float T00, T01, T02, T10, T11, T12;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        for (k = 0; (k < oZdim) && !threadStopped; k++) {
            fireProgressStateChanged(Math.round((float) k / oZdim * 100));

            for (i = 0; (i < oXdim) && !threadStopped; i++) {

                for (j = 0; (j < oYdim) && !threadStopped; j++) {

                    // transform i,j
                    imm = (float) i * oXres;
                    jmm = (float) j * oYres;
                    X = (imm * T00) + (jmm * T01) + T02;
                    Y = (imm * T10) + (jmm * T11) + T12;

                    // set intensity of i,j,k to new transformed coordinate if
                    // x,y,z is w/in dimensions of image
                    X = X / iXres;
                    Y = Y / iYres;

                    roundX = (int) (X + 0.5f);
                    roundY = (int) (Y + 0.5f);

                    if ((X < -0.5) || (X >= iXdim) || (Y < -0.5) || (Y >= iYdim)) {
                        value = (float) srcImage.getMin();
                    } else {
                        xOffset = Math.min(roundX, iXdim - 1);
                        yOffset = Math.min(roundY, iYdim - 1) * iXdim;
                        value = imgBuf[xOffset + yOffset];
                    }

                    destImage.set(i, j, k, value);
                }
            }

            if (threadStopped) {
                return;
            }

            if (k < (oZdim - 1)) {

                try {
                    srcImage.exportData((k + 1) * imgLength, imgLength, imgBuf);
                } catch (IOException error) {
                    displayError("Algorithm transform: Image(s) is locked");
                    setCompleted(false);


                    return;
                }
            } // if (k < (oZdim - 1))
        } // for (k = 0; k < oZdim && !threadStopped; k++)
    }

    /**
     * Transforms and resamples volume using nearest neighbor interpolation.
     *
     * @param  imgBuf   input image array
     * @param  imgBuf2  output image array
     * @param  kTM     transformation matrix to be applied
     */
    private void transformNearestNeighbor3Dim2DC(float[] imgBuf, float[] imgBuf2, TransMatrix kTM) {
        int i, j, k;
        float X, Y;
        int roundX, roundY;
        int xOffset, yOffset;
        float imm, jmm;
        float T00, T01, T02, T10, T11, T12;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        for (k = 0; (k < oZdim) && !threadStopped; k++) {
            fireProgressStateChanged((int) (((float) k / oZdim * 100) + 0.5));

            for (i = 0; (i < oXdim) && !threadStopped; i++) {

                for (j = 0; (j < oYdim) && !threadStopped; j++) {

                    // transform i,j
                    imm = (float) i * oXres;
                    jmm = (float) j * oYres;
                    X = (imm * T00) + (jmm * T01) + T02;
                    Y = (imm * T10) + (jmm * T11) + T12;

                    // set intensity of i,j,k to new transformed coordinate if
                    // x,y,z is w/in dimensions of image
                    X = X / iXres;
                    Y = Y / iYres;

                    roundX = (int) (X + 0.5f);
                    roundY = (int) (Y + 0.5f);

                    if ((X < -0.5) || (X >= iXdim) || (Y < -0.5) || (Y >= iYdim)) {
                        imgBuf2[4 * (i + (oXdim * j))] = 0;
                        imgBuf2[(4 * (i + (oXdim * j))) + 1] = 0;
                        imgBuf2[(4 * (i + (oXdim * j))) + 2] = 0;
                        imgBuf2[(4 * (i + (oXdim * j))) + 3] = 0;
                    } else {
                        xOffset = Math.min(roundX, iXdim - 1);
                        yOffset = Math.min(roundY, iYdim - 1) * iXdim;
                        imgBuf2[4 * (i + (oXdim * j))] = imgBuf[4 * (xOffset + yOffset)];
                        imgBuf2[(4 * (i + (oXdim * j))) + 1] = imgBuf[(4 * (xOffset + yOffset)) + 1];
                        imgBuf2[(4 * (i + (oXdim * j))) + 2] = imgBuf[(4 * (xOffset + yOffset)) + 2];
                        imgBuf2[(4 * (i + (oXdim * j))) + 3] = imgBuf[(4 * (xOffset + yOffset)) + 3];
                    }
                }
            }

            try {
                destImage.importData(4 * k * oXdim * oYdim, imgBuf2, true);
            } catch (IOException error) {
                MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
                setCompleted(false);


                return;
            }

            if (k < (oZdim - 1)) {

                try {
                    srcImage.exportData((k + 1) * imgLength, imgLength, imgBuf);
                } catch (IOException error) {
                    MipavUtil.displayError("AlgorithmTransform: IOException Error on exportData");
                    setCompleted(false);


                    return;
                }
            } // if (k < (oZdim - 1)
        } // for (k = 0; k < oZdim && !threadStopped; k++)

    }

    /**
     * Transforms and resamples 4 dimensional object using 3D algorithm using nearest neighbor interpolation.
     *
     * @param  imgBuf  image array
     * @param  kTM    transformation matrix to be applied
     */
    private void transformNearestNeighbor4D(float[] imgBuf, TransMatrix kTM) {
        int i, j, k, l;
        float X, Y, Z;
        int xOffset, yOffset, zOffset;
        float value;
        int sliceSize;
        int roundX, roundY, roundZ;
        float imm, jmm, kmm;
        int counter = 0; // used for progress bar

        sliceSize = iXdim * iYdim;

        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged((int) (((float) l / oTdim * 100) + 0.5));

            for (i = 0; (i < oXdim) && !threadStopped; i++) {

                for (j = 0; (j < oYdim) && !threadStopped; j++) {

                    for (k = 0; (k < oZdim) && !threadStopped; k++) {

                        // transform i,j,k
                        imm = (float) i * oXres;
                        jmm = (float) j * oYres;
                        kmm = (float) k * oZres;

                        X = (imm * T00) + (jmm * T01) + (kmm * T02) + T03;
                        Y = (imm * T10) + (jmm * T11) + (kmm * T12) + T13;
                        Z = (imm * T20) + (jmm * T21) + (kmm * T22) + T23;

                        // set intensity of i,j,k to new transformed coordinate if
                        // x,y,z is w/in dimensions of image
                        X = X / iXres;
                        Y = Y / iYres;
                        Z = Z / iZres;

                        roundX = (int) (X + 0.5f);
                        roundY = (int) (Y + 0.5f);
                        roundZ = (int) (Z + 0.5f);

                        if ((X < -0.5) || (X >= iXdim) || (Y < -0.5) || (Y >= iYdim) ||
                                (Z < -0.5) || (Z >= iZdim)) {
                            value = (float) srcImage.getMin();
                        } else {
                            xOffset = Math.min(roundX, iXdim - 1);
                            yOffset = Math.min(roundY, iYdim - 1) * iXdim;
                            zOffset = Math.min(roundZ, iZdim - 1) * sliceSize;
                            value = imgBuf[xOffset + yOffset + zOffset];

                        }

                        destImage.set(i, j, k, l, value);
                        counter++;
                    }
                }
            } // for i

            if (l < (oTdim - 1)) {

                try {
                    srcImage.exportData((l + 1) * imgLength, imgLength, imgBuf);
                } catch (IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);


                    return;
                }
            } // end if (l < (oTdim - 1))
        } // for l
    }
    
    /**
     * Transforms and resamples 4 dimensional object using 3D algorithm using nearest neighbor interpolation.
     *
     * @param  imgBuf  image array
     * @param  imgBufffer2
     * @param  kTM    transformation matrix to be applied
     */
    private void transformNearestNeighbor4DC(float[] imgBuf, float imgBuffer2[], TransMatrix kTM) {
        int i, j, k, l;
        float X, Y, Z;
        int xOffset, yOffset, zOffset;
        int temp;
        float value;
        int sliceSize;
        int oSliceSize;
        int oVolSize;
        int pos;
        int roundX, roundY, roundZ;
        float imm, jmm, kmm;
        int counter = 0; // used for progress bar

        sliceSize = iXdim * iYdim;
        oSliceSize = oXdim * oYdim;
        oVolSize = oSliceSize * oZdim;

        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged((int) (((float) l / oTdim * 100) + 0.5));

            for (i = 0; (i < oXdim) && !threadStopped; i++) {

                for (j = 0; (j < oYdim) && !threadStopped; j++) {

                    for (k = 0; (k < oZdim) && !threadStopped; k++) {
                        temp = 4 * (i + (j * oXdim) + (k * oSliceSize));

                        // transform i,j,k
                        imm = (float) i * oXres;
                        jmm = (float) j * oYres;
                        kmm = (float) k * oZres;

                        X = (imm * T00) + (jmm * T01) + (kmm * T02) + T03;
                        Y = (imm * T10) + (jmm * T11) + (kmm * T12) + T13;
                        Z = (imm * T20) + (jmm * T21) + (kmm * T22) + T23;

                        // set intensity of i,j,k to new transformed coordinate if
                        // x,y,z is w/in dimensions of image
                        X = X / iXres;
                        Y = Y / iYres;
                        Z = Z / iZres;

                        roundX = (int) (X + 0.5f);
                        roundY = (int) (Y + 0.5f);
                        roundZ = (int) (Z + 0.5f);

                        if ((X < -0.5) || (X >= iXdim) || (Y < -0.5) || (Y >= iYdim) ||
                                (Z < -0.5) || (Z >= iZdim)) {
                            imgBuffer2[temp] = 0; // if voxel is transformed out of bounds
                            imgBuffer2[temp + 1] = 0;
                            imgBuffer2[temp + 2] = 0;
                            imgBuffer2[temp + 3] = 0;
                        } else {
                            xOffset = Math.min(roundX, iXdim - 1);
                            yOffset = Math.min(roundY, iYdim - 1) * iXdim;
                            zOffset = Math.min(roundZ, iZdim - 1) * sliceSize;
                            pos = xOffset + yOffset + zOffset;
                            imgBuffer2[temp] = imgBuf[4*pos];
                            imgBuffer2[temp+1] = imgBuf[4*pos+1];
                            imgBuffer2[temp+2] = imgBuf[4*pos+2];
                            imgBuffer2[temp+3] = imgBuf[4*pos+3];
                        }
                        counter++;
                    }
                }
            } // for i
            
            try {
                destImage.importData(4 * l * oVolSize, imgBuffer2, false);
            }
            catch(IOException error) {
                MipavUtil.displayError("AlgorithmTransform: IOException on destImage.importData");
            }

            if (l < (oTdim - 1)) {

                try {
                    srcImage.exportData((l + 1) * imgLength, imgLength, imgBuf);
                } catch (IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);


                    return;
                }
            } // end if (l < (oTdim - 1))
        } // for l
        destImage.calcMinMax();
    }

    /**
     * Transforms and resamples volume using nearest neighbor interpolation.
     *
     * @param  imgBuf  image array
     * @param  kTM    transformation matrix to be applied
     */
    private void transformNearestNeighbor4Dim2D(float[] imgBuf, TransMatrix kTM) {
        int i, j, k, l;
        float X, Y;
        int roundX, roundY;
        int xOffset, yOffset;
        float value;
        float imm, jmm;
        float T00, T01, T02, T10, T11, T12;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged((int) (((float) l / oTdim * 100) + 0.5));

            for (k = 0; (k < oZdim) && !threadStopped; k++) {

                for (i = 0; (i < oXdim) && !threadStopped; i++) {

                    for (j = 0; (j < oYdim) && !threadStopped; j++) {

                        // transform i,j
                        imm = (float) i * oXres;
                        jmm = (float) j * oYres;
                        X = (imm * T00) + (jmm * T01) + T02;
                        Y = (imm * T10) + (jmm * T11) + T12;

                        // set intensity of i,j to new transformed coordinate if
                        // x,y is w/in dimensions of image
                        X = X / iXres;
                        Y = Y / iYres;

                        roundX = (int) (X + 0.5f);
                        roundY = (int) (Y + 0.5f);

                        if ((X < -0.5) || (X >= iXdim) || (Y < -0.5) || (Y >= iYdim)) {
                            value = (float) srcImage.getMin();
                        } else {
                            xOffset = Math.min(roundX, iXdim - 1);
                            yOffset = Math.min(roundY, iYdim - 1) * iXdim;
                            value = imgBuf[xOffset + yOffset];
                        }

                        destImage.set(i, j, k, l, value);
                    }
                }

                if ((k < (oZdim - 1)) || (l < (oTdim - 1))) {

                    try {
                        srcImage.exportData((l * oZdim * imgLength) + ((k + 1) * imgLength), imgLength, imgBuf);
                    } catch (IOException error) {
                        displayError("Algorithm Transform: Image(s) locked");
                        setCompleted(false);


                        return;
                    }
                } // if ((k < (oZdim - 1))|| (l < (oTdim - 1)))
            } // for k
        } // for l

    }

    /**
     * Transforms and resamples volume using nearest neighbor interpolation.
     *
     * @param  imgBuf   input image array
     * @param  imgBuf2  output image array
     * @param  kTM     transformation matrix to be applied
     */
    private void transformNearestNeighbor4Dim2DC(float[] imgBuf, float[] imgBuf2, TransMatrix kTM) {
        int i, j, k, l;
        float X, Y;
        int roundX, roundY;
        int xOffset, yOffset;
        float imm, jmm;
        float T00, T01, T02, T10, T11, T12;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged((int) (((float) l / oTdim * 100) + 0.5));

            for (k = 0; (k < oZdim) && !threadStopped; k++) {

                for (i = 0; (i < oXdim) && !threadStopped; i++) {

                    for (j = 0; (j < oYdim) && !threadStopped; j++) {

                        // transform i,j
                        imm = (float) i * oXres;
                        jmm = (float) j * oYres;
                        X = (imm * T00) + (jmm * T01) + T02;
                        Y = (imm * T10) + (jmm * T11) + T12;

                        // set intensity of i,j,k,l to new transformed coordinate if
                        // x,y,z,t is w/in dimensions of image
                        X = X / iXres;
                        Y = Y / iYres;

                        roundX = (int) (X + 0.5f);
                        roundY = (int) (Y + 0.5f);

                        if ((X < -0.5) || (X >= iXdim) || (Y < -0.5) || (Y >= iYdim)) {
                            imgBuf2[4 * (i + (oXdim * j))] = 0;
                            imgBuf2[(4 * (i + (oXdim * j))) + 1] = 0;
                            imgBuf2[(4 * (i + (oXdim * j))) + 2] = 0;
                            imgBuf2[(4 * (i + (oXdim * j))) + 3] = 0;
                        } else {
                            xOffset = Math.min(roundX, iXdim - 1);
                            yOffset = Math.min(roundY, iYdim - 1) * iXdim;
                            imgBuf2[4 * (i + (oXdim * j))] = imgBuf[4 * (xOffset + yOffset)];
                            imgBuf2[(4 * (i + (oXdim * j))) + 1] = imgBuf[(4 * (xOffset + yOffset)) + 1];
                            imgBuf2[(4 * (i + (oXdim * j))) + 2] = imgBuf[(4 * (xOffset + yOffset)) + 2];
                            imgBuf2[(4 * (i + (oXdim * j))) + 3] = imgBuf[(4 * (xOffset + yOffset)) + 3];
                        }
                    }
                }

                try {
                    destImage.importData((4 * l * oXdim * oYdim * oZdim) + (4 * k * oXdim * oYdim), imgBuf2, true);
                } catch (IOException error) {
                    MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
                }

                if ((k < (oZdim - 1)) || (l < (oTdim - 1))) {

                    try {
                        srcImage.exportData((l * oZdim * imgLength) + ((k + 1) * imgLength), imgLength, imgBuf);
                    } catch (IOException error) {
                        displayError("Algorithm Transform: IOException Error on exportData");
                        setCompleted(false);


                        return;
                    }
                } // if ((k < (oZdim - 1))|| (l < (oTdim - 1)))
            } // for k
        } // for l

    }

    /**
     * Transforms and resamples volume using quintic Lagrangian interpolation.
     *
     * @param  imgBuf  Image array
     * @param  kTM    Transformation matrix to be applied
     * @param  clip    If <code>true</code> clip output values to be within input range
     */
    private void transformQuinticLagrangian2D(float[] imgBuf, TransMatrix kTM, boolean clip) {
        AlgorithmQuinticLagrangian QLag = new AlgorithmQuinticLagrangian();
        int i, j;
        float X, Y;
        float value;
        float imm, jmm;
        int[] inVolExtents = { iXdim, iYdim };
        int mod = Math.max(1, oXdim / 50);
        int counter = 0; // used for progress bar
        float temp1, temp2;
        float T00, T01, T02, T10, T11, T12;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        QLag.setup2DQuinticLagrangian(imgBuf, inVolExtents, clip);

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged((int) (((float) i / oXdim * 100) + 0.5));
            }

            imm = (float) i * oXres;
            temp1 = (imm * T00) + T02;
            temp2 = (imm * T10) + T12;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                // convert to mm
                value = (float) srcImage.getMin(); // will remain zero if boundary conditions not met

                jmm = (float) j * oYres;

                // transform i,j
                X = (temp1 + (jmm * T01)) / iXres;

                if ((X >= 0) && (X < iXdim)) { // check bounds
                    Y = (temp2 + (jmm * T11)) / iYres;

                    if ((Y >= 0) && (Y < iYdim)) {
                        value = QLag.quinticLagrangian2D(X, Y);
                    }
                }

                destImage.set(i, j, value);
                counter++;
            }
        }

        QLag.finalize();
        QLag = null;
        Preferences.debug("finished quintic Lagrangian");
    }

    /**
     * Transforms and resamples volume using quintic Lagrangian interpolation This version used with color images.
     *
     * @param  imgBuf   Input image array
     * @param  imgBuf2  Output image array
     * @param  kTM     Transformation matrix to be applied
     * @param  clip     If <code>true</code> clip output values to be within input range
     */
    private void transformQuinticLagrangian2DC(float[] imgBuf, float[] imgBuf2, TransMatrix kTM, boolean clip) {
        AlgorithmQuinticLagrangian QLag = new AlgorithmQuinticLagrangian();
        int i, j;
        float X, Y;
        float[] value = new float[4];
        float imm, jmm;
        int[] inVolExtents = { iXdim, iYdim };
        int mod = Math.max(1, oXdim / 50);
        int counter = 0; // used for progress bar
        float temp1, temp2;
        int temp3;
        float T00, T01, T02, T10, T11, T12;
        float argbMax = 255.0f;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        if (srcImage.getType() == ModelStorageBase.ARGB) {
            argbMax = 255.0f;
        } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
            argbMax = 65535.0f;
        }

        QLag.setup2DQuinticLagrangianC(imgBuf, inVolExtents, argbMax, clip);

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged((int) (((float) i / oXdim * 100) + 0.5));
            }

            imm = (float) i * oXres;
            temp1 = (imm * T00) + T02;
            temp2 = (imm * T10) + T12;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                // convert to mm
                value[0] = 0; // will remain zero if boundary conditions not met
                value[1] = 0;
                value[2] = 0;
                value[3] = 0;

                jmm = (float) j * oYres;

                // transform i,j
                X = (temp1 + (jmm * T01)) / iXres;

                if ((X >= 0) && (X < iXdim)) { // check bounds
                    Y = (temp2 + (jmm * T11)) / iYres;

                    if ((Y >= 0) && (Y < iYdim)) {
                        value = QLag.quinticLagrangian2DC(X, Y);
                    }
                }

                temp3 = 4 * (i + (j * oXdim));
                imgBuf2[temp3] = value[0];
                imgBuf2[temp3 + 1] = value[1];
                imgBuf2[temp3 + 2] = value[2];
                imgBuf2[temp3 + 3] = value[3];
                counter++;
            }
        }

        if (threadStopped) {
            return;
        }

        try {
            destImage.importData(0, imgBuf2, true);
        } catch (IOException error) {
            MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
        }

        QLag.finalize();
        QLag = null;
        Preferences.debug("finished quintic Lagrangian");

    }

    /**
     * Transforms and resamples volume using quintic Lagrangian interpolation.
     *
     * @param  imgBuf  Image array
     * @param  kTM    Transformation matrix to be applied
     * @param  clip    If <code>true</code> clip output values to be within input range
     */
    private void transformQuinticLagrangian3D(float[] imgBuf, TransMatrix kTM, boolean clip) {
        AlgorithmQuinticLagrangian QLag = new AlgorithmQuinticLagrangian();
        int i, j, k;
        float X, Y, Z;
        float value;
        int sliceSize;
        float imm, jmm, kmm;
        int[] inVolExtents = { iXdim, iYdim, iZdim };
        int mod = Math.max(1, oXdim / 50);
        int counter = 0; // used for progress bar

        sliceSize = iXdim * iYdim;

        float i1, i2, i3, j1, j2, j3;
        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;

        QLag.setup3DQuinticLagrangian(imgBuf, inVolExtents, clip);

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged((int) (((float) i / oXdim * 100) + 0.5));
            }

            imm = (float) i * oXres;
            i1 = (imm * T00) + T03;
            i2 = (imm * T10) + T13;
            i3 = (imm * T20) + T23;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {
                jmm = (float) j * oYres;
                j1 = (jmm * T01) + i1;
                j2 = (jmm * T11) + i2;
                j3 = (jmm * T21) + i3;

                for (k = 0; (k < oZdim) && !threadStopped; k++) {

                    // convert to mm
                    value = (float) srcImage.getMin(); // will remain zero if boundary conditions not met
                    kmm = (float) k * oZres;

                    // transform i,j,k
                    X = (j1 + (kmm * T02)) / iXres;

                    if ((X >= 0) && (X < iXdim)) { // check bounds
                        Y = (j2 + (kmm * T12)) / iYres;

                        if ((Y >= 0) && (Y < iYdim)) {
                            Z = (j3 + (kmm * T22)) / iZres;

                            if ((Z >= 0) && (Z < iZdim)) {
                                value = QLag.quinticLagrangian3D(X, Y, Z);
                            }
                        }
                    }

                    destImage.set(i, j, k, value);
                    counter++;
                }
            }
        }

        QLag.finalize();
        QLag = null;
        Preferences.debug("finished quintic Lagrangian");
    }

    /**
     * Transforms and resamples volume using quintic Lagrangian interpolation This version used with color images.
     *
     * @param  imgBuf   Input image array
     * @param  imgBuf2  Output image array
     * @param  kTM     Transformation matrix to be applied
     * @param  clip     If <code>true</code> clip output values to be within input range
     */
    private void transformQuinticLagrangian3DC(float[] imgBuf, float[] imgBuf2, TransMatrix kTM, boolean clip) {
        AlgorithmQuinticLagrangian QLag = new AlgorithmQuinticLagrangian();
        int i, j, k;
        float X, Y, Z;
        float[] value = new float[4];
        int sliceSize;
        float imm, jmm, kmm;
        int[] inVolExtents = { iXdim, iYdim, iZdim };
        int mod = Math.max(1, oXdim / 50);
        int counter = 0; // used for progress bar

        sliceSize = iXdim * iYdim;

        int osliceSize = oXdim * oYdim;
        float i1, i2, i3, j1, j2, j3;
        float temp1, temp2, temp3;
        int temp4;
        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        float argbMax = 255.0f;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;

        if (srcImage.getType() == ModelStorageBase.ARGB) {
            argbMax = 255.0f;
        } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
            argbMax = 65535.0f;
        }

        QLag.setup3DQuinticLagrangianC(imgBuf, inVolExtents, argbMax, clip);

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged((int) (((float) i / oXdim * 100) + 0.5));
            }

            imm = (float) i * oXres;
            i1 = (imm * T00) + T03;
            i2 = (imm * T10) + T13;
            i3 = (imm * T20) + T23;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {
                jmm = (float) j * oYres;
                j1 = jmm * T01;
                j2 = jmm * T11;
                j3 = jmm * T21;
                temp1 = i3 + j3;
                temp2 = i2 + j2;
                temp3 = i1 + j1;

                for (k = 0; (k < oZdim) && !threadStopped; k++) {

                    // convert to mm
                    value[0] = 0; // will remain zero if boundary conditions not met
                    value[1] = 0;
                    value[2] = 0;
                    value[3] = 0;
                    kmm = (float) k * oZres;

                    // transform i,j,k
                    X = (temp3 + (kmm * T02)) / iXres;

                    // convert back to pixels
                    if ((X >= 0) && (X < iXdim)) { // check bounds
                        Y = (temp2 + (kmm * T12)) / iYres;

                        if ((Y >= 0) && (Y < iYdim)) {
                            Z = (temp1 + (kmm * T22)) / iZres;

                            if ((Z >= 0) && (Z < iZdim)) {
                                value = QLag.quinticLagrangian3DC(X, Y, Z);
                            }
                        }
                    }

                    temp4 = 4 * (i + (j * oXdim) + (k * osliceSize));
                    imgBuf2[temp4] = value[0];
                    imgBuf2[temp4 + 1] = value[1];
                    imgBuf2[temp4 + 2] = value[2];
                    imgBuf2[temp4 + 3] = value[3];
                    counter++;
                }
            }
        }

        if (threadStopped) {
            return;
        }

        try {
            destImage.importData(0, imgBuf2, true);
        } catch (IOException error) {
            MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
        }

        QLag.finalize();
        QLag = null;
        Preferences.debug("finished quintic Lagrangian");

    }

    /**
     * Transforms and resamples volume using quintic Lagrangian interpolation Does a slice by slice quintic Lagrangian
     * interpolation on a 3 dimensional object.
     *
     * @param  imgBuf  Image array
     * @param  kTM    Transformation matrix to be applied
     * @param  clip    If <code>true</code> clip output values to be within input range
     */
    private void transformQuinticLagrangian3Dim2D(float[] imgBuf, TransMatrix kTM, boolean clip) {
        AlgorithmQuinticLagrangian QLag = new AlgorithmQuinticLagrangian();
        int i, j, k;
        float X, Y;
        float value;
        int sliceSize;
        float imm, jmm;
        int[] inVolExtents = { iXdim, iYdim };

        sliceSize = iXdim * iYdim;

        int mod = Math.max(1, oZdim / 50);
        int counter = 0; // used for progress bar
        float temp1, temp2;
        float T00, T01, T02, T10, T11, T12;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        for (k = 0; (k < oZdim) && !threadStopped; k++) {

            if (((k % mod) == 0)) {
                fireProgressStateChanged((int) (((float) k / oZdim * 100) + 0.5));
            }

            QLag.setup2DQuinticLagrangian(imgBuf, inVolExtents, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                imm = (float) i * oXres;
                temp1 = (imm * T00) + T02;
                temp2 = (imm * T10) + T12;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {

                    // convert to mm
                    value = (float) srcImage.getMin(); // will remain zero if boundary conditions not met
                    jmm = (float) j * oYres;

                    // transform i,j
                    X = (temp1 + (jmm * T01)) / iXres;

                    if ((X >= 0) && (X < iXdim)) { // check bounds
                        Y = (temp2 + (jmm * T11)) / iYres;

                        if ((Y >= 0) && (Y < iYdim)) {
                            value = QLag.quinticLagrangian2D(X, Y);
                        }
                    }

                    destImage.set(i, j, k, value);
                    counter++;
                }
            }

            if (k < (oZdim - 1)) {

                try {
                    srcImage.exportData((k + 1) * imgLength, imgLength, imgBuf);
                } catch (IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);


                    return;
                }
            } // end if (k < (oZdim - 1))
        } // end for k

        QLag.finalize();
        QLag = null;
        Preferences.debug("finished quintic Lagrangian");
    }

    /**
     * Transforms and resamples volume using quintic Lagrangian interpolation This version used with color images This
     * version performs a slice by slice algorithm on a 3 dimensional object.
     *
     * @param  imgBuf   Input image array
     * @param  imgBuf2  Output image array
     * @param  kTM     Transformation matrix to be applied
     * @param  clip     If <code>true</code> clip output values to be within input range
     */
    private void transformQuinticLagrangian3Dim2DC(float[] imgBuf, float[] imgBuf2, TransMatrix kTM, boolean clip) {
        AlgorithmQuinticLagrangian QLag = new AlgorithmQuinticLagrangian();
        int i, j, k;
        float X, Y;
        float[] value = new float[4];
        int sliceSize;
        float imm, jmm;
        int[] inVolExtents = { iXdim, iYdim };

        sliceSize = iXdim * iYdim;

        int mod = Math.max(1, oZdim / 50);
        int counter = 0; // used for progress bar
        float temp1, temp2;
        int temp3;
        float T00, T01, T02, T10, T11, T12;
        float argbMax = 255.0f;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        for (k = 0; (k < oZdim) && !threadStopped; k++) {

            if (((k % mod) == 0)) {
                fireProgressStateChanged((int) (((float) k / oZdim * 100) + 0.5));
            }

            if (srcImage.getType() == ModelStorageBase.ARGB) {
                argbMax = 255.0f;
            } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
                argbMax = 65535.0f;
            }

            QLag.setup2DQuinticLagrangianC(imgBuf, inVolExtents, argbMax, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                imm = (float) i * oXres;
                temp1 = (imm * T00) + T02;
                temp2 = (imm * T10) + T12;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {

                    // convert to mm
                    value[0] = 0; // will remain zero if boundary conditions not met
                    value[1] = 0;
                    value[2] = 0;
                    value[3] = 0;

                    jmm = (float) j * oYres;

                    // transform i,j
                    X = (temp1 + (jmm * T01)) / iXres;

                    if ((X >= 0) && (X < iXdim)) { // check bounds
                        Y = (temp2 + (jmm * T11)) / iYres;

                        if ((Y >= 0) && (Y < iYdim)) {
                            value = QLag.quinticLagrangian2DC(X, Y);
                        }
                    }

                    temp3 = 4 * (i + (j * oXdim));
                    imgBuf2[temp3] = value[0];
                    imgBuf2[temp3 + 1] = value[1];
                    imgBuf2[temp3 + 2] = value[2];
                    imgBuf2[temp3 + 3] = value[3];
                    counter++;
                } // for i
            } // for j

            if (threadStopped) {
                return;
            }

            try {
                destImage.importData(4 * k * oXdim * oYdim, imgBuf2, true);
            } catch (IOException error) {
                MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
            }

            if (k < (oZdim - 1)) {

                try {
                    srcImage.exportData((k + 1) * imgLength, imgLength, imgBuf);
                } catch (IOException error) {
                    displayError("Algorithm Transform: IOException Error on importData");
                    setCompleted(false);


                    return;
                }
            } // end if (k < (oZdim - 1))
        } // end for k

        QLag.finalize();
        QLag = null;
        Preferences.debug("finished quintic Lagrangian");
    }

    /**
     * Transforms and resamples 4 dimensional color object using 3D quintic Lagrangian interpolation.
     *
     * @param  imgBuf  Image array
     * @param  imgBuf2
     * @param  kTM    Transformation matrix to be applied
     * @param  clip    If <code>true</code> clip output values to be within input range
     */
    private void transformQuinticLagrangian4DC(float[] imgBuf, float[] imgBuffer2, TransMatrix kTM, boolean clip) {
        AlgorithmQuinticLagrangian QLag = new AlgorithmQuinticLagrangian();
        int i, j, k, l;
        float X, Y, Z;
        float value[] = new float[4];
        int temp4;
        float imm, jmm, kmm;
        int[] inVolExtents = { iXdim, iYdim, iZdim };
        int oSliceSize;
        int oVolSize;
        int mod = Math.max(1, oTdim / 50);
        int counter = 0; // used for progress bar

        float i1, i2, i3, j1, j2, j3;
        float temp1, temp2, temp3;
        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        float argbMax = 255.0f;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;
        
        oSliceSize = oXdim * oYdim;
        oVolSize = oSliceSize * oZdim;
        
        if (srcImage.getType() == ModelStorageBase.ARGB) {
            argbMax = 255.0f;
        }
        else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
            argbMax = 65535.0f;
        }

        for (l = 0; (l < oTdim) && !threadStopped; l++) {

            if (((l % mod) == 0)) {
                fireProgressStateChanged((int) (((float) l / oTdim * 100) + .5));
            }

            QLag.setup3DQuinticLagrangianC(imgBuf, inVolExtents, argbMax, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                imm = (float) i * oXres;
                i1 = (imm * T00) + T03;
                i2 = (imm * T10) + T13;
                i3 = (imm * T20) + T23;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {
                    jmm = (float) j * oYres;
                    j1 = jmm * T01;
                    j2 = jmm * T11;
                    j3 = jmm * T21;
                    temp1 = i3 + j3;
                    temp2 = i2 + j2;
                    temp3 = i1 + j1;

                    for (k = 0; (k < oZdim) && !threadStopped; k++) {

                        // convert to mm
                        value[0] = 0; // will remain zero if boundary conditions not met
                        value[1] = 0;
                        value[2] = 0;
                        value[3] = 0;
                        kmm = (float) k * oZres;

                        // transform i,j,k
                        X = (temp3 + (kmm * T02)) / iXres;

                        if ((X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (kmm * T12)) / iYres;

                            if ((Y >= 0) && (Y < iYdim)) {
                                Z = (temp1 + (kmm * T22)) / iZres;

                                if ((Z >= 0) && (Z < iZdim)) {
                                    value = QLag.quinticLagrangian3DC(X, Y, Z);
                                }
                            }
                        }

                        temp4 = 4 *(i + (j * oXdim) + (k * oSliceSize));
                        imgBuffer2[temp4] = value[0];
                        imgBuffer2[temp4 + 1] = value[1];
                        imgBuffer2[temp4 + 2] = value[2];
                        imgBuffer2[temp4 + 3] = value[3];
                        counter++;
                    }
                }
            }
            
            try {
                destImage.importData(4 * l * oVolSize, imgBuffer2, false);
            }
            catch (IOException error) {
                MipavUtil.displayError("AlgorithmTransform: IOException on destImage.importData");
            }

            if (l < (oTdim - 1)) {

                try {
                    srcImage.exportData((l + 1) * imgLength, imgLength, imgBuf);
                } catch (IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);


                    return;
                }
            } // end if (l < (oTdim - 1))
        } // for l

        destImage.calcMinMax();
        QLag.finalize();
        QLag = null;
        Preferences.debug("finished quintic Lagrangian");

    }
    
    /**
     * Transforms and resamples 4 dimensional object using 3D quintic Lagrangian interpolation.
     *
     * @param  imgBuf  Image array
     * @param  kTM    Transformation matrix to be applied
     * @param  clip    If <code>true</code> clip output values to be within input range
     */
    private void transformQuinticLagrangian4D(float[] imgBuf, TransMatrix kTM, boolean clip) {
        AlgorithmQuinticLagrangian QLag = new AlgorithmQuinticLagrangian();
        int i, j, k, l;
        float X, Y, Z;
        float value;
        float imm, jmm, kmm;
        int[] inVolExtents = { iXdim, iYdim, iZdim };
        int mod = Math.max(1, oTdim / 50);
        int counter = 0; // used for progress bar

        float i1, i2, i3, j1, j2, j3;
        float temp1, temp2, temp3;
        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {

            if (((l % mod) == 0)) {
                fireProgressStateChanged((int) (((float) l / oTdim * 100) + .5));
            }

            QLag.setup3DQuinticLagrangian(imgBuf, inVolExtents, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                imm = (float) i * oXres;
                i1 = (imm * T00) + T03;
                i2 = (imm * T10) + T13;
                i3 = (imm * T20) + T23;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {
                    jmm = (float) j * oYres;
                    j1 = jmm * T01;
                    j2 = jmm * T11;
                    j3 = jmm * T21;
                    temp1 = i3 + j3;
                    temp2 = i2 + j2;
                    temp3 = i1 + j1;

                    for (k = 0; (k < oZdim) && !threadStopped; k++) {

                        // convert to mm
                        value = (float) srcImage.getMin(); // will remain zero if boundary conditions not met
                        kmm = (float) k * oZres;

                        // transform i,j,k
                        X = (temp3 + (kmm * T02)) / iXres;

                        if ((X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (kmm * T12)) / iYres;

                            if ((Y >= 0) && (Y < iYdim)) {
                                Z = (temp1 + (kmm * T22)) / iZres;

                                if ((Z >= 0) && (Z < iZdim)) {
                                    value = QLag.quinticLagrangian3D(X, Y, Z);
                                }
                            }
                        }

                        destImage.set(i, j, k, l, value);
                        counter++;
                    }
                }
            }

            if (l < (oTdim - 1)) {

                try {
                    srcImage.exportData((l + 1) * imgLength, imgLength, imgBuf);
                } catch (IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);


                    return;
                }
            } // end if (l < (oTdim - 1))
        } // for l

        QLag.finalize();
        QLag = null;
        Preferences.debug("finished quintic Lagrangian");

    }

    /**
     * Transforms and resamples volume using quintic Lagrangian interpolation Does a slice by slice quintic Lagrangian
     * interpolation on a 4 dimensional object.
     *
     * @param  imgBuf  Image array
     * @param  kTM    Transformation matrix to be applied
     * @param  clip    If <code>true</code> clip output values to be within input range
     */
    private void transformQuinticLagrangian4Dim2D(float[] imgBuf, TransMatrix kTM, boolean clip) {
        AlgorithmQuinticLagrangian QLag = new AlgorithmQuinticLagrangian();
        int i, j, k, l;
        float X, Y;
        float value;
        int sliceSize;
        float imm, jmm;
        int[] inVolExtents = { iXdim, iYdim };

        sliceSize = iXdim * iYdim;

        int volSize = sliceSize * iZdim;
        int counter = 0; // used for progress bar
        float temp1, temp2;
        float T00, T01, T02, T10, T11, T12;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged((int) (((float) l / oTdim * 100) + 0.5));

            for (k = 0; (k < oZdim) && !threadStopped; k++) {
                QLag.setup2DQuinticLagrangian(imgBuf, inVolExtents, clip);

                for (i = 0; (i < oXdim) && !threadStopped; i++) {
                    imm = (float) i * oXres;
                    temp1 = (imm * T00) + T02;
                    temp2 = (imm * T10) + T12;

                    for (j = 0; (j < oYdim) && !threadStopped; j++) {

                        // convert to mm
                        value = (float) srcImage.getMin(); // will remain zero if boundary conditions not met
                        jmm = (float) j * oYres;

                        // transform i,j
                        X = (temp1 + (jmm * T01)) / iXres;

                        if ((X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (jmm * T11)) / iYres;

                            if ((Y >= 0) && (Y < iYdim)) {
                                value = QLag.quinticLagrangian2D(X, Y);
                            }
                        }

                        destImage.set(i, j, k, l, value);
                        counter++;
                    }
                }

                if ((k < (oZdim - 1)) || (l < (oTdim - 1))) {

                    try {
                        srcImage.exportData((l * oZdim * imgLength) + ((k + 1) * imgLength), imgLength, imgBuf);
                    } catch (IOException error) {
                        displayError("Algorithm Transform: Image(s) locked");
                        setCompleted(false);


                        return;
                    }
                } // end if ((k < (oZdim - 1))|| (l < (oTdim - 1)))
            } // end for k
        } // end for l

        QLag.finalize();
        QLag = null;
        Preferences.debug("finished quintic Lagrangian");

    }

    /**
     * Transforms and resamples volume using quintic Lagrangian interpolation This version used with color images This
     * version performs a slice by slice algorithm on a 4 dimensional object.
     *
     * @param  imgBuf   Input image array
     * @param  imgBuf2  Output image array
     * @param  kTM     Transformation matrix to be applied
     * @param  clip     if <code>true</code> clip output values to be within input range
     */
    private void transformQuinticLagrangian4Dim2DC(float[] imgBuf, float[] imgBuf2, TransMatrix kTM, boolean clip) {
        AlgorithmQuinticLagrangian QLag = new AlgorithmQuinticLagrangian();
        int i, j, k, l;
        float X, Y;
        float[] value = new float[4];
        float imm, jmm;
        int[] inVolExtents = { iXdim, iYdim };

        int counter = 0; // used for progress bar
        float temp1, temp2;
        int temp3;
        float T00, T01, T02, T10, T11, T12;
        float argbMax = 255.0f;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged((int) (((float) l / oTdim * 100) + 0.5));

            for (k = 0; (k < oZdim) && !threadStopped; k++) {

                if (srcImage.getType() == ModelStorageBase.ARGB) {
                    argbMax = 255.0f;
                } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
                    argbMax = 65535.0f;
                }

                QLag.setup2DQuinticLagrangianC(imgBuf, inVolExtents, argbMax, clip);

                for (i = 0; (i < oXdim) && !threadStopped; i++) {
                    imm = (float) i * oXres;
                    temp1 = (imm * T00) + T02;
                    temp2 = (imm * T10) + T12;

                    for (j = 0; (j < oYdim) && !threadStopped; j++) {

                        // convert to mm
                        value[0] = 0; // will remain zero if boundary conditions not met
                        value[1] = 0;
                        value[2] = 0;
                        value[3] = 0;

                        jmm = (float) j * oYres;

                        // transform i,j
                        X = (temp1 + (jmm * T01)) / iXres;

                        if ((X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (jmm * T11)) / iYres;

                            if ((Y >= 0) && (Y < iYdim)) {
                                value = QLag.quinticLagrangian2DC(X, Y);
                            }
                        }

                        temp3 = 4 * (i + (j * oXdim));
                        imgBuf2[temp3] = value[0];
                        imgBuf2[temp3 + 1] = value[1];
                        imgBuf2[temp3 + 2] = value[2];
                        imgBuf2[temp3 + 3] = value[3];
                        counter++;
                    } // for j
                } // for i

                try {
                    destImage.importData((4 * l * oXdim * oYdim * oZdim) + (4 * k * oXdim * oYdim), imgBuf2, true);
                } catch (IOException error) {
                    MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
                }

                if ((k < (oZdim - 1)) || (l < (oTdim - 1))) {

                    try {
                        srcImage.exportData((l * oZdim * imgLength) + ((k + 1) * imgLength), imgLength, imgBuf);
                    } catch (IOException error) {
                        displayError("Algorithm Transform: IOException Error on importData");
                        setCompleted(false);


                        return;
                    }
                } // end if ((k < (oZdim - 1))|| (l < (oTdim - 1)))
            } // end for k
        } // end for l

        QLag.finalize();
        QLag = null;
        Preferences.debug("finished quintic Lagrangian");
    }


    /**
     * Transforms and resamples a 3D volume using trilinear interpolation.
     *
     * @param  imgBuffer  Image array
     * @param  kTM       Transformation matrix to be applied
     */
    private void transformTrilinear(float[] imgBuffer, TransMatrix kTM) {
        int i, j, k;
        int iAdj, jAdj, kAdj;
        float[] tempBuf;
        float X, Y, Z;
        int x0, y0, z0;
        float value;
        float imm, jmm, kmm;
        float k1, k2, k3, j1, j2, j3;
        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        int deltaX, deltaY, deltaZ;

        int sliceSize;
        sliceSize = iXdim * iYdim;

        imgLength2 = oXdim * oYdim * oZdim;
        tempBuf = new float[imgLength2];

        int mod = Math.max(1, oZdim / 50);

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;
        // T30 = (float)xfrm[3][0]; T31 = (float)xfrm[3][1]; T32 = (float)xfrm[3][2]; T33 = (float)xfrm[3][3];

        int position1, position2;
        float b1, b2;
        float dx, dy, dz, dx1, dy1;

        float invXRes = 1.f / iXres;
        float invYRes = 1.f / iYres;
        float invZRes = 1.f / iZres;

        int index = 0;

        for (k = 0; (k < oZdim) && !threadStopped; k++) {

            if (((k % mod) == 0)) {
                fireProgressStateChanged((int) (((float) k / oZdim * 100) + 0.5f));
            }

            if (pad) {
                kAdj = k - margins[2];
            } else {
                kAdj = k;
            }

            kmm = kAdj * oZres;
            k1 = (kmm * T02) + T03;
            k2 = (kmm * T12) + T13;
            k3 = (kmm * T22) + T23;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                if (pad) {
                    jAdj = j - margins[1];
                } else {
                    jAdj = j;
                }

                jmm = jAdj * oYres;
                j1 = (jmm * T01) + k1;
                j2 = (jmm * T11) + k2;
                j3 = (jmm * T21) + k3;

                for (i = 0; (i < oXdim) && !threadStopped; i++) {

                    // transform i,j,k
                    if (pad) {
                        value = (float) padVal; // Either set by user in dialog or else set to 0 by JDialogTransform.
                        iAdj = i - margins[0];
                    } else {
                        value = (float) srcImage.getMin(); // Will remain zero if voxel is transformed out of bounds
                        iAdj = i;
                    }

                    imm = iAdj * oXres;
                    X = (j1 + (imm * T00)) * invXRes;
                    Y = (j2 + (imm * T10)) * invYRes;
                    Z = (j3 + (imm * T20)) * invZRes;

                    if ((X > -0.5f) && (X < iXdim)) {

                        if ((Y > -0.5f) && (Y < iYdim)) {

                            if ((Z > -0.5f) && (Z < iZdim)) {

                                if (X <= 0) {
                                    x0 = 0;
                                    dx = 0;
                                    deltaX = 0;
                                } else if (X >= (iXdim - 1)) {
                                    x0 = iXdim - 1;
                                    dx = 0;
                                    deltaX = 0;
                                } else {
                                    x0 = (int) X;
                                    dx = X - x0;
                                    deltaX = 1;
                                }

                                if (Y <= 0) {
                                    y0 = 0;
                                    dy = 0;
                                    deltaY = 0;
                                } else if (Y >= (iYdim - 1)) {
                                    y0 = iYdim - 1;
                                    dy = 0;
                                    deltaY = 0;
                                } else {
                                    y0 = (int) Y;
                                    dy = Y - y0;
                                    deltaY = iXdim;
                                }

                                if (Z <= 0) {
                                    z0 = 0;
                                    dz = 0;
                                    deltaZ = 0;
                                } else if (Z >= (iZdim - 1)) {
                                    z0 = iZdim - 1;
                                    dz = 0;
                                    deltaZ = 0;
                                } else {
                                    z0 = (int) Z;
                                    dz = Z - z0;
                                    deltaZ = sliceSize;
                                }

                                dx1 = 1 - dx;
                                dy1 = 1 - dy;

                                position1 = (z0 * sliceSize) + (y0 * iXdim) + x0;
                                position2 = position1 + deltaZ;

                                b1 = (dy1 * ((dx1 * imgBuffer[position1]) + (dx * imgBuffer[position1 + deltaX]))) +
                                     (dy *
                                          ((dx1 * imgBuffer[position1 + deltaY]) +
                                               (dx * imgBuffer[position1 + deltaY + deltaX])));

                                b2 = (dy1 * ((dx1 * imgBuffer[position2]) + (dx * imgBuffer[position2 + deltaX]))) +
                                     (dy *
                                          ((dx1 * imgBuffer[position2 + deltaY]) +
                                               (dx * imgBuffer[position2 + deltaY + deltaX])));

                                value = ((1 - dz) * b1) + (dz * b2);

                            } // end if Z in bounds
                        } // end if Y in bounds
                    } // end if X in bounds

                    tempBuf[index++] = value;
                } // end for i
            } // end for j
        } // end for k

        try {
            destImage.importData(0, tempBuf, false);
            tempBuf = null;
        } catch (IOException error) {
            displayError("AlgorithmTransform: IOException on destImage.importdata(0,imgBUf2, false).");

            setCompleted(false);

            return;
        }
    }

    /**
     * Transforms and resamples 4 dimensional object using trilinear interpolation.
     *
     * @param  imgBuffer  Image array
     * @param  kTM       Transformation matrix to be applied
     */
    private void transformTrilinear4D(float[] imgBuffer, TransMatrix kTM) {
        int i, j, k, l;
        float X, Y, Z;
        int x0, y0, z0;
        float value;
        int sliceSize;
        float imm, jmm, kmm;
        float k1, k2, k3, j1, j2, j3;

        sliceSize = iXdim * iYdim;

        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        int deltaX, deltaY, deltaZ;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;

        int position1, position2;
        float b1, b2;
        float dx, dy, dz, dx1, dy1;

        float invXRes = 1 / iXres;
        float invYRes = 1 / iYres;
        float invZRes = 1 / iZres;

        int index = 0;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged(Math.round((float) l / oTdim * 100));

            for (k = 0; (k < oZdim) && !threadStopped; k++) {
                kmm = k * oZres;
                k1 = (kmm * T02) + T03;
                k2 = (kmm * T12) + T13;
                k3 = (kmm * T22) + T23;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {
                    jmm = j * oYres;
                    j1 = (jmm * T01) + k1;
                    j2 = (jmm * T11) + k2;
                    j3 = (jmm * T21) + k3;

                    for (i = 0; (i < oXdim) && !threadStopped; i++) {

                        // transform i,j,k
                        value = (float) srcImage.getMin(); // remains zero if voxel is transformed out of bounds
                        imm = i * oXres;
                        X = (j1 + (imm * T00)) * invXRes;

                        if ((X > -0.5f) && (X < iXdim)) {
                            Y = (j2 + (imm * T10)) * invYRes;

                            if ((Y > -0.5f) && (Y < iYdim)) {
                                Z = (j3 + (imm * T20)) * invZRes;

                                if ((Z > -0.5f) && (Z < iZdim)) {

                                    if (X <= 0) {
                                        x0 = 0;
                                        dx = 0;
                                        deltaX = 0;
                                    } else if (X >= (iXdim - 1)) {
                                        x0 = iXdim - 1;
                                        dx = 0;
                                        deltaX = 0;
                                    } else {
                                        x0 = (int) X;
                                        dx = X - x0;
                                        deltaX = 1;
                                    }

                                    if (Y <= 0) {
                                        y0 = 0;
                                        dy = 0;
                                        deltaY = 0;
                                    } else if (Y >= (iYdim - 1)) {
                                        y0 = iYdim - 1;
                                        dy = 0;
                                        deltaY = 0;
                                    } else {
                                        y0 = (int) Y;
                                        dy = Y - y0;
                                        deltaY = iXdim;
                                    }

                                    if (Z <= 0) {
                                        z0 = 0;
                                        dz = 0;
                                        deltaZ = 0;
                                    } else if (Z >= (iZdim - 1)) {
                                        z0 = iZdim - 1;
                                        dz = 0;
                                        deltaZ = 0;
                                    } else {
                                        z0 = (int) Z;
                                        dz = Z - z0;
                                        deltaZ = sliceSize;
                                    }

                                    dx1 = 1 - dx;
                                    dy1 = 1 - dy;

                                    position1 = (z0 * sliceSize) + (y0 * iXdim) + x0;
                                    position2 = position1 + deltaZ;

                                    b1 = (dy1 * ((dx1 * imgBuffer[position1]) + (dx * imgBuffer[position1 + deltaX]))) +
                                         (dy *
                                              ((dx1 * imgBuffer[position1 + deltaY]) +
                                                   (dx * imgBuffer[position1 + deltaY + deltaX])));

                                    b2 = (dy1 * ((dx1 * imgBuffer[position2]) + (dx * imgBuffer[position2 + deltaX]))) +
                                         (dy *
                                              ((dx1 * imgBuffer[position2 + deltaY]) +
                                                   (dx * imgBuffer[position2 + deltaY + deltaX])));

                                    value = ((1 - dz) * b1) + (dz * b2);
                                } // end if Z in bounds
                            } // end if Y in bounds
                        } // end if X in bounds

                        destImage.set(index++, value);
                    } // end for i
                } // end for j
            } // end for k

            if (l < (oTdim - 1)) {

                try {
                    srcImage.exportData((l + 1) * imgLength, imgLength, imgBuf);
                } catch (IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);


                    return;
                }
            } // end if (l < (oTdim - 1))
        } // end for l
    }
    
    /**
     * Transforms and resamples 4 dimensional color object using trilinear interpolation.
     *
     * @param  imgBuffer  Image array
     * @param  imgBuf2
     * @param  kTM       Transformation matrix to be applied
     */
    private void transformTrilinear4DC(float[] imgBuffer, float [] imgBuffer2, TransMatrix kTM) {
        int i, j, k, l;
        float X, Y, Z;
        int x0, y0, z0;
        int temp;
        float value;
        int sliceSize;
        int oSliceSize;
        int oVolSize;
        float imm, jmm, kmm;
        float k1, k2, k3, j1, j2, j3;

        sliceSize = iXdim * iYdim;
        oSliceSize = oXdim * oYdim;
        oVolSize = oSliceSize * oZdim;

        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        int deltaX, deltaY, deltaZ;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;

        int position1, position2;
        float b1, b2;
        float dx, dy, dz, dx1, dy1;

        float invXRes = 1 / iXres;
        float invYRes = 1 / iYres;
        float invZRes = 1 / iZres;

        int index = 0;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged(Math.round((float) l / oTdim * 100));

            for (k = 0; (k < oZdim) && !threadStopped; k++) {
                kmm = k * oZres;
                k1 = (kmm * T02) + T03;
                k2 = (kmm * T12) + T13;
                k3 = (kmm * T22) + T23;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {
                    jmm = j * oYres;
                    j1 = (jmm * T01) + k1;
                    j2 = (jmm * T11) + k2;
                    j3 = (jmm * T21) + k3;

                    for (i = 0; (i < oXdim) && !threadStopped; i++) {

                        // transform i,j,k
                        temp = 4 * (i + (j * oXdim) + (k * oSliceSize));
                        imgBuffer2[temp] = 0; // remains zero if voxel is transformed out of bounds
                        imgBuffer2[temp + 1] = 0;
                        imgBuffer2[temp + 2] = 0;
                        imgBuffer2[temp + 3] = 0;
                        imm = i * oXres;
                        X = (j1 + (imm * T00)) * invXRes;

                        if ((X > -0.5f) && (X < iXdim)) {
                            Y = (j2 + (imm * T10)) * invYRes;

                            if ((Y > -0.5f) && (Y < iYdim)) {
                                Z = (j3 + (imm * T20)) * invZRes;

                                if ((Z > -0.5f) && (Z < iZdim)) {

                                    if (X <= 0) {
                                        x0 = 0;
                                        dx = 0;
                                        deltaX = 0;
                                    } else if (X >= (iXdim - 1)) {
                                        x0 = iXdim - 1;
                                        dx = 0;
                                        deltaX = 0;
                                    } else {
                                        x0 = (int) X;
                                        dx = X - x0;
                                        deltaX = 1;
                                    }

                                    if (Y <= 0) {
                                        y0 = 0;
                                        dy = 0;
                                        deltaY = 0;
                                    } else if (Y >= (iYdim - 1)) {
                                        y0 = iYdim - 1;
                                        dy = 0;
                                        deltaY = 0;
                                    } else {
                                        y0 = (int) Y;
                                        dy = Y - y0;
                                        deltaY = iXdim;
                                    }

                                    if (Z <= 0) {
                                        z0 = 0;
                                        dz = 0;
                                        deltaZ = 0;
                                    } else if (Z >= (iZdim - 1)) {
                                        z0 = iZdim - 1;
                                        dz = 0;
                                        deltaZ = 0;
                                    } else {
                                        z0 = (int) Z;
                                        dz = Z - z0;
                                        deltaZ = sliceSize;
                                    }

                                    dx1 = 1 - dx;
                                    dy1 = 1 - dy;

                                    position1 = (z0 * sliceSize) + (y0 * iXdim) + x0;
                                    position2 = position1 + deltaZ;

                                    b1 = (dy1 * ((dx1 * imgBuffer[4*position1]) + (dx * imgBuffer[4*(position1 + deltaX)]))) +
                                         (dy *
                                              ((dx1 * imgBuffer[4*(position1 + deltaY)]) +
                                                   (dx * imgBuffer[4*(position1 + deltaY + deltaX)])));

                                    b2 = (dy1 * ((dx1 * imgBuffer[4*position2]) + (dx * imgBuffer[4*(position2 + deltaX)]))) +
                                         (dy *
                                              ((dx1 * imgBuffer[4*(position2 + deltaY)]) +
                                                   (dx * imgBuffer[4*(position2 + deltaY + deltaX)])));

                                    imgBuffer2[temp] = ((1 - dz) * b1) + (dz * b2);
                                    
                                    b1 = (dy1 * ((dx1 * imgBuffer[4*position1+1]) + (dx * imgBuffer[4*(position1 + deltaX)+1]))) +
                                    (dy *
                                         ((dx1 * imgBuffer[4*(position1 + deltaY)+1]) +
                                              (dx * imgBuffer[4*(position1 + deltaY + deltaX)+1])));

                                    b2 = (dy1 * ((dx1 * imgBuffer[4*position2+1]) + (dx * imgBuffer[4*(position2 + deltaX)+1]))) +
                                    (dy *
                                         ((dx1 * imgBuffer[4*(position2 + deltaY)+1]) +
                                              (dx * imgBuffer[4*(position2 + deltaY + deltaX)+1])));

                                    imgBuffer2[temp+1] = ((1 - dz) * b1) + (dz * b2);
                                    
                                    b1 = (dy1 * ((dx1 * imgBuffer[4*position1+2]) + (dx * imgBuffer[4*(position1 + deltaX)+2]))) +
                                    (dy *
                                         ((dx1 * imgBuffer[4*(position1 + deltaY)+2]) +
                                              (dx * imgBuffer[4*(position1 + deltaY + deltaX)+2])));

                                    b2 = (dy1 * ((dx1 * imgBuffer[4*position2+2]) + (dx * imgBuffer[4*(position2 + deltaX)+2]))) +
                                    (dy *
                                         ((dx1 * imgBuffer[4*(position2 + deltaY)+2]) +
                                              (dx * imgBuffer[4*(position2 + deltaY + deltaX)+2])));

                                    imgBuffer2[temp+2] = ((1 - dz) * b1) + (dz * b2);
                                    
                                    b1 = (dy1 * ((dx1 * imgBuffer[4*position1+3]) + (dx * imgBuffer[4*(position1 + deltaX)+3]))) +
                                    (dy *
                                         ((dx1 * imgBuffer[4*(position1 + deltaY)+3]) +
                                              (dx * imgBuffer[4*(position1 + deltaY + deltaX)+3])));

                                    b2 = (dy1 * ((dx1 * imgBuffer[4*position2+3]) + (dx * imgBuffer[4*(position2 + deltaX)+3]))) +
                                    (dy *
                                         ((dx1 * imgBuffer[4*(position2 + deltaY)+3]) +
                                              (dx * imgBuffer[4*(position2 + deltaY + deltaX)+3])));

                                    imgBuffer2[temp+3] = ((1 - dz) * b1) + (dz * b2);
                                } // end if Z in bounds
                            } // end if Y in bounds
                        } // end if X in bounds
                    } // end for i
                } // end for j
            } // end for k
            
            try {
                destImage.importData(4 * l * oVolSize, imgBuffer2, false);
            }
            catch (IOException error) {
                MipavUtil.displayError("AlgorithmTransform: IOException on destImage.importData");
            }

            if (l < (oTdim - 1)) {

                try {
                    srcImage.exportData((l + 1) * imgLength, imgLength, imgBuf);
                } catch (IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);


                    return;
                }
            } // end if (l < (oTdim - 1))
        } // end for l
        destImage.calcMinMax();
    }

    /**
     * Transforms and resamples volume using trilinear interpolation This version is used with color images.
     *
     * @param  imgBuffer   Input image array
     * @param  imgBuffer2  Output image array
     * @param  kTM        Transformation matrix to be applied
     */
    private void transformTrilinearC(float[] imgBuffer, float[] imgBuffer2, TransMatrix kTM) {
        int i, j, k;
        int iAdj, jAdj, kAdj;
        float X, Y, Z;
        int x0, y0, z0;
        int sliceSize, osliceSize;
        float imm, jmm, kmm;
        float temp1, temp2, temp3;
        int temp8;
        int deltaX, deltaY, deltaZ;
        int position1, position2;
        float b1, b2;
        float dx, dy, dz, dx1, dy1;

        sliceSize = iXdim * iYdim;
        osliceSize = oXdim * oYdim;

        float i1, i2, i3, j1, j2, j3;
        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;

        for (i = 0; (i < oXdim) && !threadStopped; i++) {
            fireProgressStateChanged(Math.round((float) i / (oXdim) * 100));

            if (pad) {
                iAdj = i - margins[0];
            } else {
                iAdj = i;
            }

            imm = iAdj * oXres;
            i1 = (imm * T00) + T03;
            i2 = (imm * T10) + T13;
            i3 = (imm * T20) + T23;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                if (pad) {
                    jAdj = j - margins[1];
                } else {
                    jAdj = j;
                }

                jmm = jAdj * oYres;
                j1 = jmm * T01;
                j2 = jmm * T11;
                j3 = jmm * T21;
                temp1 = i3 + j3;
                temp2 = i2 + j2;
                temp3 = i1 + j1;

                for (k = 0; (k < oZdim) && !threadStopped; k++) {
                    
                    // transform i,j,k
                    temp8 = 4 * (i + (j * oXdim) + (k * osliceSize));

                    if (pad) {
                        imgBuffer2[temp8] = (float) padVal;
                        imgBuffer2[temp8 + 1] = (float) padVal;
                        imgBuffer2[temp8 + 2] = (float) padVal;
                        imgBuffer2[temp8 + 3] = (float) padVal;
                        kAdj = k - margins[2];
                    } else {
                        imgBuffer2[temp8] = 0; // remains zero if voxel is transformed out of bounds
                        imgBuffer2[temp8 + 1] = 0;
                        imgBuffer2[temp8 + 2] = 0;
                        imgBuffer2[temp8 + 3] = 0;
                        kAdj = k;
                    }

                    kmm = kAdj * oZres;
                    X = (temp3 + (kmm * T02)) / iXres;

                    if ((X > -0.5f) && (X < iXdim)) {
                        Y = (temp2 + (kmm * T12)) / iYres;

                        if ((Y > -0.5f) && (Y < iYdim)) {
                            temp1 = 0;
                            T22 = 1.0f;
                            Z = (temp1 + (kmm * T22)) / iZres;

                            if ((Z > -0.5f) && (Z < iZdim)) {
                                if (X <= 0) {
                                    x0 = 0;
                                    dx = 0;
                                    deltaX = 0;
                                } else if (X >= (iXdim - 1)) {
                                    x0 = iXdim - 1;
                                    dx = 0;
                                    deltaX = 0;
                                } else {
                                    x0 = (int) X;
                                    dx = X - x0;
                                    deltaX = 1;
                                }

                                if (Y <= 0) {
                                    y0 = 0;
                                    dy = 0;
                                    deltaY = 0;
                                } else if (Y >= (iYdim - 1)) {
                                    y0 = iYdim - 1;
                                    dy = 0;
                                    deltaY = 0;
                                } else {
                                    y0 = (int) Y;
                                    dy = Y - y0;
                                    deltaY = iXdim;
                                }

                                if (Z <= 0) {
                                    z0 = 0;
                                    dz = 0;
                                    deltaZ = 0;
                                } else if (Z >= (iZdim - 1)) {
                                    z0 = iZdim - 1;
                                    dz = 0;
                                    deltaZ = 0;
                                } else {
                                    z0 = (int) Z;
                                    dz = Z - z0;
                                    deltaZ = sliceSize;
                                }

                                dx1 = 1 - dx;
                                dy1 = 1 - dy;

                                position1 = (z0 * sliceSize) + (y0 * iXdim) + x0;
                                position2 = position1 + deltaZ;

                                b1 = (dy1 * ((dx1 * imgBuffer[4*position1]) + (dx * imgBuffer[4*(position1 + deltaX)]))) +
                                     (dy *
                                          ((dx1 * imgBuffer[4*(position1 + deltaY)]) +
                                               (dx * imgBuffer[4*(position1 + deltaY + deltaX)])));

                                b2 = (dy1 * ((dx1 * imgBuffer[4*position2]) + (dx * imgBuffer[4*(position2 + deltaX)]))) +
                                     (dy *
                                          ((dx1 * imgBuffer[4*(position2 + deltaY)]) +
                                               (dx * imgBuffer[4*(position2 + deltaY + deltaX)])));

                                imgBuffer2[temp8] = ((1 - dz) * b1) + (dz * b2);
                                
                                b1 = (dy1 * ((dx1 * imgBuffer[4*position1+ 1]) + (dx * imgBuffer[4*(position1 + deltaX)+1]))) +
                                (dy *
                                     ((dx1 * imgBuffer[4*(position1 + deltaY)+1]) +
                                          (dx * imgBuffer[4*(position1 + deltaY + deltaX)+1])));

                                b2 = (dy1 * ((dx1 * imgBuffer[4*position2+1]) + (dx * imgBuffer[4*(position2 + deltaX)+1]))) +
                                (dy *
                                     ((dx1 * imgBuffer[4*(position2 + deltaY)+1]) +
                                          (dx * imgBuffer[4*(position2 + deltaY + deltaX)+1])));

                                imgBuffer2[temp8+1] = ((1 - dz) * b1) + (dz * b2);
                                
                                b1 = (dy1 * ((dx1 * imgBuffer[4*position1+2]) + (dx * imgBuffer[4*(position1 + deltaX)+2]))) +
                                (dy *
                                     ((dx1 * imgBuffer[4*(position1 + deltaY)+2]) +
                                          (dx * imgBuffer[4*(position1 + deltaY + deltaX)+2])));

                                b2 = (dy1 * ((dx1 * imgBuffer[4*position2+2]) + (dx * imgBuffer[4*(position2 + deltaX)+2]))) +
                                (dy *
                                     ((dx1 * imgBuffer[4*(position2 + deltaY)+2]) +
                                          (dx * imgBuffer[4*(position2 + deltaY + deltaX)+2])));

                                 imgBuffer2[temp8+2] = ((1 - dz) * b1) + (dz * b2);
                                 
                                 b1 = (dy1 * ((dx1 * imgBuffer[4*position1+3]) + (dx * imgBuffer[4*(position1 + deltaX)+3]))) +
                                 (dy *
                                      ((dx1 * imgBuffer[4*(position1 + deltaY)+3]) +
                                           (dx * imgBuffer[4*(position1 + deltaY + deltaX)+3])));

                                 b2 = (dy1 * ((dx1 * imgBuffer[4*position2+3]) + (dx * imgBuffer[4*(position2 + deltaX)+3]))) +
                                 (dy *
                                      ((dx1 * imgBuffer[4*(position2 + deltaY)+3]) +
                                           (dx * imgBuffer[4*(position2 + deltaY + deltaX)+3])));

                                 imgBuffer2[temp8+3] = ((1 - dz) * b1) + (dz * b2);

                            } // end if Z in bounds
                        } // end if Y in bounds
                    } // end if X in bounds
                } // end for k
            } // end for j
        } // end for i

        if (threadStopped) {
            return;
        }

        try {
            destImage.importData(0, imgBuffer2, true);
        } catch (IOException error) {
            MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
        }
    }

    /**
     * Transforms and resamples volume using windowed sinc interpolation.
     *
     * @param  imgBuf  Image array
     * @param  kTM    Transformation matrix to be applied
     * @param  clip    If <code>true</code> clip output values to be within input range
     */
    private void transformWSinc2D(float[] imgBuf, TransMatrix kTM, boolean clip) {
        AlgorithmWSinc WSinc = new AlgorithmWSinc();
        int i, j;
        float X, Y;
        float value;
        float imm, jmm;
        int[] inVolExtents = { iXdim, iYdim };
        int mod = Math.max(1, oXdim / 50);
        int counter = 0; // used for progress bar
        float temp1, temp2;
        float T00, T01, T02, T10, T11, T12;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        WSinc.setup2DWSinc(imgBuf, inVolExtents, clip);

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged((int) (((float) i / oXdim * 100) + 0.5));
            }

            imm = (float) i * oXres;
            temp1 = (imm * T00) + T02;
            temp2 = (imm * T10) + T12;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                // convert to mm
                value = (float) srcImage.getMin(); // will remain zero if boundary conditions not met

                jmm = (float) j * oYres;

                // transform i,j
                X = (temp1 + (jmm * T01)) / iXres;

                if ((X >= 0) && (X < iXdim)) { // check bounds
                    Y = (temp2 + (jmm * T11)) / iYres;

                    if ((Y >= 0) && (Y < iYdim)) {
                        value = WSinc.wSinc2D(X, Y);
                    }
                }

                destImage.set(i, j, value);
                counter++;
            }
        }

        WSinc.finalize();
        WSinc = null;
        Preferences.debug("finished windowed sinc");
    }

    /**
     * Transforms and resamples volume using windowed sinc interpolation This version used with color images.
     *
     * @param  imgBuf   Input image array
     * @param  imgBuf2  Output image array
     * @param  kTM     Transformation matrix to be applied
     * @param  clip     If <code>true</code> clip output values to be within input range
     */
    private void transformWSinc2DC(float[] imgBuf, float[] imgBuf2, TransMatrix kTM, boolean clip) {
        AlgorithmWSinc WSinc = new AlgorithmWSinc();
        int i, j;
        float X, Y;
        float[] value = new float[4];
        float imm, jmm;
        int[] inVolExtents = { iXdim, iYdim };
        int mod = Math.max(1, oXdim / 50);
        int counter = 0; // used for progress bar
        float temp1, temp2;
        int temp3;
        float T00, T01, T02, T10, T11, T12;
        float argbMax = 255.0f;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        if (srcImage.getType() == ModelStorageBase.ARGB) {
            argbMax = 255.0f;
        } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
            argbMax = 65535.0f;
        }

        WSinc.setup2DWSincC(imgBuf, inVolExtents, argbMax, clip);

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged((int) (((float) i / oXdim * 100) + 0.5));
            }

            imm = (float) i * oXres;
            temp1 = (imm * T00) + T02;
            temp2 = (imm * T10) + T12;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                // convert to mm
                value[0] = 0; // will remain zero if boundary conditions not met
                value[1] = 0;
                value[2] = 0;
                value[3] = 0;

                jmm = (float) j * oYres;

                // transform i,j
                X = (temp1 + (jmm * T01)) / iXres;

                if ((X >= 0) && (X < iXdim)) { // check bounds
                    Y = (temp2 + (jmm * T11)) / iYres;

                    if ((Y >= 0) && (Y < iYdim)) {
                        value = WSinc.wSinc2DC(X, Y);
                    }
                }

                temp3 = 4 * (i + (j * oXdim));
                imgBuf2[temp3] = value[0];
                imgBuf2[temp3 + 1] = value[1];
                imgBuf2[temp3 + 2] = value[2];
                imgBuf2[temp3 + 3] = value[3];
                counter++;
            }
        }

        if (threadStopped) {
            return;
        }

        try {
            destImage.importData(0, imgBuf2, true);
        } catch (IOException error) {
            MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
        }

        WSinc.finalize();
        WSinc = null;
        Preferences.debug("finished windowed sinc");
    }

    /**
     * Transforms and resamples volume using windowed sinc interpolation.
     *
     * @param  imgBuf  Image array
     * @param  kTM    Transformation matrix to be applied
     * @param  clip    If <code>true</code> clip output values to be within input range
     */
    private void transformWSinc3D(float[] imgBuf, TransMatrix kTM, boolean clip) {
        AlgorithmWSinc WSinc = new AlgorithmWSinc();
        int i, j, k;
        float X, Y, Z;
        float value;
        int sliceSize;
        float imm, jmm, kmm;
        int[] inVolExtents = { iXdim, iYdim, iZdim };
        int mod = Math.max(1, oXdim / 50);
        int counter = 0; // used for progress bar

        sliceSize = iXdim * iYdim;

        float i1, i2, i3, j1, j2, j3;
        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;

        WSinc.setup3DWSinc(imgBuf, inVolExtents, clip);

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged((int) (((float) i / oXdim * 100) + 0.5));
            }

            imm = (float) i * oXres;
            i1 = (imm * T00) + T03;
            i2 = (imm * T10) + T13;
            i3 = (imm * T20) + T23;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {
                jmm = (float) j * oYres;
                j1 = (jmm * T01) + i1;
                j2 = (jmm * T11) + i2;
                j3 = (jmm * T21) + i3;

                for (k = 0; (k < oZdim) && !threadStopped; k++) {

                    // convert to mm
                    value = (float) srcImage.getMin(); // will remain zero if boundary conditions not met
                    kmm = (float) k * oZres;

                    // transform i,j,k
                    X = (j1 + (kmm * T02)) / iXres;

                    if ((X >= 0) && (X < iXdim)) { // check bounds
                        Y = (j2 + (kmm * T12)) / iYres;

                        if ((Y >= 0) && (Y < iYdim)) {
                            Z = (j3 + (kmm * T22)) / iZres;

                            if ((Z >= 0) && (Z < iZdim)) {
                                value = WSinc.wSinc3D(X, Y, Z);
                            }
                        }
                    }

                    destImage.set(i, j, k, value);
                    counter++;
                }
            }
        }

        WSinc.finalize();
        WSinc = null;
        Preferences.debug("finished windowed sinc");
    }

    /**
     * Transforms and resamples volume using windowed sinc interpolation This version used with color images.
     *
     * @param  imgBuf   Input image array
     * @param  imgBuf2  Output image array
     * @param  kTM     Transformation matrix to be applied
     * @param  clip     If <code>true</code> clip output values to be within input range
     */
    private void transformWSinc3DC(float[] imgBuf, float[] imgBuf2, TransMatrix kTM, boolean clip) {
        AlgorithmWSinc WSinc = new AlgorithmWSinc();
        int i, j, k;
        float X, Y, Z;
        float[] value = new float[4];
        int sliceSize;
        float imm, jmm, kmm;
        int[] inVolExtents = { iXdim, iYdim, iZdim };
        int mod = Math.max(1, oXdim / 50);
        int counter = 0; // used for progress bar

        sliceSize = iXdim * iYdim;

        int osliceSize = oXdim * oYdim;
        float i1, i2, i3, j1, j2, j3;
        float temp1, temp2, temp3;
        int temp4;
        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        float argbMax = 255.0f;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;

        if (srcImage.getType() == ModelStorageBase.ARGB) {
            argbMax = 255.0f;
        } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
            argbMax = 65535.0f;
        }

        WSinc.setup3DWSincC(imgBuf, inVolExtents, argbMax, clip);

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if (((i % mod) == 0)) {
                fireProgressStateChanged((int) (((float) i / oXdim * 100) + 0.5));
            }

            imm = (float) i * oXres;
            i1 = (imm * T00) + T03;
            i2 = (imm * T10) + T13;
            i3 = (imm * T20) + T23;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {
                jmm = (float) j * oYres;
                j1 = jmm * T01;
                j2 = jmm * T11;
                j3 = jmm * T21;
                temp1 = i3 + j3;
                temp2 = i2 + j2;
                temp3 = i1 + j1;

                for (k = 0; (k < oZdim) && !threadStopped; k++) {

                    // convert to mm
                    value[0] = 0; // will remain zero if boundary conditions not met
                    value[1] = 0;
                    value[2] = 0;
                    value[3] = 0;
                    kmm = (float) k * oZres;

                    // transform i,j,k
                    X = (temp3 + (kmm * T02)) / iXres;

                    // convert back to pixels
                    if ((X >= 0) && (X < iXdim)) { // check bounds
                        Y = (temp2 + (kmm * T12)) / iYres;

                        if ((Y >= 0) && (Y < iYdim)) {
                            Z = (temp1 + (kmm * T22)) / iZres;

                            if ((Z >= 0) && (Z < iZdim)) {
                                value = WSinc.wSinc3DC(X, Y, Z);
                            }
                        }
                    }

                    temp4 = 4 * (i + (j * oXdim) + (k * osliceSize));
                    imgBuf2[temp4] = value[0];
                    imgBuf2[temp4 + 1] = value[1];
                    imgBuf2[temp4 + 2] = value[2];
                    imgBuf2[temp4 + 3] = value[3];
                    counter++;
                }
            }
        }

        if (threadStopped) {
            return;
        }

        try {
            destImage.importData(0, imgBuf2, true);
        } catch (IOException error) {
            MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
        }

        WSinc.finalize();
        WSinc = null;
        Preferences.debug("finished windowed sinc");
    }

    /**
     * Transforms and resamples volume using windowed sinc interpolation Does a slice by slice windowed sinc
     * interpolation on a 3 dimensional object.
     *
     * @param  imgBuf  Image array
     * @param  kTM    Transformation matrix to be applied
     * @param  clip    If <code>true</code> clip output values to be within input range
     */
    private void transformWSinc3Dim2D(float[] imgBuf, TransMatrix kTM, boolean clip) {
        AlgorithmWSinc WSinc = new AlgorithmWSinc();
        int i, j, k;
        float X, Y;
        float value;
        int sliceSize;
        float imm, jmm;
        int[] inVolExtents = { iXdim, iYdim };

        sliceSize = iXdim * iYdim;

        int mod = Math.max(1, oZdim / 50);
        int counter = 0; // used for progress bar
        float temp1, temp2;
        float T00, T01, T02, T10, T11, T12;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        for (k = 0; (k < oZdim) && !threadStopped; k++) {

            if (((k % mod) == 0)) {
                fireProgressStateChanged((int) (((float) k / oZdim * 100) + 0.5));
            }

            WSinc.setup2DWSinc(imgBuf, inVolExtents, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                imm = (float) i * oXres;
                temp1 = (imm * T00) + T02;
                temp2 = (imm * T10) + T12;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {

                    // convert to mm
                    value = (float) srcImage.getMin(); // will remain zero if boundary conditions not met
                    jmm = (float) j * oYres;

                    // transform i,j
                    X = (temp1 + (jmm * T01)) / iXres;

                    if ((X >= 0) && (X < iXdim)) { // check bounds
                        Y = (temp2 + (jmm * T11)) / iYres;

                        if ((Y >= 0) && (Y < iYdim)) {
                            value = WSinc.wSinc2D(X, Y);
                        }
                    }

                    destImage.set(i, j, k, value);
                    counter++;
                }
            }

            if (k < (oZdim - 1)) {

                try {
                    srcImage.exportData((k + 1) * imgLength, imgLength, imgBuf);
                } catch (IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);


                    return;
                }
            } // end if (k < (oZdim - 1))
        } // end for k

        WSinc.finalize();
        WSinc = null;
        Preferences.debug("finished windowed sinc");
    }

    /**
     * Transforms and resamples volume using windowed sinc interpolation This version used with color images This
     * version performs a slice by slice algorithm on a 3 dimensional object.
     *
     * @param  imgBuf   Input image array
     * @param  imgBuf2  Output image array
     * @param  kTM     Transformation matrix to be applied
     * @param  clip     If <code>true</code> clip output values to be within input range
     */
    private void transformWSinc3Dim2DC(float[] imgBuf, float[] imgBuf2, TransMatrix kTM, boolean clip) {
        AlgorithmWSinc WSinc = new AlgorithmWSinc();
        int i, j, k;
        float X, Y;
        float[] value = new float[4];
        int sliceSize;
        float imm, jmm;
        int[] inVolExtents = { iXdim, iYdim };

        sliceSize = iXdim * iYdim;

        int mod = Math.max(1, oZdim / 50);
        int counter = 0; // used for progress bar
        float temp1, temp2;
        int temp3;
        float T00, T01, T02, T10, T11, T12;
        float argbMax = 255.0f;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        for (k = 0; (k < oZdim) && !threadStopped; k++) {

            if (((k % mod) == 0)) {
                fireProgressStateChanged((int) (((float) k / oZdim * 100) + 0.5));
            }

            if (srcImage.getType() == ModelStorageBase.ARGB) {
                argbMax = 255.0f;
            } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
                argbMax = 65535.0f;
            }

            WSinc.setup2DWSincC(imgBuf, inVolExtents, argbMax, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                imm = (float) i * oXres;
                temp1 = (imm * T00) + T02;
                temp2 = (imm * T10) + T12;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {

                    // convert to mm
                    value[0] = 0; // will remain zero if boundary conditions not met
                    value[1] = 0;
                    value[2] = 0;
                    value[3] = 0;

                    jmm = (float) j * oYres;

                    // transform i,j
                    X = (temp1 + (jmm * T01)) / iXres;

                    if ((X >= 0) && (X < iXdim)) { // check bounds
                        Y = (temp2 + (jmm * T11)) / iYres;

                        if ((Y >= 0) && (Y < iYdim)) {
                            value = WSinc.wSinc2DC(X, Y);
                        }
                    }

                    temp3 = 4 * (i + (j * oXdim));
                    imgBuf2[temp3] = value[0];
                    imgBuf2[temp3 + 1] = value[1];
                    imgBuf2[temp3 + 2] = value[2];
                    imgBuf2[temp3 + 3] = value[3];
                    counter++;
                } // for i
            } // for j

            try {
                destImage.importData(4 * k * oXdim * oYdim, imgBuf2, true);
            } catch (IOException error) {
                MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
            }

            if (k < (oZdim - 1)) {

                try {
                    srcImage.exportData((k + 1) * imgLength, imgLength, imgBuf);
                } catch (IOException error) {
                    displayError("Algorithm Transform: IOException Error on importData");
                    setCompleted(false);


                    return;
                }
            } // end if (k < (oZdim - 1))
        } // end for k

        WSinc.finalize();
        WSinc = null;
        Preferences.debug("finished windowed sinc");
    }

    /**
     * Transforms and resamples 4 dimensional object using 3D windowed sinc interpolation.
     *
     * @param  imgBuf  Image array
     * @param  kTM    Transformation matrix to be applied
     * @param  clip    If <code>true</code> clip output values to be within input range
     */
    private void transformWSinc4D(float[] imgBuf, TransMatrix kTM, boolean clip) {
        AlgorithmWSinc WSinc = new AlgorithmWSinc();
        int i, j, k, l;
        float X, Y, Z;
        float value;
        float imm, jmm, kmm;
        int[] inVolExtents = { iXdim, iYdim, iZdim };
        int mod = Math.max(1, oTdim / 50);
        int counter = 0; // used for progress bar

        float i1, i2, i3, j1, j2, j3;
        float temp1, temp2, temp3;
        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {

            if (((l % mod) == 0)) {
                fireProgressStateChanged((int) (((float) l / oTdim * 100) + .5));
            }

            WSinc.setup3DWSinc(imgBuf, inVolExtents, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                imm = (float) i * oXres;
                i1 = (imm * T00) + T03;
                i2 = (imm * T10) + T13;
                i3 = (imm * T20) + T23;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {
                    jmm = (float) j * oYres;
                    j1 = jmm * T01;
                    j2 = jmm * T11;
                    j3 = jmm * T21;
                    temp1 = i3 + j3;
                    temp2 = i2 + j2;
                    temp3 = i1 + j1;

                    for (k = 0; (k < oZdim) && !threadStopped; k++) {

                        // convert to mm
                        value = (float) srcImage.getMin(); // will remain zero if boundary conditions not met
                        kmm = (float) k * oZres;

                        // transform i,j,k
                        X = (temp3 + (kmm * T02)) / iXres;

                        if ((X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (kmm * T12)) / iYres;

                            if ((Y >= 0) && (Y < iYdim)) {
                                Z = (temp1 + (kmm * T22)) / iZres;

                                if ((Z >= 0) && (Z < iZdim)) {
                                    value = WSinc.wSinc3D(X, Y, Z);
                                }
                            }
                        }

                        destImage.set(i, j, k, l, value);
                        counter++;
                    }
                }
            }

            if (l < (oTdim - 1)) {

                try {
                    srcImage.exportData((l + 1) * imgLength, imgLength, imgBuf);
                } catch (IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);


                    return;
                }
            } // end if (l < (oTdim - 1))
        } // for l

        WSinc.finalize();
        WSinc = null;
        Preferences.debug("finished windowed sinc");
    }
    
    /**
     * Transforms and resamples 4 dimensional object using 3D windowed sinc interpolation.
     *
     * @param  imgBuf  Image array
     * @param  imgBuffer2
     * @param  kTM    Transformation matrix to be applied
     * @param  clip    If <code>true</code> clip output values to be within input range
     */
    private void transformWSinc4DC(float[] imgBuf, float [] imgBuffer2, TransMatrix kTM, boolean clip) {
        AlgorithmWSinc WSinc = new AlgorithmWSinc();
        int i, j, k, l;
        float X, Y, Z;
        float value[] = new float[4];
        int temp4;
        float imm, jmm, kmm;
        int[] inVolExtents = { iXdim, iYdim, iZdim };
        int oSliceSize;
        int oVolSize;
        int mod = Math.max(1, oTdim / 50);
        int counter = 0; // used for progress bar

        float i1, i2, i3, j1, j2, j3;
        float temp1, temp2, temp3;
        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        
        float argbMax = 255.0f;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T03 = kTM.M03;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;
        T13 = kTM.M13;
        T20 = kTM.M20;
        T21 = kTM.M21;
        T22 = kTM.M22;
        T23 = kTM.M23;
        
        oSliceSize = oXdim * oYdim;
        oVolSize = oSliceSize * oZdim;
        
        if (srcImage.getType() == ModelStorageBase.ARGB) {
            argbMax = 255.0f;
        }
        else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
            argbMax = 65535.0f;
        }

        for (l = 0; (l < oTdim) && !threadStopped; l++) {

            if (((l % mod) == 0)) {
                fireProgressStateChanged((int) (((float) l / oTdim * 100) + .5));
            }

            WSinc.setup3DWSincC(imgBuf, inVolExtents, argbMax, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                imm = (float) i * oXres;
                i1 = (imm * T00) + T03;
                i2 = (imm * T10) + T13;
                i3 = (imm * T20) + T23;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {
                    jmm = (float) j * oYres;
                    j1 = jmm * T01;
                    j2 = jmm * T11;
                    j3 = jmm * T21;
                    temp1 = i3 + j3;
                    temp2 = i2 + j2;
                    temp3 = i1 + j1;

                    for (k = 0; (k < oZdim) && !threadStopped; k++) {

                        // convert to mm
                        value[0] = 0; // will remain zero if boundary conditions not met
                        value[1] = 0;
                        value[2] = 0;
                        value[3] = 0;
                        kmm = (float) k * oZres;

                        // transform i,j,k
                        X = (temp3 + (kmm * T02)) / iXres;

                        if ((X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (kmm * T12)) / iYres;

                            if ((Y >= 0) && (Y < iYdim)) {
                                Z = (temp1 + (kmm * T22)) / iZres;

                                if ((Z >= 0) && (Z < iZdim)) {
                                    value = WSinc.wSinc3DC(X, Y, Z);
                                }
                            }
                        }

                        temp4 = 4 *(i + (j * oXdim) + (k * oSliceSize));
                        imgBuffer2[temp4] = value[0];
                        imgBuffer2[temp4 + 1] = value[1];
                        imgBuffer2[temp4 + 2] = value[2];
                        imgBuffer2[temp4 + 3] = value[3];
                        counter++;
                    }
                }
            }
            
            try {
                destImage.importData(4 * l * oVolSize, imgBuffer2, false);
            }
            catch (IOException error) {
                MipavUtil.displayError("AlgorithmTransform: IOException on destImage.importData");
            }

            if (l < (oTdim - 1)) {

                try {
                    srcImage.exportData((l + 1) * imgLength, imgLength, imgBuf);
                } catch (IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);


                    return;
                }
            } // end if (l < (oTdim - 1))
        } // for l

        destImage.calcMinMax();
        WSinc.finalize();
        WSinc = null;
        Preferences.debug("finished windowed sinc");
    }

    /**
     * Transforms and resamples volume using windowed sinc interpolation Does a slice by slice windowed sinc
     * interpolation on a 4 dimensional object.
     *
     * @param  imgBuf  Image array
     * @param  kTM    Transformation matrix to be applied
     * @param  clip    If <code>true</true> clip output values to be within input range</code>
     */
    private void transformWSinc4Dim2D(float[] imgBuf, TransMatrix kTM, boolean clip) {
        AlgorithmWSinc WSinc = new AlgorithmWSinc();
        int i, j, k, l;
        float X, Y;
        float value;
        int sliceSize;
        float imm, jmm;
        int[] inVolExtents = { iXdim, iYdim };

        sliceSize = iXdim * iYdim;

        int volSize = sliceSize * iZdim;
        int counter = 0; // used for progress bar
        float temp1, temp2;
        float T00, T01, T02, T10, T11, T12;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged((int) (((float) l / oTdim * 100) + 0.5));

            for (k = 0; (k < oZdim) && !threadStopped; k++) {
                WSinc.setup2DWSinc(imgBuf, inVolExtents, clip);

                for (i = 0; (i < oXdim) && !threadStopped; i++) {
                    imm = (float) i * oXres;
                    temp1 = (imm * T00) + T02;
                    temp2 = (imm * T10) + T12;

                    for (j = 0; (j < oYdim) && !threadStopped; j++) {

                        // convert to mm
                        value = (float) srcImage.getMin(); // will remain zero if boundary conditions not met
                        jmm = (float) j * oYres;

                        // transform i,j
                        X = (temp1 + (jmm * T01)) / iXres;

                        if ((X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (jmm * T11)) / iYres;

                            if ((Y >= 0) && (Y < iYdim)) {
                                value = WSinc.wSinc2D(X, Y);
                            }
                        }

                        destImage.set(i, j, k, l, value);
                        counter++;
                    }
                }

                if ((k < (oZdim - 1)) || (l < (oTdim - 1))) {

                    try {
                        srcImage.exportData((l * oZdim * imgLength) + ((k + 1) * imgLength), imgLength, imgBuf);
                    } catch (IOException error) {
                        displayError("Algorithm Transform: Image(s) locked");
                        setCompleted(false);


                        return;
                    }
                } // end if ((k < (oZdim - 1))|| (l < (oTdim - 1)))
            } // end for k
        } // end for l

        WSinc.finalize();
        WSinc = null;
        Preferences.debug("finished windowed sinc");
    }

    /**
     * Transforms and resamples volume using windowed sinc interpolation This version used with color images This
     * version performs a slice by slice algorithm on a 4 dimensional object.
     *
     * @param  imgBuf   Input image array
     * @param  imgBuf2  Output image array
     * @param  kTM     Transformation matrix to be applied
     * @param  clip     If <code>true</code> clip output values to be within input range
     */
    private void transformWSinc4Dim2DC(float[] imgBuf, float[] imgBuf2, TransMatrix kTM, boolean clip) {
        AlgorithmWSinc WSinc = new AlgorithmWSinc();
        int i, j, k, l;
        float X, Y;
        float[] value = new float[4];
        float imm, jmm;
        int[] inVolExtents = { iXdim, iYdim };

        int counter = 0; // used for progress bar
        float temp1, temp2;
        int temp3;
        float T00, T01, T02, T10, T11, T12;
        float argbMax = 255.0f;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged((int) (((float) l / oTdim * 100) + 0.5));

            for (k = 0; (k < oZdim) && !threadStopped; k++) {

                if (srcImage.getType() == ModelStorageBase.ARGB) {
                    argbMax = 255.0f;
                } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
                    argbMax = 65535.0f;
                }

                WSinc.setup2DWSincC(imgBuf, inVolExtents, argbMax, clip);

                for (i = 0; (i < oXdim) && !threadStopped; i++) {
                    imm = (float) i * oXres;
                    temp1 = (imm * T00) + T02;
                    temp2 = (imm * T10) + T12;

                    for (j = 0; (j < oYdim) && !threadStopped; j++) {

                        // convert to mm
                        value[0] = 0; // will remain zero if boundary conditions not met
                        value[1] = 0;
                        value[2] = 0;
                        value[3] = 0;

                        jmm = (float) j * oYres;

                        // transform i,j
                        X = (temp1 + (jmm * T01)) / iXres;

                        if ((X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (jmm * T11)) / iYres;

                            if ((Y >= 0) && (Y < iYdim)) {
                                value = WSinc.wSinc2DC(X, Y);
                            }
                        }

                        temp3 = 4 * (i + (j * oXdim));
                        imgBuf2[temp3] = value[0];
                        imgBuf2[temp3 + 1] = value[1];
                        imgBuf2[temp3 + 2] = value[2];
                        imgBuf2[temp3 + 3] = value[3];
                        counter++;
                    } // for i
                } // for j

                try {
                    destImage.importData((4 * l * oXdim * oYdim * oZdim) + (4 * k * oXdim * oYdim), imgBuf2, true);
                } catch (IOException error) {
                    MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
                }

                if ((k < (oZdim - 1)) || (l < (oTdim - 1))) {

                    try {
                        srcImage.exportData((l * oZdim * imgLength) + ((k + 1) * imgLength), imgLength, imgBuf);
                    } catch (IOException error) {
                        displayError("Algorithm Transform: IOException Error on importData");
                        setCompleted(false);


                        return;
                    }
                } // end if ((k < (oZdim - 1))|| (l < (oTdim - 1)))
            } // end for k
        } // end for l

        WSinc.finalize();
        WSinc = null;
        Preferences.debug("finished windowed sinc");
    }

    /**
     * Update origin to reflect padding.
     */
    private void updateOriginMargins() {
        float tx, ty, tz;

        tx = direct[0] * margins[0] * oXres;
        ty = direct[1] * margins[1] * oYres;
        tz = direct[2] * margins[2] * oZres;

        // System.out.println("Image origin before padding: " +imgOrigin[0] +" " +imgOrigin[1] +" " +imgOrigin[2]);
        imgOrigin[0] -= tx;
        imgOrigin[1] -= ty;
        imgOrigin[2] -= tz;
    }

    /**
     * Update origin to reflect padding.
     */
    private void updateOriginMargins2D() {
        float tx, ty;

        tx = direct[0] * margins[0] * oXres;
        ty = direct[1] * margins[1] * oYres;

        // System.out.println("Image origin before padding: " +imgOrigin[0] +" " +imgOrigin[1]);
        imgOrigin[0] -= tx;
        imgOrigin[1] -= ty;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * This is a port of code from a C language spline interpolation program by Philippe Thevenaz found at
     * http://bigwww.epfl.ch/thevenaz/interpolation. The C program is based on the paper: Philippe Thevenaz, Thierry
     * Blu, and Michael Unser, "Interpolation Revisited", IEEE Transactions on Medical Imaging, Vol. 19, No. 7, pp.
     * 739-785, July, 2000.
     */
    public class BSplineProcessing {

        /** ?? */
        private static final double DBL_EPSILON = 2.220446e-16;


        /**
         * 2D Spline interpolation routine.
         *
         * @param   coeff         input B-spline array of coefficients
         * @param   x             point to interpolate
         * @param   y             point to interpolate
         * @param   nx            image dimensions
         * @param   ny            image dimensions
         * @param   SplineDegree  degree of the spline model
         *
         * @return  DOCUMENT ME!
         */
        public final float interpolatedValue(float[][] coeff, double x, double y, int nx, int ny, int SplineDegree) {
            double[] xWeight = new double[6], yWeight = new double[6];
            double interpolated;
            double w, w2, w4, t, t0, t1;
            int[] xIndex = new int[6], yIndex = new int[6];
            int nx2 = (2 * nx) - 2, ny2 = (2 * ny) - 2;
            int i, j, k;

            // compute the interpolation indexes
            if ((SplineDegree % 2) == 1) {

                // System.out.print("spline degree type 1: "+SplineDegree+"\n");
                // odd degree
                i = (int) Math.floor(x) - (SplineDegree / 2);
                j = (int) Math.floor(y) - (SplineDegree / 2);

                for (k = 0; k <= SplineDegree; k++) {
                    xIndex[k] = i++;
                    yIndex[k] = j++;
                }
            } else {

                // System.out.print("spline degree type 2: "+SplineDegree+"\n");
                // even degree
                i = (int) Math.floor(x + 0.5) - (SplineDegree / 2);
                j = (int) Math.floor(y + 0.5) - (SplineDegree / 2);

                for (k = 0; k <= SplineDegree; k++) {
                    xIndex[k] = i++;
                    yIndex[k] = j++;
                }
            }

            // compute the interpolation weights
            switch (SplineDegree) {

                case 2:

                    // x
                    w = x - (double) xIndex[1];
                    xWeight[1] = (3.0 / 4.0) - (w * w);
                    xWeight[2] = (1.0 / 2.0) * (w - xWeight[1] + 1.0);
                    xWeight[0] = 1.0 - xWeight[1] - xWeight[2];

                    // y
                    w = y - (double) yIndex[1];
                    yWeight[1] = (3.0 / 4.0) - (w * w);
                    yWeight[2] = (1.0 / 2.0) * (w - yWeight[1] + 1.0);
                    yWeight[0] = 1.0 - yWeight[1] - yWeight[2];
                    break;

                case 3:

                    // x
                    w = x - (double) xIndex[1];
                    xWeight[3] = (1.0 / 6.0) * w * w * w;
                    xWeight[0] = (1.0 / 6.0) + ((1.0 / 2.0) * w * (w - 1.0)) - xWeight[3];
                    xWeight[2] = w + xWeight[0] - (2.0 * xWeight[3]);
                    xWeight[1] = 1.0 - xWeight[0] - xWeight[2] - xWeight[3];

                    // y
                    w = y - (double) yIndex[1];
                    yWeight[3] = (1.0 / 6.0) * w * w * w;
                    yWeight[0] = (1.0 / 6.0) + ((1.0 / 2.0) * w * (w - 1.0)) - yWeight[3];
                    yWeight[2] = w + yWeight[0] - (2.0 * yWeight[3]);
                    yWeight[1] = 1.0 - yWeight[0] - yWeight[2] - yWeight[3];
                    break;

                case 4:

                    // x
                    w = x - (double) xIndex[2];
                    w2 = w * w;
                    t = (1.0 / 6.0) * w2;
                    xWeight[0] = (1.0 / 2.0) - w;
                    xWeight[0] *= xWeight[0];
                    xWeight[0] *= (1.0 / 24.0) * xWeight[0];
                    t0 = w * (t - (11.0 / 24.0));
                    t1 = (19.0 / 96.0) + (w2 * ((1.0 / 4.0) - t));
                    xWeight[1] = t1 + t0;
                    xWeight[3] = t1 - t0;
                    xWeight[4] = xWeight[0] + t0 + ((1.0 / 2.0) * w);
                    xWeight[2] = 1.0 - xWeight[0] - xWeight[1] - xWeight[3] - xWeight[4];

                    // y
                    w = y - (double) yIndex[2];
                    w2 = w * w;
                    t = (1.0 / 6.0) * w2;
                    yWeight[0] = (1.0 / 2.0) - w;
                    yWeight[0] *= yWeight[0];
                    yWeight[0] *= (1.0 / 24.0) * yWeight[0];
                    t0 = w * (t - (11.0 / 24.0));
                    t1 = (19.0 / 96.0) + (w2 * ((1.0 / 4.0) - t));
                    yWeight[1] = t1 + t0;
                    yWeight[3] = t1 - t0;
                    yWeight[4] = yWeight[0] + t0 + ((1.0 / 2.0) * w);
                    yWeight[2] = 1.0 - yWeight[0] - yWeight[1] - yWeight[3] - yWeight[4];
                    break;

                case 5:

                    // x
                    w = x - (double) xIndex[2];
                    w2 = w * w;
                    xWeight[5] = (1.0 / 120.0) * w * w2 * w2;
                    w2 -= w;
                    w4 = w2 * w2;
                    w -= 1.0 / 2.0;
                    t = w2 * (w2 - 3.0);
                    xWeight[0] = ((1.0 / 24.0) * ((1.0 / 5.0) + w2 + w4)) - xWeight[5];
                    t0 = (1.0 / 24.0) * ((w2 * (w2 - 5.0)) + (46.0 / 5.0));
                    t1 = (-1.0 / 12.0) * w * (t + 4.0);
                    xWeight[2] = t0 + t1;
                    xWeight[3] = t0 - t1;
                    t0 = (1.0 / 16.0) * ((9.0 / 5.0) - t);
                    t1 = (1.0 / 24.0) * w * (w4 - w2 - 5.0);
                    xWeight[1] = t0 + t1;
                    xWeight[4] = t0 - t1;

                    // y
                    w = y - (double) yIndex[2];
                    w2 = w * w;
                    yWeight[5] = (1.0 / 120.0) * w * w2 * w2;
                    w2 -= w;
                    w4 = w2 * w2;
                    w -= 1.0 / 2.0;
                    t = w2 * (w2 - 3.0);
                    yWeight[0] = ((1.0 / 24.0) * ((1.0 / 5.0) + w2 + w4)) - yWeight[5];
                    t0 = (1.0 / 24.0) * ((w2 * (w2 - 5.0)) + (46.0 / 5.0));
                    t1 = (-1.0 / 12.0) * w * (t + 4.0);
                    yWeight[2] = t0 + t1;
                    yWeight[3] = t0 - t1;
                    t0 = (1.0 / 16.0) * ((9.0 / 5.0) - t);
                    t1 = (1.0 / 24.0) * w * (w4 - w2 - 5.0);
                    yWeight[1] = t0 + t1;
                    yWeight[4] = t0 - t1;
                    break;

                default:
                    System.out.print("Invalid spline degree\n");

                    return (0.0f);
            }

            // apply the mirror boundary conditions
            for (k = 0; k <= SplineDegree; k++) {
                xIndex[k] = (nx == 1)
                            ? (0)
                            : ((xIndex[k] < 0) ? (-xIndex[k] - (nx2 * ((-xIndex[k]) / nx2)))
                                               : (xIndex[k] - (nx2 * (xIndex[k] / nx2))));

                if (nx <= xIndex[k]) {
                    xIndex[k] = nx2 - xIndex[k];
                }

                yIndex[k] = (ny == 1)
                            ? (0)
                            : ((yIndex[k] < 0) ? (-yIndex[k] - (ny2 * ((-yIndex[k]) / ny2)))
                                               : (yIndex[k] - (ny2 * (yIndex[k] / ny2))));

                if (ny <= yIndex[k]) {
                    yIndex[k] = ny2 - yIndex[k];
                }
            }

            // perform interpolation
            // System.out.print("["+x+","+y+"] ");
            interpolated = 0.0;

            for (j = 0; j <= SplineDegree; j++) {
                w = 0.0;

                for (i = 0; i <= SplineDegree; i++) {
                    w += xWeight[i] * coeff[xIndex[i]][yIndex[j]];
                    // System.out.print("("+xIndex[i]+","+yIndex[j]+"|"+coeff[xIndex[i]][yIndex[j]]+")");
                }

                interpolated += yWeight[j] * w;
            }
            // System.out.println("->"+interpolated);

            return (float) (interpolated);
        } // end InterpolatedValue

        /**
         * 3D Spline interpolation routine.
         *
         * @param   coeff         input B-spline array of coefficients
         * @param   x             point to interpolate
         * @param   y             point to interpolate
         * @param   z             point to interpolate
         * @param   nx            image dimensions
         * @param   ny            image dimensions
         * @param   nz            image dimensions
         * @param   SplineDegree  degree of the spline model
         *
         * @return  DOCUMENT ME!
         */
        public final float interpolatedValue(float[][][] coeff, double x, double y, double z, int nx, int ny, int nz,
                                             int SplineDegree) {
            double[] xWeight = new double[6], yWeight = new double[6], zWeight = new double[6];
            double interpolated;
            double w, w2, w4, t, t0, t1;
            int[] xIndex = new int[6], yIndex = new int[6], zIndex = new int[6];
            int nx2 = (2 * nx) - 2, ny2 = (2 * ny) - 2, nz2 = (2 * nz) - 2;
            int i, j, k, l;

            // compute the interpolation indexes
            if ((SplineDegree % 2) == 1) {
                i = (int) Math.floor(x) - (SplineDegree / 2);
                j = (int) Math.floor(y) - (SplineDegree / 2);
                l = (int) Math.floor(z) - (SplineDegree / 2);

                for (k = 0; k <= SplineDegree; k++) {
                    xIndex[k] = i++;
                    yIndex[k] = j++;
                    zIndex[k] = l++;
                }
            } else {
                i = (int) Math.floor(x + 0.5) - (SplineDegree / 2);
                j = (int) Math.floor(y + 0.5) - (SplineDegree / 2);
                l = (int) Math.floor(z + 0.5) - (SplineDegree / 2);

                for (k = 0; k <= SplineDegree; k++) {
                    xIndex[k] = i++;
                    yIndex[k] = j++;
                    zIndex[k] = l++;
                }
            }

            // compute the interpolation weights
            switch (SplineDegree) {

                case 2:

                    // x
                    w = x - (double) xIndex[1];
                    xWeight[1] = (3.0 / 4.0) - (w * w);
                    xWeight[2] = (1.0 / 2.0) * (w - xWeight[1] + 1.0);
                    xWeight[0] = 1.0 - xWeight[1] - xWeight[2];

                    // y
                    w = y - (double) yIndex[1];
                    yWeight[1] = (3.0 / 4.0) - (w * w);
                    yWeight[2] = (1.0 / 2.0) * (w - yWeight[1] + 1.0);
                    yWeight[0] = 1.0 - yWeight[1] - yWeight[2];

                    // z
                    w = z - (double) zIndex[1];
                    zWeight[1] = (3.0 / 4.0) - (w * w);
                    zWeight[2] = (1.0 / 2.0) * (w - zWeight[1] + 1.0);
                    zWeight[0] = 1.0 - zWeight[1] - zWeight[2];
                    break;

                case 3:

                    // x
                    w = x - (double) xIndex[1];
                    xWeight[3] = (1.0 / 6.0) * w * w * w;
                    xWeight[0] = (1.0 / 6.0) + ((1.0 / 2.0) * w * (w - 1.0)) - xWeight[3];
                    xWeight[2] = w + xWeight[0] - (2.0 * xWeight[3]);
                    xWeight[1] = 1.0 - xWeight[0] - xWeight[2] - xWeight[3];

                    // y
                    w = y - (double) yIndex[1];
                    yWeight[3] = (1.0 / 6.0) * w * w * w;
                    yWeight[0] = (1.0 / 6.0) + ((1.0 / 2.0) * w * (w - 1.0)) - yWeight[3];
                    yWeight[2] = w + yWeight[0] - (2.0 * yWeight[3]);
                    yWeight[1] = 1.0 - yWeight[0] - yWeight[2] - yWeight[3];

                    // z
                    w = z - (double) zIndex[1];
                    zWeight[3] = (1.0 / 6.0) * w * w * w;
                    zWeight[0] = (1.0 / 6.0) + ((1.0 / 2.0) * w * (w - 1.0)) - zWeight[3];
                    zWeight[2] = w + zWeight[0] - (2.0 * zWeight[3]);
                    zWeight[1] = 1.0 - zWeight[0] - zWeight[2] - zWeight[3];
                    break;

                case 4:

                    // x
                    w = x - (double) xIndex[2];
                    w2 = w * w;
                    t = (1.0 / 6.0) * w2;
                    xWeight[0] = (1.0 / 2.0) - w;
                    xWeight[0] *= xWeight[0];
                    xWeight[0] *= (1.0 / 24.0) * xWeight[0];
                    t0 = w * (t - (11.0 / 24.0));
                    t1 = (19.0 / 96.0) + (w2 * ((1.0 / 4.0) - t));
                    xWeight[1] = t1 + t0;
                    xWeight[3] = t1 - t0;
                    xWeight[4] = xWeight[0] + t0 + ((1.0 / 2.0) * w);
                    xWeight[2] = 1.0 - xWeight[0] - xWeight[1] - xWeight[3] - xWeight[4];

                    // y
                    w = y - (double) yIndex[2];
                    w2 = w * w;
                    t = (1.0 / 6.0) * w2;
                    yWeight[0] = (1.0 / 2.0) - w;
                    yWeight[0] *= yWeight[0];
                    yWeight[0] *= (1.0 / 24.0) * yWeight[0];
                    t0 = w * (t - (11.0 / 24.0));
                    t1 = (19.0 / 96.0) + (w2 * ((1.0 / 4.0) - t));
                    yWeight[1] = t1 + t0;
                    yWeight[3] = t1 - t0;
                    yWeight[4] = yWeight[0] + t0 + ((1.0 / 2.0) * w);
                    yWeight[2] = 1.0 - yWeight[0] - yWeight[1] - yWeight[3] - yWeight[4];

                    // z
                    w = z - (double) zIndex[2];
                    w2 = w * w;
                    t = (1.0 / 6.0) * w2;
                    zWeight[0] = (1.0 / 2.0) - w;
                    zWeight[0] *= zWeight[0];
                    zWeight[0] *= (1.0 / 24.0) * zWeight[0];
                    t0 = w * (t - (11.0 / 24.0));
                    t1 = (19.0 / 96.0) + (w2 * ((1.0 / 4.0) - t));
                    zWeight[1] = t1 + t0;
                    zWeight[3] = t1 - t0;
                    zWeight[4] = zWeight[0] + t0 + ((1.0 / 2.0) * w);
                    zWeight[2] = 1.0 - zWeight[0] - zWeight[1] - zWeight[3] - zWeight[4];
                    break;

                case 5:

                    // x
                    w = x - (double) xIndex[2];
                    w2 = w * w;
                    xWeight[5] = (1.0 / 120.0) * w * w2 * w2;
                    w2 -= w;
                    w4 = w2 * w2;
                    w -= 1.0 / 2.0;
                    t = w2 * (w2 - 3.0);
                    xWeight[0] = ((1.0 / 24.0) * ((1.0 / 5.0) + w2 + w4)) - xWeight[5];
                    t0 = (1.0 / 24.0) * ((w2 * (w2 - 5.0)) + (46.0 / 5.0));
                    t1 = (-1.0 / 12.0) * w * (t + 4.0);
                    xWeight[2] = t0 + t1;
                    xWeight[3] = t0 - t1;
                    t0 = (1.0 / 16.0) * ((9.0 / 5.0) - t);
                    t1 = (1.0 / 24.0) * w * (w4 - w2 - 5.0);
                    xWeight[1] = t0 + t1;
                    xWeight[4] = t0 - t1;

                    // y
                    w = y - (double) yIndex[2];
                    w2 = w * w;
                    yWeight[5] = (1.0 / 120.0) * w * w2 * w2;
                    w2 -= w;
                    w4 = w2 * w2;
                    w -= 1.0 / 2.0;
                    t = w2 * (w2 - 3.0);
                    yWeight[0] = ((1.0 / 24.0) * ((1.0 / 5.0) + w2 + w4)) - yWeight[5];
                    t0 = (1.0 / 24.0) * ((w2 * (w2 - 5.0)) + (46.0 / 5.0));
                    t1 = (-1.0 / 12.0) * w * (t + 4.0);
                    yWeight[2] = t0 + t1;
                    yWeight[3] = t0 - t1;
                    t0 = (1.0 / 16.0) * ((9.0 / 5.0) - t);
                    t1 = (1.0 / 24.0) * w * (w4 - w2 - 5.0);
                    yWeight[1] = t0 + t1;
                    yWeight[4] = t0 - t1;

                    // z
                    w = z - (double) zIndex[2];
                    w2 = w * w;
                    zWeight[5] = (1.0 / 120.0) * w * w2 * w2;
                    w2 -= w;
                    w4 = w2 * w2;
                    w -= 1.0 / 2.0;
                    t = w2 * (w2 - 3.0);
                    zWeight[0] = ((1.0 / 24.0) * ((1.0 / 5.0) + w2 + w4)) - zWeight[5];
                    t0 = (1.0 / 24.0) * ((w2 * (w2 - 5.0)) + (46.0 / 5.0));
                    t1 = (-1.0 / 12.0) * w * (t + 4.0);
                    zWeight[2] = t0 + t1;
                    zWeight[3] = t0 - t1;
                    t0 = (1.0 / 16.0) * ((9.0 / 5.0) - t);
                    t1 = (1.0 / 24.0) * w * (w4 - w2 - 5.0);
                    zWeight[1] = t0 + t1;
                    zWeight[4] = t0 - t1;
                    break;

                default:
                    System.out.print("Invalid spline degree\n");

                    return (0.0f);
            }

            // apply the mirror boundary conditions
            for (k = 0; k <= SplineDegree; k++) {
                xIndex[k] = (nx == 1)
                            ? (0)
                            : ((xIndex[k] < 0) ? (-xIndex[k] - (nx2 * ((-xIndex[k]) / nx2)))
                                               : (xIndex[k] - (nx2 * (xIndex[k] / nx2))));

                if (nx <= xIndex[k]) {
                    xIndex[k] = nx2 - xIndex[k];
                }

                yIndex[k] = (ny == 1)
                            ? (0)
                            : ((yIndex[k] < 0) ? (-yIndex[k] - (ny2 * ((-yIndex[k]) / ny2)))
                                               : (yIndex[k] - (ny2 * (yIndex[k] / ny2))));

                if (ny <= yIndex[k]) {
                    yIndex[k] = ny2 - yIndex[k];
                }

                zIndex[k] = (nz == 1)
                            ? (0)
                            : ((zIndex[k] < 0) ? (-zIndex[k] - (nz2 * ((-zIndex[k]) / nz2)))
                                               : (zIndex[k] - (nz2 * (zIndex[k] / nz2))));

                if (nz <= zIndex[k]) {
                    zIndex[k] = nz2 - zIndex[k];
                }
            }

            // perform interpolation
            interpolated = 0.0;

            for (i = 0; i <= SplineDegree; i++) {

                for (j = 0; j <= SplineDegree; j++) {

                    for (l = 0; l <= SplineDegree; l++) {
                        interpolated += xWeight[i] * yWeight[j] * zWeight[l] * coeff[xIndex[i]][yIndex[j]][zIndex[l]];
                    }
                }
            }

            return (float) (interpolated);
        } // end InterpolatedValue

        /**
         * main function for transferring 2D image samples into spline coefficients.
         *
         * @param   Image         in-place processing
         * @param   nx            image dimensions
         * @param   ny            image dimensions
         * @param   SplineDegree  degree of the spline model
         *
         * @return  DOCUMENT ME!
         */
        public final int samplesToCoefficients(float[][] Image, int nx, int ny, int SplineDegree) {
            double[] Line;
            double[] pole = new double[2];
            int Npoles;

            // recover the poles from a lookup table
            switch (SplineDegree) {

                case 2:
                    Npoles = 1;
                    pole[0] = Math.sqrt(8.0) - 3.0;
                    break;

                case 3:
                    Npoles = 1;
                    pole[0] = Math.sqrt(3.0) - 2.0;
                    break;

                case 4:
                    Npoles = 2;
                    pole[0] = Math.sqrt(664.0 - Math.sqrt(438976.0)) + Math.sqrt(304.0) - 19.0;
                    pole[1] = Math.sqrt(664.0 + Math.sqrt(438976.0)) - Math.sqrt(304.0) - 19.0;
                    break;

                case 5:
                    Npoles = 2;
                    pole[0] = Math.sqrt((135.0 / 2.0) - Math.sqrt(17745.0 / 4.0)) + Math.sqrt(105.0 / 4.0) -
                              (13.0 / 2.0);
                    pole[1] = Math.sqrt((135.0 / 2.0) + Math.sqrt(17745.0 / 4.0)) - Math.sqrt(105.0 / 4.0) -
                              (13.0 / 2.0);
                    break;

                default:
                    System.out.print("Invalid spline degree\n");

                    return (1);
            }

            // convert the image samples into interpolation coefficients
            // in-place separable process, along x
            Line = new double[nx];

            for (int y = 0; y < ny; y++) {
                getRow(Image, y, nx, Line);
                convertToInterpolationCoefficients(Line, nx, pole, Npoles, DBL_EPSILON);
                putRow(Image, y, nx, Line);
            }

            Line = null;

            // in-place separable process, along y
            Line = new double[ny];

            for (int x = 0; x < nx; x++) {
                getColumn(Image, x, ny, Line);
                convertToInterpolationCoefficients(Line, ny, pole, Npoles, DBL_EPSILON);
                putColumn(Image, x, ny, Line);
            }

            Line = null;

            return (0);
        } // samplesToCoefficients

        /**
         * main function for transferring 3D image samples into spline coefficients.
         *
         * @param   Image         in-place processing
         * @param   nx            image dimensions
         * @param   ny            image dimensions
         * @param   nz            image dimensions
         * @param   SplineDegree  degree of the spline model
         *
         * @return  DOCUMENT ME!
         */
        public final int samplesToCoefficients(float[][][] Image, int nx, int ny, int nz, int SplineDegree) {
            double[] Line;
            double[] pole = new double[2];
            int Npoles;

            // recover the poles from a lookup table
            switch (SplineDegree) {

                case 2:
                    Npoles = 1;
                    pole[0] = Math.sqrt(8.0) - 3.0;
                    break;

                case 3:
                    Npoles = 1;
                    pole[0] = Math.sqrt(3.0) - 2.0;
                    break;

                case 4:
                    Npoles = 2;
                    pole[0] = Math.sqrt(664.0 - Math.sqrt(438976.0)) + Math.sqrt(304.0) - 19.0;
                    pole[1] = Math.sqrt(664.0 + Math.sqrt(438976.0)) - Math.sqrt(304.0) - 19.0;
                    break;

                case 5:
                    Npoles = 2;
                    pole[0] = Math.sqrt((135.0 / 2.0) - Math.sqrt(17745.0 / 4.0)) + Math.sqrt(105.0 / 4.0) -
                              (13.0 / 2.0);
                    pole[1] = Math.sqrt((135.0 / 2.0) + Math.sqrt(17745.0 / 4.0)) - Math.sqrt(105.0 / 4.0) -
                              (13.0 / 2.0);
                    break;

                default:
                    System.out.print("Invalid spline degree\n");

                    return (1);
            }

            // convert the image samples into interpolation coefficients
            // in-place separable process, along x
            Line = new double[nx];

            for (int y = 0; y < ny; y++) {

                for (int z = 0; z < nz; z++) {
                    getRow(Image, y, z, nx, Line);
                    convertToInterpolationCoefficients(Line, nx, pole, Npoles, DBL_EPSILON);
                    putRow(Image, y, z, nx, Line);
                }
            }

            Line = null;

            // in-place separable process, along y
            Line = new double[ny];

            for (int x = 0; x < nx; x++) {

                for (int z = 0; z < nz; z++) {
                    getColumn(Image, x, z, ny, Line);
                    convertToInterpolationCoefficients(Line, ny, pole, Npoles, DBL_EPSILON);
                    putColumn(Image, x, z, ny, Line);
                }
            }

            Line = null;

            // in-place separable process, along z
            Line = new double[nz];

            for (int x = 0; x < nx; x++) {

                for (int y = 0; y < ny; y++) {
                    getStack(Image, x, y, nz, Line);
                    convertToInterpolationCoefficients(Line, nz, pole, Npoles, DBL_EPSILON);
                    putStack(Image, x, y, nz, Line);
                }
            }

            Line = null;

            return (0);
        } // samplesToCoefficients

        /**
         * to convert data points to spline coefficients.
         *
         * @param  coeff      : input samples --> output coefficients
         * @param  Ndata      : number of samples or coefficients
         * @param  pole       : poles
         * @param  Npoles     : number of poles
         * @param  Tolerance  : admissible relative error
         */
        private void convertToInterpolationCoefficients(double[] coeff, int Ndata, double[] pole, int Npoles,
                                                        double Tolerance) {
            double Lambda = 1.0;

            // special case required by mirror boundaries
            if (Ndata == 1) {
                return;
            }

            // compute the overall gain
            for (int k = 0; k < Npoles; k++) {
                Lambda = Lambda * (1.0 - pole[k]) * (1.0 - (1.0 / pole[k]));
            }

            // apply the gain
            for (int n = 0; n < Ndata; n++) {
                coeff[n] *= Lambda;
            }

            // loop over all poles
            for (int k = 0; k < Npoles; k++) {

                // causal initialization
                coeff[0] = initialCausalCoefficient(coeff, Ndata, pole[k], Tolerance);

                // causal recursion
                for (int n = 1; n < Ndata; n++) {
                    coeff[n] += pole[k] * coeff[n - 1];
                }

                // anticausal initialization
                coeff[Ndata - 1] = initialAntiCausalCoefficient(coeff, Ndata, pole[k]);

                // anticausal recursion
                for (int n = Ndata - 2; 0 <= n; n--) {
                    coeff[n] = pole[k] * (coeff[n + 1] - coeff[n]);
                }
            }
        } // convertToInterpolationCoefficients

        /**
         * extract a column from a 2D image.
         *
         * @param  Image  DOCUMENT ME!
         * @param  x      DOCUMENT ME!
         * @param  ny     DOCUMENT ME!
         * @param  Line   DOCUMENT ME!
         */
        private void getColumn(float[][] Image, int x, int ny, double[] Line) {

            for (int y = 0; y < ny; y++) {
                Line[y] = (double) Image[x][y];
            }
        } // getColumn

        /**
         * extract a column from a 3D image.
         *
         * @param  Image  DOCUMENT ME!
         * @param  x      DOCUMENT ME!
         * @param  z      DOCUMENT ME!
         * @param  ny     DOCUMENT ME!
         * @param  Line   DOCUMENT ME!
         */
        private void getColumn(float[][][] Image, int x, int z, int ny, double[] Line) {

            for (int y = 0; y < ny; y++) {
                Line[y] = (double) Image[x][y][z];
            }
        } // getColumn

        /**
         * extract a row from a 2D image.
         *
         * @param  Image  DOCUMENT ME!
         * @param  y      DOCUMENT ME!
         * @param  nx     DOCUMENT ME!
         * @param  Line   DOCUMENT ME!
         */
        private void getRow(float[][] Image, int y, int nx, double[] Line) {

            for (int x = 0; x < nx; x++) {
                Line[x] = (double) Image[x][y];
            }
        } // getRow

        /**
         * extract a row from a 3D image.
         *
         * @param  Image  DOCUMENT ME!
         * @param  y      DOCUMENT ME!
         * @param  z      DOCUMENT ME!
         * @param  nx     DOCUMENT ME!
         * @param  Line   DOCUMENT ME!
         */
        private void getRow(float[][][] Image, int y, int z, int nx, double[] Line) {

            for (int x = 0; x < nx; x++) {
                Line[x] = (double) Image[x][y][z];
            }
        } // getRow

        /**
         * extract a stack (Z direction) from a 3D image.
         *
         * @param  Image  DOCUMENT ME!
         * @param  x      DOCUMENT ME!
         * @param  y      DOCUMENT ME!
         * @param  nz     DOCUMENT ME!
         * @param  Line   DOCUMENT ME!
         */
        private void getStack(float[][][] Image, int x, int y, int nz, double[] Line) {

            for (int z = 0; z < nz; z++) {
                Line[z] = (double) Image[x][y][z];
            }
        } // getStack

        /**
         * spline subroutine.
         *
         * @param   coeff  : coefficients
         * @param   Ndata  : number of coefficients
         * @param   pole   : actual pole
         *
         * @return  DOCUMENT ME!
         */
        private double initialAntiCausalCoefficient(double[] coeff, int Ndata, double pole) {

            // this initialization corresponds to mirror boundaries
            return ((pole / ((pole * pole) - 1.0)) * ((pole * coeff[Ndata - 2]) + coeff[Ndata - 1]));
        } // initialAntiCausalCoefficient

        /**
         * spline subroutine.
         *
         * @param   coeff      : coefficients
         * @param   Ndata      : number of coefficients
         * @param   pole       : actual pole
         * @param   Tolerance  : admissible relative error
         *
         * @return  DOCUMENT ME!
         */
        private double initialCausalCoefficient(double[] coeff, int Ndata, double pole, double Tolerance) {
            double Sum, zn, z2n, iz;
            int Horizon;

            // this initialization corresponds to mirror boundaries
            Horizon = Ndata;

            if (Tolerance > 0.0) {
                Horizon = (int) Math.ceil(Math.log(Tolerance) / Math.log(Math.abs(pole)));
            }

            if (Horizon < Ndata) {

                // accelerated loop
                zn = pole;
                Sum = coeff[0];

                for (int n = 1; n < Horizon; n++) {
                    Sum += zn * coeff[n];
                    zn *= pole;
                }

                return (Sum);
            } else {

                // full loop
                zn = pole;
                iz = 1.0f / pole;
                z2n = Math.pow(pole, (double) (Ndata - 1));
                Sum = coeff[0] + (z2n * coeff[Ndata - 1]);
                z2n *= z2n * iz;

                for (int n = 1; n <= (Ndata - 2); n++) {
                    Sum += (zn + z2n) * coeff[n];
                    zn *= pole;
                    z2n *= iz;
                }

                return (Sum / (1.0f - (zn * zn)));
            }
        } // initialCausalCoefficient

        /**
         * write a column in a 2D image.
         *
         * @param  Image  DOCUMENT ME!
         * @param  x      DOCUMENT ME!
         * @param  ny     DOCUMENT ME!
         * @param  Line   DOCUMENT ME!
         */
        private void putColumn(float[][] Image, int x, int ny, double[] Line) {

            for (int y = 0; y < ny; y++) {
                Image[x][y] = (float) Line[y];
            }
        } // putColumn

        /**
         * write a column in a 3D image.
         *
         * @param  Image  DOCUMENT ME!
         * @param  x      DOCUMENT ME!
         * @param  z      DOCUMENT ME!
         * @param  ny     DOCUMENT ME!
         * @param  Line   DOCUMENT ME!
         */
        private void putColumn(float[][][] Image, int x, int z, int ny, double[] Line) {

            for (int y = 0; y < ny; y++) {
                Image[x][y][z] = (float) Line[y];
            }
        } // putColumn

        /**
         * write a row in a 2D image.
         *
         * @param  Image  DOCUMENT ME!
         * @param  y      DOCUMENT ME!
         * @param  nx     DOCUMENT ME!
         * @param  Line   DOCUMENT ME!
         */
        private void putRow(float[][] Image, int y, int nx, double[] Line) {

            for (int x = 0; x < nx; x++) {
                Image[x][y] = (float) Line[x];
            }
        } // putRow

        /**
         * write a row in a 3D image.
         *
         * @param  Image  DOCUMENT ME!
         * @param  y      DOCUMENT ME!
         * @param  z      DOCUMENT ME!
         * @param  nx     DOCUMENT ME!
         * @param  Line   DOCUMENT ME!
         */
        private void putRow(float[][][] Image, int y, int z, int nx, double[] Line) {

            for (int x = 0; x < nx; x++) {
                Image[x][y][z] = (float) Line[x];
            }
        } // putRow

        /**
         * write a stack in a 3D image.
         *
         * @param  Image  DOCUMENT ME!
         * @param  x      DOCUMENT ME!
         * @param  y      DOCUMENT ME!
         * @param  nz     DOCUMENT ME!
         * @param  Line   DOCUMENT ME!
         */
        private void putStack(float[][][] Image, int x, int y, int nz, double[] Line) {

            for (int z = 0; z < nz; z++) {
                Image[x][y][z] = (float) Line[z];
            }
        } // putStack

    } // BSplineProcessing
}
