package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.util.MipavCoordinateSystems;

import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.io.IOException;
import java.util.BitSet;

import WildMagic.LibFoundation.Mathematics.*;


/**
 * Transforms Volume by resampling using transformation matrix and the choice of nearest-neighbor, trilinear
 * interpolation, 3rd order Bspline, 4th order Bspline, cubic Lagrangian, quintic Lagrangian, heptic Lagrangian, or
 * windowed sinc. Must indicate output volume's desired resolutions and dimensions.
 * 
 * <p>
 * Also includes static methods to transform images. Once caution - these static methods are NOT run in a separate
 * thread. Consequently, if a progress bar is sent into the method to update progress, it should not include a cancel
 * button because the user is unable to cancel a static transformation. If we want a transformation to have the ability
 * to be cancelled, we should NOT use the static method, but rather construct a new AlgorithmTransform and run the
 * algorithm using the standard .run() method.
 * </p>
 * 
 * <p>
 * NOTE for possible improvements in the future. To move images into "mm" space we presently multiple by voxel
 * resolutions as we loop through the dimensions. A more efficent method is to modify the scale values of the the
 * transformation matrix (i.e. the diagonals). This would speed the process by reducing the number of mults and
 * divisions. Not sure how much but faster is better. Change should happen here and in the JDialogTransform interface.
 * </p>
 * 
 * <p>
 * This version of AlgorithmTransform includes a flag (passed as the last parameter) indicating whether to pad the image
 * volume.
 * </p>
 * 
 * @version 0.1 Nov, 1999
 * @author Delia McGarry
 * @author William Gandler
 * @author Matthew McAuliffe
 * @author Zohara Cohen
 */
public class AlgorithmTransform extends AlgorithmBase {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

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
    private static float[] imgOrigin = new float[4];

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

    private static float startTime;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int bufferFactor;

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
    private double[] imgBuf = null;

    /** DOCUMENT ME! */
    private float[] imgBuf2 = null;

    /** DOCUMENT ME! */
    private int imgLength, imgLength2; // length of buffers used for import and export Data

    /** DOCUMENT ME! */
    private final int interp;

    /** flag for determining if the transform is for scanner anatomical (->AXIAL). */
    private boolean isSATransform = false;

    /** DOCUMENT ME! */
    private final int iXdim, iYdim;

    private int iZdim, iTdim;

    /** DOCUMENT ME! */
    private static float iXres, iYres;

    private static float iZres;

    /** DOCUMENT ME! */
    private final int[] oUnits;

    /** DOCUMENT ME! */
    private int oXdim, oYdim, oZdim, oTdim;

    /** DOCUMENT ME! */
    private final float oXres, oYres;

    private float oZres;

    /** DOCUMENT ME! */
    private boolean pad = true;

    /**
     * Used for out of bounds values in the transformation routines. Set to (float)srcImage.getMin() in the
     * constructors.
     */
    private float fillValue = 0.0f;

    /** DOCUMENT ME! */
    private ModelImage srcImage, destImage, maskImage;

    /** DOCUMENT ME! */
    private boolean transformVOI = false;

    /** DOCUMENT ME! */
    private final TransMatrix transMatrix;

    private boolean haveCentered = false;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * 2D constructor for transformation algorithm. Also used for 2.5D algorithms on 3D and 4D images.
     * 
     * @param srcImage ModelImage to be transformed
     * @param xfrm Transformation matrix to be applied
     * @param interp Type of interpolation (NEAREST_NEIGHBOR, BILINEAR, BSPLINE3, BSPLINE4, etc)
     * @param oXres X resolution of output image
     * @param oYres Y resolution of output image
     * @param oXdim X dimension of output image
     * @param oYdim Y dimension of output image
     * @param tVOI if <code>true</code> the VOI should be transformed with the volume
     * @param clip if <code>true</code> output range is clipped to input range
     * @param pad if <code>true</code> output image is padded so that none of the image is clipped
     */
    public AlgorithmTransform(final ModelImage srcImage, final TransMatrix xfrm, final int interp, final float oXres,
            final float oYres, final int oXdim, final int oYdim, final boolean tVOI, final boolean clip,
            final boolean pad) {
        this(srcImage, xfrm, interp, oXres, oYres, oXdim, oYdim, new int[] {srcImage.getUnitsOfMeasure(0),
                srcImage.getUnitsOfMeasure(1)}, tVOI, clip, pad);
    }

    /**
     * Creates a new AlgorithmTransform object.
     * 
     * @param srcImage ModelImage to be transformed
     * @param xfrm Transformation matrix to be applied
     * @param interp Type of interpolation (NEAREST_NEIGHBOR, BILINEAR, BSPLINE3, BSPLINE4, etc)
     * @param oXres X resolution of output image
     * @param oYres Y resolution of output image
     * @param oXdim X dimension of output image
     * @param oYdim Y dimension of output image
     * @param units DOCUMENT ME!
     * @param tVOI if <code>true</code> the VOI should be transformed with the volume
     * @param clip if <code>true</code> output range is clipped to input range
     * @param pad if <code>true</code> output image is padded so that none of the image is clipped
     */
    public AlgorithmTransform(final ModelImage srcImage, final TransMatrix xfrm, final int interp, final float oXres,
            final float oYres, int oXdim, int oYdim, final int[] units, final boolean tVOI, boolean clip,
            final boolean pad) {
        super(null, srcImage);
        transformVOI = tVOI;
        this.srcImage = srcImage;
        this.clip = clip;
        this.pad = pad;
        fillValue = (float) srcImage.getMin();

        int[] extents;
        final String name = JDialogBase.makeImageName(srcImage.getImageName(), "_transform");

        AlgorithmTransform.imgOrient = srcImage.getFileInfo(0).getImageOrientation();

        AlgorithmTransform.axisOrient = new int[srcImage.getFileInfo(0).getAxisOrientation().length];

        for (int i = 0; i < AlgorithmTransform.axisOrient.length; i++) {
            AlgorithmTransform.axisOrient[i] = srcImage.getFileInfo(0).getAxisOrientation()[i];
        }

        AlgorithmTransform.imgOrigin = srcImage.getFileInfo(0).getOrigin().clone();

        final int type = srcImage.getType();

        /* Read the direction vector from the MipavCoordinateSystems class: */
        AlgorithmTransform.direct = MipavCoordinateSystems.getModelDirections(srcImage);
        DIM = srcImage.getNDims();

        if (pad) {

            AlgorithmTransform.margins = getImageMargins(srcImage, xfrm, oXres, oYres);
            Preferences.debug("Padding is " + AlgorithmTransform.margins[0] + ", " + AlgorithmTransform.margins[1]
                    + ".\n", Preferences.DEBUG_ALGORITHM);
            updateOriginMargins2D();

            // System.out.println("Image origin with padding: " +imgOrigin[0] +" " +imgOrigin[1]);
            oXdim = oXdim + AlgorithmTransform.margins[0] + AlgorithmTransform.margins[2];
            oYdim = oYdim + AlgorithmTransform.margins[1] + AlgorithmTransform.margins[3];
        } else {

            for (int m = 0; m < 4; m++) {
                AlgorithmTransform.margins[m] = 0;
            }
        }

        if (srcImage.getNDims() == 2) {
            DIM = 2;
            do25D = false;
            extents = new int[] {oXdim, oYdim};
            destResolutions = new float[] {oXres, oYres};

            if ( ( (interp == AlgorithmTransform.WSINC) || (interp == AlgorithmTransform.CUBIC_LAGRANGIAN)
                    || (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) || (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN))
                    && ( !clip) && ( (type == ModelStorageBase.BYTE) || (type == ModelStorageBase.UBYTE))) {
                destImage = new ModelImage(ModelStorageBase.SHORT, extents, name);
            } else if ( ( (interp == AlgorithmTransform.WSINC) || (interp == AlgorithmTransform.CUBIC_LAGRANGIAN)
                    || (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) || (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN))
                    && ( !clip) && (type == ModelStorageBase.USHORT)) {
                destImage = new ModelImage(ModelStorageBase.INTEGER, extents, name);
            } else if ( ( (interp == AlgorithmTransform.WSINC) || (interp == AlgorithmTransform.CUBIC_LAGRANGIAN)
                    || (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) || (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN))
                    && ( !clip) && (type == ModelStorageBase.UINTEGER)) {
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
            AlgorithmTransform.startPos = AlgorithmTransform.imgOrigin[2]; // temporarily set origin for all slices as
            // origin of first slice
            extents = new int[] {oXdim, oYdim, oZdim};

            destResolutions = new float[] {oXres, oYres, srcImage.getFileInfo(0).getResolutions()[2]};

            if ( ( (interp == AlgorithmTransform.WSINC) || (interp == AlgorithmTransform.CUBIC_LAGRANGIAN)
                    || (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) || (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN))
                    && ( !clip) && ( (type == ModelStorageBase.BYTE) || (type == ModelStorageBase.UBYTE))) {
                destImage = new ModelImage(ModelStorageBase.SHORT, extents, name);
            } else if ( ( (interp == AlgorithmTransform.WSINC) || (interp == AlgorithmTransform.CUBIC_LAGRANGIAN)
                    || (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) || (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN))
                    && ( !clip) && (type == ModelStorageBase.USHORT)) {
                destImage = new ModelImage(ModelStorageBase.INTEGER, extents, name);
            } else if ( ( (interp == AlgorithmTransform.WSINC) || (interp == AlgorithmTransform.CUBIC_LAGRANGIAN)
                    || (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) || (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN))
                    && ( !clip) && (type == ModelStorageBase.UINTEGER)) {
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
            AlgorithmTransform.startPos = AlgorithmTransform.imgOrigin[2]; // temporarily set origin for all slices as
            // origin of first slice
            AlgorithmTransform.startTime = AlgorithmTransform.imgOrigin[3];
            extents = new int[] {oXdim, oYdim, oZdim, oTdim};
            destResolutions = new float[] {oXres, oYres, srcImage.getFileInfo(0).getResolutions()[2],
                    srcImage.getFileInfo(0).getResolutions()[3]};

            if ( ( (interp == AlgorithmTransform.WSINC) || (interp == AlgorithmTransform.CUBIC_LAGRANGIAN)
                    || (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) || (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN))
                    && ( !clip) && ( (type == ModelStorageBase.BYTE) || (type == ModelStorageBase.UBYTE))) {
                destImage = new ModelImage(ModelStorageBase.SHORT, extents, name);
            } else if ( ( (interp == AlgorithmTransform.WSINC) || (interp == AlgorithmTransform.CUBIC_LAGRANGIAN)
                    || (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) || (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN))
                    && ( !clip) && (type == ModelStorageBase.USHORT)) {
                destImage = new ModelImage(ModelStorageBase.INTEGER, extents, name);
            } else if ( ( (interp == AlgorithmTransform.WSINC) || (interp == AlgorithmTransform.CUBIC_LAGRANGIAN)
                    || (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) || (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN))
                    && ( !clip) && (type == ModelStorageBase.UINTEGER)) {
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
     * @param srcImage DOCUMENT ME!
     * @param xfrm DOCUMENT ME!
     * @param interp DOCUMENT ME!
     * @param oXres DOCUMENT ME!
     * @param oYres DOCUMENT ME!
     * @param oXdim DOCUMENT ME!
     * @param oYdim DOCUMENT ME!
     * @param units DOCUMENT ME!
     * @param tVOI DOCUMENT ME!
     * @param clip DOCUMENT ME!
     * @param pad DOCUMENT ME!
     * @param doCenter
     * @param center
     */
    public AlgorithmTransform(final ModelImage srcImage, final TransMatrix xfrm, final int interp, final float oXres,
            final float oYres, int oXdim, int oYdim, final int[] units, final boolean tVOI, boolean clip,
            final boolean pad, final boolean doCenter, final Vector3f center) {
        super(null, srcImage);
        transformVOI = tVOI;
        this.srcImage = srcImage;
        this.clip = clip;
        this.pad = pad;
        fillValue = (float) srcImage.getMin();
        this.doCenter = doCenter;
        this.center = center;

        int[] extents;
        TransMatrix xfrmC;
        final String name = JDialogBase.makeImageName(srcImage.getImageName(), "_transform");

        AlgorithmTransform.imgOrient = srcImage.getFileInfo(0).getImageOrientation();

        AlgorithmTransform.axisOrient = new int[srcImage.getFileInfo(0).getAxisOrientation().length];

        for (int i = 0; i < AlgorithmTransform.axisOrient.length; i++) {
            AlgorithmTransform.axisOrient[i] = srcImage.getFileInfo(0).getAxisOrientation()[i];
        }

        AlgorithmTransform.imgOrigin = srcImage.getFileInfo(0).getOrigin().clone();

        final int type = srcImage.getType();

        /* Read the direction vector from the MipavCoordinateSystems class: */
        AlgorithmTransform.direct = MipavCoordinateSystems.getModelDirections(srcImage);
        DIM = srcImage.getNDims();

        if (pad) {

            if (doCenter) {
                xfrmC = new TransMatrix(3);
                // xfrmC.identity();
                xfrmC.setTranslate(center.X, center.Y);
                xfrm.multLeft(xfrmC);
                xfrm.setTranslate( -center.X, -center.Y);
                haveCentered = true;
            }

            AlgorithmTransform.margins = getImageMargins(srcImage, xfrm, oXres, oYres);
            Preferences.debug("Padding is " + AlgorithmTransform.margins[0] + ", " + AlgorithmTransform.margins[1]
                    + ".\n", Preferences.DEBUG_ALGORITHM);
            updateOriginMargins2D();

            // System.out.println("Image origin with padding: " +imgOrigin[0] +" " +imgOrigin[1]);
            oXdim = oXdim + AlgorithmTransform.margins[0] + AlgorithmTransform.margins[2];
            oYdim = oYdim + AlgorithmTransform.margins[1] + AlgorithmTransform.margins[3];
        } else {

            for (int m = 0; m < 4; m++) {
                AlgorithmTransform.margins[m] = 0;
            }
        }

        if (srcImage.getNDims() == 2) {
            DIM = 2;
            do25D = false;
            extents = new int[] {oXdim, oYdim};
            destResolutions = new float[] {oXres, oYres};

            if ( ( (interp == AlgorithmTransform.WSINC) || (interp == AlgorithmTransform.CUBIC_LAGRANGIAN)
                    || (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) || (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN))
                    && ( !clip) && ( (type == ModelStorageBase.BYTE) || (type == ModelStorageBase.UBYTE))) {
                destImage = new ModelImage(ModelStorageBase.SHORT, extents, name);
            } else if ( ( (interp == AlgorithmTransform.WSINC) || (interp == AlgorithmTransform.CUBIC_LAGRANGIAN)
                    || (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) || (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN))
                    && ( !clip) && (type == ModelStorageBase.USHORT)) {
                destImage = new ModelImage(ModelStorageBase.INTEGER, extents, name);
            } else if ( ( (interp == AlgorithmTransform.WSINC) || (interp == AlgorithmTransform.CUBIC_LAGRANGIAN)
                    || (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) || (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN))
                    && ( !clip) && (type == ModelStorageBase.UINTEGER)) {
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
            AlgorithmTransform.startPos = AlgorithmTransform.imgOrigin[2]; // temporarily set origin for all slices as
            // origin of first slice
            extents = new int[] {oXdim, oYdim, oZdim};

            destResolutions = new float[] {oXres, oYres, srcImage.getFileInfo(0).getResolutions()[2]};

            if ( ( (interp == AlgorithmTransform.WSINC) || (interp == AlgorithmTransform.CUBIC_LAGRANGIAN)
                    || (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) || (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN))
                    && ( !clip) && ( (type == ModelStorageBase.BYTE) || (type == ModelStorageBase.UBYTE))) {
                destImage = new ModelImage(ModelStorageBase.SHORT, extents, name);
            } else if ( ( (interp == AlgorithmTransform.WSINC) || (interp == AlgorithmTransform.CUBIC_LAGRANGIAN)
                    || (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) || (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN))
                    && ( !clip) && (type == ModelStorageBase.USHORT)) {
                destImage = new ModelImage(ModelStorageBase.INTEGER, extents, name);
            } else if ( ( (interp == AlgorithmTransform.WSINC) || (interp == AlgorithmTransform.CUBIC_LAGRANGIAN)
                    || (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) || (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN))
                    && ( !clip) && (type == ModelStorageBase.UINTEGER)) {
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
            AlgorithmTransform.startPos = AlgorithmTransform.imgOrigin[2]; // temporarily set origin for all slices as
            // origin of first slice
            AlgorithmTransform.startTime = AlgorithmTransform.imgOrigin[3];
            extents = new int[] {oXdim, oYdim, oZdim, oTdim};
            destResolutions = new float[] {oXres, oYres, srcImage.getFileInfo(0).getResolutions()[2],
                    srcImage.getFileInfo(0).getResolutions()[3]};

            if ( ( (interp == AlgorithmTransform.WSINC) || (interp == AlgorithmTransform.CUBIC_LAGRANGIAN)
                    || (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) || (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN))
                    && ( !clip) && ( (type == ModelStorageBase.BYTE) || (type == ModelStorageBase.UBYTE))) {
                destImage = new ModelImage(ModelStorageBase.SHORT, extents, name);
            } else if ( ( (interp == AlgorithmTransform.WSINC) || (interp == AlgorithmTransform.CUBIC_LAGRANGIAN)
                    || (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) || (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN))
                    && ( !clip) && (type == ModelStorageBase.USHORT)) {
                destImage = new ModelImage(ModelStorageBase.INTEGER, extents, name);
            } else if ( ( (interp == AlgorithmTransform.WSINC) || (interp == AlgorithmTransform.CUBIC_LAGRANGIAN)
                    || (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) || (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN))
                    && ( !clip) && (type == ModelStorageBase.UINTEGER)) {
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
     * @param _srcImage ModelImage to be transformed
     * @param xfrm Transformation matrix to be applied
     * @param interp Type of interpolation (NEAREST_NEIGHBOR, TRILINEAR, BSPLINE3, BSPLINE4, etc)
     * @param _oXres X resolution of output image
     * @param _oYres Y resolution of output image
     * @param _oZres Z resolution of output image
     * @param _oXdim X dimension of output image
     * @param _oYdim Y dimension of output image
     * @param _oZdim Z dimension of output image
     * @param tVOI if <code>true</code> the VOI should be transformed with the volume
     * @param clip if <code>true</code> output range is clipped to input range
     * @param pad if <code>true</code> output image is padded so that none of the image is clipped
     */
    public AlgorithmTransform(final ModelImage _srcImage, final TransMatrix xfrm, final int interp, final float _oXres,
            final float _oYres, final float _oZres, final int _oXdim, final int _oYdim, final int _oZdim,
            final boolean tVOI, final boolean clip, final boolean pad) {
        this(_srcImage, xfrm, interp, _oXres, _oYres, _oZres, _oXdim, _oYdim, _oZdim, new int[] {
                _srcImage.getUnitsOfMeasure(0), _srcImage.getUnitsOfMeasure(1), _srcImage.getUnitsOfMeasure(2)}, tVOI,
                clip, pad);

    }

    /**
     * Creates a new $class.name$ object.
     * 
     * @param _srcImage DOCUMENT ME!
     * @param xfrm DOCUMENT ME!
     * @param interp DOCUMENT ME!
     * @param _oXres DOCUMENT ME!
     * @param _oYres DOCUMENT ME!
     * @param _oZres DOCUMENT ME!
     * @param _oXdim DOCUMENT ME!
     * @param _oYdim DOCUMENT ME!
     * @param _oZdim DOCUMENT ME!
     * @param units DOCUMENT ME!
     * @param tVOI DOCUMENT ME!
     * @param clip DOCUMENT ME!
     * @param pad DOCUMENT ME!
     */
    public AlgorithmTransform(final ModelImage _srcImage, final TransMatrix xfrm, final int interp, final float _oXres,
            final float _oYres, final float _oZres, final int _oXdim, final int _oYdim, final int _oZdim,
            final int[] units, final boolean tVOI, boolean clip, final boolean pad) {
        super(null, _srcImage);
        this.interp = interp;
        this.srcImage = _srcImage;
        this.transMatrix = xfrm;
        transformVOI = tVOI;
        this.clip = clip;
        this.pad = pad;
        fillValue = (float) srcImage.getMin();

        this.oXres = _oXres;
        this.oYres = _oYres;
        this.oZres = _oZres;
        this.oXdim = _oXdim;
        this.oYdim = _oYdim;
        this.oZdim = _oZdim;

        this.oUnits = units;

        DIM = srcImage.getNDims();
        AlgorithmTransform.imgOrigin = srcImage.getFileInfo(0).getOrigin().clone();
        AlgorithmTransform.axisOrient = new int[srcImage.getFileInfo(0).getAxisOrientation().length];

        for (int i = 0; i < AlgorithmTransform.axisOrient.length; i++) {
            AlgorithmTransform.axisOrient[i] = srcImage.getFileInfo(0).getAxisOrientation()[i];
        }

        AlgorithmTransform.imgOrient = srcImage.getFileInfo(0).getImageOrientation();
        iXres = srcImage.getFileInfo(0).getResolutions()[0];
        iYres = srcImage.getFileInfo(0).getResolutions()[1];
        iZres = srcImage.getFileInfo(0).getResolutions()[2];
        iXdim = srcImage.getExtents()[0];
        iYdim = srcImage.getExtents()[1];
        iZdim = srcImage.getExtents()[2];

        /* Read the direction vector from the MipavCoordinateSystems class: */
        AlgorithmTransform.direct = MipavCoordinateSystems.getModelDirections(srcImage);

        // System.out.println("Directions are " +direct[0] +", " +direct[1] +" and " +direct[2]);
        if (pad) {

            AlgorithmTransform.margins = AlgorithmTransform.getImageMargins(srcImage, xfrm, oXres, oYres, oZres);
            Preferences.debug("Padding is " + AlgorithmTransform.margins[0] + ", " + AlgorithmTransform.margins[1]
                    + " and " + AlgorithmTransform.margins[2] + ".\n", Preferences.DEBUG_ALGORITHM);
            updateOriginMargins();

            // System.out.println("Image origin with padding: " +imgOrigin[0] +" " +imgOrigin[1] +" " +imgOrigin[2]);
            oXdim = oXdim + AlgorithmTransform.margins[0] + AlgorithmTransform.margins[3];
            oYdim = oYdim + AlgorithmTransform.margins[1] + AlgorithmTransform.margins[4];
            oZdim = oZdim + AlgorithmTransform.margins[2] + AlgorithmTransform.margins[5];
        } else {

            for (int m = 0; m < 6; m++) {
                AlgorithmTransform.margins[m] = 0;
            }
        }

        AlgorithmTransform.startPos = AlgorithmTransform.imgOrigin[2];

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
            AlgorithmTransform.startTime = AlgorithmTransform.imgOrigin[3];
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

        final String name = JDialogBase.makeImageName(srcImage.getImageName(), "_transform");
        int type = srcImage.getType();

        if (DIM == 3) {

            if ( ( (interp == AlgorithmTransform.WSINC) || (interp == AlgorithmTransform.CUBIC_LAGRANGIAN)
                    || (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) || (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN))
                    && ( !clip) && ( (type == ModelStorageBase.BYTE) || (type == ModelStorageBase.UBYTE))) {
                type = ModelStorageBase.SHORT;
            } else if ( ( (interp == AlgorithmTransform.WSINC) || (interp == AlgorithmTransform.CUBIC_LAGRANGIAN)
                    || (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) || (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN))
                    && ( !clip) && (type == ModelStorageBase.USHORT)) {
                type = ModelStorageBase.INTEGER;
            } else if ( ( (interp == AlgorithmTransform.WSINC) || (interp == AlgorithmTransform.CUBIC_LAGRANGIAN)
                    || (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) || (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN))
                    && ( !clip) && (type == ModelStorageBase.UINTEGER)) {
                type = ModelStorageBase.LONG;
            }
        } // end of if DIM == 3
        else { // DIM == 4

            if ( ( (interp == AlgorithmTransform.WSINC) || (interp == AlgorithmTransform.CUBIC_LAGRANGIAN)
                    || (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) || (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN))
                    && ( !clip) && ( (type == ModelStorageBase.BYTE) || (type == ModelStorageBase.UBYTE))) {
                type = ModelStorageBase.SHORT;
            } else if ( ( (interp == AlgorithmTransform.WSINC) || (interp == AlgorithmTransform.CUBIC_LAGRANGIAN)
                    || (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) || (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN))
                    && ( !clip) && (type == ModelStorageBase.USHORT)) {
                type = ModelStorageBase.INTEGER;
            } else if ( ( (interp == AlgorithmTransform.WSINC) || (interp == AlgorithmTransform.CUBIC_LAGRANGIAN)
                    || (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) || (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN))
                    && ( !clip) && (type == ModelStorageBase.UINTEGER)) {
                type = ModelStorageBase.LONG;
            }
        } // end of DIM == 4

        destImage = new ModelImage(type, extents, name);
    }

    /**
     * Creates a new $class.name$ object.
     * 
     * @param _srcImage DOCUMENT ME!
     * @param xfrm DOCUMENT ME!
     * @param interp DOCUMENT ME!
     * @param _oXres DOCUMENT ME!
     * @param _oYres DOCUMENT ME!
     * @param _oZres DOCUMENT ME!
     * @param _oXdim DOCUMENT ME!
     * @param _oYdim DOCUMENT ME!
     * @param _oZdim DOCUMENT ME!
     * @param units DOCUMENT ME!
     * @param tVOI DOCUMENT ME!
     * @param clip DOCUMENT ME!
     * @param pad DOCUMENT ME!
     * @param doCenter
     * @param center
     */
    public AlgorithmTransform(final ModelImage _srcImage, final TransMatrix xfrm, final int interp, final float _oXres,
            final float _oYres, final float _oZres, final int _oXdim, final int _oYdim, final int _oZdim,
            final int[] units, final boolean tVOI, boolean clip, final boolean pad, final boolean doCenter,
            final Vector3f center) {
        super(null, _srcImage);
        this.interp = interp;
        this.srcImage = _srcImage;
        this.transMatrix = xfrm;
        transformVOI = tVOI;
        this.clip = clip;
        this.pad = pad;
        fillValue = (float) srcImage.getMin();
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
        AlgorithmTransform.imgOrigin = srcImage.getFileInfo(0).getOrigin().clone();
        AlgorithmTransform.axisOrient = new int[srcImage.getFileInfo(0).getAxisOrientation().length];

        for (int i = 0; i < AlgorithmTransform.axisOrient.length; i++) {
            AlgorithmTransform.axisOrient[i] = srcImage.getFileInfo(0).getAxisOrientation()[i];
        }

        AlgorithmTransform.imgOrient = srcImage.getFileInfo(0).getImageOrientation();
        iXres = srcImage.getFileInfo(0).getResolutions()[0];
        iYres = srcImage.getFileInfo(0).getResolutions()[1];
        iZres = srcImage.getFileInfo(0).getResolutions()[2];
        iXdim = srcImage.getExtents()[0];
        iYdim = srcImage.getExtents()[1];
        iZdim = srcImage.getExtents()[2];

        /* Read the direction vector from the MipavCoordinateSystems class: */
        AlgorithmTransform.direct = MipavCoordinateSystems.getModelDirections(srcImage);

        // System.out.println("Directions are " +direct[0] +", " +direct[1] +" and " +direct[2]);
        if (pad) {

            if (doCenter) {
                xfrmC = new TransMatrix(4);
                // xfrmC.identity();
                xfrmC.setTranslate(center.X, center.Y, center.Z);
                xfrm.multLeft(xfrmC);
                xfrm.setTranslate( -center.X, -center.Y, -center.Z);
                haveCentered = true;
            }

            AlgorithmTransform.margins = AlgorithmTransform.getImageMargins(srcImage, xfrm, oXres, oYres, oZres);
            Preferences.debug("Padding is " + AlgorithmTransform.margins[0] + ", " + AlgorithmTransform.margins[1]
                    + " and " + AlgorithmTransform.margins[2] + ".\n", Preferences.DEBUG_ALGORITHM);
            updateOriginMargins();

            // System.out.println("Image origin with padding: " +imgOrigin[0] +" " +imgOrigin[1] +" " +imgOrigin[2]);
            oXdim = oXdim + AlgorithmTransform.margins[0] + AlgorithmTransform.margins[3];
            oYdim = oYdim + AlgorithmTransform.margins[1] + AlgorithmTransform.margins[4];
            oZdim = oZdim + AlgorithmTransform.margins[2] + AlgorithmTransform.margins[5];
        } else {

            for (int m = 0; m < 6; m++) {
                AlgorithmTransform.margins[m] = 0;
            }
        }

        AlgorithmTransform.startPos = AlgorithmTransform.imgOrigin[2];

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
            AlgorithmTransform.startTime = AlgorithmTransform.imgOrigin[3];
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

        final String name = JDialogBase.makeImageName(srcImage.getImageName(), "_transform");
        int type = srcImage.getType();

        if (DIM == 3) {

            if ( ( (interp == AlgorithmTransform.WSINC) || (interp == AlgorithmTransform.CUBIC_LAGRANGIAN)
                    || (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) || (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN))
                    && ( !clip) && ( (type == ModelStorageBase.BYTE) || (type == ModelStorageBase.UBYTE))) {
                type = ModelStorageBase.SHORT;
            } else if ( ( (interp == AlgorithmTransform.WSINC) || (interp == AlgorithmTransform.CUBIC_LAGRANGIAN)
                    || (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) || (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN))
                    && ( !clip) && (type == ModelStorageBase.USHORT)) {
                type = ModelStorageBase.INTEGER;
            } else if ( ( (interp == AlgorithmTransform.WSINC) || (interp == AlgorithmTransform.CUBIC_LAGRANGIAN)
                    || (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) || (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN))
                    && ( !clip) && (type == ModelStorageBase.UINTEGER)) {
                type = ModelStorageBase.LONG;
            }
        } // end of if DIM == 3
        else { // DIM == 4

            if ( ( (interp == AlgorithmTransform.WSINC) || (interp == AlgorithmTransform.CUBIC_LAGRANGIAN)
                    || (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) || (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN))
                    && ( !clip) && ( (type == ModelStorageBase.BYTE) || (type == ModelStorageBase.UBYTE))) {
                type = ModelStorageBase.SHORT;
            } else if ( ( (interp == AlgorithmTransform.WSINC) || (interp == AlgorithmTransform.CUBIC_LAGRANGIAN)
                    || (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) || (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN))
                    && ( !clip) && (type == ModelStorageBase.USHORT)) {
                type = ModelStorageBase.INTEGER;
            } else if ( ( (interp == AlgorithmTransform.WSINC) || (interp == AlgorithmTransform.CUBIC_LAGRANGIAN)
                    || (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) || (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN))
                    && ( !clip) && (type == ModelStorageBase.UINTEGER)) {
                type = ModelStorageBase.LONG;
            }
        } // end of DIM == 4

        destImage = new ModelImage(type, extents, name);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Performs bspline interpolation on black and white image data.
     * 
     * @param image Image from which the data is derived
     * @param resultImage Image to put result in; can be null.
     * @param degree Degree of the spline algorithm, either 3 or 4.
     * @param xfrm Transformation to apply.
     * @param progressBar Buffer containing image data.
     * 
     * @return DOCUMENT ME!
     */
    public static ModelImage bspline(final ModelImage image, final ModelImage resultImage, final int degree,
            final TransMatrix xfrm, final ViewJProgressBar progressBar) {

        try {
            final AlgorithmBSpline Bspline = new AlgorithmBSpline();
            int i, j, k;
            double X, Y, Z;
            double value;
            double imm, jmm, kmm;
            final int xDim = image.getExtents()[0];
            final int yDim = image.getExtents()[1];
            final int zDim = image.getExtents()[2];
            final float[] resols = new float[3];
            final int[] inVolExtents = {xDim, yDim, zDim};
            final int length = xDim * yDim * zDim;
            final int mod = length / 100; // mod is 1 percent of length
            int counter = 0; // used for progress bar
            double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

            resols[0] = image.getFileInfo()[0].getResolutions()[0];
            resols[1] = image.getFileInfo()[0].getResolutions()[1];
            resols[2] = image.getFileInfo()[0].getResolutions()[2];

            final int bufferSize = image.getSliceSize() * image.getExtents()[2];
            final double[] imgBuffer = new double[bufferSize];

            try {

                if (resultImage != null) {
                    resultImage.exportData(0, bufferSize, imgBuffer);
                } else {
                    image.exportData(0, bufferSize, imgBuffer);
                }
            } catch (final IOException error) {
                MipavUtil.displayError("ViewJFrameTriImage: IOException error on exportData");
            }

            final TransMatrix kTM = AlgorithmTransform.matrixtoInverseArray(xfrm);
            T00 = (double)kTM.M00;
            T01 = (double)kTM.M01;
            T02 = (double)kTM.M02;
            T03 = (double)kTM.M03;
            T10 = (double)kTM.M10;
            T11 = (double)kTM.M11;
            T12 = (double)kTM.M12;
            T13 = (double)kTM.M13;
            T20 = (double)kTM.M20;
            T21 = (double)kTM.M21;
            T22 = (double)kTM.M22;
            T23 = (double)kTM.M23;

            Bspline.setup3DBSpline(imgBuffer, inVolExtents, degree);

            for (i = 0; i < xDim; i++) {

                for (j = 0; j < yDim; j++) {

                    for (k = 0; k < zDim; k++) {

                        if ( (progressBar != null) && ( (counter % mod) == 0)) {
                            progressBar.updateValueImmed(Math.round((float) counter / (length - 1) * 100));
                        }

                        value = 0; // will remain zero if boundary conditions not met
                        imm = i * resols[0];
                        jmm = j * resols[1];
                        kmm = k * resols[2];

                        // transform i,j,k
                        X = (imm * T00) + (jmm * T01) + (kmm * T02) + T03;
                        X = X / resols[0];

                        if ( (X >= 0) && (X < xDim)) { // check bounds
                            Y = (imm * T10) + (jmm * T11) + (kmm * T12) + T13;
                            Y = Y / resols[1];

                            if ( (Y >= 0) && (Y < yDim)) {
                                Z = (imm * T20) + (jmm * T21) + (kmm * T22) + T23;
                                Z = Z / resols[2];

                                if ( (Z >= 0) && (Z < zDim)) {
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
        } finally {}
    }

    /**
     * Performs bspline interpolation on black and white image data in 4D image. Works time slice by time slice.
     * 
     * @param image Image from which the data is derived
     * @param resultImage Image to put result in; can be null.
     * @param degree Degree of the spline algorithm, either 3 or 4.
     * @param xfrm Transformation to apply.
     * @param progressBar Buffer containing image data.
     * 
     * @return DOCUMENT ME!
     */
    public static ModelImage bspline4D(final ModelImage image, final ModelImage resultImage, final int degree,
            final TransMatrix xfrm, final ViewJProgressBar progressBar) {
        final AlgorithmBSpline Bspline = new AlgorithmBSpline();
        int i, j, k, l;
        double X, Y, Z;
        double value;
        double imm, jmm, kmm;
        final int xDim = image.getExtents()[0];
        final int yDim = image.getExtents()[1];
        final int zDim = image.getExtents()[2];
        final float[] resols = new float[3];
        final int[] inVolExtents = {xDim, yDim, zDim};
        int length;
        int mod; // mod is 1 percent of length
        int tDim;
        int counter = 0; // used for progress bar
        tDim = image.getExtents()[3];
        length = xDim * yDim * zDim * tDim;
        mod = length / 100;

        double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        final int bufferSize = image.getSliceSize() * image.getExtents()[2];

        final double[] imgBuffer = new double[bufferSize];

        try {

            if (resultImage != null) {
                resultImage.exportData(0, bufferSize, imgBuffer);
            } else {
                image.exportData(0, bufferSize, imgBuffer);
            }
        } catch (final IOException error) {
            MipavUtil.displayError("ViewJFrameTriImage: IOException error on exportData");
        }

        resols[0] = image.getFileInfo()[0].getResolutions()[0];
        resols[1] = image.getFileInfo()[0].getResolutions()[1];
        resols[2] = image.getFileInfo()[0].getResolutions()[2];

        final TransMatrix kTM = AlgorithmTransform.matrixtoInverseArray(xfrm);
        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T03 = (double)kTM.M03;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;
        T13 = (double)kTM.M13;
        T20 = (double)kTM.M20;
        T21 = (double)kTM.M21;
        T22 = (double)kTM.M22;
        T23 = (double)kTM.M23;

        for (l = 0; l < tDim; l++) {
            Bspline.setup3DBSpline(imgBuffer, inVolExtents, degree);

            for (i = 0; i < xDim; i++) {

                for (j = 0; j < yDim; j++) {

                    for (k = 0; k < zDim; k++) {

                        if ( (progressBar != null) && ( (counter % mod) == 0)) {
                            progressBar.updateValue(Math.round((float) counter / (length - 1) * 100), false);
                        }

                        value = 0; // will remain zero if boundary conditions not met
                        imm = i * resols[0];
                        jmm = j * resols[1];
                        kmm = k * resols[2];

                        // transform i,j,k
                        X = (imm * T00) + (jmm * T01) + (kmm * T02) + T03;
                        X = X / resols[0];

                        if ( (X >= 0) && (X < xDim)) { // check bounds
                            Y = (imm * T10) + (jmm * T11) + (kmm * T12) + T13;
                            Y = Y / resols[1];

                            if ( (Y >= 0) && (Y < yDim)) {
                                Z = (imm * T20) + (jmm * T21) + (kmm * T22) + T23;
                                Z = Z / resols[2];

                                if ( (Z >= 0) && (Z < zDim)) {
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
                        resultImage.exportData( (l + 1) * imgBuffer.length, imgBuffer.length, imgBuffer);
                    } else {
                        image.exportData( (l + 1) * imgBuffer.length, imgBuffer.length, imgBuffer);
                    }
                } catch (final IOException error) {
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
     * @param image Image from which the data is derived
     * @param resultImage Image to put result in; can be null.
     * @param degree Degree of the spline algorithm, either 3 or 4.
     * @param xfrm Transformation to apply.
     * @param progressBar Buffer containing image data.
     * 
     * @return DOCUMENT ME!
     */
    public static ModelImage bsplineC(final ModelImage image, final ModelImage resultImage, final int degree,
            final TransMatrix xfrm, final ViewJProgressBar progressBar) {

        try {
            final AlgorithmBSpline Bspline = new AlgorithmBSpline();
            int i, j, k;
            double X, Y, Z;
            double[] value = new double[4];
            int sliceSize;
            double imm, jmm, kmm;
            final int xDim = image.getExtents()[0];
            final int yDim = image.getExtents()[1];
            final int zDim = image.getExtents()[2];
            final float[] resols = new float[3];
            final int[] inVolExtents = {xDim, yDim, zDim};
            final int length = xDim * yDim * zDim;
            final int mod = length / 100; // mod is 1 percent of length
            int counter = 0; // used for progress bar
            sliceSize = xDim * yDim;

            double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
            int index;
            final int bufferSize = 4 * image.getSliceSize() * image.getExtents()[2];
            final double[] imgBuffer = new double[bufferSize];
            final float[] imgBuffer2 = new float[imgBuffer.length];

            try {

                if (resultImage != null) {
                    resultImage.exportData(0, bufferSize, imgBuffer);
                } else {
                    image.exportData(0, bufferSize, imgBuffer);
                }
            } catch (final IOException error) {
                MipavUtil.displayError("ViewJFrameTriImage: IOException error on exportData");
            }

            resols[0] = image.getFileInfo()[0].getResolutions()[0];
            resols[1] = image.getFileInfo()[0].getResolutions()[1];
            resols[2] = image.getFileInfo()[0].getResolutions()[2];

            final TransMatrix kTM = AlgorithmTransform.matrixtoInverseArray(xfrm);
            T00 = (double)kTM.M00;
            T01 = (double)kTM.M01;
            T02 = (double)kTM.M02;
            T03 = (double)kTM.M03;
            T10 = (double)kTM.M10;
            T11 = (double)kTM.M11;
            T12 = (double)kTM.M12;
            T13 = (double)kTM.M13;
            T20 = (double)kTM.M20;
            T21 = (double)kTM.M21;
            T22 = (double)kTM.M22;
            T23 = (double)kTM.M23;

            Bspline.setup3DBSplineC(imgBuffer, inVolExtents, degree);

            for (i = 0; i < xDim; i++) {

                for (j = 0; j < yDim; j++) {

                    for (k = 0; k < zDim; k++) {

                        if ( (progressBar != null) && ( (counter % mod) == 0)) {
                            progressBar.updateValueImmed(Math.round((float) counter / (length - 1) * 100));
                        }

                        value[0] = value[1] = value[2] = value[3] = 0; // will remain zero if boundary conditions not
                        // met
                        imm = i * resols[0];
                        jmm = j * resols[1];
                        kmm = k * resols[2];

                        // transform i,j,k
                        X = (imm * T00) + (jmm * T01) + (kmm * T02) + T03;
                        X = X / resols[0];

                        if ( (X >= 0) && (X < xDim)) { // check bounds
                            Y = (imm * T10) + (jmm * T11) + (kmm * T12) + T13;
                            Y = Y / resols[1];

                            if ( (Y >= 0) && (Y < yDim)) {
                                Z = (imm * T20) + (jmm * T21) + (kmm * T22) + T23;
                                Z = Z / resols[2];

                                if ( (Z >= 0) && (Z < zDim)) {
                                    value = Bspline.bSpline3DC(0, 0, 0, X, Y, Z);
                                }
                            }
                        }

                        index = 4 * (i + (j * xDim) + (k * sliceSize));
                        imgBuffer2[index] = (float)value[0];
                        imgBuffer2[index + 1] = (float)value[1];
                        imgBuffer2[index + 2] = (float)value[2];
                        imgBuffer2[index + 3] = (float)value[3];

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
            } catch (final IOException error) {
                MipavUtil.displayError("ViewJFrameTriImage: IOException Error on importData");

                return null;
            }
        } finally {}
    }

    /**
     * Performs bspline interpolation on color image data in 4D image. Works time slice by time slice.
     * 
     * @param image Image from which the data is derived
     * @param resultImage Image to put result in; can be null.
     * @param degree Degree of the spline algorithm, either 3 or 4.
     * @param xfrm Transformation to apply.
     * @param progressBar Buffer containing image data.
     * 
     * @return DOCUMENT ME!
     */
    public static ModelImage bsplineC4D(final ModelImage image, final ModelImage resultImage, final int degree,
            final TransMatrix xfrm, final ViewJProgressBar progressBar) {
        final AlgorithmBSpline Bspline = new AlgorithmBSpline();
        int i, j, k, l;
        double X, Y, Z;
        double[] value = new double[4];
        int sliceSize;
        double imm, jmm, kmm;
        final int xDim = image.getExtents()[0];
        final int yDim = image.getExtents()[1];
        final int zDim = image.getExtents()[2];
        final float[] resols = new float[3];
        final int[] inVolExtents = {xDim, yDim, zDim};
        int length;
        int mod; // mod is 1 percent of length
        int tDim;
        int counter = 0; // used for progress bar
        tDim = image.getExtents()[3];
        length = xDim * yDim * zDim * tDim;
        mod = length / 100;
        sliceSize = xDim * yDim;

        double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        int index;

        final int bufferSize = image.getSliceSize() * image.getExtents()[2];

        final double[] imgBuffer = new double[bufferSize];

        try {

            if (resultImage != null) {
                resultImage.exportData(0, bufferSize, imgBuffer);
            } else {
                image.exportData(0, bufferSize, imgBuffer);
            }
        } catch (final IOException error) {
            MipavUtil.displayError("ViewJFrameTriImage: IOException error on exportData");
        }

        final float[] imgBuffer2 = new float[imgBuffer.length];

        resols[0] = image.getFileInfo()[0].getResolutions()[0];
        resols[1] = image.getFileInfo()[0].getResolutions()[1];
        resols[2] = image.getFileInfo()[0].getResolutions()[2];

        final TransMatrix kTM = AlgorithmTransform.matrixtoInverseArray(xfrm);
        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T03 = (double)kTM.M03;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;
        T13 = (double)kTM.M13;
        T20 = (double)kTM.M20;
        T21 = (double)kTM.M21;
        T22 = (double)kTM.M22;
        T23 = (double)kTM.M23;

        for (l = 0; l < tDim; l++) {
            Bspline.setup3DBSplineC(imgBuffer, inVolExtents, degree);

            for (i = 0; i < xDim; i++) {

                for (j = 0; j < yDim; j++) {

                    for (k = 0; k < zDim; k++) {

                        if ( (progressBar != null) && ( (counter % mod) == 0)) {
                            progressBar.updateValue(Math.round((float) counter / (length - 1) * 100), false);
                        }

                        value[0] = value[1] = value[2] = value[3] = 0; // will remain zero if boundary conditions not
                        // met
                        imm = i * resols[0];
                        jmm = j * resols[1];
                        kmm = k * resols[2];

                        // transform i,j,k
                        X = (imm * T00) + (jmm * T01) + (kmm * T02) + T03;
                        X = X / resols[0];

                        if ( (X >= 0) && (X < xDim)) { // check bounds
                            Y = (imm * T10) + (jmm * T11) + (kmm * T12) + T13;
                            Y = Y / resols[1];

                            if ( (Y >= 0) && (Y < yDim)) {
                                Z = (imm * T20) + (jmm * T21) + (kmm * T22) + T23;
                                Z = Z / resols[2];

                                if ( (Z >= 0) && (Z < zDim)) {
                                    value = Bspline.bSpline3DC(0, 0, 0, X, Y, Z);
                                }
                            }
                        }

                        index = 4 * (i + (j * xDim) + (k * sliceSize));
                        imgBuffer2[index] = (float)value[0];
                        imgBuffer2[index + 1] = (float)value[1];
                        imgBuffer2[index + 2] = (float)value[2];
                        imgBuffer2[index + 3] = (float)value[3];

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
            } catch (final IOException error) {
                MipavUtil.displayError("ViewJFrameTriImage: IOException Error on importData");
            }

            if (l < (tDim - 1)) {

                try {

                    if (resultImage != null) {
                        resultImage.exportData( (l + 1) * imgBuffer.length, imgBuffer.length, imgBuffer);
                    } else {
                        image.exportData( (l + 1) * imgBuffer.length, imgBuffer.length, imgBuffer);
                    }
                } catch (final IOException error) {
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
     * @param srcImage DOCUMENT ME!
     * @param transMatrix array with transformation matrix
     * @param dxOut DOCUMENT ME!
     * @param dyOut DOCUMENT ME!
     * @param dzOut DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public static int[] getImageMargins(final ModelImage srcImage, final TransMatrix transMatrix, final float dxOut,
            final float dyOut, final float dzOut) {
        int i;
        final float xi = 0.f;
        final float yi = 0.f;
        final float zi = 0.f;
        final float dx = srcImage.getFileInfo(0).getResolutions()[0];
        final float dy = srcImage.getFileInfo(0).getResolutions()[1];
        final float dz = srcImage.getFileInfo(0).getResolutions()[2];
        final int nx = srcImage.getExtents()[0];
        final int ny = srcImage.getExtents()[1];
        final int nz = srcImage.getExtents()[2];
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

        /*
         * Set up array of 8 points representing the corners of the image volume and then transform them with
         * transMatrix.
         */
        ptsi3 = new Vector3f[8];
        ptsf3 = new Vector3f[8];

        for (i = 1; i <= 8; i++) {
            ptsi3[i - 1] = new Vector3f();
            ptsf3[i - 1] = new Vector3f();

            if ( (i == 1) || (i == 4) || (i == 5) || (i == 8)) {
                ptsi3[i - 1].X = xi;
            } else {
                ptsi3[i - 1].X = xf;
            }

            if ( (i == 1) || (i == 2) || (i == 5) || (i == 6)) {
                ptsi3[i - 1].Y = yi;
            } else {
                ptsi3[i - 1].Y = yf;
            }

            if ( (i == 1) || (i == 2) || (i == 3) || (i == 4)) {
                ptsi3[i - 1].Z = zi;
            } else {
                ptsi3[i - 1].Z = zf;
            }
            // System.out.println("Initial point " +i +": " +(int)ptsi3[i-1].X +", " +(int)ptsi3[i-1].Y +", "
            // +(int)ptsi3[i-1].Z);
        }

        /* Transform corner points, ptsi3, to get transformed points, ptsf3. */
        for (i = 1; i <= 8; i++) {
            transMatrix.transformAsPoint3Df(ptsi3[i - 1], ptsf3[i - 1]);
        }

        /* Find new min and max values for the transformed point. */
        for (i = 1; i <= 8; i++) {

            // System.out.println("Transformed point " +i +": " +(int)ptsf3[i-1].X +", " +(int)ptsf3[i-1].Y +", "
            // +(int)ptsf3[i-1].Z);
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
        leftPad = (int) ( ( (xi - minx) / dxOut) + 0.5);
        rightPad = (int) ( ( (maxx - xf) / dxOut) + 0.5);

        // System.out.println("Padding in x is: " + leftPad +" and " +rightPad);
        topPad = (int) ( ( (yi - miny) / dyOut) + 0.5);
        bottomPad = (int) ( ( (maxy - yf) / dyOut) + 0.5);

        // System.out.println("Padding in y is: " + topPad+" and " +bottomPad);
        front = (int) ( ( (zi - minz) / dzOut) + 0.5);
        back = (int) ( ( (maxz - zf) / dzOut) + 0.5);
        // System.out.println("Padding in z is: " + front + " and " + back);

        AlgorithmTransform.margins[0] = leftPad;
        AlgorithmTransform.margins[1] = topPad;
        AlgorithmTransform.margins[2] = front;
        AlgorithmTransform.margins[3] = rightPad;
        AlgorithmTransform.margins[4] = bottomPad;
        AlgorithmTransform.margins[5] = back;

        return AlgorithmTransform.margins;
    }

    /**
     * Converts matrix to inverse array.
     * 
     * @param transMatrix Matrix to convert.
     * 
     * @return The inverted array.
     */
    public static final TransMatrix matrixtoInverseArray(final TransMatrix transMatrix) {

        if (transMatrix.isIdentity()) {

            // Added explicit handling of identity matrix - or else the new matrix is other than
            // identity because of round-off error. This situation (of identity) matrix
            // occurs frequently -- any time there is resampling without transformation.
            return new TransMatrix(transMatrix);
        } else {
            final TransMatrix kTM = new TransMatrix(transMatrix);
            kTM.Inverse();
            return kTM;
        }

    }

    /**
     * Transforms using bilinear interpolation.
     * 
     * @param image Image to be transformed
     * @param transformedImg Transformed image
     * @param trans Transformation matrix to be applied
     * @param progressBar Progress bar to update. Can be null. Should NOT have cancel button.
     */
    public static final void transformBilinear(final ModelImage image, final ModelImage transformedImg,
            final TransMatrix trans, final ViewJProgressBar progressBar) {
        AlgorithmTransform.transformBilinear(image, transformedImg, trans, progressBar, true);
    }

    /**
     * Transforms using bilinear interpolation.
     * 
     * @param image Image to be transformed
     * @param transformedImg Transformed image
     * @param trans Transformation matrix to be applied
     * @param progressBar Progress bar to update. Can be null. Should NOT have cancel button.
     * @param activeImage true if the algorithm is being run in a separate thread, false otherwise, to control progress
     *            bar repainting
     */
    public static final void transformBilinear(final ModelImage image, final ModelImage transformedImg,
            final TransMatrix trans, final ViewJProgressBar progressBar, final boolean activeImage) {
        int i, j;
        float X, Y;
        int x0, y0;
        float j1, j2;
        float imm, jmm;
        float value;

        AlgorithmTransform.imgOrigin = image.getFileInfo(0).getOrigin().clone();

        if (AlgorithmTransform.updateOrigin) {
            AlgorithmTransform.updateOrigin(trans);
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

        AlgorithmTransform.updateFileInfo(image, transformedImg, transformedImg.getFileInfo(0).getResolutions(), image
                .getFileInfo()[0].getUnitsOfMeasure(), trans, false, null);

        final int mod = Math.max(1, oYdim / 50);
        final int imgLength = iXdim * iYdim;
        float[] imgBuf;

        try {
            imgBuf = new float[imgLength];
            image.exportData(0, imgLength, imgBuf);
        } catch (final IOException error) {
            imgBuf = null;
            MipavUtil.displayError("Algorithm Transform: Image(s) locked");

            return;
        } catch (final OutOfMemoryError error) {
            imgBuf = null;
            System.gc();
            MipavUtil.displayError("Algorithm Transform: Out of memory");

            return;
        }

        float T00, T01, T02, T10, T11, T12;

        final TransMatrix kTM = AlgorithmTransform.matrixtoInverseArray(trans);
        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        int position;
        float dx, dy, dx1, dy1;

        final float invXRes = 1 / iXres;
        final float invYRes = 1 / iYres;

        int index = 0;
        int deltaX, deltaY;

        for (j = 0; j < oYdim; j++) {

            if ( (progressBar != null) && ( (j % mod) == 0)) {
                progressBar.updateValue((int) ( ((float) j / oYdim * 100) + 0.5f), activeImage);
            }

            jmm = j * oYres;
            j1 = (jmm * T01) + T02;
            j2 = (jmm * T11) + T12;

            for (i = 0; i < oXdim; i++) {

                // transform i,j,k
                value = (float) image.getMin(); // remains zero if voxel is transformed out of bounds
                imm = i * oXres;
                X = (j1 + (imm * T00)) * invXRes;

                if ( (X > -0.5f) && (X < iXdim)) {
                    Y = (j2 + (imm * T10)) * invYRes;

                    if ( (Y > -0.5f) && (Y < iYdim)) {

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

                        value = (dy1 * ( (dx1 * imgBuf[position]) + (dx * imgBuf[position + deltaX])))
                                + (dy * ( (dx1 * imgBuf[position + deltaY]) + (dx * imgBuf[position + deltaY + deltaX])));
                    } // end if Y in bounds
                } // end if X in bounds

                transformedImg.set(index++, value);
            } // end for i
        } // end for j
    }

    /**
     * Transforms using bilinear interpolation.
     * 
     * @param image Image to be transformed
     * @param transformedImg Transformed image
     * @param trans Transformation matrix to be applied
     * @param progressBar Progress bar to update. Can be null. Should NOT have cancel button.
     * @param activeImage true if the algorithm is being run in a separate thread, false otherwise, to control progress
     *            bar repainting
     * @param fillValue value used for out of bounds pixels
     */
    public static final void transformBilinear(final ModelImage image, final ModelImage transformedImg,
            final TransMatrix trans, final ViewJProgressBar progressBar, final boolean activeImage,
            final float fillValue) {
        int i, j;
        float X, Y;
        int x0, y0;
        float j1, j2;
        float imm, jmm;
        float value;

        AlgorithmTransform.imgOrigin = image.getFileInfo(0).getOrigin().clone();

        if (AlgorithmTransform.updateOrigin) {
            AlgorithmTransform.updateOrigin(trans);
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

        AlgorithmTransform.updateFileInfo(image, transformedImg, transformedImg.getFileInfo(0).getResolutions(), image
                .getFileInfo()[0].getUnitsOfMeasure(), trans, false, null);

        final int mod = Math.max(1, oYdim / 50);
        final int imgLength = iXdim * iYdim;
        float[] imgBuf;

        try {
            imgBuf = new float[imgLength];
            image.exportData(0, imgLength, imgBuf);
        } catch (final IOException error) {
            imgBuf = null;
            MipavUtil.displayError("Algorithm Transform: Image(s) locked");

            return;
        } catch (final OutOfMemoryError error) {
            imgBuf = null;
            System.gc();
            MipavUtil.displayError("Algorithm Transform: Out of memory");

            return;
        }

        float T00, T01, T02, T10, T11, T12;

        final TransMatrix kTM = AlgorithmTransform.matrixtoInverseArray(trans);
        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        int position;
        float dx, dy, dx1, dy1;

        final float invXRes = 1 / iXres;
        final float invYRes = 1 / iYres;

        int index = 0;
        int deltaX, deltaY;

        for (j = 0; j < oYdim; j++) {

            if ( (progressBar != null) && ( (j % mod) == 0)) {
                progressBar.updateValue((int) ( ((float) j / oYdim * 100) + 0.5f), activeImage);
            }

            jmm = j * oYres;
            j1 = (jmm * T01) + T02;
            j2 = (jmm * T11) + T12;

            for (i = 0; i < oXdim; i++) {

                // transform i,j,k
                value = fillValue; // if voxel is transformed out of bounds
                imm = i * oXres;
                X = (j1 + (imm * T00)) * invXRes;

                if ( (X > -0.5f) && (X < iXdim)) {
                    Y = (j2 + (imm * T10)) * invYRes;

                    if ( (Y > -0.5f) && (Y < iYdim)) {

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

                        value = (dy1 * ( (dx1 * imgBuf[position]) + (dx * imgBuf[position + deltaX])))
                                + (dy * ( (dx1 * imgBuf[position + deltaY]) + (dx * imgBuf[position + deltaY + deltaX])));
                    } // end if Y in bounds
                } // end if X in bounds

                transformedImg.set(index++, value);
            } // end for i
        } // end for j
    }

    /**
     * Transforms using bilinear interpolation.
     * 
     * @param imgBuf Image buffer to be transformed
     * @param transformedImg Transformed image
     * @param trans Transformation matrix to be applied
     * @param iXdim X dimension of input image
     * @param iYdim Y dimension of input image
     * @param iXres X resolution of input image
     * @param iYres Y resolution of input image
     * @param progressBar Progress bar to update. Can be null. Should NOT have cancel button.
     */
    public static final void transformBilinear(final float[] imgBuf, final ModelImage transformedImg,
            final TransMatrix trans, final int iXdim, final int iYdim, final float iXres, final float iYres,
            final ViewJProgressBar progressBar) {
        AlgorithmTransform.transformBilinear(imgBuf, transformedImg, trans, iXdim, iYdim, iXres, iYres, progressBar,
                true);
    }

    /**
     * Transforms using bilinear interpolation.
     * 
     * @param imgBuf Image buffer to be transformed
     * @param transformedImg Transformed image
     * @param trans Transformation matrix to be applied
     * @param iXdim X dimension of input image
     * @param iYdim Y dimension of input image
     * @param iXres X resolution of input image
     * @param iYres Y resolution of input image
     * @param progressBar Progress bar to update. Can be null. Should NOT have cancel button.
     * @param activeImage true if the algorithm is being run in a separate thread, false otherwise, to control progress
     *            bar repainting
     */
    public static final void transformBilinear(final float[] imgBuf, final ModelImage transformedImg,
            final TransMatrix trans, final int iXdim, final int iYdim, final float iXres, final float iYres,
            final ViewJProgressBar progressBar, final boolean activeImage) {
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

        AlgorithmTransform.imgOrigin = transformedImg.getFileInfo(0).getOrigin().clone();

        if (AlgorithmTransform.updateOrigin) {
            AlgorithmTransform.updateOrigin(trans);
        }

        final int mod = Math.max(1, oYdim / 50);

        float T00, T01, T02, T10, T11, T12;

        final TransMatrix kTM = AlgorithmTransform.matrixtoInverseArray(trans);
        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        int position;
        float dx, dy, dx1, dy1;

        final float invXRes = 1 / iXres;
        final float invYRes = 1 / iYres;

        float min = Float.MAX_VALUE;

        for (final float element : imgBuf) {

            if (min > element) {
                min = element;
            }
        }

        int index = 0;

        for (j = 0; j < oYdim; j++) {

            if ( (progressBar != null) && ( (j % mod) == 0)) {
                progressBar.updateValue((int) ( ((float) j / oYdim * 100) + 0.5f), activeImage);
            }

            jmm = j * oYres;
            j1 = (jmm * T01) + T02;
            j2 = (jmm * T11) + T12;

            for (i = 0; i < oXdim; i++) {

                // transform i,j,k
                value = min; // remains zero if voxel is transformed out of bounds
                imm = i * oXres;
                X = (j1 + (imm * T00)) * invXRes;

                if ( (X > -0.5f) && (X < iXdim)) {
                    Y = (j2 + (imm * T10)) * invYRes;

                    if ( (Y > -0.5f) && (Y < iYdim)) {

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

                        value = (dy1 * ( (dx1 * imgBuf[position]) + (dx * imgBuf[position + deltaX])))
                                + (dy * ( (dx1 * imgBuf[position + deltaY]) + (dx * imgBuf[position + deltaY + deltaX])));
                    } // end if Y in bounds
                } // end if X in bounds

                transformedImg.set(index++, value);
            } // end for i
        } // end for j
    }

    /**
     * Transforms using bilinear interpolation.
     * 
     * @param imgBuf Image buffer to be transformed
     * @param tImgBuf Transformed image
     * @param trans Transformation matrix to be applied
     * @param iXdim X dimension of input image
     * @param iYdim Y dimension of input image
     * @param iXres X resolution of input image
     * @param iYres Y resolution of input image
     * @param oXdim X dimension of output image
     * @param oYdim Y dimension of output image
     * @param oXres X resolution of output image
     * @param oYres Y resolution of output image
     * @param progressBar Progress bar to be updated. Can be null. Should NOT have cancel button.
     */
    public static final void transformBilinear(final float[] imgBuf, final float[] tImgBuf, final TransMatrix trans,
            final int iXdim, final int iYdim, final float iXres, final float iYres, final int oXdim, final int oYdim,
            final float oXres, final float oYres, final ViewJProgressBar progressBar) {
        AlgorithmTransform.transformBilinear(imgBuf, tImgBuf, trans, iXdim, iYdim, iXres, iYres, oXdim, oYdim, oXres,
                oYres, progressBar, true);
    }

    /**
     * Transforms using bilinear interpolation.
     * 
     * @param imgBuf Image buffer to be transformed
     * @param tImgBuf Transformed image
     * @param trans Transformation matrix to be applied
     * @param iXdim X dimension of input image
     * @param iYdim Y dimension of input image
     * @param iXres X resolution of input image
     * @param iYres Y resolution of input image
     * @param oXdim X dimension of output image
     * @param oYdim Y dimension of output image
     * @param oXres X resolution of output image
     * @param oYres Y resolution of output image
     * @param progressBar Progress bar to be updated. Can be null. Should NOT have cancel button.
     * @param activeImage true if the algorithm is being run in a separate thread, false otherwise, to control progress
     *            bar repainting
     */
    public static final void transformBilinear(final float[] imgBuf, final float[] tImgBuf, final TransMatrix trans,
            final int iXdim, final int iYdim, final float iXres, final float iYres, final int oXdim, final int oYdim,
            final float oXres, final float oYres, final ViewJProgressBar progressBar, final boolean activeImage) {
        int i, j;
        float X, Y;
        int x0, y0;
        float j1, j2;
        float value;
        float imm, jmm;

        float T00, T01, T02, T10, T11, T12;
        final int mod = Math.max(1, oYdim / 50);
        int deltaX, deltaY;

        final TransMatrix kTM = AlgorithmTransform.matrixtoInverseArray(trans);
        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        int position;
        float dx, dy, dx1, dy1;

        final float invXRes = 1 / iXres;
        final float invYRes = 1 / iYres;

        float min = Float.MAX_VALUE;

        for (final float element : imgBuf) {

            if (min > element) {
                min = element;
            }
        }

        int index = 0;

        for (j = 0; j < oYdim; j++) {

            if ( (progressBar != null) && ( (j % mod) == 0)) {
                progressBar.updateValue((int) ( ((float) j / oYdim * 100) + 0.5f), activeImage);
            }

            jmm = j * oYres;
            j1 = (jmm * T01) + T02;
            j2 = (jmm * T11) + T12;

            for (i = 0; i < oXdim; i++) {

                // transform i,j,k
                value = min; // remains zero if voxel is transformed out of bounds
                imm = i * oXres;
                X = (j1 + (imm * T00)) * invXRes;

                if ( (X > -0.5f) && (X < iXdim)) {
                    Y = (j2 + (imm * T10)) * invYRes;

                    if ( (Y > -0.5f) && (Y < iYdim)) {

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

                        value = (dy1 * ( (dx1 * imgBuf[position]) + (dx * imgBuf[position + deltaX])))
                                + (dy * ( (dx1 * imgBuf[position + deltaY]) + (dx * imgBuf[position + deltaY + deltaX])));

                    } // end if Y in bounds
                } // end if X in bounds

                tImgBuf[index++] = value;
            } // end for i
        } // end for j
    }

    /**
     * Used on color images. USE THIS IF OUTPUT IMAGE HAS DIFFERENT DIM/RES THAN INPUT IMAGE
     * 
     * @param image Input image to be transformed
     * @param transformedImg Transformed image
     * @param trans Transformation matrix to be applied
     * @param oXdim Dimensions of output image
     * @param oYdim Dimensions of output image
     * @param oXres Resolutions of output image
     * @param oYres Resolutions of output image
     */
    public static final void transformBilinearC(final ModelImage image, final ModelImage transformedImg,
            final TransMatrix trans, final int oXdim, final int oYdim, final float oXres, final float oYres) {
        int i, j;
        int X0pos, Y0pos;
        int X1pos, Y1pos;
        float X, Y;
        float x0, y0;
        float x1, y1;
        float imm, jmm;

        AlgorithmTransform.imgOrigin = image.getFileInfo(0).getOrigin().clone();

        if (AlgorithmTransform.updateOrigin) {
            AlgorithmTransform.updateOrigin(trans);
        }

        int iXdim, iYdim;
        float iXres, iYres;

        iXdim = image.getExtents()[0];
        iYdim = image.getExtents()[1];
        iXres = image.getFileInfo(0).getResolutions()[0];
        iYres = image.getFileInfo(0).getResolutions()[1];

        final int imgLength = 4 * iXdim * iYdim;
        final float[] imgBuf = new float[imgLength];
        final float[] imgBuf2 = new float[imgLength];
        int index, indexDest;
        int index00, index01, index10, index11;

        try {
            image.exportData(0, imgLength, imgBuf);
        } catch (final IOException error) {
            MipavUtil.displayError("Algorithm Transform: Image(s) locked");

            return;
        }

        final float[] resolutions = new float[] {oXres, oYres};

        AlgorithmTransform.updateFileInfo(image, transformedImg, resolutions, image.getFileInfo()[0]
                .getUnitsOfMeasure(), trans, false, null);

        float temp1, temp2;
        float T00, T01, T02, T10, T11, T12;

        final TransMatrix kTM = AlgorithmTransform.matrixtoInverseArray(trans);
        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        for (i = 0; i < oXdim; i++) {
            imm = i * oXres;
            temp1 = (imm * T00) + T02;
            temp2 = (imm * T10) + T12;

            for (j = 0; j < oYdim; j++) {

                // transform i,j
                indexDest = 4 * (i + (j * oXdim));
                imgBuf2[indexDest] = 255;
                imgBuf2[indexDest + 1] = 0; // R,G, and B remain zero if pixel is transformed out of bounds
                imgBuf2[indexDest + 2] = 0;
                imgBuf2[indexDest + 3] = 0;
                jmm = j * oYres;
                X = (temp1 + (jmm * T01)) / iXres;

                if ( (X >= 0) && (X < iXdim)) {
                    Y = (temp2 + (jmm * T11)) / iYres;

                    if ( (Y >= 0) && (Y < iYdim)) {

                        if ( (X >= (iXdim - 1)) || (Y >= (iYdim - 1))) { // cannot interpolate on last X or Y
                            X0pos = Math.min((int) (X + 0.5f), (iXdim - 1));
                            Y0pos = Math.min((int) (Y + 0.5f), (iYdim - 1)) * iXdim;
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
                            imgBuf2[indexDest + 1] = (x1 * y1 * imgBuf[index00 + 1]) + (x0 * y1 * imgBuf[index01 + 1])
                                    + (x1 * y0 * imgBuf[index10 + 1]) + (x0 * y0 * imgBuf[index11 + 1]);
                            imgBuf2[indexDest + 2] = (x1 * y1 * imgBuf[index00 + 2]) + (x0 * y1 * imgBuf[index01 + 2])
                                    + (x1 * y0 * imgBuf[index10 + 2]) + (x0 * y0 * imgBuf[index11 + 2]);
                            imgBuf2[indexDest + 3] = (x1 * y1 * imgBuf[index00 + 3]) + (x0 * y1 * imgBuf[index01 + 3])
                                    + (x1 * y0 * imgBuf[index10 + 3]) + (x0 * y0 * imgBuf[index11 + 3]);
                        } // end else
                    } // end if y in bounds
                } // end if x in bounds
            } // end for j
        } // end for i

        try {
            transformedImg.importData(0, imgBuf2, true);
        } catch (final IOException error) {
            MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
        }

        return;
    }

    /**
     * Used on color images. USE THIS IF OUTPUT IMAGE HAS DIFFERENT DIM/RES THAN INPUT IMAGE
     * 
     * @param image Input image to be transformed
     * @param transformedImg Transformed image
     * @param trans Transformation matrix to be applied
     * @param oXdim Dimensions of output image
     * @param oYdim Dimensions of output image
     * @param oXres Resolutions of output image
     * @param oYres Resolutions of output image
     * @param fillValue value used for out of bounds pixel transformations
     */
    public static final void transformBilinearC(final ModelImage image, final ModelImage transformedImg,
            final TransMatrix trans, final int oXdim, final int oYdim, final float oXres, final float oYres,
            final float fillValue) {
        int i, j;
        int X0pos, Y0pos;
        int X1pos, Y1pos;
        float X, Y;
        float x0, y0;
        float x1, y1;
        float imm, jmm;

        AlgorithmTransform.imgOrigin = image.getFileInfo(0).getOrigin().clone();

        if (AlgorithmTransform.updateOrigin) {
            AlgorithmTransform.updateOrigin(trans);
        }

        int iXdim, iYdim;
        float iXres, iYres;

        iXdim = image.getExtents()[0];
        iYdim = image.getExtents()[1];
        iXres = image.getFileInfo(0).getResolutions()[0];
        iYres = image.getFileInfo(0).getResolutions()[1];

        final int imgLength = 4 * iXdim * iYdim;
        final float[] imgBuf = new float[imgLength];
        final float[] imgBuf2 = new float[imgLength];
        int index, indexDest;
        int index00, index01, index10, index11;

        try {
            image.exportData(0, imgLength, imgBuf);
        } catch (final IOException error) {
            MipavUtil.displayError("Algorithm Transform: Image(s) locked");

            return;
        }

        final float[] resolutions = new float[] {oXres, oYres};

        AlgorithmTransform.updateFileInfo(image, transformedImg, resolutions, image.getFileInfo()[0]
                .getUnitsOfMeasure(), trans, false, null);

        float temp1, temp2;
        float T00, T01, T02, T10, T11, T12;

        final TransMatrix kTM = AlgorithmTransform.matrixtoInverseArray(trans);
        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        for (i = 0; i < oXdim; i++) {
            imm = i * oXres;
            temp1 = (imm * T00) + T02;
            temp2 = (imm * T10) + T12;

            for (j = 0; j < oYdim; j++) {

                // transform i,j
                indexDest = 4 * (i + (j * oXdim));
                imgBuf2[indexDest] = 255;
                imgBuf2[indexDest + 1] = fillValue; // if pixel is transformed out of bounds
                imgBuf2[indexDest + 2] = fillValue;
                imgBuf2[indexDest + 3] = fillValue;
                jmm = j * oYres;
                X = (temp1 + (jmm * T01)) / iXres;

                if ( (X >= 0) && (X < iXdim)) {
                    Y = (temp2 + (jmm * T11)) / iYres;

                    if ( (Y >= 0) && (Y < iYdim)) {

                        if ( (X >= (iXdim - 1)) || (Y >= (iYdim - 1))) { // cannot interpolate on last X or Y
                            X0pos = Math.min((int) (X + 0.5f), (iXdim - 1));
                            Y0pos = Math.min((int) (Y + 0.5f), (iYdim - 1)) * iXdim;
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
                            imgBuf2[indexDest + 1] = (x1 * y1 * imgBuf[index00 + 1]) + (x0 * y1 * imgBuf[index01 + 1])
                                    + (x1 * y0 * imgBuf[index10 + 1]) + (x0 * y0 * imgBuf[index11 + 1]);
                            imgBuf2[indexDest + 2] = (x1 * y1 * imgBuf[index00 + 2]) + (x0 * y1 * imgBuf[index01 + 2])
                                    + (x1 * y0 * imgBuf[index10 + 2]) + (x0 * y0 * imgBuf[index11 + 2]);
                            imgBuf2[indexDest + 3] = (x1 * y1 * imgBuf[index00 + 3]) + (x0 * y1 * imgBuf[index01 + 3])
                                    + (x1 * y0 * imgBuf[index10 + 3]) + (x0 * y0 * imgBuf[index11 + 3]);
                        } // end else
                    } // end if y in bounds
                } // end if x in bounds
            } // end for j
        } // end for i

        try {
            transformedImg.importData(0, imgBuf2, true);
        } catch (final IOException error) {
            MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
        }

        return;
    }

    /**
     * Transforms using Nearest neighbor interpolation.
     * 
     * @param imgBuf Image buffer to be transformed
     * @param tImgBuf Transformed image buffer
     * @param trans Transformation matrix to be applied
     * @param xdim X dimension of input AND output image
     * @param ydim Y dimension of input AND output image
     */
    public static final void transformNearestNeighbor2D(float[] imgBuf, final float[] tImgBuf, final TransMatrix trans,
            final int xdim, final int ydim) {
        int i, j;
        float X, Y;
        int xOffset, yOffset;
        float value;

        if (AlgorithmTransform.updateOrigin) {
            AlgorithmTransform.updateOrigin(trans);
        }

        float T00, T01, T02, T10, T11, T12;
        float i1, i2;
        int roundX, roundY;

        final TransMatrix kTM = AlgorithmTransform.matrixtoInverseArray(trans);
        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        float min = Float.MAX_VALUE;

        for (final float element : imgBuf) {

            if (min > element) {
                min = element;
            }
        }

        for (i = 0; i < xdim; i++) {
            i1 = (i * T00) + T02;
            i2 = (i * T10) + T12;

            for (j = 0; j < ydim; j++) {

                // transform i,j
                X = i1 + (j * T01);
                Y = i2 + (j * T11);

                // set intensity of i,j,k to new transformed coordinate if
                // x,y,z is w/in dimensions of image
                roundX = (int) (X + 0.5f);
                roundY = (int) (Y + 0.5f);

                if ( (X < -0.5) || (X >= xdim) || (Y < -0.5) || (Y >= ydim)) {
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
     * @param imgBuf Image buffer to be transformed
     * @param tImgBuf Transformed image buffer
     * @param trans Transformation matrix to be applied
     * @param iXdim X dimension of input image
     * @param iYdim Y dimension of input image
     * @param iXres X resolution of input image
     * @param iYres Y resolution of input image
     * @param oXdim X dimension of output image
     * @param oYdim Y dimension of output image
     * @param oXres X resolution of output image
     * @param oYres Y resolution of output image
     */
    public static final void transformNearestNeighbor2D(final float[] imgBuf, final float[] tImgBuf,
            final TransMatrix trans, final int iXdim, final int iYdim, final float iXres, final float iYres,
            final int oXdim, final int oYdim, final float oXres, final float oYres) {
        int i, j;
        float X, Y;
        int xOffset, yOffset;
        float value;
        float imm, jmm;

        int roundX, roundY;

        float T00, T01, T02, T10, T11, T12;
        float i1, i2;

        final TransMatrix kTM = AlgorithmTransform.matrixtoInverseArray(trans);
        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        float min = Float.MAX_VALUE;

        for (final float element : imgBuf) {

            if (min > element) {
                min = element;
            }
        }

        for (i = 0; i < oXdim; i++) {
            imm = i * oXres;
            i1 = (imm * T00) + T02;
            i2 = (imm * T10) + T12;

            for (j = 0; j < oYdim; j++) {

                // transform i,j
                jmm = j * oYres;
                X = i1 + (jmm * T01);
                Y = i2 + (jmm * T11);

                // set intensity of i,j,k to new transformed coordinate if
                // x,y,z is w/in dimensions of image
                X = X / iXres;
                Y = Y / iYres;
                roundX = (int) (X + 0.5f);
                roundY = (int) (Y + 0.5f);

                if ( (X < -0.5) || (X >= iXdim) || (Y < -0.5) || (Y >= iYdim)) {
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
     * @param imgBuf Image array to transform
     * @param transformedImg Transformed image.
     * @param Xdim X dimension of input image
     * @param Ydim Y dimension of input image
     * @param Zdim Z dimension of input image
     * @param trans Transformation matrix to be applied
     */
    public static final void transformNearestNeighbor3D(final float[] imgBuf, final ModelImage transformedImg,
            final int Xdim, final int Ydim, final int Zdim, final TransMatrix trans) {
        int i, j, k;
        float X, Y, Z;
        int xOffset, yOffset, zOffset;
        float value;
        int sliceSize;

        sliceSize = Xdim * Ydim;

        int roundX, roundY, roundZ;
        float i1, i2, i3, j1, j2, j3;

        AlgorithmTransform.imgOrigin = transformedImg.getFileInfo(0).getOrigin().clone();

        if (AlgorithmTransform.updateOrigin) {
            AlgorithmTransform.updateOrigin(trans);
        }

        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        final TransMatrix kTM = AlgorithmTransform.matrixtoInverseArray(trans);
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

        for (final float element : imgBuf) {

            if (min > element) {
                min = element;
            }
        }

        for (i = 0; i < Xdim; i++) {
            i1 = (i * T00) + T03;
            i2 = (i * T10) + T13;
            i3 = (i * T20) + T23;

            for (j = 0; j < Ydim; j++) {
                j1 = j * T01;
                j2 = j * T11;
                j3 = j * T21;

                for (k = 0; k < Zdim; k++) {
                    X = i1 + j1 + (k * T02);
                    Y = i2 + j2 + (k * T12);
                    Z = i3 + j3 + (k * T22);
                    roundX = (int) (X + 0.5f);
                    roundY = (int) (Y + 0.5f);
                    roundZ = (int) (Z + 0.5f);

                    if ( (X < -0.5) || (X >= Xdim) || (Y < -0.5) || (Y >= Ydim) || (Z < -0.5) || (Z >= Zdim)) {
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
     * @param image Image to transform
     * @param transformedImg Transformed image.
     * @param trans Transformation matrix to be applied
     * @param progressBar The progress bar. Can be null. Should NOT have a cancel button.
     */
    public static final void transformTrilinear(final ModelImage image, final ModelImage transformedImg,
            final TransMatrix trans, final ViewJProgressBar progressBar) {
        AlgorithmTransform.transformTrilinear(image, transformedImg, trans, progressBar, true);
    }

    /**
     * Transforms and resamples volume using trilinear interpolation.
     * 
     * @param image Image to transform
     * @param transformedImg Transformed image.
     * @param trans Transformation matrix to be applied
     * @param progressBar The progress bar. Can be null. Should NOT have a cancel button.
     * @param activeImage true if the algorithm is being run in a separate thread, false otherwise, to control progress
     *            bar repainting
     */
    public static final void transformTrilinear(final ModelImage image, final ModelImage transformedImg,
            final TransMatrix trans, final ViewJProgressBar progressBar, final boolean activeImage) {
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

        AlgorithmTransform.imgOrigin = image.getFileInfo(0).getOrigin().clone();

        if (AlgorithmTransform.updateOrigin) {
            AlgorithmTransform.updateOrigin(trans);
        }

        final TransMatrix kTM = AlgorithmTransform.matrixtoInverseArray(trans);
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

        final int imgLength = iXdim * iYdim * iZdim;
        final float[] imgBuffer = new float[imgLength];

        float min = Float.MAX_VALUE;

        for (final float element : imgBuffer) {

            if (min > element) {
                min = element;
            }
        }

        try {
            image.exportData(0, imgLength, imgBuffer);
        } catch (final IOException error) {
            MipavUtil.displayError("Algorithm Transform: Image(s) locked");

            return;
        }

        mod = Math.max(1, oZdim / 50);

        final float invXRes = 1 / iXres;
        final float invYRes = 1 / iYres;
        final float invZRes = 1 / iZres;

        int index = 0;

        for (k = 0; k < oZdim; k++) {

            if ( (progressBar != null) && ( (k % mod) == 0)) {
                progressBar.updateValue((int) ( ((float) k / oZdim * 100) + 0.5f), activeImage);
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

                    if ( (X > -0.5f) && (X < iXdim)) {
                        Y = (j2 + (imm * T10)) * invYRes;

                        if ( (Y > -0.5f) && (Y < iYdim)) {
                            Z = (j3 + (imm * T20)) * invZRes;

                            if ( (Z > -0.5f) && (Z < iZdim)) {

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

                                b1 = (dy1 * ( (dx1 * imgBuffer[position1]) + (dx * imgBuffer[position1 + deltaX])))
                                        + (dy * ( (dx1 * imgBuffer[position1 + deltaY]) + (dx * imgBuffer[position1
                                                + deltaY + deltaX])));

                                b2 = (dy1 * ( (dx1 * imgBuffer[position2]) + (dx * imgBuffer[position2 + deltaX])))
                                        + (dy * ( (dx1 * imgBuffer[position2 + deltaY]) + (dx * imgBuffer[position2
                                                + deltaY + deltaX])));

                                value = ( (1 - dz) * b1) + (dz * b2);

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
     * @param image Image to transform
     * @param transformedImg Transformed image.
     * @param trans Transformation matrix to be applied
     * @param progressBar The progress bar. Can be null. Should NOT have a cancel button.
     * @param activeImage true if the algorithm is being run in a separate thread, false otherwise, to control progress
     *            bar repainting
     * @param fillValue value used if transformed pixel is out of bounds
     */
    public static final void transformTrilinear(final ModelImage image, final ModelImage transformedImg,
            final TransMatrix trans, final ViewJProgressBar progressBar, final boolean activeImage,
            final float fillValue) {
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

        AlgorithmTransform.imgOrigin = image.getFileInfo(0).getOrigin().clone();

        if (AlgorithmTransform.updateOrigin) {
            AlgorithmTransform.updateOrigin(trans);
        }

        final TransMatrix kTM = AlgorithmTransform.matrixtoInverseArray(trans);
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

        final int imgLength = iXdim * iYdim * iZdim;
        final float[] imgBuffer = new float[imgLength];

        try {
            image.exportData(0, imgLength, imgBuffer);
        } catch (final IOException error) {
            MipavUtil.displayError("Algorithm Transform: Image(s) locked");

            return;
        }

        mod = Math.max(1, oZdim / 50);

        final float invXRes = 1 / iXres;
        final float invYRes = 1 / iYres;
        final float invZRes = 1 / iZres;

        int index = 0;

        for (k = 0; k < oZdim; k++) {

            if ( (progressBar != null) && ( (k % mod) == 0)) {
                progressBar.updateValue((int) ( ((float) k / oZdim * 100) + 0.5f), activeImage);
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
                    value = fillValue; // if voxel is transformed out of bounds
                    imm = i * oXres;
                    X = (j1 + (imm * T00)) * invXRes;

                    if ( (X > -0.5f) && (X < iXdim)) {
                        Y = (j2 + (imm * T10)) * invYRes;

                        if ( (Y > -0.5f) && (Y < iYdim)) {
                            Z = (j3 + (imm * T20)) * invZRes;

                            if ( (Z > -0.5f) && (Z < iZdim)) {

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

                                b1 = (dy1 * ( (dx1 * imgBuffer[position1]) + (dx * imgBuffer[position1 + deltaX])))
                                        + (dy * ( (dx1 * imgBuffer[position1 + deltaY]) + (dx * imgBuffer[position1
                                                + deltaY + deltaX])));

                                b2 = (dy1 * ( (dx1 * imgBuffer[position2]) + (dx * imgBuffer[position2 + deltaX])))
                                        + (dy * ( (dx1 * imgBuffer[position2 + deltaY]) + (dx * imgBuffer[position2
                                                + deltaY + deltaX])));

                                value = ( (1 - dz) * b1) + (dz * b2);

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
     * @param imgBuffer Image array
     * @param transformedImg Image after transform
     * @param trans Transformation matrix to be applied
     * @param iXdim X dimension of input image
     * @param iYdim Y dimension of input image
     * @param iZdim Z dimension of input image
     * @param iXres X resolution of input image
     * @param iYres Y resolution of input image
     * @param iZres Z resolution of input image
     * @param progressBar Progress bar to update. Can be null. Should NOT have a cancel button.
     */
    public static final void transformTrilinear(final float[] imgBuffer, final ModelImage transformedImg,
            final TransMatrix trans, final int iXdim, final int iYdim, final int iZdim, final float iXres,
            final float iYres, final float iZres, final ViewJProgressBar progressBar) {
        AlgorithmTransform.transformTrilinear(imgBuffer, transformedImg, trans, iXdim, iYdim, iZdim, iXres, iYres,
                iZres, progressBar, true);
    }

    /**
     * Transforms and resamples volume using trilinear interpolation.
     * 
     * @param imgBuffer Image array
     * @param transformedImg Image after transform
     * @param trans Transformation matrix to be applied
     * @param iXdim X dimension of input image
     * @param iYdim Y dimension of input image
     * @param iZdim Z dimension of input image
     * @param iXres X resolution of input image
     * @param iYres Y resolution of input image
     * @param iZres Z resolution of input image
     * @param progressBar Progress bar to update. Can be null. Should NOT have a cancel button.
     * @param activeImage true if the algorithm is being run in a separate thread, false otherwise, to control progress
     *            bar repainting
     */
    public static final void transformTrilinear(final float[] imgBuffer, final ModelImage transformedImg,
            final TransMatrix trans, final int iXdim, final int iYdim, final int iZdim, final float iXres,
            final float iYres, final float iZres, final ViewJProgressBar progressBar, final boolean activeImage) {

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

            AlgorithmTransform.imgOrigin = transformedImg.getFileInfo(0).getOrigin().clone();

            if (AlgorithmTransform.updateOrigin) {
                AlgorithmTransform.updateOrigin(trans);
            }

            final TransMatrix kTM = AlgorithmTransform.matrixtoInverseArray(trans);
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

            final float invXRes = 1 / iXres;
            final float invYRes = 1 / iYres;
            final float invZRes = 1 / iZres;

            float min = Float.MAX_VALUE;

            for (final float element : imgBuffer) {

                if (min > element) {
                    min = element;
                }
            }

            final int mod = Math.max(1, oZdim / 50);
            int index = 0;

            for (k = 0; k < oZdim; k++) {

                if ( (progressBar != null) && ( (k % mod) == 0)) {
                    progressBar.updateValue((int) ( ((float) k / oZdim * 100) + 0.5f), activeImage);
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

                        if ( (X > -0.5f) && (X < iXdim)) {
                            Y = (j2 + (imm * T10)) * invYRes;

                            if ( (Y > -0.5f) && (Y < iYdim)) {
                                Z = (j3 + (imm * T20)) * invZRes;

                                if ( (Z > -0.5f) && (Z < iZdim)) {

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

                                    b1 = (dy1 * ( (dx1 * imgBuffer[position1]) + (dx * imgBuffer[position1 + deltaX])))
                                            + (dy * ( (dx1 * imgBuffer[position1 + deltaY]) + (dx * imgBuffer[position1
                                                    + deltaY + deltaX])));

                                    b2 = (dy1 * ( (dx1 * imgBuffer[position2]) + (dx * imgBuffer[position2 + deltaX])))
                                            + (dy * ( (dx1 * imgBuffer[position2 + deltaY]) + (dx * imgBuffer[position2
                                                    + deltaY + deltaX])));

                                    value = ( (1 - dz) * b1) + (dz * b2);
                                } // end if Z in bounds
                            } // end if Y in bounds
                        } // end if X in bounds

                        transformedImg.set(index++, value);
                    } // end for k
                } // end for j
            } // end for i
        } finally {}
    }

    /**
     * Transforms and resamples volume using trilinear interpolation.
     * 
     * @param imgBuffer Image buffer to be transformed
     * @param tImgBuf Transformed image
     * @param trans Transformation matrix to be applied
     * @param iXdim X dimension of input image
     * @param iYdim Y dimension of input image
     * @param iZdim Z dimension of input image
     * @param iXres X resolution of input image
     * @param iYres Y resolution of input image
     * @param iZres Z resolution of input image
     * @param oXdim X dimension of output image
     * @param oYdim Y dimension of output image
     * @param oZdim Z dimension of output image
     * @param oXres X resolution of output image
     * @param oYres Y resolution of output image
     * @param oZres Z resolution of output image
     * @param progressBar Progress bar. Can be null. Should NOT have cancel button.
     */
    public static final void transformTrilinear(final float[] imgBuffer, final float[] tImgBuf,
            final TransMatrix trans, final int iXdim, final int iYdim, final int iZdim, final float iXres,
            final float iYres, final float iZres, final int oXdim, final int oYdim, final int oZdim, final float oXres,
            final float oYres, final float oZres, final ViewJProgressBar progressBar) {
        AlgorithmTransform.transformTrilinear(imgBuffer, tImgBuf, trans, iXdim, iYdim, iZdim, iXres, iYres, iZres,
                oXdim, oYdim, oZdim, oXres, oYres, oZres, progressBar, true);
    }

    /**
     * Transforms and resamples volume using trilinear interpolation.
     * 
     * @param imgBuffer Image buffer to be transformed
     * @param tImgBuf Transformed image
     * @param trans Transformation matrix to be applied
     * @param iXdim X dimension of input image
     * @param iYdim Y dimension of input image
     * @param iZdim Z dimension of input image
     * @param iXres X resolution of input image
     * @param iYres Y resolution of input image
     * @param iZres Z resolution of input image
     * @param oXdim X dimension of output image
     * @param oYdim Y dimension of output image
     * @param oZdim Z dimension of output image
     * @param oXres X resolution of output image
     * @param oYres Y resolution of output image
     * @param oZres Z resolution of output image
     * @param progressBar Progress bar. Can be null. Should NOT have cancel button.
     * @param activeImage true if the algorithm is being run in a separate thread, false otherwise, to control progress
     *            bar repainting
     */
    public static final void transformTrilinear(final float[] imgBuffer, final float[] tImgBuf,
            final TransMatrix trans, final int iXdim, final int iYdim, final int iZdim, final float iXres,
            final float iYres, final float iZres, final int oXdim, final int oYdim, final int oZdim, final float oXres,
            final float oYres, final float oZres, final ViewJProgressBar progressBar, final boolean activeImage) {

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

        final int mod = Math.max(1, oZdim / 50);

        final TransMatrix kTM = AlgorithmTransform.matrixtoInverseArray(trans);

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

        final float invXRes = 1 / iXres;
        final float invYRes = 1 / iYres;
        final float invZRes = 1 / iZres;

        float min = Float.MAX_VALUE;

        for (final float element : imgBuffer) {

            if (min > element) {
                min = element;
            }
        }

        int index = 0;

        for (k = 0; k < oZdim; k++) {

            if ( (progressBar != null) && ( (k % mod) == 0)) {
                progressBar.updateValue((int) ( ((float) k / oZdim * 100) + 0.5f), activeImage);
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

                    if ( (X > -0.5f) && (X < iXdim)) {
                        Y = (j2 + (imm * T10)) * invYRes;

                        if ( (Y > -0.5f) && (Y < iYdim)) {
                            Z = (j3 + (imm * T20)) * invZRes;

                            if ( (Z > -0.5f) && (Z < iZdim)) {

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

                                b1 = (dy1 * ( (dx1 * imgBuffer[position1]) + (dx * imgBuffer[position1 + deltaX])))
                                        + (dy * ( (dx1 * imgBuffer[position1 + deltaY]) + (dx * imgBuffer[position1
                                                + deltaY + deltaX])));

                                b2 = (dy1 * ( (dx1 * imgBuffer[position2]) + (dx * imgBuffer[position2 + deltaX])))
                                        + (dy * ( (dx1 * imgBuffer[position2 + deltaY]) + (dx * imgBuffer[position2
                                                + deltaY + deltaX])));

                                value = ( (1 - dz) * b1) + (dz * b2);
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
     * @param image Image from which the data is derived
     * @param resultImage Image to put result in; can be null.
     * @param xfrm Transformation to apply.
     * @param progressBar Buffer containing image data.
     * 
     * @return DOCUMENT ME!
     */
    public static ModelImage transformTrilinear4D(final ModelImage image, final ModelImage resultImage,
            final TransMatrix xfrm, final ViewJProgressBar progressBar) {

        try {
            float X, Y, Z;
            int x0, y0, z0;
            float value;
            int sliceSize;
            float imm, jmm, kmm;
            final int counter = 0;
            int position1, position2;
            float b1, b2;
            float dx, dy, dz, dx1, dy1;
            float k1, k2, k3, j1, j2, j3;
            final float[] resols = new float[3];

            final int xDim = image.getExtents()[0];
            final int yDim = image.getExtents()[1];
            final int zDim = image.getExtents()[2];
            final float xRes = image.getFileInfo(0).getResolutions()[0];
            final float yRes = image.getFileInfo(0).getResolutions()[1];
            final float zRes = image.getFileInfo(0).getResolutions()[2];

            final int bufferSize = image.getSliceSize() * image.getExtents()[2];
            final float[] imgBuffer = new float[bufferSize];

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

            final TransMatrix kTM = AlgorithmTransform.matrixtoInverseArray(xfrm);
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

            final float invXRes = 1 / xRes;
            final float invYRes = 1 / yRes;
            final float invZRes = 1 / zRes;

            int index = 0;

            for (int l = 0; l < tDim; l++) {

                if ( (progressBar != null) && ( (counter % mod) == 0)) {
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

                            if ( (X >= 0) && (X < xDim)) {
                                Y = (j2 + (imm * T10)) * invYRes;

                                if ( (Y >= 0) && (Y < yDim)) {
                                    Z = (j3 + (imm * T20)) * invZRes;

                                    if ( (Z >= 0) && (Z < zDim)) {

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

                                        b1 = (dy1 * ( (dx1 * imgBuffer[position1]) + (dx * imgBuffer[position1 + 1])))
                                                + (dy * ( (dx1 * imgBuffer[position1 + xDim]) + (dx * imgBuffer[position1
                                                        + xDim + 1])));

                                        b2 = (dy1 * ( (dx1 * imgBuffer[position2]) + (dx * imgBuffer[position2 + 1])))
                                                + (dy * ( (dx1 * imgBuffer[position2 + xDim]) + (dx * imgBuffer[position2
                                                        + xDim + 1])));

                                        value = ( (1 - dz) * b1) + (dz * b2);

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
                            resultImage.exportData( (l + 1) * imgBuffer.length, imgBuffer.length, imgBuffer);
                        } else {
                            image.exportData( (l + 1) * imgBuffer.length, imgBuffer.length, imgBuffer);
                        }
                    } catch (final IOException error) {
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
        } finally {}
    }

    /**
     * Performs trilinear interpolation on color image data.
     * 
     * @param image Image from which the data is derived
     * @param resultImage Image to put result in; can be null.
     * @param imgBuffer Buffer containing image data.
     * @param xfrm Transformation to apply.
     * @param progressBar DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public static ModelImage transformTrilinearC(final ModelImage image, final ModelImage resultImage,
            final float[] imgBuffer, final TransMatrix xfrm, final ViewJProgressBar progressBar) {

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
            final int xDim = image.getExtents()[0];
            final int yDim = image.getExtents()[1];
            final int zDim = image.getExtents()[2];
            final float[] resols = new float[3];
            final int length = xDim * yDim * zDim;
            final int mod = length / 100; // mod is 1 percent of length
            float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
            int index;
            int index000, index001, index010, index011, index100, index101, index110, index111;
            float w000, w001, w010, w011, w100, w101, w110, w111;

            final float[] imgBuffer2 = new float[imgBuffer.length];

            resols[0] = image.getFileInfo()[0].getResolutions()[0];
            resols[1] = image.getFileInfo()[0].getResolutions()[1];
            resols[2] = image.getFileInfo()[0].getResolutions()[2];
            sliceSize = xDim * yDim;
            final TransMatrix kTM = AlgorithmTransform.matrixtoInverseArray(xfrm);
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

                        if ( (progressBar != null) && ( (counter % mod) == 0)) {
                            progressBar.updateValue(Math.round((float) counter / (length - 1) * 100), true);
                        }

                        // transform i,j,k
                        valueA = 0; // remains zero if voxel is transformed out of bounds
                        valueR = 0;
                        valueG = 0;
                        valueB = 0;
                        imm = i * resols[0];
                        jmm = j * resols[1];
                        kmm = k * resols[2];

                        X = (imm * T00) + (jmm * T01) + (kmm * T02) + T03;
                        X = X / resols[0];
                        roundX = (int) (X + 0.5f);

                        if ( (X >= 0) && (X < xDim)) {
                            Y = (imm * T10) + (jmm * T11) + (kmm * T12) + T13;
                            Y = Y / resols[1];
                            roundY = (int) (Y + 0.5f);

                            if ( (Y >= 0) && (Y < yDim)) {
                                Z = (imm * T20) + (jmm * T21) + (kmm * T22) + T23;
                                Z = Z / resols[2];
                                roundZ = (int) (Z + 0.5f);

                                if ( (Z >= 0) && (Z < zDim)) {

                                    if ( (X >= (xDim - 1)) || (Y >= (yDim - 1)) || (Z >= (zDim - 1))) { // cannot
                                        // interpolate
                                        // on last X, Y,
                                        // or Z
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

                                        valueA = (w000 * imgBuffer[index000]) + (w001 * imgBuffer[index001])
                                                + (w010 * imgBuffer[index010]) + (w011 * imgBuffer[index011])
                                                + (w100 * imgBuffer[index100]) + (w101 * imgBuffer[index101])
                                                + (w110 * imgBuffer[index110]) + (w111 * imgBuffer[index111]);
                                        valueR = (w000 * imgBuffer[index000 + 1]) + (w001 * imgBuffer[index001 + 1])
                                                + (w010 * imgBuffer[index010 + 1]) + (w011 * imgBuffer[index011 + 1])
                                                + (w100 * imgBuffer[index100 + 1]) + (w101 * imgBuffer[index101 + 1])
                                                + (w110 * imgBuffer[index110 + 1]) + (w111 * imgBuffer[index111 + 1]);
                                        valueG = (w000 * imgBuffer[index000 + 2]) + (w001 * imgBuffer[index001 + 2])
                                                + (w010 * imgBuffer[index010 + 2]) + (w011 * imgBuffer[index011 + 2])
                                                + (w100 * imgBuffer[index100 + 2]) + (w101 * imgBuffer[index101 + 2])
                                                + (w110 * imgBuffer[index110 + 2]) + (w111 * imgBuffer[index111 + 2]);
                                        valueB = (w000 * imgBuffer[index000 + 3]) + (w001 * imgBuffer[index001 + 3])
                                                + (w010 * imgBuffer[index010 + 3]) + (w011 * imgBuffer[index011 + 3])
                                                + (w100 * imgBuffer[index100 + 3]) + (w101 * imgBuffer[index101 + 3])
                                                + (w110 * imgBuffer[index110 + 3]) + (w111 * imgBuffer[index111 + 3]);
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
            } catch (final IOException error) {
                MipavUtil.displayError("ViewJFrameTriImage: IOException Error on importData");

                return null;
            }
        } finally {}
    }

    /**
     * Transforms and resamples volume using trilinear interpolation Use on color images USE THIS IF OUTPUT IMAGE HAS
     * DIFFERENT DIM/RES THAN INPUT IMAGE.
     * 
     * @param image Image to transform
     * @param transformedImg Result image.
     * @param trans Transformation matrix to be applied
     * @param oXdim DOCUMENT ME!
     * @param oYdim DOCUMENT ME!
     * @param oZdim DOCUMENT ME!
     * @param oXres DOCUMENT ME!
     * @param oYres DOCUMENT ME!
     * @param oZres DOCUMENT ME!
     */
    public static final void transformTrilinearC(final ModelImage image, final ModelImage transformedImg,
            final TransMatrix trans, final int oXdim, final int oYdim, final int oZdim, final float oXres,
            final float oYres, final float oZres) {
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
        AlgorithmTransform.imgOrigin = image.getFileInfo(0).getOrigin().clone();

        if (AlgorithmTransform.updateOrigin) {
            AlgorithmTransform.updateOrigin(trans);
        }

        final TransMatrix kTM = AlgorithmTransform.matrixtoInverseArray(trans);

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

        final int imgLength = 4 * iXdim * iYdim * iZdim;
        final float[] imgBuffer = new float[imgLength];
        final float[] imgBuffer2 = new float[imgLength];
        int indexDest;
        int index;
        int index000, index001, index010, index011, index100, index101, index110, index111;

        try {
            image.exportData(0, imgLength, imgBuffer);
        } catch (final IOException error) {
            MipavUtil.displayError("Algorithm Transform: Image(s) locked");

            return;
        }

        // int [] extents = new int [] {oXdim, oYdim, oZdim};
        final float[] resolutions = new float[] {oXres, oYres, oZres};

        AlgorithmTransform.updateFileInfo(image, transformedImg, resolutions, image.getFileInfo()[0]
                .getUnitsOfMeasure(), trans, false, null);

        for (i = 0; i < oXdim; i++) {
            imm = i * oXres;
            i1 = (imm * T00) + T03;
            i2 = (imm * T10) + T13;
            i3 = (imm * T20) + T23;

            for (j = 0; j < oYdim; j++) {
                jmm = j * oYres;
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
                    kmm = k * oZres;
                    X = (temp3 + (kmm * T02)) / iXres;
                    roundX = (int) (X + 0.5f);

                    if ( (X >= 0) && (X < iXdim)) {
                        Y = temp2 + (kmm * T12);
                        Y = Y / iYres;
                        roundY = (int) (Y + 0.5f);

                        if ( (Y >= 0) && (Y < iYdim)) {
                            Z = temp1 + (kmm * T22);
                            Z = Z / iZres;
                            roundZ = (int) (Z + 0.5f);

                            if ( (Z >= 0) && (Z < iZdim)) {

                                if ( (X >= (iXdim - 1)) || (Y >= (iYdim - 1)) || (Z >= (iZdim - 1))) { // cannot
                                    // interpolate
                                    // on last X, Y,
                                    // or Z
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

                                    imgBuffer2[indexDest + 1] = (x1 * temp4 * imgBuffer[index000 + 1])
                                            + (x0 * temp4 * imgBuffer[index001 + 1])
                                            + (x1 * temp5 * imgBuffer[index010 + 1])
                                            + (x0 * temp5 * imgBuffer[index011 + 1])
                                            + (x1 * temp6 * imgBuffer[index100 + 1])
                                            + (x0 * temp6 * imgBuffer[index101 + 1])
                                            + (x1 * temp7 * imgBuffer[index110 + 1])
                                            + (x0 * temp7 * imgBuffer[index111 + 1]);
                                    imgBuffer2[indexDest + 2] = (x1 * temp4 * imgBuffer[index000 + 2])
                                            + (x0 * temp4 * imgBuffer[index001 + 2])
                                            + (x1 * temp5 * imgBuffer[index010 + 2])
                                            + (x0 * temp5 * imgBuffer[index011 + 2])
                                            + (x1 * temp6 * imgBuffer[index100 + 2])
                                            + (x0 * temp6 * imgBuffer[index101 + 2])
                                            + (x1 * temp7 * imgBuffer[index110 + 2])
                                            + (x0 * temp7 * imgBuffer[index111 + 2]);
                                    imgBuffer2[indexDest + 3] = (x1 * temp4 * imgBuffer[index000 + 3])
                                            + (x0 * temp4 * imgBuffer[index001 + 3])
                                            + (x1 * temp5 * imgBuffer[index010 + 3])
                                            + (x0 * temp5 * imgBuffer[index011 + 3])
                                            + (x1 * temp6 * imgBuffer[index100 + 3])
                                            + (x0 * temp6 * imgBuffer[index101 + 3])
                                            + (x1 * temp7 * imgBuffer[index110 + 3])
                                            + (x0 * temp7 * imgBuffer[index111 + 3]);

                                }
                            } // end if Z in bounds
                        } // end if Y in bounds
                    } // end if X in bounds
                } // end for k
            } // end for j
        } // end for i

        try {
            transformedImg.importData(0, imgBuffer2, true);
        } catch (final IOException error) {
            MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
        }
    }

    /**
     * Transforms and resamples volume using trilinear interpolation Use on color images USE THIS IF OUTPUT IMAGE HAS
     * DIFFERENT DIM/RES THAN INPUT IMAGE.
     * 
     * @param image Image to transform
     * @param transformedImg Result image.
     * @param trans Transformation matrix to be applied
     * @param oXdim DOCUMENT ME!
     * @param oYdim DOCUMENT ME!
     * @param oZdim DOCUMENT ME!
     * @param oXres DOCUMENT ME!
     * @param oYres DOCUMENT ME!
     * @param oZres DOCUMENT ME!
     * @param fillValue value if transformed pixel is out of bounds
     */
    public static final void transformTrilinearC(final ModelImage image, final ModelImage transformedImg,
            final TransMatrix trans, final int oXdim, final int oYdim, final int oZdim, final float oXres,
            final float oYres, final float oZres, final float fillValue) {
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
        AlgorithmTransform.imgOrigin = image.getFileInfo(0).getOrigin().clone();

        if (AlgorithmTransform.updateOrigin) {
            AlgorithmTransform.updateOrigin(trans);
        }

        final TransMatrix kTM = AlgorithmTransform.matrixtoInverseArray(trans);

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

        final int imgLength = 4 * iXdim * iYdim * iZdim;
        final float[] imgBuffer = new float[imgLength];
        final float[] imgBuffer2 = new float[imgLength];
        int indexDest;
        int index;
        int index000, index001, index010, index011, index100, index101, index110, index111;

        try {
            image.exportData(0, imgLength, imgBuffer);
        } catch (final IOException error) {
            MipavUtil.displayError("Algorithm Transform: Image(s) locked");

            return;
        }

        // int [] extents = new int [] {oXdim, oYdim, oZdim};
        final float[] resolutions = new float[] {oXres, oYres, oZres};

        AlgorithmTransform.updateFileInfo(image, transformedImg, resolutions, image.getFileInfo()[0]
                .getUnitsOfMeasure(), trans, false, null);

        for (i = 0; i < oXdim; i++) {
            imm = i * oXres;
            i1 = (imm * T00) + T03;
            i2 = (imm * T10) + T13;
            i3 = (imm * T20) + T23;

            for (j = 0; j < oYdim; j++) {
                jmm = j * oYres;
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
                    imgBuffer2[indexDest + 1] = fillValue; // if voxel is transformed out of bounds
                    imgBuffer2[indexDest + 2] = fillValue;
                    imgBuffer2[indexDest + 3] = fillValue;
                    kmm = k * oZres;
                    X = (temp3 + (kmm * T02)) / iXres;
                    roundX = (int) (X + 0.5f);

                    if ( (X >= 0) && (X < iXdim)) {
                        Y = temp2 + (kmm * T12);
                        Y = Y / iYres;
                        roundY = (int) (Y + 0.5f);

                        if ( (Y >= 0) && (Y < iYdim)) {
                            Z = temp1 + (kmm * T22);
                            Z = Z / iZres;
                            roundZ = (int) (Z + 0.5f);

                            if ( (Z >= 0) && (Z < iZdim)) {

                                if ( (X >= (iXdim - 1)) || (Y >= (iYdim - 1)) || (Z >= (iZdim - 1))) { // cannot
                                    // interpolate
                                    // on last X, Y,
                                    // or Z
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

                                    imgBuffer2[indexDest + 1] = (x1 * temp4 * imgBuffer[index000 + 1])
                                            + (x0 * temp4 * imgBuffer[index001 + 1])
                                            + (x1 * temp5 * imgBuffer[index010 + 1])
                                            + (x0 * temp5 * imgBuffer[index011 + 1])
                                            + (x1 * temp6 * imgBuffer[index100 + 1])
                                            + (x0 * temp6 * imgBuffer[index101 + 1])
                                            + (x1 * temp7 * imgBuffer[index110 + 1])
                                            + (x0 * temp7 * imgBuffer[index111 + 1]);
                                    imgBuffer2[indexDest + 2] = (x1 * temp4 * imgBuffer[index000 + 2])
                                            + (x0 * temp4 * imgBuffer[index001 + 2])
                                            + (x1 * temp5 * imgBuffer[index010 + 2])
                                            + (x0 * temp5 * imgBuffer[index011 + 2])
                                            + (x1 * temp6 * imgBuffer[index100 + 2])
                                            + (x0 * temp6 * imgBuffer[index101 + 2])
                                            + (x1 * temp7 * imgBuffer[index110 + 2])
                                            + (x0 * temp7 * imgBuffer[index111 + 2]);
                                    imgBuffer2[indexDest + 3] = (x1 * temp4 * imgBuffer[index000 + 3])
                                            + (x0 * temp4 * imgBuffer[index001 + 3])
                                            + (x1 * temp5 * imgBuffer[index010 + 3])
                                            + (x0 * temp5 * imgBuffer[index011 + 3])
                                            + (x1 * temp6 * imgBuffer[index100 + 3])
                                            + (x0 * temp6 * imgBuffer[index101 + 3])
                                            + (x1 * temp7 * imgBuffer[index110 + 3])
                                            + (x0 * temp7 * imgBuffer[index111 + 3]);

                                }
                            } // end if Z in bounds
                        } // end if Y in bounds
                    } // end if X in bounds
                } // end for k
            } // end for j
        } // end for i

        try {
            transformedImg.importData(0, imgBuffer2, true);
        } catch (final IOException error) {
            MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
        }
    }

    /**
     * Performs trilinear interpolation on color image data in a 4D image. Works time slice by time slice.
     * 
     * @param image Image from which the data is derived
     * @param resultImage Image to put result in; can be null.
     * @param xfrm Transformation to apply.
     * @param progressBar Buffer containing image data.
     * 
     * @return DOCUMENT ME!
     */
    public static ModelImage transformTrilinearC4D(final ModelImage image, final ModelImage resultImage,
            final TransMatrix xfrm, final ViewJProgressBar progressBar) {

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
            final int xDim = image.getExtents()[0];
            final int yDim = image.getExtents()[1];
            final int zDim = image.getExtents()[2];
            final float[] resols = new float[3];

            final int bufferSize = image.getSliceSize() * image.getExtents()[2];

            final float[] imgBuffer = new float[bufferSize];

            try {

                if (resultImage != null) {
                    resultImage.exportData(0, bufferSize, imgBuffer);
                } else {
                    image.exportData(0, bufferSize, imgBuffer);
                }
            } catch (final IOException error) {
                MipavUtil.displayError("ViewJFrameTriImage: IOException error on exportData");
            }

            final float[] imgBuffer2 = new float[imgBuffer.length];
            resols[0] = image.getFileInfo()[0].getResolutions()[0];
            resols[1] = image.getFileInfo()[0].getResolutions()[1];
            resols[2] = image.getFileInfo()[0].getResolutions()[2];
            sliceSize = xDim * yDim;
            tDim = image.getExtents()[3];
            length = xDim * yDim * zDim * tDim;
            mod = length / 100;

            final TransMatrix kTM = AlgorithmTransform.matrixtoInverseArray(xfrm);
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

                            if ( (progressBar != null) && ( (counter % mod) == 0)) {
                                progressBar.updateValue(Math.round((float) counter / (length - 1) * 100), true);
                            }

                            // transform i,j,k
                            valueA = 0; // remains zero if voxel is transformed out of bounds
                            valueR = 0;
                            valueG = 0;
                            valueB = 0;
                            imm = i * resols[0];
                            jmm = j * resols[1];
                            kmm = k * resols[2];

                            X = (imm * T00) + (jmm * T01) + (kmm * T02) + T03;
                            X = X / resols[0];
                            roundX = (int) (X + 0.5f);

                            if ( (X >= 0) && (X < xDim)) {
                                Y = (imm * T10) + (jmm * T11) + (kmm * T12) + T13;
                                Y = Y / resols[1];
                                roundY = (int) (Y + 0.5f);

                                if ( (Y >= 0) && (Y < yDim)) {
                                    Z = (imm * T20) + (jmm * T21) + (kmm * T22) + T23;
                                    Z = Z / resols[2];
                                    roundZ = (int) (Z + 0.5f);

                                    if ( (Z >= 0) && (Z < zDim)) {

                                        if ( (X >= (xDim - 1)) || (Y >= (yDim - 1)) || (Z >= (zDim - 1))) { // cannot
                                                                                                            // interpolate
                                                                                                            // on last
                                                                                                            // X, Y, or
                                                                                                            // Z
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

                                            valueA = (w000 * imgBuffer[index000]) + (w001 * imgBuffer[index001])
                                                    + (w010 * imgBuffer[index010]) + (w011 * imgBuffer[index011])
                                                    + (w100 * imgBuffer[index100]) + (w101 * imgBuffer[index101])
                                                    + (w110 * imgBuffer[index110]) + (w111 * imgBuffer[index111]);
                                            valueR = (w000 * imgBuffer[index000 + 1])
                                                    + (w001 * imgBuffer[index001 + 1])
                                                    + (w010 * imgBuffer[index010 + 1])
                                                    + (w011 * imgBuffer[index011 + 1])
                                                    + (w100 * imgBuffer[index100 + 1])
                                                    + (w101 * imgBuffer[index101 + 1])
                                                    + (w110 * imgBuffer[index110 + 1])
                                                    + (w111 * imgBuffer[index111 + 1]);
                                            valueG = (w000 * imgBuffer[index000 + 2])
                                                    + (w001 * imgBuffer[index001 + 2])
                                                    + (w010 * imgBuffer[index010 + 2])
                                                    + (w011 * imgBuffer[index011 + 2])
                                                    + (w100 * imgBuffer[index100 + 2])
                                                    + (w101 * imgBuffer[index101 + 2])
                                                    + (w110 * imgBuffer[index110 + 2])
                                                    + (w111 * imgBuffer[index111 + 2]);
                                            valueB = (w000 * imgBuffer[index000 + 3])
                                                    + (w001 * imgBuffer[index001 + 3])
                                                    + (w010 * imgBuffer[index010 + 3])
                                                    + (w011 * imgBuffer[index011 + 3])
                                                    + (w100 * imgBuffer[index100 + 3])
                                                    + (w101 * imgBuffer[index101 + 3])
                                                    + (w110 * imgBuffer[index110 + 3])
                                                    + (w111 * imgBuffer[index111 + 3]);
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
                } catch (final IOException error) {
                    MipavUtil.displayError("ViewJFrameTriImage: IOException Error on importData");
                }

                if (l < (tDim - 1)) {

                    try {

                        if (resultImage != null) {
                            resultImage.exportData( (l + 1) * imgBuffer.length, imgBuffer.length, imgBuffer);
                        } else {
                            image.exportData( (l + 1) * imgBuffer.length, imgBuffer.length, imgBuffer);
                        }
                    } catch (final IOException error) {
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
        } finally {}
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
     * @param srcImage DOCUMENT ME!
     * @param transMatrix array with transformation matrix
     * @param dxOut DOCUMENT ME!
     * @param dyOut DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    private int[] getImageMargins(final ModelImage srcImage, final TransMatrix transMatrix, final float dxOut,
            final float dyOut) {
        int i;
        final float xi = 0.f;
        final float yi = 0.f;
        final float dx = srcImage.getFileInfo(0).getResolutions()[0];
        final float dy = srcImage.getFileInfo(0).getResolutions()[1];
        final int nx = srcImage.getExtents()[0];
        final int ny = srcImage.getExtents()[1];
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

        /*
         * Set up array of 4 points representing the corners of the image volume and then transform them with
         * transMatrix.
         */
        ptsi2 = new Vector2f[4];
        ptsf2 = new Vector2f[4];

        for (i = 1; i <= 4; i++) {
            ptsi2[i - 1] = new Vector2f();
            ptsf2[i - 1] = new Vector2f();

            if ( (i == 1) || (i == 4)) {
                ptsi2[i - 1].X = xi;
            } else {
                ptsi2[i - 1].X = xf;
            }

            if ( (i == 1) || (i == 2)) {
                ptsi2[i - 1].Y = yi;
            } else {
                ptsi2[i - 1].Y = yf;
            }

            // System.out.println("Initial point " +i +": " +(int)ptsi2[i-1].X +", " +(int)ptsi2[i-1].Y);
        }

        /* Transform corner points, ptsi2, to get transformed points, ptsf2. */
        transMatrix.transformAsPoint2Df(ptsi2, ptsf2);

        /* Find new min and max values for the transformed point. */
        for (i = 1; i <= 4; i++) {

            // System.out.println("Transformed point " +i +": " +(int)ptsf2[i-1].X +", " +(int)ptsf2[i-1].Y);
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
        leftPad = (int) ( ( (xi - minx) / dxOut) + 0.5);
        rightPad = (int) ( ( (maxx - xf) / dxOut) + 0.5);

        // System.out.println("Padding in x is: " + leftPad +" and " +rightPad);
        topPad = (int) ( ( (yi - miny) / dyOut) + 0.5);
        bottomPad = (int) ( ( (maxy - yf) / dyOut) + 0.5);
        // System.out.println("Padding in y is: " + topPad+" and " +bottomPad);

        AlgorithmTransform.margins[0] = leftPad;
        AlgorithmTransform.margins[1] = topPad;
        AlgorithmTransform.margins[2] = rightPad;
        AlgorithmTransform.margins[3] = bottomPad;

        return AlgorithmTransform.margins;
    }

    /**
     * Returns transformed volume.
     * 
     * @return destImage
     */
    public ModelImage getTransformedImage() {
        return destImage;
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {
        TransMatrix newMatrix;
        // TransMatrix newTMatrix;

        if (AlgorithmTransform.updateOrigin) {
            //AlgorithmTransform.updateOrigin(this.transMatrix);
            updateOrigin();
        }

        // System.err.println("MATRIX: " + transMatrix);

        // BEN: fix this so the origin is updated correctly
        AlgorithmTransform
                .updateFileInfo(srcImage, destImage, destResolutions, oUnits, this.transMatrix, isSATransform, null);

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
                    newMatrix.mult(transMatrix);
                } else { // 2.5D processing
                    newMatrix = new TransMatrix(srcImage.getMatrix());

                    final TransMatrix mat3D = new TransMatrix(4);
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
                    newMatrix.mult(mat3D);
                }

            } else { // srcImage.getNDims() == 2
                newMatrix = new TransMatrix(3);

                // There is the posibility the for 2D DICOM that the matrix might be 4x4
                // If 3 x3 OK to load else the newMatrix is identity
                if (srcImage.getMatrix().getDim() == 3) {
                    newMatrix.Copy(srcImage.getMatrix());
                }

                newMatrix.mult(transMatrix);
            }

            // System.err.println("NEW MATRIX: " + newTMatrix);

            // replace the destination image's default (composite) matrix
            // newTMatrix.setTransformID(TransMatrix.TRANSFORM_COMPOSITE);
            // destImage.setMatrix(newTMatrix);

        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @param center DOCUMENT ME!
     */
    public void setCenter(final Vector3f center) {
        this.center = center;
        if (pad) {

        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @param doCenter DOCUMENT ME!
     */
    public void setDoCenter(final boolean doCenter) {
        this.doCenter = doCenter;
    }

    /**
     * Set value for out of bounds transformation values. Set in constructors to a default of srcImage.getMin().
     * 
     * @param fillValue
     */
    public void setFillValue(final float fillValue) {
        this.fillValue = fillValue;
    }

    /**
     * Set value for out of bounds transformation values. Set in constructors to a default of srcImage.getMin().
     * 
     * @param padValue
     * @deprecated The value is not just used for padding. Please use setFillValue().
     */
    public void setPadValue(final float padValue) {
        setFillValue(padValue);
    }

    /**
     * Sets the origin flag used indicated that origin should be changed based using the supplied transformation matrix.
     * 
     * @param originFlag if true sets the updateOrigin flag to true
     */
    public void setUpdateOriginFlag(final boolean originFlag) {
        AlgorithmTransform.updateOrigin = originFlag;
    }

    /**
     * Sets the tranform to set orientation to AXIAL (this is a scanner anatomical transform).
     * 
     * @param useSA set to axial orientation
     */
    public void setUseScannerAnatomical(final boolean useSA) {
        this.isSATransform = useSA;
    }

    /**
     * Copy important file information to resultant image structure.
     * 
     * @param image Source image.
     * @param resultImage Resultant image.
     * @param resolutions DOCUMENT ME!
     * @param units DOCUMENT ME!
     * @param matrix DOCUMENT ME!
     * @param useSATransform DOCUMENT ME!
     * @param m 
     */
    private static void updateFileInfo(final ModelImage image, final ModelImage resultImage, final float[] resolutions,
            final int[] units, final TransMatrix matrix, final boolean useSATransform, AlgorithmBase srcAlg) {

    	final FileInfoBase[] fileInfo = resultImage.getFileInfo();
    	float firstPos[] = null;
    	float delPos[] = null;
    	float sliceLocation0 = Float.NaN;
    	float delLoc = Float.NaN;

        if (resultImage.getNDims() == 2) {
            fileInfo[0] = (FileInfoBase) image.getFileInfo(0).clone();
            fileInfo[0].setDataType(resultImage.getType());
            fileInfo[0].setModality(image.getFileInfo()[0].getModality());
            fileInfo[0].setFileDirectory(image.getFileInfo()[0].getFileDirectory());
            fileInfo[0].setEndianess(image.getFileInfo()[0].getEndianess());
            fileInfo[0].setUnitsOfMeasure(image.getFileInfo()[0].getUnitsOfMeasure());
            fileInfo[0].setResolutions(resolutions);
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
            final float[] coord = new float[3];
            final float[] tempPos = new float[3];
            String orientation;

            // if the transform was scanner anatomical, set to AXIAL
            if (useSATransform) {
                AlgorithmTransform.imgOrient = FileInfoBase.AXIAL;
                AlgorithmTransform.axisOrient[0] = FileInfoBase.ORI_R2L_TYPE;
                AlgorithmTransform.axisOrient[1] = FileInfoBase.ORI_A2P_TYPE;
                AlgorithmTransform.axisOrient[2] = FileInfoBase.ORI_I2S_TYPE;
            }

            if (image.getFileInfo(0).getFileFormat() == FileUtility.DICOM) {
                final FileInfoDicom oldDicomInfo = (FileInfoDicom) image.getFileInfo(0);
                final FileDicomTagTable[] childTagTables = new FileDicomTagTable[resultImage.getExtents()[2] - 1];

                // first create all of the new file infos (reference and children) and fill them with tags from the old
                // file info. some of these tag values will be overridden in the next loop
                for (int i = 0; i < resultImage.getExtents()[2]; i++) {

                    if (i == 0) {

                        // create a new reference file info
                        fileInfo[0] = new FileInfoDicom(oldDicomInfo.getFileName(), oldDicomInfo.getFileDirectory(),
                                oldDicomInfo.getFileFormat());
                        ((FileInfoDicom) fileInfo[0]).setVr_type(oldDicomInfo.getVr_type()); 
                    } else {

                        // all other slices are children of the first file info..
                        fileInfo[i] = new FileInfoDicom(oldDicomInfo.getFileName(), oldDicomInfo.getFileDirectory(),
                                oldDicomInfo.getFileFormat(), (FileInfoDicom) fileInfo[0]);
                        ((FileInfoDicom) fileInfo[i]).setVr_type(oldDicomInfo.getVr_type()); 
                        childTagTables[i - 1] = ((FileInfoDicom) fileInfo[i]).getTagTable();
                    }

                    if (image.getExtents()[2] > i) {

                        // more correct information for a Z-axis rotation, so copy the file info on a slice basis
                        ((FileInfoDicom) fileInfo[i]).getTagTable().importTags((FileInfoDicom) image.getFileInfo(i).clone());
                    } else {

                        // not possible for other rotations because the z-dimension is different
                        ((FileInfoDicom) fileInfo[i]).getTagTable().importTags((FileInfoDicom)oldDicomInfo.clone());
                    }
                    ((FileInfoDicom) fileInfo[i]).getTagTable().setValue("0028,0010", new Short((short) resultImage.getExtents()[1]), 2);
                    ((FileInfoDicom) fileInfo[i]).getTagTable().setValue("0028,0011", new Short((short) resultImage.getExtents()[0]), 2);
                    if(srcAlg != null) {
                        srcAlg.fireProgressStateChanged((float).7*(i/((float)resultImage.getExtents()[2])), "Reorient", "Reorient on slice "+i);
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
                    
                    if(srcAlg != null) {
                        srcAlg.fireProgressStateChanged((float).7*(i/((float)resultImage.getExtents()[2])), "Reorient", "Reorient on slice "+i);
                    }
                }
            }
          
            if ((image.getFileInfo(0).getFileFormat() == FileUtility.DICOM) &&
            	(image.getExtents()[2] != resultImage.getExtents()[2])) {
            	
            	float lastPos[] = null;
                orientation = (String) ((FileInfoDicom) fileInfo[0]).getTagTable().getValue("0020,0032");
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
                        firstPos = new float[3];

                        matrix.transform(coord[0], coord[1], coord[2], firstPos);
                } // if (orientation != null)
                orientation = (String) ((FileInfoDicom) fileInfo[resultImage.getExtents()[2]-1]).getTagTable().getValue("0020,0032");
                
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
                    
                    lastPos = new float[3];

                    matrix.transform(coord[0], coord[1], coord[2], lastPos);
                } // if (orientation != null)
                if ((firstPos != null) && (lastPos != null)) {
                    delPos = new float[3];
                    for (int i = 0; i <= 2; i++) {
                    	delPos[i] = (lastPos[i] - firstPos[i])/(resultImage.getExtents()[2] - 1);
                    }
                } // if ((firstPos != null) && (lastPos != null)
                if ((((FileInfoDicom)fileInfo[0]).getTagTable().containsTag("0020,1041")) &&
                		(((FileInfoDicom)fileInfo[1]).getTagTable().containsTag("0020,1041")))	{
                	if (((String)((FileInfoDicom)fileInfo[0]).getTagTable().getValue("0020,1041") != null) &&
                			((String)((FileInfoDicom)fileInfo[1]).getTagTable().getValue("0020,1041") != null)) {
                		try {
                		    sliceLocation0 = Float.parseFloat((String)((FileInfoDicom)fileInfo[0]).getTagTable().getValue("0020,1041"));
                		    float sliceLocation1 = Float.parseFloat((String)((FileInfoDicom)fileInfo[1]).getTagTable().getValue("0020,1041"));
                		    delLoc = (sliceLocation1 - sliceLocation0) * resolutions[2]/image.getFileInfo()[0].getResolutions()[2];
                		}
                		catch (NumberFormatException nfe) {
                			
                		}
                	}
                	
                }
            } // if ((image.getFileInfo(0).getFileFormat() == FileUtility.DICOM) &&
            
            
            for (int i = 0; i < resultImage.getExtents()[2]; i++) {

                fileInfo[i].setDataType(resultImage.getType());
                fileInfo[i].setResolutions(resolutions);
                fileInfo[i].setSliceThickness(resolutions[2]);
                fileInfo[i].setExtents(resultImage.getExtents());
                fileInfo[i].setMax(resultImage.getMax());
                fileInfo[i].setMin(resultImage.getMin());
                fileInfo[i].setImageOrientation(AlgorithmTransform.imgOrient);
                fileInfo[i].setAxisOrientation(AlgorithmTransform.axisOrient);
                fileInfo[i].setUnitsOfMeasure(units);

                fileInfo[i].setOrigin(AlgorithmTransform.imgOrigin);
                AlgorithmTransform.imgOrigin[2] = AlgorithmTransform.startPos
                        + (AlgorithmTransform.direct[2] * i * resolutions[2]);
                

                if (fileInfo[i].getFileFormat() == FileUtility.DICOM) {
                	if (image.getExtents()[2] == resultImage.getExtents()[2]) {
                		// don't interpolate here in case spacing between slices is uneven
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
                        } // if (orientation != null)
	                    
                    } // if (image.getExtents()[2] == resultImage.getExtents()[2])
                	else { // image.getExtents()[2] != resultImage.getExtents()[2]
                        if (delPos != null) {
                        	orientation = (firstPos[0] + i * delPos[0]) + "\\" + (firstPos[1]+ i * delPos[1]) + "\\" +
                        	              (firstPos[2] + i * delPos[2]);
                        	((FileInfoDicom) fileInfo[i]).getTagTable().setValue("0020,0032", orientation);
                        } // if (delPos != null)
                        if (!Float.isNaN(delLoc)) {
                        	String sliceLoc = Float.toString(sliceLocation0 + i * delLoc);
                        	// slice location
                        	((FileInfoDicom)fileInfo[i]).getTagTable().setValue("0020,1041", sliceLoc, sliceLoc.length()); 
                        }
                        String instanceString = Integer.toString(i+1);
                        ((FileInfoDicom)fileInfo[i]).getTagTable().setValue("0020,0013", instanceString, instanceString.length());
                        String imagesInAcquisition = Integer.toString(resultImage.getExtents()[2]);
                        ((FileInfoDicom)fileInfo[i]).getTagTable().setValue("0020,1002", imagesInAcquisition, imagesInAcquisition.length());
                        String res2 = String.valueOf(resolutions[2]);
                        if (((FileInfoDicom)fileInfo[i]).getTagTable().containsTag("0018,0050")) {
                        	// Slice thickness
                        	((FileInfoDicom)fileInfo[i]).getTagTable().setValue("0018,0050", res2, res2.length());    	
                        }
                        if (((FileInfoDicom)fileInfo[i]).getTagTable().containsTag("0018,0088")) {
                        	// Spacing  between slices
                        	((FileInfoDicom)fileInfo[i]).getTagTable().setValue("0018,0088", res2, res2.length());    	
                        }
                    } // else image.getExtents()[2] != resultImage.getExtents()[2]
                } // if (fileInfo[i].getFileFormat() == FileUtility.DICOM)
            } // for (int i = 0; i < resultImage.getExtents()[2]; i++)
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
                fileInfo[i].setImageOrientation(AlgorithmTransform.imgOrient);
                fileInfo[i].setAxisOrientation(AlgorithmTransform.axisOrient);
                AlgorithmTransform.imgOrigin[2] = AlgorithmTransform.startPos
                        + (AlgorithmTransform.direct[2] * (i % resultImage.getExtents()[2]) * resolutions[2]);
                AlgorithmTransform.imgOrigin[3] = AlgorithmTransform.startTime + (i / resultImage.getExtents()[2])
                        * resolutions[3];
                fileInfo[i].setOrigin(AlgorithmTransform.imgOrigin);
                fileInfo[i].setPixelPadValue(image.getFileInfo()[0].getPixelPadValue());
                fileInfo[i].setPhotometric(image.getFileInfo()[0].getPhotometric());
            }
        }

        resultImage.setFileInfo(fileInfo);
    }

    /**
     * Update origin.
     * Translate the image origin from image space to patient space (scanner space).
     * The basic ideas,  tranlate the (0,0,0) point from scaner space to image space,
     * then subtract the original image origin. 
     * The (0, 0, 0) point is the upper left corner of the scanner space's image.   
     * @param xfrm DOCUMENT ME!
     */
    
    private static void updateOrigin(final TransMatrix xfrm) {

		if (xfrm.getDim() == 3) {
			xfrm.transform(imgOrigin[0], imgOrigin[1], imgOrigin);
		} else {
			xfrm.transform(imgOrigin[0], imgOrigin[1], imgOrigin[2], imgOrigin);
		}

	}
    
    
    /**
	 * Translate the image origin from image space to patient space (scanner
	 * space). The basic ideas, tranlate the (0,0,0) point from scaner space to
	 * image space, then multiply the resolution, and subtract the image origin. The (0, 0, 0) point
	 * is the upper left corner of the scanner space's image.
	 */
    private void updateOrigin() {

        // Remmember the the interpolation routines going from the output to the input image
        // use the inverse matrix, but that here we wish to take the input origin to the 
        // output origin, so we do not take the inverse.
        TransMatrix trans;
        TransMatrix xfrmC;
       
        if ( (DIM >= 3) && ( !do25D)) {
            imgLength = iXdim * iYdim * iZdim;
        } else {
            imgLength = iXdim * iYdim;
        }

        try {

            if ( (do25D) || (DIM == 2)) {
                trans = new TransMatrix(3);
            } else { // (DIM >= 3) && (!do25D)
                trans = new TransMatrix(4);
            }

            trans.Copy(transMatrix);

            if ( (doCenter) && ( !haveCentered)) {

                if ( (do25D) || (DIM == 2)) {
                    xfrmC = new TransMatrix(3);
                } else { // (DIM >= 3) && (!do25D)
                    xfrmC = new TransMatrix(4);
                }

                // by default: xfrmC.identity();

                if ( (DIM >= 3) && ( !do25D)) {
                    xfrmC.setTranslate(center.X, center.Y, center.Z);
                } else { // (DIM == 2) || do25D
                    xfrmC.setTranslate(center.X, center.Y);
                }

                trans.Copy(xfrmC);
                trans.mult(transMatrix);

                if ( (DIM >= 3) && ( !do25D)) {
                    trans.setTranslate( -center.X, -center.Y, -center.Z);
                } else { // (DIM == 2) || do25D
                    trans.setTranslate( -center.X, -center.Y);
                }
            } // if ((doCenter) && (!haveCentered))
           
            if (trans.getDim() == 3) {
                trans.transform(imgOrigin[0], imgOrigin[1], imgOrigin);
            } else {
                //System.err.println("Before: AlgorithmTransform.imgOrigin[0] = " + AlgorithmTransform.imgOrigin[0] +
                //		"AlgorithmTransform.imgOrigin[1] = " + AlgorithmTransform.imgOrigin[1] +
                //		"AlgorithmTransform.imgOrigin[2] = " + AlgorithmTransform.imgOrigin[2]);
                trans.transform(imgOrigin[0], imgOrigin[1], imgOrigin[2], imgOrigin);
                
                //System.err.println("After: AlgorithmTransform.imgOrigin[0] = " + AlgorithmTransform.imgOrigin[0] +
                //		"AlgorithmTransform.imgOrigin[1] = " + AlgorithmTransform.imgOrigin[1] +
                //		"AlgorithmTransform.imgOrigin[2] = " + AlgorithmTransform.imgOrigin[2]);
            }
        } catch (final OutOfMemoryError e) {
            disposeLocal();
            System.gc();
            displayError("Algorithm Transform: ZZZ. Out of memory on srcImage.exportData");
            setCompleted(false);

            return;
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
        byte byteBuf[] = null;
        byte byteBuf2[] = null;

        if ( (DIM >= 3) && ( !do25D)) {
            imgLength = iXdim * iYdim * iZdim;
        } else {
            imgLength = iXdim * iYdim;
        }

        try {

            if ( (do25D) || (DIM == 2)) {
                trans = new TransMatrix(3);
            } else { // (DIM >= 3) && (!do25D)
                trans = new TransMatrix(4);
            }

            trans.Copy(transMatrix);

            if ( (doCenter) && ( !haveCentered)) {

                if ( (do25D) || (DIM == 2)) {
                    xfrmC = new TransMatrix(3);
                } else { // (DIM >= 3) && (!do25D)
                    xfrmC = new TransMatrix(4);
                }

                // by default: xfrmC.identity();

                if ( (DIM >= 3) && ( !do25D)) {
                    xfrmC.setTranslate(center.X, center.Y, center.Z);
                } else { // (DIM == 2) || do25D
                    xfrmC.setTranslate(center.X, center.Y);
                }

                trans.Copy(xfrmC);
                trans.mult(transMatrix);

                if ( (DIM >= 3) && ( !do25D)) {
                    trans.setTranslate( -center.X, -center.Y, -center.Z);
                } else { // (DIM == 2) || do25D
                    trans.setTranslate( -center.X, -center.Y);
                }
            } // if ((doCenter) && (!haveCentered))

            xfrm = AlgorithmTransform.matrixtoInverseArray(trans);

            bufferFactor = 1;

            if (srcImage.isColorImage()) {
                bufferFactor = 4;
                imgLength = imgLength * 4;
            }

            if ( (srcImage.getType() == ModelStorageBase.ARGB) && (interp == AlgorithmTransform.TRILINEAR)) {
                // Reduce needed memory by a factor of 4
                byteBuf = new byte[imgLength];
                srcImage.exportData(0, imgLength, byteBuf);
            } else {
                imgBuf = new double[imgLength];
                srcImage.exportData(0, imgLength, imgBuf);
            }

            if (bufferFactor == 4) {

                if ( (DIM >= 3) && ( !do25D)) {
                    if (srcImage.getType() == ModelStorageBase.ARGB) {
                        // Reduce needed memory by a factor of 4
                        byteBuf2 = new byte[4 * oXdim * oYdim * oZdim];
                    } else {
                        imgBuf2 = new float[4 * oXdim * oYdim * oZdim];
                    }
                } else {
                    imgBuf2 = new float[4 * oXdim * oYdim];
                }
            }

            fireProgressStateChanged(srcImage.getImageName(), "Transforming image ...");

        } catch (final IOException error) {
            displayError("Algorithm Transform: IOException on srcImage.exportData");
            setCompleted(false);

            disposeLocal();

            return;
        } catch (final OutOfMemoryError e) {
            disposeLocal();
            System.gc();
            displayError("Algorithm Transform: ZZZ. Out of memory on srcImage.exportData");
            setCompleted(false);

            return;
        }

        if (bufferFactor == 1) { // black and white

            if ( (do25D) && (DIM == 4)) {

                if (interp == AlgorithmTransform.NEAREST_NEIGHBOR) {
                    transformNearestNeighbor4Dim2D(imgBuf, xfrm);
                } else if (interp == AlgorithmTransform.BILINEAR) {
                    transformBilinear4D(imgBuf, xfrm);
                } else if (interp == AlgorithmTransform.BSPLINE3) {
                    transformAlgorithmBspline2D(imgBuf, xfrm, 3);
                } else if (interp == AlgorithmTransform.BSPLINE4) {
                	transformAlgorithmBspline2D(imgBuf, xfrm, 4);
                } else if (interp == AlgorithmTransform.CUBIC_LAGRANGIAN) {
                    transformCubicLagrangian4Dim2D(imgBuf, xfrm, clip);
                } else if (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) {
                    transformQuinticLagrangian4Dim2D(imgBuf, xfrm, clip);
                } else if (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN) {
                    transformHepticLagrangian4Dim2D(imgBuf, xfrm, clip);
                } else if (interp == AlgorithmTransform.WSINC) {
                    transformWSinc4Dim2D(imgBuf, xfrm, clip);
                }
            } else if ( (do25D) && (DIM == 3)) {

                if (interp == AlgorithmTransform.NEAREST_NEIGHBOR) {
                    transformNearestNeighbor3Dim2D(imgBuf, xfrm);
                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.BILINEAR) {
                    transformBilinear3D(imgBuf, xfrm);
                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.BSPLINE3) {
                	transformAlgorithmBspline2D(imgBuf, xfrm, 3);
                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.BSPLINE4) {
                	transformAlgorithmBspline2D(imgBuf, xfrm, 4);
                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.CUBIC_LAGRANGIAN) {
                    transformCubicLagrangian3Dim2D(imgBuf, xfrm, clip);
                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) {
                    transformQuinticLagrangian3Dim2D(imgBuf, xfrm, clip);
                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN) {
                    transformHepticLagrangian3Dim2D(imgBuf, xfrm, clip);
                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.WSINC) {
                    transformWSinc3Dim2D(imgBuf, xfrm, clip);
                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                }
            } else if (DIM == 4) {

                if (interp == AlgorithmTransform.TRILINEAR) {
                    transformTrilinear4D(imgBuf, xfrm);
                } else if (interp == AlgorithmTransform.BSPLINE3) {
                    transformAlgorithmBspline4D(imgBuf, xfrm, 3);
                } else if (interp == AlgorithmTransform.BSPLINE4) {
                    transformAlgorithmBspline4D(imgBuf, xfrm, 4);
                } else if (interp == AlgorithmTransform.NEAREST_NEIGHBOR) {
                    transformNearestNeighbor4D(imgBuf, xfrm);
                } else if (interp == AlgorithmTransform.CUBIC_LAGRANGIAN) {
                    transformCubicLagrangian4D(imgBuf, xfrm, clip);
                } else if (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) {
                    transformQuinticLagrangian4D(imgBuf, xfrm, clip);
                } else if (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN) {
                    transformHepticLagrangian4D(imgBuf, xfrm, clip);
                } else if (interp == AlgorithmTransform.WSINC) {
                    transformWSinc4D(imgBuf, xfrm, clip);
                } else if (interp == AlgorithmTransform.BILINEAR) {
                    displayError("Cannot select bilinear interpolation for 4D");

                    return;
                }
            } else if (DIM == 3) {

                if (interp == AlgorithmTransform.TRILINEAR) {
                    transformTrilinear(imgBuf, xfrm);

                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform3DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.BSPLINE3) {
                    transformAlgorithmBspline3D(imgBuf, xfrm, 3);

                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform3DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.BSPLINE4) {
                	transformAlgorithmBspline3D(imgBuf, xfrm, 4);

                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform3DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.NEAREST_NEIGHBOR) {
                    transformNearestNeighbor3D(imgBuf, xfrm);

                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform3DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.CUBIC_LAGRANGIAN) {
                    transformCubicLagrangian3D(imgBuf, xfrm, clip);

                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform3DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) {
                    transformQuinticLagrangian3D(imgBuf, xfrm, clip);

                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform3DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN) {
                    transformHepticLagrangian3D(imgBuf, xfrm, clip);

                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform3DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.WSINC) {
                    transformWSinc3D(imgBuf, xfrm, clip);

                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform3DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.BILINEAR) {
                    displayError("Cannot select bilinear interpolation for 3D");

                    return;
                }
            } else if (DIM == 2) {

                if (interp == AlgorithmTransform.BILINEAR) {
                    transformBilinear(imgBuf, xfrm);

                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.NEAREST_NEIGHBOR) {
                    transformNearestNeighbor2D(imgBuf, xfrm);

                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.BSPLINE3) {
                	transformAlgorithmBspline2D(imgBuf, xfrm, 3);

                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.BSPLINE4) {
                	transformAlgorithmBspline2D(imgBuf, xfrm, 4);

                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.CUBIC_LAGRANGIAN) {
                    transformCubicLagrangian2D(imgBuf, xfrm, clip);

                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) {
                    transformQuinticLagrangian2D(imgBuf, xfrm, clip);

                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN) {
                    transformHepticLagrangian2D(imgBuf, xfrm, clip);

                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.WSINC) {
                    transformWSinc2D(imgBuf, xfrm, clip);

                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.TRILINEAR) {
                    displayError("Cannot specify trilinear interpolation for 2D");

                    return;
                }
            }
        } // end of if (bufferFactor == 1)
        else { // color with bufferFactor == 4

            if ( (do25D) && (DIM == 4)) {

                if (interp == AlgorithmTransform.NEAREST_NEIGHBOR) {
                    transformNearestNeighbor4Dim2DC(imgBuf, imgBuf2, xfrm);
                } else if (interp == AlgorithmTransform.BILINEAR) {
                    transformBilinear4DC(imgBuf, imgBuf2, xfrm);
                } else if (interp == AlgorithmTransform.BSPLINE3) {
                    transformAlgorithmBspline2DC(imgBuf, xfrm, 3);
                } else if (interp == AlgorithmTransform.BSPLINE4) {
                	transformAlgorithmBspline2DC(imgBuf, xfrm, 4);
                } else if (interp == AlgorithmTransform.CUBIC_LAGRANGIAN) {
                    transformCubicLagrangian4Dim2DC(imgBuf, imgBuf2, xfrm, clip);
                } else if (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) {
                    transformQuinticLagrangian4Dim2DC(imgBuf, imgBuf2, xfrm, clip);
                } else if (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN) {
                    transformHepticLagrangian4Dim2DC(imgBuf, imgBuf2, xfrm, clip);
                } else if (interp == AlgorithmTransform.WSINC) {
                    transformWSinc4Dim2DC(imgBuf, imgBuf2, xfrm, clip);
                }
            } else if ( (do25D) && (DIM == 3)) {

                if (interp == AlgorithmTransform.NEAREST_NEIGHBOR) {
                    transformNearestNeighbor3Dim2DC(imgBuf, imgBuf2, xfrm);
                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                }

                else if (interp == AlgorithmTransform.BILINEAR) {
                    transformBilinear3DC(imgBuf, imgBuf2, xfrm);
                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.BSPLINE3) {
                	transformAlgorithmBspline2DC(imgBuf, xfrm, 3);
                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.BSPLINE4) {
                	transformAlgorithmBspline2DC(imgBuf, xfrm, 4);
                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.CUBIC_LAGRANGIAN) {
                    transformCubicLagrangian3Dim2DC(imgBuf, imgBuf2, xfrm, clip);
                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) {
                    transformQuinticLagrangian3Dim2DC(imgBuf, imgBuf2, xfrm, clip);
                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN) {
                    transformHepticLagrangian3Dim2DC(imgBuf, imgBuf2, xfrm, clip);
                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.WSINC) {
                    transformWSinc3Dim2DC(imgBuf, imgBuf2, xfrm, clip);
                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform25DVOI(srcImage, imgBuf, xfrm);
                    }
                }
            } else if (DIM == 4) {
                if (interp == AlgorithmTransform.TRILINEAR) {
                    if (srcImage.getType() == ModelStorageBase.ARGB) {
                        // Save a factor of 4 in memory
                        transformTrilinear4DByteC(byteBuf, byteBuf2, xfrm);
                    } else {
                        transformTrilinear4DC(imgBuf, imgBuf2, xfrm);
                    }
                } else if (interp == AlgorithmTransform.BSPLINE3) {
                    transformAlgorithmBspline4DC(imgBuf, xfrm, 3);
                } else if (interp == AlgorithmTransform.BSPLINE4) {
                    transformAlgorithmBspline4DC(imgBuf, xfrm, 4);
                } else if (interp == AlgorithmTransform.NEAREST_NEIGHBOR) {
                    transformNearestNeighbor4DC(imgBuf, imgBuf2, xfrm);
                } else if (interp == AlgorithmTransform.CUBIC_LAGRANGIAN) {
                    transformCubicLagrangian4DC(imgBuf, imgBuf2, xfrm, clip);
                } else if (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) {
                    transformQuinticLagrangian4DC(imgBuf, imgBuf2, xfrm, clip);
                } else if (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN) {
                    transformHepticLagrangian4DC(imgBuf, imgBuf2, xfrm, clip);
                } else if (interp == AlgorithmTransform.WSINC) {
                    transformWSinc4DC(imgBuf, imgBuf2, xfrm, clip);
                } else if (interp == AlgorithmTransform.BILINEAR) {
                    displayError("Cannot specify bilinear interpolation for 4D");

                    return;
                }
            } else if (DIM == 3) {

                if (interp == AlgorithmTransform.TRILINEAR) {
                    if (srcImage.getType() == ModelStorageBase.ARGB) {
                        // Save a factor of 4 in memory
                        transformTrilinearByteC(byteBuf, byteBuf2, xfrm);

                        if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                            transform3DVOIByte(srcImage, byteBuf, xfrm);
                        }
                    } else {
                        transformTrilinearC(imgBuf, imgBuf2, xfrm);

                        if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                            transform3DVOI(srcImage, imgBuf, xfrm);
                        }
                    }
                } else if (interp == AlgorithmTransform.BSPLINE3) {
                    transformAlgorithmBspline3DC(imgBuf, xfrm, 3);

                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform3DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.BSPLINE4) {
                	transformAlgorithmBspline3DC(imgBuf, xfrm, 4);

                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform3DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.NEAREST_NEIGHBOR) {
                    transformNearestNeighbor3DC(imgBuf, imgBuf2, xfrm);

                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform3DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.CUBIC_LAGRANGIAN) {
                    transformCubicLagrangian3DC(imgBuf, imgBuf2, xfrm, clip);

                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform3DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) {
                    transformQuinticLagrangian3DC(imgBuf, imgBuf2, xfrm, clip);

                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform3DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN) {
                    transformHepticLagrangian3DC(imgBuf, imgBuf2, xfrm, clip);

                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform3DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.WSINC) {
                    transformWSinc3DC(imgBuf, imgBuf2, xfrm, clip);

                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform3DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.BILINEAR) {
                    displayError("Cannot specify bilinear interpolation for 3D");

                    return;
                }
            } else if (DIM == 2) {

                if (interp == AlgorithmTransform.BILINEAR) {
                    transformBilinearC(imgBuf, imgBuf2, xfrm);

                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.NEAREST_NEIGHBOR) {
                    transformNearestNeighbor2DC(imgBuf, imgBuf2, xfrm);

                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.BSPLINE3) {
                	transformAlgorithmBspline2DC(imgBuf, xfrm, 3);

                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.BSPLINE4) {
                	transformAlgorithmBspline2DC(imgBuf, xfrm, 4);

                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.CUBIC_LAGRANGIAN) {
                    transformCubicLagrangian2DC(imgBuf, imgBuf2, xfrm, clip);

                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.QUINTIC_LAGRANGIAN) {
                    transformQuinticLagrangian2DC(imgBuf, imgBuf2, xfrm, clip);

                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.HEPTIC_LAGRANGIAN) {
                    transformHepticLagrangian2DC(imgBuf, imgBuf2, xfrm, clip);

                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.WSINC) {
                    transformWSinc2DC(imgBuf, imgBuf2, xfrm, clip);

                    if ( (transformVOI == true) && (srcImage.getVOIs().size() != 0)) {
                        transform2DVOI(srcImage, imgBuf, xfrm);
                    }
                } else if (interp == AlgorithmTransform.TRILINEAR) {
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
     * For each VOI in VOIVector:
     * <li>Export VOIs as a mask image</li>
     * <li>Transform mask</li>
     * <li>Extract VOI contours from mask image and put in new image.</li>
     * </ol>
     * 
     * @param image Image where VOIs are stored
     * @param imgBuffer Image array
     * @param xfrm Transformation matrix to be applied
     */
    private void transform2DVOI(final ModelImage image, final double[] imgBuffer, final TransMatrix kTM) {

        int i, j;
        int iAdj, jAdj;
        int X0pos, Y0pos;
        double X, Y;
        double temp1, temp2;
        double value;
        double imm, jmm;
        int roundX, roundY;
        int index;
        int index2;
        int indexC;
        int length = iXdim * iYdim;
        int index2Size;

        double T00, T01, T02, T10, T11, T12;
        ModelImage tmpMask;
        VOIVector voiVector;

        final int mod = Math.max(1, oXdim / 50);

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;
        
        voiVector = image.getVOIs();
        
        if (voiVector.size() == 0) {
            return;
        }

        indexC = -1;
        try {
            maskImage = new ModelImage(ModelStorageBase.SHORT, image.getExtents(), "Short Image");
            tmpMask = new ModelImage(ModelStorageBase.SHORT, destImage.getExtents(), null);
        } catch (final OutOfMemoryError error) {
            throw error;
        }
        for (index = 0; index < voiVector.size(); index++) {
        	VOI presentVOI = voiVector.elementAt(index);
        	if (presentVOI.getCurveType() == VOI.CONTOUR) {
        		VOIBaseVector curves = presentVOI.getCurves();	
        		index2Size = curves.size();
        	}
        	else {
        		index2Size = 1;
        	}
        	for (index2 = 0; index2 < index2Size; index2++) {
        		indexC++;
		        
		        maskImage.clearMask();
		        
		        (voiVector.elementAt(index)).createOneElementBinaryMask3D(maskImage.getMask(), iXdim, iYdim, false, false, index2);

				BitSet mask = maskImage.getMask();

				
				for (i = 0; i < length; i++) {

					if (mask.get(i)) {
						maskImage.set(i, indexC + 1);
					}
					else {
						maskImage.set(i, 0);
					}
				}
		        
		
		        try {
		            maskImage.exportData(0, length, imgBuffer); // locks and releases lock
		            
		        } catch (final IOException error) {
		            displayError("Algorithm VOI transform: Image(s) locked");
		            setCompleted(false);
		
		            return;
		        }
		
		        for (i = 0; (i < oXdim) && !threadStopped; i++) {
		
		            if ( ( (i % mod) == 0)) {
		                fireProgressStateChanged((int) ( ((float) i / oXdim * 100) + .5));
		            }
		
		            if (pad) {
		                iAdj = i - AlgorithmTransform.margins[0];
		            } else {
		                iAdj = i;
		            }
		
		            imm = iAdj * oXres;
		            temp1 = (imm * T00) + T02;
		            temp2 = (imm * T10) + T12;
		
		            for (j = 0; (j < oYdim) && !threadStopped; j++) {
		
		                // transform i,j
		                if (pad) {
		                    jAdj = j - AlgorithmTransform.margins[1];
		                } else {
		                    jAdj = j;
		                }
		
		                jmm = jAdj * oYres;
		                value = fillValue; // if transformed out of bounds.
		                X = (temp1 + (jmm * T01)) / iXres;
		                roundX = (int) (X + 0.5);
		
		                if ( (X >= -0.5) && (X < iXdim)) {
		                    Y = (temp2 + (jmm * T11)) / iYres;
		                    roundY = (int) (Y + 0.5);
		
		                    if ( (Y >= -0.5) && (Y < iYdim)) {
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
		        VOIExtAlgo.finalize();
		        VOIExtAlgo = null;
		        destImage.addVOIs(tmpMask.getVOIs());
		        tmpMask.resetVOIs();
		        for (j = 0; j < oYdim; j++) {
	        		for (i = 0; i < oXdim; i++) {
	        			tmpMask.set(i, j, fillValue);
	        		}
	        	}
        	} // for (index2 = 0; index2 < curves.size(); index2++)
        } // for (index = 0; index < voiVector.size(); index++)
        tmpMask.disposeLocal();
        tmpMask = null;
        maskImage.disposeLocal();
        maskImage = null;
       
    }

    /**
     * Transforms and resamples a 3D VOI using nearest neighbor interpolation.
     * 
     * <ol>
     * <li>Export VOIs as a mask image</li>
     * <li>Transform mask</li>
     * <li>Extract VOI contours from mask image and put in new image.</li>
     * </ol>
     * 
     * @param image Image where VOIs are stored
     * @param imgBuffer Image array
     * @param kTM Transformation matrix to be applied
     */
    private void transform25DVOI(final ModelImage image, final double[] imgBuffer, final TransMatrix kTM) {
    	int i, j, z;
        int iAdj, jAdj;
        int X0pos, Y0pos;
        double X, Y;
        double temp1, temp2;
        double value;
        double imm, jmm;
        int roundX, roundY;
        int index;
        int index2;
        int indexC;
        int sliceSize = iXdim * iYdim;
        int index2Size;
        VOIBaseVector curves = null;
        int xBounds[] = new int[2];
        int yBounds[] = new int[2];
        int zBounds[] = new int[2];
        int zFound[] = new int[iZdim];
        boolean duplicateZ = false;

        double T00, T01, T02, T10, T11, T12;
        ModelImage tmpMask = null;
        VOIVector voiVector;

        final int mod = Math.max(1, oXdim / 50);

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;
        
        voiVector = image.getVOIs();
        
        if (voiVector.size() == 0) {
            return;
        }

        indexC = 0;
        try {
            maskImage = new ModelImage(ModelStorageBase.SHORT, image.getExtents(), "Short Image");
            tmpMask = new ModelImage(ModelStorageBase.SHORT, destImage.getExtents(), null);
        } catch (final OutOfMemoryError error) {
            throw error;
        }
        for (index = 0; index < voiVector.size(); index++) {
        	VOI presentVOI = voiVector.elementAt(index);
        	if (presentVOI.getCurveType() == VOI.CONTOUR) {
        		curves = presentVOI.getCurves();	
        		index2Size = curves.size();
        	}
        	else {
        		index2Size = 1;
        	}
            for (i = 0; i < iZdim; i++) {
            	zFound[i] = 0;
            }
        	for (index2 = 0; index2 < index2Size; index2++) {
        		if (presentVOI.getCurveType() == VOI.CONTOUR) {
        		    curves.get(index2).getBounds(xBounds, yBounds, zBounds);	
        		}
        		else {
        			presentVOI.getBounds(xBounds, yBounds, zBounds);
        		}
        		duplicateZ = false;
        		for (i = zBounds[0]; i <= zBounds[1]; i++) {
        			zFound[i]++;
        			if (zFound[i] >= 2) {
        				duplicateZ = true;
        			}
        		}
        		if (duplicateZ) {
        			indexC++;
		        	duplicateZ = false;
		        	for (i = 0; i < iZdim; i++) {
		        		zFound[i] = 0;
		        	}
			        tmpMask.calcMinMax();
			
			        AlgorithmVOIExtraction VOIExtAlgo = new AlgorithmVOIExtraction(tmpMask);
			
			        VOIExtAlgo.setRunningInSeparateThread(runningInSeparateThread);
			        VOIExtAlgo.run();
			        VOIExtAlgo.finalize();
			        VOIExtAlgo = null;
			        destImage.addVOIs(tmpMask.getVOIs());
			        tmpMask.resetVOIs();
			        for (z = 0; z < oZdim; z++) {
			        	for (j = 0; j < oYdim; j++) {
			        		for (i = 0; i < oXdim; i++) {
			        			tmpMask.set(i, j, z, fillValue);
			        		}
			        	}
			        }
			        index2--;
			        continue;
		        }
		        
		        maskImage.clearMask();
		        
		        (voiVector.elementAt(index)).createOneElementBinaryMask3D(maskImage.getMask(), iXdim, iYdim, false, false, index2);

				BitSet mask = maskImage.getMask();

				
				for (i = zBounds[0]*sliceSize; i < (zBounds[1]+1)*sliceSize; i++) {

					if (mask.get(i)) {
						maskImage.set(i, indexC + 1);
					}
					else {
						maskImage.set(i, 0);
					}
				}
		  
		
		        for (z = zBounds[0]; z <= zBounds[1]; z++) {
		            
		            try {
		                maskImage.exportData(z * sliceSize, sliceSize, imgBuffer); // locks and releases lock
		            } catch (final IOException error) {
		                displayError("Algorithm VOI transform: Image(s) locked");
		                setCompleted(false);

		                return;
		            }
		        
		
			        for (i = 0; (i < oXdim) && !threadStopped; i++) {
			
			            if ( ( (i % mod) == 0)) {
			                fireProgressStateChanged((int) ( ((float) i / oXdim * 100) + .5));
			            }
			
			            if (pad) {
			                iAdj = i - AlgorithmTransform.margins[0];
			            } else {
			                iAdj = i;
			            }
			
			            imm = iAdj * oXres;
			            temp1 = (imm * T00) + T02;
			            temp2 = (imm * T10) + T12;
			
			            for (j = 0; (j < oYdim) && !threadStopped; j++) {
			
			                // transform i,j
			                if (pad) {
			                    jAdj = j - AlgorithmTransform.margins[1];
			                } else {
			                    jAdj = j;
			                }
			
			                jmm = jAdj * oYres;
			                value = fillValue; // if transformed out of bounds.
			                X = (temp1 + (jmm * T01)) / iXres;
			                roundX = (int) (X + 0.5);
			
			                if ( (X >= -0.5) && (X < iXdim)) {
			                    Y = (temp2 + (jmm * T11)) / iYres;
			                    roundY = (int) (Y + 0.5);
			
			                    if ( (Y >= -0.5) && (Y < iYdim)) {
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
		        } // for (z = zBounds[0]; z <= zBounds[1]; z++)
		
		        // ******* Make algorithm for VOI extraction.
		        if (index2 == curves.size()-1) {
		        	indexC++;
		        	duplicateZ = false;
		        	for (i = 0; i < iZdim; i++) {
		        		zFound[i] = 0;
		        	}
			        tmpMask.calcMinMax();
			
			        AlgorithmVOIExtraction VOIExtAlgo = new AlgorithmVOIExtraction(tmpMask);
			
			        VOIExtAlgo.setRunningInSeparateThread(runningInSeparateThread);
			        VOIExtAlgo.run();
			        VOIExtAlgo.finalize();
			        VOIExtAlgo = null;
			        destImage.addVOIs(tmpMask.getVOIs());
			        tmpMask.resetVOIs();
			        for (z = 0; z < oZdim; z++) {
			        	for (j = 0; j < oYdim; j++) {
			        		for (i = 0; i < oXdim; i++) {
			        			tmpMask.set(i, j, z, fillValue);
			        		}
			        	}
			        }
		        }
        	} // for (index2 = 0; index2 < curves.size(); index2++)
        } // for (index = 0; index < voiVector.size(); index++)
        maskImage.disposeLocal();
        maskImage = null;
        tmpMask.disposeLocal();
        tmpMask = null;
       
    }

    /**
     * Transforms and resamples a 3D VOI using nearest neighbor interpolation.
     * 
     * <ol>
     * <li>Export VOIs as a mask image</li>
     * <li>Transform mask</li>
     * <li>Extract VOI contours from mask image and put in new image.</li>
     * </ol>
     * 
     * @param image Image where VOIs are stored
     * @param imgBuffer Image array
     * @param kTM Transformation matrix to be applied
     */
    private void transform3DVOI(final ModelImage image, final double[] imgBuffer, final TransMatrix kTM) {
    	int i, j, k, z;
        int iAdj, jAdj, kAdj;
        int X0pos, Y0pos, Z0pos;
        double X, Y, Z;
        double value;
        double imm, jmm, kmm;
        double k1, k2, k3, j1, j2, j3;
        int index;
        int index2;
        int indexC;
        int sliceSize = iXdim * iYdim;
        int length = sliceSize * iZdim;
        int index2Size;
        VOIBaseVector curves = null;

        double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        ModelImage tmpMask = null;
        VOIVector voiVector;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T03 = (double)kTM.M03;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;
        T13 = (double)kTM.M13;
        T20 = (double)kTM.M20;
        T21 = (double)kTM.M21;
        T22 = (double)kTM.M22;
        T23 = (double)kTM.M23;
        
        voiVector = image.getVOIs();
        
        if (voiVector.size() == 0) {
            return;
        }

        indexC = 0;
        final float invXRes = 1 / iXres;
        final float invYRes = 1 / iYres;
        final float invZRes = 1 / iZres;

        try {
            maskImage = new ModelImage(ModelStorageBase.SHORT, image.getExtents(), "Short Image");
            tmpMask = new ModelImage(ModelStorageBase.SHORT, destImage.getExtents(), null);
        } catch (final OutOfMemoryError error) {
            throw error;
        }
        for (z = 0; z < oZdim; z++) {
        	for (j = 0; j < oYdim; j++) {
        		for (i = 0; i < oXdim; i++) {
        			tmpMask.set(i, j, z, fillValue);
        		}
        	}
        }
      
        for (index = 0; index < voiVector.size(); index++) {
        	VOI presentVOI = voiVector.elementAt(index);
        	if (presentVOI.getCurveType() == VOI.CONTOUR) {
        		curves = presentVOI.getCurves();	
        		index2Size = curves.size();
        	}
        	else {
        		index2Size = 1;
        	}
           
        	for (index2 = 0; index2 < index2Size; index2++) {
		        
		        maskImage.clearMask();
		        
		        (voiVector.elementAt(index)).createOneElementBinaryMask3D(maskImage.getMask(), iXdim, iYdim, false, false, index2);

				BitSet mask = maskImage.getMask();

				
				for (i = 0; i < length; i++) {

					if (mask.get(i)) {
						maskImage.set(i, indexC + 1);
					}
					else {
						maskImage.set(i, 0);
					}
				}
		  
		
	            try {
	                maskImage.exportData(0, length, imgBuffer); // locks and releases lock
	            } catch (final IOException error) {
	                displayError("Algorithm VOI transform: Image(s) locked");
	                setCompleted(false);

	                return;
	            }
		        
		        
		        for (k = 0; (k < oZdim) && !threadStopped; k++) {

		            if (pad) {
		                kAdj = k - AlgorithmTransform.margins[2];
		            } else {
		                kAdj = k;
		            }

		            kmm = kAdj * oZres;
		            k1 = (kmm * T02) + T03;
		            k2 = (kmm * T12) + T13;
		            k3 = (kmm * T22) + T23;

		            for (j = 0; (j < oYdim) && !threadStopped; j++) {

		                if (pad) {
		                    jAdj = j - AlgorithmTransform.margins[1];
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
		                        iAdj = i - AlgorithmTransform.margins[0];
		                    } else {
		                        iAdj = i;
		                    }

		                    imm = iAdj * oXres;
		                    value = fillValue; // if voxel is transformed out of bounds
		                    X = (j1 + (imm * T00)) * invXRes;

		                    if ( (X >= -0.5) && (X < iXdim)) {
		                        Y = (j2 + (imm * T10)) * invYRes;

		                        if ( (Y >= -0.5) && (Y < iYdim)) {
		                            Z = (j3 + (imm * T20)) * invZRes;

		                            if ( (Z >= -0.5) && (Z < iZdim)) {
		                                X0pos = Math.min((int) (X + 0.5), iXdim - 1);
		                                Y0pos = Math.min((int) (Y + 0.5), iYdim - 1) * iXdim;
		                                Z0pos = Math.min((int) (Z + 0.5), iZdim - 1) * sliceSize;
		                                value = imgBuffer[Z0pos + Y0pos + X0pos];
		                            } // end if Z in bounds
		                        } // end if Y in bounds
		                    } // end if X in bounds

		                    if (value != fillValue) {
		                        tmpMask.set(i, j, k, value);
		                    }
		                } // end for i
		            } // end for j
		        } // end for k
			       
			
		        if (threadStopped) {
		            return;
		        }
		
		        // ******* Make algorithm for VOI extraction.
		        if (index2 == curves.size()-1) {
		        	indexC++;
			        tmpMask.calcMinMax();
			
			        AlgorithmVOIExtraction VOIExtAlgo = new AlgorithmVOIExtraction(tmpMask);
			
			        VOIExtAlgo.setRunningInSeparateThread(runningInSeparateThread);
			        VOIExtAlgo.run();
			        VOIExtAlgo.finalize();
			        VOIExtAlgo = null;
			        destImage.addVOIs(tmpMask.getVOIs());
			        tmpMask.resetVOIs();
			        for (z = 0; z < oZdim; z++) {
			        	for (j = 0; j < oYdim; j++) {
			        		for (i = 0; i < oXdim; i++) {
			        			tmpMask.set(i, j, z, fillValue);
			        		}
			        	}
			        }
		        }
        	} // for (index2 = 0; index2 < curves.size(); index2++)
        } // for (index = 0; index < voiVector.size(); index++)
        maskImage.disposeLocal();
        maskImage = null;
        tmpMask.disposeLocal();
        tmpMask = null;

        
    }

    /**
     * Transforms and resamples a 3D VOI using nearest neighbor interpolation.
     * 
     * <ol>
     * <li>Export VOIs as a mask image</li>
     * <li>Transform mask</li>
     * <li>Extract VOI contours from mask image and put in new image.</li>
     * </ol>
     * 
     * @param image Image where VOIs are stored
     * @param imgBuffer Image array
     * @param kTM Transformation matrix to be applied use byte imgBuffer to save a factor of 4 in memory in 3D ARGB
     *            color images.
     */
    private void transform3DVOIByte(final ModelImage image, final byte[] imgBuffer, final TransMatrix kTM) {
        int i, j, k, z;
        int iAdj, jAdj, kAdj;
        int X0pos, Y0pos, Z0pos;
        float X, Y, Z;
        float value;
        float imm, jmm, kmm;
        float k1, k2, k3, j1, j2, j3;
        float byteFillValue;
        int index;
        int index2;
        int indexC;
        int sliceSize = iXdim * iYdim;
        int length = sliceSize * iZdim;
        int index2Size;
        VOIBaseVector curves = null;

        if (fillValue > 255.0f) {
            byteFillValue = 255.0f;
        } else if (fillValue < 0.0f) {
            byteFillValue = 0.0f;
        } else {
            byteFillValue = Math.round(fillValue);
        }


        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        ModelImage tmpMask;
        VOIVector voiVector;

        final int mod = Math.max(1, oXdim / 50);

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
        
        voiVector = image.getVOIs();
        
        if (voiVector.size() == 0) {
            return;
        }

        indexC = 0;
        final float invXRes = 1 / iXres;
        final float invYRes = 1 / iYres;
        final float invZRes = 1 / iZres;

        try {
            maskImage = new ModelImage(ModelStorageBase.SHORT, image.getExtents(), "Short Image");
            tmpMask = new ModelImage(ModelStorageBase.SHORT, destImage.getExtents(), null);
        } catch (final OutOfMemoryError error) {
            throw error;
        }
        for (z = 0; z < oZdim; z++) {
        	for (j = 0; j < oYdim; j++) {
        		for (i = 0; i < oXdim; i++) {
        			tmpMask.set(i, j, z, fillValue);
        		}
        	}
        }
      
        for (index = 0; index < voiVector.size(); index++) {
        	VOI presentVOI = voiVector.elementAt(index);
        	if (presentVOI.getCurveType() == VOI.CONTOUR) {
        		curves = presentVOI.getCurves();	
        		index2Size = curves.size();
        	}
        	else {
        		index2Size = 1;
        	}
            
        	for (index2 = 0; index2 < index2Size; index2++) {
        
		        maskImage.clearMask();
		        
		        (voiVector.elementAt(index)).createOneElementBinaryMask3D(maskImage.getMask(), iXdim, iYdim, false, false, index2);

				BitSet mask = maskImage.getMask();

				
				for (i = 0; i < length; i++) {

					if (mask.get(i)) {
						maskImage.set(i, indexC + 1);
					}
					else {
						maskImage.set(i, 0);
					}
				}
		  
		
	            try {
	                maskImage.exportData(0, length, imgBuffer); // locks and releases lock
	            } catch (final IOException error) {
	                displayError("Algorithm VOI transform: Image(s) locked");
	                setCompleted(false);

	                return;
	            }
		        
		        
		        for (k = 0; (k < oZdim) && !threadStopped; k++) {

		            if ( ( (k % mod) == 0)) {
		                fireProgressStateChanged((int) ( ((float) k / oZdim * 100) + 0.5f));
		            }

		            if (pad) {
		                kAdj = k - AlgorithmTransform.margins[2];
		            } else {
		                kAdj = k;
		            }

		            kmm = kAdj * oZres;
		            k1 = (kmm * T02) + T03;
		            k2 = (kmm * T12) + T13;
		            k3 = (kmm * T22) + T23;

		            for (j = 0; (j < oYdim) && !threadStopped; j++) {

		                if (pad) {
		                    jAdj = j - AlgorithmTransform.margins[1];
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
		                        iAdj = i - AlgorithmTransform.margins[0];
		                    } else {
		                        iAdj = i;
		                    }

		                    imm = iAdj * oXres;
		                    value = byteFillValue; // if voxel is transformed out of bounds
		                    X = (j1 + (imm * T00)) * invXRes;

		                    if ( (X >= -0.5) && (X < iXdim)) {
		                        Y = (j2 + (imm * T10)) * invYRes;

		                        if ( (Y >= -0.5) && (Y < iYdim)) {
		                            Z = (j3 + (imm * T20)) * invZRes;

		                            if ( (Z >= -0.5) && (Z < iZdim)) {
		                                X0pos = Math.min((int) (X + 0.5f), iXdim - 1);
		                                Y0pos = Math.min((int) (Y + 0.5f), iYdim - 1) * iXdim;
		                                Z0pos = Math.min((int) (Z + 0.5f), iZdim - 1) * sliceSize;
		                                value = (imgBuffer[Z0pos + Y0pos + X0pos] & 0xff);
		                            } // end if Z in bounds
		                        } // end if Y in bounds
		                    } // end if X in bounds

		                    if (value != byteFillValue) {
		                        tmpMask.set(i, j, k, value);
		                    }
		                } // end for i
		            } // end for j
		        } // end for k
			       
			
		        if (threadStopped) {
		            return;
		        }
		
		        // ******* Make algorithm for VOI extraction.
		        if (index2 == curves.size()-1) {
		        	indexC++;
		        	
			        tmpMask.calcMinMax();
			
			        AlgorithmVOIExtraction VOIExtAlgo = new AlgorithmVOIExtraction(tmpMask);
			
			        VOIExtAlgo.setRunningInSeparateThread(runningInSeparateThread);
			        VOIExtAlgo.run();
			        VOIExtAlgo.finalize();
			        VOIExtAlgo = null;
			        destImage.addVOIs(tmpMask.getVOIs());
			        tmpMask.resetVOIs();
			        for (z = 0; z < oZdim; z++) {
			        	for (j = 0; j < oYdim; j++) {
			        		for (i = 0; i < oXdim; i++) {
			        			tmpMask.set(i, j, z, byteFillValue);
			        		}
			        	}
			        }
		        }
        	} // for (index2 = 0; index2 < curves.size(); index2++)
        } // for (index = 0; index < voiVector.size(); index++)
        maskImage.disposeLocal();
        maskImage = null;
        tmpMask.disposeLocal();
        tmpMask = null;

        
    }

    /**
     * Transforms and resamples volume using bilinear interpolation.
     * 
     * @param imgBuf Image array
     * @param kTM Matrix to be applied
     */
    private void transformBilinear(final double[] imgBuf, final TransMatrix kTM) {
        int i, j;
        int iAdj, jAdj;
        double X, Y;
        int x0, y0;
        double j1, j2;
        double value;
        double imm, jmm;
        int deltaX, deltaY;

        final int mod = Math.max(1, oYdim / 25);

        double T00, T01, T02, T10, T11, T12;

        T00 = (double)kTM.M00;
        T01 =(double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        int position;
        double dx, dy, dx1, dy1;

        final double invXRes = 1.0 / iXres;
        final double invYRes = 1.0 / iYres;

        int index = 0;

        for (j = 0; (j < oYdim) && !threadStopped; j++) {

            if ( ( (j % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) j / oYdim * 100) + 0.5f));
            }

            if (pad) {
                jAdj = j - AlgorithmTransform.margins[1];
            } else {
                jAdj = j;
            }

            jmm = jAdj * oYres;
            j1 = (jmm * T01) + T02;
            j2 = (jmm * T11) + T12;

            for (i = 0; (i < oXdim) && !threadStopped; i++) {

                // transform i,j,k
                value = fillValue; // if voxel is transformed out of bounds
                if (pad) {
                    iAdj = i - AlgorithmTransform.margins[0];
                } else {
                    iAdj = i;
                }

                imm = iAdj * oXres;
                X = (j1 + (imm * T00)) * invXRes;

                if ( (X > -0.5) && (X < iXdim)) {
                    Y = (j2 + (imm * T10)) * invYRes;

                    if ( (Y > -0.5) && (Y < iYdim)) {

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

                        value = (dy1 * ( (dx1 * imgBuf[position]) + (dx * imgBuf[position + deltaX])))
                                + (dy * ( (dx1 * imgBuf[position + deltaY]) + (dx * imgBuf[position + deltaY + deltaX])));
                    } // end if Y in bounds
                } // end if X in bounds

                destImage.set(index++, value);
            } // end for i
        } // end for j
    }

    /**
     * Transforms and resamples volume using bilinear interpolation. Used as a slice-only algorithm on 3D images.
     * 
     * @param imgBuf Image array
     * @param kTM Matrix to be applied
     */
    private void transformBilinear3D(final double[] imgBuf, final TransMatrix kTM) {
        int i, j, k;
        int iAdj, jAdj;
        int X0pos, Y0pos;
        int X1pos, Y1pos;
        double X, Y;
        double x0, y0;
        double x1, y1;
        double value;
        double imm, jmm;
        int roundX, roundY;
        double temp1, temp2;
        double T00, T01, T02, T10, T11, T12;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        for (k = 0; (k < oZdim) && !threadStopped; k++) {
            fireProgressStateChanged(Math.round((float) k / oZdim * 100));

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                if (pad) {
                    iAdj = i - AlgorithmTransform.margins[0];
                } else {
                    iAdj = i;
                }
                imm = iAdj * oXres;
                temp1 = (imm * T00) + T02;
                temp2 = (imm * T10) + T12;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {

                    // transform i,j
                    value = fillValue; // if voxel is transformed out of bounds
                    if (pad) {
                        jAdj = j - AlgorithmTransform.margins[1];
                    } else {
                        jAdj = j;
                    }
                    jmm = jAdj * oYres;
                    X = (temp1 + (jmm * T01)) / iXres;
                    roundX = (int) (X + 0.5);

                    if ( (X >= 0) && (X < iXdim)) {
                        Y = (temp2 + (jmm * T11)) / iYres;
                        roundY = (int) (Y + 0.5);

                        if ( (Y >= 0) && (Y < iYdim)) {

                            if ( (X >= (iXdim - 1)) || (Y >= (iYdim - 1))) { // cannot interpolate on last X
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
                                value = (x1 * y1 * imgBuf[Y0pos + X0pos]) + (x0 * y1 * imgBuf[Y0pos + X1pos])
                                        + (x1 * y0 * imgBuf[Y1pos + X0pos]) + (x0 * y0 * imgBuf[Y1pos + X1pos]);
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
                    srcImage.exportData( (k + 1) * imgLength, imgLength, imgBuf);
                } catch (final IOException error) {
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
     * @param imgBuf Input image array
     * @param imgBuf2 Output image array
     * @param kTM Matrix to be applied
     */
    private void transformBilinear3DC(final double[] imgBuf, final float[] imgBuf2, final TransMatrix kTM) {
        int i, j, k;
        int iAdj, jAdj;
        int X0pos, Y0pos;
        int X1pos, Y1pos;
        double X, Y;
        double x0, y0;
        double x1, y1;
        double imm, jmm;

        final int mod = Math.max(1, oXdim / 50);

        int roundX, roundY;
        double x1y1, x0y1, x1y0, x0y0;
        double temp1, temp2;
        int temp3, temp4, temp5, temp6, temp7, temp8;
        double T00, T01, T02, T10, T11, T12;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        for (k = 0; (k < oZdim) && !threadStopped; k++) {

            if ( ( (k % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) k / oZdim * 100) + 0.5));
            }

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                if (pad) {
                    iAdj = i - AlgorithmTransform.margins[0];
                } else {
                    iAdj = i;
                }
                imm = iAdj * oXres;
                temp1 = (imm * T00) + T02;
                temp2 = (imm * T10) + T12;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {
                    temp3 = 4 * (i + (j * oXdim));
                    imgBuf2[temp3] = fillValue; // if voxel is transformed out of bounds
                    imgBuf2[temp3 + 1] = fillValue;
                    imgBuf2[temp3 + 2] = fillValue;
                    imgBuf2[temp3 + 3] = fillValue;
                    // transform i,j
                    if (pad) {
                        jAdj = j - AlgorithmTransform.margins[1];
                    } else {
                        jAdj = j;
                    }

                    jmm = jAdj * oYres;
                    X = (temp1 + (jmm * T01)) / iXres;
                    roundX = (int) (X + 0.5);

                    if ( (X >= 0) && (X < iXdim)) {
                        Y = (temp2 + (jmm * T11)) / iYres;
                        roundY = (int) (Y + 0.5);

                        if ( (Y >= 0) && (Y < iYdim)) {

                            if ( (X >= (iXdim - 1)) || (Y >= (iYdim - 1))) { // cannot interpolate on last X
                                // or Y
                                X0pos = Math.min(roundX, iXdim - 1);
                                Y0pos = Math.min(roundY, iYdim - 1) * iXdim;
                                temp4 = 4 * (Y0pos + X0pos);
                                imgBuf2[4 * (i + (j * oXdim))] = (float)imgBuf[temp4];
                                imgBuf2[ (4 * (i + (j * oXdim))) + 1] = (float)imgBuf[temp4 + 1];
                                imgBuf2[ (4 * (i + (j * oXdim))) + 2] = (float)imgBuf[temp4 + 2];
                                imgBuf2[ (4 * (i + (j * oXdim))) + 3] = (float)imgBuf[temp4 + 3];
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
                                imgBuf2[temp3] = (float)((x1y1 * imgBuf[temp5]) + (x0y1 * imgBuf[temp6])
                                        + (x1y0 * imgBuf[temp7]) + (x0y0 * imgBuf[temp8]));
                                imgBuf2[temp3 + 1] = (float)((x1y1 * imgBuf[temp5 + 1]) + (x0y1 * imgBuf[temp6 + 1])
                                        + (x1y0 * imgBuf[temp7 + 1]) + (x0y0 * imgBuf[temp8 + 1]));
                                imgBuf2[temp3 + 2] = (float)((x1y1 * imgBuf[temp5 + 2]) + (x0y1 * imgBuf[temp6 + 2])
                                        + (x1y0 * imgBuf[temp7 + 2]) + (x0y0 * imgBuf[temp8 + 2]));
                                imgBuf2[temp3 + 3] = (float)((x1y1 * imgBuf[temp5 + 3]) + (x0y1 * imgBuf[temp6 + 3])
                                        + (x1y0 * imgBuf[temp7 + 3]) + (x0y0 * imgBuf[temp8 + 3]));
                            }
                        } // end if Y in bounds
                    } // end if X in bounds

                } // end for j
            } // end for i

            try {
                destImage.importData(4 * k * oXdim * oYdim, imgBuf2, true);
            } catch (final IOException error) {
                MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
            }

            if (k < (oZdim - 1)) {

                try {
                    srcImage.exportData( (k + 1) * imgLength, imgLength, imgBuf);
                } catch (final IOException error) {
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
     * @param imgBuf Image array
     * @param kTM Transformation matrix to be applied
     */
    private void transformBilinear4D(final double[] imgBuf, final TransMatrix kTM) {
        int i, j, k, l;
        int iAdj, jAdj;
        int X0pos, Y0pos;
        int X1pos, Y1pos;
        double X, Y;
        double x0, y0;
        double x1, y1;
        double value;
        float imm, jmm;
        int roundX, roundY;
        double temp1, temp2;
        double T00, T01, T02, T10, T11, T12;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged((int) ( ((float) l / oTdim * 100) + 0.5));

            for (k = 0; (k < oZdim) && !threadStopped; k++) {

                for (i = 0; (i < oXdim) && !threadStopped; i++) {
                    if (pad) {
                        iAdj = i - AlgorithmTransform.margins[0];
                    } else {
                        iAdj = i;
                    }
                    imm = iAdj * oXres;
                    temp1 = (imm * T00) + T02;
                    temp2 = (imm * T10) + T12;

                    for (j = 0; (j < oYdim) && !threadStopped; j++) {

                        // transform i,j
                        value = fillValue; // if voxel is transformed out of bounds
                        if (pad) {
                            jAdj = j - AlgorithmTransform.margins[1];
                        } else {
                            jAdj = j;
                        }
                        jmm = jAdj * oYres;
                        X = (temp1 + (jmm * T01)) / iXres;
                        roundX = (int) (X + 0.5);

                        if ( (X >= 0) && (X < iXdim)) {
                            Y = (temp2 + (jmm * T11)) / iYres;
                            roundY = (int) (Y + 0.5);

                            if ( (Y >= 0) && (Y < iYdim)) {

                                if ( (X >= (iXdim - 1)) || (Y >= (iYdim - 1))) { // cannot interpolate on last
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
                                    value = (x1 * y1 * imgBuf[Y0pos + X0pos]) + (x0 * y1 * imgBuf[Y0pos + X1pos])
                                            + (x1 * y0 * imgBuf[Y1pos + X0pos]) + (x0 * y0 * imgBuf[Y1pos + X1pos]);
                                }
                            } // end if Y in bounds
                        } // end if X in bounds

                        destImage.set(i, j, k, l, value);
                    } // end for j
                } // end for i

                if ( (k < (oZdim - 1)) || (l < (oTdim - 1))) {

                    try {
                        srcImage.exportData( (l * oZdim * imgLength) + ( (k + 1) * imgLength), imgLength, imgBuf);
                    } catch (final IOException error) {
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
     * @param imgBuf Input image array
     * @param imgBuf2 Output image array
     * @param kTM Matrix to be applied
     */
    private void transformBilinear4DC(final double[] imgBuf, final float[] imgBuf2, final TransMatrix kTM) {
        int i, j, k, l;
        int iAdj, jAdj;
        int X0pos, Y0pos;
        int X1pos, Y1pos;
        double X, Y;
        double x0, y0;
        double x1, y1;
        double imm, jmm;
        int roundX, roundY;
        double x1y1, x0y1, x1y0, x0y0;
        double temp1, temp2;
        int temp3, temp4, temp5, temp6, temp7, temp8;
        double T00, T01, T02, T10, T11, T12;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged((int) ( ((float) l / oTdim * 100) + 0.5));

            for (k = 0; (k < oZdim) && !threadStopped; k++) {

                for (i = 0; (i < oXdim) && !threadStopped; i++) {
                    if (pad) {
                        iAdj = i - AlgorithmTransform.margins[0];
                    } else {
                        iAdj = i;
                    }
                    imm = iAdj * oXres;
                    temp1 = (imm * T00) + T02;
                    temp2 = (imm * T10) + T12;

                    for (j = 0; (j < oYdim) && !threadStopped; j++) {

                        // transform i,j
                        temp3 = 4 * (i + (j * oXdim));
                        imgBuf2[temp3] = fillValue; // if voxel is transformed out of bounds
                        imgBuf2[temp3 + 1] = fillValue;
                        imgBuf2[temp3 + 2] = fillValue;
                        imgBuf2[temp3 + 3] = fillValue;
                        if (pad) {
                            jAdj = j - AlgorithmTransform.margins[1];
                        } else {
                            jAdj = j;
                        }

                        jmm = jAdj * oYres;
                        X = (temp1 + (jmm * T01)) / iXres;
                        roundX = (int) (X + 0.5);

                        if ( (X >= 0) && (X < iXdim)) {
                            Y = (temp2 + (jmm * T11)) / iYres;
                            roundY = (int) (Y + 0.5);

                            if ( (Y >= 0) && (Y < iYdim)) {

                                if ( (X >= (iXdim - 1)) || (Y >= (iYdim - 1))) { // cannot interpolate on last
                                    // X or Y
                                    X0pos = Math.min(roundX, iXdim - 1);
                                    Y0pos = Math.min(roundY, iYdim - 1) * iXdim;
                                    temp4 = 4 * (Y0pos + X0pos);
                                    imgBuf2[temp3] = (float)imgBuf[temp4];
                                    imgBuf2[temp3 + 1] = (float)imgBuf[temp4 + 1];
                                    imgBuf2[temp3 + 2] = (float)imgBuf[temp4 + 2];
                                    imgBuf2[temp3 + 3] = (float)imgBuf[temp4 + 3];
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
                                    imgBuf2[temp3] = (float)((x1y1 * imgBuf[temp5]) + (x0y1 * imgBuf[temp6])
                                            + (x1y0 * imgBuf[temp7]) + (x0y0 * imgBuf[temp8]));
                                    imgBuf2[temp3 + 1] = (float)((x1y1 * imgBuf[temp5 + 1]) + (x0y1 * imgBuf[temp6 + 1])
                                            + (x1y0 * imgBuf[temp7 + 1]) + (x0y0 * imgBuf[temp8 + 1]));
                                    imgBuf2[temp3 + 2] = (float)((x1y1 * imgBuf[temp5 + 2]) + (x0y1 * imgBuf[temp6 + 2])
                                            + (x1y0 * imgBuf[temp7 + 2]) + (x0y0 * imgBuf[temp8 + 2]));
                                    imgBuf2[temp3 + 3] = (float)((x1y1 * imgBuf[temp5 + 3]) + (x0y1 * imgBuf[temp6 + 3])
                                            + (x1y0 * imgBuf[temp7 + 3]) + (x0y0 * imgBuf[temp8 + 3]));
                                }
                            } // end if Y in bounds
                        } // end if X in bounds

                    } // end for j
                } // end for i

                try {
                    destImage.importData( (4 * l * oXdim * oYdim * oZdim) + (4 * k * oXdim * oYdim), imgBuf2, true);
                } catch (final IOException error) {
                    MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
                }

                if ( (k < (oZdim - 1)) || (l < (oTdim - 1))) {

                    try {
                        srcImage.exportData( (l * oZdim * imgLength) + ( (k + 1) * imgLength), imgLength, imgBuf);
                    } catch (final IOException error) {
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
     * @param imgBuf Input image array
     * @param imgBuf2 Output image array
     * @param kTM Matrix to be applied
     */
    private void transformBilinearC(final double[] imgBuf, final float[] imgBuf2, final TransMatrix kTM) {
        int i, j;
        int iAdj, jAdj;
        int X0pos, Y0pos;
        int X1pos, Y1pos;
        double X, Y;
        double x0, y0;
        double x1, y1;
        double imm, jmm;

        final int mod = Math.max(1, oXdim / 50);

        int roundX, roundY;
        double x1y1, x0y1, x1y0, x0y0;
        double temp1, temp2;
        int temp3, temp4, temp5, temp6, temp7, temp8;
        double T00, T01, T02, T10, T11, T12;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if (pad) {
                iAdj = i - AlgorithmTransform.margins[0];
            } else {
                iAdj = i;
            }

            imm = iAdj * oXres;
            temp1 = (imm * T00) + T02;
            temp2 = (imm * T10) + T12;

            if ( ( (i % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) i / oXdim * 100) + 0.5));
            }

            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                // transform i,j
                temp3 = 4 * (i + (j * oXdim));
                imgBuf2[temp3] = fillValue; // if voxel is transformed out of bounds
                imgBuf2[temp3 + 1] = fillValue;
                imgBuf2[temp3 + 2] = fillValue;
                imgBuf2[temp3 + 3] = fillValue;
                if (pad) {
                    jAdj = j - AlgorithmTransform.margins[1];
                } else {
                    jAdj = j;
                }

                jmm = jAdj * oYres;

                X = (temp1 + (jmm * T01)) / iXres;
                roundX = (int) (X + 0.5);

                if ( (X >= 0) && (X < iXdim)) {
                    Y = (temp2 + (jmm * T11)) / iYres;
                    roundY = (int) (Y + 0.5);

                    if ( (Y >= 0) && (Y < iYdim)) {

                        if ( (X >= (iXdim - 1)) || (Y >= (iYdim - 1))) { // cannot interpolate on last X or Y
                            X0pos = Math.min(roundX, iXdim - 1);
                            Y0pos = Math.min(roundY, iYdim - 1) * iXdim;
                            temp4 = 4 * (Y0pos + X0pos);
                            imgBuf2[temp3] = (float)imgBuf[temp4];
                            imgBuf2[temp3 + 1] = (float)imgBuf[temp4 + 1];
                            imgBuf2[temp3 + 2] = (float)imgBuf[temp4 + 2];
                            imgBuf2[temp3 + 3] = (float)imgBuf[temp4 + 3];
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
                            imgBuf2[temp3] = (float)((x1y1 * imgBuf[temp5]) + (x0y1 * imgBuf[temp6]) + (x1y0 * imgBuf[temp7])
                                    + (x0y0 * imgBuf[temp8]));
                            imgBuf2[temp3 + 1] = (float)((x1y1 * imgBuf[temp5 + 1]) + (x0y1 * imgBuf[temp6 + 1])
                                    + (x1y0 * imgBuf[temp7 + 1]) + (x0y0 * imgBuf[temp8 + 1]));
                            imgBuf2[temp3 + 2] = (float)((x1y1 * imgBuf[temp5 + 2]) + (x0y1 * imgBuf[temp6 + 2])
                                    + (x1y0 * imgBuf[temp7 + 2]) + (x0y0 * imgBuf[temp8 + 2]));
                            imgBuf2[temp3 + 3] = (float)((x1y1 * imgBuf[temp5 + 3]) + (x0y1 * imgBuf[temp6 + 3])
                                    + (x1y0 * imgBuf[temp7 + 3]) + (x0y0 * imgBuf[temp8 + 3]));
                        }
                    } // end if Y in bounds
                } // end if X in bounds

            } // end for j
        } // end for i

        if (threadStopped) {
            return;
        }

        try {
            destImage.importData(0, imgBuf2, true);
        } catch (final IOException error) {
            MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
        }
    }
    
	/**
	 * Transforms and resamples volume using Bspline interpolation.
	 * 
	 * @param imgBuf image array
	 * @param kTM transformation matrix to be applied
	 * @param degree degree of polynomial
	 */
	private void transformAlgorithmBspline2D(final double[] imgBuf, final TransMatrix kTM, final int degree) {
		int i, j;
		int iAdj, jAdj;
		double X, Y;
		double value;
		double imm, jmm;
		final int mod = Math.max(1, oYdim / 50);

		double j1, j2;
		double T00, T01, T02, T10, T11, T12;
		int nz;
		int x, y, z;
		double sliceMin;
		double sliceMax;

		T00 = (double)kTM.M00;
		T01 = (double)kTM.M01;
		T02 = (double)kTM.M02;
		T10 = (double)kTM.M10;
		T11 = (double)kTM.M11;
		T12 = (double)kTM.M12;

		if (srcImage.getNDims() == 2) {
			nz = 1;
		} else if (srcImage.getNDims() == 3) {
			nz = srcImage.getExtents()[2];
		} else {
			nz = srcImage.getExtents()[2] * srcImage.getExtents()[3];
		}

		AlgorithmBSpline Bspline = new AlgorithmBSpline();

		final float invXRes = 1 / iXres;
		final float invYRes = 1 / iYres;

		int index = 0;

		for (z = 0; z < nz; z++) {

			if (z >= 1) {

				try {
					srcImage.exportData(z * imgLength, imgLength, imgBuf);
				} catch (final IOException error) {
					displayError("Algorithm Transform: Image(s) locked");
					setCompleted(false);

					return;
				}
			}

			sliceMin = Float.MAX_VALUE;
			sliceMax = -Float.MAX_VALUE;

			for (y = 0; y < iYdim; y++) {
				for (x = 0; x < iXdim; x++) {
					if (imgBuf[x + (iXdim * y)] > sliceMax) {
						sliceMax = imgBuf[x + (iXdim * y)];
					}
					if (imgBuf[x + (iXdim * y)] < sliceMin) {
						sliceMin = imgBuf[x + (iXdim * y)];
					}
				}
			}

			Bspline.setup2DBSpline(imgBuf, new int[]{iXdim,iYdim}, degree);

			for (j = 0; (j < oYdim) && !threadStopped; j++) {

				if ( ( (j % mod) == 0)) {
					fireProgressStateChanged((int) ( ( ((float) z / nz * 100) + ((float) j / (oYdim * nz) * 100)) + 0.5f));
				}

				if (pad) {
					jAdj = j - AlgorithmTransform.margins[1];
				} else {
					jAdj = j;
				}

				jmm = jAdj * oYres;
				j1 = (jmm * T01) + T02;
				j2 = (jmm * T11) + T12;

				for (i = 0; (i < oXdim) && !threadStopped; i++) {

					// transform i,j,z
					value = fillValue; // if voxel is transformed out of bounds
					if (pad) {
						iAdj = i - AlgorithmTransform.margins[0];
					} else {
						iAdj = i;
					}
					imm = iAdj * oXres;
					X = (j1 + (imm * T00)) * invXRes;

					if ( (X > -0.5f) && (X < iXdim)) {
						Y = (j2 + (imm * T10)) * invYRes;

						if ( (Y > -0.5f) && (Y < iYdim)) {
							value = Bspline.bSpline2D(0, 0, X, Y);

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

		Preferences.debug("finished Bspline", Preferences.DEBUG_ALGORITHM);

	}


	/**
	 * Transforms and resamples volume using Bspline interpolation.
	 * 
	 * @param imgBuf image array
	 * @param kTM transformation matrix to be applied
	 * @param degree degree of polynomial
	 */
	private void transformAlgorithmBspline2DC(final double[] imgBuf, final TransMatrix kTM, final int degree) {
		int i, j;
		int iAdj, jAdj;
		double X, Y;
		double[] value = new double[4];
		double imm, jmm;
		final int mod = Math.max(1, oYdim / 50);

		double j1, j2;
		double T00, T01, T02, T10, T11, T12;
		int nz;
		int x, y, z;

		T00 = (double)kTM.M00;
		T01 = (double)kTM.M01;
		T02 = (double)kTM.M02;
		T10 = (double)kTM.M10;
		T11 = (double)kTM.M11;
		T12 = (double)kTM.M12;

		if (srcImage.getNDims() == 2) {
			nz = 1;
		} else if (srcImage.getNDims() == 3) {
			nz = srcImage.getExtents()[2];
		} else {
			nz = srcImage.getExtents()[2] * srcImage.getExtents()[3];
		}

		final AlgorithmBSpline Bspline = new AlgorithmBSpline();

		final double invXRes = 1.0 / iXres;
		final double invYRes = 1.0 / iYres;

		final int[] index = new int[4];
		index[0] = 0;
		index[1] = 1;
		index[2] = 2;
		index[3] = 3;

		final double[] imageMin = new double[4];
		final double[] imageMax = new double[4];

		for (z = 0; z < nz; z++) {

			if (z >= 1) {

				try {
					srcImage.exportData(z * imgLength, imgLength, imgBuf);
				} catch (final IOException error) {
					displayError("Algorithm Transform: Image(s) locked");
					setCompleted(false);

					return;
				}
			}
			for ( int c = 0; c < 4; c++ )
			{
				imageMin[c] = Float.MAX_VALUE;
				imageMax[c] = Float.MIN_VALUE;
			}

			for (y = 0; y < iYdim; y++) {
				for (x = 0; x < iXdim; x++) {
					for ( int c = 0; c < 4; c++ )
					{
						if ( imgBuf[ 4 * (y * iXdim + x) + c] > imageMax[c] )
						{
							imageMax[c] = imgBuf[ 4 * (y * iXdim + x) + c];
						}
						if ( imgBuf[ 4 * (y * iXdim + x) + c] < imageMin[c] )
						{
							imageMin[c] = imgBuf[ 4 * (y * iXdim + x) + c];
						}                   		
					}
				}
			}

			Bspline.setup2DBSplineC(imgBuf, new int[]{iXdim,iYdim}, degree);

			for (j = 0; (j < oYdim) && !threadStopped; j++) {

				if ( ( (j % mod) == 0)) {
					fireProgressStateChanged((int) ( ( ((float) z / nz * 100) + ((float) j / (oYdim * nz) * 100)) + 0.5f));
				}

				if (pad) {
					jAdj = j - AlgorithmTransform.margins[1];
				} else {
					jAdj = j;
				}

				jmm = jAdj * oYres;
				j1 = (jmm * T01) + T02;
				j2 = (jmm * T11) + T12;

				for (i = 0; (i < oXdim) && !threadStopped; i++) {

					// transform i,j,z
					value[0] = fillValue; // if voxel is transformed out of bounds
					value[1] = fillValue; // if voxel is transformed out of bounds
					value[2] = fillValue; // if voxel is transformed out of bounds
					value[3] = fillValue; // if voxel is transformed out of bounds
					if (pad) {
						iAdj = i - AlgorithmTransform.margins[0];
					} else {
						iAdj = i;
					}
					imm = iAdj * oXres;
					X = (j1 + (imm * T00)) * invXRes;

					if ( (X > -0.5f) && (X < iXdim)) {
						Y = (j2 + (imm * T10)) * invYRes;

						if ( (Y > -0.5f) && (Y < iYdim)) {
							value = Bspline.bSpline2DC(0, 0, X, Y);

							for ( int c = 0; c < 4; c++ )
							{
								if (value[c] > imageMax[c]) {
									value[c] = imageMax[c];
								} else if (value[c] < imageMin[c]) {
									value[c] = imageMin[c];
								}
							}
						}
					}

					for ( int c = 0; c < 4; c++ )
					{
						destImage.set(index[c], value[c]);
						index[c] += 4;
					}
				}
			}
		}

		Preferences.debug("finished Bspline", Preferences.DEBUG_ALGORITHM);

	}


	/**
	 * Transforms and resamples volume using Bspline interpolation.
	 * 
	 * @param imgBuf image array
	 * @param kTM transformation matrix to be applied
	 * @param degree degree of polynomial
	 */
	private void transformAlgorithmBspline3D(final double[] imgBuf, final TransMatrix kTM, final int degree) {
		int i, j, k;
		int iAdj, jAdj, kAdj;
		double X, Y, Z;
		double value;
		double imm, jmm, kmm;
		final int mod = Math.max(1, oZdim / 50);
		final AlgorithmBSpline Bspline = new AlgorithmBSpline();
		double imageMin;
		double imageMax;

		srcImage.calcMinMax();
		imageMin = srcImage.getMin();
		imageMax = srcImage.getMax();

		double k1, k2, k3, j1, j2, j3;

		double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

		T00 = (double)kTM.M00;
		T01 = (double)kTM.M01;
		T02 = (double)kTM.M02;
		T03 = (double)kTM.M03;
		T10 = (double)kTM.M10;
		T11 = (double)kTM.M11;
		T12 = (double)kTM.M12;
		T13 = (double)kTM.M13;
		T20 = (double)kTM.M20;
		T21 = (double)kTM.M21;
		T22 = (double)kTM.M22;
		T23 = (double)kTM.M23;

		Bspline.setup3DBSpline(imgBuf, new int[]{iXdim,iYdim,iZdim}, degree);

		final double invXRes = 1.0 / iXres;
		final double invYRes = 1.0 / iYres;
		final double invZRes = 1.0 / iZres;

		int index = 0;

		for (k = 0; (k < oZdim) && !threadStopped; k++) {

			if ( ( (k % mod) == 0)) {
				fireProgressStateChanged((int) ( ((float) k / oZdim * 100) + 0.5f));
			}

			if (pad) {
				kAdj = k - AlgorithmTransform.margins[2];
			} else {
				kAdj = k;
			}

			kmm = kAdj * oZres;
			k1 = (kmm * T02) + T03;
			k2 = (kmm * T12) + T13;
			k3 = (kmm * T22) + T23;

			for (j = 0; (j < oYdim) && !threadStopped; j++) {
				if (pad) {
					jAdj = j - AlgorithmTransform.margins[1];
				} else {
					jAdj = j;
				}
				jmm = jAdj * oYres;
				j1 = (jmm * T01) + k1;
				j2 = (jmm * T11) + k2;
				j3 = (jmm * T21) + k3;

				for (i = 0; (i < oXdim) && !threadStopped; i++) {

					// transform i,j,k
					value = fillValue; // if voxel is transformed out of bounds
					if (pad) {
						iAdj = i - AlgorithmTransform.margins[0];
					} else {
						iAdj = i;
					}
					imm = iAdj * oXres;
					X = (j1 + (imm * T00)) * invXRes;

					if ( (X > -0.5f) && (X < iXdim)) {
						Y = (j2 + (imm * T10)) * invYRes;

						if ( (Y > -0.5f) && (Y < iYdim)) {
							Z = (j3 + (imm * T20)) * invZRes;

							if ( (Z > -0.5f) && (Z < iZdim)) {
								//value = splineAlg.interpolatedValue(image, X, Y, Z, iXdim, iYdim, iZdim, degree);
								value = Bspline.bSpline3D(0, 0, 0, X, Y, Z);

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

		Preferences.debug("finished Bspline", Preferences.DEBUG_ALGORITHM);

	}


	/**
	 * Transforms and resamples volume using Bspline interpolation.
	 * 
	 * @param imgBuf image array
	 * @param kTM transformation matrix to be applied
	 * @param degree degree of polynomial
	 * */
	private void transformAlgorithmBspline3DC(final double[] imgBuf, final TransMatrix kTM, final int degree) {
		int i, j, k;
		int iAdj, jAdj, kAdj;
		double X, Y, Z;
		double[] value = new double[4];
		double imm, jmm, kmm;
		final int mod = Math.max(1, oZdim / 50);
		final double[] imageMin = new double[4];
		final double[] imageMax = new double[4];

		double k1, k2, k3, j1, j2, j3;

		double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

		T00 = (double)kTM.M00;
		T01 = (double)kTM.M01;
		T02 = (double)kTM.M02;
		T03 = (double)kTM.M03;
		T10 = (double)kTM.M10;
		T11 = (double)kTM.M11;
		T12 = (double)kTM.M12;
		T13 = (double)kTM.M13;
		T20 = (double)kTM.M20;
		T21 = (double)kTM.M21;
		T22 = (double)kTM.M22;
		T23 = (double)kTM.M23;

		final double invXRes = 1.0 / iXres;
		final double invYRes = 1.0 / iYres;
		final double invZRes = 1.0 / iZres;

		final int[] index = new int[4];
		index[0] = 0;
		index[1] = 1;
		index[2] = 2;
		index[3] = 3;

		imageMin[0] = srcImage.getMinA();
		imageMin[1] = srcImage.getMinR();
		imageMin[2] = srcImage.getMinG();
		imageMin[3] = srcImage.getMinB();
		imageMax[0] = srcImage.getMaxA();
		imageMax[1] = srcImage.getMaxR();
		imageMax[2] = srcImage.getMaxG();
		imageMax[3] = srcImage.getMaxB();

		AlgorithmBSpline Bspline = new AlgorithmBSpline();
		Bspline.setup3DBSplineC(imgBuf, new int[]{iXdim,iYdim,iZdim}, degree);

		for (k = 0; (k < oZdim) && !threadStopped; k++) {

			if ( ( (k % mod) == 0)) {
				fireProgressStateChanged((int) ( ((float) k / oZdim * 100) + 0.5f));
			}

			if (pad) {
				kAdj = k - AlgorithmTransform.margins[2];
			} else {
				kAdj = k;
			}

			kmm = kAdj * oZres;
			k1 = (kmm * T02) + T03;
			k2 = (kmm * T12) + T13;
			k3 = (kmm * T22) + T23;

			for (j = 0; (j < oYdim) && !threadStopped; j++) {
				if (pad) {
					jAdj = j - AlgorithmTransform.margins[1];
				} else {
					jAdj = j;
				}
				jmm = jAdj * oYres;
				j1 = (jmm * T01) + k1;
				j2 = (jmm * T11) + k2;
				j3 = (jmm * T21) + k3;

				for (i = 0; (i < oXdim) && !threadStopped; i++) {

					// transform i,j,k
					value[0] = fillValue; // if voxel is transformed out of bounds
					value[1] = fillValue; // if voxel is transformed out of bounds
					value[2] = fillValue; // if voxel is transformed out of bounds
					value[3] = fillValue; // if voxel is transformed out of bounds
					if (pad) {
						iAdj = i - AlgorithmTransform.margins[0];
					} else {
						iAdj = i;
					}
					imm = iAdj * oXres;
					X = (j1 + (imm * T00)) * invXRes;

					if ( (X > -0.5f) && (X < iXdim)) {
						Y = (j2 + (imm * T10)) * invYRes;

						if ( (Y > -0.5f) && (Y < iYdim)) {
							Z = (j3 + (imm * T20)) * invZRes;

							if ( (Z > -0.5f) && (Z < iZdim)) {
								value = Bspline.bSpline3DC(0, 0, 0, X, Y, Z);
								for ( int c = 0; c < 4; c++ )
								{
									if (value[c] > imageMax[c]) {
										value[c] = imageMax[c];
									} else if (value[c] < imageMin[c]) {
										value[c] = imageMin[c];
									}
								}
							}
						}
					}
					for ( int c = 0; c < 4; c++ )
					{
						destImage.set(index[c], value[c]);
						index[c] += 4;
					}
				}
			}
		}

		Preferences.debug("finished Bspline", Preferences.DEBUG_ALGORITHM);
	}

	/**
	 * Transforms and resamples volume using Bspline interpolation.
	 * 
	 * @param imgBuf image array
	 * @param kTM transformation matrix to be applied
	 * @param degree degree of polynomial
	 */
	private void transformAlgorithmBspline4D(final double[] imgBuf, final TransMatrix kTM, final int degree) {
		int i, j, k;
		int iAdj, jAdj, kAdj;
		double X, Y, Z;
		double value;
		double imm, jmm, kmm;
		int x, y, z;
		double volMin;
		double volMax;
		int t;

		double k1, k2, k3, j1, j2, j3;

		double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

		T00 = (double)kTM.M00;
		T01 = (double)kTM.M01;
		T02 = (double)kTM.M02;
		T03 = (double)kTM.M03;
		T10 = (double)kTM.M10;
		T11 = (double)kTM.M11;
		T12 = (double)kTM.M12;
		T13 = (double)kTM.M13;
		T20 = (double)kTM.M20;
		T21 = (double)kTM.M21;
		T22 = (double)kTM.M22;
		T23 = (double)kTM.M23;

		AlgorithmBSpline Bspline = new AlgorithmBSpline();

		final double invXRes = 1.0 / iXres;
		final double invYRes = 1.0 / iYres;
		final double invZRes = 1.0 / iZres;

		int index = 0;

		int iProgress = iTdim * oZdim;
		for (t = 0; t < iTdim; t++) {

			if ( (t >= 1)) {

				try {
					srcImage.exportData(t * imgLength, imgLength, imgBuf);
				} catch (final IOException error) {
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
						if (imgBuf[x + (iXdim * y) + (iXdim * iYdim * z)] > volMax) {
							volMax = imgBuf[x + (iXdim * y) + (iXdim * iYdim * z)];
						}
						if (imgBuf[x + (iXdim * y) + (iXdim * iYdim * z)] < volMin) {
							volMin = imgBuf[x + (iXdim * y) + (iXdim * iYdim * z)];
						}
					}
				}
			}

			Bspline.setup3DBSpline(imgBuf, new int[]{iXdim,iYdim,iZdim}, degree);

			for (k = 0; (k < oZdim) && !threadStopped; k++) {
				fireProgressStateChanged((int) ( ((float) (t*oZdim + k) / iProgress * 100) + 0.5));
				if (pad) {
					kAdj = k - AlgorithmTransform.margins[2];
				} else {
					kAdj = k;
				}
				kmm = kAdj * oZres;
				k1 = (kmm * T02) + T03;
				k2 = (kmm * T12) + T13;
				k3 = (kmm * T22) + T23;

				for (j = 0; (j < oYdim) && !threadStopped; j++) {
					if (pad) {
						jAdj = j - AlgorithmTransform.margins[1];
					} else {
						jAdj = j;
					}
					jmm = jAdj * oYres;
					j1 = (jmm * T01) + k1;
					j2 = (jmm * T11) + k2;
					j3 = (jmm * T21) + k3;

					for (i = 0; (i < oXdim) && !threadStopped; i++) {

						// transform i,j,k
						value = fillValue; // if voxel is transformed out of bounds
						if (pad) {
							iAdj = i - AlgorithmTransform.margins[0];
						} else {
							iAdj = i;
						}
						imm = iAdj * oXres;
						X = (j1 + (imm * T00)) * invXRes;

						if ( (X > -0.5f) && (X < iXdim)) {
							Y = (j2 + (imm * T10)) * invYRes;

							if ( (Y > -0.5f) && (Y < iYdim)) {
								Z = (j3 + (imm * T20)) * invZRes;

								if ( (Z > -0.5f) && (Z < iZdim)) {
									value = Bspline.bSpline3D(0, 0, 0, X, Y, Z);
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

		Preferences.debug("finished Bspline", Preferences.DEBUG_ALGORITHM);

	}

	/**
	 * Transforms and resamples color volume using Bspline interpolation.
	 * 
	 * @param imgBuf image array
	 * @param kTM transformation matrix to be applied
	 * @param degree degree of polynomial
	 */
	private void transformAlgorithmBspline4DC(final double[] imgBuf, final TransMatrix kTM, final int degree) {
		int i, j, k;
		int iAdj, jAdj, kAdj;
		double X, Y, Z;
		double[] value = new double[4];
		double imm, jmm, kmm;
		int x, y, z, c;
		final double volMin[] = new double[4];
		final double volMax[] = new double[4];
		int t;

		double k1, k2, k3, j1, j2, j3;

		double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

		T00 = (double)kTM.M00;
		T01 = (double)kTM.M01;
		T02 = (double)kTM.M02;
		T03 = (double)kTM.M03;
		T10 = (double)kTM.M10;
		T11 = (double)kTM.M11;
		T12 = (double)kTM.M12;
		T13 = (double)kTM.M13;
		T20 = (double)kTM.M20;
		T21 = (double)kTM.M21;
		T22 = (double)kTM.M22;
		T23 = (double)kTM.M23;

		AlgorithmBSpline Bspline = new AlgorithmBSpline();

		final double invXRes = 1.0 / iXres;
		final double invYRes = 1.0 / iYres;
		final double invZRes = 1.0 / iZres;

		final int index[] = new int[4];
		index[0] = 0;
		index[1] = 1;
		index[2] = 2;
		index[3] = 3;

		int iProgress = iTdim * oZdim;
		for (t = 0; t < iTdim; t++) {

			if ( (t >= 1)) {

				try {
					srcImage.exportData(t * imgLength, imgLength, imgBuf);
				} catch (final IOException error) {
					displayError("Algorithm Transform: Image(s) locked");
					setCompleted(false);

					return;
				}
			}

			for (c = 0; c < 4; c++) {
				volMin[c] = Float.MAX_VALUE;
				volMax[c] = -Float.MAX_VALUE;
			}
			for (z = 0; z < iZdim; z++) {
				for (y = 0; y < iYdim; y++) {
					for (x = 0; x < iXdim; x++) {
						for (c = 0; c < 4; c++) {
							if (imgBuf[4 * (x + (iXdim * y) + (iXdim * iYdim * z)) + c] > volMax[c]) {
								volMax[c] = imgBuf[4 * (x + (iXdim * y) + (iXdim * iYdim * z)) + c];
							}

							if (imgBuf[4 * (x + (iXdim * y) + (iXdim * iYdim * z)) + c] < volMin[c]) {
								volMin[c] = imgBuf[4 * (x + (iXdim * y) + (iXdim * iYdim * z)) + c];
							}
						}
					}
				}
			}

			Bspline.setup3DBSplineC(imgBuf, new int[]{iXdim,iYdim,iZdim}, degree);

			for (k = 0; (k < oZdim) && !threadStopped; k++) {
				fireProgressStateChanged((int) ( ((float) (t*oZdim + k) / iProgress * 100) + 0.5));
				
				if (pad) {
					kAdj = k - AlgorithmTransform.margins[2];
				} else {
					kAdj = k;
				}
				kmm = kAdj * oZres;
				k1 = (kmm * T02) + T03;
				k2 = (kmm * T12) + T13;
				k3 = (kmm * T22) + T23;

				for (j = 0; (j < oYdim) && !threadStopped; j++) {
					if (pad) {
						jAdj = j - AlgorithmTransform.margins[1];
					} else {
						jAdj = j;
					}
					jmm = jAdj * oYres;
					j1 = (jmm * T01) + k1;
					j2 = (jmm * T11) + k2;
					j3 = (jmm * T21) + k3;

					for (i = 0; (i < oXdim) && !threadStopped; i++) {

						// transform i,j,k
						value[0] = fillValue; // if voxel is transformed out of bounds
						value[1] = fillValue; // if voxel is transformed out of bounds
						value[2] = fillValue; // if voxel is transformed out of bounds
						value[3] = fillValue; // if voxel is transformed out of bounds
						if (pad) {
							iAdj = i - AlgorithmTransform.margins[0];
						} else {
							iAdj = i;
						}
						imm = iAdj * oXres;
						X = (j1 + (imm * T00)) * invXRes;

						if ( (X > -0.5f) && (X < iXdim)) {
							Y = (j2 + (imm * T10)) * invYRes;

							if ( (Y > -0.5f) && (Y < iYdim)) {
								Z = (j3 + (imm * T20)) * invZRes;

								if ( (Z > -0.5f) && (Z < iZdim)) {
									value = Bspline.bSpline3DC(0, 0, 0, X, Y, Z);
									for ( c = 0; c < 4; c++ )
									{
										if (value[c] > volMax[c]) {
											value[c] = volMax[c];
										} else if (value[c] < volMin[c]) {
											value[c] = volMin[c];
										}
									}
								}
							}
						}
						for ( c = 0; c < 4; c++ )
						{								
							destImage.set(index[c], value[c]);
							index[c] += 4;
						}
					}
				}
			}
		}
		Preferences.debug("finished Bspline", Preferences.DEBUG_ALGORITHM);
	}

    /**
     * Transforms and resamples volume using cubic Lagrangian interpolation.
     * 
     * @param imgBuf Image array
     * @param kTM Transformation matrix to be applied
     * @param clip if <code>true</code> clip output values to be within input range
     */
    private void transformCubicLagrangian2D(final double[] imgBuf, final TransMatrix kTM, final boolean clip) {
        AlgorithmCubicLagrangian CLag = new AlgorithmCubicLagrangian();
        int i, j;
        int iAdj, jAdj;
        double X, Y;
        double value;
        double imm, jmm;
        final int[] inVolExtents = {iXdim, iYdim};
        final int mod = Math.max(1, oXdim / 50);
        double temp1, temp2;
        double T00, T01, T02, T10, T11, T12;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        CLag.setup2DCubicLagrangian(imgBuf, inVolExtents, clip);

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if ( ( (i % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) i / oXdim * 100) + 0.5));
            }

            if (pad) {
                iAdj = i - AlgorithmTransform.margins[0];
            } else {
                iAdj = i;
            }

            imm = iAdj * oXres;
            temp1 = (imm * T00) + T02;
            temp2 = (imm * T10) + T12;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                // convert to mm
                value = fillValue; // if voxel is transformed out of bounds
                if (pad) {
                    jAdj = j - AlgorithmTransform.margins[1];
                } else {
                    jAdj = j;
                }
                jmm = jAdj * oYres;

                // transform i,j
                X = (temp1 + (jmm * T01)) / iXres;

                if ( (X >= 0) && (X < iXdim)) { // check bounds
                    Y = (temp2 + (jmm * T11)) / iYres;

                    if ( (Y >= 0) && (Y < iYdim)) {
                        value = CLag.cubicLagrangian2D(X, Y);
                    }
                }

                destImage.set(i, j, value);
            }
        }

        CLag.finalize();
        CLag = null;
        Preferences.debug("finished cubic Lagrangian", Preferences.DEBUG_ALGORITHM);
    }

    /**
     * Transforms and resamples volume using cubic Lagrangian interpolation. This version used with color images
     * 
     * @param imgBuf Input image array
     * @param imgBuf2 Output image array
     * @param kTM Transformation matrix to be applied
     * @param clip if true clip output values to be within input range
     */
    private void transformCubicLagrangian2DC(final double[] imgBuf, final float[] imgBuf2, final TransMatrix kTM,
            final boolean clip) {
        AlgorithmCubicLagrangian CLag = new AlgorithmCubicLagrangian();
        int i, j;
        int iAdj, jAdj;
        double X, Y;
        double[] value = new double[4];
        double imm, jmm;
        final int[] inVolExtents = {iXdim, iYdim};
        final int mod = Math.max(1, oXdim / 50);
        double temp1, temp2;
        int temp3;
        double T00, T01, T02, T10, T11, T12;
        float argbMax = 255.0f;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        if (srcImage.getType() == ModelStorageBase.ARGB) {
            argbMax = 255.0f;
        } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
            argbMax = 65535.0f;
        }

        CLag.setup2DCubicLagrangianC(imgBuf, inVolExtents, argbMax, clip);

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if ( ( (i % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) i / oXdim * 100) + 0.5));
            }

            if (pad) {
                iAdj = i - AlgorithmTransform.margins[0];
            } else {
                iAdj = i;
            }

            imm = iAdj * oXres;
            temp1 = (imm * T00) + T02;
            temp2 = (imm * T10) + T12;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                // convert to mm
                value[0] = fillValue; // if voxel is transformed out of bounds
                value[1] = fillValue;
                value[2] = fillValue;
                value[3] = fillValue;
                if (pad) {
                    jAdj = j - AlgorithmTransform.margins[1];
                } else {
                    jAdj = j;
                }
                jmm = jAdj * oYres;

                // transform i,j
                X = (temp1 + (jmm * T01)) / iXres;

                if ( (X >= 0) && (X < iXdim)) { // check bounds
                    Y = (temp2 + (jmm * T11)) / iYres;

                    if ( (Y >= 0) && (Y < iYdim)) {
                        value = CLag.cubicLagrangian2DC(X, Y);
                    }
                }

                temp3 = 4 * (i + (j * oXdim));
                imgBuf2[temp3] = (float)value[0];
                imgBuf2[temp3 + 1] = (float)value[1];
                imgBuf2[temp3 + 2] = (float)value[2];
                imgBuf2[temp3 + 3] = (float)value[3];
            }
        }

        if (threadStopped) {
            return;
        }

        try {
            destImage.importData(0, imgBuf2, true);
        } catch (final IOException error) {
            MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
        }

        CLag.finalize();
        CLag = null;
        Preferences.debug("finished cubic Lagrangian", Preferences.DEBUG_ALGORITHM);
    }

    /**
     * Transforms and resamples volume using cubic Lagrangian interpolation.
     * 
     * @param imgBuf Image array
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformCubicLagrangian3D(final double[] imgBuf, final TransMatrix kTM, final boolean clip) {
        AlgorithmCubicLagrangian CLag = new AlgorithmCubicLagrangian();
        int i, j, k;
        int iAdj, jAdj, kAdj;
        double X, Y, Z;
        double value;
        double imm, jmm, kmm;
        final int[] inVolExtents = {iXdim, iYdim, iZdim};
        final int mod = Math.max(1, oXdim / 50);

        double k1, k2, k3, j1, j2, j3;
        double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T03 = (double)kTM.M03;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;
        T13 = (double)kTM.M13;
        T20 = (double)kTM.M20;
        T21 = (double)kTM.M21;
        T22 = (double)kTM.M22;
        T23 = (double)kTM.M23;

        CLag.setup3DCubicLagrangian(imgBuf, inVolExtents, clip);

        final double invXRes = 1.0 / iXres;
        final double invYRes = 1.0 / iYres;
        final double invZRes = 1.0 / iZres;

        int index = 0;

        for (k = 0; (k < oZdim) && !threadStopped; k++) {

            if ( ( (k % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) k / oZdim * 100) + 0.5f));
            }

            if (pad) {
                kAdj = k - AlgorithmTransform.margins[2];
            } else {
                kAdj = k;
            }

            kmm = kAdj * oZres;
            k1 = (kmm * T02) + T03;
            k2 = (kmm * T12) + T13;
            k3 = (kmm * T22) + T23;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {
                if (pad) {
                    jAdj = j - AlgorithmTransform.margins[1];
                } else {
                    jAdj = j;
                }
                jmm = jAdj * oYres;
                j1 = (jmm * T01) + k1;
                j2 = (jmm * T11) + k2;
                j3 = (jmm * T21) + k3;

                for (i = 0; (i < oXdim) && !threadStopped; i++) {

                    // transform i,j,k
                    value = fillValue; // if voxel is transformed out of bounds
                    if (pad) {
                        iAdj = i - AlgorithmTransform.margins[0];
                    } else {
                        iAdj = i;
                    }
                    imm = iAdj * oXres;
                    X = (j1 + (imm * T00)) * invXRes;

                    if ( (X >= 0) && (X < iXdim)) {
                        Y = (j2 + (imm * T10)) * invYRes;

                        if ( (Y >= 0) && (Y < iYdim)) {
                            Z = (j3 + (imm * T20)) * invZRes;

                            if ( (Z >= 0) && (Z < iZdim)) {
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
        Preferences.debug("finished cubic Lagrangian", Preferences.DEBUG_ALGORITHM);
    }

    /**
     * Transforms and resamples volume using cubic Lagrangian interpolation. This version used with color images
     * 
     * @param imgBuf Input image array
     * @param imgBuf2 Output image array
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformCubicLagrangian3DC(final double[] imgBuf, final float[] imgBuf2, final TransMatrix kTM,
            final boolean clip) {
        AlgorithmCubicLagrangian CLag = new AlgorithmCubicLagrangian();
        int i, j, k;
        int iAdj, jAdj, kAdj;
        double X, Y, Z;
        double[] value = new double[4];
        double imm, jmm, kmm;
        final int[] inVolExtents = {iXdim, iYdim, iZdim};
        final int mod = Math.max(1, oXdim / 50);

        final int osliceSize = oXdim * oYdim;
        double i1, i2, i3, j1, j2, j3;
        double temp1, temp2, temp3;
        int temp4;
        double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        float argbMax = 255.0f;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T03 = (double)kTM.M03;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;
        T13 = (double)kTM.M13;
        T20 = (double)kTM.M20;
        T21 = (double)kTM.M21;
        T22 = (double)kTM.M22;
        T23 = (double)kTM.M23;

        if (srcImage.getType() == ModelStorageBase.ARGB) {
            argbMax = 255.0f;
        } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
            argbMax = 65535.0f;
        }

        CLag.setup3DCubicLagrangianC(imgBuf, inVolExtents, argbMax, clip);

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if ( ( (i % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) i / oXdim * 100) + 0.5));
            }

            if (pad) {
                iAdj = i - AlgorithmTransform.margins[0];
            } else {
                iAdj = i;
            }
            imm = iAdj * oXres;
            i1 = (imm * T00) + T03;
            i2 = (imm * T10) + T13;
            i3 = (imm * T20) + T23;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {
                if (pad) {
                    jAdj = j - AlgorithmTransform.margins[1];
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

                    // convert to mm
                    value[0] = fillValue; // if voxel is tranformed out of bounds
                    value[1] = fillValue;
                    value[2] = fillValue;
                    value[3] = fillValue;
                    if (pad) {
                        kAdj = k - AlgorithmTransform.margins[2];
                    } else {
                        kAdj = k;
                    }
                    kmm = kAdj * oZres;

                    // transform i,j,k
                    X = (temp3 + (kmm * T02)) / iXres;

                    // convert back to pixels
                    if ( (X >= 0) && (X < iXdim)) { // check bounds
                        Y = (temp2 + (kmm * T12)) / iYres;

                        if ( (Y >= 0) && (Y < iYdim)) {
                            Z = (temp1 + (kmm * T22)) / iZres;

                            if ( (Z >= 0) && (Z < iZdim)) {
                                value = CLag.cubicLagrangian3DC(X, Y, Z);
                            }
                        }
                    }

                    temp4 = 4 * (i + (j * oXdim) + (k * osliceSize));
                    imgBuf2[temp4] = (float)value[0];
                    imgBuf2[temp4 + 1] = (float)value[1];
                    imgBuf2[temp4 + 2] = (float)value[2];
                    imgBuf2[temp4 + 3] = (float)value[3];
                }
            }
        }

        if (threadStopped) {
            return;
        }

        try {
            destImage.importData(0, imgBuf2, true);
        } catch (final IOException error) {
            MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
        }

        CLag.finalize();
        CLag = null;
        Preferences.debug("finished cubic Lagrangian", Preferences.DEBUG_ALGORITHM);

    }

    /**
     * Transforms and resamples volume using cubic Lagrangian interpolation Does a slice by slice cubic Lagrangian
     * interpolation on a 3 dimensional object.
     * 
     * @param imgBuf Image array
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformCubicLagrangian3Dim2D(final double[] imgBuf, final TransMatrix kTM, final boolean clip) {
        AlgorithmCubicLagrangian CLag = new AlgorithmCubicLagrangian();
        int i, j, k;
        int iAdj, jAdj;
        double X, Y;
        double value;
        double imm, jmm;
        final int[] inVolExtents = {iXdim, iYdim};

        final int mod = Math.max(1, oZdim / 50);
        double temp1, temp2;
        double T00, T01, T02, T10, T11, T12;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        for (k = 0; (k < oZdim) && !threadStopped; k++) {

            if ( ( (k % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) k / oZdim * 100) + 0.5));
            }

            CLag.setup2DCubicLagrangian(imgBuf, inVolExtents, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                if (pad) {
                    iAdj = i - AlgorithmTransform.margins[0];
                } else {
                    iAdj = i;
                }
                imm = iAdj * oXres;
                temp1 = (imm * T00) + T02;
                temp2 = (imm * T10) + T12;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {

                    // convert to mm
                    value = fillValue; // if voxel is transformed out of bounds
                    if (pad) {
                        jAdj = j - AlgorithmTransform.margins[1];
                    } else {
                        jAdj = j;
                    }
                    jmm = jAdj * oYres;

                    // transform i,j
                    X = (temp1 + (jmm * T01)) / iXres;

                    if ( (X >= 0) && (X < iXdim)) { // check bounds
                        Y = (temp2 + (jmm * T11)) / iYres;

                        if ( (Y >= 0) && (Y < iYdim)) {
                            value = CLag.cubicLagrangian2D(X, Y);
                        }
                    }

                    destImage.set(i, j, k, value);
                }
            }

            if (k < (oZdim - 1)) {

                try {
                    srcImage.exportData( (k + 1) * imgLength, imgLength, imgBuf);
                } catch (final IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);

                    return;
                }
            } // end if (k < (oZdim - 1))
        } // end for k

        CLag.finalize();
        CLag = null;
        Preferences.debug("finished cubic Lagrangian", Preferences.DEBUG_ALGORITHM);

    }

    /**
     * Transforms and resamples volume using cubic Lagrangian interpolation This version used with color images This
     * version performs a slice by slice algorithm on a 3 dimensional object.
     * 
     * @param imgBuf Input image array
     * @param imgBuf2 Output image array
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformCubicLagrangian3Dim2DC(final double[] imgBuf, final float[] imgBuf2, final TransMatrix kTM,
            final boolean clip) {
        AlgorithmCubicLagrangian CLag = new AlgorithmCubicLagrangian();
        int i, j, k;
        int iAdj, jAdj;
        double X, Y;
        double[] value = new double[4];
        double imm, jmm;
        final int[] inVolExtents = {iXdim, iYdim};

        final int mod = Math.max(1, oZdim / 50);
        double temp1, temp2;
        int temp3;
        double T00, T01, T02, T10, T11, T12;
        float argbMax = 255.0f;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        for (k = 0; (k < oZdim) && !threadStopped; k++) {

            if ( ( (k % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) k / oZdim * 100) + 0.5));
            }

            if (srcImage.getType() == ModelStorageBase.ARGB) {
                argbMax = 255.0f;
            } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
                argbMax = 65535.0f;
            }

            CLag.setup2DCubicLagrangianC(imgBuf, inVolExtents, argbMax, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                if (pad) {
                    iAdj = i - AlgorithmTransform.margins[0];
                } else {
                    iAdj = i;
                }
                imm = iAdj * oXres;
                temp1 = (imm * T00) + T02;
                temp2 = (imm * T10) + T12;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {

                    // convert to mm
                    value[0] = fillValue; // if voxel is transformed out of bounds
                    value[1] = fillValue;
                    value[2] = fillValue;
                    value[3] = fillValue;
                    if (pad) {
                        jAdj = j - AlgorithmTransform.margins[1];
                    } else {
                        jAdj = j;
                    }
                    jmm = jAdj * oYres;

                    // transform i,j
                    X = (temp1 + (jmm * T01)) / iXres;

                    if ( (X >= 0) && (X < iXdim)) { // check bounds
                        Y = (temp2 + (jmm * T11)) / iYres;

                        if ( (Y >= 0) && (Y < iYdim)) {
                            value = CLag.cubicLagrangian2DC(X, Y);
                        }
                    }

                    temp3 = 4 * (i + (j * oXdim));
                    imgBuf2[temp3] = (float)value[0];
                    imgBuf2[temp3 + 1] = (float)value[1];
                    imgBuf2[temp3 + 2] = (float)value[2];
                    imgBuf2[temp3 + 3] = (float)value[3];
                } // for i
            } // for j

            try {
                destImage.importData(4 * k * oXdim * oYdim, imgBuf2, true);
            } catch (final IOException error) {
                MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
            }

            if (k < (oZdim - 1)) {

                try {
                    srcImage.exportData( (k + 1) * imgLength, imgLength, imgBuf);
                } catch (final IOException error) {
                    displayError("Algorithm Transform: IOException Error on importData");
                    setCompleted(false);

                    return;
                }
            } // end if (k < (oZdim - 1))
        } // end for k

        CLag.finalize();
        CLag = null;
        Preferences.debug("finished cubic Lagrangian", Preferences.DEBUG_ALGORITHM);

    }

    /**
     * Transforms and resamples 4 dimensional object using 3D cubic Lagrangian interpolation.
     * 
     * @param imgBuf Image array
     * @param kTM Transformation matrix to be applied
     * @param clip if <code>true</code> clip output values to be within input range
     */
    private void transformCubicLagrangian4D(final double[] imgBuf, final TransMatrix kTM, final boolean clip) {
        AlgorithmCubicLagrangian CLag = new AlgorithmCubicLagrangian();
        int i, j, k, l;
        int iAdj, jAdj, kAdj;
        double X, Y, Z;
        double value;
        float imm, jmm, kmm;
        final int[] inVolExtents = {iXdim, iYdim, iZdim};
        final int mod = Math.max(1, oTdim / 50);

        double i1, i2, i3, j1, j2, j3;
        double temp1, temp2, temp3;
        double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T03 = (double)kTM.M03;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;
        T13 = (double)kTM.M13;
        T20 = (double)kTM.M20;
        T21 = (double)kTM.M21;
        T22 = (double)kTM.M22;
        T23 = (double)kTM.M23;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {

            if ( ( (l % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) l / oTdim * 100) + .5));
            }

            CLag.setup3DCubicLagrangian(imgBuf, inVolExtents, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                if (pad) {
                    iAdj = i - AlgorithmTransform.margins[0];
                } else {
                    iAdj = i;
                }
                imm = iAdj * oXres;
                i1 = (imm * T00) + T03;
                i2 = (imm * T10) + T13;
                i3 = (imm * T20) + T23;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {
                    if (pad) {
                        jAdj = j - AlgorithmTransform.margins[1];
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

                        // convert to mm
                        value = fillValue; // if voxel is transformed out of bounds
                        if (pad) {
                            kAdj = k - AlgorithmTransform.margins[2];
                        } else {
                            kAdj = k;
                        }
                        kmm = kAdj * oZres;

                        // transform i,j,k
                        X = (temp3 + (kmm * T02)) / iXres;

                        if ( (X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (kmm * T12)) / iYres;

                            if ( (Y >= 0) && (Y < iYdim)) {
                                Z = (temp1 + (kmm * T22)) / iZres;

                                if ( (Z >= 0) && (Z < iZdim)) {
                                    value = CLag.cubicLagrangian3D(X, Y, Z);
                                }
                            }
                        }

                        destImage.set(i, j, k, l, value);
                    }
                }
            }

            if (l < (oTdim - 1)) {

                try {
                    srcImage.exportData( (l + 1) * imgLength, imgLength, imgBuf);
                } catch (final IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);

                    return;
                }
            } // end if (l < (oTdim - 1))
        } // for l

        CLag.finalize();
        CLag = null;
        Preferences.debug("finished cubic Lagrangian", Preferences.DEBUG_ALGORITHM);

    }

    /**
     * Transforms and resamples 4 dimensional object using 3D cubic Lagrangian interpolation.
     * 
     * @param imgBuf Image array
     * @param imgBuffer2
     * @param kTM Transformation matrix to be applied
     * @param clip if <code>true</code> clip output values to be within input range
     */
    private void transformCubicLagrangian4DC(final double[] imgBuf, final float imgBuffer2[], final TransMatrix kTM,
            final boolean clip) {
        AlgorithmCubicLagrangian CLag = new AlgorithmCubicLagrangian();
        int i, j, k, l;
        int iAdj, jAdj, kAdj;
        double X, Y, Z;
        double value[] = new double[4];
        int temp4;
        double imm, jmm, kmm;
        final int[] inVolExtents = {iXdim, iYdim, iZdim};
        int oSliceSize;
        int oVolSize;
        final int mod = Math.max(1, oTdim / 50);

        double i1, i2, i3, j1, j2, j3;
        double temp1, temp2, temp3;
        double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        float argbMax = 255.0f;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T03 = (double)kTM.M03;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;
        T13 = (double)kTM.M13;
        T20 = (double)kTM.M20;
        T21 = (double)kTM.M21;
        T22 = (double)kTM.M22;
        T23 = (double)kTM.M23;

        oSliceSize = oXdim * oYdim;
        oVolSize = oSliceSize * oZdim;

        if (srcImage.getType() == ModelStorageBase.ARGB) {
            argbMax = 255.0f;
        } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
            argbMax = 65535.0f;
        }

        for (l = 0; (l < oTdim) && !threadStopped; l++) {

            if ( ( (l % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) l / oTdim * 100) + .5));
            }

            CLag.setup3DCubicLagrangianC(imgBuf, inVolExtents, argbMax, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                if (pad) {
                    iAdj = i - AlgorithmTransform.margins[0];
                } else {
                    iAdj = i;
                }
                imm = iAdj * oXres;
                i1 = (imm * T00) + T03;
                i2 = (imm * T10) + T13;
                i3 = (imm * T20) + T23;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {
                    if (pad) {
                        jAdj = j - AlgorithmTransform.margins[1];
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

                        // convert to mm
                        value[0] = fillValue; // if voxel is transformed out of bounds
                        value[1] = fillValue;
                        value[2] = fillValue;
                        value[3] = fillValue;
                        if (pad) {
                            kAdj = k - AlgorithmTransform.margins[2];
                        } else {
                            kAdj = k;
                        }
                        kmm = kAdj * oZres;

                        // transform i,j,k
                        X = (temp3 + (kmm * T02)) / iXres;

                        if ( (X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (kmm * T12)) / iYres;

                            if ( (Y >= 0) && (Y < iYdim)) {
                                Z = (temp1 + (kmm * T22)) / iZres;

                                if ( (Z >= 0) && (Z < iZdim)) {
                                    value = CLag.cubicLagrangian3DC(X, Y, Z);
                                }
                            }
                        }

                        temp4 = 4 * (i + (j * oXdim) + (k * oSliceSize));
                        imgBuffer2[temp4] = (float)value[0];
                        imgBuffer2[temp4 + 1] = (float)value[1];
                        imgBuffer2[temp4 + 2] = (float)value[2];
                        imgBuffer2[temp4 + 3] = (float)value[3];
                    }
                }
            }

            try {
                destImage.importData(4 * l * oVolSize, imgBuffer2, false);
            } catch (final IOException error) {
                MipavUtil.displayError("AlgorithmTransform: IOException on destImage.importData");
            }

            if (l < (oTdim - 1)) {

                try {
                    srcImage.exportData( (l + 1) * imgLength, imgLength, imgBuf);
                } catch (final IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);

                    return;
                }
            } // end if (l < (oTdim - 1))
        } // for l

        destImage.calcMinMax();
        CLag.finalize();
        CLag = null;
        Preferences.debug("finished cubic Lagrangian", Preferences.DEBUG_ALGORITHM);

    }

    /**
     * Transforms and resamples volume using cubic Lagrangian interpolation Does a slice by slice cubic Lagrangian
     * interpolation on a 4 dimensional object.
     * 
     * @param imgBuf Image array
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformCubicLagrangian4Dim2D(final double[] imgBuf, final TransMatrix kTM, final boolean clip) {
        AlgorithmCubicLagrangian CLag = new AlgorithmCubicLagrangian();
        int i, j, k, l;
        int iAdj, jAdj;
        double X, Y;
        double value;
        double imm, jmm;
        final int[] inVolExtents = {iXdim, iYdim};

        double temp1, temp2;
        double T00, T01, T02, T10, T11, T12;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged((int) ( ((float) l / oTdim * 100) + 0.5));

            for (k = 0; (k < oZdim) && !threadStopped; k++) {
                CLag.setup2DCubicLagrangian(imgBuf, inVolExtents, clip);

                for (i = 0; (i < oXdim) && !threadStopped; i++) {
                    if (pad) {
                        iAdj = i - AlgorithmTransform.margins[0];
                    } else {
                        iAdj = i;
                    }
                    imm = iAdj * oXres;
                    temp1 = (imm * T00) + T02;
                    temp2 = (imm * T10) + T12;

                    for (j = 0; (j < oYdim) && !threadStopped; j++) {

                        // convert to mm
                        value = fillValue; // if voxel is transformed out of bounds
                        if (pad) {
                            jAdj = j - AlgorithmTransform.margins[1];
                        } else {
                            jAdj = j;
                        }
                        jmm = jAdj * oYres;

                        // transform i,j
                        X = (temp1 + (jmm * T01)) / iXres;

                        if ( (X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (jmm * T11)) / iYres;

                            if ( (Y >= 0) && (Y < iYdim)) {
                                value = CLag.cubicLagrangian2D(X, Y);
                            }
                        }

                        destImage.set(i, j, k, l, value);
                    }
                }

                if ( (k < (oZdim - 1)) || (l < (oTdim - 1))) {

                    try {
                        srcImage.exportData( (l * oZdim * imgLength) + ( (k + 1) * imgLength), imgLength, imgBuf);
                    } catch (final IOException error) {
                        displayError("Algorithm Transform: Image(s) locked");
                        setCompleted(false);

                        return;
                    }
                } // end if ((k < (oZdim - 1))|| (l < (oTdim - 1)))
            } // end for k
        } // end for l

        CLag.finalize();
        CLag = null;
        Preferences.debug("finished cubic Lagrangian", Preferences.DEBUG_ALGORITHM);
    }

    /**
     * Transforms and resamples volume using cubic Lagrangian interpolation This version used with color images This
     * version performs a slice by slice algorithm on a 4 dimensional object.
     * 
     * @param imgBuf Input image array
     * @param imgBuf2 Output image array
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformCubicLagrangian4Dim2DC(final double[] imgBuf, final float[] imgBuf2, final TransMatrix kTM,
            final boolean clip) {
        AlgorithmCubicLagrangian CLag = new AlgorithmCubicLagrangian();
        int i, j, k, l;
        int iAdj, jAdj;
        double X, Y;
        double[] value = new double[4];
        double imm, jmm;
        final int[] inVolExtents = {iXdim, iYdim};

        double temp1, temp2;
        int temp3;
        double T00, T01, T02, T10, T11, T12;
        float argbMax = 255.0f;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged((int) ( ((float) l / oTdim * 100) + 0.5));

            for (k = 0; (k < oZdim) && !threadStopped; k++) {

                if (srcImage.getType() == ModelStorageBase.ARGB) {
                    argbMax = 255.0f;
                } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
                    argbMax = 65535.0f;
                }

                CLag.setup2DCubicLagrangianC(imgBuf, inVolExtents, argbMax, clip);

                for (i = 0; (i < oXdim) && !threadStopped; i++) {
                    if (pad) {
                        iAdj = i - AlgorithmTransform.margins[0];
                    } else {
                        iAdj = i;
                    }
                    imm = iAdj * oXres;
                    temp1 = (imm * T00) + T02;
                    temp2 = (imm * T10) + T12;

                    for (j = 0; (j < oYdim) && !threadStopped; j++) {

                        // convert to mm
                        value[0] = fillValue; // if voxel is transformed out of bounds
                        value[1] = fillValue;
                        value[2] = fillValue;
                        value[3] = fillValue;
                        if (pad) {
                            jAdj = j - AlgorithmTransform.margins[1];
                        } else {
                            jAdj = j;
                        }
                        jmm = jAdj * oYres;

                        // transform i,j
                        X = (temp1 + (jmm * T01)) / iXres;

                        if ( (X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (jmm * T11)) / iYres;

                            if ( (Y >= 0) && (Y < iYdim)) {
                                value = CLag.cubicLagrangian2DC(X, Y);
                            }
                        }

                        temp3 = 4 * (i + (j * oXdim));
                        imgBuf2[temp3] = (float)value[0];
                        imgBuf2[temp3 + 1] = (float)value[1];
                        imgBuf2[temp3 + 2] = (float)value[2];
                        imgBuf2[temp3 + 3] = (float)value[3];
                    } // for i
                } // for j

                try {
                    destImage.importData( (4 * l * oXdim * oYdim * oZdim) + (4 * k * oXdim * oYdim), imgBuf2, true);
                } catch (final IOException error) {
                    MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
                }

                if ( (k < (oZdim - 1)) || (l < (oTdim - 1))) {

                    try {
                        srcImage.exportData( (l * oZdim * imgLength) + ( (k + 1) * imgLength), imgLength, imgBuf);
                    } catch (final IOException error) {
                        displayError("Algorithm Transform: IOException Error on importData");
                        setCompleted(false);

                        return;
                    }
                } // end if ((k < (oZdim - 1))|| (l < (oTdim - 1)))
            } // end for k
        } // end for l

        CLag.finalize();
        CLag = null;
        Preferences.debug("finished cubic Lagrangian", Preferences.DEBUG_ALGORITHM);
    }

    /**
     * Transforms and resamples volume using heptic Lagrangian interpolation.
     * 
     * @param imgBuf Image array
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformHepticLagrangian2D(final double[] imgBuf, final TransMatrix kTM, final boolean clip) {
        AlgorithmHepticLagrangian HLag = new AlgorithmHepticLagrangian();
        int i, j;
        int iAdj, jAdj;
        double X, Y;
        double value;
        double imm, jmm;
        final int[] inVolExtents = {iXdim, iYdim};
        final int mod = Math.max(1, oXdim / 50);
        double temp1, temp2;
        double T00, T01, T02, T10, T11, T12;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        HLag.setup2DHepticLagrangian(imgBuf, inVolExtents, clip);

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if ( ( (i % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) i / oXdim * 100) + 0.5));
            }

            if (pad) {
                iAdj = i - AlgorithmTransform.margins[0];
            } else {
                iAdj = i;
            }

            imm = iAdj * oXres;
            temp1 = (imm * T00) + T02;
            temp2 = (imm * T10) + T12;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                // convert to mm
                value = fillValue; // if voxel is tranformed out of bounds
                if (pad) {
                    jAdj = j - AlgorithmTransform.margins[1];
                } else {
                    jAdj = j;
                }
                jmm = jAdj * oYres;

                // transform i,j
                X = (temp1 + (jmm * T01)) / iXres;

                if ( (X >= 0) && (X < iXdim)) { // check bounds
                    Y = (temp2 + (jmm * T11)) / iYres;

                    if ( (Y >= 0) && (Y < iYdim)) {
                        value = HLag.hepticLagrangian2D(X, Y);
                    }
                }

                destImage.set(i, j, value);
            }
        }

        HLag.finalize();
        HLag = null;
        Preferences.debug("finished heptic Lagrangian", Preferences.DEBUG_ALGORITHM);
    }

    /**
     * Transforms and resamples volume using heptic Lagrangian interpolation This version used with color images.
     * 
     * @param imgBuf Input image array
     * @param imgBuf2 Output image array
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformHepticLagrangian2DC(final double[] imgBuf, final float[] imgBuf2, final TransMatrix kTM,
            final boolean clip) {
        AlgorithmHepticLagrangian HLag = new AlgorithmHepticLagrangian();
        int i, j;
        int iAdj, jAdj;
        double X, Y;
        double[] value = new double[4];
        double imm, jmm;
        final int[] inVolExtents = {iXdim, iYdim};
        final int mod = Math.max(1, oXdim / 50);
        double temp1, temp2;
        int temp3;
        double T00, T01, T02, T10, T11, T12;
        float argbMax = 255.0f;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        if (srcImage.getType() == ModelStorageBase.ARGB) {
            argbMax = 255.0f;
        } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
            argbMax = 65535.0f;
        }

        HLag.setup2DHepticLagrangianC(imgBuf, inVolExtents, argbMax, clip);

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if ( ( (i % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) i / oXdim * 100) + 0.5));
            }

            if (pad) {
                iAdj = i - AlgorithmTransform.margins[0];
            } else {
                iAdj = i;
            }

            imm = iAdj * oXres;
            temp1 = (imm * T00) + T02;
            temp2 = (imm * T10) + T12;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                // convert to mm
                value[0] = fillValue; // if voxel is transformed out of bounds
                value[1] = fillValue;
                value[2] = fillValue;
                value[3] = fillValue;
                if (pad) {
                    jAdj = j - AlgorithmTransform.margins[1];
                } else {
                    jAdj = j;
                }
                jmm = jAdj * oYres;

                // transform i,j
                X = (temp1 + (jmm * T01)) / iXres;

                if ( (X >= 0) && (X < iXdim)) { // check bounds
                    Y = (temp2 + (jmm * T11)) / iYres;

                    if ( (Y >= 0) && (Y < iYdim)) {
                        value = HLag.hepticLagrangian2DC(X, Y);
                    }
                }

                temp3 = 4 * (i + (j * oXdim));
                imgBuf2[temp3] = (float)value[0];
                imgBuf2[temp3 + 1] = (float)value[1];
                imgBuf2[temp3 + 2] = (float)value[2];
                imgBuf2[temp3 + 3] = (float)value[3];
            }
        }

        if (threadStopped) {
            return;
        }

        try {
            destImage.importData(0, imgBuf2, true);
        } catch (final IOException error) {
            MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
        }

        HLag.finalize();
        HLag = null;
        Preferences.debug("finished heptic Lagrangian", Preferences.DEBUG_ALGORITHM);

    }

    /**
     * Transforms and resamples volume using heptic Lagrangian interpolation.
     * 
     * @param imgBuf Image array
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformHepticLagrangian3D(final double[] imgBuf, final TransMatrix kTM, final boolean clip) {
        AlgorithmHepticLagrangian HLag = new AlgorithmHepticLagrangian();
        int i, j, k;
        int iAdj, jAdj, kAdj;
        double X, Y, Z;
        double value;
        double imm, jmm, kmm;
        final int[] inVolExtents = {iXdim, iYdim, iZdim};
        final int mod = Math.max(1, oXdim / 50);

        double i1, i2, i3, j1, j2, j3;
        double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T03 = (double)kTM.M03;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;
        T13 = (double)kTM.M13;
        T20 = (double)kTM.M20;
        T21 = (double)kTM.M21;
        T22 = (double)kTM.M22;
        T23 = (double)kTM.M23;

        HLag.setup3DHepticLagrangian(imgBuf, inVolExtents, clip);

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if ( ( (i % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) i / oXdim * 100) + 0.5));
            }

            if (pad) {
                iAdj = i - AlgorithmTransform.margins[0];
            } else {
                iAdj = i;
            }
            imm = iAdj * oXres;
            i1 = (imm * T00) + T03;
            i2 = (imm * T10) + T13;
            i3 = (imm * T20) + T23;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {
                if (pad) {
                    jAdj = j - AlgorithmTransform.margins[1];
                } else {
                    jAdj = j;
                }
                jmm = jAdj * oYres;
                j1 = (jmm * T01) + i1;
                j2 = (jmm * T11) + i2;
                j3 = (jmm * T21) + i3;

                for (k = 0; (k < oZdim) && !threadStopped; k++) {

                    // convert to mm
                    value = fillValue; // if voxel is transformed out of bounds
                    if (pad) {
                        kAdj = k - AlgorithmTransform.margins[2];
                    } else {
                        kAdj = k;
                    }
                    kmm = kAdj * oZres;

                    // transform i,j,k
                    X = (j1 + (kmm * T02)) / iXres;

                    if ( (X >= 0) && (X < iXdim)) { // check bounds
                        Y = (j2 + (kmm * T12)) / iYres;

                        if ( (Y >= 0) && (Y < iYdim)) {
                            Z = (j3 + (kmm * T22)) / iZres;

                            if ( (Z >= 0) && (Z < iZdim)) {
                                value = HLag.hepticLagrangian3D(X, Y, Z);
                            }
                        }
                    }

                    destImage.set(i, j, k, value);
                }
            }
        }

        HLag.finalize();
        HLag = null;
        Preferences.debug("finished heptic Lagrangian", Preferences.DEBUG_ALGORITHM);
    }

    /**
     * Transforms and resamples volume using heptic Lagrangian interpolation This version used with color images.
     * 
     * @param imgBuf Input image array
     * @param imgBuf2 Output image array
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformHepticLagrangian3DC(final double[] imgBuf, final float[] imgBuf2, final TransMatrix kTM,
            final boolean clip) {
        AlgorithmHepticLagrangian HLag = new AlgorithmHepticLagrangian();
        int i, j, k;
        int iAdj, jAdj, kAdj;
        double X, Y, Z;
        double[] value = new double[4];
        double imm, jmm, kmm;
        final int[] inVolExtents = {iXdim, iYdim, iZdim};
        final int mod = Math.max(1, oXdim / 50);

        final int osliceSize = oXdim * oYdim;
        double i1, i2, i3, j1, j2, j3;
        double temp1, temp2, temp3;
        int temp4;
        double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        float argbMax = 255.0f;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T03 = (double)kTM.M03;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;
        T13 = (double)kTM.M13;
        T20 = (double)kTM.M20;
        T21 = (double)kTM.M21;
        T22 = (double)kTM.M22;
        T23 = (double)kTM.M23;

        if (srcImage.getType() == ModelStorageBase.ARGB) {
            argbMax = 255.0f;
        } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
            argbMax = 65535.0f;
        }

        HLag.setup3DHepticLagrangianC(imgBuf, inVolExtents, argbMax, clip);

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if ( ( (i % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) i / oXdim * 100) + 0.5));
            }

            if (pad) {
                iAdj = i - AlgorithmTransform.margins[0];
            } else {
                iAdj = i;
            }
            imm = iAdj * oXres;
            i1 = (imm * T00) + T03;
            i2 = (imm * T10) + T13;
            i3 = (imm * T20) + T23;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {
                if (pad) {
                    jAdj = j - AlgorithmTransform.margins[1];
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

                    // convert to mm
                    value[0] = fillValue; // if voxel is transformed out of bounds
                    value[1] = fillValue;
                    value[2] = fillValue;
                    value[3] = fillValue;
                    if (pad) {
                        kAdj = k - AlgorithmTransform.margins[2];
                    } else {
                        kAdj = k;
                    }
                    kmm = kAdj * oZres;

                    // transform i,j,k
                    X = (temp3 + (kmm * T02)) / iXres;

                    // convert back to pixels
                    if ( (X >= 0) && (X < iXdim)) { // check bounds
                        Y = (temp2 + (kmm * T12)) / iYres;

                        if ( (Y >= 0) && (Y < iYdim)) {
                            Z = (temp1 + (kmm * T22)) / iZres;

                            if ( (Z >= 0) && (Z < iZdim)) {
                                value = HLag.hepticLagrangian3DC(X, Y, Z);
                            }
                        }
                    }

                    temp4 = 4 * (i + (j * oXdim) + (k * osliceSize));
                    imgBuf2[temp4] = (float)value[0];
                    imgBuf2[temp4 + 1] = (float)value[1];
                    imgBuf2[temp4 + 2] = (float)value[2];
                    imgBuf2[temp4 + 3] = (float)value[3];
                }
            }
        }

        if (threadStopped) {
            return;
        }

        try {
            destImage.importData(0, imgBuf2, true);
        } catch (final IOException error) {
            MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
        }

        HLag.finalize();
        HLag = null;
        Preferences.debug("finished heptic Lagrangian", Preferences.DEBUG_ALGORITHM);

    }

    /**
     * Transforms and resamples volume using heptic Lagrangian interpolation Does a slice by slice heptic Lagrangian
     * interpolation on a 3 dimensional object.
     * 
     * @param imgBuf Image array
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformHepticLagrangian3Dim2D(final double[] imgBuf, final TransMatrix kTM, final boolean clip) {
        AlgorithmHepticLagrangian HLag = new AlgorithmHepticLagrangian();
        int i, j, k;
        int iAdj, jAdj;
        double X, Y;
        double value;
        double imm, jmm;
        final int[] inVolExtents = {iXdim, iYdim};

        final int mod = Math.max(1, oZdim / 50);
        double temp1, temp2;
        double T00, T01, T02, T10, T11, T12;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        for (k = 0; (k < oZdim) && !threadStopped; k++) {

            if ( ( (k % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) k / oZdim * 100) + 0.5));
            }

            HLag.setup2DHepticLagrangian(imgBuf, inVolExtents, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                if (pad) {
                    iAdj = i - AlgorithmTransform.margins[0];
                } else {
                    iAdj = i;
                }
                imm = iAdj * oXres;
                temp1 = (imm * T00) + T02;
                temp2 = (imm * T10) + T12;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {

                    // convert to mm
                    value = fillValue; // if voxel is transformed out of bounds
                    if (pad) {
                        jAdj = j - AlgorithmTransform.margins[1];
                    } else {
                        jAdj = j;
                    }
                    jmm = jAdj * oYres;

                    // transform i,j
                    X = (temp1 + (jmm * T01)) / iXres;

                    if ( (X >= 0) && (X < iXdim)) { // check bounds
                        Y = (temp2 + (jmm * T11)) / iYres;

                        if ( (Y >= 0) && (Y < iYdim)) {
                            value = HLag.hepticLagrangian2D(X, Y);
                        }
                    }

                    destImage.set(i, j, k, value);
                }
            }

            if (k < (oZdim - 1)) {

                try {
                    srcImage.exportData( (k + 1) * imgLength, imgLength, imgBuf);
                } catch (final IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);

                    return;
                }
            } // end if (k < (oZdim - 1))
        } // end for k

        HLag.finalize();
        HLag = null;
        Preferences.debug("finished heptic Lagrangian", Preferences.DEBUG_ALGORITHM);
    }

    /**
     * Transforms and resamples volume using heptic Lagrangian interpolation This version used with color images This
     * version performs a slice by slice algorithm on a 3 dimensional object.
     * 
     * @param imgBuf Input image array
     * @param imgBuf2 Output image array
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformHepticLagrangian3Dim2DC(final double[] imgBuf, final float[] imgBuf2, final TransMatrix kTM,
            final boolean clip) {
        AlgorithmHepticLagrangian HLag = new AlgorithmHepticLagrangian();
        int i, j, k;
        int iAdj, jAdj;
        double X, Y;
        double[] value = new double[4];
        double imm, jmm;
        final int[] inVolExtents = {iXdim, iYdim};

        final int mod = Math.max(1, oZdim / 50);
        double temp1, temp2;
        int temp3;
        double T00, T01, T02, T10, T11, T12;
        float argbMax = 255.0f;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        for (k = 0; (k < oZdim) && !threadStopped; k++) {

            if ( ( (k % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) k / oZdim * 100) + 0.5));
            }

            if (srcImage.getType() == ModelStorageBase.ARGB) {
                argbMax = 255.0f;
            } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
                argbMax = 65535.0f;
            }

            HLag.setup2DHepticLagrangianC(imgBuf, inVolExtents, argbMax, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                if (pad) {
                    iAdj = i - AlgorithmTransform.margins[0];
                } else {
                    iAdj = i;
                }
                imm = iAdj * oXres;
                temp1 = (imm * T00) + T02;
                temp2 = (imm * T10) + T12;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {

                    // convert to mm
                    value[0] = fillValue; // if voxel is transformed out of bounds
                    value[1] = fillValue;
                    value[2] = fillValue;
                    value[3] = fillValue;
                    if (pad) {
                        jAdj = j - AlgorithmTransform.margins[1];
                    } else {
                        jAdj = j;
                    }
                    jmm = jAdj * oYres;

                    // transform i,j
                    X = (temp1 + (jmm * T01)) / iXres;

                    if ( (X >= 0) && (X < iXdim)) { // check bounds
                        Y = (temp2 + (jmm * T11)) / iYres;

                        if ( (Y >= 0) && (Y < iYdim)) {
                            value = HLag.hepticLagrangian2DC(X, Y);
                        }
                    }

                    temp3 = 4 * (i + (j * oXdim));
                    imgBuf2[temp3] = (float)value[0];
                    imgBuf2[temp3 + 1] = (float)value[1];
                    imgBuf2[temp3 + 2] = (float)value[2];
                    imgBuf2[temp3 + 3] = (float)value[3];
                } // for i
            } // for j

            try {
                destImage.importData(4 * k * oXdim * oYdim, imgBuf2, true);
            } catch (final IOException error) {
                MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
            }

            if (k < (oZdim - 1)) {

                try {
                    srcImage.exportData( (k + 1) * imgLength, imgLength, imgBuf);
                } catch (final IOException error) {
                    displayError("Algorithm Transform: IOException Error on importData");
                    setCompleted(false);

                    return;
                }
            } // end if (k < (oZdim - 1))
        } // end for k

        HLag.finalize();
        HLag = null;
        Preferences.debug("finished heptic Lagrangian", Preferences.DEBUG_ALGORITHM);
    }

    /**
     * transforms and resamples 4 dimensional object using 3D heptic Lagrangian interpolation.
     * 
     * @param imgBuf image array
     * @param kTM transformation matrix to be applied
     * @param clip if true clip output values to be within input range
     */
    private void transformHepticLagrangian4D(final double[] imgBuf, final TransMatrix kTM, final boolean clip) {
        AlgorithmHepticLagrangian HLag = new AlgorithmHepticLagrangian();
        int i, j, k, l;
        int iAdj, jAdj, kAdj;
        double X, Y, Z;
        double value;
        double imm, jmm, kmm;
        final int[] inVolExtents = {iXdim, iYdim, iZdim};
        final int mod = Math.max(1, oTdim / 50);

        double i1, i2, i3, j1, j2, j3;
        double temp1, temp2, temp3;
        double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T03 = (double)kTM.M03;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;
        T13 = (double)kTM.M13;
        T20 = (double)kTM.M20;
        T21 = (double)kTM.M21;
        T22 = (double)kTM.M22;
        T23 = (double)kTM.M23;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {

            if ( ( (l % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) l / oTdim * 100) + .5));
            }

            HLag.setup3DHepticLagrangian(imgBuf, inVolExtents, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                if (pad) {
                    iAdj = i - AlgorithmTransform.margins[0];
                } else {
                    iAdj = i;
                }
                imm = iAdj * oXres;
                i1 = (imm * T00) + T03;
                i2 = (imm * T10) + T13;
                i3 = (imm * T20) + T23;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {
                    if (pad) {
                        jAdj = j - AlgorithmTransform.margins[1];
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

                        // convert to mm
                        value = fillValue; // if voxel transformed out of bounds
                        if (pad) {
                            kAdj = k - AlgorithmTransform.margins[2];
                        } else {
                            kAdj = k;
                        }
                        kmm = kAdj * oZres;

                        // transform i,j,k
                        X = (temp3 + (kmm * T02)) / iXres;

                        if ( (X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (kmm * T12)) / iYres;

                            if ( (Y >= 0) && (Y < iYdim)) {
                                Z = (temp1 + (kmm * T22)) / iZres;

                                if ( (Z >= 0) && (Z < iZdim)) {
                                    value = HLag.hepticLagrangian3D(X, Y, Z);
                                }
                            }
                        }

                        destImage.set(i, j, k, l, value);
                    }
                }
            }

            if (l < (oTdim - 1)) {

                try {
                    srcImage.exportData( (l + 1) * imgLength, imgLength, imgBuf);
                } catch (final IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);

                    return;
                }
            } // end if (l < (oTdim - 1))
        } // for l

        HLag.finalize();
        HLag = null;
        Preferences.debug("finished heptic Lagrangian", Preferences.DEBUG_ALGORITHM);
    }

    /**
     * transforms and resamples 4 dimensional object using 3D heptic Lagrangian interpolation.
     * 
     * @param imgBuf image array
     * @param imgBuffer2
     * @param kTM transformation matrix to be applied
     * @param clip if true clip output values to be within input range
     */
    private void transformHepticLagrangian4DC(final double[] imgBuf, final float imgBuffer2[], final TransMatrix kTM,
            final boolean clip) {
        AlgorithmHepticLagrangian HLag = new AlgorithmHepticLagrangian();
        int i, j, k, l;
        int iAdj, jAdj, kAdj;
        double X, Y, Z;
        double value[] = new double[4];
        int temp4;
        double imm, jmm, kmm;
        final int[] inVolExtents = {iXdim, iYdim, iZdim};
        int oSliceSize;
        int oVolSize;
        final int mod = Math.max(1, oTdim / 50);

        double i1, i2, i3, j1, j2, j3;
        double temp1, temp2, temp3;
        double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        float argbMax = 255.0f;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T03 = (double)kTM.M03;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;
        T13 = (double)kTM.M13;
        T20 = (double)kTM.M20;
        T21 = (double)kTM.M21;
        T22 = (double)kTM.M22;
        T23 = (double)kTM.M23;

        oSliceSize = oXdim * oYdim;
        oVolSize = oSliceSize * oZdim;

        if (srcImage.getType() == ModelStorageBase.ARGB) {
            argbMax = 255.0f;
        } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
            argbMax = 65535.0f;
        }

        for (l = 0; (l < oTdim) && !threadStopped; l++) {

            if ( ( (l % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) l / oTdim * 100) + .5));
            }

            HLag.setup3DHepticLagrangianC(imgBuf, inVolExtents, argbMax, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                if (pad) {
                    iAdj = i - AlgorithmTransform.margins[0];
                } else {
                    iAdj = i;
                }
                imm = iAdj * oXres;
                i1 = (imm * T00) + T03;
                i2 = (imm * T10) + T13;
                i3 = (imm * T20) + T23;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {
                    if (pad) {
                        jAdj = j - AlgorithmTransform.margins[1];
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

                        // convert to mm
                        value[0] = fillValue; // if voxel transformed out of bounds
                        value[1] = fillValue;
                        value[2] = fillValue;
                        value[3] = fillValue;
                        if (pad) {
                            kAdj = k - AlgorithmTransform.margins[2];
                        } else {
                            kAdj = k;
                        }
                        kmm = kAdj * oZres;

                        // transform i,j,k
                        X = (temp3 + (kmm * T02)) / iXres;

                        if ( (X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (kmm * T12)) / iYres;

                            if ( (Y >= 0) && (Y < iYdim)) {
                                Z = (temp1 + (kmm * T22)) / iZres;

                                if ( (Z >= 0) && (Z < iZdim)) {
                                    value = HLag.hepticLagrangian3DC(X, Y, Z);
                                }
                            }
                        }

                        temp4 = 4 * (i + (j * oXdim) + (k * oSliceSize));
                        imgBuffer2[temp4] = (float)value[0];
                        imgBuffer2[temp4 + 1] = (float)value[1];
                        imgBuffer2[temp4 + 2] =(float)value[2];
                        imgBuffer2[temp4 + 3] = (float)value[3];
                    }
                }
            }

            try {
                destImage.importData(4 * l * oVolSize, imgBuffer2, false);
            } catch (final IOException error) {
                MipavUtil.displayError("AlgorithmTransform: IOException on destImage.importData");
            }

            if (l < (oTdim - 1)) {

                try {
                    srcImage.exportData( (l + 1) * imgLength, imgLength, imgBuf);
                } catch (final IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);

                    return;
                }
            } // end if (l < (oTdim - 1))
        } // for l

        destImage.calcMinMax();
        HLag.finalize();
        HLag = null;
        Preferences.debug("finished heptic Lagrangian", Preferences.DEBUG_ALGORITHM);
    }

    /**
     * Transforms and resamples volume using heptic Lagrangian interpolation Does a slice by slice heptic Lagrangian
     * interpolation on a 4 dimensional object.
     * 
     * @param imgBuf Image array
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformHepticLagrangian4Dim2D(final double[] imgBuf, final TransMatrix kTM, final boolean clip) {
        AlgorithmHepticLagrangian HLag = new AlgorithmHepticLagrangian();
        int i, j, k, l;
        int iAdj, jAdj;
        double X, Y;
        double value;
        double imm, jmm;
        final int[] inVolExtents = {iXdim, iYdim};

        double temp1, temp2;
        double T00, T01, T02, T10, T11, T12;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged((int) ( ((float) l / oTdim * 100) + 0.5));

            for (k = 0; (k < oZdim) && !threadStopped; k++) {
                HLag.setup2DHepticLagrangian(imgBuf, inVolExtents, clip);

                for (i = 0; (i < oXdim) && !threadStopped; i++) {
                    if (pad) {
                        iAdj = i - AlgorithmTransform.margins[0];
                    } else {
                        iAdj = i;
                    }
                    imm = iAdj * oXres;
                    temp1 = (imm * T00) + T02;
                    temp2 = (imm * T10) + T12;

                    for (j = 0; (j < oYdim) && !threadStopped; j++) {

                        // convert to mm
                        value = fillValue; // if voxel transformed out of bounds
                        if (pad) {
                            jAdj = j - AlgorithmTransform.margins[1];
                        } else {
                            jAdj = j;
                        }
                        jmm = jAdj * oYres;

                        // transform i,j
                        X = (temp1 + (jmm * T01)) / iXres;

                        if ( (X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (jmm * T11)) / iYres;

                            if ( (Y >= 0) && (Y < iYdim)) {
                                value = HLag.hepticLagrangian2D(X, Y);
                            }
                        }

                        destImage.set(i, j, k, l, value);
                    }
                }

                if ( (k < (oZdim - 1)) || (l < (oTdim - 1))) {

                    try {
                        srcImage.exportData( (l * oZdim * imgLength) + ( (k + 1) * imgLength), imgLength, imgBuf);
                    } catch (final IOException error) {
                        displayError("Algorithm Transform: Image(s) locked");
                        setCompleted(false);

                        return;
                    }
                } // end if ((k < (oZdim - 1))|| (l < (oTdim - 1)))
            } // end for k
        } // end for l

        HLag.finalize();
        HLag = null;
        Preferences.debug("finished heptic Lagrangian", Preferences.DEBUG_ALGORITHM);
    }

    /**
     * Transforms and resamples volume using heptic Lagrangian interpolation This version used with color images This
     * version performs a slice by slice algorithm on a 4 dimensional object.
     * 
     * @param imgBuf Input image array
     * @param imgBuf2 Output image array
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformHepticLagrangian4Dim2DC(final double[] imgBuf, final float[] imgBuf2, final TransMatrix kTM,
            final boolean clip) {
        AlgorithmHepticLagrangian HLag = new AlgorithmHepticLagrangian();
        int i, j, k, l;
        int iAdj, jAdj;
        double X, Y;
        double[] value = new double[4];
        double imm, jmm;
        final int[] inVolExtents = {iXdim, iYdim};

        double temp1, temp2;
        int temp3;
        double T00, T01, T02, T10, T11, T12;
        float argbMax = 255.0f;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged((int) ( ((float) l / oTdim * 100) + 0.5));

            for (k = 0; (k < oZdim) && !threadStopped; k++) {

                if (srcImage.getType() == ModelStorageBase.ARGB) {
                    argbMax = 255.0f;
                } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
                    argbMax = 65535.0f;
                }

                HLag.setup2DHepticLagrangianC(imgBuf, inVolExtents, argbMax, clip);

                for (i = 0; (i < oXdim) && !threadStopped; i++) {
                    if (pad) {
                        iAdj = i - AlgorithmTransform.margins[0];
                    } else {
                        iAdj = i;
                    }
                    imm = iAdj * oXres;
                    temp1 = (imm * T00) + T02;
                    temp2 = (imm * T10) + T12;

                    for (j = 0; (j < oYdim) && !threadStopped; j++) {

                        // convert to mm
                        value[0] = fillValue; // if voxel transformed out of bounds
                        value[1] = fillValue;
                        value[2] = fillValue;
                        value[3] = fillValue;
                        if (pad) {
                            jAdj = j - AlgorithmTransform.margins[1];
                        } else {
                            jAdj = j;
                        }
                        jmm = jAdj * oYres;

                        // transform i,j
                        X = (temp1 + (jmm * T01)) / iXres;

                        if ( (X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (jmm * T11)) / iYres;

                            if ( (Y >= 0) && (Y < iYdim)) {
                                value = HLag.hepticLagrangian2DC(X, Y);
                            }
                        }

                        temp3 = 4 * (i + (j * oXdim));
                        imgBuf2[temp3] = (float)value[0];
                        imgBuf2[temp3 + 1] = (float)value[1];
                        imgBuf2[temp3 + 2] = (float)value[2];
                        imgBuf2[temp3 + 3] = (float)value[3];
                    } // for i
                } // for j

                try {
                    destImage.importData( (4 * l * oXdim * oYdim * oZdim) + (4 * k * oXdim * oYdim), imgBuf2, true);
                } catch (final IOException error) {
                    MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
                }

                if ( (k < (oZdim - 1)) || (l < (oTdim - 1))) {

                    try {
                        srcImage.exportData( (l * oZdim * imgLength) + ( (k + 1) * imgLength), imgLength, imgBuf);
                    } catch (final IOException error) {
                        displayError("Algorithm Transform: IOException Error on importData");
                        setCompleted(false);

                        return;
                    }
                } // end if ((k < (oZdim - 1))|| (l < (oTdim - 1)))
            } // end for k
        } // end for l

        HLag.finalize();
        HLag = null;
        Preferences.debug("finished heptic Lagrangian", Preferences.DEBUG_ALGORITHM);
    }

    /**
     * Transforms and resamples volume using nearest neighbor interpolation.
     * 
     * @param imgBuf image array
     * @param kTM transformation matrix to be applied
     */
    private void transformNearestNeighbor2D(final double[] imgBuf, final TransMatrix kTM) {
        int i, j;
        int iAdj, jAdj;
        double X, Y;
        int roundX, roundY;
        int xOffset, yOffset;
        double value;
        double imm, jmm;
        double i1, i2;
        final double invXRes = 1.0 / iXres;
        final double invYRes = 1.0 / iYres;
        final int mod = Math.max(1, oXdim / 50);
        double T00, T01, T02, T10, T11, T12;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if ( ( (i % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) i / oXdim * 100) + 0.5));
            }

            if (pad) {
                iAdj = i - AlgorithmTransform.margins[0];
            } else {
                iAdj = i;
            }
            imm = iAdj * oXres;
            i1 = (imm * T00) + T02;
            i2 = (imm * T10) + T12;
            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                // transform i,j

                if (pad) {
                    jAdj = j - AlgorithmTransform.margins[1];
                } else {
                    jAdj = j;
                }
                jmm = jAdj * oYres;
                value = fillValue;
                X = (jmm * T01) + i1;
                X = X * invXRes;
                if ((X >= -0.5) && (X < iXdim)) {
                    Y = (jmm * T11) + i2;
                    Y = Y * invYRes;
                    if ((Y >= -0.5) && (Y < iYdim)) {
                        roundX = (int) (X + 0.5);
                        roundY = (int) (Y + 0.5);

                        xOffset = Math.min(roundX, iXdim - 1);
                        yOffset = Math.min(roundY, iYdim - 1) * iXdim;
                        value = imgBuf[xOffset + yOffset];
                    } // if ((Y >= -0.5) && (Y < iYdim))
                } // if ((X >= -0.5) && (X < iXdim))

                destImage.set(i, j, value);
            }
        }
    }

    /**
     * Transforms and resamples volume using nearest neighbor interpolation.
     * 
     * @param imgBuf input image array
     * @param imgBuf2 output image array
     * @param kTM transformation matrix to be applied
     */
    private void transformNearestNeighbor2DC(final double[] imgBuf, final float[] imgBuf2, final TransMatrix kTM) {
        int i, j;
        int iAdj, jAdj;
        double X, Y;
        int roundX, roundY;
        int xOffset, yOffset;
        double imm, jmm;
        double i1, i2;
        float aValue;
        float rValue;
        float gValue;
        float bValue;
        final double invXRes = 1.0 / iXres;
        final double invYRes = 1.0 / iYres;
        int inputSum;
        int outputSum;
        final int mod = Math.max(1, oXdim / 50);
        double T00, T01, T02, T10, T11, T12;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if ( ( (i % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) i / oXdim * 100) + 0.5));
            }

            if (pad) {
                iAdj = i - AlgorithmTransform.margins[0];
            } else {
                iAdj = i;
            }
            imm = iAdj * oXres;
            i1 = (imm * T00) + T02;
            i2 = (imm * T10) + T12;
            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                // transform i,j
                if (pad) {
                    jAdj = j - AlgorithmTransform.margins[1];
                } else {
                    jAdj = j;
                }
                jmm = jAdj * oYres;
                aValue = fillValue;
                rValue = fillValue;
                gValue = fillValue;
                bValue = fillValue;
                X = (jmm * T01) + i1;
                X = X * invXRes;
                if ((X >= -0.5) && (X < iXdim)) {
                    Y = (jmm * T11) + i2;
                    Y = Y * invYRes;
                    if ((Y >= -0.5) && (Y < iYdim)) {
                        roundX = (int) (X + 0.5);
                        roundY = (int) (Y + 0.5);

                        xOffset = Math.min(roundX, iXdim - 1);
                        yOffset = Math.min(roundY, iYdim - 1) * iXdim;
                        inputSum = 4*(xOffset + yOffset);
                        aValue = (float)imgBuf[inputSum];
                        rValue = (float)imgBuf[inputSum+1];
                        gValue = (float)imgBuf[inputSum+2];
                        bValue = (float)imgBuf[inputSum+3];
                    } // if ((Y >= -0.5) && (Y < iYdim))
                } // if ((X >= -0.5) && (X < iXdim))
                outputSum = 4*(i + (oXdim *j));
                imgBuf2[outputSum] = aValue;
                imgBuf2[outputSum+1] = rValue;
                imgBuf2[outputSum+2] = gValue;
                imgBuf2[outputSum+3] = bValue;
                
            }
        }

        if (threadStopped) {
            return;
        }

        try {
            destImage.importData(0, imgBuf2, true);
        } catch (final IOException error) {
            MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
        }

    }

    /**
     * Transforms and resamples volume using nearest neighbor interpolation.
     * 
     * @param imgBuf image array
     * @param kTM transformation matrix to be applied
     */
    private void transformNearestNeighbor3D(final double[] imgBuf, final TransMatrix kTM) {
        int i, j, k;
        int iAdj, jAdj, kAdj;
        double X, Y, Z;
        int xOffset, yOffset, zOffset;
        double value;
        int sliceSize;
        int roundX, roundY, roundZ;
        double imm, jmm, kmm;
        double i1, i2, i3, j1, j2, j3;
        final double invXRes = 1.0 / iXres;
        final double invYRes = 1.0 / iYres;
        final double invZRes = 1.0 / iZres;
        
        final int mod = Math.max(1, oXdim / 50);

        sliceSize = iXdim * iYdim;

        double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T03 = (double)kTM.M03;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;
        T13 = (double)kTM.M13;
        T20 = (double)kTM.M20;
        T21 = (double)kTM.M21;
        T22 = (double)kTM.M22;
        T23 = (double)kTM.M23;

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if ( ( (i % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) i / oXdim * 100) + 0.5));
            }

            if (pad) {
                iAdj = i - AlgorithmTransform.margins[0];
            } else {
                iAdj = i;
            }
            imm = iAdj * oXres;
            i1 = (imm * T00) + T03;
            i2 = (imm * T10) + T13;
            i3 = (imm * T20) + T23;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {
                if (pad) {
                    jAdj = j - AlgorithmTransform.margins[1];
                } else {
                    jAdj = j;
                }
                jmm = jAdj * oYres;
                j1 = (jmm * T01) + i1;
                j2 = (jmm * T11) + i2;
                j3 = (jmm * T21) + i3;

                for (k = 0; (k < oZdim) && !threadStopped; k++) {

                    // transform i,j,k
                    if (pad) {
                        kAdj = k - AlgorithmTransform.margins[2];
                    } else {
                        kAdj = k;
                    }
                    kmm = kAdj * oZres;
                    value = fillValue;
                    X = (kmm * T02) + j1;
                    X = X * invXRes;
                    if ((X >= -0.5) && (X < iXdim)) {
                        Y = (kmm * T12) + j2;
                        Y = Y * invYRes;
                        if ((Y >= -0.5) && (Y < iYdim)) {
                            Z = (kmm * T22) + j3;
                            Z = Z * invZRes;
                            if ((Z >= -0.5) && (Z < iZdim)) {
        
                                roundX = (int) (X + 0.5);
                                roundY = (int) (Y + 0.5);
                                roundZ = (int) (Z + 0.5);
                            
                                xOffset = Math.min(roundX, iXdim - 1);
                                yOffset = Math.min(roundY, iYdim - 1) * iXdim;
                                zOffset = Math.min(roundZ, iZdim - 1) * sliceSize;
                                value = imgBuf[xOffset + yOffset + zOffset];
                            } // if ((Z >= -0.5) && (Z < iZdim))
                        } // if ((Y >= -0.5) && (Y < iYdim))
                    } // if ((X >= -0.5) && (X < iXdim))

                    destImage.set(i, j, k, value);
                }
            }
        }
    }

    /**
     * Transforms and resamples volume using nearest neighbor interpolation.
     * 
     * @param imgBuf input image array
     * @param imgBuf2 output image array
     * @param kTM transformation matrix to be applied
     */
    private void transformNearestNeighbor3DC(final double[] imgBuf, final float[] imgBuf2, final TransMatrix kTM) {
        int i, j, k;
        int iAdj, jAdj, kAdj;
        double X, Y, Z;
        int roundX, roundY, roundZ;
        int xOffset, yOffset, zOffset;
        int sliceSize;
        int outSliceSize;
        double imm, jmm, kmm;
        double i1, i2, i3, j1, j2, j3;
        float aValue;
        float rValue;
        float gValue;
        float bValue;
        final double invXRes = 1.0 / iXres;
        final double invYRes = 1.0 / iYres;
        final double invZRes = 1.0 / iZres;
        int inputSum;
        int outputSum;
        final int mod = Math.max(1, oXdim / 50);

        sliceSize = iXdim * iYdim;
        outSliceSize = oXdim * oYdim;

        double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T03 = (double)kTM.M03;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;
        T13 = (double)kTM.M13;
        T20 = (double)kTM.M20;
        T21 = (double)kTM.M21;
        T22 = (double)kTM.M22;
        T23 = (double)kTM.M23;

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if ( ( (i % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) i / oXdim * 100) + 0.5));
            }
            if (pad) {
                iAdj = i - AlgorithmTransform.margins[0];
            } else {
                iAdj = i;
            }
            imm = iAdj * oXres;
            i1 = (imm * T00) + T03;
            i2 = (imm * T10) + T13;
            i3 = (imm * T20) + T23;
            for (j = 0; (j < oYdim) && !threadStopped; j++) {
                if (pad) {
                    jAdj = j - AlgorithmTransform.margins[1];
                } else {
                    jAdj = j;
                }
                jmm = jAdj * oYres;
                j1 = (jmm * T01) + i1;
                j2 = (jmm * T11) + i2;
                j3 = (jmm * T21) + i3;
                for (k = 0; (k < oZdim) && !threadStopped; k++) {

                    // transform i,j,k
                    if (pad) {
                        kAdj = k - AlgorithmTransform.margins[2];
                    } else {
                        kAdj = k;
                    }
                    kmm = kAdj * oZres;
                    aValue = fillValue;
                    rValue = fillValue;
                    gValue = fillValue;
                    bValue = fillValue;
                    X = (kmm * T02) + j1;
                    X = X * invXRes;
                    if ((X >= -0.5) && (X < iXdim)) {
                        Y = (kmm * T12) + j2;
                        Y = Y * invYRes;
                        if ((Y >= -0.5) && (Y < iYdim)) {
                            Z = (kmm * T22) + j3;
                            Z = Z * invZRes;
                            if ((Z >= -0.5) && (Z < iZdim)) {
                                roundX = (int) (X + 0.5);
                                roundY = (int) (Y + 0.5);
                                roundZ = (int) (Z + 0.5);
                            
                                xOffset = Math.min(roundX, iXdim - 1);
                                yOffset = Math.min(roundY, iYdim - 1) * iXdim;
                                zOffset = Math.min(roundZ, iZdim - 1) * sliceSize;
                                inputSum = 4*(xOffset + yOffset + zOffset);
                                aValue = (float)imgBuf[inputSum];
                                rValue = (float)imgBuf[inputSum+1];
                                gValue = (float)imgBuf[inputSum+2];
                                bValue = (float)imgBuf[inputSum+3];
                            } // if ((Z >= -0.5) && (Z < iZdim))
                        } // if ((Y >= -0.5) && (Y < iYdim))
                    } // if ((X >= -0.5) && (X < iXdim))
                    outputSum = 4*(i + (oXdim *j) + (outSliceSize *k));
                    imgBuf2[outputSum] = aValue;
                    imgBuf2[outputSum+1] = rValue;
                    imgBuf2[outputSum+2] = gValue;
                    imgBuf2[outputSum+3] = bValue;
                }
            }
        }

        if (threadStopped) {
            return;
        }

        try {
            destImage.importData(0, imgBuf2, true);
        } catch (final IOException error) {
            MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
        }

    }

    /**
     * Transforms and resamples volume using nearest neighbor interpolation.
     * 
     * @param imgBuf image array
     * @param kTM transformation matrix to be applied
     */
    private void transformNearestNeighbor3Dim2D(final double[] imgBuf, final TransMatrix kTM) {
        int i, j, k;
        int iAdj, jAdj;
        double X, Y;
        int roundX, roundY;
        int xOffset, yOffset;
        double value;
        double imm, jmm;
        double i1, i2;
        final double invXRes = 1.0 / iXres;
        final double invYRes = 1.0 / iYres;
        double T00, T01, T02, T10, T11, T12;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        for (k = 0; (k < oZdim) && !threadStopped; k++) {
            fireProgressStateChanged(Math.round((float) k / oZdim * 100));

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                if (pad) {
                    iAdj = i - AlgorithmTransform.margins[0];
                } else {
                    iAdj = i;
                }
                imm = iAdj * oXres;
                i1 = (imm * T00) + T02;
                i2 = (imm * T10) + T12;
                for (j = 0; (j < oYdim) && !threadStopped; j++) {

                    // transform i,j
                    if (pad) {
                        jAdj = j - AlgorithmTransform.margins[1];
                    } else {
                        jAdj = j;
                    }
                    jmm = jAdj * oYres;
                    value = fillValue;
                    X = (jmm * T01) + i1;
                    X = X * invXRes;
                    if ((X >= -0.5) && (X < iXdim)) {
                        Y = (jmm * T11) + i2;
                        Y = Y * invYRes;
                        if ((Y >= -0.5) && (Y < iYdim)) {
                            roundX = (int) (X + 0.5);
                            roundY = (int) (Y + 0.5);

                            xOffset = Math.min(roundX, iXdim - 1);
                            yOffset = Math.min(roundY, iYdim - 1) * iXdim;
                            value = imgBuf[xOffset + yOffset];
                        } // if ((Y >= -0.5) && (Y < iYdim))
                    } // if ((X >= -0.5) && (X < iXdim))
                    
                    destImage.set(i, j, k, value);
                }
            }

            if (threadStopped) {
                return;
            }

            if (k < (oZdim - 1)) {

                try {
                    srcImage.exportData( (k + 1) * imgLength, imgLength, imgBuf);
                } catch (final IOException error) {
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
     * @param imgBuf input image array
     * @param imgBuf2 output image array
     * @param kTM transformation matrix to be applied
     */
    private void transformNearestNeighbor3Dim2DC(final double[] imgBuf, final float[] imgBuf2, final TransMatrix kTM) {
        int i, j, k;
        int iAdj, jAdj;
        double X, Y;
        int roundX, roundY;
        int xOffset, yOffset;
        double imm, jmm;
        double i1, i2;
        float aValue;
        float rValue;
        float gValue;
        float bValue;
        final double invXRes = 1.0 / iXres;
        final double invYRes = 1.0 / iYres;
        int inputSum;
        int outputSum;
        int oSliceSize = oXdim * oYdim;
        double T00, T01, T02, T10, T11, T12;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        for (k = 0; (k < oZdim) && !threadStopped; k++) {
            fireProgressStateChanged((int) ( ((float) k / oZdim * 100) + 0.5));

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                if (pad) {
                    iAdj = i - AlgorithmTransform.margins[0];
                } else {
                    iAdj = i;
                }
                imm = iAdj * oXres;
                i1 = (imm * T00) + T02;
                i2 = (imm * T10) + T12;
                for (j = 0; (j < oYdim) && !threadStopped; j++) {

                    // transform i,j
                    if (pad) {
                        jAdj = j - AlgorithmTransform.margins[1];
                    } else {
                        jAdj = j;
                    }
                    jmm = jAdj * oYres;
                    aValue = fillValue;
                    rValue = fillValue;
                    gValue = fillValue;
                    bValue = fillValue;
                    X = (jmm * T01) + i1;
                    X = X * invXRes;
                    if ((X >= -0.5) && (X < iXdim)) {
                        Y = (jmm * T11) + i2;
                        Y = Y * invYRes;
                        if ((Y >= -0.5) && (Y < iYdim)) {
                            roundX = (int) (X + 0.5);
                            roundY = (int) (Y + 0.5);

                            xOffset = Math.min(roundX, iXdim - 1);
                            yOffset = Math.min(roundY, iYdim - 1) * iXdim;
                            inputSum = 4*(xOffset + yOffset);
                            aValue = (float)imgBuf[inputSum];
                            rValue = (float)imgBuf[inputSum+1];
                            gValue = (float)imgBuf[inputSum+2];
                            bValue = (float)imgBuf[inputSum+3];
                        } // if ((Y >= -0.5) && (Y < iYdim))
                    } // if ((X >= -0.5) && (X < iXdim))
                    outputSum = 4*(i + (oXdim *j));
                    imgBuf2[outputSum] = aValue;
                    imgBuf2[outputSum+1] = rValue;
                    imgBuf2[outputSum+2] = gValue;
                    imgBuf2[outputSum+3] = bValue;
                }
            }

            try {
                destImage.importData(4 * k * oSliceSize, imgBuf2, true);
            } catch (final IOException error) {
                MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
                setCompleted(false);

                return;
            }

            if (k < (oZdim - 1)) {

                try {
                    srcImage.exportData( (k + 1) * imgLength, imgLength, imgBuf);
                } catch (final IOException error) {
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
     * @param imgBuf image array
     * @param kTM transformation matrix to be applied
     */
    private void transformNearestNeighbor4D(final double[] imgBuf, final TransMatrix kTM) {
        int i, j, k, l;
        int iAdj, jAdj, kAdj;
        double X, Y, Z;
        int xOffset, yOffset, zOffset;
        double value;
        int sliceSize;
        int roundX, roundY, roundZ;
        double imm, jmm, kmm;
        double i1, i2, i3, j1, j2, j3;
        final double invXRes = 1.0 / iXres;
        final double invYRes = 1.0 / iYres;
        final double invZRes = 1.0 / iZres;

        sliceSize = iXdim * iYdim;

        double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T03 = (double)kTM.M03;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;
        T13 = (double)kTM.M13;
        T20 = (double)kTM.M20;
        T21 = (double)kTM.M21;
        T22 = (double)kTM.M22;
        T23 = (double)kTM.M23;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged((int) ( ((float) l / oTdim * 100) + 0.5));

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                if (pad) {
                    iAdj = i - AlgorithmTransform.margins[0];
                } else {
                    iAdj = i;
                }
                imm = iAdj * oXres;
                i1 = (imm * T00) + T03;
                i2 = (imm * T10) + T13;
                i3 = (imm * T20) + T23;
                for (j = 0; (j < oYdim) && !threadStopped; j++) {
                    if (pad) {
                        jAdj = j - AlgorithmTransform.margins[1];
                    } else {
                        jAdj = j;
                    }
                    jmm = jAdj * oYres;
                    j1 = (jmm * T01) + i1;
                    j2 = (jmm * T11) + i2;
                    j3 = (jmm * T21) + i3;
                    for (k = 0; (k < oZdim) && !threadStopped; k++) {

                        // transform i,j,k
                        if (pad) {
                            kAdj = k - AlgorithmTransform.margins[2];
                        } else {
                            kAdj = k;
                        }
                        kmm = kAdj * oZres;
                        value = fillValue;
                        X = (kmm * T02) + j1;
                        X = X * invXRes;
                        if ((X >= -0.5) && (X < iXdim)) {
                            Y = (kmm * T12) + j2;
                            Y = Y * invYRes;
                            if ((Y >= -0.5) && (Y < iYdim)) {
                                Z = (kmm * T22) + j3;
                                Z = Z * invZRes;
                                if ((Z >= -0.5) && (Z < iZdim)) {
            
                                    roundX = (int) (X + 0.5);
                                    roundY = (int) (Y + 0.5);
                                    roundZ = (int) (Z + 0.5);
                                
                                    xOffset = Math.min(roundX, iXdim - 1);
                                    yOffset = Math.min(roundY, iYdim - 1) * iXdim;
                                    zOffset = Math.min(roundZ, iZdim - 1) * sliceSize;
                                    value = imgBuf[xOffset + yOffset + zOffset];
                                } // if ((Z >= -0.5) && (Z < iZdim))
                            } // if ((Y >= -0.5) && (Y < iYdim))
                        } // if ((X >= -0.5) && (X < iXdim))

                        destImage.set(i, j, k, l, value);
                    }
                }
            } // for i

            if (l < (oTdim - 1)) {

                try {
                    srcImage.exportData( (l + 1) * imgLength, imgLength, imgBuf);
                } catch (final IOException error) {
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
     * @param imgBuf image array
     * @param imgBufffer2
     * @param kTM transformation matrix to be applied
     */
    private void transformNearestNeighbor4DC(final double[] imgBuf, final float imgBuffer2[], final TransMatrix kTM) {
        int i, j, k, l;
        int iAdj, jAdj, kAdj;
        double X, Y, Z;
        int xOffset, yOffset, zOffset;
        int sliceSize;
        int oSliceSize;
        int oVolSize;
        int roundX, roundY, roundZ;
        double imm, jmm, kmm;
        double i1, i2, i3, j1, j2, j3;
        float aValue;
        float rValue;
        float gValue;
        float bValue;
        final double invXRes = 1.0 / iXres;
        final double invYRes = 1.0 / iYres;
        final double invZRes = 1.0 / iZres;
        int inputSum;
        int outputSum;

        sliceSize = iXdim * iYdim;
        oSliceSize = oXdim * oYdim;
        oVolSize = oSliceSize * oZdim;

        double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T03 = (double)kTM.M03;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;
        T13 = (double)kTM.M13;
        T20 = (double)kTM.M20;
        T21 = (double)kTM.M21;
        T22 = (double)kTM.M22;
        T23 = (double)kTM.M23;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged((int) ( ((float) l / oTdim * 100) + 0.5));

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                if (pad) {
                    iAdj = i - AlgorithmTransform.margins[0];
                } else {
                    iAdj = i;
                }
                imm = iAdj * oXres;
                i1 = (imm * T00) + T03;
                i2 = (imm * T10) + T13;
                i3 = (imm * T20) + T23;
                for (j = 0; (j < oYdim) && !threadStopped; j++) {
                    if (pad) {
                        jAdj = j - AlgorithmTransform.margins[1];
                    } else {
                        jAdj = j;
                    }
                    jmm = jAdj * oYres;
                    j1 = (jmm * T01) + i1;
                    j2 = (jmm * T11) + i2;
                    j3 = (jmm * T21) + i3;
                    for (k = 0; (k < oZdim) && !threadStopped; k++) {

                        // transform i,j,k
                        if (pad) {
                            kAdj = k - AlgorithmTransform.margins[2];
                        } else {
                            kAdj = k;
                        }
                        kmm = kAdj * oZres;
                        aValue = fillValue;
                        rValue = fillValue;
                        gValue = fillValue;
                        bValue = fillValue;
                        X = (kmm * T02) + j1;
                        X = X * invXRes;
                        if ((X >= -0.5) && (X < iXdim)) {
                            Y = (kmm * T12) + j2;
                            Y = Y * invYRes;
                            if ((Y >= -0.5) && (Y < iYdim)) {
                                Z = (kmm * T22) + j3;
                                Z = Z * invZRes;
                                if ((Z >= -0.5) && (Z < iZdim)) {
                                    roundX = (int) (X + 0.5);
                                    roundY = (int) (Y + 0.5);
                                    roundZ = (int) (Z + 0.5);
                                
                                    xOffset = Math.min(roundX, iXdim - 1);
                                    yOffset = Math.min(roundY, iYdim - 1) * iXdim;
                                    zOffset = Math.min(roundZ, iZdim - 1) * sliceSize;
                                    inputSum = 4*(xOffset + yOffset + zOffset);
                                    aValue = (float)imgBuf[inputSum];
                                    rValue = (float)imgBuf[inputSum+1];
                                    gValue = (float)imgBuf[inputSum+2];
                                    bValue = (float)imgBuf[inputSum+3];
                                } // if ((Z >= -0.5) && (Z < iZdim))
                            } // if ((Y >= -0.5) && (Y < iYdim))
                        } // if ((X >= -0.5) && (X < iXdim))
                        outputSum = 4*(i + (oXdim *j) + (oSliceSize *k));
                        imgBuffer2[outputSum] = aValue;
                        imgBuffer2[outputSum+1] = rValue;
                        imgBuffer2[outputSum+2] = gValue;
                        imgBuffer2[outputSum+3] = bValue;
                    }
                }
            } // for i

            try {
                destImage.importData(4 * l * oVolSize, imgBuffer2, false);
            } catch (final IOException error) {
                MipavUtil.displayError("AlgorithmTransform: IOException on destImage.importData");
            }

            if (l < (oTdim - 1)) {

                try {
                    srcImage.exportData( (l + 1) * imgLength, imgLength, imgBuf);
                } catch (final IOException error) {
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
     * @param imgBuf image array
     * @param kTM transformation matrix to be applied
     */
    private void transformNearestNeighbor4Dim2D(final double[] imgBuf, final TransMatrix kTM) {
        int i, j, k, l;
        int iAdj, jAdj;
        double X, Y;
        int roundX, roundY;
        int xOffset, yOffset;
        double value;
        double imm, jmm;
        double i1, i2;
        final double invXRes = 1.0 / iXres;
        final double invYRes = 1.0 / iYres;
        double T00, T01, T02, T10, T11, T12;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged((int) ( ((float) l / oTdim * 100) + 0.5));

            for (k = 0; (k < oZdim) && !threadStopped; k++) {

                for (i = 0; (i < oXdim) && !threadStopped; i++) {
                    if (pad) {
                        iAdj = i - AlgorithmTransform.margins[0];
                    } else {
                        iAdj = i;
                    }
                    imm = iAdj * oXres;
                    i1 = (imm * T00) + T02;
                    i2 = (imm * T10) + T12;
                    for (j = 0; (j < oYdim) && !threadStopped; j++) {

                        // transform i,j
                        if (pad) {
                            jAdj = j - AlgorithmTransform.margins[1];
                        } else {
                            jAdj = j;
                        }
                        jmm = jAdj * oYres;
                        value = fillValue;
                        X = (jmm * T01) + i1;
                        X = X * invXRes;
                        if ((X >= -0.5) && (X < iXdim)) {
                            Y = (jmm * T11) + i2;
                            Y = Y * invYRes;
                            if ((Y >= -0.5) && (Y < iYdim)) {
                                roundX = (int) (X + 0.5);
                                roundY = (int) (Y + 0.5);

                                xOffset = Math.min(roundX, iXdim - 1);
                                yOffset = Math.min(roundY, iYdim - 1) * iXdim;
                                value = imgBuf[xOffset + yOffset];
                            } // if ((Y >= -0.5) && (Y < iYdim))
                        } // if ((X >= -0.5) && (X < iXdim))
                        
                        destImage.set(i, j, k, l, value);
                    }
                }

                if ( (k < (oZdim - 1)) || (l < (oTdim - 1))) {

                    try {
                        srcImage.exportData( (l * oZdim * imgLength) + ( (k + 1) * imgLength), imgLength, imgBuf);
                    } catch (final IOException error) {
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
     * @param imgBuf input image array
     * @param imgBuf2 output image array
     * @param kTM transformation matrix to be applied
     */
    private void transformNearestNeighbor4Dim2DC(final double[] imgBuf, final float[] imgBuf2, final TransMatrix kTM) {
        int i, j, k, l;
        int iAdj, jAdj;
        double X, Y;
        int roundX, roundY;
        int xOffset, yOffset;
        double imm, jmm;
        double i1, i2;
        float aValue;
        float rValue;
        float gValue;
        float bValue;
        final double invXRes = 1.0 / iXres;
        final double invYRes = 1.0 / iYres;
        int inputSum;
        int outputSum;
        int oSliceSize = oXdim * oYdim;
        int oVolSize = oSliceSize * oZdim;
        double T00, T01, T02, T10, T11, T12;
    
        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;
    
        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged((int) ( ((float) l / oTdim * 100) + 0.5));
    
            for (k = 0; (k < oZdim) && !threadStopped; k++) {
    
                for (i = 0; (i < oXdim) && !threadStopped; i++) {
                    if (pad) {
                        iAdj = i - AlgorithmTransform.margins[0];
                    } else {
                        iAdj = i;
                    }
                    imm = iAdj * oXres;
                    i1 = (imm * T00) + T02;
                    i2 = (imm * T10) + T12;
                    for (j = 0; (j < oYdim) && !threadStopped; j++) {
    
                        // transform i,j
                        if (pad) {
                            jAdj = j - AlgorithmTransform.margins[1];
                        } else {
                            jAdj = j;
                        }
                        jmm = jAdj * oYres;
                        aValue = fillValue;
                        rValue = fillValue;
                        gValue = fillValue;
                        bValue = fillValue;
                        X = (jmm * T01) + i1;
                        X = X * invXRes;
                        if ((X >= -0.5) && (X < iXdim)) {
                            Y = (jmm * T11) + i2;
                            Y = Y * invYRes;
                            if ((Y >= -0.5) && (Y < iYdim)) {
                                roundX = (int) (X + 0.5);
                                roundY = (int) (Y + 0.5);

                                xOffset = Math.min(roundX, iXdim - 1);
                                yOffset = Math.min(roundY, iYdim - 1) * iXdim;
                                inputSum = 4*(xOffset + yOffset);
                                aValue = (float)imgBuf[inputSum];
                                rValue = (float)imgBuf[inputSum+1];
                                gValue = (float)imgBuf[inputSum+2];
                                bValue = (float)imgBuf[inputSum+3];
                            } // if ((Y >= -0.5) && (Y < iYdim))
                        } // if ((X >= -0.5) && (X < iXdim))
                        outputSum = 4*(i + (oXdim *j));
                        imgBuf2[outputSum] = aValue;
                        imgBuf2[outputSum+1] = rValue;
                        imgBuf2[outputSum+2] = gValue;
                        imgBuf2[outputSum+3] = bValue;
                    }
                }
    
                try {
                    destImage.importData( (4 * l * oVolSize) + (4 * k * oSliceSize), imgBuf2, true);
                } catch (final IOException error) {
                    MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
                }
    
                if ( (k < (oZdim - 1)) || (l < (oTdim - 1))) {
    
                    try {
                        srcImage.exportData( (l * oZdim * imgLength) + ( (k + 1) * imgLength), imgLength, imgBuf);
                    } catch (final IOException error) {
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
     * @param imgBuf Image array
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformQuinticLagrangian2D(final double[] imgBuf, final TransMatrix kTM, final boolean clip) {
        AlgorithmQuinticLagrangian QLag = new AlgorithmQuinticLagrangian();
        int i, j;
        int iAdj, jAdj;
        double X, Y;
        double value;
        double imm, jmm;
        final int[] inVolExtents = {iXdim, iYdim};
        final int mod = Math.max(1, oXdim / 50);
        double temp1, temp2;
        double T00, T01, T02, T10, T11, T12;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        QLag.setup2DQuinticLagrangian(imgBuf, inVolExtents, clip);

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if ( ( (i % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) i / oXdim * 100) + 0.5));
            }

            if (pad) {
                iAdj = i - AlgorithmTransform.margins[0];
            } else {
                iAdj = i;
            }
            imm = iAdj * oXres;
            temp1 = (imm * T00) + T02;
            temp2 = (imm * T10) + T12;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                // convert to mm
                value = fillValue; // if voxel transformed out of bounds
                if (pad) {
                    jAdj = j - AlgorithmTransform.margins[1];
                } else {
                    jAdj = j;
                }
                jmm = jAdj * oYres;

                // transform i,j
                X = (temp1 + (jmm * T01)) / iXres;

                if ( (X >= 0) && (X < iXdim)) { // check bounds
                    Y = (temp2 + (jmm * T11)) / iYres;

                    if ( (Y >= 0) && (Y < iYdim)) {
                        value = QLag.quinticLagrangian2D(X, Y);
                    }
                }

                destImage.set(i, j, value);
            }
        }

        QLag.finalize();
        QLag = null;
        Preferences.debug("finished quintic Lagrangian", Preferences.DEBUG_ALGORITHM);
    }

    /**
     * Transforms and resamples volume using quintic Lagrangian interpolation This version used with color images.
     * 
     * @param imgBuf Input image array
     * @param imgBuf2 Output image array
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformQuinticLagrangian2DC(final double[] imgBuf, final float[] imgBuf2, final TransMatrix kTM,
            final boolean clip) {
        AlgorithmQuinticLagrangian QLag = new AlgorithmQuinticLagrangian();
        int i, j;
        int iAdj, jAdj;
        double X, Y;
        double[] value = new double[4];
        double imm, jmm;
        final int[] inVolExtents = {iXdim, iYdim};
        final int mod = Math.max(1, oXdim / 50);
        double temp1, temp2;
        int temp3;
        double T00, T01, T02, T10, T11, T12;
        float argbMax = 255.0f;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        if (srcImage.getType() == ModelStorageBase.ARGB) {
            argbMax = 255.0f;
        } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
            argbMax = 65535.0f;
        }

        QLag.setup2DQuinticLagrangianC(imgBuf, inVolExtents, argbMax, clip);

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if ( ( (i % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) i / oXdim * 100) + 0.5));
            }

            if (pad) {
                iAdj = i - AlgorithmTransform.margins[0];
            } else {
                iAdj = i;
            }
            imm = iAdj * oXres;
            temp1 = (imm * T00) + T02;
            temp2 = (imm * T10) + T12;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                // convert to mm
                value[0] = fillValue; // if voxel transforms out of bounds
                value[1] = fillValue;
                value[2] = fillValue;
                value[3] = fillValue;
                if (pad) {
                    jAdj = j - AlgorithmTransform.margins[1];
                } else {
                    jAdj = j;
                }
                jmm = jAdj * oYres;

                // transform i,j
                X = (temp1 + (jmm * T01)) / iXres;

                if ( (X >= 0) && (X < iXdim)) { // check bounds
                    Y = (temp2 + (jmm * T11)) / iYres;

                    if ( (Y >= 0) && (Y < iYdim)) {
                        value = QLag.quinticLagrangian2DC(X, Y);
                    }
                }

                temp3 = 4 * (i + (j * oXdim));
                imgBuf2[temp3] = (float)value[0];
                imgBuf2[temp3 + 1] = (float)value[1];
                imgBuf2[temp3 + 2] = (float)value[2];
                imgBuf2[temp3 + 3] = (float)value[3];
            }
        }

        if (threadStopped) {
            return;
        }

        try {
            destImage.importData(0, imgBuf2, true);
        } catch (final IOException error) {
            MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
        }

        QLag.finalize();
        QLag = null;
        Preferences.debug("finished quintic Lagrangian", Preferences.DEBUG_ALGORITHM);

    }

    /**
     * Transforms and resamples volume using quintic Lagrangian interpolation.
     * 
     * @param imgBuf Image array
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformQuinticLagrangian3D(final double[] imgBuf, final TransMatrix kTM, final boolean clip) {
        AlgorithmQuinticLagrangian QLag = new AlgorithmQuinticLagrangian();
        int i, j, k;
        int iAdj, jAdj, kAdj;
        double X, Y, Z;
        double value;
        double imm, jmm, kmm;
        final int[] inVolExtents = {iXdim, iYdim, iZdim};
        final int mod = Math.max(1, oXdim / 50);

        double i1, i2, i3, j1, j2, j3;
        double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T03 = (double)kTM.M03;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;
        T13 = (double)kTM.M13;
        T20 = (double)kTM.M20;
        T21 = (double)kTM.M21;
        T22 = (double)kTM.M22;
        T23 = (double)kTM.M23;

        QLag.setup3DQuinticLagrangian(imgBuf, inVolExtents, clip);

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if ( ( (i % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) i / oXdim * 100) + 0.5));
            }

            if (pad) {
                iAdj = i - AlgorithmTransform.margins[0];
            } else {
                iAdj = i;
            }
            imm = iAdj * oXres;
            i1 = (imm * T00) + T03;
            i2 = (imm * T10) + T13;
            i3 = (imm * T20) + T23;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {
                if (pad) {
                    jAdj = j - AlgorithmTransform.margins[1];
                } else {
                    jAdj = j;
                }
                jmm = jAdj * oYres;
                j1 = (jmm * T01) + i1;
                j2 = (jmm * T11) + i2;
                j3 = (jmm * T21) + i3;

                for (k = 0; (k < oZdim) && !threadStopped; k++) {

                    // convert to mm
                    value = fillValue; // if voxel transforms out of bounds
                    if (pad) {
                        kAdj = k - AlgorithmTransform.margins[2];
                    } else {
                        kAdj = k;
                    }
                    kmm = kAdj * oZres;

                    // transform i,j,k
                    X = (j1 + (kmm * T02)) / iXres;

                    if ( (X >= 0) && (X < iXdim)) { // check bounds
                        Y = (j2 + (kmm * T12)) / iYres;

                        if ( (Y >= 0) && (Y < iYdim)) {
                            Z = (j3 + (kmm * T22)) / iZres;

                            if ( (Z >= 0) && (Z < iZdim)) {
                                value = QLag.quinticLagrangian3D(X, Y, Z);
                            }
                        }
                    }

                    destImage.set(i, j, k, value);
                }
            }
        }

        QLag.finalize();
        QLag = null;
        Preferences.debug("finished quintic Lagrangian", Preferences.DEBUG_ALGORITHM);
    }

    /**
     * Transforms and resamples volume using quintic Lagrangian interpolation This version used with color images.
     * 
     * @param imgBuf Input image array
     * @param imgBuf2 Output image array
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformQuinticLagrangian3DC(final double[] imgBuf, final float[] imgBuf2, final TransMatrix kTM,
            final boolean clip) {
        AlgorithmQuinticLagrangian QLag = new AlgorithmQuinticLagrangian();
        int i, j, k;
        int iAdj, jAdj, kAdj;
        double X, Y, Z;
        double[] value = new double[4];
        double imm, jmm, kmm;
        final int[] inVolExtents = {iXdim, iYdim, iZdim};
        final int mod = Math.max(1, oXdim / 50);

        final int osliceSize = oXdim * oYdim;
        double i1, i2, i3, j1, j2, j3;
        double temp1, temp2, temp3;
        int temp4;
        double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        float argbMax = 255.0f;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T03 = (double)kTM.M03;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;
        T13 = (double)kTM.M13;
        T20 = (double)kTM.M20;
        T21 = (double)kTM.M21;
        T22 = (double)kTM.M22;
        T23 = (double)kTM.M23;

        if (srcImage.getType() == ModelStorageBase.ARGB) {
            argbMax = 255.0f;
        } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
            argbMax = 65535.0f;
        }

        QLag.setup3DQuinticLagrangianC(imgBuf, inVolExtents, argbMax, clip);

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if ( ( (i % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) i / oXdim * 100) + 0.5));
            }

            if (pad) {
                iAdj = i - AlgorithmTransform.margins[0];
            } else {
                iAdj = i;
            }
            imm = iAdj * oXres;
            i1 = (imm * T00) + T03;
            i2 = (imm * T10) + T13;
            i3 = (imm * T20) + T23;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {
                if (pad) {
                    jAdj = j - AlgorithmTransform.margins[1];
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

                    // convert to mm
                    value[0] = fillValue; // if voxel transforms out of bounds
                    value[1] = fillValue;
                    value[2] = fillValue;
                    value[3] = fillValue;
                    if (pad) {
                        kAdj = k - AlgorithmTransform.margins[2];
                    } else {
                        kAdj = k;
                    }
                    kmm = kAdj * oZres;

                    // transform i,j,k
                    X = (temp3 + (kmm * T02)) / iXres;

                    // convert back to pixels
                    if ( (X >= 0) && (X < iXdim)) { // check bounds
                        Y = (temp2 + (kmm * T12)) / iYres;

                        if ( (Y >= 0) && (Y < iYdim)) {
                            Z = (temp1 + (kmm * T22)) / iZres;

                            if ( (Z >= 0) && (Z < iZdim)) {
                                value = QLag.quinticLagrangian3DC(X, Y, Z);
                            }
                        }
                    }

                    temp4 = 4 * (i + (j * oXdim) + (k * osliceSize));
                    imgBuf2[temp4] = (float)value[0];
                    imgBuf2[temp4 + 1] = (float)value[1];
                    imgBuf2[temp4 + 2] = (float)value[2];
                    imgBuf2[temp4 + 3] = (float)value[3];
                }
            }
        }

        if (threadStopped) {
            return;
        }

        try {
            destImage.importData(0, imgBuf2, true);
        } catch (final IOException error) {
            MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
        }

        QLag.finalize();
        QLag = null;
        Preferences.debug("finished quintic Lagrangian", Preferences.DEBUG_ALGORITHM);

    }

    /**
     * Transforms and resamples volume using quintic Lagrangian interpolation Does a slice by slice quintic Lagrangian
     * interpolation on a 3 dimensional object.
     * 
     * @param imgBuf Image array
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformQuinticLagrangian3Dim2D(final double[] imgBuf, final TransMatrix kTM, final boolean clip) {
        AlgorithmQuinticLagrangian QLag = new AlgorithmQuinticLagrangian();
        int i, j, k;
        int iAdj, jAdj;
        double X, Y;
        double value;
        double imm, jmm;
        final int[] inVolExtents = {iXdim, iYdim};

        final int mod = Math.max(1, oZdim / 50);
        double temp1, temp2;
        double T00, T01, T02, T10, T11, T12;

        T00 = kTM.M00;
        T01 = kTM.M01;
        T02 = kTM.M02;
        T10 = kTM.M10;
        T11 = kTM.M11;
        T12 = kTM.M12;

        for (k = 0; (k < oZdim) && !threadStopped; k++) {

            if ( ( (k % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) k / oZdim * 100) + 0.5));
            }

            QLag.setup2DQuinticLagrangian(imgBuf, inVolExtents, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                if (pad) {
                    iAdj = i - AlgorithmTransform.margins[0];
                } else {
                    iAdj = i;
                }
                imm = iAdj * oXres;
                temp1 = (imm * T00) + T02;
                temp2 = (imm * T10) + T12;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {

                    // convert to mm
                    value = fillValue; // if voxel transforms out of bounds
                    if (pad) {
                        jAdj = j - AlgorithmTransform.margins[1];
                    } else {
                        jAdj = j;
                    }
                    jmm = jAdj * oYres;

                    // transform i,j
                    X = (temp1 + (jmm * T01)) / iXres;

                    if ( (X >= 0) && (X < iXdim)) { // check bounds
                        Y = (temp2 + (jmm * T11)) / iYres;

                        if ( (Y >= 0) && (Y < iYdim)) {
                            value = QLag.quinticLagrangian2D(X, Y);
                        }
                    }

                    destImage.set(i, j, k, value);
                }
            }

            if (k < (oZdim - 1)) {

                try {
                    srcImage.exportData( (k + 1) * imgLength, imgLength, imgBuf);
                } catch (final IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);

                    return;
                }
            } // end if (k < (oZdim - 1))
        } // end for k

        QLag.finalize();
        QLag = null;
        Preferences.debug("finished quintic Lagrangian", Preferences.DEBUG_ALGORITHM);
    }

    /**
     * Transforms and resamples volume using quintic Lagrangian interpolation This version used with color images This
     * version performs a slice by slice algorithm on a 3 dimensional object.
     * 
     * @param imgBuf Input image array
     * @param imgBuf2 Output image array
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformQuinticLagrangian3Dim2DC(final double[] imgBuf, final float[] imgBuf2, final TransMatrix kTM,
            final boolean clip) {
        AlgorithmQuinticLagrangian QLag = new AlgorithmQuinticLagrangian();
        int i, j, k;
        int iAdj, jAdj;
        double X, Y;
        double[] value = new double[4];
        double imm, jmm;
        final int[] inVolExtents = {iXdim, iYdim};

        final int mod = Math.max(1, oZdim / 50);
        double temp1, temp2;
        int temp3;
        double T00, T01, T02, T10, T11, T12;
        float argbMax = 255.0f;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        for (k = 0; (k < oZdim) && !threadStopped; k++) {

            if ( ( (k % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) k / oZdim * 100) + 0.5));
            }

            if (srcImage.getType() == ModelStorageBase.ARGB) {
                argbMax = 255.0f;
            } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
                argbMax = 65535.0f;
            }

            QLag.setup2DQuinticLagrangianC(imgBuf, inVolExtents, argbMax, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                if (pad) {
                    iAdj = i - AlgorithmTransform.margins[0];
                } else {
                    iAdj = i;
                }
                imm = iAdj * oXres;
                temp1 = (imm * T00) + T02;
                temp2 = (imm * T10) + T12;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {

                    // convert to mm
                    value[0] = fillValue; // if voxel transforms out of bounds
                    value[1] = fillValue;
                    value[2] = fillValue;
                    value[3] = fillValue;
                    if (pad) {
                        jAdj = j - AlgorithmTransform.margins[1];
                    } else {
                        jAdj = j;
                    }
                    jmm = jAdj * oYres;

                    // transform i,j
                    X = (temp1 + (jmm * T01)) / iXres;

                    if ( (X >= 0) && (X < iXdim)) { // check bounds
                        Y = (temp2 + (jmm * T11)) / iYres;

                        if ( (Y >= 0) && (Y < iYdim)) {
                            value = QLag.quinticLagrangian2DC(X, Y);
                        }
                    }

                    temp3 = 4 * (i + (j * oXdim));
                    imgBuf2[temp3] = (float)value[0];
                    imgBuf2[temp3 + 1] = (float)value[1];
                    imgBuf2[temp3 + 2] = (float)value[2];
                    imgBuf2[temp3 + 3] = (float)value[3];
                } // for i
            } // for j

            if (threadStopped) {
                return;
            }

            try {
                destImage.importData(4 * k * oXdim * oYdim, imgBuf2, true);
            } catch (final IOException error) {
                MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
            }

            if (k < (oZdim - 1)) {

                try {
                    srcImage.exportData( (k + 1) * imgLength, imgLength, imgBuf);
                } catch (final IOException error) {
                    displayError("Algorithm Transform: IOException Error on importData");
                    setCompleted(false);

                    return;
                }
            } // end if (k < (oZdim - 1))
        } // end for k

        QLag.finalize();
        QLag = null;
        Preferences.debug("finished quintic Lagrangian", Preferences.DEBUG_ALGORITHM);
    }

    /**
     * Transforms and resamples 4 dimensional color object using 3D quintic Lagrangian interpolation.
     * 
     * @param imgBuf Image array
     * @param imgBuf2
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformQuinticLagrangian4DC(final double[] imgBuf, final float[] imgBuffer2, final TransMatrix kTM,
            final boolean clip) {
        AlgorithmQuinticLagrangian QLag = new AlgorithmQuinticLagrangian();
        int i, j, k, l;
        int iAdj, jAdj, kAdj;
        double X, Y, Z;
        double value[] = new double[4];
        int temp4;
        double imm, jmm, kmm;
        final int[] inVolExtents = {iXdim, iYdim, iZdim};
        int oSliceSize;
        int oVolSize;
        final int mod = Math.max(1, oTdim / 50);

        double i1, i2, i3, j1, j2, j3;
        double temp1, temp2, temp3;
        double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        float argbMax = 255.0f;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T03 = (double)kTM.M03;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;
        T13 = (double)kTM.M13;
        T20 = (double)kTM.M20;
        T21 = (double)kTM.M21;
        T22 = (double)kTM.M22;
        T23 = (double)kTM.M23;

        oSliceSize = oXdim * oYdim;
        oVolSize = oSliceSize * oZdim;

        if (srcImage.getType() == ModelStorageBase.ARGB) {
            argbMax = 255.0f;
        } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
            argbMax = 65535.0f;
        }

        for (l = 0; (l < oTdim) && !threadStopped; l++) {

            if ( ( (l % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) l / oTdim * 100) + .5));
            }

            QLag.setup3DQuinticLagrangianC(imgBuf, inVolExtents, argbMax, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                if (pad) {
                    iAdj = i - AlgorithmTransform.margins[0];
                } else {
                    iAdj = i;
                }
                imm = iAdj * oXres;
                i1 = (imm * T00) + T03;
                i2 = (imm * T10) + T13;
                i3 = (imm * T20) + T23;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {
                    if (pad) {
                        jAdj = j - AlgorithmTransform.margins[1];
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

                        // convert to mm
                        value[0] = fillValue; // if voxel transforms out of bounds
                        value[1] = fillValue;
                        value[2] = fillValue;
                        value[3] = fillValue;
                        if (pad) {
                            kAdj = k - AlgorithmTransform.margins[2];
                        } else {
                            kAdj = k;
                        }
                        kmm = kAdj * oZres;

                        // transform i,j,k
                        X = (temp3 + (kmm * T02)) / iXres;

                        if ( (X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (kmm * T12)) / iYres;

                            if ( (Y >= 0) && (Y < iYdim)) {
                                Z = (temp1 + (kmm * T22)) / iZres;

                                if ( (Z >= 0) && (Z < iZdim)) {
                                    value = QLag.quinticLagrangian3DC(X, Y, Z);
                                }
                            }
                        }

                        temp4 = 4 * (i + (j * oXdim) + (k * oSliceSize));
                        imgBuffer2[temp4] = (float)value[0];
                        imgBuffer2[temp4 + 1] = (float)value[1];
                        imgBuffer2[temp4 + 2] = (float)value[2];
                        imgBuffer2[temp4 + 3] = (float)value[3];
                    }
                }
            }

            try {
                destImage.importData(4 * l * oVolSize, imgBuffer2, false);
            } catch (final IOException error) {
                MipavUtil.displayError("AlgorithmTransform: IOException on destImage.importData");
            }

            if (l < (oTdim - 1)) {

                try {
                    srcImage.exportData( (l + 1) * imgLength, imgLength, imgBuf);
                } catch (final IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);

                    return;
                }
            } // end if (l < (oTdim - 1))
        } // for l

        destImage.calcMinMax();
        QLag.finalize();
        QLag = null;
        Preferences.debug("finished quintic Lagrangian", Preferences.DEBUG_ALGORITHM);

    }

    /**
     * Transforms and resamples 4 dimensional object using 3D quintic Lagrangian interpolation.
     * 
     * @param imgBuf Image array
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformQuinticLagrangian4D(final double[] imgBuf, final TransMatrix kTM, final boolean clip) {
        AlgorithmQuinticLagrangian QLag = new AlgorithmQuinticLagrangian();
        int i, j, k, l;
        int iAdj, jAdj, kAdj;
        double X, Y, Z;
        double value;
        double imm, jmm, kmm;
        final int[] inVolExtents = {iXdim, iYdim, iZdim};
        final int mod = Math.max(1, oTdim / 50);

        double i1, i2, i3, j1, j2, j3;
        double temp1, temp2, temp3;
        double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T03 = (double)kTM.M03;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;
        T13 = (double)kTM.M13;
        T20 = (double)kTM.M20;
        T21 = (double)kTM.M21;
        T22 = (double)kTM.M22;
        T23 = (double)kTM.M23;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {

            if ( ( (l % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) l / oTdim * 100) + .5));
            }

            QLag.setup3DQuinticLagrangian(imgBuf, inVolExtents, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                if (pad) {
                    iAdj = i - AlgorithmTransform.margins[0];
                } else {
                    iAdj = i;
                }
                imm = iAdj * oXres;
                i1 = (imm * T00) + T03;
                i2 = (imm * T10) + T13;
                i3 = (imm * T20) + T23;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {
                    if (pad) {
                        jAdj = j - AlgorithmTransform.margins[1];
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

                        // convert to mm
                        value = fillValue; // if voxel transforms out of bounds
                        if (pad) {
                            kAdj = k - AlgorithmTransform.margins[2];
                        } else {
                            kAdj = k;
                        }
                        kmm = kAdj * oZres;

                        // transform i,j,k
                        X = (temp3 + (kmm * T02)) / iXres;

                        if ( (X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (kmm * T12)) / iYres;

                            if ( (Y >= 0) && (Y < iYdim)) {
                                Z = (temp1 + (kmm * T22)) / iZres;

                                if ( (Z >= 0) && (Z < iZdim)) {
                                    value = QLag.quinticLagrangian3D(X, Y, Z);
                                }
                            }
                        }

                        destImage.set(i, j, k, l, value);
                    }
                }
            }

            if (l < (oTdim - 1)) {

                try {
                    srcImage.exportData( (l + 1) * imgLength, imgLength, imgBuf);
                } catch (final IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);

                    return;
                }
            } // end if (l < (oTdim - 1))
        } // for l

        QLag.finalize();
        QLag = null;
        Preferences.debug("finished quintic Lagrangian", Preferences.DEBUG_ALGORITHM);

    }

    /**
     * Transforms and resamples volume using quintic Lagrangian interpolation Does a slice by slice quintic Lagrangian
     * interpolation on a 4 dimensional object.
     * 
     * @param imgBuf Image array
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformQuinticLagrangian4Dim2D(final double[] imgBuf, final TransMatrix kTM, final boolean clip) {
        AlgorithmQuinticLagrangian QLag = new AlgorithmQuinticLagrangian();
        int i, j, k, l;
        int iAdj, jAdj;
        double X, Y;
        double value;
        double imm, jmm;
        final int[] inVolExtents = {iXdim, iYdim};

        double temp1, temp2;
        double T00, T01, T02, T10, T11, T12;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged((int) ( ((float) l / oTdim * 100) + 0.5));

            for (k = 0; (k < oZdim) && !threadStopped; k++) {
                QLag.setup2DQuinticLagrangian(imgBuf, inVolExtents, clip);

                for (i = 0; (i < oXdim) && !threadStopped; i++) {
                    if (pad) {
                        iAdj = i - AlgorithmTransform.margins[0];
                    } else {
                        iAdj = i;
                    }
                    imm = iAdj * oXres;
                    temp1 = (imm * T00) + T02;
                    temp2 = (imm * T10) + T12;

                    for (j = 0; (j < oYdim) && !threadStopped; j++) {

                        // convert to mm
                        value = fillValue; // if voxel transforms out of bounds
                        if (pad) {
                            jAdj = j - AlgorithmTransform.margins[1];
                        } else {
                            jAdj = j;
                        }
                        jmm = jAdj * oYres;

                        // transform i,j
                        X = (temp1 + (jmm * T01)) / iXres;

                        if ( (X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (jmm * T11)) / iYres;

                            if ( (Y >= 0) && (Y < iYdim)) {
                                value = QLag.quinticLagrangian2D(X, Y);
                            }
                        }

                        destImage.set(i, j, k, l, value);
                    }
                }

                if ( (k < (oZdim - 1)) || (l < (oTdim - 1))) {

                    try {
                        srcImage.exportData( (l * oZdim * imgLength) + ( (k + 1) * imgLength), imgLength, imgBuf);
                    } catch (final IOException error) {
                        displayError("Algorithm Transform: Image(s) locked");
                        setCompleted(false);

                        return;
                    }
                } // end if ((k < (oZdim - 1))|| (l < (oTdim - 1)))
            } // end for k
        } // end for l

        QLag.finalize();
        QLag = null;
        Preferences.debug("finished quintic Lagrangian", Preferences.DEBUG_ALGORITHM);

    }

    /**
     * Transforms and resamples volume using quintic Lagrangian interpolation This version used with color images This
     * version performs a slice by slice algorithm on a 4 dimensional object.
     * 
     * @param imgBuf Input image array
     * @param imgBuf2 Output image array
     * @param kTM Transformation matrix to be applied
     * @param clip if <code>true</code> clip output values to be within input range
     */
    private void transformQuinticLagrangian4Dim2DC(final double[] imgBuf, final float[] imgBuf2, final TransMatrix kTM,
            final boolean clip) {
        AlgorithmQuinticLagrangian QLag = new AlgorithmQuinticLagrangian();
        int i, j, k, l;
        int iAdj, jAdj;
        double X, Y;
        double[] value = new double[4];
        double imm, jmm;
        final int[] inVolExtents = {iXdim, iYdim};

        double temp1, temp2;
        int temp3;
        double T00, T01, T02, T10, T11, T12;
        float argbMax = 255.0f;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged((int) ( ((float) l / oTdim * 100) + 0.5));

            for (k = 0; (k < oZdim) && !threadStopped; k++) {

                if (srcImage.getType() == ModelStorageBase.ARGB) {
                    argbMax = 255.0f;
                } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
                    argbMax = 65535.0f;
                }

                QLag.setup2DQuinticLagrangianC(imgBuf, inVolExtents, argbMax, clip);

                for (i = 0; (i < oXdim) && !threadStopped; i++) {
                    if (pad) {
                        iAdj = i - AlgorithmTransform.margins[0];
                    } else {
                        iAdj = i;
                    }
                    imm = iAdj * oXres;
                    temp1 = (imm * T00) + T02;
                    temp2 = (imm * T10) + T12;

                    for (j = 0; (j < oYdim) && !threadStopped; j++) {

                        // convert to mm
                        value[0] = fillValue; // if voxel transforms out of bounds
                        value[1] = fillValue;
                        value[2] = fillValue;
                        value[3] = fillValue;
                        if (pad) {
                            jAdj = j - AlgorithmTransform.margins[1];
                        } else {
                            jAdj = j;
                        }
                        jmm = jAdj * oYres;

                        // transform i,j
                        X = (temp1 + (jmm * T01)) / iXres;

                        if ( (X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (jmm * T11)) / iYres;

                            if ( (Y >= 0) && (Y < iYdim)) {
                                value = QLag.quinticLagrangian2DC(X, Y);
                            }
                        }

                        temp3 = 4 * (i + (j * oXdim));
                        imgBuf2[temp3] = (float)value[0];
                        imgBuf2[temp3 + 1] = (float)value[1];
                        imgBuf2[temp3 + 2] = (float)value[2];
                        imgBuf2[temp3 + 3] = (float)value[3];
                    } // for j
                } // for i

                try {
                    destImage.importData( (4 * l * oXdim * oYdim * oZdim) + (4 * k * oXdim * oYdim), imgBuf2, true);
                } catch (final IOException error) {
                    MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
                }

                if ( (k < (oZdim - 1)) || (l < (oTdim - 1))) {

                    try {
                        srcImage.exportData( (l * oZdim * imgLength) + ( (k + 1) * imgLength), imgLength, imgBuf);
                    } catch (final IOException error) {
                        displayError("Algorithm Transform: IOException Error on importData");
                        setCompleted(false);

                        return;
                    }
                } // end if ((k < (oZdim - 1))|| (l < (oTdim - 1)))
            } // end for k
        } // end for l

        QLag.finalize();
        QLag = null;
        Preferences.debug("finished quintic Lagrangian", Preferences.DEBUG_ALGORITHM);
    }

    /**
     * Transforms and resamples a 3D volume using trilinear interpolation.
     * 
     * @param imgBuffer Image array
     * @param kTM Transformation matrix to be applied
     */
    private void transformTrilinear(final double[] imgBuffer, final TransMatrix kTM) {
        int i, j, k;
        int iAdj, jAdj, kAdj;
        double[] tempBuf;
        double X, Y, Z;
        int x0, y0, z0;
        double value;
        double imm, jmm, kmm;
        double k1, k2, k3, j1, j2, j3;
        double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        int deltaX, deltaY, deltaZ;

        int sliceSize;
        sliceSize = iXdim * iYdim;

        imgLength2 = oXdim * oYdim * oZdim;
        tempBuf = new double[imgLength2];

        final int mod = Math.max(1, oZdim / 50);

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T03 = (double)kTM.M03;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;
        T13 = (double)kTM.M13;
        T20 = (double)kTM.M20;
        T21 = (double)kTM.M21;
        T22 = (double)kTM.M22;
        T23 = (double)kTM.M23;
        // T30 = (float)xfrm[3][0]; T31 = (float)xfrm[3][1]; T32 = (float)xfrm[3][2]; T33 = (float)xfrm[3][3];

        int position1, position2;
        double b1, b2;
        double dx, dy, dz, dx1, dy1;

        final double invXRes = 1.0 / iXres;
        final double invYRes = 1.0 / iYres;
        final double invZRes = 1.0 / iZres;

        int index = 0;

        for (k = 0; (k < oZdim) && !threadStopped; k++) {

            if ( ( (k % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) k / oZdim * 100) + 0.5f));
            }

            if (pad) {
                kAdj = k - AlgorithmTransform.margins[2];
            } else {
                kAdj = k;
            }

            kmm = kAdj * oZres;
            k1 = (kmm * T02) + T03;
            k2 = (kmm * T12) + T13;
            k3 = (kmm * T22) + T23;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                if (pad) {
                    jAdj = j - AlgorithmTransform.margins[1];
                } else {
                    jAdj = j;
                }

                jmm = jAdj * oYres;
                j1 = (jmm * T01) + k1;
                j2 = (jmm * T11) + k2;
                j3 = (jmm * T21) + k3;

                for (i = 0; (i < oXdim) && !threadStopped; i++) {

                    // transform i,j,k
                    value = fillValue; // if voxel transforms out of bounds
                    if (pad) {
                        iAdj = i - AlgorithmTransform.margins[0];
                    } else {
                        iAdj = i;
                    }

                    imm = iAdj * oXres;
                    X = (j1 + (imm * T00)) * invXRes;

                    if ( (X > -0.5) && (X < iXdim)) {
                        Y = (j2 + (imm * T10)) * invYRes;
                        if ( (Y > -0.5) && (Y < iYdim)) {
                            Z = (j3 + (imm * T20)) * invZRes;
                            if ( (Z > -0.5) && (Z < iZdim)) {

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

                                b1 = (dy1 * ( (dx1 * imgBuffer[position1]) + (dx * imgBuffer[position1 + deltaX])))
                                        + (dy * ( (dx1 * imgBuffer[position1 + deltaY]) + (dx * imgBuffer[position1
                                                + deltaY + deltaX])));

                                b2 = (dy1 * ( (dx1 * imgBuffer[position2]) + (dx * imgBuffer[position2 + deltaX])))
                                        + (dy * ( (dx1 * imgBuffer[position2 + deltaY]) + (dx * imgBuffer[position2
                                                + deltaY + deltaX])));

                                value = ( (1 - dz) * b1) + (dz * b2);

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
        } catch (final IOException error) {
            displayError("AlgorithmTransform: IOException on destImage.importdata(0,imgBUf2, false).");

            setCompleted(false);

            return;
        }
    }

    /**
     * Transforms and resamples 4 dimensional object using trilinear interpolation.
     * 
     * @param imgBuffer Image array
     * @param kTM Transformation matrix to be applied
     */
    private void transformTrilinear4D(final double[] imgBuffer, final TransMatrix kTM) {
        int i, j, k, l;
        int iAdj, jAdj, kAdj;
        double X, Y, Z;
        int x0, y0, z0;
        double value;
        int sliceSize;
        double imm, jmm, kmm;
        double k1, k2, k3, j1, j2, j3;

        sliceSize = iXdim * iYdim;

        double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        int deltaX, deltaY, deltaZ;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T03 = (double)kTM.M03;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;
        T13 = (double)kTM.M13;
        T20 = (double)kTM.M20;
        T21 = (double)kTM.M21;
        T22 = (double)kTM.M22;
        T23 = (double)kTM.M23;

        int position1, position2;
        double b1, b2;
        double dx, dy, dz, dx1, dy1;

        final double invXRes = 1.0 / iXres;
        final double invYRes = 1.0 / iYres;
        final double invZRes = 1.0 / iZres;

        int index = 0;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged(Math.round((float) l / oTdim * 100));

            for (k = 0; (k < oZdim) && !threadStopped; k++) {
                if (pad) {
                    kAdj = k - AlgorithmTransform.margins[2];
                } else {
                    kAdj = k;
                }
                kmm = kAdj * oZres;
                k1 = (kmm * T02) + T03;
                k2 = (kmm * T12) + T13;
                k3 = (kmm * T22) + T23;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {
                    if (pad) {
                        jAdj = j - AlgorithmTransform.margins[1];
                    } else {
                        jAdj = j;
                    }
                    jmm = jAdj * oYres;
                    j1 = (jmm * T01) + k1;
                    j2 = (jmm * T11) + k2;
                    j3 = (jmm * T21) + k3;

                    for (i = 0; (i < oXdim) && !threadStopped; i++) {

                        // transform i,j,k
                        value = fillValue; // if voxel is transformed out of bounds
                        if (pad) {
                            iAdj = i - AlgorithmTransform.margins[0];
                        } else {
                            iAdj = i;
                        }
                        imm = iAdj * oXres;
                        X = (j1 + (imm * T00)) * invXRes;

                        if ( (X > -0.5) && (X < iXdim)) {
                            Y = (j2 + (imm * T10)) * invYRes;

                            if ( (Y > -0.5) && (Y < iYdim)) {
                                Z = (j3 + (imm * T20)) * invZRes;

                                if ( (Z > -0.5) && (Z < iZdim)) {

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

                                    b1 = (dy1 * ( (dx1 * imgBuffer[position1]) + (dx * imgBuffer[position1 + deltaX])))
                                            + (dy * ( (dx1 * imgBuffer[position1 + deltaY]) + (dx * imgBuffer[position1
                                                    + deltaY + deltaX])));

                                    b2 = (dy1 * ( (dx1 * imgBuffer[position2]) + (dx * imgBuffer[position2 + deltaX])))
                                            + (dy * ( (dx1 * imgBuffer[position2 + deltaY]) + (dx * imgBuffer[position2
                                                    + deltaY + deltaX])));

                                    value = ( (1 - dz) * b1) + (dz * b2);
                                } // end if Z in bounds
                            } // end if Y in bounds
                        } // end if X in bounds

                        destImage.set(index++, value);
                    } // end for i
                } // end for j
            } // end for k

            if (l < (oTdim - 1)) {

                try {
                    srcImage.exportData( (l + 1) * imgLength, imgLength, imgBuf);
                } catch (final IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);

                    return;
                }
            } // end if (l < (oTdim - 1))
        } // end for l
    }

    /**
     * Transforms and resamples 4 dimensional color ARGB object using trilinear interpolation.
     * 
     * @param imgBuffer Image array
     * @param imgBuf2
     * @param kTM Transformation matrix to be applied Byte buffers are used to reduce memory requirements by a factor of
     *            4
     */
    private void transformTrilinear4DByteC(final byte[] imgBuffer, final byte[] imgBuffer2, final TransMatrix kTM) {
        int i, j, k, l;
        int iAdj, jAdj, kAdj;
        float X, Y, Z;
        int x0, y0, z0;
        int temp;
        int sliceSize;
        int oSliceSize;
        int oVolSize;
        float imm, jmm, kmm;
        float k1, k2, k3, j1, j2, j3;
        byte byteFillValue;

        if (fillValue > 255.0f) {
            byteFillValue = (byte) 255;
        } else if (fillValue < 0.0f) {
            byteFillValue = 0;
        } else {
            byteFillValue = (byte) (fillValue + 0.5f);
        }

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

        final float invXRes = 1 / iXres;
        final float invYRes = 1 / iYres;
        final float invZRes = 1 / iZres;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged(Math.round((float) l / oTdim * 100));

            for (k = 0; (k < oZdim) && !threadStopped; k++) {
                if (pad) {
                    kAdj = k - AlgorithmTransform.margins[2];
                } else {
                    kAdj = k;
                }
                kmm = kAdj * oZres;
                k1 = (kmm * T02) + T03;
                k2 = (kmm * T12) + T13;
                k3 = (kmm * T22) + T23;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {
                    if (pad) {
                        jAdj = j - AlgorithmTransform.margins[1];
                    } else {
                        jAdj = j;
                    }
                    jmm = jAdj * oYres;
                    j1 = (jmm * T01) + k1;
                    j2 = (jmm * T11) + k2;
                    j3 = (jmm * T21) + k3;

                    for (i = 0; (i < oXdim) && !threadStopped; i++) {

                        // transform i,j,k
                        temp = 4 * (i + (j * oXdim) + (k * oSliceSize));
                        imgBuffer2[temp] = byteFillValue; // if voxel is transformed out of bounds
                        imgBuffer2[temp + 1] = byteFillValue;
                        imgBuffer2[temp + 2] = byteFillValue;
                        imgBuffer2[temp + 3] = byteFillValue;
                        if (pad) {
                            iAdj = i - AlgorithmTransform.margins[0];
                        } else {
                            iAdj = i;
                        }
                        imm = iAdj * oXres;
                        X = (j1 + (imm * T00)) * invXRes;

                        if ( (X > -0.5f) && (X < iXdim)) {
                            Y = (j2 + (imm * T10)) * invYRes;

                            if ( (Y > -0.5f) && (Y < iYdim)) {
                                Z = (j3 + (imm * T20)) * invZRes;

                                if ( (Z > -0.5f) && (Z < iZdim)) {

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

                                    b1 = (dy1 * ( (dx1 * (imgBuffer[4 * position1] & 0xff)) + (dx * (imgBuffer[4 * (position1 + deltaX)] & 0xff))))
                                            + (dy * ( (dx1 * (imgBuffer[4 * (position1 + deltaY)] & 0xff)) + (dx * (imgBuffer[4 * (position1
                                                    + deltaY + deltaX)] & 0xff))));

                                    b2 = (dy1 * ( (dx1 * (imgBuffer[4 * position2] & 0xff)) + (dx * (imgBuffer[4 * (position2 + deltaX)] & 0xff))))
                                            + (dy * ( (dx1 * (imgBuffer[4 * (position2 + deltaY)] & 0xff)) + (dx * (imgBuffer[4 * (position2
                                                    + deltaY + deltaX)] & 0xff))));

                                    imgBuffer2[temp] = (byte) ( ( (1 - dz) * b1) + (dz * b2) + 0.5f);

                                    b1 = (dy1 * ( (dx1 * (imgBuffer[4 * position1 + 1] & 0xff)) + (dx * (imgBuffer[4 * (position1 + deltaX) + 1] & 0xff))))
                                            + (dy * ( (dx1 * (imgBuffer[4 * (position1 + deltaY) + 1] & 0xff)) + (dx * (imgBuffer[4 * (position1
                                                    + deltaY + deltaX) + 1] & 0xff))));

                                    b2 = (dy1 * ( (dx1 * (imgBuffer[4 * position2 + 1] & 0xff)) + (dx * (imgBuffer[4 * (position2 + deltaX) + 1] & 0xff))))
                                            + (dy * ( (dx1 * (imgBuffer[4 * (position2 + deltaY) + 1] & 0xff)) + (dx * (imgBuffer[4 * (position2
                                                    + deltaY + deltaX) + 1] & 0xff))));

                                    imgBuffer2[temp + 1] = (byte) ( ( (1 - dz) * b1) + (dz * b2) + 0.5f);

                                    b1 = (dy1 * ( (dx1 * (imgBuffer[4 * position1 + 2] & 0xff)) + (dx * (imgBuffer[4 * (position1 + deltaX) + 2] & 0xff))))
                                            + (dy * ( (dx1 * (imgBuffer[4 * (position1 + deltaY) + 2] & 0xff)) + (dx * (imgBuffer[4 * (position1
                                                    + deltaY + deltaX) + 2] & 0xff))));

                                    b2 = (dy1 * ( (dx1 * (imgBuffer[4 * position2 + 2] & 0xff)) + (dx * (imgBuffer[4 * (position2 + deltaX) + 2] & 0xff))))
                                            + (dy * ( (dx1 * (imgBuffer[4 * (position2 + deltaY) + 2] & 0xff)) + (dx * (imgBuffer[4 * (position2
                                                    + deltaY + deltaX) + 2] & 0xff))));

                                    imgBuffer2[temp + 2] = (byte) ( ( (1 - dz) * b1) + (dz * b2) + 0.5f);

                                    b1 = (dy1 * ( (dx1 * (imgBuffer[4 * position1 + 3] & 0xff)) + (dx * (imgBuffer[4 * (position1 + deltaX) + 3] & 0xff))))
                                            + (dy * ( (dx1 * (imgBuffer[4 * (position1 + deltaY) + 3] & 0xff)) + (dx * (imgBuffer[4 * (position1
                                                    + deltaY + deltaX) + 3] & 0xff))));

                                    b2 = (dy1 * ( (dx1 * (imgBuffer[4 * position2 + 3] & 0xff)) + (dx * (imgBuffer[4 * (position2 + deltaX) + 3] & 0xff))))
                                            + (dy * ( (dx1 * (imgBuffer[4 * (position2 + deltaY) + 3] & 0xff)) + (dx * (imgBuffer[4 * (position2
                                                    + deltaY + deltaX) + 3] & 0xff))));

                                    imgBuffer2[temp + 3] = (byte) ( ( (1 - dz) * b1) + (dz * b2) + 0.5f);
                                } // end if Z in bounds
                            } // end if Y in bounds
                        } // end if X in bounds
                    } // end for i
                } // end for j
            } // end for k

            try {
                destImage.importData(4 * l * oVolSize, imgBuffer2, false);
            } catch (final IOException error) {
                MipavUtil.displayError("AlgorithmTransform: IOException on destImage.importData");
            }

            if (l < (oTdim - 1)) {

                try {
                    srcImage.exportData( (l + 1) * imgLength, imgLength, imgBuf);
                } catch (final IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);

                    return;
                }
            } // end if (l < (oTdim - 1))
        } // end for l
        destImage.calcMinMax();
    }

    /**
     * Transforms and resamples 4 dimensional color object using trilinear interpolation.
     * 
     * @param imgBuffer Image array
     * @param imgBuf2
     * @param kTM Transformation matrix to be applied
     */
    private void transformTrilinear4DC(final double[] imgBuffer, final float[] imgBuffer2, final TransMatrix kTM) {
        int i, j, k, l;
        int iAdj, jAdj, kAdj;
        double X, Y, Z;
        int x0, y0, z0;
        int temp;
        int sliceSize;
        int oSliceSize;
        int oVolSize;
        double imm, jmm, kmm;
        double k1, k2, k3, j1, j2, j3;

        sliceSize = iXdim * iYdim;
        oSliceSize = oXdim * oYdim;
        oVolSize = oSliceSize * oZdim;

        double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        int deltaX, deltaY, deltaZ;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T03 = (double)kTM.M03;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;
        T13 = (double)kTM.M13;
        T20 = (double)kTM.M20;
        T21 = (double)kTM.M21;
        T22 = (double)kTM.M22;
        T23 = (double)kTM.M23;

        int position1, position2;
        double b1, b2;
        double dx, dy, dz, dx1, dy1;

        final double invXRes = 1.0 / iXres;
        final double invYRes = 1.0 / iYres;
        final double invZRes = 1.0 / iZres;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged(Math.round((float) l / oTdim * 100));

            for (k = 0; (k < oZdim) && !threadStopped; k++) {
                if (pad) {
                    kAdj = k - AlgorithmTransform.margins[2];
                } else {
                    kAdj = k;
                }
                kmm = kAdj * oZres;
                k1 = (kmm * T02) + T03;
                k2 = (kmm * T12) + T13;
                k3 = (kmm * T22) + T23;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {
                    if (pad) {
                        jAdj = j - AlgorithmTransform.margins[1];
                    } else {
                        jAdj = j;
                    }
                    jmm = jAdj * oYres;
                    j1 = (jmm * T01) + k1;
                    j2 = (jmm * T11) + k2;
                    j3 = (jmm * T21) + k3;

                    for (i = 0; (i < oXdim) && !threadStopped; i++) {

                        // transform i,j,k
                        temp = 4 * (i + (j * oXdim) + (k * oSliceSize));
                        imgBuffer2[temp] = fillValue; // if voxel is transformed out of bounds
                        imgBuffer2[temp + 1] = fillValue;
                        imgBuffer2[temp + 2] = fillValue;
                        imgBuffer2[temp + 3] = fillValue;
                        if (pad) {
                            iAdj = i - AlgorithmTransform.margins[0];
                        } else {
                            iAdj = i;
                        }
                        imm = iAdj * oXres;
                        X = (j1 + (imm * T00)) * invXRes;

                        if ( (X > -0.5) && (X < iXdim)) {
                            Y = (j2 + (imm * T10)) * invYRes;

                            if ( (Y > -0.5) && (Y < iYdim)) {
                                Z = (j3 + (imm * T20)) * invZRes;

                                if ( (Z > -0.5) && (Z < iZdim)) {

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

                                    b1 = (dy1 * ( (dx1 * imgBuffer[4 * position1]) + (dx * imgBuffer[4 * (position1 + deltaX)])))
                                            + (dy * ( (dx1 * imgBuffer[4 * (position1 + deltaY)]) + (dx * imgBuffer[4 * (position1
                                                    + deltaY + deltaX)])));

                                    b2 = (dy1 * ( (dx1 * imgBuffer[4 * position2]) + (dx * imgBuffer[4 * (position2 + deltaX)])))
                                            + (dy * ( (dx1 * imgBuffer[4 * (position2 + deltaY)]) + (dx * imgBuffer[4 * (position2
                                                    + deltaY + deltaX)])));

                                    imgBuffer2[temp] = (float)(( (1 - dz) * b1) + (dz * b2));

                                    b1 = (dy1 * ( (dx1 * imgBuffer[4 * position1 + 1]) + (dx * imgBuffer[4 * (position1 + deltaX) + 1])))
                                            + (dy * ( (dx1 * imgBuffer[4 * (position1 + deltaY) + 1]) + (dx * imgBuffer[4 * (position1
                                                    + deltaY + deltaX) + 1])));

                                    b2 = (dy1 * ( (dx1 * imgBuffer[4 * position2 + 1]) + (dx * imgBuffer[4 * (position2 + deltaX) + 1])))
                                            + (dy * ( (dx1 * imgBuffer[4 * (position2 + deltaY) + 1]) + (dx * imgBuffer[4 * (position2
                                                    + deltaY + deltaX) + 1])));

                                    imgBuffer2[temp + 1] = (float)(( (1 - dz) * b1) + (dz * b2));

                                    b1 = (dy1 * ( (dx1 * imgBuffer[4 * position1 + 2]) + (dx * imgBuffer[4 * (position1 + deltaX) + 2])))
                                            + (dy * ( (dx1 * imgBuffer[4 * (position1 + deltaY) + 2]) + (dx * imgBuffer[4 * (position1
                                                    + deltaY + deltaX) + 2])));

                                    b2 = (dy1 * ( (dx1 * imgBuffer[4 * position2 + 2]) + (dx * imgBuffer[4 * (position2 + deltaX) + 2])))
                                            + (dy * ( (dx1 * imgBuffer[4 * (position2 + deltaY) + 2]) + (dx * imgBuffer[4 * (position2
                                                    + deltaY + deltaX) + 2])));

                                    imgBuffer2[temp + 2] = (float)(( (1 - dz) * b1) + (dz * b2));

                                    b1 = (dy1 * ( (dx1 * imgBuffer[4 * position1 + 3]) + (dx * imgBuffer[4 * (position1 + deltaX) + 3])))
                                            + (dy * ( (dx1 * imgBuffer[4 * (position1 + deltaY) + 3]) + (dx * imgBuffer[4 * (position1
                                                    + deltaY + deltaX) + 3])));

                                    b2 = (dy1 * ( (dx1 * imgBuffer[4 * position2 + 3]) + (dx * imgBuffer[4 * (position2 + deltaX) + 3])))
                                            + (dy * ( (dx1 * imgBuffer[4 * (position2 + deltaY) + 3]) + (dx * imgBuffer[4 * (position2
                                                    + deltaY + deltaX) + 3])));

                                    imgBuffer2[temp + 3] = (float)(( (1 - dz) * b1) + (dz * b2));
                                } // end if Z in bounds
                            } // end if Y in bounds
                        } // end if X in bounds
                    } // end for i
                } // end for j
            } // end for k

            try {
                destImage.importData(4 * l * oVolSize, imgBuffer2, false);
            } catch (final IOException error) {
                MipavUtil.displayError("AlgorithmTransform: IOException on destImage.importData");
            }

            if (l < (oTdim - 1)) {

                try {
                    srcImage.exportData( (l + 1) * imgLength, imgLength, imgBuf);
                } catch (final IOException error) {
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
     * @param imgBuffer Input image array
     * @param imgBuffer2 Output image array
     * @param kTM Transformation matrix to be applied
     */
    private void transformTrilinearC(final double[] imgBuffer, final float[] imgBuffer2, final TransMatrix kTM) {
        int i, j, k;
        int iAdj, jAdj, kAdj;
        double X, Y, Z;
        int x0, y0, z0;
        int sliceSize, osliceSize;
        double imm, jmm, kmm;
        double temp1, temp2, temp3;
        int temp8;
        int deltaX, deltaY, deltaZ;
        int position1, position2;
        double b1, b2;
        double dx, dy, dz, dx1, dy1;

        sliceSize = iXdim * iYdim;
        osliceSize = oXdim * oYdim;

        double i1, i2, i3, j1, j2, j3;
        double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T03 = (double)kTM.M03;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;
        T13 = (double)kTM.M13;
        T20 = (double)kTM.M20;
        T21 = (double)kTM.M21;
        T22 = (double)kTM.M22;
        T23 = (double)kTM.M23;

        for (i = 0; (i < oXdim) && !threadStopped; i++) {
            fireProgressStateChanged(Math.round((float) i / (oXdim) * 100));

            if (pad) {
                iAdj = i - AlgorithmTransform.margins[0];
            } else {
                iAdj = i;
            }

            imm = iAdj * oXres;
            i1 = (imm * T00) + T03;
            i2 = (imm * T10) + T13;
            i3 = (imm * T20) + T23;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                if (pad) {
                    jAdj = j - AlgorithmTransform.margins[1];
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
                    imgBuffer2[temp8] = fillValue; // if voxel is transformed out of bounds
                    imgBuffer2[temp8 + 1] = fillValue;
                    imgBuffer2[temp8 + 2] = fillValue;
                    imgBuffer2[temp8 + 3] = fillValue;
                    if (pad) {
                        kAdj = k - AlgorithmTransform.margins[2];
                    } else {
                        kAdj = k;
                    }

                    kmm = kAdj * oZres;
                    X = (temp3 + (kmm * T02)) / iXres;

                    if ( (X > -0.5) && (X < iXdim)) {
                        Y = (temp2 + (kmm * T12)) / iYres;

                        if ( (Y > -0.5) && (Y < iYdim)) {
                            Z = (temp1 + (kmm * T22)) / iZres;

                            if ( (Z > -0.5) && (Z < iZdim)) {
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

                                b1 = (dy1 * ( (dx1 * imgBuffer[4 * position1]) + (dx * imgBuffer[4 * (position1 + deltaX)])))
                                        + (dy * ( (dx1 * imgBuffer[4 * (position1 + deltaY)]) + (dx * imgBuffer[4 * (position1
                                                + deltaY + deltaX)])));

                                b2 = (dy1 * ( (dx1 * imgBuffer[4 * position2]) + (dx * imgBuffer[4 * (position2 + deltaX)])))
                                        + (dy * ( (dx1 * imgBuffer[4 * (position2 + deltaY)]) + (dx * imgBuffer[4 * (position2
                                                + deltaY + deltaX)])));

                                imgBuffer2[temp8] =(float)( ( (1 - dz) * b1) + (dz * b2));

                                b1 = (dy1 * ( (dx1 * imgBuffer[4 * position1 + 1]) + (dx * imgBuffer[4 * (position1 + deltaX) + 1])))
                                        + (dy * ( (dx1 * imgBuffer[4 * (position1 + deltaY) + 1]) + (dx * imgBuffer[4 * (position1
                                                + deltaY + deltaX) + 1])));

                                b2 = (dy1 * ( (dx1 * imgBuffer[4 * position2 + 1]) + (dx * imgBuffer[4 * (position2 + deltaX) + 1])))
                                        + (dy * ( (dx1 * imgBuffer[4 * (position2 + deltaY) + 1]) + (dx * imgBuffer[4 * (position2
                                                + deltaY + deltaX) + 1])));

                                imgBuffer2[temp8 + 1] = (float)(( (1 - dz) * b1) + (dz * b2));

                                b1 = (dy1 * ( (dx1 * imgBuffer[4 * position1 + 2]) + (dx * imgBuffer[4 * (position1 + deltaX) + 2])))
                                        + (dy * ( (dx1 * imgBuffer[4 * (position1 + deltaY) + 2]) + (dx * imgBuffer[4 * (position1
                                                + deltaY + deltaX) + 2])));

                                b2 = (dy1 * ( (dx1 * imgBuffer[4 * position2 + 2]) + (dx * imgBuffer[4 * (position2 + deltaX) + 2])))
                                        + (dy * ( (dx1 * imgBuffer[4 * (position2 + deltaY) + 2]) + (dx * imgBuffer[4 * (position2
                                                + deltaY + deltaX) + 2])));

                                imgBuffer2[temp8 + 2] = (float)(( (1 - dz) * b1) + (dz * b2));

                                b1 = (dy1 * ( (dx1 * imgBuffer[4 * position1 + 3]) + (dx * imgBuffer[4 * (position1 + deltaX) + 3])))
                                        + (dy * ( (dx1 * imgBuffer[4 * (position1 + deltaY) + 3]) + (dx * imgBuffer[4 * (position1
                                                + deltaY + deltaX) + 3])));

                                b2 = (dy1 * ( (dx1 * imgBuffer[4 * position2 + 3]) + (dx * imgBuffer[4 * (position2 + deltaX) + 3])))
                                        + (dy * ( (dx1 * imgBuffer[4 * (position2 + deltaY) + 3]) + (dx * imgBuffer[4 * (position2
                                                + deltaY + deltaX) + 3])));

                                imgBuffer2[temp8 + 3] = (float)(( (1 - dz) * b1) + (dz * b2));

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
        } catch (final IOException error) {
            MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
        }
    }

    /**
     * Transforms and resamples volume using trilinear interpolation This version is used with ARGB color images. Use
     * byte[] rather than float[] to save memory
     * 
     * @param imgBuffer Input image array
     * @param imgBuffer2 Output image array
     * @param kTM Transformation matrix to be applied
     */
    private void transformTrilinearByteC(final byte[] imgBuffer, final byte[] imgBuffer2, final TransMatrix kTM) {
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
        byte byteFillValue;

        if (fillValue > 255.0f) {
            byteFillValue = (byte) 255;
        } else if (fillValue < 0.0f) {
            byteFillValue = 0;
        } else {
            byteFillValue = (byte) (fillValue + 0.5f);
        }

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
                iAdj = i - AlgorithmTransform.margins[0];
            } else {
                iAdj = i;
            }

            imm = iAdj * oXres;
            i1 = (imm * T00) + T03;
            i2 = (imm * T10) + T13;
            i3 = (imm * T20) + T23;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                if (pad) {
                    jAdj = j - AlgorithmTransform.margins[1];
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
                    imgBuffer2[temp8] = byteFillValue; // if voxel is transformed out of bounds
                    imgBuffer2[temp8 + 1] = byteFillValue;
                    imgBuffer2[temp8 + 2] = byteFillValue;
                    imgBuffer2[temp8 + 3] = byteFillValue;
                    if (pad) {
                        kAdj = k - AlgorithmTransform.margins[2];
                    } else {
                        kAdj = k;
                    }

                    kmm = kAdj * oZres;
                    X = (temp3 + (kmm * T02)) / iXres;

                    if ( (X > -0.5f) && (X < iXdim)) {
                        Y = (temp2 + (kmm * T12)) / iYres;

                        if ( (Y > -0.5f) && (Y < iYdim)) {
                            Z = (temp1 + (kmm * T22)) / iZres;

                            if ( (Z > -0.5f) && (Z < iZdim)) {
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

                                b1 = (dy1 * ( (dx1 * (imgBuffer[4 * position1] & 0xff)) + (dx * (imgBuffer[4 * (position1 + deltaX)] & 0xff))))
                                        + (dy * ( (dx1 * (imgBuffer[4 * (position1 + deltaY)] & 0xff)) + (dx * (imgBuffer[4 * (position1
                                                + deltaY + deltaX)] & 0xff))));

                                b2 = (dy1 * ( (dx1 * (imgBuffer[4 * position2] & 0xff)) + (dx * (imgBuffer[4 * (position2 + deltaX)] & 0xff))))
                                        + (dy * ( (dx1 * (imgBuffer[4 * (position2 + deltaY)] & 0xff)) + (dx * (imgBuffer[4 * (position2
                                                + deltaY + deltaX)] & 0xff))));

                                imgBuffer2[temp8] = (byte) ( ( (1 - dz) * b1) + (dz * b2) + 0.5f);

                                b1 = (dy1 * ( (dx1 * (imgBuffer[4 * position1 + 1] & 0xff)) + (dx * (imgBuffer[4 * (position1 + deltaX) + 1] & 0xff))))
                                        + (dy * ( (dx1 * (imgBuffer[4 * (position1 + deltaY) + 1] & 0xff)) + (dx * (imgBuffer[4 * (position1
                                                + deltaY + deltaX) + 1] & 0xff))));

                                b2 = (dy1 * ( (dx1 * (imgBuffer[4 * position2 + 1] & 0xff)) + (dx * (imgBuffer[4 * (position2 + deltaX) + 1] & 0xff))))
                                        + (dy * ( (dx1 * (imgBuffer[4 * (position2 + deltaY) + 1] & 0xff)) + (dx * (imgBuffer[4 * (position2
                                                + deltaY + deltaX) + 1] & 0xff))));

                                imgBuffer2[temp8 + 1] = (byte) ( ( (1 - dz) * b1) + (dz * b2) + 0.5f);

                                b1 = (dy1 * ( (dx1 * (imgBuffer[4 * position1 + 2] & 0xff)) + (dx * (imgBuffer[4 * (position1 + deltaX) + 2] & 0xff))))
                                        + (dy * ( (dx1 * (imgBuffer[4 * (position1 + deltaY) + 2] & 0xff)) + (dx * (imgBuffer[4 * (position1
                                                + deltaY + deltaX) + 2] & 0xff))));

                                b2 = (dy1 * ( (dx1 * (imgBuffer[4 * position2 + 2] & 0xff)) + (dx * (imgBuffer[4 * (position2 + deltaX) + 2] & 0xff))))
                                        + (dy * ( (dx1 * (imgBuffer[4 * (position2 + deltaY) + 2] & 0xff)) + (dx * (imgBuffer[4 * (position2
                                                + deltaY + deltaX) + 2] & 0xff))));

                                imgBuffer2[temp8 + 2] = (byte) ( ( (1 - dz) * b1) + (dz * b2) + 0.5f);

                                b1 = (dy1 * ( (dx1 * (imgBuffer[4 * position1 + 3] & 0xff)) + (dx * (imgBuffer[4 * (position1 + deltaX) + 3] & 0xff))))
                                        + (dy * ( (dx1 * (imgBuffer[4 * (position1 + deltaY) + 3] & 0xff)) + (dx * (imgBuffer[4 * (position1
                                                + deltaY + deltaX) + 3] & 0xff))));

                                b2 = (dy1 * ( (dx1 * (imgBuffer[4 * position2 + 3] & 0xff)) + (dx * (imgBuffer[4 * (position2 + deltaX) + 3] & 0xff))))
                                        + (dy * ( (dx1 * (imgBuffer[4 * (position2 + deltaY) + 3] & 0xff)) + (dx * (imgBuffer[4 * (position2
                                                + deltaY + deltaX) + 3] & 0xff))));

                                imgBuffer2[temp8 + 3] = (byte) ( ( (1 - dz) * b1) + (dz * b2) + 0.5f);

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
        } catch (final IOException error) {
            MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
        }
    }

    /**
     * Transforms and resamples volume using windowed sinc interpolation.
     * 
     * @param imgBuf Image array
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformWSinc2D(final double[] imgBuf, final TransMatrix kTM, final boolean clip) {
        AlgorithmWSinc WSinc = new AlgorithmWSinc();
        int i, j;
        int iAdj, jAdj;
        double X, Y;
        double value;
        double imm, jmm;
        final int[] inVolExtents = {iXdim, iYdim};
        final int mod = Math.max(1, oXdim / 50);
        double temp1, temp2;
        double T00, T01, T02, T10, T11, T12;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        WSinc.setup2DWSinc(imgBuf, inVolExtents, clip);

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if ( ( (i % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) i / oXdim * 100) + 0.5));
            }

            if (pad) {
                iAdj = i - AlgorithmTransform.margins[0];
            } else {
                iAdj = i;
            }
            imm = iAdj * oXres;
            temp1 = (imm * T00) + T02;
            temp2 = (imm * T10) + T12;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                // convert to mm
                value = fillValue; // if voxel transformed out of bounds
                if (pad) {
                    jAdj = j - AlgorithmTransform.margins[1];
                } else {
                    jAdj = j;
                }
                jmm = jAdj * oYres;

                // transform i,j
                X = (temp1 + (jmm * T01)) / iXres;

                if ( (X >= 0) && (X < iXdim)) { // check bounds
                    Y = (temp2 + (jmm * T11)) / iYres;

                    if ( (Y >= 0) && (Y < iYdim)) {
                        value = WSinc.wSinc2D(X, Y);
                    }
                }

                destImage.set(i, j, value);
            }
        }

        WSinc.finalize();
        WSinc = null;
        Preferences.debug("finished windowed sinc", Preferences.DEBUG_ALGORITHM);
    }

    /**
     * Transforms and resamples volume using windowed sinc interpolation This version used with color images.
     * 
     * @param imgBuf Input image array
     * @param imgBuf2 Output image array
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformWSinc2DC(final double[] imgBuf, final float[] imgBuf2, final TransMatrix kTM,
            final boolean clip) {
        AlgorithmWSinc WSinc = new AlgorithmWSinc();
        int i, j;
        int iAdj, jAdj;
        double X, Y;
        double[] value = new double[4];
        double imm, jmm;
        final int[] inVolExtents = {iXdim, iYdim};
        final int mod = Math.max(1, oXdim / 50);
        double temp1, temp2;
        int temp3;
        double T00, T01, T02, T10, T11, T12;
        float argbMax = 255.0f;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        if (srcImage.getType() == ModelStorageBase.ARGB) {
            argbMax = 255.0f;
        } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
            argbMax = 65535.0f;
        }

        WSinc.setup2DWSincC(imgBuf, inVolExtents, argbMax, clip);

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if ( ( (i % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) i / oXdim * 100) + 0.5));
            }

            if (pad) {
                iAdj = i - AlgorithmTransform.margins[0];
            } else {
                iAdj = i;
            }
            imm = iAdj * oXres;
            temp1 = (imm * T00) + T02;
            temp2 = (imm * T10) + T12;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {

                // convert to mm
                value[0] = fillValue; // if voxel transformed out of bounds
                value[1] = fillValue;
                value[2] = fillValue;
                value[3] = fillValue;
                if (pad) {
                    jAdj = j - AlgorithmTransform.margins[1];
                } else {
                    jAdj = j;
                }
                jmm = jAdj * oYres;

                // transform i,j
                X = (temp1 + (jmm * T01)) / iXres;

                if ( (X >= 0) && (X < iXdim)) { // check bounds
                    Y = (temp2 + (jmm * T11)) / iYres;

                    if ( (Y >= 0) && (Y < iYdim)) {
                        value = WSinc.wSinc2DC(X, Y);
                    }
                }

                temp3 = 4 * (i + (j * oXdim));
                imgBuf2[temp3] = (float)value[0];
                imgBuf2[temp3 + 1] = (float)value[1];
                imgBuf2[temp3 + 2] = (float)value[2];
                imgBuf2[temp3 + 3] = (float)value[3];
            }
        }

        if (threadStopped) {
            return;
        }

        try {
            destImage.importData(0, imgBuf2, true);
        } catch (final IOException error) {
            MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
        }

        WSinc.finalize();
        WSinc = null;
        Preferences.debug("finished windowed sinc", Preferences.DEBUG_ALGORITHM);
    }

    /**
     * Transforms and resamples volume using windowed sinc interpolation.
     * 
     * @param imgBuf Image array
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformWSinc3D(final double[] imgBuf, final TransMatrix kTM, final boolean clip) {
        AlgorithmWSinc WSinc = new AlgorithmWSinc();
        int i, j, k;
        int iAdj, jAdj, kAdj;
        double X, Y, Z;
        double value;
        double imm, jmm, kmm;
        final int[] inVolExtents = {iXdim, iYdim, iZdim};
        final int mod = Math.max(1, oXdim / 50);

        double i1, i2, i3, j1, j2, j3;
        double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T03 = (double)kTM.M03;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;
        T13 = (double)kTM.M13;
        T20 = (double)kTM.M20;
        T21 = (double)kTM.M21;
        T22 = (double)kTM.M22;
        T23 = (double)kTM.M23;

        WSinc.setup3DWSinc(imgBuf, inVolExtents, clip);

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if ( ( (i % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) i / oXdim * 100) + 0.5));
            }

            if (pad) {
                iAdj = i - AlgorithmTransform.margins[0];
            } else {
                iAdj = i;
            }
            imm = iAdj * oXres;
            i1 = (imm * T00) + T03;
            i2 = (imm * T10) + T13;
            i3 = (imm * T20) + T23;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {
                if (pad) {
                    jAdj = j - AlgorithmTransform.margins[1];
                } else {
                    jAdj = j;
                }
                jmm = jAdj * oYres;
                j1 = (jmm * T01) + i1;
                j2 = (jmm * T11) + i2;
                j3 = (jmm * T21) + i3;

                for (k = 0; (k < oZdim) && !threadStopped; k++) {

                    // convert to mm
                    value = fillValue; // if voxel transformed out of bounds
                    if (pad) {
                        kAdj = k - AlgorithmTransform.margins[2];
                    } else {
                        kAdj = k;
                    }
                    kmm = kAdj * oZres;

                    // transform i,j,k
                    X = (j1 + (kmm * T02)) / iXres;

                    if ( (X >= 0) && (X < iXdim)) { // check bounds
                        Y = (j2 + (kmm * T12)) / iYres;

                        if ( (Y >= 0) && (Y < iYdim)) {
                            Z = (j3 + (kmm * T22)) / iZres;

                            if ( (Z >= 0) && (Z < iZdim)) {
                                value = WSinc.wSinc3D(X, Y, Z);
                            }
                        }
                    }

                    destImage.set(i, j, k, value);
                }
            }
        }

        WSinc.finalize();
        WSinc = null;
        Preferences.debug("finished windowed sinc", Preferences.DEBUG_ALGORITHM);
    }

    /**
     * Transforms and resamples volume using windowed sinc interpolation This version used with color images.
     * 
     * @param imgBuf Input image array
     * @param imgBuf2 Output image array
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformWSinc3DC(final double[] imgBuf, final float[] imgBuf2, final TransMatrix kTM,
            final boolean clip) {
        AlgorithmWSinc WSinc = new AlgorithmWSinc();
        int i, j, k;
        int iAdj, jAdj, kAdj;
        double X, Y, Z;
        double[] value = new double[4];
        double imm, jmm, kmm;
        final int[] inVolExtents = {iXdim, iYdim, iZdim};
        final int mod = Math.max(1, oXdim / 50);

        final int osliceSize = oXdim * oYdim;
        double i1, i2, i3, j1, j2, j3;
        double temp1, temp2, temp3;
        int temp4;
        double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        float argbMax = 255.0f;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T03 = (double)kTM.M03;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;
        T13 = (double)kTM.M13;
        T20 = (double)kTM.M20;
        T21 = (double)kTM.M21;
        T22 = (double)kTM.M22;
        T23 = (double)kTM.M23;

        if (srcImage.getType() == ModelStorageBase.ARGB) {
            argbMax = 255.0f;
        } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
            argbMax = 65535.0f;
        }

        WSinc.setup3DWSincC(imgBuf, inVolExtents, argbMax, clip);

        for (i = 0; (i < oXdim) && !threadStopped; i++) {

            if ( ( (i % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) i / oXdim * 100) + 0.5));
            }

            if (pad) {
                iAdj = i - AlgorithmTransform.margins[0];
            } else {
                iAdj = i;
            }
            imm = iAdj * oXres;
            i1 = (imm * T00) + T03;
            i2 = (imm * T10) + T13;
            i3 = (imm * T20) + T23;

            for (j = 0; (j < oYdim) && !threadStopped; j++) {
                if (pad) {
                    jAdj = j - AlgorithmTransform.margins[1];
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

                    // convert to mm
                    value[0] = fillValue; // if voxel transformed out of bounds
                    value[1] = fillValue;
                    value[2] = fillValue;
                    value[3] = fillValue;
                    if (pad) {
                        kAdj = k - AlgorithmTransform.margins[2];
                    } else {
                        kAdj = k;
                    }
                    kmm = kAdj * oZres;

                    // transform i,j,k
                    X = (temp3 + (kmm * T02)) / iXres;

                    // convert back to pixels
                    if ( (X >= 0) && (X < iXdim)) { // check bounds
                        Y = (temp2 + (kmm * T12)) / iYres;

                        if ( (Y >= 0) && (Y < iYdim)) {
                            Z = (temp1 + (kmm * T22)) / iZres;

                            if ( (Z >= 0) && (Z < iZdim)) {
                                value = WSinc.wSinc3DC(X, Y, Z);
                            }
                        }
                    }

                    temp4 = 4 * (i + (j * oXdim) + (k * osliceSize));
                    imgBuf2[temp4] = (float)value[0];
                    imgBuf2[temp4 + 1] = (float)value[1];
                    imgBuf2[temp4 + 2] = (float)value[2];
                    imgBuf2[temp4 + 3] = (float)value[3];
                }
            }
        }

        if (threadStopped) {
            return;
        }

        try {
            destImage.importData(0, imgBuf2, true);
        } catch (final IOException error) {
            MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
        }

        WSinc.finalize();
        WSinc = null;
        Preferences.debug("finished windowed sinc", Preferences.DEBUG_ALGORITHM);
    }

    /**
     * Transforms and resamples volume using windowed sinc interpolation Does a slice by slice windowed sinc
     * interpolation on a 3 dimensional object.
     * 
     * @param imgBuf Image array
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformWSinc3Dim2D(final double[] imgBuf, final TransMatrix kTM, final boolean clip) {
        AlgorithmWSinc WSinc = new AlgorithmWSinc();
        int i, j, k;
        int iAdj, jAdj;
        double X, Y;
        double value;
        double imm, jmm;
        final int[] inVolExtents = {iXdim, iYdim};

        final int mod = Math.max(1, oZdim / 50);
        double temp1, temp2;
        double T00, T01, T02, T10, T11, T12;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        for (k = 0; (k < oZdim) && !threadStopped; k++) {

            if ( ( (k % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) k / oZdim * 100) + 0.5));
            }

            WSinc.setup2DWSinc(imgBuf, inVolExtents, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                if (pad) {
                    iAdj = i - AlgorithmTransform.margins[0];
                } else {
                    iAdj = i;
                }
                imm = iAdj * oXres;
                temp1 = (imm * T00) + T02;
                temp2 = (imm * T10) + T12;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {

                    // convert to mm
                    value = fillValue; // if voxel transformed out of bounds
                    if (pad) {
                        jAdj = j - AlgorithmTransform.margins[1];
                    } else {
                        jAdj = j;
                    }
                    jmm = jAdj * oYres;

                    // transform i,j
                    X = (temp1 + (jmm * T01)) / iXres;

                    if ( (X >= 0) && (X < iXdim)) { // check bounds
                        Y = (temp2 + (jmm * T11)) / iYres;

                        if ( (Y >= 0) && (Y < iYdim)) {
                            value = WSinc.wSinc2D(X, Y);
                        }
                    }

                    destImage.set(i, j, k, value);
                }
            }

            if (k < (oZdim - 1)) {

                try {
                    srcImage.exportData( (k + 1) * imgLength, imgLength, imgBuf);
                } catch (final IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);

                    return;
                }
            } // end if (k < (oZdim - 1))
        } // end for k

        WSinc.finalize();
        WSinc = null;
        Preferences.debug("finished windowed sinc", Preferences.DEBUG_ALGORITHM);
    }

    /**
     * Transforms and resamples volume using windowed sinc interpolation This version used with color images This
     * version performs a slice by slice algorithm on a 3 dimensional object.
     * 
     * @param imgBuf Input image array
     * @param imgBuf2 Output image array
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformWSinc3Dim2DC(final double[] imgBuf, final float[] imgBuf2, final TransMatrix kTM,
            final boolean clip) {
        AlgorithmWSinc WSinc = new AlgorithmWSinc();
        int i, j, k;
        int iAdj, jAdj;
        double X, Y;
        double[] value = new double[4];
        double imm, jmm;
        final int[] inVolExtents = {iXdim, iYdim};

        final int mod = Math.max(1, oZdim / 50);
        double temp1, temp2;
        int temp3;
        double T00, T01, T02, T10, T11, T12;
        float argbMax = 255.0f;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        for (k = 0; (k < oZdim) && !threadStopped; k++) {

            if ( ( (k % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) k / oZdim * 100) + 0.5));
            }

            if (srcImage.getType() == ModelStorageBase.ARGB) {
                argbMax = 255.0f;
            } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
                argbMax = 65535.0f;
            }

            WSinc.setup2DWSincC(imgBuf, inVolExtents, argbMax, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                if (pad) {
                    iAdj = i - AlgorithmTransform.margins[0];
                } else {
                    iAdj = i;
                }
                imm = iAdj * oXres;
                temp1 = (imm * T00) + T02;
                temp2 = (imm * T10) + T12;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {

                    // convert to mm
                    value[0] = fillValue; // if voxel transformed out of bounds
                    value[1] = fillValue;
                    value[2] = fillValue;
                    value[3] = fillValue;

                    if (pad) {
                        jAdj = j - AlgorithmTransform.margins[1];
                    } else {
                        jAdj = j;
                    }
                    jmm = jAdj * oYres;

                    // transform i,j
                    X = (temp1 + (jmm * T01)) / iXres;

                    if ( (X >= 0) && (X < iXdim)) { // check bounds
                        Y = (temp2 + (jmm * T11)) / iYres;

                        if ( (Y >= 0) && (Y < iYdim)) {
                            value = WSinc.wSinc2DC(X, Y);
                        }
                    }

                    temp3 = 4 * (i + (j * oXdim));
                    imgBuf2[temp3] = (float)value[0];
                    imgBuf2[temp3 + 1] = (float)value[1];
                    imgBuf2[temp3 + 2] = (float)value[2];
                    imgBuf2[temp3 + 3] = (float)value[3];
                } // for i
            } // for j

            try {
                destImage.importData(4 * k * oXdim * oYdim, imgBuf2, true);
            } catch (final IOException error) {
                MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
            }

            if (k < (oZdim - 1)) {

                try {
                    srcImage.exportData( (k + 1) * imgLength, imgLength, imgBuf);
                } catch (final IOException error) {
                    displayError("Algorithm Transform: IOException Error on importData");
                    setCompleted(false);

                    return;
                }
            } // end if (k < (oZdim - 1))
        } // end for k

        WSinc.finalize();
        WSinc = null;
        Preferences.debug("finished windowed sinc", Preferences.DEBUG_ALGORITHM);
    }

    /**
     * Transforms and resamples 4 dimensional object using 3D windowed sinc interpolation.
     * 
     * @param imgBuf Image array
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformWSinc4D(final double[] imgBuf, final TransMatrix kTM, final boolean clip) {
        AlgorithmWSinc WSinc = new AlgorithmWSinc();
        int i, j, k, l;
        int iAdj, jAdj, kAdj;
        double X, Y, Z;
        double value;
        double imm, jmm, kmm;
        final int[] inVolExtents = {iXdim, iYdim, iZdim};
        final int mod = Math.max(1, oTdim / 50);

        double i1, i2, i3, j1, j2, j3;
        double temp1, temp2, temp3;
        double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T03 = (double)kTM.M03;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;
        T13 = (double)kTM.M13;
        T20 = (double)kTM.M20;
        T21 = (double)kTM.M21;
        T22 = (double)kTM.M22;
        T23 = (double)kTM.M23;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {

            if ( ( (l % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) l / oTdim * 100) + .5));
            }

            WSinc.setup3DWSinc(imgBuf, inVolExtents, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                if (pad) {
                    iAdj = i - AlgorithmTransform.margins[0];
                } else {
                    iAdj = i;
                }
                imm = iAdj * oXres;
                i1 = (imm * T00) + T03;
                i2 = (imm * T10) + T13;
                i3 = (imm * T20) + T23;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {
                    if (pad) {
                        jAdj = j - AlgorithmTransform.margins[1];
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

                        // convert to mm
                        value = fillValue; // if voxel transformed out of bounds
                        if (pad) {
                            kAdj = k - AlgorithmTransform.margins[2];
                        } else {
                            kAdj = k;
                        }
                        kmm = kAdj * oZres;

                        // transform i,j,k
                        X = (temp3 + (kmm * T02)) / iXres;

                        if ( (X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (kmm * T12)) / iYres;

                            if ( (Y >= 0) && (Y < iYdim)) {
                                Z = (temp1 + (kmm * T22)) / iZres;

                                if ( (Z >= 0) && (Z < iZdim)) {
                                    value = WSinc.wSinc3D(X, Y, Z);
                                }
                            }
                        }

                        destImage.set(i, j, k, l, value);
                    }
                }
            }

            if (l < (oTdim - 1)) {

                try {
                    srcImage.exportData( (l + 1) * imgLength, imgLength, imgBuf);
                } catch (final IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);

                    return;
                }
            } // end if (l < (oTdim - 1))
        } // for l

        WSinc.finalize();
        WSinc = null;
        Preferences.debug("finished windowed sinc", Preferences.DEBUG_ALGORITHM);
    }

    /**
     * Transforms and resamples 4 dimensional object using 3D windowed sinc interpolation.
     * 
     * @param imgBuf Image array
     * @param imgBuffer2
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformWSinc4DC(final double[] imgBuf, final float[] imgBuffer2, final TransMatrix kTM,
            final boolean clip) {
        AlgorithmWSinc WSinc = new AlgorithmWSinc();
        int i, j, k, l;
        int iAdj, jAdj, kAdj;
        double X, Y, Z;
        double value[] = new double[4];
        int temp4;
        double imm, jmm, kmm;
        final int[] inVolExtents = {iXdim, iYdim, iZdim};
        int oSliceSize;
        int oVolSize;
        final int mod = Math.max(1, oTdim / 50);

        double i1, i2, i3, j1, j2, j3;
        double temp1, temp2, temp3;
        double T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;

        float argbMax = 255.0f;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T03 = (double)kTM.M03;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;
        T13 = (double)kTM.M13;
        T20 = (double)kTM.M20;
        T21 = (double)kTM.M21;
        T22 = (double)kTM.M22;
        T23 = (double)kTM.M23;

        oSliceSize = oXdim * oYdim;
        oVolSize = oSliceSize * oZdim;

        if (srcImage.getType() == ModelStorageBase.ARGB) {
            argbMax = 255.0f;
        } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
            argbMax = 65535.0f;
        }

        for (l = 0; (l < oTdim) && !threadStopped; l++) {

            if ( ( (l % mod) == 0)) {
                fireProgressStateChanged((int) ( ((float) l / oTdim * 100) + .5));
            }

            WSinc.setup3DWSincC(imgBuf, inVolExtents, argbMax, clip);

            for (i = 0; (i < oXdim) && !threadStopped; i++) {
                if (pad) {
                    iAdj = i - AlgorithmTransform.margins[0];
                } else {
                    iAdj = i;
                }
                imm = iAdj * oXres;
                i1 = (imm * T00) + T03;
                i2 = (imm * T10) + T13;
                i3 = (imm * T20) + T23;

                for (j = 0; (j < oYdim) && !threadStopped; j++) {
                    if (pad) {
                        jAdj = j - AlgorithmTransform.margins[1];
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

                        // convert to mm
                        value[0] = fillValue; // if voxel transformed out of bounds
                        value[1] = fillValue;
                        value[2] = fillValue;
                        value[3] = fillValue;
                        if (pad) {
                            kAdj = k - AlgorithmTransform.margins[2];
                        } else {
                            kAdj = k;
                        }
                        kmm = kAdj * oZres;

                        // transform i,j,k
                        X = (temp3 + (kmm * T02)) / iXres;

                        if ( (X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (kmm * T12)) / iYres;

                            if ( (Y >= 0) && (Y < iYdim)) {
                                Z = (temp1 + (kmm * T22)) / iZres;

                                if ( (Z >= 0) && (Z < iZdim)) {
                                    value = WSinc.wSinc3DC(X, Y, Z);
                                }
                            }
                        }

                        temp4 = 4 * (i + (j * oXdim) + (k * oSliceSize));
                        imgBuffer2[temp4] = (float)value[0];
                        imgBuffer2[temp4 + 1] = (float)value[1];
                        imgBuffer2[temp4 + 2] = (float)value[2];
                        imgBuffer2[temp4 + 3] = (float)value[3];
                    }
                }
            }

            try {
                destImage.importData(4 * l * oVolSize, imgBuffer2, false);
            } catch (final IOException error) {
                MipavUtil.displayError("AlgorithmTransform: IOException on destImage.importData");
            }

            if (l < (oTdim - 1)) {

                try {
                    srcImage.exportData( (l + 1) * imgLength, imgLength, imgBuf);
                } catch (final IOException error) {
                    displayError("Algorithm Transform: Image(s) locked");
                    setCompleted(false);

                    return;
                }
            } // end if (l < (oTdim - 1))
        } // for l

        destImage.calcMinMax();
        WSinc.finalize();
        WSinc = null;
        Preferences.debug("finished windowed sinc", Preferences.DEBUG_ALGORITHM);
    }

    /**
     * Transforms and resamples volume using windowed sinc interpolation Does a slice by slice windowed sinc
     * interpolation on a 4 dimensional object.
     * 
     * @param imgBuf Image array
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</true> clip output values to be within input range</code>
     */
    private void transformWSinc4Dim2D(final double[] imgBuf, final TransMatrix kTM, final boolean clip) {
        AlgorithmWSinc WSinc = new AlgorithmWSinc();
        int i, j, k, l;
        int iAdj, jAdj;
        double X, Y;
        double value;
        double imm, jmm;
        final int[] inVolExtents = {iXdim, iYdim};

        double temp1, temp2;
        double T00, T01, T02, T10, T11, T12;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged((int) ( ((float) l / oTdim * 100) + 0.5));

            for (k = 0; (k < oZdim) && !threadStopped; k++) {
                WSinc.setup2DWSinc(imgBuf, inVolExtents, clip);

                for (i = 0; (i < oXdim) && !threadStopped; i++) {
                    if (pad) {
                        iAdj = i - AlgorithmTransform.margins[0];
                    } else {
                        iAdj = i;
                    }
                    imm = iAdj * oXres;
                    temp1 = (imm * T00) + T02;
                    temp2 = (imm * T10) + T12;

                    for (j = 0; (j < oYdim) && !threadStopped; j++) {

                        // convert to mm
                        value = fillValue; // if voxel transformed out of bounds
                        if (pad) {
                            jAdj = j - AlgorithmTransform.margins[1];
                        } else {
                            jAdj = j;
                        }
                        jmm = jAdj * oYres;

                        // transform i,j
                        X = (temp1 + (jmm * T01)) / iXres;

                        if ( (X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (jmm * T11)) / iYres;

                            if ( (Y >= 0) && (Y < iYdim)) {
                                value = WSinc.wSinc2D(X, Y);
                            }
                        }

                        destImage.set(i, j, k, l, value);
                    }
                }

                if ( (k < (oZdim - 1)) || (l < (oTdim - 1))) {

                    try {
                        srcImage.exportData( (l * oZdim * imgLength) + ( (k + 1) * imgLength), imgLength, imgBuf);
                    } catch (final IOException error) {
                        displayError("Algorithm Transform: Image(s) locked");
                        setCompleted(false);

                        return;
                    }
                } // end if ((k < (oZdim - 1))|| (l < (oTdim - 1)))
            } // end for k
        } // end for l

        WSinc.finalize();
        WSinc = null;
        Preferences.debug("finished windowed sinc", Preferences.DEBUG_ALGORITHM);
    }

    /**
     * Transforms and resamples volume using windowed sinc interpolation This version used with color images This
     * version performs a slice by slice algorithm on a 4 dimensional object.
     * 
     * @param imgBuf Input image array
     * @param imgBuf2 Output image array
     * @param kTM Transformation matrix to be applied
     * @param clip If <code>true</code> clip output values to be within input range
     */
    private void transformWSinc4Dim2DC(final double[] imgBuf, final float[] imgBuf2, final TransMatrix kTM,
            final boolean clip) {
        AlgorithmWSinc WSinc = new AlgorithmWSinc();
        int i, j, k, l;
        int iAdj, jAdj;
        double X, Y;
        double[] value = new double[4];
        double imm, jmm;
        final int[] inVolExtents = {iXdim, iYdim};

        double temp1, temp2;
        int temp3;
        double T00, T01, T02, T10, T11, T12;
        float argbMax = 255.0f;

        T00 = (double)kTM.M00;
        T01 = (double)kTM.M01;
        T02 = (double)kTM.M02;
        T10 = (double)kTM.M10;
        T11 = (double)kTM.M11;
        T12 = (double)kTM.M12;

        for (l = 0; (l < oTdim) && !threadStopped; l++) {
            fireProgressStateChanged((int) ( ((float) l / oTdim * 100) + 0.5));

            for (k = 0; (k < oZdim) && !threadStopped; k++) {

                if (srcImage.getType() == ModelStorageBase.ARGB) {
                    argbMax = 255.0f;
                } else if (srcImage.getType() == ModelStorageBase.ARGB_USHORT) {
                    argbMax = 65535.0f;
                }

                WSinc.setup2DWSincC(imgBuf, inVolExtents, argbMax, clip);

                for (i = 0; (i < oXdim) && !threadStopped; i++) {
                    if (pad) {
                        iAdj = i - AlgorithmTransform.margins[0];
                    } else {
                        iAdj = i;
                    }
                    imm = iAdj * oXres;
                    temp1 = (imm * T00) + T02;
                    temp2 = (imm * T10) + T12;

                    for (j = 0; (j < oYdim) && !threadStopped; j++) {

                        // convert to mm
                        value[0] = fillValue; // if voxel transformed out of bounds
                        value[1] = fillValue;
                        value[2] = fillValue;
                        value[3] = fillValue;
                        if (pad) {
                            jAdj = j - AlgorithmTransform.margins[1];
                        } else {
                            jAdj = j;
                        }
                        jmm = jAdj * oYres;

                        // transform i,j
                        X = (temp1 + (jmm * T01)) / iXres;

                        if ( (X >= 0) && (X < iXdim)) { // check bounds
                            Y = (temp2 + (jmm * T11)) / iYres;

                            if ( (Y >= 0) && (Y < iYdim)) {
                                value = WSinc.wSinc2DC(X, Y);
                            }
                        }

                        temp3 = 4 * (i + (j * oXdim));
                        imgBuf2[temp3] = (float)value[0];
                        imgBuf2[temp3 + 1] = (float)value[1];
                        imgBuf2[temp3 + 2] = (float)value[2];
                        imgBuf2[temp3 + 3] = (float)value[3];
                    } // for i
                } // for j

                try {
                    destImage.importData( (4 * l * oXdim * oYdim * oZdim) + (4 * k * oXdim * oYdim), imgBuf2, true);
                } catch (final IOException error) {
                    MipavUtil.displayError("AlgorithmTransform: IOException Error on importData");
                }

                if ( (k < (oZdim - 1)) || (l < (oTdim - 1))) {

                    try {
                        srcImage.exportData( (l * oZdim * imgLength) + ( (k + 1) * imgLength), imgLength, imgBuf);
                    } catch (final IOException error) {
                        displayError("Algorithm Transform: IOException Error on importData");
                        setCompleted(false);

                        return;
                    }
                } // end if ((k < (oZdim - 1))|| (l < (oTdim - 1)))
            } // end for k
        } // end for l

        WSinc.finalize();
        WSinc = null;
        Preferences.debug("finished windowed sinc", Preferences.DEBUG_ALGORITHM);
    }

    /**
     * Update origin to reflect padding.
     */
    private void updateOriginMargins() {
        float tx, ty, tz;

        tx = AlgorithmTransform.direct[0] * AlgorithmTransform.margins[0] * oXres;
        ty = AlgorithmTransform.direct[1] * AlgorithmTransform.margins[1] * oYres;
        tz = AlgorithmTransform.direct[2] * AlgorithmTransform.margins[2] * oZres;

        // System.out.println("Image origin before padding: " +imgOrigin[0] +" " +imgOrigin[1] +" " +imgOrigin[2]);
        AlgorithmTransform.imgOrigin[0] -= tx;
        AlgorithmTransform.imgOrigin[1] -= ty;
        AlgorithmTransform.imgOrigin[2] -= tz;
    }

    /**
     * Update origin to reflect padding.
     */
    private void updateOriginMargins2D() {
        float tx, ty;

        tx = AlgorithmTransform.direct[0] * AlgorithmTransform.margins[0] * oXres;
        ty = AlgorithmTransform.direct[1] * AlgorithmTransform.margins[1] * oYres;

        // System.out.println("Image origin before padding: " +imgOrigin[0] +" " +imgOrigin[1]);
        AlgorithmTransform.imgOrigin[0] -= tx;
        AlgorithmTransform.imgOrigin[1] -= ty;
    }


   
}
