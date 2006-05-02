package gov.nih.mipav.view;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.icons.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;


/**
 * DOCUMENT ME!
 *
 * @author  William Gandler
 */
public class ViewJComponentRegistration extends ViewJComponentBase implements MouseMotionListener, MouseListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4961820290271772572L;

    /** The 3 types of markers - rotation center, reference slice, and adjusted slice. */
    public static final int ROTATIONCENTER = 0;

    /** DOCUMENT ME! */
    public static final int REFMARK = 1;

    /** DOCUMENT ME! */
    public static final int ADJMARK = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public JDialogCheckerBoard checkerRegDialog = null;

    /** Opacity value used by the paint brush. value = 1.0 - opaque value = 0.25 - default (mostly see through) */
    public float OPACITY = 0.25f;

    /** DOCUMENT ME! */
    protected Cursor addPointCursor = new Cursor(Cursor.HAND_CURSOR);

    /** alphaBlending values for compositing two images. */
    protected float alphaBlend = 0.5f;

    /** DOCUMENT ME! */
    protected float alphaPrime = 0.5f;

    /** DOCUMENT ME! */
    protected BitSet checkerRegBitmap = null;

    /** protected Cursor crosshairCursor = new Cursor(Cursor.CROSSHAIR_CURSOR);. */
    protected Cursor crosshairCursor = new Cursor(Cursor.DEFAULT_CURSOR);

    /** DOCUMENT ME! */
    protected Cursor defaultCursor = new Cursor(Cursor.DEFAULT_CURSOR);

    /** Frame where the component image is displayed. */
    protected ViewJFrameRegistration frame;

    /** Model for image A. */
    protected ModelImage imageA;

    /** Model for image B. */
    protected ModelImage imageB;

    /** Lookup table for image A. */
    protected ModelLUT LUTa;

    /** Lookup table for image B. */
    protected ModelLUT LUTb;

    /** DOCUMENT ME! */
    protected int[] lutBufferRemapped = null;

    /** Custom cursor: magnify region. */
    protected Cursor magRegionCursor;

    /** mode - used to describe the cursor mode. */
    protected int mode;

    /** DOCUMENT ME! */
    protected Cursor moveCursor = new Cursor(Cursor.MOVE_CURSOR);

    /** DOCUMENT ME! */
    protected int orientation = NA;

    /** DOCUMENT ME! */
    protected Cursor pointCursor = new Cursor(Cursor.NE_RESIZE_CURSOR);

    /** DOCUMENT ME! */
    protected Cursor rectCursor = new Cursor(Cursor.DEFAULT_CURSOR);

    /** DOCUMENT ME! */
    protected Cursor removePointCursor = new Cursor(Cursor.E_RESIZE_CURSOR);

    /** DOCUMENT ME! */
    protected ModelRGB RGBTA = null;

    /** DOCUMENT ME! */
    protected ModelRGB RGBTB = null;

    /** DOCUMENT ME! */
    protected int slice = -99;

    /** DOCUMENT ME! */
    protected long time;

    /** DOCUMENT ME! */
    protected int timeSlice = 0;

    /** Frame where component image is displayed (only for new RegistrationTool). */
    protected ViewJFrameRegistrationTool toolFrame;

    /** DOCUMENT ME! */
    protected Cursor waitCursor = new Cursor(Cursor.WAIT_CURSOR);

    /** DOCUMENT ME! */
    protected Cursor wandCursor = new Cursor(Cursor.HAND_CURSOR);

    /** DOCUMENT ME! */
    int memCount = 0;

    /** DOCUMENT ME! */
    private int adjMark = 0; // number of VOIs for adjustable slice

    /** DOCUMENT ME! */
    private Point anchorPt = new Point(0, 0);

    /** DOCUMENT ME! */
    private short centerid; // id of rotation center

    /** DOCUMENT ME! */
    private VOI centerVOI = null; // center roation VOI

    /** DOCUMENT ME! */
    private int columnCheckers = -1;

    /** DOCUMENT ME! */
    private boolean doAdjMark = false;

    /** DOCUMENT ME! */
    private boolean doCenter = false;

    /** DOCUMENT ME! */
    private boolean doDrag = true; // no adjusted slice movements with mouseDrag

    /** DOCUMENT ME! */
    private boolean doRefMark = false;

    /** DOCUMENT ME! */
    private float hue;

    /** DOCUMENT ME! */
    private short id;

    /** DOCUMENT ME! */
    private ModelImage imageActive = null;

    /** DOCUMENT ME! */
    private float[] imageBufferA = null;

    /** DOCUMENT ME! */
    private float[] imageBufferActive = null;

    /** DOCUMENT ME! */
    private float[] imageBufferB = null;

    /** DOCUMENT ME! */
    private int[] imageExtents;

    /** DOCUMENT ME! */
    private boolean logMagDisplay = false;

    /** DOCUMENT ME! */
    private int[] markerType;

    /** Used to "lock" display when an algorithm is in the calculation process. */
    private boolean modifyFlag = true;

    /** DOCUMENT ME! */
    private VOI newVOI = null;

    /** adjustable slice VOI. */
    private int newX, newY;

    /** DOCUMENT ME! */
    private int nMovingVOI = 0; // number of rotation center and

    /** DOCUMENT ME! */
    private int nPts = 0;

    /** Used to set old or new mode of registration. */
    private boolean oldFrame = true;

    /** Buffer used to indicate if the pixel location is painted (true) or unpainted (false). */
    private BitSet paintBitmap;

    /** Buffer that displays the combined paintBitmap and pixBuffer buffers. */
    private int[] paintBuffer = null;

    /** Buffer used to store ARGB images of the image presently being displayed. */
    private int[] pixBuffer = null;

    /** Buffer used to store ARGB image of the windowed imageB. */
    private int[] pixBufferB = null;

    /** DOCUMENT ME! */
    private float[] ptCoord;

    /** DOCUMENT ME! */
    private int red, green, blue;

    /** DOCUMENT ME! */
    private int refMark = 0; // number of VOIs for reference slice

    /** DOCUMENT ME! */
    private int rotCenterMark = 0; // number of VOIs for rotation center

    /** checkerboard parameters. */
    private int rowCheckers = -1;

    /** DOCUMENT ME! */
    private boolean showVOIs = true;

    /** DOCUMENT ME! */
    private boolean useDualVOIs = false; // to use VOIs from both imageA and imageB

    /** DOCUMENT ME! */
    private int[] x, y;

    /** DOCUMENT ME! */
    private float xFinish, yFinish; // values obtained when mouseReleased

    /** DOCUMENT ME! */
    private int[] xOrg;

    /** DOCUMENT ME! */
    private int[] xPres;

    /** DOCUMENT ME! */
    private float xRotation;

    /** DOCUMENT ME! */
    private float xStart, yStart; // values obtained when mousePressed

    /** DOCUMENT ME! */
    private int[] yOrg;

    /** DOCUMENT ME! */
    private int[] yPres;

    /** DOCUMENT ME! */
    private float yRotation;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor: ImageA and ImageB are expected to be of the same dimensionality !!
     *
     * @param  _frame         frame where image(s) will be displayed
     * @param  _imageA        Model of the image that will be displayed
     * @param  _LUTa          LUT used to display imageA
     * @param  imgBufferA     storage buffer used to display image A
     * @param  _imageB        Model of the image that will be displayed
     * @param  _LUTb          LUT used to display imageB
     * @param  imgBufferB     storage buffer used to display image B
     * @param  pixelBuffer    storage buffer used to build a displayable image
     * @param  zoom           initial magnification of image
     * @param  extents        initial display dimensions of the image
     * @param  logMagDisplay  display log magnitude of image
     * @param  _orientation   orientation of the image
     * @param  alphaBl        DOCUMENT ME!
     */
    public ViewJComponentRegistration(ViewJFrameRegistration _frame, ModelImage _imageA, ModelLUT _LUTa,
                                      float[] imgBufferA, ModelImage _imageB, ModelLUT _LUTb, float[] imgBufferB,
                                      int[] pixelBuffer, float zoom, int[] extents, boolean logMagDisplay,
                                      int _orientation, float alphaBl) {
        super(new Dimension(extents[0], extents[1]));

        frame = _frame;
        imageA = _imageA;
        imageB = _imageB;
        imageActive = imageA;
        imageExtents = extents;

        orientation = _orientation;
        alphaBlend = alphaBl;

        LUTa = _LUTa;
        LUTb = _LUTb;
        lutBufferRemapped = new int[1];
        xOrg = new int[40];
        yOrg = new int[40];
        xPres = new int[40];
        yPres = new int[40];
        markerType = new int[40];
        ptCoord = new float[2];

        mode = DEFAULT;

        imageBufferA = imgBufferA;
        imageBufferB = imgBufferB;
        pixBuffer = pixelBuffer;

        paintBitmap = imageA.getMask();

        imageBufferActive = imageBufferA;
        this.logMagDisplay = logMagDisplay;

        setZoom(zoom, zoom);

        addMouseMotionListener(this);
        addMouseListener(this);

        try { // this try does not work... sun didn't bother to propogate the exception ... maybe someday ...

            // img = Toolkit.getDefaultToolkit().getImage(PlaceHolder.class.getResource("emptycursor.gif"));
            img = MipavUtil.getIconImage("emptycursor.gif");

            magRegionCursor = Toolkit.getDefaultToolkit().createCustomCursor(img, new Point(12, 12), "Magnification");
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                              ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <" + error.getMessage() +
                               ">.  Check that this file is available.\n");
            magRegionCursor = crosshairCursor;
        }

        setVisible(true);

    }

    /**
     * Constructor: ImageA and ImageB are expected to be of the same dimensionality !!
     *
     * @param  _toolFrame     frame where image(s) will be displayed
     * @param  _imageA        Model of the image that will be displayed
     * @param  _LUTa          LUT used to display imageA
     * @param  imgBufferA     storage buffer used to display image A
     * @param  _imageB        Model of the image that will be displayed
     * @param  _LUTb          LUT used to display imageB
     * @param  imgBufferB     storage buffer used to display image B
     * @param  pixelBuffer    storage buffer used to build a displayable image
     * @param  zoom           initial magnification of image
     * @param  extents        initial display dimensions of the image
     * @param  logMagDisplay  display log magnitude of image
     * @param  _orientation   orientation of the image
     * @param  alphaBl        DOCUMENT ME!
     */
    public ViewJComponentRegistration(ViewJFrameRegistrationTool _toolFrame, ModelImage _imageA, ModelLUT _LUTa,
                                      float[] imgBufferA, ModelImage _imageB, ModelLUT _LUTb, float[] imgBufferB,
                                      int[] pixelBuffer, float zoom, int[] extents, boolean logMagDisplay,
                                      int _orientation, float alphaBl) {
        super(new Dimension(extents[0], extents[1]));

        oldFrame = false;
        toolFrame = _toolFrame;
        imageA = _imageA;
        imageB = _imageB;
        imageActive = imageA;
        imageExtents = extents;

        orientation = _orientation;
        alphaBlend = alphaBl;

        LUTa = _LUTa;
        LUTb = _LUTb;
        lutBufferRemapped = new int[1];
        xOrg = new int[40];
        yOrg = new int[40];
        xPres = new int[40];
        yPres = new int[40];
        markerType = new int[40];
        ptCoord = new float[2];

        mode = DEFAULT;

        imageBufferA = imgBufferA;
        imageBufferB = imgBufferB;
        pixBuffer = pixelBuffer;

        paintBitmap = imageA.getMask();

        imageBufferActive = imageBufferA;
        this.logMagDisplay = logMagDisplay;

        setZoom(zoom, zoom);

        addMouseMotionListener(this);
        addMouseListener(this);

        try { // this try does not work... sun didn't bother to propogate the exception ... maybe someday ...

            // img = Toolkit.getDefaultToolkit().getImage(PlaceHolder.class.getResource("emptycursor.gif"));
            img = MipavUtil.getIconImage("emptycursor.gif");

            magRegionCursor = Toolkit.getDefaultToolkit().createCustomCursor(img, new Point(12, 12), "Magnification");
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                              ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <" + error.getMessage() +
                               ">.  Check that this file is available.\n");
            magRegionCursor = crosshairCursor;
        }

        setVisible(true);

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Deletes all ROTATIONCENTER and ADJMARK VOIs.
     */
    public void deleteAdjRotVOIs() {
        int i;
        int nVOI;

        ViewVOIVector VOIs = imageActive.getVOIs();

        nVOI = VOIs.size();

        if (nVOI == 0) {
            return;
        }

        for (i = (nVOI - 1); i >= 0; i--) {

            if ((markerType[i] == ROTATIONCENTER) || (markerType[i] == ADJMARK)) {
                VOIs.removeElementAt(i);
            }
        }

        nVOI = VOIs.size(); // number of REFMARK VOIs

        for (i = 0; i < nVOI; i++) {
            VOIs.VOIAt(i).getPointCoordinates(ptCoord);
            xOrg[i] = Math.round(ptCoord[0]);
            yOrg[i] = Math.round(ptCoord[1]);
            markerType[i] = REFMARK;
        }

        newVOI = null;
        id = (short) nVOI;
        rotCenterMark = 0;
        adjMark = 0;
        refMark = nVOI;
        // imageActive.notifyImageDisplayListeners(null,true);
    }

    /**
     * Deletes all ADJMARK VOIs.
     */
    public void deleteAdjustableVOIs() {
        int i;
        int j = -1;
        int nVOI;

        ViewVOIVector VOIs = imageActive.getVOIs();

        nVOI = VOIs.size();

        if (nVOI == 0) {
            return;
        }

        for (i = (nVOI - 1); i >= 0; i--) {

            if (markerType[i] == ROTATIONCENTER) {
                j = i;
            } else if (markerType[i] == ADJMARK) {
                VOIs.removeElementAt(i);
                j--;
            }
        }

        nVOI = VOIs.size(); // number of ROTATIONCENTER AND REFMARK VOIs

        for (i = 0; i < nVOI; i++) {
            VOIs.VOIAt(i).getPointCoordinates(ptCoord);
            xOrg[i] = Math.round(ptCoord[0]);
            yOrg[i] = Math.round(ptCoord[1]);

            if (i == j) {
                markerType[i] = ROTATIONCENTER;
            } else {
                markerType[i] = REFMARK;
            }
        }

        newVOI = null;
        id = (short) nVOI;
        adjMark = 0;

        if (rotCenterMark == 1) {
            refMark = nVOI - 1;
        } else {
            refMark = nVOI;
        }
        // imageActive.notifyImageDisplayListeners(null,true);
    }

    /**
     * If a point is selected of the given type (reference or adjusted) delete the point and reset the.
     *
     * @param  isReference  delete point if of this type
     */
    public void deletePoint(boolean isReference) {
        int i, s, nVOI;

        ViewVOIVector VOIs = imageActive.getVOIs();

        nVOI = VOIs.size();

        if (nVOI == 0) {
            return;
        }

        for (i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).isActive() == true) {
                break;
            } // Set i
        }

        if (i == nVOI) {
            return;
        } else if (isReference && (markerType[i] != REFMARK)) {
            MipavUtil.displayError("Must select a reference VOI.");

            return;
        } else if (!isReference && (markerType[i] != ADJMARK)) {
            MipavUtil.displayError("Must select an adjusted VOI.");

            return;
        }

        imageActive.unregisterVOI(VOIs.VOIAt(i));

        for (int j = (i + 1); j < (refMark + adjMark); j++) {

            if (j > 0) {
                markerType[j - 1] = markerType[j];
                xOrg[j - 1] = xOrg[j];
                yOrg[j - 1] = yOrg[j];
            }
        }

        if (isReference) {
            refMark--;
        } else {
            adjMark--;
        }

        if (oldFrame) {
            frame.updateImages(true);
        } else {
            toolFrame.updateImages(true);
        }
    }

    /**
     * Deletes all REFMARK VOIs.
     */
    public void deleteReferenceVOIs() {
        int i;
        int j = -1;
        int nVOI;

        ViewVOIVector VOIs = imageActive.getVOIs();

        nVOI = VOIs.size();

        if (nVOI == 0) {
            return;
        }

        for (i = (nVOI - 1); i >= 0; i--) {

            if (markerType[i] == ROTATIONCENTER) {
                j = i;
            } else if (markerType[i] == REFMARK) {
                VOIs.removeElementAt(i);
                j--;
            }
        }

        nVOI = VOIs.size(); // number of ROTATIONCENTER AND ADJMARK VOIs

        for (i = 0; i < nVOI; i++) {
            VOIs.VOIAt(i).getPointCoordinates(ptCoord);
            xOrg[i] = Math.round(ptCoord[0]);
            yOrg[i] = Math.round(ptCoord[1]);

            if (i == j) {
                markerType[i] = ROTATIONCENTER;
            } else {
                markerType[i] = ADJMARK;
            }
        }

        newVOI = null;
        id = (short) nVOI;
        refMark = 0;

        if (rotCenterMark == 1) {
            adjMark = nVOI - 1;
        } else {
            adjMark = nVOI;
        }
        // imageActive.notifyImageDisplayListeners(null,true);
    }

    /**
     * Deletes all REFMARK and ROTATIONCENTER VOIs.
     */
    public void deleteRefRotVOIs() {
        int i;
        int nVOI;

        ViewVOIVector VOIs = imageActive.getVOIs();

        nVOI = VOIs.size();

        if (nVOI == 0) {
            return;
        }

        for (i = (nVOI - 1); i >= 0; i--) {

            if ((markerType[i] == REFMARK) || (markerType[i] == ROTATIONCENTER)) {
                VOIs.removeElementAt(i);
            }
        }

        nVOI = VOIs.size(); // number of ADJMARK VOIs

        for (i = 0; i < nVOI; i++) {
            VOIs.VOIAt(i).getPointCoordinates(ptCoord);
            xOrg[i] = Math.round(ptCoord[0]);
            yOrg[i] = Math.round(ptCoord[1]);
            markerType[i] = ADJMARK;
        }

        newVOI = null;
        id = (short) nVOI;
        rotCenterMark = 0;
        adjMark = nVOI;
        refMark = 0;
        // imageActive.notifyImageDisplayListeners(null,true);
    }

    /**
     * Deletes all VOIs.
     */
    public void deleteVOIs() {
        int i;
        int nVOI;

        ViewVOIVector VOIs = imageActive.getVOIs();

        nVOI = VOIs.size();

        if (nVOI == 0) {
            return;
        }

        for (i = (nVOI - 1); i >= 0; i--) {
            VOIs.removeElementAt(i);
        }

        newVOI = null;
        id = 0;
        rotCenterMark = 0;
        refMark = 0;
        adjMark = 0;
        // imageActive.notifyImageDisplayListeners(null,true);
    }

    /**
     * Sets all variables to null, disposes, and garbage collects.
     *
     * @param  gcFlag  if true garbage collector should be called.
     */
    public void dispose(boolean gcFlag) {

        lutBufferRemapped = null;
        imageBufferA = null;
        imageBufferB = null;
        pixBuffer = null;
        pixBufferB = null;
        paintBuffer = null;
        paintBitmap = null;
        imageActive = null;
        imageBufferActive = null;
        frame = null;
        imageA = null;
        imageB = null;

        LUTa = null;
        LUTb = null;

        super.disposeLocal();

        if (gcFlag == true) {
            System.gc();
        }
    }

    /**
     * Returns the active image.
     *
     * @return  active image
     */
    public ModelImage getActiveImage() {
        return imageActive;
    }

    /**
     * Returns the active image buffer.
     *
     * @return  float[] active image buffer
     */
    public float[] getActiveImageBuffer() {
        return imageBufferActive;
    }

    /**
     * Returns the adjust mark flag.
     *
     * @return  int adjust mark flag
     */
    public int getAdjMark() {
        return adjMark;
    }

    /**
     * accessor that returns int blue.
     *
     * @return  int blue component
     */
    public int getBlue() {
        return blue;
    }

    /**
     * Returns the frame.
     *
     * @return  frame
     */
    public ViewJFrameBase getFrame() {
        return frame;
    }

    /**
     * Accessor that returns int green.
     *
     * @return  int green component
     */
    public int getGreen() {
        return green;
    }

    /**
     * Returns the imageA.
     *
     * @return  ModelImage imageA
     */
    public ModelImage getImageA() {
        return imageA;
    }

    /**
     * Returns the imageB.
     *
     * @return  ModelImage imageB
     */
    public ModelImage getImageB() {
        return imageB;
    }

    /**
     * Returns the model lut for the imageA.
     *
     * @return  ModelLUT the model LUT for imageA
     */
    public ModelLUT getLUTa() {
        return LUTa;
    }

    /**
     * Returns the model lut for the imageB.
     *
     * @return  ModelLUT the model LUT for imageB
     */
    public ModelLUT getLUTb() {
        return LUTb;
    }

    /**
     * Returns the marker type.
     *
     * @return  int[] marker
     */
    public int[] getMarkerType() {
        return (markerType);
    }

    /**
     * Returns the VOI mode.
     *
     * @return  int drawing mode for the VOI tools (i.e. ELLIPSE, LINE ...)
     */
    public int getMode() {
        return mode;
    }

    /**
     * Returns the number of VOIs.
     *
     * @return  int number of VOIs
     */
    public int getnVOI() {
        ViewVOIVector VOIs = imageActive.getVOIs();

        return (VOIs.size());
    }

    /**
     * Returns the opacity used in alpha blending between the input image and the reference image.
     *
     * @return  float opacity
     */
    public float getOpacity() {
        return OPACITY;
    }

    /**
     * Returns the image's orientation.
     *
     * @return  int image orientation
     */
    public int getOrientation() {
        return orientation;
    }

    /**
     * Returns BitSet object paintBitmap.
     *
     * @return  BitSet paintBitmap
     */
    public BitSet getpaintBitmap() {
        return paintBitmap;
    }

    /**
     * Returns int red.
     *
     * @return  int red component
     */
    public int getRed() {
        return red;
    }

    /**
     * Returns the reference mark flag.
     *
     * @return  int reference mark flag
     */
    public int getRefMark() {
        return refMark;
    }

    /**
     * Returns the ModelRGB RGBTA for imageA.
     *
     * @return  RGBTA for imageA
     */
    public ModelRGB getRGBTA() {
        return RGBTA;
    }

    /**
     * Returns the ModelRGB for imageB.
     *
     * @return  RGBTB for imageB
     */
    public ModelRGB getRGBTB() {
        return RGBTB;
    }

    /**
     * Returns the X origin.
     *
     * @return  int[] the X origin
     */
    public int[] getxOrg() {
        return (xOrg);
    }

    /**
     * DOCUMENT ME!
     *
     * @return  int[] list of x coordinates
     */
    public int[] getxPres() {
        int i;
        int nVOI;
        ViewVOIVector VOIs = imageActive.getVOIs();
        nVOI = VOIs.size();

        for (i = 0; i < nVOI; i++) {
            VOIs.VOIAt(i).getPointCoordinates(ptCoord);
            xPres[i] = Math.round(ptCoord[0]);
        }

        return (xPres);
    }

    /**
     * Returns the Y origin.
     *
     * @return  int[] the Y origin
     */
    public int[] getyOrg() {
        return (yOrg);
    }

    /**
     * DOCUMENT ME!
     *
     * @return  int[] list of y coordinates
     */
    public int[] getyPres() {
        int i;
        int nVOI;
        ViewVOIVector VOIs = imageActive.getVOIs();
        nVOI = VOIs.size();

        for (i = 0; i < nVOI; i++) {
            VOIs.VOIAt(i).getPointCoordinates(ptCoord);
            yPres[i] = Math.round(ptCoord[1]);
        }

        return (yPres);
    }

    /**
     * **************************** Key Events *****************************.*******************************************
     * *****************************
     *
     * @param  e  DOCUMENT ME!
     */
    public void keyPressed(KeyEvent e) { }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void keyReleased(KeyEvent e) {
        int keyCode = e.getKeyCode();

        // int modifiers = e.getModifiers();
        if (mode == WIN_REGION) {

            switch (keyCode) {

                case KeyEvent.VK_SHIFT:
                    update(getGraphics());
            }
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void keyTyped(KeyEvent e) { }

    /**
     * Constructs reference point VOI from the refPoint passed in.
     *
     * @param  adjPoint  a point VOI is generated from the 3D point
     */
    public void makeAdjustableVOI(Point3Df adjPoint) {
        hue = 1.0f / 3.0f; // hue for green

        try {
            float[] x = new float[1];
            float[] y = new float[1];
            float[] z = new float[1];

            id = (short) imageActive.getVOIs().size();
            newVOI = new VOI(id, "point2d.voi", 1, VOI.POINT, hue);
            x[0] = adjPoint.x;
            y[0] = adjPoint.y;
            z[0] = 0.0f;
            newVOI.importCurve(x, y, z, (int) z[0]);
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentRegistration.mouseReleased");
            setMode(DEFAULT);

            return;
        }

        adjMark++;
        xOrg[id] = (int) adjPoint.x;
        yOrg[id] = (int) adjPoint.y;
        markerType[id] = ADJMARK;
        imageActive.registerVOI(newVOI);

        if (oldFrame) {
            frame.updateImages(true);
        } else {
            toolFrame.updateImages(true);
            // imageActive.notifyImageDisplayListeners();
        }
    }

    /**
     * Constructs reference point VOI from the refPoint passed in.
     *
     * @param  refPoint  a point VOI is generated from the 3D point
     */
    public void makeReferenceVOI(Point3Df refPoint) {
        hue = 0.0f; // hue for red

        try {
            float[] x = new float[1];
            float[] y = new float[1];
            float[] z = new float[1];

            id = (short) imageActive.getVOIs().size();
            newVOI = new VOI(id, "point2d.voi", 1, VOI.POINT, hue);
            x[0] = refPoint.x;
            y[0] = refPoint.y;
            z[0] = 0.0f;
            newVOI.importCurve(x, y, z, (int) z[0]);
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentRegistration.mouseReleased");
            setMode(DEFAULT);

            return;
        }

        refMark++;
        xOrg[id] = (int) refPoint.x;
        yOrg[id] = (int) refPoint.y;
        markerType[id] = REFMARK;
        imageActive.registerVOI(newVOI);

        // imageActive.notifyImageDisplayListeners();
        if (oldFrame) {
            frame.updateImages(true);
        } else {
            toolFrame.updateImages(true);
        }
    }

    // ************************************************************************
    // ***************************** Mouse Events *****************************
    // ************************************************************************

    /**
     * When the mouse is clicked in the image, several different things may happen. If a Region of Interest (VOI) is
     * selected and the click count is 2, a VOI dialog should pop up. If the click count is 1 and the mouse is in an
     * VOI, it should select the VOI. In all other cases, a click within the image but not in an VOI should deselect all
     * VOIs.
     *
     * @param  mouseEvent  event that triggers function; contains click count
     */

    public void mouseClicked(MouseEvent mouseEvent) {
        int xS, yS;
        int nVOI;
        ViewVOIVector VOIs;

        xS = Math.round((mouseEvent.getX() / (getZoomX() * resolutionX)) - 0.5f);
        yS = Math.round((mouseEvent.getY() / (getZoomY() * resolutionY)) - 0.5f);

        if ((xS < 0) || (xS >= imageActive.getExtents()[0]) || // Check for validity
                (yS < 0) || (yS >= imageActive.getExtents()[1])) {
            return;
        }

        if ((mouseEvent.getClickCount() == 1) && ((mode == DEFAULT) || (mode == TRANSLATE) || (mode == ROTATE))) {
            VOIs = imageActive.getVOIs();
            nVOI = VOIs.size();

            for (int k = 0; k < nVOI; k++) { // deactivate all VOIs
                VOIs.VOIAt(k).setAllActive(false);
            }

            // imageActive.notifyImageDisplayListeners();
            if (oldFrame) {
                frame.updateImages(true);
            } else {
                toolFrame.updateImages(true);
            }
        }

    }

    // ************************************************************************
    // ************************** Mouse Motion Events *************************
    // ************************************************************************

    /**
     * Drags an VOI real time by calling a series of translations and draws. Once the mouse is released, the positions
     * are reset permenantly. Also rubberbands points if the cursor indicates it by calling rubberbandVOI.
     *
     * @param  mouseEvent  event that triggered function
     */
    public void mouseDragged(MouseEvent mouseEvent) {

        // int             mouseMods = mouseEvent.getModifiers();
        Graphics g;
        int i;
        int nVOI;
        ViewVOIVector VOIs = imageActive.getVOIs();
        int xS, yS;
        int distX, distY;
        int xDim, yDim;
        int zDim = 1;

        if ((pixBuffer == null) || (imageBufferActive == null) || (modifyFlag == false)) {
            return;
        }

        xS = Math.round((mouseEvent.getX() / (getZoomX() * resolutionX)) - 0.5f); // zoomed x.  Used as cursor
        yS = Math.round((mouseEvent.getY() / (getZoomY() * resolutionY)) - 0.5f); // zoomed y.  Used as cursor

        xDim = imageActive.getExtents()[0];
        yDim = imageActive.getExtents()[1];

        if (imageActive.getNDims() >= 3) {
            zDim = imageActive.getExtents()[2];
        }

        if ((xS < 0) || (xS >= xDim) || (yS < 0) || (yS >= yDim)) {
            return;
        }

        if ((doDrag == true) && ((mode == TRANSLATE) || (mode == ROTATE))) {
            xFinish = xS * resolutionX;
            yFinish = yS * resolutionY;

            if (oldFrame) {
                frame.setMove(xStart, yStart, xFinish, yFinish);
            } else {
                toolFrame.setMove(xStart, yStart, xFinish, yFinish);
            }

            xStart = xFinish;
            yStart = yFinish;

            if (mode == ROTATE) {
                g = getGraphics();

                if (g == null) {
                    return;
                }

                g.setColor(Color.yellow);
                g.drawLine((int) ((xRotation * getZoomX()) + 0.5), (int) ((yRotation * getZoomX()) + 0.5),
                           mouseEvent.getX(), mouseEvent.getY());
            }
        }

        distX = xS - anchorPt.x; // distance from original to cursor
        distY = yS - anchorPt.y;

        g = getGraphics();

        if (g == null) {
            return;
        }

        nVOI = VOIs.size();

        if (mode == MOVE) {

            for (i = 0; i < nVOI; i++) {

                if (VOIs.VOIAt(i).isActive() && VOIs.VOIAt(i).isVisible()) {

                    if (VOIs.VOIAt(i).getCurveType() == VOI.POINT) {
                        setCursor(crosshairCursor);
                        VOIs.VOIAt(i).moveVOI(slice, xDim, yDim, zDim, distX, distY, 0);
                        newX = xOrg[i] + distX;
                        newY = yOrg[i] + distY;

                        if ((newX >= 0) && (newX < imageActive.getExtents()[0]) && (newY >= 0) &&
                                (newY < imageActive.getExtents()[1])) {
                            xOrg[i] = newX;
                            yOrg[i] = newY;
                        }

                        if (oldFrame) {
                            frame.updateImages(true);
                        } else {
                            toolFrame.updateImages(true);
                            // imageActive.notifyImageDisplayListeners();
                        }

                        break;
                    }
                }
            }

            anchorPt.x = xS;
            anchorPt.y = yS;

            if (i == nVOI) {
                g.dispose();

                return;
            }

            g.dispose();

            return;
        }

        g.dispose();
    }

    /**
     * Unchanged.
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseEntered(MouseEvent mouseEvent) { }

    /**
     * Resets the level set stack.
     *
     * @param  mouseEvent  event that triggered function
     */
    public void mouseExited(MouseEvent mouseEvent) { }

    /**
     * If the mode is level set, draws level sets as user moves mouse. Otherwise, changes the cursor depending on where
     * the mouse is in relation to the VOI.
     *
     * @param  mouseEvent  event that triggered the function
     */
    public void mouseMoved(MouseEvent mouseEvent) {
        int i;
        int x, y;
        int xS, yS;
        int nVOI;
        ViewVOIVector VOIs;
        Graphics g = getGraphics();

        if ((g == null) || (modifyFlag == false) || (slice == -99)) {
            return;
        }

        if ((pixBuffer == null) || (imageBufferActive == null)) {
            g.dispose();

            return;
        }

        xS = Math.round((mouseEvent.getX() / (getZoomX() * resolutionX)) - 0.5f);
        yS = Math.round((mouseEvent.getY() / (getZoomY() * resolutionY)) - 0.5f);
        x = mouseEvent.getX();
        y = mouseEvent.getY();

        if ((xS < 0) || (xS >= imageActive.getExtents()[0]) || // Check to ensure point is within
                (yS < 0) || (yS >= imageActive.getExtents()[1])) { // the image bounds
            g.dispose();

            return;
        }

        if (mode == WIN_REGION) {
            paintWindowComponent(g, mouseEvent);
            g.dispose();

            return;
        }

        if ((mode == POINT_VOI) || (mode == TRANSLATE) || (mode == ROTATE)) {
            g.dispose();

            return;
        }

        VOIs = imageActive.getVOIs(); // Get the VOIs from the active image.
        nVOI = VOIs.size();

        for (i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.POINT) {

                if (VOIs.VOIAt(i).isVisible() &&
                        VOIs.VOIAt(i).nearPoint(x, y, slice, getZoomX(), resolutionX, resolutionY)) {
                    setMode(MOVE);
                    g.dispose();

                    return;
                }
            }
        }

        setMode(DEFAULT);
        // setCursor(crosshairCursor);
    }

    /**
     * Sets the mode of the program depending on the cursor mode. If the mode is move, activates the contour or line and
     * enables the delete button.
     *
     * @param  mouseEvent  event that triggered function
     */
    public void mousePressed(MouseEvent mouseEvent) {
        int xS, yS;
        int x, y;
        int deltaX, deltaY;
        int i;
        int nVOI;
        ViewVOIVector VOIs;

        if (modifyFlag == false) {
            return;
        }

        try {

            xS = Math.round((mouseEvent.getX() / (getZoomX() * resolutionX)) - 0.5f);
            yS = Math.round((mouseEvent.getY() / (getZoomY() * resolutionY)) - 0.5f);
            x = mouseEvent.getX();
            y = mouseEvent.getY();

            if ((xS < 0) || (xS >= imageActive.getExtents()[0]) || (yS < 0) || (yS >= imageActive.getExtents()[1])) {
                return;
            }

            xStart = xS * resolutionX;
            yStart = yS * resolutionY;
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentRegistration.mousePressed");
            setMode(DEFAULT);

            return;
        }

        if (mode == MOVE) {
            anchorPt.setLocation(xS, yS); // For use in dragging VOIs

            VOIs = imageActive.getVOIs();
            nVOI = VOIs.size();

            for (i = 0; i < nVOI; i++) {
                VOIs.VOIAt(i).setAllActive(false); // deactivate all other VOIs
            }

            for (i = 0; i < nVOI; i++) {

                if (VOIs.VOIAt(i).getCurveType() == VOI.POINT) { // curve type is Point

                    if (VOIs.VOIAt(i).nearPoint(x, y, slice, getZoomX(), resolutionX, resolutionY)) {

                        VOIs.VOIAt(i).setActive(true);
                        ((VOIPoint) (VOIs.VOIAt(i).getCurves()[slice].elementAt(0))).setActive(true);
                        VOIs.VOIAt(i).drawVOISpecial(slice, getGraphics(), imageActive.getFileInfo(0).getResolutions(),
                                                     imageActive.getFileInfo(0).getUnitsOfMeasure(),
                                                     imageActive.getExtents()[0], imageActive.getExtents()[1],
                                                     getZoomX(), getZoomY(), resolutionX, resolutionY);

                        if (oldFrame) {
                            frame.updateImages(true);
                        } else {
                            toolFrame.updateImages(true);
                            // imageActive.notifyImageDisplayListeners();
                        }

                        return;
                    }
                }
            }
        } else if (mode == ROTATE) {

            if ((doCenter) && (rotCenterMark == 0)) {
                hue = 1.0f / 6.0f; // hue for yellow

                try {
                    float[] xR = new float[1];
                    float[] yR = new float[1];
                    float[] zR = new float[1];

                    id = (short) imageActive.getVOIs().size();
                    centerid = id;
                    newVOI = new VOI(id, "point2d.voi", 1, VOI.POINT, hue);
                    centerVOI = newVOI;
                    xR[0] = xS;
                    yR[0] = yS;
                    zR[0] = 0.0f;
                    newVOI.importCurve(xR, yR, zR, 0);
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: ComponentRegistration.mouseReleased");
                    setMode(DEFAULT);

                    return;
                }

                xOrg[id] = xS;
                yOrg[id] = yS;
                markerType[id] = ROTATIONCENTER;
                rotCenterMark = 1;
                imageActive.registerVOI(newVOI);

                // imageActive.notifyImageDisplayListeners();
                xRotation = xS * resolutionX;
                yRotation = yS * resolutionY;

                if (oldFrame) {
                    frame.setRotationCenter(xRotation, yRotation);
                    frame.updateImages(true);
                } else {
                    toolFrame.setRotationCenter(xRotation, yRotation);
                    toolFrame.updateImages(true);
                }
            } // end of if ((doCenter) && (rotCenterMark == 0))
            else if ((doCenter) && (rotCenterMark == 1)) {
                deltaX = xS - xOrg[centerid];
                deltaY = yS - yOrg[centerid];

                ((VOIPoint) (centerVOI.getCurves()[0].elementAt(0))).moveVOIPoint(deltaX, deltaY, 0,
                                                                                  imageActive.getExtents()[0],
                                                                                  imageActive.getExtents()[1], 1);
                xOrg[centerid] = xS;
                yOrg[centerid] = yS;
                xRotation = xS * resolutionX;
                yRotation = yS * resolutionY;

                if (oldFrame) {
                    frame.setRotationCenter(xRotation, yRotation);
                    frame.updateImages(true);
                } else {
                    toolFrame.setRotationCenter(xRotation, yRotation);
                    toolFrame.updateImages(true);
                }
            }
        } // else if (mode == ROTATE)
    }

    /**
     * This function sets up and draws the VOI according to the mode.
     *
     * @param  mouseEvent  event that triggered function
     */
    public void mouseReleased(MouseEvent mouseEvent) {
        int xS, yS, xDim, yDim;

        if (modifyFlag == false) {
            return;
        }

        xS = Math.round((mouseEvent.getX() / (getZoomX() * resolutionX)) - 0.5f);
        yS = Math.round((mouseEvent.getY() / (getZoomY() * resolutionY)) - 0.5f);
        xDim = imageActive.getExtents()[0];
        yDim = imageActive.getExtents()[1];

        if ((xS < 0) || (xS >= imageActive.getExtents()[0]) || (yS < 0) || (yS >= imageActive.getExtents()[1])) {
            return;
        }

        if ((doDrag == false) && ((mode == TRANSLATE) || (mode == ROTATE))) {
            xFinish = xS * resolutionX;
            yFinish = yS * resolutionY;

            if (oldFrame) {
                frame.setMove(xStart, yStart, xFinish, yFinish);
            } else {
                toolFrame.setMove(xStart, yStart, xFinish, yFinish);
            }
        }

        if (mode == POINT_VOI) {

            if (doRefMark) {
                hue = 0.0f; // hue for red

                try {
                    float[] x = new float[1];
                    float[] y = new float[1];
                    float[] z = new float[1];

                    id = (short) imageActive.getVOIs().size();
                    newVOI = new VOI(id, "point2d.voi", 1, VOI.POINT, hue);
                    x[0] = xS;
                    y[0] = yS;
                    z[0] = slice;
                    newVOI.importCurve(x, y, z, slice);
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: ComponentRegistration.mouseReleased");
                    setMode(DEFAULT);

                    return;
                }

                refMark++;
                xOrg[id] = xS;
                yOrg[id] = yS;
                markerType[id] = REFMARK;
                imageActive.registerVOI(newVOI);

                if (oldFrame) {
                    frame.updateImages(true);
                } else {
                    toolFrame.updateImages(true);
                }

                // if (mouseEvent.isShiftDown() != true) {
                // setMode(DEFAULT);
                // }
                // imageActive.notifyImageDisplayListeners();
            } // end of if (doRefMark)
            else if (doAdjMark) {
                hue = 1.0f / 3.0f; // hue for green

                try {
                    float[] x = new float[1];
                    float[] y = new float[1];
                    float[] z = new float[1];

                    id = (short) imageActive.getVOIs().size();

                    // System.err.println("id is: " + id);
                    newVOI = new VOI(id, "point2d.voi", 1, VOI.POINT, hue);
                    x[0] = xS;
                    y[0] = yS;
                    z[0] = slice;
                    newVOI.importCurve(x, y, z, slice);
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: ComponentRegistration.mouseReleased");
                    setMode(DEFAULT);

                    return;
                }

                adjMark++;
                xOrg[id] = xS;
                yOrg[id] = yS;
                markerType[id] = ADJMARK;
                imageActive.registerVOI(newVOI);

                // imageActive.notifyImageDisplayListeners();
                if (oldFrame) {
                    frame.updateImages(true);
                } else {
                    toolFrame.updateImages(true);
                    // if (mouseEvent.isShiftDown() != true) {
                    // setMode(DEFAULT);
                    // }
                }
            } // end of if (doAdjMark)
        }

        memCount++;

        if ((Runtime.getRuntime().freeMemory() < 20000000) || (memCount >= 15)) {
            System.gc();
        }
    }

    /**
     * Moves VOIs to new position by deltaX and deltaY.
     *
     * @param  deltaX  distance VOI moves in the X direction
     * @param  deltaY  distance VOI moves in the Y direction
     */
    public void moveVOIPosition(int deltaX, int deltaY) {

        if (newVOI != null) {

            // Just moves the most recently created POINT_VOI which is newVOI
            if (imageActive.getNDims() >= 3) {
                newVOI.moveVOI(slice, imageActive.getExtents()[0], imageActive.getExtents()[1],
                               imageActive.getExtents()[2], deltaX, deltaY, 0);
            } else {
                newVOI.moveVOI(slice, imageActive.getExtents()[0], imageActive.getExtents()[1], 0, deltaX, deltaY, 0);
            }

            if (oldFrame) {
                frame.updateImages(true);
            } else {
                toolFrame.updateImages(true);
                // imageActive.notifyImageDisplayListeners();
            }

            newX = xOrg[id] + deltaX;
            newY = yOrg[id] + deltaY;

            if ((newX >= 0) && (newX < imageActive.getExtents()[0]) && (newY >= 0) &&
                    (newY < imageActive.getExtents()[1])) {
                xOrg[id] = newX;
                yOrg[id] = newY;
            }
        } // end of if (newVOI != null)
    }

    /**
     * Moves VOI with ID == id to a new position (x,y).
     *
     * @param  id  DOCUMENT ME!
     * @param  x   x coordinate of new position
     * @param  y   y coordinate of new position
     */
    public void moveVOITo(int id, int x, int y) {

        // id is the point VOI identifier
        // x,y is the coordinate to move the point VOI to
        int presentX;
        int presentY;
        int deltaX;
        int deltaY;
        ViewVOIVector VOIs = imageActive.getVOIs();
        VOIs.VOIAt(id).getPointCoordinates(ptCoord);
        presentX = Math.round(ptCoord[0]);
        presentY = Math.round(ptCoord[1]);
        deltaX = x - presentX;
        deltaY = y - presentY;
        VOIs.VOIAt(id).setAllActive(true);

        if (imageActive.getNDims() >= 3) {
            VOIs.VOIAt(id).moveVOI(slice, imageActive.getExtents()[0], imageActive.getExtents()[1],
                                   imageActive.getExtents()[2], deltaX, deltaY, 0);
        } else {
            VOIs.VOIAt(id).moveVOI(0, imageActive.getExtents()[0], imageActive.getExtents()[1], 1, deltaX, deltaY, 0);
        }
    }

    /**
     * Paints the image and calls drawSelf for all VOIs.
     *
     * @param  g  graphics
     */
    public void paintComponent(Graphics g) {
        int i;
        int nVOI;
        int curRefMark;
        int curAdjMark;
        ViewVOIVector VOIs = null;

        try {

            if (g == null) {
                MipavUtil.displayError("ComponentRegistration.paintComponent: graphics = null");

                return;
            }

            super.paintComponent(g);

            // Draw VOIs (unless told not to by showVOIs = false)
            if (showVOIs) {

                if (useDualVOIs) {

                    if (orientation == NA) {
                        setActiveImage(IMAGE_B);
                        VOIs = imageB.getVOIs();
                        nVOI = VOIs.size();

                        if (slice != -99) {

                            for (i = nVOI - 1; i >= 0; i--) {
                                VOIs.VOIAt(i).drawSelf(getZoomX(), getZoomY(), resolutionX, resolutionY, 0f, 0f,
                                                       imageActive.getFileInfo(0).getResolutions(),
                                                       imageActive.getFileInfo(0).getUnitsOfMeasure(), slice,
                                                       orientation, g);
                            }
                        }

                        VOIs = imageA.getVOIs();
                        nVOI = VOIs.size();

                        if (slice != -99) {

                            for (i = nVOI - 1; i >= 0; i--) {
                                VOIs.VOIAt(i).drawSelf(getZoomX(), getZoomY(), resolutionX, resolutionY, 0f, 0f,
                                                       imageActive.getFileInfo(0).getResolutions(),
                                                       imageActive.getFileInfo(0).getUnitsOfMeasure(), slice,
                                                       orientation, g);
                            }
                        }
                    }

                } else {
                    VOIs = imageActive.getVOIs();

                    if (orientation == NA) {
                        nVOI = VOIs.size();

                        if (slice != -99) {
                            curRefMark = refMark;
                            curAdjMark = adjMark;

                            for (i = nVOI - 1; i >= 0; i--) {

                                if (markerType[i] == ROTATIONCENTER) {
                                    VOIs.VOIAt(i).drawSelf(getZoomX(), getZoomY(), resolutionX, resolutionY, 0f, 0f,
                                                           imageActive.getFileInfo(0).getResolutions(),
                                                           imageActive.getFileInfo(0).getUnitsOfMeasure(), slice,
                                                           orientation, g);
                                } // end of if (markerType[i] == ROTATIONCENTER)
                                else if (markerType[i] == REFMARK) {
                                    VOIs.VOIAt(i).setColor(Color.red);
                                    ((VOIPoint) (VOIs.VOIAt(i).getCurves()[slice].elementAt(0))).setLabel(String.valueOf(curRefMark));
                                    VOIs.VOIAt(i).drawSelf(getZoomX(), getZoomY(), resolutionX, resolutionY, 0f, 0f,
                                                           imageActive.getFileInfo(0).getResolutions(),
                                                           imageActive.getFileInfo(0).getUnitsOfMeasure(), slice,
                                                           orientation, g);
                                    curRefMark--;
                                } // end of else if (markerType[i] == REFMARK)
                                else if (markerType[i] == ADJMARK) {
                                    VOIs.VOIAt(i).setColor(Color.green);
                                    ((VOIPoint) (VOIs.VOIAt(i).getCurves()[slice].elementAt(0))).setLabel(String.valueOf(curAdjMark));
                                    VOIs.VOIAt(i).drawSelf(getZoomX(), getZoomY(), resolutionX, resolutionY, 0f, 0f,
                                                           imageActive.getFileInfo(0).getResolutions(),
                                                           imageActive.getFileInfo(0).getUnitsOfMeasure(), slice,
                                                           orientation, g);
                                    curAdjMark--;
                                } // end of else if (markerType[i] == ADJMARK)
                            }
                        } // end if slice != -99
                    }
                }
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentRegistration.paintComponent.");
        }
    }

    /**
     * Paints the image and calls drawSelf for all VOIs.
     *
     * @param  g           graphics
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void paintWindowComponent(Graphics g, MouseEvent mouseEvent) {

        int x = mouseEvent.getX();
        int y = mouseEvent.getY();

        if (g == null) {
            MipavUtil.displayError("ComponentRegistration.paintMagComponent: graphics = null");

            return;
        }

        if (mouseEvent.isShiftDown() == false) {
            super.paintWindowComponent(g, x, y, 100, 100, getZoomX());
        } else if (mouseEvent.isShiftDown() == true) {

            // super.paintMagComponent(g, x, y, MAGR_WIDTH, MAGR_HEIGHT, getZoomX(),
            // imageBufferActive, imageActive.getType(),                        imageActive.getExtents()[0],
            // getActiveImage().getMin(), getActiveImage().getMax());
            super.update(getGraphics());
        }
    }

    /**
     * Sets the active image for drawing VOIs.
     *
     * @param  active  IMAGE_A or IMAGE_B
     */
    public void setActiveImage(int active) {

        if (active == IMAGE_A) {
            imageActive = imageA;
            imageBufferActive = imageBufferA;
            paintBitmap = imageA.getMask();
        } else {
            imageActive = imageB;
            imageBufferActive = imageBufferB;
            paintBitmap = imageB.getMask();
        }
    }

    /**
     * Sets the adjusted mark flag.
     *
     * @param  doAdj  DOCUMENT ME!
     */
    public void setAdjMark(boolean doAdj) {
        doAdjMark = doAdj;
    }

    /**
     * Set the buffers of the frame in which the image(s) is displayed, allocates the memory and uses this method to
     * pass the references to the buffers.
     *
     * @param  imgBufferA  storage buffer used to display image A
     * @param  imgBufferB  storage buffer used to display image B
     * @param  pixBuff     storage buffer used to build a displayable image
     * @param  pixBuffB    storage buffer used to build a displayable imageB for the window
     * @param  paintBuff   storage buffer used to display the combined paintBitmap and pixBuffer buffers
     */
    public void setBuffers(float[] imgBufferA, float[] imgBufferB, int[] pixBuff, int[] pixBuffB, int[] paintBuff) {

        imageBufferA = imgBufferA;
        imageBufferB = imgBufferB;
        pixBuffer = pixBuff;
        pixBufferB = pixBuffB;
        paintBuffer = paintBuff;
        imageBufferActive = imageBufferA;
    }

    /**
     * Sets the center point used in transformations.
     *
     * @param  doCen  if true construct a center point VOI
     */
    public void setCenter(boolean doCen) {
        int xS, yS;
        doCenter = doCen;

        if ((doCenter) && (rotCenterMark == 0)) {
            hue = 1.0f / 6.0f; // hue for yellow

            try {
                float[] xR = new float[1];
                float[] yR = new float[1];
                float[] zR = new float[1];
                xS = imageB.getExtents()[0] / 2;
                yS = imageB.getExtents()[1] / 2;
                id = (short) imageActive.getVOIs().size();
                centerid = id;
                newVOI = new VOI(id, "point2d.voi", 1, VOI.POINT, hue);
                centerVOI = newVOI;
                xR[0] = xS;
                yR[0] = yS;
                zR[0] = 0.0f;
                newVOI.importCurve(xR, yR, zR, 0);
                ((VOIPoint) newVOI.getCurves()[0].elementAt(0)).setLabel("C");
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: ComponentRegistration.setCenter");
                setMode(DEFAULT);

                return;
            }

            xOrg[id] = xS;
            yOrg[id] = yS;
            markerType[id] = ROTATIONCENTER;
            rotCenterMark = 1;
            imageActive.registerVOI(newVOI);

            // imageActive.notifyImageDisplayListeners();
            xRotation = xS * resolutionX;
            yRotation = yS * resolutionY;

            if (oldFrame) {
                frame.setRotationCenter(xRotation, yRotation);
                frame.updateImages(true);
            } else {
                toolFrame.setRotationCenter(xRotation, yRotation);
                toolFrame.updateImages(true);
            }
        } // end of if ((doCenter) && (rotCenterMark == 0))
    }

    /**
     * Enables or disables the component for modification.
     *
     * @param  flag  true = modify, and false = locked
     */
    public void setEnabled(boolean flag) {
        modifyFlag = flag;
        // rubberband.setActive(flag);
    }

    /**
     * Sets component's ImageA.
     *
     * @param  image  assumes dimensionality same as image B's for now
     */
    public void setImageA(ModelImage image) {
        imageA = image;
        setZoom(1, 1); // sets zoom
    }

    /**
     * Sets component's ImageB !!!!!! assumes dimensionality same as image A's for now will fix soon.
     *
     * @param  image  imageB
     */
    public void setImageB(ModelImage image) {
        imageB = image;

        if (imageB == null) {

            // remove checker boarding
            rowCheckers = -1;
            columnCheckers = -1;
        }
    }

    /************************************************************************/
    /**
     * Accessors.
     *
     * @param  flag  DOCUMENT ME!
     */
    /************************************************************************/

    /**
     * Sets the log magnitude display flag.
     *
     * @param  flag  if true display log of the Magnitude of the complex image
     */
    public void setLogMagDisplay(boolean flag) {
        logMagDisplay = flag;
    }

    /**
     * Sets the model LUT for the imageA.
     *
     * @param  LUT  the model LUT
     */
    public void setLUTa(ModelLUT LUT) {
        LUTa = LUT;

        if (oldFrame) {
            frame.setLUTa(LUT);
        } else {
            toolFrame.setLUTa(LUT);
        }
    }

    /**
     * Sets the model LUTb for the imageB.
     *
     * @param  LUT  the model LUT
     */
    public void setLUTb(ModelLUT LUT) {
        LUTb = LUT;

        if (oldFrame) {
            frame.setLUTb(LUT);
        } else {
            toolFrame.setLUTb(LUT);
        }
    }

    /**
     * Switches modes based on the variable mode. Sets rubberband activity and the cursor.
     *
     * @param  mode  the integer mode
     */
    public void setMode(int mode) {

        this.mode = mode;

        switch (mode) {

            case DEFAULT:
                setCursor(crosshairCursor);
                if (oldFrame) {
                    frame.setDefaultMode();
                } else {
                    toolFrame.setDefaultMode();
                }

                break;

            case POINT_VOI:
                setCursor(crosshairCursor);
                break;

            case MOVE:
                setCursor(moveCursor);
                break;

            case TRANSLATE:
                setCursor(moveCursor);
                break;

            case ROTATE:
                setCursor(crosshairCursor);
                break;

            case WIN_REGION:
                setCursor(magRegionCursor);
                break;
        }
    }

    /**
     * doDrag true enables the adjusted slice to respond to mouse press and drag events doDrag false restricts the
     * adjusted slice to responding to mouse press and release events.
     *
     * @param  doDrag  DOCUMENT ME!
     */
    public void setMouseDrag(boolean doDrag) {
        this.doDrag = doDrag;
    }

    /**
     * Sets the alpha blending of parameter for two image displaying.
     *
     * @param  value  amount [0,100] that is the percentage of Image A to be displayed
     */
    public void setNewAlphaBlend(int value) {
        alphaBlend = value / 100.0f;
        alphaPrime = 1 - alphaBlend;
    }

    /**
     * Sets the orientation of the image.
     *
     * @param  _orientation  orientaiton of image slice to be displayed
     */
    public void setOrientation(int _orientation) {
        orientation = _orientation;
    }

    /**
     * Sets the paint mask.
     *
     * @param  mask  DOCUMENT ME!
     */
    public void setPaintMask(BitSet mask) {
        paintBitmap = mask;
    }

    /**
     * Sets the reference mark flag.
     *
     * @param  doRef  DOCUMENT ME!
     */
    public void setRefMark(boolean doRef) {
        doRefMark = doRef;
    }

    /**
     * Sets the RGB table for image A.
     *
     * @param  RGBT  the RGB table
     */
    public void setRGBTA(ModelRGB RGBT) {

        // System.out.println ("RGBTA = " + RGBT);
        RGBTA = RGBT;
    }

    /**
     * Sets the RGB table for image B.
     *
     * @param  RGBT  the RGB table
     */
    public void setRGBTB(ModelRGB RGBT) {

        // System.out.println ("RGBTB = " + RGBT);
        RGBTB = RGBT;
    }

    // When the apply or close button is pressed, JDialogCheckerBoard sets the following 2 parameters
    // are used.

    /**
     * Sets the number of checkers in a row and a column.
     *
     * @param  rowCheckers     DOCUMENT ME!
     * @param  columnCheckers  DOCUMENT ME!
     */
    public void setRowColumnCheckers(int rowCheckers, int columnCheckers) {
        int xSep, ySep, xMod, yMod;
        int xDim, yDim;
        int xIndex, yIndex;
        int x, y;
        int[] xStart;
        int[] yStart;
        boolean doA;
        this.rowCheckers = rowCheckers;
        this.columnCheckers = columnCheckers;

        if (columnCheckers != -1) {

            // imageExtents could describe xy, xz, or zy
            xDim = imageExtents[0];
            yDim = imageExtents[1];

            if (checkerRegBitmap == null) {
                checkerRegBitmap = new BitSet(xDim * yDim);
            }

            xSep = xDim / columnCheckers;
            xMod = xDim % columnCheckers;
            ySep = yDim / rowCheckers;
            yMod = yDim % rowCheckers;
            xStart = new int[columnCheckers + 1];
            yStart = new int[rowCheckers + 1];
            xStart[0] = 0;

            for (x = 1; x <= columnCheckers; x++) {
                xStart[x] = xStart[x - 1] + xSep;

                if (x <= xMod) {
                    xStart[x]++;
                }
            } // for (x = 1; x < columnCheckers; x++)

            yStart[0] = 0;

            for (y = 1; y <= rowCheckers; y++) {
                yStart[y] = yStart[y - 1] + ySep;

                if (y <= yMod) {
                    yStart[y]++;
                }
            } // for (y = 1; y < rowCheckers; y++)

            // Do checkered rows starting with doA = true;
            doA = true;
            xIndex = 1;
            yIndex = 1;

            for (y = 0; y < yDim;) {

                for (x = 0; x < xDim;) {

                    if (doA) {
                        checkerRegBitmap.set(x + (y * xDim));
                    } else {
                        checkerRegBitmap.clear(x + (y * xDim));
                    }

                    x++;

                    if (x == xDim) {
                        doA = true;
                        xIndex = 1;
                    } else if (x == xStart[xIndex]) {
                        doA = !doA;
                        xIndex++;
                    }
                } // for (x = 0; x < xDim;)

                y++;

                if (y == yDim) { }
                else if (y == yStart[yIndex]) {
                    yIndex++;
                    y = yStart[yIndex];
                    yIndex++;
                }
            } // for (y = 0; y < yDim;)

            // Do checkered rows starting with doA = false;
            doA = false;
            xIndex = 1;
            yIndex = 2;

            for (y = yStart[1]; y < yDim;) {

                for (x = 0; x < xDim;) {

                    if (doA) {
                        checkerRegBitmap.set(x + (y * xDim));
                    } else {
                        checkerRegBitmap.clear(x + (y * xDim));
                    }

                    x++;

                    if (x == xDim) {
                        doA = false;
                        xIndex = 1;
                    } else if (x == xStart[xIndex]) {
                        doA = !doA;
                        xIndex++;
                    }
                } // for (x = 0; x < xDim;)

                y++;

                if (y == yDim) { }
                else if (y == yStart[yIndex]) {
                    yIndex++;
                    y = yStart[yIndex];
                    yIndex++;
                }
            } // for (y = yStart[1]; y < yDim;)
        } // if (columnCheckers != -1)

        imageActive.notifyImageDisplayListeners(null, true);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  dual  DOCUMENT ME!
     */
    public void setUseDualVOIs(boolean dual) {
        this.useDualVOIs = dual;
    }

    /**
     * For generating the display of 1 or 2 RGB images.
     *
     * @param   tSlice     t (time) slice to show
     * @param   zSlice     z slice to show
     * @param   forceShow  forces this method to import image and recalculate java image
     *
     * @return  boolean to indicate if the show was successful
     */
    public boolean show(int tSlice, int zSlice, boolean forceShow) {
        // Note that alphaBlending is applied with 1 component taken as zero if both components are not present -for
        // example, if either imageA or imageB but not both has red, then the red component is alphaBlended with zero.

        int j;
        int bufferSize;
        int offset;
        int index;
        int Ra, Ga, Ba, Rb, Gb, Bb;
        int imageSize;
        int pixValue;
        float opacityPrime;
        float redMapped, greenMapped, blueMapped;
        int[] RGBIndexBufferA = new int[256];
        int[] RGBIndexBufferB = new int[256];

        bufferSize = imageExtents[0] * imageExtents[1] * 4;
        imageSize = imageExtents[0] * imageExtents[1];

        OPACITY = 0.5f;
        opacityPrime = 1 - OPACITY;

        red = 128;
        green = 0;
        blue = 0;

        if (RGBTA != null) {
            RGBIndexBufferA = RGBTA.exportIndexedRGB();
        }

        if ((imageB != null) && (RGBTB != null)) {
            RGBIndexBufferB = RGBTB.exportIndexedRGB();
        }

        if ((slice != zSlice) || (timeSlice != tSlice) || (forceShow == true)) {
            slice = zSlice;
            timeSlice = tSlice;

            int zDimSlices = 0;

            if (imageA.getNDims() >= 3) {
                zDimSlices = imageExtents[2];
            }

            try {

                // imageA.exportData((timeSlice*zDimSlices*bufferSize + zSlice*bufferSize), bufferSize, imageBufferA);
                // if (imageB != null) {    imageB.exportData((timeSlice*zDimSlices * bufferSize + zSlice*bufferSize),
                // bufferSize, imageBufferB); }
                if ((orientation == AXIAL) || (orientation == NA)) {
                    imageA.exportData((timeSlice * zDimSlices * bufferSize) + (slice * bufferSize), bufferSize,
                                      imageBufferA);
                } else if (orientation == SAGITTAL) {

                    // Fix exportSliceZY ... to handle color !!!!!!!!
                    imageA.exportRGBSliceZY(timeSlice, slice, imageBufferA);
                } else if (orientation == CORONAL) {
                    imageA.exportRGBSliceXZ(timeSlice, slice, imageBufferA);
                }

                if (imageB != null) {

                    if ((orientation == AXIAL) || (orientation == NA)) {
                        imageB.exportData((timeSlice * zDimSlices * bufferSize) + (slice * bufferSize), bufferSize,
                                          imageBufferB);
                    } else if (orientation == SAGITTAL) {
                        imageB.exportRGBSliceZY(timeSlice, slice, imageBufferB);
                    } else if (orientation == CORONAL) {
                        imageB.exportRGBSliceXZ(timeSlice, slice, imageBufferB);
                    }
                }
            } catch (IOException error) {
                MipavUtil.displayError("" + error);

                return false;
            }

            if (imageB == null) {
                offset = zSlice * imageSize;

                for (index = 0, j = 0; j < imageSize; index += 4, j++) {

                    if (RGBTA != null) {

                        if (RGBTA.getROn()) {
                            redMapped = (RGBIndexBufferA[(int) imageBufferA[index + 1]] & 0x00ff0000) >> 16;
                        } else {
                            redMapped = 0;
                        }

                        if (RGBTA.getGOn()) {
                            greenMapped = (RGBIndexBufferA[(int) imageBufferA[index + 2]] & 0x0000ff00) >> 8;
                        } else {
                            greenMapped = 0;
                        }

                        if (RGBTA.getBOn()) {
                            blueMapped = (RGBIndexBufferA[(int) imageBufferA[index + 3]] & 0x000000ff);
                        } else {
                            blueMapped = 0;
                        }
                    } // end of if (RGBTA != null)
                    else {
                        redMapped = imageBufferA[index + 1];
                        greenMapped = imageBufferA[index + 2];
                        blueMapped = imageBufferA[index + 3];
                    }

                    pixValue = (0xff000000) |
                                   (((int) (redMapped) << 16) | (((int) (greenMapped) << 8) | ((int) (blueMapped))));
                    paintBuffer[j] = pixValue;
                }
            } else {
                offset = zSlice * imageSize;

                for (index = 0, j = 0; j < imageSize; index += 4, j++) {

                    if ((RGBTA != null) && (RGBTB != null)) {

                        if (RGBTA.getROn()) {
                            Ra = (RGBIndexBufferA[(int) imageBufferA[index + 1]] & 0x00ff0000) >> 16;
                        } else {
                            Ra = 0;
                        }

                        if (RGBTB.getROn()) {
                            Rb = (RGBIndexBufferB[(int) imageBufferB[index + 1]] & 0x00ff0000) >> 16;
                        } else {
                            Rb = 0;
                        }

                        if (RGBTA.getGOn()) {
                            Ga = (RGBIndexBufferA[(int) imageBufferA[index + 2]] & 0x0000ff00) >> 8;
                        } else {
                            Ga = 0;
                        }

                        if (RGBTB.getGOn()) {
                            Gb = (RGBIndexBufferB[(int) imageBufferB[index + 2]] & 0x0000ff00) >> 8;
                        } else {
                            Gb = 0;
                        }

                        if (RGBTA.getBOn()) {
                            Ba = (RGBIndexBufferA[(int) imageBufferA[index + 3]] & 0x000000ff);
                        } else {
                            Ba = 0;
                        }

                        if (RGBTB.getBOn()) {
                            Bb = (RGBIndexBufferB[(int) imageBufferB[index + 3]] & 0x000000ff);
                        } else {
                            Bb = 0;
                        }
                    } else {
                        Ra = (int) imageBufferA[index + 1];
                        Rb = (int) imageBufferB[index + 1];
                        Ga = (int) imageBufferA[index + 2];
                        Gb = (int) imageBufferB[index + 2];
                        Ba = (int) imageBufferA[index + 3];
                        Bb = (int) imageBufferB[index + 3];
                    }

                    if (columnCheckers == -1) { // no checkerboarding
                        Ra = (int) ((Ra * alphaBlend) + (Rb * alphaPrime));
                        Ga = (int) ((Ga * alphaBlend) + (Gb * alphaPrime));
                        Ba = (int) ((Ba * alphaBlend) + (Bb * alphaPrime));
                    } else { // checkerboarding

                        if (!checkerRegBitmap.get(j)) {
                            Ra = Rb;
                            Ga = Gb;
                            Ba = Bb;
                        }
                    }

                    pixValue = (0xff000000) | (Ra << 16) | (Ga << 8) | Ba;
                    paintBuffer[j] = pixValue;
                    pixBufferB[j] = (0xff000000) | (Rb << 16) | (Gb << 8) | Bb;
                } // for
            }

            importImage(paintBuffer);

            if (imageB != null) {
                importImageB(pixBufferB);
            }

            time = System.currentTimeMillis() - time;
        } else {

            if (imageB == null) {
                offset = zSlice * imageSize;

                for (index = 0, j = 0; j < imageSize; index += 4, j++) {

                    if (RGBTA != null) {

                        if (RGBTA.getROn()) {
                            redMapped = (RGBIndexBufferA[(int) imageBufferA[index + 1]] & 0x00ff0000) >> 16;
                        } else {
                            redMapped = 0;
                        }

                        if (RGBTA.getGOn()) {
                            greenMapped = (RGBIndexBufferA[(int) imageBufferA[index + 2]] & 0x0000ff00) >> 8;
                        } else {
                            greenMapped = 0;
                        }

                        if (RGBTA.getBOn()) {
                            blueMapped = (RGBIndexBufferA[(int) imageBufferA[index + 3]] & 0x000000ff);
                        } else {
                            blueMapped = 0;
                        }
                    } else {
                        redMapped = imageBufferA[index + 1];
                        greenMapped = imageBufferA[index + 2];
                        blueMapped = imageBufferA[index + 3];
                    }

                    pixValue = (0xff000000) |
                                   (((int) (redMapped) << 16) | (((int) (greenMapped) << 8) | ((int) (blueMapped))));
                    paintBuffer[j] = pixValue;

                }
            } else {
                offset = zSlice * imageSize;

                for (index = 0, j = 0; j < imageSize; index += 4, j++) {

                    if ((RGBTA != null) && (RGBTB != null)) {

                        if (RGBTA.getROn()) {
                            Ra = (RGBIndexBufferA[(int) imageBufferA[index + 1]] & 0x00ff0000) >> 16;
                        } else {
                            Ra = 0;
                        }

                        if (RGBTB.getROn()) {
                            Rb = (RGBIndexBufferB[(int) imageBufferB[index + 1]] & 0x00ff0000) >> 16;
                        } else {
                            Rb = 0;
                        }

                        if (RGBTA.getGOn()) {
                            Ga = (RGBIndexBufferA[(int) imageBufferA[index + 2]] & 0x0000ff00) >> 8;
                        } else {
                            Ga = 0;
                        }

                        if (RGBTB.getGOn()) {
                            Gb = (RGBIndexBufferB[(int) imageBufferB[index + 2]] & 0x0000ff00) >> 8;
                        } else {
                            Gb = 0;
                        }

                        if (RGBTA.getBOn()) {
                            Ba = (RGBIndexBufferA[(int) imageBufferA[index + 3]] & 0x000000ff);
                        } else {
                            Ba = 0;
                        }

                        if (RGBTB.getBOn()) {
                            Bb = (RGBIndexBufferB[(int) imageBufferB[index + 3]] & 0x000000ff);
                        } else {
                            Bb = 0;
                        }
                    } else {
                        Ra = (int) imageBufferA[index + 1];
                        Rb = (int) imageBufferB[index + 1];
                        Ga = (int) imageBufferA[index + 2];
                        Gb = (int) imageBufferB[index + 2];
                        Ba = (int) imageBufferA[index + 3];
                        Bb = (int) imageBufferB[index + 3];
                    }

                    if (columnCheckers == -1) { // no checkerboarding
                        Ra = (int) ((Ra * alphaBlend) + (Rb * alphaPrime));
                        Ga = (int) ((Ga * alphaBlend) + (Gb * alphaPrime));
                        Ba = (int) ((Ba * alphaBlend) + (Bb * alphaPrime));
                    } else { // checkerboarding

                        if (!checkerRegBitmap.get(j)) {
                            Ra = Rb;
                            Ga = Gb;
                            Ba = Bb;
                        }
                    }

                    // now allows the use of two Masks if needed (for new registration window)
                    if (paintBitmap.get(offset + j) == true) {
                        pixValue = (0xff000000) |
                                       (((int) ((Ra * opacityPrime) + red) << 16) |
                                            (((int) ((Ga * opacityPrime) + green) << 8) |
                                                 ((int) ((Ba * opacityPrime) + blue))));
                        paintBuffer[j] = pixValue;
                        pixBuffer[j] = (0xff000000) | (Ra << 16) | (Ga << 8) | Ba;
                    } else {
                        pixValue = (0xff000000) | (Ra << 16) | (Ga << 8) | Ba;
                        paintBuffer[j] = pixValue;
                        pixBufferB[j] = (0xff000000) | (Rb << 16) | (Gb << 8) | Bb;
                    }
                }
            }

            importImage(paintBuffer);

            if (imageB != null) {
                importImageB(pixBufferB);
            }
        }

        paintComponent(getGraphics());

        return true;
    }

    /**
     * Shows the image and the VOI(s).
     *
     * @param   tSlice     t (time) slice to show
     * @param   zSlice     z slice to show
     * @param   _LUTa      LUTa - to change to new LUT for imageA else null
     * @param   _LUTb      LUTb - to change to new LUT for imageB else null
     * @param   forceShow  forces this method to import image and recalculate java image
     *
     * @return  boolean to indicate if the show was successful
     */
    public boolean show(int tSlice, int zSlice, ModelLUT _LUTa, ModelLUT _LUTb, boolean forceShow) {

        float rangeA = 0, rangeB = 0;
        float remapConstA = 1, remapConstB = 1;
        float imageMinA = 0, imageMaxA = 0, imageMinB = 0, imageMaxB = 0;
        int xDim, yDim;
        int bufferSize;
        int lutHeightA = 0, lutHeightB = 0;
        int index;
        float[][] RGB_LUTa = null, RGB_LUTb = null;
        int[][] iRGB_LUTa = null, iRGB_LUTb = null;
        int Ra, Ga, Ba, Rb, Gb, Bb;
        int indexA, indexB;
        int pix;

        time = System.currentTimeMillis();

        if (imageA.isColorImage()) {

            // call the show method for displaying RGB images
            return (show(tSlice, zSlice, forceShow));
        }

        if (imageA == null) {
            return false;
        }

        if ((LUTa == null) && (_LUTb == null)) {
            return false;
        }

        if (_LUTa != null) {
            LUTa = _LUTa;
        }

        if ((imageB != null) && (_LUTb != null)) {
            LUTb = _LUTb;
        }

        lutHeightA = LUTa.getExtents()[1];

        if (LUTb != null) {
            lutHeightB = LUTb.getExtents()[1];
        }

        xDim = imageExtents[0];
        yDim = imageExtents[1];

        if (lutHeightA != lutBufferRemapped.length) {

            try {
                lutBufferRemapped = new int[lutHeightA];
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: ComponentRegistration.show");

                return false;
            }
        }

        if (imageB == null) {
            LUTa.exportIndexedLUT(lutBufferRemapped);
        }

        bufferSize = xDim * yDim;

        if (imageA.getType() == ModelStorageBase.UBYTE) {
            imageMinA = 0;
            imageMaxA = 255;
        } else if (imageA.getType() == ModelStorageBase.BYTE) {
            imageMinA = -128;
            imageMaxA = 127;
        } else {
            imageMinA = (float) imageA.getMin();
            imageMaxA = (float) imageA.getMax();
        }

        rangeA = imageMaxA - imageMinA;

        if (rangeA == 0) {
            rangeA = 1;
        }

        if ((lutHeightA - 1) == 0) {
            remapConstA = 1;
        } else {
            remapConstA = (lutHeightA - 1) / rangeA;
        }

        if (imageB != null) {

            if (imageB.getType() == ModelStorageBase.UBYTE) {
                imageMinB = 0;
                imageMaxB = 255;
            } else if (imageB.getType() == ModelStorageBase.BYTE) {
                imageMinB = -128;
                imageMaxB = 127;
            } else {
                imageMinB = (float) imageB.getMin();
                imageMaxB = (float) imageB.getMax();
            }

            rangeB = imageMaxB - imageMinB;

            if (rangeB == 0) {
                rangeB = 1;
            }

            if ((lutHeightB - 1) == 0) {
                remapConstB = 1;
            } else {
                remapConstB = (lutHeightB - 1) / rangeB;
            }

            RGB_LUTa = LUTa.exportRGB_LUT(true);
            RGB_LUTb = LUTb.exportRGB_LUT(true);
            iRGB_LUTa = new int[3][RGB_LUTa[0].length];
            iRGB_LUTb = new int[3][RGB_LUTb[0].length];

            for (int c = 0; c < RGB_LUTa[0].length; c++) {
                iRGB_LUTa[0][c] = (int) ((RGB_LUTa[0][c] * alphaBlend) + 0.5f);
                iRGB_LUTb[0][c] = (int) ((RGB_LUTb[0][c] * alphaPrime) + 0.5f);
                iRGB_LUTa[1][c] = (int) ((RGB_LUTa[1][c] * alphaBlend) + 0.5f);
                iRGB_LUTb[1][c] = (int) ((RGB_LUTb[1][c] * alphaPrime) + 0.5f);
                iRGB_LUTa[2][c] = (int) ((RGB_LUTa[2][c] * alphaBlend) + 0.5f);
                iRGB_LUTb[2][c] = (int) ((RGB_LUTb[2][c] * alphaPrime) + 0.5f);
            }
        }

        red = 128;
        green = 0;
        blue = 0;

        if ((slice != zSlice) || (timeSlice != tSlice) || (forceShow == true)) {
            slice = zSlice;
            timeSlice = tSlice;

            int zDimSlices = 0;

            // if (imageA.getNDims() >= 3) zDimSlices = imageA.getExtents()[2];
            if (imageA.getNDims() >= 3) {
                zDimSlices = imageExtents[2];
            }

            try {

                if (imageA.getType() == ModelStorageBase.COMPLEX) {
                    imageA.exportComplexSliceXY((timeSlice * zDimSlices) + slice, imageBufferA, logMagDisplay);
                } else if ((orientation == AXIAL) || (orientation == NA)) {
                    imageA.exportSliceXY((timeSlice * zDimSlices) + slice, imageBufferA);
                } else if (orientation == SAGITTAL) {

                    imageA.exportSliceZY(timeSlice, zSlice, imageBufferA);
                } else if (orientation == CORONAL) {
                    imageA.exportSliceXZ(timeSlice, zSlice, imageBufferA);
                }

                if (imageB != null) {

                    if (imageB.getType() == ModelStorageBase.COMPLEX) {
                        imageB.exportComplexSliceXY((timeSlice * zDimSlices) + slice, imageBufferB, logMagDisplay);
                    } else if ((orientation == AXIAL) || (orientation == NA)) {
                        imageB.exportSliceXY((timeSlice * zDimSlices) + slice, imageBufferB);
                    } else if (orientation == SAGITTAL) {
                        imageB.exportSliceZY(timeSlice, slice, imageBufferB);
                    } else if (orientation == CORONAL) {
                        imageB.exportSliceXZ(timeSlice, slice, imageBufferB);
                    }
                }
            } catch (IOException error) {
                MipavUtil.displayError("" + error); // Need to fix this

                return false;
            }

            pix = 0;

            if (imageB == null) {
                TransferFunction tf_imgA = LUTa.getTransferFunction();

                for (index = 0; index < bufferSize; index++) {
                    pix = (int) (tf_imgA.getRemappedValue(imageBufferA[index], 256) + 0.5);
                    paintBuffer[index] = pixBuffer[index] = lutBufferRemapped[pix];
                }
            } else {
                indexA = indexB = 0;

                TransferFunction tf_imgA = LUTa.getTransferFunction();
                TransferFunction tf_imgB = LUTb.getTransferFunction();

                for (index = 0; index < bufferSize; index++) {
                    indexA = (int) (tf_imgA.getRemappedValue(imageBufferA[index], 256) + 0.5);
                    indexB = (int) (tf_imgB.getRemappedValue(imageBufferB[index], 256) + 0.5);

                    Ra = iRGB_LUTa[0][indexA];
                    Rb = iRGB_LUTb[0][indexB];
                    Ga = iRGB_LUTa[1][indexA];
                    Gb = iRGB_LUTb[1][indexB];
                    Ba = iRGB_LUTa[2][indexA];
                    Bb = iRGB_LUTb[2][indexB];

                    if (columnCheckers == -1) { // no checkerboarding
                        Ra = (Ra + Rb);
                        Ga = (Ga + Gb);
                        Ba = (Ba + Bb);
                    } else { // checkerboarding

                        if (!checkerRegBitmap.get(index)) {
                            Ra = Rb;
                            Ga = Gb;
                            Ba = Bb;
                        }
                    }

                    pix = (0xff000000) | (Ra << 16) | (Ga << 8) | Ba;
                    paintBuffer[index] = pix;
                    pixBufferB[index] = (0xff000000) | ((int) (RGB_LUTb[0][indexB]) << 16) |
                                            ((int) (RGB_LUTb[1][indexB]) << 8) | (int) (RGB_LUTb[2][indexB]);
                }
            }

            importImage(paintBuffer);

            if (imageB != null) {
                importImageB(pixBufferB);
            }

            time = System.currentTimeMillis() - time;
        } else {

            if (imageB == null) {

                for (index = 0; index < bufferSize; index++) {
                    pix = (int) (((imageBufferA[index] - imageMinA) * remapConstA) + 0.5);
                    paintBuffer[index] = pixBuffer[index] = lutBufferRemapped[pix];
                }
            } else {

                for (index = 0; index < bufferSize; index++) {
                    indexA = (short) (((imageBufferA[index] - imageMinA) * remapConstA) + 0.5);
                    indexB = (short) (((imageBufferB[index] - imageMinB) * remapConstB) + 0.5);
                    Ra = iRGB_LUTa[0][indexA];
                    Rb = iRGB_LUTb[0][indexB];
                    Ga = iRGB_LUTa[1][indexA];
                    Gb = iRGB_LUTb[1][indexB];
                    Ba = iRGB_LUTa[2][indexA];
                    Bb = iRGB_LUTb[2][indexB];

                    if (columnCheckers == -1) { // no checkerboarding
                        Ra = (Ra + Rb);
                        Ga = (Ga + Gb);
                        Ba = (Ba + Bb);
                    } else { // checkerboarding

                        if (!checkerRegBitmap.get(index)) {
                            Ra = Rb;
                            Ga = Gb;
                            Ba = Bb;
                        }
                    }

                    pix = (0xff000000) | (Ra << 16) | (Ga << 8) | Ba;
                    paintBuffer[index] = pix;
                    pixBufferB[index] = (0xff000000) | ((int) (RGB_LUTb[0][indexB]) << 16) |
                                            ((int) (RGB_LUTb[1][indexB]) << 8) | (int) (RGB_LUTb[2][indexB]);
                }
            }

            importImage(paintBuffer);

            if (imageB != null) {
                importImageB(pixBufferB);
            }
        }

        paintComponent(getGraphics());

        return true;
    }

    /**
     * Resets the buffer to 0s and displays a blank image.
     *
     * @return  boolean to indicate that the show was successful
     */
    public boolean showBlank() {
        int i;

        for (i = 0; i < pixBuffer.length; i++) {
            pixBuffer[i] = 0;
        }

        importImage(pixBuffer); // Method in parent class to import the image
        slice = -99;

        return true;
    }

    /**
     * tells whether or not to draw VOIs when paintComponent() is called.
     *
     * @param  doShow  DOCUMENT ME!
     */
    public void showVOIs(boolean doShow) {
        this.showVOIs = doShow;
    }

    /**
     * Calls paintComponent - reduces flicker.
     *
     * @param  g  graphics
     */
    public void update(Graphics g) {
        this.paintComponent(g);
    }

    /**
     * Cleans up memory.
     */
    protected void finalize() {
        dispose(true);
    }

}
