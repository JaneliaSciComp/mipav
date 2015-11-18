package gov.nih.mipav.view;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.structures.*;


import java.awt.*;
import java.awt.event.*;

import java.io.*;


/**
 * DOCUMENT ME!
 *
 * @author  William Gandler
 */
public class ViewJComponentRegistration
    extends ViewJComponentEditImage
    implements MouseMotionListener, MouseListener {

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

    /** Opacity value used by the paint brush. value = 1.0 - opaque value = 0.25 - default (mostly see through) */
    public float OPACITY = 0.25f;

    /** DOCUMENT ME! */
    protected Cursor addPointCursor = new Cursor(Cursor.HAND_CURSOR);

    /** DOCUMENT ME! */
    protected Cursor defaultCursor = new Cursor(Cursor.DEFAULT_CURSOR);

    /** Custom cursor: magnify region. */
    protected Cursor magRegionCursor;

    /** DOCUMENT ME! */
    protected Cursor moveCursor = new Cursor(Cursor.MOVE_CURSOR);

    /** DOCUMENT ME! */
    protected Cursor pointCursor = new Cursor(Cursor.NE_RESIZE_CURSOR);

    /** DOCUMENT ME! */
    protected Cursor rectCursor = new Cursor(Cursor.DEFAULT_CURSOR);

    /** DOCUMENT ME! */
    protected Cursor removePointCursor = new Cursor(Cursor.E_RESIZE_CURSOR);

    /** DOCUMENT ME! */
    protected Cursor waitCursor = new Cursor(Cursor.WAIT_CURSOR);

    /** DOCUMENT ME! */
    protected Cursor wandCursor = new Cursor(Cursor.HAND_CURSOR);

    /** Frame where component image is displayed (only for new RegistrationTool). */
    protected ViewJFrameRegistrationTool toolFrame;

    /** DOCUMENT ME! */
    int memCount = 0;

    /** number of VOIs for adjustable slice */
    private int adjMark = 0; 

    /** DOCUMENT ME! */
    private Point anchorPt = new Point(0, 0);

    /** id of rotation center */
    private short centerid; 

    /** center rotation VOI */
    private VOI centerVOI = null;

    /** DOCUMENT ME! */
    private boolean doAdjMark = false;

    /** DOCUMENT ME! */
    private boolean doCenter = false;

    /** no adjusted slice movements with mouseDrag */
    private boolean doDrag = true; 

    /** DOCUMENT ME! */
    private boolean doRefMark = false;

    /** DOCUMENT ME! */
    private float hue;

    /** DOCUMENT ME! */
    private short id;

    /** DOCUMENT ME! */
    private int[] markerType;

    /** DOCUMENT ME! */
    private VOI newVOI = null;

    /** adjustable slice VOI. */
    private int newX, newY; 

    /** Used to set old or new mode of registration. */
    private boolean oldFrame = true;

    /** DOCUMENT ME! */
    private float[] ptCoord;

    /** DOCUMENT ME! */
    private int red, green, blue;

    /** number of VOIs for reference slice */
    private int refMark = 0;

    /** number of VOIs for rotation center */
    private int rotCenterMark = 0; 

    /** values obtained when mouseReleased */
    private float xFinish, yFinish; 

    /** DOCUMENT ME! */
    private int[] xOrg;

    /** DOCUMENT ME! */
    private int[] xPres;

    /** DOCUMENT ME! */
    private float xRotation;

    /** values obtained when mousePressed */
    private float xStart, yStart;

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
        super( _frame, _imageA, _LUTa, imgBufferA, _imageB, _LUTb, imgBufferB, pixelBuffer, zoom, extents, logMagDisplay, _orientation );

        alphaBlend = alphaBl;

        xOrg = new int[40];
        yOrg = new int[40];
        xPres = new int[40];
        yPres = new int[40];
        markerType = new int[40];
        ptCoord = new float[2];

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
        super( _toolFrame, _imageA, _LUTa, imgBufferA, _imageB, _LUTb, imgBufferB, pixelBuffer, zoom, extents, logMagDisplay, _orientation );

        oldFrame = false;
        toolFrame = _toolFrame;
        alphaBlend = alphaBl;

        xOrg = new int[40];
        yOrg = new int[40];
        xPres = new int[40];
        yPres = new int[40];
        markerType = new int[40];
        ptCoord = new float[2];

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
        int i, nVOI;

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
        super.disposeLocal();

        if (gcFlag == true) {
            System.gc();
        }
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
     * Accessor that returns int green.
     *
     * @return  int green component
     */
    public int getGreen() {
        return green;
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
     * Returns the number of VOIs.
     *
     * @return  int number of VOIs
     */
    public int getnVOI() {
        ViewVOIVector VOIs = imageActive.getVOIs();

        return (VOIs.size());
    }

    /**
     * Returns the opacity used in alpha blending between the input image and
     * the reference image.
     *
     * @return  float opacity
     */
    public float getOpacity() {
        return OPACITY;
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
     * Constructs reference point VOI from the refPoint passed in.
     *
     * @param  adjPoint  a point VOI is generated from the 3D point
     */
    public void makeAdjustableVOI(Vector3f adjPoint) {
        hue = 1.0f / 3.0f; // hue for green

        try {
            float[] x = new float[1];
            float[] y = new float[1];
            float[] z = new float[1];

            id = (short) imageActive.getVOIs().size();
            newVOI = new VOI(id, "point2d.voi", VOI.POINT, hue);
            x[0] = adjPoint.X;
            y[0] = adjPoint.Y;
            z[0] = 0.0f;
            newVOI.importCurve(x, y, z);
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentRegistration.mouseReleased");
            setCursorMode(DEFAULT);

            return;
        }

        adjMark++;
        xOrg[id] = (int) adjPoint.X;
        yOrg[id] = (int) adjPoint.Y;
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
    public void makeReferenceVOI(Vector3f refPoint) {
        hue = 0.0f; // hue for red

        try {
            float[] x = new float[1];
            float[] y = new float[1];
            float[] z = new float[1];

            id = (short) imageActive.getVOIs().size();
            newVOI = new VOI(id, "point2d.voi", VOI.POINT, hue);
            x[0] = refPoint.X;
            y[0] = refPoint.Y;
            z[0] = 0.0f;
            newVOI.importCurve(x, y, z);
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentRegistration.mouseReleased");
            setCursorMode(DEFAULT);

            return;
        }

        refMark++;
        xOrg[id] = (int) refPoint.X;
        yOrg[id] = (int) refPoint.Y;
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

        if ((mouseEvent.getClickCount() == 1) && ((cursorMode == DEFAULT) || (cursorMode == TRANSLATE) || (cursorMode == ROTATE))) {
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

        if ((doDrag == true) && ((cursorMode == TRANSLATE) || (cursorMode == ROTATE))) {
            xFinish = xS * resolutionX;
            yFinish = yS * resolutionY;

            if (oldFrame) {
                ((ViewJFrameRegistration)frame).setMove(xStart, yStart, xFinish, yFinish);
            } else {
                toolFrame.setMove(xStart, yStart, xFinish, yFinish);
            }

            xStart = xFinish;
            yStart = yFinish;

            if (cursorMode == ROTATE) {
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

        if (cursorMode == MOVE) {

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
     * Sets the mode of the program depending on the cursor mode. If the mode
     * is move, activates the contour or line and enables the delete button.
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
            setCursorMode(DEFAULT);

            return;
        }

        if (cursorMode == MOVE) {
            anchorPt.setLocation(xS, yS); // For use in dragging VOIs

            VOIs = imageActive.getVOIs();
            nVOI = VOIs.size();

            for (i = 0; i < nVOI; i++) {
                VOIs.VOIAt(i).setAllActive(false); // deactivate all other VOIs
            }

            for (i = 0; i < nVOI; i++) {

                if (VOIs.VOIAt(i).getCurveType() == VOI.POINT) { // curve type is Point

                    if (VOIs.VOIAt(i).nearPoint(x, y, slice)) {

                        VOIs.VOIAt(i).setAllActive(true);
                        if (oldFrame) {
                            frame.updateImages(true);
                        } else {
                            toolFrame.updateImages(true);
                        }

                        return;
                    }
                }
            }
        } else if (cursorMode == ROTATE) {

            if ((doCenter) && (rotCenterMark == 0)) {
                hue = 1.0f / 6.0f; // hue for yellow

                try {
                    float[] xR = new float[1];
                    float[] yR = new float[1];
                    float[] zR = new float[1];

                    id = (short) imageActive.getVOIs().size();
                    centerid = id;
                    newVOI = new VOI(id, "point2d.voi", VOI.POINT, hue);
                    centerVOI = newVOI;
                    xR[0] = xS;
                    yR[0] = yS;
                    zR[0] = 0.0f;
                    newVOI.importCurve(xR, yR, zR);
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: ComponentRegistration.mouseReleased");
                    setCursorMode(DEFAULT);

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
                    ((ViewJFrameRegistration)frame).setRotationCenter(xRotation, yRotation);
                    frame.updateImages(true);
                } else {
                    toolFrame.setRotationCenter(xRotation, yRotation);
                    toolFrame.updateImages(true);
                }
            } // end of if ((doCenter) && (rotCenterMark == 0))
            else if ((doCenter) && (rotCenterMark == 1)) {
                deltaX = xS - xOrg[centerid];
                deltaY = yS - yOrg[centerid];

                ((VOIPoint) (centerVOI.getCurves().elementAt(0))).moveVOIPoint(deltaX, deltaY, 0,
                        imageActive.getExtents()[0],
                        imageActive.getExtents()[1], 1);
                xOrg[centerid] = xS;
                yOrg[centerid] = yS;
                xRotation = xS * resolutionX;
                yRotation = yS * resolutionY;

                if (oldFrame) {
                    ((ViewJFrameRegistration)frame).setRotationCenter(xRotation, yRotation);
                    frame.updateImages(true);
                } else {
                    toolFrame.setRotationCenter(xRotation, yRotation);
                    toolFrame.updateImages(true);
                }
            }
        } // else if (mode == ROTATE)
    }

    /**
     * If the mode is level set, draws level sets as user moves
     * mouse. Otherwise, changes the cursor depending on where the mouse is in
     * relation to the VOI.
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

        if (cursorMode == WIN_REGION) {
            super.mouseMoved(mouseEvent);
            g.dispose();

            return;
        }

        if ((cursorMode == POINT_VOI) || (cursorMode == TRANSLATE) || (cursorMode == ROTATE)) {
            g.dispose();

            return;
        }

        VOIs = imageActive.getVOIs(); // Get the VOIs from the active image.
        nVOI = VOIs.size();

        for (i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.POINT) {

                if (VOIs.VOIAt(i).isVisible() &&
                        VOIs.VOIAt(i).nearPoint(x, y, slice)) {
                    setCursorMode(MOVE);
                    g.dispose();

                    return;
                }
            }
        }

        setCursorMode(DEFAULT);
        // setCursor(crosshairCursor);
    }

    
    /**
     * This function sets up and draws the VOI according to the mode.
     *
     * @param  mouseEvent  event that triggered function
     */
    public void mouseReleased(MouseEvent mouseEvent) {
        int xS, yS;
        if (modifyFlag == false) {
            return;
        }

        xS = Math.round((mouseEvent.getX() / (getZoomX() * resolutionX)) - 0.5f);
        yS = Math.round((mouseEvent.getY() / (getZoomY() * resolutionY)) - 0.5f);

        if ((xS < 0) || (xS >= imageActive.getExtents()[0]) || (yS < 0) || (yS >= imageActive.getExtents()[1])) {
            return;
        }

        if ((doDrag == false) && ((cursorMode == TRANSLATE) || (cursorMode == ROTATE))) {
            xFinish = xS * resolutionX;
            yFinish = yS * resolutionY;

            if (oldFrame) {
                ((ViewJFrameRegistration)frame).setMove(xStart, yStart, xFinish, yFinish);
            } else {
                toolFrame.setMove(xStart, yStart, xFinish, yFinish);
            }
        }

        if (cursorMode == POINT_VOI) {

            if (doRefMark) {
                hue = 0.0f; // hue for red

                try {
                    float[] x = new float[1];
                    float[] y = new float[1];
                    float[] z = new float[1];

                    id = (short) imageActive.getVOIs().size();
                    newVOI = new VOI(id, "point2d.voi", VOI.POINT, hue);
                    x[0] = xS;
                    y[0] = yS;
                    z[0] = slice;
                    newVOI.importCurve(x, y, z);
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: ComponentRegistration.mouseReleased");
                    setCursorMode(DEFAULT);

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
                    newVOI = new VOI(id, "point2d.voi", VOI.POINT, hue);
                    x[0] = xS;
                    y[0] = yS;
                    z[0] = slice;
                    newVOI.importCurve(x, y, z);
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: ComponentRegistration.mouseReleased");
                    setCursorMode(DEFAULT);

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

        if ((MipavUtil.getFreeHeapMemory() < 20000000) || (memCount >= 15)) {
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
        //int i;
        //int nVOI;
        //int curRefMark;
        //int curAdjMark;
        //ViewVOIVector VOIs = null;

        try {

            if (g == null) {
                MipavUtil.displayError("ComponentRegistration.paintComponent: graphics = null");

                return;
            }
            super.paintComponent(g);
            // Draw VOIs (unless told not to by showVOIs = false)
            /*
            if (showVOIs) {
                if (useDualVOIs) {

                    if (orientation == FileInfoBase.UNKNOWN_ORIENT) {
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

                    if (orientation == FileInfoBase.UNKNOWN_ORIENT) {
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
                                    ((VOIPoint) (VOIs.VOIAt(i).getCurvesTemp()[slice].elementAt(0))).setLabel(String.valueOf(curRefMark));
                                    VOIs.VOIAt(i).drawSelf(getZoomX(), getZoomY(), resolutionX, resolutionY, 0f, 0f,
                                                           imageActive.getFileInfo(0).getResolutions(),
                                                           imageActive.getFileInfo(0).getUnitsOfMeasure(), slice,
                                                           orientation, g);
                                    curRefMark--;
                                } // end of else if (markerType[i] == REFMARK)
                                else if (markerType[i] == ADJMARK) {
                                    VOIs.VOIAt(i).setColor(Color.green);
                                    ((VOIPoint) (VOIs.VOIAt(i).getCurvesTemp()[slice].elementAt(0))).setLabel(String.valueOf(curAdjMark));
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
            } */
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

        //int x = mouseEvent.getX();
        //int y = mouseEvent.getY();

        if (g == null) {
            MipavUtil.displayError("ComponentRegistration.paintMagComponent: graphics = null");

            return;
        }

        if (mouseEvent.isShiftDown() == false) {
        	
            //super.paintWindowComponent(g, x, y, 100, 100, getZoomX());
        } else if (mouseEvent.isShiftDown() == true) {

            // super.paintMagComponent(g, x, y, MAGR_WIDTH, MAGR_HEIGHT, getZoomX(),
            // imageBufferActive, imageActive.getType(),                        imageActive.getExtents()[0],
            // getActiveImage().getMin(), getActiveImage().getMax());
            super.update(getGraphics());
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
                newVOI = new VOI(id, "center.voi", VOI.POINT, hue);
                centerVOI = newVOI;
                xR[0] = xS;
                yR[0] = yS;
                zR[0] = 0.0f;
                newVOI.importCurve(xR, yR, zR);
                ((VOIPoint) newVOI.getCurves().elementAt(0)).setLabel("C");
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: ComponentRegistration.setCenter");
                setCursorMode(DEFAULT);

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
                ((ViewJFrameRegistration)frame).setRotationCenter(xRotation, yRotation);
                frame.updateImages(true);
            } else {
                toolFrame.setRotationCenter(xRotation, yRotation);
                toolFrame.updateImages(true);
            }
        } // end of if ((doCenter) && (rotCenterMark == 0))
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
        super.setLUTa(LUT);

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
        super.setLUTb(LUT);

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
    public void setCursorMode(int mode) {
        this.cursorMode = mode;
        //getVOIHandler().setMode(mode);
        switch (mode) {

            case DEFAULT:
                setCursor(crosshairCursor);
                if (oldFrame) {
                    ((ViewJFrameRegistration)frame).setDefaultMode();
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
            	super.setCursorMode(WIN_REGION);
                setCursor(magRegionCursor);
                break;
        }
    }

    /**
     * doDrag true enables the adjusted slice to respond to mouse press and
     * drag events doDrag false restricts the adjusted slice to responding to
     * mouse press and release events.
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
     * Sets the reference mark flag.
     *
     * @param  doRef  DOCUMENT ME!
     */
    public void setRefMark(boolean doRef) {
        doRefMark = doRef;
    }

    /**
     * Cleans up memory.
     */
    protected void finalize() {
        dispose(true);
    }

}
