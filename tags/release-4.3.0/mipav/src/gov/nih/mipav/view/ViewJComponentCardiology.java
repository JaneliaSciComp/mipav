package gov.nih.mipav.view;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;


/**
 * Basic displayable image object in MIPAV. Contains the viewable objects such as VOIs and carries listeners to most any
 * action that can happen in a window.
 *
 * @version  0.1 Nov 18, 1997
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class ViewJComponentCardiology extends ViewJComponentEditImage
        implements MouseMotionListener, MouseListener, PaintGrowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7846145645892843524L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * opacity value used by the paint brush. The opacity value may range from 0 to 1, where 1 is completely opaque, and
     * 0 is completely invisible.
     *
     * <p>Opacity definitions:</p>
     *
     * <ol>
     *   <li>
     *     <ul>
     *       <li>Opaque is 1.0</li>
     *     </ul>
     *
     *     <ul>
     *       <li>default is 0.25</li>
     *     </ul>
     *
     *     <ul>
     *       <li>Invisible is 0.0</li>
     *     </ul>
     *   </li>
     * </ol>
     *
     * <p>Values outside the bounds of 0.0 to 1.0 are not defined.</p>
     */
    public float opacity = 0.25f;

    /** Flag to indicate if DICOM overlay should be displayed. */
    protected boolean overlayOn = false;

    /** DOCUMENT ME! */
    private ViewJPopupCardiologyVOI popupCard = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor: ImageA and ImageB are expected to be of the same dimensionality !!
     *
     * @param  _frame         frame where image(s) will be displayed
     * @param  _imageA        Model of the image that will be displayed
     * @param  _LUTa          LUT used to display imageA
     * @param  imgBufferA     storage buffer used to display image A
     * @param  pixelBuffer    storage buffer used to build a displayable image
     * @param  zoom           initial magnification of image
     * @param  extents        initial display dimensions of the image
     * @param  logMagDisplay  display log magnitude of image
     * @param  _orientation   orientation of the image
     */
    public ViewJComponentCardiology(ViewJFrameBase _frame, ModelImage _imageA, ModelLUT _LUTa, float[] imgBufferA,
                                    int[] pixelBuffer, float zoom, int[] extents, boolean logMagDisplay,
                                    int _orientation) {
        super(_frame, _imageA, _LUTa, imgBufferA, null, null, null, pixelBuffer, zoom, extents, logMagDisplay,
              _orientation);

        removeMouseListener(voiHandler.getPopupPt());
        removeMouseListener(voiHandler.getPopupVOI());
        voiHandler.getPopupPt().setEnabled(false);
        voiHandler.getPopupVOI().setEnabled(false);

        // getVOIHandler().removeVOIUpdateListener(getVOIHandler().getVOIDialog());
        popupCard = new ViewJPopupCardiologyVOI(this);
    }

    /* ********************************************************************** */

    /* ****************************** Accessors ***************************** */

    /* ********************************************************************** */

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public void calculateVOIIntensities() {
        VOICardiology cardioVOI = ((VOICardiology) imageActive.getVOIs().VOIAt(0).getCurves()[0].elementAt(0));
        boolean[] activeArray = cardioVOI.getActive();
        float[] avgIntensities = new float[2];

        int sliceSize = imageActive.getSliceSize();

        float[] imageGraphBuffer = new float[sliceSize];


        try {
            imageActive.exportData(0, sliceSize, imageGraphBuffer);
        } catch (Exception ex) {
            System.err.println("exception, woohoo");
        }

        for (int i = 0; i < activeArray.length; i++) {

            if (activeArray[i]) {
                avgIntensities = cardioVOI.calcAverageIntensity(imageGraphBuffer, i, imageActive.getExtents()[0]);

                System.err.println(i + " entire section: " + avgIntensities[0] + ", infarction: " + avgIntensities[1]);

                //        System.err.println("i: " + cardioVOI.calcAverageIntensity(voiHandler.getImageGraphBuffer(), i,
                // imageActive.getExtents()[0], imageActive.getUserInterface()));
            }
        }

        voiHandler.setImageGraphBuffer(imageGraphBuffer);
    }

    /**
     * DOCUMENT ME!
     */
    public void calculateVOILengths() {
        // System.err.println("calculating segment lengths");

        VOICardiology cardioVOI = ((VOICardiology) imageActive.getVOIs().VOIAt(0).getCurves()[0].elementAt(0));
        boolean[] activeArray = cardioVOI.getActive();
        int numActive = 0; // counter for number of active sections

        // for each active section, run through and calculate lengths
        for (int i = 0; i < activeArray.length; i++) {

            if (activeArray[i]) {
                numActive++;

                cardioVOI.calcAverageLength(i, imageActive.getExtents()[0], imageActive.getExtents()[1],
                                            imageActive.getResolutions(0));

            }
        }

    }

    /**
     * Sets all variables to null, disposes, and garbage collects.
     *
     * @param  flag  if true garbage collector should be called.
     */
    public void disposeLocal(boolean flag) {

        if (flag == true) {
            super.disposeLocal();
        }
    }

    /**
     * Erases all paint.
     */
    public void eraseAllPaint() {
        super.eraseAllPaint(false);
    }


    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean getOnTop() {
        return onTop;
    }

    // ************************************************************************
    // ***************************** Mouse Events *****************************
    // ************************************************************************

    /**
     * A mouse event. When the mouse is clicked in the image, several different things may happen. If a Volume of
     * Interest (VOI) is selected and the click count is 2, a VOI dialog should pop up. If the click count is 1 and the
     * mouse is in an VOI, it should select the VOI. In all other cases, a click within the image but not in an VOI
     * should deselect all VOIs.
     *
     * @param  mouseEvent  event that triggers function; contains click count
     */
    public void mouseClicked(MouseEvent mouseEvent) {
        super.mouseClicked(mouseEvent);

        // xS = Math.round( mouseEvent.getX() / (getZoomX() * resolutionX)); // zoomed x.  Used as cursor x
        // yS = Math.round( mouseEvent.getY() / (getZoomY() * resolutionY)); // zoomed y.  Used as cursor y
        int xS = Math.round((mouseEvent.getX() / (getZoomX() * resolutionX)) - 0.5f); // zoomed x.  Used as cursor
        int yS = Math.round((mouseEvent.getY() / (getZoomY() * resolutionY)) - 0.5f); // zoomed y.  Used as cursor

        if ((xS < 0) || (xS >= imageActive.getExtents()[0]) || // Check for validity
                (yS < 0) || (yS >= imageActive.getExtents()[1])) {
            return;
        }

        // System.out.println("Mode is " + mode);
        if ((mouseEvent.getClickCount() == 1) && (cursorMode == DEFAULT)) {
            // System.out.println("Deactivating all VOIs");

            voiHandler.selectAllVOIs(false);
            // voiHandler.setLastPointVOI_ID(-1); // next mouseClick will deactivate point VOI unless reselected

            imageActive.notifyImageDisplayListeners();
        }
    }

    // ************************************************************************
    // ************************** Mouse Motion Events *************************
    // ************************************************************************

    /**
     * A mouse event. Drags a VOI real time by calling a series of translations and draws. Once the mouse is released,
     * the positions are reset permenantly. Also rubberbands points if the cursor indicates it by calling rubberbandVOI.
     *
     * @param  mouseEvent  event that triggered function
     */
    public void mouseDragged(MouseEvent mouseEvent) {

        if ((pixBuffer == null) || (imageBufferActive == null) || (modifyFlag == false)) {
            return;
        }

        int xS = Math.round((mouseEvent.getX() / (getZoomX() * resolutionX)) - 0.5f); // zoomed x.  Used as cursor
        int yS = Math.round((mouseEvent.getY() / (getZoomY() * resolutionY)) - 0.5f); // zoomed y.  Used as cursor

        int xDim = imageActive.getExtents()[0];
        int yDim = imageActive.getExtents()[1];

        if ((xS < 0) || (xS >= xDim) || (yS < 0) || (yS >= yDim)) {
            return;
        }

        // System.err.println("Dragging");
        processDefaultMouseDrag(mouseEvent, xS, yS);

        ViewVOIVector VOIs = imageActive.getVOIs();
        int nVOI = VOIs.size();

        if ((cursorMode == MOVE_INTERSECTION_POINT) || (cursorMode == MOVE_POINT)) {

            if ((nVOI == 1) && (VOIs.VOIAt(0).getCurveType() == VOI.CARDIOLOGY)) {
                xS = Math.round((mouseEvent.getX() / (getZoomX() * resolutionX)));
                yS = Math.round((mouseEvent.getY() / (getZoomY() * resolutionY)));
                setCursor(MipavUtil.blankCursor);

                VOIs.VOIAt(0).rubberbandVOI(xS, yS, slice, xDim, yDim, false);
                imageActive.notifyImageDisplayListeners(null, true);
            }

            return;

        }

        getGraphics().dispose();
    }

    /**
     * A mouse event. If the mode is level set, draws level sets as user moves mouse. Otherwise, changes the cursor
     * depending on where the mouse is in relation to the VOI.
     *
     * @param  mouseEvent  event that triggered the function
     */
    public void mouseMoved(MouseEvent mouseEvent) {
        Graphics g = getGraphics();

        removeMouseListener(popupCard);

        if ((g == null) || (modifyFlag == false) || (slice == -99)) {
            return;
        }

        if ((pixBuffer == null) || (imageBufferActive == null)) {
            g.dispose();

            return;
        }

        int xS = Math.round((mouseEvent.getX() / (getZoomX() * resolutionX)) - 0.5f); // zoomed x.  Used as cursor
        int yS = Math.round((mouseEvent.getY() / (getZoomY() * resolutionY)) - 0.5f); // zoomed y.  Used as cursor

        int x = mouseEvent.getX();
        int y = mouseEvent.getY();

        if ((xS < 0) || (xS >= imageActive.getExtents()[0]) || // Check to ensure point is within
                (yS < 0) || (yS >= imageActive.getExtents()[1])) { // the image bounds
            g.dispose();

            return;
        }

        if ((cursorMode == RECTANGLE) || (cursorMode == ELLIPSE) || (cursorMode == LINE) ||
                (cursorMode == RECTANGLE3D) || (cursorMode == POINT_VOI) || (cursorMode == POLYLINE) ||
                (cursorMode == LEVELSET) || (cursorMode == PAINT_VOI) || (cursorMode == DROPPER_PAINT) ||
                (cursorMode == ERASER_PAINT) || (cursorMode == QUICK_LUT) || (cursorMode == PROTRACTOR) ||
                (cursorMode == LIVEWIRE) || (cursorMode == ANNOTATION)) {
            g.dispose();

            return;
        }

        int nVOI = 0;
        ViewVOIVector VOIs = imageActive.getVOIs(); // Get the VOIs from the active image.

        if (VOIs != null) {
            nVOI = VOIs.size();

            // only handle ONE voi (VOICardiology type)
            if ((nVOI != 1) || (VOIs.VOIAt(0).getCurveType() != VOI.CARDIOLOGY)) {
                return;
            }

            int index = VOIs.VOIAt(0).nearCardioPoint(x, y, slice, getZoomX(), resolutionX, resolutionY);

            if (index >= 0) {
                setCursorMode(MOVE_INTERSECTION_POINT);
                g.dispose();

                return;
            } else if (index == VOICardiology.BOTH) {
                setCursorMode(MOVE_INTERSECTION_POINT);
                g.dispose();

                return;
            } else if (index == -1) {

                if (mouseEvent.isShiftDown()) {
                    setCursorMode(DELETE_POINT);
                } else {
                    setCursorMode(MOVE_POINT);
                }

                g.dispose();

                return;
            } else if (VOIs.VOIAt(0).nearCardioLine(xS, yS, slice)) {
                setCursorMode(NEW_POINT);
                g.dispose();

                return;
            } else {

                // System.err.println("doing section check");
                int section = ((VOICardiology) VOIs.VOIAt(0).getCurves()[0].elementAt(0)).getSection(xS, yS);

                if (section >= 0) {

                    // System.err.println("Inside section: " + section);
                    setCursorMode(MOVE);
                    addMouseListener(popupCard);
                    g.dispose();

                    return;
                }
            }


        }

        Vector[] curves;

        for (int i = 0; i < nVOI; i++) {
            int curveType = VOIs.VOIAt(i).getCurveType();

            if ((curveType == VOI.CONTOUR) || (curveType == VOI.POLYLINE) || (curveType == VOI.PROTRACTOR) ||
                    (curveType == VOI.LINE) || (curveType == VOI.ANNOTATION)) {
                curves = ((VOI) (VOIs.elementAt(i))).getCurves();

                for (int j = 0; j < curves[slice].size(); j++) {
                    boolean isContained = false;

                    if ((curveType == VOI.CONTOUR) || (curveType == VOI.POLYLINE)) {
                        isContained = ((VOIContour) (curves[slice].elementAt(j))).contains(xS, yS, true);
                    } else if (curveType == VOI.PROTRACTOR) {
                        isContained = ((VOIProtractor) (curves[slice].elementAt(j))).contains(xS, yS, true);
                        ((VOIProtractor) (curves[slice].elementAt(j))).setShowLength(true);
                    } else if (curveType == VOI.LINE) {
                        isContained = ((VOILine) (curves[slice].elementAt(j))).contains(xS, yS, true);
                    } else if (curveType == VOI.ANNOTATION) {
                        isContained = ((VOIText) (curves[slice].elementAt(j))).contains(xS, yS, getZoomX(), getZoomY(),
                                                                                        imageActive.getResolutions(0),
                                                                                        g);
                    }
                }
            }
        }

        setCursorMode(DEFAULT);
        // setCursor(crosshairCursor);
    }

    /**
     * A mouse event. Sets the mode of the program depending on the cursor mode. If the mode is move, activates the
     * contour or line and enables the delete button.
     *
     * @param  mouseEvent  event that triggered function
     */
    public void mousePressed(MouseEvent mouseEvent) {
        float[] position;
        float[] intensity;
        ViewJFrameGraph lineGraph;
        int i, j, m;
        int nVOI;
        ViewVOIVector VOIs;
        Graphics g = getGraphics();

        if (modifyFlag == false) {
            return;
        }

        // save the state of the shift button
        mousePressIsShiftDown = mouseEvent.isShiftDown();

        if (cursorMode != DEFAULT) {
            VOIs = imageActive.getVOIs();
            nVOI = VOIs.size();

            // if (nVOI == 0) return;
            for (i = 0; i < nVOI; i++) {

                if (VOIs.VOIAt(i).isActive() == true) {

                    // System.out.println("Start of mouse pressed: Coping VOI to clip board for Undo");
                    getVOIHandler().copyVOIforUndo();

                    break;
                }
            }
        }
        // System.out.println("Mouse pressed - mode : " + mode);

        int xS = (int) (getScaledX(mouseEvent.getX()) + 0.5f); // zoomed x.  Used as cursor
        int yS = (int) (getScaledY(mouseEvent.getY()) + 0.5f); // zoomed y.  Used as cursor

        float[] lineX = new float[2];
        float[] lineY = new float[2];
        float[] lineZ = new float[2];

        try {
            mousePressedPaint(mouseEvent);

            if (mouseEvent.getModifiers() == MouseEvent.BUTTON3_MASK) {
                VOIs = (ViewVOIVector) imageActive.getVOIs();
                nVOI = VOIs.size();

                for (i = 0; i < nVOI; i++) {

                    if (VOIs.VOIAt(i).isActive() && (VOIs.VOIAt(i).isVisible() == true)) {

                        if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {

                            for (j = 0; j < VOIs.VOIAt(i).getCurves()[slice].size(); j++) {

                                if (((VOIContour) (VOIs.VOIAt(i).getCurves()[slice].elementAt(j))).contains(xS, yS,
                                                                                                                true)) {
                                    return;
                                }
                            }
                        }

                        if (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE) {
                            // addMouseListener( popup );
                        } else if (VOIs.VOIAt(i).getCurveType() == VOI.LINE) {

                            for (j = 0; j < VOIs.VOIAt(i).getCurves()[slice].size(); j++) {

                                if (((VOILine) (VOIs.VOIAt(i).getCurves()[slice].elementAt(j))).nearLine(xS, yS)) {
                                    int length;
                                    float[][] rgbPositions;
                                    float[][] rgbIntensities;

                                    float[][] rgbPos = null;
                                    float[][] rgbInten = null;

                                    VOIs.VOIAt(i).exportArrays(lineX, lineY, lineZ, slice, j);

                                    if (imageActive.isColorImage() == true) {

                                        length = (int)
                                                     (Math.sqrt(((lineX[1] - lineX[0]) * (lineX[1] - lineX[0])) +
                                                                ((lineY[1] - lineY[0]) * (lineY[1] - lineY[0]))));
                                        rgbPositions = new float[3][(length * 2) + 1];
                                        rgbIntensities = new float[3][(length * 2) + 1];

                                        for (int c = 0; c < 3; c++) {
                                            int pt = ((VOILine) (VOIs.VOIAt(i).getCurves()[slice].elementAt(j)))
                                                         .findPositionAndIntensityRGB(rgbPositions[c],
                                                                                          rgbIntensities[c], c,
                                                                                          getActiveImageBuffer(),
                                                                                          imageActive.getResolutions(slice),
                                                                                          getActiveImage().getExtents()[0],
                                                                                          getActiveImage().getExtents()[1]);

                                            if (c == 0) {
                                                rgbPos = new float[3][pt];
                                                rgbInten = new float[3][pt];
                                            }

                                            for (m = 0; m < pt; m++) {
                                                rgbPos[c][m] = rgbPositions[c][m];
                                                rgbInten[c][m] = rgbIntensities[c][m];
                                            }
                                        }

                                        if (VOIs.VOIAt(i).getContourGraph() == null) {
                                            ViewJFrameGraph contourGraph = new ViewJFrameGraph(rgbPos, rgbInten,
                                                                                               "Intensity Graph",
                                                                                               VOIs.VOIAt(i),
                                                                                               FileInfoBase.getUnitsOfMeasureAbbrevStr(imageActive.getUnitsOfMeasure(0)));

                                            contourGraph.setDefaultDirectory(ViewUserInterface.getReference().getDefaultDirectory());
                                            contourGraph.setVisible(true);
                                            VOIs.VOIAt(i).setContourGraph(contourGraph);
                                            contourGraph.setVOI(VOIs.VOIAt(i));
                                        } else {
                                            VOIs.VOIAt(i).getContourGraph().setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(imageActive.getUnitsOfMeasure(0)));
                                            VOIs.VOIAt(i).getContourGraph().saveNewFunction(rgbPos, rgbInten, j);
                                        }

                                        return;
                                    } else {

                                        length = (int)
                                                     (Math.sqrt(((lineX[1] - lineX[0]) * (lineX[1] - lineX[0])) +
                                                                ((lineY[1] - lineY[0]) * (lineY[1] - lineY[0]))));
                                        position = new float[(length * 2) + 1];
                                        intensity = new float[(length * 2) + 1];

                                        int pt = VOIs.VOIAt(i).findPositionAndIntensity(slice, j, position, intensity,
                                                                                        imageBufferActive,
                                                                                        imageActive.getResolutions(slice),
                                                                                        imageActive.getExtents()[0],
                                                                                        imageActive.getExtents()[1]);
                                        float[] pos = new float[pt];
                                        float[] inten = new float[pt];

                                        for (m = 0; m < pt; m++) {
                                            pos[m] = position[m];
                                            inten[m] = intensity[m];
                                        }

                                        if (VOIs.VOIAt(i).getContourGraph() == null) {
                                            lineGraph = new ViewJFrameGraph(pos, inten, "Line VOI Graph", VOIs.VOIAt(i),
                                                                            FileInfoBase.getUnitsOfMeasureAbbrevStr(imageActive.getUnitsOfMeasure(0)));
                                            lineGraph.setDefaultDirectory(ViewUserInterface.getReference().getDefaultDirectory());
                                            lineGraph.setVisible(true);
                                            VOIs.VOIAt(i).setContourGraph(lineGraph);
                                            lineGraph.setVOI(VOIs.VOIAt(i));
                                        } else {
                                            VOIs.VOIAt(i).getContourGraph().setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(imageActive.getUnitsOfMeasure(0)));
                                            VOIs.VOIAt(i).getContourGraph().replaceFunction(pos, inten, VOIs.VOIAt(i),
                                                                                            j);
                                        }

                                        // update...*/
                                        return;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentEditImage.mousePressed");
            setCursorMode(DEFAULT);

            return;
        }

        if (cursorMode == MOVE) {
            voiHandler.getAnchorPt().setLocation(xS, yS); // For use in dragging VOIs

            // the actual selecting was moved to mouseReleased()
        }
    }

    /**
     * A mouse event. This function sets up and draws the VOI according to the mode.
     *
     * @param  mouseEvent  event that triggered function
     */
    public void mouseReleased(MouseEvent mouseEvent) {
        int i, j, k;
        int xR, yR;
        int nVOI;
        int xS, yS, xDim, yDim;
        ViewVOIVector VOIs = imageActive.getVOIs();
        Graphics g = getGraphics();

        if (modifyFlag == false) {
            return;
        }

        // Removed -0.5f to be consistent with mouseDragged
        xS = Math.round(mouseEvent.getX() / (getZoomX() * resolutionX) /* - 0.5f*/); // zoomed x.  Used as cursor
        yS = Math.round(mouseEvent.getY() / (getZoomY() * resolutionY) /* - 0.5f*/); // zoomed y.  Used as cursor
        xR = mouseEvent.getX();
        yR = mouseEvent.getY();

        xDim = imageActive.getExtents()[0];
        yDim = imageActive.getExtents()[1];

        if ((xS < 0) || (xS >= imageActive.getExtents()[0]) || (yS < 0) || (yS >= imageActive.getExtents()[1])) {
            return;
        }

        setPixelInformationAtLocation(xS, yS);

        if ((mouseEvent.getModifiers() & InputEvent.BUTTON2_MASK) != 0) {
            ViewJFrameTriImage triFrame = imageActive.getTriImageFrame();

            if (triFrame != null) {
                int xx = Math.round(mouseEvent.getX() / (getZoomX() * resolutionX));
                int yy = Math.round(mouseEvent.getY() / (getZoomY() * resolutionY));

                // System.out.println(" x = " + xx + " y = " + yy + " z = " + slice);
                triFrame.setSlicesFromFrame(xx, yy, slice);
            }
        }

        VOIs = imageActive.getVOIs(); // Get the VOIs from the active image.
        nVOI = 0;

        if (VOIs != null) {
            nVOI = VOIs.size();

            // only handle ONE voi (VOICardiology type)
            if ((nVOI != 1) || (VOIs.VOIAt(0).getCurveType() != VOI.CARDIOLOGY)) {
                return;
            }

            if ((mouseEvent.getModifiers() & InputEvent.BUTTON1_MASK) != 0) {

                if (cursorMode == NEW_POINT) {
                    ((VOICardiology) (VOIs.VOIAt(0).getCurves()[slice].elementAt(0))).insertElement(xS, yS, slice);
                    imageActive.notifyImageDisplayListeners(null, true);
                    g.dispose();
                    setCursorMode(DEFAULT);

                    return;
                } else if (cursorMode == DELETE_POINT) {
                    ((VOICardiology) (VOIs.VOIAt(0).getCurves()[slice].elementAt(0))).removeElement();

                    imageActive.notifyImageDisplayListeners();
                    setCursorMode(MOVE_POINT);

                    return;

                } else if (cursorMode == MOVE) {
                    int section = ((VOICardiology) VOIs.VOIAt(0).getCurves()[0].elementAt(0)).getSection(xS, yS);

                    if (section >= 0) {
                        boolean isActive = ((VOICardiology) VOIs.VOIAt(0).getCurves()[0].elementAt(0)).isActive(section);
                        ((VOICardiology) VOIs.VOIAt(0).getCurves()[0].elementAt(0)).setActive(section, !isActive);
                        imageActive.notifyImageDisplayListeners();
                    }

                    return;
                }
            }
        }


        if (cursorMode == SELECT) { // paintComponent(getGraphics());

            // setMode(DEFAULT);
        } else if (cursorMode == POINT_VOI) {

            if ((mouseEvent.getModifiers() & InputEvent.BUTTON1_MASK) != 0) {

                if (voiHandler.isNewVoiNeeded(VOI.POINT)) { // create new VOI

                    VOI newPtVOI = null;

                    try {
                        float[] x = new float[1];
                        float[] y = new float[1];
                        float[] z = new float[1];


                        voiHandler.setVOI_ID(imageActive.getVOIs().size());

                        int colorID = 0;

                        if (imageActive.getVOIs().size() > 0) {
                            colorID = ((VOI) (imageActive.getVOIs().lastElement())).getID() + 1;
                        }

                        if (imageActive.getNDims() > 2) {

                            newPtVOI = new VOI((short) colorID, "point3D.voi", imageActive.getExtents()[2], VOI.POINT,
                                               -1.0f);
                        } else {
                            newPtVOI = new VOI((short) colorID, "point2d.voi", 1, VOI.POINT, -1.0f);
                        }

                        x[0] = xS;
                        y[0] = yS;
                        z[0] = slice;
                        newPtVOI.importCurve(x, y, z, slice);
                        newPtVOI.setUID(newPtVOI.hashCode());

                    } catch (OutOfMemoryError error) {
                        System.gc();
                        MipavUtil.displayError("Out of memory: ComponentEditImage.mouseReleased");
                        setCursorMode(DEFAULT);

                        return;
                    }

                    // voiHandler.setLastPointVOI_ID(voiID);
                    imageActive.registerVOI(newPtVOI);
                    newPtVOI.setActive(true);

                    getVOIHandler().updateVOIColor(newPtVOI.getColor(), newPtVOI.getUID());
                    ((VOIPoint) (VOIs.VOIAt(voiHandler.getVOI_ID()).getCurves()[slice].elementAt(0))).setActive(true);

                    imageActive.notifyImageDisplayListeners();

                    voiHandler.graphPointVOI(newPtVOI,
                                             ((VOIPoint)
                                              (VOIs.VOIAt(voiHandler.getVOI_ID()).getCurves()[slice].elementAt(0))), 0);

                    if (mouseEvent.isShiftDown() != true) {
                        setCursorMode(DEFAULT);
                    }

                } // end of if (voiID == -1)
                else { // voiID != -1 add point to existing VOI

                    // System.err.println("Adding to existing voi");

                    int index;

                    nVOI = VOIs.size();

                    float[] x = new float[1];
                    float[] y = new float[1];
                    float[] z = new float[1];

                    x[0] = xS;
                    y[0] = yS;
                    z[0] = slice;

                    for (i = 0; i < nVOI; i++) {

                        if (VOIs.VOIAt(i).getID() == voiHandler.getVOI_ID()) {

                            if (VOIs.VOIAt(i).getCurveType() == VOI.POINT) {
                                VOIs.VOIAt(i).importCurve(x, y, z, slice);

                                break;
                            } else {
                                MipavUtil.displayError("Can't add Point VOI to other VOI structure.");

                                return;
                            }
                        }
                    }

                    int end;

                    if (imageActive.getNDims() >= 3) {
                        end = imageActive.getExtents()[2];
                    } else {
                        end = 1;
                    }

                    for (j = 0; j < end; j++) {
                        index = VOIs.VOIAt(i).getCurves()[j].size();

                        for (k = 0; k < index; k++) {
                            ((VOIPoint) (VOIs.VOIAt(i).getCurves()[j].elementAt(k))).setActive(false);
                        }
                    }

                    index = VOIs.VOIAt(i).getCurves()[slice].size();
                    ((VOIPoint) (VOIs.VOIAt(i).getCurves()[slice].elementAt(index - 1))).setActive(true);

                    imageActive.notifyImageDisplayListeners();

                    if (!((VOIs.VOIAt(i).getContourGraph() != null) && (imageActive.isColorImage() == true))) {
                        voiHandler.graphPointVOI(VOIs.VOIAt(i),
                                                 ((VOIPoint) (VOIs.VOIAt(i).getCurves()[slice].elementAt(index - 1))),
                                                 index - 1);
                    }

                    if (mouseEvent.isShiftDown() != true) {
                        setCursorMode(DEFAULT);
                    }

                    return;
                } // end of else for if voiID != -1 add point to existing VOI
            } // end of if ((mouseEvent.getModifiers() & mouseEvent.BUTTON1_MASK) != 0)
        } // end of else if (mode == POINT_VOI)
        else if (cursorMode == ANNOTATION) {

            if ((mouseEvent.getModifiers() & InputEvent.BUTTON1_MASK) != 0) {

                VOI newTextVOI = null;

                int colorID = 0;

                if (imageActive.getVOIs().size() > 0) {
                    colorID = ((VOI) (imageActive.getVOIs().lastElement())).getID() + 1;
                }

                if (imageActive.getNDims() > 2) {
                    newTextVOI = new VOI((short) colorID, "annotation3d.voi", imageActive.getExtents()[2],
                                         VOI.ANNOTATION, -1.0f);
                } else {
                    newTextVOI = new VOI((short) colorID, "annotation2d.voi", 1, VOI.ANNOTATION, -1.0f);
                }

                float[] x = new float[1];
                float[] y = new float[1];
                float[] z = new float[1];

                voiHandler.setVOI_ID(imageActive.getVOIs().size());

                int sliceNum = 0;

                if (imageActive.getNDims() > 2) {
                    sliceNum = slice;
                }

                x[0] = xS;
                y[0] = yS;
                z[0] = sliceNum;

                newTextVOI.importCurve(x, y, z, sliceNum);
                newTextVOI.setUID(newTextVOI.hashCode());
                newTextVOI.setColor(Color.WHITE);

                // pop up a dialog that allows text input, color, and font formatting
                new JDialogAnnotation(imageActive, newTextVOI, slice, false);

                if (mouseEvent.isShiftDown() != true) {
                    setCursorMode(DEFAULT);
                }

            } // end of if ((mouseEvent.getModifiers() & mouseEvent.BUTTON1_MASK) != 0)

        } else if ((cursorMode == POLYLINE) || (cursorMode == LIVEWIRE)) {
            return;
            // setMode(DEFAULT);
        } else if (cursorMode == LEVELSET) { }
        else if (cursorMode == RECTANGLE) { }
        else if (cursorMode == RECTANGLE3D) { }
        else if (cursorMode == ELLIPSE) { }
        else if (cursorMode == LINE) { }
        else if (cursorMode == PROTRACTOR) { }
        else if (cursorMode == NEW_POINT) { // impossible for LINE

            if (mouseEvent.getModifiers() == MouseEvent.BUTTON1_MASK) {
                nVOI = VOIs.size();

                for (i = 0; i < nVOI; i++) {

                    if (VOIs.VOIAt(i).isActive()) {
                        break;
                    }
                }

                if (i == nVOI) {
                    return;
                }

                int index = VOIs.VOIAt(i).getActiveContourIndex(slice);

                ((VOIContour) (VOIs.VOIAt(i).getCurves()[slice].elementAt(index))).insertElement(xS, yS, slice);
                imageActive.notifyImageDisplayListeners(null, true);

                if (VOIs.VOIAt(i).isVisible()) {

                    if (((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) ||
                             (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) && (mouseEvent.isControlDown() == false) &&
                            (mouseEvent.getModifiers() != MouseEvent.BUTTON3_MASK)) {

                        if ((VOIs.VOIAt(i).getContourGraph() != null) && VOIs.VOIAt(i).getContourGraph().isVisible()) {
                            VOI v;
                            float intensitySum;
                            int length = imageActive.getSliceSize();
                            int s;
                            int numPixels;
                            boolean foundCurve;
                            float[] position = VOIs.VOIAt(i).getPosition();
                            float[] intensity = VOIs.VOIAt(i).getIntensity();
                            float[][] rgbPositions = VOIs.VOIAt(i).getRGBPositions();
                            float[][] rgbIntensities = VOIs.VOIAt(i).getRGBIntensities();

                            if (imageActive.getNDims() == 3) {

                                if (imageActive.isColorImage() == true) {

                                    try {
                                        v = VOIs.VOIAt(i);

                                        for (s = 0, foundCurve = false; s < imageActive.getExtents()[2]; s++) {

                                            try {

                                                for (int c = 0; c < 3; c++) {
                                                    numPixels = 0;

                                                    for (j = 0, intensitySum = 0;
                                                             j < VOIs.VOIAt(i).getCurves()[s].size(); j++) {

                                                        if (((VOIContour) VOIs.VOIAt(i).getCurves()[s].elementAt(j))
                                                                .isActive() || foundCurve) {

                                                            if (!foundCurve) {
                                                                imageActive.exportData(s * length * 4, length * 4,
                                                                                       voiHandler.getImageGraphBuffer());
                                                            } // locks and releases lock

                                                            intensitySum += ((VOIContour)
                                                                                 (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                                .calcRGBIntensity(voiHandler.getImageGraphBuffer(),
                                                                                                      imageActive.getExtents()[0],
                                                                                                      c);
                                                            numPixels += ((VOIContour)
                                                                              (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                             .getLastNumPixels();
                                                            foundCurve = true;
                                                        }
                                                    }

                                                    if (foundCurve) {
                                                        rgbPositions[c][s] = s;

                                                        if (v.getTotalIntensity() || (numPixels == 0)) {
                                                            rgbIntensities[c][s] = intensitySum;
                                                        } else {
                                                            rgbIntensities[c][s] = intensitySum / numPixels;
                                                        }
                                                    }
                                                }
                                            } catch (IOException error) {
                                                MipavUtil.displayError("Image(s) locked");

                                                return;
                                            }

                                            foundCurve = false;
                                        }

                                        VOIs.VOIAt(i).getContourGraph().update(rgbPositions, rgbIntensities, 0);
                                        VOIs.VOIAt(i).getContourGraph().setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(imageActive.getUnitsOfMeasure(0)));

                                    } catch (OutOfMemoryError error) {
                                        System.gc();
                                        MipavUtil.displayError("Out of memory: ComponentEditImage.graphVOI");

                                        return;
                                    }
                                } else {

                                    try {
                                        v = VOIs.VOIAt(i);

                                        for (s = 0, foundCurve = false; s < imageActive.getExtents()[2]; s++) {

                                            try {
                                                numPixels = 0;

                                                for (j = 0, intensitySum = 0; j < VOIs.VOIAt(i).getCurves()[s].size();
                                                         j++) {

                                                    if (((VOIContour) VOIs.VOIAt(i).getCurves()[s].elementAt(j))
                                                            .isActive() || foundCurve) {

                                                        if (!foundCurve) {
                                                            imageActive.exportData(s * length, length,
                                                                                   voiHandler.getImageGraphBuffer());
                                                        } // locks and releases lock

                                                        intensitySum += ((VOIContour)
                                                                             (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                            .calcIntensity(voiHandler.getImageGraphBuffer(),
                                                                                               imageActive.getExtents()[0]);
                                                        numPixels += ((VOIContour)
                                                                          (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                         .getLastNumPixels();
                                                        foundCurve = true;
                                                    }
                                                }

                                                if (foundCurve) {
                                                    position[s] = s;

                                                    if (v.getTotalIntensity() || (numPixels == 0)) {
                                                        intensity[s] = intensitySum;
                                                    } else {
                                                        intensity[s] = intensitySum / numPixels;
                                                    }

                                                    foundCurve = false;
                                                }
                                            } catch (IOException error) {
                                                MipavUtil.displayError("Image(s) locked");

                                                return;
                                            }
                                        }

                                        VOIs.VOIAt(i).getContourGraph().update(position, intensity, 0);
                                        VOIs.VOIAt(i).getContourGraph().setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(imageActive.getUnitsOfMeasure(0)));

                                    } catch (OutOfMemoryError error) {
                                        System.gc();
                                        MipavUtil.displayError("Out of memory: ComponentEditImage.graphVOI");

                                        return;
                                    }
                                }
                            } else if (imageActive.getNDims() == 4) {
                                int zDim = imageActive.getExtents()[2];

                                try {
                                    v = VOIs.VOIAt(i);

                                    for (int t = 0; t < imageActive.getExtents()[3]; t++) {

                                        try {
                                            numPixels = 0;

                                            for (s = 0, intensitySum = 0; s < imageActive.getExtents()[2]; s++) {
                                                imageActive.exportData((t * xDim * yDim * zDim) + (s * xDim * yDim),
                                                                       length, voiHandler.getImageGraphBuffer()); // locks and releases lock

                                                for (j = 0; j < VOIs.VOIAt(i).getCurves()[s].size(); j++) {
                                                    intensitySum += ((VOIContour)
                                                                         (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                        .calcIntensity(voiHandler.getImageGraphBuffer(),
                                                                                           imageActive.getExtents()[0]);
                                                    numPixels += ((VOIContour)
                                                                      (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                     .getLastNumPixels();

                                                }
                                            }

                                            position[t] = t;

                                            if (v.getTotalIntensity() || (numPixels == 0)) {
                                                intensity[t] = intensitySum;
                                            } else {
                                                intensity[t] = intensitySum / numPixels;
                                            }
                                        } catch (IOException error) {
                                            MipavUtil.displayError("Image(s) locked");

                                            return;
                                        }
                                    }

                                    VOIs.VOIAt(i).getContourGraph().update(position, intensity, 0);
                                    VOIs.VOIAt(i).getContourGraph().setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(imageActive.getUnitsOfMeasure(0)));
                                } catch (OutOfMemoryError error) {
                                    System.gc();
                                    MipavUtil.displayError("Out of memory: ComponentEditImage.graphVOI");

                                    return;
                                }
                            }
                        }
                    }
                }
            }
        } else if (cursorMode == DELETE_POINT) { // impossible for LINE
            nVOI = VOIs.size();

            for (i = 0; i < nVOI; i++) {

                if (VOIs.VOIAt(i).isActive()) {
                    break;
                }
            }

            if (i == nVOI) {
                return;
            }

            int index = VOIs.VOIAt(i).getActiveContourIndex(slice);

            ((VOIContour) (VOIs.VOIAt(i).getCurves()[slice].elementAt(index))).removeElement();

            imageActive.notifyImageDisplayListeners();
            setCursorMode(MOVE_POINT);
        } else if (cursorMode == PAINT_CAN) {
            xPG = (short) xS;
            yPG = (short) yS;
            zPG = (short) slice;
            seedVal = imageBufferActive[(yS * imageActive.getExtents()[0]) + xS];
            regionGrow((short) xS, (short) yS, (short) slice, seedVal, null, true);
            imageActive.notifyImageDisplayListeners(null, true);

        } else if (cursorMode == PAINT_VASC) {
            int index = xS + (yS * imageActive.getExtents()[0]);
            int z = Math.round(((ViewJFramePaintVasculature) frame).getMIPZValue(index));
            float value = ((ViewJFramePaintVasculature) frame).imageBuffer[index + (z * imageActive.getSliceSize())];

            ((ViewJFrameImage) ((ViewJFramePaintVasculature) frame).parent).getComponentImage().regionGrow((short) xS,
                                                                                                           (short) yS,
                                                                                                           (short) z,
                                                                                                           value, null,
                                                                                                           true);
            ((ViewJFrameImage) ((ViewJFramePaintVasculature) frame).parent).getComponentImage().setRegionGrowVars((short)
                                                                                                                  xS,
                                                                                                                  (short)
                                                                                                                  yS,
                                                                                                                  (short)
                                                                                                                  z,
                                                                                                                  value);
            imageActive.notifyImageDisplayListeners(null, true);
        } else if (cursorMode == MOVE) {
            nVOI = VOIs.size();

            for (i = 0; i < nVOI; i++) {
                VOIs.VOIAt(i).setAllActive(false); // deactivate all other VOIs
            }

            for (i = 0; i < nVOI; i++) {
                VOIBase selectedCurve = null;

                for (j = 0; j < VOIs.VOIAt(i).getCurves()[slice].size(); j++) {

                    // get the curve referenced by the VOI.  We'll check it.
                    selectedCurve = ((VOIBase) VOIs.VOIAt(i).getCurves()[slice].elementAt(j));

                    if ((selectedCurve instanceof VOIPoint) &&
                            ((VOIPoint) selectedCurve).nearPoint(xR, yR, getZoomX(), resolutionX, resolutionY)) {

                        // points are not true curves, but we want to check if we
                        // released mouse over it. we'll at least set the point active.
                        if (mouseEvent.isShiftDown()) {
                            allActive = true;

                            // System.err.println("Got a shift down");
                            // if true set all points in VOI active - move all points
                            VOIs.VOIAt(i).setAllActive(true);
                            getVOIHandler().updateVOIColor(VOIs.VOIAt(i).getColor(), VOIs.VOIAt(i).getUID());
                            voiHandler.setVOI_ID(VOIs.VOIAt(i).getID());

                            // and we are done with this VOI.
                            // skip the rest of the curves
                            j = VOIs.VOIAt(i).getCurves()[slice].size();
                        } else {
                            allActive = false;
                            VOIs.VOIAt(i).setActive(true);
                            getVOIHandler().updateVOIColor(VOIs.VOIAt(i).getColor(), VOIs.VOIAt(i).getUID());
                            ((VOIPoint) (selectedCurve)).setActive(true);
                            voiHandler.setVOI_ID(VOIs.VOIAt(i).getID());
                        }

                        getVOIHandler().fireVOISelectionChange(VOIs.VOIAt(i), selectedCurve);

                    } else if ((selectedCurve instanceof VOIText) &&
                                   ((VOIText) selectedCurve).contains(xS, yS, getZoomX(), getZoomY(),
                                                                          imageActive.getResolutions(0), g)) {

                        allActive = false;
                        VOIs.VOIAt(i).setActive(true);
                        getVOIHandler().updateVOIColor(VOIs.VOIAt(i).getColor(), VOIs.VOIAt(i).getUID());
                        ((VOIText) (selectedCurve)).setActive(true);
                        voiHandler.setVOI_ID(VOIs.VOIAt(i).getID());

                        // if the Text was double-clicked, bring up the editor
                        if (mouseEvent.getClickCount() == 2) {
                            new JDialogAnnotation(imageActive, VOIs.VOIAt(i), slice, true);
                        }
                    } else if (selectedCurve.contains(xS, yS, true)) {

                        // if we released the mouse over another kind of curve,
                        // we'll at least set it active.
                        if (mousePressIsShiftDown) {

                            // System.err.println("Setting all active for mousePressIsShiftDown");
                            // when shift is presed at the same time,
                            // select all the curves in the (contour) grouping.
                            allActive = true;
                            VOIs.VOIAt(i).setAllActive(true);
                            getVOIHandler().updateVOIColor(VOIs.VOIAt(i).getColor(), VOIs.VOIAt(i).getUID());
                            voiHandler.setVOI_ID(VOIs.VOIAt(i).getID());

                            // and we are done with this VOI. Skip the rest of the curves
                            j = VOIs.VOIAt(i).getCurves()[slice].size();
                        } else {
                            allActive = false;

                            // otherwise, we'll only select the one we clicked on,
                            // and we want to deactivate all other curves.
                            for (k = 0; k < nVOI; k++) {
                                VOIs.VOIAt(k).setAllActive(false); // deactivate all VOIs
                            }

                            VOIs.VOIAt(i).setActive(true); // set the current active // move single contour
                            getVOIHandler().updateVOIColor(VOIs.VOIAt(i).getColor(), VOIs.VOIAt(i).getUID());
                            selectedCurve.setActive(true); // set its courve to active (for display)
                            voiHandler.setVOI_ID(VOIs.VOIAt(i).getID());
                        }

                        getVOIHandler().fireVOISelectionChange(VOIs.VOIAt(i), selectedCurve);
                    } else { // selected curve was not selected, so set false.
                        selectedCurve.setActive(false);
                    }
                } // end of curves in this VOI
            } // end checking all VOIs in the active image

            imageActive.notifyImageDisplayListeners();
        } else if (cursorMode == MOVE_POINT) {

            nVOI = VOIs.size();

            for (i = 0; i < nVOI; i++) {

                if ((VOIs.VOIAt(i).isActive() && (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)) ||
                        (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
                    VOIs.VOIAt(i).rubberbandVOI(xS, yS, slice, xDim, yDim, true);
                } else if ((VOIs.VOIAt(i).isActive() && (VOIs.VOIAt(i).getCurveType() == VOI.LINE)) ||
                               (VOIs.VOIAt(i).getCurveType() == VOI.PROTRACTOR)) {

                    for (j = 0; j < VOIs.VOIAt(i).getCurves()[slice].size(); j++) {

                        if (((VOIBase) (VOIs.VOIAt(i).getCurves()[slice].elementAt(j))).isActive()) {

                            if (VOIs.VOIAt(i).getCurveType() == VOI.PROTRACTOR) {
                                ((VOIProtractor) (VOIs.VOIAt(i).getCurves()[slice].elementAt(j))).setShowLength(true);
                            }

                            imageActive.notifyImageDisplayListeners();
                            g.dispose();

                            return;
                        }
                    }
                }
            }

            imageActive.notifyImageDisplayListeners(null, true);
        } else if (cursorMode == RETRACE) {
            nVOI = VOIs.size();

            for (i = 0; i < nVOI; i++) {

                if (VOIs.VOIAt(i).isActive()) {
                    ((VOIContour) (VOIs.VOIAt(i).getActiveContour(slice))).trimPoints(Preferences.getTrim(), true);
                    ((VOIContour) (VOIs.VOIAt(i).getActiveContour(slice))).resetIndex();

                    break;
                }
            }

            imageActive.notifyImageDisplayListeners();
        }

        g.dispose();

        // reset mousePressIsShiftDown for next mouse click
        mousePressIsShiftDown = false;

    } // end mouseReleased()

    /**
     * DOCUMENT ME!
     *
     * @param  selectAll  DOCUMENT ME!
     */
    public void selectAllVOISections(boolean selectAll) {
        VOICardiology cardioVOI = ((VOICardiology) imageActive.getVOIs().VOIAt(0).getCurves()[0].elementAt(0));
        int numSections = cardioVOI.getNumSections();

        for (int i = 0; i < numSections; i++) {
            cardioVOI.setActive(i, selectAll);
        }

        imageActive.notifyImageDisplayListeners();
    }

    /**
     * DOCUMENT ME!
     */
    public void toggleVOIPoints() {
        ((VOICardiology) imageActive.getVOIs().VOIAt(0).getCurves()[0].elementAt(0)).togglePoints();
        imageActive.notifyImageDisplayListeners();
    }

    /**
     * Calls dispose to dump this instance.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        disposeLocal(false);
        super.finalize();
    }
}
