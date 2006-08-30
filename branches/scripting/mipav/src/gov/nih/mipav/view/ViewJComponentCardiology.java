package gov.nih.mipav.view;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;

//import static gov.nih.mipav.view.MipavUtil.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.text.*;

import java.util.*;


/**
 * Basic displayable image object in MIPAV. Contains the viewable objects such as VOIs and carries listeners to most any
 * action that can happen in a window.
 *
 * @version  0.1 Nov 18, 1997
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class ViewJComponentCardiology extends ViewJComponentEditImage
        implements MouseMotionListener, MouseListener, KeyListener, PaintGrowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7846145645892843524L;


    /** Thin paint brush, 1 pixel wide. */
    public static final int thinPaint = 0;

    /** Medium paint brush, 4 pixel wide. */
    public static final int medPaint = 1;

    /** Thick paint brush, 8 pixel wide. */
    public static final int thickPaint = 2;

    /** The thickest paint brush, 16 pixels wide. */
    public static final int thickestPaint = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** used in paint to indicate intensity painted into the image. */
    public float intensityDropper;

    /** default = 120 pixels. */
    public int MAGR_HEIGHT = 120;

    /** magnification value of magnifier window -- default = 4.0. */
    public float MAGR_MAG = 4f;

    /** default = 120 pixels. */
    public int MAGR_WIDTH = 120;

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

    /** Used to "lock" display when an algorithm is in the calculation process. */
    protected boolean modifyFlag = true;

    /** Flag to indicate if DICOM overlay should be displayed. */
    protected boolean overlayOn = false;

    /** Used with commitMask(int imagesDone), to save the value. */
    protected float saveValue;

    /** Used with commitMask(int imagesDone), to save positions. */
    protected short saveX, saveY, saveZ;


    /** DOCUMENT ME! */
    private ViewJPopupCardiologyVOI popupCard = null;

    /*protected AlgorithmVOIExtractionPaint borderDetector;
     * protected Vector paintBorderPolygons = new Vector();protected boolean paintChangeFlag = false;*/

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor: ImageA and ImageB are expected to be of the same dimensionality !!
     *
     * @param  _frame             frame where image(s) will be displayed
     * @param  _imageA            Model of the image that will be displayed
     * @param  _LUTa              LUT used to display imageA
     * @param  imgBufferA         storage buffer used to display image A
     * @param  pixelBuffer        storage buffer used to build a displayable image
     * @param  zoom               initial magnification of image
     * @param  extents            initial display dimensions of the image
     * @param  logMagDisplay      display log magnitude of image
     * @param  _orientation       orientation of the image
     */
    public ViewJComponentCardiology(ViewJFrameBase _frame, ModelImage _imageA, ModelLUT _LUTa, float[] imgBufferA,
                                    int[] pixelBuffer, float zoom, int[] extents, boolean logMagDisplay,
                                    int _orientation )
    {
        super(_frame, _imageA, _LUTa, imgBufferA, null, null, null, pixelBuffer, zoom, extents, logMagDisplay, _orientation );

        removeMouseListener(voiHandler.getPopupPt());
        removeMouseListener(voiHandler.getPopupVOI());
        voiHandler.getPopupPt().setEnabled(false);
        voiHandler.getPopupVOI().setEnabled(false);
      //  getVOIHandler().removeVOIUpdateListener(getVOIHandler().getVOIDialog());
        popupCard = new ViewJPopupCardiologyVOI(this);
    }

    /* ********************************************************************** */

    /* ****************************** Accessors ***************************** */

    /* ********************************************************************** */

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Calculates the volume of the painted voxels.
     *
     * @param  str  string in showRegionGrow {@link #showRegionGrow(int, String)}
     */
    public void calcPaintedVolume(String str) {
        int i, count = 0;
        int end = paintBitmap.size();

        for (i = 0, count = 0; i < end; i++) {

            if (paintBitmap.get(i) == true) {
                count++;
            }
        }

        showRegionInfo(count, str);
    }

    /**
     * DOCUMENT ME!
     */
    public void calculateVOIIntensities() {
        VOICardiology cardioVOI = ((VOICardiology) imageActive.getVOIs().VOIAt(0).getCurves()[0].elementAt(0));
        boolean[] activeArray = cardioVOI.getActive();
        float[] avgIntensities = new float[2];

        int sliceSize = imageActive.getSliceSize();

        float [] imageGraphBuffer = new float[sliceSize];


        try {
            imageActive.exportData(0, sliceSize, imageGraphBuffer);
        } catch (Exception ex) {
            System.err.println("exception, woohoo");
        }

        for (int i = 0; i < activeArray.length; i++) {

            if (activeArray[i]) {
                avgIntensities = cardioVOI.calcAverageIntensity(imageGraphBuffer, i, imageActive.getExtents()[0],
                                                                imageActive.getUserInterface());

                System.err.println(i + " entire section: " + avgIntensities[0] + ", infarction: " + avgIntensities[1]);

                //        System.err.println("i: " + cardioVOI.calcAverageIntensity(voiHandler.getImageGraphBuffer(), i,
                // imageActive.getExtents()[0],
                // imageActive.getUserInterface()));
            }
        }
        voiHandler.setImageGraphBuffer(imageGraphBuffer);
    }

    /**
     * DOCUMENT ME!
     */
    public void calculateVOILengths() {
        System.err.println("calculating segment lengths");

        double[] averageOuterLength = null;
        double[] averageInnerLength = null;
        double[] averageInfarctionLength = null;

        VOICardiology cardioVOI = ((VOICardiology) imageActive.getVOIs().VOIAt(0).getCurves()[0].elementAt(0));

        boolean[] activeArray = cardioVOI.getActive();
        averageOuterLength = new double[activeArray.length];
        averageInnerLength = new double[activeArray.length];
        averageInfarctionLength = new double[activeArray.length];

        int numActive = 0; // counter for number of active sections

        double innerToOuter = 0f;

        // for each active section, run through and calculate lengths
        for (int i = 0; i < activeArray.length; i++) {

            if (activeArray[i]) {
                numActive++;

                cardioVOI.calcAverageLength(i, imageActive.getExtents()[0], imageActive.getExtents()[1],
                                            imageActive.getResolutions(0), imageActive.getUserInterface());

            }
        }

    }

    /**
     * Replace intensities in the image using painted mask.
     *
     * @param  imagesDone      IMAGE_A, IMAGE_B, or BseedVOTH
     * @param  clearPaintMask  if true clear paint mask
     * @param  polarity        DOCUMENT ME!
     */
    public void commitMask(int imagesDone, boolean clearPaintMask, boolean polarity) {

        float min, max;
        Color fillColor = new Color(128, 0, 0);

        AlgorithmMask maskAlgo = null;

        if (imageA.isColorImage() == true) {

            if (frame.getControls() != null) {
                fillColor = frame.getControls().getTools().getPaintColor();
            } else if (frameControls != null) {
                fillColor = frameControls.getTools().getPaintColor();
            } else {
                fillColor = new Color(128, 0, 0);
            }
        }

        if ((imagesDone == IMAGE_A) || (imagesDone == BOTH)) {

            if (imageA.isColorImage() == true) {
                maskAlgo = new AlgorithmMask(imageA, fillColor, polarity, false, 0, 100);
                maskAlgo.setRunningInSeparateThread(false);
                maskAlgo.calcInPlace25DC(paintBitmap, fillColor, timeSlice);
            } else {
                maskAlgo = new AlgorithmMask(imageA, intensityDropper, polarity, false, 0, 100);
                maskAlgo.setRunningInSeparateThread(false);
                maskAlgo.calcInPlace25D(paintBitmap, intensityDropper, timeSlice);

                if (imageA.getType() == ModelStorageBase.UBYTE) {
                    min = 0;
                    max = 255;
                } else if (imageA.getType() == ModelStorageBase.BYTE) {
                    min = -128;
                    max = 127;
                } else {
                    min = (float) imageA.getMin();
                    max = (float) imageA.getMax();
                }

                float imgMin = (float) imageA.getMin();
                float imgMax = (float) imageA.getMax();

                LUTa.resetTransferLine(min, imgMin, max, imgMax);
            }
        }

        maskAlgo = null;

        if ((imagesDone == IMAGE_B) || (imagesDone == BOTH)) {

            if (imageB.isColorImage() == true) {
                maskAlgo = new AlgorithmMask(imageB, fillColor, polarity, false, 0, 100);
                maskAlgo.setRunningInSeparateThread(false);
                maskAlgo.calcInPlace25DC(paintBitmap, fillColor, timeSlice);
            } else {
                maskAlgo = new AlgorithmMask(imageB, intensityDropper, polarity, false, 0, 100);
                maskAlgo.setRunningInSeparateThread(false);
                maskAlgo.calcInPlace25D(paintBitmap, intensityDropper, timeSlice);

                if (imageB.getType() == ModelStorageBase.UBYTE) {
                    min = 0;
                    max = 255;
                } else if (imageB.getType() == ModelStorageBase.BYTE) {
                    min = -128;
                    max = 127;
                } else {
                    min = (float) imageB.getMin();
                    max = (float) imageB.getMax();
                }

                float imgMin = (float) imageB.getMin();
                float imgMax = (float) imageB.getMax();

                LUTb.resetTransferLine(min, imgMin, max, imgMax);
            }
        }

        if (clearPaintMask == true) {
            paintBitmap.clear();
            // paintChangeFlag = true;
        }
    }

    /**
     * Sets all variables to null, disposes, and garbage collects.
     *
     * @param  flag  if true garbage collector should be called.
     */
    public void disposeLocal(boolean flag) {
        lutBufferRemapped = null;
        imageBufferA = null;
        imageBufferB = null;
        pixBuffer = null;
        pixBufferB = null;
        paintBitmap = null;
        paintBitmapBU = null;
        imageActive = null;
        imageBufferActive = null;
        frame = null;
        frameControls = null;
        imageA = null;
        imageB = null;


        if (imageStatList != null) {

            // removeVOIUpdateListener(imageStatList);
            imageStatList.dispose();
            imageStatList = null;
        }

        if (magSettings != null) {
            magSettings.dispose();
            magSettings = null;
        }

        if (growDialog != null) {

            if (growDialog instanceof JDialogBase) {
                ((JDialogBase) growDialog).dispose();
            }

            growDialog = null;
        }

        if (checkerDialog != null) {
            checkerDialog.dispose();
            checkerDialog = null;
        }

        LUTa = null;
        LUTb = null;

        if (flag == true) {
            super.disposeLocal();
        }
    }

    /**
     * Erases all paint.
     */
    public void eraseAllPaint() {
        paintBitmap.clear();

        if (seedPaintBitmap != null) {
            seedPaintBitmap.clear();
            seedVal = (float) imageActive.getMin();
            this.saveValue = seedVal;
            saveX = saveY = saveZ = xPG = yPG = -1;
        }

        if (growDialog != null) {
            growDialog.notifyPaintListeners(false, false, paintBitmap);
        } else {
            imageActive.notifyImageDisplayListeners(null, true);
        }
    }


    /**
     * Finds the number of points in the active VOI contour.
     *
     * @return  the number of points in the selected VOI
     */
    public int getNumPoints() {
        ViewVOIVector VOIs = imageActive.getVOIs();

        if (VOIs.size() == 0) {
            return 0;
        } else {
            return ((VOI) VOIs.elementAt(0)).getCurves()[0].size();
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean getOnTop() {
        return onTop;
    }

    /**
     * Gets the paint mask.
     *
     * @return  the current paint mask
     */
    public BitSet getPaintMask() {

        // System.err.println("calling getPaintMask()");
        return paintBitmap;
    }


    /**
     * Determines if a VOI is selected (active).
     *
     * @return  true if active VOI is found --- Must add to JDialog base - should return null if nothing selected else
     *          return VOI selected!!!!
     */
    public VOI getSelectedVOI() {
        int i;
        int nVOI;
        ViewVOIVector VOIs = imageActive.getVOIs();

        nVOI = VOIs.size();

        if (nVOI == 0) {
            return null;
        }

        for (i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).isActive() == true) {
                break;
            }
        }

        if (i == nVOI) {
            return null;
        } // No VOI to delete

        return VOIs.VOIAt(i);

    }

    /**
     * Determines whether to enable the showIntensity checkbox for magnification box.
     *
     * @return  whether to enable showIntensity checkbox
     */
    public boolean getShowMagIntensityEnabled() {
        Graphics g = getGraphics();

        if (g == null) {
            return false;
        }

        g.setFont(MipavUtil.font10);

        return super.getShowMagIntensityEnabled(g, MAGR_WIDTH, MAGR_HEIGHT, MAGR_MAG, imageActive.getType(),
                                                getActiveImage().getMin(), getActiveImage().getMax());

    }

    // ************************************************************************
    // **************************** Key Events *****************************
    // ************************************************************************

    /**
     * Does nothing.
     *
     * @param  e  event
     */
    public void keyPressed(KeyEvent e) { }

    /**
     * If the shift key is pressed, updates the graphics.
     *
     * @param  e  the key released event
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
     * Does nothing.cent.
     *
     * @param  e  event
     */
    public void keyTyped(KeyEvent e) { }

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
        int xS, yS;

        if (frame instanceof ViewJFrameLightBox) {

            // on a double click, or a single click from the
            // right mouse button (indicated by the META modifier)
            // .. set the current image slice
            // on a single click from the left mouse button, select
            // the individual lightbox pane
            if (mouseEvent.getClickCount() == 2) {
                ((ViewJFrameLightBox) frame).updateImageSlice(slice, timeSlice);

                return;
            } else if (mouseEvent.getClickCount() == 1) {

                // check to see if this event was consumed -- just in case
                // the double click is also showing a single click
                if (mouseEvent.isConsumed()) {
                    return;
                }

                if (mouseEvent.isMetaDown()) { // right mouse click
                    ((ViewJFrameLightBox) frame).updateImageSlice(slice, timeSlice);
                } else {
                    ((ViewJFrameLightBox) frame).updateImageSelection(slice, mouseEvent.isShiftDown());
                }

                return;
            }

        }

        // xS = Math.round( mouseEvent.getX() / (getZoomX() * resolutionX)); // zoomed x.  Used as cursor x
        // yS = Math.round( mouseEvent.getY() / (getZoomY() * resolutionY)); // zoomed y.  Used as cursor y
        xS = Math.round((mouseEvent.getX() / (getZoomX() * resolutionX)) - 0.5f); // zoomed x.  Used as cursor
        yS = Math.round((mouseEvent.getY() / (getZoomY() * resolutionY)) - 0.5f); // zoomed y.  Used as cursor

        if ((xS < 0) || (xS >= imageActive.getExtents()[0]) || // Check for validity
                (yS < 0) || (yS >= imageActive.getExtents()[1])) {
            return;
        }

        // System.out.println("Mode is " + mode);
        if ((mouseEvent.getClickCount() == 1) && (mode == DEFAULT)) {
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
        int mouseMods = mouseEvent.getModifiers();
        Graphics g = getGraphics();
        int i, j, m;
        int nVOI;
        ViewVOIVector VOIs = imageActive.getVOIs();
        FileInfoBase fileInfo;
        int xS, yS;
        int distX, distY;
        int xDim, yDim;
        int zDim = 1;
        int brushSize = 0;
        int hBrushSize = 0;
        int st;
        int offset;
        int red, green, blue, pix;
        Color dropperColor;
        float[] lineX = new float[2];
        float[] lineY = new float[2];
        float[] lineZ = new float[2];
        float[] position;
        float[] intensity;
        String str;
        int sliceNum;
        int windowChange, levelChange;

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

        // System.err.println("Dragging");

        try {

            if ((!imageActive.isColorImage()) && (mode == DEFAULT)) {

                if ((mouseEvent.getModifiers() & MouseEvent.BUTTON3_MASK) != 0) {

                    // Dragging the mouse with the right mouse button pressed
                    // increases the window when going from left to right.
                    // Dragging the mouse with the right mouse button pressed
                    // increases the level when going from down to up.
                    setCursor(MipavUtil.winLevelCursor);

                    if (!winLevelSet) {
                        winLevelSet = true;

                        if (imageActive.getType() == ModelStorageBase.UBYTE) {
                            minImageWin = 0;
                            maxImageWin = 255;
                        } else if (imageActive.getType() == ModelStorageBase.BYTE) {
                            minImageWin = -128;
                            maxImageWin = 127;
                        } else {
                            minImageWin = (float) imageActive.getMin();
                            maxImageWin = (float) imageActive.getMax();
                        }

                        minWin = Float.MAX_VALUE;
                        maxWin = -Float.MAX_VALUE;

                        for (i = 0; i < imageBufferActive.length; i++) {

                            if (imageBufferActive[i] > maxWin) {
                                maxWin = imageBufferActive[i];
                            }

                            if (imageBufferActive[i] < minWin) {
                                minWin = imageBufferActive[i];
                            }
                        }

                        // THIS IS (one of) THE CULPRIT(s).  ADJUST THE FOLLOWING TO
                        // SET THE TRANSFER FUNCTION CORRECTLY (to current state)!!
                        // Set LUT min max values;
                        xWin[0] = minImageWin;
                        yWin[0] = 255;
                        zWin[0] = 0;
                        xWin[1] = minWin;
                        yWin[1] = 255;
                        zWin[1] = 0;
                        xWin[2] = maxWin;
                        yWin[2] = 0;
                        zWin[2] = 0;
                        xWin[3] = maxImageWin;
                        yWin[3] = 0;
                        zWin[3] = 0;

                        if (imageA == imageActive) {
                            LUTa.getTransferFunction().importArrays(xWin, yWin, 4);
                            imageActive.notifyImageDisplayListeners(LUTa, false);
                        } else {
                            LUTb.getTransferFunction().importArrays(xWin, yWin, 4);
                            imageActive.notifyImageDisplayListeners(LUTb, false);
                        }

                        level = (xWin[1] + xWin[2]) / 2.0f;
                        window = xWin[2] - xWin[1];
                        oldXS = xS;
                        oldYS = yS;
                    } // if (!winLevelSet)
                    else if (winLevelSet && ((xS != oldXS) || (yS != oldYS))) {
                        windowChange = xS - oldXS;
                        window = window + (windowChange * 4 * (maxImageWin - minImageWin) / (xDim - 1));

                        if (window > (2 * (maxImageWin - minImageWin))) {
                            window = 2 * (maxImageWin - minImageWin);
                        } else if (window < 1) {
                            window = 1;
                        }

                        levelChange = oldYS - yS;
                        level = level + (levelChange * 2 * (maxImageWin - minImageWin) / (yDim - 1));

                        if (level > maxImageWin) {
                            level = maxImageWin;
                        } else if (level < minImageWin) {
                            level = minImageWin;
                        }

                        xWin[2] = level + (window / 2);

                        if (xWin[2] > maxImageWin) {
                            yWin[2] = 255.0f * (xWin[2] - maxImageWin) / window;
                            xWin[2] = maxImageWin;
                        } else {
                            yWin[2] = 0.0f;
                        }

                        xWin[1] = level - (window / 2);

                        if (xWin[1] < minImageWin) {
                            yWin[1] = 255.0f - (255.0f * (minImageWin - xWin[1]) / window);
                            xWin[1] = minImageWin;
                        } else {
                            yWin[1] = 255.0f;
                        }

                        // update the transfer function so the on-screen image
                        // (modelImage/viewJFrameImage) updates for the user
                        if (imageA == imageActive) {
                            LUTa.getTransferFunction().importArrays(xWin, yWin, 4);
                            imageActive.notifyImageDisplayListeners(LUTa, false);

                            if (imageA.getHistoLUTFrame() != null) {
                                imageA.getHistoLUTFrame().update();
                            }
                        } else {
                            LUTb.getTransferFunction().importArrays(xWin, yWin, 4);
                            imageActive.notifyImageDisplayListeners(LUTb, false);

                            if (imageB.getHistoLUTFrame() != null) {
                                imageB.getHistoLUTFrame().update();
                            }
                        }

                        oldXS = xS;
                        oldYS = yS;
                    } // else if (winLevelSet && ((xS != oldXS) || (yS != oldYS)))
                } // if ((mouseEvent.getModifiers() & MouseEvent.BUTTON3_MASK) != 0)
            } // if ((!imageActive.isColorImage()) && (mode == DEFAULT))

            if ((imageActive.getOrigin()[0] != 0) || (imageActive.getOrigin()[1] != 0) ||
                    (imageActive.getOrigin()[2] != 0)) {

                fileInfo = imageActive.getFileInfo()[slice];

                String[] values = setMouseOverlayData(fileInfo, mouseEvent, slice);

                if (values != null) {
                    str = "  X: " + String.valueOf(xS) + " Y: " + String.valueOf(yS) + "  Intensity:  " +
                          String.valueOf(imageBufferActive[(yS * imageActive.getExtents()[0]) + xS]) + " Position: " +
                          values[0] + " " + values[1] + " " + values[2];
                    frame.setMessageText(str);

                    if ((mouseEvent.getModifiers() & MouseEvent.BUTTON2_MASK) != 0) {
                        frame.getUserInterface().setDataText("\n" + str);
                    }
                } else {
                    str = "  X: " + String.valueOf(xS) + " Y: " + String.valueOf(yS) + "  Intensity:  " +
                          String.valueOf(imageBufferActive[(yS * imageActive.getExtents()[0]) + xS]);
                    frame.setMessageText(str);

                    if ((mouseEvent.getModifiers() & MouseEvent.BUTTON2_MASK) != 0) {
                        frame.getUserInterface().setDataText("\n" + str);
                    }
                }
            } else {

                if (imageActive.isColorImage() == true) {
                    str = "  X: " + String.valueOf(xS) + " Y: " + String.valueOf(yS) + "  R:  " +
                          String.valueOf(imageBufferActive[(4 * ((yS * imageActive.getExtents()[0]) + xS)) + 1]) +
                          "  G:  " +
                          String.valueOf(imageBufferActive[(4 * ((yS * imageActive.getExtents()[0]) + xS)) + 2]) +
                          "  B:  " +
                          String.valueOf(imageBufferActive[(4 * ((yS * imageActive.getExtents()[0]) + xS)) + 3]);
                    frame.setMessageText(str);

                    if ((mouseEvent.getModifiers() & MouseEvent.BUTTON2_MASK) != 0) {
                        frame.getUserInterface().setDataText("\n" + str);
                    }
                } else {
                    str = "  X: " + String.valueOf(xS) + " Y: " + String.valueOf(yS) + "  Intensity:  " +
                          String.valueOf(imageBufferActive[(yS * imageActive.getExtents()[0]) + xS]);
                    frame.setMessageText(str);

                    if ((mouseEvent.getModifiers() & MouseEvent.BUTTON2_MASK) != 0) {
                        frame.getUserInterface().setDataText("\n" + str);
                    }
                }
            }
        } catch (ArrayIndexOutOfBoundsException error) {
            str = "  X: " + String.valueOf(xS) + " Y: " + String.valueOf(yS);
            frame.setMessageText(str);

            if ((mouseEvent.getModifiers() & MouseEvent.BUTTON2_MASK) != 0) {
                frame.getUserInterface().setDataText("\n" + str);
            }
        }

        distX = xS - voiHandler.getAnchorPt().x; // distance from original to cursor
        distY = yS - voiHandler.getAnchorPt().y;

        int end = 1;

        nVOI = VOIs.size();

        if ((mode == MOVE_INTERSECTION_POINT) || (mode == MOVE_POINT)) {

            if ((nVOI == 1) && (VOIs.VOIAt(0).getCurveType() == VOI.CARDIOLOGY)) {
                xS = Math.round((mouseEvent.getX() / (getZoomX() * resolutionX)));
                yS = Math.round((mouseEvent.getY() / (getZoomY() * resolutionY)));
                setCursor(MipavUtil.blankCursor);

                VOIs.VOIAt(0).rubberbandVOI(xS, yS, slice, xDim, yDim, false);
                imageActive.notifyImageDisplayListeners(null, true);
            }

            return;

        }

        g.dispose();
    }

    /**
     * Unchanged.
     *
     * @param  mouseEvent  event
     */
    public void mouseEntered(MouseEvent mouseEvent) { /* if
                                                       * (System.getProperties().getProperty("os.name").equals("MacOS"))
                                                       * {  //??????
                                                       * frame.setVisible(true); frame.windowActivated(fnull);}*/
    }

    /**
     * Resets the level set stack.
     *
     * @param  mouseEvent  event that triggered function
     */
    public void mouseExited(MouseEvent mouseEvent) {

        if ((mode == MAG_REGION) || (mode == PAINT_VOI) || (mode == ERASER_PAINT)) {

            // repaint();
            paintComponent(getGraphics());
        }
    }

    /**
     * A mouse event. If the mode is level set, draws level sets as user moves mouse. Otherwise, changes the cursor
     * depending on where the mouse is in relation to the VOI.
     *
     * @param  mouseEvent  event that triggered the function
     */
    public void mouseMoved(MouseEvent mouseEvent) {
        int i, j;
        int x, y;
        int xS, yS;
        int nVOI;
        ViewVOIVector VOIs;
        int nCurves;
        Vector[] curves;
        Graphics g = getGraphics();

        removeMouseListener(popupCard);

        if ((g == null) || (modifyFlag == false) || (slice == -99)) {
            return;
        }

        if ((pixBuffer == null) || (imageBufferActive == null)) {
            g.dispose();

            return;
        }

        xS = Math.round((mouseEvent.getX() / (getZoomX() * resolutionX)) - 0.5f); // zoomed x.  Used as cursor
        yS = Math.round((mouseEvent.getY() / (getZoomY() * resolutionY)) - 0.5f); // zoomed y.  Used as cursor

        x = mouseEvent.getX();
        y = mouseEvent.getY();

        if ((xS < 0) || (xS >= imageActive.getExtents()[0]) || // Check to ensure point is within
                (yS < 0) || (yS >= imageActive.getExtents()[1])) { // the image bounds
            g.dispose();

            return;
        }

        if ((mode == RECTANGLE) || (mode == ELLIPSE) || (mode == LINE) || (mode == RECTANGLE3D) ||
                (mode == POINT_VOI) || (mode == POLYLINE) || (mode == LEVELSET) || (mode == PAINT_VOI) ||
                (mode == DROPPER_PAINT) || (mode == ERASER_PAINT) || (mode == QUICK_LUT) || (mode == PROTRACTOR) ||
                (mode == LIVEWIRE) || (mode == ANNOTATION)) {
            g.dispose();

            return;
        }

        VOIs = imageActive.getVOIs(); // Get the VOIs from the active image.
        nVOI = 0;

        if (VOIs != null) {
            nVOI = VOIs.size();

            // only handle ONE voi (VOICardiology type)
            if ((nVOI != 1) || (VOIs.VOIAt(0).getCurveType() != VOI.CARDIOLOGY)) {
                return;
            }

            int index = VOIs.VOIAt(0).nearCardioPoint(x, y, slice, getZoomX(), resolutionX, resolutionY);

            if (index >= 0) {
                setMode(MOVE_INTERSECTION_POINT);
                g.dispose();

                return;
            } else if (index == VOICardiology.BOTH) {
                setMode(MOVE_INTERSECTION_POINT);
                g.dispose();

                return;
            } else if (index == -1) {

                if (mouseEvent.isShiftDown()) {
                    setMode(DELETE_POINT);
                } else {
                    setMode(MOVE_POINT);
                }

                g.dispose();

                return;
            } else if (VOIs.VOIAt(0).nearCardioLine(xS, yS, slice)) {
                setMode(NEW_POINT);
                g.dispose();

                return;
            } else {

                // System.err.println("doing section check");
                int section = ((VOICardiology) VOIs.VOIAt(0).getCurves()[0].elementAt(0)).getSection(xS, yS);

                if (section >= 0) {

                    // System.err.println("Inside section: " + section);
                    setMode(MOVE);
                    addMouseListener(popupCard);
                    g.dispose();

                    return;
                }
            }


        }

        //
        for (i = 0; i < nVOI; i++) {
            int curveType = VOIs.VOIAt(i).getCurveType();

            if ((curveType == VOI.CONTOUR) || (curveType == VOI.POLYLINE) || (curveType == VOI.PROTRACTOR) ||
                    (curveType == VOI.LINE) || (curveType == VOI.ANNOTATION)) {
                curves = ((VOI) (VOIs.elementAt(i))).getCurves();

                for (j = 0; j < curves[slice].size(); j++) {
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

        setMode(DEFAULT);
        // setCursor(crosshairCursor);
    }

    /**
     * A mouse event. Sets the mode of the program depending on the cursor mode. If the mode is move, activates the
     * contour or line and enables the delete button.
     *
     * @param  mouseEvent  event that triggered function
     */
    public void mousePressed(MouseEvent mouseEvent) {
        int xS, yS;
        int x, y;
        Color dropperColor;
        float[] lineX = null;
        float[] lineY = null;
        float[] lineZ = null;
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

        if (mode != DEFAULT) {
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

        try {
            lineX = new float[2];
            lineY = new float[2];
            lineZ = new float[2];

            xS = Math.round((mouseEvent.getX() / (getZoomX() * resolutionX)) - 0.5f); // zoomed x.  Used as cursor
            yS = Math.round((mouseEvent.getY() / (getZoomY() * resolutionY)) - 0.5f); // zoomed y.  Used as cursor

            x = mouseEvent.getX();
            y = mouseEvent.getY();

            if ((xS < 0) || (xS >= imageActive.getExtents()[0]) || (yS < 0) || (yS >= imageActive.getExtents()[1])) {
                return;
            }

            if (mode == DROPPER_PAINT) {

                if (imageActive.isColorImage() == true) {
                    dropperColor = new Color((int)
                                                 imageBufferActive[(4 * ((yS * imageActive.getExtents()[0]) + xS)) + 1],
                                             (int)
                                                 imageBufferActive[(4 * ((yS * imageActive.getExtents()[0]) + xS)) + 2],
                                             (int)
                                                 imageBufferActive[(4 * ((yS * imageActive.getExtents()[0]) + xS)) + 3]);
                    frame.getControls().getTools().setPaintColor(dropperColor);
                } else {
                    intensityDropper = imageBufferActive[(yS * imageActive.getExtents()[0]) + xS];
                    frame.getControls().getTools().setIntensityPaintName(String.valueOf((int) (intensityDropper)));
                }
            }

            if (mode == ERASER_PAINT) {
                int brushSize = 0;
                int hBrushSize = 0;

                if (paintBrushSize == 0) {
                    brushSize = 1;
                    hBrushSize = 1 / 2;
                } else if (paintBrushSize == 1) {
                    brushSize = 4;
                    hBrushSize = 4 / 2;
                } else if (paintBrushSize == 2) {
                    brushSize = 8;
                    hBrushSize = 8 / 2;
                } else if (paintBrushSize == 3) {
                    brushSize = 16;
                    hBrushSize = 16 / 2;
                }

                int jMin = Math.max(yS - hBrushSize, 0);
                int jMax = Math.min(yS - hBrushSize + brushSize - 1, imageActive.getExtents()[1] - 1);
                int iMin = Math.max(xS - hBrushSize, 0);
                int iMax = Math.min(xS - hBrushSize + brushSize - 1, imageActive.getExtents()[0] - 1);

                int st;
                int offset = imageActive.getSliceSize() * slice;

                for (j = jMin; j <= jMax; j++) {

                    for (i = iMin; i <= iMax; i++) {

                        st = (j * imageActive.getExtents()[0]) + i;
                        paintBitmap.clear(offset + st);
                    }
                }

                if (imageActive.getType() == ModelStorageBase.COMPLEX) {
                    int temp = iMin;

                    iMin = imageActive.getExtents()[0] - Math.max(iMax, 1);
                    iMax = imageActive.getExtents()[0] - Math.max(temp, 1);
                    temp = jMin;
                    jMin = imageActive.getExtents()[1] - Math.max(jMax, 1);
                    jMax = imageActive.getExtents()[1] - Math.max(temp, 1);

                    if (imageActive.getNDims() == 3) {
                        offset = imageActive.getSliceSize() * (imageActive.getExtents()[2] - Math.max(slice, 1));
                    }

                    for (j = jMin; j <= jMax; j++) {

                        for (i = iMin; i <= iMax; i++) {

                            st = (j * imageActive.getExtents()[0]) + i;
                            paintBitmap.clear(offset + st);
                        }
                    }
                } // if (imageActive.getType() == ModelStorageBase.COMPLEX)

                importImage(pixBuffer);

                // repaint();
                paintComponent(g);
                g.dispose();

                return;
            } else if (mode == PAINT_VOI) {

                if (mouseEvent.getModifiers() == MouseEvent.BUTTON3_MASK) {

                    // erase
                    int brushSize = 0;
                    int hBrushSize = 0;

                    if (paintBrushSize == 0) {
                        brushSize = 1;
                        hBrushSize = 1 / 2;
                    } else if (paintBrushSize == 1) {
                        brushSize = 4;
                        hBrushSize = 4 / 2;
                    } else if (paintBrushSize == 2) {
                        brushSize = 8;
                        hBrushSize = 8 / 2;
                    } else if (paintBrushSize == 3) {
                        brushSize = 16;
                        hBrushSize = 16 / 2;
                    }

                    int jMin = Math.max(yS - hBrushSize, 0);
                    int jMax = Math.min(yS - hBrushSize + brushSize - 1, imageActive.getExtents()[1] - 1);
                    int iMin = Math.max(xS - hBrushSize, 0);
                    int iMax = Math.min(xS - hBrushSize + brushSize - 1, imageActive.getExtents()[0] - 1);

                    int st;
                    int offset = imageActive.getSliceSize() * slice;

                    for (j = jMin; j <= jMax; j++) {

                        for (i = iMin; i <= iMax; i++) {

                            st = (j * imageActive.getExtents()[0]) + i;
                            paintBitmap.clear(offset + st);
                        }
                    }

                    if (imageActive.getType() == ModelStorageBase.COMPLEX) {
                        int temp = iMin;

                        iMin = imageActive.getExtents()[0] - Math.max(iMax, 1);
                        iMax = imageActive.getExtents()[0] - Math.max(temp, 1);
                        temp = jMin;
                        jMin = imageActive.getExtents()[1] - Math.max(jMax, 1);
                        jMax = imageActive.getExtents()[1] - Math.max(temp, 1);

                        if (imageActive.getNDims() == 3) {
                            offset = imageActive.getSliceSize() * (imageActive.getExtents()[2] - Math.max(slice, 1));
                        }

                        for (j = jMin; j <= jMax; j++) {

                            for (i = iMin; i <= iMax; i++) {

                                st = (j * imageActive.getExtents()[0]) + i;
                                paintBitmap.clear(offset + st);
                            }
                        }
                    } // if (imageActive.getType() == ModelStorageBase.COMPLEX

                    importImage(pixBuffer);

                    // repaint();
                    paintComponent(g);
                    g.dispose();

                    return;
                } else {

                    // paint
                    Color color = frame.getControls().getTools().getPaintColor();

                    g.setPaintMode();
                    g.setColor(color);

                    int brushSize = getBrushSize();
                    int hBrushSize = getHBrushSize();

                    int jMin = Math.max(yS - hBrushSize, 0);
                    int jMax = Math.min(yS - hBrushSize + brushSize - 1, imageActive.getExtents()[1] - 1);
                    int iMin = Math.max(xS - hBrushSize, 0);
                    int iMax = Math.min(xS - hBrushSize + brushSize - 1, imageActive.getExtents()[0] - 1);

                    int st;
                    int offset = imageActive.getSliceSize() * slice;

                    for (j = jMin; j <= jMax; j++) {

                        for (i = iMin; i <= iMax; i++) {

                            st = (j * imageActive.getExtents()[0]) + i;
                            paintBitmap.set(offset + st);
                        }
                    }

                    if (imageActive.getType() == ModelStorageBase.COMPLEX) {
                        int temp = iMin;

                        iMin = imageActive.getExtents()[0] - Math.max(iMax, 1);
                        iMax = imageActive.getExtents()[0] - Math.max(temp, 1);
                        temp = jMin;
                        jMin = imageActive.getExtents()[1] - Math.max(jMax, 1);
                        jMax = imageActive.getExtents()[1] - Math.max(temp, 1);

                        if (imageActive.getNDims() == 3) {
                            offset = imageActive.getSliceSize() * (imageActive.getExtents()[2] - Math.max(slice, 1));
                        }

                        for (j = jMin; j <= jMax; j++) {

                            for (i = iMin; i <= iMax; i++) {

                                st = (j * imageActive.getExtents()[0]) + i;
                                paintBitmap.set(offset + st);
                            }
                        }
                    } // if (imageActive.getType() == ModelStorageBase.COMPLEX)

                    importImage(pixBuffer);

                    // repaint();
                    paintComponent(g);
                    g.dispose();

                    return;
                }
            }

            if ((mode == MAG_REGION) && (mouseEvent.getModifiers() == MouseEvent.BUTTON3_MASK)) {

                if ((magSettings != null) && !magSettings.isVisible()) {
                    magSettings.setWidthText((int) (frame.getSize().width * 0.25));
                    magSettings.setVisible(true);
                }
            }

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

                                        length = (int) (Math.sqrt(((lineX[1] - lineX[0]) * (lineX[1] - lineX[0])) +
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

                                            contourGraph.setDefaultDirectory(getActiveImage().getUserInterface().getDefaultDirectory());
                                            contourGraph.setVisible(true);
                                            VOIs.VOIAt(i).setContourGraph(contourGraph);
                                            contourGraph.setVOI(VOIs.VOIAt(i));
                                        } else {
                                            VOIs.VOIAt(i).getContourGraph().setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(imageActive.getUnitsOfMeasure(0)));
                                            VOIs.VOIAt(i).getContourGraph().saveNewFunction(rgbPos, rgbInten, j);
                                        }

                                        return;
                                    } else {

                                        length = (int) (Math.sqrt(((lineX[1] - lineX[0]) * (lineX[1] - lineX[0])) +
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
                                            lineGraph.setDefaultDirectory(imageActive.getUserInterface().getDefaultDirectory());
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
            setMode(DEFAULT);

            return;
        }

        if (mode == MOVE) {
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
        FileInfoBase fileInfo;
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

        try {

            // System.out.println("imageActive.getFileInfo(0) = " + imageActive.getFileInfo(0));
            if ((imageActive.getOrigin()[0] != 0) || (imageActive.getOrigin()[1] != 0) ||
                    (imageActive.getOrigin()[2] != 0)) {
                fileInfo = imageActive.getFileInfo()[slice];

                String[] values = setMouseOverlayData(fileInfo, mouseEvent, slice);

                if (values != null) {
                    frame.setMessageText("  X: " + String.valueOf(xS) + " Y: " + String.valueOf(yS) + "  Intensity:  " +
                                         String.valueOf(imageBufferActive[(yS * imageActive.getExtents()[0]) + xS]) +
                                         " Position: " + values[0] + " " + values[1] + " " + values[2]);
                } else {
                    frame.setMessageText("  X: " + String.valueOf(xS) + " Y: " + String.valueOf(yS) + "  Intensity:  " +
                                         String.valueOf(imageBufferActive[(yS * imageActive.getExtents()[0]) + xS]));
                }

            } else {

                if (imageActive.isColorImage() == true) {
                    frame.setMessageText("  X: " + String.valueOf(xS) + " Y: " + String.valueOf(yS) + "  R:  " +
                                         String.valueOf(imageBufferActive[(4 *
                                                                               ((yS * imageActive.getExtents()[0]) +
                                                                                    xS)) + 1]) + "  G:  " +
                                         String.valueOf(imageBufferActive[(4 *
                                                                               ((yS * imageActive.getExtents()[0]) +
                                                                                    xS)) + 2]) + "  B:  " +
                                         String.valueOf(imageBufferActive[(4 *
                                                                               ((yS * imageActive.getExtents()[0]) +
                                                                                    xS)) + 3]));
                } else {
                    frame.setMessageText("  X: " + String.valueOf(xS) + " Y: " + String.valueOf(yS) + "  Intensity:  " +
                                         String.valueOf(imageBufferActive[(yS * imageActive.getExtents()[0]) + xS]));
                }
            }
        } catch (ArrayIndexOutOfBoundsException error) {
            frame.setMessageText("  X: " + String.valueOf(xS) + " Y: " + String.valueOf(yS));
        }

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

                if (mode == NEW_POINT) {
                    ((VOICardiology) (VOIs.VOIAt(0).getCurves()[slice].elementAt(0))).insertElement(xS, yS, slice);
                    imageActive.notifyImageDisplayListeners(null, true);
                    g.dispose();
                    setMode(DEFAULT);

                    return;
                } else if (mode == DELETE_POINT) {
                    ((VOICardiology) (VOIs.VOIAt(0).getCurves()[slice].elementAt(0))).removeElement();

                    imageActive.notifyImageDisplayListeners();
                    setMode(MOVE_POINT);

                    return;

                } else if (mode == MOVE) {
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


        if (mode == SELECT) { // paintComponent(getGraphics());

            // setMode(DEFAULT);
        } else if (mode == POINT_VOI) {

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
                        setMode(DEFAULT);

                        return;
                    }

                  //  voiHandler.setLastPointVOI_ID(voiID);
                    imageActive.registerVOI(newPtVOI);
                    newPtVOI.setActive(true);

                    updateVOIColor(newPtVOI.getColor(), newPtVOI.getUID());
                    ((VOIPoint) (VOIs.VOIAt(voiHandler.getVOI_ID()).getCurves()[slice].elementAt(0))).setActive(true);

                    imageActive.notifyImageDisplayListeners();

                    voiHandler.graphPointVOI(newPtVOI, ((VOIPoint) (VOIs.VOIAt(voiHandler.getVOI_ID()).getCurves()[slice].elementAt(0))), 0);

                    if (mouseEvent.isShiftDown() != true) {
                        setMode(DEFAULT);
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
                                      ((VOIPoint) (VOIs.VOIAt(i).getCurves()[slice].elementAt(index - 1))), index - 1);
                    }

                    if (mouseEvent.isShiftDown() != true) {
                        setMode(DEFAULT);
                    }

                    return;
                } // end of else for if voiID != -1 add point to existing VOI
            } // end of if ((mouseEvent.getModifiers() & mouseEvent.BUTTON1_MASK) != 0)
        } // end of else if (mode == POINT_VOI)
        else if (mode == ANNOTATION) {

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
                    setMode(DEFAULT);
                }

            } // end of if ((mouseEvent.getModifiers() & mouseEvent.BUTTON1_MASK) != 0)

        } else if ((mode == POLYLINE) || (mode == LIVEWIRE)) {
            return;
            // setMode(DEFAULT);
        } else if (mode == LEVELSET) { }
        else if (mode == RECTANGLE) { }
        else if (mode == RECTANGLE3D) { }
        else if (mode == ELLIPSE) { }
        else if (mode == LINE) { }
        else if (mode == PROTRACTOR) { }
        else if (mode == NEW_POINT) { // impossible for LINE

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

                                                            intensitySum += ((VOIContour) (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                                .calcRGBIntensity(voiHandler.getImageGraphBuffer(),
                                                                                                      imageActive.getExtents()[0],
                                                                                                      c);
                                                            numPixels += ((VOIContour) (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
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
                                                            imageActive.exportData(s * length, length, voiHandler.getImageGraphBuffer());
                                                        } // locks and releases lock

                                                        intensitySum += ((VOIContour) (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                            .calcIntensity(voiHandler.getImageGraphBuffer(),
                                                                                               imageActive.getExtents()[0]);
                                                        numPixels += ((VOIContour) (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
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
                                                    intensitySum += ((VOIContour) (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                        .calcIntensity(voiHandler.getImageGraphBuffer(),
                                                                                           imageActive.getExtents()[0]);
                                                    numPixels += ((VOIContour) (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
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
        } else if (mode == DELETE_POINT) { // impossible for LINE
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
            setMode(MOVE_POINT);
        } else if (mode == PAINT_CAN) {
            xPG = (short) xS;
            yPG = (short) yS;
            zPG = (short) slice;
            seedVal = imageBufferActive[(yS * imageActive.getExtents()[0]) + xS];
            regionGrow((short) xS, (short) yS, (short) slice, seedVal, null, true);
            imageActive.notifyImageDisplayListeners(null, true);

        } else if (mode == PAINT_VASC) {
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
        } else if (mode == MOVE) {
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
                            updateVOIColor(VOIs.VOIAt(i).getColor(), VOIs.VOIAt(i).getUID());
                            voiHandler.setVOI_ID(VOIs.VOIAt(i).getID());

                            // and we are done with this VOI.
                            // skip the rest of the curves
                            j = VOIs.VOIAt(i).getCurves()[slice].size();
                        } else {
                            allActive = false;
                            VOIs.VOIAt(i).setActive(true);
                            updateVOIColor(VOIs.VOIAt(i).getColor(), VOIs.VOIAt(i).getUID());
                            ((VOIPoint) (selectedCurve)).setActive(true);
                            voiHandler.setVOI_ID(VOIs.VOIAt(i).getID());
                        }

                        getVOIHandler().fireVOISelectionChange(VOIs.VOIAt(i), selectedCurve);

                    } else if ((selectedCurve instanceof VOIText) &&
                                   ((VOIText) selectedCurve).contains(xS, yS, getZoomX(), getZoomY(),
                                                                          imageActive.getResolutions(0),
                                                                          g)) {

                        allActive = false;
                        VOIs.VOIAt(i).setActive(true);
                        updateVOIColor(VOIs.VOIAt(i).getColor(), VOIs.VOIAt(i).getUID());
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
                            updateVOIColor(VOIs.VOIAt(i).getColor(), VOIs.VOIAt(i).getUID());
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
                            updateVOIColor(VOIs.VOIAt(i).getColor(), VOIs.VOIAt(i).getUID());
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
        } else if (mode == MOVE_POINT) {

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
        } else if (mode == RETRACE) {
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
     * Grows a region based on a starting supplied. A voxel is added to the the paintBitmap mask if its intensity is
     * between previously supplied bounds.
     *
     * @param  str  the string to prepend to message containing region growth statistics
     */
    public void regionGrow(String str) {

        if (seedPaintBitmap != null) {
            regionGrow(xPG, yPG, zPG, seedVal, str, false);
        }
    }

    /**
     * Grows a region based on a starting supplied. A voxel is added to the the paintBitmap mask if its intensity is
     * between the the bounds where are also supplied.
     *
     * <p>when click <code>false</code>, adds points in the newly grown region which weren't in the old one remove
     * points which were in the old region but aren't in the new one (and which weren't in the region painted before the
     * last click), otherwise, the regions are simply added into the new set.</p>
     *
     * @param  x      x coordinate of the seed point
     * @param  y      y coordinate of the seed point
     * @param  z      z coordinate of the seed point
     * @param  value  Intensity value at the seed point
     * @param  str    String to start line with
     * @param  click  whether this region grow was initiated by a click on the image
     */
    public void regionGrow(short x, short y, short z, float value, String str, boolean click) {

        Cursor cursor = getCursor();

        setCursor(MipavUtil.waitCursor);

        int count;
        BitSet tempBitmap = null;

        if (click) {

            // backup the current paint mask
            int pEnd = paintBitmap.size();

            for (int p = 0; p < pEnd; p++) {

                if (paintBitmap.get(p)) {
                    paintBitmapBU.set(p);
                } else {
                    paintBitmapBU.clear(p);
                }
            }

            // reset the seedPaintBitmask so that we can keep track of points added in the regionGrow for this click
            if (seedPaintBitmap == null) {
                seedPaintBitmap = new BitSet();
            } else {
                seedPaintBitmap.clear();
            }
        } else {
            tempBitmap = (BitSet) seedPaintBitmap.clone();
            seedPaintBitmap.clear();
        }

        if (x != -1) {
            saveX = x;
            saveY = y;
            saveZ = z;
            saveValue = value;
        } else {
            return;
        }

        if (growDialog != null) {
            fuzzyThreshold = growDialog.getFuzzyThreshold();
            useVOI = growDialog.getUseVOI();
            displayFuzzy = growDialog.getDisplayFuzzy();
            sizeLimit = growDialog.getMaxSize();
            maxDistance = growDialog.getMaxDistance();
            less = growDialog.getLowerBound();
            more = growDialog.getUpperBound();
            variableThresholds = growDialog.getVariableThresholds();
        }

        if ((fuzzyThreshold == -2.0f) || (sizeLimit == -2) || (maxDistance == -2)) {
            return;
        }

        try {
            AlgorithmRegionGrow regionGrowAlgo = new AlgorithmRegionGrow(imageActive, 1.0f, 1.0f);

            regionGrowAlgo.setRunningInSeparateThread(false);

            if (imageActive.getType() == ModelStorageBase.BOOLEAN) {
                less = 0;
                more = 0;
                variableThresholds = false;
            }

            if (imageActive.getNDims() == 2) {
                count = regionGrowAlgo.regionGrow2D(seedPaintBitmap, new Point(saveX, saveY), fuzzyThreshold, useVOI,
                                                    displayFuzzy, growDialog, saveValue - less, saveValue + more,
                                                    sizeLimit, maxDistance, variableThresholds);
                showRegionInfo(count, str);
            } else if ((imageActive.getNDims() == 3) || (imageActive.getNDims() == 4)) {

                count = regionGrowAlgo.regionGrow3D(seedPaintBitmap, new Point3Ds(saveX, saveY, saveZ), fuzzyThreshold,
                                                    useVOI, displayFuzzy, growDialog, saveValue - less,
                                                    saveValue + more, sizeLimit, maxDistance, variableThresholds,
                                                    timeSlice, null);
                showRegionInfo(count, str);
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentEditImage.regionGrow");
        }

        if (!click) {

            // add points in the newly grown region which weren't in the old one
            BitSet diff = (BitSet) seedPaintBitmap.clone();

            diff.andNot(tempBitmap);
            paintBitmap.or(diff);

            // remove points which were in the old region but aren't in the new one
            // (and which weren't in the region painted before the last click)
            diff = (BitSet) seedPaintBitmap.clone();
            tempBitmap.andNot(diff);
            tempBitmap.andNot(paintBitmapBU);
            paintBitmap.xor(tempBitmap);
        } else {
            paintBitmap.or(seedPaintBitmap);
        }

        // paintChangeFlag = true;

        if (growDialog != null) {
            growDialog.notifyPaintListeners(true, false, paintBitmap);
        }

        imageActive.notifyImageDisplayListeners(null, true);
        setCursor(cursor);
    }

    /**
     * Grows a region based on a starting supplied. A voxel is added to the the paintBitmap mask if its intensity is
     * between the the bounds where are also supplied.
     *
     * <p>When click is <code>false</code>, adds points in the newly grown region which weren't in the old one remove
     * points which were in the old region but aren't in the new one (and which weren't in the region painted before the
     * last click), otherwise, the regions are simply added into the new set.</p>
     *
     * @param  x           x coordinate of the seed point
     * @param  y           y coordinate of the seed point
     * @param  z           z coordinate of the seed point
     * @param  value       Intensity value at the seed point
     * @param  image       the image to perform the region grow in
     * @param  leadString  the string to append to the region grow output
     * @param  click       whether this region grow was initiated by a click on the image
     */
    public void regionGrow(short x, short y, short z, float value, ModelImage image, String leadString, boolean click) {
        Cursor cursor = getCursor();

        setCursor(MipavUtil.waitCursor);

        int count;

        BitSet tempBitmap = null;

        if (click) {

            // backup the current paint mask
            int pEnd = paintBitmap.size();

            for (int p = 0; p < pEnd; p++) {

                if (paintBitmap.get(p)) {
                    paintBitmapBU.set(p);
                } else {
                    paintBitmapBU.clear(p);
                }
            }

            if (seedPaintBitmap == null) {

                // System.err.println("reseting seedPaintBitmap");
                seedPaintBitmap = new BitSet();
            } else {

                // System.err.println("Clearing seedPaintBitmap");
                seedPaintBitmap.clear();
            }
        } else {
            tempBitmap = (BitSet) seedPaintBitmap.clone();
            seedPaintBitmap.clear();
        }

        if (x != -1) {
            saveX = x;
            saveY = y;
            saveZ = z;
            saveValue = value;
        } else {
            return;
        }

        if (growDialog != null) {
            fuzzyThreshold = growDialog.getFuzzyThreshold();
            useVOI = growDialog.getUseVOI();
            displayFuzzy = growDialog.getDisplayFuzzy();
            sizeLimit = growDialog.getMaxSize();
            maxDistance = growDialog.getMaxDistance();
            less = growDialog.getLowerBound();
            more = growDialog.getUpperBound();
            variableThresholds = growDialog.getVariableThresholds();
        }

        if ((fuzzyThreshold == -2.0f) || (sizeLimit == -2) || (maxDistance == -2)) {
            return;
        }

        try {
            AlgorithmRegionGrow regionGrowAlgo = new AlgorithmRegionGrow(image, 1.0f, 1.0f);

            regionGrowAlgo.setRunningInSeparateThread(false);

            if (image.getType() == ModelStorageBase.BOOLEAN) {
                less = 0;
                more = 0;
                variableThresholds = false;
            }

            if (image.getNDims() == 2) {
                count = regionGrowAlgo.regionGrow2D(seedPaintBitmap, new Point(saveX, saveY), fuzzyThreshold, useVOI,
                                                    displayFuzzy, growDialog, saveValue - less, saveValue + more,
                                                    sizeLimit, maxDistance, variableThresholds);
                showRegionInfo(count, leadString);
            } else if ((image.getNDims() == 3) || (image.getNDims() == 4)) {
                count = regionGrowAlgo.regionGrow3D(seedPaintBitmap, new Point3Ds(saveX, saveY, saveZ), fuzzyThreshold,
                                                    useVOI, displayFuzzy, growDialog, saveValue - less,
                                                    saveValue + more, sizeLimit, maxDistance, variableThresholds,
                                                    timeSlice, null);
                showRegionInfo(count, leadString);
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentEditImage.regionGrow");
        }

        if (!click) {

            // add points in the newly grown region which weren't in the old one
            BitSet diff = (BitSet) seedPaintBitmap.clone();

            diff.andNot(tempBitmap);
            paintBitmap.or(diff);

            // remove points which were in the old region but aren't in the new one
            // (and which weren't in the region painted before the last click)
            diff = (BitSet) seedPaintBitmap.clone();
            tempBitmap.andNot(diff);
            tempBitmap.andNot(paintBitmapBU);
            paintBitmap.xor(tempBitmap);
        } else {
            paintBitmap.or(seedPaintBitmap);
        }

        // paintChangeFlag = true;

        if (growDialog != null) {
            growDialog.notifyPaintListeners(true, false, paintBitmap);
        }

        image.notifyImageDisplayListeners(null, true);
        setCursor(cursor);
    }

    /**
     * Remembers the current paint brush size so that it can be reset later.
     */
    public void rememberPaintBrushSize() {

        // only remember if we're not remembering another brush size
        if (previousPaintBrush == -1) {
            previousPaintBrush = paintBrushSize;
        }
    }

    /**
     * Resets the LUTs.
     */
    public void resetLUTs() {

        try {
            float min, max;
            float[] x = new float[4];
            float[] y = new float[4];
            float[] z = new float[4];

            float[] x2 = new float[4];
            float[] y2 = new float[4];
            float[] z2 = new float[4];

            float[] x3 = new float[4];
            float[] y3 = new float[4];
            float[] z3 = new float[4];
            Dimension dim = new Dimension(256, 256);

            if (imageA.isColorImage() == false) {

                if (imageA == imageActive) {

                    // Set LUT min max values;
                    if (imageA.getType() == ModelStorageBase.UBYTE) {
                        min = 0;
                        max = 255;
                    } else if (imageA.getType() == ModelStorageBase.BYTE) {
                        min = -128;
                        max = 127;
                    } else {
                        min = (float) imageA.getMin();
                        max = (float) imageA.getMax();
                    }

                    x[0] = min;
                    y[0] = dim.height - 1;
                    z[0] = 0;
                    x[1] = (min + ((max - min) / 3.0f));
                    y[1] = (dim.height - 1) - ((dim.height - 1) / 3.0f);
                    z[1] = 0;
                    x[2] = (min + ((max - min) * 0.67f));
                    y[2] = (dim.height - 1) - ((dim.height - 1) * 0.67f);
                    z[2] = 0;
                    x[3] = max;
                    y[3] = 0;
                    z[3] = 0;
                    LUTa.getTransferFunction().importArrays(x, y, 4);
                } else if ((imageB != null) && (imageB == imageActive)) {

                    if (imageB.getType() == ModelStorageBase.UBYTE) {
                        min = 0;
                        max = 255;
                    } else if (imageB.getType() == ModelStorageBase.BYTE) {
                        min = -128;
                        max = 127;
                    } else {
                        min = (float) imageB.getMin();
                        max = (float) imageB.getMax();
                    }

                    x2[0] = min;
                    y2[0] = dim.height - 1;
                    z2[0] = 0;
                    x2[1] = (min + ((max - min) / 3.0f));
                    y2[1] = (dim.height - 1) - ((dim.height - 1) / 3.0f);
                    z2[1] = 0;
                    x2[2] = (min + ((max - min) * 0.67f));
                    y2[2] = (dim.height - 1) - ((dim.height - 1) * 0.67f);
                    z2[2] = 0;
                    x2[3] = max;
                    y2[3] = 0;
                    z2[3] = 0;
                    LUTb.getTransferFunction().importArrays(x2, y2, 4);
                }
            } else { // RGB image

                int[] RGBExtents = new int[2];

                RGBExtents[0] = 4;
                RGBExtents[1] = 256;

                if (imageA == imageActive) {

                    // Set LUT min max values;
                    x[0] = 0;
                    y[0] = dim.height - 1;
                    z[0] = 0;
                    x[1] = 255 * 0.333f;
                    y[1] = (dim.height - 1) - ((dim.height - 1) / 3.0f);
                    z[1] = 0;
                    x[2] = 255 * 0.667f;
                    y[2] = (dim.height - 1) - ((dim.height - 1) * 0.67f);
                    z[2] = 0;
                    x[3] = 255;
                    y[3] = 0;
                    z[3] = 0;

                    // System.out.pri ntln("RGBTA = " + RGBTA);
                    if (RGBTA == null) {
                        RGBTA = new ModelRGB(RGBExtents);
                        // imageA.setRGBT(RGBTA);
                    }

                    RGBTA.getRedFunction().importArrays(x, y, 4);

                    x2[0] = 0;
                    y2[0] = dim.height - 1;
                    z2[0] = 0;
                    x2[1] = 255 * 0.333f;
                    y2[1] = (dim.height - 1) - ((dim.height - 1) / 3.0f);
                    z2[1] = 0;
                    x2[2] = 255 * 0.667f;
                    y2[2] = (dim.height - 1) - ((dim.height - 1) * 0.67f);
                    z2[2] = 0;
                    x2[3] = 255;
                    y2[3] = 0;
                    z2[3] = 0;
                    RGBTA.getGreenFunction().importArrays(x2, y2, 4);

                    x3[0] = 0;
                    y3[0] = dim.height - 1;
                    z3[0] = 0;
                    x3[1] = 255 * 0.333f;
                    y3[1] = (dim.height - 1) - ((dim.height - 1) / 3.0f);
                    z3[1] = 0;
                    x3[2] = 255 * 0.667f;
                    y3[2] = (dim.height - 1) - ((dim.height - 1) * 0.67f);
                    z3[2] = 0;
                    x3[3] = 255;
                    y3[3] = 0;
                    z3[3] = 0;
                    RGBTA.getBlueFunction().importArrays(x3, y3, 4);

                    RGBTA.makeRGB(-1);
                } else if ((imageBufferB != null) && (imageB != null) && (imageB == imageActive)) {

                    // Set LUT min max values;
                    x = new float[4];
                    y = new float[4];
                    z = new float[4];

                    x2 = new float[4];
                    y2 = new float[4];
                    z2 = new float[4];

                    x3 = new float[4];
                    y3 = new float[4];
                    z3 = new float[4];

                    x[0] = 0;
                    y[0] = dim.height - 1;
                    z[0] = 0;
                    x[1] = 255 * 0.333f;
                    y[1] = (dim.height - 1) - ((dim.height - 1) / 3.0f);
                    z[1] = 0;
                    x[2] = 255 * 0.667f;
                    y[2] = (dim.height - 1) - ((dim.height - 1) * 0.67f);
                    z[2] = 0;
                    x[3] = 255;
                    y[3] = 0;
                    z[3] = 0;

                    if (RGBTB == null) {
                        RGBTB = new ModelRGB(RGBExtents);
                        // imageB.setRGBT(RGBTB);
                    }

                    RGBTB.getRedFunction().importArrays(x, y, 4);

                    x2[0] = 0;
                    y2[0] = dim.height - 1;
                    z2[0] = 0;
                    x2[1] = 255 * 0.333f;
                    y2[1] = (dim.height - 1) - ((dim.height - 1) / 3.0f);
                    z2[1] = 0;
                    x2[2] = 255 * 0.667f;
                    y2[2] = (dim.height - 1) - ((dim.height - 1) * 0.67f);
                    z2[2] = 0;
                    x2[3] = 255;
                    y2[3] = 0;
                    z2[3] = 0;
                    RGBTB.getGreenFunction().importArrays(x2, y2, 4);

                    x3[0] = 0;
                    y3[0] = dim.height - 1;
                    z3[0] = 0;
                    x3[1] = 255 * 0.333f;
                    y3[1] = (dim.height - 1) - ((dim.height - 1) / 3.0f);
                    z3[1] = 0;
                    x3[2] = 255 * 0.667f;
                    y3[2] = (dim.height - 1) - ((dim.height - 1) * 0.67f);
                    z3[2] = 0;
                    x3[3] = 255;
                    y3[3] = 0;
                    z3[3] = 0;
                    RGBTB.getBlueFunction().importArrays(x3, y3, 4);

                    RGBTB.makeRGB(-1);
                }
            }

            imageA.notifyImageDisplayListeners(null, false);

            if (imageB != null) {
                imageB.notifyImageDisplayListeners(null, false);
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentEditImage.resetLUT");
        }
    }

    /**
     * Resets the paint brush size to the remembered value.
     */
    public void resetPaintBrushSize() {

        // don't reset unless we've remembered a brush size
        if (previousPaintBrush != -1) {
            setPaintBrushSize(previousPaintBrush);
        }

        previousPaintBrush = -1;
    }

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
     * For generating the display of 1 or 2 RGB images.
     *
     * @param   tSlice     t (time) slice to show
     * @param   zSlice     z slice to show
     * @param   forceShow  forces this method to import image and recalculate java image
     *
     * @return  boolean to indicate if the show was successful
     */
    public boolean show(int tSlice, int zSlice, boolean forceShow) {

        if (setColorPaintBuffers(tSlice, zSlice, forceShow)) {
            paintComponent(getGraphics());

            return true;
        } else {
            return false;
        }
    }

    /**
     * Shows the image and the VOI(s).
     *
     * @param   tSlice      t (time) slice to show
     * @param   zSlice      z slice to show
     * @param   _LUTa       LUTa - to change to new LUT for imageA else null
     * @param   _LUTb       LUTb - to change to new LUT for imageB else null
     * @param   forceShow   forces this method to import image and recalculate java image
     * @param   interpMode  image interpolation method (Nearest or Smooth)
     *
     * @return  boolean to indicate if the show was successful
     */
    public boolean show(int tSlice, int zSlice, ModelLUT _LUTa, ModelLUT _LUTb, boolean forceShow, int interpMode) {

        // if ( interpMode > -1 ) {
        // setInterpolationMode( interpMode );
        // }

        if (imageA.isColorImage()) {

            // call the show method for displaying RGB images
            return (show(tSlice, zSlice, forceShow));
        }

        if (setPaintBuffers(tSlice, zSlice, _LUTa, _LUTb, forceShow)) {
            paintComponent(getGraphics());

            return true;
        } else {
            return false;
        }
    } // end of show(int tSlice, int zSlice, ModelLUT _LUTa, ModelLUT _LUTb, boolean forceShow)

    /**
     * Display statistics about the grown region.
     *
     * @param  count       Number of pixels (voxels)
     * @param  leadString  the string to prepend to message containing region growth statistics
     */
    public void showRegionInfo(int count, String leadString) {
        float volume;
        float area;
        int measure;

        try {
            String str = new String();

            FileInfoBase[] fileInfo = imageActive.getFileInfo();

            if (imageActive.getNDims() == 2) {
                area = count * imageActive.getResolutions(0)[0] * imageActive.getResolutions(0)[1];
                measure = imageActive.getUnitsOfMeasure(0);

                if (measure == FileInfoBase.INCHES) {
                    str = " inches^2";
                } else if (measure == FileInfoBase.ANGSTROMS) {
                    str = " A^2";
                } else if (measure == FileInfoBase.NANOMETERS) {
                    str = " nm^2";
                } else if (measure == FileInfoBase.MICROMETERS) {
                    str = " um^2";
                } else if (measure == FileInfoBase.MILLIMETERS) {
                    str = " mm^2";
                } else if (measure == FileInfoBase.CENTIMETERS) {
                    str = " cm^2";
                } else if (measure == FileInfoBase.METERS) {
                    str = " m^2";
                } else if (measure == FileInfoBase.KILOMETERS) {
                    str = " km^2";
                } else if (measure == FileInfoBase.MILES) {
                    str = " miles^2";
                } else {
                    str = "Unknown";
                }

                if (leadString != null) {
                    frame.getUserInterface().setDataText(leadString + " region grow: pixels = " + count +
                                                         "\t  area = " + area + str + "\n");
                } else {
                    frame.getUserInterface().setDataText("Region grow: pixels = " + count + "\t  area = " + area + str +
                                                         "\n");
                }

            } else {
                volume = count * imageActive.getResolutions(0)[0] * imageActive.getResolutions(0)[1] * imageActive.getResolutions(0)[2];

                measure = imageActive.getUnitsOfMeasure(0);

                if (measure == FileInfoBase.INCHES) {
                    str = " inches^3";
                } else if (measure == FileInfoBase.ANGSTROMS) {
                    str = " A^3";
                } else if (measure == FileInfoBase.NANOMETERS) {
                    str = " nm^3";
                } else if (measure == FileInfoBase.MICROMETERS) {
                    str = " um^3";
                } else if (measure == FileInfoBase.MILLIMETERS) {
                    str = " mm^3";
                } else if (measure == FileInfoBase.CENTIMETERS) {
                    str = " cm^3";
                } else if (measure == FileInfoBase.METERS) {
                    str = " m^3";
                } else if (measure == FileInfoBase.KILOMETERS) {
                    str = " km^3";
                } else if (measure == FileInfoBase.MILES) {
                    str = " miles^3";
                } else {
                    str = "Unknown";
                }

                if (leadString != null) {
                    frame.getUserInterface().setDataText(leadString + " region grow: pixels = " + count +
                                                         "\t  volume = " + volume + str + "\n");
                } else {
                    frame.getUserInterface().setDataText("Region grow: pixels = " + count + "\t  volume = " + volume +
                                                         str + "\n");
                }
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentEditImage.showRegionInfo");
        }
    }

    /**
     * DOCUMENT ME!
     */
    public void toggleVOIPoints() {
        ((VOICardiology) imageActive.getVOIs().VOIAt(0).getCurves()[0].elementAt(0)).togglePoints();
        imageActive.notifyImageDisplayListeners();
    }

    /**
     * Undoes the last paint.
     */
    public void undoLastPaint() {
        int pEnd = paintBitmap.size();

        for (int p = 0; p < pEnd; p++) {

            if (paintBitmapBU.get(p)) {
                paintBitmap.set(p);
            } else {
                paintBitmap.clear(p);
            }
        }

        // paintChangeFlag = true;
        if (growDialog != null) {
            growDialog.notifyPaintListeners(false, false, paintBitmap);
        } else {
            imageActive.notifyImageDisplayListeners(null, true);
        }
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

    /**
     * Sets position data to display in message bar - for DICOM and MINC images, gives patient position as well. The
     * image's associated transformation must be FileInfoBase.TRANSFORM_SCANNER_ANATOMICAL or the function returns null.
     *
     * @param   fileInfo    File info object of image displayed.
     * @param   mouseEvent  Event that triggered this call.
     * @param   zSlice      Index to slice in the Z-plane.
     *
     * @return  An array of strings that represent patient position.
     */
    protected String[] setMouseOverlayData(FileInfoBase fileInfo, MouseEvent mouseEvent, int zSlice) {

        if (imageActive.getFileInfo()[0].getTransformID() != FileInfoBase.TRANSFORM_SCANNER_ANATOMICAL) {
            return null;
        }

        int nDims = imageActive.getNDims();

        if (nDims > 3) {
            nDims = 3;
        }

        String[] strs = new String[3];
        DecimalFormat nf = new DecimalFormat("#####0.0##");
        float[] coord = new float[3];
        float[] tCoord = new float[3];
        float[] origin = new float[3];
        float[] res = new float[3];

        // Get the voxel coordinate in from mouse events in image space
        coord[0] = Math.round((mouseEvent.getX() / (getZoomX() * resolutionX)) - 0.5f); // zoomed x.  Used as cursor
        coord[1] = Math.round((mouseEvent.getY() / (getZoomY() * resolutionY)) - 0.5f); // zoomed y.  Used as cursor
        coord[2] = zSlice;

        // Get the DICOM reference point ( origin ) in the image orientation (axial, sag, coronal )
        origin[0] = imageActive.getOrigin()[0];
        origin[1] = imageActive.getOrigin()[1];
        origin[2] = imageActive.getOrigin()[2];
        // System.out.println("Origin     "  + origin[0] + ", " + origin[1] + ", " + origin[2] );

        // Get voxel resolutions
        res[0] = fileInfo.getResolutions()[0];
        res[1] = fileInfo.getResolutions()[1];
        res[2] = fileInfo.getResolutions()[2];

        // Change voxel coordinate into millimeter space
        coord[0] = coord[0] * res[0];
        coord[1] = coord[1] * res[1];
        coord[2] = coord[2] * res[2];

        // Get the DICOM transform that discribes the transformation from axial to this image orientation
        TransMatrix dicomMatrix = (TransMatrix) (imageActive.getMatrix().clone());

        // Finally convert the point to axial millimeter DICOM space.
        dicomMatrix.transform(coord, tCoord);

        // Add in the DICOM origin
        tCoord[0] = origin[0] + tCoord[0];
        tCoord[1] = origin[1] + tCoord[1];
        tCoord[2] = origin[2] + tCoord[2];

        if (tCoord[0] < 0) {
            strs[0] = "R: " + String.valueOf(nf.format(-tCoord[0]));
        } else {
            strs[0] = "L: " + String.valueOf(nf.format(tCoord[0]));
        }

        if (tCoord[1] < 0) {
            strs[1] = "A: " + String.valueOf(nf.format(-tCoord[1]));
        } else {
            strs[1] = "P: " + String.valueOf(nf.format(tCoord[1]));
        }

        if (tCoord[2] < 0) {
            strs[2] = "I: " + String.valueOf(nf.format(-tCoord[2]));
        } else {
            strs[2] = "S: " + String.valueOf(nf.format(tCoord[2]));
        }

        return strs;
    }

    /**
     * Update the voi color.
     *
     * @param  voiColor  the new voi color
     * @param  voiUID    the last voi id
     */
    protected void updateVOIColor(Color voiColor, int voiUID) {

        if (frame instanceof ViewJFrameImage) {
            ((ViewJFrameImage) frame).getControls().setVOIColor(voiColor);

            // System.err.println("updating VOI Color with ID: " + voiID + " and UID: " + voiUID);
            ((ViewJFrameImage) frame).setLastVOI_UID(voiUID);
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  id      DOCUMENT ME!
     * @param  voiUID  DOCUMENT ME!
     */
    protected void updateVOIColor(int id, int voiUID) {

        if (frame instanceof ViewJFrameImage) {
            ((ViewJFrameImage) frame).getControls().setVOIColor(id);

            // System.err.println("updating VOI Color with ID: " + voiID + " and UID: " + voiUID);
            ((ViewJFrameImage) frame).setLastVOI_UID(voiUID);
        }
    }


}
