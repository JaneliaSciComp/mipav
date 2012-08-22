package gov.nih.mipav.view;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.scripting.ScriptRecorder;

import gov.nih.mipav.model.scripting.actions.ActionPaintToMask;
import gov.nih.mipav.model.structures.*;
import Jama.*;

import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOIManagerInterface;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOIManagerInterfaceListener;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;

import de.jtem.numericalMethods.algebra.linear.decompose.Singularvalue;

import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * ViewJFrameRegistrationTool is called in ViewJFrameImage. ViewJFrameRegistrationTool calls ViewJComponentRegistration.
 *
 * <p>In ordinary image dragging you simply drag one image. Here the adjustable slice is moved while the reference slice
 * stays fixed, and a bilinear transformation followed by an alpha blending of the 2 images occurs. Because g.drawImage
 * in ViewJComponentBase draws the blended image, the movement cannot simply be performed by changing the parameters of
 * g.drawImage.</p>
 *
 * <p>The adjustable image slice moves are determined by floating point values fed into a transform routine that
 * performs bilinear interpolation.</p>
 *
 * <p>The file menu has 2 items, Show VOIs in blended window and Close registration.
 * An alpha blending slider determines the percentages of the image produced by the reference slice(image R) and the
 * adjusted slice(imageA). 2 tabs are present, blended and dual.  On the  blended tab 17 toolbar buttons are present:</p>
 *
 * <p>1.) Display LUT table calls forth a display panel which allows the user to set separate histograms for the
 * reference and adjusted slices.<br>
 * 2.) Create checkerboard pattern<br>
 * 3.) Magnify image 2.0X.<br>
 * 4.) Magnify image 0.5X.<br>
 * 5.) Window region of imageB<br>
 * 6.) Reset to return slice to original state and remove all markers<br>
 * 7.) Commit the slice to the image.<br>
 * 8.) Set the pixel increment for image translations and movements of the rotation center. Values can range from 0.01
 * to 2048.0. Movements of the rotation center will only be performed to the nearest integer. The default value is 1.0
 * pixels.<br>
 * 9.) Put in translate mode. In this mode the image can either be moved with the up, down, right, and left buttons or
 * moved a distance and direction with mouse dragging. 
 * 10.) up button for image translation in translate mode.
 * <br>
 * 11.) down button for image translation in translate mode.
 * <br>
 * 12.) right button for image translation in translate mode.
 * <br>
 * 13.) left button for image translation in translate mode.
 * <br>
 * 14.) Set degree increment for image rotations. Values can range from 0.01 to 360.0 degrees. The default value is 1.0
 * degrees.<br>
 * 15.) Put in rotate mode for rotating the image. In this mode the image can be moved either with the cw and ccw
 * buttons or moved an angle around the rotation center point as given by mouse dragging.  The initial rotation
 * point is the center, but a new rotation point can be set with a mouse press.<br>
 * 16.) cw button for rotating an image clockwise in rotate mode.<br>
 * 17.) ccw button for rotating an image counterclockwise in rotate mode.<br>
 * 
 * On the dual tab 16 toolbar buttons are present:</p>
 * 
 * * <p>1.) Display LUT table calls forth a display panel which allows the user to set separate histograms for the
 * reference and adjusted slices.<br>
 * 2.) Magnify reference image 2.0X.<br>
 * 3.) Magnify reference image 0.5X.<br>
 * 4.) Magnify adjusted image 2.0X.<br>
 * 5.) Magnify adjusted image 0.5X.<br>
 * 6.) Apply least squares alignment.<br>
 * 7.) Apply thin plate spline alignment.<br>
 * 8.) Reset image to original state and remove all markers.<br>
 * 9.) Put in POINT_VOI mode for setting the location of reference slice markers. Red markers are used.<br>
 * 10.) Put in the POINT_VOI mode for setting the location of adjusted slice markers. Green markers are used.<br>
 * 11.) Delete selected reference slice markers.<br>
 * 12.) Delete selected adjusted slice markers.<br>
 * 13.) Copy reference markers to the adjusted image.<br>
 * 14.) Copy adjusted markers to the reference image.<br>
 * 15.) Return to default mode.<br>
 * 16.) Commit the slice to image.<br>
 * </p>
 *
 * @version  1.0
 */
public class ViewJFrameRegistrationTool extends ViewJFrameBase
        implements ItemListener, ChangeListener, FocusListener, WindowListener, VOIManagerInterfaceListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 5476210706978180244L;

    /** The 3 types of markers - rotation center, reference slice, and adjusted slice. */
    public static final int ROTATIONCENTER = 0;

    /** reference marker type. */
    public static final int REFMARK = 1;

    /** adjusted marker type. */
    public static final int ADJMARK = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public JToggleButton[] toggleArray = new JToggleButton[6]; // holds all toggle buttons

    /** DOCUMENT ME! */
    protected Font serif12;

    /** DOCUMENT ME! */
    private int adjMark;

    /** DOCUMENT ME! */
    private JToggleButton adjMarkButton; // button for adding adj marks

    /** DOCUMENT ME! */
    private JButton adjMarkMinusButton; // delete selected adj markers

    /** DOCUMENT ME! */
    private JSlider alphaSlider; // slider to adjust alpha blending

    /** DOCUMENT ME! */
    private JPanel bothPanel; // panel with images together

    /** DOCUMENT ME! */
    private int bufferFactor;

    /** DOCUMENT ME! */
    private int bufferSize;

    /** DOCUMENT ME! */
    private JButton[] buttonArray = new JButton[16]; // holds all buttons

    /** DOCUMENT ME! */
    private JButton ccwButton; // rotate counterclockwise


    /** for both images displayed together. */
    private ViewJComponentRegistration componentImage;

    /** for images to be displayed separately. */
    private ViewJComponentSingleRegistration componentImageA; // reference image

    /** DOCUMENT ME! */
    private ViewJComponentSingleRegistration componentImageB; // adjusted image

    /** DOCUMENT ME! */
    //private int componentY;

    /** DOCUMENT ME! */
    private JPanel controlPanel; // panel to hold the tool bars

    /** DOCUMENT ME! */
    private ViewControlsImage controls;

    /** DOCUMENT ME! */
    private JButton copyAdjToRefButton; // copy adj VOIs to reference

    /** DOCUMENT ME! */
    private JButton copyRefToAdjButton; // copy reference VOIs to adj

    /** DOCUMENT ME! */
    private GridBagConstraints cpGBC; // control panel grid bag constraints

    /** DOCUMENT ME! */
    private GridBagLayout cpGBL; // control panel grid bag layout

    /** DOCUMENT ME! */
    private int curAdjMark;

    /** DOCUMENT ME! */
    private int curRefMark;

    /** DOCUMENT ME! */
    private JButton cwButton; // rotate clockwise

    /** DOCUMENT ME! */
    private JToggleButton defaultModeButton; // switch to default mode

    /** DOCUMENT ME! */
    private float degreeIncrement = 1.0f; // standard degree increment for rotation

    /** DOCUMENT ME! */
    private JButton degreeIncrementButton; // change degree increment for rotation

    /** DOCUMENT ME! */
    private boolean doDrag = true;

    /** DOCUMENT ME! */
    private boolean doneLeastSquares = false;

    /** DOCUMENT ME! */
    private boolean doRegionB = true;

    /** DOCUMENT ME! */
    private JButton downButton; // transform down

    /** DOCUMENT ME! */
    private Border etchedBorder = BorderFactory.createEtchedBorder();

    /** DOCUMENT ME! */
    private int[] extents;

    /** DOCUMENT ME! */
    private JMenu fileMenu;

    /** DOCUMENT ME! */
    private boolean firstDraw = true; // draws the dual images when window is first created

    /** DOCUMENT ME! */
    private Font font12 = MipavUtil.font12;

    /** DOCUMENT ME! */
    private GridBagConstraints gbc; // content pane grid bag constraints

    /** DOCUMENT ME! */
    private JMenu helpMenu;

    /** DOCUMENT ME! */
    private int i, j; // Not sure these should be class variables

    /** DOCUMENT ME! */
    private ModelImage image; // primary image passed in (reference image)

    /** DOCUMENT ME! */
    private float[] imageBufferA; // buffer for new imageA

    /** DOCUMENT ME! */
    private float[] imageBufferB; // buffer for new imageB

    /** DOCUMENT ME! */
    private float[] imageBufferOriginalB; // original buffer for adjusted image (for reset)

    /** DOCUMENT ME! */
    private int imageSize;

    /** DOCUMENT ME! */
    private JMenuItem itemClose; // menu item to close registration window

    /** DOCUMENT ME! */
    private JMenuItem itemHelp; // menu item to close registration window

    /** DOCUMENT ME! */
    private JButton leftButton; // transform left


    /** DOCUMENT ME! */
    private boolean logMagDisplay;

    /** DOCUMENT ME! */
    private boolean lsPerformed = false;

    /** DOCUMENT ME! */
    private int mode;

    /** DOCUMENT ME! */
    private ButtonGroup movementGroup; // group of buttons for moving image

    /** DOCUMENT ME! */
    private int n;

    /** DOCUMENT ME! */
    private int newAlphaBlend = 50;

    /** DOCUMENT ME! */
    private int[] newExtents;

    /** DOCUMENT ME! */
    private JMenuBar openingMenuBar; // menu bar to hold file options

    /** DOCUMENT ME! */
    private int[] pixBuffer;

    /** DOCUMENT ME! */
    private int[] pixBufferB;

    /** DOCUMENT ME! */
    private int[] pixBufferCompA;

    /** DOCUMENT ME! */
    private int[] pixBufferCompB;

    /** DOCUMENT ME! */
    private float pixelIncrement = 1.0f; // standard pixel increment

    /** DOCUMENT ME! */
    private JButton pixelIncrementButton; // change pixel increment

    /** DOCUMENT ME! */
    private double[][] pointSetA;

    /** DOCUMENT ME! */
    private double[][] pointSetB;

    /** DOCUMENT ME! */
    private double[][] pointSetBT;

    /** DOCUMENT ME! */
    private int refMark;

    /** DOCUMENT ME! */
    private JToggleButton refMarkButton; // button for adding ref marks

    /** DOCUMENT ME! */
    private JButton refMarkMinusButton; // delete selected reference markers

    /** DOCUMENT ME! */
    private JToggleButton regButton = null;

    /** DOCUMENT ME! */
    private ModelRGB RGBa;

    /** DOCUMENT ME! */
    private ModelRGB RGBb;

    /** DOCUMENT ME! */
    private JButton rightButton; // transform right

    /** DOCUMENT ME! */
    private JToggleButton rotateButton; // for rotating the image

    /** DOCUMENT ME! */
    private JScrollPane scrollPane; // for both together

    /** DOCUMENT ME! */
    private JScrollPane scrollPaneSeparateA; // for both separate (ref image)

    /** DOCUMENT ME! */
    private JScrollPane scrollPaneSeparateB; // for both separate (adj image)

    /** DOCUMENT ME! */
    private int scrollPaneSize = 350;

    /** DOCUMENT ME! */
    private ModelImage secondImage; // second image passed in (adjusted image)

    /** DOCUMENT ME! */
    private JPanel separatePanel; // panel with images separate

    /** DOCUMENT ME! */
    private JCheckBoxMenuItem showVOIs; // option to show/hide vois in blended window

    /** DOCUMENT ME! */
    //private int structureY; // all totals in Y direction not due to image

    /** DOCUMENT ME! */
    private JTabbedPane tabbedPane; // tabbed pane to hold the blended and dual panels

    /** DOCUMENT ME! */
    private JToolBar toolBar, toolBar2; // tool bars for the blended window

    /** DOCUMENT ME! */
    private JToolBar toolBarSep; // tool bar for dual window

    /** DOCUMENT ME! */
    private JPanel topPanel = null;

    /** DOCUMENT ME! */
    private JToggleButton translateButton; // for translating the image

    /** DOCUMENT ME! */
    private JButton upButton; // transform up

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private int[] xCoords;

    /** DOCUMENT ME! */
    private int xDim;

    /** DOCUMENT ME! */
    private TransMatrix xfrm = null;

    /** DOCUMENT ME! */
    private TransMatrix xfrmBA = null;

    /** DOCUMENT ME! */
    private TransMatrix xfrmD = null;

    /** DOCUMENT ME! */
    private TransMatrix xfrmH = null;

    /** DOCUMENT ME! */
    private int[] xOrg;

    /** DOCUMENT ME! */
    private float xRes;

    /** DOCUMENT ME! */
    private float xRotation; // x rotation center

    /** DOCUMENT ME! */
    private int xScreen, yScreen;

    /** DOCUMENT ME! */
    private int[] yCoords;

    /** DOCUMENT ME! */
    private int yDim;

    /** DOCUMENT ME! */
    private int[] yOrg;

    /** DOCUMENT ME! */
    private float yRes;

    /** DOCUMENT ME! */
    private float yRotation; // y rotation center

    /** DOCUMENT ME! */
    private float zoom = 1; // zoom factor
    
    private JTextArea costFunctionTextArea;
    
    /** Scroll Pane for the Text Area **/
    private JScrollPane costFunctionScrollPane;
    
    /** Holds extents, other info about firstImage and SecondImage */
    private ModelSimpleImage simpleImg1, simpleImg2;
    
    /** Identity matrix for testing cost */
    private TransMatrix tMatrix;
    
    private AlgorithmCostFunctions2D algoCost;
    
    private JButton calculateCostButton;
    
    private VOIManagerInterface voiManager;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Makes a frame of the manually registered image.
     *
     * @param  _image   the first image
     * @param  _image2  the second image
     * @param  _LUT     lut for the first image
     * @param  _LUT2    lut for the second image
     */
    public ViewJFrameRegistrationTool(ModelImage _image, ModelImage _image2, ModelStorageBase _LUT,
                                      ModelStorageBase _LUT2) {

        super(_image, _image2);

        int bufferSize2;

        serif12 = MipavUtil.font12;
        userInterface = ViewUserInterface.getReference();

        if (_LUT != null) {

            if (_LUT instanceof ModelLUT) {
                LUTa = (ModelLUT) _LUT;
            } else {
                RGBa = (ModelRGB) _LUT;
            }
        }

        if (_LUT2 != null) {

            if (_LUT2 instanceof ModelLUT) {
                LUTb = (ModelLUT) _LUT2;
            } else {
                RGBb = (ModelRGB) _LUT2;
            }
        }

        image = _image;
        secondImage = _image2;
        newExtents = new int[2];
        newExtents[0] = image.getExtents()[0];
        newExtents[1] = image.getExtents()[1];

        String name = JDialogBase.makeImageName(image.getImageName(), "_result1");
        imageA = new ModelImage(image.getType(), newExtents, name);

        name = JDialogBase.makeImageName(image.getImageName(), "_result2");
        imageB = new ModelImage(secondImage.getType(), newExtents, name);

        bufferFactor = 1;

        if (image.isColorImage()) {
            bufferFactor = 4;
        }

        bufferSize = bufferFactor * image.getSliceSize();
        bufferSize2 = bufferFactor * secondImage.getSliceSize();

        imageBufferA = new float[bufferSize];
        imageBufferB = new float[bufferSize2];
        imageBufferOriginalB = new float[bufferSize2];

        // import data from image into imageA and secondImage into imageB
        try {
            image.exportData(0, bufferSize, imageBufferA);
            imageA.importData(0, imageBufferA, true);

            secondImage.exportData(0, bufferSize, imageBufferOriginalB);
            secondImage.exportData(0, bufferSize, imageBufferB);
            imageB.importData(0, imageBufferB, true);
        } catch (IOException error) {
            MipavUtil.displayError("ViewJFrameRegistrationTool: IOException Error on exportData - importData sequence");
        }

        init();
        addWindowListener(this);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * SQR = x^2.
     *
     * @param   x  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public static final double SQR(double x) {
        x *= x;

        return x;
    }

    // ************************************************************************
    // **************************** Action Events *****************************
    // ************************************************************************

    /**
     * Calls various methods depending on the action.
     *
     * @param  event  event that triggered function
     */
    public void actionPerformed(ActionEvent event) {

        String command;
        command = event.getActionCommand();
        if (command.equals("toggleVOIs")) {
            updateImages(true);
        } else if (command.equals("copyRefAdj")) {
            VOI pointRef = new VOI( componentImageA.getPointVOI() );
            componentImageB.setPointVOI(pointRef);
            componentImageB.getActiveImage().notifyImageDisplayListeners();
        } else if (command.equals("copyAdjRef")) {
            VOI pointRef = new VOI( componentImageB.getPointVOI() );
            componentImageA.setPointVOI(pointRef);
            componentImageA.getActiveImage().notifyImageDisplayListeners();
        } else if (command.equals("DisplayLUT")) {

            if (componentImage.getActiveImage().getType() == ModelStorageBase.BOOLEAN) {
                MipavUtil.displayError(" Cannot change the LUT of a Boolean image.");
            } else {

                if ((imageA.getHistoLUTFrame() == null) && (imageA.getHistoRGBFrame() == null)) {
                    JDialogHistogramLUT histogramDialog = null;

                    if (imageA.isColorImage() == false) {

                        try {
                            histogramDialog = new JDialogHistogramLUT(this, componentImage, imageA, imageB, LUTa, LUTb);
                        } catch (OutOfMemoryError error) {
                            MipavUtil.displayError("Out of memory: unable to open LUT frame.");
                        }
                    } else {

                        try {
                            histogramDialog = new JDialogHistogramLUT(this, componentImage, imageA, imageB,
                                                                      componentImage.getRGBTA(),
                                                                      componentImage.getRGBTB());
                        } catch (OutOfMemoryError error) {
                            MipavUtil.displayError("Out of memory: unable to open LUT frame.");
                        }
                    }

                    histogramDialog.histogramLUT(true);
                }
            }
        } else if (command.equals("CheckerBoard")) {

            if (tabbedPane.getSelectedIndex() == 0) {

                if (componentImage.checkerDialog != null) {
                    return;
                }

                componentImage.checkerDialog = new JDialogCheckerBoard(this, componentImage);
            } else {

                if (componentImageA.checkerDialog != null) {
                    componentImageA.checkerDialog = new JDialogCheckerBoard(this, componentImageA);
                }

                if (componentImageB.checkerDialog != null) {
                    componentImageB.checkerDialog = new JDialogCheckerBoard(this, componentImageB);
                }
            }
        } else if (command.equals("MagRegister")) {
            float zoom = 2.0f * componentImage.getZoomX();
            componentImage.setZoom(zoom, zoom);
            validate();
            updateImages(true);
            setTitle();
            // updateFrame(zoom, zoom);
        } else if (command.equals("MagRegisterReference")) {
            float zoom = 2.0f * componentImageA.getZoomX();
            componentImageA.setZoom(zoom, zoom);
            validate();
            updateImages(true);
            setTitle();
        } else if (command.equals("MagRegisterAdjust")) {
            float zoom = 2.0f * componentImageB.getZoomX();
            componentImageB.setZoom(zoom, zoom);
            validate();
            updateImages(true);
            setTitle();
        } else if (command.equals("UnMagRegister")) {
            float zoom = 0.5f * componentImage.getZoomX();
            componentImage.setZoom(zoom, zoom);
            validate();
            updateImages(true);
            setTitle();
            // updateFrame(zoom, zoom);
        } else if (command.equals("UnMagRegisterReference")) {
            float zoom = 0.5f * componentImageA.getZoomX();
            componentImageA.setZoom(zoom, zoom);
            validate();
            updateImages(true);
            setTitle();
        } else if (command.equals("UnMagRegisterAdjust")) {
            float zoom = 0.5f * componentImageB.getZoomX();
            componentImageB.setZoom(zoom, zoom);
            validate();
            updateImages(true);
            setTitle();
        } else if (command.equals("WinRegion")) {

            if (doRegionB) {
                componentImage.setCursorMode(ViewJComponentRegistration.WIN_REGION);
            } else {
                componentImage.setCursorMode(ViewJComponentRegistration.DEFAULT);
                updateImages(true);
            }

            doRegionB = !doRegionB;
        } else if (command.equals("pixelIncrement")) {
            JDialogIncrement dialog = new JDialogIncrement(this, true);
            pixelIncrement = dialog.getIncrement();
            pixelIncrementButton.setText(String.valueOf(pixelIncrement));
        } else if (command.equals("translate")) {
            cwButton.setEnabled(false);
            ccwButton.setEnabled(false);
            upButton.setEnabled(true);
            downButton.setEnabled(true);
            rightButton.setEnabled(true);
            leftButton.setEnabled(true);
            mode = ViewJComponentBase.TRANSLATE;
            componentImage.setCursorMode(ViewJComponentBase.TRANSLATE);
            componentImage.setAdjMark(false);
            componentImage.setRefMark(false);
            componentImage.setCenter(false);
        } else if (command.equals("up")) {

            if (mode == ViewJComponentBase.TRANSLATE) {
                xfrmH.MakeIdentity();
                xfrmH.setTranslate(0, -pixelIncrement * yRes);
                xfrm.Mult(xfrmH, xfrm);

                if (bufferFactor == 1) {
                    transform();
                } else {
                    transformC();
                }
            } else if (mode == ViewJComponentBase.DEFAULT) {
                componentImage.moveVOIPosition(0, Math.round(-pixelIncrement * yRes));
            }
        } else if (command.equals("down")) {

            if (mode == ViewJComponentBase.TRANSLATE) {
                xfrmH.MakeIdentity();
                xfrmH.setTranslate(0, pixelIncrement * yRes);
                xfrm.Mult(xfrmH, xfrm);

                if (bufferFactor == 1) {
                    transform();
                } else {
                    transformC();
                }
            } else if (mode == ViewJComponentBase.DEFAULT) {
                componentImage.moveVOIPosition(0, Math.round(pixelIncrement * yRes));
            }

        } else if (command.equals("right")) {

            if (mode == ViewJComponentBase.TRANSLATE) {
                xfrmH.MakeIdentity();
                xfrmH.setTranslate(pixelIncrement * xRes, 0);
                xfrm.Mult(xfrmH, xfrm);

                if (bufferFactor == 1) {
                    transform();
                } else {
                    transformC();
                }
            } else if (mode == ViewJComponentBase.DEFAULT) {
                componentImage.moveVOIPosition(Math.round(pixelIncrement * xRes), 0);
            }
        } else if (command.equals("left")) {

            if (mode == ViewJComponentBase.TRANSLATE) {
                xfrmH.MakeIdentity();
                xfrmH.setTranslate(-pixelIncrement * xRes, 0);
                xfrm.Mult(xfrmH, xfrm);

                if (bufferFactor == 1) {
                    transform();
                } else {
                    transformC();
                }
            } else if (mode == ViewJComponentBase.DEFAULT) {
                componentImage.moveVOIPosition(Math.round(-pixelIncrement * xRes), 0);
            }
        } else if (command.equals("refMark")) {
            cwButton.setEnabled(false);
            ccwButton.setEnabled(false);
            upButton.setEnabled(true);
            downButton.setEnabled(true);
            rightButton.setEnabled(true);
            leftButton.setEnabled(true);
            mode = ViewJComponentBase.POINT_VOI;
            setActiveImage(IMAGE_A);
            //componentImageA.setCursorMode(ViewJComponentBase.POINT_VOI);
            componentImage.setCenter(false);
            //componentImage.setCursorMode(ViewJComponentRegistration.DEFAULT);
            //componentImageB.setCursorMode(ViewJComponentRegistration.DEFAULT);
            voiManager.selectAllVOIs(false);
            voiManager.setSelectedVOI( componentImageA.getPointVOI(), false, false );
            voiManager.actionPerformed(new ActionEvent(this,0,CustomUIBuilder.PARAM_VOI_POINT.getActionCommand()));
        } else if (command.equals("adjMark")) {
            cwButton.setEnabled(false);
            ccwButton.setEnabled(false);
            upButton.setEnabled(true);
            downButton.setEnabled(true);
            rightButton.setEnabled(true);
            leftButton.setEnabled(true);
            mode = ViewJComponentBase.POINT_VOI;
            setActiveImage(IMAGE_B);
            //componentImageB.setCursorMode(ViewJComponentBase.POINT_VOI);
            componentImage.setCenter(false);
            //componentImage.setCursorMode(ViewJComponentRegistration.DEFAULT);
            //componentImageA.setCursorMode(ViewJComponentRegistration.DEFAULT);
            voiManager.selectAllVOIs(false);
            voiManager.setSelectedVOI( componentImageB.getPointVOI(), false, false );
            voiManager.actionPerformed(new ActionEvent(this,0,CustomUIBuilder.PARAM_VOI_POINT.getActionCommand()));
        } else if (command.equals("refMarkMinus")) {
            setActiveImage(IMAGE_A);
            voiManager.setSelectedVOI( componentImageA.getPointVOI(), false, false );
            voiManager.deleteSelectedVOI(true);
        } else if (command.equals("adjMarkMinus")) {
            setActiveImage(IMAGE_B);
            voiManager.setSelectedVOI( componentImageB.getPointVOI(), false, false );
            voiManager.deleteSelectedVOI(true);
        } else if (command.equals("defaultMode")) {
            adjMarkButton.setSelected(false);
            refMarkButton.setSelected(false);
            translateButton.setSelected(false);
            upButton.setEnabled(false);
            downButton.setEnabled(false);
            rightButton.setEnabled(false);
            leftButton.setEnabled(false);
            cwButton.setEnabled(false);
            ccwButton.setEnabled(false);
            componentImage.setCenter(false);
            //componentImage.setCursorMode(ViewJComponentRegistration.DEFAULT);
            //componentImageA.setCursorMode(ViewJComponentBase.DEFAULT);
            //componentImageB.setCursorMode(ViewJComponentBase.DEFAULT);
            voiManager.actionPerformed(new ActionEvent(this,0,CustomUIBuilder.PARAM_VOI_DEFAULT_POINTER.getActionCommand()));
            setDefaultMode();
        } else if (command.equals("degreeIncrement")) {
            JDialogIncrement dialog = new JDialogIncrement(this, false);
            degreeIncrement = dialog.getIncrement();
            degreeIncrementButton.setText(String.valueOf(degreeIncrement));
        } else if (command.equals("rotate")) {

            if (componentImageB.getCenterPtLocation() == -1) {

                if (componentImageB.getnVOI() == 0) {
                    componentImageB.setCenterPtLocation(0);
                } else {
                    componentImageB.setCenterPtLocation(1);
                }
            }

            upButton.setEnabled(false);
            downButton.setEnabled(false);
            rightButton.setEnabled(false);
            leftButton.setEnabled(false);
            cwButton.setEnabled(true);
            ccwButton.setEnabled(true);
            mode = ViewJComponentBase.ROTATE;
            componentImage.setCursorMode(ViewJComponentBase.ROTATE);
            componentImage.setCenter(true);
            // componentImageB.setRegCenterPtCreated(true);
        } else if (command.equals("cw")) {
            upButton.setEnabled(false);
            downButton.setEnabled(false);
            rightButton.setEnabled(false);
            leftButton.setEnabled(false);
            mode = ViewJComponentBase.ROTATE;
            componentImage.setCursorMode(ViewJComponentBase.ROTATE);
            componentImage.setAdjMark(false);
            componentImage.setRefMark(false);
            componentImage.setCenter(false);
            xfrmH.MakeIdentity();
            xfrmH.setTranslate(xRotation, yRotation);
            xfrmH.setRotate(degreeIncrement);
            xfrmH.setTranslate(-xRotation, -yRotation);
            xfrm.Mult(xfrmH, xfrm);

            if (bufferFactor == 1) {
                transform();
            } else {
                transformC();
            }

        } else if (command.equals("ccw")) {
            upButton.setEnabled(false);
            downButton.setEnabled(false);
            rightButton.setEnabled(false);
            leftButton.setEnabled(false);
            mode = ViewJComponentBase.ROTATE;
            componentImage.setCursorMode(ViewJComponentBase.ROTATE);
            componentImage.setAdjMark(false);
            componentImage.setRefMark(false);
            componentImage.setCenter(false);
            xfrmH.MakeIdentity();
            xfrmH.setTranslate(xRotation, yRotation);
            xfrmH.setRotate(-degreeIncrement);
            xfrmH.setTranslate(-xRotation, -yRotation);
            xfrm.Mult(xfrmH, xfrm);

            if (bufferFactor == 1) {
                transform();
            } else {
                transformC();
            }
        } else if (command.equals("leastSquares")) {

            if (leastSquares()) {
                calculateResiduals();
                updateImages(true);
            }
        } else if (command.equals("tpSpline")) {
            tpSpline();
        } else if (command.equals("resetSlice")) {

            for (i = 0; i < bufferSize; i++) {
                imageBufferB[i] = imageBufferOriginalB[i];
            }

            try {
                imageB.importData(0, imageBufferB, true);
            } catch (IOException error) {
                MipavUtil.displayError("ViewJFrameRegistrationTool: IOException Error on imageB.importData");
            }

            xfrm.MakeIdentity();
            imageB.calcMinMax();
            voiManager.deleteVOIs();
            componentImage.deleteVOIs();
            xRotation = xRes * (image.getExtents()[0] / 2);
            yRotation = yRes * (image.getExtents()[1] / 2);
            defaultModeButton.setSelected(true);
            componentImageA.setCursorMode(ViewJComponentBase.DEFAULT);
            componentImageB.setCursorMode(ViewJComponentBase.DEFAULT);
            componentImage.setCursorMode(ViewJComponentBase.DEFAULT);
            updateImages(true);
        } else if (command.equals("commitSlice")) {

            try {
                imageB.exportData(0, bufferSize, imageBufferB);
                imageB.exportData(0, bufferSize, imageBufferOriginalB);
                secondImage.importData(0, imageBufferB, true);

                secondImage.getMatrix().Mult(xfrm);

                xfrm.MakeIdentity();

                if (lsPerformed && (xfrmBA != null)) {
                    secondImage.getMatrix().Mult(xfrmBA);

                    xfrmBA.MakeIdentity();
                    lsPerformed = false;
                }

                secondImage.notifyImageDisplayListeners(null, true);
            } catch (IOException error) {
                MipavUtil.displayError("ViewJFrameRegistrationTool: IOException Error on exportData - importData sequence");
            }

        } // end of else if (command.equals("commitSlice"))
        else if (command.equals("movesDone")) {
            image.notifyImageDisplayListeners(null, true);
            close();
        } else if (command.equals("help")) {
            //MipavUtil.showHelp("10046");
            MipavUtil.showWebHelp("Registration:_Manual_2D_Series");
        }else if(command.equals("costFunction")) {
        	calculateCostFunctionValues();
        }
    }


    /**
     * Builds 4x4 transformation matrix from R and T T=p2-R*p1.
     *
     * @param   p1  from Match
     * @param   p2  from Match
     * @param   R   matrix from Match
     *
     * @return  transformation matrix
     */
    public TransMatrix buildXfrm(double[] p1, double[] p2, Matrix R) {
        int i, j;

        try {
            double[] T = new double[2]; // translation parameters
            TransMatrix xfrmb = new TransMatrix(3, TransMatrix.TRANSFORM_ANOTHER_DATASET);
            Matrix P1 = new Matrix(2, 1);

            for (i = 0; i < 2; i++) {
                P1.set(i, 0, p1[i]);
            }

            P1 = R.times(P1);

            for (i = 0; i < 2; i++) {
                T[i] = p2[i] - P1.get(i, 0); // T=p2-R*p1
                xfrmb.set(i, 2, T[i]); // set last col of xfrm to T
            }

            xfrmb.setMatrix(0, 1, 0, 1, R.getArray()); // copy R into dimxdim elements of xfrm

            for (j = 0; j < 2; j++) {
                xfrmb.set(2, j, 0); // set last row of xfrm to 0,0,0,1
            }

            xfrmb.set(2, 2, 1.0);

            // xfrm.print();
            userInterface.setDataText("\nTransformation matrix = " + "\n" + xfrmb);
            componentImageB.getActiveImage().setMatrix(xfrmb);

            return xfrmb;
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("buildXfrm: unable to allocate enough memory");

            return null;
        }
    }

    /**
     * Calculates the residuals of the least squares fit.
     */
    public void calculateResiduals() {
        int i, j;
        int numCoords = refMark;
        double[] ptB = new double[2];
        double[] ptA = new double[2];
        double[] ptBT = new double[2];
        double[] residual = new double[numCoords];

        for (j = 0; j < numCoords; j++) {

            for (i = 0; i < 2; i++) {
                ptB[i] = pointSetB[i][j];
                ptA[i] = pointSetA[i][j];
            }

            xfrmBA.transform(ptB, ptBT);

            for (i = 0; i < 2; i++) {
                pointSetBT[i][j] = ptBT[i];
            }

            residual[j] = euclideanDistance(ptA, ptBT);
            userInterface.setDataText("\npoint " + (j + 1) + " residual = " + residual[j] + " mm\n");
        }

        componentImageB.resetAdjustableVOIs(pointSetBT, pointSetB);
    }

    /**
     * Disposes of components and frame.
     */
    public void close() {
        setVisible(false);

        if (image != null) {
            image.removeImageDisplayListener(this);
        }

        if (secondImage != null) {
            secondImage.removeImageDisplayListener(this);
        }

        if (componentImage != null) {
            componentImage.dispose(true);
        }

        componentImage = null;

        if (componentImageA != null) {
            componentImageA.disposeLocal(true);
        }

        componentImageA = null;

        if (componentImageB != null) {
            componentImageB.disposeLocal(true);
        }

        componentImageB = null;

        imageBufferA = null;
        imageBufferB = null;

        pixBufferCompA = null;
        pixBufferCompB = null;

        imageBufferOriginalB = null;
        pixBufferB = null;
        extents = null;
        newExtents = null;

        xfrmD = null;
        xfrm = null;
        xfrmH = null;

        xOrg = null;
        yOrg = null;
        xCoords = null;
        yCoords = null;
        pointSetA = null;
        pointSetB = null;
        pointSetBT = null;
        xfrmBA = null;

        pixBuffer = null;
        scrollPane = null;
        toolBar = null;
        toolBarSep = null;
        controlPanel = null;

        controls = null;

        if (imageA != null) {
            imageA.disposeLocal();
        }

        if (imageB != null) {
            imageB.disposeLocal();
        }

        imageA = imageB = null;

        super.close();

    }

    /**
     * Resizes frame and all components.
     *
     * @param  event  event that triggered function
     */
    public synchronized void componentResized(ComponentEvent event) {

        if ((getSize().width >= (xScreen - 20)) || (getSize().height >= (yScreen - 20))) {
            return;
        }

        validate();
        setTitle();
        updateImages(true);
    }

    /**
     * Calculates the Euclidean distance.
     *
     * @param   ptA  point A
     * @param   ptB  point B
     *
     * @return  the euclidean distatnce between points
     */
    public double euclideanDistance(double[] ptA, double[] ptB) {
        double dist = 0;
        int i;
        double sum = 0;

        if (ptA.length != ptB.length) {
            MipavUtil.displayError("Residual error");
        } else {

            for (i = 0; i < ptA.length; i++) {
                sum += SQR(ptA[i] - ptB[i]);
            }

            dist = Math.sqrt(sum);
        }

        return dist;
    }

    /**
     * Finalizes and closes the window.
     */
    public void finalize() {
        close();

        try {
            super.finalize();
        } catch (Throwable t) { }
    }

    /**
     * Currently unused.
     *
     * @param  event  DOCUMENT ME!
     */
    public void focusGained(FocusEvent event) { }

    /**
     * Currently unused.
     *
     * @param  event  DOCUMENT ME!
     */
    public void focusLost(FocusEvent event) { }

    /**
     * Gets control widgets for frame.
     *
     * @return  controls
     */
    public ViewControlsImage getControls() {
        return controls;
    }

    /**
     * Returns the reference to imageA.
     *
     * @return  image
     */
    public ModelImage getImageA() {

        if (componentImage != null) {
            return componentImage.getImageA();
        } else {
            return null;
        }
    }

    /**
     * Returns the reference to imageB.
     *
     * @return  imageB
     */
    public ModelImage getImageB() {

        if (componentImage != null) {
            return componentImage.getImageB();
        } else {
            return null;
        }
    }

    // ********************************************************************
    // ************************** Item Events *****************************
    // ********************************************************************

    /**
     * sets the flags for the checkboxes.
     *
     * @param  event  event that triggered this function
     */
    public synchronized void itemStateChanged(ItemEvent event) {

        Object source = event.getSource();
        int state = event.getStateChange();

        for (int i = 0; i < toggleArray.length; i++) {

            if ((source == toggleArray[i]) && (state == ItemEvent.SELECTED)) {
                ((JToggleButton) source).setBorderPainted(true);
            } else if ((source == toggleArray[i]) && (state == ItemEvent.DESELECTED)) {
                ((JToggleButton) source).setBorderPainted(false);
            }
        }

        for (int i = 0; i < buttonArray.length; i++) {

            if ((source == buttonArray[i]) && (state == ItemEvent.SELECTED)) {
                ((JButton) source).setBorderPainted(true);
            } else if ((source == buttonArray[i]) && (state == ItemEvent.DESELECTED)) {
                ((JButton) source).setBorderPainted(false);
            }
        }
    }


    /**
     * Removes the menu and controls of the main frame so that a new frame can load the main frame with the proper
     * controls. Currently unused.
     */
    public void removeControls() { }

    /**
     * Sets the active image for drawing VOIs. VOIs are only drawn in the active image. In addition, algorithms are
     * executed on the active window.
     *
     * @param  active  IMAGE_A or IMAGE_B
     */
    public void setActiveImage(int active) {

        if (componentImage != null) {
            componentImage.setActiveImage(active);
        }
    }

    /**
     * Sets the alpha blending of parameter for two image displaying. Currently unused.
     *
     * @param  value  amount [0,100] that is the percentage of Image A to be displayed
     */
    public void setAlphaBlend(int value) { }

    /**
     * Sets the menu and controls (i.e. toolbars) of the main frame! This puts the menus and controls needed to controls
     * the operations of this frame. Different image frames have different menu and controls. Currently unused.
     */
    public void setControls() { }

    /**
     * setDefaultMode.
     */
    public void setDefaultMode() {
        mode = ViewJComponentBase.DEFAULT;
    }

    /**
     * Controls whether or not the images/VOIs of the frame can be modified. Currently unused.
     *
     * @param  flag  if true the image/VOIs can be modified; if false image/VOIs can NOT be modified
     */
    public void setEnabled(boolean flag) { }

    /**
     * Sets the reference to imageB. Currently unused.
     *
     * @param  _imageB  Image to set the frame to
     */
    public void setImageB(ModelImage _imageB) { }

    /**
     * Sets the model LUT for the imageA.
     *
     * @param  LUT  the model LUT
     */
    public void setLUTa(ModelLUT LUT) {
        LUTa = LUT;
        updateImages(LUTa, LUTb, true, -1);
    }

    /**
     * setLUTb - accessor that sets the model LUTb for the imageB.
     *
     * @param  LUT  the model LUT
     */
    public void setLUTb(ModelLUT LUT) {
        LUTb = LUT;
        updateImages(LUTa, LUTb, true, -1);
    }

    /**
     * sets mode.
     *
     * @param  mode  DOCUMENT ME!
     */
    public void setMode(int mode) {
        this.mode = mode;
    }

    /**
     * If doDrag == false, this routine processes movements generated by mouse presses and releases. The mouse press
     * location sets (xStart,yStart) and the mouse release location sets (xFinish,yFinish). If doDrag == true, this
     * routine processes movements generated by mouse press and drag events.
     *
     * @param  xStart   DOCUMENT ME!
     * @param  yStart   DOCUMENT ME!
     * @param  xFinish  DOCUMENT ME!
     * @param  yFinish  DOCUMENT ME!
     */
    public void setMove(float xStart, float yStart, float xFinish, float yFinish) {
        float deltaX, deltaY;
        double theta1, theta2;
        float deltaTheta;

        System.err.println( "setMove" );
        if (mode == ViewJComponentBase.TRANSLATE) {
            deltaX = xFinish - xStart;
            deltaY = yFinish - yStart;
            xfrmH.MakeIdentity();
            xfrmH.setTranslate(deltaX, deltaY);
            xfrm.Mult(xfrmH, xfrm);

            if (bufferFactor == 1) {
                transform();
            } else {
                transformC();
            }
        } // end of if (mode == ViewJComponentBase.TRANSLATE)
        else if (mode == ViewJComponentBase.ROTATE) {
            deltaX = xStart - xRotation;
            deltaY = yStart - yRotation;

            if ((deltaX == 0) && (deltaY == 0)) {
                return;
            } else {
                theta1 = java.lang.Math.atan2((double) deltaX, (double) deltaY);
            }

            deltaX = xFinish - xRotation;
            deltaY = yFinish - yRotation;
            theta2 = java.lang.Math.atan2((double) deltaX, (double) deltaY);
            deltaTheta = (float) ((180.0 / Math.PI) * (theta1 - theta2));
            xfrmH.MakeIdentity();
            xfrmH.setTranslate(xRotation, yRotation);
            xfrmH.setRotate(deltaTheta);
            xfrmH.setTranslate(-xRotation, -yRotation);
            xfrm.Mult(xfrmH, xfrm);

            if (bufferFactor == 1) {
                transform();
            } else {
                transformC();
            }
        } // end of if (mode == ViewJComponentBase.ROTATE)
    }

    /**
     * When switching the active image, take the paintBitmap of the previous active image as the paintBitmap of the new
     * active image.
     *
     * @param  paintBitmapSwitch  if true don't copy the new acitve image mask
     */
    public void setPaintBitmapSwitch(boolean paintBitmapSwitch) { }

    // The following 2 functions set the RGB tables for ARGB images A and B.
    /**
     * Sets the RGB LUT table for ARGB image A.
     *
     * @param  RGBT  the new RGB LUT to be applied to the image
     */
    public void setRGBTA(ModelRGB RGBT) {

        if (componentImage != null) {
            componentImage.setRGBTA(RGBT);
        }
    }

    /**
     * Sets the RGB LUT table for ARGB image B.
     *
     * @param  RGBT  the new RGB LUT to be applied to the image
     */
    public void setRGBTB(ModelRGB RGBT) {

        if (componentImage != null) {
            componentImage.setRGBTB(RGBT);
        }
    }

    /**
     * Sets the coordinates of the point the will be the new image rotation center.
     *
     * @param  xRot  DOCUMENT ME!
     * @param  yRot  DOCUMENT ME!
     */
    public void setRotationCenter(float xRot, float yRot) {
        xRotation = xRot;
        yRotation = yRot;
    }


    /**
     * Not used.
     *
     * @param  slice  DOCUMENT ME!
     */
    public void setSlice(int slice) { }

    /**
     * Not used.
     *
     * @param  slice  DOCUMENT ME!
     */
    public void setTimeSlice(int slice) { }

    /**
     * sets the title of the frame with the image name and magnification.
     */
    public void setTitle() {
        String str = null;

        if (tabbedPane.getSelectedIndex() == 0) {
            str = image.getImageName() + " + " + secondImage.getImageName() + "   " + " M:" +
                  makeString(componentImage.getZoomX(), 2);
        } else {
            str = image.getImageName() + " (M: " + makeString(componentImageA.getZoomX(), 2) + ") + " +
                  secondImage.getImageName() + " (M:" + makeString(componentImageB.getZoomX(), 2) + ")";
        }

        setTitle(str);

        userInterface.setTitle(str);
    }

    /**
     * sets values based on knob along slider and catches the tabbed pane switching events.
     *
     * @param  e  event that triggered this function
     */
    public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();


        // slider for the blending percentages of the reference and adjustable slices
        if (source == alphaSlider) {

            if ((alphaSlider.getValueIsAdjusting() == true) && ((imageSize > (1024 * 1024)) || !doDrag)) {
                return;
            }

            newAlphaBlend = 100 - alphaSlider.getValue();
            // updateFrames(false);
        } else if (source == tabbedPane) {
            actionPerformed(new ActionEvent("DummySource", 0, "defaultMode"));
            setTitle();

            // probably need to put algorithmPerformed stuff back in here

        }

        updateImages(true);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  sX  zoom in the x dimension
     * @param  sY  zoom in the y dimension
     */
    public void updateFrame(float sX, float sY) { }

    /**
     * This methods calls the componentImage's REPAINT method to redraw the screen. The extents on this image have
     * changed, so the extents need to be read in again and menus, panes and slide bars adjusted accordingly.
     *
     * @return  DOCUMENT ME!
     */
    public boolean updateImageExtents() {
        return false;
    }

    /**
     * This methods calls the componentImage's update method to redraw the screen.
     *
     * @return  boolean confirming successful update
     */
    public final boolean updateImages() {
        return updateImages(true);
    }

    /**
     * Calls the componentImage's update method to redraw the screen. Without LUT changes.
     *
     * @param   forceShow  forces show to re import image and calc. java image
     *
     * @return  boolean confirming successful update
     */
    public final boolean updateImages(boolean forceShow) {

        if (componentImage == null) {
            return false;
        }

        if (tabbedPane.getSelectedIndex() == 0) {
            componentImage.setNewAlphaBlend(newAlphaBlend);

            if (componentImage.show(0, 0, null, null, forceShow) == false) {
                return false;
            }
        }

        if ((tabbedPane.getSelectedIndex() == 1) || firstDraw) {
            firstDraw = false;

            if (componentImageB.show(0, 0, null, null, forceShow, -1) == false) {
                return false;
            }

            if (componentImageA.show(0, 0, null, null, forceShow, -1) == false) {
                return false;
            }


        }

        return true;
    }

    /**
     * This methods calls the componentImage's update method to redraw the screen.
     *
     * @param   LUTa        LUT used to update imageA
     * @param   LUTb        LUT used to update imageB
     * @param   forceShow   forces show to re import image and calc. java image
     * @param   interpMode  image interpolation method (Nearest or Smooth)
     *
     * @return  boolean confirming successful update
     */
    public final boolean updateImages(ModelLUT LUTa, ModelLUT LUTb, boolean forceShow, int interpMode) {

        if (componentImage == null) {
            return false;
        }

        if (tabbedPane.getSelectedIndex() == 0) {
            componentImage.setNewAlphaBlend(newAlphaBlend);

            if (componentImage.show(0, 0, LUTa, LUTb, forceShow) == false) {
                return false;
            }
        } else {

            if (componentImageA.show(0, 0, LUTa, null, forceShow, -1) == false) {
                return false;
            }

            if (componentImageB.show(0, 0, LUTb, null, forceShow, -1) == false) {
                return false;
            }
        }

        return true;
    }

    /**
     * unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowActivated(WindowEvent event) { }

    /**
     * unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowClosed(WindowEvent event) { }

    /**
     * Calls dispose.
     *
     * @param  event  event that triggered function
     */
    public void windowClosing(WindowEvent event) {
        this.close();
    }

    /**
     * unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowDeactivated(WindowEvent event) { }

    /**
     * unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowDeiconified(WindowEvent event) { }

    /**
     * unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowIconified(WindowEvent event) { }

    // ************************************************************************
    // **************************** Window Events *****************************
    // ************************************************************************

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowOpened(WindowEvent event) { }

    /**
     * Adds a component to the control panel.
     *
     * @param  c    component
     * @param  gbc  GridBagConstraints
     * @param  x    DOCUMENT ME!
     * @param  y    DOCUMENT ME!
     * @param  w    DOCUMENT ME!
     * @param  h    DOCUMENT ME!
     */
    private void addControlPanel(Component c, GridBagConstraints gbc, int x, int y, int w, int h) {
        gbc.gridx = x;
        gbc.gridy = y;
        gbc.gridwidth = w;
        gbc.gridheight = h;
        controlPanel.add(c, gbc);
    }

    /**
     * Adds a component to the top panel.
     *
     * @param  c    component
     * @param  gbc  GridbagConstraints
     * @param  x    DOCUMENT ME!
     * @param  y    DOCUMENT ME!
     * @param  w    DOCUMENT ME!
     * @param  h    DOCUMENT ME!
     */
    private void addTopPanel(Component c, GridBagConstraints gbc, int x, int y, int w, int h) {
        gbc.gridx = x;
        gbc.gridy = y;
        gbc.gridwidth = w;
        gbc.gridheight = h;
        topPanel.add(c, gbc);
    }

    /**
     * panel that sets the at rest frame number and the desired frames per second.
     */
    private void buildControlPanel() {

        cpGBL = new GridBagLayout();
        cpGBC = new GridBagConstraints();
        cpGBC.fill = GridBagConstraints.BOTH;
        cpGBC.anchor = GridBagConstraints.WEST;
        cpGBC.weightx = 100;
        cpGBC.weighty = 100;
        cpGBC.insets = new Insets(10, 10, 10, 10);
        controlPanel = new JPanel();
        controlPanel.setBounds(10, 100, 500, 120);
        controlPanel.setBorder(new EtchedBorder());
        controlPanel.setLayout(cpGBL);

        cpGBC.fill = GridBagConstraints.BOTH;
        cpGBC.anchor = GridBagConstraints.WEST;

        JLabel labelAlphaBlend = new JLabel("AlphaBlending");
        labelAlphaBlend.setForeground(Color.black);
        labelAlphaBlend.setFont(serif12);
        labelAlphaBlend.setEnabled(true);
        addControlPanel(labelAlphaBlend, cpGBC, 0, 0, 2, 1);

        // Make labels to be used in display in the alpha blending slider
        Hashtable<Integer, JLabel> dictionary = new Hashtable<Integer, JLabel>();
        JLabel label1 = new JLabel("Ref. R");
        label1.setForeground(Color.black);
        label1.setFont(font12);
        dictionary.put(new Integer(0), label1);

        JLabel label2 = new JLabel("0.75R");
        label2.setForeground(Color.black);
        label2.setFont(font12);
        dictionary.put(new Integer(25), label2);

        JLabel label3 = new JLabel("0.5R/A");
        label3.setForeground(Color.black);
        label3.setFont(font12);
        dictionary.put(new Integer(50), label3);

        JLabel label4 = new JLabel("0.75A");
        label4.setForeground(Color.black);
        label4.setFont(font12);
        dictionary.put(new Integer(75), label4);

        JLabel label5 = new JLabel("Adj. A");
        label5.setForeground(Color.black);
        label5.setFont(font12);
        dictionary.put(new Integer(100), label5);

        alphaSlider = new JSlider(JSlider.HORIZONTAL, 0, 100, 50);
        alphaSlider.setValueIsAdjusting(false); // doesn't seem to work
        alphaSlider.setMajorTickSpacing(25);
        alphaSlider.setPaintTicks(true);
        alphaSlider.setPaintLabels(true);
        alphaSlider.setLabelTable(dictionary); // loads the labels made above
        alphaSlider.setValue(50);

        addControlPanel(alphaSlider, cpGBC, 2, 0, 8, 1);
        
        costFunctionTextArea = new JTextArea(15,38);
        costFunctionScrollPane = new JScrollPane(costFunctionTextArea, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        addControlPanel(costFunctionScrollPane, cpGBC, 10, 0, 5, 3);
        
        
        alphaSlider.addChangeListener(this);

    }

    /**
     * this method builds a menu which contains the options Save image as and Close Registration.
     */
    private void buildMenu() {

        Font font12B = MipavUtil.font12B;

        try {
            fileMenu = new JMenu("File");
            helpMenu = new JMenu("Help");
            openingMenuBar = new JMenuBar();
            itemHelp = new JMenuItem("Help");
            itemClose = new JMenuItem("Close Registration");
            showVOIs = new JCheckBoxMenuItem("Show VOIs in blended window");
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameRegistrationTool.buildMenu");

            return;
        }

        fileMenu.setFont(font12B);
        helpMenu.setFont(font12B);

        itemHelp.addActionListener(this);
        itemHelp.setAccelerator(KeyStroke.getKeyStroke('H', Event.CTRL_MASK, false));
        itemHelp.setActionCommand("help");
        itemHelp.setFont(font12B);
        helpMenu.add(itemHelp);

        // fileMenu.addSeparator();

        itemClose.addActionListener(this);
        itemClose.setAccelerator(KeyStroke.getKeyStroke('X', Event.CTRL_MASK, false));
        itemClose.setActionCommand("movesDone");
        itemClose.setFont(font12B);
        fileMenu.add(itemClose);

        showVOIs.setSelected(true);
        showVOIs.addActionListener(this);
        showVOIs.setFont(MipavUtil.font12B);
        showVOIs.setActionCommand("toggleVOIs");
        fileMenu.add(showVOIs);

        openingMenuBar.add(fileMenu);
        openingMenuBar.add(helpMenu);
    }

    /**
     * builds the first registration toolbar.
     *
     * @param   al    ActionListener
     * @param   both  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private JToolBar buildRegistrationToolBar(ActionListener al, boolean both) {

        JToolBar registrationToolBar = new JToolBar();
        registrationToolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        registrationToolBar.setBorder(etchedBorder);

        JButton histoLUTButton = new JButton(MipavUtil.getIcon("histolut.gif"));
        histoLUTButton.addActionListener(al);
        histoLUTButton.setToolTipText("Displays Lookup Table(LUT)");
        histoLUTButton.setActionCommand("DisplayLUT");
        histoLUTButton.setBorderPainted(false);
        histoLUTButton.setRolloverIcon(MipavUtil.getIcon("histolutroll.gif"));
        histoLUTButton.setFocusPainted(false);
        histoLUTButton.addItemListener(this);
        registrationToolBar.add(histoLUTButton);
        buttonArray[0] = histoLUTButton;

        if (image.getType() == ModelStorageBase.ARGB_FLOAT) {
            histoLUTButton.setEnabled(false);
        }

        if (both) {
            JButton checkerBoardButton = new JButton(MipavUtil.getIcon("checker.gif"));
            checkerBoardButton.addActionListener(al);
            checkerBoardButton.setToolTipText("Checker Board A&B");
            checkerBoardButton.setActionCommand("CheckerBoard");
            checkerBoardButton.setBorderPainted(false);
            checkerBoardButton.setRolloverIcon(MipavUtil.getIcon("checkerroll.gif"));
            checkerBoardButton.setFocusPainted(false);
            checkerBoardButton.setEnabled(true);
            registrationToolBar.add(checkerBoardButton);
            buttonArray[11] = checkerBoardButton;

            registrationToolBar.add(makeSeparator());

            JButton zoomInButton = new JButton(MipavUtil.getIcon("zoomin.gif"));
            zoomInButton.addActionListener(al);
            zoomInButton.setToolTipText("Magnify image 2.0x");
            zoomInButton.setActionCommand("MagRegister");
            zoomInButton.setBorderPainted(false);
            zoomInButton.setRolloverIcon(MipavUtil.getIcon("zoominroll.gif"));
            zoomInButton.setFocusPainted(false);
            registrationToolBar.add(zoomInButton);
            buttonArray[1] = zoomInButton;

            JButton zoomOutButton = new JButton(MipavUtil.getIcon("zoomout.gif"));
            zoomOutButton.addActionListener(al);
            zoomOutButton.setToolTipText("Magnify image 0.5x");
            zoomOutButton.setActionCommand("UnMagRegister");
            zoomOutButton.setBorderPainted(false);
            zoomOutButton.setRolloverIcon(MipavUtil.getIcon("zoomoutroll.gif"));
            zoomOutButton.setFocusPainted(false);
            registrationToolBar.add(zoomOutButton);
            buttonArray[2] = zoomOutButton;

            regButton = new JToggleButton(MipavUtil.getIcon("winregion.gif"));

            // regButton.setMargin(new Insets(0, 0, 0, 0));
            regButton.addActionListener(al);
            regButton.setToolTipText("Window region of image B");
            regButton.setActionCommand("WinRegion");
            regButton.setBorderPainted(false);
            regButton.setRolloverEnabled(true);
            regButton.setRolloverIcon(MipavUtil.getIcon("winregionroll.gif"));

            // regButton.setBorder(pressedBorder);
            regButton.setEnabled(true);
            regButton.addItemListener(this);
            regButton.setFocusPainted(false);
            registrationToolBar.add(regButton);
            toggleArray[5] = regButton;
        } else {
            registrationToolBar.add(makeSeparator());

            JButton zoomInReferenceButton = new JButton(MipavUtil.getIcon("zoominreference.gif"));
            zoomInReferenceButton.addActionListener(al);
            zoomInReferenceButton.setToolTipText("Magnify reference image 2.0x");
            zoomInReferenceButton.setActionCommand("MagRegisterReference");
            zoomInReferenceButton.setBorderPainted(false);
            zoomInReferenceButton.setRolloverIcon(MipavUtil.getIcon("zoominreferenceroll.gif"));
            zoomInReferenceButton.setFocusPainted(false);
            registrationToolBar.add(zoomInReferenceButton);

            JButton zoomOutReferenceButton = new JButton(MipavUtil.getIcon("zoomoutreference.gif"));
            zoomOutReferenceButton.addActionListener(al);
            zoomOutReferenceButton.setToolTipText("Magnify reference image 0.5x");
            zoomOutReferenceButton.setActionCommand("UnMagRegisterReference");
            zoomOutReferenceButton.setBorderPainted(false);
            zoomOutReferenceButton.setRolloverIcon(MipavUtil.getIcon("zoomoutreferenceroll.gif"));
            zoomOutReferenceButton.setFocusPainted(false);
            registrationToolBar.add(zoomOutReferenceButton);

            registrationToolBar.add(makeSeparator());

            JButton zoomInAdjustButton = new JButton(MipavUtil.getIcon("zoominadjust.gif"));
            zoomInAdjustButton.addActionListener(al);
            zoomInAdjustButton.setToolTipText("Magnify adjusted image 2.0x");
            zoomInAdjustButton.setActionCommand("MagRegisterAdjust");
            zoomInAdjustButton.setBorderPainted(false);
            zoomInAdjustButton.setRolloverIcon(MipavUtil.getIcon("zoominadjustroll.gif"));
            zoomInAdjustButton.setFocusPainted(false);
            registrationToolBar.add(zoomInAdjustButton);

            JButton zoomOutAdjustButton = new JButton(MipavUtil.getIcon("zoomoutadjust.gif"));
            zoomOutAdjustButton.addActionListener(al);
            zoomOutAdjustButton.setToolTipText("Magnify adjusted image 0.5x");
            zoomOutAdjustButton.setActionCommand("UnMagRegisterAdjust");
            zoomOutAdjustButton.setBorderPainted(false);
            zoomOutAdjustButton.setRolloverIcon(MipavUtil.getIcon("zoomoutadjustroll.gif"));
            zoomOutAdjustButton.setFocusPainted(false);
            registrationToolBar.add(zoomOutAdjustButton);

        }

        registrationToolBar.add(makeSeparator());

        if (!both) {
            JButton leastSquaresButton = new JButton(MipavUtil.getIcon("reglsq.gif"));
            leastSquaresButton.addActionListener(al);
            leastSquaresButton.setToolTipText("Apply least squares alignment");
            leastSquaresButton.setActionCommand("leastSquares");
            leastSquaresButton.setBorderPainted(false);
            leastSquaresButton.setRolloverIcon(MipavUtil.getIcon("reglsqroll.gif"));
            leastSquaresButton.setFocusPainted(false);
            registrationToolBar.add(leastSquaresButton);
            buttonArray[12] = leastSquaresButton;

            JButton tpSplineButton = new JButton(MipavUtil.getIcon("regtsp.gif"));
            tpSplineButton.addActionListener(al);
            tpSplineButton.setToolTipText("Apply thin plate spline alignment");
            tpSplineButton.setActionCommand("tpSpline");
            tpSplineButton.setBorderPainted(false);
            tpSplineButton.setRolloverIcon(MipavUtil.getIcon("regtsproll.gif"));
            tpSplineButton.setFocusPainted(false);
            registrationToolBar.add(tpSplineButton);
            buttonArray[13] = tpSplineButton;

            registrationToolBar.add(makeSeparator());
        }

        JButton resetSliceButton = new JButton(MipavUtil.getIcon("undopaint.gif"));
        resetSliceButton.addActionListener(al);
        resetSliceButton.setToolTipText("Reset image to original state");
        resetSliceButton.setActionCommand("resetSlice");
        resetSliceButton.setBorderPainted(false);
        resetSliceButton.setRolloverIcon(MipavUtil.getIcon("undopaintroll.gif"));
        resetSliceButton.setFocusPainted(false);
        registrationToolBar.add(resetSliceButton);
        buttonArray[14] = resetSliceButton;

        registrationToolBar.add(makeSeparator());


        if (!both) {
            refMarkButton = new JToggleButton(MipavUtil.getIcon("reference.gif"));

            // refMarkButton.setMargin(new Insets(0, 0, 0, 0));
            refMarkButton.addActionListener(al);
            refMarkButton.setToolTipText("Reference slice markers");
            refMarkButton.setActionCommand("refMark");
            refMarkButton.setSelected(false);
            movementGroup.add(refMarkButton);
            refMarkButton.setBorderPainted(false);
            refMarkButton.setRolloverEnabled(true);

            // refMarkButton.setBorder(pressedBorder);
            refMarkButton.setRolloverIcon(MipavUtil.getIcon("referenceroll.gif"));
            refMarkButton.addItemListener(this);
            refMarkButton.setFocusPainted(false);
            registrationToolBar.add(refMarkButton);
            toggleArray[2] = refMarkButton;

            adjMarkButton = new JToggleButton(MipavUtil.getIcon("adjust.gif"));

            // adjMarkButton.setMargin(new Insets(0, 0, 0, 0));
            adjMarkButton.addActionListener(al);
            adjMarkButton.setToolTipText("Adjusted slice markers");
            adjMarkButton.setActionCommand("adjMark");
            adjMarkButton.setSelected(false);
            movementGroup.add(adjMarkButton);
            adjMarkButton.setBorderPainted(false);
            adjMarkButton.setRolloverEnabled(true);

            // adjMarkButton.setBorder(pressedBorder);
            adjMarkButton.setRolloverIcon(MipavUtil.getIcon("adjustroll.gif"));
            adjMarkButton.addItemListener(this);
            adjMarkButton.setFocusPainted(false);
            registrationToolBar.add(adjMarkButton);
            toggleArray[3] = adjMarkButton;
            registrationToolBar.add(makeSeparator());


            refMarkMinusButton = new JButton(MipavUtil.getIcon("referenceminus.gif"));

            // refMarkMinusButton.setMargin(new Insets(0, 0, 0, 0));
            refMarkMinusButton.addActionListener(al);
            refMarkMinusButton.setToolTipText("Delete selected reference slice markers");
            refMarkMinusButton.setActionCommand("refMarkMinus");
            refMarkMinusButton.setSelected(false);
            refMarkMinusButton.setBorderPainted(false);
            refMarkMinusButton.setRolloverEnabled(true);

            // refMarkMinusButton.setBorder(pressedBorder);
            refMarkMinusButton.setRolloverIcon(MipavUtil.getIcon("referencerollminus.gif"));
            refMarkMinusButton.addItemListener(this);
            refMarkMinusButton.setFocusPainted(false);
            registrationToolBar.add(refMarkMinusButton);

            adjMarkMinusButton = new JButton(MipavUtil.getIcon("adjustminus.gif"));

            // adjMarkMinusButton.setMargin(new Insets(0, 0, 0, 0));
            adjMarkMinusButton.addActionListener(al);
            adjMarkMinusButton.setToolTipText("Delete selected adjusted slice markers");
            adjMarkMinusButton.setActionCommand("adjMarkMinus");
            adjMarkMinusButton.setSelected(false);
            adjMarkMinusButton.setBorderPainted(false);
            adjMarkMinusButton.setRolloverEnabled(true);

            // adjMarkMinusButton.setBorder(pressedBorder);
            adjMarkMinusButton.setRolloverIcon(MipavUtil.getIcon("adjustrollminus.gif"));
            adjMarkMinusButton.addItemListener(this);
            adjMarkMinusButton.setFocusPainted(false);
            registrationToolBar.add(adjMarkMinusButton);
            registrationToolBar.add(makeSeparator());

            copyRefToAdjButton = new JButton(MipavUtil.getIcon("refToAdj.gif"));

            // copyRefToAdjButton.setMargin(new Insets(0, 0, 0, 0));
            copyRefToAdjButton.addActionListener(al);
            copyRefToAdjButton.setToolTipText("Copy reference markers to adjusted image");
            copyRefToAdjButton.setActionCommand("copyRefAdj");
            copyRefToAdjButton.setSelected(false);
            copyRefToAdjButton.setBorderPainted(false);
            copyRefToAdjButton.setRolloverEnabled(true);

            // copyRefToAdjButton.setBorder(pressedBorder);
            copyRefToAdjButton.setRolloverIcon(MipavUtil.getIcon("refToAdjRoll.gif"));
            copyRefToAdjButton.addItemListener(this);
            copyRefToAdjButton.setFocusPainted(false);
            registrationToolBar.add(copyRefToAdjButton);
            // registrationToolBar.add(makeSeparator());

            copyAdjToRefButton = new JButton(MipavUtil.getIcon("adjToRef.gif"));

            // copyAdjToRefButton.setMargin(new Insets(0, 0, 0, 0));
            copyAdjToRefButton.addActionListener(al);
            copyAdjToRefButton.setToolTipText("Copy adjusted markers to reference image");
            copyAdjToRefButton.setActionCommand("copyAdjRef");
            copyAdjToRefButton.setSelected(false);
            copyAdjToRefButton.setBorderPainted(false);
            copyAdjToRefButton.setRolloverEnabled(true);

            // copyAdjToRefButton.setBorder(pressedBorder);
            copyAdjToRefButton.setRolloverIcon(MipavUtil.getIcon("adjToRefRoll.gif"));
            copyAdjToRefButton.addItemListener(this);
            copyAdjToRefButton.setFocusPainted(false);
            registrationToolBar.add(copyAdjToRefButton);
            registrationToolBar.add(makeSeparator());

            defaultModeButton = new JToggleButton(MipavUtil.getIcon("pointer.gif"));

            // defaultModeButton.setMargin(new Insets(0, 0, 0, 0));
            defaultModeButton.addActionListener(al);
            defaultModeButton.setToolTipText("Return to default mode");
            defaultModeButton.setActionCommand("defaultMode");
            defaultModeButton.setSelected(true);
            defaultModeButton.setRolloverEnabled(true);
            defaultModeButton.setBorderPainted(false);
            defaultModeButton.setRolloverIcon(MipavUtil.getIcon("pointerroll.gif"));
            defaultModeButton.setFocusPainted(false);
            movementGroup.add(defaultModeButton);
            registrationToolBar.add(defaultModeButton);
            registrationToolBar.add(makeSeparator());
        }

        JButton commitMoveButton = new JButton("Apply");
        commitMoveButton.addActionListener(al);
        commitMoveButton.setToolTipText("Commit change to image");
        commitMoveButton.setActionCommand("commitSlice");
        commitMoveButton.setFont(MipavUtil.font12B);
        commitMoveButton.setMinimumSize(new Dimension(50, 20));
        commitMoveButton.setFocusPainted(false);

        // commitMoveButton.setPreferredSize(MipavUtil.defaultButtonSize);
        commitMoveButton.setBorderPainted(false);

        commitMoveButton.setMargin(new Insets(7, 7, 7, 7));

        // commitMoveButton.setMargin(new Insets(0, 0, 0, 0));
        commitMoveButton.addItemListener(this);
        registrationToolBar.add(commitMoveButton);

        registrationToolBar.add(makeSeparator());

        registrationToolBar.setFloatable(false);

        return registrationToolBar;
    }

    /**
     * Make a scroll frame and puts an image component into it.
     */
    private void buildScrollPanes() {
        JPanel innerPanel, innerPanelA, innerPanelB;


        try {

            innerPanel = new JPanel(new GridBagLayout());
            innerPanel.setBackground(Color.gray);

            innerPanelA = new JPanel(new GridBagLayout());
            innerPanelA.setBackground(Color.gray);
            // innerPanelA.setBorder(new LineBorder(Color.red, 3));

            innerPanelB = new JPanel(new GridBagLayout());
            innerPanelB.setBackground(Color.gray);
            // innerPanelB.setBorder(new LineBorder(Color.green, 5));

            scrollPaneSize = 350;

            image.setImageOrder(ModelImage.IMAGE_A);
            extents = new int[2];
            extents[0] = Math.round(image.getExtents()[0]);
            extents[1] = Math.round(image.getExtents()[1]);

            pixBuffer = new int[extents[0] * extents[1]];
            pixBufferB = new int[extents[0] * extents[1]];

            pixBufferCompA = new int[extents[0] * extents[1]];
            pixBufferCompB = new int[extents[0] * extents[1]];

            logMagDisplay = imageA.getLogMagDisplay();

            // Create the componentImage to be displayed in the blended window (with alpha sliding)
            componentImage = new ViewJComponentRegistration(this, imageA, LUTa, imageBufferA, imageB, LUTb,
                                                            imageBufferB, pixBuffer, 1, newExtents, logMagDisplay,
                                                            FileInfoBase.UNKNOWN_ORIENT, newAlphaBlend);

            if (imageA.isColorImage()) {
                componentImage.setRGBTA(RGBa);
                componentImage.setRGBTB(RGBb);
            }

            componentImage.setBuffers(imageBufferA, imageBufferB, pixBuffer, pixBufferB);

            // Create the componentImageA which will be the reference image in the dual window
            componentImageA = new ViewJComponentSingleRegistration(this, imageA, LUTa, imageBufferA, pixBufferCompA, 1,
                                                                   newExtents, logMagDisplay,
                                                                   FileInfoBase.UNKNOWN_ORIENT, true);

            componentImageA.setBuffers(imageBufferA, null, pixBufferCompA, null);

            // Create the componentImageB which will be the adjusted image in the dual window
            componentImageB = new ViewJComponentSingleRegistration(this, imageB, LUTb, imageBufferB, pixBufferCompB, 1,
                                                                   newExtents, logMagDisplay,
                                                                   FileInfoBase.UNKNOWN_ORIENT, false);

            componentImageB.setBuffers(imageBufferB, null, pixBufferCompB, null);

            if (imageA.isColorImage()) {
                componentImage.setRGBTA(RGBa);
                componentImage.setRGBTB(RGBb);
                componentImageA.setRGBTA(RGBa);
                componentImageB.setRGBTB(RGBb);
            }

            xRes = image.getResolutions(0)[0];
            yRes = image.getResolutions(0)[1];

            if ((xRes <= 0.0f) || (yRes <= 0.0f)) {
                xRes = 1.0f;
                yRes = 1.0f;
            }

            if (yRes >= xRes) {
                componentImage.setResolutions(1, yRes / xRes);
                componentImageA.setResolutions(1, yRes / xRes);
                componentImageB.setResolutions(1, yRes / xRes);
                yRes = yRes / xRes;
                xRes = 1.0f;
            } else {
                componentImage.setResolutions(xRes / yRes, 1);
                componentImageA.setResolutions(xRes / yRes, 1);
                componentImageB.setResolutions(xRes / yRes, 1);
                xRes = xRes / yRes;
                yRes = 1.0f;
            }

            xRotation = xRes * (image.getExtents()[0] / 2);
            yRotation = yRes * (image.getExtents()[1] / 2);
            componentImage.setShowSliceNumber(false);

            componentImageA.setShowSliceNumber(false);
            componentImageB.setShowSliceNumber(false);

            GridBagConstraints gbcIP = new GridBagConstraints();
            gbcIP.gridx = 0;
            gbcIP.gridy = 0;
            gbcIP.gridwidth = 1;
            gbcIP.gridheight = 1;
            gbcIP.fill = GridBagConstraints.NONE;
            gbcIP.anchor = GridBagConstraints.CENTER;
            gbcIP.weightx = 0;
            gbcIP.weighty = 0;
            innerPanel.add(componentImage, gbcIP);
            innerPanelA.add(componentImageA, gbcIP);
            innerPanelB.add(componentImageB, gbcIP);
        } catch (OutOfMemoryError e) {
            e.printStackTrace();
            throw (e);
        }

        // The blended component image will be displayed in this scrollpane
        scrollPane = new JScrollPane(innerPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                     JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        scrollPane.setBounds(0, 0, (scrollPaneSize * 2) + 3, scrollPaneSize + 3);

        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.BOTH;
        bothPanel.add(scrollPane, gbc);

        // the reference image will be displayed in this scrollpane
        scrollPaneSeparateA = new JScrollPane(innerPanelA, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                              JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        scrollPaneSeparateA.setBounds(0, 0, scrollPaneSize + 3, scrollPaneSize + 3);
        scrollPaneSeparateA.setBorder(new LineBorder(new Color(165, 0, 0), 5));


        separatePanel.add(scrollPaneSeparateA, gbc);

        // the adjusted image will be displayed in this scrollpane
        scrollPaneSeparateB = new JScrollPane(innerPanelB, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                              JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollPaneSeparateB.setBounds(0, 0, scrollPaneSize + 3, scrollPaneSize + 3);
        scrollPaneSeparateB.setBorder(new LineBorder(new Color(0, 165, 0), 5));

        gbc.gridx = 1;
        gbc.gridy = 1;

        separatePanel.add(scrollPaneSeparateB, gbc);

        scrollPane.setBackground(Color.black);
        scrollPane.setVisible(true);
        scrollPane.validate();

        scrollPaneSeparateA.setBackground(Color.black);
        scrollPaneSeparateA.setVisible(true);
        scrollPaneSeparateA.validate();

        scrollPaneSeparateB.setBackground(Color.black);
        scrollPaneSeparateB.setVisible(true);
        scrollPaneSeparateB.validate();

        // MUST register frame to image models
        imageA.addImageDisplayListener(this);
        imageB.addImageDisplayListener(this);
        pack();
    }

    /**
     * builds the second registration toolbar.
     *
     * @param   al  ActionListener
     *
     * @return  DOCUMENT ME!
     */
    private JToolBar buildToolBar2(ActionListener al) {

        JToolBar toolBar2 = new JToolBar();
        toolBar2.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        movementGroup = new ButtonGroup();
        toolBar2.setBorder(etchedBorder);

        pixelIncrementButton = new JButton(String.valueOf(pixelIncrement));
        pixelIncrementButton.addActionListener(al);
        pixelIncrementButton.setToolTipText("Set pixel increment");
        pixelIncrementButton.setFont(MipavUtil.font12B);

        // pixelIncrementButton.setMinimumSize(new Dimension(20, 20));
        pixelIncrementButton.setMargin(new Insets(7, 7, 7, 7));
        pixelIncrementButton.setActionCommand("pixelIncrement");
        pixelIncrementButton.setBorderPainted(false);
        pixelIncrementButton.setRolloverEnabled(true);
        pixelIncrementButton.addItemListener(this);
        pixelIncrementButton.setFocusPainted(false);
        toolBar2.add(pixelIncrementButton);
        buttonArray[3] = pixelIncrementButton;

        translateButton = new JToggleButton(MipavUtil.getIcon("translate.gif"));

        // translateButton.setMargin(new Insets(0, 0, 0, 0));
        translateButton.addActionListener(al);
        translateButton.setToolTipText("Translate image");
        translateButton.setActionCommand("translate");
        translateButton.setSelected(false);
        movementGroup.add(translateButton);
        translateButton.setBorderPainted(false);
        translateButton.setRolloverEnabled(true);

        // translateButton.setBorder(pressedBorder);
        translateButton.setRolloverIcon(MipavUtil.getIcon("translateroll.gif"));
        translateButton.addItemListener(this);
        translateButton.setFocusPainted(false);
        toolBar2.add(translateButton);
        toggleArray[1] = translateButton;

        upButton = new JButton(MipavUtil.getIcon("up.gif"));
        upButton.addActionListener(al);
        upButton.setToolTipText("Move image up");
        upButton.setActionCommand("up");
        upButton.setEnabled(false);
        upButton.setBorderPainted(false);
        upButton.setRolloverEnabled(true);
        upButton.setRolloverIcon(MipavUtil.getIcon("uproll.gif"));
        upButton.addItemListener(this);
        upButton.setFocusPainted(false);
        toolBar2.add(upButton);
        buttonArray[4] = upButton;

        downButton = new JButton(MipavUtil.getIcon("down.gif"));
        downButton.addActionListener(al);
        downButton.setToolTipText("Move image down");
        downButton.setActionCommand("down");
        downButton.setEnabled(false);
        downButton.setBorderPainted(false);
        downButton.setRolloverEnabled(true);
        downButton.setRolloverIcon(MipavUtil.getIcon("downroll.gif"));
        downButton.addItemListener(this);
        downButton.setFocusPainted(false);
        toolBar2.add(downButton);
        buttonArray[5] = downButton;

        rightButton = new JButton(MipavUtil.getIcon("rightarrow.gif"));
        rightButton.addActionListener(al);
        rightButton.setToolTipText("Move image right");
        rightButton.setActionCommand("right");
        rightButton.setEnabled(false);
        rightButton.setBorderPainted(false);
        rightButton.setRolloverEnabled(true);
        rightButton.setRolloverIcon(MipavUtil.getIcon("rightarrowroll.gif"));
        rightButton.addItemListener(this);
        rightButton.setFocusPainted(false);
        toolBar2.add(rightButton);
        buttonArray[6] = rightButton;

        leftButton = new JButton(MipavUtil.getIcon("leftarrow.gif"));
        leftButton.addActionListener(al);
        leftButton.setToolTipText("Move image left");
        leftButton.setActionCommand("left");
        leftButton.setEnabled(false);
        leftButton.setBorderPainted(false);
        leftButton.setRolloverEnabled(true);
        leftButton.setRolloverIcon(MipavUtil.getIcon("leftarrowroll.gif"));
        leftButton.addItemListener(this);
        leftButton.setFocusPainted(false);
        toolBar2.add(leftButton);
        buttonArray[7] = leftButton;

        toolBar2.add(makeSeparator());

        degreeIncrementButton = new JButton(String.valueOf(degreeIncrement));
        degreeIncrementButton.addActionListener(al);
        degreeIncrementButton.setToolTipText("Set degree increment");
        degreeIncrementButton.setFont(MipavUtil.font12B);

        // degreeIncrementButton.setMinimumSize(new Dimension(20, 20));
        degreeIncrementButton.setMargin(new Insets(7, 7, 7, 7));
        degreeIncrementButton.setActionCommand("degreeIncrement");
        degreeIncrementButton.setBorderPainted(false);
        degreeIncrementButton.setRolloverEnabled(true);

        // degreeIncrementButton.setRolloverIcon(MipavUtil.getIcon("degreeIncrementrollover.gif"));
        degreeIncrementButton.addItemListener(this);
        degreeIncrementButton.setFocusPainted(false);
        toolBar2.add(degreeIncrementButton);
        buttonArray[8] = degreeIncrementButton;

        rotateButton = new JToggleButton(MipavUtil.getIcon("rotate.gif"));

        // rotateButton.setMargin(new Insets(0, 0, 0, 0));
        rotateButton.addActionListener(al);
        rotateButton.setToolTipText("Rotate image");
        rotateButton.setActionCommand("rotate");
        rotateButton.setSelected(false);
        movementGroup.add(rotateButton);
        rotateButton.setBorderPainted(false);
        rotateButton.setRolloverEnabled(true);

        // rotateButton.setBorder(pressedBorder);
        rotateButton.setRolloverIcon(MipavUtil.getIcon("rotateroll.gif"));
        rotateButton.addItemListener(this);
        rotateButton.setFocusPainted(false);
        toolBar2.add(rotateButton);
        toggleArray[4] = rotateButton;

        cwButton = new JButton(MipavUtil.getIcon("clockwise.gif"));
        cwButton.addActionListener(al);
        cwButton.setToolTipText("Rotate clockwise");
        cwButton.setActionCommand("cw");
        cwButton.setEnabled(false);
        cwButton.setBorderPainted(false);
        cwButton.setRolloverEnabled(true);
        cwButton.setRolloverIcon(MipavUtil.getIcon("clockwiseroll.gif"));
        cwButton.addItemListener(this);
        cwButton.setFocusPainted(false);
        toolBar2.add(cwButton);
        buttonArray[9] = cwButton;

        ccwButton = new JButton(MipavUtil.getIcon("counterclockwise.gif"));
        ccwButton.addActionListener(al);
        ccwButton.setToolTipText("Rotate counterclockwise");
        ccwButton.setActionCommand("ccw");
        ccwButton.setEnabled(false);
        ccwButton.setBorderPainted(false);
        ccwButton.setRolloverEnabled(true);
        ccwButton.setRolloverIcon(MipavUtil.getIcon("counterclockwiseroll.gif"));
        ccwButton.addItemListener(this);
        ccwButton.setFocusPainted(false);
        toolBar2.add(ccwButton);
        buttonArray[10] = ccwButton;

        toolBar2.add(makeSeparator());
        
        calculateCostButton = new JButton("Cost Function");
        calculateCostButton.addActionListener(al);
        calculateCostButton.setToolTipText("Calulate Cost Function");
        calculateCostButton.setFont(MipavUtil.font12B);

        // degreeIncrementButton.setMinimumSize(new Dimension(20, 20));
        calculateCostButton.setMargin(new Insets(7, 7, 7, 7));
        calculateCostButton.setActionCommand("costFunction");
        calculateCostButton.setBorderPainted(false);
        calculateCostButton.setRolloverEnabled(true);
        toolBar2.add(calculateCostButton);

        toolBar2.setFloatable(false);

        return toolBar2;
    }

    /**
     * Creates the GUI.
     */
    private void init() {

        bothPanel = new JPanel(new GridBagLayout());
        separatePanel = new JPanel(new GridBagLayout());
        tabbedPane = new JTabbedPane();
        tabbedPane.setFont(MipavUtil.font12B);

        buildMenu();
        setJMenuBar(openingMenuBar);

        xScreen = Toolkit.getDefaultToolkit().getScreenSize().width;
        yScreen = Toolkit.getDefaultToolkit().getScreenSize().height;

        xDim = image.getExtents()[0];
        yDim = image.getExtents()[1];

        imageSize = xDim * yDim;

        // if not a color image and LUTa is null then make a LUT
        if (image.isColorImage() == false) {
            int[] dimExtentsLUT = new int[2];
            dimExtentsLUT[0] = 4;
            dimExtentsLUT[1] = 256;

            if (LUTa == null) {
                LUTa = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);

                float min, max;

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

            if (LUTb == null) {
                LUTb = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);

                float min, max;

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
        } else {

            // images are color... need to see if lutRGBa and lutRGBb are null
            int[] RGBExtents = new int[2];
            RGBExtents[0] = 4;
            RGBExtents[1] = 256;

            if (RGBa == null) {
                RGBa = new ModelRGB(RGBExtents);
            }

            if (RGBb == null) {
                RGBb = new ModelRGB(RGBExtents);
            }
        }

        gbc = new GridBagConstraints();

        /* First is the toolbar.
         * The tool bar can grow horizontally, but it cannot grow vertically. The tool bar is given a minimum width of
         * 450 - without this  minimum width the text fields for pixel and degree increment become unusable. */

        toolBar = buildRegistrationToolBar(this, true);
        toolBar2 = buildToolBar2(this);

        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        gbc.weighty = 0;

        buildControlPanel();

        topPanel = new JPanel(new GridBagLayout());

        /* The control panel can also grow horizontally but not vertically. */
        addTopPanel(controlPanel, gbc, 0, 0, 1, 1);
        addTopPanel(toolBar, gbc, 0, 1, 1, 1);
        addTopPanel(toolBar2, gbc, 0, 2, 1, 1);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        bothPanel.add(topPanel, gbc);

        toolBarSep = buildRegistrationToolBar(this, false);
        toolBar.setMinimumSize(toolBarSep.getPreferredSize());

        gbc.gridwidth = 2;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.weighty = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        separatePanel.add(toolBarSep, gbc);

        xfrm = new TransMatrix(3);
        xfrmH = new TransMatrix(3);
        xfrmD = new TransMatrix(3);
        xOrg = new int[40];
        yOrg = new int[40];
        xCoords = new int[40];
        yCoords = new int[40];

        // builds image panel and puts it into a scrollpane
        buildScrollPanes();
        setActiveImage(IMAGE_A);

        /* componentY is added so that the previous software for ViewJFrameImage can be
         * reused.  There the image was resized without a toolbar, controlPanel, ormenubar contributing to the vertical
         * length. */
        //componentY = toolBar.getHeight() + toolBar2.getHeight() + controlPanel.getHeight() + openingMenuBar.getHeight();

        // structureY is the total of all nonimage components in the Y direction
        //structureY = getInsets().top + componentY + getInsets().bottom;
        //setSize((int) Math.round((scrollPaneSize * 2) + 3 + getInsets().left + getInsets().right),
                //(int) Math.round(scrollPaneSize + 3 + structureY));
        
        setSize(1000,1000);

        zoom = Math.min(zoom, (float) (scrollPane.getViewportBorderBounds().width - 1) / (imageA.getExtents()[0] - 1));
        zoom = Math.min(zoom, (float) (scrollPane.getViewportBorderBounds().height - 1) / (imageA.getExtents()[1] - 1));

        for (i = -10; i <= 10; i++) {

            if ((zoom >= Math.pow(2.0, (double) i)) && (zoom < Math.pow(2.0, (double) (i + 1)))) {
                zoom = (float) Math.pow(2.0, (double) i);
            }
        }

        componentImage.setZoom(zoom, zoom);
        componentImageA.setZoom(zoom, zoom);
        componentImageB.setZoom(zoom, zoom);

        setBackground(Color.black);

        // getContentPane().setBackground(Color.black);
        setLocation(100, 100);
        addWindowListener(this);
        addComponentListener(this);

        userInterface.regFrame(this);

        System.gc();
        setResizable(true);

        // add panels to tab pane
        tabbedPane.addTab("Blended", bothPanel);
        tabbedPane.addTab("Dual", separatePanel);
        tabbedPane.addChangeListener(this);
        getContentPane().add(tabbedPane, BorderLayout.CENTER);
        setTitle();
        //pack();
        setVisible(true);
        
        initVOI();
    }

    /**
     * This code comes from matchBtoA() and buildXfrm(double p1[],double p2[], Matrix R) in AlgorithmRegLeastSquares.
     *
     * @return  DOCUMENT ME!
     */
    private boolean leastSquares() {
        Matrix Q1, Q2;
        Matrix H;
        Matrix X;
        Matrix rotateBA;
        double det;

        try {
            curRefMark = 0;
            curAdjMark = 0;
            refMark = componentImageA.getNumPoints();
            adjMark = componentImageB.getNumPoints();


            // System.err.println("Reference marks: " + refMark);
            // System.err.println("Adjusted marks: " + adjMark);

            if (refMark != adjMark) {
                MipavUtil.displayError("Least squares requires equal numbers of reference and adjustable markers");

                return false;
            } else if (refMark < 3) {
                MipavUtil.displayError("At least 3 markers each needed in reference and adjustable");

                return false;
            }

            pointSetA = new double[2][refMark];
            pointSetB = new double[2][adjMark];
            pointSetBT = new double[2][adjMark];

            xCoords = componentImageA.getXCoords();
            yCoords = componentImageA.getYCoords();

            for (n = 0; n < refMark; n++) {
                pointSetA[0][curRefMark] = (double) xCoords[n];
                pointSetA[1][curRefMark] = (double) yCoords[n];

                // System.err.println("PointSetA X: " + pointSetA[0][curRefMark] + " Y: " + pointSetA[1][curRefMark]);
                curRefMark++;
            }

            xCoords = componentImageB.getXCoords();
            yCoords = componentImageB.getYCoords();

            for (n = 0; n < adjMark; n++) {
                pointSetB[0][curAdjMark] = (double) xCoords[n];
                pointSetB[1][curAdjMark] = (double) yCoords[n];

                // System.err.println("PointSetB X: " + pointSetB[0][curAdjMark] + " Y: " + pointSetB[1][curAdjMark]);
                curAdjMark++;
            }

            double[] p1 = new double[2];
            double[] p2 = new double[2];
            double[][] q1 = new double[2][refMark];
            double[][] q2 = new double[2][refMark];

            for (i = 0; i < 2; i++) {
                p1[i] = 0;
                p2[i] = 0;
            }

            for (i = 0; i < 2; i++) {

                for (j = 0; j < refMark; j++) {
                    p1[i] += pointSetB[i][j];
                    p2[i] += pointSetA[i][j];
                }

                p1[i] *= 1 / ((double) refMark);
                p2[i] *= 1 / ((double) refMark);
            }

            for (i = 0; i < 2; i++) {

                for (j = 0; j < refMark; j++) {
                    q1[i][j] = pointSetB[i][j] - p1[i];
                    q2[i][j] = pointSetA[i][j] - p2[i];
                }
            }

            Q1 = new Matrix(q1, 2, refMark);
            Q2 = new Matrix(q2, 2, refMark);
            H = Q1.times(Q2.transpose());

            int m = H.getRowDimension();
            int n = H.getColumnDimension();
            double[][] U = new double[m][n];
            double[][] V = new double[n][n];
            double[] singularValues = new double[Math.min(m+1,n)];

            Singularvalue.decompose( H.getArray(), U, V, singularValues );
            Matrix Vmat = new Matrix(V);
            Matrix Umat = new Matrix(U);
            // X=V*U'
            X = Vmat.times(Umat.transpose());
            det = X.det();
            userInterface.setDataText("\ndet = " + det);

            if ((det >= 0.99) && (det <= 1.01)) {
                rotateBA = X.copy();
                xfrmBA = buildXfrm(p1, p2, rotateBA);
                xfrm.Mult(xfrmBA);

                doneLeastSquares = true;

                if (image.isColorImage()) {
                    transformC();
                } else {
                    transform();
                }

                lsPerformed = true;
            } else if ((det <= -0.99) && (det >= -1.01)) {
                MipavUtil.displayError("Least Squares Failed");

                return false;
            } else {
                MipavUtil.displayError("Least Squares Rounding Problem");

                return false;
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("leastSquares: unable to allocate enough memory");

            return false;
        }

        return true;
    } // end of leastSquares

    /**
     * makes a separator for the use in the toolbars.
     *
     * @return  DOCUMENT ME!
     */
    private JButton makeSeparator() {

        JButton separator = new JButton(MipavUtil.getIcon("separator.gif"));
        separator.setBorderPainted(false);
        separator.setFocusPainted(false);

        return (separator);
    }

    /**
     * Thin-plate registration method.
     */
    private void tpSpline() {
        int i, j, k, n;
        double[] xSource;
        double[] ySource;
        double[] xTar;
        double[] yTar;
        AlgorithmTPSpline spline;
        float[] xWarp;
        float[] yWarp;
        int length;
        float[][] result;
        int pos, yPos, roundX, roundY;
        float value;
        float valueA, valueR, valueG, valueB;
        int X0pos, Y0pos, X1pos, Y1pos;
        float x0, y0, x1, y1;
        float X, Y;
        float tmpa1, tmpa2, tmpa3, tmpa4;
        int tmpb1, tmpb2, tmpb3, tmpb4;
        int index;

        curRefMark = 0;
        curAdjMark = 0;
        refMark = componentImageA.getNumPoints();
        adjMark = componentImageB.getNumPoints();

        if (refMark != adjMark) {
            MipavUtil.displayError("tpSpline requires equal numbers of reference and adjustable markers");

            return;
        } else if (refMark < 3) {
            MipavUtil.displayError("At least 3 markers each needed in reference and adjustable");

            return;
        }

        pointSetA = new double[2][refMark];
        pointSetB = new double[2][adjMark];

        xCoords = componentImageA.getXCoords();
        yCoords = componentImageA.getYCoords();

        for (n = 0; n < refMark; n++) {

            pointSetA[0][curRefMark] = (double) xCoords[n];
            pointSetA[1][curRefMark] = (double) yCoords[n];

            // System.err.println("PointSetA X: " + pointSetA[0][curRefMark] + " Y: " + pointSetA[1][curRefMark]);
            curRefMark++;
        }

        xCoords = componentImageB.getXCoords();
        yCoords = componentImageB.getYCoords();

        for (n = 0; n < adjMark; n++) {
            pointSetB[0][curAdjMark] = (double) xCoords[n];
            pointSetB[1][curAdjMark] = (double) yCoords[n];

            // System.err.println("PointSetB X: " + pointSetB[0][curAdjMark] + " Y: " + pointSetB[1][curAdjMark]);
            curAdjMark++;
        }

        // Calculate the reverse direction to find the values of the grid positions in x',y' space in
        // terms of x, y values in the original space
        xSource = new double[curRefMark];
        ySource = new double[curRefMark];
        xTar = new double[curAdjMark];
        yTar = new double[curAdjMark];

        for (i = 0; i < curRefMark; i++) {
            xSource[i] = pointSetA[0][i];
            ySource[i] = pointSetA[1][i];
        }

        for (i = 0; i < curAdjMark; i++) {
            xTar[i] = pointSetB[0][i];
            yTar[i] = pointSetB[1][i];
        }

        spline = new AlgorithmTPSpline();

        // spline.setSeparateThread(false);
        // 0.0f for no smoothing, with smoothing interpolation is not exact
        spline.setupTPSpline2D(xSource, ySource, xTar, yTar, 0.0f);
        length = xDim * yDim;
        xWarp = new float[length];
        yWarp = new float[length];

        for (i = 0; i < yDim; i++) {
            k = xDim * i;

            for (j = 0; j < xDim; j++) {
                xWarp[j + k] = (float) j;
                yWarp[j + k] = (float) i;
            }
        }

        result = spline.tpSpline2D(xWarp, yWarp);

        if (image.isColorImage() == false) {

            for (i = 0; i < yDim; i++) {
                yPos = i * xDim;

                for (j = 0; j < xDim; j++) {
                    value = 0; // remains zero if transformed out of bounds
                    pos = yPos + j;
                    X = result[0][pos];
                    roundX = (int) (X + 0.5f);

                    if ((X >= 0) && (roundX < xDim)) {
                        Y = result[1][pos];
                        roundY = (int) (Y + 0.5f);

                        if ((Y >= 0) && (roundY < yDim)) {

                            if ((roundX == (xDim - 1)) || (roundY == (yDim - 1))) {

                                // cannot interpolate if last X or last Y
                                value = imageBufferOriginalB[roundX + (xDim * roundY)];
                            } else {

                                // bi-linear interp.
                                // set intensity of i,j to new transformed coordinates if
                                // x,y is within dimensions of image
                                X0pos = (int) (X);
                                Y0pos = (int) (Y) * xDim;
                                x0 = X - X0pos;
                                y0 = Y - (int) (Y);
                                x1 = 1 - x0;
                                y1 = 1 - y0;
                                X1pos = X0pos + 1;
                                Y1pos = Y0pos + xDim;
                                value = (x1 * y1 * imageBufferOriginalB[Y0pos + X0pos]) +
                                        (x0 * y1 * imageBufferOriginalB[Y0pos + X1pos]) +
                                        (x1 * y0 * imageBufferOriginalB[Y1pos + X0pos]) +
                                        (x0 * y0 * imageBufferOriginalB[Y1pos + X1pos]);
                            }
                        } // if ((roundY >= 0) && (roundY < yDim))
                    } // if ((roundX >= 0) && (roundX < xDim))

                    imageBufferB[pos] = value;
                } // for (j = 0; j < xDim; j++)
            } // for (i = 0; i < yDim; i++)
        } // if ( image.isColorImage() == false )
        else { // color

            for (i = 0; i < yDim; i++) {
                yPos = i * xDim;

                for (j = 0; j < xDim; j++) {
                    valueA = 255; // remains 255 always
                    valueR = 0; // R, G, and B remain zero if transformed out of bounds
                    valueG = 0;
                    valueB = 0;
                    pos = yPos + j;
                    X = result[0][pos];
                    roundX = (int) (X + 0.5f);

                    if ((X >= 0) && (roundX < xDim)) {
                        Y = result[1][pos];
                        roundY = (int) (Y + 0.5f);

                        if ((Y >= 0) && (roundY < yDim)) {

                            if ((roundX == (xDim - 1)) || (roundY == (yDim - 1))) {

                                // cannot interpolate if last X or last Y
                                X0pos = roundX;
                                Y0pos = roundY * xDim;
                                valueA = imageBufferOriginalB[4 * (Y0pos + X0pos)];
                                valueR = imageBufferOriginalB[(4 * (Y0pos + X0pos)) + 1];
                                valueG = imageBufferOriginalB[(4 * (Y0pos + X0pos)) + 2];
                                valueB = imageBufferOriginalB[(4 * (Y0pos + X0pos)) + 3];
                            } else {

                                // bi-linear interp.
                                // set intensity of i,j to new transformed coordinates if
                                // x,y is within dimensions of image
                                X0pos = (int) (X);
                                Y0pos = (int) (Y) * xDim;
                                x0 = X - X0pos;
                                y0 = Y - (int) (Y);
                                x1 = 1 - x0;
                                y1 = 1 - y0;
                                X1pos = X0pos + 1;
                                Y1pos = Y0pos + xDim;

                                tmpb1 = 4 * (Y0pos + X0pos);
                                tmpb2 = 4 * (Y0pos + X1pos);
                                tmpb3 = 4 * (Y1pos + X0pos);
                                tmpb4 = 4 * (Y1pos + X1pos);

                                tmpa1 = x1 * y1;
                                tmpa2 = x0 * y1;
                                tmpa3 = x1 * y0;
                                tmpa4 = x0 * y0;

                                // valueA = tmpa1*imageBufferOriginalB[tmpb1] +
                                // tmpa2*imageBufferOriginalB[tmpb2] +
                                // tmpa3*imageBufferOriginalB[tmpb3] +
                                // tmpa4*imageBufferOriginalB[tmpb4];
                                valueR = (tmpa1 * imageBufferOriginalB[tmpb1 + 1]) +
                                         (tmpa2 * imageBufferOriginalB[tmpb2 + 1]) +
                                         (tmpa3 * imageBufferOriginalB[tmpb3 + 1]) +
                                         (tmpa4 * imageBufferOriginalB[tmpb4 + 1]);
                                valueG = (tmpa1 * imageBufferOriginalB[tmpb1 + 2]) +
                                         (tmpa2 * imageBufferOriginalB[tmpb2 + 2]) +
                                         (tmpa3 * imageBufferOriginalB[tmpb3 + 2]) +
                                         (tmpa4 * imageBufferOriginalB[tmpb4 + 2]);
                                valueB = (tmpa1 * imageBufferOriginalB[tmpb1 + 3]) +
                                         (tmpa2 * imageBufferOriginalB[tmpb2 + 3]) +
                                         (tmpa3 * imageBufferOriginalB[tmpb3 + 3]) +
                                         (tmpa4 * imageBufferOriginalB[tmpb4 + 3]);
                            }
                        } // if ((roundY >= 0) && (roundY < yDim))
                    } // if ((roundX >= 0) && (roundX < xDim))

                    index = 4 * pos;
                    imageBufferB[index] = valueA;
                    imageBufferB[index + 1] = valueR;
                    imageBufferB[index + 2] = valueG;
                    imageBufferB[index + 3] = valueB;
                } // for (j = 0; j < xDim; j++)
            } // for (i = 0; i < yDim; i++)
        } // else color

        try {
            imageB.importData(0, imageBufferB, true);
        } catch (IOException error) {
            MipavUtil.displayError("ViewJFrameRegistrationTool: IOException Error on importData into imageB");
        }

        // Move Adjustable Markers to line up with Reference Markers
        componentImageB.resetAdjustableVOIs(pointSetA, pointSetB);

        updateImages(true);
    }

    /**
     * Transforms the image.
     */
    private void transform() {
        // This code comes from matrixtoInverseArray and transformBilinear in AlgorithmTransform.
        // A slice is transformed and resampled using bilinear interpolation.

        float X, Y;
        int x0, y0;

        // float x1,y1;
        float value;

        // float i1, i2;
        float j1, j2;
        int nVOI;
        int n;
        float frm00, frm01, frm02, frm10, frm11, frm12;

        xfrmD.Inverse(xfrm);

        frm00 = xfrmD.Get(0, 0);
        frm01 = xfrmD.Get(0, 1);
        frm02 = xfrmD.Get(0, 2);
        frm10 = xfrmD.Get(1, 0);
        frm11 = xfrmD.Get(1, 1);
        frm12 = xfrmD.Get(1, 2);

        int position;
        float dx, dy, dx1, dy1;
        int iXdim1 = xDim - 1;
        int iYdim1 = yDim - 1;
        float minimum;
        minimum = Float.MAX_VALUE;

        for (i = 0; i < imageBufferOriginalB.length; i++) {

            if (imageBufferOriginalB[i] < minimum) {
                minimum = imageBufferOriginalB[i];
            }
        }

        int index = 0;

        for (j = 0; j < yDim; j++) {
            j1 = (j * frm01) + frm02;
            j2 = (j * frm11) + frm12;

            for (i = 0; i < xDim; i++) {

                // transform i,j,k
                value = minimum; // remains minimum value if voxel is transformed out of bounds
                X = (j1 + (i * frm00));

                if ((X >= 0) && (X < iXdim1)) {
                    Y = (j2 + (i * frm10));

                    if ((Y >= 0) && (Y < iYdim1)) {
                        x0 = (int) X;
                        y0 = (int) Y;

                        dx = X - x0;
                        dy = Y - y0;

                        dx1 = 1 - dx;
                        dy1 = 1 - dy;

                        position = (y0 * xDim) + x0;

                        value = (dy1 *
                                     ((dx1 * imageBufferOriginalB[position]) +
                                          (dx * imageBufferOriginalB[position + 1]))) +
                                (dy *
                                     ((dx1 * imageBufferOriginalB[position + xDim]) +
                                          (dx * imageBufferOriginalB[position + xDim + 1])));

                    } // end if Y in bounds
                } // end if X in bounds

                imageBufferB[index++] = value;
                // transformedImage.set(index++, value);
            } // end for i
        } // end for j

        try {
            imageB.importData(0, imageBufferB, true);
        } catch (IOException error) {
            MipavUtil.displayError("ViewJFrameRegistrationTool: IOException Error on importData into imageB");
        }

        // Note that in transforming the image we have used the inverse of the transformation
        // matrix to get the old point coordinates from the new point coordinates.
        // In moving the VOIs we wish to use the transformation matrix to get the new
        // point coordinates from the old point coordinates
        nVOI = componentImage.getnVOI();

        int[] markerType = null;

        if (nVOI > 0) {
            xOrg = componentImage.getxOrg();
            yOrg = componentImage.getyOrg();
            markerType = componentImage.getMarkerType();

            if (doneLeastSquares) {
                xfrmD.Copy(xfrmBA);
            } else {
                xfrmD.Copy(xfrm);
            }

        } // end of if (nVOI > 0)

        for (n = 0; n < nVOI; n++) {

            if ((markerType[n] != REFMARK) && (markerType[n] != ROTATIONCENTER)) {
                i = Math.round((xOrg[n] * xfrmD.Get(0, 0)) + (yOrg[n] * xfrmD.Get(0, 1)) + xfrmD.Get(0, 2));
                j = Math.round((xOrg[n] * xfrmD.Get(1, 0)) + (yOrg[n] * xfrmD.Get(1, 1)) + xfrmD.Get(1, 2));
                componentImage.moveVOITo(n, i, j);
            } // end of if (markerType[n] != REFMARK)
        } // end of for (n = 0; n < nVOI; n++)

        doneLeastSquares = false;
        updateImages(true);
    } // end of private void transform()

    /**
     * Transforms color image.
     */
    private void transformC() {

        // A version of transform for ARGB and ARGB_FLOAT
        // This code comes from matrixtoInverseArray and transformBilinear in AlgorithmTransform.
        // A slice is transformed and resampled using bilinear interpolation.
        // int roundX, roundY;
        float tmpa1, tmpa2, tmpa3, tmpa4;
        int tmpb1, tmpb2, tmpb3, tmpb4;
        int X0pos, Y0pos;
        int X1pos, Y1pos;
        float j1, j2;
        float X, Y;
        float x0, y0;
        float x1, y1;
        float valueA, valueR, valueG, valueB;

        // float imm,jmm;
        // float i1, i2;
        int index;
        //int nVOI;
        //int n;
        float frm00, frm01, frm02, frm10, frm11, frm12;

        xfrmD.Inverse(xfrm);

        frm00 = xfrmD.Get(0, 0);
        frm01 = xfrmD.Get(0, 1);
        frm02 = xfrmD.Get(0, 2);
        frm10 = xfrmD.Get(1, 0);
        frm11 = xfrmD.Get(1, 1);
        frm12 = xfrmD.Get(1, 2);

        int iXdim1 = xDim - 1;
        int iYdim1 = yDim - 1;

        valueA = 255;

        // int index = 0;
        for (j = 0; j < yDim; j++) {
            j1 = (j * frm01) + frm02;
            j2 = (j * frm11) + frm12;

            for (i = 0; i < xDim; i++) {

                // transform i,j,k
                valueR = 0;
                valueG = 0;
                valueB = 0; // remains zero if voxel is transformed out of bounds
                X = (j1 + (i * frm00));

                if ((X >= 0) && (X < iXdim1)) {
                    Y = (j2 + (i * frm10));

                    if ((Y >= 0) && (Y < iYdim1)) {

                        // set intensity of i,j to new transformed coordinates if
                        // x,y is within dimensions of image
                        x0 = X - (int) X;
                        y0 = Y - (int) Y;
                        x1 = 1 - x0;
                        y1 = 1 - y0;
                        X0pos = (int) X;
                        Y0pos = (int) Y * xDim;
                        X1pos = X0pos + 1;
                        Y1pos = Y0pos + xDim;

                        tmpb1 = 4 * (Y0pos + X0pos);
                        tmpb2 = 4 * (Y0pos + X1pos);
                        tmpb3 = 4 * (Y1pos + X0pos);
                        tmpb4 = 4 * (Y1pos + X1pos);

                        tmpa1 = x1 * y1;
                        tmpa2 = x0 * y1;
                        tmpa3 = x1 * y0;
                        tmpa4 = x0 * y0;

                        // valueA = tmpa1*imageBufferOriginalB[tmpb1] +
                        // tmpa2*imageBufferOriginalB[tmpb2] +
                        // tmpa3*imageBufferOriginalB[tmpb3] +
                        // tmpa4*imageBufferOriginalB[tmpb4];
                        valueR = (tmpa1 * imageBufferOriginalB[tmpb1 + 1]) + (tmpa2 * imageBufferOriginalB[tmpb2 + 1]) +
                                 (tmpa3 * imageBufferOriginalB[tmpb3 + 1]) + (tmpa4 * imageBufferOriginalB[tmpb4 + 1]);
                        valueG = (tmpa1 * imageBufferOriginalB[tmpb1 + 2]) + (tmpa2 * imageBufferOriginalB[tmpb2 + 2]) +
                                 (tmpa3 * imageBufferOriginalB[tmpb3 + 2]) + (tmpa4 * imageBufferOriginalB[tmpb4 + 2]);
                        valueB = (tmpa1 * imageBufferOriginalB[tmpb1 + 3]) + (tmpa2 * imageBufferOriginalB[tmpb2 + 3]) +
                                 (tmpa3 * imageBufferOriginalB[tmpb3 + 3]) + (tmpa4 * imageBufferOriginalB[tmpb4 + 3]);
                    } // end if Y in bounds
                } // end if X in bounds

                index = 4 * (i + (j * xDim));
                imageBufferB[index] = valueA;
                imageBufferB[index + 1] = valueR;
                imageBufferB[index + 2] = valueG;
                imageBufferB[index + 3] = valueB;
            } // end of for (j = 0; j < yDim; j++)
        } // end of for (i = 0; i < xDim; i++)

        try {
            imageB.importData(0, imageBufferB, true);
        } catch (IOException error) {
            MipavUtil.displayError("ViewJFrameRegistrationTool: IOException Error on importData into imageB");
        }

        // Note that in transforming the image we have use the inverse of the transformation
        // matrix to get the old point coordinates from the new point coordinates.
        // In moving the VOIs we wish to use the transformation matrix to get the new
        // point coordinates from the old point coordinates
        

        if (doneLeastSquares) {
            xfrmD.Copy(xfrmBA);
        } else {
            xfrmD.Copy(xfrm);
        }
        
        //nVOI = componentImageB.getNumPoints();        
        //if (nVOI > 0) {
            //xOrg = componentImageB.getxOrg();
            //yOrg = componentImageB.getyOrg();
       // for (n = 0; n < nVOI; n++) {
            //i = Math.round((xOrg[n] * xfrmD.Get(0, 0)) + (yOrg[n] * xfrmD.Get(0, 1)) + xfrmD.Get(0, 2));
            //j = Math.round((xOrg[n] * xfrmD.Get(1, 0)) + (yOrg[n] * xfrmD.Get(1, 1)) + xfrmD.Get(1, 2));
            //componentImageB.moveVOITo(n, i, j);
        //} // end of for (n = 0; n < nVOI; n++)
        //} // end of if (nVOI > 0)

        componentImageB.setRotate(xfrmD);
        doneLeastSquares = false;
        updateImages(true);
    } // end of private void transformC()
    
    
    
    private void calculateCostFunctionValues() {
    	
   	 int bin1 = 256;
        double possibleIntValues1 = imageA.getMax() - imageA.getMin() + 1;

        if (((imageA.getType() == ModelStorageBase.BYTE) || (imageA.getType() == ModelStorageBase.UBYTE) ||
                 (imageA.getType() == ModelStorageBase.SHORT) ||
                 (imageA.getType() == ModelStorageBase.USHORT) ||
                 (imageA.getType() == ModelStorageBase.INTEGER) ||
                 (imageA.getType() == ModelStorageBase.UINTEGER) ||
                 (imageA.getType() == ModelStorageBase.LONG)) && (possibleIntValues1 < 256)) {
            bin1 = (int) Math.round(possibleIntValues1);
        }
        
        
        simpleImg1 = new ModelSimpleImage(imageA.getExtents(), imageA.getFileInfo(0).getResolutions(),
       		 imageA);

        simpleImg2 = new ModelSimpleImage(imageB.getExtents(), imageB.getFileInfo(0).getResolutions(),
                imageB);
        
        
        float smoothSize = 1;
        double cost;
        
        algoCost = new AlgorithmCostFunctions2D(simpleImg1, simpleImg2, AlgorithmCostFunctions2D.CORRELATION_RATIO_SMOOTHED, bin1, smoothSize);
        tMatrix = new TransMatrix(3);
        cost = algoCost.cost(tMatrix);
        costFunctionTextArea.append("Correlation Ratio Smoothed" + ":\t\t" + cost + "\n");
        
        algoCost = new AlgorithmCostFunctions2D(simpleImg1, simpleImg2, AlgorithmCostFunctions2D.MUTUAL_INFORMATION_SMOOTHED, bin1, smoothSize);
        tMatrix = new TransMatrix(3);
        cost = algoCost.cost(tMatrix);
        costFunctionTextArea.append("Mutual Information Smoothed" + ":\t\t" + cost + "\n");
        
        algoCost = new AlgorithmCostFunctions2D(simpleImg1, simpleImg2, AlgorithmCostFunctions2D.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED, bin1, smoothSize);
        tMatrix = new TransMatrix(3);
        cost = algoCost.cost(tMatrix);
        costFunctionTextArea.append("Normalized Mutual Information Smoothed" + ":\t" + cost + "\n");
        
        algoCost = new AlgorithmCostFunctions2D(simpleImg1, simpleImg2, AlgorithmCostFunctions2D.NORMALIZED_XCORRELATION_SMOOTHED, bin1, smoothSize);
        tMatrix = new TransMatrix(3);
        cost = algoCost.cost(tMatrix);
        costFunctionTextArea.append("Normalized Cross Correlation Smoothed" + ":\t" + cost + "\n");
        costFunctionTextArea.append("-------------------------------------------------------------------------------------------------\n");
        
        
        if(algoCost != null) {
       	 algoCost.disposeLocal();
        	 algoCost = null;
        }
   }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Class for the two dialogs for incrementing - the pixel increment dialog and the degree increment dialog.
     */
    private class JDialogIncrement extends JDialogBase {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -5860401659180340533L;

        /** Text field to enter increment. */
        JTextField field;

        /** Increment value. */
        float increment = 1;

        /** Pixel or degree. */
        boolean pixel;

        /**
         * Creates new dialog for incrementing either the pixel or the degree.
         *
         * @param  parent  The parent frame.
         * @param  _pixel  DOCUMENT ME!<code>true</code> means pixel dialog, otherwise degree dialog.
         */
        public JDialogIncrement(Frame parent, boolean _pixel) {
            super(parent, true);
            pixel = _pixel;
            init(_pixel);
        }

        /**
         * DOCUMENT ME!
         *
         * @param  e  DOCUMENT ME!
         */
        public void actionPerformed(ActionEvent e) {
            String command = e.getActionCommand();

            if (command.equals("Apply")) {

                if (pixel == true) {

                    if (testParameter(field.getText(), 0.01, 2048.0)) {
                        increment = Float.valueOf(field.getText()).floatValue();
                        dispose();
                    } else {
                        field.requestFocus();
                        field.selectAll();
                    }
                } else {

                    if (testParameter(field.getText(), 0.01, 360.0)) {
                        increment = Float.valueOf(field.getText()).floatValue();
                        dispose();
                    } else {
                        field.requestFocus();
                        field.selectAll();
                    }
                }
            } else if (command.equals("Cancel")) {
                dispose();
            }
        }

        /**
         * Accessor that returns the new increment value.
         *
         * @return  DOCUMENT ME!
         */
        public float getIncrement() {
            return increment;
        }

        /**
         * Sets the dialog visible in the same location as the parent frame.
         *
         * @param  flag  <code>true</code> means set visible.
         */
        public void setVisible(boolean flag) {
            setLocation(parentFrame.getLocation());
            super.setVisibleStandard(flag);
        }

        /**
         * Initializes GUI based on whether this is the pixel or degree dialog.
         *
         * @param  pixel  <code>true</code> means pixel dialog, otherwise degree dialog.
         */
        private void init(boolean pixel) {
            JLabel label;

            if (pixel == true) {
                label = new JLabel("Pixel Increment (0.01 - 2048.0)");
                super.setTitle("Change pixel increment");
            } else {
                label = new JLabel("Degree Increment (0.01 - 360.0)");
                super.setTitle("Change degree increment");
            }

            label.setFont(MipavUtil.font12);
            label.setForeground(Color.black);

            field = new JTextField(5);
            field.setText("1.0");
            field.setFont(MipavUtil.font12);

            JPanel panel = new JPanel();
            panel.add(label);
            panel.add(field);

            JPanel buttonPanel = new JPanel();
            buildOKButton();
            buildCancelButton();
            OKButton.setText("Apply");
            buttonPanel.add(OKButton);
            buttonPanel.add(cancelButton);
            getContentPane().add(panel);
            getContentPane().add(buttonPanel, BorderLayout.SOUTH);
            pack();
            setVisible(true);

        }
    }


    @Override
    public void PointerActive(boolean bActive) {
        if ( bActive )
        {
            componentImage.setCursorMode(ViewJComponentBase.VOI_3D);
            componentImageA.setCursorMode(ViewJComponentBase.VOI_3D);
            componentImageA.setCursorMode(ViewJComponentBase.VOI_3D);
        }
        else
        {
            componentImage.setCursorMode(ViewJComponentBase.DEFAULT);
            componentImageA.setCursorMode(ViewJComponentBase.DEFAULT);
            componentImageA.setCursorMode(ViewJComponentBase.DEFAULT);
            setDefaultMode();
        }
    }

    @Override
    public Vector3f PropDown(int iActive) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Vector3f PropUp(int iActive) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void create3DVOI(boolean bIntersection) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void enableBoth(boolean bEnable) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public ModelImage getActiveImage() {
        if (componentImage != null) {
            return componentImage.getActiveImage();
        }
        return null;
    }
    
    @Override
    public ModelLUT getActiveLUT() {
        if (componentImage != null) {
            return componentImage.getActiveLUT();
        }
        return null;
    }

    @Override
    public ModelRGB getActiveRGB() {
        if (componentImage != null) {
            return componentImage.getActiveRGB();
        }
        return null;
    }

    @Override
    public Vector3f getCenterPt() {
        return new Vector3f();
    }

    @Override
    public JFrame getFrame() {
        // TODO Auto-generated method stub
        return null;
    }
    @Override
    public void setActiveImage(ModelImage kImage) {
        if ( kImage == imageA )
        {
            setActiveImage( IMAGE_A );
        }
        else
        {
            setActiveImage( IMAGE_B );
        }
    }
    
    public VOIManagerInterface getVOIManager()
    {
        return voiManager;
    }

    @Override
    public void setCenter(Vector3f kCenter) {
        // TODO Auto-generated method stub
        
    }

    /* (non-Javadoc)
     * @see java.awt.Window#setCursor(java.awt.Cursor)
     */
    public void setCursor(Cursor kCursor) {
        componentImage.setCursor(kCursor);
        if ( getActiveImage() == imageA )
        {
            componentImageA.setCursor(kCursor);
        }
        else if ( getActiveImage() == imageB )
        {
            componentImageB.setCursor(kCursor);
        }
    }

    @Override
    public void setModified() {
        updateImages();
    }

    @Override
    public void updateData(boolean bCopyToCPU) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void setPaintMask(BitSet mask) {
        if (componentImage != null) {
            componentImage.setPaintMask(mask);
        }
    }

    @Override
    public void paintToShortMask()
    {
        final ModelImage maskImage = ViewUserInterface.getReference().getRegisteredImageByName(
                componentImage.commitPaintToMask());

        ScriptRecorder.getReference().addLine(
                new ActionPaintToMask(getActiveImage(), maskImage, ActionPaintToMask.MASK_SHORT));
        ProvenanceRecorder.getReference().addLine(
                new ActionPaintToMask(getActiveImage(), maskImage, ActionPaintToMask.MASK_SHORT));
    }
    

    @Override
    public void paintToUbyteMask()
    {
        final ModelImage maskImage = ViewUserInterface.getReference().getRegisteredImageByName(
                componentImage.commitPaintToUbyteMask());

        ScriptRecorder.getReference().addLine(
                new ActionPaintToMask(getActiveImage(), maskImage, ActionPaintToMask.MASK_UBYTE));
        ProvenanceRecorder.getReference().addLine(
                new ActionPaintToMask(getActiveImage(), maskImage, ActionPaintToMask.MASK_UBYTE));
    }

    @Override
    public void maskToPaint()
    {
        imageA.getParentFrame().maskToPaint();
    }
    
    private void initVOI()
    {
        voiManager = new VOIManagerInterface( this, imageA, imageB, 2, false, null );
        voiManager.setPointerButton(defaultModeButton);
        voiManager.getVOIManager(0).init( this, imageA, null,
                componentImageA, componentImageA,
                componentImageA.getOrientation() );
        componentImageA.setVOIManager(voiManager.getVOIManager(0));
        
        voiManager.getVOIManager(1).init( this, imageB, null,
                componentImageB, componentImageB,
                componentImageB.getOrientation() );
        componentImageB.setVOIManager(voiManager.getVOIManager(1));
        
        componentImage.setVOIManager(voiManager.getVOIManager(0));
    }

}
