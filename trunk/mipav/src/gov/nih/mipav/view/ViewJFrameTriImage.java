package gov.nih.mipav.view;

import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;
import java.awt.*;
import java.awt.event.*;
import java.lang.reflect.*;
import java.util.*;
import java.text.DecimalFormat;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;
import gov.nih.mipav.*;
import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.algorithms.*;

/**
 *   This program works with 3D and 4D images.
 *   Makes a frame and puts 3 images into it - an XY image in the upper left quadrant,
 *   a ZY image in the upper right quadrant, and a XZ image in the lower left quadrant.
 *   The XY image always has increasing X going from left to right and increasing Y going
 *   from top to bottom.  If the image orientation is FileInfoBase.UNKNOWN_ORIENT, the ZY
 *   image has Z increasing from left to right and Y increasing from top to bottom.  If
 *   the image orientation is known, the ZY image has Y increasing from left to right and
 *   Z increasing from bottom to top.  The XZ image always has X increasing from left to
 *   right.  If the orientation is unknown, Z increases from top to bottom.  If the
 *   orientation is known, Z increases from bottom to top.  If the image orientation is
 *   known XY has an axial slice, ZY has a sagittal slice, and XZ has a coronal slice.
 *   The lower right quadrant displays the X, Y, and Z coordinates of the point selected.
 *	<p>
 *   There are 2 menus - a file menu and an options menu.  The file menu only has a close
 *   frame command.  The Options menu has a Show Axes command,a Show Crosshairs command,
 *   a show Talairach grid, a show Talairach position command, and a Link to another
 *   image command.
 *	<p>
 *   There are 2 rows of toolbar buttons.  The first row has 15 toolbar buttons:<br>
 *   1.) Traverse image with pressed mouse button.
 *       As you traverse the XY image, the XY image is unchanged.  The X
 *       slice shown in the ZY image changes and the Y coordinate in the ZY image changes.
 *       The Z coordinate in the ZY image remains unchanged.  The Y slice shown in the XZ
 *       image changes and the X coordinate in the XZ image changes.  The Z coordinate shown
 *       in the XZ image remains unchanged.  Analagous operation for traversing the ZY and
 *       XZ slices.<br>
 *   2.) Magnify image 2.0X.  Note that magnification is always a power of 2.<br>
 *   3.) Magnify image 0.5X.<br>
 *   4.) Set image magnification to 1.0X.<br>
 *   5.) Draw a line VOI which can be used for measurement.
 *   6.) Identify center of volume to determine translations.  Centers are moved with mouse
 *       dragging operations.  Plus sign markers appear to show the new center for each image.
 *       Otherwise, operation is similar to the traverse command.  The centers disappear
 *       when traverse mode is entered.<br>
 *   7.) Alignment tool to determine rotations.  Protractors with 2 line segments intersecting
 *       in a common point appear in all 3 images.  By putting a cursor at the tip of a
 *       protractor line segment and pressing the mouse button,
 *       it is possible to shrink, grow, or rotate the line segment.
 *       The thicker line segment is the reference line segment which will always click into
 *       a vertical or horizontal orientation(whichever is closer) when the mouse button is
 *       released.  The thinner line segment can be left in any orientation.  The angle is
 *       measured in degrees by going clockwise from the thick reference segment to the thinner
 *       segment.  When the mouse button is pressed with the cursor on the protractor, the
 *       angle in degrees will appear.  If the cursor is placed over a part of the protractor
 *       other than a nonintersecting line segment end and the mouse button is pressed, then
 *       the protractor can be moved unchanged in size and angle.<br>
 *   8.) Apply rotations and translations.  An apply transformation matrix dialog appears.
 *       Under Destination radio buttons for new image and replace image are present.  Under
 *       interpolation a combo box with trilinear, bspline 3rd order, and bspline 4rth order
 *       are present.  Apply and cancel buttons are present on the bottom of the dialog.
 *       For 4D images apply the same transformation to all time slices.<br>
 *   9.) Add  a non center point to all images.<br>
 *  10.) New VOI.<br>
 *  11.) Delete a non center point from all images.<br>
 *  12.) Bounding cube to set boundaries for cropping.<br>
 *  13.) Crop the image using the bounding cube boundaries.  For 4D images apply the same
 *       cropping to all time slices.<br>
 *  14.) (**REMOVED**)Bring up a dialog for placing markers to create a new image in the AC-PC aligned view.<br>
 *  15.) (**REMOVED**)Bring up a dialog for placing markers to create a new Talairach space image from an
 *       image in the AC-PC aligned view.<br><p>
 *
 *   The second toolbar row is a paint toolbar with 14 buttons:<br>
 *   1.) Draw using a brush.<br>
 *   2.) Pick up a color from an image.<br>
 *   3.) Fills an area with the desired color - the Paint Grow dialog box appears.  The box has
 *       an upper bound slider, a lower bound slider, and text fields for entering maximum size
 *       and maximum distance.  Apply and cancel buttons are present.<br>
 *   4.) Erases a portion of the image.<br>
 *   5.) Erases all paint.<br>
 *   6.) Draw using small size.<br>
 *   7.) Draw using medium size.<br>
 *   8.) Draw using large size.<br>
 *   9.) Change intensity level of the paint - a desired paint intensity dialog box appears.
 *       The box tells the minimum and maximum allowed intensity levels.  The user inputs
 *       inputs the desired intensity level.  Apply and cancel buttons are present.  Pressing
 *       the apply button causes the desired intensity level to be placed in intensityDropper.
 *       The initial desired intensity level is 1.<br>
 *  10.) Change paint color - a pick paint color dialog box appears.  If controls is not null,
 *       the original value of color is given by color = controls.getTools().paintColor;
 *       Otherwise, the original value is taken as new Color(255,0,0).<br>
 *  11.) Change opacity of paint - a paint opacity dialog box appears. If controls is not null,
 *       the original value of opacity is given by OPACITY = controls.getTools().opacity;
 *       Otherwise, the original value is taken as 0.25.  The dialog box has a slider whose
 *       value can be varied from 0 to 1.<br>
 *  12.) Commit - Changes image where painted.  The painted pixels are given values equal to
 *       intensityDropper and the paint is erased.<br>
 *  13.) Undo last paint region created by a mouse release.<br>
 *  14.) Calculate volume of paint.<br><p>
 *
 *  If imageA and imageB are both loaded, an Active Image Panel with 3 radio buttons, image A,
 *  imageB, and both appears.  Commit paint changes and apply rotations and translations may be
 *  selected for imageA, imageB, or both.<p>
 *
 *  With 4D images a slider for the fourth time dimension appears below the second toolbar.
 */
public class ViewJFrameTriImage extends ViewJFrameBase implements ItemListener, ChangeListener, KeyListener,
    MouseListener
{

    /** Dimensions used for Talairach view clipping box - lateral.*/
    public static final float ATLAS_BBOX_LAT = 80.0f;

    /** Dimensions used for Talairach view clipping box - anterior.*/
    public static final float ATLAS_BBOX_ANT = 80.0f;

    /** Dimensions used for Talairach view clipping box - posterior.*/
    public static final float ATLAS_BBOX_POS = 110.0f;

    /** Dimensions used for Talairach view clipping box - inferior.*/
    public static final float ATLAS_BBOX_INF = 55.0f;

    /** Dimensions used for Talairach view clipping box - superior.*/
    public static final float ATLAS_BBOX_SUP = 85.0f;

    /** Dimensions used for Talairach view clipping box - inferior new.
     3/06/96: extra 10 mm for cerebellum .*/
    public static final float ATLAS_BBOX_INF_NEW = 65.0f;

    /** AC to PC dimension. */
    public static final float ATLAS_AC_TO_PC = 23.0f;

    /** Menu items storage. */
    protected ViewMenuBuilder menuObj;

    /** Used to obtain initial paint opacity and color. */
    protected ViewControlsImage controls = null;

    /** Flag for showing the Talairach grid on the component images. */
    protected boolean showTalairachGrid = false;

    /** Flag for showing the Talairach position on the component images. */
    protected boolean showTalairachPosition = false;

    /** if true use cerebellum-adjusted static variable. */
    protected boolean useInfNew;

    protected ViewJComponentTriImage triImage[] = new ViewJComponentTriImage[MAX_TRI_IMAGES];
    protected JScrollPane scrollPane[] = new JScrollPane[MAX_TRI_IMAGES];
    protected JPanel triImagePanel[] = new JPanel[MAX_TRI_IMAGES];

    public static final int AXIAL_A = 0;
    public static final int SAGITTAL_A = 1;
    public static final int CORONAL_A = 2;
    public static final int AXIAL_AB = 3;
    public static final int SAGITTAL_AB = 4;
    public static final int CORONAL_AB = 5;
    public static final int AXIAL_B = 6;
    public static final int SAGITTAL_B = 7;
    public static final int CORONAL_B = 8;

    /** Magnification for image. */
    protected float zoom = 1.0f;

    /**
     * X slice that this image is on.  The X component in the axial (XY) orientation of the image volume.
     * To get the current image volume coords: <code>triImage[XYAB].getVolumePosition(xSlice, ySlice, zSlice);</code>
     */
    protected int sagittalComponentSlice;

    /**
     * Y slice that this image is on.  The Y component in the axial (XY) orientation of the image volume.
     * To get the current image volume coords: <code>triImage[XYAB].getVolumePosition(xSlice, ySlice, zSlice);</code>
     */
    protected int coronalComponentSlice;

    /**
     * Z slice that this image is on.  The Z component in the axial (XY) orientation of the image volume.
     * To get the current image volume coords: <code>triImage[XYAB].getVolumePosition(xSlice, ySlice, zSlice);</code>
     */
    protected int axialComponentSlice;

    /** Time slice that this image is on. */
    protected int tSlice;

    /**
     * The current values of the absolute position labels.
     */
    protected Point3D currentAbsolutePositionLabels;

    /**
     * Label heading for the absolute x, y, z values in the image volume.
     */
    protected JLabel absLabel;

    protected JLabel labelXPos; // Label the absolute x value in the image volume.
    protected JLabel labelYPos; // Label the absolute y value in the image volume.
    protected JLabel labelZPos; // Label the absolute z value in the image volume.

    /**
     * Label heading for the anatomical position x, y, z values in the image volume.
     */
    protected JLabel posLabel;

    /**
     * Label the anatomical position x value in the image volume.
     */
    protected JLabel labelXRef;
    protected JLabel scannerLabelX;

    /**
     * Label the anatomical position y value in the image volume.
     */
    protected JLabel labelYRef;
    protected JLabel scannerLabelY;

    /**
     * Label the anatomical position z value in the image volume.
     */
    protected JLabel labelZRef;
    protected JLabel scannerLabelZ;

    protected JLabel labelXTal; // Label the Talairach position x value in the image volume.
    protected JLabel labelYTal; // Label the Talairach position y value in the image volume.
    protected JLabel labelZTal; // Label the Talairach position z value in the image volume.

    /** Color chooser to use when selecting paint color. */
    protected ViewJColorChooser colorChooser;

    /** Button for traversing the images. */
    protected JToggleButton traverseButton;

    /** Panel that holds the toolbars. */
    protected JPanel panelToolbar = new JPanel();

    /** Button for changing the color of the paint. */
    protected JButton colorPaintButton;

    /** Color of the paint. */
    protected Color color = new Color(225, 0, 0);

    /**
     * The point to become center of transformed image.
     */
    protected int[] volumeCenter = new int[3];

    /**
     * Time dimension of the original image.
     */
    protected int tDim;

    /** 1 for black and white, 4 for color. */
    protected int bufferFactor;

    /** Resolutions of image. */
    protected float[] resols = new float[3];

    /** Units of image - mm, inches, etc. */
    protected int[] units = new int[3];

    /** Extents of image. */
    protected int[] extents;

    /** Opacity of paint. */
    protected float OPACITY = 0.25f;

    /** Slider for 4D images, time dimension. */
    protected JSlider tImageSlider;

    /** Panel for deciding which image is active; appears when 2 images are loaded. */
    protected JPanel panelActiveImage = null;

    /** Radio button for selecting image A as active. */
    protected JRadioButton radioImageA;

    /** Radio button for selecting image B as active. */
    protected JRadioButton radioImageB;

    /** Radio button for selecting both images as active. */
    protected JRadioButton radioImageBoth;
    
    protected JCheckBox chkShowTalairachGrid; // "Show talairach grid" checkbox
    protected JCheckBox chkShowTalairachGridMarkers; // "Show talairach gridmarkers" checkbox

    /** Orientations of the three axes. */
    protected int[] orient = new int[3];

    /** axialOrientation is true if imageConfiguration is originally AXIAL, CORONAL, or SAGITTAL
     It is reordered to an axial with x going from right to left, y going
     from anterior to posterior, and z going from inferior to superior
     if it is not already in this format.
     Note that axis reversal is not required in ANALYZE, NIFTI, and DICOM,
     but it is required in AFNI and MINC.*/
    protected boolean hasOrientation;

    /**
     * Indicates which of the 3 components was last interacted with.
     * @see #AXIAL
     * @see #CORONAL
     * @see #SAGITTAL
     */
    protected int orientation;

    /** Paint tool bar */
    protected JToolBar paintToolBar;

    /** Image control toolbar */
    protected JToolBar imageToolBar;

    /** We want to be able to toggle the bounding box button, but don't want it to be part
     *  of any other button group. The button group can't be programmatically set to "unselected"
     *  after its already been selected. To emulate this behavior, I'm adding an invisible button to the
     *  button group that will be "toggled" on and off when the user clicks the bounding
     *  box button. These two buttons will act as the button group.
     */
    protected static final int NUM_INVISIBLE_BUTTONS = 4;
    protected JToggleButton[] btnInvisible = new JToggleButton[NUM_INVISIBLE_BUTTONS];
    protected boolean centerButtonSelected = false;
    protected ViewJFrameImage parentFrame; // reference to the parent window

    protected JSpinner intensitySpinner; // spinner component for the paint intensity

    public static int zoomMode = ViewJComponentEditImage.EXPONENTIAL;

    /**
     * A list of CoordinateChangeListeners who want to know about changes to the
     * coordinate currently pointed to by the tri-image frame's crosshairs.
     */
    protected Vector coordinateListeners = new Vector();

    // Used to setup the paint spinner
    private double spinnerDefaultValue = 1, spinnerMin = 0, spinnerMax = 255, spinnerStep = 1;

    protected int defaultPreferredHeight = 1000; // the default hight of the window, if it cannot be calculated
    public static final int MAX_TRI_IMAGES = 9; // maximum number of tri-images
    protected boolean oldLayout = Preferences.is(Preferences.PREF_TRIPLANAR_2X2_LAYOUT); // flag to indicated whether or not to use the old tri-planar layout
    protected static final String OLD_LAYOUT = "OldLayout"; // a constant for which to test in the actionPerformed
    protected static final String PANEL_PLUGIN = "PanelPlugin"; // a constant for which to test in the actionPerformed
    protected static final int DEFAULT_OPTIMAL_ZOOM = 256; // constant to determine how many pixels would be optimal for the image to be initially zoomed to

    /** Toolbar builder reference. */
    private ViewToolBarBuilder toolbarBuilder;

    protected Component pluginPanel; // reference to the plug-in panel in the 2x2 (old) layout
    protected JPanel volumePositionPanel; // reference to the volume coordinate panel when it is in the plug-in position
    protected VolumePositionFrame volumePositionFrame; // reference to the volume coordinate frame

    private Point3D boundingBoxPoints[] = new Point3D[8]; // array of points that specify the corners of the bounding box

    // these constants are used to index into the boundingBoxPoints[] array, and represent the
    // corners of the bounding box. relative to the image's ORIGINAL orientation
    public static final int UPPER_LEFT_FRONT = 0;
    public static final int UPPER_RIGHT_FRONT = 1;
    public static final int LOWER_RIGHT_FRONT = 2;
    public static final int LOWER_LEFT_FRONT = 3;
    public static final int UPPER_LEFT_BACK = 4;
    public static final int UPPER_RIGHT_BACK = 5;
    public static final int LOWER_RIGHT_BACK = 6;
    public static final int LOWER_LEFT_BACK = 7;
    
    private JLabel talairachVoxelLabel;
    private JToggleButton addPointToggleButton;
    private JToggleButton dropperPaintToggleButton;
    private JToggleButton paintCanToggleButton;
    private JButton leastSquaresButton;
    private JButton tpSplineButton;

    /**
     *   @deprecated use ViewJFrameTriImage(ModelImage, ModelLUT, ModelImage, ModelLUT, ViewControlsImage, ViewJFrameImage)
     *   Make a frame and puts an image component into it.
     *   @param _imageA        First image to display
     *   @param LUTa           LUT of the imageA (if null grayscale LUT is constructed)
     *   @param _imageB        Second loaded image
     *   @param LUTb           LUT of the imageB
     *   @param ui             main user interface frame.
     *   @param controls       controls used to obtain initial OPACITY and color
     */
    public ViewJFrameTriImage(ModelImage _imageA, ModelLUT LUTa,
                              ModelImage _imageB, ModelLUT LUTb,
                              ViewUserInterface ui,
                              ViewControlsImage controls,
                              ViewJFrameImage parent)
    {
        this(_imageA, LUTa, _imageB, LUTb, controls, parent);
    }

    /**
     *   Make a frame and puts an image component into it.
     *   @param _imageA        First image to display
     *   @param LUTa           LUT of the imageA (if null grayscale LUT is constructed)
     *   @param _imageB        Second loaded image
     *   @param LUTb           LUT of the imageB
     *   @param ui             main user interface frame.
     *   @param controls       controls used to obtain initial OPACITY and color
     */
    public ViewJFrameTriImage(ModelImage _imageA, ModelLUT LUTa,
                              ModelImage _imageB, ModelLUT LUTb,
                              ViewControlsImage controls,
                              ViewJFrameImage parent)
    {
        super(_imageA, _imageB);
        userInterface = ViewUserInterface.getReference();
        this.LUTa = LUTa;
        this.LUTb = LUTb;
        this.controls = controls;
        parentFrame = parent;
        try
        {
            setIconImage(MipavUtil.getIconImage("3plane_16x16.gif"));
        }
        catch (Exception e)
        {}
        init();
    }

    /**
     * Initialize the the image orientations and call the frame layout method 'configureFrame()'.
     */
    protected void init()
    {
        addKeyListener(this);
        if (controls != null)
        {
            OPACITY = controls.getTools().getOpacity();
            color = controls.getTools().getPaintColor();
        }

        int imageOrientation = imageA.getImageOrientation();
        if (imageOrientation == FileInfoBase.UNKNOWN_ORIENT)
        {
            hasOrientation = false;

        }
        else
        {
            hasOrientation = true;

            orient = imageA.getFileInfo()[0].getAxisOrientation();
            if (orient[0] == FileInfoBase.ORI_UNKNOWN_TYPE)
            {
                // set to default values if axis orientation is unknown
                if (imageOrientation == FileInfoBase.AXIAL)
                {
                    orient[0] = FileInfoBase.ORI_R2L_TYPE;
                    orient[1] = FileInfoBase.ORI_A2P_TYPE;
                    orient[2] = FileInfoBase.ORI_I2S_TYPE;
                }
                else if (imageOrientation == FileInfoBase.CORONAL)
                {
                    orient[0] = FileInfoBase.ORI_R2L_TYPE;
                    orient[1] = FileInfoBase.ORI_S2I_TYPE;
                    orient[2] = FileInfoBase.ORI_A2P_TYPE;
                }
                else
                { // imageOrientation == FileInfoBase.SAGITTAL
                    orient[0] = FileInfoBase.ORI_A2P_TYPE;
                    orient[1] = FileInfoBase.ORI_S2I_TYPE;
                    orient[2] = FileInfoBase.ORI_R2L_TYPE;
                }
            } // if (orient[0] == -1)
        }

        imageA.setImageOrder(ModelImage.IMAGE_A);
        if (imageB != null)
        {
            imageB.setImageOrder(ModelImage.IMAGE_B);
        }
        if ( ( (imageA.getNDims() == 3) && (imageB == null))
            || ( (imageA.getNDims() == 3) && (imageB != null) && (imageB.getNDims() == 3)))
        {
            extents = new int[3];
            extents[0] = imageA.getExtents()[0];
            extents[1] = imageA.getExtents()[1];
            extents[2] = imageA.getExtents()[2];
        }
        else
        { // imageA.getNDims() == 4 or imageB.getNDims() == 4
            extents = new int[4];
            extents[0] = imageA.getExtents()[0];
            extents[1] = imageA.getExtents()[1];
            extents[2] = imageA.getExtents()[2];
            if (imageA.getNDims() == 4)
            {
                extents[3] = imageA.getExtents()[3];
            }
            else
            {
                extents[3] = imageB.getExtents()[3];
            }
        }

        bufferFactor = 1;
        if (imageA.isColorImage())
        {
            bufferFactor = 4;
        }

        resols[0] = Math.abs(imageA.getFileInfo()[0].getResolutions()[0]);
        resols[1] = Math.abs(imageA.getFileInfo()[0].getResolutions()[1]);
        resols[2] = Math.abs(imageA.getFileInfo()[0].getResolutions()[2]);

        // if the slice spacing value is greater than the z-res, use the slice spacing instead
        if (resols[2] < imageA.getFileInfo(0).getSliceSpacing())
        {
            resols[2] = imageA.getFileInfo(0).getSliceSpacing();
        }

        if ( (resols[0] == 0.0f) || (resols[1] == 0.0f) || (resols[2] == 0.0f))
        {
            resols[0] = 1.0f;
            resols[1] = 1.0f;
            resols[2] = 1.0f;
        }
        units[0] = imageA.getFileInfo()[0].getUnitsOfMeasure()[0];
        units[1] = imageA.getFileInfo()[0].getUnitsOfMeasure()[1];
        units[2] = imageA.getFileInfo()[0].getUnitsOfMeasure()[2];

        // Talaiarach images must already be in standard dicom order
        if (imageA.getExtents()[2] * resols[2] > (ATLAS_BBOX_INF + ATLAS_BBOX_SUP + 6))
        {
            // Use ATLAS_BBOX_INF_NEW = 65.0f;
            useInfNew = true;
        }
        else
        {
            // Use ATLAS_BBOX_INF = 55.0f;
            useInfNew = false;
        }

        configureFrame();

        // set up the initial values for the frame's labels
        Point3D newLabel = triImage[AXIAL_A].getVolumePosition(sagittalComponentSlice, coronalComponentSlice,
            axialComponentSlice);
        setPositionLabels(newLabel.x, newLabel.y, newLabel.z);
    }

    /**
     * Constructs main frame structures for 3 images (image A only) or 9 images (image A and image B).
     * Assumes imageA is not null. Builds the labels for the position frame. Adds the tri-images to
     * this frame's layout.
     */
    protected void configureFrame()
    {
        if (imageA == null)
        {
            return;
        }

        JMenuBar menuBar = buildMenu();
        setJMenuBar(menuBar);
        buildToolbars();

        setResizable(true);

        buildLabels();

        buildLUTs();

        if (imageB != null)
        {
            triImage[AXIAL_AB] = buildTriImage(imageA, LUTa, imageB, LUTb, ViewJComponentBase.AXIAL);
            triImage[CORONAL_AB] = buildTriImage(imageA, LUTa, imageB, LUTb, ViewJComponentBase.CORONAL);
            triImage[SAGITTAL_AB] = buildTriImage(imageA, LUTa, imageB, LUTb, ViewJComponentBase.SAGITTAL);
            triImage[AXIAL_B] = buildTriImage(imageB, LUTb, null, null, ViewJComponentBase.AXIAL);
            triImage[CORONAL_B] = buildTriImage(imageB, LUTb, null, null, ViewJComponentBase.CORONAL);
            triImage[SAGITTAL_B] = buildTriImage(imageB, LUTb, null, null, ViewJComponentBase.SAGITTAL);
        }

        triImage[AXIAL_A] = buildTriImage(imageA, LUTa, null, null, ViewJComponentBase.AXIAL);
        triImage[SAGITTAL_A] = buildTriImage(imageA, LUTa, null, null, ViewJComponentBase.SAGITTAL);
        triImage[CORONAL_A] = buildTriImage(imageA, LUTa, null, null, ViewJComponentBase.CORONAL);

        updateLayout();

        volumeCenter[0] = (extents[0]) / 2;
        volumeCenter[1] = (extents[1]) / 2;
        volumeCenter[2] = (extents[2]) / 2;

        tSlice = 0;

        setupBoundingBoxPoints();

        zoom = getOptimalZoom(DEFAULT_OPTIMAL_ZOOM, DEFAULT_OPTIMAL_ZOOM);

        // these lines of code set up the initial crosshair position - centered in the image and independent of orientation
        for (int i = 0; i < MAX_TRI_IMAGES; i++)
        {
            if (triImage[i] != null)
            {
                triImage[i].setZoom(zoom, zoom);
                triImage[i].makeCenter();
                triImage[i].setDoCenter(false);

                int axisOrder [] = triImage[i].getAxisOrder();

                int x = volumeCenter[axisOrder[0]];
                x = (int) (x * triImage[i].getZoomX() * triImage[i].getResolutionX());
                int y = volumeCenter[axisOrder[1]];
                y = (int) (y * triImage[i].getZoomY() * triImage[i].getResolutionY());

                triImage[i].updateCrosshairPosition(x, y);
            }
        }

        fireCoordinateChange(sagittalComponentSlice, coronalComponentSlice, axialComponentSlice);

        setTitle();

        //MUST register frame to image models
        imageA.addImageDisplayListener(this);
        if (imageB != null)
        {
            imageB.addImageDisplayListener(this);
        }

        pack(); // DO NOT REMOVE!!!!!!!!!!!!!!!!!!! weird things happen to the layout in certain circumstances
        MipavUtil.centerOnScreen(this);
        setVisible(true);
        updateImages(true);
        setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
    }

    /**
     * Calculates the optimal zoom value for an image based on the two parameters desiredWidth and desiredHeight.
     * Since MIPAV doesn't officially support images with differing zoom values, the zoom value returned will be
     * that value which, when applied to the image, will ensure the image size on screen is no more than both
     * desiredWidth or desiredHeight
     * @param desiredWidth int the ideal width for the image displayed on-screen
     * @param desiredHeight int the idea height for the image displayed on-screen
     * @return float the zoom value required to show the image on-screen at no more than desiredWidth and no
     * more than desired height
     */
    private float getOptimalZoom(int desiredWidth, int desiredHeight)
    {
        int[] axisOrderAxial = triImage[AXIAL_A].getAxisOrder();
        int[] axisOrderSagittal = triImage[SAGITTAL_A].getAxisOrder();
        int[] axisOrderCoronal = triImage[CORONAL_A].getAxisOrder();

        float zoomAxialWidth = desiredWidth /
            (imageA.getExtents()[axisOrderAxial[0]] * triImage[AXIAL_A].getResolutionX());
        float zoomSagittalWidth = desiredWidth /
            (imageA.getExtents()[axisOrderSagittal[0]] * triImage[SAGITTAL_A].getResolutionX());
        float zoomCoronalWidth = desiredWidth /
            (imageA.getExtents()[axisOrderCoronal[0]] * triImage[CORONAL_A].getResolutionX());

        float optimalZoomWidth = Math.min(zoomAxialWidth, Math.min(zoomSagittalWidth, zoomCoronalWidth));

        float zoomAxialHeight = desiredHeight /
            (imageA.getExtents()[axisOrderAxial[1]] * triImage[AXIAL_A].getResolutionY());
        float zoomSagittalHeight = desiredHeight /
            (imageA.getExtents()[axisOrderSagittal[1]] * triImage[SAGITTAL_A].getResolutionY());
        float zoomCoronal = desiredHeight /
            (imageA.getExtents()[axisOrderCoronal[1]] * triImage[CORONAL_A].getResolutionY());

        float optimalZoomHeight = Math.min(zoomAxialHeight, Math.min(zoomSagittalHeight, zoomCoronal));

        return Math.min(optimalZoomWidth, optimalZoomHeight);
    }

    /**
     * Initialized the bounding box array. This is an array of volume space points that represent the 8
     * corners of the tri-planar's bounding box.
     */
    private void setupBoundingBoxPoints()
    {
        boundingBoxPoints[UPPER_LEFT_FRONT] = new Point3D((int) (extents[0] * 0.25f), (int) (extents[1] * 0.25f), (int) (extents[2] * 0.25f));
        boundingBoxPoints[UPPER_RIGHT_FRONT] = new Point3D((int) (extents[0] * 0.75f), (int) (extents[1] * 0.25f), (int) (extents[2] * 0.25f));
        boundingBoxPoints[LOWER_RIGHT_FRONT] = new Point3D((int) (extents[0] * 0.75f), (int) (extents[1] * 0.75f), (int) (extents[2] * 0.25f));
        boundingBoxPoints[LOWER_LEFT_FRONT] = new Point3D((int) (extents[0] * 0.25f), (int) (extents[1] * 0.75f), (int) (extents[2] * 0.25f));
        boundingBoxPoints[UPPER_LEFT_BACK] = new Point3D((int) (extents[0] * 0.25f), (int) (extents[1] * 0.25f), (int) (extents[2] * 0.75f));
        boundingBoxPoints[UPPER_RIGHT_BACK] = new Point3D((int) (extents[0] * 0.75f), (int) (extents[1] * 0.25f), (int) (extents[2] * 0.75f));
        boundingBoxPoints[LOWER_RIGHT_BACK] = new Point3D((int) (extents[0] * 0.75f), (int) (extents[1] * 0.75f), (int) (extents[2] * 0.75f));
        boundingBoxPoints[LOWER_LEFT_BACK] = new Point3D((int) (extents[0] * 0.25f), (int) (extents[1] * 0.75f), (int) (extents[2] * 0.75f));
    }

    /**
     * Gets the array of 3D volume space points that represent the 8 corners of the bounding box
     * @return Point3D[] the array of 3D points
     */
    public Point3D[] getBoundingBoxPoints()
    {
        return boundingBoxPoints;
    }


    /**
     * This method should be called whenever the layout of the tri-images has changed. For example,
     * when image B is removed or added, this method should be called so that the frame can resize
     * itself and properly layout the tri-image components.
     */
    protected void updateLayout()
    {
        if (oldLayout)
        {
            // the old layout does it differently
            doOldLayout();
            return;
        }

        GridBagLayout gbLayout = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.weightx = 0;
        gbc.weighty = 0;

        if (imageB != null)
        {
            triImagePanel[AXIAL_AB] = new JPanel();
            triImagePanel[AXIAL_AB].setLayout(gbLayout);
            triImagePanel[AXIAL_AB].setBackground(Color.black);

            triImagePanel[SAGITTAL_AB] = new JPanel();
            triImagePanel[SAGITTAL_AB].setLayout(gbLayout);
            triImagePanel[SAGITTAL_AB].setBackground(Color.black);

            triImagePanel[CORONAL_AB] = new JPanel();
            triImagePanel[CORONAL_AB].setLayout(gbLayout);
            triImagePanel[CORONAL_AB].setBackground(Color.black);

            triImagePanel[AXIAL_B] = new JPanel();
            triImagePanel[AXIAL_B].setLayout(gbLayout);
            triImagePanel[AXIAL_B].setBackground(Color.black);

            triImagePanel[CORONAL_B] = new JPanel();
            triImagePanel[CORONAL_B].setLayout(gbLayout);
            triImagePanel[CORONAL_B].setBackground(Color.black);

            triImagePanel[SAGITTAL_B] = new JPanel();
            triImagePanel[SAGITTAL_B].setLayout(gbLayout);
            triImagePanel[SAGITTAL_B].setBackground(Color.black);

            triImagePanel[AXIAL_AB].add(triImage[AXIAL_AB], gbc);
            triImagePanel[CORONAL_AB].add(triImage[CORONAL_AB], gbc);
            triImagePanel[SAGITTAL_AB].add(triImage[SAGITTAL_AB], gbc);
            triImagePanel[AXIAL_B].add(triImage[AXIAL_B], gbc);
            triImagePanel[CORONAL_B].add(triImage[CORONAL_B], gbc);
            triImagePanel[SAGITTAL_B].add(triImage[SAGITTAL_B], gbc);

            scrollPane[AXIAL_AB] = new JScrollPane(triImagePanel[AXIAL_AB], JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            scrollPane[SAGITTAL_AB] = new JScrollPane(triImagePanel[SAGITTAL_AB],
                JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            scrollPane[CORONAL_AB] = new JScrollPane(triImagePanel[CORONAL_AB],
                JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            scrollPane[AXIAL_B] = new JScrollPane(triImagePanel[AXIAL_B], JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                                  JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            scrollPane[SAGITTAL_B] = new JScrollPane(triImagePanel[SAGITTAL_B],
                JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            scrollPane[CORONAL_B] = new JScrollPane(triImagePanel[CORONAL_B], JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        }

        triImagePanel[AXIAL_A] = new JPanel();
        triImagePanel[AXIAL_A].setLayout(gbLayout);
        triImagePanel[AXIAL_A].setBackground(Color.black);

        triImagePanel[CORONAL_A] = new JPanel();
        triImagePanel[CORONAL_A].setLayout(gbLayout);
        triImagePanel[CORONAL_A].setBackground(Color.black);

        triImagePanel[SAGITTAL_A] = new JPanel();
        triImagePanel[SAGITTAL_A].setLayout(gbLayout);
        triImagePanel[SAGITTAL_A].setBackground(Color.black);

        triImagePanel[AXIAL_A].add(triImage[AXIAL_A], gbc);
        triImagePanel[CORONAL_A].add(triImage[CORONAL_A], gbc);
        triImagePanel[SAGITTAL_A].add(triImage[SAGITTAL_A], gbc);

        scrollPane[AXIAL_A] = new JScrollPane(triImagePanel[AXIAL_A], JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                              JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollPane[SAGITTAL_A] = new JScrollPane(triImagePanel[SAGITTAL_A], JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                                 JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollPane[CORONAL_A] = new JScrollPane(triImagePanel[CORONAL_A], JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        volumePositionPanel = buildVolumePositionPanel();

        getContentPane().removeAll();
        getContentPane().add(panelToolbar, BorderLayout.NORTH);

        JPanel panelA = new JPanel(new GridLayout(1, 3, 10, 10));

        if (imageB == null)
        {
            JSplitPane splitPane1 = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, scrollPane[AXIAL_A],
                scrollPane[SAGITTAL_A]);
            splitPane1.setDividerLocation(0.5);
            splitPane1.setOneTouchExpandable(true);
            JSplitPane splitPane2 = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, splitPane1, scrollPane[CORONAL_A]);
            splitPane2.setDividerLocation(0.67);
            splitPane2.setOneTouchExpandable(true);
            panelA.add(splitPane2);
            getContentPane().add(panelA, BorderLayout.CENTER);
            pack();
        }
        else
        {
            JPanel panelAB = new JPanel(new GridLayout(1, 3, 10, 10));
            JPanel panelB = new JPanel(new GridLayout(1, 3, 10, 10));

            panelAB.add(scrollPane[AXIAL_AB]);
            panelAB.add(scrollPane[SAGITTAL_AB]);
            panelAB.add(scrollPane[CORONAL_AB]);
            panelA.add(scrollPane[AXIAL_A]);
            panelA.add(scrollPane[SAGITTAL_A]);
            panelA.add(scrollPane[CORONAL_A]);
            panelB.add(scrollPane[AXIAL_B]);
            panelB.add(scrollPane[SAGITTAL_B]);
            panelB.add(scrollPane[CORONAL_B]);

            JSplitPane splitPane1 = new JSplitPane(JSplitPane.VERTICAL_SPLIT, panelAB, panelA);
            JSplitPane splitPane2 = new JSplitPane(JSplitPane.VERTICAL_SPLIT, splitPane1, panelB);
            getContentPane().add(splitPane2, BorderLayout.CENTER);

            setSize(getSize().width, defaultPreferredHeight);
            pack();
            splitPane1.setOneTouchExpandable(true);
            splitPane2.setOneTouchExpandable(true);
            splitPane1.setDividerLocation(0.5);
            splitPane2.setDividerLocation(0.67);
        }
    }

    /**
     * This is an alternative layout arrangement. It is the original layout before the tri-planar and
     * the dual tri-planar were merged into this class.
     */
    protected void doOldLayout()
    {
        GridBagLayout gbLayout = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.weightx = 0;
        gbc.weighty = 0;

        int axialIndex = AXIAL_A;
        int coronalIndex = CORONAL_A;
        int sagittalIndex = SAGITTAL_A;

        if (triImage[AXIAL_AB] != null &&
            triImage[CORONAL_AB] != null &&
            triImage[SAGITTAL_AB] != null)
        {
            // if image B exists, show blended images in place of image A
            axialIndex = AXIAL_AB;
            coronalIndex = CORONAL_AB;
            sagittalIndex = SAGITTAL_AB;
        }

        triImagePanel[axialIndex] = new JPanel();
        triImagePanel[axialIndex].setLayout(gbLayout);
        triImagePanel[axialIndex].setBackground(Color.black);

        triImagePanel[coronalIndex] = new JPanel();
        triImagePanel[coronalIndex].setLayout(gbLayout);
        triImagePanel[coronalIndex].setBackground(Color.black);

        triImagePanel[sagittalIndex] = new JPanel();
        triImagePanel[sagittalIndex].setLayout(gbLayout);
        triImagePanel[sagittalIndex].setBackground(Color.black);

        triImagePanel[axialIndex].add(triImage[axialIndex], gbc);
        triImagePanel[coronalIndex].add(triImage[coronalIndex], gbc);
        triImagePanel[sagittalIndex].add(triImage[sagittalIndex], gbc);

        scrollPane[axialIndex] = new JScrollPane(triImagePanel[axialIndex], JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                                 JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollPane[sagittalIndex] = new JScrollPane(triImagePanel[sagittalIndex],
            JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
            JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollPane[coronalIndex] = new JScrollPane(triImagePanel[coronalIndex],
            JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
            JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        if (volumePositionPanel == null)
        {
            volumePositionPanel = buildVolumePositionPanel();
        }

        JPanel mainPanel = new JPanel(new GridLayout(2, 2, 8, 8));

        mainPanel.add(scrollPane[axialIndex]);
        mainPanel.add(scrollPane[sagittalIndex]);
        mainPanel.add(scrollPane[coronalIndex]);

        if (pluginPanel != null)
        {
            mainPanel.add(pluginPanel);
        }
        else
        {
            if (volumePositionFrame != null)
            {
                volumePositionFrame.setVisible(true);
            }
            else
            {
                mainPanel.add(volumePositionPanel);
            }
        }

        getContentPane().removeAll();
        getContentPane().add(panelToolbar, BorderLayout.NORTH);
        getContentPane().add(mainPanel);
        getContentPane().validate();

        setSize(getSize().width, defaultPreferredHeight);
        pack();
    }

    /**
     * The purpose of this method is to set the paint bitmap so that it is the same for all 9 images.
     * The reason it is here is because I couldn't find a way to automatically set the paint bitmap
     * without calling this method. There was a problem because if you were painting in image A, image A/B
     * would also get painted, but not image B. Paint in image A/B resulted in image A also being painted
     * but not image B. Painting in image B resulting in image B being the only image affected. I
     * thought this was weird so I created this method so that all images were painted equally. -- lorsino
     * @param paintMap BitSet the paint bitset object to set the images to
     */
    protected void updatePaint(BitSet paintMap)
    {
        for (int i = 0; i < MAX_TRI_IMAGES; i++)
        {
            if (triImage[i] != null)
            {
                triImage[i].setPaintMask(paintMap);
            }
        }
    }

    /**
     * This method creates an image from the two ModelImage objects and ModelLUT objects
     * passed as parameters.
     * @param imageA ModelImage image A
     * @param lutA ModelLUT image A's LUT
     * @param imageB ModelImage image B
     * @param lutB ModelLUT image B's LUT
     * @param orientation the desired orientation of the result image
     * @return ViewJComponentTriImage
     */
    protected ViewJComponentTriImage buildTriImage(ModelImage imageA, ModelLUT lutA,
        ModelImage imageB, ModelLUT lutB, int orientation)
    {
        ViewJComponentTriImage triImage = new ViewJComponentTriImage(this, imageA, lutA, null, imageB, lutB,
            null, null, zoom, extents, imageA.getLogMagDisplay(), orientation, hasOrientation, orient);

        int axisOrder[] = triImage.getAxisOrder();

        triImage.setBuffers(triImage.getImageBufferA(),
                            triImage.getImageBufferB(),
                            triImage.getPixBuffer(),
                            new int[extents[axisOrder[0]] * extents[axisOrder[1]]]);

        triImage.setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));

        // setResolutions calls set resolutionX and resolutionY in ViewJComponentBase to
        // the correct values for the XY, XZ, and ZY images.
        float axialWidthResFactor = 1.0f;
        float axialHeightResFactor = 1.0f;
        if (imageA.getType() == ModelImage.COMPLEX)
        {
            triImage.setResolutions(1, 1);
        }
        else if ( (resols[axisOrder[1]] >= resols[axisOrder[0]]) && (resols[axisOrder[1]] < 50.0f * resols[axisOrder[0]])
                 && (units[axisOrder[0]] == units[axisOrder[1]]))
        {
            axialHeightResFactor = resols[axisOrder[1]] / resols[axisOrder[0]];
            triImage.setResolutions(1, axialHeightResFactor);
        }
        else if ( (resols[axisOrder[0]] > resols[axisOrder[1]]) && (resols[axisOrder[0]] < 50.0f * resols[axisOrder[1]])
                 && (units[axisOrder[0]] == units[axisOrder[1]]))
        {
            axialWidthResFactor = resols[axisOrder[0]] / resols[axisOrder[1]];
            triImage.setResolutions(axialWidthResFactor, 1);
        }
        else
        {
            triImage.setResolutions(1, 1);
        }

        return triImage;
    }

    /**
     * Convenience method created to simplify configureFrame()
     */
    private void buildLUTs()
    {
        // if not a color image and LUTa is null then make a LUT
        if (imageA.isColorImage() == false)
        {
            int[] dimExtentsLUT = new int[2];
            dimExtentsLUT[0] = 4;
            dimExtentsLUT[1] = 256;
            if (LUTa == null)
            {
                LUTa = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
                float min, max;
                if (imageA.getType() == ModelStorageBase.UBYTE)
                {
                    min = 0;
                    max = 255;
                }
                else if (imageA.getType() == ModelStorageBase.BYTE)
                {
                    min = -128;
                    max = 127;
                }
                else
                {
                    min = (float) imageA.getMin();
                    max = (float) imageA.getMax();
                }
                float imgMin = (float) imageA.getMin();
                float imgMax = (float) imageA.getMax();
                LUTa.resetTransferLine(min, imgMin, max, imgMax);
            }
            if ( (imageB != null) && (LUTb == null))
            {
                LUTb = new ModelLUT(ModelLUT.HOTMETAL, 256, dimExtentsLUT);
                float min, max;
                if (imageB.getType() == ModelStorageBase.UBYTE)
                {
                    min = 0;
                    max = 255;
                }
                else if (imageB.getType() == ModelStorageBase.BYTE)
                {
                    min = -128;
                    max = 127;
                }
                else
                {
                    min = (float) imageB.getMin();
                    max = (float) imageB.getMax();
                }
                float imgMin = (float) imageB.getMin();
                float imgMax = (float) imageB.getMax();
                LUTb.resetTransferLine(min, imgMin, max, imgMax);
            }
        }
    }

    /**
     * Convenience method created to simplify configureFrame()
     */
    private void buildLabels()
    {
        absLabel = new JLabel("Volume coordinates");
        absLabel.setToolTipText("Coordinates in 3D image space");
        absLabel.setForeground(Color.black);
        absLabel.setFont(MipavUtil.font12B);

        labelXPos = new JLabel("");
        labelXPos.setForeground(Color.black);
        labelXPos.setFont(MipavUtil.font12B);

        labelYPos = new JLabel("");
        labelYPos.setForeground(Color.black);
        labelYPos.setFont(MipavUtil.font12B);

        labelZPos = new JLabel("");
        labelZPos.setForeground(Color.black);
        labelZPos.setFont(MipavUtil.font12B);

        posLabel = new JLabel("Position");
        posLabel.setForeground(Color.black);
        posLabel.setFont(MipavUtil.font12B);
        posLabel.setEnabled(false);

        labelXRef = new JLabel("  X:");
        labelXRef.setForeground(Color.black);
        labelXRef.setFont(MipavUtil.font12B);
        labelXRef.setEnabled(false);

        labelYRef = new JLabel("  Y:");
        labelYRef.setForeground(Color.black);
        labelYRef.setFont(MipavUtil.font12B);
        labelYRef.setEnabled(false);

        labelZRef = new JLabel("  Z:");
        labelZRef.setForeground(Color.black);
        labelZRef.setFont(MipavUtil.font12B);
        labelZRef.setEnabled(false);

        if (imageA.getFileInfo(0).getOrigin(0) != 0 || imageA.getFileInfo(0).getOrigin(1) != 0
            || imageA.getFileInfo(0).getOrigin(2) != 0)
        {
            posLabel.setEnabled(true);
            labelXRef.setEnabled(true);
            labelYRef.setEnabled(true);
            labelZRef.setEnabled(true);
        }

        //talLabel = new JLabel( "Talairach grid coordinates" );
        //talLabel.setForeground( Color.black );
        //talLabel.setFont( MipavUtil.font12B );
        //talLabel.setEnabled( showTalairachPosition );

        labelXTal = new JLabel("");
        labelXTal.setForeground(Color.black);
        labelXTal.setFont(MipavUtil.font12B);
        labelXTal.setEnabled(showTalairachPosition);

        labelYTal = new JLabel("");
        labelYTal.setForeground(Color.black);
        labelYTal.setFont(MipavUtil.font12B);
        labelYTal.setEnabled(showTalairachPosition);

        labelZTal = new JLabel("");
        labelZTal.setForeground(Color.black);
        labelZTal.setFont(MipavUtil.font12B);
        labelZTal.setEnabled(showTalairachPosition);

        scannerLabelX = new JLabel();
        scannerLabelY = new JLabel();
        scannerLabelZ = new JLabel();
        
        talairachVoxelLabel = new JLabel("     "); // must be initialized to 5 empty spaces
    }

    /**
     *	Updates the VOI ID for the three component images.
     *	@param voiID	New VOI ID.
     */
    public void updatevoiID(int voiID)
    {
        for (int i = 0; i < MAX_TRI_IMAGES; i++)
        {
            if (triImage[i] != null)
            {
                triImage[i].setVOI_ID(voiID);
            }
        }
    }

    /**
     *   Sets the menu and controls (i.e. toolbars) of the
     *   main frame! This puts the menus and controls needed
     *   to controls the operations of this frame. Different
     *   image frames have different menu and controls. Currently unused.
     */
    public void setControls()
    {}

    /**
     *   Removes the menu and controls of the main frame so
     *   that a new frame can load the main frame with the
     *   proper controls.  Currently unused.
     */
    public void removeControls()
    {}

    /**
     * Gets reference to control widgets for frame.
     * @return       controls
     */
    public ViewControlsImage getControls()
    {
        return controls;
    }

    /**
     *   Set the title of the frame with the image name of slice location
     */
    public void setTitle()
    {
        String str;

        if (displayMode == ViewJComponentBase.IMAGE_A)
        {
            str = imageA.getImageName() + "  M:" + makeString(triImage[AXIAL_A].getZoomX(), 2);
            setTitle(str);
        }
        else
        {
            if (imageB != null && triImage[AXIAL_B] != null)
            {
                str = imageB.getImageName() + "  M:" +
                    makeString(triImage[AXIAL_B].getZoomX(), 2);
                setTitle(str);
            }
        }
    }

    /**
     *  Accessor that returns the reference to imageA
     *  @return     image
     */
    public ModelImage getImageA()
    {
        if (triImage[AXIAL_A] != null)
        {
            return triImage[AXIAL_A].getImageA();
        }
        else
        {
            return null;
        }
    }

    /**
     *  Accessor that returns the reference to imageB
     *  @return     imageB
     */
    public ModelImage getImageB()
    {
        if (triImage[AXIAL_B] != null)
        {
            return triImage[AXIAL_B].getImageB();
        }
        else
        {
            return null;
        }
    }

    /**
     * Changes imageA to a new model image reference.  Swaps the references in the frame and all the component images.
     * @param image  the new image to use
     */
    public void setImageA(ModelImage image)
    {
        super.setImageA(image);

        for (int i = 0; i < MAX_TRI_IMAGES; i++)
        {
            if (triImage[i] != null)
            {
                triImage[i].setImageA(image);
            }
        }

        // Get all frames
        Vector frameList = image.getImageFrameVector();

        if (frameList == null)
        {
            return;
        }
        for (int i = 0; i < frameList.size(); i++)
        {
            if ( ( (ViewJFrameBase) frameList.elementAt(i)) != this)
            {
                ( (ViewJFrameBase) frameList.elementAt(i)).setImageA(image);
            }
        }

        image.setImageOrder(ModelImage.IMAGE_A);

        image.addImageDisplayListener(this);

        if (image.getHistoLUTFrame() != null)
        {
            updateHistoLUTFrame(ViewJComponentBase.IMAGE_A);
        }
        setActiveImage(ViewJComponentBase.IMAGE_A);
    }

    /**
     *   Accessor that sets the reference to imageB.  Includes changing the frame's reference
     *   and the references the components keep.
     *   @param _imageB  image to set the frame to
     */
    public void setImageB(ModelImage _imageB)
    {
        if (imageB != null)
        {
            imageB.disposeLocal();
        } // Dispose of the memory of the old image
        imageB = _imageB;
        imageB.setImageOrder(ModelImage.IMAGE_B);
        
        // tri image objects must be rebuilt with the new image B
        triImage[AXIAL_AB] = buildTriImage(imageA, LUTa, imageB, LUTb, ViewJComponentBase.AXIAL);
        triImage[SAGITTAL_AB] = buildTriImage(imageA, LUTa, imageB, LUTb, ViewJComponentBase.SAGITTAL);
        triImage[CORONAL_AB] = buildTriImage(imageA, LUTa, imageB, LUTb, ViewJComponentBase.CORONAL);
        triImage[AXIAL_B] = buildTriImage(imageB, LUTb, null, null, ViewJComponentBase.AXIAL);
        triImage[CORONAL_B] = buildTriImage(imageB, LUTb, null, null, ViewJComponentBase.CORONAL);
        triImage[SAGITTAL_B] = buildTriImage(imageB, LUTb, null, null, ViewJComponentBase.SAGITTAL);

        imageB.addImageDisplayListener(this);
        if (imageB.isColorImage() == false)
        {
            int[] dimExtentsLUT = new int[2];
            dimExtentsLUT[0] = 4;
            dimExtentsLUT[1] = 256;
            if (LUTb == null)
            {
            	LUTb = new ModelLUT(ModelLUT.HOTMETAL, 256, dimExtentsLUT);
            }
            float min, max;
            if (imageB.getType() == ModelStorageBase.UBYTE)
            {
                min = 0;
                max = 255;
            }
            else if (imageB.getType() == ModelStorageBase.BYTE)
            {
                min = -128;
                max = 127;
            }
            else
            {
                min = (float) imageB.getMin();
                max = (float) imageB.getMax();
            }
            float imgMin = (float) imageB.getMin();
            float imgMax = (float) imageB.getMax();
            LUTb.resetTransferLine(min, imgMin, max, imgMax);
            setLUTb(LUTb);
        }
        
        setImageSelectorPanelVisible(true);
        
        leastSquaresButton.setEnabled(true);
        tpSplineButton.setEnabled(true);

        // Get all frames
        Vector frameList = imageB.getImageFrameVector();
        if (frameList == null)
        {
            return;
        }
        for (int i = 0; i < frameList.size(); i++)
        {
            if ( ( (ViewJFrameBase) frameList.elementAt(i)) != this)
            {
                ( (ViewJFrameBase) frameList.elementAt(i)).setImageB(imageB);
            }
        }
        
        if (imageB.getHistoLUTFrame() != null)
        {
            updateHistoLUTFrame(ViewJComponentBase.IMAGE_B);
        }
        setActiveImage(ViewJComponentBase.IMAGE_B);
        
        zoom = getOptimalZoom(DEFAULT_OPTIMAL_ZOOM, DEFAULT_OPTIMAL_ZOOM);
        for (int i = 0; i < MAX_TRI_IMAGES; i++)
        {
            if (triImage[i] != null)
            {
                triImage[i].setZoom(zoom, zoom);
            }
        }
        
        updateLayout();
    }

    /**
     *  When switching the active image, take the paintBitmap of the previous
     *  active image as the paintBitmap of the new active image
     *  Currenlty unused
     *  @param paintBitmapSwitch if true don't do a getMask on the new actve image
     */
    public void setPaintBitmapSwitch(boolean paintBitmapSwitch)
    {}

    /**
     * Returns a reference to one of the component tri-image components.
     * @param index int the index of the component tri-image to get. Possibilies are
     * AXIAL_A, AXIAL_B, AXIAL_AB, CORONAL_A, CORONAL_B, CORONAL_AB, SAGITTAL_A,
     * SAGITTAL_B, SAGITTAL_AB
     * @return ViewJComponentTriImage
     */
    public ViewJComponentTriImage getTriImage(int index)
    {
        return triImage[index];
    }

    /**
     *  Sets paint intensity in axial image.
     *  @param intensityDropper  the paint intensity value for the XY image
     */
    public void setIntensityDropper(float intensityDropper)
    {
    	for (int i = 0; i < MAX_TRI_IMAGES; i++)
    	{
    		if (triImage[i] != null)
    		{
    			triImage[i].setIntensityDropper(intensityDropper);
    		}
    	}
    }

    /**
     *  Sets integer value on intensityPaintButton
     *  @param intensityDropper  the paint button intensity value to show
     */
    public void setIntensityPaintName(float intensityDropper)
    {
        Double doubleValue = new Double( (double) intensityDropper);
        ( (SpinnerNumberModel) intensitySpinner.getModel()).setValue(doubleValue);
    }

    /**
     *  Sets modes in all images to ViewJComponentBase.DEFAULT
     */
    public void setDefault()
    {
        for (int i = 0; i < MAX_TRI_IMAGES; i++)
        {
            if (triImage[i] != null)
            {
                triImage[i].setMode(ViewJComponentBase.DEFAULT);
            }
        }
    }

    /**
     *   Controls whether or not the images/VOIs of the frame can be modified.
     *   @param flag  if true the image/VOIs can be modified; if false image/VOIs
     *                can NOT be modified
     */
    public void setEnabled(boolean flag)
    {
        for (int i = 0; i < MAX_TRI_IMAGES; i++)
        {
            if (triImage[i] != null)
            {
                triImage[i].setEnabled(flag);
            }
        }
    }

    /**
     *  Sets sagittalComponentSlice
     *  @param sagittalComponentSlice the slice to set
     */
    public void setSagittalComponentSlice(int _sagittalComponentSlice)
    {
        sagittalComponentSlice = _sagittalComponentSlice;
    }

    /**
     *  Sets coronalComponentSlice
     *  @param coronalComponentSlice the slice to set
     */
    public void setCoronalComponentSlice(int _coronalComponentSlice)
    {
        coronalComponentSlice = _coronalComponentSlice;
    }

    /**
     *  Sets axialComponentSlice
     *  @param axialComponentSlice the slice to set
     */
    public void setAxialComponentSlice(int _axialComponentSlice)
    {
        axialComponentSlice = _axialComponentSlice;
    }

    /**
     *   Sets the alpha blending of parameter for two image displaying
     *   @param value      amount [0,100] that is the percentage of Image A to be displayed
     */
    public void setAlphaBlend(int value)
    {
        for (int i = 0; i < MAX_TRI_IMAGES; i++)
        {
            if (triImage[i] != null)
            {
                triImage[i].setAlphaBlend(value);
            }
        }
    }

    /**
     *  Sets the RGB table for ARGB image A.
     *  @param RGBT  the new RGB transfer functions for imageA
     */
    public void setRGBTA(ModelRGB RGBT)
    {
        for (int i = 0; i < MAX_TRI_IMAGES; i++)
        {
            if (triImage[i] != null)
            {
                triImage[i].setRGBTA(RGBT);
            }
        }
    }

    /**
     *   Sets the RGB table for image B.
     *   @param RGBT  the new RGB transfer functions for imageB
     */
    public void setRGBTB(ModelRGB RGBT)
    {
        for (int i = 0; i < MAX_TRI_IMAGES; i++)
        {
            if (triImage[i] != null)
            {
                triImage[i].setRGBTB(RGBT);
            }
        }
    }

    /**
     *   This methods calls the componentImage's update method to redraw the screen. Without LUT changes.
     *   @param forceShow  forces show to reimport image and calc. java image
     *   @return           boolean confirming successful update
     */
    public boolean updateImages(boolean forceShow)
    {
        if (imageA.getType() != ModelImage.COMPLEX)
        {
            if (triImage[AXIAL_AB] != null &&
                triImage[AXIAL_AB].showUsingOrientation(tSlice, axialComponentSlice, null, null, forceShow, -1) == false)
            {
                return false;
            }

            if (triImage[CORONAL_AB] != null &&
                triImage[CORONAL_AB].showUsingOrientation(tSlice, coronalComponentSlice, null, null, forceShow, -1) == false)
            {
                return false;
            }

            if (triImage[SAGITTAL_AB] != null &&
                triImage[SAGITTAL_AB].showUsingOrientation(tSlice, sagittalComponentSlice, null, null, forceShow, -1) == false)
            {
                return false;
            }
            if (triImage[AXIAL_A] != null &&
                triImage[AXIAL_A].showUsingOrientation(tSlice, axialComponentSlice, null, null, forceShow, -1) == false)
            {
                return false;
            }

            if (triImage[CORONAL_A] != null &&
                triImage[CORONAL_A].showUsingOrientation(tSlice, coronalComponentSlice, null, null, forceShow, -1) == false)
            {
                return false;
            }

            if (triImage[SAGITTAL_A] != null &&
                triImage[SAGITTAL_A].showUsingOrientation(tSlice, sagittalComponentSlice, null, null, forceShow, -1) == false)
            {
                return false;
            }
            if (triImage[AXIAL_B] != null &&
                triImage[AXIAL_B].showUsingOrientation(tSlice, axialComponentSlice, null, null, forceShow, -1) == false)
            {
                return false;
            }

            if (triImage[CORONAL_B] != null &&
                triImage[CORONAL_B].showUsingOrientation(tSlice, coronalComponentSlice, null, null, forceShow, -1) == false)
            {
                return false;
            }

            if (triImage[SAGITTAL_B] != null &&
                triImage[SAGITTAL_B].showUsingOrientation(tSlice, sagittalComponentSlice, null, null, forceShow, -1) == false)
            {
                return false;
            }
        } // if (hasOrientation)
        else
        {
            if (triImage[AXIAL_AB] != null &&
                triImage[AXIAL_AB].show(tSlice, axialComponentSlice, null, null, forceShow, -1) == false)
            {
                return false;
            }

            if (triImage[CORONAL_AB] != null &&
                triImage[CORONAL_AB].show(tSlice, coronalComponentSlice, null, null, forceShow, -1) == false)
            {
                return false;
            }

            if (triImage[SAGITTAL_AB] != null &&
                triImage[SAGITTAL_AB].show(tSlice, sagittalComponentSlice, null, null, forceShow, -1) == false)
            {
                return false;
            }
            if (triImage[AXIAL_A] != null &&
                triImage[AXIAL_A].show(tSlice, axialComponentSlice, null, null, forceShow, -1) == false)
            {
                return false;
            }

            if (triImage[CORONAL_A] != null &&
                triImage[CORONAL_A].show(tSlice, coronalComponentSlice, null, null, forceShow, -1) == false)
            {
                return false;
            }

            if (triImage[SAGITTAL_A] != null &&
                triImage[SAGITTAL_A].show(tSlice, sagittalComponentSlice, null, null, forceShow, -1) == false)
            {
                return false;
            }
            if (triImage[AXIAL_B] != null &&
                triImage[AXIAL_B].show(tSlice, axialComponentSlice, null, null, forceShow, -1) == false)
            {
                return false;
            }

            if (triImage[CORONAL_B] != null &&
                triImage[CORONAL_B].show(tSlice, coronalComponentSlice, null, null, forceShow, -1) == false)
            {
                return false;
            }

            if (triImage[SAGITTAL_B] != null &&
                triImage[SAGITTAL_B].show(tSlice, sagittalComponentSlice, null, null, forceShow, -1) == false)
            {
                return false;
            }
        } // else not hasOrientation

        return true;
    }

    /**
     *   This methods calls the componentImage's update method to redraw the screen.
     *   @param LUTa       LUT used to update imageA
     *   @param LUTb       LUT used to update imageB
     *   @param forceShow  forces show to reimport image and calc. java image
     *   @param interpMode image interpolation method (Nearest or Smooth)
     *   @return           boolean confirming successful update
     */
    public boolean updateImages(ModelLUT LUTa, ModelLUT LUTb,
                                boolean forceShow, int interpMode)
    {
        if (imageA.getType() != ModelImage.COMPLEX)
        {
            if (triImage[AXIAL_AB] != null &&
                triImage[AXIAL_AB].showUsingOrientation(tSlice, axialComponentSlice, LUTa, LUTb, forceShow, interpMode) == false)
            {
                return false;
            }

            if (triImage[CORONAL_AB] != null &&
                triImage[CORONAL_AB].showUsingOrientation(tSlice, coronalComponentSlice, LUTa, LUTb, forceShow,
                interpMode) == false)
            {
                return false;
            }

            if (triImage[SAGITTAL_AB] != null &&
                triImage[SAGITTAL_AB].showUsingOrientation(tSlice, sagittalComponentSlice, LUTa, LUTb, forceShow,
                interpMode) == false)
            {
                return false;
            }
            if (triImage[AXIAL_A] != null &&
                triImage[AXIAL_A].showUsingOrientation(tSlice, axialComponentSlice, LUTa, LUTb, forceShow, interpMode) == false)
            {
                return false;
            }

            if (triImage[CORONAL_A] != null &&
                triImage[CORONAL_A].showUsingOrientation(tSlice, coronalComponentSlice, LUTa, LUTb, forceShow,
                interpMode) == false)
            {
                return false;
            }

            if (triImage[SAGITTAL_A] != null &&
                triImage[SAGITTAL_A].showUsingOrientation(tSlice, sagittalComponentSlice, LUTa, LUTb, forceShow,
                interpMode) == false)
            {
                return false;
            }
            if (triImage[AXIAL_B] != null &&
                triImage[AXIAL_B].showUsingOrientation(tSlice, axialComponentSlice, LUTa, LUTb, forceShow, interpMode) == false)
            {
                return false;
            }

            if (triImage[CORONAL_B] != null &&
                triImage[CORONAL_B].showUsingOrientation(tSlice, coronalComponentSlice, LUTa, LUTb, forceShow,
                interpMode) == false)
            {
                return false;
            }

            if (triImage[SAGITTAL_B] != null &&
                triImage[SAGITTAL_B].showUsingOrientation(tSlice, sagittalComponentSlice, LUTa, LUTb, forceShow,
                interpMode) == false)
            {
                return false;
            }
        } // if (hasOrientation)
        else
        {
            if (triImage[AXIAL_AB] != null &&
                triImage[AXIAL_AB].show(tSlice, axialComponentSlice, LUTa, LUTb, forceShow, interpMode) == false)
            {
                return false;
            }

            if (triImage[CORONAL_AB] != null &&
                triImage[CORONAL_AB].show(tSlice, coronalComponentSlice, LUTa, LUTb, forceShow, interpMode) == false)
            {
                return false;
            }

            if (triImage[SAGITTAL_AB] != null &&
                triImage[SAGITTAL_AB].show(tSlice, sagittalComponentSlice, LUTa, LUTb, forceShow, interpMode) == false)
            {
                return false;
            }
            if (triImage[AXIAL_A] != null &&
                triImage[AXIAL_A].show(tSlice, axialComponentSlice, LUTa, LUTb, forceShow, interpMode) == false)
            {
                return false;
            }

            if (triImage[CORONAL_A] != null &&
                triImage[CORONAL_A].show(tSlice, coronalComponentSlice, LUTa, LUTb, forceShow, interpMode) == false)
            {
                return false;
            }

            if (triImage[SAGITTAL_A] != null &&
                triImage[SAGITTAL_A].show(tSlice, sagittalComponentSlice, LUTa, LUTb, forceShow, interpMode) == false)
            {
                return false;
            }
            if (triImage[AXIAL_B] != null &&
                triImage[AXIAL_B].show(tSlice, axialComponentSlice, LUTa, LUTb, forceShow, interpMode) == false)
            {
                return false;
            }

            if (triImage[CORONAL_B] != null &&
                triImage[CORONAL_B].show(tSlice, coronalComponentSlice, LUTa, LUTb, forceShow, interpMode) == false)
            {
                return false;
            }

            if (triImage[SAGITTAL_B] != null &&
                triImage[SAGITTAL_B].show(tSlice, sagittalComponentSlice, LUTa, LUTb, forceShow, interpMode) == false)
            {
                return false;
            }
        } // else not hasOrientation

        return true;
    }
    
    public void updateImageSubset(ViewJComponentTriImage triImage)
    {
    	if (triImage == this.triImage[AXIAL_A] || triImage == this.triImage[CORONAL_A] || triImage == this.triImage[SAGITTAL_A])
    	{
    		this.triImage[AXIAL_A].showUsingOrientation(tSlice, axialComponentSlice, null, null, true, -1);
    		this.triImage[CORONAL_A].showUsingOrientation(tSlice, coronalComponentSlice, null, null, true, -1);
    		this.triImage[SAGITTAL_A].showUsingOrientation(tSlice, sagittalComponentSlice, null, null, true, -1);
    		return;
    	}
    	
    	if (triImage == this.triImage[AXIAL_B] || triImage == this.triImage[CORONAL_B] || triImage == this.triImage[SAGITTAL_B])
    	{
    		this.triImage[AXIAL_B].showUsingOrientation(tSlice, axialComponentSlice, null, null, true, -1);
    		this.triImage[CORONAL_B].showUsingOrientation(tSlice, coronalComponentSlice, null, null, true, -1);
    		this.triImage[SAGITTAL_B].showUsingOrientation(tSlice, sagittalComponentSlice, null, null, true, -1);
    		return;
    	}
    	
    	if (triImage == this.triImage[AXIAL_AB] || triImage == this.triImage[CORONAL_AB] || triImage == this.triImage[SAGITTAL_AB])
    	{
    		updateImages();
    	}
    }

    /**
     *   This methods calls the componentImage's update method to redraw the screen.
     *   @return           boolean confirming successful update
     */
    public boolean updateImages()
    {
        if (imageA.getType() != ModelImage.COMPLEX)
        {

            if (triImage[AXIAL_A] != null &&
                triImage[AXIAL_A].showUsingOrientation(tSlice, axialComponentSlice, null, null, true, -1) == false)
            {
                return false;
            }

            if (triImage[CORONAL_A] != null &&
                triImage[CORONAL_A].showUsingOrientation(tSlice, coronalComponentSlice, null, null, true, -1) == false)
            {
                return false;
            }

            if (triImage[SAGITTAL_A] != null &&
                triImage[SAGITTAL_A].showUsingOrientation(tSlice, sagittalComponentSlice, null, null, true, -1) == false)
            {
                return false;
            }

            if (triImage[AXIAL_B] != null &&
                triImage[AXIAL_B].showUsingOrientation(tSlice, axialComponentSlice, null, null, true, -1) == false)
            {
                return false;
            }

            if (triImage[CORONAL_B] != null &&
                triImage[CORONAL_B].showUsingOrientation(tSlice, coronalComponentSlice, null, null, true, -1) == false)
            {
                return false;
            }

            if (triImage[SAGITTAL_B] != null &&
                triImage[SAGITTAL_B].showUsingOrientation(tSlice, sagittalComponentSlice, null, null, true, -1) == false)
            {
                return false;
            }

            if (triImage[AXIAL_AB] != null &&
                triImage[AXIAL_AB].showUsingOrientation(tSlice, axialComponentSlice, null, null, true, -1) == false)
            {
                return false;
            }

            if (triImage[CORONAL_AB] != null &&
                triImage[CORONAL_AB].showUsingOrientation(tSlice, coronalComponentSlice, null, null, true, -1) == false)
            {
                return false;
            }

            if (triImage[SAGITTAL_AB] != null &&
                triImage[SAGITTAL_AB].showUsingOrientation(tSlice, sagittalComponentSlice, null, null, true, -1) == false)
            {
                return false;
            }
        } // if not COMPLEX
        else
        {
            if (triImage[AXIAL_AB] != null && triImage[AXIAL_AB].show(tSlice, axialComponentSlice, null, null, true, -1) == false)
            {
                return false;
            }

            if (triImage[CORONAL_AB] != null &&
                triImage[CORONAL_AB].show(tSlice, coronalComponentSlice, null, null, true, -1) == false)
            {
                return false;
            }

            if (triImage[SAGITTAL_AB] != null &&
                triImage[SAGITTAL_AB].show(tSlice, sagittalComponentSlice, null, null, true, -1) == false)
            {
                return false;
            }
            if (triImage[AXIAL_A] != null && triImage[AXIAL_A].show(tSlice, axialComponentSlice, null, null, true, -1) == false)
            {
                return false;
            }

            if (triImage[CORONAL_A] != null &&
                triImage[CORONAL_A].show(tSlice, coronalComponentSlice, null, null, true, -1) == false)
            {
                return false;
            }

            if (triImage[SAGITTAL_A] != null &&
                triImage[SAGITTAL_A].show(tSlice, sagittalComponentSlice, null, null, true, -1) == false)
            {
                return false;
            }
            if (triImage[AXIAL_B] != null && triImage[AXIAL_B].show(tSlice, axialComponentSlice, null, null, true, -1) == false)
            {
                return false;
            }

            if (triImage[CORONAL_B] != null &&
                triImage[CORONAL_B].show(tSlice, coronalComponentSlice, null, null, true, -1) == false)
            {
                return false;
            }

            if (triImage[SAGITTAL_B] != null &&
                triImage[SAGITTAL_B].show(tSlice, sagittalComponentSlice, null, null, true, -1) == false)
            {
                return false;
            }
        } // else COMPLEX

        return true;
    }

    /**
     * Do nothing - required by ViewJFrameBase.
     * @return  always false
     */
    public boolean updateImageExtents()
    {
        return false;
    }

    /**
     *   Called from the &quot;normal&quot; image component, sets the slices for
     *   the tri planar view to display.  Parameters are in terms of the image volume
     *   and so must be converted.
     *   @param x    X Slice of image.
     *   @param y    Y Slice of image.
     *   @param z    Z Slice of image.
     */
    public void setSlicesFromFrame(int x, int y, int z)
    {
        int newX = x;
        int newY = y;
        int newZ = z;
        switch (orient[0])
        {
            case FileInfoBase.ORI_R2L_TYPE:
                newX = x;
                break;

            case FileInfoBase.ORI_L2R_TYPE:
                newX = imageA.getExtents()[0] - 1 - x;
                break;

            case FileInfoBase.ORI_A2P_TYPE:
                newY = x;
                break;

            case FileInfoBase.ORI_P2A_TYPE:
                newY = imageA.getExtents()[0] - 1 - x;
                break;

            case FileInfoBase.ORI_I2S_TYPE:
                newZ = x;
                break;

            case FileInfoBase.ORI_S2I_TYPE:
                newZ = imageA.getExtents()[0] - 1 - x;
                break;
        }

        switch (orient[1])
        {
            case FileInfoBase.ORI_R2L_TYPE:
                newX = y;
                break;

            case FileInfoBase.ORI_L2R_TYPE:
                newX = imageA.getExtents()[1] - 1 - y;
                break;

            case FileInfoBase.ORI_A2P_TYPE:
                newY = y;
                break;

            case FileInfoBase.ORI_P2A_TYPE:
                newY = imageA.getExtents()[1] - 1 - y;
                break;

            case FileInfoBase.ORI_I2S_TYPE:
                newZ = y;
                break;

            case FileInfoBase.ORI_S2I_TYPE:
                newZ = imageA.getExtents()[1] - 1 - y;
                break;
        }

        switch (orient[2])
        {
            case FileInfoBase.ORI_R2L_TYPE:
                newX = z;
                break;

            case FileInfoBase.ORI_L2R_TYPE:
                newX = imageA.getExtents()[2] - 1 - z;
                break;

            case FileInfoBase.ORI_A2P_TYPE:
                newY = z;
                break;

            case FileInfoBase.ORI_P2A_TYPE:
                newY = imageA.getExtents()[2] - 1 - z;
                break;

            case FileInfoBase.ORI_I2S_TYPE:
                newZ = z;
                break;

            case FileInfoBase.ORI_S2I_TYPE:
                newZ = imageA.getExtents()[2] - 1 - z;
                break;
        }

        setSlices(newX, newY, newZ);

        updateImages(true);

        // x, y, z passed in from ViewJComponentEditImage.mouseReleased() are already in image volume space
        setPositionLabels(x, y, z);

        fireCoordinateChange(x, y, z);
    }

    /**
     *   Gets the x position of the slice.
     *   @return The x location in the slice.
     */
    public int getSagittalComponentSlice()
    {
        return sagittalComponentSlice;
    }

    /**
     *   Gets the y position of the slice.
     *   @return The y location in the slice.
     */
    public int getCoronalComponentSlice()
    {
        return coronalComponentSlice;
    }

    /**
     *   Gets the axial position of the slice.
     *   @return The axial location in the slice.
     */
    public int getAxialComponentSlice()
    {
        return axialComponentSlice;
    }

    /**
     *   Gets the linked ViewJFrameTriImage
     *   @return linkedFrame
     */
    public ViewJFrameTriImage getLinkedTriFrame()
    {
        return linkTriFrame;
    }

    /**
     * Sets the slice index for each plane in the frame and components. Should be zero indexed.
     * @param x slice index in the patient
     * @param y slice index in the patient
     * @param z slice index in the patient
     */
    public void setSlices(int x, int y, int z)
    {
        sagittalComponentSlice = x;
        coronalComponentSlice = y;
        axialComponentSlice = z;

        fireCoordinateChange(sagittalComponentSlice, coronalComponentSlice, axialComponentSlice);
    }

    /**
     * Sets the slice index for each plane in the frame and components. Should be zero indexed.
     * @param x slice index in the patient
     * @param y slice index in the patient
     * @param z slice index in the patient
     * @param sourceImage the triImage that called this method
     */
    public void setCrosshairs(int x, int y, int z, ViewJComponentTriImage sourceImage)
    {
    	if (sourceImage == null)
    	{
    		return;
    	}
    	
    	int orientation = sourceImage.getOrientation();
    	
    	boolean affectA = false;
    	boolean affectB = false;
    	
    	if (sourceImage == triImage[AXIAL_A] || sourceImage == triImage[SAGITTAL_A] || sourceImage == triImage[CORONAL_A] ||
   			sourceImage == triImage[AXIAL_AB] || sourceImage == triImage[SAGITTAL_AB] || sourceImage == triImage[CORONAL_AB])
    	{
    		affectA = true;
    	}
    	
    	if (sourceImage == triImage[AXIAL_B] || sourceImage == triImage[SAGITTAL_B] || sourceImage == triImage[CORONAL_B] ||
   			sourceImage == triImage[AXIAL_AB] || sourceImage == triImage[SAGITTAL_AB] || sourceImage == triImage[CORONAL_AB])
    	{
    		affectB = true;
    	}
    	
        if (hasOrientation == false)
        {
            // This updates positions of the cursors in the other components.
            if (orientation == ViewJComponentBase.AXIAL)
            {
                if (triImage[SAGITTAL_A] != null && affectA)
                {
                    triImage[SAGITTAL_A].updateCrosshairPosition(z, y);
                }
                if (triImage[SAGITTAL_B] != null && affectB)
                {
                    triImage[SAGITTAL_B].updateCrosshairPosition(z, y);
                }
                if (triImage[SAGITTAL_AB] != null && affectA && affectB)
                {
                    triImage[SAGITTAL_AB].updateCrosshairPosition(z, y);
                }
                if (triImage[CORONAL_A] != null && affectA)
                {
                    triImage[CORONAL_A].updateCrosshairPosition(x, z);
                }
                if (triImage[CORONAL_B] != null && affectB)
                {
                    triImage[CORONAL_B].updateCrosshairPosition(x, z);
                }
                if (triImage[CORONAL_AB] != null && affectA && affectB)
                {
                    triImage[CORONAL_AB].updateCrosshairPosition(x, z);
                }
                if (triImage[AXIAL_A] != null && affectA)
                {
                    triImage[AXIAL_A].updateCrosshairPosition(x, y);
                }
                if (triImage[AXIAL_B] != null && affectB)
                {
                    triImage[AXIAL_B].updateCrosshairPosition(x, y);
                }
                if (triImage[AXIAL_AB] != null && affectA && affectB)
                {
                    triImage[AXIAL_AB].updateCrosshairPosition(x, y);
                }
            }
            else if (orientation == ViewJComponentBase.CORONAL)
            {
                if (triImage[AXIAL_A] != null && affectA)
                {
                    triImage[AXIAL_A].updateCrosshairPosition(x, z);
                }
                if (triImage[AXIAL_B] != null && affectB)
                {
                    triImage[AXIAL_B].updateCrosshairPosition(x, z);
                }
                if (triImage[AXIAL_AB] != null && affectA && affectB)
                {
                    triImage[AXIAL_AB].updateCrosshairPosition(x, z);
                }
                if (triImage[SAGITTAL_A] != null && affectA)
                {
                    triImage[SAGITTAL_A].updateCrosshairPosition(y, z);
                }
                if (triImage[SAGITTAL_B] != null && affectB)
                {
                    triImage[SAGITTAL_B].updateCrosshairPosition(y, z);
                }
                if (triImage[SAGITTAL_AB] != null && affectA && affectB)
                {
                    triImage[SAGITTAL_AB].updateCrosshairPosition(y, z);
                }
                if (triImage[CORONAL_A] != null && affectA)
                {
                    triImage[CORONAL_A].updateCrosshairPosition(x, y);
                }
                if (triImage[CORONAL_B] != null && affectB)
                {
                    triImage[CORONAL_B].updateCrosshairPosition(x, y);
                }
                if (triImage[CORONAL_AB] != null && affectA && affectB)
                {
                    triImage[CORONAL_AB].updateCrosshairPosition(x, y);
                }
            }
            else // SAGITTAL
            {
                if (triImage[AXIAL_A] != null && affectA)
                {
                    triImage[AXIAL_A].updateCrosshairPosition(z, y);
                }
                if (triImage[AXIAL_B] != null && affectB)
                {
                    triImage[AXIAL_B].updateCrosshairPosition(z, y);
                }
                if (triImage[AXIAL_AB] != null && affectA && affectB)
                {
                    triImage[AXIAL_AB].updateCrosshairPosition(z, y);
                }
                if (triImage[CORONAL_A] != null && affectA)
                {
                    triImage[CORONAL_A].updateCrosshairPosition(z, x);
                }
                if (triImage[CORONAL_B] != null && affectB)
                {
                    triImage[CORONAL_B].updateCrosshairPosition(z, x);
                }
                if (triImage[CORONAL_AB] != null && affectA && affectB)
                {
                    triImage[CORONAL_AB].updateCrosshairPosition(z, x);
                }
                if (triImage[SAGITTAL_A] != null && affectA)
                {
                    triImage[SAGITTAL_A].updateCrosshairPosition(x, y);
                }
                if (triImage[SAGITTAL_B] != null && affectB)
                {
                    triImage[SAGITTAL_B].updateCrosshairPosition(x, y);
                }
                if (triImage[SAGITTAL_AB] != null && affectA && affectB)
                {
                    triImage[SAGITTAL_AB].updateCrosshairPosition(x, y);
                }
            }
        }

        // This updates positions of the cursors in the other components.
        else // hasOrientation == true
        {
	        if (orientation == ViewJComponentBase.AXIAL)
	        {
	            if (triImage[SAGITTAL_A] != null && affectA)
	            {
	                triImage[SAGITTAL_A].updateCrosshairPosition(y, z);
	            }
	            if (triImage[SAGITTAL_B] != null && affectB)
	            {
	                triImage[SAGITTAL_B].updateCrosshairPosition(y, z);
	            }
	            if (triImage[SAGITTAL_AB] != null && affectA && affectB)
	            {
	                triImage[SAGITTAL_AB].updateCrosshairPosition(y, z);
	            }
	            if (triImage[CORONAL_A] != null && affectA)
	            {
	                triImage[CORONAL_A].updateCrosshairPosition(x, z);
	            }
	            if (triImage[CORONAL_B] != null && affectB)
	            {
	                triImage[CORONAL_B].updateCrosshairPosition(x, z);
	            }
	            if (triImage[CORONAL_AB] != null && affectA && affectB)
	            {
	                triImage[CORONAL_AB].updateCrosshairPosition(x, z);
	            }
	            if (triImage[AXIAL_A] != null && affectA)
	            {
	                triImage[AXIAL_A].updateCrosshairPosition(x, y);
	            }
	            if (triImage[AXIAL_B] != null && affectB)
	            {
	                triImage[AXIAL_B].updateCrosshairPosition(x, y);
	            }
	            if (triImage[AXIAL_AB] != null && affectA && affectB)
	            {
	                triImage[AXIAL_AB].updateCrosshairPosition(x, y);
	            }
	        }
	        else if (orientation == ViewJComponentBase.CORONAL)
	        {
	            if (triImage[AXIAL_A] != null && affectA)
	            {
	                triImage[AXIAL_A].updateCrosshairPosition(x, z);
	            }
	            if (triImage[AXIAL_B] != null && affectB)
	            {
	                triImage[AXIAL_B].updateCrosshairPosition(x, z);
	            }
	            if (triImage[AXIAL_AB] != null && affectA && affectB)
	            {
	                triImage[AXIAL_AB].updateCrosshairPosition(x, z);
	            }
	            if (triImage[SAGITTAL_A] != null && affectA)
	            {
	                triImage[SAGITTAL_A].updateCrosshairPosition(z, y);
	            }
	            if (triImage[SAGITTAL_B] != null && affectB)
	            {
	                triImage[SAGITTAL_B].updateCrosshairPosition(z, y);
	            }
	            if (triImage[SAGITTAL_AB] != null && affectA && affectB)
	            {
	                triImage[SAGITTAL_AB].updateCrosshairPosition(z, y);
	            }
	            if (triImage[CORONAL_A] != null && affectA)
	            {
	                triImage[CORONAL_A].updateCrosshairPosition(x, y);
	            }
	            if (triImage[CORONAL_B] != null && affectB)
	            {
	                triImage[CORONAL_B].updateCrosshairPosition(x, y);
	            }
	            if (triImage[CORONAL_AB] != null && affectA && affectB)
	            {
	                triImage[CORONAL_AB].updateCrosshairPosition(x, y);
	            }
	        }
	        else // SAGITTAL
	        {
	            if (triImage[AXIAL_A] != null && affectA)
	            {
	                triImage[AXIAL_A].updateCrosshairPosition(z, x);
	            }
	            if (triImage[AXIAL_B] != null && affectB)
	            {
	                triImage[AXIAL_B].updateCrosshairPosition(z, x);
	            }
	            if (triImage[AXIAL_AB] != null && affectA && affectB)
	            {
	                triImage[AXIAL_AB].updateCrosshairPosition(z, x);
	            }
	            if (triImage[CORONAL_A] != null && affectA)
	            {
	                triImage[CORONAL_A].updateCrosshairPosition(z, y);
	            }
	            if (triImage[CORONAL_B] != null && affectB)
	            {
	                triImage[CORONAL_B].updateCrosshairPosition(z, y);
	            }
	            if (triImage[CORONAL_AB] != null && affectA && affectB)
	            {
	                triImage[CORONAL_AB].updateCrosshairPosition(z, y);
	            }
	            if (triImage[SAGITTAL_A] != null && affectA)
	            {
	                triImage[SAGITTAL_A].updateCrosshairPosition(x, y);
	            }
	            if (triImage[SAGITTAL_B] != null && affectB)
	            {
	                triImage[SAGITTAL_B].updateCrosshairPosition(x, y);
	            }
	            if (triImage[SAGITTAL_AB] != null && affectA && affectB)
	            {
	                triImage[SAGITTAL_AB].updateCrosshairPosition(x, y);
	            }
	        }
        }
        
        if (linkTriFrame != null)
        {
        	ViewJComponentTriImage linkTriImage = null;
        	
        	if (sourceImage == triImage[AXIAL_A]) linkTriImage = linkTriFrame.getTriImage(AXIAL_A);
        	if (sourceImage == triImage[SAGITTAL_A]) linkTriImage = linkTriFrame.getTriImage(SAGITTAL_A);
        	if (sourceImage == triImage[CORONAL_A]) linkTriImage = linkTriFrame.getTriImage(CORONAL_A);
        	if (sourceImage == triImage[AXIAL_B]) linkTriImage = linkTriFrame.getTriImage(AXIAL_B);
        	if (sourceImage == triImage[SAGITTAL_B]) linkTriImage = linkTriFrame.getTriImage(SAGITTAL_B);
        	if (sourceImage == triImage[CORONAL_B]) linkTriImage = linkTriFrame.getTriImage(CORONAL_B);
        	if (sourceImage == triImage[AXIAL_AB]) linkTriImage = linkTriFrame.getTriImage(AXIAL_AB);
        	if (sourceImage == triImage[SAGITTAL_AB]) linkTriImage = linkTriFrame.getTriImage(SAGITTAL_AB);
        	if (sourceImage == triImage[CORONAL_AB]) linkTriImage = linkTriFrame.getTriImage(CORONAL_AB);
        		
        	if (linkTriImage != null)
        	{
	        	linkTriFrame.setCrosshairs(x, y, z, linkTriImage);
	        	linkTriFrame.updateImages(true);
        	}
        }
    }

    /**
     * Sets the slice index for each plane in the frame and components. 
     * Should be zero indexed.
     * @param x slice index in the patient
     * @param y slice index in the patient
     * @param z slice index in the patient
     */
    public void setDisplaySlices(int x, int y, int z, int componentOrientation)
    {
        if (componentOrientation == ViewJComponentBase.AXIAL)
        {
            sagittalComponentSlice = x;
            coronalComponentSlice = y;
            axialComponentSlice = z;
        }
        else if (componentOrientation == ViewJComponentBase.CORONAL)
        {
            sagittalComponentSlice = x;
            coronalComponentSlice = z;
            axialComponentSlice = y;
        }
        else // SAGITTAL
        {
            if (hasOrientation == false)
            {
                sagittalComponentSlice = z;
                coronalComponentSlice = y;
                axialComponentSlice = x;
            }
            else
            {
                sagittalComponentSlice = z;
                coronalComponentSlice = x;
                axialComponentSlice = y;
            }
        }
        
        if (linkTriFrame != null)
        {
        	linkTriFrame.setDisplaySlices(x, y, z, componentOrientation);
        }

        fireCoordinateChange(sagittalComponentSlice, coronalComponentSlice, axialComponentSlice);
    }

    /**
     * Sets the labels which show the absolute position within the image volume and the patient position.
     * @param x  the x volume coordinate
     * @param y  the y volume coordinate
     * @param z  the z volume coordinate
     */
    public void setPositionLabels(int x, int y, int z)
    {
        setAbsolutePositionLabels(x, y, z);
        setScannerPositionLabels(x, y, z);
        if (showTalairachPosition)
        {
            setTalairachPositionLabels(x, y, z);
        }

        currentAbsolutePositionLabels = new Point3D(x, y, z);
    }

    /**
     * Get the last point that the position labels got set to.
     * @return the current values for the absolute position labels
     */
    public Point3D getCurrentPositionLabels()
    {
        return currentAbsolutePositionLabels;
    }

    /**
     * Sets the labels which show the absolute position within the image volume.
     * @param x  the x volume coordinate
     * @param y  the y volume coordinate
     * @param z  the z volume coordinate
     */
    protected void setAbsolutePositionLabels(int x, int y, int z)
    {
        labelXPos.setText(Integer.toString(x + 1));
        labelYPos.setText(Integer.toString(y + 1));
        labelZPos.setText(Integer.toString(z + 1));
        
        if (volumePositionFrame != null)
        {
        	volumePositionFrame.labelXPos.setText(Integer.toString(x + 1));
        	volumePositionFrame.labelYPos.setText(Integer.toString(y + 1));
        	volumePositionFrame.labelZPos.setText(Integer.toString(z + 1));
        }
    }

    /**
     *	Sets the labels that refer to relative position within the image.
     *	@param	x	Absolute x value in slice.
     *	@param	y	Absolute y value in slice.
     *	@param  z	Absolute z value in slice.
     */
    protected void setScannerPositionLabels(int x, int y, int z)
    {
        DecimalFormat nf = new DecimalFormat("#####0.0##");
        float[] tCoord = new float[3];

        imageA.getScannerCoordLPS(x, y, z, tCoord);

        if (tCoord[0] < 0)
        {
            scannerLabelX.setText("R: ");
            labelXRef.setText(String.valueOf(nf.format( -tCoord[0])));
            if (volumePositionFrame != null)
            {
                volumePositionFrame.labelXRef.setText(String.valueOf(nf.format( -tCoord[0])));
            }
        }
        else
        {
            scannerLabelX.setText("L: ");
            labelXRef.setText(String.valueOf(nf.format(tCoord[0])));
            if (volumePositionFrame != null)
            {
                volumePositionFrame.labelXRef.setText(String.valueOf(nf.format(tCoord[0])));
            }
        }

        if (tCoord[1] < 0)
        {
            scannerLabelY.setText("A: ");
            labelYRef.setText(String.valueOf(nf.format( -tCoord[1])));
            if (volumePositionFrame != null)
            {
                volumePositionFrame.labelYRef.setText(String.valueOf(nf.format( -tCoord[1])));
            }
        }
        else
        {
            scannerLabelY.setText("P: ");
            labelYRef.setText(String.valueOf(nf.format(tCoord[1])));
            if (volumePositionFrame != null)
            {
                volumePositionFrame.labelYRef.setText(String.valueOf(nf.format(tCoord[1])));
            }
        }

        if (tCoord[2] < 0)
        {
            scannerLabelZ.setText("I: ");
            labelZRef.setText(String.valueOf(nf.format( -tCoord[2])));
            if (volumePositionFrame != null)
            {
                volumePositionFrame.labelZRef.setText(String.valueOf(nf.format( -tCoord[2])));
            }
        }
        else
        {
            scannerLabelZ.setText("S: ");
            labelZRef.setText(String.valueOf(nf.format(tCoord[2])));
            if (volumePositionFrame != null)
            {
                volumePositionFrame.labelZRef.setText(String.valueOf(nf.format(tCoord[2])));
            }
        }

    }

    /**
     *	Sets the labels that refer to Talairach position within the image.
     *	@param	x	Absolute x value in slice.
     *	@param	y	Absolute y value in slice.
     *	@param  z	Absolute z value in slice.
     */
    protected void setTalairachPositionLabels(int x, int y, int z)
    {
        float xTal, yTal, zTal;
        Point3Df pt;
        DecimalFormat nf = new DecimalFormat("#####0.0##");
        String strX = "";
        String strY = "";
        String strZ = "";

        try
        {
            TalairachTransformInfo tInfo = imageA.getTalairachTransformInfo();
            if (tInfo != null) {
                pt = tInfo.getTlrcAC();
            }
            else {
                tInfo.setAcpcRes(imageA.getFileInfo()[0].getResolutions()[0]);
                pt = tInfo.getTlrcAC();
            }
            xTal = x - pt.x;
            yTal = y - pt.y;
            zTal = z - pt.z;
        }
        catch (Exception ex)
        {
            xTal = x * imageA.getFileInfo()[0].getResolutions()[0] - ATLAS_BBOX_LAT;
            yTal = y * imageA.getFileInfo()[0].getResolutions()[1] - ATLAS_BBOX_ANT;
            if (useInfNew)
            {
                zTal = z * imageA.getFileInfo()[0].getResolutions()[2] - ATLAS_BBOX_INF_NEW;
            }
            else
            {
                zTal = z * imageA.getFileInfo()[0].getResolutions()[2] - ATLAS_BBOX_INF;
            }
        }

        strX = String.valueOf(nf.format(xTal));
        strY = String.valueOf(nf.format(yTal));
        strZ = String.valueOf(nf.format(zTal));

        int[] axisOrder = triImage[AXIAL_A].getAxisOrder();

        if (x >= 0 && x < imageA.getExtents()[axisOrder[0]])
        {
            labelXTal.setText(strX);
        }
        if (y >= 0 && y < imageA.getExtents()[axisOrder[1]])
        {
            labelYTal.setText(strY);
        }
        if (z >= 0 && z < imageA.getExtents()[axisOrder[2]])
        {
            labelZTal.setText(strZ);
        }

        if (volumePositionFrame != null)
        {
            if (x >= 0 && x < imageA.getExtents()[axisOrder[0]])
            {
                volumePositionFrame.labelXTal.setText(strX);
            }
            if (y >= 0 && y < imageA.getExtents()[axisOrder[1]])
            {
                volumePositionFrame.labelYTal.setText(strY);
            }
            if (z >= 0 && z < imageA.getExtents()[axisOrder[2]])
            {
                volumePositionFrame.labelZTal.setText(strZ);
            }
        }
    }

    /**
     *   Sets the x coordinate of the point to be the center of the transformed image.
     *   @param   x	The x coordinate of the center.
     */
    public int[] getCenter()
    {
        return volumeCenter;
    }

    /**
     *   Sets the x coordinate of the point to be the center of the transformed image.
     *   @param   x	The x coordinate of the center.
     */
    public void setVolumeCenter(int x, int y, int z)
    {
        volumeCenter[0] = x;
        volumeCenter[1] = y;
        volumeCenter[2] = z;
    }

    /**
     *   Sets the x coordinate of the point to be the center of the transformed image.
     *   @param   x	The x coordinate of the center.
     */
    public void setVolumeCenter(Point3D newVolumeCenter)
    {
        volumeCenter[0] = newVolumeCenter.x;
        volumeCenter[1] = newVolumeCenter.y;
        volumeCenter[2] = newVolumeCenter.z;
    }

    /**
     * Does nothing.
     * @param slice  the slice to show
     */
    public void setSlice(int slice)
    {}

    /**
     *   Sets the slice to be displayed and updates title frame
     *   @param slice indicates image time-slice (4th dimension) to be displayed
     */
    public void setTimeSlice(int slice)
    {
        if ( ( (imageA.getNDims() <= 3) && (imageB == null))
            || ( (imageA.getNDims() <= 3) && (imageB != null) && (imageB.getNDims() <= 3)))
        {
            return;
        }
        if ( ( (imageA.getNDims() == 4) && (tSlice < imageA.getExtents()[3]))
            || ( (imageB != null) && (imageB.getNDims() == 4) && (tSlice < imageB.getExtents()[3])))
        {
            tSlice = slice;
            updateImages(true);
            setTitle();
            setTSlider(slice);
        }
    }

    /**
     *   Sets time slider slice
     *   @param tSlice   Slice to set to.
     */
    protected void setTSlider(int tSlice)
    {
        int newValue;

        if (tImageSlider == null)
        {
            return;
        }
        newValue = Math.round(100.0f * tSlice / (extents[3] - 1) - 0.01f);
        tImageSlider.removeChangeListener(this);
        tImageSlider.setValue(newValue);
        tImageSlider.addChangeListener(this);
    }

    /**
     * Converts to DICOM positions.  TODO: This method really should be relocated as it is essentially just
     * a worker method. Shouldn't be in a GUI class
     * Convert from input image oriented x,y,z to Dicom x,y,z
     * (x axis = R->L, y axis = A->P, z axis = I->S)
     * Image distances are oriented the same as DICOM,
     * just in a permuted order.
     * @param in     the input reference point in the original image
     * @param image  the image from which to translate the point <code>in</code>
     * @return       the point <code>in</code> converted into Dicom space
     */
    public static Point3Df toDicom(Point3Df in, ModelImage image)
    {
        int[] orient = image.getFileInfo(0).getAxisOrientation();
        int xDim = image.getExtents()[0];
        int yDim = image.getExtents()[1];
        int zDim = image.getExtents()[2];
        Point3Df out = new Point3Df(0.0f, 0.0f, 0.0f);

        switch (orient[0])
        {
            case FileInfoBase.ORI_R2L_TYPE:
                out.x = in.x;
                break;

            case FileInfoBase.ORI_L2R_TYPE:
                out.x = xDim - 1 - in.x;
                break;

            case FileInfoBase.ORI_A2P_TYPE:
                out.y = in.x;
                break;

            case FileInfoBase.ORI_P2A_TYPE:
                out.y = xDim - 1 - in.x;
                break;

            case FileInfoBase.ORI_I2S_TYPE:
                out.z = in.x;
                break;

            case FileInfoBase.ORI_S2I_TYPE:
                out.z = xDim - 1 - in.x;
                break;
        }

        switch (orient[1])
        {
            case FileInfoBase.ORI_R2L_TYPE:
                out.x = in.y;
                break;

            case FileInfoBase.ORI_L2R_TYPE:
                out.x = yDim - 1 - in.y;
                break;

            case FileInfoBase.ORI_A2P_TYPE:
                out.y = in.y;
                break;

            case FileInfoBase.ORI_P2A_TYPE:
                out.y = yDim - 1 - in.y;
                break;

            case FileInfoBase.ORI_I2S_TYPE:
                out.z = in.y;
                break;

            case FileInfoBase.ORI_S2I_TYPE:
                out.z = yDim - 1 - in.y;
                break;
        }

        switch (orient[2])
        {
            case FileInfoBase.ORI_R2L_TYPE:
                out.x = in.z;
                break;

            case FileInfoBase.ORI_L2R_TYPE:
                out.x = zDim - 1 - in.z;
                break;

            case FileInfoBase.ORI_A2P_TYPE:
                out.y = in.z;
                break;

            case FileInfoBase.ORI_P2A_TYPE:
                out.y = zDim - 1 - in.z;
                break;

            case FileInfoBase.ORI_I2S_TYPE:
                out.z = in.z;
                break;

            case FileInfoBase.ORI_S2I_TYPE:
                out.z = zDim - 1 - in.z;
                break;
        }

        return out;
    }

    /**
     * Converts DICOM back to original. 
     * TODO: This method really should be relocated as it is essentially just
     * a worker method. Shouldn't be in a GUI class
     * @param image   Image to convert.
     * @param in      Original point.
     * @param orient  The image orientation.
     * @return        The point <code>in</code converted from DICOM space into the space of the image
     */
    protected static Point3Df toOriginal(ModelImage image, Point3Df in, int[] orient)
    {
        int xDim = image.getExtents()[0];
        int yDim = image.getExtents()[1];
        int zDim = image.getExtents()[2];
        Point3Df out = new Point3Df(0.0f, 0.0f, 0.0f);

        switch (orient[0])
        {
            case FileInfoBase.ORI_R2L_TYPE:
                out.x = in.x;
                break;

            case FileInfoBase.ORI_L2R_TYPE:
                out.x = xDim - 1 - in.x;
                break;

            case FileInfoBase.ORI_A2P_TYPE:
                out.x = in.y;
                break;

            case FileInfoBase.ORI_P2A_TYPE:
                out.x = yDim - 1 - in.y;
                break;

            case FileInfoBase.ORI_I2S_TYPE:
                out.x = in.z;
                break;

            case FileInfoBase.ORI_S2I_TYPE:
                out.x = zDim - 1 - in.z;
                break;
        }

        switch (orient[1])
        {
            case FileInfoBase.ORI_R2L_TYPE:
                out.y = in.x;
                break;

            case FileInfoBase.ORI_L2R_TYPE:
                out.y = xDim - 1 - in.x;
                break;

            case FileInfoBase.ORI_A2P_TYPE:
                out.y = in.y;
                break;

            case FileInfoBase.ORI_P2A_TYPE:
                out.y = yDim - 1 - in.y;
                break;

            case FileInfoBase.ORI_I2S_TYPE:
                out.y = in.z;
                break;

            case FileInfoBase.ORI_S2I_TYPE:
                out.y = zDim - 1 - in.z;
                break;
        }

        switch (orient[2])
        {
            case FileInfoBase.ORI_R2L_TYPE:
                out.z = in.x;
                break;

            case FileInfoBase.ORI_L2R_TYPE:
                out.z = xDim - 1 - in.x;
                break;

            case FileInfoBase.ORI_A2P_TYPE:
                out.z = in.y;
                break;

            case FileInfoBase.ORI_P2A_TYPE:
                out.z = yDim - 1 - in.y;
                break;

            case FileInfoBase.ORI_I2S_TYPE:
                out.z = in.z;
                break;

            case FileInfoBase.ORI_S2I_TYPE:
                out.z = zDim - 1 - in.z;
                break;
        }

        return out;
    }

    /**
     *	Accessor that sets the LUT
     *   @param LUT  the LUT
     */
    public void setLUTa(ModelLUT LUT)
    {
        if (triImage[AXIAL_AB] != null)
        {
            triImage[AXIAL_AB].setLUTa(LUT);
        }
        if (triImage[CORONAL_AB] != null)
        {
            triImage[CORONAL_AB].setLUTa(LUT);
        }
        if (triImage[SAGITTAL_AB] != null)
        {
            triImage[SAGITTAL_AB].setLUTa(LUT);
        }
        if (triImage[AXIAL_B] != null)
        {
            triImage[AXIAL_B].setLUTa(LUT);
        }
        if (triImage[CORONAL_B] != null)
        {
            triImage[CORONAL_B].setLUTa(LUT);
        }
        if (triImage[SAGITTAL_B] != null)
        {
            triImage[SAGITTAL_B].setLUTa(LUT);
        }
        updateImages(true);
    }

    /**
     *	Accessor that sets the LUT
     *	@param LUT  the LUT
     */
    public void setLUTb(ModelLUT LUT)
    {
        if (triImage[AXIAL_AB] != null)
        {
            triImage[AXIAL_AB].setLUTb(LUT);
        }
        if (triImage[CORONAL_AB] != null)
        {
            triImage[CORONAL_AB].setLUTb(LUT);
        }
        if (triImage[SAGITTAL_AB] != null)
        {
            triImage[SAGITTAL_AB].setLUTb(LUT);
        }
        if (triImage[AXIAL_B] != null)
        {
            triImage[AXIAL_B].setLUTa(LUT); // must set LUT a because image B is in image A slot
        }
        if (triImage[CORONAL_B] != null)
        {
            triImage[CORONAL_B].setLUTa(LUT); // must set LUT a because image B is in image A slot
        }
        if (triImage[SAGITTAL_B] != null)
        {
            triImage[SAGITTAL_B].setLUTa(LUT); // must set LUT a because image B is in image A slot
        }
        updateImages(true);
    }

    /**
     *   Set the active image for drawing VOIs. VOIs are only
     *   drawn in the active image. In addition, algorithms
     *   are executed on the active window.
     *   @param active   ViewJComponentBase.IMAGE_A or ViewJComponentBase.IMAGE_B
     */
    public void setActiveImage(int active)
    {
        for (int i = 0; i < MAX_TRI_IMAGES; i++)
        {
            if (triImage[i] != null)
            {
                triImage[i].setActiveImage(active);
            }
        }

        if (active == ViewJComponentBase.IMAGE_B)
        {
            displayMode = ViewJComponentBase.IMAGE_B;
            setTitle();
            radioImageB.setSelected(true);
            setSpinnerValues(imageB.getType());
        }
        else
        {
            displayMode = ViewJComponentBase.IMAGE_A;
            setTitle();
            radioImageA.setSelected(true);
            setSpinnerValues(imageA.getType());
        }

        /** updateImages(false) was commented to facilitate placement of VOI points during
         * image registration. This allows the user to control the crosshairs and image slice
         * independently of other images.
         */
        //updateImages(false);
    }

    /**
     *   Displays histoLUT frame.
     *   @param imageAorB    ViewJComponentBase.IMAGE_A or ViewJComponentBase.IMAGE_B
     */
    protected void updateHistoLUTFrame(int imageAorB)
    {
        updateImages(true);
        if (imageA.getHistoLUTFrame() != null && imageAorB == ViewJComponentBase.IMAGE_A && triImage[AXIAL_A] != null)
        {
            imageA.getHistoLUTFrame().updateHistoLUT(imageA, triImage[AXIAL_A].getLUTa(), null, null, true);
        }
        else if (imageA.getHistoLUTFrame() != null && imageAorB == ViewJComponentBase.IMAGE_B && triImage[AXIAL_B] != null)
        {
            imageA.getHistoLUTFrame().updateHistoLUT(null, null, imageB, triImage[AXIAL_B].getLUTb(), true);
        }
        else if (imageA.getHistoLUTFrame() != null && imageAorB == IMAGE_A_B && triImage[AXIAL_AB] != null)
        {
            imageA.getHistoLUTFrame().updateHistoLUT(imageA, triImage[AXIAL_AB].getLUTa(), imageB,
                triImage[AXIAL_AB].getLUTb(), true);
        }
    }

    //************************************************************************
     //**************************** Action Events *****************************
      //************************************************************************

       /**
        *  Calls various methods depending on the action
        *  @param event      event that triggered function
        */
       public void actionPerformed(ActionEvent event)
       {
           String command = event.getActionCommand();
           Object source = event.getSource();

           Preferences.debug(command, Preferences.DEBUG_MINOR);

           if (command.equals("CloseFrame"))
           {
               windowClosing(null);
           }
           else if (command.equals("leastSquares"))
           {
        	   handleLeastSquares();
        	   return;
           }
           else if (command.equals("tpSpline"))
           {
        	   handleTPSpline();
        	   return;
           }
           else if (command.equals(OLD_LAYOUT))
           {
               oldLayout = !oldLayout;

               Preferences.setProperty(Preferences.PREF_TRIPLANAR_2X2_LAYOUT, String.valueOf(oldLayout));

               float optimalZoom = getOptimalZoom(DEFAULT_OPTIMAL_ZOOM, DEFAULT_OPTIMAL_ZOOM);

               for (int i = 0; i < MAX_TRI_IMAGES; i++)
               {
                   if (triImage[i] != null)
                   {
                       triImage[i].setZoom(optimalZoom, optimalZoom);
                   }
               }

               updateLayout();
           }
           else if (command.equals(PANEL_PLUGIN))
           {
               handlePluginPanelSelection();
               return;
           }
           else if (command.equals("ShowAxes"))
           {
               for (int i = 0; i < MAX_TRI_IMAGES; i++)
               {
                   if (triImage[i] != null)
                   {
                       triImage[i].displayAxes(menuObj.isMenuItemSelected("Show axes"));
                   }
               }

               updateImages(true);
           }
           else if (command.equals("ShowXHairs"))
           {
               for (int i = 0; i < MAX_TRI_IMAGES; i++)
               {
                   if (triImage[i] != null)
                   {
                       triImage[i].displayXHairs(menuObj.isMenuItemSelected("Show crosshairs"));
                   }
               }

               updateImages(true);
           }
           else if (command.equals("FastPaint"))
           {
               Preferences.setProperty(Preferences.PREF_FAST_TRIPLANAR_REPAINT,
                                       (menuObj.isMenuItemSelected("Fast rendering in paint mode") ? "true" : "false"));
           }
           else if (command.equals("ShowTalairachGrid"))
           {
               // according to current logic, there is a possibility that the
               // volume position frame and the tabbed volume position pane in the plug-in
               // position could both be active at once, hence the need for this
               // sourceCheckbox logic
               JCheckBox sourceCheckbox = (JCheckBox) (event.getSource());

               showTalairachGrid = sourceCheckbox.isSelected();
               chkShowTalairachGrid.setSelected(showTalairachGrid);
               chkShowTalairachGridMarkers.setEnabled(showTalairachGrid);

               if (volumePositionFrame != null)
               {
                   volumePositionFrame.setShowTalairachGrid(showTalairachGrid);
               }

               menuObj.setMenuItemSelected("ShowXHairs", !showTalairachGrid);
               menuObj.setMenuItemSelected("ShowAxes", !showTalairachGrid);

               for (int i = 0; i < MAX_TRI_IMAGES; i++)
               {
                   if (triImage[i] != null)
                   {
                       triImage[i].showTalairachGrid(showTalairachGrid);
                       triImage[i].displayAxes(!showTalairachGrid);
                       triImage[i].displayXHairs(!showTalairachGrid);
                   }
               }

               updateImages(true);
           }
           else if (command.equals("ShowTalairachGridmarkers"))
           {
               // according to current logic, there is a possibility that the
               // volume position frame and the tabbed volume position pane in the plug-in
               // position could both be active at once, hence the need for this
               // sourceCheckbox logic
               JCheckBox sourceCheckbox = (JCheckBox) (event.getSource());

               boolean showGridMarkers = sourceCheckbox.isSelected();
               chkShowTalairachGridMarkers.setSelected(showGridMarkers);
               if (volumePositionFrame != null)
               {
                   volumePositionFrame.setShowTalairachGridMarkers(showGridMarkers);
               }

               for (int i = 0; i < MAX_TRI_IMAGES; i++)
               {
                   if (triImage[i] != null)
                   {
                       triImage[i].showTalairachGridmarkers(showGridMarkers);
                   }
               }

               updateImages(true);
           }
           else if (command.equals("PaintBrush"))
           {
               if ( (imageB != null) && (!radioImageBoth.isEnabled()))
               {
                   radioImageBoth.setEnabled(true);
               }

               for (int i = 0; i < MAX_TRI_IMAGES; i++)
               {
                   if (triImage[i] != null)
                   {
                       triImage[i].setMode(ViewJComponentBase.PAINT_VOI);
                   }
               }

           }
           else if (command.equals("Dropper"))
           {
        	   for (int i = 0; i < MAX_TRI_IMAGES; i++)
               {
                   if (triImage[i] != null)
                   {
                       triImage[i].setMode(ViewJComponentBase.DROPPER_PAINT);
                   }
               }
           }
           else if (command.equals("Eraser"))
           {
               if ( (imageB != null) && (!radioImageBoth.isEnabled()))
               {
                   radioImageBoth.setEnabled(true);
               }
               for (int i = 0; i < MAX_TRI_IMAGES; i++)
               {
                   if (triImage[i] != null)
                   {
                       triImage[i].setMode(ViewJComponentBase.ERASER_PAINT);
                   }
               }
           }
           else if (command.equals("EraseAll"))
           {
        	   triImage[AXIAL_A].eraseAllPaint(false);

        	   imageA.notifyImageDisplayListeners(null, true);
               imageB.notifyImageDisplayListeners(null, true);
           }
           else if (command.equals("PaintMask"))
           {
               if (triImage[AXIAL_AB] != null)
               {
                   triImage[AXIAL_AB].getActiveImage().setMask(triImage[AXIAL_AB].getActiveImage().generateVOIMask());
                   triImage[AXIAL_AB].setPaintMask(triImage[AXIAL_AB].getActiveImage().getMask());
               }
               updateImages(true);
           }
           else if (command.equals("traverse"))
           {
               if ( (imageB != null) && (!radioImageBoth.isEnabled()))
               {
                   radioImageBoth.setEnabled(true);
               }
               for (int i = 0; i < MAX_TRI_IMAGES; i++)
               {
                   if (triImage[i] != null)
                   {
                       triImage[i].setMode(ViewJComponentBase.DEFAULT);
                   }
               }

               updateImages(true);
           }
           else if (command.equals("Center"))
           {
               centerButtonSelected = !centerButtonSelected;

               btnInvisible[3].setSelected(!centerButtonSelected);

               if (centerButtonSelected)
               {
                   for (int i = 0; i < MAX_TRI_IMAGES; i++)
                   {
                       if (triImage[i] != null)
                       {
                           triImage[i].makeCenter();
                       }
                   }
               }
               else
               {
                   for (int i = 0; i < MAX_TRI_IMAGES; i++)
                   {
                       if (triImage[i] != null)
                       {
                           triImage[i].setDoCenter(false);
                       }
                   }
               }

               updateImages(true);
           }
           else if (command.equals("Protractor"))
           {

               if (triImage[AXIAL_A].isProtractorVisible())
               {
                   btnInvisible[2].setSelected(true);
               }

               if ( ( (JToggleButton) source).isSelected())
               {
                   if ( (imageB != null) && (!radioImageBoth.isEnabled()))
                   {
                       radioImageBoth.setEnabled(true);
                   }

                   for (int i = 0; i < MAX_TRI_IMAGES; i++)
                   {
                       if (triImage[i] != null)
                       {
                           triImage[i].setMode(ViewJComponentBase.PROTRACTOR);
                           triImage[i].makeProtractor();
                           triImage[i].setProtractorVisible(true);
                           triImage[i].setIntensityLineVisible(false);
                           triImage[i].repaint();
                       }
                   }
               }
               else
               {
                   for (int i = 0; i < MAX_TRI_IMAGES; i++)
                   {
                       if (triImage[i] != null)
                       {
                           triImage[i].setMode(ViewJComponentBase.DEFAULT);
                           triImage[i].setProtractorVisible(false);
                           triImage[i].repaint();
                       }
                   }
               }
           }
           else if (command.equals("Line"))
           {

               if (triImage[AXIAL_A].isIntensityLineVisible())
               {
                   btnInvisible[2].setSelected(true);
               }

               if ( (imageB != null) && (!radioImageBoth.isEnabled()))
               {
                   radioImageBoth.setEnabled(true);
               }

               if ( ( (JToggleButton) source).isSelected())
               {
                   for (int i = 0; i < MAX_TRI_IMAGES; i++)
                   {
                       if (triImage[i] != null)
                       {
                           triImage[i].setMode(ViewJComponentBase.LINE);
                           triImage[i].setIntensityLineVisible(true);
                           triImage[i].setProtractorVisible(false);
                           triImage[i].repaint();
                       }
                   }
               }
               else
               {
                   for (int i = 0; i < MAX_TRI_IMAGES; i++)
                   {
                       if (triImage[i] != null)
                       {
                           triImage[i].setMode(ViewJComponentBase.DEFAULT);
                           triImage[i].setIntensityLineVisible(false);
                           //triImage[i].setProtractorVisible(true);
                           triImage[i].repaint();
                       }
                   }
               }
           }
           else if (command.equals("ThinPaint"))
           {
               for (int i = 0; i < MAX_TRI_IMAGES; i++)
               {
                   if (triImage[i] != null)
                   {
                       triImage[i].setPaintBrushSize(ViewJComponentTriImage.thinPaint);
                   }
               }
           }
           else if (command.equals("MedPaint"))
           {
               for (int i = 0; i < MAX_TRI_IMAGES; i++)
               {
                   if (triImage[i] != null)
                   {
                       triImage[i].setPaintBrushSize(ViewJComponentTriImage.medPaint);
                   }
               }
           }
           else if (command.equals("ThickPaint"))
           {
               for (int i = 0; i < MAX_TRI_IMAGES; i++)
               {
                   if (triImage[i] != null)
                   {
                       triImage[i].setPaintBrushSize(ViewJComponentTriImage.thickPaint);
                   }
               }
           }
           else if (command.equals("ThickestPaint"))
           {
               for (int i = 0; i < MAX_TRI_IMAGES; i++)
               {
                   if (triImage[i] != null)
                   {
                       triImage[i].setPaintBrushSize(ViewJComponentTriImage.thickestPaint);
                   }
               }
           }
           else if (command.equals("colorPaint"))
           {
               colorChooser = new ViewJColorChooser(this, "Pick paint color", new OkColorListener(), null);
           }
           else if (command.equals("OpacityPaint"))
           {
               if (controls != null)
               {
                   new JDialogOpacityControls(this, controls);
               }
               else
               {
                   new JDialogOpacityControls(this, OPACITY);
               }
               triImage[AXIAL_A].getActiveImage().notifyImageDisplayListeners(null, true);
           }
           else if (command.equals("CommitPaint"))
           {

               if (getSelectedImage() == ViewJComponentBase.BOTH)
               {
                   if (triImage[AXIAL_AB] != null)
                   {
                       triImage[AXIAL_AB].commitMask(ViewJComponentBase.BOTH, true, true);
                       triImage[AXIAL_AB].getActiveImage().notifyImageDisplayListeners(null, true);
                   }
                   else
                   {
                       //triImage[AXIAL_A].commitMask(ViewJComponentBase.BOTH, true, true);
                   }
               }
               else if (getSelectedImage() == ViewJComponentBase.IMAGE_A)
               {
                   if (triImage[AXIAL_A] != null)
                   {
                       triImage[AXIAL_A].commitMask(ViewJComponentBase.IMAGE_A, true, true);
                       triImage[AXIAL_A].getActiveImage().notifyImageDisplayListeners(null, true);
                   }
               }
               else if (getSelectedImage() == ViewJComponentBase.IMAGE_B)
               {
                   // must set IMAGE_A because in the AXIAL_B, SAGITTAL_B, and CORONAL_B images,
                   // imageB is in the imageA slot
                   if (triImage[AXIAL_B] != null)
                   {
                       triImage[AXIAL_B].commitMask(ViewJComponentBase.IMAGE_A, true, true);
                       triImage[AXIAL_B].getActiveImage().notifyImageDisplayListeners(null, true);
                   }
               }

               //updateImages(true);
           }
           else if (command.equals("CommitPaintExt"))
           {
               if (getSelectedImage() == ViewJComponentBase.BOTH)
               {
                   if (triImage[AXIAL_AB] != null)
                   {
                       triImage[AXIAL_AB].commitMask(ViewJComponentBase.BOTH, true, false);
                       triImage[AXIAL_AB].getActiveImage().notifyImageDisplayListeners(null, true);
                   }
                   else
                   {
                       //triImage[AXIAL_A].commitMask(ViewJComponentBase.BOTH, true, false);
                   }
                   
               }
               else if (getSelectedImage() == ViewJComponentBase.IMAGE_A)
               {
                   if (triImage[AXIAL_A] != null)
                   {
                       triImage[AXIAL_A].commitMask(ViewJComponentBase.IMAGE_A, true, false);
                       triImage[AXIAL_A].getActiveImage().notifyImageDisplayListeners(null, true);
                   }
                   //triImage[AXIAL_A].getImageA().notifyImageDisplayListeners(null, true);
               }
               else if (getSelectedImage() == ViewJComponentBase.IMAGE_B)
               {
            	   // must set IMAGE_A because in the AXIAL_B, SAGITTAL_B, and CORONAL_B images,
                   // imageB is in the imageA slot
                   if (triImage[AXIAL_B] != null)
                   {
                       triImage[AXIAL_B].commitMask(ViewJComponentBase.IMAGE_A, true, false);
                       triImage[AXIAL_B].getImageA().notifyImageDisplayListeners(null, true);
                   }
                   //updateImages(true);
               }
           }
           else if (command.equals("PaintCan"))
           {
               for (int i = 0; i < MAX_TRI_IMAGES; i++)
               {
                   if (triImage[i] != null)
                   {
                       triImage[i].setMode(ViewJComponentBase.PAINT_CAN);
                   }
               }

               if (triImage[AXIAL_A].growDialog != null && ( (JDialogPaintGrow) triImage[AXIAL_A].growDialog).isShowing())
               {
                   return;
               }

               Vector listeners = new Vector();

               for (int i = 0; i < MAX_TRI_IMAGES; i++)
               {
                   if (triImage[i] != null)
                   {
                       listeners.add(triImage[i]);
                   }
               }

               JDialogPaintGrow jdpg = new JDialogPaintGrow(this, listeners);

               for (int i = 0; i < MAX_TRI_IMAGES; i++)
               {
                   if (triImage[i] != null)
                   {
                       triImage[i].growDialog = jdpg;
                   }
               }
           }
           else if (command.equals("UndoPaint"))
           {
               for (int i = 0; i < MAX_TRI_IMAGES; i++)
               {
                   if (triImage[i] != null)
                   {
                       triImage[i].undoLastPaint();
                   }
               }

               if (triImage[AXIAL_A] != null)
               {
                   triImage[AXIAL_A].getImageA().notifyImageDisplayListeners(null, true);
               }
               if (triImage[AXIAL_B] != null)
               {
                   triImage[AXIAL_B].getImageB().notifyImageDisplayListeners(null, true);
               }
           }
           else if (command.equals("CalcPaint"))
           {
               if (triImage[AXIAL_A] != null)
               {
                   triImage[AXIAL_A].calcPaintedVolume(null);
               }
           }
           else if (command.equals("UnMagImage"))
           {
        	   float oldZoom = zoom;
        	   
               if (zoomMode == ViewJComponentEditImage.LINEAR && triImage[AXIAL_A].getZoomX() > 1.0f)
               {
                   // linear zoom is prevented if getZoomX() <= 1.0
                   zoom = triImage[AXIAL_A].getZoomX() - 1.0f;
               }
               else
               {
                   zoom = 0.5f * triImage[AXIAL_A].getZoomX();
               }
               for (int i = 0; i < MAX_TRI_IMAGES; i++)
               {
                   if (triImage[i] != null)
                   {
                	   triImage[i].setZoom(zoom, zoom);
                       
                       Point oldCrosshairPoint = triImage[i].getCrosshairPoint();
                       
                       if (oldCrosshairPoint != null)
                       {
                    	   int newX = MipavMath.round((oldCrosshairPoint.x * zoom) / oldZoom);
                    	   int newY = MipavMath.round((oldCrosshairPoint.y * zoom) / oldZoom);
                    	   
                    	   triImage[i].updateCrosshairPosition(newX, newY);
                    	   
                    	   adjustScrollbars(newX, newY, scrollPane[i]);
                       }
                   }
               }
               validate();
               updateImages(true);
               setTitle();
           }
           else if (command.equals("MagImage"))
           {
        	   float oldZoom = zoom;
        	   
               if (zoomMode == ViewJComponentEditImage.LINEAR && triImage[AXIAL_A] != null)
               {
                   zoom = triImage[AXIAL_A].getZoomX() + 1.0f;
               }
               else if (triImage[AXIAL_A] != null) // zoomMode == ViewJComponentEditImage.EXPONENTIAL
               {
                   zoom = 2.0f * triImage[AXIAL_A].getZoomX();
               }
               
               for (int i = 0; i < MAX_TRI_IMAGES; i++)
               {
                   if (triImage[i] != null)
                   {
                       triImage[i].setZoom(zoom, zoom);
                       
                       Point oldCrosshairPoint = triImage[i].getCrosshairPoint();
                       
                       if (oldCrosshairPoint != null)
                       {
                    	   int newX = MipavMath.round((oldCrosshairPoint.x * zoom) / oldZoom);
                    	   int newY = MipavMath.round((oldCrosshairPoint.y * zoom) / oldZoom);
                    	   
                    	   triImage[i].updateCrosshairPosition(newX, newY);
                    	   
                    	   adjustScrollbars(newX, newY, scrollPane[i]);
                       }
                   }
               }
               validate();
               updateImages(true);
               setTitle();
           }
           else if (command.equals("ZoomOne"))
           {
               for (int i = 0; i < MAX_TRI_IMAGES; i++)
               {
                   if (triImage[i] != null)
                   {
                	   float oldZoom = zoom;
                	   
                       triImage[i].setZoom(1, 1);
                       
                       Point oldCrosshairPoint = triImage[i].getCrosshairPoint();
                       
                       if (oldCrosshairPoint != null)
                       {
                    	   int newX = (int) (oldCrosshairPoint.x / oldZoom);
                    	   int newY = (int) (oldCrosshairPoint.y / oldZoom);
                    	   
                    	   triImage[i].updateCrosshairPosition(newX, newY);
                    	   
                    	   adjustScrollbars(newX, newY, scrollPane[i]);
                       }
                   }
               }
               validate();
               updateImages(true);
               setTitle();
           }
           else if (command.equals("Zoom linearly"))
           {
               zoomMode = ViewJComponentEditImage.LINEAR;
           }
           else if (command.equals("Zoom exponentially"))
           {
               zoomMode = ViewJComponentEditImage.EXPONENTIAL;
           }
           else if (command.equals("createTransformation"))
           {
               JDialogTriImageTransformation dialog;
               float originalZoom = triImage[AXIAL_A].getZoomX();

               if (getSelectedImage() == ViewJComponentBase.IMAGE_A)
               {
                   dialog = new JDialogTriImageTransformation(this, imageA);
                   if (!dialog.doNew())
                   {
                       setImageA(imageA);
                       for (int i = 0; i < MAX_TRI_IMAGES; i++)
                       {
                           if (triImage[i] != null)
                           {
                               triImage[i].setZoom(originalZoom, originalZoom);
                           }
                       }

                       if (triImage[AXIAL_A] != null)
                       {
                           triImage[AXIAL_A].getActiveImage().notifyImageDisplayListeners(null, true);
                       }
                       if (triImage[AXIAL_B] != null)
                       {
                           triImage[AXIAL_B].getActiveImage().notifyImageDisplayListeners(null, true);
                       }
                   }
               }
               else if (getSelectedImage() == ViewJComponentBase.IMAGE_B)
               {
                   dialog = new JDialogTriImageTransformation(this, imageB);
                   if (!dialog.doNew())
                   {
                       setImageB(imageB);
                       for (int i = 0; i < MAX_TRI_IMAGES; i++)
                       {
                           if (triImage[i] != null)
                           {
                               triImage[i].setZoom(originalZoom, originalZoom);
                           }
                       }

                       if (triImage[AXIAL_A] != null)
                       {
                           triImage[AXIAL_A].getActiveImage().notifyImageDisplayListeners(null, true);
                       }
                       if (triImage[AXIAL_B] != null)
                       {
                           triImage[AXIAL_B].getActiveImage().notifyImageDisplayListeners(null, true);
                       }
                   }
               }
               else if (getSelectedImage() == ViewJComponentBase.BOTH)
               {
                   dialog = new JDialogTriImageTransformation(this, imageA, imageB);
                   if (!dialog.doNew())
                   {
                       setImageA(imageA);
                       for (int i = 0; i < MAX_TRI_IMAGES; i++)
                       {
                           if (triImage[i] != null)
                           {
                               triImage[i].setZoom(originalZoom, originalZoom);
                           }
                       }
                       setImageB(imageB);
                       for (int i = 0; i < MAX_TRI_IMAGES; i++)
                       {
                           if (triImage[i] != null)
                           {
                               triImage[i].setZoom(originalZoom, originalZoom);
                           }
                       }
                       if (triImage[AXIAL_A] != null)
                       {
                           triImage[AXIAL_A].getActiveImage().notifyImageDisplayListeners(null, true);
                       }
                       if (triImage[AXIAL_B] != null)
                       {
                           triImage[AXIAL_B].getActiveImage().notifyImageDisplayListeners(null, true);
                       }
                   }
               }
           }
           else if (command.equals("addPoint"))
           {
               for (int i = 0; i < MAX_TRI_IMAGES; i++)
               {
                   if (triImage[i] != null)
                   {
                       triImage[i].setMode(ViewJComponentBase.POINT_VOI);
                   }
               }
           }
           else if (command.equals("NewVOI"))
           {
               for (int i = 0; i < MAX_TRI_IMAGES; i++)
               {
                   if (triImage[i] != null)
                   {
                       triImage[i].setMode(ViewJComponentBase.NEW_VOI);
                   }
               }
           }
           else if (command.equals("deleteVOI"))
           {               
               for (int i = 0; i < MAX_TRI_IMAGES; i++)
               {
            	   if (triImage[i] != null)
            	   {
            		   triImage[i].deleteSelectedContours();
            	   }
               }
               
               imageA.notifyImageDisplayListeners();
               if (imageB != null) imageB.notifyImageDisplayListeners();
           }
           else if (command.equals("boundingBox"))
           {
               // this block of code is the logic for emulating the behavior
               // of the button group containing the invisible button
               boolean showBoundingRect = !triImage[AXIAL_A].isShowBoundingRect();

               btnInvisible[1].setSelected(!showBoundingRect);

               for (int i = 0; i < MAX_TRI_IMAGES; i++)
               {
                   if (triImage[i] != null)
                   {
                       triImage[i].setShowBoundingRect(showBoundingRect);
                       if (showBoundingRect)
                       {
                           triImage[i].setMode(ViewJComponentBase.CUBE_BOUNDS);
                       }
                       else
                       {
                           // this means the toggle button was un-pressed, so reset to DEFAULT mode
                           triImage[i].setMode(ViewJComponentBase.DEFAULT);
                       }
                       triImage[i].setDoCenter(false);
                       triImage[i].deleteAllVOIs();
                       triImage[i].setProtractorVisible(false);
                       triImage[i].setIntensityLineVisible(false);
                   }
               }

               // un-push intensityLine, center mark, and protractor button
               btnInvisible[2].setSelected(true);
               btnInvisible[3].setSelected(true);

               updateImages(true);
           }
           else if (command.equals("cropVolume"))
           {
               JDialogCrop dialogCrop;

               if (radioImageB.isSelected())
               {
                   if (imageB != null)
                   {
                       dialogCrop = new JDialogCrop(imageB);
                   }
                   else
                   {
                       dialogCrop = new JDialogCrop(imageA);
                   }
               }
               else
               {
                   dialogCrop = new JDialogCrop(imageA);
               }

               dialogCrop.setSeparateThread(true);

               int [] xBounds = new int[] {boundingBoxPoints[UPPER_LEFT_FRONT].x, boundingBoxPoints[UPPER_RIGHT_FRONT].x};
               int [] yBounds = new int[] {boundingBoxPoints[UPPER_LEFT_FRONT].y, boundingBoxPoints[LOWER_LEFT_FRONT].y};
               int [] zBounds = new int[] {boundingBoxPoints[UPPER_LEFT_FRONT].z, boundingBoxPoints[UPPER_LEFT_BACK].z};

               dialogCrop.setXBounds(xBounds);
               dialogCrop.setYBounds(yBounds);
               dialogCrop.setZBounds(zBounds);

               dialogCrop.callAlgorithm();

               if (radioImageBoth.isSelected())
               {
                   dialogCrop = new JDialogCrop(imageB);

                   dialogCrop.setSeparateThread(true);

                   dialogCrop.setXBounds(xBounds);
                   dialogCrop.setYBounds(yBounds);
                   dialogCrop.setZBounds(zBounds);

                   dialogCrop.callAlgorithm();
               }
           }
           else if (command.equals("LinkFrame"))
           {
               if (isMultipleSameSizeTriImages() == true)
               {
                   new JDialogTriFrameLinker(this, triImage[AXIAL_A].getActiveImage()); // todo:  fix
               }
               else
               {
                   MipavUtil.displayError(
                       "There is no image with the same dimensions as\n"
                       + triImage[AXIAL_A].getActiveImage().getImageName() + " to operate on."); // todo:  fix
                   return;
               }
           }
           else if (source == radioImageA || source == radioImageB || source == radioImageBoth)
           {
               if (radioImageA.isSelected())
               {
                   setSpinnerValues(imageA.getType());
                   parentFrame.setActiveImage(ViewJComponentBase.IMAGE_A);
                   setImageActiveInTriComponents(ViewJComponentBase.IMAGE_A);
               }
               else if (radioImageB.isSelected())
               {
                   setSpinnerValues(imageB.getType());
                   parentFrame.setActiveImage(IMAGE_B);
                   setImageActiveInTriComponents(ViewJComponentBase.IMAGE_B);
               }

               ( (SpinnerNumberModel) (intensitySpinner.getModel())).setMinimum(new Double(spinnerMin));
               ( (SpinnerNumberModel) (intensitySpinner.getModel())).setMaximum(new Double(spinnerMax));
               ( (SpinnerNumberModel) (intensitySpinner.getModel())).setStepSize(new Double(spinnerStep));
               ( (SpinnerNumberModel) (intensitySpinner.getModel())).setValue(new Double(spinnerDefaultValue));

               /** updateImages(false) was commented to facilitate placement of VOI points during
                * image registration. This allows the user to control the crosshairs and image slice
                * independently of other images.
                */
               //updateImages();
               return;
           }
           else if (command.equals("PositionFrame"))
           {
               if (volumePositionFrame != null)
               {
                   MipavUtil.centerOnScreen(volumePositionFrame);
                   volumePositionFrame.setVisible(true);
                   volumePositionFrame.toFront();
               }
               else
               {
                   volumePositionFrame = new VolumePositionFrame(this);
                   // make sure the position frame has the same state as the plug-in
                   // panel's position panel, because logically, it is possible
                   // for both to exist simultaneously (although that would be pointless)
                   volumePositionFrame.labelXPos.setText(labelXPos.getText());
                   volumePositionFrame.labelYPos.setText(labelYPos.getText());
                   volumePositionFrame.labelZPos.setText(labelZPos.getText());
                   volumePositionFrame.labelXRef.setText(labelXRef.getText());
                   volumePositionFrame.labelYRef.setText(labelYRef.getText());
                   volumePositionFrame.labelZRef.setText(labelZRef.getText());
                   volumePositionFrame.labelXTal.setText(labelXTal.getText());
                   volumePositionFrame.labelYTal.setText(labelYTal.getText());
                   volumePositionFrame.labelZTal.setText(labelZTal.getText());
                   volumePositionFrame.setShowTalairachGrid(chkShowTalairachGrid.isSelected());
                   volumePositionFrame.setShowTalairachGridMarkers(chkShowTalairachGridMarkers.isSelected());
                   volumePositionFrame.setVisible(true);
               }
           }
           this.requestFocusInWindow();
       }

       protected void handleTPSpline()
       {
		   if (imageB == null)
		   {
			   MipavUtil.displayError("Image B must be present to use the least squares algorithm.");
	
			   return;
		   }
		   
		   ViewVOIVector imageAVOIs = (ViewVOIVector) imageA.getVOIs();
		   ViewVOIVector imageBVOIs = (ViewVOIVector) imageB.getVOIs();
		   
		   if (imageAVOIs != null && imageBVOIs != null && 
			   imageAVOIs.size() != imageBVOIs.size())
		   {
			   MipavUtil.displayError("Number of VOI points must be identical in each image.");
			   return;
		   }
		   
		   if (imageAVOIs.size() < 3)
		   {
			   MipavUtil.displayError("At least three points must be in place for each image.");
			   return;
		   }
		   
		   double [] xSourceA = new double[imageAVOIs.size()];
	       double [] ySourceA = new double[imageAVOIs.size()];
	       double [] zSourceA = new double[imageAVOIs.size()];
	       double [] xTargetB = new double[imageBVOIs.size()];
	       double [] yTargetB = new double[imageBVOIs.size()];
	       double [] zTargetB = new double[imageBVOIs.size()];
		   
		   // extract point VOIs for image A
		   Vector pointVOIVector = new Vector();		   
		   int nVOI = imageAVOIs.size();
		   for (int i = nVOI - 1; i >= 0; i--)
		   {
			   if (imageAVOIs.VOIAt(i).getCurveType() == VOI.POINT)
		       {
				   for (int k = 0; k < imageA.getExtents()[2]; k++)
		           {
		               Point3Df[] voiPoints = imageAVOIs.VOIAt(i).exportPoints(k);
		               
		               for (int j = 0; j < voiPoints.length; j++)
		               {
		            	   pointVOIVector.add(voiPoints);
		               }
		           }
		       }
		   }
		   
		   for (int i = 0; i < pointVOIVector.size(); i++)
		   {
			   Point3Df [] point3df = (Point3Df[]) pointVOIVector.elementAt(i);
			   xSourceA[i] = point3df[0].x;
			   ySourceA[i] = point3df[0].y;
			   zSourceA[i] = point3df[0].z;
		   }
		   
		   // extract point VOIs for image B
		   pointVOIVector = new Vector();
		   nVOI = imageBVOIs.size();
		   for (int i = nVOI - 1; i >= 0; i--)
		   {
			   if (imageBVOIs.VOIAt(i).getCurveType() == VOI.POINT)
		       {
				   for (int k = 0; k < imageA.getExtents()[2]; k++)
		           {
		               Point3Df[] voiPoints = imageBVOIs.VOIAt(i).exportPoints(k);
		               
		               for (int j = 0; j < voiPoints.length; j++)
		               {
		            	   pointVOIVector.add(voiPoints);
		               }
		           }
		       }
		   }
		
		   for (int i = 0; i < pointVOIVector.size(); i++)
		   {
			   Point3Df [] point3df = (Point3Df[]) pointVOIVector.elementAt(i);
			   xTargetB[i] = point3df[0].x;
			   yTargetB[i] = point3df[0].y;
			   zTargetB[i] = point3df[0].z;
		   }
		   		   
		   parentFrame.setActiveImage(ViewJComponentBase.IMAGE_A);
		   ModelImage clonedImage = (ModelImage) imageB.clone();
		   
		   AlgorithmTPSpline algoTPSpline = new AlgorithmTPSpline(xSourceA, ySourceA, zSourceA, xTargetB, yTargetB, zTargetB, 0.0f, imageB, clonedImage);
		   algoTPSpline.setActiveImage(false);
		   algoTPSpline.setupTPSpline2D(xSourceA, ySourceA, xTargetB, yTargetB, 0.0f);
		   algoTPSpline.runAlgorithm();
		   
		   if (imageB != null)
		   {
			   imageB.disposeLocal();
			   imageB = null;
		   }
		   
		   if (clonedImage != null)
		   {
			   clonedImage.disposeLocal();
			   clonedImage = null;
		   }
		   
		   ModelImage newImageB = algoTPSpline.getResultImage();
		   
		   parentFrame.setImageB(newImageB);
				   
    	   return;
       }
       
	/**
	 * Method handles transformations for least squares algorithm in the tri-planar frame.
	 */
	protected void handleLeastSquares() 
	{
		final SwingWorker worker = new SwingWorker() 
		{
			public Object construct() 
			{
				ViewJProgressBar progressBar = null;
				
				try
				{
				   if (imageB == null)
				   {
					   MipavUtil.displayError("Image B must be present to use the least squares algorithm.");
					   return null;
				   }
				   
				   ViewVOIVector imageAVOIs = (ViewVOIVector) imageA.getVOIs();
				   ViewVOIVector imageBVOIs = (ViewVOIVector) imageB.getVOIs();
				   
				   if (imageAVOIs != null && imageBVOIs != null && 
					   imageAVOIs.size() != imageBVOIs.size())
				   {
					   MipavUtil.displayError("Number of VOI points must be identical in each image.");
					   return null;
				   }
				   
				   if (imageAVOIs.size() < 3)
				   {
					   MipavUtil.displayError("At least three VOI points must be in place for each image.");
					   return null;
				   }
				   
				   // extract point VOIs for image A
				   Vector pointVOIVector = new Vector();		   
				   int nVOI = imageAVOIs.size();
				   for (int i = nVOI - 1; i >= 0; i--)
				   {
					   if (imageAVOIs.VOIAt(i).getCurveType() == VOI.POINT)
				       {
						   for (int k = 0; k < imageA.getExtents()[2]; k++)
				           {
				               Point3Df[] voiPoints = imageAVOIs.VOIAt(i).exportPoints(k);
				               
				               for (int j = 0; j < voiPoints.length; j++)
				               {
				            	   pointVOIVector.add(voiPoints);
				               }
				           }
				       }
				   }
				   
				   double [][] coordsA = new double[pointVOIVector.size()][3];
				   for (int i = 0; i < pointVOIVector.size(); i++)
				   {
					   Point3Df [] point3df = (Point3Df[]) pointVOIVector.elementAt(i);
					   coordsA[i][0] = point3df[0].x;
					   coordsA[i][1] = point3df[0].y;
					   coordsA[i][2] = point3df[0].z;
				   }
				   
				   // extract point VOIs for image B
				   pointVOIVector = new Vector();
				   nVOI = imageBVOIs.size();
				   for (int i = nVOI - 1; i >= 0; i--)
				   {
					   if (imageBVOIs.VOIAt(i).getCurveType() == VOI.POINT)
				       {
						   for (int k = 0; k < imageA.getExtents()[2]; k++)
				           {
				               Point3Df[] voiPoints = imageBVOIs.VOIAt(i).exportPoints(k);
				               
				               for (int j = 0; j < voiPoints.length; j++)
				               {
				            	   pointVOIVector.add(voiPoints);
				               }
				           }
				       }
				   }
				
				   double [][] coordsB = new double[pointVOIVector.size()][3];
				   for (int i = 0; i < pointVOIVector.size(); i++)
				   {
					   Point3Df [] point3df = (Point3Df[]) pointVOIVector.elementAt(i);
					   coordsB[i][0] = point3df[0].x;
					   coordsB[i][1] = point3df[0].y;
					   coordsB[i][2] = point3df[0].z;
				   }
		     	   
				   // now that we have the point coords, we can build the least squares algorithm
				   AlgorithmRegLeastSquares algoLeastSquares = new AlgorithmRegLeastSquares(coordsA, coordsB, 3);
				   algoLeastSquares.runAlgorithm();
				   TransMatrix transMatrix = algoLeastSquares.getTransformBtoA();
				   
				   if (algoLeastSquares.isCompleted() == false)
				   {
					   // algorithm failed
					   return null;
				   }
				           	   
				   progressBar = new ViewJProgressBar("Applying transformation", 
						      "Transforming", 0, 100,
				              false, null, null);
				   progressBar.setVisible(true);
				   progressBar.setSeparateThread(true);
				   
				   ModelImage resultImage = (ModelImage) imageB.clone();
				   
				   AlgorithmTransform.transformTrilinear(imageB, resultImage, transMatrix, progressBar);
				   
				   imageB.disposeLocal();
				   
				   parentFrame.setImageB(resultImage);
				   parentFrame.setActiveImage(ViewJComponentBase.IMAGE_A);
				   
				   JDialogImageInfo infoDialogB = parentFrame.getImageInfoDialogB();
				   infoDialogB.setMatrix(transMatrix.getMatrix());
				   
				   updateImages();
				   return null;
				}
				finally
				{
					if (progressBar != null)
					{
						progressBar.dispose();
					}
				}
			}
		};
		
		worker.start();
	}

       /**
        * Builds the volume position panel, which is the panel that sits in the plug-in area
        * of the 2x2 tri-planar layout
        * @return JPanel the JPanel that has been constructed
        */
       protected JPanel buildVolumePositionPanel()
    {
        JPanel volumePositionPanel = new JPanel();

        JTabbedPane tabbedPane = new JTabbedPane();

        GridBagLayout gbLayout = new GridBagLayout();
        GridBagConstraints gbConstraints = new GridBagConstraints();
        JPanel volumePanel = new JPanel(gbLayout);
        volumePanel.setBorder(BorderFactory.createTitledBorder("Volume coordinates"));

        gbConstraints.anchor = GridBagConstraints.EAST;
        volumePanel.add(new JLabel("X: "), gbConstraints);
        gbConstraints.anchor = GridBagConstraints.WEST;
        gbConstraints.gridx = 1;
        volumePanel.add(labelXPos, gbConstraints);

        gbConstraints.gridy = 1;
        gbConstraints.gridx = 0;
        gbConstraints.anchor = GridBagConstraints.EAST;
        volumePanel.add(new JLabel("Y: "), gbConstraints);
        gbConstraints.anchor = GridBagConstraints.WEST;
        gbConstraints.gridx = 1;
        volumePanel.add(labelYPos, gbConstraints);

        gbConstraints.gridy = 2;
        gbConstraints.gridx = 0;
        gbConstraints.anchor = GridBagConstraints.EAST;
        volumePanel.add(new JLabel("Z: "), gbConstraints);
        gbConstraints.anchor = GridBagConstraints.WEST;
        gbConstraints.gridx = 1;
        volumePanel.add(labelZPos, gbConstraints);       

        gbLayout = new GridBagLayout();
        gbConstraints = new GridBagConstraints();
        JPanel talairachPanel = new JPanel(gbLayout);

        chkShowTalairachGrid = new JCheckBox("Show Talairach grid");
        chkShowTalairachGrid.setActionCommand("ShowTalairachGrid");
        chkShowTalairachGrid.addActionListener(this);
        chkShowTalairachGrid.setFont(MipavUtil.font12B);
        chkShowTalairachGridMarkers = new JCheckBox("Show Talairach grid markers");
        chkShowTalairachGridMarkers.setActionCommand("ShowTalairachGridmarkers");
        chkShowTalairachGridMarkers.setEnabled(chkShowTalairachGrid.isSelected());
        chkShowTalairachGridMarkers.addActionListener(this);
        chkShowTalairachGridMarkers.setFont(MipavUtil.font12B);

        gbConstraints.anchor = GridBagConstraints.WEST;
        gbConstraints.weightx = 1;
        talairachPanel.add(chkShowTalairachGrid, gbConstraints);

        gbConstraints.gridy = 1;
        talairachPanel.add(chkShowTalairachGridMarkers, gbConstraints);

        GridBagLayout gbSubLayout = new GridBagLayout();
        GridBagConstraints gbSubConstraints = new GridBagConstraints();

        JPanel talairachSubPanel = new JPanel(gbSubLayout);
        talairachSubPanel.setBorder(BorderFactory.createTitledBorder("Talairach grid coordinates"));

        gbConstraints.gridy++;
        gbConstraints.gridwidth = 3;
        gbConstraints.fill = GridBagConstraints.BOTH;
        gbConstraints.weighty = 1;
        talairachPanel.add(talairachSubPanel, gbConstraints);

        gbSubConstraints.gridwidth = GridBagConstraints.RELATIVE;
        gbSubConstraints.gridy = 1;
        gbSubConstraints.weightx = 1;
        gbSubConstraints.anchor = GridBagConstraints.WEST;
        gbSubConstraints.insets = new Insets(0, 20, 0, 0);
        talairachSubPanel.add(new JLabel("X: "), gbSubConstraints);
        gbSubConstraints.gridx = 1;
        talairachSubPanel.add(labelXTal, gbSubConstraints);

        gbSubConstraints.gridx = GridBagConstraints.RELATIVE;
        gbSubConstraints.gridy = 2;
        talairachSubPanel.add(new JLabel("Y: "), gbSubConstraints);
        gbSubConstraints.gridx = 1;
        talairachSubPanel.add(labelYTal, gbSubConstraints);

        gbSubConstraints.gridx = GridBagConstraints.RELATIVE;
        gbSubConstraints.gridy = 3;
        talairachSubPanel.add(new JLabel("Z: "), gbSubConstraints);
        gbSubConstraints.gridx = 1;
        talairachSubPanel.add(labelZTal, gbSubConstraints);

        gbSubConstraints.gridx = GridBagConstraints.RELATIVE;
        gbSubConstraints.gridy = 4;
        talairachSubPanel.add(new JLabel("Talairach voxel: "), gbSubConstraints);
        gbSubConstraints.gridx = 1;
        talairachSubPanel.add(talairachVoxelLabel, gbSubConstraints); 

        float[] tCoord = new float[3];
        imageA.getScannerCoordLPS(triImage[AXIAL_A].getActiveImage().getExtents()[0] / 2,
                               triImage[AXIAL_A].getActiveImage().getExtents()[1] / 2,
                               triImage[AXIAL_A].getActiveImage().getExtents()[2] / 2, tCoord);

        if (tCoord[0] < 0)
        {
            scannerLabelX.setText("R: ");
        }
        else
        {
            scannerLabelX.setText("L: ");
        }

        if (tCoord[1] < 0)
        {
            scannerLabelY.setText("A: ");
        }
        else
        {
            scannerLabelY.setText("P: ");
        }

        if (tCoord[2] < 0)
        {
            scannerLabelZ.setText("I: ");
        }
        else
        {
            scannerLabelZ.setText("S: ");
        }

        gbLayout = new GridBagLayout();
        gbConstraints = new GridBagConstraints();
        JPanel scannerPanel = new JPanel(gbLayout);
        scannerPanel.setBorder(BorderFactory.createTitledBorder("Scanner position"));

        gbConstraints.anchor = GridBagConstraints.EAST;
        scannerPanel.add(scannerLabelX, gbConstraints);
        gbConstraints.anchor = GridBagConstraints.WEST;
        gbConstraints.gridx = 1;
        scannerPanel.add(labelXRef, gbConstraints);

        gbConstraints.gridx = 0;
        gbConstraints.gridy = 1;
        gbConstraints.anchor = GridBagConstraints.EAST;
        scannerPanel.add(scannerLabelY, gbConstraints);
        gbConstraints.anchor = GridBagConstraints.WEST;
        gbConstraints.gridx = 1;
        scannerPanel.add(labelYRef, gbConstraints);

        gbConstraints.gridx = 0;
        gbConstraints.gridy = 2;
        gbConstraints.anchor = GridBagConstraints.EAST;
        scannerPanel.add(scannerLabelZ, gbConstraints);
        gbConstraints.anchor = GridBagConstraints.WEST;
        gbConstraints.gridx = 1;
        scannerPanel.add(labelZRef, gbConstraints);

        tabbedPane.add("Volume", volumePanel);
        tabbedPane.add("Scanner", scannerPanel);
        tabbedPane.add("Talairach", talairachPanel);

        volumePositionPanel.setLayout(new GridLayout(1, 1));
        volumePositionPanel.add(tabbedPane);

        return volumePositionPanel;
    }

    /**
     * This method will load a plug-in into the plug-in area of the 
     * tri-planar frame. The plug-in must extend java.awt.Component,
     * and take this class as the only parameter to its constructor.
     */
    protected void handlePluginPanelSelection()
    {
        if (oldLayout == false)
        {
            int response = JOptionPane.showConfirmDialog(this, "You must be using the 2x2 layout to enable this feature. Change to this layout?",
                                          "Change layout?", JOptionPane.YES_NO_OPTION);

            if (response == JOptionPane.YES_OPTION)
            {
                oldLayout = true;
                updateLayout();
            }
            else
            {
                return;
            }
        }

        JFileChooser fileChooser = new JFileChooser();
        fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        fileChooser.setMultiSelectionEnabled(false);
        int response = fileChooser.showOpenDialog(this);

        if (response == JFileChooser.APPROVE_OPTION)
        {
            File selectedFile = fileChooser.getSelectedFile();

            try
            {
                String filename = selectedFile.getName();
                filename = filename.substring(0, filename.lastIndexOf('.'));
                Class pluginClass = Class.forName(filename);

                Constructor pluginConstructor = pluginClass.getConstructor(new Class [] {getClass()});
                pluginPanel = (Component) pluginConstructor.newInstance(new Object [] {this});
                Preferences.debug("Tri-planar plug-in loaded OK", Preferences.DEBUG_MINOR);
            }
            catch (NoSuchMethodException nsme)
            {
            	MipavUtil.displayError("Class loading failed! The plug-in class must take ViewJFrameTriImage as an argument to its constructor.");
            	return;
            }
            catch (ClassCastException cce)
            {
            	MipavUtil.displayError("Class loading failed! The plug-in class must extend java.awt.Component.");
            	return;
            }
            catch (Exception e)
            {
                MipavUtil.displayError("Class loading failed!");
                return;
            }
        }
        else
        {
            return;
        }

        updateLayout();

        return;
    }

    /**
     *   Helper method to establish if there are images of the
     *   same dimensionality so that a dialog can be created.
     *   Used by image calculator, concat, etc.
     *   @return <code>true</code> if there are images to operate on.
     */
    protected boolean isMultipleSameSizeTriImages()
    {
        Enumeration registeredImageNames = userInterface.getRegisteredImageNames();
        boolean createDialog = false;
        String activeImageName;
        ModelImage activeImage;

        if (imageB == null)
        {
            activeImage = triImage[AXIAL_A].getImageA();
        }
        else
        {
            activeImage = triImage[AXIAL_AB].getActiveImage();
        }

        activeImageName = activeImage.getImageName();

        // Add images from user interface that have the same exact dimensionality
        while (registeredImageNames.hasMoreElements())
        {
            String registeredImageName = (String) registeredImageNames.nextElement();
            if (!activeImageName.equals(registeredImageName))
            {
                try
                {
                    ModelImage img = userInterface.getRegisteredImageByName(registeredImageName);
                    if (img.getTriImageFrame() != null)
                    {
                        if (activeImage.getNDims() == img.getNDims())
                        {
                            int counter = 0;
                            for (int j = 0; j < activeImage.getNDims(); j++)
                            {
                                if (activeImage.getExtents()[j] == img.getExtents()[j])
                                {
                                    counter++;
                                }
                            }
                            if (counter == activeImage.getNDims())
                            {
                                createDialog = true;
                            }
                        }
                    }
                }
                catch (IllegalArgumentException iae)
                {
                    //MipavUtil.displayError("There was a problem with the supplied name.\n" );
                    Preferences.debug(
                        "Illegal Argument Exception in " + "ViewJFrameTriImage.isMultipleSameSizeTriImages(). "
                        + "Somehow the Image list sent an incorrect name to " + "the image image hashtable. " + "\n",
                        1);
                }
            }
        }
        return createDialog;
    }

    /**
     *   Closes window and disposes of frame and component
     *   @param event    Event that triggered function
     */
    public void windowClosing(WindowEvent event)
    {
        if (volumePositionFrame != null)
        {
            volumePositionFrame.dispose();
            volumePositionFrame = null;
        }
        close();
        this.disposeLocal();
    }

    protected void finalize() throws Throwable
    {
        disposeLocal();
        super.finalize();
    }

    /**
     * Should be called when window is closing to perform cleanup.
     */
    public void disposeLocal()
    {
    	if (volumePositionFrame != null)
    	{
    		volumePositionFrame.dispose();
    		volumePositionFrame = null;
    	}
    	
        for (int i = 0; i < MAX_TRI_IMAGES; i++)
        {
            if (triImage[i] != null)
            {
                triImage[i].disposeLocal(true);
                triImage[i] = null;
            }
        }

        colorChooser = null;
        controls = null;

        parentFrame.triPlanarClosing();
    }

    /**
     * Builds menus for the tri-planar view
     * @return  the tri-image frame menu bar
     */
    protected JMenuBar buildMenu()
    {

        JSeparator separator = new JSeparator();

        int id = imageA.getFileInfo()[0].getTransformID();
        this.showTalairachPosition = (id == FileInfoBase.TRANSFORM_TALAIRACH_TOURNOUX &&
                                      imageA.getTalairachTransformInfo() != null);

        menuObj = new ViewMenuBuilder(this);
        JMenuBar menuBar = new JMenuBar();
        menuBar.add(
            menuObj.makeMenu("File", false,
                             new JComponent[]
                             {separator, menuObj.buildMenuItem("Close frame", "CloseFrame", 0, null, false)}));
        menuBar.add(
            menuObj.makeMenu("Options", false,
                             new JComponent[]
                             {
                             menuObj.buildCheckBoxMenuItem("Show axes", "ShowAxes", true),
                             menuObj.buildCheckBoxMenuItem("Show crosshairs", "ShowXHairs", true),
                             separator,
                             menuObj.buildCheckBoxMenuItem("Fast rendering in paint mode", "FastPaint",
            Preferences.is(Preferences.PREF_FAST_TRIPLANAR_REPAINT)),
                             menuObj.buildCheckBoxMenuItem("Use 2x2 tri-planar layout", OLD_LAYOUT, oldLayout),
                             separator,
                             menuObj.buildMenuItem("Show volume coordinates and Talairach controls", "PositionFrame", 0, null, false),
                             menuObj.buildMenuItem("Link to another TriImage frame", "LinkFrame", 0, null, false),
                             menuObj.buildMenuItem("Select panel plug-in", PANEL_PLUGIN, 0, null, false)}));

        return menuBar;
    }

    /**
     *   Builds the toolbars for the tri-planar view
     */
    protected void buildToolbars()
    {
        toolbarBuilder = new ViewToolBarBuilder(this);
        buildActiveImagePanel();
        panelToolbar.setLayout(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.weighty = 1;

        for (int i = 0; i < NUM_INVISIBLE_BUTTONS; i++)
        {
            btnInvisible[i] = new JToggleButton("");
        }

        Border etchedBorder = BorderFactory.createEtchedBorder();
        ButtonGroup VOIGroup = new ButtonGroup();

        imageToolBar = new JToolBar();
        imageToolBar.setBorder(etchedBorder);
        imageToolBar.setBorderPainted(true);
        imageToolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        imageToolBar.setFloatable(false);

        traverseButton = toolbarBuilder.buildToggleButton("traverse", "Traverse image", "translate", VOIGroup);
        imageToolBar.add(traverseButton);

        imageToolBar.add(ViewToolBarBuilder.makeSeparator());
        imageToolBar.add(toolbarBuilder.buildButton("MagImage", "Magnify image 2.0x", "zoomin"));
        imageToolBar.add(toolbarBuilder.buildButton("UnMagImage", "Magnify image 0.5x", "zoomout"));
        imageToolBar.add(toolbarBuilder.buildButton("ZoomOne", "Magnify image 1.0x", "zoom1"));

        imageToolBar.add(ViewToolBarBuilder.makeSeparator());

        ButtonGroup intensityLineGroup = new ButtonGroup();
        imageToolBar.add(toolbarBuilder.buildToggleButton("Line", "Draw line VOI", "linear", intensityLineGroup));
        intensityLineGroup.add(btnInvisible[0]);

        imageToolBar.add(ViewToolBarBuilder.makeSeparator());

        ButtonGroup centerGroup = new ButtonGroup();
        imageToolBar.add(toolbarBuilder.buildToggleButton("Center", "Identify center of volume", "centerpt",
            centerGroup));

        centerGroup.add(btnInvisible[3]);

        imageToolBar.add(toolbarBuilder.buildToggleButton("Protractor", "Alignment tool", "protractor",
            intensityLineGroup));

        intensityLineGroup.add(btnInvisible[2]);

        imageToolBar.add(toolbarBuilder.buildTextButton("Apply", "Applies rotations and translations",
            "createTransformation"));
        imageToolBar.add(ViewToolBarBuilder.makeSeparator());

        addPointToggleButton = toolbarBuilder.buildToggleButton("addPoint", "Add point", "pointROI", VOIGroup);
        addPointToggleButton.addItemListener(this);
        imageToolBar.add(addPointToggleButton);
        //imageToolBar.add(toolbarBuilder.buildToggleButton("NewVOI", "Initiate new VOI", "newvoi", VOIGroup));
        imageToolBar.add(toolbarBuilder.buildButton("deleteVOI", "Delete point VOI", "delete"));
        
        leastSquaresButton = new JButton(MipavUtil.getIcon("reglsq.gif"));
        leastSquaresButton.addActionListener(this);
        leastSquaresButton.setToolTipText("Apply least squares alignment");
        leastSquaresButton.setActionCommand("leastSquares");
        leastSquaresButton.setBorderPainted(false);
        leastSquaresButton.setRolloverIcon(MipavUtil.getIcon("reglsqroll.gif"));
        leastSquaresButton.setFocusPainted(false);
        if (imageB == null)
        {
        	leastSquaresButton.setEnabled(false);
        }
        imageToolBar.add(leastSquaresButton);
        
        tpSplineButton = new JButton(MipavUtil.getIcon("regtsp.gif"));
        tpSplineButton.addActionListener(this);
        tpSplineButton.setToolTipText("Apply thin plate spline alignment");
        tpSplineButton.setActionCommand("tpSpline");
        tpSplineButton.setBorderPainted(false);
        tpSplineButton.setRolloverIcon(MipavUtil.getIcon("regtsproll.gif"));
        tpSplineButton.setFocusPainted(false);
        if (imageB == null)
        {
        	tpSplineButton.setEnabled(false);
        }
        imageToolBar.add(tpSplineButton);

        imageToolBar.add(ViewToolBarBuilder.makeSeparator());

        ButtonGroup oneButtonToggleGroup = new ButtonGroup();
        oneButtonToggleGroup.add(btnInvisible[1]);
        imageToolBar.add(toolbarBuilder.buildToggleButton("boundingBox", "Show/hide crop volume", "boundingcube",
            oneButtonToggleGroup));

        imageToolBar.add(toolbarBuilder.buildTextButton("Crop", "Crops image delineated by the bounding cube",
            "cropVolume"));
           
        panelToolbar.add(imageToolBar, gbc);

        // Paint toolbar
        paintToolBar = new JToolBar();
        paintToolBar.setBorder(etchedBorder);
        paintToolBar.setBorderPainted(true);
        paintToolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        paintToolBar.setFloatable(false);

        paintToolBar.add(toolbarBuilder.buildToggleButton("PaintBrush", "Draw using a brush", "brush", VOIGroup));
        dropperPaintToggleButton = toolbarBuilder.buildToggleButton("Dropper", "Picks up a color from the image", "dropper",
                VOIGroup);
        dropperPaintToggleButton.addItemListener(this);
        paintToolBar.add(dropperPaintToggleButton);
        
        paintCanToggleButton = toolbarBuilder.buildToggleButton("PaintCan", "Fills an area with desired color", "paintcan",
                VOIGroup);
        paintCanToggleButton.addItemListener(this);
        paintToolBar.add(paintCanToggleButton);
        
        paintToolBar.add(toolbarBuilder.buildToggleButton("Eraser", "Erases paint", "eraser", VOIGroup));
        paintToolBar.add(toolbarBuilder.buildButton("EraseAll", "Erase all paint", "clear"));
        paintToolBar.add(ViewToolBarBuilder.makeSeparator());

        ButtonGroup paintThicknessGroup = new ButtonGroup();
        paintToolBar.add(toolbarBuilder.buildToggleButton("ThinPaint", "Draw using small brush size (1px)", "thinpaint",
            paintThicknessGroup));
        paintToolBar.add(toolbarBuilder.buildToggleButton("MedPaint", "Draw using medium brush size (4px)", "medpaint",
            paintThicknessGroup));
        paintToolBar.add(toolbarBuilder.buildToggleButton("ThickPaint", "Draw using large brush size (8px)",
            "thickpaint", paintThicknessGroup));
        paintToolBar.add(toolbarBuilder.buildToggleButton("ThickestPaint", "Draw using largest brush size (16px)",
            "thickestpaint", paintThicknessGroup));

        paintToolBar.add(ViewToolBarBuilder.makeSeparator());

        if (!imageA.isColorImage())
        {
            if (imageB == null)
            {
                setSpinnerValues(imageA.getType());
            }
            else
            {
                setSpinnerValues(imageB.getType());
            }
        }
        intensitySpinner = new JSpinner(new SpinnerNumberModel(spinnerDefaultValue, spinnerMin, spinnerMax, spinnerStep));
        intensitySpinner.setMaximumSize(new Dimension(56, 24)); // 56 pixlls wide, 24 tall
        intensitySpinner.setPreferredSize(new Dimension(56, 24)); // 56 pixlls wide, 24 tall
        intensitySpinner.addChangeListener(this);
        intensitySpinner.setToolTipText("Paint intensity"); // bug in API prevents this from working
        if (imageA.isColorImage())
        {
            intensitySpinner.setEnabled(false);
        }
        paintToolBar.add(intensitySpinner);

        paintToolBar.add(ViewToolBarBuilder.makeSeparator());

        colorPaintButton = toolbarBuilder.buildButton("colorPaint", "Change paint color", "colorpaint");
        colorPaintButton.setBackground(color);

        paintToolBar.add(colorPaintButton);
        paintToolBar.add(toolbarBuilder.buildTextButton("Opacity", "Change opacity of paint", "OpacityPaint"));
        paintToolBar.add(ViewToolBarBuilder.makeSeparator());
        paintToolBar.add(toolbarBuilder.buildButton("CommitPaint", "Changes image where painted inside", "paintinside"));
        paintToolBar.add(ViewToolBarBuilder.makeSeparator());
        paintToolBar.add(toolbarBuilder.buildButton("CommitPaintExt", "Changes image where painted outside",
            "paintoutside"));
        paintToolBar.add(ViewToolBarBuilder.makeSeparator());
        paintToolBar.add(toolbarBuilder.buildButton("UndoPaint", "Undo last region paint", "undopaint"));
        paintToolBar.add(ViewToolBarBuilder.makeSeparator());
        paintToolBar.add(toolbarBuilder.buildButton("CalcPaint", "Calculate volume of paint", "calc"));

        paintToolBar.setFloatable(false);

        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 100;
        gbc.weighty = 100;
        panelToolbar.add(paintToolBar, gbc);

        if ( (imageA.getNDims() == 4) || ( (imageB != null) && (imageB.getNDims() == 4)))
        {
            JPanel panelImageSlider = new JPanel();
            panelImageSlider.setLayout(new GridLayout(1, 1));
            panelImageSlider.setForeground(Color.black);
            TitledBorder borderImageSlider = new TitledBorder(" Time slice");
            borderImageSlider.setTitleColor(Color.black);
            borderImageSlider.setBorder(new EtchedBorder());
            panelImageSlider.setBorder(borderImageSlider);

            tDim = extents[3];
            tImageSlider = new JSlider(JSlider.HORIZONTAL, 0, 100, 50);
            tImageSlider.setMinorTickSpacing(Math.round(100.0f / (tDim - 1)));
            tImageSlider.setSnapToTicks(true);
            tImageSlider.setPaintTicks(true);
            tImageSlider.setPaintLabels(true);
            tImageSlider.setLabelTable(buildTImageSliderLabels(1, tDim));
            tImageSlider.setValue(1);
            tImageSlider.setValue(0);
            tImageSlider.addChangeListener(this);

            panelImageSlider.add(tImageSlider);
            gbc.gridx = 0;
            gbc.gridy = 4;
            gbc.gridwidth = 1;
            gbc.gridheight = 1;
            gbc.fill = GridBagConstraints.BOTH;
            gbc.anchor = GridBagConstraints.WEST;
            gbc.weightx = 100;
            gbc.weighty = 100;
            panelToolbar.add(panelImageSlider, gbc);
        }

        setImageSelectorPanelVisible(true);
    }

    /**
     *   Sets the spinner values based on image type.
     *   @param type Image type (BYTE, FLOAT, ...)
     */
    private void setSpinnerValues(int type)
    {
        spinnerDefaultValue = 1.0;
        if (type == ModelStorageBase.BOOLEAN)
        {
            spinnerMin = 0;
            spinnerMax = 1;
        }
        else if (type == ModelStorageBase.BYTE)
        {
            spinnerMin = -128;
            spinnerMax = 127;
            spinnerStep = 1;
        }
        else if (type == ModelStorageBase.UBYTE)
        {
            spinnerMin = 0;
            spinnerMax = 255;
            spinnerStep = 1;
        }
        else if (type == ModelStorageBase.SHORT)
        {
            spinnerMin = -32768;
            spinnerMax = 32767;
            spinnerStep = 1;
        }
        else if (type == ModelStorageBase.USHORT)
        {
            spinnerMin = 0;
            spinnerMax = 65535;
            spinnerStep = 1;
        }
        else if (type == ModelStorageBase.INTEGER)
        {
            spinnerMin = Integer.MIN_VALUE;
            spinnerMax = Integer.MAX_VALUE;
            spinnerStep = 1;
        }
        else if (type == ModelStorageBase.UINTEGER)
        {
            spinnerMin = 0;
            spinnerMax = 4294967295L;
            spinnerStep = 1;
        }
        else if (type == ModelStorageBase.LONG)
        {
            spinnerMin = Long.MIN_VALUE;
            spinnerMax = Long.MAX_VALUE;
            spinnerStep = 1;
        }
        else if (type == ModelStorageBase.FLOAT)
        {
            spinnerMin = -Float.MAX_VALUE;
            spinnerMax = Float.MAX_VALUE;
            spinnerStep = 0.1;
        }
        else if (type == ModelStorageBase.DOUBLE)
        {
            spinnerMin = -Double.MAX_VALUE;
            spinnerMax = Double.MAX_VALUE;
            spinnerStep = 1;
        }
    }

    // this method either shows or hides the image selector panel, based on the value of the parameter
    public void setImageSelectorPanelVisible(boolean visible)
    {
        if (visible == true)
        {
            if (imageB != null) // do not show if we don't have a mask
            {
                GridBagConstraints gbc = new GridBagConstraints();

                gbc.gridy = 4;
                gbc.gridwidth = 1;
                gbc.gridheight = 1;
                gbc.fill = GridBagConstraints.BOTH;
                gbc.anchor = GridBagConstraints.WEST;
                gbc.weightx = 100;
                gbc.weighty = 100;

                panelToolbar.add(panelActiveImage, gbc);
            }
        }
        else
        {
            panelToolbar.remove(panelActiveImage);
        }

        validate();
    }

    /**
     *   Builds the active image panel for choosing which
     *   image (A, B, or BOTH) to perform operations on.
     */
    protected void buildActiveImagePanel()
    {
        panelActiveImage = new JPanel();
        panelActiveImage.setLayout(new GridLayout(1, 3));
        TitledBorder borderActiveImage = new TitledBorder("Image to affect");
        borderActiveImage.setTitleColor(Color.black);
        borderActiveImage.setBorder(new EtchedBorder());
        panelActiveImage.setBorder(borderActiveImage);

        ButtonGroup group1 = new ButtonGroup();
        radioImageA = new JRadioButton("Image A      ", true);
        radioImageA.setFont(MipavUtil.font12);
        group1.add(radioImageA);
        radioImageA.addActionListener(this);
        panelActiveImage.add(radioImageA);

        radioImageB = new JRadioButton("Image B       ", false);
        radioImageB.setFont(MipavUtil.font12);
        group1.add(radioImageB);
        radioImageB.addActionListener(this);
        panelActiveImage.add(radioImageB);

        radioImageBoth = new JRadioButton("Both     ", false);
        radioImageBoth.setFont(MipavUtil.font12);
        group1.add(radioImageBoth);
        radioImageBoth.addActionListener(this);
        panelActiveImage.add(radioImageBoth);
    }

    /**
     * Helper method to build a text button for the toolbar.
     * @param text     Text for button.
     * @param toolTip  Tool tip to be associated with button.
     * @param action   Action command for button.
     * @param toolbar  Tool bar to add this button to.
     * @return  the new named text button
     */
    protected JButton buildNamedTextButton(String text, String toolTip,
                                           String action, JToolBar toolbar)
    {
        JButton button = new JButton(text);
        button.addActionListener(this);
        button.setToolTipText(toolTip);
        button.setFont(MipavUtil.font12B);
        button.setMinimumSize(new Dimension(20, 20));
        button.setPreferredSize(new Dimension(90, 20));
        button.setMargin(new Insets(2, 7, 2, 7));
        button.setActionCommand(action);
        toolbar.add(button);
        return button;
    }

    /**
     *   Helper method to build a text button for the toolbar.
     *   @param text     Text for button.
     *   @param toolTip  Tool tip to be associated with button.
     *   @param action   Action command for button.
     *   @param toolbar  Tool bar to add this button to.
     */
    protected void buildDisabledTextButton(String text, String toolTip,
                                           String action, JToolBar toolbar)
    {
        JButton button = new JButton(text);
        button.addActionListener(this);
        button.setToolTipText(toolTip);
        button.setFont(MipavUtil.font12B);
        button.setMinimumSize(new Dimension(20, 20));
        button.setPreferredSize(new Dimension(90, 20));
        button.setMargin(new Insets(2, 7, 2, 7));
        button.setActionCommand(action);
        button.setEnabled(false);
        toolbar.add(button);
    }

    /**
     *   Helper method to build an icon button for the toolbar.
     *   @param icon     Name of icon for button.
     *   @param toolTip  Tool tip to be associated with button.
     *   @param action   Action command for button.
     *   @param iconroll Name of icon for rollover.
     *   @param toolbar  Tool bar to add this button to.
     */
    protected void buildIconButton(String icon, String toolTip, String action,
                                   String iconroll, JToolBar toolbar)
    {
        JButton button = new JButton(MipavUtil.getIcon(icon));
        button.addActionListener(this);
        if (action != null && (action.equals("MagImage") || action.equals("UnMagImage")))
        {
            button.addMouseListener(this);
        }
        button.setToolTipText(toolTip);
        button.setActionCommand(action);
        button.setBorderPainted(false);
        button.setRolloverEnabled(true);
        button.setRolloverIcon(MipavUtil.getIcon(iconroll));
        button.setFocusPainted(false);
        toolbar.add(button);
    }

    /**
     *   Helper method to build a toggle button for the toolbar.
     *   @param icon     Name of icon for button.
     *   @param toolTip  Tool tip to be associated with button.
     *   @param action   Action command for button.
     *   @param iconroll Name of icon for rollover.
     *   @param group    Button group to add this toggle to.
     *   @param toolbar  Tool bar to add this button to.
     */
    protected void buildToggleButton(String icon, String toolTip, String action,
                                     String iconroll,
                                     ButtonGroup group, JToolBar toolbar)
    {
        buildToggleButton(icon, toolTip, action, iconroll, group, toolbar, false);
    }

    /**
     *   Helper method to build a toggle button for the toolbar.
     *   @param icon     Name of icon for button.
     *   @param toolTip  Tool tip to be associated with button.
     *   @param action   Action command for button.
     *   @param iconroll Name of icon for rollover.
     *   @param group    Button group to add this toggle to.
     *   @param toolbar  Tool bar to add this button to.
     *   @param selected whether or not the button is initially selected
     */
    protected void buildToggleButton(String icon, String toolTip, String action,
                                     String iconroll, ButtonGroup group, JToolBar toolbar,
                                     boolean selected)
    {
        JToggleButton button = new JToggleButton(MipavUtil.getIcon(icon));
        button.setMargin(new Insets(0, 0, 0, 0));
        button.addActionListener(this);
        button.setToolTipText(toolTip);
        button.setActionCommand(action);
        button.setBorderPainted(false);
        button.setRolloverEnabled(true);
        button.setRolloverIcon(MipavUtil.getIcon(iconroll));
        button.addItemListener(this);
        button.setFocusPainted(false);
        button.setBorder(BorderFactory.createLoweredBevelBorder());
        group.add(button);
        toolbar.add(button);
        button.setSelected(selected);
    }

    /**
     *   Makes a separator for the use in the toolbars
     *   @return Separator for the toolbar.
     */
    protected JButton makeSeparator()
    {
        JButton separator = new JButton(MipavUtil.getIcon("separator.gif"));
        separator.setBorderPainted(false);
        separator.setFocusPainted(false);
        return (separator);
    }

    /**
     *   Builds the slider labels for the time slider.
     *   @param min  Min value of slider
     *   @param max  Max value of slider.
     *   @return     Slider labels hash.
     */
    protected Hashtable buildTImageSliderLabels(int min, int max)
    {
        Hashtable tImageSliderDictionary = new Hashtable();

        Font font12 = MipavUtil.font12;
        float rangeF = (max) / 4.0f;

        JLabel label1 = new JLabel("1");
        label1.setForeground(Color.black);
        label1.setFont(font12);
        tImageSliderDictionary.put(new Integer(0), label1);

        if ( (max - min) > 3)
        {
            JLabel label2 = new JLabel(Integer.toString(Math.round(rangeF * 2)));
            label2.setForeground(Color.black);
            label2.setFont(font12);
            tImageSliderDictionary.put(new Integer(50), label2);
        }

        JLabel label5 = new JLabel(Integer.toString(max));
        label5.setForeground(Color.black);
        label5.setFont(font12);
        tImageSliderDictionary.put(new Integer(100), label5);
        return tImageSliderDictionary;
    }

    /**
     * Constructs progress bar.
     * @param imageName   The name of the image.
     * @param message     Message to be displayed in the frame
     * @param start       Start (typical = 0)
     * @param end         End   (typical = 100)
     */
    protected void buildProgressBar(String imageName, String message, int start,
                                    int end)
    {
        progressBar = new ViewJProgressBar(imageName, message, start, end, false, null, null);
    }

    /**
     *   Initializes the progress bar. Sets the location relative the middle the screen
     *   and makes it visible.
     */
    protected void initProgressBar()
    {
        if (progressBar != null)
        {
            int xScreen = Toolkit.getDefaultToolkit().getScreenSize().width;
            progressBar.setLocation(xScreen / 2, 50);
            progressBar.setVisible(true);
        }
    }

    /**
     * Sets values based on knob along time slider.
     * @param e  Event that triggered this function
     */
    public void stateChanged(ChangeEvent e)
    {
        Object source = e.getSource();

        if (source == tImageSlider)
        {
        	int newValue = 1;
        	
            if (tImageSlider.getValueIsAdjusting() == true)
            {
                return;
            }
            newValue = Math.round(tImageSlider.getValue() / 100.0f * (extents[3] - 1) - 0.01f);
            setTimeSlice(newValue);
            controls.setTimeSl(newValue);
        }
        else if (source == intensitySpinner)
        {
        	float paintIntensity = ( (SpinnerNumberModel) intensitySpinner.getModel()).getNumber().floatValue();

            for (int i = 0; i < MAX_TRI_IMAGES; i++)
            {
                if (triImage[i] != null)
                {
                    triImage[i].setIntensityDropper(paintIntensity);
                }
            }
        }
    }

	/**
	 *   Does setBorderPainted for the appropriate button.
	 *   @param event    Event that triggered this function
	 */
	public void itemStateChanged(ItemEvent event)
	{
	    Object source = event.getSource();
	    int state = event.getStateChange();
	
	    if (state == ItemEvent.SELECTED)
	    {
	        ( (AbstractButton) source).setBorderPainted(true);
	    }
	    else
	    {
	
	        ( (AbstractButton) source).setBorderPainted(false);
	    }
	    
	    if (source == addPointToggleButton || source == dropperPaintToggleButton ||
	   		source == paintCanToggleButton)
	    {
		   	// for certain operations, we don't support affecting both images
		    // so the "Both" radio button is disabled and imageA is selected by default
		    //if (((JToggleButton) source).isSelected())
	    	if (state == ItemEvent.SELECTED)
			{
				if (radioImageBoth.isSelected())
				{
					radioImageA.setSelected(true);
				}
				radioImageBoth.setEnabled(false);
			}
			else
			{
				// "add point" button de-selected, so re-enable
				// the "Both" radio button
			   	radioImageBoth.setEnabled(true);
			}
		}
	}

    /**
     *   Sets the traverse button to selected.
     */
    public void setTraverseButton()
    {
        traverseButton.setSelected(true);
    }

    /**
     *   Closes the image B and performs cleanup.
     */
    public void closeImageB()
    {
        if (imageB != null)
        {
            for (int i = 0; i < MAX_TRI_IMAGES; i++)
            {
                triImage[i].setActiveImage(ViewJComponentBase.IMAGE_A);
            }

            imageB.removeImageDisplayListener(this);
            if (imageB.getImageFrameVector().isEmpty())
            {
                imageB.disposeLocal();
            }
            imageB = null;

            triImage[AXIAL_B] = null;
            triImage[CORONAL_B] = null;
            triImage[SAGITTAL_B] = null;
            triImage[AXIAL_AB] = null;
            triImage[SAGITTAL_AB] = null;
            triImage[CORONAL_AB] = null;

            setImageSelectorPanelVisible(false);

            float optimalZoom = getOptimalZoom(256, 256);
            triImage[AXIAL_A].setZoom(optimalZoom, optimalZoom);
            triImage[SAGITTAL_A].setZoom(optimalZoom, optimalZoom);
            triImage[CORONAL_A].setZoom(optimalZoom, optimalZoom);
            
            leastSquaresButton.setEnabled(false);
            tpSplineButton.setEnabled(false);

            updateLayout();

            validate();
            componentResized(null);

            validate();

            updateImages(true);
        }
        System.gc();
    }

    /**
     * Sets the orientation variable, which determines the last component tha was interacted with
     * @param orientation int
     */
    public void setOrientation(int orientation)
    {
        this.orientation = orientation;
    }

    /**
     * Sets the color of the X slice crosshairs
     * @param c Color the color to set the X slice to
     */
    public void setXSliceHairColor(Color c)
    {
        for (int i = 0; i < MAX_TRI_IMAGES; i++)
        {
            if (triImage[i] != null)
            {
                triImage[i].setXSliceHairColor(c);
            }
        }
        updateImages(true);
    }

    /**
     * Sets the color of the Y slice crosshairs
     * @param c Color the color to set the Y slice to
     */
    public void setYSliceHairColor(Color c)
    {
        for (int i = 0; i < MAX_TRI_IMAGES; i++)
        {
            if (triImage[i] != null)
            {
                triImage[i].setYSliceHairColor(c);
            }
        }
        updateImages(true);
    }

    /**
     * Sets the color of the Z slice crosshairs
     * @param c Color the color to set the Z slice to
     */
    public void setZSliceHairColor(Color c)
    {
        for (int i = 0; i < MAX_TRI_IMAGES; i++)
        {
            if (triImage[i] != null)
            {
                triImage[i].setZSliceHairColor(c);
            }
        }
        updateImages(true);
    }

    /**
     * Gets the color of the X slice crosshair
     * @return Color the color of the X slice crosshair
     */
    public Color getXSliceHairColor()
    {
        if (triImage[AXIAL_A] != null)
        {
            return triImage[AXIAL_A].getXSliceHairColor();
        }
        else
        {
            return null;
        }
    }

    /**
     * Gets the color of the Y slice crosshair
     * @return Color the color of the Y slice crosshair
     */
    public Color getYSliceHairColor()
    {
        if (triImage[AXIAL_A] != null)
        {
            return triImage[AXIAL_A].getYSliceHairColor();
        }
        else
        {
            return null;
        }
    }

    /**
     * Gets the color of the Z slice crosshair
     * @return Color the color of the Z slice crosshair
     */
    public Color getZSliceHairColor()
    {
        if (triImage[AXIAL_A] != null)
        {
            return triImage[AXIAL_A].getZSliceHairColor();
        }
        else
        {
            return null;
        }
    }

    /**
     * Always returns true
     * @return boolean always returns true
     */
    public boolean isFocusable()
    {
        return true;
    }

    /**
     * keyPressed event method for KeyListener
     * @param e KeyEvent
     */
    public void keyPressed(KeyEvent e)
    {
        int keyCode = e.getKeyCode();
        int newPaintBrushSize = triImage[AXIAL_A].getBrushSize();

        switch (keyCode)
        {
            case KeyEvent.VK_1:
                newPaintBrushSize = ViewJComponentEditImage.thinPaint;
                break;
            case KeyEvent.VK_2:
                newPaintBrushSize = ViewJComponentEditImage.medPaint;
                break;
            case KeyEvent.VK_3:
                newPaintBrushSize = ViewJComponentEditImage.thickPaint;
                break;
            case KeyEvent.VK_4:
                newPaintBrushSize = ViewJComponentEditImage.thickestPaint;
                break;
        }

        if (newPaintBrushSize != triImage[AXIAL_A].getBrushSize()) // there has been a change in the paint brush size
        {
            for (int i = 0; i < MAX_TRI_IMAGES; i++)
            {
                if (triImage[i] != null)
                {
                    triImage[i].rememberPaintBrushSize();
                    triImage[i].setPaintBrushSize(newPaintBrushSize);
                    triImage[i].repaint();
                }
            }
        }
    }

    /**
     * keyReleased event method for KeyListener
     * @param e KeyEvent
     */
    public void keyReleased(KeyEvent e)
    {
        int keyCode = e.getKeyCode();

        switch (keyCode)
        {
            case KeyEvent.VK_PAGE_DOWN:
                if (orientation == ViewJComponentBase.AXIAL || !hasOrientation)
                {
                    if (axialComponentSlice == 0)
                    {
                        return;
                    }
                    axialComponentSlice--;
                }
                else if (orientation == ViewJComponentBase.CORONAL)
                {
                    if (coronalComponentSlice == 0)
                    {
                        return;
                    }
                    coronalComponentSlice--;
                }
                else
                { // orientation == SAGITTAL
                    if (sagittalComponentSlice == 0)
                    {
                        return;
                    }
                    sagittalComponentSlice--;
                }

                Point3D newLabel = triImage[AXIAL_A].getVolumePosition(sagittalComponentSlice, coronalComponentSlice,
                    axialComponentSlice);
                setPositionLabels(newLabel.x, newLabel.y, newLabel.z);

                fireCoordinateChange(sagittalComponentSlice, coronalComponentSlice, axialComponentSlice);

                updateImages();
                break;

            case KeyEvent.VK_PAGE_UP:
                int[] axisOrder = triImage[AXIAL_A].getAxisOrder();
                int xDim = imageA.getExtents()[axisOrder[0]];
                int yDim = imageA.getExtents()[axisOrder[1]];
                int zDim = imageA.getExtents()[axisOrder[2]];

                if (orientation == ViewJComponentBase.AXIAL || !hasOrientation)
                {
                    if (axialComponentSlice == zDim - 1)
                    {
                        return;
                    }
                    axialComponentSlice++;
                }
                else if (orientation == ViewJComponentBase.CORONAL)
                {
                    if (coronalComponentSlice == yDim - 1)
                    {
                        return;
                    }
                    coronalComponentSlice++;
                }
                else
                { // orientation == SAGITTAL
                    if (sagittalComponentSlice == xDim - 1)
                    {
                        return;
                    }
                    sagittalComponentSlice++;
                }

                newLabel = triImage[AXIAL_A].getVolumePosition(sagittalComponentSlice, coronalComponentSlice,
                    axialComponentSlice);
                setPositionLabels(newLabel.x, newLabel.y, newLabel.z);

                fireCoordinateChange(sagittalComponentSlice, coronalComponentSlice, axialComponentSlice);

                updateImages();
                break;

            case KeyEvent.VK_B:

                // swap the border painting
                Preferences.setProperty("ShowPaintBorder",
                                        String.valueOf("" + !Preferences.is(Preferences.PREF_SHOW_PAINT_BORDER)));
                triImage[AXIAL_A].getActiveImage().notifyImageDisplayListeners(null, true);
                if (triImage[AXIAL_B] != null)
                {
                    triImage[AXIAL_B].getActiveImage().notifyImageDisplayListeners(null, true);
                }
                break;

            case KeyEvent.VK_1:
            case KeyEvent.VK_2:
            case KeyEvent.VK_3:
            case KeyEvent.VK_4:
                for (int i = 0; i < MAX_TRI_IMAGES; i++)
                {
                    if (triImage[i] != null)
                    {
                        triImage[i].resetPaintBrushSize();
                        triImage[i].repaint();
                    }
                }
                break;
        }

    }

    public void keyTyped(KeyEvent e)
    {}

    /**
     *	Listener to pass to JColorChooser when user wants to change the color of the paint.
     */
    class OkColorListener
        implements ActionListener
    {

        /**
         *    Pick up the selected color and call method to change the color.
         *    @param e   Event that triggered this function.
         */
        public void actionPerformed(ActionEvent e)
        {
            if (colorChooser != null)
            {
                color = colorChooser.getColor();
            }

            colorPaintButton.setBackground(color);

            if (controls != null)
            {
                controls.getTools().setPaintColor(color);
            }

            for (int i = 0; i < MAX_TRI_IMAGES; i++)
            {
                if (triImage[i] != null)
                {
                    triImage[i].getActiveImage().notifyImageDisplayListeners(null, true);
                }
            }
        }
    }

    /**
     * Sets the color of the paint.
     * @param color Color the desired color of the paint
     */
    public void setPaintColor(Color color)
    {
        this.color = color;

        colorPaintButton.setBackground(color);

        for (int i = 0; i < MAX_TRI_IMAGES; i++)
        {
            if (triImage[i] != null)
            {
                triImage[i].getActiveImage().notifyImageDisplayListeners(null, true);
            }
        }
    }

    public void mousePressed(MouseEvent event)
    {

    }

    public void mouseClicked(MouseEvent event)
    {
        if (event.getButton() == MouseEvent.BUTTON3)
        {
            if (event.getSource() instanceof AbstractButton)
            {
                AbstractButton btnSource = (AbstractButton) event.getSource();

                if (btnSource.getActionCommand().equals("MagImage") || btnSource.getActionCommand().equals("UnMagImage"))
                {
                    handleZoomPopupMenu(btnSource, event);
                }
            }
        }
    }

    public void mouseExited(MouseEvent event)
    {

    }

    public void mouseReleased(MouseEvent event)
    {

    }

    public void mouseEntered(MouseEvent event)
    {

    }

    /**
     * Builds the pop-up menu when the user right-clicks on either magnification button
     * @param component Component
     * @param event MouseEvent
     */
    protected void handleZoomPopupMenu(Component component, MouseEvent event)
    {
        JPopupMenu popupMenu = new JPopupMenu();

        JMenuItem menuItem = new JMenuItem("Use exponential zoom increment");
        menuItem.addActionListener(this);
        menuItem.setActionCommand("Zoom exponentially");
        popupMenu.add(menuItem);

        menuItem = new JMenuItem("Use linear zoom increment");
        menuItem.addActionListener(this);
        menuItem.setActionCommand("Zoom linearly");
        popupMenu.add(menuItem);

        popupMenu.show(component, event.getX(), event.getY());
    }

    /**
     * Add someone who wants to be notified about crosshair coordinate changes.
     * @param listener  the listener
     */
    public void addCoordinateChangeListener(CoordinateChangeListener listener)
    {
        coordinateListeners.add(listener);
    }

    /**
     * Remove a coordinate change listener from this frame's list.
     * @param listener  a coordinate change listener (hopefully in the list..)
     */
    public void removeCoordinateChangeListener(CoordinateChangeListener listener)
    {
        coordinateListeners.remove(listener);
    }

    /**
     * Tell coordinate change listeners about a coordinate change
     * (after changing the coordinate to image volume space from the tri-planar space).
     * @param x  the x coordinate
     * @param y  the y coordinate
     * @param z  the z coordinate
     */
    protected void fireCoordinateChange(int x, int y, int z)
    {
        Point3D volPt = null;
        if (orientation == ViewJComponentBase.AXIAL && triImage[AXIAL_A] != null)
        {
            volPt = triImage[AXIAL_A].getVolumePosition(x, y, z);
        }
        else if (orientation == ViewJComponentBase.CORONAL && triImage[CORONAL_A] != null)
        {
            volPt = triImage[CORONAL_A].getVolumePosition(x, y, z);
        }
        else if (orientation == ViewJComponentBase.SAGITTAL && triImage[SAGITTAL_A] != null)
        {
            volPt = triImage[SAGITTAL_A].getVolumePosition(x, y, z);
        }

        if (volPt != null)
        {
            for (Enumeration e = coordinateListeners.elements(); e.hasMoreElements(); )
            {
                ( (CoordinateChangeListener) e.nextElement()).coordinateChanged(volPt.x, volPt.y, volPt.z);
            }
        }
    }

    /**
     * Returns an integer which represents the image that is selected. The possible
     * values are ViewJComponentBase.BOTH, ViewJComponentBase.IMAGE_A, ViewJComponentBase.IMAGE_B
     * @return int the image that is selected in the active image panel
     */
    public int getSelectedImage()
    {
        if (radioImageBoth.isSelected())
        {
            return ViewJComponentBase.BOTH;
        }
        if (radioImageB.isSelected())
        {
            return ViewJComponentBase.IMAGE_B;
        }

        return ViewJComponentBase.IMAGE_A;
    }
    
    /**
     * Sets the text to display in the Talairach voxel label. This text
     * has a format corresponding to the Talairach grid. Example:
     * "AcL3" - an full explanation of what the text means is beyond the 
     * scope of this comment.
     * @param newLabelText the text to display in the Talairach voxel
     * label
     */
    public void setTalairachVoxelLabelText(String newLabelText)
    {
    	talairachVoxelLabel.setText(newLabelText);
    	if (volumePositionFrame != null)
    	{
    		volumePositionFrame.setTalairachVoxelLabel(newLabelText);
    	}
    }
    
    /**
     * Gets the text stored in the Talairach voxel label
     * @return
     */
    public String getTalairachVoxelLabelText()
    {
    	return talairachVoxelLabel.getText();
    }
    
    /**
     * Returns a reference to the ViewJFrameImage object that is the parent of this frame.
     * @return a reference to the ViewJFrameImage object that is the parent of this frame
     */
    public ViewJFrameImage getParentFrame()
    {
    	return parentFrame;
    }
    
    protected void setImageActiveInTriComponents(int activeImage)
    {
		for (int i = 0; i < MAX_TRI_IMAGES; i++)
		{
			if (triImage[i] != null)
			{
				triImage[i].setActiveImage(ViewJComponentBase.IMAGE_A);
			}
		}
    	
    	if (activeImage == ViewJComponentBase.IMAGE_B)
    	{
    		if (triImage[AXIAL_AB] != null)
			{
				triImage[AXIAL_AB].setActiveImage(ViewJComponentBase.IMAGE_B);
			}
    		if (triImage[SAGITTAL_AB] != null)
			{
				triImage[SAGITTAL_AB].setActiveImage(ViewJComponentBase.IMAGE_B);
			}
    		if (triImage[CORONAL_AB] != null)
			{
				triImage[CORONAL_AB].setActiveImage(ViewJComponentBase.IMAGE_B);
			}
    		
    	}
    }
    
    private void adjustScrollbars(final int x, final int y, final JScrollPane scrollPane)
    {
    	if (scrollPane == null) return;
    	
    	JViewport viewport = scrollPane.getViewport();
    	if (viewport == null) return;
    	
    	Dimension extentSize = viewport.getExtentSize();
    	if (extentSize == null) return;
    	
        final int scrollPaneX = extentSize.width / 2;
        final int scrollPaneY = extentSize.height / 2;

        Runnable adjustScrollbarsAWTEvent = new Runnable()
        {
            public void run()
            {
                scrollPane.getHorizontalScrollBar().setValue(x - scrollPaneX);
                scrollPane.getVerticalScrollBar().setValue(y - scrollPaneY);
            }
        };
        SwingUtilities.invokeLater(adjustScrollbarsAWTEvent);
    }
}
