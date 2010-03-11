package gov.nih.mipav.view;

import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.*;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.renderer.WildMagic.Render.LocalVolumeVOI;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOIManagerInterface;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOIManagerInterfaceListener;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.lang.reflect.*;

import java.net.*;

import java.text.*;

import java.util.*;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;


/**
 * This program works with 3D and 4D images. Makes a frame and puts 3 images into it - an XY image in the upper left
 * quadrant, a ZY image in the upper right quadrant, and a XZ image in the lower left quadrant. The XY image always has
 * increasing X going from left to right and increasing Y going from top to bottom. If the image orientation is
 * FileInfoBase.UNKNOWN_ORIENT, the ZY image has Z increasing from left to right and Y increasing from top to bottom. If
 * the image orientation is known, the ZY image has Y increasing from left to right and Z increasing from bottom to top.
 * The XZ image always has X increasing from left to right. If the orientation is unknown, Z increases from top to
 * bottom. If the orientation is known, Z increases from bottom to top. If the image orientation is known XY has an
 * axial slice, ZY has a sagittal slice, and XZ has a coronal slice. The lower right quadrant displays the X, Y, and Z
 * coordinates of the point selected.
 *
 * <p>There are 2 menus - a file menu and an options menu. The file menu only has a close frame command. The Options
 * menu has a Show Axes command,a Show Crosshairs command, a show Talairach grid, a show Talairach position command, and
 * a Link to another image command.</p>
 *
 * <p>There are 2 rows of toolbar buttons. The first row has 15 toolbar buttons:<br>
 * 1.) Traverse image with pressed mouse button. As you traverse the XY image, the XY image is unchanged. The X slice
 * shown in the ZY image changes and the Y coordinate in the ZY image changes. The Z coordinate in the ZY image remains
 * unchanged. The Y slice shown in the XZ image changes and the X coordinate in the XZ image changes. The Z coordinate
 * shown in the XZ image remains unchanged. Analagous operation for traversing the ZY and XZ slices.<br>
 * 2.) Magnify image 2.0X. Note that magnification is always a power of 2.<br>
 * 3.) Magnify image 0.5X.<br>
 * 4.) Set image magnification to 1.0X.<br>
 * 5.) Draw a line VOI which can be used for measurement. 6.) Identify center of volume to determine translations.
 * Centers are moved with mouse dragging operations. Plus sign markers appear to show the new center for each image.
 * Otherwise, operation is similar to the traverse command. The centers disappear when traverse mode is entered.<br>
 * 7.) Alignment tool to determine rotations. Protractors with 2 line segments intersecting in a common point appear in
 * all 3 images. By putting a cursor at the tip of a protractor line segment and pressing the mouse button, it is
 * possible to shrink, grow, or rotate the line segment. The thicker line segment is the reference line segment which
 * will always click into a vertical or horizontal orientation(whichever is closer) when the mouse button is released.
 * The thinner line segment can be left in any orientation. The angle is measured in degrees by going clockwise from the
 * thick reference segment to the thinner segment. When the mouse button is pressed with the cursor on the protractor,
 * the angle in degrees will appear. If the cursor is placed over a part of the protractor other than a nonintersecting
 * line segment end and the mouse button is pressed, then the protractor can be moved unchanged in size and angle.<br>
 * 8.) Apply rotations and translations. An apply transformation matrix dialog appears. Under Destination radio buttons
 * for new image and replace image are present. Under interpolation a combo box with trilinear, bspline 3rd order, and
 * bspline 4rth order are present. Apply and cancel buttons are present on the bottom of the dialog. For 4D images apply
 * the same transformation to all time slices.<br>
 * 9.) Add a non center point to all images.<br>
 * 10.) New VOI.<br>
 * 11.) Delete a non center point from all images.<br>
 * 12.) Bounding cube to set boundaries for cropping.<br>
 * 13.) Crop the image using the bounding cube boundaries. For 4D images apply the same cropping to all time slices.<br>
 * 14.) (**REMOVED**)Bring up a dialog for placing markers to create a new image in the AC-PC aligned view.<br>
 * 15.) (**REMOVED**)Bring up a dialog for placing markers to create a new Talairach space image from an image in the
 * AC-PC aligned view.<br>
 * </p>
 *
 * <p>The second toolbar row is a paint toolbar with 14 buttons:<br>
 * 1.) Draw using a brush.<br>
 * 2.) Pick up a color from an image.<br>
 * 3.) Fills an area with the desired color - the Paint Grow dialog box appears. The box has an upper bound slider, a
 * lower bound slider, and text fields for entering maximum size and maximum distance. Apply and cancel buttons are
 * present.<br>
 * 4.) Erases a portion of the image.<br>
 * 5.) Erases all paint.<br>
 * 6.) Draw using small size.<br>
 * 7.) Draw using medium size.<br>
 * 8.) Draw using large size.<br>
 * 9.) Change intensity level of the paint - a desired paint intensity dialog box appears. The box tells the minimum and
 * maximum allowed intensity levels. The user inputs inputs the desired intensity level. Apply and cancel buttons are
 * present. Pressing the apply button causes the desired intensity level to be placed in intensityDropper. The initial
 * desired intensity level is 1.<br>
 * 10.) Change paint color - a pick paint color dialog box appears. If controls is not null, the original value of color
 * is given by color = controls.getTools().paintColor; Otherwise, the original value is taken as new Color(255,0,0).<br>
 * 11.) Change opacity of paint - a paint opacity dialog box appears. If controls is not null, the original value of
 * opacity is given by OPACITY = controls.getTools().opacity; Otherwise, the original value is taken as 0.25. The dialog
 * box has a slider whose value can be varied from 0 to 1.<br>
 * 12.) Commit - Changes image where painted. The painted pixels are given values equal to intensityDropper and the
 * paint is erased.<br>
 * 13.) Undo last paint region created by a mouse release.<br>
 * 14.) Calculate volume of paint.<br>
 * </p>
 *
 * <p>If imageA and imageB are both loaded, an Active Image Panel with 3 radio buttons, image A, imageB, and both
 * appears. Commit paint changes and apply rotations and translations may be selected for imageA, imageB, or both.</p>
 *
 * <p>With 4D images a slider for the fourth time dimension appears below the second toolbar.</p>
 */
public class ViewJFrameTriImage extends ViewJFrameBase
        implements ItemListener, ChangeListener, KeyListener, MouseListener, VOIManagerInterfaceListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -7588617549055746009L;

    /** Dimensions used for Talairach view clipping box - lateral. */
    public static final float ATLAS_BBOX_LAT = 80.0f;

    /** Dimensions used for Talairach view clipping box - anterior. */
    public static final float ATLAS_BBOX_ANT = 80.0f;

    /** Dimensions used for Talairach view clipping box - posterior. */
    public static final float ATLAS_BBOX_POS = 110.0f;

    /** Dimensions used for Talairach view clipping box - inferior. */
    public static final float ATLAS_BBOX_INF = 55.0f;

    /** Dimensions used for Talairach view clipping box - superior. */
    public static final float ATLAS_BBOX_SUP = 85.0f;

    /** Dimensions used for Talairach view clipping box - inferior new. 3/06/96: extra 10 mm for cerebellum . */
    public static final float ATLAS_BBOX_INF_NEW = 65.0f;

    /** Dimensions used for Talairach coordinates - lateral. */
    public static final float ATLAS_AC_TO_LAT = 68.0f;

    /** Dimensions used for Talairach coordinates - anterior. */
    public static final float ATLAS_AC_TO_ANT = 70.0f;

    /** Dimensions used for Talairach coordinates - posterior. */
    public static final float ATLAS_PC_TO_POS = 79.0f;

    /** Dimensions used for Talairach coordinates - inferior. */
    public static final float ATLAS_AC_TO_INF = 42.0f;

    /** Dimensions used for Talairach coordinates - superior. */
    public static final float ATLAS_AC_TO_SUP = 74.0f;

    /** AC to PC dimension. */
    public static final float ATLAS_AC_TO_PC = 23.0f;

    /** DOCUMENT ME! */
    public static final int AXIAL_A = 0;

    /** DOCUMENT ME! */
    public static final int SAGITTAL_A = 1;

    /** DOCUMENT ME! */
    public static final int CORONAL_A = 2;

    /** DOCUMENT ME! */
    public static final int AXIAL_AB = 3;

    /** DOCUMENT ME! */
    public static final int SAGITTAL_AB = 4;

    /** DOCUMENT ME! */
    public static final int CORONAL_AB = 5;

    /** DOCUMENT ME! */
    public static final int AXIAL_B = 6;

    /** DOCUMENT ME! */
    public static final int SAGITTAL_B = 7;

    /** DOCUMENT ME! */
    public static final int CORONAL_B = 8;

    /**
     * We want to be able to toggle the bounding box button, but don't want it to be part of any other button group. The
     * button group can't be programmatically set to "unselected" after its already been selected. To emulate this
     * behavior, I'm adding an invisible button to the button group that will be "toggled" on and off when the user
     * clicks the bounding box button. These two buttons will act as the button group.
     */
    protected static final int NUM_INVISIBLE_BUTTONS = 4;

    /** Maximum number of tri-images! */
    public static final int MAX_TRI_IMAGES = 9;

    /** A constant for which to test in the actionPerformed. */
    protected static final String OLD_LAYOUT = "OldLayout";

    /** A constant for which to test in the actionPerformed. */
    protected static final String PANEL_PLUGIN = "PanelPlugin";

    /** Constant to determine how many pixels would be optimal for the image to be initially zoomed to. */
    protected static final int DEFAULT_OPTIMAL_ZOOM = 256;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    // protected JScrollPane[] scrollPane = new JScrollPane[MAX_TRI_IMAGES];
    // made public for use by visualization plug-ins (PLB)
    public JScrollPane[] scrollPane = new JScrollPane[MAX_TRI_IMAGES];

    /** DOCUMENT ME! */
    // protected ViewJComponentTriImage[] triImage = new ViewJComponentTriImage[MAX_TRI_IMAGES];
    // made public for use by visualization plug-ins (PLB)
    public ViewJComponentTriImage[] triImage = new ViewJComponentTriImage[MAX_TRI_IMAGES];

    /** DOCUMENT ME! */
    // protected JPanel[] triImagePanel = new JPanel[MAX_TRI_IMAGES];
    // made public for use by visualization plug-ins (PLB)
    public JPanel[] triImagePanel = new JPanel[MAX_TRI_IMAGES];

    /** DOCUMENT ME! */
    protected JToggleButton[] btnInvisible = new JToggleButton[NUM_INVISIBLE_BUTTONS];

    /** 1 for black and white, 4 for color. */
    protected int bufferFactor;

    /** DOCUMENT ME! */
    protected boolean centerButtonSelected = false;

    /** "Show talairach grid" checkbox. */
    protected JCheckBox chkShowTalairachGrid;

    /** "Show talairach gridmarkers" checkbox. */
    protected JCheckBox chkShowTalairachGridMarkers;

    /** Color of the paint. */
    protected Color color = new Color(225, 0, 0);

    /** Color chooser to use when selecting paint color. */
    protected ViewJColorChooser colorChooser;

    /** Button for changing the color of the paint. */
    protected JButton colorPaintButton;

    /** Used to obtain initial paint opacity and color. */
    protected ViewControlsImage controls = null;

    /**
     * A list of CoordinateChangeListeners who want to know about changes to the coordinate currently pointed to by the
     * tri-image frame's crosshairs.
     */
    protected Vector coordinateListeners = new Vector();

    /** Spinner component for the crosshair gap size. */
    protected JSpinner crosshairSpinner;

    /** The current values of the absolute position labels. */
    protected Point3D currentAbsolutePositionLabels;

    /** The default hight of the window, if it cannot be calculated. */
    protected int defaultPreferredHeight = 1000;

    /** Extents of image. */
    protected int[] extents;

    /**
     * axialOrientation is true if imageConfiguration is originally AXIAL, CORONAL, or SAGITTAL It is reordered to an
     * axial with x going from right to left, y going from anterior to posterior, and z going from inferior to superior
     * if it is not already in this format. Note that axis reversal is not required in ANALYZE, NIFTI, and DICOM, but it
     * is required in AFNI and MINC.
     */

    /** Image control toolbar. */
    protected JToolBar imageToolBar;
    ButtonGroup VOIGroup = new ButtonGroup();
    
    protected JToolBar imageAlignToolBar;

    /** Spinner component for the paint intensity. */
    protected JSpinner intensitySpinner;

    /** Label the Talairach position x value in the image volume. */
    protected JTextField labelXTal;

    /** Label the Talairach position y value in the image volume. */
    protected JTextField labelYTal;

    /** Label the Talairach position z value in the image volume. */
    protected JTextField labelZTal;

    /** Menu items storage. */
    protected ViewMenuBuilder menuObj;

    /** DOCUMENT ME! */
    protected boolean oldLayout = Preferences.is(Preferences.PREF_TRIPLANAR_2X2_LAYOUT); // flag to indicated whether
                                                                                         // or not to use the old
                                                                                         // tri-planar layout

    /** Opacity of paint. */
    protected float OPACITY = 0.25f;

    /** List of built-in and user-defined paint brushes. */
    protected String[] paintBrushNames = null;

    /** Paint tool bar. */
    protected JToolBar paintToolBar;
    
    public JToggleButton borderPaintButton, bogusBorderPaintButton;

    /** Panel for deciding which image is active; appears when 2 images are loaded. */
    protected JPanel panelActiveImage = null;

    /** Panel that holds the toolbars. */
    protected JPanel panelToolbar = new JPanel();
    protected GridBagConstraints panelToolBarGBC = new GridBagConstraints();

    /** Reference to the parent window. */
    protected ViewJFrameImage parentFrame;

    /** Reference to the plug-in panel in the 2x2 (old) layout. */
    protected Component pluginPanel;

    /** Radio button for selecting image A as active. */
    protected JRadioButton radioImageA;

    /** Radio button for selecting image B as active. */
    protected JRadioButton radioImageB;

    /** Radio button for selecting both images as active. */
    protected JRadioButton radioImageBoth;

    /** Resolutions of image. */
    protected float[] resols = new float[3];

    /** Flag telling the crosshair movement to update slice in original image frame. */
    protected boolean scrollOriginalCrosshair = false;

    /** Flag for showing the Talairach grid on the component images. */
    protected boolean showTalairachGrid = false;

    /** Flag for showing the Talairach position on the component images. */
    protected boolean showTalairachPosition = false;

    /** Time dimension of the original image. */
    protected int tDim;

    /** Slider for 4D images, time dimension. */
    protected JSlider tImageSlider;

    /** Button for traversing the images. */
    protected JToggleButton traverseButton;

    /** Time slice that this image is on. */
    protected int tSlice;

    /** Units of image - mm, inches, etc. */
    protected int[] units = new int[3];

    /** if true use cerebellum-adjusted static variable. */
    protected boolean useInfNew;

    /** The point to become center of transformed image. */
    protected int[] volumeCenter = new int[3];

    /** Reference to the volume coordinate frame. */
    protected VolumePositionFrame volumePositionFrame;

    /** Reference to the volume coordinate panel when it is in the plug-in position. */
    protected JPanel volumePositionPanel;

    /** Magnification for image. */
    protected float zoom = 1.0f;

    /** DOCUMENT ME! */
    private JToggleButton addPointToggleButton;

    /** DOCUMENT ME! */
    private JToggleButton dropperPaintToggleButton;


    /** refs to indiviadual frame zooming in and out buttons. */
    private JToggleButton indivMagButton, indivMinButton;

    /** DOCUMENT ME! */
    private JButton leastSquaresButton;

    /** refs to the magnify and minimize button.* */
    private JButton magButton, minButton;

    /** Box holding the list of available paint brushes. */
    private JComboBox paintBox = null;

    /** used with the above to say the paint brush has been changed, waiting to change back. */
    private boolean paintBrushLocked = false;

    /** DOCUMENT ME! */
    private JToggleButton paintCanToggleButton;

    /** int used for quick-key painting for speedier paint brush access. */
    private int quickPaintBrushIndex = -1;
    
    /** The button that indicates whether this triframe is linked with tri-frames of similar dimensionality. */
    private JCheckBox scrollButton;

    /** Flag for snapping protractor to nearest multiple of 90 degrees. */
    private boolean snapProtractor90 = false;

    /** Used to setup the paint spinner. */
    private double spinnerDefaultValue = 1, spinnerMin = 0, spinnerMax = 255, spinnerStep = 1;

    /** DOCUMENT ME! */
    private JLabel talairachVoxelLabel;

    /** Toolbar builder reference. */
    private ViewToolBarBuilder toolbarBuilder;

    /** DOCUMENT ME! */
    private JButton tpSplineButton;

    /** Volume Boundary may be changed for cropping the volume. */
    private CubeBounds volumeBounds;
    
    private VOIManagerInterface voiManager;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Make a frame and puts an image component into it.
     *
     * @param  _imageA   First image to display
     * @param  LUTa      LUT of the imageA (if null grayscale LUT is constructed)
     * @param  _imageB   Second loaded image
     * @param  LUTb      LUT of the imageB
     * @param  controls  controls used to obtain initial OPACITY and color
     * @param  parent    main user interface frame.
     */
    public ViewJFrameTriImage(ModelImage _imageA, ModelLUT LUTa, ModelImage _imageB, ModelLUT LUTb,
                              ViewControlsImage controls, ViewJFrameImage parent) {
        super(_imageA, _imageB);
        userInterface = ViewUserInterface.getReference();
        this.LUTa = LUTa;
        this.LUTb = LUTb;
        this.controls = controls;
        parentFrame = parent;

        try {
            scrollOriginalCrosshair = Preferences.is(Preferences.PREF_TRIPLANAR_SCROLL_ORIGINAL);
        } catch (Exception e) {
            scrollOriginalCrosshair = false;
        }

        try {
            setIconImage(MipavUtil.getIconImage("3plane_16x16.gif"));
        } catch (Exception e) { }

        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // **************************** Action Events *****************************
    // ************************************************************************

    /**
     * Calls various methods depending on the action.
     *
     * @param  event  event that triggered function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        Object source = event.getSource();

        Preferences.debug(command, Preferences.DEBUG_MINOR);

        if (command.equals("ScrollLink")) {
            linkedScrolling = !linkedScrolling;    
                
            Enumeration names = userInterface.getRegisteredImageNames();

            boolean sameDims = false;
            while (names.hasMoreElements()) {
                String name = (String) names.nextElement();
                sameDims = true;

                if (!imageA.getImageName().equals(name)) {
                    ModelImage img = userInterface.getRegisteredImageByName(name);

                    if (img.getTriImageFrame() != null) {

                        if (imageA.getNDims() == img.getNDims()) {

                            for (int z = 0; z < imageA.getNDims(); z++) {

                                if (imageA.getExtents()[z] != img.getExtents()[z]) {
                                    sameDims = false;
                                }
                            }

                            if (sameDims == true) {
                                img.getTriImageFrame().setLinkButtonSelected(linkedScrolling);
                            }
                        }
                    }
                }
            }
            
        } else if (command.equals("CloseFrame")) {
            windowClosing(null);
        } else if (command.equals("leastSquares")) {
            handleLeastSquares();

            return;
        } else if (command.equals("tpSpline")) {
            handleTPSpline();

            return;
        } else if (command.equals(OLD_LAYOUT)) {
            setOldLayout(!oldLayout);
        } else if (command.equals(PANEL_PLUGIN)) {
            handlePluginPanelSelection();

            return;
        } else if (command.equals("ShowAxes")) {

            for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                if (triImage[i] != null) {
                    triImage[i].displayAxes(menuObj.isMenuItemSelected("Show axes"));
                }
            }

            updateImages(true);
        } else if (command.equals("ShowXHairs")) {

            for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                if (triImage[i] != null) {
                    triImage[i].displayXHairs(menuObj.isMenuItemSelected("Show crosshairs"));
                }
            }

            updateImages(true);
        } else if (command.equals("ScrollOriginal")) {

            scrollOriginalCrosshair = menuObj.isMenuItemSelected("Scroll original image with crosshair");
            Preferences.setProperty(Preferences.PREF_TRIPLANAR_SCROLL_ORIGINAL,
                                    Boolean.toString(scrollOriginalCrosshair));
            updateImages(true);
        } else if (command.equals("FastPaint")) {
            Preferences.setProperty(Preferences.PREF_FAST_TRIPLANAR_REPAINT,
                                    (menuObj.isMenuItemSelected("Fast rendering in paint mode") ? "true" : "false"));
        } else if (command.equals("Snap90")) {
            Preferences.setProperty(Preferences.PREF_TRIPLANAR_SNAP90,
                                    (menuObj.isMenuItemSelected("Snap protractor to 90 degrees multiple") ? "true"
                                                                                                          : "false"));

            snapProtractor90 = menuObj.isMenuItemSelected("Snap protractor to 90 degrees multiple");

            for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                if (triImage[i] != null) {
                    triImage[i].setSnapProtractor90(snapProtractor90);
                }
            }
        } else if (command.equals("ShowTalairachGrid")) {

            // according to current logic, there is a possibility that the
            // volume position frame and the tabbed volume position pane in the plug-in
            // position could both be active at once, hence the need for this
            // sourceCheckbox logic
            JCheckBox sourceCheckbox = (JCheckBox) (event.getSource());

            showTalairachGrid = sourceCheckbox.isSelected();
            chkShowTalairachGrid.setSelected(showTalairachGrid);
            chkShowTalairachGridMarkers.setEnabled(showTalairachGrid);

            menuObj.setMenuItemSelected("ShowXHairs", !showTalairachGrid);
            menuObj.setMenuItemSelected("ShowAxes", !showTalairachGrid);

            for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                if (triImage[i] != null) {
                    triImage[i].showTalairachGrid(showTalairachGrid);
                    triImage[i].displayAxes(!showTalairachGrid);
                    triImage[i].displayXHairs(!showTalairachGrid);
                }
            }

            updateImages(true);
        } else if (command.equals("ShowTalairachGridmarkers")) {

            // according to current logic, there is a possibility that the
            // volume position frame and the tabbed volume position pane in the plug-in
            // position could both be active at once, hence the need for this
            // sourceCheckbox logic
            JCheckBox sourceCheckbox = (JCheckBox) (event.getSource());

            boolean showGridMarkers = sourceCheckbox.isSelected();
            chkShowTalairachGridMarkers.setSelected(showGridMarkers);

            for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                if (triImage[i] != null) {
                    triImage[i].showTalairachGridmarkers(showGridMarkers);
                }
            }

            updateImages(true);
        } else if (command.equals("PaintBrush")) {
            if ((imageB != null) && (!radioImageBoth.isEnabled())) {
                radioImageBoth.setEnabled(true);
            }

            for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                if (triImage[i] != null) {
                    triImage[i].setCursorMode(ViewJComponentBase.PAINT_VOI);
                    triImage[i].setProtractorVisible(false);
                    triImage[i].setIntensityLineVisible(false);
                    triImage[i].repaint();
                }
            }

        } else if (command.equals("Dropper")) {

            for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                if (triImage[i] != null) {
                    triImage[i].setCursorMode(ViewJComponentBase.DROPPER_PAINT);
                    triImage[i].setProtractorVisible(false);
                    triImage[i].setIntensityLineVisible(false);
                    triImage[i].repaint();
                }
            }
        } else if (command.equals("Eraser")) {

            if ((imageB != null) && (!radioImageBoth.isEnabled())) {
                radioImageBoth.setEnabled(true);
            }

            for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                if (triImage[i] != null) {
                    triImage[i].setCursorMode(ViewJComponentBase.ERASER_PAINT);
                    triImage[i].setProtractorVisible(false);
                    triImage[i].setIntensityLineVisible(false);
                    triImage[i].repaint();
                }
            }
        } else if (command.equals("EraseAll")) {

            for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                if (triImage[i] != null) {
                    triImage[i].setProtractorVisible(false);
                    triImage[i].setIntensityLineVisible(false);
                    triImage[i].repaint();
                }
            }

            triImage[AXIAL_A].eraseAllPaint(false);

            imageA.notifyImageDisplayListeners(null, true);

            if (imageB != null) {
                imageB.notifyImageDisplayListeners(null, true);
            }
        } else if (command.equals("PaintMask")) {

            if (triImage[AXIAL_AB] != null) {
                triImage[AXIAL_AB].getActiveImage().setMask(triImage[AXIAL_AB].getActiveImage().generateVOIMask());
                triImage[AXIAL_AB].setPaintMask(triImage[AXIAL_AB].getActiveImage().getMask());
            }

            updateImages(true);
        } else if (command.equals("traverse")) {

            if ((imageB != null) && (!radioImageBoth.isEnabled())) {
                radioImageBoth.setEnabled(true);
            }

            for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                if (triImage[i] != null) {
                    triImage[i].setCursorMode(ViewJComponentBase.DEFAULT);
                    triImage[i].setProtractorVisible(false);
                    triImage[i].setIntensityLineVisible(false);

                }
            }

            updateImages(true);
        } else if (command.equals("Center")) {
            centerButtonSelected = !centerButtonSelected;

            btnInvisible[3].setSelected(!centerButtonSelected);

            for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                if (triImage[i] != null) {
                    triImage[i].setCursorMode(ViewJComponentBase.DEFAULT);
                    triImage[i].setProtractorVisible(false);
                    triImage[i].setIntensityLineVisible(false);
                    triImage[i].repaint();
                }
            }

            if (centerButtonSelected) {
                setCenter((extents[0] - 1) / 2, (extents[1] - 1) / 2, (extents[2] - 1) / 2);

            } else {

                for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                    if (triImage[i] != null) {
                        triImage[i].setDoCenter(false);
                    }
                }
            }

            updateImages(true);
        } else if (command.equals(CustomUIBuilder.PARAM_VOI_PROTRACTOR.getActionCommand())) {

            snapProtractor90 = menuObj.isMenuItemSelected("Snap protractor to 90 degrees multiple");

            if (triImage[AXIAL_A].isProtractorVisible()) {
                btnInvisible[2].setSelected(true);
            }

            if (((JToggleButton) source).isSelected()) {

                if ((imageB != null) && (!radioImageBoth.isEnabled())) {
                    radioImageBoth.setEnabled(true);
                }

                for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                    if (triImage[i] != null) {
                        triImage[i].setCursorMode(ViewJComponentBase.PROTRACTOR);
                        triImage[i].setSnapProtractor90(snapProtractor90);
                        triImage[i].makeProtractor();
                        triImage[i].setProtractorVisible(true);
                        triImage[i].setIntensityLineVisible(false);
                        triImage[i].repaint();
                    }
                }
            } else {

                for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                    if (triImage[i] != null) {
                        triImage[i].setCursorMode(ViewJComponentBase.DEFAULT);
                        triImage[i].setProtractorVisible(false);
                        triImage[i].repaint();
                    }
                }
            }
        } else if (command.equals(CustomUIBuilder.PARAM_VOI_LINE.getActionCommand())) {

            if (triImage[AXIAL_A].isIntensityLineVisible()) {
                btnInvisible[2].setSelected(true);
            }


            if (((JToggleButton) source).isSelected()) {

                if ((imageB != null) && (!radioImageBoth.isEnabled())) {
                    radioImageBoth.setEnabled(true);
                }

                for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                    if (triImage[i] != null) {
                        triImage[i].setCursorMode(ViewJComponentBase.DEFAULT);
                        triImage[i].setIntensityLineVisible(true);
                        triImage[i].setProtractorVisible(false);
                        triImage[i].repaint();
                    }
                }
            } else {

                for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                    if (triImage[i] != null) {
                        triImage[i].setCursorMode(ViewJComponentBase.DEFAULT);
                        triImage[i].setIntensityLineVisible(false);

                        // triImage[i].setProtractorVisible(true);
                        triImage[i].repaint();
                    }
                }
            }
        } else if (command.equals("colorPaint")) {
            colorChooser = new ViewJColorChooser(this, "Pick paint color", new OkColorListener(), null);
        } else if (command.equals("OpacityPaint")) {

            if (controls != null) {
                new JDialogOpacityControls(this, controls);
            } else {
                new JDialogOpacityControls(this, OPACITY);
            }

            triImage[AXIAL_A].getActiveImage().notifyImageDisplayListeners(null, true);
        } else if (command.equals("DisplayBorder")) {
        	if(Preferences.is(Preferences.PREF_SHOW_PAINT_BORDER)) {
            	getControls().getTools().bogusBorderPaintButton.setSelected(true);
            	bogusBorderPaintButton.setSelected(true);
            }else {
            	getControls().getTools().borderPaintButton.setSelected(true);
            	borderPaintButton.setSelected(true);
            }

        	Preferences.setProperty(Preferences.PREF_SHOW_PAINT_BORDER, String.valueOf("" + !Preferences.is(Preferences.PREF_SHOW_PAINT_BORDER)));

            updateImages(true);
            triImage[AXIAL_A].getActiveImage().notifyImageDisplayListeners();

        } else if (command.equals("CommitPaint")) {
        	//THIS MAY NOT BE CORRECT
        	 boolean saveMasksAs4D = false;
             if(imageA.getNDims() == 4) {
             	JDialogMask3D4D dialogMask3D4D = new JDialogMask3D4D(this);
             	if (dialogMask3D4D.isCancelled()) {
                     return;
                 } else {
                 	saveMasksAs4D = dialogMask3D4D.isSaveMasksAs4D();
                 }
             }
        	

            if (getSelectedImage() == ViewJComponentBase.BOTH) {

                if (triImage[AXIAL_AB] != null) {
                    triImage[AXIAL_AB].commitMask(ViewJComponentBase.BOTH, true, true, saveMasksAs4D);
                    triImage[AXIAL_AB].getActiveImage().notifyImageDisplayListeners(null, true);
                } else {
                    // triImage[AXIAL_A].commitMask(ViewJComponentBase.BOTH, true, true);
                }
            } else if (getSelectedImage() == ViewJComponentBase.IMAGE_A) {

                if (triImage[AXIAL_A] != null) {
                    triImage[AXIAL_A].commitMask(ViewJComponentBase.IMAGE_A, true, true, saveMasksAs4D);
                    triImage[AXIAL_A].getActiveImage().notifyImageDisplayListeners(null, true);
                }
            } else if (getSelectedImage() == ViewJComponentBase.IMAGE_B) {

                // must set IMAGE_A because in the AXIAL_B, SAGITTAL_B, and CORONAL_B images,
                // imageB is in the imageA slot
                if (triImage[AXIAL_B] != null) {
                    triImage[AXIAL_B].commitMask(ViewJComponentBase.IMAGE_A, true, true, saveMasksAs4D);
                    triImage[AXIAL_B].getActiveImage().notifyImageDisplayListeners(null, true);
                }
            }

            updateImages(true);
        } else if (command.equals("CommitPaintExt")) {
        	//THIS MAY NOT BE CORRECT
       	 	boolean saveMasksAs4D = false;
            if(imageA.getNDims() == 4) {
            	JDialogMask3D4D dialogMask3D4D = new JDialogMask3D4D(this);
            	if (dialogMask3D4D.isCancelled()) {
                    return;
                } else {
                	saveMasksAs4D = dialogMask3D4D.isSaveMasksAs4D();
                }
            }
            
            
            if (getSelectedImage() == ViewJComponentBase.BOTH) {

                if (triImage[AXIAL_AB] != null) {
                    triImage[AXIAL_AB].commitMask(ViewJComponentBase.BOTH, true, false, saveMasksAs4D);
                    triImage[AXIAL_AB].getActiveImage().notifyImageDisplayListeners(null, true);
                } else {
                    // triImage[AXIAL_A].commitMask(ViewJComponentBase.BOTH, true, false);
                }

            } else if (getSelectedImage() == ViewJComponentBase.IMAGE_A) {

                if (triImage[AXIAL_A] != null) {
                    triImage[AXIAL_A].commitMask(ViewJComponentBase.IMAGE_A, true, false, saveMasksAs4D);
                    triImage[AXIAL_A].getActiveImage().notifyImageDisplayListeners(null, true);
                }
                // triImage[AXIAL_A].getImageA().notifyImageDisplayListeners(null, true);
            } else if (getSelectedImage() == ViewJComponentBase.IMAGE_B) {

                // must set IMAGE_A because in the AXIAL_B, SAGITTAL_B, and CORONAL_B images,
                // imageB is in the imageA slot
                if (triImage[AXIAL_B] != null) {
                    triImage[AXIAL_B].commitMask(ViewJComponentBase.IMAGE_A, true, false, saveMasksAs4D);
                    triImage[AXIAL_B].getImageA().notifyImageDisplayListeners(null, true);
                }
            }
            updateImages(true);
        } else if (command.equals("PaintCan")) {

            for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                if (triImage[i] != null) {
                    triImage[i].setCursorMode(ViewJComponentBase.PAINT_CAN);
                    triImage[i].setProtractorVisible(false);
                    triImage[i].setIntensityLineVisible(false);
                    triImage[i].repaint();
                }
            }

            if ((triImage[AXIAL_A].growDialog != null) &&
                    ((JDialogPaintGrow) triImage[AXIAL_A].growDialog).isShowing()) {
                return;
            }

            Vector listeners = new Vector();

            for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                if (triImage[i] != null) {
                    listeners.add(triImage[i]);
                }
            }

            JDialogPaintGrow jdpg = new JDialogPaintGrow(this, listeners);

            for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                if (triImage[i] != null) {
                    triImage[i].growDialog = jdpg;
                }
            }
        } else if (command.equals("UndoPaint")) {

            for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                if (triImage[i] != null) {
                    triImage[i].undoLastPaint();
                }
            }

            if (triImage[AXIAL_A] != null) {
                triImage[AXIAL_A].getImageA().notifyImageDisplayListeners(null, true);
            }

            if (triImage[AXIAL_B] != null) {
                triImage[AXIAL_B].getImageB().notifyImageDisplayListeners(null, true);
            }
        } else if (command.equals("CalcPaint")) {

            if (triImage[AXIAL_A] != null) {
                triImage[AXIAL_A].calcPaintedVolume(null);
            }
        } else if (command.equals("UnMagImage")) {
            float oldZoom = triImage[AXIAL_A].getZoomX();

            float newZoom = 1;

            if ((Preferences.is(Preferences.PREF_ZOOM_LINEAR)) && (triImage[AXIAL_A].getZoomX() > 1.0f)) {

                // linear zoom is prevented if getZoomX() <= 1.0
                newZoom = triImage[AXIAL_A].getZoomX() - 1.0f;
            } else {
                newZoom = 0.5f * triImage[AXIAL_A].getZoomX();
            }

            for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                if (triImage[i] != null) {
                    triImage[i].setZoom(newZoom, newZoom);

                    Vector2f oldCrosshairPoint = triImage[i].getCrosshairPoint();

                    if (oldCrosshairPoint != null) {
                        int newX = MipavMath.round((oldCrosshairPoint.X * newZoom) / oldZoom);
                        int newY = MipavMath.round((oldCrosshairPoint.Y * newZoom) / oldZoom);

                        triImage[i].updateCrosshairPosition(newX, newY);

                        adjustScrollbars(newX, newY, scrollPane[i]);
                    }
                }
            }

            validate();
            updateImages(true);
            setTitle();
        } else if (command.equals("MagImage")) {
            float oldZoom = triImage[AXIAL_A].getZoomX();

            float newZoom = 1;
            
            if ((Preferences.is(Preferences.PREF_ZOOM_LINEAR)) && (triImage[AXIAL_A] != null)) {
            	if(triImage[AXIAL_A].getZoomX() < 1.0f) {
            		newZoom = 2.0f * triImage[AXIAL_A].getZoomX();
            	}else {
            		newZoom = triImage[AXIAL_A].getZoomX() + 1.0f;
            	}
            } else if (triImage[AXIAL_A] != null) // zoomMode == ViewJComponentEditImage.EXPONENTIAL
            {

            	newZoom = 2.0f * triImage[AXIAL_A].getZoomX();

            }

            for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                if (triImage[i] != null) {
                    triImage[i].setZoom(newZoom, newZoom);

                    Vector2f oldCrosshairPoint = triImage[i].getCrosshairPoint();

                    if (oldCrosshairPoint != null) {
                        int newX = MipavMath.round((oldCrosshairPoint.X * newZoom) / oldZoom);
                        int newY = MipavMath.round((oldCrosshairPoint.Y * newZoom) / oldZoom);

                        triImage[i].updateCrosshairPosition(newX, newY);

                        adjustScrollbars(newX, newY, scrollPane[i]);
                    }
                }
            }

            validate();
            updateImages(true);
            setTitle();
        } else if (command.equals("ZoomOne")) {

            for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                if (triImage[i] != null) {
                    float oldZoom = triImage[i].getZoomX();

                    triImage[i].setZoom(1, 1);

                    Vector2f oldCrosshairPoint = triImage[i].getCrosshairPoint();

                    if (oldCrosshairPoint != null) {
                        int newX = (int) (oldCrosshairPoint.X / oldZoom);
                        int newY = (int) (oldCrosshairPoint.Y / oldZoom);

                        triImage[i].updateCrosshairPosition(newX, newY);

                        adjustScrollbars(newX, newY, scrollPane[i]);
                    }
                }
            }

            magButton.setEnabled(true);
            minButton.setEnabled(true);
            validate();
            updateImages(true);
            setTitle();
        } else if (command.equals("IndivMagImage")) {

            for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                if (triImage[i] != null) {

                    triImage[i].setCursorMode(ViewJComponentBase.ZOOMING_IN);
                    triImage[i].setCursor(MipavUtil.magnifyCursor);

                }

            }

        } else if (command.equals("IndivMinImage")) {

            for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                if (triImage[i] != null) {
                    triImage[i].setCursorMode(ViewJComponentBase.ZOOMING_OUT);
                    triImage[i].setCursor(MipavUtil.unmagnifyCursor);
                }
            }
        } else if (command.equals("createTransformation")) {
            JDialogTriImageTransformation dialog;
            float originalZoom = triImage[AXIAL_A].getZoomX();

            if (getSelectedImage() == ViewJComponentBase.IMAGE_A) {
                dialog = new JDialogTriImageTransformation(this, imageA);
            } else if (getSelectedImage() == ViewJComponentBase.IMAGE_B) {
                dialog = new JDialogTriImageTransformation(this, imageB);
            } else if (getSelectedImage() == ViewJComponentBase.BOTH) {
                dialog = new JDialogTriImageTransformation(this, imageA, imageB);
            }
        } else if (command.equals("addPoint")) {

            for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                if (triImage[i] != null) {
                    triImage[i].setCursorMode(ViewJComponentBase.POINT_VOI);
                    triImage[i].setProtractorVisible(false);
                    triImage[i].setIntensityLineVisible(false);
                    triImage[i].repaint();
                }
            }
        } else if (command.equals("NewVOI")) {

            for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                if (triImage[i] != null) {
                    triImage[i].setCursorMode(ViewJComponentBase.NEW_VOI);
                }
            }
        } else if (command.equals("deleteVOI")) {

            for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                if (triImage[i] != null) {
                    triImage[i].deleteSelectedContours();
                }
            }

            imageA.notifyImageDisplayListeners();

            if (imageB != null) {
                imageB.notifyImageDisplayListeners();
            }
        } else if (command.equals("boundingBox")) {

            // this block of code is the logic for emulating the behavior
            // of the button group containing the invisible button
            boolean showBoundingRect = !triImage[AXIAL_A].isShowBoundingRect();

            btnInvisible[1].setSelected(!showBoundingRect);

            for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                if (triImage[i] != null) {
                    triImage[i].setShowBoundingRect(showBoundingRect);

                    if (showBoundingRect) {
                        triImage[i].setCursorMode(ViewJComponentBase.CUBE_BOUNDS);
                    } else {

                        // this means the toggle button was un-pressed, so reset to DEFAULT mode
                        triImage[i].setCursorMode(ViewJComponentBase.DEFAULT);
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
        } else if (command.equals("cropVolume")) {
            JDialogCrop dialogCrop;

            if (radioImageB.isSelected()) {

                if (imageB != null) {
                    dialogCrop = new JDialogCrop(imageB);
                } else {
                    dialogCrop = new JDialogCrop(imageA);
                }
            } else {
                dialogCrop = new JDialogCrop(imageA);
            }

            dialogCrop.setSeparateThread(true);

            int[] xBounds = { volumeBounds.lowX(), volumeBounds.highX() };
            int[] yBounds = { volumeBounds.lowY(), volumeBounds.highY() };
            int[] zBounds = { volumeBounds.lowZ(), volumeBounds.highZ() };

            dialogCrop.setXBounds(xBounds);
            dialogCrop.setYBounds(yBounds);
            dialogCrop.setZBounds(zBounds);

            dialogCrop.callAlgorithm();

            if (radioImageBoth.isSelected()) {
                dialogCrop = new JDialogCrop(imageB);

                dialogCrop.setSeparateThread(true);

                dialogCrop.setXBounds(xBounds);
                dialogCrop.setYBounds(yBounds);
                dialogCrop.setZBounds(zBounds);

                dialogCrop.callAlgorithm();
            }
        } else if (command.equals("LinkFrame")) {

            if (isMultipleSameSizeTriImages() == true) {
                new JDialogTriFrameLinker(this, triImage[AXIAL_A].getActiveImage()); // todo:  fix
            } else {
                MipavUtil.displayError("There is no image with the same dimensions as\n" +
                                       triImage[AXIAL_A].getActiveImage().getImageName() + " to operate on."); // todo:  fix

                return;
            }
        } else if ((source == radioImageA) || (source == radioImageB) || (source == radioImageBoth)) {

            if (radioImageA.isSelected()) {
                setSpinnerValues(imageA.getType());
                parentFrame.setActiveImage(ViewJComponentBase.IMAGE_A);
                setImageActiveInTriComponents(ViewJComponentBase.IMAGE_A);
            } else if (radioImageB.isSelected()) {
                setSpinnerValues(imageB.getType());
                parentFrame.setActiveImage(ViewJComponentBase.IMAGE_B);
                setImageActiveInTriComponents(ViewJComponentBase.IMAGE_B);
            }

            ((SpinnerNumberModel) (intensitySpinner.getModel())).setMinimum(new Double(spinnerMin));
            ((SpinnerNumberModel) (intensitySpinner.getModel())).setMaximum(new Double(spinnerMax));
            ((SpinnerNumberModel) (intensitySpinner.getModel())).setStepSize(new Double(spinnerStep));
            ((SpinnerNumberModel) (intensitySpinner.getModel())).setValue(new Double(spinnerDefaultValue));

            /** updateImages(false) was commented to facilitate placement of VOI points during
             * image registration. This allows the user to control the crosshairs and image slice independently of other
             * images.
             */
            // updateImages();
            return;
        } else if (command.equals("PositionFrame")) {

            if (volumePositionPanel == null) {
                buildVolumePositionPanel();
            }

            volumePositionFrame = null;
            volumePositionFrame = new VolumePositionFrame(this, tabbedPane);
            setOldLayout(false);
            volumePositionFrame.setVisible(true);
        } else if (command.equals("RadiologicalView")) {
            imageA.setRadiologicalView(true);

            if (imageB != null) {
                imageB.setRadiologicalView(true);
            }

            Vector3f kCenter = triImage[0].getCenter();
            setPositionLabels((int) kCenter.X, (int) kCenter.Y, (int) kCenter.Z);
            updateImages(true);
        } else if (command.equals("NeurologicalView")) {
            imageA.setRadiologicalView(false);

            if (imageB != null) {
                imageB.setRadiologicalView(false);
            }

            Vector3f kCenter = triImage[0].getCenter();
            setPositionLabels((int) kCenter.X, (int) kCenter.Y, (int) kCenter.Z);
            updateImages(true);
        } 

        else if (command.equals("VOIToolbar")) 
        {
            if ( voiManager == null )
            {
                initVOI();
            }
            voiManager.getToolBar().setVisible(menuObj.isMenuItemSelected("VOI toolbar"));
            updateLayout();
        }
        else if (command.equals("PaintToolbar") ) {
            paintToolBar.setVisible(menuObj.isMenuItemSelected("Paint toolbar"));
        }
        else if (command.equals("ImageAlignToolbar") ) {
            imageAlignToolBar.setVisible(menuObj.isMenuItemSelected("Image Align toolbar"));
        }
        else {
			getParentFrame().actionPerformed(event);	
		}

        this.requestFocusInWindow();
    }

    /**
     * Add someone who wants to be notified about crosshair coordinate changes.
     *
     * @param  listener  the listener
     */
    public void addCoordinateChangeListener(CoordinateChangeListener listener) {
        coordinateListeners.add(listener);
    }

    /**
     * Called when the volumePositionFrame closes, the volumePositionPanel is again displayed in the window:.
     */
    public void addTabbedPane() {
        cleanVolumePositionPanel();
        volumePositionPanel = null;
        setOldLayout(true);
    }

    /**
     * Closes the image B and performs cleanup.
     */
    public void closeImageB() {

        if (imageB != null) {

            for (int i = 0; i < MAX_TRI_IMAGES; i++) {
                triImage[i].setActiveImage(ViewJComponentBase.IMAGE_A);
            }

            imageB.removeImageDisplayListener(this);

            if (imageB.getImageFrameVector().isEmpty()) {
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
     * Should be called when window is closing to perform cleanup.
     */
    public void disposeLocal() {

        if (volumePositionFrame != null) {
            volumePositionFrame.dispose();
            volumePositionFrame = null;
        }

        for (int i = 0; i < MAX_TRI_IMAGES; i++) {

            if (triImage[i] != null) {
                triImage[i].disposeLocal(true);
                triImage[i] = null;
            }
        }

        colorChooser = null;
        controls = null;

        parentFrame.triPlanarClosing();
        
        if ( voiManager != null )
        {
            voiManager.dispose();
            voiManager = null;
        }
    }


    /**
     * Gets the axial position of the slice.
     *
     * @return  The axial location in the slice.
     */
    public int getAxialComponentSlice() {
        return triImage[AXIAL_A].getSlice();
    }

    /**
     * Returns the crop bounding volume as a CubeBounds object.
     *
     * @return  volumeBounds the crop volume
     */
    public CubeBounds getBoundedVolume() {
        return volumeBounds;
    }


    /**
     * Sets the x coordinate of the point to be the center of the transformed image.
     *
     * @return  DOCUMENT ME!
     */
    public int[] getCenter() {
        return volumeCenter;
    }

    /**
     * Gets reference to control widgets for frame.
     *
     * @return  controls
     */
    public ViewControlsImage getControls() {
        return controls;
    }

    /**
     * Gets the y position of the slice.
     *
     * @return  The y location in the slice.
     */
    public int getCoronalComponentSlice() {
        return triImage[CORONAL_A].getSlice();
    }

    /**
     * Get the last point that the position labels got set to.
     *
     * @return  the current values for the absolute position labels
     */
    public Point3D getCurrentPositionLabels() {
        return currentAbsolutePositionLabels;
    }

    /**
     * Accessor that returns the reference to imageA.
     *
     * @return  image
     */
    public ModelImage getImageA() {

        if (triImage[AXIAL_A] != null) {
            return triImage[AXIAL_A].getImageA();
        } else {
            return null;
        }
    }

    /**
     * Accessor that returns the reference to imageB.
     *
     * @return  imageB
     */
    public ModelImage getImageB() {

        if (triImage[AXIAL_B] != null) {
            return triImage[AXIAL_B].getImageB();
        } else {
            return null;
        }
    }

    /**
     * Gets the linked ViewJFrameTriImage.
     *
     * @return  linkedFrame
     */
    public ViewJFrameTriImage getLinkedTriFrame() {
        return linkTriFrame;
    }

    /**
     * Returns a reference to the ViewJFrameImage object that is the parent of this frame.
     *
     * @return  a reference to the ViewJFrameImage object that is the parent of this frame
     */
    public ViewJFrameImage getParentFrame() {
        return parentFrame;
    }

    /**
     * Gets the x position of the slice.
     *
     * @return  The x location in the slice.
     */
    public int getSagittalComponentSlice() {
        return triImage[SAGITTAL_A].getSlice();
    }

    /**
     * Returns an integer which represents the image that is selected. The possible values are ViewJComponentBase.BOTH,
     * ViewJComponentBase.IMAGE_A, ViewJComponentBase.IMAGE_B
     *
     * @return  int the image that is selected in the active image panel
     */
    public int getSelectedImage() {

        if (radioImageBoth.isSelected()) {
            return ViewJComponentBase.BOTH;
        }

        if (radioImageB.isSelected()) {
            return ViewJComponentBase.IMAGE_B;
        }

        return ViewJComponentBase.IMAGE_A;
    }

    /**
     * Gets the text stored in the Talairach voxel label.
     *
     * @return  DOCUMENT ME!
     */
    public String getTalairachVoxelLabelText() {
        return talairachVoxelLabel.getText();
    }

    /**
     * Returns a reference to one of the component tri-image components.
     *
     * @param   index  int the index of the component tri-image to get. Possibilies are AXIAL_A, AXIAL_B, AXIAL_AB,
     *                 CORONAL_A, CORONAL_B, CORONAL_AB, SAGITTAL_A, SAGITTAL_B, SAGITTAL_AB
     *
     * @return  ViewJComponentTriImage
     */
    public ViewJComponentTriImage getTriImage(int index) {
        return triImage[index];
    }

    /**
     * Gets the color of the X slice crosshair.
     *
     * @return  Color the color of the X slice crosshair
     */
    public Color getXSliceHairColor() {

        if (triImage[AXIAL_A] != null) {
            return triImage[AXIAL_A].getXSliceHairColor();
        } else {
            return null;
        }
    }

    /**
     * Gets the color of the Y slice crosshair.
     *
     * @return  Color the color of the Y slice crosshair
     */
    public Color getYSliceHairColor() {

        if (triImage[AXIAL_A] != null) {
            return triImage[AXIAL_A].getYSliceHairColor();
        } else {
            return null;
        }
    }

    /**
     * Gets the color of the Z slice crosshair.
     *
     * @return  Color the color of the Z slice crosshair
     */
    public Color getZSliceHairColor() {

        if (triImage[AXIAL_A] != null) {
            return triImage[AXIAL_A].getZSliceHairColor();
        } else {
            return null;
        }
    }

    /**
     * Always returns true.
     *
     * @return  boolean always returns true
     */
    public boolean isFocusable() {
        return true;
    }

    /**
     * Does setBorderPainted for the appropriate button.
     *
     * @param  event  Event that triggered this function
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();

        if (source.equals(paintBox)) {
            int index = paintBox.getSelectedIndex();
            
            for (int i = 0; i < MAX_TRI_IMAGES; i++) {
            	if(triImage[i] != null) {
            		triImage[i].loadPaintBrush(paintBrushNames[index], false);
            	}
            }
            
            Preferences.setProperty(Preferences.PREF_LAST_PAINT_BRUSH, paintBrushNames[index]);


        } else {

            int state = event.getStateChange();

            if (state == ItemEvent.SELECTED) {
                ((AbstractButton) source).setBorderPainted(true);
            } else {

                ((AbstractButton) source).setBorderPainted(false);
            }

            if ((source == addPointToggleButton) || (source == dropperPaintToggleButton) ||
                    (source == paintCanToggleButton)) {

                // for certain operations, we don't support affecting both images
                // so the "Both" radio button is disabled and imageA is selected by default
                // if (((JToggleButton) source).isSelected())
                if (state == ItemEvent.SELECTED) {

                    if (radioImageBoth.isSelected()) {
                        radioImageA.setSelected(true);
                    }

                    radioImageBoth.setEnabled(false);
                } else {

                    // "add point" button de-selected, so re-enable
                    // the "Both" radio button
                    radioImageBoth.setEnabled(true);
                }
            }
        }
    }

    /**
     * keyPressed event method for KeyListener.
     *
     * @param  e  KeyEvent
     */
    public void keyPressed(KeyEvent e) {
        int keyCode = e.getKeyCode();

        if (!e.isControlDown()) {

            if ((keyCode >= '0') && (keyCode <= '9')) {

                int index = keyCode - 49;

                if (index < 0) {
                    index = 10;
                }

                if (!paintBrushLocked) {

                    if (quickPaintBrushIndex == index) {

                        for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                            if (triImage[i] != null) {
                                triImage[i].quickSwitchBrush();
                            }
                        }

                        triImage[AXIAL_A].getActiveImage().notifyImageDisplayListeners(null, true);
                        paintBrushLocked = true;
                    } else {
                        quickPaintBrushIndex = index;

                        String name = getControls().getTools().getPaintBrushName(index);

                        if (name != null) {

                            for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                                if (triImage[i] != null) {
                                    triImage[i].loadPaintBrush(name, true);
                                }
                            }

                            triImage[AXIAL_A].getActiveImage().notifyImageDisplayListeners(null, true);
                            paintBrushLocked = true;
                        }
                    }
                }

                return;
            }
        }
		
		// pass the key bindings to the underlying image (plb)
		String command = null;
        KeyStroke ks = KeyStroke.getKeyStrokeForEvent(e);

        command = Preferences.getShortcutCommand(ks);

        if (command != null) {
            parentFrame.actionPerformed(new ActionEvent(ks, 0, command));
        }

    }

    /**
     * keyReleased event method for KeyListener.
     *
     * @param  e  KeyEvent
     */
    public void keyReleased(KeyEvent e) {
        int keyCode = e.getKeyCode();

        switch (keyCode) {

            case KeyEvent.VK_B:

                // swap the border painting
            	if(Preferences.is(Preferences.PREF_SHOW_PAINT_BORDER)) {
                	getControls().getTools().bogusBorderPaintButton.setSelected(true);
                	bogusBorderPaintButton.setSelected(true);
                }else {
                	getControls().getTools().borderPaintButton.setSelected(true);
                	borderPaintButton.setSelected(true);
                }
            	
                Preferences.setProperty(Preferences.PREF_SHOW_PAINT_BORDER, String.valueOf("" + !Preferences.is(Preferences.PREF_SHOW_PAINT_BORDER)));
                triImage[AXIAL_A].getActiveImage().notifyImageDisplayListeners(null, true);
                if (triImage[AXIAL_B] != null) {
                    triImage[AXIAL_B].getActiveImage().notifyImageDisplayListeners(null, true);
                }

                break;

        }

        if (!e.isControlDown()) {

            if ((keyCode >= '0') && (keyCode <= '9')) {

                if (paintBrushLocked) {

                    // getControls().getTools().setPaintBrush(previousPaintBrushIndex);
                    for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                        if (triImage[i] != null) {
                            triImage[i].quickSwitchBrush();
                        }
                    }

                    triImage[AXIAL_A].getActiveImage().notifyImageDisplayListeners(null, true);
                    paintBrushLocked = false;
                }
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
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void mouseClicked(MouseEvent event) {

        if (event.getButton() == MouseEvent.BUTTON3) {

            if (event.getSource() instanceof AbstractButton) {
                AbstractButton btnSource = (AbstractButton) event.getSource();

                if (btnSource.getActionCommand().equals("MagImage") ||
                        btnSource.getActionCommand().equals("UnMagImage") ||
                        btnSource.getActionCommand().equals("IndivMagImage") ||
                        btnSource.getActionCommand().equals("IndivMinImage")) {

                    handleZoomPopupMenu(btnSource, event);

                    return;
                }
            }
        }


        if (event.getSource() instanceof ViewJComponentTriImage) {

            // since we set the mode to xooming in to all frames...we just need to check one of the frames
            // to see what mode we are in
            if (triImage[0] != null) {

                // get coordinates of where user clicked..used for adjusting scrollbars
                int x = event.getX();
                int y = event.getY();

                if (triImage[0].getCursorMode() == ViewJComponentBase.ZOOMING_IN) {

                    // get frame number
                    int frame = (new Integer(((ViewJComponentTriImage) event.getSource()).getName())).intValue();

                    // zoom in
                    zoomInFrame(frame);

                    // adjust scrollbars
                    adjustScrollbars(frame, x, y);

                    if (event.isShiftDown()) {
                        // do nothing
                    } else {

                        // reset mode
                        triImage[0].setCursorMode(ViewJComponentBase.DEFAULT);
                        traverseButton.setSelected(true);
                    }


                    // if after zooming a particular frame, all the frame are of the same zoom,
                    // then we should enable the global  zooms...otherwise disable them
                    float zoomX = triImage[0].getZoomX();
                    boolean test = true;

                    for (int i = 1; i < MAX_TRI_IMAGES; i++) {

                        if (triImage[i] != null) {

                            if (zoomX != triImage[i].getZoomX()) {
                                test = false;
                            }

                            if (event.isShiftDown()) {
                                // do nothing
                            } else {

                                // also..lets reset the mode for the others
                                triImage[i].setCursorMode(ViewJComponentBase.DEFAULT);
                            }
                        }
                    }

                    if (test) {
                        magButton.setEnabled(true);
                        minButton.setEnabled(true);
                    } else {
                        magButton.setEnabled(false);
                        minButton.setEnabled(false);
                    }
                } else if (triImage[0].getCursorMode() == ViewJComponentBase.ZOOMING_OUT) {

                    // get frame number
                    int frame = (new Integer(((ViewJComponentTriImage) event.getSource()).getName())).intValue();

                    // zoom out
                    zoomOutFrame(frame);

                    // adjust scrollbars
                    adjustScrollbars(frame, x, y);

                    if (event.isShiftDown()) {
                        // do nothing
                    } else {

                        // reset mode
                        triImage[0].setCursorMode(ViewJComponentBase.DEFAULT);
                        traverseButton.setSelected(true);
                    }

                    // if after zooming a particular frame, all the frame are of the same zoom,
                    // then we should enable the global  zooms...otherwise disable them
                    float zoomX = triImage[0].getZoomX();
                    boolean test = true;

                    for (int i = 1; i < MAX_TRI_IMAGES; i++) {

                        if (triImage[i] != null) {

                            if (zoomX != triImage[i].getZoomX()) {
                                test = false;
                            }

                            if (event.isShiftDown()) {
                                // do nothing
                            } else {

                                // also..lets reset the mode for the others
                                triImage[i].setCursorMode(ViewJComponentBase.DEFAULT);
                            }
                        }
                    }

                    if (test) {
                        magButton.setEnabled(true);
                        minButton.setEnabled(true);
                    } else {
                        magButton.setEnabled(false);
                        minButton.setEnabled(false);
                    }
                }

            }

        }


    }

    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void mouseEntered(MouseEvent event) { }

    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void mouseExited(MouseEvent event) { }

    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void mousePressed(MouseEvent event) { }

    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void mouseReleased(MouseEvent event) { }

    /**
     * Removes the menu and controls of the main frame so that a new frame can load the main frame with the proper
     * controls. Currently unused.
     */
    public void removeControls() { }

    /**
     * Remove a coordinate change listener from this frame's list.
     *
     * @param  listener  a coordinate change listener (hopefully in the list..)
     */
    public void removeCoordinateChangeListener(CoordinateChangeListener listener) {
        coordinateListeners.remove(listener);
    }

    /**
     * Set the active image for drawing VOIs. VOIs are only drawn in the active image. In addition, algorithms are
     * executed on the active window.
     *
     * @param  active  ViewJComponentBase.IMAGE_A or ViewJComponentBase.IMAGE_B
     */
    public void setActiveImage(int active) {
        for (int i = 0; i < MAX_TRI_IMAGES; i++) {

            if (triImage[i] != null) {
                triImage[i].setActiveImage(active);
            }
        }

        if (active == ViewJComponentBase.IMAGE_B) {
            displayMode = ViewJComponentBase.IMAGE_B;
            setTitle();
            radioImageB.setSelected(true);
            setSpinnerValues(imageB.getType());
        } else {
            displayMode = ViewJComponentBase.IMAGE_A;
            setTitle();
            radioImageA.setSelected(true);
            setSpinnerValues(imageA.getType());
        }

        /** updateImages(false) was commented to facilitate placement of VOI points during
         * image registration. This allows the user to control the crosshairs and image slice independently of other
         * images.
         */
        // updateImages(false);
    }

    /**
     * Sets the alpha blending of parameter for two image displaying.
     *
     * @param  value  amount [0,100] that is the percentage of Image A to be displayed
     */
    public void setAlphaBlend(int value) {

        for (int i = 0; i < MAX_TRI_IMAGES; i++) {

            if (triImage[i] != null) {
                triImage[i].setAlphaBlend(value);
            }
        }
    }


    /**
     * sets the crosshair positions and slices for each of the triImages. The inputs are in FileCoordinates, and are
     * passed to the triImages in FileCoordinates. Each triImage converts from FileCoordinates to the local
     * PatientCoordinate space, based on the triImage orientation (FileInfoBase.AXIAL, FileInfoBase.CORONAL,
     * FileInfoBase.SAGITTAL).
     *
     * @param  i  model space coordinate
     * @param  j  model space coordinate
     * @param  k  model space coordinate
     */
    public void setCenter(int i, int j, int k) {
    	setCenter(i,j,k, true);
    }
    
    /**
     * sets the crosshair positions and slices for each of the triImages. The inputs are in FileCoordinates, and are
     * passed to the triImages in FileCoordinates. Each triImage converts from FileCoordinates to the local
     * PatientCoordinate space, based on the triImage orientation (FileInfoBase.AXIAL, FileInfoBase.CORONAL,
     * FileInfoBase.SAGITTAL).
     *
     * @param  i  model space coordinate
     * @param  j  model space coordinate
     * @param  k  model space coordinate
     * @param checkLinkedScroll (boolean telling whether to look for linked images to sync scroll... necessary to avoid infinite loop)
     */
    public void setCenter(int i, int j, int k, boolean checkLinkedScroll) {
        i = (i < 0) ? 0 : ((i >= extents[0]) ? (extents[0] - 1) : i);
        j = (j < 0) ? 0 : ((j >= extents[1]) ? (extents[1] - 1) : j);
        k = (k < 0) ? 0 : ((k >= extents[2]) ? (extents[2] - 1) : k);

        setVolumeCenter(i, j, k);

        for (int image = 0; image < MAX_TRI_IMAGES; image++) {

            if (triImage[image] != null) {
                triImage[image].setCenter(i, j, k);
            }
        }

        if (checkLinkedScroll && linkedScrolling) {
        	
        	Enumeration names = userInterface.getRegisteredImageNames();

            boolean sameDims = false;
            while (names.hasMoreElements()) {
                String name = (String) names.nextElement();
                sameDims = true;

                if (!imageA.getImageName().equals(name)) {
                    ModelImage img = userInterface.getRegisteredImageByName(name);

                    if (img.getTriImageFrame() != null) {

                        if (imageA.getNDims() == img.getNDims()) {

                            for (int z = 0; z < imageA.getNDims(); z++) {

                                if (imageA.getExtents()[z] != img.getExtents()[z]) {
                                    sameDims = false;
                                }
                            }

                            if (sameDims == true) {
                            	img.getTriImageFrame().setCenter(i, j, k, false);
                                img.getTriImageFrame().setTimeSlice(tSlice, false);	
                            }
                        }
                    }
                }
            }
        }
        
        // BEN
        if (scrollOriginalCrosshair) {
            parentFrame.setSlice(k);
        }
        
        if ( voiManager != null )
        {
            voiManager.setCenter( new Vector3f( i, j, k ) );
        }

        fireCoordinateChange(i, j, k);
        setPositionLabels(i, j, k);
        updateImages(true);
    }


    /**
     * Sets the menu and controls (i.e. toolbars) of the main frame! This puts the menus and controls needed to controls
     * the operations of this frame. Different image frames have different menu and controls. Currently unused.
     */
    public void setControls() { }

    /**
     * Sets the new crop volume in the ViewJComponentTriImage frames and sets the volumeBounds.
     *
     * @param  lower  the lower corner of the crop volume in File Coordinates
     * @param  upper  the upper corner of the crop volume in File Coordinates
     */
    public void setCrop(Vector3f lower, Vector3f upper) {

        /* set the crop dimensions for each triImage: */
        for (int i = 0; i < MAX_TRI_IMAGES; i++) {

            if (triImage[i] != null) {
                triImage[i].setCrop(lower, upper);
            }
        }

        /* set the volumeBounds: */
        setVolumeBounds(lower, upper);
    }

    /**
     * Sets modes in all images to ViewJComponentBase.DEFAULT.
     */
    public void setDefault() {

        for (int i = 0; i < MAX_TRI_IMAGES; i++) {

            if (triImage[i] != null) {
                triImage[i].setCursorMode(ViewJComponentBase.DEFAULT);
            }
        }
    }

    /**
     * Controls whether or not the images/VOIs of the frame can be modified.
     *
     * @param  flag  if true the image/VOIs can be modified; if false image/VOIs can NOT be modified
     */
    public void setEnabled(boolean flag) {

        for (int i = 0; i < MAX_TRI_IMAGES; i++) {

            if (triImage[i] != null) {
                triImage[i].setEnabled(flag);
            }
        }
    }

    /**
     * Changes imageA to a new model image reference. Swaps the references in the frame and all the component images.
     *
     * @param  image  the new image to use
     */
    public void setImageA(ModelImage image) {
        super.setImageA(image);

        for (int i = 0; i < MAX_TRI_IMAGES; i++) {

            if (triImage[i] != null) {
                triImage[i].setImageA(image);
            }
        }

        // Get all frames
        Vector frameList = image.getImageFrameVector();

        if (frameList == null) {
            return;
        }

        for (int i = 0; i < frameList.size(); i++) {

            if (((ViewJFrameBase) frameList.elementAt(i)) != this) {
                ((ViewJFrameBase) frameList.elementAt(i)).setImageA(image);
            }
        }

        image.setImageOrder(ModelImage.IMAGE_A);

        image.addImageDisplayListener(this);

        if (image.getHistoLUTFrame() != null) {
            updateHistoLUTFrame(ViewJComponentBase.IMAGE_A);
        }

        setActiveImage(ViewJComponentBase.IMAGE_A);
    }

    /**
     * Accessor that sets the reference to imageB. Includes changing the frame's reference and the references the
     * components keep.
     *
     * @param  _imageB  image to set the frame to
     */
    public void setImageB(ModelImage _imageB) {

        if (imageB != null) {
            imageB.disposeLocal();
        } // Dispose of the memory of the old image

        imageB = _imageB;
        imageB.setImageOrder(ModelImage.IMAGE_B);

        // tri image objects must be rebuilt with the new image B
        triImage[AXIAL_AB] = buildTriImage(imageA, LUTa, imageB, LUTb, FileInfoBase.AXIAL);
        triImage[SAGITTAL_AB] = buildTriImage(imageA, LUTa, imageB, LUTb, FileInfoBase.SAGITTAL);
        triImage[CORONAL_AB] = buildTriImage(imageA, LUTa, imageB, LUTb, FileInfoBase.CORONAL);
        triImage[AXIAL_B] = buildTriImage(imageB, LUTb, null, null, FileInfoBase.AXIAL);
        triImage[CORONAL_B] = buildTriImage(imageB, LUTb, null, null, FileInfoBase.CORONAL);
        triImage[SAGITTAL_B] = buildTriImage(imageB, LUTb, null, null, FileInfoBase.SAGITTAL);

        imageB.addImageDisplayListener(this);

        if (imageB.isColorImage() == false) {
            int[] dimExtentsLUT = new int[2];
            dimExtentsLUT[0] = 4;
            dimExtentsLUT[1] = 256;

            if (LUTb == null) {
                LUTb = new ModelLUT(ModelLUT.HOTMETAL, 256, dimExtentsLUT);
            }

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
            setLUTb(LUTb);
        }

        setImageSelectorPanelVisible(true);

        leastSquaresButton.setEnabled(true);
        tpSplineButton.setEnabled(true);

        // Get all frames
        Vector frameList = imageB.getImageFrameVector();

        if (frameList == null) {
            return;
        }

        for (int i = 0; i < frameList.size(); i++) {

            if (((ViewJFrameBase) frameList.elementAt(i)) != this) {
                ((ViewJFrameBase) frameList.elementAt(i)).setImageB(imageB);
            }
        }

        if (imageB.getHistoLUTFrame() != null) {
            updateHistoLUTFrame(ViewJComponentBase.IMAGE_B);
        }

        setActiveImage(ViewJComponentBase.IMAGE_B);

        zoom = getOptimalZoom(DEFAULT_OPTIMAL_ZOOM, DEFAULT_OPTIMAL_ZOOM);

        for (int i = 0; i < MAX_TRI_IMAGES; i++) {

            if (triImage[i] != null) {
                triImage[i].setZoom(zoom, zoom);
            }
        }


        /* set the center after the triImage[].setResolutions and
         * triImage[].setZoom calls have been made: */
        setCenter((extents[0] - 1) / 2, (extents[1] - 1) / 2, (extents[2] - 1) / 2);

        updateLayout();
    }

    /**
     * this method either shows or hides the image selector panel, based on the value of the parameter.
     *
     * @param  visible  DOCUMENT ME!
     */
    public void setImageSelectorPanelVisible(boolean visible) {

        if (visible == true) {

            if (imageB != null) // do not show if we don't have a mask
            {
                panelToolBarGBC.gridy = 4;
                panelToolBarGBC.gridwidth = 1;
                panelToolBarGBC.gridheight = 1;
                panelToolBarGBC.fill = GridBagConstraints.BOTH;
                panelToolBarGBC.anchor = GridBagConstraints.WEST;
                panelToolBarGBC.weightx = 100;
                panelToolBarGBC.weighty = 100;

                panelToolbar.add(panelActiveImage, panelToolBarGBC);
            }
        } else {
            panelToolbar.remove(panelActiveImage);
        }

        validate();
    }

    /**
     * Sets paint intensity in axial image.
     *
     * @param  intensityDropper  the paint intensity value for the XY image
     */
    public void setIntensityDropper(float intensityDropper) {

        for (int i = 0; i < MAX_TRI_IMAGES; i++) {

            if (triImage[i] != null) {
                triImage[i].setIntensityDropper(intensityDropper);
            }
        }
    }

    /**
     * Sets integer value on intensityPaintButton.
     *
     * @param  intensityDropper  the paint button intensity value to show
     */
    public void setIntensityPaintName(float intensityDropper) {
        Double doubleValue = new Double((double) intensityDropper);
        ((SpinnerNumberModel) intensitySpinner.getModel()).setValue(doubleValue);
    }

    /**
     * Accessor that sets the LUT.
     *
     * @param  LUT  the LUT
     */
    public void setLUTa(ModelLUT LUT) {

        if (triImage[AXIAL_AB] != null) {
            triImage[AXIAL_AB].setLUTa(LUT);
        }

        if (triImage[CORONAL_AB] != null) {
            triImage[CORONAL_AB].setLUTa(LUT);
        }

        if (triImage[SAGITTAL_AB] != null) {
            triImage[SAGITTAL_AB].setLUTa(LUT);
        }

        if (triImage[AXIAL_B] != null) {
            triImage[AXIAL_B].setLUTa(LUT);
        }

        if (triImage[CORONAL_B] != null) {
            triImage[CORONAL_B].setLUTa(LUT);
        }

        if (triImage[SAGITTAL_B] != null) {
            triImage[SAGITTAL_B].setLUTa(LUT);
        }

        updateImages(true);
    }

    /**
     * Accessor that sets the LUT.
     *
     * @param  LUT  the LUT
     */
    public void setLUTb(ModelLUT LUT) {

        if (triImage[AXIAL_AB] != null) {
            triImage[AXIAL_AB].setLUTb(LUT);
        }

        if (triImage[CORONAL_AB] != null) {
            triImage[CORONAL_AB].setLUTb(LUT);
        }

        if (triImage[SAGITTAL_AB] != null) {
            triImage[SAGITTAL_AB].setLUTb(LUT);
        }

        if (triImage[AXIAL_B] != null) {
            triImage[AXIAL_B].setLUTa(LUT); // must set LUT a because image B is in image A slot
        }

        if (triImage[CORONAL_B] != null) {
            triImage[CORONAL_B].setLUTa(LUT); // must set LUT a because image B is in image A slot
        }

        if (triImage[SAGITTAL_B] != null) {
            triImage[SAGITTAL_B].setLUTa(LUT); // must set LUT a because image B is in image A slot
        }

        updateImages(true);
    }

    /**
     * When switching the active image, take the paintBitmap of the previous active image as the paintBitmap of the new
     * active image Currenlty unused.
     *
     * @param  paintBitmapSwitch  if true don't do a getMask on the new actve image
     */
    public void setPaintBitmapSwitch(boolean paintBitmapSwitch) { }

    /**
     * Sets the color of the paint.
     *
     * @param  color  Color the desired color of the paint
     */
    public void setPaintColor(Color color) {
        this.color = color;

        colorPaintButton.setBackground(color);

        for (int i = 0; i < MAX_TRI_IMAGES; i++) {

            if (triImage[i] != null) {
                triImage[i].getActiveImage().notifyImageDisplayListeners(null, true);
            }
        }
    }

    /**
     * Sets the labels which show the absolute position within the image volume and the patient position.
     *
     * @param  x  the x volume coordinate
     * @param  y  the y volume coordinate
     * @param  z  the z volume coordinate
     */
    public void setPositionLabels(int x, int y, int z) {
        setAbsPositionLabels(new Vector3f(x, y, z));
        setScannerPosition(new Vector3f(x, y, z));

        if (showTalairachPosition) {
            setTalairachPositionLabels(x, y, z);
        }

        currentAbsolutePositionLabels = new Point3D(x, y, z);

        if (linkTriFrame != null) {
            linkTriFrame.setSlicesFromFrame(x, y, z);
        }

        // BEN

    }

    /**
     * Sets the RGB table for ARGB image A.
     *
     * @param  RGBT  the new RGB transfer functions for imageA
     */
    public void setRGBTA(ModelRGB RGBT) {

        for (int i = 0; i < MAX_TRI_IMAGES; i++) {

            if (triImage[i] != null) {
                triImage[i].setRGBTA(RGBT);
            }
        }
    }

    /**
     * Sets the RGB table for image B.
     *
     * @param  RGBT  the new RGB transfer functions for imageB
     */
    public void setRGBTB(ModelRGB RGBT) {

        for (int i = 0; i < MAX_TRI_IMAGES; i++) {

            if (triImage[i] != null) {
                triImage[i].setRGBTB(RGBT);
            }
        }
    }
    
    /**
     * Sets whether the linking button should be set for this image, implies that 
     * this image will be linked to another tri-frame when true
     */
    
    public void setLinkButtonSelected(boolean selected) {
        scrollButton.setSelected(selected);
    }

    /**
     * Does nothing.
     *
     * @param  slice  the slice to show
     */
    public void setSlice(int slice) { }

    /**
     * Sets the slice index for each plane in the frame and components. Should be zero indexed.
     *
     * @param  x  slice index in the patient
     * @param  y  slice index in the patient
     * @param  z  slice index in the patient
     */
    public void setSlices(int x, int y, int z) {
        setCenter(x, y, z);
    }

    /**
     * Called from the &quot;normal&quot; image component, sets the slices for the tri planar view to display.
     * Parameters are in terms of the image volume and so must be converted.
     *
     * @param  x  X Slice of image.
     * @param  y  Y Slice of image.
     * @param  z  Z Slice of image.
     */

    public void setSlicesFromFrame(int x, int y, int z) {
        Vector3f inPoint = new Vector3f();
        inPoint.X = x;
        inPoint.Y = y;
        inPoint.Z = z;

        // x, y, z passed in from ViewJComponentEditImage.mouseReleased() are
        // already in image volume space
        setCenter(x, y, z);
        setPositionLabels(x, y, z);
    }

    /**
     * Sets the text to display in the Talairach voxel label. This text has a format corresponding to the Talairach
     * grid. Example: "AcL3" - an full explanation of what the text means is beyond the scope of this comment.
     *
     * @param  newLabelText  the text to display in the Talairach voxel label
     */
    public void setTalairachVoxelLabelText(String newLabelText) {
        talairachVoxelLabel.setText(newLabelText);
    }

    /**
     * Sets the slice to be displayed and updates title frame.
     *
     * @param  slice  indicates image time-slice (4th dimension) to be displayed
     */
    public void setTimeSlice(int slice) {
        setTimeSlice(slice, true);
    }
    
    /**
     * Sets the slice to be displayed and updates title frame.
     *
     * @param  slice  indicates image time-slice (4th dimension) to be displayed
     * @param checkedLinkedScroll whether corresponding tri-frames should also be scrolled
     */
    public void setTimeSlice(int slice, boolean checkedLinkedScroll) {

        if (((imageA.getNDims() <= 3) && (imageB == null)) ||
                ((imageA.getNDims() <= 3) && (imageB != null) && (imageB.getNDims() <= 3))) {
            return;
        }

        ModelImage compImg = null;
        if(((imageA.getNDims() == 4) && (tSlice < imageA.getExtents()[3]))) {
            compImg = imageA;
        } else if((imageB != null) && (imageB.getNDims() == 4) && (tSlice < imageB.getExtents()[3])) {
            compImg = imageB;
        }
        
        if (((imageA.getNDims() == 4) && (tSlice < imageA.getExtents()[3])) ||
                ((imageB != null) && (imageB.getNDims() == 4) && (tSlice < imageB.getExtents()[3]))) {
            tSlice = slice;
            updateImages(true);
            setTitle();
            setTSlider(slice);
        }
       
        
        if (checkedLinkedScroll && linkedScrolling && compImg != null) {
            
            int[] center = compImg.getTriImageFrame().getCenter();
            Enumeration names = userInterface.getRegisteredImageNames();

            boolean sameDims = false;
            while (names.hasMoreElements()) {
                String name = (String) names.nextElement();
                sameDims = true;

                if (!compImg.getImageName().equals(name)) {
                    ModelImage img = userInterface.getRegisteredImageByName(name);

                    if (img.getTriImageFrame() != null) {

                        if (compImg.getNDims() == img.getNDims()) {

                            for (int z = 0; z < compImg.getNDims(); z++) {

                                if (compImg.getExtents()[z] != img.getExtents()[z]) {
                                    sameDims = false;
                                }
                            }

                            if (sameDims == true) {
                                img.getTriImageFrame().setCenter(center[0], center[1], center[2], false);
                                img.getTriImageFrame().setTimeSlice(tSlice, false);    
                            }
                        }
                    }
                }
            }
        }
    }
    
    /**
     * Sets the title bar for the tri-image frame.  Called for initialization and updating.
     * Displays time series location for 4D volumes.
     */
    public void setTitle() {
        String str;

        if (displayMode == ViewJComponentBase.IMAGE_A) {
            if (imageA.getNDims() > 3) { // Setup the title for 4D image
                str = imageA.getImageName() + "  "+ tSlice + "/" + (tDim - 1)
                        + "t M:" + makeString(triImage[AXIAL_A].getZoomX(), 2);
                setTitle(str);
            } else {
                str = imageA.getImageName() + "  M:" + makeString(triImage[AXIAL_A].getZoomX(), 2);
                setTitle(str);
            }
        } else if ((imageB != null) && (triImage[AXIAL_B] != null)) { 
            if (imageB.getNDims() > 3) { // Setup the title for 4D image
                str = imageB.getImageName() + "  " + tSlice + "/" + (tDim - 1)
                        + "t M:" + makeString(triImage[AXIAL_B].getZoomX(), 2);
                setTitle(str);
            } else {
                str = imageB.getImageName() + "  M:" + makeString(triImage[AXIAL_B].getZoomX(), 2);
                setTitle(str);
            }
        }
    }

    /**
     * Sets the traverse button to selected.
     */
    public void setTraverseButton() {
        traverseButton.setSelected(true);

        if ((imageB != null) && (!radioImageBoth.isEnabled())) {
            radioImageBoth.setEnabled(true);
        }

        for (int i = 0; i < MAX_TRI_IMAGES; i++) {

            if (triImage[i] != null) {
                triImage[i].setCursorMode(ViewJComponentBase.DEFAULT);
                triImage[i].setProtractorVisible(false);
                triImage[i].setIntensityLineVisible(false);

            }
        }

        updateImages(true);
    }

    /**
     * Sets the x coordinate of the point to be the center of the transformed image.
     *
     * @param  newVolumeCenter  The x coordinate of the center.
     */
    public void setVolumeCenter(Point3D newVolumeCenter) {
        volumeCenter[0] = newVolumeCenter.x;
        volumeCenter[1] = newVolumeCenter.y;
        volumeCenter[2] = newVolumeCenter.z;
    }

    /**
     * Sets the x coordinate of the point to be the center of the transformed image.
     *
     * @param  x  The x coordinate of the center.
     * @param  y  DOCUMENT ME!
     * @param  z  DOCUMENT ME!
     */
    public void setVolumeCenter(int x, int y, int z) {
        volumeCenter[0] = x;
        volumeCenter[1] = y;
        volumeCenter[2] = z;
    }

    /**
     * Sets the color of the X slice crosshairs.
     *
     * @param  c  Color the color to set the X slice to
     */
    public void setXSliceHairColor(Color c) {

        for (int i = 0; i < MAX_TRI_IMAGES; i++) {

            if (triImage[i] != null) {
                triImage[i].setXSliceHairColor(c);
            }
        }

        updateImages(true);
    }

    /**
     * Sets the color of the Y slice crosshairs.
     *
     * @param  c  Color the color to set the Y slice to
     */
    public void setYSliceHairColor(Color c) {

        for (int i = 0; i < MAX_TRI_IMAGES; i++) {

            if (triImage[i] != null) {
                triImage[i].setYSliceHairColor(c);
            }
        }

        updateImages(true);
    }

    /**
     * Sets the color of the Z slice crosshairs.
     *
     * @param  c  Color the color to set the Z slice to
     */
    public void setZSliceHairColor(Color c) {

        for (int i = 0; i < MAX_TRI_IMAGES; i++) {

            if (triImage[i] != null) {
                triImage[i].setZSliceHairColor(c);
            }
        }

        updateImages(true);
    }

    /**
     * Sets values based on knob along time slider.
     *
     * @param  e  Event that triggered this function
     */
    public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();

        if (source == tImageSlider) {

            int newValue = 1;

            // removed to make the slider behavior match the time slider in the main mipav frame
            // if (tImageSlider.getValueIsAdjusting() == true) {
            // return;
            // }

            newValue = Math.round((tImageSlider.getValue() / 100.0f * (extents[3] - 1)) - 0.01f);
            setTimeSlice(newValue);
            controls.setTimeSl(newValue);
        } else if (source == intensitySpinner) {
            float paintIntensity = ((SpinnerNumberModel) intensitySpinner.getModel()).getNumber().floatValue();

            for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                if (triImage[i] != null) {
                    triImage[i].setIntensityDropper(paintIntensity);
                }
            }
        } else if (source == crosshairSpinner) {
            int crosshairPixelGap = ((SpinnerNumberModel) crosshairSpinner.getModel()).getNumber().intValue();

            // System.err.println(crosshairPixelGap);
            for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                if (triImage[i] != null) {
                    triImage[i].setCrosshairPixelGap(crosshairPixelGap);
                }

                Preferences.setProperty(Preferences.PREF_CROSSHAIR_PIXEL_GAP, Integer.toString(crosshairPixelGap));
                this.updateImages();
            }
        }
    }

    /**
     * Do nothing - required by ViewJFrameBase.
     *
     * @return  always false
     */
    public boolean updateImageExtents() {
        return false;
    }

    /**
     * This methods calls the componentImage's update method to redraw the screen.
     *
     * @return  boolean confirming successful update
     */
    public boolean updateImages() {
        return updateImages(null, null, true, -1);
    }

    /**
     * This methods calls the componentImage's update method to redraw the screen. Without LUT changes.
     *
     * @param   forceShow  forces show to reimport image and calc. java image
     *
     * @return  boolean confirming successful update
     */
    public boolean updateImages(boolean forceShow) {
        return updateImages(null, null, forceShow, -1);
    }

    /**
     * This methods calls the componentImage's update method to redraw the screen.
     *
     * @param   LUTa        LUT used to update imageA
     * @param   LUTb        LUT used to update imageB
     * @param   forceShow   forces show to reimport image and calc. java image
     * @param   interpMode  image interpolation method (Nearest or Smooth)
     *
     * @return  boolean confirming successful update
     */
    public boolean updateImages(ModelLUT LUTa, ModelLUT LUTb, boolean forceShow, int interpMode) {

        for (int i = 0; i < MAX_TRI_IMAGES; i++) {

            if (triImage[i] != null) {

                // redraw the paint brush cursor (quick)
                triImage[i].updatePaintBrushCursor();

                if (triImage[i].show(tSlice, LUTa, LUTb, forceShow, interpMode) == false) {
                    return false;
                }
            }
        }

        return true;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  triImage  DOCUMENT ME!
     */
    public void updateImageSubset(ViewJComponentTriImage triImage) {

        if ((triImage == this.triImage[AXIAL_A]) || (triImage == this.triImage[CORONAL_A]) ||
                (triImage == this.triImage[SAGITTAL_A])) {
            this.triImage[AXIAL_A].show(tSlice, null, null, true, -1);
            this.triImage[CORONAL_A].show(tSlice, null, null, true, -1);
            this.triImage[SAGITTAL_A].show(tSlice, null, null, true, -1);
        } else if ((triImage == this.triImage[AXIAL_B]) || (triImage == this.triImage[CORONAL_B]) ||
                       (triImage == this.triImage[SAGITTAL_B])) {
            this.triImage[AXIAL_B].show(tSlice, null, null, true, -1);
            this.triImage[CORONAL_B].show(tSlice, null, null, true, -1);
            this.triImage[SAGITTAL_B].show(tSlice, null, null, true, -1);
        } else if ((triImage == this.triImage[AXIAL_AB]) || (triImage == this.triImage[CORONAL_AB]) ||
                       (triImage == this.triImage[SAGITTAL_AB])) {
            updateImages();
        }

        if (linkTriFrame != null) {

            for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                if (triImage == this.triImage[i]) {
                    linkTriFrame.updateImageSubset(linkTriFrame.getTriImage(i));
                }
            }
        }

        return;
    }

    /**
     * Updates the VOI ID for the three component images.
     *
     * @param  voiID  New VOI ID.
     */
    public void updatevoiID(int voiID) {

        for (int i = 0; i < MAX_TRI_IMAGES; i++) {

            if (triImage[i] != null) {
                triImage[i].getVOIHandler().setVOI_ID(voiID);
            }
        }
    }

    /**
     * Closes window and disposes of frame and component.
     *
     * @param  event  Event that triggered function
     */
    public void windowClosing(WindowEvent event) {

        if (volumePositionFrame != null) {
            volumePositionFrame.dispose();
            volumePositionFrame = null;
        }

        close();
        this.disposeLocal();
    }

    public void windowOpened(final WindowEvent event)
    {
        setOldLayout(oldLayout);
    }

    /**
     * Builds the active image panel for choosing which image (A, B, or BOTH) to perform operations on.
     */
    protected void buildActiveImagePanel() {
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
     *
     * @param  text     Text for button.
     * @param  toolTip  Tool tip to be associated with button.
     * @param  action   Action command for button.
     * @param  toolbar  Tool bar to add this button to.
     */
    protected void buildDisabledTextButton(String text, String toolTip, String action, JToolBar toolbar) {
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
     * Helper method to build an icon button for the toolbar.
     *
     * @param  icon      Name of icon for button.
     * @param  toolTip   Tool tip to be associated with button.
     * @param  action    Action command for button.
     * @param  iconroll  Name of icon for rollover.
     * @param  toolbar   Tool bar to add this button to.
     */
    protected void buildIconButton(String icon, String toolTip, String action, String iconroll, JToolBar toolbar) {
        JButton button = new JButton(MipavUtil.getIcon(icon));
        button.addActionListener(this);

        if ((action != null) && (action.equals("MagImage") || action.equals("UnMagImage"))) {
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
     * Builds menus for the tri-planar view.
     *
     * @return  the tri-image frame menu bar
     */
    protected JMenuBar buildMenu() {

        JSeparator separator = new JSeparator();

        this.showTalairachPosition = ((imageA.getMatrixHolder().containsType(TransMatrix.TRANSFORM_TALAIRACH_TOURNOUX)) &&
                                          (imageA.getTalairachTransformInfo() != null));

        menuObj = new ViewMenuBuilder(this);

        JMenuBar menuBar = new JMenuBar();
        menuBar.add(menuObj.makeMenu("File", false,
                                     new JComponent[] {
                                         separator, menuObj.buildMenuItem("Close frame", "CloseFrame", 0, null, false)
                                     }));
        menuBar.add(menuObj.makeMenu("Options", false,
                                     new JComponent[] {
                                         menuObj.buildCheckBoxMenuItem("Show axes", "ShowAxes", true),
                                         menuObj.buildCheckBoxMenuItem("Show crosshairs", "ShowXHairs", true),
                                         separator,
                                         menuObj.buildCheckBoxMenuItem("Scroll original image with crosshair",
                                                                       "ScrollOriginal", scrollOriginalCrosshair),
                                         separator,
                                         menuObj.buildCheckBoxMenuItem("Fast rendering in paint mode", "FastPaint",
                                                                       Preferences.is(Preferences.PREF_FAST_TRIPLANAR_REPAINT)),
                                         menuObj.buildCheckBoxMenuItem("Snap protractor to 90 degrees multiple",
                                                                       "Snap90",
                                                                       Preferences.is(Preferences.PREF_TRIPLANAR_SNAP90)),
                                         menuObj.buildCheckBoxMenuItem("Use 2x2 tri-planar layout", OLD_LAYOUT,
                                                                       oldLayout), separator,
                                         menuObj.buildMenuItem("Show volume coordinates and Talairach controls",
                                                               "PositionFrame", 0, null, false),
                                         menuObj.buildMenuItem("Link to another TriImage frame", "LinkFrame", 0, null,
                                                               false),
                                         menuObj.buildMenuItem("Select panel plug-in", PANEL_PLUGIN, 0, null, false)
                                     }));
        
        
        

        boolean showVOIToolbar = Preferences.is(Preferences.PREF_VOI_TOOLBAR_ON);
        final boolean showPaintToolbar = Preferences.is(Preferences.PREF_PAINT_TOOLBAR_ON);

        // default the VOI and Image toolbars to on if the user hasn't explicitly turned them off
        if ( !showVOIToolbar && !Preferences.isPreferenceSet(Preferences.PREF_VOI_TOOLBAR_ON)) {
            showVOIToolbar = true;
        }

        menuBar.add(menuObj.makeMenu("Toolbars", 'T', false, new JMenuItem[] {
                menuObj.buildCheckBoxMenuItem("Paint toolbar", "PaintToolbar", showPaintToolbar),
                menuObj.buildCheckBoxMenuItem("VOI toolbar", "VOIToolbar", false),
                menuObj.buildCheckBoxMenuItem("Image Align toolbar", "ImageAlignToolbar", false)
                }));

        return menuBar;
    }

    /**
     * Helper method to build a text button for the toolbar.
     *
     * @param   text     Text for button.
     * @param   toolTip  Tool tip to be associated with button.
     * @param   action   Action command for button.
     * @param   toolbar  Tool bar to add this button to.
     *
     * @return  the new named text button
     */
    protected JButton buildNamedTextButton(String text, String toolTip, String action, JToolBar toolbar) {
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
     * Constructs progress bar.
     *
     * @param  imageName  The name of the image.
     * @param  message    Message to be displayed in the frame
     * @param  start      Start (typical = 0)
     * @param  end        End (typical = 100)
     */
    protected void buildProgressBar(String imageName, String message, int start, int end) {
        progressBar = new ViewJProgressBar(imageName, message, start, end, false, null, null);
    }

    /**
     * Builds the slider labels for the time slider.
     *
     * @param   min  Min value of slider
     * @param   max  Max value of slider.
     *
     * @return  Slider labels hash.
     */
    protected Hashtable buildTImageSliderLabels(int min, int max) {
        Hashtable tImageSliderDictionary = new Hashtable();

        Font font12 = MipavUtil.font12;
        float rangeF = (max) / 4.0f;

        JLabel label1 = new JLabel("0");
        label1.setForeground(Color.black);
        label1.setFont(font12);
        tImageSliderDictionary.put(new Integer(0), label1);

        if ((max - min) > 3) {
            JLabel label2 = new JLabel(Integer.toString(Math.round(rangeF * 2)-1));
            label2.setForeground(Color.black);
            label2.setFont(font12);
            tImageSliderDictionary.put(new Integer(50), label2);
        }

        JLabel label5 = new JLabel(Integer.toString(max-1));
        label5.setForeground(Color.black);
        label5.setFont(font12);
        tImageSliderDictionary.put(new Integer(100), label5);

        return tImageSliderDictionary;
    }

    /**
     * Helper method to build a toggle button for the toolbar.
     *
     * @param  icon      Name of icon for button.
     * @param  toolTip   Tool tip to be associated with button.
     * @param  action    Action command for button.
     * @param  iconroll  Name of icon for rollover.
     * @param  group     Button group to add this toggle to.
     * @param  toolbar   Tool bar to add this button to.
     */
    protected void buildToggleButton(String icon, String toolTip, String action, String iconroll, ButtonGroup group,
                                     JToolBar toolbar) {
        buildToggleButton(icon, toolTip, action, iconroll, group, toolbar, false);
    }

    /**
     * Helper method to build a toggle button for the toolbar.
     *
     * @param  icon      Name of icon for button.
     * @param  toolTip   Tool tip to be associated with button.
     * @param  action    Action command for button.
     * @param  iconroll  Name of icon for rollover.
     * @param  group     Button group to add this toggle to.
     * @param  toolbar   Tool bar to add this button to.
     * @param  selected  whether or not the button is initially selected
     */
    protected void buildToggleButton(String icon, String toolTip, String action, String iconroll, ButtonGroup group,
                                     JToolBar toolbar, boolean selected) {
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
     * Builds the toolbars for the tri-planar view.
     */
    protected void buildToolbars() {
        toolbarBuilder = new ViewToolBarBuilder(this);
        buildActiveImagePanel();
        panelToolbar.setLayout(new GridBagLayout());

        panelToolBarGBC.gridx = 0;
        panelToolBarGBC.gridy = 2;
        panelToolBarGBC.gridwidth = 1;
        panelToolBarGBC.gridheight = 1;
        panelToolBarGBC.fill = GridBagConstraints.BOTH;
        panelToolBarGBC.anchor = GridBagConstraints.WEST;
        panelToolBarGBC.weightx = 1;
        panelToolBarGBC.weighty = 1;

        for (int i = 0; i < NUM_INVISIBLE_BUTTONS; i++) {
            btnInvisible[i] = new JToggleButton("");
        }

        Border etchedBorder = BorderFactory.createEtchedBorder();

        imageToolBar = new JToolBar();
        imageToolBar.setBorder(etchedBorder);
        imageToolBar.setBorderPainted(true);
        imageToolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        imageToolBar.setFloatable(false);

        traverseButton = toolbarBuilder.buildToggleButton(CustomUIBuilder.PARAM_TRIIMAGE_TRAVERSE, VOIGroup);
        traverseButton.setSelected(true);
        imageToolBar.add(traverseButton);

        imageToolBar.add(ViewToolBarBuilder.makeSeparator());


        magButton = toolbarBuilder.buildButton(CustomUIBuilder.PARAM_IMAGE_MAG);
        magButton.addMouseListener(this);
        imageToolBar.add(magButton);
        minButton = toolbarBuilder.buildButton(CustomUIBuilder.PARAM_IMAGE_UNMAG);
        minButton.addMouseListener(this);
        imageToolBar.add(minButton);
        imageToolBar.add(toolbarBuilder.buildButton(CustomUIBuilder.PARAM_IMAGE_MAG_ONE_TO_ONE));


        imageToolBar.add(ViewToolBarBuilder.makeSeparator());

        // ButtonGroup indivMagGroup = new ButtonGroup();
        indivMagButton = toolbarBuilder.buildToggleButton(CustomUIBuilder.PARAM_TRIIMAGE_MAG, VOIGroup);
        indivMagButton.addMouseListener(this);
        imageToolBar.add(indivMagButton);

        indivMinButton = toolbarBuilder.buildToggleButton(CustomUIBuilder.PARAM_TRIIMAGE_UNMAG,
                                                          VOIGroup);
        indivMinButton.addMouseListener(this);
        imageToolBar.add(indivMinButton);

        // bogusMagButton = toolbarBuilder.buildToggleButton("bogusMinImage", "Magnify individual frame 0.5x",
        // "trizoomout", indivMagGroup); bogusMagButton.setVisible(false); imageToolBar.add(bogusMagButton);

        //imageToolBar.add(ViewToolBarBuilder.makeSeparator());

        scrollButton = new JCheckBox(MipavUtil.getIcon("link_broken.gif"));
        scrollButton.setPreferredSize(new Dimension(24,24));
        scrollButton.setSelectedIcon(MipavUtil.getIcon("link.gif"));
        scrollButton.addActionListener(this);
        scrollButton.setActionCommand("ScrollLink");
        scrollButton.setToolTipText("Link tri-images of like-dimensions for scrolling.");
        
        imageToolBar.add(scrollButton);
        imageToolBar.add(ViewToolBarBuilder.makeSeparator());

        int crosshairPixelGap = 0;

        try {
            crosshairPixelGap = Integer.parseInt(Preferences.getProperty(Preferences.PREF_CROSSHAIR_PIXEL_GAP));
        } catch (Exception e) {
            Preferences.setProperty(Preferences.PREF_CROSSHAIR_PIXEL_GAP, "0");
        }

        imageToolBar.add(ViewToolBarBuilder.makeSeparator());
        crosshairSpinner = new JSpinner(new SpinnerNumberModel(crosshairPixelGap, 0, 25, 1));
        crosshairSpinner.setMaximumSize(new Dimension(56, 24)); // 56 pixlls wide, 24 tall
        crosshairSpinner.setPreferredSize(new Dimension(56, 24)); // 56 pixlls wide, 24 tall
        crosshairSpinner.addChangeListener(this);
        crosshairSpinner.setToolTipText("Crosshair pixel gap"); // bug in API prevents this from working

        imageToolBar.add(crosshairSpinner);

        panelToolbar.add(imageToolBar, panelToolBarGBC);

        // Paint toolbar
        paintToolBar = new JToolBar();
        paintToolBar.setBorder(etchedBorder);
        paintToolBar.setBorderPainted(true);
        paintToolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        paintToolBar.setFloatable(false);

        paintToolBar.add(toolbarBuilder.buildToggleButton(CustomUIBuilder.PARAM_PAINT_BRUSH, VOIGroup));
        dropperPaintToggleButton = toolbarBuilder.buildToggleButton(CustomUIBuilder.PARAM_PAINT_DROPPER, VOIGroup);
        dropperPaintToggleButton.addItemListener(this);
        paintToolBar.add(dropperPaintToggleButton);

        paintCanToggleButton = toolbarBuilder.buildToggleButton(CustomUIBuilder.PARAM_PAINT_FILL, VOIGroup);
        paintCanToggleButton.addItemListener(this);
        paintToolBar.add(paintCanToggleButton);

        paintToolBar.add(toolbarBuilder.buildToggleButton(CustomUIBuilder.PARAM_PAINT_ERASER, VOIGroup));
        paintToolBar.add(toolbarBuilder.buildButton(CustomUIBuilder.PARAM_PAINT_ERASE_ALL));
        paintToolBar.add(ViewToolBarBuilder.makeSeparator());

        // create the list of brushes

        String userBrushes = System.getProperty("user.home") + File.separator + "mipav" + File.separator + "brushes" +
                             File.separator;

        int numBrushes = ViewToolBarBuilder.NUM_BRUSHES_INTERNAL; // built in... 5 so far

        File brushesDir = new File(userBrushes);

        if (brushesDir.isDirectory()) {
            File[] brushes = brushesDir.listFiles();

            for (int i = 0; i < brushes.length; i++) {

                if (brushes[i].getName().endsWith(".png")) {
                    numBrushes++;
                }
            }
        }

        Integer[] intArray = new Integer[numBrushes];

        for (int i = 0; i < intArray.length; i++) {
            intArray[i] = new Integer(i);
        }

        paintBrushNames = new String[numBrushes];

        paintBrushNames[0] = "square 1x1.gif";
        paintBrushNames[1] = "square 4x4.gif";
        paintBrushNames[2] = "square 8x8.gif";
        paintBrushNames[3] = "square 16x16.gif";
        paintBrushNames[4] = "square 24x24.gif";
        paintBrushNames[5] = "circle 10x10.gif";
        paintBrushNames[6] = "circle 14x14.gif";
        paintBrushNames[7] = "circle 20x20.gif";

        if (brushesDir.isDirectory()) {
            File[] brushes = brushesDir.listFiles();
            int brushIndex = ViewToolBarBuilder.NUM_BRUSHES_INTERNAL;

            for (int i = 0; i < brushes.length; i++) {

                if (brushes[i].getName().endsWith(".png")) {
                    paintBrushNames[brushIndex] = brushes[i].getName();
                    brushIndex++;
                }
            }
        }

        // build the new combo box of paintbrushes
        paintBox = new JComboBox(intArray);
        paintBox.setFont(MipavUtil.font12);

        String brushName = Preferences.getProperty(Preferences.PREF_LAST_PAINT_BRUSH);

        if (brushName == null) {
            paintBox.setSelectedIndex(2);
        } else {
            int selectedIndex = 2;

            for (int i = 0; i < paintBrushNames.length; i++) {

                if (brushName.endsWith(paintBrushNames[i])) {
                    selectedIndex = i;

                    break;
                }
            }

            paintBox.setSelectedIndex(selectedIndex);
        }

        paintBox.setRenderer(new PaintBoxRenderer());
        paintBox.addItemListener(this);

        Dimension buttonSize = paintCanToggleButton.getPreferredSize();
        buttonSize.setSize(150, buttonSize.getHeight());


        paintBox.setPreferredSize(buttonSize);
        paintBox.setMaximumSize(buttonSize);

        paintToolBar.add(paintBox);


        paintToolBar.add(ViewToolBarBuilder.makeSeparator());

        if (!imageA.isColorImage()) {

            if (imageB == null) {
                setSpinnerValues(imageA.getType());
            } else {
                setSpinnerValues(imageB.getType());
            }
        }

        intensitySpinner = new JSpinner(new SpinnerNumberModel(spinnerDefaultValue, spinnerMin, spinnerMax,
                                                               spinnerStep));
        intensitySpinner.setMaximumSize(new Dimension(56, 24)); // 56 pixlls wide, 24 tall
        intensitySpinner.setPreferredSize(new Dimension(56, 24)); // 56 pixlls wide, 24 tall
        intensitySpinner.addChangeListener(this);
        intensitySpinner.setToolTipText("Paint intensity"); // bug in API prevents this from working

        if (imageA.isColorImage()) {
            intensitySpinner.setEnabled(false);
        }

        paintToolBar.add(intensitySpinner);

        paintToolBar.add(ViewToolBarBuilder.makeSeparator());

        colorPaintButton = toolbarBuilder.buildButton("colorPaint", "Change paint color", "colorpaint");
        colorPaintButton.setBackground(color);

        paintToolBar.add(colorPaintButton);
        paintToolBar.add(toolbarBuilder.buildTextButton("Opacity", "Change opacity of paint", "OpacityPaint"));
        ButtonGroup borderPaintGroup = new ButtonGroup();
        borderPaintButton = toolbarBuilder.buildToggleButton(CustomUIBuilder.PARAM_PAINT_BORDER,borderPaintGroup);
        bogusBorderPaintButton = new JToggleButton();
        borderPaintGroup.add(bogusBorderPaintButton);
        if (Preferences.is(Preferences.PREF_SHOW_PAINT_BORDER)) {
        	borderPaintButton.setSelected(true);
        }else {
        	bogusBorderPaintButton.setSelected(true);
        }
        paintToolBar.add(borderPaintButton);
        paintToolBar.add(ViewToolBarBuilder.makeSeparator());
        paintToolBar.add(toolbarBuilder.buildButton("CommitPaint", "Changes image where painted inside",
                                                    "paintinside"));
        paintToolBar.add(ViewToolBarBuilder.makeSeparator());
        paintToolBar.add(toolbarBuilder.buildButton("CommitPaintExt", "Changes image where painted outside",
                                                    "paintoutside"));
        paintToolBar.add(ViewToolBarBuilder.makeSeparator());
        paintToolBar.add(toolbarBuilder.buildButton("UndoPaint", "Undo last region paint", "undopaint"));
        paintToolBar.add(ViewToolBarBuilder.makeSeparator());
        paintToolBar.add(toolbarBuilder.buildButton("CalcPaint", "Calculate volume of paint", "calc"));

        paintToolBar.setFloatable(false);

        panelToolBarGBC.gridx = 0;
        panelToolBarGBC.gridy = 3;
        panelToolBarGBC.gridwidth = 1;
        panelToolBarGBC.gridheight = 1;
        panelToolBarGBC.fill = GridBagConstraints.BOTH;
        panelToolBarGBC.anchor = GridBagConstraints.WEST;
        panelToolBarGBC.weightx = 100;
        panelToolBarGBC.weighty = 100;
        panelToolbar.add(paintToolBar, panelToolBarGBC);
        paintToolBar.setVisible(menuObj.isMenuItemSelected("Paint toolbar"));

        if ((imageA.getNDims() == 4) || ((imageB != null) && (imageB.getNDims() == 4))) {
            JPanel panelImageSlider = new JPanel();
            panelImageSlider.setLayout(new GridLayout(1, 1));
            panelImageSlider.setForeground(Color.black);
            
            tDim = extents[3];
            TitledBorder borderImageSlider = new TitledBorder(" Time slice index [total number volumes="+ tDim + "] ");
            borderImageSlider.setTitleColor(Color.black);
            borderImageSlider.setTitleFont(MipavUtil.font12B);
            borderImageSlider.setBorder(new EtchedBorder());
            panelImageSlider.setBorder(borderImageSlider);
 
            tImageSlider = new JSlider(JSlider.HORIZONTAL, 0, 100, 50);
            tImageSlider.setMinorTickSpacing(Math.round(100.0f / (tDim - 1)));
            tImageSlider.setSnapToTicks(true);
            tImageSlider.setPaintTicks(true);
            tImageSlider.setPaintLabels(true);
            tImageSlider.setLabelTable(buildTImageSliderLabels(0, tDim));
            tImageSlider.setValue(1);
            tImageSlider.setValue(0);
            tImageSlider.addChangeListener(this);

            panelImageSlider.add(tImageSlider);
            panelToolBarGBC.gridx = 0;
            panelToolBarGBC.gridy = 4;
            panelToolBarGBC.gridwidth = 1;
            panelToolBarGBC.gridheight = 1;
            panelToolBarGBC.fill = GridBagConstraints.BOTH;
            panelToolBarGBC.anchor = GridBagConstraints.WEST;
            panelToolBarGBC.weightx = 100;
            panelToolBarGBC.weighty = 100;
            panelToolbar.add(panelImageSlider, panelToolBarGBC);
        }

        setImageSelectorPanelVisible(true);
    }
    
    protected void buildImageAlignToolBar()
    {
        Border etchedBorder = BorderFactory.createEtchedBorder();
        imageAlignToolBar = new JToolBar();
        imageAlignToolBar.setBorder(etchedBorder);
        imageAlignToolBar.setBorderPainted(true);
        imageAlignToolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        imageAlignToolBar.setFloatable(false);

        ButtonGroup intensityLineGroup = new ButtonGroup();
        //imageAlignToolBar.add(toolbarBuilder.buildToggleButton(CustomUIBuilder.PARAM_VOI_LINE, VOIGroup));
        //intensityLineGroup.add(btnInvisible[0]);

        //imageAlignToolBar.add(ViewToolBarBuilder.makeSeparator());

        ButtonGroup centerGroup = new ButtonGroup();
        imageAlignToolBar.add(toolbarBuilder.buildToggleButton(CustomUIBuilder.PARAM_TRIIMAGE_CENTER, VOIGroup));

        centerGroup.add(btnInvisible[3]);

        imageAlignToolBar.add(toolbarBuilder.buildToggleButton(CustomUIBuilder.PARAM_VOI_PROTRACTOR, VOIGroup));

        intensityLineGroup.add(btnInvisible[2]);

        imageAlignToolBar.add(toolbarBuilder.buildTextButton("Apply", "Applies rotations and translations",
                                                        "createTransformation"));
        imageAlignToolBar.add(ViewToolBarBuilder.makeSeparator());

        addPointToggleButton = toolbarBuilder.buildToggleButton(CustomUIBuilder.PARAM_VOI_POINT_ADD, VOIGroup);
        addPointToggleButton.addItemListener(this);
        imageAlignToolBar.add(addPointToggleButton);

        // imageToolBar.add(toolbarBuilder.buildToggleButton("NewVOI", "Initiate new VOI", "newvoi", VOIGroup));
        imageAlignToolBar.add(toolbarBuilder.buildButton(CustomUIBuilder.PARAM_VOI_POINT_DELETE));

        leastSquaresButton = new JButton(MipavUtil.getIcon("reglsq.gif"));
        leastSquaresButton.addActionListener(this);
        leastSquaresButton.setToolTipText("Apply least squares alignment");
        leastSquaresButton.setActionCommand("leastSquares");
        leastSquaresButton.setBorderPainted(false);
        leastSquaresButton.setRolloverIcon(MipavUtil.getIcon("reglsqroll.gif"));
        leastSquaresButton.setFocusPainted(false);

        if (imageB == null) {
            leastSquaresButton.setEnabled(false);
        }

        imageAlignToolBar.add(leastSquaresButton);

        tpSplineButton = new JButton(MipavUtil.getIcon("regtsp.gif"));
        tpSplineButton.addActionListener(this);
        tpSplineButton.setToolTipText("Apply thin plate spline alignment");
        tpSplineButton.setActionCommand("tpSpline");
        tpSplineButton.setBorderPainted(false);
        tpSplineButton.setRolloverIcon(MipavUtil.getIcon("regtsproll.gif"));
        tpSplineButton.setFocusPainted(false);

        if (imageB == null) {
            tpSplineButton.setEnabled(false);
        }

        imageAlignToolBar.add(tpSplineButton);

        imageAlignToolBar.add(ViewToolBarBuilder.makeSeparator());

        ButtonGroup oneButtonToggleGroup = new ButtonGroup();
        oneButtonToggleGroup.add(btnInvisible[1]);
        imageAlignToolBar.add(toolbarBuilder.buildToggleButton(CustomUIBuilder.PARAM_TRIIMAGE_BOUNDING_BOX, oneButtonToggleGroup));

        imageAlignToolBar.add(toolbarBuilder.buildTextButton("Crop", "Crops image delineated by the bounding cube",
                                                        "cropVolume"));
        

        panelToolBarGBC.gridy++;
        panelToolbar.add( imageAlignToolBar, panelToolBarGBC );
        imageAlignToolBar.setVisible(false);

    }

    /**
     * This method creates an image from the two ModelImage objects and ModelLUT objects passed as parameters.
     *
     * @param   imageA       ModelImage image A
     * @param   lutA         ModelLUT image A's LUT
     * @param   imageB       ModelImage image B
     * @param   lutB         ModelLUT image B's LUT
     * @param   orientation  the desired orientation of the result image
     *
     * @return  ViewJComponentTriImage
     */
    protected ViewJComponentTriImage buildTriImage(ModelImage imageA, ModelLUT lutA, ModelImage imageB, ModelLUT lutB,
                                                   int orientation) {
        ViewJComponentTriImage triImage = new ViewJComponentTriImage(this, imageA, lutA, null, imageB, lutB, null, null,
                                                                     zoom, extents, imageA.getLogMagDisplay(),
                                                                     orientation);

        int[] triExtents = triImage.getExtents();
        float[] tmpResols = new float[3];
        float minResol = 0;

        float[] triResols = triImage.getResolutions();
        minResol = triResols[0];

        if (triResols[1] < minResol) {
            minResol = triResols[1];
        }

        if (triResols[2] < minResol) {
            minResol = triResols[2];
        }

        tmpResols[0] = triResols[0] / minResol;
        tmpResols[1] = triResols[1] / minResol;
        tmpResols[2] = triResols[2] / minResol;

        triImage.setBuffers(triImage.getImageBufferA(), triImage.getImageBufferB(), triImage.getPixBuffer(),
                            new int[triExtents[0] * triExtents[1]]);

        triImage.setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));

        // setResolutions calls set resolutionX and resolutionY in ViewJComponentBase to
        // the correct values for the XY, XZ, and ZY images.
        float axialWidthResFactor = 1.0f;
        float axialHeightResFactor = 1.0f;

        if (imageA.getType() == ModelImage.COMPLEX) {
            triImage.setResolutions(1, 1);
        } else if ((tmpResols[1] > tmpResols[0]) && (tmpResols[1] < (50.0f * tmpResols[0])) && (units[0] == units[1])) {
            axialHeightResFactor = tmpResols[1] / tmpResols[0];
            triImage.setResolutions(1, axialHeightResFactor);
        } else if ((tmpResols[0] > tmpResols[1]) && (tmpResols[0] < (50.0f * tmpResols[1])) && (units[0] == units[1])) {
            axialWidthResFactor = tmpResols[0] / tmpResols[1];
            triImage.setResolutions(axialWidthResFactor, 1);
        } else if ((tmpResols[0] == tmpResols[1])) {
            triImage.setResolutions(tmpResols[0], tmpResols[0]);
        } else {
            triImage.setResolutions(1, 1);
        }

        triImage.addKeyListener(this);

        return triImage;
    }

    /**
     * Builds the volume position panel, which is the panel that sits in the plug-in area of the 2x2 tri-planar layout.
     */
    protected void buildVolumePositionPanel() {
        cleanVolumePositionPanel();
        volumePositionPanel = new JPanel();

        GridBagLayout gbLayout = new GridBagLayout();
        GridBagConstraints gbConstraints = new GridBagConstraints();
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


        gbLayout = new GridBagLayout();
        gbConstraints = new GridBagConstraints();

        /* Panel for switiching between radiological and neurological viewing
         * conventions: */
        JPanel viewPanel = new JPanel(gbLayout);
        viewPanel.setBorder(BorderFactory.createTitledBorder("Viewing Convention"));

        ButtonGroup displayGroup = new ButtonGroup();

        /* radiological radio button: */
        JRadioButton radiologicalView = new JRadioButton();
        radiologicalView.setSelected(true);
        radiologicalView.addActionListener(this);
        radiologicalView.setActionCommand("RadiologicalView");
        gbConstraints.gridx = 0;
        gbConstraints.gridy = 0;
        viewPanel.add(radiologicalView, gbConstraints);
        gbConstraints.gridx++;
        viewPanel.add(new JLabel("Radiological View"), gbConstraints);
        displayGroup.add(radiologicalView);

        /* neurological radio button: */
        JRadioButton neurologicalView = new JRadioButton();
        neurologicalView.setSelected(false);
        neurologicalView.addActionListener(this);
        neurologicalView.setActionCommand("NeurologicalView");
        gbConstraints.gridx = 0;
        gbConstraints.gridy++;
        viewPanel.add(neurologicalView, gbConstraints);
        gbConstraints.gridx++;
        viewPanel.add(new JLabel("Neurological View"), gbConstraints);
        displayGroup.add(neurologicalView);

        tabbedPane = new JTabbedPane();
        super.buildLabelPanel();

        tabbedPane.add("Volume", absolutePanel);
        tabbedPane.add("Scanner", scannerPanel);
        tabbedPane.add("Talairach", talairachPanel);
        tabbedPane.add("View", viewPanel);

        volumePositionPanel.setLayout(new GridLayout(1, 1));
        volumePositionPanel.add(tabbedPane);
    }

    /**
     * Constructs main frame structures for 3 images (image A only) or 9 images (image A and image B). Assumes imageA is
     * not null. Builds the labels for the position frame. Adds the tri-images to this frame's layout.
     */
    protected void configureFrame() {

        if (imageA == null) {
            return;
        }

        JMenuBar menuBar = buildMenu();
        setJMenuBar(menuBar);
        buildToolbars();
        buildImageAlignToolBar();

        setResizable(true);

        buildLabels();

        buildLUTs();

        if (imageB != null) {
            triImage[AXIAL_AB] = buildTriImage(imageA, LUTa, imageB, LUTb, FileInfoBase.AXIAL);
            triImage[AXIAL_AB].addMouseListener(this);
            triImage[AXIAL_AB].setName((new Integer(AXIAL_AB)).toString());
            triImage[CORONAL_AB] = buildTriImage(imageA, LUTa, imageB, LUTb, FileInfoBase.CORONAL);
            triImage[CORONAL_AB].addMouseListener(this);
            triImage[CORONAL_AB].setName((new Integer(CORONAL_AB)).toString());
            triImage[SAGITTAL_AB] = buildTriImage(imageA, LUTa, imageB, LUTb, FileInfoBase.SAGITTAL);
            triImage[SAGITTAL_AB].addMouseListener(this);
            triImage[SAGITTAL_AB].setName((new Integer(SAGITTAL_AB)).toString());
            triImage[AXIAL_B] = buildTriImage(imageB, LUTb, null, null, FileInfoBase.AXIAL);
            triImage[AXIAL_B].addMouseListener(this);
            triImage[AXIAL_B].setName((new Integer(AXIAL_B)).toString());
            triImage[CORONAL_B] = buildTriImage(imageB, LUTb, null, null, FileInfoBase.CORONAL);
            triImage[CORONAL_B].addMouseListener(this);
            triImage[CORONAL_B].setName((new Integer(CORONAL_B)).toString());
            triImage[SAGITTAL_B] = buildTriImage(imageB, LUTb, null, null, FileInfoBase.SAGITTAL);
            triImage[SAGITTAL_B].addMouseListener(this);
            triImage[SAGITTAL_B].setName((new Integer(SAGITTAL_B)).toString());
        }

        triImage[AXIAL_A] = buildTriImage(imageA, LUTa, null, null, FileInfoBase.AXIAL);
        triImage[AXIAL_A].addMouseListener(this);
        triImage[AXIAL_A].setName((new Integer(AXIAL_A)).toString());
        triImage[SAGITTAL_A] = buildTriImage(imageA, LUTa, null, null, FileInfoBase.SAGITTAL);
        triImage[SAGITTAL_A].addMouseListener(this);
        triImage[SAGITTAL_A].setName((new Integer(SAGITTAL_A)).toString());
        triImage[CORONAL_A] = buildTriImage(imageA, LUTa, null, null, FileInfoBase.CORONAL);
        triImage[CORONAL_A].addMouseListener(this);
        triImage[CORONAL_A].setName((new Integer(CORONAL_A)).toString());


        tSlice = 0;

        zoom = getOptimalZoom(DEFAULT_OPTIMAL_ZOOM, DEFAULT_OPTIMAL_ZOOM);

        // these lines of code set up the initial crosshair position - centered in the image and independent of
        // orientation
        for (int i = 0; i < MAX_TRI_IMAGES; i++) {

            if (triImage[i] != null) {
                triImage[i].setZoom(zoom, zoom);
                triImage[i].setDoCenter(false);
            }
        }
        updateLayout();

        /* set the center after the triImage[].setResolutions and
         * triImage[].setZoom calls have been made: */
        setCenter((extents[0] - 1) / 2, (extents[1] - 1) / 2, (extents[2] - 1) / 2);
        setCrop(new Vector3f(extents[0] * 0.25f, extents[1] * 0.25f, extents[2] * 0.25f),
                new Vector3f(extents[0] * 0.75f, extents[1] * 0.75f, extents[2] * 0.75f));


        setTitle();

        // MUST register frame to image models
        imageA.addImageDisplayListener(this);

        if (imageB != null) {
            imageB.addImageDisplayListener(this);
        }

        pack(); // DO NOT REMOVE!!!!!!!!!!!!!!!!!!! weird things happen to the layout in certain circumstances
        MipavUtil.centerOnScreen(this);
        setVisible(true);
        updateImages(true);
        setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
    }

    /**
     * This is an alternative layout arrangement. It is the original layout before the tri-planar and the dual
     * tri-planar were merged into this class.
     */
    protected void doOldLayout() {
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

        if ((triImage[AXIAL_AB] != null) && (triImage[CORONAL_AB] != null) && (triImage[SAGITTAL_AB] != null)) {

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

        if (volumePositionPanel == null) {
            buildVolumePositionPanel();
        }


        JPanel mainPanel = new JPanel(new GridLayout(1, 1, 8, 8));
        JPanel topPanel = new JPanel(new GridLayout(1, 1, 8, 8));
        JPanel bottomPanel = new JPanel(new GridLayout(1, 1, 8, 8));
        JSplitPane splitPane1 = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, scrollPane[axialIndex],
                                               scrollPane[sagittalIndex]);
        splitPane1.setDividerLocation(0.5);
        splitPane1.setResizeWeight(0.5);
        splitPane1.setOneTouchExpandable(true);
        topPanel.add(splitPane1);

        JSplitPane splitPane2 = null;


        if (pluginPanel != null) {

            // mainPanel.add(pluginPanel);
            splitPane2 = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, scrollPane[coronalIndex], pluginPanel);
        } else {

            if (volumePositionPanel != null) {
                splitPane2 = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, scrollPane[coronalIndex], volumePositionPanel);
            }
        }

        splitPane2.setDividerLocation(0.5);
        splitPane2.setResizeWeight(0.5);
        splitPane2.setOneTouchExpandable(true);
        bottomPanel.add(splitPane2);

        JSplitPane splitPane3 = new JSplitPane(JSplitPane.VERTICAL_SPLIT, topPanel, bottomPanel);
        splitPane3.setDividerLocation(0.5);
        splitPane3.setResizeWeight(0.5);
        splitPane3.setOneTouchExpandable(true);

        mainPanel.add(splitPane3);

        getContentPane().removeAll();
        getContentPane().add(panelToolbar, BorderLayout.NORTH);
        getContentPane().add(mainPanel);
        getContentPane().validate();

        setSize(getSize().width, defaultPreferredHeight);
        pack();
    }

    /**
     * DOCUMENT ME!
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        disposeLocal();
        super.finalize();
    }

    /**
     * Tell coordinate change listeners about a coordinate change (after changing the coordinate to image volume space
     * from the tri-planar space).
     *
     * @param  i  the x coordinate
     * @param  j  the y coordinate
     * @param  k  the z coordinate
     */
    protected void fireCoordinateChange(int i, int j, int k) {

        for (Enumeration e = coordinateListeners.elements(); e.hasMoreElements();) {
            ((CoordinateChangeListener) e.nextElement()).coordinateChanged(i, j, k);
        }
    }

    /**
     * Method handles transformations for least squares algorithm in the tri-planar frame.
     */
    protected void handleLeastSquares() {
        final gov.nih.mipav.SwingWorker worker = new gov.nih.mipav.SwingWorker() {
            public Object construct() {
                ViewJProgressBar progressBar = null;

                try {

                    if (imageB == null) {
                        MipavUtil.displayError("Image B must be present to use the least squares algorithm.");

                        return null;
                    }

                    ViewVOIVector imageAVOIs = (ViewVOIVector) imageA.getVOIs();
                    ViewVOIVector imageBVOIs = (ViewVOIVector) imageB.getVOIs();

                    if ((imageAVOIs != null) && (imageBVOIs != null) && (imageAVOIs.size() != imageBVOIs.size())) {
                        MipavUtil.displayError("Number of VOI points must be identical in each image.");

                        return null;
                    }

                    if (imageAVOIs.size() < 3) {
                        MipavUtil.displayError("At least three VOI points must be in place for each image.");

                        return null;
                    }

                    // extract point VOIs for image A
                    Vector pointVOIVector = new Vector();
                    int nVOI = imageAVOIs.size();

                    for (int i = nVOI - 1; i >= 0; i--) {

                        if (imageAVOIs.VOIAt(i).getCurveType() == VOI.POINT) {

                            for (int k = 0; k < imageA.getExtents()[2]; k++) {
                                Vector3f[] voiPoints = imageAVOIs.VOIAt(i).exportPoints(k);

                                for (int j = 0; j < voiPoints.length; j++) {
                                    pointVOIVector.add(voiPoints);
                                }
                            }
                        }
                    }

                    double[][] coordsA = new double[pointVOIVector.size()][3];

                    for (int i = 0; i < pointVOIVector.size(); i++) {
                        Vector3f[] Vector3f = (Vector3f[]) pointVOIVector.elementAt(i);
                        coordsA[i][0] = Vector3f[0].X;
                        coordsA[i][1] = Vector3f[0].Y;
                        coordsA[i][2] = Vector3f[0].Z;
                    }

                    // extract point VOIs for image B
                    pointVOIVector = new Vector();
                    nVOI = imageBVOIs.size();

                    for (int i = nVOI - 1; i >= 0; i--) {

                        if (imageBVOIs.VOIAt(i).getCurveType() == VOI.POINT) {

                            for (int k = 0; k < imageA.getExtents()[2]; k++) {
                                Vector3f[] voiPoints = imageBVOIs.VOIAt(i).exportPoints(k);

                                for (int j = 0; j < voiPoints.length; j++) {
                                    pointVOIVector.add(voiPoints);
                                }
                            }
                        }
                    }

                    double[][] coordsB = new double[pointVOIVector.size()][3];

                    for (int i = 0; i < pointVOIVector.size(); i++) {
                        Vector3f[] Vector3f = (Vector3f[]) pointVOIVector.elementAt(i);
                        coordsB[i][0] = Vector3f[0].X;
                        coordsB[i][1] = Vector3f[0].Y;
                        coordsB[i][2] = Vector3f[0].Z;
                    }

                    // now that we have the point coords, we can build the least squares algorithm
                    AlgorithmRegLeastSquares algoLeastSquares = new AlgorithmRegLeastSquares(coordsA, coordsB, 3);
                    algoLeastSquares.runAlgorithm();

                    TransMatrix transMatrix = algoLeastSquares.getTransformBtoA();

                    if (algoLeastSquares.isCompleted() == false) {

                        // algorithm failed
                        return null;
                    }

                    progressBar = new ViewJProgressBar("Applying transformation", "Transforming", 0, 100, false, null,
                                                       null);
                    progressBar.setVisible(true);
                    progressBar.setSeparateThread(true);

                    ModelImage resultImage = (ModelImage) imageB.clone();

                    AlgorithmTransform.transformTrilinear(imageB, resultImage, transMatrix, progressBar);

                    imageB.disposeLocal();

                    parentFrame.setImageB(resultImage);
                    parentFrame.setActiveImage(ViewJComponentBase.IMAGE_A);

                    JDialogImageInfo infoDialogB = parentFrame.getImageInfoDialogB();
                    infoDialogB.setMatrix(transMatrix);

                    updateImages();

                    return null;
                } finally {

                    if (progressBar != null) {
                        progressBar.dispose();
                    }
                }
            }
        };

        worker.start();
    }

    /**
     * This method will load a plug-in into the plug-in area of the tri-planar frame. The plug-in must extend
     * java.awt.Component, and take this class as the only parameter to its constructor.
     */
    protected void handlePluginPanelSelection() {

        if (oldLayout == false) {
            int response = JOptionPane.showConfirmDialog(this,
                                                         "You must be using the 2x2 layout to enable this feature. Change to this layout?",
                                                         "Change layout?", JOptionPane.YES_NO_OPTION);

            if (response == JOptionPane.YES_OPTION) {
                oldLayout = true;
                updateLayout();
            } else {
                return;
            }
        }

        JFileChooser fileChooser = new JFileChooser();
        fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        fileChooser.setMultiSelectionEnabled(false);

        int response = fileChooser.showOpenDialog(this);

        if (response == JFileChooser.APPROVE_OPTION) {
            File selectedFile = fileChooser.getSelectedFile();

            try {
                String filename = selectedFile.getName();
                filename = filename.substring(0, filename.lastIndexOf('.'));

                Class pluginClass = Class.forName(filename);

                Constructor pluginConstructor = pluginClass.getConstructor(new Class[] { getClass() });
                pluginPanel = (Component) pluginConstructor.newInstance(new Object[] { this });
                Preferences.debug("Tri-planar plug-in loaded OK", Preferences.DEBUG_MINOR);
            } catch (NoSuchMethodException nsme) {
                MipavUtil.displayError("Class loading failed! The plug-in class must take ViewJFrameTriImage as an argument to its constructor.");

                return;
            } catch (ClassCastException cce) {
                MipavUtil.displayError("Class loading failed! The plug-in class must extend java.awt.Component.");

                return;
            } catch (Exception e) {
                MipavUtil.displayError("Class loading failed!");

                return;
            }
        } else {
            return;
        }

        updateLayout();

        return;
    }

    /**
     * DOCUMENT ME!
     */
    protected void handleTPSpline() {

        if (imageB == null) {
            MipavUtil.displayError("Image B must be present to use the least squares algorithm.");

            return;
        }

        ViewVOIVector imageAVOIs = (ViewVOIVector) imageA.getVOIs();
        ViewVOIVector imageBVOIs = (ViewVOIVector) imageB.getVOIs();

        if ((imageAVOIs != null) && (imageBVOIs != null) && (imageAVOIs.size() != imageBVOIs.size())) {
            MipavUtil.displayError("Number of VOI points must be identical in each image.");

            return;
        }

        if (imageAVOIs.size() < 3) {
            MipavUtil.displayError("At least three points must be in place for each image.");

            return;
        }

        double[] xSourceA = new double[imageAVOIs.size()];
        double[] ySourceA = new double[imageAVOIs.size()];
        double[] zSourceA = new double[imageAVOIs.size()];
        double[] xTargetB = new double[imageBVOIs.size()];
        double[] yTargetB = new double[imageBVOIs.size()];
        double[] zTargetB = new double[imageBVOIs.size()];

        // extract point VOIs for image A
        Vector pointVOIVector = new Vector();
        int nVOI = imageAVOIs.size();

        for (int i = nVOI - 1; i >= 0; i--) {

            if (imageAVOIs.VOIAt(i).getCurveType() == VOI.POINT) {

                for (int k = 0; k < imageA.getExtents()[2]; k++) {
                    Vector3f[] voiPoints = imageAVOIs.VOIAt(i).exportPoints(k);

                    for (int j = 0; j < voiPoints.length; j++) {
                        pointVOIVector.add(voiPoints);
                    }
                }
            }
        }

        for (int i = 0; i < pointVOIVector.size(); i++) {
            Vector3f[] Vector3f = (Vector3f[]) pointVOIVector.elementAt(i);
            xSourceA[i] = Vector3f[0].X;
            ySourceA[i] = Vector3f[0].Y;
            zSourceA[i] = Vector3f[0].Z;
        }

        // extract point VOIs for image B
        pointVOIVector = new Vector();
        nVOI = imageBVOIs.size();

        for (int i = nVOI - 1; i >= 0; i--) {

            if (imageBVOIs.VOIAt(i).getCurveType() == VOI.POINT) {

                for (int k = 0; k < imageA.getExtents()[2]; k++) {
                    Vector3f[] voiPoints = imageBVOIs.VOIAt(i).exportPoints(k);

                    for (int j = 0; j < voiPoints.length; j++) {
                        pointVOIVector.add(voiPoints);
                    }
                }
            }
        }

        for (int i = 0; i < pointVOIVector.size(); i++) {
            Vector3f[] Vector3f = (Vector3f[]) pointVOIVector.elementAt(i);
            xTargetB[i] = Vector3f[0].X;
            yTargetB[i] = Vector3f[0].Y;
            zTargetB[i] = Vector3f[0].Z;
        }

        parentFrame.setActiveImage(ViewJComponentBase.IMAGE_A);

        ModelImage clonedImage = (ModelImage) imageB.clone();

        AlgorithmTPSpline algoTPSpline = new AlgorithmTPSpline(xSourceA, ySourceA, zSourceA, xTargetB, yTargetB,
                                                               zTargetB, 0.0f, imageB, clonedImage);
        algoTPSpline.setRunningInSeparateThread(false);
        algoTPSpline.setupTPSpline2D(xSourceA, ySourceA, xTargetB, yTargetB, 0.0f);
        algoTPSpline.runAlgorithm();

        if (imageB != null) {
            imageB.disposeLocal();
            imageB = null;
        }

        if (clonedImage != null) {
            clonedImage.disposeLocal();
            clonedImage = null;
        }

        ModelImage newImageB = algoTPSpline.getResultImage();

        parentFrame.setImageB(newImageB);

        return;
    }

    /**
     * Initialize the the image orientations and call the frame layout method 'configureFrame()'.
     */
    protected void init() {
        addKeyListener(this);

        if (controls != null) {
            OPACITY = controls.getTools().getOpacity();
            color = controls.getTools().getPaintColor();
        }

        imageA.setImageOrder(ModelImage.IMAGE_A);

        if (imageB != null) {
            imageB.setImageOrder(ModelImage.IMAGE_B);
        }

        if (((imageA.getNDims() == 3) && (imageB == null)) ||
                ((imageA.getNDims() == 3) && (imageB != null) && (imageB.getNDims() == 3))) {
            extents = new int[3];
            extents[0] = imageA.getExtents()[0];
            extents[1] = imageA.getExtents()[1];
            extents[2] = imageA.getExtents()[2];
        } else { // imageA.getNDims() == 4 or imageB.getNDims() == 4
            extents = new int[4];
            extents[0] = imageA.getExtents()[0];
            extents[1] = imageA.getExtents()[1];
            extents[2] = imageA.getExtents()[2];

            if (imageA.getNDims() == 4) {
                extents[3] = imageA.getExtents()[3];
            } else {
                extents[3] = imageB.getExtents()[3];
            }
        }

        bufferFactor = 1;

        if (imageA.isColorImage()) {
            bufferFactor = 4;
        }

        resols[0] = Math.abs(imageA.getFileInfo()[0].getResolutions()[0]);
        resols[1] = Math.abs(imageA.getFileInfo()[0].getResolutions()[1]);
        resols[2] = Math.abs(imageA.getFileInfo()[0].getResolutions()[2]);

        if ((resols[0] == 0.0f) || (resols[1] == 0.0f) || (resols[2] == 0.0f)) {
            resols[0] = 1.0f;
            resols[1] = 1.0f;
            resols[2] = 1.0f;
        }

        units[0] = imageA.getFileInfo()[0].getUnitsOfMeasure()[0];
        units[1] = imageA.getFileInfo()[0].getUnitsOfMeasure()[1];
        units[2] = imageA.getFileInfo()[0].getUnitsOfMeasure()[2];

        // Talaiarach images must already be in standard dicom order
        if ((imageA.getExtents()[2] * resols[2]) > (ATLAS_BBOX_INF + ATLAS_BBOX_SUP + 6)) {

            // Use ATLAS_BBOX_INF_NEW = 65.0f;
            useInfNew = true;
        } else {

            // Use ATLAS_BBOX_INF = 55.0f;
            useInfNew = false;
        }

        configureFrame();
    }

    /**
     * Initializes the progress bar. Sets the location relative the middle the screen and makes it visible.
     */
    protected void initProgressBar() {

        if (progressBar != null) {
            int xScreen = Toolkit.getDefaultToolkit().getScreenSize().width;
            progressBar.setLocation(xScreen / 2, 50);
            progressBar.setVisible(true);
        }
    }

    /**
     * Helper method to establish if there are images of the same dimensionality so that a dialog can be created. Used
     * by image calculator, concat, etc.
     *
     * @return  <code>true</code> if there are images to operate on.
     */
    protected boolean isMultipleSameSizeTriImages() {
        Enumeration registeredImageNames = userInterface.getRegisteredImageNames();
        boolean createDialog = false;
        String activeImageName;
        ModelImage activeImage;

        if (imageB == null) {
            activeImage = triImage[AXIAL_A].getImageA();
        } else {
            activeImage = triImage[AXIAL_AB].getActiveImage();
        }

        activeImageName = activeImage.getImageName();

        // Add images from user interface that have the same exact dimensionality
        while (registeredImageNames.hasMoreElements()) {
            String registeredImageName = (String) registeredImageNames.nextElement();

            if (!activeImageName.equals(registeredImageName)) {

                try {
                    ModelImage img = userInterface.getRegisteredImageByName(registeredImageName);

                    if (img.getTriImageFrame() != null) {

                        if (activeImage.getNDims() == img.getNDims()) {
                            int counter = 0;

                            for (int j = 0; j < activeImage.getNDims(); j++) {

                                if (activeImage.getExtents()[j] == img.getExtents()[j]) {
                                    counter++;
                                }
                            }

                            if (counter == activeImage.getNDims()) {
                                createDialog = true;
                            }
                        }
                    }
                } catch (IllegalArgumentException iae) {

                    // MipavUtil.displayError("There was a problem with the supplied name.\n" );
                    Preferences.debug("Illegal Argument Exception in " +
                                      "ViewJFrameTriImage.isMultipleSameSizeTriImages(). " +
                                      "Somehow the Image list sent an incorrect name to " +
                                      "the image image hashtable. " + "\n", 1);
                }
            }
        }

        return createDialog;
    }

    /**
     * Makes a separator for the use in the toolbars.
     *
     * @return  Separator for the toolbar.
     */
    protected JButton makeSeparator() {
        JButton separator = new JButton(MipavUtil.getIcon("separator.gif"));
        separator.setBorderPainted(false);
        separator.setFocusPainted(false);

        return (separator);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  activeImage  DOCUMENT ME!
     */
    protected void setImageActiveInTriComponents(int activeImage) {

        for (int i = 0; i < MAX_TRI_IMAGES; i++) {

            if (triImage[i] != null) {
                triImage[i].setActiveImage(ViewJComponentBase.IMAGE_A);
                if ( voiManager != null )
                {
                    voiManager.getVOIManager(i).setActiveImage(activeImage);
                }
            }
        }
        
        if (activeImage == ViewJComponentBase.IMAGE_B) {

            if (triImage[AXIAL_AB] != null) {
                triImage[AXIAL_AB].setActiveImage(ViewJComponentBase.IMAGE_B);
            }

            if (triImage[SAGITTAL_AB] != null) {
                triImage[SAGITTAL_AB].setActiveImage(ViewJComponentBase.IMAGE_B);
            }

            if (triImage[CORONAL_AB] != null) {
                triImage[CORONAL_AB].setActiveImage(ViewJComponentBase.IMAGE_B);
            }

        }
    }

    /**
     * Sets the labels that refer to Talairach position within the image.
     *
     * @param  x  Absolute x value in slice.
     * @param  y  Absolute y value in slice.
     * @param  z  Absolute z value in slice.
     */
    protected void setTalairachPositionLabels(int x, int y, int z) {
        float xTal, yTal, zTal;
        Vector3f pt;
        DecimalFormat nf = new DecimalFormat("#####0.0##");
        String strX = "";
        String strY = "";
        String strZ = "";

        try {
            TalairachTransformInfo tInfo = imageA.getTalairachTransformInfo();

            if (tInfo != null) {
                pt = tInfo.getTlrcAC();
            } else {
                tInfo.setAcpcRes(imageA.getResolutions(0)[0]);
                pt = tInfo.getTlrcAC();
            }

            xTal = x - pt.X;
            yTal = y - pt.Y;
            zTal = z - pt.Z;
        } catch (Exception ex) {
            xTal = (x * imageA.getResolutions(0)[0]) - ATLAS_BBOX_LAT;
            yTal = (y * imageA.getResolutions(0)[1]) - ATLAS_BBOX_ANT;

            if (useInfNew) {
                zTal = (z * imageA.getResolutions(0)[2]) - ATLAS_BBOX_INF_NEW;
            } else {
                zTal = (z * imageA.getResolutions(0)[2]) - ATLAS_BBOX_INF;
            }
        }

        strX = String.valueOf(nf.format(xTal));
        strY = String.valueOf(nf.format(yTal));
        strZ = String.valueOf(nf.format(zTal));

        int[] triExtents = triImage[AXIAL_A].getExtents();

        if ((x >= 0) && (x < triExtents[0])) {
            labelXTal.setText(strX);
        }

        if ((y >= 0) && (y < triExtents[1])) {
            labelYTal.setText(strY);
        }

        if ((z >= 0) && (z < triExtents[2])) {
            labelZTal.setText(strZ);
        }
    }

    /**
     * Sets time slider slice.
     *
     * @param  tSlice  Slice to set to.
     */
    protected void setTSlider(int tSlice) {
        int newValue;

        if (tImageSlider == null) {
            return;
        }

        newValue = Math.round((100.0f * tSlice / (extents[3] - 1)) - 0.01f);
        tImageSlider.removeChangeListener(this);
        tImageSlider.setValue(newValue);
        tImageSlider.addChangeListener(this);
    }

    /**
     * Displays histoLUT frame.
     *
     * @param  imageAorB  ViewJComponentBase.IMAGE_A or ViewJComponentBase.IMAGE_B
     */
    protected void updateHistoLUTFrame(int imageAorB) {
        updateImages(true);

        if ((imageA.getHistoLUTFrame() != null) && (imageAorB == ViewJComponentBase.IMAGE_A) &&
                (triImage[AXIAL_A] != null)) {
            imageA.getHistoLUTFrame().updateHistoLUT(imageA, triImage[AXIAL_A].getLUTa(), null, null, true);
        } else if ((imageA.getHistoLUTFrame() != null) && (imageAorB == ViewJComponentBase.IMAGE_B) &&
                       (triImage[AXIAL_B] != null)) {
            imageA.getHistoLUTFrame().updateHistoLUT(null, null, imageB, triImage[AXIAL_B].getLUTb(), true);
        } else if ((imageA.getHistoLUTFrame() != null) && (imageAorB == IMAGE_A_B) && (triImage[AXIAL_AB] != null)) {
            imageA.getHistoLUTFrame().updateHistoLUT(imageA, triImage[AXIAL_AB].getLUTa(), imageB,
                                                     triImage[AXIAL_AB].getLUTb(), true);
        }
    }


    /**
     * This method should be called whenever the layout of the tri-images has changed. For example, when image B is
     * removed or added, this method should be called so that the frame can resize itself and properly layout the
     * tri-image components.
     */
    protected void updateLayout() {

        if (oldLayout) {

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

        if (imageB != null) {
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

        scrollPane[AXIAL_A].setName("axialA");
        scrollPane[SAGITTAL_A] = new JScrollPane(triImagePanel[SAGITTAL_A], JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                                 JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollPane[CORONAL_A] = new JScrollPane(triImagePanel[CORONAL_A], JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        getContentPane().removeAll();
        getContentPane().add(panelToolbar, BorderLayout.NORTH);

        JPanel panelA = new JPanel(new GridLayout(1, 3, 10, 10));

        if (imageB == null) {
            JSplitPane splitPane1 = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, scrollPane[AXIAL_A],
                                                   scrollPane[SAGITTAL_A]);
            splitPane1.setDividerLocation(0.5);
            splitPane1.setResizeWeight(0.5);
            splitPane1.setOneTouchExpandable(true);

            JSplitPane splitPane2 = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, splitPane1, scrollPane[CORONAL_A]);
            splitPane2.setDividerLocation(0.67);
            splitPane2.setResizeWeight(0.5);
            splitPane2.setOneTouchExpandable(true);
            panelA.add(splitPane2);
            getContentPane().add(panelA, BorderLayout.CENTER);
            pack();
        } else {
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


            splitPane1.setOneTouchExpandable(true);
            splitPane2.setOneTouchExpandable(true);
            splitPane1.setDividerLocation(0.5);
            splitPane2.setDividerLocation(0.67);
            splitPane1.setResizeWeight(0.5);
            splitPane2.setResizeWeight(0.5);

            getContentPane().add(splitPane2, BorderLayout.CENTER);
            setSize(getSize().width, defaultPreferredHeight);
            pack();
        }
    }

    /**
     * The purpose of this method is to set the paint bitmap so that it is the same for all 9 images. The reason it is
     * here is because I couldn't find a way to automatically set the paint bitmap without calling this method. There
     * was a problem because if you were painting in image A, image A/B would also get painted, but not image B. Paint
     * in image A/B resulted in image A also being painted but not image B. Painting in image B resulting in image B
     * being the only image affected. I thought this was weird so I created this method so that all images were painted
     * equally. -- lorsino
     *
     * @param  paintMap  BitSet the paint bitset object to set the images to
     */
    protected void updatePaint(BitSet paintMap) {

        for (int i = 0; i < MAX_TRI_IMAGES; i++) {

            if (triImage[i] != null) {
                triImage[i].setPaintMask(paintMap);
            }
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  x           DOCUMENT ME!
     * @param  y           DOCUMENT ME!
     * @param  scrollPane  DOCUMENT ME!
     */
    private void adjustScrollbars(final int x, final int y, final JScrollPane scrollPane) {

        if (scrollPane == null) {
            return;
        }

        JViewport viewport = scrollPane.getViewport();

        if (viewport == null) {
            return;
        }

        Dimension extentSize = viewport.getExtentSize();

        if (extentSize == null) {
            return;
        }

        final int scrollPaneX = extentSize.width / 2;
        final int scrollPaneY = extentSize.height / 2;

        Runnable adjustScrollbarsAWTEvent = new Runnable() {
            public void run() {
                scrollPane.getHorizontalScrollBar().setValue(x - scrollPaneX);
                scrollPane.getVerticalScrollBar().setValue(y - scrollPaneY);
            }
        };
        SwingUtilities.invokeLater(adjustScrollbarsAWTEvent);
    }


    /**
     * This method adjust the scrollbars to area where user clicked when doing individual frame zooming in and out.
     *
     * @param  frame  DOCUMENT ME!
     * @param  x      int
     * @param  y      int
     */
    private void adjustScrollbars(int frame, int x, int y) {
        final int theFrame = frame;

        if (triImage[frame] != null) {
            final int xTemp = (int) (x * (triImage[frame].getZoomX() * triImage[frame].getResolutionX()));
            final int yTemp = (int) (y * (triImage[frame].getZoomY() * triImage[frame].getResolutionY()));

            final int scrollPaneX = scrollPane[theFrame].getWidth() / 2;
            final int scrollPaneY = scrollPane[theFrame].getHeight() / 2;

            Runnable adjustScrollbarsAWTEvent = new Runnable() {
                public void run() {
                    scrollPane[theFrame].getHorizontalScrollBar().setValue(xTemp - scrollPaneX);
                    scrollPane[theFrame].getVerticalScrollBar().setValue(yTemp - scrollPaneY);
                }
            };
            SwingUtilities.invokeLater(adjustScrollbarsAWTEvent);
        }
    }

    /**
     * Convenience method created to simplify configureFrame().
     */
    private void buildLabels() {
        // talLabel = new JLabel( "Talairach grid coordinates" );
        // talLabel.setForeground( Color.black );
        // talLabel.setFont( MipavUtil.font12B );
        // talLabel.setEnabled( showTalairachPosition );

        labelXTal = new JTextField("", 5);
        labelXTal.setForeground(Color.black);
        labelXTal.setFont(MipavUtil.font12B);
        labelXTal.setEnabled(showTalairachPosition);
        labelXTal.setBorder(BorderFactory.createEmptyBorder());
        labelXTal.setEditable(false);

        labelYTal = new JTextField("", 5);
        labelYTal.setForeground(Color.black);
        labelYTal.setFont(MipavUtil.font12B);
        labelYTal.setEnabled(showTalairachPosition);
        labelYTal.setBorder(BorderFactory.createEmptyBorder());
        labelYTal.setEditable(false);

        labelZTal = new JTextField("", 5);
        labelZTal.setForeground(Color.black);
        labelZTal.setFont(MipavUtil.font12B);
        labelZTal.setEnabled(showTalairachPosition);
        labelZTal.setBorder(BorderFactory.createEmptyBorder());
        labelZTal.setEditable(false);

        talairachVoxelLabel = new JLabel("     "); // must be initialized to 5 empty spaces
    }

    /**
     * Convenience method created to simplify configureFrame().
     */
    private void buildLUTs() {

        // if not a color image and LUTa is null then make a LUT
        if (imageA.isColorImage() == false) {
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

            if ((imageB != null) && (LUTb == null)) {
                LUTb = new ModelLUT(ModelLUT.HOTMETAL, 256, dimExtentsLUT);

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
        }
    }

    /**
     * Clears the volumePositionPanel, so it can be recreated when the volumePositionFrame is closed:.
     */
    private void cleanVolumePositionPanel() {

        if (volumePositionPanel != null) {
            volumePositionPanel.removeAll();
            chkShowTalairachGrid = null;
            chkShowTalairachGridMarkers = null;
            tabbedPane.removeAll();
            tabbedPane = null;
            volumePositionPanel = null;
        }
    }

    /**
     * Calculates the optimal zoom value for an image based on the two parameters desiredWidth and desiredHeight. Since
     * MIPAV doesn't officially support images with differing zoom values, the zoom value returned will be that value
     * which, when applied to the image, will ensure the image size on screen is no more than both desiredWidth or
     * desiredHeight
     *
     * @param   desiredWidth   int the ideal width for the image displayed on-screen
     * @param   desiredHeight  int the idea height for the image displayed on-screen
     *
     * @return  float the zoom value required to show the image on-screen at no more than desiredWidth and no more than
     *          desired height
     */
    private float getOptimalZoom(int desiredWidth, int desiredHeight) {
        int[] extentsAxial = triImage[AXIAL_A].getExtents();
        int[] extentsSagittal = triImage[SAGITTAL_A].getExtents();
        int[] extentsCoronal = triImage[CORONAL_A].getExtents();

        float zoomAxialWidth = (desiredWidth * triImage[AXIAL_A].getResolutionX()) / (extentsAxial[0] * triImage[AXIAL_A].getResolutionX());
        float zoomSagittalWidth = (desiredWidth * triImage[SAGITTAL_A].getResolutionX()) / (extentsSagittal[0] * triImage[SAGITTAL_A].getResolutionX());
        float zoomCoronalWidth = (desiredWidth * triImage[CORONAL_A].getResolutionX()) / (extentsCoronal[0] * triImage[CORONAL_A].getResolutionX());

        float optimalZoomWidth = Math.min(zoomAxialWidth, Math.min(zoomSagittalWidth, zoomCoronalWidth));

        float zoomAxialHeight = (desiredHeight * triImage[AXIAL_A].getResolutionY()) / (extentsAxial[1] * triImage[AXIAL_A].getResolutionY());
        float zoomSagittalHeight = (desiredHeight * triImage[SAGITTAL_A].getResolutionY())/ (extentsSagittal[1] * triImage[SAGITTAL_A].getResolutionY());
        float zoomCoronal = (desiredHeight * triImage[CORONAL_A].getResolutionY())/ (extentsCoronal[1] * triImage[CORONAL_A].getResolutionY());

        float optimalZoomHeight = Math.min(zoomAxialHeight, Math.min(zoomSagittalHeight, zoomCoronal));

        return Math.min(optimalZoomWidth, optimalZoomHeight);
    }

    /**
     * Transition between the 2x2 window layout and 3x1 window layout:
     *
     * @param  bLayout  the new layout, = 2x2 layout when true and 3x1 when false
     */
    private void setOldLayout(boolean bLayout) {
        oldLayout = bLayout;
        Preferences.setProperty(Preferences.PREF_TRIPLANAR_2X2_LAYOUT, String.valueOf(oldLayout));

        float optimalZoom = getOptimalZoom(DEFAULT_OPTIMAL_ZOOM, DEFAULT_OPTIMAL_ZOOM);

        for (int i = 0; i < MAX_TRI_IMAGES; i++) {

            if (triImage[i] != null) {
                triImage[i].setZoom(optimalZoom, optimalZoom);
            }
        }

        updateLayout();
    }

    /**
     * Sets the spinner values based on image type.
     *
     * @param  type  Image type (BYTE, FLOAT, ...)
     */
    private void setSpinnerValues(int type) {
        spinnerDefaultValue = 1.0;

        if (type == ModelStorageBase.BOOLEAN) {
            spinnerMin = 0;
            spinnerMax = 1;
        } else if (type == ModelStorageBase.BYTE) {
            spinnerMin = -128;
            spinnerMax = 127;
            spinnerStep = 1;
        } else if (type == ModelStorageBase.UBYTE) {
            spinnerMin = 0;
            spinnerMax = 255;
            spinnerStep = 1;
        } else if (type == ModelStorageBase.SHORT) {
            spinnerMin = -32768;
            spinnerMax = 32767;
            spinnerStep = 1;
        } else if (type == ModelStorageBase.USHORT) {
            spinnerMin = 0;
            spinnerMax = 65535;
            spinnerStep = 1;
        } else if (type == ModelStorageBase.INTEGER) {
            spinnerMin = Integer.MIN_VALUE;
            spinnerMax = Integer.MAX_VALUE;
            spinnerStep = 1;
        } else if (type == ModelStorageBase.UINTEGER) {
            spinnerMin = 0;
            spinnerMax = 4294967295L;
            spinnerStep = 1;
        } else if (type == ModelStorageBase.LONG) {
            spinnerMin = Long.MIN_VALUE;
            spinnerMax = Long.MAX_VALUE;
            spinnerStep = 1;
        } else if (type == ModelStorageBase.FLOAT) {
            spinnerMin = -Float.MAX_VALUE;
            spinnerMax = Float.MAX_VALUE;
            spinnerStep = 0.1;
        } else if (type == ModelStorageBase.DOUBLE) {
            spinnerMin = -Double.MAX_VALUE;
            spinnerMax = Double.MAX_VALUE;
            spinnerStep = 1;
        }
    }

    /**
     * Sets the CubeBounds data member volumeBounds to the crop volume defined by the lower and upper volume points. The
     * CubeBounds are ordered low to high and are checked against the volume extents.
     *
     * @param  lower  the lower bound in File Coordinates
     * @param  upper  the upper bound in File Coordinates
     */
    private void setVolumeBounds(Vector3f lower, Vector3f upper) {
        int[] xBounds = new int[2];
        xBounds[0] = ((int) lower.X <= (int) upper.X) ? (int) lower.X : (int) upper.X;
        xBounds[1] = ((int) lower.X <= (int) upper.X) ? (int) upper.X : (int) lower.X;
        xBounds[0] = Math.max(xBounds[0], 0);
        xBounds[1] = Math.min(xBounds[1], extents[0] - 1);

        int[] yBounds = new int[2];
        yBounds[0] = ((int) lower.Y <= (int) upper.Y) ? (int) lower.Y : (int) upper.Y;
        yBounds[1] = ((int) lower.Y <= (int) upper.Y) ? (int) upper.Y : (int) lower.Y;
        yBounds[0] = Math.max(yBounds[0], 0);
        yBounds[1] = Math.min(yBounds[1], extents[1] - 1);

        int[] zBounds = new int[2];
        zBounds[0] = ((int) lower.Z <= (int) upper.Z) ? (int) lower.Z : (int) upper.Z;
        zBounds[1] = ((int) lower.Z <= (int) upper.Z) ? (int) upper.Z : (int) lower.Z;
        zBounds[0] = Math.max(zBounds[0], 0);
        zBounds[1] = Math.min(zBounds[1], extents[2] - 1);

        volumeBounds = new CubeBounds(xBounds[1], xBounds[0], yBounds[1], yBounds[0], zBounds[1], zBounds[0]);
    }


    /**
     * this method will zoom in a particular frame.
     *
     * @param  frame  frame the frame number
     */
    private void zoomInFrame(int frame) {
        float oldZoom = triImage[frame].getZoomX();

        float newZoom = 1;

        if ((Preferences.is(Preferences.PREF_ZOOM_LINEAR)) && (triImage[frame] != null)) {
        	if(triImage[frame].getZoomX() < 1.0f) {
        		newZoom = 2.0f * triImage[frame].getZoomX();
        	} else {
        		newZoom = triImage[frame].getZoomX() + 1.0f;
        	}
        } else if (triImage[frame] != null) // zoomMode == ViewJComponentEditImage.EXPONENTIAL
        {
        	newZoom = 2.0f * triImage[frame].getZoomX();

        }

        if (triImage[frame] != null) {
            triImage[frame].setZoom(newZoom, newZoom);

            Vector2f oldCrosshairPoint = triImage[frame].getCrosshairPoint();

            if (oldCrosshairPoint != null) {
                int newX = MipavMath.round((oldCrosshairPoint.X * newZoom) / oldZoom);
                int newY = MipavMath.round((oldCrosshairPoint.Y * newZoom) / oldZoom);

                triImage[frame].updateCrosshairPosition(newX, newY);

                adjustScrollbars(newX, newY, scrollPane[frame]);
            }
        }

        validate();
        updateImages(true);

    }


    /**
     * this method will zoom out a particular frame.
     *
     * @param  frame  frame the frame number
     */
    private void zoomOutFrame(int frame) {
        float oldZoom = triImage[frame].getZoomX();

        float newZoom = 1;

        if ((Preferences.is(Preferences.PREF_ZOOM_LINEAR)) && (triImage[frame].getZoomX() > 1.0f)) {

            // linear zoom is prevented if getZoomX() <= 1.0
            newZoom = triImage[frame].getZoomX() - 1.0f;
        } else {
            newZoom = 0.5f * triImage[frame].getZoomX();
        }

        if (triImage[frame] != null) {
            triImage[frame].setZoom(newZoom, newZoom);

            Vector2f oldCrosshairPoint = triImage[frame].getCrosshairPoint();

            if (oldCrosshairPoint != null) {
                int newX = MipavMath.round((oldCrosshairPoint.X * newZoom) / oldZoom);
                int newY = MipavMath.round((oldCrosshairPoint.Y * newZoom) / oldZoom);

                triImage[frame].updateCrosshairPosition(newX, newY);

                adjustScrollbars(newX, newY, scrollPane[frame]);
            }
        }

        validate();
        updateImages(true);
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Listener to pass to JColorChooser when user wants to change the color of the paint.
     */
    class OkColorListener implements ActionListener {

        /**
         * Pick up the selected color and call method to change the color.
         *
         * @param  e  Event that triggered this function.
         */
        public void actionPerformed(ActionEvent e) {

            if (colorChooser != null) {
                color = colorChooser.getColor();
            }

            colorPaintButton.setBackground(color);

            if (controls != null) {
                controls.getTools().setPaintColor(color);
            }

            for (int i = 0; i < MAX_TRI_IMAGES; i++) {

                if (triImage[i] != null) {
                    triImage[i].getActiveImage().notifyImageDisplayListeners(null, true);
                }
            }
        }
    }

    /**
     * DOCUMENT ME!
     */
    class PaintBoxRenderer extends JLabel implements ListCellRenderer {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -965184394601656795L;

        /**
         * Creates a new ComboBoxRenderer2 object.
         */
        public PaintBoxRenderer() {
            setOpaque(true);
            setHorizontalAlignment(LEFT);
            setVerticalAlignment(CENTER);
        }

        /**
         * This method finds the image and text corresponding to the selected value and returns the label, set up to
         * display the text and image.
         *
         * @param   list          DOCUMENT ME!
         * @param   value         DOCUMENT ME!
         * @param   index         DOCUMENT ME!
         * @param   isSelected    DOCUMENT ME!
         * @param   cellHasFocus  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected,
                                                      boolean cellHasFocus) {

            // Get the selected index. (The index param isn't
            // always valid, so just use the value.)
            int selectedIndex = ((Integer) value).intValue();

            if (isSelected) {
                setBackground(list.getSelectionBackground());
                setForeground(list.getSelectionForeground());
            } else {
                setBackground(list.getBackground());
                setForeground(list.getForeground());
            }


            // Set the icon and text.  If icon was null, say so.
            ImageIcon icon = null;

            setFont(MipavUtil.font12);

            if (selectedIndex < ViewToolBarBuilder.NUM_BRUSHES_INTERNAL) {
                icon = MipavUtil.getIcon(paintBrushNames[selectedIndex]);

            } else {
                URL res = null;

                try {
                    res = new File(ViewToolBarBuilder.USER_BRUSHES + File.separator + paintBrushNames[selectedIndex]).toURI().toURL();
                    icon = new ImageIcon(res);

                    if ((icon.getIconHeight() >= 20) || (icon.getIconWidth() >= 20)) {
                        int newWidth, newHeight;

                        if (icon.getIconHeight() < icon.getIconWidth()) {
                            newWidth = 20;

                            float factor = 24f / icon.getIconWidth();
                            newHeight = (int) (icon.getIconHeight() * factor);
                        } else {
                            newHeight = 20;

                            float factor = 24f / icon.getIconHeight();
                            newWidth = (int) (icon.getIconWidth() * factor);
                        }

                        icon = new ImageIcon(icon.getImage().getScaledInstance(newWidth, newHeight, 0));
                    }
                } catch (Exception e) {
                    // e.printStackTrace();
                }
            }

            setText(paintBrushNames[selectedIndex].substring(0, paintBrushNames[selectedIndex].lastIndexOf(".")));
            setIcon(icon);

            setPreferredSize(new Dimension(90, 24));
            setIconTextGap(10);
            setHorizontalTextPosition(LEFT);

            return this;
        }
    }
    

    private void initVOI()
    {
        int iActiveCount = 0;
        for ( int i = 0; i < triImage.length; i++ )
        {
            if ( triImage[i] != null )
            {
                iActiveCount++;
            }
        }
        voiManager = new VOIManagerInterface( this, imageA, imageB, iActiveCount, false );
        panelToolBarGBC.gridy++;
        panelToolbar.add( voiManager.getToolBar(), panelToolBarGBC );
        

        if (imageB != null) {
            voiManager.getVOIManager(AXIAL_AB).init( imageA, imageB,
                    triImage[AXIAL_AB], triImage[AXIAL_AB],
                    triImage[AXIAL_AB].getOrientation(), triImage[AXIAL_AB].getSlice() );
            
            voiManager.getVOIManager(CORONAL_AB).init( imageA, imageB,
                    triImage[CORONAL_AB], triImage[CORONAL_AB],
                    triImage[CORONAL_AB].getOrientation(), triImage[CORONAL_AB].getSlice() );
            
            voiManager.getVOIManager(SAGITTAL_AB).init( imageA, imageB,
                    triImage[SAGITTAL_AB], triImage[SAGITTAL_AB],
                    triImage[SAGITTAL_AB].getOrientation(), triImage[SAGITTAL_AB].getSlice() );
            

            voiManager.getVOIManager(AXIAL_B).init( null, imageB,
                    triImage[AXIAL_B], triImage[AXIAL_B],
                    triImage[AXIAL_B].getOrientation(), triImage[AXIAL_B].getSlice() );
            
            voiManager.getVOIManager(CORONAL_B).init( null, imageB,
                    triImage[CORONAL_B], triImage[CORONAL_B],
                    triImage[CORONAL_B].getOrientation(), triImage[CORONAL_B].getSlice() );
            
            voiManager.getVOIManager(SAGITTAL_B).init( null, imageB,
                    triImage[SAGITTAL_B], triImage[SAGITTAL_B],
                    triImage[SAGITTAL_B].getOrientation(), triImage[SAGITTAL_B].getSlice() );
        }


        voiManager.getVOIManager(AXIAL_A).init( imageA, null,
                triImage[AXIAL_A], triImage[AXIAL_A],
                triImage[AXIAL_A].getOrientation(), triImage[AXIAL_A].getSlice() );
        
        voiManager.getVOIManager(CORONAL_A).init( imageA, null,
                triImage[CORONAL_A], triImage[CORONAL_A],
                triImage[CORONAL_A].getOrientation(), triImage[CORONAL_A].getSlice() );
        
        voiManager.getVOIManager(SAGITTAL_A).init( imageA, null,
                triImage[SAGITTAL_A], triImage[SAGITTAL_A],
                triImage[SAGITTAL_A].getOrientation(), triImage[SAGITTAL_A].getSlice() );
        
        voiManager.getToolBar().setVisible(true);
    }

    public void PointerActive(boolean bActive) {
        for ( int i = 0; i < triImage.length; i++ )
        {
            if ( triImage[i] != null )
            {
                if ( bActive )
                {
                    triImage[i].setCursorMode(ViewJComponentBase.VOI_3D);
                }
                else
                {
                    triImage[i].setCursorMode(ViewJComponentBase.DEFAULT);
                }
            }
        }
    }

    public Vector3f PropDown(int iActive) {
        if ( triImage[iActive] != null )
        {
            return triImage[iActive].downSlice();
        }
        return null;
    }


    public Vector3f PropUp(int iActive) {
        if ( triImage[iActive] != null )
        {
            return triImage[iActive].upSlice();
        }
        return null;
    }


    public void create3DVOI(boolean bIntersection) {
        System.err.println( "create3DVOI " + bIntersection );        
    }

    public void setCursor(Cursor kCursor) {
        for (int i = 0; i < MAX_TRI_IMAGES; i++) {
            if (triImage[i] != null) {
                triImage[i].setCursor(kCursor);
            }
        }
    }

    public void setDefaultCursor() {
        PointerActive(false);
    }

    public void setModified() {
        updateImages();
    }

    public void updateData(boolean bCopyToCPU) {
        System.err.println( "updateData " + bCopyToCPU); 
    }
}
