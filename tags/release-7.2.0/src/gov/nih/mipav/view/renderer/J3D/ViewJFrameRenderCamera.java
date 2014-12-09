package gov.nih.mipav.view.renderer.J3D;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;

import javax.swing.*;
import javax.swing.border.*;


/**
 * This class builds a "light box" view of a 3D or 4D dataset. In addition, a 2nd image can be overlayed and an alpha
 * value can be adjusted to control the amount of each image that is displayed. The user can control how the images are
 * tiled by specifing the number of row or cols, border thickness and size, and magnification. The options can changed
 * using the dialog accessed via the options menu.
 *
 * @version  1.0 July 8, 1999
 * @author   Matthew J. McAuliffe Ph.D.
 * @author   Ruida Cheng
 */

public class ViewJFrameRenderCamera extends ViewJFrameBase implements MouseListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6223794265610151148L;

    /**
     * default values indicating minimum, maximum sizes are in PIXELS on a side for a magnification. this is to fix the
     * magnification problems of very small images for which the arbitrary limits of lightbox image sizes of 25%-75% of
     * the full size of an image. in practice the default max will reference the side with greatest magnitude ...
     */
    private static final int DEFAULT_IMAGE_SIZE = 128;

    /** DOCUMENT ME! */
    private static final int DEFAULT_IMAGE_MIN = 96; // 108;

    /** DOCUMENT ME! */
    private static final int DEFAULT_XSCREEN_SPACE = 128;

    /** DOCUMENT ME! */
    private static final int DEFAULT_YSCREEN_SPACE = 64;

    /**
     * default row is dependent, and column independent.
     *
     * <p>the default is the number of rows is the dependent variable, and is dependent apon the total number of images
     * and the number of columns.</p>
     */
    public static final boolean DEFAULT_DEPENDENT_ROW = true;

    /** default magnification of images in the light-box. */
    public static final float MIN_MAGNIFICATION = 25;

    /** DOCUMENT ME! */
    public static final float MAX_MAGNIFICATION = 75;

    /** maximum row value (when it is the independent variable). */
    public static final int MAX_GRID_ROW = 1;

    /** maximum column value (when it is the independent variable). */
    public static final int MAX_GRID_COL = 10;

    /** maximum spacing in-between images in the light-box. */
    public static final int MAX_GRID_SIZE = 20;

    /** maximum spacing of the border surrounding images in the light-box. */
    public static final int MAX_GRID_BORDER = 10;

    /** maximum spacing of the selection border surrounding images in the light-box. */
    public static final int MAX_SELECTED_GRID_BORDER = 5;

    /** DOCUMENT ME! */
    public static final int NO_AXIS = -1;

    /** X axis constant. */
    public static final int X_AXIS = 1;

    /** Y axis constant. */
    public static final int Y_AXIS = 2;

    /** Z axis constant. */
    public static final int Z_AXIS = 3;

    /** elements of tBar. */
    private static JToggleButton[] toggleArray;

    /** Show standard progress bar with cancel. */
    public static final int STANDARD = 0;

    /** Show progress bar without a cancel button. */
    public static final int NO_CANCEL = 1;

    /** Do no show progress bar. */
    public static final int NO_PROGRESS = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Flag indicating if a whether of not the progress bar is visible. */
    protected boolean pBarVisible = true;

    /** Progress bar object. */
    protected ViewJProgressBar progressBar;

    /** Progress bar default location. */
    protected Point progressBarLocation = null;

    /** Progress mode - either standard, no cancel, or no progress bar. */
    protected int progressMode = STANDARD;

    /** color for the line border surrounding each image in the light-box. */
    private Color borderColor;

    /** spacing for the line border surrounding each image in the light-box. */
    private int borderSize;

    /** DOCUMENT ME! */
    private Vector componentImageVector;

    /** DOCUMENT ME! */
    private int currentSelectedSlice = 0;

    /** current info -- this slice info refers to the *real* slice number, not the page index for a slice on a page. */
    private int currentSlice = 0; // current highlighted slice

    /** Delete the captured image button. */
    private JButton deleteButton;

    /** Delete menu icon item. */
    private JMenuItem deleteSelection;

    /** Extract the selected image button. */
    private JButton extractButton;

    /** Extrace menu icon item. */
    private JMenuItem extractSelection;

    /** DOCUMENT ME! */
    private boolean firstTime = true;

    /** DOCUMENT ME! */
    private int frameHeight;

    /** DOCUMENT ME! */
    private Point frameStartLocation = new Point();

    /** frame information. */
    private int frameWidth;

    /** color for the region in-between the images in the light-box. */
    private Color gridColor;

    /** DOCUMENT ME! */
    private int gridColumn = 0; // #of widths of image the viewing area should display.

    /**
     * value for the number of rows (number of images along the y-axis) and columns (number of images along the x-axis)
     * in the light-box.
     *
     * <p>Note that both row and column set to 1 cannot make sense, but that one or the other will be the 'dependent
     * variable' as defined by the row_dependent boolean var.</p>
     *
     * <p>A value of 0 means that the dependent variable will automatically be sized to its maximum size that can fit
     * nicely on the screen.</p>
     */
    private int gridRow = 0; // guess that this is same as below, but in other direction:

    /** spacing between images in the light-box. */
    private int gridSpacing;

    /** DOCUMENT ME! */
    private ButtonGroup group = new ButtonGroup();

    /** DOCUMENT ME! */
    private Vector imageBorderVector;

    /** DOCUMENT ME! */
    private float[] imageBufferA;

    /** DOCUMENT ME! */
    private int imageHeight = 128;

    /** per num visible images information. */
    private int imagePanelSizeX, imagePanelSizeY;

    /** DOCUMENT ME! */
    private Vector imagePanelVector;

    /** DOCUMENT ME! */
    private int imageWidth = 128;

    /** DOCUMENT ME! */
    private int lastSelectedSlice = 0; // keep track of the last selected slice

    /** DOCUMENT ME! */
    private float magMax = MAX_MAGNIFICATION;

    /** DOCUMENT ME! */
    private float magMin = MIN_MAGNIFICATION;

    /** magnification of images in the light-box (mag is a percentage). */
    private float magnification = 45;

    /** DOCUMENT ME! */
    private int maxPagePanelSizeX = 0;

    /** DOCUMENT ME! */
    private int maxPagePanelSizeY = 0;

    /** variables for the menubar. */
    private JMenuBar menuBar;

    /** DOCUMENT ME! */
    private Vector modelImageVector;

    /** DOCUMENT ME! */
    private int numTotalSlices = 0;

    /** Number of pages in this frame will always be one. */
    private JPanel pagePanel;

    /** DOCUMENT ME! */
    private int pagePanelSizeX = 0;

    /** DOCUMENT ME! */
    private int pagePanelSizeY = 0;

    /** Scroll panel to handle the frame scolling. */
    private JScrollPane pageScrollPanel;

    /** DOCUMENT ME! */
    private RenderViewBase parentFrame;

    /** DOCUMENT ME! */
    private int[] pixBuffer;

    /**
     * indicates which--row or column--should be dependent on the other.
     *
     * <p>the default is the number of rows is the dependent variable, and is dependent apon the total number of images
     * and the number of columns. Don't confuse the reading to be "This display is (t/f) dependent on the row." It
     * -could- be read as "The display is a column-independent (t/f) matrix".</p>
     */
    private boolean row_dependent = true; // number of rows (y-axis) is dependent on # images & # cols.

    /** color for the line border surrounding each selected image in the light-box. */
    private Color selectedBorderColor;

    /** spacing for the selected line border surrounding each image in the light-box. */
    private int selectedBorderSize;

    /**
     * the selectedImages contains the *real* slice numbers -- not the index of a slice on a page. So the maximum number
     * in the vector will be numTotalSlices - 1.
     */
    private Vector selectedImages = new Vector();

    /** Reference to imageA. */
    private ModelImage srcImage = null;

    /** Frame ToolBar. */
    private JToolBar tBar;

    /** variables for the toolbar. */
    private JPanel toolbarPanel;

    /** DOCUMENT ME! */
    private Color unselectedBorderColor = Color.black;

    /** DOCUMENT ME! */
    private int xScreen, yScreen;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Builds the initial lightbox view.
     *
     * @param  _imageA       model of image A
     * @param  _parentFrame  title of the frame
     *
     * @parem  _parentFrame reference to the paren frame.
     */
    public ViewJFrameRenderCamera(ModelImage _imageA, RenderViewBase _parentFrame) {

        super(_imageA, null);

        parentFrame = _parentFrame;

        componentImageVector = new Vector();
        imagePanelVector = new Vector();
        imageBorderVector = new Vector();
        modelImageVector = new Vector();
        toggleArray = new JToggleButton[2];

        setResizable(true);

        // initialize the selectedImages vector
        selectedImages.removeAllElements();

        // set the mag's max/min percentages so that magnification can be properly set
        setMagMax();
        setMagMin();

        // all defaults for the lightbox have been moved to the defaults in the
        // Preferences class.  So can just get the initial values from the properties.
        setGridColumn(Integer.parseInt(Preferences.getProperty(Preferences.PREF_LB_GRID_COL)));
        setGridSpacing(Integer.parseInt(Preferences.getProperty(Preferences.PREF_LB_GRID_SIZE)));
        setGridColor(extractColor(Preferences.getProperty(Preferences.PREF_LB_GRID_COLOR)));
        setBorderSize(Integer.parseInt(Preferences.getProperty(Preferences.PREF_LB_BORDER_SIZE)));
        setBorderColor(extractColor(Preferences.getProperty(Preferences.PREF_LB_BORDER_COLOR)));
        setSelectedBorderColor(extractColor(Preferences.getProperty(Preferences.PREF_LB_SELECTED_BORDER_COLOR)));
        setSelectedBorderSize(Integer.parseInt(Preferences.getProperty(Preferences.PREF_LB_SELECTED_BORDER_SIZE)));

        try {

            // get the number of slices and numTSlices from imageA
            numTotalSlices = imageA.getExtents()[2];

            if ((gridRow * gridColumn) > numTotalSlices) {

                if (row_dependent == true) {
                    gridRow = 1;
                    gridColumn = numTotalSlices;
                } else {
                    gridRow = numTotalSlices;
                    gridColumn = 1;
                }
                // storeToDefaults();
            }

            // determine all the window and panel sizes first
            calcScreenSize(); // won't change during runtime
            calcImagePanelSize(); // changes if magnification or image changes

            // need to build the toolbars and menubar in order to
            // determine the maxPagePanelSize
            buildMenuBar(); // size won't change during runtime
            buildToolbar(); // size will change if row_dependent changes

            // now can get the size of the pagePanel, this also
            // determines gridRow and gridColumn, and the number of images per page
            calcPagePanelSize(); // changes if magnification, gridRow, gridColumn or row_dependent change

            // initialize the buffers that hold the image data
            initBuffers();

            // set the current slice to the middle slice
            currentSlice = numTotalSlices / 2;

            setupCameraStorage();

            addComponentListener(this);
            setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
        } catch (OutOfMemoryError error) {
            throw error;
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Handler for action events.
     *
     * @param  event  event which trigger the call to the handler
     */
    public void actionPerformed(ActionEvent event) {

        if (event.getActionCommand().equals("SelectAll")) {
            Preferences.debug("Lightbox: Select All \n");
            selectAll();
        } else if (event.getActionCommand().equals("SelectNone")) {
            Preferences.debug("Lightbox: Select None \n");
            selectNone();
        } else if (event.getActionCommand().equals("SelectInvert")) {
            Preferences.debug("Lightbox: Select Invert \n");
            invertSelections();
        } else if (event.getActionCommand().equals("DeleteSelection")) {
            Preferences.debug("Lightbox: Delete selections \n");
            deleteSlices();
        } else if (event.getActionCommand().equals("ExtractSelection")) {
            Preferences.debug("Lightbox: Extract selections \n");
            extractImages();
        } else if (event.getActionCommand().equals("MagRegion")) {

            for (int i = 0; i < numTotalSlices; i++) {
                (((ViewJComponentEditImage) componentImageVector.elementAt(i))).setCursorMode(ViewJComponentEditImage.MAG_REGION);
            }
        } else if (event.getActionCommand().equals(CustomUIBuilder.PARAM_VOI_DEFAULT_POINTER)) {

            for (int i = 0; i < numTotalSlices; i++) {
                (((ViewJComponentEditImage) componentImageVector.elementAt(i))).setCursorMode(ViewJComponentEditImage.DEFAULT);
            }
        } else if (event.getActionCommand().equals("SaveImageAs")) {
            saveAVI();
        }

    }

    /**
     * Add model image into the capture frame.
     *
     * @param  _imageA  Model image A.
     */
    public void addImage(ModelImage _imageA) {
        modelImageVector.add(_imageA);
        updatePanelLayout();

        try {
            buildPage();
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Error rebuilding page: " + e.getMessage());
            throw (e);
        }

        if (updateImages(true) == false) {
            return;
        }

        Point t = new Point();
        int fwidth = (int)
                         Math.round((double) gridColumn *
                                        (double) (imagePanelSizeX + gridSpacing + (2 * borderSize) +
                                                      (2 * selectedBorderSize))) + gridSpacing;

        t.setLocation(fwidth, 0);
        pageScrollPanel.getViewport().setViewPosition(t);

    }

    /**
     * Closes the frame and calls disposeLocal and dispose.
     */
    public void close() {
        setVisible(false);

        if (imageA != null) {
            imageA.removeImageDisplayListener(this);
        }

        if (imageB != null) {
            imageB.removeImageDisplayListener(this);
        }

        disposeLocal(false);
        dispose();
    }

    /**
     * Closes image and updates display.
     *
     * @param  image  ModelImage reference
     */
    public void closeImage(ModelImage image) {

        if (image != null) {
            image.removeImageDisplayListener(this);

            if ((image.getImageFrameVector() != null) && image.getImageFrameVector().isEmpty()) {
                image.disposeLocal();
            }

            image = null;
        }
    }

    /**
     * Method called when a component resize event is generated. This method snaps the size of the frame and pagePanel
     * to the nearest row, column sizing (so the gridRow and gridColumn and page layout may change).
     *
     * @param  event  ComponentEvent
     */
    public synchronized void componentResized(ComponentEvent event) { }

    /**
     * Dispose the global variables.
     *
     * @param  flag  boolean dispose super or not.
     */
    public void disposeLocal(boolean flag) {

        imageBufferA = null;
        pixBuffer = null;

        if (pagePanel != null) {
            pagePanel.removeAll();
        }

        pagePanel = null;
        menuBar = null;

        if (toolbarPanel != null) {
            toolbarPanel.removeAll();
        }

        toolbarPanel = null;
        LUTa = null;
        LUTb = null;

        menuBar = null;
        deleteSelection = null;
        extractSelection = null;

        toolbarPanel = null;
        group = null;
        deleteButton = null;
        extractButton = null;
        tBar = null;

        srcImage = null;

        frameStartLocation = null;
        progressBarLocation = null;
        pageScrollPanel = null;

        if (progressBar != null) {
            progressBar = null;
        }

        toggleArray = null;

        if (imageBorderVector != null) {
            imageBorderVector.removeAllElements();
            imageBorderVector = null;
        }

        if (imagePanelVector != null) {
            imagePanelVector.removeAllElements();
            imagePanelVector = null;
        }

        ViewJComponentEditImage compRef;

        if (componentImageVector != null) {

            for (int i = 0; i < componentImageVector.size(); i++) {
                compRef = (ViewJComponentEditImage) (componentImageVector.elementAt(i));
                compRef.dispose(false);
                compRef = null;
            }

            componentImageVector.removeAllElements();
            componentImageVector = null;
        }

        ModelImage imageRef;

        if (modelImageVector != null) {

            for (int i = 0; i < modelImageVector.size(); i++) {
                imageRef = ((ModelImage) (modelImageVector.elementAt(i)));
                closeImage(imageRef);
            }

            modelImageVector.removeAllElements();
            modelImageVector = null;
        }

        if (selectedImages != null) {
            selectedImages.removeAllElements();
            selectedImages = null;
        }

        if (flag == true) {
            super.close();
        }

    }

    /**
     * Gets control widgets for frame.
     *
     * @return  DOCUMENT ME!
     */
    public ViewControlsImage getControls() {
        return null;
    }

    /**
     * Returns the reference to imageA.
     *
     * @return  image
     */
    public ModelImage getImageA() {
        return imageA;
    }

    /**
     * Returns the reference to imageB.
     *
     * @return  imageB
     */
    public ModelImage getImageB() {
        return imageB;
    }

    /**
     * Returns flag that indicates that the progressBar is visible.
     *
     * @return  <code>true</code> if progress bar is visible.
     */
    public final boolean isProgressBarVisible() {

        if ((pBarVisible == true) && (progressBar != null)) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * Loads the images for a page into component images and then loads the page.
     *
     * @throws  OutOfMemoryError  DOCUMENT ME!
     */
    public void loadPage() throws OutOfMemoryError {

        // build bordered panel and put image slice inside
        // System.out.println("loadPage: page " + page + " num of visible slices " + numTotalSlices);
        for (int i = 0; i < numTotalSlices; i++) {

            if (i < modelImageVector.size()) {

                // slice refers to the "real" slice number in the image
                int slice = i;

                if (slice < numTotalSlices) {
                    ((ViewJComponentEditImage) (componentImageVector.elementAt(i))).setSlice(slice);
                } else {
                    ((ViewJComponentEditImage) (componentImageVector.elementAt(i))).showBlank();
                }

                if (slice == userInterface.getActiveImageFrame().getViewableSlice()) {
                    ((ViewJComponentEditImage) (componentImageVector.elementAt(i))).useHighlight(true);
                    currentSlice = slice;
                } else {
                    ((ViewJComponentEditImage) (componentImageVector.elementAt(i))).useHighlight(false);
                }
            }
        } // end loop on visible slices
    } // end loadPage()


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
        Object source = mouseEvent.getSource();

        for (int i = 0; i < componentImageVector.size(); i++) {

            if (((ViewJComponentEditImage) source).equals(componentImageVector.elementAt(i))) {
                updateImageSelection(i, mouseEvent.isShiftDown());
            }
        }
    }

    /**
     * unchanged.
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
     * A mouse event. Sets the mode of the program depending on the cursor mode. If the mode is move, activates the
     * contour or line and enables the delete button.
     *
     * @param  mouseEvent  event that triggered function
     */
    public void mousePressed(MouseEvent mouseEvent) { }

    /**
     * A mouse event. This function sets up and draws the VOI according to the mode.
     *
     * @param  mouseEvent  event that triggered function
     */
    public void mouseReleased(MouseEvent mouseEvent) { }

    /**
     * Removes the menu and controls of the main frame so that a new frame can load the main frame with the proper
     * controls.
     *
     * <p>This function no longer loads its controls into the main control window however, this method is required
     * because this class extends ViewJFrameBase</p>
     */
    public void removeControls() { }

    /**
     * Sets the active image for drawing VOIs.
     *
     * @param  active  IMAGE_A or IMAGE_B
     */
    public void setActiveImage(int active) {

        if (componentImageVector != null) {

            for (int i = 0; i < numTotalSlices; i++) {
                (((ViewJComponentEditImage) componentImageVector.elementAt(i))).setActiveImage(active);
            }
        }

        if (active == IMAGE_A) {
            setTitle();
            displayMode = IMAGE_A;
        } else {
            setTitle();
            displayMode = IMAGE_B;
        }

        updateImages(false);
    }

    /**
     * Sets the alpha blending of parameter for two image displaying.
     *
     * @param  value  amount [0,100] that is the percentage of Image A to be displayed
     */
    public void setAlphaBlend(int value) { }


    /**
     * Sets the menu and controls (i.e. toolbars) of the main frame! This puts the menus and controls needed to controls
     * the operations of this frame. Different image frames have different menu and controls.
     *
     * <p>This function no longer loads its controls into the main control window however, this method is required
     * because this class extends ViewJFrameBase</p>
     */
    public void setControls() { }

    /**
     * Controls whether or not the images/VOIs of the frame can be modified.
     *
     * @param  flag  if true the image/VOIs can be modified; if false image/VOIs can NOT be modified
     */
    public void setEnabled(boolean flag) { }

    /**
     * Accessor that sets the reference to imageA.
     *
     * @param  _imageA  image to set the frame to
     */
    public void setImageA(ModelImage _imageA) { }

    /**
     * Accessor that sets the reference to imageB.
     *
     * @param  _imageB  image to set the frame to
     */
    public void setImageB(ModelImage _imageB) {
        return;
    }

    /**
     * When switching the active image, copy the paintBitmap of the previous active image into the paintBitmap of the
     * new active image.
     *
     * @param  paintBitmapSwitch  DOCUMENT ME!
     */
    public void setPaintBitmapSwitch(boolean paintBitmapSwitch) { }

    /**
     * DOCUMENT ME!
     *
     * @param  xDim  DOCUMENT ME!
     * @param  yDim  DOCUMENT ME!
     */
    public void setProgressBarInitLocation(int xDim, int yDim) {

        if (progressBarLocation == null) {
            progressBarLocation = new Point(xDim, yDim);
        } else {
            progressBarLocation.setLocation(xDim, yDim);
        }
    }

    /**
     * Sets Progress Bar visibility.
     *
     * @param  flag  flag to set to
     */
    public void setProgressBarVisible(boolean flag) {

        pBarVisible = flag;

        if (progressBar != null) {
            progressBar.setVisible(flag);
        }
    }

    /**
     * The following 2 functions set the RGB tables for ARGB images A and B.
     *
     * @param  RGBT  DOCUMENT ME!
     */
    public void setRGBTA(ModelRGB RGBT) { }

    /**
     * DOCUMENT ME!
     *
     * @param  RGBT  DOCUMENT ME!
     */
    public void setRGBTB(ModelRGB RGBT) { }

    /**
     * Used in ViewImageUpdateInterface. This method sets the
     *
     * @param  slice  image plane
     */
    public void setSlice(int slice) {
        updateImage(currentSlice, false);
        currentSlice = slice;
        updateImage(currentSlice, true);
    }

    /**
     * Sets the time slice if the image has 4 dimensions and updates the image and title.
     *
     * @param  tslice  time slice
     */
    public void setTimeSlice(int tslice) { }

    /**
     * Sets the title of the frame with the image name of slice location.
     */
    public void setTitle() {
        String str;

        if (displayMode == IMAGE_A) {

            str = imageA.getImageName() + "  M:" +
                  makeString((((ViewJComponentEditImage) componentImageVector.elementAt(0))).getZoomX(), 2);
            setTitle(str);
        } else {

            str = imageB.getImageName() + "  M:" +
                  makeString((((ViewJComponentEditImage) componentImageVector.elementAt(0))).getZoomX(), 2);
            setTitle(str);
        }
    }

    /**
     * This methods calls the componentImage's update method to repaint the screen. This will set the requested
     * highlight when the repaint is issued. Use setSlice() to update a single frames highlight, as this will not set
     * the current slice.
     *
     * @param   slice          the slice of the image to update -- NOT the index into the visibile slices.
     * @param   highlightFlag  requests to paint the highlight around the component image
     *
     * @return  boolean confirming successful update see setSlice
     *
     * @see     ViewJComponentEditImage#useHighlight(boolean)
     */
    public final boolean updateImage(int slice, boolean highlightFlag) {

        if (componentImageVector == null) {
            return false;
        }

        int i = slice;

        try {

            if (i < componentImageVector.size()) {

                if (((ViewJComponentEditImage) componentImageVector.elementAt(i)) != null) {
                    (((ViewJComponentEditImage) componentImageVector.elementAt(i))).useHighlight(highlightFlag);
                    (((ViewJComponentEditImage) componentImageVector.elementAt(i))).paintComponent((((ViewJComponentEditImage)
                                                                                                         componentImageVector.elementAt(i)))
                                                                                                       .getGraphics());
                }
            }
        } catch (ArrayIndexOutOfBoundsException aioobe) {
            Preferences.debug("Unable to update lightbox image!  Out of bounds!");

            return false;
        }

        return true;
    }

    /**
     * This methods updates the image border for a single slice.
     *
     * @param  i  the index of the image to update (NOT the slice number)
     */
    public void updateImageBorder(int i) {

        if (imageBorderVector == null) {
            return;
        }

        int slice = i;

        try {
            LineBorder outerBorder = new LineBorder(borderColor, borderSize);
            LineBorder innerBorder;

            if (selectedImages.contains(Integer.toString(slice))) {
                innerBorder = new LineBorder(selectedBorderColor, selectedBorderSize);
            } else {
                innerBorder = new LineBorder(unselectedBorderColor, selectedBorderSize);
            }

            imageBorderVector.set(i, new CompoundBorder(outerBorder, innerBorder));
            (((JPanel) imagePanelVector.elementAt(i))).setBorder((CompoundBorder) (imageBorderVector.elementAt(i)));
        } catch (ArrayIndexOutOfBoundsException aioobe) {
            Preferences.debug("Unable to update lightbox border for image " + i + ".  Out of bounds!");
        }
    }

    /**
     * This method updates all the image borders for a page. This needs to be called whenever a new page is loaded.
     */
    public void updateImageBorders() {

        for (int i = 0; i < numTotalSlices; i++) {
            updateImageBorder(i);
        }

    } // end updateImageBorders()

    /**
     * (Part of ViewImageUpdateInterface) This methods calls the componentImage's REPAINT method to redraw the screen.
     * The extents on this image have changed, so the extents need to be read in again and menus, panes and slide bars
     * adjusted accordingly.
     *
     * @return  DOCUMENT ME!
     */
    public boolean updateImageExtents() {
        return false;
    }

    /**
     * This methods calls the componentImage's update method to repaint the screen.
     *
     * @return  boolean confirming successful update
     */
    public final boolean updateImages() {

        if (componentImageVector == null) {
            return false;
        }

        for (int i = 0; i < componentImageVector.size(); i++) {

            if (((ViewJComponentEditImage) componentImageVector.elementAt(i)) != null) {
                int slice = i;

                if (slice < numTotalSlices) {

                    if (slice == userInterface.getActiveImageFrame().getViewableSlice()) {
                        (((ViewJComponentEditImage) componentImageVector.elementAt(i))).useHighlight(true);
                        currentSlice = slice;
                    } else {
                        (((ViewJComponentEditImage) componentImageVector.elementAt(i))).useHighlight(false);
                    }

                    (((ViewJComponentEditImage) componentImageVector.elementAt(i))).paintComponent((((ViewJComponentEditImage)
                                                                                                         componentImageVector.elementAt(i)))
                                                                                                       .getGraphics());
                } else {
                    (((ViewJComponentEditImage) componentImageVector.elementAt(i))).showBlank();
                }

            }
        }

        return true;
    }

    /**
     * This methods calls the componentImage's update method to redraw the screen. Without LUT changes.
     *
     * @param   forceShow  forces show to re import image and calc. java image
     *
     * @return  boolean confirming successful update
     */
    public final boolean updateImages(boolean forceShow) {

        if (componentImageVector == null) {
            return false;
        }

        int currentIndex = modelImageVector.size() - 1;

        if (currentIndex < 0) {
            currentIndex = 0;
        }

        if (componentImageVector.size() <= currentIndex) {
            return false;
        }

        (((ViewJComponentEditImage) componentImageVector.elementAt(currentIndex))).setStringOverride(String.valueOf(currentIndex +
                                                                                                                    1));
        (((ViewJComponentEditImage) componentImageVector.elementAt(currentIndex))).show(0, 0, null, null, true, -1);

        return true;
    }

    /**
     * This methods calls the componentImage's update method to redraw the screen.
     *
     * @param   _LUTa       LUT used to update imageA
     * @param   _LUTb       LUT used to update imageB
     * @param   forceShow   forces show to re import image and calc. java image
     * @param   interpMode  image interpolation method (Nearest or Smooth)
     *
     * @return  boolean confirming successful update
     */
    public final boolean updateImages(ModelLUT _LUTa, ModelLUT _LUTb, boolean forceShow, int interpMode) {

        if (componentImageVector == null) {
            return false;
        }

        for (int i = 0; i < componentImageVector.size(); i++) {
            int slice = i;

            if (slice < numTotalSlices) {

                if ((((ViewJComponentEditImage) componentImageVector.elementAt(i))).show(0, 0, LUTa, LUTb, true,
                                                                                             interpMode) == false) {
                    return false;
                }

                if (slice == userInterface.getActiveImageFrame().getViewableSlice()) {
                    (((ViewJComponentEditImage) componentImageVector.elementAt(i))).useHighlight(true);
                    currentSlice = slice;
                } else {
                    (((ViewJComponentEditImage) componentImageVector.elementAt(i))).useHighlight(false);
                }
            } else {
                (((ViewJComponentEditImage) componentImageVector.elementAt(i))).showBlank();
            }
        }

        return true;
    }

    /**
     * Updates the Z(3rd dimension) plane and the time (4th dimesion) volume.
     *
     * @param  plane       image plane that is to displayed
     * @param  timeVolume  the volume in which the plane the is to be displayed from
     */
    public void updateImageSlice(int plane, int timeVolume) {

        // imageFrame.getImageA().setSlice( plane );
        // imageFrame.getImageA().setTimeSlice( timeVolume );
        imageA.setSlice(plane);
        imageA.setTimeSlice(timeVolume);
        setSlice(plane);
    }

    /**
     * Over loaded so that it doesn't do anything.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowActivated(WindowEvent event) { }

    /**
     * Ask user if really wishes to close the image frame.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowClosing(WindowEvent event) {

        int reply = JOptionPane.showConfirmDialog(this, "Do you really want to close the camera capture frame?",
                                                  "Camera capture frame close", JOptionPane.YES_NO_OPTION,
                                                  JOptionPane.QUESTION_MESSAGE);

        if (reply == JOptionPane.YES_OPTION) {

            if (parentFrame != null) {
                parentFrame.disableCamera();
            }

            close();
        }
    }

    /**
     * Constructs progress bar.
     *
     * @param  imageName  title of the toolbar
     * @param  message    message to be displayed in the frame
     * @param  start      start (typical = 0)
     * @param  end        end (typical = 100)
     */
    protected void buildProgressBar(String imageName, String message, int start, int end) {

        if (pBarVisible == true) {

            if (progressMode == STANDARD) {
                progressBar = new ViewJProgressBar(imageName, message, start, end, true, this, this);
            } else if (progressMode == NO_CANCEL) {
                progressBar = new ViewJProgressBar(imageName, message, start, end, false, this, this);
            } else if (progressMode == NO_PROGRESS) {
                Preferences.debug("Tried to build a progress bar when progressMode == NO_PROGRESS.\n");
            }
        }
    }

    /**
     * Disposes of progress bar.
     */
    protected void disposeProgressBar() {

        if (progressBar != null) {
            progressBar.dispose();
        }
    }

    /**
     * finalize - calls dispose.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        disposeLocal(false);
        super.finalize();
    }

    /**
     * Initializes progress bar.
     */
    protected void initProgressBar() {

        if ((pBarVisible == true) && (progressBar != null)) {

            if (progressBarLocation == null) {
                int xScreen = Toolkit.getDefaultToolkit().getScreenSize().width;
                int yScreen = 100; // Toolkit.getDefaultToolkit().getScreenSize().height;

                progressBar.setLocation(xScreen / 2, yScreen / 2);
            } else {
                progressBar.setLocation(progressBarLocation);
            }

            progressBar.setVisible(true);
        }
    }

    /**
     * Method to invert the selection of slices in an image.
     */
    protected void invertSelections() {

        // go through the selectedImages list -- remove items that
        // are in it, and add items that aren't in it
        for (int i = 0; i < numTotalSlices; i++) {

            if (selectedImages.contains(Integer.toString(i))) {
                selectedImages.removeElement(Integer.toString(i));
            } else {
                selectedImages.addElement(Integer.toString(i));
            }

            // only update the borders of visible slices
            int pageIndex = i;

            if ((pageIndex >= 0) && (pageIndex < numTotalSlices)) {
                updateImageBorder(pageIndex);
            }
        }

        setButtonStatus();
        System.gc();

    } // end invertSelections()

    /**
     * Method to select all the slices in an image.
     */
    protected void selectAll() {

        for (int i = 0; i < numTotalSlices; i++) {

            // only add to list if it's not already there
            if (!selectedImages.contains(Integer.toString(i))) {
                selectedImages.addElement(Integer.toString(i));
            }

            // only update the borders of visible slices
            int pageIndex = i;

            if ((pageIndex >= 0) && (pageIndex < numTotalSlices)) {
                updateImageBorder(pageIndex);
            }
        }

        setButtonStatus();
        System.gc();

    } // end selectAll()

    /**
     * Method to select none of the slices in an image.
     */
    protected void selectNone() {

        for (int i = 0; i < numTotalSlices; i++) {

            // only remove from list if it's already there
            if (selectedImages.contains(Integer.toString(i))) {
                selectedImages.removeElement(Integer.toString(i));
            }

            // only update the borders of visible slices
            int pageIndex = i;

            if ((pageIndex >= 0) && (pageIndex < numTotalSlices)) {
                updateImageBorder(pageIndex);
            }
        }

        setButtonStatus();
        System.gc();

    } // end selectNone()

    /**
     * Method to set the status of the delete and extract menuItems and Buttons based on the selectedImages vector.
     */
    protected void setButtonStatus() {

        if (selectedImages.isEmpty()) {
            deleteSelection.setEnabled(false);
            extractSelection.setEnabled(false);
            deleteButton.setEnabled(false);
            extractButton.setEnabled(false);
        } else {
            deleteSelection.setEnabled(true);
            extractSelection.setEnabled(true);
            deleteButton.setEnabled(true);
            extractButton.setEnabled(true);
        }

    } // end setButtonStatus()

    /**
     * Sets the maximum magnification to the image. will ensure that the current magnification is not larger than the
     * maximum.
     */
    protected void setMagMax() {
        magMax = ((float) this.DEFAULT_IMAGE_SIZE) / Math.max(imageA.getExtents()[0], imageA.getExtents()[1]) * 100;

        if (magnification > magMax) {
            magnification = magMax;
        }
    }

    /**
     * Sets the minimum magnification to the image. will ensure that the current magnification is not smaller than the
     * minimum.
     */
    protected void setMagMin() {
        magMin = ((float) this.DEFAULT_IMAGE_MIN) / Math.max(imageA.getExtents()[0], imageA.getExtents()[1]) * 100;

        if (magnification < magMin) {
            magnification = magMin;
        }
    }


    /**
     * Builds a simple menubar for this frame (ViewJFrameLightBox).
     */
    private void buildMenuBar() {
        Font font12B = MipavUtil.font12B;

        JMenu editMenu = new JMenu("Edit");
        editMenu.setFont(font12B);

        JMenu optionMenu = new JMenu("Options");
        optionMenu.setFont(font12B);

        // add menu items to edit menu
        JMenuItem selectAll = new JMenuItem("Select All", 'A');

        selectAll.addActionListener((ActionListener) this);
        selectAll.setActionCommand("SelectAll");
        selectAll.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_A, java.awt.Event.CTRL_MASK));
        selectAll.setFont(font12B);
        editMenu.add(selectAll);

        JMenuItem selectNone = new JMenuItem("Select None");

        selectNone.addActionListener((ActionListener) this);
        selectNone.setActionCommand("SelectNone");
        selectNone.setFont(font12B);
        editMenu.add(selectNone);

        JMenuItem selectInvert = new JMenuItem("Invert Selections");

        selectInvert.addActionListener((ActionListener) this);
        selectInvert.setActionCommand("SelectInvert");
        selectInvert.setFont(font12B);
        editMenu.add(selectInvert);

        editMenu.addSeparator();

        deleteSelection = new JMenuItem("Delete", MipavUtil.getIcon("delete.gif"));
        deleteSelection.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0));
        deleteSelection.addActionListener((ActionListener) this);
        deleteSelection.setActionCommand("DeleteSelection");
        deleteSelection.setFont(font12B);
        deleteSelection.setEnabled(false);
        editMenu.add(deleteSelection);

        extractSelection = new JMenuItem("Extract", MipavUtil.getIcon("extract.gif"));
        extractSelection.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_E, java.awt.Event.CTRL_MASK));
        extractSelection.addActionListener((ActionListener) this);
        extractSelection.setActionCommand("ExtractSelection");
        extractSelection.setFont(font12B);
        extractSelection.setEnabled(false);
        editMenu.add(extractSelection);

        menuBar = new JMenuBar();
        menuBar.add(editMenu);
        menuBar.setOpaque(true);
        setJMenuBar(menuBar);
    }

    /**
     * Builds the panels for a page and adds them to the page.
     *
     * @throws  OutOfMemoryError  DOCUMENT ME!
     */
    private void buildPage() throws OutOfMemoryError {
        int[] extents;

        extents = new int[3];
        extents[0] = Math.round(imageA.getExtents()[0]);
        extents[1] = Math.round(imageA.getExtents()[1]);
        extents[2] = Math.round(imageA.getExtents()[2]);

        if (firstTime) {
            modelImageVector.add(imageA);
            numTotalSlices = modelImageVector.size();
            firstTime = false;
        }

        int currentIndex = modelImageVector.size() - 1;

        if (currentIndex < 0) {
            currentIndex = 0;
        }

        JPanel imagePanel = new JPanel();

        imagePanel.setLayout(new BorderLayout());

        LineBorder outerBorder = new LineBorder(borderColor, borderSize);
        LineBorder innerBorder = new LineBorder(unselectedBorderColor, selectedBorderSize);

        CompoundBorder imageBorder = new CompoundBorder(outerBorder, innerBorder);

        imagePanel.setBorder(imageBorder);
        imageBorderVector.add(imageBorder);
        imagePanel.setBounds(0, 0, imagePanelSizeX, imagePanelSizeY);

        pagePanel.add(imagePanel);

        ViewJComponentEditImage componentImage = new ViewJComponentEditImage(this,
                                                                             (ModelImage)
                                                                             (modelImageVector.elementAt(currentIndex)),
                                                                             LUTa, imageBufferA, imageB, LUTb, null,
                                                                             null, magnification / 100, extents, false,
                                                                             FileInfoBase.UNKNOWN_ORIENT);

        componentImage.setLocation(borderSize + selectedBorderSize, borderSize + selectedBorderSize);

        componentImage.setBuffers(imageBufferA, null, pixBuffer, null);
        componentImage.MAGR_MAG = magnification / 100 * 2.0f;

        if (componentImage.MAGR_MAG < 1) {
            componentImage.MAGR_MAG = 1;
        }

        componentImage.MAGR_WIDTH = (int) (magnification / 100 * 0.5f * extents[0]);
        componentImage.MAGR_HEIGHT = (int) (magnification / 100 * 0.5f * extents[1]);
        componentImage.setResolutions(1, 1);
        componentImage.addMouseListener(this);
        componentImageVector.add(componentImage);

        imagePanel.add(componentImage);
        imagePanelVector.add(imagePanel);
        frameStartLocation = this.getLocation();
        // componentImage = null;
    } // end buildPage()

    /**
     * Builds a simple toolbar for this frame (ViewJFrameLightBox).
     */
    private void buildToolbar() {

        Border etchedBorder = BorderFactory.createEtchedBorder();
        Border pressedBorder = BorderFactory.createLoweredBevelBorder();

        toolbarPanel = new JPanel();

        GridLayout gLayout;
        gLayout = new GridLayout(1, 1, 0, 0);
        toolbarPanel.setLayout(gLayout);

        tBar = new JToolBar();
        tBar.setBorder(etchedBorder);
        tBar.setBorderPainted(true);
        tBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        tBar.setFloatable(false);

        JToggleButton pointerVOIButton = new JToggleButton(MipavUtil.getIcon("pointer.gif"));

        pointerVOIButton.addActionListener(this);
        pointerVOIButton.setMargin(new Insets(0, 0, 0, 0));
        pointerVOIButton.setToolTipText("Default Mode");
        pointerVOIButton.setActionCommand(CustomUIBuilder.PARAM_VOI_DEFAULT_POINTER.getActionCommand());
        pointerVOIButton.setBorderPainted(false);
        pointerVOIButton.setRolloverEnabled(true);
        pointerVOIButton.setRolloverIcon(MipavUtil.getIcon("pointerroll.gif"));
        pointerVOIButton.setBorder(pressedBorder);
        pointerVOIButton.setFocusPainted(false);
        group.add(pointerVOIButton);
        tBar.add(pointerVOIButton);
        toggleArray[0] = pointerVOIButton;

        JToggleButton zoomRegionButton = new JToggleButton(MipavUtil.getIcon("magregion.gif"));

        zoomRegionButton.setMargin(new Insets(0, 0, 0, 0));
        zoomRegionButton.addActionListener(this);
        zoomRegionButton.setToolTipText("Magnify Region");
        zoomRegionButton.setActionCommand("MagRegion");
        zoomRegionButton.setBorderPainted(false);
        zoomRegionButton.setRolloverEnabled(true);
        zoomRegionButton.setRolloverIcon(MipavUtil.getIcon("magregionroll.gif"));
        zoomRegionButton.setBorder(pressedBorder);
        zoomRegionButton.setFocusPainted(false);
        group.add(zoomRegionButton);
        tBar.add(zoomRegionButton);
        toggleArray[1] = zoomRegionButton;

        JButton separator = new JButton(MipavUtil.getIcon("separator.gif"));

        separator.setBorderPainted(false);
        separator.setFocusPainted(false);
        tBar.add(separator);

        // add new buttons for deleting and extracting selections
        deleteButton = new JButton(MipavUtil.getIcon("delete.gif"));
        deleteButton.addActionListener((ActionListener) this);
        deleteButton.setActionCommand("DeleteSelection");
        deleteButton.setFont(MipavUtil.font12B);
        deleteButton.setToolTipText("Delete Selected Slices");
        deleteButton.setBorderPainted(false);
        deleteButton.setRolloverEnabled(true);
        deleteButton.setRolloverIcon(MipavUtil.getIcon("deleteroll.gif"));
        deleteButton.setBorder(pressedBorder);
        deleteButton.setFocusPainted(false);
        deleteButton.setEnabled(false);
        tBar.add(deleteButton);

        extractButton = new JButton(MipavUtil.getIcon("extract.gif"));
        extractButton.addActionListener((ActionListener) this);
        extractButton.setActionCommand("ExtractSelection");
        extractButton.setFont(MipavUtil.font12B);
        extractButton.setToolTipText("Extract Selected Slices to a New Image");
        extractButton.setBorderPainted(false);
        extractButton.setRolloverEnabled(true);
        extractButton.setRolloverIcon(MipavUtil.getIcon("extractroll.gif"));
        extractButton.setBorder(pressedBorder);
        extractButton.setFocusPainted(false);
        extractButton.setEnabled(false);
        tBar.add(extractButton);

        JButton separatorCp = new JButton(MipavUtil.getIcon("separator.gif"));

        separatorCp.setBorderPainted(false);
        separatorCp.setFocusPainted(false);
        tBar.add(separatorCp);

        JButton selectAllButton = new JButton(MipavUtil.getIcon("selectall.gif"));

        selectAllButton.addActionListener((ActionListener) this);
        selectAllButton.setActionCommand("SelectAll");
        selectAllButton.setFont(MipavUtil.font12B);
        selectAllButton.setToolTipText("Select All");
        selectAllButton.setBorderPainted(false);
        selectAllButton.setRolloverEnabled(true);
        selectAllButton.setRolloverIcon(MipavUtil.getIcon("selectallrollover.gif"));
        selectAllButton.setBorder(pressedBorder);
        selectAllButton.setFocusPainted(false);
        tBar.add(selectAllButton);

        JButton unSelectAllButton = new JButton(MipavUtil.getIcon("unselectall.gif"));

        unSelectAllButton.addActionListener((ActionListener) this);
        unSelectAllButton.setActionCommand("SelectNone");
        unSelectAllButton.setFont(MipavUtil.font12B);
        unSelectAllButton.setToolTipText("Unselect All");
        unSelectAllButton.setBorderPainted(false);
        unSelectAllButton.setRolloverEnabled(true);
        unSelectAllButton.setRolloverIcon(MipavUtil.getIcon("unselectallroll.gif"));
        unSelectAllButton.setBorder(pressedBorder);
        unSelectAllButton.setFocusPainted(false);
        tBar.add(unSelectAllButton);

        JButton recordButton = new JButton(MipavUtil.getIcon("movie.gif"));

        recordButton.addActionListener(this);
        recordButton.setMargin(new Insets(0, 0, 0, 0));
        recordButton.setToolTipText("Capture to AVI");
        recordButton.setActionCommand("SaveImageAs");
        recordButton.setBorderPainted(false);
        recordButton.setRolloverEnabled(true);
        recordButton.setRolloverIcon(MipavUtil.getIcon("movieroll.gif"));
        recordButton.setBorder(pressedBorder);
        recordButton.setFocusPainted(false);
        tBar.add(recordButton);

        // add tBar to toolbarPanel
        toolbarPanel.add(tBar);

        getContentPane().add(toolbarPanel, BorderLayout.NORTH);
    }

    /**
     * Calculate the individual image panel size based on the current magnification of the image.
     */
    private void calcImagePanelSize() {

        // first get the width and height of the image
        imageWidth = imageA.getExtents()[0];
        imageHeight = imageA.getExtents()[1];

        // set the individual image panel size based on magnification and image size
        imagePanelSizeX = Math.round((magnification / 100.0f) * imageWidth);
        imagePanelSizeY = Math.round((magnification / 100.0f) * imageHeight);

        // System.out.println("calcImagePanelSize: image panel X,Y = " + imagePanelSizeX + ", " + imagePanelSizeY);

    } // end calcImagePanelSize()

    /**
     * Calculate the maximum page panel size based on the screen size, the size of the menubar and toolbars and whether
     * the columns or rows are independent.
     */
    private void calcMaxPagePanelSize() {

        // if the independent variable is the column, then
        // the max vertical size (y) is the screen size (vertical)
        // minus the heights of the menubar and toolbars.
        if (row_dependent == true) {
            maxPagePanelSizeX = xScreen - DEFAULT_XSCREEN_SPACE;
            maxPagePanelSizeY = yScreen - menuBar.getHeight() - toolbarPanel.getHeight();
        } // if the independent variable is the row, then

        // the max horizontal size (x) is the screen size (horizontal)
        // minus the widths of the menubar and toolbars.
        else { // rows are independent

            // maxPagePanelSizeX = xScreen - menuBar.getWidth() - toolbarPanel.getWidth();
            maxPagePanelSizeX = xScreen;
            maxPagePanelSizeY = yScreen - DEFAULT_YSCREEN_SPACE;
        }

    } // end calcMaxPagePanelSize()


    /**
     * Calculate the size of the page Panel. First the maximum size of the pagePanel must have been determined. Once the
     * maximum page panel size is determined, then the actual size may be reduced to nicely fit the current image panel
     * size. This will also determine how many images will fit into the page panel.
     */
    private void calcPagePanelSize() {

        // get the maximum page panel size
        calcMaxPagePanelSize(); // only changes if toolbar panel size changes (row_dependent)

        // if the independent variable is the column, then
        // use the preferences defined size for the number of
        // columns --- UNLESS, that width exceeds the max width.
        // if it does, then reduce the number of columns by 1
        // but there must always be at least 1 column
        //
        // Once the number of columns (and the width from them) is
        // determined, then find out how many rows will nicely fit
        // into the max height.  Then the actual panel height can
        // be determined.
        int tmpSize = 0;

        if (row_dependent == true) {

            int tmpCols = gridColumn;

            if (tmpCols < 1) {
                tmpCols = 1;
            } // force it to be valid

            while (tmpCols >= 1) {

                // get width from columns first
                tmpSize = (tmpCols * (imagePanelSizeX + gridSpacing + (2 * borderSize) + (2 * selectedBorderSize))) -
                          gridSpacing; // subtract one spacing, since spaces only occur between image panels

                if (tmpSize < maxPagePanelSizeX) { // we have a winner!
                    pagePanelSizeX = tmpSize;
                    gridColumn = tmpCols;

                    break;
                }

                // too big for max space, try reducing number of columns
                tmpCols--;
            } // end while loop on tmpCols

            // if tmpCols is 0, then it's just too big -- set columns to 1 and use it anyway
            if (tmpCols == 0) {
                gridColumn = 1;
                pagePanelSizeX = tmpSize;
            }

            // now that the width is taken care of, do the height
            // if too many rows were specified, then set them to 0 so they will be
            // automatically set
            int maxRows = (int) Math.ceil((double) numTotalSlices / (double) gridColumn);
            int tmpRows = maxRows;

            if (gridRow < tmpRows) {
                tmpRows = gridRow;
            }

            // System.out.println("calcPagePanelSize: gridRow = " + gridRow);
            while (tmpRows >= 1) {

                // get height from rows first
                tmpSize = (tmpRows * (imagePanelSizeY + gridSpacing + (2 * borderSize) + (2 * selectedBorderSize))) -
                          gridSpacing; // subtract one spacing, since spaces only occur between image panels

                if ((tmpSize < maxPagePanelSizeY) && (tmpRows <= maxRows)) {
                    pagePanelSizeY = tmpSize;
                    gridRow = tmpRows;

                    break;
                }

                // too big for max space, try reducing number of rows
                tmpRows--;

            } // end while loop on tmpRows

            // if tmpRows is 0, then it's just too big -- set rows to 1 and use it anyway
            if (tmpRows == 0) {
                gridRow = 1;
                pagePanelSizeY = tmpSize;
            }

            // the above was slightly off, since the gridSpacing only occurs 'between' image panels
            // plus, there may have been extra room .. so now set the actual page panel height
            pagePanelSizeY = (gridRow * (imagePanelSizeY + gridSpacing + (2 * borderSize) + (2 * selectedBorderSize))) -
                             gridSpacing; // subtract one spacing, since spaces only occur between image panels
        } else { // rows are independent

            int tmpRows = gridRow;

            if (tmpRows < 1) {
                tmpRows = 1;
            } // force it to be valid

            while (tmpRows >= 1) {

                // get height from rows first
                tmpSize = (tmpRows * (imagePanelSizeY + gridSpacing + (2 * borderSize) + (2 * selectedBorderSize))) -
                          gridSpacing; // subtract one spacing, since spaces only occur between image panels

                if (tmpSize <= maxPagePanelSizeY) {
                    pagePanelSizeY = tmpSize;
                    gridRow = tmpRows;

                    break;
                }

                // too big for max space, try reducing number of rows
                tmpRows--;
            } // end while loop on tmpRows

            // if tmpRows is 0, then it's just too big -- set rows to 1 and
            // use it anyway
            if (tmpRows == 0) {
                gridRow = 1;
                pagePanelSizeY = tmpSize;
            }

            // now that the height is taken care of, do the width
            // if too many columns were specified, then set them to 0 so they will be
            // automatically set
            int maxCols = (int) Math.ceil((double) numTotalSlices / (double) gridRow);
            // System.out.println("AR  max cols  = " + maxCols);

            int tmpCols = maxCols;

            if (gridColumn < tmpCols) {
                tmpCols = gridColumn;
            }

            while (tmpCols >= 1) {

                // get width from columns first
                tmpSize = (tmpCols * (imagePanelSizeX + gridSpacing + (2 * borderSize) + (2 * selectedBorderSize))) -
                          gridSpacing; // subtract one spacing, since spaces only occur between image panels

                if ((tmpSize <= maxPagePanelSizeX) && (tmpCols <= maxCols)) { // we have a winner!
                    pagePanelSizeX = tmpSize;
                    gridColumn = tmpCols;

                    break;
                }

                // too big for max space, try reducing number of columns
                tmpCols--;

            } // end while loop on tmpCols

            // if tmpCols is 0, then it's just too big -- set columns to 1 and use it anyway
            if (tmpCols == 0) {
                gridColumn = 1;
                pagePanelSizeX = tmpSize;
            }

            // the above was slightly off, since the gridSpacing only occurs 'between' image panels
            // plus, there may have been extra room .. so now set the actual page panel width
            pagePanelSizeX = (gridColumn *
                                  (imagePanelSizeX + gridSpacing + (2 * borderSize) + (2 * selectedBorderSize))) -
                             gridSpacing; // subtract one spacing, since spaces only occur between image panels

        } // end if row or column is independent

        // now that gridRow and gridColumn have been determined
        // the number of image per page can be found
        numTotalSlices = gridRow * gridColumn;

        // System.out.println("calcPagePanelSize: Row, Col = " + gridRow + ", " + gridColumn);
        // System.out.println("calcPagePanelSize: pagePanelSize X,Y " + pagePanelSizeX + ", " + pagePanelSizeY);
    } // end calcPagePanelSize()

    /**
     * Calculate the screen size in pixels.
     */
    private void calcScreenSize() {

        // get the screen size from the toolkit
        // take a little off the vertical to account for icon bars
        xScreen = Toolkit.getDefaultToolkit().getScreenSize().width;
        yScreen = Toolkit.getDefaultToolkit().getScreenSize().height - 50;

    } // end calcScreenSize()


    /**
     * This method creates the compound image border array for each image in the light box. The outer border is a
     * LineBorder that uses the border color defined in the settings. The inner border is also a Line Border, however
     * it's color depends on whether an image is selected or not (see the selectedImages vector).
     *
     * @return  DOCUMENT ME!
     */
    private CompoundBorder createImageBorder() {
        CompoundBorder border; // = new CompoundBorder[size];
        LineBorder outerBorder = new LineBorder(borderColor, borderSize);
        LineBorder innerBorder = new LineBorder(unselectedBorderColor, selectedBorderSize);

        border = new CompoundBorder(outerBorder, innerBorder);

        return border;
    } // end createImageBorder


    /**
     * Delete selected slices.
     */
    private void deleteSlices() {
        int slice;

        // System.out.println("selectedImages.size() = " + selectedImages.size());
        int[] sliceList = new int[selectedImages.size()];

        for (int k = 0; k < selectedImages.size(); k++) {
            sliceList[k] = Integer.parseInt(selectedImages.elementAt(k).toString());
        }

        // sort the int array
        for (int i = 1; i < sliceList.length; i++) {

            for (int j = i; (j > 0) && (sliceList[j] < sliceList[j - 1]); j--) {
                int temp = sliceList[j - 1];

                sliceList[j - 1] = sliceList[j];
                sliceList[j] = temp;
            }
        }

        // remove vector element from the slice list array TOP->DONW manner.
        for (int i = sliceList.length - 1; i >= 0; i--) {
            slice = sliceList[i];
            modelImageVector.remove(slice);
            componentImageVector.remove(slice);
            imageBorderVector.remove(slice);
            imagePanelVector.remove(slice);
        }

        selectedImages = new Vector();
        repaintFrame();
        setButtonStatus();
    }


    /**
     * Makes a series of ints, corresponding to a color string stored in the Mipav.preferences file which looks like a
     * color string defined in web pages ("RRGGBB"). and returns a java.awt.Color based on those values.
     *
     * <p>Call with extractColor(Preferences.getProperty("OneOfThoseColors"));</p>
     *
     * <p>if preferencesColorString is null, or incomplete, returns black.</p>
     *
     * @return  java.awt.Color
     *
     * @see     java.awt.Color
     *
     * @param   String  preferencesColorString-- this class pre-arranges the colors to be
     */
    private Color extractColor(String preferencesColorString) {
        int[] RGB = new int[3];
        Color prefColor;

        try {

            if (preferencesColorString != null) { // so long as the string is available, pull out expected color string.
                RGB[0] = Integer.parseInt(String.valueOf(preferencesColorString.toCharArray()[0]) +
                                          String.valueOf(preferencesColorString.toCharArray()[1]), 16);
                RGB[1] = Integer.parseInt(String.valueOf(preferencesColorString.toCharArray()[2]) +
                                          String.valueOf(preferencesColorString.toCharArray()[3]), 16);
                RGB[2] = Integer.parseInt(String.valueOf(preferencesColorString.toCharArray()[4]) +
                                          String.valueOf(preferencesColorString.toCharArray()[5]), 16);
            }
        } catch (Exception npe) {
            Preferences.debug("ViewJFrameLightBox: Color string was improper.  String was '" + preferencesColorString +
                              "'");

            // reset all three values with
            RGB[0] = 0;
            RGB[1] = 0;
            RGB[2] = 0;
        } finally {
            prefColor = new Color(RGB[0], RGB[1], RGB[2]);
        }

        return prefColor;
    }


    // ************************************************************************
    // ************************** Algorithm Invocations ***********************
    // ************************************************************************
    /**
     * Extract the camera capture frame into the resulting ModelImage model. The extraction takes the first slice in
     * each componentImageVector, then binding them into the resulting modelImage model.
     */
    private void extractImages() {
        int[] sliceList = new int[selectedImages.size()];
        float[] imageBuffer;
        int tDestOffset;
        int sliceArea;
        int[] destExtents = null;
        ModelImage destImage;

        // System.out.println("selectedImages.size() = " + selectedImages.size());
        for (int k = 0; k < selectedImages.size(); k++) {
            sliceList[k] = Integer.parseInt(selectedImages.elementAt(k).toString());
        }

        // sort the int array
        for (int i = 1; i < sliceList.length; i++) {

            for (int j = i; (j > 0) && (sliceList[j] < sliceList[j - 1]); j--) {
                int temp = sliceList[j - 1];

                sliceList[j - 1] = sliceList[j];
                sliceList[j] = temp;
            }
        }

        srcImage = imageA;
        sliceArea = srcImage.getSliceSize(); // one slice has sliceArea number of pixels

        if (srcImage.getNDims() == 3) {

            // destination image extents (length in a particular direction)
            // if user only extracts 1 slice, make dest a 2D image:
            if (sliceList.length == 1) {
                destExtents = new int[2];
                destExtents[0] = srcImage.getExtents()[0];
                destExtents[1] = srcImage.getExtents()[1];
            } // else dest will have volume, so make it a 3D image:
            else if (sliceList.length > 1) {
                destExtents = new int[3];
                destExtents[0] = srcImage.getExtents()[0];
                destExtents[1] = srcImage.getExtents()[1];
                destExtents[2] = sliceList.length;
            }
        }

        destImage = new ModelImage(srcImage.getType(), destExtents, srcImage.getImageName() + "Extract");


        // create  <FileInfoBase fileInfoBuffer;> buffer; may be of type FileInfoDicom
        try {

            if (srcImage.isColorImage()) {
                imageBuffer = new float[4 * sliceArea];
            } else {
                imageBuffer = new float[sliceArea];
            }

            // buildProgressBar(srcImage.getImageName(), "Extracting Selected Slices...", 0, 100);
        } catch (OutOfMemoryError e) {
            imageBuffer = null;
            System.gc();

            return;
        }

        tDestOffset = 0;
        buildProgressBar(srcImage.getImageName(), "Extracting Selected Slices...", 0, 100);

        // make a location & view the progressbar; make length & increment of progressbar.
        initProgressBar();

        for (int i = 0; i < sliceList.length; i++) {

            // let user know something is happening by updating the progressbar
            if (isProgressBarVisible()) {
                progressBar.updateValue(Math.round((float) (i) / (sliceList.length - 1) * 100), false);
            }

            try {

                // try copying the zSrc slice out of srcImage, making it the zDest in destImage
                ModelImage tempImage = ((ViewJComponentEditImage) (componentImageVector.elementAt(sliceList[i])))
                                           .getImageA();

                if (tempImage.isColorImage()) {
                    imageBuffer = new float[4 * sliceArea];
                } else {
                    imageBuffer = new float[sliceArea];
                }

                if (tempImage.isColorImage()) {
                    tempImage.exportData(0, 4 * sliceArea, imageBuffer);
                    destImage.importData(tDestOffset + (i * 4 * sliceArea), imageBuffer, false);
                } else {
                    tempImage.exportSliceXY(0, imageBuffer);
                    destImage.importData(tDestOffset + (i * sliceArea), imageBuffer, false);
                }
            } catch (IOException error) {
                MipavUtil.displayError(("ViewJFrameCamera: " + error.getMessage()));

                return;
            }
        }

        disposeProgressBar();
        new ViewJFrameImage(destImage, null, new Dimension(610, 200));
    }

    /**
     * Initializes the buffers that hold the image data.
     */
    private void initBuffers() {

        int colorFactor = 4;

        imageBufferA = new float[colorFactor * imageA.getSliceSize()];
        pixBuffer = new int[imageA.getSliceSize()];
    } // end initBuffers()

    /**
     * Only allow resizing the frame horizontally. Vertial not allow. Dr. Ben's contribution.
     *
     * @param  frame  DOCUMENT ME!
     */
    private void lockSize(final JFrame frame) {

        // Ensures user cannot resize frame to be smaller than frame is right now.
        final int origX = frame.getSize().width;
        final int origY = frame.getSize().height;

        frame.addComponentListener(new java.awt.event.ComponentAdapter() {
                public void componentResized(ComponentEvent event) {
                    int gridColumnLimit, availColumn, widthFrame, gridColumnMinimized;
                    int frameWidthLimit = (gridColumn *
                                               (imagePanelSizeX + gridSpacing + (2 * borderSize) +
                                                    (2 * selectedBorderSize))) - gridSpacing;

                    gridColumnLimit = gridColumn;
                    frameStartLocation = frame.getLocation();

                    if ((frameStartLocation.getX() + frameWidthLimit) >= xScreen) {
                        gridColumnLimit = (int) ((double) (xScreen - frameStartLocation.getX()) /
                                                     (double) imagePanelSizeX) - 1;
                    }

                    gridColumnMinimized = (int)
                                              Math.round((double) frame.getSize().width /
                                                             (double) (imagePanelSizeX + gridSpacing +
                                                                           (2 * borderSize) + (2 * selectedBorderSize)));
                    availColumn = Math.min(gridColumnLimit, gridColumn);
                    availColumn = Math.min(gridColumnMinimized, availColumn);

                    if (availColumn <= 1) {
                        availColumn = 2;
                    }

                    // System.out.println("gridColumnMinimized = " + gridColumnMinimized + " gridColumn = " +
                    // gridColumn + " gridColumnLimit = " + gridColumnLimit);
                    widthFrame = (int)
                                     Math.round((double) availColumn *
                                                    (double) (imagePanelSizeX + gridSpacing + (2 * borderSize) +
                                                                  (2 * selectedBorderSize))) + gridSpacing;
                    frame.setSize(widthFrame, origY);
                }
            });
    }

    /**
     * Updating image for each componentImage vector element.
     */
    private void repaintFrame() {
        updatePanelLayout();
        frameWidth = pagePanelSizeX + getInsets().left + getInsets().right;
        frameHeight = pagePanelSizeY + getInsets().top + getInsets().bottom + getJMenuBar().getHeight() +
                      toolbarPanel.getHeight();
        setSize(frameWidth, frameHeight);

        updatePagePanel();

        for (int j = 0; j < componentImageVector.size(); j++) {
            (((ViewJComponentEditImage) componentImageVector.elementAt(j))).setStringOverride(String.valueOf(j + 1));
            (((ViewJComponentEditImage) componentImageVector.elementAt(j))).show(0, 0, null, null, true, -1);
        }
    }

    /**
     * Saves screen graps to the animation frame and uses the animation frame to save the images to AVI format.
     */
    private void saveAVI() {

        float[] imageBuffer;
        int tDestOffset;
        int sliceArea;
        int[] destExtents = null;
        ModelImage destImage;

        /************************** Select all images ***************************/
        for (int i = 0; i < numTotalSlices; i++) {

            // only add to list if it's not already there
            if (!selectedImages.contains(Integer.toString(i))) {
                selectedImages.addElement(Integer.toString(i));
            }
        }

        /************************ Extract image **************************************/
        int[] sliceList = new int[selectedImages.size()];

        // System.out.println("selectedImages.size() = " + selectedImages.size());
        for (int k = 0; k < selectedImages.size(); k++) {
            sliceList[k] = Integer.parseInt(selectedImages.elementAt(k).toString());
        }

        // sort the int array
        for (int i = 1; i < sliceList.length; i++) {

            for (int j = i; (j > 0) && (sliceList[j] < sliceList[j - 1]); j--) {
                int temp = sliceList[j - 1];

                sliceList[j - 1] = sliceList[j];
                sliceList[j] = temp;
            }
        }

        srcImage = imageA;
        sliceArea = srcImage.getSliceSize(); // one slice has sliceArea number of pixels

        if (srcImage.getNDims() == 3) {

            // destination image extents (length in a particular direction)
            // if user only extracts 1 slice, make dest a 2D image:
            if (sliceList.length == 1) {
                destExtents = new int[2];
                destExtents[0] = srcImage.getExtents()[0];
                destExtents[1] = srcImage.getExtents()[1];
            } // else dest will have volume, so make it a 3D image:
            else if (sliceList.length > 1) {
                destExtents = new int[3];
                destExtents[0] = srcImage.getExtents()[0];
                destExtents[1] = srcImage.getExtents()[1];
                destExtents[2] = sliceList.length;
            }
        }

        destImage = new ModelImage(srcImage.getType(), destExtents, srcImage.getImageName() + "Extract");


        // create  <FileInfoBase fileInfoBuffer;> buffer; may be of type FileInfoDicom
        try {

            if (srcImage.isColorImage()) {
                imageBuffer = new float[4 * sliceArea];
            } else {
                imageBuffer = new float[sliceArea];
            }

            // buildProgressBar(srcImage.getImageName(), "Extracting Selected Slices...", 0, 100);
        } catch (OutOfMemoryError e) {
            imageBuffer = null;
            System.gc();

            return;
        }

        tDestOffset = 0;
        buildProgressBar(srcImage.getImageName(), "Extracting Selected Slices...", 0, 100);

        // make a location & view the progressbar; make length & increment of progressbar.
        initProgressBar();

        for (int i = 0; i < sliceList.length; i++) {

            // let user know something is happening by updating the progressbar
            if (isProgressBarVisible()) {
                progressBar.updateValue(Math.round((float) (i) / (sliceList.length - 1) * 100), false);
            }

            try {

                // try copying the zSrc slice out of srcImage, making it the zDest in destImage
                ModelImage tempImage = ((ViewJComponentEditImage) (componentImageVector.elementAt(i))).getImageA();

                if (tempImage.isColorImage()) {
                    imageBuffer = new float[4 * sliceArea];
                } else {
                    imageBuffer = new float[sliceArea];
                }

                if (tempImage.isColorImage()) {
                    tempImage.exportData(0, 4 * sliceArea, imageBuffer);
                    destImage.importData(tDestOffset + (i * 4 * sliceArea), imageBuffer, false);
                } else {
                    tempImage.exportSliceXY(0, imageBuffer);
                    destImage.importData(tDestOffset + (i * sliceArea), imageBuffer, false);
                }
            } catch (IOException error) {
                MipavUtil.displayError(("ViewJFrameCamera: " + error.getMessage()));

                return;
            }
        }

        disposeProgressBar();
        selectedImages.removeAllElements();

        /************************* Animate Frame *************************/
        JDialogAnimate dialogAnimate;

        if (imageA.getNDims() > 4) {
            MipavUtil.displayError(" Animate cannot handle images with more than 4 dimensions");
        }

        if (imageA.getAnimateFrame() == null) {

            dialogAnimate = new JDialogAnimate(this, destImage, LUTa, null, null, false);
            dialogAnimate.animate();
            dialogAnimate.invokeSaveImgAs(5);
        }
    }

    /**
     * Sets the color of the image borders.
     *
     * @param  col  color of the image borders
     */
    private void setBorderColor(Color col) {

        if ((borderColor != null) && borderColor.equals(col)) {
            return;
        }

        borderColor = col;
    }

    /**
     * Sets the image border size.
     *
     * @param  size  thichness of the image border.
     */
    private void setBorderSize(int size) {

        if ((size < 0) || (size > MAX_GRID_BORDER)) {
            return;
        }

        if (size == borderSize) {
            return;
        }

        borderSize = size;
    }


    /**
     * Sets the background color of the panel that is in the scrollpane.
     *
     * @param  col  the color the background of the panel that is in the scrollpane
     */
    private void setGridColor(Color col) {

        if ((gridColor != null) && gridColor.equals(col)) {
            return;
        }

        gridColor = col;
    }


    /**
     * This method is called when there is a change to the light box column number--in the limiting case. It ensures
     * that the column value is within bounds.
     *
     * @param  col  the number of columns to be used to display the images
     */
    private void setGridColumn(int col) {

        if (col > MAX_GRID_COL) { // check bounds
            col = MAX_GRID_COL;
        } else {
            gridColumn = col;
        }
    }

    /**
     * Sets the distance between adjacent images. verifies that the width value is within bounds. If width is outside of
     * bounds, method fails quietly.
     *
     * @param  width  the width of space between images.
     */
    private void setGridSpacing(int width) {

        if ((width < 0) || (width > MAX_GRID_SIZE)) {
            return;
        }

        if (width == gridSpacing) {
            return;
        }

        gridSpacing = width;
    }

    /**
     * Sets the color of the selected image borders.
     *
     * @param  col  color of the selected image borders
     */
    private void setSelectedBorderColor(Color col) {

        if ((selectedBorderColor != null) && selectedBorderColor.equals(col)) {
            return;
        }

        selectedBorderColor = col;
    }

    /**
     * Sets the selected image border size.
     *
     * @param  size  thichness of the selected image border.
     */
    private void setSelectedBorderSize(int size) {

        if ((size < 0) || (size > MAX_SELECTED_GRID_BORDER)) {
            return;
        }

        if (size == selectedBorderSize) {
            return;
        }

        selectedBorderSize = size;
    }

    /**
     * Setup the light box view. A scrollpane is added to the frame. A panel with a grid layout is added to the
     * scrollpane. Bordered panels the size of the image are added to each position of the grid. Lastly image slices are
     * added to each bordered panel.
     */
    private void setupCameraStorage() {

        try {

            // set up the page
            pagePanel = new JPanel(new GridLayout(gridRow, gridColumn, gridSpacing, gridSpacing));
            pagePanel.setBackground(gridColor);

            pagePanel.setPreferredSize(new Dimension(pagePanelSizeX, pagePanelSizeY));
            pagePanel.setBounds(0, 0, pagePanelSizeX, pagePanelSizeY);

            // set the frame size
            frameWidth = pagePanelSizeX + getInsets().left + getInsets().right;
            frameHeight = pagePanelSizeY + getInsets().top + getInsets().bottom + getJMenuBar().getHeight() +
                          toolbarPanel.getHeight();

            setSize(frameWidth, frameHeight);

        } catch (OutOfMemoryError e) {
            System.gc();
            throw (e);
        }

        try {

            // build bordered panel and put image slice inside
            buildPage(); // sets up the image panels on the page -- no contents yet

            // load the images for the current page
            loadPage();

        } catch (OutOfMemoryError e) {

            if (componentImageVector != null) {

                for (int i = 0; i < numTotalSlices; i++) {

                    if (((ViewJComponentEditImage) (componentImageVector.elementAt(i))) != null) {
                        ((ViewJComponentEditImage) (componentImageVector.elementAt(i))).dispose(false);
                    }

                    // componentImage[i] = null;
                    componentImageVector.remove(i);

                    // imagePanel[i]     = null;
                    imagePanelVector.remove(i);

                    // imageBorder[i]    = null;
                    imageBorderVector.remove(i);
                }
            }

            System.gc();
            throw (e);
        }

        // getContentPane().add(pagePanel);
        pageScrollPanel = new JScrollPane(pagePanel, JScrollPane.VERTICAL_SCROLLBAR_NEVER,
                                          JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);

        int minX = (3 * (imagePanelSizeX + gridSpacing + (2 * borderSize) + (2 * selectedBorderSize))) - gridSpacing;

        int minY = (1 * (imagePanelSizeY + gridSpacing + (2 * borderSize) + (2 * selectedBorderSize))) - gridSpacing;

        pageScrollPanel.setMinimumSize(new Dimension(minX, minY));
        getContentPane().add(pageScrollPanel);


        updateImages(true);
        pack();
        lockSize(this);
        setSize(frameWidth, frameHeight);
        this.setLocation(xScreen - (frameWidth + 50), 0);
        setTitle();
        toBack();
        setVisible(true);

    } // end setupLightBox()

    /**
     * Updates the image selection list by toggling the selection of the Z(3rd dimension) plane and the time (4th
     * dimesion) volume.
     *
     * @param  plane         image plane that is to displayed
     * @param  applyToRange  the volume in which the plane the is to be displayed from
     */
    private void updateImageSelection(int plane, boolean applyToRange) {

        // keep track of this slice and the last one selected
        // this is needed for ranges of selections
        lastSelectedSlice = currentSelectedSlice;
        currentSelectedSlice = plane;

        // if applying image selection to a range, then the assumption
        // is that the entire (inclusive) range is being selected -- never
        // deselected
        if (applyToRange) {
            int minNum = (currentSelectedSlice < lastSelectedSlice) ? currentSelectedSlice : lastSelectedSlice;
            int maxNum = (currentSelectedSlice > lastSelectedSlice) ? currentSelectedSlice : lastSelectedSlice;

            for (int slice = minNum; slice <= maxNum; slice++) {

                // only add to list if it's not already there
                if (!selectedImages.contains(Integer.toString(slice)) && (slice < numTotalSlices)) {
                    selectedImages.addElement(Integer.toString(slice));
                }

                // only update the borders of visible slices
                int pageIndex = slice;

                if ((pageIndex >= 0) && (pageIndex < numTotalSlices)) {

                    // update the borders
                    updateImageBorder(pageIndex);
                }
            }
        } else { // otherwise, no range is selected -- simple select and deselect

            // update the inner border of the border of this slice
            // based on whether it's now on or off

            if (selectedImages.contains(Integer.toString(plane)) && (plane < numTotalSlices)) {
                selectedImages.removeElement(Integer.toString(plane));
            } else if (plane < numTotalSlices) {
                selectedImages.addElement(Integer.toString(plane));
            }

            // only update the borders of visible slices
            int pageIndex = plane;

            if ((pageIndex >= 0) && (pageIndex < numTotalSlices)) {
                updateImageBorder(pageIndex);
            }

        } // endif range is selected

        setButtonStatus();
        System.gc();

    } // end updateImageSelection()


    /**
     * Refill the page panel with imagePanelVector.
     */
    private void updatePagePanel() {
        int[] extents;

        extents = new int[3];
        extents[0] = Math.round(imageA.getExtents()[0]);
        extents[1] = Math.round(imageA.getExtents()[1]);
        extents[2] = Math.round(imageA.getExtents()[2]);
        pagePanel.removeAll();

        for (int i = 0; i < imagePanelVector.size(); i++) {
            pagePanel.add((JPanel) (imagePanelVector.elementAt(i)));
        }

    }

    /**
     * Updating frame panel layout.
     */
    private void updatePanelLayout() {
        numTotalSlices = modelImageVector.size();
        gridRow = 1;
        gridColumn = modelImageVector.size();
        pagePanelSizeX = (gridColumn * (imagePanelSizeX + gridSpacing + (2 * borderSize) + (2 * selectedBorderSize))) -
                         gridSpacing;
        numTotalSlices = gridRow * gridColumn;

        GridLayout gLayout = new GridLayout(gridRow, gridColumn, gridSpacing, gridSpacing);

        pagePanel.setLayout(gLayout);
        pagePanel.setPreferredSize(new Dimension(pagePanelSizeX, pagePanelSizeY));
        pagePanel.setBounds(0, 0, pagePanelSizeX, pagePanelSizeY);
    }

}
