package gov.nih.mipav.view;



import gov.nih.mipav.model.file.*;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOIManagerInterface;


import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;
import javax.swing.border.*;




/**
 * This class builds a "light box" view of a 3D or 4D dataset. In addition, a 2nd image can be overlayed and an alpha
 * value can be adjusted to control the amount of each image that is displayed. The user can control how the images are
 * tiled by specifing the number of row or cols, border thickness and size, and magnification. The options can changed
 * using the dialog accessed via the options menu.
 */

public class ViewJFrameLightBox extends ViewJFrameBase implements ItemListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -4014825600247045462L;

    /**
     * default values indicating minimum, maximum sizes are in PIXELS on a side for a magnification. this is to fix the
     * magnification problems of very small images for which the arbitrary limits of lightbox image sizes of 25%-75% of
     * the full size of an image. in practice the default max will reference the side with greatest magnitude ...
     */
    //private static final int DEFAULT_IMAGE_SIZE = 128;

    /** DOCUMENT ME! */
    private static final int DEFAULT_IMAGE_MAX = 256;

    /** DOCUMENT ME! */
    private static final int DEFAULT_IMAGE_MIN = 96;

    /** DOCUMENT ME! */
    private static final int DEFAULT_XSCREEN_SPACE = 128;

    /** DOCUMENT ME! */
    private static final int DEFAULT_YSCREEN_SPACE = 64;

    /**
     * Default row is dependent, and column independent.
     *
     * <p>the default is the number of rows is the dependent variable, and is dependent apon the total number of images
     * and the number of columns.</p>
     */
    public static final boolean DEFAULT_DEPENDENT_ROW = true;

    /** Minimum amount of magnification in the light-box. */
    public static final float MIN_MAGNIFICATION = 25;

    /** Maximum amount of magnification in the light-box. */
    public static final float MAX_MAGNIFICATION = 75;

    /** Maximum row value (when it is the independent variable). */
    public static final int MAX_GRID_ROW = 10;

    /** Maximum column value (when it is the independent variable). */
    public static final int MAX_GRID_COL = 10;

    /** Maximum spacing in-between images in the light-box. */
    public static final int MAX_GRID_SIZE = 20;

    /** Maximum spacing of the border surrounding images in the light-box. */
    public static final int MAX_GRID_BORDER = 10;

    /** Maximum spacing of the selection border surrounding images in the light-box. */
    public static final int MAX_SELECTED_GRID_BORDER = 5;

    /** Elements of tBar. */
    private static JToggleButton[] toggleArray = new JToggleButton[2];

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JButton activeSliceButton;

    /** Color for the line border surrounding each image in the light-box. */
    private Color borderColor;

    /** Spacing for the line border surrounding each image in the light-box. */
    private int borderSize;

    /** DOCUMENT ME! */
    private ViewJComponentEditImage[] componentImage; // not the number of total slices

    /** DOCUMENT ME! */
    private ViewControlsImage controls;

    /** Page information. */
    private int currentPage = 0;

    /** DOCUMENT ME! */
    private int currentSelectedSlice = 0;

    /** Current info -- this slice info refers to the *real* slice number, not the page index for a slice on a page. */
    private int currentSlice = 0; // current highlighted slice

    /** DOCUMENT ME! */
    private int currentTSlice = 0;

    /** DOCUMENT ME! */
    private JButton deleteButton;

    /** DOCUMENT ME! */
    private JMenuItem deleteSelection;

    /** DOCUMENT ME! */
    private JButton extractButton;

    /** DOCUMENT ME! */
    private JMenuItem extractSelection;
    
    /** DOCUMENT ME! */
    private JButton interpolateShapesButton;

    /** DOCUMENT ME! */
    private JButton firstPageButton;

    /** DOCUMENT ME! */
    private JMenuItem firstPageMenuItem;

    /** DOCUMENT ME! */
    private int frameHeight;

    /** Frame information. */
    private int frameWidth;

    /** DOCUMENT ME! */
    private JTextField goToSliceText;

    /** Color for the region in-between the images in the light-box. */
    private Color gridColor;

    /** DOCUMENT ME! */
    private int gridColumn = 0; // #of widths of image the viewing area should display.

    /**
     * Value for the number of rows (number of images along the y-axis) and columns (number of images along the x-axis)
     * in the light-box.
     *
     * <p>Note that both row and column set to 1 cannot make sense, but that one or the other will be the 'dependent
     * variable' as defined by the row_dependent boolean var.</p>
     *
     * <p>A value of 0 means that the dependent variable will automatically be sized to its maximum size that can fit
     * nicely on the screen.</p>
     */
    private int gridRow = 0; // guess that this is same as below, but in other direction:

    /** Spacing between images in the light-box. */
    private int gridSpacing;
    
    /* Increment between displayed slices in the light-box */
    private int increment;

    /** DOCUMENT ME! */
    private ButtonGroup group = new ButtonGroup();

    /** DOCUMENT ME! */
    private CompoundBorder[] imageBorder; // these arrays are sized for the

    /** DOCUMENT ME! */
    private float[] imageBufferA;

    /** DOCUMENT ME! */
    private float[] imageBufferB;

    /** DOCUMENT ME! */
    private ViewJFrameImage imageFrame;

    /** DOCUMENT ME! */
    private int imageHeight = 128;

    /** DOCUMENT ME! */
    private JPanel[] imagePanel; // number of *visible* slices,

    /** Per num visible images information. */
    private int imagePanelSizeX, imagePanelSizeY;

    /** DOCUMENT ME! */
    private int imageWidth = 128;

    /** DOCUMENT ME! */
    private JButton lastPageButton;

    /** DOCUMENT ME! */
    private JMenuItem lastPageMenuItem;

    /** DOCUMENT ME! */
    private int lastSelectedSlice = 0; // keep track of the last selected slice

    /** DOCUMENT ME! */
    private JDialogLightBox lightBoxOptions;

    /** DOCUMENT ME! */
    private ModelLUT LUTa;

    /** DOCUMENT ME! */
    private ModelLUT LUTb;

    /** DOCUMENT ME! */
    private float magMax = MAX_MAGNIFICATION;

    /** DOCUMENT ME! */
    private float magMin = MIN_MAGNIFICATION;

    /** Magnification of images in the light-box (mag is a percentage). */
    private float magnification = 45;

    /** DOCUMENT ME! */
    private int maxPagePanelSizeX = 0;

    /** DOCUMENT ME! */
    private int maxPagePanelSizeY = 0;

    /** Variables for the menubar. */
    private JMenuBar menuBar;

    /** elements of paging toolbar. */
    private JButton nextPageButton;

    /** DOCUMENT ME! */
    private JMenuItem nextPageMenuItem;

    /** DOCUMENT ME! */
    private int numPages = 1;

    /** DOCUMENT ME! */
    private int numTotalSlices = 0;

    /** DOCUMENT ME! */
    private int numTSlices = 1;

    /** DOCUMENT ME! */
    private int numVisibleSlices = 0;

    /** DOCUMENT ME! */
    private JPanel pagePanel;

    /** DOCUMENT ME! */
    private int pagePanelSizeX = 0;

    /** DOCUMENT ME! */
    private int pagePanelSizeY = 0;

    /** DOCUMENT ME! */
    private JToolBar pagingTBar;

    /** DOCUMENT ME! */
    private int[] pixBuffer;

    /** DOCUMENT ME! */
    private int[] pixBufferB;

    /** DOCUMENT ME! */
    private JButton prevPageButton;

    /** DOCUMENT ME! */
    private JMenuItem prevPageMenuItem;

    /** DOCUMENT ME! */
    private JButton repaintButton;

    /** DOCUMENT ME! */
    private float resolutionX = 1;

    /** DOCUMENT ME! */
    private float resolutionY = 1;

    /**
     * Indicates which--row or column--should be dependent on the other.
     *
     * <p>the default is the number of rows is the dependent variable, and is dependent apon the total number of images
     * and the number of columns. Don't confuse the reading to be "This display is (t/f) dependent on the row." It
     * -could- be read as "The display is a column-independent (t/f) matrix".</p>
     */
    private boolean row_dependent = true; // number of rows (y-axis) is dependent on # images & # cols.

    /** Color for the line border surrounding each selected image in the light-box. */
    private Color selectedBorderColor;

    /** Spacing for the selected line border surrounding each image in the light-box. */
    private int selectedBorderSize;

    /**
     * The selectedImages contains the *real* slice numbers -- not the index of a slice on a page. So the maximum number
     * in the vector will be numTotalSlices - 1.
     */
    private Vector<String> selectedImages = new Vector<String>();


    /**
     * The selected Time Slices vector contains the real slice number and time slice number in the format (slice
     * #).(time slice #) the maximum number in the vector will be numTotalSlices * t.
     */
    private Vector<String> selectedTimeSlices = new Vector<String>();

    /** DOCUMENT ME! */
    private JCheckBoxMenuItem selectIndividualTSlices;

    /** DOCUMENT ME! */
    private boolean singleTSlice = false;


    /** DOCUMENT ME! */
    private ModelImage srcImage = null;

    /** DOCUMENT ME! */
    private JToolBar tBar;

    /** Variables for the toolbar. */
    private JPanel toolbarPanel;

    /** DOCUMENT ME! */
    private Color unselectedBorderColor = Color.black;

    /** DOCUMENT ME! */
    private JCheckBoxMenuItem updatePaint;

    /** DOCUMENT ME! */
    private int xScreen, yScreen;
    

    protected VOIManagerInterface voiManager;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Builds the initial lightbox view.
     *
     * @param  imgFrame     the parent image frame
     * @param  _frameTitle  title of the frame
     * @param  _imageA      model of image A
     * @param  _LUTa        Lookup table used for image A
     * @param  _imageB      model of image B
     * @param  _LUTb        Lookup table used for image B
     * @param  resX         x dimension image resolution
     * @param  resY         y dimension image resolution
     * @param  loc          location to display frame of image - NOT USED
     * @param  _controls    controls for the frame
     */
    public ViewJFrameLightBox(ViewJFrameImage imgFrame, String _frameTitle, ModelImage _imageA, ModelLUT _LUTa,
                              ModelImage _imageB, ModelLUT _LUTb, float resX, float resY, Dimension loc,
                              ViewControlsImage _controls, VOIManagerInterface voiManager) {
    	
        super(_imageA, _imageB);
        try {
            setIconImage(MipavUtil.getIconImage("lightbox_16x16.gif"));
        } catch (Exception e) {
            e.printStackTrace();
        }

        setLUTs(_LUTa, _LUTb); // sets the LUTS in ViewJFrameBase

        // if there's no image, then nothing to display
        if ((imgFrame == null) || (imageA == null)) {
            return;
        }

        // initialize variables from parameters
        imageFrame = imgFrame;
        controls = _controls;
        this.voiManager = voiManager;
        resolutionX = resX;
        resolutionY = resY;

        setResizable(true);

        // initialize the selectedImages vector
        selectedImages.removeAllElements();
        selectedTimeSlices.removeAllElements();

        // set the mag's max/min percentages so that magnification can be properly set
        setMagMax();
        setMagMin();

        // all defaults for the lightbox have been moved to the defaults in the
        // Preferences class.  So can just get the initial values from the properties.
        setRowDependent(Boolean.valueOf(Preferences.getProperty(Preferences.PREF_LB_ROW_DEPENDENT)).booleanValue());
        setGridRow(Integer.parseInt(Preferences.getProperty(Preferences.PREF_LB_GRID_ROW)));
        setGridColumn(Integer.parseInt(Preferences.getProperty(Preferences.PREF_LB_GRID_COL)));
        setGridSpacing(Integer.parseInt(Preferences.getProperty(Preferences.PREF_LB_GRID_SIZE)));
        setIncrement(Integer.parseInt(Preferences.getProperty(Preferences.PREF_LB_INCREMENT)));
        setGridColor(extractColor(Preferences.getProperty(Preferences.PREF_LB_GRID_COLOR)));
        setBorderSize(Integer.parseInt(Preferences.getProperty(Preferences.PREF_LB_BORDER_SIZE)));
        setBorderColor(extractColor(Preferences.getProperty(Preferences.PREF_LB_BORDER_COLOR)));
        setSelectedBorderColor(extractColor(Preferences.getProperty(Preferences.PREF_LB_SELECTED_BORDER_COLOR)));
        setSelectedBorderSize(Integer.parseInt(Preferences.getProperty(Preferences.PREF_LB_SELECTED_BORDER_SIZE)));
        setMagnification(Float.parseFloat(Preferences.getProperty(Preferences.PREF_LB_MAG)));

        String singleTString = Preferences.getProperty("LightBoxIndividualTSlice");

        if ((singleTString != null) && (imageA.getNDims() > 3)) {
            singleTSlice = new Boolean(singleTString).booleanValue();
        }


        try {

            // get the number of slices and numTSlices from imageA
            numTotalSlices =  1 + (imageA.getExtents()[2] - 1)/increment;

            if ((gridRow * gridColumn) > numTotalSlices) {

                if (row_dependent == true) {
                    gridRow = 1;
                    gridColumn = numTotalSlices;
                } else {
                    gridRow = numTotalSlices;
                    gridColumn = 1;
                }
            }


            if (imageA.getNDims() == 4) {
                numTSlices = imageA.getExtents()[3];
            } else if ((imageB != null) && (imageB.getNDims() == 4)) {
                numTSlices = imageB.getExtents()[3];
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

            // once the framework for the page is in place,
            // the number of pages can be determined
            // then the page can be loaded with images

            // calculate the number of pages based on the numTotalSlices and
            // numVisibleSlices
            numPages = calcNumPages(); // changes if gridRow, gridColumn, or numTotalSlices changes

            // initialize the buffers that hold the image data
            initBuffers();

            // set the current page to the first page
            currentPage = 0;

            // set the current slice to the middle slice
            currentSlice = numTotalSlices / 2;

            // set the state of the paging toolbar buttons and menu items
            updatePagingToolbar();

            // create the options dialog and make sure it's initialized
            lightBoxOptions = new JDialogLightBox(this);
            lightBoxOptions.setValues(row_dependent, gridRow, gridColumn);

            setupLightBox(_LUTa, _LUTb);
            
            // add listeners to pick up changes in images
            imageA.addImageDisplayListener(this);

            if (imageB != null) {
                imageB.addImageDisplayListener(this);
            }

            // User interface will have list of frames
            userInterface.regFrame(this);
            addComponentListener(this);
            resize();
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
        // Object source = event.getSource();

        if (event.getActionCommand().equals("SelectAll")) {
            Preferences.debug("Lightbox: Select All \n");
            selectAll();
        } else if (event.getActionCommand().equals("SelectNone")) {
            Preferences.debug("Lightbox: Select None \n");
            selectNone();
        } else if (event.getActionCommand().equals("SelectInvert")) {
            Preferences.debug("Lightbox: Select Invert \n");
            invertSelections(true);
        } else if (event.getActionCommand().equals("DeleteSelection")) {
            Preferences.debug("Lightbox: Delete selections \n");
            callRemoveAlgorithm();
        } else if (event.getActionCommand().equals("ExtractSelection")) {
            Preferences.debug("Lightbox: Extract selections \n");
            callExtractAlgorithm();
        } else if (event.getActionCommand().equals("FirstPage")) {
            Preferences.debug("Lightbox: First page \n");
            currentPage = 0;

            // update the display
            updateImageBorders();

            boolean lastState = updatePaint.getState();
            updatePaint.setState(true);
            updateImages(true);
            updatePaint.setState(lastState);

            // update the toolbar and menu items
            updatePagingToolbar();
        } else if (event.getActionCommand().equals("PreviousPage")) {
            Preferences.debug("Lightbox: Previous page \n");

            if (currentPage == 0) {
                return;
            }

            currentPage--;

            // update the display
            updateImageBorders();

            boolean lastState = updatePaint.getState();
            updatePaint.setState(true);
            updateImages(true);
            updatePaint.setState(lastState);


            // update the toolbar and menu items
            updatePagingToolbar();
        } else if (event.getActionCommand().equals("NextPage")) {
            Preferences.debug("Lightbox: Next page \n");

            if (currentPage == (numPages - 1)) {
                return;
            }

            currentPage++;

            boolean lastState = updatePaint.getState();
            updatePaint.setState(true);
            updateImages(true);
            updatePaint.setState(lastState);
            repaintButton.setEnabled(false);
            updateImageBorders();

            // update the toolbar and menu items
            updatePagingToolbar();

        } else if (event.getActionCommand().equals("LastPage")) {
            Preferences.debug("Lightbox: Last page \n");
            currentPage = numPages - 1;

            // update the display
            updateImageBorders();

            boolean lastState = updatePaint.getState();
            updatePaint.setState(true);
            updateImages(true);
            updatePaint.setState(lastState);

            // update the toolbar and menu items
            updatePagingToolbar();
        } else if (event.getActionCommand().equals("ActiveSlice")) {
            Preferences.debug("Lightbox: Active slice \n");
            setPageToCurrentSlice();
        } else if (event.getActionCommand().equals("GoToSlice")) {
            Preferences.debug("Lightbox: Go to slice \n");

            // get the slice from the text box
            String tmpStr = goToSliceText.getText();
            int newSlice = currentSlice + 1;

            try {
                newSlice = Integer.valueOf(tmpStr).intValue();
            } catch (NumberFormatException nfe) {
                MipavUtil.displayError("Please enter a number from 1 to " + numTotalSlices);

                return;
            }

            // subtract 1 since the user see the range 1 - numTotalSlices, inclusive
            // and range in code is 0 to numTotalSlices-1
            setPageToSlice(newSlice - 1);
        } else if (event.getActionCommand().equals("Settings")) {

            try {
                lightBoxOptions.setVisible(true);
            } catch (OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: FrameLightBox");
            }
        } else if (event.getActionCommand().equals("SaveSettings")) {
            storeToDefaults();
        } else if (event.getActionCommand().equals("MagRegion")) {

            for (int i = 0; i < numVisibleSlices; i++) {
                componentImage[i].setCursorMode(ViewJComponentEditImage.MAG_REGION);
            }
        } else if (event.getActionCommand().equals(CustomUIBuilder.PARAM_VOI_DEFAULT_POINTER)) {

            for (int i = 0; i < numVisibleSlices; i++) {
                componentImage[i].setCursorMode(ViewJComponentEditImage.DEFAULT);
            }
        } else if (event.getActionCommand().equals("Repaint")) {
            updatePaint.setState(true);
            updateImages(LUTa, LUTb, false, -1);
            updatePaint.setState(false);
            repaintButton.setEnabled(false);

            /*
             * if (currentPage == numPages - 1 )  return;
             *
             * currentPage++;
             *
             * loadPage(); updateImageBorders(); boolean lastState = updatePaint.getState(); updatePaint.setState(true);
             * updateImages(true); updatePaint.setState(lastState);
             *
             * // update the toolbar and menu items updatePagingToolbar();
             */
        }else if (event.getActionCommand().equals("interpolateShapes")) {
        	//dialog that is not visible...calls the algorithm immediately
        	new JDialogVOIShapeInterpolation(imageA);

        } 

    }

    /**
     * Builds the panels for a page and adds them to the page.
     *
     * @throws  OutOfMemoryError  DOCUMENT ME!
     */
    public void buildPage() throws OutOfMemoryError {

        int[] extents;
        extents = new int[3];
        extents[0] = Math.round(imageA.getExtents()[0]);
        extents[1] = Math.round(imageA.getExtents()[1]);
        extents[2] = Math.round(imageA.getExtents()[2]);

        // if imagePanel and componentImage already exist, then
        // free up memory first
        if (imagePanel != null) {

            for (int j = 0; j < imagePanel.length; j++) {

                if (componentImage[j] != null) {

                    if ( voiManager != null )
                    {
                        voiManager.removeVOIManager( componentImage[j].getVOIManager() );
                    }
                    
                    componentImage[j].dispose(false);
                }

                if (imagePanel[j] != null) {
                    imagePanel[j].removeAll();
                }

                imagePanel[j] = null;
                componentImage[j] = null;
                imageBorder[j] = null;
            }

            imagePanel = null;
            componentImage = null;
            imageBorder = null;
            pagePanel.removeAll();
            System.gc();
        }

        // create and initialize the imagePanel, the componentImage and the imageBorder
        imagePanel = new JPanel[numVisibleSlices];
        componentImage = new ViewJComponentEditImage[numVisibleSlices];
        imageBorder = createImageBorder(numVisibleSlices);

        // build bordered panel so that it's ready to put the image slice inside
        for (int i = 0; i < numVisibleSlices; i++) {

            imagePanel[i] = new JPanel();

            // used to be null layout
            imagePanel[i].setLayout(new BorderLayout());
            imagePanel[i].setBorder(imageBorder[i]);
            imagePanel[i].setBounds(0, 0, imagePanelSizeX, imagePanelSizeY);

            pagePanel.add(imagePanel[i]);

            // since the componentImage doesn't need to change (just the
            // slice number gets updated) go ahead and create that here too
            // slice refers to the "real" slice number in the image

            componentImage[i] = new ViewJComponentEditImage(this, imageA, LUTa, null, imageB, LUTb, null, null,
                                                            magnification / 100, extents, false,
                                                            FileInfoBase.UNKNOWN_ORIENT);
            componentImage[i].setFrameControls(controls);
            componentImage[i].setLocation(borderSize + selectedBorderSize, borderSize + selectedBorderSize);

            componentImage[i].setBuffers(imageBufferA, imageBufferB, pixBuffer, pixBufferB);
            componentImage[i].MAGR_MAG = magnification / 100 * 2.0f;

            if (componentImage[i].MAGR_MAG < 1) {
                componentImage[i].MAGR_MAG = 1;
            }

            componentImage[i].MAGR_WIDTH = (int) (magnification / 100 * 0.5f * extents[0]);
            componentImage[i].MAGR_HEIGHT = (int) (magnification / 100 * 0.5f * extents[1]);
            componentImage[i].setResolutions(resolutionX, resolutionY);

            imagePanel[i].add(componentImage[i]);
            
            

            if ( voiManager != null )
            {
                componentImage[i].setVOIManager(voiManager.addVOIManager( imageA, imageB,
                        componentImage[i], componentImage[i],
                        componentImage[i].getOrientation() ));
            }

        } // end loop on visible slices

    } // end buildPage()

    /**
     * Calculate the individual image panel size based on the current magnification of the image.
     */
    public void calcImagePanelSize() {

        // first get the width and height of the image
        imageWidth = imageA.getExtents()[0];
        imageHeight = imageA.getExtents()[1];

        // set the individual image panel size based on magnification and image size
        imagePanelSizeX = Math.round((magnification / 100.0f) * imageWidth * resolutionX);
        imagePanelSizeY = Math.round((magnification / 100.0f) * imageHeight * resolutionY);

        // System.out.println("calcImagePanelSize: image panel X,Y = " + imagePanelSizeX + ", " + imagePanelSizeY);

    } // end calcImagePanelSize()

    /**
     * Calculate the maximum page panel size based on the screen size, the size of the menubar and toolbars and whether
     * the columns or rows are independent.
     */
    public void calcMaxPagePanelSize() {

        // if the independent variable is the column, then
        // the max vertical size (y) is the screen size (vertical)
        // minus the heights of the menubar and toolbars.
        if (row_dependent == true) {
            maxPagePanelSizeX = xScreen - DEFAULT_XSCREEN_SPACE;
            maxPagePanelSizeY = yScreen - menuBar.getHeight() - toolbarPanel.getHeight();
        }

        // if the independent variable is the row, then
        // the max horizontal size (x) is the screen size (horizontal)
        // minus the widths of the menubar and toolbars.
        else { // rows are independent

            // maxPagePanelSizeX = xScreen - menuBar.getWidth() - toolbarPanel.getWidth();
            maxPagePanelSizeX = xScreen;
            maxPagePanelSizeY = yScreen - DEFAULT_YSCREEN_SPACE;
        }

        // System.out.println("calcMaxPagePanelSize: max X = " + maxPagePanelSizeX + " max Y = " + maxPagePanelSizeY);

    } // end calcMaxPagePanelSize()

    /**
     * Calculate the number of pages. This is dependent on the number of visible slices and the number of total slices.
     * It's possible that the last page will not be completely filled with images.
     *
     * @return  DOCUMENT ME!
     */
    public int calcNumPages() {

        int num = 1;

        num = (int) Math.ceil((double) numTotalSlices / (double) numVisibleSlices);

        // this must be at least 1
        if (num < 1) {
            num = 1;
        }

        return num;
    } // end calcNumPages()

    /**
     * Calculate the size of the page Panel. First the maximum size of the pagePanel must have been determined. Once the
     * maximum page panel size is determined, then the actual size may be reduced to nicely fit the current image panel
     * size. This will also determine how many images will fit into the page panel.
     */
    public void calcPagePanelSize() {

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
                tmpCols = 1; // force it to be valid
            }

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
                tmpRows = 1; // force it to be valid
            }

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
        numVisibleSlices = gridRow * gridColumn;

        // System.out.println("calcPagePanelSize: Row, Col = " + gridRow + ", " + gridColumn);
        // System.out.println("calcPagePanelSize: pagePanelSize X,Y " + pagePanelSizeX + ", " + pagePanelSizeY);
    } // end calcPagePanelSize()

    /**
     * Calculate the screen size in pixels.
     */
    public void calcScreenSize() {

        // get the screen size from the toolkit
        // take a little off the vertical to account for icon bars
        xScreen = Toolkit.getDefaultToolkit().getScreenSize().width;
        yScreen = Toolkit.getDefaultToolkit().getScreenSize().height - 50;

    } // end calcScreenSize()


    /**
     * Closes the frame.
     */
    public void close() {

        setVisible(false);
        userInterface.unregisterFrame(this);

        if (imageA != null) {
            imageA.removeImageDisplayListener(this);
        }

        if (imageB != null) {
            imageB.removeImageDisplayListener(this);
        }

        dispose();
    }

    /**
     * Closes image A.
     */
    public void closeImageA() {

        if ((imageA != null) && (imageB == null)) {

            if (componentImage != null) {

                for (int i = 0; i < numVisibleSlices; i++) {
                    componentImage[i].dispose(false);
                    componentImage[i] = null;
                    imagePanel[i] = null;
                    imageBorder[i] = null;
                }

                close();
            }

            return;
        } else if ((imageA != null) && (imageB != null)) {
            imageA.removeImageDisplayListener(this);
            imageA = imageB;
            imageB = null;
            imageA.setImageOrder(ModelImage.IMAGE_A);

            if (componentImage != null) {

                for (int i = 0; i < numVisibleSlices; i++) {
                    componentImage[i].setImageB(null);
                    componentImage[i].setImageA(imageA);
                }
            }
        }

        System.gc();
        updateImages(true);
    }

    /**
     * Closes image B.
     */
    public void closeImageB() {

        if (imageB != null) {
            imageB.removeImageDisplayListener(this);
            imageB = null;

            if (componentImage != null) {

                for (int i = 0; i < numVisibleSlices; i++) {
                    componentImage[i].setImageB(null);
                }
            }
        }

        System.gc();
        updateImages(true);
    }

    /**
     * Method called when a component resize event is generated. This method snaps the size of the frame and pagePanel
     * to the nearest row, column sizing (so the gridRow and gridColumn and page layout may change).
     *
     * @param  event  DOCUMENT ME!
     */
    public synchronized void componentResized(ComponentEvent event) {

        // if this event is generated as a result of an internal change
        // then ignore it
        int tmpWidth = getSize().width;
        int tmpHeight = getSize().height;

        // no change in size
        if ((tmpWidth == frameWidth) && (tmpHeight == frameHeight)) {
            return;
        }

        resize();
    }

    /**
     * This method creates the compound image border array for each image in the light box. The outer border is a
     * LineBorder that uses the border color defined in the settings. The inner border is also a Line Border, however
     * it's color depends on whether an image is selected or not (see the selectedImages vector).
     *
     * @param   size  the number of images for which borders are needed
     *
     * @return  DOCUMENT ME!
     */
    public CompoundBorder[] createImageBorder(int size) {
        CompoundBorder[] border = new CompoundBorder[size];

        for (int i = 0; i < size; i++) {
            LineBorder outerBorder = new LineBorder(borderColor, borderSize);
            LineBorder innerBorder = new LineBorder(unselectedBorderColor, selectedBorderSize);

            border[i] = new CompoundBorder(outerBorder, innerBorder);
        }

        return border;

    } // end createImageBorder

    /**
     * Returns the color of the image border.
     *
     * @return  the image border color.
     */
    public Color getBorderColor() {
        return borderColor;
    }

    /**
     * Returns the size of the image border.
     *
     * @return  the size of the image border.
     */
    public int getBorderSize() {
        return borderSize;
    }

    /**
     * Gets control widgets for frame.
     *
     * @return  DOCUMENT ME!
     */
    public ViewControlsImage getControls() {
        return null;
    } // controls; }

    /**
     * Returns the current tslice.
     *
     * @return  the current time slice.
     */
    public int getCurrentTSlice() {
        return currentTSlice;
    }

    /**
     * Returns the color of the background of the grid panel.
     *
     * @return  the background color of the grid panel.
     */
    public Color getGridColor() {
        return gridColor;
    }

    /**
     * getLightBoxCol - return the number of columns.
     *
     * @return  the number of columns.
     */
    public int getGridColumn() {
        return gridColumn;
    }

    /**
     * Returns the number of rows.
     *
     * @return  the number of rows.
     */
    public int getGridRow() {
        return gridRow;
    }

    /**
     * Returns the size of the space between image borders.
     *
     * @return  the size of space between image borders.
     */
    public int getGridSpacing() {
        return gridSpacing;
    }
    
    /**
     * Returns the increment between displayed slices
     * @return increment between displayed slices
     */
    public int getIncrement() {
        return increment;
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
     * Returns the maximum magnification of the image.
     *
     * @return  the maximum magnification of the image.
     */
    public float getMagMax() {
        return magMax;
    }

    /**
     * Returns the minimum magnificaiton of the image.
     *
     * @return  the minimum magnification of the image.
     */
    public float getMagMin() {
        return magMin;
    }

    /**
     * Returns the present magnification of the image.
     *
     * @return  the present magnification of the image.
     */
    public float getMagnification() {
        return magnification;
    }

    /**
     * Returns the number of images in the volume.
     *
     * @return  the number of image (slices).
     */
    public int getNumTotalSlices() {
        return numTotalSlices;
    }

    /**
     * Returns the number of time slices.
     *
     * @return  the number of time slices.
     */
    public int getNumTSlices() {
        return numTSlices;
    }


    /**
     * Returns the row dependent flag.
     *
     * @return  the boolean indicating if rows are the dependent variable.
     */
    public boolean getRowDependent() {
        return row_dependent;
    }

    /**
     * Returns the color of the selected image border.
     *
     * @return  the selected image border color.
     */
    public Color getSelectedBorderColor() {
        return selectedBorderColor;
    }

    /**
     * Initializes the buffers that hold the image data.
     */
    public void initBuffers() {

        int colorFactor = 1;

        if (imageA.isColorImage()) {
            colorFactor = 4;
        }

        imageBufferA = new float[colorFactor * imageA.getSliceSize()];
        pixBuffer = new int[imageA.getSliceSize()];

        if (imageB != null) {
            if (imageB.isColorImage()) {
                colorFactor = 4;
            }
            imageBufferB = new float[colorFactor * imageB.getSliceSize()];
            pixBufferB = new int[imageB.getSliceSize()];
        }

    } // end initBuffers()

    /**
     * Initializes the LUTs for the images.
     *
     * @param   _LUTa  DOCUMENT ME!
     * @param   _LUTb  DOCUMENT ME!
     *
     * @throws  OutOfMemoryError  DOCUMENT ME!
     */
    public void initImageLUTs(ModelLUT _LUTa, ModelLUT _LUTb) throws OutOfMemoryError {

        int[] dimExtentsLUT;
        dimExtentsLUT = new int[2];
        dimExtentsLUT[0] = 4;
        dimExtentsLUT[1] = 256;

        LUTa = _LUTa;
        LUTb = _LUTb;

        if (LUTa == null) {
            LUTa = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
            LUTa.resetTransferLine(0.0f, (int) Math.round(imageA.getMin()), 255.0f, (int) Math.round(imageA.getMax()));
        }

        if ((LUTb == null) && (imageB != null)) {
            LUTb = new ModelLUT(ModelLUT.HOTMETAL, 256, dimExtentsLUT);
            LUTb.resetTransferLine(0.0f, (int) Math.round(imageB.getMin()), 255.0f, (int) Math.round(imageB.getMax()));
        }

        imageA.setImageOrder(ModelImage.IMAGE_A);

        if (imageB != null) {
            imageB.setImageOrder(ModelImage.IMAGE_B);
        }

    } // end initImageLUTs()


    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * itemStateChanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();
        int state = event.getStateChange();

        if (source == updatePaint) {

            if ((updatePaint.getState() == true) && (repaintButton != null)) {
                repaintButton.setEnabled(false);
                // updateImages(LUTa, LUTb, true, -1);
            } else if (repaintButton != null) {
                repaintButton.setEnabled(true);
            }
        } else if ((selectIndividualTSlices != null) && (source == selectIndividualTSlices)) {

            if (selectIndividualTSlices.isSelected()) {
                singleTSlice = true;
                selectedImages.removeAllElements();
            } else {
                singleTSlice = false;
                selectedTimeSlices.removeAllElements();
            }

            selectNone();
        } else {

            for (int i = 0; i < toggleArray.length; i++) {

                if ((source == toggleArray[i]) && (state == ItemEvent.SELECTED)) {
                    ((JToggleButton) source).setBorderPainted(true);
                } else if ((source == toggleArray[i]) && (state == ItemEvent.DESELECTED)) {
                    ((JToggleButton) source).setBorderPainted(false);
                }
            }
        }


    }

    /**
     * Loads the currentPage images see loadPage(int page).
     *
     * @throws  OutOfMemoryError  DOCUMENT ME!
     */
    public void loadPage() throws OutOfMemoryError {
        loadPage(currentPage);
    }

    /**
     * Loads the images for a page into component images and then loads the page.
     *
     * @param   page  DOCUMENT ME!
     *
     * @throws  OutOfMemoryError  DOCUMENT ME!
     */
    public void loadPage(int page) throws OutOfMemoryError {

        // build bordered panel and put image slice inside
        // System.out.println("loadPage: page " + page + " num of visible slices " + numVisibleSlices);
        for (int i = 0; i < numVisibleSlices; i++) {

            // slice refers to the "real" slice number in the image
            int slice = i + (page * numVisibleSlices);

            if (slice < numTotalSlices) {
                componentImage[i].setSlice(slice*increment);
            } else {
                componentImage[i].showBlank();
            }

            if (slice == imageFrame.getViewableSlice()) {
                componentImage[i].useHighlight(true);
                currentSlice = slice;
            } else {
                componentImage[i].useHighlight(false);
            }

        } // end loop on visible slices

    } // end loadPage()

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

        if (componentImage != null) {

            for (int i = 0; i < numVisibleSlices; i++) {
                componentImage[i].setActiveImage(active);
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
    public void setAlphaBlend(int value) {

        if (componentImage != null) {

            for (int i = 0; i < numVisibleSlices; i++) {
                componentImage[i].setAlphaBlend(value);
            }
        }
    }

    /**
     * Sets the color of the image borders.
     *
     * @param  col  color of the image borders
     */
    public void setBorderColor(Color col) {

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
    public void setBorderSize(int size) {

        if ((size < 0) || (size > MAX_GRID_BORDER)) {
            return;
        }

        if (size == borderSize) {
            return;
        }

        borderSize = size;
    }

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
    public void setEnabled(boolean flag) {

        if (componentImage != null) {

            for (int i = 0; i < numVisibleSlices; i++) {
                int slice = i + (currentPage * numVisibleSlices);

                if (slice < numTotalSlices) {
                    componentImage[i].setEnabled(flag);
                }
            }
        }
    }

    /**
     * Sets the background color of the panel that is in the scrollpane.
     *
     * @param  col  the color the background of the panel that is in the scrollpane
     */
    public void setGridColor(Color col) {

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
    public void setGridColumn(int col) {

        if (col > MAX_GRID_COL) { // check bounds
            col = MAX_GRID_COL;
        } else {
            gridColumn = col;
        }
    }

    /**
     * This method is called when there is a change to the light box row number--in the limiting case. It ensures that
     * the row value is within bounds.
     *
     * @param  row  the number of rows to be used to display the images
     */
    public void setGridRow(int row) {

        if (row > MAX_GRID_ROW) { // check bounds
            row = MAX_GRID_ROW;
        } else {
            gridRow = row;
        }
    }

    /**
     * Sets the distance between adjacent images. verifies that the width value is within bounds. If width is outside of
     * bounds, method fails quietly.
     *
     * @param  width  the width of space between images.
     */
    public void setGridSpacing(int width) {

        if ((width < 0) || (width > MAX_GRID_SIZE)) {
            return;
        }

        if (width == gridSpacing) {
            return;
        }

        gridSpacing = width;
    }
    
    /**
     * Sets the increment between displayed slices
     * @param _increment increment between displayed slices
     */
    public void setIncrement(int _increment) {
        if (_increment < 0) {
            return;
        }
        
        if (_increment == increment) {
            return;
        }
        
        increment = _increment;
        numTotalSlices = 1 + (imageA.getExtents()[2] - 1)/increment;
    }

    /**
     * Sets the color of all the images highlight to the specified color.
     *
     * @param  col  color of the image highlight
     */
    public void setHighlightColor(Color col) {

        // don't allow highlight color to be the same
        // as the border color
        if ((borderColor != null) && borderColor.equals(col)) {
            return;
        }

        for (int i = 0; i < componentImage.length; i++) {
            componentImage[i].setHighlightColor(col);
        }
    }

    /**
     * Accessor that sets the reference to imageA.
     *
     * @param  _imageA  image to set the frame to
     */
    public void setImageA(ModelImage _imageA) {

        imageA = _imageA;
        setZoom(magnification / 100, magnification / 100);

        if (componentImage != null) {

            for (int i = 0; i < numVisibleSlices; i++) {
                componentImage[i].setImageA(imageA);
            }
        } else {
            return;
        }


        if (imageA.getNDims() == 4) {
            currentTSlice = 0;
            numTSlices = imageA.getExtents()[3];
            numTotalSlices =  1 + (imageA.getExtents()[2] - 1)/increment;
        } else if (imageA.getNDims() == 3) {
            numTotalSlices = 1 + (imageA.getExtents()[2] - 1)/increment;
        }

        imageA.addImageDisplayListener(this);
        updateImages(true);

        return;
    }

    /**
     * Accessor that sets the reference to imageB.
     *
     * @param  _imageB  image to set the frame to
     */
    public void setImageB(ModelImage _imageB) {

        imageB = _imageB;
        setZoom(magnification / 100, magnification / 100);

        if (imageB.getNDims() == 4) {
            currentTSlice = 0;
            numTSlices = imageB.getExtents()[3];
        }

        if (componentImage != null) {

            for (int i = 0; i < numVisibleSlices; i++) {
                componentImage[i].setImageB(imageB);
            }
        } else {
            return;
        }

        imageB.addImageDisplayListener(this);

        if (imageB.isColorImage() == false) {
            int[] dimExtentsLUT = new int[2];
            dimExtentsLUT[0] = 4;
            dimExtentsLUT[1] = 256;

            ModelLUT LUT = new ModelLUT(ModelLUT.HOTMETAL, 256, dimExtentsLUT);

            updateImages(null, LUT, true, -1);
        } else {
            updateImages(null, null, true, -1);
        }

        return;
    }

    /**
     * Sets the magnification of the images will ensure that the current magnification is not larger than the maximum.
     *
     * @param  num  number between magMin and magMax
     */
    public void setMagnification(int num) {
        setMagnification((float) num);
    }

    /**
     * Sets the magnification of the images will ensure that the current magnification is not smaller than the minimum.
     *
     * @param  num  number between magMin and magMax
     */
    public void setMagnification(float num) {

        if (num < magMin) {
            magnification = magMin;
        } else if (num > magMax) {
            magnification = magMax;
        } else {
            magnification = num;
        }
    }


    /**
     * Sets the current page so that the current image slice is displayed.
     */
    public void setPageToCurrentSlice() {

        setPageToSlice(currentSlice);

    } // end setPageToCurrentSlice()

    /**
     * Sets the page so that the given image slice is displayed.
     *
     * @param  slice  - the image slice to display
     */
    public void setPageToSlice(int slice) {

        // make sure slice is a legitimate number
        if ((slice < 0) || (slice/increment >= numTotalSlices)) {
            MipavUtil.displayError("Selected slice must be in the range 1 to " + numTotalSlices);

            return;
        }

        int newPage = slice / (increment * numVisibleSlices);

        // if (newPage == currentPage)
        // return;

        currentPage = newPage;

        // update the display
        loadPage();
        updateImageBorders();

        boolean lastState = updatePaint.getState();
        updatePaint.setState(true);
        updateImages(true);
        updatePaint.setState(lastState);

        // update the toolbar and menu items
        updatePagingToolbar();

    } // end setPageToSlice()

    /**
     * When switching the active image, copy the paintBitmap of the previous active image into the paintBitmap of the
     * new active image.
     *
     * @param  paintBitmapSwitch  DOCUMENT ME!
     */
    public void setPaintBitmapSwitch(boolean paintBitmapSwitch) { }

    /**
     * The following 2 functions set the RGB tables for ARGB images A and B.
     *
     * @param  RGBT  DOCUMENT ME!
     */
    public void setRGBTA(ModelRGB RGBT) {

        if (componentImage != null) {

            for (int i = 0; i < numVisibleSlices; i++) {
                componentImage[i].setRGBTA(RGBT);
            }
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  RGBT  DOCUMENT ME!
     */
    public void setRGBTB(ModelRGB RGBT) {

        if (componentImage != null) {

            for (int i = 0; i < numVisibleSlices; i++) {
                componentImage[i].setRGBTB(RGBT);
            }
        }
    }

    /**
     * setRowDependent -- sets the row dependent flag.
     *
     * @param  imgMatrixIsRowDependent  DOCUMENT ME!
     */
    public void setRowDependent(boolean imgMatrixIsRowDependent) {
        row_dependent = imgMatrixIsRowDependent;
    }

    /**
     * Sets the color of the selected image borders.
     *
     * @param  col  color of the selected image borders
     */
    public void setSelectedBorderColor(Color col) {

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
    public void setSelectedBorderSize(int size) {

        if ((size < 0) || (size > MAX_SELECTED_GRID_BORDER)) {
            return;
        }

        if (size == selectedBorderSize) {
            return;
        }

        selectedBorderSize = size;
    }

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
    public void setTimeSlice(int tslice) {

        if (((imageA.getNDims() <= 3) && (imageB == null)) ||
                ((imageA.getNDims() <= 3) && (imageB != null) && (imageB.getNDims() <= 3))) {
            return;
        }

        if (((imageA.getNDims() == 4) && (currentTSlice < imageA.getExtents()[3])) ||
                ((imageB != null) && (imageB.getNDims() == 4) && (currentTSlice < imageB.getExtents()[3]))) {
            currentTSlice = tslice;


            if (singleTSlice) {
                this.updateBorderColor(borderColor);
            }

            for (int i = 0; i < numVisibleSlices; i++) {
                int slice = i + (currentPage * numVisibleSlices);

                if (slice < numTotalSlices) {

                    if (componentImage[i].show(currentTSlice, (slice*increment), null, null, true, -1) == false) {
                        return;
                    }
                } else {
                    componentImage[i].showBlank();
                }
            }

            updateImages(true);
            setTitle();
            lightBoxOptions.setTSlider(currentTSlice);
        }
    }


    /**
     * Sets the title of the frame with the image name of slice location.
     */
    public void setTitle() {
        String str;

        if (displayMode == IMAGE_A) {

            if (imageA.getNDims() > 3) { // Setup the title
                str = imageA.getImageName() + "  " + String.valueOf(currentTSlice + 1) + "/" +
                      String.valueOf(numTSlices) + " M:" + makeString(componentImage[0].getZoomX(), 2);
                setTitle(str);
            } else {
                str = imageA.getImageName() + "  M:" + makeString(componentImage[0].getZoomX(), 2);
                setTitle(str);
            }
        } else {

            if (imageB.getNDims() > 3) { // Setup the title
                str = imageB.getImageName() + "  " + String.valueOf(currentTSlice + 1) + "/" +
                      String.valueOf(numTSlices) + " M:" + makeString(componentImage[0].getZoomX(), 2);
                setTitle(str);
            } else {
                str = imageB.getImageName() + "  M:" + makeString(componentImage[0].getZoomX(), 2);
                setTitle(str);
            }
        }
    }


    /**
     * Sets the scales that defines the magnification of the image.
     *
     * @param   zX  zoom in the x direction
     * @param   zY  zoom in the y direction
     *
     * @return  DOCUMENT ME!
     */
    public boolean setZoom(float zX, float zY) {
        int xDimTemp;
        int yDimTemp;
        int xDim = imageA.getExtents()[0];
        int yDim = imageA.getExtents()[1];

        xDimTemp = Math.round(xDim * zX * resolutionX);
        yDimTemp = Math.round(yDim * zY * resolutionY);

        float[] imageTempA = null;
        float[] imageTempB = null;
        int[] pixBufferTemp = null;
        int[] pixBufferTempB = null;

        try {
            imageTempA = new float[xDim * yDim];

            if (imageB != null) {
                imageTempB = new float[xDim * yDim];
            }

            pixBufferTemp = new int[xDim * yDim];
            pixBufferTempB = new int[xDim * yDim];
        } catch (OutOfMemoryError error) {
            imageTempA = null;
            imageTempB = null;
            pixBufferTemp = null;
            pixBufferTempB = null;

            System.gc();
            throw (error);
        }

        imageBufferA = imageTempA;
        imageBufferB = imageTempB;
        pixBuffer = pixBufferTemp;
        pixBufferB = pixBufferTempB;

        for (int i = 0; i < componentImage.length; i++) {
            componentImage[i].setBuffers(imageBufferA, imageBufferB, pixBuffer, pixBufferB);
            componentImage[i].MAGR_MAG = magnification / 100 * 2.0f;
            componentImage[i].MAGR_WIDTH = (int) (magnification / 100 * 0.5f * xDimTemp);
            componentImage[i].MAGR_HEIGHT = (int) (magnification / 100 * 0.5f * yDimTemp);
        }

        System.gc();

        return true;
    }


    /**
     * Stores the current light box display properties.
     *
     * <ul>
     *   <li>row dependent</li>
     *   <li>grid row</li>
     *   <li>grid column</li>
     *   <li>grid size</li>
     *   <li>grid color</li>
     *   <li>border size</li>
     *   <li>border color</li>
     *   <li>magnification</li>
     *   <li>frame location</li>
     * </ul>
     *
     * <p>into the MipavPreferences file.</p>
     */
    public void storeToDefaults() {
        Preferences.setProperty(Preferences.PREF_LB_ROW_DEPENDENT, new Boolean(row_dependent).toString());
        Preferences.setProperty(Preferences.PREF_LB_GRID_ROW, new Integer(gridRow).toString());
        Preferences.setProperty(Preferences.PREF_LB_GRID_COL, new Integer(gridColumn).toString());
        Preferences.setProperty(Preferences.PREF_LB_GRID_SIZE, new Integer(gridSpacing).toString());
        Preferences.setProperty(Preferences.PREF_LB_INCREMENT, new Integer(increment).toString());
        Preferences.setProperty(Preferences.PREF_LB_GRID_COLOR, this.makeColorString(gridColor));
        Preferences.setProperty(Preferences.PREF_LB_BORDER_SIZE, new Integer(borderSize).toString());
        Preferences.setProperty(Preferences.PREF_LB_BORDER_COLOR, this.makeColorString(borderColor));
        Preferences.setProperty(Preferences.PREF_LB_SELECTED_BORDER_COLOR, this.makeColorString(selectedBorderColor));
        Preferences.setProperty(Preferences.PREF_LB_MAG, new Float(magnification).toString());
        Preferences.setProperty(Preferences.PREF_LB_CUPDATE, new Boolean(updatePaint.getState()).toString());

        // location: a bad location will be trapped when loading the preferences, so store it anyway:
        Preferences.setProperty(Preferences.PREF_LB_LOCATION, this.getLocation().x + "," + this.getLocation().y);

        if (selectIndividualTSlices != null) {
            Preferences.setProperty(Preferences.PREF_LB_TSLICE, new Boolean(singleTSlice).toString());
        }

    }

    /**
     * Sets the color of the image borders.
     *
     * @param  col  color of the image borders
     */
    public void updateBorderColor(Color col) {

        if (borderColor.equals(col) && !singleTSlice) {
            return;
        }

        setBorderColor(col);

        for (int i = 0; i < numVisibleSlices; i++) {
            int slice = i + (currentPage * numVisibleSlices);
            LineBorder outerBorder = new LineBorder(borderColor, borderSize);
            LineBorder innerBorder;

            if (!singleTSlice) {

                if (selectedImages.contains(Integer.toString(slice*increment))) {
                    innerBorder = new LineBorder(selectedBorderColor, selectedBorderSize);
                } else {
                    innerBorder = new LineBorder(unselectedBorderColor, selectedBorderSize);
                }
            } else {

                if (selectedTimeSlices.contains(Integer.toString(slice*increment) + "." + Integer.toString(this.currentTSlice))) {
                    innerBorder = new LineBorder(selectedBorderColor, selectedBorderSize);
                } else {
                    innerBorder = new LineBorder(unselectedBorderColor, selectedBorderSize);
                }
            }

            imageBorder[i] = new CompoundBorder(outerBorder, innerBorder);
            imagePanel[i].setBorder(imageBorder[i]);
        }

        System.gc();
    }

    /**
     * Sets the image border size and then updates light box. Verifies that the width value is within bounds. If width
     * is outside of bounds, method fails quietly.
     *
     * @param  size  thichness of the image border.
     */
    public void updateBorderSize(int size) {

        if ((size < 0) || (size > MAX_GRID_BORDER)) {
            return;
        }

        if (size == borderSize) {
            return;
        }

        borderSize = size;

        for (int i = 0; i < imageBorder.length; i++) {
            int slice = i + (currentPage * numVisibleSlices);
            LineBorder outerBorder = new LineBorder(borderColor, borderSize);
            LineBorder innerBorder;


            if (!singleTSlice) {

                if (selectedImages.contains(Integer.toString(slice*increment))) {
                    innerBorder = new LineBorder(selectedBorderColor, selectedBorderSize);
                } else {
                    innerBorder = new LineBorder(unselectedBorderColor, selectedBorderSize);
                }
            } else {

                if (selectedTimeSlices.contains(Integer.toString(slice*increment) + "." + Integer.toString(this.currentTSlice))) {
                    innerBorder = new LineBorder(selectedBorderColor, selectedBorderSize);
                } else {
                    innerBorder = new LineBorder(unselectedBorderColor, selectedBorderSize);
                }
            }

            imageBorder[i] = new CompoundBorder(outerBorder, innerBorder);
            imagePanel[i].setBorder(imageBorder[i]);

            // recenter the component image
            componentImage[i].setLocation(borderSize + selectedBorderSize, borderSize + selectedBorderSize);
        }

        updateLayout();
    }

    /**
     * Sets the background color of the panel that is in the scrollpane.
     *
     * @param  col  the color the background of the panel that is in the scrollpane
     */
    public void updateGridColor(Color col) {
        setGridColor(col);
        pagePanel.setBackground(col);
    }

    /**
     * This method is called when there is a change to the light box column number verifies that the width value is
     * within bounds. If width is outside of bounds, method fails quietly. 1. recalculate the rows of the light box 2.
     * recalculate the size of the scrollPanel and scrollPane 3. change the layout accordingly
     *
     * @param  col  the number of columns to be used to display the images
     */
    public void updateGridColumn(int col) {

        row_dependent = true;
        setGridColumn(col);
        setGridRow(0);
        updateLayout();
    }

    /**
     * This method is called when there is a change to the light box row number and then updates light box. Verifies
     * that the width value is within bounds row value. If width is outside of bounds, method fails quietly. 1.
     * recalculate the columns of the light box 2. recalculate the size of the scrollPanel and scrollPane 3. change the
     * layout accordingly
     *
     * @param  row  the number of rows to be used to display the images
     */
    public void updateGridRow(int row) {

        row_dependent = false;
        setGridRow(row);
        setGridColumn(0);
        updateLayout();
    }

    /**
     * Set the distance between adjacent images and updates light box. verifies that the width value is within bounds.
     * If width is outside of bounds, method fails quietly.
     *
     * @param  width  the width of space between images.
     */
    public void updateGridSpacing(int width) {

        setGridSpacing(width);
        updateLayout();
    }
    
    /**
     * Sets the increment between displayed slices and updates the light box.
     * @param increment between displayed slices
     */
    public void updateIncrement(int increment) {
        setIncrement(increment);
        updateLayout();
        
    }

    /**
     * This methods calls the componentImage's update method to repaint the screen. This will set the requested
     * highlight when the repaint is issued. Use setSlice() to update a single frames highlight, as this will not set
     * the current slice.
     *
     * @param   slice          the slice of the image to update -- NOT the index into the visible slices.
     * @param   highlightFlag  requests to paint the highlight around the component image
     *
     * @return  boolean confirming successful update see setSlice
     *
     * @see     ViewJComponentEditImage#useHighlight(boolean)
     */
    public final boolean updateImage(int slice, boolean highlightFlag) {

        if (componentImage == null) {
            return false;
        }

        int i = slice - (currentPage * numVisibleSlices);

        try {

            if (i < componentImage.length) {

                if (componentImage[i] != null) {
                    componentImage[i].useHighlight(highlightFlag);
                    componentImage[i].paintComponent(componentImage[i].getGraphics());
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

        if (imageBorder == null) {
            return;
        }

        int slice = i + (currentPage * numVisibleSlices);

        try {
            LineBorder outerBorder = new LineBorder(borderColor, borderSize);
            LineBorder innerBorder;

            if (!singleTSlice) {

                if (selectedImages.contains(Integer.toString(slice*increment))) {
                    innerBorder = new LineBorder(selectedBorderColor, selectedBorderSize);
                } else {
                    innerBorder = new LineBorder(unselectedBorderColor, selectedBorderSize);
                }
            } else {

                if (selectedTimeSlices.contains(Integer.toString(slice*increment) + "." + Integer.toString(this.currentTSlice))) {
                    innerBorder = new LineBorder(selectedBorderColor, selectedBorderSize);
                } else {
                    innerBorder = new LineBorder(unselectedBorderColor, selectedBorderSize);
                }
            }

            imageBorder[i] = new CompoundBorder(outerBorder, innerBorder);
            imagePanel[i].setBorder(imageBorder[i]);
        } catch (ArrayIndexOutOfBoundsException aioobe) {
            Preferences.debug("Unable to update lightbox border for image " + i + ".  Out of bounds!");
        }
    }

    /**
     * This method updates all the image borders for a page. This needs to be called whenever a new page is loaded.
     */
    public void updateImageBorders() {

        for (int i = 0; i < numVisibleSlices; i++) {
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

        if (componentImage == null) {
            return false;
        }

        if (updatePaint.getState() == false) {
            repaintButton.setEnabled(true);

            return false;
        }

        for (int i = 0; i < componentImage.length; i++) {

            if (componentImage[i] != null) {
                int slice = i + (currentPage * numVisibleSlices);

                if (slice < numTotalSlices) {

                    if (slice == imageFrame.getViewableSlice()) {
                        componentImage[i].useHighlight(true);
                        currentSlice = slice;
                    } else {
                        componentImage[i].useHighlight(false);
                    }

                    componentImage[i].paintComponent(componentImage[i].getGraphics());
                } else {
                    componentImage[i].showBlank();
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

        if (componentImage == null) {
            return false;
        }

        if (updatePaint.getState() == false) {
            repaintButton.setEnabled(true);

            return false;
        }

        for (int i = 0; i < numVisibleSlices; i++) {
            int slice = i + (currentPage * numVisibleSlices);

            if (slice < numTotalSlices) {

                if (componentImage[i].show(currentTSlice, (slice*increment), null, null, true, -1) == false) {
                    return false;
                }

                if (slice == imageFrame.getViewableSlice()) {
                    componentImage[i].useHighlight(true);
                    currentSlice = slice;
                } else {
                    componentImage[i].useHighlight(false);
                }
            } else {
                componentImage[i].showBlank();
            }
     
        }

        // menuBar.repaint();
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

        if (componentImage == null) {
            return false;
        }

        if (updatePaint.getState() == false) {
            repaintButton.setEnabled(true);
            LUTa = _LUTa;
            LUTb = _LUTb;

            return false;
        }

        for (int i = 0; i < componentImage.length; i++) {
            int slice = i + (currentPage * numVisibleSlices);

            if (slice < numTotalSlices) {

                if (componentImage[i].show(currentTSlice, (slice*increment), LUTa, LUTb, true, interpMode) == false) {
                    return false;
                }

                if (slice == imageFrame.getViewableSlice()) {
                    componentImage[i].useHighlight(true);
                    currentSlice = slice;
                } else {
                    componentImage[i].useHighlight(false);
                }
            } else {
                componentImage[i].showBlank();
            }
        }

        return true;
    }

    /**
     * Updates the image selection list by toggling the selection of the Z(3rd dimension) plane and the time (4th
     * dimesion) volume.
     *
     * @param  plane         image plane that is to displayed
     * @param  applyToRange  the volume in which the plane the is to be displayed from
     */
    public void updateImageSelection(int plane, boolean applyToRange) {

        //System.err.println("Updating image selection. plane: " + plane + " apply to range: " + applyToRange);

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


                if (!singleTSlice) {

                    if (!selectedImages.contains(Integer.toString(slice)) && (slice/increment < numTotalSlices)) {
                        selectedImages.addElement(Integer.toString(slice));
                    }
                } else {

                    if (!selectedTimeSlices.contains(Integer.toString(slice) + "." + Integer.toString(currentTSlice)) &&
                            (slice/increment < numTotalSlices)) {
                        selectedTimeSlices.addElement(Integer.toString(slice) + "." + Integer.toString(currentTSlice));
                    }
                }

                // only update the borders of visible slices
                int pageIndex = slice/increment - (currentPage * numVisibleSlices);

                if ((pageIndex >= 0) && (pageIndex < numVisibleSlices)) {

                    // update the borders
                    updateImageBorder(pageIndex);
                }
            }
        } else { // otherwise, no range is selected -- simple select and deselect

            // update the inner border of the border of this slice
            // based on whether it's now on or off


            if (!singleTSlice) {

                if (selectedImages.contains(Integer.toString(plane)) && (plane/increment < numTotalSlices)) {
                    selectedImages.removeElement(Integer.toString(plane));
                } else if (plane/increment < numTotalSlices) {
                    selectedImages.addElement(Integer.toString(plane));
                }
            } else {

                if (selectedTimeSlices.contains(Integer.toString(plane) + "." + Integer.toString(currentTSlice)) &&
                        (plane/increment < numTotalSlices)) {
                    selectedTimeSlices.removeElement(Integer.toString(plane) + "." + Integer.toString(currentTSlice));
                } else if (plane/increment < numTotalSlices) {
                    selectedTimeSlices.addElement(Integer.toString(plane) + "." + Integer.toString(currentTSlice));
                }
            }

            // only update the borders of visible slices
            int pageIndex = plane/increment - (currentPage * numVisibleSlices);

            if ((pageIndex >= 0) && (pageIndex < numVisibleSlices)) {
                
                updateImageBorder(pageIndex);
            }

        } // endif range is selected

        setButtonStatus();
        System.gc();


    } // end updateImageSelection()

    /**
     * Updates the Z(3rd dimension) plane and the time (4th dimesion) volume.
     *
     * @param  plane       image plane that is to displayed
     * @param  timeVolume  the volume in which the plane the is to be displayed from
     */
    public void updateImageSlice(int plane, int timeVolume) {
        imageFrame.getImageA().setSlice(plane);
        imageFrame.getImageA().setTimeSlice(timeVolume);
        setSlice(plane/increment);
    }

    /**
     * Sets the magnification of the images and then updates light box.
     *
     * @param  num  number between magMin and magMax
     */
    public void updateMagnification(int num) {
        setMagnification(num);

        calcImagePanelSize();

        for (int i = 0; i < numVisibleSlices; i++) {
            imagePanel[i].setBounds(0, 0, imagePanelSizeX, imagePanelSizeY);
        }

        for (int i = 0; i < numVisibleSlices; i++) {
            componentImage[i].setZoom(magnification / 100, magnification / 100);
            componentImage[i].setResolutions(resolutionX, resolutionY);
            componentImage[i].MAGR_MAG = magnification / 100 * 2.0f;

            if (componentImage[i].MAGR_MAG < 1) {
                componentImage[i].MAGR_MAG = 1;
            }

            componentImage[i].MAGR_WIDTH = (int) (imagePanelSizeX * 0.5f);
            componentImage[i].MAGR_HEIGHT = (int) (imagePanelSizeY * 0.5f);
        }

        updateLayout();
    }

    /**
     * Enable or disable the buttons on the paging toolbar and the view menu items based on the current page and the
     * number of pages.
     */
    public void updatePagingToolbar() {

        // if there's only 1 page, then everything is
        // disabled
        if (numPages == 1) {
            firstPageButton.setEnabled(false);
            prevPageButton.setEnabled(false);
            nextPageButton.setEnabled(false);
            lastPageButton.setEnabled(false);

            firstPageMenuItem.setEnabled(false);
            prevPageMenuItem.setEnabled(false);
            nextPageMenuItem.setEnabled(false);
            lastPageMenuItem.setEnabled(false);

            return;
        }

        // if this is the first page, disable the
        // previous page buttons, but since we know
        // there is another page enable the next buttons
        if (currentPage == 0) {
            firstPageButton.setEnabled(false);
            prevPageButton.setEnabled(false);
            nextPageButton.setEnabled(true);
            lastPageButton.setEnabled(true);

            firstPageMenuItem.setEnabled(false);
            prevPageMenuItem.setEnabled(false);
            nextPageMenuItem.setEnabled(true);
            lastPageMenuItem.setEnabled(true);
        }

        // if this is the last page, disable the
        // next page buttons, but since we know
        // there are more pages enable the previous buttons
        else if (currentPage == (numPages - 1)) {
            firstPageButton.setEnabled(true);
            prevPageButton.setEnabled(true);
            nextPageButton.setEnabled(false);
            lastPageButton.setEnabled(false);

            firstPageMenuItem.setEnabled(true);
            prevPageMenuItem.setEnabled(true);
            nextPageMenuItem.setEnabled(false);
            lastPageMenuItem.setEnabled(false);
        }

        // otherwise, this is some page in the middle,
        // so enable all the buttons
        else {
            firstPageButton.setEnabled(true);
            prevPageButton.setEnabled(true);
            nextPageButton.setEnabled(true);
            lastPageButton.setEnabled(true);

            firstPageMenuItem.setEnabled(true);
            prevPageMenuItem.setEnabled(true);
            nextPageMenuItem.setEnabled(true);
            lastPageMenuItem.setEnabled(true);
        }

    } // end updatePagingToolbar()

    /**
     * This method is called when there is a change to the light box setting for rows, columns, or row_dependency.
     *
     * @param  row_dependent  boolean indicating if rows are the dependent variables
     * @param  row            int indicating the number of rows to display on a page
     * @param  col            int indicating the number of columns to display on a page
     */
    public void updateRowsColumns(boolean row_dependent, int row, int col) {

        this.row_dependent = row_dependent;
        gridColumn = col;
        gridRow = row;
        updateLayout();
    }

    /**
     * Updates the toolbar panel's layout based on the row dependcies.
     */
    public void updateToolbarLayout() {

        // build panel to contain both toolbars... want the toolbars to
        // be stacked vertically when lightbox is a column, but want
        // then to be horizontally next to each other when lightbox is a row.
        GridLayout gLayout;

        if (row_dependent == true) { // columns are independent

            // toolbars should be 2 rows, 1 column
            gLayout = new GridLayout(2, 1, 0, 0);
        } else {

            // toolbars should be 1 row, 2 colummns
            gLayout = new GridLayout(1, 2, 0, 0);
        }

        toolbarPanel.setLayout(gLayout);

    } // end updateToolbarLayout()

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

        int reply = JOptionPane.showConfirmDialog(this, "Do you really want to close the Light Box?", "Light Box close",
                                                  JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);

        if (reply == JOptionPane.YES_OPTION) {

            if (componentImage != null) {

                for (int i = 0; i < numVisibleSlices; i++) {
                    componentImage[i].dispose(false);
                    componentImage[i] = null;
                    imagePanel[i] = null;
                    imageBorder[i] = null;
                }

                imagePanel = null;
                imageBorder = null;
                componentImage = null;
            }

            imageBufferA = null;
            imageBufferB = null;
            pixBuffer = null;
            pixBufferB = null;
            pagePanel.removeAll();
            pagePanel = null;
            menuBar = null;
            toolbarPanel.removeAll();
            toolbarPanel = null;
            LUTa = null;
            LUTb = null;

            if (lightBoxOptions != null) {
                lightBoxOptions.dispose();
                lightBoxOptions = null;
            }

            System.gc();
            this.close();
        }
    }

    /**
     * Method to invert the selection of slices in an image.
     *
     * @param  updateBorders  DOCUMENT ME!
     */
    protected void invertSelections(boolean updateBorders) {

        // go through the selectedImages list -- remove items that
        // are in it, and add items that aren't in it
        for (int i = 0; i < numTotalSlices; i++) {

            if (!singleTSlice) {

                if (selectedImages.contains(Integer.toString(i*increment))) {
                    selectedImages.removeElement(Integer.toString(i*increment));
                } else {
                    selectedImages.addElement(Integer.toString(i*increment));
                }
            } else {
                int numTSlice = imageA.getExtents()[3];

                for (int t = 0; t < numTSlice; t++) {

                    if (selectedTimeSlices.contains(Integer.toString(i*increment) + "." + Integer.toString(t))) {
                        selectedTimeSlices.removeElement(Integer.toString(i*increment) + "." + Integer.toString(t));
                    } else {
                        selectedTimeSlices.addElement(Integer.toString(i*increment) + "." + Integer.toString(t));
                    }
                }

            }

            if (updateBorders) {

                // only update the borders of visible slices
                int pageIndex = i - (currentPage * numVisibleSlices);

                if ((pageIndex >= 0) && (pageIndex < numVisibleSlices)) {
                    updateImageBorder(pageIndex);
                }
            }
        }

        if (updateBorders) {
            setButtonStatus();
        }

        System.gc();

    } // end invertSelections()

    /**
     * Method to select all the slices in an image.
     */
    protected void selectAll() {
        selectedImages.removeAllElements();
        selectedTimeSlices.removeAllElements();

        for (int i = 0; i < numTotalSlices; i++) {

            if (!singleTSlice) {
                selectedImages.addElement(Integer.toString(i*increment));
            } else {
                int numTSlices = imageA.getExtents()[3];

                for (int t = 0; t < numTSlices; t++) {
                    selectedTimeSlices.addElement(Integer.toString(i*increment) + "." + Integer.toString(t));
                }
            }

            // only update the borders of visible slices
            int pageIndex = i - (currentPage * numVisibleSlices);

            if ((pageIndex >= 0) && (pageIndex < numVisibleSlices)) {
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
        selectedImages.removeAllElements();
        selectedTimeSlices.removeAllElements();

        for (int i = 0; i < numTotalSlices; i++) {

            // only update the borders of visible slices
            int pageIndex = i - (currentPage * numVisibleSlices);

            if ((pageIndex >= 0) && (pageIndex < numVisibleSlices)) {
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

        if (selectedImages.isEmpty() && selectedTimeSlices.isEmpty()) {
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
        magMax = ((float) ViewJFrameLightBox.DEFAULT_IMAGE_MAX) / Math.max(imageA.getExtents()[0], imageA.getExtents()[1]) * 100;

        if (magnification > magMax) {
            magnification = magMax;
        }
    }

    /**
     * Sets the minimum magnification to the image. will ensure that the current magnification is not smaller than the
     * minimum.
     */
    protected void setMagMin() {
        magMin = ((float) ViewJFrameLightBox.DEFAULT_IMAGE_MIN) / Math.max(imageA.getExtents()[0], imageA.getExtents()[1]) * 100;

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

        JMenu viewMenu = new JMenu("View");
        viewMenu.setFont(font12B);

        JMenu optionMenu = new JMenu("Options");
        optionMenu.setFont(font12B);

        // add menu items to edit menu
        JMenuItem selectAll = new JMenuItem("Select All", 'A');
        selectAll.addActionListener(this);
        selectAll.setActionCommand("SelectAll");
        selectAll.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_A, java.awt.Event.CTRL_MASK));
        selectAll.setFont(font12B);
        editMenu.add(selectAll);

        JMenuItem selectNone = new JMenuItem("Select None");
        selectNone.addActionListener(this);
        selectNone.setActionCommand("SelectNone");
        selectNone.setFont(font12B);
        editMenu.add(selectNone);

        JMenuItem selectInvert = new JMenuItem("Invert Selections");
        selectInvert.addActionListener(this);
        selectInvert.setActionCommand("SelectInvert");
        selectInvert.setFont(font12B);
        editMenu.add(selectInvert);

        editMenu.addSeparator();

        deleteSelection = new JMenuItem("Delete", MipavUtil.getIcon("delete.gif"));
        deleteSelection.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0));
        deleteSelection.addActionListener(this);
        deleteSelection.setActionCommand("DeleteSelection");
        deleteSelection.setFont(font12B);
        deleteSelection.setEnabled(false);
        editMenu.add(deleteSelection);

        extractSelection = new JMenuItem("Extract", MipavUtil.getIcon("extract.gif"));
        extractSelection.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_E, java.awt.Event.CTRL_MASK));
        extractSelection.addActionListener(this);
        extractSelection.setActionCommand("ExtractSelection");
        extractSelection.setFont(font12B);
        extractSelection.setEnabled(false);
        editMenu.add(extractSelection);
        
       
        

        // add menu items to view menu
        firstPageMenuItem = new JMenuItem("First Page", MipavUtil.getIcon("firstarrow.gif"));
        firstPageMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F, java.awt.Event.CTRL_MASK));
        firstPageMenuItem.addActionListener(this);
        firstPageMenuItem.setActionCommand("FirstPage");
        firstPageMenuItem.setFont(font12B);
        firstPageMenuItem.setEnabled(false);
        viewMenu.add(firstPageMenuItem);

        prevPageMenuItem = new JMenuItem("Previous Page", MipavUtil.getIcon("leftarrow.gif"));
        prevPageMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_UP, 0));
        prevPageMenuItem.addActionListener(this);
        prevPageMenuItem.setActionCommand("PreviousPage");
        prevPageMenuItem.setFont(font12B);
        prevPageMenuItem.setEnabled(false);
        viewMenu.add(prevPageMenuItem);

        viewMenu.addSeparator();

        nextPageMenuItem = new JMenuItem("Next Page", MipavUtil.getIcon("rightarrow.gif"));
        nextPageMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_DOWN, 0));
        nextPageMenuItem.addActionListener(this);
        nextPageMenuItem.setActionCommand("NextPage");
        nextPageMenuItem.setFont(font12B);
        nextPageMenuItem.setEnabled(true);
        viewMenu.add(nextPageMenuItem);

        lastPageMenuItem = new JMenuItem("Last Page", MipavUtil.getIcon("lastarrow.gif"));
        lastPageMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_L, java.awt.Event.CTRL_MASK));
        lastPageMenuItem.addActionListener(this);
        lastPageMenuItem.setActionCommand("LastPage");
        lastPageMenuItem.setFont(font12B);
        lastPageMenuItem.setEnabled(true);
        viewMenu.add(lastPageMenuItem);

        // add menu items to options menu
        JMenuItem itemSettings = new JMenuItem("Settings");
        itemSettings.addActionListener(this);
        itemSettings.setActionCommand("Settings");
        itemSettings.setFont(font12B);
        optionMenu.add(itemSettings);

        // this is here for now.  Is there a better place to put Save Settings...?
        JMenuItem saveSettings = new JMenuItem("Save Settings");
        saveSettings.addActionListener(this);
        saveSettings.setActionCommand("SaveSettings");
        saveSettings.setFont(font12B);
        optionMenu.add(saveSettings);

        

        /*  the print method cannot be used with the lightbox until a new version of
         *  the PrintJob class is released JMenuItem itemPrint = new JMenuItem("Print");
         * optionMenu.addSeparator();
         * itemPrint.addActionListener((ActionListener)this); itemPrint.setActionCommand("Print");
         * itemPrint.setFont(font12B); optionMenu.add(itemPrint);
         */

        optionMenu.addSeparator();

        updatePaint = new JCheckBoxMenuItem("Continuous update");
        updatePaint.setActionCommand("update");
        updatePaint.addItemListener(this);
        updatePaint.setFont(font12B);
        updatePaint.setState(Boolean.valueOf(Preferences.getProperty(Preferences.PREF_LB_CUPDATE)).booleanValue());
        
        optionMenu.add(updatePaint);

        if (imageA.getNDims() > 3) {
            selectIndividualTSlices = new JCheckBoxMenuItem("Select individual time slices");
            selectIndividualTSlices.setActionCommand("singleTS");
            selectIndividualTSlices.setFont(font12B);
            selectIndividualTSlices.setState(singleTSlice);
            selectIndividualTSlices.addItemListener(this);
            optionMenu.add(selectIndividualTSlices);
        }

        menuBar = new JMenuBar();
        menuBar.add(editMenu);
        menuBar.add(viewMenu);
        menuBar.add(optionMenu);
        menuBar.setOpaque(true);
        setJMenuBar(menuBar);
    }

    /**
     * Builds a simple toolbar for this frame (ViewJFrameLightBox).
     */
    private void buildToolbar() {

        Border etchedBorder = BorderFactory.createEtchedBorder();
        Border pressedBorder = BorderFactory.createLoweredBevelBorder();

        toolbarPanel = new JPanel();
        updateToolbarLayout();

        tBar = new JToolBar();
        tBar.setBorder(etchedBorder);
        tBar.setBorderPainted(true);
        tBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        
        JToggleButton pointerVOIButton = new JToggleButton(MipavUtil.getIcon("pointer.gif"));
        pointerVOIButton.addActionListener(this);
        pointerVOIButton.setMargin(new Insets(0, 0, 0, 0));
        pointerVOIButton.setToolTipText("Default Mode");
        pointerVOIButton.setActionCommand(CustomUIBuilder.PARAM_VOI_DEFAULT_POINTER.getActionCommand());
        pointerVOIButton.setBorderPainted(false);
        pointerVOIButton.setRolloverEnabled(true);
        pointerVOIButton.setRolloverIcon(MipavUtil.getIcon("pointerroll.gif"));
        pointerVOIButton.setBorder(pressedBorder);
        pointerVOIButton.addItemListener(this);
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
        zoomRegionButton.addItemListener(this);
        zoomRegionButton.setFocusPainted(false);
        group.add(zoomRegionButton);
        tBar.add(zoomRegionButton);
        toggleArray[1] = zoomRegionButton;

        repaintButton = new JButton(MipavUtil.getIcon("paintinside.gif"));
        repaintButton.addActionListener(this);
        repaintButton.setRolloverIcon(MipavUtil.getIcon("paintinsideroll.gif"));
        repaintButton.setBorderPainted(false);
        repaintButton.setToolTipText("Repaints images");
        repaintButton.setActionCommand("Repaint");
        if(updatePaint.getState()) {
        	repaintButton.setEnabled(false);
        }
        tBar.add(repaintButton);

        JButton separator = new JButton(MipavUtil.getIcon("separator.gif"));
        separator.setBorderPainted(false);
        separator.setFocusPainted(false);

        tBar.add(separator);

        // add new buttons for deleting and extracting selections
        deleteButton = new JButton(MipavUtil.getIcon("delete.gif"));
        deleteButton.addActionListener(this);
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
        extractButton.addActionListener(this);
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
        
        interpolateShapesButton = new JButton(MipavUtil.getIcon("voiShapeInterp.gif"));
        interpolateShapesButton.addActionListener(this);
        interpolateShapesButton.setRolloverIcon(MipavUtil.getIcon("voiShapeInterpRoll.gif"));
        interpolateShapesButton.setBorderPainted(false);
        interpolateShapesButton.setToolTipText("Interpolate VOIs");
        interpolateShapesButton.setActionCommand("interpolateShapes");
        tBar.add(interpolateShapesButton);
        
        
        

        // add tBar to toolbarPanel
        toolbarPanel.add(tBar);

        // build the paging toolbar
        pagingTBar = new JToolBar();
        pagingTBar.setBorder(etchedBorder);
        pagingTBar.setBorderPainted(true);
        pagingTBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);

        // add buttons for paging
        firstPageButton = new JButton(MipavUtil.getIcon("firstarrow.gif"));
        firstPageButton.addActionListener(this);
        firstPageButton.setActionCommand("FirstPage");
        firstPageButton.setFont(MipavUtil.font12B);
        firstPageButton.setToolTipText("First Page");
        firstPageButton.setBorderPainted(false);
        firstPageButton.setRolloverEnabled(true);
        firstPageButton.setRolloverIcon(MipavUtil.getIcon("firstarrowroll.gif"));
        firstPageButton.setBorder(pressedBorder);
        firstPageButton.setFocusPainted(false);
        firstPageButton.setEnabled(false);
        pagingTBar.add(firstPageButton);

        prevPageButton = new JButton(MipavUtil.getIcon("leftarrow.gif"));
        prevPageButton.addActionListener(this);
        prevPageButton.setActionCommand("PreviousPage");
        prevPageButton.setFont(MipavUtil.font12B);
        prevPageButton.setToolTipText("Previous Page");
        prevPageButton.setBorderPainted(false);
        prevPageButton.setRolloverEnabled(true);
        prevPageButton.setRolloverIcon(MipavUtil.getIcon("leftarrowroll.gif"));
        prevPageButton.setBorder(pressedBorder);
        prevPageButton.setFocusPainted(false);
        prevPageButton.setEnabled(false);
        pagingTBar.add(prevPageButton);

        nextPageButton = new JButton(MipavUtil.getIcon("rightarrow.gif"));
        nextPageButton.addActionListener(this);
        nextPageButton.setActionCommand("NextPage");
        nextPageButton.setFont(MipavUtil.font12B);
        nextPageButton.setToolTipText("Next Page");
        nextPageButton.setBorderPainted(false);
        nextPageButton.setRolloverEnabled(true);
        nextPageButton.setRolloverIcon(MipavUtil.getIcon("rightarrowroll.gif"));
        nextPageButton.setBorder(pressedBorder);
        nextPageButton.setFocusPainted(false);
        nextPageButton.setEnabled(false);
        pagingTBar.add(nextPageButton);

        lastPageButton = new JButton(MipavUtil.getIcon("lastarrow.gif"));
        lastPageButton.addActionListener(this);
        lastPageButton.setActionCommand("LastPage");
        lastPageButton.setFont(MipavUtil.font12B);
        lastPageButton.setToolTipText("Last Page");
        lastPageButton.setBorderPainted(false);
        lastPageButton.setRolloverEnabled(true);
        lastPageButton.setRolloverIcon(MipavUtil.getIcon("lastarrowroll.gif"));
        lastPageButton.setBorder(pressedBorder);
        lastPageButton.setFocusPainted(false);
        lastPageButton.setEnabled(false);
        pagingTBar.add(lastPageButton);

        activeSliceButton = new JButton(MipavUtil.getIcon("activelightbox.gif"));
        activeSliceButton.addActionListener(this);
        activeSliceButton.setActionCommand("ActiveSlice");
        activeSliceButton.setFont(MipavUtil.font12B);
        activeSliceButton.setToolTipText("Go to active slice");
        activeSliceButton.setBorderPainted(false);
        activeSliceButton.setRolloverEnabled(true);
        activeSliceButton.setRolloverIcon(MipavUtil.getIcon("activelightboxroll.gif"));
        activeSliceButton.setBorder(pressedBorder);
        activeSliceButton.setFocusPainted(false);
        pagingTBar.add(activeSliceButton);

        /*
         * goToSlicePanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 0, 10)); goToSliceLabel = new JLabel ("Go to
         * slice: ", SwingConstants.RIGHT); goToSliceLabel.setFont(MipavUtil.font12B);
         * goToSliceLabel.setForeground(Color.black); goToSlicePanel.add(goToSliceLabel);
         *
         * goToSliceText = new JTextField (4); goToSliceText.setHorizontalAlignment(JTextField.LEFT);
         * goToSliceText.addActionListener((ActionListener)this); goToSliceText.setActionCommand("GoToSlice");
         * goToSliceText.setFont(MipavUtil.font12B); goToSliceText.setToolTipText("Go to slice");
         * goToSlicePanel.add(goToSliceText); pagingTBar.add(goToSlicePanel);
         */
        // add pagingTBar to toolbarPanel
        toolbarPanel.add(pagingTBar);

        getContentPane().add(toolbarPanel, BorderLayout.NORTH);
    }

    // ************************************************************************
    // ************************** Algorithm Invocations ***********************
    // ************************************************************************

    /**
     * call the ExractSlices algorithm, building the dest image based on the type of the src image. The ExtractSlices
     * algorithm always extracts slices to a new dest image.
     */
    private void callExtractAlgorithm() {
        System.gc();

        int numDestSlices = 0;

        if (!singleTSlice) {
            numDestSlices = selectedImages.size();
            // System.err.println("Using " + numDestSlices + " slices from selectedImages()");
        } else {
            numDestSlices = selectedTimeSlices.size();
        }

        if (displayMode == IMAGE_A) {
            srcImage = imageA;
        } else {
            srcImage = imageB;
        }

        if ((!singleTSlice && (numDestSlices == srcImage.getExtents()[2])) ||
                (singleTSlice && (numDestSlices == (srcImage.getExtents()[2] * srcImage.getExtents()[3])))) {
            MipavUtil.displayError("All slices are selected!  Unselect some slices.");

            return;
        }

        if (numDestSlices != 0) {

            // build array of Strings for algorithm
            // create dialog (which is not visible) to run the algorithm
            JDialogExtractSlices dialogES = null;

            if (!singleTSlice) {
                dialogES = new JDialogExtractSlices(this, srcImage, selectedImages);
                // System.err.println("set up extract slice dialog normally (no 4d->3dconversion)");
            } else {
                dialogES = new JDialogExtractSlices(this, srcImage, selectedTimeSlices);
                dialogES.setConvert4Dto3D(true);
            }

            dialogES.setSeparateThread(true);
            dialogES.callAlgorithm();

            // turn all the selections off if successful
            if (dialogES.isSuccessful()) {
                this.selectNone();
            }

        } else if (numDestSlices == 0) {
            MipavUtil.displayError("No slices were selected!  Select some slices.");
        }


    } // end callExtractAlgorithm()

    /**
     * call the RemoveSlices algorithm, building the dest image based on the type of the src image. The RemoveSlices
     * algorithm always deletes slices from the src image, replacing the old src image.
     */
    private void callRemoveAlgorithm() {
        System.gc();


        if (displayMode == IMAGE_A) {
            srcImage = imageA;
        } else {
            srcImage = imageB;
        }

        if (!singleTSlice) {
            int numDestSlices = selectedImages.size();

            // can't remove either all slices or no slices
            if ((numDestSlices != srcImage.getExtents()[2]) && (numDestSlices != 0)) {

                // build array of Strings for algorithm
                // create dialog (which is not visible) to run the algorithm
                JDialogRemoveSlices dialogRS = new JDialogRemoveSlices(this, srcImage);

                dialogRS.setCheckListRemove(selectedImages);
                dialogRS.setDisplayLocNew(); // temporary while testing

                // dialogRS.setDisplayLocReplace();
                dialogRS.setSeparateThread(true);
                dialogRS.actionPerformed(new ActionEvent(this, 1, "Script"));

                // turn all the selections off if successful
                if (dialogRS.isSuccessful()) {
                    this.selectNone();
                }

            } else if (numDestSlices == 0) {
                MipavUtil.displayError("No slices were selected!  Select some slices.");
            } else {
                MipavUtil.displayError("All slices are selected!  Unselect some slices.");
            }
        }
        // we want to "delete" specific time slices.. so we must extract the inverted selections into
        // a 3D volume using the extract slices algorithm
        else {

            if (selectedTimeSlices.size() == 0) {
                MipavUtil.displayError("No slices are selected!  Select some slices.");

                return;
            } else if (selectedTimeSlices.size() == (srcImage.getExtents()[2] * srcImage.getExtents()[3])) {
                MipavUtil.displayError("All slices are selected!  Unselect some slices.");

                return;
            }

            invertSelections(false);
            callExtractAlgorithm();
            invertSelections(false);
        }

    } // end callRemoveAlgorithm()

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

    /**
     * Takes a java.awt.Color and forms a string representing its color. the string appears as 6 hex digits and looks
     * like the color coding used in html files, as in: "RRGGBB".
     *
     * @param   aColor  the color to be converted to a hexidecimal
     *
     * @return  hexa-decimal string representing the 8-bit values of an RGB color, in the form of "RRGGBB".
     */
    private String makeColorString(Color aColor) {
        String[] rgbString = new String[3];
        int n;

        rgbString[0] = Integer.toString(aColor.getRed(), 16);
        rgbString[1] = Integer.toString(aColor.getGreen(), 16);
        rgbString[2] = Integer.toString(aColor.getBlue(), 16);

        for (n = 0; n < 3; n++) {

            if ((rgbString[n]).length() == 1) {
                rgbString[n] = "0" + rgbString[n];
            }
        }

        return (rgbString[0] + rgbString[1] + rgbString[2]);
    }

    /**
     * Method that resizes the frame and adjusts the rows, columns as needed.
     */
    private void resize() {
        int width, height;

        width = getSize().width - getInsets().left - getInsets().right;
        height = getSize().height - getInsets().top - getInsets().bottom - getJMenuBar().getSize().height -
                 toolbarPanel.getHeight();

        // we want to set the pagePanelSize to the size that will
        // 1. be the closest to the size that fits the images without any
        // remainder and 2. is still less than the max size.

        // find what the new gridRow and gridColumn would be if we
        // "snap" to the nearest image size
        int newGridRow = (int)
                             Math.round((double) height /
                                            (double) (imagePanelSizeY + gridSpacing + (2 * borderSize) +
                                                          (2 * selectedBorderSize)));
        int newGridColumn = (int)
                                Math.round((double) width /
                                               (double) (imagePanelSizeX + gridSpacing + (2 * borderSize) +
                                                             (2 * selectedBorderSize)));

        if (((newGridRow == gridRow) && (newGridColumn == gridColumn))) {
            // || newGridRow * newGridColumn > numTotalSlices  ) {

            // make sure to snap back to 'good' size
            calcPagePanelSize();

            pagePanel.setPreferredSize(new Dimension(pagePanelSizeX, pagePanelSizeY));
            pagePanel.setBounds(0, 0, pagePanelSizeX, pagePanelSizeY);

            // set the frame size
            frameWidth = pagePanelSizeX + getInsets().left + getInsets().right;
            frameHeight = pagePanelSizeY + getInsets().top + getInsets().bottom + getJMenuBar().getHeight() +
                          toolbarPanel.getHeight();

            pack();

            return;
        }
        // otherwise, grid row and col changed, so need to redo the page panel

        // we'll make an assumption:
        // if the size is changed so that the width is greater than the height,
        // then assume the rows are independent, if the height is greater than
        // the width, then make the columns independent
        if (getSize().width > getSize().height) { // check the width and height of the frame, not the panel
            setRowDependent(false);
        } else {
            setRowDependent(true);
        }

        // System.out.println("resize: Row dependent = " + row_dependent);

        gridRow = newGridRow;
        gridColumn = newGridColumn;
        // System.out.println("resize: row, col = " + gridRow + ", " + gridColumn);

        updateLayout();
        setTitle();
    }

    /**
     * Setup the light box view. A scrollpane is added to the frame. A panel with a grid layout is added to the
     * scrollpane. Bordered panels the size of the image are added to each position of the grid. Lastly image slices are
     * added to each bordered panel.
     *
     * @param  LUTa  Lookup table used to display image A.
     * @param  LUTb  Lookup table used to display image B.
     */
    private void setupLightBox(ModelLUT LUTa, ModelLUT LUTb) {

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

            initImageLUTs(LUTa, LUTb);
        } catch (OutOfMemoryError e) {
            System.gc();
            throw (e);
        }

        try {

            // build bordered panel and put image slice inside
            buildPage(); // sets up the image panels on the page -- no contents yet

            // load the images for the current page
            loadPage(currentPage);
        } catch (OutOfMemoryError e) {

            if (componentImage != null) {

                for (int i = 0; i < numVisibleSlices; i++) {

                    if (componentImage[i] != null) {
                        componentImage[i].dispose(false);
                    }

                    componentImage[i] = null;
                    imagePanel[i] = null;
                    imageBorder[i] = null;
                }
            }

            System.gc();
            throw (e);
        }

        getContentPane().add(pagePanel);
        pack(); // let the system calculate its own size based on component preferred sizes
        setVisible(true);

        updateLightBoxLocation(Preferences.getProperty(Preferences.PREF_LB_LOCATION));
        updatePaint.setState(true);
        updateImages(true);
        //updatePaint.setState(false);
        updatePaint.setState(Boolean.valueOf(Preferences.getProperty(Preferences.PREF_LB_CUPDATE)).booleanValue());
        setTitle();

    } // end setupLightBox()

    /**
     * Changes the layout of the light box (i.e. row or col) 1. reset the layout 2. reset the size of panel and pane 3.
     * resize accordingly
     *
     * <p>This will work for simple layout updates --- where the new page size can fit on the screen. If the number of
     * pages decrease, then the page needs to be rebuilt.</p>
     */
    private void updateLayout() {

        calcPagePanelSize();

        int newNumPages = calcNumPages();

        // System.out.println("updateLayout: newNumPages = " + newNumPages + "  numPages = " + numPages );

        try {
            GridLayout gLayout = new GridLayout(gridRow, gridColumn, gridSpacing, gridSpacing);

            pagePanel.setLayout(gLayout);
            pagePanel.setPreferredSize(new Dimension(pagePanelSizeX, pagePanelSizeY));
            pagePanel.setBounds(0, 0, pagePanelSizeX, pagePanelSizeY);

            frameWidth = pagePanelSizeX + getInsets().left + getInsets().right;
            frameHeight = pagePanelSizeY + getInsets().top + getInsets().bottom + menuBar.getHeight() +
                          toolbarPanel.getHeight();
            setSize(frameWidth, frameHeight);

            numPages = newNumPages;

            // make sure that currentPage is not greater than numPages
            if (currentPage >= numPages) {
                currentPage = numPages - 1;
            }

            buildPage();
            loadPage();
            updatePagingToolbar();

            // update borders for this page
            updateImageBorders();

        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Error rebuilding page: " + e.getMessage());
            throw (e);
        }

        pack();
        boolean lastState = updatePaint.getState();
        updatePaint.setState(true);
        updateImages(true);
        updatePaint.setState(lastState);

        // update the dialog
        if (lightBoxOptions != null) {
            lightBoxOptions.setValues(row_dependent, gridRow, gridColumn);
        }

    }

    /**
     * Gets the string from the preferences and verifies the upper-left-hand verifies the upper-left-hand corner is
     * within the screen boundaries. So long as it is within screen boundaries, sets the frame to the given location.
     * String is specified by: "x,y" where x & y are pixel locations (ints in those dimensions).
     *
     * @param  ulCorner  DOCUMENT ME!
     */
    private void updateLightBoxLocation(String ulCorner) {
        StringTokenizer corner;
        int horiz = 0;
        int vert = 0;

        int vertBoundary = yScreen - (pagePanelSizeY + menuBar.getHeight() + toolbarPanel.getHeight());
        int horizBoundary = xScreen - (pagePanelSizeX + getInsets().left + getInsets().right + 5);

        try {
            corner = new StringTokenizer(ulCorner, ",");
        } catch (NullPointerException npe) {
            return;
        }

        try {

            if (corner.countTokens() != 2) {

                // horiz = xScreen - (pagePanel.getSize().width  + getInsets().left + getInsets().right + 15);
                // vert = 0;
                if (row_dependent == true) { // columns are independent
                    horiz = horizBoundary;
                    vert = 0;
                } else { // rows are independent -- move to bottom of screen
                    horiz = 0;
                    vert = vertBoundary;
                }
            } else { // 2: x, y
                horiz = Integer.parseInt(corner.nextToken());
                vert = Integer.parseInt(corner.nextToken());

                if (((horiz < -9) || (vert < -9)) || // -1, -2 ... values are easy to do in practice.  double digits
                                                         // should be difficult
                        (horiz > horizBoundary) || (vert > vertBoundary)) { // beyond edge of screen

                    // horiz = xScreen - (pagePanel.getSize().width  + getInsets().left + getInsets().right + 15);
                    // vert = 0;
                    if (row_dependent == true) { // columns are independent
                        horiz = horizBoundary;
                        vert = 0;
                    } else { // rows are independent -- move to bottom of screen
                        horiz = 0;
                        vert = vertBoundary;
                    }
                }

            }
        } catch (NullPointerException npe) {
            horiz = 0;
            vert = 0;
        }

        this.setLocation(horiz, vert);
    }    
       
} // end class ViewJFrameLightBox
