package gov.nih.mipav.view;



import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.renderer.surfaceview.flythruview.*;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;

import java.io.*;
import java.awt.*;
import java.awt.event.*;
import java.util.*;
import java.text.*;

import javax.media.*;


/**
 *   This class produces a frame surrounding an image whose slices are
 *   animated.  Tool bar buttons are present for continuous reverse,
 *   reverse, continuous forward, forward, pause, stop, magnify,
 *   unmagnify, for generating a dialog to set brightness and contrast,
 *   and for deleting the current slice from the animation.
 *   In continuous forward the images are continuously
 *   displayed from start to end over and over again until pause or
 *   stop are hit.  If pause is hit, the display may be resumed by
 *   repushing the continuous forward button.  In forward each image
 *   is only displayed once from start to finish unless pause or stop
 *   are hit.  If pause is hit, the display may be resumed by repushing
 *   the forward button.  Continuous reverse and reverse work the same
 *   way as continuous forward and forward except for a reversal in
 *   direction.  Users can instantly jump from one mode to another without
 *   hitting stop ro pause - from continuous forward to continuous reverse
 *   for example.  Magnify will double the present magnification and
 *   Unmagnify will half the present magnification.  Magnifications are
 *   only powers of 2.  However, the scale function can be set to any
 *   value in the dialog box called up by JDialogAnimate.  This scale function
 *   will do a slice by slice bilinear of bspline interpolation to create
 *   the initial slices on which the power of 2 magnification may be changed.
 *	<p>
 *   The dialog box for brightness and contrast has brightness and contrast sliders, an Apply button,
 *   and a Cancel button.  The brightness will add an offset ranging from -255 to 255 to every
 *   scaled red, green, and blue in the image.  Contrast will multiply every original red,
 *   green, and blue value by a floating point number ranging from 0.1 to 10.0.  Before
 *   apply is pressed, slider changes are only temporarily made to the currently displayed slice.
 *   If apply is pressed, these changes are permanently made to all slices.  Pressing cancel keeps all
 *   slices in their original state.
 *	<p>
 *   Images may also be viewed in a steady state mode.  The slider is
 *   used to control the number of the slice shown when animation is
 *   not occurring.  A text field for the desired frames per second is
 *   present.  The initial default value is 30 per second.  This is
 *   followed by a text field for the actual frames per second.
 *	<p>
 *   The file menu only has 2 simple functions - a save as function and a
 *   close animate structure function.  The options menu has a view z slice
 *   numbers for use for 4D images.
 *	<p>
 *   ViewJFrameAnimate is called in ViewJFrameImage.
 *	<p>
 *   An animation of a blended image A and image B can be performed.
 *   However, all the blending parameters must be set before the
 *   animation structure is created.  One parameter is alphaBlend
 *   for all images.  For color only are also the parameters:
 *   RaOn, GaOn, BaOn, RbOn, GbOn, BbOn, RGBTA, and RGBTB.
 *   Changes in these parmeters that are made after the animation
 *   structure is created will not be propagated into the animation
 *   structure.
 *	<p>
 *   If 4D images are animated, the animation is performed on the fourth time dimension
 *   with all the z slices for a given time present in a given frame.  The number of rows
 *   or columns for the z slices, whether or not a border frame is present around z slices,
 *   and the border frame color are all selected in JDialogAnimate before ViewJFrameAnimate
 *   is invoked.  Note that JDialogAnimate converts all 4D images to 3D images so
 *   ViewJFrameAnimate actually is always passed 3D images.
 *
 *		@version    1.0
 *
 */
public class ViewJFrameAnimateClip extends ViewJFrameBase
    implements ChangeListener {

    private JPanel controlPanel; // panel that sets the z slice number and the desired
    // frames per second and reports the actual frames per second

    private JMenuBar openingMenuBar; // contains File and Options menus
    private ViewMenuBuilder menuObj;

    private JToolBar toolBar;
    public JToggleButton[] toggleArray = new JToggleButton[8];
    private JToggleButton cReverseButton;
    private JToggleButton reverseButton;
    private JToggleButton cForwardButton;
    private JToggleButton backForthButton;
    private JToggleButton forwardButton;
    private JToggleButton pauseButton;
    private JToggleButton stopButton;
    private JButton recordButton;
    private JButton[] buttonArray = new JButton[3];
    private boolean pause; // When pause is true, execute a while loop
    // containing only 5 msec. sleep commands.
    private boolean stop; // when true forward, continuous forward, reverse, or
    // continuous reverse loop exits

    private Border pressedBorder = BorderFactory.createLoweredBevelBorder();
    private Border etchedBorder = BorderFactory.createEtchedBorder();
    private int zSlice; // slice value determined by the slider
    private int zShow; // Changes during animation - otherwise equals zSlice
    private int xScreen, yScreen; // screen width, screen height
    private float zoom = 1; // present magnification - can only be a power of 2
    // with complex images
    private ViewJComponentAnimateClip componentImage;
    private int nImage; // number of slices in the animation
    private JScrollPane scrollPane;
    private ViewControlsImage controls;

    private JLabel labelFramesPerSecond;
    private JTextField textFramesPerSecond; // desired frames per second
    private float framesPerSecond;
    private long msWait; // msec. between frames to maintain desired rate
    private long newmsWait; // if msWait > msElapsed,
    // newmsWait = msWait - msElapsed is passed to the sleep function
    private long localTime; // System time before call to updateImages(true)
    private long localTime2; // System time after call to updateImages(true);
    private long msElapsed; // msec. taken to do updateImages(true)
    // = localTime2 - localTime
    private long startTime, stopTime;
    private long cycleTime; // = stopTime - startTime
    private JLabel labelActualPerSecond;
    private JTextField textActualPerSecond; // actual frames per second
    private float actualPerSecond;

    private JLabel labelAnimationFrame;
    // Note that an attempt to have DocumentListener for textAnimationFrame set
    // the slider value was unsuccessful.
    private JTextField textAnimationFrame;
    private JSlider slider; // slider for current z slice display
    private JSlider fpsSlider; // frames per second slider
    private Hashtable labelTable; // for z slice slider
    private GridBagLayout cpGBL; // control panel grid bag layout
    private GridBagConstraints cpGBC; // control panel grid bag constraints
    private GridBagLayout gbl; // content pane grid bag layout
    private GridBagConstraints gbc; // content pane grid bag constraints
    private int componentY; // height of TopPanel + openingMenuBar
    private int structureY; // all totals in Y direction not due to image
    private int minimumToolBarWidth = 400; // minimum scroll pane width
    private int minimumHeight = 100; // minimum scroll pane height
    private NumberFormat nf; // number formatting used in frames per second
    // set to give 2 digits to the right of the decimal
    private String aps; // actual frames per second
    private JMenu fileMenu; // menu with save as and close commands
    private boolean fRun = false; // whether forward is currently running
    private boolean bfRun = false; // whether back and forth
    private boolean cfRun = false; // whether continuous forward is
    // currently running
    private boolean rRun = false; // whether reverse is currently running
    private boolean crRun = false; // whether continuous reverse is
    // currently running
    ViewJProgressBar progressBar;

    private JPanel innerPanel = null; // componentImage placed in innerPane
    // and innerPanel placed in scrollPane
    private JPanel topPanel = null; // contains toolBar and controlPanel
    private GridBagConstraints gbcTP;
    private int scrollPaneSize = 512;
    private int nRow; // 1 for 3D, rows of z images in 4D images
    private int nColumn; // 1 for 3D, columns of z images in 4D images
    private int xDim, yDim, originalXDim, originalYDim;
    // for 4D originalXDim and originalYDim are xDim and yDim before 4D to 3D conversion
    private int originalZDim; // equals zDim of original and present 3D
    // equals zDim of original 4D before 4D to 3D conversion
    private boolean showNumbers = true; // whether to show z slice numbers
    // for each individual z Slice for 4D, 1 number for frame for 3D
    private Vector menuItemVector = new Vector();
    private boolean showFrameBorder; // for 4D if true show border around each
    // z slice
    private Color borderCol; // color of border surrounding z slices with 4D
    // upon exiting ViewJFrameAnimate.  It will be true unless unscaled 3D images are passed.

    private int sliceOldNumber[]; // Translates the slice number of the current
    // image into the slice number the image had before any deletions were performed.

    private JButton deleteButton;

    private String dir;
    /**
     *   Makes a frame of the animated image
     *   @param _imageA         Model of imageA
     *   @param nRow            rows of z images in 4D images
     *   @param nColumn         columns of z images in 4D images
     *   @param nImages         number of images made into video
     */
    public ViewJFrameAnimateClip(ModelImage _imageA, int nRow, int nColumn, int nImages ) {

        super(_imageA, null );
        addNotify();
        dir = _imageA.getFileInfo( 0 ).getFileDirectory() + "flythru"+ File.separatorChar;

        try {
            setIconImage(MipavUtil.getIconImage("movie_16x16.gif"));
        }
        catch (FileNotFoundException error) {
            Preferences.debug(
                "Exception ocurred while getting <" + error.getMessage()
                + ">.  Check that this file is available.\n");
        }

        buildMenu();
        setJMenuBar(openingMenuBar);

        this.nRow = nRow;
        this.nColumn = nColumn;
        this.nImage = nImages;
        this.originalZDim = originalZDim;
        this.showFrameBorder = showFrameBorder;
        this.borderCol = borderCol;
        // setTitle(imageA.getImageName());

        /* Not that the loading is often sufficiently quick so that the
         animation frame will complete loading before the progress
         bar message appears and only a transparent progress bar is
         seen. */

        progressBar = new ViewJProgressBar("image",
                "Constructing animation structure...", 0, 100, true, this, this);

        xScreen = Toolkit.getDefaultToolkit().getScreenSize().width;
        yScreen = Toolkit.getDefaultToolkit().getScreenSize().height;
        progressBar.setLocation(xScreen / 2, yScreen / 2);
        progressBar.setVisible(true);


        // In ViewJFrameImage public void setAlphaBlend(int value) has the line
        // imageA.setAlphaBlend(value) which sets alphaBlend in ModelStorageBase
        // alphaBlend = imageA.getAlphaBlend();

        zSlice = (nImage - 1) / 2;
        zShow = nImage;

        sliceOldNumber = new int[nImage];
        for (int i = 0; i < nImage; i++) {
            sliceOldNumber[i] = i;
        }

        setResizable(true);
        setLocation(100, 100);

        toolBar = buildAnimateToolBar(this);
        buildControlPanel();
        topPanel = new JPanel();
        topPanel.setLayout(new GridBagLayout());

        gbcTP = new GridBagConstraints();
        gbcTP.gridx = 0;
        gbcTP.gridy = 0;
        gbcTP.gridwidth = 1;
        gbcTP.gridheight = 1;
        gbcTP.fill = GridBagConstraints.BOTH;
        gbcTP.anchor = GridBagConstraints.WEST;
        gbcTP.weightx = 100;
        gbcTP.weighty = 100;
        topPanel.add(toolBar, gbcTP);

        gbcTP.gridx = 0;
        gbcTP.gridy = 1;
        gbcTP.gridwidth = 1;
        gbcTP.gridheight = 3;
        gbcTP.fill = GridBagConstraints.BOTH;
        gbcTP.anchor = GridBagConstraints.WEST;
        gbcTP.weightx = 100;
        gbcTP.weighty = 100;
        topPanel.add(controlPanel, gbcTP);
        getContentPane().add(topPanel, "North");
        buildScrollPane(userInterface);

        /* componentY is added so that the previous software for ViewJFrameImage can be
         reused.  There the image was resized without a toolbar, controlPanel, or
         menubar contributing to the vertical length. */
        componentY = topPanel.getHeight() + openingMenuBar.getHeight();
        // structureY is the total of all nonimage components in the Y direction
        structureY = getInsets().top + componentY + getInsets().bottom;
        setSize(
                (int) Math.round(
                        nRow + 3 + getInsets().left
                        + getInsets().right),
                        (int) Math.round(nColumn + 3 + structureY));

        addWindowListener(this);
        addComponentListener(this);
        progressBar.dispose();
        setVisible(true);

        zShow = zSlice;

        // The magnification is set to the highest power of 2.0 for which scroll bars are not needed.

        componentImage.setZoom(1, 1);
        userInterface.regFrame(this);
    }

    /**
     *   Make a scroll frame and puts an image component into it.
     *   @param ui              main user interface frame
     */
    private void buildScrollPane(ViewUserInterface ui) {

        try {
            innerPanel = new JPanel();
            innerPanel.setLayout(new GridBagLayout());
            innerPanel.setBackground(Color.black);

            if (yScreen < 768) {
                scrollPaneSize = 512 - (768 - yScreen);
                if (scrollPaneSize < 400) {
                    scrollPaneSize = 400;
                }
            } else {
                scrollPaneSize = 512;
            }


            // tells if log magnitude display is used with complex images
            componentImage = new ViewJComponentAnimateClip(zoom, nRow, nColumn, nImage);

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
        } catch (OutOfMemoryError e) {
            throw (e);
        }

        // The component image will be displayed in a scrollpane.
        scrollPane = new JScrollPane(innerPanel,
                JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        scrollPane.setBounds(0, 0, scrollPaneSize + 3, scrollPaneSize + 3);
        getContentPane().add(scrollPane);
        scrollPane.setBackground(Color.black);
        scrollPane.setVisible(true);
        scrollPane.validate();

        System.out.println("nImage = " + nImage);

        for (int i = 0; i < nImage; i++) {
            progressBar.updateValueImmed( (int) ( (float) (i + 1) / (float) nImage * 100));
           // Results in createImage producing an Image img[slice] for every slice.
           componentImage.buildImageObject(i, dir);
       }

    }

    /**
     *  Resizes frame and all components
     *  @param event       event that triggered function
     */
    public synchronized void componentResized(ComponentEvent event) {
        int width, height;

        if (getSize().width >= xScreen - 20 || getSize().height >= yScreen - 20) {
            return;
        }

        removeComponentListener(this);

        width = (int) Math.round(
                Math.max(getSize().width - 2 * getInsets().left - 3,
                minimumToolBarWidth));
        height = (int) Math.round(
                Math.max(
                        getSize().height - getInsets().top - componentY
                        - getInsets().bottom - 3,
                        minimumHeight));

        scrollPane.setSize(width, height);
        setSize(
                Math.max(
                        scrollPane.getSize().width + getInsets().left
                        + getInsets().right,
                        minimumToolBarWidth + getInsets().left
                        + getInsets().right),
                        Math.max(
                                getInsets().top + componentY
                                + scrollPane.getSize().height
                                + getInsets().bottom,
                                minimumHeight));

        validate();
        setTitle();
        addComponentListener(this);
        updateImages(true);
    }

    /**
     *  Does nothing.
     */
    public void updateFrame(float sX, float sY) {}

    /**
     *   Set the title of the frame with the image name and magnification
     */
    public void setTitle() {

    }

    /**
     *   This method builds a menu which contains the options
     *   Save image as and Close Animate.
     */
    private void buildMenu() {
        JSeparator separator;

        try {
            separator = new JSeparator();
            menuObj = new ViewMenuBuilder(this);
            openingMenuBar = new JMenuBar();
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameAnimate.buildMenu");
            return;
        }

        openingMenuBar.add(
                menuObj.makeMenu("File", false,
                new JComponent[] {
            menuObj.buildMenuItem("Save image as *.avi",
                    "SaveImageAs", 0, null, false),
            separator,
            menuObj.buildMenuItem("Close", "CloseAnimate", 0, null, false) }));

        openingMenuBar.add(
                menuObj.makeMenu("Options", false,
                new JComponent[] {
            menuObj.buildCheckBoxMenuItem("Show Z slice numbers", "ShowNumbers", true) }));
    }

    /**
     *   Method to build the toolbar for the Animate frame
     *   @param al         Action listener (this frame)
     *   @return           The animation toolbar
     */
    private JToolBar buildAnimateToolBar(ActionListener al) {
        JToolBar animateToolBar = new JToolBar();
        ButtonGroup animateGroup = new ButtonGroup();

        animateToolBar.setBorder(etchedBorder);
        animateToolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);

        cReverseButton = new JToggleButton(MipavUtil.getIcon("leftcont.gif"),
                false);
        cReverseButton.addActionListener(al);
        cReverseButton.setMargin(new Insets(0, 0, 0, 0));
        cReverseButton.setToolTipText("Continuous Reverse");
        cReverseButton.setActionCommand("cReverse");
        cReverseButton.setBorderPainted(false);
        cReverseButton.setRolloverEnabled(true);
        cReverseButton.setRolloverIcon(MipavUtil.getIcon("leftcontroll.gif"));
        cReverseButton.setBorder(pressedBorder);
        cReverseButton.setFocusPainted(false);
        animateGroup.add(cReverseButton);
        animateToolBar.add(cReverseButton);
        toggleArray[0] = cReverseButton;

        reverseButton = new JToggleButton(MipavUtil.getIcon("playreverse.gif"),
                false);
        reverseButton.addActionListener(al);
        reverseButton.setMargin(new Insets(0, 0, 0, 0));
        reverseButton.setToolTipText("Reverse");
        reverseButton.setActionCommand("reverse");
        reverseButton.setBorderPainted(false);
        reverseButton.setRolloverEnabled(true);
        reverseButton.setRolloverIcon(MipavUtil.getIcon("playreverseroll.gif"));
        reverseButton.setBorder(pressedBorder);
        reverseButton.setFocusPainted(false);
        animateGroup.add(reverseButton);
        animateToolBar.add(reverseButton);
        toggleArray[1] = reverseButton;

        forwardButton = new JToggleButton(MipavUtil.getIcon("play.gif"), false);
        forwardButton.addActionListener(al);
        forwardButton.setMargin(new Insets(0, 0, 0, 0));
        forwardButton.setToolTipText("Forward");
        forwardButton.setActionCommand("forward");
        forwardButton.setBorderPainted(false);
        forwardButton.setRolloverEnabled(true);
        forwardButton.setRolloverIcon(MipavUtil.getIcon("playroll.gif"));
        forwardButton.setBorder(pressedBorder);
        forwardButton.setFocusPainted(false);
        animateGroup.add(forwardButton);
        animateToolBar.add(forwardButton);
        toggleArray[3] = forwardButton;

        cForwardButton = new JToggleButton(MipavUtil.getIcon("rightcont.gif"),
                false);
        cForwardButton.addActionListener(al);
        cForwardButton.setMargin(new Insets(0, 0, 0, 0));
        cForwardButton.setToolTipText("Continuous Forward");
        cForwardButton.setActionCommand("cForward");
        cForwardButton.setBorderPainted(false);
        cForwardButton.setRolloverEnabled(true);
        cForwardButton.setRolloverIcon(MipavUtil.getIcon("rightcontroll.gif"));
        cForwardButton.setBorder(pressedBorder);
        cForwardButton.setFocusPainted(false);
        animateGroup.add(cForwardButton);
        animateToolBar.add(cForwardButton);
        toggleArray[2] = cForwardButton;

        backForthButton = new JToggleButton(MipavUtil.getIcon("backforth.gif"),
                false);
        backForthButton.addActionListener(al);
        backForthButton.setMargin(new Insets(0, 0, 0, 0));
        backForthButton.setToolTipText("Backward and Forward");
        backForthButton.setActionCommand("BackForth");
        backForthButton.setBorderPainted(false);
        backForthButton.setRolloverEnabled(true);
        backForthButton.setRolloverIcon(MipavUtil.getIcon("backforthroll.gif"));
        backForthButton.setBorder(pressedBorder);
        backForthButton.setFocusPainted(false);
        animateGroup.add(backForthButton);
        animateToolBar.add(backForthButton);
        toggleArray[6] = backForthButton;

        pauseButton = new JToggleButton(MipavUtil.getIcon("pause.gif"), false);
        pauseButton.addActionListener(al);
        pauseButton.setMargin(new Insets(0, 0, 0, 0));
        pauseButton.setToolTipText("Pause");
        pauseButton.setActionCommand("pause");
        pauseButton.setBorderPainted(false);
        pauseButton.setRolloverEnabled(true);
        pauseButton.setRolloverIcon(MipavUtil.getIcon("pauseroll.gif"));
        pauseButton.setBorder(pressedBorder);
        pauseButton.setFocusPainted(false);
        animateGroup.add(pauseButton);
        animateToolBar.add(pauseButton);
        toggleArray[4] = pauseButton;

        stopButton = new JToggleButton(MipavUtil.getIcon("stop.gif"), false);
        stopButton.addActionListener(al);
        stopButton.setMargin(new Insets(0, 0, 0, 0));
        stopButton.setToolTipText("Stop");
        stopButton.setActionCommand("stop");
        stopButton.setBorderPainted(false);
        stopButton.setRolloverEnabled(true);
        stopButton.setRolloverIcon(MipavUtil.getIcon("stoproll.gif"));
        stopButton.setBorder(pressedBorder);
        stopButton.setFocusPainted(false);
        animateGroup.add(stopButton);
        animateToolBar.add(stopButton);
        toggleArray[5] = stopButton;

        recordButton = new JButton(MipavUtil.getIcon("movie.gif"));
        recordButton.addActionListener(al);
        recordButton.setMargin(new Insets(0, 0, 0, 0));
        recordButton.setToolTipText("Capture to AVI");
        recordButton.setActionCommand("SaveImageAs");
        recordButton.setBorderPainted(false);
        recordButton.setRolloverEnabled(true);
        recordButton.setRolloverIcon(MipavUtil.getIcon("movieroll.gif"));
        recordButton.setBorder(pressedBorder);
        recordButton.setFocusPainted(false);
        animateGroup.add(recordButton);
        animateToolBar.add(recordButton);
        //toggleArray[6] = recordButton;

        animateToolBar.add(makeSeparator());

        JButton zoomInButton = new JButton(MipavUtil.getIcon("zoomin.gif"));

        zoomInButton.addActionListener(al);
        zoomInButton.setToolTipText("Magnify image 2.0x");
        zoomInButton.setActionCommand("MagAnimate");
        zoomInButton.setBorderPainted(false);
        zoomInButton.setRolloverIcon(MipavUtil.getIcon("zoominroll.gif"));
        zoomInButton.setFocusPainted(false);
        animateToolBar.add(zoomInButton);
        buttonArray[0] = zoomInButton;

        JButton zoomOutButton = new JButton(MipavUtil.getIcon("zoomout.gif"));

        zoomOutButton.addActionListener(al);
        zoomOutButton.setToolTipText("Magnify image 0.5x");
        zoomOutButton.setActionCommand("UnMagAnimate");
        zoomOutButton.setBorderPainted(false);
        zoomOutButton.setRolloverIcon(MipavUtil.getIcon("zoomoutroll.gif"));
        zoomOutButton.setFocusPainted(false);
        animateToolBar.add(zoomOutButton);
        buttonArray[1] = zoomOutButton;

        animateToolBar.add(makeSeparator());

        JButton brightnessButton = new JButton(MipavUtil.getIcon("histolut.gif"));

        brightnessButton.addActionListener(al);
        brightnessButton.setToolTipText("Set brightness/contrast");
        brightnessButton.setActionCommand("Brightness");
        brightnessButton.setBorderPainted(false);
        brightnessButton.setRolloverIcon(MipavUtil.getIcon("histolutroll.gif"));
        brightnessButton.setFocusPainted(false);
        animateToolBar.add(brightnessButton);

        animateToolBar.add(makeSeparator());

        deleteButton = new JButton(MipavUtil.getIcon("delete.gif"));
        deleteButton.addActionListener(al);
        deleteButton.setToolTipText("Delete current slice");
        deleteButton.setActionCommand("deleteSlice");
        deleteButton.setBorderPainted(false);
        deleteButton.setRolloverEnabled(true);
        deleteButton.setRolloverIcon(MipavUtil.getIcon("deleteroll.gif"));
        deleteButton.setMargin(new Insets(0, 0, 0, 0));
        animateToolBar.add(deleteButton);
        buttonArray[2] = deleteButton;

        animateToolBar.setFloatable(false);
        return animateToolBar;
    }

    /**
     *   Makes a separator for the use in the toolbars
     *   @return Separator button.
     */
    private JButton makeSeparator() {
        JButton separator = new JButton(MipavUtil.getIcon("separator.gif"));

        separator.setBorderPainted(false);
        separator.setFocusPainted(false);
        return (separator);
    }

    /**
     *   Panel that sets the at rest frame number and the
     *   desired frames per second and reports the actual
     *   frames per second
     */
    private void buildControlPanel() {

        cpGBL = new GridBagLayout();
        cpGBC = new GridBagConstraints();
        cpGBC.fill = GridBagConstraints.BOTH;
        cpGBC.weightx = 100;
        cpGBC.weighty = 100;
        controlPanel = new JPanel();
        controlPanel.setBounds(10, 100, 500, 120);
        controlPanel.setBorder(new EtchedBorder());
        controlPanel.setLayout(cpGBL);

        labelAnimationFrame = new JLabel(
                " Frame number (1 - " + String.valueOf(nImage) + ")");
        labelAnimationFrame.setForeground(Color.black);
        labelAnimationFrame.setFont(MipavUtil.font12);
        labelAnimationFrame.setEnabled(true);
        addControlPanel(labelAnimationFrame, cpGBC, 0, 0, 2, 1);

        slider = new JSlider(1, nImage, 1);
        slider.setFont(MipavUtil.font12);
        slider.setEnabled(true);
        slider.setMinorTickSpacing(nImage / 10);
        slider.setPaintTicks(true);
        slider.addChangeListener(this);
        slider.setVisible(true);
        labelTable = new Hashtable();
        labelTable.put(new Integer(1), createLabel("1"));
        labelTable.put(new Integer(zSlice + 1),
                createLabel(String.valueOf(zSlice + 1)));
        labelTable.put(new Integer(nImage), createLabel(String.valueOf(nImage)));
        slider.setLabelTable(labelTable);
        slider.setPaintLabels(true);
        addControlPanel(slider, cpGBC, 3, 0, 8, 1);

        textAnimationFrame = new JTextField(String.valueOf(zSlice + 1), 4);
        textAnimationFrame.setFont(MipavUtil.font12);
        textAnimationFrame.setEnabled(false);
        cpGBC.fill = GridBagConstraints.NONE;
        addControlPanel(textAnimationFrame, cpGBC, 13, 0, 1, 1);

        cpGBC.fill = GridBagConstraints.BOTH;
        labelFramesPerSecond = new JLabel(" Desired frames per second: 50");
        labelFramesPerSecond.setForeground(Color.black);
        labelFramesPerSecond.setFont(MipavUtil.font12);
        labelFramesPerSecond.setEnabled(true);
        addControlPanel(labelFramesPerSecond, cpGBC, 0, 2, 3, 1);

        fpsSlider = new JSlider(1, 100, 30);
        fpsSlider.setFont(MipavUtil.font12);
        fpsSlider.setEnabled(true);
        fpsSlider.setMinorTickSpacing(5);
        fpsSlider.setPaintTicks(true);
        fpsSlider.addChangeListener(this);
        fpsSlider.setVisible(true);
        labelTable = new Hashtable();

        labelTable.put(new Integer(1), createLabel("1"));
        labelTable.put(new Integer(50), createLabel("50"));
        labelTable.put(new Integer(100), createLabel("100"));
        fpsSlider.setLabelTable(labelTable);
        fpsSlider.setPaintLabels(true);
        addControlPanel(fpsSlider, cpGBC, 3, 2, 8, 1);

        textActualPerSecond = new JTextField("30.0", 6);
        textActualPerSecond.setFont(MipavUtil.font12);
        textActualPerSecond.setEnabled(false);
        cpGBC.fill = GridBagConstraints.NONE;
        addControlPanel(textActualPerSecond, cpGBC, 13, 2, 1, 1);

        framesPerSecond = 30;

        nf = NumberFormat.getNumberInstance();
        nf.setMaximumFractionDigits(2);
    }

    /**
     *  Method that adds components to the control paenl
     *  @param c     component added to the control panel
     *  @param gbc   GridBagConstraints of added component
     *  @param x     grdix location
     *  @param y     gridy location
     *  @param w     gridwidth
     *  @param h     gridheight
     */
    private void addControlPanel(Component c, GridBagConstraints gbc, int x,
            int y, int w, int h) {
        gbc.gridx = x;
        gbc.gridy = y;
        gbc.gridwidth = w;
        gbc.gridheight = h;
        controlPanel.add(c, gbc);
    }

    /**
     * Save the movie into both AVI and QuickTime files.
     */
    public void saveMovie() {
        MediaLocator oml;
        Vector inputFiles = new Vector();
        File outputFile = null;
        File[] inputFile = new File[nImage];
        String file;

        for (int i = 0; i < nImage; i++) {
            inputFiles.addElement(dir + "captureImage" + i + "." + "jpg");
            inputFile[i] = new File(dir + "captureImage" + i + "." + "jpg");
        }

        // Save quick time movie.
        file = "file:" + dir + "flythru.mov";
         if ( (oml = new MediaLocator(file)) == null) {
             System.err.println("Cannot build media locator from: " + dir);
             return;
         }
        JpegImagesToMovie imageToMovie = new JpegImagesToMovie();
        imageToMovie.doIt(nRow, nColumn, 3, inputFiles, oml);

        // Save AVI movie.
        file = dir + "flythru.avi";
        outputFile = new File( file );
        try {
            MovieMaker movieMake = new MovieMaker(nRow, nColumn, 3, outputFile, inputFile);
            movieMake.makeMovie();
        }
        catch (Throwable t) {
            t.printStackTrace();
        }

        // AviCreator imageToAVI = new AviCreator();
        // imageToAVI.doIt(nRow, nColumn, 3, inputFiles, oml);

    }

    /**
     *  Disposes of components and frame.
     */
    public void dispose() {
        setVisible(false);
        // imageA.registerAnimateFrame(animateFrame) occurred in
        // JDialogAnimate.  unregisterAnimateFrame() sets
        // animateFrame to null in modelImage.
      //  imageA.removeImageDisplayListener(this);

        if (componentImage != null) {
            componentImage.dispose(true);
        }
        componentImage = null;
        scrollPane = null;
        controlPanel = null;
        controls = null;

        toolBar = null;
        topPanel = null;
        innerPanel = null;
        cpGBL = null;
        cpGBC = null;
        gbl = null;
        gbc = null;
        toggleArray = null;
        buttonArray = null;
        menuItemVector = null;
        cReverseButton = null;
        backForthButton = null;
        reverseButton = null;
        cForwardButton = null;
        forwardButton = null;
        pauseButton = null;
        stopButton = null;
        labelAnimationFrame = null;
        slider = null;
        labelTable = null;
        textAnimationFrame = null;
        labelFramesPerSecond = null;
        textFramesPerSecond = null;
        labelActualPerSecond = null;
        textActualPerSecond = null;
    }

    // ************************************************************************
    // **************************** Action Events *****************************
    // ************************************************************************

    /**
     *  Calls various methods depending on the action
     *  @param event      event that triggered function
     */
    public void actionPerformed(ActionEvent event) {

        String command;

        // String tmpStr;
        command = event.getActionCommand();

        // tmpStr = textFramesPerSecond.getText();
        // framesPerSecond = java.lang.Float.valueOf(tmpStr).floatValue();
        framesPerSecond = fpsSlider.getValue();
        if (framesPerSecond < 0.0f) {
            MipavUtil.displayError("Frames per second cannot be negative");
            textFramesPerSecond.requestFocus();
            textFramesPerSecond.selectAll();
            return;
        } else if (framesPerSecond > 1000.0f) {
            MipavUtil.displayError("Frames per second cannot exceed 1000.0");
            textFramesPerSecond.requestFocus();
            textFramesPerSecond.selectAll();
            return;
        }
        msWait = (long) ((1000 / framesPerSecond) + 0.4999);

        /* The slider could be used to set the text field, but an attempt to
         use DocumentListener to have the text field set the slider was
         unsuccessful. */

        /* tmpStr = textAnimationFrame.getText();
         zSlice = Integer.parseInt(tmpStr) - 1;
         if (zSlice < 0) {
         MipavUtil.displayError("Animation frame number cannot be less than 1");
         textAnimationFrame.requestFocus();
         textAnimationFrame.selectAll();
         return;
         }
         else if (zSlice > (nImage-1)) {
         MipavUtil.displayError("Animation Frame number cannot exceed " +
         String.valueOf(nImage));
         textAnimationFrame.requestFocus();
         textAnimationFrame.selectAll();
         return;
         } */
        slider.setValue(zSlice + 1);
        updateImages(true);

        pause = false;
        stop = false;

        if (event.getActionCommand().equals("cReverse")) {
            // continuous reverse
            cReverseButton.setBorderPainted(true);
            reverseButton.setBorderPainted(false);
            cForwardButton.setBorderPainted(false);
            backForthButton.setBorderPainted(false);
            forwardButton.setBorderPainted(false);
            pauseButton.setBorderPainted(false);
            stopButton.setBorderPainted(false);
            deleteButton.setEnabled(false);
            slider.setEnabled(false);
            fpsSlider.setEnabled(false);
            for (Enumeration en = slider.getLabelTable().elements(); en.hasMoreElements();) {
                ((JLabel) en.nextElement()).setEnabled(false);
            }
            for (Enumeration en = fpsSlider.getLabelTable().elements(); en.hasMoreElements();) {
                ((JLabel) en.nextElement()).setEnabled(false);
            }
            pause = false;
            // Stop any ongoing continuous forward, forward, or reverse
            // before launching continuous reverse
            crStop crs = new crStop();

            crs.start();
        } else if (event.getActionCommand().equals("reverse")) {
            // reverse - one play from last slice to first slice
            cReverseButton.setBorderPainted(false);
            reverseButton.setBorderPainted(true);
            cForwardButton.setBorderPainted(false);
            backForthButton.setBorderPainted(false);
            forwardButton.setBorderPainted(false);
            pauseButton.setBorderPainted(false);
            stopButton.setBorderPainted(false);
            pause = false;
            // Stop any ongoing continuous forward, forward, or continuous reverse
            // before launching reverse
            rStop rs = new rStop();

            rs.start();
        } else if (event.getActionCommand().equals("cForward")) {
            // continuous forward
            cReverseButton.setBorderPainted(false);
            reverseButton.setBorderPainted(false);
            cForwardButton.setBorderPainted(true);
            backForthButton.setBorderPainted(false);
            forwardButton.setBorderPainted(false);
            pauseButton.setBorderPainted(false);
            stopButton.setBorderPainted(false);

            deleteButton.setEnabled(false);
            slider.setEnabled(false);
            fpsSlider.setEnabled(false);
            for (Enumeration en = slider.getLabelTable().elements(); en.hasMoreElements();) {
                ((JLabel) en.nextElement()).setEnabled(false);
            }
            for (Enumeration en = fpsSlider.getLabelTable().elements(); en.hasMoreElements();) {
                ((JLabel) en.nextElement()).setEnabled(false);
            }
            pause = false;
            // Stop any ongoing forward, continuous reverse, or reverse
            // before launching continuous forward
            cfStop cfs = new cfStop();

            cfs.start();
        } else if (event.getActionCommand().equals("BackForth")) {
            // continuous back and forth
            cReverseButton.setBorderPainted(false);
            reverseButton.setBorderPainted(false);
            cForwardButton.setBorderPainted(false);
            backForthButton.setBorderPainted(true);
            forwardButton.setBorderPainted(false);
            pauseButton.setBorderPainted(false);
            stopButton.setBorderPainted(false);

            deleteButton.setEnabled(false);
            slider.setEnabled(false);
            fpsSlider.setEnabled(false);
            for (Enumeration en = slider.getLabelTable().elements(); en.hasMoreElements();) {
                ((JLabel) en.nextElement()).setEnabled(false);
            }
            for (Enumeration en = fpsSlider.getLabelTable().elements(); en.hasMoreElements();) {
                ((JLabel) en.nextElement()).setEnabled(false);
            }
            pause = false;
            // Stop any ongoing forward, continuous reverse, or reverse
            // before launching continuous back and forth
            cbfStop cfs = new cbfStop();

            cfs.start();
        } else if (event.getActionCommand().equals("forward")) {
            // forward - one play from first slice to last slice
            cReverseButton.setBorderPainted(false);
            reverseButton.setBorderPainted(false);
            cForwardButton.setBorderPainted(false);
            backForthButton.setBorderPainted(false);
            forwardButton.setBorderPainted(true);
            pauseButton.setBorderPainted(false);
            stopButton.setBorderPainted(false);
            pause = false;
            // Stop any ongoing  continuous forward, continuous reverse, or reverse
            // before launching forward
            fStop fs = new fStop();

            fs.start();
        } else if (event.getActionCommand().equals("pause")) {
            // pauses the animation
            cReverseButton.setBorderPainted(false);
            reverseButton.setBorderPainted(false);
            cForwardButton.setBorderPainted(false);
            forwardButton.setBorderPainted(false);
            backForthButton.setBorderPainted(false);
            pauseButton.setBorderPainted(true);
            stopButton.setBorderPainted(false);
            pause = true;
        } else if (event.getActionCommand().equals("stop")) {
            // stops the animation
            cReverseButton.setBorderPainted(false);
            reverseButton.setBorderPainted(false);
            cForwardButton.setBorderPainted(false);
            forwardButton.setBorderPainted(false);
            backForthButton.setBorderPainted(false);
            pauseButton.setBorderPainted(false);
            stopButton.setBorderPainted(true);

            deleteButton.setEnabled(true);
            slider.setEnabled(true);
            fpsSlider.setEnabled(true);
            for (Enumeration en = slider.getLabelTable().elements(); en.hasMoreElements();) {
                ((JLabel) en.nextElement()).setEnabled(true);
            }
            for (Enumeration en = fpsSlider.getLabelTable().elements(); en.hasMoreElements();) {
                ((JLabel) en.nextElement()).setEnabled(true);
            }
            stop = true;
        }
        else if (command.equals("MagAnimate")) {
            // Doubles the present magnification.  The zoom is always a power of 2.
            zoom = 2.0f * componentImage.getZoomX();
            componentImage.setZoom(zoom, zoom);
            validate();
            updateImages(true);
            setTitle();
        } else if (command.equals("UnMagAnimate")) {
            // Halves the present magnification.  The zoom is always a power of 2.
            zoom = 0.5f * componentImage.getZoomX();
            componentImage.setZoom(zoom, zoom);
            validate();
            updateImages(true);
            setTitle();
        } else if (event.getActionCommand().equals("SaveImageAs")) {
            saveMovie();
        } else if (event.getActionCommand().equals("CloseAnimate")) {
            dispose();
            close();
            // dispose();
        } else if (command.equals("ShowNumbers")) {
            showNumbers = menuObj.isMenuItemSelected("Show Z slice numbers");
            if ((nRow > 1) || (nColumn > 1)) { // 4D image - number for each individual z slice
                componentImage.displayNumbers(showNumbers);
            } else { // 3D image
                componentImage.setShowSliceNumber(showNumbers);
            }
            updateImages(true);
        } else if (command.equals("Brightness")) {
            stop = true;
            // The toolbar button to create the dialog for brightness and contrast was pushed
           // new JDialogBrightness(this, componentImage, origBrightness,
           //         origContrast);
        } else if (command.equals("deleteSlice")) {
            // Delete the current slice from the animation.  Note that the software does not actually
            // delete the slice.  It simply uses a sliceOldNumber table in ViewJFrameAnimate and a
            // ignoreSlice boolean array in ViewJComponentAnimate to make sure that the image is never
            // called.  The control panel is rebuild with the slider and labelAnimationFrame having new
            // values.
            slider.setEnabled(false);
            componentImage.ignoreSlice();
            for (int i = zSlice; i < (nImage - 1); i++) {
                sliceOldNumber[i] = sliceOldNumber[i + 1];
            }
            nImage = nImage - 1;
            if (zSlice > 0) {
                zSlice = zSlice - 1;
            }
            zShow = zSlice;
            getContentPane().remove(topPanel);
            topPanel.remove(controlPanel);
            controlPanel = null;
            buildControlPanel();
            topPanel.add(controlPanel, gbcTP);
            getContentPane().add(topPanel, "North");
            validate();
            slider.setEnabled(true);
            updateImages(true);
        }

    }

    /* Threads are used because they are required by the
     sleep function.  cf    , fStop, crStop, and rStop
     set stop to true and keep sleeping for 5 millisecond
     intervals until the other 3 animation modes have ended.
     The, stop is set to false and the desired animation
     mode is started. This allows instantaneous switching
     between any 2 buttons. */

    /* Continuous forward has been selected.  If forward, continuous reverse, or reverse
     are ongoing, then set stop to true and keep sleeping for 5 millisecond intervals until
     none of these 3 functions is going.  Then, stop is set to false and the continuous
     forward mode is started. */
    class cfStop extends Thread {
        public cfStop() {
            ;
        }

        public void run() {

            while (rRun || crRun || fRun || bfRun) {
                stop = true;
                try {
                    sleep(5L);
                } catch (InterruptedException error) {}
            }
            stop = false;
            if (!cfRun) {
                CForward cf = new CForward();

                cf.start();
            }
        } // end of public void run()

    } // end of class cfStop extends Thread


    /* Continuous back and forth has been selected.  If forward, continuous reverse, or reverse
     are ongoing, then set stop to true and keep sleeping for 5 millisecond intervals until
     none of these 3 functions is going.  Then, stop is set to false and the continuous
     forward mode is started. */
    class cbfStop extends Thread {
        public cbfStop() {
            ;
        }

        public void run() {

            while (rRun || crRun || fRun || bfRun) {
                stop = true;
                try {
                    sleep(5L);
                } catch (InterruptedException error) {}
            }
            stop = false;
            if (!cfRun) {
                CBackForward cbf = new CBackForward();

                cbf.start();
            }
        } // end of public void run()
    } // end of class cfStop extends Thread


    /* Forward has been selected.  If continuous forward, continuous reverse, or reverse
     are ongoing, then set stop to true and keep sleeping for 5 millisecond intervals until
     none of these 3 functions is going.  Then, stop is set to false and the
     forward mode is started. */
    class fStop extends Thread {
        public fStop() {
            ;
        }

        public void run() {

            while (rRun || crRun || cfRun || bfRun) {
                stop = true;
                try {
                    sleep(5L);
                } catch (InterruptedException error) {}
            }
            stop = false;
            if (!fRun) {
                Forward f = new Forward();

                f.start();
            }
        } // end of public void run()

    } // end of class fStop extends Thread


    /* Continuous reverse has been selected.  If continuous forward, forward, or reverse
     are ongoing, then set stop to true and keep sleeping for 5 millisecond intervals until
     none of these 3 functions is going.  Then, stop is set to false and the continuous
     reverse mode is started. */
    class crStop extends Thread {
        public crStop() {
            ;
        }

        public void run() {

            while (rRun || cfRun || fRun || bfRun) {
                stop = true;
                try {
                    sleep(5L);
                } catch (InterruptedException error) {}
            }
            stop = false;
            if (!crRun) {
                CReverse cr = new CReverse();

                cr.start();
            }
        } // end of public void run()

    } // end of class crStop extends Thread


    /* Reverse has been selected.  If continuous forward, forward, or continuous reverse
     are ongoing, then set stop to true and keep sleeping for 5 millisecond intervals until
     none of these 3 functions is going.  Then, stop is set to false and the
     reverse mode is started. */
    class rStop extends Thread {
        public rStop() {
            ;
        }

        public void run() {

            while (fRun || crRun || cfRun || bfRun) {
                stop = true;
                try {
                    sleep(5L);
                } catch (InterruptedException error) {}
            }
            stop = false;
            if (!rRun) {
                Reverse r = new Reverse();

                r.start();
            }
        } // end of public void run()

    } // end of class rStop extends Thread


    // continuous forward continuously cycles from first to last slice
    // unless a pause suspends the loop in 5 millisecond sleep functions
    // or a stop causes the loop to exit
    class CForward extends Thread {
        public CForward() {
            ;
        }

        public void run() {
            cfRun = true;
            for (zShow = 0; (!stop); zShow++) {
                if (zShow == 0) {
                    startTime = System.currentTimeMillis();
                }
                localTime = System.currentTimeMillis();
                updateImages(true);
                localTime2 = System.currentTimeMillis();
                msElapsed = localTime2 - localTime;
                if (msWait > msElapsed) {
                    newmsWait = msWait - msElapsed;
                    try {
                        sleep(newmsWait);
                    } catch (InterruptedException error) {}
                } // end of if (msWait > msElapsed)
                else {
                    try {
                        // so chance to respond to another button
                        sleep(1L);
                    } catch (InterruptedException error) {}
                }
                while (pause) {
                    try {
                        sleep(5L);
                    } catch (InterruptedException error) {}
                } // end of while(pause)
                if (zShow == (nImage - 1)) {
                    stopTime = System.currentTimeMillis();
                    cycleTime = stopTime - startTime;
                    // Calculates actual frames per second
                    actualPerSecond = (1000.0f * nImage) / cycleTime;
                    // Produces a string with 2 digits to the right of the decimal
                    aps = nf.format(actualPerSecond);
                    textActualPerSecond.setText(aps);
                    zShow = -1;
                }
            } // end of for (zShow = 0;(!stop);zShow++)
            zShow = zSlice;
            // display the slice currently selected by the slider
            updateImages(true);
            cfRun = false;
        } // end of public void run()

    } // end of class CForward extends Thread


    // continuous backward and forward - continuously cycles from first to last slice
    // unless a pause suspends the loop in 5 millisecond sleep functions
    // or a stop causes the loop to exit
    class CBackForward extends Thread {
        public CBackForward() {
            ;
        }

        public void run() {
            bfRun = true;
            boolean forward = true;

            for (zShow = 0; (!stop);) {
                if (zShow == 0) {
                    startTime = System.currentTimeMillis();
                }
                localTime = System.currentTimeMillis();

                updateImages(true);

                localTime2 = System.currentTimeMillis();
                msElapsed = localTime2 - localTime;
                if (msWait > msElapsed) {
                    newmsWait = msWait - msElapsed;
                    try {
                        sleep(newmsWait);
                    } catch (InterruptedException error) {}
                } // end of if (msWait > msElapsed)
                else {
                    try {
                        // so chance to respond to another button
                        sleep(1L);
                    } catch (InterruptedException error) {}
                }
                while (pause) {
                    try {
                        sleep(5L);
                    } catch (InterruptedException error) {}
                } // end of while(pause)
                if (zShow == (nImage - 1)) {
                    stopTime = System.currentTimeMillis();
                    cycleTime = stopTime - startTime;
                    // Calculates actual frames per second
                    actualPerSecond = (1000.0f * nImage) / cycleTime;
                    // Produces a string with 2 digits to the right of the decimal
                    aps = nf.format(actualPerSecond);
                    textActualPerSecond.setText(aps);
                    forward = false;
                } else if (zShow == 0) {
                    forward = true;
                }

                if (forward == true) {
                    zShow++;
                } else {
                    zShow--;
                }
            } // end of for (zShow = 0;(!stop);zShow++)
            zShow = zSlice;
            // display the slice currently selected by the slider
            updateImages(true);
            bfRun = false;
        } // end of public void run()
    } // end of class CForward extends Thread


    // forward cycles a single time from first to last slice
    // unless a pause suspends the loop in 5 millisecond sleep functions
    // or a stop causes the loop to exit
    class Forward extends Thread {
        public Forward() {
            ;
        }

        public void run() {
            fRun = true;
            startTime = System.currentTimeMillis();
            for (zShow = 0; (zShow < nImage) && (!stop); zShow++) {
                localTime = System.currentTimeMillis();
                synchronized ( this ) {
                    updateImages(true);
                }
                localTime2 = System.currentTimeMillis();
                msElapsed = localTime2 - localTime;
                if (msWait > msElapsed) {
                    newmsWait = msWait - msElapsed;
                    try {
                        sleep(newmsWait);
                    } catch (InterruptedException error) {}
                } // end of if (msWait > msElapsed)
                else {
                    try {
                        // so chance to respond to another button
                        sleep(1L);
                    } catch (InterruptedException error) {}
                }
                while (pause) {
                    try {
                        sleep(5L);
                    } catch (InterruptedException error) {}
                } // end of while(pause)
            } // end of for (zShow = 0; (zShow < nImage) && (!stop);zShow++)
            stopTime = System.currentTimeMillis();
            cycleTime = stopTime - startTime;
            // Calculates actual frames per second
            actualPerSecond = (1000.0f * nImage) / cycleTime;
            // Produces a string with 2 digits to the right of the decimal place
            aps = nf.format(actualPerSecond);
            textActualPerSecond.setText(aps);
            zShow = zSlice;
            // Displays the currently selected slice
            updateImages(true);
            fRun = false;
        } // end of public void run()

    } // end of class Forward extends Thread


    // continuous reverse continuously cycles from last to frist slice
    // unless a pause suspends the loop in 5 millisecond sleep functions
    // or a stop causes the loop to exit
    class CReverse extends Thread {
        public CReverse() {
            ;
        }

        public void run() {
            crRun = true;
            startTime = System.currentTimeMillis();
            for (zShow = nImage - 1; (!stop); zShow--) {
                localTime = System.currentTimeMillis();
                updateImages(true);
                localTime2 = System.currentTimeMillis();
                msElapsed = localTime2 - localTime;
                if (msWait > msElapsed) {
                    newmsWait = msWait - msElapsed;
                    try {
                        sleep(newmsWait);
                    } catch (InterruptedException error) {}
                } // end of if (msWait > msElapsed)
                else {
                    try {
                        // so chance to respond to another button
                        sleep(1L);
                    } catch (InterruptedException error) {}
                }
                while (pause) {
                    try {
                        sleep(5L);
                    } catch (InterruptedException error) {}
                } // end of while(pause)
                if (zShow == 0) {
                    stopTime = System.currentTimeMillis();
                    cycleTime = stopTime - startTime;
                    // calculates actual frames per second
                    actualPerSecond = (1000.0f * nImage) / cycleTime;
                    // produces a string with 2 digits to the right of the decimal point
                    aps = nf.format(actualPerSecond);
                    textActualPerSecond.setText(aps);
                    zShow = nImage;
                }
            } // end of for (zShow = nImage - 1;(!stop);zShow--)
            zShow = zSlice;
            // displays the currenlty selected slice
            updateImages(true);
            crRun = false;
        } // end of public void run()
    } // end of class CReverse extends thread


    // reverse cycles a single time from last to first slice
    // unless a pause suspends the loop in 5 millisecond sleep functions
    // or a stop causes the loop to exit
    class Reverse extends Thread {
        public Reverse() {
            ;
        }

        public void run() {
            rRun = true;
            startTime = System.currentTimeMillis();
            for (zShow = nImage - 1; (zShow >= 0) && (!stop); zShow--) {
                localTime = System.currentTimeMillis();
                updateImages(true);
                localTime2 = System.currentTimeMillis();
                msElapsed = localTime2 - localTime;
                if (msWait > msElapsed) {
                    newmsWait = msWait - msElapsed;
                    try {
                        sleep(newmsWait);
                    } catch (InterruptedException error) {}
                } // end of if (msWait > msElapsed)
                else {
                    try {
                        // so chance to respond to another button press
                        sleep(1L);
                    } catch (InterruptedException error) {}
                }
                while (pause) {
                    try {
                        sleep(5L);
                    } catch (InterruptedException error) {}
                } // end of while(pause)
            } // end of for (zShow = nImage - 1; (zShow >= 0) && (!stop);zShow--)
            stopTime = System.currentTimeMillis();
            cycleTime = stopTime - startTime;
            // calculate actual frames per second
            actualPerSecond = (1000.0f * nImage) / cycleTime;
            // produces string with 2 digits to the right of the decimal point
            aps = nf.format(actualPerSecond);
            textActualPerSecond.setText(aps);
            zShow = zSlice;
            // displays the currently selected slice
            updateImages(true);
            rRun = false;
        } // end of public void run()
    } // end of class Reverse extends Thread

    /**
     *   Tests that the entered parameter is in range
     *   @param str        the value entered by the user
     *   @param minValue   the minimum value this variable may be set to
     *   @param maxValue   the maximum value this variable may be set to
     *   @return           boolean result of test
     */
    protected boolean testParameter(String str, double minValue, double maxValue) {
        double tmp;

        try {
            tmp = Double.valueOf(str).doubleValue();
            if (tmp > maxValue || tmp < minValue) {
                MipavUtil.displayError(
                        "Value is out of range: " + String.valueOf(minValue)
                        + " , " + String.valueOf(maxValue));
                return false;
            } else {
                return true;
            }
        } catch (NumberFormatException error) {
            MipavUtil.displayError("Must enter numeric value");
            return false;
        }
    }

    // ********************************************************************
    // ************************** Item Events *****************************
    // ********************************************************************

    /**
     *   Sets values based on knob along slider
     *   @param e    Event that triggered this function
     */
    public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();
        String str;

        if (source == slider) {
            // Change the currently displayed z slice
            zSlice = slider.getValue() - 1;
            textAnimationFrame.setText(String.valueOf(zSlice + 1));
            zShow = zSlice;
            updateImages(true);
        } else if (source == fpsSlider) {
            // Change the currently displayed fps
            framesPerSecond = fpsSlider.getValue();
            str = " " + String.valueOf(Math.round(framesPerSecond));
            labelFramesPerSecond.setText("Desired frames per second: " + str);
            updateImages(true);
        }
    }

    // ************************************************************************
    // **************************** Window Events *****************************
    // ************************************************************************

    /**
     *   Does nothing.
     */
    public void windowOpened(WindowEvent event) {}

    /**
     *  Stops thread, calls close
     *  @param event    event that triggered function
     */
    public void windowClosing(WindowEvent event) {
        stop = true;
        dispose();
        close();
    }

    /**
     *   Does nothing.
     */
    public void windowClosed(WindowEvent event) {}

    /**
     *   Does nothing.
     */
    public void windowIconified(WindowEvent event) {}

    /**
     *   Does nothing.
     */
    public void windowDeiconified(WindowEvent event) {}

    /**
     *   Does nothing.
     */
    public void windowActivated(WindowEvent event) {}

    /**
     *   Does nothing.
     */
    public void windowDeactivated(WindowEvent event) {}

    /**
     *   Does nothing.
     */
    public void setEnabled(boolean flag) {}

    /**
     *   Does nothing.
     */
    public void setAlphaBlend(int value) {}

    /**
     *   Sets the RGB LUT table for ARGB image A
     *   @param RGBT the new RGB LUT to be applied to the image
     */
    public void setRGBTA(ModelRGB RGBT) {

    }

    /**
     *   Sets the RGB LUT table for ARGB image B
     *   @param RGBT the new RGB LUT to be applied to the image
     */
    public void setRGBTB(ModelRGB RGBT) {

    }

    /**
     *   This methods calls the componentImage's update method
     *   to redraw the screen. Without LUT changes.
     *   @param forceShow  unused parameter
     *   @return           boolean confirming successful update
     */
    public final boolean updateImages(boolean forceShow) {
        if (componentImage == null) {
            return false;
        }
        componentImage.setSlice(sliceOldNumber[zShow]);

        componentImage.update();
        return true;
    }

    /**
     *   Does nothing.
     */
    public final boolean updateImages(ModelLUT LUTa, ModelLUT LUTb,
            boolean forceShow, int interpMode) {
        return true;
    }

    /**
     *   Does nothing.
     */
    public final boolean updateImages() {
        return true;
    }

    /**
     *   Does nothing.
     */
    public boolean updateImageExtents() {
        return false;
    }

    /**
     *   Does nothing.
     */
    public void setControls() {}

    /**
     *   Does nothing.
     */
    public void removeControls() {}

    /**
     *   Get control widgets for frame
     *   @return       controls
     */
    public ViewControlsImage getControls() {
        return controls;
    }

    /**
     *   Does nothing.
     */
    public void setActiveImage(int active) {}

    /**
     *   Does nothing.
     */
    public void setPaintBitmapSwitch(boolean paintBitmapSwitch) {}

    /**
     *   Sets the slice to be displayed and updates title frame
     *   @param slice indicates image slice to be displayed
     */
    public void setSlice(int slice) {

        if (imageA.getNDims() <= 2) {
            return;
        }
        if (zSlice < imageA.getExtents()[2]) {
            zSlice = slice;
            updateImages(true);
            setTitle();
        }
    }

    /**
     *   Does nothing.
     */
    public void setTimeSlice(int slice) {}


    /**
     * Sets the slider value (for invoking save image from dialog)
     * @param fps int frames per second
     */
    public void setFramesPerSecond(int fps) {
        //textFramesPerSecond.setText(new Integer(fps).toString());
        fpsSlider.setValue(fps);
    }

    /**
     *   Accessor that returns the reference to imageA
     *   @return     image
     */
    public ModelImage getImageA() {
        return null;
    }

    /**
     *   Accessor that returns the reference to imageB
     *   @return     imageB
     */
    public ModelImage getImageB() {
        return null;
    }

    /**
     *   Does nothing.
     */
    public void setImageB(ModelImage _imageB) {}

    /**
     *	Helper method to create a label with the proper font and font color.
     *	@param title	Text of the label.
     *	@return			New label.
     */
    private JLabel createLabel(String title) {
        JLabel label = new JLabel(title);

        label.setFont(MipavUtil.font12);
        label.setForeground(Color.black);
        return label;
    }
}
