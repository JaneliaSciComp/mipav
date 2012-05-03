package gov.nih.mipav.view.renderer;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;


/**
 * This class produces a frame where the histogram of the image data is displayed using the color mapping. All frames
 * using the color map are dynamically updated with the new color map. This control panel manipulate the Histogram RGB
 * LUT table for the color images.
 *
 * @version  1.0
 * @author   Matthew J. McAuliffe, Ph.D. ( Primary )
 * @author   Ruida Cheng
 */
public class JPanelHistoRGB
        implements ItemListener, ActionListener, ChangeListener, MouseMotionListener, MouseListener, HistoLUTParent {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Display mode image A. */
    public static final int IMAGE_A = 0;

    /** Display mode image B. */
    public static final int IMAGE_B = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Indicates the amount of blending when two images are loaded in the image frame. */
    protected float alphaBlend = 0.5f;

    /** Indicates which image is to be acted upon when two images are displayed. */
    protected int displayMode = IMAGE_A;

    /** Update Blue channel histogram check box A, B. Updating during mouse dragging */
    private JCheckBox blueCheckBoxA, blueCheckBoxB;

    /** Flag indicating if histogram should be done on all of image or not. */
    private boolean entireFlag = true;

    /** Update Green channel histogram check box A, B. Updating during mouse dragging */
    private JCheckBox greenCheckBoxA, greenCheckBoxB;

    /** imageA histogram of the Blue channel. */
    private ModelHistogram histogramABlue = null;

    /** imageA histogram of the Green channel. */
    private ModelHistogram histogramAGreen = null;

    /** imageA histogram of the Red channel. */
    private ModelHistogram histogramARed = null;

    /** imageB histogram, the Blue channel. */
    private ModelHistogram histogramBBlue = null;

    /** imageB histogram, the Green channel. */
    private ModelHistogram histogramBGreen = null;

    /** imageB histogram of the Red channel. */
    private ModelHistogram histogramBRed = null;

    /** The panel containing the RGB histogram component. */
    private ViewJPanelHistoLUT histoPanelA;

    /** The panel containing the RGB histogram component. */
    private ViewJPanelHistoLUT histoPanelB;

    /** Model image A, B. Define inside this class in order to get rid of the JFrame */
    private ModelImage imageA;

    /** ImageB reference.f */
    private ModelImage imageB;

    /** The histogram log view check box. */
    private JCheckBox logCheckBoxA, logCheckBoxB;

    /** The main GUI control panel. */
    private JPanel mainPanel;

    /** Not used now. */
    private ViewMenuBuilder menuObj;

    /** Tabbed control panel A, B. */
    private JPanel panelA, panelB;

    /** Update histogram check box A, B. Updating during mouse dragging */
    // private JCheckBox updateCheckBoxA = null, updateCheckBoxB = null;

    /** Update Red channel histogram check box A, B. Updating during mouse dragging */
    private JCheckBox redCheckBoxA, redCheckBoxB;

    /** R, G, B channel control buttons. */
    private JToggleButton redRGBButton, blueRGBButton, greenRGBButton, allRGBButton;

    /** Not used now. */
    private ViewJComponentRegistration regComponent = null;

    /** RGB extents. */
    private int[] RGBExtents = new int[2];

    /** Model RGB table A reference. */
    private ModelRGB RGBTA;

    /** Model RGB table B reference. */
    private ModelRGB RGBTB;

    /** Scroll pane. */
    private JScrollPane scroller;

    /** Scroll panel that holding the all the control components. */
    private DrawingPanel scrollPanel;

    /** The Tabbed control panel. */
    private JTabbedPane tabbedPane;

    /** ToolBar that hold the linear, horizontal mode etc. */
    private JToolBar toolBar;

    /** Object which generates the toolbar. */
    private ViewToolBarBuilder toolBarObj;

    /** Toggle buttons for transfer, threshold and threshold inverse. */
    private JToggleButton transferButton, thresholdButton, inverseThresholdButton;

    /** Update the LUT in real-time: */
    private boolean bImageUpdate = false;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Makes a frame of the histogram.
     *
     * @param  _imageA      Model of imageA
     * @param  _imageB      Model of imageB
     * @param  _RGBTA       Model RGB
     * @param  _RGBTB       Model RGB
     * @param  _entireFlag  Flag indicating if histogram should be done on all of image.
     */
    public JPanelHistoRGB(ModelImage _imageA, ModelImage _imageB, ModelRGB _RGBTA, ModelRGB _RGBTB,
                          boolean _entireFlag) {

        imageA = _imageA;
        imageB = _imageB;

        RGBTA = _RGBTA;
        RGBTB = _RGBTB;
        buildMenu();

        entireFlag = _entireFlag;

        // Scroll panel that hold the control panel layout in order to use JScrollPane
        scrollPanel = new DrawingPanel();
        scrollPanel.setLayout(new BorderLayout());

        // Put the drawing area in a scroll pane.
        scroller = new JScrollPane(scrollPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                   JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        mainPanel = new JPanel();
        mainPanel.setLayout(new BorderLayout());

        RGBExtents[0] = 4;
        RGBExtents[1] = 256;

        toolBarObj = new ViewToolBarBuilder(this);
        toolBar = toolBarObj.buildRGBToolBar();
        redRGBButton = (JToggleButton) toolBar.getComponentAtIndex(0);
        greenRGBButton = (JToggleButton) toolBar.getComponentAtIndex(1);
        blueRGBButton = (JToggleButton) toolBar.getComponentAtIndex(2);
        allRGBButton = (JToggleButton) toolBar.getComponentAtIndex(3);

        mainPanel.add(toolBar, BorderLayout.NORTH);

        tabbedPane = new JTabbedPane();
        tabbedPane.setFont(MipavUtil.font12B);
        buildPanelA(imageA, entireFlag);

        if (imageB != null) {
            buildPanelB(imageB, entireFlag);
        }

        tabbedPane.setSelectedIndex(0);

        // mainPanel.add( tabbedPane );
        tabbedPane.addChangeListener(this);
        scrollPanel.add(tabbedPane, BorderLayout.NORTH);

        mainPanel.add(scroller, BorderLayout.CENTER);
    }
    
    /**
     * Makes a frame of the histogram.
     *
     * @param  _imageA      Model of imageA
     * @param  _imageB      Model of imageB
     * @param  _RGBTA       Model RGB
     * @param  _RGBTB       Model RGB
     * @param  _entireFlag  Flag indicating if histogram should be done on all of image.
     * @param  bUpdateImage  update the displayed image continuously when true or on mouse-release when false.
     */
    public JPanelHistoRGB(ModelImage _imageA, ModelImage _imageB, ModelRGB _RGBTA, ModelRGB _RGBTB,
                          boolean _entireFlag, boolean bUpdateImage) {
        this(_imageA, _imageB, _RGBTA, _RGBTB, _entireFlag);
        this.bImageUpdate = bUpdateImage;
    }

    /**
     * Makes a frame of the histogram.
     *
     * @param  _regComponent  component to pass parameters to in manual registration
     * @param  _imageA        Model of imageA
     * @param  _imageB        Model of imageB
     * @param  _RGBTA         Model RGB
     * @param  _RGBTB         Model RGB
     * @param  _entireFlag    Flag indicating if histogram should be done on all of image.
     */
    public JPanelHistoRGB(ViewJComponentRegistration _regComponent, ModelImage _imageA, ModelImage _imageB,
                          ModelRGB _RGBTA, ModelRGB _RGBTB, boolean _entireFlag) {
        this(_imageA, _imageB, _RGBTA, _RGBTB, _entireFlag);

        regComponent = _regComponent;
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

        String command;

        command = event.getActionCommand();

        if (command.equals("red")) {
            redRGBButton.setBorderPainted(true);
            greenRGBButton.setBorderPainted(false);
            blueRGBButton.setBorderPainted(false);
            allRGBButton.setBorderPainted(false);

            if (tabbedPane.getSelectedComponent() == panelA) {

                if (getHistoLUTComponentA() != null) {
                    getHistoLUTComponentA().setMode(getHistoLUTComponentA().RED);
                    getHistoLUTComponentA().setHistogramInfo(imageA, histogramARed);
                    getHistoLUTComponentA().showHistogram();
                }
            } else {

                if (getHistoLUTComponentB() != null) {
                    getHistoLUTComponentB().setMode(getHistoLUTComponentB().RED);
                    getHistoLUTComponentB().setHistogramInfo(imageB, histogramBRed);
                    getHistoLUTComponentB().showHistogram();
                }
            }
        } else if (command.equals("green")) {
            redRGBButton.setBorderPainted(false);
            greenRGBButton.setBorderPainted(true);
            blueRGBButton.setBorderPainted(false);
            allRGBButton.setBorderPainted(false);

            if (tabbedPane.getSelectedComponent() == panelA) {

                if (getHistoLUTComponentA() != null) {
                    getHistoLUTComponentA().setMode(getHistoLUTComponentA().GREEN);
                    getHistoLUTComponentA().setHistogramInfo(imageA, histogramAGreen);
                    getHistoLUTComponentA().showHistogram();
                }
            } else {

                if (getHistoLUTComponentB() != null) {
                    getHistoLUTComponentB().setMode(getHistoLUTComponentB().GREEN);
                    getHistoLUTComponentB().setHistogramInfo(imageB, histogramBGreen);
                    getHistoLUTComponentB().showHistogram();
                }
            }
        } else if (command.equals("blue")) {
            redRGBButton.setBorderPainted(false);
            greenRGBButton.setBorderPainted(false);
            blueRGBButton.setBorderPainted(true);
            allRGBButton.setBorderPainted(false);

            if (tabbedPane.getSelectedComponent() == panelA) {

                if (getHistoLUTComponentA() != null) {
                    getHistoLUTComponentA().setMode(getHistoLUTComponentA().BLUE);
                    getHistoLUTComponentA().setHistogramInfo(imageA, histogramABlue);
                    getHistoLUTComponentA().showHistogram();
                }
            } else {

                if (getHistoLUTComponentB() != null) {
                    getHistoLUTComponentB().setMode(getHistoLUTComponentB().BLUE);
                    getHistoLUTComponentB().setHistogramInfo(imageB, histogramBBlue);
                    getHistoLUTComponentB().showHistogram();
                }
            }
        } else if (command.equals("all")) {
            redRGBButton.setBorderPainted(false);
            greenRGBButton.setBorderPainted(false);
            blueRGBButton.setBorderPainted(false);
            allRGBButton.setBorderPainted(true);

            if (tabbedPane.getSelectedComponent() == panelA) {

                if (getHistoLUTComponentA() != null) {
                    getHistoLUTComponentA().setMode(getHistoLUTComponentA().ALL);
                }
            } else {

                if (getHistoLUTComponentB() != null) {
                    getHistoLUTComponentB().setMode(getHistoLUTComponentB().ALL);
                }
            }
        } else if (command.equals("linearLUT")) {
            // resetThresholdChannels();

            if (tabbedPane.getSelectedComponent() == panelA) {

                if (getHistoLUTComponentA() != null) {

                    // turn off threshold fields:
                    // threshLowerF.setEnabled( false );
                    // threshUpperF.setEnabled( false );
                    // threshFillF.setEnabled( false );
                    getHistoLUTComponentA().noThreshold();
                }
            } else {

                if (getHistoLUTComponentB() != null) {
                    // turn off threshold fields
                    // threshLowerBF.setEnabled( false );
                    // threshUpperBF.setEnabled( false );
                    // threshFillBF.setEnabled( false );

                    getHistoLUTComponentB().noThreshold();
                }
            }

        } else if (command.equals("resetLinearLUT")) {
            allRGBButton.setEnabled(true);

            if (tabbedPane.getSelectedComponent() == panelA) {

                if (getHistoLUTComponentA() != null) {
                    getHistoLUTComponentA().linearMode();
                }
            } else {

                if (getHistoLUTComponentB() != null) {
                    getHistoLUTComponentB().linearMode();
                }
            }
        } else if (command.equals("evendistriLUT")) {
            allRGBButton.setEnabled(true);

            if (tabbedPane.getSelectedComponent() == panelA) {

                if (getHistoLUTComponentA() != null) {
                    getHistoLUTComponentA().evenDistribution();
                }
            } else {

                if (getHistoLUTComponentB() != null) {
                    getHistoLUTComponentB().evenDistribution();
                }
            }
        } else if (command.equals("thresholdLUT")) {

            if (tabbedPane.getSelectedComponent() == panelA) {

                if (getHistoLUTComponentA() != null) {

                    // turn on threshold fields
                    // threshLowerF.setEnabled( true );
                    // threshUpperF.setEnabled( true );
                    // threshFillF.setEnabled( true );
                    // if ( threshFillF.getText() == null ) {
                    // threshFillF.setText( Double.toString( imageA.getMin() ) );
                    // }
                    getHistoLUTComponentA().dualThresholdMode(getHistoLUTComponentA().DUAL_THRESHOLD);
                }
            } else {

                if (getHistoLUTComponentB() != null) {
                    // turn on threshold fields
                    // threshLowerBF.setEnabled( true );
                    // threshUpperBF.setEnabled( true );
                    // threshFillBF.setEnabled( true );
                    // if ( threshFillBF.getText() == null ) {
                    // threshFillBF.setText( Double.toString( imageB.getMin() ) );
                    // }

                    getHistoLUTComponentB().dualThresholdMode(getHistoLUTComponentA().DUAL_THRESHOLD);
                }
            }
        } else if (command.equals("inverseThresholdLUT")) {

            if (tabbedPane.getSelectedComponent() == panelA) {

                if (getHistoLUTComponentA() != null) {

                    // turn on threshold fields
                    // threshLowerF.setEnabled( true );
                    // threshUpperF.setEnabled( true );
                    // threshFillF.setEnabled( true );
                    getHistoLUTComponentA().dualThresholdMode(getHistoLUTComponentA().DUAL_THRESHOLD_INV);
                }
            } else {

                if (getHistoLUTComponentB() != null) {

                    // turn on threshold fields
                    // threshLowerBF.setEnabled( true );
                    // threshUpperBF.setEnabled( true );
                    // threshFillBF.setEnabled( true );
                    getHistoLUTComponentB().dualThresholdMode(getHistoLUTComponentB().DUAL_THRESHOLD_INV);
                }
            }
        } else if (command.equals("SaveFuncts")) {

            // save only the transfer functions
            saveLUTAs(false, null, null);

            if (displayMode == IMAGE_A) {
                getHistoLUTComponentA().showHistogram();

                if (regComponent != null) {
                    regComponent.setRGBTA(RGBTA);
                }
            } else {
                getHistoLUTComponentB().showHistogram();

                if (regComponent != null) {
                    regComponent.setRGBTB(RGBTB);
                }
            }

            updateFrames(false);

        } else if (command.equals("OpenFuncts")) {

            // open only the transfer functions
            loadLUTFrom(false, null, null, false);

            if (displayMode == IMAGE_A) {
                getHistoLUTComponentA().showHistogram();

                if (regComponent != null) {
                    regComponent.setRGBTA(RGBTA);
                }
            } else {
                getHistoLUTComponentB().showHistogram();

                if (regComponent != null) {
                    regComponent.setRGBTB(RGBTB);
                }
            }

            updateFrames(false);
        } else if (event.getActionCommand().equals("UpdateA")) {
            updateHistoRGB(imageA, null, false);
            // menuObj.setMenuItemEnabled( "Reset Histogram & LUT A", false );
        } else if (event.getActionCommand().equals("UpdateB")) {
            updateHistoRGB(null, imageB, false);
            // menuObj.setMenuItemEnabled( "Reset Histogram & LUT B", false );
        }
    }

    /**
     * Still needs more work Matt.
     */
    public void disposeLocal() {
        imageA = null;
        imageB = null;

        if (RGBTA != null) {
            RGBTA.disposeLocal();
            RGBTA = null;
        }

        if (RGBTB != null) {
            RGBTB.disposeLocal();
            RGBTB = null;
        }

        if (histoPanelA != null) {
            histoPanelA.finalize();
            histoPanelA = null;
        }

        if (histoPanelB != null) {
            histoPanelB.finalize();
            histoPanelB = null;
        }

        if (histogramARed != null) {
            histogramARed.disposeLocal();
            histogramARed = null;
        }

        if (histogramBRed != null) {
            histogramBRed.disposeLocal();
            histogramBRed = null;
        }

        if (histogramAGreen != null) {
            histogramAGreen.disposeLocal();
            histogramAGreen = null;
        }

        if (histogramBGreen != null) {
            histogramBGreen.disposeLocal();
            histogramBGreen = null;
        }

        if (histogramABlue != null) {
            histogramABlue.disposeLocal();
            histogramABlue = null;
        }

        if (histogramBBlue != null) {
            histogramBBlue.disposeLocal();
            histogramBBlue = null;
        }
    }

    /**
     * Placeholder.
     *
     * @param  mouseEvent  drag event
     */
    public void dragPoint(MouseEvent mouseEvent) { }

    /**
     * Get the histogram component for imageA.
     *
     * @return  the imageA histogram component
     */
    public final ViewJComponentHistoRGB getHistoLUTComponentA() {
        return (ViewJComponentHistoRGB) histoPanelA.getHistoLUTComponent();
    }

    /**
     * Get the histogram component for imageB.
     *
     * @return  the imageB histogram component
     */
    public final ViewJComponentHistoRGB getHistoLUTComponentB() {
        return (ViewJComponentHistoRGB) histoPanelB.getHistoLUTComponent();
    }

    /**
     * Accessor that returns the imageA.
     *
     * @return  image
     */
    public ModelImage getImageA() {
        return imageA;
    }

    /**
     * Accessor that returns the imageB.
     *
     * @return  imageB
     */
    public ModelImage getImageB() {
        return imageB;
    }

    /**
     * Get the control panel.
     *
     * @return  Container control panel.
     */
    public JPanel getMainPanel() {
        return mainPanel;
    }

    /**
     * {@inheritDoc}
     */
    public boolean isImageUpdate() {
        return bImageUpdate;
    }

    // ********************************************************************
    // ************************** Item Events *****************************
    // ********************************************************************

    /**
     * Sets the flags for the checkboxes.
     *
     * @param  event  event that triggered this function
     */
    public synchronized void itemStateChanged(ItemEvent event) {

        Object source = event.getSource();

        if (source == logCheckBoxA) {

            if (logCheckBoxA.isSelected() == true) {
                getHistoLUTComponentA().setLogFlag(true);
            } else {
                getHistoLUTComponentA().setLogFlag(false);
            }

            getHistoLUTComponentA().showHistogram();
        } else if (source == logCheckBoxB) {

            if (logCheckBoxB.isSelected() == true) {
                getHistoLUTComponentB().setLogFlag(true);
            } else {
                getHistoLUTComponentB().setLogFlag(false);
            }

            getHistoLUTComponentB().showHistogram();
        } else if (source == redCheckBoxA) {

            if (redCheckBoxA.isSelected() == true) {
                RGBTA.setROn(true);
            } else {
                RGBTA.setROn(false);
            }

            if (regComponent == null) {
                updateFrames(false);
            } else {

                // Not sure next is needed - Matt
                regComponent.show(0, 0, null, null, true);
            }
        } else if (source == greenCheckBoxA) {

            if (greenCheckBoxA.isSelected() == true) {
                RGBTA.setGOn(true);
            } else {
                RGBTA.setGOn(false);
            }

            if (regComponent == null) {
                updateFrames(false);
            } else {

                // Not sure next is needed - Matt
                regComponent.show(0, 0, null, null, true);
            }
        } else if (source == blueCheckBoxA) {

            if (blueCheckBoxA.isSelected() == true) {
                RGBTA.setBOn(true);
            } else {
                RGBTA.setBOn(false);
            }

            if (regComponent == null) {
                updateFrames(false);
            } else {

                // regComponent.setBaOn(RGBTA.getBOn());// Not sure next is needed
                regComponent.show(0, 0, null, null, true);
            }
        } else if (source == redCheckBoxB) {

            if (redCheckBoxB.isSelected() == true) {
                RGBTB.setROn(true);
            } else {
                RGBTB.setROn(false);
            }

            if (regComponent == null) {
                updateFrames(false);
            } else { // Not sure next is needed - Matt

                // regComponent.setRbOn(RGBTB.getROn());
                regComponent.show(0, 0, null, null, true);
            }
        } else if (source == greenCheckBoxB) {

            if (greenCheckBoxB.isSelected() == true) {
                RGBTB.setGOn(true);
            } else {
                RGBTB.setGOn(false);
            }

            if (regComponent == null) {
                updateFrames(false);
            } else { // Not sure next is needed - Matt

                // regComponent.setGbOn(RGBTB.getGOn());
                regComponent.show(0, 0, null, null, true);
            }
        } else if (source == blueCheckBoxB) {

            if (blueCheckBoxB.isSelected() == true) {
                RGBTB.setBOn(true);
            } else {
                RGBTB.setBOn(false);
            }

            if (regComponent == null) {
                updateFrames(false);
            } else { // Not sure next is needed - Matt

                // regComponent.setBbOn(RGBTB.getBOn());
                regComponent.show(0, 0, null, null, true);
            }
        }
    }

    /**
     * This method loads the LUT for the active image. If the image is not a color image then both the functions and the
     * LUT data are loaded. If this is a color image, then only the functions are loaded.
     *
     * @param  loadAll    loadAll boolean indicating that both lut and transfer functions should be loaded. If false,
     *                    then only transfer functions are loaded.
     * @param  filename   filename filename to save LUT as
     * @param  dirName    dirName directory to save LUT to
     * @param  quietMode  quietMode if true indicates that warnings should not be displayed.
     */
    public void loadLUTFrom(boolean loadAll, String filename, String dirName, boolean quietMode) {

        ModelRGB rgb = null;
        ModelLUT lut = null;
        ModelImage img;
        FileHistoLUT fileHistoLUT;
        boolean useLUT = false;

        if (displayMode == IMAGE_A) {
            img = this.getImageA();

            if (img.isColorImage()) {
                useLUT = false;
                rgb = RGBTA;
                lut = null;
            }
        } else {
            img = this.getImageB();

            if (img.isColorImage()) {
                useLUT = false;
                rgb = RGBTB;
                lut = null;
            }
        }

        // if not using a lut (i.e. rgb only), then you
        // can't loadAll.... there are only functions, so
        // reset the loadAll variable
        if (!useLUT) {
            loadAll = false;
        }

        if ((filename == null) || (dirName == null)) {
            dirName = img.getFileInfo(0).getFileDirectory();

            if (dirName == null) {
                dirName = System.getProperties().getProperty("user.dir");
            }

            JFileChooser chooser = new JFileChooser();

            chooser.setCurrentDirectory(new File(dirName));

            if (loadAll) {
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.LUT));
            } else {
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.FUNCT));
            }

            int returnVal = chooser.showOpenDialog(ViewUserInterface.getReference().getActiveImageFrame());

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                filename = chooser.getSelectedFile().getName();
                dirName = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            } else if (returnVal == JFileChooser.CANCEL_OPTION) {
                chooser.setVisible(false);

                return;
            }
        }

        try {

            if (useLUT) {
                fileHistoLUT = new FileHistoLUT(filename, dirName, lut);

                if (loadAll) {
                    fileHistoLUT.readLUTandTransferFunction(quietMode);
                } else {
                    fileHistoLUT.readFunctions();
                }

                /*
                 * if (displayMode == IMAGE_A) { setLUTa(lut); } else { setLUTb(lut); }
                 */
            } else {
                fileHistoLUT = new FileHistoLUT(filename, dirName, rgb);

                if (loadAll) {
                    fileHistoLUT.readLUTandTransferFunction(quietMode);
                } else {
                    fileHistoLUT.readFunctions();
                }

                if (displayMode == IMAGE_A) {
                    this.setRGBTA(rgb);
                } else {
                    this.setRGBTB(rgb);
                }
            }

            img.notifyImageDisplayListeners(lut, true);

        } catch (IOException error) {

            if (!quietMode) {
                MipavUtil.displayError("Error reading LUT: \n" + error.getMessage());
            }
        }
    } // end loadLUTFrom()

    /**
     * Unchanged.
     *
     * @param  mouseEvent  mouse event handler.
     */
    public void mouseClicked(MouseEvent mouseEvent) { }

    // ************************************************************************
    // ************************** Mouse Motion Events *************************
    // ************************************************************************

    /**
     * Unchanged.
     *
     * @param  mouseEvent  mouse event handler.
     */
    public void mouseDragged(MouseEvent mouseEvent) { }

    /**
     * Unchanged.
     *
     * @param  mouseEvent  mouse event handler.
     */
    public void mouseEntered(MouseEvent mouseEvent) { }

    /**
     * Unchanged.
     *
     * @param  mouseEvent  mouse event handler.
     */
    public void mouseExited(MouseEvent mouseEvent) { }

    /**
     * Unchanged.
     *
     * @param  mouseEvent  mouse event handler.
     */
    public void mouseMoved(MouseEvent mouseEvent) { }

    /**
     * Unchanged.
     *
     * @param  mouseEvent  mouse event handler.
     */
    public void mousePressed(MouseEvent mouseEvent) { }

    /**
     * Unchanged.
     *
     * @param  mouseEvent  mouse event handler.
     */
    public void mouseReleased(MouseEvent mouseEvent) { }

    /**
     * Removes the tabbed pane for the histogram of image B.
     */
    public void removeHistoRGBb() {

        if (tabbedPane.getTabCount() == 2) {
            tabbedPane.removeTabAt(1);
            imageB = null;
            panelB = null;
            tabbedPane.validate();
        }
    }

    /**
     * Resizig the control panel with ViewJFrameVolumeView's frame width and height.
     *
     * @param  panelWidth   width
     * @param  frameHeight  height
     */
    public void resizePanel(int panelWidth, int frameHeight) {
        scroller.setPreferredSize(new Dimension(panelWidth, frameHeight - (toolBar.getHeight() * 2)));
        scroller.setSize(new Dimension(panelWidth, frameHeight - (toolBar.getHeight() * 2)));
        scroller.revalidate();
    }

    /**
     * This method saves the LUT for the active image. If the image is not a color image then both the functions and the
     * LUT data are saved. If this is a color image, then only the functions are saved.
     *
     * @param  saveAll   boolean indicating that both lut and transfer functions should be saved. If false, then only
     *                   transfer functions are saved.
     *
     *                   <p>If either filename or directory is null, then the user will be prompted for a filename.</p>
     * @param  filename  filename to save LUT as
     * @param  dirName   directory to save LUT to
     */
    public void saveLUTAs(boolean saveAll, String filename, String dirName) {

        ModelRGB rgb = null;
        ModelLUT lut = null;
        ModelImage img;
        FileHistoLUT fileHistoLUT;
        boolean useLUT = false;

        if (displayMode == IMAGE_A) {
            img = this.getImageA();

            if (img.isColorImage()) {
                useLUT = false;
                rgb = RGBTA;
                lut = null;
            }
        } else {
            img = this.getImageB();

            if (img.isColorImage()) {
                useLUT = false;
                rgb = RGBTB;
                lut = null;
            }
        }

        // if not using a lut (i.e. rgb only), then you
        // can't saveAll.... there are only functions, so
        // reset the saveAll variable
        if (!useLUT) {
            saveAll = false;
        }

        // if filename and/or dirName is null, then get it from user
        if ((filename == null) || (dirName == null)) {
            dirName = img.getFileInfo(0).getFileDirectory();

            if (dirName == null) {
                dirName = System.getProperties().getProperty("user.dir");
            }

            JFileChooser chooser = new JFileChooser();

            chooser.setCurrentDirectory(new File(dirName));

            if (saveAll) {
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.LUT));
            } else {
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.FUNCT));
            }

            int returnVal = chooser.showSaveDialog(ViewUserInterface.getReference().getActiveImageFrame());

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                filename = chooser.getSelectedFile().getName();
                dirName = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            } else if (returnVal == JFileChooser.CANCEL_OPTION) {
                chooser.setVisible(false);

                return;
            }

        }

        try {

            if (useLUT) {
                fileHistoLUT = new FileHistoLUT(filename, dirName, lut);
            } else {
                fileHistoLUT = new FileHistoLUT(filename, dirName, rgb);
            }

            if (saveAll) {
                fileHistoLUT.writeAll();
            } else {
                fileHistoLUT.writeFunctions();
            }

        } catch (IOException error) {
            MipavUtil.displayError("Error writing LUT: \n" + error.getMessage());
        }

    } // end saveLUTAs()

    /**
     * HistoLUTParent interface.
     */

    /**
     * {@inheritDoc}
     */
    public void setAllOff() {
        allRGBButton.setEnabled(false);
    }

    /**
     * Sets the alpha blending of parameter for two image displaying.
     *
     * @param  value  amount [0,100] that is the percentage of Image A to be displayed and (1-percentage) of Image B to
     *                be displayed
     */
    public void setAlphaBlend(int value) { /* alphaBlend = value / 100.0f;*/
    }

    /**
     * Accessor that sets the imageA.
     *
     * @param  image  image to set frame to
     */
    public void setImageA(ModelImage image) {
        imageA = image;
    }

    /**
     * Accessor that sets the imageB.
     *
     * @param  image  image to set frame to
     */
    public void setImageB(ModelImage image) {
        imageB = image;
    }

    /**
     * Placeholder.
     *
     * @param  newLUT  lut
     */
    public void setLUT(ModelLUT newLUT) { }

    /**
     * Placeholder.
     *
     * @param  x       x
     * @param  y       y
     * @param  _index  index
     */
    public void setRangeText(float x, float y, int _index) { }

    /**
     * The following 2 functions set the RGB tables for images A and B.
     *
     * @param  RGBT  RGB table reference.
     */
    public void setRGBTA(ModelRGB RGBT) {
        RGBTA = RGBT;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  RGBT  RGB table reference.
     */
    public void setRGBTB(ModelRGB RGBT) {
        RGBTB = RGBT;
    }

    public void setRedOn( boolean bOn, boolean bImageA )
    {
        if ( bImageA )
        {
            redCheckBoxA.setSelected(bOn);
        }
        else
        {
            redCheckBoxB.setSelected(bOn);
        }
    }
    
    public void setGreenOn( boolean bOn, boolean bImageA )
    {
        if ( bImageA )
        {
            greenCheckBoxA.setSelected(bOn);
        }
        else
        {
            greenCheckBoxB.setSelected(bOn);
        }
    }

    public void setBlueOn( boolean bOn, boolean bImageA )
    {
        if ( bImageA )
        {
            blueCheckBoxA.setSelected(bOn);
        }
        else
        {
            blueCheckBoxB.setSelected(bOn);
        }
    }
    
    /**
     * Resets the buttons depending on which tab was selected.
     *
     * @param  e  Event that triggered this function.
     */
    public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();
        int modeA, modeB;

        if ((source == tabbedPane) && (tabbedPane.getSelectedComponent() == panelA)) {
            displayMode = IMAGE_A;
            modeA = getHistoLUTComponentA().getMode();

            if (modeA == getHistoLUTComponentA().RED) {
                redRGBButton.setBorderPainted(true);
                redRGBButton.setSelected(true);
                greenRGBButton.setBorderPainted(false);
                blueRGBButton.setBorderPainted(false);
                allRGBButton.setBorderPainted(false);
            } else if (modeA == getHistoLUTComponentA().GREEN) {
                redRGBButton.setBorderPainted(false);
                greenRGBButton.setBorderPainted(true);
                greenRGBButton.setSelected(true);
                blueRGBButton.setBorderPainted(false);
                allRGBButton.setBorderPainted(false);
            } else if (modeA == getHistoLUTComponentA().BLUE) {
                redRGBButton.setBorderPainted(false);
                greenRGBButton.setBorderPainted(false);
                blueRGBButton.setBorderPainted(true);
                blueRGBButton.setSelected(true);
                allRGBButton.setBorderPainted(false);
            } else if (modeA == getHistoLUTComponentA().ALL) {
                redRGBButton.setBorderPainted(false);
                greenRGBButton.setBorderPainted(false);
                blueRGBButton.setBorderPainted(false);
                allRGBButton.setSelected(true);
                allRGBButton.setBorderPainted(true);
            }
        } else if ((source == tabbedPane) && (tabbedPane.getSelectedComponent() == panelB) && (imageB != null)) {
            displayMode = IMAGE_B;
            modeB = getHistoLUTComponentB().getMode();

            if (modeB == getHistoLUTComponentB().RED) {
                redRGBButton.setBorderPainted(true);
                redRGBButton.setSelected(true);
                greenRGBButton.setBorderPainted(false);
                blueRGBButton.setBorderPainted(false);
                allRGBButton.setBorderPainted(false);
            } else if (modeB == getHistoLUTComponentB().GREEN) {
                redRGBButton.setBorderPainted(false);
                greenRGBButton.setBorderPainted(true);
                greenRGBButton.setSelected(true);
                blueRGBButton.setBorderPainted(false);
                allRGBButton.setBorderPainted(false);
            } else if (modeB == getHistoLUTComponentB().BLUE) {
                redRGBButton.setBorderPainted(false);
                greenRGBButton.setBorderPainted(false);
                blueRGBButton.setBorderPainted(true);
                blueRGBButton.setSelected(true);
                allRGBButton.setBorderPainted(false);
            } else if (modeB == getHistoLUTComponentB().ALL) {
                redRGBButton.setBorderPainted(false);
                greenRGBButton.setBorderPainted(false);
                blueRGBButton.setBorderPainted(false);
                allRGBButton.setSelected(true);
                allRGBButton.setBorderPainted(true);
            }
        }
    }

    /**
     * Redisplay histoLUT.
     */
    public void update() {

        if (tabbedPane.getSelectedComponent() == panelA) {
            getHistoLUTComponentA().showHistogram();
        } else if (tabbedPane.getSelectedComponent() == panelB) {
            getHistoLUTComponentB().showHistogram();
        }

    }

    /**
     * Placeholder.
     */
    public void updateComponentLUT() { }

    /**
     * {@inheritDoc}
     */
    public void updateFrames(boolean flag) {

        if (imageA != null) {

            if ((redCheckBoxA != null) && (greenCheckBoxA != null) && (blueCheckBoxA != null) && (RGBTA != null)) {

                if (regComponent == null) {
                    imageA.notifyImageDisplayListeners(flag, (int) ((alphaBlend * 100) + 0.5), RGBTA);
                } else {
                    regComponent.setRGBTA(RGBTA);
                    regComponent.show(0, 0, null, null, true);
                }
            } else {
                imageA.notifyImageDisplayListeners(null, flag);
            }
        }

        if (imageB != null) {

            if ((redCheckBoxB != null) && (greenCheckBoxB != null) && (blueCheckBoxB != null) && (RGBTB != null)) {

                if (regComponent == null) {
                    imageB.notifyImageDisplayListeners(flag, (int) ((alphaBlend * 100) + 0.5), RGBTB);
                } else {
                    regComponent.setRGBTB(RGBTB);
                    regComponent.show(0, 0, null, null, true);
                }
            } else {
                imageB.notifyImageDisplayListeners(null, flag);
            }
        }
    }

    /**
     * This method is called to update the histogram(s) displayed in each tabbed pane of the frame.
     *
     * @param  _imageA       image A
     * @param  _imageB       image B
     * @param  progressFlag  passed to calculateHistogram algorithm. If false progress bar is not displayed.
     */
    public void updateHistoRGB(ModelImage _imageA, ModelImage _imageB, boolean progressFlag) {
        int modeA, modeB;

        if (_imageA != null) {
            setImageA(_imageA);

            calcHistogram(IMAGE_A, entireFlag, progressFlag);
            modeA = getHistoLUTComponentA().getMode();

            if (modeA == getHistoLUTComponentA().RED) {
                getHistoLUTComponentA().setHistogramInfo(imageA, histogramARed);
            } else if (modeA == getHistoLUTComponentA().GREEN) {
                getHistoLUTComponentA().setHistogramInfo(imageA, histogramAGreen);
            } else if (modeA == getHistoLUTComponentA().BLUE) {
                getHistoLUTComponentA().setHistogramInfo(imageA, histogramABlue);
            }

            getHistoLUTComponentA().showHistogram();
        }

        if ((_imageB != null) && (panelB == null)) {
            setImageB(_imageB);
            buildPanelB(imageB, true);
        } else if ((_imageB != null) && (imageB != null)) {
            setImageB(_imageB);
            calcHistogram(IMAGE_B, entireFlag, progressFlag);
            modeB = getHistoLUTComponentB().getMode();

            if (modeB == getHistoLUTComponentB().RED) {
                getHistoLUTComponentB().setHistogramInfo(imageB, histogramBRed);
            } else if (modeB == getHistoLUTComponentB().GREEN) {
                getHistoLUTComponentB().setHistogramInfo(imageB, histogramBGreen);
            } else if (modeB == getHistoLUTComponentB().BLUE) {
                getHistoLUTComponentB().setHistogramInfo(imageB, histogramBBlue);
            }

            getHistoLUTComponentB().showHistogram();
        }

        tabbedPane.validate();
    }

    /**
     * Placeholder.
     *
     * @param  str  string
     */
    public void updateLUTPositionString(String str) { }

    /**
     * Placeholder.
     *
     * @param  lower  lower threshold
     * @param  upper  upper threshold
     */
    public void updateThresholdFields(float lower, float upper) { }

    /**
     * Calls dispose.
     *
     * @throws  Throwable   throw exception.
     */
    protected void finalize() throws Throwable {
        this.disposeLocal();
        super.finalize();
    }

    /**
     * Tests that the entered parameter is in range.
     *
     * @param   str       the value entered by the user
     * @param   minValue  the minimum value this variable may be set to
     * @param   maxValue  the maximum value this variable may be set to
     *
     * @return  boolean result of test
     */
    protected boolean testParameter(String str, double minValue, double maxValue) {

        double tmp;

        try {
            tmp = Double.valueOf(str).doubleValue();

            if ((tmp > maxValue) || (tmp < minValue)) {
                MipavUtil.displayError("Value is out of range.");

                return false;
            } else {
                return true;
            }
        } catch (NumberFormatException error) {
            MipavUtil.displayError("Must enter numeric value");

            return false;
        }
    }

    /**
     * This method builds a menu which contains the options for opening/saving a LUT or set of transfer functions,
     * closing the LUT, and utilities such as CT presets.
     */
    private void buildMenu() { /*
                                * JMenuBar menuBar;
                                *
                                * try { menuObj = new ViewMenuBuilder( this ); menuBar = new JMenuBar(); JSeparator
                                * separator = new JSeparator();
                                *
                                * menuBar.add( menuObj.makeMenu( "File", 'F', new JComponent[] { menuObj.buildMenuItem(
                                * "Open LUT", "OpenLUT", KeyStroke.getKeyStroke( 'O', Event.CTRL_MASK, false ),
                                * "open.gif" ), menuObj.buildMenuItem( "Save LUT", "SaveLUT", KeyStroke.getKeyStroke(
                                * 'S', Event.CTRL_MASK, false ), "save.gif" ), menuObj.buildMenuItem( "Open Transfer
                                * Functions", "OpenFuncts", null, "open.gif" ), menuObj.buildMenuItem( "Save Transfer
                                * Functions", "SaveFuncts", null, "save.gif" ), separator, menuObj.buildMenuItem( "Close
                                * LUT", "CloseLUT", KeyStroke.getKeyStroke( 'X', Event.CTRL_MASK, false ), null ) } ) );
                                * menuBar.add( menuObj.makeMenu( "Utilities", 'U', new JComponent[] {
                                * menuObj.buildMenuItem( "Reset Transfer Function", "Linear", null, null ),
                                * menuObj.buildMenuItem( "Reset Histogram & LUT A", "UpdateA", null, null ),
                                * menuObj.buildMenuItem( "Reset Histogram & LUT B", "UpdateB", null, null ), } ) );
                                * menuObj.setMenuItemEnabled( "Reset Histogram & LUT A", false );
                                * menuObj.setMenuItemEnabled( "Reset Histogram & LUT B", false );
                                *
                                * } catch ( OutOfMemoryError error ) { MipavUtil.displayError( "Out of memory:
                                * ViewJFrameHistoLUT.buildMenu" ); return; } setJMenuBar( menuBar );
                                */
    }

    /**
     * Method that displays the histogram and LUT and other controls to manipulate the LUT. Panel for image A.
     *
     * @param  image       Model of image
     * @param  entireFlag  Flag indicating if histogram should be made of entire image.
     */
    private void buildPanelA(ModelImage image, boolean entireFlag) {
        calcHistogram(IMAGE_A, entireFlag, false);

        histoPanelA = new ViewJPanelHistoLUT(this, image, RGBTA, histogramARed);

        JPanel controlPanel = new JPanel(new BorderLayout());

        controlPanel.setBorder(new EtchedBorder());
        /*
                updateCheckBoxA = new JCheckBox( "Update (real-time)", true );
                updateCheckBoxA.setFont( MipavUtil.font12 );
                updateCheckBoxA.addItemListener( this );
                updateCheckBoxA.setSelected( false );
        */
        logCheckBoxA = new JCheckBox("Log scale (Histogram)", true);
        logCheckBoxA.setFont(MipavUtil.font12);
        logCheckBoxA.addItemListener(this);

        JLabel componentsLabel = new JLabel("Image components: ");

        componentsLabel.setFont(MipavUtil.font12B);
        componentsLabel.setForeground(Color.black);

        redCheckBoxA = new JCheckBox("Red", true);
        redCheckBoxA.setFont(MipavUtil.font12);
        redCheckBoxA.addItemListener(this);
        RGBTA.setROn(true);

        greenCheckBoxA = new JCheckBox("Green", true);
        greenCheckBoxA.setFont(MipavUtil.font12);
        greenCheckBoxA.addItemListener(this);
        RGBTA.setGOn(true);

        blueCheckBoxA = new JCheckBox("Blue", true);
        blueCheckBoxA.setFont(MipavUtil.font12);
        blueCheckBoxA.addItemListener(this);
        RGBTA.setBOn(true);

        Box upPanel = new Box(BoxLayout.Y_AXIS);

        // upPanel.add( updateCheckBoxA );
        upPanel.add(logCheckBoxA);

        Box lowPanel = new Box(BoxLayout.X_AXIS);

        lowPanel.add(componentsLabel);
        lowPanel.add(redCheckBoxA);
        lowPanel.add(greenCheckBoxA);
        lowPanel.add(blueCheckBoxA);

        controlPanel.add(upPanel, BorderLayout.CENTER);
        controlPanel.add(lowPanel, BorderLayout.SOUTH);

        panelA = new JPanel(new BorderLayout());
        panelA.add(controlPanel, BorderLayout.CENTER);
        panelA.add(histoPanelA, BorderLayout.SOUTH);

        getHistoLUTComponentA().setMode(getHistoLUTComponentA().RED);
        getHistoLUTComponentA().setMode(getHistoLUTComponentA().ALL);

        tabbedPane.addTab("ImageA", null, panelA);
        tabbedPane.setFont(MipavUtil.font12B);
    }

    /**
     * Method that displays the histogram and LUT and other controls to manipulate the LUT. Panel for image B.
     *
     * @param  image       Model of image
     * @param  entireFlag  Flag indicating if histogram should be made of entire image.
     */
    private void buildPanelB(ModelImage image, boolean entireFlag) {

        // go calc histo
        calcHistogram(IMAGE_B, entireFlag, false);

        histoPanelB = new ViewJPanelHistoLUT(this, image, RGBTB, histogramBRed);

        JPanel controlPanelB = new JPanel(new BorderLayout());

        controlPanelB.setBorder(new EtchedBorder());
        /*
                updateCheckBoxB = new JCheckBox( "Update (real-time)", true );
                updateCheckBoxB.setFont( MipavUtil.font12 );
                updateCheckBoxB.addItemListener( this );
                updateCheckBoxB.setSelected( false );
        */
        logCheckBoxB = new JCheckBox("Log scale (Histogram)", true);
        logCheckBoxB.setFont(MipavUtil.font12);
        logCheckBoxB.addItemListener(this);

        JLabel componentsLabel = new JLabel("Image components: ");

        componentsLabel.setFont(MipavUtil.font12B);
        componentsLabel.setForeground(Color.black);
        controlPanelB.add(componentsLabel);

        redCheckBoxB = new JCheckBox("Red", true);
        redCheckBoxB.setFont(MipavUtil.font12);
        redCheckBoxB.addItemListener(this);
        RGBTB.setROn(true);

        greenCheckBoxB = new JCheckBox("Green", true);
        greenCheckBoxB.setFont(MipavUtil.font12);
        greenCheckBoxB.addItemListener(this);
        RGBTB.setGOn(true);

        blueCheckBoxB = new JCheckBox("Blue", true);
        blueCheckBoxB.setFont(MipavUtil.font12);
        blueCheckBoxB.addItemListener(this);
        RGBTB.setBOn(true);

        Box upPanel = new Box(BoxLayout.Y_AXIS);

        // upPanel.add( updateCheckBoxB );
        upPanel.add(logCheckBoxB);

        Box lowPanel = new Box(BoxLayout.X_AXIS);

        lowPanel.add(componentsLabel);
        lowPanel.add(redCheckBoxB);
        lowPanel.add(greenCheckBoxB);
        lowPanel.add(blueCheckBoxB);

        controlPanelB.add(upPanel, BorderLayout.CENTER);
        controlPanelB.add(lowPanel, BorderLayout.SOUTH);

        panelB = new JPanel(new BorderLayout());
        panelB.add(controlPanelB, BorderLayout.CENTER);
        panelB.add(histoPanelB, BorderLayout.SOUTH);

        getHistoLUTComponentB().setMode(getHistoLUTComponentB().RED);
        getHistoLUTComponentB().setMode(getHistoLUTComponentB().ALL);

        tabbedPane.addTab("ImageB", null, panelB);
        tabbedPane.setFont(MipavUtil.font12B);
    }

    /**
     * Calculates histogram for the image(s).
     *
     * @param  imageAorB     flag to indicate if histogram is to be calculated for imageA or imageB.
     * @param  entireFlag    if true calculate histogram for the entire image. if false uses areas defined by VOI
     *                       regions.
     * @param  progressFlag  passed to calculateHistogram algorithm. If false progress bar is not displayed.
     */
    private void calcHistogram(int imageAorB, boolean entireFlag, boolean progressFlag) {

        int[] dimExtentsA = new int[1];
        int[] dimExtentsB = new int[1];
        int offset = 0;

        if ((imageA != null) && (imageAorB == IMAGE_A)) {

            if (imageA.getType() != ModelStorageBase.ARGB_USHORT) {
                dimExtentsA[0] = 256;
            } else {
                dimExtentsA[0] = (int) (imageA.getMaxR() - imageA.getMinR() + 0.5) + 1;
            }

            histogramARed = new ModelHistogram(ModelStorageBase.INTEGER, dimExtentsA);
            offset = 1;

            AlgorithmHistogram histoAlgoARed = new AlgorithmHistogram(histogramARed, offset, imageA, entireFlag);

            histoAlgoARed.run();

            if (imageA.getType() != ModelStorageBase.ARGB_USHORT) {
                dimExtentsA[0] = 256;
            } else {
                dimExtentsA[0] = (int) (imageA.getMaxG() - imageA.getMinG() + 0.5) + 1;
            }

            histogramAGreen = new ModelHistogram(ModelStorageBase.INTEGER, dimExtentsA);
            offset = 2;

            AlgorithmHistogram histoAlgoAGreen = new AlgorithmHistogram(histogramAGreen, offset, imageA, entireFlag);

            histoAlgoAGreen.run();

            if (imageA.getType() != ModelStorageBase.ARGB_USHORT) {
                dimExtentsA[0] = 256;
            } else {
                dimExtentsA[0] = (int) (imageA.getMaxB() - imageA.getMinB() + 0.5) + 1;
            }

            histogramABlue = new ModelHistogram(ModelStorageBase.INTEGER, dimExtentsA);
            offset = 3;

            AlgorithmHistogram histoAlgoABlue = new AlgorithmHistogram(histogramABlue, offset, imageA, entireFlag);

            histoAlgoABlue.run();
        }

        if ((imageB != null) && (imageAorB == IMAGE_B)) {

            if (imageB.getType() != ModelStorageBase.ARGB_USHORT) {
                dimExtentsB[0] = 256;
            } else {
                dimExtentsB[0] = (int) (imageB.getMaxR() - imageB.getMinR() + 0.5) + 1;
            }

            histogramBRed = new ModelHistogram(ModelStorageBase.INTEGER, dimExtentsB);
            offset = 1;

            AlgorithmHistogram histoAlgoBRed = new AlgorithmHistogram(histogramBRed, offset, imageB, entireFlag);

            histoAlgoBRed.run();

            if (imageB.getType() != ModelStorageBase.ARGB_USHORT) {
                dimExtentsB[0] = 256;
            } else {
                dimExtentsB[0] = (int) (imageB.getMaxG() - imageB.getMinG() + 0.5) + 1;
            }

            histogramBGreen = new ModelHistogram(ModelStorageBase.INTEGER, dimExtentsB);
            offset = 2;

            AlgorithmHistogram histoAlgoBGreen = new AlgorithmHistogram(histogramBGreen, offset, imageB, entireFlag);

            histoAlgoBGreen.run();

            if (imageB.getType() != ModelStorageBase.ARGB_USHORT) {
                dimExtentsB[0] = 256;
            } else {
                dimExtentsB[0] = (int) (imageB.getMaxB() - imageB.getMinB() + 0.5) + 1;
            }

            histogramBBlue = new ModelHistogram(ModelStorageBase.INTEGER, dimExtentsB);
            offset = 3;

            AlgorithmHistogram histoAlgoBBlue = new AlgorithmHistogram(histogramBBlue, offset, imageB, entireFlag);

            histoAlgoBBlue.run();
        }
    }

    /**
     * Accessor that indicates which image is active:
     * @return displayMode = IMAGE_A or IMAGE_B
     */
    public int getDisplayMode()
    {
        return displayMode;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Wrapper in order to hold the control panel layout in the JScrollPane.
     */
    class DrawingPanel extends JPanel {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -1286961329861675276L;

        /**
         * Wrapper to repaing the panel.
         *
         * @param  g  graphics reference. 
         */
        protected void paintComponent(Graphics g) {
            super.paintComponent(g);

        }
    }

}
