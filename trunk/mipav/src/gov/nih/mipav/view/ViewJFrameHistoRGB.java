package gov.nih.mipav.view;

import WildMagic.LibFoundation.Mathematics.Vector2f;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;


/**
 * This class produces a frame where the histogram of the image data is displayed using the color mapping. All frames
 * using the color map are dynamically updated with the new color map.
 *
 * @version  1.0
 * @author   Matthew J. McAuliffe, Ph.D. (primary)
 * @author   Harman Singh
 * @see      ViewJComponentHistoRGB
 */
public class ViewJFrameHistoRGB extends ViewJFrameBase
        implements ItemListener, ActionListener, KeyListener, WindowListener, ChangeListener, ViewImageUpdateInterface,
                   MouseMotionListener, MouseListener, HistoLUTParent {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -4685406207107911894L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JCheckBox blueCheckBoxA, blueCheckBoxB;

    /** DOCUMENT ME! */
    private boolean calcThresholdVolume = true;

    /** DOCUMENT ME! */
    private boolean entireFlag = true;

    /** DOCUMENT ME! */
    private float[] fillValuesA = new float[3];

    /** DOCUMENT ME! */
    private float[] fillValuesB = new float[3];

    /** DOCUMENT ME! */
    private JCheckBox greenCheckBoxA, greenCheckBoxB;

    /** DOCUMENT ME! */
    private ModelHistogram histogramABlue = null;

    /** DOCUMENT ME! */
    private ModelHistogram histogramAGreen = null;

    /** DOCUMENT ME! */
    private ModelHistogram histogramARed = null;

    /** DOCUMENT ME! */
    private ModelHistogram histogramBBlue = null;

    /** DOCUMENT ME! */
    private ModelHistogram histogramBGreen = null;

    /** DOCUMENT ME! */
    private ModelHistogram histogramBRed = null;

    /** DOCUMENT ME! */
    private ViewJPanelHistoLUT histoPanelA;

    /** DOCUMENT ME! */
    private ViewJPanelHistoLUT histoPanelB;

    /** DOCUMENT ME! */
    private JCheckBox logCheckBoxA, logCheckBoxB;

    /** DOCUMENT ME! */
    private ViewMenuBuilder menuObj;

    /** DOCUMENT ME! */
    private JPanel panelA, panelB;

    /** DOCUMENT ME! */
    private JCheckBox redCheckBoxA, redCheckBoxB;

    /** DOCUMENT ME! */
    private JToggleButton redRGBButton, blueRGBButton, greenRGBButton, allRGBButton;

    /** DOCUMENT ME! */
    private ViewJComponentRegistration regComponent = null;

    /** DOCUMENT ME! */
    private int[] RGBExtents = new int[2];

    /** DOCUMENT ME! */
    private ModelRGB RGBTA;

    /** DOCUMENT ME! */
    private ModelRGB RGBTB;

    /** DOCUMENT ME! */
    private JTabbedPane tabbedPane;

    /** DOCUMENT ME! */
    private JTextField threshLowerBF, threshUpperBF, threshFillBF;

    /** threshold related. */
    private JTextField threshLowerF, threshUpperF, threshFillF;

    /** DOCUMENT ME! */
    private JToolBar toolBar;

    /** Object which generates the toolbar. */
    private ViewToolBarBuilder toolBarObj;

    /** DOCUMENT ME! */
    private JCheckBox updateCheckBoxA = null, updateCheckBoxB = null;

    /** DOCUMENT ME! */
    private boolean[] useThresholdChannelA = new boolean[3];

    /** DOCUMENT ME! */
    private boolean[] useThresholdChannelB = new boolean[3];

    /** DOCUMENT ME! */
    private JLabel voxelVolumeLabel, voxelVolumeLabelB;
    
    /** DOCUMENT ME **/
    private JCheckBox lutAdjustCheckboxB;

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
    public ViewJFrameHistoRGB(ModelImage _imageA, ModelImage _imageB, ModelRGB _RGBTA, ModelRGB _RGBTB,
                              boolean _entireFlag) {

        super(_imageA, _imageB);

        RGBTA = _RGBTA;
        RGBTB = _RGBTB;

        buildMenu();

        entireFlag = _entireFlag;
        setTitle("Lookup Table: " + _imageA.getImageName());

        RGBExtents[0] = 4;
        RGBExtents[1] = 256;

        getContentPane().setLayout(new BorderLayout());

        try {
            setIconImage(MipavUtil.getIconImage("histolut.gif"));
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                              ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <" + error.getMessage() +
                               ">.  Check that this file is available.\n");
        }

        setLocation(200, 200);
        setBackground(new Color(160, 160, 160));

        toolBarObj = new ViewToolBarBuilder(this);
        toolBar = toolBarObj.buildRGBToolBar();
        redRGBButton = (JToggleButton) toolBar.getComponentAtIndex(0);
        greenRGBButton = (JToggleButton) toolBar.getComponentAtIndex(1);
        blueRGBButton = (JToggleButton) toolBar.getComponentAtIndex(2);
        allRGBButton = (JToggleButton) toolBar.getComponentAtIndex(3);

        getContentPane().add(toolBar, BorderLayout.NORTH);


        tabbedPane = new JTabbedPane();
        tabbedPane.setFont(MipavUtil.font12B);
        buildPanelA(imageA, entireFlag);

        if (imageB != null) {
            buildPanelB(imageB, entireFlag);
        }

        tabbedPane.setSelectedIndex(0);
        getContentPane().add(tabbedPane);
        tabbedPane.addChangeListener(this);
        pack();
        addWindowListener(this);
        setResizable(false);

        // make sure that RGB xfer function is selected (all)
        allRGBButton.setSelected(true);
        actionPerformed(new ActionEvent(allRGBButton, 0, "all"));

        setVisible(true);
    }

    /**
     * Makes a frame of the histogram.
     *
     * @param  _regComponent  component to pass parameters to in manual registration
     * @param  _imageA        Model of imageA
     * @param  _imageB        Model of imageB
     * @param  _RGBTA         Model RGB
     * @param  _RGBTB         Model RGB
     * @param  _entireFlag    DOCUMENT ME!
     */
    public ViewJFrameHistoRGB(ViewJComponentRegistration _regComponent, ModelImage _imageA, ModelImage _imageB,
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
                    getHistoLUTComponentA();
					getHistoLUTComponentA().setMode(ViewJComponentHistoRGB.RED);
                    getHistoLUTComponentA().setHistogramInfo(imageA, histogramARed);

                    getHistoLUTComponentA();
					if (getHistoLUTComponentA().getThresholdMode() != ViewJComponentHistoRGB.NO_THRESHOLD) {
                        getHistoLUTComponentA().dualThresholdMode(getHistoLUTComponentA().getThresholdMode());
                    }

                    getHistoLUTComponentA().showHistogram();
                }
            } else {

                if (getHistoLUTComponentB() != null) {
                    getHistoLUTComponentB();
					getHistoLUTComponentB().setMode(ViewJComponentHistoRGB.RED);
                    getHistoLUTComponentB().setHistogramInfo(imageA, histogramBRed);

                    getHistoLUTComponentB();
					if (getHistoLUTComponentB().getThresholdMode() != ViewJComponentHistoRGB.NO_THRESHOLD) {
                        getHistoLUTComponentB().dualThresholdMode(getHistoLUTComponentB().getThresholdMode());
                    }

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
                    getHistoLUTComponentA();
					getHistoLUTComponentA().setMode(ViewJComponentHistoRGB.GREEN);
                    getHistoLUTComponentA().setHistogramInfo(imageA, histogramAGreen);

                    getHistoLUTComponentA();
					if (getHistoLUTComponentA().getThresholdMode() != ViewJComponentHistoRGB.NO_THRESHOLD) {
                        getHistoLUTComponentA().dualThresholdMode(getHistoLUTComponentA().getThresholdMode());
                    }

                    getHistoLUTComponentA().showHistogram();
                }
            } else {

                if (getHistoLUTComponentB() != null) {
                    getHistoLUTComponentB();
					getHistoLUTComponentB().setMode(ViewJComponentHistoRGB.GREEN);
                    getHistoLUTComponentB().setHistogramInfo(imageA, histogramBGreen);

                    getHistoLUTComponentB();
					if (getHistoLUTComponentB().getThresholdMode() != ViewJComponentHistoRGB.NO_THRESHOLD) {
                        getHistoLUTComponentB().dualThresholdMode(getHistoLUTComponentB().getThresholdMode());
                    }

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
                    getHistoLUTComponentA();
					getHistoLUTComponentA().setMode(ViewJComponentHistoRGB.BLUE);
                    getHistoLUTComponentA().setHistogramInfo(imageA, histogramABlue);

                    getHistoLUTComponentA();
					if (getHistoLUTComponentA().getThresholdMode() != ViewJComponentHistoRGB.NO_THRESHOLD) {
                        getHistoLUTComponentA().dualThresholdMode(getHistoLUTComponentA().getThresholdMode());
                    }

                    getHistoLUTComponentA().showHistogram();
                }
            } else {

                if (getHistoLUTComponentB() != null) {
                    getHistoLUTComponentB();
					getHistoLUTComponentB().setMode(ViewJComponentHistoRGB.BLUE);
                    getHistoLUTComponentB().setHistogramInfo(imageA, histogramBBlue);

                    getHistoLUTComponentB();
					if (getHistoLUTComponentB().getThresholdMode() != ViewJComponentHistoRGB.NO_THRESHOLD) {
                        getHistoLUTComponentB().dualThresholdMode(getHistoLUTComponentB().getThresholdMode());
                    }

                    getHistoLUTComponentB().showHistogram();
                }
            }
        } else if (command.equals("all")) {
            clearVoxelLabel();
            redRGBButton.setBorderPainted(false);
            greenRGBButton.setBorderPainted(false);
            blueRGBButton.setBorderPainted(false);
            allRGBButton.setBorderPainted(true);

            if (tabbedPane.getSelectedComponent() == panelA) {

                if (getHistoLUTComponentA() != null) {
                    getHistoLUTComponentA();
					getHistoLUTComponentA().setMode(ViewJComponentHistoRGB.ALL);
                }
            } else {

                if (getHistoLUTComponentB() != null) {
                    getHistoLUTComponentB();
					getHistoLUTComponentB().setMode(ViewJComponentHistoRGB.ALL);
                }
            }
        } else if (command.equals("linearLUT")) {
            toolBar.getComponentAtIndex(6).setEnabled(true);
            toolBar.getComponentAtIndex(10).setEnabled(false);
            toolBar.getComponentAtIndex(11).setEnabled(false);
            resetThresholdChannels();

            if (tabbedPane.getSelectedComponent() == panelA) {

                if (getHistoLUTComponentA() != null) {

                    // turn off threshold fields:
                    threshLowerF.setEnabled(false);
                    threshUpperF.setEnabled(false);
                    threshFillF.setEnabled(false);
                    getHistoLUTComponentA().noThreshold();
                }
            } else {

                if (getHistoLUTComponentB() != null) {

                    // turn off threshold fields
                    threshLowerBF.setEnabled(false);
                    threshUpperBF.setEnabled(false);
                    threshFillBF.setEnabled(false);

                    getHistoLUTComponentB().noThreshold();
                }
            }

        } else if (command.equals("resetLinearLUT")) {
            clearVoxelLabel();
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
            clearVoxelLabel();
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
            toolBar.getComponentAtIndex(10).setEnabled(true);
            toolBar.getComponentAtIndex(11).setEnabled(false);
            toolBar.getComponentAtIndex(6).setEnabled(false);

            if (tabbedPane.getSelectedComponent() == panelA) {

                if (getHistoLUTComponentA() != null) {

                    // turn on threshold fields
                    threshLowerF.setEnabled(true);
                    threshUpperF.setEnabled(true);
                    threshFillF.setEnabled(true);

                    if (threshFillF.getText() == null) {
                        threshFillF.setText(Double.toString(imageA.getMin()));
                    }

                    getHistoLUTComponentA();
					getHistoLUTComponentA().dualThresholdMode(ViewJComponentHistoRGB.DUAL_THRESHOLD);
                }
            } else {

                if (getHistoLUTComponentB() != null) {

                    // turn on threshold fields
                    threshLowerBF.setEnabled(true);
                    threshUpperBF.setEnabled(true);
                    threshFillBF.setEnabled(true);

                    if (threshFillBF.getText() == null) {
                        threshFillBF.setText(Double.toString(imageB.getMin()));
                    }

                    getHistoLUTComponentB();
					getHistoLUTComponentB().dualThresholdMode(ViewJComponentHistoRGB.DUAL_THRESHOLD);
                }
            }
        } else if (command.equals("inverseThresholdLUT")) {
            toolBar.getComponentAtIndex(10).setEnabled(false);
            toolBar.getComponentAtIndex(11).setEnabled(true);

            if (tabbedPane.getSelectedComponent() == panelA) {

                if (getHistoLUTComponentA() != null) {

                    // turn on threshold fields
                    threshLowerF.setEnabled(true);
                    threshUpperF.setEnabled(true);
                    threshFillF.setEnabled(true);
                    getHistoLUTComponentA();
					getHistoLUTComponentA().dualThresholdMode(ViewJComponentHistoRGB.DUAL_THRESHOLD_INV);
                }
            } else {

                if (getHistoLUTComponentB() != null) {

                    // turn on threshold fields
                    threshLowerBF.setEnabled(true);
                    threshUpperBF.setEnabled(true);
                    threshFillBF.setEnabled(true);
                    getHistoLUTComponentB();
					getHistoLUTComponentB().dualThresholdMode(ViewJComponentHistoRGB.DUAL_THRESHOLD_INV);
                }
            }
        } else if (command.equals("runThreshold") || command.equals("runInverseThreshold")) {

            // set up the threshold fields
            float[] r = null;
            float[] g = null;
            float[] b = null;

            if (tabbedPane.getSelectedComponent() == panelA) {

                if (getHistoLUTComponentA() != null) {

                    if (RGBTA.getRedFunction().size() == 6) {
                        r = new float[2];
                        r[0] = ((Vector2f) (RGBTA.getRedFunction().getPoint(1))).X;
                        r[1] = ((Vector2f) (RGBTA.getRedFunction().getPoint(3))).X;
                    }

                    if (RGBTA.getGreenFunction().size() == 6) {
                        g = new float[2];
                        g[0] = ((Vector2f) (RGBTA.getGreenFunction().getPoint(1))).X;
                        g[1] = ((Vector2f) (RGBTA.getGreenFunction().getPoint(3))).X;
                    }

                    if (RGBTA.getBlueFunction().size() == 6) {
                        b = new float[2];
                        b[0] = ((Vector2f) (RGBTA.getBlueFunction().getPoint(1))).X;
                        b[1] = ((Vector2f) (RGBTA.getBlueFunction().getPoint(3))).X;
                    }

                    JDialogThresholdRGB dialog = new JDialogThresholdRGB();

                    dialog.runFromLUTFrame(imageA, r, g, b, fillValuesA, (command.equals("runThresholdInverse")));

                }
            } else {

                if (getHistoLUTComponentB() != null) {

                    if (RGBTB.getRedFunction().size() == 6) {
                        r = new float[2];
                        r[0] = ((Vector2f) (RGBTB.getRedFunction().getPoint(1))).X;
                        r[1] = ((Vector2f) (RGBTB.getRedFunction().getPoint(3))).X;
                    }

                    if (RGBTB.getGreenFunction().size() == 6) {
                        g = new float[2];
                        g[0] = ((Vector2f) (RGBTB.getRedFunction().getPoint(1))).X;
                        g[1] = ((Vector2f) (RGBTB.getRedFunction().getPoint(3))).X;
                    }

                    if (RGBTB.getBlueFunction().size() == 6) {
                        b = new float[2];
                        b[0] = ((Vector2f) (RGBTB.getRedFunction().getPoint(1))).X;
                        b[1] = ((Vector2f) (RGBTB.getRedFunction().getPoint(3))).X;
                    }
                }
            }

        } else if (command.equals("CloseLUT")) {
            setVisible(false);
            imageA.removeImageDisplayListener((ViewImageUpdateInterface) this);

            if (imageB != null) {
                imageB.removeImageDisplayListener((ViewImageUpdateInterface) this);
            }

            dispose();
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
        	loadLUTandTransferFunctionFrom(false, null, null, false);

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
        } else if (event.getActionCommand().equals("Close")) {
            setVisible(false);
            imageA.removeImageDisplayListener(this);

            if (imageB != null) {
                imageB.removeImageDisplayListener(this);
            }

            dispose();
        } else if (event.getActionCommand().equals("UpdateA")) {
            updateHistoRGB(imageA, null, false);
            menuObj.setMenuItemEnabled("Reset histogram & LUT A", false);
        } else if (event.getActionCommand().equals("UpdateB")) {
            updateHistoRGB(null, imageB, false);
            menuObj.setMenuItemEnabled("Reset histogram & LUT B", false);
        } else if (command.equals("calcThresholdVolume")) {
            calcThresholdVolume = menuObj.isMenuItemSelected("Calculate threshold volume");

            if (!calcThresholdVolume) {
                clearVoxelLabel();
            }
        }
    }

    /**
     * Disposes of components and frame.
     */
    public void dispose() {
        histoPanelA.finalize();
        histogramARed.disposeLocal();
        histogramAGreen.disposeLocal();
        histogramABlue.disposeLocal();

        if (histogramBRed != null) {
            histoPanelB.finalize();
            histogramBRed.disposeLocal();
            histogramBGreen.disposeLocal();
            histogramBBlue.disposeLocal();
        }

        super.dispose();
    }

    /**
     * Placeholder.
     *
     * @param  mouseEvent  drag event
     */
    public void dragPoint(MouseEvent mouseEvent) { }

    /**
     * Gets the control widgets for the frame.
     *
     * @return  DOCUMENT ME!
     */
    public ViewControlsImage getControls() {
        return null;
    }

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
     * Returns whether the imageA LUT panel is the one being worked on.
     *
     * @return  whether the imageA LUT panel is the one being worked on
     */
    public boolean isImageASelected() {
        return tabbedPane.getSelectedComponent() == panelA;
    }

    /**
     * Returns whether the imageB LUT panel is the one being worked on.
     *
     * @return  whether the imageB LUT panel is the one being worked on
     */
    public boolean isImageBSelected() {
        return tabbedPane.getSelectedComponent() == panelB;
    }

    /**
     * {@inheritDoc}
     */
    public boolean isImageUpdate() {
        return updateCheckBoxA.isSelected();
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
        } else if (source == updateCheckBoxA) {

            if (updateCheckBoxB != null) {
                updateCheckBoxB.removeItemListener(this);
            }

            if ((updateCheckBoxA.isSelected() == true) && (updateCheckBoxB != null)) {
                updateCheckBoxB.setSelected(true);
            } else if ((updateCheckBoxA.isSelected() == false) && (updateCheckBoxB != null)) {
                updateCheckBoxB.setSelected(false);
            }

            if (updateCheckBoxB != null) {
                updateCheckBoxB.addItemListener(this);
            }
        } else if (source == updateCheckBoxB) {
            updateCheckBoxB.removeItemListener(this);

            if (updateCheckBoxB.isSelected() == true) {
                updateCheckBoxA.setSelected(true);
            } else {
                updateCheckBoxA.setSelected(false);
            }

            updateCheckBoxA.addItemListener(this);
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
                // regComponent.setRaOn(RGBTA.getROn());
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
                // regComponent.setGaOn(RGBTA.getGOn());
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
        } else if (source == lutAdjustCheckboxB) {
        	if(lutAdjustCheckboxB.isSelected()) {
        		imageA.getParentFrame().getComponentImage().setZeroToOneLUTAdj(true);
        	}
        	else {
        		imageA.getParentFrame().getComponentImage().setZeroToOneLUTAdj(false);
        	}

            updateFrames(false);
        }
    }

    /**
     * Unchanged.
     *
     * @param  e  DOCUMENT ME!
     */
    public void keyPressed(KeyEvent e) { }

    /**
     * Unchanged.
     *
     * @param  e  DOCUMENT ME!
     */
    public void keyReleased(KeyEvent e) { }

    /**
     * If the ENTER key is hit while in threshold boxes, update the LUT's threshold (for dual threshold).
     *
     * @param  e  KeyEvent
     */
    public void keyTyped(KeyEvent e) {

        if (e.getKeyChar() == KeyEvent.VK_ENTER) {

            if (e.getSource().equals(threshLowerF)) {

                if (this.testParameter(threshLowerF.getText(), imageA.getMin(),
                                           getHistoLUTComponentA().getUpperThreshold())) {

                    getHistoLUTComponentA().updateDualThreshold(new Float(threshLowerF.getText()).floatValue(),
                                                                new Float(threshUpperF.getText()).floatValue());
                } else {
                    threshLowerF.requestFocus();
                    threshLowerF.selectAll();
                }
            } else if (e.getSource().equals(threshUpperF)) {

                if (this.testParameter(threshUpperF.getText(), getHistoLUTComponentA().getLowerThreshold(),
                                           imageA.getMax())) {
                    getHistoLUTComponentA().updateDualThreshold(new Float(threshLowerF.getText()).floatValue(),
                                                                new Float(threshUpperF.getText()).floatValue());
                } else {
                    threshUpperF.requestFocus();
                    threshUpperF.selectAll();
                }
            } else if (e.getSource().equals(threshFillF)) {

                if (this.testParameter(threshFillF.getText(), imageA.getMin(), imageA.getMax())) {
                    int mode = getHistoLUTComponentA().getMode();
                    float value = new Float(threshFillF.getText()).floatValue();

                    getHistoLUTComponentA();
					if (mode == ViewJComponentHistoRGB.RED) {
                        fillValuesA[0] = value;
                    } else {
						getHistoLUTComponentA();
						if (mode == ViewJComponentHistoRGB.GREEN) {
						    fillValuesA[1] = value;
						} else {
							getHistoLUTComponentA();
							if (mode == ViewJComponentHistoRGB.BLUE) {
							    fillValuesA[2] = value;
							} else {
								getHistoLUTComponentA();
								if (mode == ViewJComponentHistoRGB.ALL) {
								    fillValuesA[0] = value;
								    fillValuesA[1] = value;
								    fillValuesA[2] = value;
								}
							}
						}
					}
                } else {
                    threshFillF.requestFocus();
                    threshFillF.selectAll();
                }
            } else if (e.getSource().equals(threshLowerBF)) {

                if (this.testParameter(threshLowerBF.getText(), imageB.getMin(),
                                           getHistoLUTComponentB().getUpperThreshold())) {

                    getHistoLUTComponentB().updateDualThreshold(new Float(threshLowerBF.getText()).floatValue(),
                                                                new Float(threshUpperBF.getText()).floatValue());
                } else {
                    threshLowerBF.requestFocus();
                    threshLowerBF.selectAll();
                }
            } else if (e.getSource().equals(threshUpperBF)) {

                if (this.testParameter(threshUpperBF.getText(), getHistoLUTComponentB().getLowerThreshold(),
                                           imageB.getMax())) {
                    getHistoLUTComponentB().updateDualThreshold(new Float(threshLowerBF.getText()).floatValue(),
                                                                new Float(threshUpperBF.getText()).floatValue());
                } else {
                    threshUpperBF.requestFocus();
                    threshUpperBF.selectAll();
                }
            } else if (e.getSource().equals(threshFillBF)) {

                // System.err.println("MIN: " + imageA.getMin() + " MAX: " + imageA.getMax());
                if (this.testParameter(threshFillBF.getText(), imageB.getMin(), imageB.getMax())) {
                    int mode = getHistoLUTComponentB().getMode();
                    float value = new Float(threshFillBF.getText()).floatValue();

                    getHistoLUTComponentB();
					if (mode == ViewJComponentHistoRGB.RED) {
                        fillValuesB[0] = value;
                    } else {
						getHistoLUTComponentB();
						if (mode == ViewJComponentHistoRGB.GREEN) {
						    fillValuesB[1] = value;
						} else {
							getHistoLUTComponentB();
							if (mode == ViewJComponentHistoRGB.BLUE) {
							    fillValuesB[2] = value;
							} else {
								getHistoLUTComponentB();
								if (mode == ViewJComponentHistoRGB.ALL) {
								    fillValuesB[0] = value;
								    fillValuesB[1] = value;
								    fillValuesB[2] = value;
								}
							}
						}
					}

                } else {
                    threshFillBF.requestFocus();
                    threshFillBF.selectAll();
                }
            }

        }
    }

    /**
     * Unchanged.
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseClicked(MouseEvent mouseEvent) { }

    // ************************************************************************
    // ************************** Mouse Motion Events *************************
    // ************************************************************************

    /**
     * Unchanged.
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseDragged(MouseEvent mouseEvent) { }

    /**
     * Unchanged.
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseEntered(MouseEvent mouseEvent) { }

    /**
     * Unchanged.
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseExited(MouseEvent mouseEvent) { }

    /**
     * Unchanged.
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseMoved(MouseEvent mouseEvent) { }

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */

    public void mousePressed(MouseEvent event) { }

    /**
     * Unchanged.
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseReleased(MouseEvent mouseEvent) { }

    /**
     * Enable button to indicate image has changed and the histogram should be recalculated.
     *
     * @param  LUT        new Lookup table
     * @param  imageAorB  indicates which histogram needs to be recalculated
     */
    public void notifyOfUpdate(ModelLUT LUT, int imageAorB) {

        if (imageAorB == ModelImage.IMAGE_A) {
            menuObj.setMenuItemEnabled("Reset histogram & LUT A", true);
        } else {
            menuObj.setMenuItemEnabled("Reset histogram & LUT B", true);
        }
    }

    /**
     * Removes the menu and controls of the main frame so that a new frame can load the main frame with the proper
     * controls.Abstract and must be extended.
     */
    public void removeControls() { }

    /**
     * Removes the tabbed pane for the histogram of image B.
     */
    public void removeHistoRGBb() {

        if (tabbedPane.getTabCount() == 2) {
            tabbedPane.removeTabAt(1);
            imageB = null;
            panelB = null;
            tabbedPane.validate();
            validate();
        }
    }

    /**
     * Sets the active image for drawing VOIs and appling algorithms.
     *
     * @param  active  IMAGE_A or IMAGE_B
     */

    /**
     * Sets the active image for drawing VOIs. VOIs are only drawn in the active image. In addition, algorithms are
     * executed on the active window.
     *
     * @param  active  IMAGE_A or IMAGE_B
     */
    public void setActiveImage(int active) {

        if (active == IMAGE_A) {
            displayMode = IMAGE_A;
            setTitle();
        } else {
            displayMode = IMAGE_B;
            setTitle();
        }

        updateImages(false);
    }

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
     * Sets the menu and controls (i.e. toolbars) of the main frame! This puts only the menus and controls needed to
     * controls the operations of this frame. Different image frames have different menu and controls.
     */
    public void setControls() { }

    /**
     * Controls whether or not the images/VOIs of the frame can be modified.
     *
     * @param  flag  if true the image/VOIs can be modified; if false image/VOIs can NOT be modified
     */
    public void setEnabled(boolean flag) { }

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
     * DOCUMENT ME!
     *
     * @param  flag  DOCUMENT ME!
     */
    public void setPaintBitmapSwitch(boolean flag) { }

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
     * @param  RGBT  DOCUMENT ME!
     */
    public void setRGBTA(ModelRGB RGBT) {
        RGBTA = RGBT;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  RGBT  DOCUMENT ME!
     */
    public void setRGBTB(ModelRGB RGBT) {
        RGBTB = RGBT;
    }

    /**
     * Does nothing in this class.
     *
     * @param  slice  DOCUMENT ME!
     */
    public void setSlice(int slice) { }

    // ===============Abstract Methods from ViewJFrameBase===========================

    /**
     * Does nothing in this class.
     *
     * @param  tSlice  DOCUMENT ME!
     */
    public void setTimeSlice(int tSlice) { }

    /**
     * setTitle.
     */
    public void setTitle() { }

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
            setTitle("Lookup Table: " + imageA.getImageName());
            modeA = getHistoLUTComponentA().getMode();

            getHistoLUTComponentA();
			if (modeA == ViewJComponentHistoRGB.RED) {
                redRGBButton.setBorderPainted(true);
                redRGBButton.setSelected(true);
                greenRGBButton.setBorderPainted(false);
                blueRGBButton.setBorderPainted(false);
                allRGBButton.setBorderPainted(false);
            } else {
				getHistoLUTComponentA();
				if (modeA == ViewJComponentHistoRGB.GREEN) {
				    redRGBButton.setBorderPainted(false);
				    greenRGBButton.setBorderPainted(true);
				    greenRGBButton.setSelected(true);
				    blueRGBButton.setBorderPainted(false);
				    allRGBButton.setBorderPainted(false);
				} else {
					getHistoLUTComponentA();
					if (modeA == ViewJComponentHistoRGB.BLUE) {
					    redRGBButton.setBorderPainted(false);
					    greenRGBButton.setBorderPainted(false);
					    blueRGBButton.setBorderPainted(true);
					    blueRGBButton.setSelected(true);
					    allRGBButton.setBorderPainted(false);
					} else {
						getHistoLUTComponentA();
						if (modeA == ViewJComponentHistoRGB.ALL) {
						    redRGBButton.setBorderPainted(false);
						    greenRGBButton.setBorderPainted(false);
						    blueRGBButton.setBorderPainted(false);
						    allRGBButton.setSelected(true);
						    allRGBButton.setBorderPainted(true);
						}
					}
				}
			}
        } else if ((source == tabbedPane) && (tabbedPane.getSelectedComponent() == panelB) && (imageB != null)) {
            displayMode = IMAGE_B;
            setTitle("Lookup Table: " + imageB.getImageName());
            modeB = getHistoLUTComponentB().getMode();

            getHistoLUTComponentB();
			if (modeB == ViewJComponentHistoRGB.RED) {
                redRGBButton.setBorderPainted(true);
                redRGBButton.setSelected(true);
                greenRGBButton.setBorderPainted(false);
                blueRGBButton.setBorderPainted(false);
                allRGBButton.setBorderPainted(false);
            } else {
				getHistoLUTComponentB();
				if (modeB == ViewJComponentHistoRGB.GREEN) {
				    redRGBButton.setBorderPainted(false);
				    greenRGBButton.setBorderPainted(true);
				    greenRGBButton.setSelected(true);
				    blueRGBButton.setBorderPainted(false);
				    allRGBButton.setBorderPainted(false);
				} else {
					getHistoLUTComponentB();
					if (modeB == ViewJComponentHistoRGB.BLUE) {
					    redRGBButton.setBorderPainted(false);
					    greenRGBButton.setBorderPainted(false);
					    blueRGBButton.setBorderPainted(true);
					    blueRGBButton.setSelected(true);
					    allRGBButton.setBorderPainted(false);
					} else {
						getHistoLUTComponentB();
						if (modeB == ViewJComponentHistoRGB.ALL) {
						    redRGBButton.setBorderPainted(false);
						    greenRGBButton.setBorderPainted(false);
						    blueRGBButton.setBorderPainted(false);
						    allRGBButton.setSelected(true);
						    allRGBButton.setBorderPainted(true);
						}
					}
				}
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

            getHistoLUTComponentA();
			if (modeA == ViewJComponentHistoRGB.RED) {
                getHistoLUTComponentA().setHistogramInfo(imageA, histogramARed);
            } else {
				getHistoLUTComponentA();
				if (modeA == ViewJComponentHistoRGB.GREEN) {
				    getHistoLUTComponentA().setHistogramInfo(imageA, histogramAGreen);
				} else {
					getHistoLUTComponentA();
					if (modeA == ViewJComponentHistoRGB.BLUE) {
					    getHistoLUTComponentA().setHistogramInfo(imageA, histogramABlue);
					}
				}
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

            getHistoLUTComponentB();
			if (modeB == ViewJComponentHistoRGB.RED) {
                getHistoLUTComponentB().setHistogramInfo(imageB, histogramBRed);
            } else {
				getHistoLUTComponentB();
				if (modeB == ViewJComponentHistoRGB.GREEN) {
				    getHistoLUTComponentB().setHistogramInfo(imageB, histogramBGreen);
				} else {
					getHistoLUTComponentB();
					if (modeB == ViewJComponentHistoRGB.BLUE) {
					    getHistoLUTComponentB().setHistogramInfo(imageB, histogramBBlue);
					}
				}
			}

            getHistoLUTComponentB().showHistogram();
        }

        tabbedPane.validate();
        validate();
    }

    /**
     * This methods calls the componentImage's REPAINT method to redraw the screen. The extents on this image have
     * changed, so the extents need to be read in again and menus, panes and slide bars adjusted accordingly.
     *
     * @return  DOCUMENT ME!
     */
    public boolean updateImageExtents() {
        return false;
    }

    // **** Methods here only because ViewImageUpdateInterface is implemented. They do
    // nothing here.

    /**
     * This methods calls the componentImage's REPAINT method to redraw the screen. Without LUT changes or image changes
     *
     * @return  DOCUMENT ME!
     */
    public boolean updateImages() {
        return false;
    }

    /**
     * This methods calls the componentImage's update method to redraw the screen. Without LUT changes. Does nothing in
     * this class.
     *
     * @param   flag  forces show to re import image and calc. java image
     *
     * @return  boolean confirming successful update
     */
    public boolean updateImages(boolean flag) {
        return false;
    }

    /**
     * This methods calls the componentImage's update method to redraw the screen. Does nothing in this class.
     *
     * @param   LUTa        LUT used to update imageA
     * @param   LUTb        LUT used to update imageB
     * @param   flag        forces show to re import image and calc. java image
     * @param   interpMode  image interpolation method (Nearest or Smooth)
     *
     * @return  boolean confirming a successful update
     */
    public boolean updateImages(ModelLUT LUTa, ModelLUT LUTb, boolean flag, int interpMode) {
        return false;
    }

    /**
     * Placeholder.
     *
     * @param  str  string
     */
    public void updateLUTPositionString(String str) { }

    /**
     * {@inheritDoc}
     */
    public void updateThresholdFields(float lower, float upper) {

        if (isImageASelected()) {
            threshLowerF.setText(Float.toString(lower));
            threshUpperF.setText(Float.toString(upper));

            if (getHistoLUTComponentA().getMode() == ViewJComponentHistoRGB.ALL) {
                return;
            }
        } else {
            threshLowerBF.setText(Float.toString(lower));
            threshUpperBF.setText(Float.toString(upper));

            if (getHistoLUTComponentB().getMode() == ViewJComponentHistoRGB.ALL) {
                return;
            }
        }

        if (calcThresholdVolume) {

            float voxelAmount = 0f;
            voxelAmount = calcThresholdVolume(lower, upper);

            if (imageA.getNDims() > 2) {

                if (isImageASelected()) {
                    voxelVolumeLabel.setText("Threshold volume: " + String.valueOf(voxelAmount) +
                                             imageA.getFileInfo(0).getAreaUnitsOfMeasureStr());
                } else { // imageB selected
                    voxelVolumeLabelB.setText("Threshold volume: " + String.valueOf(voxelAmount) +
                                              imageB.getFileInfo(0).getAreaUnitsOfMeasureStr());
                }

            } else {

                if (isImageASelected()) {
                    voxelVolumeLabel.setText("Threshold area: " + String.valueOf(voxelAmount) +
                                             imageA.getFileInfo(0).getAreaUnitsOfMeasureStr());
                } else { // imageB selected
                    voxelVolumeLabelB.setText("Threshold area: " + String.valueOf(voxelAmount) +
                                              imageB.getFileInfo(0).getAreaUnitsOfMeasureStr());
                }

            }

        }
    }

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowActivated(WindowEvent event) { }

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowClosed(WindowEvent event) { }

    /**
     * Calls close.
     *
     * @param  event  event that triggered function
     */
    public void windowClosing(WindowEvent event) {
        setVisible(false);
        imageA.removeImageDisplayListener(this);

        if (imageB != null) {
            imageB.removeImageDisplayListener(this);
        }

        dispose();
    }

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowDeactivated(WindowEvent event) { }

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowDeiconified(WindowEvent event) { }

    /**
     * Unchanged.
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
    private void buildMenu() {
        JMenuBar menuBar;

        try {
            menuObj = new ViewMenuBuilder(this);
            menuBar = new JMenuBar();

            JSeparator separator = new JSeparator();

            menuBar.add(menuObj.makeMenu("File", 'F', false,
                                         new JComponent[] {
                                             menuObj.buildMenuItem("Open LUT  and transfer functions", "OpenLUT", 0, "open.gif", true),
                                             menuObj.buildMenuItem("Save LUT  and transfer functions", "SaveLUT", 0, "save.gif", true),
                                             menuObj.buildMenuItem("Open transfer functions", "OpenFuncts", 0,
                                                                   "open.gif", true),
                                             menuObj.buildMenuItem("Save transfer functions", "SaveFuncts", 0,
                                                                   "save.gif", true), separator,
                                             menuObj.buildMenuItem("Close LUT", "CloseLUT", 0, null, true)
                                         }));
            menuBar.add(menuObj.makeMenu("Utilities", 'U', false,
                                         new JComponent[] {
                                             menuObj.buildMenuItem("Reset transfer function", "Linear", 0, null,
                                                                   false),
                                             menuObj.buildMenuItem("Reset histogram & LUT A", "UpdateA", 0, null,
                                                                   false),
                                             menuObj.buildMenuItem("Reset histogram & LUT B", "UpdateB", 0, null,
                                                                   false),
                                             menuObj.buildCheckBoxMenuItem("Calculate threshold volume",
                                                                           "calcThresholdVolume", true),
                                         }));
            menuObj.setMenuItemEnabled("Reset histogram & LUT A", false);
            menuObj.setMenuItemEnabled("Reset histogram & LUT B", false);

        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameHistoLUT.buildMenu");

            return;
        }

        setJMenuBar(menuBar);
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

        JPanel controlPanel = new JPanel(new GridBagLayout());

        controlPanel.setBorder(new EtchedBorder());

        JPanel optionsPanel = new JPanel(new GridBagLayout());
        optionsPanel.setBorder(new TitledBorder(new EtchedBorder(), "Histogram options", TitledBorder.LEFT,
                                                TitledBorder.CENTER, MipavUtil.font12B, Color.black));

        GridBagConstraints gbc = new GridBagConstraints();

        updateCheckBoxA = new JCheckBox("Update image (real-time)", Preferences.is(Preferences.PREF_HISTOGRAM_DISPLAY));
        updateCheckBoxA.setFont(MipavUtil.font12);
        updateCheckBoxA.addItemListener(this);

        logCheckBoxA = new JCheckBox("Log scale (Histogram)", true);
        logCheckBoxA.setFont(MipavUtil.font12);
        logCheckBoxA.addItemListener(this);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        optionsPanel.add(updateCheckBoxA, gbc);

        gbc.gridy = 1;
        optionsPanel.add(logCheckBoxA, gbc);

        JPanel colorPanel = new JPanel(new GridBagLayout());
        colorPanel.setBorder(new TitledBorder(new EtchedBorder(), "Image components", TitledBorder.LEFT,
                                              TitledBorder.CENTER, MipavUtil.font12B, Color.black));

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

        gbc.gridx = 0;
        gbc.gridy = 0;
        colorPanel.add(redCheckBoxA, gbc);

        gbc.gridy = 1;
        colorPanel.add(greenCheckBoxA, gbc);

        gbc.gridy = 2;
        colorPanel.add(blueCheckBoxA, gbc);

        JPanel thresholdPanel = new JPanel(new GridBagLayout());

        thresholdPanel.setBorder(new TitledBorder(new EtchedBorder(), "Threshold options", TitledBorder.LEFT,
                                                  TitledBorder.CENTER, MipavUtil.font12B, Color.black));

        JLabel upperThreshLabel = new JLabel("Upper threshold:");

        upperThreshLabel.setFont(MipavUtil.font12);
        upperThreshLabel.setForeground(Color.black);

        JLabel lowerThreshLabel = new JLabel("Lower threshold:");

        lowerThreshLabel.setFont(MipavUtil.font12);
        lowerThreshLabel.setForeground(Color.black);

        JLabel fillLabel = new JLabel("Fill value:");

        fillLabel.setFont(MipavUtil.font12);
        fillLabel.setForeground(Color.black);

        voxelVolumeLabel = new JLabel("Threshold volume:                       ");

        voxelVolumeLabel.setFont(MipavUtil.font10);
        voxelVolumeLabel.setForeground(Color.black);

        threshLowerF = new JTextField(8);
        MipavUtil.makeNumericsOnly(threshLowerF, true);
        threshLowerF.setFont(MipavUtil.font12);
        threshLowerF.setEnabled(false);
        threshLowerF.addKeyListener(this);

        threshUpperF = new JTextField(8);
        MipavUtil.makeNumericsOnly(threshUpperF, true);
        threshUpperF.setFont(MipavUtil.font12);
        threshUpperF.setEnabled(false);
        threshUpperF.addKeyListener(this);

        threshFillF = new JTextField(8);
        MipavUtil.makeNumericsOnly(threshFillF, true);
        threshFillF.setFont(MipavUtil.font12);
        threshFillF.setEnabled(false);
        threshFillF.addKeyListener(this);

        Insets leftInsets = new Insets(5, 5, -2, 5);
        Insets rightInsets = new Insets(5, -2, 5, 5);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbc.insets = leftInsets;
        thresholdPanel.add(upperThreshLabel, gbc);

        gbc.gridx = 1;
        gbc.insets = rightInsets;
        thresholdPanel.add(threshUpperF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.insets = leftInsets;
        thresholdPanel.add(lowerThreshLabel, gbc);

        gbc.gridx = 1;
        gbc.insets = rightInsets;
        thresholdPanel.add(threshLowerF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.insets = leftInsets;
        thresholdPanel.add(fillLabel, gbc);

        gbc.gridx = 1;
        gbc.insets = rightInsets;
        thresholdPanel.add(threshFillF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = 2;
        gbc.weighty = 1;
        gbc.insets = leftInsets;
        thresholdPanel.add(voxelVolumeLabel, gbc);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        gbc.weighty = 0;
        controlPanel.add(optionsPanel, gbc);

        gbc.gridy = 1;
        controlPanel.add(colorPanel, gbc);

        gbc.gridy = 2;
        gbc.weighty = 1;
        gbc.fill = GridBagConstraints.BOTH;
        controlPanel.add(thresholdPanel, gbc);

        panelA = new JPanel(new BorderLayout());
        panelA.add(controlPanel, BorderLayout.WEST);
        panelA.add(histoPanelA);
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

        JPanel optionsPanel = new JPanel(new GridBagLayout());
        optionsPanel.setBorder(new TitledBorder(new EtchedBorder(), "Histogram options", TitledBorder.LEFT,
                                                TitledBorder.CENTER, MipavUtil.font12B, Color.black));

        JPanel colorPanel = new JPanel(new GridBagLayout());
        colorPanel.setBorder(new TitledBorder(new EtchedBorder(), "Image components", TitledBorder.LEFT,
                                              TitledBorder.CENTER, MipavUtil.font12B, Color.black));

        updateCheckBoxB = new JCheckBox("Update image (real-time)", Preferences.is(Preferences.PREF_HISTOGRAM_DISPLAY));
        updateCheckBoxB.setFont(MipavUtil.font12);
        updateCheckBoxB.addItemListener(this);

        logCheckBoxB = new JCheckBox("Log scale (Histogram)", true);
        logCheckBoxB.setFont(MipavUtil.font12);
        logCheckBoxB.addItemListener(this);
        
        lutAdjustCheckboxB = new JCheckBox("0 to 1 LUT adjustment");
        lutAdjustCheckboxB.setFont(MipavUtil.font12);
        if (image.getParentFrame() != null) {
            lutAdjustCheckboxB.setSelected(image.getParentFrame().getComponentImage().isZeroToOneLUTAdj());
        }
        else if (regComponent != null) {
            lutAdjustCheckboxB.setSelected(regComponent.isZeroToOneLUTAdj());        
        }
        lutAdjustCheckboxB.addItemListener(this);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        optionsPanel.add(updateCheckBoxB, gbc);

        gbc.gridy = 1;
        optionsPanel.add(logCheckBoxB, gbc);
        
        gbc.gridy = 2;
        optionsPanel.add(lutAdjustCheckboxB, gbc);

        gbc = new GridBagConstraints();

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

        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        colorPanel.add(redCheckBoxB, gbc);

        gbc.gridy = 1;
        colorPanel.add(greenCheckBoxB, gbc);

        gbc.gridy = 2;
        colorPanel.add(blueCheckBoxB, gbc);

        gbc = new GridBagConstraints();

        /** --------- Here is the code for adding the threshold options panel --------- **/
        JPanel thresholdPanelB = new JPanel(new GridBagLayout());

        thresholdPanelB.setBorder(new TitledBorder(new EtchedBorder(), "Threshold options", TitledBorder.LEFT,
                                                   TitledBorder.CENTER, MipavUtil.font12B, Color.black));

        JLabel upperThreshLabelB = new JLabel("Upper threshold:");

        upperThreshLabelB.setFont(MipavUtil.font12);
        upperThreshLabelB.setForeground(Color.black);

        JLabel lowerThreshLabelB = new JLabel("Lower threshold:");

        lowerThreshLabelB.setFont(MipavUtil.font12);
        lowerThreshLabelB.setForeground(Color.black);

        JLabel fillLabelB = new JLabel("Fill value:");

        fillLabelB.setFont(MipavUtil.font12);
        fillLabelB.setForeground(Color.black);

        voxelVolumeLabelB = new JLabel("Threshold volume:");

        voxelVolumeLabelB.setFont(MipavUtil.font10);
        voxelVolumeLabelB.setForeground(Color.black);

        threshLowerBF = new JTextField(8);
        MipavUtil.makeNumericsOnly(threshLowerBF, true);
        threshLowerBF.setFont(MipavUtil.font12);
        threshLowerBF.setEnabled(false);
        threshLowerBF.addKeyListener(this);

        threshUpperBF = new JTextField(8);
        MipavUtil.makeNumericsOnly(threshUpperBF, true);
        threshUpperBF.setFont(MipavUtil.font12);
        threshUpperBF.setEnabled(false);
        threshUpperBF.addKeyListener(this);

        threshFillBF = new JTextField(8);
        MipavUtil.makeNumericsOnly(threshFillBF, true);
        threshFillBF.setFont(MipavUtil.font12);
        threshFillBF.setEnabled(false);
        threshFillBF.addKeyListener(this);

        Insets leftInsets = new Insets(5, 5, -2, 5);
        Insets rightInsets = new Insets(5, -2, 5, 5);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.NORTHWEST;
        gbc.insets = leftInsets;
        thresholdPanelB.add(upperThreshLabelB, gbc);

        gbc.gridx = 1;
        gbc.insets = rightInsets;
        thresholdPanelB.add(threshUpperBF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.insets = leftInsets;
        thresholdPanelB.add(lowerThreshLabelB, gbc);

        gbc.gridx = 1;
        gbc.insets = rightInsets;
        thresholdPanelB.add(threshLowerBF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.insets = leftInsets;
        thresholdPanelB.add(fillLabelB, gbc);

        gbc.gridx = 1;
        gbc.insets = rightInsets;
        thresholdPanelB.add(threshFillBF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = 2;
        gbc.weighty = 1;
        gbc.insets = leftInsets;
        thresholdPanelB.add(voxelVolumeLabelB, gbc);

        /** --------- End code for adding the threshold options panel --------- **/


        gbc = new GridBagConstraints();

        JPanel controlPanelB = new JPanel(new GridBagLayout());
        controlPanelB.setBorder(new EtchedBorder());

        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.NORTH;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        controlPanelB.add(optionsPanel, gbc);

        gbc.gridy = 1;
        controlPanelB.add(colorPanel, gbc);

        gbc.gridy = 2;
        gbc.weighty = 1;
        gbc.fill = GridBagConstraints.BOTH;
        controlPanelB.add(thresholdPanelB, gbc);

        panelB = new JPanel(new BorderLayout());
        panelB.add(controlPanelB, BorderLayout.WEST);
        panelB.add(histoPanelB);
        tabbedPane.addTab("ImageB", null, panelB);
        tabbedPane.setFont(MipavUtil.font12B);
    }

    /**
     * end HistoLUTParent interface.
     *
     * @param  imageAorB     DOCUMENT ME!
     * @param  entireFlag    DOCUMENT ME!
     * @param  progressFlag  DOCUMENT ME!
     */

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

                if (dimExtentsA[0] < 256) {
                    dimExtentsA[0] = 256;
                }
            }

            histogramARed = new ModelHistogram(ModelStorageBase.INTEGER, dimExtentsA);
            offset = 1;

            AlgorithmHistogram histoAlgoARed = new AlgorithmHistogram(histogramARed, offset, imageA, entireFlag);

           // histoAlgoARed.setProgressBarVisible(progressFlag);

            // histoAlgoARed.setSeparateThread(false);
            histoAlgoARed.run();

            if (imageA.getType() != ModelStorageBase.ARGB_USHORT) {
                dimExtentsA[0] = 256;
            } else {
                dimExtentsA[0] = (int) (imageA.getMaxG() - imageA.getMinG() + 0.5) + 1;

                if (dimExtentsA[0] < 256) {
                    dimExtentsA[0] = 256;
                }
            }

            histogramAGreen = new ModelHistogram(ModelStorageBase.INTEGER, dimExtentsA);
            offset = 2;

            AlgorithmHistogram histoAlgoAGreen = new AlgorithmHistogram(histogramAGreen, offset, imageA, entireFlag);

           // histoAlgoAGreen.setProgressBarVisible(progressFlag);

            // histoAlgoAGreen.setSeparateThread(false);
            histoAlgoAGreen.run();

            if (imageA.getType() != ModelStorageBase.ARGB_USHORT) {
                dimExtentsA[0] = 256;
            } else {
                dimExtentsA[0] = (int) (imageA.getMaxB() - imageA.getMinB() + 0.5) + 1;

                if (dimExtentsA[0] < 256) {
                    dimExtentsA[0] = 256;
                }
            }

            histogramABlue = new ModelHistogram(ModelStorageBase.INTEGER, dimExtentsA);
            offset = 3;

            AlgorithmHistogram histoAlgoABlue = new AlgorithmHistogram(histogramABlue, offset, imageA, entireFlag);

           // histoAlgoABlue.setProgressBarVisible(progressFlag);

            // histoAlgoABlue.setSeparateThread(false);
            histoAlgoABlue.run();
        }

        if ((imageB != null) && (imageAorB == IMAGE_B)) {

            if (imageB.getType() != ModelStorageBase.ARGB_USHORT) {
                dimExtentsB[0] = 256;
            } else {
                dimExtentsB[0] = (int) (imageB.getMaxR() - imageB.getMinR() + 0.5) + 1;

                if (dimExtentsB[0] < 256) {
                    dimExtentsB[0] = 256;
                }
            }

            histogramBRed = new ModelHistogram(ModelStorageBase.INTEGER, dimExtentsB);
            offset = 1;

            AlgorithmHistogram histoAlgoBRed = new AlgorithmHistogram(histogramBRed, offset, imageB, entireFlag);

         //   histoAlgoBRed.setProgressBarVisible(progressFlag);

            // histoAlgoBRed.setSeparateThread(false);
            histoAlgoBRed.run();

            if (imageB.getType() != ModelStorageBase.ARGB_USHORT) {
                dimExtentsB[0] = 256;
            } else {
                dimExtentsB[0] = (int) (imageB.getMaxG() - imageB.getMinG() + 0.5) + 1;

                if (dimExtentsB[0] < 256) {
                    dimExtentsB[0] = 256;
                }
            }

            histogramBGreen = new ModelHistogram(ModelStorageBase.INTEGER, dimExtentsB);
            offset = 2;

            AlgorithmHistogram histoAlgoBGreen = new AlgorithmHistogram(histogramBGreen, offset, imageB, entireFlag);

            //histoAlgoBGreen.setProgressBarVisible(progressFlag);

            // histoAlgoBGreen.setSeparateThread(false);
            histoAlgoBGreen.run();

            if (imageB.getType() != ModelStorageBase.ARGB_USHORT) {
                dimExtentsB[0] = 256;
            } else {
                dimExtentsB[0] = (int) (imageB.getMaxB() - imageB.getMinB() + 0.5) + 1;

                if (dimExtentsB[0] < 256) {
                    dimExtentsB[0] = 256;
                }
            }

            histogramBBlue = new ModelHistogram(ModelStorageBase.INTEGER, dimExtentsB);
            offset = 3;

            AlgorithmHistogram histoAlgoBBlue = new AlgorithmHistogram(histogramBBlue, offset, imageB, entireFlag);

         //   histoAlgoBBlue.setProgressBarVisible(progressFlag);

            // histoAlgoBBlue.setSeparateThread(false);
            histoAlgoBBlue.run();
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param   lower  DOCUMENT ME!
     * @param   upper  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private float calcThresholdVolume(float lower, float upper) {
        ModelImage image;
        int colorIndex = 0;

        boolean isInverse = false;

        if (isImageASelected()) {
            image = imageA;
            colorIndex = getHistoLUTComponentA().getMode() - 6;

            if (getHistoLUTComponentA().getThresholdMode() == ViewJComponentHistoRGB.DUAL_THRESHOLD_INV) {
                isInverse = true;
            }

        } else {
            image = imageB;
            colorIndex = getHistoLUTComponentB().getMode() - 6;

            if (getHistoLUTComponentB().getThresholdMode() == ViewJComponentHistoRGB.DUAL_THRESHOLD_INV) {
                isInverse = true;
            }

        }

        int[] imageBuffer = new int[image.getExtents()[0] * image.getExtents()[1] * 4];
        int numVoxels = 0;

        int z = 1;

        if (image.getNDims() > 2) {
            z = image.getExtents()[2];
        }

        for (int i = 0; i < z; i++) {

            try {
                image.exportData(i * image.getExtents()[0] * image.getExtents()[1], imageBuffer.length, imageBuffer);

                for (int j = 0; j < imageBuffer.length; j += 4) {

                    if (!isInverse) {

                        if ((imageBuffer[j + colorIndex] >= lower) && (imageBuffer[j + colorIndex] <= upper)) {
                            numVoxels++;
                        }
                    } else {

                        if ((imageBuffer[j + colorIndex] <= lower) || (imageBuffer[j + colorIndex] >= upper)) {
                            numVoxels++;
                        }

                    }
                }
            } catch (IOException ioe) {
                return 0.0f;
            }
        }

        if (image.getNDims() > 2) {
            return (numVoxels * Math.abs(image.getFileInfo(0).getResolutions()[0]) *
                        Math.abs(image.getFileInfo(0).getResolutions()[1]) *
                        Math.abs(image.getFileInfo(0).getResolutions()[2]));
        } else {
            return (numVoxels * Math.abs(image.getFileInfo(0).getResolutions()[0]) *
                        Math.abs(image.getFileInfo(0).getResolutions()[1]));

        }
    }

    /**
     * Clears (blanks out) the threshold voxel information (when RGB mode or not thresholding).
     */
    private void clearVoxelLabel() {

        if (isImageASelected()) {

            if (imageA.getNDims() > 2) { }
            else {
                voxelVolumeLabel.setText("Threshold area:");
            }
        } else { // imageB selected

            if (imageB.getNDims() > 2) {
                voxelVolumeLabel.setText("Threshold volume:");
            } else {
                voxelVolumeLabelB.setText("Threshold volume:");
            }
        }

    }

    /**
     * resets boolean for threshold channels.
     */
    private void resetThresholdChannels() {

        if (tabbedPane.getSelectedComponent() == panelA) {
            useThresholdChannelA[0] = false;
            useThresholdChannelA[1] = false;
            useThresholdChannelA[2] = false;
        } else {
            useThresholdChannelB[0] = false;
            useThresholdChannelB[1] = false;
            useThresholdChannelB[2] = false;
        }
    }
}
