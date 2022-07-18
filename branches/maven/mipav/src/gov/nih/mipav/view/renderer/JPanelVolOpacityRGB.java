package gov.nih.mipav.view.renderer;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.J3D.*;
import gov.nih.mipav.view.renderer.WildMagic.*;

import java.awt.*;
import java.awt.event.*;
import java.io.IOException;

import javax.swing.*;
import javax.swing.border.*;


/**
 * This class produces a frame where the histogram of the image data is displayed using the color mapping. All frames
 * using the color map are dynamically updated with the new color map. This is the color image volume opacity control
 * panel. In addition to the JPanelColorHisoRGB, the panel hold the gradient magnitude hisogram control panel.
 *
 * @version  1.0
 * @deprecated
 * @see JPanelVolumeOpacity
 */
public class JPanelVolOpacityRGB extends JPanelVolOpacityBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 8837612116721229202L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

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

    /** imageA histogram of the Blue channel. */
    private ModelHistogram histogramGM_ABlue = null;

    /** imageA histogram of the Green channel. */
    private ModelHistogram histogramGM_AGreen = null;

    /** imageA histogram of the Red channel. */
    private ModelHistogram histogramGM_ARed = null;

    /** imageB histogram, the Blue channel. */
    private ModelHistogram histogramGM_BBlue = null;

    /** imageB histogram, the Green channel. */
    private ModelHistogram histogramGM_BGreen = null;

    /** imageB histogram of the Red channel. */
    private ModelHistogram histogramGM_BRed = null;

    /** A reference to the volume renderer frame's progress bar. */
    private JProgressBar rendererProgressBar;

    /** Tagged for deletion - Matt, Ruida, Lee, ModelRGB table for imageA, B. */
    private ModelRGB RGBTA;

    /** Tagged for deletion - Matt, Ruida, Lee, RGB table reference of the image A, B. */
    private ModelRGB RGBTA_GM;

    /** DOCUMENT ME! */
    private ModelRGB RGBTB;

    /** DOCUMENT ME! */
    private ModelRGB RGBTB_GM;

    /** ToolBar that hold the linear, horizontal mode etc. */
    private JToolBar toolBar;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Makes a frame of the histogram.
     *
     * @param  theParentFrame  Model RGB
     * @param  _imageA         Model of imageA
     * @param  _imageB         Model of imageB
     * @deprecated
     * @see JPanelVolumeOpacity
     */
    public JPanelVolOpacityRGB(RenderViewBase theParentFrame, ModelImage _imageA, ModelImage _imageB) {
        super(theParentFrame);

        rendererProgressBar = ViewJFrameVolumeView.getRendererProgressBar();

        int[] RGBExtents = new int[2];
        RGBExtents[0] = 4;
        RGBExtents[1] = 256;

        imageA = _imageA;
        imageB = _imageB;
        RGBTA = new ModelRGB(RGBExtents);
        RGBTB = new ModelRGB(RGBExtents);
        RGBTA_GM = new ModelRGB(RGBExtents);
        RGBTB_GM = new ModelRGB(RGBExtents);

        tabbedPane = new JTabbedPane();
        tabbedPane.setFont(MipavUtil.font12B);

        toolBar = buildRGBToolBar(this);

        buildPanelA();

        if (imageB != null) {
            buildPanelB();
        }

        JPanel blendPanel = buildBlendPanel();
        
        GridBagLayout gbLayout = new GridBagLayout();
        GridBagConstraints gbConstraints = new GridBagConstraints();
        JPanel subPanel = new JPanel(gbLayout);
        subPanel.setLayout(gbLayout);

        gbConstraints.weightx = 1;
        gbConstraints.fill = GridBagConstraints.HORIZONTAL;
        gbConstraints.anchor = GridBagConstraints.NORTH;
        subPanel.add(toolBar, gbConstraints);

        gbConstraints.gridy = 1;
        gbConstraints.fill = GridBagConstraints.BOTH;
        subPanel.add(tabbedPane, gbConstraints);

        gbConstraints.gridy = 2;
        gbConstraints.weighty = 1;
        gbConstraints.fill = GridBagConstraints.HORIZONTAL;
        subPanel.add(blendPanel, gbConstraints);
        
        JScrollPane scrollPaneA = new JScrollPane(subPanel);
        scrollPaneA.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);

        mainPanel = new JPanel(new GridLayout(1, 1));
        mainPanel.add(scrollPaneA);
    }

    /**
     * Makes a frame of the histogram.
     *
     * @param  theParentFrame  Model RGB
     * @param  _imageA         Model of imageA
     * @param  _imageB         Model of imageB
     * @deprecated
     * @see JPanelVolumeOpacity
     */
    public JPanelVolOpacityRGB(VolumeTriPlanarInterface theParentFrame, ModelImage _imageA, ModelImage _imageB) {
        super(theParentFrame);

        rendererProgressBar = ViewJFrameVolumeView.getRendererProgressBar();

        int[] RGBExtents = new int[2];
        RGBExtents[0] = 4;
        RGBExtents[1] = 256;

        imageA = _imageA;
        imageB = _imageB;
        RGBTA = new ModelRGB(RGBExtents);
        RGBTB = new ModelRGB(RGBExtents);
        RGBTA_GM = new ModelRGB(RGBExtents);
        RGBTB_GM = new ModelRGB(RGBExtents);

        tabbedPane = new JTabbedPane();
        tabbedPane.setFont(MipavUtil.font12B);

        toolBar = buildRGBToolBar(this);

        buildPanelA();

        if (imageB != null) {
            buildPanelB();
        }

        JPanel blendPanel = buildBlendPanel();

        GridBagLayout gbLayout = new GridBagLayout();
        GridBagConstraints gbConstraints = new GridBagConstraints();
        JPanel subPanel = new JPanel(gbLayout);
        subPanel.setLayout(gbLayout);

        gbConstraints.weightx = 1;
        gbConstraints.fill = GridBagConstraints.HORIZONTAL;
        gbConstraints.anchor = GridBagConstraints.NORTH;
        subPanel.add(toolBar, gbConstraints);

        gbConstraints.gridy = 1;
        gbConstraints.fill = GridBagConstraints.BOTH;
        subPanel.add(tabbedPane, gbConstraints);

        gbConstraints.gridy = 2;
        gbConstraints.weighty = 1;
        gbConstraints.fill = GridBagConstraints.HORIZONTAL;
        subPanel.add(blendPanel, gbConstraints);
        
        JScrollPane scrollPaneA = new JScrollPane(subPanel);
        scrollPaneA.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);

        mainPanel = new JPanel(new GridLayout(1, 1));
        mainPanel.add(scrollPaneA);
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

        if (command.equals("Linear")) {
            ViewJComponentVolOpacityBase selectedComponent = getSelectedComponent();
            selectedComponent.linearMode();
        } else if (command.equals("resetLinearBackSlash")) {
            ViewJComponentVolOpacityBase selectedComponent = getSelectedComponent();
            selectedComponent.linearBackSlashMode();
        } else if (command.equals("Horizon")) {
            ViewJComponentVolOpacityBase selectedComponent = getSelectedComponent();
            selectedComponent.horizonMode();
        }
        if ( m_kVolumeViewer != null )
        {
            m_kVolumeViewer.updateImages(true);
        }
    }

    /**
     * Add the gradient magnitude hitogram to the opacity control panel.
     */
    public void addGM() {

        if (panelOpacityGM_A == null) {
            rendererProgressBar.setValue(0);
            rendererProgressBar.update(rendererProgressBar.getGraphics());

            calcHistogramGM();
            rendererProgressBar.setValue(100);
            rendererProgressBar.update(rendererProgressBar.getGraphics());
            buildPanelGM_A();

            if (imageB != null) {
                buildPanelGM_B();
            }
        } else {

            if (tabbedPane.indexOfComponent(panelOpacityGM_A) == -1) {
                tabbedPane.addTab(OPACITY_COMPONENT_TAB_A_GM, null, panelOpacityGM_A);
            }

            if (imageB != null) {

                if (tabbedPane.indexOfComponent(panelOpacityGM_B) == -1) {
                    tabbedPane.addTab(OPACITY_COMPONENT_TAB_B_GM, null, panelOpacityGM_B);
                }
            }
        }           
        if ( m_kVolumeViewer != null )
        {
            m_kVolumeViewer.setGradientMagnitude(true);
        }
    }

    /**
     * Method to build the toolbar for the RGB frame.
     *
     * @param   al  action listener (this frame)
     *
     * @return  DOCUMENT ME!
     */
    public JToolBar buildRGBToolBar(ActionListener al) {
        JToolBar RGBToolBar = new JToolBar();
        Border pressedBorder = BorderFactory.createLoweredBevelBorder();
        Border etchedBorder = BorderFactory.createEtchedBorder();

        RGBToolBar.setBorder(etchedBorder);
        RGBToolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);

        JButton resetButton = new JButton(MipavUtil.getIcon("linear.gif"));

        resetButton.addActionListener(al);
        resetButton.setMargin(new Insets(0, 0, 0, 0));
        resetButton.setToolTipText("Reset to linear transfer function");
        resetButton.setActionCommand("Linear");
        resetButton.setBorderPainted(false);
        resetButton.setRolloverEnabled(true);
        resetButton.setRolloverIcon(MipavUtil.getIcon("linearroll.gif"));
        resetButton.setBorder(pressedBorder);
        resetButton.setFocusPainted(false);
        RGBToolBar.add(resetButton);

        JButton linearBackSlashButton = new JButton(MipavUtil.getIcon("linearbackslash.gif"));

        linearBackSlashButton.addActionListener(this);
        linearBackSlashButton.setRolloverIcon(MipavUtil.getIcon("linearbackslashroll.gif"));
        linearBackSlashButton.setBorderPainted(false);
        linearBackSlashButton.setToolTipText("Reset transfer function");
        linearBackSlashButton.setActionCommand("resetLinearBackSlash");
        RGBToolBar.add(linearBackSlashButton);

        JButton horizonButton = new JButton(MipavUtil.getIcon("linearhorizon.gif"));

        horizonButton.addActionListener(al);
        horizonButton.setMargin(new Insets(0, 0, 0, 0));
        horizonButton.setToolTipText("Reset to linear horizontal transfer function");
        horizonButton.setActionCommand("Horizon");
        horizonButton.setBorderPainted(false);
        horizonButton.setRolloverEnabled(true);
        horizonButton.setRolloverIcon(MipavUtil.getIcon("linearhorizonroll.gif"));
        horizonButton.setBorder(pressedBorder);
        horizonButton.setFocusPainted(false);
        RGBToolBar.add(horizonButton);

        RGBToolBar.add(ViewToolBarBuilder.makeSeparator());
        GMCheckBox = new JCheckBox("Gradient Map", false);
        GMCheckBox.setFont(MipavUtil.font12);
        GMCheckBox.addItemListener(this);
        RGBToolBar.add(GMCheckBox);

        RGBToolBar.setFloatable(false);

        return RGBToolBar;
    }

    /**
     * Disposes of components and frame.
     */
    public void disposeLocal() {

        if (componentOpacityA != null) {
            componentOpacityA.dispose();
        }

        if (histogramARed != null) {
            histogramARed.disposeLocal();
        }

        if (histogramAGreen != null) {
            histogramAGreen.disposeLocal();
        }

        if (histogramABlue != null) {
            histogramABlue.disposeLocal();
        }

        histogramAGreen = null;
        histogramABlue = null;
        componentOpacityA = null;

        if (histogramBRed != null) {

            if (componentOpacityB != null) {
                componentOpacityB.dispose();
            }

            if (histogramBRed != null) {
                histogramBRed.disposeLocal();
            }

            if (histogramBGreen != null) {
                histogramBGreen.disposeLocal();
            }

            if (histogramBBlue != null) {
                histogramBBlue.disposeLocal();
            }

            histogramBRed = null;
            histogramBGreen = null;
            histogramBBlue = null;
            componentOpacityB = null;
        }

        if (RGBTA != null) {
            RGBTA.disposeLocal();
            RGBTA = null;
        }

        if (RGBTB != null) {
            RGBTB.disposeLocal();
            RGBTB = null;
        }

        if (RGBTA_GM != null) {
            RGBTA_GM.disposeLocal();
            RGBTA_GM = null;
        }

        if (RGBTB_GM != null) {
            RGBTB_GM.disposeLocal();
            RGBTB_GM = null;
        }

        if (renderBase != null) {
            renderBase = null;
        }

        super.disposeLocal();
    }

    /**
     * Calls dispose.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    public void finalize() throws Throwable {
        disposeLocal();
        super.finalize();
    }

    /**
     * Get the gradient magnitude imageA.
     *
     * @return  ModelImage GM imageA
     */
    public ModelImage getGradMagA() {
        return gradMagRescale_A;
    }

    /**
     * Get the gradient magnitude imageA.
     *
     * @return  ModelImage GM imageA
     */
    public ModelImage getGradMagB() {
        return gradMagRescale_B;
    }

    /**
     * Returns the opacity transfer function for image A.
     *
     * @param   channel  DOCUMENT ME!
     *
     * @return  TransferFunction
     */
    public TransferFunction getOpacityAfn(int channel) {
        return componentOpacityA.getOpacityTransferFunction();
    }

    /**
     * Returns the opacity transfer function for image B.
     *
     * @param   channel  DOCUMENT ME!
     *
     * @return  TransferFunction
     */
    public TransferFunction getOpacityBfn(int channel) {
        return componentOpacityB.getOpacityTransferFunction();
    }

    /**
     * Returns the opacity transfer function for image A.
     *
     * @param   channel  DOCUMENT ME!
     *
     * @return  TransferFunction
     */
    public TransferFunction getOpacityGM_Afn(int channel) {
        return componentOpacityGM_A.getOpacityTransferFunction();
    }

    /**
     * Returns the opacity transfer function for image B.
     *
     * @param   channel  DOCUMENT ME!
     *
     * @return  TransferFunction
     */
    public TransferFunction getOpacityGM_Bfn(int channel) {
        return componentOpacityGM_B.getOpacityTransferFunction();
    }

    /**
     * Returns the opacity transfer function via the ModelRGB object for image B.
     *
     * @return  ModelRGB
     */
    public ModelRGB getRGB_OpacityA() {
        return RGBTA;
    }

    /**
     * Returns the opacity transfer function for the gradient magnitude image via the ModelRGB object for image A.
     *
     * @return  ModelRGB
     */
    public ModelRGB getRGB_OpacityA_GM() {
        return RGBTA_GM;
    }

    /**
     * Returns the opacity transfer function via the ModelRGB object for image B.
     *
     * @return  ModelRGB
     */
    public ModelRGB getRGB_OpacityB() {
        return RGBTB;
    }

    /**
     * Returns the opacity transfer function for the gradient magnitude image via the ModelRGB object for image B.
     *
     * @return  ModelRGB
     */
    public ModelRGB getRGB_OpacityB_GM() {
        return RGBTB_GM;
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

        if (source == GMCheckBox) {

            if (GMCheckBox.isSelected() == true) {
                addGM();
            } else {
                removeGM();
            }
        }
    }

    /**
     * Add the gradient magnitude hitogram to the opacity control panel.
     */
    public void removeGM() {

        if (imageA != null) {
            tabbedPane.remove(panelOpacityGM_A);
        }

        if (imageB != null) {
            tabbedPane.remove(panelOpacityGM_B);
        }
        if ( m_kVolumeViewer != null )
        {
            m_kVolumeViewer.setGradientMagnitude(false);
        }
    }

    /**
     * Method that displays the histogram and LUT and other controls to manipulate the LUT. Panel for image A.
     */
    private void buildPanelA() {
        int borderSize = 3;

        histogramARed = calcHistogram(imageA, RED);
        histogramAGreen = calcHistogram(imageA, GREEN);
        histogramABlue = calcHistogram(imageA, BLUE);

        // Make a display version of the histogram
        componentOpacityA = new ViewJComponentVolOpacityRGB(this, histogramARed, imageA);
        componentOpacityA.setLocation(borderSize, borderSize);
        componentOpacityA.setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
        componentOpacityA.setBackground(new Color(190, 208, 230));
        componentOpacityA.setMode(componentOpacityA.ALL);

        panelOpacityA = new JPanel(new BorderLayout());
        panelOpacityA.setForeground(Color.black);
        panelOpacityA.setBorder(buildTitledBorder("Opacity function for image A"));

        mouseSlider = new JSlider(0, 100, 50);
        mouseSlider.setMinorTickSpacing(10);
        mouseSlider.setPaintTicks(true);
        mouseSlider.addChangeListener(this);
        mouseSlider.setPaintLabels(true);
        mouseSlider.setLabelTable(getLabelTableA());
        mouseSlider.setEnabled((componentOpacityA.getActiveIndex() != componentOpacityA.INACTIVE) &&
                                   !componentOpacityA.getOpacityTransferFunction().isEndpoint(componentOpacityA.getActiveIndex()));

        JPanel precisionPanel = new JPanel(new GridLayout(1, 1));
        precisionPanel.add(mouseSlider);
        precisionPanel.setBorder(buildTitledBorder("Transfer function precision adjustment"));

        panelOpacityA.add(componentOpacityA, BorderLayout.CENTER);
        panelOpacityA.add(precisionPanel, BorderLayout.SOUTH);

        tabbedPane.addTab(OPACITY_COMPONENT_TAB_A, null, panelOpacityA);
    }

    /**
     * Method that displays the histogram and LUT and other controls to manipulate the LUT. Panel for image B.
     */
    private void buildPanelB() {
        int borderSize = 3;

        histogramBRed = calcHistogram(imageB, RED);
        histogramBGreen = calcHistogram(imageB, GREEN);
        histogramBBlue = calcHistogram(imageB, BLUE);

        // Make a display version of the histogram
        componentOpacityB = new ViewJComponentVolOpacityRGB(this, histogramBRed, imageB);
        componentOpacityB.setLocation(borderSize, borderSize);
        componentOpacityB.setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
        componentOpacityB.setBackground(new Color(190, 208, 230));
        componentOpacityB.setMode(componentOpacityA.ALL);

        panelOpacityB = new JPanel(new BorderLayout());
        panelOpacityB.setForeground(Color.black);
        panelOpacityB.setBorder(buildTitledBorder("Opacity function for image B"));

        mouseSliderB = new JSlider(0, 100, 50);
        mouseSliderB.setMinorTickSpacing(10);
        mouseSliderB.setPaintTicks(true);
        mouseSliderB.addChangeListener(this);
        mouseSliderB.setPaintLabels(true);
        mouseSliderB.setFont(serif12);
        mouseSliderB.setEnabled((componentOpacityB.getActiveIndex() != componentOpacityB.INACTIVE) &&
                                    !componentOpacityB.getOpacityTransferFunction().isEndpoint(componentOpacityB.getActiveIndex()));

        mouseSliderB.setLabelTable(getLabelTableB());
        mouseSliderB.setEnabled((componentOpacityB.getActiveIndex() != componentOpacityB.INACTIVE) &&
                                    !componentOpacityB.getOpacityTransferFunction().isEndpoint(componentOpacityB.getActiveIndex()));

        JPanel precisionPanel = new JPanel(new GridLayout(1, 1));
        precisionPanel.add(mouseSliderB);
        precisionPanel.setBorder(buildTitledBorder("Transfer function precision adjustment"));

        panelOpacityB.add(componentOpacityB, BorderLayout.CENTER);
        panelOpacityB.add(precisionPanel, BorderLayout.SOUTH);

        tabbedPane.addTab(OPACITY_COMPONENT_TAB_B, null, panelOpacityB);
    }

    /**
     * Method that displays the histogram and LUT and other controls to manipulate the LUT. Panel for image A.
     */
    private void buildPanelGM_A() {

        // Make a display version of the histogram
        componentOpacityGM_A = new ViewJComponentVolOpacityRGB(this, histogramGM_ARed, gradMagRescale_A);
        componentOpacityGM_A.setLocation(3, 3);
        componentOpacityGM_A.setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
        componentOpacityGM_A.setBackground(new Color(190, 208, 230));
        componentOpacityGM_A.horizonMode();
        componentOpacityGM_A.setMode(componentOpacityA.ALL);

        panelOpacityGM_A = new JPanel(new BorderLayout());
        panelOpacityGM_A.setForeground(Color.black);
        panelOpacityGM_A.setBorder(buildTitledBorder("Opacity function for image A gradient magnitude"));

        mouseSliderGM_A = new JSlider(0, 100, 50);
        mouseSliderGM_A.setMinorTickSpacing(10);
        mouseSliderGM_A.setPaintTicks(true);
        mouseSliderGM_A.addChangeListener(this);
        mouseSliderGM_A.setPaintLabels(true);
        mouseSliderGM_A.setFont(serif12);
        mouseSliderGM_A.setEnabled((componentOpacityGM_A.getActiveIndex() != componentOpacityGM_A.INACTIVE) &&
                                       !componentOpacityGM_A.getOpacityTransferFunction().isEndpoint(componentOpacityGM_A.getActiveIndex()));

        mouseSlider.setLabelTable(getLabelTableGM_A());
        mouseSlider.setEnabled((componentOpacityGM_A.getActiveIndex() != componentOpacityGM_A.INACTIVE) &&
                                   !componentOpacityGM_A.getOpacityTransferFunction().isEndpoint(componentOpacityGM_A.getActiveIndex()));

        JPanel precisionPanel = new JPanel(new GridLayout(1, 1));
        precisionPanel.add(mouseSliderGM_A);
        precisionPanel.setBorder(buildTitledBorder("Transfer function precision adjustment"));

        panelOpacityGM_A.add(componentOpacityGM_A, BorderLayout.CENTER);
        panelOpacityGM_A.add(precisionPanel, BorderLayout.SOUTH);

        tabbedPane.addTab(OPACITY_COMPONENT_TAB_A_GM, null, panelOpacityGM_A);
    }

    /**
     * Method that displays the histogram and LUT and other controls to manipulate the LUT. Panel for image B.
     */
    private void buildPanelGM_B() {

        // Make a display version of the histogram
        componentOpacityGM_B = new ViewJComponentVolOpacityRGB(this, histogramGM_BRed, gradMagRescale_B);
        componentOpacityGM_B.setLocation(3, 3);
        componentOpacityGM_B.setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
        componentOpacityGM_B.setBackground(new Color(190, 208, 230));
        componentOpacityGM_B.horizonMode();
        componentOpacityGM_B.setMode(componentOpacityB.ALL);

        panelOpacityGM_B = new JPanel(new BorderLayout());
        panelOpacityGM_B.setForeground(Color.black);
        panelOpacityGM_B.setBorder(buildTitledBorder("Opacity function for image B gradient magnitude"));

        mouseSliderGM_B = new JSlider(0, 100, 50);
        mouseSliderGM_B.setMinorTickSpacing(10);
        mouseSliderGM_B.setPaintTicks(true);
        mouseSliderGM_B.addChangeListener(this);
        mouseSliderGM_B.setPaintLabels(true);
        mouseSliderGM_B.setFont(serif12);
        mouseSliderGM_B.setEnabled((componentOpacityGM_B.getActiveIndex() != componentOpacityGM_B.INACTIVE) &&
                                       !componentOpacityGM_B.getOpacityTransferFunction().isEndpoint(componentOpacityGM_B.getActiveIndex()));

        mouseSlider.setLabelTable(getLabelTableGM_B());
        mouseSlider.setEnabled((componentOpacityGM_B.getActiveIndex() != componentOpacityGM_B.INACTIVE) &&
                                   !componentOpacityGM_B.getOpacityTransferFunction().isEndpoint(componentOpacityGM_B.getActiveIndex()));

        JPanel precisionPanel = new JPanel(new GridLayout(1, 1));
        precisionPanel.add(mouseSliderGM_B);
        precisionPanel.setBorder(buildTitledBorder("Transfer function precision adjustment"));

        panelOpacityGM_B.add(componentOpacityGM_B, BorderLayout.CENTER);
        panelOpacityGM_B.add(precisionPanel, BorderLayout.SOUTH);

        tabbedPane.addTab(OPACITY_COMPONENT_TAB_B_GM, null, panelOpacityGM_B);

    }

    /**
     * Calculates histogram for the image(s).
     *
     * @param   image    DOCUMENT ME!
     * @param   channel  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private ModelHistogram calcHistogram(ModelImage image, int channel) {

        ModelHistogram histogram = null;

        int[] dimExtentsA = new int[1];

        if (image != null) {

            if (imageA.getType() != ModelStorageBase.ARGB_USHORT) {
                dimExtentsA[0] = 256;
            } else {
                dimExtentsA[0] = (int) (imageA.getMaxR() - imageA.getMinR() + 0.5) + 1;
            }

            histogram = new ModelHistogram(ModelStorageBase.INTEGER, dimExtentsA);

            AlgorithmHistogram histoAlgo = new AlgorithmHistogram(histogram, channel, image, true);
            histoAlgo.run();

            if (histoAlgo.isCompleted()) {
                histoAlgo.finalize();
                histoAlgo = null;
            }
        }

        return histogram;
    }

    /**
     * Calculates the histogram for the color images GM.
     */
    private void calcHistogramGM() {

        ModelImage gradMag_A, gradMag_B;
        float[] sigma = new float[3];

        sigma[0] = 0.5f;
        sigma[1] = 0.5f;
        sigma[2] = 0.5f;

        if (imageA != null) {
            gradMag_A = new ModelImage(ModelImage.ARGB, imageA.getExtents(), imageA.getImageName() + "_gm");
            gradMagRescale_A = new ModelImage(ModelImage.ARGB, imageA.getExtents(),
                                              imageA.getImageName() + "_gm_rescale");

            if (!loadGMImage(ViewUserInterface.getReference().getDefaultDirectory(),
                                 imageA.getImageName() + "_gm_rescale" + ".xml", true)) {
                rendererProgressBar.setValue(10);
                rendererProgressBar.update(rendererProgressBar.getGraphics());

                AlgorithmGradientMagnitude gradMagAlgo_A = new AlgorithmGradientMagnitude(gradMag_A, imageA, sigma,
                                                                                          true, false);

                gradMagAlgo_A.setRed(true);
                gradMagAlgo_A.setBlue(true);
                gradMagAlgo_A.setGreen(true);
                gradMagAlgo_A.setRunningInSeparateThread(isActiveImage); // progress bar junk.
                gradMagAlgo_A.run();

                if (gradMagAlgo_A.isCompleted()) {
                    gradMagAlgo_A.finalize();
                    gradMagAlgo_A = null;
                }

                rendererProgressBar.setValue(20);
                rendererProgressBar.update(rendererProgressBar.getGraphics());

                AlgorithmChangeType changeTypeAlgo_A = new AlgorithmChangeType(gradMagRescale_A, gradMag_A,
                                                                               gradMag_A.getMin(), gradMag_A.getMax(),
                                                                               0, 255, false);

                changeTypeAlgo_A.setRunningInSeparateThread(isActiveImage);
                changeTypeAlgo_A.run();
                gradMagRescale_A.calcMinMax();

                if (changeTypeAlgo_A.isCompleted()) {

                    if (gradMagRescale_A != null) {
                        rendererProgressBar.setValue(30);
                        rendererProgressBar.update(rendererProgressBar.getGraphics());
                        ModelImage.saveImage(gradMagRescale_A);
                        rendererProgressBar.setValue(40);
                        rendererProgressBar.update(rendererProgressBar.getGraphics());
                        histogramGM_ARed = calcHistogram(gradMagRescale_A, RED);
                        histogramGM_AGreen = calcHistogram(gradMagRescale_A, GREEN);
                        histogramGM_ABlue = calcHistogram(gradMagRescale_A, BLUE);
                    }

                    changeTypeAlgo_A.finalize();
                    changeTypeAlgo_A = null;
                }

                rendererProgressBar.setValue(50);
                rendererProgressBar.update(rendererProgressBar.getGraphics());
            } else { // Gradient Magnitude image is calculated already.
                rendererProgressBar.setValue(20);
                rendererProgressBar.update(rendererProgressBar.getGraphics());
                histogramGM_ARed = calcHistogram(gradMagRescale_A, RED);
                histogramGM_AGreen = calcHistogram(gradMagRescale_A, GREEN);
                histogramGM_ABlue = calcHistogram(gradMagRescale_A, BLUE);
            }

            rendererProgressBar.setValue(50);
            rendererProgressBar.update(rendererProgressBar.getGraphics());

            if (gradMag_A != null) {
                gradMag_A.disposeLocal();
                gradMag_A = null;
            }
        }

        if (imageB != null) {
            gradMag_B = new ModelImage(ModelImage.ARGB, imageB.getExtents(), imageB.getImageName() + "_gm");
            gradMagRescale_B = new ModelImage(ModelImage.ARGB, imageB.getExtents(),
                                              imageB.getImageName() + "_gm_rescale");

            if (!loadGMImage(ViewUserInterface.getReference().getDefaultDirectory(),
                                 imageB.getImageName() + "_gm_rescale" + ".xml", false)) {
                rendererProgressBar.setValue(60);
                rendererProgressBar.update(rendererProgressBar.getGraphics());

                AlgorithmGradientMagnitudeSep gradMagAlgo_B = new AlgorithmGradientMagnitudeSep(imageB,
                                                                                                sigma, true, false);

                gradMagAlgo_B.setRed(true);
                gradMagAlgo_B.setBlue(true);
                gradMagAlgo_B.setGreen(true);
                gradMagAlgo_B.setRunningInSeparateThread(isActiveImage); // progress bar junk.
                gradMagAlgo_B.run();

                if (gradMagAlgo_B.isCompleted()) {
                    try{
                    	gradMag_B.importData(0, gradMagAlgo_B.getResultBuffer(), true);
                    }catch(IOException e){
            			MipavUtil.displayError("Algorithm Watershed importData: Image(s) locked");
            			gradMag_B.disposeLocal();
            			return;
                    }
                    gradMagAlgo_B.finalize();
                    gradMagAlgo_B = null;
                }

                rendererProgressBar.setValue(70);
                rendererProgressBar.update(rendererProgressBar.getGraphics());

                AlgorithmChangeType changeTypeAlgo_B = new AlgorithmChangeType(gradMagRescale_B, gradMag_B,
                                                                               gradMag_B.getMin(), gradMag_B.getMax(),
                                                                               0, 255, false);

                changeTypeAlgo_B.setRunningInSeparateThread(isActiveImage);
                changeTypeAlgo_B.run();
                gradMagRescale_B.calcMinMax();

                if (changeTypeAlgo_B.isCompleted()) {

                    if (gradMagRescale_B != null) {
                        ModelImage.saveImage(gradMagRescale_B);
                        rendererProgressBar.setValue(80);
                        rendererProgressBar.update(rendererProgressBar.getGraphics());
                        histogramGM_BRed = calcHistogram(gradMagRescale_B, RED);
                        histogramGM_BGreen = calcHistogram(gradMagRescale_B, GREEN);
                        histogramGM_BBlue = calcHistogram(gradMagRescale_B, BLUE);
                    }

                    changeTypeAlgo_B.finalize();
                    changeTypeAlgo_B = null;
                    rendererProgressBar.setValue(90);
                    rendererProgressBar.update(rendererProgressBar.getGraphics());
                }

            } else { // Gradient Magnitude image is calculated already.
                rendererProgressBar.setValue(20);
                rendererProgressBar.update(rendererProgressBar.getGraphics());
                rendererProgressBar.setValue(80);
                rendererProgressBar.update(rendererProgressBar.getGraphics());
                histogramGM_BRed = calcHistogram(gradMagRescale_B, RED);
                histogramGM_BGreen = calcHistogram(gradMagRescale_B, GREEN);
                histogramGM_BBlue = calcHistogram(gradMagRescale_B, BLUE);
            }

            if (gradMag_B != null) {
                gradMag_B.disposeLocal();
                gradMag_B = null;
            }
        }
    }
}
