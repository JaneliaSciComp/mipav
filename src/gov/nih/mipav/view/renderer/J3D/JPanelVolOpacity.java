package gov.nih.mipav.view.renderer.J3D;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.*;
import gov.nih.mipav.view.renderer.WildMagic.*;

import java.awt.*;
import java.awt.event.*;
import java.io.IOException;

import javax.swing.*;


/**
 * Simple panel containing the volume renderer opacity controls.
 *
 * @version  0.1 May, 2003
 * @deprecated
 * @see JPanelVolumeOpacity
 */
public class JPanelVolOpacity extends JPanelVolOpacityBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2618795264166878795L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Histogram reference for image A. */
    private ModelHistogram histogramA = null;

    /** Histogram reference for image B. */
    private ModelHistogram histogramB = null;

    /** Histogram reference for imageA GM. */
    private ModelHistogram histogramGM_A = null;

    /** Histogram reference for imageB GM. */
    private ModelHistogram histogramGM_B = null;

    /** A reference to the volume renderer frame's progress bar. */
    private JProgressBar rendererProgressBar;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog for converting type of image.
     *
     * @param  theParentFrame  Parent frame.
     * @param  _imgA           Source imageA.
     * @param  _imgB           Source imageB.
     * @deprecated
     * @see JPanelVolumeOpacity
     */
    public JPanelVolOpacity(RenderViewBase theParentFrame, ModelImage _imgA, ModelImage _imgB) {
        super(theParentFrame);

        rendererProgressBar = ViewJFrameVolumeView.getRendererProgressBar();

        imageA = _imgA;
        imageB = _imgB;
        initialize();
    }

    /**
     * Creates new dialog for converting type of image.
     *
     * @param  theParentFrame  Parent frame.
     * @param  _imgA           Source imageA.
     * @param  _imgB           Source imageB.
     * @deprecated
     * @see JPanelVolumeOpacity
     */
    public JPanelVolOpacity(VolumeTriPlanarInterface theParentFrame, ModelImage _imgA, ModelImage _imgB) {
        super(theParentFrame);
        
        rendererProgressBar = VolumeTriPlanarInterface.getRendererProgressBar();

        
        imageA = _imgA;
        imageB = _imgB;
        initialize();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and sets the variables.
     *
     * @param  event  Event that triggers this function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) { }
        else if (command.equals("Script")) { }
        else if (command.equals("Close")) { }
        else if (command.equals("OpenOpacityFrom")) { }
        else if (command.equals("SaveOpacityAs")) { }
        else if (command.equals("FullRange")) { }
        else if (command.equals("UserRange")) { }
        else if (command.equals("resetLinear")) {
            ViewJComponentVolOpacityBase selectedComponent = getSelectedComponent();
            selectedComponent.linearMode();
        } else if (command.equals("resetLinearBackSlash")) {
            ViewJComponentVolOpacityBase selectedComponent = getSelectedComponent();
            selectedComponent.linearBackSlashMode();
        } else if (command.equals("resetHorizon")) {
            ViewJComponentVolOpacityBase selectedComponent = getSelectedComponent();
            selectedComponent.horizonMode();
        }
        if ( renderBase instanceof 
                gov.nih.mipav.view.renderer.J3D.surfaceview.SurfaceRender )
        {
            gov.nih.mipav.view.renderer.J3D.surfaceview.SurfaceRender kParent = (gov.nih.mipav.view.renderer.J3D.surfaceview.SurfaceRender) renderBase;
            ViewJFrameVolumeView kParentFrame = kParent.getParentFrame();
        }
    } // end actionPerformed()

    /**
     * Add the gradient magnitude histogram to the opacity control panel.
     */
    public void addGM() {

        if (panelOpacityGM_A == null) {
            rendererProgressBar.setValue(0);
            rendererProgressBar.update(rendererProgressBar.getGraphics());

            calcHistogramsGM();
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
        if ( renderBase instanceof 
                gov.nih.mipav.view.renderer.J3D.surfaceview.SurfaceRender )
        {
            gov.nih.mipav.view.renderer.J3D.surfaceview.SurfaceRender kParent = (gov.nih.mipav.view.renderer.J3D.surfaceview.SurfaceRender) renderBase;
            ViewJFrameVolumeView kParentFrame = kParent.getParentFrame();
        }            
        if ( m_kVolumeViewer != null )
        {
            m_kVolumeViewer.updateImages(true);
            m_kVolumeViewer.setGradientMagnitude( true );
        }
    }

    /**
     * Clear memory and garbage collection.
     */
    public void disposeLocal() {

        if (gradMagRescale_A != null) {
            gradMagRescale_A.disposeLocal();
            gradMagRescale_A = null;
        }

        if (gradMagRescale_B != null) {
            gradMagRescale_B.disposeLocal();
            gradMagRescale_B = null;
        }

        if (histogramA != null) {
            histogramA.disposeLocal();
            histogramA = null;
        }

        if (histogramB != null) {
            histogramB.disposeLocal();
            histogramB = null;
        }

        if (histogramGM_A != null) {
            histogramGM_A.disposeLocal();
            histogramGM_A = null;
        }

        if (histogramGM_B != null) {
            histogramGM_B.disposeLocal();
            histogramGM_B = null;
        }

        if (componentOpacityA != null) {
            componentOpacityA.dispose();
            componentOpacityA = null;
        }

        if (componentOpacityB != null) {
            componentOpacityB.dispose();
            componentOpacityB = null;
        }

        if (componentOpacityGM_A != null) {
            componentOpacityGM_A.dispose();
            componentOpacityGM_A = null;
        }

        if (componentOpacityGM_B != null) {
            componentOpacityGM_B.dispose();
            componentOpacityGM_B = null;
        }

        super.disposeLocal();
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
     * Get the gradient magnitude imageB.
     *
     * @return  ModelImage GM imageB
     */
    public ModelImage getGradMagB() {
        return gradMagRescale_B;
    }

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
        if ( renderBase instanceof 
                gov.nih.mipav.view.renderer.J3D.surfaceview.SurfaceRender )
        {
            gov.nih.mipav.view.renderer.J3D.surfaceview.SurfaceRender kParent = (gov.nih.mipav.view.renderer.J3D.surfaceview.SurfaceRender) renderBase;
            ViewJFrameVolumeView kParentFrame = kParent.getParentFrame();
        }         
        if ( m_kVolumeViewer != null )
        {
            m_kVolumeViewer.setGradientMagnitude( false );
        }
    }

    /**
     * Calls dispose.
     *
     * @throws  Throwable  DOCUMENT ME!
     */
    protected void finalize() throws Throwable {
        disposeLocal();
        super.finalize();
    }

    /**
     * Method that displays the histogram and controls to manipulate the opacity. Panel for image A.
     */
    private void buildPanelA() {
        int borderSize = 3;
        scaleRangeA = ((int) Math.round(imageA.getMax() - imageA.getMin()) + 1) / 256;

        // Make a display version of the histogram
        componentOpacityA = new ViewJComponentVolOpacity(this, histogramA, imageA);
        componentOpacityA.setLocation(borderSize, borderSize);
        componentOpacityA.setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
        componentOpacityA.setBackground(new Color(190, 208, 230));

        panelOpacityA = new JPanel(new BorderLayout());
        panelOpacityA.setForeground(Color.black);
        panelOpacityA.setBorder(buildTitledBorder("Opacity function for image A"));

        mouseSlider = new JSlider(0, 100, 50);
        mouseSlider.setMinorTickSpacing(10);
        mouseSlider.setPaintTicks(true);
        mouseSlider.addChangeListener(this);
        mouseSlider.setPaintLabels(true);
        mouseSlider.setFont(serif12);
        mouseSlider.setEnabled((componentOpacityA.getActiveIndex() != componentOpacityA.INACTIVE) &&
                                   !componentOpacityA.getOpacityTransferFunction().isEndpoint(componentOpacityA.getActiveIndex()));

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
     * Method that displays the histogram and controls to manipulate the opacity. Panel for image B. Reset the layout.
     */
    private void buildPanelB() {
        int borderSize = 3;
        scaleRangeB = ((int) Math.round(imageA.getMax() - imageA.getMin()) + 1) / 256;

        // Make a display version of the histogram
        componentOpacityB = new ViewJComponentVolOpacity(this, histogramB, imageB);
        componentOpacityB.setLocation(borderSize, borderSize);
        componentOpacityB.setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
        componentOpacityB.setBackground(new Color(190, 208, 230));

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
     * Method that displays the histogram and controls to manipulate the opacity. Panel for image GM_A.
     */
    private void buildPanelGM_A() {
        int borderSize = 3;
        scaleRangeGM_A = ((int) Math.round(gradMagRescale_A.getMax() - gradMagRescale_A.getMin()) + 1) / 256;

        // Make a display version of the histogram
        componentOpacityGM_A = new ViewJComponentVolOpacity(this, histogramGM_A, gradMagRescale_A);
        componentOpacityGM_A.setLocation(borderSize, borderSize);
        componentOpacityGM_A.setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
        componentOpacityGM_A.setBackground(new Color(190, 208, 230));

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
     * Method that displays the histogram and controls to manipulate the opacity. Panel for image GM_B.
     */
    private void buildPanelGM_B() {
        int borderSize = 3;
        scaleRangeGM_B = ((int) Math.round(gradMagRescale_B.getMax() - gradMagRescale_B.getMin()) + 1) / 256;

        // Make a display version of the histogram
        componentOpacityGM_B = new ViewJComponentVolOpacity(this, histogramGM_B, gradMagRescale_B);
        componentOpacityGM_B.setLocation(borderSize, borderSize);
        componentOpacityGM_B.setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
        componentOpacityGM_B.setBackground(new Color(190, 208, 230));

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
     * Calculates histogram for the imageA, B.
     */
    private void calcHistograms() {
        int[] dimExtentsA = new int[1];
        int[] dimExtentsB = new int[1];

        if (imageA != null) {
            dimExtentsA[0] = 256;
            histogramA = new ModelHistogram(ModelStorageBase.INTEGER, dimExtentsA);

            AlgorithmHistogram histoAlgoA = new AlgorithmHistogram(histogramA, imageA, true);
            histoAlgoA.setRunningInSeparateThread(isActiveImage);
            histoAlgoA.run();

            if (histoAlgoA.isCompleted()) {
                histoAlgoA.finalize();
                histoAlgoA = null;
            }
        }

        if (imageB != null) {
            dimExtentsB[0] = 256;
            histogramB = new ModelHistogram(ModelStorageBase.INTEGER, dimExtentsB);

            AlgorithmHistogram histoAlgoB = new AlgorithmHistogram(histogramB, imageB, true);
            histoAlgoB.setRunningInSeparateThread(isActiveImage);
            histoAlgoB.run();

            if (histoAlgoB.isCompleted()) {
                histoAlgoB.finalize();
                histoAlgoB = null;
            }
        }
    }

    /**
     * Calculates histogram for the gradient magnitude imageA, B.
     */
    private void calcHistogramsGM() {

        int[] dimExtentsGM_A = new int[1];
        int[] dimExtentsGM_B = new int[1];

        ModelImage gradMag_A, gradMag_B;
        float[] sigma = new float[3];

        boolean loadImageA = false;
        boolean loadImageB = false;

        sigma[0] = 0.5f;
        sigma[1] = 0.5f;
        sigma[2] = 0.5f;

        if (imageA != null) {
            gradMag_A = new ModelImage(ModelImage.FLOAT, imageA.getExtents(), imageA.getImageName() + "_gm");
            gradMagRescale_A = new ModelImage(ModelImage.USHORT, imageA.getExtents(),
                                              imageA.getImageName() + "_gm_rescale");

            loadImageA = loadGMImage(ViewUserInterface.getReference().getDefaultDirectory(),
                                     imageA.getImageName() + "_gm_rescale" + ".xml", true);

            if (gradMagRescale_A.getExtents()[2] != imageA.getExtents()[2]) {
                gradMagRescale_A = new ModelImage(ModelImage.USHORT, imageA.getExtents(),
                                                  imageA.getImageName() + "_gm_rescale");
                loadImageA = false;
            }

            if (!loadImageA) {
                rendererProgressBar.setValue(10);
                rendererProgressBar.update(rendererProgressBar.getGraphics());

                AlgorithmGradientMagnitude gradMagAlgo_A = new AlgorithmGradientMagnitude(gradMag_A, imageA, sigma,
                                                                                          true, false);

                gradMagAlgo_A.setRunningInSeparateThread(isActiveImage); // progress bar junk.
                gradMagAlgo_A.run();

                if (gradMagAlgo_A.isCompleted()) {
                    gradMagAlgo_A.finalize();
                    gradMagAlgo_A = null;
                }

                rendererProgressBar.setValue(20);
                rendererProgressBar.update(rendererProgressBar.getGraphics());

                /** Scale the intensity range to 1024. */
                AlgorithmChangeType changeTypeAlgo_A = new AlgorithmChangeType(gradMagRescale_A, gradMag_A,
                                                                               gradMag_A.getMin(), gradMag_A.getMax(),
                                                                               0, 1023, false);

                changeTypeAlgo_A.setRunningInSeparateThread(isActiveImage);
                changeTypeAlgo_A.run();
                gradMagRescale_A.calcMinMax();
                rendererProgressBar.setValue(30);
                rendererProgressBar.update(rendererProgressBar.getGraphics());

                if (changeTypeAlgo_A.isCompleted()) {
                    ModelImage.saveImage(gradMagRescale_A);
                    rendererProgressBar.setValue(40);
                    rendererProgressBar.update(rendererProgressBar.getGraphics());

                    if (gradMagRescale_A != null) {

                        dimExtentsGM_A[0] = 1024;
                        histogramGM_A = new ModelHistogram(ModelStorageBase.INTEGER, dimExtentsGM_A);

                        AlgorithmHistogram histoAlgoGM_A = new AlgorithmHistogram(histogramGM_A, gradMagRescale_A,
                                                                                  true);
                        histoAlgoGM_A.setRunningInSeparateThread(isActiveImage);
                        histoAlgoGM_A.run();

                        if (histoAlgoGM_A.isCompleted()) {
                            histoAlgoGM_A.finalize();
                            histoAlgoGM_A = null;
                        }
                    }

                    changeTypeAlgo_A.finalize();
                    changeTypeAlgo_A = null;
                    rendererProgressBar.setValue(50);
                    rendererProgressBar.update(rendererProgressBar.getGraphics());
                }
                JDialogBase.updateFileInfo(imageA, gradMagRescale_A);
            } else {
                dimExtentsGM_A[0] = 1024;
                rendererProgressBar.setValue(20);
                rendererProgressBar.update(rendererProgressBar.getGraphics());
                histogramGM_A = new ModelHistogram(ModelStorageBase.INTEGER, dimExtentsGM_A);

                AlgorithmHistogram histoAlgoGM_A = new AlgorithmHistogram(histogramGM_A, gradMagRescale_A, true);
                histoAlgoGM_A.setRunningInSeparateThread(isActiveImage);
                histoAlgoGM_A.run();

                if (histoAlgoGM_A.isCompleted()) {
                    histoAlgoGM_A.finalize();
                    histoAlgoGM_A = null;
                }

                rendererProgressBar.setValue(50);
                rendererProgressBar.update(rendererProgressBar.getGraphics());
            }

            if (gradMag_A != null) {
                gradMag_A.disposeLocal();
                gradMag_A = null;
            }
        }

        if (imageB != null) {
            gradMag_B = new ModelImage(ModelImage.FLOAT, imageB.getExtents(), imageB.getImageName() + "_gm");
            gradMagRescale_B = new ModelImage(ModelImage.USHORT, imageB.getExtents(),
                                              imageB.getImageName() + "_gm_rescale");

            loadImageB = loadGMImage(ViewUserInterface.getReference().getDefaultDirectory(),
                                     imageB.getImageName() + "_gm_rescale" + ".xml", true);

            if (gradMagRescale_B.getExtents()[2] != imageB.getExtents()[2]) {
                gradMagRescale_B = new ModelImage(ModelImage.USHORT, imageB.getExtents(),
                                                  imageB.getImageName() + "_gm_rescale");
                loadImageB = false;
            }

            if (!loadImageB) {
                rendererProgressBar.setValue(60);
                rendererProgressBar.update(rendererProgressBar.getGraphics());

                AlgorithmGradientMagnitudeSep gradMagAlgo_B = new AlgorithmGradientMagnitudeSep(imageB,
                                                                                                sigma, true, false);

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

                /** Scale the intensity range to 1024. */
                AlgorithmChangeType changeTypeAlgo_B = new AlgorithmChangeType(gradMagRescale_B, gradMag_B,
                                                                               gradMag_B.getMin(), gradMag_B.getMax(),
                                                                               0, 1023, false);

                changeTypeAlgo_B.setRunningInSeparateThread(isActiveImage);
                changeTypeAlgo_B.run();

                if (changeTypeAlgo_B.isCompleted()) {
                    rendererProgressBar.setValue(80);
                    rendererProgressBar.update(rendererProgressBar.getGraphics());
                    ModelImage.saveImage(gradMagRescale_B);

                    if (gradMagRescale_B != null) {
                        dimExtentsGM_B[0] = 1024;
                        histogramGM_B = new ModelHistogram(ModelStorageBase.INTEGER, dimExtentsGM_B);

                        AlgorithmHistogram histoAlgoGM_B = new AlgorithmHistogram(histogramGM_B, gradMagRescale_B,
                                                                                  true);
                        histoAlgoGM_B.setRunningInSeparateThread(isActiveImage);
                        histoAlgoGM_B.run();

                        if (histoAlgoGM_B.isCompleted()) {
                            histoAlgoGM_B.finalize();
                            histoAlgoGM_B = null;
                        }

                    }

                    changeTypeAlgo_B.finalize();
                    changeTypeAlgo_B = null;
                    rendererProgressBar.setValue(90);
                    rendererProgressBar.update(rendererProgressBar.getGraphics());
                }
                JDialogBase.updateFileInfo(imageA, gradMagRescale_A);
            } else {
                dimExtentsGM_B[0] = 1024;
                rendererProgressBar.setValue(60);
                rendererProgressBar.update(rendererProgressBar.getGraphics());
                histogramGM_B = new ModelHistogram(ModelStorageBase.INTEGER, dimExtentsGM_B);

                AlgorithmHistogram histoAlgoGM_B = new AlgorithmHistogram(histogramGM_B, gradMagRescale_B, true);
                histoAlgoGM_B.setRunningInSeparateThread(isActiveImage);
                histoAlgoGM_B.run();

                if (histoAlgoGM_B.isCompleted()) {
                    histoAlgoGM_B.finalize();
                    histoAlgoGM_B = null;
                }

                rendererProgressBar.setValue(90);
                rendererProgressBar.update(rendererProgressBar.getGraphics());
            }

            if (gradMag_B != null) {
                gradMag_B.disposeLocal();
                gradMag_B = null;
            }
        }
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen. Change the layout of the volume opacity
     * change diagram.
     */
    private void initialize() {
        calcHistograms();

        if (mainPanel != null) {
            mainPanel.removeAll();
        } else {
            mainPanel = new JPanel();
        }

        setForeground(Color.black);

        JPanel toolbarPanel = new JPanel(new GridLayout(1, 7, 0, 0));
        toolbarPanel.setBorder(BorderFactory.createEtchedBorder());

        JToolBar ctlToolBar = new JToolBar();
        ctlToolBar.setBorder(BorderFactory.createEtchedBorder());
        ctlToolBar.setBorderPainted(true);
        ctlToolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);

        JButton linearButton = new JButton(MipavUtil.getIcon("linear.gif"));

        linearButton.addActionListener(this);
        linearButton.setRolloverIcon(MipavUtil.getIcon("linearroll.gif"));
        linearButton.setBorderPainted(false);
        linearButton.setToolTipText("Reset transfer function");
        linearButton.setActionCommand("resetLinear");
        ctlToolBar.add(linearButton);

        JButton linearBackSlashButton = new JButton(MipavUtil.getIcon("linearbackslash.gif"));

        linearBackSlashButton.addActionListener(this);
        linearBackSlashButton.setRolloverIcon(MipavUtil.getIcon("linearbackslashroll.gif"));
        linearBackSlashButton.setBorderPainted(false);
        linearBackSlashButton.setToolTipText("Reset transfer function");
        linearBackSlashButton.setActionCommand("resetLinearBackSlash");
        ctlToolBar.add(linearBackSlashButton);

        JButton horizonButton = new JButton(MipavUtil.getIcon("linearhorizon.gif"));

        horizonButton.addActionListener(this);
        horizonButton.setRolloverIcon(MipavUtil.getIcon("linearhorizonroll.gif"));
        horizonButton.setBorderPainted(false);
        horizonButton.setToolTipText("Reset transfer function");
        horizonButton.setActionCommand("resetHorizon");
        ctlToolBar.add(horizonButton);

        JButton separator = new JButton(MipavUtil.getIcon("separator.gif"));

        separator.setBorderPainted(false);
        separator.setFocusPainted(false);
        ctlToolBar.add(separator);

        GMCheckBox = new JCheckBox("Gradient Map", false);
        GMCheckBox.setFont(MipavUtil.font12);
        GMCheckBox.addItemListener(this);

        ctlToolBar.add(GMCheckBox);
        toolbarPanel.add(ctlToolBar, BorderLayout.WEST);

        tabbedPane = new JTabbedPane();
        tabbedPane.addChangeListener(this);
        tabbedPane.setFont(MipavUtil.font12B);

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
        subPanel.add(toolbarPanel, gbConstraints);

        gbConstraints.gridy = 1;
        gbConstraints.fill = GridBagConstraints.BOTH;
        subPanel.add(tabbedPane, gbConstraints);

        gbConstraints.gridy = 2;
        gbConstraints.weighty = 1;
        gbConstraints.fill = GridBagConstraints.HORIZONTAL;
        subPanel.add(blendPanel, gbConstraints);
        
        JScrollPane scrollPaneA = new JScrollPane(subPanel);
        scrollPaneA.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);

        mainPanel.setLayout(new GridLayout(1, 1));
        mainPanel.add(scrollPaneA);
    }
}
