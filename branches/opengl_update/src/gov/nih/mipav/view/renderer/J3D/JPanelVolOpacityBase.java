package gov.nih.mipav.view.renderer.J3D;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.*;
import gov.nih.mipav.view.renderer.WildMagic.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;

import javax.swing.*;
import javax.swing.event.*;


/**
 * Simple panel containing the volume renderer opacity controls.
 *
 * @version  0.1 August, 2005
 */
public abstract class JPanelVolOpacityBase extends JPanelRendererJ3D implements ChangeListener, MouseListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5320201609864903620L;

    /** Component tag indicator. */
    public static final int COMP_A = 0, COMP_B = 1, COMP_GM_A = 2, COMP_GM_B = 3;

    /** DOCUMENT ME! */
    protected static final String OPACITY_COMPONENT_TAB_A = "ImgA";

    /** DOCUMENT ME! */
    protected static final String OPACITY_COMPONENT_TAB_B = "ImgB";

    /** DOCUMENT ME! */
    protected static final String OPACITY_COMPONENT_TAB_A_GM = "ImgA_GM";

    /** DOCUMENT ME! */
    protected static final String OPACITY_COMPONENT_TAB_B_GM = "ImgB_GM";

    /** Component tag indicator. */
    public static final int RED = 1, GREEN = 2, BLUE = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Opacity slider of texture 3D volume opacity changes. */
    protected JSlider blendSlider;
    
    /** Reference to compoment opacity control A. */
    protected ViewJComponentVolOpacityBase componentOpacityA;

    /** Reference to compoment opacity control B. */
    protected ViewJComponentVolOpacityBase componentOpacityB;

    /** Reference to compoment opacity control A GM Rescale. */
    protected ViewJComponentVolOpacityBase componentOpacityGM_A;

    /** Reference to compoment opacity control B GM Rescale. */
    protected ViewJComponentVolOpacityBase componentOpacityGM_B;

    /** Gradient Magnitude Check box of the grey scale image. */
    protected JCheckBox GMCheckBox;

    /** Model image of the gradient magnitude of image A rescaled to have value in the range [0:255]. */
    protected ModelImage gradMagRescale_A;

    /** Model image of the gradient magnitude of image B rescaled to have value in the range [0:255]. */
    protected ModelImage gradMagRescale_B;

    /** Model image A. */
    protected ModelImage imageA;

    /** Model image B. */
    protected ModelImage imageB;

    /** Historgram dialog slider labels of the imageA, B and GM imageA, B. */
    protected Hashtable labelsTable, labelsTableB, labelsTableGM_A, labelsTableGM_B;

    /** DOCUMENT ME! */
    protected float middleLabelValueA;

    /** DOCUMENT ME! */
    protected float middleLabelValueB;

    /** DOCUMENT ME! */
    protected float middleLabelValueGM_A;

    /** DOCUMENT ME! */
    protected float middleLabelValueGM_B;

    /** DOCUMENT ME! */
    protected JSlider mouseSlider, mouseSliderB;

    /** DOCUMENT ME! */
    protected JSlider mouseSliderGM_A, mouseSliderGM_B;

    /** The labels below the opacity slider. */
    protected JLabel[] opacitySliderLabelsA, opacitySliderLabelsB;

    /** DOCUMENT ME! */
    protected JLabel[] opacitySliderLabelsGM_A, opacitySliderLabelsGM_B;

    /** Panels that hold the the control components (opacity maps). */
    protected JPanel panelOpacityA;

    /** DOCUMENT ME! */
    protected JPanel panelOpacityB;

    /** DOCUMENT ME! */
    protected JPanel panelOpacityGM_A;

    /** DOCUMENT ME! */
    protected JPanel panelOpacityGM_B;

    /** X range text field in the imageA, B histogram dialog. */
    protected JTextField rangeText, rangeTextB;

    /** X range text field in the Gradient Magnitude imageA, B histogram dialog. */
    protected JTextField rangeTextGM_A, rangeTextGM_B;

    /** Scale range value according to the image min and max. */
    protected float scaleRangeA, scaleRangeB, scaleRangeGM_A, scaleRangeGM_B;

    /** Tabbed pane that contains the list of opacity functions. */
    protected JTabbedPane tabbedPane;

    /**
     * X range text field in the imageA, B and GM image A, B histogram dialog. Following text fields are used by the
     * tri-planar volume view.
     */
    protected JTextField xRangeTextA, xRangeTextB, xRangeTextGM_A, xRangeTextGM_B;

    /**
     * Y range text field in the imageA, B and GM image A, B histogram dialog. Following text fields are used by the
     * tri-planar volume view.
     */
    protected JTextField yRangeTextA, yRangeTextB, yRangeTextGM_A, yRangeTextGM_B;
    

    /** Render base. */
    protected VolumeTriPlanarInterface m_kVolumeViewer = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Base for the opacity objects.
     *
     * @param  theParentFrame  RenderViewBase
     */
    public JPanelVolOpacityBase(RenderViewBase theParentFrame) {
        super(theParentFrame);
    }

    /**
     * Base for the opacity objects.
     *
     */
    public JPanelVolOpacityBase(VolumeTriPlanarInterface kVolumeViewer) {
        super(null);
        m_kVolumeViewer = kVolumeViewer;
    }


    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Action performed method required when extending JPanelRendererBase.
     *
     * @param  event  DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent event) { }

    /**
     * Clear memory and garbage collection.
     */
    public void disposeLocal() {

        imageA = null;
        imageB = null;

        if (gradMagRescale_A != null) {
            gradMagRescale_A.disposeLocal();
            gradMagRescale_A = null;
        }

        if (gradMagRescale_B != null) {
            gradMagRescale_B.disposeLocal();
            gradMagRescale_B = null;
        }
    }

    public abstract void addGM();
    public abstract void removeGM();
    
    /**
     * This function returns the value of the blending slider.
     *
     * @return  int the value of the slider, or a default of 50 (the median value) if the slider has not been
     *          initialized
     */
    public int getAlphaBlendSliderValue() {

        if (blendSlider == null) {
            return 50; // 50 is the halfway point of the slider (0 min, 100 max)

            // if it hasn't been initialized yet, assume half opacity
        }

        return blendSlider.getValue();
    }

    /**
     * Sets the blendSlider value.
     * @param iValue new slider value.
     */
    public void setAlphaBlendSliderValue(int iValue) {

        if (blendSlider != null) {
            blendSlider.setValue(iValue);
        }
    }
    
    /**
     * Return the opacity histrogram component of the imageA.
     *
     * @return  componentOpacityA opacity component A.
     */
    public ViewJComponentVolOpacityBase getCompA() {
        return componentOpacityA;
    }

    /**
     * Return the opacity histrogram component of the Gradient Magnitude imageA.
     *
     * @return  componentOpacityGM_A gradient magnitude component A.
     */
    public ViewJComponentVolOpacityBase getCompA_GM() {
        return componentOpacityGM_A;
    }

    /**
     * Return the opacity histrogram component of the imageB.
     *
     * @return  componentOpacityB opacity component B.
     */
    public ViewJComponentVolOpacityBase getCompB() {
        return componentOpacityB;
    }

    /**
     * Return the opacity histrogram component of the Gradient Magnitude imageB.
     *
     * @return  componentOpacityGM_B gradient magnitude component B.
     */
    public ViewJComponentVolOpacityBase getCompB_GM() {
        return componentOpacityGM_B;
    }

    /**
     * Get the gradient magnitude imageA.
     *
     * @return  ModelImage GM imageA
     */
    public ModelImage getGradMagA() {
        return null;
    }

    /**
     * Get the gradient magnitude imageB.
     *
     * @return  ModelImage GM imageB
     */
    public ModelImage getGradMagB() {
        return null;
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
     * Accessor that returns the image.
     *
     * @return  image
     */
    public ModelImage getImageB() {
        return imageB;
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen. Change the layout of the volume opacity
     * change diagram.
     *
     * @return  DOCUMENT ME!
     */
    public JPanel getMainPanel() {
        return mainPanel;
    }

    /**
     * Return the parent frame.
     *
     * @return  myParent ViewJFrameSurfaceRender frame
     */
    public RenderViewBase getParentFrame() {

        // This instance is stored in the base class.
        return renderBase;
    }
    

    /**
     */
    public VolumeTriPlanarInterface getParentVolumeViewer() {

        // This instance is stored in the base class.
        return m_kVolumeViewer;
    }

    /**
     * Return the selected tabbbed pane component. Should be overridden in JPanelVolOpacity and JPanelVolOpacityRGB
     *
     * @return  ViewJComponentHLUTBase the opacity component.
     */
    public ViewJComponentVolOpacityBase getSelectedComponent() {
        int selectedTab = tabbedPane.getSelectedIndex();

        if (tabbedPane.getTitleAt(selectedTab).equals(OPACITY_COMPONENT_TAB_A)) {
            return componentOpacityA;
        } else if (tabbedPane.getTitleAt(selectedTab).equals(OPACITY_COMPONENT_TAB_B)) {
            return componentOpacityB;
        } else if (tabbedPane.getTitleAt(selectedTab).equals(OPACITY_COMPONENT_TAB_A_GM)) {
            return componentOpacityGM_A;
        } else if (tabbedPane.getTitleAt(selectedTab).equals(OPACITY_COMPONENT_TAB_B_GM)) {
            return componentOpacityGM_B;
        }

        return null;
    }
    
    public int getSelectedTabIndex()
    {
        return tabbedPane.getSelectedIndex();
    }
    
    public void setSelectedTabIndex(int i)
    {
        tabbedPane.setSelectedIndex(i);
    }

    /**
     * Return the active image component.
     *
     * @param   _whichComp  true = imageA, false = imageB
     *
     * @return  componentOpacity A or B.
     */
    public ViewJComponentVolOpacityBase getSelectedComponent(int _whichComp) {

        if (_whichComp == COMP_A) {
            return componentOpacityA;
        } else if (_whichComp == COMP_B) {
            return componentOpacityB;
        } else if (_whichComp == COMP_GM_A) {
            return componentOpacityGM_A;
        } else if (_whichComp == COMP_GM_B) {
            return componentOpacityGM_B;
        }

        return null;
    }

    /**
     * Accessor to the tabbed pane.
     *
     * @return  JTabbedPane
     */
    public JTabbedPane getTabbedPane() {
        return tabbedPane;
    }

    /**
     * Return whether or not the opacity mapping based on gradient magnitude is enabled.
     *
     * @return  boolean True if using gradient magnitude-based opacity mapping.
     */
    public boolean isGradientMagnitudeOpacityEnabled() {

        if (null != GMCheckBox) {
            return GMCheckBox.isSelected();
        } 
        return false;
    }
    
    /**
     * Return whether or not the opacity mapping based on gradient magnitude is enabled.
     *
     * @return  boolean True if using gradient magnitude-based opacity mapping.
     */
    public void setGradientMagnitudeOpacityEnabled(boolean value) {

        if (null != GMCheckBox) {
            GMCheckBox.setSelected(value); 
            if (GMCheckBox.isSelected() == true) {
                addGM();
            } else {
                removeGM();
            }
        } 
    }

    /**
     * Not used method.
     *
     * @param  event  MouseEvent
     */
    public void mouseClicked(MouseEvent event) { }

    /**
     * Not used method.
     *
     * @param  event  MouseEvent
     */
    public void mouseEntered(MouseEvent event) { }

    /**
     * Not used method.
     *
     * @param  event  MouseEvent
     */
    public void mouseExited(MouseEvent event) { }

    /**
     * Not used method.
     *
     * @param  event  MouseEvent
     */
    public void mousePressed(MouseEvent event) { }

    /**
     * Mouse release event.
     *
     * @param  event  MouseEvent
     */
    public void mouseReleased(MouseEvent event) {

        if (((event.getSource() == blendSlider) && (blendSlider.isEnabled() == true))) {
            int selectedTab = tabbedPane.getSelectedIndex();

            if ((imageA != null) &&
                    (tabbedPane.getTitleAt(selectedTab).equals(OPACITY_COMPONENT_TAB_A) ||
                         tabbedPane.getTitleAt(selectedTab).equals(OPACITY_COMPONENT_TAB_A_GM))) {
                imageA.notifyImageDisplayListeners();
            }

            if ((imageB != null) &&
                    (tabbedPane.getTitleAt(selectedTab).equals(OPACITY_COMPONENT_TAB_B) ||
                         tabbedPane.getTitleAt(selectedTab).equals(OPACITY_COMPONENT_TAB_B_GM))) {
                imageB.notifyImageDisplayListeners();
            }
        }
    }

    /**
     * This method will enable or disable the opacity panel's slider component and input text fields, based on the value
     * of the boolean parameter passed into it.
     *
     * @param  enabled  boolean whether the adjusters should be enabled or disabled
     */
    public void setAdjustersEnabled(boolean enabled) {

        if (mouseSlider != null) {
            mouseSlider.setEnabled(enabled);
        }

        if (xRangeTextA != null) {
            xRangeTextA.setEnabled(enabled);
        }

        if (mouseSliderB != null) {
            mouseSliderB.setEnabled(enabled);
        }

        if (xRangeTextB != null) {
            xRangeTextB.setEnabled(enabled);
        }

        if (mouseSliderGM_A != null) {
            mouseSliderGM_A.setEnabled(enabled);
        }

        if (xRangeTextGM_A != null) {
            xRangeTextGM_A.setEnabled(enabled);
        }

        if (mouseSliderGM_B != null) {
            mouseSliderGM_B.setEnabled(enabled);
        }

        if (xRangeTextGM_B != null) {
            xRangeTextGM_B.setEnabled(enabled);
        }
    }

    /**
     * ChangeListener for opacity slider changes.
     *
     * @param  event  The change event.
     */
    public void stateChanged(ChangeEvent event) {

        if (event.getSource() == mouseSlider) {

            if (mouseSlider.getValueIsAdjusting() == true) {
                return;
            }

            float upperLabel = Float.parseFloat(opacitySliderLabelsA[2].getText());
            float lowerLabel = Float.parseFloat(opacitySliderLabelsA[0].getText());

            scaleRangeA = (upperLabel - lowerLabel) / 2;

            float deltaValue;
            float sliderValue = mouseSlider.getValue();

            if (sliderValue > 50) {
                deltaValue = (sliderValue - 50) / 100.0f * scaleRangeA * 2.0f;
            } else {
                deltaValue = -(50 - sliderValue) / 100.0f * scaleRangeA * 2.0f;
            }

            componentOpacityA.updateCursorXPos(middleLabelValueA + deltaValue);
        } else if (event.getSource() == mouseSliderB) {

            if (mouseSliderB.getValueIsAdjusting() == true) {
                return;
            }

            float upperLabel = Float.parseFloat(opacitySliderLabelsB[2].getText());
            float lowerLabel = Float.parseFloat(opacitySliderLabelsB[0].getText());

            scaleRangeB = (upperLabel - lowerLabel) / 2;

            float deltaValue;
            float sliderValue = mouseSliderB.getValue();

            if (sliderValue > 50) {
                deltaValue = (sliderValue - 50) / 100.0f * scaleRangeB * 2.0f;
            } else {
                deltaValue = -(50 - sliderValue) / 100.0f * scaleRangeB * 2.0f;
            }

            componentOpacityB.updateCursorXPos(middleLabelValueB + deltaValue);

        } else if (event.getSource() == mouseSliderGM_A) {

            if (mouseSliderGM_A.getValueIsAdjusting() == true) {
                return;
            }

            float upperLabel = Float.parseFloat(opacitySliderLabelsGM_A[2].getText());
            float lowerLabel = Float.parseFloat(opacitySliderLabelsGM_A[0].getText());

            scaleRangeGM_A = (upperLabel - lowerLabel) / 2;

            float deltaValue;
            float sliderValue = mouseSliderGM_A.getValue();

            if (sliderValue > 50) {
                deltaValue = (sliderValue - 50) / 100.0f * scaleRangeGM_A * 2.0f;
            } else {
                deltaValue = -(50 - sliderValue) / 100.0f * scaleRangeGM_A * 2.0f;
            }

            componentOpacityGM_A.updateCursorXPos(middleLabelValueGM_A + deltaValue);
        } else if (event.getSource() == mouseSliderGM_B) {

            if (mouseSliderGM_B.getValueIsAdjusting() == true) {
                return;
            }

            float upperLabel = Float.parseFloat(opacitySliderLabelsGM_B[2].getText());
            float lowerLabel = Float.parseFloat(opacitySliderLabelsGM_B[0].getText());

            scaleRangeGM_B = (upperLabel - lowerLabel) / 2;

            float deltaValue;
            float sliderValue = mouseSliderGM_B.getValue();

            if (sliderValue > 50) {
                deltaValue = (sliderValue - 50) / 100.0f * scaleRangeGM_B * 2.0f;
            } else {
                deltaValue = -(50 - sliderValue) / 100.0f * scaleRangeGM_B * 2.0f;
            }

            componentOpacityGM_B.updateCursorXPos(middleLabelValueGM_B + deltaValue);
        }
        else if ( event.getSource() == blendSlider )
        {
            if ( renderBase instanceof gov.nih.mipav.view.renderer.J3D.surfaceview.SurfaceRender )
            {
            }   
            else if ( m_kVolumeViewer != null )
            {
                m_kVolumeViewer.updateABBlend( );
            }
        }
    }

    /**
     * Adjust the opacity slider for the minor opacity changes.
     *
     * @param  childComponent  ViewJComponentVolOpacityBase reference
     */
    public void updateSlider(ViewJComponentVolOpacityBase childComponent) {

        try {

            if (childComponent == componentOpacityA) {
                mouseSlider.setLabelTable(getLabelTableA());
                mouseSlider.removeChangeListener(this);
                mouseSlider.setValue(50);
                mouseSlider.addChangeListener(this);

                middleLabelValueA = Float.parseFloat(opacitySliderLabelsA[1].getText());
            } else if (childComponent == componentOpacityB) {
                mouseSliderB.setLabelTable(getLabelTableB());
                mouseSliderB.removeChangeListener(this);
                mouseSliderB.setValue(50);
                mouseSliderB.addChangeListener(this);

                middleLabelValueB = Float.parseFloat(opacitySliderLabelsB[1].getText());

            } else if (childComponent == componentOpacityGM_A) {
                mouseSliderGM_A.setLabelTable(getLabelTableGM_A());
                mouseSliderGM_A.removeChangeListener(this);
                mouseSliderGM_A.setValue(50);
                mouseSliderGM_A.addChangeListener(this);

                middleLabelValueGM_A = Float.parseFloat(opacitySliderLabelsGM_A[1].getText());

            } else if (childComponent == componentOpacityGM_B) {
                mouseSliderGM_B.setLabelTable(getLabelTableGM_B());
                mouseSliderGM_B.removeChangeListener(this);
                mouseSliderGM_B.setValue(50);
                mouseSliderGM_B.addChangeListener(this);

                middleLabelValueGM_B = Float.parseFloat(opacitySliderLabelsGM_B[1].getText());

            }
        } catch (Exception e) { } // do nothing, just return
    }

    /**
     * Build the blend slider control panel.
     *
     * @return  blendPanel built blend panel.
     */
    protected JPanel buildBlendPanel() {
        Hashtable labels = new Hashtable();

        labels.put(new Integer(0), new JLabel("Image A"));
        labels.put(new Integer(25), new JLabel("0.75A"));
        labels.put(new Integer(50), new JLabel("0.5A/B"));
        labels.put(new Integer(75), new JLabel("0.75B"));
        labels.put(new Integer(100), new JLabel("Image B"));

        blendSlider = new JSlider(0, 100, 50);
        blendSlider.setFont(serif12);
        blendSlider.setMinorTickSpacing(25);
        blendSlider.setPaintTicks(true);
        blendSlider.setLabelTable(labels);
        blendSlider.setPaintLabels(true);
        blendSlider.addMouseListener(this);
        blendSlider.addChangeListener(this);
        blendSlider.setEnabled(imageB != null);

        JPanel blendPanel = new JPanel(new GridLayout(1, 1));

        blendPanel.add(blendSlider);
        blendPanel.setBorder(buildTitledBorder("Blending"));

        return blendPanel;
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
     * Build the hash table for the imageA opacity slider.
     *
     * @return  labels built label hash table.
     */
    protected Hashtable getLabelTableA() {

        if (opacitySliderLabelsA == null) {
            initBlendSliderLabels();
        }

        int tfActiveIndex = componentOpacityA.getActiveIndex();

        if (tfActiveIndex == componentOpacityA.INACTIVE) {
            opacitySliderLabelsA[0].setText(" ");
            opacitySliderLabelsA[1].setText(" ");
            opacitySliderLabelsA[2].setText(" ");
        } else {
            TransferFunction tf = componentOpacityA.getOpacityTransferFunction();

            opacitySliderLabelsA[0].setText(MipavUtil.makeFloatString((tf.getPoint(tfActiveIndex).X - 1), 1));
            opacitySliderLabelsA[1].setText(MipavUtil.makeFloatString((tf.getPoint(tfActiveIndex).X), 1));
            opacitySliderLabelsA[2].setText(MipavUtil.makeFloatString((tf.getPoint(tfActiveIndex).X + 1), 1));
        }

        Hashtable labels = new Hashtable();

        // the slider has a range of 0-100. the float values that are used as the text of the labels
        // get cut off if they are positioned at 0, 50, 100, so for the end two labels, they are
        // positioned at 3 and 97 to in-set them from the edges and not get cut off
        labels.put(new Integer(3), opacitySliderLabelsA[0]);
        labels.put(new Integer(50), opacitySliderLabelsA[1]);
        labels.put(new Integer(97), opacitySliderLabelsA[2]);

        return labels;
    }

    /**
     * Build the hash table for the imageB opacity slider.
     *
     * @return  labels built label hash table.
     */
    protected Hashtable getLabelTableB() {

        if (opacitySliderLabelsB == null) {
            initBlendSliderLabels();
        }

        int tfActiveIndex = componentOpacityB.getActiveIndex();

        if (tfActiveIndex == componentOpacityB.INACTIVE) {
            opacitySliderLabelsB[0].setText(" ");
            opacitySliderLabelsB[1].setText(" ");
            opacitySliderLabelsB[2].setText(" ");
        } else {
            TransferFunction tf = componentOpacityB.getOpacityTransferFunction();

            opacitySliderLabelsB[0].setText(MipavUtil.makeFloatString((tf.getPoint(tfActiveIndex).X - 5), 1)); // 5 is the range of the slider labels
            opacitySliderLabelsB[1].setText(MipavUtil.makeFloatString((tf.getPoint(tfActiveIndex).X), 1));
            opacitySliderLabelsB[2].setText(MipavUtil.makeFloatString((tf.getPoint(tfActiveIndex).X + 5), 1));
        }

        Hashtable labels = new Hashtable();

        // the slider has a range of 0-100. the float values that are used as the text of the labels
        // get cut off if they are positioned at 0, 50, 100, so for the end two labels, they are
        // positioned at 3 and 97 to in-set them from the edges and not get cut off
        labels.put(new Integer(3), opacitySliderLabelsB[0]);
        labels.put(new Integer(50), opacitySliderLabelsB[1]);
        labels.put(new Integer(97), opacitySliderLabelsB[2]);

        return labels;
    }

    /**
     * Build the hash table for the imageA GM opacity slider.
     *
     * @return  labels built label hash table.
     */
    protected Hashtable getLabelTableGM_A() {

        if (opacitySliderLabelsGM_A == null) {
            initBlendSliderLabels();
        }

        int tfActiveIndex = componentOpacityGM_A.getActiveIndex();

        if (tfActiveIndex == componentOpacityGM_A.INACTIVE) {
            opacitySliderLabelsGM_A[0].setText(" ");
            opacitySliderLabelsGM_A[1].setText(" ");
            opacitySliderLabelsGM_A[2].setText(" ");
        } else {
            TransferFunction tf = componentOpacityGM_A.getOpacityTransferFunction();

            opacitySliderLabelsGM_A[0].setText(MipavUtil.makeFloatString((tf.getPoint(tfActiveIndex).X - 5), 1)); // 5 is the range of the slider labels
            opacitySliderLabelsGM_A[1].setText(MipavUtil.makeFloatString((tf.getPoint(tfActiveIndex).X), 1));
            opacitySliderLabelsGM_A[2].setText(MipavUtil.makeFloatString((tf.getPoint(tfActiveIndex).X + 5), 1));
        }

        Hashtable labels = new Hashtable();

        // the slider has a range of 0-100. the float values that are used as the text of the labels
        // get cut off if they are positioned at 0, 50, 100, so for the end two labels, they are
        // positioned at 3 and 97 to in-set them from the edges and not get cut off
        labels.put(new Integer(3), opacitySliderLabelsGM_A[0]);
        labels.put(new Integer(50), opacitySliderLabelsGM_A[1]);
        labels.put(new Integer(97), opacitySliderLabelsGM_A[2]);

        return labels;
    }

    /**
     * Build the hash table for the imageA GM opacity slider.
     *
     * @return  labels built label hash table.
     */
    protected Hashtable getLabelTableGM_B() {

        if (opacitySliderLabelsGM_B == null) {
            initBlendSliderLabels();
        }

        int tfActiveIndex = componentOpacityGM_B.getActiveIndex();

        if (tfActiveIndex == componentOpacityGM_B.INACTIVE) {
            opacitySliderLabelsGM_B[0].setText(" ");
            opacitySliderLabelsGM_B[1].setText(" ");
            opacitySliderLabelsGM_B[2].setText(" ");
        } else {
            TransferFunction tf = componentOpacityGM_B.getOpacityTransferFunction();

            opacitySliderLabelsGM_B[0].setText(MipavUtil.makeFloatString((tf.getPoint(tfActiveIndex).X - 5), 1)); // 5 is the range of the slider labels
            opacitySliderLabelsGM_B[1].setText(MipavUtil.makeFloatString((tf.getPoint(tfActiveIndex).X), 1));
            opacitySliderLabelsGM_B[2].setText(MipavUtil.makeFloatString((tf.getPoint(tfActiveIndex).X + 5), 1));
        }

        Hashtable labels = new Hashtable();

        // the slider has a range of 0-100. the float values that are used as the text of the labels
        // get cut off if they are positioned at 0, 50, 100, so for the end two labels, they are
        // positioned at 3 and 97 to in-set them from the edges and not get cut off
        labels.put(new Integer(3), opacitySliderLabelsGM_B[0]);
        labels.put(new Integer(50), opacitySliderLabelsGM_B[1]);
        labels.put(new Integer(97), opacitySliderLabelsGM_B[2]);

        return labels;
    }

    /**
     * Initialize the blend slider labels.
     */
    protected void initBlendSliderLabels() {

        if (opacitySliderLabelsA == null) {
            opacitySliderLabelsA = new JLabel[3];
        }

        opacitySliderLabelsA[0] = new JLabel(" ");
        opacitySliderLabelsA[1] = new JLabel(" ");
        opacitySliderLabelsA[2] = new JLabel(" ");
        opacitySliderLabelsA[0].setFont(MipavUtil.font12);
        opacitySliderLabelsA[1].setFont(MipavUtil.font12);
        opacitySliderLabelsA[2].setFont(MipavUtil.font12);

        if (opacitySliderLabelsB == null) {
            opacitySliderLabelsB = new JLabel[3];
        }

        opacitySliderLabelsB[0] = new JLabel(" ");
        opacitySliderLabelsB[1] = new JLabel(" ");
        opacitySliderLabelsB[2] = new JLabel(" ");
        opacitySliderLabelsB[0].setFont(MipavUtil.font12);
        opacitySliderLabelsB[1].setFont(MipavUtil.font12);
        opacitySliderLabelsB[2].setFont(MipavUtil.font12);

        if (opacitySliderLabelsGM_A == null) {
            opacitySliderLabelsGM_A = new JLabel[3];
        }

        opacitySliderLabelsGM_A[0] = new JLabel(" ");
        opacitySliderLabelsGM_A[1] = new JLabel(" ");
        opacitySliderLabelsGM_A[2] = new JLabel(" ");
        opacitySliderLabelsGM_A[0].setFont(MipavUtil.font12);
        opacitySliderLabelsGM_A[1].setFont(MipavUtil.font12);
        opacitySliderLabelsGM_A[2].setFont(MipavUtil.font12);

        if (opacitySliderLabelsGM_B == null) {
            opacitySliderLabelsGM_B = new JLabel[3];
        }

        opacitySliderLabelsGM_B[0] = new JLabel(" ");
        opacitySliderLabelsGM_B[1] = new JLabel(" ");
        opacitySliderLabelsGM_B[2] = new JLabel(" ");
        opacitySliderLabelsGM_B[0].setFont(MipavUtil.font12);
        opacitySliderLabelsGM_B[1].setFont(MipavUtil.font12);
        opacitySliderLabelsGM_B[2].setFont(MipavUtil.font12);
    }
    
    /**
     * Loads the gradient magnitude image instead of recalculating the image.
     *
     * @param   dName     String User specified directory name.
     * @param   fName     String GM image file name.
     * @param   isImageA  boolean Indicates GM imageA or GM imageB
     *
     * @return  boolean Indicates loading GM image successful or not.
     */
    protected boolean loadGMImage(String dName, String fName, boolean isImageA) {
        FileIO fileIO = new FileIO();

        fileIO.setQuiet(true);

        if (new File(dName + File.separator + fName).exists()) {

            if (isImageA) {
                gradMagRescale_A.disposeLocal();
                gradMagRescale_A = null;
                gradMagRescale_A = fileIO.readImage(fName, dName, false, null, false);
            } else { // is ImageB
                gradMagRescale_B.disposeLocal();
                gradMagRescale_B = null;
                gradMagRescale_B = fileIO.readImage(fName, dName, false, null, false);
            }

            return true;
        }

        return false;
    }
    
    public void update()
    {
        /*
    	if ( renderBase instanceof gov.nih.mipav.view.renderer.J3D.surfaceview.SurfaceRender )
        {
             ((gov.nih.mipav.view.renderer.J3D.surfaceview.SurfaceRender)renderBase).updateParent();
        }
        else */
        if ( m_kVolumeViewer != null )
        {
            m_kVolumeViewer.updateImages(true);
        }
    }
}
