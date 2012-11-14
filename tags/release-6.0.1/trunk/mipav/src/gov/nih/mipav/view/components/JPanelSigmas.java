package gov.nih.mipav.view.components;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.event.*;

import javax.swing.*;


/**
 * A panel containing fields which allow the user to input sigmas in three dimensions (of, for example, a gaussian
 * kernel). Also optionally normalizes the third dimension based on the image resolution.
 *
 * @author  mccreedy
 */
public class JPanelSigmas extends JPanel implements FocusListener, ItemListener {

    //~ Static fields/initializers ---------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4032897701053956108L;

    //~ Instance fields --------------------------------------------------------------------------------

    /** gui */
    private JCheckBox correctionFactorCheckbox;

    /** label */
    private JLabel correctionFactorLabel;

    /** label */
    private JLabel labelGaussZ;

    /** the input image which will be processed */
    private ModelImage srcImage;

    /** gui, sigma value */
    private JTextField textGaussX;

    /** gui, sigma value */
    private JTextField textGaussY;

    /** gui, sigma value */
    private JTextField textGaussZ;

    //~ Constructors -----------------------------------------------------------------------------------

    /**
     * Construct the sigma panel.
     *
     * @param  img  the input image which will be processed
     */
    public JPanelSigmas(ModelImage img) {
        srcImage = img;

        initGUI();
    }

    //~ Methods ----------------------------------------------------------------------------------------

    /**
     * Enables or disables the components in this panel which are 3D-related.
     *
     * @param  enable3D  whether to enable the 3D components
     */
    public void enable3DComponents(boolean enable3D) {
        if (correctionFactorLabel != null) {
            correctionFactorLabel.setEnabled(enable3D);
        }
        if (correctionFactorCheckbox != null) {
            correctionFactorCheckbox.setEnabled(enable3D);
        }
        labelGaussZ.setEnabled(enable3D);
        textGaussZ.setEnabled(enable3D);
    }

    /**
     * Enables or disables z-dimension sigma resolution correction.
     *
     * @param  enableCorrection  whether to enable the correction
     */
    public void enableResolutionCorrection(boolean enableCorrection) {
        correctionFactorCheckbox.setSelected(enableCorrection);
    }

    /**
     * Do nothing.
     *
     * @param  event  focus gained event
     */
    public void focusGained(FocusEvent event) { }

    /**
     * When the user clicks the mouse out of a text field, resets the neccessary variables.
     *
     * @param  event  focus lost event
     */
    public void focusLost(FocusEvent event) {
        Object source = event.getSource();

        if (source == textGaussZ) {
            updateResolutionCorrection();
        }
    }

    /**
     * Returns the sigmas to use in the algorithm, with the z-dim normalized if the user has requested it.
     *
     * @return  a 2 sigma array if the image is 2D, 3 sigmas if the input image is 3D (with the 3rd dim optionally
     *          normalized)
     */
    public float[] getNormalizedSigmas() {

        if (srcImage.getNDims() == 2) {
            return new float[] { Float.parseFloat(textGaussX.getText()), Float.parseFloat(textGaussY.getText()) };
        } else {

            if (isResolutionCorrectionEnabled()) {
                return new float[] {
                           Float.parseFloat(textGaussX.getText()), Float.parseFloat(textGaussY.getText()),
                           Float.parseFloat(textGaussZ.getText()) * getCorrectionFactor()
                       };
            } else {
                return new float[] {
                           Float.parseFloat(textGaussX.getText()), Float.parseFloat(textGaussY.getText()),
                           Float.parseFloat(textGaussZ.getText())
                       };
            }
        }
    }

    /**
     * Returns the un-normalized sigmas in three dimensions (even if the input image is 2D). This is used to record the
     * values in the text fields when saving dialog defaults and during script recording.
     *
     * @return  a 3 element array of un-normalized floating-point sigmas
     */
    public float[] getUnnormalized3DSigmas() {
        return new float[] {
                   Float.parseFloat(textGaussX.getText()), Float.parseFloat(textGaussY.getText()),
                   Float.parseFloat(textGaussZ.getText())
               };
    }

    /**
     * Returns whether the z-dimension sigma should be corrected based on the image resolution.
     *
     * @return  true if the z-dimension sigma should be corrected
     */
    public boolean isResolutionCorrectionEnabled() {
        return correctionFactorCheckbox.isSelected();
    }

    /**
     * Changes resolution normalization label based on whether or not the resolution normalization check box is checked.
     *
     * @param  event  checkbox item change event
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();

        if (source == correctionFactorCheckbox) {
            updateResolutionCorrection();
        }
    }

    /**
     * Changes the sigma value in the x direction.
     *
     * @param  sigma  the new sigma
     */
    public void setSigmaX(float sigma) {
        textGaussX.setText("" + sigma);
    }

    /**
     * Changes the sigma value in the y direction.
     *
     * @param  sigma  the new sigma
     */
    public void setSigmaY(float sigma) {
        textGaussY.setText("" + sigma);
    }

    /**
     * Changes the un-normalized sigma value in the z direction.
     *
     * @param  sigma  the new sigma
     */
    public void setSigmaZ(float sigma) {
        textGaussZ.setText("" + sigma);
    }

    /**
     * Tests the un-normalized sigma values against pre-determined okay values.
     *
     * @return  true if all the sigmas are in a good range, false otherwise
     */
    public boolean testSigmaValues() {

        if (!MipavUtil.testParameter(textGaussX.getText(), 0.0, 10.0)) {
            textGaussX.requestFocus();
            textGaussX.selectAll();

            return false;
        }

        if (!MipavUtil.testParameter(textGaussY.getText(), 0.0, 10.0)) {
            textGaussY.requestFocus();
            textGaussY.selectAll();

            return false;
        }

        if (!MipavUtil.testParameter(textGaussZ.getText(), 0.0, 10.0)) {
            textGaussZ.requestFocus();
            textGaussZ.selectAll();

            return false;
        }

        return true;
    }

    /**
     * Returns the amount of correction which should be applied to the z-direction sigma (assuming that correction is
     * requested).
     *
     * @return  the amount to multiply the z-sigma by to correct for resolution differences
     */
    private float getCorrectionFactor() {
        int index = srcImage.getExtents()[2] / 2;
        float xRes = srcImage.getFileInfo(index).getResolutions()[0];
        float zRes = srcImage.getFileInfo(index).getResolutions()[2];

        return xRes / zRes;
    }

    /**
     * Returns a string indicating the z-direction sigma after applying the resolution correction factor.
     *
     * @return  resolution corrected z-sigma string
     */
    private String getCorrectionFactorString() {
        return MipavUtil.makeFloatString(getCorrectionFactor() * Float.parseFloat(textGaussZ.getText()), 4);
    }
    
    /**
     * Set the name of the titled border
     * @param borderName
     */
    public void setBorderName(String borderName) {
        setBorder(WidgetFactory.buildTitledBorder(borderName));
    }

    /**
     * Initialize the panel's GUI.
     */
    private void initGUI() {
        PanelManager scalePanelManager = new PanelManager(this);
        setBorder(WidgetFactory.buildTitledBorder("Scale of the Gaussian"));

        textGaussX = WidgetFactory.buildTextField("1.0");
        textGaussX.setColumns(5);
        textGaussY = WidgetFactory.buildTextField("1.0");
        textGaussY.setColumns(5);
        labelGaussZ = WidgetFactory.buildLabel("Z dimension (0.0 - 10.0) ");
        textGaussZ = WidgetFactory.buildTextField("1.0");
        textGaussZ.setColumns(5);
        correctionFactorCheckbox = WidgetFactory.buildCheckBox("Use image resolutions to normalize Z scale", true);

        scalePanelManager.add(WidgetFactory.buildLabel("X dimension (0.0 - 10.0) "));
        scalePanelManager.add(textGaussX);
        scalePanelManager.addOnNextLine(WidgetFactory.buildLabel("Y dimension (0.0 - 10.0) "));
        scalePanelManager.add(textGaussY);
        scalePanelManager.addOnNextLine(labelGaussZ);
        scalePanelManager.add(textGaussZ);
        scalePanelManager.addOnNextLine(correctionFactorCheckbox);

        if (srcImage.getNDims() >= 3) { // if the source image is 3D then allow
            correctionFactorCheckbox.setEnabled(true); // the user to indicate if it wishes to
            correctionFactorCheckbox.addItemListener(this); // use the correction factor
            textGaussZ.addFocusListener(this);
            textGaussZ.setEnabled(true);

            correctionFactorLabel = WidgetFactory.buildLabel("      Corrected scale = " + getCorrectionFactorString());
            scalePanelManager.addOnNextLine(correctionFactorLabel);
        } else {
            correctionFactorCheckbox.setEnabled(false);
            labelGaussZ.setEnabled(false);
            textGaussZ.setEnabled(false);
        }
    }

    /**
     * Updates the resolution-corrected z-direction sigma label.
     */
    private void updateResolutionCorrection() {

        if (correctionFactorCheckbox.isSelected()) {
            correctionFactorLabel.setText("      Corrected scale = " + getCorrectionFactorString());
        } else {
            correctionFactorLabel.setText(" ");
        }
    }
}
