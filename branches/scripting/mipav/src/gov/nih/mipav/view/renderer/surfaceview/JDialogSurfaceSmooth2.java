package gov.nih.mipav.view.renderer.surfaceview;


import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to get the two parameters needed for surface smoothing, initial and the stiffnessIters.
 */
public class JDialogSurfaceSmooth2 extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -4316340713091503370L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Number of iterations. */
    private int iterations;

    /** Text field for getting iterations. */
    private JTextField iterationsText;

    /**
     * If limitCheckBox is selected iterations stop when volume change from initial volume is greater than or equal to
     * volumePercent.
     */
    private JCheckBox limitCheckBox;

    /** stiffness. */
    private float stiffness;

    /** Text field for getting stiffness for smoothing. */
    private JTextField stiffnessText;

    /** flag indicates volume is greater than or equal to volumePercent. */
    private boolean volumeLimit;

    /** Volume percentage. */
    private float volumePercent = 3.0f;

    /** Volume text field. */
    private JTextField volumeText;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog to get iterations and stiffness for smoothing a mesh surface.
     *
     * @param  parent  Parent frame.
     */
    public JDialogSurfaceSmooth2(Frame parent) {
        super(parent, true);
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Sets iterations and stiffness when "OK" is pressed; disposes dialog when "Cancel" is pressed.
     *
     * @param  e  Event that triggered function.
     */
    public void actionPerformed(ActionEvent e) {
        Object source = e.getSource();

        if (source == cancelButton) {
            cancelFlag = true;
            dispose();
        } else if (source == limitCheckBox) {

            if (limitCheckBox.isSelected()) {
                volumeText.setEnabled(true);
            } else {
                volumeText.setEnabled(false);
            }
        } // else if (source == limitCheckBox)
        else if (source == OKButton) {

            if (testParameter(iterationsText.getText(), 1, 10000)) {
                iterations = Integer.valueOf(iterationsText.getText()).intValue();
            } else {
                iterationsText.requestFocus();
                iterationsText.selectAll();

                return;
            }

            if (testParameter(stiffnessText.getText(), 0.0, 0.8)) {
                stiffness = Float.valueOf(stiffnessText.getText()).floatValue();
            } else {
                stiffnessText.requestFocus();
                stiffnessText.selectAll();

                return;
            }

            if (limitCheckBox.isSelected()) {
                volumeLimit = true;

                if (testParameter(volumeText.getText(), 0.0, 100.0)) {
                    volumePercent = Float.valueOf(volumeText.getText()).floatValue();
                } else {
                    volumeText.requestFocus();
                    volumeText.selectAll();

                    return;
                }
            } else {
                volumeLimit = false;
            }

            dispose();
        }
    }

    /**
     * Accessor that returns the number of iterations.
     *
     * @return  Number of iterations.
     */
    public int getIterations() {
        return iterations;
    }

    /**
     * Accessor that returns the stiffness.
     *
     * @return  stiffness .
     */
    public float getStiffness() {
        return stiffness;
    }

    /**
     * Accessor that returns whether or not iterations are stopped after the present volume is different from the
     * initial volume by volumePercent or more.
     *
     * @return  volumeLimit
     */
    public boolean getVolumeLimit() {
        return volumeLimit;
    }

    /**
     * Accessor that returns the percentage difference from the initial volume at which iterations stop.
     *
     * @return  volumePercent
     */
    public float getVolumePercent() {
        return volumePercent;
    }

    /**
     * Initializes GUI components and displays dialog.
     */
    private void init() {
        setTitle("Smoothing options");

        JPanel optionsPanel = new JPanel(new GridLayout(3, 2));
        optionsPanel.setBorder(buildTitledBorder("Parameters"));

        JLabel iterationsLabel = new JLabel(" Iterations: ");
        iterationsLabel.setFont(serif12);
        iterationsLabel.setForeground(Color.black);
        optionsPanel.add(iterationsLabel);

        iterationsText = new JTextField(5);
        iterationsText.setFont(serif12);
        iterationsText.setText("50");
        optionsPanel.add(iterationsText);

        JLabel stiffnessLabel = new JLabel(" Stiffness(0.0 - 0.8): ");
        stiffnessLabel.setFont(serif12);
        stiffnessLabel.setForeground(Color.black);
        optionsPanel.add(stiffnessLabel);

        stiffnessText = new JTextField(5);
        stiffnessText.setFont(serif12);
        stiffnessText.setText("0.2");
        optionsPanel.add(stiffnessText);

        limitCheckBox = new JCheckBox("Volume % change limit:");
        limitCheckBox.setFont(serif12);
        limitCheckBox.setForeground(Color.black);
        limitCheckBox.setSelected(false);
        limitCheckBox.addActionListener(this);
        optionsPanel.add(limitCheckBox);

        volumeText = new JTextField(7);
        volumeText.setFont(serif12);
        volumeText.setText("3.0");
        volumeText.setEnabled(false);
        optionsPanel.add(volumeText);

        JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(optionsPanel);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel buttonPanel = new JPanel();
        OKButton = buildOKButton();
        cancelButton = buildCancelButton();
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }

}
