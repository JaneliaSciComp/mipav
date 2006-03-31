package gov.nih.mipav.view.renderer.surfaceview;

import gov.nih.mipav.view.dialogs.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;

/**
*   Dialog to get the two parameters needed for
*   surface smoothing, the iterations and the alpha.
*/
public class JDialogSurfaceSmooth extends JDialogBase {

    /** Text field for getting iterations. */
    private JTextField iterationsText;

    /** Number of iterations for smoothing formula. */
    private int        iterations;

    /** Text field for getting alpha smoothing factor. */
    private JTextField alphaText;

    /** Alpha smoothing factor. */
    private float      alpha;

    /* If limitCheckBox is selected iterations stop when volume change from initial
       volume is greater than or equal to volumePercent */
    private JCheckBox limitCheckBox;

    /** flag indicates volume is greater than or equal to volumePercent. */
    private boolean volumeLimit;

    /** Volume text field. */
    private JTextField volumeText;

    /** Volume percentage. */
    private float volumePercent = 3.0f;

    /**
    *   Creates new dialog to get iterations and alpha for
    *   smoothing a mesh surface.
    *   @param parent   Parent frame.
    */
    public JDialogSurfaceSmooth(Frame parent) {
        super(parent, true);
        init();
    }

    /**
    *   Initializes GUI components and displays dialog.
    */
    private void init() {
        setTitle("Smoothing options");
        JPanel optionsPanel = new JPanel(new GridLayout(3,2));
        optionsPanel.setBorder(buildTitledBorder("Parameters"));

        JLabel iterationsLabel = new JLabel(" Number of iterations: ");
        iterationsLabel.setFont(serif12);
        iterationsLabel.setForeground(Color.black);
        optionsPanel.add(iterationsLabel);

        iterationsText = new JTextField(5);
        iterationsText.setFont(serif12);
        iterationsText.setText("50");
        optionsPanel.add(iterationsText);

        JLabel alphaLabel = new JLabel(" Smoothing factor: ");
        alphaLabel.setFont(serif12);
        alphaLabel.setForeground(Color.black);
        optionsPanel.add(alphaLabel);

        alphaText = new JTextField(5);
        alphaText.setFont(serif12);
        alphaText.setText(".05");
        optionsPanel.add(alphaText);

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
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5,5,5,5));

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

    /**
    *   Sets iterations and alpha when "OK" is pressed;
    *   disposes dialog when "Cancel" is pressed.
    *   @param e    Event that triggered function.
    */
    public void actionPerformed(ActionEvent e) {
        Object source = e.getSource();

        if (source == cancelButton) {
            cancelFlag = true;
            dispose();
        }
        else if (source == limitCheckBox) {
            if (limitCheckBox.isSelected()) {
                volumeText.setEnabled(true);
            }
            else {
                volumeText.setEnabled(false);
            }
        } // else if (source == limitCheckBox)
        else if (source == OKButton) {
            if (testParameter(iterationsText.getText(), 0, 10000)) {
                iterations = Integer.valueOf(iterationsText.getText()).intValue();
            }
            else {
                iterationsText.requestFocus();
                iterationsText.selectAll();
                return;
            }
            if (testParameter(alphaText.getText(), 0, 1)) {
                alpha = Float.valueOf(alphaText.getText()).floatValue();
            }
            else {
                alphaText.requestFocus();
                alphaText.selectAll();
                return;
            }
            if (limitCheckBox.isSelected()) {
                volumeLimit = true;
                if (testParameter(volumeText.getText(),0.0,100.0)) {
                    volumePercent = Float.valueOf(volumeText.getText()).floatValue();
                }
                else {
                    volumeText.requestFocus();
                    volumeText.selectAll();
                    return;
                }
            }
            else {
                volumeLimit = false;
            }
            dispose();
        }
    }

    /**
    *   Accessor that returns the number of iterations.
    *   @return Number of iterations.
    */
    public int   getIterations(){ return iterations; }

    /**
    *   Accessor that returns the alpha smoothing factor.
    *   @return Alpha smoothing factor.
    */
    public float getAlpha()     { return alpha; }

    /**
    *   Accessor that returns whether or not iterations are stopped
    *   after the present volume is different from the initial volume
    *   by volumePercent or more
    *   @return volumeLimit
    */
    public boolean getVolumeLimit() {return volumeLimit; }

    /**
    *   Accessor that returns the percentage difference from the
    *   initial volume at which iterations stop
    *   @return volumePercent
    */
    public float getVolumePercent() {return volumePercent; }

}
