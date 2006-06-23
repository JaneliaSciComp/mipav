package gov.nih.mipav.view.renderer.surfaceview;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to get the three parameters needed for surface smoothing, iterations, positive scale factor lambda, and
 * negative scale factor mu. Requires -mu > lambda > 0 Requires (1/lambda) + (1/mu) < 2
 */
public class JPanelSurfaceSmooth3 extends JPanelRendererBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 6435657183936591544L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Number of iterations. */
    private int iterations;

    /** Text field for getting iterations. */
    private JTextField iterationsText;

    /** positive scale factor. */
    private float lambda;

    /** Text field for getting lambda for smoothing. */
    private JTextField lambdaText;

    /** negative scale factor. */
    private float mu;


    /** DOCUMENT ME! */
    private JTextField muText;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog to get iterations, lambda, and mu for smoothing a mesh surface.
     *
     * @param  parent  Parent frame.
     */
    public JPanelSurfaceSmooth3(SurfaceRender parent) {
        super(parent);
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Sets iterations, lambda, and mu when "OK" is pressed; disposes dialog when "Cancel" is pressed.
     *
     * @param  e  Event that triggered function.
     */
    public void actionPerformed(ActionEvent e) {
        Object source = e.getSource();
        String command = e.getActionCommand();

        if (source == cancelButton) {
            cancelFlag = true;
            setVisible(false);
        } else if ((source == OKButton) || command.equals("Smooth3")) {

            if (testParameter(iterationsText.getText(), 1, 10000)) {
                iterations = Integer.valueOf(iterationsText.getText()).intValue();
            } else {
                iterationsText.requestFocus();
                iterationsText.selectAll();

                return;
            }

            if (testParameter(lambdaText.getText(), 0.1, 1.0)) {
                lambda = Float.valueOf(lambdaText.getText()).floatValue();
            } else {
                lambdaText.requestFocus();
                lambdaText.selectAll();

                return;
            }

            if (testParameter(muText.getText(), -1.1f * lambda, -lambda - 0.001f)) {
                mu = Float.valueOf(muText.getText()).floatValue();
            } else {
                muText.requestFocus();
                muText.selectAll();

                return;
            }

            if (((1 / mu) + (1 / lambda)) >= 2.0) {
                MipavUtil.displayError("Require (1/mu) + (1/lambda) < 2.0");
                muText.requestFocus();
                muText.selectAll();

                return;
            }

            // dispose();
            setVisible(false);
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
     * Accessor that returns the positive scale factor lambda.
     *
     * @return  lambda .
     */
    public float getLambda() {
        return lambda;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public JPanel getMainPanel() {
        return mainPanel;
    }


    /**
     * Accessor that returns the negative scale factor mu.
     *
     * @return  mu
     */
    public float getMu() {
        return mu;
    }

    /**
     * Initializes GUI components and displays dialog.
     */
    private void init() {

        // setTitle("Smoothing options");
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

        JLabel lambdaLabel = new JLabel(" lambda( > 0.0): ");
        lambdaLabel.setFont(serif12);
        lambdaLabel.setForeground(Color.black);
        optionsPanel.add(lambdaLabel);

        lambdaText = new JTextField(5);
        lambdaText.setFont(serif12);
        lambdaText.setText("0.33");
        optionsPanel.add(lambdaText);

        JLabel muLabel = new JLabel(" mu(1.1*lambda >= -mu > lambda): ");
        muLabel.setFont(serif12);
        muLabel.setForeground(Color.black);
        optionsPanel.add(muLabel);


        muText = new JTextField(7);
        muText.setFont(serif12);
        muText.setText("-0.34");
        optionsPanel.add(muText);

        mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(optionsPanel);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel buttonPanel = new JPanel();
        OKButton = buildOKButton();
        cancelButton = buildCancelButton();
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);

        // getContentPane().add(mainPanel);
        // getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        // pack();
        // setVisible(true);
    }

}
