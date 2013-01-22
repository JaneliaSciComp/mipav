package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * User-interface for smoothing the ModelTriangleMesh surfaces. When the
 * JDialogSmoothMesh object is created the type of smooth dialog is
 * specified. The smooth type may be SMOOTH1, SMOOTH2, or SMOOTH3. These
 * correspond to the different smooth operations available for the
 * ModelTriangleMesh object.
 *
 * The smooth type variable determines which components of the user-interface
 * are displayed -- whichever components correspond to the parameters of the
 * ModelTriangleMesh smooth operations.
 *
 * SMOOTH1 -- corresponds to ModelTriangleMesh.smoothMesh function. The
 * interface components displayed are the Number of Iterations, Alpha, Volume
 * Limit, and the Volume Percent Change.
 *
 * SMOOTH2 -- corresponds to ModelTriangleMesh.smoothTwo function. The
 * interface components displayed are the Number of Iterations, Stiffness,
 * Volume Limit, and Volume Percent Change.
 * 
 * SMOOTH3 -- corresponds to ModelTriangleMesh.smoothThree function. The
 * interface components displayed are the Number of Iterations, Lambda, and
 * Mu.
 *
 * @see ModelTriangleMesh.java
 * @see JPanelSurfaces.java
 */
public class JDialogSmoothMesh extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -4644401872827443236L;

    /** Smooth type: SMOOTH1 corresponds to ModelTriangleMesh.smoothMesh() */
    public static final int SMOOTH1 = 1;
    /** Smooth type: SMOOTH2 corresponds to ModelTriangleMesh.smoothTwo() */
    public static final int SMOOTH2 = 2;
    /** Smooth type: SMOOTH3 corresponds to ModelTriangleMesh.smoothThree() */
    public static final int SMOOTH3 = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    // Fields that are smooth-type dependent:
    /** Alpha smoothing factor. */
    private float alpha;

    /** Text field for getting alpha smoothing factor. */
    private JTextField alphaText;

    /** stiffness. */
    private float stiffness;

    /** Text field for getting stiffness for smoothing. */
    private JTextField stiffnessText;

    /** positive scale factor. */
    private float lambda;

    /** Text field for getting lambda for smoothing. */
    private JTextField lambdaText;

    /** negative scale factor. */
    private float mu;

    /** Volume text field. */
    private JTextField muText;

    // Fields that are smooth-type independent:
    /** main panel containing interface: */
    JPanel mainPanel = null;

    /** Number of iterations for smoothing formula. */
    private int iterations;

    /** Text field for getting iterations. */
    private JTextField iterationsText;

    /**
     * If limitCheckBox is selected iterations stop when volume change from
     * initial volume is greater than or equal to volumePercent.
     */
    private JCheckBox limitCheckBox;

    /** flag indicates volume is greater than or equal to volumePercent. */
    private boolean volumeLimit;

    /** Volume percentage. */
    private float volumePercent = 3.0f;

    /** Volume text field. */
    private JTextField volumeText;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog to get iterations and alpha for smoothing a mesh surface.
     *
     * @param  parent  Parent frame.
     * @param show when true display the interface in a dialog.
     * @param type smooth level (1, 2, 3)
     */
    public JDialogSmoothMesh( Frame parent, boolean show, int type )
    {
        super(parent, true);
        init( show, type );
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Sets smoothing varaibles when "OK" is pressed; enables the volumeText
     * box when the limitCheckBox is pressed, disposes dialog when "Cancel" is
     * pressed.
     *
     * @param  e  Event that triggered function.
     */
    public void actionPerformed(ActionEvent e) {
        Object source = e.getSource();

        /* Cancel and dispose: */
        if (source == cancelButton) {
            cancelFlag = true;
            dispose();
        }
        /* Enable volumeText: */
        else if (source == limitCheckBox) {
            if (limitCheckBox.isSelected()) {
                volumeText.setEnabled(true);
            } else {
                volumeText.setEnabled(false);
            }
        } 
        /* set variables and dispose: */
        else if (source == OKButton) {
            setVariables();
            dispose();
        } else {
            super.actionPerformed(e);
        }
    }

    /**
     * returns the mainPanel so this interface can be displayed inside another
     * panel.
     *
     * @return mainPanel, the main panel containing the user-interface for
     * this object.
     */
    public JPanel getMainPanel() {
        return mainPanel;
    }

    /**
     * Accessor that returns the alpha smoothing factor.
     *
     * @return  Alpha smoothing factor.
     */
    public float getAlpha() {
        return alpha;
    }

    /**
     * Accessor that returns the stiffness.
     *
     * @return  stiffness.
     */
    public float getStiffness() {
        return stiffness;
    }

    /**
     * Accessor that returns the positive scale factor lambda.
     *
     * @return  lambda.
     */
    public float getLambda() {
        return lambda;
    }


    /**
     * Accessor that returns the negative scale factor mu.
     *
     * @return  mu.
     */
    public float getMu() {
        return mu;
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
     * Accessor that returns whether or not iterations are stopped after the
     * present volume is different from the initial volume by volumePercent or
     * more.
     *
     * @return  volumeLimit.
     */
    public boolean getVolumeLimit() {
        return volumeLimit;
    }

    /**
     * Accessor that returns the percentage difference from the initial volume
     * at which iterations stop.
     *
     * @return  volumePercent.
     */
    public float getVolumePercent() {
        return volumePercent;
    }

    /**
     * sets the variables based on user-input.
     */
    public void setVariables()
    {
        if (testParameter(alphaText.getText(), 0, 1)) {
            alpha = Float.valueOf(alphaText.getText()).floatValue();
        } else {
            alphaText.requestFocus();
            alphaText.selectAll();
            
            return;
        }
        if (testParameter(stiffnessText.getText(), 0.0, 0.8)) {
            stiffness = Float.valueOf(stiffnessText.getText()).floatValue();
        } else {
            stiffnessText.requestFocus();
            stiffnessText.selectAll();
            
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
        
        
        if (testParameter(iterationsText.getText(), 0, 10000)) {
            iterations = Integer.valueOf(iterationsText.getText()).intValue();
        } else {
            iterationsText.requestFocus();
            iterationsText.selectAll();
            
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
    }


    /**
     * Initializes GUI components based on the type parameter.. When show
     * parameter is true the interface is displayed as a separate dialog box.
     * @param show when true display the interface as a separate dialog box.
     * @param type the type of smooth dialog to create (SMOOTH1, SMOOTH2,
     * SMOOTH3). The type will determine which components of the
     * user-interface are displayed to match the ModelTriangleMesh smooth type
     * operation parameters.
     */
    private void init( boolean show, int type )
    {
        setTitle("Smoothing options");

        JPanel optionsPanel = new JPanel(new GridLayout(3, 2));
        optionsPanel.setBorder(buildTitledBorder("Parameters"));

        JLabel iterationsLabel = new JLabel(" Number of iterations: ");
        iterationsLabel.setFont(serif12);
        iterationsLabel.setForeground(Color.black);
        optionsPanel.add(iterationsLabel);

        iterationsText = new JTextField(5);
        iterationsText.setFont(serif12);
        iterationsText.setText("50");
        optionsPanel.add(iterationsText);

        /* Smooth-type dependant fields, not that optionsPanel.add is called based on type: */
        JLabel alphaLabel = new JLabel(" Smoothing factor: ");
        alphaLabel.setFont(serif12);
        alphaLabel.setForeground(Color.black);
        alphaText = new JTextField(5);
        alphaText.setFont(serif12);
        alphaText.setText(".05");
        if ( type == SMOOTH1 )
        {
            optionsPanel.add(alphaLabel);
            optionsPanel.add(alphaText);
        }

        JLabel stiffnessLabel = new JLabel(" Stiffness(0.0 - 0.8): ");
        stiffnessLabel.setFont(serif12);
        stiffnessLabel.setForeground(Color.black);

        stiffnessText = new JTextField(5);
        stiffnessText.setFont(serif12);
        stiffnessText.setText("0.2");
        if ( type == SMOOTH2 )
        {
            optionsPanel.add(stiffnessLabel);
            optionsPanel.add(stiffnessText);
        }

        JLabel lambdaLabel = new JLabel(" lambda( > 0.0): ");
        lambdaLabel.setFont(serif12);
        lambdaLabel.setForeground(Color.black);
        lambdaText = new JTextField(5);
        lambdaText.setFont(serif12);
        lambdaText.setText("0.33");
        JLabel muLabel = new JLabel(" mu(1.1*lambda >= -mu > lambda): ");
        muLabel.setFont(serif12);
        muLabel.setForeground(Color.black);
        muText = new JTextField(7);
        muText.setFont(serif12);
        muText.setText("-0.34");
        if ( type == SMOOTH3 )
        {
            optionsPanel.add(lambdaLabel);
            optionsPanel.add(lambdaText);
            optionsPanel.add(muLabel);
            optionsPanel.add(muText);
        }

        limitCheckBox = new JCheckBox("Volume % change limit:");
        limitCheckBox.setFont(serif12);
        limitCheckBox.setForeground(Color.black);
        limitCheckBox.setSelected(false);
        limitCheckBox.addActionListener(this);
        volumeText = new JTextField(7);
        volumeText.setFont(serif12);
        volumeText.setText("3.0");
        volumeText.setEnabled(false);
        if ( type != SMOOTH3 )
        {
            optionsPanel.add(limitCheckBox);
            optionsPanel.add(volumeText);
        }
        /* End Smooth-type dependant fields */

        mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(optionsPanel);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        if ( show )
        {
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
}
