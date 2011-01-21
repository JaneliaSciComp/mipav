package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Simple dialog to change desired intensity level for painting.
 *
 * @author  Neva Cherniavsky
 */

public class JDialogIntensityPaint extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -736880761566398344L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JLabel inputLabel;

    /** DOCUMENT ME! */
    private JTextField inputText;

    /** DOCUMENT ME! */
    private double intensityDouble;

    /** DOCUMENT ME! */
    private String intensityStringValue = "0";

    /** DOCUMENT ME! */
    private double outMax;

    /** DOCUMENT ME! */
    private double outMin;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog for getting the intensity of the paint.
     *
     * @param  theParentFrame  Parent frame
     * @param  name            Current intensity string
     * @param  type            Image type.
     */
    public JDialogIntensityPaint(Frame theParentFrame, String name, int type) {
        super(theParentFrame, true);
        init(name, type);
    }

    /**
     * Creates new dialog for getting the intensity of the paint.
     *
     * @param  theParentFrame  Parent frame
     * @param  name            Current intensity string
     * @param  type            Image type.
     * @param  isVisible       Dialog visible or not.
     */
    public JDialogIntensityPaint(Frame theParentFrame, String name, int type, boolean isVisible) {
        super(theParentFrame, true);
        // getMaintPanel(name, type);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // **************************** Action Events *****************************
    // ************************************************************************

    /**
     * Tests if entered intensity is within bounds and saves it.
     *
     * @param  event  Event that triggered function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {
            String intensityText = inputText.getText();

            if (testParameter(intensityText, outMin, outMax)) {
                intensityDouble = Double.valueOf(intensityText).doubleValue();
            } else {
                inputText.requestFocus();
                inputText.selectAll();

                return;
            }

            intensityStringValue = intensityText;
            paint(getGraphics());
            dispose();
        } else if (command.equals("Cancel")) {
            cancelFlag = true;
            dispose();
        }
    }

    /**
     * Accessor that returns current intensity value to commit.
     *
     * @return  Intensity value.
     */
    public double getIntensity() {
        return intensityDouble;
    }


    /**
     * Accessor that returns current intensity value to commit in the form of a string.
     *
     * @return  Intensity value.
     */
    public String getIntensityValue() {
        return intensityStringValue;
    }

    /**
     * Initializes GUI components and displays dialog.
     *
     * @param   name  Current intensity string
     * @param   type  Image type.
     *
     * @return  DOCUMENT ME!
     */
    public JPanel getMainPanel(String name, int type) {
        setForeground(Color.black);
        setTitle("Desired Paint Intensity");

        try {
            inputLabel = new JLabel();
            inputText = new JTextField(10);
            buildOKButton();

            // buildCancelButton();
            OKButton.setText("Apply");
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory");

            return null;
        }

        if (type == 0) {
            inputLabel.setText("Intensity (0 - 1): ");
            outMin = 0;
            outMax = 1;
        } else if (type == 1) {
            inputLabel.setText("Intensity (-128 - 127): ");
            outMin = -128;
            outMax = 127;
        } else if (type == 2) {
            inputLabel.setText("Intensity (0 - 255): ");
            outMin = 0;
            outMax = 255;
        } else if (type == 3) {
            inputLabel.setText("Intensity (-32768 - 32767): ");
            outMin = -32768;
            outMax = 32767;
        } else if (type == 4) {
            inputLabel.setText("Intensity (0 - 65535): ");
            outMin = 0;
            outMax = 65767;
        } else if (type == 5) {
            inputLabel.setText("Intensity (-2.147 E+9 - 2.147 E+9): ");
            outMin = Integer.MIN_VALUE;
            outMax = Integer.MAX_VALUE;
        } else if (type == 6) {
            inputLabel.setText("Intensity (-9.22 E+18 - 9.22 E+18): ");
            outMin = Long.MIN_VALUE;
            outMax = Long.MAX_VALUE;

        } else if (type == 7) {
            inputLabel.setText("Intensity (-3.40 E+38 - 3.40 E+38): ");
            outMin = Float.NEGATIVE_INFINITY;

            // outMin = Float.MIN_VALUE;
            outMax = Float.MAX_VALUE;
        } else if (type == 8) {
            inputLabel.setText("Intensity (-1.80 E+308 - 1.80 E+308): ");
            outMin = Double.NEGATIVE_INFINITY;
            outMax = Double.MAX_VALUE;
        }

        inputLabel.setFont(serif12);
        inputLabel.setForeground(Color.black);

        inputText.setFont(serif12);
        inputText.setText(name);
        inputText.setForeground(Color.black);

        OKButton.setActionCommand("OK");

        JPanel mainPanel = new JPanel(new BorderLayout());

        mainPanel.add(inputLabel, BorderLayout.NORTH);
        mainPanel.add(inputText, BorderLayout.CENTER);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(OKButton);
        // buttonPanel.add(cancelButton);

        JPanel panel = new JPanel();
        panel.add(mainPanel);
        panel.add(buttonPanel, BorderLayout.SOUTH);

        return panel;
    }

    /**
     * Initializes GUI components and displays dialog.
     *
     * @param  name  Current intensity string
     * @param  type  Image type.
     */
    private void init(String name, int type) {
        setForeground(Color.black);
        setTitle("Desired Paint Intensity");

        try {
            inputLabel = new JLabel();
            inputText = new JTextField(10);
            buildOKButton();
            buildCancelButton();
            OKButton.setText("Apply");
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory");

            return;
        }

        if (type == 0) {
            inputLabel.setText("Intensity (0 - 1): ");
            outMin = 0;
            outMax = 1;
        } else if (type == 1) {
            inputLabel.setText("Intensity (-128 - 127): ");
            outMin = -128;
            outMax = 127;
        } else if (type == 2) {
            inputLabel.setText("Intensity (0 - 255): ");
            outMin = 0;
            outMax = 255;
        } else if (type == 3) {
            inputLabel.setText("Intensity (-32768 - 32767): ");
            outMin = -32768;
            outMax = 32767;
        } else if (type == 4) {
            inputLabel.setText("Intensity (0 - 65535): ");
            outMin = 0;
            outMax = 65767;
        } else if (type == 5) {
            inputLabel.setText("Intensity (-2.147 E+9 - 2.147 E+9): ");
            outMin = Integer.MIN_VALUE;
            outMax = Integer.MAX_VALUE;
        } else if (type == 6) {
            inputLabel.setText("Intensity (-9.22 E+18 - 9.22 E+18): ");
            outMin = Long.MIN_VALUE;
            outMax = Long.MAX_VALUE;

        } else if (type == 7) {
            inputLabel.setText("Intensity (-3.40 E+38 - 3.40 E+38): ");
            outMin = Float.NEGATIVE_INFINITY;

            // outMin = Float.MIN_VALUE;
            outMax = Float.MAX_VALUE;
        } else if (type == 8) {
            inputLabel.setText("Intensity (-1.80 E+308 - 1.80 E+308): ");
            outMin = Double.NEGATIVE_INFINITY;
            outMax = Double.MAX_VALUE;
        }

        inputLabel.setFont(serif12);
        inputLabel.setForeground(Color.black);

        inputText.setFont(serif12);
        inputText.setText(name);
        inputText.setForeground(Color.black);

        OKButton.setActionCommand("OK");

        JPanel mainPanel = new JPanel();
        mainPanel.add(inputLabel);
        mainPanel.add(inputText);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }
}
