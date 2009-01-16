package gov.nih.mipav.view.dialogs;


import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Simple dialog to change the number of colors in the histogram LUT.
 *
 * @author   Neva Cherniavsky
 * @version  1.0 June 1, 2002
 * @see      ViewJFrameHistoLUT
 */
public class JDialogNColors extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2280005601447558064L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JTextField field;

    /** DOCUMENT ME! */
    private int nColors;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates modal dialog for entering number of colors for histogram LUT.
     *
     * @param  parent  Parent frame.
     */
    public JDialogNColors(Frame parent) {
        super(parent, true);
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Tests then sets the number of colors when the "OK" button is pressed. Sets the cancel flag and disposes when the
     * "Cancel" button is pressed.
     *
     * @param  event  Event that triggered this function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("Apply")) {
            String colorText = field.getText();

            if (testParameter(colorText, 2, 256)) {
                nColors = Integer.valueOf(colorText).intValue();
            } else {
                field.requestFocus();
                field.selectAll();

                return;
            }
        } else if (command.equals("Cancel")) {
            cancelFlag = true;
        }

        dispose();
    }

    /**
     * Accessor to get the number of colors.
     *
     * @return  The number of colors.
     */
    public int getNColors() {
        return nColors;
    }

    /**
     * Initializes GUI components and adds them to the dialog.
     */
    private void init() {
        setTitle("Change Number of Colors");

        JLabel label = new JLabel("Number of Colors (2-256)");
        label.setFont(serif12);
        label.setForeground(Color.black);

        field = new JTextField("256", 5);
        field.setFont(serif12);

        JPanel panel = new JPanel();
        panel.add(label);
        panel.add(field);
        panel.setBorder(buildTitledBorder("Change Number of Colors"));

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        OKButton.setText("Apply");
        buildCancelButton();
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);

        getContentPane().add(panel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }

}
