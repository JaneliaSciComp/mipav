package gov.nih.mipav.view.dialogs;

import java.awt.*;
import java.awt.event.*;


import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. The user has the option to generate a new image or replace the
 * source image. In addition the user can indicate if you wishes to have the algorithm applied to whole image or to the
 * VOI regions. In should be noted, that the algorithms are executed in their own thread.
 *
 */
public class JDialogMask3D4D extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7339956453691640420L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Radio button indicating that the image should only be masked in 3D */
    private JRadioButton threeD;

    /** Radio button indicating that the image should  be masked in 4D */
    private JRadioButton fourD;

    private boolean saveMasksAs4D = false;


    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogMask3D4D object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogMask3D4D(Frame theParentFrame) {
        super(theParentFrame, true);
        init();
    }


    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {
            if (threeD.isSelected()) {
            	saveMasksAs4D = false;
            } else if (fourD.isSelected()) {
            	saveMasksAs4D = true;
            }
            dispose();
        } else if (command.equals("Cancel")) {
            dispose();
        } else {
            super.actionPerformed(event);
        }
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Mask");

        ButtonGroup destinationGroup = new ButtonGroup();
        threeD = new JRadioButton("3D", true);
        threeD.setFont(serif12);
        destinationGroup.add(threeD);

        fourD = new JRadioButton("4D", false);
        fourD.setFont(serif12);
        destinationGroup.add(fourD);

        GridBagConstraints gbc = new GridBagConstraints();

        JPanel destinationPanel = new JPanel(new GridBagLayout());
        destinationPanel.setBorder(buildTitledBorder("Options"));

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        destinationPanel.add(threeD, gbc);
        gbc.gridy = 1;
        destinationPanel.add(fourD, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        //buildCancelButton();
        //buttonPanel.add(cancelButton);

        destinationPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        getContentPane().add(destinationPanel, BorderLayout.CENTER);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }


	public boolean isSaveMasksAs4D() {
		return saveMasksAs4D;
	}



}
