package gov.nih.mipav.view.dialogs;


import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Confirmation Dialog giving user the choice to choose what type of NIFTI file to write.
 *
 * @author   not attributable
 * @version  1.0
 */
public class JDialogNIFTIChoice extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 5469246588202892297L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean okayPressed = false;

    /** DOCUMENT ME! */
    private JRadioButton oneFile;

    /** DOCUMENT ME! */
    private JRadioButton twoFiles;


    /** DOCUMENT ME! */
    private ButtonGroup writeGroup;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame of dialog.
     */
    public JDialogNIFTIChoice(Frame theParentFrame) {
        super(theParentFrame, true);
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Checks to see if the OK or Cancel buttons were pressed.
     *
     * @param  event  Event that triggered this function.
     */
    public void actionPerformed(ActionEvent event) {

        if (event.getSource() == OKButton) {
            okayPressed = true;
            dispose();
        } else {
            super.actionPerformed(event);
        }

        
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean getOneFile() {

        if (oneFile.isSelected()) {
            return true;
        } else {
            return false;
        }
    }


    /**
     * Was the okay button pressed.
     *
     * @return  boolean was okay pressed
     */
    public boolean okayPressed() {
        return okayPressed;
    }

    /**
     * Creates and displays dialog.
     */
    private void init() {
        setTitle("Choose type of NIFTI file");

        JPanel createPanel = new JPanel(new GridBagLayout());
        createPanel.setBorder(buildTitledBorder("Write file as"));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(0, 20, 0, 0);

        writeGroup = new ButtonGroup();
        oneFile = new JRadioButton("Header and data in one .nii file", true);
        oneFile.setFont(serif12);
        oneFile.setForeground(Color.black);
        writeGroup.add(oneFile);
        oneFile.setEnabled(true);
        createPanel.add(oneFile, gbc);

        gbc.gridy = 1;
        twoFiles = new JRadioButton("Header in .hdr file and image in .img file", false);
        twoFiles.setFont(serif12);
        twoFiles.setForeground(Color.black);
        writeGroup.add(twoFiles);
        twoFiles.setEnabled(true);
        createPanel.add(twoFiles, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);

        mainDialogPanel.add(createPanel);
        mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

        pack();
        setVisible(true);
    }
}
