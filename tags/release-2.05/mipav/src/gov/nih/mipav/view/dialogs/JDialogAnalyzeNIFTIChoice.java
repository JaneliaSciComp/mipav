package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;

import com.sun.media.codec.video.vcm.*;

import java.awt.*;
import java.awt.event.*;

import javax.media.*;
import javax.media.format.*;

import javax.swing.*;


/**
 * Confirmation Dialog giving user the choice to write an analyze file or a nifti file.
 *
 * @author   not attributable
 * @version  1.0
 */
public class JDialogAnalyzeNIFTIChoice extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4588130080855618027L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JRadioButton analyzeFile;

    /** DOCUMENT ME! */
    private JRadioButton niftiFile;

    /** DOCUMENT ME! */
    private boolean okayPressed = false;


    /** DOCUMENT ME! */
    private ButtonGroup writeGroup;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame of dialog.
     */
    public JDialogAnalyzeNIFTIChoice(Frame theParentFrame) {
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
        }

        dispose();
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean isAnalyzeFile() {

        if (analyzeFile.isSelected()) {
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
        setTitle("Choose type of file to write");

        JPanel createPanel = new JPanel(new GridBagLayout());
        createPanel.setBorder(buildTitledBorder("Write file as"));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = gbc.WEST;
        gbc.insets = new Insets(0, 20, 0, 0);

        writeGroup = new ButtonGroup();
        analyzeFile = new JRadioButton("Analyze file", true);
        analyzeFile.setFont(serif12);
        analyzeFile.setForeground(Color.black);
        writeGroup.add(analyzeFile);
        analyzeFile.setEnabled(true);
        createPanel.add(analyzeFile, gbc);

        gbc.gridy = 1;
        niftiFile = new JRadioButton("NIFTI file", false);
        niftiFile.setFont(serif12);
        niftiFile.setForeground(Color.black);
        writeGroup.add(niftiFile);
        niftiFile.setEnabled(true);
        createPanel.add(niftiFile, gbc);

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
