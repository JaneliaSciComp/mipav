package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;
import gov.nih.mipav.model.file.FileUtility;

import java.awt.*;
import java.awt.event.*;

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

    /** Check box indicating that the user wants to always save .img files in Analyze format. */
    private JCheckBox alwaysSaveAnalyzeCheckBox;

    /** Radio button to indicate that an analyze img file should be written out. */
    private JRadioButton analyzeFile;
    
    /** Radio button to indicate that an Interfile img file should be written out. */
    private JRadioButton interfileFile;

    /** Radio button to indicate that a nifti img file should be written out. */
    private JRadioButton niftiFile;

    /** Whether the window was closed through the user clicking the OK button (and not just killing the dialog). */
    private boolean okayPressed = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame of dialog.
     */
    public JDialogAnalyzeNIFTIChoice(Frame theParentFrame) {
        super(theParentFrame, true);
        init();

        // skip the dialog if the user has requested to not be bothered
        if (Preferences.is(Preferences.PREF_ALWAYS_SAVE_IMG_AS_ANALYZE)) {
            alwaysSaveAnalyzeCheckBox.setSelected(true);
            analyzeFile.setSelected(true);
            okayPressed = true;
            dispose();

            return;
        }

        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Checks to see if the OK or Cancel buttons were pressed.
     *
     * @param  event  Event that triggered this function.
     */
    public void actionPerformed(ActionEvent event) {

        if (event.getSource() == OKButton) {

            if (alwaysSaveAnalyzeCheckBox.isSelected()) {
                Preferences.setAlwaysSaveImgAsAnalyze(true);
            }

            okayPressed = true;
        }

        dispose();
    }

    /**
     * Returns whether analyze, interfile, or nifti
     *
     * @return  whether analyze, interfile, or nifti
     */
    public int fileType() {

        if (analyzeFile.isSelected()) {
            return FileUtility.ANALYZE;
        } 
        else if (interfileFile.isSelected()){
            return FileUtility.INTERFILE;
        }
        else {
            return FileUtility.NIFTI;
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

        PanelManager manager = new PanelManager("Write .img as..");

        ButtonGroup writeGroup = new ButtonGroup();
        analyzeFile = WidgetFactory.buildRadioButton("Analyze file", true, writeGroup);
        interfileFile = WidgetFactory.buildRadioButton("Interfile file", false, writeGroup);
        niftiFile = WidgetFactory.buildRadioButton("NIfTI file", false, writeGroup);
        alwaysSaveAnalyzeCheckBox = WidgetFactory.buildCheckBox("Always save .img files in Analyze format", false);

        manager.getConstraints().insets = new Insets(0, 15, 0, 10);
        manager.add(analyzeFile);
        manager.addOnNextLine(interfileFile);
        manager.addOnNextLine(niftiFile);

        manager.getConstraints().insets = new Insets(10, 0, 0, 0);
        manager.addOnNextLine(alwaysSaveAnalyzeCheckBox);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);

        mainDialogPanel.add(manager.getPanel());
        mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

        pack();
    }
}
