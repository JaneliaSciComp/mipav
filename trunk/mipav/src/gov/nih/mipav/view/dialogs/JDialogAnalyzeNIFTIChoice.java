package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;
import gov.nih.mipav.model.file.FileUtility;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Confirmation Dialog giving user the choice to write an analyze file , interfile file, or a nifti file.
 */
public class JDialogAnalyzeNIFTIChoice extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4588130080855618027L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Radio button to indicate that an analyze img file should be written out. */
    private JRadioButton analyzeFile;
    
    /** Checkbox to zero funused fields at 112, 116, and 120.  Default is not selected. */
    private JCheckBox zeroCheckBox;
    
    /** Radio button to indicate that an Interfile img file should be written out. */
    private JRadioButton interfileFile;

    /** Radio button to indicate that a nifti img file should be written out. */
    private JRadioButton niftiFile;

    /** Whether the window was closed through the user clicking the OK button (and not just killing the dialog). */
    private boolean okayPressed = false;
    
    /** Whether to save as selected by dialog or always as analyze, interfile, or nifti. */
    private JComboBox comboBoxSaveMethod;

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
            comboBoxSaveMethod.setSelectedIndex(1);
            analyzeFile.setSelected(true);
            okayPressed = true;
            dispose();

            return;
        }
        else if (Preferences.is(Preferences.PREF_ALWAYS_SAVE_IMG_AS_INTERFILE)){
            comboBoxSaveMethod.setSelectedIndex(2);
            interfileFile.setSelected(true);
            okayPressed = true;
            dispose();
            
            return;
        }
        else if (Preferences.is(Preferences.PREF_ALWAYS_SAVE_IMG_AS_NIFTI)){
            comboBoxSaveMethod.setSelectedIndex(3);
            niftiFile.setSelected(true);
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

            switch(comboBoxSaveMethod.getSelectedIndex()) {
                case 0:
                    // do nothing
                    break;
                case 1:
                    Preferences.setAlwaysSaveImgAsAnalyze(true);
                    break;
                case 2:
                    Preferences.setAlwaysSaveImgAsInterfile(true);
                    break;
                case 3:
                    Preferences.setAlwaysSaveImgAsNifti(true);
                    break;
            }

            okayPressed = true;
            
            dispose();
        }
        else if ((event.getSource() == analyzeFile) || (event.getSource() == interfileFile) ||
        		 (event.getSource() == niftiFile)) {
        	if (analyzeFile.isSelected()) {
        		zeroCheckBox.setEnabled(true);
        	}
        	else {
        		zeroCheckBox.setEnabled(false);
        	}
        }

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
    
    public boolean zerofunused() {
    	if (zeroCheckBox.isSelected()) {
    		return true;
    	}
    	else {
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
        JLabel saveLabel;
        setTitle("Choose type of file to write");

        PanelManager manager = new PanelManager("Write .hdr/.img as..");

        ButtonGroup writeGroup = new ButtonGroup();
        analyzeFile = WidgetFactory.buildRadioButton("Analyze file", true, writeGroup);
        analyzeFile.addActionListener(this);
        zeroCheckBox = WidgetFactory.buildCheckBox("Zero funused fields", false);
        interfileFile = WidgetFactory.buildRadioButton("Interfile file", false, writeGroup);
        interfileFile.addActionListener(this);
        niftiFile = WidgetFactory.buildRadioButton("Nifti file", false, writeGroup);
        niftiFile.addActionListener(this);
        saveLabel = new JLabel("Always save .hdr/.img files ");
        saveLabel.setFont(serif12);
        saveLabel.setForeground(Color.black);
        comboBoxSaveMethod = new JComboBox();
        comboBoxSaveMethod.setFont(serif12);
        comboBoxSaveMethod.setBackground(Color.white);
        comboBoxSaveMethod.addItem("from dialog choice");
        comboBoxSaveMethod.addItem("in Analyze format");
        comboBoxSaveMethod.addItem("in Interfile format");
        comboBoxSaveMethod.addItem("in Nifti format");
        comboBoxSaveMethod.setSelectedIndex(0);

        manager.getConstraints().insets = new Insets(0, 15, 0, 10);
        manager.add(analyzeFile);
        manager.addOnNextLine(zeroCheckBox);
        
        manager.addOnNextLine(interfileFile);
        manager.addOnNextLine(niftiFile);

        manager.getConstraints().insets = new Insets(10, 0, 0, 0);
        manager.addOnNextLine(saveLabel);
        manager.add(comboBoxSaveMethod);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);

        mainDialogPanel.add(manager.getPanel());
        mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

        pack();
    }
}
