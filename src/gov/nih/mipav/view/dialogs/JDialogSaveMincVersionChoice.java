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
public class JDialogSaveMincVersionChoice extends JDialogBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Radio button to indicate that an Minc1 CDF mnc file should be written out. */
    private JRadioButton minc1File;
    
    /** Radio button to indicate that a Minc2 HDF5 mnc file should be written out. */
    private JRadioButton minc2File;
    
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
    public JDialogSaveMincVersionChoice(Frame theParentFrame) {
        super(theParentFrame, true);
        init();

        // skip the dialog if the user has requested to not be bothered
        if (Preferences.is(Preferences.PREF_ALWAYS_SAVE_MNC_AS_MINC1)) {
            comboBoxSaveMethod.setSelectedIndex(1);
            minc1File.setSelected(true);
            okayPressed = true;
            dispose();

            return;
        }
        else if (Preferences.is(Preferences.PREF_ALWAYS_SAVE_MNC_AS_MINC2)){
            comboBoxSaveMethod.setSelectedIndex(2);
            minc2File.setSelected(true);
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
                    Preferences.setAlwaysSaveMncAsMinc1(true);
                    break;
                case 2:
                    Preferences.setAlwaysSaveMncAsMinc2(true);
                    break;
            }

            okayPressed = true;
            dispose();
        } else {
            super.actionPerformed(event);
        }

        
    }

    /**
     * Returns whether Minc1 CDF or Minc2 HDF5 has been selected.
     *
     * @return  whether whether Minc1 CDF or Minc2 HDF5
     */
    public int fileType() {

        if (minc2File.isSelected()) {
            return FileUtility.MINC_HDF;
        } 
        else {
            return FileUtility.MINC;
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

        PanelManager manager = new PanelManager("Write .mnc file as..");

        ButtonGroup writeGroup = new ButtonGroup();
        minc1File = WidgetFactory.buildRadioButton("Minc-1.0 CDF file", true, writeGroup);
        minc2File = WidgetFactory.buildRadioButton("Minc-2.0 HDF5 file", false, writeGroup);
        saveLabel = new JLabel("Always save .mnc files ");
        saveLabel.setFont(serif12);
        saveLabel.setForeground(Color.black);
        comboBoxSaveMethod = new JComboBox();
        comboBoxSaveMethod.setFont(serif12);
        comboBoxSaveMethod.setBackground(Color.white);
        comboBoxSaveMethod.addItem("from dialog choice");
        comboBoxSaveMethod.addItem("in Minc-1.0 CDF format");
        comboBoxSaveMethod.addItem("in Minc-2.0 HDF5 format");
        comboBoxSaveMethod.setSelectedIndex(0);

        manager.getConstraints().insets = new Insets(0, 15, 0, 10);
        manager.add(minc1File);
        manager.addOnNextLine(minc2File);

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
