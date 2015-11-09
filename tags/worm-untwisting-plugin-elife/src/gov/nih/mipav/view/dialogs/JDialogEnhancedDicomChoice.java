package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.components.*;

import gov.nih.mipav.model.dicomcomm.DICOM_Constants;
import gov.nih.mipav.model.file.FileUtility;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Confirmation Dialog giving user the choice to write an enhanced dicom MR, CT, or XA file.
 */
public class JDialogEnhancedDicomChoice extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
   // private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Radio button to indicate that an enhanced MR file should be written out. */
    private JRadioButton enhancedMR;
    
    
    
    /** Radio button to indicate that an enhanced CT file should be written out. */
    private JRadioButton enhancedCT;

    /** Radio button to indicate that an enhanced XA file should be written out. */
    private JRadioButton enhancedXA;

    /** Whether the window was closed through the user clicking the OK button (and not just killing the dialog). */
    private boolean okayPressed = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame of dialog.
     */
    public JDialogEnhancedDicomChoice(Frame theParentFrame) {
        super(theParentFrame, true);
        init();

     
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

            okayPressed = true;
            
            dispose();
        } else {
            super.actionPerformed(event);
        }

    }

    /**
     * Returns whether enhancedMR, enhancedCT, or enhancedXA
     *
     * @return  whether enhancedMR, enhancedCT, or enhancedXA
     */
    public String dicomType() {

        if (enhancedMR.isSelected()) {
            return new String(DICOM_Constants.UID_EnhancedMRStorage);
        } 
        else if (enhancedCT.isSelected()){
            return new String(DICOM_Constants.UID_EnhancedCTStorage);
        }
        else {
            return new String(DICOM_Constants.UID_EnhancedXAStorage);
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
        setTitle("Choose type of enhanced dicom file to write");

        PanelManager manager = new PanelManager("Write enhanced dicom as..");

        ButtonGroup writeGroup = new ButtonGroup();
        enhancedMR = WidgetFactory.buildRadioButton("Enhanced MR", true, writeGroup);
        enhancedMR.addActionListener(this);
        enhancedCT = WidgetFactory.buildRadioButton("Enhanced CT", false, writeGroup);
        enhancedCT.addActionListener(this);
        enhancedXA = WidgetFactory.buildRadioButton("Enhanced XA", false, writeGroup);
        enhancedXA.addActionListener(this);

        manager.getConstraints().insets = new Insets(0, 50, 0, 10);
        manager.add(enhancedMR);
        
        manager.addOnNextLine(enhancedCT);
        manager.addOnNextLine(enhancedXA);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);

        mainDialogPanel.add(manager.getPanel());
        mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

        pack();
    }
}
