package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;
import gov.nih.mipav.model.file.FileUtility;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Confirmation Dialog giving user the choice as to how to choose a graph time axis for dicom 4D images.
 * User can choose 0, 1, 2, ... indexing or Frame Reference Time
 */
public class JDialogDicomTimeAxisChoice extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID = ;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Radio button to indicate that the time axis numbers should be 0, 1, 2, 3. */
    private JRadioButton button0123;
    
    /** Radio button to indicate that the time axis should be the frame reference times. */
    private JRadioButton buttonFrameReferenceTime;

    /** Whether the window was closed through the user clicking the OK button (and not just killing the dialog). */
    private boolean okayPressed = false;
    
    /** Whether to graph as selected by dialog or always as 0, 1, 2,. or always as frame reference times. */
    private JComboBox comboBoxGraphMethod;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame of dialog.
     */
    public JDialogDicomTimeAxisChoice(Frame theParentFrame) {
        super(theParentFrame, true);
        init();

        // skip the dialog if the user has requested to not be bothered
        if (Preferences.is(Preferences.PREF_ALWAYS_GRAPH_TIME_AS_0123)) {
            comboBoxGraphMethod.setSelectedIndex(1);
            button0123.setSelected(true);
            okayPressed = true;
            dispose();

            return;
        }
        else if (Preferences.is(Preferences.PREF_ALWAYS_GRAPH_FRAME_REFERENCE_TIME)){
            comboBoxGraphMethod.setSelectedIndex(2);
            buttonFrameReferenceTime.setSelected(true);
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

            switch(comboBoxGraphMethod.getSelectedIndex()) {
                case 0:
                    // do nothing
                    break;
                case 1:
                    Preferences.setAlwaysGraphTimeAs0123(true);
                    break;
                case 2:
                    Preferences.setAlwaysGraphFrameReferenceTime(true);
                    break;
            }

            okayPressed = true;
        }

        dispose();
    }

    /**
     * Returns whether dicom time axis display is 0, 1, 2, ... or frame reference time
     *
     * @return  whether 0, 1, 2, ... or frame reference time
     */
    public int axisType() {

        if (button0123.isSelected()) {
            return 1;
        } 
        else {
            return 2;
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
        setTitle("Choose type of dicom time axis for graph ");

        PanelManager manager = new PanelManager("Graph dicom time axis as..");

        ButtonGroup graphGroup = new ButtonGroup();
        button0123 = WidgetFactory.buildRadioButton("0, 1, 2, ...", true, graphGroup);
        buttonFrameReferenceTime = WidgetFactory.buildRadioButton("Frame Reference Times", false, graphGroup);
        saveLabel = new JLabel("Always graph dicom time axis ");
        saveLabel.setFont(serif12);
        saveLabel.setForeground(Color.black);
        comboBoxGraphMethod = new JComboBox();
        comboBoxGraphMethod.setFont(serif12);
        comboBoxGraphMethod.setBackground(Color.white);
        comboBoxGraphMethod.addItem("from dialog choice");
        comboBoxGraphMethod.addItem("as 0, 1, 2, ...");
        comboBoxGraphMethod.addItem("as frame reference times");
        comboBoxGraphMethod.setSelectedIndex(0);

        manager.getConstraints().insets = new Insets(0, 15, 0, 10);
        manager.add(button0123);
        manager.addOnNextLine(buttonFrameReferenceTime);

        manager.getConstraints().insets = new Insets(10, 0, 0, 0);
        manager.addOnNextLine(saveLabel);
        manager.add(comboBoxGraphMethod);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);

        mainDialogPanel.add(manager.getPanel());
        mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

        pack();
    }
}
