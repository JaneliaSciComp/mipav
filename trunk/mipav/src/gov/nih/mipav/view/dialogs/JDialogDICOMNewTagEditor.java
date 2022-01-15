package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * dialog for editing DICOM tag information currently allows a limited number of special tags (procssing of which must
 * be performed by the DICOM tag itself), but allows editing seperate multiplicity values.
 *
 * @author   William Gandler
 * @version  0.01
 */
public class JDialogDICOMNewTagEditor extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 8765963102638731411L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JCheckBox applyToAllSlicesCheckBox;

    /** DOCUMENT ME! */
    private JPanelEdit[] newInputPanel;

    /** DOCUMENT ME! */
    private JTextField originalTextField;

    /** DOCUMENT ME! */
    private boolean struckOkayButton = false; // if the user hit the OKAY button set to true

    /** DOCUMENT ME! */
    private String tagKey;

    /** A reference to the tag table containing the tag we will be editing. */
    private FileDicomTagTable tagTable;

    /** The main panel that is accessed. */
	private Box mainBox;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * builds a dialog box with as many input panels as is needed to allow changing each value (when there are more than
     * one values (vm > 1). as in "v1\v2\v3") independantly of any other value in the tag.
     *
     * @param  parent     DICOM key (gggg,eeee) for this tag (as might be stored in a hashtable.
     * @param  _tagTable  the tag table containing the tag to edit.
     * @param  modal      force this dialog to stay on top (when true)
     */
    public JDialogDICOMNewTagEditor(Dialog parent, FileDicomTagTable _tagTable, boolean modal, boolean isStandalone) {
        super(parent, modal);

        this.tagTable = _tagTable;
        
        if(isStandalone) {
        	setTitle("Add new tag");
        }
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        
        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setBorder(buildTitledBorder("Tag parameters"));
        
        JLabel labelTagGroup = new JLabel("Hexadecimal tag group");
        labelTagGroup.setForeground(Color.black);
        labelTagGroup.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 0;
        paramPanel.add(labelTagGroup, gbc);

        JTextField textTagGroup = new JTextField(10);
        textTagGroup.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textTagGroup, gbc);
        
        JLabel labelTagElement = new JLabel("Hexadecimal tag element");
        labelTagElement.setForeground(Color.black);
        labelTagElement.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 1;
        paramPanel.add(labelTagElement, gbc);

        JTextField textTagElement = new JTextField(10);
        textTagElement.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textTagElement, gbc);
        
        JLabel labelTagName = new JLabel("Tag name");
        labelTagName.setForeground(Color.black);
        labelTagName.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 2;
        paramPanel.add(labelTagName, gbc);

        JTextField textTagName = new JTextField(80);
        textTagName.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textTagName, gbc);
        
        JLabel labelTagValue = new JLabel("Tag value");
        labelTagValue.setForeground(Color.black);
        labelTagValue.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 3;
        paramPanel.add(labelTagValue, gbc);

        JTextField textTagValue = new JTextField(80);
        textTagGroup.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textTagValue, gbc);
        
        JCheckBox applyToAllSlicesCheckBox = new JCheckBox("Apply this change to all slices in image");
        applyToAllSlicesCheckBox.setActionCommand("NewTagEditorApplyToAllSlicesCheckBox");
        applyToAllSlicesCheckBox.setFont(serif12);
        applyToAllSlicesCheckBox.setForeground(Color.black);
        gbc.gridx = 0;
        gbc.gridy = 4;
        paramPanel.add(applyToAllSlicesCheckBox, gbc);

        

        if(isStandalone) {
	        getContentPane().add(paramPanel, BorderLayout.CENTER);
	        getContentPane().add(buildOKCancelPanel(), BorderLayout.SOUTH);
	
	        pack();
	        setResizable(true); // default anyway
	        setVisible(true);
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * closes dialog box when the OK button is pressed.
     *
     * @param  event  event that triggers this function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
            	//tagTable.setValue(tagKey, newValue.toString());
            	// hide & set flags
                setVisible(false);
                dispose();
                struckOkayButton = true;
            }
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("");
        } else if (command.equals("Cancel")) {
            dispose();
        } else {
            super.actionPerformed(event);
        }
    }
    
    private boolean setVariables() {

        System.gc();

        String tmpStr;
        
        /*tmpStr = textHighThreshold.getText();

        if (testParameter(tmpStr, 0.001, 1.0)) {
            highThreshold = Double.valueOf(tmpStr).doubleValue();
        } else {
            textHighThreshold.requestFocus();
            textHighThreshold.selectAll();

            return false;
        }

        tmpStr = textLowThreshold.getText();

        if (testParameter(tmpStr, 0.0001, 0.9999 * highThreshold)) {
            lowThreshold = Double.valueOf(tmpStr).doubleValue();
        } else {
            textLowThreshold.requestFocus();
            textLowThreshold.selectAll();

            return false;
        }
        
        tmpStr = textSigma.getText();

        if (testParameter(tmpStr, 1.0, 10.0)) {
            sigma = Float.valueOf(tmpStr).floatValue();
        } else {
            textSigma.requestFocus();
            textSigma.selectAll();

            return false;
        }*/
       

        return true;

    }

    

    /**
     * tell the "Apply to all slices" check-box, OK and cancel buttons to tell the calling object that it is doing
     * something. Creates a listener to an object
     *
     * @param  listener  listener object to listen to the OKButton, cancelButton, and applyToAllSlicesCheckBox.
     */
    public void addButtonListener(ActionListener listener) {
        applyToAllSlicesCheckBox.addActionListener(listener);
        OKButton.addActionListener(listener);
        cancelButton.addActionListener(listener);
    }


    /**
     * accessor to see the value of the selected value of the applyToAllSlicesCheckBox. Allows the user class to find
     * out if the user wants the changes in the DICOM tag to be spread over an entire image-set.
     *
     * @return  boolean the value of applyToAllSlicesCheckBox.isSelected()
     */
    public boolean applyToAllSlices() {
        return applyToAllSlicesCheckBox.isSelected();
    }


    /**
     * returns the key to the edited tag (group,element) for the tag (ie., "0010,0040").
     *
     * @return  DOCUMENT ME!
     */
    public String getTagKey() {
        return tagKey;
    }

    /**
	 * @return the mainBox
	 */
	public Box getMainBox() {
		return mainBox;
	}

	/**
     * check the dialog so that if all the fields are okay (have the right number of digits, etc) and there are no
     * messages to send back to the user about correctness.
     *
     * @return  boolean true if the dialog box is closing okay.
     */
    public boolean isDialogOkay() {

        for (int i = 0; i < newInputPanel.length; i++) {

            if (!newInputPanel[i].checkFields()) { // if (!okay)
                return false;
            }
        }

        return true;
    }

    /**
     * returns the value of the edited tag.
     *
     * @return  DOCUMENT ME!
     */
    public FileDicomTag returnTag() {
        return tagTable.get(tagKey);
    }

    /**
     * check the dialog that dialog's OKAY button was struck, and it closed without problems.
     *
     * @return  boolean true if the dialog box closed okay.
     */
    public boolean wasDialogOkay() {
        return struckOkayButton;
    }
    
    public void setStruckOkayButton(boolean struckOkayButton) {
        this.struckOkayButton = struckOkayButton;
    }

    /**
     * builds the panel which allows user to accept or decline using this algorithm.
     *
     * @return  the panel already populated.
     */
    protected JPanel buildOKCancelPanel() {

        // Build the Panel that holds the OK and CANCEL Buttons
        JPanel OKCancelPanel = new JPanel();

        // size and place the OK button
        buildOKButton();
        OKCancelPanel.add(OKButton, BorderLayout.WEST);
        OKButton.setActionCommand("TagEditorOK");

        // size and place the CANCEL button
        buildCancelButton();
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
        cancelButton.setActionCommand("TagEditorCancel");

        return OKCancelPanel;
    }


    /**
     * allows editing of the input value as allowed by kind of VR or keyword.
     *
     * @param   editString  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private JPanelEdit makeAppropriateInputPanel(String editString) {
        JPanelEdit inputPanel = null;

        switch(tagTable.get(tagKey).getValueRepresentation()) {
        case DA:
        	inputPanel = new JPanelEditDate(editString, false);
        	break;
        case TM:
        	inputPanel = new JPanelEditTime(editString, false);
        	break;
        case SQ:
        	//TODO: Create panel for editing dicom sequences
        	//break;
            
            inputPanel = new JPanelEditDefault(editString);
            break;
        default:
        	if (tagTable.get(tagKey).getKeyword().equals("PatientSex")) {
        		inputPanel = new JPanelEditSex(editString);
        	} else {
        		inputPanel = new JPanelEditDefault(editString);
        	}
        }
        return inputPanel;
    }
}
