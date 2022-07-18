package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * dialog for adding new DICOM tag information currently allows a limited number of special tags (procssing of which must
 * be performed by the DICOM tag itself), but allows editing seperate multiplicity values.
 *
 * @author   William Gandler
 * @version  0.01
 */
public class JDialogDICOMNewTagEditor extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JCheckBox applyToAllSlicesCheckBox;


    /** DOCUMENT ME! */
    private JTextField textTagGroup;
    
    private JTextField textTagElement;
    
    private JTextField textTagName;
    
    private JTextField textTagValue;
    
    String groupString;
    
    String elementString;
    
    String tagName;
    
    String tagValue;

    /** DOCUMENT ME! */
    private boolean struckOkayButton = false; // if the user hit the OKAY button set to true

    /** DOCUMENT ME! */
    private String tagKey;

    /** A reference to the tag table containing the tag we will be editing. */
    private FileDicomTagTable tagTable;


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

        textTagGroup = new JTextField(10);
        textTagGroup.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textTagGroup, gbc);
        
        JLabel labelTagElement = new JLabel("Hexadecimal tag element");
        labelTagElement.setForeground(Color.black);
        labelTagElement.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 1;
        paramPanel.add(labelTagElement, gbc);

        textTagElement = new JTextField(10);
        textTagElement.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textTagElement, gbc);
        
        JLabel labelTagName = new JLabel("Tag name");
        labelTagName.setForeground(Color.black);
        labelTagName.setFont(serif12);
        labelTagName.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy = 2;
        paramPanel.add(labelTagName, gbc);

        textTagName = new JTextField(80);
        textTagName.setFont(serif12);
        textTagName.setEnabled(false);
        gbc.gridx = 1;
        paramPanel.add(textTagName, gbc);
        
        JLabel labelTagValue = new JLabel("Tag value");
        labelTagValue.setForeground(Color.black);
        labelTagValue.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 3;
        paramPanel.add(labelTagValue, gbc);

        textTagValue = new JTextField(80);
        textTagGroup.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textTagValue, gbc);
        
        applyToAllSlicesCheckBox = new JCheckBox("Apply this change to all slices in image");
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

        if (command.equals("TagAddOK")) {

            if (setVariables()) {
            	tagTable.setValue(tagKey, tagValue);
            	// hide & set flags
                setVisible(false);
                dispose();
                struckOkayButton = true;
            }
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("");
        } else if (command.equals("TagAddCancel")) {
            dispose();
        } else {
            super.actionPerformed(event);
        }
    }
    
    private boolean setVariables() {
        int i;
        System.gc();
        
        groupString = textTagGroup.getText();
        if (!isHexadecimal(groupString)) {
        	MipavUtil.displayError("The tag group string is not hexadecimal");
            textTagGroup.requestFocus();
            textTagGroup.selectAll();
            return false;
        }
        if (groupString.length() > 4) {
        	MipavUtil.displayError("The tag group string cannot have a length greater than 4");
            textTagGroup.requestFocus();
            textTagGroup.selectAll();
            return false;
        }
        
        while (groupString.length() < 4) { // prepend with '0' as needed
            groupString = "0" + groupString;
        }

        groupString = groupString.toUpperCase();

        elementString = textTagElement.getText();
        if (!isHexadecimal(elementString)) {
        	MipavUtil.displayError("The tag element string is not hexadecimal");
            textTagElement.requestFocus();
            textTagElement.selectAll();
            return false;
        }
        if (elementString.length() > 4) {
        	MipavUtil.displayError("The tag element string cannot have a length greater than 4");
            textTagElement.requestFocus();
            textTagElement.selectAll();
            return false;
        }
        
        while (elementString.length() < 4) { // prepend with '0' as needed
            elementString = "0" + elementString;
        }

        elementString = elementString.toUpperCase();
        
        tagKey = groupString + "," + elementString;
        if (tagTable.containsTag(tagKey)) {
            MipavUtil.displayError("The tagTable already contains the speicifed group,element");
            return false;
        }
        
        tagName = textTagName.getText();
        /*if (tagName == null || tagName.length() == 0) {
        	MipavUtil.displayError("A name has not been provided");
            textTagName.requestFocus();
            textTagName.selectAll();
            return false;    	
        }*/
        
        tagValue = textTagValue.getText();
        if (tagValue == null || tagValue.length() == 0) {
        	MipavUtil.displayError("A value has not been provided");
            textTagValue.requestFocus();
            textTagValue.selectAll();
            return false;    	
        }
       

        return true;

    }
    
    private boolean isHexadecimal(String tString) {
        if (tString == null || tString.length() == 0 || 
             (tString.charAt(0) != '-' && Character.digit(tString.charAt(0), 16) == -1))
            return false;
        if ( tString.length() == 1 && tString.charAt(0) == '-' )
            return false;

        for ( int i = 1 ; i < tString.length() ; i++ )
            if ( Character.digit(tString.charAt(i), 16) == -1 )
                return false;
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
    
    public String getTagName() {
    	return tagName;
    }
    
    public String getTagValue() {
    	return tagValue;
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
        OKButton.setActionCommand("TagAddOK");

        // size and place the CANCEL button
        buildCancelButton();
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
        cancelButton.setActionCommand("TagAddCancel");

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
