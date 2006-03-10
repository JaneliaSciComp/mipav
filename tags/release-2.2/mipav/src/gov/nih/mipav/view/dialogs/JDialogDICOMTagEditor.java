package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.file.FileDicomTag;

import javax.swing.*;
import java.util.*;
import java.awt.event.*;
import java.awt.*;


/**
*   dialog for editing DICOM tag information currently allows
*   a limited number of special tags (procssing of which must
*   be performed by the DICOM tag itself), but allows editing seperate
*   multiplicity values.
*
*   @author David Parsons (parsonsd)
*   @version 0.02
*/
public class JDialogDICOMTagEditor extends JDialogBase {
    private     FileDicomTag    tag;
    private     String          tagKey;

    private     JCheckBox       applyToAllSlicesCheckBox;
    private     JTextField      originalTextField;
    private     JPanelEdit[]    newInputPanel;

    private     boolean         okay;          // check for if input fields are okay to send back
    private     boolean         struckOkayButton = false; // if the user hit the OKAY button set to true

    /**
    *   builds a dialog box with as many input
    *   panels as is needed to allow changing
    *   each value (when there are more than one
    *   values (vm > 1). as in "v1\v2\v3") independantly
    *   of any other value in the tag.
    *   @param _tagKey--the DICOM key (gggg,eeee) for this tag (as might be
    *                   stored in a hashtable.
    *   @param _tag  -- the DicomTag to edit.
    *   @param parent-- the owner of the JDialog.  Sets the imageIcon.
    *   @param modal -- force this dialog to stay on top (when true)
    */
    public JDialogDICOMTagEditor(Dialog parent, String _tagKey, FileDicomTag _tag, boolean modal) {
        super(parent, modal);

        okay = false;
        this.tag = _tag;
        this.tagKey   = _tagKey;
        setTitle("Edit tag ("+tagKey+"): "+ tag.getName()   );

        Box mainBox = new Box(BoxLayout.Y_AXIS);
        Box editBox = new Box(BoxLayout.Y_AXIS);

        // Build original value panel; contains both label and tag-value
        JPanel originalValuePanel = new JPanel();
        originalValuePanel.setBorder(buildTitledBorder("Original Tag Value"));

        originalTextField = new JTextField(this.tag.getValue(true).toString());
        originalTextField.setColumns(32);
        originalTextField.setEditable(false);
        originalTextField.setBackground(Color.lightGray);
        originalTextField.setFont(MipavUtil.font12);
        originalValuePanel.add(originalTextField);
        editBox.add(originalValuePanel);

        editBox.add(Box.createVerticalStrut(5));

        // Build editing panels.
        int i = 0;
        String editString;
        StringTokenizer val = new StringTokenizer(this.tag.getValue(true).toString(), "\\");
        if (val.countTokens() == 0) {
            newInputPanel = new JPanelEdit[1];
        }
        else {
            newInputPanel = new JPanelEdit[val.countTokens()];
        }
        Box newInputPanelHolder = new Box(BoxLayout.Y_AXIS);
        if (val.countTokens() == 0) {       // allow 1 edit panel.  This may not work correctly...
            newInputPanel[0] = makeAppropriateInputPanel("");
            newInputPanelHolder.add(newInputPanel[0]);
        }
        else {
            while (val.hasMoreTokens()) {
                editString = val.nextToken();       // will only give up to the '\' if it exists,
                                                    // and if no '\', then will get whole token
                newInputPanel[i] = makeAppropriateInputPanel(editString);
                newInputPanelHolder.add(newInputPanel[i]);
                i++;
            }
        }

        // build a scroll pane to put all the editing panels in.
        JScrollPane inputPanelScroller = new JScrollPane(newInputPanelHolder);
        // add a titles border around the scroll box
        inputPanelScroller.setBorder(buildTitledBorder("New Tag Value"));
        editBox.add(inputPanelScroller);

        mainBox.add(editBox);

        // add the apply-to-all-images checkbox.
        applyToAllSlicesCheckBox = new JCheckBox("Apply this change to all slices in image");
        applyToAllSlicesCheckBox.setActionCommand("TagEditorApplyToAllSlicesCheckBox");
        applyToAllSlicesCheckBox.setFont(serif12B);
        applyToAllSlicesCheckBox.setForeground(Color.black);
        mainBox.add(applyToAllSlicesCheckBox);

		getContentPane().add(mainBox,     BorderLayout.CENTER);
		getContentPane().add(buildOKCancelPanel(), BorderLayout.SOUTH);

	    pack();
		setResizable(true);         // default anyway
		setVisible(true);
    }

    /** builds the panel which allows user to accept or decline
    *   using this algorithm.
    *   @return the panel already populated.
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
   *    closes dialog box when the OK button is pressed
   *    @param event      event that triggers this function
   */
    public void actionPerformed(ActionEvent event) {
   	    Object source = event.getSource();
   	    int i;
   	    boolean okay;
   	    if (source == OKButton) {
   	        // verify there are no problems
   	        //  \\ this code can be trimmed some ....
   	        if (!isDialogOkay()) {
   	            for (i = 0; i < newInputPanel.length; i++) {
   	                if (!newInputPanel[i].checkFields()) {  // trap first error
   	                    MipavUtil.displayError(newInputPanel[i].getErrorString());
   	                    newInputPanel[i].getErrorComponent().requestFocus();
                        return;
   	                }
   	            }
            }
            okay = true;

            // build panel value
            StringBuffer newValue = new StringBuffer();
            int numValues = newInputPanel.length - 1;
            for (i = 0; i < numValues; i++) {
                newValue.append(newInputPanel[i].getPanelValue());
                newValue.append('\\');
            }
            newValue.append(newInputPanel[i].getPanelValue());
            tag.setValue(newValue.toString());

            // hide & set flags
  	        setVisible(false);
  	        dispose();
  	        struckOkayButton = true;
   	    }
        else if ( source == cancelButton ) {
       	    dispose();
        }
    }

    /**
    *   tell the "Apply to all slices" check-box, OK and cancel buttons to tell
    *   the calling object that it is doing something.  Creates a listener to
    *   an object
    *
    *   @param ActionListener listener  object to listen to the OKButton, cancelButton, and
    *                                   applyToAllSlicesCheckBox.
    */
    public void addButtonListener(ActionListener listener){
        applyToAllSlicesCheckBox.addActionListener(listener);
        OKButton.addActionListener(listener);
        cancelButton.addActionListener(listener);
    }


    /**
    *   returns the key to the edited tag (group,element) for the tag (ie., "0010,0040")
    *
    */
    public String getTagKey() {
        return tagKey;
    }

    /**
    *   returns the value of the edited tag.
    *
    */
    public Object returnTag() {
        return tag;
    }

    /**
    *   check the dialog so that if all the fields are okay (have the right number of
    *   digits, etc) and there are no messages to send back to the user about correctness
    *   @return boolean  true if the dialog box is closing okay.
    */
    public boolean isDialogOkay() {
	    for (int i = 0; i < newInputPanel.length; i++) {
   	        if (!newInputPanel[i].checkFields()) {// if (!okay)
   	            return false;
   	        }
        }

        return true;
    }
    /**
    *   check the dialog that dialog's OKAY button was struck, and it closed
    *   without problems.
    *   @return boolean  true if the dialog box closed okay.
    */
    public boolean wasDialogOkay() {
        return struckOkayButton;
    }


    /**
    *   accessor to see the value of the selected value of the
    *   applyToAllSlicesCheckBox.  Allows the user class to find out if the user
    *   wants the changes in the DICOM tag to be spread over an entire image-set.
    *   @return boolean     the value of applyToAllSlicesCheckBox.isSelected()
    *
    */
    public boolean applyToAllSlices() {
        return applyToAllSlicesCheckBox.isSelected();
    }


    /**
    *   allows editing of the input value as allowed by
    *   kind of VR or keyword
    *
    */
    private JPanelEdit makeAppropriateInputPanel(String editString) {
        JPanelEdit inputPanel;

        if (tag.getVR().equals("DA")) {
            inputPanel = new JPanelEditDate(editString, false);
        }
        else if (tag.getVR().equals("TM")) {
            inputPanel = new JPanelEditTime(editString, false);
        }
        else if (tag.getKeyword().equals("PatientSex")) {
            inputPanel = new JPanelEditSex(editString);
        }
        else {
            inputPanel = new JPanelEditDefault(editString);
        }
        return inputPanel;
    }




}
