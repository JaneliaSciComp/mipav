package gov.nih.mipav.view;


import javax.swing.*;


/**
 * adds panel with combobox to change field containing the patient's sex.
 *
 * @author   David Parsons
 * @version  1.0
 */
public class JPanelEditSex extends JPanelEdit {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -3767740357915707674L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    JComboBox sexComboBox;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * allows the user to select a DICOM allowable sex: Male, Female, Other; also allows "no value" and will return ""
     * when asked about that value.
     *
     * @param  initialText  DOCUMENT ME!
     */
    public JPanelEditSex(String initialText) {
        super();

        sexComboBox = new JComboBox();
        sexComboBox.setFont(MipavUtil.font12);
        sexComboBox.addItem("Male");
        sexComboBox.addItem("Female");
        sexComboBox.addItem("Other");
        sexComboBox.addItem("No Value");

        if (initialText.equals("No value set") || initialText.equals("")) {
            sexComboBox.setSelectedItem("No Value");
        } else {
            sexComboBox.setSelectedItem((Object) initialText);
        }

        this.add(sexComboBox);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * method does nothing in this class.
     *
     * @return  boolean FieldsOKAY
     */
    public boolean checkFields() {
        return true; // everything is fine
    }

    /**
     * If the information held by the panel is stored as a code (as in 'M' for "male" or 'F' for "female"), then this
     * code will be returned by this method. If there is no code for this value, this method will return a the same
     * value as getPanelValue()
     *
     * @return  the coded value. If the panel <i>has</i> no coded value, then the return value will be the String
     *          returned by getPanelValue().
     */
    public Object getCodedValue() {
        return getPanelValue();
    }

    /**
     * returns an empty String when the panel has been set to "No Value" otherwise, it returns the value.
     *
     * @return  DOCUMENT ME!
     */
    public String getPanelValue() {

        if (sexComboBox.getSelectedItem().equals("No Value")) {
            return ""; // make "No Value" really not a value
        }

        return (String) sexComboBox.getSelectedItem();
    }
} // end of JPanelEditSex
