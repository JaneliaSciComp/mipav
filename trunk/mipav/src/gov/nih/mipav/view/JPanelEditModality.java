package gov.nih.mipav.view;


import gov.nih.mipav.model.file.*;

import java.awt.*;

import javax.swing.*;


/**
 * An EditPanel which allows the user to select from a list of modalities (from FileInfoBase.modalityStr).
 */
public class JPanelEditModality extends JPanelEdit {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -7456502877292143209L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JComboBox modalityBox;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Sets the panel to contain a standard, white JComboBox.
     *
     * @param  initialText  the text to which the box will be set when begun
     *
     * @see    JComboBox
     */
    public JPanelEditModality(String initialText) {
        super();

        modalityBox = new JComboBox(FileInfoBase.getModalityStr());
        modalityBox.setBackground(Color.white);
        modalityBox.setFont(MipavUtil.font12);

        for (int i = 0; i < FileInfoBase.getModalityStr().length; i++) {

            if (initialText.equalsIgnoreCase(FileInfoBase.getModalityStr()[i])) {
                modalityBox.setSelectedIndex(i);
            } else if (initialText.equalsIgnoreCase("transverse")) {
                modalityBox.setSelectedIndex(1);
            }
        } // else, first selection is made.

        this.add(modalityBox);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * There is nothing to check in this Panel. Returns <code>true</code>.
     *
     * @return  <code>true</code>
     */
    public boolean checkFields() {
        return true; // no fields to check -- fields OKAY
    }

    /**
     * If the information held by the panel is stored as a code (as in 'M' for "male" or 'F' for "female"), then this
     * code will be returned by this method.
     *
     * <p>The codes returned are a Byte format for the following entry</p>
     *
     * <table>
     *   <tr>
     *     <td>unknown</td>
     *     <td>-1</td>
     *   </tr>
     *   <tr>
     *     <td>transverse</td>
     *     <td>0</td>
     *   </tr>
     *   <tr>
     *     <td>coronal</td>
     *     <td>1</td>
     *   </tr>
     *   <tr>
     *     <td>sagittal</td>
     *     <td>2</td>
     *   </tr>
     * </table>
     *
     * @return  a Byte. returned by getPanelValue().
     */
    public Object getCodedValue() {
        return new Byte((byte) (modalityBox.getSelectedIndex() - 1));
    }

    /**
     * gets the data which the user could redefine within this panel.
     *
     * @return  This is the value translated from the user-editable fields
     */
    public String getPanelValue() {
        return (String) modalityBox.getSelectedItem();
    }

}
