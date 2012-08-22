package gov.nih.mipav.view;


import java.awt.*;

import javax.swing.*;


/**
 * An EditPanel which allows the user to select among the Analyze possibilities for image orientation. The possibility
 * of "Unknown" is an option, but is coded as -1. All Mipav images use the options:
 *
 * <table>
 *   <tr>
 *     <td>unknown</td>
 *     <td>-1</td>
 *   </tr>
 *   <tr>
 *     <td>transverse or Axial</td>
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
 * Its values need not be verified.
 *
 * @author   David Parsons
 * @version  1.0
 */
public class JPanelEditImageOrientation extends JPanelEdit {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 155707351575492439L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JComboBox orientationBox;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Sets the panel to contain a standard, white JComboBox.
     *
     * @param  initialText  the text to which the box will be set when begun
     *
     * @see    JComboBox
     */
    public JPanelEditImageOrientation(String initialText) {
        super();

        String[] orients = { "Unknown", "Axial", "Coronal", "Sagittal" };
        orientationBox = new JComboBox(orients);
        orientationBox.setBackground(Color.white);
        orientationBox.setFont(MipavUtil.font12);

        for (int i = 0; i < orients.length; i++) {

            if (initialText.equalsIgnoreCase(orients[i])) {
                orientationBox.setSelectedIndex(i);
            } else if (initialText.equalsIgnoreCase("transverse")) {
                orientationBox.setSelectedIndex(1);
            }
        } // else, first selection is made.

        this.add(orientationBox);
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
        return new Byte((byte) (orientationBox.getSelectedIndex() - 1));
    }

    /**
     * gets the data which the user could redefine within this panel.
     *
     * @return  This is the value translated from the user-editable fields
     */
    public String getPanelValue() {
        return (String) orientationBox.getSelectedItem();
    }

}
