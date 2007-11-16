package gov.nih.mipav.view;


import gov.nih.mipav.model.file.*;

import java.awt.*;

import javax.swing.*;


/**
 * An EditPanel which allows the user to select among the Analyze possibilities for image orientation. The possibility
 * of "Unknown" is not an option. Analyze images use the options:
 *
 * <table>
 *   <tr>
 *     <td>unknown</td>
 *     <td>-1</td>
 *   </tr>
 *   <tr>
 *     <td>transverse unflipped</td>
 *     <td>0</td>
 *   </tr>
 *   <tr>
 *     <td>coronal unflipped</td>
 *     <td>1</td>
 *   </tr>
 *   <tr>
 *     <td>sagittal unflipped</td>
 *     <td>2</td>
 *   </tr>
 *   <tr>
 *     <td>transverse flipped</td>
 *     <td>3</td>
 *   </tr>
 *   <tr>
 *     <td>coronal flipped</td>
 *     <td>4</td>
 *   </tr>
 *   <tr>
 *     <td>sagittal flipped</td>
 *     <td>5</td>
 *   </tr>
 * </table>
 *
 * Its values need not be verified.
 *
 * @author   David Parsons
 * @version  1.0
 */
public class JPanelEditAxisOrientation extends JPanelEdit {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6931501818687637674L;

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
    public JPanelEditAxisOrientation(String initialText) {
        super();

        String[] orients = {
            "Unknown", "Right to Left", "Left to Right", "Posterior to Anterior", "Anterior to Posterior",
            "Inferior to Superior", "Superior to Inferior"
        };
        orientationBox = new JComboBox(orients);
        orientationBox.setBackground(Color.white);
        orientationBox.setFont(MipavUtil.font12);

        for (int i = 0; i < orients.length; i++) {

            if (initialText.equalsIgnoreCase(orients[i])) {
                orientationBox.setSelectedIndex(i);
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
     *     <td>tRight to Left</td>
     *     <td>0</td>
     *   </tr>
     *   <tr>
     *     <td>Left to Right</td>
     *     <td>1</td>
     *   </tr>
     *   <tr>
     *     <td>Posterior to Anterior</td>
     *     <td>2</td>
     *   </tr>
     *   <tr>
     *     <td>Anterior to Posterior</td>
     *     <td>3</td>
     *   </tr>
     *   <tr>
     *     <td>Inferior to Superior</td>
     *     <td>4</td>
     *   </tr>
     *   <tr>
     *     <td>Superior to Inferior</td>
     *     <td>5</td>
     *   </tr>
     * </table>
     *
     * @return  an Integer. returned by getPanelValue().
     */
    public Object getCodedValue() {
        Integer value = null;

        switch (orientationBox.getSelectedIndex()) {

            case 0:
                value = new Integer(FileInfoBase.ORI_UNKNOWN_TYPE);
                break;

            case 1:
                value = new Integer(FileInfoBase.ORI_R2L_TYPE);
                break;

            case 2:
                value = new Integer(FileInfoBase.ORI_L2R_TYPE);
                break;

            case 3:
                value = new Integer(FileInfoBase.ORI_P2A_TYPE);
                break;

            case 4:
                value = new Integer(FileInfoBase.ORI_A2P_TYPE);
                break;

            case 5:
                value = new Integer(FileInfoBase.ORI_I2S_TYPE);
                break;

            case 6:
                value = new Integer(FileInfoBase.ORI_S2I_TYPE);
                break;
        }

        return value;
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
