package gov.nih.mipav.view;


import java.awt.*;

import javax.swing.*;


/**
 * An EditPanel which allows the user to select among the Analyze possibilities for image orientation. The possibility
 * of "Unknown" is an option, but is coded as -1. Analyze images use the options:
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
public class JPanelEditOrientation extends JPanelEdit {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2127377398183049555L;

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
    public JPanelEditOrientation(String initialText) {
        super();

        String[] orients = {
            "Unknown", "transverse unflipped", "coronal unflipped", "sagittal unflipped", "transverse flipped",
            "coronal flipped", "sagittal flipped",
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
