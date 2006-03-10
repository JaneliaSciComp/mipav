package gov.nih.mipav.view;


import javax.swing.*;
import java.awt.*;


/**
 *   An EditPanel which allows the user to select a boolean value ('true' or 'false').
 *   Its values need not be verified.
 *   @author Evan McCreedy
 *   @version 1.0 August 16, 2004
 */
public class JPanelEditBoolean extends JPanelEdit {
    private JComboBox booleanBox;

    /**
     *   Sets the panel to contain a standard, white JComboBox
     *   @param initialText the text to which the box will be set when begun
     *   @see JComboBox
     */
    public JPanelEditBoolean( String initialText ) {
        super();
        String[] options = { "True", "False" };

        booleanBox = new JComboBox( options );
        booleanBox.setBackground( Color.white );
        booleanBox.setFont( MipavUtil.font12 );
        for ( int i = 0; i < options.length; i++ ) {
            if ( initialText.equalsIgnoreCase( options[i] ) ) {
                booleanBox.setSelectedIndex( i );
            }
        } // else, first selection is made.
        this.add( booleanBox );
    }

    /**
     *   gets the data which the user could redefine within this panel
     *   @return This is the value translated from the user-editable fields
     */
    public String getPanelValue() {
        return (String) booleanBox.getSelectedItem();
    }

    /**
     * If the information held by the panel is stored as a
     * code (as in 'M' for "male" or 'F' for "female"), then
     * this code will be returned by this method.
     * @return a Byte.
     */
    public Object getCodedValue() {
        return Boolean.valueOf( (String) booleanBox.getSelectedItem() );
    }

    /**
     *   There is  nothing to check in this Panel.  Returns <code>true</code>.
     *   @return <code>true</code>
     */
    public boolean checkFields() {
        return true; // no fields to check -- fields OKAY
    }
}
