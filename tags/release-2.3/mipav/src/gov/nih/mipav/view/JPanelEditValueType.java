package gov.nih.mipav.view;


import javax.swing.*;
import java.awt.*;

/**
*   An EditPanel which allows the user to select from a
*   list of parameter value types
*/
public class JPanelEditValueType extends JPanelEdit {
    private JComboBox valueTypeBox;

    /**
    *   Sets the panel to contain a standard, white JComboBox
    *   @param initialText the text to which the box will be set when begun
    *   @see JComboBox
    */
    public JPanelEditValueType(String initialText) {
        super();

        String valueTypes[] = {"ubyte",
                               "byte",
                               "ushort",
                               "short",
                               "int",
                               "long",
                               "float",
                               "double",
                               "string",
                               "boolean"};

        valueTypeBox = new JComboBox(valueTypes);
        valueTypeBox.setBackground(Color.white);
        valueTypeBox.setFont(MipavUtil.font12);
        for (int i = 0; i < valueTypes.length; i++) {
            if (initialText.equalsIgnoreCase(valueTypes[i])) {
                valueTypeBox.setSelectedIndex(i);
            }
        }   // else, first selection is made.
        this.add(valueTypeBox);
    }

    /**
    *   gets the data which the user could redefine within this panel
    *   @return This is the value translated from the user-editable fields
    */
    public String getPanelValue() {
        return (String)valueTypeBox.getSelectedItem();
    }

    public Object getCodedValue() {
        return new Byte((byte) (valueTypeBox.getSelectedIndex() - 1));
    }

    /**
    *   There is  nothing to check in this Panel.  Returns <code>true</code>.
    *   @return <code>true</code>
    */
    public boolean checkFields() {
        return true;        // no fields to check -- fields OKAY
    }

}
