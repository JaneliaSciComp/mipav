package gov.nih.mipav.view;

import javax.swing.*;
import java.awt.event.*;
import java.awt.*;

/** Very simple EditPanel which merely edit a letter at a time.
*   Its values cannot be verified.
*   @author David Parsons
*   @version 1.0
*/
public class JPanelEditChar extends JPanelEdit {
    private JTextField editTextField;

    /**
    *   Sets the panel to contain a standard, white JTextField
    *   @param initialChar the text which will fill the text field whn begun
    *   @see JTextField
    */
    public JPanelEditChar(char initialChar) {
        super();
        editTextField = new JTextField(initialChar);
        editTextField.setBackground(Color.white);
        editTextField.setFont(MipavUtil.font12);
        editTextField.setColumns(3);
        editTextField.setHorizontalAlignment(JTextField.CENTER);
        editTextField.addKeyListener(new KeyAdapter() {
            public void keyTyped(KeyEvent ke) {
                JTextField t = (JTextField) ke.getSource();
                t.selectAll();
            }
        });
        this.add(editTextField);
    }

    /**
    *   gets the data which the user could redefine within this panel
    *   @return This is the value translated from the user-editable fields
    */
    public String getPanelValue() {
        return editTextField.getText();
    }

    /** If the information held by the panel is stored as a
    *   code (as in 'M' for "male" or 'F' for "female"), then
    *   this code will be returned by this method.  If there is
    *   no code for this value, this method will return a
    *   the same value as getPanelValue()
    *   @return the coded value.  If the panel <i>has</i> no coded
    *           value, then the return value will be the String
    *           returned by getPanelValue().
    */
    public Object getCodedValue() {
        return getPanelValue();
    }

    /**
    *   There is  nothing to check in this Panel.  Returns <code>true</code>.
    *   @return <code>true</code>
    */
    public boolean checkFields() {
        return true;        // no fields to check -- fields OKAY
    }

}
