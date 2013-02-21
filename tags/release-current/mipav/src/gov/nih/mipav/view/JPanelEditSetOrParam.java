package gov.nih.mipav.view;


import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Very simple EditPanel which merely contains some text. Its values cannot be verified.
 *
 * @author   David Parsons
 * @version  1.0
 */
public class JPanelEditSetOrParam extends JPanelEdit {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 2261232133976028312L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    int maxLength = -1;

    /** DOCUMENT ME! */
    private JTextField editTextField;

    /** DOCUMENT ME! */
    private boolean isSet;

    /** DOCUMENT ME! */
    private Hashtable<?,?> table;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Sets the panel to contain a standard, white JTextField.
     *
     * @param  initialText  the text which will fill the text field whn begun
     *
     * @see    JTextField
     */
    public JPanelEditSetOrParam(String initialText) {
        super();
        editTextField = new JTextField(initialText);
        editTextField.setBackground(Color.white);
        editTextField.setFont(MipavUtil.font12);
        editTextField.setColumns(32);
        editTextField.requestFocus();
        this.add(editTextField);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * There is nothing to check in this Panel. Returns <code>true</code>.
     *
     * @return  <code>true</code>
     */
    public boolean checkFields() {

        if (table.containsKey(getPanelValue())) {

            if (isSet) {
                errorString = "A set with this description exists!";
            } else {
                errorString = "A parameter with this name exists!";
            }

            errorComponent = editTextField;

            return false;
        }

        return true;
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
     * gets the data which the user could redefine within this panel.
     *
     * @return  This is the value translated from the user-editable fields
     */
    public String getPanelValue() {
        return editTextField.getText();
    }

    /**
     * @see  JTextField#setColumns(int)
     */
    public void setColumns(int col) {
        editTextField.setColumns(col);
    }

    /**
     * sets the system bell to ring whenever the string is larger than the specified value. attempting to reset to value
     * larger than a previous setting results in an undefinable behaviour
     *
     * @see  JTextField#setColumns(int)
     */
    public void setMaxLength(int col) {
        this.setColumns(col); // attempt to set the max # of columns.
        maxLength = col - 1;

        // set a listener to this to "ping" when the max number is reached.
        editTextField.addKeyListener(new KeyAdapter() {
                public void keyTyped(KeyEvent ke) {
                    JTextField t = (JTextField) ke.getComponent();

                    if ((ke.getKeyChar() != KeyEvent.VK_DELETE) && (ke.getKeyChar() != KeyEvent.VK_BACK_SPACE)) {

                        if (t.getText().length() > maxLength) {
                            Toolkit.getDefaultToolkit().beep();
                            ke.consume();
                        }
                    }
                }
            });
    }

    /**
     * DOCUMENT ME!
     *
     * @param  table  DOCUMENT ME!
     * @param  isSet  DOCUMENT ME!
     */
    public void setTable(Hashtable<?,?> table, boolean isSet) {
        this.table = table;
        this.isSet = isSet;
    }

}
