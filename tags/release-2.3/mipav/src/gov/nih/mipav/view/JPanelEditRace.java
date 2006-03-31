package gov.nih.mipav.view;

import javax.swing.*;
import java.awt.*;

/**
*   An EditPanel which allows the user to select from a
*   list of races
*/
public class JPanelEditRace extends JPanelEdit {
    private JComboBox raceBox;

    /**
    *   Sets the panel to contain a standard, white JComboBox
    *   @param initialText the text to which the box will be set when begun
    *   @see JComboBox
    */
    public JPanelEditRace(String initialText) {
        super();

        String races[] = {"Unknown",
                          "Caucasian",
                          "American Indian",
                          "African-American",
                          "Asian",
                          "Hispanic"};

        raceBox = new JComboBox(races);
        raceBox.setBackground(Color.white);
        raceBox.setFont(MipavUtil.font12);
        for (int i = 0; i < races.length; i++) {
            if (initialText.equalsIgnoreCase(races[i])) {
                raceBox.setSelectedIndex(i);
            }
        }   // else, first selection is made.
        this.add(raceBox);
    }

    /**
    *   gets the data which the user could redefine within this panel
    *   @return This is the value translated from the user-editable fields
    */
    public String getPanelValue() {
        return (String)raceBox.getSelectedItem();
    }

    public Object getCodedValue() {
        return new Byte((byte) (raceBox.getSelectedIndex() - 1));
    }

    /**
    *   There is  nothing to check in this Panel.  Returns <code>true</code>.
    *   @return <code>true</code>
    */
    public boolean checkFields() {
        return true;        // no fields to check -- fields OKAY
    }

}
