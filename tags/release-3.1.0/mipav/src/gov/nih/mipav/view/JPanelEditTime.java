package gov.nih.mipav.view;


import java.awt.event.*;

import javax.swing.*;


/**
 * panel contains 4 fields:
 *
 * <ul>
 *   <li>3 comboboxes (hour, minute, second),</LI>
 *   <li>1 text field (decimal fraction of a second),</li>
 * </ul>
 *
 * @author   David Parsons
 * @version  1.0
 */
public class JPanelEditTime extends JPanelEdit {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -1541002811257405137L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    JTextField secFraction;

    /** permits whether or not to set this field. */
    private JCheckBox secCheckBox, minCheckBox, hourCheckBox;

    /** value to add to this time field. */
    private JComboBox secComboBox, minComboBox, hourComboBox;

    /** DOCUMENT ME! */
    private boolean xml = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * builds a panel containing 4 fields:
     *
     * <ul>
     *   <li>3 comboboxes (hour, minute, second),</LI>
     *   <li>1 text field (decimal fraction of a second),</li>
     * </ul>
     *
     * @param  initialText  the text which will fill the text field whn begun-- date string will be in hh:mm:ss.frac
     *                      format (anything to he right of a colon may be left unset)
     * @param  xml          DOCUMENT ME!
     */
    public JPanelEditTime(String initialText, boolean xml) {
        super();
        this.xml = xml;

        int i;
        hourCheckBox = new JCheckBox("Set Hour", false);
        minCheckBox = new JCheckBox("Set Minute", false);
        secCheckBox = new JCheckBox("Set Second", false);
        hourCheckBox.setFont(MipavUtil.font12);
        minCheckBox.setFont(MipavUtil.font12);
        secCheckBox.setFont(MipavUtil.font12);

        hourComboBox = new JComboBox();
        minComboBox = new JComboBox();
        secComboBox = new JComboBox();
        secFraction = new JTextField(5);
        hourComboBox.setFont(MipavUtil.font12);
        minComboBox.setFont(MipavUtil.font12);
        secComboBox.setFont(MipavUtil.font12);
        secFraction.setFont(MipavUtil.font12);
        secFraction.setHorizontalAlignment(JTextField.CENTER);

        // hour box
        for (i = 0; i < 24; i++) {
            hourComboBox.addItem(Integer.toString(i));
        }

        // minute and second box
        for (i = 0; i < 60; i++) {
            minComboBox.addItem(Integer.toString(i));
            secComboBox.addItem(Integer.toString(i));
        }

        setItemListeners();

        String hour, min, sec, frac;

        if (initialText.length() < 2) { // initial string is empty
            hourCheckBox.setSelected(false);
            hourComboBox.setEnabled(false);
            minCheckBox.setEnabled(false);
            minComboBox.setEnabled(false);
            secCheckBox.setEnabled(false);
            secComboBox.setEnabled(false);

            hourComboBox.setSelectedIndex(0);
            minComboBox.setSelectedIndex(0);
            secComboBox.setSelectedIndex(0);
        } else if (initialText.length() >= 2) { // contains hour

            char[] field = new char[2];
            field[0] = initialText.charAt(0);
            field[1] = initialText.charAt(1);
            hour = new String(field);

            hourCheckBox.setSelected(true);
            hourComboBox.setEnabled(true);
            minCheckBox.setEnabled(true);
            minComboBox.setEnabled(false);
            secCheckBox.setEnabled(false);
            secComboBox.setEnabled(false);

            hourComboBox.setSelectedIndex(Integer.parseInt(hour));

            if (initialText.length() >= 5) { // contains minutes
                field[0] = initialText.charAt(3);
                field[1] = initialText.charAt(4);
                min = new String(field);
                minCheckBox.setSelected(true);
                minComboBox.setEnabled(true);
                secCheckBox.setEnabled(true);
                secComboBox.setEnabled(false);

                minComboBox.setSelectedIndex(Integer.parseInt(min));

                if (initialText.length() >= 7) { // contains secs
                    field[0] = initialText.charAt(6);
                    field[1] = initialText.charAt(7);
                    sec = new String(field);
                    secCheckBox.setSelected(true);
                    secComboBox.setEnabled(true);
                    secFraction.setEnabled(true);

                    secComboBox.setSelectedIndex(Integer.parseInt(sec));

                    if (initialText.length() > 8) {
                        frac = new String(initialText.substring(9));
                        secFraction.setText(frac);
                    }
                }
            }
        }


        Box hourSetBox = new Box(BoxLayout.Y_AXIS);
        hourSetBox.add(hourCheckBox);
        hourSetBox.add(hourComboBox);
        this.add(hourSetBox);

        Box minSetBox = new Box(BoxLayout.Y_AXIS);
        minSetBox.add(minCheckBox);
        minSetBox.add(minComboBox);
        this.add(minSetBox);

        JTextField decimalPoint = new JTextField(".", 1);

        // decimalPoint.setFont(MipavUtil.font12);
        decimalPoint.setEditable(false);

        Box secSetBox = new Box(BoxLayout.Y_AXIS);
        secSetBox.add(secCheckBox);

        JPanel secFrac = new JPanel();
        secFrac.add(secComboBox);
        secFrac.add(decimalPoint);
        secFrac.add(secFraction);
        secSetBox.add(secFrac);
        this.add(secSetBox);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * If owner were to check, this panel always checks out fine. Ie., no check required
     *
     * @return  DOCUMENT ME!
     */
    public boolean checkFields() {
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
     * returns as much of the time as was set.
     *
     * @return  DOCUMENT ME!
     */
    public String getPanelValue() {
        String tmp;
        StringBuffer output = new StringBuffer("");

        if (hourCheckBox.isSelected()) {
            tmp = hourComboBox.getSelectedItem().toString();

            if (tmp.length() < 2) {
                output.append("0");
            }

            output.append(tmp);

            if (minCheckBox.isSelected()) {
                output.append(":");
                tmp = minComboBox.getSelectedItem().toString();

                if (tmp.length() < 2) {
                    output.append("0");
                }

                output.append(tmp);

                if (secCheckBox.isSelected()) {
                    output.append(":");
                    tmp = secComboBox.getSelectedItem().toString();

                    if (tmp.length() < 2) {
                        output.append("0");
                    }

                    output.append(tmp);
                    tmp = secFraction.getText();

                    if (!tmp.equals("")) {

                        if (xml) {
                            output.append("-");
                        } else {
                            output.append(".");
                        }

                        output.append(tmp);
                    }
                }
            }
        }

        return output.toString();
    }

    /**
     * gets the internal components to listen to themselves for panel self-verification.
     */
    private void setItemListeners() {
        hourCheckBox.addItemListener(new ItemListener() {
                public void itemStateChanged(ItemEvent e) {

                    if (hourCheckBox.isSelected()) {
                        hourComboBox.setEnabled(true);
                        minCheckBox.setEnabled(true);

                        if (minCheckBox.isSelected()) {
                            minComboBox.setEnabled(true);
                            secCheckBox.setEnabled(true);

                            if (secCheckBox.isSelected()) {
                                secComboBox.setEnabled(true);
                                secFraction.setEnabled(true);
                            } else {
                                secComboBox.setEnabled(false);
                                secFraction.setEnabled(false);
                            }
                        } else {
                            minComboBox.setEnabled(false);
                            secCheckBox.setEnabled(false);
                            secComboBox.setEnabled(false);
                            secFraction.setEnabled(false);
                        }
                    } else {
                        hourComboBox.setEnabled(false);
                        minCheckBox.setEnabled(false);
                        minComboBox.setEnabled(false);
                        secCheckBox.setEnabled(false);
                        secComboBox.setEnabled(false);
                        secFraction.setEnabled(false);
                    }
                }
            });
        minCheckBox.addItemListener(new ItemListener() {
                public void itemStateChanged(ItemEvent e) {

                    if (minCheckBox.isSelected()) {
                        minComboBox.setEnabled(true);
                        secCheckBox.setEnabled(true);

                        if (secCheckBox.isSelected()) {
                            secComboBox.setEnabled(true);
                            secFraction.setEnabled(true);
                        } else {
                            secComboBox.setEnabled(false);
                            secFraction.setEnabled(false);
                        }
                    } else {
                        minComboBox.setEnabled(false);
                        secCheckBox.setEnabled(false);
                        secComboBox.setEnabled(false);
                        secFraction.setEnabled(false);
                    }
                }
            });
        secCheckBox.addItemListener(new ItemListener() {
                public void itemStateChanged(ItemEvent e) {

                    if (secCheckBox.isSelected()) {
                        secComboBox.setEnabled(true);
                        secFraction.setEnabled(true);
                    } else {
                        secComboBox.setEnabled(false);
                        secFraction.setEnabled(false);
                    }
                }
            });

        // protect the set fraction text box
        secFraction.addKeyListener(new KeyAdapter() { // make the second-fraction field not accept letters
                public void keyTyped(KeyEvent evt) {
                    char ch = evt.getKeyChar();

                    if (((ch != KeyEvent.VK_BACK_SPACE) && (ch < '0')) || (ch > '9')) { // key is not a digit
                        evt.consume();
                    } else if (ch != KeyEvent.VK_BACK_SPACE) {

                        try {
                            StringBuffer fieldText = new StringBuffer(secFraction.getText());

                            if (fieldText.append(ch).length() > 6) {
                                evt.consume();
                            }
                        } catch (NullPointerException ex) {
                            // do nothing -- ignore
                        }
                    }
                }
            });

    }
} // end of JPanelEditTime
