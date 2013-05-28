package gov.nih.mipav.view;


import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * panel contains 3 fields: 2 comboboxes (day, month), 1 text field (year), panel handles proper day/month and leap-year
 * input.
 *
 * @author   David Parsons
 * @version  1.0
 */
public class JPanelEditDate extends JPanelEdit {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 3504702516257604573L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JComboBox dayCombobox, monthCombobox;

    /** DOCUMENT ME! */
    private boolean noFutureDates = false; // allow no dates before today.

    /** DOCUMENT ME! */
    private boolean noPastDates = false; // allow no dates prior-to today.

    /** DOCUMENT ME! */
    private boolean xml = false; // xml formatted dates

    /** DOCUMENT ME! */
    private JTextField yearTextField;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Builds the panel with 3 fields. It is necessary to validate these fields before accepting the values.
     *
     * @param  initialText  the text which will fill the text field whn begun-- date string will be in mm/dd/yyyy format
     * @param  xml          DOCUMENT ME!
     */
    public JPanelEditDate(String initialText, boolean xml) {
        super();
        this.xml = xml;

        Calendar cal;
        int i;
        int month = 0;
        int day = 0;
        int year = 0;
        dayCombobox = new JComboBox();
        dayCombobox.setFont(MipavUtil.font12);
        monthCombobox = new JComboBox();
        monthCombobox.setFont(MipavUtil.font12);
        yearTextField = new JTextField(6); // year field may only have 4 numbers
        yearTextField.setHorizontalAlignment(JTextField.CENTER);
        yearTextField.setFont(MipavUtil.font12);

        for (i = 1; i < 32; i++) {
            dayCombobox.addItem(Integer.toString(i)); // include all 31 days of any month
        }

        dayCombobox.setEditable(false);

        // add the months of the year
        monthCombobox.addItem("January");
        monthCombobox.addItem("February");
        monthCombobox.addItem("March");
        monthCombobox.addItem("April");
        monthCombobox.addItem("May");
        monthCombobox.addItem("June");
        monthCombobox.addItem("July");
        monthCombobox.addItem("August");
        monthCombobox.addItem("September");
        monthCombobox.addItem("October");
        monthCombobox.addItem("November");
        monthCombobox.addItem("December");
        monthCombobox.setEditable(false);

        if (xml) {
            StringTokenizer date = new StringTokenizer(initialText, "-");

            if (date.hasMoreElements()) {
                year = Integer.parseInt(date.nextToken());
                month = Integer.parseInt(date.nextToken());
                day = Integer.parseInt(date.nextToken());
            }
        } else {
            StringTokenizer date = new StringTokenizer(initialText, "/");

            if (date.hasMoreElements()) {
                month = Integer.parseInt(date.nextToken());
                day = Integer.parseInt(date.nextToken());
                year = Integer.parseInt(date.nextToken());
            } else { // date does not exist (it is "").  Give numbers a value to prevent
                     // throwing an exception.  at a later date we might allow for 'no-value dates'

                cal = Calendar.getInstance();

                month = cal.get(Calendar.MONTH) + 1;
                day = cal.get(Calendar.DAY_OF_MONTH);
                year = cal.get(Calendar.YEAR);
            }
        }

        if ((year <= 0) || (month <= 0) || (day <= 0)) {
            cal = Calendar.getInstance();
            month = cal.get(Calendar.MONTH) + 1;
            day = cal.get(Calendar.DAY_OF_MONTH);
            year = cal.get(Calendar.YEAR);
        }

        monthCombobox.setSelectedIndex(month - 1);
        dayCombobox.setSelectedIndex(day - 1);
        yearTextField.setText(String.valueOf(year));

        this.add(dayCombobox);
        this.add(monthCombobox);
        this.add(yearTextField);
        monthCombobox.addItemListener(new ItemListener() {
                public void itemStateChanged(ItemEvent e) {
                    int Year = Integer.parseInt(yearTextField.getText());
                    int numberOfDays = dayCombobox.getItemCount();
                    int currentMonth = monthCombobox.getSelectedIndex() + 1;

                    updatePanel(numberOfDays, currentMonth, Year);
                }
            });

        // protect the year text box // just like makeNumericsOnly methods spread around the project
        yearTextField.addKeyListener(new KeyAdapter() { // make the year field not accept letters
                public void keyTyped(KeyEvent evt) {
                    char ch = evt.getKeyChar();

                    if (((ch < '0') || (ch > '9')) && (ch != KeyEvent.VK_DELETE) && (ch != KeyEvent.VK_BACK_SPACE) &&
                            (ch != KeyEvent.VK_ENTER)) { // key is not a digit
                        evt.consume();
                    } else {

                        try {
                            StringBuffer fieldText = new StringBuffer(yearTextField.getText());

                            if (fieldText.append(ch).length() == 4) {
                                int Year = Integer.parseInt(fieldText.toString());
                                int numberOfDays = dayCombobox.getItemCount();
                                int currentMonth = monthCombobox.getSelectedIndex() + 1;
                                updatePanel(numberOfDays, currentMonth, Year);
                            }
                        } catch (NullPointerException ex) {
                            // do nothing -- ignore
                        } catch (NumberFormatException nfe) { // most likely to handle adding the

                            // do nothing -- ignore
                        }
                    }
                }

            });

        // preset the comboboxes to the original date
        updatePanel(dayCombobox.getItemCount(), month, year); // index 0 is month 01; index 0 is day 01;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Because DICOM requires 4-digit years, we must check the year field. If the year-text has fewer or more than 4
     * digits, the field must be fixed. Any year with 4 digits is okay.
     *
     * <p>If there is an error, the errorString is set to describe the problem and errorComponent is set to point to the
     * JComponent that had an illegal value. This method then returns <code>false</code>. When processed normally, the
     * caller finds a <code>true</code>, and the errorString and errorComponent are set to <code>null</code> to prevent
     * undesired consequences.</p>
     *
     * @return  boolean FieldsOKAY
     *
     * @see     MipavUtil#displayError(String)
     */
    public boolean checkFields() {

        if (yearTextField.getText().length() != 4) {
            errorString = "Year must have 4 digits!";
            errorComponent = yearTextField;

            return false; // field must be re-entered
        } else if (monthCombobox.getSelectedIndex() == -1) {
            errorString = "Please select a month!";
            errorComponent = monthCombobox;

            return false;
        } else if (dayCombobox.getSelectedIndex() == -1) {
            errorString = "Please select a day!";
            errorComponent = dayCombobox;

            return false;
        } else {
            errorString = null;
            errorComponent = null;

            return true; // field is fine.  process normally
        }
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
     * concatonates the date as MM/DD/YYYY; padding any years < 1000 with leading zeros.
     *
     * @return  DOCUMENT ME!
     */
    public String getPanelValue() {
        StringBuffer yearField = new StringBuffer(yearTextField.getText());

        while (yearField.length() < 4) { // pad with zeroes
            yearField.insert(0, '0');
        }

        if (xml) {
            String temp;
            int m = monthCombobox.getSelectedIndex() + 1;
            String day = (String) dayCombobox.getSelectedItem();
            int d = new Integer(day).intValue();
            temp = yearField.toString() + "-";

            if (m < 10) {
                temp += "0";
            }

            temp += new Integer(m).toString() + "-";

            if (d < 10) {
                temp += "0";
            }

            temp += new Integer(d).toString();

            return temp;
        } else {
            return ((String.valueOf(monthCombobox.getSelectedIndex() + 1)) + "/" +
                    (String) dayCombobox.getSelectedItem() + "/" + yearField.toString());
        }
    }

    /**
     * accessor to find out whether the panel is set to restrict the panel-display from displaying dates in the future.
     *
     * @return  boolean whether the user may select a date in the future.
     */
    public boolean restrictNoFutureDates() {
        return noFutureDates;
    }


    /**
     * sets the panel use the restriction that it is to allow no dates after today.
     *
     * @param  permission  to display dates in the future. <code>true</code> prevents the panel from displaying a date
     *                     in the future; <code>false</code> (the default) permits selection of a date in the future.
     */
    public void restrictNoFutureDates(boolean permission) {
        noFutureDates = permission;
    }


    /**
     * accessor to find out whether the panel is set to restrict the panel-display from displaying dates in the past.
     *
     * @return  boolean whether the user may select a date in the past.
     */
    public boolean restrictPastDates() {
        return noPastDates;
    }


    /**
     * sets the panel use the restriction that it is to allow no dates prior to today.
     *
     * @param  permission  to display dates in the past. <code>true</code> prevents the panel from displaying a date in
     *                     the past; <code>false</code> (the default) permits selection of a date in the past.
     */
    public void restrictPastDates(boolean permission) {
        noPastDates = permission;
    }

    /**
     * self-verification of panel values, prior to requesting for checkFields. In fact, so that it is not necessary to
     * explicitly verify the value.
     *
     * @param  numberOfDays  DOCUMENT ME!
     * @param  currentMonth  DOCUMENT ME!
     * @param  year          DOCUMENT ME!
     */
    private void updatePanel(int numberOfDays, int currentMonth, int year) {
        int i;
        int selectedDay = dayCombobox.getSelectedIndex() + 1;

        // when the the calling class requests we prevent selecting future dates we check
        // if the selected date is in the future, we will reset year/month/day, of
        // whichever are in the future. (ie., month is future, ordinal day is past, so
        // date becomes, this month, selected day.; today is 6 june; selected is 4 august,
        // date becomes 4 june.)
        if (noFutureDates) {

            if (year >= Calendar.getInstance().get(Calendar.YEAR)) {
                year = Calendar.getInstance().get(Calendar.YEAR);

                if (currentMonth >= Calendar.getInstance().get(Calendar.MONTH)) {
                    currentMonth = Calendar.getInstance().get(Calendar.MONTH);

                    if (selectedDay >= Calendar.getInstance().get(Calendar.DAY_OF_MONTH)) {
                        selectedDay = Calendar.getInstance().get(Calendar.DAY_OF_MONTH);
                    }
                }
            }
        }

        // when the the calling class requests we prevent selecting past dates we check
        // if the selected date is in the past, we will reset year/month/day, of
        // whichever are in the past. (ie., month is past, ordinal day is future, so
        // date becomes, this month, selected day; today is 6 june; selected is 10 may,
        // date becomes 10 june.)
        if (noPastDates) {

            if (year <= Calendar.getInstance().get(Calendar.YEAR)) {
                year = Calendar.getInstance().get(Calendar.YEAR);

                if (currentMonth <= Calendar.getInstance().get(Calendar.MONTH)) {
                    currentMonth = Calendar.getInstance().get(Calendar.MONTH);

                    if (selectedDay <= Calendar.getInstance().get(Calendar.DAY_OF_MONTH)) {
                        selectedDay = Calendar.getInstance().get(Calendar.DAY_OF_MONTH);
                    }
                }
            }
        }

        dayCombobox.setSelectedIndex(0);

        if (currentMonth == 2) { // february

            if (((year % 4) == 0) && (((year % 100) != 0) || ((year % 400) == 0))) { // year is a leap year

                // make the number of days 29!!
                if (numberOfDays == 28) { // this month is feb with only 28 days
                    dayCombobox.addItem(String.valueOf(29)); // add feb 29
                } else { // other months have 30/31 days.  remove those extra days

                    for (i = numberOfDays - 1; i >= 29; i--) { // i uses index: index = day+1
                        dayCombobox.removeItemAt(i);
                    }
                }

                numberOfDays = 29;
            } else {

                // not leap year--make the number of days 28!!!
                for (i = numberOfDays - 1; i >= 28; i--) {
                    dayCombobox.removeItemAt(i); // index = day-1: remove 29, 30, or 31
                }

                numberOfDays = 28;
            }
        } else if ((currentMonth == 4) || (currentMonth == 6) || (currentMonth == 9) || (currentMonth == 11)) {

            // make the number of days 30!!
            if (numberOfDays > 30) {
                dayCombobox.removeItemAt(30); // index(day=31) = 30
            } else if (numberOfDays < 30) {

                for (i = numberOfDays + 1; i <= 30; i++) {
                    dayCombobox.addItem(String.valueOf(i));
                }
            }

            numberOfDays = 30;
        } else if ((currentMonth == 1) || (currentMonth == 3) || (currentMonth == 5) || (currentMonth == 7) ||
                       (currentMonth == 8) || (currentMonth == 10) || (currentMonth == 12)) {

            // make the number of days 31!!
            for (i = numberOfDays + 1; i <= 31; i++) {
                dayCombobox.addItem(String.valueOf(i));
            }

            numberOfDays = 31;
        }

        // set the highlighted day to last-day-of-the-month if it had been something that isn't in the current month
        if (selectedDay > numberOfDays) { // ie., was 31 May; switched to April; so reset day from 31 to 30.
            dayCombobox.setSelectedIndex(numberOfDays - 1); // index+1 = day.... set to highest allowable day
        } else {
            dayCombobox.setSelectedIndex(selectedDay - 1); // leave the selectedday the same as before
        }


    }

}
