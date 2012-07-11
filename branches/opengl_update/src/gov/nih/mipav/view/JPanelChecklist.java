package gov.nih.mipav.view;


import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.border.*;


/**
 * Creates a JPanel to hold a scrolling list of checkboxes. Extending class must include implementation of the method
 * <code>makeCheckBoxList()</code>.
 */
public abstract class JPanelChecklist extends JPanel implements ActionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6168461302590312089L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** must be same size as listLength. */
    protected String[] checkboxLabels;

    /** DOCUMENT ME! */
    protected JCheckBox[] checkboxList;

    /** DOCUMENT ME! */
    protected JPanel checkboxPanel;

    /** DOCUMENT ME! */
    protected JButton checkButton; // dialog button to set all checks to TRUE (checked-TRUE means 'expurgate this bit')

    /** DOCUMENT ME! */
    protected JPanel chkUnchkPanel;

    /** must be given a nonzero number on instantiation. must be of same size as checkboxLabels. */
    protected int listLength = 0;

    /**
     * notice that for the next two lists, ith entry on one corresponds to the ith entry on the other. the entire list
     * of choices possible.
     */
    protected boolean[] removeList;

    /** DOCUMENT ME! */
    protected JScrollPane scrollPane;

    /** DOCUMENT ME! */
    protected JButton unCheckButton; // dialog button to set all checks to FALSE

    /** the list of the choices that can be selected. */
    protected boolean[] enabledList;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor to build a panel allowing user to find which tags are available to anonymize.
     *
     * <p>By defualt, sets all checkboxes to enabled. To set the checkboxes to image-specific enabled, use setDicomInfo.
     * </p>
     *
     * @see  JPanelAnonymizeImage#setDicomInfo(FileInfoDicom)
     */
    public JPanelChecklist() {
        super(true);
        // set a titled border
        // call init() to create the layout
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and does the routine.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource(); // whatever the user clicked on

        if (source == checkButton) {
            setSelectedList(true);
        } else if (source == unCheckButton) {
            setSelectedList(false);
        }
    }

    /**
     * DOCUMENT ME!
     */
    public void buildLayout() {
        this.setLayout(new BorderLayout());

        // make the list scroll if there are enough checkboxes
        scrollPane = new JScrollPane(makeCheckContainer(), // checkboxPanel,
                                     JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                     JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollPane.setForeground(Color.white);
        add(scrollPane, BorderLayout.CENTER);
        add(chkUnchkPanel = makeCheckUncheckPanel(), BorderLayout.SOUTH);
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getCheckboxLength() {
        return checkboxList.length;
    } // unless extended, should be same size as list length

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getListLength() {
        return checkboxLabels.length;
    } // should be same size as list length

    /**
     * Returns a new list of the tag names of all checkboxes selected.
     *
     * @return  String[] array of tag names
     */
    public String[] getNameList() {
        int numSelected = getNumberSelected();
        String[] list = new String[numSelected];

        for (int i = 0, item = 0; i < checkboxList.length; i++) {

            if (checkboxList[i].isSelected()) {
                list[item++] = checkboxList[i].getText();
            }
        }

        return list;
    }

    /**
     * This method finds the number of checked checkboxes in the list.
     *
     * @return  int numChecked
     */
    public int getNumberSelected() {
        int i;
        int numChecked = 0;

        for (i = 0; i < checkboxList.length; i++) {

            if (checkboxList[i].isSelected()) {
                numChecked++;
            }
        }

        return (numChecked);
    }

    /**
     * This method finds the number of enabled checkboxes in the list.
     *
     * @return  int numChecked
     */
    public int getNumberVisible() {
        int i;
        int numvis = 0;

        for (i = 0; i < enabledList.length; i++) {

            if (enabledList[i]) {
                numvis++;
            }
        }

        return (numvis);
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getRemoveLength() {
        return removeList.length;
    } // unless extended, should be same size as list length

    /**
     * Returns a new list of all checkboxes selected.
     *
     * @return  boolean[] array containing selection status
     */
    public boolean[] getSelectedList() {
        boolean[] list = new boolean[listLength];

        for (int i = 0; i < listLength; i++) {
            list[i] = checkboxList[i].isSelected();
        }

        return list;
    }

    /**
     * Returns whether or not a particular checkbox has been selected.
     *
     * @param   i  the index of the checkbox in question
     *
     * @return  boolean status of checkbox
     */
    public boolean getSelectedList(int i) {

        try {
            return checkboxList[i].isSelected();
        } catch (ArrayIndexOutOfBoundsException aioobe) {
            throw aioobe;
        } catch (NullPointerException npe) {
            throw npe;
        }
    }

    /**
     * Returns whether or not a particular checkbox has been selected.
     *
     * @param      label  the text of the checkbox that we wish to know about.
     *
     * @exception  IllegalArgumentException  is thrown when <code>label</code> is is not found in the list of check
     *                                       boxes.
     *
     * @return     boolean status of checkbox
     */
    public boolean getSelectedList(String label) {
        int i = 0;

        try {

            for (i = 0; !label.equals(checkboxList[i].getText()); i++) { /* everything is done in the loop condition! */
            }

            return checkboxList[i].isSelected();
        } catch (ArrayIndexOutOfBoundsException aioobe) {
            throw new IllegalArgumentException("\"" + label + "\" not found in checklist.");
        } catch (NullPointerException npe) {
            throw npe;
        }
    }

    /**
     * Gets the values of which boxes are visible.
     *
     * @return  boolean[] visibleTags
     */
    public boolean[] getVisible() {
        return enabledList;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getVisibleLength() {
        return enabledList.length;
    } // unless extended, should be same size as list length

    /**
     * DOCUMENT ME!
     *
     * @param  title  DOCUMENT ME!
     */
    public void setBorder(String title) {
        TitledBorder border = new TitledBorder(title);
        border.setTitleColor(Color.black);
        border.setTitleFont(MipavUtil.font12B);
        border.setBorder(new EtchedBorder()); // give the border a grooved look
        this.setBorder(border);
    }

    /**
     * Set the checkboxes to be clickable. Only useful in applications where the tags which are available to be used are
     * known before-hand.
     *
     * <p>The visible tags must be set first.</p>
     */
    public void setCheckBoxesEnabled() {
        int enabledCount = 0;

        // make checkbox enabled only if the box is visible.
        for (int i = 0; i < enabledList.length; i++) {

            if ((enabledList[i])) {
                checkboxList[i].setEnabled(true);
                enabledCount++; // count how many are visible
            } else { // otherwise, uncheck those that cannot be checked.
                checkboxList[i].setSelected(false);
                checkboxList[i].setEnabled(false);
            }
        }

        // if there are no fields which the user may select,
        // turn off the check-all/uncheck-all buttons
        boolean checkUncheckEnabled = false;

        if (enabledCount > 0) { // if any tag is visible

            // let user activate the check-all/uncheck-all buttons
            checkUncheckEnabled = true;
        }

        // set the status of the check-all/uncheck-all buttons as decided above
        unCheckButton.setEnabled(checkUncheckEnabled);
        checkButton.setEnabled(checkUncheckEnabled);
    }

    /**
     * Set the checkboxes to be clickable. Only useful in applications where the tags which are availble to be used are
     * known before-hand.
     *
     * <p>The visible tags must be set first.</p>
     *
     * @param  i        the index of the checkbox in question
     * @param  enabled  DOCUMENT ME!
     */
    public void setCheckBoxesEnabled(int i, boolean enabled) {

        try {
            checkboxList[i].setEnabled(enabled);
        } catch (NullPointerException npe) {
            throw npe;
        } catch (ArrayIndexOutOfBoundsException aioobe) {
            throw aioobe;
        }
    }

    /**
     * Set the checkboxes to be clickable. Only useful in applications where the tags which are availble to be used are
     * known before-hand.
     *
     * <p>The visible tags must be set first.</p>
     *
     * @param      label    the text of the checkbox in question
     * @param      enabled  DOCUMENT ME!
     *
     * @exception  IllegalArgumentException  is thrown when <code>label</code> is not found in any of the checkboxes
     */
    public void setCheckBoxesEnabled(String label, boolean enabled) {
        int i;

        try {

            for (i = 0; !label.equals(checkboxList[i].getText()); i++) { /* everything is done in the loop condition! */
            }

            checkboxList[i].setEnabled(enabled);
        } catch (NullPointerException npe) {
            throw npe;
        } catch (ArrayIndexOutOfBoundsException aioobe) {
            throw new IllegalArgumentException("\"" + label + "\" not found in checkbox list.");
        }
    }

    /**
     * Set the the checkbox list from the script dialog.
     *
     * @param  _list  checkbox select list array
     */
    public void setSelectedList(boolean[] _list) {

        for (int i = 0; i < listLength; i++) {
            checkboxList[i].setSelected(_list[i]);
        }
    }

    /**
     * Sets whether or not aall checkboxes have been selected.
     *
     * @param      isSet  the preselected value of all checkboxes in the list.
     *
     * @exception  NullPointerException  if no checkboxes exist when called
     *
     * @see        JPanelAnonymizeImage#getSelectedList()
     */
    public void setSelectedList(boolean isSet) {

        for (int i = 0; i < enabledList.length; i++) {

            if (enabledList[i]) {

                try {
                    checkboxList[i].setSelected(isSet);
                } catch (NullPointerException npe) {
                    throw npe;
                }
            }
        }
    }

    /**
     * Sets the boxes which are enabled.
     *
     * @param  list  DOCUMENT ME!
     */
    public void setEnabledList(boolean[] list) {
        enabledList = list;
    }

    /**
     * Sets whether or not all checkboxes are enabled.
     *
     * @param      isSet  the preselected value of all checkboxes in the list.
     *
     * @exception  NullPointerException  if no checkboxes exist when called
     *
     * @see        JPanelAnonymizeImage#getSelectedList()
     */
    public void setEnabledList(boolean isSet) {

        for (int i = 0; i < enabledList.length; i++) {

            try {
                enabledList[i] = isSet;
            } catch (NullPointerException npe) {
                throw npe;
            } catch (ArrayIndexOutOfBoundsException aioobe) {
                throw aioobe;
            }
        }
    }

    /**
     * Sets a particular box which to be visible.
     *
     * @param  i        the index of the checkbox in question
     * @param  visible  whether the checkbox should be enabled
     */
    public void setEnabledList(int i, boolean visible) {

        try {
            enabledList[i] = visible;
        } catch (NullPointerException npe) {
            throw npe;
        } catch (ArrayIndexOutOfBoundsException aioobe) {
            throw aioobe;
        }
    }
    
    /**
     * Sets a particular box which to be visible by specifying the name of the label.
     *
     * @param  i        the index of the checkbox in question
     * @param  visible  whether the checkbox should be enabled
     */
    public boolean setEnabledList(String label, boolean visible) {
        for(int i=0; i<checkboxLabels.length; i++) {
            if(label.equals(checkboxLabels[i])) {
                enabledList[i] = visible;
                return true;
            }
        }
        return false; //label was not found
    }

    /**
     * Creates the list of labels to use in the checkboxes.
     *
     * @return  DOCUMENT ME!
     */
    protected abstract String[] makeCheckboxLabels();

    /**
     * Abstract to ensure the variable <code>listLength</code> has a value.
     */
    protected abstract void setListLength();

    /**
     * Builds all checkboxes; prepare for this step by having an array of Strings with the names to use. Called <code>
     * checkboxLabels</code> one for each of the dicom tags that may be sanitized. Also sets the properties of all check
     * boxes.
     *
     * @return  JCheckBox[] array of new check boxes
     */
    protected JCheckBox[] makeCheckBoxList() {

        // start by instantiating the boolean arrays:
        removeList = new boolean[listLength];
        enabledList = new boolean[listLength];

        JCheckBox[] list = new JCheckBox[listLength]; // selector for the user to choose which slices to remove.  TRUE
                                                      // means remove.

        try {

            for (int i = 0; i < listLength; i++) {
                list[i] = new JCheckBox(checkboxLabels[i]); // just uses name
                list[i].setBackground(Color.white);
            }
        } catch (ArrayIndexOutOfBoundsException aioobe) {
            throw aioobe;
        } catch (NullPointerException npe) {
            throw npe;
        }

        // set checkbox properties ....
        for (int i = 0; i < listLength; i++) {
            list[i].setFont(MipavUtil.font12);
        }

        return list;
    }

    /**
     * Builds the display of the container which holds the display of checkboxes. This box can then be displayed
     * anywhere.
     *
     * @return  Container the new container
     */
    protected Container makeCheckContainer() {
        JPanel chkboxBox = new JPanel();
        chkboxBox.setLayout(new BoxLayout(chkboxBox, BoxLayout.Y_AXIS));
        checkboxList = makeCheckBoxList();

        // place tagCount of check options for user and give them a name:
        for (int i = 0; i < listLength; i++) {
            setEnabledList(i, true);
            setCheckBoxesEnabled(i, true);
            chkboxBox.add(checkboxList[i]);
        }

        chkboxBox.setForeground(Color.white);
        chkboxBox.setBackground(Color.white);

        return chkboxBox;
    }

    /**
     * Builds the panel with the &quotcheck all&quot;&#47;&quot;uncheck all&quot; buttons.
     *
     * @return  JPanel the new panel
     */
    protected JPanel makeCheckUncheckPanel() {
        JPanel panel = new JPanel();

        // make check & uncheck buttons for the panel--place inside the above border
        checkButton = new JButton("Select all");
        checkButton.setFont(MipavUtil.font12B);
        checkButton.setPreferredSize(new Dimension(85, 30));
        panel.add(checkButton, BorderLayout.WEST);
        checkButton.addActionListener(this);

        unCheckButton = new JButton("Clear");
        unCheckButton.setFont(MipavUtil.font12B);
        unCheckButton.setPreferredSize(new Dimension(85, 30));
        unCheckButton.addActionListener(this);
        panel.add(unCheckButton, BorderLayout.EAST);

        return panel;
    }

}
