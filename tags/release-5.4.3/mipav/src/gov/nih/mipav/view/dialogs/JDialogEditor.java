package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;

import javax.swing.*;


/**
 * Simple editor using JPanelEdits. Goal for 1.1 is to make JDialogDICOMTagEditor a subclass of this.
 */
public class JDialogEditor extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 1979336171963042575L;

    /** DOCUMENT ME! */
    public static final int CHAR = 0;

    /** DOCUMENT ME! */
    public static final int STRING = 1;

    /** DOCUMENT ME! */
    public static final int INT_STRING = 2;

    /** DOCUMENT ME! */
    public static final int FLOAT_STRING = 3;

    /** DOCUMENT ME! */
    public static final int ANALYZE_DATATYPE = 4; // see JPanelEditDatatype (drop down)

    /** DOCUMENT ME! */
    public static final int ANALYZE_ORIENTATION = 5; // see JPanelEditOrientation (drop down)

    /** DOCUMENT ME! */
    public static final int ANALYZE_DESCRIPTION = 6; // 80 CHAR max text field

    /** DOCUMENT ME! */
    public static final int ANALYZE_ORIGINATOR = 7; // 10 char max string

    /** DOCUMENT ME! */
    public static final int ANALYZE_AUX = 8; // 24 char max string

    /** DOCUMENT ME! */
    public static final int ANALYZE_DBNAME = 9; // 18 char max string

    /** DOCUMENT ME! */
    public static final int ANALYZE_VOX = 10; // 4  char max string

    /** DOCUMENT ME! */
    public static final int ANALYZE_CAL = 11; // 8  char max string

    /** DOCUMENT ME! */
    public static final int ANALYZE_AXIS_ORIENTATION = 12; // see JPanelEditAxisOrientation (drop down) // this is a
                                                           // MIPAV hack to the analyze format to overcome a difficulty
                                                           // of display.  (uses the unused variables)

    /** DOCUMENT ME! */
    public static final int IMAGE_ORIENTATION = 13; // JPanelEditImageOrientation (drop down)

    /** DOCUMENT ME! */
    public static final int XML_MODALITY = 14;

    /** DOCUMENT ME! */
    public static final int XML_RACE = 15;

    /** DOCUMENT ME! */
    public static final int XML_DOB = 16;

    /** DOCUMENT ME! */
    public static final int XML_SEX = 17;

    /** DOCUMENT ME! */
    public static final int XML_DATE = 18;

    /** DOCUMENT ME! */
    public static final int XML_TIME = 19;

    /** DOCUMENT ME! */
    public static final int XML_VALUETYPE = 20;

    /** DOCUMENT ME! */
    public static final int XML_SETORPARAM = 21;

    /** DOCUMENT ME! */
    public static final int XML_LINKEDIMAGE = 22;

    /** DOCUMENT ME! */
    public static final int BOOLEAN = 23;

    /** DOCUMENT ME! */
    public static final int FILE_STRING = 24;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected boolean closedOkay = false; // okay button closed the dialog; no errors.

    /** the representative key of this object; must be unique. */
    protected Object key;

    /** DOCUMENT ME! */
    protected JPanelEdit[] newValue;

    /** DOCUMENT ME! */
    protected JTextField oldValue;

    /** DOCUMENT ME! */
    private String imageFileName; // used for editing images associated with a project

    /** DOCUMENT ME! */
    private String psetDesc;

    /** DOCUMENT ME! */
    private int row;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * makes the editor with a simple JPanelEditDefault.
     *
     * @param  owner       DOCUMENT ME!
     * @param  tag         DOCUMENT ME!
     * @param  old         DOCUMENT ME!
     * @param  editorType  DOCUMENT ME!
     */

    /*   public JDialogEditor(Object tag, String old) {
     * super(); getContentPane().setLayout(new BorderLayout()); this.key = tag; Box values = new Box(BoxLayout.Y_AXIS);
     * JPanel oldPanel = new JPanel(); oldPanel.setBorder(buildTitledBorder("Original Value")); oldValue = new
     * JTextField(old); oldValue.setColumns(32); oldValue.setEditable(false); oldValue.setBackground(Color.lightGray);
     * oldPanel.add(oldValue); oldPanel.add(oldValue); values.add(oldPanel); newValue = new JPanelEdit[1]; newValue[0] =
     * new JPanelEditDefault(old); newValue[0].setBorder(buildTitledBorder("New Value")); values.add(newValue[0]);
     * this.getContentPane().add(values); this.getContentPane().add(buildOkayCancelPanel(), BorderLayout.SOUTH); pack();
     * // does not setVisible(true); }*/

    /**
     * sadly, the dialog doesn't know how to figure out what editor panels to apply to the different inputs, so old
     * values must be sent as an array of objects.
     *
     * @param  owner       an array of Object that is the coded value for each of the values. The Old values will be
     *                     printed out seperated by spaces.
     * @param  tag         a unique identifier for this editor (use getKey())
     * @param  old         DOCUMENT ME!
     * @param  editorType  an array specifying how each of the old values are to be edited. Ie.,
     */
    public JDialogEditor(Dialog owner, Object tag, Object[] old, int[] editorType) {
        super(owner, false);
        getContentPane().setLayout(new BorderLayout());
        this.key = tag;

        Box values = new Box(BoxLayout.Y_AXIS);
        JPanel oldPanel = new JPanel();

        oldPanel.setBorder(buildTitledBorder("Original Value"));
        // make the list of old values into a string to display

        String theOldValue = null;

        try {
            theOldValue = old[0].toString();

            for (int i = 1; i < old.length; i++) {
                theOldValue += " " + old[i].toString();
            }
        } catch (NullPointerException nex) {
            theOldValue = new String("");
        }

        oldValue = new JTextField(theOldValue);
        oldValue.setColumns(32);
        oldValue.setEditable(false);
        oldValue.setBackground(Color.lightGray);
        oldValue.setFont(MipavUtil.font12);
        oldPanel.add(oldValue);
        values.add(oldPanel);

        Box editBox = new Box(BoxLayout.Y_AXIS);
        Box newInputPanelHolder = new Box(BoxLayout.Y_AXIS);

        newValue = new JPanelEdit[old.length];

        for (int i = 0; i < old.length; i++) {
            newValue[i] = makeAppropriateInputPanel((String) old[i], editorType[i]);
            newInputPanelHolder.add(newValue[i]);
        }

        JScrollPane inputPanelScroller = new JScrollPane(newInputPanelHolder);

        inputPanelScroller.setBorder(buildTitledBorder("New Value"));
        editBox.add(inputPanelScroller);
        values.add(editBox);
        this.getContentPane().add(values);

        this.getContentPane().add(buildOkayCancelPanel(), BorderLayout.SOUTH);
        pack();
        // does not setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param  ae  DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent ae) {
        String command = ae.getActionCommand();

        if (command.equalsIgnoreCase("ok")) {

            if (!isDialogOkay()) { // if there are no problems, this whole method is skipped

                for (int j = 0; j < newValue.length; j++) {

                    if (!newValue[j].checkFields()) { // trap first error

                        try { // original code found in JDialogEditDICOMtags
                            MipavUtil.displayError(newValue[j].getErrorString());
                            newValue[j].getErrorComponent().requestFocus();
                            closedOkay = false;

                            return;
                        } catch (NullPointerException npe) {
                            MipavUtil.displayError("Edit Error Occured.  Edit canceled.");
                            dispose();

                            return;
                        }
                    }
                }
            }

            // hide & set flags
            closedOkay = true;
            setVisible(false);
            dispose();
        } else if (command.equalsIgnoreCase("cancel")) {
            dispose();
        }
    }

    /**
     * this editor's okay editable value.
     *
     * <p>all output strings are placed together, seperated by a space.</p>
     *
     * <p>It might be required to subclass this method to have the seperator character be something else.</p>
     *
     * @return  DOCUMENT ME!
     */
    public String getDisplayValue() {
        String outputString = newValue[0].getPanelValue();

        for (int i = 1; i < newValue.length; i++) {
            outputString += " " + newValue[i].getPanelValue();
        }

        return outputString;
    }

    /**
     * used when editing images associated with a project.
     *
     * @return  DOCUMENT ME!
     */
    public String getDisplayValueForInfo() {
        String outputString = newValue[0].getPanelValue();

        for (int i = 1; i < newValue.length; i++) {
            outputString += "####" + newValue[i].getPanelValue();
        }

        return outputString;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getDisplayValueForParam() {
        String outputString = newValue[0].getPanelValue();

        for (int i = 1; i < newValue.length; i++) {
            outputString += "\\" + newValue[i].getPanelValue();
        }

        return outputString;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getImageFileName() {
        return this.imageFileName;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Object getKey() {
        return this.key;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String getPSetDescription() {
        return this.psetDesc;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getRow() {
        return this.row;
    }

    /**
     * panel may have many seperate fields of entry for the user to speeratetly enter some values. For instance, if the
     * date were entered in several different fields <i>[month] [day] [year]</i>, then the return values could be the
     * set {01, 01, 01}, instead of {jan, 01, 2001}. The way the returns <em>is</em> implementation specific, but that
     * this permits a panel to interperet data for the caller.
     *
     * @return  The value of the Panel which can be in any format appropriate to the type, containing as many fields as
     *          needed to represent each distinct field of entry in the panel.
     *
     * @see     JPanelEdit#getCodedValue()
     */
    public Vector<Object> getValue() {
        Vector<Object> outputValues = new Vector<Object>(newValue.length);

        for (int i = 0; i < newValue.length; i++) {
            outputValues.add(i, newValue[i].getCodedValue());
        }

        return outputValues;
    }

    /**
     * check the dialog so that if all the fields are okay (have the right number of digits, etc) and there are no
     * messages to send back to the user about correctness.
     *
     * @return  boolean true if the dialog box is closing okay.
     */
    public boolean isDialogOkay() {

        for (int i = 0; i < newValue.length; i++) {

            if (!newValue[i].checkFields()) {
                return false;
            }
        }

        return true;
    }

    /**
     * Selects the panel for a given inputted edType, filling it the given string. If the editor Type is not found, the
     * JPanelEditDefault is returned.
     *
     * @param   oldVal  The string to fill the editor w
     * @param   edType  the editor panel to use
     *
     * @return  the apropriate JPanelEdit.
     */
    public JPanelEdit makeAppropriateInputPanel(String oldVal, int edType) {
        JPanelEdit anEditor;

        // System.out.println("Editor Type: " + edType);
        switch (edType) {

            case JDialogEditor.CHAR:
                anEditor = new JPanelEditChar(oldVal.charAt(0));
                break;

            case JDialogEditor.STRING:
                anEditor = new JPanelEditDefault(oldVal);
                break;

            case JDialogEditor.INT_STRING:
                anEditor = new JPanelEditDefault(oldVal);
                ((JPanelEditDefault) anEditor).makeNumericsOnly(false);
                break;

            case JDialogEditor.FLOAT_STRING:
                anEditor = new JPanelEditDefault(oldVal);
                ((JPanelEditDefault) anEditor).makeNumericsOnly(true);
                break;

            case JDialogEditor.ANALYZE_DATATYPE:
                anEditor = new JPanelEditDefault(oldVal);
                break;

            case JDialogEditor.ANALYZE_DESCRIPTION:
                anEditor = new JPanelEditDefault(oldVal);
                ((JPanelEditDefault) anEditor).setMaxLength(80);
                break;

            case JDialogEditor.IMAGE_ORIENTATION:
                anEditor = new JPanelEditImageOrientation(oldVal);
                break;

            case JDialogEditor.ANALYZE_ORIENTATION:
                anEditor = new JPanelEditOrientation(oldVal);
                break;

            case JDialogEditor.ANALYZE_AXIS_ORIENTATION:
                anEditor = new JPanelEditAxisOrientation(oldVal);
                break;

            case JDialogEditor.ANALYZE_ORIGINATOR:
                anEditor = new JPanelEditDefault(oldVal);
                ((JPanelEditDefault) anEditor).setMaxLength(10);
                break;

            case JDialogEditor.ANALYZE_AUX:
                anEditor = new JPanelEditDefault(oldVal);
                ((JPanelEditDefault) anEditor).setMaxLength(24);
                break;

            case JDialogEditor.ANALYZE_VOX:
                anEditor = new JPanelEditDefault(oldVal);
                ((JPanelEditDefault) anEditor).setMaxLength(4);
                break;

            case JDialogEditor.ANALYZE_CAL:
                anEditor = new JPanelEditDefault(oldVal);
                ((JPanelEditDefault) anEditor).setMaxLength(8);
                break;

            case JDialogEditor.ANALYZE_DBNAME:
                anEditor = new JPanelEditDefault(oldVal);
                ((JPanelEditDefault) anEditor).setMaxLength(18);
                break;

            case JDialogEditor.XML_MODALITY:
                anEditor = new JPanelEditModality(oldVal);
                break;

            case JDialogEditor.XML_RACE:
                anEditor = new JPanelEditRace(oldVal);
                break;

            case JDialogEditor.XML_DOB:
                anEditor = new JPanelEditDate(oldVal, true);
                break;

            case JDialogEditor.XML_SEX:
                anEditor = new JPanelEditSex(oldVal);
                break;

            case JDialogEditor.XML_DATE:
                anEditor = new JPanelEditDate(oldVal, true);
                break;

            case JDialogEditor.XML_TIME:
                anEditor = new JPanelEditTime(oldVal, true);
                break;

            case JDialogEditor.XML_VALUETYPE:
                anEditor = new JPanelEditValueType(oldVal);
                break;

            case JDialogEditor.XML_SETORPARAM:
                anEditor = new JPanelEditSetOrParam(oldVal);
                break;

            case JDialogEditor.XML_LINKEDIMAGE:
                if ((oldVal != null) && !oldVal.equals("")) {
                    File tempF = new File(oldVal);

                    if (tempF.isFile()) {
                        anEditor = new JPanelFileSelection(tempF, "Choose Linked Image", true);
                        tempF = null;

                        break;
                    }
                }

                anEditor = new JPanelFileSelection(true);
                break;

            case JDialogEditor.BOOLEAN:
                anEditor = new JPanelEditBoolean(oldVal);
                break;

            case JDialogEditor.FILE_STRING:
                if ((oldVal != null) && !oldVal.equals("")) {
                    File tempF = new File(oldVal);

                    if (tempF.isFile()) {
                        anEditor = new JPanelFileSelection(tempF, "Choose Linked Image", true);
                        tempF = null;

                        break;
                    }
                }

                anEditor = new JPanelFileSelection(true);
                break;

            default:
                anEditor = new JPanelEditDefault(oldVal);
        }

        return anEditor;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  file  DOCUMENT ME!
     */
    public void setImageFileName(String file) {
        this.imageFileName = file;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  tag  DOCUMENT ME!
     */
    public void setKey(Object tag) {
        this.key = tag;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  desc  DOCUMENT ME!
     */
    public void setPSetDescription(String desc) {
        this.psetDesc = desc;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  row  DOCUMENT ME!
     */
    public void setRow(int row) {
        this.row = row;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  ht     DOCUMENT ME!
     * @param  isSet  DOCUMENT ME!
     */
    public void setTable(Hashtable<?,?> ht, boolean isSet) {

        for (int i = 0; i < newValue.length; i++) {

            if (newValue[i] instanceof JPanelEditSetOrParam) {
                ((JPanelEditSetOrParam) newValue[i]).setTable(ht, isSet);
            }
        }
    }

    /**
     * dialog is closed, but can its value be accepted now?
     *
     * @return  DOCUMENT ME!
     */
    public boolean wasDialogOkay() {
        return closedOkay;
    }

    /**
     * creates a JPanel to hold the Okay and Cancel buttons. in a FlowLayout; presets all the listeners to the buttons,
     * the fonts and text colour.
     *
     * @return  the built and loaded JPanel.
     */
    private JPanel buildOkayCancelPanel() {
        JPanel okayCancelPanel = new JPanel();

        okayCancelPanel.setLayout(new FlowLayout());
        buildOKButton();
        buildCancelButton();
        okayCancelPanel.add(OKButton);
        okayCancelPanel.add(cancelButton);

        return okayCancelPanel;
    }
}
