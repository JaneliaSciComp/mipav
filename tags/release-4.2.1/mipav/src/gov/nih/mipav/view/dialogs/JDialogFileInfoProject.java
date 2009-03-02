package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * This class shows the dialog which conatains the project info as contained in the FileInfoProject class.
 *
 * <p>It builds two tables, and any row can be made editable when supplied with the appropriate editor to use. Entries
 * that are edited okay, reports updates to project info.</p>
 *
 * <p>It merely brings up a JDialogEditor when "edit" button is clicked.</p>
 *
 * <p>(based off parsonsd's JDialogFileInfoXML)</p>
 *
 * @author  Evan McCreedy
 */
public class JDialogFileInfoProject extends JDialogBase implements ActionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 2059007700162379513L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JButton addImage;

    /** DOCUMENT ME! */
    private JMenuItem addInfo;

    /** DOCUMENT ME! */
    private JMenuItem addParam;

    /** DOCUMENT ME! */
    private JButton addSet;

    /** DOCUMENT ME! */
    private JButton edit;

    /** DOCUMENT ME! */
    private JDialogEditor editorDialog;

    /** DOCUMENT ME! */
    private Hashtable editorDialogTable;

    /** DOCUMENT ME! */
    private FileInfoProject fileinfo;

    /** DOCUMENT ME! */
    private String imageFileNameforAddInfo;

    /** DOCUMENT ME! */
    private Hashtable imageHashtable;

    /** DOCUMENT ME! */
    private String[] infoColumnNames = { "Name", "Description", "Value-Type", "Value" };

    /** DOCUMENT ME! */
    private ViewTableModel infoModel; // also different

    /** DOCUMENT ME! */
    private JTable infoTable;

    /** DOCUMENT ME! */
    private ViewTableModel investigatorModel;

    /** DOCUMENT ME! */
    private JTable investigatorTable;

    /** DOCUMENT ME! */
    private Hashtable investigatorTypeHolder, investigatorEditorHolder;

    /** DOCUMENT ME! */
    private ListSelectionModel listSelector;

    /** DOCUMENT ME! */
    private JScrollPane masterScrollPane;

    /** DOCUMENT ME! */
    private int numImages = 0;

    /** DOCUMENT ME! */
    private int numSets = 0;

    /** DOCUMENT ME! */
    private String[] parameterColumnNames = { "Name", "Description", "Value-Type", "Value", "Date", "Time" };

    /** DOCUMENT ME! */
    private ViewTableModel parameterModel; // diff than others

    /** DOCUMENT ME! */
    private JTable parameterTable;

    /** DOCUMENT ME! */
    private ViewTableModel primaryModel;

    /** DOCUMENT ME! */
    private JTable primaryTable;

    /** tpe holds the type of editor to be used; editor holds the editor dialog. */
    private Hashtable primaryTypeHolder, primaryEditorHolder;

    /** DOCUMENT ME! */
    private JButton removeInfo;

    /** DOCUMENT ME! */
    private JButton removeParam;

    /** DOCUMENT ME! */
    private Box scrollingBox;

    /** DOCUMENT ME! */
    private JScrollPane scrollPane;

    /** DOCUMENT ME! */
    private int selectedRow;

    /** DOCUMENT ME! */
    private String setDescforAddParam;

    /** DOCUMENT ME! */
    private Hashtable setHashtable;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * This method displays all the project info. It calls parseDate and parseTime when needed and translates other
     * strings.
     *
     * @param  parent  the parent frame of this dialog
     * @param  title   the title for this dialog
     */
    public JDialogFileInfoProject(Frame parent, String title) {
        super(parent, false);
        setTitle(title);
        primaryTypeHolder = new Hashtable(); // all editable lines in primary, keyed by location in Jtable
        primaryEditorHolder = new Hashtable(); // all editable lines in primary, keyed by location in Jtable

        setHashtable = new Hashtable();
        imageHashtable = new Hashtable();
        addParam = new JMenuItem("Add Parameter");
        addParam.setActionCommand("AddParameter");
        addParam.addActionListener(this);
        addInfo = new JMenuItem("Add Image Info Item");
        addInfo.setActionCommand("AddInfo");
        addInfo.addActionListener(this);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Permits the caller to get a value out of the primary table by using the name given to the fileInfo.
     *
     * @param   name  entry to retrieve from the primary table
     *
     * @return  the value returned is the first value which keys to this name; any other instances of the name will be
     *          ignored. <code>Null</code> is returned if the name cannot be found
     */
    public String accessPrimaryData(String name) {
        String possibleValue = null;

        for (int i = 0; i < primaryModel.getRowCount(); i++) {
            possibleValue = (String) primaryModel.getValueAt(i, 1);

            if (name.equals(possibleValue)) {
                return possibleValue;
            }
        }

        return null;
    }

    /**
     * Closes the dialog when the user clicks close.
     *
     * <p>Creates editor dialogs to allow changing the value-field of a tag when user clicks "Edit Tag" button. This
     * implmentation supports virtually any number of tag editors, bringing forward any previously opened editor. Most
     * processing occurs when this class hears an editor window close;</p>
     *
     * <p>Will alert any open window (frame) to set title as that information may have changed.</p>
     *
     * <p>To make this more FileInfoBase friendly, add a public static void stateChanged(Vector) to FileInfoBase. Then
     * remove the references to the cast. Otherwise, using the editors with other varieties of FileInfo will throw
     * ClassCastExceptions. Also suggest that a distinct datatype (other than Vector) be created to handle the special
     * needs.</p>
     *
     * @param  e  event that triggered this action
     */
    public void actionPerformed(ActionEvent e) {
        JDialogEditor editor;

        if (e.getActionCommand().equals("Close")) { // close

            // clear out the editor dialog boxes
            for (Enumeration en = editorDialogTable.elements(); en.hasMoreElements();) {
                editor = (JDialogEditor) en.nextElement();
                editor.dispose();
            }

            editorDialogTable.clear();
            this.dispose(); // remove self
        } else if (e.getActionCommand().equals("RemoveParameter")) {
            Enumeration eset = setHashtable.keys();
            boolean atLeastOne = false;

            while (eset.hasMoreElements()) {
                String psetdesc = (String) eset.nextElement();
                PSetDisplay pset = (PSetDisplay) setHashtable.get(psetdesc);
                int[] rows = pset.getTable().getSelectedRows();
                boolean[] deleterows = new boolean[rows.length];
                String[] paramNames = new String[rows.length];

                for (int i = 0; i < rows.length; i++) {
                    paramNames[i] = new String((String) pset.getModel().getValueAt(rows[i], 0));

                    int reply = JOptionPane.showConfirmDialog(this,
                                                              "Remove Parameter (" + paramNames[i] + ") from Set (" +
                                                              psetdesc + ")?", "Confirm Remove Parameter",
                                                              JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);

                    if (reply == JOptionPane.YES_OPTION) {
                        atLeastOne = true;
                        deleterows[i] = true;
                    } else {
                        deleterows[i] = false;
                    }
                }

                // delete from last to first (so we don't mess up the indices)
                for (int i = rows.length - 1; i > -1; i--) {

                    if (deleterows[i]) {

                        // first remove from fileinfo, then from model
                        fileinfo.getPSet(psetdesc).removeParameter(paramNames[i]);
                        pset.getModel().removeRow(rows[i]);
                        getContentPane().validate();

                        // if this is the last row, then we need to delete the set as well
                        if (pset.getTable().getRowCount() < 1) {
                            scrollingBox.remove(pset.getTable().getTableHeader());
                            scrollingBox.remove(pset.getTable());
                            scrollingBox.remove(pset.getLabel());
                            setHashtable.remove(psetdesc);
                            getContentPane().validate();
                            fileinfo.removePSet(psetdesc);
                        }
                    }
                }
            }
        } else if (e.getActionCommand().equals("AddParameter")) {

            // System.out.println("going to add parameter");
            if (this.setDescforAddParam != null) {
                // Create an editor to set the Parameter Name
                // (required and non editable post-creation)

                String[] values = new String[6];
                values[0] = new String("Name");
                values[1] = new String("Description");
                values[2] = new String("Value Type");
                values[3] = new String("Value");
                values[4] = new String("0000-00-00");
                values[5] = new String("00:00:00-00:00");

                int[] editors = new int[6];
                editors[0] = JDialogEditor.XML_SETORPARAM;
                editors[1] = JDialogEditor.STRING;
                editors[2] = JDialogEditor.XML_VALUETYPE;
                editors[3] = JDialogEditor.STRING;
                editors[4] = JDialogEditor.XML_DATE;
                editors[5] = JDialogEditor.XML_TIME;
                editor = new JDialogEditor(this, new String("NewParameter"), values, editors);
                editor.setTitle("New Parameter Data");
                editor.setVisible(true);

                Hashtable tmp = fileinfo.getParameterTable(setDescforAddParam);

                editor.setTable(fileinfo.getParameterTable(this.setDescforAddParam), false);
                editor.addWindowListener(new WindowAdapter() {

                        // when windowClosed: check to see if dialog closed by OKbutton (if it did then there
                        // were no problems with user inputted values); if it closed by okbutton, then
                        // modify all slices if user so desired.
                        // In addition, when the change was applied to all slices, notify the image frames
                        // in the image frame vector to reset the title (title changes only for name,
                        // @see ModelImage#setTitle(String)
                        //
                        private boolean alreadyClosed = false; // was the editor closed?

                        public void windowClosed(WindowEvent e) {
                            JDialogEditor ed;
                            ed = (JDialogEditor) e.getSource();

                            if (ed.wasDialogOkay() && !alreadyClosed) {
                                alreadyClosed = true;

                                String s = ed.getDisplayValueForParam();
                                StringTokenizer dt = new StringTokenizer(s, "\\");

                                if (dt.hasMoreElements()) {
                                    String name = dt.nextToken();
                                    String desc = dt.nextToken();
                                    String vt = dt.nextToken();
                                    String val = dt.nextToken();
                                    String date = dt.nextToken();
                                    String time = dt.nextToken();

                                    appendParameter(setDescforAddParam, name, desc, vt, val, date, time);

                                    // update fileinfo
                                    FileInfoProject.PSet temp = fileinfo.getPSet(setDescforAddParam);
                                    temp.addParameter(name);
                                    temp.getParameter(name).setDescription(desc);
                                    temp.getParameter(name).setValueType(vt);
                                    temp.getParameter(name).setDate(date);
                                    temp.getParameter(name).setTime(time);
                                }
                            }
                        }
                    });
            }
        } else if (e.getActionCommand().equals("AddSet")) {
            this.numSets++;

            String[] values = new String[7];
            values[0] = new String("Set Description");
            values[1] = new String("Parameter Name");
            values[2] = new String("Parameter Description");
            values[3] = new String("Parameter Value Type");
            values[4] = new String("Parameter Value");
            values[5] = new String("0000-00-00");
            values[6] = new String("00:00:00-00:00");

            int[] editors = new int[7];
            editors[0] = JDialogEditor.XML_SETORPARAM;
            editors[1] = JDialogEditor.STRING;
            editors[2] = JDialogEditor.STRING;
            editors[3] = JDialogEditor.XML_VALUETYPE;
            editors[4] = JDialogEditor.STRING;
            editors[5] = JDialogEditor.XML_DATE;
            editors[6] = JDialogEditor.XML_TIME;
            editor = new JDialogEditor(this, new String("NewSet" + numSets), values, editors);
            editor.setTitle("Add New Set with Parameter");
            editor.setVisible(true);
            editor.setTable(fileinfo.getPSetHashtable(), true);

            editor.addWindowListener(new WindowAdapter() {

                    // when windowClosed: check to see if dialog closed by OKbutton (if it did then there
                    // were no problems with user inputted values); if it closed by okbutton, then
                    // modify all slices if user so desired.
                    // In addition, when the change was applied to all slices, notify the image frames
                    // in the image frame vector to reset the title (title changes only for name,
                    // @see ModelImage#setTitle(String)
                    //
                    private boolean alreadyClosed = false; // was the editor closed?

                    public void windowClosed(WindowEvent e) {
                        JDialogEditor ed;
                        ed = (JDialogEditor) e.getSource();

                        if (ed.wasDialogOkay() && !alreadyClosed) {
                            alreadyClosed = true;

                            String s = ed.getDisplayValueForParam();
                            StringTokenizer dt = new StringTokenizer(s, "\\");

                            if (dt.hasMoreElements()) {
                                String psetdesc = dt.nextToken();
                                String name = dt.nextToken();
                                String desc = dt.nextToken();
                                String vt = dt.nextToken();
                                String val = dt.nextToken();
                                String date = dt.nextToken();
                                String time = dt.nextToken();

                                setHashtable.put(psetdesc, new PSetDisplay(psetdesc));

                                scrollingBox.add(((PSetDisplay) setHashtable.get(psetdesc)).getLabel());
                                scrollingBox.add(((PSetDisplay) setHashtable.get(psetdesc)).getTable().getTableHeader());
                                scrollingBox.add(((PSetDisplay) setHashtable.get(psetdesc)).getTable());

                                appendParameter(psetdesc, name, desc, vt, val, date, time);
                                getContentPane().validate();

                                // update fileinfo
                                fileinfo.createPSet(psetdesc);
                                fileinfo.getPSet(psetdesc).addParameter(name);
                                fileinfo.getPSet(psetdesc).getParameter(name).setDescription(desc);
                                fileinfo.getPSet(psetdesc).getParameter(name).setValueType(vt);
                                fileinfo.getPSet(psetdesc).getParameter(name).setValue(val);
                                fileinfo.getPSet(psetdesc).getParameter(name).setDate(date);
                                fileinfo.getPSet(psetdesc).getParameter(name).setTime(time);
                            }
                        }
                    }
                });
        } else if (e.getActionCommand().equals("RemoveInfo")) {
            Enumeration eset = imageHashtable.keys();
            boolean atLeastOne = false;

            while (eset.hasMoreElements()) {
                String file = (String) eset.nextElement();
                ImageDisplay image = (ImageDisplay) imageHashtable.get(file);
                int[] rows = image.getTable().getSelectedRows();
                boolean[] deleterows = new boolean[rows.length];
                String[] infoNames = new String[rows.length];

                for (int i = 0; i < rows.length; i++) {
                    infoNames[i] = new String((String) image.getModel().getValueAt(rows[i], 0));

                    int reply = JOptionPane.showConfirmDialog(this,
                                                              "Remove Image Info (" + infoNames[i] + ") from Image (" +
                                                              file + ")?", "Confirm Remove Image Information",
                                                              JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);

                    if (reply == JOptionPane.YES_OPTION) {
                        atLeastOne = true;
                        deleterows[i] = true;
                    } else {
                        deleterows[i] = false;
                    }
                }

                // delete from last to first (so we don't mess up the indices)
                for (int i = rows.length - 1; i > -1; i--) {

                    if (deleterows[i]) {

                        // first remove from fileinfo, then from model
                        fileinfo.getImage(file).removeInfo(infoNames[i]);
                        image.getModel().removeRow(rows[i]);
                        getContentPane().validate();

                        // if this is the last row, then we need to delete the set as well
                        if (image.getTable().getRowCount() < 1) {
                            scrollingBox.remove(image.getTable().getTableHeader());
                            scrollingBox.remove(image.getTable());
                            scrollingBox.remove(image.getLabel());
                            imageHashtable.remove(file);
                            getContentPane().validate();
                            fileinfo.removeImage(file);
                        }
                    }
                }
            }
        } else if (e.getActionCommand().equals("AddInfo")) {

            // System.out.println("going to add info");
            if (this.imageFileNameforAddInfo != null) {
                // Create an editor to set the info item Name
                // (required and non editable post-creation)

                String[] values = new String[4];
                values[0] = new String("Name");
                values[1] = new String("Description");
                values[2] = new String("Value Type");
                values[3] = new String("Value");

                int[] editors = new int[4];
                editors[0] = JDialogEditor.XML_SETORPARAM;
                editors[1] = JDialogEditor.STRING;
                editors[2] = JDialogEditor.XML_VALUETYPE;
                editors[3] = JDialogEditor.STRING;
                editor = new JDialogEditor(this, new String("NewInfo"), values, editors);
                editor.setTitle("New Image Info Data");
                editor.setVisible(true);

                Hashtable tmp = fileinfo.getImageInfoTable(imageFileNameforAddInfo);

                editor.setTable(fileinfo.getImageInfoTable(this.imageFileNameforAddInfo), false);
                editor.addWindowListener(new WindowAdapter() {

                        // when windowClosed: check to see if dialog closed by OKbutton (if it did then there
                        // were no problems with user inputted values)
                        //
                        private boolean alreadyClosed = false; // was the editor closed?

                        public void windowClosed(WindowEvent e) {
                            JDialogEditor ed;
                            ed = (JDialogEditor) e.getSource();

                            if (ed.wasDialogOkay() && !alreadyClosed) {
                                alreadyClosed = true;

                                String s = ed.getDisplayValueForInfo();
                                StringTokenizer dt = new StringTokenizer(s, "####");

                                if (dt.hasMoreElements()) {
                                    String name = dt.nextToken();
                                    String desc = dt.nextToken();
                                    String vt = dt.nextToken();
                                    String val = dt.nextToken();

                                    appendInfo(imageFileNameforAddInfo, name, desc, vt, val);

                                    // update fileinfo
                                    FileInfoProject.ProjectImage temp = fileinfo.getImage(imageFileNameforAddInfo);
                                    temp.addInfo(name);
                                    temp.getInfo(name).setDescription(desc);
                                    temp.getInfo(name).setValueType(vt);
                                    temp.getInfo(name).setValue(val);
                                }
                            }
                        }
                    });
            }
        } else if (e.getActionCommand().equals("AddImage")) {
            this.numImages++;

            String[] values = new String[5];
            values[0] = new String("Image File Name");
            values[1] = new String("Info Name");
            values[2] = new String("Info Description");
            values[3] = new String("Info Value Type");
            values[4] = new String("Info Value");

            int[] editors = new int[5];
            editors[0] = JDialogEditor.XML_SETORPARAM;
            editors[1] = JDialogEditor.STRING;
            editors[2] = JDialogEditor.STRING;
            editors[3] = JDialogEditor.XML_VALUETYPE;
            editors[4] = JDialogEditor.STRING;
            editor = new JDialogEditor(this, new String("NewImage" + numSets), values, editors);
            editor.setTitle("Add New Image");
            editor.setVisible(true);
            editor.setTable(fileinfo.getImages(), true);

            editor.addWindowListener(new WindowAdapter() {

                    // when windowClosed: check to see if dialog closed by OKbutton (if it did then there
                    // were no problems with user inputted values)
                    private boolean alreadyClosed = false; // was the editor closed?

                    public void windowClosed(WindowEvent e) {
                        JDialogEditor ed;
                        ed = (JDialogEditor) e.getSource();

                        if (ed.wasDialogOkay() && !alreadyClosed) {
                            alreadyClosed = true;

                            String s = ed.getDisplayValueForInfo();
                            StringTokenizer dt = new StringTokenizer(s, "####");

                            if (dt.hasMoreElements()) {
                                String file = dt.nextToken();
                                String name = dt.nextToken();
                                String desc = dt.nextToken();
                                String vt = dt.nextToken();
                                String val = dt.nextToken();

                                imageHashtable.put(file, new ImageDisplay(file));

                                scrollingBox.add(((ImageDisplay) imageHashtable.get(file)).getLabel());
                                scrollingBox.add(((ImageDisplay) imageHashtable.get(file)).getTable().getTableHeader());
                                scrollingBox.add(((ImageDisplay) imageHashtable.get(file)).getTable());

                                appendInfo(file, name, desc, vt, val);
                                getContentPane().validate();

                                // update fileinfo
                                fileinfo.addImage(file);
                                fileinfo.getImage(file).addInfo(name);
                                fileinfo.getImage(file).getInfo(name).setDescription(desc);
                                fileinfo.getImage(file).getInfo(name).setValueType(vt);
                                fileinfo.getImage(file).getInfo(name).setValue(val);
                            }
                        }
                    }
                });
        } else if (e.getActionCommand().equals("EditTag")) { // edit the high-lighted tag

            Integer named;
            int[] rows;
            int i = 0;
            Object obj;
            Vector objs;

            // go thruough primary table, editing highlighted (selected ) rows
            rows = primaryTable.getSelectedRows();

            for (i = 0; i < rows.length; i++) {

                // determine if this row is one which can be edited
                named = new Integer(rows[i]);

                if (primaryTypeHolder.containsKey(named)) { // if named
                    primaryTable.removeRowSelectionInterval(rows[i], rows[i]);

                    // if previously named, bring the editors to the front
                    obj = primaryEditorHolder.get(named);

                    if (obj != null) {
                        ((JDialogEditor) (obj)).toFront();
                    } else {
                        objs = (Vector) primaryTypeHolder.get(named);

                        String[] values = new String[1]; // this is unclean design now to cover something i didn't
                                                         // foresee

                        if (objs.size() > 1) { // assume seperate words mean something special

                            // when there is more than one editor specified.
                            values = separateValues((String) primaryTable.getValueAt(rows[i], 1));
                        } else {
                            values[0] = (String) primaryTable.getValueAt(rows[i], 1);
                        }

                        int[] editors = new int[objs.size()];

                        for (int j = 0; j < objs.size(); j++) {
                            editors[j] = ((Integer) (objs.elementAt(j))).intValue(); // what the user asked for
                        }

                        editor = new JDialogEditor(this, named, values, editors);
                        editor.setTitle((String) primaryTable.getValueAt(rows[i], 0));
                        editor.setVisible(true);
                        editor.addWindowListener(new WindowAdapter() {

                                //  when windowClosed: check to see if dialog closed by OKbutton (if it did then there
                                //    were no problems with user inputted values); if it closed by okbutton, then
                                // modify all slices if user so desired.      In addition, when the change was applied
                                // to all slices, notify the image frames      in the image frame vector to reset the
                                // title (title changes only for name,      @see ModelImage#setTitle(String)
                                private boolean alreadyClosed = false; // was the editor closed?

                                public void windowClosed(WindowEvent e) {
                                    JDialogEditor ed;
                                    ed = (JDialogEditor) e.getSource();

                                    Integer edID = (Integer) ed.getKey();
                                    Vector changed = new Vector(5);
                                    changed.add(0, new Integer(1)); // table
                                    changed.add(1, edID); // line
                                    changed.add(2, primaryModel.getValueAt(edID.intValue(), 0));
                                    changed.add(3, ed.getValue());
                                    changed.add(4, ed.getDisplayValue());

                                    if (ed.wasDialogOkay() && !alreadyClosed) {
                                        alreadyClosed = true;

                                        // to make this more FileInfoBase friendly, add a
                                        // public static void stateChanged(Vector)
                                        // to FileInfoBase.  Then remove the references to
                                        // the cast.  Otherwise, using the editors with other
                                        // varieties of FileInfo will throw ClassCastExceptions.
                                        // Also suggest that a distinct datatype (other than Vector)
                                        // be created to handle the special needs.
                                        fileinfo.stateChanged(changed); // what to do to update fileinfo!?!?!
                                        primaryModel.setValueAt(ed.getDisplayValue(), edID.intValue(), 1);
                                        primaryEditorHolder.remove(edID);
                                    } else {
                                        primaryEditorHolder.remove(edID);
                                        changed = null; // forget it
                                    }


                                }
                            });
                        primaryEditorHolder.put(named, editor);
                    }
                } else { }
            }

            // selected rows on investigator table
            rows = investigatorTable.getSelectedRows();

            for (i = 0; i < rows.length; i++) {

                // determine if this row can be edited
                named = new Integer(rows[i]);

                if (investigatorTypeHolder.containsKey(named)) { // if named
                    investigatorTable.removeRowSelectionInterval(rows[i], rows[i]);

                    // if previously named, bring the editors to the front
                    obj = investigatorEditorHolder.get(named);

                    if (obj != null) {
                        ((JDialogEditor) (obj)).toFront();
                    } else {
                        objs = (Vector) investigatorTypeHolder.get(named);

                        String[] values = new String[1]; // this is unclean design now to cover something i didn't
                                                         // foresee

                        if (objs.size() > 1) { // assume seperate words mean something special

                            // when there is more than one editor specified.
                            values = separateValues((String) investigatorTable.getValueAt(rows[i], 1));
                        } else {
                            values[0] = (String) investigatorTable.getValueAt(rows[i], 1);
                        }

                        int[] editors = new int[objs.size()]; // because we tell an editor to do something based on an
                                                              // int code

                        for (int j = 0; j < objs.size(); j++) {
                            editors[j] = ((Integer) (objs.elementAt(j))).intValue(); // what the user asked for
                        }

                        editor = new JDialogEditor(this, named, values, editors);
                        editor.setTitle((String) investigatorTable.getValueAt(rows[i], 0));
                        editor.setVisible(true);
                        editor.addWindowListener(new WindowAdapter() {

                                //  when windowClosed: check to see if dialog closed by OKbutton (if it did then there
                                //    were no problems with user inputted values); if it closed by okbutton, then
                                // modify all slices if user so desired.      In addition, when the change was applied
                                // to all slices, notify the image frames      in the image frame vector to reset the
                                // title (title changes only for name,      @see ModelImage#setTitle(String)
                                private boolean alreadyClosed = false;

                                public void windowClosed(WindowEvent e) {
                                    alreadyClosed = true;

                                    JDialogEditor ed;
                                    ed = (JDialogEditor) e.getSource();

                                    Integer edID = (Integer) ed.getKey();
                                    Vector changed = new Vector(4);
                                    changed.add(0, new Integer(1)); // table
                                    changed.add(1, edID); // line
                                    changed.add(2, investigatorModel.getValueAt(edID.intValue(), 0));
                                    changed.add(3, ed.getValue());
                                    changed.add(4, ed.getDisplayValue());

                                    if (ed.wasDialogOkay() && !alreadyClosed) {

                                        // to make this more FileInfoBase friendly, add a
                                        // public static void stateChanged(Vector)
                                        // to FileInfoBase.  Then remove the references to
                                        // the cast.  Otherwise, using the editors with other
                                        // varieties of FileInfo will throw ClassCastExceptions.
                                        // Also suggest that a distinct datatype (other than Vector)
                                        // be created to handle the special needs.
                                        fileinfo.stateChanged(changed); // what to do to update fileinfo!?!?!
                                        investigatorModel.setValueAt(ed.getDisplayValue(), edID.intValue(), 1);
                                        investigatorEditorHolder.remove(edID);
                                    } else {
                                        investigatorEditorHolder.remove(edID);
                                        changed = null; // forget it
                                    }
                                }
                            });
                        investigatorEditorHolder.put(named, editor);
                    }
                } else { }
            }

            // Check to see if any Parameters are highlighted
            Enumeration eset = setHashtable.keys();

            while (eset.hasMoreElements()) {
                PSetDisplay pset = (PSetDisplay) setHashtable.get(eset.nextElement());
                rows = pset.getTable().getSelectedRows();


                for (int index = 0; index < rows.length; index++) {
                    pset.getTable().removeRowSelectionInterval(rows[i], rows[i]);

                    String[] values = new String[5];
                    values[0] = (String) pset.getTable().getValueAt(rows[index], 1);
                    values[1] = (String) pset.getTable().getValueAt(rows[index], 2);
                    values[2] = (String) pset.getTable().getValueAt(rows[index], 3);
                    values[3] = (String) pset.getTable().getValueAt(rows[index], 4);
                    values[4] = (String) pset.getTable().getValueAt(rows[index], 5);

                    int[] editors = new int[5];
                    editors[0] = JDialogEditor.STRING;
                    editors[1] = JDialogEditor.XML_VALUETYPE;
                    editors[2] = JDialogEditor.STRING;
                    editors[3] = JDialogEditor.XML_DATE;
                    editors[4] = JDialogEditor.XML_TIME;

                    editor = new JDialogEditor(this, pset.getDescription(), values, editors);
                    editor.setTitle(pset.getDescription() + "-" + (String) pset.getTable().getValueAt(rows[index], 0));
                    editor.setVisible(true);
                    editor.setPSetDescription(pset.getDescription());
                    editor.setRow(rows[index]);
                    editor.addWindowListener(new WindowAdapter() {

                            // when windowClosed: check to see if dialog closed by OKbutton (if it did then there
                            // were no problems with user inputted values); if it closed by okbutton, then
                            // modify all slices if user so desired.
                            // In addition, when the change was applied to all slices, notify the image frames
                            // in the image frame vector to reset the title (title changes only for name,
                            // @see ModelImage#setTitle(String)
                            //
                            private boolean alreadyClosed = false;

                            public void windowClosed(WindowEvent e) {
                                JDialogEditor ed;
                                ed = (JDialogEditor) e.getSource();

                                if (ed.wasDialogOkay() && !alreadyClosed) {
                                    alreadyClosed = true;

                                    String s = ed.getDisplayValueForParam();
                                    StringTokenizer st = new StringTokenizer(s, "\\");

                                    if (st.hasMoreElements()) {
                                        String desc = st.nextToken();
                                        String vt = st.nextToken();
                                        String val = st.nextToken();
                                        String date = st.nextToken();

                                        // time is optional
                                        String time = null;

                                        if (st.hasMoreElements()) {
                                            time = st.nextToken();
                                        }

                                        PSetDisplay temp = (PSetDisplay) setHashtable.get(ed.getPSetDescription());
                                        int row = ed.getRow();
                                        temp.getModel().setValueAt(desc, row, 1);
                                        temp.getModel().setValueAt(vt, row, 2);
                                        temp.getModel().setValueAt(val, row, 3);
                                        temp.getModel().setValueAt(date, row, 4);

                                        if (time != null) {
                                            temp.getModel().setValueAt(time, row, 5);
                                        }

                                        Vector pData = new Vector(7);
                                        pData.add(0, ed.getPSetDescription());
                                        pData.add(1, temp.getModel().getValueAt(row, 0));
                                        pData.add(2, desc);
                                        pData.add(3, vt);
                                        pData.add(4, val);
                                        pData.add(5, date);

                                        if (time != null) {
                                            pData.add(6, time);
                                        }

                                        fileinfo.parameterChanged(pData);
                                    }
                                }
                            }
                        });
                }
            }

            // Check to see if any Image Info items are highlighted
            Enumeration ie = imageHashtable.keys();

            while (ie.hasMoreElements()) {
                ImageDisplay image = (ImageDisplay) imageHashtable.get(ie.nextElement());
                rows = image.getTable().getSelectedRows();

                for (int index = 0; index < rows.length; index++) {
                    image.getTable().removeRowSelectionInterval(rows[i], rows[i]);

                    String[] values = new String[3];
                    values[0] = (String) image.getTable().getValueAt(rows[index], 1);
                    values[1] = (String) image.getTable().getValueAt(rows[index], 2);
                    values[2] = (String) image.getTable().getValueAt(rows[index], 3);

                    int[] editors = new int[3];
                    editors[0] = JDialogEditor.STRING;
                    editors[1] = JDialogEditor.XML_VALUETYPE;
                    editors[2] = JDialogEditor.STRING;

                    editor = new JDialogEditor(this, image.getFileName(), values, editors);
                    editor.setTitle(image.getFileName() + "-" + (String) image.getTable().getValueAt(rows[index], 0));
                    editor.setVisible(true);
                    editor.setImageFileName(image.getFileName());
                    editor.setRow(rows[index]);
                    editor.addWindowListener(new WindowAdapter() {

                            // when windowClosed: check to see if dialog closed by OKbutton (if it did then there
                            // were no problems with user inputted values)
                            private boolean alreadyClosed = false;

                            public void windowClosed(WindowEvent e) {
                                JDialogEditor ed;
                                ed = (JDialogEditor) e.getSource();

                                if (ed.wasDialogOkay() && !alreadyClosed) {
                                    alreadyClosed = true;

                                    String s = ed.getDisplayValueForInfo();
                                    StringTokenizer st = new StringTokenizer(s, "####");

                                    if (st.hasMoreElements()) {
                                        String desc = st.nextToken();
                                        String vt = st.nextToken();
                                        String val = st.nextToken();
                                        ImageDisplay temp = (ImageDisplay) imageHashtable.get(ed.getImageFileName());
                                        int row = ed.getRow();
                                        temp.getModel().setValueAt(desc, row, 1);
                                        temp.getModel().setValueAt(vt, row, 2);
                                        temp.getModel().setValueAt(val, row, 3);

                                        Vector iData = new Vector(5);
                                        iData.add(0, ed.getImageFileName());
                                        iData.add(1, temp.getModel().getValueAt(row, 0));
                                        iData.add(2, desc);
                                        iData.add(3, vt);
                                        iData.add(4, val);
                                        fileinfo.infoChanged(iData);
                                    }
                                }
                            }
                        });
                }
            }

        }

    }

    /**
     * Appends an editable row with the given image info data to the proper image display table.
     *
     * @param  file       image file name
     * @param  name       info name
     * @param  desc       info description
     * @param  valueType  info value type
     * @param  value      info value
     */
    public void appendInfo(String file, String name, String desc, String valueType, String value) {
        ((ImageDisplay) imageHashtable.get(file)).addInfoData(name, desc, valueType, value);
    }

    /**
     * Appends an editable row to the end of the investigators table.
     *
     * @param  name    the investigator name
     * @param  value   investigator information to display
     * @param  editor  list of editor types for editing this row
     */
    public void appendInvestigatorData(String name, String value, int[] editor) {
        String[] rose = { name, value };
        investigatorModel.addRow(rose);

        Vector editorInts = new Vector();

        for (int i = 0; i < editor.length; i++) { // set the list of editors to use
            editorInts.addElement(new Integer(editor[i]));
        }

        investigatorTypeHolder.put(new Integer(investigatorModel.getRowCount() - 1), editorInts);
    }


    /**
     * Appends an editable row with the given parameter data to the proper set display table.
     *
     * @param  setDesc    set description
     * @param  paramName  parameter name
     * @param  paramDesc  parameter description
     * @param  valueType  parameter value type
     * @param  value      parameter value
     * @param  date       parameter date
     * @param  time       parameter time
     */
    public void appendParameter(String setDesc, String paramName, String paramDesc, String valueType, String value,
                                String date, String time) {

        if (date == null) {
            date = new String("0000-01-01");
        }

        if (time == null) {
            time = new String("00:00:00-00:00");
        }

        ((PSetDisplay) setHashtable.get(setDesc)).addParameterData(paramName, paramDesc, valueType, value, date, time);
    }

    /**
     * Appends a non editable row to the end of the primary table.
     *
     * @param  name   file info parameter (ie., dimensions, extents, &c).
     * @param  value  value assigned to a fileinfo parameter
     */
    public void appendPrimaryData(String name, String value) {

        if (value.indexOf('\n') == -1) { // \n doesn't occur in the value

            String[] rose = { name, value };
            primaryModel.addRow(rose);
        } else {
            StringTokenizer stok = new StringTokenizer(value, "\n");
            String[] values = new String[stok.countTokens()];
            int i = 0;

            while (stok.hasMoreTokens()) {
                values[i++] = stok.nextToken();
            }

            String[] rose = { name, values[0] };
            primaryModel.addRow(rose);
            i = 1;
            rose[0] = "";

            while (i < values.length) {
                rose[1] = values[i++];
                primaryModel.addRow(rose);
            }
        }
    }

    /**
     * Appends a row to the end of the primary info table, assigns this name/value pair to be editable and adds the
     * fileinfo to listen for this name.
     *
     * @param  name    file info parameter (ie., dimensions, extents, &c).
     * @param  value   value assigned to a fileinfo parameter
     * @param  editor  The value of editor is the editor interface to be used. Eg., a JPanelEditDefault. Specified by
     *
     *                 <ul>
     *                   <li>JDialogFileInfo#IntString</li>
     *                   <li>JDialogFileInfo#FloatString</li>
     *                 </ul>
     */
    public void appendPrimaryData(String name, String value, int[] editor) {
        String[] rose = { name, value };
        primaryModel.addRow(rose);

        Vector editorInts = new Vector();

        for (int i = 0; i < editor.length; i++) { // set the list of editors to use
            editorInts.addElement(new Integer(editor[i]));
        }

        primaryTypeHolder.put(new Integer(primaryModel.getRowCount() - 1), editorInts);
    }

    /**
     * Makes the display frame and builds the layout.
     *
     * @param  fileInfo  the info to display
     */
    public void displayAboutInfo(FileInfoProject fileInfo) {
        JScrollPane scrollPane;
        JScrollPane primaryPane, investigatorPane;
        String[] columnNames = { "Name", "Value" };

        JScrollPane[] setPanes = new JScrollPane[fileInfo.getNumPSets()];
        JScrollPane[] imagePanes = new JScrollPane[fileInfo.getNumImages()];

        try {
            this.fileinfo = fileInfo;

            investigatorTypeHolder = new Hashtable();
            investigatorEditorHolder = new Hashtable();
            investigatorModel = new ViewTableModel();
            investigatorTable = new JTable(investigatorModel);
            investigatorTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
            investigatorTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);

            // for the diff tables inside the scrollpanel:
            scrollingBox = new Box(BoxLayout.Y_AXIS);

            primaryModel = new ViewTableModel();
            primaryTable = new JTable(primaryModel);
            editorDialogTable = new Hashtable(); // hastable to hold editing dialogs
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("ViewFileInfo reports: Out of memory!");

            return;
        } catch (IllegalArgumentException ex) {
            MipavUtil.displayError("ViewFileInfo reports: Editing table too small!" + ex);

            return;
        }

        int i;

        for (i = 0; i < 2; i++) {
            primaryModel.addColumn(columnNames[i]);
            investigatorModel.addColumn(columnNames[i]);
        }

        primaryTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
        primaryTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        primaryTable.getColumn("Name").setMinWidth(160);
        primaryTable.getColumn("Name").setMaxWidth(500);
        primaryTable.getColumn("Value").setMinWidth(50);
        primaryTable.getColumn("Value").setMaxWidth(1000);
        primaryTable.getTableHeader().setReorderingAllowed(false);

        JLabel priLabel = new JLabel("Project Information");
        priLabel.setForeground(Color.black);
        scrollingBox.add(priLabel);
        scrollingBox.add(primaryTable.getTableHeader());
        scrollingBox.add(primaryTable);

        investigatorTable.getColumn("Name").setMinWidth(160);
        investigatorTable.getColumn("Name").setMaxWidth(500);
        investigatorTable.getColumn("Value").setMinWidth(50);
        investigatorTable.getColumn("Value").setMaxWidth(1000);
        investigatorTable.getTableHeader().setReorderingAllowed(false);

        JLabel investigatorLabel = new JLabel("Investigators");
        investigatorLabel.setForeground(Color.black);

        scrollingBox.add(investigatorLabel);
        scrollingBox.add(investigatorTable.getTableHeader());
        scrollingBox.add(investigatorTable);

        // create set displays for each set
        Enumeration e = fileInfo.getPSetKeys();

        while (e.hasMoreElements()) {
            String desc = fileInfo.getPSet((String) e.nextElement()).getDescription();

            setHashtable.put(desc, new PSetDisplay(desc));
            scrollingBox.add(((PSetDisplay) setHashtable.get(desc)).getLabel());
            scrollingBox.add(((PSetDisplay) setHashtable.get(desc)).getTable().getTableHeader());
            scrollingBox.add(((PSetDisplay) setHashtable.get(desc)).getTable());
        }

        // create image displays for each image
        Enumeration ie = fileInfo.getImageKeys();

        while (ie.hasMoreElements()) {
            String file = fileInfo.getImage((String) ie.nextElement()).getFileName();

            imageHashtable.put(file, new ImageDisplay(file));
            scrollingBox.add(((ImageDisplay) imageHashtable.get(file)).getLabel());
            scrollingBox.add(((ImageDisplay) imageHashtable.get(file)).getTable().getTableHeader());
            scrollingBox.add(((ImageDisplay) imageHashtable.get(file)).getTable());
        }

        masterScrollPane = new JScrollPane(scrollingBox, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                           JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        getContentPane().add(masterScrollPane);

        JButton close = new JButton("Close");
        close.setActionCommand("Close");
        close.addActionListener(this);
        close.setPreferredSize(MipavUtil.defaultButtonSize);
        close.setFont(serif12B);

        edit = new JButton("Edit tag");

        // edit.setEnabled(false);
        edit.setActionCommand("EditTag");
        edit.addActionListener(this);
        edit.setPreferredSize(MipavUtil.defaultButtonSize);
        edit.setFont(serif12B);

        addSet = new JButton("Add Set");
        addSet.setActionCommand("AddSet");
        addSet.addActionListener(this);
        addSet.setPreferredSize(MipavUtil.defaultButtonSize);
        addSet.setFont(serif12B);

        removeParam = new JButton("Remove Parameter");
        removeParam.setActionCommand("RemoveParameter");
        removeParam.addActionListener(this);
        removeParam.setFont(serif12B);

        addImage = new JButton("Add Image");
        addImage.setActionCommand("AddImage");
        addImage.addActionListener(this);
        addImage.setPreferredSize(MipavUtil.defaultButtonSize);
        addImage.setFont(serif12B);

        removeInfo = new JButton("Remove Image Info");
        removeInfo.setActionCommand("RemoveInfo");
        removeInfo.addActionListener(this);
        removeInfo.setFont(serif12B);

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(addSet);
        buttonPanel.add(edit);
        buttonPanel.add(removeParam);
        buttonPanel.add(addImage);
        buttonPanel.add(removeInfo);
        buttonPanel.add(close);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        this.setSize(new Dimension(700, 800));

    }

    /**
     * Handle key events in the dialog.
     *
     * @param  ke  the key event to handle
     */
    public void keyTyped(KeyEvent ke) {
        edit.doClick();
    }

    /**
     * Divides space separate strings into an array of strings.
     *
     * @param   incoming  string
     *
     * @return  array of separated strings
     */
    protected String[] separateValues(String incoming) {
        StringTokenizer stok = new StringTokenizer(incoming, " ");
        String[] outgoing = new String[stok.countTokens()];
        int i = 0;

        while (stok.hasMoreTokens()) {
            outgoing[i++] = stok.nextToken();
        }

        return outgoing;
    }


    /**
     * Sort the tag column or name column of the table model. If reverse is true, sorts in reverse order.
     *
     * @param  model    the table model to sort on
     * @param  col      column to sort on
     * @param  reverse  whether or not to sort in reverse order.
     */
    private static void sort(ViewTableModel model, int col, boolean reverse) {
        int begin = 1;

        for (int p = begin; p < model.getRowCount(); p++) {

            for (int j = begin - 1; j < p; j++) {

                if (model.getValueAt(p, col) != null) {

                    if (reverse) {

                        if (((String) model.getValueAt(p, col)).compareTo((String) model.getValueAt(j, col)) > 0) {
                            model.moveRow(p, p, j);

                            break;
                        }
                    } else {

                        if (((String) model.getValueAt(p, col)).compareTo((String) model.getValueAt(j, col)) < 0) {
                            model.moveRow(p, p, j);

                            break;
                        }
                    }
                }
            }
        }
    }

    /**
     * Checks whether or not the dialog exists; if it does, it brings the dialog to front.
     *
     * @param   tagKey  the tag's Key. Used to determine if this tag already has an editor associated with it.
     * @param   model   the place to look for the dialog in
     *
     * @return  true if both a tag with the tagkey existed in the list and the associated dialog was brought to front.
     */
    private boolean bringToFront(String tagKey, Hashtable model) {
        JDialogEditor editor; // temporary tag editor dialog

        // list is empty

        if (model.isEmpty()) {
            return false;
        }
        // check all tag editors in the list to see if the given tag key is has a tag in the list.
        // drop out once one has been found and brought to front of list and of screen.
        else if (model.containsKey(tagKey)) {

            try {
                editor = (JDialogEditor) model.get(tagKey);
                editor.toFront();

                return true;
            } catch (ClassCastException cce) {
                return false;
            }
        } else {
            return false;
        }
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Class to hold one table, model, & label per image There can be infinite images, and the images are deletable, so
     * each image display must be stored in a hashtable for easy access/deletion.
     */
    public class ImageDisplay {

        /** DOCUMENT ME! */
        private String fileName;

        /** DOCUMENT ME! */
        private JLabel imageLabel;

        /** DOCUMENT ME! */
        private ViewTableModel imageModel;

        /** DOCUMENT ME! */
        private JTable imageTable;

        /**
         * Creates a image display with the given file name that will be used as the key.
         *
         * @param  file  the image to display information about
         */
        public ImageDisplay(String file) {
            this.fileName = file;

            imageLabel = new JLabel("Image: " + file);
            imageLabel.setForeground(Color.black);
            imageModel = new ViewTableModel();
            imageTable = new JTable(imageModel);

            for (int i = 0; i < 4; i++) {
                imageModel.addColumn(infoColumnNames[i]);
            }

            imageTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
            imageTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);

            imageTable.getColumn("Name").setMinWidth(160);
            imageTable.getColumn("Name").setMaxWidth(500);
            imageTable.getColumn("Description").setMinWidth(50);
            imageTable.getColumn("Description").setMaxWidth(200);
            imageTable.getColumn("Value-Type").setMinWidth(70);
            imageTable.getColumn("Value-Type").setMaxWidth(220);
            imageTable.getColumn("Value").setMinWidth(50);
            imageTable.getColumn("Value").setMaxWidth(1000);

            imageTable.getTableHeader().setReorderingAllowed(false);
            imageTable.getTableHeader().addMouseListener(new HeaderListener());

        }

        /**
         * Adds a new row of info data to the display's table.
         *
         * @param  name       info name
         * @param  desc       info description
         * @param  valueType  info value type
         * @param  value      info value
         */
        public void addInfoData(String name, String desc, String valueType, String value) {
            String[] rose = { name, desc, valueType, value };
            imageModel.addRow(rose);
        }

        /**
         * Gets the image display's file name.
         *
         * @return  the image file name
         */
        public String getFileName() {
            return this.fileName;
        }

        /**
         * Gets the image label.
         *
         * @return  the image label
         */
        public JLabel getLabel() {
            return this.imageLabel;
        }

        /**
         * Gets the image table model.
         *
         * @return  the image table model
         */
        public ViewTableModel getModel() {
            return this.imageModel;
        }

        /**
         * Gets the table of images.
         *
         * @return  the table of images
         */
        public JTable getTable() {
            return this.imageTable;
        }

        /**
         * Simple listener for the table header.
         */
        private class HeaderListener implements MouseListener {

            /**
             * When the user right clicks on a header, sorts the column. when the user left clicks, a popup menu gives
             * the option to add a info item to the table, triggering a JDialog
             *
             * @param  e  event that triggered this method
             */
            public void mouseClicked(MouseEvent e) {
                Object source = e.getSource();
                Point p = e.getPoint();
                int col;

                if (source.equals(imageTable.getTableHeader()) && (e.getButton() == MouseEvent.BUTTON1)) {
                    col = imageTable.columnAtPoint(p);

                    if (e.isShiftDown()) {
                        sort(imageModel, col, true);
                    } else {
                        sort(imageModel, col, false);
                    }
                } else if (source.equals(imageTable.getTableHeader()) && (e.getButton() == MouseEvent.BUTTON3)) {
                    imageFileNameforAddInfo = new String(getFileName());

                    JPopupMenu popUp = new JPopupMenu();

                    popUp.add(addInfo);
                    popUp.show(imageTable.getTableHeader(), p.x, p.y);
                }
            }

            /**
             * Unchanged.
             *
             * @param  e  the mouse event
             */
            public void mouseDragged(MouseEvent e) { }

            /**
             * Unchanged.
             *
             * @param  e  the mouse event
             */
            public void mouseEntered(MouseEvent e) { }

            /**
             * Unchanged.
             *
             * @param  e  the mouse event
             */
            public void mouseExited(MouseEvent e) { }

            /**
             * Unchanged.
             *
             * @param  e  the mouse event
             */
            public void mousePressed(MouseEvent e) { }

            /**
             * Unchanged.
             *
             * @param  e  the mouse event
             */
            public void mouseReleased(MouseEvent e) { }
        }
    }

    /**
     * Class to hold one table, model, & label per parameter set There can be infinite parameter sets, and the sets are
     * deletable, so each set display must be stored in a hashtable for easy access/deletion.
     */
    public class PSetDisplay {

        /** DOCUMENT ME! */
        private String description;

        /** DOCUMENT ME! */
        private JLabel setLabel;

        /** DOCUMENT ME! */
        private ViewTableModel setModel;

        /** DOCUMENT ME! */
        private JTable setTable;

        /**
         * Creates a parameter set display with the given description that will be used as the key.
         *
         * @param  description  description of this parameter set
         */
        public PSetDisplay(String description) {
            this.description = description;

            setLabel = new JLabel("Set: " + description);
            setLabel.setForeground(Color.black);
            setModel = new ViewTableModel();
            setTable = new JTable(setModel);

            for (int i = 0; i < 6; i++) {
                setModel.addColumn(parameterColumnNames[i]);
            }

            setTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
            setTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);

            setTable.getColumn("Name").setMinWidth(160);
            setTable.getColumn("Name").setMaxWidth(500);
            setTable.getColumn("Description").setMinWidth(50);
            setTable.getColumn("Description").setMaxWidth(200);
            setTable.getColumn("Value-Type").setMinWidth(70);
            setTable.getColumn("Value-Type").setMaxWidth(220);
            setTable.getColumn("Value").setMinWidth(50);
            setTable.getColumn("Value").setMaxWidth(1000);
            setTable.getColumn("Date").setMinWidth(50);
            setTable.getColumn("Date").setMaxWidth(200);
            setTable.getColumn("Time").setMinWidth(80);
            setTable.getColumn("Time").setMaxWidth(200);

            setTable.getTableHeader().setReorderingAllowed(false);
            setTable.getTableHeader().addMouseListener(new HeaderListener());
        }

        /**
         * Adds a new row of parameter data to the display's table.
         *
         * @param  paramName  parameter name
         * @param  paramDesc  parameter description
         * @param  valueType  parameter value type
         * @param  value      parameter value
         * @param  date       parameter date
         * @param  time       parameter time
         */
        public void addParameterData(String paramName, String paramDesc, String valueType, String value, String date,
                                     String time) {
            String[] rose = { paramName, paramDesc, valueType, value, date, time };
            setModel.addRow(rose);
        }

        /**
         * Gets the set display's description.
         *
         * @return  this parameter set's description
         */
        public String getDescription() {
            return this.description;
        }

        /**
         * Gets the label.
         *
         * @return  the label for this parameter set
         */
        public JLabel getLabel() {
            return this.setLabel;
        }

        /**
         * Gets the table data model.
         *
         * @return  parameter table data model
         */
        public ViewTableModel getModel() {
            return this.setModel;
        }

        /**
         * Gets the table of parameters.
         *
         * @return  parameter set table
         */
        public JTable getTable() {
            return this.setTable;
        }

        /**
         * Simple listener for the table header.
         */
        private class HeaderListener implements MouseListener {

            /**
             * When the user right clicks on a header, sorts the column. when the user left clicks, a popup menu gives
             * the option to add a parameter to the table, triggering a JDialog
             *
             * @param  e  event that triggered this method
             */
            public void mouseClicked(MouseEvent e) {
                Object source = e.getSource();
                Point p = e.getPoint();
                int col;

                if (source.equals(setTable.getTableHeader()) && (e.getButton() == MouseEvent.BUTTON1)) {
                    col = setTable.columnAtPoint(p);

                    if (e.isShiftDown()) {
                        sort(setModel, col, true);
                    } else {
                        sort(setModel, col, false);
                    }
                } else if (source.equals(setTable.getTableHeader()) && (e.getButton() == MouseEvent.BUTTON3)) {
                    setDescforAddParam = new String(getDescription());

                    JPopupMenu popUp = new JPopupMenu();

                    popUp.add(addParam);
                    popUp.show(setTable.getTableHeader(), p.x, p.y);
                }
            }

            /**
             * Unchanged.
             *
             * @param  e  the mouse event
             */
            public void mouseDragged(MouseEvent e) { }

            /**
             * Unchanged.
             *
             * @param  e  the mouse event
             */
            public void mouseEntered(MouseEvent e) { }

            /**
             * Unchanged.
             *
             * @param  e  the mouse event
             */
            public void mouseExited(MouseEvent e) { }

            /**
             * Unchanged.
             *
             * @param  e  the mouse event
             */
            public void mousePressed(MouseEvent e) { }

            /**
             * Unchanged.
             *
             * @param  e  the mouse event
             */
            public void mouseReleased(MouseEvent e) { }
        }
    }
}
