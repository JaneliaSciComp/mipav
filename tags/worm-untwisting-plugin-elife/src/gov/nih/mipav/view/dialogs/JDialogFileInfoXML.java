package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.util.*;

import javax.swing.*;


/**
 * This class shows the dialog which conatains the file-info header information as used in the FileInfoBase class.
 * 
 * <p>
 * it builds two tables, and any row can be made editable when supplied with the appropriate editor to use. Entries that
 * are edited okay, reports updates to file info.
 * </p>
 * 
 * <p>
 * It merely brings up a JDalogEditor when "edit" button is clicked.
 * </p>
 * 
 * <p>
 * <i>17 January 2002</i>: Right now, this class is set up to handle <i>only</i> FileInfoAnalyze edits. This is
 * because the
 * </p>
 * 
 * @author parsonsd;
 * @version 0.2
 */
public class JDialogFileInfoXML extends JDialogBase implements ActionListener {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -8790820691884630599L;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** menu item for adding parameter. */
    private final JMenuItem addParam;

    /** button for adding sets. */
    private JButton addSet;

    /** button for adding surfaces. */
    private JButton addSurface;

    /** edit button. */
    private JButton edit;

    /** expand dicom tags button. */
    private JButton expandTags;

    /** are the dicom tags expanded * */
    private boolean isExpanded = false;

    /** hashtable to store editor dialogs associated with the table. */
    private Hashtable<?,JDialogEditor> editorDialogTable;

    /** file info xml to be displayed. */
    private FileInfoImageXML fileinfo;

    /** model image associated with the FileInfo. */
    private final ModelImage image;

    /** model associated with investigator information. */
    private ViewTableModel investigatorModel;

    /** model associated with tag information. */
    private ViewTableModel tagModel;

    /** investigator information table. */
    private JTable investigatorTable;

    private JTable tagTable;

    /** master scroll pane in which to display all information. */
    private JScrollPane masterScrollPane;

    /** counter for number of sets within file info. */
    private int numSets = 0;

    /** array of strings for parameter column names. */
    private final String[] parameterColumnNames = {"Name", "Description", "Value-Type", "Value", "Date", "Time"};

    /** model associated with primary image information. */
    private ViewTableModel primaryModel;

    /** primary image information table. */
    private JTable primaryTable;

    // tpe holds the type of editor to be used; editor holds the editor dialog
    /** Type holds the type of editor, editor holds the actual editor dialog. */
    private final Hashtable<Integer,Vector<Integer>> primaryTypeHolder;
    private final Hashtable <Integer,JDialogEditor>primaryEditorHolder;

    private Hashtable<Integer,Vector<Integer>> subjectTypeHolder;
    private Hashtable<Integer,JDialogEditor> subjectEditorHolder;
    private Hashtable<Integer,Vector<Integer>> scanTypeHolder;
    private Hashtable<Integer,JDialogEditor> scanEditorHolder;
    private Hashtable<Integer,Vector<Integer>> investigatorTypeHolder;
    private Hashtable<Integer,JDialogEditor> investigatorEditorHolder;
    private Hashtable<Integer,Vector<Integer>> tagTypeHolder;

    /** button for removing parameters. */
    private JButton removeParam;

    /** button for removing surfaces. */
    private JButton removeSurface;

    /** model associated with scan information. */
    private ViewTableModel scanModel;

    /** scan infomation table. */
    private JTable scanTable;

    /** Box to hold table information. */
    private Box scrollingBox;

    private JLabel tagLabel;

    private TableSorter sorter;

    /** string holding set description when adding a parameter. */
    private String setDescforAddParam;

    /** hash table holding set information. */
    private final Hashtable<String,PSetDisplay> setHashtable;

    /** model associated with subject information. */
    private ViewTableModel subjectModel;

    /** subject information table. */
    private JTable subjectTable;

    /** array of strings for surface column names. */
    private final String[] surfaceColumnNames = {"Path", "Opacity", "Load"};

    /** for displaying surfaces. */
    private final SurfaceDisplay surfaces;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogFileInfoXML object.
     * 
     * @param parent Frame
     * @param title String
     * @param img ModelImage
     */
    public JDialogFileInfoXML(final Frame parent, final String title, final ModelImage img) {
        super(parent, false);
        setTitle(title);
        try {
            setIconImage(MipavUtil.getIconImage("header.gif"));
        } catch (final Exception e) {
            // setIconImage() is not part of the Java 1.5 API - catch any runtime error on those systems
        }
        image = img;
        primaryTypeHolder = new Hashtable<Integer,Vector<Integer>>(); // all editable lines in primary, keyed by location in Jtable
        primaryEditorHolder = new Hashtable<Integer,JDialogEditor>(); // all editable lines in primary, keyed by location in Jtable
        surfaces = new SurfaceDisplay();

        setHashtable = new Hashtable<String,PSetDisplay>();
        addParam = new JMenuItem("Add Parameter");
        addParam.setActionCommand("AddParameter");
        addParam.addActionListener(this);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * permits the caller to get a value out of the primary table by using the name given to the fileInfo.
     * 
     * @param name DOCUMENT ME!
     * 
     * @return the value returned is the first value which keys to this name; any other instances of the name will be
     *         ignored. <code>Null</code> is returned if the name cannot be found
     */
    public String accessPrimaryData(final String name) {
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
     * closes the dialog when the user clicks close.
     * 
     * <p>
     * Creates editor dialogs to allow changing the value-field of a tag when user clicks "Edit Tag" button. This
     * implmentation supports virtually any number of tag editors, bringing forward any previously opened editor. Most
     * processing occurs when this class hears an editor window close;
     * </p>
     * 
     * <p>
     * will alert any open window (frame) to set title as that information may have changed.
     * </p>
     * 
     * <p>
     * to make this more FileInfoBase friendly, add a public static void stateChanged(Vector) to FileInfoBase. Then
     * remove the references to the cast. Otherwise, using the editors with other varieties of FileInfo will throw
     * ClassCastExceptions. Also suggest that a distinct datatype (other than Vector) be created to handle the special
     * needs.
     * </p>
     * 
     * @param e event that triggered this action
     */
    @SuppressWarnings("unchecked")
    public void actionPerformed(final ActionEvent e) {
        JDialogEditor editor;

        if (e.getActionCommand().equals("Close")) { // close

            // clear out the editor dialog boxes
            for (final Enumeration<JDialogEditor> en = editorDialogTable.elements(); en.hasMoreElements();) {
                editor = en.nextElement();
                editor.dispose();
            }

            editorDialogTable.clear();
            this.dispose(); // remove self
        } else if (e.getActionCommand().equals("RemoveParameter")) {
            final Enumeration<String> eset = setHashtable.keys();
            boolean atLeastOne = false;

            while (eset.hasMoreElements()) {
                final String psetdesc = eset.nextElement();
                final PSetDisplay pset = setHashtable.get(psetdesc);
                final int[] rows = pset.getTable().getSelectedRows();
                final boolean[] deleterows = new boolean[rows.length];
                final String[] paramNames = new String[rows.length];

                for (int i = 0; i < rows.length; i++) {
                    paramNames[i] = new String((String) pset.getModel().getValueAt(rows[i], 0));

                    final int reply = JOptionPane.showConfirmDialog(this, "Remove Parameter (" + paramNames[i]
                            + ") from Set (" + psetdesc + ")?", "Confirm Remove Parameter", JOptionPane.YES_NO_OPTION,
                            JOptionPane.QUESTION_MESSAGE);

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

            // if there is at least one parameter deleted, we need to update all fileinfos
            if (atLeastOne) {

                for (int q = 0; q < image.getFileInfo().length; q++) {
                    fileinfo.updateFileInfos((FileInfoXML) image.getFileInfo(q));
                }
            }
        } else if (e.getActionCommand().equals("AddParameter")) {

            // System.out.println("going to add parameter");
            if (this.setDescforAddParam != null) {
                // Create an editor to set the Parameter Name
                // (required and non editable post-creation)

                final String[] values = new String[6];

                values[0] = new String("Name");
                values[1] = new String("Description");
                values[2] = new String("Value Type");
                values[3] = new String("Value");
                values[4] = new String("0000-00-00");
                values[5] = new String("00:00:00-00:00");

                final int[] editors = new int[6];

                editors[0] = JDialogEditor.XML_SETORPARAM;
                editors[1] = JDialogEditor.STRING;
                editors[2] = JDialogEditor.XML_VALUETYPE;
                editors[3] = JDialogEditor.STRING;
                editors[4] = JDialogEditor.XML_DATE;
                editors[5] = JDialogEditor.XML_TIME;
                editor = new JDialogEditor(this, new String("NewParameter"), values, editors);
                editor.setTitle("New Parameter Data");
                editor.setVisible(true);

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

                    public void windowClosed(final WindowEvent e) {
                        JDialogEditor ed;

                        ed = (JDialogEditor) e.getSource();

                        if (ed.wasDialogOkay() && !alreadyClosed) {
                            alreadyClosed = true;

                            final String s = ed.getDisplayValueForParam();
                            final StringTokenizer dt = new StringTokenizer(s, "\\");

                            if (dt.hasMoreElements()) {
                                final String name = dt.nextToken();
                                final String desc = dt.nextToken();
                                final String vt = dt.nextToken();
                                final String val = dt.nextToken();
                                final String date = dt.nextToken();
                                final String time = dt.nextToken();

                                appendParameter(setDescforAddParam, name, desc, vt, val, date, time);

                                // update fileinfo
                                final XMLPSet temp = fileinfo.getPSet(setDescforAddParam);

                                temp.addParameter(name);
                                temp.getParameter(name).setDescription(desc);
                                temp.getParameter(name).setValueType(vt);
                                temp.getParameter(name).setDate(date);
                                temp.getParameter(name).setTime(time);

                                for (int q = 0; q < image.getFileInfo().length; q++) {
                                    fileinfo.updateFileInfos((FileInfoXML) image.getFileInfo(q));
                                }
                            }
                        }
                    }
                });
            }
        } else if (e.getActionCommand().equals("AddSet")) {
            this.numSets++;

            final String[] values = new String[7];

            values[0] = new String("Set Description");
            values[1] = new String("Parameter Name");
            values[2] = new String("Parameter Description");
            values[3] = new String("Parameter Value Type");
            values[4] = new String("Parameter Value");
            values[5] = new String("0000-00-00");
            values[6] = new String("00:00:00-00:00");

            final int[] editors = new int[7];

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

                public void windowClosed(final WindowEvent e) {
                    JDialogEditor ed;

                    ed = (JDialogEditor) e.getSource();

                    if (ed.wasDialogOkay() && !alreadyClosed) {
                        alreadyClosed = true;

                        final String s = ed.getDisplayValueForParam();
                        final StringTokenizer dt = new StringTokenizer(s, "\\");

                        if (dt.hasMoreElements()) {
                            final String psetdesc = dt.nextToken();
                            final String name = dt.nextToken();
                            final String desc = dt.nextToken();
                            final String vt = dt.nextToken();
                            final String val = dt.nextToken();
                            final String date = dt.nextToken();
                            final String time = dt.nextToken();

                            setHashtable.put(psetdesc, new PSetDisplay(psetdesc));

                            scrollingBox.add( (setHashtable.get(psetdesc)).getLabel());
                            scrollingBox.add( (setHashtable.get(psetdesc)).getTable().getTableHeader());
                            scrollingBox.add( (setHashtable.get(psetdesc)).getTable());

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

                            for (int q = 0; q < image.getFileInfo().length; q++) {
                                fileinfo.updateFileInfos((FileInfoXML) image.getFileInfo(q));
                            }
                        }

                    }
                }
            });
        } else if (e.getActionCommand().equals("AddSurface")) {
            final String[] values = new String[3];

            // / get the default directory from user interface
            values[0] = new String("");
            values[1] = new String("1.0");
            values[2] = new String("true");

            final int[] editors = new int[3];
            editors[0] = JDialogEditor.FILE_STRING;
            editors[1] = JDialogEditor.STRING;
            editors[2] = JDialogEditor.BOOLEAN;

            editor = new JDialogEditor(this, "NewSurface", values, editors);
            editor.setTitle("Add New Surface");
            editor.setVisible(true);

            editor.addWindowListener(new WindowAdapter() {

                // when windowClosed: check to see if dialog closed by OKbutton (if it did then there
                // were no problems with user inputted values); if it closed by okbutton, then
                // modify all slices if user so desired.
                // In addition, when the change was applied to all slices, notify the image frames
                // in the image frame vector to reset the title (title changes only for name,
                // @see ModelImage#setTitle(String)
                //
                private boolean alreadyClosed = false; // was the editor closed?

                public void windowClosed(final WindowEvent e) {
                    JDialogEditor ed;

                    ed = (JDialogEditor) e.getSource();

                    if (ed.wasDialogOkay() && !alreadyClosed) {
                        alreadyClosed = true;

                        final String s = ed.getDisplayValueForInfo(); // get data with #### separator
                        final StringTokenizer dt = new StringTokenizer(s, "####");

                        if (dt.hasMoreElements()) {
                            final String file = dt.nextToken();
                            final float opacity = Float.parseFloat(dt.nextToken());
                            final boolean display = Boolean.valueOf(dt.nextToken()).booleanValue();

                            surfaces.addSurface(file, opacity, display);

                            getContentPane().validate();

                            fileinfo.addSurface(file);
                            fileinfo.getSurface(file).setOpacity(opacity);
                            fileinfo.getSurface(file).setDisplay(display);

                            for (int q = 0; q < image.getFileInfo().length; q++) {
                                fileinfo.updateFileInfos((FileInfoXML) image.getFileInfo(q));
                            }
                        }
                    }
                }
            });
        } else if (e.getActionCommand().equals("RemoveSurface")) {
            boolean atLeastOne = false;
            final int[] rows = surfaces.getTable().getSelectedRows();
            final boolean[] deleterows = new boolean[rows.length];
            final String[] surPaths = new String[rows.length];

            for (int i = 0; i < rows.length; i++) {
                surPaths[i] = new String((String) surfaces.getModel().getValueAt(rows[i], 0));

                final int reply = JOptionPane.showConfirmDialog(this, "Remove Surface (" + surPaths[i] + ")?",
                        "Confirm Remove Surface", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);

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
                    fileinfo.removeSurface(surPaths[i]);
                    surfaces.getModel().removeRow(rows[i]);
                    getContentPane().validate();
                }
            }

            // if there is at least one parameter deleted, we need to update all fileinfos
            if (atLeastOne) {

                for (int q = 0; q < image.getFileInfo().length; q++) {
                    fileinfo.updateFileInfos((FileInfoXML) image.getFileInfo(q));
                }
            }

        } else if (e.getActionCommand().equals("ExpandTags")) {
            if ( !isExpanded) {
                scrollingBox.remove(tagLabel);
                scrollingBox.remove(tagTable.getTableHeader());
                scrollingBox.remove(tagTable);

                // create set displays for each set
                final Enumeration<String> a = fileinfo.getPSetKeys();

                while (a.hasMoreElements()) {
                    final String desc = fileinfo.getPSet(a.nextElement()).getDescription();

                    setHashtable.put(desc, new PSetDisplay(desc));
                    scrollingBox.add( (setHashtable.get(desc)).getLabel());
                    scrollingBox.add( (setHashtable.get(desc)).getTable().getTableHeader());
                    scrollingBox.add( (setHashtable.get(desc)).getTable());
                }

                final Enumeration<String> ee = fileinfo.getPSetKeys();

                while (ee.hasMoreElements()) {
                    final XMLPSet temp = fileinfo.getPSet(ee.nextElement());
                    final String desc = temp.getDescription();
                    final Enumeration<String> pe = temp.getParameterKeys();
                    final int editorChoice[] = new int[1];
                    while (pe.hasMoreElements()) {
                        final XMLParameter tp = temp.getParameter(pe.nextElement());
                        editorChoice[0] = JDialogEditor.STRING;
                        appendParameter(desc, tp.getName(), tp.getDescription(), tp.getValueType(), tp.getValue(), tp
                                .getDate(), tp.getTime());

                    }
                }
                isExpanded = true;

                expandTags.setText("Collapse Tags");
            } else {
                final Enumeration<String> a = fileinfo.getPSetKeys();

                while (a.hasMoreElements()) {
                    final String desc = fileinfo.getPSet(a.nextElement()).getDescription();

                    // setHashtable.put(desc, new PSetDisplay(desc));
                    scrollingBox.remove( (setHashtable.get(desc)).getLabel());
                    scrollingBox.remove( (setHashtable.get(desc)).getTable().getTableHeader());
                    scrollingBox.remove( (setHashtable.get(desc)).getTable());
                }

                scrollingBox.add(tagLabel);
                scrollingBox.add(tagTable.getTableHeader());
                scrollingBox.add(tagTable);

                scrollingBox.repaint();
                masterScrollPane.revalidate();
                isExpanded = false;

                expandTags.setText("Expand Tags");
            }

        } else if (e.getActionCommand().equals("EditTag")) { // edit the high-lighted tag

            Integer named;
            int[] rows;
            int i = 0;
            Object obj;
            Vector<Integer> objs;

            // go through primary table, editing highlighted (selected ) rows
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
                        objs = primaryTypeHolder.get(named);

                        String[] values = new String[1]; // this is unclean design now to cover something i didn't
                        // foresee

                        if (objs.size() > 1) { // assume seperate words mean something special

                            // when there is more than one editor specified.
                            values = separateValues((String) primaryTable.getValueAt(rows[i], 1));
                        } else {
                            values[0] = (String) primaryTable.getValueAt(rows[i], 1);
                        }

                        final int[] editors = new int[objs.size()];

                        for (int j = 0; j < objs.size(); j++) {
                            editors[j] = ((objs.elementAt(j))).intValue(); // what the user asked for
                        }

                        editor = new JDialogEditor(this, named, values, editors);
                        editor.setTitle((String) primaryTable.getValueAt(rows[i], 0));
                        editor.setVisible(true);
                        editor.addWindowListener(new WindowAdapter() {

                            // when windowClosed: check to see if dialog closed by OKbutton (if it did then there
                            // were no problems with user inputted values); if it closed by okbutton, then
                            // modify all slices if user so desired.
                            // In addition, when the change was applied to all slices, notify the image frames
                            // in the image frame vector to reset the title (title changes only for name,
                            // @see ModelImage#setTitle(String)
                            //
                            private boolean alreadyClosed = false; // was the editor closed?

                            public void windowClosed(final WindowEvent e) {
                                JDialogEditor ed;

                                ed = (JDialogEditor) e.getSource();

                                final Integer edID = (Integer) ed.getKey();
                                Vector<Object> changed = new Vector<Object>(5);

                                changed.add(0, new Integer(1)); // table
                                changed.add(1, edID); // line
                                changed.add(2, primaryModel.getValueAt(edID.intValue(), 0));
                                changed.add(3, ed.getValue());
                                changed.add(4, ed.getDisplayValue());

                                if (ed.wasDialogOkay() && !alreadyClosed) {
                                    alreadyClosed = true;

                                    // to make this more FileInfoBase friendly, add a
                                    // public static void stateChanged(Vector)
                                    // to FileInfoBase. Then remove the references to
                                    // the cast. Otherwise, using the editors with other
                                    // varieties of FileInfo will throw ClassCastExceptions.
                                    // Also suggest that a distinct datatype (other than Vector)
                                    // be created to handle the special needs.
                                    fileinfo.stateChanged(changed); // what to do to update fileinfo!?!?!
                                    primaryModel.setValueAt(ed.getDisplayValue(), edID.intValue(), 1);
                                    primaryEditorHolder.remove(edID);

                                    // a really nice idea: add an option (default ON, of course)
                                    // for fileInfo propogation, that this would do in a local
                                    // fileInfo friendly way--ie., the fileInfo knows which
                                    // values should be propogated.
                                    // Loop through other fileinfos from image.
                                    for (int q = 0; q < image.getFileInfo().length; q++) {
                                        fileinfo.updateFileInfos((FileInfoXML) image.getFileInfo(q));
                                    }
                                } else {
                                    primaryEditorHolder.remove(edID);
                                    changed = null; // forget it
                                }

                            }
                        });
                        primaryEditorHolder.put(named, editor);
                    }
                } else {}
            }

            // now for subject information rows

            // go through subject table, editing highlighted (selected) rows
            rows = subjectTable.getSelectedRows();

            for (i = 0; i < rows.length; i++) {

                // determine if this row can be edited
                named = new Integer(rows[i]);

                if (subjectTypeHolder.containsKey(named)) { // if named
                    subjectTable.removeRowSelectionInterval(rows[i], rows[i]);

                    // if previously named, bring the editors to the front
                    obj = subjectEditorHolder.get(named);

                    if (obj != null) {
                        ((JDialogEditor) (obj)).toFront();
                    } else {
                        objs = subjectTypeHolder.get(named);

                        String[] values = new String[1]; // this is unclean design now to cover something i didn't
                        // foresee

                        if (objs.size() > 1) { // assume seperate words mean something special

                            // when there is more than one editor specified.
                            values = separateValues((String) subjectTable.getValueAt(rows[i], 1));
                        } else {
                            values[0] = (String) subjectTable.getValueAt(rows[i], 1);
                        }

                        final int[] editors = new int[objs.size()]; // because we tell an editor to do something based
                        // on an
                        // int code

                        for (int j = 0; j < objs.size(); j++) {
                            editors[j] = ((objs.elementAt(j))).intValue(); // what the user asked for
                        }

                        editor = new JDialogEditor(this, named, values, editors);
                        editor.setTitle((String) subjectTable.getValueAt(rows[i], 0));
                        editor.setVisible(true);
                        editor.addWindowListener(new WindowAdapter() {

                            // when windowClosed: check to see if dialog closed by OKbutton (if it did then there
                            // were no problems with user inputted values); if it closed by okbutton, then
                            // modify all slices if user so desired.
                            // In addition, when the change was applied to all slices, notify the image frames
                            // in the image frame vector to reset the title (title changes only for name,
                            // @see ModelImage#setTitle(String)
                            //
                            private boolean alreadyClosed = false; // was the editor closed?

                            public void windowClosed(final WindowEvent e) {

                                // alreadyClosed = true;
                                JDialogEditor ed;

                                ed = (JDialogEditor) e.getSource();
                                ed.removeWindowListener(ed);

                                final Integer edID = (Integer) ed.getKey();
                                Vector<Object> changed = new Vector<Object>(4);

                                changed.add(0, new Integer(1)); // table
                                changed.add(1, edID); // line
                                changed.add(2, subjectModel.getValueAt(edID.intValue(), 0));
                                changed.add(3, ed.getValue());
                                changed.add(4, ed.getDisplayValue());

                                // System.err.println(ed.wasDialogOkay() + " " + alreadyClosed);
                                if (ed.wasDialogOkay() && !alreadyClosed) {

                                    // to make this more FileInfoBase friendly, add a
                                    // public static void stateChanged(Vector)
                                    // to FileInfoBase. Then remove the references to
                                    // the cast. Otherwise, using the editors with other
                                    // varieties of FileInfo will throw ClassCastExceptions.
                                    // Also suggest that a distinct datatype (other than Vector)
                                    // be created to handle the special needs.
                                    fileinfo.stateChanged(changed); // what to do to update fileinfo!?!?!
                                    subjectModel.setValueAt(ed.getDisplayValue(), edID.intValue(), 1);
                                    subjectEditorHolder.remove(edID);

                                    // a really nice idea: add an option (default ON, of course)
                                    // for fileInfo propogation, that this would do in a local
                                    // fileInfo friendly way--ie., the fileInfo knows which
                                    // values should be propogated.
                                    // Loop through other fileinfos from image.
                                    if (image.getFileInfo() != null) {

                                        for (int q = 0; q < image.getFileInfo().length; q++) {
                                            fileinfo.updateFileInfos((FileInfoXML) image.getFileInfo(q));
                                        }
                                    }
                                } else {
                                    subjectEditorHolder.remove(edID);
                                    changed = null; // forget it
                                }
                            }
                        });
                        subjectEditorHolder.put(named, editor);
                    }
                } else {}
            }

            // selected rows on scan table
            rows = scanTable.getSelectedRows();

            for (i = 0; i < rows.length; i++) {

                // determine if this row can be edited
                named = new Integer(rows[i]);

                if (scanTypeHolder.containsKey(named)) { // if named
                    scanTable.removeRowSelectionInterval(rows[i], rows[i]);

                    // if previously named, bring the editors to the front
                    obj = scanEditorHolder.get(named);

                    if (obj != null) {
                        ((JDialogEditor) (obj)).toFront();
                    } else {
                        objs = scanTypeHolder.get(named);

                        String[] values = new String[1]; // this is unclean design now to cover something i didn't
                        // foresee

                        if (objs.size() > 1) { // assume seperate words mean something special

                            // when there is more than one editor specified.
                            values = separateValues((String) scanTable.getValueAt(rows[i], 1));
                        } else {
                            values[0] = (String) scanTable.getValueAt(rows[i], 1);
                        }

                        final int[] editors = new int[objs.size()]; // because we tell an editor to do something based
                        // on an
                        // int code

                        for (int j = 0; j < objs.size(); j++) {
                            editors[j] = ((objs.elementAt(j))).intValue(); // what the user asked for
                        }

                        editor = new JDialogEditor(this, named, values, editors);
                        editor.setTitle((String) scanTable.getValueAt(rows[i], 0));
                        editor.setVisible(true);
                        editor.addWindowListener(new WindowAdapter() {

                            // when windowClosed: check to see if dialog closed by OKbutton (if it did then there
                            // were no problems with user inputted values); if it closed by okbutton, then
                            // modify all slices if user so desired.
                            // In addition, when the change was applied to all slices, notify the image frames
                            // in the image frame vector to reset the title (title changes only for name,
                            // @see ModelImage#setTitle(String)
                            //
                            private boolean alreadyClosed = false; // was the editor closed?

                            public void windowClosed(final WindowEvent e) {

                                // alreadyClosed = true;
                                JDialogEditor ed;

                                ed = (JDialogEditor) e.getSource();

                                final Integer edID = (Integer) ed.getKey();
                                Vector<Object> changed = new Vector<Object>(4);

                                changed.add(0, new Integer(1)); // table
                                changed.add(1, edID); // line
                                changed.add(2, scanModel.getValueAt(edID.intValue(), 0));
                                changed.add(3, ed.getValue());
                                changed.add(4, ed.getDisplayValue());

                                if (ed.wasDialogOkay() && !alreadyClosed) {

                                    // to make this more FileInfoBase friendly, add a
                                    // public static void stateChanged(Vector)
                                    // to FileInfoBase. Then remove the references to
                                    // the cast. Otherwise, using the editors with other
                                    // varieties of FileInfo will throw ClassCastExceptions.
                                    // Also suggest that a distinct datatype (other than Vector)
                                    // be created to handle the special needs.
                                    fileinfo.stateChanged(changed); // what to do to update fileinfo!?!?!
                                    scanModel.setValueAt(ed.getDisplayValue(), edID.intValue(), 1);
                                    scanEditorHolder.remove(edID);

                                    // a really nice idea: add an option (default ON, of course)
                                    // for fileInfo propogation, that this would do in a local
                                    // fileInfo friendly way--ie., the fileInfo knows which
                                    // values should be propogated.
                                    // Loop through other fileinfos from image.
                                    for (int q = 0; q < image.getFileInfo().length; q++) {
                                        fileinfo.updateFileInfos((FileInfoXML) image.getFileInfo(q));
                                    }
                                } else {
                                    scanEditorHolder.remove(edID);
                                    changed = null; // forget it
                                }
                            }
                        });
                        scanEditorHolder.put(named, editor);
                    }
                } else {}
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
                        objs = investigatorTypeHolder.get(named);

                        String[] values = new String[1]; // this is unclean design now to cover something i didn't
                        // foresee

                        if (objs.size() > 1) { // assume seperate words mean something special

                            // when there is more than one editor specified.
                            values = separateValues((String) investigatorTable.getValueAt(rows[i], 1));
                        } else {
                            values[0] = (String) investigatorTable.getValueAt(rows[i], 1);
                        }

                        final int[] editors = new int[objs.size()]; // because we tell an editor to do something based
                        // on an
                        // int code

                        for (int j = 0; j < objs.size(); j++) {
                            editors[j] = ((objs.elementAt(j))).intValue(); // what the user asked for
                        }

                        editor = new JDialogEditor(this, named, values, editors);
                        editor.setTitle((String) investigatorTable.getValueAt(rows[i], 0));
                        editor.setVisible(true);
                        editor.addWindowListener(new WindowAdapter() {

                            // when windowClosed: check to see if dialog closed by OKbutton (if it did then there
                            // were no problems with user inputted values); if it closed by okbutton, then
                            // modify all slices if user so desired.
                            // In addition, when the change was applied to all slices, notify the image frames
                            // in the image frame vector to reset the title (title changes only for name,
                            // @see ModelImage#setTitle(String)
                            //
                            private boolean alreadyClosed = false;

                            public void windowClosed(final WindowEvent e) {

                                // alreadyClosed = true;
                                JDialogEditor ed;

                                ed = (JDialogEditor) e.getSource();

                                final Integer edID = (Integer) ed.getKey();
                                Vector<Object> changed = new Vector<Object>(4);

                                changed.add(0, new Integer(1)); // table
                                changed.add(1, edID); // line
                                changed.add(2, investigatorModel.getValueAt(edID.intValue(), 0));
                                changed.add(3, ed.getValue());
                                changed.add(4, ed.getDisplayValue());

                                if (ed.wasDialogOkay() && !alreadyClosed) {

                                    // to make this more FileInfoBase friendly, add a
                                    // public static void stateChanged(Vector)
                                    // to FileInfoBase. Then remove the references to
                                    // the cast. Otherwise, using the editors with other
                                    // varieties of FileInfo will throw ClassCastExceptions.
                                    // Also suggest that a distinct datatype (other than Vector)
                                    // be created to handle the special needs.
                                    fileinfo.stateChanged(changed); // what to do to update fileinfo!?!?!
                                    investigatorModel.setValueAt(ed.getDisplayValue(), edID.intValue(), 1);
                                    investigatorEditorHolder.remove(edID);

                                    // a really nice idea: add an option (default ON, of course)
                                    // for fileInfo propogation, that this would do in a local
                                    // fileInfo friendly way--ie., the fileInfo knows which
                                    // values should be propogated.
                                    // Loop through other fileinfos from image.
                                    for (int q = 0; q < image.getFileInfo().length; q++) {
                                        fileinfo.updateFileInfos((FileInfoXML) image.getFileInfo(q));
                                    }
                                } else {
                                    investigatorEditorHolder.remove(edID);
                                    changed = null; // forget it
                                }
                            }
                        });
                        investigatorEditorHolder.put(named, editor);
                    }
                } else {}
            }

            // Check to see if any Parameters are highlighted
            final Enumeration<String> eset = setHashtable.keys();

            while (eset.hasMoreElements()) {
                final PSetDisplay pset = setHashtable.get(eset.nextElement());

                rows = pset.getTable().getSelectedRows();

                for (final int element : rows) {
                    pset.getTable().removeRowSelectionInterval(rows[i], rows[i]);

                    final String[] values = new String[5];

                    values[0] = (String) pset.getTable().getValueAt(element, 1);
                    values[1] = (String) pset.getTable().getValueAt(element, 2);
                    values[2] = (String) pset.getTable().getValueAt(element, 3);
                    values[3] = (String) pset.getTable().getValueAt(element, 4);
                    values[4] = (String) pset.getTable().getValueAt(element, 5);

                    final int[] editors = new int[5];

                    editors[0] = JDialogEditor.STRING;
                    editors[1] = JDialogEditor.XML_VALUETYPE;
                    editors[2] = JDialogEditor.STRING;
                    editors[3] = JDialogEditor.XML_DATE;
                    editors[4] = JDialogEditor.XML_TIME;

                    editor = new JDialogEditor(this, pset.getDescription(), values, editors);
                    editor.setTitle(pset.getDescription() + "-" + (String) pset.getTable().getValueAt(element, 0));
                    editor.setVisible(true);
                    editor.setPSetDescription(pset.getDescription());
                    editor.setRow(element);
                    editor.addWindowListener(new WindowAdapter() {

                        // when windowClosed: check to see if dialog closed by OKbutton (if it did then there
                        // were no problems with user inputted values); if it closed by okbutton, then
                        // modify all slices if user so desired.
                        // In addition, when the change was applied to all slices, notify the image frames
                        // in the image frame vector to reset the title (title changes only for name,
                        // @see ModelImage#setTitle(String)
                        //
                        private boolean alreadyClosed = false;

                        public void windowClosed(final WindowEvent e) {
                            JDialogEditor ed;

                            ed = (JDialogEditor) e.getSource();

                            if (ed.wasDialogOkay() && !alreadyClosed) {
                                alreadyClosed = true;

                                final String s = ed.getDisplayValueForParam();
                                final StringTokenizer st = new StringTokenizer(s, "\\");

                                if (st.hasMoreElements()) {
                                    final String desc = st.nextToken();
                                    final String vt = st.nextToken();
                                    final String val = st.nextToken();
                                    final String date = st.nextToken();
                                    final String time = st.nextToken();
                                    final PSetDisplay temp = setHashtable.get(ed.getPSetDescription());
                                    final int row = ed.getRow();

                                    temp.getModel().setValueAt(desc, row, 1);
                                    temp.getModel().setValueAt(vt, row, 2);
                                    temp.getModel().setValueAt(val, row, 3);
                                    temp.getModel().setValueAt(date, row, 4);
                                    temp.getModel().setValueAt(time, row, 5);

                                    final Vector<String> pData = new Vector<String>(7);

                                    pData.add(0, ed.getPSetDescription());
                                    pData.add(1, (String)temp.getModel().getValueAt(row, 0));
                                    pData.add(2, desc);
                                    pData.add(3, vt);
                                    pData.add(4, val);
                                    pData.add(5, date);
                                    pData.add(6, time);
                                    fileinfo.parameterChanged(pData);

                                    for (int q = 0; q < image.getFileInfo().length; q++) {
                                        fileinfo.updateFileInfos((FileInfoXML) image.getFileInfo(q));
                                    }
                                }
                            }
                        }
                    });
                }
            }

            // surfaces
            rows = surfaces.getTable().getSelectedRows();

            for (final int element : rows) {
                surfaces.getTable().removeRowSelectionInterval(rows[i], rows[i]);

                final String[] values = new String[3];
                values[0] = (String) surfaces.getTable().getValueAt(element, 0);
                values[1] = (String) surfaces.getTable().getValueAt(element, 1);
                values[2] = (String) surfaces.getTable().getValueAt(element, 2);

                final int[] editors = new int[3];
                editors[0] = JDialogEditor.FILE_STRING;
                editors[1] = JDialogEditor.STRING;
                editors[2] = JDialogEditor.BOOLEAN;

                editor = new JDialogEditor(this, values[0], values, editors);
                editor.setTitle(values[0]);
                editor.setVisible(true);
                editor.setRow(element);
                editor.addWindowListener(new WindowAdapter() {

                    // when windowClosed: check to see if dialog closed by OKbutton (if it did then there
                    // were no problems with user inputted values); if it closed by okbutton, then
                    // modify all slices if user so desired.
                    // In addition, when the change was applied to all slices, notify the image frames
                    // in the image frame vector to reset the title (title changes only for name,
                    // @see ModelImage#setTitle(String)
                    //
                    private boolean alreadyClosed = false;

                    public void windowClosed(final WindowEvent e) {
                        JDialogEditor ed;

                        ed = (JDialogEditor) e.getSource();

                        if (ed.wasDialogOkay() && !alreadyClosed) {
                            alreadyClosed = true;

                            final String s = ed.getDisplayValueForInfo(); // get data with #### separator
                            final StringTokenizer st = new StringTokenizer(s, "####");

                            if (st.hasMoreElements()) {
                                final String file = st.nextToken();
                                final float opacity = Float.parseFloat(st.nextToken());
                                final boolean display = Boolean.valueOf(st.nextToken()).booleanValue();

                                final int row = ed.getRow();

                                final String oldFile = (String) surfaces.getModel().getValueAt(row, 0);

                                surfaces.getModel().setValueAt(file, row, 0);
                                surfaces.getModel().setValueAt(new String("" + opacity), row, 1);
                                surfaces.getModel().setValueAt(new String("" + display), row, 2);

                                fileinfo.removeSurface(oldFile);
                                fileinfo.addSurface(file);
                                fileinfo.getSurface(file).setOpacity(opacity);
                                fileinfo.getSurface(file).setDisplay(display);

                                for (int q = 0; q < image.getFileInfo().length; q++) {
                                    fileinfo.updateFileInfos((FileInfoXML) image.getFileInfo(q));
                                }
                            }
                        }
                    }
                });
            }
        } else {
            super.actionPerformed(e);
        }

    }

    /**
     * appends an editable row to the end of the investigators table.
     * 
     * @param name DOCUMENT ME!
     * @param value DOCUMENT ME!
     * @param editor - list of editor types for editing this row
     */
    public void appendInvestigatorData(final String name, final String value, final int[] editor) {
        final String[] rose = {name, value};

        investigatorModel.addRow(rose);

        final Vector<Integer> editorInts = new Vector<Integer>();

        for (final int element : editor) { // set the list of editors to use
            editorInts.addElement(new Integer(element));
        }

        investigatorTypeHolder.put(new Integer(investigatorModel.getRowCount() - 1), editorInts);
    }

    /**
     * appends an editable row to the end of the tag table.
     * 
     * @param name DOCUMENT ME!
     * @param value DOCUMENT ME!
     * @param date
     * @param editor - list of editor types for editing this row
     */
    public void appendTagData(final String tag, final String name, final String value, final int[] editor) {

        final String[] rose = {tag, name, value};

        tagModel.addRow(rose);
        final Vector<Integer> editorInts = new Vector<Integer>();

        for (final int element : editor) { // set the list of editors to use
            editorInts.addElement(new Integer(element));
        }

        tagTypeHolder.put(new Integer(tagModel.getRowCount() - 1), editorInts);
        // tagTable.setAutoCreateRowSorter(true);
        // tagTable.getRowSorter().toggleSortOrder(0);
        sorter.setSortingStatus(0, TableSorter.ASCENDING);

    }

    /**
     * appends an editable row with the given parameter data to the proper set display table.
     * 
     * @param setDesc String
     * @param paramName String
     * @param paramDesc String
     * @param valueType String
     * @param value String
     * @param date String
     * @param time String
     */
    public void appendParameter(final String setDesc, final String paramName, final String paramDesc,
            final String valueType, final String value, String date, String time) {

        if (date == null) {
            date = new String("0000-01-01");
        }

        if (time == null) {
            time = new String("00:00:00-00:00");
        }

        (setHashtable.get(setDesc)).addParameterData(paramName, paramDesc, valueType, value, date, time);
    }

    /**
     * appends a non editable row to the end of the primary table.
     * 
     * @param name - file info parameter (ie., dimensions, extents, &c).
     * @param value - value assigned to a fileinfo parameter
     */
    public void appendPrimaryData(final String name, final String value) {

        if (value.indexOf('\n') == -1) { // \n doesn't occur in the value

            final String[] rose = {name, value};

            primaryModel.addRow(rose);
        } else {
            final StringTokenizer stok = new StringTokenizer(value, "\n");
            final String[] values = new String[stok.countTokens()];
            int i = 0;

            while (stok.hasMoreTokens()) {
                values[i++] = stok.nextToken();
            }

            final String[] rose = {name, values[0]};

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
     * appends a row to the end of the primary info table. assigns this name/value pair to be editable and adds the
     * fileinfo to listen for this name.
     * 
     * @param name file info parameter (ie., dimensions, extents, &c).
     * @param value value assigned to a fileinfo parameter
     * @param editor The value of editor is the editor interface to be used. Eg., a JPanelEditDefault. Specified by
     * 
     * <ul>
     * <li>JDialogFileInfo#IntString</li>
     * <li>JDialogFileInfo#FloatString</li>
     * <li>JDialogFileInfo#AnalyzeDataType</li>
     * <li>JDialogFileInfo#AnalyzeDescription</li>
     * <li>JDialogFileInfo#AnalyzeOrientation</li>
     * </ul>
     */
    public void appendPrimaryData(final String name, final String value, final int[] editor) {
        final String[] rose = {name, value};

        primaryModel.addRow(rose);

        final Vector<Integer> editorInts = new Vector<Integer>();

        for (final int element : editor) { // set the list of editors to use
            editorInts.addElement(new Integer(element));
        }

        primaryTypeHolder.put(new Integer(primaryModel.getRowCount() - 1), editorInts);
    }

    /**
     * appends an editable row to the end of the scan information table.
     * 
     * @param name DOCUMENT ME!
     * @param value DOCUMENT ME!
     * @param editor - list of editor types for editing this row
     */
    public void appendScanData(final String name, final String value, final int[] editor) {
        final String[] rose = {name, value};

        scanModel.addRow(rose);

        final Vector<Integer> editorInts = new Vector<Integer>();

        for (final int element : editor) { // set the list of editors to use
            editorInts.addElement(new Integer(element));
        }

        scanTypeHolder.put(new Integer(scanModel.getRowCount() - 1), editorInts);
    }

    /**
     * appends an editable row to the end of the subject information table.
     * 
     * @param name DOCUMENT ME!
     * @param value DOCUMENT ME!
     * @param editor - list of editor types for editing this row
     */
    public void appendSubjectData(final String name, final String value, final int[] editor) {
        final String[] rose = {name, value};

        subjectModel.addRow(rose);

        final Vector<Integer> editorInts = new Vector<Integer>();

        for (final int element : editor) { // set the list of editors to use
            editorInts.addElement(new Integer(element));
        }

        subjectTypeHolder.put(new Integer(subjectModel.getRowCount() - 1), editorInts);
    }

    /**
     * makes the display frame. builds the layout.
     * 
     * @param fileInfo DOCUMENT ME!
     */
    @SuppressWarnings("unchecked")
    public void displayAboutInfo(final FileInfoImageXML fileInfo) {
        final String[] columnNames = {"Name", "Value"};

        try {
            this.fileinfo = fileInfo;

            subjectTypeHolder = new Hashtable<Integer,Vector<Integer>>();
            subjectEditorHolder = new Hashtable<Integer,JDialogEditor>();
            subjectModel = new ViewTableModel();
            subjectTable = new JTable(subjectModel);
            subjectTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
            subjectTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);

            scanTypeHolder = new Hashtable<Integer,Vector<Integer>>();
            scanEditorHolder = new Hashtable<Integer,JDialogEditor>();
            scanModel = new ViewTableModel();
            scanTable = new JTable(scanModel);
            scanTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
            scanTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);

            investigatorTypeHolder = new Hashtable<Integer,Vector<Integer>>();
            investigatorEditorHolder = new Hashtable<Integer,JDialogEditor>();
            investigatorModel = new ViewTableModel();
            investigatorTable = new JTable(investigatorModel);
            investigatorTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
            investigatorTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);

            tagTypeHolder = new Hashtable<Integer,Vector<Integer>>();
            tagModel = new ViewTableModel();
            sorter = new TableSorter(tagModel);
            tagTable = new JTable(sorter);
            sorter.setTableHeader(tagTable.getTableHeader());

            tagTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
            tagTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);

            // for the diff tables inside the scrollpanel:
            scrollingBox = new Box(BoxLayout.Y_AXIS);

            primaryModel = new ViewTableModel();
            primaryTable = new JTable(primaryModel);
            editorDialogTable = new Hashtable(); // hastable to hold editing dialogs
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("ViewFileInfo reports: Out of memory!");

            return;
        } catch (final IllegalArgumentException ex) {
            MipavUtil.displayError("ViewFileInfo reports: Editing table too small!" + ex);

            return;
        }

        int i;

        for (i = 0; i < 2; i++) {
            primaryModel.addColumn(columnNames[i]);

            subjectModel.addColumn(columnNames[i]);
            scanModel.addColumn(columnNames[i]);
            investigatorModel.addColumn(columnNames[i]);
        }

        primaryTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
        primaryTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        primaryTable.getColumn("Name").setMinWidth(160);
        primaryTable.getColumn("Name").setMaxWidth(500);
        primaryTable.getColumn("Value").setMinWidth(50);
        primaryTable.getColumn("Value").setMaxWidth(1000);
        primaryTable.getTableHeader().setReorderingAllowed(false);

        final JLabel priLabel = new JLabel("Essential Image Information");

        priLabel.setForeground(Color.black);
        scrollingBox.add(priLabel);
        scrollingBox.add(primaryTable.getTableHeader());
        scrollingBox.add(primaryTable);

        subjectTable.getColumn("Name").setMinWidth(160);
        subjectTable.getColumn("Name").setMaxWidth(500);
        subjectTable.getColumn("Value").setMinWidth(50);
        subjectTable.getColumn("Value").setMaxWidth(1000);
        subjectTable.getTableHeader().setReorderingAllowed(false);

        final JLabel subjectLabel = new JLabel("Subject Information");

        subjectLabel.setForeground(Color.black);

        scrollingBox.add(subjectLabel);
        scrollingBox.add(subjectTable.getTableHeader());
        scrollingBox.add(subjectTable);

        scanTable.getColumn("Name").setMinWidth(160);
        scanTable.getColumn("Name").setMaxWidth(500);
        scanTable.getColumn("Value").setMinWidth(50);
        scanTable.getColumn("Value").setMaxWidth(1000);
        scanTable.getTableHeader().setReorderingAllowed(false);

        final JLabel scanLabel = new JLabel("Scan Attributes");

        scanLabel.setForeground(Color.black);

        scrollingBox.add(scanLabel);
        scrollingBox.add(scanTable.getTableHeader());
        scrollingBox.add(scanTable);

        investigatorTable.getColumn("Name").setMinWidth(160);
        investigatorTable.getColumn("Name").setMaxWidth(500);
        investigatorTable.getColumn("Value").setMinWidth(50);
        investigatorTable.getColumn("Value").setMaxWidth(1000);
        investigatorTable.getTableHeader().setReorderingAllowed(false);

        final JLabel investigatorLabel = new JLabel("Investigators");

        investigatorLabel.setForeground(Color.black);

        scrollingBox.add(investigatorLabel);
        scrollingBox.add(investigatorTable.getTableHeader());
        scrollingBox.add(investigatorTable);

        final Enumeration<String> e = fileInfo.getSurfaceKeys();

        while (e.hasMoreElements()) {
            final String path = e.nextElement();
            surfaces.addSurface(fileInfo.getSurface(path).getPath(), fileInfo.getSurface(path).getOpacity(), fileInfo
                    .getSurface(path).getDisplay());
        }

        scrollingBox.add(surfaces.getLabel());
        scrollingBox.add(surfaces.getTable().getTableHeader());
        scrollingBox.add(surfaces.getTable());

        tagModel.addColumn("Tag");
        tagModel.addColumn("Name");
        tagModel.addColumn("Value");
        tagTable.getColumn("Tag").setMinWidth(30);
        tagTable.getColumn("Tag").setMaxWidth(100);
        tagTable.getColumn("Name").setMinWidth(120);
        tagTable.getColumn("Name").setMaxWidth(450);
        tagTable.getColumn("Value").setMinWidth(50);
        tagTable.getColumn("Value").setMaxWidth(1000);
        tagTable.getTableHeader().setReorderingAllowed(false);

        tagLabel = new JLabel("DICOM Tags");

        tagLabel.setForeground(Color.black);

        scrollingBox.add(tagLabel);
        scrollingBox.add(tagTable.getTableHeader());
        scrollingBox.add(tagTable);

        masterScrollPane = new JScrollPane(scrollingBox, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        masterScrollPane.getVerticalScrollBar().setUnitIncrement(14);
        getContentPane().add(masterScrollPane);

        final JButton close = new JButton("Close");

        close.setActionCommand("Close");
        close.addActionListener(this);

        // close.setPreferredSize( MipavUtil.defaultButtonSize );
        close.setFont(serif12B);

        edit = new JButton("Edit tag");

        // edit.setEnabled(false);
        edit.setActionCommand("EditTag");
        edit.addActionListener(this);

        // edit.setPreferredSize( MipavUtil.defaultButtonSize );
        edit.setFont(serif12B);

        addSet = new JButton("Add Set");
        addSet.setActionCommand("AddSet");
        addSet.addActionListener(this);

        // addSet.setPreferredSize( MipavUtil.defaultButtonSize );
        addSet.setFont(serif12B);

        removeParam = new JButton("Remove Parameter");
        removeParam.setActionCommand("RemoveParameter");
        removeParam.addActionListener(this);

        // removeParam.setPreferredSize( MipavUtil.defaultButtonSize );
        removeParam.setFont(serif12B);

        addSurface = new JButton("Add Surface");
        addSurface.setActionCommand("AddSurface");
        addSurface.addActionListener(this);

        // removeParam.setPreferredSize( MipavUtil.defaultButtonSize );
        addSurface.setFont(serif12B);

        expandTags = new JButton("Expand Tags");
        expandTags.setActionCommand("ExpandTags");
        expandTags.addActionListener(this);

        // addSurface.setPreferredSize( MipavUtil.defaultButtonSize );
        expandTags.setFont(serif12B);

        removeSurface = new JButton("Remove Surface");
        removeSurface.setActionCommand("RemoveSurface");
        removeSurface.addActionListener(this);

        // removeSurface.setPreferredSize( MipavUtil.defaultButtonSize );
        removeSurface.setFont(serif12B);

        final JPanel buttonPanel = new JPanel();

        buttonPanel.add(addSet);
        buttonPanel.add(edit);
        buttonPanel.add(expandTags);
        buttonPanel.add(removeParam);
        buttonPanel.add(addSurface);
        buttonPanel.add(removeSurface);
        buttonPanel.add(close);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        this.setSize(new Dimension(800, 800));

    }

    /**
     * DOCUMENT ME!
     * 
     * @param ke DOCUMENT ME!
     */
    public void keyTyped(final KeyEvent ke) {
        edit.doClick();
    }

    /**
     * Divides space separate strings into an array of strings.
     * 
     * @param incoming string
     * 
     * @return array of separated strings
     */
    protected String[] separateValues(final String incoming) {
        final StringTokenizer stok = new StringTokenizer(incoming, " ");
        final String[] outgoing = new String[stok.countTokens()];
        int i = 0;

        while (stok.hasMoreTokens()) {
            outgoing[i++] = stok.nextToken();
        }

        return outgoing;
    }

    /**
     * Sort the tag column or name column of the table model. If reverse is true, sorts in reverse order.
     * 
     * @param model the table model to sort on
     * @param col column to sort on
     * @param reverse whether or not to sort in reverse order.
     */
    private static void sort(final ViewTableModel model, final int col, final boolean reverse) {
        final int begin = 1;

        for (int p = begin; p < model.getRowCount(); p++) {

            for (int j = begin - 1; j < p; j++) {

                if (model.getValueAt(p, col) != null) {

                    if (reverse) {

                        if ( ((String) model.getValueAt(p, col)).compareTo((String) model.getValueAt(j, col)) > 0) {
                            model.moveRow(p, p, j);

                            break;
                        }
                    } else {

                        if ( ((String) model.getValueAt(p, col)).compareTo((String) model.getValueAt(j, col)) < 0) {
                            model.moveRow(p, p, j);

                            break;
                        }
                    }
                }
            }
        }
    }

    /**
     * checks whether or not the dialog exists; if it does, it brings the dialog to front.
     * 
     * @param tagKey the tag's Key. Used to dtermine if this tag already has an editor associated with it.
     * @param model DOCUMENT ME!
     * 
     * @return true if both a tag with the tagkey existed in the list and the associated dialog was brought to front.
     */
    @SuppressWarnings("unused")
    private boolean bringToFront(final String tagKey, final Hashtable<String,JDialogEditor> model) {
        JDialogEditor editor; // temporary tag editor dialog

        // list is empty

        if (model.isEmpty()) {
            return false;
        } // check all tag editors in the list to see if the given tag key is has a tag in the list.

        // drop out once one has been found and brought to front of list and of screen.
        else if (model.containsKey(tagKey)) {

            try {
                editor = model.get(tagKey);
                editor.toFront();

                return true;
            } catch (final ClassCastException cce) {
                return false;
            }
        } else {
            return false;
        }
    }

    // ~ Inner Classes
    // --------------------------------------------------------------------------------------------------

    /**
     * Class to hold one table, model, & label per parameter set There can be infinite parameter sets, and the sets are
     * deletable, so each set display must be stored in a hashtable for easy access/deletion.
     */
    public class PSetDisplay {

        /** DOCUMENT ME! */
        private final String description;

        /** DOCUMENT ME! */
        private final JLabel setLabel;

        /** DOCUMENT ME! */
        private final ViewTableModel setModel;

        /** DOCUMENT ME! */
        private final JTable setTable;

        /**
         * Creates a parameter set display with the given description that will be used as the key.
         * 
         * @param description DOCUMENT ME!
         */
        public PSetDisplay(final String description) {
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
         * @param paramName String
         * @param paramDesc String
         * @param valueType String
         * @param value String
         * @param date String
         * @param time String
         */
        public void addParameterData(final String paramName, final String paramDesc, final String valueType,
                final String value, final String date, final String time) {
            final String[] rose = {paramName, paramDesc, valueType, value, date, time};

            setModel.addRow(rose);
        }

        /**
         * Gets the set display's description.
         * 
         * @return description
         */
        public String getDescription() {
            return this.description;
        }

        /**
         * Gets the label.
         * 
         * @return label
         */
        public JLabel getLabel() {
            return this.setLabel;
        }

        /**
         * Gets the table model.
         * 
         * @return table model
         */
        public ViewTableModel getModel() {
            return this.setModel;
        }

        /**
         * Gets the table of parameters.
         * 
         * @return table
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
             * @param e event that triggered this method
             */

            public void mouseClicked(final MouseEvent e) {
                final Object source = e.getSource();
                final Point p = e.getPoint();
                int col;

                if (source.equals(setTable.getTableHeader()) && (e.getButton() == MouseEvent.BUTTON1)) {
                    col = setTable.columnAtPoint(p);

                    if (e.isShiftDown()) {
                        JDialogFileInfoXML.sort(setModel, col, true);
                    } else {
                        JDialogFileInfoXML.sort(setModel, col, false);
                    }
                } else if (source.equals(setTable.getTableHeader()) && (e.getButton() == MouseEvent.BUTTON3)) {
                    setDescforAddParam = new String(getDescription());

                    final JPopupMenu popUp = new JPopupMenu();

                    popUp.add(addParam);
                    popUp.show(setTable.getTableHeader(), p.x, p.y);
                }
            }

            /**
             * Unchanged.
             * 
             * @param e DOCUMENT ME!
             */
            @SuppressWarnings("unused")
            public void mouseDragged(final MouseEvent e) {}

            /**
             * Unchanged.
             * 
             * @param e DOCUMENT ME!
             */
            public void mouseEntered(final MouseEvent e) {}

            /**
             * Unchanged.
             * 
             * @param e DOCUMENT ME!
             */
            public void mouseExited(final MouseEvent e) {}

            /**
             * Unchanged.
             * 
             * @param e DOCUMENT ME!
             */
            public void mousePressed(final MouseEvent e) {}

            /**
             * Unchanged.
             * 
             * @param e DOCUMENT ME!
             */
            public void mouseReleased(final MouseEvent e) {}
        }
    }

    /**
     * Class to hold one table, model, & label per parameter set There can be infinite parameter sets, and the sets are
     * deletable, so each set display must be stored in a hashtable for easy access/deletion.
     */
    public class SurfaceDisplay {

        /** DOCUMENT ME! */
        private final JLabel surLabel;

        /** DOCUMENT ME! */
        private final ViewTableModel surModel;

        /** DOCUMENT ME! */
        private final JTable surTable;

        /**
         * Creates surface display table.
         */
        public SurfaceDisplay() {
            surLabel = new JLabel("Surfaces");
            surLabel.setForeground(Color.black);
            surModel = new ViewTableModel();
            surTable = new JTable(surModel);

            for (final String element : surfaceColumnNames) {
                surModel.addColumn(element);
            }

            surTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
            surTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);

            surTable.getColumn("Path").setMinWidth(160);
            surTable.getColumn("Path").setMaxWidth(700);
            surTable.getColumn("Opacity").setMinWidth(50);
            surTable.getColumn("Opacity").setMaxWidth(150);
            surTable.getColumn("Load").setMinWidth(20);
            surTable.getColumn("Load").setMaxWidth(100);

            surTable.getTableHeader().setReorderingAllowed(false);
            surTable.getTableHeader().addMouseListener(new HeaderListener());
        }

        /**
         * Add a surface to the table.
         * 
         * @param path the surface file path
         * @param opacity the surface opacity
         * @param load whether to load the surface when opening the volume renderer
         */
        public void addSurface(final String path, final float opacity, final boolean load) {
            final Object[] rose = {path, new String("" + opacity), new String("" + load)};
            surModel.addRow(rose);
        }

        /**
         * Gets the label.
         * 
         * @return label
         */
        public JLabel getLabel() {
            return this.surLabel;
        }

        /**
         * Gets the table model.
         * 
         * @return table model
         */
        public ViewTableModel getModel() {
            return this.surModel;
        }

        /**
         * Gets the table of surfaces.
         * 
         * @return table
         */
        public JTable getTable() {
            return this.surTable;
        }

        /**
         * Simple listener for the table header.
         */
        private class HeaderListener implements MouseListener {

            /**
             * When the user right clicks on a header, sorts the column. when the user left clicks, a popup menu gives
             * the option to add a parameter to the table, triggering a JDialog
             * 
             * @param e event that triggered this method
             */
            public void mouseClicked(final MouseEvent e) {
                final Object source = e.getSource();
                final Point p = e.getPoint();
                int col;

                if (source.equals(surTable.getTableHeader()) && (e.getButton() == MouseEvent.BUTTON1)) {
                    col = surTable.columnAtPoint(p);

                    if (e.isShiftDown()) {
                        JDialogFileInfoXML.sort(surModel, col, true);
                    } else {
                        JDialogFileInfoXML.sort(surModel, col, false);
                    }
                }
            }

            /**
             * Unchanged.
             * 
             * @param e DOCUMENT ME!
             */
            @SuppressWarnings("unused")
            public void mouseDragged(final MouseEvent e) {}

            /**
             * Unchanged.
             * 
             * @param e DOCUMENT ME!
             */
            public void mouseEntered(final MouseEvent e) {}

            /**
             * Unchanged.
             * 
             * @param e DOCUMENT ME!
             */
            public void mouseExited(final MouseEvent e) {}

            /**
             * Unchanged.
             * 
             * @param e DOCUMENT ME!
             */
            public void mousePressed(final MouseEvent e) {}

            /**
             * Unchanged.
             * 
             * @param e DOCUMENT ME!
             */
            public void mouseReleased(final MouseEvent e) {}
        }
    }

}
