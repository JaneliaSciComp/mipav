package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.ModelImage;

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

public class JDialogFileInfo extends JDialogBase implements ActionListener {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 1629395651992086059L;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JButton edit;

    /** DOCUMENT ME! */
    private Hashtable editorDialogTable;

    /** DOCUMENT ME! */
    private FileInfoBase fileinfo;

    /** DOCUMENT ME! */
    private final ModelImage image;

    /** DOCUMENT ME! */
    private ViewTableModel primaryModel;

    /** DOCUMENT ME! */
    private JTable primaryTable;

    /** tpe holds the type of editor to be used; editor holds the editor dialog. */
    private final Hashtable<Integer,Vector<Integer>> primaryTypeHolder;
    private final Hashtable<Integer,JDialogEditor> primaryEditorHolder;

    /** DOCUMENT ME! */
    private JScrollPane scrollPane;

    /** DOCUMENT ME! */
    private ViewTableModel secondaryModel;

    /** DOCUMENT ME! */
    private JTable secondaryTable;

    /** DOCUMENT ME! */
    private final Hashtable<Integer,Vector<Integer>> secondaryTypeHolder;
    private final Hashtable<Integer,JDialogEditor> secondaryEditorHolder;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * This method displays all the valid variables, that is, the ones that are no longer equal to their default values.
     * It calls parseDate and parseTime when needed and translates other strings. However, this method does not yet
     * translate every single DICOM tag, only those most used. The others it outputs as strings.
     * 
     * @param parent the number of columns an editable table should have (either 2 or 3 is allowed).
     * @param title DOCUMENT ME!
     * @param img DOCUMENT ME!
     */
    public JDialogFileInfo(final Frame parent, final String title, final ModelImage img) {
        super(parent, false);
        setTitle(title);
        try {
            setIconImage(MipavUtil.getIconImage("header.gif"));
        } catch (final Exception e) {
            // setIconImage() is not part of the Java 1.5 API - catch any runtime error on those systems
        }

        image = img;
        primaryTypeHolder = new Hashtable<Integer,Vector<Integer>>(); // all editable lines in primary, keyed by location in Jtable
        secondaryTypeHolder = new Hashtable<Integer,Vector<Integer>>(); // all editable lines in secondary, keyed by location in Jtable
        primaryEditorHolder = new Hashtable<Integer,JDialogEditor>(); // all editable lines in primary, keyed by location in Jtable
        secondaryEditorHolder = new Hashtable<Integer,JDialogEditor>(); // all editable lines in secondary, keyed by location in Jtable
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
        } else if (e.getActionCommand().equals("EditTag")) { // edit the high-lighted tag

            Integer named;
            int[] rows;
            int i = 0;
            Object obj;
            Vector<Integer> objs;

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
                            // modify all slices if user so desired. In addition, when the change was applied
                            // to all slices, notify the image frames in the image frame vector to reset the
                            // title (title changes only for name, @see ModelImage#setTitle(String)
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

                                if (ed.wasDialogOkay()) {

                                    // to make this more FileInfoBase friendly, add a
                                    // public static void stateChanged(Vector)
                                    // to FileInfoBase. Then remove the references to
                                    // the cast. Otherwise, using the editors with other
                                    // varieties of FileInfo will throw ClassCastExceptions.
                                    // Also suggest that a distinct datatype (other than Vector)
                                    // be created to handle the special needs.
                                    ((FileInfoAnalyze) fileinfo).stateChanged(changed); // what to do to update
                                    // fileinfo!?!?!
                                    primaryModel.setValueAt(ed.getDisplayValue(), edID.intValue(), 1);
                                    primaryEditorHolder.remove(edID);

                                    // a really nice idea: add an option (default ON, of course)
                                    // for fileInfo propogation, that this would do in a local
                                    // fileInfo friendly way--ie., the fileInfo knows which
                                    // values should be propogated.
                                    // Loop through other fileinfos from image.
                                    for (int q = 0; q < image.getFileInfo().length; q++) {
                                        ((FileInfoAnalyze) fileinfo).updateFileInfos((FileInfoAnalyze) image
                                                .getFileInfo(q));
                                    }
                                } else {
                                    primaryEditorHolder.remove(edID);
                                    changed = null; // forget it
                                }

                            }
                        });
                        primaryEditorHolder.put(named, editor);
                    }
                } else {
                    super.actionPerformed(e);
                }
            }

            // go through secondary table, editing highlighted (selected) rows
            rows = secondaryTable.getSelectedRows();

            for (i = 0; i < rows.length; i++) {

                // determine if this row can be edited
                named = new Integer(rows[i]);

                if (secondaryTypeHolder.containsKey(named)) { // if named
                    secondaryTable.removeRowSelectionInterval(rows[i], rows[i]);

                    // if previously named, bring the editors to the front
                    obj = secondaryEditorHolder.get(named);

                    if (obj != null) {
                        ((JDialogEditor) (obj)).toFront();
                    } else {
                        objs = secondaryTypeHolder.get(named);

                        String[] values = new String[1]; // this is unclean design now to cover something i didn't
                        // foresee

                        if (objs.size() > 1) { // assume seperate words mean something special

                            // when there is more than one editor specified.
                            values = separateValues((String) secondaryTable.getValueAt(rows[i], 1));
                        } else {
                            values[0] = (String) secondaryTable.getValueAt(rows[i], 1);
                        }

                        final int[] editors = new int[objs.size()]; // because we tell an editor to do something based
                                                                    // on an
                        // int code

                        for (int j = 0; j < objs.size(); j++) {
                            editors[j] = ((objs.elementAt(j))).intValue(); // what the user asked for
                        }

                        editor = new JDialogEditor(this, named, values, editors);
                        editor.setTitle((String) secondaryTable.getValueAt(rows[i], 0));
                        editor.setVisible(true);
                        editor.addWindowListener(new WindowAdapter() {

                            // when windowClosed: check to see if dialog closed by OKbutton (if it did then there
                            // were no problems with user inputted values); if it closed by okbutton, then
                            // modify all slices if user so desired. In addition, when the change was applied
                            // to all slices, notify the image frames in the image frame vector to reset the
                            // title (title changes only for name, @see ModelImage#setTitle(String)
                            public void windowClosed(final WindowEvent e) {

                                JDialogEditor ed;
                                ed = (JDialogEditor) e.getSource();

                                final Integer edID = (Integer) ed.getKey();
                                Vector<Object> changed = new Vector<Object>(4);
                                changed.add(0, new Integer(1)); // table
                                changed.add(1, edID); // line
                                changed.add(2, secondaryModel.getValueAt(edID.intValue(), 0));
                                changed.add(3, ed.getValue());
                                changed.add(4, ed.getDisplayValue());

                                if (ed.wasDialogOkay()) {

                                    // to make this more FileInfoBase friendly, add a
                                    // public static void stateChanged(Vector)
                                    // to FileInfoBase. Then remove the references to
                                    // the cast. Otherwise, using the editors with other
                                    // varieties of FileInfo will throw ClassCastExceptions.
                                    // Also suggest that a distinct datatype (other than Vector)
                                    // be created to handle the special needs.
                                    ((FileInfoAnalyze) fileinfo).stateChanged(changed); // what to do to update
                                    // fileinfo!?!?!
                                    secondaryModel.setValueAt(ed.getDisplayValue(), edID.intValue(), 1);
                                    secondaryEditorHolder.remove(edID);

                                    // a really nice idea: add an option (default ON, of course)
                                    // for fileInfo propogation, that this would do in a local
                                    // fileInfo friendly way--ie., the fileInfo knows which
                                    // values should be propogated.
                                    // Loop through other fileinfos from image.
                                    if ( (image != null) && (image.getFileInfo() != null)) {

                                        for (int q = 0; q < image.getFileInfo().length; q++) {
                                            ((FileInfoAnalyze) fileinfo).updateFileInfos((FileInfoAnalyze) image
                                                    .getFileInfo(q));
                                        }
                                    }
                                } else {
                                    secondaryEditorHolder.remove(edID);
                                    changed = null; // forget it
                                }
                            }

                        });
                        secondaryEditorHolder.put(named, editor);
                    }
                } else {}
            }
        }

    }

    /**
     * appends a row to the end of the editable table.
     * 
     * @param name name file info parameter (ie., dimensions, extents, &c).
     * @param value value value assigned to a fileinfo parameter
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
     * appends a row to the end of the Secondary Info table. For non-editable data.
     * 
     * @param name name file info parameter (ie., dimensions, extents, &c).
     * @param value ring value value assigned to a fileinfo parameter
     */
    public void appendSecondaryData(final String name, final String value) {

        if (value.indexOf('\n') == -1) { // \n doesn't occur in the value

            final String[] rose = {name, value};
            secondaryModel.addRow(rose);
        } else {
            final StringTokenizer stok = new StringTokenizer(value, "\n");
            final String[] values = new String[stok.countTokens()];
            int i = 0;

            while (stok.hasMoreTokens()) {
                values[i++] = stok.nextToken();
            }

            final String[] rose = {name, values[0]};
            secondaryModel.addRow(rose);
            i = 1;
            rose[0] = "";

            while (i < values.length) {
                rose[1] = values[i++];
                secondaryModel.addRow(rose);
            }
        }
    }

    /**
     * appends a row to the end of the editable table.
     * 
     * @param String name file info parameter (ie., dimensions, extents, &c).
     * @param String value value assigned to a fileinfo parameter
     * @param editor The value of editor is the editor interface to be used. Eg., a JPanelEditDefault. For example,
     *            specified by
     * 
     * <ul>
     * <li>JDialogEditor#STRING</li>
     * <li>JDialogEditor#INT_STRING</li>
     * <li>JDialogEditor#FLOAT_STRING</li>
     * <li>JDialogEditor#ANALYZE_DATATYPE</li>
     * <li>JDialogEditor#ANALYZE_DESCRIPTION</li>
     * <li>JDialogEditor#ANALYZE_ORIENTATION</li>
     * <li>JDialogEditor#ANALYZE_AXIS_ORIENTATION</li>
     * </ul>
     * as a partial list.
     * 
     * @see JPanelEdit
     */
    public void appendSecondaryData(final String name, final String value, final int[] editor) {
        final String[] rose = {name, value};
        secondaryModel.addRow(rose);

        final Vector<Integer> editorInts = new Vector<Integer>();

        for (final int element : editor) { // set the list of editors to use
            editorInts.addElement(new Integer(element));
        }

        secondaryTypeHolder.put(new Integer(secondaryModel.getRowCount() - 1), editorInts);
    }

    /**
     * makes the display frame. builds the layout.
     * 
     * @param fileInfo DOCUMENT ME!
     */
    public void displayAboutInfo(final FileInfoBase fileInfo) {
        Box scrollingBox;
        final String[] columnNames = {"Name", "Value"};

        try {
            this.fileinfo = fileInfo;

            // for the diff tables inside the scrollpanel:
            scrollingBox = new Box(BoxLayout.Y_AXIS);

            primaryModel = new ViewTableModel();
            primaryTable = new JTable(primaryModel);
            secondaryModel = new ViewTableModel();
            secondaryTable = new JTable(secondaryModel);

            editorDialogTable = new Hashtable(); // hastable to hold editing dialogs
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("ViewFileInfo reports: Out of memory!");

            return;
        } catch (final IllegalArgumentException ex) {
            MipavUtil.displayError("ViewFileInfo reports: Editing table too small!");

            return;
        }

        int i;

        for (i = 0; i < 2; i++) {
            primaryModel.addColumn(columnNames[i]);
            secondaryModel.addColumn(columnNames[i]);
        }

        primaryTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
        primaryTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        primaryTable.getColumn("Name").setMinWidth(160);
        primaryTable.getColumn("Name").setMaxWidth(500);
        primaryTable.getColumn("Value").setMinWidth(50);
        primaryTable.getColumn("Value").setMaxWidth(1000);

        secondaryTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
        secondaryTable.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
        secondaryTable.getColumn("Name").setMinWidth(160);
        secondaryTable.getColumn("Name").setMaxWidth(500);
        secondaryTable.getColumn("Value").setMinWidth(50);
        secondaryTable.getColumn("Value").setMaxWidth(1000);

        final JLabel priLabel = new JLabel("Essential Image Information");
        priLabel.setForeground(Color.black);
        scrollingBox.add(priLabel);
        scrollingBox.add(primaryTable);

        final JLabel secLabel = new JLabel("Other Image Information");
        secLabel.setForeground(Color.black);
        scrollingBox.add(secLabel);
        scrollingBox.add(secondaryTable);

        try {
            scrollPane = new JScrollPane(scrollingBox, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                    ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            scrollPane.setPreferredSize(new Dimension(200, 200));
            scrollPane.setMinimumSize(new Dimension(150, 100));
            scrollPane.getVerticalScrollBar().setUnitIncrement(14);
            this.setSize(new Dimension(600, 400));
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("ViewFileInfo reports: Out of memory!");

            return;
        }

        scrollPane.setBackground(Color.black);

        getContentPane().add(scrollPane);

        final JButton close = new JButton("Close");
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

        final JPanel buttonPanel = new JPanel();
        buttonPanel.add(edit);
        buttonPanel.add(close);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);

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
     * DOCUMENT ME!
     * 
     * @param incoming DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
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
        }
        // check all tag editors in the list to see if the given tag key is has a tag in the list.
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

}
