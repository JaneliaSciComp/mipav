package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.WindowEvent;
import java.io.*;
import java.util.*;

import javax.swing.*;


/**
 * Dialog to select DICOM files to convert to MIPAV XML or Minc 2.0 HDF format.
 * 
 * @author parsonsd
 */
public class JDialogDicom2XMLSelection extends JDialogListSaveSelection {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -1864581773877529737L;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /**
     * Boolean that determines if the window was closed (hitting the X) wasOkay = true means the [x] was not clicked.
     */
    private boolean wasOkay = true;

    private String destFormatStr = "XML";

    /**
     * FileInfoDicom for source image
     */
    private FileInfoDicom sourceInfo;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new instance of JDialogDicom2XMLSelection.
     */
    public JDialogDicom2XMLSelection(FileInfoDicom sourceInfo, boolean isXML) {
        super();
        this.sourceInfo = sourceInfo;

        if ( !isXML) {
            destFormatStr = "Minc 2.0 HDF";
        }

        setTitle("Select DICOM tags to convert to " + destFormatStr);
        populateLists();
        addSortButtons();
        setPreferredSize(new Dimension(1200, 800));
        pack();
        addWindowListener(this);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Handles the IGNORE button possibility of the ActionEvent, otherwise passes control to the super event handler.
     * 
     * @param ae DOCUMENT ME!
     */
    public void actionPerformed(java.awt.event.ActionEvent ae) {

        if (ae.getActionCommand().equalsIgnoreCase("ignore")) {
            ignoreAction();
        } else if (ae.getActionCommand().equalsIgnoreCase("sortKey")) {} else if (ae.getActionCommand()
                .equalsIgnoreCase("sortKeyRev")) {} else if (ae.getActionCommand().equalsIgnoreCase("sortName")) {} else if (ae
                .getActionCommand().equalsIgnoreCase("sortNameRev")) {} else if (ae.getActionCommand()
                .equalsIgnoreCase("help")) {
            MipavUtil.showHelp("U4018");
        } else {
            super.actionPerformed(ae);
        }
    }

    /**
     * Returns a hashtable of the keys and associated DICOM tags that are in the right-list.
     * 
     * @return a Hashtable of the DICOM tags listed in the save-list.
     */
    public Hashtable<FileDicomKey, FileDicomTagInfo> getSaveTable() {
        Hashtable<FileDicomKey, FileDicomTagInfo> saveTable = new Hashtable<FileDicomKey, FileDicomTagInfo>();
        ListModel saveList = getSelectedList().getModel();

        for (int i = 0; i < saveList.getSize(); i++) {
            DicomTagIdentifier tag = (JDialogDicom2XMLSelection.DicomTagIdentifier) saveList.getElementAt(i);
            saveTable.put(tag.getKey(), tag.getTag());
        }

        return saveTable;
    }

    /**
     * Returns true if the window was not closed using the 'X' and was instead closed after the user clicked the 'Save'
     * button.
     * 
     * @return whether the window was closed after the user clicked the 'Save' button
     */
    public boolean wasOkay() {
        return wasOkay;
    }

    /**
     * Closes window and disposes of frame and component.
     * 
     * @param event Event that triggered function
     */
    public void windowClosing(WindowEvent event) {
        this.wasOkay = false;
    }

    /**
     * Creates an OK Button, which is relabeled &quot;Save&quot;, an &quot;Ignore&quot; button, a &quot;Cancel&quot; and
     * a &quot;Help&quot; button.
     * 
     * @return JPanel The button panel containing a collection of buttons to close the dialog and perform some action.
     */
    protected JPanel buildButtons() {
        JPanel buttonPanel = new JPanel();
        buttonPanel.add(buildOKButton());
        buttonPanel.add(buildIgnoreButton());
        // buttonPanel.add(buildCancelButton());
        buttonPanel.add(buildHelpButton());
        OKButton.setToolTipText("Save list to file and convert tags in image");

        return buttonPanel;
    }

    /**
     * Creates a button labelled &quot;Ignore&quot; and connects it to the action listener event list.
     * 
     * @return DOCUMENT ME!
     */
    protected JButton buildIgnoreButton() {
        JButton ignoreButton = new JButton("Ignore");
        ignoreButton.addActionListener(this);
        ignoreButton.setToolTipText("Ignore the list and convert no tags in image");
        ignoreButton.setMinimumSize(MipavUtil.defaultButtonSize);
        ignoreButton.setPreferredSize(MipavUtil.defaultButtonSize);
        ignoreButton.setFont(serif12B);

        return ignoreButton;
    }

    /**
     * Uses the super method to create a standard OK Button, but then resets the button text to read &quot;Save&quot;
     * leaving the action command string as normal: &quot;OK&quot;.
     * 
     * @return DOCUMENT ME!
     */
    protected JButton buildOKButton() {
        super.buildOKButton();
        OKButton.setText("Save");
        OKButton.setActionCommand("OK");

        return OKButton;
    }

    /**
     * Calls the super.cancelAction.
     */
    protected void cancelAction() {
        super.cancelAction();
        // throw new Exception("Cancel");
    }

    /**
     * Creates a single JPanelFileSelector which refers to the file &quot;dicomsave.dictionary&quot; in the user.home
     * (specified by java's properties) directory by default.
     * 
     * @return this JPanelFileSelector
     */
    protected JComponent createFileSelectorComponent() {

        // build List to save to:
        selectFileB = new JPanelFileSelection(new File( /* start directory */Preferences.getDICOMSaveDictionary()),
        /* title */"DICOM Tags Dictionary File for saving to " + destFormatStr);

        return selectFileB;
    }

    /**
     * Generates and returns the list data for the Source list from the dicom dictionary specified by the save panel as
     * a Vector of <code>DicomTagIdentifier</code>. The Vector returned is empty if the File listed by the save panel
     * contains no tags, is non-existant, or is unspecified.
     * 
     * @return An ordered list of <code>DicomTagIdentifier</code> of tags from the DICOM dictionary listed in the save
     *         tag file panel. The Vector returned is empty if the File listed by the save panel contains no tags, is
     *         non-existant, or is unspecified.
     * 
     * @see DicomDictionary
     * @see #getSaveTagFilePanel()
     */
    protected Vector<DicomTagIdentifier> getSaveListData() {

        // load the selected list from the pointed-to file, if possible:
        Vector<DicomTagIdentifier> newTagList = new Vector<DicomTagIdentifier>();

        try {

            if (getSaveTagFilePanel().getSelectedFile().exists()) {
                Hashtable<FileDicomKey, FileDicomTagInfo> hashtable = DicomDictionary.getSubsetDicomTagTable();
                FileDicomKey[] saveKeys = DicomDictionary.sortTagKeys(hashtable);

                for (int i = 0; i < saveKeys.length; i++) {
                    FileDicomKey key = (FileDicomKey) saveKeys[i];
                    newTagList.add(new DicomTagIdentifier(key, (FileDicomTagInfo) hashtable.get(key)));
                }

                Preferences.setProperty(Preferences.PREF_DICOM_SAVE_DICTIONARY, getSaveTagFilePanel().getSelectedFile()
                        .getPath());
            }
        } catch (NullPointerException noFile) {

            // we'll do nothing here, and leave the list empty.
            System.err.println("save Tag file was not a DICOM dictionary file");
        }

        return newTagList;
    }

    /**
     * Returns the SelectFileB panel.
     * 
     * @return DOCUMENT ME!
     */
    protected JPanelFileSelection getSaveTagFilePanel() {
        return selectFileB;
    }

    /**
     * Generates and returns the list data for the Source list from the dicom dictionary as a Vector of <code>
     * DicomTagIdentifier</code>.
     * 
     * @return Vector An ordered list of <code>DicomTagIdentifier</code> of tags from the DICOM dictionary
     * 
     * @see DicomDictionary
     */
    protected Vector<DicomTagIdentifier> getSourceListData() {
        Vector<DicomTagIdentifier> dicomList = new Vector<DicomTagIdentifier>();
        Hashtable<FileDicomKey, FileDicomTagInfo> tagsTable = DicomDictionary.getDicomTagTable();
        FileDicomKey[] dictionaryKeyList = DicomDictionary.sortTagKeys(tagsTable);

        for (int i = 0; i < dictionaryKeyList.length; i++) {
            FileDicomKey key = (FileDicomKey) dictionaryKeyList[i];
            dicomList.add(new DicomTagIdentifier(key, (FileDicomTagInfo) tagsTable.get(key)));
        }

        return (dicomList);
    }

    /**
     * Empties the right-list, then closes the dialog.
     */
    protected void ignoreAction() {
        getSelectedList().setListData(new Vector<DicomTagIdentifier>());
        setVisible(false);
    }

    /**
     * Saves the list on the left side of the dialog, then closes the dialog.
     */
    protected void okAction() {
        saveListB();
        setVisible(false);
    }

    /**
     * Fills the source and selected item lists with the appropriate data.
     */
    protected void populateLists() {
        getSourceList().setListData(getSourceListData());
        getSourceList().setCellRenderer(new MyCellRenderer());
        getSelectedList().setListData(getSaveListData());
    }

    /**
     * Here to allow subclasses to select the action to occur when a property change occurs in the select B panel.
     */
    protected void propertyChangeInSelectB() {
        getSelectedList().setListData(getSaveListData());
    }

    /**
     * Method to save the items in List B. This is called by the OKAY button, and is probably implementation dependant.
     */
    protected void saveListB() {

        try {

            if (getSaveTagFilePanel().getSelectedFile().getName().equals(DicomDictionary.DEFAULT_DICTIONARY_FILENAME)) {
                Preferences.debug("Attempted to write DICOM dictionary file "
                        + "for converting DICOM-to-XML images to the master " + "dicom dictionary, \""
                        + DicomDictionary.SUBSET_DICTIONARY_FILENAME + "\"." + "  No file was saved.\n", 
                        Preferences.DEBUG_FILEIO);
            } else {
                Preferences.setProperty(Preferences.PREF_DICOM_SAVE_DICTIONARY, getSaveTagFilePanel().getSelectedFile()
                        .getPath());

                if ( !getSaveTagFilePanel().getSelectedFile().exists()) {

                    // create the file if it doesnt yet exist
                    getSaveTagFilePanel().getSelectedFile().createNewFile();
                }

                if (getSaveTagFilePanel().getSelectedFile().canWrite()) {

                    // save list only when there are items in the list:
                    if (getSelectedList().getModel().getSize() > 0) {
                        DicomDictionary.writeFile(getSaveTagFilePanel().getSelectedFile(), getSaveTable(),
                                "These tags were saved while converting a DICOM " + "image into MIPAV's "
                                        + destFormatStr + " format.");
                    }
                }
            }
        } catch (IOException notwritable) {
            MipavUtil.displayWarning(getSaveTagFilePanel().getSelectedFile().toString() + " cannot be written!");
            Preferences.debug("\"" + getSaveTagFilePanel().getSelectedFile().toString() + "\" cannot be written!\n", 
            		Preferences.DEBUG_FILEIO);
        }
    }

    /**
     * DOCUMENT ME!
     */
    private void addSortButtons() {

        if (selector != null) {}
    }

    // ~ Inner Classes
    // --------------------------------------------------------------------------------------------------

    /**
     * DicomTagIdentifiers allow collecting both the FileDicomTag with the associated FileDicomKey (the Group and
     * element numbers, properly formatted) of the Dicom Tag in one place, and to ensure that they only display the key
     * and the name string of the Tag in <CODE>toString()</CODE>.
     */
    public class DicomTagIdentifier {

        /** The key (held here because it is properly formatted) of the DicomTag held. */
        protected FileDicomKey key;

        /** The Dicom Tag, held here to return the name of the tag, and when we need the value when processing. */
        protected FileDicomTagInfo tag;

        /**
         * Creates a new DicomTagIdentifier object.
         * 
         * @param k DOCUMENT ME!
         * @param t DOCUMENT ME!
         */
        DicomTagIdentifier(FileDicomKey k, FileDicomTagInfo t) {
            key = k;
            tag = t;
        }

        /**
         * Accessor to the DICOM Key held.
         * 
         * @return the DICOM key refering to the tag held.
         */
        public FileDicomKey getKey() {
            return key;
        }

        /**
         * Accessor to the DICOM tag held.
         * 
         * @return The dicom tag with Group and Element number represented by the key.
         */
        public FileDicomTagInfo getTag() {
            return tag;
        }

        /**
         * Returns the group and element in parenthesis as displayed by the FileDicomKey and the name of the DICOM tag.
         * 
         * <p>
         * Example: <code>(0018,603A) Doppler Sample Volume Y Position</code>
         * </p>
         * 
         * @return Returns the group and element in parenthesis as displayed by the FileDicomKey and the name of the
         *         DICOM tag.
         */
        public String toString() {
            return new String("(" + key.toString() + ") -- " + tag.getName());
        }
    }

    /**
     * This inner class is the font style renderer for the dicom tags The left panel will display all possible DICOM
     * tags...however, if the image has that particular tag, it will be bold font
     * 
     * @author pandyan
     */
    private class MyCellRenderer extends JLabel implements ListCellRenderer {
        private Font arial13 = MipavUtil.arial13;

        private Font arial13B = MipavUtil.arial13B;

        private ArrayList<String> sourceTagsAL = new ArrayList<String>();

        public MyCellRenderer() {
            populateSourceTagsList();
        }

        public void populateSourceTagsList() {
            Enumeration<FileDicomKey> e;
            String name;
            FileDicomKey key;
            Hashtable<FileDicomKey, FileDicomTag> tagsList = sourceInfo.getTagTable().getTagList();
            int ii;
            for (ii = 0, e = tagsList.keys(); e.hasMoreElements(); ii++) {
                key = e.nextElement();
                name = key.getKey();
                if (tagsList.get(key).getValue(true) != null) {
                    String tagName = "(" + name + ")";
                    sourceTagsAL.add(tagName);
                }
            }
        }

        public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected,
                boolean cellHasFocus) {
            String s = value.toString();
            String tag = s.substring(0, (s.indexOf(")") + 1));
            if (sourceTagsAL.size() > 0) {
                for (int i = 0; i < sourceTagsAL.size(); i++) {
                    String sourceTag = sourceTagsAL.get(i);
                    if (tag.equals(sourceTag)) {
                        setFont(arial13B);
                        break;
                    } else {
                        setFont(arial13);
                    }
                }
            } else {
                setFont(arial13);
            }
            if (isSelected) {
                setBackground(list.getSelectionBackground());
                setForeground(list.getSelectionForeground());
            } else {
                setBackground(list.getBackground());
                setForeground(list.getForeground());
            }
            setEnabled(true);
            setText(s);
            setOpaque(true);

            return this;
        }
    }
}
