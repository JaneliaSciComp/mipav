package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileTypeInfo;
import gov.nih.mipav.model.file.FileTypeTable;

import java.awt.BorderLayout;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListSelectionModel;


/**
 * Simple dialog to indicate type of image when the program doesn't recognize the name.
 * 
 * @version 1.0 Feb 4, 1999
 * @author Matthew McAuliffe, Ph.D.
 * @see FileIO
 */
public class JDialogUnknownIO extends JDialogBase {

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 960085709232971480L;

    /** DOCUMENT ME! */
    private static String[] typeDescriptions;

    /** DOCUMENT ME! */
    private static String[] typeDefaultExtensions;

    /** DOCUMENT ME! */
    private static int[] typeInts;

    static {
        Hashtable<Integer, FileTypeInfo> fileTypeTable = FileTypeTable.getFileTypeTable();
        Enumeration<Integer> fileTypeEnum = fileTypeTable.keys();

        Vector<Integer> fileTypeTemp = new Vector<Integer>(fileTypeTable.size());
        for (int i = 0; fileTypeEnum.hasMoreElements(); i++) {
            int type = fileTypeEnum.nextElement();
            String ext = FileTypeTable.getFileTypeInfo(type).getDefaultExtension();
            String desc = FileTypeTable.getFileTypeInfo(type).getDescription();

            // skip any file types without any extensions assigned
            if ( !ext.equals("") && !desc.equals("JIMI") && !desc.contains("multifile")) {
                fileTypeTemp.add(type);
            }
        }

        typeDescriptions = new String[fileTypeTemp.size()];
        int[] fileTypeIntsNotSorted = new int[fileTypeTemp.size()];
        String[] fileTypeDefaultSuffixesNotSorted = new String[fileTypeTemp.size()];
        for (int i = 0; i < fileTypeTemp.size(); i++) {
            fileTypeIntsNotSorted[i] = fileTypeTemp.elementAt(i);
            typeDescriptions[i] = FileTypeTable.getFileTypeInfo(fileTypeIntsNotSorted[i]).getExtendedDescription();
            fileTypeDefaultSuffixesNotSorted[i] = FileTypeTable.getFileTypeInfo(fileTypeIntsNotSorted[i])
                    .getDefaultExtension();

        }

        // sort the file type descriptions and then base the sorting of the file type ints on this ordering
        Arrays.sort(typeDescriptions);
        typeInts = new int[typeDescriptions.length];
        typeDefaultExtensions = new String[typeDescriptions.length];
        for (int i = 0; i < typeDescriptions.length; i++) {
            for (int j = 0; j < fileTypeIntsNotSorted.length; j++) {
                if (typeDescriptions[i].equals(FileTypeTable.getFileTypeInfo(fileTypeIntsNotSorted[j])
                        .getExtendedDescription())) {
                    typeInts[i] = fileTypeIntsNotSorted[j];
                    typeDefaultExtensions[i] = fileTypeDefaultSuffixesNotSorted[j];
                }
            }
        }
    }

    /** DOCUMENT ME! */
    private int imageType;

    /** DOCUMENT ME! */
    private JList list;

    /** DOCUMENT ME! */
    private String suffix = null;

    /**
     * Creates and displays dialog.
     * 
     * @param theParentFrame Parent frame.
     * @param title Title of dialog frame.
     */
    public JDialogUnknownIO(Frame theParentFrame, String title) {
        super(theParentFrame, true);

        setTitle(title);
        setResizable(true);

        list = new JList(typeDescriptions);
        list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        list.setVisibleRowCount(15);

        JScrollPane sp = new JScrollPane(list, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(sp);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel OKCancelPanel = new JPanel();
        OKButton = buildOKButton();
        cancelButton = buildCancelButton();
        OKCancelPanel.add(OKButton);
        OKCancelPanel.add(cancelButton);

        getContentPane().add(mainPanel);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);

        pack();
    }

    /**
     * Accessor that returns the static list of type names.
     * 
     * @return Array of type names for IO.
     */
    public static String[] getTypeNames() {
        return typeDescriptions;
    }

    /**
     * Accessor that returns the static list of type integers.
     * 
     * @return Array of type integers for IO.
     */
    public static int[] getTypeInts() {
        return typeInts;
    }

    /**
     * Accessor that returns the static list of type extensions (just the default extension).
     * 
     * @return Array of type extensions for IO.
     */
    public static String[] getTypeDefaultExtensions() {
        return typeDefaultExtensions;
    }

    /**
     * Closes dialog box when the OK button is pressed and sets the image type and suffix.
     * 
     * @param event Event that triggers this function.
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();

        if (source == OKButton) {
            int index = list.getSelectedIndex();
            imageType = typeInts[index];
            suffix = typeDefaultExtensions[index];
            dispose();
        } else if (source == cancelButton) {
            cancelFlag = true;
            dispose();
        } else {
            super.actionPerformed(event);
        }
    }

    /**
     * Accessor that returns the image type.
     * 
     * @return The image type.
     */
    public int getFileType() {
        return imageType;
    }

    /**
     * Accessor that returns the suffix.
     * 
     * @return The suffix.
     */
    public String getSuffix() {
        return suffix;
    }
}
