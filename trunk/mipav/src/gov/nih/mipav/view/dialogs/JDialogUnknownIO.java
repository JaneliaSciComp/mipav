package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Simple dialog to indicate type of image when the program doesn't recognize the name.
 *
 * @version  1.0 Feb 4, 1999
 * @author   Matthew McAuliffe, Ph.D.
 * @see      FileIO
 */
public class JDialogUnknownIO extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 960085709232971480L;

    /** DOCUMENT ME! */
    private static String[] typeNames = FileUtility.getUnknownDialogsTypeNames();

    /** DOCUMENT ME! */
    private static String[] typeSuffices = FileUtility.getUnknownDialogsTypeSuffices();
    /** DOCUMENT ME! */
    
    private static int[] typeInts = FileUtility.getUnknownDialogsTypeInts();

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int imageType;

    /** DOCUMENT ME! */
    private JList list;

    /** DOCUMENT ME! */
    private String suffix = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates and displays dialog.
     *
     * @param  theParentFrame  Parent frame.
     * @param  title           Title of dialog frame.
     */
    public JDialogUnknownIO(Frame theParentFrame, String title) {
        super(theParentFrame, true);

        setTitle(title);
        setResizable(true);

        list = new JList(typeNames);
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

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Accessor that returns the suffix, without the ".", based on the index.
     *
     * @param   index  Index into suffix array.
     *
     * @return  Suffix without ".".
     */
    public static String getSuffixFromIndex(int index) {
        String end = typeSuffices[index];

        if (end.length() < 1) {
            return "";
        }

        return end.substring(1);
    }

    /**
     * Accessor that returns the FileBase image type based on the index.
     *
     * @param   index  Index into file type array.
     *
     * @return  FileBase type.
     */
    public static int getTypeFromIndex(int index) {
        return typeInts[index];
    }

    /**
     * Accessor that returns the static list of type names.
     *
     * @return  Array of type names for IO.
     */
    public static String[] getTypeNames() {
        return typeNames;
    }

    /**
     * Closes dialog box when the OK button is pressed and sets the image type and suffix.
     *
     * @param  event  Event that triggers this function.
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();

        if (source == OKButton) {
            int index = list.getSelectedIndex();
            imageType = typeInts[index];
            suffix = typeSuffices[index];
            dispose();
        } else if (source == cancelButton) {
            cancelFlag = true;
            dispose();
        }
    }

    /**
     * Accessor that returns the image type.
     *
     * @return  The image type.
     */
    public int getImageType() {
        return imageType;
    }

    /**
     * Accessor that returns the suffix.
     *
     * @return  The suffix.
     */
    public String getSuffix() {
        return suffix;
    }
}
