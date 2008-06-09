package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.FileTypeTable;

import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewImageDirectory;
import gov.nih.mipav.view.ViewImageFileFilter;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.util.Enumeration;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.JScrollPane;


/**
 * Simple dialog to choose the filter for the view image directory.
 * 
 * @author Neva Cherniavsky
 * @version 1.0 June 1, 2002
 * @see ViewImageFileFilter
 * @see ViewImageDirectory
 */
public class JDialogFilterChoice extends JDialogBase {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 3198970819287117617L;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JCheckBox[] checkImages;

    /** DOCUMENT ME! */
    private ViewImageFileFilter imageFilter;

    private String[] fileTypeDescriptions = JDialogUnknownIO.getTypeNames();

    private int[] fileTypeInts = JDialogUnknownIO.getTypeInts();

    private String[] fileTypeExtensions = JDialogUnknownIO.getTypeDefaultExtensions();

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Initializes the dialog.
     * 
     * @param parent PArent frame of dialog.
     */
    public JDialogFilterChoice(Frame parent) {
        super(parent, true);
        init();
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Sets the appropriate file filter when the "OK" button is pressed; otherwise disposes of the dialog.
     * 
     * @param e Event that triggered this function.
     */
    public void actionPerformed(ActionEvent e) {
        String command = e.getActionCommand();

        if (command.equals("OK")) {
            int count = 0;

            for (int i = 0; i < checkImages.length; i++) {

                if (checkImages[i].isSelected()) {
                    count += FileTypeTable.getFileTypeInfo(fileTypeInts[i]).getExtensionList().size();
                }
            }

            String[] exts = new String[count];
            count = 0;

            for (int i = 0; i < checkImages.length; i++) {

                if (checkImages[i].isSelected()) {
                    Enumeration<String> typeExts = FileTypeTable.getFileTypeInfo(fileTypeInts[i]).getExtensionList()
                            .elements();
                    while (typeExts.hasMoreElements()) {
                        exts[count++] = typeExts.nextElement();
                    }
                }
            }

            imageFilter = new ViewImageFileFilter(exts);
            dispose();
        } else if (command.equals("Cancel")) {
            cancelFlag = true;
            dispose();
        } else if (command.equals("Check")) {

            for (int i = 0; i < checkImages.length; i++) {
                checkImages[i].setSelected(true);
            }
        } else if (command.equals("Uncheck")) {

            for (int i = 0; i < checkImages.length; i++) {
                checkImages[i].setSelected(false);
            }
        }
    }

    /**
     * Accessor that gets the file filter.
     * 
     * @return The file filter.
     */
    public ViewImageFileFilter getFilter() {
        return imageFilter;
    }

    /**
     * Initializes the dialog and adds the GUI components.
     */
    private void init() {
        setTitle("Choose Image Filter");

        JPanel panel = new JPanel(new GridLayout(fileTypeDescriptions.length, 1));
        panel.setForeground(Color.white);
        panel.setBackground(Color.white);

        // set the filter type to the preferences saved filter
        int filter = 0;

        try {
            filter = Integer.parseInt(Preferences.getProperty(Preferences.PREF_FILENAME_FILTER));
        } catch (NumberFormatException nfe) {

            // an invalid value was set in preferences -- so don't use it!
            filter = -1;
        }

        imageFilter = new ViewImageFileFilter(filter);

        checkImages = new JCheckBox[fileTypeDescriptions.length];

        for (int i = 0; i < fileTypeDescriptions.length; i++) {
            checkImages[i] = new JCheckBox(fileTypeDescriptions[i]);
            checkImages[i].setFont(serif12);
            checkImages[i].setForeground(Color.black);
            checkImages[i].setBackground(Color.white);

            if (imageFilter.accept(fileTypeExtensions[i])) {
                checkImages[i].setSelected(true);
            }

            panel.add(checkImages[i]);
        }

        JScrollPane sp = new JScrollPane(panel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(sp);
        mainPanel.setBorder(buildTitledBorder("Choose types of images to display"));
        mainPanel.setPreferredSize(new Dimension(250, 400));

        JButton checkAll = new JButton("Select all");
        checkAll.setFont(serif12B);
        checkAll.setForeground(Color.black);
        checkAll.setPreferredSize(new Dimension(100, 30));
        checkAll.addActionListener(this);
        checkAll.setActionCommand("Check");

        JButton uncheckAll = new JButton("Clear");
        uncheckAll.setFont(serif12B);
        uncheckAll.setForeground(Color.black);
        uncheckAll.setPreferredSize(new Dimension(100, 30));
        uncheckAll.addActionListener(this);
        uncheckAll.setActionCommand("Uncheck");

        JPanel checkPanel = new JPanel();
        checkPanel.add(checkAll);
        checkPanel.add(uncheckAll);

        mainPanel.add(checkPanel, BorderLayout.SOUTH);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }
}
