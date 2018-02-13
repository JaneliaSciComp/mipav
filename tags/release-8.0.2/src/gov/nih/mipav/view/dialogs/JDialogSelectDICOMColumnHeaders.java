package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * DOCUMENT ME!
 */
public class JDialogSelectDICOMColumnHeaders extends JDialogBase implements ActionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 5883528252352616682L;

    /** DOCUMENT ME! */
    protected static String APPLY = "Apply";

    /** DOCUMENT ME! */
    protected static String CLOSE = "Close";

    /** DOCUMENT ME! */
    protected static String REMEMBER = "Remember this configuration";

    /** DOCUMENT ME! */
    public static String CUSTOM = "non-native";

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected ViewSelectableDoubleListPanel listPanel;

    /** DOCUMENT ME! */
    protected ViewJFrameDICOMParser parentFrame;

    /** DOCUMENT ME! */
    protected JTable rightTable;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogSelectDICOMColumnHeaders object.
     *
     * @param  parentFrame  DOCUMENT ME!
     */
    public JDialogSelectDICOMColumnHeaders(ViewJFrameDICOMParser parentFrame) {
        super(parentFrame, true);

        setTitle("Configure DICOM columns");

        this.parentFrame = parentFrame;

        setSize(640, 480); // default height and width to make this component look good on screen

        MipavUtil.centerOnScreen(this);

        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);

        getContentPane().add(buildInterface());

        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals(APPLY)) {
            parentFrame.setHeaderConfiguration(rightTable);
            parentFrame.reloadRows();
        } else if (command.equals(CLOSE)) {
            dispose();
        } else if (command.equals(REMEMBER)) {
            TableSorter tableSorter = (TableSorter) rightTable.getModel();
            SortingTableModel tableModel = (SortingTableModel) tableSorter.getTableModel();

            Vector<String> configuredColumnsVector = new Vector<String>();

            for (int i = 0; i < tableModel.getRowCount(); i++) {
                Vector<Object> row = tableModel.getRow(i);

                configuredColumnsVector.addElement((String)row.elementAt(1)); // index 1 is the index of the column name in the
                                                                      // table
            }

            boolean success = Preferences.setDICOMBrowserTableConfiguration(configuredColumnsVector);

            if (success) {
                MipavUtil.displayInfo("Configuration saved.");
            }
            // Preferences class will display error message in case of error
        } else {
            super.actionPerformed(event);
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    protected JPanel buildInterface() {
        JPanel mainPanel = new JPanel(new BorderLayout());

        mainPanel.add(buildMainPanel(), BorderLayout.CENTER);

        mainPanel.add(buildOKCancelPanel(), BorderLayout.SOUTH);

        return mainPanel;
    }


    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    @SuppressWarnings("unchecked")
    protected JPanel buildMainPanel() {
        Vector<String> columnNames = parentFrame.getColumnNames();

        SortingTableModel rightTableModel = new SortingTableModel();

        TableSorter tableSorter = new TableSorter(rightTableModel);
        rightTable = new JTable(tableSorter);
        tableSorter.setTableHeader(rightTable.getTableHeader());

        tableSorter.setColumnComparator(new String().getClass(), TableSorter.LEXICAL_COMPARATOR);

        rightTableModel.addColumn("Key");
        rightTableModel.addColumn("Selection");
        rightTable.getColumn("Key").setMinWidth(0);
        rightTable.getColumn("Key").setMaxWidth(0);

        for (int i = 0; i < columnNames.size(); i++) {
            Vector<String> newRow = new Vector<String>();

            String keyStr = DicomDictionary.getKeyFromTagName((String) columnNames.elementAt(i));

            if (keyStr == null) {
                newRow.addElement(CUSTOM);
            } else {
                newRow.addElement(keyStr);
            }

            newRow.addElement((String) columnNames.elementAt(i));

            rightTableModel.addRow(newRow);
        }

        SortingTableModel leftTableModel = new SortingTableModel();

        tableSorter = new TableSorter(leftTableModel);

        JTable leftTable = new JTable(tableSorter);
        tableSorter.setTableHeader(leftTable.getTableHeader());

        tableSorter.setColumnComparator(new String().getClass(), TableSorter.LEXICAL_COMPARATOR);

        /** X-position, Y-position, and Z-position are not DICOM tags. But there is a good
         *  chance the user will want to sort on them, so they are included ad-hoc */
        Vector<String> tagsVector = getNiceDICOMTags();
        tagsVector.addElement("X-position");
        tagsVector.addElement("Y-position");
        tagsVector.addElement("Z-position");

        /** There are no keys associated with these tags, so CUSTOM entries are added */
        Vector<String> keysVector = new Vector(DicomDictionary.getDicomTagTable().keySet());
        keysVector.addElement(CUSTOM);
        keysVector.addElement(CUSTOM);
        keysVector.addElement(CUSTOM);

        leftTableModel.addColumn("Key", keysVector);
        leftTableModel.addColumn("Value", tagsVector);

        // todo: un-hard-code this value
        leftTable.getColumn("Key").setPreferredWidth(70);

        JPanel mainPanel = new JPanel(new GridLayout(1, 1));

        listPanel = new ViewDICOMDoubleListPanel(leftTable, rightTable);
        listPanel.buildGUI();
        mainPanel.add(listPanel);

        return mainPanel;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    protected JPanel buildOKCancelPanel() {
        JButton btnApply = new JButton(APPLY);
        btnApply.addActionListener(this);
        btnApply.setActionCommand(APPLY);

        JButton btnClose = new JButton(CLOSE);
        btnClose.addActionListener(this);
        btnClose.setActionCommand(CLOSE);

        JButton btnRemember = new JButton(REMEMBER);
        btnRemember.addActionListener(this);
        btnRemember.setActionCommand(REMEMBER);

        GridBagLayout gbLayout = new GridBagLayout();
        JPanel subPanel = new JPanel(gbLayout);
        GridBagConstraints gbConstraints = new GridBagConstraints();

        gbConstraints.insets = new Insets(3, 3, 3, 3); // number of pixels around edges of layout
        gbConstraints.ipadx = 3; // number of pixels between the buttons
        gbLayout.setConstraints(btnApply, gbConstraints);
        subPanel.add(btnApply);

        gbConstraints.gridx = 1;
        gbConstraints.fill = GridBagConstraints.RELATIVE;
        gbLayout.setConstraints(btnClose, gbConstraints);
        subPanel.add(btnClose);

        gbConstraints.gridx = 2;
        gbConstraints.fill = GridBagConstraints.REMAINDER;
        gbLayout.setConstraints(btnRemember, gbConstraints);
        subPanel.add(btnRemember);

        return subPanel;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    protected Vector getNiceDICOMTags() {
        Hashtable<FileDicomKey,FileDicomTagInfo> tagsTable = DicomDictionary.getDicomTagTable();
        Vector tagsVector = new Vector(tagsTable.values());

        for (int i = 0; i < tagsVector.size(); i++) {
            FileDicomTagInfo fileDICOMTag = (FileDicomTagInfo) tagsVector.elementAt(i);

            tagsVector.setElementAt(fileDICOMTag.getName(), i);
        }

        return tagsVector;
    }
}
