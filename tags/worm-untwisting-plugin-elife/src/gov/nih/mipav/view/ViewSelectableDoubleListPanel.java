package gov.nih.mipav.view;


import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * DOCUMENT ME!
 */
public class ViewSelectableDoubleListPanel extends JPanel implements ActionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -7066171606587907360L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected JButton btnAdd;

    /** DOCUMENT ME! */
    protected JButton btnRemove;

    /** DOCUMENT ME! */
    protected GridBagConstraints gbConstraints;

    /** DOCUMENT ME! */
    protected GridBagLayout gbLayout;

    /** DOCUMENT ME! */
    protected JTable leftTable;

    /** DOCUMENT ME! */
    protected JTable rightTable;

    //~ Constructors ---------------------------------------------------------------------------------------------------


    /**
     * Creates a new ViewSelectableDoubleListPanel object.
     *
     * @param  leftTable   DOCUMENT ME!
     * @param  rightTable  DOCUMENT ME!
     */
    public ViewSelectableDoubleListPanel(JTable leftTable, JTable rightTable) {
        this.leftTable = leftTable;
        this.rightTable = rightTable;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent event) {
        /*
         * String command = event.getActionCommand();
         *
         * if (command.equals("add")) { int [] selectedRows = leftTable.getSelectedRows();
         *
         * TableSorter rightTableSorter = (TableSorter) rightTable.getModel(); TableSorter leftTableSorter = (TableSorter)
         * leftTable.getModel();
         *
         * SortingTableModel rightTableModel = (SortingTableModel) rightTableSorter.getTableModel(); SortingTableModel
         * leftTableModel = (SortingTableModel) leftTableSorter.getTableModel();
         *
         * for (int i = 0; i < selectedRows.length; i++) {
         * rightTableModel.addRow(leftTableModel.getRow(leftTableSorter.modelIndex(selectedRows[i]))); } } else if
         * (command.equals("remove")) { int [] selectedRows = rightTable.getSelectedRows();
         *
         * TableSorter rightTableSorter = (TableSorter) rightTable.getModel(); SortingTableModel rightTableModel =
         * (SortingTableModel) rightTableSorter.getTableModel();
         *
         */
    }

    /**
     * DOCUMENT ME!
     */
    public void buildGUI() {
        gbLayout = new GridBagLayout();
        gbConstraints = new GridBagConstraints();

        btnAdd = new JButton("Add >>");
        btnAdd.setActionCommand("add");
        btnRemove = new JButton("<< Remove");
        btnRemove.setActionCommand("remove");

        gbConstraints.gridwidth = 10;
        gbConstraints.gridheight = 10;
        gbConstraints.weightx = 1;
        gbConstraints.weighty = 1;
        gbConstraints.insets = new Insets(2, 2, 2, 2);
        gbConstraints.fill = GridBagConstraints.BOTH;

        JScrollPane scrollPane = new JScrollPane(leftTable);
        gbLayout.setConstraints(scrollPane, gbConstraints);
        add(scrollPane);

        gbConstraints.gridwidth = 3;
        gbConstraints.gridheight = 1;
        gbConstraints.weightx = 0;
        gbConstraints.weighty = 0;
        gbConstraints.gridx = 11;

        gbLayout.setConstraints(btnAdd, gbConstraints);
        add(btnAdd);

        gbConstraints.gridy = 1;

        gbLayout.setConstraints(btnRemove, gbConstraints);
        add(btnRemove);

        gbConstraints.gridy = 2;

        gbConstraints.gridwidth = 10;
        gbConstraints.gridheight = 10;
        gbConstraints.weightx = 1;
        gbConstraints.weighty = 1;
        gbConstraints.gridx = 14;
        gbConstraints.gridy = 0;

        scrollPane = new JScrollPane(rightTable);
        gbLayout.setConstraints(scrollPane, gbConstraints);
        add(scrollPane);

        setLayout(gbLayout);

        btnAdd.addActionListener(this);
        btnRemove.addActionListener(this);
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public JTable getLeftTable() {
        return leftTable;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public JTable getRightTable() {
        return rightTable;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  leftTable  DOCUMENT ME!
     */
    public void setLeftTable(JTable leftTable) {
        this.leftTable = leftTable;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  rightTable  DOCUMENT ME!
     */
    public void setRightTable(JTable rightTable) {
        this.rightTable = rightTable;
    }

}
