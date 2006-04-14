package gov.nih.mipav.view;

import java.awt.event.*;
import java.util.*;
import javax.swing.*;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.*;

public class ViewDICOMDoubleListPanel extends ViewSelectableDoubleListPanel
{
    protected JButton btnMoveUp;
    protected JButton btnMoveDown;

    protected TableSorter rightTableSorter;
    protected TableSorter leftTableSorter;

    protected SortingTableModel rightTableModel;
    protected SortingTableModel leftTableModel;

    public ViewDICOMDoubleListPanel(JTable leftTable, JTable rightTable)
    {
        super(leftTable, rightTable);

        rightTableSorter = (TableSorter) rightTable.getModel();
        leftTableSorter = (TableSorter) leftTable.getModel();

        rightTableModel = (SortingTableModel) rightTableSorter.getTableModel();
        leftTableModel = (SortingTableModel) leftTableSorter.getTableModel();

        rightTable.setSelectionMode(ListSelectionModel.
                                    SINGLE_INTERVAL_SELECTION);
    }

    public void actionPerformed(ActionEvent event)
    {
        String command = event.getActionCommand();

        if (command.equals("add"))
        {
            int[] selectedRows = leftTable.getSelectedRows();

            for (int i = 0; i < selectedRows.length; i++)
            {
                Vector dicomRowVector = leftTableModel.getRow(leftTableSorter.modelIndex(selectedRows[i]));

                Object object = dicomRowVector.elementAt(0);
                if (object instanceof FileDicomKey)
                {
                    FileDicomKey dicomKey = (FileDicomKey) dicomRowVector.elementAt(0);

                    dicomRowVector.setElementAt(dicomKey.getKey(), 0); // change the key from a FileDicomKey to a String
                }
                else
                {
                    dicomRowVector.setElementAt(object, 0); // should be String, namely the CUSTOM tag
                }
                rightTableModel.addRow(dicomRowVector);
            }

            leftTable.clearSelection();
        }
        else if (command.equals("remove"))
        {
            int[] selectedRows = rightTable.getSelectedRows();

            // todo: explain wtf is happening here: why reverse counter loop?
            for (int i = selectedRows.length - 1; i >= 0; i--)
            {
                Vector row = rightTableModel.getRow(rightTableSorter.modelIndex(selectedRows[i]));

                String tagName = (String) row.elementAt(0);

                if (tagName.equals("Instance (formerly Image) Number"))
                {
                    MipavUtil.displayInfo("Can't remove 'Instance (formerly Image) Number' column.");
                    continue;
                }

                rightTableModel.removeRow(rightTableSorter.modelIndex(selectedRows[i]));
            }
        }
        else if (command.equals("move up") || command.equals("move down"))
        {
            /**
             * This is an ugly algorithm, but I don't know any other way of doing it. The problem is that
             * the TableSorter blah blah blah todo:finish this explanation
             *
             */

            // save current row order into vector
            Vector currentRowOrder = getCurrentRowOrder(rightTableSorter);

            int[] selectedRows = rightTable.getSelectedRows();

            if (selectedRows.length < 1 || currentRowOrder.size() <= 1)
            {
                return;
            }

            if (command.equals("move up"))
            {
                // reorder the elements that need reordering
                for (int i = 0; i < selectedRows.length; i++)
                {
                    if (selectedRows[i] - 1 >= 0)
                    {
                        Object moveObject = currentRowOrder.elementAt(selectedRows[i]);

                        currentRowOrder.removeElementAt(selectedRows[i]);

                        currentRowOrder.insertElementAt(moveObject, selectedRows[i] - 1);
                    }
                }
            }
            else if (command.equals("move down"))
            {
                for (int i = selectedRows.length - 1; i >= 0; i--)
                {
                    if (selectedRows[i] + 1 < currentRowOrder.size())
                    {
                        Object moveObject = currentRowOrder.elementAt(selectedRows[i]);

                        currentRowOrder.removeElementAt(selectedRows[i]);

                        currentRowOrder.insertElementAt(moveObject, selectedRows[i] + 1);
                    }
                }
            }

            // remove all rows from current table
            rightTableModel.removeAllRows();

            // set the table sorting status to "unsorted"
            rightTableSorter.setSortingStatus(0, TableSorter.NOT_SORTED);

            // re-add the reordered rows
            for (int i = 0; i < currentRowOrder.size(); i++)
            {
                rightTableModel.addRow((Vector) currentRowOrder.elementAt(i));
            }

            // re-select previously selected rows
            if (command.equals("move up"))
            {
                int selection0 = Math.max(0, (selectedRows[0] - 1));
                int selection1 = Math.max(0, (selectedRows[selectedRows.length - 1] - 1));

                rightTable.setRowSelectionInterval(selection0, selection1);
            }
            else if (command.equals("move down"))
            {
                int selection0 = Math.min(rightTableModel.getRowCount() - 1, (selectedRows[0] + 1));
                int selection1 = Math.min(rightTableModel.getRowCount() - 1, (selectedRows[selectedRows.length - 1] + 1));

                rightTable.setRowSelectionInterval(selection0, selection1);
            }
        }
    }

    protected static Vector getCurrentRowOrder(TableSorter tableSorter)
    {
        SortingTableModel tableModel = (SortingTableModel) tableSorter.
                                       getTableModel();

        Vector currentOrder = new Vector();

        for (int i = 0; i < tableModel.getRowCount(); i++)
        {
            Vector row = tableModel.getRow(tableSorter.modelIndex(i));

            currentOrder.addElement(row);
        }

        return currentOrder;
    }

    public void buildGUI()
    {
        super.buildGUI();

        btnMoveUp = new JButton("Move up >");
        btnMoveUp.setActionCommand("move up");
        btnMoveUp.addActionListener(this);

        gbConstraints.gridy = 2;
        gbConstraints.gridwidth = 3;
        gbConstraints.gridheight = 1;
        gbConstraints.weightx = 0;
        gbConstraints.weighty = 0;
        gbConstraints.gridx = 11;

        gbLayout.setConstraints(btnMoveUp, gbConstraints);
        add(btnMoveUp);

        btnMoveDown = new JButton("Move down >");
        btnMoveDown.setActionCommand("move down");
        btnMoveDown.addActionListener(this);

        gbConstraints.gridy = 3;

        gbLayout.setConstraints(btnMoveDown, gbConstraints);
        add(btnMoveDown);
    }
}
