package gov.nih.mipav.model.structures;

import java.util.*;
import javax.swing.table.*;

public class SortingTableModel extends DefaultTableModel
{
    public static final String EMPTY_CELL = "";

    private Hashtable htColumnClass;

    public SortingTableModel()
    {
        super();

        htColumnClass = new Hashtable();
    }

    public Vector getRow(int rowNumber)
    {
        Vector row = new Vector();

        for (int i = 0; i < getColumnCount(); i++)
        {
            row.addElement(getValueAt(rowNumber, i));
        }

        return row;
    }

    public Vector getColumnNames()
    {
        Vector columnNames = new Vector();

        for (int i = 0; i < getColumnCount(); i++)
        {
            columnNames.addElement(getColumnName(i));
        }

        return columnNames;
    }

    public boolean isCellEditable(int row, int col)
    {
        return false;
    }

    public void removeAllRows()
    {
        for (int i = getRowCount() - 1; i >= 0; i--)
        {
            removeRow(i);
        }
    }

    public Class getColumnClass(int columnIndex)
    {
        Class classObj = (Class) htColumnClass.get(new Integer(columnIndex));

        if (classObj == null)
        {
            return new String().getClass();
        }

        return classObj;
    }

    public void setColumnClass(Class classType, int columnIndex)
    {
        htColumnClass.put(new Integer(columnIndex), classType);
    }
}
