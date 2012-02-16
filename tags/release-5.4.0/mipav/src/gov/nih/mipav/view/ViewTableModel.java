package gov.nih.mipav.view;


import java.io.*;

import java.util.*;

import javax.swing.table.*;


/**
 * This is a simple class that creates a DefaultTableModel with uneditable cells. With the cells uneditable, the table
 * may react to mouse events, such as double clicking on a row.
 *
 * @version  0.1 June 30, 1999
 * @author   Neva Cherniavsky
 */

public class ViewTableModel extends DefaultTableModel implements Serializable {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 487484434950231158L;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Calls super constructor.
     */
    public ViewTableModel() {
        super();
    }

    /**
     * Constructs a new table model with the given column names and row count.
     *
     * @param  columnNames  Names of columns.
     * @param  rowCount     Number of rows.
     */
    public ViewTableModel(Object[] columnNames, int rowCount) {
        super(columnNames, rowCount);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Copies the object that extends this class.
     *
     * @return  An exact copy of this class.
     */
    public Object clone() {

        try {
            ByteArrayOutputStream bout = new ByteArrayOutputStream();
            ObjectOutputStream out = new ObjectOutputStream(bout);
            out.writeObject(this);

            ByteArrayInputStream bin = new ByteArrayInputStream(bout.toByteArray());
            ObjectInputStream in = new ObjectInputStream(bin);

            Object ret = in.readObject();
            in.close();

            return ret;

        } catch (Exception e) {
            MipavUtil.displayError("Object not serializable");

            return null;
        }
    }

    /**
     * Returns the class of the item in the column.
     *
     * @param   col  Column index.
     *
     * @return  Class of column.
     */
    public Class<? extends Object> getColumnClass(int col) {

        try {
            Vector v = (Vector) dataVector.elementAt(0);

            return v.elementAt(col).getClass();
        } catch (NullPointerException e) {
            return super.getColumnClass(col);
        }
    }

    /**
     * Gets the index of the given column name.
     *
     * @param   name  Name of column.
     *
     * @return  Index of column when created, or -1 if none.
     */
    public int getColumnIndex(String name) {

        for (int i = 0; i < columnIdentifiers.size(); i++) {

            if (name.equals(columnIdentifiers.elementAt(i))) {
                return i;
            }
        }

        return -1;
    }

    /**
     * Gets the index of a column name that starts with the given string
     * @param name String name of column
     * @return int Index of column, or -1 if none
     */
    public int getColumnStartsWithIndex(String name) {

        for (int i = 0; i < columnIdentifiers.size(); i++) {

            if (((String)columnIdentifiers.elementAt(i)).startsWith(name)) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Gets the index of the given column name.
     *
     * @param   name  Name of column.
     *
     * @return  Index of column when created, or -1 if none.
     */
    public int getColumnBaseIndex(String name) {
        int strLength;
        for (int i = 0; i < columnIdentifiers.size(); i++) {
            strLength = ((String)columnIdentifiers.elementAt(i)).length();
            if ((strLength >= name.length()) &&
                (name.equals(((String)columnIdentifiers.elementAt(i)).substring(0,name.length())))) {
                return i;
            }
        }

        return -1;
    }

    /**
     * Returns false for all cells, so none can be edited.
     *
     * @param   x  x value of cell
     * @param   y  y value of cell
     *
     * @return  false, always
     */
    public boolean isCellEditable(int x, int y) {
        return false;
    }

    /**
     * Sets the column name to the new name and fires the table event. Table widths must be reset after this call.
     *
     * @param  col      Column to rename
     * @param  newName  New name.
     */
    public void setColumnName(int col, Object newName) {
        columnIdentifiers.setElementAt(newName, col);
        fireTableStructureChanged();
    }

    /**
     * Sets the value at the given row and column of the table.
     *
     * @param  value  Value to set.
     * @param  row    Row to set value at.
     * @param  col    Column to set value at.
     */
    public void setValueAt(Object value, int row, int col) {
        super.setValueAt(value, row, col);
        fireTableCellUpdated(row, col);
    }

    /**
     * Updates the bulbs.
     *
     * @param  selectedRow  Row to update.
     */
    public void updateBulbs(int selectedRow) {

        for (int r = 0; r < getRowCount(); r++) {

            if (r == selectedRow) {
                setValueAt(Boolean.TRUE, r, 0);
            } else {
                setValueAt(Boolean.FALSE, r, 0);
            }
        }
    }

}
