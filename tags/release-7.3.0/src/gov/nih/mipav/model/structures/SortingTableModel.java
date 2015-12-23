package gov.nih.mipav.model.structures;


import java.util.*;

import javax.swing.table.*;


/**
 * DOCUMENT ME!
 */
public class SortingTableModel extends DefaultTableModel {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -1782879801334035117L;

    /** DOCUMENT ME! */
    public static final String EMPTY_CELL = "";

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private Hashtable<Integer,Class<?>> htColumnClass;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new SortingTableModel object.
     */
    public SortingTableModel() {
        super();

        htColumnClass = new Hashtable<Integer,Class<?>>();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param   columnIndex  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Class<?> getColumnClass(int columnIndex) {
        Class<?> classObj = (Class<?>) htColumnClass.get(new Integer(columnIndex));

        if (classObj == null) {
            return new String().getClass();
        }

        return classObj;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Vector<String> getColumnNames() {
        Vector<String> columnNames = new Vector<String>();

        for (int i = 0; i < getColumnCount(); i++) {
            columnNames.addElement(getColumnName(i));
        }

        return columnNames;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   rowNumber  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Vector<Object> getRow(int rowNumber) {
        Vector<Object> row = new Vector<Object>();

        for (int i = 0; i < getColumnCount(); i++) {
            row.addElement(getValueAt(rowNumber, i));
        }

        return row;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   row  DOCUMENT ME!
     * @param   col  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean isCellEditable(int row, int col) {
        return false;
    }

    /**
     * DOCUMENT ME!
     */
    public void removeAllRows() {

        for (int i = getRowCount() - 1; i >= 0; i--) {
            removeRow(i);
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  classType    DOCUMENT ME!
     * @param  columnIndex  DOCUMENT ME!
     */
    public void setColumnClass(Class<?> classType, int columnIndex) {
        htColumnClass.put(new Integer(columnIndex), classType);
    }
}
