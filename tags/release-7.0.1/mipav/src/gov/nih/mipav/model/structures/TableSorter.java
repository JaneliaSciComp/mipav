package gov.nih.mipav.model.structures;


import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.event.InputEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.swing.Icon;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;


/**
 * TableSorter is a decorator for TableModels; adding sorting functionality to a supplied TableModel. TableSorter does
 * not store or copy the data in its TableModel; instead it maintains a map from the row indexes of the view to the row
 * indexes of the model. As requests are made of the sorter (like getValueAt(row, col)) they are passed to the
 * underlying model after the row numbers have been translated via the internal mapping array. This way, the TableSorter
 * appears to hold another copy of the table with the rows in a different order.
 * 
 * <p>
 * TableSorter registers itself as a listener to the underlying model, just as the JTable itself would. Events recieved
 * from the model are examined, sometimes manipulated (typically widened), and then passed on to the TableSorter's
 * listeners (typically the JTable). If a change to the model has invalidated the order of TableSorter's rows, a note of
 * this is made and the sorter will resort the rows the next time a value is requested.
 * </p>
 * 
 * <p>
 * When the tableHeader property is set, either by using the setTableHeader() method or the two argument constructor,
 * the table header may be used as a complete UI for TableSorter. The default renderer of the tableHeader is decorated
 * with a renderer that indicates the sorting status of each column. In addition, a mouse listener is installed with the
 * following behavior:
 * </p>
 * 
 * <ul>
 * <li>Mouse-click: Clears the sorting status of all other columns and advances the sorting status of that column
 * through three values: {NOT_SORTED, ASCENDING, DESCENDING} (then back to NOT_SORTED again).</li>
 * <li>SHIFT-mouse-click: Clears the sorting status of all other columns and cycles the sorting status of the column
 * through the same three values, in the opposite order: {NOT_SORTED, DESCENDING, ASCENDING}.</li>
 * <li>CONTROL-mouse-click and CONTROL-SHIFT-mouse-click: as above except that the changes to the column do not cancel
 * the statuses of columns that are already sorting - giving a way to initiate a compound sort.</li>
 * </ul>
 * 
 * <p>
 * This is a long overdue rewrite of a class of the same name that first appeared in the swing table demos in 1997.
 * </p>
 * 
 * <hr>
 * 
 * <pre>
 * Copyright (c) 1995 - 2008 Sun Microsystems, Inc.  All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 * 
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 * 
 *   - Neither the name of Sun Microsystems nor the names of its
 *     contributors may be used to endorse or promote products derived
 *     from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS &quot;AS
 * IS&quot; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * </pre>
 * 
 * @author Philip Milne
 * @author Brendon McLean
 * @author Dan van Enckevort
 * @author Parwinder Sekhon
 * @version 2.0 02/27/04
 */

public class TableSorter extends AbstractTableModel {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 3279717528747239046L;

    /** DOCUMENT ME! */
    public static final int DESCENDING = -1;

    /** DOCUMENT ME! */
    public static final int NOT_SORTED = 0;

    /** DOCUMENT ME! */
    public static final int ASCENDING = 1;

    /** DOCUMENT ME! */
    private static Directive EMPTY_DIRECTIVE = new Directive( -1, TableSorter.NOT_SORTED);

    /** DOCUMENT ME! */
    @SuppressWarnings("unchecked")
    public static final Comparator<Object> COMPARABLE_COMPARATOR = new Comparator<Object>() {
        public int compare(final Object o1, final Object o2) {
            return ((Comparable<Object>) o1).compareTo(o2);
        }
    };

    /** DOCUMENT ME! */
    public static final Comparator<Object> LEXICAL_COMPARATOR = new Comparator<Object>() {
        public int compare(final Object o1, final Object o2) {
            return o1.toString().compareTo(o2.toString());
        }
    };

    /**
     * This is comparator such that it also determines if the string has numbers leading the string....if it does have
     * numbers, it sorts using the numbers also
     * */
    public static final Comparator<Object> LEXICAL_NUMS_COMPARATOR = new Comparator<Object>() {
        public int compare(final Object o1, final Object o2) {
            final String s1 = (String) o1;
            // this is the int value if there are numbers leading s1....initilaize to -1
            int b1 = -1;
            final String s2 = (String) o2;
            // this is the int value if there are numbers leading s2....initilaize to -1
            int b2 = -1;
            ArrayList<Character> numChars = new ArrayList<Character>();
            for (int i = 0; i < s1.length(); i++) {
                final char c = s1.charAt(i);
                if (Character.isDigit(c)) {
                    numChars.add(new Character(c));
                } else {
                    break;
                }
            }
            if (numChars.size() > 0) {
                final char data[] = new char[numChars.size()];
                for (int i = 0; i < numChars.size(); i++) {
                    data[i] = (numChars.get(i)).charValue();
                }
                final Integer int1 = new Integer(new String(data));
                b1 = int1.intValue();
            }
            numChars = new ArrayList<Character>();
            for (int i = 0; i < s2.length(); i++) {
                final char c = s2.charAt(i);
                if (Character.isDigit(c)) {
                    numChars.add(new Character(c));
                } else {
                    break;
                }
            }
            if (numChars.size() > 0) {
                final char data[] = new char[numChars.size()];
                for (int i = 0; i < numChars.size(); i++) {
                    data[i] = (numChars.get(i)).charValue();
                }
                final Integer int2 = new Integer(new String(data));
                b2 = int2.intValue();
            }
            // this means no numbers in either strings....use string compare
            if (b1 == -1 && b2 == -1) {
                return o1.toString().compareTo(o2.toString());
            }
            // this means s1 has numbers leading it and s2 has NO numbers leading it
            if (b1 != -1 && b2 == -1) {
                return -1;
            }
            // this means s1 has NO numbers leading it and s2 has numbers leading it
            if (b1 == -1 && b2 != -1) {
                return 1;
            }
            // this means they both have numbers leading it
            if (b1 != -1 && b2 != -1) {
                if (b1 < b2) {
                    return -1;
                } else if (b1 > b2) {
                    return 1;
                } else {
                    return o1.toString().compareTo(o2.toString());
                }
            }
            // this should never happen
            return -1;
        }

    };

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected TableModel tableModel;

    @SuppressWarnings("unchecked")
    private final Map<Class,Comparator> columnComparators = new HashMap<Class,Comparator>();

    /** DOCUMENT ME! */
    private int[] modelToView;

    /** DOCUMENT ME! */
    private final MouseListener mouseListener;

    /** DOCUMENT ME! */
    private final List<Directive> sortingColumns = new ArrayList<Directive>();

    /** DOCUMENT ME! */
    private JTableHeader tableHeader;

    /** DOCUMENT ME! */
    private final TableModelListener tableModelListener;

    /** DOCUMENT ME! */
    private Row[] viewToModel;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new TableSorter object.
     */
    public TableSorter() {
        this.mouseListener = new MouseHandler();
        this.tableModelListener = new TableModelHandler();
    }

    /**
     * Creates a new TableSorter object.
     * 
     * @param tableModel DOCUMENT ME!
     */
    public TableSorter(final TableModel tableModel) {
        this();
        setTableModel(tableModel);
    }

    /**
     * Creates a new TableSorter object.
     * 
     * @param tableModel DOCUMENT ME!
     * @param tableHeader DOCUMENT ME!
     */
    public TableSorter(final TableModel tableModel, final JTableHeader tableHeader) {
        this();
        setTableHeader(tableHeader);
        setTableModel(tableModel);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     * 
     * @param column DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    @SuppressWarnings("unchecked")
    public Class getColumnClass(final int column) {
        return tableModel.getColumnClass(column);
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public int getColumnCount() {
        return (tableModel == null) ? 0 : tableModel.getColumnCount();
    }

    /**
     * DOCUMENT ME!
     * 
     * @param column DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public String getColumnName(final int column) {
        return tableModel.getColumnName(column);
    }

    /**
     * TableModel interface methods.
     * 
     * @return DOCUMENT ME!
     */
    public int getRowCount() {
        return (tableModel == null) ? 0 : tableModel.getRowCount();
    }

    /**
     * DOCUMENT ME!
     * 
     * @param column DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public int getSortingStatus(final int column) {
        return getDirective(column).direction;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public JTableHeader getTableHeader() {
        return tableHeader;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public TableModel getTableModel() {
        return tableModel;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param row DOCUMENT ME!
     * @param column DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public Object getValueAt(final int row, final int column) {
        return tableModel.getValueAt(modelIndex(row), column);
    }

    /**
     * DOCUMENT ME!
     * 
     * @param row DOCUMENT ME!
     * @param column DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public boolean isCellEditable(final int row, final int column) {
        return tableModel.isCellEditable(modelIndex(row), column);
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public boolean isSorting() {
        return sortingColumns.size() != 0;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param viewIndex DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public int modelIndex(final int viewIndex) {

        if (getRowCount() == 1) {
            return 0;
        }

        return getViewToModel()[viewIndex].modelIndex;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param type DOCUMENT ME!
     * @param comparator DOCUMENT ME!
     */
    @SuppressWarnings("unchecked")
    public void setColumnComparator(final Class type, final Comparator comparator) {

        if (comparator == null) {
            columnComparators.remove(type);
        } else {
            columnComparators.put(type, comparator);
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @param column DOCUMENT ME!
     * @param status DOCUMENT ME!
     */
    public void setSortingStatus(final int column, final int status) {
        final Directive directive = getDirective(column);

        if (directive != TableSorter.EMPTY_DIRECTIVE) {
            sortingColumns.remove(directive);
        }

        if (status != TableSorter.NOT_SORTED) {
            sortingColumns.add(new Directive(column, status));
        }

        sortingStatusChanged();
    }

    /**
     * DOCUMENT ME!
     * 
     * @param tableHeader DOCUMENT ME!
     */
    public void setTableHeader(final JTableHeader tableHeader) {

        if (this.tableHeader != null) {
            this.tableHeader.removeMouseListener(mouseListener);

            final TableCellRenderer defaultRenderer = this.tableHeader.getDefaultRenderer();

            if (defaultRenderer instanceof SortableHeaderRenderer) {
                this.tableHeader.setDefaultRenderer( ((SortableHeaderRenderer) defaultRenderer).tableCellRenderer);
            }
        }

        this.tableHeader = tableHeader;

        if (this.tableHeader != null) {
            this.tableHeader.addMouseListener(mouseListener);
            this.tableHeader.setDefaultRenderer(new SortableHeaderRenderer(this.tableHeader.getDefaultRenderer()));
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @param tableModel DOCUMENT ME!
     */
    public void setTableModel(final TableModel tableModel) {

        if (this.tableModel != null) {
            this.tableModel.removeTableModelListener(tableModelListener);
        }

        this.tableModel = tableModel;

        if (this.tableModel != null) {
            this.tableModel.addTableModelListener(tableModelListener);
        }

        clearSortingState();
        fireTableStructureChanged();
    }

    /**
     * DOCUMENT ME!
     * 
     * @param aValue DOCUMENT ME!
     * @param row DOCUMENT ME!
     * @param column DOCUMENT ME!
     */
    public void setValueAt(final Object aValue, final int row, final int column) {
        tableModel.setValueAt(aValue, modelIndex(row), column);
    }

    /**
     * Gets an appropriate comparator for sorting the particular column
     * 
     * @param column The column to sort by from <code>tableModel</code>
     * 
     * @return A comparator for a given column
     */
    @SuppressWarnings("unchecked")
    protected Comparator<Object> getComparator(final int column) {
        final Class columnType = tableModel.getColumnClass(column);

        final Comparator<Object> comparator = columnComparators.get(columnType);

        if (comparator != null) {
            return comparator;
        }

        if (Comparable.class.isAssignableFrom(columnType)) {
            return TableSorter.COMPARABLE_COMPARATOR;
        }

        return TableSorter.LEXICAL_COMPARATOR;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param column DOCUMENT ME!
     * @param size DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    protected Icon getHeaderRendererIcon(final int column, final int size) {
        final Directive directive = getDirective(column);

        if (directive == TableSorter.EMPTY_DIRECTIVE) {
            return null;
        }

        return new Arrow(directive.direction == TableSorter.DESCENDING, size, sortingColumns.indexOf(directive));
    }

    /**
     * DOCUMENT ME!
     */
    private void cancelSorting() {
        sortingColumns.clear();
        sortingStatusChanged();
    }

    /**
     * DOCUMENT ME!
     */
    private void clearSortingState() {
        viewToModel = null;
        modelToView = null;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param column DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    private Directive getDirective(final int column) {

        for (int i = 0; i < sortingColumns.size(); i++) {
            final Directive directive = (Directive) sortingColumns.get(i);

            if (directive.column == column) {
                return directive;
            }
        }

        return TableSorter.EMPTY_DIRECTIVE;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    private int[] getModelToView() {

        if (modelToView == null) {
            final int n = getViewToModel().length;
            modelToView = new int[n];

            for (int i = 0; i < n; i++) {
                modelToView[modelIndex(i)] = i;
            }
        }

        return modelToView;
    }

    /**
     * DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    private Row[] getViewToModel() {

        if (viewToModel == null) {
            final int tableModelRowCount = tableModel.getRowCount();
            viewToModel = new Row[tableModelRowCount];

            for (int row = 0; row < tableModelRowCount; row++) {
                viewToModel[row] = new Row(row);
            }

            if (isSorting()) {
                Arrays.sort(viewToModel);
            }
        }

        return viewToModel;
    }

    /**
     * DOCUMENT ME!
     */
    private void sortingStatusChanged() {
        clearSortingState();
        fireTableDataChanged();

        if (tableHeader != null) {
            tableHeader.repaint();
        }
    }

    // ~ Inner Classes
    // --------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    private static class Arrow implements Icon {

        /** DOCUMENT ME! */
        private final boolean descending;

        /** DOCUMENT ME! */
        private final int priority;

        /** DOCUMENT ME! */
        private final int size;

        /**
         * Creates a new Arrow object.
         * 
         * @param descending DOCUMENT ME!
         * @param size DOCUMENT ME!
         * @param priority DOCUMENT ME!
         */
        public Arrow(final boolean descending, final int size, final int priority) {
            this.descending = descending;
            this.size = size;
            this.priority = priority;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int getIconHeight() {
            return size;
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int getIconWidth() {
            return size;
        }

        /**
         * DOCUMENT ME!
         * 
         * @param c DOCUMENT ME!
         * @param g DOCUMENT ME!
         * @param x DOCUMENT ME!
         * @param y DOCUMENT ME!
         */
        public void paintIcon(final Component c, final Graphics g, final int x, int y) {
            final Color color = (c == null) ? Color.GRAY : c.getBackground();

            // In a compound sort, make each succesive triangle 20%
            // smaller than the previous one.
            final int dx = (int) (size / 2 * Math.pow(0.8, priority));
            final int dy = descending ? dx : -dx;

            // Align icon (roughly) with font baseline.
            y = y + (5 * size / 6) + (descending ? -dy : 0);

            final int shift = descending ? 1 : -1;
            g.translate(x, y);

            // Right diagonal.
            g.setColor(color.darker());
            g.drawLine(dx / 2, dy, 0, 0);
            g.drawLine(dx / 2, dy + shift, 0, shift);

            // Left diagonal.
            g.setColor(color.brighter());
            g.drawLine(dx / 2, dy, dx, 0);
            g.drawLine(dx / 2, dy + shift, dx, shift);

            // Horizontal line.
            if (descending) {
                g.setColor(color.darker().darker());
            } else {
                g.setColor(color.brighter().brighter());
            }

            g.drawLine(dx, 0, 0, 0);

            g.setColor(color);
            g.translate( -x, -y);
        }
    }

    /**
     * DOCUMENT ME!
     */
    private static class Directive {

        /** DOCUMENT ME! */
        private final int column;

        /** DOCUMENT ME! */
        private final int direction;

        /**
         * Creates a new Directive object.
         * 
         * @param column DOCUMENT ME!
         * @param direction DOCUMENT ME!
         */
        public Directive(final int column, final int direction) {
            this.column = column;
            this.direction = direction;
        }
    }

    /**
     * DOCUMENT ME!
     */
    private class MouseHandler extends MouseAdapter {

        /**
         * DOCUMENT ME!
         * 
         * @param e DOCUMENT ME!
         */
        public void mouseClicked(final MouseEvent e) {

            if (e.getModifiers() == InputEvent.BUTTON1_MASK) {
                final JTableHeader h = (JTableHeader) e.getSource();
                final TableColumnModel columnModel = h.getColumnModel();
                final int viewColumn = columnModel.getColumnIndexAtX(e.getX());
                final int column = columnModel.getColumn(viewColumn).getModelIndex();

                if (column != -1) {
                    int status = getSortingStatus(column);

                    if ( !e.isControlDown()) {
                        cancelSorting();
                    }

                    // Cycle the sorting states through {NOT_SORTED, ASCENDING, DESCENDING} or
                    // {NOT_SORTED, DESCENDING, ASCENDING} depending on whether shift is pressed.
                    // June 7, 2005 NOT_SORTED removed per Matt's orders
                    if (status == TableSorter.DESCENDING) {
                        status = TableSorter.ASCENDING;
                    } else {
                        status = TableSorter.DESCENDING;
                    }

                    // June 7, 2005 NOT_SORTED removed per Matt's orders. Uncomment the folowing lines
                    // and delete the if-else block above to re-enable the NOT_SORTED feature
                    // status = status + (e.isShiftDown() ? -1 : 1);
                    // status = (status + 4) % 3 - 1; // signed mod, returning {-1, 0, 1}

                    setSortingStatus(column, status);
                }
            }
        }
    }

    /**
     * Helper classes.
     */
    private class Row implements Comparable<Object> {

        /** DOCUMENT ME! */
        private final int modelIndex;

        /**
         * Creates a new Row object.
         * 
         * @param index DOCUMENT ME!
         */
        public Row(final int index) {
            this.modelIndex = index;
        }

        /**
         * DOCUMENT ME!
         * 
         * @param o DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public int compareTo(final Object o) {
            final int row1 = modelIndex;
            final int row2 = ((Row) o).modelIndex;

            for (final Iterator<Directive> it = sortingColumns.iterator(); it.hasNext();) {
                final Directive directive = it.next();
                final int column = directive.column;
                final Object o1 = tableModel.getValueAt(row1, column);
                final Object o2 = tableModel.getValueAt(row2, column);

                int comparison = 0;

                // Define null less than everything, except null.
                if ( (o1 == null) && (o2 == null)) {
                    comparison = 0;
                } else if (o1 == null) {
                    comparison = -1;
                } else if (o2 == null) {
                    comparison = 1;
                } else {
                    comparison = getComparator(column).compare(o1, o2);
                }

                if (comparison != 0) {
                    return (directive.direction == TableSorter.DESCENDING) ? -comparison : comparison;
                }
            }

            return 0;
        }
    }

    /**
     * DOCUMENT ME!
     */
    private class SortableHeaderRenderer implements TableCellRenderer {

        /** DOCUMENT ME! */
        private final TableCellRenderer tableCellRenderer;

        /**
         * Creates a new SortableHeaderRenderer object.
         * 
         * @param tableCellRenderer DOCUMENT ME!
         */
        public SortableHeaderRenderer(final TableCellRenderer tableCellRenderer) {
            this.tableCellRenderer = tableCellRenderer;
        }

        /**
         * DOCUMENT ME!
         * 
         * @param table DOCUMENT ME!
         * @param value DOCUMENT ME!
         * @param isSelected DOCUMENT ME!
         * @param hasFocus DOCUMENT ME!
         * @param row DOCUMENT ME!
         * @param column DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public Component getTableCellRendererComponent(final JTable table, final Object value,
                final boolean isSelected, final boolean hasFocus, final int row, final int column) {
            final Component c = tableCellRenderer.getTableCellRendererComponent(table, value, isSelected, hasFocus,
                    row, column);

            if (c instanceof JLabel) {
                final JLabel l = (JLabel) c;
                l.setHorizontalTextPosition(SwingConstants.LEFT);

                final int modelColumn = table.convertColumnIndexToModel(column);
                l.setIcon(getHeaderRendererIcon(modelColumn, l.getFont().getSize()));
            }

            return c;
        }
    }

    /**
     * DOCUMENT ME!
     */
    private class TableModelHandler implements TableModelListener {

        /**
         * DOCUMENT ME!
         * 
         * @param e DOCUMENT ME!
         */
        public void tableChanged(final TableModelEvent e) {

            // If we're not sorting by anything, just pass the event along.
            if ( !isSorting()) {
                clearSortingState();
                fireTableChanged(e);

                return;
            }

            // If the table structure has changed, cancel the sorting; the
            // sorting columns may have been either moved or deleted from
            // the model.
            if (e.getFirstRow() == TableModelEvent.HEADER_ROW) {
                cancelSorting();
                fireTableChanged(e);

                return;
            }

            // We can map a cell event through to the view without widening
            // when the following conditions apply:
            //
            // a) all the changes are on one row (e.getFirstRow() == e.getLastRow()) and,
            // b) all the changes are in one column (column != TableModelEvent.ALL_COLUMNS) and,
            // c) we are not sorting on that column (getSortingStatus(column) == NOT_SORTED) and,
            // d) a reverse lookup will not trigger a sort (modelToView != null)
            //
            // Note: INSERT and DELETE events fail this test as they have column == ALL_COLUMNS.
            //
            // The last check, for (modelToView != null) is to see if modelToView
            // is already allocated. If we don't do this check; sorting can become
            // a performance bottleneck for applications where cells
            // change rapidly in different parts of the table. If cells
            // change alternately in the sorting column and then outside of
            // it this class can end up re-sorting on alternate cell updates -
            // which can be a performance problem for large tables. The last
            // clause avoids this problem.
            final int column = e.getColumn();

            if ( (e.getFirstRow() == e.getLastRow()) && (column != TableModelEvent.ALL_COLUMNS)
                    && (getSortingStatus(column) == TableSorter.NOT_SORTED) && (modelToView != null)) {
                final int viewIndex = getModelToView()[e.getFirstRow()];
                fireTableChanged(new TableModelEvent(TableSorter.this, viewIndex, viewIndex, column, e.getType()));

                return;
            }

            // Something has happened to the data that may have invalidated the row order.
            clearSortingState();
            fireTableDataChanged();

            return;
        }
    }
}
