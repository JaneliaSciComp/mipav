package gov.nih.mipav.view;


import java.awt.*;

import javax.swing.*;
import javax.swing.table.*;


/**
 * <p>Title: AlternatingTableCellRenderer</p>
 *
 * <p>Description: The purpose of this class is to override the DefaultTableCellRenderer so that table cells are
 * rendered with a background color which alternates between rows.</p>
 *
 * <p>Company: NIH MIPAV Project</p>
 *
 * @author   lorsino
 * @version  1.0
 */
public class AlternatingTableCellRenderer extends DefaultTableCellRenderer implements TableCellRenderer {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 3028551564787955672L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int alternateRowCount = 1;

    /** DOCUMENT ME! */
    private Color color1 = Color.white;

    /** DOCUMENT ME! */
    private Color color2 = Color.white;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlternatingTableCellRenderer object.
     */
    public AlternatingTableCellRenderer() {
        super();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param   table       DOCUMENT ME!
     * @param   value       DOCUMENT ME!
     * @param   isSelected  DOCUMENT ME!
     * @param   hasFocus    DOCUMENT ME!
     * @param   row         DOCUMENT ME!
     * @param   column      DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus,
                                                   int row, int column) {
        Component component = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);

        if ((row % (alternateRowCount * 2)) < alternateRowCount) {

            if (color1 != null) {
                component.setBackground(color1);
            }
        } else {

            if (color2 != null) {
                component.setBackground(color2);
            }
        }

        return component;
    }

    /**
     * This method sets the alternating interval.
     *
     * @param  rowCount  int
     */
    public void setAlternateRowCount(int rowCount) {
        alternateRowCount = rowCount;
    }

    /**
     * This method sets the first alternating background color.
     *
     * @param  color  Color
     */
    public void setColor1(Color color) {
        this.color1 = color;
    }

    /**
     * This method sets the second alternating background color.
     *
     * @param  color  Color
     */
    public void setColor2(Color color) {
        this.color2 = color;
    }
}
