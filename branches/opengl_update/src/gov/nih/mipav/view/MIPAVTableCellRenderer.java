package gov.nih.mipav.view;


import java.awt.*;

import javax.swing.*;
import javax.swing.table.*;


/**
 * <p>Title: MIPAVTableCellRenderer</p>
 *
 * <p>Description: The purpose of this class is to override the DefaultTablCellRenderer so that both String objects and
 * Number objects are displayed right-justified inside of a table cell.</p>
 *
 * <p>Company: NIH MIPAV Project</p>
 *
 * @author   lorsino
 * @version  1.0
 */
public class MIPAVTableCellRenderer extends DefaultTableCellRenderer implements TableCellRenderer {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 2587142275980890594L;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new MIPAVTableCellRenderer object.
     */
    public MIPAVTableCellRenderer() {
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

        if (component instanceof JLabel) {
            JLabel label = (JLabel) component;
            label.setHorizontalAlignment(JLabel.RIGHT);

            return label;
        }

        return component;
    }
}
