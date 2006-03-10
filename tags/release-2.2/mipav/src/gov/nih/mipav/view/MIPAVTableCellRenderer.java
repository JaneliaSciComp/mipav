package gov.nih.mipav.view;

import javax.swing.table.*;
import javax.swing.*;
import java.awt.*;

/**
 *
 * <p>Title: MIPAVTableCellRenderer</p>
 *
 * <p>Description: The purpose of this class is to override the DefaultTablCellRenderer so that
 * both String objects and Number objects are displayed right-justified inside of a table cell.</p>
 *
 * <p>Company: NIH MIPAV Project</p>
 *
 * @author lorsino
 * @version 1.0
 */
public class MIPAVTableCellRenderer extends DefaultTableCellRenderer implements TableCellRenderer
{
    public MIPAVTableCellRenderer()
    {
        super();
    }

    public Component getTableCellRendererComponent(JTable table,
                                                   Object value,
                                                   boolean isSelected,
                                                   boolean hasFocus,
                                                   int row,
                                                   int column)
        {
            Component component = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);

            if (component instanceof JLabel)
            {
                JLabel label = (JLabel) component;
                label.setHorizontalAlignment(JLabel.RIGHT);
                return label;
            }

            return component;
        }
}
