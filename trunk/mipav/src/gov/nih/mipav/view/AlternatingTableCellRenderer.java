package gov.nih.mipav.view;

import javax.swing.table.*;
import javax.swing.*;
import java.awt.*;

/**
 *
 * <p>Title: AlternatingTableCellRenderer</p>
 *
 * <p>Description: The purpose of this class is to override the DefaultTableCellRenderer so that
 * table cells are rendered with a background color which alternates between rows.</p>
 *
 * <p>Company: NIH MIPAV Project</p>
 *
 * @author lorsino
 * @version 1.0
 */
public class AlternatingTableCellRenderer extends DefaultTableCellRenderer implements
        TableCellRenderer
{
    private Color color1 = Color.white;
    private Color color2 = Color.white;

    private int alternateRowCount = 1;

    public AlternatingTableCellRenderer()
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
        Component component = super.getTableCellRendererComponent(table, value,
                isSelected, hasFocus, row, column);

        if ((row % (alternateRowCount * 2)) < alternateRowCount)
        {
            if (color1 != null)
            {
                component.setBackground(color1);
            }
        }
        else
        {
            if (color2 != null)
            {
                component.setBackground(color2);
            }
        }

        return component;
    }

    /**
     * This method sets the first alternating background color.
     *
     * @param color Color
     */
    public void setColor1(Color color)
    {
        this.color1 = color;
    }

    /**
     * This method sets the second alternating background color.
     * @param color Color
     */
    public void setColor2(Color color)
    {
        this.color2 = color;
    }

    /**
     * This method sets the alternating interval.
     *
     * @param rowCount int
     */
    public void setAlternateRowCount(int rowCount)
    {
        alternateRowCount = rowCount;
    }
}
