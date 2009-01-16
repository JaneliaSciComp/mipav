package gov.nih.mipav.view.xcede;

import java.awt.Component;
import java.awt.Color;

import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.table.TableCellRenderer;
import javax.swing.border.EmptyBorder;
import javax.swing.border.Border;
import javax.swing.UIManager;

public class MultiLineTableCellRenderer extends JTextArea implements
        TableCellRenderer {
    protected static Border noFocusBorder = new EmptyBorder(1, 1, 1, 1);
    
    private Color unselectedForeground; 
    private Color unselectedBackground; 

    /**
     * Creates a multiline table cell renderer.
     */
    public MultiLineTableCellRenderer() {
        super();
        setOpaque(true);
        setBorder(noFocusBorder);
        setEditable(true);
    }
    /**
     * Overrides <code>JComponent.setForeground</code> to assign
     * the unselected-foreground color to the specified color.
     * 
     * @param c set the foreground color to this value
     */
    public void setForeground(Color c) {
        super.setForeground(c); 
        unselectedForeground = c; 
    }
    
    /**
     * Overrides <code>JComponent.setBackground</code> to assign
     * the unselected-background color to the specified color.
     *
     * @param c set the background color to this value
     */
    public void setBackground(Color c) {
        super.setBackground(c); 
        unselectedBackground = c; 
    }

    /**
     * Notification from the <code>UIManager</code> that the look and feel
     * [L&F] has changed.
     * Replaces the current UI object with the latest version from the 
     * <code>UIManager</code>.
     *
     * @see JComponent#updateUI
     */
    public void updateUI() {
        super.updateUI(); 
        setForeground(null);
        setBackground(null);
    }

    public Component getTableCellRendererComponent(JTable table, Object value,
            boolean isSelected, boolean hasFocus, int row, int column) {
        if (isSelected) {
            super.setForeground(table.getSelectionForeground());
            super.setBackground(table.getSelectionBackground());
        } else {
            super.setForeground((unselectedForeground != null) ? unselectedForeground:table.getForeground());
            super.setBackground((unselectedBackground != null) ? unselectedBackground:table.getBackground());
        }

        setFont(table.getFont());

        if (hasFocus) {
            setBorder(UIManager.getBorder("Table.focusCellHighlightBorder"));
            if (!isSelected && table.isCellEditable(row, column)) {
                Color col;
                col = UIManager.getColor("Table.focusCellForeground");
                if (col != null) {
                    super.setForeground(col);
                }
                col = UIManager.getColor("Table.focusCellBackground");
                if (col != null) {
                    super.setBackground(col);
                }
            }
        } else {
            setBorder(noFocusBorder);
        }

        setValue(value);

        return this;
    }

    public boolean isOpaque() { 
        Color back = getBackground();
        Component p = getParent(); 
        if (p != null) { 
            p = p.getParent(); 
        }
        // p should now be the JTable.
        boolean colorMatch = (back != null) && (p != null) && 
            back.equals(p.getBackground()) && 
                p.isOpaque();
        return !colorMatch && super.isOpaque(); 
        }

    protected void setValue(Object value) {
        setText((value == null) ? "" : value.toString());
    }
}
