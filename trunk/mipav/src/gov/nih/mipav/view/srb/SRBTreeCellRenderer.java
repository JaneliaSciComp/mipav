package gov.nih.mipav.view.srb;


import edu.sdsc.grid.io.*;

import java.awt.*;
import javax.swing.*;
import javax.swing.plaf.*;
import javax.swing.plaf.basic.*;
import javax.swing.tree.*;


/**
 * Displays an entry in a tree. <code>DefaultTreeCellRenderer</code> is not opaque and unless you subclass paint you
 * should not change this. See <a href="http://java.sun.com/docs/books/tutorial/uiswing/components/tree.html">How to Use
 * Trees</a> in <em>The Java Tutorial</em> for examples of customizing node display using this class.
 *
 * <p><strong><a name="override">Implementation Note:</a></strong> This class overrides <code>invalidate</code>, <code>
 * validate</code>, <code>revalidate</code>, <code>repaint</code>, and <code>firePropertyChange</code> solely to improve
 * performance. If not overridden, these frequently called methods would execute code paths that are unnecessary for the
 * default tree cell renderer. If you write your own renderer, take care to weigh the benefits and drawbacks of
 * overriding these methods.</p>
 *
 * <p><strong>Warning:</strong> Serialized objects of this class will not be compatible with future Swing releases. The
 * current serialization support is appropriate for short term storage or RMI between applications running the same
 * version of Swing. As of 1.4, support for long term storage of all JavaBeans<sup><font size="-2">TM</font></sup> has
 * been added to the <code>java.beans</code> package. Please see {@link java.beans.XMLEncoder}.</p>
 *
 * @version  1.51 01/23/04
 * @author   Rob Davis
 * @author   Ray Ryan
 * @author   Scott Violet
 */
public class SRBTreeCellRenderer extends JLabel implements TreeCellRenderer {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -8881545568255157801L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Color to use for the background when the node isn't selected. */
    protected Color backgroundNonSelectionColor;

    /** Color to use for the background when a node is selected. */
    protected Color backgroundSelectionColor;

    /** Color to use for the focus indicator when the node has focus. */
    protected Color borderSelectionColor;

    // Icons
    /** Icon used to show non-leaf nodes that aren't expanded. */
    protected transient Icon closedIcon;

    /** True if has focus. */
    protected boolean hasFocus;

    /** Icon used to show leaf nodes. */
    protected transient Icon leafIcon;

    /** Icon used to show non-leaf nodes that are expanded. */
    protected transient Icon openIcon;

    /** Is the value currently selected. */
    protected boolean selected;

    /** Color to use for the foreground for non-selected nodes. */
    protected Color textNonSelectionColor;

    // Colors
    /** Color to use for the foreground for selected nodes. */
    protected Color textSelectionColor;

    /** If true, a dashed line is drawn as the focus indicator. */
    private boolean drawDashedFocusIndicator;

    /** True if draws focus border around icon as well. */
    private boolean drawsFocusBorderAroundIcon;

    /** Color to draw the focus indicator in, determined from the background. color. */
    private Color focusBGColor;

    /** Last tree the renderer was painted in. */
    private JTree tree;

    // If drawDashedFocusIndicator is true, the following are used.
    /** Background color of the tree. */
    private Color treeBGColor;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Returns a new instance of SRBTreeCellRenderer. Alignment is set to left aligned. Icons and text color are
     * determined from the UIManager.
     */
    public SRBTreeCellRenderer() {
        setHorizontalAlignment(JLabel.LEFT);

        setLeafIcon(UIManager.getIcon("Tree.leafIcon"));
        setClosedIcon(UIManager.getIcon("Tree.closedIcon"));
        setOpenIcon(UIManager.getIcon("Tree.openIcon"));

        setTextSelectionColor(UIManager.getColor("Tree.selectionForeground"));
        setTextNonSelectionColor(UIManager.getColor("Tree.textForeground"));
        setBackgroundSelectionColor(UIManager.getColor("Tree.selectionBackground"));
        setBackgroundNonSelectionColor(UIManager.getColor("Tree.textBackground"));
        setBorderSelectionColor(UIManager.getColor("Tree.selectionBorderColor"));

        Object value = UIManager.get("Tree.drawsFocusBorderAroundIcon");
        drawsFocusBorderAroundIcon = ((value != null) && ((Boolean) value).booleanValue());
        value = UIManager.get("Tree.drawDashedFocusIndicator");
        drawDashedFocusIndicator = ((value != null) && ((Boolean) value).booleanValue());
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Overridden for performance reasons. See the <a href="#override">Implementation Note</a> for more information.
     *
     * @param  propertyName  DOCUMENT ME!
     * @param  oldValue      DOCUMENT ME!
     * @param  newValue      DOCUMENT ME!
     */
    public void firePropertyChange(String propertyName, byte oldValue, byte newValue) { }

    /**
     * Overridden for performance reasons. See the <a href="#override">Implementation Note</a> for more information.
     *
     * @param  propertyName  DOCUMENT ME!
     * @param  oldValue      DOCUMENT ME!
     * @param  newValue      DOCUMENT ME!
     */
    public void firePropertyChange(String propertyName, char oldValue, char newValue) { }

    /**
     * Overridden for performance reasons. See the <a href="#override">Implementation Note</a> for more information.
     *
     * @param  propertyName  DOCUMENT ME!
     * @param  oldValue      DOCUMENT ME!
     * @param  newValue      DOCUMENT ME!
     */
    public void firePropertyChange(String propertyName, short oldValue, short newValue) { }

    /**
     * Overridden for performance reasons. See the <a href="#override">Implementation Note</a> for more information.
     *
     * @param  propertyName  DOCUMENT ME!
     * @param  oldValue      DOCUMENT ME!
     * @param  newValue      DOCUMENT ME!
     */
    public void firePropertyChange(String propertyName, int oldValue, int newValue) { }

    /**
     * Overridden for performance reasons. See the <a href="#override">Implementation Note</a> for more information.
     *
     * @param  propertyName  DOCUMENT ME!
     * @param  oldValue      DOCUMENT ME!
     * @param  newValue      DOCUMENT ME!
     */
    public void firePropertyChange(String propertyName, long oldValue, long newValue) { }

    /**
     * Overridden for performance reasons. See the <a href="#override">Implementation Note</a> for more information.
     *
     * @param  propertyName  DOCUMENT ME!
     * @param  oldValue      DOCUMENT ME!
     * @param  newValue      DOCUMENT ME!
     */
    public void firePropertyChange(String propertyName, float oldValue, float newValue) { }

    /**
     * Overridden for performance reasons. See the <a href="#override">Implementation Note</a> for more information.
     *
     * @param  propertyName  DOCUMENT ME!
     * @param  oldValue      DOCUMENT ME!
     * @param  newValue      DOCUMENT ME!
     */
    public void firePropertyChange(String propertyName, double oldValue, double newValue) { }

    /**
     * Overridden for performance reasons. See the <a href="#override">Implementation Note</a> for more information.
     *
     * @param  propertyName  DOCUMENT ME!
     * @param  oldValue      DOCUMENT ME!
     * @param  newValue      DOCUMENT ME!
     */
    public void firePropertyChange(String propertyName, boolean oldValue, boolean newValue) { }

    /**
     * Returns the background color to be used for non selected nodes.
     *
     * @return  DOCUMENT ME!
     */
    public Color getBackgroundNonSelectionColor() {
        return backgroundNonSelectionColor;
    }

    /**
     * Returns the color to use for the background if node is selected.
     *
     * @return  DOCUMENT ME!
     */
    public Color getBackgroundSelectionColor() {
        return backgroundSelectionColor;
    }

    /**
     * Returns the color the border is drawn.
     *
     * @return  DOCUMENT ME!
     */
    public Color getBorderSelectionColor() {
        return borderSelectionColor;
    }

    /**
     * Returns the icon used to represent non-leaf nodes that are not expanded.
     *
     * @return  DOCUMENT ME!
     */
    public Icon getClosedIcon() {
        return closedIcon;
    }

    /**
     * Returns the default icon, for the current laf, that is used to represent non-leaf nodes that are not expanded.
     *
     * @return  DOCUMENT ME!
     */
    public Icon getDefaultClosedIcon() {
        return UIManager.getIcon("Tree.closedIcon");
    }

    /**
     * Returns the default icon, for the current laf, that is used to represent leaf nodes.
     *
     * @return  DOCUMENT ME!
     */
    public Icon getDefaultLeafIcon() {
        return UIManager.getIcon("Tree.leafIcon");
    }

    /**
     * Returns the default icon, for the current laf, that is used to represent non-leaf nodes that are expanded.
     *
     * @return  DOCUMENT ME!
     */
    public Icon getDefaultOpenIcon() {
        return UIManager.getIcon("Tree.openIcon");
    }

    /**
     * Gets the font of this component.
     *
     * @return  this component's font; if a font has not been set for this component, the font of its parent is returned
     */
    public Font getFont() {
        Font font = super.getFont();

        if ((font == null) && (tree != null)) {

            // Strive to return a non-null value, otherwise the html support
            // will typically pick up the wrong font in certain situations.
            font = tree.getFont();
        }

        return font;
    }

    /**
     * Returns the icon used to represent leaf nodes.
     *
     * @return  DOCUMENT ME!
     */
    public Icon getLeafIcon() {
        return leafIcon;
    }

    /**
     * Returns the icon used to represent non-leaf nodes that are expanded.
     *
     * @return  DOCUMENT ME!
     */
    public Icon getOpenIcon() {
        return openIcon;
    }

    /**
     * Overrides <code>JComponent.getPreferredSize</code> to return slightly wider preferred size value.
     *
     * @return  DOCUMENT ME!
     */
    public Dimension getPreferredSize() {
        Dimension retDimension = super.getPreferredSize();

        if (retDimension != null) {
            retDimension = new Dimension(retDimension.width + 3, retDimension.height);
        }

        return retDimension;
    }

    /**
     * Returns the color the text is drawn with when the node isn't selected.
     *
     * @return  DOCUMENT ME!
     */
    public Color getTextNonSelectionColor() {
        return textNonSelectionColor;
    }

    /**
     * Returns the color the text is drawn with when the node is selected.
     *
     * @return  DOCUMENT ME!
     */
    public Color getTextSelectionColor() {
        return textSelectionColor;
    }

    /**
     * Configures the renderer based on the passed in components. The value is set from messaging the tree with <code>
     * convertValueToText</code>, which ultimately invokes <code>toString</code> on <code>value</code>. The foreground
     * color is set based on the selection and the icon is set based on on leaf and expanded.
     *
     * @param   tree      DOCUMENT ME!
     * @param   value     DOCUMENT ME!
     * @param   sel       DOCUMENT ME!
     * @param   expanded  DOCUMENT ME!
     * @param   leaf      DOCUMENT ME!
     * @param   row       DOCUMENT ME!
     * @param   hasFocus  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Component getTreeCellRendererComponent(JTree tree, Object value, boolean sel, boolean expanded, boolean leaf,
                                                  int row, boolean hasFocus) {
        String stringValue = convertValueToText(value, sel, expanded, leaf, row, hasFocus);

        this.tree = tree;
        this.hasFocus = hasFocus;
        setText(stringValue);

        if (sel) {
            setForeground(getTextSelectionColor());
        } else {
            setForeground(getTextNonSelectionColor());
        }

        // There needs to be a way to specify disabled icons.
        if (!tree.isEnabled()) {
            setEnabled(false);

            if (leaf) {
                setDisabledIcon(getLeafIcon());
            } else if (expanded) {
                setDisabledIcon(getOpenIcon());
            } else {
                setDisabledIcon(getClosedIcon());
            }
        } else {
            setEnabled(true);

            if (leaf) {
                setIcon(getLeafIcon());
            } else if (expanded) {
                setIcon(getOpenIcon());
            } else {
                setIcon(getClosedIcon());
            }
        }

        setComponentOrientation(tree.getComponentOrientation());

        selected = sel;

        return this;
    }

    /**
     * Overridden for performance reasons. See the <a href="#override">Implementation Note</a> for more information.
     *
     * @since  1.5
     */
    public void invalidate() { }

    /**
     * Paints the value. The background is filled based on selected.
     *
     * @param  g  DOCUMENT ME!
     */
    public void paint(Graphics g) {
        Color bColor;

        if (selected) {
            bColor = getBackgroundSelectionColor();
        } else {
            bColor = getBackgroundNonSelectionColor();

            if (bColor == null) {
                bColor = getBackground();
            }
        }

        int imageOffset = -1;

        if (bColor != null) {
            Icon currentI = getIcon();

            imageOffset = getLabelStart();
            g.setColor(bColor);

            if (getComponentOrientation().isLeftToRight()) {
                g.fillRect(imageOffset, 0, getWidth() - imageOffset, getHeight());
            } else {
                g.fillRect(0, 0, getWidth() - imageOffset, getHeight());
            }
        }

        if (hasFocus) {

            if (drawsFocusBorderAroundIcon) {
                imageOffset = 0;
            } else if (imageOffset == -1) {
                imageOffset = getLabelStart();
            }

            if (getComponentOrientation().isLeftToRight()) {
                paintFocus(g, imageOffset, 0, getWidth() - imageOffset, getHeight());
            } else {
                paintFocus(g, 0, 0, getWidth() - imageOffset, getHeight());
            }
        }

        super.paint(g);
    }

    /**
     * Overridden for performance reasons. See the <a href="#override">Implementation Note</a> for more information.
     *
     * @since  1.5
     */
    public void repaint() { }

    /**
     * Overridden for performance reasons. See the <a href="#override">Implementation Note</a> for more information.
     *
     * @param  r  DOCUMENT ME!
     */
    public void repaint(Rectangle r) { }

    /**
     * Overridden for performance reasons. See the <a href="#override">Implementation Note</a> for more information.
     *
     * @param  tm      DOCUMENT ME!
     * @param  x       DOCUMENT ME!
     * @param  y       DOCUMENT ME!
     * @param  width   DOCUMENT ME!
     * @param  height  DOCUMENT ME!
     */
    public void repaint(long tm, int x, int y, int width, int height) { }

    /**
     * Overridden for performance reasons. See the <a href="#override">Implementation Note</a> for more information.
     */
    public void revalidate() { }

    /**
     * Subclassed to map <code>ColorUIResource</code>s to null. If <code>color</code> is null, or a <code>
     * ColorUIResource</code>, this has the effect of letting the background color of the JTree show through. On the
     * other hand, if <code>color</code> is non-null, and not a <code>ColorUIResource</code>, the background becomes
     * <code>color</code>.
     *
     * @param  color  DOCUMENT ME!
     */
    public void setBackground(Color color) {

        if (color instanceof ColorUIResource) {
            color = null;
        }

        super.setBackground(color);
    }

    /**
     * Sets the background color to be used for non selected nodes.
     *
     * @param  newColor  DOCUMENT ME!
     */
    public void setBackgroundNonSelectionColor(Color newColor) {
        backgroundNonSelectionColor = newColor;
    }

    /**
     * Sets the color to use for the background if node is selected.
     *
     * @param  newColor  DOCUMENT ME!
     */
    public void setBackgroundSelectionColor(Color newColor) {
        backgroundSelectionColor = newColor;
    }

    /**
     * Sets the color to use for the border.
     *
     * @param  newColor  DOCUMENT ME!
     */
    public void setBorderSelectionColor(Color newColor) {
        borderSelectionColor = newColor;
    }

    /**
     * Sets the icon used to represent non-leaf nodes that are not expanded.
     *
     * @param  newIcon  DOCUMENT ME!
     */
    public void setClosedIcon(Icon newIcon) {
        closedIcon = newIcon;
    }

    /**
     * Subclassed to map <code>FontUIResource</code>s to null. If <code>font</code> is null, or a <code>
     * FontUIResource</code>, this has the effect of letting the font of the JTree show through. On the other hand, if
     * <code>font</code> is non-null, and not a <code>FontUIResource</code>, the font becomes <code>font</code>.
     *
     * @param  font  DOCUMENT ME!
     */
    public void setFont(Font font) {

        if (font instanceof FontUIResource) {
            font = null;
        }

        super.setFont(font);
    }

    /**
     * Sets the icon used to represent leaf nodes.
     *
     * @param  newIcon  DOCUMENT ME!
     */
    public void setLeafIcon(Icon newIcon) {
        leafIcon = newIcon;
    }

    /**
     * Sets the icon used to represent non-leaf nodes that are expanded.
     *
     * @param  newIcon  DOCUMENT ME!
     */
    public void setOpenIcon(Icon newIcon) {
        openIcon = newIcon;
    }

    /**
     * Sets the color the text is drawn with when the node isn't selected.
     *
     * @param  newColor  DOCUMENT ME!
     */
    public void setTextNonSelectionColor(Color newColor) {
        textNonSelectionColor = newColor;
    }

    /**
     * Sets the color the text is drawn with when the node is selected.
     *
     * @param  newColor  DOCUMENT ME!
     */
    public void setTextSelectionColor(Color newColor) {
        textSelectionColor = newColor;
    }

    /**
     * Overridden for performance reasons. See the <a href="#override">Implementation Note</a> for more information.
     */
    public void validate() { }

    /**
     * Overridden for performance reasons. See the <a href="#override">Implementation Note</a> for more information.
     *
     * @param  propertyName  DOCUMENT ME!
     * @param  oldValue      DOCUMENT ME!
     * @param  newValue      DOCUMENT ME!
     */
    protected void firePropertyChange(String propertyName, Object oldValue, Object newValue) {

        // Strings get interned...
        if (propertyName == "text") {
            super.firePropertyChange(propertyName, oldValue, newValue);
        }
    }

    /**
     * @see  JTree.convertValueToText.
     */
    private String convertValueToText(Object value, boolean selected, boolean expanded, boolean leaf, int row,
                                      boolean hasFocus) {

        if (value != null) {
            String aValue = null;

            if (value instanceof GeneralFile) {
                aValue = ((GeneralFile) value).getName();
            } else {
                aValue = value.toString();
            }

            if (aValue != null) {
                return aValue;
            }
        }

        return "";
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private int getLabelStart() {
        Icon currentI = getIcon();

        if ((currentI != null) && (getText() != null)) {
            return currentI.getIconWidth() + Math.max(0, getIconTextGap() - 1);
        }

        return 0;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  g  DOCUMENT ME!
     * @param  x  DOCUMENT ME!
     * @param  y  DOCUMENT ME!
     * @param  w  DOCUMENT ME!
     * @param  h  DOCUMENT ME!
     */
    private void paintFocus(Graphics g, int x, int y, int w, int h) {
        Color bsColor = getBorderSelectionColor();

        if ((bsColor != null) && (selected || !drawDashedFocusIndicator)) {
            g.setColor(bsColor);
            g.drawRect(x, y, w - 1, h - 1);
        }

        if (drawDashedFocusIndicator) {
            Color color;

            if (selected) {
                color = getBackgroundSelectionColor();
            } else {
                color = getBackgroundNonSelectionColor();

                if (color == null) {
                    color = getBackground();
                }
            }

            if (treeBGColor != color) {
                treeBGColor = color;
                focusBGColor = new Color(~color.getRGB());
            }

            g.setColor(focusBGColor);
            BasicGraphicsUtils.drawDashedRect(g, x, y, w, h);
        }
    }

}
