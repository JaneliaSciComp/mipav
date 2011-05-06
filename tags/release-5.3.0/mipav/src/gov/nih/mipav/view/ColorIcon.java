package gov.nih.mipav.view;


import java.awt.*;
import gov.nih.mipav.model.structures.*;
import javax.swing.*;


/**
 * This is an icon which is a block of color with bounds specified in the constructor. It is from <U>Graphic Java
 * Mastering the JFC Volume II: Swing</U>, by David Geary, published by Sun Microsystems Press 1999, page 1085.
 *
 * @version  0.1 Aug 1, 1999
 * @author   David Geary
 * @see      ColorRenderer
 */

public class ColorIcon extends ModelSerialCloneable implements Icon {

	public static final Color TRANSPARENT = new Color(0, 0, 0, 0);
	public static final Color FOREGROUND = new Color(122, 138, 153, 255);
	
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private Color color;

    /** DOCUMENT ME! */
    private int w, h;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor Makes a color icon with the default color of gray and default width and height values of 50 and 15,
     * respectively.
     */
    public ColorIcon() {
        this(Color.gray, 50, 15);
    }

    /**
     * Constructor Makes a color icon with the specified color, width, and height.
     *
     * @param  color  color to set the icon to
     * @param  w      width to set the icon to
     * @param  h      height to set the icon to
     */
    public ColorIcon(Color color, int w, int h) {
        this.color = color;
        this.w = w;
        this.h = h;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * getColor - gets the color of the icon.
     *
     * @return  color of the icon
     */
    public Color getColor() {
        return color;
    }

    /**
     * getIconHeight - gets the height of the icon.
     *
     * @return  height of the icon
     */
    public int getIconHeight() {
        return h;
    }

    /**
     * getIconWidth - gets the width of the icon.
     *
     * @return  width of the icon
     */
    public int getIconWidth() {
        return w;
    }

    /**
     * paintIcon - paints the icon with a balck border.
     *
     * @param  c  component to paint
     * @param  g  graphics to paint in
     * @param  x  beginning x-coordinate
     * @param  y  beginning y-coordinate
     */
    public void paintIcon(Component c, Graphics g, int x, int y) {
    	if (color != TRANSPARENT) {
    		g.setColor(Color.black);
    		g.drawRect(x, y, w - 1, h - 1);
    	} else {
    		g.setColor(FOREGROUND);
    		g.drawRect(x, y, w - 1, h - 1);
    	}
        g.setColor(color);
        g.fillRect(x + 1, y + 1, w - 2, h - 2);
    }

    /**
     * setColor - sets the color of the icon.
     *
     * @param  color  color of the icon
     */
    public void setColor(Color color) {
        this.color = color;
    }

    /**
     * setIconHeight - sets the height of the icon.
     *
     * @param  h  height of the icon
     */
    public void setIconHeight(int h) {
        this.h = h;
    }

    /**
     * setIconWidth - sets the width of the icon.
     *
     * @param  w  width of the icon
     */
    public void setIconWidth(int w) {
        this.w = w;
    }

}
