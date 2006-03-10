 package gov.nih.mipav.view;


 import java.awt.*;


/**
 * A Rubberband that does rectangles.
 *
 */

 public class RubberbandRectangle extends Rubberband {
    /**
    *  Constructor
    */
	public RubberbandRectangle() {
	}

    /**
    *
    *  @param component   component to add to
    */
    public RubberbandRectangle(Component component) {
        super(component);
        component.addMouseMotionListener(this);
        component.addMouseListener(this);
    }

    /**
    *  Draws a rectangle based on the rubberband's last bounds
    *  @param graphics   graphics to draw in
    */
    public void drawLast(Graphics graphics) {
        Rectangle rect = lastBounds();
        graphics.drawRect(rect.x, rect.y,
                          rect.width, rect.height);
    }

    /**
    *  Draws a rectangle based on the rubberband's bounds
    *  @param graphics   graphics to draw in
    */
    public void drawNext(Graphics graphics) {
        Rectangle rect = getBounds();
        graphics.drawRect(rect.x, rect.y, rect.width, rect.height);
    }

}
