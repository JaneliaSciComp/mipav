package gov.nih.mipav.view;

import java.awt.*;
import java.awt.event.*;

/**
 * An abstract base class for rubberbands.<p>
 *
 * Rubberbands do their rubberbanding inside of a Component,
 * which must be specified at construction time.<p>
 *
 * Subclasses are responsible for implementing
 * <em>void drawLast(Graphics g)</em> and
 * <em>void drawNext(Graphics g)</em>.
 *
 * drawLast() draws the appropriate geometric shape at the last
 * rubberband location, while drawNext() draws the appropriate
 * geometric shape at the next rubberband location.  All of the
 * underlying support for rubberbanding is taken care of here,
 * including handling XOR mode setting; extensions of Rubberband
 * need not concern themselves with anything but drawing the
 * last and next geometric shapes.  Extensions may get information
 * about where to draw their shape from getAnchor(), getStretched(),
 * and getLast(), and getEnd(), all of which return a Point.<p>
 *
 *
 * @see     RubberbandEllipse
 * @see     RubberbandLine
 * @see     RubberbandPolyline
 * @see     RubberbandRectangle
 */

abstract public class Rubberband extends Component implements
                                     MouseMotionListener, MouseListener {

    protected Point     anchorPt     = new Point(0,0);
    protected Point     stretchedPt  = new Point(0,0);
    protected Point     lastPt       = new Point(0,0);
    protected Point     endPt        = new Point(0,0);

    protected Component component = null;
    protected boolean   usableComponent = true;
    protected int       xS;
    protected int       yS;
    protected int       xMax;
    protected int       yMax;
    protected float     presetHue = -1.0f;
    private   boolean   firstStretch = true;
	private   boolean   active       = false;

    /**
    *  Default constructor, does nothing.
    */
	public Rubberband() {
	}

	/**
	*  Sets the parent component that the rubberband is for.
	*  @param c    Component that the rubberband is for
	*/
	public Rubberband(Component c) {
		setComponent(c);
	}

	/**
	*  Sets the presetHue
	*  @param presetHue
	*/
	public void setPresetHue(float presetHue) {
	    this.presetHue = presetHue;
	}

    /**
    *   Draws the appropriate geometric shape at the last
    *   rubberband location.
    */
    abstract public void drawLast(Graphics g);

    /**
    *   Draws the appropriate geometric shape at the next
    *   rubberband location.
    */
    abstract public void drawNext(Graphics g);

	/**
	*   Sets parent component image to null.
	*/
	public void dispose() {
	    component =  null;
	}

    /**
    *  Tests the point for validity within the image
    *  @param pt   point to test
    *  @return     boolean result of test
    */
    protected boolean testPoint(Point pt){
        int x,y;

        x  = pt.x;
        y  = pt.y;
        xS  = Math.round( x / (((ViewJComponentEditImage)(component)).getZoomX() * ((ViewJComponentEditImage)(component)).getResolutionX()));
        yS  = Math.round( y / (((ViewJComponentEditImage)(component)).getZoomY() * ((ViewJComponentEditImage)(component)).getResolutionY()));
        if (xS < 0 || xS >=   ((ViewJComponentEditImage)(component)).getImageA().getExtents()[0] ||
            yS < 0 || yS >=   ((ViewJComponentEditImage)(component)).getImageA().getExtents()[1]) {
            return false;
        }
        else {
            return true;
        }
    }


    /**
    *  Sets the rubberband to active or not active
    *  @param b    boolean indicating activity
    */
	public void setActive(boolean b) {
		active = b;
	}

	/**
	*  Sets the component to the parameter
	*  @param c       the component
	*/
	public void setComponent(Component c) {
		component = c;
		try {
		    xMax = Math.round(
		    (((ViewJComponentEditImage)(component)).getImageA().getExtents()[0]- 1) *
		    ((ViewJComponentEditImage)(component)).getResolutionX() *
		                     ((ViewJComponentEditImage)(component)).getZoomX());
		    yMax = Math.round(
		    (((ViewJComponentEditImage)(component)).getImageA().getExtents()[1]- 1) *
		    ((ViewJComponentEditImage)(component)).getResolutionY() *
		                 ((ViewJComponentEditImage)(component)).getZoomY());
		}
		catch (ClassCastException e) {
		    usableComponent = false;
		}
	}

	/**
	*  Returns a boolean indicating activity
	*  @return    boolean indicating activity
	*/
	public boolean isActive    () { return active;      }

	/**
	*  Returns the anchor point of the rubberband
	*  @return     the anchor point
	*/
    public Point   getAnchor   () { return anchorPt;    }

	/**
	*  Returns the stretched point of the rubberband
	*  @return        the stretched point
	*/
    public Point   getStretched() { return stretchedPt; }

	/**
	*  Returns the last point of the rubberband
	*  @return   the last point
	*/
    public Point   getLast     () { return lastPt;      }

	/**
	*  Returns the end point of the rubberband
	*  @return  the end point
	*/
    public Point   getEnd      () { return endPt;       }

    /**
    *  Anchors the rubberband to this point
    *  @param p  point to anchor the rubberband to
    */
    public void anchor(Point p) {
        firstStretch = true;
        if (p.x < 0) {
            p.x = 0;
        }
        if (p.y < 0) {
            p.y = 0;
        }
        if ((component != null) && (usableComponent)) {
            xMax = Math.round(
		    (((ViewJComponentEditImage)(component)).getImageA().getExtents()[0]- 1) *
		    ((ViewJComponentEditImage)(component)).getResolutionX() *
		                     ((ViewJComponentEditImage)(component)).getZoomX());
		    yMax = Math.round(
		    (((ViewJComponentEditImage)(component)).getImageA().getExtents()[1]- 1) *
		    ((ViewJComponentEditImage)(component)).getResolutionY() *
		                 ((ViewJComponentEditImage)(component)).getZoomY());
            if (p.x > xMax) {
                p.x = xMax;
            }
            if (p.y > yMax) {
                p.y = yMax;
            }
        }
        anchorPt.x = p.x;
        anchorPt.y = p.y;

        stretchedPt.x = lastPt.x = anchorPt.x;
        stretchedPt.y = lastPt.y = anchorPt.y;
    }

    /**
    *  Stretch the rubberband to this point
    *  @param p  point to stretch to
    */
    public void stretch(Point p) {
        lastPt.x      = stretchedPt.x;
        lastPt.y      = stretchedPt.y;

        if (p.x < 0) {
            p.x = 0;
        }
        if (p.y < 0) {
            p.y = 0;
        }
        if ((component != null) && (usableComponent)) {
            xMax = Math.round(
		    (((ViewJComponentEditImage)(component)).getImageA().getExtents()[0]- 1) *
		    ((ViewJComponentEditImage)(component)).getResolutionX() *
		                     ((ViewJComponentEditImage)(component)).getZoomX());
		    yMax = Math.round(
		    (((ViewJComponentEditImage)(component)).getImageA().getExtents()[1]- 1) *
		    ((ViewJComponentEditImage)(component)).getResolutionY() *
		                 ((ViewJComponentEditImage)(component)).getZoomY());
            if (p.x > xMax) {
                p.x = xMax;
            }
            if (p.y > yMax) {
                p.y = yMax;
            }
        }
        stretchedPt.x = p.x;
        stretchedPt.y = p.y;

        Graphics g = component.getGraphics();
        if(g != null) {

            g.setXORMode(Preferences.getVOIDrawColor());

            if(firstStretch == true) firstStretch = false;
            else                     drawLast(g);
            drawNext(g);
            g.setPaintMode();
			g.dispose();
        }
    }

    /**
    *  Sets the end point to this point
    *  @param p  point to set the end to
    */
    public void end(Point p) {
        if (p.x < 0) {
            p.x = 0;
        }
        if (p.y < 0) {
            p.y = 0;
        }
        if ((component != null) && (usableComponent)) {
            xMax = Math.round(
		    (((ViewJComponentEditImage)(component)).getImageA().getExtents()[0]- 1) *
		    ((ViewJComponentEditImage)(component)).getResolutionX() *
		                     ((ViewJComponentEditImage)(component)).getZoomX());
		    yMax = Math.round(
		    (((ViewJComponentEditImage)(component)).getImageA().getExtents()[1]- 1) *
		    ((ViewJComponentEditImage)(component)).getResolutionY() *
		                 ((ViewJComponentEditImage)(component)).getZoomY());
            if (p.x > xMax) {
                p.x = xMax;
            }
            if (p.y > yMax) {
                p.y = yMax;
            }
        }
        lastPt.x = endPt.x = p.x;
        lastPt.y = endPt.y = p.y;

        Graphics g = component.getGraphics();
        if(g != null) {
            //g.setXORMode(yellow);
            //drawLast(g);
            g.setColor(Preferences.getVOIDrawColor());
            g.setPaintMode();
            drawLast(g);
			g.dispose();
        }
    }

    /**
    *  Gets a rectangle that is the bounds of the rubberband
    *  @return     rectangle of the bounds
    */
    public Rectangle getBounds() {
      return new Rectangle(stretchedPt.x < anchorPt.x ?
                           stretchedPt.x : anchorPt.x,
                           stretchedPt.y < anchorPt.y ?
                           stretchedPt.y : anchorPt.y,
                           Math.abs(stretchedPt.x - anchorPt.x),
                           Math.abs(stretchedPt.y - anchorPt.y));
    }

    /**
    *  Gets the last bounds of the rubberband
    *  @return      rectangle of the bounds
    */
    public Rectangle lastBounds() {
      return new Rectangle(
                  lastPt.x < anchorPt.x ? lastPt.x : anchorPt.x,
                  lastPt.y < anchorPt.y ? lastPt.y : anchorPt.y,
                  Math.abs(lastPt.x - anchorPt.x),
                  Math.abs(lastPt.y - anchorPt.y));
    }

    /**
    *  Unchanged
    */
    public void mouseEntered(MouseEvent mouseEvent) {
    }

    /**
    *  Unchanged
    */
    public void mouseExited(MouseEvent mouseEvent) {
    }

    /**
    *  Gets the anchor point of the rubberband
    *  @param mouseEvent  event that triggered function
    */
    public void mousePressed(MouseEvent mouseEvent) {
        if(isActive()) {
            anchor(mouseEvent.getPoint());
	    }
    }

    /**
    *  Gets the end point of the rubberband
    *  @param mouseEvent  event that triggered function
    */
    public void mouseClicked(MouseEvent mouseEvent) {
        if(isActive())
		    end(mouseEvent.getPoint());
    }

    /**
    *  Gets the end point of the rubberband
    *  @param mouseEvent  event that triggered function
    */
    public void mouseReleased(MouseEvent mouseEvent) {
        if(isActive())
		    end(mouseEvent.getPoint());
    }

    /**
    *  Gets the stretch point of the rubberband
    *  @param mouseEvent  event that triggered function
    */
    public void mouseDragged(MouseEvent mouseEvent){
        if(isActive())
			stretch(mouseEvent.getPoint());
    }

    /**
    *  Unchanged
    */
    public void mouseMoved(MouseEvent mouseEvent){
    }

    /**
    *  Calculates the 2D euclidian distance between two points
    *  @param x1   first x coordinate
    *  @param x2   second x coordinate
    *  @param y1   first y coordinate
    *  @param y2   second y coordinate
    *  @return     returns the distance
    */
    protected final static double distance(int x1, int x2, int y1, int y2){
        return Math.sqrt((x2-x1)*(x2-x1) +  (y2- y1)*(y2-y1));
    }


}
