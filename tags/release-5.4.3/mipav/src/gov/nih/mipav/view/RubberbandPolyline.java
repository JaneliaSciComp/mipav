package gov.nih.mipav.view;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.structures.*;

import java.awt.*;
import java.awt.event.*;


/**
 * A Rubberband that does polylines and polygons.
 *
 * @version  1.0, 12/27/95
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      Rubberband
 */
public class RubberbandPolyline extends Rubberband {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -9153642132322433825L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private VOIContour contour = new VOIContour(false);

    /** DOCUMENT ME! */
    private boolean firstPoint = true;

    /** DOCUMENT ME! */
    private boolean mouseDragged = false;

    /** DOCUMENT ME! */
    private Point tmpPt = new Point(0, 0);

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs the object and adds mouse listeners.
     *
     * @param  component  component to add to
     */
    public RubberbandPolyline(Component component) {
        super(component);
        component.addMouseMotionListener(this);
        component.addMouseListener(this);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Draws a line based on the rubberband's last bounds.
     *
     * @param  graphics  graphics to draw in
     */
    public void drawLast(Graphics graphics) {
        graphics.drawLine(anchorPt.x, anchorPt.y, lastPt.x, lastPt.y);
    }

    /**
     * Draws a line based on the rubberband's bounds.
     *
     * @param  graphics  graphics to draw in
     */
    public void drawNext(Graphics graphics) {
        graphics.drawLine(anchorPt.x, anchorPt.y, stretchedPt.x, stretchedPt.y);
    }

    /**
     * Unchanged.
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseClicked(MouseEvent mouseEvent) { }

    /**
     * Makes a contour out of the curve drawn.
     *
     * @param  mouseEvent  event that triggered this function
     */
    public void mouseDragged(MouseEvent mouseEvent) {
        Point pt = mouseEvent.getPoint();

        if (pt.x < 0) {
            pt.x = 0;
        }

        if (pt.y < 0) {
            pt.y = 0;
        }

        if ((component != null) && (usableComponent)) {
            xMax = Math.round((((ViewJComponentEditImage) (component)).getImageA().getExtents()[0] - 1) *
                                  ((ViewJComponentEditImage) (component)).getResolutionX() *
                                  ((ViewJComponentEditImage) (component)).getZoomX());
            yMax = Math.round((((ViewJComponentEditImage) (component)).getImageA().getExtents()[1] - 1) *
                                  ((ViewJComponentEditImage) (component)).getResolutionY() *
                                  ((ViewJComponentEditImage) (component)).getZoomY());

            if (pt.x > xMax) {
                pt.x = xMax;
            }

            if (pt.y > yMax) {
                pt.y = yMax;
            }
        }

        mouseDragged = true;

        tmpPt.x = Math.round(getEnd().x /
                                 (((ViewJComponentEditImage) (component)).getZoomX() *
                                      ((ViewJComponentEditImage) (component)).getResolutionX()));
        tmpPt.y = Math.round(getEnd().y /
                                 (((ViewJComponentEditImage) (component)).getZoomY() *
                                      ((ViewJComponentEditImage) (component)).getResolutionY()));

        if (isActive() && testPoint(pt) && ((xS != tmpPt.x) || (yS != tmpPt.y))) {
            contour.addElement(xS, yS, ((ViewJComponentEditImage) (component)).getSlice());

            Graphics g = super.component.getGraphics();

            if (g == null) {
                return;
            }

            if (firstPoint) {
                anchor(pt);
                firstPoint = false;
            } else {
                g.setXORMode(Color.black);
                g.setColor(Preferences.getVOIDrawColor());
                g.drawRect((int) anchorPt.x - 3, (int) anchorPt.y - 3, 6, 6);
                g.drawRect(pt.x - 3, pt.y - 3, 6, 6);
                stretch(pt);
                end(pt);
                anchor(pt);
            }
        }
    }

    /**
     * Unchanged.
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseEntered(MouseEvent mouseEvent) { }

    /**
     * Unchanged.
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseExited(MouseEvent mouseEvent) { }

    /**
     * Stretches if the VOI is active.
     *
     * @param  mouseEvent  event that triggered this function
     */
    public void mouseMoved(MouseEvent mouseEvent) {
        Point pt = mouseEvent.getPoint();

        if (pt.x < 0) {
            pt.x = 0;
        }

        if (pt.y < 0) {
            pt.y = 0;
        }

        if ((component != null) && (usableComponent)) {
            xMax = Math.round((((ViewJComponentEditImage) (component)).getImageA().getExtents()[0] - 1) *
                                  ((ViewJComponentEditImage) (component)).getResolutionX() *
                                  ((ViewJComponentEditImage) (component)).getZoomX());
            yMax = Math.round((((ViewJComponentEditImage) (component)).getImageA().getExtents()[1] - 1) *
                                  ((ViewJComponentEditImage) (component)).getResolutionY() *
                                  ((ViewJComponentEditImage) (component)).getZoomY());

            if (pt.x > xMax) {
                pt.x = xMax;
            }

            if (pt.y > yMax) {
                pt.y = yMax;
            }
        }

        mouseDragged = false;

        if (isActive() && (firstPoint == false) && testPoint(pt)) {
            stretch(pt);
        }
    }

    /**
     * Unchanged.
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mousePressed(MouseEvent mouseEvent) { }

    /**
     * Makes an VOI out of the contours.
     *
     * @param  mouseEvent  event that triggered function
     */
    public void mouseReleased(MouseEvent mouseEvent) {
        VOI newVOI;
        Point pt = mouseEvent.getPoint();

        if (pt.x < 0) {
            pt.x = 0;
        }

        if (pt.y < 0) {
            pt.y = 0;
        }

        if ((component != null) && (usableComponent)) {
            xMax = Math.round((((ViewJComponentEditImage) (component)).getImageA().getExtents()[0] - 1) *
                                  ((ViewJComponentEditImage) (component)).getResolutionX() *
                                  ((ViewJComponentEditImage) (component)).getZoomX());
            yMax = Math.round((((ViewJComponentEditImage) (component)).getImageA().getExtents()[1] - 1) *
                                  ((ViewJComponentEditImage) (component)).getResolutionY() *
                                  ((ViewJComponentEditImage) (component)).getZoomY());

            if (pt.x > xMax) {
                pt.x = xMax;
            }

            if (pt.y > yMax) {
                pt.y = yMax;
            }
        }

        int xxS = Math.round(pt.x /
                                 (((ViewJComponentEditImage) (component)).getZoomX() *
                                      ((ViewJComponentEditImage) (component)).getResolutionX()));
        int yyS = Math.round(pt.y /
                                 (((ViewJComponentEditImage) (component)).getZoomY() *
                                      ((ViewJComponentEditImage) (component)).getResolutionY()));
        int index;
        int colorID;
        int i;
        ViewVOIVector VOIs;
        int nVOI;
        String name;
        boolean open;
        boolean done;

        // open - Polyline VOI
        if ((((ViewJComponentEditImage) (component)).getZoomX() >= 8.0f) && isActive() && (contour.size() > 2) &&
                (xxS == (int) ((Vector3f) (contour.lastElement())).X) &&
                (yyS == (int) ((Vector3f) (contour.lastElement())).Y) && (mouseDragged == false)) {
            open = true;
            done = true;
        }
        // User clicked back again on first point.  closed - Polygon VOI
        else if ((((ViewJComponentEditImage) (component)).getZoomX() >= 8.0f) && isActive() && (contour.size() > 2) &&
                     (xxS == (int) ((Vector3f) (contour.elementAt(0))).X) &&
                     (yyS == (int) ((Vector3f) (contour.elementAt(0))).Y)) {
            open = false;
            done = true;
        } else if ((((ViewJComponentEditImage) (component)).getZoomX() < 8.0f) && isActive() && (contour.size() > 2) &&
                       (distance(xxS, (int) ((Vector3f) (contour.lastElement())).X, yyS,
                                     (int) ((Vector3f) (contour.lastElement())).Y) < 3) && (mouseDragged == false)) {
            open = true;
            done = true;
        }
        // User clicked back again on first point.  closed - Polygon VOI
        else if ((((ViewJComponentEditImage) (component)).getZoomX() < 8.0f) &&
                     (((ViewJComponentEditImage) (component)).getZoomX() >= 1.0f) && isActive() &&
                     (contour.size() > 2) &&
                     (distance(xxS, (int) ((Vector3f) (contour.elementAt(0))).X, yyS,
                                   (int) ((Vector3f) (contour.elementAt(0))).Y) < 3)) {
            open = false;
            done = true;
        } else if ((((ViewJComponentEditImage) (component)).getZoomX() < 1.0f) && isActive() && (contour.size() > 2) &&
                       (distance(xxS, (int) ((Vector3f) (contour.elementAt(0))).X, yyS,
                                     (int) ((Vector3f) (contour.elementAt(0))).Y) <
                            (3 * (1.0 / ((ViewJComponentEditImage) (component)).getZoomX())))) {
            open = false;
            done = true;
        }

        // continue drawing
        else {
            open = false;
            done = false;
        }

        if (done) {
            contour.trimPoints(Preferences.getTrim(), Preferences.getTrimAdjacient());

            ModelImage image = ((ViewJComponentEditImage) (component)).getActiveImage();
            int constant;

            if (open == true) {
                constant = VOI.POLYLINE;
            } else {
                contour.setClosed(true);
                constant = VOI.CONTOUR;
            }

            if (((ViewJComponentEditImage) (component)).getVOIHandler().isNewVoiNeeded(constant)) {

                try {
                    VOIs = image.getVOIs();
                    index = VOIs.size();
                    colorID = 0;

                    if (image.getVOIs().size() > 0) {
                        colorID = ((VOI) (image.getVOIs().lastElement())).getID() + 1;
                    }

                    nVOI = VOIs.size();

                    if (open == true) {
                        name = "polyline" + (index + 1);
                    } else {
                        name = "polygon" + (index + 1);
                    }

                    int test;

                    do {
                        test = 0;

                        for (i = 0; i < nVOI; i++) {

                            if (name.equals(VOIs.VOIAt(i).getName())) {
                                index++;

                                if (open == true) {
                                    name = "polyline" + (index + 1);
                                } else {
                                    name = "polygon" + (index + 1);
                                }

                                test = 1;
                            }
                        }
                    } while (test == 1);

                    /*
                     * do { test = 0; for (i = 0; i < nVOI; i++) {     if (colorID ==((int)VOIs.VOIAt(i).getID())) {
                     *     colorID++;         test = 1;     } } } while (test == 1);
                     */

                    newVOI = new VOI((short) colorID, name, constant, presetHue);
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to form new polyline");
                    ((ViewJComponentEditImage) (component)).setCursorMode(ViewJComponentEditImage.DEFAULT);

                    return;
                }

                contour.setClosed(!open);
                newVOI.importCurve(contour);
                image.registerVOI(newVOI);
                image.notifyImageDisplayListeners();

                if (!(mouseEvent.isShiftDown() == true || Preferences.is(Preferences.PREF_CONTINUOUS_VOI_CONTOUR))) {
                    ((ViewJComponentEditImage) (component)).setCursorMode(ViewJComponentEditImage.DEFAULT);
                }

                ((ViewJComponentEditImage) (component)).getVOIHandler().setVOI_IDs(newVOI.getID(), newVOI.getUID());

                // setup for next time this class is used
                firstPoint = true;

                try {
                    contour = new VOIContour(false);
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to form new polyline");
                    ((ViewJComponentEditImage) (component)).setCursorMode(ViewJComponentEditImage.DEFAULT);

                    return;
                }
            } else {

                // get selected contour
                VOIs = image.getVOIs();
                nVOI = VOIs.size();

                for (i = 0; i < nVOI; i++) {

                    if (VOIs.VOIAt(i).getID() == ((ViewJComponentEditImage) (component)).getVOIHandler().getVOI_ID()) {

                        if (VOIs.VOIAt(i).getCurveType() == constant) {
                            // System.err.println("adding element");
                            // VOIs.VOIAt(i).getCurves()[((ViewJComponentEditImage)(component)).getSlice()].addElement(contour);

                            VOIs.VOIAt(i).importCurve(contour);
                        } else {
                            MipavUtil.displayError("Can't add this VOI to other VOI structure.");
                        }
                    }
                }

                image.notifyImageDisplayListeners();

                if (!(mouseEvent.isShiftDown() == true || Preferences.is(Preferences.PREF_CONTINUOUS_VOI_CONTOUR))) {
                    ((ViewJComponentEditImage) (component)).setCursorMode(ViewJComponentEditImage.DEFAULT);
                }


                // setup for next time this class is used
                firstPoint = true;

                try {
                    contour = new VOIContour(false);
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to form new polygon");
                    ((ViewJComponentEditImage) (component)).setCursorMode(ViewJComponentEditImage.DEFAULT);

                    return;
                }

                return;
            }

            return;
        }

        if (!open) {
            tmpPt.x = Math.round(getEnd().x /
                                     (((ViewJComponentEditImage) (component)).getZoomX() *
                                          ((ViewJComponentEditImage) (component)).getResolutionX()));
            tmpPt.y = Math.round(getEnd().y /
                                     (((ViewJComponentEditImage) (component)).getZoomY() *
                                          ((ViewJComponentEditImage) (component)).getResolutionY()));

            if (isActive() && testPoint(pt) && ((xS != tmpPt.x) || (yS != tmpPt.y))) {
                contour.addElement(xS, yS, ((ViewJComponentEditImage) (component)).getSlice());

                Graphics g = super.component.getGraphics();

                if (firstPoint) {
                    anchor(pt);
                    firstPoint = false;
                } else {
                    g.setXORMode(Color.black);
                    g.setColor(Preferences.getVOIDrawColor());
                    g.drawRect((int) anchorPt.x - 3, (int) anchorPt.y - 3, 6, 6);
                    g.drawRect(pt.x - 3, pt.y - 3, 6, 6);
                    end(pt);
                    anchor(pt);
                }
            }
        }
    }
}
