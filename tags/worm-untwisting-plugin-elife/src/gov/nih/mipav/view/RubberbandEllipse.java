package gov.nih.mipav.view;

import WildMagic.LibFoundation.Mathematics.Vector2f;

import gov.nih.mipav.model.structures.*;

import java.awt.*;
import java.awt.event.*;


/**
 * A Rubberband that does ellipses.
 */

public class RubberbandEllipse extends Rubberband {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -3765430195677943807L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private final int endAngle = 360;

    /** DOCUMENT ME! */
    private final int startAngle = 0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs rubberbandEllipse.
     *
     * @param  component  component to add to
     */
    public RubberbandEllipse(Component component) {
        super(component);
        component.addMouseMotionListener(this);
        component.addMouseListener(this);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Draws an arc based on the rubberband's last bounds.
     *
     * @param  graphics  graphics to draw in
     */
    public void drawLast(Graphics graphics) {
        Rectangle rect = lastBounds();
        graphics.drawArc(rect.x, rect.y, rect.width, rect.height, startAngle, endAngle);
    }

    /**
     * Draws an arc based on the rubberband's bounds.
     *
     * @param  graphics  graphics to draw in
     */
    public void drawNext(Graphics graphics) {
        Rectangle rect = getBounds();
        graphics.drawArc(rect.x, rect.y, rect.width, rect.height, startAngle, endAngle);
    }


    /**
     * Gets the stretch point of the rubberband.
     *
     * @param  mouseEvent  event that triggered function
     */
    public void mouseDragged(MouseEvent mouseEvent) {

        if (isActive()) {

            // stretch(mouseEvent.getPoint());
            Point p = mouseEvent.getPoint();

            if (p.x < 0) {
                p.x = 0;
            }

            if (p.y < 0) {
                p.y = 0;
            }

            if ((component != null) && (usableComponent)) {
                xMax = Math.round((((ViewJComponentEditImage) (component)).getImageA().getExtents()[0] - 1) *
                                      ((ViewJComponentEditImage) (component)).getResolutionX() *
                                      ((ViewJComponentEditImage) (component)).getZoomX());
                yMax = Math.round((((ViewJComponentEditImage) (component)).getImageA().getExtents()[1] - 1) *
                                      ((ViewJComponentEditImage) (component)).getResolutionY() *
                                      ((ViewJComponentEditImage) (component)).getZoomY());

                if (p.x > xMax) {
                    p.x = xMax;
                }

                if (p.y > yMax) {
                    p.y = yMax;
                }
            }

            lastPt.x = stretchedPt.x;
            lastPt.y = stretchedPt.y;
            stretchedPt.x = p.x;
            stretchedPt.y = p.y;

            Rectangle rect;

            Graphics g = component.getGraphics();

            if (g != null) {

                g.setXORMode(Preferences.getVOIDrawColor());


                rect = lastBounds();

                if (mouseEvent.isControlDown() == true) {

                    if (rect.width > rect.height) {
                        rect.width = rect.height;
                    } else {
                        rect.height = rect.width;
                    }
                }

                g.drawArc(rect.x, rect.y, rect.width, rect.height, startAngle, endAngle);


                rect = getBounds();

                if (mouseEvent.isControlDown() == true) {

                    if (rect.width > rect.height) {
                        rect.width = rect.height;
                    } else {
                        rect.height = rect.width;
                    }
                }

                g.drawArc(rect.x, rect.y, rect.width, rect.height, startAngle, endAngle);

                g.setPaintMode();
                g.dispose();
            }


        }
    }

    /**
     * Makes an elliptical using an contour VOI upon a mouse release.
     *
     * @param  mouseEvent  event that triggered function
     */
    public void mouseReleased(MouseEvent mouseEvent) {
        VOI newVOI = null;
        int slice;
        float zoomX, zoomY;
        float resolutionX, resolutionY;
        double x, y;
        int xOld, yOld, xInt, yInt;
        int a, b, c;
        int[] xPtsNew;
        int[] yPtsNew;
        double[] xPts = null;
        double[] yPts = null;

        int[] xFinal = null;
        int[] yFinal = null;
        int[] zFinal = null;

        double incX;
        double hwidth, shwidth;
        double hheight, shheight;
        Vector2f origin;
        int index;
        int colorID;
        ViewVOIVector VOIs;
        int i;
        String name;
        int nVOI;

        zoomX = ((ViewJComponentEditImage) (component)).getZoomX();
        zoomY = ((ViewJComponentEditImage) (component)).getZoomY();
        resolutionX = ((ViewJComponentEditImage) (component)).getResolutionX();
        resolutionY = ((ViewJComponentEditImage) (component)).getResolutionY();

        if (isActive()) {
            ModelImage image = ((ViewJComponentEditImage) (component)).getActiveImage();

            slice = ((ViewJComponentEditImage) (component)).getSlice();

            // define the data points for the contour
            try {
                xPts = new double[40];
                yPts = new double[40];

                Rectangle rect = getBounds();

                if (mouseEvent.isControlDown() == true) {

                    if (rect.width > rect.height) {
                        rect.width = rect.height;
                    } else {
                        rect.height = rect.width;
                    }
                }

                if ((rect.width <= 2) || (rect.height <= 2)) {
                    image.notifyImageDisplayListeners();
                    ((ViewJComponentEditImage) (component)).setCursorMode(ViewJComponentEditImage.DEFAULT);

                    return;
                }

                Rectangle cRect = new Rectangle(Math.round(rect.x / (zoomX * resolutionX)),
                                                Math.round(rect.y / (zoomY * resolutionY)),
                                                Math.round(rect.width / (zoomX * resolutionX)),
                                                Math.round(rect.height / (zoomY * resolutionY)));


                origin = new Vector2f((float) (cRect.x + (cRect.width / 2.0)),
                                      (float) (cRect.y + (cRect.height / 2.0)));
                a = 0;
                incX = cRect.width / 19.0;
                hwidth = cRect.width / 2.0;
                shwidth = hwidth * hwidth;

                hheight = cRect.height / 2.0;
                shheight = hheight * hheight;

                for (x = hwidth; x >= (-hwidth - 0.1); x -= incX, a++) {

                    if ((x * x) > shwidth) {
                        y = 0;
                    } else {
                        y = Math.sqrt((1 - ((x * x) / shwidth)) * shheight);
                    }

                    xPts[a] = x;
                    yPts[a] = y;
                }

                for (b = a - 2; b >= 1; b--, a++) {
                    xPts[a] = xPts[b];
                    yPts[a] = -yPts[b];
                }

                xPtsNew = new int[a];
                yPtsNew = new int[a];
                xOld = 0;
                yOld = 0;
                c = 0;

                for (b = 0; b < a; b++) {
                    xInt = (int) Math.round(origin.X + xPts[b]);
                    yInt = (int) Math.round(origin.Y + yPts[b]);

                    if (((xInt != xOld) || (yInt != yOld))) {
                        xPtsNew[c] = xInt;
                        yPtsNew[c] = yInt;
                        xOld = xPtsNew[c];
                        yOld = yPtsNew[c];
                        c++;
                    }
                }

                xFinal = new int[c];
                yFinal = new int[c];
                zFinal = new int[c];

                for (a = 0; a < c; a++) {
                    xFinal[a] = xPtsNew[a];
                    yFinal[a] = yPtsNew[a];
                    zFinal[a] = slice;
                }
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: RubberbandEllipse.mouseReleased");
                ((ViewJComponentEditImage) (component)).setCursorMode(ViewJComponentEditImage.DEFAULT);

                return;
            }

            if (((ViewJComponentEditImage) (component)).getVOIHandler().isNewVoiNeeded(VOI.CONTOUR)) {
                try {
                    VOIs = (ViewVOIVector) image.getVOIs();
                    index = VOIs.size();
                    colorID = 0;

                    if (image.getVOIs().size() > 0) {
                        colorID = ((VOI) (image.getVOIs().lastElement())).getID() + 1;
                    }

                    nVOI = VOIs.size();
                    name = "ellipse" + (index + 1);

                    int test;
                    /*
                     * do{ test =0; for(i=0; i <nVOI; i++) {     if (colorID ==((int)VOIs.VOIAt(i).getID())) {
                     * colorID++;         test=1;     } } } while(test==1);
                     *
                     */

                    do {
                        test = 0;

                        for (i = 0; i < nVOI; i++) {

                            if (name.equals(VOIs.VOIAt(i).getName())) {
                                index++;
                                name = "ellipse" + (index + 1);
                                test = 1;
                            }
                        }
                    } while (test == 1);

                    newVOI = new VOI((short) colorID, name, VOI.CONTOUR, presetHue);
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to form new ellipse VOI.");

                    return;
                }

                newVOI.importCurve(xFinal, yFinal, zFinal);
                image.registerVOI(newVOI);
                ((ViewJComponentEditImage) (component)).getVOIHandler().setVOI_IDs(newVOI.getID(), newVOI.getUID());

            } // end if need to create a new voi

            // else add to the current voi
            else {
                VOIs = image.getVOIs();
                nVOI = VOIs.size();

                for (i = 0; i < nVOI; i++) {

                    if (VOIs.VOIAt(i).getID() == ((ViewJComponentEditImage) (component)).getVOIHandler().getVOI_ID()) {

                        if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                            VOIs.VOIAt(i).importCurve(xFinal, yFinal, zFinal);
                        } else {
                            MipavUtil.displayError("Can't add Ellipse VOI to other VOI structure.");
                        }
                    }
                }
            }

            image.notifyImageDisplayListeners();

            if (!(mouseEvent.isShiftDown() == true || Preferences.is(Preferences.PREF_CONTINUOUS_VOI_CONTOUR))) {
                ((ViewJComponentEditImage) (component)).setCursorMode(ViewJComponentEditImage.DEFAULT);
            }
        }
    }
}
