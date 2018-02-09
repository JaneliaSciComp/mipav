package gov.nih.mipav.view;


import gov.nih.mipav.model.structures.*;

import java.awt.*;
import java.awt.event.*;


/**
 * A Rubberband that does rectangles.
 *
 * @version  1.1, 5/20/99
 * @author   Matthew McAuliffe, Ph.D.
 */

public class RubberbandRectangleVOI extends RubberbandRectangle {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 3071762927070207288L;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new RubberbandRectangleVOI object.
     *
     * @param  component  component to add to
     */
    public RubberbandRectangleVOI(Component component) {
        super(component);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Gets the stretch point of the rubberband.
     *
     * @param  mouseEvent  event that triggered function
     */
    public void mouseDragged(MouseEvent mouseEvent) {

        if (isActive()) {
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

                g.drawRect(rect.x, rect.y, rect.width, rect.height);

                rect = getBounds();

                if (mouseEvent.isControlDown() == true) {

                    if (rect.width > rect.height) {
                        rect.width = rect.height;
                    } else {
                        rect.height = rect.width;
                    }
                }

                g.drawRect(rect.x, rect.y, rect.width, rect.height);

                g.setPaintMode();
                g.dispose();
            }
        }
    }

    /**
     * Makes a VOI out of the contours.
     *
     * @param  mouseEvent  event that triggered function
     */
    public void mouseReleased(MouseEvent mouseEvent) {
        int i, j;
        VOI newVOI;
        float zoomX, zoomY;
        float resolutionX, resolutionY;
        int[] x = null;
        int[] y = null;
        int[] z = null;
        int index;
        int colorID;
        int nVOI;
        ViewVOIVector VOIs;
        String name;

        // System.err.println("adding contour");

        zoomX = ((ViewJComponentEditImage) (component)).getZoomX();
        zoomY = ((ViewJComponentEditImage) (component)).getZoomY();
        resolutionX = ((ViewJComponentEditImage) (component)).getResolutionX();
        resolutionY = ((ViewJComponentEditImage) (component)).getResolutionY();

        if (isActive()) {

            ModelImage image = ((ViewJComponentEditImage) (component)).getActiveImage();

            if (((ViewJComponentEditImage) (component)).getVOIHandler().isNewVoiNeeded(VOI.CONTOUR)) {

                try {
                    VOIs = image.getVOIs();
                    index = VOIs.size();
                    colorID = 0;

                    if (image.getVOIs().size() > 0) {
                        colorID = ((VOI) (image.getVOIs().lastElement())).getID() + 1;
                    }

                    nVOI = VOIs.size();
                    name = "rect" + (index + 1);

                    int test;

                    do {
                        test = 0;

                        for (i = 0; i < nVOI; i++) {

                            if (name.equals(VOIs.VOIAt(i).getName())) {
                                index++;
                                name = "rect" + (index + 1);
                                test = 1;
                            }
                        }
                    } while (test == 1);
                    /*
                     * do{ test =0; for(i=0; i <nVOI; i++) {         if (colorID ==((int)VOIs.VOIAt(i).getID())) {
                     *       colorID++;             test=1;         } } } while(test==1);
                     */


                    newVOI = new VOI((short) colorID, name, VOI.CONTOUR, presetHue);

                    x = new int[4];
                    y = new int[4];
                    z = new int[4];

                    Rectangle rect = getBounds();

                    if (mouseEvent.isControlDown() == true) {

                        if (rect.width > rect.height) {
                            rect.width = rect.height;
                        } else {
                            rect.height = rect.width;
                        }
                    }

                    int ptx = Math.round(rect.x / (zoomX * resolutionX));
                    int pty = Math.round(rect.y / (zoomY * resolutionY));
                    int width = Math.round(rect.width / (zoomX * resolutionX));
                    int height = Math.round(rect.height / (zoomY * resolutionY));

                    x[0] = ptx;
                    y[0] = pty;
                    x[1] = ptx + width;
                    y[1] = pty;
                    x[2] = ptx + width;
                    y[2] = pty + height;
                    x[3] = ptx;
                    y[3] = pty + height;

                    if ((Math.abs(x[2] - x[0]) > 1) && (Math.abs(y[2] - y[0]) > 1)) {

                        if ((image.getNDims() > 2) &&
                                (((ViewJComponentEditImage) (component)).getCursorMode() ==
                                     ViewJComponentBase.RECTANGLE3D)) {

                            for (i = 0; i < image.getExtents()[2]; i++) {
                                z[0] = z[1] = z[2] = z[3] = i;
                                newVOI.importCurve(x, y, z);
                            }
                        } else {
                        	int curSlice = ((ViewJComponentEditImage) (component)).getSlice();
                        	for (i = 0; i < z.length; i++) {
                        		z[i] = curSlice;
                        	}
                            newVOI.importCurve(x, y, z);
                        }

                        image.registerVOI(newVOI);
                        ((ViewJComponentEditImage) (component)).getVOIHandler().setVOI_IDs(newVOI.getID(), newVOI.getUID());
                    }
                } catch (OutOfMemoryError error) {
                    System.gc();
                    ((ViewJComponentEditImage) (component)).setCursorMode(ViewJComponentEditImage.DEFAULT);
                    MipavUtil.displayError("Out of memory: RubberbandRectangle.mouseReleased");
                    ((ViewJComponentEditImage) (component)).setCursorMode(ViewJComponentEditImage.DEFAULT);

                    return;
                }

                image.notifyImageDisplayListeners();
            } else {

                // get selected contour
                VOIs = image.getVOIs();
                nVOI = VOIs.size();

                try {
                    x = new int[4];
                    y = new int[4];
                    z = new int[4];

                    Rectangle rect = getBounds();

                    if (mouseEvent.isControlDown() == true) {

                        if (rect.width > rect.height) {
                            rect.width = rect.height;
                        } else {
                            rect.height = rect.width;
                        }
                    }

                    int ptx = Math.round(rect.x / (zoomX * resolutionX));
                    int pty = Math.round(rect.y / (zoomY * resolutionY));
                    int width = Math.round(rect.width / (zoomX * resolutionX));
                    int height = Math.round(rect.height / (zoomY * resolutionY));

                    x[0] = ptx;
                    y[0] = pty;
                    x[1] = ptx + width;
                    y[1] = pty;
                    x[2] = ptx + width;
                    y[2] = pty + height;
                    x[3] = ptx;
                    y[3] = pty + height;

                    for (i = 0; i < nVOI; i++) {

                        if (VOIs.VOIAt(i).getID() == ((ViewJComponentEditImage) (component)).getVOIHandler().getVOI_ID()) {

                            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {

                                if ((Math.abs(x[2] - x[0]) > 1) && (Math.abs(y[2] - y[0]) > 1)) {

                                    if ((image.getNDims() > 2) &&
                                            (((ViewJComponentEditImage) (component)).getCursorMode() ==
                                                 ViewJComponentBase.RECTANGLE3D)) {

                                        for (j = 0; j < image.getExtents()[2]; j++) {
                                            z[0] = z[1] = z[2] = z[3] = j;
                                            VOIs.VOIAt(i).importCurve(x, y, z);
                                        }
                                    } else {
                                        z[0] = ((ViewJComponentEditImage) (component)).getSlice();
                                        VOIs.VOIAt(i).importCurve(x, y, z);
                                    }
                                }
                            } else {
                                MipavUtil.displayError("Can't add Polygon VOI to other VOI structure.");
                            }
                        }
                    }


                } catch (OutOfMemoryError error) {
                    System.gc();
                    ((ViewJComponentEditImage) (component)).setCursorMode(ViewJComponentEditImage.DEFAULT);
                    MipavUtil.displayError("Out of memory: RubberbandRectangle.mouseReleased");
                    ((ViewJComponentEditImage) (component)).setCursorMode(ViewJComponentEditImage.DEFAULT);

                    return;
                }

                image.notifyImageDisplayListeners();
            }

            if (!(mouseEvent.isShiftDown() == true || Preferences.is(Preferences.PREF_CONTINUOUS_VOI_CONTOUR))) {

                // System.err.println("Rect Shift down is NOT true");
                ((ViewJComponentEditImage) (component)).setCursorMode(ViewJComponentEditImage.DEFAULT);
            } else {
                // System.err.println("Rect shift is down");
            }
        }

    }
}
