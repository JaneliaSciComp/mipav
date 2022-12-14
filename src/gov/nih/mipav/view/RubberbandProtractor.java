package gov.nih.mipav.view;


import gov.nih.mipav.model.structures.*;

import java.awt.*;
import java.awt.event.*;


/**
 * A Rubberband that does protractors.
 */
public class RubberbandProtractor extends Rubberband {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 808272437770323870L;

    //~ Instance fields ------------------------------------------------------------------------------------------------


    /** DOCUMENT ME! */
    private int[] x = new int[3];

    /** DOCUMENT ME! */
    private int[] y = new int[3];

    /** DOCUMENT ME! */
    private int[] z = new int[3];

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new RubberbandProtractor object.
     *
     * @param  component  component to add to
     */
    public RubberbandProtractor(Component component) {
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
     * Draws a line based on the rubberband's present bounds.
     *
     * @param  graphics  graphics to draw in
     */
    public void drawNext(Graphics graphics) {
        graphics.drawLine(anchorPt.x, anchorPt.y, stretchedPt.x, stretchedPt.y);
    }

    /**
     * Makes a VOI.PROTRACTOR.
     *
     * @param  mouseEvent  event that triggered function
     */
    public void mouseReleased(MouseEvent mouseEvent) {
        VOI newVOI;
        int beginx, endx, beginy, endy;
        int slice;
        float zoomX, zoomY;
        float resolutionX, resolutionY;
        int index;
        int colorID;
        int i;
        ViewVOIVector VOIs;
        int nVOI;
        String name;

        zoomX = ((ViewJComponentEditImage) (component)).getZoomX();
        zoomY = ((ViewJComponentEditImage) (component)).getZoomY();
        resolutionX = ((ViewJComponentEditImage) (component)).getResolutionX();
        resolutionY = ((ViewJComponentEditImage) (component)).getResolutionY();

        if (isActive()) {
            ModelImage image = ((ViewJComponentEditImage) (component)).getActiveImage();

            // get the points for the protractor
            beginx = (int) (getAnchor().x / (zoomX * resolutionX));
            beginy = (int) (getAnchor().y / (zoomY * resolutionY));
            endx = (int) (getLast().x / (zoomX * resolutionX));
            endy = (int) (getLast().y / (zoomY * resolutionY));

            slice = ((ViewJComponentEditImage) (component)).getSlice();

            if (beginx <= endx) {
                x[0] = beginx;
                y[0] = beginy;
                z[0] = slice;
                x[1] = (beginx + endx) / 2;
                y[1] = (beginy + endy) / 2;
                z[1] = slice;
                x[2] = endx;
                y[2] = endy;
                z[2] = slice;
            } else {
                x[0] = endx;
                y[0] = endy;
                z[0] = slice;
                x[1] = (beginx + endx) / 2;
                y[1] = (beginy + endy) / 2;
                z[1] = slice;
                x[2] = beginx;
                y[2] = beginy;
                z[2] = slice;
            }

            // get selected protractor
            VOIs = image.getVOIs();
            nVOI = VOIs.size();

            for (i = 0; i < nVOI; i++) {

                if (VOIs.VOIAt(i).getID() == 0) {

                    if (VOIs.VOIAt(i).getCurveType() == VOI.PROTRACTOR) {
                        VOIs.VOIAt(i).importCurve(x, y, z);
                    } else {
                        MipavUtil.displayError("Can't add Protractor VOI to other VOI structure.");
                    }
                }
            }

            image.notifyImageDisplayListeners();

            if (mouseEvent.isShiftDown() != true) {
                ((ViewJComponentEditImage) (component)).setCursorMode(ViewJComponentEditImage.DEFAULT);
            }
        }
    }
}
