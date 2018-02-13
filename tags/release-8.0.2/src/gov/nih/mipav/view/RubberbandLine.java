package gov.nih.mipav.view;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;


/**
 * A Rubberband that does lines.
 */
public class RubberbandLine extends Rubberband {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 6355805256478756437L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int[] x = new int[2];

    /** DOCUMENT ME! */
    private int[] y = new int[2];

    /** DOCUMENT ME! */
    private int[] z = new int[2];

    /** boolean for if this is a VOI Splitter (not line drawer)*/
    private boolean doSplit = false;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs a rubberbanded line and dds mouse listeners.
     *
     * @param  component  component to add to
     */
    public RubberbandLine(Component component) {
        super(component);
        component.addMouseMotionListener(this);
        component.addMouseListener(this);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    public void setSplit(boolean split) {
    	this.doSplit = split;
    }
    
    public boolean isSplitter() {
    	return this.doSplit;
    }
    
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
     * Makes a line VOI upon mouse release.
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

            // get the points for the line
            beginx = (int) (getAnchor().x / (zoomX * resolutionX));
            beginy = (int) (getAnchor().y / (zoomY * resolutionY));
            endx = (int) (getLast().x / (zoomX * resolutionX));
            endy = (int) (getLast().y / (zoomY * resolutionY));

            slice = ((ViewJComponentEditImage) (component)).getSlice();

            if (beginx <= endx) {
                x[0] = beginx;
                y[0] = beginy;
                z[0] = slice;
                x[1] = endx;
                y[1] = endy;
                z[1] = slice;
            } else {
                x[0] = endx;
                y[0] = endy;
                z[0] = slice;
                x[1] = beginx;
                y[1] = beginy;
                z[1] = slice;
            }
        	
        	if (!doSplit) {
        	
            

            // check to see if this is a *new* VOI
            if (((ViewJComponentEditImage) (component)).getVOIHandler().isNewVoiNeeded(VOI.LINE)) {

                try {
                    VOIs = image.getVOIs();
                    index = VOIs.size();
                    colorID = 0;

                    if (image.getVOIs().size() > 0) {
                        colorID = ((VOI) (image.getVOIs().lastElement())).getID() + 1;
                    }

                    nVOI = VOIs.size();
                    name = "line" + (index + 1);

                    int test;


                    do {
                        test = 0;

                        for (i = 0; i < nVOI; i++) {

                            if (name.equals(VOIs.VOIAt(i).getName())) {
                                index++;
                                name = "line" + (index + 1);
                                test = 1;
                            }
                        }
                    } while (test == 1);

                    /*
                     * do{     test =0;     for(i=0; i <nVOI; i++) {             if (colorID
                     * ==((int)VOIs.VOIAt(i).getID())) {                 colorID++;                 test=1;
                     * }     } } while(test==1);
                     */
                    newVOI = new VOI((short) colorID, name, VOI.LINE, presetHue);
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to form new line VOI.");
                    ((ViewJComponentEditImage) (component)).setCursorMode(ViewJComponentEditImage.DEFAULT);

                    return;
                }
 
                if ((Math.abs(x[1] - x[0]) > 1) || (Math.abs(y[1] - y[0]) > 1)) {
                    newVOI.importCurve(x, y, z);
                    image.registerVOI(newVOI);
                }

                image.notifyImageDisplayListeners();

                if (!(mouseEvent.isShiftDown() == true || Preferences.is(Preferences.PREF_CONTINUOUS_VOI_CONTOUR))) {
                    ((ViewJComponentEditImage) (component)).setCursorMode(ViewJComponentEditImage.DEFAULT);
                }

                ((ViewJComponentEditImage) (component)).getVOIHandler().setVOI_IDs(newVOI.getID(), newVOI.getUID());

            } // end if this is a *new* VOI
            else { // this is not a *new* VOI ... add to existing VOI

                // get selected line
                VOIs = image.getVOIs();
                nVOI = VOIs.size();

                for (i = 0; i < nVOI; i++) {

                    if (VOIs.VOIAt(i).getID() == ((ViewJComponentEditImage) (component)).getVOIHandler().getVOI_ID()) {

                        if (VOIs.VOIAt(i).getCurveType() == VOI.LINE) {
                            VOIs.VOIAt(i).importCurve(x, y, z);
                        } else {
                            MipavUtil.displayError("Can't add Line VOI to other VOI structure.");
                        }
                    }
                }

                image.notifyImageDisplayListeners();

                if (!(mouseEvent.isShiftDown() == true || Preferences.is(Preferences.PREF_CONTINUOUS_VOI_CONTOUR))) {
                    ((ViewJComponentEditImage) (component)).setCursorMode(ViewJComponentEditImage.DEFAULT);
                }

                return;

            } // end if not a *new* VOI

        } else {
        	new JDialogVOISplitter(image.getParentFrame(), image, new Vector3f(x[0],y[0],z[0]), new Vector3f(x[1],y[1],z[1])) ;
        }
    }
    }
}
