package gov.nih.mipav.view;


import gov.nih.mipav.model.structures.*;
import java.awt.event.*;

/**
 * VOIRegistrationHandler:  class to handle the VOI actions for the ViewJFrameRegistrationTool (and in turn
 * ViewJComponentSingleRegistration), which requires that there are two different types of 
 * VOIPoints for registration:  reference and adjusted
 * @author linkb
 *
 */
public class VOIRegistrationHandler extends VOIHandler implements MouseListener, MouseMotionListener {
  
  
	//~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default constructor.
     *
     * @param  comp  ViewJComponentEditImage
     */
    public VOIRegistrationHandler(ViewJComponentEditImage comp) {
    	super(comp);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------


    
//  ************************************************************************
    // ************************** Mouse Motion Events *************************
    // ************************************************************************

    /**
     * A mouse event. Drags a VOI real time by calling a series of translations and draws. Once the mouse is released,
     * the positions are reset permenantly. Also rubberbands points if the cursor indicates it by calling rubberbandVOI.
     *
     * @param  mouseEvent  event that triggered function
     */
    public void mouseDragged(MouseEvent mouseEvent) {
        int i, j;
        int nVOI;
        ViewVOIVector VOIs = compImage.getActiveImage().getVOIs();
        int xS, yS;
        int distX, distY;
        int xDim, yDim;
        int zDim = 1;

        int sliceNum;
        
        xS = (int) (mouseEvent.getX() / (compImage.getZoomX() * compImage.getResolutionX())); // zoomed x.  Used as cursor
        yS = (int) (mouseEvent.getY() / (compImage.getZoomY() * compImage.getResolutionY())); // zoomed y.  Used as cursor

        xDim = compImage.getActiveImage().getExtents()[0];
        yDim = compImage.getActiveImage().getExtents()[1];

        if (compImage.getActiveImage().getNDims() >= 3) {
            zDim = compImage.getActiveImage().getExtents()[2];
        }

        if ((xS < 0) || (xS >= xDim) || (yS < 0) || (yS >= yDim)) {
            return;
        }
        
        distX = xS - getAnchorPt().x; // distance from original to cursor
        distY = yS - getAnchorPt().y;


        nVOI = VOIs.size();

        if (mode == ViewJComponentEditImage.MOVE) {

            for (i = 0; i < nVOI; i++) {
                if (VOIs.VOIAt(i).isActive() && VOIs.VOIAt(i).isVisible()) {

                    if (VOIs.VOIAt(i).getCurveType() == VOI.POINT) {
                        setCursor(compImage.crosshairCursor);

                        if (allActive) {
                            VOIs.VOIAt(i).moveVOI(-1, xDim, yDim, zDim, distX, distY, 0);
                        } else {

                            // System.err.println("Not doing point shift down");
                            VOIs.VOIAt(i).moveVOI(compImage.getSlice(), xDim, yDim, zDim, distX, distY, 0);
                        }

                        compImage.getActiveImage().notifyImageDisplayListeners();
                        break;
                    }
                }
            }

            getAnchorPt().x = xS;
            getAnchorPt().y = yS;

            if (i == nVOI) {
                return;
            }


            return;
        } else if (mode == ViewJComponentEditImage.MOVE_POINT) { // rubberband a point

            if (mouseEvent.isShiftDown() == false) {

                for (i = 0; i < nVOI; i++) {

                    if (VOIs.VOIAt(i).isActive() && (VOIs.VOIAt(i).getCurveType() != VOI.POINT)) {

                        // Hides the cursor during dragging so it doesn't get in the way.
                        setCursor(MipavUtil.blankCursor);

                        VOIs.VOIAt(i).rubberbandVOI(xS, yS, compImage.getSlice(), xDim, yDim, false);

                    } // if( VOIs.VOIAt(i).isActive() && VOIs.VOIAt(i).getCurveType() != VOI.POINT)
                } // for (i = 0; i < nVOI; i++)
            } // end of if (mouseEvent.isShiftDown() == false)
            else { // shift key is depressed

                
            	sliceNum = 1;
                

                for (i = 0; i < nVOI; i++) {

                    if (VOIs.VOIAt(i).isActive() && (VOIs.VOIAt(i).getCurveType() != VOI.POINT)) {

                        for (j = 0; j < sliceNum; j++) {
                            VOIs.VOIAt(i).rubberbandVOI(xS, yS, j, xDim, yDim, false);
                        }

                        break;
                    }
                }
            } // end of else for shift key is depressed

            compImage.getActiveImage().notifyImageDisplayListeners();
            return;
        }

        
    }
    
    /**
     * A mouse event. This function sets up and draws the VOI according to the mode.
     *
     * @param  mouseEvent  event that triggered function
     */
    public void mouseReleased(MouseEvent mouseEvent) {
        int i, j, k;
        int xR, yR;
        int nVOI;
        int xS, yS;
        ViewVOIVector VOIs = compImage.getActiveImage().getVOIs();

        xS = (int) (mouseEvent.getX() / (compImage.getZoomX() * compImage.getResolutionX())); // zoomed x.  Used as cursor
        yS = (int) (mouseEvent.getY() / (compImage.getZoomY() * compImage.getResolutionY())); // zoomed y.  Used as cursor
        xR = mouseEvent.getX();
        yR = mouseEvent.getY();

        if ((xS < 0) || (xS >= compImage.getActiveImage().getExtents()[0]) || (yS < 0) || (yS >= compImage.getActiveImage().getExtents()[1])) {
            return;
        }

        if (mode == ViewJComponentBase.POINT_VOI) {

            if ((mouseEvent.getModifiers() & InputEvent.BUTTON1_MASK) != 0) {

                if (isNewVoiNeeded(VOI.POINT)) { // create new VOI
                    VOI newPtVOI = null;
                    try {
                        float[] x = new float[1];
                        float[] y = new float[1];
                        float[] z = new float[1];

                        setVOI_ID(compImage.getActiveImage().getVOIs().size());

                        int colorID = 0;

                        if (compImage.getActiveImage().getVOIs().size() > 0) {
                            colorID = ((VOI) (compImage.getActiveImage().getVOIs().lastElement())).getID() + 1;
                        }

                        if (compImage.getActiveImage().getNDims() > 2) {

                            newPtVOI = new VOI((short) colorID, "point3D.voi", compImage.getActiveImage().getExtents()[2], VOI.POINT,
                                               -1.0f);
                        } else {
                            newPtVOI = new VOI((short) colorID, "point2d.voi", 1, VOI.POINT, -1.0f);
                        }

                        x[0] = xS;
                        y[0] = yS;
                        z[0] = compImage.getSlice();
                        newPtVOI.importCurve(x, y, z, compImage.getSlice());
                        newPtVOI.setUID(newPtVOI.hashCode());


                        if (((ViewJComponentSingleRegistration)compImage).isReference()) {
                            newPtVOI.setColor(0.0f);
                        } else {
                            newPtVOI.setColor(1.0f / 3.0f);
                        }

                    } catch (OutOfMemoryError error) {
                        System.gc();
                        MipavUtil.displayError("Out of memory: ComponentEditImage.mouseReleased");
                        setMode(ViewJComponentBase.DEFAULT);

                        return;
                    }

                   // setLastPointVOI_ID(voiID);
                    compImage.getActiveImage().registerVOI(newPtVOI);
                    newPtVOI.setActive(true);

                    updateVOIColor(newPtVOI.getColor(), newPtVOI.getUID());
                    ((VOIPoint) (VOIs.VOIAt(getVOI_ID()).getCurves()[compImage.getSlice()].elementAt(0))).setActive(true);

                    compImage.getActiveImage().notifyImageDisplayListeners();

                } // end of if (voiID == -1)
                else { // voiID != -1 add point to existing VOI

                    // System.err.println("Adding to existing voi");

                    int index;

                    nVOI = VOIs.size();

                    float[] x = new float[1];
                    float[] y = new float[1];
                    float[] z = new float[1];

                    x[0] = xS;
                    y[0] = yS;
                    z[0] = compImage.getSlice();

                    for (i = 0; i < nVOI; i++) {

                        if (VOIs.VOIAt(i).getID() == getVOI_ID()) {

                            if ((VOIs.VOIAt(i).getCurveType() == VOI.POINT) && 
                            		(((ViewJComponentSingleRegistration)compImage).getCenterPtLocation() != i)) {
                                VOIs.VOIAt(i).importCurve(x, y, z, compImage.getSlice());

                                break;
                            } else {
                                MipavUtil.displayError("Can't add Point VOI to other VOI structure.");

                                return;
                            }
                        }
                    }

                    int end;

                    if (compImage.getActiveImage().getNDims() >= 3) {
                        end = compImage.getActiveImage().getExtents()[2];
                    } else {
                        end = 1;
                    }

                    for (j = 0; j < end; j++) {
                        index = VOIs.VOIAt(i).getCurves()[j].size();

                        for (k = 0; k < index; k++) {
                            ((VOIPoint) (VOIs.VOIAt(i).getCurves()[j].elementAt(k))).setActive(false);
                        }
                    }

                    index = VOIs.VOIAt(i).getCurves()[compImage.getSlice()].size();
                    ((VOIPoint) (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(index - 1))).setActive(true);

                    compImage.getActiveImage().notifyImageDisplayListeners();
                    return;
                } // end of else for if voiID != -1 add point to existing VOI
            } // end of if ((mouseEvent.getModifiers() & mouseEvent.BUTTON1_MASK) != 0)
        } // end of else if (mode == ViewJComponentBase.POINT_VOI)
        else if (mode == ViewJComponentBase.DELETE_POINT) { // impossible for LINE
            nVOI = VOIs.size();

            for (i = 0; i < nVOI; i++) {

                if (VOIs.VOIAt(i).isActive()) {
                    break;
                }
            }

            if (i == nVOI) {
                return;
            }

            int index = VOIs.VOIAt(i).getActiveContourIndex(compImage.getSlice());

            ((VOIContour) (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(index))).removeElement();

            compImage.getActiveImage().notifyImageDisplayListeners();
            setMode(ViewJComponentEditImage.MOVE_POINT);
        }  else if (mode == ViewJComponentEditImage.MOVE) {
            nVOI = VOIs.size();

            for (i = 0; i < nVOI; i++) {
                VOIs.VOIAt(i).setAllActive(false); // deactivate all other VOIs
            }

            for (i = 0; i < nVOI; i++) {
                VOIBase selectedCurve = null;

                for (j = 0; j < VOIs.VOIAt(i).getCurves()[compImage.getSlice()].size(); j++) {

                    // get the curve referenced by the VOI.  We'll check it.
                    selectedCurve = ((VOIBase) VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j));

                    if ((selectedCurve instanceof VOIPoint) &&
                            ((VOIPoint) selectedCurve).nearPoint(xR, yR, compImage.getZoomX(), compImage.getResolutionX(), compImage.getResolutionY())) {

                        // points are not true curves, but we want to check if we
                        // released mouse over it. we'll at least set the point active.
                        if (mouseEvent.isShiftDown()) {
                            allActive = true;

                            // System.err.println("Got a shift down");
                            // if true set all points in VOI active - move all points
                            VOIs.VOIAt(i).setAllActive(true);
                            updateVOIColor(VOIs.VOIAt(i).getColor(), VOIs.VOIAt(i).getUID());
                            setVOI_ID(VOIs.VOIAt(i).getID());

                            // and we are done with this VOI.
                            // skip the rest of the curves
                            j = VOIs.VOIAt(i).getCurves()[compImage.getSlice()].size();
                        } else {
                            allActive = false;
                            VOIs.VOIAt(i).setActive(true);
                            updateVOIColor(VOIs.VOIAt(i).getColor(), VOIs.VOIAt(i).getUID());
                            ((VOIPoint) (selectedCurve)).setActive(true);
                            setVOI_ID(VOIs.VOIAt(i).getID());
                        }
                    } else { // selected curve was not selected, so set false.
                        selectedCurve.setActive(false);
                    }
                } // end of curves in this VOI
            } // end checking all VOIs in the active image

            compImage.getActiveImage().notifyImageDisplayListeners();
        }


        // reset mousePressIsShiftDown for next mouse click
        mousePressIsShiftDown = false;

    } // end mouseReleased()

    
    /**
     * Compares the current VOI ID and its type to the given voi type. If the types are different, then set the voi ID
     * to -1, and return true (to indicate changing the type). If the types are the same, then it's not necessary to
     * change types so return false.
     *
     * @param   voiType  this should be one of the static ints from <code>VOI</code> (for instance <code>
     *                   VOI.LINE</code>).
     *
     * @return  whether or not a *new* voi must be created.
     */
    public boolean isNewVoiNeeded(int voiType) {

        if (getVOI_ID() == -1) {
            return true;
        }

        if (((ViewJComponentSingleRegistration)compImage).getCenterPtLocation() != -1) {

            if (compImage.getActiveImage().getVOIs().size() == 1) {
                return true;
            }
        }

        return false;
    }
    
}
