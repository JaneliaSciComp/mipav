package gov.nih.mipav.view;


import gov.nih.mipav.*;

import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;

import javax.swing.*;
import javax.swing.event.*;


/**
 * VOIHandler class is used to handle all aspects of VOI movement from the ViewJComponentEditImage. It functions as a
 * mouse listener and also handles Popup-menus and VOI graph displays.
 *
 * @author   not attributable
 * @version  1.0
 */
public class VOIHandler extends JComponent implements MouseListener, MouseMotionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6198406879690675965L;

    /** Static Variables for VOI and VOI Contour movement. */
    public static final int FORWARD = 0;

    /** DOCUMENT ME! */
    public static final int BACKWARD = 1;

    /** DOCUMENT ME! */
    public static final int FRONT = 2;

    /** DOCUMENT ME! */
    public static final int BACK = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Buffer for holding VOI graphing information. */
    protected float[] graphImgBuff;

    /** color of grid. */
    protected Color gridColor = Color.lightGray;

    /** if number/lettering should be displayed for grid boxes. */
    protected boolean gridLabelingOn = false;

    /** boolean to determine the orientation: true is x-axis numbered false is x-axis lettered. */
    protected boolean gridLabelOrientation = true;

    /** Flag to indicate if NEI grid overlay should be displayed. */
    protected boolean gridOverlayOn = false;

    /** spacing of the grid (horizontal) in terms of resolution. */
    protected float gridSpacingX = 20f;

    /** spacing of the grid (vertical) in terms of resolution. */
    protected float gridSpacingY = 20f;

    /** Keep track of state of shift for mouse Pressed events. */
    protected boolean mousePressIsShiftDown = false;

    /** New polyline slice VOI. */
    protected VOI newPolySliceVOI = null;

    /** Flag to indicate if DICOM overlay should be displayed. */
    protected boolean overlayOn = false;

    /** Popup Menu for VOIs (non-point). */
    protected ViewJPopupVOI popup = null;

    /** Popup Menu for VOIPoints. */
    protected ViewJPopupPt popupPt = null;

    /** Buffer for holding intensities at specific points. */
    protected float[] ptIntensity;

    /** buffer used when graphing a VOIPoint on a grayscale image. */
    protected float[] ptPosition;

    /** Buffer for holding RGB intensities [0,1,2] at specific points. */
    protected float[][] ptRGBIntensities = null;

    /** Buffer used for graphing VOIPoints for an RGB image. */
    protected float[][] ptRGBPositions = null;


    /** Rubberband tool for VOIs. */
    protected RubberbandLevelSet rbLevelSet;

    /** Rubberband tool for VOIs. */
    protected Rubberband rbRect, rbPolyline, rbEllipse, rbLine, rbRectQuick, rbProtractor, rbLivewire;

    /** Rubberband tool for VOIs. */
    protected Rubberband rubberband;

    /** VOI clipboard used to undo the VOI. This VOI is a copy of last VOI selected. */
    protected VOI undoVOI;

    /** Dialog for displaying/calculating VOI statistics as well as the VOI Browser. */
    protected JDialogVOIStats voiDialog;

    /** Active VOI's ID. */
    protected int voiID = -1;

    /** VOI ID for image A. */
    protected int voiIDa = -1;

    /** VOI ID for image B. */
    protected int voiIDb = -1;

    /** Whether all counters of the currently selected VOI are active. */
    protected boolean allActive; // ?

    /** Anchor point used in calculated VOI movement distances. */
    protected Point anchorPt = new Point(0, 0);

    /** The component image that owns this class. */
    protected ViewJComponentEditImage compImage;

    /**
     * created to handle VOI updates. Must fireVOIUpdate(...) to get listeners to handle the update. Perhaps better
     * location for the VOIupdate is in <code>ViewJCompoenentEditImage</code>, but this listenerlist will handle
     * listeners of more than one type.
     */
    protected EventListenerList listenerList = new EventListenerList();


    /** Current mode for mouse behavior (VOI manipulation). */
    protected int mode;

    /** The preset hue for the livewire VOI rubberband. */
    protected float presetHue = -1.0f;

    /** an update event for the VOI. */
    protected UpdateVOIEvent voiUpdate = null;

    /** Whether the VOI was being dragged. */
    protected boolean wasDragging;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default constructor.
     *
     * @param  comp  ViewJComponentEditImage
     */
    public VOIHandler(ViewJComponentEditImage comp) {
        this.compImage = comp;

        /**
         * Create Popup Dialogs for VOIs and VOI points
         */
        popup = new ViewJPopupVOI(this);

        if (compImage.getActiveImage().getNDims() < 3) {
            popup.setEnabledPropagate(false);
        }

        popupPt = new ViewJPopupPt(this);

        if (compImage.getActiveImage().getNDims() < 3) {
            popupPt.setEnabledGraph(false);
            popupPt.setEnabledProp(false);
        }

        compImage.addMouseListener(popup);
        compImage.addMouseListener(popupPt);

        if (compImage.getFrame() != null) {
            voiDialog = new JDialogVOIStats(compImage.getFrame(), compImage.getActiveImage(), null);
            addVOIUpdateListener(voiDialog);
        }

        // create rubberbanding tools for VOIs
        rbRectQuick = new RubberbandRectangle(compImage);
        rbRect = new RubberbandRectangleVOI(compImage);
        rbPolyline = new RubberbandPolyline(compImage);
        rbEllipse = new RubberbandEllipse(compImage);
        rbLine = new RubberbandLine(compImage);
        rbProtractor = new RubberbandProtractor(compImage);
        rbLevelSet = new RubberbandLevelSet(compImage);
        rubberband = rbRect;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    // --------- Event-handling routines:
    // to add this object to send out events for listening
    // objects, at least the following 3 methods must be
    // present: addListener, removeListener, fireEvent as
    // present below.
    /**
     * adds the update listener.
     *
     * @param  listener  DOCUMENT ME!
     */
    public void addVOIUpdateListener(UpdateVOISelectionListener listener) {
        listenerList.add(UpdateVOISelectionListener.class, listener);
    }

    /**
     * Calls the VOI Function (for polyline slices) to calculate and display distances.
     */
    public void calcPLineSliceDistances() {
        int i, nVOI;

        ViewVOIVector VOIs = compImage.getActiveImage().getVOIs();

        nVOI = VOIs.size();

        if (nVOI == 0) {
            return;
        }

        for (i = 0; i < nVOI; i++) {

            if ((VOIs.VOIAt(i).isActive() == true) && (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE_SLICE)) {

                VOIs.VOIAt(i).calcPLineDistances(compImage.getActiveImage().getFileInfo(0));
            }

        }

    }

    /**
     * New all-in-one function for changing the order of whole VOIs and VOI contours.
     *
     * @param  doContour  boolean contour only (true) or entire VOI (false)
     * @param  direction  int (FORWARD, BACKWARD, FRONT, BACK)
     */
    public void changeVOIOrder(boolean doContour, int direction) {

        if ((direction != FORWARD) && (direction != BACKWARD) && (direction != FRONT) && (direction != BACK)) {
            return;
        }

        int i;
        int nVOI;
        ViewVOIVector VOIs;

        Vector[] curves;
        VOIBase selectedCurve;
        boolean foundActive = false;

        int index = 1;
        int nSlices = 1;
        int nContours;

        if (compImage.getActiveImage().getNDims() > 2) {
            nSlices = compImage.getActiveImage().getExtents()[2];
        }

        Preferences.debug("ComponentEditImage.changeVOIOrder\n");

        try {
            VOIs = compImage.getActiveImage().getVOIs();
            nVOI = VOIs.size();

            for (i = 0; i < nVOI; i++) {

                if (VOIs.VOIAt(i).isActive() && VOIs.VOIAt(i).isVisible()) {


                    if (!doContour) {
                        foundActive = true;

                        if (direction == FORWARD) {

                            if (i == (nVOI - 1)) {
                                break;
                            } else if (i == (nVOI - 2)) { // already at top
                                VOIs.add(VOIs.VOIAt(i).clone());
                            } else {
                                VOIs.insertElementAt(VOIs.VOIAt(i).clone(), i + 2);
                            }

                            VOIs.removeElementAt(i);

                        } else if (direction == BACKWARD) {

                            if (i == 0) {
                                break;
                            } else {
                                VOIs.insertElementAt(VOIs.VOIAt(i).clone(), i - 1);
                            }

                            VOIs.removeElementAt(i + 1);

                        } else if (direction == FRONT) {
                            VOIs.add(VOIs.VOIAt(i).clone());
                            VOIs.removeElementAt(i);

                        } else if (direction == BACK) {
                            VOIs.add(0, VOIs.VOIAt(i).clone());
                            VOIs.removeElementAt(i + 1);

                        }

                        break;

                    } else { // move contour only

                        curves = VOIs.VOIAt(i).getCurves();

                        for (int j = 0; j < curves[compImage.getSlice()].size(); j++) {
                            selectedCurve = (VOIBase) curves[compImage.getSlice()].elementAt(j);

                            if (selectedCurve.isActive()) {
                                foundActive = true;

                                VOIBase curveClone = (VOIBase) selectedCurve.clone();
                                curves[compImage.getSlice()].removeElementAt(j);

                                if (direction == FORWARD) {

                                    if ((j - 1) >= 0) {
                                        curves[compImage.getSlice()].insertElementAt(curveClone, j - 1);
                                    } else {
                                        curves[compImage.getSlice()].insertElementAt(curveClone, 0);
                                    }

                                } else if (direction == BACKWARD) {

                                    if (curves[compImage.getSlice()].size() > (j + 1)) {
                                        curves[compImage.getSlice()].insertElementAt(curveClone, j + 1);
                                    } else {
                                        curves[compImage.getSlice()].addElement(curveClone);
                                    }
                                } else if (direction == FRONT) {

                                    curves[compImage.getSlice()].insertElementAt(curveClone, 0);
                                } else if (direction == BACK) {

                                    curves[compImage.getSlice()].addElement(curveClone);

                                }

                                // relabel the VOIs to be ordered correctly
                                index = 1;

                                for (int k = 0; k < nSlices; k++) {
                                    nContours = curves[k].size();

                                    for (int m = 0; m < nContours; m++) {
                                        ((VOIBase) curves[k].elementAt(m)).setLabel(String.valueOf((index++)));
                                    }
                                }

                                break;

                            }

                        }

                    }
                }
            }

            if (!foundActive) {
                MipavUtil.displayError("Please select a VOI!");
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ViewJComponentEditImage.changeVOIOrder.");

            return;
        }

        compImage.getActiveImage().notifyImageDisplayListeners(null, true);

    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean convertPointToPoly() {

        // first determine if there is more than one point on more than one compImage.getSlice()/frame

        ViewVOIVector VOIs = compImage.getActiveImage().getVOIs();
        int nVOI = VOIs.size();
        int xDim = compImage.getActiveImage().getExtents()[0];
        int yDim = compImage.getActiveImage().getExtents()[1];
        int zDim = 1;

        if (compImage.getActiveImage().getNDims() > 2) {
            zDim = compImage.getActiveImage().getExtents()[2];
        } else {
            return false;
        }

        for (int i = 0; i < nVOI; i++) {
            int curveType = VOIs.VOIAt(i).getCurveType();

            if (VOIs.VOIAt(i).isActive() && (curveType == VOI.POINT)) {
                System.err.println("got active point... will convert");
            }
        }

        return true;
    }

    /**
     * Places the active VOI and places it in clipboard.<br>
     * Note: this method does not actually "cut" voi, but rather just "copies it" (the voi is not deleted with this
     * method)
     */
    public void copyVOIforUndo() {
        int i;
        int nVOI;
        ViewVOIVector VOIs = compImage.getActiveImage().getVOIs();

        nVOI = VOIs.size();

        if (nVOI == 0) {
            return;
        }

        for (i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).isActive() == true) {
                break;
            } // Set i
        }

        this.undoVOI = (VOI) ((VOI) VOIs.elementAt(i)).clone();
    }

    /**
     * Places the active VOI and places it in clipboard.<br>
     * Note: this method does not actually "cut" voi, but rather just "copies it" (the voi is not deleted with this
     * method)
     *
     * @return  indicates if the cut was successful
     */
    public boolean copyVOItoClipBrd() {
        int i;
        int nVOI;
        int zDim = 1;
        ViewVOIVector VOIs = compImage.getActiveImage().getVOIs();

        nVOI = VOIs.size();

        VOI voi;
        Vector[] curves;

        // tells whether any copying was done
        boolean didCopy = false;

        // clear the clipboard
        ViewUserInterface.getReference().clearClippedVOIs();

        if (nVOI == 0) {
            return false;
        }

        
        if (compImage.getActiveImage().getNDims() == 2) {
        	for (i = 0; i < nVOI; i++) {

        		if (VOIs.VOIAt(i).isActive() == true) {
        			voi = (VOI) ((VOI) VOIs.elementAt(i)).clone();
        			voi.setUID(voi.hashCode());
        			this.undoVOI = (VOI) (voi.clone());

        			curves = voi.getCurves();
	                int numCurves = 0;

	                for (int s = 0; s < zDim; s++) {
	                	numCurves = curves[s].size();

	                	for (int j = numCurves - 1; j >= 0; j--) {

 	                       if (!((VOIBase) (curves[s].elementAt(j))).isActive()) {
 	                    	   curves[s].removeElementAt(j);
 	                       }
	                	}
	                }

	                // add the voi to the clipboard
	                ViewUserInterface.getReference().addClipped2DVOI(voi, compImage.getSlice());
	                didCopy = true;
        		}
        	}
        } else {
        	//image is 3+ dims... so copy the VOIs in scanner coordinate mode instead
        	for (i = 0; i < nVOI; i++) {

        		if (VOIs.VOIAt(i).isActive() == true) {
        			voi = (VOI) ((VOI) VOIs.elementAt(i)).clone();
        			voi.setUID(voi.hashCode());
        			this.undoVOI = (VOI) (voi.clone());

        			curves = voi.getCurves();

        			zDim = compImage.getActiveImage().getExtents()[2];

	                int numCurves = 0;

	                for (int s = 0; s < zDim; s++) {
	                	numCurves = curves[s].size();

	                	for (int j = numCurves - 1; j >= 0; j--) {

 	                       if (!((VOIBase) (curves[s].elementAt(j))).isActive()) {
 	                    	   curves[s].removeElementAt(j);
 	                       }
	                	}
	                }

	                //convert this VOI over to scanner coords and pass on to UI
	                Vector scannerVector = new Vector();
	                VOIBase tempBase = null;
	                int numPoints = 0;
	                Point3Df inPoint = new Point3Df();
	                Point3Df outPoint = null;
	                for (int s = 0; s < zDim; s++) {
	                	numCurves = curves[s].size();

	                	for (int j = 0; j < numCurves; j++) {
	                		tempBase = (VOIBase)curves[s].elementAt(j);
	                		numPoints = tempBase.size();
	                		for (int k = 0; k < numPoints; k++) {
	                			outPoint = new Point3Df();
	                			inPoint.x = ((Point3Df)tempBase.elementAt(k)).x;
	                			inPoint.y = ((Point3Df)tempBase.elementAt(k)).y;
	                			inPoint.z = s;
	                			
	                //			System.err.println("Initial point: " + inPoint);
	                			//convert to scanner and add to vector
	                			MipavCoordinateSystems.FileToScanner(inPoint, outPoint, compImage.getActiveImage());
	                			scannerVector.add(outPoint);
	                //			System.err.println("Scanner point: " + outPoint);
	                		}
	                	}
	                }
	                
	                
	                // add the voi to the clipboard
	                ViewUserInterface.getReference().addClippedScannerVOI(voi, compImage.getSlice(), scannerVector);
	                didCopy = true;
	          //      System.err.println("DID COPY");
        		}
        	}
        }

        if (!didCopy) {
            MipavUtil.displayError("VOI must be selected.");

            return false; // No VOI to delete
        }

        // frame.getUserInterface().setClippedVOI( voi, compImage.getSlice() );
        compImage.getActiveImage().notifyImageDisplayListeners(null, true);

        return true;
    }


    /**
     * Deletes the active contour, line, protractor, point of a VOI.
     *
     * @param  voi     volume of interest
     * @param  zSlice  the compImage.getSlice() presently displayed from a 3D dataset or 0 for 2D
     */
    public void deleteContour(VOI voi, int zSlice) {
        int j;
        int nCurves;
        Vector[] curves;

        curves = voi.getCurves();
        nCurves = curves[zSlice].size();

        for (j = 0; j < nCurves; j++) {

            if ((voi.getCurveType() == VOI.CONTOUR) || (voi.getCurveType() == VOI.POLYLINE)) {

                if (((VOIContour) (curves[zSlice].elementAt(j))).isActive()) {
                    voi.removeCurve(j, zSlice);
                    j = -1;
                    curves = voi.getCurves();
                    nCurves = curves[zSlice].size();
                }
            } else if (voi.getCurveType() == VOI.LINE) {

                if (((VOILine) (curves[zSlice].elementAt(j))).isActive()) {
                    voi.removeCurve(j, zSlice);
                    j = -1;
                    curves = voi.getCurves();
                    nCurves = curves[zSlice].size();
                }
            } else if (voi.getCurveType() == VOI.PROTRACTOR) {

                if (((VOIProtractor) (curves[zSlice].elementAt(j))).isActive()) {
                    voi.removeCurve(j, zSlice);
                    j = -1;
                    curves = voi.getCurves();
                    nCurves = curves[zSlice].size();
                }
            } else if (voi.getCurveType() == VOI.POINT) {

                if (((VOIPoint) (curves[zSlice].elementAt(j))).isActive()) {
                    voi.removeCurve(j, zSlice);
                    j = -1;
                    curves = voi.getCurves();
                    nCurves = curves[zSlice].size();
                }
            } else if (voi.getCurveType() == VOI.ANNOTATION) {

                if (((VOIText) (curves[zSlice].elementAt(j))).isActive()) {
                    voi.removeCurve(j, zSlice);
                    j = -1;
                    curves = voi.getCurves();
                    nCurves = curves[zSlice].size();
                }
            } else if (voi.getCurveType() == VOI.POLYLINE_SLICE) {

                if (((VOIPoint) (curves[zSlice].elementAt(j))).isActive()) {
                    voi.removeCurves(zSlice);
                    j = -1;
                    nCurves = 0;
                }
            }

        }
    }

    /**
     * Deletes selected VOIs or VOI contours (boolean).
     *
     * @param  contoursOnly  boolean (true = only delete selected contours, false = delete entire selected VOI)
     */
    public void deleteSelectedVOI(boolean contoursOnly) {
        int i, s, nVOI;

        ViewVOIVector VOIs = compImage.getActiveImage().getVOIs();
        boolean foundActive = false;
        nVOI = VOIs.size();

        if (nVOI == 0) {
            return;
        }

        for (i = nVOI - 1; i >= 0; i--) {

            if (VOIs.VOIAt(i).isActive() == true) {
                foundActive = true;

                if (contoursOnly) {

                    if (compImage.getActiveImage().getNDims() == 2) {
                        deleteContour(VOIs.VOIAt(i), 0);
                    } else if (compImage.getActiveImage().getNDims() >= 3) {

                        for (s = 0; s < compImage.getActiveImage().getExtents()[2]; s++) {
                            deleteContour(VOIs.VOIAt(i), s);
                        }
                    }

                    if (VOIs.VOIAt(i).isEmpty() == true) {
                        compImage.getActiveImage().unregisterVOI(VOIs.VOIAt(i));

                        int id = (compImage.getActiveImage().getVOIs().size() > 0)
                                 ? (((VOI) (compImage.getActiveImage().getVOIs().lastElement())).getID() + 1) : 0;
                        int lastUID = (compImage.getActiveImage().getVOIs().size() > 0)
                                      ? (((VOI) (compImage.getActiveImage().getVOIs().lastElement())).getUID() + 1)
                                      : -1;

                        this.updateVOIColor(id, lastUID);
                        voiID = -1;
                    }
                } else {
                    VOIs.removeElementAt(i);
                    voiID = -1;
                }
            } // Set i
        }

        if (!foundActive) {
            MipavUtil.displayError("VOI must be selected.");

            return; // No VOI to delete
        }

        fireVOISelectionChange(null);
        compImage.getActiveImage().notifyImageDisplayListeners(null, true);


    }

    /**
     * Removes (if there is an active Polyline or polygon) the active point (a single point) from the contour.
     */
    public void deleteVOIActivePt() {
        int end = 0;

        ViewVOIVector VOIs = compImage.getActiveImage().getVOIs();
        int nVOI = VOIs.size();

        for (int i = 0; i < nVOI; i++) {
            int curveType = VOIs.VOIAt(i).getCurveType();

            if (VOIs.VOIAt(i).isActive()) {

                if ((curveType == VOI.CONTOUR) || (curveType == VOI.POLYLINE)) {

                    if (compImage.getActiveImage().getNDims() < 3) {
                        end = 1;
                    } else {
                        end = compImage.getActiveImage().getExtents()[2];
                    }

                    if (allActive) {

                        return;
                    } else {

                        for (int sl = 0; sl < end; sl++) {

                            for (int j = 0; j < VOIs.VOIAt(i).getCurves()[sl].size(); j++) {

                                if (((VOIContour) (VOIs.VOIAt(i).getCurves()[sl].elementAt(j))).isActive()) {
                                    ((VOIContour) (VOIs.VOIAt(i).getCurves()[sl].elementAt(j))).removeActivePt();

                                    compImage.getActiveImage().notifyImageDisplayListeners();

                                    return;
                                }
                            }
                        }
                    }
                }
            }

        }

    }

    /**
     * Deletes all VOIs.
     */
    public void deleteVOIs() {
        compImage.getActiveImage().unregisterAllVOIs();
        fireVOISelectionChange(null);
        voiID = -1;
    }


    /**
     * Cleanup for memory.
     *
     * @param  flag  boolean
     */
    public void disposeLocal(boolean flag) {

        if (popup != null) {
            popup = null;
        }

        if (popupPt != null) {
            popupPt = null;
        }


        if (voiDialog != null) {
            removeVOIUpdateListener(voiDialog); // just in case....
            voiDialog.dispose();
            voiDialog = null;
        }

        if (rbRect != null) {
            rbRect.dispose();
        }

        if (rbPolyline != null) {
            rbPolyline.dispose();
        }

        if (rbEllipse != null) {
            rbEllipse.dispose();
        }

        if (rbLine != null) {
            rbLine.dispose();
        }

        if (rbLivewire != null) {
            rbLivewire.dispose();
        }

        if (rbProtractor != null) {
            rbProtractor.dispose();
        }

        rubberband = null;
        rbRect = rbPolyline = rbEllipse = rbLine = rbProtractor = rbLivewire = null;


        listenerList = null;
    }

    /**
     * Draws the image's VOIs. Called from the paintComponent method
     *
     * @param  graphics  Graphics the graphics context to draw in
     */
    public void drawVOIs(Graphics graphics) {
        ViewVOIVector VOIs = compImage.getActiveImage().getVOIs();

        if (compImage.getOrientation() == FileInfoBase.UNKNOWN_ORIENT) {

            if (VOIs != null) {
                int nVOI = VOIs.size();

                if (compImage.getSlice() != -99) {
                    float originX = (float) compImage.getActiveImage().getFileInfo(0).getOrigin()[0];
                    float originY = (float) compImage.getActiveImage().getFileInfo(0).getOrigin()[1];

                    for (int i = 0; i < nVOI; i++) {
                        VOIs.VOIAt(i).drawSelf(compImage.getZoomX(), compImage.getZoomY(), compImage.getResolutionX(),
                                               compImage.getResolutionY(), originX, originY,
                                               compImage.getActiveImage().getFileInfo(0).getResolutions(),
                                               compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(),
                                               compImage.getSlice(), compImage.getOrientation(),
                                               compImage.getActiveImage().getFileInfo(0),
                                               compImage.getActiveImage().getNDims(), graphics);
                    }
                }
            } // if (VOIs != null)

            if (mode == ViewJComponentEditImage.LEVELSET) {

                if (rbLevelSet.getLevelSetPolygon() != null) {
                    graphics.setColor(Color.yellow);
                    graphics.drawPolygon(zoomPolygon(rbLevelSet.getLevelSetPolygon(),
                                                     compImage.getZoomX() * compImage.getResolutionX(),
                                                     compImage.getZoomY() * compImage.getResolutionY()));
                }
            }

            if (overlayOn) {

                // System.err.println("showing text overlay");
                showOverlay(graphics);
            }

            if (gridOverlayOn) {

                // System.err.println("showing grid overlay");
                showGridOverlay(graphics);
            }
        }
    }


    /**
     * Fires a VOI selection change event based on the VOI and curve.
     *
     * @param  voi    DOCUMENT ME!
     * @param  curve  DOCUMENT ME!
     */
    public void fireVOISelectionChange(VOI voi, VOIBase curve) {

        try {

            // only if there are listeners to send events to should we
            // bother with creating an event and bothering the event queue.
            if (listenerList.getListenerCount(UpdateVOISelectionListener.class) == 0) {
                return;
            }
        } catch (NullPointerException npe) {
            listenerList = new EventListenerList();
            Preferences.debug("Why did we need to make a new listener list??");

            return;
        }

        // always create a new Event, since we need to carry
        // the changed VOI around.
        voiUpdate = new UpdateVOIEvent(this, voi, curve);

        // Guaranteed to return a non-null array
        Object[] listeners = listenerList.getListenerList();

        // Process the listeners last to first, notifying
        // those that are interested in this event
        for (int i = listeners.length - 2; i >= 0; i -= 2) {

            if (listeners[i] == UpdateVOISelectionListener.class) {
                ((UpdateVOISelectionListener) listeners[i + 1]).selectionChanged(voiUpdate);
            }
        }
    }

    /**
     * Get the number of active VOIS.
     *
     * @return  int active VOI number.
     */
    public int getActiveVOICount() {
        int i;
        int nVOI;
        int nActive = 0;
        ViewVOIVector VOIs = compImage.getActiveImage().getVOIs();

        nVOI = VOIs.size();

        for (i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).isActive()) {
                nActive++;
            }
        }

        return nActive;
    }

    /**
     * Gets the anchor point used for calculating VOI movement distances.
     *
     * @return  Point
     */
    public Point getAnchorPt() {
        return this.anchorPt;
    }


    /**
     * Returns the component image to which this class is linked.
     *
     * @return  ViewJComponentEditImage
     */
    public ViewJComponentEditImage getComponentImage() {
        return this.compImage;
    }

    /**
     * get the color of the grid.
     *
     * @return  Color grid color
     */

    public Color getGridColor() {
        return this.gridColor;
    }

    /**
     * Whether or not labels should be drawn on the grid overlay.
     *
     * @return  boolean
     */
    public boolean getGridLabeling() {
        return gridLabelingOn;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean getGridLabelOrientation() {
        return gridLabelOrientation;
    }

    /**
     * returns whether grid overlay is being displayed.
     *
     * @return  boolean is grid overlay on?
     */
    public boolean getGridOverlay() {
        return gridOverlayOn;
    }

    /**
     * returns the grid spacing in terms of resolution.
     *
     * @return  float grid spacing
     */
    public float getGridSpacingX() {
        return gridSpacingX;
    }

    /**
     * returns the grid spacing in terms of resolution.
     *
     * @return  float grid spacing
     */
    public float getGridSpacingY() {
        return gridSpacingY;
    }

    /**
     * Returns the buffer for the image's VOI graph.
     *
     * @return  float[]
     */
    public float[] getImageGraphBuffer() {
        return this.graphImgBuff;
    }

    /**
     * Returns if image/dicom overlay should be shown.
     *
     * @return  boolean is the overlay shown
     */
    public boolean getOverlayOn() {
        return this.overlayOn;
    }

    /**
     * Gets the point intensity buffer.
     *
     * @return  float[] buffer of intensities
     */
    public float[] getPointIntensities() {
        return ptIntensity;
    }

    /**
     * Gets the point position buffer.
     *
     * @return  float[] buffer of positions
     */
    public float[] getPointPositions() {
        return ptPosition;
    }

    /**
     * Gets the RGB Point intensity buffer.
     *
     * @return  float[][] buffer of rgb point intensities [0,1,2]
     */
    public float[][] getPointRGBIntensities() {
        return ptRGBIntensities;
    }

    /**
     * Gets the RGB point position buffer.
     *
     * @return  float[][] buffer of rgb point positions
     */
    public float[][] getPointRGBPositions() {
        return ptRGBPositions;
    }

    /**
     * Gets the popup menu for VOI points.
     *
     * @return  ViewJPopupPt
     */
    public ViewJPopupPt getPopupPt() {
        return this.popupPt;
    }

    /**
     * Gets the VOI Popup menu (used for enabling/disabling and mouse listening).
     *
     * @return  ViewJPopupVOI popup VOI menu
     */
    public ViewJPopupVOI getPopupVOI() {
        return this.popup;
    }

    /**
     * Gets the VOI Rubberband currently in use (or set).
     *
     * @return  Rubberband current VOI rubberband
     */
    public Rubberband getRubberband() {
        return rubberband;
    }


    /**
     * Returns the VOI ID in which to add an ID.
     *
     * @return  VOI ID to add new contour to.
     */
    public int getVOI_ID() {
        return voiID;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Polygon getZoomedLevelSetPolygon() {
        return zoomPolygon(rbLevelSet.getLevelSetPolygon(), compImage.getZoomX(), compImage.getZoomY());
    }

    /**
     * Generates and displays a 1D graph of the average or total intensity of 2.5 VOI of 2.5D image (3D).
     *
     * @param  totalIntensity  if true calculates total sum of the intensity else calculates the average pixel intensity
     * @param  useThreshold    whether or not to threshold this intensity plot
     * @param  threshold       the threshold value to use, if thresholding.
     */
    public void graph25VOI_CalcInten(boolean totalIntensity, boolean useThreshold, float threshold) {
        int i, j, s;
        int nVOI;
        ViewVOIVector VOIs;
        VOI v;
        int length;
        float intensitySum;
        float[] position;
        float[] intensity;
        float[][] rgbPositions;
        float[][] rgbIntensities;
        int numPixels;

        try {
            length = compImage.getActiveImage().getSliceSize();

            if (compImage.getActiveImage().isColorImage() == true) {
                graphImgBuff = new float[length * 4];
            } else {
                graphImgBuff = new float[length];
            }
        } catch (OutOfMemoryError e) {
            graphImgBuff = null;
            System.gc();
            MipavUtil.displayError("Out of memory");

            return;
        }

        if (compImage.getActiveImage().getNDims() == 3) {

            if (compImage.getActiveImage().isColorImage() == true) {

                try {
                    rgbPositions = new float[3][compImage.getActiveImage().getExtents()[2]];
                    rgbIntensities = new float[3][compImage.getActiveImage().getExtents()[2]];

                    VOIs = compImage.getActiveImage().getVOIs();
                    nVOI = VOIs.size();

                    for (i = 0; i < nVOI; i++) {

                        if (VOIs.VOIAt(i).isActive() && (VOIs.VOIAt(i).isVisible() == true)) {
                            v = VOIs.VOIAt(i);

                            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {

                                for (s = 0; s < compImage.getActiveImage().getExtents()[2]; s++) {

                                    try {
                                        compImage.getActiveImage().exportData(s * length * 4, length * 4, graphImgBuff); // locks and releases lock

                                        for (int c = 0; c < 3; c++) {
                                            numPixels = 0;

                                            for (j = 0, intensitySum = 0; j < VOIs.VOIAt(i).getCurves()[s].size();
                                                     j++) {

                                                if (useThreshold) {
                                                    intensitySum += ((VOIContour)
                                                                         (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                        .calcRGBIntensityThreshold(graphImgBuff,
                                                                                                       compImage.getActiveImage().getExtents()[0],
                                                                                                       c, threshold);
                                                } else {
                                                    intensitySum += ((VOIContour)
                                                                         (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                        .calcRGBIntensity(graphImgBuff,
                                                                                              compImage.getActiveImage().getExtents()[0],
                                                                                              c);
                                                }

                                                numPixels += ((VOIContour) (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                 .getLastNumPixels();

                                            }

                                            rgbPositions[c][s] = s;

                                            if (totalIntensity || (numPixels == 0)) {
                                                rgbIntensities[c][s] = intensitySum;
                                            } else {
                                                rgbIntensities[c][s] = intensitySum / numPixels;
                                            }

                                        }
                                    } catch (IOException error) {
                                        MipavUtil.displayError("Image(s) locked");

                                        return;
                                    }
                                }

                                ViewJFrameGraph contourGraph = new ViewJFrameGraph(rgbPositions, rgbIntensities,
                                                                                   "Intensity Graph", v,
                                                                                   FileInfoBase.getUnitsOfMeasureAbbrevStr(compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(2)));

                                contourGraph.setDefaultDirectory(compImage.getActiveImage().getUserInterface().getDefaultDirectory());
                                v.setContourGraph(contourGraph);
                                contourGraph.setVisible(true);
                                v.setTotalIntensity(totalIntensity);
                                v.setRGBPositions(rgbPositions);
                                v.setRGBIntensities(rgbIntensities);

                                return;
                            }
                        }
                    }

                    if (i == nVOI) {
                        MipavUtil.displayError("Please select a contour VOI!");
                    }
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: ComponentEditImage.graphVOI");

                    return;
                }
            } else {

                try {
                    position = new float[compImage.getActiveImage().getExtents()[2]];
                    intensity = new float[compImage.getActiveImage().getExtents()[2]];
                    VOIs = compImage.getActiveImage().getVOIs();
                    nVOI = VOIs.size();

                    for (i = 0; i < nVOI; i++) {

                        if (VOIs.VOIAt(i).isActive() && (VOIs.VOIAt(i).isVisible() == true)) {

                            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                                v = VOIs.VOIAt(i);

                                for (s = 0; s < compImage.getActiveImage().getExtents()[2]; s++) {

                                    try {
                                        numPixels = 0;
                                        compImage.getActiveImage().exportData(s * length, length, graphImgBuff); // locks and releases

                                        // lock

                                        for (j = 0, intensitySum = 0; j < VOIs.VOIAt(i).getCurves()[s].size(); j++) {

                                            if (useThreshold) {
                                                intensitySum += ((VOIContour)
                                                                     (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                    .calcIntensityThreshold(graphImgBuff,
                                                                                                compImage.getActiveImage().getExtents()[0],
                                                                                                threshold);

                                            } else {
                                                intensitySum += ((VOIContour)
                                                                     (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                    .calcIntensity(graphImgBuff,
                                                                                       compImage.getActiveImage().getExtents()[0]);
                                            }

                                            numPixels += ((VOIContour) (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                             .getLastNumPixels();

                                        }

                                        position[s] = s;

                                        if (totalIntensity || (numPixels == 0)) {
                                            intensity[s] = intensitySum;
                                        } else {
                                            intensity[s] = intensitySum / numPixels;
                                        }
                                    } catch (IOException error) {
                                        MipavUtil.displayError("Image(s) locked");

                                        return;
                                    }
                                }

                                ViewJFrameGraph contourGraph = new ViewJFrameGraph(position, intensity,
                                                                                   "Intensity Graph", v,
                                                                                   FileInfoBase.getUnitsOfMeasureAbbrevStr(compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(0)));

                                contourGraph.setDefaultDirectory(ViewUserInterface.getReference().getDefaultDirectory());
                                v.setContourGraph(contourGraph);
                                contourGraph.setVisible(true);
                                v.setTotalIntensity(totalIntensity);
                                v.setPosition(position);
                                v.setIntensity(intensity);

                                return;
                            }
                        }
                    }

                    if (i == nVOI) {
                        MipavUtil.displayError("Please select a contour VOI!");
                    }
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: ComponentEditImage.graphVOI");

                    return;
                }
            }
        } else if (compImage.getActiveImage().getNDims() == 4) {
            int xDim = compImage.getActiveImage().getExtents()[0];
            int yDim = compImage.getActiveImage().getExtents()[1];
            int zDim = compImage.getActiveImage().getExtents()[2];

            try {
                position = new float[compImage.getActiveImage().getExtents()[3]];
                intensity = new float[compImage.getActiveImage().getExtents()[3]];
                VOIs = compImage.getActiveImage().getVOIs();
                nVOI = VOIs.size();

                for (i = 0; i < nVOI; i++) {

                    if (VOIs.VOIAt(i).isActive() && (VOIs.VOIAt(i).isVisible() == true)) {

                        if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                            v = VOIs.VOIAt(i);

                            for (int t = 0; t < compImage.getActiveImage().getExtents()[3]; t++) {

                                try {
                                    numPixels = 0;

                                    for (s = 0, intensitySum = 0; s < compImage.getActiveImage().getExtents()[2]; s++) {
                                        compImage.getActiveImage().exportData((t * xDim * yDim * zDim) +
                                                                              (s * xDim * yDim), length, graphImgBuff); // locks and releases lock

                                        for (j = 0; j < VOIs.VOIAt(i).getCurves()[s].size(); j++) {
                                            intensitySum += ((VOIContour) (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                .calcIntensity(graphImgBuff,
                                                                                   compImage.getActiveImage().getExtents()[0]);
                                            numPixels += ((VOIContour) (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                             .getLastNumPixels();
                                        }
                                    }

                                    position[t] = t;

                                    if (totalIntensity || (numPixels == 0)) {
                                        intensity[t] = intensitySum;
                                    } else {
                                        intensity[t] = intensitySum / numPixels;
                                    }
                                } catch (IOException error) {
                                    MipavUtil.displayError("Image(s) locked");

                                    return;
                                }
                            }

                            ViewJFrameGraph contourGraph = new ViewJFrameGraph(position, intensity, "Intensity Graph",
                                                                               v,
                                                                               FileInfoBase.getUnitsOfMeasureAbbrevStr(compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(0)));

                            contourGraph.setDefaultDirectory(ViewUserInterface.getReference().getDefaultDirectory());
                            v.setContourGraph(contourGraph);
                            contourGraph.setVisible(true);
                            v.setTotalIntensity(totalIntensity);
                            v.setPosition(position);
                            v.setIntensity(intensity);

                            return;
                        }
                    }
                }

                if (i == nVOI) {
                    MipavUtil.displayError("Please select a contour VOI!");
                }
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: ComponentEditImage.graphVOI");

                return;
            }
        }

    }


    /**
     * Generates and displays a 1D graph of the VOI.
     *
     * @param  v      The VOI that the point is from
     * @param  voiPt  The VOIPoint which the graph is to be of
     * @param  j      The number of the point
     */
    public void graphPointVOI(VOI v, VOIPoint voiPt, int j) {
        int t, s;
        Point3Df pt;

        if ((compImage.getActiveImage().getNDims() != 3) && (compImage.getActiveImage().getNDims() != 4)) {
            return;
        }

        if (compImage.getActiveImage().getNDims() == 3) {

            if (compImage.getActiveImage().isColorImage() == true) {
                ptRGBPositions = new float[3][compImage.getActiveImage().getExtents()[2]];
                ptRGBIntensities = new float[3][compImage.getActiveImage().getExtents()[2]];
                pt = voiPt.exportPoint();

                for (s = 0; s < compImage.getActiveImage().getExtents()[2]; s++) {

                    for (int c = 0; c < 3; c++) {
                        ptRGBPositions[c][s] = s;
                        ptRGBIntensities[c][s] = compImage.getActiveImage().getFloat(((4 *
                                                                                           ((s *
                                                                                                 compImage.getActiveImage().getSliceSize()) +
                                                                                                ((int) pt.y *
                                                                                                     compImage.getActiveImage().getExtents()[0]) +
                                                                                                (int) pt.x)) + c + 1));
                    }
                }

                if (v.getContourGraph() == null) {
                    ViewJFrameGraph contourGraph = new ViewJFrameGraph(ptRGBPositions, ptRGBIntensities,
                                                                       "Intensity Graph", v,
                                                                       FileInfoBase.getUnitsOfMeasureAbbrevStr(compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(0)));

                    contourGraph.setDefaultDirectory(compImage.getActiveImage().getUserInterface().getDefaultDirectory());
                    contourGraph.setVisible(false);
                    v.setContourGraph(contourGraph);
                } else {
                    v.getContourGraph().setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(0)));
                    v.getContourGraph().saveNewFunction(ptRGBPositions, ptRGBIntensities, j);
                }

                return;
            } else {

                try {
                    ptPosition = new float[compImage.getActiveImage().getExtents()[2]];
                    ptIntensity = new float[compImage.getActiveImage().getExtents()[2]];

                    for (s = 0; s < compImage.getActiveImage().getExtents()[2]; s++) {

                        pt = voiPt.exportPoint();
                        ptPosition[s] = s;
                        ptIntensity[s] = compImage.getActiveImage().getFloat((int) ((s *
                                                                                         compImage.getActiveImage().getSliceSize()) +
                                                                                    (pt.y *
                                                                                         compImage.getActiveImage().getExtents()[0]) +
                                                                                    pt.x));
                    }

                    if (v.getContourGraph() == null) {
                        ViewJFrameGraph contourGraph = new ViewJFrameGraph(ptPosition, ptIntensity, "Intensity Graph",
                                                                           v,
                                                                           FileInfoBase.getUnitsOfMeasureAbbrevStr(compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(0)));

                        contourGraph.setDefaultDirectory(compImage.getActiveImage().getUserInterface().getDefaultDirectory());
                        contourGraph.setVisible(false);
                        v.setContourGraph(contourGraph);
                    } else {
                        v.getContourGraph().setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(0)));
                        v.getContourGraph().saveNewFunction(ptPosition, ptIntensity, j);
                    }

                    return;
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: ComponentEditImage.graphVOI");

                    return;
                }
            }
        } else if (compImage.getActiveImage().getNDims() == 4) {
            int xDim = compImage.getActiveImage().getExtents()[0];
            int yDim = compImage.getActiveImage().getExtents()[1];
            int zDim = compImage.getActiveImage().getExtents()[2];

            try {
                ptPosition = new float[compImage.getActiveImage().getExtents()[3]];
                ptIntensity = new float[compImage.getActiveImage().getExtents()[3]];

                for (t = 0; t < compImage.getActiveImage().getExtents()[3]; t++) {

                    pt = voiPt.exportPoint();
                    ptPosition[t] = t;
                    ptIntensity[t] = compImage.getActiveImage().getFloat((int) ((t * xDim * yDim * zDim) +
                                                                                (pt.z * xDim * yDim) + (pt.y * xDim) +
                                                                                pt.x));
                }

                if (v.getContourGraph() == null) {
                    ViewJFrameGraph contourGraph = new ViewJFrameGraph(ptPosition, ptIntensity, "Intensity Graph", v,
                                                                       FileInfoBase.getUnitsOfMeasureAbbrevStr(compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(0)));

                    contourGraph.setDefaultDirectory(compImage.getActiveImage().getUserInterface().getDefaultDirectory());
                    contourGraph.setVisible(false);
                    v.setContourGraph(contourGraph);
                } else {
                    v.getContourGraph().setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(0)));
                    v.getContourGraph().saveNewFunction(ptPosition, ptIntensity, j);
                }

                return;

            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: ComponentEditImage.graphVOI");

                return;
            }
        }

    }

    /**
     * Generates and displays a 1D graph of the image intensities underlying the contour of the VOI.
     */
    public void graphVOI() {
        int i, j;
        int nVOI;
        ViewVOIVector VOIs;
        int length;
        float[] position;
        float[] intensity;
        int pt;
        float[][] rgbPositions;
        float[][] rgbIntensities;

        float[][] rgbPos = null;
        float[][] rgbInten = null;

        if (compImage.getActiveImage().isColorImage() == true) {

            try {
                VOIs = compImage.getActiveImage().getVOIs();
                nVOI = VOIs.size();

                for (i = 0; i < nVOI; i++) {

                    if (VOIs.VOIAt(i).isActive() && (VOIs.VOIAt(i).isVisible() == true)) {

                        if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) ||
                                (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {

                            for (j = 0; j < VOIs.VOIAt(i).getCurves()[compImage.getSlice()].size(); j++) {

                                if (((VOIContour) (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j)))
                                        .isActive()) {
                                    length = (int)
                                                 MipavMath.round(((VOIContour)
                                                                      (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j)))
                                                                     .getLengthPtToPt(compImage.getActiveImage().getFileInfo(0).getResolutions()));

                                    if (compImage.getActiveImage().getFileInfo(0).getResolutions()[0] > 0) {
                                        length = (int)
                                                     MipavMath.round(length /
                                                                         compImage.getActiveImage().getFileInfo(0).getResolutions()[0]);
                                    } else {
                                        MipavUtil.displayError("Image resolutions appear to be incorrect!");
                                    }

                                    rgbPositions = new float[3][(length * 2) + 1];
                                    rgbIntensities = new float[3][(length * 2) + 1];

                                    for (int c = 0; c < 3; c++) {
                                        pt = ((VOIContour)
                                                  (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j)))
                                                 .findPositionAndIntensityRGB(rgbPositions[c], rgbIntensities[c], c,
                                                                                  compImage.getActiveImageBuffer(),
                                                                                  compImage.getActiveImage().getResolutions(compImage.getSlice()),
                                                                                  compImage.getActiveImage().getExtents()[0],
                                                                                  compImage.getActiveImage().getExtents()[1]);

                                        if (c == 0) {
                                            rgbPos = new float[3][pt];
                                            rgbInten = new float[3][pt];
                                        }

                                        for (int m = 0; m < pt; m++) {
                                            rgbPos[c][m] = rgbPositions[c][m];
                                            rgbInten[c][m] = rgbIntensities[c][m];
                                        }
                                    }
                                }
                            }

                            ViewJFrameGraph contourGraph = new ViewJFrameGraph(rgbPos, rgbInten, "Intensity Graph");

                            contourGraph.setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(0)));
                            contourGraph.setDefaultDirectory(compImage.getActiveImage().getUserInterface().getDefaultDirectory());
                            contourGraph.setVisible(true);

                            return;
                        } else if ((VOIs.VOIAt(i).getCurveType() == VOI.LINE)) {
                            System.err.println("graphin");
                            graphLineVOI(i, 0);

                            return;
                        }
                    }
                }

                if (i == nVOI) {
                    MipavUtil.displayError("Please select a contour or line VOI!");
                }
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: ComponentEditImage.graphVOI");

                return;
            }
        } else {

            try {
                VOIs = compImage.getActiveImage().getVOIs();
                nVOI = VOIs.size();

                for (i = 0; i < nVOI; i++) {

                    if (VOIs.VOIAt(i).isActive() && (VOIs.VOIAt(i).isVisible() == true)) {

                        if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) ||
                                (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {

                            for (j = 0; j < VOIs.VOIAt(i).getCurves()[compImage.getSlice()].size(); j++) {

                                if (((VOIContour) (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j)))
                                        .isActive()) {
                                    length = (int)
                                                 MipavMath.round(((VOIContour)
                                                                      (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j)))
                                                                     .getLengthPtToPt(compImage.getActiveImage().getFileInfo(0).getResolutions()));
                                    length = (int)
                                                 MipavMath.round(length /
                                                                     compImage.getActiveImage().getFileInfo(0).getResolutions()[0]);

                                    position = new float[(length * 2) + 1];
                                    intensity = new float[(length * 2) + 1];
                                    pt = ((VOIContour) (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j)))
                                             .findPositionAndIntensity(position, intensity,
                                                                           compImage.getActiveImageBuffer(),
                                                                           compImage.getActiveImage().getFileInfo()[compImage.getSlice()].getResolutions(),
                                                                           compImage.getActiveImage().getExtents()[0],
                                                                           compImage.getActiveImage().getExtents()[1]);

                                    float[] pos = new float[pt];
                                    float[] inten = new float[pt];

                                    for (i = 0; i < pt; i++) {
                                        pos[i] = position[i];
                                        inten[i] = intensity[i];
                                    }

                                    ViewJFrameGraph contourGraph = new ViewJFrameGraph(pos, inten, "Contour VOI Graph");

                                    contourGraph.setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(0)));
                                    contourGraph.setDefaultDirectory(compImage.getActiveImage().getUserInterface().getDefaultDirectory());
                                    contourGraph.setVisible(true);

                                    return;
                                }
                            }
                        } else if ((VOIs.VOIAt(i).getCurveType() == VOI.LINE)) {
                            System.err.println("graphin");
                            graphLineVOI(i, 0);

                            return;
                        }
                    }
                }

                if (i == nVOI) {
                    MipavUtil.displayError("Please select a contour or line VOI!");
                }
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: ComponentEditImage.graphVOI");

                return;
            }
        }
    }


    /**
     * Handles VOI manipulation for UP/DOWN/LEFT/RIGHT arrow keys.
     *
     * <p>Arrow key = move active VOI CTRL + left/right = cycle through VOI's contours CTRL + up/down = cycle through
     * VOIs Shift + arrow key = move active contour's active point CTRL + Shift + arrow key = cycle through active
     * contour's active points</p>
     *
     * @param  e  KeyEvent
     */
    public void handleArrowKeysVOI(KeyEvent e) {
        int keyCode = e.getKeyCode();

        if (e.isControlDown() && e.isShiftDown()) {
            handleVOIActivePt(keyCode, false);
        } else if (e.isControlDown()) {
            cycleVOI(keyCode);
        } else if (e.isShiftDown()) {
            handleVOIActivePt(keyCode, true);
        } else {
            moveVOI(keyCode);
        }
    }

    /**
     * Accessor that returns a flag indicating if the rubberband live wire is null or not.
     *
     * @return  whether the rubberband livewire is null (hasn't been initialized)
     */
    public boolean isLivewireNull() {
        return (rbLivewire == null);
    }


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

        // if voiID = -1, then a new VOI is needed
        if (voiID == -1) {
            return true;
        }

        ViewVOIVector VOIs = compImage.getActiveImage().getVOIs();
        int nVOI = VOIs.size();

        for (int i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).getID() == voiID) {

                if (VOIs.VOIAt(i).getCurveType() == voiType) {
                    return false;
                } else {
                    return true;
                }
            }
        }

        // if the current voiID was never matched, then a newVOI
        // needs to be created, so return true
        return true;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  mouseEvent  MouseEvent
     */
    public void mouseClicked(MouseEvent mouseEvent) {
    	if (compImage instanceof ViewJComponentRegistration) {
    		return;
    	}
        int xS, yS;
        xS = compImage.getScaledX(mouseEvent.getX()); // zoomed x.  Used as cursor
        yS = compImage.getScaledY(mouseEvent.getY()); // zoomed y.  Used as cursor

        if ((xS < 0) || (xS >= compImage.getActiveImage().getExtents()[0]) || // Check for validity
                (yS < 0) || (yS >= compImage.getActiveImage().getExtents()[1])) {
            return;
        }

        if ((mouseEvent.getClickCount() == 1) && (mode == compImage.DEFAULT)) {
            selectAllVOIs(false);
            // fireVOISelectionChange(null);
            // lastPointVOI = -1; // next mouseClick will deactivate point VOI unless reselected

            compImage.getActiveImage().notifyImageDisplayListeners();
        }
    }


    /**
     * Mouse handling for mouse being dragged.
     *
     * @param  mouseEvent  MouseEvent the mouse event
     */
    public void mouseDragged(MouseEvent mouseEvent) {
    	if (compImage instanceof ViewJComponentRegistration) {
    		return;
    	}
    	
        int mouseMods = mouseEvent.getModifiers();
        int i, j, m;
        int nVOI;
        ViewVOIVector VOIs = compImage.getActiveImage().getVOIs();
        int xS, yS;
        int distX, distY;
        int xDim, yDim;
        int zDim = 1;
        float[] lineX = new float[2];
        float[] lineY = new float[2];
        float[] lineZ = new float[2];
        float[] position;
        float[] intensity;
        int sliceNum;


        xS = compImage.getScaledX(mouseEvent.getX()); // zoomed x.  Used as cursor
        yS = compImage.getScaledY(mouseEvent.getY()); // zoomed y.  Used as cursor

        xDim = compImage.getActiveImage().getExtents()[0];
        yDim = compImage.getActiveImage().getExtents()[1];

        if (compImage.getActiveImage().getNDims() >= 3) {
            zDim = compImage.getActiveImage().getExtents()[2];
        }

        if ((xS < 0) || (xS >= xDim) || (yS < 0) || (yS >= yDim)) {
            return;
        }

        distX = xS - anchorPt.x; // distance from original to cursor
        distY = yS - anchorPt.y;

        int end = 1;

        nVOI = VOIs.size();

        if (mode == ViewJComponentEditImage.MOVE) {

            for (i = 0; i < nVOI; i++) {
                int curveType = VOIs.VOIAt(i).getCurveType();

                if (VOIs.VOIAt(i).isActive()) {

                    if (((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) ||
                             (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE) ||
                             (VOIs.VOIAt(i).getCurveType() == VOI.LINE) ||
                             (VOIs.VOIAt(i).getCurveType() == VOI.PROTRACTOR) ||
                             (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE_SLICE)) &&
                            (mouseEvent.isControlDown() == false) &&
                            (mouseEvent.getModifiers() != MouseEvent.BUTTON3_MASK)) {


                        wasDragging = true;

                        if (compImage.getActiveImage().getNDims() < 3) {
                            end = 1;
                        } else {
                            end = compImage.getActiveImage().getExtents()[2];
                        }

                        if (allActive) {
                            VOIs.VOIAt(i).moveVOI(compImage.getSlice(), xDim, yDim, zDim, distX, distY, 0);
                            compImage.getActiveImage().notifyImageDisplayListeners();
                        } else {

                            for (int sl = 0; sl < end; sl++) {

                                if (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE_SLICE) {

                                    if ((sl == compImage.getSlice()) && VOIs.VOIAt(i).nearLine(xS, yS, sl)) {

                                        VOIs.VOIAt(i).moveVOI(sl, xDim, yDim, zDim, distX, distY, 0);
                                        compImage.getActiveImage().notifyImageDisplayListeners();
                                    }

                                } else {

                                    for (j = 0; j < VOIs.VOIAt(i).getCurves()[sl].size(); j++) {
                                        boolean contains = false;
                                        boolean isActive = false;

                                        if ((curveType == VOI.CONTOUR) || (curveType == VOI.POLYLINE)) {

                                            contains = ((VOIContour) (VOIs.VOIAt(i).getCurves()[sl].elementAt(j)))
                                                           .contains(xS, yS, true);

                                            isActive = ((VOIContour) (VOIs.VOIAt(i).getCurves()[sl].elementAt(j)))
                                                           .isActive();
                                        } else if (curveType == VOI.LINE) {
                                            contains = ((VOILine) (VOIs.VOIAt(i).getCurves()[sl].elementAt(j)))
                                                           .contains(xS, yS, true);
                                            isActive = ((VOILine) (VOIs.VOIAt(i).getCurves()[sl].elementAt(j)))
                                                           .isActive();
                                        } else if (curveType == VOI.PROTRACTOR) {
                                            contains = ((VOIProtractor) (VOIs.VOIAt(i).getCurves()[sl].elementAt(j)))
                                                           .contains(xS, yS, true);
                                            isActive = ((VOIProtractor) (VOIs.VOIAt(i).getCurves()[sl].elementAt(j)))
                                                           .isActive();
                                        }

                                        if (contains && isActive) {
                                            VOIs.VOIAt(i).moveVOI(sl, xDim, yDim, zDim, distX, distY, 0);
                                            compImage.getActiveImage().notifyImageDisplayListeners();
                                        }
                                    }
                                }
                            }
                        }

                        if ((VOIs.VOIAt(i).getContourGraph() != null) && VOIs.VOIAt(i).getContourGraph().isVisible() &&
                                ((curveType == VOI.CONTOUR) || (curveType == VOI.POLYLINE))) {
                            VOI v;
                            float intensitySum;
                            int length = compImage.getActiveImage().getSliceSize();
                            int s;
                            int numPixels;
                            boolean foundCurve;

                            position = VOIs.VOIAt(i).getPosition();
                            intensity = VOIs.VOIAt(i).getIntensity();

                            float[][] rgbPositions = VOIs.VOIAt(i).getRGBPositions();
                            float[][] rgbIntensities = VOIs.VOIAt(i).getRGBIntensities();

                            if (compImage.getActiveImage().getNDims() == 3) {

                                if (compImage.getActiveImage().isColorImage() == true) {

                                    try {
                                        v = VOIs.VOIAt(i);

                                        for (s = 0, foundCurve = false; s < compImage.getActiveImage().getExtents()[2];
                                                 s++) {

                                            try {

                                                for (int c = 0; c < 3; c++) {
                                                    numPixels = 0;

                                                    for (j = 0, intensitySum = 0;
                                                             j < VOIs.VOIAt(i).getCurves()[s].size(); j++) {

                                                        if (((VOIContour) VOIs.VOIAt(i).getCurves()[s].elementAt(j))
                                                                .isActive() || foundCurve) {

                                                            if (!foundCurve) {
                                                                compImage.getActiveImage().exportData(s * length * 4,
                                                                                                      length * 4,
                                                                                                      graphImgBuff);
                                                            } // locks and releases lock

                                                            intensitySum += ((VOIContour)
                                                                                 (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                                .calcRGBIntensity(graphImgBuff,
                                                                                                      compImage.getActiveImage().getExtents()[0],
                                                                                                      c);
                                                            numPixels += ((VOIContour)
                                                                              (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                             .getLastNumPixels();
                                                            foundCurve = true;
                                                        }
                                                    }

                                                    if (foundCurve) {
                                                        rgbPositions[c][s] = s;

                                                        if (v.getTotalIntensity() || (numPixels == 0)) {
                                                            rgbIntensities[c][s] = intensitySum;
                                                        } else {
                                                            rgbIntensities[c][s] = intensitySum / numPixels;
                                                        }
                                                    }
                                                }
                                            } catch (IOException error) {
                                                MipavUtil.displayError("Image(s) locked");

                                                return;
                                            }

                                            foundCurve = false;
                                        }

                                        VOIs.VOIAt(i).getContourGraph().update(rgbPositions, rgbIntensities, 0);
                                        VOIs.VOIAt(i).getContourGraph().setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(0)));

                                    } catch (OutOfMemoryError error) {
                                        System.gc();
                                        MipavUtil.displayError("Out of memory: ComponentEditImage.graphVOI");

                                        return;
                                    }
                                } else {

                                    try {
                                        v = VOIs.VOIAt(i);

                                        for (s = 0, foundCurve = false; s < compImage.getActiveImage().getExtents()[2];
                                                 s++) {

                                            try {
                                                numPixels = 0;

                                                for (j = 0, intensitySum = 0; j < VOIs.VOIAt(i).getCurves()[s].size();
                                                         j++) {
                                                    boolean isActive = ((VOIContour)
                                                                            (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                           .isActive();

                                                    if (isActive || foundCurve) {

                                                        if (!foundCurve) {
                                                            compImage.getActiveImage().exportData(s * length, length,
                                                                                                  graphImgBuff);
                                                        } // locks and releases lock

                                                        intensitySum += ((VOIContour)
                                                                             (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                            .calcIntensity(graphImgBuff,
                                                                                               compImage.getActiveImage().getExtents()[0]);
                                                        numPixels += ((VOIContour)
                                                                          (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                         .getLastNumPixels();
                                                        foundCurve = true;
                                                    }
                                                }

                                                if (foundCurve) {
                                                    position[s] = s;

                                                    if (v.getTotalIntensity() || (numPixels == 0)) {
                                                        intensity[s] = intensitySum;
                                                    } else {
                                                        intensity[s] = intensitySum / numPixels;
                                                    }

                                                    foundCurve = false;
                                                }
                                            } catch (IOException error) {
                                                MipavUtil.displayError("Image(s) locked");

                                                return;
                                            }
                                        }

                                        VOIs.VOIAt(i).getContourGraph().update(position, intensity, 0);
                                        VOIs.VOIAt(i).getContourGraph().setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(0)));

                                    } catch (OutOfMemoryError error) {
                                        System.gc();
                                        MipavUtil.displayError("Out of memory: ComponentEditImage.graphVOI");

                                        return;
                                    }
                                }
                            } else if (compImage.getActiveImage().getNDims() == 4) {

                                try {
                                    v = VOIs.VOIAt(i);

                                    for (int t = 0; t < compImage.getActiveImage().getExtents()[3]; t++) {

                                        try {
                                            numPixels = 0;

                                            for (s = 0, intensitySum = 0;
                                                     s < compImage.getActiveImage().getExtents()[2]; s++) {
                                                compImage.getActiveImage().exportData((t * xDim * yDim * zDim) +
                                                                                      (s * xDim * yDim), length,
                                                                                      graphImgBuff); // locks and releases lock

                                                for (j = 0; j < VOIs.VOIAt(i).getCurves()[s].size(); j++) {
                                                    intensitySum += ((VOIContour)
                                                                         (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                        .calcIntensity(graphImgBuff,
                                                                                           compImage.getActiveImage().getExtents()[0]);
                                                    numPixels += ((VOIContour)
                                                                      (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                     .getLastNumPixels();

                                                }
                                            }

                                            position[t] = t;

                                            if (v.getTotalIntensity() || (numPixels == 0)) {
                                                intensity[t] = intensitySum;
                                            } else {
                                                intensity[t] = intensitySum / numPixels;
                                            }
                                        } catch (IOException error) {
                                            MipavUtil.displayError("Image(s) locked");

                                            return;
                                        }
                                    }

                                    VOIs.VOIAt(i).getContourGraph().update(position, intensity, 0);
                                    VOIs.VOIAt(i).getContourGraph().setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(0)));
                                } catch (OutOfMemoryError error) {
                                    System.gc();
                                    MipavUtil.displayError("Out of memory: ComponentEditImage.graphVOI");

                                    return;
                                }
                            }
                        }
                    } else if ((VOIs.VOIAt(i).getCurveType() == VOI.POINT) ||
                                   (VOIs.VOIAt(i).getCurveType() == VOI.ANNOTATION)) {
                        compImage.setCursor(compImage.crosshairCursor);
//asdf
                        if (allActive) {
                            VOIs.VOIAt(i).moveVOI(-1, xDim, yDim, zDim, distX, distY, 0);
                        } else {
                            VOIs.VOIAt(i).moveVOI(compImage.getSlice(), xDim, yDim, zDim, distX, distY, 0);
                        }

                        compImage.getActiveImage().notifyImageDisplayListeners();

                        if (VOIs.VOIAt(i).getCurveType() == VOI.POINT) {
                            Point3Df pt;

                            for (j = 0; j < VOIs.VOIAt(i).getCurves()[compImage.getSlice()].size(); j++) {

                                if (((VOIPoint) (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j)))
                                        .isActive()) {
                                    pt = ((VOIPoint) (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j)))
                                             .getActivePt();
                                    compImage.setPixelInformationAtLocation((int) pt.x, (int) pt.y);

                                    if (compImage.getActiveImage().getNDims() == 3) {

                                        if (compImage.getActiveImage().isColorImage() == true) {

                                            for (int s = 0; s < compImage.getActiveImage().getExtents()[2]; s++) {
                                                pt = ((VOIPoint)
                                                          (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j)))
                                                         .exportPoint();

                                                for (int c = 0; c < 3; c++) {

                                                    if ((ptRGBPositions != null) && (ptRGBIntensities != null)) {
                                                        ptRGBPositions[c][s] = s;
                                                        ptRGBIntensities[c][s] = compImage.getActiveImage().getFloat((int)
                                                                                                                     ((4 *
                                                                                                                           ((s *
                                                                                                                                 compImage.getActiveImage().getExtents()[0] *
                                                                                                                                 compImage.getActiveImage().getExtents()[1]) +
                                                                                                                                (pt.y *
                                                                                                                                     compImage.getActiveImage().getExtents()[0]) +
                                                                                                                                pt.x)) +
                                                                                                                      c +
                                                                                                                      1));
                                                    }
                                                }
                                            }

                                            if (VOIs.VOIAt(i).getContourGraph() != null) {
                                                VOIs.VOIAt(i).getContourGraph().update(ptRGBPositions, ptRGBIntensities,
                                                                                       j);
                                                VOIs.VOIAt(i).getContourGraph().setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(0)));
                                            }
                                        } else {

                                            for (int s = 0; s < compImage.getActiveImage().getExtents()[2]; s++) {

                                                pt = ((VOIPoint)
                                                          (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j)))
                                                         .exportPoint();

                                                if ((ptPosition != null) && (ptIntensity != null)) {
                                                    ptPosition[s] = s;
                                                    ptIntensity[s] = compImage.getActiveImage().getFloat((int) ((s *
                                                                                                                     compImage.getActiveImage().getExtents()[0] *
                                                                                                                     compImage.getActiveImage().getExtents()[1]) +
                                                                                                                (pt.y *
                                                                                                                     compImage.getActiveImage().getExtents()[0]) +
                                                                                                                pt.x));
                                                }
                                            }

                                            if (VOIs.VOIAt(i).getContourGraph() != null) {
                                                VOIs.VOIAt(i).getContourGraph().update(ptPosition, ptIntensity, j);
                                                VOIs.VOIAt(i).getContourGraph().setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(0)));
                                            }
                                        }
                                    } else if (compImage.getActiveImage().getNDims() == 4) {

                                        for (int t = 0; t < compImage.getActiveImage().getExtents()[3]; t++) {
                                            pt = ((VOIPoint)
                                                      (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j)))
                                                     .exportPoint();

                                            if ((ptPosition != null) && (ptIntensity != null)) {
                                                ptPosition[t] = t;
                                                ptIntensity[t] = compImage.getActiveImage().getFloat((int) ((t * xDim *
                                                                                                                 yDim *
                                                                                                                 zDim) +
                                                                                                            (pt.z *
                                                                                                                 xDim *
                                                                                                                 yDim) +
                                                                                                            (pt.y *
                                                                                                                 xDim) +
                                                                                                            pt.x));
                                            }
                                        }

                                        if (VOIs.VOIAt(i).getContourGraph() != null) {
                                            VOIs.VOIAt(i).getContourGraph().update(ptPosition, ptIntensity, j);
                                            VOIs.VOIAt(i).getContourGraph().setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(0)));
                                        }
                                    }
                                }
                            }
                        }

                        break;
                    }
                }
            }

            anchorPt.x = xS;
            anchorPt.y = yS;

            return;
        } else if (mode == ViewJComponentEditImage.MOVE_POINT) { // rubberband a point
            // System.err.println("move point");
            xS = compImage.getScaledX(mouseEvent.getX());
            yS = compImage.getScaledY(mouseEvent.getY());

            if (mouseEvent.isShiftDown() == false) {
                Graphics g = compImage.getGraphics();

                for (i = 0; i < nVOI; i++) {

                    if (VOIs.VOIAt(i).isActive() && (VOIs.VOIAt(i).getCurveType() != VOI.POINT)) {

                        // Hides the cursor during dragging so it doesn't get in the way.
                        compImage.setCursor(MipavUtil.blankCursor);

                        VOIs.VOIAt(i).rubberbandVOI(xS, yS, compImage.getSlice(), xDim, yDim, false,
                                                    compImage.getZoomX(), compImage.getZoomY(),
                                                    compImage.getResolutionX(), compImage.getResolutionY(), g);

                        if (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE_SLICE) {
                            Point3Df pt = VOIs.VOIAt(i).exportPSlicePoint(compImage.getSlice());

                            if (pt != null) {
                                compImage.setPixelInformationAtLocation((int) pt.x, (int) pt.y);
                            }

                        } else if (VOIs.VOIAt(i).getCurveType() == VOI.LINE) {

                            if (VOIs.VOIAt(i).getContourGraph() != null) {
                                graphLineVOI(i, 0);

                                break;
                            }
                        } 
                    } // if( VOIs.VOIAt(i).isActive() && VOIs.VOIAt(i).getCurveType() != VOI.POINT)
                } // for (i = 0; i < nVOI; i++)

                compImage.getGraphics().dispose();
            } // end of if (mouseEvent.isShiftDown() == false)
            else { // shift key is depressed

                if (compImage.getActiveImage().getNDims() >= 3) {
                    sliceNum = compImage.getActiveImage().getExtents()[2];
                } else {
                    sliceNum = 1;
                }

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

            for (i = 0; i < nVOI; i++) {

                if (VOIs.VOIAt(i).isActive() && VOIs.VOIAt(i).isVisible()) {

                    if (((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) ||
                             (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) && (mouseEvent.isControlDown() == false) &&
                            (mouseEvent.getModifiers() != MouseEvent.BUTTON3_MASK)) {

                        if ((VOIs.VOIAt(i).getContourGraph() != null) && VOIs.VOIAt(i).getContourGraph().isVisible()) {
                            VOI v;
                            float intensitySum;
                            int length = compImage.getActiveImage().getSliceSize();
                            int s;
                            int numPixels;
                            boolean foundCurve;

                            position = VOIs.VOIAt(i).getPosition();
                            intensity = VOIs.VOIAt(i).getIntensity();

                            float[][] rgbPositions = VOIs.VOIAt(i).getRGBPositions();
                            float[][] rgbIntensities = VOIs.VOIAt(i).getRGBIntensities();

                            if (compImage.getActiveImage().getNDims() == 3) {

                                if (compImage.getActiveImage().isColorImage() == true) {

                                    try {
                                        v = VOIs.VOIAt(i);

                                        for (s = 0, foundCurve = false; s < compImage.getActiveImage().getExtents()[2];
                                                 s++) {

                                            try {

                                                for (int c = 0; c < 3; c++) {
                                                    numPixels = 0;

                                                    for (j = 0, intensitySum = 0;
                                                             j < VOIs.VOIAt(i).getCurves()[s].size(); j++) {

                                                        if (((VOIContour) VOIs.VOIAt(i).getCurves()[s].elementAt(j))
                                                                .isActive() || foundCurve) {

                                                            if (!foundCurve) {
                                                                compImage.getActiveImage().exportData(s * length * 4,
                                                                                                      length * 4,
                                                                                                      graphImgBuff);
                                                            } // locks and releases lock

                                                            intensitySum += ((VOIContour)
                                                                                 (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                                .calcRGBIntensity(graphImgBuff,
                                                                                                      compImage.getActiveImage().getExtents()[0],
                                                                                                      c);
                                                            numPixels += ((VOIContour)
                                                                              (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                             .getLastNumPixels();
                                                            foundCurve = true;
                                                        }
                                                    }

                                                    if (foundCurve) {
                                                        rgbPositions[c][s] = s;

                                                        if (v.getTotalIntensity() || (numPixels == 0)) {
                                                            rgbIntensities[c][s] = intensitySum;
                                                        } else {
                                                            rgbIntensities[c][s] = intensitySum / numPixels;
                                                        }
                                                    }
                                                }
                                            } catch (IOException error) {
                                                MipavUtil.displayError("Image(s) locked");

                                                return;
                                            }

                                            foundCurve = false;
                                        }

                                        VOIs.VOIAt(i).getContourGraph().update(rgbPositions, rgbIntensities, 0);
                                        VOIs.VOIAt(i).getContourGraph().setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(0)));

                                    } catch (OutOfMemoryError error) {
                                        System.gc();
                                        MipavUtil.displayError("Out of memory: ComponentEditImage.graphVOI");

                                        return;
                                    }
                                } else {

                                    try {
                                        v = VOIs.VOIAt(i);

                                        for (s = 0, foundCurve = false; s < compImage.getActiveImage().getExtents()[2];
                                                 s++) {

                                            try {
                                                numPixels = 0;

                                                for (j = 0, intensitySum = 0; j < VOIs.VOIAt(i).getCurves()[s].size();
                                                         j++) {

                                                    if (((VOIContour) VOIs.VOIAt(i).getCurves()[s].elementAt(j))
                                                            .isActive() || foundCurve) {

                                                        if (!foundCurve) {
                                                            compImage.getActiveImage().exportData(s * length, length,
                                                                                                  graphImgBuff);
                                                        } // locks and releases lock

                                                        intensitySum += ((VOIContour)
                                                                             (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                            .calcIntensity(graphImgBuff,
                                                                                               compImage.getActiveImage().getExtents()[0]);
                                                        numPixels += ((VOIContour)
                                                                          (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                         .getLastNumPixels();
                                                        foundCurve = true;
                                                    }
                                                }

                                                if (foundCurve) {
                                                    position[s] = s;

                                                    if (v.getTotalIntensity() || (numPixels == 0)) {
                                                        intensity[s] = intensitySum;
                                                    } else {
                                                        intensity[s] = intensitySum / numPixels;
                                                    }

                                                    foundCurve = false;
                                                }
                                            } catch (IOException error) {
                                                MipavUtil.displayError("Image(s) locked");

                                                return;
                                            }
                                        }

                                        VOIs.VOIAt(i).getContourGraph().update(position, intensity, 0);
                                        VOIs.VOIAt(i).getContourGraph().setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(0)));

                                    } catch (OutOfMemoryError error) {
                                        System.gc();
                                        MipavUtil.displayError("Out of memory: ComponentEditImage.graphVOI");

                                        return;
                                    }
                                }
                            } else if (compImage.getActiveImage().getNDims() == 4) {

                                try {
                                    v = VOIs.VOIAt(i);

                                    for (int t = 0; t < compImage.getActiveImage().getExtents()[3]; t++) {

                                        try {
                                            numPixels = 0;

                                            for (s = 0, intensitySum = 0;
                                                     s < compImage.getActiveImage().getExtents()[2]; s++) {
                                                compImage.getActiveImage().exportData((t * xDim * yDim * zDim) +
                                                                                      (s * xDim * yDim), length,
                                                                                      graphImgBuff); // locks and releases lock

                                                for (j = 0; j < VOIs.VOIAt(i).getCurves()[s].size(); j++) {
                                                    intensitySum += ((VOIContour)
                                                                         (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                        .calcIntensity(graphImgBuff,
                                                                                           compImage.getActiveImage().getExtents()[0]);
                                                    numPixels += ((VOIContour)
                                                                      (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                     .getLastNumPixels();

                                                }
                                            }

                                            position[t] = t;

                                            if (v.getTotalIntensity() || (numPixels == 0)) {
                                                intensity[t] = intensitySum;
                                            } else {
                                                intensity[t] = intensitySum / numPixels;
                                            }
                                        } catch (IOException error) {
                                            MipavUtil.displayError("Image(s) locked");

                                            return;
                                        }
                                    }

                                    VOIs.VOIAt(i).getContourGraph().update(position, intensity, 0);
                                    VOIs.VOIAt(i).getContourGraph().setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(0)));
                                } catch (OutOfMemoryError error) {
                                    System.gc();
                                    MipavUtil.displayError("Out of memory: ComponentEditImage.graphVOI");

                                    return;
                                }
                            }
                        }
                    }
                }
            }

            return;
        } else if (mode == ViewJComponentEditImage.RETRACE) {
            Graphics g = compImage.getGraphics();

            for (i = 0; i < nVOI; i++) {

                if (VOIs.VOIAt(i).isActive()) {
                    ((VOIContour) (VOIs.VOIAt(i).getActiveContour(compImage.getSlice()))).retraceContour(compImage.getZoomX(),
                                                                                                         compImage.getZoomY(),
                                                                                                         compImage.getResolutionX(),
                                                                                                         compImage.getResolutionY(),
                                                                                                         compImage.getActiveImage().getFileInfo(0).getResolutions(),
                                                                                                         xS, yS, g);
                    compImage.setMode(ViewJComponentEditImage.RETRACE);

                    break;
                }
            }

            g.dispose();

            return;
        } else if (mode == compImage.PAINT_VOI) {
            compImage.performPaint(mouseEvent, mouseMods == MouseEvent.BUTTON3_MASK);
            compImage.getActiveImage().notifyImageDisplayListeners();
        }
    }

    /**
     * Does nothing.
     *
     * @param  mouseEvent  MouseEvent
     */
    public void mouseEntered(MouseEvent mouseEvent) { }

    /**
     * Does nothing.
     *
     * @param  mouseEvent  MouseEvent
     */
    public void mouseExited(MouseEvent mouseEvent) { }

    /**
     * Mouse handling for Mouse Moved.
     *
     * @param  mouseEvent  MouseEvent
     */
    public void mouseMoved(MouseEvent mouseEvent) {
        int i, j;
        int x, y;
        int xS, yS;
        int nVOI;
        ViewVOIVector VOIs;
        int nCurves;
        Vector[] curves;
        Graphics g = compImage.getGraphics();

        if ((mode == compImage.ZOOMING_IN) || (mode == compImage.ZOOMING_OUT)) {

            // if we are in zoom mode, we don't care about any of the other things
            // that are happening here, in fact, zoom breaks if we don't return
            return;
        }

        xS = compImage.getScaledX(mouseEvent.getX()); // zoomed x.  Used as cursor
        yS = compImage.getScaledY(mouseEvent.getY()); // zoomed y.  Used as cursor

        if ((mode == ViewJComponentEditImage.PAINT_VOI) && mouseEvent.isShiftDown()) {
            compImage.performPaint(mouseEvent, false);
            compImage.getActiveImage().notifyImageDisplayListeners(null, true);

            return;
        }

        compImage.removeMouseListener(popup);
        compImage.removeMouseListener(popupPt);

        if ((compImage.getGraphics() == null) || (compImage.getModifyFlag() == false) ||
                (compImage.getSlice() == -99)) {
            return;
        }

        if ((compImage.getPixBuffer() == null) || (compImage.getActiveImageBuffer() == null)) {
            compImage.getGraphics().dispose();

            return;
        }

        // System.err.println("got here....more stuff");
        x = mouseEvent.getX();
        y = mouseEvent.getY();

        if ((xS < 0) || (xS >= compImage.getActiveImage().getExtents()[0]) || // Check to ensure point is within
                (yS < 0) || (yS >= compImage.getActiveImage().getExtents()[1])) { // the image bounds
            compImage.getGraphics().dispose();

            return;
        }

        // System.err.println("got here....past graphics dispose for extents prob");

        if (mode == ViewJComponentEditImage.MAG_REGION) {
            compImage.repaint();

            return;
        } else if (mode == ViewJComponentEditImage.WIN_REGION) {
            compImage.repaint();

            return;
        } else if ((mode == ViewJComponentEditImage.PAINT_VOI) || (mode == ViewJComponentEditImage.ERASER_PAINT)) {
        	
            // repaint();
            System.err.println("calling compImage.paintComponent from VOIHandler mouseMoved");
            compImage.paintComponent(g);

            return;
        }

        if ((mode == ViewJComponentEditImage.RECTANGLE) || (mode == ViewJComponentEditImage.ELLIPSE) ||
                (mode == ViewJComponentEditImage.LINE) || (mode == ViewJComponentEditImage.RECTANGLE3D) ||
                (mode == ViewJComponentEditImage.POINT_VOI) || (mode == ViewJComponentEditImage.POLYLINE) ||
                (mode == ViewJComponentEditImage.LEVELSET) || (mode == ViewJComponentEditImage.PAINT_VOI) ||
                (mode == ViewJComponentEditImage.DROPPER_PAINT) || (mode == ViewJComponentEditImage.ERASER_PAINT) ||
                (mode == ViewJComponentEditImage.QUICK_LUT) || (mode == ViewJComponentEditImage.PROTRACTOR) ||
                (mode == ViewJComponentEditImage.LIVEWIRE) || (mode == ViewJComponentEditImage.ANNOTATION) ||
                (mode == ViewJComponentEditImage.POLYLINE_SLICE_VOI) || (mode == ViewJComponentEditImage.PAINT_CAN) ||
                (mode == ViewJComponentBase.TRANSLATE) || (mode == ViewJComponentBase.ROTATE)) {
            compImage.getGraphics().dispose();
            // System.err.println("returning from mouseMoved() with rectangle of VOIHandler");
            // System.err.println("returning from mouseMoved with mode of: " + mode);
            return;
        }

        VOIs = compImage.getActiveImage().getVOIs(); // Get the VOIs from the active image.
        nVOI = 0;

        if (VOIs != null) {
            nVOI = VOIs.size();

            for (i = 0; i < nVOI; i++) {

                if (VOIs.VOIAt(i).isActive() && VOIs.VOIAt(i).isVisible()) {
                    curves = VOIs.VOIAt(i).getCurves();
                    nCurves = curves[compImage.getSlice()].size();

                    if (mouseEvent.isShiftDown() &&
                            VOIs.VOIAt(i).nearPoint(x, y, compImage.getSlice(), compImage.getZoomX(),
                                                        compImage.getResolutionX(), compImage.getResolutionY())) {

                        if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) ||
                                (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {

                            // System.err.println("IN HERE checkin for NEAR POINT with shift down");
                            for (j = 0; j < nCurves; j++) {

                                if (VOIs.VOIAt(i).nearPoint(x, y, compImage.getSlice(), j, compImage.getZoomX(),
                                                                compImage.getResolutionX(),
                                                                compImage.getResolutionY())) {
                                    break;
                                }
                            }

                            if (j == nCurves) {
                                return;
                            }

                            // System.err.println("setting mode to DELETE_POINT");
                            // BEN LINK CHANGE can delete point from bounding box'd contour
                            // if ( VOIs.VOIAt( i ).getBoundingBoxFlag() == false ) {
                            compImage.setMode(ViewJComponentEditImage.DELETE_POINT);

                            // } else { compImage.setMode( MOVE_POINT ); } Displays contour with the active point
                            // highlighted VOIs.VOIAt(i).drawVertices(compImage.getZoomX(), getZoomY(),
                            // compImage.getResolutionX(), compImage.getResolutionY(), compImage.getSlice(), g);
                            VOIs.VOIAt(i).drawVertices(compImage.getZoomX(), compImage.getZoomY(),
                                                       compImage.getResolutionX(), compImage.getResolutionY(),
                                                       compImage.getActiveImage().getFileInfo(0).getResolutions(),
                                                       compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(),
                                                       compImage.getSlice(), compImage.getOrientation(), g, j);
                            compImage.getGraphics().dispose();

                            return;
                        }
                    } else if (VOIs.VOIAt(i).nearPoint(x, y, compImage.getSlice(), compImage.getZoomX(),
                                                           compImage.getResolutionX(), compImage.getResolutionY()) &&
                                   (VOIs.VOIAt(i).getCurveType() != VOI.POINT) &&
                                   (VOIs.VOIAt(i).getCurveType() != VOI.PROTRACTOR) &&
                                   (mouseEvent.isShiftDown() == false) && (mouseEvent.isAltDown() == false)) {

                        if (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE_SLICE) {

                            // System.err.println("setting polyslice to move point mode");
                            compImage.setMode(ViewJComponentEditImage.MOVE_POINT);
                            compImage.getGraphics().dispose();

                            return;
                        } else if (VOIs.VOIAt(i).getCurveType() == VOI.ANNOTATION) {
                        	compImage.setMode(ViewJComponentEditImage.MOVE_POINT);
                            compImage.getGraphics().dispose();
                            return;
                        }
                        else {

                            // System.err.println("gotta be a line eh?");
                            for (j = 0; j < nCurves; j++) {

                                if (VOIs.VOIAt(i).nearPoint(x, y, compImage.getSlice(), j, compImage.getZoomX(),
                                                                compImage.getResolutionX(),
                                                                compImage.getResolutionY())) {
                                    break;
                                }
                            }

                            if (j == nCurves) {
                                return;
                            }

                            compImage.setMode(ViewJComponentEditImage.MOVE_POINT);

                            // don't bother redrawing for a line ... point is not highlighted
                            if (VOIs.VOIAt(i).getCurveType() != VOI.LINE) {

                                // Displays contour with the active point highlighted
                                VOIs.VOIAt(i).drawVertices(compImage.getZoomX(), compImage.getZoomY(),
                                                           compImage.getResolutionX(), compImage.getResolutionY(),
                                                           compImage.getActiveImage().getFileInfo(0).getResolutions(),
                                                           compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(),
                                                           compImage.getSlice(), compImage.getOrientation(), g, j);
                            }

                            compImage.getGraphics().dispose();
                            mouseEvent.consume();

                            return;
                        }
                    } else if (VOIs.VOIAt(i).getCurveType() == VOI.PROTRACTOR) {

                        for (j = 0; j < nCurves; j++) {

                            if (VOIs.VOIAt(i).nearOuterPoint(x, y, compImage.getSlice(), j, compImage.getZoomX(),
                                                                 compImage.getResolutionX(),
                                                                 compImage.getResolutionY()) &&
                                    (mouseEvent.isShiftDown() == false) && (mouseEvent.isAltDown() == false)) {
                                compImage.setMode(ViewJComponentEditImage.MOVE_POINT);
                                VOIs.VOIAt(i).drawSelf(compImage.getZoomX(), compImage.getZoomY(),
                                                       compImage.getResolutionX(), compImage.getResolutionY(), 0f, 0f,
                                                       compImage.getActiveImage().getFileInfo(0).getResolutions(),
                                                       compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(),
                                                       compImage.getSlice(), compImage.getOrientation(), g);
                                compImage.getGraphics().dispose();

                                return;
                            }
                        }
                    }
                    // see if the mouse is near the line in between the points of the contour/polyline/polyline_slice
                    else if ((VOIs.VOIAt(i)).nearLine(xS, yS, compImage.getSlice()) &&
                                 (mouseEvent.isAltDown() == false)) {

                        if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) ||
                                (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {

                            for (j = 0; j < nCurves; j++) {

                                if (((VOIContour) (curves[compImage.getSlice()].elementAt(j))).isActive() &&
                                        ((VOIContour) (curves[compImage.getSlice()].elementAt(j))).nearLine(xS, yS)) {
                                    break;
                                }
                            }

                            if (j == nCurves) {
                                return;
                            }

                            compImage.setMode(ViewJComponentEditImage.NEW_POINT);
                            compImage.getGraphics().dispose();

                            return;
                        } else if ((VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE_SLICE) && mouseEvent.isShiftDown()) {
                            compImage.setMode(ViewJComponentEditImage.NEW_POINT);
                            compImage.getGraphics().dispose();

                            return;
                        }
                    } else if (mouseEvent.isAltDown() == true) {

                        if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) ||
                                (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
                            compImage.setMode(ViewJComponentEditImage.RETRACE);
                            compImage.getGraphics().dispose();

                            return;
                        }
                    }
                } // end of if( VOIs.VOIAt(i).isActive() && VOIs.VOIAt(i).isVisible() && VOIs.VOIAt(i).getShowUp())
            } // end of for (i = 0; i < nVOI; i++) {
        } // if (VOIs != null)


        for (i = 0; i < nVOI; i++) {
            int curveType = VOIs.VOIAt(i).getCurveType();

            if ((curveType == VOI.CONTOUR) || (curveType == VOI.POLYLINE) || (curveType == VOI.PROTRACTOR) ||
                    (curveType == VOI.LINE) || (curveType == VOI.ANNOTATION)) {
                curves = ((VOI) (VOIs.elementAt(i))).getCurves();

                for (j = 0; j < curves[compImage.getSlice()].size(); j++) {
                    boolean isContained = false;

                    if ((curveType == VOI.CONTOUR) || (curveType == VOI.POLYLINE)) {
                        isContained = ((VOIContour) (curves[compImage.getSlice()].elementAt(j))).contains(xS, yS,
                                                                                                          false);
                    } else if (curveType == VOI.PROTRACTOR) {
                        isContained = ((VOIProtractor) (curves[compImage.getSlice()].elementAt(j))).contains(xS, yS,
                                                                                                             false);
                        ((VOIProtractor) (curves[compImage.getSlice()].elementAt(j))).setShowLength(true);
                    } else if (curveType == VOI.LINE) {
                        isContained = ((VOILine) (curves[compImage.getSlice()].elementAt(j))).contains(xS, yS, false);
                    } else if (curveType == VOI.ANNOTATION) {
                        isContained = ((VOIText) (curves[compImage.getSlice()].elementAt(j))).contains(xS, yS,
                                                                                                       compImage.getZoomX(),
                                                                                                       compImage.getZoomY(),
                                                                                                       compImage.getActiveImage().getFileInfo(0).getResolutions(),
                                                                                                       g);
                    }

                    if (VOIs.VOIAt(i).isVisible() && isContained) {
                        compImage.setMode(ViewJComponentEditImage.MOVE);

                        if ((curveType == VOI.CONTOUR) || (curveType == VOI.POLYLINE)) {
                            popup.setEnabledProps(true);
                            compImage.addMouseListener(popup);
                        }

                        compImage.getGraphics().dispose();
                        mouseEvent.consume();

                        // System.err.println("consumed event for isContained of VOIHandler");
                        return;
                    }
                }
            } else if (VOIs.VOIAt(i).getCurveType() == VOI.POINT) {

                if (VOIs.VOIAt(i).isVisible() &&
                        VOIs.VOIAt(i).nearPoint(x, y, compImage.getSlice(), compImage.getZoomX(),
                                                    compImage.getResolutionX(), compImage.getResolutionY())) {
                    compImage.setMode(ViewJComponentEditImage.MOVE);

                    compImage.addMouseListener(popupPt);

                    compImage.getGraphics().dispose();

                    return;
                }
            } else if (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE_SLICE) {

                if (VOIs.VOIAt(i).isVisible() &&
                        (VOIs.VOIAt(i).nearPoint(x, y, compImage.getSlice(), compImage.getZoomX(),
                                                     compImage.getResolutionX(), compImage.getResolutionY()) ||

                             // VOIs.VOIAt(i).nearLinePoint(
                             VOIs.VOIAt(i).nearLine(xS, yS, compImage.getSlice()))) {
                    compImage.setMode(ViewJComponentEditImage.MOVE);

                    popup.setEnabledProps(true);
                    compImage.addMouseListener(popup);

                    compImage.getGraphics().dispose();

                    return;

                }
            }
        }

        compImage.setMode(ViewJComponentEditImage.DEFAULT);
        
    } // end mouseMoved


    /**
     * Mouse handling for Mouse Pressed.
     *
     * @param  mouseEvent  MouseEvent the mouse event
     */
    public void mousePressed(MouseEvent mouseEvent) {
    	if (compImage instanceof ViewJComponentRegistration) {
    		return;
    	}
        int xS, yS;
        int x, y;

        int i, j;
        int nVOI;
        ViewVOIVector VOIs;

        if (compImage.getModifyFlag() == false) {
            return;
        }

        // save the state of the shift button
        mousePressIsShiftDown = mouseEvent.isShiftDown();

        if (mode != ViewJComponentEditImage.DEFAULT) {
            VOIs = compImage.getActiveImage().getVOIs();
            nVOI = VOIs.size();

            // if (nVOI == 0) return;
            for (i = 0; i < nVOI; i++) {

                if (VOIs.VOIAt(i).isActive() == true) {
                    copyVOIforUndo();

                    break;
                }
            }
        }

        try {

            xS = compImage.getScaledX(mouseEvent.getX()); // zoomed x.  Used as cursor
            yS = compImage.getScaledY(mouseEvent.getY()); // zoomed y.  Used as cursor

            x = mouseEvent.getX();
            y = mouseEvent.getY();

            if ((xS < 0) || (xS >= compImage.getActiveImage().getExtents()[0]) || (yS < 0) ||
                    (yS >= compImage.getActiveImage().getExtents()[1])) {
                return;
            }

            if (mode == ViewJComponentEditImage.DROPPER_PAINT) {
                // shouldnt be here
            }

            if (mode == ViewJComponentEditImage.ERASER_PAINT) {
                // shouldnt be here
            } else if (mode == ViewJComponentEditImage.PAINT_VOI) {
                // shouldnt be here
            }

            if ((mode == ViewJComponentEditImage.MAG_REGION) &&
                    (mouseEvent.getModifiers() == MouseEvent.BUTTON3_MASK)) {
                // shouldnt be here
            }

            if ((mode == ViewJComponentEditImage.WIN_REGION) &&
                    (mouseEvent.getModifiers() == MouseEvent.BUTTON3_MASK)) {
                // shouldnt be here
            }

            if ((mouseEvent.getModifiers() & MouseEvent.BUTTON3_MASK) != 0) {
                VOIs = (ViewVOIVector) compImage.getActiveImage().getVOIs();
                nVOI = VOIs.size();

                for (i = 0; i < nVOI; i++) {

                    if (VOIs.VOIAt(i).isActive() && (VOIs.VOIAt(i).isVisible() == true)) {

                        if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {

                            for (j = 0; j < VOIs.VOIAt(i).getCurves()[compImage.getSlice()].size(); j++) {

                                if (((VOIContour) (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j)))
                                        .contains(xS, yS, true)) {
                                    return;
                                }
                            }
                        }

                        if (VOIs.VOIAt(i).getCurveType() == VOI.LINE) {

                            for (j = 0; j < VOIs.VOIAt(i).getCurves()[compImage.getSlice()].size(); j++) {

                                if (((VOILine) (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j))).nearLine(xS,
                                                                                                                            yS)) {

                                    // if CTRL was held down, then print out the Line's information to the
                                    // DATA window
                                    if ((mouseEvent.getModifiers() &
                                             (MouseEvent.CTRL_DOWN_MASK | MouseEvent.CTRL_MASK)) != 0) {
                                        Point3Df startPt = null;
                                        Point3Df endPt = null;

                                        startPt = (Point3Df)
                                                      ((VOILine)
                                                           (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j)))
                                                          .elementAt(0);
                                        endPt = (Point3Df)
                                                    ((VOILine)
                                                         (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j)))
                                                        .elementAt(1);

                                        double angle = ((VOILine)
                                                            (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j)))
                                                           .getAngle();

                                        String tmpString2 = String.valueOf(angle);
                                        i = tmpString2.indexOf('.');

                                        if (tmpString2.length() >= (i + 3)) {
                                            tmpString2 = tmpString2.substring(0, i + 3);
                                        }

                                        tmpString2 += " deg";


                                        if ((compImage.getActiveImage().getFileInfo(0).getOrigin()[0] != 0) ||
                                                (compImage.getActiveImage().getFileInfo(0).getOrigin()[1] != 0) ||
                                                (compImage.getActiveImage().getFileInfo(0).getOrigin()[2] != 0)) {
                                            FileInfoBase fileInfo = compImage.getActiveImage().getFileInfo()[compImage.getSlice()];
                                            String[] startValues = compImage.setScannerPosition((int) startPt.x,
                                                                                                (int) startPt.y,
                                                                                                compImage.getSlice());
                                            String[] endValues = compImage.setScannerPosition((int) endPt.x,
                                                                                              (int) endPt.y,
                                                                                              compImage.getSlice());

                                            if ((startValues != null) && (endValues != null)) {
                                                ViewUserInterface.getReference().setDataText("Line: " + tmpString2 +
                                                                                             ", Position(start): " +
                                                                                             startValues[0] + " " +
                                                                                             startValues[1] + " " +
                                                                                             startValues[2] +
                                                                                             ", Position(end): " +
                                                                                             endValues[0] + " " +
                                                                                             endValues[1] + " " +
                                                                                             endValues[2] + "\n");
                                            } else {
                                                ViewUserInterface.getReference().setDataText("Line: " + tmpString2 +
                                                                                             ", Position(start): " +
                                                                                             (int) (startPt.x + 1) +
                                                                                             " " +
                                                                                             (int) (startPt.y + 1) +
                                                                                             ", Position(end): " +
                                                                                             (int) (endPt.x + 1) + " " +
                                                                                             (int) (endPt.y + 1) +
                                                                                             "\n");

                                            }
                                        } else {
                                            ViewUserInterface.getReference().setDataText("Line: " + tmpString2 +
                                                                                         ", Position(start): " +
                                                                                         (int) (startPt.x + 1) + " " +
                                                                                         (int) (startPt.y + 1) +
                                                                                         ", Position(end): " +
                                                                                         (int) (endPt.x + 1) + " " +
                                                                                         (int) (endPt.y + 1) + "\n");
                                        }

                                        // do not pop up graph here, instead return
                                        return;
                                    } else {
                                        graphLineVOI(i, j);
                                    }

                                }
                            }

                        } else if (VOIs.VOIAt(i).getCurveType() == VOI.POINT) {

                            if (VOIs.VOIAt(i).nearPoint(x, y, compImage.getSlice(), compImage.getZoomX(),
                                                            compImage.getResolutionX(), compImage.getResolutionY())) {
                                compImage.addMouseListener(popupPt);
                            }
                        }
                    }
                }
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentEditImage.mousePressed");
            compImage.setMode(ViewJComponentEditImage.DEFAULT);

            return;
        }

        if (mode == ViewJComponentEditImage.MOVE) {
            anchorPt.setLocation(xS, yS); // For use in dragging VOIs

            // the actual selecting was moved to mouseReleased()
        }
    }


    /**
     * A mouse event. This function sets up and draws the VOI according to the mode.
     *
     * @param  mouseEvent  event that triggered function
     */
    public void mouseReleased(MouseEvent mouseEvent) {
    	if (compImage instanceof ViewJComponentRegistration) {
    		return;
    	}
        int i, j, k;
        int nVOI;
        ViewVOIVector VOIs = compImage.getActiveImage().getVOIs();

        if (wasDragging) {
            wasDragging = false;

            return;
        }

        if (compImage.getModifyFlag() == false) {
            return;
        }

        int xS = compImage.getScaledX(mouseEvent.getX()); // zoomed x.  Used as cursor
        int yS = compImage.getScaledY(mouseEvent.getY()); // zoomed y.  Used as cursor
        int xR = mouseEvent.getX();
        int yR = mouseEvent.getY();

        int xDim = compImage.getActiveImage().getExtents()[0];
        int yDim = compImage.getActiveImage().getExtents()[1];

        if ((xS < 0) || (xS >= compImage.getActiveImage().getExtents()[0]) || (yS < 0) ||
                (yS >= compImage.getActiveImage().getExtents()[1])) {
            return;
        }

        if (mode != ViewJComponentEditImage.MOVE) {
            compImage.setPixelInformationAtLocation(xS, yS);
        }

        // clicking with the right mouse button in a regular image frame updates the image's
        // tri-image frame (if one is open) to show that point in all of the components
        if ((mouseEvent.getModifiers() & InputEvent.BUTTON2_MASK) != 0) {
            ViewJFrameTriImage triFrame = compImage.getActiveImage().getTriImageFrame();

            if (triFrame != null) {
                triFrame.setSlicesFromFrame(xS, yS, compImage.getSlice());
            }
        }

        if (mode == ViewJComponentEditImage.POINT_VOI) {

            if ((mouseEvent.getModifiers() & InputEvent.BUTTON1_MASK) != 0) {

                if (isNewVoiNeeded(VOI.POINT)) { // create new VOI

                    VOI newPtVOI = null;

                    try {
                        float[] x = new float[1];
                        float[] y = new float[1];
                        float[] z = new float[1];

                        voiID = compImage.getActiveImage().getVOIs().size();

                        int colorID = 0;

                        if (compImage.getActiveImage().getVOIs().size() > 0) {
                            colorID = ((VOI) (compImage.getActiveImage().getVOIs().lastElement())).getID() + 1;
                        }

                        if (compImage.getActiveImage().getNDims() > 2) {

                            newPtVOI = new VOI((short) colorID, "point3D.voi",
                                               compImage.getActiveImage().getExtents()[2], VOI.POINT, -1.0f);
                        } else {
                            newPtVOI = new VOI((short) colorID, "point2d.voi", 1, VOI.POINT, -1.0f);
                        }

                        x[0] = xS;
                        y[0] = yS;
                        z[0] = compImage.getSlice();
                        newPtVOI.importCurve(x, y, z, compImage.getSlice());
                        newPtVOI.setUID(newPtVOI.hashCode());

                    } catch (OutOfMemoryError error) {
                        System.gc();
                        MipavUtil.displayError("Out of memory: ComponentEditImage.mouseReleased");
                        compImage.setMode(ViewJComponentEditImage.DEFAULT);

                        return;
                    }

                    // lastPointVOI = voiID;
                    compImage.getActiveImage().registerVOI(newPtVOI);
                    newPtVOI.setActive(true);

                    updateVOIColor(newPtVOI.getColor(), newPtVOI.getUID());
                    ((VOIPoint) (VOIs.VOIAt(voiID).getCurves()[compImage.getSlice()].elementAt(0))).setActive(true);

                    compImage.getActiveImage().notifyImageDisplayListeners();

                    graphPointVOI(newPtVOI,
                                  ((VOIPoint) (VOIs.VOIAt(voiID).getCurves()[compImage.getSlice()].elementAt(0))), 0);

                    if (!((mouseEvent.isShiftDown() == true) ||
                              Preferences.is(Preferences.PREF_CONTINUOUS_VOI_CONTOUR))) {
                        compImage.setMode(ViewJComponentEditImage.DEFAULT);
                    }

                } // end of if (voiID == -1)
                else { // voiID != -1 add point to existing VOI

                    int index;

                    nVOI = VOIs.size();

                    float[] x = new float[1];
                    float[] y = new float[1];
                    float[] z = new float[1];

                    x[0] = xS;
                    y[0] = yS;
                    z[0] = compImage.getSlice();

                    for (i = 0; i < nVOI; i++) {

                        if (VOIs.VOIAt(i).getID() == voiID) {

                            if (VOIs.VOIAt(i).getCurveType() == VOI.POINT) {
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

                    if (!((VOIs.VOIAt(i).getContourGraph() != null) &&
                              (compImage.getActiveImage().isColorImage() == true))) {
                        graphPointVOI(VOIs.VOIAt(i),
                                      ((VOIPoint)
                                       (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(index - 1))),
                                      index - 1);
                    }

                    if (!((mouseEvent.isShiftDown() == true) ||
                              Preferences.is(Preferences.PREF_CONTINUOUS_VOI_CONTOUR))) {
                        compImage.setMode(ViewJComponentEditImage.DEFAULT);
                    }

                    return;
                } // end of else for if voiID != -1 add point to existing VOI
            } // end of if ((mouseEvent.getModifiers() & mouseEvent.BUTTON1_MASK) != 0)
        } // end of else if (mode == POINT_VOI)
        else if (mode == ViewJComponentEditImage.POLYLINE_SLICE_VOI) {

            if ((mouseEvent.getModifiers() & InputEvent.BUTTON1_MASK) != 0) {

                if (isNewVoiNeeded(VOI.POLYLINE_SLICE)) { // create new VOI

                    try {
                        float[] x = new float[1];
                        float[] y = new float[1];
                        float[] z = new float[1];

                        voiID = compImage.getActiveImage().getVOIs().size();

                        int colorID = 0;

                        if (compImage.getActiveImage().getVOIs().size() > 0) {
                            colorID = ((VOI) (compImage.getActiveImage().getVOIs().lastElement())).getID() + 1;
                        }

                        if (compImage.getActiveImage().getNDims() > 2) {
                            newPolySliceVOI = new VOI((short) colorID, "Polyline_slice",
                                                      compImage.getActiveImage().getExtents()[2], VOI.POLYLINE_SLICE,
                                                      -1.0f);
                        } else {
                            MipavUtil.displayError("Inter-frame polyline must be used on 2.5/3D images only");

                            return;
                        }

                        x[0] = xS;
                        y[0] = yS;
                        z[0] = compImage.getSlice();
                        newPolySliceVOI.importCurve(x, y, z, compImage.getSlice());
                        newPolySliceVOI.setUID(newPolySliceVOI.hashCode());

                    } catch (OutOfMemoryError error) {
                        System.gc();
                        MipavUtil.displayError("Out of memory: ComponentEditImage.mouseReleased");
                        compImage.setMode(ViewJComponentEditImage.DEFAULT);

                        return;
                    }

                    compImage.getActiveImage().registerVOI(newPolySliceVOI);
                    newPolySliceVOI.setActive(true);
                    allActive = true;
                    ((VOIBase) (newPolySliceVOI.getCurves()[compImage.getSlice()].elementAt(0))).setActive(true);

                    updateVOIColor(newPolySliceVOI.getColor(), newPolySliceVOI.getUID());
                    // ( (VOIPoint) (VOIs.VOIAt(voiID).getCurves()[compImage.getSlice()].elementAt(0))).setActive(true);

                    compImage.getActiveImage().notifyImageDisplayListeners();

                    // System.err.println("click count: " + mouseEvent.getClickCount());

                    if (!((mouseEvent.isShiftDown() == true) ||
                              Preferences.is(Preferences.PREF_CONTINUOUS_VOI_CONTOUR))) {
                        compImage.setMode(ViewJComponentEditImage.DEFAULT);
                    }

                } // end of if (voiID == -1)
                else { // voiID != -1 add point to existing VOI

                    int index;

                    nVOI = VOIs.size();

                    float[] x = new float[1];
                    float[] y = new float[1];
                    float[] z = new float[1];

                    x[0] = xS;
                    y[0] = yS;
                    z[0] = compImage.getSlice();

                    for (i = 0; i < nVOI; i++) {

                        if (VOIs.VOIAt(i).getID() == voiID) {

                            if (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE_SLICE) {
                                VOIs.VOIAt(i).importCurve(x, y, z, compImage.getSlice());
                                VOIs.VOIAt(i).setAllActive(true);
                                VOIs.VOIAt(i).setActive(true);

                                break;
                            } else {
                                MipavUtil.displayError("Can't add POLYLINE_SLICE VOI to other VOI structure.");

                                return;
                            }
                        }
                    }

                    allActive = true;
                    compImage.getActiveImage().notifyImageDisplayListeners();

                    if (!((mouseEvent.isShiftDown() == true) ||
                              Preferences.is(Preferences.PREF_CONTINUOUS_VOI_CONTOUR))) {
                        compImage.setMode(ViewJComponentEditImage.DEFAULT);
                    }

                    return;
                } // end of else for if voiID != -1 add point to existing VOI
            } // end of if ((mouseEvent.getModifiers() & mouseEvent.BUTTON1_MASK) != 0)

        } else if (mode == ViewJComponentEditImage.ANNOTATION) {

            if ((mouseEvent.getModifiers() & InputEvent.BUTTON1_MASK) != 0) {

                VOI newTextVOI = null;

                int colorID = 0;

                if (compImage.getActiveImage().getVOIs().size() > 0) {
                    colorID = ((VOI) (compImage.getActiveImage().getVOIs().lastElement())).getID() + 1;
                }

                if (compImage.getActiveImage().getNDims() > 2) {
                    newTextVOI = new VOI((short) colorID, "annotation3d.voi",
                                         compImage.getActiveImage().getExtents()[2], VOI.ANNOTATION, -1.0f);
                } else {
                    newTextVOI = new VOI((short) colorID, "annotation2d.voi", 1, VOI.ANNOTATION, -1.0f);
                }

                float[] x = new float[2];
                float[] y = new float[2];
                float[] z = new float[2];

                voiID = compImage.getActiveImage().getVOIs().size();

                int sliceNum = 0;

                if (compImage.getActiveImage().getNDims() > 2) {
                    sliceNum = compImage.getSlice();
                }

                x[0] = xS;
                y[0] = yS;
                z[0] = sliceNum;

                x[1] = xS + 15;
                y[1] = yS + 15;
                z[1] = sliceNum;
                
                newTextVOI.importCurve(x, y, z, sliceNum);
                newTextVOI.setUID(newTextVOI.hashCode());
                newTextVOI.setColor(Color.WHITE);

                // pop up a dialog that allows text input, color, and font formatting
                new JDialogAnnotation(compImage.getActiveImage(), newTextVOI, compImage.getSlice(), false);

                if (!((mouseEvent.isShiftDown() == true) || Preferences.is(Preferences.PREF_CONTINUOUS_VOI_CONTOUR))) {
                    compImage.setMode(ViewJComponentEditImage.DEFAULT);
                }

            } // end of if ((mouseEvent.getModifiers() & mouseEvent.BUTTON1_MASK) != 0)

        } else if ((mode == ViewJComponentEditImage.POLYLINE) || (mode == ViewJComponentEditImage.LIVEWIRE)) {
            return;
            // compImage.setMode(DEFAULT);
        } else if (mode == ViewJComponentEditImage.LEVELSET) { }
        else if (mode == ViewJComponentEditImage.RECTANGLE) { }
        else if (mode == ViewJComponentEditImage.RECTANGLE3D) { }
        else if (mode == ViewJComponentEditImage.ELLIPSE) { }
        else if (mode == ViewJComponentEditImage.LINE) { }
        else if (mode == ViewJComponentEditImage.PROTRACTOR) { }
        else if (mode == ViewJComponentEditImage.NEW_POINT) { // impossible for LINE

            if (((mouseEvent.isShiftDown() == true))) {
                nVOI = VOIs.size();

                for (i = 0; i < nVOI; i++) {

                    if (VOIs.VOIAt(i).isActive()) {
                        break;
                    }
                }
                if (i == nVOI) {
                    return;
                }

                // Handle differently for POLYLINE_SLICE type VOIs...insert new VOIPoint structures
                if (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE_SLICE) {

                    if (!((mouseEvent.isShiftDown() == true) ||
                              Preferences.is(Preferences.PREF_CONTINUOUS_VOI_CONTOUR))) {
                        VOIs.VOIAt(i).insertPSlicePt(new Point3Df(xS, yS, 0), compImage.getSlice());
                        compImage.getActiveImage().notifyImageDisplayListeners(null, true);
                    }

                    return;
                }

            } else if (mouseEvent.getModifiers() == MouseEvent.BUTTON1_MASK) {
                nVOI = VOIs.size();

                for (i = 0; i < nVOI; i++) {

                    if (VOIs.VOIAt(i).isActive()) {
                        break;
                    }
                }

                if (i == nVOI) {
                    return;
                }

                if (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE_SLICE) {
                    return;
                }

                int index = VOIs.VOIAt(i).getActiveContourIndex(compImage.getSlice());

                ((VOIContour) (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(index))).insertElement(xS, yS,
                                                                                                                compImage.getSlice());
                compImage.getActiveImage().notifyImageDisplayListeners(null, true);

                if (VOIs.VOIAt(i).isVisible()) {

                    if (((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) ||
                             (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) && (mouseEvent.isControlDown() == false) &&
                            (mouseEvent.getModifiers() != MouseEvent.BUTTON3_MASK)) {

                        if ((VOIs.VOIAt(i).getContourGraph() != null) && VOIs.VOIAt(i).getContourGraph().isVisible()) {
                            VOI v;
                            float intensitySum;
                            int length = compImage.getActiveImage().getSliceSize();
                            int s;
                            int numPixels;
                            boolean foundCurve;
                            float[] position = VOIs.VOIAt(i).getPosition();
                            float[] intensity = VOIs.VOIAt(i).getIntensity();
                            float[][] rgbPositions = VOIs.VOIAt(i).getRGBPositions();
                            float[][] rgbIntensities = VOIs.VOIAt(i).getRGBIntensities();

                            if (compImage.getActiveImage().getNDims() == 3) {

                                if (compImage.getActiveImage().isColorImage() == true) {

                                    try {
                                        v = VOIs.VOIAt(i);

                                        for (s = 0, foundCurve = false; s < compImage.getActiveImage().getExtents()[2];
                                                 s++) {

                                            try {

                                                for (int c = 0; c < 3; c++) {
                                                    numPixels = 0;

                                                    for (j = 0, intensitySum = 0;
                                                             j < VOIs.VOIAt(i).getCurves()[s].size(); j++) {

                                                        if (((VOIContour) VOIs.VOIAt(i).getCurves()[s].elementAt(j))
                                                                .isActive() || foundCurve) {

                                                            if (!foundCurve) {
                                                                compImage.getActiveImage().exportData(s * length * 4,
                                                                                                      length * 4,
                                                                                                      graphImgBuff);
                                                            } // locks and releases lock

                                                            intensitySum += ((VOIContour)
                                                                                 (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                                .calcRGBIntensity(graphImgBuff,
                                                                                                      compImage.getActiveImage().getExtents()[0],
                                                                                                      c);
                                                            numPixels += ((VOIContour)
                                                                              (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                             .getLastNumPixels();
                                                            foundCurve = true;
                                                        }
                                                    }

                                                    if (foundCurve) {
                                                        rgbPositions[c][s] = s;

                                                        if (v.getTotalIntensity() || (numPixels == 0)) {
                                                            rgbIntensities[c][s] = intensitySum;
                                                        } else {
                                                            rgbIntensities[c][s] = intensitySum / numPixels;
                                                        }
                                                    }
                                                }
                                            } catch (IOException error) {
                                                MipavUtil.displayError("Image(s) locked");

                                                return;
                                            }

                                            foundCurve = false;
                                        }

                                        VOIs.VOIAt(i).getContourGraph().update(rgbPositions, rgbIntensities, 0);
                                        VOIs.VOIAt(i).getContourGraph().setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(0)));

                                    } catch (OutOfMemoryError error) {
                                        System.gc();
                                        MipavUtil.displayError("Out of memory: ComponentEditImage.graphVOI");

                                        return;
                                    }
                                } else {

                                    try {
                                        v = VOIs.VOIAt(i);

                                        for (s = 0, foundCurve = false; s < compImage.getActiveImage().getExtents()[2];
                                                 s++) {

                                            try {
                                                numPixels = 0;

                                                for (j = 0, intensitySum = 0; j < VOIs.VOIAt(i).getCurves()[s].size();
                                                         j++) {

                                                    if (((VOIContour) VOIs.VOIAt(i).getCurves()[s].elementAt(j))
                                                            .isActive() || foundCurve) {

                                                        if (!foundCurve) {
                                                            compImage.getActiveImage().exportData(s * length, length,
                                                                                                  graphImgBuff);
                                                        } // locks and releases lock

                                                        intensitySum += ((VOIContour)
                                                                             (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                            .calcIntensity(graphImgBuff,
                                                                                               compImage.getActiveImage().getExtents()[0]);
                                                        numPixels += ((VOIContour)
                                                                          (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                         .getLastNumPixels();
                                                        foundCurve = true;
                                                    }
                                                }

                                                if (foundCurve) {
                                                    position[s] = s;

                                                    if (v.getTotalIntensity() || (numPixels == 0)) {
                                                        intensity[s] = intensitySum;
                                                    } else {
                                                        intensity[s] = intensitySum / numPixels;
                                                    }

                                                    foundCurve = false;
                                                }
                                            } catch (IOException error) {
                                                MipavUtil.displayError("Image(s) locked");

                                                return;
                                            }
                                        }

                                        VOIs.VOIAt(i).getContourGraph().update(position, intensity, 0);
                                        VOIs.VOIAt(i).getContourGraph().setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(0)));

                                    } catch (OutOfMemoryError error) {
                                        System.gc();
                                        MipavUtil.displayError("Out of memory: ComponentEditImage.graphVOI");

                                        return;
                                    }
                                }
                            } else if (compImage.getActiveImage().getNDims() == 4) {
                                int zDim = compImage.getActiveImage().getExtents()[2];

                                try {
                                    v = VOIs.VOIAt(i);

                                    for (int t = 0; t < compImage.getActiveImage().getExtents()[3]; t++) {

                                        try {
                                            numPixels = 0;

                                            for (s = 0, intensitySum = 0;
                                                     s < compImage.getActiveImage().getExtents()[2]; s++) {
                                                compImage.getActiveImage().exportData((t * xDim * yDim * zDim) +
                                                                                      (s * xDim * yDim), length,
                                                                                      graphImgBuff); // locks and releases lock

                                                for (j = 0; j < VOIs.VOIAt(i).getCurves()[s].size(); j++) {
                                                    intensitySum += ((VOIContour)
                                                                         (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                        .calcIntensity(graphImgBuff,
                                                                                           compImage.getActiveImage().getExtents()[0]);
                                                    numPixels += ((VOIContour)
                                                                      (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                     .getLastNumPixels();

                                                }
                                            }

                                            position[t] = t;

                                            if (v.getTotalIntensity() || (numPixels == 0)) {
                                                intensity[t] = intensitySum;
                                            } else {
                                                intensity[t] = intensitySum / numPixels;
                                            }
                                        } catch (IOException error) {
                                            MipavUtil.displayError("Image(s) locked");

                                            return;
                                        }
                                    }

                                    VOIs.VOIAt(i).getContourGraph().update(position, intensity, 0);
                                    VOIs.VOIAt(i).getContourGraph().setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(0)));
                                } catch (OutOfMemoryError error) {
                                    System.gc();
                                    MipavUtil.displayError("Out of memory: ComponentEditImage.graphVOI");

                                    return;
                                }
                            }
                        }
                    }
                }
            }
        } else if (mode == ViewJComponentEditImage.DELETE_POINT) { // impossible for LINE
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
            compImage.setMode(ViewJComponentEditImage.MOVE_POINT);
        } else if ((mode == ViewJComponentEditImage.PAINT_CAN) || (mode == ViewJComponentEditImage.PAINT_VASC)) {
            return;
        } else if (mode == ViewJComponentEditImage.MOVE) {
            Graphics g = compImage.getGraphics();
            nVOI = VOIs.size();

            if (!mouseEvent.isControlDown()) {

                for (i = 0; i < nVOI; i++) { // VOIs.VOIAt(i).setAllActive(false); // deactivate all other VOIs
                }
            }

            for (i = 0; i < nVOI; i++) {
                VOIBase selectedCurve = null;

                for (j = 0; j < VOIs.VOIAt(i).getCurves()[compImage.getSlice()].size(); j++) {

                    // get the curve referenced by the VOI.  We'll check it.
                    selectedCurve = ((VOIBase) VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j));

                    if (selectedCurve instanceof VOIPoint) {

                        if (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE_SLICE) {

                            if (VOIs.VOIAt(i).nearLine(xS, yS, compImage.getSlice())) {
                                VOIs.VOIAt(i).setActive(true);
                                VOIs.VOIAt(i).setAllActive(true);

                                VOIs.VOIAt(i).markPSlicePt(compImage.getSlice());

                                allActive = true;
                                updateVOIColor(VOIs.VOIAt(i).getColor(), VOIs.VOIAt(i).getUID());
                                voiID = VOIs.VOIAt(i).getID();
                                j = VOIs.VOIAt(i).getCurves()[compImage.getSlice()].size();
                                fireVOISelectionChange(VOIs.VOIAt(i), selectedCurve);

                                break;
                            }
                        } else if (((VOIPoint) selectedCurve).nearPoint(xR, yR, compImage.getZoomX(),
                                                                            compImage.getResolutionX(),
                                                                            compImage.getResolutionY())) {

                            // points are not true curves, but we want to check if we
                            // released mouse over it. we'll at least set the point active.
                            if (mouseEvent.isShiftDown()) {
                                allActive = true;

                                // if true set all points in VOI active - move all points
                                VOIs.VOIAt(i).setAllActive(true);
                                updateVOIColor(VOIs.VOIAt(i).getColor(), VOIs.VOIAt(i).getUID());
                                voiID = VOIs.VOIAt(i).getID();

                                // and we are done with this VOI.
                                // skip the rest of the curves
                                j = VOIs.VOIAt(i).getCurves()[compImage.getSlice()].size();
                            } else {
                                allActive = false;
                                VOIs.VOIAt(i).setActive(true);
                                updateVOIColor(VOIs.VOIAt(i).getColor(), VOIs.VOIAt(i).getUID());
                                ((VOIPoint) (selectedCurve)).setActive(true);

                                Point3Df pt = ((VOIPoint) (selectedCurve)).exportPoint();

                                compImage.setPixelInformationAtLocation((int) pt.x, (int) pt.y);

                                voiID = VOIs.VOIAt(i).getID();
                            }

                            fireVOISelectionChange(VOIs.VOIAt(i), selectedCurve);

                        }
                    } else if ((selectedCurve instanceof VOIText) &&
                                   ((VOIText) selectedCurve).contains(xS, yS, compImage.getZoomX(),
                                                                          compImage.getZoomY(),
                                                                          compImage.getActiveImage().getResolutions(0),
                                                                          g)) {

                        allActive = false;
                        VOIs.VOIAt(i).setActive(true);
                        updateVOIColor(VOIs.VOIAt(i).getColor(), VOIs.VOIAt(i).getUID());
                        ((VOIText) (selectedCurve)).setActive(true);
                        voiID = VOIs.VOIAt(i).getID();

                        // if the Text was double-clicked, bring up the editor
                        if (mouseEvent.getClickCount() == 2) {
                            new JDialogAnnotation(compImage.getActiveImage(), VOIs.VOIAt(i), compImage.getSlice(), true);
                        }
                    } else if (selectedCurve.contains(xS, yS, true)) {

                        // if we released the mouse over another kind of curve,
                        // we'll at least set it active.
                        if (mousePressIsShiftDown) {

                            // when shift is presed at the same time,
                            // select all the curves in the (contour) grouping.
                            allActive = true;
                            VOIs.VOIAt(i).setAllActive(true);
                            updateVOIColor(VOIs.VOIAt(i).getColor(), VOIs.VOIAt(i).getUID());
                            voiID = VOIs.VOIAt(i).getID();

                            // and we are done with this VOI. Skip the rest of the curves
                            j = VOIs.VOIAt(i).getCurves()[compImage.getSlice()].size();
                        } else {
                            allActive = false;

                            // toggle isActive (or if VOI was dragged, leave active)
                            boolean toggle = !selectedCurve.isActive() || wasDragging || mouseEvent.isPopupTrigger();

                            // if VOI should be made/left active, do not
                            // make any other contours active unless
                            // control was held down
                            if (!mouseEvent.isControlDown() && toggle) {

                                for (k = 0; k < nVOI; k++) {
                                    VOIs.VOIAt(k).setAllActive(false); // deactivate all VOIs
                                }

                                VOIs.VOIAt(i).setActive(true);
                                selectedCurve.setActive(toggle);
                                voiID = VOIs.VOIAt(i).getID();
                                fireVOISelectionChange(VOIs.VOIAt(i), selectedCurve);
                                updateVOIColor(VOIs.VOIAt(i).getColor(), VOIs.VOIAt(i).getUID());

                                break;
                            } else {
                                VOIs.VOIAt(i).setActive(toggle);
                                selectedCurve.setActive(toggle);
                            }
                            // VOIs.VOIAt( i ).setActive( toggle ); // set the current active // move single contour
                            // updateVOIColor( VOIs.VOIAt( i ).getColor(), VOIs.VOIAt( i ).getUID() );

                            selectedCurve.setActive(toggle); // set its curve to active (for display)

                            voiID = VOIs.VOIAt(i).getID();
                        }

                        fireVOISelectionChange(VOIs.VOIAt(i), selectedCurve);
                    } else if (!mouseEvent.isControlDown() && !mouseEvent.isPopupTrigger()) { // selected curve was not
                                                                                              // selected, so set false.
                        selectedCurve.setActive(false);
                        fireVOISelectionChange(VOIs.VOIAt(i));
                        // System.err.println("set something to inactive...fired a changed?");
                    }
                } // end of curves in this VOI
            } // end checking all VOIs in the active image

            wasDragging = false; // reset the mouse drag boolean
            compImage.getGraphics().dispose();
            compImage.getActiveImage().notifyImageDisplayListeners();
        } else if (mode == ViewJComponentEditImage.MOVE_POINT) {

            nVOI = VOIs.size();

            for (i = 0; i < nVOI; i++) {

                if ((VOIs.VOIAt(i).isActive() && (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)) ||
                        (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
                    VOIs.VOIAt(i).rubberbandVOI(xS, yS, compImage.getSlice(), xDim, yDim, true);

                    for (j = 0; j < VOIs.VOIAt(i).getCurves()[compImage.getSlice()].size(); j++) {

                        if (((VOIBase) (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j))).isActive()) {
                            Point3Df pt = ((VOIBase) (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j)))
                                              .getActivePt();
                            compImage.setPixelInformationAtLocation((int) pt.x, (int) pt.y);

                            break;
                        }
                    }

                    VOIs.VOIAt(i).nearPoint(0, 0, compImage.getSlice(), compImage.getZoomX(),
                                            compImage.getResolutionX(), compImage.getResolutionY());
                } else if ((VOIs.VOIAt(i).isActive() && (VOIs.VOIAt(i).getCurveType() == VOI.LINE)) ||
                               (VOIs.VOIAt(i).getCurveType() == VOI.PROTRACTOR)) {

                    for (j = 0; j < VOIs.VOIAt(i).getCurves()[compImage.getSlice()].size(); j++) {

                        if (((VOIBase) (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j))).isActive()) {

                            if (VOIs.VOIAt(i).getCurveType() == VOI.LINE) {
                                Point3Df pt = ((VOIBase) (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j)))
                                                  .getActivePt();
                                compImage.setPixelInformationAtLocation((int) pt.x, (int) pt.y);
                            }

                            if (VOIs.VOIAt(i).getCurveType() == VOI.PROTRACTOR) {
                                ((VOIProtractor) (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j)))
                                    .setShowLength(true);
                            }

                            compImage.getActiveImage().notifyImageDisplayListeners();

                            return;
                        }
                    }
                }
            }

            compImage.getActiveImage().notifyImageDisplayListeners(null, true);
        } else if (mode == ViewJComponentEditImage.RETRACE) {
            nVOI = VOIs.size();

            for (i = 0; i < nVOI; i++) {

                if (VOIs.VOIAt(i).isActive()) {
                    ((VOIContour) (VOIs.VOIAt(i).getActiveContour(compImage.getSlice()))).trimPoints(Preferences.getTrim(),
                                                                                                     true);
                    ((VOIContour) (VOIs.VOIAt(i).getActiveContour(compImage.getSlice()))).resetIndex();

                    break;
                }
            }

            compImage.getActiveImage().notifyImageDisplayListeners();
        } else if (mode == ViewJComponentEditImage.QUICK_LUT) {

            if (compImage.getActiveImage().getHistoLUTFrame() != null) {
                compImage.getActiveImage().getHistoLUTFrame().update();
            }
        }

        // reset mousePressIsShiftDown for next mouse click
        mousePressIsShiftDown = false;

    } // end mouseReleased()

    /**
     * Draws the VOI, blending it with the image.
     *
     * @param  graphics  DOCUMENT ME!
     */
    public void paintSolidVOIinImage(Graphics graphics) {
        ViewVOIVector VOIs = compImage.getActiveImage().getVOIs();

        if (VOIs != null) {
            int nVOI = VOIs.size();

            if (compImage.getSlice() != -99) {
                for (int i = nVOI - 1; i >= 0; i--) {
                    VOIs.VOIAt(i).drawBlendSelf(1, 1, 1, 1, compImage.getSlice(), compImage.getPaintBuffer(), graphics,
                                                compImage.getActiveImage().getExtents()[0],
                                                compImage.getActiveImage().getExtents()[1]);
                }
            }
        }
    }

    /**
     * Pastes a VOI from the MIPAV clipboard into the active image.
     */
    public void pasteVOI() {
        float[] x, y, z;
        int imageXExt, imageYExt, zExt;
        VOI newVOI, outVOI;
        int newSlice;

        boolean isFrom2D = ViewUserInterface.getReference().isClippedVOI2D();
        
        if ((compImage.getActiveImage().getNDims() > 2 && isFrom2D) ||
        		(compImage.getActiveImage().getNDims() == 2 && !isFrom2D)) {
        	//display an error here
        	MipavUtil.displayInfo("Can not copy/paste VOIs between 2D and 2.5D/3D images.");
        	return;
        }
        
        boolean isAnnotation = false;

        ViewVOIVector clippedVOIs = ViewUserInterface.getReference().getClippedVOIs();
        Vector clippedSlices = ViewUserInterface.getReference().getClippedSlices();
        int numClipped = clippedVOIs.size();
        
        Vector scannerVectors = null;
        if (!isFrom2D) {
        	scannerVectors = ViewUserInterface.getReference().getClippedScannerVectors();
        }

        for (int k = 0; k < numClipped; k++) {

            try {
                x = new float[2];
                y = new float[2];
                z = new float[2];
                imageXExt = compImage.getActiveImage().getExtents()[0];
                imageYExt = compImage.getActiveImage().getExtents()[1];
                newVOI = (VOI) clippedVOIs.VOIAt(k).clone();
                
                //if the VOI was from a 3D image, process accordingly (using scanner conversion)
                if (isFrom2D == false) {
                	Vector [] curves = newVOI.getCurves();
                	Vector [] newCurves = new Vector[compImage.getActiveImage().getExtents()[2]];
                	for (int n = 0; n < newCurves.length; n++){
                		newCurves[n] = new Vector();
                	}
                	VOIBase tempBase;
                	int numCurves = 0;
                	Vector scannerVector = (Vector)scannerVectors.elementAt(k);
                	int scannerIndex = 0;
                	Point3Df inPoint = null;
                	Point3Df outPoint = new Point3Df();
                	
                	int slice = -1;
                	for (int s = 0; s < curves.length; s++) {
                		numCurves = curves[s].size();
                		int numPoints = 0;
                		for (int j = 0; j < numCurves; j++) {
                			tempBase = (VOIBase)curves[s].elementAt(j);
                			numPoints = tempBase.size();
                		//	System.err.println("number of points: " + numPoints);
                			slice = -1;
                			for (int c = 0; c < numPoints; c++, scannerIndex++) {
                				inPoint = (Point3Df)scannerVector.elementAt(scannerIndex);
    	                		
                			//	System.err.println("In point (scanner): " + inPoint);
                				
                				//convert from scanner
                				MipavCoordinateSystems.ScannerToFile(inPoint, outPoint, compImage.getActiveImage());
                				((Point3Df)tempBase.elementAt(c)).x = outPoint.x;
                				((Point3Df)tempBase.elementAt(c)).y = outPoint.y;
                				((Point3Df)tempBase.elementAt(c)).z = outPoint.z;
                				
                				//System.err.println("Out point is: " + outPoint);
                				
                				if (slice != -1 && slice != MipavMath.round(outPoint.z)) {
                					//error...shouldnt be different slices
                				//	System.err.println("ERROR: slice is " + slice + ", while outPoint.z is " + MipavMath.round(outPoint.z));
                					MipavUtil.displayError("VOI's contour points do not fall on the same slices: aborting");
                					return;
                				}
                				slice = MipavMath.round(outPoint.z);
    	                	}
                			newCurves[slice].addElement(tempBase);
                		}
                	}
                	newVOI.setCurves(newCurves);
                }
                
                
                outVOI = (VOI) newVOI.clone();
                newSlice = ((Integer) clippedSlices.elementAt(k)).intValue();
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory in pasteVOI.");

                return;
            } catch (NullPointerException e) {
            	e.printStackTrace();
                MipavUtil.displayInfo("The clipboard is empty, so there is nothing to paste.");

                return;
            } catch (ArrayIndexOutOfBoundsException e) {
            	MipavUtil.displayError("VOI bounds do not match those of current image: aborting.");
            	return;
            }

            
            
            newVOI.getBounds(x, y, z);

            for (int i = 0; i < x.length; i++) {
          //      System.err.println("x-" + i + ": " + x[i]);
            }

            for (int i = 0; i < y.length; i++) {
           //     System.err.println("y-" + i + ": " + y[i]);
            }

            for (int i = 0; i < z.length; i++) {
           //     System.err.println("z-" + i + ": " + z[i]);
            }


            if (outVOI.getCurveType() == VOI.ANNOTATION) {
                isAnnotation = true;
            }

            if ((imageXExt <= (x[1] - x[0])) || (imageYExt <= (y[1] - y[0])) || (imageXExt <= x[0]) ||
                    (imageXExt <= x[1]) || (imageYExt <= y[0]) || (imageYExt <= y[1])) { // checks to make sure if voi
                                                                                         // can fit
                MipavUtil.displayError("Cannot paste VOI : VOI out of image bounds."); // in image in x and y dimension

                return;
            } else if ((newVOI.getCurveType() == VOI.POLYLINE_SLICE) && (compImage.getActiveImage().getNDims() < 3)) {
                MipavUtil.displayError("Cannot paste VOI : Polyline-Slice VOIs are restricted to 3D+ images");
            }


            int ID = 0; // finds an ID to assign the new VOI, which is not
            int test; // already in use by another VOI in the image

            do {
                test = 1;

                for (int i = 0; i < compImage.getActiveImage().getVOIs().size(); i++) {
                    if (ID == ((VOI) compImage.getActiveImage().getVOIs().elementAt(i)).getID()) {
                        ID++;
                        test = 0;
                    }
                }
            } while (test == 0);

            outVOI.setID((short)ID);
            float hue = (float) ((((ID) * 35) % 360) / 360.0);
            outVOI.setColor(Color.getHSBColor(hue, (float) 1.0, (float) 1.0));
            
            // check to see if a VOI with this name exists, if not, keep same name
            // otherwise append "_pasted_#" to the name
            for (int i = 0; i < compImage.getActiveImage().getVOIs().size(); i++) {

                if (outVOI.getName().equals(((VOI) compImage.getActiveImage().getVOIs().elementAt(i)).getName())) {
                    outVOI.setName(outVOI.getName() + "_pasted_" + ID);
                }
            }

            if (compImage.getActiveImage().getExtents().length > 2) {
                zExt = compImage.getActiveImage().getExtents()[2]; // gets the Z dimensions of the active image
            } else {
                zExt = 1;
            }

            if ((newVOI.getCurveType() == VOI.POLYLINE_SLICE) || (newVOI.getCurveType() == VOI.POINT)) {

                for (int i = 0; i < outVOI.getCurves().length; i++) {
                    outVOI.removeCurves(i);
                }

                int oldIndex = 0;

                for (oldIndex = 0; oldIndex < newVOI.getCurves().length; oldIndex++) {

                    if (newVOI.getCurves()[oldIndex].size() > 0) {
                        break;
                    }
                }

                int numSlices = (int) (z[1] - z[0]) + 1;
                int numElements = 0;
                int sliceCounter = 0;

                for (int start = compImage.getSlice(); sliceCounter < numSlices; start++, oldIndex++, sliceCounter++) {

                    numElements = newVOI.getCurves()[oldIndex].size();

                    for (int i = 0; i < numElements; i++) {
                        outVOI.getCurves()[start].addElement(newVOI.getCurves()[oldIndex].elementAt(i));
                    }
                }

                compImage.getActiveImage().getVOIs().addElement(outVOI);

            } else {

                if (((z[1] - z[0]) != 0) || (outVOI.getCurveType() == VOI.POLYLINE_SLICE)) { // checks to see if it is
                                                                                             // a 3D VOI

                    if ((z[1] - z[0] + 1) == zExt) {
                        compImage.getActiveImage().getVOIs().addElement(outVOI); // Add entire VOI
                    } else if ((z[0] >= 0) && (z[1] <= (zExt - 1))) {

                        // might wish to mod to copy only subset of input contours into VOI
                        compImage.getActiveImage().getVOIs().addElement(outVOI);
                    } else {
                        outVOI.importNewVOI(compImage.getSlice(), newSlice, newVOI, zExt); // add only contour in
                                                                                           // compImage.getSlice()
                        compImage.getActiveImage().getVOIs().addElement(outVOI);
                    }
                } else {

                    if (isAnnotation) {
                        compImage.getActiveImage().registerVOI(outVOI);

                        VOIVector vois = compImage.getActiveImage().getVOIs();
                    } else {
                        outVOI.importNewVOI(compImage.getSlice(), newSlice, newVOI, zExt);
                        compImage.getActiveImage().getVOIs().addElement(outVOI);
                    }
                }
            }
        }

        compImage.getActiveImage().notifyImageDisplayListeners(null, true);
    }

    /**
     * Copies a VOI contour to the next compImage.getSlice().
     *
     * @param   direction  if positive propagate upward, if negative propagate downward
     * @param   active     Whether or not to propogate a levelset VOI.
     *
     * @return  if false failed to propVOI
     */
    public boolean propVOI(int direction, boolean active) {
        int i, j;
        int nVOI;
        int nCurves;
        Polygon[] gons = null;
        Point3Df[] points = null;
        ViewVOIVector VOIs = compImage.getActiveImage().getVOIs();
        Vector[] curves;
        boolean doPoint = false;

        if (compImage.getActiveImage().getNDims() < 3) {
            return false;
        }

        if ((compImage.getSlice() < (compImage.getActiveImage().getExtents()[2] - 1)) && (direction > 0)) { // Propagate up
            nVOI = VOIs.size();

            if (nVOI == 0) {
                return false;
            }

            for (i = 0; i < nVOI; i++) {

                if ((VOIs.VOIAt(i).isActive() == true) &&
                        ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) ||
                             (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE))) {
                    gons = VOIs.VOIAt(i).exportPolygons(compImage.getSlice());

                    break;
                } else if ((VOIs.VOIAt(i).isActive() == true) && (VOIs.VOIAt(i).getCurveType() == VOI.POINT)) {
                    points = VOIs.VOIAt(i).exportPoints(compImage.getSlice());
                    doPoint = true;

                    break;
                }
            }

            if (i == nVOI) {
                MipavUtil.displayError("Please select a VOI");

                return false; // Should show error -- Must select VOI
            }

            curves = VOIs.VOIAt(i).getCurves();
            nCurves = curves[compImage.getSlice()].size();

            if (!doPoint) {

                for (j = 0; j < nCurves; j++) {

                    if (((VOIContour) (curves[compImage.getSlice()].elementAt(j))).isActive()) {
                        break;
                    }
                }

                if (j == nCurves) {
                    MipavUtil.displayError("Please select a VOI");

                    return false; // Should show error -- Must select VOI
                }

                if (active) {

                    if (VOIs.VOIAt(i).getLevel() == VOIs.VOIAt(i).NOT_A_LEVELSET) {
                        MipavUtil.displayError("You can only use active propogation with a levelset VOI.");

                        return false;
                    } else {
                        RubberbandLevelSet rubber = new RubberbandLevelSet(compImage);

                        removeMouseListener(rubber);
                        removeMouseMotionListener(rubber);
                        compImage.show(compImage.getTimeSlice(), compImage.getSlice() + 1, null, null, true,
                                       compImage.getInterpMode());

                        Polygon gon = rubber.findNextLevelSet(VOIs.VOIAt(i).getLevel(), gons[j]);

                        compImage.show(compImage.getTimeSlice(), compImage.getSlice() - 1, null, null, true,
                                       compImage.getInterpMode());

                        if (gon == null) {
                            MipavUtil.displayError("Active propogation returned no viable contour.\nUse regular propogation.");

                            return false;
                        }

                        VOIs.VOIAt(i).setAllActive(false);
                        VOIs.VOIAt(i).importPolygon(gon, compImage.getSlice() + 1);
                        VOIs.VOIAt(i).setActive(true);
                        updateVOIColor(VOIs.VOIAt(i).getColor(), VOIs.VOIAt(i).getUID());
                        VOIs.VOIAt(i).setLevel(rubber.getLevel());
                        ((VOIContour) (VOIs.VOIAt(i).getCurves()[compImage.getSlice() + 1].lastElement())).setActive(true);
                        
                        return true;
                    }
                }

                VOIs.VOIAt(i).setAllActive(false);
                VOIs.VOIAt(i).importPolygon(gons[j], compImage.getSlice() + 1);
                VOIs.VOIAt(i).setActive(true);
                updateVOIColor(VOIs.VOIAt(i).getColor(), VOIs.VOIAt(i).getUID());
                ((VOIContour) (VOIs.VOIAt(i).getCurves()[compImage.getSlice() + 1].lastElement())).setActive(true);
                fireVOISelectionChange(VOIs.VOIAt(i), null);
            } // end of if (!doPoint)
            else { // VOI.POINT

                if (active) {
                    MipavUtil.displayError("You can only use active propogation with a levelset VOI.");

                    return false;
                }

                for (j = 0; j < nCurves; j++) {

                    if (((VOIPoint) (curves[compImage.getSlice()].elementAt(j))).isActive()) {
                        break;
                    }
                }

                if (j == nCurves) {
                    MipavUtil.displayError("Please select a VOI");

                    return false; // Should show error -- Must select VOI
                }

                VOIs.VOIAt(i).setAllActive(false);
                VOIs.VOIAt(i).importPoint(points[j], compImage.getSlice() + 1);
                VOIs.VOIAt(i).setActive(true);
                updateVOIColor(VOIs.VOIAt(i).getColor(), VOIs.VOIAt(i).getUID());
                ((VOIPoint) (VOIs.VOIAt(i).getCurves()[compImage.getSlice() + 1].lastElement())).setActive(true);
                fireVOISelectionChange(VOIs.VOIAt(i), null);
            } // end of else for VOI.POINT
        } // end of Propagate up
        else if ((compImage.getSlice() > 0) && (direction < 0)) { // propagate down
            nVOI = VOIs.size();

            if (nVOI == 0) {
                return false;
            }

            for (i = 0; i < nVOI; i++) {

                if ((VOIs.VOIAt(i).isActive() == true) &&
                        ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) ||
                             (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE))) {
                    gons = VOIs.VOIAt(i).exportPolygons(compImage.getSlice());

                    break;
                } else if ((VOIs.VOIAt(i).isActive() == true) && (VOIs.VOIAt(i).getCurveType() == VOI.POINT)) {
                    points = VOIs.VOIAt(i).exportPoints(compImage.getSlice());
                    doPoint = true;

                    break;
                }
            }

            if (i == nVOI) {
                MipavUtil.displayError("Please select a VOI");

                return false; // Should show error -- Must select VOI
            }

            curves = VOIs.VOIAt(i).getCurves();
            nCurves = curves[compImage.getSlice()].size();

            if (!doPoint) {

                for (j = 0; j < nCurves; j++) {

                    if (((VOIContour) (curves[compImage.getSlice()].elementAt(j))).isActive()) {
                        break;
                    }
                }

                if (j == nCurves) {
                    MipavUtil.displayError("Please select a VOI");

                    return false; // Should show error -- Must select VOI
                }

                if (active) {

                    if (VOIs.VOIAt(i).getLevel() == VOIs.VOIAt(i).NOT_A_LEVELSET) {
                        MipavUtil.displayError("You can only use active propogation with a levelset VOI.");

                        return false;
                    } else {
                        RubberbandLevelSet rubber = new RubberbandLevelSet(compImage);

                        removeMouseListener(rubber);
                        removeMouseMotionListener(rubber);
                        compImage.show(compImage.getTimeSlice(), compImage.getSlice() - 1, null, null, true,
                                       compImage.getInterpMode());

                        Polygon gon = rubber.findNextLevelSet(VOIs.VOIAt(i).getLevel(), gons[j]);

                        compImage.show(compImage.getTimeSlice(), compImage.getSlice() + 1, null, null, true,
                                       compImage.getInterpMode());

                        if (gon == null) {
                            MipavUtil.displayError("Active propogation returned no viable contour.\nUse regular propogation.");

                            return false;
                        }

                        VOIs.VOIAt(i).setAllActive(false);
                        VOIs.VOIAt(i).importPolygon(gon, compImage.getSlice() - 1);
                        VOIs.VOIAt(i).setActive(true);
                        updateVOIColor(VOIs.VOIAt(i).getColor(), VOIs.VOIAt(i).getUID());
                        VOIs.VOIAt(i).setLevel(rubber.getLevel());
                        ((VOIContour) (VOIs.VOIAt(i).getCurves()[compImage.getSlice() - 1].lastElement())).setActive(true);

                        return true;
                    }
                }

                VOIs.VOIAt(i).setAllActive(false);
                VOIs.VOIAt(i).importPolygon(gons[j], compImage.getSlice() - 1);
                VOIs.VOIAt(i).setActive(true);
                updateVOIColor(VOIs.VOIAt(i).getColor(), VOIs.VOIAt(i).getUID());
                ((VOIContour) (VOIs.VOIAt(i).getCurves()[compImage.getSlice() - 1].lastElement())).setActive(true);
                fireVOISelectionChange(VOIs.VOIAt(i), null);
            } // end of if (!doPoint)
            else { // VOI.POINT

                if (active) {
                    MipavUtil.displayError("You can only use active propogation with a levelset VOI.");

                    return false;
                }

                for (j = 0; j < nCurves; j++) {

                    if (((VOIPoint) (curves[compImage.getSlice()].elementAt(j))).isActive()) {
                        break;
                    }
                }

                if (j == nCurves) {
                    MipavUtil.displayError("Please select a VOI");

                    return false; // Should show error -- Must select VOI
                }

                VOIs.VOIAt(i).setAllActive(false);
                VOIs.VOIAt(i).importPoint(points[j], compImage.getSlice() - 1);
                VOIs.VOIAt(i).setActive(true);
                updateVOIColor(VOIs.VOIAt(i).getColor(), VOIs.VOIAt(i).getUID());
                ((VOIPoint) (VOIs.VOIAt(i).getCurves()[compImage.getSlice() - 1].lastElement())).setActive(true);
                fireVOISelectionChange(VOIs.VOIAt(i), null);
            } // end of else for VOI.POINT
        } // end of propagate down

        return true;
    }

    /**
     * Copies a VOI contour to all slices.
     *
     * @return  if false failed to propVOI
     */
    public boolean propVOIAll() {
        int i, j;
        int nVOI;
        int nCurves;
        Polygon[] gons = null;
        Point3Df[] points = null;
        ViewVOIVector VOIs = compImage.getActiveImage().getVOIs();
        Vector[] curves;
        boolean doPoint = false;

        nVOI = VOIs.size();

        if (nVOI == 0) {
            return false;
        }

        for (i = 0; i < nVOI; i++) {

            if ((VOIs.VOIAt(i).isActive() == true) &&
                    ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) || (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE))) {
                gons = VOIs.VOIAt(i).exportPolygons(compImage.getSlice());

                break;
            } else if ((VOIs.VOIAt(i).isActive() == true) && (VOIs.VOIAt(i).getCurveType() == VOI.POINT)) {
                points = VOIs.VOIAt(i).exportPoints(compImage.getSlice());
                doPoint = true;

                break;
            }
        }

        if (i == nVOI) {
            MipavUtil.displayError("Please select a VOI");

            return false; // Should show error -- Must select VOI
        }

        curves = VOIs.VOIAt(i).getCurves();
        nCurves = curves[compImage.getSlice()].size();

        if (!doPoint) {

            for (j = 0; j < nCurves; j++) {

                if (((VOIContour) (curves[compImage.getSlice()].elementAt(j))).isActive()) {
                    break;
                }
            }

            if (j == nCurves) {
                MipavUtil.displayError("Please select a VOI");

                return false; // Should show error -- Must select VOI
            }

            VOIs.VOIAt(i).setAllActive(false);

            for (int s = 0; s < compImage.getActiveImage().getExtents()[2]; s++) {

                if (s != compImage.getSlice()) {
                    VOIs.VOIAt(i).importPolygon(gons[j], s);
                }
            }

            // compImage.getActiveImage().notifyImageDisplayListeners();
            VOIs.VOIAt(i).setAllActive(true);
            updateVOIColor(VOIs.VOIAt(i).getColor(), VOIs.VOIAt(i).getUID());
            fireVOISelectionChange(VOIs.VOIAt(i), null);
        } // end of if (!doPoint)
        else { // VOI.POINT

            for (j = 0; j < nCurves; j++) {

                if (((VOIPoint) (curves[compImage.getSlice()].elementAt(j))).isActive()) {
                    break;
                }
            }

            if (j == nCurves) {
                MipavUtil.displayError("Please select a VOI");

                return false; // Should show error -- Must select VOI
            }

            VOIs.VOIAt(i).setAllActive(false);

            for (int s = 0; s < compImage.getActiveImage().getExtents()[2]; s++) {

                if (s != compImage.getSlice()) {
                    VOIs.VOIAt(i).importPoint(points[j], s);
                }
            }

            VOIs.VOIAt(i).setAllActive(true);
            updateVOIColor(VOIs.VOIAt(i).getColor(), VOIs.VOIAt(i).getUID());
            fireVOISelectionChange(VOIs.VOIAt(i), null);
        } // end of else for VOI.POINT

        compImage.getActiveImage().notifyImageDisplayListeners();

        return true;
    }

    /**
     * Regenerates and displays a 1D graph of the.
     */
    public void redrawGraphPointVOI() {

        int i, j;
        Point3Df pt;
        ViewVOIVector VOIs = compImage.getActiveImage().getVOIs();
        int nVOI = VOIs.size();
        int yDim = compImage.getActiveImage().getExtents()[1];
        int xDim = compImage.getActiveImage().getExtents()[0];
        int zDim = 1;

        if (compImage.getActiveImage().getNDims() >= 3) {
            zDim = compImage.getActiveImage().getExtents()[2];
        }

        for (i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.POINT) {

                VOIs.VOIAt(i).setContourGraph(null);

                for (j = 0; j < VOIs.VOIAt(i).getCurves()[compImage.getSlice()].size(); j++) {

                    if (compImage.getActiveImage().getNDims() == 3) {

                        if (compImage.getActiveImage().isColorImage() == true) {

                            for (int s = 0; s < compImage.getActiveImage().getExtents()[2]; s++) {
                                pt = ((VOIPoint) (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j)))
                                         .exportPoint();

                                for (int c = 0; c < 3; c++) {
                                    ptRGBPositions[c][s] = s;
                                    ptRGBIntensities[c][s] = compImage.getActiveImage().getFloat((int) ((4 *
                                                                                                             ((s *
                                                                                                                   compImage.getActiveImage().getSliceSize()) +
                                                                                                                  (pt.y *
                                                                                                                       compImage.getActiveImage().getExtents()[0]) +
                                                                                                                  pt.x)) +
                                                                                                        c + 1));
                                }
                            }

                            // VOIs.VOIAt(i).getContourGraph().update(ptRGBPositions, ptRGBIntensities, j);
                            graphPointVOI(VOIs.VOIAt(i),
                                          (VOIPoint) (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j)), j);
                        } else {

                            for (int s = 0; s < compImage.getActiveImage().getExtents()[2]; s++) {

                                pt = ((VOIPoint) (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j)))
                                         .exportPoint();
                                ptPosition[s] = s;
                                ptIntensity[s] = compImage.getActiveImage().getFloat((int) ((s *
                                                                                                 compImage.getActiveImage().getSliceSize()) +
                                                                                            (pt.y *
                                                                                                 compImage.getActiveImage().getExtents()[0]) +
                                                                                            pt.x));
                            }

                            // VOIs.VOIAt(i).getContourGraph().update(ptPosition, ptIntensity, j);
                            graphPointVOI(VOIs.VOIAt(i),
                                          (VOIPoint) (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j)), j);
                        }
                    } else if (compImage.getActiveImage().getNDims() == 4) {

                        for (int t = 0; t < compImage.getActiveImage().getExtents()[3]; t++) {
                            pt = ((VOIPoint) (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j)))
                                     .exportPoint();
                            ptPosition[t] = t;
                            ptIntensity[t] = compImage.getActiveImage().getFloat((int) ((t * xDim * yDim * zDim) +
                                                                                        (pt.z * xDim * yDim) +
                                                                                        (pt.y * xDim) + pt.x));
                        }

                        // VOIs.VOIAt(i).getContourGraph().update(ptPosition, ptIntensity, j);
                        graphPointVOI(VOIs.VOIAt(i),
                                      (VOIPoint) (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j)), j);
                    }
                }
            }
        }
    }

    /**
     * removes the update listener.
     *
     * @param  listener  DOCUMENT ME!
     */
    public void removeVOIUpdateListener(UpdateVOISelectionListener listener) {
        listenerList.remove(UpdateVOISelectionListener.class, listener);
    }

    /**
     * Resets the rubberband live wire to null. When the rubberband live wire is null, the grad mag field is recaculated
     * the next time live wire mode is selected.
     */
    public void resetLivewire() {
        rbLivewire = null;
    }

    /**
     * Selects (or de-selects) all VOIs.
     *
     * @param  doSelect  boolean select all or deselect all
     */
    public void selectAllVOIs(boolean doSelect) {
        int i;
        int nVOI;
        ViewVOIVector VOIs = compImage.getActiveImage().getVOIs();

        nVOI = VOIs.size();

        for (i = 0; i < nVOI; i++) {

            VOIs.VOIAt(i).setAllActive(doSelect);
        }

        compImage.getActiveImage().notifyImageDisplayListeners(null, true);
    }
    
    /**
     * Selects all contours of the currently active VOI (like holding down Shift and clicking on a VOI)
     *
     */
    public void selectAllContours() {
    	int i, j;
        int nVOI;
        int zDim = 1;
        if (compImage.getActiveImage().getNDims() > 2) {
        	zDim = compImage.getActiveImage().getExtents()[2];
        }
        ViewVOIVector VOIs = compImage.getActiveImage().getVOIs();

        Vector [] curves;
        nVOI = VOIs.size();

        for (i = 0; i < nVOI; i++) {
        	if (VOIs.VOIAt(i).isActive()) {
        		for (j = 0; j < zDim; j++) {
        			curves = VOIs.VOIAt(i).getCurves();
                    for (i = 0; i < curves[j].size(); i++) {
                        ((VOIBase) (curves[j].elementAt(i))).setActive(true);
                    }
                }
        		return;
        	}
        }

        compImage.getActiveImage().notifyImageDisplayListeners(null, true);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  active  DOCUMENT ME!
     */
    public void setActiveVOI_ID(int active) {

        if (active == ViewJComponentEditImage.IMAGE_A) {
            voiIDb = voiID;
            voiID = voiIDa;
        } else {
            voiIDa = voiID;
            voiID = voiIDb;
        }
    }

    /**
     * Sets the graph associated with a VOI as visible.
     */
    public void setGraphVisible() {
        int nVOI;
        ViewVOIVector VOIs;

        VOIs = compImage.getActiveImage().getVOIs();
        nVOI = VOIs.size();

        for (int i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).isActive() && (VOIs.VOIAt(i).isVisible() == true)) {

                if ((VOIs.VOIAt(i).getCurveType() == VOI.POINT) && (VOIs.VOIAt(i).getContourGraph() != null)) {
                    VOIs.VOIAt(i).getContourGraph().setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(0)));

                    if (compImage.getActiveImage().isColorImage() == true) {

                        for (int j = 0; j < VOIs.VOIAt(i).getCurves()[compImage.getSlice()].size(); j++) {

                            if (((VOIPoint) (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j))).isActive()) {
                                graphPointVOI(VOIs.VOIAt(i),
                                              (VOIPoint)
                                              (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j)), j);
                            }
                        }
                    }

                    VOIs.VOIAt(i).getContourGraph().setVisible(true);
                } else if ((VOIs.VOIAt(i).getCurveType() == VOI.POINT) && (VOIs.VOIAt(i).getContourGraph() == null)) {

                    for (int j = 0; j < VOIs.VOIAt(i).getCurves()[compImage.getSlice()].size(); j++) {
                        graphPointVOI(VOIs.VOIAt(i),
                                      (VOIPoint) (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j)), j);
                    }

                    VOIs.VOIAt(i).getContourGraph().setVisible(true);
                }
            }
        }
    }

    /**
     * set the color of the grid.
     *
     * @param  color  Color
     */
    public void setGridColor(Color color) {
        this.gridColor = color;
    }

    /**
     * Tells the grid overlay (if on) to show abc/123 labeling.
     *
     * @param  doLabel  boolean
     */
    public void setGridLabelingOn(boolean doLabel) {
        this.gridLabelingOn = doLabel;
    }

    /**
     * Sets the axis orientation of abc and 123 labeling of the grid overlay.
     *
     * @param  or  boolean true = x-axis numbered, false = x-axis lettered
     */
    public void setGridLabelOrientation(boolean or) {
        this.gridLabelOrientation = or;
    }


    /**
     * Sets whether or not to show the NEI grid overlay.
     *
     * @param  flag  boolean show grid overlay (or not!)
     */
    public void setGridOverlay(boolean flag) {
        gridOverlayOn = flag;
    }

    /**
     * sets the grid spacing (horizontal) in terms of resolution.
     *
     * @param  spacing  float new grid spacing
     */
    public void setGridSpacingX(float spacing) {
        this.gridSpacingX = spacing;
    }

    /**
     * sets the grid spacing (horizontal) in terms of resolution.
     *
     * @param  spacing  float new grid spacing
     */
    public void setGridSpacingY(float spacing) {
        this.gridSpacingY = spacing;
    }

    /**
     * Sets the image's VOI graph buffer.
     *
     * @param  buf  float[]
     */
    public void setImageGraphBuffer(float[] buf) {
        this.graphImgBuff = buf;
    }


    /**
     * Function called to set the mode/drawing state for the VOIHandler/VJCompEditImage The mode change will trigger
     * cursor changes and rubberbanding actions, as well as prepare the mouse listening for actions (moving a VOI,
     * adding points to a VOI etc).
     *
     * @param  mode  int the current mode
     */
    public void setMode(int mode) {
        this.mode = mode;

        rubberband.setActive(false);

        boolean isImageFrame = compImage.getFrame() instanceof ViewJFrameImage;

        switch (mode) {

            case ViewJComponentEditImage.DEFAULT:
                compImage.setCursor(MipavUtil.smallPointerCursor);
                if (isImageFrame) {
                    compImage.getFrame().getControls().getTools().setPointerSelected();
                }

                break;

            case ViewJComponentEditImage.PROBE:
                compImage.setCursor(MipavUtil.probeCursor);
                if (isImageFrame) {
                    compImage.getFrame().getControls().getTools().setPointerSelected();
                }

                break;

            case ViewJComponentEditImage.SELECT:
                rubberband = rbRect;
                rubberband.setActive(true);
                compImage.setCursor(MipavUtil.defaultCursor);
                break;

            case ViewJComponentEditImage.POINT_VOI:
                if (isImageFrame) {
                    compImage.getFrame().getControls().getTools().setVOIButtonSelected("point");
                }

                compImage.setCursor(compImage.crosshairCursor);
                break;

            case ViewJComponentEditImage.RECTANGLE:
                if (isImageFrame) {
                    compImage.getFrame().getControls().getTools().setVOIButtonSelected("rectvoi");
                }

                rubberband = rbRect;
                rubberband.setActive(true);
                compImage.setCursor(compImage.crosshairCursor);
                break;

            case ViewJComponentEditImage.ELLIPSE:
                if (isImageFrame) {
                    compImage.getFrame().getControls().getTools().setVOIButtonSelected("ellipsevoi");
                }

                rubberband = rbEllipse;
                rubberband.setActive(true);
                compImage.setCursor(compImage.crosshairCursor);
                break;

            case ViewJComponentEditImage.RECTANGLE3D:
                if (isImageFrame) {
                    compImage.getFrame().getControls().getTools().setVOIButtonSelected("rect3dvoi");
                }

                rubberband = rbRect;
                rubberband.setActive(true);
                compImage.setCursor(compImage.crosshairCursor);
                break;

            case ViewJComponentEditImage.NEW_VOI:
                compImage.setCursor(compImage.crosshairCursor);
                if (isImageFrame) {
                    compImage.getFrame().getControls().getTools().setPointerSelected();
                }

                // deselect VOIs !!!!!!!
                ViewVOIVector VOIs = compImage.getActiveImage().getVOIs();
                int nVOI = VOIs.size();

                for (int i = 0; i < nVOI; i++) { // deactivate all VOIs
                    VOIs.VOIAt(i).setAllActive(false);
                }

                compImage.getActiveImage().notifyImageDisplayListeners();

                voiID = -1; // -1 indicates new VOI should be created
                rbLivewire = null;
                break;

            case ViewJComponentEditImage.POLYLINE:
                if (isImageFrame) {
                    compImage.getFrame().getControls().getTools().setVOIButtonSelected("polyline");
                }

                rubberband = rbPolyline;
                rubberband.setActive(true);
                compImage.setCursor(compImage.crosshairCursor);
                break;

            case ViewJComponentEditImage.LIVEWIRE:
                if (isImageFrame) {
                    compImage.getFrame().getControls().getTools().setVOIButtonSelected("livewirevoi");
                }

                rubberband = rbLivewire;
                rubberband.setActive(true);
                compImage.setCursor(compImage.crosshairCursor);
                break;

            case ViewJComponentEditImage.LEVELSET:
                if (isImageFrame) {
                    compImage.getFrame().getControls().getTools().setVOIButtonSelected("levelsetvoi");
                }

                compImage.setCursor(compImage.crosshairCursor);
                break;

            case ViewJComponentEditImage.LINE:
                if (isImageFrame) {
                    compImage.getFrame().getControls().getTools().setVOIButtonSelected("line");
                }

                rubberband = rbLine;
                rubberband.setActive(true);
                compImage.setCursor(compImage.crosshairCursor);
                break;

            case ViewJComponentEditImage.PROTRACTOR:
                if (isImageFrame) {
                    compImage.getFrame().getControls().getTools().setVOIButtonSelected("protractor");
                }

                rubberband = rbProtractor;
                rubberband.setActive(true);
                compImage.setCursor(compImage.crosshairCursor);
                break;

            case ViewJComponentEditImage.MOVE:
                compImage.setCursor(MipavUtil.moveCursor);
                break;

            case ViewJComponentEditImage.MOVE_POINT:
                compImage.setCursor(compImage.crosshairCursor);
                break;

            case ViewJComponentEditImage.NEW_POINT:
                compImage.setCursor(MipavUtil.addPointCursor);
                break;

            case ViewJComponentEditImage.POLYLINE_SLICE_VOI:
                if (isImageFrame) {
                    compImage.getFrame().getControls().getTools().setVOIButtonSelected("polyslice");
                }

                compImage.setCursor(compImage.crosshairCursor);
                break;

            case ViewJComponentEditImage.DELETE_POINT:
                compImage.setCursor(compImage.crosshairCursor);
                break;

            case ViewJComponentEditImage.WAND:
                compImage.setCursor(MipavUtil.wandCursor); // Hand cursor
                break;

            case ViewJComponentEditImage.RETRACE:
                compImage.setCursor(compImage.crosshairCursor);
                break;

            case ViewJComponentEditImage.PAINT_VOI:
                compImage.setCursor(MipavUtil.blankCursor);
                compImage.getFrame().requestFocus();
                break;

            case ViewJComponentEditImage.PAINT_CAN:
                compImage.setCursor(compImage.crosshairCursor);
                break;

            case ViewJComponentEditImage.PAINT_VASC:
                compImage.setCursor(compImage.crosshairCursor);
                break;

            case ViewJComponentEditImage.DROPPER_PAINT:
                compImage.setCursor(compImage.crosshairCursor);
                break;

            case ViewJComponentEditImage.ERASER_PAINT:
                compImage.setCursor(MipavUtil.blankCursor);
                compImage.getFrame().requestFocus();
                break;

            case ViewJComponentEditImage.MAG_REGION:
                compImage.setCursor(MipavUtil.magRegionCursor);
                break;

            case ViewJComponentEditImage.WIN_REGION:
                compImage.setCursor(MipavUtil.magRegionCursor);
                break;

            case ViewJComponentEditImage.QUICK_LUT:
                rubberband = rbRectQuick;
                rubberband.setActive(true);
                compImage.setCursor(MipavUtil.quickLUTcursor);
                break;

            case ViewJComponentEditImage.ANNOTATION:
                compImage.setCursor(MipavUtil.textCursor);
                break;

            case ViewJComponentEditImage.ZOOMING_IN:

                Toolkit toolkit = Toolkit.getDefaultToolkit();
                Cursor magnifyCursor = toolkit.createCustomCursor(MipavUtil.getIcon("zoomin.gif").getImage(),
                                                                  new Point(10, 10), "zoomin");

                compImage.setCursor(magnifyCursor);
                break;

            case ViewJComponentEditImage.ZOOMING_OUT:
                toolkit = Toolkit.getDefaultToolkit();

                Cursor unmagnifyCursor = toolkit.createCustomCursor(MipavUtil.getIcon("zoomout.gif").getImage(),
                                                                    new Point(10, 10), "zoomout");

                compImage.setCursor(unmagnifyCursor);
                break;

        }

        rubberband.setPresetHue(presetHue);
    }

    /**
     * Sets the mode to Live wire and sets the cost function of the live wire rubberband.
     *
     * @param  selection  Cost function selection.
     */
    public void setModeLivewire(int selection) {
        this.mode = ViewJComponentEditImage.LIVEWIRE;
        rubberband.setActive(false);
        rbLivewire = new RubberbandLivewire(compImage, selection);
        rbLivewire.setPresetHue(presetHue);
        rubberband = rbLivewire;
        rubberband.setActive(true);
        compImage.setCursor(compImage.crosshairCursor);
    }

    /**
     * Sets the mode to Livewire and provides parameters to the livewire cost function.
     *
     * @param  gradSigma      std dev of gaussian function for the gradient
     * @param  edgeLap        std dev of function for the edge lapacian and determines lo/hi thresholds
     * @param  gradWeight     weight to apply to gradient vector (gradWeight+2*kernWeight == 1)
     * @param  kernWeight     weight to apply to the small kernel (gradWeight+2*kernWeight == 1)
     * @param  smoothVOIFlag  whether to smooth out the voi contour after it is finished
     */
    public void setModeLivewire(float gradSigma, float edgeLap, float gradWeight, float kernWeight,
                                boolean smoothVOIFlag) {
        this.mode = ViewJComponentEditImage.LIVEWIRE;
        rubberband.setActive(false);
        rbLivewire = new RubberbandLivewire(this, new float[] { gradSigma, gradSigma },
                                            new float[] { edgeLap, edgeLap }, kernWeight, gradWeight, -(edgeLap),
                                            edgeLap, smoothVOIFlag);
        rubberband = rbLivewire;
        rubberband.setActive(true);
        compImage.setCursor(compImage.crosshairCursor);
    }

    /**
     * Sets whether or not to show the overlay.
     *
     * @param  flag  boolean that tells whether or not to show the overlay
     */
    public void setOverlay(boolean flag) {
        overlayOn = flag;
    }

    /**
     * Displays the dialog for calculating point area average intensities (for a fixed Point location (x,y)) , and pops
     * up the associated graph when "OK" is pressed.
     */
    public void setPAAIGraphVisible() {
        int nVOI;
        ViewVOIVector VOIs;

        VOIs = compImage.getActiveImage().getVOIs();
        nVOI = VOIs.size();

        Point3Df pt;

        for (int i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.POINT) {

                // get the x,y coords for each active VOIPoint and open up a JDialogPointArea
                for (int j = 0; j < VOIs.VOIAt(i).getCurves()[compImage.getSlice()].size(); j++) {

                    if (((VOIPoint) (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j))).isActive()) {
                        pt = ((VOIPoint) (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j))).exportPoint();
                        new JDialogPointArea(compImage.getFrame(), compImage.getActiveImage(), (int) pt.x, (int) pt.y,
                                             true);
                    }
                }
            }
        }
    }

    /**
     * Sets the hue that will be used by rubberband if >= 0.0.
     *
     * @param  presetHue  the hue to be used
     */
    public void setPresetHue(float presetHue) {
        this.presetHue = presetHue;

        if (rbLevelSet != null) {
            rbLevelSet.setPresetHue(presetHue);
        }

        if (rbLivewire != null) {
            rbLivewire.setPresetHue(presetHue);
        }
    }

    /**
     * Used ONLY by ViewJFrameDualTriImage.
     *
     * @param  ID  The VOI ID number.
     */
    public void setVOI_ID(int ID) {
        voiID = ID;
    }

    /**
     * Typically used to reset VOI ID = -1.
     *
     * @param  ID   new ID.
     * @param  UID  Unique ID of the VOI.
     */
    public void setVOI_IDs(int ID, int UID) {
        voiID = ID;
        this.updateVOIColor(ID, UID);
    }

    /**
     * If a VOI is selected the VOI properties dialog is displayed with the current properites of the selected VOI.
     *
     * @param  openColor  DOCUMENT ME!
     */
    public void showVOIProperties(boolean openColor) {

        if (voiDialog == null) {
            voiDialog = new JDialogVOIStats(compImage.getFrame(), compImage.getActiveImage(), null);
            addVOIUpdateListener(voiDialog);

            if (openColor) {
                voiDialog.showColorChooser();
            }
        }

        if (voiDialog != null) {
            voiDialog.setVisible(true);

            if (openColor) {
                voiDialog.showColorChooser();
            }
        }
    }

    /**
     * Undoes the last VOI manipulation.
     */
    public void undoLastVOI() {

        // Find VOI with same name and replace it with undoVOI.
        int i;
        int nVOI;
        ViewVOIVector VOIs = compImage.getActiveImage().getVOIs();

        nVOI = VOIs.size();

        boolean found = false;

        if (undoVOI == null) {
            return;
        }

        for (i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).getName().equals(undoVOI.getName())) {
                VOIs.removeElementAt(i);
                VOIs.add(undoVOI);
                found = true;
            }
        }

        if (found == false) {
            VOIs.add(undoVOI);
        }

        undoVOI.setAllActive(false);
        compImage.getActiveImage().notifyImageDisplayListeners(null, true);
    }

    /**
     * Fires a VOI selection change event based on the VOI.
     *
     * @param  voi  DOCUMENT ME!
     */
    protected void fireVOISelectionChange(VOI voi) {
        fireVOISelectionChange(voi, null);
    }

    /**
     * Draws a grid on top of the image according to the gridSpacingX and gridSpacingY.
     *
     * @param  g  Graphics the graphics used to draw
     */
    protected void showGridOverlay(Graphics g) {
        g.setColor(gridColor);

        Insets insets = compImage.getFrame().getInsets();
        int rightOffset = compImage.getBounds().width - insets.left;
        int bottomOffset = compImage.getBounds().height - insets.bottom;
        float offset = 0;

        int xDim = compImage.getActiveImage().getExtents()[0];
        int yDim = compImage.getActiveImage().getExtents()[0];

        float resX = compImage.getActiveImage().getResolutions(0)[0];
        float resY = compImage.getActiveImage().getResolutions(0)[1];

        float numVertical = (xDim * resX) / gridSpacingX;
        float numHorizontal = (yDim * resY) / gridSpacingY;

        float verticalSpacing = (xDim / numVertical) * compImage.getZoomX();
        float horizontalSpacing = (yDim / numHorizontal) * compImage.getZoomY();

        for (int i = 0; i < numVertical; i++, offset += verticalSpacing) {
            g.drawLine(MipavMath.round(offset), 0, MipavMath.round(offset), bottomOffset);
        }

        offset = 0;

        for (int i = 0; i < numHorizontal; i++, offset += horizontalSpacing) {
            g.drawLine(0, MipavMath.round(offset), rightOffset, MipavMath.round(offset));
        }

        if (gridLabelingOn) {
            int i, j;
            float xOffset = 0;
            float yOffset = 0;
            String gridLabel = "";

            float xPadding = 2 + (2 * compImage.getZoomX());
            float yPadding = 15 + (2 * compImage.getZoomY());

            char alphaLabel;

            for (i = 0; i < numHorizontal; i++, xOffset += horizontalSpacing) {

                for (j = 0, yOffset = 0; j < numVertical; j++, yOffset += verticalSpacing) {

                    if ((i == 0) || (j == 0)) {

                        if (gridLabelOrientation) {

                            // x-axis is 1, 2, 3, 4... y-axis is a, b, c, d
                            alphaLabel = (char) (97 + j);

                            gridLabel = Integer.toString((i + 1)) + alphaLabel;

                        } else {
                            alphaLabel = (char) (97 + i);
                            gridLabel = alphaLabel + Integer.toString((j + 1));
                        }

                        g.drawString(gridLabel, MipavMath.round(yOffset + xPadding),
                                     MipavMath.round(xOffset + yPadding));

                    }
                }

            }
        }

    }


    /**
     * Displays Image overlays (DICOM or image attributes).
     *
     * @param  g  Graphics object used to paint and display the strings.
     */
    protected void showOverlay(Graphics g) {
        String[] overlays = new String[16];
        String[] overlayNames = new String[16];

        if (compImage.getActiveImage().getFileInfo(0) instanceof FileInfoDicom) {
            FileInfoDicom fileInfo;

            if (compImage.getSlice() >= 0) {
                fileInfo = (FileInfoDicom) (compImage.getActiveImage().getFileInfo())[compImage.getSlice()];
            } else {
                fileInfo = (FileInfoDicom) (compImage.getActiveImage().getFileInfo())[0];
            }

            String[] dicomKeys = Preferences.getOverlays(true);
            overlayNames = Preferences.getOverlayNames(true);

            for (int i = 0; i < 16; i++) {

                if ((dicomKeys[i] != null) && !dicomKeys[i].equals("-")) {
                    overlays[i] = buildOverlayStrings(fileInfo, overlayNames[i], dicomKeys[i]);

                    if (overlays[i] != null) {
                        //        System.err.println(i + ": OVERLAY NAME DICOM: " + overlayNames[i] +        ", key: " +
                        // dicomKeys[i] +                           ", overlay: " + overlays[i]);
                    }
                }
            }

            Insets insets = compImage.getFrame().getInsets();
            int rightOffset = compImage.getBounds().width - insets.left;
            int bottomOffset = compImage.getBounds().height - insets.bottom - 15;

            int len;

            for (int i = 0; i < 16; i++) {

                if (overlays[i] != null) {
                    len = g.getFontMetrics(g.getFont()).stringWidth(overlays[i]);

                    if (i < 4) {
                        compImage.drawStringBW(overlays[i], g, 5, (15 * (i + 1)));
                    } else if ((i > 3) && (i < 8)) {
                        compImage.drawStringBW(overlays[i], g, rightOffset - len, (15 * ((i % 4) + 1)));
                    } else if ((i > 7) && (i < 12)) {
                        compImage.drawStringBW(overlays[i], g, 5, bottomOffset - 45 + (i % 4 * 15));
                    } else if (i > 11) {
                        compImage.drawStringBW(overlays[i], g, rightOffset - len, bottomOffset - 45 + (i % 4 * 15));
                    }
                    // System.err.println(i + ": should have drawn a string somewhere: " + overlays[i]);
                }
            }

            compImage.drawGradicules(g, fileInfo.getResolutions()[0], fileInfo.getResolutions()[1]);

            /*
             * float reconDiameter;
             *
             * try { reconDiameter = Float.valueOf((String) (fileInfo.getTag("0018,1100").getValue(true))).floatValue(); }
             * catch (Exception ex) { reconDiameter = compImage.getActiveImage().getExtents()[0] *
             * fileInfo.getResolutions()[0]; }
             *
             * String[] values = compImage.setOverlayValues(compImage.getActiveImage().getImageOrientation(),
             * MipavMath.round(fileInfo.xLocation),                                MipavMath.round(fileInfo.yLocation),
             * MipavMath.round(fileInfo.zLocation),                                MipavMath.round(reconDiameter));
             *
             * int index = values[0].length() / 2;
             *
             * for (int i = 0; i < values[0].length(); i++) { compImage.drawStringBW(String.valueOf(values[0].charAt(i)),
             * g, 5, (getHeight() / 2) - ((index - i) * 15)); }
             *
             * index = values[1].length() / 2;
             *
             * for (int i = 0; i < values[1].length(); i++) { compImage.drawStringBW(String.valueOf(values[1].charAt(i)),
             * g, rightOffset - 10,              (getHeight() / 2) - ((index - i) * 15)); }
             *
             * len = g.getFontMetrics(g.getFont()).stringWidth(values[2]); compImage.drawStringBW(values[2], g,
             * (getWidth() / 2) - (len / 2), 15); len = g.getFontMetrics(g.getFont()).stringWidth(values[3]);
             * compImage.drawStringBW(values[3], g, (getWidth() / 2) - (len / 2), bottomOffset);
             * compImage.drawStringBW(values[4], g, 5, 75);
             */
        } else {
            FileInfoBase fileInfo;

            if (compImage.getSlice() >= 0) {
                fileInfo = compImage.getActiveImage().getFileInfo()[compImage.getSlice()];
            } else {
                fileInfo = compImage.getActiveImage().getFileInfo()[0];
            }

            String[] attribs = Preferences.getOverlays(false);

            overlayNames = Preferences.getOverlayNames(false);

            for (int i = 0; i < 16; i++) {

                if ((attribs[i] != null) && !attribs[i].equals("-")) {
                    overlays[i] = buildOverlayStrings(fileInfo, overlayNames[i], attribs[i]);
                }
            }

            Insets insets = compImage.getFrame().getInsets();
            int rightOffset = compImage.getBounds().width - insets.left;
            int bottomOffset = compImage.getBounds().height - insets.bottom - 15;

            int len;

            for (int i = 0; i < 16; i++) {

                if (overlays[i] != null) {
                    len = g.getFontMetrics(g.getFont()).stringWidth(overlays[i]);

                    if (i < 4) {
                        compImage.drawStringBW(overlays[i], g, 5, (15 * (i + 1)));
                    } else if ((i > 3) && (i < 8)) {
                        compImage.drawStringBW(overlays[i], g, rightOffset - len, (15 * ((i % 4) + 1)));
                    } else if ((i > 7) && (i < 12)) {
                        compImage.drawStringBW(overlays[i], g, 5, bottomOffset - 45 + (i % 4 * 15));
                    } else if (i > 11) {
                        compImage.drawStringBW(overlays[i], g, rightOffset - len, bottomOffset - 45 + (i % 4 * 15));
                    }
                }
            }
        }
    }

    /**
     * Update the voi color.
     *
     * @param  voiColor  the new voi color
     * @param  voiUID    the last voi id
     */
    protected void updateVOIColor(Color voiColor, int voiUID) {

        if (compImage.getFrame() instanceof ViewJFrameImage) {
            ((ViewJFrameImage) compImage.getFrame()).getControls().setVOIColor(voiColor);
            ((ViewJFrameImage) compImage.getFrame()).setLastVOI_UID(voiUID);
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  id      DOCUMENT ME!
     * @param  voiUID  DOCUMENT ME!
     */
    protected void updateVOIColor(int id, int voiUID) {

        if (compImage.getFrame() instanceof ViewJFrameImage) {
            ((ViewJFrameImage) compImage.getFrame()).getControls().setVOIColor(id);
            ((ViewJFrameImage) compImage.getFrame()).setLastVOI_UID(voiUID);
        }
    }

    /**
     * Builds the overlay Strings from the tag's value. Concatenates the output strings from the tags and ensures that
     * any properly read-in string has usable (if empty) values.
     *
     * @param   inf           The FileInfo with DICOM tags to display.
     * @param   name          DOCUMENT ME!
     * @param   attribString  Key of tag to display.
     *
     * @return  null when value is not a String or when the tag does not exist.
     */
    private String buildOverlayStrings(FileInfoBase inf, String name, String attribString) {

        if (inf instanceof FileInfoDicom) {

            try {

                if ((attribString == null) || (attribString == "")) {
                    return null;
                }

                Object val = ((FileInfoDicom) inf).getTag(attribString).getValue(true);

                if (val == null) {
                    return null;
                } else if ((name != null) && !(name.trim().equals(""))) {
                    return (name + " - " + val);
                } else {
                    return val.toString();
                }
            } catch (IllegalArgumentException ex) {
                Preferences.debug("Illegal arg on: " + attribString);

                return null;
            } catch (ClassCastException notStr) {
                Preferences.debug("Creating strings for DICOM overlay for " + attribString +
                                  " but encountered a ClassCastException.\n", 4);

                return null;
            } catch (NullPointerException noTag) {
                Preferences.debug("Creating strings for DICOM overlay for " + attribString +
                                  " but encountered a NullPointerException.\n", 4);

                return null;
            }
        } else { // FileInfo is NOT DICOM

            if (attribString == null) {
                return null;
            }

            String resultStr = new String();

            if ((name != null) && (name != "")) {
                resultStr = name + " - ";
            }

            String[] atts = JDialogOverlay.attribStr;

            if (attribString.equals(atts[0])) {
                return resultStr + Integer.toString(inf.getExtents()[0]);
            } else if (attribString.equals(atts[1])) {
                return resultStr + Integer.toString(inf.getExtents()[1]);
            } else if (attribString.equals(atts[2])) {

                if (inf.getExtents().length > 2) {
                    return resultStr + Integer.toString(inf.getExtents()[2]);
                }
            } else if (attribString.equals(atts[3])) {

                if (inf.getExtents().length > 3) {
                    return resultStr + Integer.toString(inf.getExtents()[3]);
                }
            } else if (attribString.equals(atts[4])) {
                return resultStr + compImage.getActiveImage().getTypeString();
            } else if (attribString.equals(atts[5])) {
                return resultStr + Double.toString(compImage.getActiveImage().getMin());
            } else if (attribString.equals(atts[6])) {
                return resultStr + Double.toString(compImage.getActiveImage().getMax());
            } else if (attribString.equals(atts[7])) {
                return resultStr + FileInfoBase.getImageOrientationStr(inf.getImageOrientation());
            } else if (attribString.equals(atts[8])) {
                return resultStr + FileInfoBase.getAxisOrientationStr(inf.getAxisOrientation(0));
            } else if (attribString.equals(atts[9])) {
                return resultStr + FileInfoBase.getAxisOrientationStr(inf.getAxisOrientation(1));
            } else if (attribString.equals(atts[10])) {

                if (inf.getExtents().length > 2) {
                    return resultStr + FileInfoBase.getAxisOrientationStr(inf.getAxisOrientation(2));
                }
            } else if (attribString.equals(atts[11])) {
                return new String(resultStr + inf.getResolutions()[0] + " " +
                                  FileInfoBase.getUnitsOfMeasureAbbrevStr(inf.getUnitsOfMeasure()[0]));
            } else if (attribString.equals(atts[12])) {
                return new String(resultStr + inf.getResolutions()[1] + " " +
                                  FileInfoBase.getUnitsOfMeasureAbbrevStr(inf.getUnitsOfMeasure()[1]));
            } else if (attribString.equals(atts[13])) {

                if (inf.getExtents().length > 2) {
                    return new String(resultStr + inf.getResolutions()[2] + " " +
                                      FileInfoBase.getUnitsOfMeasureAbbrevStr(inf.getUnitsOfMeasure()[2]));
                }
            } else if (attribString.equals(atts[14])) {

                if (inf.getExtents().length > 3) {
                    return new String(resultStr + inf.getResolutions()[3] + " " +
                                      FileInfoBase.getUnitsOfMeasureAbbrevStr(inf.getUnitsOfMeasure()[3]));
                }
            } else if (attribString.equals(atts[15])) {
                return resultStr + Float.toString(inf.getSliceThickness());
            } else if (attribString.equals(atts[16])) {
                return resultStr + Float.toString(inf.getOrigin()[0]);
            } else if (attribString.equals(atts[17])) {
                return resultStr + Float.toString(inf.getOrigin()[1]);
            } else if (attribString.equals(atts[18])) {

                if (inf.getExtents().length > 2) {
                    return resultStr + Float.toString(inf.getOrigin()[2]);
                }
            } else if (attribString.equals(atts[19])) {

                if (inf.getExtents().length > 3) {
                    return resultStr + Float.toString(inf.getOrigin()[3]);
                }
            } else if (attribString.equals(atts[20])) {

                if (inf.getEndianess()) {
                    return resultStr + "big endian";
                } else {
                    return resultStr + "little endian";
                }
            } else if (attribString.equals(atts[21])) {
                return resultStr + FileInfoBase.getTransformIDStr(inf.getTransformID());
            }

            return null;

        }
    }

    /**
     * Handles cycling through contours or VOIs left or right = cycle through contours up or down = cycle through VOIs.
     *
     * @param  keyCode  int directional arrow key
     */
    private void cycleVOI(int keyCode) {
        int end = 0;
        int size = 0;

        ViewVOIVector VOIs = compImage.getActiveImage().getVOIs();
        int nVOI = VOIs.size();

        boolean contourOnly = ((keyCode == KeyEvent.VK_LEFT) || (keyCode == KeyEvent.VK_RIGHT));

        for (int i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).isActive()) {

                if (compImage.getActiveImage().getNDims() < 3) {
                    end = 1;
                } else {
                    end = compImage.getActiveImage().getExtents()[2];
                }

                if (contourOnly) {

                    if (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE_SLICE) {

                        // do nothing...return
                        return;
                    }

                    // if they are all active, set the 1st contour to active
                    if (allActive) {
                        VOIs.VOIAt(i).setAllActive(false);

                        // sketchy : ( (VOIBase) (VOIs.VOIAt(i).getCurves()[0].elementAt(0))).setActive(true);
                        compImage.getActiveImage().notifyImageDisplayListeners();
                    } else {

                        for (int sl = 0; sl < end; sl++) {
                            size = VOIs.VOIAt(i).getCurves()[sl].size();

                            for (int j = 0; j < size; j++) {

                                if (((VOIBase) (VOIs.VOIAt(i).getCurves()[sl].elementAt(j))).isActive()) {
                                    ((VOIBase) (VOIs.VOIAt(i).getCurves()[sl].elementAt(j))).setActive(false);

                                    int index = 0;


                                    if (keyCode == KeyEvent.VK_RIGHT) {

                                        if ((j + 1) < size) {
                                            index = j + 1;
                                        } else {
                                            index = 0;
                                        }
                                    } else {

                                        if ((j - 1) >= 0) {
                                            index = j - 1;
                                        } else {
                                            index = size - 1;
                                        }
                                    }

                                    ((VOIBase) (VOIs.VOIAt(i).getCurves()[sl].elementAt(index))).setActive(true);

                                    Point3Df pt = ((VOIBase) (VOIs.VOIAt(i).getCurves()[sl].elementAt(index)))
                                                      .getActivePt();

                                    if (pt != null) {
                                        compImage.setPixelInformationAtLocation((int) pt.x, (int) pt.y);
                                    }

                                    compImage.getActiveImage().notifyImageDisplayListeners();
                                    fireVOISelectionChange(VOIs.VOIAt(i));

                                    return;
                                }
                            }
                        }
                    }
                }
                // end if contourOnly
                else { // for cycling between whole VOIs (not contours)

                    VOIs.VOIAt(i).setActive(false);
                    VOIs.VOIAt(i).setAllActive(false);

                    int index = 0;

                    if (keyCode == KeyEvent.VK_UP) {

                        if ((i + 1) < nVOI) {
                            index = i + 1;
                        }
                    } else {

                        if ((i - 1) >= 0) {
                            index = i - 1;
                        } else {
                            index = nVOI - 1;
                        }
                    }

                    VOIs.VOIAt(index).setActive(true);

                    if (VOIs.VOIAt(index).getCurveType() == VOI.POLYLINE_SLICE) {
                        VOIs.VOIAt(index).setAllActive(true);
                        allActive = true;
                    }

                    for (int sl = 0; sl < end; sl++) {
                        size = VOIs.VOIAt(index).getCurves()[sl].size();

                        if (size > 0) {
                            ((VOIBase) (VOIs.VOIAt(index).getCurves()[sl].elementAt(0))).setActive(true);

                            Point3Df pt = ((VOIBase) (VOIs.VOIAt(index).getCurves()[sl].elementAt(0))).getActivePt();

                            if (pt != null) {
                                compImage.setPixelInformationAtLocation((int) pt.x, (int) pt.y);
                            }

                            break;
                        }
                    }

                    fireVOISelectionChange(VOIs.VOIAt(index));
                    compImage.getActiveImage().notifyImageDisplayListeners();

                    return;
                }
            }
        }
    }


    /**
     * Graphs/updates graph of Line VOI intensities.
     *
     * @param  voiIndex      int index of active Line VOI
     * @param  contourIndex  int index of line's contour (can use zero)
     */
    private void graphLineVOI(int voiIndex, int contourIndex) {
        float[] lineX = null;
        float[] lineY = null;
        float[] lineZ = null;
        lineX = new float[2];
        lineY = new float[2];
        lineZ = new float[2];

        float[] position;
        float[] intensity;
        ViewJFrameGraph lineGraph;
        int length;
        float[][] rgbPositions;
        float[][] rgbIntensities;

        float[][] rgbPos = null;
        float[][] rgbInten = null;
        int m;

        ViewVOIVector VOIs = compImage.getActiveImage().getVOIs();

        VOIs.VOIAt(voiIndex).exportArrays(lineX, lineY, lineZ, compImage.getSlice(), contourIndex);

        if (compImage.getActiveImage().isColorImage() == true) {

            length = (int)
                         (Math.sqrt(((lineX[1] - lineX[0]) * (lineX[1] - lineX[0])) +
                                    ((lineY[1] - lineY[0]) * (lineY[1] - lineY[0]))));
            rgbPositions = new float[3][(length * 2) + 1];
            rgbIntensities = new float[3][(length * 2) + 1];

            for (int c = 0; c < 3; c++) {
                int pt = ((VOILine) (VOIs.VOIAt(voiIndex).getCurves()[compImage.getSlice()].elementAt(contourIndex)))
                             .findPositionAndIntensityRGB(rgbPositions[c], rgbIntensities[c], c,
                                                              compImage.getActiveImageBuffer(),
                                                              compImage.getActiveImage().getResolutions(compImage.getSlice()),
                                                              compImage.getActiveImage().getExtents()[0],
                                                              compImage.getActiveImage().getExtents()[1]);

                if (c == 0) {
                    rgbPos = new float[3][pt];
                    rgbInten = new float[3][pt];
                }

                for (m = 0; m < pt; m++) {
                    rgbPos[c][m] = rgbPositions[c][m];
                    rgbInten[c][m] = rgbIntensities[c][m];
                }
            }

            if (VOIs.VOIAt(voiIndex).getContourGraph() == null) {
                ViewJFrameGraph contourGraph = new ViewJFrameGraph(rgbPos, rgbInten, "Intensity Graph",
                                                                   VOIs.VOIAt(voiIndex),
                                                                   FileInfoBase.getUnitsOfMeasureAbbrevStr(compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(0)));

                contourGraph.setDefaultDirectory(compImage.getActiveImage().getUserInterface().getDefaultDirectory());
                contourGraph.setVisible(true);
                VOIs.VOIAt(voiIndex).setContourGraph(contourGraph);
                contourGraph.setVOI(VOIs.VOIAt(voiIndex));
            } else {
                VOIs.VOIAt(voiIndex).getContourGraph().setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(0)));
                VOIs.VOIAt(voiIndex).getContourGraph().saveNewFunction(rgbPos, rgbInten, contourIndex);
            }

            return;
        } else {

            length = (int)
                         (Math.sqrt(((lineX[1] - lineX[0]) * (lineX[1] - lineX[0])) +
                                    ((lineY[1] - lineY[0]) * (lineY[1] - lineY[0]))));
            position = new float[(length * 2) + 1];
            intensity = new float[(length * 2) + 1];

            int pt = VOIs.VOIAt(voiIndex).findPositionAndIntensity(compImage.getSlice(), contourIndex, position,
                                                                   intensity, compImage.getActiveImageBuffer(),
                                                                   compImage.getActiveImage().getFileInfo()[compImage.getSlice()].getResolutions(),
                                                                   compImage.getActiveImage().getExtents()[0],
                                                                   compImage.getActiveImage().getExtents()[1]);
            float[] pos = new float[pt];
            float[] inten = new float[pt];

            for (m = 0; m < pt; m++) {
                pos[m] = position[m];
                inten[m] = intensity[m];
            }

            if (VOIs.VOIAt(voiIndex).getContourGraph() == null) {
                lineGraph = new ViewJFrameGraph(pos, inten, "Line VOI Graph", VOIs.VOIAt(voiIndex),
                                                FileInfoBase.getUnitsOfMeasureAbbrevStr(compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(0)));
                lineGraph.setDefaultDirectory(compImage.getActiveImage().getUserInterface().getDefaultDirectory());
                lineGraph.setVisible(true);
                VOIs.VOIAt(voiIndex).setContourGraph(lineGraph);
                lineGraph.setVOI(VOIs.VOIAt(voiIndex));
            } else {
                VOIs.VOIAt(voiIndex).getContourGraph().setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(0)));
                VOIs.VOIAt(voiIndex).getContourGraph().replaceFunction(pos, inten, VOIs.VOIAt(voiIndex), contourIndex);
            }

            // update...*/
            return;
        }
    }


    /**
     * Handles cycling/moving the active point of a contour (must be active).
     *
     * @param  keyCode  int (arrow key)
     * @param  doMove   boolean (true = move, false = cycle)
     */
    private void handleVOIActivePt(int keyCode, boolean doMove) {
        int end = 0;

        ViewVOIVector VOIs = compImage.getActiveImage().getVOIs();
        int nVOI = VOIs.size();
        int xDim = compImage.getActiveImage().getExtents()[0];
        int yDim = compImage.getActiveImage().getExtents()[1];

        for (int i = 0; i < nVOI; i++) {
            int curveType = VOIs.VOIAt(i).getCurveType();

            if (VOIs.VOIAt(i).isActive() && VOIs.VOIAt(i).isVisible()) {

                if (((curveType == VOI.CONTOUR) || (curveType == VOI.POLYLINE) || (curveType == VOI.LINE) ||
                         (curveType == VOI.POINT) || (curveType == VOI.POLYLINE_SLICE))) {

                    if (compImage.getActiveImage().getNDims() < 3) {
                        end = 1;
                    } else {
                        end = compImage.getActiveImage().getExtents()[2];
                    }

                    // System.err.println("Doin poly stuff...all active is: " + allActive);
                    if (allActive) {

                        if (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE_SLICE) {

                            if (doMove) {
                                VOIs.VOIAt(i).moveVOI(compImage.getSlice(), xDim, yDim,
                                                      compImage.getActiveImage().getExtents()[2], 0, 0, keyCode);

                                Point3Df pt = VOIs.VOIAt(i).exportPSlicePoint(compImage.getSlice());

                                if (pt != null) {
                                    compImage.setPixelInformationAtLocation((int) pt.x, (int) pt.y);
                                }

                                compImage.getActiveImage().notifyImageDisplayListeners();
                            } else {
                                VOIs.VOIAt(i).cyclePSlicePt(compImage.getSlice(), keyCode);
                                compImage.getActiveImage().notifyImageDisplayListeners();
                            }
                        }

                        return;
                    } else {

                        for (int sl = 0; sl < end; sl++) {

                            for (int j = 0; j < VOIs.VOIAt(i).getCurves()[sl].size(); j++) {

                                if (((VOIBase) (VOIs.VOIAt(i).getCurves()[sl].elementAt(j))).isActive()) {

                                    if (doMove) {
                                        ((VOIBase) (VOIs.VOIAt(i).getCurves()[sl].elementAt(j))).moveActivePt(keyCode,
                                                                                                              xDim,
                                                                                                              yDim);
                                    } else {
                                        ((VOIBase) (VOIs.VOIAt(i).getCurves()[sl].elementAt(j))).cycleActivePt(keyCode);
                                    }

                                    // show the active point's new (or dif active pt) location

                                    Point3Df pt = ((VOIBase) (VOIs.VOIAt(i).getCurves()[sl].elementAt(j))).getActivePt();

                                    if (pt != null) {
                                        compImage.setPixelInformationAtLocation((int) pt.x, (int) pt.y);
                                    }

                                    compImage.getActiveImage().notifyImageDisplayListeners();

                                    return;
                                }
                            }
                        }
                    }
                }
            }

        }

        return;
    }

    /**
     * Allows user to move any active VOI using the up/down/left/right keys.
     *
     * @param  keyCode  int (only up down left and right do anything)
     */
    private void moveVOI(int keyCode) {

        int distX = 0;
        int distY = 0;
        int end = 0;

        switch (keyCode) {

            case KeyEvent.VK_UP:
                distY = -1;
                break;

            case KeyEvent.VK_DOWN:
                distY = 1;
                break;

            case KeyEvent.VK_LEFT:
                distX = -1;
                break;

            case KeyEvent.VK_RIGHT:
                distX = 1;
                break;

            default:
                return;
        }

        ViewVOIVector VOIs = compImage.getActiveImage().getVOIs();
        int nVOI = VOIs.size();
        int xDim = compImage.getActiveImage().getExtents()[0];
        int yDim = compImage.getActiveImage().getExtents()[1];
        int zDim = 1;

        if (compImage.getActiveImage().getNDims() > 2) {
            zDim = compImage.getActiveImage().getExtents()[2];
        }

        for (int i = 0; i < nVOI; i++) {
            int curveType = VOIs.VOIAt(i).getCurveType();

            if (VOIs.VOIAt(i).isActive() && VOIs.VOIAt(i).isVisible()) {

                if (((curveType == VOI.CONTOUR) || (curveType == VOI.POLYLINE) || (curveType == VOI.LINE) ||
                         (curveType == VOI.PROTRACTOR) || (curveType == VOI.POINT) ||
                         (curveType == VOI.POLYLINE_SLICE))) {

                    if (compImage.getActiveImage().getNDims() < 3) {
                        end = 1;
                    } else {
                        end = compImage.getActiveImage().getExtents()[2];
                    }


                    if (allActive) {

                        if (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE_SLICE) {
                            VOIs.VOIAt(i).moveVOI(compImage.getSlice(), xDim, yDim, zDim, distX, distY, 0);
                        } else {
                            VOIs.VOIAt(i).moveVOI(-1, xDim, yDim, zDim, distX, distY, 0);
                        }

                        compImage.getActiveImage().notifyImageDisplayListeners();
                    } else {

                        for (int sl = 0; sl < end; sl++) {

                            for (int j = 0; j < VOIs.VOIAt(i).getCurves()[sl].size(); j++) {

                                if (((VOIBase) (VOIs.VOIAt(i).getCurves()[sl].elementAt(j))).isActive()) {
                                    VOIs.VOIAt(i).moveVOI(sl, xDim, yDim, zDim, distX, distY, 0);

                                    Point3Df pt = ((VOIBase) (VOIs.VOIAt(i).getCurves()[sl].elementAt(j))).getActivePt();

                                    if (pt != null) {
                                        compImage.setPixelInformationAtLocation((int) pt.x, (int) pt.y);
                                    }

                                    compImage.getActiveImage().notifyImageDisplayListeners();
                                }
                            }
                        }
                    }
                }
            }

        }
    }


    /**
     * Scales the points of the polygon so that it is displayed at the correct size.
     *
     * @param   gon    the original polygon
     * @param   zoomX  the scale in the x dimension
     * @param   zoomY  the scale in the y dimension
     *
     * @return  returns the zoomed polygon
     */
    private Polygon zoomPolygon(Polygon gon, float zoomX, float zoomY) {
        Polygon zoomedGon = null;

        if ((zoomX == 1.0f) && (zoomY == 1.0f)) {
            return gon;
        }

        try {
            zoomedGon = new Polygon();
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentEditImage.zoomPolygon");

            return null;
        }

        for (int i = 0; i < gon.npoints; i++) {
            zoomedGon.addPoint(MipavMath.round(gon.xpoints[i] * zoomX), MipavMath.round(gon.ypoints[i] * zoomY));
        }

        return zoomedGon;
    }

}
