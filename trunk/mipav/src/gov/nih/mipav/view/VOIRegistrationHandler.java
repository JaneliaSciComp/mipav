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
        Graphics g = getGraphics();
        int i, j, m;
        int nVOI;
        ViewVOIVector VOIs = compImage.getActiveImage().getVOIs();
        FileInfoBase fileInfo;
        int xS, yS;
        int distX, distY;
        int xDim, yDim;
        int zDim = 1;

        float[] lineX = new float[2];
        float[] lineY = new float[2];
        float[] lineZ = new float[2];
        float[] position;
        float[] intensity;
        String str;
        int windowChange, levelChange;

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

        int end = 1;

        nVOI = VOIs.size();

        if (mode == ViewJComponentEditImage.MOVE) {

            // System.out.println(" ViewJComponentEditImage.mouseDragged: nVOIs = " + nVOI);
            for (i = 0; i < nVOI; i++) {
                int curveType = VOIs.VOIAt(i).getCurveType();

                if (VOIs.VOIAt(i).isActive() && VOIs.VOIAt(i).isVisible()) {

                    if (((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) ||
                             (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE) ||
                             (VOIs.VOIAt(i).getCurveType() == VOI.LINE) ||
                             (VOIs.VOIAt(i).getCurveType() == VOI.PROTRACTOR)) &&
                            (mouseEvent.isControlDown() == false) &&
                            (mouseEvent.getModifiers() != MouseEvent.BUTTON3_MASK)) {

                        if (compImage.getActiveImage().getNDims() < 3) {
                            end = 1;
                        } else {
                            end = compImage.getActiveImage().getExtents()[2];
                        }

                        if (allActive) {

                            // System.err.println("Doing shiftDown");
                            VOIs.VOIAt(i).moveVOI(-1, xDim, yDim, zDim, distX, distY, 0);
                            compImage.getActiveImage().notifyImageDisplayListeners();
                        } else {

                            // System.err.println("Not Doing shiftDown");
                            for (int sl = 0; sl < end; sl++) {

                                for (j = 0; j < VOIs.VOIAt(i).getCurves()[sl].size(); j++) {
                                    boolean contains = false;
                                    boolean isActive = false;

                                    if ((curveType == VOI.CONTOUR) || (curveType == VOI.POLYLINE)) {

                                        // System.out.println(" j = " + j + " size = " +
                                        // VOIs.VOIAt(i).getCurves()[sl].size() + " sl = " + sl + " Slice " + compImage.getSlice() );
                                        contains = ((VOIContour) (VOIs.VOIAt(i).getCurves()[sl].elementAt(j))).contains(xS,
                                                                                                                        yS,
                                                                                                                        true);

                                        isActive = ((VOIContour) (VOIs.VOIAt(i).getCurves()[sl].elementAt(j)))
                                                       .isActive();
                                        // System.err.println("Contains is: " + contains + " active is: " + isActive + "
                                        // for compImage.getSlice() " + sl);
                                    } else if (curveType == VOI.LINE) {
                                        contains = ((VOILine) (VOIs.VOIAt(i).getCurves()[sl].elementAt(j))).contains(xS,
                                                                                                                     yS,
                                                                                                                     true);
                                        isActive = ((VOILine) (VOIs.VOIAt(i).getCurves()[sl].elementAt(j))).isActive();
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

                                        for (s = 0, foundCurve = false; s < compImage.getActiveImage().getExtents()[2]; s++) {

                                            try {

                                                for (int c = 0; c < 3; c++) {
                                                    numPixels = 0;

                                                    for (j = 0, intensitySum = 0;
                                                             j < VOIs.VOIAt(i).getCurves()[s].size(); j++) {

                                                        if (((VOIContour) VOIs.VOIAt(i).getCurves()[s].elementAt(j))
                                                                .isActive() || foundCurve) {

                                                            if (!foundCurve) {
                                                                compImage.getActiveImage().exportData(s * length * 4, length * 4,
                                                                                       getImageGraphBuffer());
                                                            } // locks and releases lock

                                                            intensitySum += ((VOIContour) (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                                .calcRGBIntensity(getImageGraphBuffer(),
                                                                                                      compImage.getActiveImage().getExtents()[0],
                                                                                                      c);
                                                            numPixels += ((VOIContour) (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
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

                                        for (s = 0, foundCurve = false; s < compImage.getActiveImage().getExtents()[2]; s++) {

                                            try {
                                                numPixels = 0;

                                                for (j = 0, intensitySum = 0; j < VOIs.VOIAt(i).getCurves()[s].size();
                                                         j++) {
                                                    boolean isActive = ((VOIContour) (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                           .isActive();

                                                    if (isActive || foundCurve) {

                                                        if (!foundCurve) {
                                                            compImage.getActiveImage().exportData(s * length, length, getImageGraphBuffer());
                                                        } // locks and releases lock

                                                        intensitySum += ((VOIContour) (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                            .calcIntensity(getImageGraphBuffer(),
                                                                                               compImage.getActiveImage().getExtents()[0]);
                                                        numPixels += ((VOIContour) (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
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

                                            for (s = 0, intensitySum = 0; s < compImage.getActiveImage().getExtents()[2]; s++) {
                                                compImage.getActiveImage().exportData((t * xDim * yDim * zDim) + (s * xDim * yDim),
                                                                       length, getImageGraphBuffer()); // locks and releases lock

                                                for (j = 0; j < VOIs.VOIAt(i).getCurves()[s].size(); j++) {
                                                    intensitySum += ((VOIContour) (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                        .calcIntensity(getImageGraphBuffer(),
                                                                                           compImage.getActiveImage().getExtents()[0]);
                                                    numPixels += ((VOIContour) (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
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
                    } else if (VOIs.VOIAt(i).getCurveType() == VOI.POINT) {
                        setCursor(compImage.crosshairCursor);

                        if (allActive) {
                            VOIs.VOIAt(i).moveVOI(-1, xDim, yDim, zDim, distX, distY, 0);
                            // System.err.println("Doing point shift down");
                        } else {

                            // System.err.println("Not doing point shift down");
                            VOIs.VOIAt(i).moveVOI(compImage.getSlice(), xDim, yDim, zDim, distX, distY, 0);
                        }

                        compImage.getActiveImage().notifyImageDisplayListeners();

                        if (VOIs.VOIAt(i).getCurveType() == VOI.POINT) {
                            Point3Df pt;

                            for (j = 0; j < VOIs.VOIAt(i).getCurves()[compImage.getSlice()].size(); j++) {

                                if (((VOIPoint) (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j))).isActive()) {

                                    if (compImage.getActiveImage().getNDims() == 3) {

                                        if (compImage.getActiveImage().isColorImage() == true) {

                                            for (int s = 0; s < compImage.getActiveImage().getExtents()[2]; s++) {
                                                pt = ((VOIPoint) (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j)))
                                                         .exportPoint();

                                                for (int c = 0; c < 3; c++) {

                                                    if ((getPointRGBPositions() != null) && (getPointRGBIntensities() != null)) {
                                                        getPointRGBPositions()[c][s] = s;
                                                        getPointRGBIntensities()[c][s] = compImage.getActiveImage().getFloat((int) ((4 *
                                                                                                                  ((s *
                                                                                                                        compImage.getActiveImage().getExtents()[0] *
                                                                                                                        compImage.getActiveImage().getExtents()[1]) +
                                                                                                                       (pt.y *
                                                                                                                            compImage.getActiveImage().getExtents()[0]) +
                                                                                                                       pt.x)) +
                                                                                                             c + 1));
                                                    }
                                                }
                                            }

                                            if (VOIs.VOIAt(i).getContourGraph() != null) {
                                                VOIs.VOIAt(i).getContourGraph().update(getPointRGBPositions(), getPointRGBIntensities(),
                                                                                       j);
                                                VOIs.VOIAt(i).getContourGraph().setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(0)));
                                            }
                                        } else {

                                            for (int s = 0; s < compImage.getActiveImage().getExtents()[2]; s++) {

                                                pt = ((VOIPoint) (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j)))
                                                         .exportPoint();

                                                if ((getPointPositions() != null) && (getPointIntensities() != null)) {
                                                    getPointPositions()[s] = s;
                                                    getPointIntensities()[s] = compImage.getActiveImage().getFloat((int) ((s *
                                                                                                      compImage.getActiveImage().getExtents()[0] *
                                                                                                      compImage.getActiveImage().getExtents()[1]) +
                                                                                                 (pt.y *
                                                                                                      compImage.getActiveImage().getExtents()[0]) +
                                                                                                 pt.x));
                                                }
                                            }

                                            if (VOIs.VOIAt(i).getContourGraph() != null) {
                                                VOIs.VOIAt(i).getContourGraph().update(getPointPositions(), getPointIntensities(), j);
                                                VOIs.VOIAt(i).getContourGraph().setUnitsInLabel(FileInfoBase.getUnitsOfMeasureAbbrevStr(compImage.getActiveImage().getFileInfo(0).getUnitsOfMeasure(0)));
                                            }
                                        }
                                    } else if (compImage.getActiveImage().getNDims() == 4) {

                                        for (int t = 0; t < compImage.getActiveImage().getExtents()[3]; t++) {
                                            pt = ((VOIPoint) (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(j)))
                                                     .exportPoint();

                                            if ((getPointPositions() != null) && (getPointIntensities() != null)) {
                                                getPointPositions()[t] = t;
                                                getPointIntensities()[t] = compImage.getActiveImage().getFloat((int) ((t * xDim * yDim * zDim) +
                                                                                             (pt.z * xDim * yDim) +
                                                                                             (pt.y * xDim) + pt.x));
                                            }
                                        }

                                        if (VOIs.VOIAt(i).getContourGraph() != null) {
                                            VOIs.VOIAt(i).getContourGraph().update(getPointPositions(), getPointIntensities(), j);
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

                                        for (s = 0, foundCurve = false; s < compImage.getActiveImage().getExtents()[2]; s++) {

                                            try {

                                                for (int c = 0; c < 3; c++) {
                                                    numPixels = 0;

                                                    for (j = 0, intensitySum = 0;
                                                             j < VOIs.VOIAt(i).getCurves()[s].size(); j++) {

                                                        if (((VOIContour) VOIs.VOIAt(i).getCurves()[s].elementAt(j))
                                                                .isActive() || foundCurve) {

                                                            if (!foundCurve) {
                                                                compImage.getActiveImage().exportData(s * length * 4, length * 4,
                                                                                       getImageGraphBuffer());
                                                            } // locks and releases lock

                                                            intensitySum += ((VOIContour) (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                                .calcRGBIntensity(getImageGraphBuffer(),
                                                                                                      compImage.getActiveImage().getExtents()[0],
                                                                                                      c);
                                                            numPixels += ((VOIContour) (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
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

                                        for (s = 0, foundCurve = false; s < compImage.getActiveImage().getExtents()[2]; s++) {

                                            try {
                                                numPixels = 0;

                                                for (j = 0, intensitySum = 0; j < VOIs.VOIAt(i).getCurves()[s].size();
                                                         j++) {

                                                    if (((VOIContour) VOIs.VOIAt(i).getCurves()[s].elementAt(j))
                                                            .isActive() || foundCurve) {

                                                        if (!foundCurve) {
                                                            compImage.getActiveImage().exportData(s * length, length, getImageGraphBuffer());
                                                        } // locks and releases lock

                                                        intensitySum += ((VOIContour) (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                            .calcIntensity(getImageGraphBuffer(),
                                                                                               compImage.getActiveImage().getExtents()[0]);
                                                        numPixels += ((VOIContour) (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
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

                                            for (s = 0, intensitySum = 0; s < compImage.getActiveImage().getExtents()[2]; s++) {
                                                compImage.getActiveImage().exportData((t * xDim * yDim * zDim) + (s * xDim * yDim),
                                                                       length, getImageGraphBuffer()); // locks and releases lock

                                                for (j = 0; j < VOIs.VOIAt(i).getCurves()[s].size(); j++) {
                                                    intensitySum += ((VOIContour) (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
                                                                        .calcIntensity(getImageGraphBuffer(),
                                                                                           compImage.getActiveImage().getExtents()[0]);
                                                    numPixels += ((VOIContour) (VOIs.VOIAt(i).getCurves()[s].elementAt(j)))
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
        Graphics g = getGraphics();

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

                    graphPointVOI(newPtVOI, ((VOIPoint) (VOIs.VOIAt(getVOI_ID()).getCurves()[compImage.getSlice()].elementAt(0))), 0);

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

                    if (!((VOIs.VOIAt(i).getContourGraph() != null) && (compImage.getActiveImage().isColorImage() == true))) {
                        graphPointVOI(VOIs.VOIAt(i),
                                      ((VOIPoint) (VOIs.VOIAt(i).getCurves()[compImage.getSlice()].elementAt(index - 1))), index - 1);
                    }

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

        // System.err.println("returning false");
        return false;
    }
    
}
