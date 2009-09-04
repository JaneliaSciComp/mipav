package gov.nih.mipav.view;


import gov.nih.mipav.model.structures.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;


/**
 * Does not really "rubberband" levelset but I wanted to keep the same naming convention. Builds a polygon of a levelset
 * given a level and a starting position.
 *
 * @version  1.0, 6/23/99
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      ViewJComponentEditImage
 */

public class RubberbandLevelSet implements MouseMotionListener, MouseListener {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected float presetHue = -1.0f;

    /** DOCUMENT ME! */
    private ViewJComponentEditImage component;

    /** DOCUMENT ME! */
    private boolean doRegistration = false;

    /** DOCUMENT ME! */
    private float[] imageBufferActive = null;

    /** DOCUMENT ME! */
    private float level = 0;

    /** DOCUMENT ME! */
    private Polygon levelSet = new Polygon();

    /** DOCUMENT ME! */
    private VOIContour levelSetCon = new VOIContour(true);

    /** DOCUMENT ME! */
    private PointStack levelSetStack = new PointStack(500);

    /** DOCUMENT ME! */
    private BitSet map = null;

    /** DOCUMENT ME! */
    private Stack stack = new Stack();

    /** DOCUMENT ME! */
    private int xDim, yDim;

    /** DOCUMENT ME! */
    private int xS, yS;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs an interactive levelset contour to added to VOI.
     *
     * @param  _component  component to add to
     */
    public RubberbandLevelSet(ViewJComponentEditImage _component) {

        component = _component;

        int xDim = component.getImageA().getExtents()[0];
        int yDim = component.getImageA().getExtents()[1];
        map = new BitSet(xDim * yDim);
        component.addMouseMotionListener(this);
        component.addMouseListener(this);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Takes a VOI contour from an adjacent slice and the intensity level at that slice and tries to find the equivalent
     * level set at this slice.
     *
     * @param   origLevel  Level of levelset VOI at adjacent slice.
     * @param   polygon    Polygon that makes up VOI at adjacent slice.
     *
     * @return  DOCUMENT ME!
     */
    public Polygon findNextLevelSet(float origLevel, Polygon polygon) {
        float tmpLevel;
        int tmpX, tmpY;
        float diff = .01f * origLevel; // allowed difference in levels is 1% of original intensity level
        Vector potentialGons = new Vector(); // vector of potential polygons
        Vector potentialLevels = new Vector(); // vector of levels at those polygons

        imageBufferActive = component.getActiveImageSliceBuffer(); // need to reset so it is the next slice.

        // first check the points on the original polygon
        for (int i = 0; i < polygon.npoints; i++) {
            tmpX = polygon.xpoints[i];
            tmpY = polygon.ypoints[i];
            tmpLevel = imageBufferActive[(tmpY * component.getActiveImage().getExtents()[0]) + tmpX];

            // if level is within + or - diff of origLevel, find the polygon at that point
            if ((tmpLevel >= (origLevel - diff)) && (tmpLevel <= (origLevel + diff))) {
                level = tmpLevel; // need to do this for paths()
                singleLevelSet(tmpX, tmpY, tmpLevel);

                if (levelSet != null) {

                    // get levelSet into appropriate format
                    VOI newVOI = new VOI((short) 0, "temp", 1, VOI.CONTOUR, presetHue);
                    newVOI.importPolygon(levelSet, 0);

                    Vector[] tempContours = newVOI.getCurves();
                    ((VOIContour) (tempContours[0].elementAt(0))).trimPoints(Preferences.getTrim(),
                                                                             Preferences.getTrimAdjacient());

                    Polygon[] gons = newVOI.exportPolygons(0);

                    // add this to potential list
                    potentialGons.add(gons[0]);
                    potentialLevels.add(new Float(tmpLevel));
                }
            }
        }

        Rectangle bounds = polygon.getBounds();

        // Now look at the entire rectangle enclosing the VOI
        for (int i = bounds.x - 5; i < (bounds.x + bounds.width + 5); i++) {

            for (int j = bounds.y - 5; j < (bounds.y + bounds.height + 5); j++) {
                tmpX = i;
                tmpY = j;
                tmpLevel = imageBufferActive[(tmpY * component.getActiveImage().getExtents()[0]) + tmpX];

                // if level is within + or - diff of origLevel, find the polygon at that point
                if ((tmpLevel >= (origLevel - diff)) && (tmpLevel <= (origLevel + diff))) {
                    level = tmpLevel;
                    singleLevelSet(tmpX, tmpY, tmpLevel);

                    if (levelSet != null) {

                        // get levelSet into appropriate format
                        VOI newVOI = new VOI((short) 0, "temp", 1, VOI.CONTOUR, presetHue);
                        newVOI.importPolygon(levelSet, 0);

                        Vector[] tempContours = newVOI.getCurves();
                        ((VOIContour) (tempContours[0].elementAt(0))).trimPoints(Preferences.getTrim(),
                                                                                 Preferences.getTrimAdjacient());

                        Polygon[] gons = newVOI.exportPolygons(0);

                        // add this to potential list
                        potentialGons.add(gons[0]);
                        potentialLevels.add(new Float(tmpLevel));
                    }
                }
            }
        }

        tmpLevel = origLevel; // reset tmpLevel in case there are no potential solutions

        float min = Float.MAX_VALUE;
        float temp;
        float testLevel;
        Polygon solution = null, potential = null;

        // go through potential polygon list
        for (int i = 0; i < potentialGons.size(); i++) {
            potential = (Polygon) potentialGons.elementAt(i);
            testLevel = ((Float) potentialLevels.elementAt(i)).floatValue();

            Rectangle test = potential.getBounds();

            // quick and dirty distance measure - if the bounding box of the potential polygon is close in
            // width and height to the bounding box of the original, it will be chosen.
            temp = Math.abs(bounds.width - test.width) + Math.abs(bounds.height - test.height) +
                   (2 * (testLevel - level));

            if (temp < min) {
                solution = potential;
                min = temp;
                tmpLevel = testLevel;
            }
        }

        // reset level to appropriate value
        level = tmpLevel;

        return solution;
    }

    /**
     * Accessor that returns the level of this levelset VOI.
     *
     * @return  The level of the levelset VOI.
     */
    public float getLevel() {
        return level;
    }

    /**
     * Accessor that returns the level set as a polygon object.
     *
     * @return  level set in the form of polygon
     */
    public Polygon getLevelSetPolygon() {
        return levelSet;
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
    public void mouseDragged(MouseEvent mouseEvent) { }

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
        int i, j;
        int r, g, b;

        if (component.getCursorMode() == ViewJComponentEditImage.LEVELSET) {
            xS = Math.round(mouseEvent.getX() / (component.getZoomX() * component.getResolutionX())); // zoomed x.
            yS = Math.round(mouseEvent.getY() / (component.getZoomY() * component.getResolutionY())); // zoomed y.

            imageBufferActive = component.getActiveImageBuffer();

            if (component.getActiveImage().isColorImage()) {

                for (i = 1, j = 0; i < imageBufferActive.length; i += 4, j++) {
                    r = (int) imageBufferActive[i];
                    g = (int) imageBufferActive[i + 1];
                    b = (int) imageBufferActive[i + 2];

                    imageBufferActive[j] = Math.round((r + g + b) / 3.0f);
                }
            }

            level = imageBufferActive[(yS * component.getActiveImage().getExtents()[0]) + xS];
            xDim = component.getActiveImage().getExtents()[0];
            singleLevelSet(xS, yS, level);
            component.getActiveImage().notifyImageDisplayListeners(null, true);
        }
    }

    /**
     * Unchanged.
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mousePressed(MouseEvent mouseEvent) { }

    /**
     * Makes an VOI out of the level set upon the mouse release.
     *
     * @param  mouseEvent  event that triggered function
     */
    public void mouseReleased(MouseEvent mouseEvent) {

        ModelImage image;
        VOI newVOI;
        int index;
        int colorID;
        ViewVOIVector VOIs;
        int i;
        int nVOI;
        String name;

        image = component.getActiveImage();

        if (component.getCursorMode() == ViewJComponentEditImage.LEVELSET) {

            if (levelSet == null) {
                return;
            }

            if (((ViewJComponentEditImage) (component)).getVOIHandler().isNewVoiNeeded(VOI.CONTOUR)) {

                try {
                    VOIs = image.getVOIs();
                    index = VOIs.size();
                    colorID = 0;

                    if (image.getVOIs().size() > 0) {
                        colorID = ((VOI) (image.getVOIs().lastElement())).getID() + 1;
                    }

                    nVOI = VOIs.size();
                    name = "levelset" + (index + 1);

                    int test;

                    do {
                        test = 0;

                        for (i = 0; i < nVOI; i++) {

                            if (name.equals(VOIs.VOIAt(i).getName())) {
                                index++;
                                name = "levelset" + (index + 1);
                                test = 1;
                            }
                        }
                    } while (test == 1);

                    /*
                     * do{ test =0; for(i=0; i < nVOI; i++) {     if (colorID ==((int)VOIs.VOIAt(i).getID())) {
                     * colorID++;         test=1;     } } } while(test==1);
                     */
                    if (image.getNDims() > 2) {
                        newVOI = new VOI((short) colorID, name, image.getExtents()[2], VOI.CONTOUR, presetHue);
                    } else {
                        newVOI = new VOI((short) colorID, name, 1, VOI.CONTOUR, presetHue);
                    }

                    newVOI.importPolygon(levelSet, component.getSlice());

                    Vector[] tempContours = newVOI.getCurves();
                    ((VOIContour) (tempContours[component.getSlice()].elementAt(0))).trimPoints(Preferences.getTrim(),
                                                                                                Preferences.getTrimAdjacient());
                    newVOI.setLevel(level);
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: RubberbandLevelSet.mouseReleased");
                    component.setCursorMode(ViewJComponentEditImage.DEFAULT);

                    return;
                }

                image.registerVOI(newVOI);
                image.notifyImageDisplayListeners();
                ((ViewJComponentEditImage) (component)).getVOIHandler().setVOI_IDs(newVOI.getID(), newVOI.getUID());


                System.gc();
                // component.setMode(ViewJComponentEditImage.DEFAULT);
            } else {

                // get selected contour
                VOIs = image.getVOIs();
                nVOI = VOIs.size();

                for (i = 0; i < nVOI; i++) {

                    if (VOIs.VOIAt(i).getID() == ((ViewJComponentEditImage) (component)).getVOIHandler().getVOI_ID()) {

                        if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                            VOIs.VOIAt(i).importPolygon(levelSet, component.getSlice());
                            ((VOIContour) (VOIs.VOIAt(i).getCurves()[component.getSlice()].lastElement())).trimPoints(Preferences.getTrim(),
                                                                                                                      Preferences.getTrimAdjacient());
                        } else {
                            MipavUtil.displayError("Can't add Level-set VOI to other VOI structure.");
                        }
                    }
                }

                image.notifyImageDisplayListeners();

            }

            if (mouseEvent.isShiftDown() != true) {
                ((ViewJComponentEditImage) (component)).setCursorMode(ViewJComponentEditImage.DEFAULT);
            }

        }
    }

    /**
     * Sets the presetHue.
     *
     * @param  presetHue  DOCUMENT ME!
     */
    public void setPresetHue(float presetHue) {
        this.presetHue = presetHue;
    }

    /**
     * This method calculates the average pixel value based on the four neighbors (N, S, E, W).
     *
     * @param   index  the center pixel where the average pixel value is to be calculated.
     *
     * @return  the average pixel value as a float.
     */
    private float avgPix(int index) {
        int xDim = component.getActiveImage().getExtents()[0];

        if ((index > xDim) && (index < (imageBufferActive.length - xDim))) {

            float sum = imageBufferActive[index];

            sum += imageBufferActive[index - xDim];
            sum += imageBufferActive[index - 1];
            sum += imageBufferActive[index + 1];
            sum += imageBufferActive[index + xDim];

            return sum / 5.0f;
        } else {
            return (imageBufferActive[index]);
        }
    }

    /**
     * Generates the possible paths of the level set and pushes them onto a stack. Looks in the 8 neighborhood
     * directions for the possible paths.
     *
     * @param  index  image location
     * @param  i      DOCUMENT ME!
     */
    private void paths(int index, int i) {

        int[] intPtr = null;

        try {
            intPtr = new int[1];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentEditImage.mouseDragged");

            return;
        }

        intPtr[0] = levelSetStack.size() - 1;

        if ((i != 0) && (imageBufferActive[index - xDim - 1] <= level) && (map.get(index - xDim - 1) == false)) {
            stack.push(intPtr);
        } else if ((i != 1) && (imageBufferActive[index - xDim] <= level) && (map.get(index - xDim) == false)) {
            stack.push(intPtr);
        } else if ((i != 2) && (imageBufferActive[index - xDim + 1] <= level) && (map.get(index - xDim + 1) == false)) {
            stack.push(intPtr);
        } else if ((i != 3) && (imageBufferActive[index + 1] <= level) && (map.get(index + 1) == false)) {
            stack.push(intPtr);
        } else if ((i != 4) && (imageBufferActive[index + xDim + 1] <= level) && (map.get(index + xDim + 1) == false)) {
            stack.push(intPtr);
        } else if ((i != 5) && (imageBufferActive[index + xDim] <= level) && (map.get(index + xDim) == false)) {
            stack.push(intPtr);
        } else if ((i != 6) && (imageBufferActive[index + xDim - 1] <= level) && (map.get(index + xDim - 1) == false)) {
            stack.push(intPtr);
        } else if ((i != 7) && (imageBufferActive[index - 1] <= level) && (map.get(index - 1) == false)) {
            stack.push(intPtr);
        }
    }

    /**
     * Creates a single level set. Takes a starting point and finds a closed path along the levelset back to the
     * starting point.
     *
     * @param  startPtX  the start point
     * @param  startPtY  the start point
     * @param  level     the level of the level set
     */
    private void singleLevelSet(float startPtX, float startPtY, float level) {

        int x, y;
        int index;
        double distance;
        stack.removeAllElements();

        if (imageBufferActive == null) {
            return;
        }

        xDim = component.getActiveImage().getExtents()[0];
        yDim = component.getActiveImage().getExtents()[1];

        for (int i = 0; i < map.size(); i++) {
            map.clear(i);
        }

        if (startPtX >= (xDim - 1)) {
            return;
        }

        if (startPtY >= (yDim - 1)) {
            return;
        }

        x = (int) (startPtX + 0.5);
        y = (int) (startPtY + 0.5);

        levelSetStack.reset();
        levelSetStack.addPoint(x, y);
        map.set((y * xDim) + x);

        int dir = -1;
        float diff = 100000;

        do {
            index = (y * xDim) + x;

            if ((x >= 2) && (x < (xDim - 2)) && (y >= 2) && (y < (yDim - 2))) {

                if ((avgPix(index - xDim) >= level) &&
                        ((avgPix(index - xDim + 1) < level) || (avgPix(index) < level) ||
                             (avgPix(index - xDim - 1) < level) || (avgPix(index - (2 * xDim)) < level)) &&
                        (map.get(index - xDim) == false)) {
                    dir = 1;
                    diff = Math.abs(avgPix(index - xDim) - avgPix(index));
                }

                if ((avgPix(index - xDim + 1) >= level) &&
                        ((avgPix(index - xDim + 2) < level) || (avgPix(index + 1) < level) ||
                             (avgPix(index - xDim) < level) || (avgPix(index - (2 * xDim) + 1) < level)) &&
                        (map.get(index - xDim + 1) == false)) {

                    if (Math.abs(avgPix(index - xDim + 1) - avgPix(index)) < diff) {
                        dir = 2;
                        diff = Math.abs(avgPix(index - xDim + 1) - avgPix(index));
                    }
                }

                if ((avgPix(index + 1) >= level) &&
                        ((avgPix(index + 2) < level) || (avgPix(index + xDim + 1) < level) || (avgPix(index) < level) ||
                             (avgPix(index - xDim + 1) < level)) && (map.get(index + 1) == false)) {

                    if (Math.abs(avgPix(index + 1) - avgPix(index)) < diff) {
                        dir = 3;
                        diff = Math.abs(avgPix(index + 1) - avgPix(index));
                    }
                }

                if ((avgPix(index + xDim + 1) >= level) &&
                        ((avgPix(index + xDim + 2) < level) || (avgPix(index + (2 * xDim) + 1) < level) ||
                             (avgPix(index + 1) < level) || (avgPix(index + xDim) < level)) &&
                        (map.get(index + xDim + 1) == false)) {

                    if (Math.abs(avgPix(index + xDim + 1) - avgPix(index)) < diff) {
                        dir = 4;
                        diff = Math.abs(avgPix(index + xDim + 1) - avgPix(index));
                    }
                }

                if ((avgPix(index + xDim) >= level) &&
                        ((avgPix(index + xDim + 1) < level) || (avgPix(index + (2 * xDim)) < level) ||
                             (avgPix(index + xDim - 1) < level) || (avgPix(index) < level)) &&
                        (map.get(index + xDim) == false)) {

                    if (Math.abs(avgPix(index + xDim) - avgPix(index)) < diff) {
                        dir = 5;
                        diff = Math.abs(avgPix(index + xDim) - avgPix(index));
                    }
                }

                if ((avgPix(index + xDim - 1) >= level) &&
                        ((avgPix(index + xDim) < level) || (avgPix(index + (2 * xDim) - 1) < level) ||
                             (avgPix(index + xDim - 2) < level) || (avgPix(index - 1) < level)) &&
                        (map.get(index + xDim - 1) == false)) {

                    if (Math.abs(avgPix(index + xDim - 1) - avgPix(index)) < diff) {
                        dir = 6;
                        diff = Math.abs(avgPix(index + xDim - 1) - avgPix(index));
                    }
                }

                if ((avgPix(index - 1) >= level) &&
                        ((avgPix(index) < level) || (avgPix(index + xDim - 1) < level) || (avgPix(index - 2) < level) ||
                             (avgPix(index - xDim - 1) < level)) && (map.get(index - 1) == false)) {

                    if (Math.abs(avgPix(index - 1) - avgPix(index)) < diff) {
                        dir = 7;
                        diff = Math.abs(avgPix(index - 1) - avgPix(index));
                    }
                }

                if ((avgPix(index - xDim - 1) >= level) &&
                        ((avgPix(index - xDim) < level) || (avgPix(index - 1) < level) ||
                             (avgPix(index - xDim - 2) < level) || (avgPix(index - (2 * xDim) - 1) < level)) &&
                        (map.get(index - xDim - 1) == false)) {

                    if (Math.abs(avgPix(index - xDim - 1) - avgPix(index)) < diff) {
                        dir = 0;
                        // diff = Math.abs(imageBufferActive[index-xDim-1] - imageBufferActive[index]);
                    }
                }

                diff = 1000000;

                if (dir == 1) {
                    // x = x;
                    y = y - 1;
                    map.set(index - xDim);
                    paths(index, 1);
                } else if (dir == 2) {
                    x = x + 1;
                    y = y - 1;
                    map.set(index - xDim + 1);
                    paths(index, 2);
                } else if (dir == 3) {
                    x = x + 1;
                    // y = y;
                    map.set(index + 1);
                    paths(index, 3);
                } else if (dir == 4) {
                    x = x + 1;
                    y = y + 1;
                    map.set(index + xDim + 1);
                    paths(index, 4);
                } else if (dir == 5) {
                    // x = x;
                    y = y + 1;
                    map.set(index + xDim);
                    paths(index, 5);
                } else if (dir == 6) {
                    x = x - 1;
                    y = y + 1;
                    map.set(index + xDim - 1);
                    paths(index, 6);
                } else if (dir == 7) {
                    x = x - 1;
                    // y = y;
                    map.set(index - 1);
                    paths(index, 7);
                } else if (dir == 0) {
                    x = x - 1;
                    y = y - 1;
                    map.set(index - xDim - 1);
                    paths(index, 0);
                } else {

                    if (!stack.empty()) {
                        int ptr = ((int[]) stack.pop())[0];
                        x = levelSetStack.getPointX(ptr);
                        y = levelSetStack.getPointY(ptr);
                        levelSetStack.setIndex(ptr);
                    } else {
                        x = y = -1;
                    }
                }

                dir = -1;
            } else { // near edge of image
                levelSetStack.reset();

                break;
            }

            if ((x == -1) || (y == -1)) {
                levelSetStack.reset();

                break;
            }

            levelSetStack.addPoint(x, y);

            distance = ((x - startPtX) * (x - startPtX)) + ((y - startPtY) * (y - startPtY));

            if ((distance < 2.1) && (levelSetStack.size() < 10)) {
                distance = 10;
            }
        } while (distance > 2.1);


        if (levelSetStack.size() != 0) {
            levelSet = levelSetStack.exportPolygon();
        } else {
            levelSet = null;
        }
    }

}
