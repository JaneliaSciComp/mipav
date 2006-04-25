package gov.nih.mipav.model.structures;


import gov.nih.mipav.model.file.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.geom.*;


/**
 * Class for a point VOI, a simple extension of CurveBase.
 *
 * @version  1.0 Feb. 10, 1999
 * @author   Matthew J. McAuliffe
 * @see      VOI
 *
 *           <p>$Logfile: /mipav/src/gov/nih/mipav/model/structures/VOIPoint.java $ $Revision: 64 $ $Date: 2/24/06 3:34p
 *           $</p>
 */
public class VOIPoint extends VOIBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -1083170022935501406L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean isPolySlice;

    /** Label of the point (e.g. 1, or 2 or 3... ) */
    private String str = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default constructor.
     */
    public VOIPoint() { }

    /**
     * Constructor that passes in the name (for optional name display instead of label).
     *
     * @param  voiName  String the VOI parent's name
     */
    public VOIPoint(String voiName) {
        this.name = voiName;
        isPolySlice = false;
    }

    /**
     * Creates a new VOIPoint object.
     *
     * @param  voiName  DOCUMENT ME!
     * @param  doPoly   DOCUMENT ME!
     */
    public VOIPoint(String voiName, boolean doPoly) {
        this.name = name;
        this.isPolySlice = doPoly;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Required to denote what points are contained by the region it encloses or is near to.
     *
     * @param   _x           x-coordinate of the point in question
     * @param   _y           y-coordinate of the point in question
     * @param   forceReload  if true export points from VOI structure
     *
     * @return  false for all points.
     */
    public boolean contains(int _x, int _y, boolean forceReload) {
        return false;
    }

    /**
     * does nothing now.
     *
     * @param  direction  int
     */
    public void cycleActivePt(int direction) { }

    /**
     * Draws point VOI into the given graphics point for axial configuration in ViewJFrameTriImage.
     *
     * @param  g       graphics to paint in
     * @param  x       the x coordinate
     * @param  y       the y coordinate
     * @param  xLabel  the x component label
     * @param  yLabel  the y component label
     */
    public void drawAxialSelf(Graphics g, int x, int y, int xLabel, int yLabel) {
        int xPos, yPos;

        if (g == null) {
            MipavUtil.displayError("VOIPoint.drawAxialSelf: grapics = null");

            return;
        }

        // show the label as 1-based, not 0-based
        xLabel++;
        yLabel++;

        doName = (Preferences.is(Preferences.PREF_SHOW_VOI_NAME) && (name != null));


        // Create the string that will be drawn for label and name
        if (doName) {
            str = new String(name + ": (" + xLabel + "," + yLabel + ")");
        } else if (label != null) {
            str = new String(label + ": (" + xLabel + "," + yLabel + ")");
        } else {
            str = new String("(" + xLabel + "," + yLabel + ")");
        }

        g.drawLine(x, y - 5, x, y + 5);
        g.drawLine(x - 5, y, x + 5, y);

        if (active == false) {

            if (doName) {

                if (x < 20) {
                    g.drawString(name, x + 10, y);
                } else {
                    g.drawString(name, x - 15, y - 5);
                }
            } else if (label != null) {

                if (x < 20) {
                    g.drawString(label, x + 10, y);
                } else {
                    g.drawString(label, x - 15, y - 5);
                }
            }
        }
        // else active is true
        else {
            g.setColor(Color.black);
            g.fillRect((int) (x - 1.5), (int) (y - 1.5), 4, 4);
            g.setColor(Color.white);
            g.drawRect((int) (x - 1.5), (int) (y - 1.5), 4, 4);
            g.setFont(MipavUtil.font12);
            g.setColor(Color.yellow);

            xPos = x;
            yPos = y;

            if (str != null) {

                if (x < 70) {
                    xPos += 10;
                } else {
                    xPos -= 60;
                }

                if (y < 30) {
                    yPos += 20;
                } else {
                    yPos -= 10;
                }

                g.drawString(str, xPos, yPos);
            }
        }
    }

    /**
     * Draws point VOI into the given graphics point.
     *
     * @param  zoomX           zoom for the x coordinate
     * @param  zoomY           zoom for the y coordinate
     * @param  resolutionX     X resolution (aspect ratio)
     * @param  resolutionY     Y resolution (aspect ratio)
     * @param  originX         DOCUMENT ME!
     * @param  originY         DOCUMENT ME!
     * @param  resols          array of pixel resolutions
     * @param  unitsOfMeasure  e.g. mm for millimeters etc.
     * @param  orientation     the orientation of the image slice where the VOI is to be drawn
     * @param  g               graphics to paint in
     * @param  solid           boolean that indicates solidity - NOT used VOIPoint
     */
    public void drawSelf(float zoomX, float zoomY, float resolutionX, float resolutionY, float originX, float originY,
                         float[] resols, int[] unitsOfMeasure, int orientation, Graphics g, boolean solid) {

        int xS, yS;
        int x, y;
        int xPos, yPos;

        if (g == null) {
            MipavUtil.displayError("VOIPoint.drawSelf: grapics = null");

            return;
        }

        doName = (Preferences.is(Preferences.PREF_SHOW_VOI_NAME) && (name != null));

        if ((orientation == NA) || (orientation == XY)) {

            // 1 -> imageDim instead of 0 -> imageDim - 1
            x = Math.round(((Point3Df) (elementAt(0))).x) + 1;
            y = Math.round(((Point3Df) (elementAt(0))).y) + 1;
            xS = Math.round(((Point3Df) (elementAt(0))).x * zoomX * resolutionX);
            yS = Math.round(((Point3Df) (elementAt(0))).y * zoomY * resolutionY);
        } else if (orientation == ZY) {

            // 1 -> imageDim instead of 0 -> imageDim - 1
            x = Math.round(((Point3Df) (elementAt(0))).z) + 1;
            y = Math.round(((Point3Df) (elementAt(0))).y) + 1;
            xS = Math.round(((Point3Df) (elementAt(0))).z * zoomX * resolutionX);
            yS = Math.round(((Point3Df) (elementAt(0))).y * zoomY * resolutionY);
        } else if (orientation == XZ) {

            // 1 -> imageDim instead of 0 -> imageDim - 1
            x = Math.round(((Point3Df) (elementAt(0))).x) + 1;
            y = Math.round(((Point3Df) (elementAt(0))).z) + 1;
            xS = Math.round(((Point3Df) (elementAt(0))).x * zoomX * resolutionX);
            yS = Math.round(((Point3Df) (elementAt(0))).z * zoomY * resolutionY);
        } else {

            // 1 -> imageDim instead of 0 -> imageDim - 1
            x = Math.round(((Point3Df) (elementAt(0))).x) + 1;
            y = Math.round(((Point3Df) (elementAt(0))).y) + 1;
            xS = Math.round(((Point3Df) (elementAt(0))).x * zoomX * resolutionX);
            yS = Math.round(((Point3Df) (elementAt(0))).y * zoomY * resolutionY);
        }

        // Create the string that will be drawn for label and name
        if (doName) {
            str = new String(name + ": (" + x + "," + y + ")");
        } else if (label != null) {
            str = new String(label + ": (" + x + "," + y + ")");
        } else {
            str = new String("(" + x + "," + y + ")");
        }

        int type;

        if (!isPolySlice) {
            type = 0;

            String typeStr = Preferences.getProperty("VOIPointDrawType");

            if (typeStr != null) {

                try {
                    type = Integer.parseInt(typeStr);

                    if ((type < 0) || (type > 3)) {
                        type = 0;
                    }
                } catch (Exception ex) { }
            }
        } else {
            type = 4;
        }

        switch (type) {

            case 0:
                g.drawLine(xS, yS - 4, xS, yS + 4);
                g.drawLine(xS - 4, yS, xS + 4, yS);
                break;

            case 1:
                g.drawLine(xS, yS - 4, xS, yS - 1);
                g.drawLine(xS, yS + 1, xS, yS + 4);
                g.drawLine(xS - 4, yS, xS - 1, yS);
                g.drawLine(xS + 1, yS, xS + 4, yS);
                break;

            case 2:
                g.drawLine(xS - 4, yS - 4, xS + 4, yS + 4);
                g.drawLine(xS - 4, yS + 4, xS + 4, yS - 4);
                break;

            case 3:
                g.drawLine(xS - 4, yS - 4, xS - 1, yS - 1);
                g.drawLine(xS + 1, yS + 1, xS + 4, yS + 4);
                g.drawLine(xS - 4, yS + 4, xS - 1, yS + 1);
                g.drawLine(xS + 1, yS - 1, xS + 4, yS - 4);
                break;

            case 4:
                ((Graphics2D) g).draw(new Ellipse2D.Float(xS, yS, 2, 2));

                break;
        }

        if (active == false) {

            if (doName) {

                if (xS < 20) {
                    g.drawString(name, xS + 10, yS);
                } else {
                    g.drawString(name, xS - 15, yS - 5);
                }
            } else {

                if (xS < 20) {
                    g.drawString(label, xS + 10, yS);
                } else {
                    g.drawString(label, xS - 15, yS - 5);
                }
            }
        }

        if (active == true) {

            if ((type != 1) && (type != 3)) {
                g.setColor(Color.black);
                g.fillRect((int) (xS - 1.5), (int) (yS - 1.5), 4, 4);
                g.setColor(Color.white);
                g.drawRect((int) (xS - 1.5), (int) (yS - 1.5), 4, 4);
            }

            g.setFont(MipavUtil.font12);
            g.setColor(Color.yellow);

            xPos = xS;
            yPos = yS;

            if (str != null) {

                if (xS < 70) {
                    xPos += 10;
                } else {
                    xPos -= 60;
                }

                if (yS < 30) {
                    yPos += 20;
                } else {
                    yPos -= 10;
                }

                g.drawString(str, xPos, yPos);
            }
        }
    }

    /**
     * Abstract method: curve draws itself.
     *
     * @param  zoomX           zoom for the x coordinate
     * @param  zoomY           zoom for the y coordinate
     * @param  resolutionX     X resolution (aspect ratio)
     * @param  resolutionY     Y resolution (aspect ratio)
     * @param  originX         Start location of X origin
     * @param  originY         Start location of Y origin
     * @param  resols          array of pixel resolutions
     * @param  unitsOfMeasure  e.g. mm for millimeters etc.
     * @param  orientation     the orientation of the image slice where the VOI is to be drawn
     * @param  g               graphics to paint in
     * @param  boundingBox     boolean that indicates if boundingBox is on or off
     * @param  fileInfo        DOCUMENT ME!
     * @param  dim             DOCUMENT ME!
     */
    public void drawSelf(float zoomX, float zoomY, float resolutionX, float resolutionY, float originX, float originY,
                         float[] resols, int[] unitsOfMeasure, int orientation, Graphics g, boolean boundingBox,
                         FileInfoBase fileInfo, int dim) { }

    /**
     * Method to access point VOI coordinate.
     *
     * @return  3d point
     */
    public Point3Df exportPoint() {
        return (Point3Df) (elementAt(0));
    }

    /**
     * Gets the active points location (point3df).
     *
     * @return  Point3Df the location
     */
    public Point3Df getActivePt() {
        return exportPoint();
    }

    /**
     * Calculates the bounds of the point (i.e. returns the point)
     *
     * @param  x  two element array where x[0] = min extent of the Contour and x[1] = max extent of the Contour in the x
     *            dimension
     * @param  y  two element array where y[0] = min extent of the Contour and y[1] = max extent of the Contour in the y
     *            dimension
     * @param  z  two element array where z[0] = min extent of the Contour and z[1] = max extent of the Contour in the z
     *            dimension
     */
    public void getBounds(int[] x, int[] y, int[] z) {
        Point3Df pt = exportPoint();
        x[0] = x[1] = Math.round(pt.x);
        y[0] = y[1] = Math.round(pt.y);
        z[0] = z[1] = Math.round(pt.z);
    }

    /**
     * Calculates the bounds of the point (i.e. returns the point)
     *
     * @param  x  two element array where x[0] = min extent of the Contour and x[1] = max extent of the Contour in the x
     *            dimension
     * @param  y  two element array where y[0] = min extent of the Contour and y[1] = max extent of the Contour in the y
     *            dimension
     * @param  z  two element array where z[0] = min extent of the Contour and z[1] = max extent of the Contour in the z
     *            dimension
     */
    public void getBounds(float[] x, float[] y, float[] z) {
        Point3Df pt = exportPoint();
        x[0] = x[1] = pt.x;
        y[0] = y[1] = pt.y;
        z[0] = z[1] = pt.z;
    }

    /**
     * This method gets the coordinates of the point.
     *
     * @param  coord  a float array, in which the first element is the x coordinate and the second element is the y
     *                coordinate
     */
    public void getCoordinates(float[] coord) {
        Point3Df pt = exportPoint();
        coord[0] = pt.x;
        coord[1] = pt.y;
    }

    /**
     * Return the VOI crop region's origin of rectangle.
     *
     * @param   fileInfo  DOCUMENT ME!
     * @param   dim       DOCUMENT ME!
     * @param   originX   DOCUMENT ME!
     * @param   originY   DOCUMENT ME!
     * @param   resols    DOCUMENT ME!
     *
     * @return  origin rectangle origin
     */
    public float[] getOrigin(FileInfoBase fileInfo, int dim, float originX, float originY, float[] resols) {
        return null;
    }

    /**
     * Imports point into VOI.
     *
     * @param  x  x point
     * @param  y  y point
     * @param  z  z point
     * @param  i  DOCUMENT ME!
     */
    public void importArrays(int[] x, int[] y, int[] z, int i) {
        this.addElement(new Point3Df(x[0], y[0], z[0]));
    }

    /**
     * Imports point into VOI.
     *
     * @param  x  x point
     * @param  y  y point
     * @param  z  z point
     * @param  i  DOCUMENT ME!
     */
    public void importArrays(float[] x, float[] y, float[] z, int i) {
        this.addElement(new Point3Df(x[0], y[0], z[0]));
    }

    /**
     * Import point into point VOI.
     *
     * @param  pt  3D point
     */
    public void importPoint(Point3Df pt) {
        this.addElement(pt);
    }

    /**
     * Imports a new point at the given image slice. Replaces the old point.
     *
     * @param  point  point to be saved
     * @param  slice  index to save the point at
     */
    public void importPoint(Point3Df point, int slice) {

        try {
            this.removeAllElements();
            this.addElement(new Point3Df(point.x, point.y, slice));
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }
    }


    /**
     * Import points into VOI - Included to extend abstract method.
     *
     * @param  pt  point
     */
    public void importPoints(Point3Df[] pt) {
        this.addElement(pt[0]);
    }

    /**
     * Import point into VOI.
     *
     * @param  pt  point
     */
    public void importPoints(Point3Df pt) {
        this.addElement(pt);
    }


    /**
     * Sets the point at (xP,yP,zP). Bounds checking is performed
     *
     * @param  xP    x location
     * @param  yP    y location
     * @param  zP    z location
     * @param  xDim  x dimension maximum
     * @param  yDim  y dimension maximum
     * @param  zDim  z dimension maximum
     */
    public void locateVOIPoint(int xP, int yP, int zP, int xDim, int yDim, int zDim) {
        Point3Df pt = exportPoint();

        if ((xP >= xDim) || (xP < 0)) {
            return;
        }

        if ((yP >= yDim) || (yP < 0)) {
            return;
        }

        if ((zP >= zDim) || (zP < 0)) {
            return;
        }

        pt.x = xP;
        pt.y = yP;
        pt.z = zP;
    }

    /**
     * Does nothing now.
     *
     * @param  direction  int
     * @param  xDim       int
     * @param  yDim       int
     */
    public void moveActivePt(int direction, int xDim, int yDim) {

        int x = 0;
        int y = 0;

        switch (direction) {

            case UP:
                y -= 1;
                break;

            case LEFT:
                x -= 1;
                break;

            case DOWN:
                y += 1;
                break;

            case RIGHT:
                x += 1;
                break;

            default:
                return;
        }

        moveVOIPoint(x, y, 0, xDim, yDim, Integer.MAX_VALUE);
    }


    /**
     * Moves the point to the new location. Bounds checking is performed
     *
     * @param  xM    amount in pixels to move the line in the x direction
     * @param  yM    amount in pixels to move the line in the y direction
     * @param  zM    amount in pixels to move the line in the z direction
     * @param  xDim  x dimension maximum
     * @param  yDim  y dimension maximum
     * @param  zDim  z dimension maximum
     */
    public void moveVOIPoint(int xM, int yM, int zM, int xDim, int yDim, int zDim) {

        Point3Df pt = exportPoint();

        if (((pt.x + xM) >= xDim) || ((pt.x + xM) < 0)) {
            return;
        }

        if (((pt.y + yM) >= yDim) || ((pt.y + yM) < 0)) {
            return;
        }

        if (((pt.z + zM) >= zDim) || ((pt.z + zM) < 0)) {
            return;
        }

        pt.x = pt.x + xM;
        pt.y = pt.y + yM;
        pt.z = pt.z + zM;
    }

    /**
     * Translates the point of the VOI to a new location.
     *
     * @param  xT  amount to translate in the x direction
     * @param  yT  amount to translate in the y direction
     * @param  zT  amount to translate in the z direction
     */
    public void translate(float xT, float yT, float zT) {

        Point3Df pt = exportPoint();
        pt.x = pt.x + xT;
        pt.y = pt.y + yT;
        pt.z = pt.z + zT;
    }

}
