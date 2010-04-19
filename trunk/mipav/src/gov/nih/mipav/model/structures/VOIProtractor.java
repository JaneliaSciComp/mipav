package gov.nih.mipav.model.structures;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.file.*;

import gov.nih.mipav.view.*;

import java.awt.*;


/**
 * This class is fundamental to the VOI class in which points are stored that describe a Protractor VOI. The points are
 * 3D and are floats (see Vector3f). It extends VOIBase and therefore it extends Vector. Point 0 is the common vertex of
 * the 2 intersecting lines, point 1 is the outer point of the initially shorter line, and point 2 is the outer point of
 * the intially longer line. This kind of VOI is used to measure angles.
 *
 * @see  VOIBase
 * @see  VOILine
 * @see  VOI
 * @see  VOIPoint
 */
public class VOIProtractor extends VOIBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4213485128976585397L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Color of the protractor. */
    private Color color;

    /** Stores coordinates of protractor to speed the process of access. */
    private float[] coordinates = new float[4];

    /** Stores coordinates of protractor to speed the process of access. */
    private float[] coords = new float[4];

    /** Length of first protractor segment. */
    private float seg1Length;

    /** DOCUMENT ME! */
    private boolean showLength = false;

    /** First segment of PROTRACTOR snaps into horizontal or vertical on mouseRelease. */
    private boolean snap = false;

    /** Angle of first protractor segment with respect to x axis. */
    private double theta = 0.0;

    /** Angle of second protractor segment with respect to first. */
    private double theta2 = 0.0;

    /** X dimension of the image where the protractor is displayed. */
    private int xDim = 0;

    /** Used in contains method. Memory allocated once here as a class variable. */
    private float[] xPts = null;

    /** Y dimension of the image where the protractor is displayed. */
    private int yDim = 0;

    /** Used in contains method. Memory allocated once here as a class variable. */
    private float[] yPts = null;

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Determines if the supplied point can be found within the points that define the contour.
     *
     * @param   _x           x-coordinate of the point in question
     * @param   _y           y-coordinate of the point in question
     * @param   forceReload  if true export points from VOI structure
     *
     * @return  true if point is within the contour
     */
    public boolean contains(int _x, int _y, boolean forceReload) {
        int i;
        int nPts = size();
        int j = nPts - 1;
        boolean isInside = false;
        float x = _x + 0.49f; // Matt add doc !!!
        float y = _y + 0.49f;

        // reloads points in this array for speed purposes
        if ((forceReload == true) || (xPts == null)) {

            try {
                xPts = new float[size()];
                yPts = new float[size()];
            } catch (OutOfMemoryError error) {
                System.gc();

                return false;
            }

            for (i = 0; i < nPts; i++) {
                xPts[i] = ((Vector3f) (elementAt(i))).X;
                yPts[i] = ((Vector3f) (elementAt(i))).Y;
            }
        }

        for (i = 0; i < nPts; i++) {

            if (((yPts[j] <= y) && (y < yPts[i]) && (area2(xPts[i], yPts[i], xPts[j], yPts[j], x, y) >= 0)) ||
                    ((yPts[i] <= y) && (y < yPts[j]) && (area2(xPts[j], yPts[j], xPts[i], yPts[i], x, y) >= 0))) {
                isInside = !isInside;
            }

            j = i;
        }

        // if the point is not inside the protractor, then check and
        // see if it's near the line (because, initially, the protractor is
        // drawn with angle = 0.
        if (isInside == true) {
            return isInside;
        }

        return this.nearLine(_x, _y, 20);
    }

    /**
     * does nothing.
     *
     * @param  direction  int
     */
    public void cycleActivePt(int direction) { }

    /**
     * Draws the protractor into the given graphics context.
     *
     * @param  zoomX           zoom for the x coordinate
     * @param  zoomY           zoom for the y coordinate
     * @param  resolutionX     X dimension thickness ratio (aspect ratio)
     * @param  resolutionY     Y dimension thickness ratio (aspect ratio)
     * @param  originX         DOCUMENT ME!
     * @param  originY         DOCUMENT ME!
     * @param  resols          array of pixel resolutions
     * @param  unitsOfMeasure  e.g. mm for millimeters etc.
     * @param  orientation     the orientation of the image slice where the VOI is to be drawn
     * @param  g               graphics to paint in
     * @param  solid           boolean that indicates solidity - NOT used VOILine
     */
    public void drawSelf(float zoomX, float zoomY, float resolutionX, float resolutionY, float originX, float originY,
                         float[] resols, int[] unitsOfMeasure, int orientation, Graphics g, boolean solid, int thickness) {
        float[] x = new float[3];
        float[] y = new float[3];
        float[] z = new float[3];

        if (g == null) {
            MipavUtil.displayError("VOIProtractor.drawSelf: grapics = null");

            return;
        }

        g.setPaintMode();
        exportArrays(x, y, z);
        x[0] = Math.round(x[0] * zoomX * resolutionX);
        y[0] = Math.round(y[0] * zoomY * resolutionY);
        x[1] = Math.round(x[1] * zoomX * resolutionX);
        y[1] = Math.round(y[1] * zoomY * resolutionY);
        x[2] = Math.round(x[2] * zoomX * resolutionX);
        y[2] = Math.round(y[2] * zoomY * resolutionY);
        g.setColor(color);

        if (Math.abs(y[1] - y[0]) > Math.abs(x[1] - x[0])) {
            g.drawLine((int) (x[0] - 1), (int) y[0], (int) (x[1] - 1), (int) y[1]);
            g.drawLine((int) x[0], (int) y[0], (int) x[1], (int) y[1]);
            g.drawLine((int) (x[0] + 1), (int) y[0], (int) (x[1] + 1), (int) y[1]);
        } else {
            g.drawLine((int) x[0], (int) (y[0] - 1), (int) x[1], (int) (y[1] - 1));
            g.drawLine((int) x[0], (int) y[0], (int) x[1], (int) y[1]);
            g.drawLine((int) x[0], (int) (y[0] + 1), (int) x[1], (int) (y[1] + 1));
        }

        g.setColor(color);
        g.drawLine((int) x[0], (int) y[0], (int) x[2], (int) y[2]);

        // if this contour is active, then also draw the vertices and
        // the angle info
        if (active == true) {

            // draw little boxes at the vertices
            // if active draw little boxes at points
            /*
             * for (int j =0; j < 3; j++){ // only 3 points g.setColor(Color.white); g.fillRect((int)(x[j]-1.5),
             * (int)(y[j]-1.5), 3, 3);
             *
             * if(nearPoint == j ) { // Highlight Active point g.setColor(Color.black); g.fillRect((int)(x[j]-1.5),
             * (int)(y[j]-1.5), 4, 4); g.setColor(Color.white); g.drawRect((int)(x[j]-1.5), (int)(y[j]-1.5), 3, 3); }
             * else { g.setColor(Color.black); g.drawRect((int)(x[j]-1.5), (int)(y[j]-1.5), 3, 3); } }
             */
            this.computeSnaplessAngle(resols);
            this.drawTickMarks(g, unitsOfMeasure, xDim, yDim, resols, zoomX, zoomY, resolutionX, resolutionY,
                               showLength);
        } // end is active
    }

    /**
     * Abstract method: curve draws itself.
     *
     * @param  zoomX           zoom for the x coordinate
     * @param  zoomY           zoom for the y coordinate
     * @param  resolutionX     X dimension thickness ratio (aspect ratio)
     * @param  resolutionY     Y dimension thickness ratio (aspect ratio)
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
                         FileInfoBase fileInfo, int dim, int thickness) { }

    /**
     * Draws tick marks at 1/4, 1/2, and 3/4 of the line. Not presently used.
     *
     * @param  g               graphics to draw in
     * @param  unitsOfMeasure  unit of measure to be displayed on line.
     * @param  xD              horizontal dimension
     * @param  yD              vertical dimension
     * @param  res             resolutions of the image for finding length
     * @param  zoomX           zoom in the x direction
     * @param  zoomY           zoom in the y direction
     * @param  resolutionX     X dimension thickness ratio (aspect ratio)
     * @param  resolutionY     Y dimension thickness ratio (aspect ratio)
     * @param  showLengths     show the lengths of the protractor segments
     */
    public void drawTickMarks(Graphics g, int[] unitsOfMeasure, int xD, int yD, float[] res, float zoomX, float zoomY,
                              float resolutionX, float resolutionY, boolean showLengths) {
        double length;
        int i;
        double slope;
        String degreeString;
        String lengthString;
        boolean close;
        float[] x = new float[3];
        float[] y = new float[3];
        float[] z = new float[3];
        float[] x2 = new float[2];
        float[] y2 = new float[2];

        if (g == null) {
            MipavUtil.displayError("VOIprotractor drawTickMarks: graphics = null");

            return;
        }

        g.setPaintMode();
        g.setFont(MipavUtil.font12);
        exportArrays(x, y, z);
        length = MipavMath.length(x, y, res);
        x[0] = Math.round(x[0] * zoomX * resolutionX);
        y[0] = Math.round(y[0] * zoomY * resolutionY);
        x[1] = Math.round(x[1] * zoomX * resolutionX);
        y[1] = Math.round(y[1] * zoomY * resolutionY);

        if ((x[1] - x[0]) != 0) {
            slope = (y[1] - y[0]) / (x[1] - x[0]);
        } else {
            slope = Double.MAX_VALUE;
        }

        close = (((y[0] <= (yD / 2)) && (slope < 1) && (slope > -1)) || (x[0] >= (xD / 2)));
        getCoords(x, y, .5); // get coordinates for tick marks
        degreeString = String.valueOf(theta2); // since y decreases going down
        i = degreeString.indexOf('.');

        if (degreeString.length() >= (i + 3)) {
            degreeString = degreeString.substring(0, i + 3);
        }

        degreeString += " deg";

        if (close == true) {

            if ((yD - y[0]) < 20) {
                g.setColor(Color.black);
                g.drawString(degreeString, (int) coords[0] - 20, (int) coords[1] - 21);
                g.drawString(degreeString, (int) coords[0] - 20, (int) coords[1] - 19);
                g.drawString(degreeString, (int) coords[0] - 21, (int) coords[1] - 20);
                g.drawString(degreeString, (int) coords[0] - 19, (int) coords[1] - 20);
                g.setColor(Color.white);
                g.drawString(degreeString, (int) coords[0] - 20, (int) coords[1] - 20);
            } else if ((xD - x[0]) < 20) {
                g.setColor(Color.black);
                g.drawString(degreeString, (int) coords[0] - 50, (int) coords[1] + 21);
                g.drawString(degreeString, (int) coords[0] - 50, (int) coords[1] + 19);
                g.drawString(degreeString, (int) coords[0] - 51, (int) coords[1] + 20);
                g.drawString(degreeString, (int) coords[0] - 49, (int) coords[1] + 20);
                g.setColor(Color.white);
                g.drawString(degreeString, (int) coords[0] - 50, (int) coords[1] + 20);
            } else {
                g.setColor(Color.black);
                g.drawString(degreeString, (int) coords[0] - 20, (int) coords[1] + 21);
                g.drawString(degreeString, (int) coords[0] - 20, (int) coords[1] + 19);
                g.drawString(degreeString, (int) coords[0] - 19, (int) coords[1] + 20);
                g.drawString(degreeString, (int) coords[0] - 21, (int) coords[1] + 20);
                g.setColor(Color.white);
                g.drawString(degreeString, (int) coords[0] - 20, (int) coords[1] + 20);
            }
        } else {

            if ((slope > 0) || (slope < -.5)) {
                g.setColor(Color.black);
                g.drawString(degreeString, (int) coords[0] + 20, (int) coords[1] + 21);
                g.drawString(degreeString, (int) coords[0] + 20, (int) coords[1] + 19);
                g.drawString(degreeString, (int) coords[0] + 21, (int) coords[1] + 20);
                g.drawString(degreeString, (int) coords[0] + 19, (int) coords[1] + 20);
                g.setColor(Color.white);
                g.drawString(degreeString, (int) coords[0] + 20, (int) coords[1] + 20);
            } else {
                g.setColor(Color.black);
                g.drawString(degreeString, (int) coords[0] - 40, (int) coords[1] - 21);
                g.drawString(degreeString, (int) coords[0] - 40, (int) coords[1] - 19);
                g.drawString(degreeString, (int) coords[0] - 41, (int) coords[1] - 20);
                g.drawString(degreeString, (int) coords[0] - 39, (int) coords[1] - 20);
                g.setColor(Color.white);
                g.drawString(degreeString, (int) coords[0] - 40, (int) coords[1] - 20);
            }
        }

        g.setColor(color);

        for (i = 0; i < 2; i++) {
            getEndLines(x, y, i);
            g.drawLine((int) coordinates[0], (int) coordinates[1], (int) coordinates[2], (int) coordinates[3]);
        }

        exportArrays(x, y, z);
        length = MipavMath.length(x[0], y[0], x[2], y[2], res);
        x[0] = Math.round(x[0] * zoomX * resolutionX);
        y[0] = Math.round(y[0] * zoomY * resolutionY);
        x[2] = Math.round(x[2] * zoomX * resolutionX);
        y[2] = Math.round(y[2] * zoomY * resolutionY);

        if ((x[2] - x[0]) != 0) {
            slope = (y[2] - y[0]) / (x[2] - x[0]);
        } else {
            slope = Double.MAX_VALUE;
        }

        close = (((y[2] <= (yD / 2)) && (slope < 1) && (slope > -1)) || (x[2] >= (xD / 2)));
        x2[0] = x[0];
        x2[1] = x[2];
        y2[0] = y[0];
        y2[1] = y[2];
        getCoords(x2, y2, .5); // get coordinates for tick marks

        if (showLengths) {
            lengthString = String.valueOf(length);
            i = lengthString.indexOf('.');

            if (lengthString.length() >= (i + 3)) {
                lengthString = lengthString.substring(0, i + 3);
            }

            switch (unitsOfMeasure[0]) {

                case FileInfoBase.INCHES:
                    lengthString = lengthString + " in";
                    break;
                    
                case FileInfoBase.MILS:
                    lengthString = lengthString + " mil";
                    break;

                case FileInfoBase.ANGSTROMS:
                    lengthString = lengthString + " A";
                    break;

                case FileInfoBase.NANOMETERS:
                    lengthString = lengthString + " nm";
                    break;

                case FileInfoBase.MICROMETERS:
                    lengthString = lengthString + " um";
                    break;

                case FileInfoBase.MILLIMETERS:
                    lengthString = lengthString + " mm";
                    break;

                case FileInfoBase.CENTIMETERS:
                    lengthString = lengthString + " cm";
                    break;

                case FileInfoBase.METERS:
                    lengthString = lengthString + " m";
                    break;

                case FileInfoBase.KILOMETERS:
                    lengthString = lengthString + " km";
                    break;

                case FileInfoBase.MILES:
                    lengthString = lengthString + " miles";
                    break;

                default:
                    lengthString = "Unknown";
            }

            if (close == true) {

                if ((yD - y2[1]) < 20) {
                    g.setColor(Color.black);
                    g.drawString(lengthString, (int) coords[0] - 20, (int) coords[1] - 21);
                    g.drawString(lengthString, (int) coords[0] - 20, (int) coords[1] - 19);
                    g.drawString(lengthString, (int) coords[0] - 21, (int) coords[1] - 20);
                    g.drawString(lengthString, (int) coords[0] - 19, (int) coords[1] - 20);
                    g.setColor(Color.white);
                    g.drawString(lengthString, (int) coords[0] - 20, (int) coords[1] - 20);
                } else if ((xD - x2[1]) < 20) {
                    g.setColor(Color.black);
                    g.drawString(lengthString, (int) coords[0] - 50, (int) coords[1] + 21);
                    g.drawString(lengthString, (int) coords[0] - 50, (int) coords[1] + 19);
                    g.drawString(lengthString, (int) coords[0] - 51, (int) coords[1] + 20);
                    g.drawString(lengthString, (int) coords[0] - 49, (int) coords[1] + 20);
                    g.setColor(Color.white);
                    g.drawString(lengthString, (int) coords[0] - 50, (int) coords[1] + 20);
                } else {
                    g.setColor(Color.black);
                    g.drawString(lengthString, (int) coords[0] - 20, (int) coords[1] + 21);
                    g.drawString(lengthString, (int) coords[0] - 20, (int) coords[1] + 19);
                    g.drawString(lengthString, (int) coords[0] - 19, (int) coords[1] + 20);
                    g.drawString(lengthString, (int) coords[0] - 21, (int) coords[1] + 20);
                    g.setColor(Color.white);
                    g.drawString(lengthString, (int) coords[0] - 20, (int) coords[1] + 20);
                }
            } else {

                if ((slope > 0) || (slope < -.5)) {
                    g.setColor(Color.black);
                    g.drawString(lengthString, (int) coords[0] + 20, (int) coords[1] + 21);
                    g.drawString(lengthString, (int) coords[0] + 20, (int) coords[1] + 19);
                    g.drawString(lengthString, (int) coords[0] + 21, (int) coords[1] + 20);
                    g.drawString(lengthString, (int) coords[0] + 19, (int) coords[1] + 20);
                    g.setColor(Color.white);
                    g.drawString(lengthString, (int) coords[0] + 20, (int) coords[1] + 20);
                } else {
                    g.setColor(Color.black);
                    g.drawString(lengthString, (int) coords[0] - 40, (int) coords[1] - 21);
                    g.drawString(lengthString, (int) coords[0] - 40, (int) coords[1] - 19);
                    g.drawString(lengthString, (int) coords[0] - 41, (int) coords[1] - 20);
                    g.drawString(lengthString, (int) coords[0] - 39, (int) coords[1] - 20);
                    g.setColor(Color.white);
                    g.drawString(lengthString, (int) coords[0] - 40, (int) coords[1] - 20);
                }
            }
        } // end of if (showLengths)

        g.setColor(color);

        // getCoords(x2, y2, 0.5);
        // g.drawLine((int)coords[0], (int)coords[1], (int)coords[2], (int)coords[3]);
        for (i = 0; i < 2; i++) {
            getEndLines(x2, y2, i);
            g.drawLine((int) coordinates[0], (int) coordinates[1], (int) coordinates[2], (int) coordinates[3]);
        }
    }

    /**
     * Does nothing.
     *
     * @return  null
     */
    public Vector3f getActivePt() {
        return null;
    }

    /**
     * Calculates the bounds of the protractor.
     *
     * @param  x  two element array where x[0] = min extent of the protractor and x[1] = max extent of the Contour in
     *            the x dimension
     * @param  y  two element array where y[0] = min extent of the protractor and y[1] = max extent of the Contour in
     *            the y dimension
     * @param  z  two element array where z[0] = min extent of the protractor and z[1] = max extent of the Contour in
     *            the z dimension
     */
    public void getBounds(int[] x, int[] y, int[] z) {
        int i;
        int xx, yy, zz;
        x[0] = 100000;
        x[1] = -1;
        y[0] = 100000;
        y[1] = -1;
        z[0] = 100000;
        z[1] = -1;

        for (i = 0; i < this.size(); i++) {
            xx = Math.round(((Vector3f) (elementAt(i))).X);
            yy = Math.round(((Vector3f) (elementAt(i))).Y);
            zz = Math.round(((Vector3f) (elementAt(i))).Z);

            if (xx < x[0]) {
                x[0] = xx;
            }

            if (xx > x[1]) {
                x[1] = xx;
            }

            if (yy < y[0]) {
                y[0] = yy;
            }

            if (yy > y[1]) {
                y[1] = yy;
            }

            if (zz < z[0]) {
                z[0] = zz;
            }

            if (zz > z[1]) {
                z[1] = zz;
            }
        }
    }

    /**
     * Calculates the bounds of the protractor.
     *
     * @param  x  two element array where x[0] = min extent of the protractor and x[1] = max extent of the Contour in
     *            the x dimension
     * @param  y  two element array where y[0] = min extent of the protractor and y[1] = max extent of the Contour in
     *            the y dimension
     * @param  z  two element array where z[0] = min extent of the protractor and z[1] = max extent of the Contour in
     *            the z dimension
     */
    public void getBounds(float[] x, float[] y, float[] z) {
        int i;
        float xx, yy, zz;
        x[0] = 100000;
        x[1] = -1;
        y[0] = 100000;
        y[1] = -1;
        z[0] = 100000;
        z[1] = -1;

        for (i = 0; i < this.size(); i++) {
            xx = ((Vector3f) (elementAt(i))).X;
            yy = ((Vector3f) (elementAt(i))).Y;
            zz = ((Vector3f) (elementAt(i))).Z;

            if (xx < x[0]) {
                x[0] = xx;
            }

            if (xx > x[1]) {
                x[1] = xx;
            }

            if (yy < y[0]) {
                y[0] = yy;
            }

            if (yy > y[1]) {
                y[1] = yy;
            }

            if (zz < z[0]) {
                z[0] = zz;
            }

            if (zz > z[1]) {
                z[1] = zz;
            }
        }
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
     * @return  origin always returns null
     */
    public float[] getOrigin(FileInfoBase fileInfo, int dim, float originX, float originY, float[] resols) {
        return null;
    }

    /**
     * Gets the angle between the two lines of the protractor.
     *
     * @return  theta
     */
    public double getTheta2() {
        return theta2;
    }

    /**
     * Import points into protractor.
     *
     * @param  x  array of x points
     * @param  y  array of y points
     * @param  z  array of z points
     * @param  n  number of points in the array
     */
    public void importArrays(int[] x, int[] y, int[] z, int n) {
        int i;

        if (n > 3) {
            return;
        }

        this.removeAllElements();

        for (i = 0; i < n; i++) {
            this.addElement(new Vector3f(x[i], y[i], z[i]));
        }
    }

    /**
     * Import points into protractor.
     *
     * @param  x  array of x points
     * @param  y  array of y points
     * @param  z  array of z points
     * @param  n  number of points in the array
     */
    public void importArrays(float[] x, float[] y, float[] z, int n) {
        int i;

        if (n > 3) {
            return;
        }

        this.removeAllElements();

        for (i = 0; i < n; i++) {
            this.addElement(new Vector3f(x[i], y[i], z[i]));
        }
    }

    /**
     * Import points into protractor.
     *
     * @param  pt  array of points
     */
    public void importPoints(Vector3f[] pt) {
        int i;

        if (pt.length > 3) {
            return;
        }

        this.removeAllElements();

        for (i = 0; i < pt.length; i++) {
            this.addElement(pt[i]);
        }
    }

    /**
     * Does nothing.
     *
     * @param  direction  int
     * @param  xD         int
     * @param  yD         int
     */
    public void moveActivePt(int direction, int xD, int yD) { }

    /**
     * Moves end point of line VOI.
     *
     * @param  x  x coordinate
     * @param  y  y coordinate
     */
    public void movePt(int x, int y) {
        replaceElement(x, y, ((Vector3f) (elementAt(nearPoint))).Z);
    }

    /**
     * Moves the protractor to a new position.
     *
     * @param  xM  amount in pixels to move the line in the x direction
     * @param  yM  amount in pixels to move the line in the y direction
     * @param  xD  x dimension maximum
     * @param  yD  y dimension maximum
     */
    public void moveVOIProtractor(int xM, int yM, int xD, int yD) {
        float[] x = new float[2];
        float[] y = new float[2];
        float[] z = new float[2];

        // get line VOI bounds
        getBounds(x, y, z);

        // ensure bounds of image and not exceed by VOI
        if (((x[0] + xM) >= xD) || ((x[0] + xM) < 0)) {
            return;
        }

        if (((x[1] + xM) >= xD) || ((x[1] + xM) < 0)) {
            return;
        }

        if (((y[0] + yM) >= yD) || ((y[0] + yM) < 0)) {
            return;
        }

        if (((y[1] + yM) >= yD) || ((y[1] + yM) < 0)) {
            return;
        }

        translate(xM, yM);
    }

    /**
     * Tests if a point is near outer point of a protractor ?? Should this be in the VOI protractor class ??
     *
     * @param   x            x coordinate of point
     * @param   y            y coordinate of point
     * @param   zoom         magnification of image
     * @param   resolutionX  X dimension thickness ratio (aspect ratio)
     * @param   resolutionY  Y dimension thickness ratio (aspect ratio)
     *
     * @return  returns boolean result of test
     */
    public boolean nearOuterPoint(int x, int y, float zoom, float resolutionX, float resolutionY) {
        int xC, yC;
        float dist, minDistance = 100000;
        nearPoint = NOT_A_POINT;
        xC = Math.round(((Vector3f) (elementAt(1))).X * zoom * resolutionX);
        yC = Math.round(((Vector3f) (elementAt(1))).Y * zoom * resolutionY);
        dist = (float) MipavMath.distance(x, xC, y, yC);

        if ((dist < 3) && (dist < minDistance)) {
            minDistance = dist;
            nearPoint = 1;
            // return true;
        }

        xC = Math.round(((Vector3f) (elementAt(2))).X * zoom * resolutionX);
        yC = Math.round(((Vector3f) (elementAt(2))).Y * zoom * resolutionY);
        dist = (float) MipavMath.distance(x, xC, y, yC);

        if ((dist < 3) && (dist < minDistance)) {
            minDistance = dist;
            nearPoint = 2;
            // return true;
        }

        if (nearPoint == NOT_A_POINT) {
            return false;
        } else {
            return true;
        }
    }

    /**
     * Used to set the color of the protractor.
     *
     * @param  color  the color of the protractor
     */
    public void setColor(Color color) {
        this.color = color;
    }

    /**
     * Used to set the flag to show length of one protractor arm.
     *
     * @param  flag  set true to show length of the protractor arm.
     */
    public void setShowLength(boolean flag) {
        showLength = flag;
    }

    /**
     * If snap is true than snap the protractor to horizontal or vertical. Used in the triplanar view
     *
     * @param  snap  if true then snap to vertical or horizonal, whichever is closer
     */
    public void setSnap(boolean snap) {
        this.snap = snap;
    }

    /**
     * Accessor to xDim and yDim for the current image that this VOI is in.
     *
     * @param  xD  X dimension of the image
     * @param  yD  Y dimension of the image
     */
    public void setXYDim(int xD, int yD) {
        xDim = xD;
        yDim = yD;
    }

    /**
     * Special function to draw tick marks on the object.
     *
     * @param  g            graphics to draw in
     * @param  res          resolutions of the image
     * @param  units        units of measure (mm or inches)
     * @param  xD           horizontal dimension
     * @param  yD           vertical dimension
     * @param  zoomX        zoom in the x direction
     * @param  zoomY        zoom in the y direction
     * @param  resolutionX  X dimension thickness ratio (aspect ratio)
     * @param  resolutionY  Y dimension thickness ratio (aspect ratio)
     * @param  showLengths  show the lengths of the protractor segments
     */
    public void showProtractorWithAngle(Graphics g, float[] res, int[] units, int xD, int yD, float zoomX, float zoomY,
                                        float resolutionX, float resolutionY, boolean showLengths, int thickness) {
        this.setXYDim(xD, yD);
        this.computeAngle(res);
        this.drawTickMarks(g, units, xD, yD, res, zoomX, zoomY, resolutionX, resolutionY, false);
        this.drawSelf(zoomX, zoomY, resolutionX, resolutionY, 0f, 0f, res, units, 0, g, false, thickness);

        return;
    }

    /**
     * Translates 3 points of the protractor VOI.
     *
     * @param  xT  amount to translate in the x direction
     * @param  yT  amount to translate in the y direction
     */
    public void translate(float xT, float yT) {
        int i;

        for (i = 0; i < 3; i++) {
            ((Vector3f) (elementAt(i))).X += xT;
            ((Vector3f) (elementAt(i))).Y += yT;
        }
    }

    /**
     * Calculates twice the area (cross product of two vectors) of a triangle given three points. This is a private
     * function only called by the function "contains".
     *
     * @param   ptAx  x-coordinate of the first point of the triangle
     * @param   ptAy  y-coordinate of the first point of the triangle
     * @param   ptBx  x-coordinate of the second point of the triangle
     * @param   ptBy  y-coordinate of the second point of the triangle
     * @param   ptCx  x-coordinate of the third point of the triangle
     * @param   ptCy  y-coordinate of the third point of the triangle
     *
     * @return  twice the area of the triangle if CCw or -2*area if CW
     */
    private float area2(float ptAx, float ptAy, float ptBx, float ptBy, float ptCx, float ptCy) {
        return (((ptAx - ptCx) * (ptBy - ptCy)) - ((ptAy - ptCy) * (ptBx - ptCx)));
    }

    /**
     * Computes the angle of the protractor.
     *
     * @param  res  resolutions of the image
     */
    private void computeAngle(float[] res) {
        int i;
        float[] x = new float[3];
        float[] y = new float[3];
        float[] z = new float[3];
        exportArrays(x, y, z);
        theta = (180.0 / Math.PI) * Math.atan2((double) ((y[1] - y[0]) * res[1]), (double) ((x[1] - x[0]) * res[0]));

        if (snap) {
            seg1Length = (float) Math.sqrt((double) (((x[1] - x[0]) * (x[1] - x[0]) * res[0] * res[0]) +
                                                     ((y[1] - y[0]) * (y[1] - y[0]) * res[1] * res[1])));
            this.removeAllElements();

            if ((theta >= -45.0) && (theta <= 45.0)) {
                x[1] = Math.min(x[0] + (seg1Length / res[0]), xDim - 1.0f);
                y[1] = y[0];
                theta = 0.0;
            } else if ((theta > 45.0) && (theta <= 135.0)) {
                x[1] = x[0];
                y[1] = Math.min(y[0] + (seg1Length / res[1]), yDim - 1.0f);
                theta = 90.0;
            } else if ((theta < -45.0) && (theta >= -135.0)) {
                x[1] = x[0];
                y[1] = Math.max(y[0] - (seg1Length / res[1]), 0.0f);
                theta = -90.0;
            } else {
                x[1] = Math.max(x[0] - (seg1Length / res[0]), 0.0f);
                y[1] = y[0];
                theta = 180.0;
            }

            for (i = 0; i < 3; i++) {
                this.addElement(new Vector3f(x[i], y[i], z[i]));
            }
        } // if (snap)

        theta2 = (180.0 / Math.PI) * Math.atan2((double) ((y[2] - y[0]) * res[1]), (double) ((x[2] - x[0]) * res[0]));
        theta2 = theta2 - theta;

        if (theta2 < -180.0) {
            theta2 = theta2 + 360.0;
        }

        if (theta2 > 180.0) {
            theta2 = theta2 - 360.0;
        }
    }

    /**
     * Computes the angle of the protractor with no snap.
     *
     * @param  res  resolutions of the image
     */
    private void computeSnaplessAngle(float[] res) {
        float[] x = new float[3];
        float[] y = new float[3];
        float[] z = new float[3];
        exportArrays(x, y, z);
        theta = (180.0 / Math.PI) * Math.atan2((double) ((y[1] - y[0]) * res[1]), (double) ((x[1] - x[0]) * res[0]));
        theta2 = (180.0 / Math.PI) * Math.atan2((double) ((y[2] - y[0]) * res[1]), (double) ((x[2] - x[0]) * res[0]));
        theta2 = theta2 - theta;

        if (theta2 < -180.0) {
            theta2 = theta2 + 360.0;
        }

        if (theta2 > 180.0) {
            theta2 = theta2 - 360.0;
        }
    }

    /**
     * Finds the coordinates for the tick marks.
     *
     * @param  x         array of x coordinates for original line
     * @param  y         array of y coordinates for original line
     * @param  fraction  what fraction of the line the tick mark should be at
     */
    private void getCoords(float[] x, float[] y, double fraction) {
        float x1, y1;
        double vector1, vector2, tmp;
        double length;
        x1 = (x[0] + x[1]) / 2;
        y1 = (y[0] + y[1]) / 2;

        if (fraction == .25) {
            x1 = (x[0] + x1) / 2;
            y1 = (y[0] + y1) / 2;
        } else if (fraction == .75) {
            x1 = (x1 + x[1]) / 2;
            y1 = (y1 + y[1]) / 2;
        }

        vector1 = (x[1] - x[0]);
        vector2 = (y[1] - y[0]);
        length = Math.sqrt((vector1 * vector1) + (vector2 * vector2));
        vector1 = (x[1] - x[0]) / length;
        vector2 = (y[1] - y[0]) / length;
        tmp = vector1;
        vector1 = 5 * (-vector2);
        vector2 = 5 * tmp;
        coords[0] = (int) (x1 + vector1 + 0.5);
        coords[1] = (int) (y1 + vector2 + 0.5);
        coords[2] = (int) (x1 - vector1 + 0.5);
        coords[3] = (int) (y1 - vector2 + 0.5);
    }

    /**
     * Finds the coordinates for the tick marks.
     *
     * @param  x     array of x coordinates for original line
     * @param  y     array of y coordinates for original line
     * @param  line  indicates which line should be the points should be retrieved from
     */
    private void getEndLines(float[] x, float[] y, int line) {
        double vector1, vector2, tmp;
        double length;
        vector1 = (x[1] - x[0]);
        vector2 = (y[1] - y[0]);
        length = Math.sqrt((vector1 * vector1) + (vector2 * vector2));
        vector1 = (x[1] - x[0]) / length;
        vector2 = (y[1] - y[0]) / length;
        tmp = vector1;
        vector1 = -10 * ((vector1 * 0.707) + (vector2 * 0.707));
        vector2 = 10 * ((tmp * 0.707) - (vector2 * 0.707));

        if (line == 0) {
            coordinates[0] = (int) (x[1]);
            coordinates[1] = (int) (y[1]);
            coordinates[2] = (int) (x[1] + vector1 + 0.5);
            coordinates[3] = (int) (y[1] + vector2 + 0.5);
        } else if (line == 1) {
            coordinates[0] = (int) (x[1]);
            coordinates[1] = (int) (y[1]);
            coordinates[2] = (int) (x[1] - vector2 + 0.5);
            coordinates[3] = (int) (y[1] + vector1 + 0.5);
        }
    }
}
