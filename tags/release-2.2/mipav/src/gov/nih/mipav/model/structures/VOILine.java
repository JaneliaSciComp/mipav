package gov.nih.mipav.model.structures;


import gov.nih.mipav.MipavMath;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import java.awt.*;


/**
 * Class for line, a simple extension of VOIBase. A line is formed of two points.
 *
 * @version 1.0 March 15, 2000
 * @author Matthew McAuliffe, Ph.D.
 * @see VOI
 */
public class VOILine extends VOIBase {
    /**
     * Scratch buffers that are allocated once at construction to speed access. stores the x coordinate of the two end
     * points.
     */
    private float x[] = new float[2];

    /**
     * Scratch buffers that are allocated once at construction to speed access. stores the y coordinate of the two end
     * points.
     */
    private float y[] = new float[2];

    /**
     * Scratch buffers that are allocated once at construction to speed access. stores the z coordinate of the two end
     * points.
     */
    private float z[] = new float[2];

    /** Scratch buffer to speed access */
    private float coordinates[] = new float[4];

    /** Scratch buffer to speed access */
    private float coords[] = new float[4];

    /** X Dimension of the image */
    private int xDim = 0;

    /** Y Dimension of the image */
    private int yDim = 0;

    /** Angle of line */
    private double theta = 0;


    public VOILine(String name) {
        super();
        this.name = name;
    }

    /**
     * Import points into line
     * @param pt array of points
     */
    public void importPoints(Point3Df pt[]) {
        int i;
        if (pt.length > 2)
            return;
        this.removeAllElements();
        for (i = 0; i < pt.length; i++ ) {
            this.addElement(pt[i]);
        }
    }

    /**
     * Import points into line
     * @param newX array of x points
     * @param newY array of y points
     * @param newZ array of z points
     * @param n number of points in the array
     */
    public void importArrays(int newX[], int newY[], int newZ[], int n) {
        int i;
        if (n > 2)
            return;
        this.removeAllElements();
        for (i = 0; i < n; i++ ) {
            this.addElement(new Point3Df(newX[i], newY[i], newZ[i]));
        }
    }

    /**
     * Import points into line
     * @param newX array of x points
     * @param newY array of y points
     * @param newZ array of z points
     * @param n number of points in the array
     */
    public void importArrays(float newX[], float newY[], float newZ[], int n) {
        int i;
        if (n > 2)
            return;
        this.removeAllElements();
        for (i = 0; i < n; i++ ) {
            this.addElement(new Point3Df(newX[i], newY[i], newZ[i]));
        }
    }

    /**
     * Accessor to xDim and yDim for the current image that this VOI is in
     * @param xDim X Dimension of the image
     * @param yDim Y Dimension of the image
     */
    public void setXYDim(int xDim, int yDim) {
        this.xDim = xDim;
        this.yDim = yDim;
    }

    /**
     * Draws the line in the given graphics context
     * @param zoomX zoom for the x coordinate
     * @param zoomY zoom for the y coordinate
     * @param originX
     * @param originY
     * @param resolutionX X dimension thickness ratio (aspect ratio)
     * @param resolutionY Y dimension thickness ratio (aspect ratio)
     * @param resols array of pixel resolutions
     * @param unitsOfMeasure e.g. mm for millimeters etc.
     * @param orientation the orientation of the image slice where the VOI is to be drawn
     * @param g graphics to paint in
     * @param solid boolean that indicates solidity - NOT used VOILine
     */
    public void drawSelf(float zoomX, float zoomY, float resolutionX, float resolutionY, float originX, float originY,
            float[] resols, int[] unitsOfMeasure, int orientation, Graphics g, boolean solid) {
        if (g == null) {
            MipavUtil.displayError("VOILine.drawSelf: graphics = null");
            return;
        }
        exportArrays(x, y, z);
        x[0] = Math.round(x[0] * zoomX * resolutionX);
        y[0] = Math.round(y[0] * zoomY * resolutionY);
        x[1] = Math.round(x[1] * zoomX * resolutionX);
        y[1] = Math.round(y[1] * zoomY * resolutionY);
        g.drawLine((int) x[0], (int) y[0], (int) x[1], (int) y[1]);
        Color currentColor = g.getColor();
        if (active == true) {
            // draw the active point dragging is taking place
            if (lastPoint != NOT_A_POINT && lastPoint >= 0 && this.size() > lastPoint) {
                g.setColor(Color.GREEN);
                g.fillRect((int) (x[lastPoint] - 1.5 + 0.5f), (int) (y[lastPoint] - 1.5 + 0.5f), 3, 3);
            }
            drawTickMarks(g, currentColor, unitsOfMeasure, xDim, yDim, resols, zoomX, zoomY, resolutionX, resolutionY);
        }
    }

    /**
     * Abstract method: curve draws itself
     * @param zoomX zoom for the x coordinate
     * @param zoomY zoom for the y coordinate
     * @param resolutionX X dimension thickness ratio (aspect ratio)
     * @param resolutionY Y dimension thickness ratio (aspect ratio)
     * @param originX Start location of X origin
     * @param originY Start location of Y origin
     * @param resols array of pixel resolutions
     * @param unitsOfMeasure e.g. mm for millimeters etc.
     * @param orientation the orientation of the image slice where the VOI is to be drawn
     * @param g graphics to paint in
     * @param boundingBox boolean that indicates if boundingBox is on or off
     * @param fileInfo
     * @param dim
     */
    public void drawSelf(float zoomX, float zoomY, float resolutionX, float resolutionY, float originX, float originY,
            float[] resols, int[] unitsOfMeasure, int orientation, Graphics g, boolean boundingBox,
            FileInfoBase fileInfo, int dim) {}

    /**
     * Return the VOI crop region's origin of rectangle.
     * @param fileInfo
     * @param dim
     * @param originX
     * @param originY
     * @param resols
     * @return origin always returns null
     */
    public float[] getOrigin(FileInfoBase fileInfo, int dim, float originX, float originY, float[] resols) {
        return null;
    }

    /**
     * Moves line to a new position with bounds checking
     * @param xM amount in pixels to move the line in the x direction
     * @param yM amount in pixels to move the line in the y direction
     * @param xD x dimension maximum
     * @param yD y dimension maximum
     */
    public void moveVOILine(int xM, int yM, int xD, int yD) {
        // get line VOI bounds
        getBounds(x, y, z);
        // ensure bounds of image and not exceed by VOI
        if (x[0] + xM >= xD || x[0] + xM < 0)
            return;
        if (x[1] + xM >= xD || x[1] + xM < 0)
            return;
        if (y[0] + yM >= yD || y[0] + yM < 0)
            return;
        if (y[1] + yM >= yD || y[1] + yM < 0)
            return;
        translate(xM, yM);
    }

    /**
     * Moves end point of line VOI
     * @param newEndPtX x coordinate
     * @param newEndPtY y coordinate
     */
    public void movePt(int newEndPtX, int newEndPtY) {
        replaceElement(newEndPtX, newEndPtY, ((Point3Df) (elementAt(nearPoint))).z);
    }

    /**
     * Moves end point of line VOI
     * @param newEndPtX x coordinate
     * @param newEndPtY y coordinate
     * @param newEndPtZ z coordinate
     * @param xD x dimension
     * @param yD y dimension
     */
    public void movePt(int newEndPtX, int newEndPtY, int newEndPtZ, int xD, int yD) {
        if (newEndPtX < 0 || newEndPtX >= xD)
            return;
        if (newEndPtY < 0 || newEndPtY >= yD)
            return;
        replaceElement(newEndPtX, newEndPtY, newEndPtZ);
    }

    /**
     * Cycles through the points (making next or previous active)
     * @param direction int the direction to cycle
     */
    public void cycleActivePt(int direction) {
        if (lastPoint != NOT_A_POINT) {
            int index = lastPoint;
            switch (direction) {
                case UP:
                case RIGHT:
                    index++ ;
                    break;
                case DOWN:
                case LEFT:
                    index-- ;
                    break;
            }
            if (index < 0) {
                index = this.size() - 1;
            } else if (index > size() - 1) {
                index = 0;
            }
            lastPoint = index;
        }
    }

    /**
     * Moves the active point up/down/left/right
     * @param direction int direction to move
     * @param xD int
     * @param yD int
     */
    public void moveActivePt(int direction, int xD, int yD) {
        if (lastPoint != NOT_A_POINT && this.size() > lastPoint && lastPoint >= 0) {
            float ptX = ((Point3Df) (elementAt(lastPoint))).x;
            float ptY = ((Point3Df) (elementAt(lastPoint))).y;
            switch (direction) {
                case UP:
                    ptY -= 1;
                    break;
                case LEFT:
                    ptX -= 1;
                    break;
                case DOWN:
                    ptY += 1;
                    break;
                case RIGHT:
                    ptX += 1;
                    break;
                default:
                    return;
            }
            if (ptX >= 0 && ptX < xD && ptY >= 0 && ptY < yD) {
                ((Point3Df) (elementAt(lastPoint))).x = ptX;
                ((Point3Df) (elementAt(lastPoint))).y = ptY;
            }
        }
    }

    /**
     * Gets the active points location (point3df)
     * @return Point3Df the location
     */
    public Point3Df getActivePt() {
        Point3Df pt = null;
        if (lastPoint >= 0 && lastPoint < this.size()) {
            pt = (Point3Df) (elementAt(lastPoint));
        }
        return pt;
    }

    /**
     * Gets the angle of the line
     * @return double the angle (theta)
     */
    public double getAngle() {
        return theta;
    }

    /**
     * Draws tick marks at 1/4, 1/2, and 3/4 of the line.
     * @param g graphics to draw in
     * @param color color of the tick marks
     * @param unitsOfMeasure unit of measure to be displayed on line.
     * @param xD x dimension
     * @param yD y dimension
     * @param res resolutions of the image for finding length
     * @param zoomX zoom in the x direction
     * @param zoomY zoom in the y direction
     * @param resolutionX X dimension thickness ratio (aspect ratio)
     * @param resolutionY Y dimension thickness ratio (aspect ratio)
     */
    public void drawTickMarks(Graphics g, Color color, int[] unitsOfMeasure, int xD, int yD, float res[], float zoomX,
            float zoomY, float resolutionX, float resolutionY) {
        double length;
        int i;
        double slope;
        String tmpString;
        boolean close;
        if (g == null) {
            MipavUtil.displayError("VOILine drawTickMarks: grapics = null");
            return;
        }
        g.setFont(MipavUtil.font12);
        exportArrays(x, y, z);
        length = MipavMath.length(x, y, res);
        x[0] = Math.round(x[0] * zoomX * resolutionX);
        y[0] = Math.round(y[0] * zoomY * resolutionY);
        x[1] = Math.round(x[1] * zoomX * resolutionX);
        y[1] = Math.round(y[1] * zoomY * resolutionY);
        if (x[1] - x[0] != 0)
            slope = (y[1] - y[0]) / (x[1] - x[0]);
        else
            slope = Double.MAX_VALUE;
        close = ( (y[0] <= yD / 2 && slope < 1 && slope > -1) || (x[0] >= xD / 2));
        getCoords(x, y, .5); // get coordinates for tick marks
        // g.setColor(Color.yellow);
        tmpString = String.valueOf(length);
        i = tmpString.indexOf('.');
        if (tmpString.length() >= i + 3) {
            tmpString = tmpString.substring(0, i + 3);
        }
        switch (unitsOfMeasure[0]) {
            case FileInfoBase.INCHES:
                tmpString = tmpString + " in";
                break;
            case FileInfoBase.ANGSTROMS:
                tmpString = tmpString + " A";
                break;
            case FileInfoBase.NANOMETERS:
                tmpString = tmpString + " nm";
                break;
            case FileInfoBase.MICROMETERS:
                tmpString = tmpString + " um";
                break;
            case FileInfoBase.MILLIMETERS:
                tmpString = tmpString + " mm";
                break;
            case FileInfoBase.CENTIMETERS:
                tmpString = tmpString + " cm";
                break;
            case FileInfoBase.METERS:
                tmpString = tmpString + " m";
                break;
            case FileInfoBase.KILOMETERS:
                tmpString = tmpString + " km";
                break;
            case FileInfoBase.MILES:
                tmpString = tmpString + " miles";
                break;
            default:
                tmpString = "Unknown";
        }
        int stringX = (int) coords[0];
        int stringY = (int) coords[1];
        boolean drawAngle = Preferences.is(Preferences.PREF_SHOW_LINE_ANGLE);
        if (x[1] > x[0] && y[0] > y[1]) {
            theta = 90.0 - (180.0 / Math.PI) * Math.atan2( (y[0] - y[1]), x[1] - x[0]);
        } else if (x[1] > x[0] && y[1] > y[0]) {
            theta = - (90.0 + (180.0 / Math.PI) * Math.atan2(y[0] - y[1], x[1] - x[0]));
        } else if (x[0] > x[1] && y[0] > y[1]) {
            theta = - (90.0 - (180.0 / Math.PI) * Math.atan2(y[0] - y[1], x[0] - x[1]));
        } else if (x[0] > x[1] && y[1] > y[0]) {
            theta = 90.0 - (180.0 / Math.PI) * Math.atan2(y[1] - y[0], x[0] - x[1]);
        } else if (x[0] == x[1]) {
            // zero angle
            theta = 0;
        } else if (y[0] == y[1]) {
            // 90deg angle
            theta = 90;
        }
        if (drawAngle) {
            String tmpString2 = String.valueOf(theta);
            i = tmpString2.indexOf('.');
            if (tmpString2.length() >= i + 3) {
                tmpString2 = tmpString2.substring(0, i + 3);
            }
            tmpString += ", " + tmpString2 + " deg";
        }
        if (close == true) {
            if (yD - y[0] < 20) {
                if (stringY - 21 < 20) {
                    stringY += 45;
                }
                if (stringX - 21 < 10) {
                    stringX += 25;
                }
                g.setColor(Color.black);
                g.drawString(tmpString, (int) stringX - 20, stringY - 21);
                g.drawString(tmpString, (int) stringX - 20, stringY - 19);
                g.drawString(tmpString, (int) stringX - 21, stringY - 20);
                g.drawString(tmpString, (int) stringX - 19, stringY - 20);
                g.setColor(Color.white);
                g.drawString(tmpString, (int) stringX - 20, stringY - 20);
            } else if (xD - x[0] < 20) {
                g.setColor(Color.black);
                g.drawString(tmpString, (int) stringX - 50, stringY + 21);
                g.drawString(tmpString, (int) stringX - 50, stringY + 19);
                g.drawString(tmpString, (int) stringX - 51, stringY + 20);
                g.drawString(tmpString, (int) stringX - 49, stringY + 20);
                g.setColor(Color.white);
                g.drawString(tmpString, (int) stringX - 50, stringY + 20);
            } else {
                g.setColor(Color.black);
                g.drawString(tmpString, (int) stringX - 20, stringY + 21);
                g.drawString(tmpString, (int) stringX - 20, stringY + 19);
                g.drawString(tmpString, (int) stringX - 19, stringY + 20);
                g.drawString(tmpString, (int) stringX - 21, stringY + 20);
                g.setColor(Color.white);
                g.drawString(tmpString, (int) stringX - 20, stringY + 20);
            }
        } else {
            if (slope > 0 || slope < -.5) {
                g.setColor(Color.black);
                g.drawString(tmpString, (int) stringX + 20, stringY + 21);
                g.drawString(tmpString, (int) stringX + 20, stringY + 19);
                g.drawString(tmpString, (int) stringX + 21, stringY + 20);
                g.drawString(tmpString, (int) stringX + 19, stringY + 20);
                g.setColor(Color.white);
                g.drawString(tmpString, (int) stringX + 20, stringY + 20);
            } else {
                g.setColor(Color.black);
                g.drawString(tmpString, (int) stringX - 40, stringY - 21);
                g.drawString(tmpString, (int) stringX - 40, stringY - 19);
                g.drawString(tmpString, (int) stringX - 41, stringY - 20);
                g.drawString(tmpString, (int) stringX - 39, stringY - 20);
                g.setColor(Color.white);
                g.drawString(tmpString, (int) stringX - 40, stringY - 20);
            }
        }
        g.setColor(color);
        g.drawLine((int) coords[0], (int) coords[1], (int) coords[2], (int) coords[3]);
        getCoords(x, y, .25);
        g.drawLine((int) coords[0], (int) coords[1], (int) coords[2], (int) coords[3]);
        getCoords(x, y, .75);
        g.drawLine((int) coords[0], (int) coords[1], (int) coords[2], (int) coords[3]);
        g.setColor(color);
        for (i = 0; i < 4; i++ ) {
            getEndLines(x, y, i);
            g.drawLine((int) coordinates[0], (int) coordinates[1], (int) coordinates[2], (int) coordinates[3]);
        }
    }

    /**
     * Finds the coordinates for the tick marks at the start/end of the line (fills <code>coordinates</code> array to
     * be used for drawing of tick marks at the end of the line).
     * @param linePtsX array of x coordinates for original line
     * @param linePtsY array of y coordinates for original line
     * @param line indicates which of the lines which comprise the bounding tick marks should be retreived
     */
    private void getEndLines(float linePtsX[], float linePtsY[], int line) {
        double vector1, vector2, tmp;
        double length;
        vector1 = (linePtsX[1] - linePtsX[0]);
        vector2 = (linePtsY[1] - linePtsY[0]);
        length = Math.sqrt( (vector1 * vector1) + (vector2 * vector2));
        vector1 = (linePtsX[1] - linePtsX[0]) / length;
        vector2 = (linePtsY[1] - linePtsY[0]) / length;
        tmp = vector1;
        vector1 = 10 * (vector1 * 0.707 + vector2 * 0.707);
        vector2 = 10 * ( -tmp * 0.707 + vector2 * 0.707);
        if (line == 0) {
            coordinates[0] = (int) (linePtsX[1]);
            coordinates[1] = (int) (linePtsY[1]);
            coordinates[2] = (int) (linePtsX[1] + vector1 + 0.5);
            coordinates[3] = (int) (linePtsY[1] + vector2 + 0.5);
        } else if (line == 1) {
            coordinates[0] = (int) (linePtsX[1]);
            coordinates[1] = (int) (linePtsY[1]);
            coordinates[2] = (int) (linePtsX[1] - vector2 + 0.5);
            coordinates[3] = (int) (linePtsY[1] + vector1 + 0.5);
        } else if (line == 2) {
            coordinates[0] = (int) (linePtsX[0]);
            coordinates[1] = (int) (linePtsY[0]);
            coordinates[2] = (int) (linePtsX[0] - vector1 + 0.5);
            coordinates[3] = (int) (linePtsY[0] - vector2 + 0.5);
        } else if (line == 3) {
            coordinates[0] = (int) (linePtsX[0]);
            coordinates[1] = (int) (linePtsY[0]);
            coordinates[2] = (int) (linePtsX[0] + vector2 + 0.5);
            coordinates[3] = (int) (linePtsY[0] - vector1 + 0.5);
        }
    }

    /**
     * Determines if the supplied point can be found within the points that define the line. Since a line really can't
     * "hold" a point, this just calls nearLine().
     *
     * @param _x x-coordinate of the point in question
     * @param _y y-coordinate of the point in question
     * @param forceReload not used in VOILine
     * @return true if point is within the contour
     */
    public boolean contains(int _x, int _y, boolean forceReload) {
        if (this.nearLine(_x, _y, 20) == true)
            return true;
        return false;
    }

    /**
     * Finds the coordinates for the tick marks shown along the line (not at the ends) (fills <code>coordinates</code>
     * array to be used for drawing of interior tick marks).
     * @param linePtsX array of x coordinates for original line
     * @param linePtsY array of y coordinates for original line
     * @param fraction what fraction of the line the tick mark should be at
     */
    private void getCoords(float linePtsX[], float linePtsY[], double fraction) {
        float x1, y1;
        double vector1, vector2, tmp;
        double length;
        x1 = (linePtsX[0] + linePtsX[1]) / 2;
        y1 = (linePtsY[0] + linePtsY[1]) / 2;
        if (fraction == .25) {
            x1 = (linePtsX[0] + x1) / 2;
            y1 = (linePtsY[0] + y1) / 2;
        } else if (fraction == .75) {
            x1 = (x1 + linePtsX[1]) / 2;
            y1 = (y1 + linePtsY[1]) / 2;
        }
        vector1 = (linePtsX[1] - linePtsX[0]);
        vector2 = (linePtsY[1] - linePtsY[0]);
        length = Math.sqrt( (vector1 * vector1) + (vector2 * vector2));
        vector1 = (linePtsX[1] - linePtsX[0]) / length;
        vector2 = (linePtsY[1] - linePtsY[0]) / length;
        tmp = vector1;
        vector1 = 5 * ( -vector2);
        vector2 = 5 * tmp;
        coords[0] = (int) (x1 + vector1 + 0.5);
        coords[1] = (int) (y1 + vector2 + 0.5);
        coords[2] = (int) (x1 - vector1 + 0.5);
        coords[3] = (int) (y1 - vector2 + 0.5);
    }

    /**
     * Translate both points of the VOI
     * @param xT amount to translate in the x direction
     * @param yT amount to translate in the y direction
     */
    public void translate(float xT, float yT) {
        int i;
        for (i = 0; i < 2; i++ ) {
            ((Point3Df) (elementAt(i))).x += xT;
            ((Point3Df) (elementAt(i))).y += yT;
        }
    }

    /**
     * Calculates the bounds of the contour
     * @param sortedX two element array where x[0] = min extent of the Contour and x[1] = max extent of the Contour in
     *            the x dimension
     * @param sortedY two element array where y[0] = min extent of the Contour and y[1] = max extent of the Contour in
     *            the y dimension
     * @param sortedZ two element array where z[0] = min extent of the Contour and z[1] = max extent of the Contour in
     *            the z dimension
     */
    public void getBounds(int sortedX[], int sortedY[], int sortedZ[]) {
        int i;
        int xx, yy, zz;
        sortedX[0] = 100000;
        sortedX[1] = -1;
        sortedY[0] = 100000;
        sortedY[1] = -1;
        sortedZ[0] = 100000;
        sortedZ[1] = -1;
        for (i = 0; i < this.size(); i++ ) {
            xx = Math.round( ((Point3Df) (elementAt(i))).x);
            yy = Math.round( ((Point3Df) (elementAt(i))).y);
            zz = Math.round( ((Point3Df) (elementAt(i))).z);
            if (xx < sortedX[0])
                sortedX[0] = xx;
            if (xx > sortedX[1])
                sortedX[1] = xx;
            if (yy < sortedY[0])
                sortedY[0] = yy;
            if (yy > sortedY[1])
                sortedY[1] = yy;
            if (zz < sortedZ[0])
                sortedZ[0] = zz;
            if (zz > sortedZ[1])
                sortedZ[1] = zz;
        }
    }

    /**
     * Calculates the bounds of the contour
     * @param sortedX two element array where x[0] = min extent of the Contour and x[1] = max extent of the Contour in
     *            the x dimension
     * @param sortedY two element array where y[0] = min extent of the Contour and y[1] = max extent of the Contour in
     *            the y dimension
     * @param sortedZ two element array where z[0] = min extent of the Contour and z[1] = max extent of the Contour in the z
     *            dimension
     */
    public void getBounds(float sortedX[], float sortedY[], float sortedZ[]) {
        int i;
        float xx, yy, zz;
        sortedX[0] = 100000;
        sortedX[1] = -1;
        sortedY[0] = 100000;
        sortedY[1] = -1;
        sortedZ[0] = 100000;
        sortedZ[1] = -1;
        for (i = 0; i < this.size(); i++ ) {
            xx = ((Point3Df) (elementAt(i))).x;
            yy = ((Point3Df) (elementAt(i))).y;
            zz = ((Point3Df) (elementAt(i))).z;
            if (xx < sortedX[0])
                sortedX[0] = xx;
            if (xx > sortedX[1])
                sortedX[1] = xx;
            if (yy < sortedY[0])
                sortedY[0] = yy;
            if (yy > sortedY[1])
                sortedY[1] = yy;
            if (zz < sortedZ[0])
                sortedZ[0] = zz;
            if (zz > sortedZ[1])
                sortedZ[1] = zz;
        }
    }

    /**
     * Finds the position/intensity along a line VOI
     * @param position array that is filled with the distance along the line in millimeters for example
     * @param intensity the corresponding intensities along the line
     * @param imageBuffer image intensities
     * @param resolutions image resolutions
     * @param xD x-Dimension of image
     * @param yD y-Dimension of image
     * @return the number of points in the position and intensity array that hava valid data.
     */
    public int findPositionAndIntensity(float position[], float intensity[], float imageBuffer[], float resolutions[],
            int xD, int yD) {
        double distance = 0;
        double subDistance = 0;
        int index, indexX = 0, indexY = 0;
        double myY, myX, yInc, xInc;
        double x0, x1, y0, y1;
        double xDist, yDist;
        x0 = ((Point3Df) (elementAt(0))).x;
        y0 = ((Point3Df) (elementAt(0))).y;
        x1 = ((Point3Df) (elementAt(1))).x;
        y1 = ((Point3Df) (elementAt(1))).y;
        distance = Math.sqrt( ( (x1 - x0) * (x1 - x0) * (resolutions[0]) * (resolutions[0]))
                + ( (y1 - y0) * (y1 - y0) * (resolutions[1]) * (resolutions[1])));
        myX = x0;
        myY = y0;
        xDist = Math.abs(x1 - x0);
        yDist = Math.abs(y1 - y0);
        // Ths insures maximum xInc or yInc of +-0.5
        xInc = ( (x1 - x0) / (2.0 * Math.max(xDist, yDist)));
        yInc = ( (y1 - y0) / (2.0 * Math.max(xDist, yDist)));
        int pt = 0;
        for (;;) {
            indexY = (int) Math.min(Math.round(myY), yD - 1);
            indexX = (int) Math.min(Math.round(myX), xD - 1);
            index = indexY * xD + indexX;
            subDistance = Math.sqrt( ( (myX - x0) * (myX - x0) * (resolutions[0]) * (resolutions[0]))
                    + ( (myY - y0) * (myY - y0) * (resolutions[1]) * (resolutions[1])));
            if (subDistance >= distance) {
                break;
            }
            if (pt >= position.length || pt >= intensity.length)
                return pt;
            position[pt] = (float) subDistance;
            intensity[pt] = imageBuffer[index];
            pt++ ;
            myX = myX + xInc;
            myY = myY + yInc;
        }
        indexY = (int) Math.min(Math.round(y1), yD - 1);
        indexX = (int) Math.min(Math.round(x1), xD - 1);
        index = indexY * xD + indexX;
        if (pt < position.length && pt < intensity.length) {
            position[pt] = (float) distance;
            intensity[pt] = imageBuffer[index];
            return pt + 1;
        } else {
            return pt;
        }
    }

    /**
     * Finds the position/intensity along a line VOI
     * @param position array that is filled with the distance along the line in millimeters for example
     * @param intensity the corresponding intensities along the line
     * @param RGorB channel to calculate data
     * @param imageBuffer image intensities
     * @param resolutions image resolutions
     * @param xD x-Dimension of image
     * @param yD y-Dimension of image
     * @return the number of points in the position and intensity array that hava valid data.
     */
    public int findPositionAndIntensityRGB(float position[], float intensity[], int RGorB, float imageBuffer[],
            float resolutions[], int xD, int yD) {
        double distance = 0;
        double subDistance = 0;
        int index, indexX = 0, indexY = 0;
        double myY, myX, yInc, xInc;
        double x0, x1, y0, y1;
        float val;
        double xDist, yDist;
        x0 = ((Point3Df) (elementAt(0))).x;
        y0 = ((Point3Df) (elementAt(0))).y;
        x1 = ((Point3Df) (elementAt(1))).x;
        y1 = ((Point3Df) (elementAt(1))).y;
        distance = Math.sqrt( ( (x1 - x0) * (x1 - x0) * (resolutions[0]) * (resolutions[0]))
                + ( (y1 - y0) * (y1 - y0) * (resolutions[1]) * (resolutions[1])));
        myX = x0;
        myY = y0;
        xDist = Math.abs(x1 - x0);
        yDist = Math.abs(y1 - y0);
        // Ths insures maximum xInc or yInc of +-0.5
        xInc = ( (x1 - x0) / (2.0 * Math.max(xDist, yDist)));
        yInc = ( (y1 - y0) / (2.0 * Math.max(xDist, yDist)));
        int pt = 0;
        for (;;) {
            indexY = (int) Math.min(Math.round(myY), yD - 1);
            indexX = (int) Math.min(Math.round(myX), xD - 1);
            index = indexY * xD + indexX;
            subDistance = Math.sqrt( ( (myX - x0) * (myX - x0) * (resolutions[0]) * (resolutions[0]))
                    + ( (myY - y0) * (myY - y0) * (resolutions[1]) * (resolutions[1])));
            if (subDistance >= distance)
                break;
            if (pt >= position.length || pt >= intensity.length)
                return pt;
            position[pt] = (float) subDistance;
            if (RGorB == 0)
                val = imageBuffer[4 * index + 1]; // skip alpha and get Red info
            else if (RGorB == 1)
                val = imageBuffer[4 * index + 2];
            else
                val = imageBuffer[4 * index + 3];
            intensity[pt] = val;
            pt++ ;
            myX = myX + xInc;
            myY = myY + yInc;
        }
        indexY = (int) Math.min(Math.round(y1), yD - 1);
        indexX = (int) Math.min(Math.round(x1), xD - 1);
        index = indexY * xD + indexX;
        if (pt < position.length && pt < intensity.length) {
            position[pt] = (float) distance;
            if (RGorB == 0)
                val = imageBuffer[4 * index + 1]; // skip alpha and get Red info
            else if (RGorB == 1)
                val = imageBuffer[4 * index + 2];
            else
                val = imageBuffer[4 * index + 3];
            intensity[pt] = val;
            return pt + 1;
        } else {
            return pt;
        }
    }

    /**
     * Gets the position/intensity along a line VOI
     * @param position array that is filled with all x coordinates
     * @param intensity the corresponding intensities along the line
     * @param imageBuffer
     * @param xD x-Dimension of image
     * @return the number of points in the position and intensity array that hava valid data.
     */
    public int getPositionAndIntensity(Vector3Df position[], float intensity[], float imageBuffer[], int xD) {
        double distance;
        int i;
        int index, indexX = 0, indexY = 0;
        double myY, myX, yInc, xInc;
        double x0, x1, y0, y1;
        int len;
        x0 = ((Point3Df) (elementAt(0))).x;
        y0 = ((Point3Df) (elementAt(0))).y;
        x1 = ((Point3Df) (elementAt(1))).x;
        y1 = ((Point3Df) (elementAt(1))).y;
        distance = Math.sqrt( ( (x1 - x0) * (x1 - x0)) + ( (y1 - y0) * (y1 - y0)));
        myY = y0;
        myX = x0;
        xInc = (x1 - x0) / (2 * distance);
        yInc = (y1 - y0) / (2 * distance);
        int pt = 0;
        len = (int) Math.round(2 * distance);
        for (i = 0; i < len; i++ ) {
            if (indexX != Math.round(myX) || indexY != Math.round(myY)) {
                indexY = (int) Math.round(myY);
                indexX = (int) Math.round(myX);
                position[pt].x = indexX;
                position[pt].y = indexY;
                index = indexY * xD + indexX;
                intensity[pt] = imageBuffer[index];
                pt++ ;
            }
            myX = myX + xInc;
            myY = myY + yInc;
        }
        return pt;
    }

    /**
     * Gets the position/intensity along a line VOI
     * @param position array that is filled with all x coordinates
     * @param intensity the corresponding intensities along the line
     * @param imageBuffer
     * @param xD x-Dimension of image
     * @return the number of points in the position and intensity array that hava valid data.
     */
    public int getPositionAndIntensityIndex(int position[], float intensity[], float imageBuffer[], int xD) {
        double distance;
        int i;
        int index, indexX = 0, indexY = 0;
        double myY, myX, yInc, xInc;
        double x0, x1, y0, y1;
        int len;
        x0 = ((Point3Df) (elementAt(0))).x;
        y0 = ((Point3Df) (elementAt(0))).y;
        x1 = ((Point3Df) (elementAt(1))).x;
        y1 = ((Point3Df) (elementAt(1))).y;
        distance = Math.sqrt( ( (x1 - x0) * (x1 - x0)) + ( (y1 - y0) * (y1 - y0)));
        myY = y0;
        myX = x0;
        xInc = (x1 - x0) / (2 * distance);
        yInc = (y1 - y0) / (2 * distance);
        int pt = 0;
        len = (int) Math.round(2 * distance);
        for (i = 0; i < len; i++ ) {
            if (indexX != Math.round(myX) || indexY != Math.round(myY)) {
                indexY = (int) Math.round(myY);
                indexX = (int) Math.round(myX);
                index = indexY * xD + indexX;
                position[pt] = index;
                intensity[pt] = imageBuffer[index];
                pt++ ;
            }
            myX = myX + xInc;
            myY = myY + yInc;
        }
        return pt;
    }

    /**
     * Tests if a point is near an edge point of a line
     * @param testPtX x coordinate of point
     * @param testPtY y coordinate of point
     * @param zoom
     * @param resolutionX X dimension thickness ratio (aspect ratio)
     * @param resolutionY Y dimension thickness ratio (aspect ratio)
     * @return returns true if near end point 0 or end point 1 returns false otherwise If true nearPoint equals 0 or 1,
     *         the value of the closest end point. If false, nearPoint equals NOT_A_POINT.
     */
    public boolean nearLinePoint(int testPtX, int testPtY, float zoom, float resolutionX, float resolutionY) {
        int xC, yC;
        float dist, minDistance = 100000;
        nearPoint = NOT_A_POINT;
        xC = Math.round( ((Point3Df) (elementAt(0))).x * zoom * resolutionX);
        yC = Math.round( ((Point3Df) (elementAt(0))).y * zoom * resolutionY);
        dist = (float) MipavMath.distance(testPtX, xC, testPtY, yC);
        if (dist < 3 && dist < minDistance) {
            minDistance = dist;
            nearPoint = 0;
        }
        xC = Math.round( ((Point3Df) (elementAt(1))).x * zoom * resolutionX);
        yC = Math.round( ((Point3Df) (elementAt(1))).y * zoom * resolutionY);
        dist = (float) MipavMath.distance(testPtX, xC, testPtY, yC);
        if (dist < 3 && dist < minDistance) {
            minDistance = dist;
            nearPoint = 1;
        }
        if (nearPoint == NOT_A_POINT)
            return false;
        else
            return true;
    }
}
