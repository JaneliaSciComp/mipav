package gov.nih.mipav.model.structures;


import gov.nih.mipav.*;

import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.jama.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.text.*;

import java.util.*;


/**
 * This class is fundamental to the VOI class in which points are stored that describe a curve of an VOI. The points are
 * 3D and are floats (see Point3Df). It extends VOIBase and therefore it extends Vector. Vector makes it very easy to
 * add points and remove points from the contour. An VOI is formed from one or many contours.
 *
 * <p>Routines that wish to sample all integer x,y values along a line must sample the line at 1/2 unit increments. As
 * an example, suppose we have a line with the following (x,y) values: (0.2,0.7),(0.7,1.2),(1.2,1.7), (1.7,2.2). If we
 * sample at unit increments with rounding we could either obtain the points (0,1),(1,2) or (1,1),(2,2). By sampling at
 * 1/2 unit increments along the line we sample at (0,1),(1,1),(1,2),(2,2).</p>
 *
 * @version  0.1 Oct 27, 1997
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      VOIBase
 * @see      VOILine
 * @see      VOI
 * @see      VOIPoint
 *
 *           <p>$Logfile: /mipav/src/gov/nih/mipav/model/structures/VOIContour.java $ $Revision: 114 $ $Date: 2/24/06
 *           3:34p $</p>
 */
public class VOIContour extends VOIBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6479418081230834617L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Indicates whether or not the contour is closed. */
    private boolean closed = true;

    /** DOCUMENT ME! */
    private boolean flagRetrace = false;

    /** These four variables are used in the retrace mode of the Contour. */
    private int indexRetrace = -99;

    /** Saves the vertex point of bounding box. See nearBoundPoint method in this class. */
    private int nearBoundPoint = NOT_A_POINT;

    /** Number of pixels in the array used in graphing intensity along the boundary. */
    private int numPixels;

    /** DOCUMENT ME! */
    private VOIContour oldContour; // old contour

    /**
     * Stores the x coordinates for the bounding box. The upper left corner and the lower right corner, respectively.
     */
    private float[] xBounds = new float[2];

    /** Used in contains method so that memory doesn't constantly need to be reallocated. */
    private float[] xPts = null;

    /**
     * Stores the y coordinates for the bounding box. The upper left corner and the lower right corner, respectively.
     */
    private float[] yBounds = new float[2];

    /** Used in contains method so that memory doesn't constantly need to be reallocated. */
    private float[] yPts = null;

    /**
     * Stores the z coordinates for the bounding box. The upper left corner and the lower right corner, respectively.
     */
    private float[] zBounds = new float[2];

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Construct a new closed or open VOI.
     *
     * @param  flag  whether the voi contour is closed
     */
    public VOIContour(boolean flag) {
        setClosed(flag);
    }

    /**
     * Creates a new VOIContour object.
     *
     * @param  voiName  DOCUMENT ME!
     * @param  flag     DOCUMENT ME!
     */
    public VOIContour(String voiName, boolean flag) {
        this.name = voiName;
        setClosed(flag);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Calculates the area of contour using vector cross product method - fast !!
     *
     * @return  returns the area
     */
    public float area() {
        int i;
        int length;
        float result = 0;
        Vector2Df oldVector, newVector;
        Point2Df[] pts = null;

        try {
            pts = new Point2Df[size()];
            length = size();

            for (i = 0; i < length; i++) {
                pts[i] = new Point2Df(((Point3Df) (elementAt(i))).x, ((Point3Df) (elementAt(i))).y);
            }
        } catch (OutOfMemoryError error) {
            System.gc();

            return 0;
        }

        if (size() >= 3) {
            oldVector = pts[1].formVector(pts[0]);

            for (i = 2; i < length; i++) {
                newVector = pts[i].formVector(pts[0]);
                result += newVector.crossProductVectors(oldVector) * 0.5;
                oldVector = newVector;
            }

            // if result is negative then points are ordered clockwise
            // if result is positive then points are ordered counter-clockwise
            if (result < 0) {
                result = -result;
            }
        }

        // System.out.println("Contour Area = " + result);
        return result;
    }

    /**
     * Calculates the average or total intensity of the VOIContour region.
     *
     * @param   data     image data
     * @param   imgXDim  x-dimension of the image
     *
     * @return  returns total sum of intensity in the VOIContour
     */
    public float calcIntensity(float[] data, int imgXDim) {
        int i, j;
        numPixels = 0;

        float sum = 0;
        contains(0, 0, true); // load buffers
        getBounds(xBounds, yBounds, zBounds);

        int startX = MipavMath.round(xBounds[0]);
        int startY = MipavMath.round(yBounds[0]);
        int endX = MipavMath.round(xBounds[1]);
        int endY = MipavMath.round(yBounds[1]);

        for (j = startY; j <= endY; j++) {

            for (i = startX; i <= endX; i++) {

                if (contains(i, j, false)) {
                    numPixels++;
                    sum += data[(j * imgXDim) + i];
                }
            }
        }

        return sum;
    }

    /**
     * Calculates the average or total intensity of the VOIContour region above a given threshold.
     *
     * @param   data       image data
     * @param   imgXDim    x-dimension of the image
     * @param   threshold  the minimum value to allow when calculating sum
     *
     * @return  returns total sum of intensity in the VOIContour
     */
    public float calcIntensityThreshold(float[] data, int imgXDim, float threshold) {
        int i, j;
        numPixels = 0;

        float sum = 0;
        contains(0, 0, true); // load buffers
        getBounds(xBounds, yBounds, zBounds);

        int startX = MipavMath.round(xBounds[0]);
        int startY = MipavMath.round(yBounds[0]);
        int endX = MipavMath.round(xBounds[1]);
        int endY = MipavMath.round(yBounds[1]);

        for (j = startY; j <= endY; j++) {

            for (i = startX; i <= endX; i++) {

                if (contains(i, j, false)) {

                    if (data[(j * imgXDim) + i] >= threshold) {
                        numPixels++;
                        sum += data[(j * imgXDim) + i];
                    }
                }
            }
        }

        return sum;
    }

    /**
     * Calculate the perimeter of the contour.
     *
     * @param   xRes  the x dimension resolution
     * @param   yRes  the y dimension resolution
     *
     * @return  the contour perimeter
     */
    public float calcPerimeter(float xRes, float yRes) {

        // calc the perimeter
        float perimeter = 0f;
        int idx;

        for (idx = 0; idx < (xPts.length - 1); idx++) {
            perimeter += MipavMath.distance(xPts[idx] * xRes, xPts[idx + 1] * xRes, yPts[idx] * yRes,
                                            yPts[idx + 1] * yRes);
        }

        if (closed) {
            perimeter += MipavMath.distance(xPts[xPts.length - 1] * xRes, xPts[0] * xRes, yPts[yPts.length - 1] * yRes,
                                            yPts[0] * yRes);
        }

        return perimeter;
    }

    /**
     * Calculates the average or total intensity of the VOIContour region.
     *
     * @param   data     image data
     * @param   imgXDim  x-dimension of the image
     * @param   RGorB    which channel to calc. (0 = R, 1 = G, 2 = B)
     *
     * @return  returns total sum of intensity in the VOIContour
     */
    public float calcRGBIntensity(float[] data, int imgXDim, int RGorB) {
        int i, j;
        numPixels = 0;

        float sum = 0;
        int os;
        contains(0, 0, true); // load buffers
        getBounds(xBounds, yBounds, zBounds);

        int startX = MipavMath.round(xBounds[0]);
        int startY = MipavMath.round(yBounds[0]);
        int endX = MipavMath.round(xBounds[1]);
        int endY = MipavMath.round(yBounds[1]);

        for (j = startY; j <= endY; j++) {

            for (i = startX, os = 0; i <= endX; i++, os++) {

                if (contains(i, j, false)) {
                    numPixels++;

                    if (RGorB == 0) {
                        sum += data[(j * 4 * imgXDim) + (startX * 4) + (os * 4) + 1];
                    } // skip alpha and get Red info
                    else if (RGorB == 1) {
                        sum += data[(j * 4 * imgXDim) + (startX * 4) + (os * 4) + 2];
                    } else {
                        sum += data[(j * 4 * imgXDim) + (startX * 4) + (os * 4) + 3];
                    }
                }
            }
        }

        return sum;
    }

    /**
     * Calculates the average or total intensity of the VOIContour region above a certain threshold.
     *
     * @param   data       image data
     * @param   imgXDim    x-dimension of the image
     * @param   RGorB      which channel to calc. (0 = R, 1 = G, 2 = B)
     * @param   threshold  the minimum value to allow when calculating sum
     *
     * @return  returns total sum of intensity in the VOIContour
     */
    public float calcRGBIntensityThreshold(float[] data, int imgXDim, int RGorB, float threshold) {
        int i, j;
        numPixels = 0;

        float sum = 0;
        int os;
        float tempSum = 0;
        contains(0, 0, true); // load buffers
        getBounds(xBounds, yBounds, zBounds);

        int startX = MipavMath.round(xBounds[0]);
        int startY = MipavMath.round(yBounds[0]);
        int endX = MipavMath.round(xBounds[1]);
        int endY = MipavMath.round(yBounds[1]);

        for (j = startY; j <= endY; j++) {

            for (i = startX, os = 0; i <= endX; i++, os++) {

                if (contains(i, j, false)) {
                    tempSum = data[(j * 4 * imgXDim) + (startX * 4) + (os * 4) + 1] +
                              data[(j * 4 * imgXDim) + (startX * 4) + (os * 4) + 2] +
                              data[(j * 4 * imgXDim) + (startX * 4) + (os * 4) + 3];

                    if ((tempSum / 3.0f) >= threshold) {
                        numPixels++;

                        if (RGorB == 0) {
                            sum += data[(j * 4 * imgXDim) + (startX * 4) + (os * 4) + 1];
                        } // skip alpha and get Red info
                        else if (RGorB == 1) {
                            sum += data[(j * 4 * imgXDim) + (startX * 4) + (os * 4) + 2];
                        } else {
                            sum += data[(j * 4 * imgXDim) + (startX * 4) + (os * 4) + 3];
                        }
                    }
                }
            }
        }

        return sum;
    }

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
        // System.err.println("contains :!!!!!!!!!!!?");
        if ((forceReload == true) || (xPts == null) || (yPts == null) || (size() > xPts.length)) {
            reloadPoints();
        }

        // System.out.println("contains : npts = " + nPts);
        for (i = 0; i < nPts; i++) {

            if (((yPts[j] <= y) && (y < yPts[i]) && (areaTwice(xPts[i], yPts[i], xPts[j], yPts[j], x, y) >= 0)) ||
                    ((yPts[i] <= y) && (y < yPts[j]) && (areaTwice(xPts[j], yPts[j], xPts[i], yPts[i], x, y) >= 0))) {
                isInside = !isInside;
            }

            j = i;
        }

        // if not inside maybe it is a striaght polyline
        if ((isInside == false) && !closed) {
            // System.err.println("doing near line from contour");
            // isInside = nearLine(_x, _y, 10);
        }

        return isInside;
    }

    /**
     * Forms the convexHull based on the set of points that defines this contour. The contour can be either be clockwise
     * or counter-clockwise.
     */
    public void convexHull() {
        int i, j, k;
        int length, start;
        float vAx, vAy, vBx, vBy, crossProd;
        boolean flag;
        Point3Df tmpPt;
        boolean repeat = true;
        boolean ccw = isCounterClockwise(); // ?
        length = size();

        if (length == 3) {
            return;
        } // Contour is a triangle. All triangles are convex.

        start = length / 4;

        if (start < 3) {
            start = 3;
        }

        if ((start % 2) == 0) {
            start++; // make start odd
        }

        for (k = start; k >= 3; k -= 2) {
            flag = true;

            while ((flag == true) && (size() > k)) {
                flag = false;

                for (i = 0; i < (length - (k - 1)); i++) {

                    // Form two vectors
                    vAx = ((Point3Df) (elementAt(i + (k / 2)))).x - ((Point3Df) (elementAt(i))).x;
                    vAy = ((Point3Df) (elementAt(i + (k / 2)))).y - ((Point3Df) (elementAt(i))).y;
                    vBx = ((Point3Df) (elementAt(i + (k - 1)))).x - ((Point3Df) (elementAt(i))).x;
                    vBy = ((Point3Df) (elementAt(i + (k - 1)))).y - ((Point3Df) (elementAt(i))).y;

                    // calc cross product
                    crossProd = (vAx * vBy) - (vAy * vBx);

                    if (ccw == false) {

                        if (crossProd <= 0) {
                            removeElementAt(i + (k / 2));
                            flag = true;
                            length = size();
                        }
                    } else {

                        if (crossProd >= 0) {
                            removeElementAt(i + (k / 2));
                            flag = true;
                            length = size();
                        }
                    }
                }
            }

            // Rotate points so that all concavities are removed.
            for (j = 0; j < (length / 2); j++) {
                tmpPt = (Point3Df) (elementAt(size() - 1));
                removeElementAt(size() - 1);
                insertElementAt(tmpPt, 0);
            }

            // Repeat to remove all local concavities
            if ((repeat == true) && (k == 3)) {
                k = 5;
                repeat = false;
            }
        }
    }

    /**
     * cycles through the points (making next or previous active).
     *
     * @param  direction  int the direction to cycle
     */
    public void cycleActivePt(int direction) {

        if (lastPoint != NOT_A_POINT) {
            int index = lastPoint;

            switch (direction) {

                case UP:
                case RIGHT:
                    index++;
                    break;

                case DOWN:
                case LEFT:
                    index--;
                    break;
            }

            if (index < 0) {
                index = this.size() - 1;
            } else if (index > (size() - 1)) {
                index = 0;
            }

            lastPoint = index;
        }
    }

    /**
     * Draws itself with the interior blended with the image pasted in as a buffer. The color 0f the VOI and the opacity
     * of the VOI are parameters of this method.
     *
     * @param  zoomX        scale for the x coordinate
     * @param  zoomY        scale for the y coordinate
     * @param  resolutionX  x resolution (aspect ratio)
     * @param  resolutionY  y resolution (aspect ratio)
     * @param  g            graphics contexts to paint in
     * @param  xDim         x dimension maximum
     * @param  yDim         y dimension maximum
     * @param  pixBuffer    pixel buffer of image
     * @param  opacity      opaqueness of the VOI
     * @param  color        color of VOI
     */
    public void drawBlendSelf(float zoomX, float zoomY, float resolutionX, float resolutionY, Graphics g, int xDim,
                              int yDim, int[] pixBuffer, float opacity, Color color) {

        int index;
        int x, y;
        int zoomXDim = MipavMath.round(xDim * zoomX * resolutionX);
        int colorInt;

        if (g == null) {
            MipavUtil.displayError("VOIContour.drawBlendSelf: graphics = null");

            return;
        }

        // reset the contains functions - loads lastest points or VOI
        contains(0, 0, true);
        getBounds(xBounds, yBounds, zBounds);

        for (y = (int) (yBounds[0] * zoomY * resolutionY); y < (int) (yBounds[1] * zoomY * resolutionY); y++) {

            for (x = (int) (xBounds[0] * zoomX * resolutionX); x < (int) (xBounds[1] * zoomX * resolutionX); x++) {

                if (contains((int) (x / (zoomX * resolutionX)), (int) (y / (zoomY * resolutionY)), false)) {
                    index = (y * zoomXDim) + x;

                    int opacityInt = (int) (opacity * 255);
                    opacityInt = opacityInt << 24;

                    colorInt = color.getRGB() & 0x00ffffff;
                    pixBuffer[index] = colorInt | opacityInt;
                }
            }
        }
    }

    /**
     * Draws center of mass of contour (2D).
     *
     * @param  scaleX       scale for the x coordinate
     * @param  scaleY       scale for the y coordinate
     * @param  resolutionX  X resolution (aspect ratio)
     * @param  resolutionY  Y resolution (aspect ratio)
     * @param  g            graphics to paint in
     */
    public void drawCenterOfMass(float scaleX, float scaleY, float resolutionX, float resolutionY, Graphics g) {
        int xS, yS;

        if (g == null) {
            MipavUtil.displayError("VOIContour.drawCenterOfMass: grapics = null");

            return;
        }

        getCenterOfMass();
        xS = MipavMath.round(cMassPt.x * scaleX * resolutionX);
        yS = MipavMath.round(cMassPt.y * scaleY * resolutionY);
        g.drawLine(xS, yS - 3, xS, yS + 3);
        g.drawLine(xS - 3, yS, xS + 3, yS);

        doName = (Preferences.is(Preferences.PREF_SHOW_VOI_NAME) && (name != null));

        if (doName) {
            g.drawString(name, xS - 10, yS - 5);
        } else if (label != null) {
            g.drawString(label, xS - 10, yS - 5);
        }
    }

    /**
     * Draws the length of open contour (Polyline).
     *
     * @param  g               graphics to draw in
     * @param  zoomX           magnification for the x coordinate
     * @param  zoomY           magnification for the y coordinate
     * @param  unitsOfMeasure  units of measure to be displayed on line.
     * @param  res             DOCUMENT ME!
     */
    public void drawLength(Graphics g, float zoomX, float zoomY, int[] unitsOfMeasure, float[] res) {
        double length;
        int i;
        String tmpString;

        if (g == null) {
            MipavUtil.displayError("VOILine drawTickMarks: grapics = null");

            return;
        }

        length = getLengthPtToPt(res);

        Point3Df pt = getCenterOfMass();

        // g.setColor(Color.yellow);
        tmpString = String.valueOf(length);
        i = tmpString.indexOf('.');

        if (tmpString.length() >= (i + 3)) {
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

        g.setColor(Color.black);
        g.drawString(tmpString, (int) (pt.x * zoomX), (int) ((pt.y * zoomY) - 1));
        g.drawString(tmpString, (int) (pt.x * zoomX), (int) ((pt.y * zoomY) + 1));
        g.drawString(tmpString, (int) ((pt.x * zoomX) + 1), (int) (pt.y * zoomY));
        g.drawString(tmpString, (int) ((pt.x * zoomX) - 1), (int) (pt.y * zoomY));
        g.setColor(Color.white);
        g.drawString(tmpString, (int) (pt.x * zoomX), (int) (pt.y * zoomY));
    }

    /**
     * Contour draws itself.
     *
     * @param  zoomX           magnification for the x coordinate
     * @param  zoomY           magnification for the y coordinate
     * @param  resolutionX     X resolution (aspect ratio)
     * @param  resolutionY     Y resolution (aspect ratio)
     * @param  originX         Start location of X origin
     * @param  originY         Start location of Y origin
     * @param  resols          array of pixel resolutions
     * @param  unitsOfMeasure  e.g. mm for millimeters etc.
     * @param  orientation     the orientation of the image slice where the VOI is to be drawn
     * @param  g               graphics to paint in
     * @param  boundingBox     boolean that indicates if boundingBox is on or off
     */
    public void drawSelf(float zoomX, float zoomY, float resolutionX, float resolutionY, float originX, float originY,
                         float[] resols, int[] unitsOfMeasure, int orientation, Graphics g, boolean boundingBox, int thickness) {
        Polygon gon = null;
        int j;
        String xUnitsString;
        String yUnitsString;
        float measuredWidth; // in mm. or inches or other units
        float measuredHeight; // in mm. or inches or other units
        DecimalFormat nf = new DecimalFormat(".##"); // float format for mm

        if (g == null) {
            MipavUtil.displayError("VOIContour.drawSelf: grapics = null");

            return;
        }

        gon = scalePolygon(zoomX, zoomY, resolutionX, resolutionY);
        
        if (active) {
        	if (closed) {
        		drawCenterOfMass(zoomX, zoomY, resolutionX, resolutionY, g);
        	} else {
        		drawLength(g, zoomX, zoomY, unitsOfMeasure, resols);
        	}
		}
        
        if (thickness == 1) {
        	if (closed == true) {
        		g.drawPolygon(gon);
        		
        	} else {
        		g.drawPolyline(gon.xpoints, gon.ypoints, gon.npoints);
        	}
        } else { 
        	//thickness is greater than 1... must draw differently
        
    		int x1, x2, y1, y2;   		
    		int dX, dY, dx, dy;
    		double ddx, ddy, lineLength, scale;
    		
            for (int i = 0; i < size() - 1; i++) {
                x1 = (int) ((((Point3Df) (elementAt(i))).x * zoomX * resolutionX) + 0.5);
                y1 = (int) ((((Point3Df) (elementAt(i))).y * zoomY * resolutionY) + 0.5);
                
                x2 = (int) ((((Point3Df) (elementAt(i+1))).x * zoomX * resolutionX) + 0.5);
                y2 = (int) ((((Point3Df) (elementAt(i+1))).y * zoomY * resolutionY) + 0.5);
                
                //now draw the connecting lines as polygons with thickness
                dX = x2 - x1;
                dY = y2 - y1;
                // line length
                lineLength = Math.sqrt(dX * dX + dY * dY);

                scale = (double)(thickness) / (2 * lineLength);

                // The x,y increments from an endpoint needed to create a rectangle...
                ddx = -scale * (double)dY;
                ddy = scale * (double)dX;
                ddx += (ddx > 0) ? 0.5 : -0.5;
                ddy += (ddy > 0) ? 0.5 : -0.5;
                dx = (int)ddx;
                dy = (int)ddy;

                // Now we can compute the corner points...
                int xPoints[] = new int[4];
                int yPoints[] = new int[4];

                xPoints[0] = x1 + dx; yPoints[0] = y1 + dy;
                xPoints[1] = x1 - dx; yPoints[1] = y1 - dy;
                xPoints[2] = x2 - dx; yPoints[2] = y2 - dy;
                xPoints[3] = x2 + dx; yPoints[3] = y2 + dy;

                g.fillPolygon(xPoints, yPoints, 4);
            }
            //if it's closed... connect the last and first points
            if (closed == true) {
            	x1 = (int) ((((Point3Df) (elementAt(size()-1))).x * zoomX * resolutionX) + 0.5);
                y1 = (int) ((((Point3Df) (elementAt(size()-1))).y * zoomY * resolutionY) + 0.5);
                
                x2 = (int) ((((Point3Df) (elementAt(0))).x * zoomX * resolutionX) + 0.5);
                y2 = (int) ((((Point3Df) (elementAt(0))).y * zoomY * resolutionY) + 0.5);
                
                //now draw the connecting lines as polygons with thickness
                dX = x2 - x1;
                dY = y2 - y1;
                // line length
                lineLength = Math.sqrt(dX * dX + dY * dY);

                scale = (double)(thickness) / (2 * lineLength);

                // The x,y increments from an endpoint needed to create a rectangle...
                ddx = -scale * (double)dY;
                ddy = scale * (double)dX;
                ddx += (ddx > 0) ? 0.5 : -0.5;
                ddy += (ddy > 0) ? 0.5 : -0.5;
                dx = (int)ddx;
                dy = (int)ddy;

                // Now we can compute the corner points...
                int xPoints[] = new int[4];
                int yPoints[] = new int[4];

                xPoints[0] = x1 + dx; yPoints[0] = y1 + dy;
                xPoints[1] = x1 - dx; yPoints[1] = y1 - dy;
                xPoints[2] = x2 - dx; yPoints[2] = y2 - dy;
                xPoints[3] = x2 + dx; yPoints[3] = y2 + dy;

                g.fillPolygon(xPoints, yPoints, 4);
            }
            
        	
        }

        if (active == true) {

            // if active draw little boxes at points
            for (j = 0; j < size(); j++) {

                if (nearPoint == j) { // Highlight Active point

                    // do nothing... active so hide point
                } else {
                    g.setColor(Color.white);
                    g.fillRect((int) (gon.xpoints[j] - 1.5 + 0.5f), (int) (gon.ypoints[j] - 1.5 + 0.5f), 3, 3);
                    g.setColor(Color.black);
                    g.drawRect((int) (gon.xpoints[j] - 1.5 + 0.5f), (int) (gon.ypoints[j] - 1.5 + 0.5f), 3, 3);
                }
            }

            // draw the 1st point only if not dragging the first point and if the active point (lastPoint)
            // is not the first point
            if ((nearPoint != 0) && (lastPoint != 0)) {
                g.setColor(Color.yellow);
                g.drawRect((int) (gon.xpoints[0] - 1.5 + 0.5f), (int) (gon.ypoints[0] - 1.5 + 0.5f), 3, 3);
            }

            // draw the active point dragging is taking place
            if ((nearPoint == NOT_A_POINT) && (lastPoint != NOT_A_POINT) && (lastPoint >= 0) &&
                    (this.size() > lastPoint)) {
                g.setColor(Color.GREEN);
                g.fillRect((int) (gon.xpoints[lastPoint] - 1.5 + 0.5f), (int) (gon.ypoints[lastPoint] - 1.5 + 0.5f), 3,
                           3);
            }

            if (boundingBox == true) {
                int x0, x1, y0, y1;
                x0 = (int) ((xBounds[0] * zoomX * resolutionX) + 0.5);
                x1 = (int) ((xBounds[1] * zoomX * resolutionX) + 0.5);
                y0 = (int) ((yBounds[0] * zoomY * resolutionY) + 0.5);
                y1 = (int) ((yBounds[1] * zoomY * resolutionY) + 0.5);
                g.setColor(Color.yellow.darker());
                g.drawRect(x0, y0, x1 - x0, y1 - y0);

                // draw corners of bounding box to make handles for resizing VOI
                g.fillRect(x0 - 2, y0 - 2, 5, 5);
                g.fillRect(x1 - 2, y0 - 2, 5, 5);
                g.fillRect(x0 - 2, y1 - 2, 5, 5);
                g.fillRect(x1 - 2, y1 - 2, 5, 5);

                // draw mid points of bounding box to make handles for resizing VOI
                g.fillRect(x0 + ((x1 - x0) / 2) - 2, y0 - 2, 5, 5);
                g.fillRect(x1 - 2, y0 + ((y1 - y0) / 2) - 2, 5, 5);
                g.fillRect(x0 + ((x1 - x0) / 2) - 2, y1 - 2, 5, 5);
                g.fillRect(x0 - 2, y0 + ((y1 - y0) / 2) - 2, 5, 5);

                // display the height/width of the bounding box above (or below) the top
                // midpoint and to the right of (or left of) the right midpoint
                String widthString, heightString;
                String measuredWidthString, measuredHeightString;
                String upperLeftLocationString;
                String lowerXYmmString;
                int width = (int) ((xBounds[1] - xBounds[0]) + 0.5f);
                int height = (int) ((yBounds[1] - yBounds[0]) + 0.5f);
                widthString = String.valueOf(width);
                heightString = String.valueOf(height);
                measuredWidth = (xBounds[1] - xBounds[0]) * resols[0];
                measuredHeight = (yBounds[1] - yBounds[0]) * resols[1];
                xUnitsString = FileInfoBase.getUnitsOfMeasureAbbrevStr(unitsOfMeasure[0]);
                yUnitsString = FileInfoBase.getUnitsOfMeasureAbbrevStr(unitsOfMeasure[1]);
                measuredWidthString = String.valueOf(nf.format(measuredWidth)) + " " + xUnitsString;
                measuredHeightString = String.valueOf(nf.format(measuredHeight)) + " " + yUnitsString;

                // System.err.println("width: " + widthString + " height: " + heightString);
                float lowerXmm = originX + (resols[0] * xBounds[0]);
                float lowerYmm = originY + (resols[1] * yBounds[0]);
                lowerXYmmString = "(" + String.valueOf(nf.format(lowerXmm)) + " " + xUnitsString + ", " +
                                  String.valueOf(nf.format(lowerYmm)) + " " + yUnitsString + ")";
                upperLeftLocationString = "(" + String.valueOf((int) (xBounds[0] + 0.5f)) + "," +
                                          String.valueOf((int) (yBounds[0] + 0.5f)) + ")";
                g.setColor(Color.black);

                // System.err.println(xBounds[0] + " " + xBounds[1] + " " + yBounds[0] + " " + yBounds[1]);
                if ((y1 - 45) < 0) {
                    g.drawString(measuredWidthString, (x0 + ((x1 - x0) / 2)) - 20, y1 + 21);
                    g.drawString(measuredWidthString, (x0 + ((x1 - x0) / 2)) - 20, y1 + 19);
                    g.drawString(measuredWidthString, (x0 + ((x1 - x0) / 2)) - 21, y1 + 20);
                    g.drawString(measuredWidthString, (x0 + ((x1 - x0) / 2)) - 19, y1 + 20);
                    g.drawString(widthString, (x0 + ((x1 - x0) / 2)) - 20, y1 + 36);
                    g.drawString(widthString, (x0 + ((x1 - x0) / 2)) - 20, y1 + 34);
                    g.drawString(widthString, (x0 + ((x1 - x0) / 2)) - 21, y1 + 35);
                    g.drawString(widthString, (x0 + ((x1 - x0) / 2)) - 19, y1 + 35);
                    g.setColor(Color.white);
                    g.drawString(measuredWidthString, (x0 + ((x1 - x0) / 2)) - 20, y1 + 20);
                    g.drawString(widthString, (x0 + ((x1 - x0) / 2)) - 20, y1 + 35);
                } else {
                    g.drawString(measuredWidthString, (x0 + ((x1 - x0) / 2)) - 20, y1 - 24);
                    g.drawString(measuredWidthString, (x0 + ((x1 - x0) / 2)) - 20, y1 - 26);
                    g.drawString(measuredWidthString, (x0 + ((x1 - x0) / 2)) - 21, y1 - 25);
                    g.drawString(measuredWidthString, (x0 + ((x1 - x0) / 2)) - 19, y1 - 25);
                    g.drawString(widthString, (x0 + ((x1 - x0) / 2)) - 20, y1 - 9);
                    g.drawString(widthString, (x0 + ((x1 - x0) / 2)) - 20, y1 - 11);
                    g.drawString(widthString, (x0 + ((x1 - x0) / 2)) - 21, y1 - 10);
                    g.drawString(widthString, (x0 + ((x1 - x0) / 2)) - 19, y1 - 10);
                    g.setColor(Color.white);
                    g.drawString(measuredWidthString, (x0 + ((x1 - x0) / 2)) - 20, y1 - 25);
                    g.drawString(widthString, (x0 + ((x1 - x0) / 2)) - 20, y1 - 10);
                }

                g.setColor(Color.black);

                if ((x0 - 40) < 0) {
                    g.drawString(measuredHeightString, x0 + 10, y0 + 10 + ((y1 - y0) / 2) + 1);
                    g.drawString(measuredHeightString, x0 + 10, y0 + 10 + ((y1 - y0) / 2) - 1);
                    g.drawString(measuredHeightString, x0 + 9, y0 + 10 + ((y1 - y0) / 2));
                    g.drawString(measuredHeightString, x0 + 11, y0 + 10 + ((y1 - y0) / 2));
                    g.drawString(heightString, x0 + 10, y0 + 25 + ((y1 - y0) / 2) + 1);
                    g.drawString(heightString, x0 + 10, y0 + 25 + ((y1 - y0) / 2) - 1);
                    g.drawString(heightString, x0 + 9, y0 + 25 + ((y1 - y0) / 2));
                    g.drawString(heightString, x0 + 11, y0 + 25 + ((y1 - y0) / 2));
                    g.setColor(Color.white);
                    g.drawString(measuredHeightString, x0 + 10, y0 + 10 + ((y1 - y0) / 2));
                    g.drawString(heightString, x0 + 10, y0 + 25 + ((y1 - y0) / 2));
                } else {
                    g.drawString(measuredHeightString, x0 - 35, y0 + 10 + ((y1 - y0) / 2) + 1);
                    g.drawString(measuredHeightString, x0 - 35, y0 + 10 + ((y1 - y0) / 2) - 1);
                    g.drawString(measuredHeightString, x0 - 36, y0 + 10 + ((y1 - y0) / 2));
                    g.drawString(measuredHeightString, x0 - 34, y0 + 10 + ((y1 - y0) / 2));
                    g.drawString(heightString, x0 - 35, y0 + 25 + ((y1 - y0) / 2) + 1);
                    g.drawString(heightString, x0 - 35, y0 + 25 + ((y1 - y0) / 2) - 1);
                    g.drawString(heightString, x0 - 36, y0 + 25 + ((y1 - y0) / 2));
                    g.drawString(heightString, x0 - 34, y0 + 25 + ((y1 - y0) / 2));
                    g.setColor(Color.white);
                    g.drawString(measuredHeightString, x0 - 35, y0 + 10 + ((y1 - y0) / 2));
                    g.drawString(heightString, x0 - 35, y0 + 25 + ((y1 - y0) / 2));
                }

                g.setColor(Color.black);

                if (((x0 - 40) <= 0) && ((y0 - 45) <= 0)) {
                    g.drawString(lowerXYmmString, x0 + 10, y0 + 11);
                    g.drawString(lowerXYmmString, x0 + 10, y0 + 13);
                    g.drawString(lowerXYmmString, x0 + 9, y0 + 12);
                    g.drawString(lowerXYmmString, x0 + 11, y0 + 12);
                    g.setColor(Color.white);
                    g.drawString(lowerXYmmString, x0 + 10, y0 + 12);
                    g.setColor(Color.black);
                    g.drawString(upperLeftLocationString, x0 + 10, y0 + 29);
                    g.drawString(upperLeftLocationString, x0 + 10, y0 + 17);
                    g.drawString(upperLeftLocationString, x0 + 9, y0 + 28);
                    g.drawString(upperLeftLocationString, x0 + 11, y0 + 28);
                    g.setColor(Color.white);
                    g.drawString(upperLeftLocationString, x0 + 10, y0 + 28);
                } else if (((x0 - 40) <= 0) && ((y0 - 45) > 0)) {
                    g.drawString(lowerXYmmString, x0 + 10, y0 - 25);
                    g.drawString(lowerXYmmString, x0 + 10, y0 - 27);
                    g.drawString(lowerXYmmString, x0 + 9, y0 - 26);
                    g.drawString(lowerXYmmString, x0 + 11, y0 - 26);
                    g.setColor(Color.white);
                    g.drawString(lowerXYmmString, x0 + 10, y0 - 26);
                    g.setColor(Color.black);
                    g.drawString(upperLeftLocationString, x0 + 10, y0 - 9);
                    g.drawString(upperLeftLocationString, x0 + 10, y0 - 21);
                    g.drawString(upperLeftLocationString, x0 + 9, y0 - 10);
                    g.drawString(upperLeftLocationString, x0 + 11, y0 - 10);
                    g.setColor(Color.white);
                    g.drawString(upperLeftLocationString, x0 + 10, y0 - 10);
                } else if (((x0 - 40) > 0) && ((y0 - 45) <= 0)) {
                    g.drawString(lowerXYmmString, x0 - 35, y0 + 11);
                    g.drawString(lowerXYmmString, x0 - 35, y0 + 13);
                    g.drawString(lowerXYmmString, x0 - 36, y0 + 12);
                    g.drawString(lowerXYmmString, x0 - 34, y0 + 12);
                    g.setColor(Color.white);
                    g.drawString(lowerXYmmString, x0 - 35, y0 + 12);
                    g.setColor(Color.black);
                    g.drawString(upperLeftLocationString, x0 - 35, y0 + 29);
                    g.drawString(upperLeftLocationString, x0 - 35, y0 + 17);
                    g.drawString(upperLeftLocationString, x0 - 36, y0 + 28);
                    g.drawString(upperLeftLocationString, x0 - 34, y0 + 28);
                    g.setColor(Color.white);
                    g.drawString(upperLeftLocationString, x0 - 35, y0 + 28);
                } else {
                    g.drawString(lowerXYmmString, x0 - 35, y0 - 25);
                    g.drawString(lowerXYmmString, x0 - 35, y0 - 27);
                    g.drawString(lowerXYmmString, x0 - 36, y0 - 26);
                    g.drawString(lowerXYmmString, x0 - 34, y0 - 26);
                    g.setColor(Color.white);
                    g.drawString(lowerXYmmString, x0 - 35, y0 - 26);
                    g.setColor(Color.black);
                    g.drawString(upperLeftLocationString, x0 - 35, y0 - 9);
                    g.drawString(upperLeftLocationString, x0 - 35, y0 - 11);
                    g.drawString(upperLeftLocationString, x0 - 36, y0 - 10);
                    g.drawString(upperLeftLocationString, x0 - 34, y0 - 10);
                    g.setColor(Color.white);
                    g.drawString(upperLeftLocationString, x0 - 35, y0 - 10);
                }

                // System.err.println("height: " + heightString + " width: " + widthString);
                g.setColor(Color.yellow.brighter());

                switch (nearBoundPoint) {

                    case 1:
                        g.fillRect(x0 - 2, y0 - 2, 5, 5);
                        g.setColor(Color.black);
                        g.drawRect(x0 - 2, y0 - 2, 4, 4);
                        break;

                    case 2:
                        g.fillRect(x1 - 2, y0 - 2, 5, 5);
                        g.setColor(Color.black);
                        g.drawRect(x1 - 2, y0 - 2, 4, 4);
                        break;

                    case 3:
                        g.fillRect(x1 - 2, y1 - 2, 5, 5);
                        g.setColor(Color.black);
                        g.drawRect(x1 - 2, y1 - 2, 4, 4);
                        break;

                    case 4:
                        g.fillRect(x0 - 2, y1 - 2, 5, 5);
                        g.setColor(Color.black);
                        g.drawRect(x0 - 2, y1 - 2, 4, 4);
                        break;

                    case 5:
                        g.fillRect(x0 + ((x1 - x0) / 2) - 2, y0 - 2, 5, 5);
                        g.setColor(Color.black);
                        g.drawRect(x0 + ((x1 - x0) / 2) - 2, y0 - 2, 4, 4);
                        break;

                    case 6:
                        g.fillRect(x1 - 2, y0 + ((y1 - y0) / 2) - 2, 5, 5);
                        g.setColor(Color.black);
                        g.drawRect(x1 - 2, y0 + ((y1 - y0) / 2) - 2, 4, 4);
                        break;

                    case 7:
                        g.fillRect(x0 + ((x1 - x0) / 2) - 2, y1 - 2, 5, 5);
                        g.setColor(Color.black);
                        g.drawRect(x0 + ((x1 - x0) / 2) - 2, y1 - 2, 4, 4);
                        break;

                    case 8:
                        g.fillRect(x0 - 2, y0 + ((y1 - y0) / 2) - 2, 5, 5);
                        g.setColor(Color.black);
                        g.drawRect(x0 - 2, y0 + ((y1 - y0) / 2) - 2, 4, 4);
                        break;
                }
            }
        }
    }

    /**
     * Contour draws itself.
     *
     * @param  zoomX           magnification for the x coordinate
     * @param  zoomY           magnification for the y coordinate
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
                         FileInfoBase fileInfo, int dim, int thickness) {
        Polygon gon = null;
        int j;
        String xUnitsString;
        String yUnitsString;
        float measuredWidth; // in mm. or inches or other units
        float measuredHeight; // in mm. or inches or other units
        DecimalFormat nf = new DecimalFormat(".##"); // float format for mm

        if (g == null) {
            MipavUtil.displayError("VOIContour.drawSelf: grapics = null");

            return;
        }

        gon = scalePolygon(zoomX, zoomY, resolutionX, resolutionY);
        
        if (active) {
        	if (closed) {
        		drawCenterOfMass(zoomX, zoomY, resolutionX, resolutionY, g);
        	} else {
        		drawLength(g, zoomX, zoomY, unitsOfMeasure, resols);
        	}
		}
        
        if (thickness == 1) {
        	if (closed == true) {
        		g.drawPolygon(gon);
        		
        	} else {
        		g.drawPolyline(gon.xpoints, gon.ypoints, gon.npoints);
        	}
        } else {
//        	thickness is greater than 1... must draw differently
            
    		int x1, x2, y1, y2;   		
    		int dX, dY, dx, dy;
    		double ddx, ddy, lineLength, scale;
    		
            for (int i = 0; i < size() - 1; i++) {
                x1 = (int) ((((Point3Df) (elementAt(i))).x * zoomX * resolutionX) + 0.5);
                y1 = (int) ((((Point3Df) (elementAt(i))).y * zoomY * resolutionY) + 0.5);
                
                x2 = (int) ((((Point3Df) (elementAt(i+1))).x * zoomX * resolutionX) + 0.5);
                y2 = (int) ((((Point3Df) (elementAt(i+1))).y * zoomY * resolutionY) + 0.5);
                
                //now draw the connecting lines as polygons with thickness
                dX = x2 - x1;
                dY = y2 - y1;
                // line length
                lineLength = Math.sqrt(dX * dX + dY * dY);

                scale = (double)(thickness) / (2 * lineLength);

                // The x,y increments from an endpoint needed to create a rectangle...
                ddx = -scale * (double)dY;
                ddy = scale * (double)dX;
                ddx += (ddx > 0) ? 0.5 : -0.5;
                ddy += (ddy > 0) ? 0.5 : -0.5;
                dx = (int)ddx;
                dy = (int)ddy;

                // Now we can compute the corner points...
                int xPoints[] = new int[4];
                int yPoints[] = new int[4];

                xPoints[0] = x1 + dx; yPoints[0] = y1 + dy;
                xPoints[1] = x1 - dx; yPoints[1] = y1 - dy;
                xPoints[2] = x2 - dx; yPoints[2] = y2 - dy;
                xPoints[3] = x2 + dx; yPoints[3] = y2 + dy;

                g.fillPolygon(xPoints, yPoints, 4);
            }
            //if it's closed... connect the last and first points
            if (closed == true) {
            	x1 = (int) ((((Point3Df) (elementAt(size()-1))).x * zoomX * resolutionX) + 0.5);
                y1 = (int) ((((Point3Df) (elementAt(size()-1))).y * zoomY * resolutionY) + 0.5);
                
                x2 = (int) ((((Point3Df) (elementAt(0))).x * zoomX * resolutionX) + 0.5);
                y2 = (int) ((((Point3Df) (elementAt(0))).y * zoomY * resolutionY) + 0.5);
                
                //now draw the connecting lines as polygons with thickness
                dX = x2 - x1;
                dY = y2 - y1;
                // line length
                lineLength = Math.sqrt(dX * dX + dY * dY);

                scale = (double)(thickness) / (2 * lineLength);

                // The x,y increments from an endpoint needed to create a rectangle...
                ddx = -scale * (double)dY;
                ddy = scale * (double)dX;
                ddx += (ddx > 0) ? 0.5 : -0.5;
                ddy += (ddy > 0) ? 0.5 : -0.5;
                dx = (int)ddx;
                dy = (int)ddy;

                // Now we can compute the corner points...
                int xPoints[] = new int[4];
                int yPoints[] = new int[4];

                xPoints[0] = x1 + dx; yPoints[0] = y1 + dy;
                xPoints[1] = x1 - dx; yPoints[1] = y1 - dy;
                xPoints[2] = x2 - dx; yPoints[2] = y2 - dy;
                xPoints[3] = x2 + dx; yPoints[3] = y2 + dy;

                g.fillPolygon(xPoints, yPoints, 4);
            }
        }
        if (active == true) {

            // if active draw little boxes at points
            for (j = 0; j < size(); j++) {

                if (nearPoint == j) { // Do not draw (dragging point)
                } else {
                    g.setColor(Color.white);
                    g.fillRect((int) (gon.xpoints[j] - 1.5 + 0.5f), (int) (gon.ypoints[j] - 1.5 + 0.5f), 3, 3);
                    g.setColor(Color.black);
                    g.drawRect((int) (gon.xpoints[j] - 1.5 + 0.5f), (int) (gon.ypoints[j] - 1.5 + 0.5f), 3, 3);
                }
            }

            // draw the 1st point only if not dragging the first point and if the active point (lastPoint)
            // is not the first point
            if ((nearPoint != 0) && (lastPoint != 0)) {
                g.setColor(Color.yellow);
                g.drawRect((int) (gon.xpoints[0] - 1.5 + 0.5f), (int) (gon.ypoints[0] - 1.5 + 0.5f), 3, 3);
            }

            // draw the active point dragging is taking place
            if ((nearPoint == NOT_A_POINT) && (lastPoint != NOT_A_POINT) && (lastPoint >= 0) &&
                    (this.size() > lastPoint)) {
                g.setColor(Color.GREEN);
                g.fillRect((int) (gon.xpoints[lastPoint] - 1.5 + 0.5f), (int) (gon.ypoints[lastPoint] - 1.5 + 0.5f), 3,
                           3);
            }

            if (boundingBox == true) {
                int x0, x1, y0, y1;
                x0 = (int) ((xBounds[0] * zoomX * resolutionX) + 0.5);
                x1 = (int) ((xBounds[1] * zoomX * resolutionX) + 0.5);
                y0 = (int) ((yBounds[0] * zoomY * resolutionY) + 0.5);
                y1 = (int) ((yBounds[1] * zoomY * resolutionY) + 0.5);
                g.setColor(Color.yellow.darker());
                g.drawRect(x0, y0, x1 - x0, y1 - y0);

                // draw corners of bounding box to make handles for resizing VOI
                g.fillRect(x0 - 2, y0 - 2, 5, 5);
                g.fillRect(x1 - 2, y0 - 2, 5, 5);
                g.fillRect(x0 - 2, y1 - 2, 5, 5);
                g.fillRect(x1 - 2, y1 - 2, 5, 5);

                // draw mid points of bounding box to make handles for resizing VOI
                g.fillRect(x0 + ((x1 - x0) / 2) - 2, y0 - 2, 5, 5);
                g.fillRect(x1 - 2, y0 + ((y1 - y0) / 2) - 2, 5, 5);
                g.fillRect(x0 + ((x1 - x0) / 2) - 2, y1 - 2, 5, 5);
                g.fillRect(x0 - 2, y0 + ((y1 - y0) / 2) - 2, 5, 5);

                // display the height/width of the bounding box above (or below) the top
                // midpoint and to the right of (or left of) the right midpoint
                String widthString, heightString;
                String measuredWidthString, measuredHeightString;
                String upperLeftLocationString;
                String lowerXYmmString;
                int width = (int) ((xBounds[1] - xBounds[0]) + 0.5f);
                int height = (int) ((yBounds[1] - yBounds[0]) + 0.5f);
                widthString = String.valueOf(width);
                heightString = String.valueOf(height);
                measuredWidth = (xBounds[1] - xBounds[0]) * resols[0];
                measuredHeight = (yBounds[1] - yBounds[0]) * resols[1];
                xUnitsString = FileInfoBase.getUnitsOfMeasureAbbrevStr(unitsOfMeasure[0]);
                yUnitsString = FileInfoBase.getUnitsOfMeasureAbbrevStr(unitsOfMeasure[1]);
                measuredWidthString = String.valueOf(nf.format(measuredWidth)) + " " + xUnitsString;
                measuredHeightString = String.valueOf(nf.format(measuredHeight)) + " " + yUnitsString;

                // System.err.println("width: " + widthString + " height: " + heightString);
                float[] result = new float[3];
                result = getOrigin(fileInfo, dim, originX, originY, resols);

                // float lowerXmm = originX + resols[0] * xBounds[0];
                // float lowerYmm = originY + resols[1] * yBounds[0];
                float lowerXmm = result[0];
                float lowerYmm = result[1];
                lowerXYmmString = "(" + String.valueOf(nf.format(lowerXmm)) + " " + xUnitsString + ", " +
                                  String.valueOf(nf.format(lowerYmm)) + " " + yUnitsString + ")";
                upperLeftLocationString = "(" + String.valueOf((int) (xBounds[0] + 0.5f)) + "," +
                                          String.valueOf((int) (yBounds[0] + 0.5f)) + ")";
                g.setColor(Color.black);

                // System.err.println(xBounds[0] + " " + xBounds[1] + " " + yBounds[0] + " " + yBounds[1]);
                if ((y1 - 45) < 0) {
                    g.drawString(measuredWidthString, (x0 + ((x1 - x0) / 2)) - 20, y1 + 21);
                    g.drawString(measuredWidthString, (x0 + ((x1 - x0) / 2)) - 20, y1 + 19);
                    g.drawString(measuredWidthString, (x0 + ((x1 - x0) / 2)) - 21, y1 + 20);
                    g.drawString(measuredWidthString, (x0 + ((x1 - x0) / 2)) - 19, y1 + 20);
                    g.drawString(widthString, (x0 + ((x1 - x0) / 2)) - 20, y1 + 36);
                    g.drawString(widthString, (x0 + ((x1 - x0) / 2)) - 20, y1 + 34);
                    g.drawString(widthString, (x0 + ((x1 - x0) / 2)) - 21, y1 + 35);
                    g.drawString(widthString, (x0 + ((x1 - x0) / 2)) - 19, y1 + 35);
                    g.setColor(Color.white);
                    g.drawString(measuredWidthString, (x0 + ((x1 - x0) / 2)) - 20, y1 + 20);
                    g.drawString(widthString, (x0 + ((x1 - x0) / 2)) - 20, y1 + 35);
                } else {
                    g.drawString(measuredWidthString, (x0 + ((x1 - x0) / 2)) - 20, y1 - 24);
                    g.drawString(measuredWidthString, (x0 + ((x1 - x0) / 2)) - 20, y1 - 26);
                    g.drawString(measuredWidthString, (x0 + ((x1 - x0) / 2)) - 21, y1 - 25);
                    g.drawString(measuredWidthString, (x0 + ((x1 - x0) / 2)) - 19, y1 - 25);
                    g.drawString(widthString, (x0 + ((x1 - x0) / 2)) - 20, y1 - 9);
                    g.drawString(widthString, (x0 + ((x1 - x0) / 2)) - 20, y1 - 11);
                    g.drawString(widthString, (x0 + ((x1 - x0) / 2)) - 21, y1 - 10);
                    g.drawString(widthString, (x0 + ((x1 - x0) / 2)) - 19, y1 - 10);
                    g.setColor(Color.white);
                    g.drawString(measuredWidthString, (x0 + ((x1 - x0) / 2)) - 20, y1 - 25);
                    g.drawString(widthString, (x0 + ((x1 - x0) / 2)) - 20, y1 - 10);
                }

                g.setColor(Color.black);

                if ((x0 - 40) < 0) {
                    g.drawString(measuredHeightString, x0 + 10, y0 + 10 + ((y1 - y0) / 2) + 1);
                    g.drawString(measuredHeightString, x0 + 10, y0 + 10 + ((y1 - y0) / 2) - 1);
                    g.drawString(measuredHeightString, x0 + 9, y0 + 10 + ((y1 - y0) / 2));
                    g.drawString(measuredHeightString, x0 + 11, y0 + 10 + ((y1 - y0) / 2));
                    g.drawString(heightString, x0 + 10, y0 + 25 + ((y1 - y0) / 2) + 1);
                    g.drawString(heightString, x0 + 10, y0 + 25 + ((y1 - y0) / 2) - 1);
                    g.drawString(heightString, x0 + 9, y0 + 25 + ((y1 - y0) / 2));
                    g.drawString(heightString, x0 + 11, y0 + 25 + ((y1 - y0) / 2));
                    g.setColor(Color.white);
                    g.drawString(measuredHeightString, x0 + 10, y0 + 10 + ((y1 - y0) / 2));
                    g.drawString(heightString, x0 + 10, y0 + 25 + ((y1 - y0) / 2));
                } else {
                    g.drawString(measuredHeightString, x0 - 35, y0 + 10 + ((y1 - y0) / 2) + 1);
                    g.drawString(measuredHeightString, x0 - 35, y0 + 10 + ((y1 - y0) / 2) - 1);
                    g.drawString(measuredHeightString, x0 - 36, y0 + 10 + ((y1 - y0) / 2));
                    g.drawString(measuredHeightString, x0 - 34, y0 + 10 + ((y1 - y0) / 2));
                    g.drawString(heightString, x0 - 35, y0 + 25 + ((y1 - y0) / 2) + 1);
                    g.drawString(heightString, x0 - 35, y0 + 25 + ((y1 - y0) / 2) - 1);
                    g.drawString(heightString, x0 - 36, y0 + 25 + ((y1 - y0) / 2));
                    g.drawString(heightString, x0 - 34, y0 + 25 + ((y1 - y0) / 2));
                    g.setColor(Color.white);
                    g.drawString(measuredHeightString, x0 - 35, y0 + 10 + ((y1 - y0) / 2));
                    g.drawString(heightString, x0 - 35, y0 + 25 + ((y1 - y0) / 2));
                }

                g.setColor(Color.black);

                if (((x0 - 40) <= 0) && ((y0 - 45) <= 0)) {
                    g.drawString(lowerXYmmString, x0 + 10, y0 + 11);
                    g.drawString(lowerXYmmString, x0 + 10, y0 + 13);
                    g.drawString(lowerXYmmString, x0 + 9, y0 + 12);
                    g.drawString(lowerXYmmString, x0 + 11, y0 + 12);
                    g.setColor(Color.white);
                    g.drawString(lowerXYmmString, x0 + 10, y0 + 12);
                    g.setColor(Color.black);
                    g.drawString(upperLeftLocationString, x0 + 10, y0 + 29);
                    g.drawString(upperLeftLocationString, x0 + 10, y0 + 17);
                    g.drawString(upperLeftLocationString, x0 + 9, y0 + 28);
                    g.drawString(upperLeftLocationString, x0 + 11, y0 + 28);
                    g.setColor(Color.white);
                    g.drawString(upperLeftLocationString, x0 + 10, y0 + 28);
                } else if (((x0 - 40) <= 0) && ((y0 - 45) > 0)) {
                    g.drawString(lowerXYmmString, x0 + 10, y0 - 25);
                    g.drawString(lowerXYmmString, x0 + 10, y0 - 27);
                    g.drawString(lowerXYmmString, x0 + 9, y0 - 26);
                    g.drawString(lowerXYmmString, x0 + 11, y0 - 26);
                    g.setColor(Color.white);
                    g.drawString(lowerXYmmString, x0 + 10, y0 - 26);
                    g.setColor(Color.black);
                    g.drawString(upperLeftLocationString, x0 + 10, y0 - 9);
                    g.drawString(upperLeftLocationString, x0 + 10, y0 - 21);
                    g.drawString(upperLeftLocationString, x0 + 9, y0 - 10);
                    g.drawString(upperLeftLocationString, x0 + 11, y0 - 10);
                    g.setColor(Color.white);
                    g.drawString(upperLeftLocationString, x0 + 10, y0 - 10);
                } else if (((x0 - 40) > 0) && ((y0 - 45) <= 0)) {
                    g.drawString(lowerXYmmString, x0 - 35, y0 + 11);
                    g.drawString(lowerXYmmString, x0 - 35, y0 + 13);
                    g.drawString(lowerXYmmString, x0 - 36, y0 + 12);
                    g.drawString(lowerXYmmString, x0 - 34, y0 + 12);
                    g.setColor(Color.white);
                    g.drawString(lowerXYmmString, x0 - 35, y0 + 12);
                    g.setColor(Color.black);
                    g.drawString(upperLeftLocationString, x0 - 35, y0 + 29);
                    g.drawString(upperLeftLocationString, x0 - 35, y0 + 17);
                    g.drawString(upperLeftLocationString, x0 - 36, y0 + 28);
                    g.drawString(upperLeftLocationString, x0 - 34, y0 + 28);
                    g.setColor(Color.white);
                    g.drawString(upperLeftLocationString, x0 - 35, y0 + 28);
                } else {
                    g.drawString(lowerXYmmString, x0 - 35, y0 - 25);
                    g.drawString(lowerXYmmString, x0 - 35, y0 - 27);
                    g.drawString(lowerXYmmString, x0 - 36, y0 - 26);
                    g.drawString(lowerXYmmString, x0 - 34, y0 - 26);
                    g.setColor(Color.white);
                    g.drawString(lowerXYmmString, x0 - 35, y0 - 26);
                    g.setColor(Color.black);
                    g.drawString(upperLeftLocationString, x0 - 35, y0 - 9);
                    g.drawString(upperLeftLocationString, x0 - 35, y0 - 11);
                    g.drawString(upperLeftLocationString, x0 - 36, y0 - 10);
                    g.drawString(upperLeftLocationString, x0 - 34, y0 - 10);
                    g.setColor(Color.white);
                    g.drawString(upperLeftLocationString, x0 - 35, y0 - 10);
                }

                // System.err.println("height: " + heightString + " width: " + widthString);
                g.setColor(Color.yellow.brighter());

                switch (nearBoundPoint) {

                    case 1:
                        g.fillRect(x0 - 2, y0 - 2, 5, 5);
                        g.setColor(Color.black);
                        g.drawRect(x0 - 2, y0 - 2, 4, 4);
                        break;

                    case 2:
                        g.fillRect(x1 - 2, y0 - 2, 5, 5);
                        g.setColor(Color.black);
                        g.drawRect(x1 - 2, y0 - 2, 4, 4);
                        break;

                    case 3:
                        g.fillRect(x1 - 2, y1 - 2, 5, 5);
                        g.setColor(Color.black);
                        g.drawRect(x1 - 2, y1 - 2, 4, 4);
                        break;

                    case 4:
                        g.fillRect(x0 - 2, y1 - 2, 5, 5);
                        g.setColor(Color.black);
                        g.drawRect(x0 - 2, y1 - 2, 4, 4);
                        break;

                    case 5:
                        g.fillRect(x0 + ((x1 - x0) / 2) - 2, y0 - 2, 5, 5);
                        g.setColor(Color.black);
                        g.drawRect(x0 + ((x1 - x0) / 2) - 2, y0 - 2, 4, 4);
                        break;

                    case 6:
                        g.fillRect(x1 - 2, y0 + ((y1 - y0) / 2) - 2, 5, 5);
                        g.setColor(Color.black);
                        g.drawRect(x1 - 2, y0 + ((y1 - y0) / 2) - 2, 4, 4);
                        break;

                    case 7:
                        g.fillRect(x0 + ((x1 - x0) / 2) - 2, y1 - 2, 5, 5);
                        g.setColor(Color.black);
                        g.drawRect(x0 + ((x1 - x0) / 2) - 2, y1 - 2, 4, 4);
                        break;

                    case 8:
                        g.fillRect(x0 - 2, y0 + ((y1 - y0) / 2) - 2, 5, 5);
                        g.setColor(Color.black);
                        g.drawRect(x0 - 2, y0 + ((y1 - y0) / 2) - 2, 4, 4);
                        break;
                }
            }
        }
    }

    /**
     * Draws the vertices of the contour.
     *
     * @param  zoomX        magnification for the x coordinate
     * @param  zoomY        magnification for the y coordinate
     * @param  resolutionX  X resolution (aspect ratio)
     * @param  resolutionY  Y resolution (aspect ratio)
     * @param  g            graphics to paint in
     * @param  boundingBox  boolean that indicates if boundingBox is on or off
     */
    public void drawVertices(float zoomX, float zoomY, float resolutionX, float resolutionY, Graphics g,
                             boolean boundingBox) {
        Polygon gon = null;
        int j;

        if (g == null) {
            MipavUtil.displayError("VOIContour.drawSelf: grapics = null");

            return;
        }

        gon = scalePolygon(zoomX, zoomY, resolutionX, resolutionY);

        // if active draw little boxes at points
        if (active == true) {

            // drawCenterOfMass(scaleX, scaleY, g);
            for (j = 0; j < size(); j++) {

                if (nearPoint != j) { // Highlight Active point
                    g.setColor(Color.white);
                    g.fillRect((int) (gon.xpoints[j] - 1.5 + 0.5f), (int) (gon.ypoints[j] - 1.5 + 0.5f), 3, 3);
                    g.setColor(Color.black);
                    g.drawRect((int) (gon.xpoints[j] - 1.5 + 0.5f), (int) (gon.ypoints[j] - 1.5 + 0.5f), 3, 3);
                }
            }

            g.setColor(Color.yellow);
            g.drawRect((int) (gon.xpoints[0] - 1.5 + 0.5f), (int) (gon.ypoints[0] - 1.5 + 0.5f), 3, 3);
        }

        if (boundingBox == true) {
            int x0, x1, y0, y1;
            x0 = (int) ((xBounds[0] * zoomX * resolutionX) + 0.5f);
            x1 = (int) ((xBounds[1] * zoomX * resolutionX) + 0.5f);
            y0 = (int) ((yBounds[0] * zoomY * resolutionY) + 0.5f);
            y1 = (int) ((yBounds[1] * zoomY * resolutionY) + 0.5f);
            g.setColor(Color.yellow.darker());
            g.drawRect(x0, y0, x1 - x0, y1 - y0);

            // draw corners of bounding box to make handles for resizing VOI
            g.fillRect(x0 - 2, y0 - 2, 5, 5);
            g.fillRect(x1 - 2, y0 - 2, 5, 5);
            g.fillRect(x0 - 2, y1 - 2, 5, 5);
            g.fillRect(x1 - 2, y1 - 2, 5, 5);

            // draw mid points of bounding box to make handles for resizing VOI
            g.fillRect(MipavMath.round(x0 + ((x1 - x0) / 2) - 2), y0 - 2, 5, 5);
            g.fillRect(x1 - 2, Math.round(y0 + ((y1 - y0) / 2) - 2), 5, 5);
            g.fillRect(MipavMath.round(x0 + ((x1 - x0) / 2) - 2), y1 - 2, 5, 5);
            g.fillRect(x0 - 2, Math.round(y0 + ((y1 - y0) / 2) - 2), 5, 5);
            g.setColor(Color.yellow.brighter());

            switch (nearBoundPoint) {

                case 1:
                    g.fillRect(x0 - 2, y0 - 2, 5, 5);
                    g.setColor(Color.black);
                    g.drawRect(x0 - 2, y0 - 2, 4, 4);
                    break;

                case 2:
                    g.fillRect(x1 - 2, y0 - 2, 5, 5);
                    g.setColor(Color.black);
                    g.drawRect(x1 - 2, y0 - 2, 4, 4);
                    break;

                case 3:
                    g.fillRect(x1 - 2, y1 - 2, 5, 5);
                    g.setColor(Color.black);
                    g.drawRect(x1 - 2, y1 - 2, 4, 4);
                    break;

                case 4:
                    g.fillRect(x0 - 2, y1 - 2, 5, 5);
                    g.setColor(Color.black);
                    g.drawRect(x0 - 2, y1 - 2, 4, 4);
                    break;

                case 5:
                    g.fillRect(MipavMath.round(x0 + ((x1 - x0) / 2) - 2), y0 - 2, 5, 5);
                    g.setColor(Color.black);
                    g.drawRect(MipavMath.round(x0 + ((x1 - x0) / 2) - 2), y0 - 2, 4, 4);
                    break;

                case 6:
                    g.fillRect(x1 - 2, MipavMath.round(y0 + ((y1 - y0) / 2) - 2), 5, 5);
                    g.setColor(Color.black);
                    g.drawRect(x1 - 2, MipavMath.round(y0 + ((y1 - y0) / 2) - 2), 4, 4);
                    break;

                case 7:
                    g.fillRect(MipavMath.round(x0 + ((x1 - x0) / 2) - 2), y1 - 2, 5, 5);
                    g.setColor(Color.black);
                    g.drawRect(MipavMath.round(x0 + ((x1 - x0) / 2) - 2), y1 - 2, 4, 4);
                    break;

                case 8:
                    g.fillRect(x0 - 2, MipavMath.round(y0 + ((y1 - y0) / 2) - 2), 5, 5);
                    g.setColor(Color.black);
                    g.drawRect(x0 - 2, MipavMath.round(y0 + ((y1 - y0) / 2) - 2), 4, 4);
                    break;
            }
        }
    }

    /**
     * Exports contour that has been transformed (Affine).
     *
     * @param   tMatrix  transformation matrix
     *
     * @return  returns the polygon
     */
    public VOIContour exportContour(TransMatrix tMatrix) {
        int i;
        VOIContour transformedContour = null;
        Point3Df pt = null;

        try {

            if (closed == true) {
                transformedContour = new VOIContour(true);
            } else {
                transformedContour = new VOIContour(false);
            }

            pt = new Point3Df();
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }

        for (i = 0; i < size(); i++) {
            tMatrix.transform((Point3Df) (elementAt(i)), pt);
            transformedContour.addElement(pt);
        }

        return transformedContour;
    }

    /**
     * Exports a transformed contour.
     *
     * @param   thetaX  rotation in x in degrees
     * @param   thetaY  rotation in y in degrees
     * @param   thetaZ  rotation in z in degrees
     * @param   tX      translation in x
     * @param   tY      translation in y
     * @param   tZ      translation in z
     * @param   scaleX  zoom in x
     * @param   scaleY  zoom in y
     * @param   scaleZ  zoom in z
     *
     * @return  returns Contour
     */
    public VOIContour exportContour(float thetaX, float thetaY, float thetaZ, float tX, float tY, float tZ,
                                    float scaleX, float scaleY, float scaleZ) {
        int i;
        VOIContour transformedContour = null;
        TransMatrix tMatrix = null;
        Point3Df pt = null;

        try {

            if (closed == true) {
                transformedContour = new VOIContour(true);
            } else {
                transformedContour = new VOIContour(false);
            }

            tMatrix = new TransMatrix(4);
            pt = new Point3Df();
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }

        getCenterOfMass();

        // construct transMatrix object
        tMatrix.setTranslate((cMassPt.x + tX), (cMassPt.y + tY), (cMassPt.z + tZ));
        tMatrix.setRotate(thetaX, thetaY, thetaZ, TransMatrix.DEGREES);
        tMatrix.setZoom(scaleX, scaleY, scaleZ);
        tMatrix.setTranslate(-cMassPt.x, -cMassPt.y, -cMassPt.z);

        for (i = 0; i < size(); i++) {
            tMatrix.transform((Point3Df) (elementAt(i)), pt);
            transformedContour.addElement(pt);
        }

        return transformedContour;
    }

    /**
     * Exports the points of the contour as a polygon (z data can not be encoded in the polygon).
     *
     * @return  returns polygon
     */
    public Polygon exportPolygon() {
        int i;
        int x, y;
        Polygon gon = null;

        try {
            gon = new Polygon();
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }

        for (i = 0; i < size(); i++) {
            x = (int) (((Point3Df) (elementAt(i))).x + 0.5);
            y = (int) (((Point3Df) (elementAt(i))).y + 0.5);
            gon.addPoint(x, y);
        }

        return gon;
    }

    /**
     * Exports polygon of the contour.
     *
     * @param   tMatrix  transformation matrix
     *
     * @return  returns the polygon
     */
    public Polygon exportPolygon(TransMatrix tMatrix) {
        int i;
        Point3Df pt = null;
        Polygon transformedGon = null;

        try {
            pt = new Point3Df();
            transformedGon = new Polygon();
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }

        for (i = 0; i < size(); i++) {
            tMatrix.transform((Point3Df) (elementAt(i)), pt);
            transformedGon.addPoint((int) (pt.x + 0.5), (int) (pt.y + 0.5));
        }

        return transformedGon;
    }

    /**
     * Returns a scaled polygon of the contour.
     *
     * @param   scaleX       scale of polygon's x coordinates
     * @param   scaleY       scale of polygon's y coordinates
     * @param   resolutionX  pixel resolution int the x direction (aspect ratio)
     * @param   resolutionY  pixel resolution int the y direction (aspect ratio)
     *
     * @return  return the polygon
     */
    public Polygon exportPolygon(float scaleX, float scaleY, float resolutionX, float resolutionY) {
        return (scalePolygon(scaleX, scaleY, resolutionX, resolutionY));
    }

    /**
     * Exports the polygon of the contour with some transformation.
     *
     * @param   thetaX  rotation in x in degrees
     * @param   thetaY  rotation in y in degrees
     * @param   thetaZ  rotation in z in degrees
     * @param   tX      translation in x
     * @param   tY      translation in y
     * @param   tZ      translation in z
     * @param   scaleX  zoom in x
     * @param   scaleY  zoom in y
     * @param   scaleZ  zoom in z
     *
     * @return  returns polygon
     */
    public Polygon exportPolygon(float thetaX, float thetaY, float thetaZ, float tX, float tY, float tZ, float scaleX,
                                 float scaleY, float scaleZ) {
        int i;
        Point3Df pt = null;
        Polygon transformedGon = null;
        TransMatrix tMatrix = null;

        // change so that I don't allocate every time ?
        try {
            pt = new Point3Df();
            transformedGon = new Polygon();
            tMatrix = new TransMatrix(4);
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }

        getCenterOfMass();

        // construct transMatrix object
        // check into order of translate
        tMatrix.setTranslate((cMassPt.x + tX), (cMassPt.y + tY), (cMassPt.z + tZ));
        tMatrix.setRotate(thetaX, thetaY, thetaZ, TransMatrix.DEGREES);
        tMatrix.setZoom(scaleX, scaleY, scaleZ);
        tMatrix.setTranslate(-cMassPt.x, -cMassPt.y, -cMassPt.z);

        for (i = 0; i < size(); i++) {
            tMatrix.transform((Point3Df) (elementAt(i)), pt);
            transformedGon.addPoint((int) (pt.x + 0.5), (int) (pt.y + 0.5));
        }

        return transformedGon;
    }

    /**
     * Finds the position/intensity along the boundary of this contour VOI.
     *
     * @param   position     array that is filled with distances along the VOI (i.e. mm)
     * @param   intensity    the corresponding intensities along the line
     * @param   imageBuffer  image array in which VOIs and lines are found
     * @param   resolutions  image resolutions
     * @param   xDim         x-Dimension of image
     * @param   yDim         y-Dimension of image
     *
     * @return  the number of points in the position and intensity array that hava valid data.
     */
    public int findPositionAndIntensity(float[] position, float[] intensity, float[] imageBuffer, float[] resolutions,
                                        int xDim, int yDim) {
        int i, j, end, pt;
        int index, indexX = 0, indexY = 0;
        double myY, myX, yInc, xInc;
        double x0, x1, y0, y1;
        double totDistance = 0;
        double ptDistance = 0;
        double subPtDistance = 0;
        end = size() - 1;
        pt = 0;

        for (j = 0; j < end; j++) {
            x0 = ((Point3Df) (elementAt(j))).x;
            y0 = ((Point3Df) (elementAt(j))).y;
            x1 = ((Point3Df) (elementAt(j + 1))).x;
            y1 = ((Point3Df) (elementAt(j + 1))).y;
            ptDistance = Math.sqrt(((x1 - x0) * (x1 - x0) * (resolutions[0]) * (resolutions[0])) +
                                   ((y1 - y0) * (y1 - y0) * (resolutions[1]) * (resolutions[1])));
            myX = x0;
            myY = y0;
            xInc = ((x1 - x0) * resolutions[0]) / (ptDistance * 2);
            yInc = ((y1 - y0) * resolutions[1]) / (ptDistance * 2);

            for (i = 0, subPtDistance = 0; subPtDistance < ptDistance; i++) {

                if ((indexX != MipavMath.round(myX)) || (indexY != Math.round(myY))) {
                    indexY = (int) MipavMath.round(myY);
                    indexX = (int) MipavMath.round(myX);
                    index = (indexY * xDim) + indexX;
                    subPtDistance = Math.sqrt(((myX - x0) * (myX - x0) * (resolutions[0]) * (resolutions[0])) +
                                              ((myY - y0) * (myY - y0) * (resolutions[1]) * (resolutions[1])));
                    position[pt] = (float) (totDistance + subPtDistance);
                    intensity[pt] = imageBuffer[index];
                    pt++;
                }

                myX = myX + xInc;
                myY = myY + yInc;
            }

            pt--;
            totDistance += ptDistance;
        }

        if (closed == true) {
            x0 = ((Point3Df) (elementAt(size() - 1))).x;
            y0 = ((Point3Df) (elementAt(size() - 1))).y;
            x1 = ((Point3Df) (elementAt(0))).x;
            y1 = ((Point3Df) (elementAt(0))).y;
            ptDistance = Math.sqrt(((x1 - x0) * (x1 - x0) * (resolutions[0]) * (resolutions[0])) +
                                   ((y1 - y0) * (y1 - y0) * (resolutions[1]) * (resolutions[1])));
            myX = x0;
            myY = y0;
            xInc = ((x1 - x0) * resolutions[0]) / (ptDistance * 2);
            yInc = ((y1 - y0) * resolutions[1]) / (ptDistance * 2);

            for (subPtDistance = 0; subPtDistance < ptDistance;) {

                if ((indexX != MipavMath.round(myX)) || (indexY != Math.round(myY))) {
                    indexY = (int) MipavMath.round(myY);
                    indexX = (int) MipavMath.round(myX);
                    index = (indexY * xDim) + indexX;
                    subPtDistance = Math.sqrt(((myX - x0) * (myX - x0) * (resolutions[0]) * (resolutions[0])) +
                                              ((myY - y0) * (myY - y0) * (resolutions[1]) * (resolutions[1])));
                    position[pt] = (float) (totDistance + subPtDistance);
                    intensity[pt] = imageBuffer[index];
                    pt++;
                }

                myX = myX + xInc;
                myY = myY + yInc;
            }

            indexY = (int) y1;
            indexX = (int) x1;
            index = (indexY * xDim) + indexX;
            subPtDistance = Math.sqrt(((x1 - x0) * (x1 - x0) * (resolutions[0]) * (resolutions[0])) +
                                      ((y1 - y0) * (y1 - y0) * (resolutions[1]) * (resolutions[1])));
            pt--;
            position[pt] = (float) (totDistance + subPtDistance);
            intensity[pt] = imageBuffer[index];
        }

        return pt + 1;
    }

    /**
     * Finds the position/intensity along the boundary of this contour VOI of an RGB image.
     *
     * @param   position     array that is filled with distances along the VOI (i.e. mm)
     * @param   intensity    the corresponding intensities along the line
     * @param   RGorB        channel to calculate data
     * @param   imageBuffer  image array in which VOIs and lines are found
     * @param   resolutions  image resolutions
     * @param   xDim         x-Dimension of image
     * @param   yDim         y-Dimension of image
     *
     * @return  the number of points in the position and intensity array that hava valid data.
     */
    public int findPositionAndIntensityRGB(float[] position, float[] intensity, int RGorB, float[] imageBuffer,
                                           float[] resolutions, int xDim, int yDim) {
        int i, j, end, pt;
        int index, indexX = 0, indexY = 0;
        double myY, myX, yInc, xInc;
        double x0, x1, y0, y1;
        double totDistance = 0;
        double ptDistance = 0;
        double subPtDistance = 0;
        float val;
        end = size() - 1;
        pt = 0;

        for (j = 0; j < end; j++) {
            x0 = ((Point3Df) (elementAt(j))).x;
            y0 = ((Point3Df) (elementAt(j))).y;
            x1 = ((Point3Df) (elementAt(j + 1))).x;
            y1 = ((Point3Df) (elementAt(j + 1))).y;
            ptDistance = Math.sqrt(((x1 - x0) * (x1 - x0) * (resolutions[0]) * (resolutions[0])) +
                                   ((y1 - y0) * (y1 - y0) * (resolutions[1]) * (resolutions[1])));
            myX = x0;
            myY = y0;
            xInc = ((x1 - x0) * resolutions[0]) / (ptDistance * 2);
            yInc = ((y1 - y0) * resolutions[1]) / (ptDistance * 2);

            for (i = 0, subPtDistance = 0; subPtDistance < ptDistance; i++) {

                if ((indexX != MipavMath.round(myX)) || (indexY != Math.round(myY))) {
                    indexY = (int) MipavMath.round(myY);
                    indexX = (int) MipavMath.round(myX);
                    index = (indexY * xDim) + indexX;
                    subPtDistance = Math.sqrt(((myX - x0) * (myX - x0) * (resolutions[0]) * (resolutions[0])) +
                                              ((myY - y0) * (myY - y0) * (resolutions[1]) * (resolutions[1])));
                    position[pt] = (float) (totDistance + subPtDistance);

                    if (RGorB == 0) {
                        val = imageBuffer[(4 * index) + 1];
                    } // skip alpha and get Red info
                    else if (RGorB == 1) {
                        val = imageBuffer[(4 * index) + 2];
                    } else {
                        val = imageBuffer[(4 * index) + 3];
                    }

                    intensity[pt] = val;
                    pt++;
                }

                myX = myX + xInc;
                myY = myY + yInc;
            }

            pt--;
            totDistance += ptDistance;
        }

        if (closed == true) {
            x0 = ((Point3Df) (elementAt(size() - 1))).x;
            y0 = ((Point3Df) (elementAt(size() - 1))).y;
            x1 = ((Point3Df) (elementAt(0))).x;
            y1 = ((Point3Df) (elementAt(0))).y;
            ptDistance = Math.sqrt(((x1 - x0) * (x1 - x0) * (resolutions[0]) * (resolutions[0])) +
                                   ((y1 - y0) * (y1 - y0) * (resolutions[1]) * (resolutions[1])));
            myX = x0;
            myY = y0;
            xInc = ((x1 - x0) * resolutions[0]) / (ptDistance * 2);
            yInc = ((y1 - y0) * resolutions[1]) / (ptDistance * 2);

            for (subPtDistance = 0; subPtDistance < ptDistance;) {

                if ((indexX != MipavMath.round(myX)) || (indexY != Math.round(myY))) {
                    indexY = (int) MipavMath.round(myY);
                    indexX = (int) MipavMath.round(myX);
                    index = (indexY * xDim) + indexX;
                    subPtDistance = Math.sqrt(((myX - x0) * (myX - x0) * (resolutions[0]) * (resolutions[0])) +
                                              ((myY - y0) * (myY - y0) * (resolutions[1]) * (resolutions[1])));
                    position[pt] = (float) (totDistance + subPtDistance);

                    if (RGorB == 0) {
                        val = imageBuffer[(4 * index) + 1];
                    } // skip alpha and get Red info
                    else if (RGorB == 1) {
                        val = imageBuffer[(4 * index) + 2];
                    } else {
                        val = imageBuffer[(4 * index) + 3];
                    }

                    intensity[pt] = val;
                    pt++;
                }

                myX = myX + xInc;
                myY = myY + yInc;
            }

            indexY = (int) y1;
            indexX = (int) x1;
            index = (indexY * xDim) + indexX;
            subPtDistance = Math.sqrt(((x1 - x0) * (x1 - x0) * (resolutions[0]) * (resolutions[0])) +
                                      ((y1 - y0) * (y1 - y0) * (resolutions[1]) * (resolutions[1])));
            pt--;
            position[pt] = (float) (totDistance + subPtDistance);

            if (RGorB == 0) {
                val = imageBuffer[(4 * index) + 1];
            } // skip alpha and get Red info
            else if (RGorB == 1) {
                val = imageBuffer[(4 * index) + 2];
            } else {
                val = imageBuffer[(4 * index) + 3];
            }

            intensity[pt] = val;
        }

        return pt + 1;
    }

    /**
     * Gets the active points location (point3df).
     *
     * @return  Point3Df the location
     */
    public Point3Df getActivePt() {
        Point3Df pt = null;

        if ((lastPoint >= 0) && (lastPoint < this.size())) {
            pt = (Point3Df) (elementAt(lastPoint));
        }

        return pt;
    }

    /**
     * Calculates the bounds of the contour.
     *
     * @param  x  two element array where x[0] = min extent of the Contour and x[1] = max extent of the Contour in the x
     *            dimension
     * @param  y  two element array where y[0] = min extent of the Contour and y[1] = max extent of the Contour in the y
     *            dimension
     * @param  z  two element array where z[0] = min extent of the Contour and z[1] = max extent of the Contour in the z
     *            dimension
     */
    public void getBounds(int[] x, int[] y, int[] z) {
        int i;
        int xx, yy, zz;
        x[0] = 10000000;
        x[1] = -10000000;
        y[0] = 10000000;
        y[1] = -10000000;
        z[0] = 10000000;
        z[1] = -10000000;

        for (i = 0; i < size(); i++) {
            xx = MipavMath.round(((Point3Df) (elementAt(i))).x);
            yy = MipavMath.round(((Point3Df) (elementAt(i))).y);
            zz = MipavMath.round(((Point3Df) (elementAt(i))).z);

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
     * Calculates the bounds of the contour.
     *
     * @param  x  two element array where x[0] = min extent of the Contour and x[1] = max extent of the Contour in the x
     *            dimension
     * @param  y  two element array where y[0] = min extent of the Contour and y[1] = max extent of the Contour in the y
     *            dimension
     * @param  z  two element array where z[0] = min extent of the Contour and z[1] = max extent of the Contour in the z
     *            dimension
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

        for (i = 0; i < size(); i++) {
            xx = ((Point3Df) (elementAt(i))).x;
            yy = ((Point3Df) (elementAt(i))).y;
            zz = ((Point3Df) (elementAt(i))).z;

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
     * Gets the center of mass of the contour.
     *
     * @return  returns the center of mass
     */
    public Point3Df getCenterOfMass() {
        int nPts = 0;
        int x, y;
        contains(0, 0, true);
        getBounds(xBounds, yBounds, zBounds);

        int xbs = (int) xBounds[0];
        int xbe = (int) xBounds[1];
        int ybs = (int) yBounds[0];
        int ybe = (int) yBounds[1];
        double sumX = 0.0;
        double sumY = 0.0;

        for (y = ybs; y <= ybe; y++) {

            for (x = xbs; x <= xbe; x++) {

                if (contains(x, y, false)) {
                    nPts++;
                    sumX += x;
                    sumY += y;
                }
            }
        }

        cMassPt.x = MipavMath.round(sumX / nPts);
        cMassPt.y = MipavMath.round(sumY / nPts);

        if (this.size() > 0) {
            cMassPt.z = ((Point3Df) (elementAt(0))).z;
        } else {
            System.err.println("Why is size == 0? weird");
            System.err.println("Here's the name: " + name + ", and label: " + label);
            System.err.println(this.toString());
        }

        return cMassPt;
    }

    /**
     * Accessor that returns the number of points used in the most recent intensity calculation of this contour.
     *
     * @return  the number of points used in the most recent intensity calculation
     */
    public int getLastNumPixels() {
        return numPixels;
    }

    /**
     * Calculates the distance as a summation of distances from point to point.
     *
     * @param   resolutions  resolutions of the pixels ([0] = x resolution; [1] = y resolution)
     *
     * @return  the distance
     */
    public double getLengthPtToPt(float[] resolutions) {
        int i, end;
        double sum = 0;
        float x0, x1, y0, y1;
        end = size() - 1;

        for (i = 0; i < end; i++) {
            x0 = ((Point3Df) (elementAt(i))).x;
            y0 = ((Point3Df) (elementAt(i))).y;

            // z0 = ((Point3Df)(elementAt(i))).z;
            x1 = ((Point3Df) (elementAt(i + 1))).x;
            y1 = ((Point3Df) (elementAt(i + 1))).y;

            // z1 = ((Point3Df)(elementAt(i+1))).z;
            sum += MipavMath.length(x0, y0, x1, y1, resolutions);
        }

        if (closed == true) {
            x0 = ((Point3Df) (elementAt(0))).x;
            y0 = ((Point3Df) (elementAt(0))).y;

            // z0 = ((Point3Df)(elementAt(0))).z;
            x1 = ((Point3Df) (elementAt(size() - 1))).x;
            y1 = ((Point3Df) (elementAt(size() - 1))).y;

            // z1 = ((Point3Df)(elementAt(size()-1))).z;
            sum += MipavMath.length(x0, y0, x1, y1, resolutions);
        }

        return sum;
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
        int nDims = dim;
        int direction;
        float temp;
        float[] origin = new float[3];

        for (int i = 0; i < nDims; i++) {
            direction = 1;

            switch (fileInfo.getAxisOrientation()[i]) {

                case FileInfoBase.ORI_L2R_TYPE:
                    direction = -1;

                case FileInfoBase.ORI_R2L_TYPE:
                    temp = originX + (direction * resols[0] * xBounds[0]);
                    origin[0] = temp;
                    break;

                case FileInfoBase.ORI_S2I_TYPE:
                    direction = -1;

                case FileInfoBase.ORI_I2S_TYPE:
                    temp = originY + (direction * resols[1] * yBounds[0]);
                    origin[1] = temp;
                    break;

                case FileInfoBase.ORI_P2A_TYPE:
                    direction = -1;

                case FileInfoBase.ORI_A2P_TYPE:
                    temp = originX + (direction * resols[0] * xBounds[0]);
                    origin[2] = temp;
                    break;
            }
        }

        for (int i = 0; i < nDims; i++) {

            switch (fileInfo.getAxisOrientation()[i]) {

                case FileInfoBase.ORI_A2P_TYPE:
                    origin[0] = origin[2];
            }
        }

        return origin;
    }

    /**
     * Gets position/intensity along the boundary of this contour VOI.
     *
     * @param   position     array that is filled with all x,y coordinates
     * @param   intensity    the corresponding intensities along the line
     * @param   imageBuffer  image array in which VOIs and lines are found
     * @param   xDim         x-Dimension of image
     *
     * @return  the number of points in the position and intensity array that hava valid data.
     */
    public int getPositionAndIntensity(Vector3Df[] position, float[] intensity, float[] imageBuffer, int xDim) {
        int i, j, end, pt;
        int index, indexX = 0, indexY = 0;
        double myY, myX, yInc, xInc;
        double x0, x1, y0, y1;
        double distance;
        int len;
        end = size() - 1;
        pt = 0;
        i = 0;
        len = 0;

        for (j = 0; j < end; j++) {
            x0 = ((Point3Df) (elementAt(j))).x;
            y0 = ((Point3Df) (elementAt(j))).y;
            x1 = ((Point3Df) (elementAt(j + 1))).x;
            y1 = ((Point3Df) (elementAt(j + 1))).y;
            distance = Math.sqrt(((x1 - x0) * (x1 - x0)) + ((y1 - y0) * (y1 - y0)));
            myX = x0;
            myY = y0;
            xInc = (x1 - x0) / (distance * 2);
            yInc = (y1 - y0) / (distance * 2);
            len = (int) (2 * distance);

            for (i = 0; i < len; i++) {

                if ((indexX != MipavMath.round(myX)) || (indexY != Math.round(myY))) {
                    indexY = (int) MipavMath.round(myY);
                    indexX = (int) MipavMath.round(myX);
                    position[pt].x = indexX;
                    position[pt].y = indexY;
                    index = (indexY * xDim) + indexX;

                    // position[pt] = index;
                    intensity[pt] = imageBuffer[index];
                    pt++;
                }

                myX = myX + xInc;
                myY = myY + yInc;
            }
        }

        if (closed == true) {
            x0 = ((Point3Df) (elementAt(size() - 1))).x;
            y0 = ((Point3Df) (elementAt(size() - 1))).y;
            x1 = ((Point3Df) (elementAt(0))).x;
            y1 = ((Point3Df) (elementAt(0))).y;
            distance = Math.sqrt(((x1 - x0) * (x1 - x0)) + ((y1 - y0) * (y1 - y0)));
            myX = x0;
            myY = y0;
            xInc = (x1 - x0) / (2 * distance);
            yInc = (y1 - y0) / (2 * distance);
            len = (int) MipavMath.round(2 * distance);

            for (i = 0; i < len; i++) {

                if ((indexX != MipavMath.round(myX)) || (indexY != Math.round(myY))) {
                    indexY = (int) MipavMath.round(myY);
                    indexX = (int) MipavMath.round(myX);
                    position[pt].x = indexX;
                    position[pt].y = indexY;
                    index = (indexY * xDim) + indexX;

                    // position[pt] = index;
                    intensity[pt] = imageBuffer[index];
                    pt++;
                }

                myX = myX + xInc;
                myY = myY + yInc;
            }
        }

        return pt;
    }

    /**
     * Gets the position/intensity along the boundary of this contour VOI.
     *
     * @param   position     array that is filled with all the indices in the form of indexY*xDim + indexX
     * @param   intensity    the corresponding intensities along the line
     * @param   imageBuffer  image array in which the VOis and lines are found
     * @param   xDim         x-Dimension of image
     *
     * @return  the number of points in the position and intensity array that hava valid data.
     */
    public int getPositionAndIntensityIndex(int[] position, float[] intensity, float[] imageBuffer, int xDim) {
        int i, j, end, pt;
        int index, indexX = 0, indexY = 0;
        double myY, myX, yInc, xInc;
        double x0, x1, y0, y1;
        double distance;
        int len;
        end = size() - 1;
        pt = 0;
        i = 0;
        len = 0;

        for (j = 0; j < end; j++) {
            x0 = ((Point3Df) (elementAt(j))).x;
            y0 = ((Point3Df) (elementAt(j))).y;
            x1 = ((Point3Df) (elementAt(j + 1))).x;
            y1 = ((Point3Df) (elementAt(j + 1))).y;
            distance = Math.sqrt(((x1 - x0) * (x1 - x0)) + ((y1 - y0) * (y1 - y0)));
            myX = x0;
            myY = y0;
            xInc = (x1 - x0) / (2 * distance);
            yInc = (y1 - y0) / (2 * distance);
            len = (int) (2 * distance);

            for (i = 0; i < len; i++) {

                if ((indexX != MipavMath.round(myX)) || (indexY != Math.round(myY))) {
                    indexY = (int) MipavMath.round(myY);
                    indexX = (int) MipavMath.round(myX);
                    index = (indexY * xDim) + indexX;
                    position[pt] = index;
                    intensity[pt] = imageBuffer[index];
                    pt++;
                }

                myX = myX + xInc;
                myY = myY + yInc;
            }
        }

        if (closed == true) {
            x0 = ((Point3Df) (elementAt(size() - 1))).x;
            y0 = ((Point3Df) (elementAt(size() - 1))).y;
            x1 = ((Point3Df) (elementAt(0))).x;
            y1 = ((Point3Df) (elementAt(0))).y;
            distance = Math.sqrt(((x1 - x0) * (x1 - x0)) + ((y1 - y0) * (y1 - y0)));
            myX = x0;
            myY = y0;
            xInc = (x1 - x0) / (2 * distance);
            yInc = (y1 - y0) / (2 * distance);
            len = (int) MipavMath.round(distance);

            for (i = 0; i < len; i++) {

                if ((indexX != MipavMath.round(myX)) || (indexY != Math.round(myY))) {
                    indexY = (int) MipavMath.round(myY);
                    indexX = (int) MipavMath.round(myX);
                    index = (indexY * xDim) + indexX;
                    position[pt] = index;
                    intensity[pt] = imageBuffer[index];
                    pt++;
                }

                myX = myX + xInc;
                myY = myY + yInc;
            }
        }

        return pt;
    }

    /**
     * Import points into the contour.
     *
     * @param  x  array of x points
     * @param  y  array of y points
     * @param  z  array of z points
     * @param  n  number of points in the array
     */
    public void importArrays(int[] x, int[] y, int[] z, int n) {
        int i;

        try {
            this.removeAllElements();

            for (i = 0; i < n; i++) {
                this.addElement(new Point3Df(x[i], y[i], z[i]));
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }
    }

    /**
     * Import points into the contour.
     *
     * @param  x  array of x points
     * @param  y  array of y points
     * @param  z  array of z points
     * @param  n  number of points in the array
     */
    public void importArrays(float[] x, float[] y, float[] z, int n) {
        int i;

        try {
            this.removeAllElements();

            for (i = 0; i < n; i++) {
                this.addElement(new Point3Df(x[i], y[i], z[i]));
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }
    }

    /**
     * Import points into the contour.
     *
     * @param  pt  array of three dimensional points
     */
    public void importPoints(Point3Df[] pt) {
        int i;
        this.removeAllElements();

        for (i = 0; i < pt.length; i++) {
            this.addElement(pt[i]);
        }
    }

    /**
     * Saves the polygon in the contour.
     *
     * @param  gon    polygon to be saved
     * @param  slice  index to save the polygon at
     */
    public void importPolygon(Polygon gon, int slice) {
        int i;

        try {
            this.removeAllElements();

            for (i = 0; i < gon.npoints; i++) {
                this.addElement(new Point3Df(gon.xpoints[i], gon.ypoints[i], slice));
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }
    }

    /**
     * Flag used to indicate type of contour: true = closed contour (i.e. contour end points are connected) false = open
     * contour
     *
     * @return  flag whether the voi contour is closed.
     */
    public boolean isClosed() {
        return closed;
    }

    /**
     * Determines if the points are ordered in a counterClockwise manner.
     *
     * @return  true if points are counter clockwise and false if clockwise.
     */
    public boolean isCounterClockwise() {
        int length = size();
        int k = 0;
        int i;
        int prev, next;
        float vAx, vAy, vBx, vBy, crossProd;

        for (i = 1; i < length; i++) {

            if ((((Point3Df) (elementAt(i))).x <= ((Point3Df) (elementAt(k))).x) &&
                    ((((Point3Df) (elementAt(i))).x < ((Point3Df) (elementAt(k))).x) ||
                         (((Point3Df) (elementAt(i))).y < ((Point3Df) (elementAt(k))).y))) {
                k = i;
            }
        }

        // k is an index to a convex vertex.
        prev = k - 1;
        next = k + 1;

        if (prev == -1) {
            prev = length - 1;
        }

        if (next == length) {
            next = 0;
        }

        // Form two vectors
        vAx = ((Point3Df) (elementAt(k))).x - ((Point3Df) (elementAt(prev))).x;
        vAy = ((Point3Df) (elementAt(k))).y - ((Point3Df) (elementAt(prev))).y;
        vBx = ((Point3Df) (elementAt(next))).x - ((Point3Df) (elementAt(prev))).x;
        vBy = ((Point3Df) (elementAt(next))).y - ((Point3Df) (elementAt(prev))).y;

        // calc cross product 2*area if CCW or -2*area if CW
        crossProd = (vAx * vBy) - (vAy * vBx);

        if (crossProd > 0) {
            return false;
        } else {
            return true;
        }
    }

    /**
     * Make contour clockwise.
     */
    public void makeClockwise() {
        int i;
        Object obj;
        int size;

        if (isCounterClockwise() == false) {
            return;
        }

        // System.out.println("Making clockwise");
        size = size();

        for (i = 0; i < size; i++) {
            obj = elementAt(i);
            insertElementAt(obj, 0);
            removeElementAt(i + 1);
        }
    }

    /**
     * Checks to see if Contour is counter-clockwise. If not it makes the contour counter-clockwise.
     */
    public void makeCounterClockwise() {
        int i;
        Object obj;
        int size;

        if (isCounterClockwise() == true) {
            return;
        }

        size = size();

        for (i = 0; i < size; i++) {
            obj = elementAt(i);
            insertElementAt(obj, 0);
            removeElementAt(i + 1);
        }
    }

    /**
     * Do nothing.
     *
     * @param  mouseEvent  mouse event
     */
    public void mouseMoved(MouseEvent mouseEvent) { }

    /**
     * moves the active point up/down/left/right.
     *
     * @param  direction  int direction to move
     * @param  xDim       int
     * @param  yDim       int
     */
    public void moveActivePt(int direction, int xDim, int yDim) {

        if ((nearPoint == NOT_A_POINT) && (lastPoint != NOT_A_POINT) && (this.size() > lastPoint) && (lastPoint >= 0)) {
            float x = ((Point3Df) (elementAt(lastPoint))).x;
            float y = ((Point3Df) (elementAt(lastPoint))).y;

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

            if ((x >= 0) && (x < xDim) && (y >= 0) && (y < yDim)) {
                ((Point3Df) (elementAt(lastPoint))).x = x;
                ((Point3Df) (elementAt(lastPoint))).y = y;
            }
        }
    }

    /**
     * Moves a point on the contour.
     *
     * @param  x  x coordinate
     * @param  y  y coordinate
     */
    public void movePt(int x, int y) {
        replaceElement(x, y, ((Point3Df) (elementAt(nearPoint))).z);
    }

    /**
     * Tests if a point is near a vertex of the bounding box.
     *
     * @param   x            x coordinate of point with zoom factor applied
     * @param   y            y coordinate of point with zoom factor applied
     * @param   zoom         magnification of image
     * @param   resolutionX  X resolution (aspect ratio)
     * @param   resolutionY  Y resolution (aspect ratio)
     *
     * @return  returns boolean result of test
     *
     *          <p>1----5----2 | | 8 6 | | 4----7----3</p>
     */
    public int nearBoundPoint(int x, int y, float zoom, float resolutionX, float resolutionY) {
        int x0, x1, y0, y1;
        float dist, minDistance = 100000;
        x0 = MipavMath.round(xBounds[0] * zoom * resolutionX);
        x1 = MipavMath.round(xBounds[1] * zoom * resolutionX);
        y0 = MipavMath.round(yBounds[0] * zoom * resolutionY);
        y1 = MipavMath.round(yBounds[1] * zoom * resolutionY);
        nearBoundPoint = NOT_A_POINT;
        dist = (float) MipavMath.distance(x, x0, y, y0);

        if ((dist < 3) && (dist < minDistance)) {
            minDistance = dist;
            nearBoundPoint = 1;
        }

        dist = (float) MipavMath.distance(x, x1, y, y0);

        if ((dist < 3) && (dist < minDistance)) {
            minDistance = dist;
            nearBoundPoint = 2;
        }

        dist = (float) MipavMath.distance(x, x1, y, y1);

        if ((dist < 3) && (dist < minDistance)) {
            minDistance = dist;
            nearBoundPoint = 3;
        }

        dist = (float) MipavMath.distance(x, x0, y, y1);

        if ((dist < 3) && (dist < minDistance)) {
            minDistance = dist;
            nearBoundPoint = 4;
        }

        dist = (float) MipavMath.distance(x, x0 + ((x1 - x0) / 2), y, y0);

        if ((dist < 3) && (dist < minDistance)) {
            minDistance = dist;
            nearBoundPoint = 5;
        }

        dist = (float) MipavMath.distance(x, x1, y, y0 + ((y1 - y0) / 2));

        if ((dist < 3) && (dist < minDistance)) {
            minDistance = dist;
            nearBoundPoint = 6;
        }

        dist = (float) MipavMath.distance(x, x0 + ((x1 - x0) / 2), y, y1);

        if ((dist < 3) && (dist < minDistance)) {
            minDistance = dist;
            nearBoundPoint = 7;
        }

        dist = (float) MipavMath.distance(x, x0, y, y0 + ((y1 - y0) / 2));

        if ((dist < 3) && (dist < minDistance)) {
            minDistance = dist;
            nearBoundPoint = 8;
        }

        return nearBoundPoint;
    }

    /**
     * Reloads points in this array for speed purposes.
     */
    public void reloadPoints() {
        int nPts = size();

        // reloads points in this array for speed purposes
        try {

            if ((yPts == null) || (xPts == null) || (size() != xPts.length)) {
                xPts = new float[size()];
                yPts = new float[size()];
            }
            // System.err.println("contains : xPts.len = " + xPts.length + " yPts.len = " + yPts.length);
        } catch (OutOfMemoryError error) {
            System.out.println("VOIContour.contains: memory allocation error");
            System.gc();
        }

        for (int i = 0; i < nPts; i++) {
            xPts[i] = ((Point3Df) (elementAt(i))).x;
            yPts[i] = ((Point3Df) (elementAt(i))).y;
        }
    }

    /**
     * Forget about the last clicked (active) point in the contour.
     */
    public void removeActivePt() {

        if ((nearPoint == NOT_A_POINT) && (lastPoint >= 0) && (lastPoint < size())) {
            removeElementAt(lastPoint);
            lastPoint = 0;
        }
    }

    /**
     * Resets the index and flag used in the retraceContour mode. It should be called after retracing a Contour.
     */
    public void resetIndex() {
        indexRetrace = NOT_A_POINT;
        flagRetrace = false;
    }

    /**
     * Redraws contour. Must move cursor in clockwise motion to work best
     *
     * @param  zoomX        zoom for the x coordinate
     * @param  zoomY        zoom for the y coordinate
     * @param  resolutionX  resolution of the pixel in the X direction (aspect ratio)
     * @param  resolutionY  resolution of the pixel in the Y direction (aspect ratio)
     * @param  resols       array of pixel resolutions
     * @param  x1           x coordinate of new point to add
     * @param  y1           y coordinateof new point to add
     * @param  g            graphics to paint in
     */
    public void retraceContour(float zoomX, float zoomY, float resolutionX, float resolutionY, float[] resols, int x1,
                               int y1, Graphics g, int thickness) {
        Point3Df ptRetrace = null;
        double minDistance, dist;
        float x2, y2, z;
        int i, j, idx;
        int newIndex, end;
        int[] units = new int[3];

        try {

            if (g == null) {
                MipavUtil.displayError("VOIContour.retraceContour: grapics = null");

                return;
            }

            if (indexRetrace == -99) {
                makeCounterClockwise();

                if (closed == true) {
                    oldContour = new VOIContour(name, true);
                } else {
                    oldContour = new VOIContour(name, false);
                }

                // oldContour = new VOIContour();
                for (i = 0; i < size(); i++) { // Make copy and save into oldContour
                    oldContour.addElement(this.elementAt(i));
                }
            }

            // Return if trying to add the same point.
            z = ((Point3Df) (elementAt(0))).z;

            if (contains(new Point3Df(x1, y1, z))) {
                return;
            }

            g.setColor(black);
            g.setXORMode(red);
            active = false;
            units[0] = 0;
            units[1] = 0;
            units[2] = 0;
            drawSelf(zoomX, zoomY, resolutionX, resolutionY, 0, 0, resols, units, 0, g, false, thickness);

            // Find nearest point in old contour
            minDistance = 9999999;
            end = oldContour.size();

            for (i = 0; i < end; i++) {
                x2 = ((Point3Df) (oldContour.elementAt(i))).x;
                y2 = ((Point3Df) (oldContour.elementAt(i))).y;
                dist = MipavMath.distance(x1, x2, y1, y2);

                if (dist < minDistance) {
                    ptRetrace = (Point3Df) oldContour.elementAt(i);
                    minDistance = dist;
                }
            }

            // remove point(s) in contour
            newIndex = indexOf(ptRetrace);

            if ((newIndex >= 0) && (newIndex < size())) {

                if (indexRetrace >= newIndex) { // should not happen when indexRetrace == -99

                    for (j = 0; j < (indexRetrace - newIndex); j++) {
                        removeElementAt(newIndex);
                        flagRetrace = true;
                    }
                } else if ((flagRetrace == true) && (((newIndex - indexRetrace) / (float) size()) > 0.75f)) {

                    for (j = 0; j < indexRetrace; j++) {
                        removeElementAt(0);
                    }

                    int size = size();

                    for (j = 0; j < (size - newIndex); j++) {
                        ;
                        removeElementAt(size() - 1);
                    }
                }
            }

            // Find index where new point is to be added
            minDistance = 999999999;
            idx = 0;

            for (i = 0; i < size(); i++) {
                x2 = ((Point3Df) (elementAt(i))).x;
                y2 = ((Point3Df) (elementAt(i))).y;
                dist = MipavMath.distance(x1, x2, y1, y2);

                if (dist < minDistance) {
                    idx = i;
                    minDistance = dist;
                }
            }

            if (indexRetrace != NOT_A_POINT) {
                insertElementAt(new Point3Df(x1, y1, z), idx);
                indexRetrace = idx;
            } else {
                indexRetrace = idx;
            }

            units[0] = 0;
            units[1] = 0;
            units[2] = 0;
            drawSelf(zoomX, zoomY, resolutionX, resolutionY, 0, 0, resols, units, 0, g, false, thickness);
            active = true;
        } catch (OutOfMemoryError error) {
            System.gc();

            return;
        }
    }

    /**
     * Finds the second order attributes or moments.
     *
     * @param  xDim          width of input image slice
     * @param  yDim          height of input image slice
     * @param  xRes          DOCUMENT ME!
     * @param  yRes          DOCUMENT ME!
     * @param  xUnits        DOCUMENT ME!
     * @param  yUnits        DOCUMENT ME!
     * @param  pAxis         angle in degrees with major axis
     * @param  eccentricity  shape (circle = 0; line = 1)
     * @param  majorAxis     diameter of major axis
     * @param  minorAxis     diameter of minor axis Reference:
     *                       http://www.caip.rutgers.edu/~xswang/object-segment/objhtml/node4.html Object Quantification
     *                       by Simon Xin Wang
     */
    public void secondOrderAttributes(int xDim, int yDim, float xRes, float yRes, int xUnits, int yUnits, float[] pAxis,
                                      float[] eccentricity, float[] majorAxis, float[] minorAxis) {
        int nPts = 0;
        BitSet mask;

        try {
            mask = new BitSet(xDim * yDim); // bitset with a rectangular size
        } catch (OutOfMemoryError oome) {
            return;
        }

        int x, y;
        int offset; // from corner
        contains(0, 0, true);
        getBounds(xBounds, yBounds, zBounds);

        int xbs = (int) xBounds[0];
        int xbe = (int) xBounds[1];
        int ybs = (int) yBounds[0];
        int ybe = (int) yBounds[1];
        double m10 = 0.0;
        double m01 = 0.0;

        for (y = ybs; y <= ybe; y++) {
            offset = y * xDim; // a horizontal offset

            for (x = xbs; x <= xbe; x++) {

                if (contains(x, y, false)) {
                    mask.set(offset + x);
                    nPts++;

                    if (xUnits == yUnits) {
                        m10 += x * xRes;
                        m01 += y * yRes;
                    } else {
                        m10 += x;
                        m01 += y;
                    }
                }
            }
        }

        m10 = m10 / nPts;
        m01 = m01 / nPts;

        double m20 = 0.0;
        double m11 = 0.0;
        double m02 = 0.0;
        double xdiff;
        double ydiff;

        for (y = ybs; y <= ybe; y++) {
            offset = y * xDim;

            for (x = xbs; x <= xbe; x++) {

                if (mask.get(offset + x)) {

                    if (xUnits == yUnits) {
                        xdiff = (x * xRes) - m10;
                        ydiff = (y * yRes) - m01;
                    } else {
                        xdiff = x - m10;
                        ydiff = y - m01;
                    }

                    m20 += xdiff * xdiff;
                    m11 += xdiff * ydiff;
                    m02 += ydiff * ydiff;
                }
            }
        }

        m20 = (m20 / nPts);
        m11 = (m11 / nPts);
        m02 = (m02 / nPts);

        // The eigenvalues of m20 m11
        // m11 m02
        // are proportional to the square of the semiaxes
        // (m20 - e)*(m02 - e) - m11*m11 = 0;
        double root = Math.sqrt(((m20 - m02) * (m20 - m02)) + (4 * m11 * m11));
        majorAxis[0] = (float) Math.sqrt(2.0 * (m20 + m02 + root));
        minorAxis[0] = (float) Math.sqrt(2.0 * (m20 + m02 - root));

        double areaEllipse = (Math.PI / 4.0) * majorAxis[0] * minorAxis[0];
        double normFactor;

        if (xUnits == yUnits) {
            normFactor = Math.sqrt(xRes * yRes * nPts / areaEllipse);
        } else {
            normFactor = Math.sqrt(nPts / areaEllipse);
        }

        majorAxis[0] = (float) (normFactor * majorAxis[0]);
        minorAxis[0] = (float) (normFactor * minorAxis[0]);
        eccentricity[0] = (float) Math.sqrt(1.0 - ((minorAxis[0] * minorAxis[0]) / (majorAxis[0] * majorAxis[0])));

        // Jahne p. 507
        pAxis[0] = (float) ((180.0 / Math.PI) * 0.5 * Math.atan2((2.0 * m11), (m20 - m02)));
    }

    /**
     * Finds the second order attributes.
     *
     * <p>Reference: Fitzgibbon, Pilu, Fisher: Direct Least Squares Fitting of Ellipses at
     * http://www.robots.ox.ac.uk/~awf/ellipse/fitellipse.html</p>
     *
     * @param  xRes          DOCUMENT ME!
     * @param  yRes          DOCUMENT ME!
     * @param  xUnits        DOCUMENT ME!
     * @param  yUnits        DOCUMENT ME!
     * @param  pAxis         angle in degrees with major axis
     * @param  eccentricity  shape (circle = 0; line = 1)
     * @param  majorAxis     diameter of major axis
     * @param  minorAxis     diameter of minor axis
     */
    public void secondOrderAttributeslsq(float xRes, float yRes, int xUnits, int yUnits, float[] pAxis,
                                         float[] eccentricity, float[] majorAxis, float[] minorAxis) {
        System.err.println("here2");
        reloadPoints();

        int nPts = size();
        double mx = 0.0;
        double my = 0.0;
        double sx;
        double sy;
        double minX = Double.MAX_VALUE;
        double maxX = -Double.MAX_VALUE;
        double minY = Double.MAX_VALUE;
        double maxY = -Double.MAX_VALUE;
        int i, j, k;
        double[] x = new double[nPts];
        double[] y = new double[nPts];

        // Normalize data
        // It's important to scale the image coordinates to be near one before
        // running the algorithm or you'll get numerical instability
        if (xUnits == yUnits) {

            for (i = 0; i < nPts; i++) {
                mx += xPts[i] * xRes;
                my += yPts[i] * yRes;

                if ((xPts[i] * xRes) < minX) {
                    minX = xPts[i] * xRes;
                }

                if ((xPts[i] * xRes) > maxX) {
                    maxX = xPts[i] * xRes;
                }

                if ((yPts[i] * yRes) < minY) {
                    minY = yPts[i] * yRes;
                }

                if ((yPts[i] * yRes) > maxY) {
                    maxY = yPts[i] * yRes;
                }
            } // for (i = 0; i < nPts; i++)
        } // if (xUnits == yUnits)
        else {

            for (i = 0; i < nPts; i++) {
                mx += xPts[i];
                my += yPts[i];

                if (xPts[i] < minX) {
                    minX = xPts[i];
                }

                if (xPts[i] > maxX) {
                    maxX = xPts[i];
                }

                if (yPts[i] < minY) {
                    minY = yPts[i];
                }

                if (yPts[i] > maxY) {
                    maxY = yPts[i];
                }
            } // for (i = 0; i < nPts; i++)
        } // else

        mx = mx / nPts;
        my = my / nPts;
        sx = (maxX - minX) / 2.0;
        sy = (maxY - minY) / 2.0;

        if (xUnits == yUnits) {

            for (i = 0; i < nPts; i++) {
                x[i] = ((xPts[i] * xRes) - mx) / sx;
                y[i] = ((yPts[i] * yRes) - my) / sy;
            } // for (i = 0; i < nPts; i++)
        } // if (xUnits == yUnits)
        else {

            for (i = 0; i < nPts; i++) {
                x[i] = (xPts[i] - mx) / sx;
                y[i] = (yPts[i] - my) / sy;
            } // for (i = 0; i < nPts; i++)
        }

        // Build design array
        double[][] D = new double[nPts][6];

        for (i = 0; i < nPts; i++) {
            D[i][0] = x[i] * x[i];
            D[i][1] = x[i] * y[i];
            D[i][2] = y[i] * y[i];
            D[i][3] = x[i];
            D[i][4] = y[i];
            D[i][5] = 1.0;
        } // for (i = 0; i < nPts; i++)

        double[][] Dtrans = new double[6][nPts];

        for (i = 0; i < nPts; i++) {

            for (j = 0; j < 6; j++) {
                Dtrans[j][i] = D[i][j];
            }
        }

        // Build scatter array
        double[][] S = new double[6][6];

        for (i = 0; i < 6; i++) {

            for (j = 0; j < 6; j++) {

                for (k = 0; k < nPts; k++) {
                    S[i][j] += Dtrans[i][k] * D[k][j];
                }
            }
        }

        double[][] tmpA = new double[3][3];

        for (i = 0; i < 3; i++) {

            for (j = 0; j < 3; j++) {
                tmpA[i][j] = S[i][j];
            }
        }

        double[][] tmpB = new double[3][3];

        for (i = 0; i < 3; i++) {

            for (j = 0; j < 3; j++) {
                tmpB[i][j] = S[i][j + 3];
            }
        }

        double[][] tmpC = new double[3][3];

        for (i = 0; i < 3; i++) {

            for (j = 0; j < 3; j++) {
                tmpC[i][j] = S[i + 3][j + 3];
            }
        }

        double[][] tmpD = new double[3][3];
        tmpD[0][2] = -2;
        tmpD[1][1] = 1;
        tmpD[2][0] = -2;

        Matrix matC = new Matrix(tmpC);
        double[][] invC = matC.inverse().getArray();
        double[][] tmpBTrans = new double[3][3];

        for (i = 0; i < 3; i++) {

            for (j = 0; j < 3; j++) {
                tmpBTrans[j][i] = tmpB[i][j];
            }
        }

        double[][] tmpE = new double[3][3];

        for (i = 0; i < 3; i++) {

            for (j = 0; j < 3; j++) {

                for (k = 0; k < 3; k++) {
                    tmpE[i][j] += invC[i][k] * tmpBTrans[k][j];
                }
            }
        }

        double[][] BE = new double[3][3];

        for (i = 0; i < 3; i++) {

            for (j = 0; j < 3; j++) {

                for (k = 0; k < 3; k++) {
                    BE[i][j] += tmpB[i][k] * tmpE[k][j];
                }
            }
        }

        double[][] ABE = new double[3][3];

        for (i = 0; i < 3; i++) {

            for (j = 0; j < 3; j++) {
                ABE[i][j] = tmpA[i][j] - BE[i][j];
            }
        }

        Matrix matD = new Matrix(tmpD);
        double[][] invD = matD.inverse().getArray();
        double[][] DABE = new double[3][3];

        for (i = 0; i < 3; i++) {

            for (j = 0; j < 3; j++) {

                for (k = 0; k < 3; k++) {
                    DABE[i][j] += invD[i][k] * ABE[k][j];
                }
            }
        }

        Matrix argMat = new Matrix(DABE);
        EigenvalueDecomposition eig;
        double[] eigenvalue;
        double[][] eigenvector;
        double temp;
        double[] tempCol = new double[3];
        int m, n, index;
        eig = new EigenvalueDecomposition(argMat);
        eigenvalue = eig.getRealEigenvalues();

        // In EigenvalueDecomposition the columns represent the
        // eigenvectors
        eigenvector = eig.getV().getArray();

        // Arrange the eigenvalues and corresponding eigenvectors
        // in descending order so that e0 >= e1 >= e2
        for (m = 0; m < 3; m++) {
            index = m;

            for (n = m + 1; n < 3; n++) {

                if (eigenvalue[n] > eigenvalue[index]) {
                    index = n;
                }
            } // for (m = m+1; n < 3; n++)

            if (index != m) {
                temp = eigenvalue[m];
                eigenvalue[m] = eigenvalue[index];
                eigenvalue[index] = temp;

                for (n = 0; n < 3; n++) {
                    tempCol[n] = eigenvector[n][m];
                    eigenvector[n][m] = eigenvector[n][index];
                    eigenvector[n][index] = tempCol[n];
                }
            } // if (index != m)
        } // for (m = 0; m < 3; m++)

        // Place the eigenvector corresponding to the negative eigenvalue
        // in A
        double[] A = new double[3];

        for (i = 0; i < 3; i++) {
            A[i] = eigenvector[i][2];
        }

        // Recover the bottom half
        double[] evec_y = new double[3];

        for (i = 0; i < 3; i++) {

            for (k = 0; k < 3; k++) {
                evec_y[i] += -tmpE[i][k] * A[k];
            }
        }

        double[] Atemp = new double[3];

        for (i = 0; i < 3; i++) {
            Atemp[i] = A[i];
        }

        A = new double[6];

        for (i = 0; i < 3; i++) {
            A[i] = Atemp[i];
            A[i + 3] = evec_y[i];
        }

        // Unnormalize
        double[] par = new double[6];
        par[0] = A[0] * sy * sy;
        par[1] = A[1] * sx * sy;
        par[2] = A[2] * sx * sx;
        par[3] = (-2 * A[0] * sy * sy * mx) - (A[1] * sx * sy * my) + (A[3] * sx * sy * sy);
        par[4] = (-A[1] * sx * sy * mx) - (2 * A[2] * sx * sx * my) + (A[4] * sx * sx * sy);
        par[5] = (A[0] * sy * sy * mx * mx) + (A[1] * sx * sy * mx * my) + (A[2] * sx * sx * my * my) -
                 (A[3] * sx * sy * sy * mx) - (A[4] * sx * sx * sy * my) + (A[5] * sx * sx * sy * sy);

        // Convert to geometric radii, and centers
        double thetarad = 0.5 * Math.atan2(par[1], (par[0] - par[2]));
        pAxis[0] = (float) ((180.0 / Math.PI) * thetarad);

        double cost = Math.cos(thetarad);
        double sint = Math.sin(thetarad);
        double sin_squared = sint * sint;
        double cos_squared = cost * cost;
        double cos_sin = sint * cost;
        double Ao = par[5];
        double Au = (par[3] * cost) + (par[4] * sint);
        double Av = (-par[3] * sint) + (par[4] * cost);
        double Auu = (par[0] * cos_squared) + (par[2] * sin_squared) + (par[1] * cos_sin);
        double Avv = (par[0] * sin_squared) + (par[2] * cos_squared) - (par[1] * cos_sin);
        double tuCentre = -Au / (2.0 * Auu);
        double tvCentre = -Av / (2.0 * Avv);
        double wCentre = Ao - (Auu * tuCentre * tuCentre) - (Avv * tvCentre * tvCentre);

        // double uCentre = tuCentre * cost - tvCentre * sint;
        // double vCentre = tuCentre * sint + tvCentre * cost;
        double Ru = -wCentre / Auu;
        double Rv = -wCentre / Avv;
        Ru = Math.sqrt(Math.abs(Ru));
        Rv = Math.sqrt(Math.abs(Rv));

        if (Ru >= Rv) {
            majorAxis[0] = (float) (2.0 * Ru);
            minorAxis[0] = (float) (2.0 * Rv);
        } else {
            majorAxis[0] = (float) (2.0 * Rv);
            minorAxis[0] = (float) (2.0 * Ru);

            if (pAxis[0] <= 0.0f) {
                pAxis[0] = pAxis[0] + 90.0f;
            } else {
                pAxis[0] = pAxis[0] - 90.0f;
            }
        }
    }

    /**
     * Sets the bounds of entire VOI not just Contour.
     *
     * @param  x  x bounds of entire VOI
     * @param  y  y bounds of entire VOI
     * @param  z  z bounds of entire VOI
     */
    public void setBounds(float[] x, float[] y, float[] z) {
        xBounds = x;
        yBounds = y;
        zBounds = z;
    }

    /**
     * Flag used to indicate type of contour: true = closed contour (i.e. contour end points are connected) false = open
     * contour
     *
     * @param  flag  indicates if the contour is open or closed.
     */
    public void setClosed(boolean flag) {
        closed = flag;
    }

    /**
     * Transforms self.
     *
     * @param  tMatrix  transformation matrix
     * @param  doRound  if true round point coordinates to integers
     */
    public void transformContour(TransMatrix tMatrix, boolean doRound) {
        int i;
        Point3Df point = null;

        try {
            point = new Point3Df();
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }

        for (i = 0; i < size(); i++) {
            tMatrix.transform((Point3Df) (elementAt(i)), point);

            if (doRound) {
                setElementAt(new Point3Df(MipavMath.round(point.x), MipavMath.round(point.y), MipavMath.round(point.z)),
                             i);
            } else {
                setElementAt(new Point3Df(point.x, point.y, point.z), i);
            }
        }
    }

    /**
     * Tranforms self using a transformation matrix (Affine).
     *
     * @param  thetaX  rotation in x
     * @param  thetaY  rotation in y
     * @param  thetaZ  rotation in z
     * @param  tX      translation in x
     * @param  tY      translation in y
     * @param  tZ      translation in z
     * @param  scaleX  zoom in x
     * @param  scaleY  zoom in y
     * @param  scaleZ  zoom in z
     */
    public void transformContour(float thetaX, float thetaY, float thetaZ, float tX, float tY, float tZ, float scaleX,
                                 float scaleY, float scaleZ) {
        int i;
        Point3Df point = null;
        TransMatrix tMatrix = null;

        try {
            point = new Point3Df();
            tMatrix = new TransMatrix(4);
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }

        getCenterOfMass();

        // construct transMatrix object
        // tMatrix.translate((cMassPt.x+tX)*scaleX, (cMassPt.y+tY)*scaleY, (cMassPt.z+tZ)*scaleZ);
        tMatrix.setTranslate((cMassPt.x + tX), (cMassPt.y + tY), (cMassPt.z + tZ));
        tMatrix.setRotate(thetaX, thetaY, thetaZ, TransMatrix.DEGREES);
        tMatrix.setZoom(scaleX, scaleY, scaleZ);
        tMatrix.setTranslate(-cMassPt.x, -cMassPt.y, -cMassPt.z);

        for (i = 0; i < size(); i++) {
            tMatrix.transform((Point3Df) (elementAt(i)), point);
            setElementAt(new Point3Df(point.x, point.y, point.z), i);
        }
    }

    /**
     * Translates a VOI by some amount in x and y.
     *
     * @param  xT  translate this amount in the x direction
     * @param  yT  translate this amount in the y direction
     */
    public void translate(float xT, float yT) {
        int i;

        for (i = 0; i < size(); i++) {
            ((Point3Df) (elementAt(i))).x += xT;
            ((Point3Df) (elementAt(i))).y += yT;
        }
    }

    /**
     * Removes collinear points (or near collinear) in the contour. If the perpendicular distance from the middle point
     * to the line defined by the 1st and 3rd point is small than the middle point is removed.
     *
     * @param  constraint  factor that controls the number of points removed. A larger constraint removes more points
     *                     0.50 typical - removes most "almost/and collinear" points 0.10 - removes only "collinear"
     *                     points
     * @param  tFlag       if true, trim adjacient points
     */
    public void trimPoints(double constraint, boolean tFlag) {
        int i;
        float ax, ay, bx, by, cx, cy;
        boolean flag = true;
        int end;

        if (size() <= 5) {
            return;
        }

        if (tFlag == true) {
            end = size();

            for (i = 0; i < (end - 1); i++) {
                ax = ((Point3Df) (elementAt(i))).x;
                ay = ((Point3Df) (elementAt(i))).y;
                bx = ((Point3Df) (elementAt(i + 1))).x;
                by = ((Point3Df) (elementAt(i + 1))).y;

                if (MipavMath.distance(ax, bx, ay, by) <= 1.5) {
                    removeElementAt(i + 1); // remove adjacient points
                    end = size();
                    i = i - 1;
                }

                if (size() <= 5) {
                    return;
                }
            }
        }

        while (flag == true) {
            flag = false;
            end = size();

            if (size() <= 5) {
                return;
            }

            for (i = 0; i < (end - 2); i++) {
                ax = ((Point3Df) (elementAt(i))).x;
                ay = ((Point3Df) (elementAt(i))).y;
                bx = ((Point3Df) (elementAt(i + 1))).x;
                by = ((Point3Df) (elementAt(i + 1))).y;
                cx = ((Point3Df) (elementAt(i + 2))).x;
                cy = ((Point3Df) (elementAt(i + 2))).y;

                if (testDistance(MipavMath.round(bx), MipavMath.round(ax), MipavMath.round(cx), MipavMath.round(by),
                                     MipavMath.round(ay), MipavMath.round(cy), constraint)) {
                    removeElementAt(i + 1);
                    end = size();
                    i = i - 1;
                    flag = true;
                }
            }
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
    private float areaTwice(float ptAx, float ptAy, float ptBx, float ptBy, float ptCx, float ptCy) {
        return (((ptAx - ptCx) * (ptBy - ptCy)) - ((ptAy - ptCy) * (ptBx - ptCx)));
    }

    /**
     * Takes a scale and returns a scaled polygon.
     *
     * @param   zoomX        scale for the x coordinate
     * @param   zoomY        scale for the y coordinate
     * @param   resolutionX  resolution of the pixel in the X direction (aspect ratio)
     * @param   resolutionY  resolution of the pixel in the Y direction (aspect ratio)
     *
     * @return  returns scaled polygon
     */
    private Polygon scalePolygon(float zoomX, float zoomY, float resolutionX, float resolutionY) {
        int i;
        int x;
        int y;
        Polygon scaledGon = null;

        try {
            scaledGon = new Polygon();
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }

        for (i = 0; i < size(); i++) {
            x = (int) ((((Point3Df) (elementAt(i))).x * zoomX * resolutionX) + 0.5);
            y = (int) ((((Point3Df) (elementAt(i))).y * zoomY * resolutionY) + 0.5);
            scaledGon.addPoint(x, y);
        }

        return scaledGon;
    }
}
