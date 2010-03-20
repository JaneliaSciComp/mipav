package gov.nih.mipav.model.structures;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.*;

import gov.nih.mipav.model.file.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;


/**
 * Base which holds the functions common to both Contour, Line and Point type VOI. Abstract class.
 *
 * @version  0.1 Jul 22, 1998
 * @author   Matthew McAuliffe
 * @see      VOILine
 * @see      VOIContour
 *
 *           <p>$Logfile: /mipav/src/gov/nih/mipav/model/structures/VOIBase.java $ $Revision: 56 $ $Date: 2/24/06 3:34p
 *           $</p>
 */
public abstract class VOIBase extends Vector<Vector3f> {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -3872480526036224600L;

    /** DOCUMENT ME! */
    public static final int XY = 0;

    /** DOCUMENT ME! */
    public static final int ZY = 1;

    /** DOCUMENT ME! */
    public static final int XZ = 2;

    /** DOCUMENT ME! */
    public static final int NA = 3;

    /**
     * Used in places which usually remember an index into a point vector. Indicates that the index should not be used.
     */
    public static final int NOT_A_POINT = -99;

    /** The up arrow key. */
    protected static final int UP = KeyEvent.VK_UP;

    /** The left arrow key. */
    protected static final int LEFT = KeyEvent.VK_LEFT;

    /** The down arrow key. */
    protected static final int DOWN = KeyEvent.VK_DOWN;

    /** The right arrow key. */
    protected static final int RIGHT = KeyEvent.VK_RIGHT;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /**
     * Flag indicating whether or not a VOI is active (selected). If the VOI is selected then the flag is true else it
     * is false
     */
    protected boolean active;

    /** General use color (black) used in VOI. */
    protected Color black = new Color(0, 0, 0);

    /** Stores the geometric center of the contour of a VOI. */
    protected Vector3f gcPt = new Vector3f(0, 0, 0);
    
    /** Stores the black and white center of mass of the contour of a VOI. */
    protected Vector3f cenMassPt = new Vector3f(0, 0, 0);
    
    /** Stores the red center of mass of the contour of a VOI. */
    protected Vector3f cenMassPtR = new Vector3f(0, 0, 0);
    
    /** Stores the green center of mass of the contour of a VOI. */
    protected Vector3f cenMassPtG = new Vector3f(0, 0, 0);
    
    /** Stores the blue center of mass of the contour of a VOI. */
    protected Vector3f cenMassPtB = new Vector3f(0, 0, 0);

    /** DOCUMENT ME! */
    protected boolean doName = false;

    /** Flag that indicates if the VOI is movable. */
    protected boolean fixed;

    /** Label (Name) for this member of the VOI. */
    protected String label = null;

    /** Keeps track of the last near point value (for keyboard moving of individual pts. */
    protected int lastPoint = 0;

    /** VOI Name (for group of contours, not specific to this base). */
    protected String name = null;

    /** Flag used to indicate if the cursor is near a point of the VOI member. */
    protected int nearPoint = NOT_A_POINT;

    /** General use color (red) used in VOI. */
    protected Color red = new Color(255, 0, 0);

    /** General use color (white) used in VOI. */
    protected Color white = new Color(255, 255, 255);
    
    /** Reference to the containing VOI object. */
    protected VOI voiGroup;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Initializes the start and the amount to increment the vector by.,
     */
    public VOIBase() {
        super(20, 10);
        active = false;
        fixed = false;
    }
    

    /**
     * Copies the input VOIBase into a new VOIBase object.
     * @param kBase
     */
    public VOIBase( VOIBase kBase ) {
        super(20, 10);
        this.active = kBase.active;
        this.gcPt.Copy(kBase.gcPt);
        this.cenMassPt.Copy(kBase.cenMassPt);
        this.cenMassPtR.Copy(kBase.cenMassPtR);
        this.cenMassPtG.Copy(kBase.cenMassPtG);
        this.cenMassPtB.Copy(kBase.cenMassPtB);
        this.doName = kBase.doName;
        this.fixed = kBase.fixed;
        if ( kBase.label != null )
        {
            this.label = new String(kBase.label);
        }
        this.lastPoint = kBase.lastPoint;
        if ( kBase.name != null )
        {
            this.name = new String(kBase.name);
        }
        this.nearPoint = kBase.nearPoint;
        this.voiGroup = kBase.voiGroup;

        for ( int i = 0; i < kBase.size(); i++ )
        {
            add( kBase.get(i) );
        } 
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * contains is abstract method, each subclass is required to denote what points are contained by the region it
     * encloses or is near to.
     *
     * @param   _x           x-coordinate of the point in question
     * @param   _y           y-coordinate of the point in question
     * @param   forceReload  if true export points from VOI structure
     *
     * @return  true if point is within the contour
     */
    public abstract boolean contains(int _x, int _y, boolean forceReload);

    /**
     * Abstract method: cycles through the active points on the curve.
     *
     * @param  keyCode  int arrow key (up/down/left/right)
     */
    public abstract void cycleActivePt(int keyCode);

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
     */
    public abstract void drawSelf(float zoomX, float zoomY, float resolutionX, float resolutionY, float originX,
                                  float originY, float[] resols, int[] unitsOfMeasure, int orientation, Graphics g,
                                  boolean boundingBox, int thickness);

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
    public abstract void drawSelf(float zoomX, float zoomY, float resolutionX, float resolutionY, float originX,
                                  float originY, float[] resols, int[] unitsOfMeasure, int orientation, Graphics g,
                                  boolean boundingBox, FileInfoBase fileInfo, int dim, int thickness);

    /**
     * Abstract method: gets the Vector3f of the active point.
     *
     * @return  Vector3f the active point's Vector3f
     */
    public abstract Vector3f getActivePt();

    /**
     * Returns the point on the curve nearest the cursor, calculated by nearLine, etc.
     * @return point on the curve nearest the cursor.
     */
    public int getNearPt()
    {
        return nearPoint;
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
    public abstract float[] getOrigin(FileInfoBase fileInfo, int dim, float originX, float originY, float[] resols);

    /**
     * Abstract method: import points into curve.
     *
     * @param  x  array of x points
     * @param  y  array of y points
     * @param  z  array of z points
     * @param  n  number of points in the array
     */
    public abstract void importArrays(int[] x, int[] y, int[] z, int n);

    /**
     * Abstract method: import points into curve.
     *
     * @param  x  array of x points
     * @param  y  array of y points
     * @param  z  array of z points
     * @param  n  number of points in the array
     */
    public abstract void importArrays(float[] x, float[] y, float[] z, int n);

    /**
     * Abstract method: import points into curve.
     *
     * @param  pt  array of points
     */
    public abstract void importPoints(Vector3f[] pt);

    /**
     * Abstract method: moves the active point.
     *
     * @param  keyCode  arrow key (up/down/left/right)
     * @param  xDim     the image's x dimension
     * @param  yDim     the image's y dimension
     */
    public abstract void moveActivePt(int keyCode, int xDim, int yDim);

    /**
     * Tests the distance for closeness; finds the length of the normal from (x,y) to the line (x1,y1) (x2,y2). Returns
     * true if the distance is shorter than tol.
     *
     * @param   x    x coordinate of point to be tested
     * @param   x1   x coordinate of first point in line
     * @param   x2   x coordinate of second point in line
     * @param   y    y coordinate of point to be tested
     * @param   y1   y coordinate of first point in line
     * @param   y2   y coordinate of second point in line
     * @param   tol  distance to test against
     *
     * @return  true if the distance is shorter than tol.
     */
    public static synchronized boolean testDistance(int x, int x1, int x2, int y, int y1, int y2, double tol) {

        // double hVx, hVy, aVx, aVy;
        double lenH, lenH2, lenA, lenO;
        lenH = MipavMath.distance(x1, x, y1, y);

        if (lenH <= 0) {
            return false;
        }

        lenH2 = MipavMath.distance(x, x2, y, y2);
        lenA = MipavMath.distance(x1, x2, y1, y2);

        if (lenA <= 0) {
            return false;
        }

        // hVx = x - x1;
        // hVy = y - y1;
        // aVx = x2 - x1;
        // aVy = y2 - y1;
        // theta = Math.acos((hVx*aVx + hVy*aVy)/(lenH*lenA));
        // lenO = lenH * Math.sin(theta);// * (lenH+lenH2+lenA);
        // The above reduces to:
        lenO = Math.abs(((y1 - y2) * x) + ((x2 - x1) * y) + ((x1 * y2) - (y1 * x2))) / lenA;

        if ((lenO < tol) && (lenH < lenA) && (lenH2 < lenA)) {
            return true;
        }

        return false;
    }

    /**
     * Adds a point to the curve.
     *
     * @param  x  x coordinate of point
     * @param  y  y coordinate of point
     * @param  z  z coordinate of point
     */
    public void addElement(int x, int y, int z) {
        addElement(new Vector3f(x, y, z));
    }

    /**
     * Adds a point to the curve.
     *
     * @param  x  x coordinate of point
     * @param  y  y coordinate of point
     * @param  z  z coordinate of point
     */
    public void addElement(float x, float y, float z) {
        addElement(new Vector3f(x, y, z));
    }

    /**
     * contains is abstract method, each subclass is required to denote what points are contained by the region it
     * encloses or is near to.
     *
     * @param   _x           x-coordinate of the point in question
     * @param   _y           y-coordinate of the point in question
     * @param   forceReload  if true export points from VOI structure
     *
     * @return  true if point is within the contour
     */
    public boolean containsComponent(int _x, int _y, boolean forceReload) {
        return false;
    }

    /**
     * Exports the arrays of the points of the curve in int array format.
     *
     * @param  x  array of x coordinates
     * @param  y  array of y coordinates
     * @param  z  array of z coordinates
     */
    public void exportArrays(int[] x, int[] y, int[] z) {
        int i;

        if ((x == null) || (y == null)) {
            return;
        }

        if ((x.length != y.length) || (x.length < size())) {
            return;
        }

        for (i = 0; i < size(); i++) {
            x[i] = MipavMath.round(((Vector3f) (elementAt(i))).X);
            y[i] = MipavMath.round(((Vector3f) (elementAt(i))).Y);
            z[i] = MipavMath.round(((Vector3f) (elementAt(i))).Z);
        }
    }

    /**
     * Exports the float arrays of the points of the curve.
     *
     * @param  x  array of x coordinates
     * @param  y  array of y coordinates
     * @param  z  array of z coordinates
     */
    public void exportArrays(float[] x, float[] y, float[] z) {
        int i;

        if ((x == null) || (y == null)) {
            return;
        }

        if ((x.length != y.length) || (x.length < size())) {
            return;
        }

        for (i = 0; i < this.size(); i++) {
            x[i] = ((Vector3f) (elementAt(i))).X;
            y[i] = ((Vector3f) (elementAt(i))).Y;
            z[i] = ((Vector3f) (elementAt(i))).Z;
        }
    }


    /**
     * Returns the VOI object that contains this contour.
     * @return the VOI object that contains this contour.
     */
    public VOI getGroup()
    {
        return voiGroup;
    }
    
    /**
     * Gets the label of the VOI.
     *
     * @return  label as a String
     */
    public String getLabel() {
        return label;
    }

    /**
     * Gets the VOI's name... need this here to display in drawSelf
     *
     * @return  String
     */
    public String getName() {
        return this.name;
    }

    /**
     * Inserts a point on the curve.
     *
     * @param  x  x coordinate of point
     * @param  y  y coordinate of point
     * @param  z  z coordinate of point
     */
    public void insertElement(int x, int y, int z) {

        // System.err.println("insert element");
        if ((nearPoint >= 0) && (nearPoint < size())) {

            if (this instanceof VOICardiology) {
                ((VOICardiology) this).insertCardioPoint(x, y, z);
            } else {
                insertElementAt(new Vector3f(x, y, z), nearPoint + 1);
                nearPoint++;
                nearPoint = NOT_A_POINT;
                lastPoint = 0;
            }
        }
    }

    /**
     * *********************************************************************. * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    /**
     * ***************************** Accessors *****************************.
     *
     * @return  DOCUMENT ME!
     */
    /**
     * *********************************************************************. * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    /**
     * Accessor to flag that indicates if an VOI is active.
     *
     * @return  flag indicating if an VOI is active
     */
    public boolean isActive() {
        return active;
    }

    /**
     * Accessor to flag that indicates if an VOI is fixed (movable).
     *
     * @return  flag indicating if an VOI is fixed
     */
    public boolean isFixed() {
        return fixed;
    }

    /**
     * Tests if a point is near the curve.
     *
     * @param   x  x coordinate of point
     * @param   y  y coordinate of point
     *
     * @return  returns boolean result of test
     */
    public synchronized boolean nearLine(int x, int y) {
        int i;
        int x1, y1, x2, y2;

        // System.err.println("near line 2");
        for (i = 0; i < (size() - 1); i++) {
            x1 = MipavMath.round(((Vector3f) (elementAt(i))).X);
            y1 = MipavMath.round(((Vector3f) (elementAt(i))).Y);
            x2 = MipavMath.round(((Vector3f) (elementAt(i + 1))).X);
            y2 = MipavMath.round(((Vector3f) (elementAt(i + 1))).Y);

            if (testDistance(x, x1, x2, y, y1, y2, 5)) {
                nearPoint = i;

                return true;
            }
        }

        x1 = MipavMath.round(((Vector3f) (elementAt(0))).X);
        y1 = MipavMath.round(((Vector3f) (elementAt(0))).Y);
        x2 = MipavMath.round(((Vector3f) (elementAt(size() - 1))).X);
        y2 = MipavMath.round(((Vector3f) (elementAt(size() - 1))).Y);

        if (testDistance(x, x1, x2, y, y1, y2, 3)) {
            nearPoint = i;

            return true;
        }

        nearPoint = NOT_A_POINT;

        return false;
    }

    /**
     * Tests if a point is near the curve.
     *
     * @param   x    x coordinate of point
     * @param   y    y coordinate of point
     * @param   tol  tolerance indicating the capture range to the line the point.
     *
     * @return  returns boolean result of test
     */
    public synchronized boolean nearLine(int x, int y, int tol) {

        int i;
        int x1, y1, x2, y2;

        // System.err.println("near line");
        for (i = 0; i < (size() - 1); i++) {
            x1 = MipavMath.round(((Vector3f) (elementAt(i))).X);
            y1 = MipavMath.round(((Vector3f) (elementAt(i))).Y);
            x2 = MipavMath.round(((Vector3f) (elementAt(i + 1))).X);
            y2 = MipavMath.round(((Vector3f) (elementAt(i + 1))).Y);

            if (testDistance(x, x1, x2, y, y1, y2, tol)) {
                nearPoint = i;

                return true;
            }
        }

        x1 = MipavMath.round(((Vector3f) (elementAt(0))).X);
        y1 = MipavMath.round(((Vector3f) (elementAt(0))).Y);
        x2 = MipavMath.round(((Vector3f) (elementAt(size() - 1))).X);
        y2 = MipavMath.round(((Vector3f) (elementAt(size() - 1))).Y);

        if (testDistance(x, x1, x2, y, y1, y2, tol)) {
            nearPoint = i;

            return true;
        }

        nearPoint = NOT_A_POINT;

        return false;
    }

    /**
     * Tests if a point is near a vertex of the curve.
     *
     * @param   x            x coordinate of point
     * @param   y            y coordinate of point
     * @param   zoom         magnification of image
     * @param   resolutionX  X resolution (aspect ratio)
     * @param   resolutionY  Y resolution (aspect ratio)
     *
     * @return  returns boolean result of test
     */
    public boolean nearPoint(int x, int y, float zoom, float resolutionX, float resolutionY) {
        int i;
        int xC, yC;
        float dist;

        // System.err.println("calling nearPoint1");
        nearPoint = NOT_A_POINT;

        for (i = 0; i < size(); i++) {
            xC = MipavMath.round(((Vector3f) (elementAt(i))).X * zoom * resolutionX);
            yC = MipavMath.round(((Vector3f) (elementAt(i))).Y * zoom * resolutionY);

            // System.err.println("xC is: " + xC + " yC is: " + yC + " x is: " + x + " y is: " + y);
            dist = (float) MipavMath.distance(x, xC, y, yC);

            if (dist < 3) {
                nearPoint = i;
                lastPoint = nearPoint;
            }
        }

        if (nearPoint == NOT_A_POINT) {
            return false;
        } else {
            return true;
        }
    }

    /**
     * Tests if a point is near a vertex of the curve.
     *
     * @param   volPoint point in FileCoordinates
     *
     * @return  returns boolean result of test
     */
    public boolean nearPointInPlane( Vector3f volPoint )
    {
        int i;
        int xC, yC, zC;
        float dist;
        boolean inPlane;

        nearPoint = NOT_A_POINT;

        for (i = 0; i < size(); i++)
        {
            if ( MipavMath.distance( volPoint, (Vector3f)(elementAt(i)) ) < 5 )
            {
                nearPoint = i;
                lastPoint = nearPoint;
            }
        }

        if (nearPoint == NOT_A_POINT) {
            return false;
        } else {
            return true;
        }
    }

    /**
     * Removes a point from the curve where the cursor is near.
     */
    public void removeElement() {

        if ((nearPoint >= 0) && (nearPoint < size())) {
            removeElementAt(nearPoint);
            lastPoint = 0;
        }
    }

    /**
     * Replaces a point on the curve with another.
     *
     * @param  x  x coordinate of point
     * @param  y  y coordinate of point
     * @param  z  z coordinate of point
     */
    public void replaceElement(float x, float y, float z) {

        if ((nearPoint >= 0) && (nearPoint < size())) {

            // removeElementAt(nearPoint);
            // insertElementAt(new Vector3f(x, y, z), nearPoint);
            ((Vector3f) (elementAt(nearPoint))).X = x;
            ((Vector3f) (elementAt(nearPoint))).Y = y;
            ((Vector3f) (elementAt(nearPoint))).Z = z;
        }
    }

    /**
     * Sets flag to indicate whether or not VOI is active.
     *
     * @param  active  flag sets the activity status of a flag
     */
    public void setActive(boolean active) {
        this.active = active;
    }


    /**
     * Sets the VOI object that contains this contour.
     * @param kGroup VOI container for this contour.
     */
    public void setGroup( VOI kGroup )
    {
        voiGroup = kGroup;
    }

    /**
     * Sets flag to indicate whether or not VOI is fixed.
     *
     * @param  fixed  flag sets the fixed status of a flag
     */
    public void setFixed(boolean fixed) {
        this.fixed = fixed;
    }

    /**
     * Sets the label of the VOI.
     *
     * @param  str  the label
     */
    public void setLabel(String str) {

        if (str == null) {
            label = null;
        } else {
            label = new String(str);
        }
    }

    /**
     * Sets the VOI name for display (instead of label...optional).
     *
     * @param  str  String
     */
    public void setName(String str) {

        if (str == null) {
            name = new String();
        } else {
            name = new String(str);
        }
    }
}
