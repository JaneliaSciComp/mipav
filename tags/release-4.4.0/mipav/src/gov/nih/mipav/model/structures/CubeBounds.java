package gov.nih.mipav.model.structures;


/**
 * <p>Title: CubeBounds</p>
 *
 * <p>Description: This class is a simple representation of a cube. It is used in the region-grow-bounding-box changes.
 * </p>
 *
 * @author   lee orsino
 * @version  1.0
 */

public class CubeBounds {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int highX; // the high value of X

    /** DOCUMENT ME! */
    private int highY; // etc..

    /** DOCUMENT ME! */
    private int highZ;

    /** DOCUMENT ME! */
    private int lowX; // the low value of X

    /** DOCUMENT ME! */
    private int lowY;

    /** DOCUMENT ME! */
    private int lowZ;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new CubeBounds object.
     *
     * @param  highXValue  int high x value
     * @param  lowXValue   int low x value
     * @param  highYValue  int high y value
     * @param  lowYValue   int low y value
     * @param  highZValue  int high z value
     * @param  lowZValue   int low z value
     */
    public CubeBounds(int highXValue, int lowXValue, int highYValue, int lowYValue, int highZValue, int lowZValue) {
        this.highX = Math.max(highXValue, lowXValue);
        this.lowX = Math.min(highXValue, lowXValue);
        this.highY = Math.max(highYValue, lowYValue);
        this.lowY = Math.min(highYValue, lowYValue);
        this.highZ = Math.max(highZValue, lowZValue);
        this.lowZ = Math.min(highZValue, lowZValue);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * method tests seedPt parameter for inclusion inside this logical cube.
     *
     * @param   seedPt  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean contains(Point3D seedPt) {

        if ((seedPt.x >= lowX) && (seedPt.x <= highX) && (seedPt.y >= lowY) && (seedPt.y <= highY) &&
                (seedPt.z >= lowZ) && (seedPt.z <= highZ)) {
            return true;
        }

        return false;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int highX() {
        return this.highX;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int highY() {
        return this.highY;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int highZ() {
        return this.highZ;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int lowX() {
        return this.lowX;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int lowY() {
        return this.lowY;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int lowZ() {
        return this.lowZ;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public String toString() {
        return "CubeBounds: (x1, x2), (y1, y2), (z1, z2) == (" + highX + ", " + lowX + "), " + "(" + highY + ", " +
               lowY + "), " + "(" + highZ + ", " + lowZ + ")";
    }
}
