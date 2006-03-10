package gov.nih.mipav.model.structures;

/**
 *
 * <p>Title: CubeBounds</p>
 *
 * <p>Description: This class is a simple representation of a cube. It is used
 * in the region-grow-bounding-box changes.
 * </p>
 *
 *
 * @author lee orsino
 * @version 1.0
 */

public class CubeBounds
{
    private int highX;  // the high value of X
    private int lowX;   // the low value of X
    private int highY;  // etc..
    private int lowY;
    private int highZ;
    private int lowZ;

    /**
     *
     * @param x1 int high x value
     * @param x2 int low x value
     * @param y1 int high y value
     * @param y2 int low y value
     * @param z1 int high z value
     * @param z2 int low z value
     */
    public CubeBounds(int highXValue, int lowXValue, int highYValue, int lowYValue, int highZValue, int lowZValue)
    {
        this.highX = Math.max(highXValue, lowXValue);
        this.lowX  = Math.min(highXValue, lowXValue);
        this.highY = Math.max(highYValue, lowYValue);
        this.lowY  = Math.min(highYValue, lowYValue);
        this.highZ = Math.max(highZValue, lowZValue);
        this.lowZ  = Math.min(highZValue, lowZValue);
    }

    public int highX()
    {
        return this.highX;
    }

    public int lowX()
    {
        return this.lowX;
    }

    public int highY()
    {
        return this.highY;
    }

    public int lowY()
    {
        return this.lowY;
    }

    public int highZ()
    {
        return this.highZ;
    }

    public int lowZ()
    {
        return this.lowZ;
    }

    // method tests seedPt parameter for inclusion inside this logical cube
    public boolean contains(Point3Ds seedPt)
    {
        if (seedPt.x >= lowX && seedPt.x <= highX &&
            seedPt.y >= lowY && seedPt.y <= highY &&
            seedPt.z >= lowZ && seedPt.z <= highZ)
        {
            return true;
        }

        return false;
    }

    public String toString()
    {
        return "CubeBounds: (x1, x2), (y1, y2), (z1, z2) == (" + highX + ", " + lowX + "), " +
                                                           "(" + highY + ", " + lowY + "), " +
                                                           "(" + highZ + ", " + lowZ + ")";
    }
}
