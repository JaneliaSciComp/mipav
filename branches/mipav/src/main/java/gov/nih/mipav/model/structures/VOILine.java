package gov.nih.mipav.model.structures;

import gov.nih.mipav.util.MipavMath;

import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * Class for line, a simple extension of VOIBase. A line is formed of two points.
 *
 * @version  1.0 March 15, 2000
 * @author   Matthew McAuliffe, Ph.D.
 * @see      VOI
 */
public class VOILine extends VOIBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2836164853862289329L;

    /**
     * Default Constructor.
     */
    public VOILine( )
    {
        super( false, false );
        m_iVOIType = VOI.LINE;
    }

    /**
     * Constructor, sets the position of the line.
     * @param kPositions
     */
    public VOILine( Vector<Vector3f> kPositions)
    {
        super( false, false, kPositions );
        m_iVOIType = VOI.LINE;
    }

    /**
     * Copy Constructor.
     * @param kVOI
     */
    public VOILine( VOILine kVOI )
    {
        super(kVOI);
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.model.structures.VOIBase#clone()
     */
    @Override
	public VOILine clone() {
        return new VOILine(this);
    }
    
    /**
     * Gets the geometric center of the contour.
     * 
     * @return returns the geometric center
     */
    @Override
	public Vector3f getGeometricCenter() {     
        gcPt.X = MipavMath.round( (get(0).X + get(1).X) / 2f );
        gcPt.Y = MipavMath.round( (get(0).Y + get(1).Y) / 2f );
        gcPt.Z = MipavMath.round( (get(0).Z + get(1).Z) / 2f );
        return gcPt;
    }

    /**
     * Gets the position/intensity along a line VOI.
     *
     * @param   position     array that is filled with all x coordinates
     * @param   intensity    the corresponding intensities along the line
     * @param   imageBuffer  DOCUMENT ME!
     * @param   xD           x-Dimension of image
     *
     * @return  the number of points in the position and intensity array that have a valid data.
     */
    public int getPositionAndIntensity(Vector3f[] position, float[] intensity, float[] imageBuffer, int xD) {
        double distance;
        int i;
        int index, indexX = 0, indexY = 0;
        double myY, myX, yInc, xInc;
        double x0, x1, y0, y1;
        int len;
        x0 = ((elementAt(0))).X;
        y0 = ((elementAt(0))).Y;
        x1 = ((elementAt(1))).X;
        y1 = ((elementAt(1))).Y;
        distance = Math.sqrt(((x1 - x0) * (x1 - x0)) + ((y1 - y0) * (y1 - y0)));
        myY = y0;
        myX = x0;
        xInc = (x1 - x0) / (2 * distance);
        yInc = (y1 - y0) / (2 * distance);

        int pt = 0;
        len = (int) Math.round(2 * distance);

        for (i = 0; i < len; i++) {

            if ((indexX != Math.round(myX)) || (indexY != Math.round(myY))) {
                indexY = (int) Math.round(myY);
                indexX = (int) Math.round(myX);
                position[pt].X = indexX;
                position[pt].Y = indexY;
                index = (indexY * xD) + indexX;
                intensity[pt] = imageBuffer[index];
                pt++;
            }

            myX = myX + xInc;
            myY = myY + yInc;
        }

        return pt;
    }
  

    /**
     * Translate both points of the VOI.
     *
     * @param  xT  amount to translate in the x direction
     * @param  yT  amount to translate in the y direction
     */
    public void translate(float xT, float yT) {
        int i;

        for (i = 0; i < 2; i++) {
            ((elementAt(i))).X += xT;
            ((elementAt(i))).Y += yT;
        }
    }

}
