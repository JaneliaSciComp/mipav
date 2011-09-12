package gov.nih.mipav.model.structures;

import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3f;



/**
 * Class for a point VOI, a simple extension of CurveBase.
 *
 * @version    1.0 Feb. 10, 1999
 * @author     Matthew J. McAuliffe
 * @see        VOI
 *
 * $Logfile: /mipav/src/gov/nih/mipav/model/structures/VOIPoint.java $
 * $Revision: 64 $
 * $Date: 2/24/06 3:34p $
 */
public class VOIPoint extends VOIBase {


    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 48410548552570045L;


    /** True if this point is the 1st point in a slice/frame of Polyline_Slice*/
    private boolean firstSlicePoint = false;

    /** Distance is calculated and passed in by VOI for polyline_slices */
    private String totalDistanceString = null;

    /** Distance between segments shown only for active point in polyline_slices*/
    private String distanceString = null;

    /** True only for the active point of a polyline_slice structure (display's coordinates)*/
    private boolean isActivePoint = false;

    /** ID if this VOIPoint is contained in a PolyLineSlice object. */
    private int m_iID = -1;

    private TransMatrix m_kRotation = null;
    private TransMatrix m_kRotationInverse = null;

    /**
     *  Default constructor
     */
    public VOIPoint() 
    {
        super();
        m_iVOIType = VOI.POINT;        
        closed = false;
    }
    
    /**
     * Sets the type. If the input type is VOI.POLYPOINT, this VOIPoint is part of a PolyLineSlice object.
     * @param iType
     */
    public VOIPoint(int iType )
    {
        super( false, false );
        m_iVOIType = iType;
    }
    
    /**
     * Sets the type and position. If the input type is VOI.POLYPOINT, this VOIPoint is part of a PolyLineSlice object.
     * @param iType
     * @param kPosition
     */
    public VOIPoint(int iType, Vector<Vector3f> kPosition)
    {
        super( false, false, kPosition);
        m_iVOIType = iType;
    }
    
    /**
     * Sets the type and position. If the input type is VOI.POLYPOINT, this VOIPoint is part of a PolyLineSlice object.
     * @param iType
     * @param kPosition
     */
    public VOIPoint(int iType, Vector3f kPosition )
    {
        super( false, false);
        m_iVOIType = iType;
        add(kPosition);
    }

    /**
     * Copy Constructor.
     * @param kVOI
     */
    public VOIPoint( VOIPoint kVOI )
    {
        super(kVOI);
        this.firstSlicePoint = kVOI.firstSlicePoint;
        if ( kVOI.totalDistanceString != null )
        {
            this.totalDistanceString = new String( kVOI.totalDistanceString );
        }
        if ( kVOI.distanceString != null )
        {
            this.distanceString = new String( kVOI.distanceString );
        }
        this.isActivePoint = kVOI.isActivePoint;
        this.m_iID = kVOI.m_iID;
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.model.structures.VOIBase#clone()
     */
    @Override
	public VOIPoint clone() {
        return new VOIPoint(this);
    }
    
    /**
     * Returns true if the input iX, iY is contained within this contour.  
     * The z-value of the contour is ignored.
     * @param iX
     * @param iY
     * @return
     */
    @Override
	public boolean contains(float iX, float iY)
    {
        if ( m_kRotationInverse != null )
        {
            Vector3f kPos = new Vector3f( iX, iY, 0 );
            iX = Math.round((kPos.X * m_kRotationInverse.Get(0, 0)) + (kPos.Y * m_kRotationInverse.Get(0, 1)) + m_kRotationInverse.Get(0, 2));
            iY = Math.round((kPos.X * m_kRotationInverse.Get(1, 0)) + (kPos.Y * m_kRotationInverse.Get(1, 1)) + m_kRotationInverse.Get(1, 2));
        }
        Vector3f[] kBounds = getImageBoundingBox();
        if ( iX < kBounds[0].X || iX > kBounds[1].X ||
                iY < kBounds[0].Y || iY > kBounds[1].Y  )
        {
            return false;
        }        
        return true;
    }


    /**
     * Returns true if the input iX,iY,iZ is contained within this contour.
     * @param iX
     * @param iY
     * @param iZ
     * @return
     */
    @Override
	public boolean contains(float iX, float iY, float iZ )
    {
        if ( m_kRotationInverse != null )
        {
            Vector3f kPos = new Vector3f( iX, iY, 0 );
            iX = Math.round((kPos.X * m_kRotationInverse.Get(0, 0)) + (kPos.Y * m_kRotationInverse.Get(0, 1)) + m_kRotationInverse.Get(0, 2));
            iY = Math.round((kPos.X * m_kRotationInverse.Get(1, 0)) + (kPos.Y * m_kRotationInverse.Get(1, 1)) + m_kRotationInverse.Get(1, 2));
        }
        Vector3f[] kBounds = getImageBoundingBox();
        if ( iX < kBounds[0].X || iX > kBounds[1].X ||
                iY < kBounds[0].Y || iY > kBounds[1].Y ||
                iZ < kBounds[0].Z || iZ > kBounds[1].Z   )
        {
            return false;
        }
        return true;
    }


    /**
     * Returns the distance string.
     * @return
     */
    public String distanceString()
    {
        return distanceString;
    }
    
    /**
     *  Method to access point VOI coordinate
     *  @return       3d point
     */
    public Vector3f exportPoint() {
        return ( elementAt( 0 ) );
    }

    /**
     *   This method gets the coordinates of the point
     *   @param coord     a float array, in which the first element is the
     *                    x coordinate and the second element is the
     *                    y coordinate
     */
    public void getCoordinates( float[] coord ) {
        Vector3f pt = exportPoint();
        if ( coord.length > 0 ) coord[0] = pt.X;
        if ( coord.length > 1 ) coord[1] = pt.Y;
        if ( coord.length > 2 ) coord[2] = pt.Z;
    }

    /**
     * Gets the geometric center of the contour.
     * 
     * @return returns the geometric center
     */
    @Override
	public Vector3f getGeometricCenter() {     
        gcPt.Copy( getPosition() );
        return new Vector3f(gcPt);
    }
    
    /**
     * Returns ID in PolyLineSlice object.
     * @return
     */
    public int getID()
    {
        return m_iID;
    }


    public Vector3f getPosition()
    {
        if ( m_kRotation == null )
        {
            return elementAt(0);
        }
        Vector3f kPos = new Vector3f(elementAt(0));

        int x = Math.round((kPos.X * m_kRotation.Get(0, 0)) + (kPos.Y * m_kRotation.Get(0, 1)) + m_kRotation.Get(0, 2));
        int y = Math.round((kPos.X * m_kRotation.Get(1, 0)) + (kPos.Y * m_kRotation.Get(1, 1)) + m_kRotation.Get(1, 2));
        kPos.X = x;
        kPos.Y = y;
        return kPos;
    }

    /**
     * True if this is the active point in the PolyLineSlice object.
     * @return
     */
    public boolean isActivePoint()
    {
        return isActivePoint;
    }
    

    /**
     * Returns true if this is the first point in the PolyLineSlice object.
     * @return
     */
    public boolean isFirstSlicePoint()
    {
        return firstSlicePoint;
    }
    

    /**
     *  Sets the point at (xP,yP,zP). Bounds checking is performed
     *  @param xP    x location
     *  @param yP    y location
     *  @param zP    z location
     *  @param xDim  x dimension maximum
     *  @param yDim  y dimension maximum
     *  @param zDim  z dimension maximum
     */
    public void locateVOIPoint( int xP, int yP, int zP, int xDim, int yDim, int zDim ) {
        Vector3f pt = exportPoint();
        if ( xP >= xDim || xP < 0 ) {
            return;
        }
        if ( yP >= yDim || yP < 0 ) {
            return;
        }
        if ( zP >= zDim || zP < 0 ) {
            return;
        }
        pt.X = xP;
        pt.Y = yP;
        pt.Z = zP;
    }



    /**
     *  Moves the point to the new location. Bounds checking is performed
     *  @param xM    amount in pixels to move the line in the x direction
     *  @param yM    amount in pixels to move the line in the y direction
     *  @param zM    amount in pixels to move the line in the z direction
     *  @param xDim  x dimension maximum
     *  @param yDim  y dimension maximum
     *  @param zDim  z dimension maximum
     */
    public void moveVOIPoint( int xM, int yM, int zM, int xDim, int yDim, int zDim ) {

        Vector3f pt = exportPoint();
        if ( pt.X + xM >= xDim || pt.X + xM < 0 ) {
            return;
        }
        if ( pt.Y + yM >= yDim || pt.Y + yM < 0 ) {
            return;
        }
        if ( pt.Z + zM >= zDim || pt.Z + zM < 0 ) {
            return;
        }
        pt.X = pt.X + xM;
        pt.Y = pt.Y + yM;
        pt.Z = pt.Z + zM;
    }

    

    /**
     * Returns true if the input position is near one of the points on this contour.
     * @param iX input x-position.
     * @param iY input y-position.
     * @param iZ input z-position
     * @return true if the input position is near one of the contour points.
     */
	public boolean nearPoint( int iX, int iY, int iZ) {
        if ( m_kRotationInverse != null )
        {
            Vector3f kPos = new Vector3f( iX, iY, 0 );
            iX = Math.round((kPos.X * m_kRotationInverse.Get(0, 0)) + (kPos.Y * m_kRotationInverse.Get(0, 1)) + m_kRotationInverse.Get(0, 2));
            iY = Math.round((kPos.X * m_kRotationInverse.Get(1, 0)) + (kPos.Y * m_kRotationInverse.Get(1, 1)) + m_kRotationInverse.Get(1, 2));
        }
        Vector3f kVOIPoint = new Vector3f(iX, iY, iZ );
        for ( int i = 0; i < size(); i++ )
        {
            Vector3f kPos = get(i);
            Vector3f kDiff = new Vector3f();
            kDiff.Sub( kPos, kVOIPoint );
            if ( (Math.abs( kDiff.X ) < 3) &&  (Math.abs( kDiff.Y ) < 3) && (Math.abs( kDiff.Z ) < 3) )
            {
                setNearPoint(i);
                return true;
            }
        }
        return false;
    }

    /**
     * Parameters for drawing each point in the PolyLineSlice object.
     * @param isFirst boolean is this the very first point of the polyline slice structure
     * @param isActPt boolean is this the active point (for drawing as green and showing segment length)
     * @param totalDistance String the total distance of the polyline slice
     * @param dist String the segment (active point to next point) distance of the polyline slice
     */
    public void setFirstPoint(boolean isFirst, boolean isActPt, String totalDistance, String dist) {
        this.firstSlicePoint = isFirst;
        this.isActivePoint = isActPt;

        this.totalDistanceString = totalDistance;
        this.distanceString = dist;
    }

    /**
     * Parameters for drawing each point in the PolyLineSlice object.
     * @param isFirst boolean is this the very first point of the polyline slice structure
     * @param isActPt boolean is this the active point (for drawing as green and showing segment length)
     * @param totalDistance String the total distance of the polyline slice
     * @param dist String the segment (active point to next point) distance of the polyline slice
     */
    public void setFirstPoint(boolean isFirst, boolean isActPt, String totalDistance, String dist, int iID) {
        this.firstSlicePoint = isFirst;
        this.isActivePoint = isActPt;

        this.totalDistanceString = totalDistance;
        this.distanceString = dist;

        m_iID = iID;
    }
    
    public void setMatrix( TransMatrix kMat )
    {
        m_kRotation = kMat;
        m_kRotationInverse = new TransMatrix(kMat);
        m_kRotationInverse.Inverse();
    }


    public void setPlane( int iPlane )
    {
        m_iPlane = XPLANE | YPLANE | ZPLANE;
        m_bUpdatePlane = false;
    }

    /**
     * Returns the totalDistance string.
     * @return
     */
    public String totalDistanceString()
    {
        return totalDistanceString;
    }
}
