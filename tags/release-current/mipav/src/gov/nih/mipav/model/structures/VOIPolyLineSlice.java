package gov.nih.mipav.model.structures;

import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3f;

/**
 * PolyLineSlice is implemented as a list of <VOIPoints>
 * Each VOIPoint in the list may be on a different slice of the image.
 *
 */
public class VOIPolyLineSlice extends VOIBase
{
    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -7909604245705492799L;
    /** List of VOIPoint that make up the polyline. */
    protected Vector<VOIPoint> m_kPoints = new Vector<VOIPoint>();

    /**
     * Default Constructor.
     */
    public VOIPolyLineSlice( )
    {
        super();
        m_iVOIType = VOI.POLYLINE_SLICE;
    }   

    /**
     * Constructor sets the first VOIPoint in the polyline.
     * @param kPoint
     */
    public VOIPolyLineSlice( VOIPoint kPoint )
    {
        super( false, false, kPoint );
        m_iVOIType = VOI.POLYLINE_SLICE;
        m_kPoints.clear();
        m_kPoints.add( kPoint.clone() );
    }   

    /**
     * Copy constructor.
     * @param kVOI
     */
    public VOIPolyLineSlice( VOIPolyLineSlice kVOI )
    {
        super( kVOI );
        m_kPoints.clear();
        for ( int i = 0; i < size(); i++ )
        {
            m_kPoints.add( kVOI.m_kPoints.get(i).clone() );
        }
    }
    
    /* 
     * Adds a point to the list, creates a new VOIPoint and adds to the list.
     * @see java.util.Vector#add(java.lang.Object)
     */
    @Override
	public boolean add( Vector3f kPt )
    {
        super.add( kPt );
        if ( m_kPoints != null )
        {
            m_kPoints.add( new VOIPoint( m_iVOIType, kPt ) );
        }

        if ( m_kVolumeVOI != null )
        {
            m_kVolumeVOI.setVOI(this);
        }
        return true;
    }

    /**
     * Add a new VOIPoint to this polyline.
     * @param kPoint
     */
    public void add( VOIPoint kPoint )
    {
        super.add( kPoint.get(0) );
        m_kPoints.add(kPoint);

        if ( m_kVolumeVOI != null )
        {
            m_kVolumeVOI.setVOI(this);
        }
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.model.structures.VOIBase#clone()
     */
    @Override
	public VOIPolyLineSlice clone() {
        return new VOIPolyLineSlice(this);
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.model.structures.VOIBase#delete(int)
     */
    @Override
	public void delete( int iPos )
    {
        m_kPoints.remove(iPos);
        super.delete(iPos);
    }

    /**
     * Returns the list of VOIPoint in this polyline.
     * @return
     */
    public Vector<VOIPoint> getPoints()
    {
        return m_kPoints;
    }    
    
    /* (non-Javadoc)
     * @see java.util.Vector#set(int, java.lang.Object)
     */
    @Override
	public Vector3f set( int i, Vector3f kNew )
    {
        Vector3f kOld = super.set(i,kNew);
        m_kPoints.get(i).set(0,kNew);
        return kOld;
    }
    
    /* 
     * Sets the active VOIPoint
     * @see gov.nih.mipav.model.structures.VOIBase#setActive(boolean)
     */
    @Override
	public void setActive(boolean active) {
        this.active = active;
        for ( int i = 0; i < m_kPoints.size(); i++ )
        {
            m_kPoints.get(i).setActive(active);
        }
        if ( active )
        {
            VOI kGroup = getGroup();
            if ( (kGroup.getCurves().size() > 1) && (kGroup.getCurves().indexOf(this) != 0) )
            {
                kGroup.getCurves().remove(this);
                kGroup.getCurves().add(0, this);
            }
        }
    }
}
