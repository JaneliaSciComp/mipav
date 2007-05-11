// Geometric Tools, Inc.
// http://www.geometrictools.com
// Copyright (c) 1998-2006.  All Rights Reserved
//
// The Wild Magic Version 4 Foundation Library source code is supplied
// under the terms of the license agreement
//     http://www.geometrictools.com/License/Wm4FoundationLicense.pdf
// and may not be copied or disclosed except in accordance with the terms
// of that agreement.
//
// Version: 4.0.0 (2006/06/28)

package gov.nih.mipav.view.WildMagic.LibFoundation.Intersection;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;

public abstract class Intersector
{
    /** Static intersection queries.  The default implementations return
     * 'false'.  The Find query produces a set of intersection.  The derived
     * class is responsible for providing access to that set, since the nature
     * of the set is dependent on the object types.
     * @return false
     */
    public boolean Test ()
    {
        // stub for derived class
        assert(false);
        return false;
    }

    /** Static intersection queries.  The default implementations return
     * 'false'.  The Find query produces a set of intersection.  The derived
     * class is responsible for providing access to that set, since the nature
     * of the set is dependent on the object types.
     * @return false
     */
    public boolean Find ()
    {
        // stub for derived class
        assert(false);
        return false;
    }

    /** Dynamic intersection queries.  The default implementations return
     * 'false'.  The Find query produces a set of first contact.  The derived
     * class is responsible for providing access to that set, since the nature
     * of the set is dependent on the object types.
     */
    public boolean Test (float fTMax, Vector3f rkVelocity0, Vector3f rkVelocity1)
    {
        // stub for derived class
        assert(false);
        return false;
    }

    /** Dynamic intersection queries.  The default implementations return
     * 'false'.  The Find query produces a set of first contact.  The derived
     * class is responsible for providing access to that set, since the nature
     * of the set is dependent on the object types.
     */
    public boolean Find (float fTMax, Vector3f rkVelocity0, Vector3f rkVelocity1)
    {
        // stub for derived class
        assert(false);
        return false;
    }

    /** The time at which two objects are in first contact for the dynamic
     * intersection queries.
     */
    public float GetContactTime ()
    {
        return m_fContactTime;
    }

    /** information about the intersection set */
    public enum IntersectionInfo
    {
        IT_EMPTY ( LinCompf.ComponentType.CT_EMPTY.Value() ),
        IT_POINT ( LinCompf.ComponentType.CT_POINT.Value() ),
        IT_SEGMENT ( LinCompf.ComponentType.CT_SEGMENT.Value() ),
        IT_RAY ( LinCompf.ComponentType.CT_RAY.Value() ),
        IT_LINE ( LinCompf.ComponentType.CT_LINE.Value() ),
        IT_POLYGON (5),
        IT_PLANE (6),
        IT_POLYHEDRON (7),
        IT_OTHER (8);

        private int m_iValue;
        IntersectionInfo( int iValue ) {m_iValue  = iValue; }
        public int Value() { return m_iValue; }
    };

    /**
     * Returns the intersection type
     * @return IntersectionInfo intersection type
     */
    public IntersectionInfo GetIntersectionType ()
    {
        return m_iIntersectionType;
    }

    /**
     * Creates a default Intersector.
     */
    protected Intersector ()
    {
        m_fContactTime = (float)0.0;
        m_iIntersectionType = IntersectionInfo.IT_EMPTY;
    }

    /** Contact time */
    protected float m_fContactTime;
    /** Intersection type */
    protected IntersectionInfo m_iIntersectionType;
}
