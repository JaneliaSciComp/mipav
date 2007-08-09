// Wild Magic Source Code
// David Eberly
// http://www.geometrictools.com
// Copyright (c) 1998-2007
//
// This library is free software; you can redistribute it and/or modify it
// under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or (at
// your option) any later version.  The license is available for reading at
// either of the locations:
//     http://www.gnu.org/copyleft/lgpl.html
//     http://www.geometrictools.com/License/WildMagicLicense.pdf
//
// Version: 4.0.0 (2006/06/28)
//
// Ported to Java by Alexandra Bokinsky, PhD, Geometric Tools, Inc. (July 2007)
//

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

    /** The time at which two objects are in first contact for the dynamic
     * intersection queries.
     * @return first contact time.
     */
    public final float GetContactTime ()
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
    public final IntersectionInfo GetIntersectionType ()
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
