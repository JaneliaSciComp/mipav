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

public class IntrSegment3Sphere3f extends Intersector
{
    /** Creates an IntrSegment3Sphere3f object 
     * @param rkSegment, the segment to intersect with the sphere
     * @param rkSphere, the Sphere to intersect with the segment
     */
    public IntrSegment3Sphere3f ( Segment3f rkSegment,
                                  Sphere3f rkSphere)
    {
        m_rkSegment = new Segment3f(rkSegment);
        m_rkSphere = new Sphere3f(rkSphere);
        m_iQuantity = 0;
        ZeroThreshold = Mathf.ZERO_TOLERANCE;
    }

    /**
     * delete memory
     */
    public void finalize()
    {
        if ( m_rkSegment != null )
        {
            m_rkSegment.finalize();
            m_rkSegment = null;
        }
        if ( m_rkSphere != null )
        {
            m_rkSphere.finalize();
            m_rkSphere = null;
        }
        for ( int i = 0; i < 2; i++ )
        {
            if ( m_akPoint[i] != null )
            {
                m_akPoint[i].finalize();
                m_akPoint[i] = null;
            }
        }
        m_akPoint = null;
        m_afSegmentT = null;
    }


    /** 
     * object access 
     * @return the current segment
     */
    public Segment3f GetSegment ()
    {
        return m_rkSegment;
    }

    /** 
     * object access 
     * @return the current sphere
     */
    public Sphere3f GetSphere ()
    {
        return m_rkSphere;
    }

    /**
     * test-intersection query
     * @return true if the segment and sphere intersect, false otherwise
     */
    public boolean Test ()
    {
        Vector3f kDiff = m_rkSegment.Origin.sub( m_rkSphere.Center );
        float fA0 = kDiff.Dot(kDiff) - m_rkSphere.Radius*m_rkSphere.Radius;
        float fA1 = m_rkSegment.Direction.Dot(kDiff);
        float fDiscr = fA1*fA1 - fA0;
        if (fDiscr < (float)0.0)
        {
            return false;
        }

        float fTmp0 = m_rkSegment.Extent*m_rkSegment.Extent + fA0;
        float fTmp1 = ((float)2.0)*fA1*m_rkSegment.Extent;
        float fQM = fTmp0 - fTmp1;
        float fQP = fTmp0 + fTmp1;
        if (fQM*fQP <= (float)0.0)
        {
            return true;
        }

        return fQM > (float)0.0 && Math.abs(fA1) < m_rkSegment.Extent;
    }

    /**
     * find-intersection query
     * @return true if the segment and sphere intersect, false otherwise
     */
    public boolean Find ()
    {
        Vector3f kDiff = m_rkSegment.Origin.sub( m_rkSphere.Center );
        float fA0 = kDiff.Dot(kDiff) - m_rkSphere.Radius*m_rkSphere.Radius;
        float fA1 = m_rkSegment.Direction.Dot(kDiff);
        float fDiscr = fA1*fA1 - fA0;
        if (fDiscr < (float)0.0)
        {
            m_iQuantity = 0;
            return false;
        }

        float fTmp0 = m_rkSegment.Extent*m_rkSegment.Extent + fA0;
        float fTmp1 = ((float)2.0)*fA1*m_rkSegment.Extent;
        float fQM = fTmp0 - fTmp1;
        float fQP = fTmp0 + fTmp1;
        float fRoot;
        if (fQM*fQP <= (float)0.0)
        {
            fRoot = (float)Math.sqrt(fDiscr);
            m_afSegmentT[0] = (fQM > (float)0.0 ? -fA1 - fRoot : -fA1 + fRoot);
            m_akPoint[0] = m_rkSegment.Origin.
                add( m_rkSegment.Direction.scale(m_afSegmentT[0]) );
            m_iQuantity = 1;
            return true;
        }

        if (fQM > (float)0.0 && Math.abs(fA1) < m_rkSegment.Extent)
        {
            if (fDiscr >= ZeroThreshold)
            {
                fRoot = (float)Math.sqrt(fDiscr);
                m_afSegmentT[0] = -fA1 - fRoot;
                m_afSegmentT[1] = -fA1 + fRoot;
                m_akPoint[0] = m_rkSegment.Origin.
                    add( m_rkSegment.Direction.scale( m_afSegmentT[0] ) );
                m_akPoint[1] = m_rkSegment.Origin.
                    add( m_rkSegment.Direction.scale(m_afSegmentT[1]) );
                m_iQuantity = 2;
            }
            else
            {
                m_afSegmentT[0] = -fA1;
                m_akPoint[0] = m_rkSegment.Origin.
                    add( m_rkSegment.Direction.scale(m_afSegmentT[0]) );
                m_iQuantity = 1;
            }
        }
        else
        {
            m_iQuantity = 0;
        }

        return m_iQuantity > 0;
    }

    /**
     * Returns the number of intersections
     * @return the number of intersections
     */
    public int GetQuantity ()
    {
        return m_iQuantity;
    }

    /**
     * Returns the ith intersection point 
     * @param i, the ith intersection
     * @return the ith intersection point 
     */
    public Vector3f GetPoint (int i)
    {
        assert(0 <= i && i < m_iQuantity);
        return m_akPoint[i];
    }

    /**
     * Returns the ith intersection point segment parameter t 
     * @param i, the ith intersection
     * @return the ith intersection point  segment parameter t 
     */
    public float GetSegmentT (int i)
    {
        assert(0 <= i && i < m_iQuantity);
        return m_afSegmentT[i];
    }

    /** default = Mathf::ZERO_TOLERANCE */
    public float ZeroThreshold = Mathf.ZERO_TOLERANCE; 

    /** the objects to intersect: Segment */
    private Segment3f m_rkSegment;
    /** the objects to intersect: Sphere */
    private Sphere3f m_rkSphere;

    /** information about the intersection set: number of intersections */
    private int m_iQuantity;
    /** information about the intersection set: intersection points */
    private Vector3f[] m_akPoint = new Vector3f[2];
    /** information about the intersection set: line equation T-values of
     * intersections */
    private float[] m_afSegmentT = new float[2];
}
