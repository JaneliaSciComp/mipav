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

public class IntrLine3Sphere3f extends Intersector
{
    /** Creates an IntrLine3Sphere3f object 
     * @param rkLine, the Line to intersect with the sphere
     * @param rkSphere, the Sphere to intersect with the line
     */
    public IntrLine3Sphere3f ( Line3f rkLine, Sphere3f rkSphere)
    {
        m_rkLine = new Line3f(rkLine);
        m_rkSphere = new Sphere3f(rkSphere);
        m_iQuantity = 0;
    }

    /**
     * delete memory
     */
    public void finalize()
    {
        if ( m_rkLine != null )
        {
            m_rkLine.finalize();
            m_rkLine = null;
        }
        m_rkLine = null;
        m_rkSphere.finalize();
        if ( m_rkLine != null )
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
        m_afLineT = null;
    }

    /** 
     * object access 
     * @return the current line
     */
    public Line3f GetLine ()
    {
        return m_rkLine;
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
     * returns true if the line and sphere intersect, false otherwise
     */
    public boolean Test ()
    {
        Vector3f kDiff = m_rkLine.Origin.sub( m_rkSphere.Center );
        float fA0 = kDiff.Dot(kDiff) - m_rkSphere.Radius*m_rkSphere.Radius;
        float fA1 = m_rkLine.Direction.Dot(kDiff);
        float fDiscr = fA1*fA1 - fA0;
        return (fDiscr >= (float)0.0);
    }

    /**
     * find-intersection query
     * returns true if the line and sphere intersect, false otherwise
     */
    public boolean Find ()
    {
        Vector3f kDiff = m_rkLine.Origin.sub( m_rkSphere.Center );
        float fA0 = kDiff.Dot(kDiff) - m_rkSphere.Radius*m_rkSphere.Radius;
        float fA1 = m_rkLine.Direction.Dot(kDiff);
        float fDiscr = fA1*fA1 - fA0;

        if (fDiscr < (float)0.0)
        {
            m_iQuantity = 0;
        }
        else if (fDiscr >= Mathf.ZERO_TOLERANCE)
        {
            float fRoot = (float)Math.sqrt(fDiscr);
            m_afLineT[0] = -fA1 - fRoot;
            m_afLineT[1] = -fA1 + fRoot;
            m_akPoint[0] = m_rkLine.Origin.add( m_rkLine.Direction.scale(m_afLineT[0]) );
            m_akPoint[1] = m_rkLine.Origin.add( m_rkLine.Direction.scale(m_afLineT[1]) );
            m_iQuantity = 2;
        }
        else
        {
            m_afLineT[0] = -fA1;
            m_akPoint[0] = m_rkLine.Origin.add( m_rkLine.Direction.scale(m_afLineT[0]) );
            m_iQuantity = 1;
        }

        return (m_iQuantity > 0);
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
     * Returns the ith intersection point line parameter t 
     * @param i, the ith intersection
     * @return the ith intersection point  line parameter t 
     */
    public float GetLineT (int i)
    {
        assert( 0 <= i && i < m_iQuantity );
        return m_afLineT[i];
    }

    /** the objects to intersect: Line */
    private Line3f m_rkLine;
    /** the objects to intersect: Sphere */
    private Sphere3f m_rkSphere;

    /** information about the intersection set: number of intersections */
    private int m_iQuantity;
    /** information about the intersection set: intersection points */
    private Vector3f[] m_akPoint = new Vector3f[2];
    /** information about the intersection set: line equation T-values of
     * intersections */
    private float[] m_afLineT = new float[2];
}
