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

public class IntrRay3Sphere3f extends Intersector
{
    /** Creates an IntrRay3Sphere3f object 
     * @param rkRay, the ray to intersect with the sphere
     * @param rkSphere, the Sphere to intersect with the ray
     */
    public IntrRay3Sphere3f (Ray3f rkRay, Sphere3f rkSphere)
    {
        m_rkRay = new Ray3f(rkRay);
        m_rkSphere = new Sphere3f(rkSphere);
        m_iQuantity = 0;
    }

    /** 
     * object access 
     * @return the current ray
     */
    public Ray3f GetRay ()
    {
        return m_rkRay;
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
     * returns true if the ray and sphere intersect, false otherwise
     */
    public boolean Test ()
    {
        Vector3f kDiff = m_rkRay.Origin.sub( m_rkSphere.Center );
        float fA0 = kDiff.Dot(kDiff) - m_rkSphere.Radius*m_rkSphere.Radius;
        if (fA0 <= (float)0.0)
        {
            // P is inside the sphere
            return true;
        }
        // else: P is outside the sphere

        float fA1 = m_rkRay.Direction.Dot(kDiff);
        if (fA1 >= (float)0.0)
        {
            return false;
        }

        // quadratic has a real root if discriminant is nonnegative
        return (fA1*fA1 >= fA0);
    }

    /**
     * find-intersection query
     * returns true if the ray and sphere intersect, false otherwise
     */
    public boolean Find ()
    {
        Vector3f kDiff = m_rkRay.Origin.sub( m_rkSphere.Center );
        float fA0 = kDiff.Dot(kDiff) - m_rkSphere.Radius*m_rkSphere.Radius;
        float fA1, fDiscr, fRoot;
        if (fA0 <= (float)0.0)
        {
            // P is inside the sphere
            m_iQuantity = 1;
            fA1 = m_rkRay.Direction.Dot(kDiff);
            fDiscr = fA1*fA1 - fA0;
            fRoot = (float)Math.sqrt(fDiscr);
            m_afRayT[0] = -fA1 + fRoot;
            m_akPoint[0] = m_rkRay.Origin.add( m_rkRay.Direction.scale(m_afRayT[0]) );
            return true;
        }
        // else: P is outside the sphere

        fA1 = m_rkRay.Direction.Dot(kDiff);
        if (fA1 >= (float)0.0)
        {
            m_iQuantity = 0;
            return false;
        }

        fDiscr = fA1*fA1 - fA0;
        if (fDiscr < (float)0.0)
        {
            m_iQuantity = 0;
        }
        else if (fDiscr >= Mathf.ZERO_TOLERANCE)
        {
            fRoot = (float)Math.sqrt(fDiscr);
            m_afRayT[0] = -fA1 - fRoot;
            m_afRayT[1] = -fA1 + fRoot;
            m_akPoint[0] = m_rkRay.Origin.add( m_rkRay.Direction.scale(m_afRayT[0]) );
            m_akPoint[1] = m_rkRay.Origin.add( m_rkRay.Direction.scale(m_afRayT[1]) );
            m_iQuantity = 2;
        }
        else
        {
            m_afRayT[0] = -fA1;
            m_akPoint[0] = m_rkRay.Origin.add( m_rkRay.Direction.scale(m_afRayT[0]) );
            m_iQuantity = 1;
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
     * Returns the ith intersection point ray parameter t 
     * @param i, the ith intersection
     * @return the ith intersection point  ray parameter t 
     */
    public float GetRayT (int i) 
    {
        assert(0 <= i && i < m_iQuantity);
        return m_afRayT[i];
    }

    /** the objects to intersect */
    private Ray3f m_rkRay;
    private Sphere3f m_rkSphere;

    /** information about the intersection set */
    private int m_iQuantity;
    private Vector3f[] m_akPoint = new Vector3f[2];
    private float[] m_afRayT = new float[2];
}
