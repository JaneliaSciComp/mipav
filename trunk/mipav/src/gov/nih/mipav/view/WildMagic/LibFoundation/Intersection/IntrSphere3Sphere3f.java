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

public class IntrSphere3Sphere3f extends Intersector
{
    /** Creates an IntrSphere3Sphere3f object 
     * @param rkSphere0, the first sphere
     * @param rkSphere1, the second sphere
     */
    public IntrSphere3Sphere3f ( Sphere3f rkSphere0, Sphere3f rkSphere1)
    {
        m_rkSphere0 = new Sphere3f(rkSphere0);
        m_rkSphere1 = new Sphere3f(rkSphere1);
    }

    /**
     * delete memory
     */
    public void finalize()
    {
        if ( m_rkSphere0 != null )
        {
            m_rkSphere0.finalize();
            m_rkSphere0 = null;
        }
        if ( m_rkSphere1 != null )
        {
            m_rkSphere1.finalize();
            m_rkSphere1 = null;
        }
        if ( m_kCenter != null )
        {
            m_kCenter.finalize();
            m_kCenter = null;
        }
        if ( m_kUAxis != null )
        {
            m_kUAxis.finalize();
            m_kUAxis = null;
        }
        if ( m_kVAxis != null )
        {
            m_kVAxis.finalize();
            m_kVAxis = null;
        }
        if ( m_kNormal != null )
        {
            m_kNormal.finalize();
            m_kNormal = null;
        }
        if ( m_kContactPoint != null )
        {
            m_kContactPoint.finalize();
            m_kContactPoint = null;
        }
    }


    /** 
     * object access 
     * @return the sphere0
     */
    public final Sphere3f GetSphere0 ()
    {
        return m_rkSphere0;
    }

    /** 
     * object access 
     * @return the sphere1
     */
    public final Sphere3f GetSphere1 ()
    {
        return m_rkSphere1;
    }

    /**
     * static test-intersection query
     * @return true if the two spheres intersect, false otherwise
     */
    public boolean Test ()
    {
        Vector3f kDiff = new Vector3f();
        m_rkSphere1.Center.sub( m_rkSphere0.Center, kDiff );
        float fRSum = m_rkSphere0.Radius + m_rkSphere1.Radius;
        boolean bResult = (kDiff.SquaredLength() <= fRSum*fRSum);
        kDiff = null;
        return bResult;
    }

    /**
     * static find-intersection query
     * @return true if the two spheres intersect, false otherwise
     */
    public boolean Find ()
    {
        // plane of intersection must have N as its normal
        m_rkSphere1.Center.sub( m_rkSphere0.Center, m_kNormal );
        float fNSqrLen = m_kNormal.SquaredLength();
        float fRSum = m_rkSphere0.Radius + m_rkSphere1.Radius;
        if (fNSqrLen > fRSum*fRSum)
        {
            // sphere centers are too far apart for intersection
            return false;
        }

        float fR0Sqr = m_rkSphere0.Radius*m_rkSphere0.Radius;
        float fR1Sqr = m_rkSphere1.Radius*m_rkSphere1.Radius;
        float fInvNSqrLen = ((float)1.0)/fNSqrLen;
        float fT = ((float)0.5)*((float)1.0+(fR0Sqr-fR1Sqr)*fInvNSqrLen);
        if (fT < (float)0.0 || fT > (float)1.0)
        {
            return false;
        }

        float fRSqr = fR0Sqr - fT*fT*fNSqrLen;
        if (fRSqr < (float)0.0)
        {
            return false;
        }

        // center and radius of circle of intersection
        Vector3f kScale = new Vector3f();
        m_kNormal.scale(fT, kScale);
        m_rkSphere0.Center.add( kScale, m_kCenter );
        m_fRadius = (float)Math.sqrt(fRSqr);
        kScale = null;
        // compute U and V for plane of circle
        //m_kNormal *= (float)Math.sqrt(fInvNSqrLen);
        m_kNormal.scaleEquals( (float)Math.sqrt(fInvNSqrLen) );
        Vector3f.GenerateComplementBasis(m_kUAxis,m_kVAxis,m_kNormal);

        return true;
    }

    /** Intersection center
     * @return center
     */
    public final Vector3f GetCenter ()
    {
        return m_kCenter;
    }

    /** Intersection U-Vector
     * @return U-Vector
     */
    public final Vector3f GetUAxis ()
    {
        return m_kUAxis;
    }

    /** Intersection V-Vector
     * @return V-Vector
     */
    public final Vector3f GetVAxis ()
    {
        return m_kVAxis;
    }

    /** Intersection plane normal
     * @return plane normal
     */
    public final Vector3f GetNormal ()
    {
        return m_kNormal;
    }

    /** Intersection radius
     * @return radius
     */
    public final float GetRadius ()
    {
        return m_fRadius;
    }

    /**
     * dynamic test-intersection query
     * @return true if the two spheres intersect, false otherwise
     */
    public boolean Test (float fTMax,  Vector3f rkVelocity0,
                         Vector3f rkVelocity1)
    {
        Vector3f kVDiff = new Vector3f();
        rkVelocity1.sub( rkVelocity0, kVDiff );
        float fA = kVDiff.SquaredLength();
        Vector3f kCDiff = new Vector3f();
        m_rkSphere1.Center.sub( m_rkSphere0.Center, kCDiff );
        float fC = kCDiff.SquaredLength();
        float fRSum = m_rkSphere0.Radius + m_rkSphere1.Radius;
        float fRSumSqr = fRSum*fRSum;

        if (fA > (float)0.0)
        {
            float fB = kCDiff.Dot(kVDiff);
            if (fB <= (float)0.0)
            {
                if (-fTMax*fA <= fB)
                {
                    return fA*fC - fB*fB <= fA*fRSumSqr;
                }
                else
                {
                    return fTMax*(fTMax*fA + ((float)2.0)*fB) + fC <= fRSumSqr;
                }
            }
        }
        kVDiff = null;
        kCDiff = null;
        return (fC <= fRSumSqr);
    }

    /**
     * dynamic find-intersection query
     * @return true if the two spheres intersect, false otherwise
     */
    public boolean Find (float fTMax, Vector3f rkVelocity0,
                         Vector3f rkVelocity1)
    {
        Vector3f kVDiff = new Vector3f();
        rkVelocity1.sub( rkVelocity0, kVDiff );
        float fA = kVDiff.SquaredLength();
        Vector3f kCDiff = new Vector3f();
        m_rkSphere1.Center.sub( m_rkSphere0.Center, kCDiff );
        float fC = kCDiff.SquaredLength();
        float fRSum = m_rkSphere0.Radius + m_rkSphere1.Radius;
        float fRSumSqr = fRSum*fRSum;

        if (fA > (float)0.0)
        {
            float fB = kCDiff.Dot(kVDiff);
            if (fB <= (float)0.0)
            {
                if (-fTMax*fA <= fB
                    ||  fTMax*(fTMax*fA + ((float)2.0)*fB) + fC <= fRSumSqr)
                {
                    float fCDiff = fC - fRSumSqr;
                    float fDiscr = fB*fB - fA*fCDiff;
                    if (fDiscr >= (float)0.0)
                    {
                        if (fCDiff <= (float)0.0)
                        {
                            // The spheres are initially intersecting.  Estimate a
                            // point of contact by using the midpoint of the line
                            // segment connecting the sphere centers.
                            m_fContactTime = (float)0.0;
                            m_rkSphere0.Center.add( m_rkSphere1.Center, m_kContactPoint);
                            m_kContactPoint.scaleEquals( 0.5f );
                        }
                        else
                        {
                            // The first time of contact is in [0,fTMax].
                            m_fContactTime = -(fB + (float)Math.sqrt(fDiscr))/fA;
                            if (m_fContactTime < (float)0.0)
                            {
                                m_fContactTime = (float)0.0;
                            }
                            else if (m_fContactTime > fTMax)
                            {
                                m_fContactTime = fTMax;
                            }

                            Vector3f kNewCDiff = new Vector3f(kVDiff);
                            kNewCDiff.scaleEquals(m_fContactTime);
                            kNewCDiff.addEquals( kCDiff );
                            kNewCDiff.scaleEquals(m_rkSphere0.Radius/fRSum);
                            Vector3f kScale = new Vector3f();
                            rkVelocity0.scale(m_fContactTime, kScale);
                            kNewCDiff.addEquals(kScale);
                            kScale = null;
                            m_rkSphere0.Center.add( kNewCDiff, m_kContactPoint );
                            kNewCDiff = null;
                        }
                        kVDiff = null;
                        kCDiff = null;
                        return true;
                    }
                }
                kVDiff = null;
                kCDiff = null;
                return false;
            }
        }
        kVDiff = null;
        kCDiff = null;
        if (fC <= fRSumSqr)
        {
            // The spheres are initially intersecting.  Estimate a point of
            // contact by using the midpoint of the line segment connecting the
            // sphere centers.
            m_fContactTime = (float)0.0;
            m_rkSphere0.Center.add( m_rkSphere1.Center, m_kContactPoint );
            m_kContactPoint.scaleEquals(0.5f);
            
            return true;
        }

        return false;
    }


    /** intersection set for dynamic find-intersection query
     * @return contact point
     */
    public final Vector3f GetContactPoint ()
    {
        return m_kContactPoint;
    }

    /** the objects to intersect: Sphere0 */
    private Sphere3f m_rkSphere0;
    /** the objects to intersect: Sphere1 */
    private Sphere3f m_rkSphere1;

    /** Circle of intersection for static spheres.  The center is C and lies
     * on a plane spanned by the unit-length, orthogonal vectors U and V.
     * The plane normal is a unit-length vector N.  The radius of the circle
     * in that plane is R.
     */
    private Vector3f m_kCenter = new Vector3f(), m_kUAxis = new Vector3f(), m_kVAxis = new Vector3f(), m_kNormal = new Vector3f();
    private float m_fRadius;

    /** Point of intersection for dynamic spheres. */
    private Vector3f m_kContactPoint = new Vector3f();
}
