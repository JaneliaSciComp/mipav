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

package gov.nih.mipav.view.WildMagic.LibFoundation.Intersection;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;

public class IntrLine3Triangle3f extends Intersector
{
    public IntrLine3Triangle3f (final Line3f rkLine,
                                final Triangle3f rkTriangle)
    {
        m_rkLine = new Line3f(rkLine);
        m_rkTriangle = new Triangle3f(rkTriangle);
    }

    // object access
    public final Line3f GetLine ()
    {
        return m_rkLine;
    }

    public final Triangle3f GetTriangle ()
    {
        return m_rkTriangle;
    }

    // test-intersection query
    public boolean Test ()
    {
        // compute the offset origin, edges, and normal
        Vector3f kDiff = m_rkLine.Origin.sub( m_rkTriangle.V[0] );
        Vector3f kEdge1 = m_rkTriangle.V[1].sub( m_rkTriangle.V[0] );
        Vector3f kEdge2 = m_rkTriangle.V[2].sub( m_rkTriangle.V[0] );
        Vector3f kNormal = kEdge1.Cross(kEdge2);

        // Solve Q + t*D = b1*E1 + b2*E2 (Q = kDiff, D = line direction,
        // E1 = kEdge1, E2 = kEdge2, N = Cross(E1,E2)) by
        //   |Dot(D,N)|*b1 = sign(Dot(D,N))*Dot(D,Cross(Q,E2))
        //   |Dot(D,N)|*b2 = sign(Dot(D,N))*Dot(D,Cross(E1,Q))
        //   |Dot(D,N)|*t = -sign(Dot(D,N))*Dot(Q,N)
        float fDdN = m_rkLine.Direction.Dot(kNormal);
        float fSign;
        if (fDdN > Mathf.ZERO_TOLERANCE)
        {
            fSign = 1.0f;
        }
        else if (fDdN < -Mathf.ZERO_TOLERANCE)
        {
            fSign = -1.0f;
            fDdN = -fDdN;
        }
        else
        {
            // Line and triangle are parallel, call it a "no intersection"
            // even if the line does intersect.
            return false;
        }

        float fDdQxE2 = fSign*m_rkLine.Direction.Dot(kDiff.Cross(kEdge2));
        if (fDdQxE2 >= 0.0)
        {
            float fDdE1xQ = fSign*m_rkLine.Direction.Dot(kEdge1.Cross(kDiff));
            if (fDdE1xQ >= 0.0)
            {
                if (fDdQxE2 + fDdE1xQ <= fDdN)
                {
                    // line intersects triangle
                    return true;
                }
                // else: b1+b2 > 1, no intersection
            }
            // else: b2 < 0, no intersection
        }
        // else: b1 < 0, no intersection

        return false;
    }

    // Find-intersection query.  The point of intersection is
    //   P = origin + t*direction = b0*V0 + b1*V1 + b2*V2
    public boolean Find ()
    {
        // compute the offset origin, edges, and normal
        Vector3f kDiff = m_rkLine.Origin.sub( m_rkTriangle.V[0] );
        Vector3f kEdge1 = m_rkTriangle.V[1].sub( m_rkTriangle.V[0] );
        Vector3f kEdge2 = m_rkTriangle.V[2].sub( m_rkTriangle.V[0] );
        Vector3f kNormal = kEdge1.Cross(kEdge2);

        // Solve Q + t*D = b1*E1 + b2*E2 (Q = kDiff, D = line direction,
        // E1 = kEdge1, E2 = kEdge2, N = Cross(E1,E2)) by
        //   |Dot(D,N)|*b1 = sign(Dot(D,N))*Dot(D,Cross(Q,E2))
        //   |Dot(D,N)|*b2 = sign(Dot(D,N))*Dot(D,Cross(E1,Q))
        //   |Dot(D,N)|*t = -sign(Dot(D,N))*Dot(Q,N)
        float fDdN = m_rkLine.Direction.Dot(kNormal);
        float fSign;
        if (fDdN > Mathf.ZERO_TOLERANCE)
        {
            fSign = 1.0f;
        }
        else if (fDdN < -Mathf.ZERO_TOLERANCE)
        {
            fSign = -1.0f;
            fDdN = -fDdN;
        }
        else
        {
            // Line and triangle are parallel, call it a "no intersection"
            // even if the line does intersect.
            return false;
        }

        float fDdQxE2 = fSign*m_rkLine.Direction.Dot(kDiff.Cross(kEdge2));
        if (fDdQxE2 >= 0.0)
        {
            float fDdE1xQ = fSign*m_rkLine.Direction.Dot(kEdge1.Cross(kDiff));
            if (fDdE1xQ >= 0.0)
            {
                if (fDdQxE2 + fDdE1xQ <= fDdN)
                {
                    // line intersects triangle
                    float fQdN = -fSign*kDiff.Dot(kNormal);
                    float fInv = (1.0f)/fDdN;
                    m_fLineT = fQdN*fInv;
                    m_fTriB1 = fDdQxE2*fInv;
                    m_fTriB2 = fDdE1xQ*fInv;
                    m_fTriB0 = 1.0f - m_fTriB1 - m_fTriB2;
                    return true;
                }
                // else: b1+b2 > 1, no intersection
            }
            // else: b2 < 0, no intersection
        }
        // else: b1 < 0, no intersection

        return false;
    }

    public float GetLineT ()
    {
        return m_fLineT;
    }

    public float GetTriB0 ()
    {
        return m_fTriB0;
    }

    public float GetTriB1 ()
    {
        return m_fTriB1;
    }

    public float GetTriB2 ()
    {
        return m_fTriB2;
    }

    // the objects to intersect
    private final Line3f m_rkLine;
    private final Triangle3f m_rkTriangle;

    // information about the intersection set
    private float m_fLineT, m_fTriB0, m_fTriB1, m_fTriB2;
}
