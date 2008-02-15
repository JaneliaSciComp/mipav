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

public class IntrTriangle3Cone3f extends Intersector
{
    public IntrTriangle3Cone3f (final Triangle3f rkTriangle,
                                final Cone3f rkCone)
    {
        m_rkTriangle = rkTriangle;
        m_rkCone = rkCone;
    }

    // object access
    public final Triangle3f GetTriangle ()
    {
        return m_rkTriangle;
    }

    public final Cone3f GetCone ()
    {
        return m_rkCone;
    }

    // static query
    public boolean Test ()
    {
        // triangle is <P0,P1,P2>, edges are E0 = P1-P0, E1=P2-P0
        int iOnConeSide = 0;
        float fP0Test = 0.0f, fP1Test = 0.0f, fP2Test = 0.0f;
        float fAdE, fEdE, fEdD, fC1, fC2;

        float fCosSqr = m_rkCone.CosAngle*m_rkCone.CosAngle;

        // test vertex P0
        Vector3f kDiff0 = m_rkTriangle.V[0].sub( m_rkCone.Vertex );
        float fAdD0 = m_rkCone.Axis.Dot(kDiff0);
        if (fAdD0 >= 0.0)
        {
            // P0 is on cone side of plane
            fP0Test = fAdD0*fAdD0 - fCosSqr*(kDiff0.Dot(kDiff0));
            if (fP0Test >= 0.0)
            {
                // P0 is inside the cone
                return true;
            }
            else
            {
                // P0 is outside the cone, but on cone side of plane
                iOnConeSide |= 1;
            }
        }
        // else P0 is not on cone side of plane

        // test vertex P1
        Vector3f kEdge0 = m_rkTriangle.V[1].sub( m_rkTriangle.V[0] );
        Vector3f kDiff1 = kDiff0.add( kEdge0 );
        float fAdD1 = m_rkCone.Axis.Dot(kDiff1);
        if (fAdD1 >= 0.0)
        {
            // P1 is on cone side of plane
            fP1Test = fAdD1*fAdD1 - fCosSqr*(kDiff1.Dot(kDiff1));
            if (fP1Test >= 0.0)
            {
                // P1 is inside the cone
                return true;
            }
            else
            {
                // P1 is outside the cone, but on cone side of plane
                iOnConeSide |= 2;
            }
        }
        // else P1 is not on cone side of plane

        // test vertex P2
        Vector3f kEdge1 = m_rkTriangle.V[2].sub( m_rkTriangle.V[0] );
        Vector3f kDiff2 = kDiff0.add( kEdge1 );
        float fAdD2 = m_rkCone.Axis.Dot(kDiff2);
        if (fAdD2 >= 0.0)
        {
            // P2 is on cone side of plane
            fP2Test = fAdD2*fAdD2 - fCosSqr*(kDiff2.Dot(kDiff2));
            if (fP2Test >= 0.0)
            {
                // P2 is inside the cone
                return true;
            }
            else
            {
                // P2 is outside the cone, but on cone side of plane
                iOnConeSide |= 4;
            }
        }
        // else P2 is not on cone side of plane

        // test edge <P0,P1> = E0
        if ((iOnConeSide & 3) != 0)
        {
            fAdE = fAdD1 - fAdD0;
            fEdE = kEdge0.Dot(kEdge0);
            fC2 = fAdE*fAdE - fCosSqr*fEdE;
            if (fC2 < 0.0)
            {
                fEdD = kEdge0.Dot(kDiff0);
                fC1 = fAdE*fAdD0 - fCosSqr*fEdD;
                if ((iOnConeSide & 1) != 0)
                {
                    if ((iOnConeSide & 2) != 0)
                    {
                        // <P0,P1> fully on cone side of plane, fC0 = fP0Test
                        if (0.0 <= fC1 && fC1 <= -fC2
                            &&  fC1*fC1 >= fP0Test*fC2)
                        {
                            return true;
                        }
                    }
                    else
                    {
                        // P0 on cone side (Dot(A,P0-V) >= 0),
                        // P1 on opposite side (Dot(A,P1-V) <= 0)
                        // (Dot(A,E0) <= 0), fC0 = fP0Test
                        if (0.0 <= fC1 && fC2*fAdD0 <= fC1*fAdE
                            &&  fC1*fC1 >= fP0Test*fC2)
                        {
                            return true;
                        }
                    }
                }
                else
                {
                    // P1 on cone side (Dot(A,P1-V) >= 0),
                    // P0 on opposite side (Dot(A,P0-V) <= 0)
                    // (Dot(A,E0) >= 0), fC0 = fP0Test (needs calculating)
                    if (fC1 <= -fC2 && fC2*fAdD0 <= fC1*fAdE)
                    {
                        fP0Test = fAdD0*fAdD0 - fCosSqr*(kDiff0.Dot(kDiff0));
                        if (fC1*fC1 >= fP0Test*fC2)
                        {
                            return true;
                        }
                    }
                }
            }
        }
        // else <P0,P1> does not intersect cone half space

        // test edge <P0,P2> = E1
        if ((iOnConeSide & 5) != 0)
        {
            fAdE = fAdD2 - fAdD0;
            fEdE = kEdge1.Dot(kEdge1);
            fC2 = fAdE*fAdE - fCosSqr*fEdE;
            if (fC2 < 0.0)
            {
                fEdD = kEdge1.Dot(kDiff0);
                fC1 = fAdE*fAdD0 - fCosSqr*fEdD;
                if ((iOnConeSide & 1) != 0)
                {
                    if ((iOnConeSide & 4) != 0)
                    {
                        // <P0,P2> fully on cone side of plane, fC0 = fP0Test
                        if (0.0 <= fC1 && fC1 <= -fC2
                            &&  fC1*fC1 >= fP0Test*fC2)
                        {
                            return true;
                        }
                    }
                    else
                    {
                        // P0 on cone side (Dot(A,P0-V) >= 0),
                        // P2 on opposite side (Dot(A,P2-V) <= 0)
                        // (Dot(A,E1) <= 0), fC0 = fP0Test
                        if (0.0 <= fC1 && fC2*fAdD0 <= fC1*fAdE
                            &&  fC1*fC1 >= fP0Test*fC2)
                        {
                            return true;
                        }
                    }
                }
                else
                {
                    // P2 on cone side (Dot(A,P2-V) >= 0),
                    // P0 on opposite side (Dot(A,P0-V) <= 0)
                    // (Dot(A,E1) >= 0), fC0 = fP0Test (needs calculating)
                    if (fC1 <= -fC2 && fC2*fAdD0 <= fC1*fAdE)
                    {
                        fP0Test = fAdD0*fAdD0 - fCosSqr*(kDiff0.Dot(kDiff0));
                        if (fC1*fC1 >= fP0Test*fC2)
                        {
                            return true;
                        }
                    }
                }
            }
        }
        // else <P0,P2> does not intersect cone half space

        // test edge <P1,P2> = E1-E0 = E2
        if ((iOnConeSide & 6) != 0)
        {
            Vector3f kE2 = kEdge1.sub( kEdge0 );
            fAdE = fAdD2 - fAdD1;
            fEdE = kE2.Dot(kE2);
            fC2 = fAdE*fAdE - fCosSqr*fEdE;
            if (fC2 < 0.0)
            {
                fEdD = kE2.Dot(kDiff1);
                fC1 = fAdE*fAdD1 - fCosSqr*fEdD;
                if ((iOnConeSide & 2) != 0)
                {
                    if ((iOnConeSide & 4) != 0)
                    {
                        // <P1,P2> fully on cone side of plane, fC0 = fP1Test
                        if (0.0 <= fC1 && fC1 <= -fC2
                            &&  fC1*fC1 >= fP1Test*fC2)
                        {
                            return true;
                        }
                    }
                    else
                    {
                        // P1 on cone side (Dot(A,P1-V) >= 0),
                        // P2 on opposite side (Dot(A,P2-V) <= 0)
                        // (Dot(A,E2) <= 0), fC0 = fP1Test
                        if (0.0 <= fC1 && fC2*fAdD1 <= fC1*fAdE
                            &&  fC1*fC1 >= fP1Test*fC2)
                        {
                            return true;
                        }
                    }
                }
                else
                {
                    // P2 on cone side (Dot(A,P2-V) >= 0),
                    // P1 on opposite side (Dot(A,P1-V) <= 0)
                    // (Dot(A,E2) >= 0), fC0 = fP1Test (needs calculating)
                    if (fC1 <= -fC2 && fC2*fAdD1 <= fC1*fAdE)
                    {
                        fP1Test = fAdD1*fAdD1 - fCosSqr*(kDiff1.Dot(kDiff1));
                        if (fC1*fC1 >= fP1Test*fC2)
                        {
                            return true;
                        }
                    }
                }
            }
        }
        // else <P1,P2> does not intersect cone half space

        // Test triangle <P0,P1,P2>.  It is enough to handle only the case when
        // at least one Pi is on the cone side of the plane.  In this case and
        // after the previous testing, if the triangle intersects the cone, the
        // set of intersection must contain the point of intersection between
        // the cone axis and the triangle.
        if (iOnConeSide > 0)
        {
            Vector3f kN = kEdge0.Cross(kEdge1);
            float fNdA = kN.Dot(m_rkCone.Axis);
            float fNdD = kN.Dot(kDiff0);
            Vector3f kU = m_rkCone.Axis.scale(fNdD).sub( kDiff0.scale(fNdA) );
            Vector3f kNcU = kN.Cross(kU);

            float fNcUdE0 = kNcU.Dot(kEdge0), fNcUdE1, fNcUdE2, fNdN;
            if (fNdA >= 0.0)
            {
                if (fNcUdE0 <= 0.0)
                {
                    fNcUdE1 = kNcU.Dot(kEdge1);
                    if (fNcUdE1 >= 0.0)
                    {
                        fNcUdE2 = fNcUdE1 - fNcUdE0;
                        fNdN = kN.SquaredLength();
                        if (fNcUdE2 <= fNdA*fNdN)
                        {
                            return true;
                        }
                    }
                }
            }
            else
            {
                if (fNcUdE0 >= 0.0)
                {
                    fNcUdE1 = kNcU.Dot(kEdge1);
                    if (fNcUdE1 <= 0.0)
                    {
                        fNcUdE2 = fNcUdE1 - fNcUdE0;
                        fNdN = kN.SquaredLength();
                        if (fNcUdE2 >= fNdA*fNdN)
                        {
                            return true;
                        }
                    }
                }
            }
        }

        return false;
    }


    // the objects to intersect
    private Triangle3f m_rkTriangle;
    private Cone3f m_rkCone;
}
