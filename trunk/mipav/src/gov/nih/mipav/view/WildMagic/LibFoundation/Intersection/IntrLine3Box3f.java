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
public class IntrLine3Box3f extends Intersector
{
    public IntrLine3Box3f ( Line3f rkLine, Box3f rkBox)
    {
        m_rkLine = rkLine;
        m_rkBox = rkBox;
    }

    // object access
    public final Line3f GetLine ()
    {
        return m_rkLine;
    }
    public final Box3f GetBox ()
    {
        return m_rkBox;
    }

    // static intersection queries
    public boolean Test ()
    {
        float[] afAWdU = new float[3];
        float[] afAWxDdU = new float[3];
        float fRhs;

        Vector3f kDiff = m_rkLine.Origin.sub( m_rkBox.Center );
        Vector3f kWxD = m_rkLine.Direction.Cross(kDiff);

        afAWdU[1] = Math.abs(m_rkLine.Direction.Dot(m_rkBox.Axis[1]));
        afAWdU[2] = Math.abs(m_rkLine.Direction.Dot(m_rkBox.Axis[2]));
        afAWxDdU[0] = Math.abs(kWxD.Dot(m_rkBox.Axis[0]));
        fRhs = m_rkBox.Extent[1]*afAWdU[2] + m_rkBox.Extent[2]*afAWdU[1];
        if (afAWxDdU[0] > fRhs)
        {
            return false;
        }

        afAWdU[0] = Math.abs(m_rkLine.Direction.Dot(m_rkBox.Axis[0]));
        afAWxDdU[1] = Math.abs(kWxD.Dot(m_rkBox.Axis[1]));
        fRhs = m_rkBox.Extent[0]*afAWdU[2] + m_rkBox.Extent[2]*afAWdU[0];
        if (afAWxDdU[1] > fRhs)
        {
            return false;
        }

        afAWxDdU[2] = Math.abs(kWxD.Dot(m_rkBox.Axis[2]));
        fRhs = m_rkBox.Extent[0]*afAWdU[1] + m_rkBox.Extent[1]*afAWdU[0];
        if (afAWxDdU[2] > fRhs)
        {
            return false;
        }

        return true;
    }

    public boolean Find ()
    {
        float[] fT0 = new float[]{-Float.MAX_VALUE};
        float[] fT1 = new float[]{Float.MAX_VALUE};
        int[] aiQuantity = new int[]{m_iQuantity};
        Intersector.IntersectionInfo[] iIntersectionType = new Intersector.IntersectionInfo[]{m_iIntersectionType};
        boolean bResult = DoClipping(fT0,fT1,m_rkLine.Origin,m_rkLine.Direction,m_rkBox,
                          true,aiQuantity,m_akPoint,iIntersectionType);
        m_iQuantity = aiQuantity[0];
        m_iIntersectionType = iIntersectionType[0];
        return bResult;
                                                
    }

    // the intersection set
    public int GetQuantity ()
    {
        return m_iQuantity;
    }

    public final Vector3f GetPoint (int i)
    {
        assert(0 <= i && i < m_iQuantity);
        return m_akPoint[i];
    }

    private static boolean Clip (float fDenom, float fNumer, float[] rfT0, float[] rfT1)
    {
        // Return value is 'true' if line segment intersects the current test
        // plane.  Otherwise 'false' is returned in which case the line segment
        // is entirely clipped.

        if (fDenom > 0.0f)
        {
            if (fNumer > fDenom*rfT1[0])
            {
                return false;
            }
            if (fNumer > fDenom*rfT0[0])
            {
                rfT0[0] = fNumer/fDenom;
            }
            return true;
        }
        else if (fDenom < 0.0f)
        {
            if (fNumer > fDenom*rfT0[0])
            {
                return false;
            }
            if (fNumer > fDenom*rfT1[0])
            {
                rfT1[0] = fNumer/fDenom;
            }
            return true;
        }
        else
        {
            return (fNumer <= 0.0f);
        }
    }

    // the objects to intersect
    private Line3f m_rkLine;
    private Box3f m_rkBox;

    // information about the intersection set
    private int m_iQuantity;
    private Vector3f[] m_akPoint = new Vector3f[2];

    // internal use (shared by IntrRay3Box3 and IntrSegment3Box3)
    public static boolean DoClipping (float[] fT0, float[] fT1, final Vector3f rkOrigin,
                                      final Vector3f rkDirection, final Box3f rkBox,
                                      boolean bSolid, int[] riQuantity, Vector3f[] akPoint,
                                      Intersector.IntersectionInfo[] riIntrType)
    {
        assert(fT0[0] < fT1[0]);

        // convert linear component to box coordinates
        Vector3f kDiff = rkOrigin.sub( rkBox.Center );
        Vector3f kBOrigin = new Vector3f(
                                         kDiff.Dot(rkBox.Axis[0]),
                                         kDiff.Dot(rkBox.Axis[1]),
                                         kDiff.Dot(rkBox.Axis[2])
                                         );
        Vector3f kBDirection = new Vector3f(
                                            rkDirection.Dot(rkBox.Axis[0]),
                                            rkDirection.Dot(rkBox.Axis[1]),
                                            rkDirection.Dot(rkBox.Axis[2])
                                            );

        float fSaveT0 = fT0[0], fSaveT1 = fT1[0];
        boolean bNotAllClipped =
            Clip(+kBDirection.X(),-kBOrigin.X()-rkBox.Extent[0],fT0,fT1) &&
            Clip(-kBDirection.X(),+kBOrigin.X()-rkBox.Extent[0],fT0,fT1) &&
            Clip(+kBDirection.Y(),-kBOrigin.Y()-rkBox.Extent[1],fT0,fT1) &&
            Clip(-kBDirection.Y(),+kBOrigin.Y()-rkBox.Extent[1],fT0,fT1) &&
            Clip(+kBDirection.Z(),-kBOrigin.Z()-rkBox.Extent[2],fT0,fT1) &&
            Clip(-kBDirection.Z(),+kBOrigin.Z()-rkBox.Extent[2],fT0,fT1);
        
        if (bNotAllClipped && (bSolid || fT0[0] != fSaveT0 || fT1[0] != fSaveT1))
        {
            if (fT1[0] > fT0[0])
            {
                riIntrType[0] = Intersector.IntersectionInfo.IT_SEGMENT;
                riQuantity[0] = 2;
                akPoint[0].SetData( rkOrigin.add( rkDirection.scale(fT0[0])));
                akPoint[1].SetData( rkOrigin.add( rkDirection.scale(fT1[0])));
            }
            else
            {
                riIntrType[0] = Intersector.IntersectionInfo.IT_POINT;
                riQuantity[0] = 1;
                akPoint[0].SetData( rkOrigin.add( rkDirection.scale(fT0[0])));
            }
        }
        else
        {
            riQuantity[0] = 0;
            riIntrType[0] = Intersector.IntersectionInfo.IT_EMPTY;
        }

        return (riIntrType[0] != Intersector.IntersectionInfo.IT_EMPTY);
    }

}
