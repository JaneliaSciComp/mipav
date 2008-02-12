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
public class IntrRay3Box3f extends Intersector
{
    public IntrRay3Box3f (final Ray3f rkRay, final Box3f rkBox)
    {
        m_rkRay = rkRay;
        m_rkBox = rkBox;
    }

    // object access
    public final Ray3f GetRay ()
    {
        return m_rkRay;
    }

    public final Box3f GetBox ()
    {
        return m_rkBox;
    }

    // static intersection queries
    public boolean Test ()
    {
        float[] afWdU = new float[3];
        float[] afAWdU = new float[3];
        float[] afDdU = new float[3];
        float[] afADdU = new float[3];
        float[] afAWxDdU = new float[3];
        float fRhs;

        Vector3f kDiff = m_rkRay.Origin.sub( m_rkBox.Center );

        afWdU[0] = m_rkRay.Direction.Dot(m_rkBox.Axis[0]);
        afAWdU[0] = Math.abs(afWdU[0]);
        afDdU[0] = kDiff.Dot(m_rkBox.Axis[0]);
        afADdU[0] = Math.abs(afDdU[0]);
        if (afADdU[0] > m_rkBox.Extent[0] && afDdU[0]*afWdU[0] >= 0.0)
        {
            return false;
        }

        afWdU[1] = m_rkRay.Direction.Dot(m_rkBox.Axis[1]);
        afAWdU[1] = Math.abs(afWdU[1]);
        afDdU[1] = kDiff.Dot(m_rkBox.Axis[1]);
        afADdU[1] = Math.abs(afDdU[1]);
        if (afADdU[1] > m_rkBox.Extent[1] && afDdU[1]*afWdU[1] >= 0.0)
        {
            return false;
        }

        afWdU[2] = m_rkRay.Direction.Dot(m_rkBox.Axis[2]);
        afAWdU[2] = Math.abs(afWdU[2]);
        afDdU[2] = kDiff.Dot(m_rkBox.Axis[2]);
        afADdU[2] = Math.abs(afDdU[2]);
        if (afADdU[2] > m_rkBox.Extent[2] && afDdU[2]*afWdU[2] >= 0.0)
        {
            return false;
        }

        Vector3f kWxD = m_rkRay.Direction.Cross(kDiff);

        afAWxDdU[0] = Math.abs(kWxD.Dot(m_rkBox.Axis[0]));
        fRhs = m_rkBox.Extent[1]*afAWdU[2] + m_rkBox.Extent[2]*afAWdU[1];
        if (afAWxDdU[0] > fRhs)
        {
            return false;
        }

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
        float[] fT0 = new float[]{0.0f};
        float[] fT1 = new float[]{Float.MAX_VALUE};
        int[] aiQuantity = new int[]{m_iQuantity};
        Intersector.IntersectionInfo[] iIntersectionType = new Intersector.IntersectionInfo[]{m_iIntersectionType};
        boolean bResult = IntrLine3Box3f.DoClipping(fT0,fT1,m_rkRay.Origin,m_rkRay.Direction,m_rkBox,
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

    // the objects to intersect
    private Ray3f m_rkRay;
    private Box3f m_rkBox;

    // information about the intersection set
    private int m_iQuantity;
    private Vector3f[] m_akPoint = new Vector3f[2];
}
