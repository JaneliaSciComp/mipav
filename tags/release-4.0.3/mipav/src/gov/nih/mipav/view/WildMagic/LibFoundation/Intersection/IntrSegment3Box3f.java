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
public class IntrSegment3Box3f extends Intersector
{
    public IntrSegment3Box3f (final Segment3f rkSegment,
                              final Box3f rkBox, boolean bSolid)
    {
        m_rkSegment = rkSegment;
        m_rkBox = rkBox;
        m_bSolid = bSolid;
    }

    // object access
    public final Segment3f GetSegment ()
    {
        return m_rkSegment;
    }

    public final Box3f GetBox ()
    {
        return m_rkBox;
    }

    // static intersection queries
    public boolean Test ()
    {
        float[] afAWdU = new float[3];
        float[] afADdU = new float[3];
        float[] afAWxDdU = new float[3];
        float fRhs;

        Vector3f kDiff = m_rkSegment.Origin.sub( m_rkBox.Center );

        afAWdU[0] = Math.abs(m_rkSegment.Direction.Dot(m_rkBox.Axis[0]));
        afADdU[0] = Math.abs(kDiff.Dot(m_rkBox.Axis[0]));
        fRhs = m_rkBox.Extent[0] + m_rkSegment.Extent*afAWdU[0];
        if (afADdU[0] > fRhs)
        {
            return false;
        }

        afAWdU[1] = Math.abs(m_rkSegment.Direction.Dot(m_rkBox.Axis[1]));
        afADdU[1] = Math.abs(kDiff.Dot(m_rkBox.Axis[1]));
        fRhs = m_rkBox.Extent[1] + m_rkSegment.Extent*afAWdU[1];
        if (afADdU[1] > fRhs)
        {
            return false;
        }

        afAWdU[2] = Math.abs(m_rkSegment.Direction.Dot(m_rkBox.Axis[2]));
        afADdU[2] = Math.abs(kDiff.Dot(m_rkBox.Axis[2]));
        fRhs = m_rkBox.Extent[2] + m_rkSegment.Extent*afAWdU[2];
        if (afADdU[2] > fRhs)
        {
            return false;
        }

        Vector3f kWxD = m_rkSegment.Direction.Cross(kDiff);

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
        float[] fT0 = new float[]{-m_rkSegment.Extent};
        float[] fT1 = new float[]{m_rkSegment.Extent};
        int[] aiQuantity = new int[]{m_iQuantity};
        Intersector.IntersectionInfo[] iIntersectionType = new Intersector.IntersectionInfo[]{m_iIntersectionType};
        boolean bResult = IntrLine3Box3f.DoClipping(fT0,fT1,m_rkSegment.Origin,m_rkSegment.Direction,m_rkBox,
                                                    m_bSolid,aiQuantity,m_akPoint,iIntersectionType);
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
    private Segment3f m_rkSegment;
    private Box3f m_rkBox;
    private boolean m_bSolid;

    // information about the intersection set
    private int m_iQuantity;
    private Vector3f[] m_akPoint = new Vector3f[2];
}
