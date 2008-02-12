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

public class IntrBox3Box3f extends Intersector
{
    public IntrBox3Box3f (final Box3f rkBox0, final Box3f rkBox1)
    {
        m_rkBox0 = rkBox0;
        m_rkBox1 = rkBox1;
    }

    // object access
    public final Box3f GetBox0 ()
    {
        return m_rkBox0;
    }
    public final Box3f GetBox1 ()
    {
        return m_rkBox1;
    }

    // static test-intersection query
    public boolean Test ()
    {
        // Cutoff for cosine of angles between box axes.  This is used to catch
        // the cases when at least one pair of axes are parallel.  If this
        // happens, there is no need to test for separation along the
        // Cross(A[i],B[j]) directions.
        float fCutoff = 1.0f - Mathf.ZERO_TOLERANCE;
        boolean bExistsParallelPair = false;
        int i;

        // convenience variables
        Vector3f[] akA = m_rkBox0.Axis;
        Vector3f[] akB = m_rkBox1.Axis;
        float[] afEA = m_rkBox0.Extent;
        float[] afEB = m_rkBox1.Extent;

        // compute difference of box centers, D = C1-C0
        Vector3f kD = m_rkBox1.Center.sub( m_rkBox0.Center );

        float[][] aafC = new float[3][3];     // matrix C = A^T B, c_{ij} = Dot(A_i,B_j)
        float[][] aafAbsC = new float[3][3];  // |c_{ij}|
        float[] afAD = new float[3];        // Dot(A_i,D)
        float fR0, fR1, fR;   // interval radii and distance between centers
        float fR01;           // = R0 + R1

        // axis C0+t*A0
        for (i = 0; i < 3; i++)
        {
            aafC[0][i] = akA[0].Dot(akB[i]);
            aafAbsC[0][i] = Math.abs(aafC[0][i]);
            if (aafAbsC[0][i] > fCutoff)
            {
                bExistsParallelPair = true;
            }
        }
        afAD[0] = akA[0].Dot(kD);
        fR = Math.abs(afAD[0]);
        fR1 = afEB[0]*aafAbsC[0][0]+afEB[1]*aafAbsC[0][1]+afEB[2]*aafAbsC[0][2];
        fR01 = afEA[0] + fR1;
        if (fR > fR01)
        {
            return false;
        }

        // axis C0+t*A1
        for (i = 0; i < 3; i++)
        {
            aafC[1][i] = akA[1].Dot(akB[i]);
            aafAbsC[1][i] = Math.abs(aafC[1][i]);
            if (aafAbsC[1][i] > fCutoff)
            {
                bExistsParallelPair = true;
            }
        }
        afAD[1] = akA[1].Dot(kD);
        fR = Math.abs(afAD[1]);
        fR1 = afEB[0]*aafAbsC[1][0]+afEB[1]*aafAbsC[1][1]+afEB[2]*aafAbsC[1][2];
        fR01 = afEA[1] + fR1;
        if (fR > fR01)
        {
            return false;
        }

        // axis C0+t*A2
        for (i = 0; i < 3; i++)
        {
            aafC[2][i] = akA[2].Dot(akB[i]);
            aafAbsC[2][i] = Math.abs(aafC[2][i]);
            if (aafAbsC[2][i] > fCutoff)
            {
                bExistsParallelPair = true;
            }
        }
        afAD[2] = akA[2].Dot(kD);
        fR = Math.abs(afAD[2]);
        fR1 = afEB[0]*aafAbsC[2][0]+afEB[1]*aafAbsC[2][1]+afEB[2]*aafAbsC[2][2];
        fR01 = afEA[2] + fR1;
        if (fR > fR01)
        {
            return false;
        }

        // axis C0+t*B0
        fR = Math.abs(akB[0].Dot(kD));
        fR0 = afEA[0]*aafAbsC[0][0]+afEA[1]*aafAbsC[1][0]+afEA[2]*aafAbsC[2][0];
        fR01 = fR0 + afEB[0];
        if (fR > fR01)
        {
            return false;
        }

        // axis C0+t*B1
        fR = Math.abs(akB[1].Dot(kD));
        fR0 = afEA[0]*aafAbsC[0][1]+afEA[1]*aafAbsC[1][1]+afEA[2]*aafAbsC[2][1];
        fR01 = fR0 + afEB[1];
        if (fR > fR01)
        {
            return false;
        }

        // axis C0+t*B2
        fR = Math.abs(akB[2].Dot(kD));
        fR0 = afEA[0]*aafAbsC[0][2]+afEA[1]*aafAbsC[1][2]+afEA[2]*aafAbsC[2][2];
        fR01 = fR0 + afEB[2];
        if (fR > fR01)
        {
            return false;
        }

        // At least one pair of box axes was parallel, so the separation is
        // effectively in 2D where checking the "edge" normals is sufficient for
        // the separation of the boxes.
        if (bExistsParallelPair)
        {
            return true;
        }

        // axis C0+t*A0xB0
        fR = Math.abs(afAD[2]*aafC[1][0]-afAD[1]*aafC[2][0]);
        fR0 = afEA[1]*aafAbsC[2][0] + afEA[2]*aafAbsC[1][0];
        fR1 = afEB[1]*aafAbsC[0][2] + afEB[2]*aafAbsC[0][1];
        fR01 = fR0 + fR1;
        if (fR > fR01)
        {
            return false;
        }

        // axis C0+t*A0xB1
        fR = Math.abs(afAD[2]*aafC[1][1]-afAD[1]*aafC[2][1]);
        fR0 = afEA[1]*aafAbsC[2][1] + afEA[2]*aafAbsC[1][1];
        fR1 = afEB[0]*aafAbsC[0][2] + afEB[2]*aafAbsC[0][0];
        fR01 = fR0 + fR1;
        if (fR > fR01)
        {
            return false;
        }

        // axis C0+t*A0xB2
        fR = Math.abs(afAD[2]*aafC[1][2]-afAD[1]*aafC[2][2]);
        fR0 = afEA[1]*aafAbsC[2][2] + afEA[2]*aafAbsC[1][2];
        fR1 = afEB[0]*aafAbsC[0][1] + afEB[1]*aafAbsC[0][0];
        fR01 = fR0 + fR1;
        if (fR > fR01)
        {
            return false;
        }

        // axis C0+t*A1xB0
        fR = Math.abs(afAD[0]*aafC[2][0]-afAD[2]*aafC[0][0]);
        fR0 = afEA[0]*aafAbsC[2][0] + afEA[2]*aafAbsC[0][0];
        fR1 = afEB[1]*aafAbsC[1][2] + afEB[2]*aafAbsC[1][1];
        fR01 = fR0 + fR1;
        if (fR > fR01)
        {
            return false;
        }

        // axis C0+t*A1xB1
        fR = Math.abs(afAD[0]*aafC[2][1]-afAD[2]*aafC[0][1]);
        fR0 = afEA[0]*aafAbsC[2][1] + afEA[2]*aafAbsC[0][1];
        fR1 = afEB[0]*aafAbsC[1][2] + afEB[2]*aafAbsC[1][0];
        fR01 = fR0 + fR1;
        if (fR > fR01)
        {
            return false;
        }

        // axis C0+t*A1xB2
        fR = Math.abs(afAD[0]*aafC[2][2]-afAD[2]*aafC[0][2]);
        fR0 = afEA[0]*aafAbsC[2][2] + afEA[2]*aafAbsC[0][2];
        fR1 = afEB[0]*aafAbsC[1][1] + afEB[1]*aafAbsC[1][0];
        fR01 = fR0 + fR1;
        if (fR > fR01)
        {
            return false;
        }

        // axis C0+t*A2xB0
        fR = Math.abs(afAD[1]*aafC[0][0]-afAD[0]*aafC[1][0]);
        fR0 = afEA[0]*aafAbsC[1][0] + afEA[1]*aafAbsC[0][0];
        fR1 = afEB[1]*aafAbsC[2][2] + afEB[2]*aafAbsC[2][1];
        fR01 = fR0 + fR1;
        if (fR > fR01)
        {
            return false;
        }

        // axis C0+t*A2xB1
        fR = Math.abs(afAD[1]*aafC[0][1]-afAD[0]*aafC[1][1]);
        fR0 = afEA[0]*aafAbsC[1][1] + afEA[1]*aafAbsC[0][1];
        fR1 = afEB[0]*aafAbsC[2][2] + afEB[2]*aafAbsC[2][0];
        fR01 = fR0 + fR1;
        if (fR > fR01)
        {
            return false;
        }

        // axis C0+t*A2xB2
        fR = Math.abs(afAD[1]*aafC[0][2]-afAD[0]*aafC[1][2]);
        fR0 = afEA[0]*aafAbsC[1][2] + afEA[1]*aafAbsC[0][2];
        fR1 = afEB[0]*aafAbsC[2][1] + afEB[1]*aafAbsC[2][0];
        fR01 = fR0 + fR1;
        if (fR > fR01)
        {
            return false;
        }

        return true;
    }


    // Dynamic test-intersection query.  The first time of contact (if any)
    // is computed, but not any information about the contact set.
    //virtual bool Test (Real fTMax, const Vector3<Real>& rkVelocity0,
    //const Vector3<Real>& rkVelocity1);

    //using Intersector<Real,Vector3<Real> >::m_fContactTime;

    // Support for dynamic queries.  The inputs are the projection intervals
    // for the boxes onto a potential separating axis, the relative speed of
    // the intervals, and the maximum time for the query.  The outputs are
    // the first time when separating fails and the last time when separation
    // begins again along that axis.  The outputs are *updates* in the sense
    // that this function is called repeatedly for the potential separating
    // axes.  The output first time is updated only if it is larger than
    // the input first time.  The output last time is updated only if it is
    // smaller than the input last time.
    //bool IsSeparated (Real fMin0, Real fMax0, Real fMin1, Real fMax1,
    //Real fSpeed, Real fTMax, Real& rfTFirst, Real& rfTLast);

    // the objects to intersect
    private Box3f m_rkBox0;
    private Box3f m_rkBox1;
}
