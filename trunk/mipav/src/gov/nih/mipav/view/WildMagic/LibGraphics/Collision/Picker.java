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
// Version: 4.0.0 (2006/08/07)

package gov.nih.mipav.view.WildMagic.LibGraphics.Collision;

import java.util.Vector;

import gov.nih.mipav.view.WildMagic.LibFoundation.Intersection.*;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;

public class Picker
{
    public Picker ()
    {
        m_kOrigin = new Vector3f(Vector3f.ZERO);
        m_kDirection = new Vector3f(Vector3f.ZERO);
        m_fTMin = 0.0f;
        m_fTMax = 0.0f;
    }

    /*
     * The linear component is parameterized by P + t*D, where P is a point on
     * the component (P is the origin), D is a unit-length direction, and t is
     * a scalar in the interval [tmin,tmax] with tmin < tmax.  The P and D
     * values must be in world coordinates.  The choices for tmin and tmax are
     *  line:     tmin = -Mathf::MAX_REAL, tmax = Mathf::MAX_REAL
     *  ray:      tmin = 0, tmax = Mathf::MAX_REAL
     *  segment:  tmin = 0, tmax > 0;
     *
     * A call to this function will automatically clear the Records array.  If
     * you need any information from this array obtained by a previous call to
     * Execute, you must save it first.
     */
    public void Execute (Spatial pkScene, final Vector3f rkOrigin,
                         final Vector3f rkDirection, float fTMin, float fTMax)
    {
        m_kOrigin = rkOrigin;
        m_kDirection = rkDirection;
        m_fTMin = fTMin;
        m_fTMax = fTMax;
        Records.clear();
        pkScene.UpdateGS();
        ExecuteRecursive(pkScene);
    }

    // The following three functions return the index of the record satisfying
    // the constraints.  They should be called only if Records.size() > 0.
    // The index i satisfies 0 <= i < Records.size().

    // Locate the record whose T value is smallest in absolute value.
    public final PickRecord GetClosestToZero ()
    {
        if (Records.size() == 0)
        {
            return ms_kInvalid;
        }

        float fClosest = Math.abs(Records.get(0).T);
        int iClosest = 0;
        for (int i = 1; i < (int)Records.size(); i++)
        {
            float fTmp = Math.abs(Records.get(i).T);
            if (fTmp < fClosest)
            {
                fClosest = fTmp;
                iClosest = i;
            }
        }
        return Records.get(iClosest);
    }


    // Locate the record with nonnegative T value closest to zero.
    public final PickRecord GetClosestNonnegative ()
    {
        if (Records.size() == 0)
        {
            return ms_kInvalid;
        }

        // Get first nonnegative value.
        float fClosest = Float.MAX_VALUE;
        int iClosest;
        for (iClosest = 0; iClosest < (int)Records.size(); iClosest++)
        {
            if (Records.get(iClosest).T >= 0.0f)
            {
                fClosest = Records.get(iClosest).T;
                break;
            }
        }
        if (iClosest == (int)Records.size())
        {
            // All values are negative.
            return ms_kInvalid;
        }

        for (int i = iClosest+1; i < (int)Records.size(); i++)
        {
            if (0.0f <= Records.get(i).T && Records.get(i).T < fClosest)
            {
                fClosest = Records.get(i).T;
                iClosest = i;
            }
        }
        return Records.get(iClosest);
    }


    // Locate the record with nonpositive T value closest to zero.
    public final PickRecord GetClosestNonpositive ()
    {
        if (Records.size() == 0)
        {
            return ms_kInvalid;
        }

        // Get first nonpositive value.
        float fClosest = -Float.MAX_VALUE;
        int iClosest;
        for (iClosest = 0; iClosest < (int)Records.size(); iClosest++)
        {
            if (Records.get(iClosest).T <= 0.0f)
            {
                fClosest = Records.get(iClosest).T;
                break;
            }
        }
        if (iClosest == (int)Records.size())
        {
            // All values are positive.
            return ms_kInvalid;
        }

        for (int i = iClosest+1; i < (int)Records.size(); i++)
        {
            if (fClosest < Records.get(i).T && Records.get(i).T <= 0.0f)
            {
                fClosest = Records.get(i).T;
                iClosest = i;
            }
        }
        return Records.get(iClosest);
    }

    public Vector<PickRecord> Records = new Vector<PickRecord>();

    // The picking occurs recursively by traversing the input scene.
    private void ExecuteRecursive (Spatial pkObject)
    {
        if (pkObject instanceof Triangles)
        {
            boolean bTestMesh = true;
            if ( pkObject instanceof HierarchicalTriMesh )
            {
                HierarchicalTriMesh pkHMesh = (HierarchicalTriMesh)pkObject;
                if ( pkHMesh.GetBoundingVolumeTree() != null )
                {
                    Vector<PickRecord> addRecords = pkHMesh.TestIntersection(m_kOrigin,m_kDirection,
                            m_fTMin,m_fTMax);
                    if ( addRecords != null )
                    {
                        for ( int i = 0; i < addRecords.size(); i++ )
                        {
                            Records.add(addRecords.get(i));
                        }
                    }
                    bTestMesh = false;
                }
            }
            if ( bTestMesh )
            {
                Triangles pkMesh = (Triangles)(pkObject);
                if (pkMesh.WorldBound.TestIntersection(m_kOrigin,m_kDirection,
                        m_fTMin,m_fTMax))
                {
                    // Convert the linear component to model-space coordinates.
                    Line3f kLine = new Line3f(pkMesh.World.ApplyInverse(m_kOrigin),
                            pkMesh.World.InvertVector(m_kDirection));

                    // Compute intersections with the model-space triangles.
                    int iTQuantity = pkMesh.GetTriangleQuantity();
                    for (int i = 0; i < iTQuantity; i++)
                    {
                        int iV0, iV1, iV2;
                        int[] aiTris = new int[3];
                        if (!pkMesh.GetTriangle(i,aiTris) )
                        {
                            continue;
                        }

                        iV0 = aiTris[0];            iV1 = aiTris[1];            iV2 = aiTris[2];
                        Triangle3f kTriangle = new Triangle3f(
                                pkMesh.VBuffer.GetPosition3(iV0),
                                pkMesh.VBuffer.GetPosition3(iV1),
                                pkMesh.VBuffer.GetPosition3(iV2));

                        IntrLine3Triangle3f kIntr = new IntrLine3Triangle3f(kLine,kTriangle);
                        if (kIntr.Find() && m_fTMin <= kIntr.GetLineT()
                                &&  kIntr.GetLineT() <= m_fTMax)
                        {
                            PickRecord kRecord = new PickRecord();
                            kRecord.Intersected = pkMesh;
                            kRecord.T = kIntr.GetLineT();
                            kRecord.Triangle = i;
                            kRecord.B0 = kIntr.GetTriB0();
                            kRecord.B1 = kIntr.GetTriB1();
                            kRecord.B2 = kIntr.GetTriB2();
                            Records.add(kRecord);
                        }
                    }
                }
            }
            return;
        }

//         if (pkObject instanceof SwitchNode )
//         {
//             SwitchNode pkSwitchNode = (SwitchNode)(pkObject);
//             int iActiveChild = pkSwitchNode.GetActiveChild();
//             if (iActiveChild != SwitchNode.SN_INVALID_CHILD)
//             {
//                 if (pkSwitchNode.WorldBound.TestIntersection(m_kOrigin,
//                                                                m_kDirection,m_fTMin,m_fTMax))
//                 {
//                     Spatial pkChild = pkSwitchNode.GetChild(iActiveChild);
//                     if (pkChild)
//                     {
//                         ExecuteRecursive(pkChild);
//                     }
//                 }
//             }
//             return;
//         }

        if (pkObject instanceof Node)
        {
            Node pkNode = (Node)(pkObject);
            if (pkNode.WorldBound.TestIntersection(m_kOrigin,m_kDirection,
                                                     m_fTMin,m_fTMax))
            {
                for (int i = 0; i < pkNode.GetQuantity(); i++)
                {
                    Spatial pkChild = pkNode.GetChild(i);
                    if (pkChild != null)
                    {
                        ExecuteRecursive(pkChild);
                    }
                }
            }
        }
    }


    private Vector3f m_kOrigin, m_kDirection;
    private float m_fTMin, m_fTMax;

    // The value returned if the Get* functions are called when Records has
    // no elements.
    private static final PickRecord ms_kInvalid = null;
}
