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

package gov.nih.mipav.view.WildMagic.LibFoundation.Containment;

import gov.nih.mipav.view.WildMagic.LibFoundation.Approximation.*;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;

public class ContBox3f
{

    // Compute the smallest axis-aligned bounding box of the points.
    public static Box3f ContAlignedBox (int iQuantity, final Vector3f[] akPoint)
    {
        Vector3f kMin = new Vector3f();
        Vector3f kMax = new Vector3f();
        
        Vector3f.ComputeExtremes(iQuantity,akPoint,kMin,kMax);

        Box3f kBox = new Box3f();
        kBox.Center = kMin.add(kMax);
        kBox.Center.scaleEquals((float)0.5);
        Vector3f kHalfDiagonal = kMax.sub(kMin);
        kHalfDiagonal.scaleEquals((float)0.5);
        for (int i = 0; i < 3; i++)
        {
            kBox.Extent[i] = kHalfDiagonal.GetData(i);
        }

        return kBox;
    }

    // Compute an oriented bounding box of the points.  The box center is the
    // average of the points.  The box axes are the eigenvectors of the covariance
    // matrix.
    public static Box3f ContOrientedBox (int iQuantity, final Vector3f[] akPoint)
    {
        Box3f kBox = ApprGaussPointsFit3f.GaussPointsFit3f(iQuantity,akPoint);

        // Let C be the box center and let U0, U1, and U2 be the box axes.  Each
        // input point is of the form X = C + y0*U0 + y1*U1 + y2*U2.  The
        // following code computes min(y0), max(y0), min(y1), max(y1), min(y2),
        // and max(y2).  The box center is then adjusted to be
        //   C' = C + 0.5*(min(y0)+max(y0))*U0 + 0.5*(min(y1)+max(y1))*U1 +
        //        0.5*(min(y2)+max(y2))*U2

        Vector3f kDiff = akPoint[0].sub( kBox.Center );
        Vector3f kMin = new Vector3f(kDiff.Dot(kBox.Axis[0]),kDiff.Dot(kBox.Axis[1]),
                                     kDiff.Dot(kBox.Axis[2]));
        Vector3f kMax = new Vector3f(kMin);
        for (int i = 1; i < iQuantity; i++)
        {
            kDiff = akPoint[i].sub( kBox.Center);
            for (int j = 0; j < 3; j++)
            {
                float fDot = kDiff.Dot(kBox.Axis[j]);
                if (fDot < kMin.GetData(j))
                {
                    kMin.SetData(j, fDot);
                }
                else if (fDot > kMax.GetData(j))
                {
                    kMax.SetData(j, fDot);
                }
            }
        }

        Vector3f kTemp0 = kBox.Axis[0].scale(0.5f*(kMin.X()+kMax.X()));
        Vector3f kTemp1 = kBox.Axis[1].scale(0.5f*(kMin.Y()+kMax.Y()));
        Vector3f kTemp2 = kBox.Axis[2].scale(0.5f*(kMin.Z()+kMax.Z()));
        kBox.Center.addEquals( kTemp0.add(kTemp1.add(kTemp2)) );

        kBox.Extent[0] = 0.5f*(kMax.X() - kMin.X());
        kBox.Extent[1] = 0.5f*(kMax.Y() - kMin.Y());
        kBox.Extent[2] = 0.5f*(kMax.Z() - kMin.Z());

        return kBox;
    }
/*
    // Compute a minimum volume oriented box containing the specified points.
    public static Box3f ContMinBox (int iQuantity, final Vector3[] akPoint)
    {
        Box3f kBox;

        // get the convex hull of the points
        ConvexHull3f kHull(iQuantity,(Vector3f*)akPoint,0.001f,false,
                                Query.QT_INT64);
        int iHDim = kHull.GetDimension();
        int iHQuantity = kHull.GetSimplexQuantity();
        const int* aiHIndex = kHull.GetIndices();

        if (iHDim == 0)
        {
            kBox.Center = akPoint[0];
            kBox.Axis[0] = Vector3f.UNIT_X;
            kBox.Axis[1] = Vector3f.UNIT_Y;
            kBox.Axis[2] = Vector3f.UNIT_Z;
            kBox.Extent[0] = (float)0.0;
            kBox.Extent[1] = (float)0.0;
            kBox.Extent[2] = (float)0.0;
            return kBox;
        }

        if (iHDim == 1)
        {
            kBox.Center = ((float)0.5)*(akPoint[aiHIndex[0]]+akPoint[aiHIndex[1]]);
            Vector3f kDiff = akPoint[aiHIndex[1]] - akPoint[aiHIndex[0]];
            kBox.Extent[0] = ((float)0.5)*kDiff.Normalize();
            kBox.Extent[1] = (float)0.0;
            kBox.Extent[2] = (float)0.0;
            kBox.Axis[0] = kDiff;
            Vector3f.GenerateComplementBasis(kBox.Axis[1],kBox.Axis[2],
                                                   kBox.Axis[0]);
            return kBox;
        }

        int i, j;
        Vector3f kOrigin, kDiff, kU, kV, kW;
        Vector2f* akPoint2;
        Box2f kBox2;

        if (iHDim == 2)
        {
            // get a coordinate system relative to the plane
            kOrigin = kHull.GetPlaneOrigin();
            kW = kHull.GetPlaneDirection(0).Cross(kHull.GetPlaneDirection(1));
            Vector3f.GenerateComplementBasis(kU,kV,kW);

            // project convex hull points onto plane
            akPoint2 = WM4_NEW Vector2f[iHQuantity];
            for (i = 0; i < iHQuantity; i++)
            {
                kDiff = akPoint[aiHIndex[i]] - kOrigin;
                akPoint2[i].X() = kU.Dot(kDiff);
                akPoint2[i].Y() = kV.Dot(kDiff);
            }

            // compute minimum area box in 2D
            kBox2 = ContMinBoxf(iHQuantity,akPoint2,false);
            WM4_DELETE[] akPoint2;

            // lift the values into 3D
            kBox.Center = kOrigin + kBox2.Center.X()*kU + kBox2.Center.Y()*kV;
            kBox.Axis[0] = kBox2.Axis[0].X()*kU + kBox2.Axis[0].Y()*kV;
            kBox.Axis[1] = kBox2.Axis[1].X()*kU + kBox2.Axis[1].Y()*kV;
            kBox.Axis[2] = kW;
            kBox.Extent[0] = kBox2.Extent[0];
            kBox.Extent[1] = kBox2.Extent[1];
            kBox.Extent[2] = (float)0.0;
            return kBox;
        }

        float fVolume, fMinVolume = Mathf.MAX_REAL;

        // Use the rotating calipers method on the projection of the hull onto
        // the plane of each face.  Also project the hull onto the normal line
        // of each face.  The minimum area box in the plane and the height on
        // the line produce a containing box.  If its volume is smaller than the
        // current volume, this box is the new candidate for the minimum volume
        // box.  The unique edges are accumulated into a set for use by a later
        // step in the algorithm.
        const int* piIndex = aiHIndex;
        float fHeight, fMinHeight, fMaxHeight;
        std.set<EdgeKey> kEdges;
        akPoint2 = WM4_NEW Vector2f[iHQuantity];
        int iTQuantity = iHQuantity/3;  // number of triangles
        for (i = 0; i < iTQuantity; i++)
        {
            // get triangle
            int iV0 = *piIndex++;
            int iV1 = *piIndex++;
            int iV2 = *piIndex++;

            // save the edges for later use
            kEdges.insert(EdgeKey(iV0,iV1));
            kEdges.insert(EdgeKey(iV1,iV2));
            kEdges.insert(EdgeKey(iV2,iV0));

            // get 3D coordinate system relative to plane of triangle
            kOrigin = (akPoint[iV0] + akPoint[iV1] + akPoint[iV2])/(float)3.0;
            Vector3f kEdge1 = akPoint[iV1] - akPoint[iV0];
            Vector3f kEdge2 = akPoint[iV2] - akPoint[iV0];
            kW = kEdge2.UnitCross(kEdge1);  // inner-pointing normal
            if (kW == Vector3f.ZERO)
            {
                // The triangle is needle-like, so skip it.
                continue;
            }
            Vector3f.GenerateComplementBasis(kU,kV,kW);

            // Project points onto plane of triangle, onto normal line of plane.
            // TO DO.  In theory, minHeight should be zero since W points to the
            // interior of the hull.  However, the snap rounding used in the 3D
            // convex hull finder involves loss of precision, which in turn can
            // cause a hull facet to have the wrong ordering (clockwise instead
            // of counterclockwise when viewed from outside the hull).  The
            // height calculations here trap that problem (the incorrectly ordered
            // face will not affect the minimum volume box calculations).
            fMinHeight = (float)0.0;
            fMaxHeight = (float)0.0;
            for (j = 0; j < iHQuantity; j++)
            {
                kDiff = akPoint[aiHIndex[j]] - kOrigin;
                akPoint2[j].X() = kU.Dot(kDiff);
                akPoint2[j].Y() = kV.Dot(kDiff);
                fHeight = kW.Dot(kDiff);
                if (fHeight > fMaxHeight)
                {
                    fMaxHeight = fHeight;
                }
                else if (fHeight < fMinHeight)
                {
                    fMinHeight = fHeight;
                }
            }
            if (-fMinHeight > fMaxHeight)
            {
                fMaxHeight = -fMinHeight;
            }

            // compute minimum area box in 2D
            kBox2 = ContMinBoxf(iHQuantity,akPoint2,false);

            // update current minimum-volume box (if necessary)
            fVolume = fMaxHeight*kBox2.Extent[0]*kBox2.Extent[1];
            if (fVolume < fMinVolume)
            {
                fMinVolume = fVolume;

                // lift the values into 3D
                kBox.Extent[0] = kBox2.Extent[0];
                kBox.Extent[1] = kBox2.Extent[1];
                kBox.Extent[2] = ((float)0.5)*fMaxHeight;
                kBox.Axis[0] = kBox2.Axis[0].X()*kU + kBox2.Axis[0].Y()*kV;
                kBox.Axis[1] = kBox2.Axis[1].X()*kU + kBox2.Axis[1].Y()*kV;
                kBox.Axis[2] = kW;
                kBox.Center = kOrigin + kBox2.Center.X()*kU + kBox2.Center.Y()*kV
                    + kBox.Extent[2]*kW;
            }
        }

        // The minimum-volume box can also be supported by three mutually
        // orthogonal edges of the convex hull.  For each triple of orthogonal
        // edges, compute the minimum-volume box for that coordinate frame by
        // projecting the points onto the axes of the frame.
        std.set<EdgeKey>.const_iterator pkE2;
        for (pkE2 = kEdges.begin(); pkE2 != kEdges.end(); pkE2++)
        {
            kW = akPoint[pkE2.V[1]] - akPoint[pkE2.V[0]];
            kW.Normalize();

            std.set<EdgeKey>.const_iterator pkE1 = pkE2;
            for (++pkE1; pkE1 != kEdges.end(); pkE1++)
            {
                kV = akPoint[pkE1.V[1]] - akPoint[pkE1.V[0]];
                kV.Normalize();
                float fDot = kV.Dot(kW);
                if (Mathf.FAbs(fDot) > Mathf.ZERO_TOLERANCE)
                {
                    continue;
                }

                std.set<EdgeKey>.const_iterator pkE0 = pkE1;
                for (++pkE0; pkE0 != kEdges.end(); pkE0++)
                {
                    kU = akPoint[pkE0.V[1]] - akPoint[pkE0.V[0]];
                    kU.Normalize();
                    fDot = kU.Dot(kV);
                    if (Mathf.FAbs(fDot) > Mathf.ZERO_TOLERANCE)
                    {
                        continue;
                    }
                    fDot = kU.Dot(kW);
                    if (Mathf.FAbs(fDot) > Mathf.ZERO_TOLERANCE)
                    {
                        continue;
                    }
    
                    // The three edges are mutually orthogonal.  Project the
                    // hull points onto the lines containing the edges.  Use
                    // hull point zero as the origin.
                    float fUMin = (float)0.0, fUMax = (float)0.0;
                    float fVMin = (float)0.0, fVMax = (float)0.0;
                    float fWMin = (float)0.0, fWMax = (float)0.0;
                    kOrigin = akPoint[aiHIndex[0]];
                    for (j = 1; j < iHQuantity; j++)
                    {
                        kDiff = akPoint[aiHIndex[j]] - kOrigin;

                        float fU = kU.Dot(kDiff);
                        if (fU < fUMin)
                        {
                            fUMin = fU;
                        }
                        else if (fU > fUMax)
                        {
                            fUMax = fU;
                        }

                        float fV = kV.Dot(kDiff);
                        if (fV < fVMin)
                        {
                            fVMin = fV;
                        }
                        else if (fV > fVMax)
                        {
                            fVMax = fV;
                        }

                        float fW = kW.Dot(kDiff);
                        if (fW < fWMin)
                        {
                            fWMin = fW;
                        }
                        else if (fW > fWMax)
                        {
                            fWMax = fW;
                        }
                    }

                    float fUExtent = ((float)0.5)*(fUMax - fUMin);
                    float fVExtent = ((float)0.5)*(fVMax - fVMin);
                    float fWExtent = ((float)0.5)*(fWMax - fWMin);

                    // update current minimum-volume box (if necessary)
                    fVolume = fUExtent*fVExtent*fWExtent;
                    if (fVolume < fMinVolume)
                    {
                        fMinVolume = fVolume;

                        kBox.Extent[0] = fUExtent;
                        kBox.Extent[1] = fVExtent;
                        kBox.Extent[2] = fWExtent;
                        kBox.Axis[0] = kU;
                        kBox.Axis[1] = kV;
                        kBox.Axis[2] = kW;
                        kBox.Center = kOrigin +
                            ((float)0.5)*(fUMin+fUMax)*kU +
                            ((float)0.5)*(fVMin+fVMax)*kV +
                            ((float)0.5)*(fWMin+fWMax)*kW;
                    }
                }
            }
        }

        WM4_DELETE[] akPoint2;
        return kBox;
    }
*/

    // Test for containment.  Let X = C + y0*U0 + y1*U1 + y2*U2 where C is the
    // box center and U0, U1, U2 are the orthonormal axes of the box.  X is in
    // the box if |y_i| <= E_i for all i where E_i are the extents of the box.
    public static boolean InBox (final Vector3f rkPoint, final Box3f rkBox)
    {
        Vector3f kDiff = rkPoint.sub( rkBox.Center );
        for (int i = 0; i < 3; i++)
        {
            float fCoeff = kDiff.Dot(rkBox.Axis[i]);
            if ((Math.abs(fCoeff) - rkBox.Extent[i]) > Mathf.ZERO_TOLERANCE)
            {
                return false;
            }
        }
        return true;
    }

    // Construct an oriented box that contains two other oriented boxes.  The
    // result is not guaranteed to be the minimum volume box containing the
    // input boxes.
    public static Box3f MergeBoxes (final Box3f rkBox0, final Box3f rkBox1)
    {
        // construct a box that contains the input boxes
        Box3f kBox = new Box3f();

        // The first guess at the box center.  This value will be updated later
        // after the input box vertices are projected onto axes determined by an
        // average of box axes.
        kBox.Center = rkBox0.Center.add(rkBox1.Center);
        kBox.Center.scaleEquals((float)0.5);

        // A box's axes, when viewed as the columns of a matrix, form a rotation
        // matrix.  The input box axes are converted to quaternions.  The average
        // quaternion is computed, then normalized to unit length.  The result is
        // the slerp of the two input quaternions with t-value of 1/2.  The result
        // is converted back to a rotation matrix and its columns are selected as
        // the merged box axes.
        Quaternion kQ0 = new Quaternion();
        Quaternion kQ1 = new Quaternion();
        kQ0.FromRotationMatrix(rkBox0.Axis);
        kQ1.FromRotationMatrix(rkBox1.Axis);
        if (kQ0.Dot(kQ1) < (float)0.0)
        {
            kQ1 = kQ1.neg();
        }

        Quaternion kQ = kQ0.add( kQ1 );
        float fInvLength = Mathf.InvSqrt(kQ.Dot(kQ));
        kQ = kQ.scale(fInvLength);
        kQ.ToRotationMatrix(kBox.Axis);

        // Project the input box vertices onto the merged-box axes.  Each axis
        // D[i] containing the current center C has a minimum projected value
        // pmin[i] and a maximum projected value pmax[i].  The corresponding end
        // points on the axes are C+pmin[i]*D[i] and C+pmax[i]*D[i].  The point C
        // is not necessarily the midpoint for any of the intervals.  The actual
        // box center will be adjusted from C to a point C' that is the midpoint
        // of each interval,
        //   C' = C + sum_{i=0}^2 0.5*(pmin[i]+pmax[i])*D[i]
        // The box extents are
        //   e[i] = 0.5*(pmax[i]-pmin[i])

        int i, j;
        float fDot;
        Vector3f[] akVertex = new Vector3f[8];
        Vector3f kDiff;
        Vector3f kMin = Vector3f.ZERO;
        Vector3f kMax = Vector3f.ZERO;

        rkBox0.ComputeVertices(akVertex);
        for (i = 0; i < 8; i++)
        {
            kDiff = akVertex[i].sub( kBox.Center );
            for (j = 0; j < 3; j++)
            {
                fDot = kDiff.Dot(kBox.Axis[j]);
                if (fDot > kMax.GetData(j))
                {
                    kMax.SetData(j, fDot);
                }
                else if (fDot < kMin.GetData(j))
                {
                    kMin.SetData(j, fDot);
                }
            }
        }

        rkBox1.ComputeVertices(akVertex);
        for (i = 0; i < 8; i++)
        {
            kDiff = akVertex[i].sub( kBox.Center );
            for (j = 0; j < 3; j++)
            {
                fDot = kDiff.Dot(kBox.Axis[j]);
                if (fDot > kMax.GetData(j))
                {
                    kMax.SetData(j, fDot);
                }
                else if (fDot < kMin.GetData(j))
                {
                    kMin.SetData(j, fDot);
                }
            }
        }

        // [kMin,kMax] is the axis-aligned box in the coordinate system of the
        // merged box axes.  Update the current box center to be the center of
        // the new box.  Compute the extents based on the new center.
        for (j = 0; j < 3; j++)
        {
            // (((float)0.5)*(kMax[j]+kMin[j]))*kBox.Axis[j];
            Vector3f kTemp = kBox.Axis[j].scale((((float)0.5)*(kMax.GetData(j)+kMin.GetData(j))));
            kBox.Center.addEquals(kTemp);
            kBox.Extent[j] = ((float)0.5)*(kMax.GetData(j)-kMin.GetData(j));
        }

        return kBox;
    }

}
