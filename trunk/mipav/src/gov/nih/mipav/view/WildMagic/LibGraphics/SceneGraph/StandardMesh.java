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

package gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;

public class StandardMesh
{
    /** Create a StandardMesh with the input attributes.
     * @param rkAttr, attributes to apply to the mesh.
     */
    public StandardMesh ( Attributes rkAttr )
    {
        m_kAttr = rkAttr;
        assert(m_kAttr.GetPChannels() == 3);
        if (m_kAttr.HasNormal())
        {
            assert(m_kAttr.GetNChannels() == 3);
        }

        for (int iUnit = 0; iUnit < m_kAttr.GetMaxTCoords(); iUnit++)
        {
            if (m_kAttr.HasTCoord(iUnit))
            {
                assert(m_kAttr.GetTChannels(iUnit) == 2);
            }
        }
        m_bInside = false;
    }

    /** Create a StandardMesh with the input attributes.
     * @param rkAttr, attributes to apply to the mesh.
     * @param bInside, true if view point is inside mesh.
     * @param pkXFrm, Transformation to apply to mesh.
     */
    public StandardMesh ( Attributes rkAttr, boolean bInside,
                          final Transformation pkXFrm )
    {
        m_kAttr = rkAttr;
        assert(m_kAttr.GetPChannels() == 3);
        if (m_kAttr.HasNormal())
        {
            assert(m_kAttr.GetNChannels() == 3);
        }

        for (int iUnit = 0; iUnit < m_kAttr.GetMaxTCoords(); iUnit++)
        {
            if (m_kAttr.HasTCoord(iUnit))
            {
                assert(m_kAttr.GetTChannels(iUnit) == 2);
            }
        }

        if (pkXFrm != null)
        {
            m_kXFrm = pkXFrm;
        }
        m_bInside = false;
    }

    /** Delete memory. */
    public void finalize()
    {
        if ( m_kAttr != null )
        {
            m_kAttr.finalize();
            m_kAttr = null;
        }
        if ( m_kXFrm != null )
        {
            m_kXFrm.finalize();
            m_kXFrm = null;
        }
    }

    /** Set mesh transformation.
     * @param rkXFrm, new mesh transformation.
     */
    public void SetTransformation (final Transformation rkXFrm)
    {
        m_kXFrm = rkXFrm;
    }

    /** Get mesh transformation.
     * @return mesh transformation.
     */
    public Transformation GetTransformation ()
    {
        return m_kXFrm;
    }

    /** Standard meshes.  Each mesh is centered at (0,0,0) and has an up-axis
     * of (0,0,1).  The other axes forming the coordinate system are (1,0,0)
     * and (0,1,0).  An application may transform the meshes as necessary.
     * @param iXSamples, number of x-samples in mesh.
     * @param iYSamples, number of y-samples in mesh.
     * @param fXExtent, x-extent of rectangle.
     * @param fYExtent, y-extent of rectangle.
     * @return Rectangle TriMesh.
     */
    public TriMesh Rectangle (int iXSamples, int iYSamples,
                              float fXExtent, float fYExtent)
    {
        int iVQuantity = iXSamples*iYSamples;
        int iTQuantity = 2*(iXSamples-1)*(iYSamples-1);
        VertexBuffer pkVB = new VertexBuffer(m_kAttr,iVQuantity);
        IndexBuffer pkIB = new IndexBuffer(3*iTQuantity);

        // generate geometry
        float fInv0 = 1.0f/(iXSamples - 1.0f);
        float fInv1 = 1.0f/(iYSamples - 1.0f);
        float fU, fV;
        int i, i0, i1;
        for (i1 = 0, i = 0; i1 < iYSamples; i1++)
        {
            fV = i1*fInv1;
            Vector3f kYTmp = new Vector3f(Vector3f.UNIT_Y).scale(((2.0f*fV-1.0f)*fYExtent));
            for (i0 = 0; i0 < iXSamples; i0++)
            {
                fU = i0*fInv0;
                Vector3f kXTmp = new Vector3f(Vector3f.UNIT_X).scale(((2.0f*fU-1.0f)*fXExtent));
                pkVB.Position3(i, kXTmp.add( kYTmp ));

                if (m_kAttr.HasNormal())
                {
                    pkVB.Normal3(i, new Vector3f(Vector3f.UNIT_Z));
                }

                if (m_kAttr.GetMaxTCoords() > 0)
                {
                    Vector2f kTCoord = new Vector2f(fU,fV);
                    for (int iUnit = 0; iUnit < m_kAttr.GetMaxTCoords(); iUnit++)
                    {
                        if (m_kAttr.HasTCoord(iUnit))
                        {
                            pkVB.TCoord2(iUnit,i, kTCoord);
                        }
                    }
                }

                i++;
            }
        }

        // generate connectivity
        int[] aiIndex = pkIB.GetData();
        for (i1 = 0, i = 0; i1 < iYSamples - 1; i1++)
        {
            for (i0 = 0; i0 < iXSamples - 1; i0++)
            {
                int iV0 = i0 + iXSamples * i1;
                int iV1 = iV0 + 1;
                int iV2 = iV1 + iXSamples;
                int iV3 = iV0 + iXSamples;
                aiIndex[i++] = iV0;
                aiIndex[i++] = iV1;
                aiIndex[i++] = iV2;
                aiIndex[i++] = iV0;
                aiIndex[i++] = iV2;
                aiIndex[i++] = iV3;
            }
        }

        TransformData(pkVB);
        TriMesh pkMesh = new TriMesh(pkVB,pkIB);
        return pkMesh;
    }

    
    /** Standard meshes.  Each mesh is centered at (0,0,0) and has an up-axis
     * of (0,0,1).  The other axes forming the coordinate system are (1,0,0)
     * and (0,1,0).  An application may transform the meshes as necessary.
     * @param iShellSamples, number of shell samples in mesh.
     * @param iRadialSamples, number of radial samples.
     * @param fRadius, radius of the Disk.
     * @return Disk TriMesh.
     */
    public TriMesh Disk (int iShellSamples, int iRadialSamples, float fRadius)
    {
        int iRSm1 = iRadialSamples - 1, iSSm1 = iShellSamples - 1;
        int iVQuantity = 1 + iRadialSamples*iSSm1;
        int iTQuantity = iRadialSamples*(2*iSSm1-1);
        VertexBuffer pkVB = new VertexBuffer(m_kAttr,iVQuantity);
        IndexBuffer pkIB = new IndexBuffer(3*iTQuantity);

        // generate geometry
        int iR, iS, i, iUnit;
        Vector2f kTCoord;

        // center of disk
        pkVB.Position3(0, Vector3f.ZERO);
        if (m_kAttr.HasNormal())
        {
            pkVB.Normal3(0, Vector3f.UNIT_Z);
        }

        if (m_kAttr.GetMaxTCoords() > 0)
        {
            kTCoord = new Vector2f(0.5f,0.5f);
            for (iUnit = 0; iUnit < m_kAttr.GetMaxTCoords(); iUnit++)
            {
                if (m_kAttr.HasTCoord(iUnit))
                {
                    pkVB.TCoord2(iUnit,0, kTCoord);
                }
            }
        }

        float fInvSSm1 = 1.0f/(float)iSSm1;
        float fInvRS = 1.0f/(float)iRadialSamples;
        for (iR = 0; iR < iRadialSamples; iR++)
        {
            float fAngle = (float)Mathf.TWO_PI*fInvRS*iR;
            float fCos = (float)Math.cos(fAngle);
            float fSin = (float)Math.sin(fAngle);
            Vector3f kRadial = new Vector3f(fCos,fSin,0.0f);

            for (iS = 1; iS < iShellSamples; iS++)
            {
                float fFraction = fInvSSm1*iS;  // in (0,R]
                Vector3f kFracRadial = kRadial.scale(fFraction);
                i = iS+iSSm1*iR;
                pkVB.Position3(i, kFracRadial.scale(fRadius));
                if (m_kAttr.HasNormal())
                {
                    pkVB.Normal3(i, Vector3f.UNIT_Z);
                }

                if (m_kAttr.GetMaxTCoords() > 0)
                {
                    kTCoord = new Vector2f(1.0f+kFracRadial.X(),
                                            1.0f+kFracRadial.Y());
                    kTCoord.scale(0.5f);
                    for (iUnit = 0; iUnit < m_kAttr.GetMaxTCoords(); iUnit++)
                    {
                        if (m_kAttr.HasTCoord(iUnit))
                        {
                            pkVB.TCoord2(iUnit,i, kTCoord);
                        }
                    }
                }
            }
        }

        // generate connectivity
        int[] aiLocalIndex = pkIB.GetData();
        int iIndex = 0;
        int iT = 0;
        for (int iR0 = iRSm1, iR1 = 0; iR1 < iRadialSamples; iR0 = iR1++)
        {
            aiLocalIndex[iIndex++] = 0;
            aiLocalIndex[iIndex++] = 1+iSSm1*iR0;
            aiLocalIndex[iIndex++] = 1+iSSm1*iR1;
            //aiLocalIndex += 3;
            iT++;
            for ( iS = 1; iS < iSSm1; iS++)
            {
                int i00 = iS+iSSm1*iR0;
                int i01 = iS+iSSm1*iR1;
                int i10 = i00+1;
                int i11 = i01+1;
                aiLocalIndex[iIndex++] = i00;
                aiLocalIndex[iIndex++] = i10;
                aiLocalIndex[iIndex++] = i11;
                aiLocalIndex[iIndex++] = i00;
                aiLocalIndex[iIndex++] = i11;
                aiLocalIndex[iIndex++] = i01;
                iT += 2;
            }
        }
        assert(iT == iTQuantity);

        TransformData(pkVB);
        TriMesh pkMesh = new TriMesh(pkVB,pkIB);
        return pkMesh;
    }

    /** Standard meshes.  Each mesh is centered at (0,0,0) and has an up-axis
     * of (0,0,1).  The other axes forming the coordinate system are (1,0,0)
     * and (0,1,0).  An application may transform the meshes as necessary.
     * @param fXExtent, x-extent of box.
     * @param fYExtent, y-extent of box.
     * @param fZExtent, z-extent of box.
     * @return Box TriMesh.
     */
    public TriMesh Box (float fXExtent, float fYExtent, float fZExtent)
    {
        int iVQuantity = 8;
        int iTQuantity = 12;
        VertexBuffer pkVB = new VertexBuffer(m_kAttr,iVQuantity);
        IndexBuffer pkIB = new IndexBuffer(3*iTQuantity);

        // generate geometry
        pkVB.Position3(0, new Vector3f(-fXExtent,-fYExtent,-fZExtent));
        pkVB.Position3(1, new Vector3f(+fXExtent,-fYExtent,-fZExtent));
        pkVB.Position3(2, new Vector3f(+fXExtent,+fYExtent,-fZExtent));
        pkVB.Position3(3, new Vector3f(-fXExtent,+fYExtent,-fZExtent));
        pkVB.Position3(4, new Vector3f(-fXExtent,-fYExtent,+fZExtent));
        pkVB.Position3(5, new Vector3f(+fXExtent,-fYExtent,+fZExtent));
        pkVB.Position3(6, new Vector3f(+fXExtent,+fYExtent,+fZExtent));
        pkVB.Position3(7, new Vector3f(-fXExtent,+fYExtent,+fZExtent));

        if (m_kAttr.GetMaxTCoords() > 0)
        {
            for (int iUnit = 0; iUnit < m_kAttr.GetMaxTCoords(); iUnit++)
            {
                if (m_kAttr.HasTCoord(iUnit))
                {
                    pkVB.TCoord2(iUnit,0, new Vector2f(0.25f,0.75f));
                    pkVB.TCoord2(iUnit,1, new Vector2f(0.75f,0.75f));
                    pkVB.TCoord2(iUnit,2, new Vector2f(0.75f,0.25f));
                    pkVB.TCoord2(iUnit,3, new Vector2f(0.25f,0.25f));
                    pkVB.TCoord2(iUnit,4, new Vector2f(0.0f,1.0f));
                    pkVB.TCoord2(iUnit,5, new Vector2f(1.0f,1.0f));
                    pkVB.TCoord2(iUnit,6, new Vector2f(1.0f,0.0f));
                    pkVB.TCoord2(iUnit,7, new Vector2f(0.0f,0.0f));
                }
            }
        }

        // generate connectivity (outside view)
        int[] aiIndex = pkIB.GetData();
        aiIndex[ 0] = 0;  aiIndex[ 1] = 2;  aiIndex[ 2] = 1;
        aiIndex[ 3] = 0;  aiIndex[ 4] = 3;  aiIndex[ 5] = 2;
        aiIndex[ 6] = 0;  aiIndex[ 7] = 1;  aiIndex[ 8] = 5;
        aiIndex[ 9] = 0;  aiIndex[10] = 5;  aiIndex[11] = 4;
        aiIndex[12] = 0;  aiIndex[13] = 4;  aiIndex[14] = 7;
        aiIndex[15] = 0;  aiIndex[16] = 7;  aiIndex[17] = 3;
        aiIndex[18] = 6;  aiIndex[19] = 4;  aiIndex[20] = 5;
        aiIndex[21] = 6;  aiIndex[22] = 7;  aiIndex[23] = 4;
        aiIndex[24] = 6;  aiIndex[25] = 5;  aiIndex[26] = 1;
        aiIndex[27] = 6;  aiIndex[28] = 1;  aiIndex[29] = 2;
        aiIndex[30] = 6;  aiIndex[31] = 2;  aiIndex[32] = 3;
        aiIndex[33] = 6;  aiIndex[34] = 3;  aiIndex[35] = 7;

        if (m_bInside)
        {
            ReverseTriangleOrder(iTQuantity,aiIndex);
        }

        TransformData(pkVB);
        TriMesh pkMesh = new TriMesh(pkVB,pkIB);
        pkMesh.UpdateMS(true);
        return pkMesh;
    }

    /** Standard meshes.  Each mesh is centered at (0,0,0) and has an up-axis
     * of (0,0,1).  The other axes forming the coordinate system are (1,0,0)
     * and (0,1,0).  An application may transform the meshes as necessary.
     * @param iAxisSamples, number of axis samples.
     * @param iRadialSamples, number of radial samples.
     * @param fRadius, cylinder radius.
     * @param fHeight, cylinder height.
     * @param bOpen, true = open cylinder, false = closed cylinder.
     * @return Cylinder TriMesh.
     */
    public TriMesh Cylinder (int iAxisSamples, int iRadialSamples, float fRadius,
                             float fHeight, boolean bOpen)
    {
        TriMesh pkMesh;

        if (bOpen)
        {
            int iVQuantity = iAxisSamples*(iRadialSamples+1);
            int iTQuantity = 2*(iAxisSamples-1)*iRadialSamples;
            VertexBuffer pkVB = new VertexBuffer(m_kAttr,iVQuantity);
            IndexBuffer pkIB = new IndexBuffer(3*iTQuantity);

            // generate geometry
            float fInvRS = 1.0f/(float)iRadialSamples;
            float fInvASm1 = 1.0f/(float)(iAxisSamples-1);
            float fHalfHeight = 0.5f*fHeight;
            int iR, iA, iAStart, i, iUnit;
            Vector2f kTCoord;

            // Generate points on the unit circle to be used in computing the
            // mesh points on a cylinder slice.
            float[] afSin = new float[iRadialSamples+1];
            float[] afCos = new float[iRadialSamples+1];
            for (iR = 0; iR < iRadialSamples; iR++)
            {
                float fAngle = Mathf.TWO_PI*fInvRS*iR;
                afCos[iR] = (float)Math.cos(fAngle);
                afSin[iR] = (float)Math.sin(fAngle);
            }
            afSin[iRadialSamples] = afSin[0];
            afCos[iRadialSamples] = afCos[0];

            // generate the cylinder itself
            for (iA = 0, i = 0; iA < iAxisSamples; iA++)
            {
                float fAxisFraction = iA*fInvASm1;  // in [0,1]
                float fZ = -fHalfHeight + fHeight*fAxisFraction;

                // compute center of slice
                Vector3f kSliceCenter = new Vector3f(0.0f,0.0f,fZ);

                // compute slice vertices with duplication at end point
                int iSave = i;
                for (iR = 0; iR < iRadialSamples; iR++)
                {
                    float fRadialFraction = iR*fInvRS;  // in [0,1)
                    Vector3f kNormal = new Vector3f(afCos[iR],afSin[iR],0.0f);
                    pkVB.Position3(i, kSliceCenter.add( kNormal.scale(fRadius)));
                    if (m_kAttr.HasNormal())
                    {
                        if (m_bInside)
                        {
                            pkVB.Normal3(i, kNormal.neg());
                        }
                        else
                        {
                            pkVB.Normal3(i, kNormal);
                        }
                    }

                    if (m_kAttr.GetMaxTCoords() > 0)
                    {
                        kTCoord = new Vector2f(fRadialFraction,fAxisFraction);
                        for (iUnit = 0; iUnit < m_kAttr.GetMaxTCoords(); iUnit++)
                        {
                            if (m_kAttr.HasTCoord(iUnit))
                            {
                                pkVB.TCoord2(iUnit,i, kTCoord);
                            }
                        }
                    }

                    i++;
                }

                pkVB.Position3(i, pkVB.Position3(iSave));
                if (m_kAttr.HasNormal())
                {
                    pkVB.Normal3(i, pkVB.Normal3(iSave));
                }

                if (m_kAttr.GetMaxTCoords() > 0)
                {
                    kTCoord = new Vector2f(1.0f,fAxisFraction);
                    for (iUnit = 0; iUnit < m_kAttr.GetMaxTCoords(); iUnit++)
                    {
                        if (m_kAttr.HasTCoord(iUnit))
                        {
                            pkVB.TCoord2(iUnit,i, kTCoord);
                        }
                    }
                }

                i++;
            }

            // generate connectivity
            int[] aiLocalIndex = pkIB.GetData();
            int iIndex = 0;
            for (iA = 0, iAStart = 0; iA < iAxisSamples-1; iA++)
            {
                int i0 = iAStart;
                int i1 = i0 + 1;
                iAStart += iRadialSamples + 1;
                int i2 = iAStart;
                int i3 = i2 + 1;
                for (i = 0; i < iRadialSamples; i++)
                {
                    if (m_bInside)
                    {
                        aiLocalIndex[iIndex++] = i0++;
                        aiLocalIndex[iIndex++] = i2;
                        aiLocalIndex[iIndex++] = i1;
                        aiLocalIndex[iIndex++] = i1++;
                        aiLocalIndex[iIndex++] = i2++;
                        aiLocalIndex[iIndex++] = i3++;
                    }
                    else // outside view
                    {
                        aiLocalIndex[iIndex++] = i0++;
                        aiLocalIndex[iIndex++] = i1;
                        aiLocalIndex[iIndex++] = i2;
                        aiLocalIndex[iIndex++] = i1++;
                        aiLocalIndex[iIndex++] = i3++;
                        aiLocalIndex[iIndex++] = i2++;
                    }
                }
            }

            afCos = null;
            afSin = null;

            TransformData(pkVB);
            pkMesh = new TriMesh(pkVB,pkIB);
        }
        else
        {
            pkMesh = Sphere(iAxisSamples,iRadialSamples,fRadius);
            VertexBuffer pkVB = pkMesh.VBuffer;
            int iVQuantity = pkVB.GetVertexQuantity();

            // flatten sphere at poles
            float fHDiv2 = 0.5f*fHeight;
            Vector3f kPosition = pkVB.Position3(iVQuantity-2);
            kPosition.Z(-fHDiv2); // south pole
            pkVB.Position3(iVQuantity-2, kPosition);
            kPosition = pkVB.Position3(iVQuantity-1);
            kPosition.Z( fHDiv2);  // north pole
            pkVB.Position3(iVQuantity-1,kPosition);

            // remap z-values to [-h/2,h/2]
            float fZFactor = 2.0f/(iAxisSamples-1);
            float fTmp0 = fRadius*(-1.0f + fZFactor);
            float fTmp1 = 1.0f/(fRadius*(+1.0f - fZFactor));
            for (int i = 0; i < iVQuantity-2; i++)
            {
                kPosition = pkVB.Position3(i);
                kPosition.Z( fHDiv2*(-1.0f+fTmp1*(
                                                  kPosition.Z()-fTmp0)));

                float fX = kPosition.X(), fY = kPosition.Y();
                float fAdjust = fRadius*Mathf.InvSqrt(fX*fX + fY*fY);
                kPosition.X( kPosition.X()* fAdjust);
                kPosition.Y( kPosition.Y()* fAdjust);

                pkVB.Position3(i, kPosition);
            }
            TransformData(pkVB);
            pkMesh.UpdateMS(true);
        }

        // The duplication of vertices at the seam cause the automatically
        // generated bounding volume to be slightly off center.  Reset the bound
        // to use the true information.
        float fMaxDist = (float)Math.sqrt(fRadius*fRadius+fHeight*fHeight);
        pkMesh.ModelBound.SetCenter(Vector3f.ZERO);
        pkMesh.ModelBound.SetRadius(fMaxDist);
        return pkMesh;
    }

    /** Standard meshes.  Each mesh is centered at (0,0,0) and has an up-axis
     * of (0,0,1).  The other axes forming the coordinate system are (1,0,0)
     * and (0,1,0).  An application may transform the meshes as necessary.
     * @param iZSamples, number of z-samples.
     * @param iRadialSamples, number of radial samples.
     * @param fRadius, sphere radius.
     * @return Sphere TriMesh.
     */
    public TriMesh Sphere (int iZSamples, int iRadialSamples, float fRadius)
    {
        int iZSm1 = iZSamples-1, iZSm2 = iZSamples-2, iZSm3 = iZSamples-3;
        int iRSp1 = iRadialSamples+1;
        int iVQuantity = iZSm2*iRSp1 + 2;
        int iTQuantity = 2*iZSm2*iRadialSamples;
        VertexBuffer pkVB = new VertexBuffer(m_kAttr,iVQuantity);
        IndexBuffer pkIB = new IndexBuffer(3*iTQuantity);
        
        // generate geometry
        float fInvRS = 1.0f/(float)iRadialSamples;
        float fZFactor = 2.0f/(float)iZSm1;
        int iR, iZ, iZStart, i, iUnit;
        Vector2f kTCoord = new Vector2f();
        
        // Generate points on the unit circle to be used in computing the mesh
        // points on a cylinder slice.
        float[] afSin = new float[iRSp1];
        float[] afCos = new float[iRSp1];
        for (iR = 0; iR < iRadialSamples; iR++)
        {
            float fAngle = Mathf.TWO_PI*fInvRS*iR;
            afCos[iR] = (float)Math.cos(fAngle);
            afSin[iR] = (float)Math.sin(fAngle);
        }
        afSin[iRadialSamples] = afSin[0];
        afCos[iRadialSamples] = afCos[0];
        
        // generate the cylinder itself
        for (iZ = 1, i = 0; iZ < iZSm1; iZ++)
        {
            float fZFraction = -1.0f + fZFactor*iZ;  // in (-1,1)
            float fZ = fRadius*fZFraction;
            
            // compute center of slice
            Vector3f kSliceCenter = new Vector3f(0.0f,0.0f,fZ);
            
            // compute radius of slice
            float fSliceRadius = (float)Math.sqrt(Math.abs(fRadius*fRadius-fZ*fZ));
            
            // compute slice vertices with duplication at end point
            Vector3f kNormal = new Vector3f();
            int iSave = i;
            for (iR = 0; iR < iRadialSamples; iR++)
            {
                float fRadialFraction = iR*fInvRS;  // in [0,1)
                Vector3f kRadial= new Vector3f(afCos[iR],afSin[iR],0.0f);
                pkVB.Position3(i, kSliceCenter.add( kRadial.scale(fSliceRadius)));
                if (m_kAttr.HasNormal())
                {
                    kNormal = pkVB.Position3(i);
                    kNormal.Normalize();
                    if (m_bInside)
                    {
                        pkVB.Normal3(i, kNormal.neg());
                    }
                    else
                    {
                        pkVB.Normal3(i, kNormal);
                    }
                }
                
                if (m_kAttr.GetMaxTCoords() > 0)
                {
                    kTCoord = new Vector2f(fRadialFraction,
                                           0.5f*(fZFraction+1.0f));
                    for (iUnit = 0; iUnit < m_kAttr.GetMaxTCoords(); iUnit++)
                    {
                        if (m_kAttr.HasTCoord(iUnit))
                        {
                            pkVB.TCoord2(iUnit,i, kTCoord);
                        }
                    }
                }
                i++;
            }
            
            pkVB.Position3(i, pkVB.Position3(iSave));
            if (m_kAttr.HasNormal())
            {
                pkVB.Normal3(i, pkVB.Normal3(iSave));
            }
            
            if (m_kAttr.GetMaxTCoords() > 0)
            {
                kTCoord = new Vector2f(1.0f,0.5f*(fZFraction+1.0f));
                for (iUnit = 0; iUnit < m_kAttr.GetMaxTCoords(); iUnit++)
                {
                    if (m_kAttr.HasTCoord(iUnit))
                    {
                        pkVB.TCoord2(iUnit,i, kTCoord);
                    }
                }
            }
            i++;
        }
        
        // south pole
        pkVB.Position3(i, new Vector3f(Vector3f.UNIT_Z).scale(-fRadius));
        if (m_kAttr.HasNormal())
        {
            if (m_bInside)
            {
                pkVB.Normal3(i, new Vector3f(Vector3f.UNIT_Z));
            }
            else
            {
                pkVB.Normal3(i, new Vector3f( Vector3f.UNIT_Z_NEG));
            }
        }
        
        if (m_kAttr.GetMaxTCoords() > 0)
        {
            kTCoord = new Vector2f(0.5f,0.5f);
            for (iUnit = 0; iUnit < m_kAttr.GetMaxTCoords(); iUnit++)
            {
                if (m_kAttr.HasTCoord(iUnit))
                {
                    pkVB.TCoord2(iUnit,i, kTCoord);
                }
            }
        }
        
        i++;
        
        // north pole
        pkVB.Position3(i, new Vector3f(Vector3f.UNIT_Z).scale(fRadius));
        if (m_kAttr.HasNormal())
        {
            if (m_bInside)
            {
                pkVB.Normal3(i, new Vector3f( Vector3f.UNIT_Z_NEG ));
            }
            else
            {
                pkVB.Normal3(i, new Vector3f( Vector3f.UNIT_Z) );
            }
        }

        if (m_kAttr.GetMaxTCoords() > 0)
        {
            kTCoord = new Vector2f(0.5f,1.0f);
            for (iUnit = 0; iUnit < m_kAttr.GetMaxTCoords(); iUnit++)
            {
                if (m_kAttr.HasTCoord(iUnit))
                {
                    pkVB.TCoord2(iUnit,i, kTCoord);
                }
            }
        }
        
        i++;
        assert(i == iVQuantity);

        // generate connectivity
        int iIndex = 0;
        int[] aiLocalIndex = pkIB.GetData();
        for (iZ = 0, iZStart = 0; iZ < iZSm3; iZ++)
        {
            int i0 = iZStart;
            int i1 = i0 + 1;
            iZStart += iRSp1;
            int i2 = iZStart;
            int i3 = i2 + 1;
            for (i = 0; i < iRadialSamples; i++/*, aiLocalIndex += 6*/)
            {
                if (m_bInside)
                {
                    aiLocalIndex[iIndex++] = i0++;
                    aiLocalIndex[iIndex++] = i2;
                    aiLocalIndex[iIndex++] = i1;
                    aiLocalIndex[iIndex++] = i1++;
                    aiLocalIndex[iIndex++] = i2++;
                    aiLocalIndex[iIndex++] = i3++;
                }
                else  // inside view
                {
                    aiLocalIndex[iIndex++] = i0++;
                    aiLocalIndex[iIndex++] = i1;
                    aiLocalIndex[iIndex++] = i2;
                    aiLocalIndex[iIndex++] = i1++;
                    aiLocalIndex[iIndex++] = i3++;
                    aiLocalIndex[iIndex++] = i2++;
                }
            }
        }

        // south pole triangles
        int iVQm2 = iVQuantity-2;
        for (i = 0; i < iRadialSamples; i++/*, aiLocalIndex += 3*/)
        {
            if (m_bInside)
            {
                aiLocalIndex[iIndex++] = i;
                aiLocalIndex[iIndex++] = i+1;
                aiLocalIndex[iIndex++] = iVQm2;
            }
            else  // inside view
            {
                aiLocalIndex[iIndex++] = i;
                aiLocalIndex[iIndex++] = iVQm2;
                aiLocalIndex[iIndex++] = i+1;
            }
        }

        // north pole triangles
        int iVQm1 = iVQuantity-1, iOffset = iZSm3*iRSp1;
        for (i = 0; i < iRadialSamples; i++/*, aiLocalIndex += 3*/)
        {
            if (m_bInside)
            {
                aiLocalIndex[iIndex++] = i+iOffset;
                aiLocalIndex[iIndex++] = iVQm1;
                aiLocalIndex[iIndex++] = i+1+iOffset;
            }
            else  // inside view
            {
                aiLocalIndex[iIndex++] = i+iOffset;
                aiLocalIndex[iIndex++] = i+1+iOffset;
                aiLocalIndex[iIndex++] = iVQm1;
            }
        }
        assert(iIndex == 3*iTQuantity);
        afCos = null;
        afSin = null;

        TransformData(pkVB);
        TriMesh pkMesh = new TriMesh(pkVB,pkIB);

        // The duplication of vertices at the seam cause the automatically
        // generated bounding volume to be slightly off center.  Reset the bound
        // to use the true information.
        pkMesh.ModelBound.SetCenter(new Vector3f(Vector3f.ZERO));
        pkMesh.ModelBound.SetRadius(fRadius);
        return pkMesh;
    }

    /** Standard meshes.  Each mesh is centered at (0,0,0) and has an up-axis
     * of (0,0,1).  The other axes forming the coordinate system are (1,0,0)
     * and (0,1,0).  An application may transform the meshes as necessary.
     * @param iCircleSamples, number of circle samples.
     * @param iRadialSamples, number of radial samples.
     * @param fOuterRadius, torus outer radius.
     * @param fInnerRadius, torus inner radius.
     * @return Torus TriMesh.
     */
    public TriMesh Torus (int iCircleSamples, int iRadialSamples,
                          float fOuterRadius, float fInnerRadius)
    {
        int iVQuantity = (iCircleSamples+1)*(iRadialSamples+1);
        int iTQuantity = 2*iCircleSamples*iRadialSamples;
        VertexBuffer pkVB = new VertexBuffer(m_kAttr,iVQuantity);
        IndexBuffer pkIB = new IndexBuffer(3*iTQuantity);

        // generate geometry
        float fInvCS = 1.0f/(float)iCircleSamples;
        float fInvRS = 1.0f/(float)iRadialSamples;
        int iC, iR, i, iUnit;
        Vector2f kTCoord;

        // generate the cylinder itself
        for (iC = 0, i = 0; iC < iCircleSamples; iC++)
        {
            // compute center point on torus circle at specified angle
            float fCircleFraction = iC*fInvCS;  // in [0,1)
            float fTheta = Mathf.TWO_PI*fCircleFraction;
            float fCosTheta = (float)Math.cos(fTheta);
            float fSinTheta = (float)Math.sin(fTheta);
            Vector3f kRadial = new Vector3f(fCosTheta,fSinTheta,0.0f);
            Vector3f kTorusMiddle = kRadial.scale(fOuterRadius);

            // compute slice vertices with duplication at end point
            int iSave = i;
            for (iR = 0; iR < iRadialSamples; iR++)
            {
                float fRadialFraction = iR*fInvRS;  // in [0,1)
                float fPhi = Mathf.TWO_PI*fRadialFraction;
                float fCosPhi = (float)Math.cos(fPhi);
                float fSinPhi = (float)Math.sin(fPhi);
                Vector3f kNormal = kRadial.scale(fCosPhi).add(Vector3f.UNIT_Z.scale(fSinPhi));
                pkVB.Position3(i, kTorusMiddle.add( kNormal.scale(fInnerRadius) ) );
                if (m_kAttr.HasNormal())
                {
                    if (m_bInside)
                    {
                        pkVB.Normal3(i, kNormal.neg() );
                    }
                    else
                    {
                        pkVB.Normal3(i, kNormal );
                    }
                }

                if (m_kAttr.GetMaxTCoords() > 0)
                {
                    kTCoord = new Vector2f(fRadialFraction,fCircleFraction);
                    for (iUnit = 0; iUnit < m_kAttr.GetMaxTCoords(); iUnit++)
                    {
                        if (m_kAttr.HasTCoord(iUnit))
                        {
                            pkVB.TCoord2(iUnit,i, kTCoord);
                        }
                    }
                }

                i++;
            }

            pkVB.Position3(i, pkVB.Position3(iSave));
            if (m_kAttr.HasNormal())
            {
                pkVB.Normal3(i, pkVB.Normal3(iSave));
            }

            if (m_kAttr.GetMaxTCoords() > 0)
            {
                kTCoord = new Vector2f(1.0f,fCircleFraction);
                for (iUnit = 0; iUnit < m_kAttr.GetMaxTCoords(); iUnit++)
                {
                    if (m_kAttr.HasTCoord(iUnit))
                    {
                        pkVB.TCoord2(iUnit,i, kTCoord);
                    }
                }
            }
            
            i++;
        }

        // duplicate the cylinder ends to form a torus
        for (iR = 0; iR <= iRadialSamples; iR++, i++)
        {
            pkVB.Position3(i, pkVB.Position3(iR));
            if (m_kAttr.HasNormal())
            {
                pkVB.Normal3(i, pkVB.Normal3(iR));
            }

            if (m_kAttr.GetMaxTCoords() > 0)
            {
                kTCoord = new Vector2f(pkVB.TCoord2(0,iR).X(),1.0f);
                for (iUnit = 0; iUnit < m_kAttr.GetMaxTCoords(); iUnit++)
                {
                    if (m_kAttr.HasTCoord(iUnit))
                    {
                        pkVB.TCoord2(iUnit,i, kTCoord);
                    }
                }
            }
        }
        assert(i == iVQuantity);

        // generate connectivity
        int iIndex = 0;
        int[] aiLocalIndex = pkIB.GetData();
        int iCStart = 0;
        for (iC = 0; iC < iCircleSamples; iC++)
        {
            int i0 = iCStart;
            int i1 = i0 + 1;
            iCStart += iRadialSamples + 1;
            int i2 = iCStart;
            int i3 = i2 + 1;
            for (i = 0; i < iRadialSamples; i++)
            {
                if (m_bInside)
                {
                    aiLocalIndex[iIndex++] = i0++;
                    aiLocalIndex[iIndex++] = i1;
                    aiLocalIndex[iIndex++] = i2;
                    aiLocalIndex[iIndex++] = i1++;
                    aiLocalIndex[iIndex++] = i3++;
                    aiLocalIndex[iIndex++] = i2++;
                }
                else  // inside view
                {
                    aiLocalIndex[iIndex++] = i0++;
                    aiLocalIndex[iIndex++] = i2;
                    aiLocalIndex[iIndex++] = i1;
                    aiLocalIndex[iIndex++] = i1++;
                    aiLocalIndex[iIndex++] = i2++;
                    aiLocalIndex[iIndex++] = i3++;
                }
            }
        }

        TransformData(pkVB);
        TriMesh pkMesh = new TriMesh(pkVB,pkIB);

        // The duplication of vertices at the seam cause the automatically
        // generated bounding volume to be slightly off center.  Reset the bound
        // to use the true information.
        pkMesh.ModelBound.SetCenter(Vector3f.ZERO);
        pkMesh.ModelBound.SetRadius(fOuterRadius);
        return pkMesh;
    }

    /** Platonic solids, inscribed in a unit sphere centered at (0,0,0).
     * @return tetrahedron TriMesh. */
    public TriMesh Tetrahedron ()
    {
        float fSqrt2Div3 = (float)Math.sqrt(2.0f)/3.0f;
        float fSqrt6Div3 = (float)Math.sqrt(6.0f)/3.0f;
        float fOneThird = 1.0f/3.0f;

        int iVQuantity = 4;
        int iTQuantity = 4;
        VertexBuffer pkVB = new VertexBuffer(m_kAttr,iVQuantity);
        IndexBuffer pkIB = new IndexBuffer(3*iTQuantity);

        pkVB.Position3(0, new Vector3f(0.0f,0.0f,1.0f));
        pkVB.Position3(1, new Vector3f(2.0f*fSqrt2Div3,0.0f,-fOneThird));
        pkVB.Position3(2, new Vector3f(-fSqrt2Div3,fSqrt6Div3,-fOneThird));
        pkVB.Position3(3, new Vector3f(-fSqrt2Div3,-fSqrt6Div3,-fOneThird));

        int[] aiIndex = pkIB.GetData();
        aiIndex[ 0] = 0;  aiIndex[ 1] = 1;  aiIndex[ 2] = 2;
        aiIndex[ 3] = 0;  aiIndex[ 4] = 2;  aiIndex[ 5] = 3;
        aiIndex[ 6] = 0;  aiIndex[ 7] = 3;  aiIndex[ 8] = 1;
        aiIndex[ 9] = 1;  aiIndex[10] = 3;  aiIndex[11] = 2;

        CreatePlatonicNormals(pkVB);
        CreatePlatonicUVs(pkVB);
        if (m_bInside)
        {
            ReverseTriangleOrder(iTQuantity,aiIndex);
        }

        TransformData(pkVB);
        TriMesh pkMesh = new TriMesh(pkVB,pkIB);
        return pkMesh;
    }

    /** Platonic solids, inscribed in a unit sphere centered at (0,0,0).
     * @return hexahedron TriMesh. */
    public TriMesh Hexahedron ()
    {
        float fSqrtThird = (float)Math.sqrt(1.0f/3.0f);

        int iVQuantity = 8;
        int iTQuantity = 12;
        VertexBuffer pkVB = new VertexBuffer(m_kAttr,iVQuantity);
        IndexBuffer pkIB = new IndexBuffer(3*iTQuantity);

        pkVB.Position3(0, new Vector3f(-fSqrtThird,-fSqrtThird,-fSqrtThird));
        pkVB.Position3(1, new Vector3f( fSqrtThird,-fSqrtThird,-fSqrtThird));
        pkVB.Position3(2, new Vector3f( fSqrtThird, fSqrtThird,-fSqrtThird));
        pkVB.Position3(3, new Vector3f(-fSqrtThird, fSqrtThird,-fSqrtThird));
        pkVB.Position3(4, new Vector3f(-fSqrtThird,-fSqrtThird, fSqrtThird));
        pkVB.Position3(5, new Vector3f( fSqrtThird,-fSqrtThird, fSqrtThird));
        pkVB.Position3(6, new Vector3f( fSqrtThird, fSqrtThird, fSqrtThird));
        pkVB.Position3(7, new Vector3f(-fSqrtThird, fSqrtThird, fSqrtThird));

        int[] aiIndex = pkIB.GetData();
        aiIndex[ 0] = 0;  aiIndex[ 1] = 3;  aiIndex[ 2] = 2;
        aiIndex[ 3] = 0;  aiIndex[ 4] = 2;  aiIndex[ 5] = 1;
        aiIndex[ 6] = 0;  aiIndex[ 7] = 1;  aiIndex[ 8] = 5;
        aiIndex[ 9] = 0;  aiIndex[10] = 5;  aiIndex[11] = 4;
        aiIndex[12] = 0;  aiIndex[13] = 4;  aiIndex[14] = 7;
        aiIndex[15] = 0;  aiIndex[16] = 7;  aiIndex[17] = 3;
        aiIndex[18] = 6;  aiIndex[19] = 5;  aiIndex[20] = 1;
        aiIndex[21] = 6;  aiIndex[22] = 1;  aiIndex[23] = 2;
        aiIndex[24] = 6;  aiIndex[25] = 2;  aiIndex[26] = 3;
        aiIndex[27] = 6;  aiIndex[28] = 3;  aiIndex[29] = 7;
        aiIndex[30] = 6;  aiIndex[31] = 7;  aiIndex[32] = 4;
        aiIndex[33] = 6;  aiIndex[34] = 4;  aiIndex[35] = 5;

        CreatePlatonicNormals(pkVB);
        CreatePlatonicUVs(pkVB);
        if (m_bInside)
        {
            ReverseTriangleOrder(iTQuantity,aiIndex);
        }

        TransformData(pkVB);
        TriMesh pkMesh = new TriMesh(pkVB,pkIB);
        return pkMesh;
    }

    /** Platonic solids, inscribed in a unit sphere centered at (0,0,0).
     * @return octahedron TriMesh. */
    public TriMesh Octahedron ()
    {
        int iVQuantity = 6;
        int iTQuantity = 8;
        VertexBuffer pkVB = new VertexBuffer(m_kAttr,iVQuantity);
        IndexBuffer pkIB = new IndexBuffer(3*iTQuantity);

        pkVB.Position3(0, new Vector3f( 1.0f, 0.0f, 0.0f));
        pkVB.Position3(1, new Vector3f(-1.0f, 0.0f, 0.0f));
        pkVB.Position3(2, new Vector3f( 0.0f, 1.0f, 0.0f));
        pkVB.Position3(3, new Vector3f( 0.0f,-1.0f, 0.0f));
        pkVB.Position3(4, new Vector3f( 0.0f, 0.0f, 1.0f));
        pkVB.Position3(5, new Vector3f( 0.0f, 0.0f,-1.0f));

        int[] aiIndex = pkIB.GetData();
        aiIndex[ 0] = 4;  aiIndex[ 1] = 0;  aiIndex[ 2] = 2;
        aiIndex[ 3] = 4;  aiIndex[ 4] = 2;  aiIndex[ 5] = 1;
        aiIndex[ 6] = 4;  aiIndex[ 7] = 1;  aiIndex[ 8] = 3;
        aiIndex[ 9] = 4;  aiIndex[10] = 3;  aiIndex[11] = 0;
        aiIndex[12] = 5;  aiIndex[13] = 2;  aiIndex[14] = 0;
        aiIndex[15] = 5;  aiIndex[16] = 1;  aiIndex[17] = 2;
        aiIndex[18] = 5;  aiIndex[19] = 3;  aiIndex[20] = 1;
        aiIndex[21] = 5;  aiIndex[22] = 0;  aiIndex[23] = 3;

        CreatePlatonicNormals(pkVB);
        CreatePlatonicUVs(pkVB);
        if (m_bInside)
        {
            ReverseTriangleOrder(iTQuantity,aiIndex);
        }

        TransformData(pkVB);
        TriMesh pkMesh = new TriMesh(pkVB,pkIB);
        return pkMesh;
    }

    /** Platonic solids, inscribed in a unit sphere centered at (0,0,0).
     * @return dodecahedron TriMesh. */
    public TriMesh Dodecahedron ()
    {
        float fA = (float)(1.0f/Math.sqrt(3.0));
        float fB = (float)Math.sqrt((3.0-Math.sqrt(5.0))/6.0);
        float fC = (float)Math.sqrt((3.0+Math.sqrt(5.0))/6.0);

        int iVQuantity = 20;
        int iTQuantity = 36;
        VertexBuffer pkVB = new VertexBuffer(m_kAttr,iVQuantity);
        IndexBuffer pkIB = new IndexBuffer(3*iTQuantity);

        pkVB.Position3( 0, new Vector3f( fA, fA, fA));
        pkVB.Position3( 1, new Vector3f( fA, fA,-fA));
        pkVB.Position3( 2, new Vector3f( fA,-fA, fA));
        pkVB.Position3( 3, new Vector3f( fA,-fA,-fA));
        pkVB.Position3( 4, new Vector3f(-fA, fA, fA));
        pkVB.Position3( 5, new Vector3f(-fA, fA,-fA));
        pkVB.Position3( 6, new Vector3f(-fA,-fA, fA));
        pkVB.Position3( 7, new Vector3f(-fA,-fA,-fA));
        pkVB.Position3( 8, new Vector3f(  fB,  fC, 0.0f));
        pkVB.Position3( 9, new Vector3f( -fB,  fC, 0.0f));
        pkVB.Position3(10, new Vector3f(  fB, -fC, 0.0f));
        pkVB.Position3(11, new Vector3f( -fB, -fC, 0.0f));
        pkVB.Position3(12, new Vector3f(  fC, 0.0f,  fB));
        pkVB.Position3(13, new Vector3f(  fC, 0.0f, -fB));
        pkVB.Position3(14, new Vector3f( -fC, 0.0f,  fB));
        pkVB.Position3(15, new Vector3f( -fC, 0.0f, -fB));
        pkVB.Position3(16, new Vector3f(0.0f,   fB,  fC));
        pkVB.Position3(17, new Vector3f(0.0f,  -fB,  fC));
        pkVB.Position3(18, new Vector3f(0.0f,   fB, -fC));
        pkVB.Position3(19, new Vector3f(0.0f,  -fB, -fC));

        int[] aiIndex = pkIB.GetData();
        aiIndex[  0] =  0;  aiIndex[  1] =  8;  aiIndex[  2] =  9;
        aiIndex[  3] =  0;  aiIndex[  4] =  9;  aiIndex[  5] =  4;
        aiIndex[  6] =  0;  aiIndex[  7] =  4;  aiIndex[  8] = 16;
        aiIndex[  9] =  0;  aiIndex[ 10] = 12;  aiIndex[ 11] = 13;
        aiIndex[ 12] =  0;  aiIndex[ 13] = 13;  aiIndex[ 14] =  1;
        aiIndex[ 15] =  0;  aiIndex[ 16] =  1;  aiIndex[ 17] =  8;
        aiIndex[ 18] =  0;  aiIndex[ 19] = 16;  aiIndex[ 20] = 17;
        aiIndex[ 21] =  0;  aiIndex[ 22] = 17;  aiIndex[ 23] =  2;
        aiIndex[ 24] =  0;  aiIndex[ 25] =  2;  aiIndex[ 26] = 12;
        aiIndex[ 27] =  8;  aiIndex[ 28] =  1;  aiIndex[ 29] = 18;
        aiIndex[ 30] =  8;  aiIndex[ 31] = 18;  aiIndex[ 32] =  5;
        aiIndex[ 33] =  8;  aiIndex[ 34] =  5;  aiIndex[ 35] =  9;
        aiIndex[ 36] = 12;  aiIndex[ 37] =  2;  aiIndex[ 38] = 10;
        aiIndex[ 39] = 12;  aiIndex[ 40] = 10;  aiIndex[ 41] =  3;
        aiIndex[ 42] = 12;  aiIndex[ 43] =  3;  aiIndex[ 44] = 13;
        aiIndex[ 45] = 16;  aiIndex[ 46] =  4;  aiIndex[ 47] = 14;
        aiIndex[ 48] = 16;  aiIndex[ 49] = 14;  aiIndex[ 50] =  6;
        aiIndex[ 51] = 16;  aiIndex[ 52] =  6;  aiIndex[ 53] = 17;
        aiIndex[ 54] =  9;  aiIndex[ 55] =  5;  aiIndex[ 56] = 15;
        aiIndex[ 57] =  9;  aiIndex[ 58] = 15;  aiIndex[ 59] = 14;
        aiIndex[ 60] =  9;  aiIndex[ 61] = 14;  aiIndex[ 62] =  4;
        aiIndex[ 63] =  6;  aiIndex[ 64] = 11;  aiIndex[ 65] = 10;
        aiIndex[ 66] =  6;  aiIndex[ 67] = 10;  aiIndex[ 68] =  2;
        aiIndex[ 69] =  6;  aiIndex[ 70] =  2;  aiIndex[ 71] = 17;
        aiIndex[ 72] =  3;  aiIndex[ 73] = 19;  aiIndex[ 74] = 18;
        aiIndex[ 75] =  3;  aiIndex[ 76] = 18;  aiIndex[ 77] =  1;
        aiIndex[ 78] =  3;  aiIndex[ 79] =  1;  aiIndex[ 80] = 13;
        aiIndex[ 81] =  7;  aiIndex[ 82] = 15;  aiIndex[ 83] =  5;
        aiIndex[ 84] =  7;  aiIndex[ 85] =  5;  aiIndex[ 86] = 18;
        aiIndex[ 87] =  7;  aiIndex[ 88] = 18;  aiIndex[ 89] = 19;
        aiIndex[ 90] =  7;  aiIndex[ 91] = 11;  aiIndex[ 92] =  6;
        aiIndex[ 93] =  7;  aiIndex[ 94] =  6;  aiIndex[ 95] = 14;
        aiIndex[ 96] =  7;  aiIndex[ 97] = 14;  aiIndex[ 98] = 15;
        aiIndex[ 99] =  7;  aiIndex[100] = 19;  aiIndex[101] =  3;
        aiIndex[102] =  7;  aiIndex[103] =  3;  aiIndex[104] = 10;
        aiIndex[105] =  7;  aiIndex[106] = 10;  aiIndex[107] = 11;

        CreatePlatonicNormals(pkVB);
        CreatePlatonicUVs(pkVB);
        if (m_bInside)
        {
            ReverseTriangleOrder(iTQuantity,aiIndex);
        }

        TransformData(pkVB);
        TriMesh pkMesh = new TriMesh(pkVB,pkIB);
        return pkMesh;
    }

    /** Platonic solids, inscribed in a unit sphere centered at (0,0,0).
     * @return icosahedron TriMesh. */
    public TriMesh Icosahedron ()
    {
        float fGoldenRatio = (float)(0.5f*(1.0f+Math.sqrt(5.0)));
        float fInvRoot = (float)(1.0f/Math.sqrt(1.0+fGoldenRatio*fGoldenRatio));
        float fU = fGoldenRatio*fInvRoot;
        float fV = fInvRoot;

        int iVQuantity = 12;
        int iTQuantity = 20;
        VertexBuffer pkVB = new VertexBuffer(m_kAttr,iVQuantity);
        IndexBuffer pkIB = new IndexBuffer(3*iTQuantity);

        pkVB.Position3( 0, new Vector3f(  fU,  fV,0.0f));
        pkVB.Position3( 1, new Vector3f( -fU,  fV,0.0f));
        pkVB.Position3( 2, new Vector3f(  fU, -fV,0.0f));
        pkVB.Position3( 3, new Vector3f( -fU, -fV,0.0f));
        pkVB.Position3( 4, new Vector3f(  fV,0.0f,  fU));
        pkVB.Position3( 5, new Vector3f(  fV,0.0f, -fU));
        pkVB.Position3( 6, new Vector3f( -fV,0.0f,  fU));
        pkVB.Position3( 7, new Vector3f( -fV,0.0f, -fU));
        pkVB.Position3( 8, new Vector3f(0.0f,  fU,  fV));
        pkVB.Position3( 9, new Vector3f(0.0f, -fU,  fV));
        pkVB.Position3(10, new Vector3f(0.0f,  fU, -fV));
        pkVB.Position3(11, new Vector3f(0.0f, -fU, -fV));

        int[] aiIndex = pkIB.GetData();
        aiIndex[ 0] =  0;  aiIndex[ 1] =  8;  aiIndex[ 2] =  4;
        aiIndex[ 3] =  0;  aiIndex[ 4] =  5;  aiIndex[ 5] = 10;
        aiIndex[ 6] =  2;  aiIndex[ 7] =  4;  aiIndex[ 8] =  9;
        aiIndex[ 9] =  2;  aiIndex[10] = 11;  aiIndex[11] =  5;
        aiIndex[12] =  1;  aiIndex[13] =  6;  aiIndex[14] =  8;
        aiIndex[15] =  1;  aiIndex[16] = 10;  aiIndex[17] =  7;
        aiIndex[18] =  3;  aiIndex[19] =  9;  aiIndex[20] =  6;
        aiIndex[21] =  3;  aiIndex[22] =  7;  aiIndex[23] = 11;
        aiIndex[24] =  0;  aiIndex[25] = 10;  aiIndex[26] =  8;
        aiIndex[27] =  1;  aiIndex[28] =  8;  aiIndex[29] = 10;
        aiIndex[30] =  2;  aiIndex[31] =  9;  aiIndex[32] = 11;
        aiIndex[33] =  3;  aiIndex[34] = 11;  aiIndex[35] =  9;
        aiIndex[36] =  4;  aiIndex[37] =  2;  aiIndex[38] =  0;
        aiIndex[39] =  5;  aiIndex[40] =  0;  aiIndex[41] =  2;
        aiIndex[42] =  6;  aiIndex[43] =  1;  aiIndex[44] =  3;
        aiIndex[45] =  7;  aiIndex[46] =  3;  aiIndex[47] =  1;
        aiIndex[48] =  8;  aiIndex[49] =  6;  aiIndex[50] =  4;
        aiIndex[51] =  9;  aiIndex[52] =  4;  aiIndex[53] =  6;
        aiIndex[54] = 10;  aiIndex[55] =  5;  aiIndex[56] =  7;
        aiIndex[57] = 11;  aiIndex[58] =  7;  aiIndex[59] =  5;

        CreatePlatonicNormals(pkVB);
        CreatePlatonicUVs(pkVB);
        if (m_bInside)
        {
            ReverseTriangleOrder(iTQuantity,aiIndex);
        }

        TransformData(pkVB);
        TriMesh pkMesh = new TriMesh(pkVB,pkIB);
        return pkMesh;
    }

    /** Create Platonic normals
     * @param pkVBuffer VertexBuffer to store normals in. */
    private void CreatePlatonicNormals (VertexBuffer pkVBuffer)
    {
        if (m_kAttr.HasNormal())
        {
            for (int i = 0; i < pkVBuffer.GetVertexQuantity(); i++)
            {
                pkVBuffer.Normal3(i, pkVBuffer.Position3(i));
            }
        }
    }

    /** Create Platonic u,v texture-coordinates.
     * @param pkVBuffer VertexBuffer to store texture coordinates in. */
    private void CreatePlatonicUVs (VertexBuffer pkVBuffer)
    {
        if (m_kAttr.GetMaxTCoords() > 0)
        {
            for (int iUnit = 0; iUnit < m_kAttr.GetMaxTCoords(); iUnit++)
            {
                if (m_kAttr.HasTCoord(iUnit))
                {
                    for (int i = 0; i < pkVBuffer.GetVertexQuantity(); i++)
                    {
                        if (Math.abs(pkVBuffer.Position3(i).Z()) < 1.0f)
                        {
                            Vector2f tCoord = pkVBuffer.TCoord2(iUnit,i);
                            tCoord.X( (float)(0.5f*(1.0f +
                                            Math.atan2(pkVBuffer.Position3(i).Y(),
                                                       pkVBuffer.Position3(i).X())*Mathf.INV_PI)) );
                            pkVBuffer.TCoord2(iUnit,i,tCoord);
                        }
                        else
                        {
                            Vector2f tCoord = pkVBuffer.TCoord2(iUnit,i);
                            tCoord.X( 0.5f );
                            pkVBuffer.TCoord2(iUnit,i,tCoord);
                        }
                        Vector2f tCoord = pkVBuffer.TCoord2(iUnit,i);
                        tCoord.Y( (float)Math.acos(
                                                     pkVBuffer.Position3(i).Z())*Mathf.INV_PI );
                        pkVBuffer.TCoord2(iUnit,i,tCoord);
                    }
                }
            }
        }
    }

    /** Reverse triangle order of a mesh.
     * @param iTQuantity, number of triangles.
     * @param aiIndex, index array to modify.
     */
    private void ReverseTriangleOrder (int iTQuantity, int[] aiIndex)
    {
        for (int i = 0; i < iTQuantity; i++)
        {
            int j1 = 3*i+1, j2 = j1+1;
            int iSave = aiIndex[j1];
            aiIndex[j1] = aiIndex[j2];
            aiIndex[j2] = iSave;
        }
    }

    /** Transform data in VertexBuffer
     * @param pkVB, VertexBuffer to transform.
     */
    private void TransformData (VertexBuffer pkVB)
    {
        if (m_kXFrm.IsIdentity())
        {
            return;
        }

        int iVQuantity = pkVB.GetVertexQuantity();
        int i;
        for (i = 0; i < iVQuantity; i++)
        {
            pkVB.Position3(i,m_kXFrm.ApplyForward(pkVB.Position3(i)));
        }

        if (m_kAttr.HasNormal())
        {
            Vector3f kSave = m_kXFrm.GetTranslate();
            m_kXFrm.SetTranslate(Vector3f.ZERO);
            for (i = 0; i < iVQuantity; i++)
            {
                pkVB.Normal3(i, m_kXFrm.ApplyForward(pkVB.Normal3(i)));
                pkVB.Normal3(i).Normalize();
            }
            m_kXFrm.SetTranslate(kSave);
        }
    }

    /** Mesh Attributes. */
    private Attributes m_kAttr;
    /** Mesh Transformation */
    private Transformation m_kXFrm = new Transformation();
    /** Inside mesh, or Outside mesh. */
    private boolean m_bInside;
}
