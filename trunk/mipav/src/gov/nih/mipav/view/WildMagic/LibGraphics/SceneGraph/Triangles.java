// Geometric Tools, Inc.
// http://www.geometrictools.com
// Copyright (c) 1998-2006.  All Rights Reserved
//
// The Wild Magic Version 4 Restricted Libraries source code is supplied
// under the terms of the license agreement
//     http://www.geometrictools.com/License/Wm4RestrictedLicense.pdf
// and may not be copied or disclosed except in accordance with the terms
// of that agreement.
//
// Version: 4.0.0 (2006/08/13)

package gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
public abstract class Triangles extends Geometry
    implements StreamInterface
{
    // These functions depend on the interpretation of the index buffer of
    // the triangle primitive.  The triangle <V0,V1,V2> is counterclockwise
    // order.
    public abstract int GetTriangleQuantity ();
    public abstract boolean GetTriangle (int i, int[] riV);

    public boolean GetModelTriangle (int i, Triangle3f rkMTri)
    {
        int[] aiTris = new int[3];
        int iV0, iV1, iV2;
        if (GetTriangle(i,aiTris))
        {
            iV0 = aiTris[0];            iV1 = aiTris[1];            iV2 = aiTris[2];
            rkMTri.V[0] = VBuffer.Position3(iV0);
            rkMTri.V[1] = VBuffer.Position3(iV1);
            rkMTri.V[2] = VBuffer.Position3(iV2);
            return true;
        }
        return false;
    }

    public boolean GetWorldTriangle (int i, Triangle3f rkWTri)
    {
        int[] aiTris = new int[3];
        int iV0, iV1, iV2;
        if (GetTriangle(i,aiTris))
        {
            iV0 = aiTris[0];            iV1 = aiTris[1];            iV2 = aiTris[2];
            rkWTri.V[0] = World.ApplyForward(VBuffer.Position3(iV0));
            rkWTri.V[1] = World.ApplyForward(VBuffer.Position3(iV1));
            rkWTri.V[2] = World.ApplyForward(VBuffer.Position3(iV2));
            return true;
        }
        return false;
    }

    public void GenerateNormals ()
    {
        if (!VBuffer.GetAttributes().HasNormal())
        {
            Attributes kAttr = VBuffer.GetAttributes();
            kAttr.SetNChannels(3);
            VertexBuffer pkVBufferPlusNormals = new VertexBuffer(kAttr,
                                                                 VBuffer.GetVertexQuantity());
            VBuffer.BuildCompatibleArray(kAttr,false,pkVBufferPlusNormals);
            VBuffer = pkVBufferPlusNormals;
        }

        UpdateModelNormals();
    }

    public Triangles () {}

    protected Triangles (VertexBuffer pkVBuffer, IndexBuffer pkIBuffer)
    {
        super(pkVBuffer,pkIBuffer);
        // The Type value will be assigned by the derived class.
    }

    // geometric updates
    protected void UpdateModelNormals ()
    {
        // Calculate normals from vertices by weighted averages of facet planes
        // that contain the vertices.
        if (!VBuffer.GetAttributes().HasNormal())
        {
            return;
        }

        int iVQuantity = VBuffer.GetVertexQuantity();
        int i;
        for (i = 0; i < iVQuantity; i++)
        {
            VBuffer.Normal3(i, new Vector3f(Vector3f.ZERO));
        }

        int iTQuantity = GetTriangleQuantity();
        for (i = 0; i < iTQuantity; i++)
        {
            // get vertex indices
            int[] aiTris = new int[3];
            int iV0, iV1, iV2;
            if (!GetTriangle(i,aiTris))
            {
                continue;
            }

            // get vertices
            iV0 = aiTris[0];            iV1 = aiTris[1];            iV2 = aiTris[2];
            Vector3f rkV0 = VBuffer.Position3(iV0);
            Vector3f rkV1 = VBuffer.Position3(iV1);
            Vector3f rkV2 = VBuffer.Position3(iV2);

            // compute the normal (length provides the weighted sum)
            Vector3f kEdge1 = rkV1.sub( rkV0 );
            Vector3f kEdge2 = rkV2.sub( rkV0 );
            Vector3f kNormal = kEdge1.Cross(kEdge2);

            VBuffer.Normal3(iV0, VBuffer.Normal3(iV0).add(kNormal));
            VBuffer.Normal3(iV1, VBuffer.Normal3(iV1).add(kNormal));
            VBuffer.Normal3(iV2, VBuffer.Normal3(iV2).add(kNormal));
        }

        for (i = 0; i < iVQuantity; i++)
        {
            Vector3f kNormalized = VBuffer.Normal3(i);
            kNormalized.Normalize();
            VBuffer.Normal3(i, kNormalized);
        }
    }

    public void Load (Stream rkStream, Stream.Link pkLink)
    {
        super.Load(rkStream,pkLink);
    } 

    public void Link (Stream rkStream, Stream.Link pkLink)
    {
        super.Link(rkStream,pkLink);
    }

    public boolean Register (Stream rkStream)
    {
        return super.Register(rkStream);
    }

    public void Save (Stream rkStream)
    {
        super.Save(rkStream);
    }

    public int GetDiskUsed (StreamVersion rkVersion)
    {
        return super.GetDiskUsed(rkVersion);
    }

    public StringTree SaveStrings (final String acTitle)
    {
        StringTree pkTree = new StringTree();
        pkTree.Append(StringTree.Format("Triangles",GetName()));
        pkTree.Append(super.SaveStrings(null));
        return pkTree;
    }
}
