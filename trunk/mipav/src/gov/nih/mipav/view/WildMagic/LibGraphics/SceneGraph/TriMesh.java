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
// Version: 4.0.2 (2006/08/13)

package gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
public class TriMesh extends Triangles
    implements StreamInterface
{
    // Construction and destruction.
    public TriMesh (VertexBuffer pkVBuffer, IndexBuffer pkIBuffer)
    {
        super(pkVBuffer,pkIBuffer);
        Type = GeometryType.GT_TRIMESH;
        UpdateModelNormals();
    }

    // Interpretation of the index buffer data.
    public int GetTriangleQuantity ()
    {
        return IBuffer.GetIndexQuantity()/3;
    }

    public boolean GetTriangle (int i, int[] riV)
    {
        if (0 <= i && i < GetTriangleQuantity())
        {
            final int[] piIndex = IBuffer.GetData();
            riV[0] = piIndex[3*i + 0];
            riV[1] = piIndex[3*i + 1];
            riV[2] = piIndex[3*i + 2];
            return true;
        }
        return false;
    }

    public TriMesh ()
    {
        Type = GeometryType.GT_TRIMESH;
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
        pkTree.Append(StringTree.Format("TriMesh",GetName()));
        pkTree.Append(super.SaveStrings(null));
        return pkTree;
    }

}
