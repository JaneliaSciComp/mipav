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
