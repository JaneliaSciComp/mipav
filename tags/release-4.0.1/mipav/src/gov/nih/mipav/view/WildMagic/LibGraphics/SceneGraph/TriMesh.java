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
    /** Construction.
     * @param pkVBuffer, vertex buffer.
     * @param pkIBuffer, index buffer.
     */
    public TriMesh (VertexBuffer pkVBuffer, IndexBuffer pkIBuffer)
    {
        super(pkVBuffer,pkIBuffer);
        Type = GeometryType.GT_TRIMESH;
        UpdateModelNormals();
    }

    /** Interpretation of the index buffer data.
     * @return number of triangles.
     */
    public final int GetTriangleQuantity ()
    {
        return IBuffer.GetIndexQuantity()/3;
    }

    /** Get triangl at the specified index.
     * @param i, index.
     * @param rkV, int[3] array to contain the three triangle index values.
     */
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

    /** Default constructor. */
    public TriMesh ()
    {
        Type = GeometryType.GT_TRIMESH;
    }

    /**
     * Loads this object from the input parameter rkStream, using the input
     * Stream.Link to store the IDs of children objects of this object
     * for linking after all objects are loaded from the Stream.
     * @param rkStream, the Stream from which this object is being read.
     * @param pkLink, the Link class for storing the IDs of this object's
     * children objcts.
     */
    public void Load (Stream rkStream, Stream.Link pkLink)
    {
        super.Load(rkStream,pkLink);
    } 

    /**
     * Copies this objects children objects from the input Stream's HashTable,
     * based on the LinkID of the child stored in the pkLink paramter.
     * @param rkStream, the Stream where the child objects are stored.
     * @param pkLink, the Link class from which the child object IDs are read.
     */
    public void Link (Stream rkStream, Stream.Link pkLink)
    {
        super.Link(rkStream,pkLink);
    }

    /**
     * Registers this object with the input Stream parameter. All objects
     * streamed to disk are registered with the Stream so that a unique list
     * of objects is maintained.
     * @param rkStream, the Stream where the child objects are stored.
     * @return true if this object is registered, false if the object has
     * already been registered.
     */
    public boolean Register (Stream rkStream)
    {
        return super.Register(rkStream);
    }

    /**
     * Write this object and all it's children to the Stream.
     * @param rkStream, the Stream where the child objects are stored.
     */
    public void Save (Stream rkStream)
    {
        super.Save(rkStream);
    }

    /**
     * Returns the size of this object and it's children on disk for the
     * current StreamVersion parameter.
     * @param rkVersion, the current version of the Stream file being created.
     * @return the size of this object on disk.
     */
    public int GetDiskUsed (StreamVersion rkVersion)
    {
        return super.GetDiskUsed(rkVersion);
    }

    /**
     * Write this object into a StringTree for the scene-graph visualization.
     * @param acTitle, the header for this object in the StringTree.
     * @return StringTree containing a String-based representation of this
     * object and it's children.
     */
    public StringTree SaveStrings (final String acTitle)
    {
        StringTree pkTree = new StringTree();
        pkTree.Append(StringTree.Format("TriMesh",GetName()));
        pkTree.Append(super.SaveStrings(null));
        return pkTree;
    }
}
