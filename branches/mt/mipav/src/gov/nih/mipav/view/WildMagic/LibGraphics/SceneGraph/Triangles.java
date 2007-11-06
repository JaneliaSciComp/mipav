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
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;

/** These functions depend on the interpretation of the index buffer of
 * the triangle primitive.  The triangle <V0,V1,V2> is counterclockwise
 * order.
 */
public abstract class Triangles extends Geometry
    implements StreamInterface
{
    /** Get the number of triangles.
     * @return the number of triangles.
     */
    public abstract int GetTriangleQuantity ();
    /** Get the triangle at given index.
     * @param i, index.
     * @param riV, int[3] array to contain 3 triangle indices.
     */
    public abstract boolean GetTriangle (int i, int[] riV);

    /**
     * Get the triangle in model coodinates.
     * @param i, index.
     * @param rkMTri, triangle in model-coordinates.
     * @return true if triangle exists, false otherwise.
     */
    public boolean GetModelTriangle (int i, Triangle3f rkMTri)
    {
        int[] aiTris = new int[3];
        int iV0, iV1, iV2;
        if (GetTriangle(i,aiTris))
        {
            iV0 = aiTris[0];            iV1 = aiTris[1];            iV2 = aiTris[2];
            VBuffer.GetPosition3(iV0, rkMTri.V[0]);
            VBuffer.GetPosition3(iV1, rkMTri.V[1]);
            VBuffer.GetPosition3(iV2, rkMTri.V[2]);
            aiTris = null;
            return true;
        }
        aiTris = null;
        return false;
    }

    /**
     * Get the triangle in world coodinates.
     * @param i, index.
     * @param rkMTri, triangle in world-coordinates.
     * @return true if triangle exists, false otherwise.
     */
    public boolean GetWorldTriangle (int i, Triangle3f rkWTri)
    {
        int[] aiTris = new int[3];
        int iV0, iV1, iV2;
        if (GetTriangle(i,aiTris))
        {
            iV0 = aiTris[0];            iV1 = aiTris[1];            iV2 = aiTris[2];
            Vector3f kPos = new Vector3f();
            
            VBuffer.GetPosition3(iV0, kPos);
            World.ApplyForward(kPos, rkWTri.V[0]);
            
            VBuffer.GetPosition3(iV1, kPos);
            World.ApplyForward(kPos, rkWTri.V[1]);
            
            VBuffer.GetPosition3(iV2, kPos);
            World.ApplyForward(kPos, rkWTri.V[2]);
            
            kPos = null;
            return true;
        }
        return false;
    }

    /** Generate triangle normals. */
    public void GenerateNormals ()
    {
        if (!VBuffer.GetAttributes().HasNormal())
        {
            Attributes kAttr = VBuffer.GetAttributes();
            kAttr.SetNChannels(3);
            VertexBuffer pkVBufferPlusNormals = new VertexBuffer(kAttr,
                                                                 VBuffer.GetVertexQuantity());
            VBuffer.BuildCompatibleArray(kAttr,pkVBufferPlusNormals);
            VBuffer = pkVBufferPlusNormals;
        }

        UpdateModelNormals();
    }

    /** Default constructor. */
    public Triangles () {}

    /** Protected constructor. The Type value will be assigned by the derived
     * class.
     * @param pkVBuffer, vertex buffer.
     * @param pkIBuffer, index buffer.
     */
    protected Triangles (VertexBuffer pkVBuffer, IndexBuffer pkIBuffer)
    {
        super(pkVBuffer,pkIBuffer);
    }
    
    /** Protected constructor. The Type value will be assigned by the derived
     * class.
     * @param pkVBuffer, vertex buffer.
     * @param pkIBuffer, index buffer.
     */
    protected Triangles (VertexBuffer pkVBuffer, IndexBuffer pkIBuffer, BoundingVolume kBounds)
    {
        super(pkVBuffer,pkIBuffer,kBounds);
    }

    /** Protected constructor. The Type value will be assigned by the derived
     * class.
     * @param pkVBuffer, vertex buffer.
     * @param pkIBuffer, index buffer.
     */
    protected Triangles (Triangles kTris)
    {
        super(kTris);
    }

    /** Update model normals. */
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
            VBuffer.SetNormal3(i,Vector3f.ZERO);
        }

        int iTQuantity = GetTriangleQuantity();
        Vector3f rkV0 = new Vector3f();
        Vector3f rkV1 = new Vector3f();
        Vector3f rkV2 = new Vector3f();

        Vector3f rkN0 = new Vector3f();
        Vector3f rkN1 = new Vector3f();
        Vector3f rkN2 = new Vector3f();
        
        Vector3f kNormal = new Vector3f();
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
            rkV0.SetData( VBuffer.GetPosition3fX(iV0), VBuffer.GetPosition3fY(iV0), VBuffer.GetPosition3fZ(iV0));
            rkV1.SetData( VBuffer.GetPosition3fX(iV1), VBuffer.GetPosition3fY(iV1), VBuffer.GetPosition3fZ(iV1));
            rkV2.SetData( VBuffer.GetPosition3fX(iV2), VBuffer.GetPosition3fY(iV2), VBuffer.GetPosition3fZ(iV2));

            // compute the normal (length provides the weighted sum)
            rkV1.subEquals( rkV0 );
            rkV2.subEquals( rkV0 );
            rkV1.CrossEquals(rkV2);
            kNormal.SetData(rkV1);
            
            rkN0.SetData( VBuffer.GetNormal3fX(iV0), VBuffer.GetNormal3fY(iV0), VBuffer.GetNormal3fZ(iV0) );
            rkN0.addEquals(kNormal);
            VBuffer.SetNormal3(iV0, rkN0);

            rkN1.SetData( VBuffer.GetNormal3fX(iV1), VBuffer.GetNormal3fY(iV1), VBuffer.GetNormal3fZ(iV1) );
            rkN1.addEquals(kNormal);
            VBuffer.SetNormal3(iV1, rkN1);
            
            rkN2.SetData( VBuffer.GetNormal3fX(iV2), VBuffer.GetNormal3fY(iV2), VBuffer.GetNormal3fZ(iV2) );
            rkN2.addEquals(kNormal);
            VBuffer.SetNormal3(iV2, rkN2);
        }

        rkV0 = null;
        rkV1 = null;
        rkV2 = null;

        rkN0 = null;
        rkN1 = null;
        rkN2 = null;

        Vector3f kNormalized = new Vector3f();
        for (i = 0; i < iVQuantity; i++)
        {
            kNormalized.SetData( VBuffer.GetNormal3fX(i), VBuffer.GetNormal3fY(i), VBuffer.GetNormal3fZ(i));
            kNormalized.Normalize();
            VBuffer.SetNormal3(i, kNormalized);
        }
        kNormalized = null;
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
        pkTree.Append(StringTree.Format("Triangles",GetName()));
        pkTree.Append(super.SaveStrings(null));
        return pkTree;
    }
}
