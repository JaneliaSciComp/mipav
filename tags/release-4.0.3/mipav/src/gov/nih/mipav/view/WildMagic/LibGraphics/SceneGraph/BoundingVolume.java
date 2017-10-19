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

public abstract class BoundingVolume extends GraphicsObject
    implements StreamInterface
{
    /** run-time type information */
    public enum BVType
    {
        BV_SPHERE,
        BV_BOX,
        BV_QUANTITY
    };
    /** Get the type of BoundingVolume. */
    public abstract BVType GetBVType ();

    /** all bounding volumes must define a center and radius
     * @param rkCenter, center of bounding volume.
     */
    public abstract void SetCenter (Vector3f rkCenter);
    /** all bounding volumes must define a center and radius
     * @param fRadius, radius of bounding volume.
     */
    public abstract void SetRadius (float fRadius);
    /** all bounding volumes must define a center and radius
     * @return center of bounding volume.
     */
    public abstract Vector3f GetCenter ();
    /** all bounding volumes must define a center and radius
     * @return radius of bounding volume.
     */
    public abstract float GetRadius ();

    /** Compute a bounding volume that contains all the points.
     * @param pkVertices, array of points.
     */
    public abstract void ComputeFromData ( final Vector3f[] pkVertices);
    /** Compute a bounding volume that contains all the points.
     * @param pkVBuffer, VertexBuffer containing points.
     */
    public abstract void ComputeFromData ( VertexBuffer pkVBuffer);

    /** Transform the bounding volume (model-to-world conversion).
     * @param rkTransform, transformation
     * @param pkResult, updated bounding volume.
     */
    public abstract void TransformBy ( Transformation rkTransform,
                                       BoundingVolume pkResult);

    /** Determine if the bounding volume is one side of the plane, the other
     * side, or straddles the plane.  If it is on the positive side (the side
     * to which the normal points), the return value is +1.  If it is on the
     * negative side, the return value is -1.  If it straddles the plane, the
     * return value is 0.
     * @param rkPlane, plane to test against bounding volume.
     * @return positive side: +1; negative side: -1; straddle: 0.
     */
    public abstract int WhichSide ( Plane3f rkPlane);

    /** Test for intersection of linear component and bound (points of
     * intersection not computed).  The linear component is parameterized by
     * P + t*D, where P is a point on the component and D is a unit-length
     * direction.  The interval [tmin,tmax] is
     *   line:     tmin = -Mathf::MAX_REAL, tmax = Mathf::MAX_REAL
     *   ray:      tmin = 0.0f, tmax = Mathf::MAX_REAL
     *   segment:  tmin = 0.0f, tmax > 0.0f
     * @param rkOrigin, origin of point.
     * @param rkDirection, unit-length direction.
     * @param fTMin, min value for parameter t.
     * @param fTMax, miax value for parameter t.
     * @return true if intersects.
     */
    public abstract boolean TestIntersection (Vector3f rkOrigin,
                                              Vector3f rkDirection, float fTMin, float fTMax);

    /** Test for intersection of the two bounds.
     * @param pkInput, BoundingVolume.
     * @return true if intersects.
     */
    public abstract boolean TestIntersection (BoundingVolume pkInput);

    /** Make a copy of the bounding volume.
     * @param pkInput, bounding volume to copy into this.
     */
    public abstract void CopyFrom (BoundingVolume pkInput);

    /** Change the current bounding volume so that it contains the input
     * bounding volume as well as its old bounding volume.
     * @param pkInput, bounding volume to grow and contain with.
     */
    public abstract void GrowToContain (BoundingVolume pkInput);

    /** test for containment of a point
     * @param rkPoint, point to test.
     * @return true if contained in this volume..
     */
    public abstract boolean Contains (Vector3f rkPoint);

    /** streaming constructor. */
    public BoundingVolume () {}

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
        pkTree.Append(StringTree.Format("BoundingVolume",GetName()));
        pkTree.Append(super.SaveStrings(null));
        return pkTree;
    }
}