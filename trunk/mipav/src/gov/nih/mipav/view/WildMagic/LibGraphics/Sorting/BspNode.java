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
// Version: 4.0.0 (2006/06/28)

package gov.nih.mipav.view.WildMagic.LibGraphics.Sorting;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;

public class BspNode extends Node
    implements StreamInterface
{
    /** Construction and destruction.  The base class Node has *three* children
     * and is not allowed to grow.  The first and last children (indices 0 and
     * 2) are the "positive" and "negative" children of the binary tree.  The
     * positive child corresponds to the positive side of the separating
     * plane.  The negative child corresponds to the negative side of the
     * separating plane.  The middle child slot is where additional geometry
     * may be attached such as the triangles that are coplanar with the
     * separating plane.
     */
    public BspNode ()
    {
        m_kModelPlane = new Plane3f(Vector3f.ZERO,0.0f);
        m_kChild.add(null);  // left child
        m_kChild.add(null);  // middle child
        m_kChild.add(null);  // right child
    }

    /** Construction and destruction.  The base class Node has *three* children
     * and is not allowed to grow.  The first and last children (indices 0 and
     * 2) are the "positive" and "negative" children of the binary tree.  The
     * positive child corresponds to the positive side of the separating
     * plane.  The negative child corresponds to the negative side of the
     * separating plane.  The middle child slot is where additional geometry
     * may be attached such as the triangles that are coplanar with the
     * separating plane.
     * @param rkModelPlane the space partitioning plane.
     */
    public BspNode (final Plane3f rkModelPlane)
    {
        m_kModelPlane = new Plane3f(rkModelPlane);
        m_kChild.add(null);  // left child
        m_kChild.add(null);  // middle child
        m_kChild.add(null);  // right child
    }

    /** These methods should be used instead of the attach/detach methods in
     * the Node base class.
     * @param pkChild the child to attach on the "positive" side.
     * @return The parent tree with the attached child.
     */
    public Spatial AttachPositiveChild (Spatial pkChild)
    {
        return SetChild(0,pkChild);
    }

    /** These methods should be used instead of the attach/detach methods in
     * the Node base class.
     * @param pkChild the child to attach on the "coplanar" side.
     * @return The parent tree with the attached child.
     */
    public Spatial AttachCoplanarChild (Spatial pkChild)
    {
        return SetChild(1,pkChild);
    }

    /** These methods should be used instead of the attach/detach methods in
     * the Node base class.
     * @param pkChild the child to attach on the "negative" side.
     * @return The parent tree with the attached child.
     */
    public Spatial AttachNegativeChild (Spatial pkChild)
    {
        return SetChild(2,pkChild);
    }

    /** These methods should be used instead of the attach/detach methods in
     * the Node base class.
     * @return The parent tree with the positive child detached.
     */
    public Spatial DetachPositiveChild ()
    {
        return DetachChildAt(0);
    }

    /** These methods should be used instead of the attach/detach methods in
     * the Node base class.
     * @return The parent tree with the coplanar child detached.
     */
    public Spatial DetachCoplanarChild ()
    {
        return DetachChildAt(1);
    }

    /** These methods should be used instead of the attach/detach methods in
     * the Node base class.
     * @return The parent tree with the negative child detached.
     */
    public Spatial DetachNegativeChild ()
    {
        return DetachChildAt(2);
    }

    /** Returns the positive subtree
     * @return positive child.
     */
    public Spatial GetPositiveChild ()
    {
        return GetChild(0);
    }

    /** Returns the coplanar subtree
     * @return coplanar child.
     */
    public Spatial GetCoplanarChild ()
    {
        return GetChild(1);
    }

    /** Returns the negative subtree
     * @return negative child.
     */
    public Spatial GetNegativeChild ()
    {
        return GetChild(2);
    }


    /** plane access
     * @return the ModelPlane
     */
    public Plane3f ModelPlane ()
    {
        return m_kModelPlane;
    }

    /** plane access
     * @return the ModelPlane
     */
    public Plane3f GetModelPlane ()
    {
        return m_kModelPlane;
    }

    /** plane access
     * @return the WorldPlane
     */
    public Plane3f GetWorldPlane ()
    {
        return m_kWorldPlane;
    }

    /** determine the portion of the scene that contains the point
     * @param rkPoint the point to locate in the scene
     * @return the subtree containing the point.
     */
    public Spatial GetContainingNode (final Vector3f rkPoint)
    {
        Spatial spkPChild = GetPositiveChild();
        Spatial spkNChild = GetNegativeChild();

        if ((spkPChild != null) || (spkNChild != null))
        {
            BspNode pkBspChild;

            if (m_kWorldPlane.WhichSide(rkPoint) < 0)
            {
                pkBspChild = (BspNode)(spkNChild);
                if (pkBspChild != null)
                {
                    return pkBspChild.GetContainingNode(rkPoint);
                }
                else
                {
                    return spkNChild;
                }
            }
            else
            {
                pkBspChild = (BspNode)(spkPChild);
                if (pkBspChild != null)
                {
                    return pkBspChild.GetContainingNode(rkPoint);
                }
                else
                {
                    return spkPChild;
                }
            }
        }

        return this;
    }


    /** geometric updates
     * @param dAppTime application time 
     */
    protected void UpdateWorldData (double dAppTime)
    {
        super.UpdateWorldData(dAppTime);
        m_kWorldPlane = World.ApplyForward(m_kModelPlane);
    }


    /** culling
     * @param rkCuller, the culling object
     * @param bNoCull culling flag on/off
     */
    protected void GetVisibleSet (Culler rkCuller, boolean bNoCull)
    {
        int i;
        for (i = 0; i < (int)m_kEffects.size(); i++)
        {
            // This is a global effect.  Place a 'begin' marker in the visible
            // set to indicate the effect is active.
            rkCuller.Insert(this,m_kEffects.get(i));
        }

        // Get visible Geometry in back-to-front order.  If a global effect is
        // active, the Geometry objects in the subtree will be drawn using it.
        Spatial spkPChild = GetPositiveChild();
        Spatial spkCChild = GetCoplanarChild();
        Spatial spkNChild = GetNegativeChild();

        final Camera pkCamera = rkCuller.GetCamera();
        int iLocSide = m_kWorldPlane.WhichSide(pkCamera.GetLocation());
        int iFruSide = rkCuller.WhichSide(m_kWorldPlane);

        if (iLocSide > 0)
        {
            // camera origin on positive side of plane

            if (iFruSide <= 0)
            {
                // The frustum is on the negative side of the plane or straddles
                // the plane.  In either case, the negative child is potentially
                // visible.
                if (spkNChild != null)
                {
                    spkNChild.OnGetVisibleSet(rkCuller,bNoCull);
                }
            }

            if (iFruSide == 0)
            {
                // The frustum straddles the plane.  The coplanar child is
                // potentially visible.
                if (spkCChild != null)
                {
                    spkCChild.OnGetVisibleSet(rkCuller,bNoCull);
                }
            }

            if (iFruSide >= 0)
            {
                // The frustum is on the positive side of the plane or straddles
                // the plane.  In either case, the positive child is potentially
                // visible.
                if (spkPChild != null)
                {
                    spkPChild.OnGetVisibleSet(rkCuller,bNoCull);
                }
            }
        }
        else if (iLocSide < 0)
        {
            // camera origin on negative side of plane

            if (iFruSide >= 0)
            {
                // The frustum is on the positive side of the plane or straddles
                // the plane.  In either case, the positive child is potentially
                // visible.
                if (spkPChild != null)
                {
                    spkPChild.OnGetVisibleSet(rkCuller,bNoCull);
                }
            }

            if (iFruSide == 0)
            {
                // The frustum straddles the plane.  The coplanar child is
                // potentially visible.
                if (spkCChild != null)
                {
                    spkCChild.OnGetVisibleSet(rkCuller,bNoCull);
                }
            }

            if (iFruSide <= 0)
            {
                // The frustum is on the negative side of the plane or straddles
                // the plane.  In either case, the negative child is potentially
                // visible.
                if (spkNChild != null)
                {
                    spkNChild.OnGetVisibleSet(rkCuller,bNoCull);
                }
            }
        }
        else
        {
            // Camera origin on plane itself.  Both sides of the plane are
            // potentially visible as well as the plane itself.  Select the
            // first-to-be-drawn half space to be the one to which the camera
            // direction points.
            float fNdD = m_kWorldPlane.Normal.Dot(pkCamera.GetDVector());
            if (fNdD >= 0.0f)
            {
                if (spkPChild != null)
                {
                    spkPChild.OnGetVisibleSet(rkCuller,bNoCull);
                }

                if (spkCChild != null)
                {
                    spkCChild.OnGetVisibleSet(rkCuller,bNoCull);
                }

                if (spkNChild != null)
                {
                    spkNChild.OnGetVisibleSet(rkCuller,bNoCull);
                }
            }
            else
            {
                if (spkNChild != null)
                {
                    spkNChild.OnGetVisibleSet(rkCuller,bNoCull);
                }

                if (spkCChild != null)
                {
                    spkCChild.OnGetVisibleSet(rkCuller,bNoCull);
                }

                if (spkPChild != null)
                {
                    spkPChild.OnGetVisibleSet(rkCuller,bNoCull);
                }
            }
        }

        for (i = 0; i < (int)m_kEffects.size(); i++)
        {
            // Place an 'end' marker in the visible set to indicate that the
            // global effect is inactive.
            rkCuller.Insert(null,null);
        }
    }

    /** ModelPlane */
    protected Plane3f m_kModelPlane;
    /** WorldPlane */
    protected Plane3f m_kWorldPlane;

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

        // native data
        rkStream.Read(m_kModelPlane);
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

        // native data
        rkStream.Write(m_kModelPlane);

        // world plane is computed from model plane in update, no need to save
    }

    /**
     * Returns the size of this object and it's children on disk for the
     * current StreamVersion parameter.
     * @param rkVersion, the current version of the Stream file being created.
     * @return the size of this object on disk.
     */
    public int GetDiskUsed (StreamVersion rkVersion)
    {
        return super.GetDiskUsed(rkVersion) + 
            3*Stream.SIZEOF_FLOAT + Stream.SIZEOF_FLOAT; //sizeof(m_kModelPlane);
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

        // strings
        pkTree.Append(StringTree.Format("BspNode",GetName()));
        pkTree.Append(super.SaveStrings(null));
        pkTree.Append(StringTree.Format("model plane =",m_kModelPlane));
        return pkTree;
    }
}
