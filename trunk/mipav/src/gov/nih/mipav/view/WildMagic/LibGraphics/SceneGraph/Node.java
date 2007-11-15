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

import java.util.Vector;

import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;

public class Node extends Spatial
    implements NameIdInterface, StreamInterface
{
    /** Default construction. */
    public Node () {}

    /** Delete memory. */
    public void dispose ()
    {
        for (int i = 0; i < (int)m_kChild.size(); i++)
        {
            Spatial spkChild = DetachChildAt(i);
            if ( spkChild != null )
            {
                spkChild.dispose();
                spkChild = null;
            }
        }
        super.dispose();
    }

    /** children
     * @return number of children nodes.
     */
    public final int GetQuantity ()
    {
        return (int)m_kChild.size();
    }

    /** children
     * @param pkChild, child node to add.
     * @return number of children nodes.
     */
    public int AttachChild (Spatial pkChild)
    {
        // Some folks are under the impression that a node can have multiple
        // parents, the scene graph therefore being a DAG.  That is not the case.
        // The parent-child relationships form a tree.  This assertion is to let
        // folks know this and to warn them that a child is being kidnapped from
        // another parent.  To be safe, you should really call DetachChild before
        // you reattach somewhere else with AttachChild or SetChild.  If you do
        // call DetachChild first, be aware that the child might self-destruct.
        // If you want this not to happen, hang onto the child via a smart
        // pointer.  For example,
        //
        //     Node* pkNode0 = WM4_NEW Node;
        //     Spatial* pkChild0 = <...>;
        //     pkNode0->AttachChild(pkChild0);  // child at index 0
        //     Node* pkNode1 = <...>;
        //
        //     // This asserts because pkChild0 already has a parent (pkNode0).
        //     pkNode1->AttachChild(pkChild0);
        //
        //     // Instead do this and avoid the potential self-destruction of
        //     // pkChild0).
        //     SpatialPtr spkSaveChild = pkNode0->GetChild(0);
        //     pkNode0->DetachChild(spkSaveChild);
        //     pkNode1->AttachChild(spkSaveChild);

        assert((pkChild != null) && (pkChild.GetParent() == null));

        pkChild.SetParent(this);

        // attach child in first available slot (if any)
        int iQuantity = (int)m_kChild.size();
        for (int i = 0; i < iQuantity; i++)
        {
            if (m_kChild.get(i) == null)
            {
                m_kChild.set(i, pkChild);
                return i;
            }
        }

        // all slots used, increase array size
        m_kChild.add(pkChild);
        return iQuantity;
    }

    /** children
     * @param pkChild, child node to detach.
     * @return position of child node, or -1.
     */
    public int DetachChild (Spatial pkChild)
    {
        if (pkChild != null)
        {
            // search to see if child exists
            for (int i = 0; i < (int)m_kChild.size(); i++)
            {
                if (m_kChild.get(i) == pkChild)
                {
                    // child found, detach it
                    pkChild.SetParent(null);
                    m_kChild.set(i, null);
                    return i;
                }
            }
        }

        return -1;
    }

    /** children
     * @param i, position of child node to detach.
     * @return detached child node.
     */
    public Spatial DetachChildAt (int i)
    {
        if (0 <= i && i < (int)m_kChild.size())
        {
            Spatial spkChild = m_kChild.get(i);
            if (spkChild != null)
            {
                // child exists in slot, detach it
                spkChild.SetParent(null);
                m_kChild.set(i, null);
            }
            return spkChild;
        }
        return null;
    }

    /** children
     * @param i, position of child node to set.
     * @param pkChild, child node.
     * @return previous child node at position i, or null.
     */
    public Spatial SetChild (int i, Spatial pkChild)
    {
        // Some folks are under the impression that a node can have multiple
        // parents, the scene graph therefore being a DAG.  That is not the case.
        // The parent-child relationships form a tree.  This assertion is to let
        // folks know this and to warn them that a child is being kidnapped from
        // another parent.  To be safe, you should really call DetachChild before
        // you reattach somewhere else with AttachChild or SetChild.  If you do
        // call DetachChild first, be aware that the child might self-destruct.
        // If you want this not to happen, hang onto the child via a smart
        // pointer.  For example,
        //
        //     Node* pkNode0 = WM4_NEW Node;
        //     Spatial* pkChild0 = <...>;
        //     pkNode0->AttachChild(pkChild0);  // child at index 0
        //     Node* pkNode1 = <...>;
        //
        //     // This asserts because pkChild0 already has a parent (pkNode0).
        //     pkNode1->AttachChild(pkChild0);
        //
        //     // Instead do this and avoid the potential self-destruction of
        //     // pkChild0).
        //     SpatialPtr spkSaveChild = pkNode0->GetChild(0);
        //     pkNode0->DetachChild(spkSaveChild);
        //     pkNode1->AttachChild(spkSaveChild);

        if (pkChild != null)
        {
            assert(pkChild.GetParent() == null);
        }

        if (0 <= i && i < (int)m_kChild.size())
        {
            // detach child currently in slot
            Spatial spkPreviousChild = m_kChild.get(i);
            if (spkPreviousChild != null)
            {
                spkPreviousChild.SetParent(null);
            }

            // attach new child to slot
            if (pkChild != null)
            {
                pkChild.SetParent(this);
            }

            m_kChild.set(i, pkChild);
            return spkPreviousChild;
        }

        // index out of range, increase array size and attach new child
        pkChild.SetParent(this);
        m_kChild.add(pkChild);
        return null;
    }

    /** children
     * @param i, position of child node to set.
     * @return child node at position i, or null.
     */
    public Spatial GetChild (int i)
    {
        if (0 <= i && i < (int)m_kChild.size())
        {
            return m_kChild.get(i);
        }
        return null;
    }

    /** geometric updates
     * @param dAppTime, animation time step from application.
     */
    protected void UpdateWorldData (double dAppTime)
    {
        super.UpdateWorldData(dAppTime);

        for (int i = 0; i < (int)m_kChild.size(); i++)
        {
            Spatial pkChild = m_kChild.get(i);
            if (pkChild != null)
            {
                pkChild.UpdateGS(dAppTime,false);
            }
        }
    }

    /** geometric updates */
    protected void UpdateWorldBound ()
    {
        if (!WorldBoundIsCurrent)
        {
            boolean bFoundFirstBound = false;
            for (int i = 0; i < (int)m_kChild.size(); i++)
            {
                Spatial pkChild = m_kChild.get(i);
                if (pkChild != null)
                {
                    if (bFoundFirstBound)
                    {
                        // merge current world bound with child world bound
                        WorldBound.GrowToContain(pkChild.WorldBound);
                    }
                    else
                    {
                        // set world bound to first non-null child world bound
                        bFoundFirstBound = true;
                        WorldBound.CopyFrom(pkChild.WorldBound);
                    }
                }
            }
        }
    }

    /** render state updates 
     * @param akGStack, global states.
     * @param akLStack, lights.
     */
    protected void UpdateState ( Vector<Vector<GlobalState>> akGStack,
                                 Vector<Light> pkLStack)
    {
        for (int i = 0; i < (int)m_kChild.size(); i++)
        {
            Spatial pkChild = m_kChild.get(i);
            if (pkChild != null)
            {
                pkChild.UpdateRS(akGStack,pkLStack);
            }
        }
    }


    /** culling
     * @param rkCuller, culling object applied to this node.
     * @param bNoCull, when true don't cull.
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

        // All Geometry objects in the subtree are added to the visible set.  If
        // a global effect is active, the Geometry objects in the subtree will be
        // drawn using it.
        for (i = 0; i < (int)m_kChild.size(); i++)
        {
            Spatial pkChild = m_kChild.get(i);
            if (pkChild != null)
            {
                pkChild.OnGetVisibleSet(rkCuller,bNoCull);
            }
        }

        for (i = 0; i < (int)m_kEffects.size(); i++)
        {
            // Place an 'end' marker in the visible set to indicate that the
            // global effect is inactive.
            rkCuller.Insert(null,null);
        }
    }

    /** children */
    protected Vector<Spatial> m_kChild = new Vector<Spatial>();

    /**
     * Returns the GraphicsObject with the name that matches the input paramter, rkName.
     * @param rkName, the name of the object to return.
     * @return the GraphicsObject that matches the input name.
     */
    public GraphicsObject GetObjectByName (final String rkName)
    {
        GraphicsObject pkFound = super.GetObjectByName(rkName);
        if (pkFound != null)
        {
            return pkFound;
        }

        for (int i = 0; i < (int)m_kChild.size(); i++)
        {
            Spatial pkChild = m_kChild.get(i);
            if (pkChild != null)
            {
                pkFound = pkChild.GetObjectByName(rkName);
                if (pkFound != null)
                {
                    return pkFound;
                }
            }
        }

        return null;
    }

    /**
     * Writes all GraphicsObjects with the name that matches the input
     * paramter, rkName into the Vector paramter rkObjects.
     * @param rkName, the name of the objects to return.
     * @param rkObjects, a Vector of all objects with the matching name.
     */
    public void GetAllObjectsByName (final String rkName,
                                     Vector<GraphicsObject> rkObjects)
    {
        super.GetAllObjectsByName(rkName,rkObjects);

        for (int i = 0; i < (int)m_kChild.size(); i++)
        {
            Spatial pkChild = m_kChild.get(i);
            if (pkChild != null)
            {
                pkChild.GetAllObjectsByName(rkName,rkObjects);
            }
        }
    }

    /**
     * Returns the GraphicsObject with the ID that matches the input paramter, uiID.
     * @param uiID, the ID of the object to return.
     * @return the GraphicsObject that matches the input name.
     */
    public GraphicsObject GetObjectByID (int uiID)
    {
        GraphicsObject pkFound = super.GetObjectByID(uiID);
        if (pkFound != null)
        {
            return pkFound;
        }

        for (int i = 0; i < (int)m_kChild.size(); i++)
        {
            Spatial pkChild = m_kChild.get(i);
            if (pkChild != null)
            {
                pkFound = pkChild.GetObjectByID(uiID);
                if (pkFound != null)
                {
                    return pkFound;
                }
            }
        }

        return null;
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

        // link data
        int iQuantity = rkStream.ReadInt();
        m_kChild.setSize(iQuantity);
        for (int i = 0; i < iQuantity; i++)
        {
            int iLinkID = rkStream.ReadInt();
            pkLink.Add(iLinkID);
        }
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

        for (int i = 0; i < m_kChild.size(); i++)
        {
            int iLinkID = pkLink.GetLinkID();
            Spatial pkChild = (Spatial)rkStream.GetFromMap(iLinkID);
            if (pkChild != null)
            {
                SetChild(i,pkChild);
            }
        }
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
        if (!super.Register(rkStream))
        {
            return false;
        }

        for (int i = 0; i < m_kChild.size(); i++)
        {
            if (m_kChild.get(i) != null)
            {
                m_kChild.get(i).Register(rkStream);
            }
        }

        return true;
    }

    /**
     * Write this object and all it's children to the Stream.
     * @param rkStream, the Stream where the child objects are stored.
     */
    public void Save (Stream rkStream)
    {
        super.Save(rkStream);

        // link data
        rkStream.Write(m_kChild.size());
        for (int i = 0; i < m_kChild.size(); i++)
        {
            rkStream.Write(m_kChild.get(i).GetID());
        }
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
            Stream.SIZEOF_INT + //sizeof(int) +  // (int)m_kChild.size()
            (m_kChild.size())*Stream.SIZEOF_INT; //sizeof(m_kChild[0]);
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
        pkTree.Append(StringTree.Format("Node",GetName()));
        pkTree.Append(super.SaveStrings(null));
        pkTree.Append(StringTree.Format("child quantity =",(int)m_kChild.size()));

        // children
        for (int i = 0; i < (int)m_kChild.size(); i++)
        {
            Spatial pkChild = m_kChild.get(i);
            if (pkChild != null)
            {
                pkTree.Append(pkChild.SaveStrings(null));
            }
            else
            {
                StringTree pkEmpty = new StringTree();
                pkEmpty.Append(StringTree.Format("unused slot"));
                pkTree.Append(pkEmpty);
            }
        }

        return pkTree;
    }
}
