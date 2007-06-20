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
// Version: 4.0.1 (2006/08/07)

package gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph;

import java.util.Vector;

import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;

public class Node extends Spatial
    implements NameIdInterface, StreamInterface
{
    // construction and destruction
    public Node () {}
    public void finalize ()
    {
        for (int i = 0; i < (int)m_kChild.size(); i++)
        {
            Spatial spkChild = DetachChildAt(i);
            spkChild = null;
        }
        super.finalize();
    }

    // children
    public int GetQuantity ()
    {
        return (int)m_kChild.size();
    }

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

    public Spatial GetChild (int i)
    {
        if (0 <= i && i < (int)m_kChild.size())
        {
            return m_kChild.get(i);
        }
        return null;
    }

    // geometric updates
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

    // render state updates
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


    // culling
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

    // children
    protected Vector<Spatial> m_kChild = new Vector<Spatial>();

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

    public int GetDiskUsed (StreamVersion rkVersion)
    {
        return super.GetDiskUsed(rkVersion) +
            Stream.SIZEOF_INT + //sizeof(int) +  // (int)m_kChild.size()
            (m_kChild.size())*Stream.SIZEOF_INT; //sizeof(m_kChild[0]);
    }

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
