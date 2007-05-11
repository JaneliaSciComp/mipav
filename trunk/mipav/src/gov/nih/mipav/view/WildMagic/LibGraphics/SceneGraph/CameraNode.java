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

package gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph;

import java.util.Vector;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;

public class CameraNode extends Node
    implements NameIdInterface, StreamInterface
{
    // Construction and destruction.  The node's world translation is used
    // as the camera's location.  The node's world rotation matrix is used
    // for the camera's coordinate axes.  Column 0 of the world rotation
    // matrix is the camera's direction vector, column 1 of the world rotation
    // matrix is the camera's up vector, and column 2 of the world rotation
    // matrix is the camera's right vector.
    //
    // On construction, the node's local transformation is set to the
    // camera's current coordinate system.
    //   local translation       = camera location
    //   local rotation column 0 = camera direction
    //   local rotation column 1 = camera up
    //   local rotation column 2 = camera right
    public CameraNode () {}

    public CameraNode (Camera pkCamera)
    {
        m_spkCamera = pkCamera;
        if (m_spkCamera != null)
        {
            Local.SetTranslate(m_spkCamera.GetLocation());
            Local.SetRotate( new Matrix3f(m_spkCamera.GetDVector(),
                                          m_spkCamera.GetUVector(),m_spkCamera.GetRVector(),true));
        }
    }

    // When you set the camera, the node's local transformation is set to the
    // camera's current current coordinate system.  The node's world
    // transformation is computed, and the camera's coordinate system is set
    // to use the node's world transformation.
    public void SetCamera (Camera pkCamera)
    {
        m_spkCamera = pkCamera;

        if (m_spkCamera != null)
        {
            Local.SetTranslate(m_spkCamera.GetLocation());
            Local.SetRotate( new Matrix3f(m_spkCamera.GetDVector(),
                                          m_spkCamera.GetUVector(),m_spkCamera.GetRVector(),true));
            UpdateGS();
        }
    }

    public Camera GetCamera ()
    {
        return m_spkCamera;
    }

    // geometric updates
    public void UpdateWorldData (double dAppTime)
    {
        super.UpdateWorldData(dAppTime);

        if (m_spkCamera != null)
        {
            m_spkCamera.SetFrame(
                                 World.GetTranslate(),
                                 World.GetRotate().GetColumn(0),
                                 World.GetRotate().GetColumn(1),
                                 World.GetRotate().GetColumn(2));
        }
    }

    protected Camera m_spkCamera;

    public WmObject GetObjectByName (final String rkName)
    {
        WmObject pkFound = super.GetObjectByName(rkName);
        if (pkFound != null)
        {
            return pkFound;
        }

        if (m_spkCamera != null)
        {
            pkFound = m_spkCamera.GetObjectByName(rkName);
            if (pkFound != null)
            {
                return pkFound;
            }
        }

        return null;
    }

    public void GetAllObjectsByName (final String rkName,
                                     Vector<WmObject> rkObjects)
    {
        super.GetAllObjectsByName(rkName,rkObjects);

        if (m_spkCamera != null)
        {
            m_spkCamera.GetAllObjectsByName(rkName,rkObjects);
        }
    }

    public WmObject GetObjectByID (int uiID)
    {
        WmObject pkFound = super.GetObjectByID(uiID);
        if (pkFound != null)
        {
            return pkFound;
        }

        if (m_spkCamera != null)
        {
            pkFound = m_spkCamera.GetObjectByID(uiID);
            if (pkFound != null)
            {
                return pkFound;
            }
        }

        return null;
    }

    public void Load (Stream rkStream, Stream.Link pkLink)
    {
        super.Load(rkStream,pkLink);

        // link data
        int iLinkID = rkStream.ReadInt();  // m_spkCamera
        pkLink.Add(iLinkID);
    }

    public void Link (Stream rkStream, Stream.Link pkLink)
    {
        super.Link(rkStream,pkLink);

        int iLinkID = pkLink.GetLinkID();
        m_spkCamera = (Camera)rkStream.GetFromMap(iLinkID);
    }

    public boolean Register (Stream rkStream)
    {
        if (!super.Register(rkStream))
        {
            return false;
        }

        if (m_spkCamera != null)
        {
            m_spkCamera.Register(rkStream);
        }

        return true;
    }

    public void Save (Stream rkStream)
    {
        super.Save(rkStream);

        // link data
        rkStream.Write(m_spkCamera.GetID());
    }

    public int GetDiskUsed (StreamVersion rkVersion)
    {
        return super.GetDiskUsed(rkVersion) + 
            Stream.SIZEOF_INT; //sizeof(m_spkCamera);
    }

    public StringTree SaveStrings (final String acTitle)
    {
        StringTree pkTree = new StringTree();
        // strings
        pkTree.Append(StringTree.Format("CameraNode",GetName()));

        // children
        pkTree.Append(super.SaveStrings(null));
        if (m_spkCamera != null)
        {
            pkTree.Append(m_spkCamera.SaveStrings(null));
        }

        return pkTree;
    }
}
