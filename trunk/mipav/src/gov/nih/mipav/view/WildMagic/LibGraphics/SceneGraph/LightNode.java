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

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;

public class LightNode extends Node
    implements NameIdInterface, StreamInterface
{
    // Construction and destruction.  The node's world translation is used
    // as the light's location.  The node's world rotation matrix is used
    // for the light's coordinate axes.  Column 0 of the world rotation
    // matrix is the light's direction vector, column 1 of the world rotation
    // matrix is the light's up vector, and column 2 of the world rotation
    // matrix is the light's right vector.
    //
    // On construction, the node's local transformation is set to the
    // light's current coordinate system.
    //   local translation       = light location
    //   local rotation column 0 = light direction
    //   local rotation column 1 = light up
    //   local rotation column 2 = light right
    public LightNode ()
    {
        m_spkLight = null;
    }

    public LightNode (Light pkLight)
    {
        m_spkLight = pkLight;
        if (m_spkLight != null)
        {
            Local.SetTranslate(m_spkLight.Position);
            Local.SetRotate( new Matrix3f(m_spkLight.DVector,m_spkLight.UVector,
                                     m_spkLight.RVector,true));
        }
    }

    // When you set the light, the node's local transformation is set to the
    // light's current current coordinate system.  The node's world
    // transformation is computed, and the light's coordinate system is set
    // to use the node's world transformation.
    public void SetLight (Light pkLight)
    {
        m_spkLight = pkLight;

        if (m_spkLight != null)
        {
            Local.SetTranslate(m_spkLight.Position);
            Local.SetRotate( new Matrix3f(m_spkLight.DVector,m_spkLight.UVector,
                                     m_spkLight.RVector,true));
            UpdateGS();
        }
    }

    public Light GetLight ()
    {
        return m_spkLight;
    }

    // geometric updates
    public void UpdateWorldData (double dAppTime)
    {
        super.UpdateWorldData(dAppTime);

        if (m_spkLight != null)
        {
            m_spkLight.Position = World.GetTranslate();
            m_spkLight.DVector = World.GetRotate().GetColumn(0);
            m_spkLight.UVector = World.GetRotate().GetColumn(1);
            m_spkLight.RVector = World.GetRotate().GetColumn(2);
        }
    }

    protected Light m_spkLight;

    public GraphicsObject GetObjectByName (final String rkName)
    {
        GraphicsObject pkFound = super.GetObjectByName(rkName);
        if (pkFound != null)
        {
            return pkFound;
        }

        if (m_spkLight != null)
        {
            pkFound = m_spkLight.GetObjectByName(rkName);
            if (pkFound != null)
            {
                return pkFound;
            }
        }

        return null;
    }

    public void GetAllObjectsByName (final String rkName,
                                     Vector<GraphicsObject> rkObjects)
    {
        super.GetAllObjectsByName(rkName,rkObjects);

        if (m_spkLight != null)
        {
            m_spkLight.GetAllObjectsByName(rkName,rkObjects);
        }
    }

    public GraphicsObject GetObjectByID (int uiID)
    {
        GraphicsObject pkFound = super.GetObjectByID(uiID);
        if (pkFound != null)
        {
            return pkFound;
        }

        if (m_spkLight != null)
        {
            pkFound = m_spkLight.GetObjectByID(uiID);
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
        int iLinkID = rkStream.ReadInt();  // m_spkLight
        pkLink.Add(iLinkID);
    }

    public void Link (Stream rkStream, Stream.Link pkLink)
    {
        super.Link(rkStream,pkLink);

        int iLinkID = pkLink.GetLinkID();
        m_spkLight = (Light)rkStream.GetFromMap(iLinkID);
    }

    public boolean Register (Stream rkStream)
    {
        if (!super.Register(rkStream))
        {
            return false;
        }

        if (m_spkLight != null)
        {
            m_spkLight.Register(rkStream);
        }

        return true;
    }

    public void Save (Stream rkStream)
    {
        super.Save(rkStream);

        // link data
        rkStream.Write(m_spkLight.GetID());
    }

    public int GetDiskUsed (StreamVersion rkVersion)
    {
        return super.GetDiskUsed(rkVersion) +
            Stream.SIZEOF_INT; //sizeof(m_spkLight);
    }

    public StringTree SaveStrings (final String acTitle)
    {
        StringTree pkTree = new StringTree();
        // strings
        pkTree.Append(StringTree.Format("LightNode",GetName()));

        // children
        pkTree.Append(super.SaveStrings(null));
        if (m_spkLight != null)
        {
            pkTree.Append(m_spkLight.SaveStrings(null));
        }

        return pkTree;
    }
}
