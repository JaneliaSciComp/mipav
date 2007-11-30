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
    /** Construction and destruction.  The node's world translation is used
     * as the light's location.  The node's world rotation matrix is used
     * for the light's coordinate axes.  Column 0 of the world rotation
     * matrix is the light's direction vector, column 1 of the world rotation
     * matrix is the light's up vector, and column 2 of the world rotation
     * matrix is the light's right vector.
     *
     * On construction, the node's local transformation is set to the
     * light's current coordinate system.
     *   local translation       = light location
     *   local rotation column 0 = light direction
     *   local rotation column 1 = light up
     *   local rotation column 2 = light right
     */
    public LightNode ()
    {
        m_spkLight = null;
    }

    /** Construction and destruction.  The node's world translation is used
     * as the light's location.  The node's world rotation matrix is used
     * for the light's coordinate axes.  Column 0 of the world rotation
     * matrix is the light's direction vector, column 1 of the world rotation
     * matrix is the light's up vector, and column 2 of the world rotation
     * matrix is the light's right vector.
     *
     * On construction, the node's local transformation is set to the
     * light's current coordinate system.
     *   local translation       = light location
     *   local rotation column 0 = light direction
     *   local rotation column 1 = light up
     *   local rotation column 2 = light right
     * @param pkLight, light to add to this node.
     */
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
    /** Delete memory. */
    public void dispose()
    {
        if ( m_spkLight != null )
        {
            m_spkLight.dispose();
            m_spkLight = null;
        }
        super.dispose();
    }

    /** When you set the light, the node's local transformation is set to the
     * light's current current coordinate system.  The node's world
     * transformation is computed, and the light's coordinate system is set
     * to use the node's world transformation.
     * @param pkLight, light to add to this node.
     */
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

    /** Return light in this node.
     * @return light in this node.
     */
    public final Light GetLight ()
    {
        return m_spkLight;
    }

    /** geometric updates 
     * @param dAppTime, animation time step from application.
     */
    public void UpdateWorldData (double dAppTime)
    {
        super.UpdateWorldData(dAppTime);

        if (m_spkLight != null)
        {
            m_spkLight.Position = World.GetTranslate();
            if (m_spkLight.DVector == null)
            {
                m_spkLight.DVector = new Vector3f();
            }
            World.GetRotate().GetColumn(0,m_spkLight.DVector);
            if (m_spkLight.UVector == null)
            {
                m_spkLight.UVector = new Vector3f();
            }
            World.GetRotate().GetColumn(1,m_spkLight.UVector);
            if (m_spkLight.RVector == null)
            {
                m_spkLight.RVector = new Vector3f();
            }
            World.GetRotate().GetColumn(2,m_spkLight.RVector);
        }
    }

    /** light in this node. */
    protected Light m_spkLight;

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

        if (m_spkLight != null)
        {
            m_spkLight.GetAllObjectsByName(rkName,rkObjects);
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
        int iLinkID = rkStream.ReadInt();  // m_spkLight
        pkLink.Add(iLinkID);
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

        int iLinkID = pkLink.GetLinkID();
        m_spkLight = (Light)rkStream.GetFromMap(iLinkID);
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

        if (m_spkLight != null)
        {
            m_spkLight.Register(rkStream);
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
        rkStream.Write(m_spkLight.GetID());
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
            Stream.SIZEOF_INT; //sizeof(m_spkLight);
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
