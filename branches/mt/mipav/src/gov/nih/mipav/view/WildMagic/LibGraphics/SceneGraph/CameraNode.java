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

public class CameraNode extends Node
    implements NameIdInterface, StreamInterface
{
    /** Construction and destruction.  The node's world translation is used
     * as the camera's location.  The node's world rotation matrix is used
     * for the camera's coordinate axes.  Column 0 of the world rotation
     * matrix is the camera's direction vector, column 1 of the world rotation
     * matrix is the camera's up vector, and column 2 of the world rotation
     * matrix is the camera's right vector.
     *
     * On construction, the node's local transformation is set to the
     * camera's current coordinate system.
     *   local translation       = camera location
     *   local rotation column 0 = camera direction
     *   local rotation column 1 = camera up
     *   local rotation column 2 = camera right
     */
    public CameraNode () {}

    /** Construction and destruction.  The node's world translation is used
     * as the camera's location.  The node's world rotation matrix is used
     * for the camera's coordinate axes.  Column 0 of the world rotation
     * matrix is the camera's direction vector, column 1 of the world rotation
     * matrix is the camera's up vector, and column 2 of the world rotation
     * matrix is the camera's right vector.
     *
     * On construction, the node's local transformation is set to the
     * camera's current coordinate system.
     *   local translation       = camera location
     *   local rotation column 0 = camera direction
     *   local rotation column 1 = camera up
     *   local rotation column 2 = camera right
     * @param pkCamera, new Camera.
     */
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

    /** When you set the camera, the node's local transformation is set to the
     * camera's current current coordinate system.  The node's world
     * transformation is computed, and the camera's coordinate system is set
     * to use the node's world transformation.
     * @param pkCamera, new Camera.
     */
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
    /** Return camera contained in this node.
     * @return camera contained in this node.
     */
    public final Camera GetCamera ()
    {
        return m_spkCamera;
    }

    /** geometric updates
     * @param dAppTime, animation time step from application.
     */
    public void UpdateWorldData (double dAppTime)
    {
        super.UpdateWorldData(dAppTime);

        if (m_spkCamera != null)
        {
            Vector3f kD = new Vector3f();
            World.GetRotate().GetColumn(0, kD);
            Vector3f kU = new Vector3f();
            World.GetRotate().GetColumn(1, kU);
            Vector3f kR = new Vector3f();
            World.GetRotate().GetColumn(2, kR);
            m_spkCamera.SetFrame(
                                 World.GetTranslate(),
                                 kD,kU,kR);
        }
    }

    /** Camera contained in this node. */
    protected Camera m_spkCamera;

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

        if (m_spkCamera != null)
        {
            m_spkCamera.GetAllObjectsByName(rkName,rkObjects);
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
        int iLinkID = rkStream.ReadInt();  // m_spkCamera
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
        m_spkCamera = (Camera)rkStream.GetFromMap(iLinkID);
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

        if (m_spkCamera != null)
        {
            m_spkCamera.Register(rkStream);
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
        rkStream.Write(m_spkCamera.GetID());
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
            Stream.SIZEOF_INT; //sizeof(m_spkCamera);
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
