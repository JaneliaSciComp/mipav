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

package gov.nih.mipav.view.WildMagic.LibGraphics.Detail;

import java.util.Vector;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;
import com.sun.opengl.util.BufferUtil;

/** The model space of the billboard has an up vector of (0,1,0) that is
 * chosen to be the billboard's axis of rotation.
 */
public class BillboardNode extends Node
    implements NameIdInterface, StreamInterface
{
    /** Construction and destruction.
     * @param pkCamera to which the billboard is aligned
     */
    public BillboardNode (Camera pkCamera)
    {
        m_spkCamera = pkCamera;
    }

    /** Default constructor, uninitialized */
    public BillboardNode () {}

    /**
     * delete memory
     */
    public void finalize()
    {
        if ( m_spkCamera != null )
        {
            m_spkCamera.finalize();
            m_spkCamera = null;
        }
    }

    /** the camera to which the billboard is aligned
     * @param pkCamera to which the billboard is aligned
     */
    public void AlignTo (Camera pkCamera)
    {
        m_spkCamera = pkCamera;
    }

    /** geometric updates
     * @param dAppTime animation time for updating any controllers associated
     * with this object
     */
    protected void UpdateWorldData (double dAppTime)
    {
        // Compute billboard's world transforms based on its parent's world
        // transform and its local transforms.  Notice that you should not call
        // Node::UpdateWorldData since that function updates its children.  The
        // children of a BillboardNode cannot be updated until the billboard is
        // aligned with the camera.
        super.UpdateWorldData(dAppTime);

        if (m_spkCamera != null)
        {
            // Inverse-transform the camera to the model space of the billboard.
            Vector3f kCLoc = World.ApplyInverse(m_spkCamera.GetLocation());

            // To align the billboard, the projection of the camera to the
            // xz-plane of the billboard's model space determines the angle of
            // rotation about the billboard's model y-axis.  If the projected
            // camera is on the model axis (x = 0 and z = 0), ATan2 returns zero
            // (rather than NaN), so there is no need to trap this degenerate
            // case and handle it separately.
            float fAngle = (float)Math.atan2(kCLoc.X(),kCLoc.Z());
            Matrix3f kOrient = new Matrix3f(Vector3f.UNIT_Y,fAngle);
            World.SetRotate(World.GetRotate().mult(kOrient));
        }

        // update the children now that the billboard orientation is known
        for (int i = 0; i < (int)m_kChild.size(); i++)
        {
            Spatial pkChild = m_kChild.get(i);
            if (pkChild != null)
            {
                pkChild.UpdateGS(dAppTime,false);
            }
        }
    }

    // Name-ID Interface:
    /**
     * Returns the GraphicsObject with the name that matches the input paramter, rkName.
     * @param rkName, the name of the object to return.
     * @return the GraphicsObject that matches the input name.
     */
    public GraphicsObject
        GetObjectByName (final String rkName)
    {
        GraphicsObject pkFound = GetObjectByNameBase(rkName);
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
        GraphicsObject pkFound = GetObjectByIDBase(uiID);
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

    // Streaming Interface:
    /**
     * Loads this object from the input parameter rkStream, using the input
     * Stream.Link to store the IDs of children objects of this object for
     * linking after all objects are loaded from the Stream.
     * @param rkStream, the Stream from which this object is being read.
     * @param pkLink, the Link class for storing the IDs of this object's
     * children objcts.
     */
    public void Load (Stream rkStream, Stream.Link pkLink)
    {
        super.Load(rkStream,pkLink);

        // link data
        int iObjectID = rkStream.ReadInt();  // m_spkCamera
        pkLink.Add(iObjectID);
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

        Integer iLinkID = pkLink.GetLinkID();
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
    public int GetDiskUsed (final StreamVersion rkVersion)
    {
        return super.GetDiskUsed(rkVersion) +
        BufferUtil.SIZEOF_INT;
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
        pkTree.Append(StringTree.Format("BillboardNode",GetName()));
                      
        // children
        pkTree.Append(super.SaveStrings(null));

        if (m_spkCamera != null)
        {
            pkTree.Append(m_spkCamera.SaveStrings(null));
        }
        
        return pkTree;
    }

    /** Camera to which the billboard is aligned */
    protected Camera m_spkCamera;
}
