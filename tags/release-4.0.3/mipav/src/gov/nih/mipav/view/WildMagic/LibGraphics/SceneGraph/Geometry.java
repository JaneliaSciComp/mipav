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
import gov.nih.mipav.view.WildMagic.LibGraphics.Effects.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;

public abstract class Geometry extends Spatial
    implements NameIdInterface, StreamInterface
{
    /** member access: VertexBuffer */
    public VertexBuffer VBuffer;
    /** member access: IndexBuffer */
    public IndexBuffer IBuffer;
    /** member access: BoundingVolume */
    public BoundingVolume ModelBound;

    /** geometric updates */
    public void UpdateMS ()
    {
        this.UpdateMS (true);
    }

    /** geometric updates
     * @param bUpdateNormals, if true call UpdateModelNormals().
     */
    public void UpdateMS (boolean bUpdateNormals)
    {
        UpdateModelBound();
        if (bUpdateNormals)
        {
            UpdateModelNormals();
        }
    }

    /** Default constructor, creates a SphereBV bounding volume. */
    public Geometry ()
    {
        ModelBound = new SphereBV();
    }

    /** Constructor, creates a SphereBV bounding volume.
     * @param pkVBuffer, VertexBuffer
     * @param pkIBuffer, IndexBuffer
     */
    protected Geometry (VertexBuffer pkVBuffer, IndexBuffer pkIBuffer)
    {
        VBuffer = pkVBuffer;
        IBuffer = pkIBuffer;
        ModelBound = new SphereBV();
        UpdateModelBound();
    }

    /** Constructor, creates a SphereBV bounding volume.
     * @param pkVBuffer, VertexBuffer
     * @param pkIBuffer, IndexBuffer
     */
    protected Geometry (VertexBuffer pkVBuffer, IndexBuffer pkIBuffer, BoundingVolume kBounds)
    {
        VBuffer = pkVBuffer;
        IBuffer = pkIBuffer;
        ModelBound = kBounds;
    }
   
    /** Constructor, creates a SphereBV bounding volume.
     * @param pkVBuffer, VertexBuffer
     * @param pkIBuffer, IndexBuffer
     */
    protected Geometry (Geometry kG)
    {
        VBuffer = kG.VBuffer;
        IBuffer = kG.IBuffer;
        ModelBound = new SphereBV( ((SphereBV)kG.GetModelBound()).GetSphere() );
    }

    /** Delete memory. */
    public void dispose()
    {
        if ( ModelBound != null )
        {
            ModelBound.dispose();
            ModelBound = null;
        }
        if ( m_spkLEffect != null )
        {
            m_spkLEffect.dispose();
            m_spkLEffect = null;
        }

        if ( States != null )
        {
            for ( int i = 0; i < GlobalState.StateType.MAX_STATE_TYPE.Value(); i++ )
            {
                if ( States[i] != null )
                {
                    States[i].dispose();
                    States[i] = null;
                }
            }
            States = null;
        }
        if ( HWorld != null )
        {
            HWorld.dispose();
            HWorld = null;
        }

        if ( VBuffer != null )
        {
            VBuffer.dispose();
            VBuffer = null;
        }
        if ( IBuffer != null )
        {
            IBuffer.dispose();
            IBuffer = null;
        }

        super.dispose();
    }
    
    public BoundingVolume GetModelBound()
    {
        return ModelBound;
    }

    /** geometric updates */
    protected void UpdateModelBound ()
    {
        ModelBound.ComputeFromData(VBuffer);
    }

    /** geometric updates */
    protected void UpdateModelNormals ()
    {
        // stub for derived classes
    }

    /** geometric updates */
    protected void UpdateWorldBound ()
    {
        ModelBound.TransformBy(World,WorldBound);
    }

    /** render state updates
     * @param akGStack, global states.
     * @param akLStack, lights.
     */
    protected void UpdateState ( Vector<Vector<GlobalState>> akGStack,
                                 Vector<Light> pkLStack)
    {
        // update global state
        int i;
        for (i = 0; i < GlobalState.StateType.MAX_STATE_TYPE.Value(); i++)
        {
            GlobalState pkGState = null;
            pkGState = akGStack.get(i).lastElement();
            States[i] = pkGState;
        }

        // update lights
        int iLQuantity = pkLStack.size();
        if (iLQuantity > 0)
        {
            if (m_spkLEffect != null)
            {
                m_spkLEffect.DetachAllLights();
            }
            else
            {
                m_spkLEffect = new LightingEffect();
                m_kEffects.add( 0, (Effect)(m_spkLEffect));
            }

            for (i = 0; i < iLQuantity; i++)
            {
                m_spkLEffect.AttachLight(pkLStack.get(i));
            }

            m_spkLEffect.Configure();
        }
        else
        {
            if (m_spkLEffect != null)
            {
                assert(m_kEffects.firstElement() == m_spkLEffect);
                m_kEffects.remove(0);
            }
            m_spkLEffect = null;
        }
    }


    /** culling
     * @param rkCuller, culling object applied to this geometry.
     * @param bNoCull (not currently used).
     */
    protected void GetVisibleSet (Culler rkCuller, boolean bNoCull)
    {
        rkCuller.Insert(this,null);
    }


    /** Dynamic lighting.  The effect is pushed onto the front of Spatial's
     * effect array so that lighting occurs before other shader effects.
     */
    protected LightingEffect m_spkLEffect;

    /** internal use
     * Compute the homogeneous world matrix from the components of the World
     * transformation.
     * @param dAppTime, animation time step from the application.
     */
    public void UpdateWorldData (double dAppTime)
    {
        super.UpdateWorldData(dAppTime);
        World.GetHomogeneous(HWorld);
    }

    /** Type of geometry: */
    public enum GeometryType
    {
        GT_POLYPOINT,
        GT_POLYLINE_SEGMENTS,
        GT_POLYLINE_OPEN,
        GT_POLYLINE_CLOSED,
        GT_TRIMESH,
        GT_TRISTRIP,
        GT_TRIFAN,
        GT_MAX_QUANTITY;

        GeometryType( )
        {
            m_iValue = Init();
        }
        private int Init ()
        {
            return m_iInitValue++;
        }
        public int Value() { return m_iValue; }
        private int m_iValue;
        private static int m_iInitValue = 0;
    };

    /** Type of geometry: */
    public GeometryType Type;
    /** Render state and lights in path to this object.  An attached effect
     * provides additional render state, lights, and any other information
     * needed to draw the object.
     */
    public GlobalState[] States = new GlobalState[GlobalState.StateType.MAX_STATE_TYPE.Value()];
    /** homogeneous world matrix */
    public Matrix4f HWorld = new Matrix4f();  


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

        if (ModelBound != null)
        {
            pkFound = ModelBound.GetObjectByName(rkName);
            if (pkFound != null)
            {
                return pkFound;
            }
        }

        if (VBuffer != null)
        {
            pkFound = VBuffer.GetObjectByName(rkName);
            if (pkFound != null)
            {
                return pkFound;
            }
        }

        if (IBuffer != null)
        {
            pkFound = IBuffer.GetObjectByName(rkName);
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

        if (ModelBound != null)
        {
            ModelBound.GetAllObjectsByName(rkName,rkObjects);
        }

        if (VBuffer != null)
        {
            VBuffer.GetAllObjectsByName(rkName,rkObjects);
        }

        if (IBuffer != null)
        {
            IBuffer.GetAllObjectsByName(rkName,rkObjects);
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

        if (ModelBound != null)
        {
            pkFound = ModelBound.GetObjectByID(uiID);
            if (pkFound != null)
            {
                return pkFound;
            }
        }

        if (VBuffer != null)
        {
            pkFound = VBuffer.GetObjectByID(uiID);
            if (pkFound != null)
            {
                return pkFound;
            }
        }

        if (IBuffer != null)
        {
            pkFound = IBuffer.GetObjectByID(uiID);
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
        int iLinkID = rkStream.ReadInt();  // ModelBound
        pkLink.Add(iLinkID);

        iLinkID = rkStream.ReadInt();  // VBuffer
        pkLink.Add(iLinkID);

        iLinkID = rkStream.ReadInt();  // IBuffer
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
        ModelBound = (BoundingVolume)rkStream.GetFromMap(iLinkID);

        iLinkID = pkLink.GetLinkID();
        VBuffer = (VertexBuffer)rkStream.GetFromMap(iLinkID);

        iLinkID = pkLink.GetLinkID();
        IBuffer = (IndexBuffer)rkStream.GetFromMap(iLinkID);
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

        if (ModelBound != null)
        {
            ModelBound.Register(rkStream);
        }

        if (VBuffer != null)
        {
            VBuffer.Register(rkStream);
        }

        if (IBuffer != null)
        {
            IBuffer.Register(rkStream);
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
        rkStream.Write(ModelBound.GetID());
        rkStream.Write(VBuffer.GetID());
        rkStream.Write(IBuffer.GetID());
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
            Stream.SIZEOF_INT + //sizeof(ModelBound) +
            Stream.SIZEOF_INT + //sizeof(VBuffer) +
            Stream.SIZEOF_INT; //sizeof(IBuffer);
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
        pkTree.Append(StringTree.Format("Geometry",GetName()));

        // children
        pkTree.Append(super.SaveStrings(null));

        if (ModelBound != null)
        {
            pkTree.Append(ModelBound.SaveStrings(null));
        }

        if (VBuffer != null)
        {
            pkTree.Append(VBuffer.SaveStrings("vertices"));
        }

        if (IBuffer != null)
        {
            pkTree.Append(IBuffer.SaveStrings("indices"));
        }

        return pkTree;
    }
}
