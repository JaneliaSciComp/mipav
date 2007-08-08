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
    // member access
    public VertexBuffer VBuffer;
    public IndexBuffer IBuffer;
    public BoundingVolume ModelBound;

    // geometric updates
    public void UpdateMS ()
    {
        this.UpdateMS (true);
    }

    public void UpdateMS (boolean bUpdateNormals)
    {
        UpdateModelBound();
        if (bUpdateNormals)
        {
            UpdateModelNormals();
        }
    }

    public Geometry ()
    {
        ModelBound = new SphereBV();
    }

    protected Geometry (VertexBuffer pkVBuffer, IndexBuffer pkIBuffer)
    {
        VBuffer = pkVBuffer;
        IBuffer = pkIBuffer;
        ModelBound = new SphereBV();
        UpdateModelBound();
    }

    public void finalize()
    {
        if ( ModelBound != null )
        {
            ModelBound.finalize();
            ModelBound = null;
        }
        if ( m_spkLEffect != null )
        {
            m_spkLEffect.finalize();
            m_spkLEffect = null;
        }

        if ( States != null )
        {
            for ( int i = 0; i < GlobalState.StateType.MAX_STATE_TYPE.Value(); i++ )
            {
                if ( States[i] != null )
                {
                    States[i].finalize();
                    States[i] = null;
                }
            }
            States = null;
        }
        if ( HWorld != null )
        {
            HWorld.finalize();
            HWorld = null;
        }

        if ( VBuffer != null )
        {
            VBuffer.finalize();
            VBuffer = null;
        }
        if ( IBuffer != null )
        {
            IBuffer.finalize();
            IBuffer = null;
        }

        super.finalize();
    }

    // geometric updates
    protected void UpdateModelBound ()
    {
        ModelBound.ComputeFromData(VBuffer);
    }

    protected void UpdateModelNormals ()
    {
        // stub for derived classes
    }

    protected void UpdateWorldBound ()
    {
        ModelBound.TransformBy(World,WorldBound);
    }

    // render state updates
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


    // culling
    protected void GetVisibleSet (Culler rkCuller, boolean bNoCull)
    {
        rkCuller.Insert(this,null);
    }


    // Dynamic lighting.  The effect is pushed onto the front of Spatial's
    // effect array so that lighting occurs before other shader effects.
    protected LightingEffect m_spkLEffect;

    // internal use
    // Compute the homogeneous world matrix from the components of the World
    // transformation.
    public void UpdateWorldData (double dAppTime)
    {
        super.UpdateWorldData(dAppTime);
        World.GetHomogeneous(HWorld);
    }


    // Render state and lights in path to this object.  An attached effect
    // provides additional render state, lights, and any other information
    // needed to draw the object.

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

    public GeometryType Type;
    public GlobalState[] States = new GlobalState[GlobalState.StateType.MAX_STATE_TYPE.Value()];
    public Matrix4f HWorld = new Matrix4f();  // homogeneous world matrix


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

    public void Save (Stream rkStream)
    {
        super.Save(rkStream);

        // link data
        rkStream.Write(ModelBound.GetID());
        rkStream.Write(VBuffer.GetID());
        rkStream.Write(IBuffer.GetID());
    }

    public int GetDiskUsed (StreamVersion rkVersion)
    {
        return super.GetDiskUsed(rkVersion) +
            Stream.SIZEOF_INT + //sizeof(ModelBound) +
            Stream.SIZEOF_INT + //sizeof(VBuffer) +
            Stream.SIZEOF_INT; //sizeof(IBuffer);
    }

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
