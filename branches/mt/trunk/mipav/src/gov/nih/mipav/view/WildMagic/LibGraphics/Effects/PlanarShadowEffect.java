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

package gov.nih.mipav.view.WildMagic.LibGraphics.Effects;

import java.util.Vector;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;

import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;

public class PlanarShadowEffect extends Effect
    implements NameIdInterface, StreamInterface
{
    /** Creates a new PlanarShadowEffect for the number of planes
     * specified.
     * @param iQuantity the number of reflection planes.
     */
    public PlanarShadowEffect (int iQuantity)
    {
        assert(iQuantity > 0);
        m_iQuantity = iQuantity;
        m_aspkPlane = new TriMesh[m_iQuantity];
        m_aspkProjector = new Light[m_iQuantity];
        m_akShadowColor = new ColorRGBA[m_iQuantity];

        m_spkAState = new AlphaState();
        m_spkMState = new MaterialState();
        m_spkSState = new StencilState();
        m_spkZState = new ZBufferState();
        m_spkMEffect = new MaterialEffect();
    }

    /** Delete data members: */
    public void finalize ()
    {
        for ( int i = 0; i < m_iQuantity; i++ )
        {
            m_aspkPlane[i].finalize();
            m_aspkPlane[i] = null;
            m_aspkProjector[i].finalize();
            m_aspkProjector[i] = null;
            m_akShadowColor[i].finalize();
            m_akShadowColor[i] = null;
        }
        m_aspkPlane = null;
        m_akShadowColor = null;

        if ( m_spkAState != null )
        {
            m_spkAState.finalize();
            m_spkAState = null;
        }
        if ( m_spkSState != null )
        {
            m_spkSState.finalize();
            m_spkSState = null;
        }
        if ( m_spkZState != null )
        {
            m_spkZState.finalize();
            m_spkZState = null;
        }
        if ( m_spkMState != null )
        {
            m_spkMState.finalize();
            m_spkMState = null;
        }
        if ( m_spkMEffect != null )
        {
            m_spkMEffect.finalize();
            m_spkMEffect = null;
        }
        super.finalize();
    }

    /**
     * Draw the planar shadows
     * @param pkRenderer the Renderer object.
     * @param pkGlobalObject the base scene-graph object.
     * @param iMin, the first VisibleObject to draw
     * @param iMax, the last VisibleObject to draw
     * @param akVisible, the list VisibleObjects to draw
     */
    public void Draw (Renderer pkRenderer, Spatial pkGlobalObject,
                      int iMin, int iMax, VisibleObject[] akVisible)
    {
        // Draw the potentially visible portions of the shadow caster.
        int j;
        for (j = iMin; j <= iMax; j++)
        {
            if (akVisible[j].IsDrawable())
            {
                pkRenderer.Draw((Geometry)akVisible[j].Object);
            }
        }

        // The number of planes is limited by the size of a value in the stencil
        // buffer (256 for an 8-bit stencil buffer).
        int iQuantity = m_iQuantity;
        if (iQuantity >= pkRenderer.GetMaxStencilIndices())
        {
            iQuantity = pkRenderer.GetMaxStencilIndices() - 1;
        }

        // Save the current global state for restoration later.
        AlphaState spkSaveAState = pkRenderer.GetAlphaState();
        MaterialState spkSaveMState = pkRenderer.GetMaterialState();
        StencilState spkSaveSState = pkRenderer.GetStencilState();
        ZBufferState spkSaveZState = pkRenderer.GetZBufferState();

        for (int i = 0; i < iQuantity; i++)
        {
            // Enable depth buffering.  NOTE: The plane object should not have a
            // ZBufferState object that changes the current settings.
            m_spkZState.Enabled = true;
            pkRenderer.SetZBufferState(m_spkZState);

            // Enable the stencil buffer so that the shadow can be clipped by the
            // plane.  The stencil values are set whenever the corresponding
            // plane pixels are visible.
            m_spkSState.Enabled = true;
            m_spkSState.Compare = StencilState.CompareFunction.CF_ALWAYS;
            m_spkSState.Reference = (i+1);
            m_spkSState.OnFail = StencilState.OperationType.OT_KEEP;      // irrelevant
            m_spkSState.OnZFail = StencilState.OperationType.OT_KEEP;     // invisible kept 0
            m_spkSState.OnZPass = StencilState.OperationType.OT_REPLACE;  // visible to i+1
            pkRenderer.SetStencilState(m_spkSState);

            // Draw the plane.
            pkRenderer.Draw(m_aspkPlane[i]);

            // Get the projection matrix relative to the projector (light).
            Matrix4f kProjection = new Matrix4f();
            if (!GetProjectionMatrix(i,pkGlobalObject.WorldBound,kProjection))
            {
                continue;
            }

            pkRenderer.SetPostWorldTransformation(kProjection);

            // Blend the shadow color with the pixels drawn on the projection
            // plane.  The blending equation is
            //   (rf,gf,bf) = as*(rs,gs,bs) + (1-as)*(rd,gd,bd)
            // where (rf,gf,bf) is the final color to be written to the frame
            // buffer, (rs,gs,bs,as) is the shadow color, and (rd,gd,bd) is the
            // current color of the frame buffer.
            m_spkAState.BlendEnabled = true;
            m_spkAState.SrcBlend = AlphaState.SrcBlendMode.SBF_SRC_ALPHA;
            m_spkAState.DstBlend = AlphaState.DstBlendMode.DBF_ONE_MINUS_SRC_ALPHA;
            pkRenderer.SetAlphaState(m_spkAState);
            m_spkMState.Diffuse.R(m_akShadowColor[i].R());
            m_spkMState.Diffuse.G(m_akShadowColor[i].G());
            m_spkMState.Diffuse.B(m_akShadowColor[i].B());
            m_spkMState.Alpha = m_akShadowColor[i].A();
            pkRenderer.SetMaterialState(m_spkMState);

            // Disable the depth buffer reading so that no depth-buffer fighting
            // occurs.  The drawing of pixels is controlled solely by the stencil
            // value.
            m_spkZState.Enabled = false;
            pkRenderer.SetZBufferState(m_spkZState);

            // Only draw where the plane has been drawn.
            m_spkSState.Enabled = true;
            m_spkSState.Compare = StencilState.CompareFunction.CF_EQUAL;
            m_spkSState.Reference = (i+1);
            m_spkSState.OnFail = StencilState.OperationType.OT_KEEP;   // invisible kept 0
            m_spkSState.OnZFail = StencilState.OperationType.OT_KEEP;  // irrelevant
            m_spkSState.OnZPass = StencilState.OperationType.OT_ZERO;  // visible set to 0
            pkRenderer.SetStencilState(m_spkSState);

            // Draw the caster again, but temporarily use a material effect so
            // that the shadow color is blended onto the plane.  TO DO:  This
            // drawing pass should use a VisibleSet relative to the projector so
            // that objects that are out of view (i.e. culled relative to the
            // camera and not in the camera's VisibleSet) can cast shadows.
            for (j = iMin; j <= iMax; j++)
            {
                if (akVisible[j].IsDrawable())
                {
                    Geometry pkGeom = (Geometry)akVisible[j].Object;

                    GlobalState spkSaveMaterialState =
                        pkGeom.States[GlobalState.StateType.MATERIAL.Value()];
                    pkGeom.States[GlobalState.StateType.MATERIAL.Value()] = null;

                    pkGeom.AttachEffect(m_spkMEffect);
                    pkGeom.SetStartEffect(pkGeom.GetEffectQuantity()-1);

                    pkRenderer.Draw((Geometry)akVisible[j].Object);

                    pkGeom.SetStartEffect(0);
                    pkGeom.DetachEffect(m_spkMEffect);

                    pkGeom.States[GlobalState.StateType.MATERIAL.Value()] = spkSaveMaterialState;
                }
            }

            // Disable the stencil buffer and alpha blending.
            m_spkSState.Enabled = false;
            pkRenderer.SetStencilState(m_spkSState);
            m_spkAState.BlendEnabled = false;
            pkRenderer.SetAlphaState(m_spkAState);

            pkRenderer.RestorePostWorldTransformation();
        }

        // Restore the global state that existed before this function call.
        pkRenderer.SetAlphaState(spkSaveAState);
        pkRenderer.SetMaterialState(spkSaveMState);
        pkRenderer.SetStencilState(spkSaveSState);
        pkRenderer.SetZBufferState(spkSaveZState);
    }


    /** 
     * Return the number of shadow planes
     * @return the number of shadow planes
     */
    public int GetQuantity ()
    {
        return m_iQuantity;
    }

    /** 
     * Sets the ith shadow plane
     * @param i the ith shadow plane to set
     * @param spkPlane the shadow plane
     */
    public void SetPlane (int i, TriMesh spkPlane)
    {
        assert(0 <= i && i < m_iQuantity);
        m_aspkPlane[i] = spkPlane;
        m_aspkPlane[i].Culling = Spatial.CullingMode.CULL_ALWAYS;
    }

    /** 
     * Returns the ith shadow plane
     * @param i the ith shadow plane
     * @return the ith shadow plane
     */
    public TriMesh GetPlane (int i)
    {
        assert(0 <= i && i < m_iQuantity);
        return m_aspkPlane[i];
    }

    /**
     * Sets the ith Light projector for the shadows.
     * @param i the ith shadow plane
     * @param spkProjector the ith Light
     */
    public void SetProjector (int i, Light spkProjector)
    {
        assert(0 <= i && i < m_iQuantity);
        m_aspkProjector[i] = spkProjector;
    }

    /**
     * Returns the ith Light projector for the shadows.
     * @param i the ith shadow plane
     * @return the ith Light Projector
     */
    public Light GetProjector (int i)
    {
        assert(0 <= i && i < m_iQuantity);
        return m_aspkProjector[i];
    }

    /**
     * Sets the ith shadow color.
     * @param i the ith shadow 
     * @param rkShadowColor, the shadow color
     */
    public void SetShadowColor (int i, final ColorRGBA rkShadowColor)
    {
        assert(0 <= i && i < m_iQuantity);
        m_akShadowColor[i] = rkShadowColor;
    }

    /**
     * Returns the ith shadow color.
     * @param i the ith shadow 
     * @return the shadow color
     */
    public ColorRGBA GetShadowColor (int i) 
    {
        assert(0 <= i && i < m_iQuantity);
        return m_akShadowColor[i];
    }

    /**
     * Computes the ith Light Projection Matrix 
     * @param i, the ith plane
     * @param pkGlobalObjectWorldBound tests to see if a shadow should be cast
     * @param rkProject the project matrix for the light source
     * @return true if sucess false on failure.
     */
    protected boolean GetProjectionMatrix (int i,
                                           final BoundingVolume pkGlobalObjectWorldBound,
                                           Matrix4f rkProjection)
    {
        // Compute the equation for the shadow plane in world coordinates.
        Triangle3f kTri = new Triangle3f();
        m_aspkPlane[i].GetWorldTriangle(0,kTri);
        Plane3f kPlane = new Plane3f(kTri.V[0],kTri.V[1],kTri.V[2]);

        // This is a conservative test to see if a shadow should be cast.  This
        // can/ cause incorrect results if the caster is large and intersects the
        // plane, but ordinarily we are not trying to cast shadows in such
        // situations.
        if (pkGlobalObjectWorldBound.WhichSide(kPlane) < 0)
        {
            // The shadow caster is on the far side of plane, so it cannot cast
            // a shadow.
            return false;
        }

        // Compute the projection matrix for the light source.
        Light pkProjector = m_aspkProjector[i];
        if (pkProjector.Type == Light.LightType.LT_DIRECTIONAL)
        {
            float fNdD = kPlane.Normal.Dot(pkProjector.DVector);
            if (fNdD >= 0.0f)
            {
                // The projection must be onto the "positive side" of the plane.
                return false;
            }

            rkProjection.MakeObliqueProjection(kPlane.Normal,kTri.V[0],
                                               pkProjector.DVector);
        }
        else if (pkProjector.Type == Light.LightType.LT_POINT
                 || pkProjector.Type == Light.LightType.LT_SPOT )
        {
            float fNdE = kPlane.Normal.Dot(pkProjector.Position);
            if (fNdE <= 0.0f)
            {
                // The projection must be onto the "positive side" of the plane.
                return false;
            }

            rkProjection.MakePerspectiveProjection(kPlane.Normal,kTri.V[0],
                                                   pkProjector.Position);
        }
        else
        {
            assert(false);
            return false;
        }

        return true;
    }

    /** The number of Light Planes */
    protected int m_iQuantity;
    /** The Light Planes */
    protected TriMesh[] m_aspkPlane;
    /** The Light Projectors */
    protected Light[] m_aspkProjector;
    /** The Shadow Colors */
    protected ColorRGBA[] m_akShadowColor;

    /** Temporary render state for drawing. */
    protected AlphaState m_spkAState;
    /** Temporary render state for drawing. */
    protected MaterialState m_spkMState;
    /** Temporary render state for drawing. */
    protected StencilState m_spkSState;
    /** Temporary render state for drawing. */
    protected ZBufferState m_spkZState;
    /** Temporary render state for drawing. */
    protected MaterialEffect m_spkMEffect;

    /** Streaming constructor: */
    public PlanarShadowEffect ()
    {
        m_iQuantity = 0;
        m_aspkPlane = null;
        m_aspkProjector = null;
        m_akShadowColor = null;
    }

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
        
        int i;
        
        if (m_aspkPlane != null)
        {
            for (i = 0; i < m_iQuantity; i++)
            {
                if (m_aspkPlane[i] != null)
                {
                    pkFound = m_aspkPlane[i].GetObjectByName(rkName);
                    if (pkFound != null)
                    {
                        return pkFound;
                    }
                }
            }
        }
        
        if (m_aspkProjector != null)
        {
            for (i = 0; i < m_iQuantity; i++)
            {
                if (m_aspkProjector[i] != null)
                {
                    pkFound = m_aspkProjector[i].GetObjectByName(rkName);
                    if (pkFound != null)
                    {
                        return pkFound;
                    }
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
        
        int i;
        
        if (m_aspkPlane != null)
        {
            for (i = 0; i < m_iQuantity; i++)
            {
                if (m_aspkPlane[i] != null)
                {
                    m_aspkPlane[i].GetAllObjectsByName(rkName,rkObjects);
                }
            }
        }
        
        if (m_aspkProjector != null)
        {
            for (i = 0; i < m_iQuantity; i++)
            {
                if (m_aspkProjector[i] != null)
                {
                    m_aspkProjector[i].GetAllObjectsByName(rkName,rkObjects);
                }
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
        
        int i;
        
        if (m_aspkPlane != null)
        {
            for (i = 0; i < m_iQuantity; i++)
            {
                if (m_aspkPlane[i] != null)
                {
                    pkFound = m_aspkPlane[i].GetObjectByID(uiID);
                    if (pkFound != null)
                    {
                        return pkFound;
                    }
                }
            }
        }
        
        if (m_aspkProjector != null)
        {
            for (i = 0; i < m_iQuantity; i++)
            {
                if (m_aspkProjector[i] != null)
                {
                    pkFound = m_aspkProjector[i].GetObjectByID(uiID);
                    if (pkFound != null)
                    {
                        return pkFound;
                    }
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

        // native data
        m_iQuantity = rkStream.ReadInt();

        if (m_iQuantity > 0)
        {
            m_aspkPlane = new TriMesh[m_iQuantity];
            m_aspkProjector = new Light[m_iQuantity];
            m_akShadowColor = new ColorRGBA[m_iQuantity];

            // native data
            rkStream.Read(m_iQuantity,m_akShadowColor);

            // link data
            for (int i = 0; i < m_iQuantity; i++)
            {
                int iLinkID = rkStream.ReadInt();  // m_aspkPlane[i]
                pkLink.Add(iLinkID);
            }
            for (int i = 0; i < m_iQuantity; i++)
            {
                int iLinkID = rkStream.ReadInt();  // m_aspkProjector[i]
                pkLink.Add(iLinkID);
            }
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

        for (int i = 0; i < m_iQuantity; i++)
        {
            int iLinkID = pkLink.GetLinkID();
            m_aspkPlane[i] = (TriMesh)rkStream.GetFromMap(iLinkID);
        }
        for (int i = 0; i < m_iQuantity; i++)
        {
            int iLinkID = pkLink.GetLinkID();
            m_aspkProjector[i] = (Light)rkStream.GetFromMap(iLinkID);
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

        if (m_aspkPlane != null)
        {
            for (int i = 0; i < m_iQuantity; i++)
            {
                if (m_aspkPlane[i] != null)
                {
                    m_aspkPlane[i].Register(rkStream);
                }
            }
        }

        if (m_aspkProjector != null)
        {
            for (int i = 0; i < m_iQuantity; i++)
            {
                if (m_aspkProjector[i] != null)
                {
                    m_aspkProjector[i].Register(rkStream);
                }
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

        // native data
        rkStream.Write(m_iQuantity);
        if (m_iQuantity > 0)
        {
            rkStream.Write(m_iQuantity,m_akShadowColor);

            // link data
            for (int i = 0; i < m_iQuantity; i++)
            {
                if (m_aspkPlane[i] != null)
                {
                    rkStream.Write(m_aspkPlane[i].GetID());
                }
            }
            for (int i = 0; i < m_iQuantity; i++)
            {
                if (m_aspkProjector[i] != null)
                {
                    rkStream.Write(m_aspkProjector[i].GetID());
                }
            }
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
        int iSize = super.GetDiskUsed(rkVersion) +
            Stream.SIZEOF_INT; //sizeof(m_iQuantity);

        if (m_iQuantity > 0)
        {
            iSize += m_iQuantity*4*Stream.SIZEOF_FLOAT; //sizeof(m_akShadowColor[0]);
            iSize += m_iQuantity*Stream.SIZEOF_INT; //sizeof(m_aspkPlane[0]);
            iSize += m_iQuantity*Stream.SIZEOF_INT; //sizeof(m_aspkProjector[0]);
        }

        return iSize;
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
        pkTree.Append(StringTree.Format("PlanarShadowEffect",GetName()));
        pkTree.Append(super.SaveStrings(null));
        pkTree.Append(StringTree.Format("plane quantity =",m_iQuantity));

        for (int i = 0; i < m_iQuantity; i++)
        {
            String kPrefix = new String("shadow color[" + i + "] =");
            pkTree.Append(StringTree.Format(kPrefix,m_akShadowColor[i]));
        }

        // children
        for (int i = 0; i < m_iQuantity; i++)
        {
            pkTree.Append(m_aspkPlane[i].SaveStrings(null));
            pkTree.Append(m_aspkProjector[i].SaveStrings(null));
        }

        return pkTree;
    }
}
