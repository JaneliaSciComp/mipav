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

package gov.nih.mipav.view.WildMagic.LibGraphics.Effects;

import java.util.Vector;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;

import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;

public class PlanarReflectionEffect extends Effect
    implements NameIdInterface, StreamInterface
{
    /** Creates a new PlanarReflectionEffect for the number of planes
     * specified.
     * @param iQuantity the number of reflection planes.
     */
    public PlanarReflectionEffect (int iQuantity)
    {
        assert(iQuantity > 0);
        m_iQuantity = iQuantity;
        m_aspkPlane = new TriMesh[m_iQuantity];
        m_afReflectance = new float[m_iQuantity];

        m_spkAState = new AlphaState();
        m_spkSState = new StencilState();
        m_spkZState = new ZBufferState();
    }

    /** Delete data members: */
    public void finalize ()
    {
        m_aspkPlane = null;
        m_afReflectance = null;
        super.finalize();
    }

    /**
     * Draw the planar reflections
     * @param pkRenderer the Renderer object.
     * @param pkGlobalObject the base scene-graph object.
     * @param iMin, the first VisibleObject to draw
     * @param iMax, the last VisibleObject to draw
     * @param akVisible, the list VisibleObjects to draw
     */
    public void Draw (Renderer pkRenderer, Spatial pkGlobalObject,
                      int iMin, int iMax, VisibleObject[] akVisible)
    {
        // Save the current global state for restoration later.
        StencilState spkSaveSState = pkRenderer.GetStencilState();
        ZBufferState spkSaveZState = pkRenderer.GetZBufferState();

        // Enable depth-buffering and stenciling.
        m_spkZState.Enabled = true;
        m_spkZState.Writable = true;
        m_spkZState.Compare = ZBufferState.CompareMode.CF_LEQUAL;
        pkRenderer.SetZBufferState(m_spkZState);

        m_spkSState.Enabled = true;
        pkRenderer.SetStencilState(m_spkSState);

        // The number of planes is limited by the size of a value in the stencil
        // buffer (256 for an 8-bit stencil buffer).
        int iQuantity = m_iQuantity;
        if (iQuantity >= pkRenderer.GetMaxStencilIndices())
        {
            iQuantity = pkRenderer.GetMaxStencilIndices() - 1;
        }

        Camera pkCamera = pkRenderer.GetCamera();
        float fPortN = pkCamera.GetPortN();
        float fPortF = pkCamera.GetPortF();

        int j;
        for (int i = 0; i < iQuantity; i++)
        {
            // Render the mirror into the stencil plane.  All visible mirror
            // pixels will have the stencil value of the mirror.  Make sure that
            // no pixels are written to the depth buffer or color buffer, but use
            // depth buffer testing so that the stencil will not be written where
            // the plane is behind something already in the depth buffer.
            m_spkSState.Compare = StencilState.CompareFunction.CF_ALWAYS;
            m_spkSState.Reference = i+1;
            m_spkSState.OnFail = StencilState.OperationType.OT_KEEP;      // irrelevant
            m_spkSState.OnZFail = StencilState.OperationType.OT_KEEP;     // invisible kept 0
            m_spkSState.OnZPass = StencilState.OperationType.OT_REPLACE;  // visible to i+1
            pkRenderer.SetStencilState(m_spkSState);

            m_spkZState.Enabled = false;
            pkRenderer.SetZBufferState(m_spkZState);

            pkRenderer.SetColorMask(false,false,false,false);

            pkRenderer.Draw(m_aspkPlane[i]);

            pkRenderer.SetColorMask(true,true,true,true);

            // Render the mirror plane again by only processing pixels where the
            // stencil buffer contains the reference value.  This time there are
            // no changes to the stencil buffer and the depth buffer value is
            // reset to the far clipping plane.  This is done by setting the range
            // of depth values in the viewport volume to be [1,1].  Since the
            // mirror plane cannot also be semi-transparent, we do not care what
            // is behind the mirror plane in the depth buffer.  We need to move
            // the depth buffer values back where the mirror plane will be
            // rendered so that when the reflected object is rendered, it can be
            // depth buffered correctly.  Note that the rendering of the reflected
            // object will cause depth value to be written, which will appear to
            // be behind the mirror plane.  Enable writes to the color buffer.
            // Later when we want to render the reflecting plane and have it blend
            // with the background, which should contain the reflected caster, we
            // want to use the same blending function so that the pixels where the
            // reflected object was not rendered will contain the reflecting plane
            // colors.  In that case, the blending result will have the reflecting
            // plane appear to be opaque when in reality it was blended with
            // blending coefficients adding to one.
            pkCamera.SetDepthRange(1.0f,1.0f);
            m_spkZState.Enabled = true;
            m_spkZState.Compare = ZBufferState.CompareMode.CF_ALWAYS;
            pkRenderer.SetZBufferState(m_spkZState);

            m_spkSState.Compare = StencilState.CompareFunction.CF_EQUAL;
            m_spkSState.Reference = i+1;
            m_spkSState.OnFail = StencilState.OperationType.OT_KEEP;
            m_spkSState.OnZFail = StencilState.OperationType.OT_KEEP;
            m_spkSState.OnZPass = StencilState.OperationType.OT_KEEP;
            pkRenderer.SetStencilState(m_spkSState);

            pkRenderer.Draw(m_aspkPlane[i]);

            // Restore the depth range and depth testing function.
            m_spkZState.Compare = ZBufferState.CompareMode.CF_LEQUAL;
            pkRenderer.SetZBufferState(m_spkZState);
            pkCamera.SetDepthRange(fPortN,fPortF);

            // Compute the equation for the mirror plane in model coordinates
            // and get the reflection matrix in world coordinates.
            Matrix4f kReflection = new Matrix4f();
            Plane3f kPlane = new Plane3f();
            GetReflectionMatrixAndPlane(i,kReflection,kPlane);

            // Enable a clip plane so that only objects above the mirror plane
            // are reflected.  This occurs before SetTransformation because it
            // needs the current geometric pipeline matrices to compute the clip
            // plane in the correct coordinate system.
            pkRenderer.EnableUserClipPlane(0,kPlane);

            // This temporarily modifies the view matrix, effectively producing
            // H_world = H'_view = H_world*(H_reflection*H_view).
            pkRenderer.SetPostWorldTransformation(kReflection);

            // Reverse the cull direction.  Allow for models that are not
            // necessarily set up with front or back face culling.
            pkRenderer.SetReverseCullFace(true);
            pkRenderer.SetCullState(pkRenderer.GetCullState());

            // Render the reflected object.  Only render where the stencil buffer
            // contains the reference value.
            for (j = iMin; j <= iMax; j++)
            {
                if (akVisible[j].IsDrawable())
                {
                    pkRenderer.Draw((Geometry)akVisible[j].Object);
                }
            }

            pkRenderer.SetReverseCullFace(false);
            pkRenderer.SetCullState(pkRenderer.GetCullState());

            pkRenderer.RestorePostWorldTransformation();
            pkRenderer.DisableUserClipPlane(0);

            // We are about to render the reflecting plane again.  Reset to the
            // global state for the reflecting plane.  We want to blend the
            // reflecting plane with what is already in the color buffer,
            // particularly either the image of the reflected caster or the
            // reflecting plane.  All we want for the reflecting plane at this
            // stage is to force the alpha channel to always be the reflectance
            // value for the reflecting plane.  Render the reflecting plane
            // wherever the stencil buffer is set to the reference value.  This
            // time clear the stencil buffer reference value where it is set.
            // Perform the normal depth buffer testing and writes.  Allow the
            // color buffer to be written to, but this time blend the reflecting
            // plane with the values in the color buffer based on the reflectance
            // value.  Note that where the stencil buffer is set, the color buffer
            // has either color values from the reflecting plane or the reflected
            // object.  Blending will use src=1-alpha (reflecting plane) and
            // dest=alpha background (reflecting plane or reflected object).
            m_spkAState.BlendEnabled = true;
            m_spkAState.SrcBlend = AlphaState.SrcBlendMode.SBF_ONE_MINUS_CONSTANT_ALPHA;
            m_spkAState.DstBlend = AlphaState.DstBlendMode.DBF_CONSTANT_ALPHA;
            m_spkAState.ConstantColor = new ColorRGBA(0.0f,0.0f,0.0f,
                                                   m_afReflectance[i]);
            pkRenderer.SetAlphaState(m_spkAState);

            m_spkSState.Compare = StencilState.CompareFunction.CF_EQUAL;
            m_spkSState.Reference = i+1;
            m_spkSState.OnFail = StencilState.OperationType.OT_KEEP;
            m_spkSState.OnZFail = StencilState.OperationType.OT_KEEP;
            m_spkSState.OnZPass = StencilState.OperationType.OT_INVERT;
            pkRenderer.SetStencilState(m_spkSState);

            pkRenderer.Draw(m_aspkPlane[i]);

            m_spkAState.BlendEnabled = false;
            pkRenderer.SetAlphaState(m_spkAState);
        }

        // Disable depth-buffering and stenciling (in the effect's states).
        m_spkZState.Enabled = false;
        m_spkSState.Enabled = false;

        // Restore the global state that existed before this function call.
        pkRenderer.SetStencilState(spkSaveSState);
        pkRenderer.SetZBufferState(spkSaveZState);

        // Render the objects as usual, this time drawing only the potentially
        // visible objects.
        for (j = iMin; j <= iMax; j++)
        {
            if (akVisible[j].IsDrawable())
            {
                pkRenderer.Draw((Geometry)akVisible[j].Object);
            }
        }
    }

    /** 
     * Return the number of reflection planes
     * @return the number of reflection planes
     */
    public int GetQuantity ()
    {
        return m_iQuantity;
    }

    /** 
     * Sets the ith reflection plane
     * @param i the ith reflection plane to set
     * @param spkPlane the reflection plane
     */
    public void SetPlane (int i, TriMesh spkPlane)
    {
        assert(0 <= i && i < m_iQuantity);
        m_aspkPlane[i] = spkPlane;
        m_aspkPlane[i].Culling = Spatial.CullingMode.CULL_ALWAYS;
    }

    /** 
     * Returns the ith reflection plane
     * @param i the ith reflection plane
     * @return the ith reflection plane
     */
    public TriMesh GetPlane (int i)
    {
        assert(0 <= i && i < m_iQuantity);
        return m_aspkPlane[i];
    }

    /** 
     * Sets the reflectance for the ith reflection plane
     * @param i the ith reflection plane
     * @param fReflectance the reflectance parameter
     */
    public void SetReflectance (int i, float fReflectance)
    {
        assert(0 <= i && i < m_iQuantity);
        m_afReflectance[i] = fReflectance;
    }

    /** 
     * Returns the reflectance for the ith reflection plane
     * @param i the ith reflection plane
     * @return the reflectance parameter
     */
    public float GetReflectance (int i)
    {
        assert(0 <= i && i < m_iQuantity);
        return m_afReflectance[i];
    }

    /** Computes and returns the reflection matrix for the ith plane in world
     * coordinates.
     * @param i the ith reflection plane
     * @param rkReflection, returned value of the reflection matrix
     * @param rkPlane, returned value of the plane in world coordinates.
     */
    protected void GetReflectionMatrixAndPlane (int i, Matrix4f rkReflection,
                                                Plane3f rkPlane)
    {
        // Compute the equation for the mirror plane in world coordinates.
        Triangle3f kTri = new Triangle3f();
        m_aspkPlane[i].GetWorldTriangle(0,kTri);
        rkPlane = new Plane3f(kTri.V[0],kTri.V[1],kTri.V[2]);

        // Compute the reflection matrix.
        rkReflection.MakeReflection(rkPlane.Normal,kTri.V[0]);

        m_aspkPlane[i].GetModelTriangle(0,kTri);
        rkPlane = new Plane3f(kTri.V[0],kTri.V[1],kTri.V[2]);
    }


    /** Number of reflection planes */
    protected int m_iQuantity;
    /** Reflection planes */
    protected TriMesh[] m_aspkPlane;
    /** Reflectance factors */
    protected float[] m_afReflectance;

    /** Temporary render state for drawing. */
    protected AlphaState m_spkAState;
    /** Temporary render state for drawing. */
    protected StencilState m_spkSState;
    /** Temporary render state for drawing. */
    protected ZBufferState m_spkZState;

    /** Streaming constructor */
    public PlanarReflectionEffect ()
    {
        m_iQuantity = 0;
        m_aspkPlane = null;
        m_afReflectance = null;
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

        if (m_aspkPlane != null)
        {
            for (int i = 0; i < m_iQuantity; i++)
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

        if (m_aspkPlane != null)
        {
            for (int i = 0; i < m_iQuantity; i++)
            {
                if (m_aspkPlane[i] != null)
                {
                    m_aspkPlane[i].GetAllObjectsByName(rkName,rkObjects);
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

        if (m_aspkPlane != null)
        {
            for (int i = 0; i < m_iQuantity; i++)
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
            m_afReflectance = new float[m_iQuantity];

            // native data
            rkStream.Read(m_iQuantity,m_afReflectance);

            // link data
            for (int i = 0; i < m_iQuantity; i++)
            {
                int iLinkID = rkStream.ReadInt();
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
            int pkLinkID = pkLink.GetLinkID();
            m_aspkPlane[i] = (TriMesh)rkStream.GetFromMap(pkLinkID);
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
            rkStream.Write(m_iQuantity,m_afReflectance);

            // link data
            for (int i = 0; i < m_iQuantity; i++)
            {
                if (m_aspkPlane[i] != null)
                {
                    rkStream.Write(m_aspkPlane[i].GetID());
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
            iSize += m_iQuantity*Stream.SIZEOF_FLOAT; //sizeof(m_afReflectance[0]);
            iSize += m_iQuantity*Stream.SIZEOF_INT; //sizeof(m_aspkPlane[0]);
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
        pkTree.Append(StringTree.Format("PlanarReflectionEffect",GetName()));
        pkTree.Append(super.SaveStrings(null));
        pkTree.Append(StringTree.Format("plane quantity =",m_iQuantity));

        for (int i = 0; i < m_iQuantity; i++)
        {
            String kPrefix = new String("reflectance[" + i + "] =");
            pkTree.Append(StringTree.Format(kPrefix,m_afReflectance[i]));
        }

        // children
        for (int i = 0; i < m_iQuantity; i++)
        {
            pkTree.Append(m_aspkPlane[i].SaveStrings(null));
        }

        return pkTree;
    }
}
