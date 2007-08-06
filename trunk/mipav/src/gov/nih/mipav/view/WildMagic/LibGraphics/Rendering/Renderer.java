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
package gov.nih.mipav.view.WildMagic.LibGraphics.Rendering;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Effects.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Shaders.*;

public abstract class Renderer
{
    // Abstract API for renderers.  Each graphics API must implement this
    // layer.

    // Run-time type information.
    public enum RendererType
    {
        OPENGL,
        DIRECTX,
        SOFTWARE,
        MAX_RENDERER_TYPES
    };

    // Make this renderer context the active one.
    public void Activate ()
    {
        SetBackgroundColor(m_kBackgroundColor);
        OnViewportChange();
        OnFrustumChange ();
        if (m_pkCamera != null )
        {
            OnFrameChange ();
        }
    }

    public boolean IsActive ()
    {
        // stub for derived classes
        return true;
    }


    // Renderer-specific information for loading shader programs.
    public abstract String GetExtension ();
    public abstract  char GetCommentCharacter ();

    // Access to the camera.
    public void SetCamera (Camera pkCamera)
    {
        if (m_pkCamera != null)
        {
            m_pkCamera.m_pkRenderer = null;
        }

        if (pkCamera != null)
        {
            pkCamera.m_pkRenderer = this;
        }

        m_pkCamera = pkCamera;

        if (m_pkCamera != null)
        {
            OnFrustumChange ();
            OnViewportChange ();
            OnFrameChange ();
        }
    }

    public Camera GetCamera ()
    {
        return m_pkCamera;
    }

    // Access to the geometry object that is to be drawn.
    public void SetGeometry (Geometry pkGeometry)
    {
        m_pkGeometry = pkGeometry;
    }

    public Geometry GetGeometry ()
    {
        return m_pkGeometry;
    }

    // Frame buffer parameters.
    public FrameBuffer.FormatType GetFormatType ()
    {
        return m_eFormat;
    }

    public FrameBuffer.DepthType GetDepthType ()
    {
        return m_eDepth;
    }

    public FrameBuffer.StencilType GetStencilType ()
    {
        return m_eStencil;
    }

    public FrameBuffer.BufferingType GetBufferingType ()
    {
        return m_eBuffering;
    }

    public FrameBuffer.MultisamplingType GetMultisamplingType ()
    {
        return m_eMultisampling;
    }

    // Window parameters.
    public int GetWidth ()
    {
        return m_iWidth;
    }

    public int GetHeight ()
    {
        return m_iHeight;
    }

    public void Resize (int iWidth, int iHeight)
    {
        m_iWidth = iWidth;
        m_iHeight = iHeight;
        OnViewportChange();
    }

    public void ToggleFullscreen ()
    {
        m_bFullscreen = !m_bFullscreen;
    }


    // Background color access.
    public void SetBackgroundColor (ColorRGBA rkColor)
    {
        m_kBackgroundColor = rkColor;
    }

    public ColorRGBA GetBackgroundColor () 
    {
        return m_kBackgroundColor;
    }

    // Support for predraw and postdraw semantics.
    public boolean BeginScene ()
    {
        // stub for derived classes
        return true;
    }

    public void EndScene ()
    {
        // stub for derived classes
    }

    // Support for full-sized window buffer operations.
    public abstract void ClearBackBuffer ();
    public abstract void ClearZBuffer ();
    public abstract void ClearStencilBuffer ();
    public abstract void ClearBuffers ();
    public abstract void DisplayBackBuffer ();

    // Clear the buffer in the specified subwindow.
    public abstract void ClearBackBuffer (int iXPos, int iYPos, int iWidth,
                                          int iHeight);
    public abstract void ClearZBuffer (int iXPos, int iYPos, int iWidth,
                                       int iHeight);
    public abstract void ClearStencilBuffer (int iXPos, int iYPos, int iWidth,
                                             int iHeight);
    public abstract void ClearBuffers (int iXPos, int iYPos, int iWidth,
                                       int iHeight);

    // The main entry point to drawing in the derived-class renderers.
    public abstract void DrawElements ();

    // Object drawing.
    public void DrawScene (VisibleSet rkVisibleSet)
    {
        // NOTE:  The stack of 2-tuples is limited to having 64 elements.  This
        // should be plenty, because the chances of having 64 global effects
        // in the same path is small (that is a *lot* of effects to apply in
        // one frame).  If it needs to be larger for your applications, increase
        // the maximum size.
        final int iMaxTuples = 64;    // maximum number of stack elements
        int[][] aaiStack = new int[iMaxTuples][2];  // elements are (startIndex,finalIndex)
        int iTop = -1;                // stack is initially empty

        final int iVisibleQuantity = rkVisibleSet.GetQuantity();
        VisibleObject[] akVisible = rkVisibleSet.GetVisible();
        for (int i = 0; i < iVisibleQuantity; i++)
        {
            if (akVisible[i].Object != null)
            {
                if (akVisible[i].GlobalEffect != null)
                {
                    // Begin the scope of a global effect.
                    iTop++;
                    assert(iTop < iMaxTuples);
                    aaiStack[iTop][0] = i;
                    aaiStack[iTop][1] = i;
                }
                else
                {
                    // Found a leaf Geometry object.
                    if (iTop == -1)
                    {
                        Draw((Geometry)akVisible[i].Object);
                    }
                    else
                    {
                        aaiStack[iTop][1]++;
                    }
                }
            }
            else
            {
                // End the scope of a global effect.
                assert(akVisible[i].GlobalEffect == null);
                int jMin = aaiStack[iTop][0];
                int jMax = aaiStack[iTop][1];

                akVisible[jMin].GlobalEffect.Draw(this,akVisible[jMin].Object,
                                                  jMin+1,jMax,akVisible);

                if (--iTop >= 0)
                {
                    aaiStack[iTop][1] = jMax + 1;
                }
            }
        }
    }

    public void Draw (Geometry pkGeometry)
    {
        m_pkGeometry = pkGeometry;

        SetGlobalState(m_pkGeometry.States);
        SetWorldTransformation();

        // Enable the index buffer.  The connectivity information is the same
        // across all effects and all passes per effect.
        EnableIBuffer();

        // Lighting (if any) is applied first.  Effects are applied second.
        boolean bPrimaryEffect = true;
        final int iMin = m_pkGeometry.GetStartEffect();
        final int iMax = m_pkGeometry.GetEffectQuantity();
        for (int i = iMin; i < iMax; i++)
        {
            // The effect should be a ShaderEffect-derived object, but it is
            // possible that an incorrectly designed Effect-derived class
            // attaches itself to the Geometry object in its overloaded
            // Effect::Draw function before calling Renderer::Draw(Geometry*).
            ShaderEffect pkEffect =
                (ShaderEffect)(m_pkGeometry.GetEffect(i));
            if (pkEffect != null)
            {
                bPrimaryEffect = ApplyEffect(pkEffect,bPrimaryEffect);
            }
            else
            {
                // Enable this line of code to trap problems with incorrectly
                // designed Effect-derived classes.
                //assert(false);
            }
        }

        // Disable the index buffer.
        DisableIBuffer();

        RestoreWorldTransformation();
        RestoreGlobalState(m_pkGeometry.States);

        m_pkGeometry = null;
    }


    // Text drawing.
    //public abstract int LoadFont (const char* acFace, int iSize, boolean bBold,
    //boolean bItalic);
    public abstract void UnloadFont (int iFontID);
    public abstract boolean SelectFont (int iFontID);
    public abstract void Draw (int iX, int iY, final ColorRGBA rkColor,
                               final char[] acText);

    // 2D drawing
    public abstract void Draw (final byte[] aucBuffer);

    // Object drawing.
    public boolean ApplyEffect (ShaderEffect pkEffect, boolean rbPrimaryEffect)
    {
        final int iPassQuantity = pkEffect.GetPassQuantity();
        for (int iPass = 0; iPass < iPassQuantity; iPass++)
        {
            // The programs must be loaded first because (1) the vertex buffer is
            // enabled based on the program inputs and (2) the global state
            // setting can access the samplers related to the program.
            pkEffect.LoadPrograms(iPass,m_iMaxColors,m_iMaxTCoords,
                                  m_iMaxVShaderImages,m_iMaxPShaderImages);

            // The global state must be set before enabling programs because the
            // programs set sampler state for samplers about to be enabled.
            pkEffect.SetGlobalState(iPass,this,rbPrimaryEffect);

            // Enable the vertex program.
            VertexProgram pkVProgram = pkEffect.GetVProgram(iPass);
            EnableVProgram(pkVProgram);

            // Enable the pixel program.
            PixelProgram pkPProgram = pkEffect.GetPProgram(iPass);
            EnablePProgram(pkPProgram);

            // Enable the textures used by the pixel program.
            final int iPTQuantity = pkEffect.GetPTextureQuantity(iPass);
            int iTexture;
            for (iTexture = 0; iTexture < iPTQuantity; iTexture++)
            {
                EnableTexture(pkEffect.GetPTexture(iPass,iTexture));
            }

            // Create or find a compatible vertex buffer for the vertex program
            // and enable it.
            final Attributes rkIAttr = pkVProgram.GetInputAttributes();
            final Attributes rkOAttr = pkVProgram.GetOutputAttributes();
            ResourceIdentifier pkID = EnableVBuffer(rkIAttr,rkOAttr);

            DrawElements();

            // Disable the vertex buffer for the vertex program.
            DisableVBuffer(pkID);

            // Disable the textures used by the pixel program.
            for (iTexture = 0; iTexture < iPTQuantity; iTexture++)
            {
                DisableTexture(pkEffect.GetPTexture(iPass,iTexture));
            }

            // Disable the pixel program.
            DisablePProgram(pkPProgram);

            // Disable the vertex program.
            DisableVProgram(pkVProgram);

            // Restore the global state that was active before this pass.
            pkEffect.RestoreGlobalState(iPass,this,rbPrimaryEffect);
        }

        rbPrimaryEffect = false;
        return rbPrimaryEffect;
    }


    // Point size, line width, and line stipple.  These translate nicely to
    // OpenGL calls.  The point size is supported by Direct3D.  However, to
    // draw thicker lines or stippled lines, a separate interface in Direct3D
    // (ID3DXLine) must be used.  Until code is added to use this interface,
    // the Direct3D renderer ignores the SetLineWidth and SetLineStipple
    // function calls.  Line stippling is disabled when either of "repeat" or
    // "pattern" is zero.
    public void SetPointSize (float fSize)
    {
        if (fSize > 0.0f)
        {
            m_fPointSize = fSize;
        }
    }

    public float GetPointSize ()
    {
        return m_fPointSize;
    }

    public void SetLineWidth (float fWidth)
    {
        if (fWidth > 0.0f)
        {
            m_fLineWidth = fWidth;
        }
    }

    public float GetLineWidth ()
    {
        return m_fLineWidth;
    }

    public void SetLineStipple (int iRepeat, short usPattern)
    {
        if (iRepeat < 0)
        {
            iRepeat = 0;
        }
        m_iLineStippleRepeat = iRepeat;
        m_usLineStipplePattern = usPattern;
    }

    public int GetLineStippleRepeat ()
    {
        return m_iLineStippleRepeat;
    }

    public short GetLineStipplePattern ()
    {
        return m_usLineStipplePattern;
    }


    // Resource limits.
    public int GetMaxLights ()
    {
        return m_iMaxLights;
    }

    public int GetMaxColors ()
    {
        return m_iMaxColors;
    }

    public int GetMaxTCoords ()
    {
        return m_iMaxTCoords;
    }

    public int GetMaxVShaderImages ()
    {
        return m_iMaxVShaderImages;
    }

    public int GetMaxPShaderImages ()
    {
        return m_iMaxPShaderImages;
    }

    public int GetMaxStencilIndices ()
    {
        return m_iMaxStencilIndices;
    }

    public int GetMaxUserClipPlanes ()
    {
        return m_iMaxUserClipPlanes;
    }

    // Global render state management.
    public void SetGlobalState (GlobalState[] aspkState)
    {
        GlobalState pkState = aspkState[GlobalState.StateType.ALPHA.Value()];
        if (pkState != null)
        {
            SetAlphaState((AlphaState)pkState);
        }

        pkState = aspkState[GlobalState.StateType.CULL.Value()];
        if (pkState != null)
        {
            SetCullState((CullState)pkState);
        }

        pkState = aspkState[GlobalState.StateType.MATERIAL.Value()];
        if (pkState != null)
        {
            SetMaterialState((MaterialState)pkState);
        }

        pkState = aspkState[GlobalState.StateType.POLYGONOFFSET.Value()];
        if (pkState != null)
        {
            SetPolygonOffsetState((PolygonOffsetState)pkState);
        }

        pkState = aspkState[GlobalState.StateType.STENCIL.Value()];
        if (pkState != null)
        {
            SetStencilState((StencilState)pkState);
        }

        pkState = aspkState[GlobalState.StateType.WIREFRAME.Value()];
        if (pkState != null)
        {
            SetWireframeState((WireframeState)pkState);
        }

        pkState = aspkState[GlobalState.StateType.ZBUFFER.Value()];
        if (pkState != null)
        {
            SetZBufferState((ZBufferState)pkState);
        }
    }

    public void RestoreGlobalState (GlobalState[] aspkState)
    {
        GlobalState pkState;
    
        if (aspkState[GlobalState.StateType.ALPHA.Value()] != null)
        {
            pkState = GlobalState.Default[GlobalState.StateType.ALPHA.Value()];
            SetAlphaState((AlphaState)pkState);
        }

        if (aspkState[GlobalState.StateType.CULL.Value()] != null)
        {
            pkState = GlobalState.Default[GlobalState.StateType.CULL.Value()];
            SetCullState((CullState)pkState);
        }

        if (aspkState[GlobalState.StateType.MATERIAL.Value()] != null)
        {
            pkState = GlobalState.Default[GlobalState.StateType.MATERIAL.Value()];
            SetMaterialState((MaterialState)pkState);
        }

        if (aspkState[GlobalState.StateType.POLYGONOFFSET.Value()] != null)
        {
            pkState = GlobalState.Default[GlobalState.StateType.POLYGONOFFSET.Value()];
            SetPolygonOffsetState((PolygonOffsetState)pkState);
        }

        if (aspkState[GlobalState.StateType.STENCIL.Value()] != null)
        {
            pkState = GlobalState.Default[GlobalState.StateType.STENCIL.Value()];
            SetStencilState((StencilState)pkState);
        }

        if (aspkState[GlobalState.StateType.WIREFRAME.Value()] != null)
        {
            pkState = GlobalState.Default[GlobalState.StateType.WIREFRAME.Value()];
            SetWireframeState((WireframeState)pkState);
        }

        if (aspkState[GlobalState.StateType.ZBUFFER.Value()] != null)
        {
            pkState = GlobalState.Default[GlobalState.StateType.ZBUFFER.Value()];
            SetZBufferState((ZBufferState)pkState);
        }
    }

    public void SetAlphaState (AlphaState pkState)
    {
        m_aspkState[GlobalState.StateType.ALPHA.Value()] = pkState;
    }

    public void SetCullState (CullState pkState)
    {
        m_aspkState[GlobalState.StateType.CULL.Value()] = pkState;
    }

    public void SetMaterialState (MaterialState pkState)
    {
        m_aspkState[GlobalState.StateType.MATERIAL.Value()] = pkState;
    }

    public void SetPolygonOffsetState (PolygonOffsetState pkState)
    {
        m_aspkState[GlobalState.StateType.POLYGONOFFSET.Value()] = pkState;
    }

    public void SetStencilState (StencilState pkState)
    {
        m_aspkState[GlobalState.StateType.STENCIL.Value()] = pkState;
    }

    public void SetWireframeState (WireframeState pkState)
    {
        m_aspkState[GlobalState.StateType.WIREFRAME.Value()] = pkState;
    }

    public void SetZBufferState (ZBufferState pkState)
    {
        m_aspkState[GlobalState.StateType.ZBUFFER.Value()] = pkState;
    }

    public AlphaState GetAlphaState ()
    {
        return (AlphaState)(m_aspkState[GlobalState.StateType.ALPHA.Value()]);
    }

    public CullState GetCullState ()
    {
        return (CullState)(m_aspkState[GlobalState.StateType.CULL.Value()]);
    }

    public MaterialState GetMaterialState ()
    {
        return (MaterialState)(m_aspkState[GlobalState.StateType.MATERIAL.Value()]);
    }

    public PolygonOffsetState GetPolygonOffsetState ()
    {
        return (PolygonOffsetState)(m_aspkState[GlobalState.StateType.POLYGONOFFSET.Value()]);
    }

    public StencilState GetStencilState ()
    {
        return (StencilState)(m_aspkState[GlobalState.StateType.STENCIL.Value()]);
    }

    public WireframeState GetWireframeState ()
    {
        return (WireframeState)(m_aspkState[GlobalState.StateType.WIREFRAME.Value()]);
    }

    public ZBufferState GetZBufferState ()
    {
        return (ZBufferState)(m_aspkState[GlobalState.StateType.ZBUFFER.Value()]);
    }

    public void SetReverseCullFace (boolean bReverseCullFace)
    {
        m_bReverseCullFace = bReverseCullFace;
    }

    public boolean GetReverseCullFace ()
    {
        return m_bReverseCullFace;
    }

    // Function pointer types for binding and unbinding resources.
    public ReleaseFunction m_kReleaseFunction;

    // Resource loading and releasing.
    public void LoadAllResources (Spatial pkScene)
    {
        Geometry pkGeometry = (Geometry)(pkScene);
        if (pkGeometry != null)
        {
            LoadResources(pkGeometry);
        }

        Node pkNode = (Node)(pkScene);
        if (pkNode != null)

        {
            for (int i = 0; i < pkNode.GetQuantity(); i++)
            {
                Spatial pkChild = pkNode.GetChild(i);
                if (pkChild != null)
                {
                    LoadAllResources(pkChild);
                }
            }
        }
    }

    public void ReleaseAllResources (Spatial pkScene)
    {
        Geometry pkGeometry = (Geometry)(pkScene);
        if (pkGeometry != null)
        {
            ReleaseResources(pkGeometry);
        }

        Node pkNode = (Node)(pkScene);
        if (pkNode != null)
        {
            for (int i = 0; i < pkNode.GetQuantity(); i++)
            {
                Spatial pkChild = pkNode.GetChild(i);
                if (pkChild != null)
                {
                    ReleaseAllResources(pkChild);
                }
            }
        }
    }

    public void LoadResources (Geometry pkGeometry)
    {
        assert(pkGeometry != null);
        assert(pkGeometry.VBuffer != null);
        assert(pkGeometry.IBuffer != null);

        // Load the index buffer into video memory.
        LoadIBuffer(pkGeometry.IBuffer);

        // Load the vertex buffer(s) and effects resources into video memory.
        final int iEffectQuantity = pkGeometry.GetEffectQuantity();
        for (int i = 0; i < iEffectQuantity; i++)
        {
            Effect pkEffect = pkGeometry.GetEffect(i);
            pkEffect.LoadResources(this,pkGeometry);
        }
    }

    public void ReleaseResources (Geometry pkGeometry)
    {
        assert(pkGeometry != null);
        assert(pkGeometry.VBuffer != null);
        assert(pkGeometry.IBuffer != null);

        // Release the index buffer from video memory.
        ReleaseIBuffer(pkGeometry.IBuffer);

        // Release the vertex buffer(s) from video memory.
        while (pkGeometry.VBuffer.GetInfoQuantity() > 0)
        {
            ReleaseVBuffer(pkGeometry.VBuffer);
        }

        // Release the effects resources from video memory.
        final int iEffectQuantity = pkGeometry.GetEffectQuantity();
        for (int i = 0; i < iEffectQuantity; i++)
        {
            Effect pkEffect = pkGeometry.GetEffect(i);
            pkEffect.ReleaseResources(this,pkGeometry);
        }
    }


    public void LoadResources (Effect pkEffect)
    {
        assert(pkEffect != null);
        pkEffect.LoadResources(this,null);
    }

    public void ReleaseResources (Effect pkEffect)
    {
        assert(pkEffect != null);
        pkEffect.ReleaseResources(this,null);
    }

    public void LoadVProgram (VertexProgram pkVProgram)
    {
        if (pkVProgram == null)
        {
            return;
        }

        ResourceIdentifier pkID = pkVProgram.GetIdentifier(this);
        if (pkID == null)
        {
            pkID = OnLoadVProgram(pkVProgram);
            pkVProgram.OnLoad(this, new ReleaseFunctionVertex(this),pkID);
        }
    }

    public void ReleaseVProgram (Bindable pkVProgram)
    {
        if (pkVProgram == null)
        {
            return;
        }

        ResourceIdentifier pkID = pkVProgram.GetIdentifier(this);
        if (pkID != null)
        {
            OnReleaseVProgram(pkID);
            pkVProgram.OnRelease(this,pkID);
        }
    }

    public void LoadPProgram (PixelProgram pkPProgram)
    {
        if (pkPProgram == null)
        {
            return;
        }

        ResourceIdentifier pkID = pkPProgram.GetIdentifier(this);
        if (pkID == null)
        {
            pkID = OnLoadPProgram(pkPProgram);
            pkPProgram.OnLoad(this,new ReleaseFunctionPixel(this),pkID);
        }
    }

    public void ReleasePProgram (Bindable pkPProgram)
    {
        if (pkPProgram == null)
        {
            return;
        }

        ResourceIdentifier pkID = pkPProgram.GetIdentifier(this);
        if (pkID != null)
        {
            OnReleasePProgram(pkID);
            pkPProgram.OnRelease(this,pkID);
        }
    }

    public void LoadTexture (Texture pkTexture)
    {
        if (pkTexture == null)
        {
            return;
        }

        ResourceIdentifier pkID = pkTexture.GetIdentifier(this);
        if (pkID == null)
        {
            pkID = OnLoadTexture(pkTexture);
            pkTexture.OnLoad(this,new ReleaseFunctionTexture(this),pkID);
        }
        else if ( pkTexture.Reload() )
        {
            OnReloadTexture(pkID);
            pkTexture.Reload(false);
        }
    }

    public void ReleaseTexture (Bindable pkTexture)
    {
        if (pkTexture == null)
        {
            return;
        }

        ResourceIdentifier pkID = pkTexture.GetIdentifier(this);
        if (pkID != null)
        {
            OnReleaseTexture(pkID);
            pkTexture.OnRelease(this,pkID);
        }
    }

    public void ReloadTexture (Bindable pkTexture)
    {
        if (pkTexture == null)
        {
            return;
        }

        ResourceIdentifier pkID = pkTexture.GetIdentifier(this);
        if (pkID != null)
        {
            OnReloadTexture(pkID);
        }
    }

    public void LoadVBuffer ( final Attributes rkIAttr, final Attributes rkOAttr,
                              VertexBuffer pkVBuffer)
    {
        if (pkVBuffer == null)
        {
            return;
        }

        // Search for a matching vertex buffer that was used during previous
        // passes.
        ResourceIdentifier pkID = null;
        for (int i = 0; i < pkVBuffer.GetInfoQuantity(); i++)
        {
            pkID = pkVBuffer.GetIdentifier(i,this);
            if (pkID != null)
            {
                if (pkID instanceof gov.nih.mipav.view.WildMagic.LibRenderers.OpenGLRenderer.VBufferID )
                {
                    if ( ((gov.nih.mipav.view.WildMagic.LibRenderers.OpenGLRenderer.VBufferID)pkID).IAttr == rkIAttr)
                    {
                        //if (rkIAttr == (Attributes)pkID)
                        // Found a matching vertex buffer in video memory.
                        return;
                    }
                }
            }
        }

        // The vertex buffer is encountered the first time.  TO DO:  For now,
        // require 3-tuples for positions and normals.
        final Attributes rkVBAttr = pkVBuffer.GetAttributes();
        assert(rkIAttr.GetPChannels() == 3 && rkVBAttr.GetPChannels() == 3);
        if (rkIAttr.HasNormal())
        {
            assert(rkIAttr.GetNChannels() == 3 && rkVBAttr.GetNChannels() == 3);
        }

        pkID = OnLoadVBuffer(rkIAttr,rkOAttr,pkVBuffer);
        pkVBuffer.OnLoad(this,new ReleaseFunctionVBuffer(this),pkID);
    }

    public void ReleaseVBuffer (Bindable pkVBuffer)
    {
        if (pkVBuffer == null)
        {
            return;
        }

        for (int i = 0; i < pkVBuffer.GetInfoQuantity(); i++)
        {
            ResourceIdentifier pkID = pkVBuffer.GetIdentifier(i,this);
            if (pkID != null)
            {
                OnReleaseVBuffer(pkID);
                pkVBuffer.OnRelease(this,pkID);
                return;
            }
        }
    }

    public void LoadIBuffer (IndexBuffer pkIBuffer)
    {
        if (pkIBuffer == null)
        {
            return;
        }

        ResourceIdentifier pkID = pkIBuffer.GetIdentifier(this);
        if (pkID == null)
        {
            pkID = OnLoadIBuffer(pkIBuffer);
            //pkIBuffer->OnLoad(this,&Renderer::ReleaseIBuffer,pkID);
            pkIBuffer.OnLoad(this,new ReleaseFunctionIBuffer(this),pkID);
        }
    }

    public void ReleaseIBuffer (Bindable pkIBuffer)
    {
        if (pkIBuffer == null)
        {
            return;
        }

        ResourceIdentifier pkID = pkIBuffer.GetIdentifier(this);
        if (pkID != null)
        {
            OnReleaseIBuffer(pkID);
            pkIBuffer.OnRelease(this,pkID);
        }
    }


    // Resource enabling and disabling.
    public enum ConstantType  // ConstantType
    {
        CT_RENDERER,
        CT_NUMERICAL,
        CT_USER
    };

    public abstract void SetVProgramConstant ( Renderer.ConstantType eCType, int iBaseRegister,
                                               int iRegisterQuantity, float[] afData);
    public abstract void SetPProgramConstant ( Renderer.ConstantType eCType, int iBaseRegister,
                                               int iRegisterQuantity, float[] afData);

    public void EnableVProgram (VertexProgram pkVProgram)
    {
        assert(pkVProgram != null);
        LoadVProgram(pkVProgram);
        ResourceIdentifier pkID = pkVProgram.GetIdentifier(this);
        assert(pkID != null);

        OnEnableVProgram(pkID);

        // Process the renderer constants.
        int i;
        for (i = 0; i < pkVProgram.GetRCQuantity(); i++)
        {
            RendererConstant pkRC = pkVProgram.GetRC(i);
            assert(pkRC != null);
            SetRendererConstant(pkRC.GetType(),pkRC.GetData());
            SetVProgramConstant(ConstantType.CT_RENDERER,pkRC.GetBaseRegister(),
                                pkRC.GetRegisterQuantity(),pkRC.GetData());
        }

        // Process the numerical constants.
        for (i = 0; i < pkVProgram.GetNCQuantity(); i++)
        {
            NumericalConstant pkNC = pkVProgram.GetNC(i);
            assert(pkNC != null);
            SetVProgramConstant(ConstantType.CT_NUMERICAL,pkNC.GetRegister(),1,
                                pkNC.GetData());
        }

        // Process the user-defined constants.
        for (i = 0; i < pkVProgram.GetUCQuantity(); i++)
        {
            UserConstant pkUC = pkVProgram.GetUC(i);
            assert(pkUC != null);
            SetVProgramConstant(ConstantType.CT_USER,pkUC.GetBaseRegister(),
                                pkUC.GetRegisterQuantity(),pkUC.GetData());
        }
    }

    public void DisableVProgram (VertexProgram pkVProgram)
    {
        assert(pkVProgram != null);
        ResourceIdentifier pkID = pkVProgram.GetIdentifier(this);
        assert(pkID != null);
        OnDisableVProgram(pkID);
    }

    public void EnablePProgram (PixelProgram pkPProgram)
    {
        assert(pkPProgram != null);
        LoadPProgram(pkPProgram);
        ResourceIdentifier pkID = pkPProgram.GetIdentifier(this);
        assert(pkID != null);

        OnEnablePProgram(pkID);

        // Process the renderer constants.
        int i;
        for (i = 0; i < pkPProgram.GetRCQuantity(); i++)
        {
            RendererConstant pkRC = pkPProgram.GetRC(i);
            assert(pkRC != null);
            SetRendererConstant(pkRC.GetType(),pkRC.GetData());
            SetPProgramConstant(ConstantType.CT_RENDERER,pkRC.GetBaseRegister(),
                                pkRC.GetRegisterQuantity(),pkRC.GetData());
        }

        // Process the numerical constants.
        for (i = 0; i < pkPProgram.GetNCQuantity(); i++)
        {
            NumericalConstant pkNC = pkPProgram.GetNC(i);
            assert(pkNC != null);
            SetPProgramConstant(ConstantType.CT_NUMERICAL,pkNC.GetRegister(),1,
                                pkNC.GetData());
        }

        // Process the user-defined constants.
        for (i = 0; i < pkPProgram.GetUCQuantity(); i++)
        {
            UserConstant pkUC = pkPProgram.GetUC(i);
            assert(pkUC != null);
            SetPProgramConstant(ConstantType.CT_USER,pkUC.GetBaseRegister(),
                                pkUC.GetRegisterQuantity(),pkUC.GetData());
        }
    }

    public void DisablePProgram (PixelProgram pkPProgram)
    {
        assert(pkPProgram != null);
        ResourceIdentifier pkID = pkPProgram.GetIdentifier(this);
        assert(pkID != null);
        OnDisablePProgram(pkID);
    }

    public void EnableTexture (Texture pkTexture)
    {
        assert(pkTexture != null);
        LoadTexture(pkTexture);
        ResourceIdentifier pkID = pkTexture.GetIdentifier(this);
        assert(pkID != null);
        OnEnableTexture(pkID);
    }

    public void DisableTexture (Texture pkTexture)
    {
        ResourceIdentifier pkID = pkTexture.GetIdentifier(this);
        assert(pkID != null);
        OnDisableTexture(pkID);
    }

    public ResourceIdentifier EnableVBuffer (final Attributes rkIAttr,
                                             final Attributes rkOAttr)
    {
        VertexBuffer pkVBuffer = m_pkGeometry.VBuffer;
        LoadVBuffer(rkIAttr,rkOAttr,pkVBuffer);

        ResourceIdentifier pkID = null;
        for (int i = 0; i < pkVBuffer.GetInfoQuantity(); i++)
        {
            pkID = pkVBuffer.GetIdentifier(i,this);
            if (pkID != null)
            {
                //if (rkIAttr == (Attributes)pkID)
                if (pkID instanceof gov.nih.mipav.view.WildMagic.LibRenderers.OpenGLRenderer.VBufferID )
                {
                    if ( ((gov.nih.mipav.view.WildMagic.LibRenderers.OpenGLRenderer.VBufferID)pkID).IAttr == rkIAttr)
                    {
                        // Found a matching vertex buffer in video memory.
                        break;
                    }
                }
            }
        }
        assert(pkID != null);

        OnEnableVBuffer(pkID);
        return pkID;
    }

    public void DisableVBuffer (ResourceIdentifier pkID)
    {
        OnDisableVBuffer(pkID);
    }

    public void EnableIBuffer ()
    {
        IndexBuffer pkIBuffer = m_pkGeometry.IBuffer;
        LoadIBuffer(pkIBuffer);
        ResourceIdentifier pkID = pkIBuffer.GetIdentifier(this);
        assert(pkID != null);
        OnEnableIBuffer(pkID);
    }

    public void DisableIBuffer ()
    {
        IndexBuffer pkIBuffer = m_pkGeometry.IBuffer;
        ResourceIdentifier pkID = pkIBuffer.GetIdentifier(this);
        assert(pkID != null);
        OnDisableIBuffer(pkID);
    }


    // For use by effects with lights.
    public void SetLight (int i, Light pkLight)
    {
        assert(0 <= i && i < m_iMaxLights);
        m_aspkLight[i] = pkLight;
    }

    public Light GetLight (int i)
    {
        assert(0 <= i && i < m_iMaxLights);
        return (Light)(m_aspkLight[i]);
    }

    // For use by effects with projectors.
    public void SetProjector (Camera pkProjector)
    {
        m_pkProjector = pkProjector;
    }

    public Camera GetProjector ()
    {
        return m_pkProjector;
    }

    // Enable or disable which color channels will be written to the color
    // buffer.
    public void SetColorMask (boolean bAllowRed, boolean bAllowGreen,
        boolean bAllowBlue, boolean bAllowAlpha)
    {
        m_bAllowRed = bAllowRed;
        m_bAllowGreen = bAllowGreen;
        m_bAllowBlue = bAllowBlue;
        m_bAllowAlpha = bAllowAlpha;
    }

    public void GetColorMask (boolean[] rbAllowColor)
    {
        rbAllowColor[0] = m_bAllowRed;
        rbAllowColor[1] = m_bAllowGreen;
        rbAllowColor[2] = m_bAllowBlue;
        rbAllowColor[3] = m_bAllowAlpha;
    }

    // Include additional clip planes.  The input plane must be in model
    // coordinates.  It is transformed internally to camera coordinates to
    // support clipping in clip space.
    public abstract void EnableUserClipPlane (int i, Plane3f rkPlane);
    public abstract void DisableUserClipPlane (int i);

    // Support for model-to-world transformation management.
    public void SetWorldTransformation ()
    {
        m_kWorldMatrix = m_pkGeometry.HWorld;
    }

    public void RestoreWorldTransformation ()
    {
        // Stub for derived classes.
    }

    // The input transformation is applied to world-space vertices before
    // the view matrix is applied.
    public void SetPostWorldTransformation (final Matrix4f rkMatrix)
    {
        m_kSaveViewMatrix = m_kViewMatrix;
        m_kViewMatrix = rkMatrix.TransposeTimes(m_kSaveViewMatrix);
    }

    public void RestorePostWorldTransformation ()
    {
        m_kViewMatrix = m_kSaveViewMatrix;
    }


    // The Renderer is an abstract base class.
    protected Renderer (FrameBuffer.FormatType eFormat, FrameBuffer.DepthType eDepth,
                        FrameBuffer.StencilType eStencil,
                        FrameBuffer.BufferingType eBuffering,
                        FrameBuffer.MultisamplingType eMultisampling, int iWidth,
                        int iHeight)
    {
        m_eFormat = eFormat;
        m_eDepth = eDepth;
        m_eStencil = eStencil;
        m_eBuffering = eBuffering;
        m_eMultisampling = eMultisampling;
        m_iWidth = iWidth;
        m_iHeight = iHeight;
        m_kBackgroundColor = new ColorRGBA(ColorRGBA.WHITE);
        m_kWorldMatrix = new Matrix4f(Matrix4f.IDENTITY);
        m_kSaveWorldMatrix = new Matrix4f(Matrix4f.IDENTITY);
        m_kViewMatrix = new Matrix4f(Matrix4f.IDENTITY);
        m_kSaveViewMatrix = new Matrix4f(Matrix4f.IDENTITY);
        m_kProjectionMatrix = new Matrix4f(Matrix4f.IDENTITY);
        m_kSaveProjectionMatrix = new Matrix4f(Matrix4f.IDENTITY);
    }
    // Support for camera access and transformation setting.
    public void OnFrameChange ()
    {
        Vector3f rkEye = m_pkCamera.GetLocation();
        Vector3f rkRVector = m_pkCamera.GetRVector();
        Vector3f rkUVector = m_pkCamera.GetUVector();
        Vector3f rkDVector = m_pkCamera.GetDVector();

        m_kViewMatrix = new Matrix4f(
                                     rkRVector.X(),
                                     rkUVector.X(),
                                     rkDVector.X(),
                                     0.0f,
                                     rkRVector.Y(),
                                     rkUVector.Y(),
                                     rkDVector.Y(),
                                     0.0f,
                                     rkRVector.Z(),
                                     rkUVector.Z(),
                                     rkDVector.Z(),
                                     0.0f,
                                     -rkRVector.Dot(rkEye),
                                     -rkUVector.Dot(rkEye),
                                     -rkDVector.Dot(rkEye),
                                     1.0f );
    }

    public void OnFrustumChange ()
    {
        if (m_pkCamera == null)
        {
            return;
        }

        float fRMin = m_pkCamera.GetRMin();
        float fRMax = m_pkCamera.GetRMax();
        float fUMin = m_pkCamera.GetUMin();
        float fUMax = m_pkCamera.GetUMax();
        float fDMin = m_pkCamera.GetDMin();
        float fDMax = m_pkCamera.GetDMax();
        //m_pkCamera.GetFrustum(fRMin,fRMax,fUMin,fUMax,fDMin,fDMax);

        float fInvRDiff = 1.0f/(fRMax - fRMin);
        float fInvUDiff = 1.0f/(fUMax - fUMin);
        float fInvDDiff = 1.0f/(fDMax - fDMin);

        if (m_pkCamera.Perspective)
        {
            m_kProjectionMatrix = new Matrix4f(
                                               2.0f*fDMin*fInvRDiff,
                                               0.0f,
                                               0.0f,
                                               0.0f,
                                               0.0f,
                                               2.0f*fDMin*fInvUDiff,
                                               0.0f,
                                               0.0f,
                                               -(fRMin + fRMax)*fInvRDiff,
                                               -(fUMin + fUMax)*fInvUDiff,
                                               fDMax*fInvDDiff,
                                               1.0f,
                                               0.0f,
                                               0.0f,
                                               -fDMax*fDMin*fInvDDiff,
                                               0.0f );
        }
        else
        {
            m_kProjectionMatrix = new Matrix4f(
                                               2.0f*fInvRDiff,
                                               0.0f,
                                               0.0f,
                                               0.0f,
                                               0.0f,
                                               2.0f*fInvUDiff,
                                               0.0f,
                                               0.0f,
                                               0.0f,
                                               0.0f,
                                               fInvDDiff,
                                               0.0f,
                                               -(fRMin + fRMax)*fInvRDiff,
                                               -(fUMin + fUMax)*fInvUDiff,
                                               -fDMin*fInvDDiff,
                                               1.0f );
        }
    }

    public abstract void OnViewportChange ();
    public abstract void OnDepthRangeChange ();

    // Resource loading and releasing (to/from video memory).
    public abstract ResourceIdentifier OnLoadVProgram (VertexProgram pkVProgram);
    public abstract void OnReleaseVProgram (ResourceIdentifier pkID);
    public abstract ResourceIdentifier OnLoadPProgram (PixelProgram pkPProgram);
    public abstract void OnReleasePProgram (ResourceIdentifier pkID);
    public abstract ResourceIdentifier OnLoadTexture (Texture pkTexture);
    public abstract void OnReloadTexture (ResourceIdentifier pkID);
    public abstract void OnReleaseTexture (ResourceIdentifier pkID);
    public abstract ResourceIdentifier OnLoadVBuffer (final Attributes rkIAttr,
                                                      final Attributes rkOAttr,
                                                      VertexBuffer pkVBuffer);
    public abstract void OnReleaseVBuffer (ResourceIdentifier pkID);
    public abstract ResourceIdentifier OnLoadIBuffer (IndexBuffer pkIBuffer);
    public abstract void OnReleaseIBuffer (ResourceIdentifier pkID);

    public abstract void OnEnableVProgram (ResourceIdentifier pkID);
    public abstract void OnDisableVProgram (ResourceIdentifier pkID);
    public abstract void OnEnablePProgram (ResourceIdentifier pkID);
    public abstract void OnDisablePProgram (ResourceIdentifier pkID);
    public abstract void OnEnableTexture (ResourceIdentifier pkID);
    public abstract void OnDisableTexture (ResourceIdentifier pkID);
    public abstract void OnEnableVBuffer (ResourceIdentifier pkID);
    public abstract void OnDisableVBuffer (ResourceIdentifier pkID);
    public abstract void OnEnableIBuffer (ResourceIdentifier pkID);
    public abstract void OnDisableIBuffer (ResourceIdentifier pkID);

    // The operations are
    //   0 = matrix
    //   1 = transpose of matrix
    //   2 = inverse of matrix
    //   3 = inverse-transpose of matrix
    public void GetTransform ( Matrix4f rkMat, int iOperation, float[] afData)
    {
        if (iOperation == 0)
        {
            // matrix
            rkMat.GetData(afData);
        }
        else if (iOperation == 1)
        {
            // transpose of matrix
            Matrix4f kMT = rkMat.Transpose();
            kMT.GetData(afData);
        }
        else if (iOperation == 2)
        {
            // inverse of matrix
            Matrix4f kMI = rkMat.Inverse();
            kMI.GetData(afData);
        }
        else
        {
            // inverse-transpose of matrix
            Matrix4f kMIT = rkMat.Inverse().Transpose();
            kMIT.GetData(afData);
        }
    }

    public void SetConstantWMatrix (int iOperation, float[] afData)
    {
        GetTransform(m_kWorldMatrix,iOperation,afData);
    }

    public void SetConstantVMatrix (int iOperation, float[] afData)
    {
        GetTransform(m_kViewMatrix,iOperation,afData);
    }

    public void SetConstantPMatrix (int iOperation, float[] afData)
    {
        GetTransform(m_kProjectionMatrix,iOperation,afData);
    }

    public void SetConstantWVMatrix (int iOperation, float[] afData)
    {
        Matrix4f kWV = m_kWorldMatrix.mult(m_kViewMatrix);
        GetTransform(kWV,iOperation,afData);
    }

    public void SetConstantVPMatrix (int iOperation, float[] afData)
    {
        Matrix4f kVP = m_kViewMatrix.mult(m_kProjectionMatrix);
        GetTransform(kVP,iOperation,afData);
    }

    public void SetConstantWVPMatrix (int iOperation, float[] afData)
    {
        Matrix4f kWVP = m_kWorldMatrix.mult(m_kViewMatrix.mult(m_kProjectionMatrix));
        GetTransform(kWVP,iOperation,afData);
    }


    // These functions do not use the option parameter, but the parameter is
    // included to allow for a class-static array of function pointers to
    // handle all shader constants.
    public void SetConstantMaterialEmissive (int iPlaceHolder, float[] afData)
    {
        MaterialState pkMaterial = (MaterialState)(m_aspkState[GlobalState.StateType.MATERIAL.Value()]);
        afData[0] = pkMaterial.Emissive.R();
        afData[1] = pkMaterial.Emissive.G();
        afData[2] = pkMaterial.Emissive.B();
        afData[3] = 1.0f;
    }

    public void SetConstantMaterialAmbient (int iPlaceHolder, float[] afData)
    {
        MaterialState pkMaterial = (MaterialState)(m_aspkState[GlobalState.StateType.MATERIAL.Value()]);
        afData[0] = pkMaterial.Ambient.R();
        afData[1] = pkMaterial.Ambient.G();
        afData[2] = pkMaterial.Ambient.B();
        afData[3] = 1.0f;
    }

    public void SetConstantMaterialDiffuse (int iPlaceHolder, float[] afData)
    {
        MaterialState pkMaterial = (MaterialState)(m_aspkState[GlobalState.StateType.MATERIAL.Value()]);
        afData[0] = pkMaterial.Diffuse.R();
        afData[1] = pkMaterial.Diffuse.G();
        afData[2] = pkMaterial.Diffuse.B();
        afData[3] = pkMaterial.Alpha;
    }

    public void SetConstantMaterialSpecular (int iPlaceHolder, float[] afData)
    {
        MaterialState pkMaterial = (MaterialState)(m_aspkState[GlobalState.StateType.MATERIAL.Value()]);
        afData[0] = pkMaterial.Specular.R();
        afData[1] = pkMaterial.Specular.G();
        afData[2] = pkMaterial.Specular.B();
        afData[3] = pkMaterial.Shininess;
    }

    public void SetConstantCameraModelPosition (int iPlaceHolder, float[] afData)
    {
        Vector3f kMLocation = m_pkGeometry.World.ApplyInverse(m_pkCamera.GetLocation());

        afData[0] = kMLocation.X();
        afData[1] = kMLocation.Y();
        afData[2] = kMLocation.Z();
        afData[3] = 1.0f;
    }

    public void SetConstantCameraModelDirection (int iPlaceHolder, float[] afData)
    {
        Vector3f kMDVector = m_pkGeometry.World.InvertVector(m_pkCamera.GetDVector());

        afData[0] = kMDVector.X();
        afData[1] = kMDVector.Y();
        afData[2] = kMDVector.Z();
        afData[3] = 0.0f;
    }

    public void SetConstantCameraModelUp (int iPlaceHolder, float[] afData)
    {
        Vector3f kMUVector = m_pkGeometry.World.InvertVector(m_pkCamera.GetUVector());

        afData[0] = kMUVector.X();
        afData[1] = kMUVector.Y();
        afData[2] = kMUVector.Z();
        afData[3] = 0.0f;
    }

    public void SetConstantCameraModelRight (int iPlaceHolder, float[] afData)
    {
        Vector3f kMRVector = m_pkGeometry.World.InvertVector(m_pkCamera.GetRVector());

        afData[0] = kMRVector.X();
        afData[1] = kMRVector.Y();
        afData[2] = kMRVector.Z();
        afData[3] = 0.0f;
    }

    public void SetConstantCameraWorldPosition (int iPlaceHolder, float[] afData)
    {
        Vector3f kWLocation = m_pkCamera.GetLocation();

        afData[0] = kWLocation.X();
        afData[1] = kWLocation.Y();
        afData[2] = kWLocation.Z();
        afData[3] = 1.0f;
    }

    public void SetConstantCameraWorldDirection (int iPlaceHolder, float[] afData)
    {
        Vector3f kWDVector = m_pkCamera.GetDVector();

        afData[0] = kWDVector.X();
        afData[1] = kWDVector.Y();
        afData[2] = kWDVector.Z();
        afData[3] = 0.0f;
    }

    public void SetConstantCameraWorldUp (int iPlaceHolder, float[] afData)
    {
        Vector3f kWUVector = m_pkCamera.GetUVector();

        afData[0] = kWUVector.X();
        afData[1] = kWUVector.Y();
        afData[2] = kWUVector.Z();
        afData[3] = 0.0f;
    }

    public void SetConstantCameraWorldRight (int iPlaceHolder, float[] afData)
    {
        Vector3f kWRVector = m_pkCamera.GetRVector();

        afData[0] = kWRVector.X();
        afData[1] = kWRVector.Y();
        afData[2] = kWRVector.Z();
        afData[3] = 0.0f;
    }

    public void SetConstantProjectorModelPosition (int iPlaceHolder, float[] afData)
    {
        assert(m_pkProjector != null);

        Vector3f kMLocation = m_pkGeometry.World.ApplyInverse(m_pkProjector.GetLocation());

        afData[0] = kMLocation.X();
        afData[1] = kMLocation.Y();
        afData[2] = kMLocation.Z();
        afData[3] = 1.0f;
    }

    public void SetConstantProjectorModelDirection (int iPlaceHolder, float[] afData)
    {
        assert(m_pkProjector != null);

        Vector3f kMDVector = m_pkGeometry.World.InvertVector(m_pkProjector.GetDVector());

        afData[0] = kMDVector.X();
        afData[1] = kMDVector.Y();
        afData[2] = kMDVector.Z();
        afData[3] = 0.0f;
    }

    public void SetConstantProjectorModelUp (int iPlaceHolder, float[] afData)
    {
        assert(m_pkProjector != null);

        Vector3f kMUVector = m_pkGeometry.World.InvertVector(m_pkProjector.GetUVector());

        afData[0] = kMUVector.X();
        afData[1] = kMUVector.Y();
        afData[2] = kMUVector.Z();
        afData[3] = 0.0f;
    }

    public void SetConstantProjectorModelRight (int iPlaceHolder, float[] afData)
    {
        assert(m_pkProjector != null);

        Vector3f kMRVector = m_pkGeometry.World.InvertVector(m_pkProjector.GetRVector());

        afData[0] = kMRVector.X();
        afData[1] = kMRVector.Y();
        afData[2] = kMRVector.Z();
        afData[3] = 0.0f;
    }

    public void SetConstantProjectorWorldPosition (int iPlaceHolder, float[] afData)
    {
        assert(m_pkProjector != null);

        Vector3f kWLocation = m_pkProjector.GetLocation();

        afData[0] = kWLocation.X();
        afData[1] = kWLocation.Y();
        afData[2] = kWLocation.Z();
        afData[3] = 1.0f;
    }

    public void SetConstantProjectorWorldDirection (int iPlaceHolder, float[] afData)
    {
        assert(m_pkProjector != null);

        Vector3f kWDVector = m_pkProjector.GetDVector();

        afData[0] = kWDVector.X();
        afData[1] = kWDVector.Y();
        afData[2] = kWDVector.Z();
        afData[3] = 0.0f;
    }

    public void SetConstantProjectorWorldUp (int iPlaceHolder, float[] afData)
    {
        assert(m_pkProjector != null);

        Vector3f kWUVector = m_pkProjector.GetUVector();

        afData[0] = kWUVector.X();
        afData[1] = kWUVector.Y();
        afData[2] = kWUVector.Z();
        afData[3] = 0.0f;
    }

    public void SetConstantProjectorWorldRight (int iPlaceHolder, float[] afData)
    {
        assert(m_pkProjector != null);

        Vector3f kWRVector = m_pkProjector.GetRVector();

        afData[0] = kWRVector.X();
        afData[1] = kWRVector.Y();
        afData[2] = kWRVector.Z();
        afData[3] = 0.0f;
    }


    public void SetConstantProjectorMatrix (int iPlaceHolder, float[] afData)
    {
        assert(m_pkProjector != null);
        
        // Set up the view matrix for the projector.
        Vector3f rkEye = m_pkProjector.GetLocation();
        Vector3f rkRVector = m_pkProjector.GetRVector();
        Vector3f rkUVector = m_pkProjector.GetUVector();
        Vector3f rkDVector = m_pkProjector.GetDVector();
        float fRdE = rkRVector.Dot(rkEye);
        float fUdE = rkUVector.Dot(rkEye);
        float fDdE = rkDVector.Dot(rkEye);
        Matrix4f kProjVMatrix = new Matrix4f(
                                             rkRVector.X(), rkUVector.X(), rkDVector.X(), 0.0f,
                                             rkRVector.Y(), rkUVector.Y(), rkDVector.Y(), 0.0f,
                                             rkRVector.Z(), rkUVector.Z(), rkDVector.Z(), 0.0f,
                                             -fRdE,        -fUdE,        -fDdE,        1.0f);
        
        // Set up the projection matrix for the projector.
        float fRMin = m_pkProjector.GetRMin();
        float fRMax = m_pkProjector.GetRMax(); 
        float fUMin = m_pkProjector.GetUMin();
        float fUMax = m_pkProjector.GetUMax();
        float fDMin = m_pkProjector.GetDMin();
        float fDMax = m_pkProjector.GetDMax();

        float fInvRDiff = 1.0f/(fRMax - fRMin);
        float fInvUDiff = 1.0f/(fUMax - fUMin);
        float fInvDDiff = 1.0f/(fDMax - fDMin);
        float fRTerm0 = fDMin*fInvRDiff;
        float fUTerm0 = fDMin*fInvUDiff;
        float fDTerm0 = fDMin*fInvDDiff;
        float fRTerm1 = -(fRMin+fRMax)*fInvRDiff;
        float fUTerm1 = -(fUMin+fUMax)*fInvUDiff;
        float fDTerm1 = fDMax*fInvDDiff;
        Matrix4f kProjPMatrix = new Matrix4f(
                                             2.0f*fRTerm0, 0.0f,         0.0f,           0.0f,
                                             0.0f,         2.0f*fUTerm0, 0.0f,           0.0f,
                                             fRTerm1,      fUTerm1,      fDTerm1,        1.0f,
                                             0.0f,         0.0f,         -fDMax*fDTerm0, 0.0f);
        
        // Set up the bias and scale matrix for the projector.
        Matrix4f kProjBSMatrix = new Matrix4f(
                                              0.5f,0.0f,0.0f,0.0f,
                                              0.0f,0.5f,0.0f,0.0f,
                                              0.0f,0.0f,1.0f,0.0f,
                                              0.5f,0.5f,0.0f,1.0f);

        // The projector matrix.
        Matrix4f kProjectorMatrix =
            m_kWorldMatrix.mult( kProjVMatrix.mult( kProjPMatrix.mult( kProjBSMatrix ) ) );

        GetTransform(kProjectorMatrix,0,afData);
    }


    // These functions set the light state.  The index iLight is between 0
    // and 7 (eight lights are currently supported).
    public void SetConstantLightModelPosition (int iLight, float[] afData)
    {
        Light pkLight = GetLight(iLight);
        if (pkLight != null)
        {
            Vector3f kMPosition = m_pkGeometry.World.ApplyInverse(pkLight.Position);

            afData[0] = kMPosition.X();
            afData[1] = kMPosition.Y();
            afData[2] = kMPosition.Z();
            afData[3] = 1.0f;
        }
        else
        {
            afData[0] = 0.0f;
            afData[1] = 0.0f;
            afData[2] = 0.0f;
            afData[3] = 1.0f;
        }
    }

    public void SetConstantLightModelDirection (int iLight, float[] afData)
    {
        Light pkLight = GetLight(iLight);
        if (pkLight != null)
        {
            Vector3f kMDVector = m_pkGeometry.World.InvertVector(
                                                                 pkLight.DVector);

            afData[0] = kMDVector.X();
            afData[1] = kMDVector.Y();
            afData[2] = kMDVector.Z();
            afData[3] = 0.0f;
        }
        else
        {
            afData[0] = 0.0f;
            afData[1] = 0.0f;
            afData[2] = 0.0f;
            afData[3] = 0.0f;
        }
    }

    public void SetConstantLightWorldPosition (int iLight, float[] afData)
    {
        Light pkLight = GetLight(iLight);
        if (pkLight != null)
        {
            afData[0] = pkLight.Position.X();
            afData[1] = pkLight.Position.Y();
            afData[2] = pkLight.Position.Z();
            afData[3] = 1.0f;
        }
        else
        {
            afData[0] = 0.0f;
            afData[1] = 0.0f;
            afData[2] = 0.0f;
            afData[3] = 1.0f;
        }
    }

    public void SetConstantLightWorldDirection (int iLight, float[] afData)
    {
        Light pkLight = GetLight(iLight);
        if (pkLight != null)
        {
            afData[0] = pkLight.DVector.X();
            afData[1] = pkLight.DVector.Y();
            afData[2] = pkLight.DVector.Z();
            afData[3] = 0.0f;
        }
        else
        {
            afData[0] = 0.0f;
            afData[1] = 0.0f;
            afData[2] = 0.0f;
            afData[3] = 0.0f;
        }
    }

    public void SetConstantLightAmbient (int iLight, float[] afData)
    {
        Light pkLight = GetLight(iLight);
        assert(pkLight != null);
        afData[0] = pkLight.Ambient.R();
        afData[1] = pkLight.Ambient.G();
        afData[2] = pkLight.Ambient.B();
        afData[3] = 1.0f;
    }

    public void SetConstantLightDiffuse (int iLight, float[] afData)
    {
        Light pkLight = GetLight(iLight);
        if (pkLight != null)
        {
            afData[0] = pkLight.Diffuse.R();
            afData[1] = pkLight.Diffuse.G();
            afData[2] = pkLight.Diffuse.B();
            afData[3] = 1.0f;
        }
        else
        {
            afData[0] = 0.0f;
            afData[1] = 0.0f;
            afData[2] = 0.0f;
            afData[3] = 1.0f;
        }
    }

    public void SetConstantLightSpecular (int iLight, float[] afData)
    {
        Light pkLight = GetLight(iLight);
        assert(pkLight != null);
        afData[0] = pkLight.Specular.R();
        afData[1] = pkLight.Specular.G();
        afData[2] = pkLight.Specular.B();
        afData[3] = 1.0f;
    }

    public void SetConstantLightSpotCutoff (int iLight, float[] afData)
    {
        Light pkLight = GetLight(iLight);
        if (pkLight != null)
        {
            afData[0] = pkLight.Angle;
            afData[1] = pkLight.CosAngle;
            afData[2] = pkLight.SinAngle;
            afData[3] = pkLight.Exponent;
        }
        else
        {
            afData[0] = (float)Math.PI;
            afData[1] = -1.0f;
            afData[2] = 0.0f;
            afData[3] = 1.0f;
        }
    }

    public void SetConstantLightAttenuation (int iLight, float[] afData)
    {
        Light pkLight = GetLight(iLight);
        if (pkLight != null)
        {
            afData[0] = pkLight.Constant;
            afData[1] = pkLight.Linear;
            afData[2] = pkLight.Quadratic;
            afData[3] = pkLight.Intensity;
        }
        else
        {
            afData[0] = 1.0f;
            afData[1] = 0.0f;
            afData[2] = 0.0f;
            afData[3] = 1.0f;
        }
    }

    public void SetRendererConstant (RendererConstant.Type eRCType, float[] afData)
    {
        int iRCType = eRCType.Value();
        int iFunction;

        if (iRCType <= RendererConstant.Type.WVP_MATRIX_INVERSE_TRANSPOSE.Value())
        {
            // The constant involves matrices.  These occur in blocks of 6:
            // matrix, transpose, inverse, inverse-transpose.
            int iOperation = iRCType / 6;
            iFunction = iRCType % 6;
            SCFunction(iFunction,iOperation,afData);
        }
        else if (iRCType <= RendererConstant.Type.PROJECTOR_MATRIX.Value())
        {
            // The constant is for material, camera, or projector data.  The
            // first parameter is irrelevant, so just set it to zero.  The "6"
            // in iFunction is the index into ms_aoSCFunction for the function
            // SetConstantMaterialEmissive.  The remainder of iFunction is a
            // relative offset to locate the material, fog, camera, or projector
            // SetConstant* functions.  The maximum iFunction value is 26.
            iFunction = 6 + iRCType - RendererConstant.Type.MATERIAL_EMISSIVE.Value();
            SCFunction(iFunction,0,afData);
        }
        else
        {
            // The constant involves lights.  These occur in blocks of 9:  model
            // position, model direction, world position, world direction,
            // ambient, diffuse, specular, spotcutoff, shininess.  The 27 in
            // iFunction is the index into ms_aoSCFunction for the function
            // SetConstantLightModelPosition.  The remainder of iFunction is a
            // relative offset to locate the light SetConstant* functions.  The
            // maximum iFunction value is 35.
            int iDiff = iRCType - RendererConstant.Type.LIGHT0_MODEL_POSITION.Value();
            int iLight = iDiff / 9;
            int iAttribute = iDiff % 9;
            iFunction = 27 + iAttribute;
            SCFunction( iFunction, iLight, afData );
        }
    }

    private void SCFunction( int iFunction, int iParam, float[] afData )
    {
        //Replace function-pointer array with a case statement, since the
        //function-pointer array is only used locally.
        switch ( iFunction )
        {
        case 0: SetConstantWMatrix( iParam, afData ); return;
        case 1: SetConstantVMatrix( iParam, afData ); return;
        case 2: SetConstantPMatrix( iParam, afData ); return;
        case 3: SetConstantWVMatrix( iParam, afData ); return;
        case 4: SetConstantVPMatrix( iParam, afData ); return;
        case 5: SetConstantWVPMatrix( iParam, afData ); return;
                
        case 6: SetConstantMaterialEmissive( iParam, afData ); return;
        case 7: SetConstantMaterialAmbient( iParam, afData ); return;
        case 8: SetConstantMaterialDiffuse( iParam, afData ); return;
        case 9: SetConstantMaterialSpecular( iParam, afData ); return;
                    
        case 10: SetConstantCameraModelPosition( iParam, afData ); return;
        case 11: SetConstantCameraModelDirection( iParam, afData ); return;
        case 12: SetConstantCameraModelUp( iParam, afData ); return;
        case 13: SetConstantCameraModelRight( iParam, afData ); return;
        case 14: SetConstantCameraWorldPosition( iParam, afData ); return;
        case 15: SetConstantCameraWorldDirection( iParam, afData ); return;
        case 16: SetConstantCameraWorldUp( iParam, afData ); return;
        case 17: SetConstantCameraWorldRight( iParam, afData ); return;
                
        case 18: SetConstantProjectorModelPosition( iParam, afData ); return;
        case 19: SetConstantProjectorModelDirection( iParam, afData ); return;
        case 20: SetConstantProjectorModelUp( iParam, afData ); return;
        case 21: SetConstantProjectorModelRight( iParam, afData ); return;
        case 22: SetConstantProjectorWorldPosition( iParam, afData ); return;
        case 23: SetConstantProjectorWorldDirection( iParam, afData ); return;
        case 24: SetConstantProjectorWorldUp( iParam, afData ); return;
        case 25: SetConstantProjectorWorldRight( iParam, afData ); return;
        case 26: SetConstantProjectorMatrix( iParam, afData ); return;
                
        case 27: SetConstantLightModelPosition( iParam, afData ); return;
        case 28: SetConstantLightModelDirection( iParam, afData ); return;
        case 29: SetConstantLightWorldPosition( iParam, afData ); return;
        case 30: SetConstantLightWorldDirection( iParam, afData ); return;
        case 31: SetConstantLightAmbient( iParam, afData ); return;
        case 32: SetConstantLightDiffuse( iParam, afData ); return;
        case 33: SetConstantLightSpecular( iParam, afData ); return;
        case 34: SetConstantLightSpotCutoff( iParam, afData ); return;
        case 35: SetConstantLightAttenuation( iParam, afData ); return;
        }
    }

    // data members

    // Resource limits.  The values are set by the Renderer-derived objects.
    protected int m_iMaxLights;
    protected int m_iMaxColors;
    protected int m_iMaxTCoords;
    protected int m_iMaxVShaderImages;
    protected int m_iMaxPShaderImages;
    protected int m_iMaxStencilIndices;
    protected int m_iMaxUserClipPlanes;

    // Parameters for the drawing window and frame buffer.
    FrameBuffer.FormatType m_eFormat;
    FrameBuffer.DepthType m_eDepth;
    FrameBuffer.StencilType m_eStencil;
    FrameBuffer.BufferingType m_eBuffering;
    FrameBuffer.MultisamplingType m_eMultisampling;
    protected int m_iWidth, m_iHeight;
    protected ColorRGBA m_kBackgroundColor;
    protected boolean m_bAllowRed, m_bAllowGreen, m_bAllowBlue, m_bAllowAlpha;

    // The camera for establishing the view frustum.
    protected Camera m_pkCamera;

    // Global render states.
    protected GlobalState[] m_aspkState =
        new GlobalState[GlobalState.StateType.MAX_STATE_TYPE.Value()];

    // Light storage for lookup by the shader-constant-setting functions.
    // The Renderer-derived classes must allocate this array during
    // construction, creating m_iMaxLights elements.  The Renderer class
    // deallocates the array during destruction.
    protected Object[] m_aspkLight;

    // The projector for various effects such as projected textures and
    // shadow maps.
    protected Camera m_pkProjector;

    // Current Geometry object for drawing.
    protected Geometry m_pkGeometry;

    // Transformations used in the geometric pipeline.  These matrices are
    // stored to support 1x4 row vectors times 4x4 matrices.
    protected Matrix4f m_kWorldMatrix, m_kSaveWorldMatrix;
    protected Matrix4f m_kViewMatrix, m_kSaveViewMatrix;
    protected Matrix4f m_kProjectionMatrix, m_kSaveProjectionMatrix;

    // Current font for text drawing.
    protected int m_iFontID;

    // Support for mirror effects (default 'false').
    protected boolean m_bReverseCullFace;

    // Toggle for fullscreen/window mode.
    protected boolean m_bFullscreen;

    // Data for point size, line width, and line stipple.
    protected float m_fPointSize = 1;  // default = 1
    protected float m_fLineWidth = 1;  // default = 1
    protected int m_iLineStippleRepeat = 0;  // default = 0 (disabled)
    protected short m_usLineStipplePattern = 0;  // default = 0 (disabled)
}
