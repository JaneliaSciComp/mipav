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
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Shaders.*;

/** Abstract API for renderers.  Each graphics API must implement this
 * layer. */
public abstract class Renderer
{
    /** Release memory. */
    public void finalize()
    {
        if ( m_kBackgroundColor != null )
        {
            m_kBackgroundColor.finalize();
            m_kBackgroundColor = null;
        }
        if ( m_kWorldMatrix != null )
        {
            m_kWorldMatrix.finalize();
            m_kWorldMatrix = null;
        }
        if ( m_kSaveWorldMatrix != null )
        {
            m_kSaveWorldMatrix.finalize();
            m_kSaveWorldMatrix = null;
        }
        if ( m_kViewMatrix != null )
        {
            m_kViewMatrix.finalize();
            m_kViewMatrix = null;
        }
        if ( m_kSaveViewMatrix != null )
        {
            m_kSaveViewMatrix.finalize();
            m_kSaveViewMatrix = null;
        }
        if ( m_kProjectionMatrix != null )
        {
            m_kProjectionMatrix.finalize();
            m_kProjectionMatrix = null;
        }
        if ( m_kSaveProjectionMatrix != null )
        {
            m_kSaveProjectionMatrix.finalize();
            m_kSaveProjectionMatrix = null;
        }
        
        if ( m_aspkState != null )
        {
            for ( int i = 0;
            i < GlobalState.StateType.MAX_STATE_TYPE.Value(); i++ )
            {
                if ( m_aspkState[i] != null )
                {
                    m_aspkState[i].finalize();
                    m_aspkState[i] = null;
                }
            }
            m_aspkState = null;
        }
        m_aspkLight = null;
    }
    
    
    /** Run-time type information. */
    public enum RendererType
    {
        OPENGL,
        DIRECTX,
        SOFTWARE,
        MAX_RENDERER_TYPES
    };

    /** Make this renderer context the active one. */
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

    /** Return it this is the active renderer.
     * @return true.
     */
    public boolean IsActive ()
    {
        // stub for derived classes
        return true;
    }


    /** Renderer-specific information for loading shader programs.
     * @return extension
     */
    public abstract String GetExtension ();
    /** Renderer-specific information for loading shader programs.
     * @return comment characeter
     */
    public abstract  char GetCommentCharacter ();

    /** Set the camera.
     * @param pkCamera, new camera object.
     */
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

    /** Get the camera.
     * @return the camera object.
     */
    public final Camera GetCamera ()
    {
        return m_pkCamera;
    }

    /** Set the geometry object that is to be drawn.
     * @param pkGeometry new geometry.
     */
    public final void SetGeometry (Geometry pkGeometry)
    {
        m_pkGeometry = pkGeometry;
    }

    /** Get the geometry object.
     * @return the geometry.
     */
    public final Geometry GetGeometry ()
    {
        return m_pkGeometry;
    }

    /** Get Frame buffer format type parameter.
     * @return Frame buffer format type parameter.
     */
    public final FrameBuffer.FormatType GetFormatType ()
    {
        return m_eFormat;
    }

    /** Get Frame buffer depth type parameter.
     * @return Frame buffer depth type parameter.
     */
    public final FrameBuffer.DepthType GetDepthType ()
    {
        return m_eDepth;
    }

    /** Get Frame buffer stencil type parameter.
     * @return Frame buffer stencil type parameter.
     */
    public final FrameBuffer.StencilType GetStencilType ()
    {
        return m_eStencil;
    }

    /** Get Frame buffer buffering type parameter.
     * @return Frame buffer buffering type parameter.
     */
    public final FrameBuffer.BufferingType GetBufferingType ()
    {
        return m_eBuffering;
    }

    /** Get Frame buffer multisampling type parameter.
     * @return Frame buffer multisampling type parameter.
     */
    public final FrameBuffer.MultisamplingType GetMultisamplingType ()
    {
        return m_eMultisampling;
    }

    /** Get Window width parameter.
     * @return Window width parameter.
     */
    public final int GetWidth ()
    {
        return m_iWidth;
    }

    /** Get Window height parameter.
     * @return Window height parameter.
     */
    public final int GetHeight ()
    {
        return m_iHeight;
    }

    /** Set Window parameters.
     * @param iWidth Window width parameter.
     * @param iHeight Window height parameter.
     */
    public void Resize (int iWidth, int iHeight)
    {
        m_iWidth = iWidth;
        m_iHeight = iHeight;
        OnViewportChange();
    }

    /** Toggle full screen mode. */
    public void ToggleFullscreen ()
    {
        m_bFullscreen = !m_bFullscreen;
    }

    /** Set the Background color.
     * @param rkColor, new background color.
     */
    public void SetBackgroundColor (ColorRGBA rkColor)
    {
        m_kBackgroundColor = rkColor;
    }

    /** Get the Background color.
     * @return the background color.
     */
    public final ColorRGBA GetBackgroundColor () 
    {
        return m_kBackgroundColor;
    }

    /** Support for predraw and postdraw semantics.
     * @return true
     */
    public boolean BeginScene ()
    {
        // stub for derived classes
        return true;
    }

    /** Support for predraw and postdraw semantics.
     */
    public void EndScene ()
    {
        // stub for derived classes
    }

    /** Clear back buffer. */
    public abstract void ClearBackBuffer ();
    /** Clear depth buffer. */
    public abstract void ClearZBuffer ();
    /** Clear stencil buffer. */
    public abstract void ClearStencilBuffer ();
    /** Clear all buffers. */
    public abstract void ClearBuffers ();
    /** Display the back buffer. */
    public abstract void DisplayBackBuffer ();

    /** Clear the back buffer in the specified subwindow.
     * @param iXPos, the x-position for the subwindow
     * @param iYPos, the y-position for the subwindow
     * @param iWidth, the subwindow width
     * @param iHeight, the subwindow height
     */
    public abstract void ClearBackBuffer (int iXPos, int iYPos, int iWidth,
                                          int iHeight);
    /** Clear the depth buffer in the specified subwindow.
     * @param iXPos, the x-position for the subwindow
     * @param iYPos, the y-position for the subwindow
     * @param iWidth, the subwindow width
     * @param iHeight, the subwindow height
     */
    public abstract void ClearZBuffer (int iXPos, int iYPos, int iWidth,
                                       int iHeight);
    /** Clear the stencil buffer in the specified subwindow.
     * @param iXPos, the x-position for the subwindow
     * @param iYPos, the y-position for the subwindow
     * @param iWidth, the subwindow width
     * @param iHeight, the subwindow height
     */
    public abstract void ClearStencilBuffer (int iXPos, int iYPos, int iWidth,
                                             int iHeight);

    /** Clear the all buffers in the specified subwindow.
     * @param iXPos, the x-position for the subwindow
     * @param iYPos, the y-position for the subwindow
     * @param iWidth, the subwindow width
     * @param iHeight, the subwindow height
     */
    public abstract void ClearBuffers (int iXPos, int iYPos, int iWidth,
                                       int iHeight);

    /** The main entry point to drawing in the derived-class renderers. */
    public abstract void DrawElements ();

    /** Object drawing.
     * @param rkVisibleSet, set of objects to draw.
     */
    public void DrawScene (VisibleSet rkVisibleSet)
    {
        // NOTE:  The stack of 2-tuples is limited to having 64 elements.  This
        // should be plenty, because the chances of having 64 global effects
        // in the same path is small (that is a *lot* of effects to apply in
        // one frame).  If it needs to be larger for your applications, increase
        // the maximum size.
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

    /** Draw the specified geometry.
     * @param pkGeometry, geometry to draw.
     */
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

    /** Clear a font based on ID. 
     * @param iFontID, the font to remove.
     */
    public abstract void UnloadFont (int iFontID);
    /** Select a font based on ID. 
     * @param iFontID, the font to use.
     * @return true if the font exists, false otherwise.
     */
    public abstract boolean SelectFont (int iFontID);
    /** 
     * Draw text.
     * @param iX, the x-position for the start of the text being drawn.
     * @param iY, the y-position for the start of the text being drawn.
     * @param rkColor, text color
     * @param acText, text to draw
     */
    public abstract void Draw (int iX, int iY, final ColorRGBA rkColor,
                               final char[] acText);

    /**
     * 2D drawing. Draw a byte bitmap.
     * @param aucBuffer, the bitmap to draw.
     */
    public abstract void Draw (final byte[] aucBuffer);

    /** Object drawing.
     * @param pkEffect, ShaderEffect to apply to geometry.
     * @param rbPrimaryEffect, true if this is the primary effect.
     * @return true on sucess.
     */
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


    /** Set point size.
     * @param fSize, point size.
     */
    public void SetPointSize (float fSize)
    {
        if (fSize > 0.0f)
        {
            m_fPointSize = fSize;
        }
    }

    /** Get point size
     * @return point size.
     */
    public final float GetPointSize ()
    {
        return m_fPointSize;
    }

    /** Set line width.
     * @param fWidth, line width.
     */
    public void SetLineWidth (float fWidth)
    {
        if (fWidth > 0.0f)
        {
            m_fLineWidth = fWidth;
        }
    }

    /** Get line width.
     * @return line width.
     */
    public final float GetLineWidth ()
    {
        return m_fLineWidth;
    }

    /** Line stippling is disabled when either of "repeat" or "pattern" is
     * zero.
     * @param iRepeat, stiple repeat.
     * @param usPattern, stiple pattern.
     */
    public void SetLineStipple (int iRepeat, short usPattern)
    {
        if (iRepeat < 0)
        {
            iRepeat = 0;
        }
        m_iLineStippleRepeat = iRepeat;
        m_usLineStipplePattern = usPattern;
    }

    /** Get Line stippling repeat.
     * @return line stiple repeat.
     */
    public final int GetLineStippleRepeat ()
    {
        return m_iLineStippleRepeat;
    }

    /** Get Line stippling pattern.
     * @return line stiple pattern.
     */
    public final short GetLineStipplePattern ()
    {
        return m_usLineStipplePattern;
    }


    /** Maximum lights resource limits.
     * @return the maximum number of lights.
     */
    public final int GetMaxLights ()
    {
        return m_iMaxLights;
    }

    /** Maximum colors resource limits.
     * @return the maximum number of color units.
     */
    public final int GetMaxColors ()
    {
        return m_iMaxColors;
    }

    /** Maximum texture coordinates resource limits.
     * @return the maximum number of texture coordinates.
     */
    public final int GetMaxTCoords ()
    {
        return m_iMaxTCoords;
    }

    /** Maximum vertex shader images resource limits.
     * @return the maximum number of vertex shader images.
     */
    public final int GetMaxVShaderImages ()
    {
        return m_iMaxVShaderImages;
    }

    /** Maximum pixel shader resource limits.
     * @return the maximum number of pixel shader images.
     */
    public final int GetMaxPShaderImages ()
    {
        return m_iMaxPShaderImages;
    }

    /** Maximum stencil resource limits.
     * @return the maximum number of stencil indices.
     */
    public final int GetMaxStencilIndices ()
    {
        return m_iMaxStencilIndices;
    }

    /** Maximum user clip planes resource limits.
     * @return the maximum number of user clip planes.
     */
    public final int GetMaxUserClipPlanes ()
    {
        return m_iMaxUserClipPlanes;
    }

    /** Global render state management.
     * @param aspkState, set new GlobalStates  
     */
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

    /** Global render state management.
     * @param aspkState, restore GlobalStates  
     */
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

    /** Alpha state management.
     * @param pkState, new AlphaState  
     */
    public void SetAlphaState (AlphaState pkState)
    {
        m_aspkState[GlobalState.StateType.ALPHA.Value()] = pkState;
    }

    /** Cull state management.
     * @param pkState, new CullState  
     */
    public void SetCullState (CullState pkState)
    {
        m_aspkState[GlobalState.StateType.CULL.Value()] = pkState;
    }

    /** Material state management.
     * @param pkState, new MaterialState  
     */
    public void SetMaterialState (MaterialState pkState)
    {
        m_aspkState[GlobalState.StateType.MATERIAL.Value()] = pkState;
    }

    /** PolygonOffset state management.
     * @param pkState, new PolygonOffsetState  
     */
    public void SetPolygonOffsetState (PolygonOffsetState pkState)
    {
        m_aspkState[GlobalState.StateType.POLYGONOFFSET.Value()] = pkState;
    }

    /** Stencil state management.
     * @param pkState, new StencilState  
     */
    public void SetStencilState (StencilState pkState)
    {
        m_aspkState[GlobalState.StateType.STENCIL.Value()] = pkState;
    }

    /** Wireframe state management.
     * @param pkState, new WireframeState  
     */
    public void SetWireframeState (WireframeState pkState)
    {
        m_aspkState[GlobalState.StateType.WIREFRAME.Value()] = pkState;
    }

    /** ZBuffer state management.
     * @param pkState, new ZBufferState  
     */
    public void SetZBufferState (ZBufferState pkState)
    {
        m_aspkState[GlobalState.StateType.ZBUFFER.Value()] = pkState;
    }

    /** Get AlphaState.
     * @return AlphaState  
     */
    public final AlphaState GetAlphaState ()
    {
        return (AlphaState)(m_aspkState[GlobalState.StateType.ALPHA.Value()]);
    }

    /** Get CullState.
     * @return CullState  
     */
    public final CullState GetCullState ()
    {
        return (CullState)(m_aspkState[GlobalState.StateType.CULL.Value()]);
    }

    /** Get MaterialState.
     * @return MaterialState  
     */
    public final MaterialState GetMaterialState ()
    {
        return (MaterialState)(m_aspkState[GlobalState.StateType.MATERIAL.Value()]);
    }

    /** Get PolygonOffsetState.
     * @return PolygonOffsetState  
     */
    public final PolygonOffsetState GetPolygonOffsetState ()
    {
        return (PolygonOffsetState)(m_aspkState[GlobalState.StateType.POLYGONOFFSET.Value()]);
    }

    /** Get StencilState.
     * @return StencilState  
     */
    public final StencilState GetStencilState ()
    {
        return (StencilState)(m_aspkState[GlobalState.StateType.STENCIL.Value()]);
    }

    /** Get WireframeState.
     * @return WireframeState  
     */
    public final WireframeState GetWireframeState ()
    {
        return (WireframeState)(m_aspkState[GlobalState.StateType.WIREFRAME.Value()]);
    }

    /** Get ZBufferState.
     * @return ZBufferState  
     */
    public final ZBufferState GetZBufferState ()
    {
        return (ZBufferState)(m_aspkState[GlobalState.StateType.ZBUFFER.Value()]);
    }

    /** Set reverse cull face.
     * @param bReverseCullFace, when true do reverse culling.
     */
    public void SetReverseCullFace (boolean bReverseCullFace)
    {
        m_bReverseCullFace = bReverseCullFace;
    }

    /** Get reverse cull face.
     * @return reverse culling.
     */
    public final boolean GetReverseCullFace ()
    {
        return m_bReverseCullFace;
    }

    /** Function pointer types for binding and unbinding resources. */
    public ReleaseFunction m_kReleaseFunction;

    /** Resource loading and releasing.
     * @param pkScene, load all resources for this scene.
     */
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

    /** Resource loading and releasing.
     * @param pkScene, release all resources for this scene.
     */
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

    /** Resource loading and releasing.
     * @param pkGeometry, load all resources for this geometry object.
     */
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

    /** Resource loading and releasing.
     * @param pkGeometry, release all resources for this geometry object.
     */
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


    /** Resource loading and releasing.
     * @param pkEffect, load all resources for this effect.
     */
    public void LoadResources (Effect pkEffect)
    {
        assert(pkEffect != null);
        pkEffect.LoadResources(this,null);
    }

    /** Resource loading and releasing.
     * @param pkEffect, release all resources for this effect.
     */
    public void ReleaseResources (Effect pkEffect)
    {
        assert(pkEffect != null);
        pkEffect.ReleaseResources(this,null);
    }

    /** Resource loading and releasing.
     * @param pkVProgram, load all resources for the Vertex Program.
     */
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
            //System.err.println("LoadVProgram");
            pkVProgram.OnLoad(this, new ReleaseFunctionVertex(this),pkID);
        }
    }

    /** Resource loading and releasing.
     * @param pkVProgram, release all resources for the Vertex Program.
     */
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

    /** Resource loading and releasing.
     * @param pkPProgram, load all resources for the Pixel Program.
     */
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
            //System.err.println("LoadPProgram");
            pkPProgram.OnLoad(this,new ReleaseFunctionPixel(this),pkID);
        }
    }

    /** Resource loading and releasing.
     * @param pkPProgram, release all resources for the Pixel Program.
     */
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

    /** Resource loading and releasing.
     * @param pkTexture, load all resources for the Texture.
     */
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
            //System.err.println("LoadTexture");
            pkTexture.OnLoad(this,new ReleaseFunctionTexture(this),pkID);
        }
        else if ( pkTexture.Reload() )
        {
            OnReloadTexture(pkID);
            pkTexture.Reload(false);
        }
    }

    /** Resource loading and releasing.
     * @param pkTexture, release all resources for the Texture.
     */
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

    /** Resource loading and releasing.
     * @param pkTexture, reload all resources for the Texture.
     */
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

    /** Resource loading and releasing.
     * @param rkIAttr, vertex program input attributes.
     * @param rkOAttr, vertex program output attributes.
     * @param pkVBuffer, load all resources for the VertexBuffer.
     */
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
        //System.err.println("LoadVBuffer");
        pkVBuffer.OnLoad(this,new ReleaseFunctionVBuffer(this),pkID);
    }

    /** Resource loading and releasing.
     * @param pkVBuffer, release all resources for the VertexBuffer.
     */
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

    /** Resource loading and releasing.
     * @param pkIBuffer, load all resources for the IndexBuffer.
     */
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
            //System.err.println("LoadIBuffer");
            pkIBuffer.OnLoad(this,new ReleaseFunctionIBuffer(this),pkID);
        }
    }

    /** Resource loading and releasing.
     * @param pkIBuffer, release all resources for the IndexBuffer.
     */
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

    /** ConstantType */
    public enum ConstantType  
    {
        CT_RENDERER,
        CT_NUMERICAL,
        CT_USER
    };

    /** Resource enabling and disabling.
     * Sets the values for the Vertex Program constant parameters.
     * @param eCTYpe, the ConstantType parameter (RENDERER, NUMERICAL, USER)
     * @param iBaseRegister, the register to load the parameter values into
     * @param iRegisterQuantity, the number of registers
     * @param afData, the parameter values.
     */
    public abstract void SetVProgramConstant ( Renderer.ConstantType eCType, int iBaseRegister,
                                               int iRegisterQuantity, float[] afData);
    /** Resource enabling and disabling.
     * Sets the values for the Pixel Program constant parameters.
     * @param eCTYpe, the ConstantType parameter (RENDERER, NUMERICAL, USER)
     * @param iBaseRegister, the register to load the parameter values into
     * @param iRegisterQuantity, the number of registers
     * @param afData, the parameter values.
     */
    public abstract void SetPProgramConstant ( Renderer.ConstantType eCType, int iBaseRegister,
                                               int iRegisterQuantity, float[] afData);

    /** Enable the vertex program parameter.
     * @param pkVProgram, vertex program to enable.
     */
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

        // Process the user-defined constants.
        for (i = 0; i < pkVProgram.GetUCQuantity(); i++)
        {
            UserConstant pkUC = pkVProgram.GetUC(i);
            assert(pkUC != null);
            SetVProgramConstant(ConstantType.CT_USER,pkUC.GetBaseRegister(),
                                pkUC.GetRegisterQuantity(),pkUC.GetData());
        }
    }

    /** Disable the vertex program parameter.
     * @param pkVProgram, vertex program to disable.
     */
    public void DisableVProgram (VertexProgram pkVProgram)
    {
        assert(pkVProgram != null);
        ResourceIdentifier pkID = pkVProgram.GetIdentifier(this);
        assert(pkID != null);
        OnDisableVProgram(pkID);
    }

    /** Enable the pixel program parameter.
     * @param pkPProgram, pixel program to enable.
     */
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

        // Process the user-defined constants.
        for (i = 0; i < pkPProgram.GetUCQuantity(); i++)
        {
            UserConstant pkUC = pkPProgram.GetUC(i);
            assert(pkUC != null);
            SetPProgramConstant(ConstantType.CT_USER,pkUC.GetBaseRegister(),
                                pkUC.GetRegisterQuantity(),pkUC.GetData());
        }
    }

    /** Disable the pixel program parameter.
     * @param pkPProgram, pixel program to disable.
     */
    public void DisablePProgram (PixelProgram pkPProgram)
    {
        assert(pkPProgram != null);
        ResourceIdentifier pkID = pkPProgram.GetIdentifier(this);
        assert(pkID != null);
        OnDisablePProgram(pkID);
    }

    /** Enable the texture parameter.
     * @param pkTexture, texture to enable.
     */
    public void EnableTexture (Texture pkTexture)
    {
        assert(pkTexture != null);
        LoadTexture(pkTexture);
        ResourceIdentifier pkID = pkTexture.GetIdentifier(this);
        assert(pkID != null);
        OnEnableTexture(pkID);
    }

    /** Disable the texture parameter.
     * @param pkTexture, texture to disable.
     */
    public void DisableTexture (Texture pkTexture)
    {
        ResourceIdentifier pkID = pkTexture.GetIdentifier(this);
        assert(pkID != null);
        OnDisableTexture(pkID);
    }

    /** Enable the vertex buffer attributes.
     * @param rkIAttr, vertex shader program input attributes.
     * @param rkOAttr, vertex shader program output attributes.
     * @return resource identifier for vertex buffer.
     */
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

    /** Disable the vertex buffer.
     * @param pkID, vertex buffer id.
     */
    public void DisableVBuffer (ResourceIdentifier pkID)
    {
        OnDisableVBuffer(pkID);
    }

    /** Enable the index buffer associated with the geometry object.  */
    public void EnableIBuffer ()
    {
        IndexBuffer pkIBuffer = m_pkGeometry.IBuffer;
        LoadIBuffer(pkIBuffer);
        ResourceIdentifier pkID = pkIBuffer.GetIdentifier(this);
        assert(pkID != null);
        OnEnableIBuffer(pkID);
    }

    /** Disable the index buffer associated with the geometry object.  */
    public void DisableIBuffer ()
    {
        IndexBuffer pkIBuffer = m_pkGeometry.IBuffer;
        ResourceIdentifier pkID = pkIBuffer.GetIdentifier(this);
        assert(pkID != null);
        OnDisableIBuffer(pkID);
    }


    /** Set light. For use by effects with lights.
     * @param i, light index to set.
     * @param pkLight, new light.
     */
    public void SetLight (int i, Light pkLight)
    {
        assert(0 <= i && i < m_iMaxLights);
        m_aspkLight[i] = pkLight;
    }

    /** Get light. For use by effects with lights.
     * @param i, light index to get.
     * @return light at index i.
     */
    public Light GetLight (int i)
    {
        assert(0 <= i && i < m_iMaxLights);
        return (Light)(m_aspkLight[i]);
    }

    /** For use by effects with projectors.
     * @param pkProjector Camera object projector.
     */
    public void SetProjector (Camera pkProjector)
    {
        m_pkProjector = pkProjector;
    }

    /** For use by effects with projectors.
     * @return Camera object projector.
     */
    public Camera GetProjector ()
    {
        return m_pkProjector;
    }

    /** Enable which color channels will be written to the color buffer.
     * @param bAllowRed, when true allow the red channel to be written.
     * @param bAllowGreen, when true allow the green channel to be written.
     * @param bAllowBlue, when true allow the blue channel to be written.
     * @param bAllowAlpha, when true allow the alpha channel to be written.
     */
    public void SetColorMask (boolean bAllowRed, boolean bAllowGreen,
                              boolean bAllowBlue, boolean bAllowAlpha)
    {
        m_bAllowRed = bAllowRed;
        m_bAllowGreen = bAllowGreen;
        m_bAllowBlue = bAllowBlue;
        m_bAllowAlpha = bAllowAlpha;
    }

    /** Get which color channels will be written to the color buffer.
     * @param rbAllowColor, boolean[] to write with allow values.
     */
    public void GetColorMask (boolean[] rbAllowColor)
    {
        rbAllowColor[0] = m_bAllowRed;
        rbAllowColor[1] = m_bAllowGreen;
        rbAllowColor[2] = m_bAllowBlue;
        rbAllowColor[3] = m_bAllowAlpha;
    }

    /** Include additional clip planes.  The input plane must be in model
     * coordinates.  It is transformed internally to camera coordinates to
     * support clipping in clip space.
     * @param i, the GL_CLIP_PLANE0 (+i) clip plane to include.
     * @param rkPlane, the clip plane definition.
     */
    public abstract void EnableUserClipPlane (int i, Plane3f rkPlane);
    /** Disables additional clip planes.
     * @param i, the clip plane to disable (GL_CLIP_PLANE0 + i).
     */
    public abstract void DisableUserClipPlane (int i);

    /** Support for model-to-world transformation management. */
    public void SetWorldTransformation ()
    {
        m_kWorldMatrix = m_pkGeometry.HWorld;
    }

    /** Support for model-to-world transformation management. */
    public void RestoreWorldTransformation ()
    {
        // Stub for derived classes.
    }

    /** The input transformation is applied to world-space vertices before the
     * view matrix is applied.
     * @param rkMatrix input transformation is applied to world-space vertices
     * before the view matrix is applied.
     */
    public void SetPostWorldTransformation (final Matrix4f rkMatrix)
    {
        m_kSaveViewMatrix = m_kViewMatrix;
        m_kViewMatrix = rkMatrix.TransposeTimes(m_kSaveViewMatrix);
    }

    /** Restores view matrix. */
    public void RestorePostWorldTransformation ()
    {
        m_kViewMatrix = m_kSaveViewMatrix;
    }


    /**
     * The Renderer is an abstract base class.
     * @param eFormat FormatType (NONE, RGB, RGBA, DEPTH)
     * @param eDepth DepthType (NONE, DEPTH_16, DEPTH_24, DEPTH_32)
     * @param eStencil StencilType (NONE, STENCIL_8)
     * @param eBuffering BufferingType (SINGLE, DOUBLE)
     * @param iWidth, the window width
     * @param iHeight, the window height
     */
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

    /** Support for camera access and transformation setting. */
    public void OnFrameChange ()
    {
        Vector3f rkEye = m_pkCamera.GetLocation();
        Vector3f rkRVector = m_pkCamera.GetRVector();
        Vector3f rkUVector = m_pkCamera.GetUVector();
        Vector3f rkDVector = m_pkCamera.GetDVector();

        m_kViewMatrix.SetData(
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

    /** Support for camera access and transformation setting. */
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
            m_kProjectionMatrix.SetData(
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
            m_kProjectionMatrix.SetData(
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

    /** Called when the viewport changes. Updates the camera. */
    public abstract void OnViewportChange ();
    /** Called when the depth range changes. Updates the camera. */
    public abstract void OnDepthRangeChange ();

    /** Resource loading and releasing (to/from video memory).
     * @param pkVProgram the vertex program to generate/bind
     * @return the new ResourceIdentifier for the VertexProgram
     */
    public abstract ResourceIdentifier OnLoadVProgram (VertexProgram pkVProgram);
    /**
     * Release the VertexProgram described in the ResourceIdentifier parameter.
     * @param pkID, the ResourceIdentifier with the VertexProgram to release.
     */
    public abstract void OnReleaseVProgram (ResourceIdentifier pkID);
    /** Resource loading and releasing (to/from video memory).
     * @param pkPProgram the pixel program to generate/bind
     * @return the new ResourceIdentifier for the PixelProgram
     */
    public abstract ResourceIdentifier OnLoadPProgram (PixelProgram pkPProgram);
    /**
     * Release the PixelProgram described in the ResourceIdentifier parameter.
     * @param pkID, the ResourceIdentifier with the PixelProgram to release.
     */
    public abstract void OnReleasePProgram (ResourceIdentifier pkID);
    /** Resource loading and releasing (to/from video memory).
     * @param pkTexture the Texture to generate/bind
     * @return the new ResourceIdentifier for the Texture
     */
    public abstract ResourceIdentifier OnLoadTexture (Texture pkTexture);
    /** Resource loading and releasing (to/from video memory).
     * @param pkTexture the Texture to generate/bind
     * @return the new ResourceIdentifier for the Texture
     */
    public abstract void OnReloadTexture (ResourceIdentifier pkID);
    /**
     * Release the Texture described in the ResourceIdentifier parameter.
     * @param pkID, the ResourceIdentifier with the Texture to release.
     */
    public abstract void OnReleaseTexture (ResourceIdentifier pkID);
    /** Resource loading and releasing (to/from video memory).
     * @param rkIAtr The Input Attributes for the VertexBuffer
     * @param rkOAtr The Output Attributes for the VertexBuffer
     * @param pkVBuffer The VertexBuffer to generate/enable
     * @return the new ResourceIdentifier for the VertexBuffer
     */
    public abstract ResourceIdentifier OnLoadVBuffer (final Attributes rkIAttr,
                                                      final Attributes rkOAttr,
                                                      VertexBuffer pkVBuffer);
    /**
     * Release the VertexBuffer described in the ResourceIdentifier parameter.
     * @param pkID, the ResourceIdentifier with the VertexBuffer to release.
     */
    public abstract void OnReleaseVBuffer (ResourceIdentifier pkID);
    /** Resource loading and releasing (to/from video memory).
     * @param pkIBuffer the IndexBuffer to generate/enable
     * @return the new ResourceIdentifier for the IndexBuffer
     */
    public abstract ResourceIdentifier OnLoadIBuffer (IndexBuffer pkIBuffer);
    /**
     * Release the IndexBuffer described in the ResourceIdentifier parameter.
     * @param pkID, the ResourceIdentifier with the IndexBuffer to release.
     */
    public abstract void OnReleaseIBuffer (ResourceIdentifier pkID);

    /**
     * Enable the VertexProgram spefified by the ResourceIdentifer parameter pkID.
     * @param pkID the ResourceIdentifier describing the VertexProgram to enable.
     */
    public abstract void OnEnableVProgram (ResourceIdentifier pkID);
    /**
     * Disable the VertexProgram spefified by the ResourceIdentifer parameter pkID.
     * @param pkID the ResourceIdentifier describing the VertexProgram to disable.
     */
    public abstract void OnDisableVProgram (ResourceIdentifier pkID);
    /**
     * Enable the PixelProgram spefified by the ResourceIdentifer parameter pkID.
     * @param pkID the ResourceIdentifier describing the PixelProgram to enable.
     */
    public abstract void OnEnablePProgram (ResourceIdentifier pkID);
    /**
     * Disable the PixelProgram spefified by the ResourceIdentifer parameter pkID.
     * @param pkID the ResourceIdentifier describing the PixelProgram to disable.
     */
    public abstract void OnDisablePProgram (ResourceIdentifier pkID);
    /**
     * Enable the Texture spefified by the ResourceIdentifer parameter pkID.
     * @param pkID the ResourceIdentifier describing the Texture to enable.
     */
    public abstract void OnEnableTexture (ResourceIdentifier pkID);
    /**
     * Disable the Texture spefified by the ResourceIdentifer parameter pkID.
     * @param pkID the ResourceIdentifier describing the Texture to disable.
     */
    public abstract void OnDisableTexture (ResourceIdentifier pkID);
    /**
     * Enable the VertexBuffer spefified by the ResourceIdentifer parameter pkID.
     * @param pkID the ResourceIdentifier describing the VertexBuffer to enable.
     */
    public abstract void OnEnableVBuffer (ResourceIdentifier pkID);
    /**
     * Disable the VertexBuffer spefified by the ResourceIdentifer parameter pkID.
     * @param pkID the ResourceIdentifier describing the VertexBuffer to disable.
     */
    public abstract void OnDisableVBuffer (ResourceIdentifier pkID);
    /**
     * Enable the IndexBuffer spefified by the ResourceIdentifer parameter pkID.
     * @param pkID the ResourceIdentifier describing the Index to enable.
     */
    public abstract void OnEnableIBuffer (ResourceIdentifier pkID);
    /**
     * Disable the IndexBuffer spefified by the ResourceIdentifer parameter pkID.
     * @param pkID the ResourceIdentifier describing the Index to disable.
     */
    public abstract void OnDisableIBuffer (ResourceIdentifier pkID);
    
    /** Get from Matrix parameter and write into float[] parameter.
     * @param rkMat, matrix to read.
     * @param iOperation, 0 = matrix; 1 = transpose of matrix; 2 = inverse of
     * matrix; 3 = inverse-transpose of matrix
     * @param afData, float[] to write.
     */
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

    /** Set RendererConstant World matrix, store in float[] parameter.
     * @param iOperation, transform operation
     * @param afData, stores result.
     */
    public void SetConstantWMatrix (int iOperation, float[] afData)
    {
        GetTransform(m_kWorldMatrix,iOperation,afData);
    }

    /** Set RendererConstant View matrix, store in float[] parameter.
     * @param iOperation, transform operation
     * @param afData, stores result.
     */
    public void SetConstantVMatrix (int iOperation, float[] afData)
    {
        GetTransform(m_kViewMatrix,iOperation,afData);
    }

    /** Set RendererConstant Projection matrix, store in float[] parameter.
     * @param iOperation, transform operation
     * @param afData, stores result.
     */
    public void SetConstantPMatrix (int iOperation, float[] afData)
    {
        GetTransform(m_kProjectionMatrix,iOperation,afData);
    }

    /** Set RendererConstant World*View matrix, store in float[] parameter.
     * @param iOperation, transform operation
     * @param afData, stores result.
     */
    public void SetConstantWVMatrix (int iOperation, float[] afData)
    {
        //Matrix4f kWV = m_kWorldMatrix.mult(m_kViewMatrix);
        //GetTransform(kWV,iOperation,afData);
        m_kWorldMatrix.mult(m_kViewMatrix, m_kConstantMatrix);
        GetTransform(m_kConstantMatrix,iOperation,afData);
    }

    /** Set RendererConstant View*Projection matrix, store in float[] parameter.
     * @param iOperation, transform operation
     * @param afData, stores result.
     */
    public void SetConstantVPMatrix (int iOperation, float[] afData)
    {
//         Matrix4f kVP = m_kViewMatrix.mult(m_kProjectionMatrix);
//         GetTransform(kVP,iOperation,afData);
        m_kViewMatrix.mult(m_kProjectionMatrix, m_kConstantMatrix);
        GetTransform(m_kConstantMatrix,iOperation,afData);
    }

    /** Set RendererConstant World*View*Projection matrix, store in float[]
     * parameter.
     * @param iOperation, transform operation
     * @param afData, stores result.
     */
    public void SetConstantWVPMatrix (int iOperation, float[] afData)
    {
//         Matrix4f kWVP = m_kWorldMatrix.mult(m_kViewMatrix.mult(m_kProjectionMatrix));
//         GetTransform(kWVP,iOperation,afData);
        m_kViewMatrix.mult(m_kProjectionMatrix, m_kConstantMatrix);
        m_kWorldMatrix.mult(m_kConstantMatrix, m_kConstantMatrix);
        GetTransform(m_kConstantMatrix,iOperation,afData);
    }

    /** Set RendererConstant Material Emissive color, store in float[]
     * parameter.  These functions do not use the option parameter, but the
     * parameter is included to allow for a class-static array of function
     * pointers to handle all shader constants.
     * @param iPlaceHolder, transform operation placeholder
     * @param afData, stores result.
     */
    public void SetConstantMaterialEmissive (int iPlaceHolder, float[] afData)
    {
        MaterialState pkMaterial = (MaterialState)(m_aspkState[GlobalState.StateType.MATERIAL.Value()]);
        afData[0] = pkMaterial.Emissive.R();
        afData[1] = pkMaterial.Emissive.G();
        afData[2] = pkMaterial.Emissive.B();
        afData[3] = 1.0f;
    }

    /** Set RendererConstant Material Ambient color, store in float[]
     * parameter.  These functions do not use the option parameter, but the
     * parameter is included to allow for a class-static array of function
     * pointers to handle all shader constants.
     * @param iPlaceHolder, transform operation placeholder
     * @param afData, stores result.
     */
    public void SetConstantMaterialAmbient (int iPlaceHolder, float[] afData)
    {
        MaterialState pkMaterial = (MaterialState)(m_aspkState[GlobalState.StateType.MATERIAL.Value()]);
        afData[0] = pkMaterial.Ambient.R();
        afData[1] = pkMaterial.Ambient.G();
        afData[2] = pkMaterial.Ambient.B();
        afData[3] = 1.0f;
    }

    /** Set RendererConstant Material Diffuse color, store in float[]
     * parameter.  These functions do not use the option parameter, but the
     * parameter is included to allow for a class-static array of function
     * pointers to handle all shader constants.
     * @param iPlaceHolder, transform operation placeholder
     * @param afData, stores result.
     */
    public void SetConstantMaterialDiffuse (int iPlaceHolder, float[] afData)
    {
        MaterialState pkMaterial = (MaterialState)(m_aspkState[GlobalState.StateType.MATERIAL.Value()]);
        afData[0] = pkMaterial.Diffuse.R();
        afData[1] = pkMaterial.Diffuse.G();
        afData[2] = pkMaterial.Diffuse.B();
        afData[3] = pkMaterial.Alpha;
        //System.err.println(afData[0] + " " + afData[1] + " " + afData[2] );
    }

    /** Set RendererConstant Material Specular color, store in float[]
     * parameter.  These functions do not use the option parameter, but the
     * parameter is included to allow for a class-static array of function
     * pointers to handle all shader constants.
     * @param iPlaceHolder, transform operation placeholder
     * @param afData, stores result.
     */
    public void SetConstantMaterialSpecular (int iPlaceHolder, float[] afData)
    {
        MaterialState pkMaterial = (MaterialState)(m_aspkState[GlobalState.StateType.MATERIAL.Value()]);
        afData[0] = pkMaterial.Specular.R();
        afData[1] = pkMaterial.Specular.G();
        afData[2] = pkMaterial.Specular.B();
        afData[3] = pkMaterial.Shininess;
    }

    /** Set RendererConstant Camera Model Position, store in float[]
     * parameter.  These functions do not use the option parameter, but the
     * parameter is included to allow for a class-static array of function
     * pointers to handle all shader constants.
     * @param iPlaceHolder, transform operation placeholder
     * @param afData, stores result.
     */
    public void SetConstantCameraModelPosition (int iPlaceHolder, float[] afData)
    {
        Vector3f kMLocation = new Vector3f();
        m_pkGeometry.World.ApplyInverse(m_pkCamera.GetLocation(), kMLocation);
        afData[0] = kMLocation.X();
        afData[1] = kMLocation.Y();
        afData[2] = kMLocation.Z();
        afData[3] = 1.0f;
        kMLocation = null;
    }

    /** Set RendererConstant Camera Model Direction, store in float[]
     * parameter.  These functions do not use the option parameter, but the
     * parameter is included to allow for a class-static array of function
     * pointers to handle all shader constants.
     * @param iPlaceHolder, transform operation placeholder
     * @param afData, stores result.
     */
    public void SetConstantCameraModelDirection (int iPlaceHolder, float[] afData)
    {
        Vector3f kMDVector = m_pkGeometry.World.InvertVector(m_pkCamera.GetDVector());

        afData[0] = kMDVector.X();
        afData[1] = kMDVector.Y();
        afData[2] = kMDVector.Z();
        afData[3] = 0.0f;
    }

    /** Set RendererConstant Camera Model Up Vector, store in float[]
     * parameter.  These functions do not use the option parameter, but the
     * parameter is included to allow for a class-static array of function
     * pointers to handle all shader constants.
     * @param iPlaceHolder, transform operation placeholder
     * @param afData, stores result.
     */
    public void SetConstantCameraModelUp (int iPlaceHolder, float[] afData)
    {
        Vector3f kMUVector = m_pkGeometry.World.InvertVector(m_pkCamera.GetUVector());

        afData[0] = kMUVector.X();
        afData[1] = kMUVector.Y();
        afData[2] = kMUVector.Z();
        afData[3] = 0.0f;
    }

    /** Set RendererConstant Camera Model Right Vector, store in float[]
     * parameter.  These functions do not use the option parameter, but the
     * parameter is included to allow for a class-static array of function
     * pointers to handle all shader constants.
     * @param iPlaceHolder, transform operation placeholder
     * @param afData, stores result.
     */
    public void SetConstantCameraModelRight (int iPlaceHolder, float[] afData)
    {
        Vector3f kMRVector = m_pkGeometry.World.InvertVector(m_pkCamera.GetRVector());

        afData[0] = kMRVector.X();
        afData[1] = kMRVector.Y();
        afData[2] = kMRVector.Z();
        afData[3] = 0.0f;
    }

    /** Set RendererConstant Camera World Position, store in float[]
     * parameter.  These functions do not use the option parameter, but the
     * parameter is included to allow for a class-static array of function
     * pointers to handle all shader constants.
     * @param iPlaceHolder, transform operation placeholder
     * @param afData, stores result.
     */
    public void SetConstantCameraWorldPosition (int iPlaceHolder, float[] afData)
    {
        Vector3f kWLocation = m_pkCamera.GetLocation();

        afData[0] = kWLocation.X();
        afData[1] = kWLocation.Y();
        afData[2] = kWLocation.Z();
        afData[3] = 1.0f;
    }

    /** Set RendererConstant Camera World Direction Vector, store in float[]
     * parameter.  These functions do not use the option parameter, but the
     * parameter is included to allow for a class-static array of function
     * pointers to handle all shader constants.
     * @param iPlaceHolder, transform operation placeholder
     * @param afData, stores result.
     */
    public void SetConstantCameraWorldDirection (int iPlaceHolder, float[] afData)
    {
        Vector3f kWDVector = m_pkCamera.GetDVector();

        afData[0] = kWDVector.X();
        afData[1] = kWDVector.Y();
        afData[2] = kWDVector.Z();
        afData[3] = 0.0f;
    }

    /** Set RendererConstant Camera World Up Vector, store in float[]
     * parameter.  These functions do not use the option parameter, but the
     * parameter is included to allow for a class-static array of function
     * pointers to handle all shader constants.
     * @param iPlaceHolder, transform operation placeholder
     * @param afData, stores result.
     */
    public void SetConstantCameraWorldUp (int iPlaceHolder, float[] afData)
    {
        Vector3f kWUVector = m_pkCamera.GetUVector();

        afData[0] = kWUVector.X();
        afData[1] = kWUVector.Y();
        afData[2] = kWUVector.Z();
        afData[3] = 0.0f;
    }

    /** Set RendererConstant Camera World Right Vector, store in float[]
     * parameter.  These functions do not use the option parameter, but the
     * parameter is included to allow for a class-static array of function
     * pointers to handle all shader constants.
     * @param iPlaceHolder, transform operation placeholder
     * @param afData, stores result.
     */
    public void SetConstantCameraWorldRight (int iPlaceHolder, float[] afData)
    {
        Vector3f kWRVector = m_pkCamera.GetRVector();

        afData[0] = kWRVector.X();
        afData[1] = kWRVector.Y();
        afData[2] = kWRVector.Z();
        afData[3] = 0.0f;
    }

    /** Set RendererConstant Projector Model Position, store in float[]
     * parameter.  These functions do not use the option parameter, but the
     * parameter is included to allow for a class-static array of function
     * pointers to handle all shader constants.
     * @param iPlaceHolder, transform operation placeholder
     * @param afData, stores result.
     */
    public void SetConstantProjectorModelPosition (int iPlaceHolder, float[] afData)
    {
        assert(m_pkProjector != null);

        Vector3f kMLocation = m_pkGeometry.World.ApplyInverse(m_pkProjector.GetLocation());

        afData[0] = kMLocation.X();
        afData[1] = kMLocation.Y();
        afData[2] = kMLocation.Z();
        afData[3] = 1.0f;
    }

    /** Set RendererConstant Projector Model Direction Vector, store in float[]
     * parameter.  These functions do not use the option parameter, but the
     * parameter is included to allow for a class-static array of function
     * pointers to handle all shader constants.
     * @param iPlaceHolder, transform operation placeholder
     * @param afData, stores result.
     */
    public void SetConstantProjectorModelDirection (int iPlaceHolder, float[] afData)
    {
        assert(m_pkProjector != null);

        Vector3f kMDVector = m_pkGeometry.World.InvertVector(m_pkProjector.GetDVector());

        afData[0] = kMDVector.X();
        afData[1] = kMDVector.Y();
        afData[2] = kMDVector.Z();
        afData[3] = 0.0f;
    }

    /** Set RendererConstant Projector Model Up Vector, store in float[]
     * parameter.  These functions do not use the option parameter, but the
     * parameter is included to allow for a class-static array of function
     * pointers to handle all shader constants.
     * @param iPlaceHolder, transform operation placeholder
     * @param afData, stores result.
     */
    public void SetConstantProjectorModelUp (int iPlaceHolder, float[] afData)
    {
        assert(m_pkProjector != null);

        Vector3f kMUVector = m_pkGeometry.World.InvertVector(m_pkProjector.GetUVector());

        afData[0] = kMUVector.X();
        afData[1] = kMUVector.Y();
        afData[2] = kMUVector.Z();
        afData[3] = 0.0f;
    }

    /** Set RendererConstant Projector Model Right Vector, store in float[]
     * parameter.  These functions do not use the option parameter, but the
     * parameter is included to allow for a class-static array of function
     * pointers to handle all shader constants.
     * @param iPlaceHolder, transform operation placeholder
     * @param afData, stores result.
     */
    public void SetConstantProjectorModelRight (int iPlaceHolder, float[] afData)
    {
        assert(m_pkProjector != null);

        Vector3f kMRVector = m_pkGeometry.World.InvertVector(m_pkProjector.GetRVector());

        afData[0] = kMRVector.X();
        afData[1] = kMRVector.Y();
        afData[2] = kMRVector.Z();
        afData[3] = 0.0f;
    }

    /** Set RendererConstant Projector World Position, store in float[]
     * parameter.  These functions do not use the option parameter, but the
     * parameter is included to allow for a class-static array of function
     * pointers to handle all shader constants.
     * @param iPlaceHolder, transform operation placeholder
     * @param afData, stores result.
     */
    public void SetConstantProjectorWorldPosition (int iPlaceHolder, float[] afData)
    {
        assert(m_pkProjector != null);

        Vector3f kWLocation = m_pkProjector.GetLocation();

        afData[0] = kWLocation.X();
        afData[1] = kWLocation.Y();
        afData[2] = kWLocation.Z();
        afData[3] = 1.0f;
    }

    /** Set RendererConstant Projector World Direction Vector, store in float[]
     * parameter.  These functions do not use the option parameter, but the
     * parameter is included to allow for a class-static array of function
     * pointers to handle all shader constants.
     * @param iPlaceHolder, transform operation placeholder
     * @param afData, stores result.
     */
    public void SetConstantProjectorWorldDirection (int iPlaceHolder, float[] afData)
    {
        assert(m_pkProjector != null);

        Vector3f kWDVector = m_pkProjector.GetDVector();

        afData[0] = kWDVector.X();
        afData[1] = kWDVector.Y();
        afData[2] = kWDVector.Z();
        afData[3] = 0.0f;
    }

    /** Set RendererConstant Projector World Up Vector, store in float[]
     * parameter.  These functions do not use the option parameter, but the
     * parameter is included to allow for a class-static array of function
     * pointers to handle all shader constants.
     * @param iPlaceHolder, transform operation placeholder
     * @param afData, stores result.
     */
    public void SetConstantProjectorWorldUp (int iPlaceHolder, float[] afData)
    {
        assert(m_pkProjector != null);

        Vector3f kWUVector = m_pkProjector.GetUVector();

        afData[0] = kWUVector.X();
        afData[1] = kWUVector.Y();
        afData[2] = kWUVector.Z();
        afData[3] = 0.0f;
    }

    /** Set RendererConstant Projector World Right Vector, store in float[]
     * parameter.  These functions do not use the option parameter, but the
     * parameter is included to allow for a class-static array of function
     * pointers to handle all shader constants.
     * @param iPlaceHolder, transform operation placeholder
     * @param afData, stores result.
     */
    public void SetConstantProjectorWorldRight (int iPlaceHolder, float[] afData)
    {
        assert(m_pkProjector != null);

        Vector3f kWRVector = m_pkProjector.GetRVector();

        afData[0] = kWRVector.X();
        afData[1] = kWRVector.Y();
        afData[2] = kWRVector.Z();
        afData[3] = 0.0f;
    }


    /** Set RendererConstant Projector Matrix, store in float[] parameter.
     * These functions do not use the option parameter, but the parameter is
     * included to allow for a class-static array of function pointers to
     * handle all shader constants.
     * @param iPlaceHolder, transform operation placeholder
     * @param afData, stores result.
     */
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

        //System.err.println("SetConstantProjectorMatrix");
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
        //System.err.println("SetConstantProjectorMatrix");
        Matrix4f kProjPMatrix = new Matrix4f(
                                             2.0f*fRTerm0, 0.0f,         0.0f,           0.0f,
                                             0.0f,         2.0f*fUTerm0, 0.0f,           0.0f,
                                             fRTerm1,      fUTerm1,      fDTerm1,        1.0f,
                                             0.0f,         0.0f,         -fDMax*fDTerm0, 0.0f);
        
        // Set up the bias and scale matrix for the projector.
        //System.err.println("SetConstantProjectorMatrix");
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

    /** Set RendererConstant Light Model Position, store in float[] parameter.
     * @param iLight, index between 0 and 7 (eight lights are currently
     * supported).
     * @param afData, stores result.
     */
    public void SetConstantLightModelPosition (int iLight, float[] afData)
    {
        Light pkLight = GetLight(iLight);
        if (pkLight != null)
        {
            Vector3f kMPosition = new Vector3f(); 
            m_pkGeometry.World.ApplyInverse(pkLight.Position,kMPosition);

            afData[0] = kMPosition.X();
            afData[1] = kMPosition.Y();
            afData[2] = kMPosition.Z();
            afData[3] = 1.0f;
            kMPosition = null;
        }
        else
        {
            afData[0] = 0.0f;
            afData[1] = 0.0f;
            afData[2] = 0.0f;
            afData[3] = 1.0f;
        }
    }

    /** Set RendererConstant Light Model Direction, store in float[] parameter.
     * @param iLight, index between 0 and 7 (eight lights are currently
     * supported).
     * @param afData, stores result.
     */
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

        //System.err.println(afData[0] + " " + afData[1] + " " + afData[2] );
    }

    /** Set RendererConstant Light World Position, store in float[] parameter.
     * @param iLight, index between 0 and 7 (eight lights are currently
     * supported).
     * @param afData, stores result.
     */
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

    /** Set RendererConstant Light World Direction, store in float[] parameter.
     * @param iLight, index between 0 and 7 (eight lights are currently
     * supported).
     * @param afData, stores result.
     */
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

    /** Set RendererConstant Light Ambient color, store in float[] parameter.
     * @param iLight, index between 0 and 7 (eight lights are currently
     * supported).
     * @param afData, stores result.
     */
    public void SetConstantLightAmbient (int iLight, float[] afData)
    {
        Light pkLight = GetLight(iLight);
        assert(pkLight != null);
        afData[0] = pkLight.Ambient.R();
        afData[1] = pkLight.Ambient.G();
        afData[2] = pkLight.Ambient.B();
        afData[3] = 1.0f;
    }

    /** Set RendererConstant Light Diffuse color, store in float[] parameter.
     * @param iLight, index between 0 and 7 (eight lights are currently
     * supported).
     * @param afData, stores result.
     */
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

    /** Set RendererConstant Light Specular color, store in float[] parameter.
     * @param iLight, index between 0 and 7 (eight lights are currently
     * supported).
     * @param afData, stores result.
     */
    public void SetConstantLightSpecular (int iLight, float[] afData)
    {
        Light pkLight = GetLight(iLight);
        assert(pkLight != null);
        afData[0] = pkLight.Specular.R();
        afData[1] = pkLight.Specular.G();
        afData[2] = pkLight.Specular.B();
        afData[3] = 1.0f;
    }

    /** Set RendererConstant Light Spot Cutoff, store in float[] parameter.
     * @param iLight, index between 0 and 7 (eight lights are currently
     * supported).
     * @param afData, stores result.
     */
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

    /** Set RendererConstant Light Attenuation, store in float[] parameter.
     * @param iLight, index between 0 and 7 (eight lights are currently
     * supported).
     * @param afData, stores result.
     */
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

    /** Set RendererConstant based on eRCType parameter, store in float[]
     * parameter.
     * @param eRCType, type of RendererConstant to set.
     * @param afData, stores result.
     */
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

    /** Choose which Set RendererConstant function to call.
     * @param iFunction, RendererConstant set function to use.
     * @param iParam, parameter to set function.
     * @param afData, stores result.
     */
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

    /** Resource limits.  The values are set by the Renderer-derived objects. */
    /** Maximum number of lights. */
    protected int m_iMaxLights;
    /** Maximum number of color units. */
    protected int m_iMaxColors;
    /** Maximum number of texture coordinates. */
    protected int m_iMaxTCoords;
    /** Maximum number of vertex shader images. */
    protected int m_iMaxVShaderImages;
    /** Maximum number of pixel shader images. */
    protected int m_iMaxPShaderImages;
    /** Maximum number of stencil indices. */
    protected int m_iMaxStencilIndices;
    /** Maximum number of user clip planes. */
    protected int m_iMaxUserClipPlanes;

    /** Parameters for the drawing window and frame buffer. */
    /** Frame buffer format type. */
    FrameBuffer.FormatType m_eFormat;
    /** Frame buffer depth type. */
    FrameBuffer.DepthType m_eDepth;
    /** Frame buffer stencil type. */
    FrameBuffer.StencilType m_eStencil;
    /** Frame buffer buffering type. */
    FrameBuffer.BufferingType m_eBuffering;
    /** Frame buffer multisampling type. */
    FrameBuffer.MultisamplingType m_eMultisampling;
    /** Window width, height. */
    protected int m_iWidth, m_iHeight;
    /** Background color. */
    protected ColorRGBA m_kBackgroundColor;
    /** Color mask filter flags. */
    protected boolean m_bAllowRed, m_bAllowGreen, m_bAllowBlue, m_bAllowAlpha;

    /** The camera for establishing the view frustum. */
    protected Camera m_pkCamera;

    /** Global render states. */
    protected GlobalState[] m_aspkState =
        new GlobalState[GlobalState.StateType.MAX_STATE_TYPE.Value()];

    /** Light storage for lookup by the shader-constant-setting functions.
     * The Renderer-derived classes must allocate this array during
     * construction, creating m_iMaxLights elements.  The Renderer class
     * deallocates the array during destruction. */
    protected GraphicsObject[] m_aspkLight;

    /** The projector for various effects such as projected textures and
     * shadow maps. */
    protected Camera m_pkProjector;

    /** Current Geometry object for drawing. */
    protected Geometry m_pkGeometry;

    /** Transformations used in the geometric pipeline.  These matrices are
     * stored to support 1x4 row vectors times 4x4 matrices. */
    protected Matrix4f m_kWorldMatrix, m_kSaveWorldMatrix;
    protected Matrix4f m_kViewMatrix, m_kSaveViewMatrix;
    protected Matrix4f m_kProjectionMatrix, m_kSaveProjectionMatrix;

    /** Current font for text drawing. */
    protected int m_iFontID;

    /** Support for mirror effects (default 'false'). */
    protected boolean m_bReverseCullFace;

    /** Toggle for fullscreen/window mode. */
    protected boolean m_bFullscreen;

    /** Data for point size, line width, and line stipple. */
    protected float m_fPointSize = 1;  // default = 1
    protected float m_fLineWidth = 1;  // default = 1
    protected int m_iLineStippleRepeat = 0;  // default = 0 (disabled)
    protected short m_usLineStipplePattern = 0;  // default = 0 (disabled)


    final int iMaxTuples = 64;    // maximum number of stack elements
    int[][] aaiStack = new int[iMaxTuples][2];  // elements are (startIndex,finalIndex)

    Matrix4f m_kConstantMatrix = new Matrix4f();
}
