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
// Version: 4.0.2 (2006/10/18)

package gov.nih.mipav.view.WildMagic.LibRenderers.OpenGLRenderer;

import java.util.Vector;
import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import java.nio.*;
import com.sun.opengl.util.BufferUtil;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Shaders.*;

public class OpenGLRenderer extends Renderer
{
   
    /**
     * Create an OpgnGLRenderer
     * @param eFormat FormatType (NONE, RGB, RGBA, DEPTH)
     * @param eDepth DepthType (NONE, DEPTH_16, DEPTH_24, DEPTH_32)
     * @param eStencil StencilType (NONE, STENCIL_8)
     * @param eBuffering BufferingType (SINGLE, DOUBLE)
     * @param iWidth, the window width
     * @param iHeight, the window height
     */
    public OpenGLRenderer (
                    FrameBuffer.FormatType eFormat,
                    FrameBuffer.DepthType eDepth, FrameBuffer.StencilType eStencil,
                    FrameBuffer.BufferingType eBuffering,
                    FrameBuffer.MultisamplingType eMultisampling,
                    int iWidth,
                    int iHeight)
    {
        super(eFormat,eDepth,eStencil,eBuffering,eMultisampling,iWidth,iHeight);
        
        GLCapabilities kGlCapabilities = new GLCapabilities();
        kGlCapabilities.setHardwareAccelerated(true);
        m_kCanvas = new GLCanvas(kGlCapabilities);
    }


    /**
     * Sets the GLAutoDrawable object for accessing JOGL function calls.
     * @param drawable, the GLAutoDrawable object.
     */
    public void SetDrawable( GLAutoDrawable drawable )
    {
        m_kDrawable = drawable;
    }

    /**
     * Clears the GLAutoDrawable object.
     */
    public void ClearDrawable()
    {
        m_kDrawable = null;
    }

    /** Renderer-specific information for loading shader programs. */
    public String GetExtension () { return "ogl"; }

    /** Renderer-specific information for loading shader programs. */
    public char GetCommentCharacter () { return '#'; }

    /** Initialize the rendering state (vertices, colors, normals, textures,
     * vertex and pixel programs, etc.) */
    public void InitializeState ()
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        // vertices always exist
        gl.glEnableClientState(GL.GL_VERTEX_ARRAY);

        // colors disabled, current color is WHITE
        gl.glDisableClientState(GL.GL_COLOR_ARRAY);
        gl.glColor4fv(ColorRGBA.WHITE.GetData(), 0);

        // normals disabled
        gl.glDisableClientState(GL.GL_NORMAL_ARRAY);

        // Query for capabilities.

        // Get the number of supported texture images for vertex program.
        m_iMaxVShaderImages = 0;
        int[] aiParams = new int[1];
        gl.glGetIntegerv(GL.GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS,aiParams,0);
        m_iMaxVShaderImages = aiParams[0];

        // Get the number of supported texture units for pixel programs.
        m_iMaxPShaderImages = 0;
        gl.glGetIntegerv(GL.GL_MAX_TEXTURE_IMAGE_UNITS,aiParams,0);
        m_iMaxPShaderImages = aiParams[0];

        // TO DO.  The OpenGL shading language has separate concepts of "texture
        // image units" and "texture coordinate sets".  The values iMaxTextures
        // and iMaxTCoords can be different.  Replace m_iMaxTextures with
        // m_iMaxTextureImageUnits and add m_iMaxTCoords.  Anytime shader programs
        // are loaded, the number of texture coordinate inputs/outputs and the
        // number of samplers need to be compared with what the hardware can
        // support.
        m_iMaxTCoords = 0;
        gl.glGetIntegerv(GL.GL_MAX_TEXTURE_COORDS,aiParams,0);
        m_iMaxTCoords = aiParams[0];

        // OpenGL supports a primary and a secondary color
        m_iMaxColors = 2;

        // Set up light model.  TO DO:  Do we need the OpenGL calls for a
        // shader-based engine?
        gl.glGetIntegerv(GL.GL_MAX_LIGHTS,aiParams,0);
        m_iMaxLights = aiParams[0];
        assert(m_iMaxLights > 0);
        m_aspkLight = new Object[m_iMaxLights];

        gl.glLightModelfv(GL.GL_LIGHT_MODEL_AMBIENT,ColorRGBA.BLACK.GetData(),0);
        gl.glLightModeli(GL.GL_LIGHT_MODEL_LOCAL_VIEWER,GL.GL_FALSE);
        gl.glLightModeli(GL.GL_LIGHT_MODEL_TWO_SIDE,GL.GL_FALSE);
        gl.glLightModeli(GL.GL_LIGHT_MODEL_COLOR_CONTROL,GL.GL_SEPARATE_SPECULAR_COLOR);
        gl.glDisable(GL.GL_LIGHTING);

        // get stencil buffer size
        int iBits = 0;
        gl.glGetIntegerv(GL.GL_STENCIL_BITS,aiParams,0);
        iBits = aiParams[0];
        m_iMaxStencilIndices = (iBits > 0 ? (1 << iBits) : 0);

        gl.glGetIntegerv(GL.GL_MAX_CLIP_PLANES,aiParams,0);
        m_iMaxUserClipPlanes = aiParams[0];

        // Disable drawing of lines as sequences of dashes.
        gl.glDisable(GL.GL_LINE_STIPPLE);

        // Initialize global render state to default settings.
        new AlphaState();
        new CullState();
        new MaterialState();
        new PolygonOffsetState();
        new StencilState();
        new WireframeState();
        new ZBufferState();
        SetGlobalState(GlobalState.Default);
    }

    /**
     * Set the background color
     * @param rkColor, the new background color.
     */
    public void SetBackgroundColor (ColorRGBA rkColor)
    {
        super.SetBackgroundColor(rkColor);
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();
        gl.glClearColor(rkColor.R(),rkColor.G(),rkColor.B(),rkColor.A());
    }

    // Full window buffer operations.

    /** Clear back buffer. */
    public void ClearBackBuffer ()
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();
        gl.glEnable( GL.GL_SCISSOR_TEST );
        gl.glScissor( 0, 0, m_iWidth, m_iHeight );
        gl.glClear( GL.GL_COLOR_BUFFER_BIT );
        gl.glDisable( GL.GL_SCISSOR_TEST );
    }

    /** Clear depth buffer. */
    public void ClearZBuffer ()
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();
        gl.glEnable(GL.GL_SCISSOR_TEST);
        gl.glScissor(0,0,m_iWidth,m_iHeight);
        gl.glEnable(GL.GL_DEPTH_TEST);
        gl.glDepthMask(true);
        gl.glClear(GL.GL_DEPTH_BUFFER_BIT);
        gl.glDisable(GL.GL_SCISSOR_TEST);
    }

    /** Clear stencil buffer. */
    public void ClearStencilBuffer ()
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();
        gl.glEnable(GL.GL_SCISSOR_TEST);
        gl.glScissor(0,0,m_iWidth,m_iHeight);
        gl.glStencilMask(~0);
        gl.glClear(GL.GL_STENCIL_BUFFER_BIT);
        gl.glDisable(GL.GL_SCISSOR_TEST);
    }

    /** Clear all buffers. */
    public void ClearBuffers ()
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();
        gl.glEnable(GL.GL_SCISSOR_TEST);
        gl.glScissor(0,0,m_iWidth,m_iHeight);
        gl.glEnable(GL.GL_DEPTH_TEST);
        gl.glDepthMask(true);
        gl.glStencilMask(~0);
        gl.glClear(GL.GL_COLOR_BUFFER_BIT|GL.GL_DEPTH_BUFFER_BIT|GL.GL_STENCIL_BUFFER_BIT);
        gl.glDisable(GL.GL_SCISSOR_TEST);
    }

    /** Begin rendering the scene.
     * @return true.
     */
    public boolean BeginScene () { return true; }

    /** Display the back buffer. */
    public void DisplayBackBuffer ()
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        /* Call swapBuffers to render on-screen: */
        m_kDrawable.swapBuffers();
    }

    /** Clear the back buffer in the specified subwindow.
     * @param iXPos, the x-position for the subwindow
     * @param iYPos, the y-position for the subwindow
     * @param iWidth, the subwindow width
     * @param iHeight, the subwindow height
     */
    public void ClearBackBuffer (int iXPos, int iYPos,
                                 int iWidth,int iHeight)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();
        gl.glEnable(GL.GL_SCISSOR_TEST);
        gl.glScissor(iXPos,iYPos,iWidth,iHeight);
        gl.glClear(GL.GL_COLOR_BUFFER_BIT);
        gl.glDisable(GL.GL_SCISSOR_TEST);
    }

    /** Clear the depth buffer in the specified subwindow.
     * @param iXPos, the x-position for the subwindow
     * @param iYPos, the y-position for the subwindow
     * @param iWidth, the subwindow width
     * @param iHeight, the subwindow height
     */
    public void ClearZBuffer (int iXPos, int iYPos,
                              int iWidth,int iHeight)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();
        gl.glEnable(GL.GL_SCISSOR_TEST);
        gl.glScissor(iXPos,iYPos,iWidth,iHeight);
        gl.glEnable(GL.GL_DEPTH_TEST);
        gl.glDepthMask(true);
        gl.glClear(GL.GL_DEPTH_BUFFER_BIT);
        gl.glDisable(GL.GL_SCISSOR_TEST);
    }

    /** Clear the stencil buffer in the specified subwindow.
     * @param iXPos, the x-position for the subwindow
     * @param iYPos, the y-position for the subwindow
     * @param iWidth, the subwindow width
     * @param iHeight, the subwindow height
     */
    public void ClearStencilBuffer (int iXPos, int iYPos,
                                    int iWidth,int iHeight)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();
        gl.glEnable(GL.GL_SCISSOR_TEST);
        gl.glScissor(iXPos,iYPos,iWidth,iHeight);
        gl.glStencilMask(~0);
        gl.glClear(GL.GL_STENCIL_BUFFER_BIT);
        gl.glDisable(GL.GL_SCISSOR_TEST);
    }

    /** Clear the all buffers in the specified subwindow.
     * @param iXPos, the x-position for the subwindow
     * @param iYPos, the y-position for the subwindow
     * @param iWidth, the subwindow width
     * @param iHeight, the subwindow height
     */
    public void ClearBuffers (int iXPos, int iYPos,
                              int iWidth,int iHeight)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();
        gl.glEnable(GL.GL_SCISSOR_TEST);
        gl.glScissor(iXPos,iYPos,iWidth,iHeight);
        gl.glEnable(GL.GL_DEPTH_TEST);
        gl.glDepthMask(true);
        gl.glStencilMask(~0);
        gl.glClear(GL.GL_COLOR_BUFFER_BIT|GL.GL_DEPTH_BUFFER_BIT|GL.GL_STENCIL_BUFFER_BIT);
        gl.glDisable(GL.GL_SCISSOR_TEST);
    }

    // Specialized drawing functions.

    /** Select a font based on ID. 
     * @param iFontID, the font to use.
     * @return true if the font exists, false otherwise.
     */
    public boolean SelectFont (int iFontID)
    {
        if (0 <= iFontID && iFontID < (int)m_kDLInfo.size())
        {
            if (m_kDLInfo.get(iFontID).Quantity > 0)
            {
                m_iFontID = iFontID;
                return true;
            }
        }
        return false;
    }

    /** Clear a font based on ID. 
     * @param iFontID, the font to remove.
     */
    public void UnloadFont (int iFontID)
    {
        // You may not unload the default font (id = 0).
        if (iFontID <= 0)
        {
            return;
        }

        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        for (int i = 0; i < m_kDLInfo.size(); i++ )
        {
            DisplayListInfo kInfo = m_kDLInfo.get(i);
            gl.glDeleteLists(kInfo.Start,kInfo.Quantity);
            if (m_iFontID == iFontID)
            {
                m_iFontID = 0;
            }
        }
        m_kDLInfo.clear();
    }

    /** 
     * Draw text.
     * @param iX, the x-position for the start of the text being drawn.
     * @param iY, the y-position for the start of the text being drawn.
     * @param rkColor, text color
     * @param acText, text to draw
     */
    public void Draw (int iX, int iY, final ColorRGBA rkColor,
                      final char[] acText)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        assert(acText != null);

// #ifdef USE_TEXT_DISPLAY_LIST
//         int[] aiParam = new int[1];
//         int iListBase;
//         gl.glGetIntegerv(GL.GL_LIST_BASE,aiParam,0);
//         iListBase = aiParam[0];
// #endif

        // switch to orthogonal view
        gl.glMatrixMode(GL.GL_PROJECTION);
        gl.glPushMatrix();
        gl.glLoadIdentity();
        gl.glOrtho(-0.5f,m_iWidth-0.5f,-0.5f,m_iHeight-0.5f,-1.0f,1.0f);
        gl.glMatrixMode(GL.GL_MODELVIEW);
        gl.glPushMatrix();
        gl.glLoadIdentity();

        // disable depth, lighting, and texturing
        boolean bDepthTest = gl.glIsEnabled(GL.GL_DEPTH_TEST);
        boolean bLighting = gl.glIsEnabled(GL.GL_LIGHTING);
        boolean bTexture2D = gl.glIsEnabled(GL.GL_TEXTURE_2D);
        if (bDepthTest)
        {
            gl.glDisable(GL.GL_DEPTH_TEST);
        }
        if (bLighting)
        {
            gl.glDisable(GL.GL_LIGHTING);
        }
        if (bTexture2D)
        {
            gl.glDisable(GL.GL_TEXTURE_2D);
        }

        // set the text color
        //gl.glColor4fv(rkColor);
        gl.glColor4f(rkColor.R(),rkColor.G(),rkColor.B(),rkColor.A());

        // draw text string (use right-handed coordinates)
        gl.glRasterPos3i(iX,m_iHeight-1-iY,0);
// #ifdef USE_TEXT_DISPLAY_LIST
//         gl.glListBase(m_kDLInfo.get(m_iFontID).Base);
//         ByteBuffer kText = ByteBuffer.allocate(acText.length);
//         kText.put(acText);
//         gl.glCallLists(acText.length,GL.GL_UNSIGNED_BYTE,kText);
// #else
        int iLength = acText.length;
        for (int i = 0; i < iLength; i++)
        {
            DrawCharacter(g_kVerdana_S16B0I0,acText[i]);
        }
// #endif

        // restore depth, lighting, and texturing
        if (bDepthTest)
        {
            gl.glEnable(GL.GL_DEPTH_TEST);
        }
        if (bLighting)
        {
            gl.glEnable(GL.GL_LIGHTING);
        }
        if (bTexture2D)
        {
            gl.glEnable(GL.GL_TEXTURE_2D);
        }

        // restore matrices
        gl.glPopMatrix();
        gl.glMatrixMode(GL.GL_PROJECTION);
        gl.glPopMatrix();
        gl.glMatrixMode(GL.GL_MODELVIEW);

        // restore projective view
        OnFrustumChange();

// #ifdef USE_TEXT_DISPLAY_LIST
//         gl.glListBase(iListBase);
// #endif
    }

    /**
     * Draw a byte bitmap.
     * @param aucBuffer, the bitmap to draw.
     */
    public void Draw (final byte[] aucBuffer)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        if (aucBuffer == null)
        {
            return;
        }

        // disable other states that are not necessary for the buffer copy
        boolean bDepthTest = gl.glIsEnabled(GL.GL_DEPTH_TEST);
        boolean bLighting = gl.glIsEnabled(GL.GL_LIGHTING);
        boolean bTexture2D = gl.glIsEnabled(GL.GL_TEXTURE_2D);
        if (bDepthTest)
        {
            gl.glDisable(GL.GL_DEPTH_TEST);
        }
        if (bLighting)
        {
            gl.glDisable(GL.GL_LIGHTING);
        }
        if (bTexture2D)
        {
            gl.glDisable(GL.GL_TEXTURE_2D);
        }

        // Set raster position to window coord (0,H-1).  The hack here avoids
        // problems with invalid raster positions which would cause glDrawPixels
        // not to execute.  OpenGL uses right-handed screen coordinates, so using
        // (0,H-1) as the raster position followed by glPixelZoom(1,-1) tells
        // OpenGL to draw the screen in left-handed coordinates starting at the
        // top row of the screen and finishing at the bottom row.
        gl.glMatrixMode(GL.GL_MODELVIEW);
        gl.glPushMatrix();
        gl.glLoadIdentity();
        gl.glMatrixMode(GL.GL_PROJECTION);
        gl.glPushMatrix();
        gl.glLoadIdentity();
        gl.glOrtho(0.0,(double)m_iWidth,0.0,(double)m_iHeight,0.0,1.0);
        gl.glRasterPos3f(0.0f,0.0f,0.0f);
        byte[] aucBitmap = new byte[]{0};
        gl.glBitmap(0,0,0.0f,0.0f,0.0f,(float)m_iHeight,aucBitmap,0);
        gl.glPopMatrix();
        gl.glMatrixMode(GL.GL_MODELVIEW);
        gl.glPopMatrix();

        gl.glPixelZoom(1.0f,-1.0f);
        ByteBuffer kBuffer = ByteBuffer.allocate(1);
        kBuffer.put(aucBitmap);
        kBuffer.rewind();
        gl.glDrawPixels(m_iWidth,m_iHeight,GL.GL_BGR,GL.GL_UNSIGNED_BYTE,kBuffer);
        gl.glPixelZoom(1.0f,1.0f);

        // reenable states
        if (bDepthTest)
        {
            gl.glEnable(GL.GL_DEPTH_TEST);
        }
        if (bLighting)
        {
            gl.glEnable(GL.GL_LIGHTING);
        }
        if (bTexture2D)
        {
            gl.glEnable(GL.GL_TEXTURE_2D);
        }
    }

    // point size, line width, and line stipple

    /** Set the point size. 
     * @param fSize, new point size.
     */
    public void SetPointSize (float fSize)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        super.SetPointSize(fSize);
        gl.glPointSize(m_fPointSize);
    }

    /** Set the line width. 
     * @param fWidth, new line width.
     */
    public void SetLineWidth (float fWidth)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        super.SetLineWidth(fWidth);
        gl.glLineWidth(m_fLineWidth);
    }

    /** Set the line stipple. 
     * @param iRepeat, the repeat factor
     * @param usPattern, the stipple pattern.
     */
    public void SetLineStipple (int iRepeat, short usPattern)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        super.SetLineStipple(iRepeat,usPattern);
        if ((m_iLineStippleRepeat != 0) && (m_usLineStipplePattern != 0))
        {
            gl.glEnable(GL.GL_LINE_STIPPLE);
            gl.glLineStipple(m_iLineStippleRepeat,m_usLineStipplePattern);
        }
        else
        {
            gl.glDisable(GL.GL_LINE_STIPPLE);
        }
    }

    /** Enable or disable which color channels will be written to the color
     * buffer.
     * @param bAllowRed, when true allow the red channel to be written
     * @param bAllowGreen, when true allow the green channel to be written
     * @param bAllowBlue, when true allow the blue channel to be written
     * @param bAllowAlpha, when true allow the alpha channel to be written
     */
    public void SetColorMask (boolean bAllowRed, boolean bAllowGreen,
                              boolean bAllowBlue, boolean bAllowAlpha)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        super.SetColorMask(bAllowRed,bAllowGreen,bAllowBlue,bAllowAlpha);

        gl.glColorMask(bAllowRed,bAllowGreen,
                       bAllowBlue,bAllowAlpha);
    }


    /** Include additional clip planes.  The input plane must be in model
     * coordinates.  It is transformed internally to camera coordinates to
     * support clipping in clip space.
     * @param i, the GL_CLIP_PLANE0 (+i) clip plane to include.
     * @param rkPlane, the clip plane definition.
     */
    public void EnableUserClipPlane (int i, Plane3f rkPlane)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        double adPlane[] = new double[]
            {
                (double)rkPlane.Normal.X(),
                (double)rkPlane.Normal.Y(),
                (double)rkPlane.Normal.Z(),
                (double)-rkPlane.Constant
            };
        gl.glClipPlane(GL.GL_CLIP_PLANE0 + i,adPlane,0);
        gl.glEnable(GL.GL_CLIP_PLANE0 + i);
    }

    /** Disables additional clip planes.
     * @param i, the clip plane to disable (GL_CLIP_PLANE0 + i).
     */
    public void DisableUserClipPlane (int i)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        gl.glDisable(GL.GL_CLIP_PLANE0 + i);
    }

    // camera updates

    /** Called when the viewport changes. Updates the camera. */
    public void OnViewportChange ()
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        float fPortL, fPortR, fPortT, fPortB;
        if (m_pkCamera != null)
        {
            // 3D applications use cameras.
            fPortL = m_pkCamera.GetPortL();
            fPortR = m_pkCamera.GetPortR();
            fPortT = m_pkCamera.GetPortT();
            fPortB = m_pkCamera.GetPortB();
        }
        else
        {
            // 2D applications do not use cameras.
            fPortL = 0.0f;
            fPortR = 1.0f;
            fPortT = 1.0f;
            fPortB = 0.0f;
        }

        // OpenGL defines the full-sized viewport to have origin at the lower
        // left corner of the screen.  The fB value specifies the relative
        // distance from the bottom of the screen.  Wild Magic also uses this
        // convention, but DirectX does not (in which case iY is computed
        // differently for DirectX).
        int iX = (int)(fPortL*m_iWidth);
        int iY = (int)(fPortB*m_iHeight);  // See note above.
        int iW = (int)((fPortR - fPortL)*m_iWidth);
        int iH = (int)((fPortT - fPortB)*m_iHeight);
        gl.glViewport(iX,iY,iW,iH);
    }

    /** Called when the depth range changes. Updates the camera. */
    public void OnDepthRangeChange ()
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        if (m_pkCamera != null)
        {
            float fPortN, fPortF;
            fPortN = m_pkCamera.GetPortN();
            fPortF = m_pkCamera.GetPortF();
            gl.glDepthRange((double)fPortN,(double)fPortF);
        }
        else
        {
            gl.glDepthRange(0.0,1.0);
        }
    }

    // global render state management

    /** Sets the AlphaState for the OpenGLRenderer.
     * @param pkState, the new AlphaState.
     */
    public void SetAlphaState (AlphaState pkState)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        super.SetAlphaState(pkState);

        if (pkState.BlendEnabled)
        {
            gl.glEnable(GL.GL_BLEND);
            gl.glBlendFunc(ms_aeAlphaSrcBlend[pkState.SrcBlend.Value()],
                           ms_aeAlphaDstBlend[pkState.DstBlend.Value()]);
        }
        else
        {
            gl.glDisable(GL.GL_BLEND);
        }

        if (pkState.TestEnabled)
        {
            gl.glEnable(GL.GL_ALPHA_TEST);
            gl.glAlphaFunc(ms_aeAlphaTest[pkState.Test.Value()],pkState.Reference);
        }
        else
        {
            gl.glDisable(GL.GL_ALPHA_TEST);
        }

        gl.glBlendColor(pkState.ConstantColor.R(),pkState.ConstantColor.G(),
                        pkState.ConstantColor.G(),pkState.ConstantColor.A());
    }

    /** Sets the CullState for the OpenGLRenderer.
     * @param pkState, the new CullState.
     */
    public void SetCullState (CullState pkState)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        super.SetCullState(pkState);

        if (pkState.Enabled)
        {
            gl.glEnable(GL.GL_CULL_FACE);
        }
        else
        {
            gl.glDisable(GL.GL_CULL_FACE);
        }

        gl.glFrontFace(ms_aeFrontFace[pkState.FrontFace.Value()]);

        if (!m_bReverseCullFace)
        {
            gl.glCullFace(ms_aeCullFace[pkState.CullFace.Value()]);
        }
        else
        {
            if (ms_aeCullFace[pkState.CullFace.Value()] == GL.GL_BACK)
            {
                gl.glCullFace(GL.GL_FRONT);
            }
            else
            {
                gl.glCullFace(GL.GL_BACK);
            }
        }
    }

    /** Sets the PolygonOffsetState for the OpenGLRenderer.
     * @param pkState, the new PolygonOffsetState.
     */
    public void SetPolygonOffsetState (PolygonOffsetState pkState)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        super.SetPolygonOffsetState(pkState);

        if (pkState.FillEnabled)
        {
            gl.glEnable(GL.GL_POLYGON_OFFSET_FILL);
        }
        else
        {
            gl.glDisable(GL.GL_POLYGON_OFFSET_FILL);
        }

        if (pkState.LineEnabled)
        {
            gl.glEnable(GL.GL_POLYGON_OFFSET_LINE);
        }
        else
        {
            gl.glDisable(GL.GL_POLYGON_OFFSET_LINE);
        }

        if (pkState.PointEnabled)
        {
            gl.glEnable(GL.GL_POLYGON_OFFSET_POINT);
        }
        else
        {
            gl.glDisable(GL.GL_POLYGON_OFFSET_POINT);
        }

        gl.glPolygonOffset(pkState.Scale,pkState.Bias);
    }

    /** Sets the StencilState for the OpenGLRenderer.
     * @param pkState, the new StencilState.
     */
    public void SetStencilState (StencilState pkState)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        super.SetStencilState(pkState);

        if (pkState.Enabled)
        {
            gl.glEnable(GL.GL_STENCIL_TEST);

            gl.glStencilFunc(ms_aeStencilCompare[pkState.Compare.Value()],
                             (int)pkState.Reference,pkState.Mask);

            gl.glStencilMask(pkState.WriteMask);

            gl.glStencilOp(ms_aeStencilOperation[pkState.OnFail.Value()],
                        ms_aeStencilOperation[pkState.OnZFail.Value()],
                        ms_aeStencilOperation[pkState.OnZPass.Value()]);
        }
        else
        {
            gl.glDisable(GL.GL_STENCIL_TEST);
        }
    }

    /** Sets the WireframeState for the OpenGLRenderer.
     * @param pkState, the new WireframeState.
     */
    public void SetWireframeState (WireframeState pkState)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        super.SetWireframeState(pkState);

        if (pkState.Enabled)
        {
            gl.glPolygonMode(GL.GL_FRONT_AND_BACK,GL.GL_LINE);
        }
        else
        {
            gl.glPolygonMode(GL.GL_FRONT_AND_BACK,GL.GL_FILL);
        }
    }

    /** Sets the ZBufferState for the OpenGLRenderer.
     * @param pkState, the new ZBufferState.
     */
    public void SetZBufferState (ZBufferState pkState)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        super.SetZBufferState(pkState);

        if (pkState.Enabled)
        {
            gl.glEnable(GL.GL_DEPTH_TEST);
            gl.glDepthFunc(ms_aeZBufferCompare[pkState.Compare.Value()]);
        }
        else
        {
            gl.glDisable(GL.GL_DEPTH_TEST);
            gl.glDepthFunc(GL.GL_ALWAYS);
        }

        if (pkState.Writable)
        {
            gl.glDepthMask(true);
        }
        else
        {
            gl.glDepthMask(false);
        }
    }


    /** The entry point to drawing a geometry object. */
    public void DrawElements ()
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        IndexBuffer pkIBuffer = m_pkGeometry.IBuffer;
        assert(pkIBuffer != null);
        int eType = ms_aeObjectType[m_pkGeometry.Type.Value()];
        int iOffset = pkIBuffer.GetOffset();
        gl.glDrawElements(eType,pkIBuffer.GetIndexQuantity(),GL.GL_UNSIGNED_INT,iOffset);
    }


    /** Resource loading and releasing (to/from video memory).
     * @param pkVProgram the vertex program to generate/bind
     * @return the new ResourceIdentifier for the VertexProgram
     */
    public ResourceIdentifier OnLoadVProgram (VertexProgram pkVProgram)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return null; }
        GL gl = m_kDrawable.getGL();

        // Generate binding information and compile the program.
        VProgramID pkResource = new VProgramID();
        ResourceIdentifier rpkID = pkResource;

        String acProgramText = pkVProgram.GetProgramText();
        //System.err.println( acProgramText );
        int iProgramLength = acProgramText.length();

        gl.glEnable(GL.GL_VERTEX_PROGRAM_ARB);
        int[] aiParams = new int[1];
        gl.glGenProgramsARB(1,aiParams,0);
        pkResource.ID = aiParams[0];
        gl.glBindProgramARB(GL.GL_VERTEX_PROGRAM_ARB,pkResource.ID);
        gl.glProgramStringARB(GL.GL_VERTEX_PROGRAM_ARB,GL.GL_PROGRAM_FORMAT_ASCII_ARB,
                              iProgramLength,acProgramText);
        gl.glDisable(GL.GL_VERTEX_PROGRAM_ARB);
        return rpkID;
    }

    /**
     * Release the VertexProgram described in the ResourceIdentifier parameter.
     * @param pkID, the ResourceIdentifier with the VertexProgram to release.
     */
    public void OnReleaseVProgram (ResourceIdentifier pkID)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        boolean bIsActive = IsActive();
        if (!bIsActive)
        {
            Activate();
        }

        VProgramID pkResource = (VProgramID)pkID;
        int[] aiParams = new int[1];
        aiParams[0] = pkResource.ID;
        gl.glDeleteProgramsARB(1,aiParams,0);
        pkResource.ID = aiParams[0];
        pkResource = null;
    }

    /** Resource loading and releasing (to/from video memory).
     * @param pkPProgram the pixel program to generate/bind
     * @return the new ResourceIdentifier for the PixelProgram
     */
    public ResourceIdentifier OnLoadPProgram (PixelProgram pkPProgram)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return null; }
        GL gl = m_kDrawable.getGL();

        // Generate binding information and compile the shader.
        PProgramID pkResource = new PProgramID();
        ResourceIdentifier rpkID = pkResource;

        String acProgramText = pkPProgram.GetProgramText();
        //System.err.println( acProgramText );
        int iProgramLength = acProgramText.length();

        gl.glEnable(GL.GL_FRAGMENT_PROGRAM_ARB);
        int[] aiParams = new int[1];
        gl.glGenProgramsARB(1,aiParams,0);
        pkResource.ID = aiParams[0];
        gl.glBindProgramARB(GL.GL_FRAGMENT_PROGRAM_ARB,pkResource.ID);
        gl.glProgramStringARB(GL.GL_FRAGMENT_PROGRAM_ARB,GL.GL_PROGRAM_FORMAT_ASCII_ARB,
                           iProgramLength,acProgramText);
        gl.glDisable(GL.GL_FRAGMENT_PROGRAM_ARB);

        return rpkID;
    }

    /**
     * Release the PixelProgram described in the ResourceIdentifier parameter.
     * @param pkID, the ResourceIdentifier with the PixelProgram to release.
     */
    public void OnReleasePProgram (ResourceIdentifier pkID)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        boolean bIsActive = IsActive();
        if (!bIsActive)
        {
            Activate();
        }

        PProgramID pkResource = (PProgramID)pkID;
        int[] aiParams = new int[1];
        aiParams[0] = pkResource.ID;
        gl.glDeleteProgramsARB(1,aiParams,0);
        pkResource.ID = aiParams[0];
        pkResource = null;
    }

    /** Resource loading and releasing (to/from video memory).
     * @param pkTexture the Texture to generate/bind
     * @return the new ResourceIdentifier for the Texture
     */
    public ResourceIdentifier OnLoadTexture (Texture pkTexture)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return null; }
        GL gl = m_kDrawable.getGL();

        // Activate the texture unit in hardware that will manage this texture.
        TextureID pkResource = new TextureID();
        pkResource.TextureObject = pkTexture;
        ResourceIdentifier rpkID = pkResource;

        SamplerInformation pkSI = pkTexture.GetSamplerInformation();
        SamplerInformation.Type eSType = pkSI.GetType();
        int eTarget = ms_aeSamplerTypes[eSType.Value()];

        // Get the texture image and its information.
        final Image pkImage = pkTexture.GetImage();
        assert(pkImage != null);
        ByteBuffer aucData = pkImage.GetDataBuffer();
        int iComponent = ms_aeImageComponents[pkImage.GetFormat().Value()];
        int eFormat = ms_aeImageFormats[pkImage.GetFormat().Value()];
        int eIType = ms_aeImageTypes[pkImage.GetFormat().Value()];

        // Generate the name and binding information.
        int[] aiParams = new int[1];
        gl.glGenTextures((int)1,aiParams,0);
        pkResource.ID = aiParams[0];
        gl.glBindTexture(eTarget,pkResource.ID);

        // Set the filter mode.
        Texture.FilterType eFType = pkTexture.GetFilterType();
        if (eFType == Texture.FilterType.NEAREST)
        {
            gl.glTexParameteri(eTarget,GL.GL_TEXTURE_MAG_FILTER,GL.GL_NEAREST);
        }
        else
        {
            // TO DO.  Support anisotropic filtering.  To query for the maximum
            // allowed anisotropy, use
            //   float fMax;
            //   glGetFloatv(GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT,&fMax);
            // To enable filtering, use
            //   glTexParameterf(eTarget,GL_TEXTURE_MAX_ANISOTROPY_EXT,fValue);
            // where 1 <= fValue <= fMax.

            gl.glTexParameteri(eTarget,GL.GL_TEXTURE_MAG_FILTER,GL.GL_LINEAR);
        }

        // Set the mipmap mode.
        gl.glTexParameteri(eTarget,GL.GL_TEXTURE_MIN_FILTER,
                        ms_aeTextureMipmap[eFType.Value()]);

        // Set the border color (for clamp to border).
        float[] afBorderColor = pkTexture.GetBorderColor().GetData();
        gl.glTexParameterfv( eTarget,GL.GL_TEXTURE_BORDER_COLOR,
                             afBorderColor, 0 );

        // Copy the image data from system memory to video memory.
        boolean bNoMip =
            (eFType == Texture.FilterType.NEAREST ||
             eFType == Texture.FilterType.LINEAR);

        boolean bDepth = pkTexture.IsDepthTexture();

        if ( eSType == SamplerInformation.Type.SAMPLER_1D )
        {
            if (bNoMip)
            {
                gl.glTexImage1D(eTarget,0,iComponent,pkImage.GetBound(0),0,
                             eFormat,eIType,aucData);
            }
            else
            {
                GLU kGLU = new GLU();
                kGLU.gluBuild1DMipmaps(eTarget,iComponent,pkImage.GetBound(0),
                                       eFormat,eIType,aucData);
            }

            gl.glTexParameteri(eTarget,GL.GL_TEXTURE_WRAP_S,
                            ms_aeWrapMode[pkTexture.GetWrapType(0).Value()]);
        }
        else if ( eSType == SamplerInformation.Type.SAMPLER_2D ||
                  eSType == SamplerInformation.Type.SAMPLER_PROJ  )
        {
            if (!bDepth)
            {
                if (bNoMip)
                {
                    gl.glTexImage2D(eTarget,0,iComponent,pkImage.GetBound(0),
                                    pkImage.GetBound(1),0,eFormat,eIType,aucData);
                }
                else
                {
                    GLU kGLU = new GLU();
                    kGLU.gluBuild2DMipmaps(eTarget,iComponent,pkImage.GetBound(0),
                                      pkImage.GetBound(1),eFormat,eIType,aucData);
                }
            }
            else
            {
                gl.glTexImage2D(eTarget,0,iComponent,pkImage.GetBound(0),
                                pkImage.GetBound(1),0,eFormat,eIType,0);

                // set up depth comparison
                gl.glTexParameteri(eTarget,GL.GL_TEXTURE_COMPARE_MODE,
                                GL.GL_COMPARE_R_TO_TEXTURE);
                gl.glTexParameteri(eTarget,GL.GL_TEXTURE_COMPARE_FUNC,
                                ms_aeDepthCompare[pkTexture.GetDepthCompare().Value()]);
            }

            gl.glTexParameteri(eTarget,GL.GL_TEXTURE_WRAP_S,
                            ms_aeWrapMode[pkTexture.GetWrapType(0).Value()]);
            gl.glTexParameteri(eTarget,GL.GL_TEXTURE_WRAP_T,
                            ms_aeWrapMode[pkTexture.GetWrapType(1).Value()]);
        }
        else if ( eSType ==  SamplerInformation.Type.SAMPLER_3D )
        {
            // TO DO.  Microsoft's GLU library does not implement the function
            // gluBuild3DMipmaps.  DirectX9 SDK does not support automatic
            // generation of mipmaps for volume textures.  For now, do not
            // support mipmaps of 3D textures.  However, manually generated
            // mipmaps can be added later.  The LibGraphics Sampler classes
            // already implement this for software rendering.
            gl.glTexImage3D(eTarget,0,iComponent,pkImage.GetBound(0),
                            pkImage.GetBound(1),pkImage.GetBound(2),0,eFormat,eIType,
                            aucData);

            gl.glTexParameteri(eTarget,GL.GL_TEXTURE_WRAP_S,
                            ms_aeWrapMode[pkTexture.GetWrapType(0).Value()]);
            gl.glTexParameteri(eTarget,GL.GL_TEXTURE_WRAP_T,
                            ms_aeWrapMode[pkTexture.GetWrapType(1).Value()]);
            gl.glTexParameteri(eTarget,GL.GL_TEXTURE_WRAP_R,
                            ms_aeWrapMode[pkTexture.GetWrapType(2).Value()]);
        }
//         case SamplerInformation.Type.SAMPLER_CUBE.Value():
//             {
//                 // A cube map image has 6 subimages (+x,-x,+y,-y,+z,-z).
//                 assert(pkImage.IsCubeImage());
//                 int i, iIncrement = pkImage.GetBytesPerPixel() *
//                     pkImage.GetQuantity();

//                 if (bNoMip)
//                 {
//                     for (i = 0; i < 6; i++, aucData += iIncrement)
//                     {
//                         gl.glTexImage2D(GL.GL_TEXTURE_CUBE_MAP_POSITIVE_X+i,0,
//                                         iComponent,pkImage.GetBound(0),
//                                         pkImage.GetBound(1),
//                                         0,eFormat,eIType,aucData);
//                     }
//                 }
//                 else
//                 {
// //                     for (i = 0; i < 6; i++, aucData += iIncrement)
// //                     {
// //                         gluBuild2DMipmaps(GL.GL_TEXTURE_CUBE_MAP_POSITIVE_X+i,
// //                                           iComponent,pkImage.GetBound(0),
// //                                           pkImage.GetBound(1),
// //                                           eFormat,eIType,aucData);
// //                     }
//                 }
//                 break;
//             }
        else
        {
            assert(false);
        }
        aucData.clear();
        aucData = null;
        return rpkID;
    }

    /**
     * Release the Texture described in the ResourceIdentifier parameter.
     * @param pkID, the ResourceIdentifier with the Texture to release.
     */
    public void OnReleaseTexture (ResourceIdentifier pkID)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        boolean bIsActive = IsActive();
        if (!bIsActive)
        {
            Activate();
        }

        TextureID pkResource = (TextureID)pkID;
        int[] aiParams = new int[1];
        aiParams[0] = pkResource.ID;
        gl.glDeleteTextures((int)1,aiParams,0);
        pkResource.ID = aiParams[0];
        pkResource = null;
    }

    /** Resource loading and releasing (to/from video memory).
     * @param rkIAtr The Input Attributes for the VertexBuffer
     * @param rkOAtr The Output Attributes for the VertexBuffer
     * @param pkVBuffer The VertexBuffer to generate/enable
     * @return the new ResourceIdentifier for the VertexBuffer
     */
    public ResourceIdentifier OnLoadVBuffer (final Attributes rkIAttr, final Attributes rkOAttr,
                                             VertexBuffer pkVBuffer)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return null; }
        GL gl = m_kDrawable.getGL();

        VBufferID pkResource = new VBufferID();
        ResourceIdentifier rpkID = pkResource;
        pkResource.IAttr = rkIAttr;
        pkResource.OAttr = rkOAttr;

        float[] afCompatible = pkVBuffer.BuildCompatibleArray(rkIAttr,false);
        int iChannels = afCompatible.length;

        // Generate the name and binding information.
        int[] aiParams = new int[1];
        gl.glGenBuffers(1,aiParams,0);
        pkResource.ID = aiParams[0];
        gl.glBindBuffer(GL.GL_ARRAY_BUFFER,pkResource.ID);

        // Copy the vertex buffer data from system memory to video memory.
        FloatBuffer akData = FloatBuffer.wrap(afCompatible);
        akData.rewind();
        gl.glBufferData(GL.GL_ARRAY_BUFFER,iChannels*BufferUtil.SIZEOF_FLOAT,akData,
                        GL.GL_STATIC_DRAW);
        return rpkID;
    }

    /**
     * Release the VertexBuffer described in the ResourceIdentifier parameter.
     * @param pkID, the ResourceIdentifier with the VertexBuffer to release.
     */
    public void OnReleaseVBuffer (ResourceIdentifier pkID)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        boolean bIsActive = IsActive();
        if (!bIsActive)
        {
            Activate();
        }

        VBufferID pkResource = (VBufferID)pkID;
        int[] aiParams = new int[1];
        aiParams[0] = pkResource.ID;
        gl.glDeleteBuffers(1,aiParams,0);
        pkResource.ID = aiParams[0];
        pkResource = null;
    }

    /** Resource loading and releasing (to/from video memory).
     * @param pkIBuffer the IndexBuffer to generate/enable
     * @return the new ResourceIdentifier for the IndexBuffer
     */
    public ResourceIdentifier OnLoadIBuffer (IndexBuffer pkIBuffer)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return null; }
        GL gl = m_kDrawable.getGL();

        IBufferID pkResource = new IBufferID();
        ResourceIdentifier rpkID = pkResource;

        int[] aiParams = new int[1];
        gl.glGenBuffers(1,aiParams,0);
        pkResource.ID = aiParams[0];
        gl.glBindBuffer(GL.GL_ELEMENT_ARRAY_BUFFER,pkResource.ID);

        // Copy the index buffer data from system memory to video memory.
        IntBuffer akData = IntBuffer.wrap(pkIBuffer.GetData());
        akData.rewind();
        gl.glBufferData(GL.GL_ELEMENT_ARRAY_BUFFER,
                        pkIBuffer.GetIndexQuantity()*BufferUtil.SIZEOF_INT,akData,
                        GL.GL_STATIC_DRAW);
        return rpkID;
    }

    /**
     * Release the IndexBuffer described in the ResourceIdentifier parameter.
     * @param pkID, the ResourceIdentifier with the IndexBuffer to release.
     */
    public void OnReleaseIBuffer (ResourceIdentifier pkID)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        boolean bIsActive = IsActive();
        if (!bIsActive)
        {
            Activate();
        }

        IBufferID pkResource = (IBufferID)pkID;
        int[] aiParams = new int[1];
        aiParams[0] = pkResource.ID;
        gl.glDeleteBuffers(1,aiParams,0);
        pkResource.ID = aiParams[0];
        pkResource = null;
    }


    /** Resource enabling and disabling.
     * Sets the values for the Vertex Program constant parameters.
     * @param eCTYpe, the ConstantType parameter (RENDERER, NUMERICAL, USER)
     * @param iBaseRegister, the register to load the parameter values into
     * @param iRegisterQuantity, the number of registers
     * @param afData, the parameter values.
     */
    public void SetVProgramConstant (Renderer.ConstantType eCType,
                                     int iBaseRegister,
                                     int iRegisterQuantity,
                                     float[] afData)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        float[] afParam = new float[4];
        if (eCType != Renderer.ConstantType.CT_NUMERICAL)
        {
            for (int j = 0; j < iRegisterQuantity; j++)
            {
                afParam[0] = afData[ j * 4 + 0];
                afParam[1] = afData[ j * 4 + 1];
                afParam[2] = afData[ j * 4 + 2];
                afParam[3] = afData[ j * 4 + 3];
                gl.glProgramLocalParameter4fARB(GL.GL_VERTEX_PROGRAM_ARB,
                                                 iBaseRegister,
                                                 afParam[0], afParam[1], afParam[2], afParam[3] );
                iBaseRegister++;
            }
        }
    }

    /** Resource enabling and disabling.
     * Sets the values for the Pixel Program constant parameters.
     * @param eCTYpe, the ConstantType parameter (RENDERER, NUMERICAL, USER)
     * @param iBaseRegister, the register to load the parameter values into
     * @param iRegisterQuantity, the number of registers
     * @param afData, the parameter values.
     */
    public void SetPProgramConstant ( Renderer.ConstantType eCType,
                                      int iBaseRegister,
                                      int iRegisterQuantity,
                                      float[] afData)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        float[] afParam = new float[4];
        if (eCType != Renderer.ConstantType.CT_NUMERICAL)
        {
            for (int j = 0; j < iRegisterQuantity; j++)
            {
                afParam[0] = afData[ j * 4 + 0];
                afParam[1] = afData[ j * 4 + 1];
                afParam[2] = afData[ j * 4 + 2];
                afParam[3] = afData[ j * 4 + 3];
                gl.glProgramLocalParameter4fARB(GL.GL_FRAGMENT_PROGRAM_ARB,
                                                 iBaseRegister,
                                                 afParam[0], afParam[1], afParam[2], afParam[3] );
                iBaseRegister++;
            }
        }
    }

    /**
     * Enable the VertexProgram spefified by the ResourceIdentifer parameter pkID.
     * @param pkID the ResourceIdentifier describing the VertexProgram to enable.
     */
    public void OnEnableVProgram (ResourceIdentifier pkID)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        VProgramID pkResource = (VProgramID)pkID;
        gl.glEnable(GL.GL_VERTEX_PROGRAM_ARB);
        gl.glBindProgramARB(GL.GL_VERTEX_PROGRAM_ARB,pkResource.ID);
    }

    /**
     * Disable the VertexProgram spefified by the ResourceIdentifer parameter pkID.
     * @param pkID the ResourceIdentifier describing the VertexProgram to disable.
     */
    public void OnDisableVProgram (ResourceIdentifier pkID)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        gl.glDisable(GL.GL_VERTEX_PROGRAM_ARB);
    }

    /**
     * Enable the PixelProgram spefified by the ResourceIdentifer parameter pkID.
     * @param pkID the ResourceIdentifier describing the PixelProgram to enable.
     */
    public void OnEnablePProgram (ResourceIdentifier pkID)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        PProgramID pkResource = (PProgramID)pkID;
        gl.glEnable(GL.GL_FRAGMENT_PROGRAM_ARB);
        gl.glBindProgramARB(GL.GL_FRAGMENT_PROGRAM_ARB,pkResource.ID);
    }

    /**
     * Disable the PixelProgram spefified by the ResourceIdentifer parameter pkID.
     * @param pkID the ResourceIdentifier describing the PixelProgram to disable.
     */
    public void OnDisablePProgram (ResourceIdentifier pkID)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        gl.glDisable(GL.GL_FRAGMENT_PROGRAM_ARB);
    }

    /**
     * Enable the Texture spefified by the ResourceIdentifer parameter pkID.
     * @param pkID the ResourceIdentifier describing the Texture to enable.
     */
    public void OnEnableTexture (ResourceIdentifier pkID)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        TextureID pkResource = (TextureID)pkID;
        Texture pkTexture = pkResource.TextureObject;

        SamplerInformation pkSI = pkTexture.GetSamplerInformation();
        SamplerInformation.Type eSType = pkSI.GetType();
        int iTextureUnit = pkSI.GetTextureUnit();
        int eTarget = ms_aeSamplerTypes[eSType.Value()];

        gl.glClientActiveTexture(GL.GL_TEXTURE0 + iTextureUnit);
        gl.glActiveTexture(GL.GL_TEXTURE0 + iTextureUnit);
        gl.glBindTexture(eTarget,pkResource.ID);
    }

    /**
     * Disable the Texture spefified by the ResourceIdentifer parameter pkID.
     * @param pkID the ResourceIdentifier describing the Texture to disable.
     */
    public void OnDisableTexture (ResourceIdentifier pkID)
    {
        // Nothing to do.
    }

    /**
     * Enable the VertexBuffer spefified by the ResourceIdentifer parameter pkID.
     * @param pkID the ResourceIdentifier describing the VertexBuffer to enable.
     */
    public void OnEnableVBuffer (ResourceIdentifier pkID)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        // Bind the current vertex buffer.
        VBufferID pkResource = (VBufferID)pkID;
        gl.glBindBuffer(GL.GL_ARRAY_BUFFER,pkResource.ID);

        final Attributes rkRAttr = pkResource.IAttr;
        int iSize = BufferUtil.SIZEOF_FLOAT*rkRAttr.GetChannelQuantity();
        if (rkRAttr.HasPosition())
        {
            gl.glEnableClientState(GL.GL_VERTEX_ARRAY);
            gl.glVertexPointer(rkRAttr.GetPChannels(),GL.GL_FLOAT,iSize,
                               rkRAttr.GetPOffset());
        }

        if (rkRAttr.HasNormal())
        {
            // OpenGL appears not to allow you to specify the number of
            // components for the normal vectors.  Thus, rkAttr.GetNChannels()
            // should be 3.
            gl.glEnableClientState(GL.GL_NORMAL_ARRAY);
            gl.glNormalPointer(GL.GL_FLOAT,iSize,
                               BufferUtil.SIZEOF_FLOAT*rkRAttr.GetNOffset());
        }

        if (rkRAttr.HasColor(0))
        {
            gl.glEnableClientState(GL.GL_COLOR_ARRAY);
            gl.glColorPointer(rkRAttr.GetCChannels(0),GL.GL_FLOAT,iSize,
                              BufferUtil.SIZEOF_FLOAT*rkRAttr.GetCOffset(0));
        }

        if (rkRAttr.HasColor(1))
        {
            gl.glEnableClientState(GL.GL_SECONDARY_COLOR_ARRAY);
            gl.glSecondaryColorPointer(rkRAttr.GetCChannels(1),GL.GL_FLOAT,iSize,
                                       BufferUtil.SIZEOF_FLOAT*rkRAttr.GetCOffset(1));
        }

        for (int iUnit = 0; iUnit < rkRAttr.GetMaxTCoords(); iUnit++)
        {
            if (rkRAttr.HasTCoord(iUnit))
            {
                gl.glClientActiveTexture(GL.GL_TEXTURE0 + iUnit);
                gl.glActiveTexture(GL.GL_TEXTURE0 + iUnit);
                gl.glEnableClientState(GL.GL_TEXTURE_COORD_ARRAY); 
                gl.glTexCoordPointer(rkRAttr.GetTChannels(iUnit),GL.GL_FLOAT,iSize,
                                     BufferUtil.SIZEOF_FLOAT*rkRAttr.GetTOffset(iUnit));
            }
        }
    }

    /**
     * Disable the VertexBuffer spefified by the ResourceIdentifer parameter pkID.
     * @param pkID the ResourceIdentifier describing the VertexBuffer to disable.
     */
    public void OnDisableVBuffer (ResourceIdentifier pkID)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        VBufferID pkResource = (VBufferID)pkID;
        final Attributes rkRAttr = pkResource.IAttr;

        // Unbind the current vertex buffer.
        gl.glBindBuffer(GL.GL_ARRAY_BUFFER,0);

        if (rkRAttr.HasPosition())
        {
            gl.glDisableClientState(GL.GL_VERTEX_ARRAY);
        }

        if (rkRAttr.HasNormal())
        {
            gl.glDisableClientState(GL.GL_NORMAL_ARRAY);
        }

        if (rkRAttr.HasColor(0))
        {
            gl.glDisableClientState(GL.GL_COLOR_ARRAY);
        }

        if (rkRAttr.HasColor(1))
        {
            gl.glDisableClientState(GL.GL_SECONDARY_COLOR_ARRAY);
        }

        for (int iUnit = 0; iUnit < rkRAttr.GetMaxTCoords(); iUnit++)
        {
            if (rkRAttr.HasTCoord(iUnit))
            {
                gl.glClientActiveTexture(GL.GL_TEXTURE0 + iUnit);
                gl.glActiveTexture(GL.GL_TEXTURE0 + iUnit);
                gl.glDisableClientState(GL.GL_TEXTURE_COORD_ARRAY);
            }
        }
    }

    /**
     * Enable the IndexBuffer spefified by the ResourceIdentifer parameter pkID.
     * @param pkID the ResourceIdentifier describing the Index to enable.
     */
    public void OnEnableIBuffer (ResourceIdentifier pkID)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        // Bind the current index buffer.
        IBufferID pkResource = (IBufferID)pkID;
        gl.glBindBuffer(GL.GL_ELEMENT_ARRAY_BUFFER,pkResource.ID);
    }

    /**
     * Disable the IndexBuffer spefified by the ResourceIdentifer parameter pkID.
     * @param pkID the ResourceIdentifier describing the Index to disable.
     */
    public void OnDisableIBuffer (ResourceIdentifier pkID)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        // Unbind the current index buffer.
        gl.glBindBuffer(GL.GL_ELEMENT_ARRAY_BUFFER,0);
    }



    /** Text drawing
     * @param rkFont, the BitmapFont for text drawing
     * @param cChar, the character to draw.
     */
    public void DrawCharacter (final BitmapFont rkFont, char cChar)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        int uiIndex = (int)cChar;
        final BitmapFontChar pkBFC = rkFont.Chars[uiIndex];

        // save unpack state
        int iSwapBytes, iLSBFirst, iRowLength, iSkipRows, iSkipPixels;
        int iAlignment;
        int[] aiParams = new int[1];
        gl.glGetIntegerv(GL.GL_UNPACK_SWAP_BYTES,aiParams,0);
        iSwapBytes = aiParams[0];
        gl.glGetIntegerv(GL.GL_UNPACK_LSB_FIRST,aiParams,0);
        iLSBFirst = aiParams[0];
        gl.glGetIntegerv(GL.GL_UNPACK_ROW_LENGTH,aiParams,0);
        iRowLength = aiParams[0];
        gl.glGetIntegerv(GL.GL_UNPACK_SKIP_ROWS,aiParams,0);
        iSkipRows = aiParams[0];
        gl.glGetIntegerv(GL.GL_UNPACK_SKIP_PIXELS,aiParams,0);
        iSkipPixels = aiParams[0];
        gl.glGetIntegerv(GL.GL_UNPACK_ALIGNMENT,aiParams,0);
        iAlignment = aiParams[0];

        gl.glPixelStorei(GL.GL_UNPACK_SWAP_BYTES,GL.GL_FALSE);
        gl.glPixelStorei(GL.GL_UNPACK_LSB_FIRST,GL.GL_FALSE);
        gl.glPixelStorei(GL.GL_UNPACK_ROW_LENGTH,0);
        gl.glPixelStorei(GL.GL_UNPACK_SKIP_ROWS,0);
        gl.glPixelStorei(GL.GL_UNPACK_SKIP_PIXELS,0);
        gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT,1);
        ByteBuffer kBitmap = ByteBuffer.wrap(pkBFC.Bitmap);
        kBitmap.rewind();
        gl.glBitmap( pkBFC.XSize, pkBFC.YSize,
                     (float)pkBFC.XOrigin, (float)pkBFC.YOrigin,
                     (float)pkBFC.XSize, 0.0f,
                     kBitmap);

        // restore unpack state
        gl.glPixelStorei(GL.GL_UNPACK_SWAP_BYTES,iSwapBytes);
        gl.glPixelStorei(GL.GL_UNPACK_LSB_FIRST,iLSBFirst);
        gl.glPixelStorei(GL.GL_UNPACK_ROW_LENGTH,iRowLength);
        gl.glPixelStorei(GL.GL_UNPACK_SKIP_ROWS,iSkipRows);
        gl.glPixelStorei(GL.GL_UNPACK_SKIP_PIXELS,iSkipPixels);
        gl.glPixelStorei(GL.GL_UNPACK_ALIGNMENT,iAlignment);
    }

    /** Display list base indices for fonts/characters. */
    class DisplayListInfo
    {
        public int Quantity;  // number of display lists, input to glGenLists
        public int Start;     // start index, output from glGenLists
        public int Base;      // base index for glListBase
    };
    public Vector<DisplayListInfo> m_kDLInfo;

    /** Mappings from Wild Magic constants to OpenGL enumerations. */
    public static int[] ms_aeObjectType = new int[]
        {
            GL.GL_POINTS,          // GT_POLYPOINT
            GL.GL_LINES,           // GT_POLYLINE_SEGMENTS
            GL.GL_LINE_STRIP,      // GT_POLYLINE_OPEN
            GL.GL_LINE_LOOP,       // GT_POLYLINE_CLOSED
            GL.GL_TRIANGLES,       // GT_TRIMESH
            GL.GL_TRIANGLE_STRIP,  // GT_TRISTRIP
            GL.GL_TRIANGLE_FAN     // GT_TRIFAN
        };

    /** Mappings from Wild Magic constants to OpenGL enumerations. */
    public static int[] ms_aeAlphaSrcBlend = new int[]
        {
            GL.GL_ZERO,
            GL.GL_ONE,
            GL.GL_DST_COLOR,
            GL.GL_ONE_MINUS_DST_COLOR,
            GL.GL_SRC_ALPHA,
            GL.GL_ONE_MINUS_SRC_ALPHA,
            GL.GL_DST_ALPHA,
            GL.GL_ONE_MINUS_DST_ALPHA,
            GL.GL_SRC_ALPHA_SATURATE,
            GL.GL_CONSTANT_COLOR,
            GL.GL_ONE_MINUS_CONSTANT_COLOR,
            GL.GL_CONSTANT_ALPHA,
            GL.GL_ONE_MINUS_CONSTANT_ALPHA
        };
    
    /** Mappings from Wild Magic constants to OpenGL enumerations. */
    public static int[] ms_aeAlphaDstBlend = new int[]
        {
            GL.GL_ZERO,
            GL.GL_ONE,
            GL.GL_SRC_COLOR,
            GL.GL_ONE_MINUS_SRC_COLOR,
            GL.GL_SRC_ALPHA,
            GL.GL_ONE_MINUS_SRC_ALPHA,
            GL.GL_DST_ALPHA,
            GL.GL_ONE_MINUS_DST_ALPHA,
            GL.GL_CONSTANT_COLOR,
            GL.GL_ONE_MINUS_CONSTANT_COLOR,
            GL.GL_CONSTANT_ALPHA,
            GL.GL_ONE_MINUS_CONSTANT_ALPHA
        };

    /** Mappings from Wild Magic constants to OpenGL enumerations. */
    public static int[] ms_aeAlphaTest = new int[]
        {
            GL.GL_NEVER,
            GL.GL_LESS,
            GL.GL_EQUAL,
            GL.GL_LEQUAL,
            GL.GL_GREATER,
            GL.GL_NOTEQUAL,
            GL.GL_GEQUAL,
            GL.GL_ALWAYS
        };

    /** Mappings from Wild Magic constants to OpenGL enumerations. */
    public static int[] ms_aeFrontFace = new int[]
        {
            GL.GL_CCW,
            GL.GL_CW
        };

    /** Mappings from Wild Magic constants to OpenGL enumerations. */
    public static int[] ms_aeCullFace = new int[]
        {
            GL.GL_FRONT,
            GL.GL_BACK
        };

    /** Mappings from Wild Magic constants to OpenGL enumerations. */
    public static int[] ms_aeStencilCompare = new int[]
        {
            GL.GL_NEVER,     // StencilState::CF_NEVER
            GL.GL_LESS,      // StencilState::CF_LESS
            GL.GL_EQUAL,     // StencilState::CF_EQUAL
            GL.GL_LEQUAL,    // StencilState::CF_LEQUAL
            GL.GL_GREATER,   // StencilState::CF_GREATER
            GL.GL_NOTEQUAL,  // StencilState::CF_NOTEQUAL
            GL.GL_GEQUAL,    // StencilState::CF_GEQUAL
            GL.GL_ALWAYS     // StencilState::CF_ALWAYS
        };

    /** Mappings from Wild Magic constants to OpenGL enumerations. */
    public static int[] ms_aeStencilOperation = new int[]
        {
            GL.GL_KEEP,      // StencilState::OT_KEEP
            GL.GL_ZERO,      // StencilState::OT_ZERO
            GL.GL_REPLACE,   // StencilState::OT_REPLACE
            GL.GL_INCR,      // StencilState::OT_INCREMENT
            GL.GL_DECR,      // StencilState::OT_DECREMENT
            GL.GL_INVERT     // StencilState::OT_INVERT
        };

    /** Mappings from Wild Magic constants to OpenGL enumerations. */
    public static int[] ms_aeZBufferCompare = new int[]
        {
            GL.GL_NEVER,
            GL.GL_LESS,
            GL.GL_EQUAL,
            GL.GL_LEQUAL,
            GL.GL_GREATER,
            GL.GL_NOTEQUAL,
            GL.GL_GEQUAL,
            GL.GL_ALWAYS
        };

    /** Mappings from Wild Magic constants to OpenGL enumerations. */
    public static int[] ms_aeTextureMipmap = new int[]
        {
            GL.GL_NEAREST,
            GL.GL_LINEAR,
            GL.GL_NEAREST_MIPMAP_NEAREST,
            GL.GL_NEAREST_MIPMAP_LINEAR,
            GL.GL_LINEAR_MIPMAP_NEAREST,
            GL.GL_LINEAR_MIPMAP_LINEAR
        };

    /** Mappings from Wild Magic constants to OpenGL enumerations. */
    public static int[] ms_aeDepthCompare = new int[]
        {
            GL.GL_NEVER,
            GL.GL_LESS,
            GL.GL_EQUAL,
            GL.GL_LEQUAL,
            GL.GL_GREATER,
            GL.GL_NOTEQUAL,
            GL.GL_GEQUAL,
            GL.GL_ALWAYS
        };

    /** Mappings from Wild Magic constants to OpenGL enumerations. */
    public static int[] ms_aeWrapMode = new int[]
        {
            GL.GL_CLAMP,
            GL.GL_REPEAT,
            GL.GL_MIRRORED_REPEAT,
            GL.GL_CLAMP_TO_BORDER,
            GL.GL_CLAMP_TO_EDGE
        };

    /** Mappings from Wild Magic constants to OpenGL enumerations. */
    public static int[] ms_aeImageComponents = new int[]
        {
            GL.GL_RGB8,               // Image::IT_RGB888
            GL.GL_RGBA8,              // Image::IT_RGBA8888
            GL.GL_DEPTH_COMPONENT16,  // Image::IT_DEPTH16
            GL.GL_DEPTH_COMPONENT24,  // Image::IT_DEPTH24
            GL.GL_DEPTH_COMPONENT32,  // Image::IT_DEPTH32
            GL.GL_RGB8,               // Image::IT_CUBE_RGB888
            GL.GL_RGBA8,              // Image::IT_CUBE_RGBA8888

            // TO DO.  Not yet tested.
            GL.GL_RGB32F_ARB,         // Image::IT_RGB32
            GL.GL_RGBA32F_ARB         // Image::IT_RGBA32
        };

    /** Mappings from Wild Magic constants to OpenGL enumerations. */
    public static int[] ms_aeImageFormats = new int[]
        {
            GL.GL_RGB,              // Image::IT_RGB888
            GL.GL_RGBA,             // Image::IT_RGBA8888
            GL.GL_DEPTH_COMPONENT,  // Image::IT_DEPTH16
            GL.GL_DEPTH_COMPONENT,  // Image::IT_DEPTH24
            GL.GL_DEPTH_COMPONENT,  // Image::IT_DEPTH32
            GL.GL_RGB,              // Image::IT_CUBE_RGB888
            GL.GL_RGBA,             // Image::IT_CUBE_RGBA8888

            // TO DO.  Not yet tested.
            GL.GL_RGB,              // Image::IT_RGB32
            GL.GL_RGBA              // Image::IT_RGBA32
        };

    /** Mappings from Wild Magic constants to OpenGL enumerations. */
    public static int[] ms_aeImageTypes = new int[]
        {
            GL.GL_UNSIGNED_BYTE,    // Image::IT_RGB888
            GL.GL_UNSIGNED_BYTE,    // Image::IT_RGBA8888
            GL.GL_FLOAT,            // Image::IT_DEPTH16
            GL.GL_FLOAT,            // Image::IT_DEPTH24
            GL.GL_DEPTH_COMPONENT,  // Image::IT_DEPTH32
            GL.GL_UNSIGNED_BYTE,    // Image::IT_CUBE_RGB888
            GL.GL_UNSIGNED_BYTE,    // Image::IT_CUBE_RGBA8888

            // TO DO.  Not yet tested.
            GL.GL_FLOAT,            // Image::IT_RGB32
            GL.GL_FLOAT             // Image::IT_RGBA32
        };

    /** Mappings from Wild Magic constants to OpenGL enumerations. */
    public static int[] ms_aeSamplerTypes = new int[]
        {
            GL.GL_TEXTURE_1D,        // SamplerInformation::SAMPLER_1D
            GL.GL_TEXTURE_2D,        // SamplerInformation::SAMPLER_2D
            GL.GL_TEXTURE_3D,        // SamplerInformation::SAMPLER_3D
            GL.GL_TEXTURE_CUBE_MAP,  // SamplerInformation::SAMPLER_CUBE
            GL.GL_TEXTURE_2D,        // SamplerInformation::SAMPLER_PROJ
        };

    /** Returns the GLCanvas for drawing.
     * @return the current GLCanvas.
     */
    public GLCanvas GetCanvas() { return m_kCanvas; }

    /** GLCanvas for Java/JOGL */
    private GLCanvas m_kCanvas;
    /** JOGL GLAutoDrawable reference */
    private GLAutoDrawable m_kDrawable = null;

    /** Bitmap Fonts: */
    private static final byte[] gs_aucChar0 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar0 = new BitmapFontChar(0,0,13,16,gs_aucChar0);

    private static final byte[] gs_aucChar1 = new byte[] 
        {
            0x00,0x00,0x10,0x00,0x10,0x00,0x10,0x00,
            0x10,0x00,0x10,0x00,0x10,0x00,0x10,0x00,
            0x1E,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar1 = new BitmapFontChar(0,0,13,16,gs_aucChar1);

    private static final byte[] gs_aucChar2 = new byte[] 
        {
            0x00,0x00,0x10,0x00,0x10,0x00,0x10,0x00,
            0x10,0x00,0x10,0x00,0x10,0x00,0x10,0x00,
            (byte)0xF0,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar2 = new BitmapFontChar(0,0,13,16,gs_aucChar2);

    private static final byte[] gs_aucChar3 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x1E,0x00,0x10,0x00,0x10,0x00,0x10,0x00,
            0x10,0x00,0x10,0x00,0x10,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar3 = new BitmapFontChar(0,0,13,16,gs_aucChar3);

    private static final byte[] gs_aucChar4 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            (byte)0xF0,0x00,0x10,0x00,0x10,0x00,0x10,0x00,
            0x10,0x00,0x10,0x00,0x10,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar4 = new BitmapFontChar(0,0,13,16,gs_aucChar4);

    private static final byte[] gs_aucChar5 = new byte[] 
        {
            0x00,0x00,0x10,0x00,0x10,0x00,0x10,0x00,
            0x10,0x00,0x10,0x00,0x10,0x00,0x10,0x00,
            0x10,0x00,0x10,0x00,0x10,0x00,0x10,0x00,
            0x10,0x00,0x10,0x00,0x10,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar5 = new BitmapFontChar(0,0,13,16,gs_aucChar5);

    private static final byte[] gs_aucChar6 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            (byte)0xFE,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar6 = new BitmapFontChar(0,0,13,16,gs_aucChar6);

    private static final byte[] gs_aucChar7 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x38,0x00,0x7C,0x00,
            0x7C,0x00,0x7C,0x00,0x38,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar7 = new BitmapFontChar(0,0,13,16,gs_aucChar7);

    private static final byte[] gs_aucChar8 = new byte[] 
        {
            0x00,0x00,(byte)0xFE,0x00,(byte)0xFE,0x00,(byte)0xFE,0x00,
            (byte)0xFE,0x00,(byte)0xFE,0x00,(byte)0xC6,0x00,(byte)0x82,0x00,
            (byte)0x82,0x00,(byte)0x82,0x00,(byte)0xC6,0x00,(byte)0xFE,0x00,
            (byte)0xFE,0x00,(byte)0xFE,0x00,(byte)0xFE,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar8 = new BitmapFontChar(0,0,13,16,gs_aucChar8);

    private static final byte[] gs_aucChar9 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar9 = new BitmapFontChar(0,0,13,16,gs_aucChar9);

    private static final byte[] gs_aucChar10 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar10 = new BitmapFontChar(0,0,13,16,gs_aucChar10);

    private static final byte[] gs_aucChar11 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x30,0x00,
            0x48,0x00,0x48,0x00,0x30,0x00,0x10,0x00,
            0x10,0x00,0x0A,0x00,0x0A,0x00,0x06,0x00,
            0x1E,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar11 = new BitmapFontChar(0,0,13,16,gs_aucChar11);

    private static final byte[] gs_aucChar12 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x3F,(byte)0xF0,
            0x20,0x10,0x20,0x10,0x20,0x10,0x20,0x10,
            0x20,0x10,0x20,0x10,0x20,0x10,0x20,0x10,
            0x3F,(byte)0xF0,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar12 = new BitmapFontChar(0,0,13,16,gs_aucChar12);

    private static final byte[] gs_aucChar13 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar13 = new BitmapFontChar(0,0,13,16,gs_aucChar13);

    private static final byte[] gs_aucChar14 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x60,0x00,
            0x66,0x00,0x26,0x00,0x22,0x00,0x22,0x00,
            0x22,0x00,0x3A,0x00,0x26,0x00,0x3A,0x00,
            0x06,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar14 = new BitmapFontChar(0,0,13,16,gs_aucChar14);

    private static final byte[] gs_aucChar15 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x54,0x00,0x54,0x00,0x28,0x00,0x28,0x00,
            0x6C,0x00,0x28,0x00,0x28,0x00,0x54,0x00,
            0x54,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar15 = new BitmapFontChar(0,0,13,16,gs_aucChar15);

    private static final byte[] gs_aucChar16 = new byte[] 
        {
            0x00,0x00,0x10,0x00,0x10,0x00,0x10,0x00,
            0x10,0x00,0x10,0x00,0x10,0x00,0x10,0x00,
            (byte)0xFE,0x00,0x10,0x00,0x10,0x00,0x10,0x00,
            0x10,0x00,0x10,0x00,0x10,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar16 = new BitmapFontChar(0,0,13,16,gs_aucChar16);

    private static final byte[] gs_aucChar17 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x02,0x00,
            0x06,0x00,0x0E,0x00,0x1E,0x00,0x3E,0x00,
            0x7E,0x00,0x3E,0x00,0x1E,0x00,0x0E,0x00,
            0x06,0x00,0x02,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar17 = new BitmapFontChar(0,0,13,16,gs_aucChar17);

    private static final byte[] gs_aucChar18 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x10,0x00,0x38,0x00,
            0x54,0x00,0x10,0x00,0x10,0x00,0x10,0x00,
            0x10,0x00,0x10,0x00,0x10,0x00,0x10,0x00,
            0x54,0x00,0x38,0x00,0x10,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar18 = new BitmapFontChar(0,0,13,16,gs_aucChar18);

    private static final byte[] gs_aucChar19 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x24,0x00,0x24,0x00,
            0x00,0x00,0x00,0x00,0x24,0x00,0x24,0x00,
            0x24,0x00,0x24,0x00,0x24,0x00,0x24,0x00,
            0x24,0x00,0x24,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar19 = new BitmapFontChar(0,0,13,16,gs_aucChar19);

    private static final byte[] gs_aucChar20 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x14,0x00,0x14,0x00,
            0x14,0x00,0x14,0x00,0x14,0x00,0x34,0x00,
            0x54,0x00,0x54,0x00,0x54,0x00,0x54,0x00,
            0x54,0x00,0x34,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar20 = new BitmapFontChar(0,0,13,16,gs_aucChar20);

    private static final byte[] gs_aucChar21 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            (byte)0xFE,0x00,0x10,0x00,0x10,0x00,0x10,0x00,
            0x10,0x00,0x10,0x00,0x10,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar21 = new BitmapFontChar(0,0,13,16,gs_aucChar21);

    private static final byte[] gs_aucChar22 = new byte[] 
        {
            0x00,0x00,0x10,0x00,0x10,0x00,0x10,0x00,
            0x10,0x00,0x10,0x00,0x10,0x00,0x10,0x00,
            (byte)0xFE,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar22 = new BitmapFontChar(0,0,13,16,gs_aucChar22);

    private static final byte[] gs_aucChar23 = new byte[] 
        {
            0x00,0x00,0x10,0x00,0x10,0x00,0x10,0x00,
            0x10,0x00,0x10,0x00,0x10,0x00,0x10,0x00,
            (byte)0xF0,0x00,0x10,0x00,0x10,0x00,0x10,0x00,
            0x10,0x00,0x10,0x00,0x10,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar23 = new BitmapFontChar(0,0,13,16,gs_aucChar23);

    private static final byte[] gs_aucChar24 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x10,0x00,0x10,0x00,
            0x10,0x00,0x10,0x00,0x10,0x00,0x10,0x00,
            0x10,0x00,0x10,0x00,0x10,0x00,0x54,0x00,
            0x38,0x00,0x10,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar24 = new BitmapFontChar(0,0,13,16,gs_aucChar24);

    private static final byte[] gs_aucChar25 = new byte[] 
        {
            0x00,0x00,0x10,0x00,0x10,0x00,0x10,0x00,
            0x10,0x00,0x10,0x00,0x10,0x00,0x10,0x00,
            0x1E,0x00,0x10,0x00,0x10,0x00,0x10,0x00,
            0x10,0x00,0x10,0x00,0x10,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar25 = new BitmapFontChar(0,0,13,16,gs_aucChar25);

    private static final byte[] gs_aucChar26 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x08,0x00,0x04,0x00,0x7E,0x00,
            0x04,0x00,0x08,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar26 = new BitmapFontChar(0,0,13,16,gs_aucChar26);

    private static final byte[] gs_aucChar27 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x10,0x00,0x20,0x00,0x7E,0x00,
            0x20,0x00,0x10,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar27 = new BitmapFontChar(0,0,13,16,gs_aucChar27);

    private static final byte[] gs_aucChar28 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar28 = new BitmapFontChar(0,0,13,16,gs_aucChar28);

    private static final byte[] gs_aucChar29 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar29 = new BitmapFontChar(0,0,13,16,gs_aucChar29);

    private static final byte[] gs_aucChar30 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar30 = new BitmapFontChar(0,0,13,16,gs_aucChar30);

    private static final byte[] gs_aucChar31 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar31 = new BitmapFontChar(0,0,13,16,gs_aucChar31);

    private static final byte[] gs_aucChar32 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar32 = new BitmapFontChar(0,0,5,16,gs_aucChar32);

    private static final byte[] gs_aucChar33 = new byte[] 
        {
            0x00,0x00,0x00,0x20,0x20,0x00,0x20,0x20,
            0x20,0x20,0x20,0x20,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar33 = new BitmapFontChar(0,0,5,16,gs_aucChar33);

    private static final byte[] gs_aucChar34 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x50,0x50,0x50,0x50,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar34 = new BitmapFontChar(0,0,5,16,gs_aucChar34);

    private static final byte[] gs_aucChar35 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x24,0x00,0x24,0x00,0x7F,0x00,0x12,0x00,
            0x12,0x00,0x3F,(byte)0x80,0x09,0x00,0x09,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar35 = new BitmapFontChar(0,0,10,16,gs_aucChar35);

    private static final byte[] gs_aucChar36 = new byte[] 
        {
            0x00,0x10,0x10,0x7C,0x12,0x12,0x1C,0x30,
            0x50,0x50,0x3E,0x10,0x10,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar36 = new BitmapFontChar(0,0,8,16,gs_aucChar36);

    private static final byte[] gs_aucChar37 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x10,(byte)0xE0,
            0x09,0x10,0x05,0x10,0x05,0x10,0x3A,(byte)0xE0,
            0x45,0x00,0x45,0x00,0x44,(byte)0x80,0x38,0x40,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar37 = new BitmapFontChar(0,0,13,16,gs_aucChar37);

    private static final byte[] gs_aucChar38 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x39,(byte)0x80,
            0x46,0x00,0x42,0x00,0x45,0x00,0x38,(byte)0x80,
            0x44,0x00,0x44,0x00,0x44,0x00,0x38,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar38 = new BitmapFontChar(0,0,9,16,gs_aucChar38);

    private static final byte[] gs_aucChar39 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x40,0x40,0x40,0x40,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar39 = new BitmapFontChar(0,0,3,16,gs_aucChar39);

    private static final byte[] gs_aucChar40 = new byte[] 
        {
            0x08,0x10,0x20,0x20,0x40,0x40,0x40,0x40,
            0x40,0x20,0x20,0x10,0x08,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar40 = new BitmapFontChar(0,0,6,16,gs_aucChar40);

    private static final byte[] gs_aucChar41 = new byte[] 
        {
            0x40,0x20,0x10,0x10,0x08,0x08,0x08,0x08,
            0x08,0x10,0x10,0x20,0x40,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar41 = new BitmapFontChar(0,0,6,16,gs_aucChar41);

    private static final byte[] gs_aucChar42 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x08,0x00,0x2A,0x00,0x1C,0x00,0x2A,0x00,
            0x08,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar42 = new BitmapFontChar(0,0,9,16,gs_aucChar42);

    private static final byte[] gs_aucChar43 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x08,0x00,0x08,0x00,0x08,0x00,0x7F,0x00,
            0x08,0x00,0x08,0x00,0x08,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar43 = new BitmapFontChar(0,0,9,16,gs_aucChar43);

    private static final byte[] gs_aucChar44 = new byte[] 
        {
            0x00,0x40,0x20,0x20,0x20,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar44 = new BitmapFontChar(0,0,5,16,gs_aucChar44);

    private static final byte[] gs_aucChar45 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x7C,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar45 = new BitmapFontChar(0,0,7,16,gs_aucChar45);

    private static final byte[] gs_aucChar46 = new byte[] 
        {
            0x00,0x00,0x00,0x20,0x20,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar46 = new BitmapFontChar(0,0,5,16,gs_aucChar46);

    private static final byte[] gs_aucChar47 = new byte[] 
        {
            0x00,(byte)0x80,(byte)0x80,0x40,0x40,0x20,0x20,0x10,
            0x10,0x08,0x08,0x04,0x04,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar47 = new BitmapFontChar(0,0,6,16,gs_aucChar47);

    private static final byte[] gs_aucChar48 = new byte[] 
        {
            0x00,0x00,0x00,0x3C,0x42,0x42,0x42,0x42,
            0x42,0x42,0x42,0x3C,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar48 = new BitmapFontChar(0,0,8,16,gs_aucChar48);

    private static final byte[] gs_aucChar49 = new byte[] 
        {
            0x00,0x00,0x00,0x3E,0x08,0x08,0x08,0x08,
            0x08,0x08,0x38,0x08,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar49 = new BitmapFontChar(0,0,8,16,gs_aucChar49);

    private static final byte[] gs_aucChar50 = new byte[] 
        {
            0x00,0x00,0x00,0x7E,0x40,0x20,0x18,0x04,
            0x02,0x42,0x42,0x3C,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar50 = new BitmapFontChar(0,0,8,16,gs_aucChar50);

    private static final byte[] gs_aucChar51 = new byte[] 
        {
            0x00,0x00,0x00,0x3C,0x42,0x02,0x02,0x1C,
            0x02,0x02,0x42,0x3C,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar51 = new BitmapFontChar(0,0,8,16,gs_aucChar51);

    private static final byte[] gs_aucChar52 = new byte[] 
        {
            0x00,0x00,0x00,0x04,0x04,0x04,0x7F,0x44,
            0x24,0x14,0x0C,0x04,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar52 = new BitmapFontChar(0,0,8,16,gs_aucChar52);

    private static final byte[] gs_aucChar53 = new byte[] 
        {
            0x00,0x00,0x00,0x3C,0x42,0x02,0x02,0x3C,
            0x20,0x20,0x20,0x3E,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar53 = new BitmapFontChar(0,0,8,16,gs_aucChar53);

    private static final byte[] gs_aucChar54 = new byte[] 
        {
            0x00,0x00,0x00,0x3C,0x42,0x42,0x42,0x42,
            0x7C,0x40,0x20,0x1C,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar54 = new BitmapFontChar(0,0,8,16,gs_aucChar54);

    private static final byte[] gs_aucChar55 = new byte[] 
        {
            0x00,0x00,0x00,0x10,0x10,0x10,0x08,0x08,
            0x04,0x04,0x02,0x7E,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar55 = new BitmapFontChar(0,0,8,16,gs_aucChar55);

    private static final byte[] gs_aucChar56 = new byte[] 
        {
            0x00,0x00,0x00,0x3C,0x42,0x42,0x42,0x3C,
            0x42,0x42,0x42,0x3C,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar56 = new BitmapFontChar(0,0,8,16,gs_aucChar56);

    private static final byte[] gs_aucChar57 = new byte[] 
        {
            0x00,0x00,0x00,0x38,0x04,0x02,0x3E,0x42,
            0x42,0x42,0x42,0x3C,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar57 = new BitmapFontChar(0,0,8,16,gs_aucChar57);

    private static final byte[] gs_aucChar58 = new byte[] 
        {
            0x00,0x00,0x00,0x20,0x20,0x00,0x00,0x00,
            0x20,0x20,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar58 = new BitmapFontChar(0,0,6,16,gs_aucChar58);

    private static final byte[] gs_aucChar59 = new byte[] 
        {
            0x00,0x40,0x20,0x20,0x20,0x00,0x00,0x00,
            0x20,0x20,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar59 = new BitmapFontChar(0,0,6,16,gs_aucChar59);

    private static final byte[] gs_aucChar60 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x03,0x00,
            0x0C,0x00,0x30,0x00,0x40,0x00,0x30,0x00,
            0x0C,0x00,0x03,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar60 = new BitmapFontChar(0,0,9,16,gs_aucChar60);

    private static final byte[] gs_aucChar61 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x7F,0x00,0x00,0x00,0x00,0x00,
            0x7F,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar61 = new BitmapFontChar(0,0,9,16,gs_aucChar61);

    private static final byte[] gs_aucChar62 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x60,0x00,
            0x18,0x00,0x06,0x00,0x01,0x00,0x06,0x00,
            0x18,0x00,0x60,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar62 = new BitmapFontChar(0,0,9,16,gs_aucChar62);

    private static final byte[] gs_aucChar63 = new byte[] 
        {
            0x00,0x00,0x00,0x10,0x10,0x00,0x10,0x10,
            0x08,0x04,0x44,0x38,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar63 = new BitmapFontChar(0,0,7,16,gs_aucChar63);

    private static final byte[] gs_aucChar64 = new byte[] 
        {
            0x00,0x00,0x0F,(byte)0x80,0x10,0x00,0x27,(byte)0xE0,
            0x48,(byte)0x90,0x48,(byte)0x90,0x48,(byte)0x90,0x48,(byte)0x90,
            0x48,(byte)0x90,0x27,(byte)0xA0,0x10,0x40,0x0F,(byte)0x80,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar64 = new BitmapFontChar(0,0,13,16,gs_aucChar64);

    private static final byte[] gs_aucChar65 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x41,0x00,
            0x41,0x00,0x3E,0x00,0x22,0x00,0x22,0x00,
            0x14,0x00,0x14,0x00,0x08,0x00,0x08,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar65 = new BitmapFontChar(0,0,9,16,gs_aucChar65);

    private static final byte[] gs_aucChar66 = new byte[] 
        {
            0x00,0x00,0x00,0x7C,0x42,0x42,0x42,0x7C,
            0x44,0x44,0x44,0x78,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar66 = new BitmapFontChar(0,0,8,16,gs_aucChar66);

    private static final byte[] gs_aucChar67 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x1E,0x00,
            0x21,0x00,0x40,0x00,0x40,0x00,0x40,0x00,
            0x40,0x00,0x40,0x00,0x21,0x00,0x1E,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar67 = new BitmapFontChar(0,0,9,16,gs_aucChar67);

    private static final byte[] gs_aucChar68 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x7C,0x00,
            0x42,0x00,0x41,0x00,0x41,0x00,0x41,0x00,
            0x41,0x00,0x41,0x00,0x42,0x00,0x7C,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar68 = new BitmapFontChar(0,0,9,16,gs_aucChar68);

    private static final byte[] gs_aucChar69 = new byte[] 
        {
            0x00,0x00,0x00,0x7E,0x40,0x40,0x40,0x7E,
            0x40,0x40,0x40,0x7E,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar69 = new BitmapFontChar(0,0,8,16,gs_aucChar69);

    private static final byte[] gs_aucChar70 = new byte[] 
        {
            0x00,0x00,0x00,0x40,0x40,0x40,0x40,0x7C,
            0x40,0x40,0x40,0x7E,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar70 = new BitmapFontChar(0,0,8,16,gs_aucChar70);

    private static final byte[] gs_aucChar71 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x1F,0x00,
            0x21,0x00,0x41,0x00,0x47,0x00,0x40,0x00,
            0x40,0x00,0x40,0x00,0x21,0x00,0x1E,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar71 = new BitmapFontChar(0,0,9,16,gs_aucChar71);

    private static final byte[] gs_aucChar72 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x41,0x00,
            0x41,0x00,0x41,0x00,0x41,0x00,0x7F,0x00,
            0x41,0x00,0x41,0x00,0x41,0x00,0x41,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar72 = new BitmapFontChar(0,0,9,16,gs_aucChar72);

    private static final byte[] gs_aucChar73 = new byte[] 
        {
            0x00,0x00,0x00,0x70,0x20,0x20,0x20,0x20,
            0x20,0x20,0x20,0x70,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar73 = new BitmapFontChar(0,0,5,16,gs_aucChar73);

    private static final byte[] gs_aucChar74 = new byte[] 
        {
            0x00,0x00,0x00,(byte)0xF0,0x08,0x08,0x08,0x08,
            0x08,0x08,0x08,0x38,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar74 = new BitmapFontChar(0,0,6,16,gs_aucChar74);

    private static final byte[] gs_aucChar75 = new byte[] 
        {
            0x00,0x00,0x00,0x42,0x44,0x48,0x50,0x60,
            0x50,0x48,0x44,0x42,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar75 = new BitmapFontChar(0,0,8,16,gs_aucChar75);

    private static final byte[] gs_aucChar76 = new byte[] 
        {
            0x00,0x00,0x00,0x7E,0x40,0x40,0x40,0x40,
            0x40,0x40,0x40,0x40,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar76 = new BitmapFontChar(0,0,7,16,gs_aucChar76);

    private static final byte[] gs_aucChar77 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x40,0x40,
            0x44,0x40,0x44,0x40,0x4A,0x40,0x4A,0x40,
            0x51,0x40,0x51,0x40,0x60,(byte)0xC0,0x60,(byte)0xC0,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar77 = new BitmapFontChar(0,0,11,16,gs_aucChar77);

    private static final byte[] gs_aucChar78 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x43,0x00,
            0x43,0x00,0x45,0x00,0x45,0x00,0x49,0x00,
            0x51,0x00,0x51,0x00,0x61,0x00,0x61,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar78 = new BitmapFontChar(0,0,9,16,gs_aucChar78);

    private static final byte[] gs_aucChar79 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x1E,0x00,
            0x21,0x00,0x40,(byte)0x80,0x40,(byte)0x80,0x40,(byte)0x80,
            0x40,(byte)0x80,0x40,(byte)0x80,0x21,0x00,0x1E,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar79 = new BitmapFontChar(0,0,10,16,gs_aucChar79);

    private static final byte[] gs_aucChar80 = new byte[] 
        {
            0x00,0x00,0x00,0x40,0x40,0x40,0x7C,0x42,
            0x42,0x42,0x42,0x7C,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar80 = new BitmapFontChar(0,0,8,16,gs_aucChar80);

    private static final byte[] gs_aucChar81 = new byte[] 
        {
            0x00,0x00,0x01,(byte)0x80,0x02,0x00,0x1E,0x00,
            0x21,0x00,0x40,(byte)0x80,0x40,(byte)0x80,0x40,(byte)0x80,
            0x40,(byte)0x80,0x40,(byte)0x80,0x21,0x00,0x1E,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar81 = new BitmapFontChar(0,0,10,16,gs_aucChar81);

    private static final byte[] gs_aucChar82 = new byte[] 
        {
            0x00,0x00,0x00,0x41,0x42,0x44,0x78,0x44,
            0x42,0x42,0x42,0x7C,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar82 = new BitmapFontChar(0,0,8,16,gs_aucChar82);

    private static final byte[] gs_aucChar83 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x3E,0x00,
            0x41,0x00,0x01,0x00,0x01,0x00,0x3E,0x00,
            0x40,0x00,0x40,0x00,0x41,0x00,0x3E,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar83 = new BitmapFontChar(0,0,9,16,gs_aucChar83);

    private static final byte[] gs_aucChar84 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x08,0x00,
            0x08,0x00,0x08,0x00,0x08,0x00,0x08,0x00,
            0x08,0x00,0x08,0x00,0x08,0x00,0x7F,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar84 = new BitmapFontChar(0,0,9,16,gs_aucChar84);

    private static final byte[] gs_aucChar85 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x1C,0x00,
            0x22,0x00,0x41,0x00,0x41,0x00,0x41,0x00,
            0x41,0x00,0x41,0x00,0x41,0x00,0x41,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar85 = new BitmapFontChar(0,0,9,16,gs_aucChar85);

    private static final byte[] gs_aucChar86 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x08,0x00,
            0x08,0x00,0x14,0x00,0x14,0x00,0x22,0x00,
            0x22,0x00,0x41,0x00,0x41,0x00,0x41,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar86 = new BitmapFontChar(0,0,9,16,gs_aucChar86);

    private static final byte[] gs_aucChar87 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x10,0x40,
            0x10,0x40,0x28,(byte)0xA0,0x28,(byte)0xA0,0x25,0x20,
            0x45,0x10,0x45,0x10,0x42,0x10,0x42,0x10,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar87 = new BitmapFontChar(0,0,13,16,gs_aucChar87);

    private static final byte[] gs_aucChar88 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x41,0x00,
            0x41,0x00,0x22,0x00,0x14,0x00,0x08,0x00,
            0x14,0x00,0x22,0x00,0x41,0x00,0x41,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar88 = new BitmapFontChar(0,0,9,16,gs_aucChar88);

    private static final byte[] gs_aucChar89 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x08,0x00,
            0x08,0x00,0x08,0x00,0x08,0x00,0x14,0x00,
            0x14,0x00,0x22,0x00,0x22,0x00,0x41,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar89 = new BitmapFontChar(0,0,9,16,gs_aucChar89);

    private static final byte[] gs_aucChar90 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x7F,0x00,
            0x40,0x00,0x20,0x00,0x10,0x00,0x08,0x00,
            0x04,0x00,0x02,0x00,0x01,0x00,0x7F,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar90 = new BitmapFontChar(0,0,9,16,gs_aucChar90);

    private static final byte[] gs_aucChar91 = new byte[] 
        {
            0x00,0x38,0x20,0x20,0x20,0x20,0x20,0x20,
            0x20,0x20,0x20,0x20,0x38,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar91 = new BitmapFontChar(0,0,6,16,gs_aucChar91);

    private static final byte[] gs_aucChar92 = new byte[] 
        {
            0x00,0x04,0x04,0x08,0x08,0x10,0x10,0x20,
            0x20,0x40,0x40,(byte)0x80,(byte)0x80,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar92 = new BitmapFontChar(0,0,6,16,gs_aucChar92);

    private static final byte[] gs_aucChar93 = new byte[] 
        {
            0x00,0x70,0x10,0x10,0x10,0x10,0x10,0x10,
            0x10,0x10,0x10,0x10,0x70,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar93 = new BitmapFontChar(0,0,6,16,gs_aucChar93);

    private static final byte[] gs_aucChar94 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x40,0x40,
            0x20,(byte)0x80,0x11,0x00,0x0A,0x00,0x04,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar94 = new BitmapFontChar(0,0,11,16,gs_aucChar94);

    private static final byte[] gs_aucChar95 = new byte[] 
        {
            0x00,(byte)0xFF,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar95 = new BitmapFontChar(0,0,8,16,gs_aucChar95);

    private static final byte[] gs_aucChar96 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x08,0x10,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar96 = new BitmapFontChar(0,0,8,16,gs_aucChar96);

    private static final byte[] gs_aucChar97 = new byte[] 
        {
            0x00,0x00,0x00,0x3E,0x42,0x42,0x3E,0x02,
            0x02,0x3C,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar97 = new BitmapFontChar(0,0,8,16,gs_aucChar97);

    private static final byte[] gs_aucChar98 = new byte[] 
        {
            0x00,0x00,0x00,0x7C,0x42,0x42,0x42,0x42,
            0x62,0x5C,0x40,0x40,0x40,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar98 = new BitmapFontChar(0,0,8,16,gs_aucChar98);

    private static final byte[] gs_aucChar99 = new byte[] 
        {
            0x00,0x00,0x00,0x3C,0x42,0x40,0x40,0x40,
            0x42,0x3C,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar99 = new BitmapFontChar(0,0,8,16,gs_aucChar99);

    private static final byte[] gs_aucChar100 = new byte[] 
        {
            0x00,0x00,0x00,0x3A,0x46,0x42,0x42,0x42,
            0x42,0x3E,0x02,0x02,0x02,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar100 = new BitmapFontChar(0,0,8,16,gs_aucChar100);

    private static final byte[] gs_aucChar101 = new byte[] 
        {
            0x00,0x00,0x00,0x3C,0x42,0x40,0x7E,0x42,
            0x42,0x3C,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar101 = new BitmapFontChar(0,0,8,16,gs_aucChar101);

    private static final byte[] gs_aucChar102 = new byte[] 
        {
            0x00,0x00,0x00,0x20,0x20,0x20,0x20,0x20,
            0x20,0x78,0x20,0x20,0x18,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar102 = new BitmapFontChar(0,0,5,16,gs_aucChar102);

    private static final byte[] gs_aucChar103 = new byte[] 
        {
            0x3C,0x02,0x02,0x3A,0x46,0x42,0x42,0x42,
            0x42,0x3E,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar103 = new BitmapFontChar(0,0,8,16,gs_aucChar103);

    private static final byte[] gs_aucChar104 = new byte[] 
        {
            0x00,0x00,0x00,0x42,0x42,0x42,0x42,0x42,
            0x62,0x5C,0x40,0x40,0x40,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar104 = new BitmapFontChar(0,0,8,16,gs_aucChar104);

    private static final byte[] gs_aucChar105 = new byte[] 
        {
            0x00,0x00,0x00,0x40,0x40,0x40,0x40,0x40,
            0x40,0x40,0x00,0x00,0x40,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar105 = new BitmapFontChar(0,0,3,16,gs_aucChar105);

    private static final byte[] gs_aucChar106 = new byte[] 
        {
            (byte)0xC0,0x20,0x20,0x20,0x20,0x20,0x20,0x20,
            0x20,0x60,0x00,0x00,0x20,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar106 = new BitmapFontChar(0,0,4,16,gs_aucChar106);

    private static final byte[] gs_aucChar107 = new byte[] 
        {
            0x00,0x00,0x00,0x44,0x48,0x50,0x60,0x50,
            0x48,0x44,0x40,0x40,0x40,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar107 = new BitmapFontChar(0,0,7,16,gs_aucChar107);

    private static final byte[] gs_aucChar108 = new byte[] 
        {
            0x00,0x00,0x00,0x40,0x40,0x40,0x40,0x40,
            0x40,0x40,0x40,0x40,0x40,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar108 = new BitmapFontChar(0,0,3,16,gs_aucChar108);

    private static final byte[] gs_aucChar109 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x44,0x40,
            0x44,0x40,0x44,0x40,0x44,0x40,0x44,0x40,
            0x66,0x40,0x59,(byte)0x80,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar109 = new BitmapFontChar(0,0,11,16,gs_aucChar109);

    private static final byte[] gs_aucChar110 = new byte[] 
        {
            0x00,0x00,0x00,0x42,0x42,0x42,0x42,0x42,
            0x62,0x5C,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar110 = new BitmapFontChar(0,0,8,16,gs_aucChar110);

    private static final byte[] gs_aucChar111 = new byte[] 
        {
            0x00,0x00,0x00,0x3C,0x42,0x42,0x42,0x42,
            0x42,0x3C,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar111 = new BitmapFontChar(0,0,8,16,gs_aucChar111);

    private static final byte[] gs_aucChar112 = new byte[] 
        {
            0x40,0x40,0x40,0x7C,0x42,0x42,0x42,0x42,
            0x62,0x5C,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar112 = new BitmapFontChar(0,0,8,16,gs_aucChar112);

    private static final byte[] gs_aucChar113 = new byte[] 
        {
            0x02,0x02,0x02,0x3A,0x46,0x42,0x42,0x42,
            0x42,0x3E,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar113 = new BitmapFontChar(0,0,8,16,gs_aucChar113);

    private static final byte[] gs_aucChar114 = new byte[] 
        {
            0x00,0x00,0x00,0x40,0x40,0x40,0x40,0x40,
            0x60,0x58,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar114 = new BitmapFontChar(0,0,5,16,gs_aucChar114);

    private static final byte[] gs_aucChar115 = new byte[] 
        {
            0x00,0x00,0x00,0x78,0x04,0x04,0x38,0x40,
            0x40,0x3C,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar115 = new BitmapFontChar(0,0,7,16,gs_aucChar115);

    private static final byte[] gs_aucChar116 = new byte[] 
        {
            0x00,0x00,0x00,0x18,0x20,0x20,0x20,0x20,
            0x20,0x78,0x20,0x20,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar116 = new BitmapFontChar(0,0,6,16,gs_aucChar116);

    private static final byte[] gs_aucChar117 = new byte[] 
        {
            0x00,0x00,0x00,0x3A,0x46,0x42,0x42,0x42,
            0x42,0x42,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar117 = new BitmapFontChar(0,0,8,16,gs_aucChar117);

    private static final byte[] gs_aucChar118 = new byte[] 
        {
            0x00,0x00,0x00,0x18,0x18,0x24,0x24,0x24,
            0x42,0x42,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar118 = new BitmapFontChar(0,0,8,16,gs_aucChar118);

    private static final byte[] gs_aucChar119 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x11,0x00,
            0x11,0x00,0x2A,(byte)0x80,0x2A,(byte)0x80,0x2A,(byte)0x80,
            0x44,0x40,0x44,0x40,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar119 = new BitmapFontChar(0,0,11,16,gs_aucChar119);

    private static final byte[] gs_aucChar120 = new byte[] 
        {
            0x00,0x00,0x00,0x44,0x44,0x28,0x10,0x28,
            0x44,0x44,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar120 = new BitmapFontChar(0,0,7,16,gs_aucChar120);

    private static final byte[] gs_aucChar121 = new byte[] 
        {
            0x20,0x10,0x10,0x18,0x18,0x24,0x24,0x24,
            0x42,0x42,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar121 = new BitmapFontChar(0,0,8,16,gs_aucChar121);

    private static final byte[] gs_aucChar122 = new byte[] 
        {
            0x00,0x00,0x00,0x7C,0x40,0x20,0x10,0x08,
            0x04,0x7C,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar122 = new BitmapFontChar(0,0,7,16,gs_aucChar122);

    private static final byte[] gs_aucChar123 = new byte[] 
        {
            0x00,0x0C,0x10,0x10,0x10,0x10,0x10,0x60,
            0x10,0x10,0x10,0x10,0x0C,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar123 = new BitmapFontChar(0,0,8,16,gs_aucChar123);

    private static final byte[] gs_aucChar124 = new byte[] 
        {
            0x00,0x10,0x10,0x10,0x10,0x10,0x10,0x10,
            0x10,0x10,0x10,0x10,0x10,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar124 = new BitmapFontChar(0,0,7,16,gs_aucChar124);

    private static final byte[] gs_aucChar125 = new byte[] 
        {
            0x00,0x30,0x08,0x08,0x08,0x08,0x08,0x06,
            0x08,0x08,0x08,0x08,0x30,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar125 = new BitmapFontChar(0,0,8,16,gs_aucChar125);

    private static final byte[] gs_aucChar126 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x43,0x00,0x4C,(byte)0x80,
            0x30,(byte)0x80,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar126 = new BitmapFontChar(0,0,11,16,gs_aucChar126);

    private static final byte[] gs_aucChar127 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x7E,0x00,
            0x42,0x00,0x42,0x00,0x42,0x00,0x42,0x00,
            0x42,0x00,0x42,0x00,0x42,0x00,0x42,0x00,
            0x42,0x00,0x7E,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar127 = new BitmapFontChar(0,0,13,16,gs_aucChar127);

    private static final byte[] gs_aucChar128 = new byte[] 
        {
            0x00,0x00,0x00,0x1C,0x22,0x40,(byte)0xF0,0x40,
            (byte)0xF8,0x40,0x22,0x1C,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar128 = new BitmapFontChar(0,0,8,16,gs_aucChar128);

    private static final byte[] gs_aucChar129 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,(byte)0xE0,0x00,
            (byte)0xA0,0x00,(byte)0xA0,0x00,(byte)0xA0,0x00,(byte)0xA0,0x00,
            (byte)0xA0,0x00,(byte)0xA0,0x00,(byte)0xA0,0x00,(byte)0xA0,0x00,
            (byte)0xE0,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar129 = new BitmapFontChar(0,0,13,16,gs_aucChar129);

    private static final byte[] gs_aucChar130 = new byte[] 
        {
            0x00,(byte)0x80,0x40,0x40,0x40,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar130 = new BitmapFontChar(0,0,3,16,gs_aucChar130);

    private static final byte[] gs_aucChar131 = new byte[] 
        {
            0x00,0x00,(byte)0xC0,0x20,0x10,0x10,0x10,0x3E,
            0x08,0x08,0x08,0x07,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar131 = new BitmapFontChar(0,0,8,16,gs_aucChar131);

    private static final byte[] gs_aucChar132 = new byte[] 
        {
            0x00,(byte)0x90,0x48,0x48,0x48,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar132 = new BitmapFontChar(0,0,6,16,gs_aucChar132);

    private static final byte[] gs_aucChar133 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x44,0x40,
            0x44,0x40,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar133 = new BitmapFontChar(0,0,11,16,gs_aucChar133);

    private static final byte[] gs_aucChar134 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x08,0x08,0x08,0x08,
            0x08,0x08,0x7F,0x08,0x08,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar134 = new BitmapFontChar(0,0,8,16,gs_aucChar134);

    private static final byte[] gs_aucChar135 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x08,0x08,0x7F,0x08,
            0x08,0x08,0x7F,0x08,0x08,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar135 = new BitmapFontChar(0,0,8,16,gs_aucChar135);

    private static final byte[] gs_aucChar136 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x24,0x18,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar136 = new BitmapFontChar(0,0,8,16,gs_aucChar136);

    private static final byte[] gs_aucChar137 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x10,(byte)0xE3,(byte)0x80,0x09,0x14,0x40,0x05,
            0x14,0x40,0x05,0x14,0x40,0x3A,(byte)0xE3,(byte)0x80,
            0x45,0x00,0x00,0x45,0x00,0x00,0x44,(byte)0x80,
            0x00,0x38,0x40,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar137 = new BitmapFontChar(0,0,20,16,gs_aucChar137);

    private static final byte[] gs_aucChar138 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x3E,0x00,
            0x41,0x00,0x01,0x00,0x01,0x00,0x3E,0x00,
            0x40,0x00,0x40,0x00,0x41,0x00,0x3E,0x00,
            0x00,0x00,0x0C,0x00,0x12,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar138 = new BitmapFontChar(0,0,9,16,gs_aucChar138);

    private static final byte[] gs_aucChar139 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x10,0x20,0x40,0x40,
            0x20,0x10,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar139 = new BitmapFontChar(0,0,6,16,gs_aucChar139);

    private static final byte[] gs_aucChar140 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x1F,(byte)0xF8,
            0x21,0x00,0x41,0x00,0x41,0x00,0x41,(byte)0xF8,
            0x41,0x00,0x41,0x00,0x21,0x00,0x1F,(byte)0xF8,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar140 = new BitmapFontChar(0,0,14,16,gs_aucChar140);

    private static final byte[] gs_aucChar141 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,(byte)0xE0,0x00,
            (byte)0xA0,0x00,(byte)0xA0,0x00,(byte)0xA0,0x00,(byte)0xA0,0x00,
            (byte)0xA0,0x00,(byte)0xA0,0x00,(byte)0xA0,0x00,(byte)0xA0,0x00,
            (byte)0xE0,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar141 = new BitmapFontChar(0,0,13,16,gs_aucChar141);

    private static final byte[] gs_aucChar142 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x7F,0x00,
            0x40,0x00,0x20,0x00,0x10,0x00,0x08,0x00,
            0x04,0x00,0x02,0x00,0x01,0x00,0x7F,0x00,
            0x00,0x00,0x0C,0x00,0x12,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar142 = new BitmapFontChar(0,0,9,16,gs_aucChar142);

    private static final byte[] gs_aucChar143 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,(byte)0xE0,0x00,
            (byte)0xA0,0x00,(byte)0xA0,0x00,(byte)0xA0,0x00,(byte)0xA0,0x00,
            (byte)0xA0,0x00,(byte)0xA0,0x00,(byte)0xA0,0x00,(byte)0xA0,0x00,
            (byte)0xE0,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar143 = new BitmapFontChar(0,0,13,16,gs_aucChar143);

    private static final byte[] gs_aucChar144 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,(byte)0xE0,0x00,
            (byte)0xA0,0x00,(byte)0xA0,0x00,(byte)0xA0,0x00,(byte)0xA0,0x00,
            (byte)0xA0,0x00,(byte)0xA0,0x00,(byte)0xA0,0x00,(byte)0xA0,0x00,
            (byte)0xE0,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar144 = new BitmapFontChar(0,0,13,16,gs_aucChar144);

    private static final byte[] gs_aucChar145 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x20,0x40,0x40,0x40,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar145 = new BitmapFontChar(0,0,3,16,gs_aucChar145);

    private static final byte[] gs_aucChar146 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x40,0x20,0x20,0x20,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar146 = new BitmapFontChar(0,0,3,16,gs_aucChar146);

    private static final byte[] gs_aucChar147 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x24,0x48,0x48,0x48,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar147 = new BitmapFontChar(0,0,6,16,gs_aucChar147);

    private static final byte[] gs_aucChar148 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x48,0x24,0x24,0x24,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar148 = new BitmapFontChar(0,0,6,16,gs_aucChar148);

    private static final byte[] gs_aucChar149 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x38,0x7C,0x7C,
            0x7C,0x38,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar149 = new BitmapFontChar(0,0,7,16,gs_aucChar149);

    private static final byte[] gs_aucChar150 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x7E,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar150 = new BitmapFontChar(0,0,8,16,gs_aucChar150);

    private static final byte[] gs_aucChar151 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x7F,(byte)0xF0,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar151 = new BitmapFontChar(0,0,13,16,gs_aucChar151);

    private static final byte[] gs_aucChar152 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x2C,0x1A,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar152 = new BitmapFontChar(0,0,8,16,gs_aucChar152);

    private static final byte[] gs_aucChar153 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x12,0x20,0x12,(byte)0xA0,0x13,0x60,0x3A,0x20,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar153 = new BitmapFontChar(0,0,13,16,gs_aucChar153);

    private static final byte[] gs_aucChar154 = new byte[] 
        {
            0x00,0x00,0x00,0x78,0x04,0x04,0x38,0x40,
            0x40,0x3C,0x00,0x18,0x24,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar154 = new BitmapFontChar(0,0,7,16,gs_aucChar154);

    private static final byte[] gs_aucChar155 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x40,0x20,0x10,0x10,
            0x20,0x40,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar155 = new BitmapFontChar(0,0,6,16,gs_aucChar155);

    private static final byte[] gs_aucChar156 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x3D,(byte)0xE0,
            0x42,0x10,0x42,0x00,0x43,(byte)0xF0,0x42,0x10,
            0x42,0x10,0x3D,(byte)0xE0,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar156 = new BitmapFontChar(0,0,13,16,gs_aucChar156);

    private static final byte[] gs_aucChar157 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,(byte)0xE0,0x00,
            (byte)0xA0,0x00,(byte)0xA0,0x00,(byte)0xA0,0x00,(byte)0xA0,0x00,
            (byte)0xA0,0x00,(byte)0xA0,0x00,(byte)0xA0,0x00,(byte)0xA0,0x00,
            (byte)0xE0,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar157 = new BitmapFontChar(0,0,13,16,gs_aucChar157);

    private static final byte[] gs_aucChar158 = new byte[] 
        {
            0x00,0x00,0x00,0x7C,0x40,0x20,0x10,0x08,
            0x04,0x7C,0x00,0x18,0x24,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar158 = new BitmapFontChar(0,0,7,16,gs_aucChar158);

    private static final byte[] gs_aucChar159 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x08,0x00,
            0x08,0x00,0x08,0x00,0x08,0x00,0x14,0x00,
            0x14,0x00,0x22,0x00,0x22,0x00,0x41,0x00,
            0x00,0x00,0x22,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar159 = new BitmapFontChar(0,0,9,16,gs_aucChar159);

    private static final byte[] gs_aucChar160 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar160 = new BitmapFontChar(0,0,5,16,gs_aucChar160);

    private static final byte[] gs_aucChar161 = new byte[] 
        {
            0x00,0x00,0x00,0x20,0x20,0x20,0x20,0x20,
            0x20,0x00,0x20,0x20,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar161 = new BitmapFontChar(0,0,5,16,gs_aucChar161);

    private static final byte[] gs_aucChar162 = new byte[] 
        {
            0x00,0x08,0x08,0x3E,0x48,0x48,0x48,0x48,
            0x48,0x3E,0x08,0x08,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar162 = new BitmapFontChar(0,0,8,16,gs_aucChar162);

    private static final byte[] gs_aucChar163 = new byte[] 
        {
            0x00,0x00,0x00,0x7E,0x40,0x20,0x20,0x7C,
            0x20,0x20,0x22,0x1C,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar163 = new BitmapFontChar(0,0,8,16,gs_aucChar163);

    private static final byte[] gs_aucChar164 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x42,0x3C,0x24,0x24,
            0x3C,0x42,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar164 = new BitmapFontChar(0,0,8,16,gs_aucChar164);

    private static final byte[] gs_aucChar165 = new byte[] 
        {
            0x00,0x00,0x00,0x08,0x08,0x3E,0x08,0x08,
            0x14,0x22,0x22,0x41,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar165 = new BitmapFontChar(0,0,8,16,gs_aucChar165);

    private static final byte[] gs_aucChar166 = new byte[] 
        {
            0x00,0x10,0x10,0x10,0x10,0x10,0x00,0x00,
            0x10,0x10,0x10,0x10,0x10,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar166 = new BitmapFontChar(0,0,7,16,gs_aucChar166);

    private static final byte[] gs_aucChar167 = new byte[] 
        {
            0x00,0x3C,0x42,0x02,0x1C,0x22,0x42,0x44,
            0x38,0x40,0x42,0x3C,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar167 = new BitmapFontChar(0,0,8,16,gs_aucChar167);

    private static final byte[] gs_aucChar168 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x24,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar168 = new BitmapFontChar(0,0,8,16,gs_aucChar168);

    private static final byte[] gs_aucChar169 = new byte[] 
        {
            0x00,0x00,0x0F,(byte)0x80,0x10,0x40,0x27,0x20,
            0x48,(byte)0x90,0x48,0x10,0x48,0x10,0x48,0x10,
            0x48,(byte)0x90,0x27,0x20,0x10,0x40,0x0F,(byte)0x80,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar169 = new BitmapFontChar(0,0,13,16,gs_aucChar169);

    private static final byte[] gs_aucChar170 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x3C,0x44,
            0x44,0x3C,0x04,0x38,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar170 = new BitmapFontChar(0,0,7,16,gs_aucChar170);

    private static final byte[] gs_aucChar171 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x12,0x24,0x48,0x48,
            0x24,0x12,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar171 = new BitmapFontChar(0,0,8,16,gs_aucChar171);

    private static final byte[] gs_aucChar172 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x01,0x00,0x01,0x00,0x01,0x00,0x7F,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar172 = new BitmapFontChar(0,0,9,16,gs_aucChar172);

    private static final byte[] gs_aucChar173 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x7C,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar173 = new BitmapFontChar(0,0,7,16,gs_aucChar173);

    private static final byte[] gs_aucChar174 = new byte[] 
        {
            0x00,0x00,0x0F,(byte)0x80,0x10,0x40,0x24,0x60,
            0x44,(byte)0x90,0x45,0x10,0x47,0x10,0x44,(byte)0x90,
            0x44,(byte)0x90,0x27,0x20,0x10,0x40,0x0F,(byte)0x80,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar174 = new BitmapFontChar(0,0,13,16,gs_aucChar174);

    private static final byte[] gs_aucChar175 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,(byte)0xFF,0x00,0x00
        };
    private static BitmapFontChar gs_kChar175 = new BitmapFontChar(0,0,8,16,gs_aucChar175);

    private static final byte[] gs_aucChar176 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x38,
            0x44,0x44,0x44,0x38,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar176 = new BitmapFontChar(0,0,7,16,gs_aucChar176);

    private static final byte[] gs_aucChar177 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x7F,0x00,0x08,0x00,0x08,0x00,0x08,0x00,
            0x7F,0x00,0x08,0x00,0x08,0x00,0x08,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar177 = new BitmapFontChar(0,0,9,16,gs_aucChar177);

    private static final byte[] gs_aucChar178 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x78,0x20,
            0x10,0x08,0x48,0x30,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar178 = new BitmapFontChar(0,0,7,16,gs_aucChar178);

    private static final byte[] gs_aucChar179 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x78,0x04,
            0x04,0x38,0x04,0x78,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar179 = new BitmapFontChar(0,0,7,16,gs_aucChar179);

    private static final byte[] gs_aucChar180 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x10,0x08,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar180 = new BitmapFontChar(0,0,8,16,gs_aucChar180);

    private static final byte[] gs_aucChar181 = new byte[] 
        {
            0x40,0x40,0x40,0x7E,0x42,0x42,0x42,0x42,
            0x42,0x42,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar181 = new BitmapFontChar(0,0,8,16,gs_aucChar181);

    private static final byte[] gs_aucChar182 = new byte[] 
        {
            0x00,0x0A,0x0A,0x0A,0x0A,0x0A,0x0A,0x3A,
            0x7A,0x7A,0x7A,0x3E,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar182 = new BitmapFontChar(0,0,8,16,gs_aucChar182);

    private static final byte[] gs_aucChar183 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x20,0x20,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar183 = new BitmapFontChar(0,0,5,16,gs_aucChar183);

    private static final byte[] gs_aucChar184 = new byte[] 
        {
            0x30,0x08,0x08,0x00,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar184 = new BitmapFontChar(0,0,8,16,gs_aucChar184);

    private static final byte[] gs_aucChar185 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x38,0x10,
            0x10,0x10,0x30,0x10,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar185 = new BitmapFontChar(0,0,7,16,gs_aucChar185);

    private static final byte[] gs_aucChar186 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x38,0x44,
            0x44,0x44,0x44,0x38,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar186 = new BitmapFontChar(0,0,7,16,gs_aucChar186);

    private static final byte[] gs_aucChar187 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x48,0x24,0x12,0x12,
            0x24,0x48,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar187 = new BitmapFontChar(0,0,8,16,gs_aucChar187);

    private static final byte[] gs_aucChar188 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x10,0x20,
            0x09,(byte)0xF0,0x09,0x20,0x24,(byte)0xA0,0x22,0x60,
            0x22,0x20,0x21,0x00,0x61,0x00,0x20,(byte)0x80,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar188 = new BitmapFontChar(0,0,13,16,gs_aucChar188);

    private static final byte[] gs_aucChar189 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x10,(byte)0xF0,
            0x08,0x40,0x08,0x20,0x24,0x10,0x22,(byte)0x90,
            0x22,0x60,0x21,0x00,0x61,0x00,0x20,(byte)0x80,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar189 = new BitmapFontChar(0,0,13,16,gs_aucChar189);

    private static final byte[] gs_aucChar190 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x08,0x20,
            0x09,(byte)0xF0,0x05,0x20,0x74,(byte)0xA0,0x0A,0x60,
            0x09,0x20,0x31,0x00,0x08,(byte)0x80,0x70,(byte)0x80,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar190 = new BitmapFontChar(0,0,13,16,gs_aucChar190);

    private static final byte[] gs_aucChar191 = new byte[] 
        {
            0x00,0x00,0x00,0x38,0x44,0x40,0x20,0x10,
            0x10,0x00,0x10,0x10,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar191 = new BitmapFontChar(0,0,7,16,gs_aucChar191);

    private static final byte[] gs_aucChar192 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x41,0x00,
            0x41,0x00,0x3E,0x00,0x22,0x00,0x22,0x00,
            0x14,0x00,0x14,0x00,0x08,0x00,0x08,0x00,
            0x00,0x00,0x04,0x00,0x08,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar192 = new BitmapFontChar(0,0,9,16,gs_aucChar192);

    private static final byte[] gs_aucChar193 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x41,0x00,
            0x41,0x00,0x3E,0x00,0x22,0x00,0x22,0x00,
            0x14,0x00,0x14,0x00,0x08,0x00,0x08,0x00,
            0x00,0x00,0x08,0x00,0x04,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar193 = new BitmapFontChar(0,0,9,16,gs_aucChar193);

    private static final byte[] gs_aucChar194 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x41,0x00,
            0x41,0x00,0x3E,0x00,0x22,0x00,0x22,0x00,
            0x14,0x00,0x14,0x00,0x08,0x00,0x08,0x00,
            0x00,0x00,0x12,0x00,0x0C,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar194 = new BitmapFontChar(0,0,9,16,gs_aucChar194);

    private static final byte[] gs_aucChar195 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x41,0x00,
            0x41,0x00,0x3E,0x00,0x22,0x00,0x22,0x00,
            0x14,0x00,0x14,0x00,0x08,0x00,0x08,0x00,
            0x00,0x00,0x2C,0x00,0x1A,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar195 = new BitmapFontChar(0,0,9,16,gs_aucChar195);

    private static final byte[] gs_aucChar196 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x41,0x00,
            0x41,0x00,0x3E,0x00,0x22,0x00,0x22,0x00,
            0x14,0x00,0x14,0x00,0x08,0x00,0x08,0x00,
            0x00,0x00,0x22,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar196 = new BitmapFontChar(0,0,9,16,gs_aucChar196);

    private static final byte[] gs_aucChar197 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x41,0x00,
            0x41,0x00,0x3E,0x00,0x22,0x00,0x22,0x00,
            0x14,0x00,0x14,0x00,0x08,0x00,0x1C,0x00,
            0x22,0x00,0x22,0x00,0x1C,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar197 = new BitmapFontChar(0,0,9,16,gs_aucChar197);

    private static final byte[] gs_aucChar198 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x43,(byte)0xE0,
            0x42,0x00,0x42,0x00,0x3E,0x00,0x23,(byte)0xE0,
            0x12,0x00,0x12,0x00,0x0A,0x00,0x0F,(byte)0xE0,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar198 = new BitmapFontChar(0,0,12,16,gs_aucChar198);

    private static final byte[] gs_aucChar199 = new byte[] 
        {
            0x0C,0x00,0x02,0x00,0x02,0x00,0x1E,0x00,
            0x21,0x00,0x40,0x00,0x40,0x00,0x40,0x00,
            0x40,0x00,0x40,0x00,0x21,0x00,0x1E,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar199 = new BitmapFontChar(0,0,9,16,gs_aucChar199);

    private static final byte[] gs_aucChar200 = new byte[] 
        {
            0x00,0x00,0x00,0x7E,0x40,0x40,0x40,0x7E,
            0x40,0x40,0x40,0x7E,0x00,0x08,0x10,0x00
        };
    private static BitmapFontChar gs_kChar200 = new BitmapFontChar(0,0,8,16,gs_aucChar200);

    private static final byte[] gs_aucChar201 = new byte[] 
        {
            0x00,0x00,0x00,0x7E,0x40,0x40,0x40,0x7E,
            0x40,0x40,0x40,0x7E,0x00,0x10,0x08,0x00
        };
    private static BitmapFontChar gs_kChar201 = new BitmapFontChar(0,0,8,16,gs_aucChar201);

    private static final byte[] gs_aucChar202 = new byte[] 
        {
            0x00,0x00,0x00,0x7E,0x40,0x40,0x40,0x7E,
            0x40,0x40,0x40,0x7E,0x00,0x24,0x18,0x00
        };
    private static BitmapFontChar gs_kChar202 = new BitmapFontChar(0,0,8,16,gs_aucChar202);

    private static final byte[] gs_aucChar203 = new byte[] 
        {
            0x00,0x00,0x00,0x7E,0x40,0x40,0x40,0x7E,
            0x40,0x40,0x40,0x7E,0x00,0x24,0x00,0x00
        };
    private static BitmapFontChar gs_kChar203 = new BitmapFontChar(0,0,8,16,gs_aucChar203);

    private static final byte[] gs_aucChar204 = new byte[] 
        {
            0x00,0x00,0x00,0x70,0x20,0x20,0x20,0x20,
            0x20,0x20,0x20,0x70,0x00,0x10,0x20,0x00
        };
    private static BitmapFontChar gs_kChar204 = new BitmapFontChar(0,0,5,16,gs_aucChar204);

    private static final byte[] gs_aucChar205 = new byte[] 
        {
            0x00,0x00,0x00,0x70,0x20,0x20,0x20,0x20,
            0x20,0x20,0x20,0x70,0x00,0x20,0x10,0x00
        };
    private static BitmapFontChar gs_kChar205 = new BitmapFontChar(0,0,5,16,gs_aucChar205);

    private static final byte[] gs_aucChar206 = new byte[] 
        {
            0x00,0x00,0x00,0x70,0x20,0x20,0x20,0x20,
            0x20,0x20,0x20,0x70,0x00,0x48,0x30,0x00
        };
    private static BitmapFontChar gs_kChar206 = new BitmapFontChar(0,0,5,16,gs_aucChar206);

    private static final byte[] gs_aucChar207 = new byte[] 
        {
            0x00,0x00,0x00,0x70,0x20,0x20,0x20,0x20,
            0x20,0x20,0x20,0x70,0x00,(byte)0x88,0x00,0x00
        };
    private static BitmapFontChar gs_kChar207 = new BitmapFontChar(0,0,5,16,gs_aucChar207);

    private static final byte[] gs_aucChar208 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x7C,0x00,
            0x42,0x00,0x41,0x00,0x41,0x00,(byte)0xF1,0x00,
            0x41,0x00,0x41,0x00,0x42,0x00,0x7C,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar208 = new BitmapFontChar(0,0,9,16,gs_aucChar208);

    private static final byte[] gs_aucChar209 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x43,0x00,
            0x43,0x00,0x45,0x00,0x45,0x00,0x49,0x00,
            0x51,0x00,0x51,0x00,0x61,0x00,0x61,0x00,
            0x00,0x00,0x2C,0x00,0x1A,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar209 = new BitmapFontChar(0,0,9,16,gs_aucChar209);

    private static final byte[] gs_aucChar210 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x1E,0x00,
            0x21,0x00,0x40,(byte)0x80,0x40,(byte)0x80,0x40,(byte)0x80,
            0x40,(byte)0x80,0x40,(byte)0x80,0x21,0x00,0x1E,0x00,
            0x00,0x00,0x04,0x00,0x08,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar210 = new BitmapFontChar(0,0,10,16,gs_aucChar210);

    private static final byte[] gs_aucChar211 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x1E,0x00,
            0x21,0x00,0x40,(byte)0x80,0x40,(byte)0x80,0x40,(byte)0x80,
            0x40,(byte)0x80,0x40,(byte)0x80,0x21,0x00,0x1E,0x00,
            0x00,0x00,0x08,0x00,0x04,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar211 = new BitmapFontChar(0,0,10,16,gs_aucChar211);

    private static final byte[] gs_aucChar212 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x1E,0x00,
            0x21,0x00,0x40,(byte)0x80,0x40,(byte)0x80,0x40,(byte)0x80,
            0x40,(byte)0x80,0x40,(byte)0x80,0x21,0x00,0x1E,0x00,
            0x00,0x00,0x12,0x00,0x0C,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar212 = new BitmapFontChar(0,0,10,16,gs_aucChar212);

    private static final byte[] gs_aucChar213 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x1E,0x00,
            0x21,0x00,0x40,(byte)0x80,0x40,(byte)0x80,0x40,(byte)0x80,
            0x40,(byte)0x80,0x40,(byte)0x80,0x21,0x00,0x1E,0x00,
            0x00,0x00,0x16,0x00,0x0D,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar213 = new BitmapFontChar(0,0,10,16,gs_aucChar213);

    private static final byte[] gs_aucChar214 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x1E,0x00,
            0x21,0x00,0x40,(byte)0x80,0x40,(byte)0x80,0x40,(byte)0x80,
            0x40,(byte)0x80,0x40,(byte)0x80,0x21,0x00,0x1E,0x00,
            0x00,0x00,0x12,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar214 = new BitmapFontChar(0,0,10,16,gs_aucChar214);

    private static final byte[] gs_aucChar215 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x20,(byte)0x80,0x11,0x00,0x0A,0x00,0x04,0x00,
            0x0A,0x00,0x11,0x00,0x20,(byte)0x80,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar215 = new BitmapFontChar(0,0,11,16,gs_aucChar215);

    private static final byte[] gs_aucChar216 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x40,0x00,0x3E,0x00,
            0x21,0x00,0x50,(byte)0x80,0x48,(byte)0x80,0x48,(byte)0x80,
            0x44,(byte)0x80,0x42,(byte)0x80,0x21,0x00,0x1F,0x00,
            0x00,(byte)0x80,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar216 = new BitmapFontChar(0,0,10,16,gs_aucChar216);

    private static final byte[] gs_aucChar217 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x1C,0x00,
            0x22,0x00,0x41,0x00,0x41,0x00,0x41,0x00,
            0x41,0x00,0x41,0x00,0x41,0x00,0x41,0x00,
            0x00,0x00,0x04,0x00,0x08,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar217 = new BitmapFontChar(0,0,9,16,gs_aucChar217);

    private static final byte[] gs_aucChar218 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x1C,0x00,
            0x22,0x00,0x41,0x00,0x41,0x00,0x41,0x00,
            0x41,0x00,0x41,0x00,0x41,0x00,0x41,0x00,
            0x00,0x00,0x08,0x00,0x04,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar218 = new BitmapFontChar(0,0,9,16,gs_aucChar218);

    private static final byte[] gs_aucChar219 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x1C,0x00,
            0x22,0x00,0x41,0x00,0x41,0x00,0x41,0x00,
            0x41,0x00,0x41,0x00,0x41,0x00,0x41,0x00,
            0x00,0x00,0x12,0x00,0x0C,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar219 = new BitmapFontChar(0,0,9,16,gs_aucChar219);

    private static final byte[] gs_aucChar220 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x1C,0x00,
            0x22,0x00,0x41,0x00,0x41,0x00,0x41,0x00,
            0x41,0x00,0x41,0x00,0x41,0x00,0x41,0x00,
            0x00,0x00,0x22,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar220 = new BitmapFontChar(0,0,9,16,gs_aucChar220);

    private static final byte[] gs_aucChar221 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x08,0x00,
            0x08,0x00,0x08,0x00,0x08,0x00,0x14,0x00,
            0x14,0x00,0x22,0x00,0x22,0x00,0x41,0x00,
            0x00,0x00,0x08,0x00,0x04,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar221 = new BitmapFontChar(0,0,9,16,gs_aucChar221);

    private static final byte[] gs_aucChar222 = new byte[] 
        {
            0x00,0x00,0x00,0x40,0x40,0x7C,0x42,0x42,
            0x42,0x7C,0x40,0x40,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar222 = new BitmapFontChar(0,0,8,16,gs_aucChar222);

    private static final byte[] gs_aucChar223 = new byte[] 
        {
            0x00,0x00,0x00,0x5C,0x42,0x42,0x42,0x44,
            0x58,0x44,0x44,0x44,0x38,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar223 = new BitmapFontChar(0,0,8,16,gs_aucChar223);

    private static final byte[] gs_aucChar224 = new byte[] 
        {
            0x00,0x00,0x00,0x3E,0x42,0x42,0x3E,0x02,
            0x02,0x3C,0x00,0x08,0x10,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar224 = new BitmapFontChar(0,0,8,16,gs_aucChar224);

    private static final byte[] gs_aucChar225 = new byte[] 
        {
            0x00,0x00,0x00,0x3E,0x42,0x42,0x3E,0x02,
            0x02,0x3C,0x00,0x08,0x04,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar225 = new BitmapFontChar(0,0,8,16,gs_aucChar225);

    private static final byte[] gs_aucChar226 = new byte[] 
        {
            0x00,0x00,0x00,0x3E,0x42,0x42,0x3E,0x02,
            0x02,0x3C,0x00,0x12,0x0C,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar226 = new BitmapFontChar(0,0,8,16,gs_aucChar226);

    private static final byte[] gs_aucChar227 = new byte[] 
        {
            0x00,0x00,0x00,0x3E,0x42,0x42,0x3E,0x02,
            0x02,0x3C,0x00,0x2C,0x1A,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar227 = new BitmapFontChar(0,0,8,16,gs_aucChar227);

    private static final byte[] gs_aucChar228 = new byte[] 
        {
            0x00,0x00,0x00,0x3E,0x42,0x42,0x3E,0x02,
            0x02,0x3C,0x00,0x24,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar228 = new BitmapFontChar(0,0,8,16,gs_aucChar228);

    private static final byte[] gs_aucChar229 = new byte[] 
        {
            0x00,0x00,0x00,0x3E,0x42,0x42,0x3E,0x02,
            0x02,0x3C,0x00,0x0C,0x12,0x12,0x0C,0x00
        };
    private static BitmapFontChar gs_kChar229 = new BitmapFontChar(0,0,8,16,gs_aucChar229);

    private static final byte[] gs_aucChar230 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x3B,(byte)0x80,
            0x44,0x40,0x44,0x00,0x3F,(byte)0xC0,0x04,0x40,
            0x04,0x40,0x3B,(byte)0x80,0x00,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar230 = new BitmapFontChar(0,0,11,16,gs_aucChar230);

    private static final byte[] gs_aucChar231 = new byte[] 
        {
            0x18,0x04,0x04,0x3C,0x42,0x40,0x40,0x40,
            0x42,0x3C,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar231 = new BitmapFontChar(0,0,8,16,gs_aucChar231);

    private static final byte[] gs_aucChar232 = new byte[] 
        {
            0x00,0x00,0x00,0x3C,0x42,0x40,0x7E,0x42,
            0x42,0x3C,0x00,0x08,0x10,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar232 = new BitmapFontChar(0,0,8,16,gs_aucChar232);

    private static final byte[] gs_aucChar233 = new byte[] 
        {
            0x00,0x00,0x00,0x3C,0x42,0x40,0x7E,0x42,
            0x42,0x3C,0x00,0x08,0x04,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar233 = new BitmapFontChar(0,0,8,16,gs_aucChar233);

    private static final byte[] gs_aucChar234 = new byte[] 
        {
            0x00,0x00,0x00,0x3C,0x42,0x40,0x7E,0x42,
            0x42,0x3C,0x00,0x24,0x18,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar234 = new BitmapFontChar(0,0,8,16,gs_aucChar234);

    private static final byte[] gs_aucChar235 = new byte[] 
        {
            0x00,0x00,0x00,0x3C,0x42,0x40,0x7E,0x42,
            0x42,0x3C,0x00,0x24,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar235 = new BitmapFontChar(0,0,8,16,gs_aucChar235);

    private static final byte[] gs_aucChar236 = new byte[] 
        {
            0x00,0x00,0x00,0x40,0x40,0x40,0x40,0x40,
            0x40,0x40,0x00,0x20,0x40,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar236 = new BitmapFontChar(0,0,3,16,gs_aucChar236);

    private static final byte[] gs_aucChar237 = new byte[] 
        {
            0x00,0x00,0x00,0x40,0x40,0x40,0x40,0x40,
            0x40,0x40,0x00,0x40,0x20,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar237 = new BitmapFontChar(0,0,3,16,gs_aucChar237);

    private static final byte[] gs_aucChar238 = new byte[] 
        {
            0x00,0x00,0x00,0x40,0x40,0x40,0x40,0x40,
            0x40,0x40,0x00,(byte)0xA0,0x40,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar238 = new BitmapFontChar(0,0,3,16,gs_aucChar238);

    private static final byte[] gs_aucChar239 = new byte[] 
        {
            0x00,0x00,0x00,0x40,0x40,0x40,0x40,0x40,
            0x40,0x40,0x00,(byte)0xA0,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar239 = new BitmapFontChar(0,0,3,16,gs_aucChar239);

    private static final byte[] gs_aucChar240 = new byte[] 
        {
            0x00,0x00,0x00,0x3C,0x42,0x42,0x42,0x42,
            0x3E,0x02,0x34,0x08,0x14,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar240 = new BitmapFontChar(0,0,8,16,gs_aucChar240);

    private static final byte[] gs_aucChar241 = new byte[] 
        {
            0x00,0x00,0x00,0x42,0x42,0x42,0x42,0x42,
            0x62,0x5C,0x00,0x2C,0x1A,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar241 = new BitmapFontChar(0,0,8,16,gs_aucChar241);

    private static final byte[] gs_aucChar242 = new byte[] 
        {
            0x00,0x00,0x00,0x3C,0x42,0x42,0x42,0x42,
            0x42,0x3C,0x00,0x08,0x10,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar242 = new BitmapFontChar(0,0,8,16,gs_aucChar242);

    private static final byte[] gs_aucChar243 = new byte[] 
        {
            0x00,0x00,0x00,0x3C,0x42,0x42,0x42,0x42,
            0x42,0x3C,0x00,0x10,0x08,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar243 = new BitmapFontChar(0,0,8,16,gs_aucChar243);

    private static final byte[] gs_aucChar244 = new byte[] 
        {
            0x00,0x00,0x00,0x3C,0x42,0x42,0x42,0x42,
            0x42,0x3C,0x00,0x24,0x18,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar244 = new BitmapFontChar(0,0,8,16,gs_aucChar244);

    private static final byte[] gs_aucChar245 = new byte[] 
        {
            0x00,0x00,0x00,0x3C,0x42,0x42,0x42,0x42,
            0x42,0x3C,0x00,0x2C,0x1A,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar245 = new BitmapFontChar(0,0,8,16,gs_aucChar245);

    private static final byte[] gs_aucChar246 = new byte[] 
        {
            0x00,0x00,0x00,0x3C,0x42,0x42,0x42,0x42,
            0x42,0x3C,0x00,0x24,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar246 = new BitmapFontChar(0,0,8,16,gs_aucChar246);

    private static final byte[] gs_aucChar247 = new byte[] 
        {
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
            0x08,0x00,0x08,0x00,0x00,0x00,0x7F,0x00,
            0x00,0x00,0x08,0x00,0x08,0x00,0x00,0x00,
            0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar247 = new BitmapFontChar(0,0,9,16,gs_aucChar247);

    private static final byte[] gs_aucChar248 = new byte[] 
        {
            0x00,0x00,0x40,0x3C,0x62,0x52,0x52,0x4A,
            0x46,0x3C,0x02,0x00,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar248 = new BitmapFontChar(0,0,8,16,gs_aucChar248);

    private static final byte[] gs_aucChar249 = new byte[] 
        {
            0x00,0x00,0x00,0x3A,0x46,0x42,0x42,0x42,
            0x42,0x42,0x00,0x08,0x10,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar249 = new BitmapFontChar(0,0,8,16,gs_aucChar249);

    private static final byte[] gs_aucChar250 = new byte[] 
        {
            0x00,0x00,0x00,0x3A,0x46,0x42,0x42,0x42,
            0x42,0x42,0x00,0x10,0x08,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar250 = new BitmapFontChar(0,0,8,16,gs_aucChar250);

    private static final byte[] gs_aucChar251 = new byte[] 
        {
            0x00,0x00,0x00,0x3A,0x46,0x42,0x42,0x42,
            0x42,0x42,0x00,0x24,0x18,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar251 = new BitmapFontChar(0,0,8,16,gs_aucChar251);

    private static final byte[] gs_aucChar252 = new byte[] 
        {
            0x00,0x00,0x00,0x3A,0x46,0x42,0x42,0x42,
            0x42,0x42,0x00,0x24,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar252 = new BitmapFontChar(0,0,8,16,gs_aucChar252);

    private static final byte[] gs_aucChar253 = new byte[] 
        {
            0x20,0x10,0x10,0x18,0x18,0x24,0x24,0x24,
            0x42,0x42,0x00,0x08,0x04,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar253 = new BitmapFontChar(0,0,8,16,gs_aucChar253);

    private static final byte[] gs_aucChar254 = new byte[] 
        {
            0x40,0x40,0x40,0x7C,0x42,0x42,0x42,0x42,
            0x62,0x5C,0x40,0x40,0x40,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar254 = new BitmapFontChar(0,0,8,16,gs_aucChar254);

    private static final byte[] gs_aucChar255 = new byte[] 
        {
            0x20,0x10,0x10,0x18,0x18,0x24,0x24,0x24,
            0x42,0x42,0x00,0x24,0x00,0x00,0x00,0x00
        };
    private static BitmapFontChar gs_kChar255 = new BitmapFontChar(0,0,8,16,gs_aucChar255);

    private static BitmapFontChar[] gs_kChars = new BitmapFontChar[]
        {
            gs_kChar0,
            gs_kChar1,
            gs_kChar2,
            gs_kChar3,
            gs_kChar4,
            gs_kChar5,
            gs_kChar6,
            gs_kChar7,
            gs_kChar8,
            gs_kChar9,
            gs_kChar10,
            gs_kChar11,
            gs_kChar12,
            gs_kChar13,
            gs_kChar14,
            gs_kChar15,
            gs_kChar16,
            gs_kChar17,
            gs_kChar18,
            gs_kChar19,
            gs_kChar20,
            gs_kChar21,
            gs_kChar22,
            gs_kChar23,
            gs_kChar24,
            gs_kChar25,
            gs_kChar26,
            gs_kChar27,
            gs_kChar28,
            gs_kChar29,
            gs_kChar30,
            gs_kChar31,
            gs_kChar32,
            gs_kChar33,
            gs_kChar34,
            gs_kChar35,
            gs_kChar36,
            gs_kChar37,
            gs_kChar38,
            gs_kChar39,
            gs_kChar40,
            gs_kChar41,
            gs_kChar42,
            gs_kChar43,
            gs_kChar44,
            gs_kChar45,
            gs_kChar46,
            gs_kChar47,
            gs_kChar48,
            gs_kChar49,
            gs_kChar50,
            gs_kChar51,
            gs_kChar52,
            gs_kChar53,
            gs_kChar54,
            gs_kChar55,
            gs_kChar56,
            gs_kChar57,
            gs_kChar58,
            gs_kChar59,
            gs_kChar60,
            gs_kChar61,
            gs_kChar62,
            gs_kChar63,
            gs_kChar64,
            gs_kChar65,
            gs_kChar66,
            gs_kChar67,
            gs_kChar68,
            gs_kChar69,
            gs_kChar70,
            gs_kChar71,
            gs_kChar72,
            gs_kChar73,
            gs_kChar74,
            gs_kChar75,
            gs_kChar76,
            gs_kChar77,
            gs_kChar78,
            gs_kChar79,
            gs_kChar80,
            gs_kChar81,
            gs_kChar82,
            gs_kChar83,
            gs_kChar84,
            gs_kChar85,
            gs_kChar86,
            gs_kChar87,
            gs_kChar88,
            gs_kChar89,
            gs_kChar90,
            gs_kChar91,
            gs_kChar92,
            gs_kChar93,
            gs_kChar94,
            gs_kChar95,
            gs_kChar96,
            gs_kChar97,
            gs_kChar98,
            gs_kChar99,
            gs_kChar100,
            gs_kChar101,
            gs_kChar102,
            gs_kChar103,
            gs_kChar104,
            gs_kChar105,
            gs_kChar106,
            gs_kChar107,
            gs_kChar108,
            gs_kChar109,
            gs_kChar110,
            gs_kChar111,
            gs_kChar112,
            gs_kChar113,
            gs_kChar114,
            gs_kChar115,
            gs_kChar116,
            gs_kChar117,
            gs_kChar118,
            gs_kChar119,
            gs_kChar120,
            gs_kChar121,
            gs_kChar122,
            gs_kChar123,
            gs_kChar124,
            gs_kChar125,
            gs_kChar126,
            gs_kChar127,
            gs_kChar128,
            gs_kChar129,
            gs_kChar130,
            gs_kChar131,
            gs_kChar132,
            gs_kChar133,
            gs_kChar134,
            gs_kChar135,
            gs_kChar136,
            gs_kChar137,
            gs_kChar138,
            gs_kChar139,
            gs_kChar140,
            gs_kChar141,
            gs_kChar142,
            gs_kChar143,
            gs_kChar144,
            gs_kChar145,
            gs_kChar146,
            gs_kChar147,
            gs_kChar148,
            gs_kChar149,
            gs_kChar150,
            gs_kChar151,
            gs_kChar152,
            gs_kChar153,
            gs_kChar154,
            gs_kChar155,
            gs_kChar156,
            gs_kChar157,
            gs_kChar158,
            gs_kChar159,
            gs_kChar160,
            gs_kChar161,
            gs_kChar162,
            gs_kChar163,
            gs_kChar164,
            gs_kChar165,
            gs_kChar166,
            gs_kChar167,
            gs_kChar168,
            gs_kChar169,
            gs_kChar170,
            gs_kChar171,
            gs_kChar172,
            gs_kChar173,
            gs_kChar174,
            gs_kChar175,
            gs_kChar176,
            gs_kChar177,
            gs_kChar178,
            gs_kChar179,
            gs_kChar180,
            gs_kChar181,
            gs_kChar182,
            gs_kChar183,
            gs_kChar184,
            gs_kChar185,
            gs_kChar186,
            gs_kChar187,
            gs_kChar188,
            gs_kChar189,
            gs_kChar190,
            gs_kChar191,
            gs_kChar192,
            gs_kChar193,
            gs_kChar194,
            gs_kChar195,
            gs_kChar196,
            gs_kChar197,
            gs_kChar198,
            gs_kChar199,
            gs_kChar200,
            gs_kChar201,
            gs_kChar202,
            gs_kChar203,
            gs_kChar204,
            gs_kChar205,
            gs_kChar206,
            gs_kChar207,
            gs_kChar208,
            gs_kChar209,
            gs_kChar210,
            gs_kChar211,
            gs_kChar212,
            gs_kChar213,
            gs_kChar214,
            gs_kChar215,
            gs_kChar216,
            gs_kChar217,
            gs_kChar218,
            gs_kChar219,
            gs_kChar220,
            gs_kChar221,
            gs_kChar222,
            gs_kChar223,
            gs_kChar224,
            gs_kChar225,
            gs_kChar226,
            gs_kChar227,
            gs_kChar228,
            gs_kChar229,
            gs_kChar230,
            gs_kChar231,
            gs_kChar232,
            gs_kChar233,
            gs_kChar234,
            gs_kChar235,
            gs_kChar236,
            gs_kChar237,
            gs_kChar238,
            gs_kChar239,
            gs_kChar240,
            gs_kChar241,
            gs_kChar242,
            gs_kChar243,
            gs_kChar244,
            gs_kChar245,
            gs_kChar246,
            gs_kChar247,
            gs_kChar248,
            gs_kChar249,
            gs_kChar250,
            gs_kChar251,
            gs_kChar252,
            gs_kChar253,
            gs_kChar254,
            gs_kChar255
        };

    private final BitmapFont g_kVerdana_S16B0I0 = new BitmapFont("Verdana_S16B0I0",256,gs_kChars);
}
