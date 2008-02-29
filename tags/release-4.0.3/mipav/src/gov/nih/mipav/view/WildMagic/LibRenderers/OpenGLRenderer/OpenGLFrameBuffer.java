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

package gov.nih.mipav.view.WildMagic.LibRenderers.OpenGLRenderer;

import java.nio.Buffer;
import javax.media.opengl.*;

import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;


public class OpenGLFrameBuffer extends FrameBuffer
{
    /**
     * Creates a new OpenGLFrameBuffer
     * @param eFormat FormatType (NONE, RGB, RGBA, DEPTH)
     * @param eDepth DepthType (NONE, DEPTH_16, DEPTH_24, DEPTH_32)
     * @param eStencil StencilType (NONE, STENCIL_8)
     * @param eBuffering BufferingType (SINGLE, DOUBLE)
     * @param pkRenderer, the renderer object
     * @param pkTarget, the target for rendering to texture.
     * @param eMultisampling MultiSamplingType (NONE, SAMPLING_2, SAMPLING_4)
     */
    public OpenGLFrameBuffer (FormatType eFormat, DepthType eDepth,
                              StencilType eStencil, BufferingType eBuffering,
                              MultisamplingType eMultisampling, Renderer pkRenderer,
                              Texture pkTarget,GLAutoDrawable drawable)
    {
        super(eFormat,eDepth,eStencil,eBuffering,eMultisampling,pkRenderer,
              pkTarget);

        SetDrawable(drawable);
        if ((pkRenderer!=null) && (pkTarget!=null))
        {
            InitializeBuffer();
        }
    }

    /** Delete memory */
    public void dispose()
    {
        m_kDrawable = null;
        super.dispose();
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

    /**
     * Initializes the OpenGLFrameBuffer.
     * @return true on sucessful initialization, false otherwise.
     */
    public boolean InitializeBuffer ()
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return false; }
        GL gl = m_kDrawable.getGL();

        GraphicsImage pkImage = m_pkTarget.GetImage();
        if (pkImage.IsCubeImage())
        {
            // TO DO.  Cube map images not supported yet, but they should be.
            return false;
        }
        // create the texture ID
        ResourceIdentifier pkID = m_pkTarget.GetIdentifier(m_pkRenderer);
        assert(pkID != null);
        TextureID pkResource = (TextureID)pkID;
        m_uiTargetID = pkResource.ID;
        gl.glBindTexture(GL.GL_TEXTURE_2D,m_uiTargetID);

        // create a frame buffer
        int[] aiParams = new int[1];
        gl.glGenFramebuffersEXT(1,aiParams,0);
        m_uiFrameBufferID = aiParams[0];
        gl.glBindFramebufferEXT(GL.GL_FRAMEBUFFER_EXT,m_uiFrameBufferID);
        
        // Add depth to the frame buffer
        gl.glGenRenderbuffersEXT(1, aiParams, 0);
        int depthbuffer = aiParams[0];
        gl.glBindRenderbufferEXT(GL.GL_RENDERBUFFER_EXT, depthbuffer);
        gl.glRenderbufferStorageEXT(GL.GL_RENDERBUFFER_EXT, GL.GL_DEPTH_COMPONENT, pkImage.GetBound(0),pkImage.GetBound(1));

        // bind the frame buffer and the render buffer
        gl.glFramebufferRenderbufferEXT(GL.GL_FRAMEBUFFER_EXT, GL.GL_DEPTH_ATTACHMENT_EXT, GL.GL_RENDERBUFFER_EXT, depthbuffer);

        // bind the framebuffer/renderbuffer to the texture for render-to-texture
        if (!m_pkTarget.IsDepthTexture())
        {
            gl.glFramebufferTexture2DEXT(GL.GL_FRAMEBUFFER_EXT,GL.GL_COLOR_ATTACHMENT0_EXT,
                                      GL.GL_TEXTURE_2D,m_uiTargetID,0);
        }
        else
        {
            gl.glFramebufferTexture2DEXT(GL.GL_FRAMEBUFFER_EXT,GL.GL_DEPTH_ATTACHMENT_EXT,
                                      GL.GL_TEXTURE_2D,m_uiTargetID,0);
            gl.glDrawBuffer(GL.GL_NONE);
            gl.glReadBuffer(GL.GL_NONE);
        }

        int uiStatus = gl.glCheckFramebufferStatusEXT(GL.GL_FRAMEBUFFER_EXT);
        int iStopHere;
        switch (uiStatus)
        {
        case GL.GL_FRAMEBUFFER_COMPLETE_EXT:
            iStopHere = 0;
            gl.glBindTexture(GL.GL_TEXTURE_2D,0);
            gl.glBindFramebufferEXT(GL.GL_FRAMEBUFFER_EXT,0);
            return true;
        //case GL.GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENTS_EXT:
         //   iStopHere = 1;
        //    break;
        case GL.GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT:
            iStopHere = 2;
            break;
        case GL.GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT:
            iStopHere = 3;
            break;
        case GL.GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT:
            iStopHere = 4;
            break;
        case GL.GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT:
            iStopHere = 5;
            break;
        case GL.GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT:
            iStopHere = 6;
            break;
        case GL.GL_FRAMEBUFFER_UNSUPPORTED_EXT:
            iStopHere = 7;
            break;
        }
        assert(false);
        return false;
    }

    /**
     * Deletes the OpenGLFramebuffer and Renderbuffer
     */
    public void TerminateBuffer ()
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        int[] aiParams = new int[1];
        aiParams[0] = m_uiFrameBufferID;
        gl.glDeleteFramebuffersEXT(1,aiParams,0);
        if (m_pkTarget.IsDepthTexture())
        {
            aiParams[0] = m_uiDepthBufferID;
            gl.glDeleteRenderbuffersEXT(1,aiParams,0);
        }
    }

    /**
     * Enables the OpenGLFramebuffer
     */
    public void Enable ()
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        gl.glBindTexture(GL.GL_TEXTURE_2D,0);
        gl.glBindFramebufferEXT(GL.GL_FRAMEBUFFER_EXT,m_uiFrameBufferID);

        OpenGLRenderer pkRenderer = (OpenGLRenderer)m_pkRenderer;
        GraphicsImage pkImage = m_pkTarget.GetImage();
        gl.glViewport(0,0,pkImage.GetBound(0),pkImage.GetBound(1));
        pkRenderer.OnFrustumChange();
        pkRenderer.OnFrameChange();
    }

    /**
     * Disables the OpenGLFramebuffer
     */
    public void Disable ()
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        gl.glBindFramebufferEXT(GL.GL_FRAMEBUFFER_EXT,0);
        gl.glBindTexture(GL.GL_TEXTURE_2D,m_uiTargetID);

        OpenGLRenderer pkRenderer = (OpenGLRenderer)m_pkRenderer;
        pkRenderer.OnViewportChange();
        pkRenderer.OnFrustumChange();
        pkRenderer.OnFrameChange();
    }

    /**
     * Copies the frame buffer to texture.
     * @param bFillImage, when true copies the VRAM copy into system memory.
     */
    public void CopyToTexture (boolean bFillImage)
    {
        if ( m_kDrawable == null ) { System.err.println( "GLDrawable null" ); return; }
        GL gl = m_kDrawable.getGL();

        // TO DO.  Only the software renderer actually does something when
        // bFillImage is 'false'.  If that is not necessary, then CopyToTexture
        // should have no parameters and should be called only when you want to
        // copy the VRAM copy into system memory.

        if (bFillImage)
        {
            GraphicsImage pkImage = m_pkTarget.GetImage();
            Buffer aucData = pkImage.GetDataBuffer();
            int iBound0 = pkImage.GetBound(0);
            int iBound1 = pkImage.GetBound(1);

            switch (m_eFormat)
            {
            case FT_FORMAT_RGB:
                gl.glReadPixels(0,0,iBound0,iBound1,GL.GL_RGB,GL.GL_UNSIGNED_BYTE,aucData);
                break;

            case FT_FORMAT_RGBA:
                gl.glReadPixels(0,0,iBound0,iBound1,GL.GL_RGBA,GL.GL_UNSIGNED_BYTE,
                             aucData);
                break;

            case FT_FORMAT_DEPTH:
                gl.glReadPixels(0,0,iBound0,iBound1,GL.GL_DEPTH_COMPONENT,GL.GL_FLOAT,
                             aucData);
                break;

            default:
                assert(false);
                break;
            }
        }
    }

    /** OpenGL FrameBufferID: */
    protected int m_uiFrameBufferID;
    /** OpenGL DepthBufferID: */
    protected int m_uiDepthBufferID;
    /** OpenGL StencilBufferID: */
    protected int m_uiStencilBufferID;
    /** OpenGL TextureID: */
    protected int m_uiTargetID;
    /** JOGL GLAutoDrawable for access to JOGL function calls. */
    private GLAutoDrawable m_kDrawable = null;

}
