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

package gov.nih.mipav.view.WildMagic.LibApplications.OpenGLApplication;

import java.awt.event.*;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Shaders.*;

public abstract class JavaApplication extends Application
    implements KeyListener
{
    /**
     * Creates a new JavaApplication3.
     * @param acWindowTitle, the window title
     * @param iXPosition, window screen x-position
     * @param iYPosition, window screen y-position
     * @param iWidth, window width
     * @param iHeight, window height
     * @param rkBackgroundColor, background color
     */
    public JavaApplication (final String acWindowTitle, int iXPosition,
                            int iYPosition, int iWidth, int iHeight,
                            final ColorRGBA rkBackgroundColor)
    {
        m_acWindowTitle = new String(acWindowTitle);
        m_iXPosition = iXPosition;
        m_iYPosition = iYPosition;
        m_iWidth = iWidth;
        m_iHeight = iHeight;
        m_kBackgroundColor = new ColorRGBA(rkBackgroundColor);
    }

    /**
     * Retuns the window title.
     * @return the window title.
     */
    public String GetWindowTitle ()
    {
        return m_acWindowTitle;
    }

    /**
     * Retuns the window x-position.
     * @return the window x-position.
     */
    public int GetXPosition ()
    {
        return m_iXPosition;
    }

    /**
     * Retuns the window y-position.
     * @return the window y-position.
     */
    public int GetYPosition ()
    {
        return m_iYPosition;
    }

    /**
     * Retuns the window width.
     * @return the window width.
     */
    public int GetWidth ()
    {
        return m_iWidth;
    }

    /**
     * Retuns the window height.
     * @return the window height.
     */
    public int GetHeight ()
    {
        return m_iHeight;
    }

    /**
     * Set the Renderer object.
     * @param pkRenderer, the new renderer object.
     */
    public void SetRenderer (Renderer pkRenderer)
    {
        m_pkRenderer = pkRenderer;
    }

    /**
     * Retuns the Renderer object.
     * @return the Renderer object.
     */
    public Renderer GetRenderer () 
    {
        return m_pkRenderer;
    }

    /**
     * Set the window id.
     * @param iWindowID, the new window id.
     */
    public void SetWindowID (int iWindowID)
    {
        m_iWindowID = iWindowID;
    }

    /**
     * Retuns the window id.
     * @return the window id.
     */
    public int GetWindowID () 
    {
        return m_iWindowID;
    }

    /**
     * OnPrecreate call back, called before the application is created.
     * @return true
     */
    public boolean OnPrecreate ()
    {
        // stub for derived classes
        return true;
    }

    /**
     * OnInitialize call back, called when the application is created.
     * @return true
     */
    public boolean OnInitialize ()
    {
        m_pkRenderer.SetBackgroundColor(m_kBackgroundColor);

        // Notify the catalogs that the renderer has been created.
        assert(VertexProgramCatalog.GetActive() != null);
        VertexProgramCatalog.GetActive().
            SetInformation(m_pkRenderer.GetExtension(),
                           m_pkRenderer.GetCommentCharacter());

        assert(PixelProgramCatalog.GetActive() != null);
        PixelProgramCatalog.GetActive().
            SetInformation(m_pkRenderer.GetExtension(),
                           m_pkRenderer.GetCommentCharacter());
        return true;
    }

    /**
     * OnTerminate call back, called when the application is terminated.
     */
    public void OnTerminate ()
    {
        // Notify the catalogs that the renderer is going away, so it is okay to
        // delete the default programs.
        assert(VertexProgramCatalog.GetActive() != null);
        VertexProgramCatalog.GetActive().SetInformation("",(char)0);

        assert(PixelProgramCatalog.GetActive() != null);
        PixelProgramCatalog.GetActive().SetInformation("",(char)0);

        m_pkRenderer = null;
    }

    /**
     * OnMove call back, called when the application window is moved.
     * @param iX, new x-position
     * @param iY, new y-position
     */
    public void OnMove (int iX, int iY)
    {
        m_iXPosition = iX;
        m_iYPosition = iY;
    }

    /**
     * OnResize call back, called when the application window is resized
     * @param iWidth, the new window width
     * @param iHeight, the new window height
     */
    public void OnResize (int iWidth, int iHeight)
    {
        if (iWidth > 0 && iHeight > 0)
        {
            if (m_pkRenderer != null)
            {
                m_pkRenderer.Resize(iWidth,iHeight);
            }

            m_iWidth = iWidth;
            m_iHeight = iHeight;
        }
    }

    /**
     * OnDisplay call back, called when the application window is displayed
     */
    public void OnDisplay ()
    {
        // stub for derived classes
    }

    /**
     * OnIdle call back, called when the application is idle 
     */
    public void OnIdle ()
    {
        // stub for derived classes
    }

    /** constructor inputs */
    protected static String m_acWindowTitle;
    protected int m_iXPosition, m_iYPosition;
    protected static int m_iWidth, m_iHeight;
    protected ColorRGBA m_kBackgroundColor;

    /** An identifier for the window (representation is platform-specific). */
    protected int m_iWindowID = 0;

    /** The frame buffer parameters for the renderer creation.  You may set
     * these to values different than the defaults during your application
     * constructor call.
     * Default buffer parameters.  These may be overridden by the derived
     * class constructor.  Current hardware seems to pack a 24-bit depth
     * buffer and 8-bit stencil buffer into a 32-bit buffer, so I suggest
     * not overriding these.  Perhaps the only one you should is the
     * multisampling value.
     */
    protected FrameBuffer.FormatType m_eFormat = FrameBuffer.FormatType.FT_FORMAT_RGBA;
    protected FrameBuffer.DepthType m_eDepth = FrameBuffer.DepthType.DT_DEPTH_24;
    protected FrameBuffer.StencilType m_eStencil = FrameBuffer.StencilType.ST_STENCIL_8;
    protected FrameBuffer.BufferingType m_eBuffering = FrameBuffer.BufferingType.BT_BUFFERED_DOUBLE;
    protected FrameBuffer.MultisamplingType m_eMultisampling = FrameBuffer.MultisamplingType.MT_SAMPLING_NONE;

    /** The renderer (used for 2D and 3D applications). */
    protected gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.Renderer m_pkRenderer = null;

    /** KeyEvents: */
    public void keyPressed(KeyEvent e) {}
    public void keyReleased(KeyEvent arg0) { }
    public void keyTyped(KeyEvent arg0) { }
}
