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

public abstract class FrameBuffer
{
    /** Frame buffer format type: */
    public enum FormatType
    {
        FT_FORMAT_NONE,
        FT_FORMAT_RGB,
        FT_FORMAT_RGBA,
        FT_FORMAT_DEPTH
    };

    /** Frame buffer depth type: */
    public enum DepthType
    {
        DT_DEPTH_NONE,
        DT_DEPTH_16,
        DT_DEPTH_24,
        DT_DEPTH_32
    };

    /** Frame buffer stencil type: */
    public enum StencilType
    {
        ST_STENCIL_NONE,
        ST_STENCIL_8
    };

    /** Frame buffer buffering type: */
    public enum BufferingType
    {
        BT_BUFFERED_SINGLE,
        BT_BUFFERED_DOUBLE
    };

    /** Frame buffer multi-sampling type: */
    public enum MultisamplingType
    {
        MT_SAMPLING_NONE,
        MT_SAMPLING_2,
        MT_SAMPLING_4
    };

    /** Create a new FrameBuffer
     * @param eFormat, format type
     * @param eDepth, depth type
     * @param eStencil, stencil type
     * @param eBuffering, buffering type
     * @param eMultisampling, multi-sampling type
     * @param pkRenderer, Renderer object.
     * @param pkTarget, Texture target, when render-to-texture.
     */
    public FrameBuffer (FormatType eFormat, DepthType eDepth,
                        StencilType eStencil, BufferingType eBuffering,
                        MultisamplingType eMultisampling, Renderer pkRenderer, Texture pkTarget)
    {
        m_eFormat = eFormat;
        m_eDepth = eDepth;
        m_eStencil = eStencil;
        m_eBuffering = eBuffering;
        m_eMultisampling = eMultisampling;
        m_pkRenderer = pkRenderer;
        m_pkTarget = pkTarget;
    }

    /** Delete memory. */
    public void finalize()
    {
        m_pkRenderer = null;
        m_pkTarget = null;
    }

    /** Get format type.
     * @return format type. */
    public final FormatType GetFormatType ()
    {
        return m_eFormat;
    }

    /** Get depth type.
     * @return depth type. */
    public final DepthType GetDepthType ()
    {
        return m_eDepth;
    }

    /** Get stencil type.
     * @return stencil type. */
    public final StencilType GetStencilType ()
    {
        return m_eStencil;
    }

    /** Get buffering type.
     * @return buffering type. */
    public final BufferingType GetBufferingType ()
    {
        return m_eBuffering;
    }

    /** Get multi-sampling type.
     * @return multi-sampling type. */
    public final MultisamplingType GetMultisamplingType ()
    {
        return m_eMultisampling;
    }

    /** Get Renderer.
     * @return Renderer. */
    public final Renderer GetRenderer ()
    {
        return m_pkRenderer;
    }

    /** Get Texture target.
     * @return texture target. */
    public final Texture GetTarget ()
    {
        return m_pkTarget;
    }

    /** Enable frame buffer: */
    public abstract void Enable ();
    /** Disable frame buffer: */
    public abstract void Disable ();

    /** Copy the data from the frame buffer object to the texture object.
     * @param bFillImage, If you want the Image object filled with the values,
     * set the input to 'true'.
     */
    public abstract void CopyToTexture (boolean bFillImage);

    /** FrameBuffer format type: */
    protected FormatType m_eFormat;
    /** FrameBuffer depth type: */
    protected DepthType m_eDepth;
    /** FrameBuffer stencil type: */
    protected StencilType m_eStencil;
    /** FrameBuffer buffering type: */
    protected BufferingType m_eBuffering;
    /** FrameBuffer multi-sampling type: */
    protected MultisamplingType m_eMultisampling;
    /** FrameBuffer Renderer: */
    protected Renderer m_pkRenderer;
    /** FrameBuffer Texture target when render-to-texture: */
    protected Texture m_pkTarget;
}
