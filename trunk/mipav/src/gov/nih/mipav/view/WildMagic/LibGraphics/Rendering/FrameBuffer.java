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

package gov.nih.mipav.view.WildMagic.LibGraphics.Rendering;

public abstract class FrameBuffer
{
    
    public enum FormatType
    {
        FT_FORMAT_NONE,
        FT_FORMAT_RGB,
        FT_FORMAT_RGBA,
        FT_FORMAT_DEPTH
    };

    public enum DepthType
    {
        DT_DEPTH_NONE,
        DT_DEPTH_16,
        DT_DEPTH_24,
        DT_DEPTH_32
    };

    public enum StencilType
    {
        ST_STENCIL_NONE,
        ST_STENCIL_8
    };

    public enum BufferingType
    {
        BT_BUFFERED_SINGLE,
        BT_BUFFERED_DOUBLE
    };

    public enum MultisamplingType
    {
        MT_SAMPLING_NONE,
        MT_SAMPLING_2,
        MT_SAMPLING_4
    };

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

    // Member access.
    public FormatType GetFormatType ()
    {
        return m_eFormat;
    }


    public DepthType GetDepthType ()
    {
        return m_eDepth;
    }

    public StencilType GetStencilType ()
    {
        return m_eStencil;
    }

    public BufferingType GetBufferingType ()
    {
        return m_eBuffering;
    }

    public MultisamplingType GetMultisamplingType ()
    {
        return m_eMultisampling;
    }

    public Renderer GetRenderer ()
    {
        return m_pkRenderer;
    }

    public Texture GetTarget ()
    {
        return m_pkTarget;
    }


    public abstract void Enable ();
    public abstract void Disable ();

    // Copy the data from the frame buffer object to the texture object.
    // If you want the Image object filled with the values, set the input
    // to 'true'.
    public abstract void CopyToTexture (boolean bFillImage);

    protected FormatType m_eFormat;
    protected DepthType m_eDepth;
    protected StencilType m_eStencil;
    protected BufferingType m_eBuffering;
    protected MultisamplingType m_eMultisampling;
    protected Renderer m_pkRenderer;
    protected Texture m_pkTarget;
}
