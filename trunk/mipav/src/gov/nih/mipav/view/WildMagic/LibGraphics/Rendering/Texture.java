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

import java.util.HashMap;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Shaders.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;

public class Texture extends Bindable
    implements StreamInterface
{
    
    private static HashMap<Integer,FilterType> ms_pkFilterTypeMap = new HashMap<Integer,FilterType>();
    private static HashMap<Integer,WrapType> ms_pkWrapTypeMap = new HashMap<Integer,WrapType>();
    private static HashMap<Integer,DepthCompare> ms_pkDepthCompareMap = new HashMap<Integer,DepthCompare>();

    public enum FilterType
    {
        NEAREST ("NEAREST"),          // nearest neighbor
        LINEAR ("LINEAR"),           // linear filtering
        //mipmap filtering:
        NEAREST_NEAREST ("NEAREST_NEAREST"),  // nearest within image, nearest across images
        NEAREST_LINEAR ("NEAREST_LINEAR"),   // nearest within image, linear across images
        LINEAR_NEAREST ("LINEAR_NEAREST"),   // linear within image, nearest across images
        LINEAR_LINEAR ("LINEAR_LINEAR"),    // linear within image, linear across images
        MAX_FILTER_TYPES ("MAX_FILTER_TYPES");

        FilterType( String kName )
        {
            m_iValue = Init();
            m_kName = kName;
            ms_pkFilterTypeMap.put( m_iValue, this );
        }
        private int Init ()
        {
            return m_iInitValue++;
        }
        public int Value() { return m_iValue; }
        public String Name() { return m_kName; }
        private int m_iValue;
        private String m_kName;
        private static int m_iInitValue = 0;
    };

    public enum WrapType
    {
        CLAMP ("CLAMP"),
        REPEAT ("REPEAT"),
        MIRRORED_REPEAT ("MIRRORED_REPEAT"),
        CLAMP_BORDER ("CLAMP_BORDER"),
        CLAMP_EDGE ("CLAMP_EDGE"),
        MAX_WRAP_TYPES ("MAX_WRAP_TYPES");

        WrapType( String kName )
        {
            m_iValue = Init();
            m_kName = kName;
            ms_pkWrapTypeMap.put( m_iValue, this );
        }
        private int Init ()
        {
            return m_iInitValue++;
        }
        public int Value() { return m_iValue; }
        public String Name() { return m_kName; }
        private int m_iValue;
        private String m_kName;
        private static int m_iInitValue = 0;
    };

    public enum DepthCompare
    {
        DC_NEVER ("DC_NEVER"),
        DC_LESS ("DC_LESS"),
        DC_EQUAL ("DC_EQUAL"),
        DC_LEQUAL ("DC_LEQUAL"),
        DC_GREATER ("DC_GREATER"),
        DC_NOTEQUAL ("DC_NOTEQUAL"),
        DC_GEQUAL ("DC_GEQUAL"),
        DC_ALWAYS ("DC_ALWAYS"),
        DC_QUANTITY ("DC_QUANTITY");

        DepthCompare( String kName )
        {
            m_iValue = Init();
            m_kName = kName;
            ms_pkDepthCompareMap.put( m_iValue, this );
        }
        private int Init ()
        {
            return m_iInitValue++;
        }
        public int Value() { return m_iValue; }
        public String Name() { return m_kName; }
        private int m_iValue;
        private String m_kName;
        private static int m_iInitValue = 0;
    };

    private static FilterType ms_eFilterTypeStatic = FilterType.MAX_FILTER_TYPES;
    private static WrapType ms_eWrapTypeStatic = WrapType.MAX_WRAP_TYPES;
    private static DepthCompare ms_eDepthCompareStatic = DepthCompare.DC_QUANTITY;

    public Texture ()
    {
        m_spkImage = null;
        m_pkSamplerInformation = null;
        m_bSIOwner = false;
    }

    // Construction and destruction.
    public Texture ( GraphicsImage pkImage, SamplerInformation pkSInfo,
                    boolean bSIOwner)
    {
        m_spkImage = pkImage;
        m_pkSamplerInformation = pkSInfo;
        m_bSIOwner = bSIOwner;
    }

    public Texture (GraphicsImage pkDepthImage, DepthCompare eCompare)
    {
        m_spkImage = pkDepthImage;
        m_eCompare = eCompare;

        m_pkSamplerInformation =
            new SamplerInformation( pkDepthImage.GetName(), SamplerInformation.Type.SAMPLER_2D, 0 );
        
        m_bOffscreenTexture = false;
        m_bSIOwner = true;
    }

    public void finalize ()
    {
        if (m_bSIOwner)
        {
            m_pkSamplerInformation.finalize();
            m_pkSamplerInformation = null;
        }

        m_aeWType = null;
        if ( m_kBorderColor != null )
        {
            m_kBorderColor.finalize();
            m_kBorderColor = null;
        }
        
        // Inform all renderers using this texture that it is being destroyed.
        // This allows the renderer to free up any associated resources.
        Release();
        super.finalize();
    }


    // Access to texture images.
    public void SetImage (GraphicsImage pkImage)
    {
        m_spkImage = pkImage;
    }

    public GraphicsImage GetImage ()
    {
        return m_spkImage;
    }

    // Access to sampler information.
    public void SetSamplerInformation (SamplerInformation pkSamplerInformation)
    {
        m_pkSamplerInformation = pkSamplerInformation;
    }

    public SamplerInformation GetSamplerInformation ()
    {
        return m_pkSamplerInformation;
    }

    // Access to filter modes.  The default is LINEAR.
    public void SetFilterType (FilterType eFType)
    {
        m_eFType = eFType;
    }

    public FilterType GetFilterType ()
    {
        return m_eFType;
    }


    // Access to wrap modes.  The input i to SetWrapType and GetWrapType must
    // satisfy 0 <= i < GetDimension().  The defaults are CLAMP_TO_EDGE.
    public void SetWrapType (int i, WrapType eWType)
    {
        assert(0 <= i && i < 3);
        m_aeWType[i] = eWType;
    }

    public WrapType GetWrapType (int i)
    {
        assert(0 <= i && i < 3);
        return m_aeWType[i];
    }


    // Access to the border color used for sampling outside the texture image.
    // The default is ColorRGBA(0,0,0,1).
    public void SetBorderColor (ColorRGBA rkBorderColor)
    {
        m_kBorderColor = rkBorderColor;
    }

    public ColorRGBA GetBorderColor ()
    {
        return m_kBorderColor;
    }


    // Support for depth textures.
    public boolean IsDepthTexture ()
    {
        return m_eCompare != DepthCompare.DC_QUANTITY;
    }

    public void SetDepthCompare (DepthCompare eCompare)
    {
        m_eCompare = eCompare;
    }


    public DepthCompare GetDepthCompare ()
    {
        return m_eCompare;
    }


    // Support for offscreen textures.
    public boolean IsOffscreenTexture ()
    {
        return m_bOffscreenTexture;
    }

    public void SetOffscreenTexture (boolean bOffscreenTexture)
    {
        m_bOffscreenTexture = bOffscreenTexture;
    }
        
    protected GraphicsImage m_spkImage;
    protected SamplerInformation m_pkSamplerInformation;
    protected FilterType m_eFType = FilterType.LINEAR;
    protected WrapType[] m_aeWType = new WrapType[]{ WrapType.CLAMP_EDGE, WrapType.CLAMP_EDGE, WrapType.CLAMP_EDGE };
    protected ColorRGBA m_kBorderColor = new ColorRGBA(ColorRGBA.BLACK);

    // Support for depth textures.  The default value is DC_QUANTITY,
    // indicating that the texture is *not* a depth texture.
    protected DepthCompare m_eCompare = DepthCompare.DC_QUANTITY;

    // Support for offscreen textures.  The default value is 'false'.
    protected boolean m_bOffscreenTexture = false;

    // If this object is the owner of m_pkSamplerInformation, the object was
    // dynamically allocated and must be deallocated during destruction.
    protected boolean m_bSIOwner;

    protected boolean m_bReload = false;
    public void Reload( boolean bReload )
    {
        m_bReload = bReload;
    }
    
    public boolean Reload()
    {
        return m_bReload;
    }
    
    public void Load (Stream rkStream, Stream.Link pkLink)
    {
        super.Load(rkStream,pkLink);

        int iFType = rkStream.ReadInt();
        m_eFType = ms_pkFilterTypeMap.get(iFType);
        for (int i = 0; i < 3; i++)
        {
            int iWType = rkStream.ReadInt();
            m_aeWType[i] = ms_pkWrapTypeMap.get(iWType);
        }
        rkStream.Read(m_kBorderColor);
        int iCompare = rkStream.ReadInt();
        m_eCompare = ms_pkDepthCompareMap.get(iCompare);
        m_bOffscreenTexture = rkStream.ReadBoolean();

        // The data members m_spkImage, m_pkSamplerInformation, and m_bSIOwner
        // are all set during resource loading at program runtime.
    }

    public void Link (Stream rkStream, Stream.Link pkLink)
    {
        super.Link(rkStream,pkLink);
    }

    public boolean Register (Stream rkStream)
    {
        return super.Register(rkStream);
    }

    public void Save (Stream rkStream)
    {
        super.Save(rkStream);

        rkStream.Write(m_eFType.Value());
        for (int i = 0; i < 3; i++)
        {
            rkStream.Write(m_aeWType[i].Value());
        }
        rkStream.Write(m_kBorderColor);
        rkStream.Write(m_eCompare.Value());
        rkStream.Write(m_bOffscreenTexture);

        // The data members m_spkImage, m_pkSamplerInformation, and m_bSIOwner
        // are all set during resource loading at program runtime.
    }

    public int GetDiskUsed (StreamVersion rkVersion) 
    {
        return super.GetDiskUsed(rkVersion) +
            Stream.SIZEOF_INT + //sizeof(int) + // m_eFType
            3 * Stream.SIZEOF_INT + //3*sizeof(int) + // m_aeWType[]
            4 * Stream.SIZEOF_FLOAT + //sizeof(m_kBorderColor) +
            Stream.SIZEOF_INT + //sizeof(int) + // m_eCompare
            Stream.SIZEOF_BOOLEAN; //sizeof(char);  // m_bOffscreenTexture
    }

    public StringTree SaveStrings (final String acTitle)
    {
        StringTree pkTree = new StringTree();

        // strings
        pkTree.Append(StringTree.Format("Texture",GetName()));
        pkTree.Append(super.SaveStrings(null));
        pkTree.Append(StringTree.Format("filter = ",m_eFType.Name()));

        for (int i = 0; i < 3; i++)
        {
            String kPrefix = new String("wrap[" + i + "] =");
            pkTree.Append(StringTree.Format(kPrefix,m_aeWType[i].Name()));
        }

        pkTree.Append(StringTree.Format("border color =",m_kBorderColor));
        if (m_eCompare != DepthCompare.DC_QUANTITY)
        {
            pkTree.Append(StringTree.Format("depth compare = ",
                                            m_eCompare.Name()));
        }
        pkTree.Append(StringTree.Format("offscreen =",m_bOffscreenTexture));
        return pkTree;
    }
}
