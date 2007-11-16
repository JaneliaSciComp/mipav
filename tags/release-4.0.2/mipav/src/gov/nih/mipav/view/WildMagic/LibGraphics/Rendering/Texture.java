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
    
    /** Maps filter type enum to int values. */
    private static HashMap<Integer,FilterType> ms_pkFilterTypeMap = new HashMap<Integer,FilterType>();
    /** Maps wrap type enum to int values. */
    private static HashMap<Integer,WrapType> ms_pkWrapTypeMap = new HashMap<Integer,WrapType>();
    /** Maps depth compare enum to int values. */
    private static HashMap<Integer,DepthCompare> ms_pkDepthCompareMap = new HashMap<Integer,DepthCompare>();

    /** Texture filter type: */
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

    /** Texture wrap type: */
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

    /** Texture depth comparison types: */
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

    /** Initializes static enum for filter type. */
    private static FilterType ms_eFilterTypeStatic = FilterType.MAX_FILTER_TYPES;
    /** Initializes static enum for wrap type. */
    private static WrapType ms_eWrapTypeStatic = WrapType.MAX_WRAP_TYPES;
    /** Initializes static enum for depth compare. */
    private static DepthCompare ms_eDepthCompareStatic = DepthCompare.DC_QUANTITY;

    /** Default constructor. */
    public Texture ()
    {
        m_spkImage = null;
        m_pkSamplerInformation = null;
        m_bSIOwner = false;
    }

    /** Construct a Texture object with the given GraphicsImage,
     * SamplerInformation and sampler information owner flag.
     * @param pkImage, GraphicsImage associated with this Texture.
     * @param mpkSInfo, SamplerInformation for shader programs.
     * @param bSIOwner, sampler information owner flag.
     */
    public Texture ( GraphicsImage pkImage, SamplerInformation pkSInfo,
                    boolean bSIOwner)
    {
        m_spkImage = pkImage;
        m_pkSamplerInformation = pkSInfo;
        m_bSIOwner = bSIOwner;
    }

    /** Construct a texture with the depth image and depth compare type.
     * @param pkDepthImage, depth image for this texture.
     * @param eCompare, depth comparison type.
     */
    public Texture (GraphicsImage pkDepthImage, DepthCompare eCompare)
    {
        m_spkImage = pkDepthImage;
        m_eCompare = eCompare;

        m_pkSamplerInformation =
            new SamplerInformation( pkDepthImage.GetName(), SamplerInformation.Type.SAMPLER_2D, 0 );
        
        m_bOffscreenTexture = false;
        m_bSIOwner = true;
    }

    /** Release memory. */
    public void dispose ()
    {
        if (m_bSIOwner)
        {
            m_pkSamplerInformation.dispose();
            m_pkSamplerInformation = null;
        }

        m_aeWType = null;
        if ( m_kBorderColor != null )
        {
            m_kBorderColor.dispose();
            m_kBorderColor = null;
        }
        
        // Inform all renderers using this texture that it is being destroyed.
        // This allows the renderer to free up any associated resources.
        Release();
        super.dispose();
    }


    /** Set texture image.
     * @param pkImage, new GraphicsImage for this Texture.
     */
    public void SetImage (GraphicsImage pkImage)
    {
        m_spkImage = pkImage;
    }

    /** Get texture image.
     * @return GraphicsImage for this Texture.
     */
    public final GraphicsImage GetImage ()
    {
        return m_spkImage;
    }

    /** Set sampler information.
     * @param pkSamplerInformation, new sampler information object.
     */
    public void SetSamplerInformation (SamplerInformation pkSamplerInformation)
    {
        m_pkSamplerInformation = pkSamplerInformation;
    }

    /** Get sampler information.
     * @return sampler information object.
     */
    public final SamplerInformation GetSamplerInformation ()
    {
        return m_pkSamplerInformation;
    }

    /** Set filter modes.  The default is LINEAR.
     * @param eFType, new filter type.
     */
    public void SetFilterType (FilterType eFType)
    {
        m_eFType = eFType;
    }

    /** Fet filter modes.  The default is LINEAR.
     * @return filter type.
     */
    public final FilterType GetFilterType ()
    {
        return m_eFType;
    }


    /** Set to wrap modes. 
     * @param i, must satisfy 0 <= i < GetDimension(). 
     * @param eWType, new wrap type.  The defaults are CLAMP_TO_EDGE. */
    public void SetWrapType (int i, WrapType eWType)
    {
        assert(0 <= i && i < 3);
        m_aeWType[i] = eWType;
    }

    /** Get to wrap modes. 
     * @param i, must satisfy 0 <= i < GetDimension(). 
     * @return wrap type.  The defaults are CLAMP_TO_EDGE. */
    public WrapType GetWrapType (int i)
    {
        assert(0 <= i && i < 3);
        return m_aeWType[i];
    }


    /** Access to the border color used for sampling outside the texture image.
     * The default is ColorRGBA(0,0,0,1).
     * @param rkBorderColor, new border color.
     */
    public void SetBorderColor (ColorRGBA rkBorderColor)
    {
        m_kBorderColor = rkBorderColor;
    }

    /** Access to the border color used for sampling outside the texture image.
     * The default is ColorRGBA(0,0,0,1).
     * @return border color.
     */
    public final ColorRGBA GetBorderColor ()
    {
        return m_kBorderColor;
    }


    /** Support for depth textures.
     * @return true if this is a depth texture.
     */
    public final boolean IsDepthTexture ()
    {
        return (m_eCompare != DepthCompare.DC_QUANTITY);
    }

    /** Set depth compare type.
     * @param eCompare, new depth compare type.
     */
    public void SetDepthCompare (DepthCompare eCompare)
    {
        m_eCompare = eCompare;
    }

    /** Get depth compare type.
     * @return depth compare type.
     */
    public final DepthCompare GetDepthCompare ()
    {
        return m_eCompare;
    }

    /** Support for offscreen textures.
     * @return true it this is an offscreen texture.
     */
    public final boolean IsOffscreenTexture ()
    {
        return m_bOffscreenTexture;
    }

    /** Set offscreen texture flag.
     * @param bOffscreenTexture true sets this to an offscreen texture.
     */
    public void SetOffscreenTexture (boolean bOffscreenTexture)
    {
        m_bOffscreenTexture = bOffscreenTexture;
    }
        
    /** GraphicsImage texture data. */
    protected GraphicsImage m_spkImage;
    /** SamplerInformation for shader programs. */
    protected SamplerInformation m_pkSamplerInformation;
    /** Default filter type */
    protected FilterType m_eFType = FilterType.LINEAR;
    /** Default wrap types. */
    protected WrapType[] m_aeWType = new WrapType[]{ WrapType.CLAMP_EDGE, WrapType.CLAMP_EDGE, WrapType.CLAMP_EDGE };
    /** Default border color. */
    protected ColorRGBA m_kBorderColor = new ColorRGBA(ColorRGBA.BLACK);

    /** Support for depth textures.  The default value is DC_QUANTITY,
     * indicating that the texture is *not* a depth texture. */
    protected DepthCompare m_eCompare = DepthCompare.DC_QUANTITY;

    /** Support for offscreen textures.  The default value is 'false'. */
    protected boolean m_bOffscreenTexture = false;

    /** If this object is the owner of m_pkSamplerInformation, the object was
     * dynamically allocated and must be deallocated during destruction. */
    protected boolean m_bSIOwner;

    /** Set to true to reload the texture data to the GPU */
    protected boolean m_bReload = false;

    /** Set to true to reload the texture data to the GPU
     * @param bReload set to true to reload the texture data to the GPU
     */
    public void Reload( boolean bReload )
    {
        m_bReload = bReload;
    }
    
    /** Get reload flag.
     * @return reload flag.
     */
    public boolean Reload()
    {
        return m_bReload;
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

    /**
     * Write this object and all it's children to the Stream.
     * @param rkStream, the Stream where the child objects are stored.
     */
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

    /**
     * Returns the size of this object and it's children on disk for the
     * current StreamVersion parameter.
     * @param rkVersion, the current version of the Stream file being created.
     * @return the size of this object on disk.
     */
    public int GetDiskUsed (StreamVersion rkVersion) 
    {
        return super.GetDiskUsed(rkVersion) +
            Stream.SIZEOF_INT + //sizeof(int) + // m_eFType
            3 * Stream.SIZEOF_INT + //3*sizeof(int) + // m_aeWType[]
            4 * Stream.SIZEOF_FLOAT + //sizeof(m_kBorderColor) +
            Stream.SIZEOF_INT + //sizeof(int) + // m_eCompare
            Stream.SIZEOF_BOOLEAN; //sizeof(char);  // m_bOffscreenTexture
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
