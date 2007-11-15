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

package gov.nih.mipav.view.WildMagic.LibGraphics.Effects;

import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Shaders.*;

public class MultitextureEffect extends ShaderEffect
    implements StreamInterface
{
    /**
     * Creates a new MultitextureEffect with the number of Textures specified
     * by iTextureQuantity
     * @param iTextureQuantity, the number of textures in this
     * MultitextureEffect.
     */
    public MultitextureEffect (int iTextureQuantity)
    {
        super(1);
        m_akImageName = null;
        SetTextureQuantity(iTextureQuantity);
    }

    /** Delete memory */
    public void dispose()
    {
        for ( int i = 0; i < m_iTextureQuantity; i++ )
        {
            m_akImageName[i] = null;
        }
        m_akImageName = null;
        super.dispose();
    }

    /** Selection of the textures to be used by the effect.  The first call
     * should be SetTextureQuantity for the desired number of textures.  For
     * each texture, specify its image with SetImageName.  Texture 0 is
     * used as is (replace mode).  Texture i is blended with texture i-1 (for
     * i > 0) according to the modes specified by alpha state i to
     * produce the current colors.  After setting all the image names and all
     * the blending modes, call Configure() to activate the correct shader
     * program for the current set of textures.
     * @param iTextureQuantity, the number of textures in this
     * MultitextureEffect.
     */
    public void SetTextureQuantity (int iTextureQuantity)
    {
        assert(iTextureQuantity >= 1);
        m_iTextureQuantity = iTextureQuantity;
        m_akImageName = null;
        m_akImageName = new String[m_iTextureQuantity];

        m_kAlphaState.setSize(m_iTextureQuantity);
        SetDefaultAlphaState();
    }

    /** Returns the number of textures in this MultitextureEffect.
     * @return the number of textures in this MultitextureEffect.
     */
    public int GetTextureQuantity ()
    {
        return m_iTextureQuantity;
    }

    /** Selection of the textures to be used by the effect.  The first call
     * should be SetTextureQuantity for the desired number of textures.  For
     * each texture, specify its image with SetImageName.  Texture 0 is
     * used as is (replace mode).  Texture i is blended with texture i-1 (for
     * i > 0) according to the modes specified by alpha state i to
     * produce the current colors.  After setting all the image names and all
     * the blending modes, call Configure() to activate the correct shader
     * program for the current set of textures.
     * @param i, the Texture for the rkImageName
     * @param rkImageName, the name of the Texture file
     */
    public void SetImageName (int i, final String rkImageName)
    {
        assert(0 <= i && i < m_iTextureQuantity);
        m_akImageName[i] = new String(rkImageName);
    }

    /**
     * Returns the name of the ith Texture
     * @param i, the Texture
     * @return the name of the ith Texture file
     */
    public String GetImageName (int i)
    {
        assert(0 <= i && i < m_iTextureQuantity);
        return m_akImageName[i];
    }

    /**
     * After setting all the image names and all the blending modes, call
     * Configure() to activate the correct shader program for the current set
     * of textures.  */
    public void Configure ()
    {
        if (m_iTextureQuantity == 1)
        {
            m_kVShader.set(0, new VertexShader("Texture"));
            m_kPShader.set(0, new PixelShader("Texture"));
            m_kPShader.get(0).SetTextureQuantity(1);
            m_kPShader.get(0).SetImageName(0,m_akImageName[0]);
            return;
        }

        // In a single-effect drawing pass, texture 0 is a source to be blended
        // with a nonexistent destination.  As such, we think of the source mode
        // as SBF_ONE and the destination mode as SDF_ZERO.
        String kVShaderName = new String("T0d2");
        String kPShaderName = new String("T0s1d0");
        int i;
        for (i = 1; i < m_iTextureQuantity; i++)
        {
            kVShaderName = kVShaderName.concat("T" + i + "d2");
            kPShaderName = kPShaderName.concat("T" + i);

            AlphaState pkAS = m_kAlphaState.get(i);

            // Source blending mode.
            kPShaderName = kPShaderName.concat("s" + pkAS.SrcBlend.Value());

            // Destination blending mode.
            kPShaderName = kPShaderName.concat("d" + pkAS.DstBlend.Value());
        }
        kVShaderName = kVShaderName.concat("PassThrough");

        m_kVShader.set(0, new VertexShader(kVShaderName));
        m_kPShader.set(0, new PixelShader(kPShaderName));
        m_kPShader.get(0).SetTextureQuantity(m_iTextureQuantity);
        for (i = 0; i < m_iTextureQuantity; i++)
        {
            m_kPShader.get(0).SetImageName(i,m_akImageName[i]);
        }
    }

    /** The number of textures */
    protected int m_iTextureQuantity;
    /** The texture names */
    protected String[] m_akImageName;

    /** streaming constructor */
    public MultitextureEffect ()
    {
        m_iTextureQuantity = 0;
        m_akImageName = null;
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
        m_iTextureQuantity = rkStream.ReadInt();
        SetTextureQuantity(m_iTextureQuantity);
        for (int i = 0; i < m_iTextureQuantity; i++)
        {
            m_akImageName[i] = rkStream.ReadString();
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
        return super.Register(rkStream);
    }

    /**
     * Write this object and all it's children to the Stream.
     * @param rkStream, the Stream where the child objects are stored.
     */
    public void Save (Stream rkStream)
    {
        super.Save(rkStream);

        // native data
        rkStream.Write(m_iTextureQuantity);
        for (int i = 0; i < m_iTextureQuantity; i++)
        {
            rkStream.Write(m_akImageName[i]);
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
            Stream.SIZEOF_INT; //sizeof(m_iTextureQuantity);
 
        for (int i = 0; i < m_iTextureQuantity; i++)
        {
            iSize += Stream.SIZEOF_INT + //sizeof(int) +
                m_akImageName[i].length();
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
        pkTree.Append(StringTree.Format("MultitextureEffect",GetName()));
        // parent
        pkTree.Append(super.SaveStrings(null));

        pkTree.Append(StringTree.Format("texture quantity =",m_iTextureQuantity));

        for (int i = 0; i < m_iTextureQuantity; i++)
        {
            String kPrefix = new String("image[" + i + "] =");
            pkTree.Append(StringTree.Format(kPrefix,m_akImageName[i]));
        }

        return pkTree;
    }
}
