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

package gov.nih.mipav.view.WildMagic.LibGraphics.Shaders;

import java.util.Vector;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;

public abstract class Shader extends GraphicsObject
    implements StreamInterface
{
    /** The name of the shader object.  The program object has a name that
     * contains the shader name as a substring, but adds additional text
     * as needed (the path to a shader on disk, the identifying information
     * for a procedurally generated shader).
     * @return shader name.
     */
    public final String GetShaderName ()
    {
        return m_kShaderName;
    }
    
    /** Set the number of textures in this shader.
     * @param iQuantity, the number of textures in this shader.
     */
    public void SetTextureQuantity (int iQuantity)
    {
        m_kTextures.clear();
        m_kTextures.setSize(iQuantity);
        for (int i = 0; i < iQuantity; i++)
        {
            m_kTextures.set(i, new Texture());
        }

        m_kImageNames.clear();
        m_kImageNames.setSize(iQuantity);
    }

    /** Return number of textures in this shader.
     * @return number of textures in this shader.
     */
    public int GetTextureQuantity ()
    {
        return (int)m_kTextures.size();
    }

    /** Set the texture at position i.
     * @param i, texture index.
     * @param kTexture, texture to put at position i.
     */
    public void SetTexture (int i, Texture kTexture)
    {
        if (0 <= i && i < (int)m_kTextures.size())
        {
             m_kTextures.set(i, kTexture);
        }
    }

    /** Get the texture at position i.
     * @param i, texture index.
     * @return texture at position i.
     */
    public Texture GetTexture (int i)
    {
        if (0 <= i && i < (int)m_kTextures.size())
        {
            return m_kTextures.get(i);
        }
        return null;
    }

    /** Get the texture by name.
     * @param rkName, texture name.
     * @return texture with the given name.
     */
    public Texture GetTexture (String rkName)
    {
        if (m_spkProgram != null)
        {
            for (int i = 0; i < (int)m_kTextures.size(); i++)
            {
                Texture pkTexture = m_kTextures.get(i);
                SamplerInformation pkSI = pkTexture.GetSamplerInformation();
                if (pkSI.GetName() == rkName)
                {
                    return pkTexture;
                }
            }
        }
        return null;
    }

    /** Set the image name.
     * @param i, image position.
     * @param rkName, image name.
     */
    public void SetImageName (int i, String rkName)
    {
        int iQuantity = (int)m_kImageNames.size();
        if (i >= iQuantity)
        {
            m_kImageNames.setSize(i+1);
        }

        m_kImageNames.set(i, rkName);
    }

    /** Get the image name.
     * @param i, image position.
     * @return image name.
     */
    public String GetImageName (int i)
    {
        assert(0 <= i && i < (int)m_kImageNames.size());
        return m_kImageNames.get(i);
    }


    /** Support for streaming. */
    public Shader () {}

    /** Delete memory. */
    public void finalize()
    {
        m_kShaderName = null;
        if ( m_spkProgram != null )
        {
            m_spkProgram.finalize();
            m_spkProgram = null;
        }
        m_kUserData.clear();
        m_kUserData = null;
        m_kImageNames.clear();
        m_kImageNames = null;
        m_kTextures.clear();
        m_kTextures = null;
    }

    /** The constructor called by the derived classes VertexShader and
     * PixelShader.
     * @param rkShaderName, shader name.
     */
    protected Shader (String rkShaderName)
    {
        m_kShaderName = new String(rkShaderName);
    }

    /** The constructor called by the derived classes VertexShader and
     * PixelShader.
     * @param rkShaderName, shader name.
     */
    protected Shader (String rkShaderName, boolean bUnique)
    {
        m_kShaderName = new String(rkShaderName);
        m_bUnique = bUnique;
    }
    
    public final boolean GetUnique()
    {
        return m_bUnique;
    }

    /** The shader name, which contributes to a uniquely identifying string
     * for a shader program. */
    protected String m_kShaderName;
 
    protected boolean m_bUnique = false;
    /** The shader program, which is dependent on graphics API. */
    protected Program m_spkProgram;

    /** The user-defined data are specific to each shader object.  The Program
     * object knows only the name, which register to assign the value to, and
     * how many registers to use.  The storage provided here is for the
     * convenience of Shader-derived classes.  However, a derived class may
     * provide alternate storage by calling UserConstant::SetDataSource for
     * each user constant of interest. */
    protected Vector<Float> m_kUserData = new Vector<Float>();

    /** The names of images used by an instance of a shader program.  The
     * Texture objects store the actual images and the samplers that are used
     * to sample the images. */
    protected Vector<String> m_kImageNames = new Vector<String>();

    /** Texture objects store the actual images and the samplers that are used
     * to sample the images. */
    protected Vector<Texture> m_kTextures = new Vector<Texture>();

    /** Called when a program is loaded.
     * @param pkProgram, the newly loaded program.
     */
    public void OnLoadProgram (Program pkProgram)
    {
        assert((m_spkProgram == null) && (pkProgram != null));
        m_spkProgram = pkProgram;

        // The data sources must be set for the user constants.  Determine how
        // many float channels are needed for the storage.
        int iUCQuantity = m_spkProgram.GetUCQuantity();
        int i, iChannels;
        UserConstant pkUC;
        for (i = 0, iChannels = 0; i < iUCQuantity; i++)
        {
            pkUC = m_spkProgram.GetUC(i);
            assert(pkUC != null);
            iChannels += 4*pkUC.GetRegisterQuantity();
        }
        m_kUserData.setSize(iChannels);

        // Set the data sources for the user constants.
        for (i = 0, iChannels = 0; i < iUCQuantity; i++)
        {
            pkUC = m_spkProgram.GetUC(i);
            assert(pkUC != null);
            int iSize = 4*pkUC.GetRegisterQuantity();
            float[] afData = new float[iSize];
            pkUC.SetDataSource(afData);
            iChannels += 4*pkUC.GetRegisterQuantity();
        }

        // Load the images into the textures.  If the image is already in
        // system memory (in the image catalog), it is ready to be used.  If
        // it is not in system memory, an attempt is made to load it from
        // disk storage.  If the image file does not exist on disk, a default
        // magenta image is used.
        int iSIQuantity = m_spkProgram.GetSIQuantity();
        m_kImageNames.setSize(iSIQuantity);
        m_kTextures.setSize(iSIQuantity);
        for (i = 0; i < iSIQuantity; i++)
        {
            GraphicsImage pkImage = ImageCatalog.GetActive().Find(m_kImageNames.get(i));
            assert(pkImage != null);
            if (m_kTextures.get(i) == null)
            {
                m_kTextures.set(i, new Texture());
            }
            m_kTextures.get(i).SetImage(pkImage);
            m_kTextures.get(i).SetSamplerInformation(m_spkProgram.GetSI(i));
        }
    }

    /** Called when a program is released. */
    public void OnReleaseProgram ()
    {
        // Destroy the program.  The texture images, if any, will be destroyed
        // by the shader destructor.  If the shader has the last reference to
        // an image, that image will be deleted from the image catalog
        // automatically.
        m_spkProgram.finalize();
        m_spkProgram = null;
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
        m_kShaderName = rkStream.ReadString();

        int iQuantity = rkStream.ReadInt();
        m_kImageNames.setSize(iQuantity);
        for (int i = 0; i < iQuantity; i++)
        {
            m_kImageNames.set(i, rkStream.ReadString());
        }

        // link data
        iQuantity = rkStream.ReadInt();
        m_kTextures.setSize(iQuantity);
        for (int i = 0; i < iQuantity; i++)
        {
            int iLinkID = rkStream.ReadInt();  // m_kTextures[i]
            pkLink.Add(iLinkID);
        }

        // The data members m_spkProgram and m_kUserData are both set during
        // resource loading at program runtime.
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

        for (int i = 0; i < m_kTextures.size(); i++)
        {
            int iLinkID = pkLink.GetLinkID();
            m_kTextures.set(i, (Texture)rkStream.GetFromMap(iLinkID));
        }
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
        if (!super.Register(rkStream))
        {
            return false;
        }

        for (int i = 0; i < m_kTextures.size(); i++)
        {
            if (m_kTextures.get(i) != null)
            {
                m_kTextures.get(i).Register(rkStream);
            }
        }

        return true;
    }

    /**
     * Write this object and all it's children to the Stream.
     * @param rkStream, the Stream where the child objects are stored.
     */
    public void Save (Stream rkStream)
    {
        super.Save(rkStream);

        // native data
        rkStream.Write(m_kShaderName);

        int iQuantity = m_kImageNames.size();
        rkStream.Write(iQuantity);
        for (int i = 0; i < iQuantity; i++)
        {
            rkStream.Write(m_kImageNames.get(i));
        }

        // link data
        iQuantity = m_kTextures.size();
        rkStream.Write(iQuantity);
        for (int i = 0; i < iQuantity; i++)
        {
            rkStream.Write(m_kTextures.get(i).GetID());
        }

        // The data members m_spkProgram and m_kUserData are both set during
        // resource loading at program runtime.
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
            Stream.SIZEOF_INT + //sizeof(int) +
            m_kShaderName.length();

        int iQuantity = m_kImageNames.size();
        iSize += Stream.SIZEOF_INT; //sizeof(int);
        for (int i = 0; i < iQuantity; i++)
        {
            iSize += Stream.SIZEOF_INT + //sizeof(int) +
                m_kImageNames.get(i).length();
        }

        iQuantity = m_kTextures.size();
        iSize += Stream.SIZEOF_INT + //sizeof(int) +
            iQuantity*Stream.SIZEOF_INT; //sizeof(m_kTextures[0]);

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
        pkTree.Append(StringTree.Format("Shader",GetName()));
        pkTree.Append(super.SaveStrings(null));
        pkTree.Append(StringTree.Format("shader name =",m_kShaderName));

        for (int i = 0; i < m_kImageNames.size(); i++)
        {
            String kPrefix = new String("image[" + i + "] =");
            pkTree.Append(StringTree.Format(kPrefix,m_kImageNames.get(i)));
        }

        // children
        for (int i = 0; i < (int)m_kTextures.size(); i++)
        {
            pkTree.Append(m_kTextures.get(i).SaveStrings(null));
        }

        return pkTree;
    }
}
