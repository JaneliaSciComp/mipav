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

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Shaders.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;

public class BlendedTerrainEffect extends ShaderEffect
    implements StreamInterface
{
    /**
     * Creates a BlendedTerrainEffect with the texture paramters.
     * @param acGrassName the name of the Grass Texture image file.
     * @param acStoneName the name of the Stone Texture image file.
     * @param acBlendName the name of the Blend Texture image file.
     * @param acCloudName the name of the Cloud Texture image file.
     */
    public BlendedTerrainEffect (final String acGrassName, final String acStoneName,
                                 final String acBlendName, final String acCloudName)
    {
        super(1);
        m_kVShader.set(0, new VertexShader("BlendedTerrain"));
        m_kPShader.set(0, new PixelShader("BlendedTerrain"));

        m_kPShader.get(0).SetTextureQuantity(4);
        m_kPShader.get(0).SetImageName(0,acGrassName);
        m_kPShader.get(0).SetImageName(1,acStoneName);
        m_kPShader.get(0).SetImageName(2,acBlendName);
        m_kPShader.get(0).SetImageName(3,acCloudName);

        Texture pkGrass = m_kPShader.get(0).GetTexture(0);
        pkGrass.SetFilterType(Texture.FilterType.LINEAR_LINEAR);
        pkGrass.SetWrapType(0,Texture.WrapType.REPEAT);
        pkGrass.SetWrapType(1,Texture.WrapType.REPEAT);

        Texture pkStone = m_kPShader.get(0).GetTexture(1);
        pkStone.SetFilterType(Texture.FilterType.LINEAR_LINEAR);
        pkStone.SetWrapType(0,Texture.WrapType.REPEAT);
        pkStone.SetWrapType(1,Texture.WrapType.REPEAT);

        Texture pkBlend = m_kPShader.get(0).GetTexture(2);
        pkBlend.SetFilterType(Texture.FilterType.LINEAR);
        pkBlend.SetWrapType(0,Texture.WrapType.CLAMP_EDGE);

        Texture pkCloud = m_kPShader.get(0).GetTexture(3);
        pkCloud.SetFilterType(Texture.FilterType.LINEAR_LINEAR);
        pkCloud.SetWrapType(0,Texture.WrapType.REPEAT);
        pkCloud.SetWrapType(1,Texture.WrapType.REPEAT);

        m_afFlowDirection[0] = 0.0f;
        m_afFlowDirection[1] = 0.0f;
        m_afPowerFactor[0] = 0.5f;
    }

    /** Parameters for the vertex program
     * @param rkFlowDirection, the direction of flow for the cloud effect.
     */
    public void SetFlowDirection (final Vector2f rkFlowDirection)
    {
        m_afFlowDirection[0] = rkFlowDirection.X();
        m_afFlowDirection[1] = rkFlowDirection.Y();
    }

    /** Parameters for the vertex program
     * @return the direction of flow for the cloud effect.
     */
    public Vector2f GetFlowDirection ()
    {
        return new Vector2f(m_afFlowDirection[0],m_afFlowDirection[1]);
    }

    /** Parameters for the pixel program 
     * @param fPowerFactor
     */
    public void SetPowerFactor (float fPowerFactor)
    {
        m_afPowerFactor[0] = fPowerFactor;
    }

    /** Parameters for the pixel program 
     * @return fPowerFactor
     */
    public float GetPowerFactor ()
    {
        return m_afPowerFactor[0];
    }

    /** Set the user-defined constants to use local storage.
     * @param iPass unused
     * @param pkVProgram the VertexProgram 
     * @param pkPProgram the PixelProgram 
     */
    public void OnLoadPrograms (int iPass, Program pkVProgram,
                                Program pkPProgram)
    {
        pkVProgram.GetUC("FlowDirection").SetDataSource(m_afFlowDirection);
        pkPProgram.GetUC("PowerFactor").SetDataSource(m_afPowerFactor);
    }


    /** The flow direction is stored in locations 0 and 1.  The others are
     * unused. */
    protected float[] m_afFlowDirection = new float[4];

    /** The power factor is stored in location 0.  The others are unused. */
    protected float[] m_afPowerFactor = new float[4];

    /** streaming constructor */
    public BlendedTerrainEffect () {}

    /**
     * Loads this object from the input parameter rkStream, using the input
     * Stream.Link to store the IDs of children objects of this object for
     * linking after all objects are loaded from the Stream.
     * @param rkStream, the Stream from which this object is being read.
     * @param pkLink, the Link class for storing the IDs of this object's
     * children objcts.
     */
    public void Load (Stream rkStream, Stream.Link pkLink)
    {
        super.Load(rkStream,pkLink);

        // native data
        m_afFlowDirection[0] = rkStream.ReadFloat();
        m_afFlowDirection[1] = rkStream.ReadFloat();
        m_afPowerFactor[0] = rkStream.ReadFloat();
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
        rkStream.Write(m_afFlowDirection[0]);
        rkStream.Write(m_afFlowDirection[1]);
        rkStream.Write(m_afPowerFactor[0]);
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
            Stream.SIZEOF_FLOAT + //sizeof(m_afFlowDirection[0]) +
            Stream.SIZEOF_FLOAT + //sizeof(m_afFlowDirection[1]) +
            Stream.SIZEOF_FLOAT; //sizeof(m_afPowerFactor[0]);
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
        pkTree.Append(StringTree.Format("BlendedTerrainEffect",GetName()));
        // parent
        pkTree.Append(super.SaveStrings(null));

        Vector2f kFlowDirection = new Vector2f(m_afFlowDirection[0],m_afFlowDirection[1]);
        pkTree.Append(StringTree.Format("flow direction =",kFlowDirection));
        pkTree.Append(StringTree.Format("power factor =",m_afPowerFactor[0]));

        return pkTree;
    }
}
