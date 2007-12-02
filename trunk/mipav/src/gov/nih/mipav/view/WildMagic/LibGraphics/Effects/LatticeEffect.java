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
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Shaders.*;

public class LatticeEffect extends ShaderEffect
    implements StreamInterface
{
    /**
     * Creates an LatticeEffect withe the texture images in the acBaseName
     * and acGradName image files.
     * @param acBaseName the base image texture.
     * @param acGradName the gradient image texture.
     */
    public LatticeEffect (final String acBaseName, final String acGradName)
    {
        // initialize single-pass rendering:
        super(1);
        // set the vertex and pixel shaders to be the Lattice shaders
        // defined in Lattice.cg:
        m_kVShader.set(0, new VertexShader("Lattice.glsl"));
        m_kPShader.set(0, new PixelShader("Lattice.glsl"));

        // The pixel-shader has two textres parameters:
        m_kPShader.get(0).SetTextureQuantity(1);
        // Set the pixel-shader texture image names:
        //m_kPShader.get(0).SetImageName(0,acBaseName);
        //m_kPShader.get(0).SetImageName(1,acGradName);

        // Set the texture wrapping and filter types:
        Texture pkBase = m_kPShader.get(0).GetTexture(0);
        pkBase.SetFilterType(Texture.FilterType.LINEAR);
        pkBase.SetWrapType(0,Texture.WrapType.REPEAT);
        pkBase.SetWrapType(1,Texture.WrapType.REPEAT);

        // Set the texture filter type:
        //Texture pkGrad = m_kPShader.get(0).GetTexture(1);
        //pkGrad.SetFilterType(Texture.FilterType.LINEAR);

        m_afScale[0] = 10.0f;
        m_afScale[1] = 10.0f;
        m_afThreshold[0] = 0.6f;
        m_afThreshold[1] = 0.3f;
    }

    /** Delete memory */
    public void dispose()
    {
        m_afScale = null;
        m_afThreshold = null;
        super.dispose();
    }

    /** Parameters for the pixel program
     * @param rkScale, 
     */
    public void SetScale (final Vector2f rkScale)
    {
        m_afScale[0] = rkScale.X();
        m_afScale[1] = rkScale.Y();
    }

    /** Parameters for the pixel program
     * @return the direction of flow for the cloud effect.
     */
    public Vector2f GetScale ()
    {
        return new Vector2f(m_afScale[0],m_afScale[1]);
    }

    /** Parameters for the pixel program
     * @param rkThreshold, 
     */
    public void SetThreshold (final Vector2f rkThreshold)
    {
        m_afThreshold[0] = rkThreshold.X();
        m_afThreshold[1] = rkThreshold.Y();
    }

    /** Parameters for the pixel program
     * @return the direction of flow for the cloud effect.
     */
    public Vector2f GetThreshold ()
    {
        return new Vector2f(m_afThreshold[0],m_afThreshold[1]);
    }


    /** Set the user-defined constants to use local storage.
     * @param iPass
     * @param pkVProgram the VertexProgram for this Effect
     * @param pkPProgram the PixelProgram for this Effect
     */
    public void OnLoadPrograms (int iPass, Program pkVProgram,
                                   Program pkPProgram)
    {
        // vertex program processing
        if ( pkPProgram.GetUC("Scale") != null )
        {
            pkPProgram.GetUC("Scale").SetDataSource(m_afScale);
        }
        if ( pkPProgram.GetUC("Threshold") != null )
        {
            pkPProgram.GetUC("Threshold").SetDataSource(m_afThreshold);
        }
    }

    /** The scale is stored at index 0, 1.  The other values are
     * unused. */
    protected float[] m_afScale = new float[4];
    protected float[] m_afThreshold = new float[4];

    /** streaming constructor */
    public LatticeEffect () {}

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
        m_afScale[0] = rkStream.ReadFloat();
        m_afScale[1] = rkStream.ReadFloat();
        m_afThreshold[0] = rkStream.ReadFloat();
        m_afThreshold[1] = rkStream.ReadFloat();
    }

    /**
     * Write this object and all it's children to the Stream.
     * @param rkStream, the Stream where the child objects are stored.
     */
    public void Save (Stream rkStream)
    {
        super.Save(rkStream);

        // native data
        rkStream.Write(m_afScale[0]);
        rkStream.Write(m_afScale[1]);
        rkStream.Write(m_afThreshold[0]);
        rkStream.Write(m_afThreshold[1]);
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
            Stream.SIZEOF_FLOAT * 4; //sizeof(m_afScale[0]);
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
        pkTree.Append(StringTree.Format("LatticeEffect",GetName()));
        // parent
        pkTree.Append(super.SaveStrings(null));

        Vector2f kScale = new Vector2f(m_afScale[0],m_afScale[1]);
        Vector2f kThreshold = new Vector2f(m_afThreshold[0],m_afThreshold[1]);
        pkTree.Append(StringTree.Format("scale =",kScale));
        pkTree.Append(StringTree.Format("threshold =",kThreshold));

        return pkTree;
    }
}
