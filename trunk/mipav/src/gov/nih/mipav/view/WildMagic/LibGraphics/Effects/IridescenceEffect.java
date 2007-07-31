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

package gov.nih.mipav.view.WildMagic.LibGraphics.Effects;

import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Shaders.*;

public class IridescenceEffect extends ShaderEffect
    implements StreamInterface
{
    /**
     * Creates an IridescenceEffect withe the texture images in the acBaseName
     * and acGradName image files.
     * @param acBaseName the base image texture.
     * @param acGradName the gradient image texture.
     */
    public IridescenceEffect (final String acBaseName, final String acGradName)
    {
        // initialize single-pass rendering:
        super(1);
        // set the vertex and pixel shaders to be the Iridescence shaders
        // defined in Iridescence.cg:
        m_kVShader.set(0, new VertexShader("Iridescence"));
        m_kPShader.set(0, new PixelShader("Iridescence"));

        // The pixel-shader has two textres parameters:
        m_kPShader.get(0).SetTextureQuantity(2);
        // Set the pixel-shader texture image names:
        m_kPShader.get(0).SetImageName(0,acBaseName);
        m_kPShader.get(0).SetImageName(1,acGradName);

        // Set the texture wrapping and filter types:
        Texture pkBase = m_kPShader.get(0).GetTexture(0);
        pkBase.SetFilterType(Texture.FilterType.LINEAR);
        pkBase.SetWrapType(0,Texture.WrapType.REPEAT);
        pkBase.SetWrapType(1,Texture.WrapType.REPEAT);

        // Set the texture filter type:
        Texture pkGrad = m_kPShader.get(0).GetTexture(1);
        pkGrad.SetFilterType(Texture.FilterType.LINEAR);
    }

    /**
     * Sets the interpolation factor
     * @param fInterpolateFactor
     */
    public void SetInterpolateFactor (float fInterpolateFactor)
    {
        m_afInterpolate[0] = fInterpolateFactor;
    }

    /**
     * Returns the interpolation factor
     * @return fInterpolateFactor
     */
    public float GetInterpolateFactor ()
    {
        return m_afInterpolate[0];
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
        pkVProgram.GetUC("InterpolateFactor").SetDataSource(m_afInterpolate);
    }

    /** The interpolation factor is stored at index 0.  The other values are
     * unused. */
    protected float[] m_afInterpolate = new float[4];

    /** streaming constructor */
    public IridescenceEffect () {}

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
        m_afInterpolate[0] = rkStream.ReadFloat();
    }

    /**
     * Write this object and all it's children to the Stream.
     * @param rkStream, the Stream where the child objects are stored.
     */
    public void Save (Stream rkStream)
    {
        super.Save(rkStream);

        // native data
        rkStream.Write(m_afInterpolate[0]);
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
            Stream.SIZEOF_FLOAT; //sizeof(m_afInterpolate[0]);
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
        pkTree.Append(StringTree.Format("IridescenceEffect",GetName()));
        // parent
        pkTree.Append(super.SaveStrings(null));

        pkTree.Append(StringTree.Format("interpolation factor =",m_afInterpolate[0]));

        return pkTree;
    }
}
