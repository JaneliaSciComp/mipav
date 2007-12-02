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
// Java example by Alexandra Bokinsky, PhD, Geometric Tools, Inc. (Nov 2007)
// Adapted from Gooch example in OpenGL Shading Language, 2nd ed, section 18.2,
// Non-photo realistic technical illustration shader. 
//

package gov.nih.mipav.view.WildMagic.LibGraphics.Effects;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;
//import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.Geometry;
//import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.Spatial;
//import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.VisibleObject;
import gov.nih.mipav.view.WildMagic.LibGraphics.Shaders.*;

public class GoochEffect extends ShaderEffect
    implements StreamInterface
{
    /**
     * Creates an GoochEffect, with default initialization. Pair with
     * WireframeBehindEffect to get silhouette edges, too.
     */
    public GoochEffect ()
    {
        // initialize single-pass rendering:
        super(1);
        // set the vertex and pixel shaders to be the Gooch shaders
        // defined in Gooch.cg:
        m_kVShader.set(0, new VertexShader("Gooch.glsl"));
        m_kPShader.set(0, new PixelShader("Gooch.glsl"));

        m_afLightPosition[0] = 0.0f;
        m_afLightPosition[1] = 10.0f;
        m_afLightPosition[2] = 4.0f;

        m_spkZState = new ZBufferState();
        m_spkSaveZState = null;
    }

    /** Delete memory */
    public void dispose()
    {
        m_afLightPosition = null;
        m_spkZState = null;
        super.dispose();
    }

    /** Parameters for the vertex program
     * @param rkLightPosition, position of the 'light' that shades the object.
     */
    public void SetLightPosition (final Vector3f rkLightPosition)
    {
        m_afLightPosition[0] = rkLightPosition.X();
        m_afLightPosition[1] = rkLightPosition.Y();
        m_afLightPosition[2] = rkLightPosition.Z();
    }

    /** Parameters for the vertex program
     * @return the light position. 
     */
    public Vector3f GetLightPosition ()
    {
        return new Vector3f(m_afLightPosition[0],m_afLightPosition[1],m_afLightPosition[2]);
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
        //pkVProgram.GetUC("LightPosition").SetDataSource(m_afLightPosition);
        // TODO several pixel-program parameters, defined as 'const', could be
        // added as 'uniform'
    }
    

    /** Override default rendering state to change the z-buffer comparison test.
     */
    public void SetGlobalState (int iPass, Renderer pkRenderer,
                                boolean bPrimaryEffect)
    {
        super.SetGlobalState(iPass, pkRenderer, bPrimaryEffect);
        m_spkSaveZState = pkRenderer.GetZBufferState();
        m_spkZState.Enabled = true;
        m_spkZState.Writable = true;
        m_spkZState.Compare = ZBufferState.CompareMode.CF_LESS;
        pkRenderer.SetZBufferState(m_spkZState);
        
    }
    /** Restore default rendering state.
     */
    public void RestoreGlobalState (int iPass, Renderer pkRenderer,
                                    boolean bPrimaryEffect)
    {
        pkRenderer.SetZBufferState(m_spkSaveZState);

        super.RestoreGlobalState(iPass, pkRenderer, bPrimaryEffect);
    }

    /** Saved and modified z-buffer state.
     */
    ZBufferState m_spkSaveZState,  m_spkZState;

    /** The light pos is stored at index 0, 1, 2.  The other value is
     * unused. */
    protected float[] m_afLightPosition = new float[4];

    /** streaming constructor */
    //public GoochEffect () {}

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
        m_afLightPosition[0] = rkStream.ReadFloat();
        m_afLightPosition[1] = rkStream.ReadFloat();
        m_afLightPosition[2] = rkStream.ReadFloat();
    }

    /**
     * Write this object and all it's children to the Stream.
     * @param rkStream, the Stream where the child objects are stored.
     */
    public void Save (Stream rkStream)
    {
        super.Save(rkStream);

        // native data
        rkStream.Write(m_afLightPosition[0]);
        rkStream.Write(m_afLightPosition[1]);
        rkStream.Write(m_afLightPosition[2]);
    }

    /**
     * Returns the size of this object and its children on disk for the
     * current StreamVersion parameter.
     * @param rkVersion, the current version of the Stream file being created.
     * @return the size of this object on disk.
     */
    public int GetDiskUsed (StreamVersion rkVersion)
    {
        return super.GetDiskUsed(rkVersion) +
            Stream.SIZEOF_FLOAT * 3; //sizeof(m_afLightPosition[0]);
    }

    /**
     * Write this object into a StringTree for the scene-graph visualization.
     * @param acTitle, the header for this object in the StringTree.
     * @return StringTree containing a String-based representation of this
     * object and its children.
     */
    public StringTree SaveStrings (final String acTitle)
    {
        StringTree pkTree = new StringTree();
        // strings
        pkTree.Append(StringTree.Format("GoochEffect",GetName()));
        // parent
        pkTree.Append(super.SaveStrings(null));

        Vector3f kLightPosition = new Vector3f(m_afLightPosition[0],m_afLightPosition[1],
                                               m_afLightPosition[2]);
        pkTree.Append(StringTree.Format("light position =",kLightPosition));

        return pkTree;
    }
}
