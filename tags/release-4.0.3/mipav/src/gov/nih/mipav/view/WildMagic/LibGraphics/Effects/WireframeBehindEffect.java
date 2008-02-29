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
// Java Example by Alexandra Bokinsky, PhD, Geometric Tools, Inc. (Nov 2007)
// Adapted from Gooch example in OpenGL Shading Language, 2nd ed, section 18.2,
// Non-photo-realistic technical illustration shader.
//

package gov.nih.mipav.view.WildMagic.LibGraphics.Effects;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Shaders.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;

public class WireframeBehindEffect extends ShaderEffect
    implements StreamInterface
{
    /** Creates an new WireframeBehindEffect */
    public WireframeBehindEffect ()
    {
        super(1);
        m_kVShader.set(0, new VertexShader("BlackColor4.glsl"));
        m_kPShader.set(0, new PixelShader("PassThrough4.glsl"));
        m_spkCullState = new CullState();
        m_spkSaveCullState = null;
        m_spkSaveWireframeState = null;
        m_spkWireframeState = new WireframeState();
    }
    
    /** Override the default rendering state, to change cull state, wireframe
     * state and line width so we render a black outline of the back faces of
     * the model.  If the normal model is drawn, too, this shows as silhouette
     * edges.
     */
    public void SetGlobalState (int iPass, Renderer pkRenderer,
                                boolean bPrimaryEffect)
    {
        super.SetGlobalState(iPass, pkRenderer, bPrimaryEffect);
        m_spkSaveCullState = pkRenderer.GetCullState();
        m_spkCullState.CullFace = CullState.CullMode.CT_FRONT;
        pkRenderer.SetCullState(m_spkCullState);
        
        m_spkSaveWireframeState = pkRenderer.GetWireframeState();
        m_spkWireframeState.Enabled = true;
        pkRenderer.SetWireframeState(m_spkWireframeState);
        
        m_fSaveLineWidth = pkRenderer.GetLineWidth();
        pkRenderer.SetLineWidth(3.3f);
    }
    
    /**
     *  Restore saved states.
     */
    public void RestoreGlobalState (int iPass, Renderer pkRenderer,
                                    boolean bPrimaryEffect)
    {
        pkRenderer.SetCullState(m_spkSaveCullState);
        pkRenderer.SetWireframeState(m_spkSaveWireframeState);
        pkRenderer.SetLineWidth(m_fSaveLineWidth);

        super.RestoreGlobalState(iPass, pkRenderer, bPrimaryEffect);
    }
    
    /** Saved and modified rendering states */
    private CullState m_spkSaveCullState,  m_spkCullState;
    private WireframeState m_spkSaveWireframeState, m_spkWireframeState;
    private float m_fSaveLineWidth;

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
    }

    /**
     * Returns the size of this object and it's children on disk for the
     * current StreamVersion parameter.
     * @param rkVersion, the current version of the Stream file being created.
     * @return the size of this object on disk.
     */
    public int GetDiskUsed (StreamVersion rkVersion)
    {
        return super.GetDiskUsed(rkVersion);
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
        pkTree.Append(StringTree.Format("WireframeBehindEffect",GetName()));
        pkTree.Append(super.SaveStrings(null));
        return pkTree;
    }
}
