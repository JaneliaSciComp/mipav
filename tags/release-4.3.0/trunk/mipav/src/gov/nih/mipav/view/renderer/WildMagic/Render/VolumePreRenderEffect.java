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

package gov.nih.mipav.view.renderer.WildMagic.Render;
import WildMagic.LibGraphics.Effects.ShaderEffect;
import WildMagic.LibGraphics.ObjectSystem.StreamInterface;
import WildMagic.LibGraphics.ObjectSystem.StringTree;
import WildMagic.LibGraphics.Shaders.PixelShader;
import WildMagic.LibGraphics.Shaders.Program;
import WildMagic.LibGraphics.Shaders.VertexShader;

public class VolumePreRenderEffect extends ShaderEffect
    implements StreamInterface
{
    /** Creates an new VolumePreRenderEffect 
     * @param bUseTextureCoords when true use texture coordinates as the colors in the pre-render.
     * @param bUnique when true create unique shader programs, when false share shader programs.
     */
    public VolumePreRenderEffect (boolean bUseTextureCoords, boolean bUnique)
    {
        super(1);
        if ( bUseTextureCoords )
        {
            m_kVShader.set(0, new VertexShader("VolumePreRender", bUnique));
        }
        else
        {
            m_kVShader.set(0, new VertexShader("VolumePreRenderColor", bUnique));            
        }
        m_kPShader.set(0, new PixelShader("PassThrough4", bUnique));
    }
    
    /**
     * Set the blend value.
     * @param fValue blend value.
     */
    public void Blend( float fValue )
    {    
        Program pkProgram = GetVProgram(0);
        if ( pkProgram == null )
        {
            return;
        }
        if ( pkProgram.GetUC("Blend") != null)
        {
            pkProgram.GetUC("Blend").GetData()[0] = fValue;
        }
    }

    /* (non-Javadoc)
     * @see WildMagic.LibGraphics.Effects.ShaderEffect#OnLoadPrograms(int, WildMagic.LibGraphics.Shaders.Program, WildMagic.LibGraphics.Shaders.Program)
     */
    public void OnLoadPrograms (int iPass, Program pkVProgram,
                                Program pkPProgram)
    {
        Blend(1);
    }

    /* (non-Javadoc)
     * @see WildMagic.LibGraphics.Effects.ShaderEffect#SaveStrings(java.lang.String)
     */
    public StringTree SaveStrings (final String acTitle)
    {
        StringTree pkTree = new StringTree();
        pkTree.Append(StringTree.Format("VolumePreRenderEffect",GetName()));
        pkTree.Append(super.SaveStrings(null));
        return pkTree;
    }
}
