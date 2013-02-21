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
import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibGraphics.Effects.ShaderEffect;
import WildMagic.LibGraphics.Shaders.PixelShader;
import WildMagic.LibGraphics.Shaders.Program;
import WildMagic.LibGraphics.Shaders.VertexShader;

public class VolumeVOIEffect extends ShaderEffect
{
    /**  */
    private static final long serialVersionUID = 8243388938991470076L;

    public VolumeVOIEffect (boolean bTransparent)
    {
        super(1);
        m_kVShader.set(0, new VertexShader("VOIVertex", true));            
        if ( bTransparent )
        {
            m_kPShader.set(0, new PixelShader("VOITransparency", true));
        }
        else
        {
            m_kPShader.set(0, new PixelShader("VOISolid", true));
        }
    }
        
    
    /**
     * Set the blend value.
     * @param fValue blend value.
     */
    public void Blend( float fValue )
    {    
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram == null )
        {
            return;
        }
        if ( pkCProgram.GetUC("Blend") != null)
        {
            pkCProgram.GetUC("Blend").GetData()[0] = fValue;
        }
    }

    /* (non-Javadoc)
     * @see WildMagic.LibGraphics.Effects.ShaderEffect#OnLoadPrograms(int, WildMagic.LibGraphics.Shaders.Program, WildMagic.LibGraphics.Shaders.Program)
     */
    public void OnLoadPrograms (int iPass, Program pkVProgram,
                                Program pkPProgram, Program pkCProgram)
    {
        Blend(1);
        SetSlice(false, 0, -1);
    }

    public void SetColor( ColorRGB kColor)
    {    
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram == null )
        {
            return;
        }
        if ( pkCProgram.GetUC("Color") != null)
        {
            pkCProgram.GetUC("Color").GetData()[0] = kColor.R;
            pkCProgram.GetUC("Color").GetData()[1] = kColor.G;
            pkCProgram.GetUC("Color").GetData()[2] = kColor.B;
        }
    }

    public void SetSlice( boolean bUseSlice, int iWhichSlice, float fSlice )
    {
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram == null )
        {
            return;
        }
        if ( pkCProgram.GetUC("Slice") != null)
        {
            pkCProgram.GetUC("Slice").GetData()[0] = fSlice;
            //System.err.println( "Slice " + fSlice );
        }
        if ( pkCProgram.GetUC("UseSlice") != null)
        {
            pkCProgram.GetUC("UseSlice").GetData()[0] = bUseSlice ? 1 : 0;
            //System.err.println( "UseSlice " + pkCProgram.GetUC("UseSlice").GetData()[0] );
        }
        if ( pkCProgram.GetUC("WhichSlice") != null)
        {
            pkCProgram.GetUC("WhichSlice").GetData()[0] = iWhichSlice;
            //System.err.println( "WhichSlice " + pkCProgram.GetUC("WhichSlice").GetData()[0] );
        }
    }

}
