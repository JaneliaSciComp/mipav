package gov.nih.mipav.view.renderer.WildMagic.Render;


import java.io.Serializable;

import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Effects.ShaderEffect;
import WildMagic.LibGraphics.ObjectSystem.StreamInterface;
import WildMagic.LibGraphics.Shaders.PixelShader;
import WildMagic.LibGraphics.Shaders.Program;
import WildMagic.LibGraphics.Shaders.VertexShader;


public class BoundingBoxEffect extends ShaderEffect
    implements StreamInterface, Serializable
{
	private static final long serialVersionUID = 7377691694355531644L;
	private Vector3f origin, range, scale, plane;
	private ColorRGB color, background;

	public BoundingBoxEffect( Vector3f origin, Vector3f range, Vector3f scale, Vector3f plane )
	{
		super(1);
		this.origin = origin;
		this.range = range;
		this.scale = scale;
		this.plane = plane;
		this.color = new ColorRGB(1,0,0);
		this.background = new ColorRGB(0,0,0);
        m_kVShader.set(0, new VertexShader("BoundingBoxGridV"));
        m_kPShader.set(0, new PixelShader("BoundingBoxGridP"));
	} 
	
	public void OnLoadPrograms (int iPass, Program pkVProgram,
            Program pkPProgram, Program pkCProgram)
    {
        if ( (pkCProgram != null) && (pkCProgram.GetUC("origin") != null) ) 
        {
            pkCProgram.GetUC("origin").GetData()[0] = origin.X;
            pkCProgram.GetUC("origin").GetData()[1] = origin.Y;
            pkCProgram.GetUC("origin").GetData()[2] = origin.Z;
        }
        if ( (pkCProgram != null) && (pkCProgram.GetUC("range") != null) ) 
        {
            pkCProgram.GetUC("range").GetData()[0] = range.X;  
            pkCProgram.GetUC("range").GetData()[1] = range.Y;  
            pkCProgram.GetUC("range").GetData()[2] = range.Z;        	
        }
        if ( (pkCProgram != null) && (pkCProgram.GetUC("volScale") != null) ) 
        {
            pkCProgram.GetUC("volScale").GetData()[0] = scale.X;  
            pkCProgram.GetUC("volScale").GetData()[1] = scale.Y;  
            pkCProgram.GetUC("volScale").GetData()[2] = scale.Z;        	
        }
        if ( (pkCProgram != null) && (pkCProgram.GetUC("plane") != null) ) 
        {
            pkCProgram.GetUC("plane").GetData()[0] = plane.X;  
            pkCProgram.GetUC("plane").GetData()[1] = plane.Y;  
            pkCProgram.GetUC("plane").GetData()[2] = plane.Z;        	
        }
        if ( (pkCProgram != null) && (pkCProgram.GetUC("Color1") != null) ) 
        {
            pkCProgram.GetUC("Color1").GetData()[0] = color.R;  
            pkCProgram.GetUC("Color1").GetData()[1] = color.G;  
            pkCProgram.GetUC("Color1").GetData()[2] = color.B;        	
        }
        if ( (pkCProgram != null) && (pkCProgram.GetUC("Color2") != null) ) 
        {
            pkCProgram.GetUC("Color2").GetData()[0] = background.R;  
            pkCProgram.GetUC("Color2").GetData()[1] = background.G;  
            pkCProgram.GetUC("Color2").GetData()[2] = background.B;        	
        }
        if ( (pkCProgram != null) && (pkCProgram.GetUC("AvgColor") != null) ) 
        {
            pkCProgram.GetUC("AvgColor").GetData()[0] = (color.R + background.R)/2f;  
            pkCProgram.GetUC("AvgColor").GetData()[1] = (color.G + background.G)/2f;  
            pkCProgram.GetUC("AvgColor").GetData()[2] = (color.B + background.B)/2f;        	
        }
        if ( (pkCProgram != null) && (pkCProgram.GetUC("METHOD") != null) ) 
        {
            pkCProgram.GetUC("METHOD").GetData()[0] = 1;
        }
    }
	
	public void setColor( ColorRGB colorC )
	{
		color.Copy(colorC);
        Program kCProgram = GetCProgram(0);  

        if ( (kCProgram != null) && (kCProgram.GetUC("Color1") != null) ) 
        {
        	kCProgram.GetUC("Color1").GetData()[0] = color.R;  
        	kCProgram.GetUC("Color1").GetData()[1] = color.G;  
        	kCProgram.GetUC("Color1").GetData()[2] = color.B;        	
        }
        if ( (kCProgram != null) && (kCProgram.GetUC("AvgColor") != null) ) 
        {
        	kCProgram.GetUC("AvgColor").GetData()[0] = (color.R + background.R)/2f;  
        	kCProgram.GetUC("AvgColor").GetData()[1] = (color.G + background.G)/2f;  
        	kCProgram.GetUC("AvgColor").GetData()[2] = (color.B + background.B)/2f;        	
        }		
	}
	
	public void setBackground( ColorRGBA backgroundC )
	{
		background.Set(backgroundC.R, backgroundC.G, backgroundC.B);
        Program kCProgram = GetCProgram(0);  
        if ( (kCProgram != null) && (kCProgram.GetUC("Color2") != null) ) 
        {
        	kCProgram.GetUC("Color2").GetData()[0] = background.R;  
        	kCProgram.GetUC("Color2").GetData()[1] = background.G;  
        	kCProgram.GetUC("Color2").GetData()[2] = background.B;        	
        }
        if ( (kCProgram != null) && (kCProgram.GetUC("AvgColor") != null) ) 
        {
        	kCProgram.GetUC("AvgColor").GetData()[0] = (color.R + background.R)/2f;  
        	kCProgram.GetUC("AvgColor").GetData()[1] = (color.G + background.G)/2f;  
        	kCProgram.GetUC("AvgColor").GetData()[2] = (color.B + background.B)/2f;        	
        }		
	}
/*
	private boolean method = false;
	public void toggleMethod()
	{
		method = !method;
        Program kCProgram = GetCProgram(0);  
        if ( (kCProgram != null) && (kCProgram.GetUC("METHOD") != null) ) 
        {
        	kCProgram.GetUC("METHOD").GetData()[0] = method ? 1 : 0;
        }
	} */
}