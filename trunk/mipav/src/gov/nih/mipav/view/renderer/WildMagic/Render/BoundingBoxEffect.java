package gov.nih.mipav.view.renderer.WildMagic.Render;


import java.io.Serializable;

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

	public BoundingBoxEffect( Vector3f origin, Vector3f range, Vector3f scale, Vector3f plane )
	{
		super(1);
		this.origin = origin;
		this.range = range;
		this.scale = scale;
		this.plane = plane;
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
    }
}