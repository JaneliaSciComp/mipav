package gov.nih.mipav.view.renderer.WildMagic.Render;


import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.view.renderer.J3D.PlaneRender;
import WildMagic.LibGraphics.Effects.ShaderEffect;
import WildMagic.LibGraphics.ObjectSystem.StreamInterface;
import WildMagic.LibGraphics.ObjectSystem.StringTree;
import WildMagic.LibGraphics.Shaders.PixelShader;
import WildMagic.LibGraphics.Shaders.Program;
import WildMagic.LibGraphics.Shaders.Shader;
import WildMagic.LibGraphics.Shaders.VertexShader;

/** The VolumePlaneEffect ShaderEffect creates shaders for mapping the volume
 * data onto the planes for the 3-orthogonal planes displayed in the
 * VolumeViewer and for the PlaneRender objects.
 * @see GPUVolumeRender.java
 * @see VolumeViewer.java
 * @see PlaneRender.java
 */
public class VolumePlaneEffect extends ShaderEffect
implements StreamInterface
{
    /**  */
    private static final long serialVersionUID = -5399442879177045664L;

    /** Shared volume data and textures. */
    private VolumeImage m_kVolumeImageA;

    /** Shared volume data and textures. */
    private VolumeImage m_kVolumeImageB;
    
    private ModelRGB rgbA, rgbB;
    
    private boolean m_bShowSurface = false;
    private float m_fBlend = 1.0f;

    private PixelShader m_kPShader;
    private boolean m_bTransparent;

    /** 
     * Creates a new VolumeShaderEffect object.
     * @param kVolumeImageA the VolumeImage containing shared data and
     * textures for rendering.
     * @param kVolumeImageB second VolumeImage.
     * @param bUnique when true the shader program must be unique.
     */
    public VolumePlaneEffect ( VolumeImage kVolumeImageA, VolumeImage kVolumeImageB, boolean bTransparent )
    {
        m_kVolumeImageA = kVolumeImageA;
        m_kVolumeImageB = kVolumeImageB;
        m_bTransparent = bTransparent;
        Init( );
    }

    /**
     * Sets the blend factor shader parameter between imageA and imageB.
     * @param fBlend blend factor (range = 0-1).
     */
    public void Blend(float fBlend)
    {
    	fBlend = (float) Math.min( 1.0, fBlend );
    	fBlend = (float) Math.max( 0.0, fBlend );
    	if ( m_fBlend != fBlend )
    	{
    		m_fBlend = fBlend;
    		createProgramText();
    	}
		Program pkCProgram = GetCProgram(0);
		if ( (pkCProgram != null) && pkCProgram.GetUC("Blend") != null ) 
		{
			pkCProgram.GetUC("Blend").GetData()[0] = m_fBlend;
		}
    }

    /**
     * memory cleanup.
     */
    public void dispose()
    {
        m_kVolumeImageA = null;
        m_kVolumeImageB = null;
        super.dispose();
    }
    
    /**
     * Sets the blend factor shader parameter between imageA and imageB.
     * @param fBlend blend factor (range = 0-1).
     */
    public float GetBlend()
    {
        return m_fBlend;
    }

    /* (non-Javadoc)
     * @see WildMagic.LibGraphics.Effects.ShaderEffect#OnLoadPrograms(int, WildMagic.LibGraphics.Shaders.Program, WildMagic.LibGraphics.Shaders.Program)
     */
    public void OnLoadPrograms (int iPass, Program pkVProgram,
            Program pkPProgram, Program pkCProgram)
    {
        Blend(1);
        
        if ( m_kVolumeImageB.GetImage() != null )
        {
            setABBlend(.5f);
        }
        else
        {
        	setABBlend(1);
        }
    }

    /* (non-Javadoc)
     * @see WildMagic.LibGraphics.Effects.ShaderEffect#SaveStrings(java.lang.String)
     */
    public StringTree SaveStrings (final String acTitle)
    {
        StringTree pkTree = new StringTree();
        pkTree.Append(StringTree.Format("VolumePlaneEffect",GetName()));
        pkTree.Append(super.SaveStrings(null));

        return pkTree;
    }
    /**
     * Sets the blend factor shader parameter between imageA and imageB.
     * @param fBlend blend factor (range = 0-1).
     */
    public void setABBlend(float fBlend)
    {
        Program pkCProgram = GetCProgram(0);
        if ( (pkCProgram != null) && pkCProgram.GetUC("ABBlend") != null ) 
        {
            pkCProgram.GetUC("ABBlend").GetData()[0] = fBlend;
        }
    }


    public void setRGBTA(ModelRGB RGBT)
    {
    	rgbA = RGBT;
    	createProgramText();
    }    

    public void setRGBTB(ModelRGB RGBT)
    {
    	rgbB = RGBT;
    	createProgramText();
    }

    /**
     * Turns rendering the planes with the surface mask on/off.
     * @param bOn on/off.
     */
    public void ShowSurface( boolean bOn )
    {
    	m_bShowSurface = bOn;
    	createProgramText();
    }    
    


    
    /** Initializes the ShaderEffect vertex and pixel shader programs. */
    private void Init ( )
    {
        /* Set single-pass rendering: */
        SetPassQuantity(1);
        SetVShader(0,new VertexShader("TextureV", Shader.vertexShaderTexture3));
        
        m_kPShader = new PixelShader("ColorOpacityTexture", "" );
        createProgramText();
                 
        SetPShader(0,m_kPShader);
    }
    

	private static String basicParameters = ""
	    	+ "in vec3 varTexCoord;" + "\n"
    	+ "uniform mat4 WVPMatrix;" + "\n"
    	+ "uniform sampler3D bVolumeImageA; " + "\n"
    	+ "uniform sampler1D cColorMapA; " + "\n"
    	+ "uniform float Blend; " + "\n"
    	+ "" + "\n";
	
	private static String basicParametersB = ""
    	+ "uniform sampler3D jVolumeImageB; " + "\n"
    	+ "uniform sampler1D kColorMapB; " + "\n"
    	+ "uniform float ABBlend; " + "\n"
    	+ "" + "\n";

	
	private static String basicParametersSurface = ""
    	+ "uniform sampler3D iSurfaceImage; " + "\n"
    	+ "" + "\n";

	private static String outputParameters = ""
    	+ "out vec4 fragColor; " + "\n"
    	+ "" + "\n";
	
	private static String outputParametersTransparency = ""
	    	+ "out vec4     outFragData0; " + "\n"
	    	+ "out vec4     outFragData1; " + "\n"
	    	+ "" + "\n";
    
    private String createProgramText()
    {
    	boolean bAddColorMap_TexturesB = false;	    	

    	String text = "";
    	
    	text += basicParameters;
    	if ( m_kVolumeImageB.GetImage() != null )
    	{
    		text += basicParametersB;
    		bAddColorMap_TexturesB = true;
    	}
    	if ( m_bShowSurface )
    	{
    		text += basicParametersSurface;
    	}
    	
    	if ( m_bTransparent )
    	{
    		text += outputParametersTransparency;
    	}
    	else
    	{
    		text += outputParameters;
    	}
    	
    	text += "\n";
    	text += "void main() {" + "\n";
    	
    	text += "vec4 kOutputColor = vec4(0.0);" + "\n";
    	text += "vec4 color = texture(bVolumeImageA, varTexCoord, 0.0 );" + "\n";
    	if ( m_kVolumeImageA.GetImage().isColorImage() )
    	{
    		if ( rgbA != null )
    		{
    			if ( rgbA.getROn() )
    			{
    				text += "kOutputColor.r = texture(cColorMapA,color.r, 0.0).r;" + "\n";
    			}
    			if ( rgbA.getGOn() )
    			{
    				text += "kOutputColor.g = texture(cColorMapA,color.g, 0.0).g;" + "\n";
    			}
    			if ( rgbA.getBOn() )
    			{
    				text += "kOutputColor.b = texture(cColorMapA,color.b, 0.0).b;" + "\n";
    			}
    		}
    	}
    	else
    	{
    		text += "kOutputColor.rgb = texture(cColorMapA, color.r, 0.0).rgb;" + "\n";
    	}
    	if ( m_kVolumeImageB.GetImage() != null )
    	{
    		text += "color = texture(jVolumeImageB, varTexCoord, 0.0 );" + "\n";
    		if ( m_kVolumeImageB.GetImage().isColorImage() )
    		{
    			if ( rgbB != null )
    			{
    				if ( rgbB.getROn() )
    				{
    					text += "kOutputColor.r = ABBlend * kOutputColor.r + (1.0 - ABBlend) * texture(kColorMapB, color.r, 0.0).r;" + "\n";
    				}
    				else
    				{
    					text += "kOutputColor.r = ABBlend * kOutputColor.r;" + "\n";
    				}
    				if ( rgbB.getGOn() )
    				{
    					text += "kOutputColor.g = ABBlend * kOutputColor.g + (1.0 - ABBlend) * texture(kColorMapB, color.g, 0.0).g;" + "\n";
    				}
    				else
    				{
    					text += "kOutputColor.g = ABBlend * kOutputColor.g;" + "\n";
    				}
    				if ( rgbB.getBOn() )
    				{
    					text += "ABBlend * kOutputColor.b + (1.0 - ABBlend) * texture(kColorMapB, color.b, 0.0).b;" + "\n";
    				}
    				else
    				{
    					text += "kOutputColor.b = ABBlend * kOutputColor.bg;" + "\n";
    				}
    			}
    		}
    		else
    		{
    			text += "kOutputColor.rgb = ABBlend * kOutputColor.rgb + (1.0 - ABBlend) * texture(kColorMapB, color.r, 0.0).rgb;" + "\n";
    		}
    	}
    	
    	if ( m_bShowSurface )
    	{
    		text += "vec4 surfaceColor = texture(iSurfaceImage, varTexCoord, 0.0);" + "\n";
    		text += "if ( (surfaceColor.r != 0) || (surfaceColor.g != 0) || (surfaceColor.b != 0)) {" + "\n";
    		text += "   kOutputColor.rgb = surfaceColor.rgb;" + "\n";
    		text += "}" + "\n";
    	}

		text += "kOutputColor.a = Blend;" + "\n";

    	if ( m_bTransparent )
		{
    		text += "outFragData0 = vec4(kOutputColor.rgb * kOutputColor.a, kOutputColor.a);" + "\n";
    		text += "outFragData1 = vec4(1.0);" + "\n";
		}
		else
		{
			text += "fragColor = kOutputColor;" + "\n";
		}
    	text += "}" + "\n";
    	
    	// Done generating program text.
    	

    	if ( (m_kPShader != null) && (m_kPShader.GetProgram() != null) )
    	{
    		if ( !text.equals( m_kPShader.GetProgram().GetProgramText() ))
    		{
    			m_kPShader.GetProgram().SetProgramText( text );
    			if ( GetCProgram(0) != null )
    			{
    				GetCProgram(0).Reload(true);
    			}

    			// Add the used textures to the shader program data structures:
    			int iTex = 0;
    			if ( m_kPShader != null )
    			{
    				m_kPShader.SetImageName(iTex, m_kVolumeImageA.GetVolumeTarget().GetName(), "bVolumeImageA" );
    				m_kPShader.SetTexture(iTex++, m_kVolumeImageA.GetVolumeTarget(), "bVolumeImageA" );
    				m_kPShader.SetImageName(iTex, m_kVolumeImageA.GetColorMapTarget().GetName(), "cColorMapA");
    				m_kPShader.SetTexture(iTex++, m_kVolumeImageA.GetColorMapTarget(), "cColorMapA");   
    			}

    			if ( bAddColorMap_TexturesB && (m_kPShader != null))
    			{
    				m_kPShader.SetImageName(iTex, m_kVolumeImageB.GetVolumeTarget().GetName(), "jVolumeImageB" );
    				m_kPShader.SetTexture(iTex++, m_kVolumeImageB.GetVolumeTarget(), "jVolumeImageB" );
    				m_kPShader.SetImageName(iTex, m_kVolumeImageB.GetColorMapTarget().GetName(), "kColorMapB");
    				m_kPShader.SetTexture(iTex++, m_kVolumeImageB.GetColorMapTarget(), "kColorMapB");   
    			}
    			if ( m_bShowSurface && (m_kPShader != null) )
    			{
    				m_kPShader.SetImageName(iTex, m_kVolumeImageA.GetSurfaceTarget().GetName(), "iSurfaceImage" );
    				m_kPShader.SetTexture(iTex++, m_kVolumeImageA.GetSurfaceTarget(), "iSurfaceImage" ); 		
    			}
    		}
    		return text;
    	}
    	
    	//System.err.println("START");
		//System.err.println( text );
    	//System.err.println("END");
    	return text;
    }
}
