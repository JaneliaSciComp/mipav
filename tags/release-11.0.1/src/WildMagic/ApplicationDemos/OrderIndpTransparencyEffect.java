
package WildMagic.ApplicationDemos;
import java.io.File;

import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibGraphics.Effects.ShaderEffect;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.Shaders.PixelShader;
import WildMagic.LibGraphics.Shaders.Program;
import WildMagic.LibGraphics.Shaders.Shader;
import WildMagic.LibGraphics.Shaders.VertexShader;

public class OrderIndpTransparencyEffect extends ShaderEffect
{

	private static final long serialVersionUID = 8505999941771583043L;


	private float m_fAlpha = 0.5f;
    

    private ColorRGBA m_kBackgroundColor;
    
    public OrderIndpTransparencyEffect ()
    {
        super(1);   
        m_fAlpha = .5f;
        String dir = new String( "dual_depth_peeling" + File.separator + "shaders" + File.separator ); 
		
        m_kVShader.set(0, new VertexShader( dir + "wavg_init_vertex2.glsl"));
        m_kPShader.set(0, new PixelShader( dir + "wavg_init_fragment3.glsl"));
    }
    
    public OrderIndpTransparencyEffect (float fAlpha)
    {
        super(1);   
        m_fAlpha = fAlpha;
        String dir = new String( "dual_depth_peeling" + File.separator + "shaders" + File.separator ); 
		
        m_kVShader.set(0, new VertexShader( dir + "wavg_init_vertex2.glsl"));
        m_kPShader.set(0, new PixelShader( dir + "wavg_init_fragment2a.glsl"));
    }
    
    public OrderIndpTransparencyEffect (final String rkBaseName, float fAlpha)
    {
        super(1);   
        m_fAlpha = fAlpha;
        m_kVShader.set(0, new VertexShader("OrderIndpTransparencyInitV", OrderIndpTransparencyInitV ));
        m_kPShader.set(0, new PixelShader("OrderIndpTransparencyInitP"));

        m_kPShader.get(0).SetTextureQuantity(1);
        m_kPShader.get(0).SetImageName(0,rkBaseName,"BaseSampler");
    }

    
    public OrderIndpTransparencyEffect(Texture[] akTextures, ColorRGBA kBackgroundColor)
    {
    	super(1);

        m_kBackgroundColor = kBackgroundColor;
    	String dir = new String( "dual_depth_peeling" + File.separator + "shaders" + File.separator ); 
        
        PixelShader kPShader = new PixelShader( dir + "wavg_final_fragment2.glsl");
        kPShader.SetTextureQuantity(akTextures.length);
        for ( int i = 0; i < akTextures.length; i++ )
        {
            kPShader.SetTexture( i, akTextures[i], "ColorTex" + i );
            kPShader.SetImageName( i, akTextures[i].GetName(), "ColorTex" + i );
        }

    	m_kVShader.set(0, new VertexShader( "TextureV", Shader.vertexShaderTexture2  ));
    	m_kPShader.set(0, kPShader);
        
    }
    

    /**
     * Creates a new MultitextureEffect with the number of Textures specified
     * by iTextureQuantity
     * @param iTextureQuantity the number of textures in this
     * MultitextureEffect.
    public OrderIndpTransparencyEffect (Texture[] akTextures, Texture kSolidTex, ColorRGBA kBackgroundColor)
    {
        super(1);   
        m_kBackgroundColor = kBackgroundColor;
        SetVShader(0,new VertexShader("TextureV", Shader.vertexShaderTexture2, true ));
        PixelShader kPShader = new PixelShader("OrderIndpTransparencyFinalP", true);
        SetPShader(0,kPShader);

        kPShader.SetTextureQuantity(3);
        for ( int i = 0; i < 2; i++ )
        {
            kPShader.SetTexture( i, akTextures[i] );
            kPShader.SetImageName( i, akTextures[i].GetName() );
        }
        kPShader.SetTexture( 2, kSolidTex );
        kPShader.SetImageName( 2, kSolidTex.GetName() );
    }
     */
    
    /* (non-Javadoc)
     * @see WildMagic.LibGraphics.Effects.ShaderEffect#OnLoadPrograms(int, WildMagic.LibGraphics.Shaders.Program, WildMagic.LibGraphics.Shaders.Program)
     */
    @Override
	public void OnLoadPrograms (int iPass, Program pkVProgram, Program pkPProgram, Program pkCProgram)
    {

        if ( pkCProgram.GetUC("BackgroundColor") != null ) 
        {
        	pkCProgram.GetUC("BackgroundColor").GetData()[0] = m_kBackgroundColor.R;
        	pkCProgram.GetUC("BackgroundColor").GetData()[1] = m_kBackgroundColor.G;
        	pkCProgram.GetUC("BackgroundColor").GetData()[2] = m_kBackgroundColor.B;
        }

        if ( pkCProgram.GetUC("MyAlpha") != null ) 
        {
            pkCProgram.GetUC("MyAlpha").GetData()[0] = m_fAlpha;
        }
        if ( pkCProgram.GetUC("Alpha") != null ) 
        {
            pkCProgram.GetUC("Alpha").GetData()[0] = m_fAlpha;
        }
    }
    public void SetAlpha (float fAlpha)
    {
        m_fAlpha = fAlpha;
        Program pkCProgram = GetCProgram(0);
        /*
        if ( pkProgram != null && pkProgram.GetUC("BackgroundColor") != null ) 
        {
            pkProgram.GetUC("BackgroundColor").GetData()[0] = m_kBackgroundColor.R;
            pkProgram.GetUC("BackgroundColor").GetData()[1] = m_kBackgroundColor.G;
            pkProgram.GetUC("BackgroundColor").GetData()[2] = m_kBackgroundColor.B;
        }
        */
        if ( pkCProgram.GetUC("MyAlpha") != null ) 
        {
            pkCProgram.GetUC("MyAlpha").GetData()[0] = m_fAlpha;
        }
    }
    


    // Vertex Shader for texture mapping with 2D Texture Coordinates.
    // Output to varTexCoord;
    public static String OrderIndpTransparencyInitV = "" 
    		+ "uniform mat4 WVPMatrix;" + "\n"
    		+ "in vec3 inPosition;" + "\n"
    		+ "in vec2 inTexcoord0;" + "\n"
    		+ "in vec4 inColor0;" + "\n"
    		+ "out vec2 varTexCoord;" + "\n"
    		+ "out vec4 varColor;" + "\n"
    		+ "void main ()" + "\n"
    		+ "{" + "\n"
    		// Transform the position from model space to clip space.
    		+ "   gl_Position = WVPMatrix * vec4(inPosition, 1.0);" + "\n"
    		// Pass through the texture coordinate.
    		+ "   varTexCoord = inTexcoord0;" + "\n"
    		// Pass through the color.
    		+ "   varColor = inColor0;" + "\n"
    		+ "}" + "\n";
}
