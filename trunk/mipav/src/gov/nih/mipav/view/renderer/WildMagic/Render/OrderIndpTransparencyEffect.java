package gov.nih.mipav.view.renderer.WildMagic.Render;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibGraphics.Effects.*;
import WildMagic.LibGraphics.Rendering.*;
import WildMagic.LibGraphics.Shaders.*;

public class OrderIndpTransparencyEffect extends ShaderEffect
{
    /**
     * Creates a new MultitextureEffect with the number of Textures specified
     * by iTextureQuantity
     * @param iTextureQuantity the number of textures in this
     * MultitextureEffect.
     */
    public OrderIndpTransparencyEffect (Texture[] akTextures, Texture kSolidTex, ColorRGBA kBackgroundColor)
    {
        super(1);   
        m_kBackgroundColor = kBackgroundColor;
        SetVShader(0,new VertexShader("TextureV", true));
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
    

    public OrderIndpTransparencyEffect (Texture[] akTextures, ColorRGBA kBackgroundColor)
    {
        super(1);   
        m_kBackgroundColor = kBackgroundColor;
        SetVShader(0,new VertexShader("TextureV", true));
        PixelShader kPShader = new PixelShader("OrderIndpTransparencyFinalP", true);
        SetPShader(0,kPShader);

        kPShader.SetTextureQuantity(akTextures.length);
        for ( int i = 0; i < akTextures.length; i++ )
        {
            kPShader.SetTexture( i, akTextures[i] );
            kPShader.SetImageName( i, akTextures[i].GetName() );
        }
    }
    
    public OrderIndpTransparencyEffect (final String rkBaseName, float fAlpha)
    {
        super(1);   
        m_fAlpha = fAlpha;
        m_kVShader.set(0, new VertexShader("MaterialTextureV"));
        //m_kPShader.set(0, new PixelShader("MaterialTextureP"));
        m_kPShader.set(0, new PixelShader("OrderIndpTransparencyInitP"));

        m_kPShader.get(0).SetTextureQuantity(1);
        m_kPShader.get(0).SetImageName(0,rkBaseName);
    }
    
    /* (non-Javadoc)
     * @see WildMagic.LibGraphics.Effects.ShaderEffect#OnLoadPrograms(int, WildMagic.LibGraphics.Shaders.Program, WildMagic.LibGraphics.Shaders.Program)
     */
    public void OnLoadPrograms (int iPass, Program pkVProgram, Program pkPProgram, Program pkCProgram)
    {
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
    
    private float m_fAlpha = 0.5f;
    private ColorRGBA m_kBackgroundColor;
}
