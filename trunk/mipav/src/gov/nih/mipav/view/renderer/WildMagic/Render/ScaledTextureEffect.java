package gov.nih.mipav.view.renderer.WildMagic.Render;
import WildMagic.LibGraphics.Effects.*;
import WildMagic.LibGraphics.ObjectSystem.*;
import WildMagic.LibGraphics.Shaders.*;

public class ScaledTextureEffect extends ShaderEffect
    implements StreamInterface
{
    private float m_fScale = 1.0f;
    
    /** Creates a new TextureEffect with the texture specified.
     * @param rkBaseName the name of the texture image.
     */
    public ScaledTextureEffect (final String rkBaseName, float fVal)
    {
        super(1);
        m_kVShader.set(0, new VertexShader("TextureV"));
        m_kPShader.set(0, new PixelShader("ScaledTextureP"));

        m_kPShader.get(0).SetTextureQuantity(1);
        m_kPShader.get(0).SetImageName(0,rkBaseName);
    
        m_fScale = fVal;
    }

    public void OnLoadPrograms (int iPass, Program pkVProgram,
            Program pkPProgram, Program pkCProgram)
    {
        if ( pkCProgram.GetUC("Scale") != null ) 
        {
            pkCProgram.GetUC("Scale").GetData()[0] = m_fScale;
        }
    }
}
