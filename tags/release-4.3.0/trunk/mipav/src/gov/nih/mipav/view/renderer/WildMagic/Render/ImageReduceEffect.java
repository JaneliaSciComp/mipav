package gov.nih.mipav.view.renderer.WildMagic.Render;
import WildMagic.LibGraphics.Effects.*;
import WildMagic.LibGraphics.ObjectSystem.*;
import WildMagic.LibGraphics.Shaders.*;

public class ImageReduceEffect extends ShaderEffect
    implements StreamInterface
{

    private int m_iWidth;
    private int m_iHeight;
    
    /** Creates a new TextureEffect with the texture specified.
     * @param rkBaseName the name of the texture image.
     */
    public ImageReduceEffect (final String rkBaseName, int iWidth, int iHeight)
    {
        super(1);
        m_kVShader.set(0, new VertexShader("TextureV", true));
        m_kPShader.set(0, new PixelShader("ImageReduceP", true));

        m_kPShader.get(0).SetTextureQuantity(1);
        m_kPShader.get(0).SetImageName(0,rkBaseName);
        
        m_iWidth = iWidth;
        m_iHeight = iHeight;
    }
    
    public void OnLoadPrograms (int iPass, Program pkVProgram,
            Program pkPProgram)
    {
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("Step") != null ) 
        {
            pkProgram.GetUC("Step").GetData()[0] = 1.0f/(float)m_iWidth;
            pkProgram.GetUC("Step").GetData()[1] = 1.0f/(float)m_iHeight;
            //System.err.println( 1.0f/(float)m_iWidth + " " + 1.0f/(float)m_iHeight );
        }
    }

}
