
package gov.nih.mipav.view.renderer.WildMagic.Render;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibGraphics.Effects.TextureEffect;
import WildMagic.LibGraphics.Shaders.PixelShader;
import WildMagic.LibGraphics.Shaders.Program;
import WildMagic.LibGraphics.Shaders.VertexShader;

public class LevWidgetEffect extends TextureEffect
{
    private float[] m_afColor = new float[4];
    private float[] m_afMidLine = new float[4];

    /** Creates a new LevWidgetEffect with the texture specified.
     * @param rkBaseName the name of the texture image.
     */
    public LevWidgetEffect (final String rkBaseName)
    {
        SetPassQuantity(1);
        m_kVShader.set(0, new VertexShader("LevWidgetEffect"));
        m_kPShader.set(0, new PixelShader("LevWidgetEffect"));

        m_kPShader.get(0).SetTextureQuantity(1);
        m_kPShader.get(0).SetImageName(0,rkBaseName);
    }

    public void SetColor( ColorRGBA kColor ) 
    {
        m_afColor[0] = kColor.R;
        m_afColor[1] = kColor.G;
        m_afColor[2] = kColor.B;
        m_afColor[3] = kColor.A;
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("LevColor") != null ) 
        {
            pkProgram.GetUC("LevColor").SetDataSource(m_afColor);
        }
    }
    

    public void SetColor( float fR, float fG, float fB, float fA ) 
    {
        m_afColor[0] = fR;
        m_afColor[1] = fG;
        m_afColor[2] = fB;
        m_afColor[3] = fA;
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("LevColor") != null ) 
        {
            pkProgram.GetUC("LevColor").SetDataSource(m_afColor);
        }
    }
    

    public void SetMidLine( float fX1, float fY1, float fX2, float fY2 ) 
    {
        m_afMidLine[0] = fX1;
        m_afMidLine[1] = fY1;
        m_afMidLine[2] = fX2;
        m_afMidLine[3] = fY2;
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("LevMidLine") != null ) 
        {
            pkProgram.GetUC("LevMidLine").SetDataSource(m_afMidLine);
        }
    }
}
