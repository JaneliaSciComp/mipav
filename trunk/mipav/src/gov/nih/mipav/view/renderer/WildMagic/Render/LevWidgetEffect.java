
package gov.nih.mipav.view.renderer.WildMagic.Render;
import WildMagic.LibGraphics.Effects.TextureEffect;
import WildMagic.LibGraphics.Shaders.PixelShader;
import WildMagic.LibGraphics.Shaders.Program;
import WildMagic.LibGraphics.Shaders.VertexShader;

public class LevWidgetEffect extends TextureEffect
{
    private LevWidgetState m_kWidgetState = new LevWidgetState();

    /** Creates a new LevWidgetEffect with the texture specified.
     * @param rkBaseName the name of the texture image.
     */
    public LevWidgetEffect (final String rkBaseName)
    {
        SetPassQuantity(1);
        m_kVShader.set(0, new VertexShader("LevWidgetEffect"));
        m_kPShader.set(0, new PixelShader("LevWidgetEffect", true));

        m_kPShader.get(0).SetTextureQuantity(1);
        m_kPShader.get(0).SetImageName(0,rkBaseName);
    }

    public LevWidgetState getState()
    {
        return m_kWidgetState;
    }

    public void SetColor( float fR, float fG, float fB, float fA ) 
    {
        m_kWidgetState.Color[0] = fR;
        m_kWidgetState.Color[1] = fG;
        m_kWidgetState.Color[2] = fB;
        m_kWidgetState.Color[3] = fA;
        Program pkProgram = GetPProgram(0);
        if ( (pkProgram != null) && (pkProgram.GetUC("LevColor") != null) ) 
        {
            pkProgram.GetUC("LevColor").SetDataSource(m_kWidgetState.Color);
        }
    }
    

    public void SetMidLine( float fX1, float fY1, float fX2, float fY2 ) 
    {
        m_kWidgetState.MidLine[0] = fX1;
        m_kWidgetState.MidLine[1] = fY1;
        m_kWidgetState.MidLine[2] = fX2;
        m_kWidgetState.MidLine[3] = fY2;
        Program pkProgram = GetPProgram(0);
        if ( (pkProgram != null) && (pkProgram.GetUC("LevMidLine") != null) ) 
        {
            pkProgram.GetUC("LevMidLine").SetDataSource(m_kWidgetState.MidLine);
        }
    }
    

    public void SetLeftLine( float fX1, float fY1, float fX2, float fY2 ) 
    {
        m_kWidgetState.LeftLine[0] = fX1;
        m_kWidgetState.LeftLine[1] = fY1;
        m_kWidgetState.LeftLine[2] = fX2;
        m_kWidgetState.LeftLine[3] = fY2;
        Program pkProgram = GetPProgram(0);
        if ( (pkProgram != null) && (pkProgram.GetUC("LevLeftLine") != null) ) 
        {
            pkProgram.GetUC("LevLeftLine").SetDataSource(m_kWidgetState.LeftLine);
            //System.err.println( fX1 + " " + fY1 + " " + fX2 + " " + fY2 );
        }
    }
    

    public void SetRightLine( float fX1, float fY1, float fX2, float fY2 ) 
    {
        m_kWidgetState.RightLine[0] = fX1;
        m_kWidgetState.RightLine[1] = fY1;
        m_kWidgetState.RightLine[2] = fX2;
        m_kWidgetState.RightLine[3] = fY2;
        Program pkProgram = GetPProgram(0);
        if ( (pkProgram != null) && (pkProgram.GetUC("LevRightLine") != null) ) 
        {
            pkProgram.GetUC("LevRightLine").SetDataSource(m_kWidgetState.RightLine);
        }
    }
    
    
    public void setBoundary( float fAlpha )
    {
        m_kWidgetState.BoundaryEmphasis[0] = fAlpha;
        Program pkProgram = GetPProgram(0);
        if ( (pkProgram != null) && (pkProgram.GetUC("BoundaryEmphasis") != null) ) 
        {
            pkProgram.GetUC("BoundaryEmphasis").SetDataSource(m_kWidgetState.BoundaryEmphasis);
        }
    }
}
