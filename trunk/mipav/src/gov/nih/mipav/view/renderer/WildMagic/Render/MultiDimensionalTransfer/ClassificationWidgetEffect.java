
package gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer;
import WildMagic.LibGraphics.Effects.TextureEffect;
import WildMagic.LibGraphics.Shaders.PixelShader;
import WildMagic.LibGraphics.Shaders.Program;
import WildMagic.LibGraphics.Shaders.VertexShader;

public class ClassificationWidgetEffect extends TextureEffect
{
    private ClassificationWidgetState m_kWidgetState = new ClassificationWidgetState();

    /** Creates a new LevWidgetEffect with the texture specified.
     * @param rkBaseName the name of the texture image.
     */
    public ClassificationWidgetEffect (final String rkBaseName)
    {
        SetPassQuantity(1);
        m_kVShader.set(0, new VertexShader("TextureV"));
        m_kPShader.set(0, new PixelShader("LevWidgetEffect", true));

        m_kPShader.get(0).SetTextureQuantity(1);
        m_kPShader.get(0).SetImageName(0,rkBaseName);
        m_kWidgetState.UseWidget[0] = 1.0f;
    }

    public void dispose()
    {
        m_kWidgetState.dispose();
        m_kWidgetState = null;
        super.dispose();
    }
    
    public ClassificationWidgetState getState()
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
        computeUniformVariables();
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
        computeUniformVariables();
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
        computeUniformVariables();
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
    
    private void computeUniformVariables()
    {

        float fShiftL = 0;
        float fShiftR = 0;
        if ( m_kWidgetState.MidLine[1] == m_kWidgetState.MidLine[3] )
        {
            float fIncr = (m_kWidgetState.MidLine[1] - m_kWidgetState.LeftLine[1]) /
            (m_kWidgetState.LeftLine[3] - m_kWidgetState.LeftLine[1]);

            fIncr = fIncr * (m_kWidgetState.RightLine[0] - m_kWidgetState.LeftLine[0]);

            float fShiftX = (m_kWidgetState.MidLine[0] - m_kWidgetState.LeftLine[0]) /
            (m_kWidgetState.RightLine[0] - m_kWidgetState.LeftLine[0]);
            fShiftL = (fShiftX)*fIncr;
            fShiftR = (1.0f-fShiftX)*fIncr;
        }
        Program pkProgram = GetPProgram(0);
        if ( (pkProgram != null) && (pkProgram.GetUC("Shift") != null) ) 
        {
            pkProgram.GetUC("Shift").GetData()[0] = fShiftL;
            pkProgram.GetUC("Shift").GetData()[1] = fShiftR;
        }
        if ( (pkProgram != null) && (pkProgram.GetUC("InvY0MY1") != null) ) 
        {            
            float fLeftInvY0MY1 = 1.0f / (m_kWidgetState.LeftLine[1] - m_kWidgetState.LeftLine[3]);
            float fMidInvY0MY1 = 1.0f / (m_kWidgetState.MidLine[1] - m_kWidgetState.MidLine[3]);
            float fRightInvY0MY1 = 1.0f / (m_kWidgetState.RightLine[1] - m_kWidgetState.RightLine[3]);
            pkProgram.GetUC("InvY0MY1").GetData()[0] = fLeftInvY0MY1;
            pkProgram.GetUC("InvY0MY1").GetData()[1] = fMidInvY0MY1;
            pkProgram.GetUC("InvY0MY1").GetData()[2] = fRightInvY0MY1;
        }
    }
    
}
