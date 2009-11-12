
package gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer;
import java.io.IOException;

import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibGraphics.Effects.TextureEffect;
import WildMagic.LibGraphics.Shaders.PixelShader;
import WildMagic.LibGraphics.Shaders.Program;
import WildMagic.LibGraphics.Shaders.VertexShader;

public class ClassificationWidgetEffect extends TextureEffect
{
    /**  */
    private static final long serialVersionUID = -7141385452679118672L;
    private ClassificationWidgetState m_kWidgetState = new ClassificationWidgetState();

    /** Creates a new LevWidgetEffect with the texture specified.
     * @param rkBaseName the name of the texture image.
     */
    public ClassificationWidgetEffect (final String rkBaseName)
    {
        SetPassQuantity(1);
        m_kVShader.set(0, new VertexShader("TextureV"));
        m_kPShader.set(0, new PixelShader("ClassificationWidgetEffect", true));

        m_kPShader.get(0).SetTextureQuantity(1);
        m_kPShader.get(0).SetImageName(0,rkBaseName);
        m_kWidgetState.UseWidget[0] = 1.0f;
    }


    public ClassificationWidgetEffect (ClassificationWidgetEffect kEffect)
    {
        SetPassQuantity(1);
        m_kVShader.set(0, new VertexShader("TextureV"));
        m_kPShader.set(0, new PixelShader("ClassificationWidgetEffect", true));

        m_kPShader.get(0).SetTextureQuantity(1);
        m_kPShader.get(0).SetImageName(0, kEffect.m_kPShader.get(0).GetImageName(0) );
        m_kWidgetState = new ClassificationWidgetState();
        m_kWidgetState.Copy(kEffect.m_kWidgetState);
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
    
    public void setState(ClassificationWidgetState kState)
    {
        m_kWidgetState = kState;
        Program pkCProgram = GetCProgram(0);
        if ( (pkCProgram != null) && (pkCProgram.GetUC("LevColor") != null) ) 
        {
            pkCProgram.GetUC("LevColor").SetDataSource(m_kWidgetState.Color);
        }
        if ( (pkCProgram != null) && (pkCProgram.GetUC("LevMidLine") != null) ) 
        {
            pkCProgram.GetUC("LevMidLine").SetDataSource(m_kWidgetState.MidLine);
        }
        if ( (pkCProgram != null) && (pkCProgram.GetUC("LevLeftLine") != null) ) 
        {
            pkCProgram.GetUC("LevLeftLine").SetDataSource(m_kWidgetState.LeftLine);
            //System.err.println( fX1 + " " + fY1 + " " + fX2 + " " + fY2 );
        }
        if ( (pkCProgram != null) && (pkCProgram.GetUC("LevRightLine") != null) ) 
        {
            pkCProgram.GetUC("LevRightLine").SetDataSource(m_kWidgetState.RightLine);
        }
        if ( (pkCProgram != null) && (pkCProgram.GetUC("BoundaryEmphasis") != null) ) 
        {
            pkCProgram.GetUC("BoundaryEmphasis").SetDataSource(m_kWidgetState.BoundaryEmphasis);
        }
        computeUniformVariables();
    }

    public ColorRGBA GetColor( ) 
    {
        return new ColorRGBA (
                m_kWidgetState.Color[0],
                m_kWidgetState.Color[1],
                m_kWidgetState.Color[2],
                m_kWidgetState.Color[3] );
    }

    public void SetColor( float fR, float fG, float fB, float fA ) 
    {
        m_kWidgetState.Color[0] = fR;
        m_kWidgetState.Color[1] = fG;
        m_kWidgetState.Color[2] = fB;
        m_kWidgetState.Color[3] = fA;
        UpdateColor();
    }

    public void UpdateColor( ) 
    {
        Program pkCProgram = GetCProgram(0);
        if ( (pkCProgram != null) && (pkCProgram.GetUC("LevColor") != null) ) 
        {
            pkCProgram.GetUC("LevColor").SetDataSource(m_kWidgetState.Color);
        }
    }
    

    public void SetMidLine( float fX1, float fY1, float fX2, float fY2 ) 
    {
        m_kWidgetState.MidLine[0] = fX1;
        m_kWidgetState.MidLine[1] = fY1;
        m_kWidgetState.MidLine[2] = fX2;
        m_kWidgetState.MidLine[3] = fY2;
        Program pkCProgram = GetCProgram(0);
        if ( (pkCProgram != null) && (pkCProgram.GetUC("LevMidLine") != null) ) 
        {
            pkCProgram.GetUC("LevMidLine").SetDataSource(m_kWidgetState.MidLine);
        }
        computeUniformVariables();
    }
    

    public void SetLeftLine( float fX1, float fY1, float fX2, float fY2 ) 
    {
        m_kWidgetState.LeftLine[0] = fX1;
        m_kWidgetState.LeftLine[1] = fY1;
        m_kWidgetState.LeftLine[2] = fX2;
        m_kWidgetState.LeftLine[3] = fY2;
        Program pkCProgram = GetCProgram(0);
        if ( (pkCProgram != null) && (pkCProgram.GetUC("LevLeftLine") != null) ) 
        {
            pkCProgram.GetUC("LevLeftLine").SetDataSource(m_kWidgetState.LeftLine);
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
        Program pkCProgram = GetCProgram(0);
        if ( (pkCProgram != null) && (pkCProgram.GetUC("LevRightLine") != null) ) 
        {
            pkCProgram.GetUC("LevRightLine").SetDataSource(m_kWidgetState.RightLine);
        }
        computeUniformVariables();
    }
    
    
    public void setBoundary( float fAlpha )
    {
        m_kWidgetState.BoundaryEmphasis[0] = fAlpha;
        Program pkCProgram = GetCProgram(0);
        if ( (pkCProgram != null) && (pkCProgram.GetUC("BoundaryEmphasis") != null) ) 
        {
            pkCProgram.GetUC("BoundaryEmphasis").SetDataSource(m_kWidgetState.BoundaryEmphasis);
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
        Program pkCProgram = GetCProgram(0);
        if ( (pkCProgram != null) && (pkCProgram.GetUC("Shift") != null) ) 
        {
            pkCProgram.GetUC("Shift").GetData()[0] = fShiftL;
            pkCProgram.GetUC("Shift").GetData()[1] = fShiftR;
        }
        if ( (pkCProgram != null) && (pkCProgram.GetUC("InvY0MY1") != null) ) 
        {            
            float fLeftInvY0MY1 = 1.0f / (m_kWidgetState.LeftLine[1] - m_kWidgetState.LeftLine[3]);
            float fMidInvY0MY1 = 1.0f / (m_kWidgetState.MidLine[1] - m_kWidgetState.MidLine[3]);
            float fRightInvY0MY1 = 1.0f / (m_kWidgetState.RightLine[1] - m_kWidgetState.RightLine[3]);
            pkCProgram.GetUC("InvY0MY1").GetData()[0] = fLeftInvY0MY1;
            pkCProgram.GetUC("InvY0MY1").GetData()[1] = fMidInvY0MY1;
            pkCProgram.GetUC("InvY0MY1").GetData()[2] = fRightInvY0MY1;
        }
    }

    public void writeObject(java.io.ObjectOutputStream out)
    throws IOException 
    {
        out.writeObject( m_kPShader.get(0).GetImageName(0) );
        out.writeObject(m_kWidgetState);
    }
    

    public void readObject(java.io.ObjectInputStream in)
    throws IOException, ClassNotFoundException
    {
        String rkBaseName = (String)in.readObject();
        SetPassQuantity(1);
        m_kVShader.set(0, new VertexShader("TextureV"));
        m_kPShader.set(0, new PixelShader("ClassificationWidgetEffect", true));

        m_kPShader.get(0).SetTextureQuantity(1);
        m_kPShader.get(0).SetImageName(0,rkBaseName);
        m_kWidgetState = (ClassificationWidgetState)in.readObject();
    }
    
}
