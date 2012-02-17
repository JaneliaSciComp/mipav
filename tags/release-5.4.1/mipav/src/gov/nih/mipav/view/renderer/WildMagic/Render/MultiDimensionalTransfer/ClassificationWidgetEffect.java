
package gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer;

import gov.nih.mipav.view.CustomUIBuilder;
import gov.nih.mipav.model.structures.ModelLUT;

import java.io.IOException;

import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibGraphics.Effects.TextureEffect;
import WildMagic.LibGraphics.Rendering.GraphicsImage;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.Shaders.PixelShader;
import WildMagic.LibGraphics.Shaders.Program;
import WildMagic.LibGraphics.Shaders.VertexShader;

/**
 * This class sets up and communicates with the GLSL shader program used to render the
 * interior of the widget in the multi-histogram panel. The parameters that control how the widget is rendered
 * in the multi-histogram panel are also used to determine how the widget is applied to the volume rendered data
 * in the GLSL volume renderer shader program. Those parameters are encapsulated in the ClassificationWidgetState class
 * which is used to pass the information on to the volume render GLSL program.
 * 
 * This class sets up the histogram tool GLSL shader programs for rendering the widgets directly.
 */
public class ClassificationWidgetEffect extends TextureEffect
{
    /**  */
    private static final long serialVersionUID = -7141385452679118672L;
    /** Current state of the widget encapsulated in the GLSL parameters needed for the Volume renderer GLSL program: */
    private ClassificationWidgetState m_kWidgetState = new ClassificationWidgetState();


    /** Creates a new ClassificationWidgetEffect with the texture specified.
     * @param rkBaseName the name of the 2D Histogram texture image.
     */
    public ClassificationWidgetEffect (final String rkBaseName)
    {
        SetPassQuantity(1);
        m_kVShader.set(0, new VertexShader("TextureV"));
        m_kPShader.set(0, new PixelShader("ClassificationWidgetEffect", true));

        m_kPShader.get(0).SetTextureQuantity(2);
        m_kPShader.get(0).SetImageName(0,rkBaseName);
        m_kWidgetState.UseWidget[0] = 1.0f;

        m_kWidgetState.UseColorMap[0] = -1.0f;
    }

    /** Creates a new ClassificationWidgetEffect with the texture specified.
     * @param rkBaseName the name of the 2D Histogram texture image.
     */
    public ClassificationWidgetEffect (final String rkBaseName,boolean bCircle)
    {
        SetPassQuantity(1);
        m_kVShader.set(0, new VertexShader("TextureV"));
        if ( bCircle )
        {
        	m_kPShader.set(0, new PixelShader("CircleClassificationWidgetEffect", true));
        }
        else
        {
        	m_kPShader.set(0, new PixelShader("ClassificationWidgetEffect", true));
        }

        m_kPShader.get(0).SetTextureQuantity(2);
        m_kPShader.get(0).SetImageName(0,rkBaseName);
        m_kWidgetState.UseWidget[0] = 1.0f;

        m_kWidgetState.UseColorMap[0] = -1.0f;
    }


    /**
     * Creates a new ClassificationWidgetEffect based on the input ClassificationWidgetEffect
     * @param kEffect ClassificationWidgetEffect, used to provide the name of the 2D Histogram texture,
     * as well as the current state of the Widget parameters.
     */
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
    
    /* (non-Javadoc)
     * @see WildMagic.LibGraphics.Effects.ShaderEffect#dispose()
     */
    public void dispose()
    {
        m_kWidgetState.dispose();
        m_kWidgetState = null;
        super.dispose();
    }
    
    /**
     * Returns the current ClassificationWidgetState GLSL Shader program parameters.
     * @return the current ClassificationWidgetState GLSL Shader program parameters.
     */
    public ClassificationWidgetState getState()
    {
        return m_kWidgetState;
    }
    
    /**
     * Sets the ClassificationWidgetState, representing the GLSL shader program parameters.
     * @param kState ClassificationWidgetState, representing the GLSL shader program parameters.
     */
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

    /**
     * Returns the color of the widget color transfer function.
     * @return the color of the widget color transfer function.
     */
    public ColorRGBA GetColor( ) 
    {
        return new ColorRGBA (
                m_kWidgetState.Color[0],
                m_kWidgetState.Color[1],
                m_kWidgetState.Color[2],
                m_kWidgetState.Color[3] );
    }

    public void SetAlpha( float fA ) 
    {
    	m_kWidgetState.Color[3] = fA;
    	if ( fA == 0 )
    	{
    		m_kWidgetState.UseWidget[0] = 0;
    	}
    	else
    	{
    		m_kWidgetState.UseWidget[0] = 1;
    	}
    	UpdateColor();
    }
    

    /**
     * Sets the color of the widget color transfer function.
     * @param the color of the widget color transfer function.
     */
    public void SetColor( float fR, float fG, float fB, float fA ) 
    {
    	m_kWidgetState.Color[0] = fR;
    	m_kWidgetState.Color[1] = fG;
    	m_kWidgetState.Color[2] = fB;
    	m_kWidgetState.Color[3] = fA;
    	if ( fA == 0 )
    	{
    		m_kWidgetState.UseWidget[0] = 0;
    	}
    	else
    	{
    		m_kWidgetState.UseWidget[0] = 1;
    	}
    	m_kWidgetState.UseColorMap[0] = -1.0f;
    	UpdateColor();
    }


    public void SetLUT( Texture kMap, int index, boolean bReverse )
    {
		m_kPShader.get(0).SetImageName(1,kMap.GetName());
		m_kPShader.get(0).SetTexture(1,kMap);
		
    	m_kWidgetState.UseColorMap[0] = index;
    	m_kWidgetState.InvertLUT = bReverse;
    	UpdateLUT();
    }


	public int GetLUTIndex( )
	{
		return (int)m_kWidgetState.UseColorMap[0];
	}
	
    /**
     * Updates the color in the GLSL shader program used to render the ClassificationWidget. 
     */
    public void UpdateColor( ) 
    {
        Program pkCProgram = GetCProgram(0);
        if ( (pkCProgram != null) && (pkCProgram.GetUC("LevColor") != null) ) 
        {
            pkCProgram.GetUC("LevColor").SetDataSource(m_kWidgetState.Color);
        }
    }

    public void UpdateLUT( ) 
    {
        Program pkCProgram = GetCProgram(0);
        if ( (pkCProgram != null) && (pkCProgram.GetUC("UseColorMap") != null) ) 
        {
            pkCProgram.GetUC("UseColorMap").SetDataSource(m_kWidgetState.UseColorMap);
        }
    }
    

    /**
     * Sets the mid-line parameter to the GLSL Shader Program.
     * @param fX1 bottom x-coordinate in Texture Coordinates.
     * @param fY1 bottom y-coordinate in Texture Coordinates.
     * @param fX2 top x-coordinate in Texture Coordinates.
     * @param fY2 top y-coordinate in Texture Coordinates.
     */
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
            //System.err.println( "EFFECT : LevMidLine " + m_kWidgetState.MidLine[0] +
            //		" "  + m_kWidgetState.MidLine[1] +
            //		" "  + m_kWidgetState.MidLine[2] +
            //		" "  + m_kWidgetState.MidLine[3] );
            
        }
    }
    public void SetCenter( float fX, float fY ) 
    {
        m_kWidgetState.Center[0] = fX;
        m_kWidgetState.Center[1] = fY;
        Program pkCProgram = GetCProgram(0);
        if ( (pkCProgram != null) && (pkCProgram.GetUC("Center") != null) ) 
        {
            pkCProgram.GetUC("Center").SetDataSource(m_kWidgetState.Center); 
            //System.err.println( "Center " + fX + " " + fY );
        }
    }
    public void SetRadius( float fRX, float fRY ) 
    {
        m_kWidgetState.Radius[0] = fRX;
        m_kWidgetState.Radius[1] = fRY;
        Program pkCProgram = GetCProgram(0);
        if ( (pkCProgram != null) && (pkCProgram.GetUC("Radius") != null) ) 
        {
            pkCProgram.GetUC("Radius").SetDataSource(m_kWidgetState.Radius); 
            //System.err.println( "Radius " + fR );
        }
    }
    

    /**
     * Sets the left-line parameter to the GLSL Shader Program.
     * @param fX1 bottom x-coordinate in Texture Coordinates.
     * @param fY1 bottom y-coordinate in Texture Coordinates.
     * @param fX2 top x-coordinate in Texture Coordinates.
     * @param fY2 top y-coordinate in Texture Coordinates.
     */
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
            //System.err.println( "EFFECT : LevLeftLine " + m_kWidgetState.LeftLine[0] +
            //		" "  + m_kWidgetState.LeftLine[1] +
            //		" "  + m_kWidgetState.LeftLine[2] +
            //		" "  + m_kWidgetState.LeftLine[3] );
        }
    }
    

    /**
     * Sets the right-line parameter to the GLSL Shader Program.
     * @param fX1 bottom x-coordinate in Texture Coordinates.
     * @param fY1 bottom y-coordinate in Texture Coordinates.
     * @param fX2 top x-coordinate in Texture Coordinates.
     * @param fY2 top y-coordinate in Texture Coordinates.
     */
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
            //System.err.println( "EFFECT : LevRightLine " + m_kWidgetState.RightLine[0] +
            //		" "  + m_kWidgetState.RightLine[1] +
            //		" "  + m_kWidgetState.RightLine[2] +
            //		" "  + m_kWidgetState.RightLine[3] );
        }
    }
    
    
    /**
     * Sets the contribution of the 2nd derivative to the volume rendering.
     * @param fAlpha the contribution of the 2nd derivative to the volume rendering.
     */
    public void setBoundary( float fAlpha )
    {
        m_kWidgetState.BoundaryEmphasis[0] = fAlpha;
        Program pkCProgram = GetCProgram(0);
        if ( (pkCProgram != null) && (pkCProgram.GetUC("BoundaryEmphasis") != null) ) 
        {
            pkCProgram.GetUC("BoundaryEmphasis").SetDataSource(m_kWidgetState.BoundaryEmphasis);
        }
    }
    
    /**
     * Computes the input parameters to the GLSL shader program based on the ClassificationWidgetState and passes
     * them to the program to render the widget in the multi-histogram panel.
     */
    protected void computeUniformVariables()
    {
    	m_kWidgetState.Shift[0] = 0;
    	m_kWidgetState.Shift[1] = 0;
        if ( m_kWidgetState.MidLine[1] == m_kWidgetState.MidLine[3] )
        {
            float fIncr = (m_kWidgetState.MidLine[1] - m_kWidgetState.LeftLine[1]) /
            (m_kWidgetState.LeftLine[3] - m_kWidgetState.LeftLine[1]);

            fIncr = fIncr * (m_kWidgetState.RightLine[0] - m_kWidgetState.LeftLine[0]);

            float fShiftX = (m_kWidgetState.MidLine[0] - m_kWidgetState.LeftLine[0]) /
            (m_kWidgetState.RightLine[0] - m_kWidgetState.LeftLine[0]);
            m_kWidgetState.Shift[0] = (fShiftX)*fIncr;
            m_kWidgetState.Shift[1] = (1.0f-fShiftX)*fIncr;
        }
        Program pkCProgram = GetCProgram(0);
        if ( (pkCProgram != null) && (pkCProgram.GetUC("Shift") != null) ) 
        {
            pkCProgram.GetUC("Shift").SetDataSource(m_kWidgetState.Shift);
            //System.err.println( "EFFECT : Shift " + pkCProgram.GetUC("Shift").GetData()[0] + " " + pkCProgram.GetUC("Shift").GetData()[1] );
        }
        if ( (pkCProgram != null) && (pkCProgram.GetUC("InvY0MY1") != null) ) 
        {            
        	m_kWidgetState.YRatio[0] = 1.0f / (m_kWidgetState.LeftLine[1] - m_kWidgetState.LeftLine[3]);
        	m_kWidgetState.YRatio[1] = 1.0f / (m_kWidgetState.MidLine[1] - m_kWidgetState.MidLine[3]);
        	m_kWidgetState.YRatio[2] = 1.0f / (m_kWidgetState.RightLine[1] - m_kWidgetState.RightLine[3]);
            pkCProgram.GetUC("InvY0MY1").SetDataSource(m_kWidgetState.YRatio);
            //System.err.println( "EFFECT : InvY0MY1 " + pkCProgram.GetUC("InvY0MY1").GetData()[0] + " " + pkCProgram.GetUC("InvY0MY1").GetData()[1] + " " + pkCProgram.GetUC("InvY0MY1").GetData()[2]);
            //System.err.println( "" );
        }
    }

    /**
     * Stream this object to disk.
     * @param out
     * @throws IOException
     */
    public void writeObject(java.io.ObjectOutputStream out)
    throws IOException 
    {
        out.writeObject( m_kPShader.get(0).GetImageName(0) );
        out.writeObject(m_kWidgetState);
    }
    

    /**
     * Read this object from disk.
     * @param in
     * @throws IOException
     * @throws ClassNotFoundException
     */
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
