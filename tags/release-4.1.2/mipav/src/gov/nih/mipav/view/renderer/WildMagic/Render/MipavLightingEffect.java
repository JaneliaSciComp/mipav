package gov.nih.mipav.view.renderer.WildMagic.Render;

import WildMagic.LibGraphics.Effects.*;
import WildMagic.LibGraphics.Shaders.*;

public class MipavLightingEffect extends ShaderEffect
{

    /** Creates a LightingEffect */
    public MipavLightingEffect ()
    {
        this(false);
    }
    
    public MipavLightingEffect ( boolean bUnique )
    {
        super(1);
        m_kVShader.set(0, new VertexShader("MipavLighting", bUnique));
        m_kPShader.set(0, new PixelShader("PassThrough4"));
    }

    /** This function is called in LoadPrograms once the shader programs are
     * created.  It gives the ShaderEffect-derived classes a chance to do
     * any additional work to hook up the effect with the low-level objects.
     * @param iPass the ith rendering pass
     */
    public void OnLoadPrograms (int iPass, Program pkVProgram,
                                Program pkPProgram)
    {
        Blend(1.0f);
    }
    
    /**
     * Sets the light type for the given light.
     * @param kLightType, the name of the light to set (Light0, Light1, etc.)
     * @param afType, the type of light (Ambient = 0, Directional = 1, Point = 2, Spot = 3).
     */
    public void SetLight( String kLightType, float[] afType )
    {
        Program pkProgram = GetVProgram(0);
        if ( pkProgram == null )
        {
            return;
        }
        if ( pkProgram.GetUC(kLightType) != null)
        {
            pkProgram.GetUC(kLightType).SetDataSource(afType);
        }
    }
    
    public void Blend( float fValue )
    {
        Program pkProgram = GetVProgram(0);
        if ( pkProgram == null )
        {
            return;
        }
        if ( pkProgram.GetUC("Blend") != null)
        {
            pkProgram.GetUC("Blend").SetDataSource(new float[]{fValue,0,0,0});
        }
    }

    /** Delete memory */
    public void dispose()
    {
        super.dispose();
    }
}

