package gov.nih.mipav.view.renderer.WildMagic.Render;

import WildMagic.LibGraphics.Effects.ShaderEffect;
import WildMagic.LibGraphics.Shaders.PixelShader;
import WildMagic.LibGraphics.Shaders.Program;
import WildMagic.LibGraphics.Shaders.VertexShader;

/**
 * MipavLightingEffect uses the lights defined in the Volume/Surface/Tri-Planar view in the light shader.
 */
public class MipavLightingEffect extends ShaderEffect
{

    /**  */
    private static final long serialVersionUID = 4315576697840371407L;

    /** Creates a MIPAV lighting effect. */
    public MipavLightingEffect ()
    {
        super(1);
        m_kVShader.set(0, new VertexShader("MipavLighting"));
        m_kPShader.set(0, new PixelShader("PassThrough4"));
    }

    /**
     * Set surface blend value.
     * @param fValue surface blend/transparency value.
     */
    public void Blend( float fValue )
    {
        Program pkProgram = GetCProgram(0);
        if ( pkProgram == null )
        {
            return;
        }
        if ( pkProgram.GetUC("Blend") != null)
        {
            pkProgram.GetUC("Blend").GetData()[0] = fValue;
        }
    }
    
    /** This function is called in LoadPrograms once the shader programs are
     * created.  It gives the ShaderEffect-derived classes a chance to do
     * any additional work to hook up the effect with the low-level objects.
     * @param iPass the ith rendering pass
     */
    public void OnLoadPrograms (int iPass, Program pkVProgram,
                                Program pkPProgram, Program pkCProgram)
    {
        Blend(1.0f);
    }
    
    /**
     * Sets the light type for the given light.
     * @param kLightType the name of the light to set (Light0, Light1, etc.)
     * @param afType the type of light (Ambient = 0, Directional = 1, Point = 2, Spot = 3).
     */
    public void SetLight( String kLightType, float[] afType )
    {
        Program pkProgram = GetCProgram(0);
        if ( pkProgram == null )
        {
            return;
        }
        if ( pkProgram.GetUC(kLightType) != null)
        {
            pkProgram.GetUC(kLightType).SetDataSource(afType);
        }
    }
}

