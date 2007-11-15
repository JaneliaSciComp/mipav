package gov.nih.mipav.view.renderer.volumeview;

import gov.nih.mipav.view.WildMagic.LibGraphics.Effects.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Shaders.*;

public class MipavLightingEffect extends ShaderEffect
{

    /** Creates a LightingEffect */
    public MipavLightingEffect ()
    {
        super(1);
        m_kVShader.set(0, new VertexShader("MipavLighting"));
        m_kPShader.set(0, new PixelShader("PassThrough4"));
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

    /** Delete memory */
    public void dispose()
    {
        super.dispose();
    }
}

