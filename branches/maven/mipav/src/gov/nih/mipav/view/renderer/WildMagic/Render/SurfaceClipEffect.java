package gov.nih.mipav.view.renderer.WildMagic.Render;

import WildMagic.LibGraphics.Shaders.PixelShader;
import WildMagic.LibGraphics.Shaders.Program;
import WildMagic.LibGraphics.Shaders.VertexShader;

/**
 * Surface clipping.
 */
public class SurfaceClipEffect extends VolumeClipEffect
{    
    /**  */
    private static final long serialVersionUID = 8502686732859951115L;
    /**
     * Create a surface clip effect. Used to generate the surface mask texture.
     */
    public SurfaceClipEffect ()
    {
        SetPassQuantity(1);
        m_kVShader.set(0, new VertexShader("SurfaceClipV") );
        m_kPShader.set(0, new PixelShader("SurfaceClipP") );
    }
    /**
     * Scale the surface to fit the mask texture. Scaling is done inside the SurfaceClip vertex shader.
     * @param fSX x-scale factor.
     * @param fSY y-scale factor.
     * @param fSZ z-scale factor.
     * @param bOn turn scale on/off.
     */
    public void Scale( float fSX, float fSY, float fSZ, boolean bOn )
    {
        Program pkCProgram = GetCProgram(0);
        if ( (pkCProgram != null) && (pkCProgram.GetUC("SurfaceScale") != null) ) 
        {
            pkCProgram.GetUC("SurfaceScale").GetData()[0] = fSX;
            pkCProgram.GetUC("SurfaceScale").GetData()[1] = fSY;
            pkCProgram.GetUC("SurfaceScale").GetData()[2] = fSZ;
            pkCProgram.GetUC("SurfaceScale").GetData()[3] = bOn ? 1 : 0;
        }
    }
}

