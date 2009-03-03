package gov.nih.mipav.view.renderer.WildMagic.Render;

import WildMagic.LibGraphics.Shaders.PixelShader;
import WildMagic.LibGraphics.Shaders.Program;
import WildMagic.LibGraphics.Shaders.VertexShader;

/**
 * Surface clipping.
 */
public class SurfaceClipEffect extends VolumeClipEffect
{    
    /**
     * Create a surface clip effect. Used to generate the surface mask texture.
     */
    public SurfaceClipEffect ()
    {
        SetPassQuantity(1);
        m_kVShader.set(0, new VertexShader("SurfaceClipV", true) );
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
        Program pkProgram = GetVProgram(0);
        if ( (pkProgram != null) && (pkProgram.GetUC("SurfaceScale") != null) ) 
        {
            pkProgram.GetUC("SurfaceScale").GetData()[0] = fSX;
            pkProgram.GetUC("SurfaceScale").GetData()[1] = fSY;
            pkProgram.GetUC("SurfaceScale").GetData()[2] = fSZ;
            pkProgram.GetUC("SurfaceScale").GetData()[3] = bOn ? 1 : 0;
        }
    }
}

