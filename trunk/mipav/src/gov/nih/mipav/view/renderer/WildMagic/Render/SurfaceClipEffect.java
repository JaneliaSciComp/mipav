package gov.nih.mipav.view.renderer.WildMagic.Render;

import WildMagic.LibGraphics.Shaders.PixelShader;
import WildMagic.LibGraphics.Shaders.Program;
import WildMagic.LibGraphics.Shaders.VertexShader;

/**
 * Surface clipping.
 */
public class SurfaceClipEffect extends VolumeClipEffect
{
    /** Scale the surface to fit the mask texture. */
    private float[] m_afSurfaceScale = new float[]{0f,0,0,0};
    
    /**
     * Create a surface clip effect. Used to generate the surface mask texture.
     */
    public SurfaceClipEffect ()
    {
        SetPassQuantity(1);
        m_kVShader.set(0, new VertexShader("SurfaceClip", true) );
        m_kPShader.set(0, new PixelShader("SurfaceClip") );
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
        m_afSurfaceScale[0] = fSX;
        m_afSurfaceScale[1] = fSY;
        m_afSurfaceScale[2] = fSZ;      
        m_afSurfaceScale[3] = bOn ? 1 : 0;
        Program pkProgram = GetVProgram(0);
        if ( (pkProgram != null) && (pkProgram.GetUC("SurfaceScale") != null) ) 
        {
            pkProgram.GetUC("SurfaceScale").SetDataSource(m_afSurfaceScale);
        }
    }
}

