package gov.nih.mipav.view.renderer.WildMagic.Render;

import WildMagic.LibGraphics.Shaders.*;

public class SurfaceClipEffect extends VolumeClipEffect
{
    public SurfaceClipEffect ()
    {
        SetPassQuantity(1);
        m_kVShader.set(0, new VertexShader("SurfaceClip", true) );
        m_kPShader.set(0, new PixelShader("SurfaceClip") );
    }
    

    /** turns surface display on/off */
    private float[] m_afSurfaceScale = new float[]{0f,0,0,0};
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

