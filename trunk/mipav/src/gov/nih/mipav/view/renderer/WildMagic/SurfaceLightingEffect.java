package gov.nih.mipav.view.renderer.WildMagic;

import gov.nih.mipav.view.WildMagic.LibGraphics.Effects.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.Renderer;
import gov.nih.mipav.view.WildMagic.LibGraphics.Shaders.*;

public class SurfaceLightingEffect extends VolumeClipEffect
{

    /** Creates a LightingEffect */
    public SurfaceLightingEffect ()
    {
        SetPassQuantity(1);
        m_kVVertexLighting = new VertexShader("MipavLighting");
        m_kPVertexLighting = new PixelShader("PassThrough4");
        
        m_kVPixelLighting = new VertexShader("MipavLightingFragment");
        m_kPPixelLighting = new PixelShader("MipavLightingFragment");
        m_kVShader.set(0, m_kVVertexLighting);
        m_kPShader.set(0, m_kPVertexLighting);
    }

    /** This function is called in LoadPrograms once the shader programs are
     * created.  It gives the ShaderEffect-derived classes a chance to do
     * any additional work to hook up the effect with the low-level objects.
     * @param iPass the ith rendering pass
     */
    public void OnLoadPrograms (int iPass, Program pkVProgram,
                                Program pkPProgram)
    {
        Blend(m_fBlend);
    }
    
    /**
     * Sets the light type for the given light.
     * @param kLightType, the name of the light to set (Light0, Light1, etc.)
     * @param afType, the type of light (Ambient = 0, Directional = 1, Point = 2, Spot = 3).
     */
    public void SetLight( String kLightType, float[] afType )
    {
        Program pkProgram = null;
        if ( m_bPerPixelLighting )
        {
            pkProgram = GetPProgram(0);
        }
        else
        {
            pkProgram = GetVProgram(0);
        }
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
        Program pkProgram = null;
        if ( m_bPerPixelLighting )
        {
            pkProgram = GetPProgram(0);
        }
        else
        {
            pkProgram = GetVProgram(0);
        }
        if ( pkProgram == null )
        {
            return;
        }
        if ( pkProgram.GetUC("Blend") != null)
        {
            pkProgram.GetUC("Blend").SetDataSource(new float[]{fValue,0,0,0});
        }
        m_fBlend = fValue;
    }

    public void SetPerPixelLighting( Renderer kRenderer, boolean bOn )
    {
        if ( m_bPerPixelLighting != bOn)
        {
            m_bPerPixelLighting = bOn;
            if ( m_bPerPixelLighting )
            {
                m_kVShader.set(0, m_kVPixelLighting);
                m_kPShader.set(0, m_kPPixelLighting);
            }
            else
            {
                m_kVShader.set(0, m_kVVertexLighting);
                m_kPShader.set(0, m_kPVertexLighting);
            }
            LoadPrograms(kRenderer, 0,kRenderer.GetMaxColors(),
                    kRenderer.GetMaxTCoords(),
                    kRenderer.GetMaxVShaderImages(),
                    kRenderer.GetMaxPShaderImages());
            Blend(m_fBlend);
            ResetClip();
        }
    }
    
    public void SetClipping( boolean bClip )
    {
        Program pkProgram = null;
        if ( m_bPerPixelLighting )
        {
            pkProgram = GetPProgram(0);
        }
        else
        {
            pkProgram = GetVProgram(0);
        }
        if ( pkProgram == null )
        {
            return;
        }
        if ( pkProgram.GetUC("ClipEnabled") != null)
        {
            if ( bClip )
            {
                pkProgram.GetUC("ClipEnabled").SetDataSource(new float[]{1,0,0,0});
            }
            else
            {
                pkProgram.GetUC("ClipEnabled").SetDataSource(new float[]{0,0,0,0});
            }
        }
    }

    
    /** Delete memory */
    public void dispose()
    {
        super.dispose();
    }

    private float m_fBlend = 1.0f;
    private VertexShader m_kVVertexLighting;
    private PixelShader m_kPVertexLighting;
    private VertexShader m_kVPixelLighting;
    private PixelShader m_kPPixelLighting;
    private boolean m_bPerPixelLighting = false;
}

