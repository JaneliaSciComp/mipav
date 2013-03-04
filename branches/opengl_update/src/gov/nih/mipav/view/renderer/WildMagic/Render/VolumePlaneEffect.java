package gov.nih.mipav.view.renderer.WildMagic.Render;


import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.view.renderer.J3D.PlaneRender;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibGraphics.Effects.ShaderEffect;
import WildMagic.LibGraphics.ObjectSystem.StreamInterface;
import WildMagic.LibGraphics.ObjectSystem.StringTree;
import WildMagic.LibGraphics.Shaders.PixelShader;
import WildMagic.LibGraphics.Shaders.Program;
import WildMagic.LibGraphics.Shaders.Shader;
import WildMagic.LibGraphics.Shaders.VertexShader;

/** The VolumePlaneEffect ShaderEffect creates shaders for mapping the volume
 * data onto the planes for the 3-orthogonal planes displayed in the
 * VolumeViewer and for the PlaneRender objects.
 * @see GPUVolumeRender.java
 * @see VolumeViewer.java
 * @see PlaneRender.java
 */
public class VolumePlaneEffect extends ShaderEffect
implements StreamInterface
{
    /**  */
    private static final long serialVersionUID = -5399442879177045664L;

    /** Shared volume data and textures. */
    private VolumeImage m_kVolumeImageA;

    /** Shared volume data and textures. */
    private VolumeImage m_kVolumeImageB;

    /** 
     * Creates a new VolumeShaderEffect object.
     * @param kVolumeImageA the VolumeImage containing shared data and
     * textures for rendering.
     * @param kVolumeImageB second VolumeImage.
     * @param bUnique when true the shader program must be unique.
     */
    public VolumePlaneEffect ( VolumeImage kVolumeImageA, VolumeImage kVolumeImageB, boolean bUnique, boolean bTransparency )
    {
        m_kVolumeImageA = kVolumeImageA;
        m_kVolumeImageB = kVolumeImageB;
        Init( bTransparency );
    }

    /**
     * Sets the blend factor shader parameter between imageA and imageB.
     * @param fBlend blend factor (range = 0-1).
     */
    public void Blend(float fBlend)
    {
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram != null && pkCProgram.GetUC("Blend") != null ) 
        {
            pkCProgram.GetUC("Blend").GetData()[0] = fBlend;
        }
    }

    /**
     * memory cleanup.
     */
    public void dispose()
    {
        m_kVolumeImageA = null;
        m_kVolumeImageB = null;
        super.dispose();
    }
    
    /**
     * Sets the blend factor shader parameter between imageA and imageB.
     * @param fBlend blend factor (range = 0-1).
     */
    public float GetBlend()
    {
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram != null && pkCProgram.GetUC("Blend") != null ) 
        {
            return pkCProgram.GetUC("Blend").GetData()[0];
        }
        return 1.0f;
    }

    /* (non-Javadoc)
     * @see WildMagic.LibGraphics.Effects.ShaderEffect#OnLoadPrograms(int, WildMagic.LibGraphics.Shaders.Program, WildMagic.LibGraphics.Shaders.Program)
     */
    public void OnLoadPrograms (int iPass, Program pkVProgram,
            Program pkPProgram, Program pkCProgram)
    {
        Blend(1);
        setABBlend(1);
        SetBackgroundColor(ColorRGBA.BLACK);
        setRGBTA(null);
        if ( pkCProgram.GetUC("IsColorA") != null ) 
        {
            if ( m_kVolumeImageA.GetImage().isColorImage() )
            {
                pkCProgram.GetUC("IsColorA").GetData()[0] = 1.0f;
            }
            else
            {
                pkCProgram.GetUC("IsColorA").GetData()[0] = 0.0f;
            }
        }    
        if ( m_kVolumeImageB.GetImage() != null )
        {
            if ( pkCProgram.GetUC("IsColorB") != null ) 
            {
                if ( m_kVolumeImageB.GetImage().isColorImage() )
                {
                    pkCProgram.GetUC("IsColorB").GetData()[0] = 1.0f;
                }
                else
                {
                    pkCProgram.GetUC("IsColorB").GetData()[0] = 0.0f;
                }
            }  
            setABBlend(.5f);
            setRGBTB(null);
        }
    }

    /* (non-Javadoc)
     * @see WildMagic.LibGraphics.Effects.ShaderEffect#SaveStrings(java.lang.String)
     */
    public StringTree SaveStrings (final String acTitle)
    {
        StringTree pkTree = new StringTree();
        pkTree.Append(StringTree.Format("VolumePlaneEffect",GetName()));
        pkTree.Append(super.SaveStrings(null));

        return pkTree;
    }
    /**
     * Sets the blend factor shader parameter between imageA and imageB.
     * @param fBlend blend factor (range = 0-1).
     */
    public void setABBlend(float fBlend)
    {
        Program pkCProgram = GetCProgram(0);
        if ( (pkCProgram != null) && pkCProgram.GetUC("ABBlend") != null ) 
        {
            pkCProgram.GetUC("ABBlend").GetData()[0] = fBlend;
        }
    }

    /**
     * Sets the BackgroundColor shader parameter.
     * @param kColor new BackgroundColor.
     */
    public void SetBackgroundColor( ColorRGBA kColor )
    {
        /*
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("BackgroundColor") != null ) 
        {
            pkProgram.GetUC("BackgroundColor").GetData()[0] = kColor.R;
            pkProgram.GetUC("BackgroundColor").GetData()[1] = kColor.G;
            pkProgram.GetUC("BackgroundColor").GetData()[2] = kColor.B;
            pkProgram.GetUC("BackgroundColor").GetData()[3] = kColor.A;
        }
        */
    }


    public void setRGBTA(ModelRGB RGBT) {
        Program pkCProgram = GetCProgram(0);
        if ( (pkCProgram != null) && (pkCProgram.GetUC("ColorLUTOnA") != null) ) 
        {
            if ( RGBT != null )
            {
                pkCProgram.GetUC("ColorLUTOnA").GetData()[0] = RGBT.getROn() ? 1.0f : 0.0f;
                pkCProgram.GetUC("ColorLUTOnA").GetData()[1] = RGBT.getGOn() ? 1.0f : 0.0f;
                pkCProgram.GetUC("ColorLUTOnA").GetData()[2] = RGBT.getBOn() ? 1.0f : 0.0f;
            }
            else
            {
                pkCProgram.GetUC("ColorLUTOnA").GetData()[0] = 1.0f;
                pkCProgram.GetUC("ColorLUTOnA").GetData()[1] = 1.0f;
                pkCProgram.GetUC("ColorLUTOnA").GetData()[2] = 1.0f;
            }
        }
    }    

    public void setRGBTB(ModelRGB RGBT) {
        Program pkCProgram = GetCProgram(0);
        if ( (pkCProgram != null) && (pkCProgram.GetUC("ColorLUTOnB") != null) ) 
        {
            if ( RGBT != null )
            {
                pkCProgram.GetUC("ColorLUTOnB").GetData()[0] = RGBT.getROn() ? 1.0f : 0.0f;
                pkCProgram.GetUC("ColorLUTOnB").GetData()[1] = RGBT.getGOn() ? 1.0f : 0.0f;
                pkCProgram.GetUC("ColorLUTOnB").GetData()[2] = RGBT.getBOn() ? 1.0f : 0.0f;
            }
            else
            {
                pkCProgram.GetUC("ColorLUTOnB").GetData()[0] = 1.0f;
                pkCProgram.GetUC("ColorLUTOnB").GetData()[1] = 1.0f;
                pkCProgram.GetUC("ColorLUTOnB").GetData()[2] = 1.0f;                
            }
        }
    }

    /**
     * Turns rendering the planes with the surface mask on/off.
     * @param bOn on/off.
     */
    public void ShowSurface( boolean bOn )
    {
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram != null && pkCProgram.GetUC("ShowSurface") != null ) 
        {
            pkCProgram.GetUC("ShowSurface").GetData()[0] = bOn? 1 : 0;
        } 
    }    
    


    public void ZSlice( float fZ )
    {
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram != null && pkCProgram.GetUC("ZSlice") != null ) 
        {
            pkCProgram.GetUC("ZSlice").GetData()[0] = fZ;
        } 
        if ( pkCProgram != null && pkCProgram.GetUC("UseZSlice") != null ) 
        {
            pkCProgram.GetUC("UseZSlice").GetData()[0] = 1;
        } 
    }
    /** Initializes the ShaderEffect vertex and pixel shader programs. */
    private void Init ( boolean bTransparency )
    {
        /* Set single-pass rendering: */
        SetPassQuantity(1);
        SetVShader(0,new VertexShader("TextureV", Shader.vertexShaderTexture3));
        PixelShader kPShader = null;
        if ( !bTransparency )
        {
            kPShader = new PixelShader("Color_Opacity_TextureP");
        }
        else
        {
            kPShader = new PixelShader("Color_Opacity_Texture_TransparencyP");
        }
        SetPShader(0,kPShader);

        kPShader.SetTextureQuantity(5);
        int iTex = 0;
        kPShader.SetImageName(iTex,m_kVolumeImageA.GetVolumeTarget().GetName(), "bVolumeImageA");
        kPShader.SetTexture(iTex++, m_kVolumeImageA.GetVolumeTarget(), "bVolumeImageA" );
        kPShader.SetImageName(iTex, m_kVolumeImageA.GetColorMapTarget().GetName(), "cColorMapA" );
        kPShader.SetTexture(iTex++, m_kVolumeImageA.GetColorMapTarget(), "cColorMapA" );
        
        kPShader.SetImageName(iTex, m_kVolumeImageA.GetSurfaceTarget().GetName(), "iSurfaceImage" );
        kPShader.SetTexture(iTex++, m_kVolumeImageA.GetSurfaceTarget(), "iSurfaceImage" );

        if ( m_kVolumeImageB.GetImage() != null )
        {
            kPShader.SetImageName(iTex,"VolumeImageB", "jVolumeImageB");
            kPShader.SetTexture(iTex++, m_kVolumeImageB.GetVolumeTarget(), "jVolumeImageB" );
            kPShader.SetImageName(iTex, "ColorMapB", "kColorMapB");
            kPShader.SetTexture(iTex++, m_kVolumeImageB.GetColorMapTarget(), "kColorMapB" );
        }
    }
}
