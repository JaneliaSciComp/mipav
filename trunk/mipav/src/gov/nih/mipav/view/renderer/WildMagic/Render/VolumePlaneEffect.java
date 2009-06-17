package gov.nih.mipav.view.renderer.WildMagic.Render;


import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.view.renderer.J3D.PlaneRender;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibGraphics.Effects.ShaderEffect;
import WildMagic.LibGraphics.ObjectSystem.StreamInterface;
import WildMagic.LibGraphics.ObjectSystem.StringTree;
import WildMagic.LibGraphics.Shaders.PixelShader;
import WildMagic.LibGraphics.Shaders.Program;
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
    /** Shared volume data and textures. */
    private VolumeImage m_kVolumeImageA;

    /** Shared volume data and textures. */
    private VolumeImage m_kVolumeImageB;

    /** 
     * Creates a new VolumeShaderEffect object.
     * @param kVolumeImageA the VolumeImage containing shared data and textures for rendering.
     * @param kVolumeImageB second VolumeImage.
     */
    public VolumePlaneEffect ( VolumeImage kVolumeImageA, VolumeImage kVolumeImageB )
    {
        m_kVolumeImageA = kVolumeImageA;
        m_kVolumeImageB = kVolumeImageB;
        Init( false );
    }

    /** 
     * Creates a new VolumeShaderEffect object.
     * @param kVolumeImageA the VolumeImage containing shared data and
     * textures for rendering.
     * @param kVolumeImageB second VolumeImage.
     * @param bUnique when true the shader program must be unique.
     */
    public VolumePlaneEffect ( VolumeImage kVolumeImageA, VolumeImage kVolumeImageB, boolean bUnique )
    {
        m_kVolumeImageA = kVolumeImageA;
        m_kVolumeImageB = kVolumeImageB;
        Init( bUnique );
    }

    /**
     * Sets the blend factor shader parameter between imageA and imageB.
     * @param fBlend blend factor (range = 0-1).
     */
    public void Blend(float fBlend)
    {
        Program pkProgram = GetPProgram(0);
        if ( pkProgram != null && pkProgram.GetUC("Blend") != null ) 
        {
            pkProgram.GetUC("Blend").GetData()[0] = fBlend;
        }
    }

    /**
     * Sets the blend factor shader parameter between imageA and imageB.
     * @param fBlend blend factor (range = 0-1).
     */
    public void setABBlend(float fBlend)
    {
        Program pkProgram = GetPProgram(0);
        if ( (pkProgram != null) && pkProgram.GetUC("ABBlend") != null ) 
        {
            pkProgram.GetUC("ABBlend").GetData()[0] = fBlend;
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
        Program pkProgram = GetPProgram(0);
        if ( pkProgram != null && pkProgram.GetUC("Blend") != null ) 
        {
            return pkProgram.GetUC("Blend").GetData()[0];
        }
        return 1.0f;
    }

    /* (non-Javadoc)
     * @see WildMagic.LibGraphics.Effects.ShaderEffect#OnLoadPrograms(int, WildMagic.LibGraphics.Shaders.Program, WildMagic.LibGraphics.Shaders.Program)
     */
    public void OnLoadPrograms (int iPass, Program pkVProgram,
            Program pkPProgram)
    {
        Blend(1);
        setABBlend(1);
        SetBackgroundColor(ColorRGBA.BLACK);
        setRGBTA(null);
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("IsColorA") != null ) 
        {
            if ( m_kVolumeImageA.GetImage().isColorImage() )
            {
                pkProgram.GetUC("IsColorA").GetData()[0] = 1.0f;
            }
            else
            {
                pkProgram.GetUC("IsColorA").GetData()[0] = 0.0f;
            }
        }    
        if ( m_kVolumeImageB != null )
        {
            if ( pkProgram.GetUC("IsColorB") != null ) 
            {
                if ( m_kVolumeImageB.GetImage().isColorImage() )
                {
                    pkProgram.GetUC("IsColorB").GetData()[0] = 1.0f;
                }
                else
                {
                    pkProgram.GetUC("IsColorB").GetData()[0] = 0.0f;
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
        Program pkProgram = GetPProgram(0);
        if ( (pkProgram != null) && (pkProgram.GetUC("ColorLUTOnA") != null) ) 
        {
            if ( RGBT != null )
            {
                pkProgram.GetUC("ColorLUTOnA").GetData()[0] = RGBT.getROn() ? 1.0f : 0.0f;
                pkProgram.GetUC("ColorLUTOnA").GetData()[1] = RGBT.getGOn() ? 1.0f : 0.0f;
                pkProgram.GetUC("ColorLUTOnA").GetData()[2] = RGBT.getBOn() ? 1.0f : 0.0f;
            }
            else
            {
                pkProgram.GetUC("ColorLUTOnA").GetData()[0] = 1.0f;
                pkProgram.GetUC("ColorLUTOnA").GetData()[1] = 1.0f;
                pkProgram.GetUC("ColorLUTOnA").GetData()[2] = 1.0f;
            }
        }
    }    

    public void setRGBTB(ModelRGB RGBT) {
        Program pkProgram = GetPProgram(0);
        if ( (pkProgram != null) && (pkProgram.GetUC("ColorLUTOnB") != null) ) 
        {
            if ( RGBT != null )
            {
                pkProgram.GetUC("ColorLUTOnB").GetData()[0] = RGBT.getROn() ? 1.0f : 0.0f;
                pkProgram.GetUC("ColorLUTOnB").GetData()[1] = RGBT.getGOn() ? 1.0f : 0.0f;
                pkProgram.GetUC("ColorLUTOnB").GetData()[2] = RGBT.getBOn() ? 1.0f : 0.0f;
            }
            else
            {
                pkProgram.GetUC("ColorLUTOnB").GetData()[0] = 1.0f;
                pkProgram.GetUC("ColorLUTOnB").GetData()[1] = 1.0f;
                pkProgram.GetUC("ColorLUTOnB").GetData()[2] = 1.0f;                
            }
        }
    }

    /**
     * Turns rendering the planes with the surface mask on/off.
     * @param bOn on/off.
     */
    public void ShowSurface( boolean bOn )
    {
        Program pkProgram = GetPProgram(0);
        if ( pkProgram != null && pkProgram.GetUC("ShowSurface") != null ) 
        {
            pkProgram.GetUC("ShowSurface").GetData()[0] = bOn? 1 : 0;
        } 
    }    
    


    public void ZSlice( float fZ )
    {
        Program pkProgram = GetPProgram(0);
        if ( pkProgram != null && pkProgram.GetUC("ZSlice") != null ) 
        {
            pkProgram.GetUC("ZSlice").GetData()[0] = fZ;
        } 
        if ( pkProgram != null && pkProgram.GetUC("UseZSlice") != null ) 
        {
            pkProgram.GetUC("UseZSlice").GetData()[0] = 1;
        } 
    }
    /** Initializes the ShaderEffect vertex and pixel shader programs. */
    private void Init ( boolean bUnique )
    {
        /* Set single-pass rendering: */
        SetPassQuantity(1);

        SetVShader(0,new VertexShader("Color_Opacity_TextureV"));
        SetPShader(0,new PixelShader("Color_Opacity_TextureP", bUnique));

        GetPShader(0).SetTextureQuantity(5);
        int iTex = 0;
        GetPShader(0).SetImageName(iTex,m_kVolumeImageA.GetVolumeTarget().GetName());
        GetPShader(0).SetTexture(iTex++, m_kVolumeImageA.GetVolumeTarget() );
        GetPShader(0).SetImageName(iTex, m_kVolumeImageA.GetColorMapTarget().GetName() );
        GetPShader(0).SetTexture(iTex++, m_kVolumeImageA.GetColorMapTarget() );
        
        GetPShader(0).SetImageName(iTex, m_kVolumeImageA.GetSurfaceTarget().GetName() );
        GetPShader(0).SetTexture(iTex++, m_kVolumeImageA.GetSurfaceTarget() );

        if ( m_kVolumeImageB != null )
        {
            GetPShader(0).SetImageName(iTex,"VolumeImageB");
            GetPShader(0).SetTexture(iTex++, m_kVolumeImageB.GetVolumeTarget() );
            GetPShader(0).SetImageName(iTex, "ColorMapB");
            GetPShader(0).SetTexture(iTex++, m_kVolumeImageB.GetColorMapTarget() );
        }
    }
}
