package gov.nih.mipav.view.renderer.WildMagic.Render;


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

    /** stores the blend function */
    private float[] m_afBlend = new float[]{1f,0,0,0};

    /** stores the background color */
    private float[] m_afBackgroundColor = new float[4];

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
        m_afBlend[0] = fBlend;
        Program pkProgram = GetPProgram(0);
        if ( pkProgram != null && pkProgram.GetUC("blend") != null ) 
        {
            pkProgram.GetUC("blend").SetDataSource(m_afBlend);
        }
    }

    /**
     * memory cleanup.
     */
    public void dispose()
    {
        m_afBlend = null;
        super.dispose();
    }

    /**
     * Sets the blend factor shader parameter between imageA and imageB.
     * @param fBlend blend factor (range = 0-1).
     */
    public float GetBlend()
    {
        return m_afBlend[0];
    }

    /* (non-Javadoc)
     * @see WildMagic.LibGraphics.Effects.ShaderEffect#OnLoadPrograms(int, WildMagic.LibGraphics.Shaders.Program, WildMagic.LibGraphics.Shaders.Program)
     */
    public void OnLoadPrograms (int iPass, Program pkVProgram,
            Program pkPProgram)
    {
        Blend(1);
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("IsColor") != null ) 
        {
            if ( m_kVolumeImageA.GetImage().isColorImage() )
            {
                pkProgram.GetUC("IsColor").SetDataSource(new float[]{1,0,0,0});
            }
            else
            {
                pkProgram.GetUC("IsColor").SetDataSource(new float[]{0,0,0,0});
            }
        }    
        if ( m_kVolumeImageB != null )
        {
            if ( pkProgram.GetUC("IsColorB") != null ) 
            {
                if ( m_kVolumeImageB.GetImage().isColorImage() )
                {
                    pkProgram.GetUC("IsColorB").SetDataSource(new float[]{1,0,0,0});
                }
                else
                {
                    pkProgram.GetUC("IsColorB").SetDataSource(new float[]{0,0,0,0});
                }
            }  
            if ( pkProgram.GetUC("ShowB") != null ) 
            {    
                pkProgram.GetUC("ShowB").SetDataSource(new float[]{1,0,0,0});
            }               
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
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("BackgroundColor") != null ) 
        {
            m_afBackgroundColor[0] = kColor.R;
            m_afBackgroundColor[1] = kColor.G;
            m_afBackgroundColor[2] = kColor.B;
            m_afBackgroundColor[3] = kColor.A;
            pkProgram.GetUC("BackgroundColor").SetDataSource(m_afBackgroundColor);
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
    /** Initializes the ShaderEffect vertex and pixel shader programs. */
    private void Init ( boolean bUnique )
    {
        /* Set single-pass rendering: */
        SetPassQuantity(1);

        SetVShader(0,new VertexShader("Color_Opacity_TextureV"));
        SetPShader(0,new PixelShader("Color_Opacity_TextureP", bUnique));

        GetPShader(0).SetTextureQuantity(3);
        //GetPShader(0).SetTextureQuantity(5);
        int iTex = 0;
        GetPShader(0).SetImageName(iTex,m_kVolumeImageA.GetVolumeTarget().GetName());
        GetPShader(0).SetTexture(iTex++, m_kVolumeImageA.GetVolumeTarget() );
        GetPShader(0).SetImageName(iTex, m_kVolumeImageA.GetColorMapTarget().GetName() );
        GetPShader(0).SetTexture(iTex++, m_kVolumeImageA.GetColorMapTarget() );
/*
        if ( m_kVolumeImageB != null )
        {
            GetPShader(0).SetImageName(2,"VolumeImageB");
            GetPShader(0).SetTexture(2, m_kVolumeImageB.GetVolumeTarget() );
            GetPShader(0).SetImageName(3, "ColorMapB");
            GetPShader(0).SetTexture(3, m_kVolumeImageB.GetColorMapTarget() );
        }
*/
        GetPShader(0).SetImageName(iTex, m_kVolumeImageA.GetSurfaceTarget().GetName() );
        GetPShader(0).SetTexture(iTex++, m_kVolumeImageA.GetSurfaceTarget() );
    }
}
