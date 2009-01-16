package gov.nih.mipav.view.renderer.WildMagic.Render;


import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibGraphics.ObjectSystem.StreamInterface;
import WildMagic.LibGraphics.ObjectSystem.StringTree;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.Shaders.PixelProgramCatalog;
import WildMagic.LibGraphics.Shaders.PixelShader;
import WildMagic.LibGraphics.Shaders.Program;
import WildMagic.LibGraphics.Shaders.VertexProgramCatalog;
import WildMagic.LibGraphics.Shaders.VertexShader;

/** 
 * VolumeShaderEffect is the workhorse of the GPU-based rendering in MIPAV. It
 * manages several Cg shaders for volume rendering. Each of the different
 * volume modes MIP, DDR, Composite, Surface, and Composite Surface
 * are implemented with different Cg shaders. The volume data and lookup table
 * information are stored and passed to the shaders as texture images;
 * VolumeShaderEffect manages these images. All UserConstant shader parameters
 * are also managed by the VolumeShaderEffect class.
 */
public class VolumeShaderEffect_WM extends VolumeClipEffect
    implements StreamInterface
{
    /** View Mode MIP Constant: */
    private final static int MIP = 0;
    /** View Mode DDR Constant: */
    private final static int DDR = 1;
    /** View Mode Composite Constant: */
    private final static int CMP = 2;
    /** View Mode Surface Constant: */
    private final static int SUR = 3;
    /** View Mode Composite-Surface Constant: */
    private final static int CMP_SUR = 4;


    /** Shared volume data and textures. */
    private VolumeImage m_kVolumeImageA;
    
    /** Shared volume data and textures. */
    private VolumeImage m_kVolumeImageB;

    /** PixelShader program and data for MIP mode: */
    private PixelShader m_kPShaderMIP = null;


    /** PixelShader program and data for DDR mode: */
    private PixelShader m_kPShaderDDR = null;

    /** PixelShader program and data for Composite mode: */
    private PixelShader m_kPShaderCMP = null;

    /** PixelShader program and data for Composite Surface and Surface modes: */
    private PixelShader m_kPShaderSUR = null;

    /** Indicates which shader to use (MIP, DDR, CMP, SUR, CMP_SUR): */
    private int m_iWhichShader = -1;

    /** Reference to the SceneImage texture: */
    private Texture m_kSceneTarget;

    /** stores the steps size */
    private float[] stepsSize = new float[]{450f,0,0,0};

    /** stores the background color */
    private ColorRGBA m_kBackgroundColor = ColorRGBA.BLACK;
    


    /** stores the self-shadow parameter on/off value: */
    private float[] m_afSelfShadow = new float[]{0,0,0,0};

    /** stores the gradient magnitude filter on/off value: */
    private float[] m_afGradientMagnitude = new float[]{0,0,0,0};

    /** Blend value for alpha-blending volume with other objects in the scene. */
    private float[] m_afBlend = new float[]{1,0,0,0};

    /** Blend value for alpha-blending two volumes. */
    private float[] m_afABBlend = new float[]{1,0,0,0};

    /** 
     * Creates a new VolumeShaderEffect object.
     * @param kImageA the VolumeImage containing the data and textures for
     * rendering.
     * @param kVolumeImageB second VolumeImage.
     * @param kSceneTarget the SceneImage texture with the back-facing polygon texture coordinates.
     */
    public VolumeShaderEffect_WM ( VolumeImage kVolumeImageA, VolumeImage kVolumeImageB, 
                                Texture kSceneTarget )
    {
        m_kVolumeImageA = kVolumeImageA;
        m_kVolumeImageB = kVolumeImageB;
        m_kSceneTarget = kSceneTarget;
        CreateVolumeTexture();
    }
    

    /**
     * Sets the blend factor shader parameter between imageA and imageB.
     * @param fBlend blend factor (range = 0-1).
     */
    public void Blend(float fBlend)
    {
        m_afBlend[0] = fBlend;
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("blend") != null ) 
        {
            pkProgram.GetUC("blend").SetDataSource(m_afBlend);
        }
    }

    /**
     * Change to the Composite mode pixel shader program.
     * @param kRenderer the Renderer displaying the scene-graph, to which the
     * new shader program is passed.
     */
    public void CMPMode(WildMagic.LibGraphics.Rendering.Renderer kRenderer )
    {
        if ( m_iWhichShader == CMP )
        {
            return;
        }
        m_iWhichShader = CMP;
        SetProgram(m_kPShaderCMP, kRenderer);
    }

    
    /**
     * Change to the DDR mode pixel shader program.
     * @param kRenderer the Renderer displaying the scene-graph, to which the
     * new shader program is passed.
     */
    public void DDRMode(WildMagic.LibGraphics.Rendering.Renderer kRenderer )
    {
        if ( m_iWhichShader == DDR )
        {
            return;
        }
        m_iWhichShader = DDR;
        SetProgram( m_kPShaderDDR, kRenderer );
    }

    /**
     * memory cleanup.
     */
    public void dispose()
    {
        if ( m_kPShaderMIP != null )
        {
            m_kPShaderMIP.dispose();
            m_kPShaderMIP = null;
        }
        if ( m_kPShaderDDR != null )
        {
            m_kPShaderDDR.dispose();
            m_kPShaderDDR = null;
        }
        if ( m_kPShaderCMP != null )
        {
            m_kPShaderCMP.dispose();
            m_kPShaderCMP = null;
        }
        if ( m_kPShaderSUR != null )
        {
            m_kPShaderSUR.dispose();
            m_kPShaderSUR = null;
        }

        m_kSceneTarget = null;
        m_afBlend = null;
        m_kBackgroundColor = null;

        m_afSelfShadow = null;
        m_afGradientMagnitude = null;

        super.dispose();
    }

    /**
     * Returns the current pixel program.
     * @return the current pixel program.
     */
    public Program GetPProgram()
    {
        return GetPProgram(0);
    }

    /**
     * Change to the MIP mode pixel shader program.
     * @param kRenderer the Renderer displaying the scene-graph, to which the
     * new shader program is passed.
     */
    public void MIPMode(WildMagic.LibGraphics.Rendering.Renderer kRenderer )
    {
        if ( m_iWhichShader == MIP )
        {
            return;
        }
        m_iWhichShader = MIP;
        SetProgram( m_kPShaderMIP, kRenderer );
    }

    /**
     * Reload the current shader programs from disk, compile and parse and
     * send to the GPU.
     * @param kRenderer the Renderer object displaying the scene-graph which
     * will apply the shader programs.
     */
    public void Reload( WildMagic.LibGraphics.Rendering.Renderer kRenderer )
    {
        Program kVProgram = GetVProgram(0);
        kVProgram.Release();
        VertexProgramCatalog.GetActive().Remove(kVProgram);

        Program kPProgram = GetPProgram(0);
        kPProgram.Release();
        PixelProgramCatalog.GetActive().Remove(kPProgram);

        VertexShader pkVShader = GetVShader(0);
        pkVShader.OnReleaseProgram();
        PixelShader pkPShader = GetPShader(0);
        pkPShader.OnReleaseProgram();

        LoadPrograms(kRenderer, 0,kRenderer.GetMaxColors(),
                     kRenderer.GetMaxTCoords(),
                     kRenderer.GetMaxVShaderImages(),
                     kRenderer.GetMaxPShaderImages());

        if ( m_iWhichShader == MIP )
        {
            m_iWhichShader = -1;
            MIPMode(kRenderer);
        }
        else if ( m_iWhichShader == DDR )
        {
            m_iWhichShader = -1;
            DDRMode(kRenderer);
        }
        else if ( m_iWhichShader == CMP )
        {
            m_iWhichShader = -1;
            CMPMode(kRenderer);
        }
        else if ( m_iWhichShader == SUR )
        {
            m_iWhichShader = -1;
            SURFASTMode(kRenderer);
        }
        else if ( m_iWhichShader == CMP_SUR )
        {
            m_iWhichShader = -1;
            SURMode(kRenderer);
        }
    }


    /**
     * Write this object into a StringTree for the scene-graph visualization.
     * @param acTitle the header for this object in the StringTree.
     * @return StringTree containing a String-based representation of this
     * object and it's children.
     */
    public StringTree SaveStrings (final String acTitle)
    {
        StringTree pkTree = new StringTree();
        pkTree.Append(StringTree.Format("VolumeShaderEffect",GetName()));
        pkTree.Append(super.SaveStrings(null));

        return pkTree;
    }

    /** 
     * Enables/Disables self-shadowing for the Surface mode.
     * @param bShadow self-shadowing on/off.
     */
    public void SelfShadow( boolean bShadow )
    {
        m_afSelfShadow[0] = 0;
        if ( bShadow )
        {
            m_afSelfShadow[0] = 1;
        }
        SetSelfShadow();
    }
    /**
     * Sets the blend factor shader parameter between imageA and imageB.
     * @param fBlend blend factor (range = 0-1).
     */
    public void setABBlend(float fBlend)
    {
        m_afABBlend[0] = fBlend;
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("ABBlend") != null ) 
        {
            pkProgram.GetUC("ABBlend").SetDataSource(m_afABBlend);
        }
    }

    /**
     * Sets the BackgroundColor shader parameter.
     * @param kColor new BackgroundColor.
     */
    public void SetBackgroundColor( ColorRGBA kColor )
    {
        m_kBackgroundColor = kColor;
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("BackgroundColor") != null ) 
        {
            float[] afColor = new float[4];
            afColor[0] = kColor.R;
            afColor[1] = kColor.G;
            afColor[2] = kColor.B;
            afColor[3] = kColor.A;
            pkProgram.GetUC("BackgroundColor").SetDataSource(afColor);
        }
    }
    /** 
     * Enables/Disables gradient magnitude filter.
     * @param bShow gradient magnitude filter on/off.
     */
    public void SetGradientMagnitude(boolean bShow)
    {
        m_afGradientMagnitude[0] = 0;
        if ( bShow )
        {
            m_afGradientMagnitude[0] = 1;
        }
        SetGradientMagnitude();
    }
    /**
     * Sets the light type for the given light.
     * @param kLightType the name of the light to set (Light0, Light1, etc.)
     * @param afType the type of light (Ambient = 0, Directional = 1, Point = 2, Spot = 3).
     */
    public void SetLight( String kLightType, float[] afType )
    {
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC(kLightType) != null)
        {
            pkProgram.GetUC(kLightType).SetDataSource(afType);
        }
    }
    /**
     * Sets the steps size factor shader parameter.
     * @param stepsSize blend factor (range = 0-1).
     */
    public void setSteps(float fsteps)
    {
        stepsSize[0] = fsteps;
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("steps") != null ) 
        {
            pkProgram.GetUC("steps").SetDataSource(stepsSize);
        }
    }

    /**
     * Change to the Surface mode pixel shader program.
     * @param kRenderer the Renderer displaying the scene-graph, to which the
     * new shader program is passed.
     */
    public void SURFASTMode(WildMagic.LibGraphics.Rendering.Renderer kRenderer )
    {
        if ( m_iWhichShader == SUR )
        {
            return;
        }
        m_iWhichShader = SUR;
        SetProgram(m_kPShaderSUR, kRenderer);
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("Composite") != null )
        {
            pkProgram.GetUC("Composite").SetDataSource(new float[] {0,0,0,0});
        }
        if ( pkProgram.GetUC("CompositeA") != null )
        {
            pkProgram.GetUC("CompositeA").SetDataSource(new float[] {0,0,0,0});
        }
        if ( pkProgram.GetUC("CompositeB") != null )
        {
            pkProgram.GetUC("CompositeB").SetDataSource(new float[] {0,0,0,0});
        }
    }
    /**
     * Change to the Composite Surface mode pixel shader program.
     * @param kRenderer the Renderer displaying the scene-graph, to which the
     * new shader program is passed.
     */
    public void SURMode(WildMagic.LibGraphics.Rendering.Renderer kRenderer )
    {

        if ( m_iWhichShader == CMP_SUR )
        {
            return;
        }

        m_iWhichShader = CMP_SUR;
        SetProgram(m_kPShaderSUR, kRenderer);
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("Composite") != null )
        {
            pkProgram.GetUC("Composite").SetDataSource(new float[] {1,0,0,0});
        }
        if ( pkProgram.GetUC("CompositeA") != null )
        {
            pkProgram.GetUC("CompositeA").SetDataSource(new float[] {1,0,0,0});
        }
        if ( pkProgram.GetUC("CompositeB") != null )
        {
            pkProgram.GetUC("CompositeB").SetDataSource(new float[] {1,0,0,0});
        }
    }
    /**
     * The VolumeShaderEffect.CreateVolumeTexture() function constructs and
     * initializes the vertex and pixel shader programs for volume
     * rendering. The vertex shader is the same for each rendering type: MIP,
     * DDR, Composite, Surface, and Composite Surface. The pixel shaders are
     * different for each.
     */
    private void CreateVolumeTexture ()
    {
        /* Set single-pass rendering: */
        SetPassQuantity(1);

        /* Create the vertex shader program, shared by all rendering types. It
         * is implemented in the VolumeShaderVertex.cg file: */        
        VertexShader pkVShader = new VertexShader("VolumeShaderVertex");

        /* The following creates the pixel shaders, implemented in
         * VolumeShader***.cg. It also sets up and stores references to the
         * texture images used by the pixel shader. The pixel-shader uses 11
         * textures. The SceneImage texture, storing the texture-coordinates
         * of the back-facing polygons. VolumeImage* texture stores the
         * ModelImage data. ColorMap* stores the color
         * lookup-table. OpacityMap* stores the opacity transfer
         * function. OpacityMap*_GM has the gradient magnitude opacity
         * transfer function.  NormalMap* stores the normal vector and
         * gradient magnitude. */
        m_kPShaderMIP = new PixelShader("VolumeShaderMIP");
        initTextures(m_kPShaderMIP);

        m_kPShaderDDR = new PixelShader("VolumeShaderDDR");
        initTextures(m_kPShaderDDR);

        m_kPShaderCMP = new PixelShader("VolumeShaderCMP");
        initTextures(m_kPShaderCMP);

        m_kPShaderSUR = new PixelShader("VolumeShaderSUR");
        initTextures(m_kPShaderSUR);
         
        /* The vertex shader is set, it never changes. The first parameter is
         * the pass number used when applying different shaders during
         * multiple rendering passes, it is 0 because this effect is
         * implemented in single-pass rendering. */
        SetVShader(0,pkVShader);
        /* The pixel shader defaults to MIP: */
        SetPShader(0,m_kPShaderCMP);
    }
    /**
     * Sets the IsColor shader parameter values.
     */
    private void SetColorImage()
    {
        Program pkProgram = GetPProgram(0);
        if ( m_kVolumeImageA.IsColorImage() )
        {
            if ( pkProgram.GetUC("IsColor") != null ) 
            {
                pkProgram.GetUC("IsColor").SetDataSource(new float[]{1,0,0,0});
            }
            if ( pkProgram.GetUC("IsColorA") != null ) 
            {
                pkProgram.GetUC("IsColorA").SetDataSource(new float[]{1,0,0,0});
            }
        }
        if ( (m_kVolumeImageB != null) && m_kVolumeImageB.IsColorImage() )
        {
            if ( pkProgram.GetUC("IsColorB") != null ) 
            {
                pkProgram.GetUC("IsColorB").SetDataSource(new float[]{1,0,0,0});
            }                
            else
            {
                pkProgram.GetUC("IsColorB").SetDataSource(new float[]{0,0,0,0});
            }
        }
    }
    /**
     * Sets the GradientMagnitude shader parameter.
     */
    private void SetGradientMagnitude()
    {
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("GradientMagnitude") != null ) 
        {
            pkProgram.GetUC("GradientMagnitude").SetDataSource(m_afGradientMagnitude);
        }
    }
    /**
     * Change to the input pixel shader program.
     * @param kShader the new pixel shader program to use.
     * @param kRenderer the Renderer displaying the scene-graph, to which the
     * new shader program is passed.
     */
    private void SetProgram(PixelShader kShader,
                            WildMagic.LibGraphics.Rendering.Renderer kRenderer )
    {
        SetPShader(0,kShader);
        LoadPrograms(kRenderer, 0,kRenderer.GetMaxColors(),
                     kRenderer.GetMaxTCoords(),
                     kRenderer.GetMaxVShaderImages(),
                     kRenderer.GetMaxPShaderImages());
        
        ResetClip();
        SetBackgroundColor( m_kBackgroundColor );
        Blend(m_afBlend[0]);
        setABBlend(m_afABBlend[0]);
        SetColorImage();
        SetGradientMagnitude();
        SetSelfShadow();
        setSteps(stepsSize[0]);
        if ( m_kVolumeImageB != null )
        {
            Program pkProgram = GetPProgram(0);
            if ( pkProgram.GetUC("ShowB") != null ) 
            {    
                pkProgram.GetUC("ShowB").SetDataSource(new float[]{1,0,0,0});
            }   
        }
    }

    /**
     * Sets the SelfShadow shader parameter.
     */
    private void SetSelfShadow()
    {
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("SelfShadow") != null ) 
        {
            pkProgram.GetUC("SelfShadow").SetDataSource(m_afSelfShadow);
        }
    }

    private void initTextures( PixelShader kPShader )
    {
        int iTex = 0;
        kPShader.SetTextureQuantity(11);
        kPShader.SetImageName(iTex,"SceneImage");
        kPShader.SetTexture(iTex++,m_kSceneTarget);
        kPShader.SetImageName(iTex,"VolumeImageA");
        kPShader.SetTexture(iTex++, m_kVolumeImageA.GetVolumeTarget() );
        kPShader.SetImageName(iTex, "ColorMapA");
        kPShader.SetTexture(iTex++, m_kVolumeImageA.GetColorMapTarget() );
        kPShader.SetImageName(iTex, "OpacityMapA");
        kPShader.SetTexture(iTex++, m_kVolumeImageA.GetOpacityMapTarget() );
        kPShader.SetImageName(iTex, "OpacityMapA_GM");
        kPShader.SetTexture(iTex++, m_kVolumeImageA.GetOpacityMapGMTarget() );
        kPShader.SetImageName(iTex, "NormalMapA" );
        kPShader.SetTexture(iTex++, m_kVolumeImageA.GetNormalMapTarget());
        if ( m_kVolumeImageB != null )
        {
            kPShader.SetImageName(iTex,"VolumeImageB");
            kPShader.SetTexture(iTex++,m_kVolumeImageB.GetVolumeTarget());
            kPShader.SetImageName(iTex, "ColorMapB");
            kPShader.SetTexture(iTex++,m_kVolumeImageB.GetColorMapTarget());
            kPShader.SetImageName(iTex, "OpacityMapB");
            kPShader.SetTexture(iTex++,m_kVolumeImageB.GetOpacityMapTarget());
            kPShader.SetImageName(iTex, "OpacityMapB_GM");
            kPShader.SetTexture(iTex++, m_kVolumeImageB.GetOpacityMapGMTarget());
            kPShader.SetImageName(iTex, "NormalMapB");
            kPShader.SetTexture(iTex++, m_kVolumeImageB.GetNormalMapTarget());
        }
    }

}
