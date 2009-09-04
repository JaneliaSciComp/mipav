package gov.nih.mipav.view.renderer.WildMagic.Render;


import gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.ClassificationWidgetState;
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
    /** View Mode Multi-histogram Constant: */
    private final static int MULTIHISTO = 5;


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

    /** PixelShader program and data for Multihistogram mode: */
    private PixelShader m_kPShaderMULTIHISTO = null;

    /** Indicates which shader to use (MIP, DDR, CMP, SUR, CMP_SUR, MULTIHISTO): */
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


    private ClassificationWidgetState[] m_akLevWidget = new ClassificationWidgetState[10];


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
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram.GetUC("blend") != null ) 
        {
            pkCProgram.GetUC("blend").SetDataSource(m_afBlend);
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
     * Change to the Multi-histogram mode pixel shader program.
     * @param kRenderer the Renderer displaying the scene-graph, to which the
     * new shader program is passed.
     */
    public void MULTIHISTOMode(WildMagic.LibGraphics.Rendering.Renderer kRenderer )
    {
        if ( m_iWhichShader == MULTIHISTO )
        {
            return;
        }
        m_iWhichShader = MULTIHISTO;
        SetProgram(m_kPShaderMULTIHISTO, kRenderer);
    }

    /**
     * Reload the current shader programs from disk, compile and parse and
     * send to the GPU.
     * @param kRenderer the Renderer object displaying the scene-graph which
     * will apply the shader programs.
     */
    public void Reload( WildMagic.LibGraphics.Rendering.Renderer kRenderer )
    {
        ReleaseResources(kRenderer, null);

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
        else if ( m_iWhichShader == MULTIHISTO )
        {
            m_iWhichShader = -1;
            MULTIHISTOMode(kRenderer);
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
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram.GetUC("ABBlend") != null ) 
        {
            pkCProgram.GetUC("ABBlend").SetDataSource(m_afABBlend);
        }
    }

    /**
     * Sets the BackgroundColor shader parameter.
     * @param kColor new BackgroundColor.
     */
    public void SetBackgroundColor( ColorRGBA kColor )
    {
        m_kBackgroundColor = kColor;
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram.GetUC("BackgroundColor") != null ) 
        {
            float[] afColor = new float[4];
            afColor[0] = kColor.R;
            afColor[1] = kColor.G;
            afColor[2] = kColor.B;
            afColor[3] = kColor.A;
            pkCProgram.GetUC("BackgroundColor").SetDataSource(afColor);
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
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram.GetUC(kLightType) != null)
        {
            pkCProgram.GetUC(kLightType).SetDataSource(afType);
        }
    }
    /**
     * Sets the steps size factor shader parameter.
     * @param stepsSize blend factor (range = 0-1).
     */
    public void setSteps(float fsteps)
    {
        stepsSize[0] = fsteps;
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram.GetUC("steps") != null ) 
        {
            pkCProgram.GetUC("steps").SetDataSource(stepsSize);
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
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram.GetUC("Composite") != null )
        {
            pkCProgram.GetUC("Composite").SetDataSource(new float[] {0,0,0,0});
        }
        if ( pkCProgram.GetUC("CompositeA") != null )
        {
            pkCProgram.GetUC("CompositeA").SetDataSource(new float[] {0,0,0,0});
        }
        if ( pkCProgram.GetUC("CompositeB") != null )
        {
            pkCProgram.GetUC("CompositeB").SetDataSource(new float[] {0,0,0,0});
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
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram.GetUC("Composite") != null )
        {
            pkCProgram.GetUC("Composite").SetDataSource(new float[] {1,0,0,0});
        }
        if ( pkCProgram.GetUC("CompositeA") != null )
        {
            pkCProgram.GetUC("CompositeA").SetDataSource(new float[] {1,0,0,0});
        }
        if ( pkCProgram.GetUC("CompositeB") != null )
        {
            pkCProgram.GetUC("CompositeB").SetDataSource(new float[] {1,0,0,0});
        }
    }
    public void updateLevWidgetState( ClassificationWidgetState kLWS, int iState )
    {
        if ( m_akLevWidget[iState] == null )
        {
            m_akLevWidget[iState] = new ClassificationWidgetState();
        }
        
        m_akLevWidget[iState].Copy( kLWS );
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram.GetUC("LevColor"+iState) != null ) 
        {
            pkCProgram.GetUC("LevColor"+iState).SetDataSource(m_akLevWidget[iState].Color);
            //System.err.println( "LevColor" + fR + " " + fG + " " + fB + " " + fA );
        }
        if ( pkCProgram.GetUC("LevMidLine"+iState) != null ) 
        {
            pkCProgram.GetUC("LevMidLine"+iState).SetDataSource(m_akLevWidget[iState].MidLine);
            //System.err.println( "LevMidLine" + fX1 + " " + fY1 + " " + fX2 + " " + fY2 );
        }
        if ( pkCProgram.GetUC("LevLeftLine"+iState) != null ) 
        {
            pkCProgram.GetUC("LevLeftLine"+iState).SetDataSource(m_akLevWidget[iState].LeftLine);
            //System.err.println( "LevLeftLine" + fX1 + " " + fY1 + " " + fX2 + " " + fY2 );
        }
        if ( pkCProgram.GetUC("LevRightLine"+iState) != null ) 
        {
            pkCProgram.GetUC("LevRightLine"+iState).SetDataSource(m_akLevWidget[iState].RightLine);
            //System.err.println( "LevRightLine" + fX1 + " " + fY1 + " " + fX2 + " " + fY2 );
        }
        if ( pkCProgram.GetUC("BoundaryEmphasis"+iState) != null ) 
        {
            pkCProgram.GetUC("BoundaryEmphasis"+iState).SetDataSource(m_akLevWidget[iState].BoundaryEmphasis);
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
         
        m_kPShaderMULTIHISTO = new PixelShader("VolumeShaderMULTIHISTO");
        initTextures(m_kPShaderMULTIHISTO);
         
        /* The vertex shader is set, it never changes. The first parameter is
         * the pass number used when applying different shaders during
         * multiple rendering passes, it is 0 because this effect is
         * implemented in single-pass rendering. */
        SetVShader(0,pkVShader);
        /* The pixel shader defaults to CMP: */
        SetPShader(0,m_kPShaderCMP);
    }
    private void initTextures( PixelShader kPShader )
    {        
        int iTex = 0;
        kPShader.SetTextureQuantity(13);
        kPShader.SetImageName(iTex, m_kSceneTarget.GetName());
        kPShader.SetTexture(iTex++, m_kSceneTarget);

        kPShader.SetImageName(iTex, m_kVolumeImageA.GetVolumeTarget().GetName() );
        kPShader.SetTexture(iTex++, m_kVolumeImageA.GetVolumeTarget() );
        kPShader.SetImageName(iTex, m_kVolumeImageA.GetColorMapTarget().GetName() );
        kPShader.SetTexture(iTex++, m_kVolumeImageA.GetColorMapTarget() );
        kPShader.SetImageName(iTex, m_kVolumeImageA.GetOpacityMapTarget().GetName() );
        kPShader.SetTexture(iTex++, m_kVolumeImageA.GetOpacityMapTarget() );
        kPShader.SetImageName(iTex, m_kVolumeImageA.GetNormalMapTarget().GetName());
        kPShader.SetTexture(iTex++, m_kVolumeImageA.GetNormalMapTarget());
        kPShader.SetImageName(iTex, m_kVolumeImageA.GetGradientMapTarget().GetName());
        kPShader.SetTexture(iTex++, m_kVolumeImageA.GetGradientMapTarget());
        kPShader.SetImageName(iTex, m_kVolumeImageA.GetOpacityMapGMTarget().GetName() );
        kPShader.SetTexture(iTex++, m_kVolumeImageA.GetOpacityMapGMTarget() );
        kPShader.SetImageName(iTex, m_kVolumeImageA.GetSecondDerivativeMapTarget().GetName());
        kPShader.SetTexture(iTex++, m_kVolumeImageA.GetSecondDerivativeMapTarget());
        if ( m_kVolumeImageB != null )
        {
            kPShader.SetImageName(iTex, m_kVolumeImageB.GetVolumeTarget().GetName());
            kPShader.SetTexture(iTex++, m_kVolumeImageB.GetVolumeTarget());
            kPShader.SetImageName(iTex, m_kVolumeImageB.GetColorMapTarget().GetName());
            kPShader.SetTexture(iTex++, m_kVolumeImageB.GetColorMapTarget());
            kPShader.SetImageName(iTex, m_kVolumeImageB.GetOpacityMapTarget().GetName());
            kPShader.SetTexture(iTex++, m_kVolumeImageB.GetOpacityMapTarget());
            kPShader.SetImageName(iTex, m_kVolumeImageB.GetOpacityMapGMTarget().GetName());
            kPShader.SetTexture(iTex++, m_kVolumeImageB.GetOpacityMapGMTarget());
            kPShader.SetImageName(iTex, m_kVolumeImageB.GetNormalMapTarget().GetName());
            kPShader.SetTexture(iTex++, m_kVolumeImageB.GetNormalMapTarget());
            kPShader.SetImageName(iTex, m_kVolumeImageB.GetGradientMapTarget().GetName());
            kPShader.SetTexture(iTex++, m_kVolumeImageB.GetGradientMapTarget());
        }
    }
    /**
     * Sets the IsColor shader parameter values.
     */
    private void SetColorImage()
    {
        Program pkCProgram = GetCProgram(0);
        if ( m_kVolumeImageA.IsColorImage() )
        {
            if ( pkCProgram.GetUC("IsColor") != null ) 
            {
                pkCProgram.GetUC("IsColor").SetDataSource(new float[]{1,0,0,0});
            }
            if ( pkCProgram.GetUC("IsColorA") != null ) 
            {
                pkCProgram.GetUC("IsColorA").SetDataSource(new float[]{1,0,0,0});
            }
        }
        if ( (m_kVolumeImageB != null) && m_kVolumeImageB.IsColorImage() )
        {
            if ( pkCProgram.GetUC("IsColorB") != null ) 
            {
                pkCProgram.GetUC("IsColorB").SetDataSource(new float[]{1,0,0,0});
            }                
            else
            {
                pkCProgram.GetUC("IsColorB").SetDataSource(new float[]{0,0,0,0});
            }
        }
    }

    /**
     * Sets the GradientMagnitude shader parameter.
     */
    private void SetGradientMagnitude()
    {
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram.GetUC("GradientMagnitude") != null ) 
        {
            pkCProgram.GetUC("GradientMagnitude").SetDataSource(m_afGradientMagnitude);
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
            Program pkCProgram = GetCProgram(0);
            if ( pkCProgram.GetUC("ShowB") != null ) 
            {    
                pkCProgram.GetUC("ShowB").SetDataSource(new float[]{1,0,0,0});
            }   
        }
    }

    /**
     * Sets the SelfShadow shader parameter.
     */
    private void SetSelfShadow()
    {
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram.GetUC("SelfShadow") != null ) 
        {
            pkCProgram.GetUC("SelfShadow").SetDataSource(m_afSelfShadow);
        }
    }


}
