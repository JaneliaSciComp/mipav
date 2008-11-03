package gov.nih.mipav.view.renderer.WildMagic.Render;


import WildMagic.LibFoundation.Mathematics.*;
import WildMagic.LibGraphics.Rendering.*;
import WildMagic.LibGraphics.Shaders.*;
import WildMagic.LibGraphics.ObjectSystem.*;

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
    /** View Mode Constants: */
    private final static int MIP = 0;
    private final static int DDR = 1;
    private final static int CMP = 2;
    private final static int SUR = 3;
    private final static int CMP_SUR = 4;


    /** 
     * Creates a new VolumeShaderEffect object.
     * @param kImageA ModelImage A
     * @param kLUTa LUT for ModelImage A
     * @param kRGBTa RGB lookup table for ModelImage A
     * @param kImageB ModelImage B
     * @param kLUTb LUT for ModelImage B
     * @param kRGBTb RGB lookup table for ModelImage B
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

        /* The following code block creates the MIP pixel shader, implemented
         * in VolumeShaderMIP.cg. It also sets up and stores references to the
         * texture images used by the pixel shader. The MIP pixel-shader uses
         * 6 textures. The SceneImage texture, storing the texture-coordinates
         * of the back-facing polygons. VolumeImageA texture stores the
         * ModelImage data. ColorMapA stores the color
         * lookup-table. OpacityMapA stores the opacity transfer
         * function. VolumeImageA_GM has the gradient magnitude data for the
         * ModelImage. OpacityMapA_GM has the gradient magnitude opacity
         * transfer function.  The references m_kVolumeTargetA,
         * m_kColorMapTargetA, m_kOpacityMapTargetA, m_kVolumeTargetA_GM, and
         * m_kOpacityTargetA_GM are stored so the images can be re-used by the
         * other pixel shaders. */
        m_kPShaderMIP = new PixelShader("VolumeShaderMIP");
        m_kPShaderMIP.SetTextureQuantity(11);
        m_kPShaderMIP.SetImageName(0,"SceneImage");
        m_kPShaderMIP.SetTexture(0,m_kSceneTarget);
        m_kPShaderMIP.SetImageName(1,"VolumeImageA");
        m_kPShaderMIP.SetTexture(1, m_kVolumeImageA.GetVolumeTarget() );
        m_kPShaderMIP.SetImageName(2, "ColorMapA");
        m_kPShaderMIP.SetTexture(2, m_kVolumeImageA.GetColorMapTarget() );
        m_kPShaderMIP.SetImageName(3, "OpacityMapA");
        m_kPShaderMIP.SetTexture(3, m_kVolumeImageA.GetOpacityMapTarget() );
        m_kPShaderMIP.SetImageName(4,"VolumeImageA_GM");
        m_kPShaderMIP.SetTexture(4, m_kVolumeImageA.GetVolumeGMTarget());
        m_kPShaderMIP.SetImageName(5, "OpacityMapA_GM");
        m_kPShaderMIP.SetTexture(5, m_kVolumeImageA.GetOpacityMapGMTarget() );
        if ( m_kVolumeImageB != null )
        {
            m_kPShaderMIP.SetImageName(6,"VolumeImageB");
            m_kPShaderMIP.SetTexture(6,m_kVolumeImageB.GetVolumeTarget());
            m_kPShaderMIP.SetImageName(7, "ColorMapB");
            m_kPShaderMIP.SetTexture(7,m_kVolumeImageB.GetColorMapTarget());
            m_kPShaderMIP.SetImageName(8, "OpacityMapB");
            m_kPShaderMIP.SetTexture(8,m_kVolumeImageB.GetOpacityMapTarget());
            m_kPShaderMIP.SetImageName(9,"VolumeImageB_GM");
            m_kPShaderMIP.SetTexture(9,m_kVolumeImageB.GetVolumeGMTarget());
            m_kPShaderMIP.SetImageName(10, "OpacityMapB_GM");
            m_kPShaderMIP.SetTexture(10, m_kVolumeImageB.GetOpacityMapGMTarget());
        }

        /* The following block creates the DDR volume shader, implemented in
         * VolumShaderDDR.cg. The DDR shader uses the same texture images and
         * data as the MIP shader. The textures are not duplicated in CPU or
         * GPU memory. */
        m_kPShaderDDR = new PixelShader("VolumeShaderDDR");
        m_kPShaderDDR.SetTextureQuantity(11);
        m_kPShaderDDR.SetImageName(0,"SceneImage");
        m_kPShaderDDR.SetTexture(0,m_kSceneTarget);
        m_kPShaderDDR.SetImageName(1,"VolumeImageA");
        m_kPShaderDDR.SetTexture(1,m_kVolumeImageA.GetVolumeTarget());
        m_kPShaderDDR.SetImageName(2, "ColorMapA");
        m_kPShaderDDR.SetTexture(2,m_kVolumeImageA.GetColorMapTarget());
        m_kPShaderDDR.SetImageName(3, "OpacityMapA");
        m_kPShaderDDR.SetTexture(3,m_kVolumeImageA.GetOpacityMapTarget());
        m_kPShaderDDR.SetImageName(4,"VolumeImageA_GM");
        m_kPShaderDDR.SetTexture(4,m_kVolumeImageA.GetVolumeGMTarget());
        m_kPShaderDDR.SetImageName(5, "OpacityMapA_GM");
        m_kPShaderDDR.SetTexture(5, m_kVolumeImageA.GetOpacityMapGMTarget());
        if ( m_kVolumeImageB != null )
        {
            m_kPShaderDDR.SetImageName(6,"VolumeImageB");
            m_kPShaderDDR.SetTexture(6,m_kVolumeImageB.GetVolumeTarget());
            m_kPShaderDDR.SetImageName(7, "ColorMapB");
            m_kPShaderDDR.SetTexture(7,m_kVolumeImageB.GetColorMapTarget());
            m_kPShaderDDR.SetImageName(8, "OpacityMapB");
            m_kPShaderDDR.SetTexture(8,m_kVolumeImageB.GetOpacityMapTarget());
            m_kPShaderDDR.SetImageName(9,"VolumeImageB_GM");
            m_kPShaderDDR.SetTexture(9,m_kVolumeImageB.GetVolumeGMTarget());
            m_kPShaderDDR.SetImageName(10, "OpacityMapB_GM");
            m_kPShaderDDR.SetTexture(10, m_kVolumeImageB.GetOpacityMapGMTarget());
        }

        /* The following block creates the Composite volume shader,
         * implemented in VolumShaderCMP.cg. The Composite shader uses the
         * same texture images and data as the MIP shader. The textures are
         * not duplicated in CPU or GPU memory. */
        m_kPShaderCMP = new PixelShader("VolumeShaderCMP");
        m_kPShaderCMP.SetTextureQuantity(11);
        m_kPShaderCMP.SetImageName(0,"SceneImage");
        m_kPShaderCMP.SetTexture(0,m_kSceneTarget);
        m_kPShaderCMP.SetImageName(1,"VolumeImageA");
        m_kPShaderCMP.SetTexture(1,m_kVolumeImageA.GetVolumeTarget());
        m_kPShaderCMP.SetImageName(2, "ColorMapA");
        m_kPShaderCMP.SetTexture(2,m_kVolumeImageA.GetColorMapTarget());
        m_kPShaderCMP.SetImageName(3, "OpacityMapA");
        m_kPShaderCMP.SetTexture(3,m_kVolumeImageA.GetOpacityMapTarget());
        m_kPShaderCMP.SetImageName(4,"VolumeImageA_GM");
        m_kPShaderCMP.SetTexture(4,m_kVolumeImageA.GetVolumeGMTarget());
        m_kPShaderCMP.SetImageName(5, "OpacityMapA_GM");
        m_kPShaderCMP.SetTexture(5, m_kVolumeImageA.GetOpacityMapGMTarget());
        if ( m_kVolumeImageB != null )
        {
            m_kPShaderCMP.SetImageName(6,"VolumeImageB");
            m_kPShaderCMP.SetTexture(6,m_kVolumeImageB.GetVolumeTarget());
            m_kPShaderCMP.SetImageName(7, "ColorMapB");
            m_kPShaderCMP.SetTexture(7,m_kVolumeImageB.GetColorMapTarget());
            m_kPShaderCMP.SetImageName(8, "OpacityMapB");
            m_kPShaderCMP.SetTexture(8,m_kVolumeImageB.GetOpacityMapTarget());
            m_kPShaderCMP.SetImageName(9,"VolumeImageB_GM");
            m_kPShaderCMP.SetTexture(9,m_kVolumeImageB.GetVolumeGMTarget());
            m_kPShaderCMP.SetImageName(10, "OpacityMapB_GM");
            m_kPShaderCMP.SetTexture(10, m_kVolumeImageB.GetOpacityMapGMTarget());
        }

        /* The following block creates the Surface and Composite Surface
         * volume shader, both are implemented in a single pixel program in
         * VolumShaderSUR.cg. The shader uses the same texture images and data
         * as the MIP shader, with the addition of another 3D volume texture
         * storing the normal data. */
        m_kPShaderSUR = new PixelShader("VolumeShaderSUR");
        m_kPShaderSUR.SetTextureQuantity(13);
        m_kPShaderSUR.SetImageName(0,"SceneImage");
        m_kPShaderSUR.SetTexture(0,m_kSceneTarget);
        m_kPShaderSUR.SetImageName(1,"VolumeImageA");
        m_kPShaderSUR.SetTexture(1,m_kVolumeImageA.GetVolumeTarget());
        m_kPShaderSUR.SetImageName(2, "ColorMapA");
        m_kPShaderSUR.SetTexture(2,m_kVolumeImageA.GetColorMapTarget());
        m_kPShaderSUR.SetImageName(3, "OpacityMapA");
        m_kPShaderSUR.SetTexture(3,m_kVolumeImageA.GetOpacityMapTarget());
        m_kPShaderSUR.SetImageName(4,"VolumeImageA_GM");
        m_kPShaderSUR.SetTexture(4,m_kVolumeImageA.GetVolumeGMTarget());
        m_kPShaderSUR.SetImageName(5, "OpacityMapA_GM");
        m_kPShaderSUR.SetTexture(5, m_kVolumeImageA.GetOpacityMapGMTarget());
        m_kPShaderSUR.SetImageName(6, "NormalMapA" );
        m_kPShaderSUR.SetTexture(6, m_kVolumeImageA.GetNormalMapTarget());
        if ( m_kVolumeImageB != null )
        {
            m_kPShaderSUR.SetImageName(7,"VolumeImageB");
            m_kPShaderSUR.SetTexture(7,m_kVolumeImageB.GetVolumeTarget());
            m_kPShaderSUR.SetImageName(8, "ColorMapB");
            m_kPShaderSUR.SetTexture(8,m_kVolumeImageB.GetColorMapTarget());
            m_kPShaderSUR.SetImageName(9, "OpacityMapB");
            m_kPShaderSUR.SetTexture(9,m_kVolumeImageB.GetOpacityMapTarget());
            m_kPShaderSUR.SetImageName(10,"VolumeImageB_GM");
            m_kPShaderSUR.SetTexture(10,m_kVolumeImageB.GetVolumeGMTarget());
            m_kPShaderSUR.SetImageName(11, "OpacityMapB_GM");
            m_kPShaderSUR.SetTexture(11, m_kVolumeImageB.GetOpacityMapGMTarget());
            m_kPShaderSUR.SetImageName(12, "NormalMapB");
            m_kPShaderSUR.SetTexture(12, m_kVolumeImageB.GetNormalMapTarget());
        }
         
        /* The vertex shader is set, it never changes. The first parameter is
         * the pass number used when applying different shaders during
         * multiple rendering passes, it is 0 because this effect is
         * implemented in single-pass rendering. */
        SetVShader(0,pkVShader);
        /* The pixel shader defaults to MIP: */
        SetPShader(0,m_kPShaderCMP);
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
        SetColorImage();
        SetGradientMagnitude();
        SetSelfShadow();
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
     * Returns the current pixel program.
     * @return the current pixel program.
     */
    public Program GetPProgram()
    {
        return GetPProgram(0);
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
     * Sets the blend factor shader parameter between imageA and imageB.
     * @param fBlend blend factor (range = 0-1).
     */
    public void setABBlend(float fBlend)
    {
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("ABBlend") != null ) 
        {
            pkProgram.GetUC("ABBlend").SetDataSource(new float[]{fBlend,0,0,0});
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
     * Loads this object from the input parameter rkStream, using the input
     * Stream.Link to store the IDs of children objects of this object
     * for linking after all objects are loaded from the Stream.
     * @param rkStream the Stream from which this object is being read.
     * @param pkLink the Link class for storing the IDs of this object's
     * children objects.
     */
    public void Load (Stream rkStream, Stream.Link pkLink)
    {
        super.Load(rkStream,pkLink);

        // native data
    }

    /**
     * Copies this objects children objects from the input Stream's HashTable,
     * based on the LinkID of the child stored in the pkLink parameter.
     * @param rkStream the Stream where the child objects are stored.
     * @param pkLink the Link class from which the child object IDs are read.
     */
    public void Link (Stream rkStream, Stream.Link pkLink)
    {
        super.Link(rkStream,pkLink);
    }

    /**
     * Registers this object with the input Stream parameter. All objects
     * streamed to disk are registered with the Stream so that a unique list
     * of objects is maintained.
     * @param rkStream the Stream where the child objects are stored.
     * @return true if this object is registered, false if the object has
     * already been registered.
     */
    public boolean Register (Stream rkStream)
    {
        if (!super.Register(rkStream))
        {
            return false;
        }
        return true;
    }

    /**
     * Write this object and all it's children to the Stream.
     * @param rkStream the Stream where the child objects are stored.
     */
    public void Save (Stream rkStream)
    {
        super.Save(rkStream);

        // native data
        //rkStream.Write(m_iPassQuantity);

        // link data
//         for (int iPass = 0; iPass < m_iPassQuantity; iPass++)
//         {
//             rkStream.Write(m_kVShader.get(iPass).GetID());
//             rkStream.Write(m_kPShader.get(iPass).GetID());
//         }
//         int iQuantity = m_kAlphaState.size();
//         rkStream.Write(iQuantity);
//         for (int i = 0; i < iQuantity; i++)
//         {
//             rkStream.Write(m_kAlphaState.get(i).GetID());
//         }

    }

    /**
     * Returns the size of this object and it's children on disk for the
     * current StreamVersion parameter.
     * @param rkVersion the current version of the Stream file being created.
     * @return the size of this object on disk.
     */
    public int GetDiskUsed (StreamVersion rkVersion)
    {
        int iSize = super.GetDiskUsed(rkVersion);

        return iSize;
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
        // strings
        pkTree.Append(StringTree.Format("VolumeShaderEffect",GetName()));
        pkTree.Append(super.SaveStrings(null));

        return pkTree;
    }

    private VolumeImage m_kVolumeImageA;
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

    private float[] m_afBlend = new float[]{1,0,0,0};
}
