package gov.nih.mipav.view.renderer.WildMagic.Render;


import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.ClassificationWidgetState;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibGraphics.ObjectSystem.StreamInterface;
import WildMagic.LibGraphics.ObjectSystem.StringTree;
import WildMagic.LibGraphics.Rendering.AlphaState;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.Shaders.PixelShader;
import WildMagic.LibGraphics.Shaders.Program;
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
public class VolumeShaderEffectMultiPass extends VolumeClipEffect
    implements StreamInterface
{
    /**  */
    private static final long serialVersionUID = 5580825290512790813L;
    /** View Mode MIP Constant: */
    private final static int MIP = 0;
    /** View Mode DRR Constant: */
    private final static int DRR = 1;
    /** View Mode Composite Constant: */
    private final static int CMP = 2;
    /** View Mode Surface Constant: */
    private final static int SUR = 3;
    /** View Mode Composite-Surface Constant: */
    private final static int CMP_SUR = 4;
    private final static int CUSTUM = 5;

    private static int ms_MaxLights = 8;
    
    private static int ms_iNumLev = 10;

    private static int ms_iMaxSamples = 1000;
    
    /** Shared volume data and textures. */
    private VolumeImage m_kVolumeImageA;

    /** Shared volume data and textures. */
    private VolumeImage m_kVolumeImageB;
  
    /** PixelShader program and data for Composite mode: */
    private PixelShader m_kPShaderCMP = null;
    /** Indicates which shader to use (MIP, DDR, CMP, SUR, CMP_SUR, MULTIHISTO): */
    private int m_iWhichShader = -1;
    
    /** Reference to the SceneImage texture: */
    private Texture m_kSceneTarget;
    private float[][] m_aafLight = new float[ms_MaxLights][4];
    private ClassificationWidgetState[] m_akLevWidget = new ClassificationWidgetState[ms_iNumLev];

    private boolean[] m_abLevWidgetInit = new boolean[ms_iNumLev];
    private int m_iPasses = 1;

    private VertexShader m_pkVShader;
    private boolean m_bMultiHisto = false;
    private int m_iBlend;
    private int m_iSrcBlend;
    private int m_iDstBlend;
    private ColorRGBA m_kBlendColor = new ColorRGBA();
    
    /** 
     * Creates a new VolumeShaderEffect object.
     * @param kImageA the VolumeImage containing the data and textures for
     * rendering.
     * @param kVolumeImageB second VolumeImage.
     * @param kSceneTarget the SceneImage texture with the back-facing polygon texture coordinates.
     */
    public VolumeShaderEffectMultiPass ( VolumeImage kVolumeImageA, VolumeImage kVolumeImageB, 
                                Texture kSceneTarget )
    {
        m_kVolumeImageA = kVolumeImageA;
        m_kVolumeImageB = kVolumeImageB;
        m_kSceneTarget = kSceneTarget;
        CreateVolumeTexture();
        for ( int i = 0; i < ms_iNumLev; i++ )
        {
            m_abLevWidgetInit[i] = false;
        }
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
     * Change to the Composite mode pixel shader program.
     */
    public void CMPMode()
    {
        if ( m_iWhichShader == CMP )
        {
            return;
        }
        m_iWhichShader = CMP;
        for (int i = 1; i < (int)m_kAlphaState.size(); i++)
        {
            m_kAlphaState.set(i, new AlphaState());
            m_kAlphaState.get(i).BlendEnabled = true;
            m_kAlphaState.get(i).SrcBlend = AlphaState.SrcBlendMode.SBF_SRC_ALPHA;
            m_kAlphaState.get(i).DstBlend = AlphaState.DstBlendMode.DBF_ONE_MINUS_SRC_ALPHA;
            m_kAlphaState.get(i).BlendEquation = AlphaState.BlendEquationMode.BE_ADD;
        }
        Program pkCProgram = GetCProgram(0);  
        if ( pkCProgram.GetUC("MIP") != null )
        {
            pkCProgram.GetUC("MIP").GetData()[0] = 0;
        }
        if ( pkCProgram.GetUC("DRRA") != null )
        {
            pkCProgram.GetUC("DRRA").GetData()[0] = 0;
        }
        if ( (m_kVolumeImageB.GetImage() != null) && (pkCProgram.GetUC("DRRB") != null) )
        {
            pkCProgram.GetUC("DRRB").GetData()[0] = 0;
        }
        if ( pkCProgram.GetUC("Composite") != null )
        {
            pkCProgram.GetUC("Composite").GetData()[0] = 0;
        }
        if ( pkCProgram.GetUC("Surface") != null )
        {
            pkCProgram.GetUC("Surface").GetData()[0] = 0;
        }
        if ( pkCProgram.GetUC("MULTIHISTO") != null )
        {
            pkCProgram.GetUC("MULTIHISTO").GetData()[0] = 0;
        }
    }

    /**
     * memory cleanup.
     */
    public void dispose()
    {
        m_kVolumeImageA = null;
        m_kVolumeImageB = null;
        //m_kPShaderInit.dispose();
        //m_kPShaderInit = null;

        m_kSceneTarget = null;;
  
        m_aafLight = null;
        for ( int i = 0; i < ms_iNumLev; i++ )
        {
            m_akLevWidget[i] = null;
        }
        m_akLevWidget = null;
        m_abLevWidgetInit = null;

        m_pkVShader.dispose();
        m_pkVShader = null;

        if ( m_kPShaderCMP != null )
        {
            m_kPShaderCMP.dispose();
            m_kPShaderCMP = null;
        }

        super.dispose();
    }

    
    /**
     * Change to the DDR mode pixel shader program.
     */
    public void DRRMode()
    {
        if ( m_iWhichShader == DRR )
        {
            return;
        }
        m_iWhichShader = DRR;
        for (int i = 1; i < (int)m_kAlphaState.size(); i++)
        {
            m_kAlphaState.set(i, new AlphaState());
            m_kAlphaState.get(i).BlendEnabled = true;
            m_kAlphaState.get(i).SrcBlend = AlphaState.SrcBlendMode.SBF_SRC_ALPHA;
            m_kAlphaState.get(i).DstBlend = AlphaState.DstBlendMode.DBF_ONE;
            m_kAlphaState.get(i).BlendEquation = AlphaState.BlendEquationMode.BE_ADD;
        }
        Program kCProgram = GetCProgram(0);  
        if ( kCProgram.GetUC("MIP") != null )
        {
            kCProgram.GetUC("MIP").GetData()[0] = 0;
        }
        if ( kCProgram.GetUC("DRRA") != null )
        {
            kCProgram.GetUC("DRRA").GetData()[0] = m_kVolumeImageA.getDRRNorm();
        }
        if ( (m_kVolumeImageB.GetImage() != null) && (kCProgram.GetUC("DRRB") != null) )
        {
            kCProgram.GetUC("DRRB").GetData()[0] = m_kVolumeImageB.getDRRNorm();
        }
        if ( kCProgram.GetUC("Composite") != null )
        {
            kCProgram.GetUC("Composite").GetData()[0] = 0;
        }
        if ( kCProgram.GetUC("Surface") != null )
        {
            kCProgram.GetUC("Surface").GetData()[0] = 0;
        }
        if ( kCProgram.GetUC("MULTIHISTO") != null )
        {
            kCProgram.GetUC("MULTIHISTO").GetData()[0] = 0;
        }
    }

    /**
     * Change to the MIP mode pixel shader program.
     */
    public void MIPMode( )
    {
        if ( m_iWhichShader == MIP )
        {
            return;
        }
        m_iWhichShader = MIP;
        
        for (int i = 1; i < m_kAlphaState.size(); i++)
        {
            m_kAlphaState.set(i, new AlphaState());
            m_kAlphaState.get(i).BlendEnabled = true;
            m_kAlphaState.get(i).SrcBlend = AlphaState.SrcBlendMode.SBF_ONE;
            m_kAlphaState.get(i).DstBlend = AlphaState.DstBlendMode.DBF_ONE;
            m_kAlphaState.get(i).BlendEquation = AlphaState.BlendEquationMode.BE_MAX;
        }
        Program kCProgram = GetCProgram(0);    
        if ( kCProgram.GetUC("MIP") != null )
        {
            kCProgram.GetUC("MIP").GetData()[0] = 1;
        }
        if ( kCProgram.GetUC("DRRA") != null )
        {
            kCProgram.GetUC("DRRA").GetData()[0] = 0;
        }
        if ( (m_kVolumeImageB.GetImage() != null) && (kCProgram.GetUC("DRRB") != null) )
        {
            kCProgram.GetUC("DRRB").GetData()[0] = 0;
        }
        if ( kCProgram.GetUC("Composite") != null )
        {
            kCProgram.GetUC("Composite").GetData()[0] = 0;
        }
        if ( kCProgram.GetUC("Surface") != null )
        {
            kCProgram.GetUC("Surface").GetData()[0] = 0;
        }
        if ( kCProgram.GetUC("MULTIHISTO") != null )
        {
            kCProgram.GetUC("MULTIHISTO").GetData()[0] = 0;
        }
    }
    
    /**
     * Change to the Multi-histogram mode pixel shader program.
     */
    public void MULTIHISTOMode(boolean bOn)
    {
        m_bMultiHisto = bOn;
        if ( m_kPShaderCMP != null )
        {
            Program kCProgram = GetCProgram(0);  
            if ( (kCProgram != null) && (kCProgram.GetUC("MULTIHISTO") != null) )
            {
                kCProgram.GetUC("MULTIHISTO").GetData()[0] = bOn? 1 : 0;
            }
        }
    }

    /** This function is called in LoadPrograms once the shader programs are
     * created.  Set up the user variable data sources.
     * @param iPass the ith rendering pass
     */
    public void OnLoadPrograms (int iPass, Program pkVProgram,
                                Program pkPProgram, Program pkCProgram)
    {
    	Blend(1f);
        SetColorImage(pkCProgram);
        setABBlend(1.0f);
        setRGBTA(null);
        setVolumeSamples( (m_kVolumeImageA.GetImage().getExtents()[2]*2.0f)/1000.0f );
        if ( m_kVolumeImageB.GetImage() != null )
        {
            setABBlend(0.5f);
            if ( pkCProgram.GetUC("ShowB") != null ) 
            {    
                pkCProgram.GetUC("ShowB").GetData()[0] = 1;
            }   
            setRGBTB(null);
        }
        for ( int i = 0; i < ms_MaxLights; i++ )
        {
            String kLightType = new String("Light"+(i)+"Type");
            if ( pkCProgram.GetUC(kLightType) != null)
            {
                pkCProgram.GetUC(kLightType).SetDataSource(m_aafLight[i]);
            }
        }
        for ( int i = 1; i < m_iPassQuantity; i++ )
        {
            SetCProgram(i,pkCProgram);
        }
        super.OnLoadPrograms ( iPass,  pkVProgram, pkPProgram, pkCProgram );
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
        LoadResources(kRenderer, null);
        setCurrentShader();
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
        //m_afSelfShadow[0] = 0;
        //if ( bShadow )
        //{
        //    m_afSelfShadow[0] = 1;
        //}
    }

    /**
     * Sets the blend factor shader parameter between imageA and imageB.
     * @param fBlend blend factor (range = 0-1).
     */
    public void setABBlend(float fBlend)
    {
        Program kCProgram = GetCProgram(0);  
        if ( (kCProgram != null) && kCProgram.GetUC("ABBlend") != null ) 
        {
            kCProgram.GetUC("ABBlend").GetData()[0] = fBlend;
        }
    }
    /**
     * Sets the background color.
     * @param kColor new background color.
     */
    public void SetBackgroundColor( ColorRGBA kColor )
    {
        if ( m_kPShaderCMP != null )
        {
            Program kCProgram = GetCProgram(0);  
            if ( (kCProgram != null) && (kCProgram.GetUC("BackgroundColor") != null) ) 
            {
                kCProgram.GetUC("BackgroundColor").GetData()[0] = kColor.R;
                kCProgram.GetUC("BackgroundColor").GetData()[1] = kColor.G;
                kCProgram.GetUC("BackgroundColor").GetData()[2] = kColor.B;
                kCProgram.GetUC("BackgroundColor").GetData()[3] = kColor.A;
            }
        }
    }

    public void SetCustumBlend(int iBlendEquation, int iLogicOp, int iSrcBlend, int iDstBlend, ColorRGBA kColor  )
    {        
        m_iWhichShader = CUSTUM;
        m_iBlend = iBlendEquation;
        m_iSrcBlend = iSrcBlend;
        m_iDstBlend = iDstBlend;
        m_kBlendColor.Copy(kColor);
        for (int i = 1; i < (int)m_kAlphaState.size(); i++)
        {
            m_kAlphaState.set(i, new AlphaState());
            m_kAlphaState.get(i).BlendEnabled = true;
            m_kAlphaState.get(i).BlendEquation = AlphaState.BlendEquationMap.get(iBlendEquation);
            m_kAlphaState.get(i).SrcBlend = AlphaState.SrcBlendModeMap.get(iSrcBlend);
            m_kAlphaState.get(i).DstBlend = AlphaState.DstBlendModeMap.get(iDstBlend);
            m_kAlphaState.get(i).ConstantColor.Copy(kColor);
        }
        Program kCProgram = GetCProgram(0);    
        if ( kCProgram.GetUC("MIP") != null )
        {
            kCProgram.GetUC("MIP").GetData()[0] = 0;
        }
        if ( kCProgram.GetUC("DRRA") != null )
        {
            kCProgram.GetUC("DRRA").GetData()[0] = 0;
        }
        if ( (m_kVolumeImageB.GetImage() != null) && (kCProgram.GetUC("DRRB") != null) )
        {
            kCProgram.GetUC("DRRB").GetData()[0] = 0;
        }
        if ( kCProgram.GetUC("Composite") != null )
        {
            kCProgram.GetUC("Composite").GetData()[0] = 0;
        }
        if ( kCProgram.GetUC("Surface") != null )
        {
            kCProgram.GetUC("Surface").GetData()[0] = 0;
        }
        if ( kCProgram.GetUC("MULTIHISTO") != null )
        {
            kCProgram.GetUC("MULTIHISTO").GetData()[0] = 0;
        }
        //System.err.println( kColor.A );
    }
    

    /** 
     * Enables/Disables gradient magnitude filter.
     * @param bShow gradient magnitude filter on/off.
     */
    public void SetGradientMagnitude(boolean bShow)
    {
        Program kCProgram = GetCProgram(0);  
        if ( kCProgram.GetUC("GradientMagnitude") != null ) 
        {
            kCProgram.GetUC("GradientMagnitude").GetData()[0] = bShow? 1 : 0;
        }
    }    
    
    /**
     * Sets the light type for the given light.
     * @param kLightType the name of the light to set (Light0, Light1, etc.)
     * @param afType the type of light (Ambient = 0, Directional = 1, Point = 2, Spot = 3).
     */
    public void SetLight( String kLightType, float[] afType )
    {
        int iLight = Integer.valueOf( kLightType.substring( 5, 6 ) ).intValue();
        m_aafLight[iLight][0] = afType[0];
    }
    
    
    public void setRGBTA(ModelRGB RGBT) {
        if ( m_kPShaderCMP != null )
        {
            Program kCProgram = GetCProgram(0);  
            if ( (kCProgram != null) && (kCProgram.GetUC("ColorLUTOnA") != null) ) 
            {
                if ( RGBT != null )
                {
                    kCProgram.GetUC("ColorLUTOnA").GetData()[0] = RGBT.getROn() ? 1.0f : 0.0f;
                    kCProgram.GetUC("ColorLUTOnA").GetData()[1] = RGBT.getGOn() ? 1.0f : 0.0f;
                    kCProgram.GetUC("ColorLUTOnA").GetData()[2] = RGBT.getBOn() ? 1.0f : 0.0f;

                }
                else
                {
                    kCProgram.GetUC("ColorLUTOnA").GetData()[0] = 1.0f;
                    kCProgram.GetUC("ColorLUTOnA").GetData()[1] = 1.0f;
                    kCProgram.GetUC("ColorLUTOnA").GetData()[2] = 1.0f;
                }
            }
        }
    }
    public void setRGBTB(ModelRGB RGBT) {
        if ( m_kPShaderCMP != null )
        {
            Program kCProgram = GetCProgram(0);  
            if ( (kCProgram != null) && (kCProgram.GetUC("ColorLUTOnB") != null) ) 
            {
                if ( RGBT != null )
                {
                    kCProgram.GetUC("ColorLUTOnB").GetData()[0] = RGBT.getROn() ? 1.0f : 0.0f;
                    kCProgram.GetUC("ColorLUTOnB").GetData()[1] = RGBT.getGOn() ? 1.0f : 0.0f;
                    kCProgram.GetUC("ColorLUTOnB").GetData()[2] = RGBT.getBOn() ? 1.0f : 0.0f;
                }
                else
                {
                    kCProgram.GetUC("ColorLUTOnB").GetData()[0] = 1.0f;
                    kCProgram.GetUC("ColorLUTOnB").GetData()[1] = 1.0f;
                    kCProgram.GetUC("ColorLUTOnB").GetData()[2] = 1.0f;                
                }
            }
        }
    }

    public void setVolumeSamples( float fSample )
    {
        m_iPasses = Math.max(1, (int)(fSample * ms_iMaxSamples));
        SetPassQuantity(m_iPasses);
        Program pkCProgram = GetCProgram(0);
        for ( int i = 0; i < m_iPasses; i++ )
        {
            SetVShader(i,m_pkVShader);
            /* The pixel shader defaults to CMP: */
            SetPShader(i,m_kPShaderCMP);
            SetCProgram(i,pkCProgram);
        }
        setCurrentShader();
    }
    /**
     * Change to the Surface mode pixel shader program.
     * @param kRenderer the Renderer displaying the scene-graph, to which the
     * new shader program is passed.
     */
    public void SURFASTMode()
    {
        if ( m_iWhichShader == SUR )
        {
            return;
        }
        m_iWhichShader = SUR;
        for (int i = 1; i < (int)m_kAlphaState.size(); i++)
        {
            m_kAlphaState.set(i, new AlphaState());
            m_kAlphaState.get(i).BlendEnabled = true;
            m_kAlphaState.get(i).SrcBlend = AlphaState.SrcBlendMode.SBF_SRC_ALPHA;
            m_kAlphaState.get(i).DstBlend = AlphaState.DstBlendMode.DBF_ONE_MINUS_SRC_ALPHA;
            m_kAlphaState.get(i).BlendEquation = AlphaState.BlendEquationMode.BE_ADD;
        }
        Program kCProgram = GetCProgram(0);    
        if ( kCProgram.GetUC("MIP") != null )
        {
            kCProgram.GetUC("MIP").GetData()[0] = 0;
        }
        if ( kCProgram.GetUC("DRRA") != null )
        {
            kCProgram.GetUC("DRRA").GetData()[0] = 0;
        }
        if ( (m_kVolumeImageB.GetImage() != null) && (kCProgram.GetUC("DRRB") != null) )
        {
            kCProgram.GetUC("DRRB").GetData()[0] = 0;
        }
        if ( kCProgram.GetUC("Composite") != null )
        {
            kCProgram.GetUC("Composite").GetData()[0] = 0;
        }
        if ( kCProgram.GetUC("Surface") != null )
        {
            kCProgram.GetUC("Surface").GetData()[0] = 1;
        }
        if ( kCProgram.GetUC("MULTIHISTO") != null )
        {
            kCProgram.GetUC("MULTIHISTO").GetData()[0] = 0;
        }
    }
    /**
     * Change to the Composite Surface mode pixel shader program.
     * @param kRenderer the Renderer displaying the scene-graph, to which the
     * new shader program is passed.
     */
    public void SURMode()
    {

        if ( m_iWhichShader == CMP_SUR )
        {
            return;
        }

        m_iWhichShader = CMP_SUR;
        for (int i = 1; i < (int)m_kAlphaState.size(); i++)
        {
            m_kAlphaState.set(i, new AlphaState());
            m_kAlphaState.get(i).BlendEnabled = true;
            m_kAlphaState.get(i).SrcBlend = AlphaState.SrcBlendMode.SBF_SRC_ALPHA;
            m_kAlphaState.get(i).DstBlend = AlphaState.DstBlendMode.DBF_ONE_MINUS_SRC_ALPHA;
            m_kAlphaState.get(i).BlendEquation = AlphaState.BlendEquationMode.BE_ADD;
        }
        Program kCProgram = GetCProgram(0);    
        if ( kCProgram.GetUC("MIP") != null )
        {
            kCProgram.GetUC("MIP").GetData()[0] = 0;
        }
        if ( kCProgram.GetUC("DRRA") != null )
        {
            kCProgram.GetUC("DRRA").GetData()[0] = 0;
        }
        if ( (m_kVolumeImageB.GetImage() != null) && (kCProgram.GetUC("DRRB") != null) )
        {
            kCProgram.GetUC("DRRB").GetData()[0] = 0;
        }
        if ( kCProgram.GetUC("Composite") != null )
        {
            kCProgram.GetUC("Composite").GetData()[0] = 1;
        }
        if ( kCProgram.GetUC("Surface") != null )
        {
            kCProgram.GetUC("Surface").GetData()[0] = 1;
        }
        if ( kCProgram.GetUC("MULTIHISTO") != null )
        {
            kCProgram.GetUC("MULTIHISTO").GetData()[0] = 0;
        }
    }
    public void updateLevWidgetState( ClassificationWidgetState kLWS, int iState )
    {
        if ( iState >= ms_iNumLev )
        {
            return;
        }
        if ( m_akLevWidget[iState] == null )
        {
            m_akLevWidget[iState] = new ClassificationWidgetState();
        }
        
        m_akLevWidget[iState].Copy( kLWS );
        if ( !m_abLevWidgetInit[iState] )
        {
            //m_abLevWidgetInit[iState] = true;
            
            float fShiftL = 0;
            float fShiftR = 0;
            if ( m_akLevWidget[iState].MidLine[1] == m_akLevWidget[iState].MidLine[3] )
            {
                float fIncr = (m_akLevWidget[iState].MidLine[1] - m_akLevWidget[iState].LeftLine[1]) /
                (m_akLevWidget[iState].LeftLine[3] - m_akLevWidget[iState].LeftLine[1]);

                fIncr = fIncr * (m_akLevWidget[iState].RightLine[0] - m_akLevWidget[iState].LeftLine[0]);

                float fShiftX = (m_akLevWidget[iState].MidLine[0] - m_akLevWidget[iState].LeftLine[0]) /
                (m_akLevWidget[iState].RightLine[0] - m_akLevWidget[iState].LeftLine[0]);
                fShiftL = (fShiftX)*fIncr;
                fShiftR = (1.0f-fShiftX)*fIncr;
            }
            
            for ( int i = 0; i < m_iPasses; i++ )
            {
                Program pkCProgram = GetCProgram(i);
                if ( pkCProgram.GetUC("UseWidget"+iState) != null ) 
                {
                    pkCProgram.GetUC("UseWidget"+iState).SetDataSource(m_akLevWidget[iState].UseWidget);
                    //System.err.println( "UseWidget"+iState + " " + m_akLevWidget[iState].UseWidget[0]);
                }
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
                if ( pkCProgram.GetUC("Shift"+iState) != null ) 
                {
                    //System.err.println( "Shift" + iState );
                    pkCProgram.GetUC("Shift"+iState).GetData()[0] = fShiftL;
                    pkCProgram.GetUC("Shift"+iState).GetData()[1] = fShiftR;
                }
                if ( pkCProgram.GetUC("InvY0MY1"+iState) != null ) 
                {            
                    //System.err.println( "InvY0MY1" + iState );
                    float fLeftInvY0MY1 = 1.0f / (m_akLevWidget[iState].LeftLine[1] - m_akLevWidget[iState].LeftLine[3]);
                    float fMidInvY0MY1 = 1.0f / (m_akLevWidget[iState].MidLine[1] - m_akLevWidget[iState].MidLine[3]);
                    float fRightInvY0MY1 = 1.0f / (m_akLevWidget[iState].RightLine[1] - m_akLevWidget[iState].RightLine[3]);
                    pkCProgram.GetUC("InvY0MY1"+iState).GetData()[0] = fLeftInvY0MY1;
                    pkCProgram.GetUC("InvY0MY1"+iState).GetData()[1] = fMidInvY0MY1;
                    pkCProgram.GetUC("InvY0MY1"+iState).GetData()[2] = fRightInvY0MY1;
  }
                
            }
        }
    }


    protected void SetDefaultAlphaState ()
    {
        m_kAlphaState.set(0, new AlphaState());
        m_kAlphaState.get(0).BlendEnabled = true;
        for (int i = 1; i < (int)m_kAlphaState.size(); i++)
        {
            m_kAlphaState.set(i, new AlphaState());
            m_kAlphaState.get(i).BlendEnabled = true;
            m_kAlphaState.get(i).SrcBlend = AlphaState.SrcBlendMode.SBF_SRC_ALPHA;
            m_kAlphaState.get(i).DstBlend = AlphaState.DstBlendMode.DBF_ONE_MINUS_SRC_ALPHA;
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
        m_iPasses = 1;
        SetPassQuantity(m_iPasses);

        /* Create the vertex shader program, shared by all rendering types. It
         * is implemented in the VolumeShaderVertex.cg file: */        
        m_pkVShader = new VertexShader("VolumeShaderVertex");

        if ( m_kVolumeImageB.GetImage() == null )
        {
            m_kPShaderCMP = new PixelShader("VolumeShaderMultiPass");
        }
        else
        {
            m_kPShaderCMP = new PixelShader("VolumeShaderABMultiPass");
        }
        initTexturesVol(m_kPShaderCMP);
                 
        for ( int i = 0; i < m_iPasses; i++ )
        {
            SetVShader(i,m_pkVShader);
            SetPShader(i,m_kPShaderCMP);
        }
    }

       
    private void initTexturesVol( PixelShader kPShader )
    {        
        int iTex = 0;
        kPShader.SetTextureQuantity(16);
        kPShader.SetImageName(iTex, m_kSceneTarget.GetName());
        kPShader.SetTexture(iTex++, m_kSceneTarget);

        kPShader.SetImageName(iTex, m_kVolumeImageA.GetVolumeTarget().GetName() );
        kPShader.SetTexture(iTex++, m_kVolumeImageA.GetVolumeTarget() );
        kPShader.SetImageName(iTex, m_kVolumeImageA.GetColorMapTarget().GetName() );
        kPShader.SetTexture(iTex++, m_kVolumeImageA.GetColorMapTarget() );
        //kPShader.SetImageName(iTex, m_kVolumeImageA.GetOpacityMapTarget().GetName() );
        //kPShader.SetTexture(iTex++, m_kVolumeImageA.GetOpacityMapTarget() );
        kPShader.SetImageName(iTex, m_kVolumeImageA.GetNormalMapTarget().GetName());
        kPShader.SetTexture(iTex++, m_kVolumeImageA.GetNormalMapTarget());
        kPShader.SetImageName(iTex, m_kVolumeImageA.GetGradientMapTarget().GetName());
        kPShader.SetTexture(iTex++, m_kVolumeImageA.GetGradientMapTarget());
        kPShader.SetImageName(iTex, m_kVolumeImageA.GetOpacityMapGMTarget().GetName() );
        kPShader.SetTexture(iTex++, m_kVolumeImageA.GetOpacityMapGMTarget() );
        kPShader.SetImageName(iTex, m_kVolumeImageA.GetLaplaceMapTarget().GetName());
        kPShader.SetTexture(iTex++, m_kVolumeImageA.GetLaplaceMapTarget());
        

        //kPShader.SetImageName(iTex, m_kVolumeImageA.GetSurfaceTarget().GetName());
        //kPShader.SetTexture(iTex++, m_kVolumeImageA.GetSurfaceTarget());
        
        
        if ( m_kVolumeImageB.GetImage() != null )
        {
            kPShader.SetImageName(iTex, m_kVolumeImageB.GetVolumeTarget().GetName() );
            kPShader.SetTexture(iTex++, m_kVolumeImageB.GetVolumeTarget() );
            kPShader.SetImageName(iTex, m_kVolumeImageB.GetColorMapTarget().GetName() );
            kPShader.SetTexture(iTex++, m_kVolumeImageB.GetColorMapTarget() );
            //kPShader.SetImageName(iTex, m_kVolumeImageB.GetOpacityMapTarget().GetName() );
            //kPShader.SetTexture(iTex++, m_kVolumeImageB.GetOpacityMapTarget() );
            kPShader.SetImageName(iTex, m_kVolumeImageB.GetNormalMapTarget().GetName());
            kPShader.SetTexture(iTex++, m_kVolumeImageB.GetNormalMapTarget());
            kPShader.SetImageName(iTex, m_kVolumeImageB.GetGradientMapTarget().GetName());
            kPShader.SetTexture(iTex++, m_kVolumeImageB.GetGradientMapTarget());
            kPShader.SetImageName(iTex, m_kVolumeImageB.GetOpacityMapGMTarget().GetName() );
            kPShader.SetTexture(iTex++, m_kVolumeImageB.GetOpacityMapGMTarget() );
            kPShader.SetImageName(iTex, m_kVolumeImageB.GetLaplaceMapTarget().GetName());
            kPShader.SetTexture(iTex++, m_kVolumeImageB.GetLaplaceMapTarget());
        }
    }
    
    
    /**
     * Sets the IsColor shader parameter values.
     */
    private void SetColorImage(Program pkPProgram)
    { 
        if ( (pkPProgram.GetUC("IsColorA") != null) && ((m_kVolumeImageA != null)) ) 
        {
            pkPProgram.GetUC("IsColorA").GetData()[0] = m_kVolumeImageA.IsColorImage() ? 1 : 0;
        } 
        if ( (pkPProgram.GetUC("IsColorB") != null) && ((m_kVolumeImageB.GetImage() != null)) ) 
        {
            pkPProgram.GetUC("IsColorB").GetData()[0] = m_kVolumeImageB.IsColorImage() ? 1 : 0;
        } 
    }
    
    private void setCurrentShader()
    {
        if ( m_iWhichShader == MIP )
        {
            m_iWhichShader = -1;
            MIPMode();
        }
        else if ( m_iWhichShader == DRR )
        {
            m_iWhichShader = -1;
            DRRMode();
        }
        else if ( m_iWhichShader == CMP )
        {
            m_iWhichShader = -1;
            CMPMode();
        }
        else if ( m_iWhichShader == SUR )
        {
            m_iWhichShader = -1;
            SURFASTMode();
        }
        else if ( m_iWhichShader == CMP_SUR )
        {
            m_iWhichShader = -1;
            SURMode();
        }
        else if ( m_iWhichShader == CUSTUM )
        {
            m_iWhichShader = -1;
            SetCustumBlend( m_iBlend, 0, m_iSrcBlend, m_iDstBlend, m_kBlendColor );
        }
        MULTIHISTOMode(m_bMultiHisto);
    }


}
