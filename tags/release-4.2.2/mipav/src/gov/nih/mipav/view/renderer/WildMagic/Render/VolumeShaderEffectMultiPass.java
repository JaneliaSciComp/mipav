package gov.nih.mipav.view.renderer.WildMagic.Render;


import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.ClassificationWidgetState;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibGraphics.ObjectSystem.StreamInterface;
import WildMagic.LibGraphics.ObjectSystem.StringTree;
import WildMagic.LibGraphics.Rendering.AlphaState;
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
public class VolumeShaderEffectMultiPass extends VolumeClipEffect
    implements StreamInterface
{
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

    private static int ms_MaxLights = 8;
    
    private static int ms_iNumLev = 10;

    private static int ms_iMaxSamples = 1000;
    
    /** Shared volume data and textures. */
    private VolumeImage m_kVolumeImageA;

    /** Shared volume data and textures. */
    private VolumeImage m_kVolumeImageB;

    /** PixelShader program and data for initializing mipav textures: */
    private PixelShader m_kPShaderInit = null;
  
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
        Program kPProgram = m_kPShaderCMP.GetProgram();
        if ( kPProgram.GetUC("Blend") != null )
        {
            kPProgram.GetUC("Blend").GetData()[0] = fBlend;
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
        Program kPProgram = m_kPShaderCMP.GetProgram();  
        if ( kPProgram.GetUC("MIP") != null )
        {
            kPProgram.GetUC("MIP").GetData()[0] = 0;
        }
        if ( kPProgram.GetUC("DRRA") != null )
        {
            kPProgram.GetUC("DRRA").GetData()[0] = 0;
        }
        if ( (m_kVolumeImageB != null) && (kPProgram.GetUC("DRRB") != null) )
        {
            kPProgram.GetUC("DRRB").GetData()[0] = 0;
        }
        if ( kPProgram.GetUC("Composite") != null )
        {
            kPProgram.GetUC("Composite").GetData()[0] = 0;
        }
        if ( kPProgram.GetUC("Surface") != null )
        {
            kPProgram.GetUC("Surface").GetData()[0] = 0;
        }
        if ( kPProgram.GetUC("MULTIHISTO") != null )
        {
            kPProgram.GetUC("MULTIHISTO").GetData()[0] = 0;
        }
    }

    /**
     * memory cleanup.
     */
    public void dispose()
    {
        m_kVolumeImageA = null;
        m_kVolumeImageB = null;
        m_kPShaderInit.dispose();
        m_kPShaderInit = null;

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
        Program kPProgram = m_kPShaderCMP.GetProgram();  
        if ( kPProgram.GetUC("MIP") != null )
        {
            kPProgram.GetUC("MIP").GetData()[0] = 0;
        }
        if ( kPProgram.GetUC("DRRA") != null )
        {
            kPProgram.GetUC("DRRA").GetData()[0] = m_kVolumeImageA.getDRRNorm();
        }
        if ( (m_kVolumeImageB != null) && (kPProgram.GetUC("DRRB") != null) )
        {
            kPProgram.GetUC("DRRB").GetData()[0] = m_kVolumeImageB.getDRRNorm();
        }
        if ( kPProgram.GetUC("Composite") != null )
        {
            kPProgram.GetUC("Composite").GetData()[0] = 0;
        }
        if ( kPProgram.GetUC("Surface") != null )
        {
            kPProgram.GetUC("Surface").GetData()[0] = 0;
        }
        if ( kPProgram.GetUC("MULTIHISTO") != null )
        {
            kPProgram.GetUC("MULTIHISTO").GetData()[0] = 0;
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
        Program kPProgram = m_kPShaderCMP.GetProgram();    
        if ( kPProgram.GetUC("MIP") != null )
        {
            kPProgram.GetUC("MIP").GetData()[0] = 1;
        }
        if ( kPProgram.GetUC("DRRA") != null )
        {
            kPProgram.GetUC("DRRA").GetData()[0] = 0;
        }
        if ( (m_kVolumeImageB != null) && (kPProgram.GetUC("DRRB") != null) )
        {
            kPProgram.GetUC("DRRB").GetData()[0] = 0;
        }
        if ( kPProgram.GetUC("Composite") != null )
        {
            kPProgram.GetUC("Composite").GetData()[0] = 0;
        }
        if ( kPProgram.GetUC("Surface") != null )
        {
            kPProgram.GetUC("Surface").GetData()[0] = 0;
        }
        if ( kPProgram.GetUC("MULTIHISTO") != null )
        {
            kPProgram.GetUC("MULTIHISTO").GetData()[0] = 0;
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
            Program kPProgram = m_kPShaderCMP.GetProgram();  
            if ( (kPProgram != null) && (kPProgram.GetUC("MULTIHISTO") != null) )
            {
                kPProgram.GetUC("MULTIHISTO").GetData()[0] = bOn? 1 : 0;
            }
        }
    }

    /** This function is called in LoadPrograms once the shader programs are
     * created.  Set up the user variable data sources.
     * @param iPass the ith rendering pass
     */
    public void OnLoadPrograms (int iPass, Program pkVProgram,
                                Program pkPProgram)
    {
        SetColorImage(pkPProgram);
        setABBlend(1.0f);
        setRGBTA(null);
        if ( m_kVolumeImageB != null )
        {
            setABBlend(0.5f);
            if ( pkPProgram.GetUC("ShowB") != null ) 
            {    
                pkPProgram.GetUC("ShowB").GetData()[0] = 1;
            }   
            setRGBTB(null);
        }
        for ( int i = 0; i < ms_MaxLights; i++ )
        {
            String kLightType = new String("Light"+(i)+"Type");
            if ( pkPProgram.GetUC(kLightType) != null)
            {
                pkPProgram.GetUC(kLightType).SetDataSource(m_aafLight[i]);
            }
        }
        super.OnLoadPrograms ( iPass,  pkVProgram, pkPProgram );
    }

    /**
     * Reload the current shader programs from disk, compile and parse and
     * send to the GPU.
     * @param kRenderer the Renderer object displaying the scene-graph which
     * will apply the shader programs.
     */
    public void Reload( WildMagic.LibGraphics.Rendering.Renderer kRenderer )
    {
        for ( int i = 0; i < m_iPasses; i++ )
        {
            Program kVProgram = GetVProgram(i);
            kVProgram.Release();
            VertexProgramCatalog.GetActive().Remove(kVProgram);

            Program kPProgram = GetPProgram(i);
            kPProgram.Release();
            PixelProgramCatalog.GetActive().Remove(kPProgram);

            VertexShader pkVShader = GetVShader(i);
            pkVShader.OnReleaseProgram();
            PixelShader pkPShader = GetPShader(i);
            pkPShader.OnReleaseProgram();
        }

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
        Program kPProgram = m_kPShaderCMP.GetProgram();  
        if ( (kPProgram != null) && kPProgram.GetUC("ABBlend") != null ) 
        {
            kPProgram.GetUC("ABBlend").GetData()[0] = fBlend;
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
            Program kPProgram = m_kPShaderCMP.GetProgram();  
            if ( (kPProgram != null) && (kPProgram.GetUC("BackgroundColor") != null) ) 
            {
                kPProgram.GetUC("BackgroundColor").GetData()[0] = kColor.R;
                kPProgram.GetUC("BackgroundColor").GetData()[1] = kColor.G;
                kPProgram.GetUC("BackgroundColor").GetData()[2] = kColor.B;
                kPProgram.GetUC("BackgroundColor").GetData()[3] = kColor.A;
            }
        }
    }

    public void SetCustumBlend(int iBlendEquation, int iLogicOp, int iSrcBlend, int iDstBlend, ColorRGBA kColor  )
    {        
        for (int i = 1; i < (int)m_kAlphaState.size(); i++)
        {
            m_kAlphaState.set(i, new AlphaState());
            m_kAlphaState.get(i).BlendEnabled = true;
            m_kAlphaState.get(i).BlendEquation = AlphaState.BlendEquationMap.get(iBlendEquation);
            m_kAlphaState.get(i).SrcBlend = AlphaState.SrcBlendModeMap.get(iSrcBlend);
            m_kAlphaState.get(i).DstBlend = AlphaState.DstBlendModeMap.get(iDstBlend);
            m_kAlphaState.get(i).ConstantColor.Copy(kColor);
        }
        //System.err.println( kColor.A );
    }
    

    /** 
     * Enables/Disables gradient magnitude filter.
     * @param bShow gradient magnitude filter on/off.
     */
    public void SetGradientMagnitude(boolean bShow)
    {
        Program kPProgram = m_kPShaderCMP.GetProgram();  
        if ( kPProgram.GetUC("GradientMagnitude") != null ) 
        {
            kPProgram.GetUC("GradientMagnitude").GetData()[0] = bShow? 1 : 0;
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
            Program kPProgram = m_kPShaderCMP.GetProgram();  
            if ( (kPProgram != null) && (kPProgram.GetUC("ColorLUTOnA") != null) ) 
            {
                if ( RGBT != null )
                {
                    kPProgram.GetUC("ColorLUTOnA").GetData()[0] = RGBT.getROn() ? 1.0f : 0.0f;
                    kPProgram.GetUC("ColorLUTOnA").GetData()[1] = RGBT.getGOn() ? 1.0f : 0.0f;
                    kPProgram.GetUC("ColorLUTOnA").GetData()[2] = RGBT.getBOn() ? 1.0f : 0.0f;

                }
                else
                {
                    kPProgram.GetUC("ColorLUTOnA").GetData()[0] = 1.0f;
                    kPProgram.GetUC("ColorLUTOnA").GetData()[1] = 1.0f;
                    kPProgram.GetUC("ColorLUTOnA").GetData()[2] = 1.0f;
                }
            }
        }
    }
    public void setRGBTB(ModelRGB RGBT) {
        if ( m_kPShaderCMP != null )
        {
            Program kPProgram = m_kPShaderCMP.GetProgram();  
            if ( (kPProgram != null) && (kPProgram.GetUC("ColorLUTOnB") != null) ) 
            {
                if ( RGBT != null )
                {
                    kPProgram.GetUC("ColorLUTOnB").GetData()[0] = RGBT.getROn() ? 1.0f : 0.0f;
                    kPProgram.GetUC("ColorLUTOnB").GetData()[1] = RGBT.getGOn() ? 1.0f : 0.0f;
                    kPProgram.GetUC("ColorLUTOnB").GetData()[2] = RGBT.getBOn() ? 1.0f : 0.0f;
                }
                else
                {
                    kPProgram.GetUC("ColorLUTOnB").GetData()[0] = 1.0f;
                    kPProgram.GetUC("ColorLUTOnB").GetData()[1] = 1.0f;
                    kPProgram.GetUC("ColorLUTOnB").GetData()[2] = 1.0f;                
                }
            }
        }
    }

    public void setVolumeSamples( float fSample )
    {
        m_iPasses = Math.max(1, (int)(fSample * ms_iMaxSamples));
        SetPassQuantity(m_iPasses);
        for ( int i = 0; i < m_iPasses; i++ )
        {
            SetVShader(i,m_pkVShader);
            /* The pixel shader defaults to CMP: */
            SetPShader(i,m_kPShaderCMP);
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
        Program kPProgram = m_kPShaderCMP.GetProgram();    
        if ( kPProgram.GetUC("MIP") != null )
        {
            kPProgram.GetUC("MIP").GetData()[0] = 0;
        }
        if ( kPProgram.GetUC("DRRA") != null )
        {
            kPProgram.GetUC("DRRA").GetData()[0] = 0;
        }
        if ( (m_kVolumeImageB != null) && (kPProgram.GetUC("DRRB") != null) )
        {
            kPProgram.GetUC("DRRB").GetData()[0] = 0;
        }
        if ( kPProgram.GetUC("Composite") != null )
        {
            kPProgram.GetUC("Composite").GetData()[0] = 0;
        }
        if ( kPProgram.GetUC("Surface") != null )
        {
            kPProgram.GetUC("Surface").GetData()[0] = 1;
        }
        if ( kPProgram.GetUC("MULTIHISTO") != null )
        {
            kPProgram.GetUC("MULTIHISTO").GetData()[0] = 0;
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
        Program kPProgram = m_kPShaderCMP.GetProgram();    
        if ( kPProgram.GetUC("MIP") != null )
        {
            kPProgram.GetUC("MIP").GetData()[0] = 0;
        }
        if ( kPProgram.GetUC("DRRA") != null )
        {
            kPProgram.GetUC("DRRA").GetData()[0] = 0;
        }
        if ( (m_kVolumeImageB != null) && (kPProgram.GetUC("DRRB") != null) )
        {
            kPProgram.GetUC("DRRB").GetData()[0] = 0;
        }
        if ( kPProgram.GetUC("Composite") != null )
        {
            kPProgram.GetUC("Composite").GetData()[0] = 1;
        }
        if ( kPProgram.GetUC("Surface") != null )
        {
            kPProgram.GetUC("Surface").GetData()[0] = 1;
        }
        if ( kPProgram.GetUC("MULTIHISTO") != null )
        {
            kPProgram.GetUC("MULTIHISTO").GetData()[0] = 0;
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
                Program pkProgram = GetPProgram(i);
                if ( pkProgram.GetUC("UseWidget"+iState) != null ) 
                {
                    pkProgram.GetUC("UseWidget"+iState).SetDataSource(m_akLevWidget[iState].UseWidget);
                    //System.err.println( "UseWidget"+iState + " " + m_akLevWidget[iState].UseWidget[0]);
                }
                if ( pkProgram.GetUC("LevColor"+iState) != null ) 
                {
                    pkProgram.GetUC("LevColor"+iState).SetDataSource(m_akLevWidget[iState].Color);
                    //System.err.println( "LevColor" + fR + " " + fG + " " + fB + " " + fA );
                }
                if ( pkProgram.GetUC("LevMidLine"+iState) != null ) 
                {
                    pkProgram.GetUC("LevMidLine"+iState).SetDataSource(m_akLevWidget[iState].MidLine);
                    //System.err.println( "LevMidLine" + fX1 + " " + fY1 + " " + fX2 + " " + fY2 );
                }
                if ( pkProgram.GetUC("LevLeftLine"+iState) != null ) 
                {
                    pkProgram.GetUC("LevLeftLine"+iState).SetDataSource(m_akLevWidget[iState].LeftLine);
                    //System.err.println( "LevLeftLine" + fX1 + " " + fY1 + " " + fX2 + " " + fY2 );
                }
                if ( pkProgram.GetUC("LevRightLine"+iState) != null ) 
                {
                    pkProgram.GetUC("LevRightLine"+iState).SetDataSource(m_akLevWidget[iState].RightLine);
                    //System.err.println( "LevRightLine" + fX1 + " " + fY1 + " " + fX2 + " " + fY2 );
                }
                if ( pkProgram.GetUC("BoundaryEmphasis"+iState) != null ) 
                {
                    pkProgram.GetUC("BoundaryEmphasis"+iState).SetDataSource(m_akLevWidget[iState].BoundaryEmphasis);
                }
                if ( pkProgram.GetUC("Shift"+iState) != null ) 
                {
                    //System.err.println( "Shift" + iState );
                    pkProgram.GetUC("Shift"+iState).GetData()[0] = fShiftL;
                    pkProgram.GetUC("Shift"+iState).GetData()[1] = fShiftR;
                }
                if ( pkProgram.GetUC("InvY0MY1"+iState) != null ) 
                {            
                    //System.err.println( "InvY0MY1" + iState );
                    float fLeftInvY0MY1 = 1.0f / (m_akLevWidget[iState].LeftLine[1] - m_akLevWidget[iState].LeftLine[3]);
                    float fMidInvY0MY1 = 1.0f / (m_akLevWidget[iState].MidLine[1] - m_akLevWidget[iState].MidLine[3]);
                    float fRightInvY0MY1 = 1.0f / (m_akLevWidget[iState].RightLine[1] - m_akLevWidget[iState].RightLine[3]);
                    pkProgram.GetUC("InvY0MY1"+iState).GetData()[0] = fLeftInvY0MY1;
                    pkProgram.GetUC("InvY0MY1"+iState).GetData()[1] = fMidInvY0MY1;
                    pkProgram.GetUC("InvY0MY1"+iState).GetData()[2] = fRightInvY0MY1;
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

        if ( m_kVolumeImageB == null )
        {
            m_kPShaderCMP = new PixelShader("VolumeShaderMultiPass");
        }
        else
        {
            m_kPShaderCMP = new PixelShader("VolumeShaderABMultiPass");
        }
        initTexturesVol(m_kPShaderCMP);
         
        m_kPShaderInit = new PixelShader("LoadMIPAVTextures");
        initTextures(m_kPShaderInit);
        
        for ( int i = 0; i < m_iPasses; i++ )
        {
            SetVShader(i,m_pkVShader);
            SetPShader(i,m_kPShaderInit);
        }
    }

    
    private void initTextures( PixelShader kPShader )
    {        
        int iTex = 0;
        kPShader.SetTextureQuantity(16);
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
        

        kPShader.SetImageName(iTex, m_kVolumeImageA.GetSurfaceTarget().GetName());
        kPShader.SetTexture(iTex++, m_kVolumeImageA.GetSurfaceTarget());
        
        
        if ( m_kVolumeImageB != null )
        {
            kPShader.SetImageName(iTex, m_kVolumeImageB.GetVolumeTarget().GetName() );
            kPShader.SetTexture(iTex++, m_kVolumeImageB.GetVolumeTarget() );
            kPShader.SetImageName(iTex, m_kVolumeImageB.GetColorMapTarget().GetName() );
            kPShader.SetTexture(iTex++, m_kVolumeImageB.GetColorMapTarget() );
            kPShader.SetImageName(iTex, m_kVolumeImageB.GetOpacityMapTarget().GetName() );
            kPShader.SetTexture(iTex++, m_kVolumeImageB.GetOpacityMapTarget() );
            kPShader.SetImageName(iTex, m_kVolumeImageB.GetNormalMapTarget().GetName());
            kPShader.SetTexture(iTex++, m_kVolumeImageB.GetNormalMapTarget());
            kPShader.SetImageName(iTex, m_kVolumeImageB.GetGradientMapTarget().GetName());
            kPShader.SetTexture(iTex++, m_kVolumeImageB.GetGradientMapTarget());
            kPShader.SetImageName(iTex, m_kVolumeImageB.GetOpacityMapGMTarget().GetName() );
            kPShader.SetTexture(iTex++, m_kVolumeImageB.GetOpacityMapGMTarget() );
            kPShader.SetImageName(iTex, m_kVolumeImageB.GetSecondDerivativeMapTarget().GetName());
            kPShader.SetTexture(iTex++, m_kVolumeImageB.GetSecondDerivativeMapTarget());
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
        kPShader.SetImageName(iTex, m_kVolumeImageA.GetSecondDerivativeMapTarget().GetName());
        kPShader.SetTexture(iTex++, m_kVolumeImageA.GetSecondDerivativeMapTarget());
        

        //kPShader.SetImageName(iTex, m_kVolumeImageA.GetSurfaceTarget().GetName());
        //kPShader.SetTexture(iTex++, m_kVolumeImageA.GetSurfaceTarget());
        
        
        if ( m_kVolumeImageB != null )
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
            kPShader.SetImageName(iTex, m_kVolumeImageB.GetSecondDerivativeMapTarget().GetName());
            kPShader.SetTexture(iTex++, m_kVolumeImageB.GetSecondDerivativeMapTarget());
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
        if ( (pkPProgram.GetUC("IsColorB") != null) && ((m_kVolumeImageB != null)) ) 
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
        MULTIHISTOMode(m_bMultiHisto);
    }


}
