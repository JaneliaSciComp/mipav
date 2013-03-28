package gov.nih.mipav.view.renderer.WildMagic.Render;


import java.util.Vector;

import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.ClassificationWidgetState;
import gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.ClassificationWidget;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibGraphics.ObjectSystem.StreamInterface;
import WildMagic.LibGraphics.ObjectSystem.StringTree;
import WildMagic.LibGraphics.Rendering.AlphaState;
import WildMagic.LibGraphics.Rendering.Renderer;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.Geometry;
import WildMagic.LibGraphics.Shaders.PixelShader;
import WildMagic.LibGraphics.Shaders.Program;
import WildMagic.LibGraphics.Shaders.SamplerInformation;
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
    protected final static int MIP = 0;
    /** View Mode DRR Constant: */
    protected final static int DRR = 1;
    /** View Mode Composite Constant: */
    protected final static int CMP = 2;
    /** View Mode Surface Constant: */
    protected final static int SUR = 3;
    /** View Mode Composite-Surface Constant: */
    protected final static int CMP_SUR = 4;
    protected final static int CUSTUM = 5;

    protected static int ms_MaxLights = 8;
    
    protected static int ms_iNumLev = 10;

    protected static int ms_iMaxSamples = 1000;
    
    /** Shared volume data and textures. */
    protected VolumeImage m_kVolumeImageA;

    /** Shared volume data and textures. */
    protected VolumeImage m_kVolumeImageB;
  
    /** PixelShader program and data for Composite mode: */
    protected PixelShader m_kPShaderCMP = null;
    /** Indicates which shader to use (MIP, DDR, CMP, SUR, CMP_SUR, MULTIHISTO): */
    protected int m_iWhichShader = -1;
    
    /** Reference to the SceneImage texture: */
    protected Texture m_kSceneTarget;
    protected float[][] m_aafLight = new float[ms_MaxLights][4];
    protected ClassificationWidgetState[] m_akLevWidget = new ClassificationWidgetState[ms_iNumLev];
    protected int m_iUsedWidgets = 0;

    protected int m_iPasses = 1;

    protected VertexShader m_pkVShader;
    protected boolean m_bMultiHisto = false;
    protected int m_iBlend;
    protected int m_iSrcBlend;
    protected int m_iDstBlend;
    protected ColorRGBA m_kBlendColor = new ColorRGBA();
    protected float m_fMaxLength = 1f;

    protected float[] m_afBlendParam = new float[]{1f,0f,0f,0f};
    protected float[] m_afABBlendParam = new float[]{1f,0f,0f,0f};
    protected ModelRGB m_kRGBT;
    protected boolean m_bGradientMag = false;
    protected float m_fSamples;
    
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

        m_iPasses = 1;
        super.SetPassQuantity(1);
        super.SetRepeatQuantity(m_iPasses);
        m_fSamples = (m_kVolumeImageA.GetImage().getExtents()[2]*2.0f)/1000.0f;

		for ( int i = 0; i < m_akLevWidget.length; i++ )
		{
			m_akLevWidget[i] = new ClassificationWidgetState();
		}
		
        CreateVolumeTexture();
    }

    /**
     * Sets the blend factor shader parameter between imageA and imageB.
     * @param fBlend blend factor (range = 0-1).
     */
    public void Blend(float fBlend)
    {       
    	m_afBlendParam[0] = fBlend;
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
         for ( int i = 0; i < (int)m_kAlphaState.size(); i++)
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
        for (int i = 0; i < (int)m_kAlphaState.size(); i++)
        {
            m_kAlphaState.set(i, new AlphaState());
            m_kAlphaState.get(i).BlendEnabled = true;
            //m_kAlphaState.get(i).SrcBlend = AlphaState.SrcBlendMode.SBF_ONE;
            m_kAlphaState.get(i).SrcBlend = AlphaState.SrcBlendMode.SBF_CONSTANT_ALPHA;
            //m_kAlphaState.get(i).DstBlend = AlphaState.DstBlendMode.DBF_CONSTANT_ALPHA;
            m_kAlphaState.get(i).ConstantColor = new ColorRGBA(1.0f,1.0f,1.0f,
            		m_kVolumeImageA.getDRRNorm());
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
        
        for (int i = 0; i < m_kAlphaState.size(); i++)
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
        if ( m_bMultiHisto == bOn )
        {
        	return;
        }
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
    	Blend(m_afBlendParam[0]);
        SetColorImage(pkCProgram);
        setRGBTA(m_kRGBT);
        setVolumeSamples( m_fSamples );
        if ( m_kVolumeImageB.GetImage() != null )
        {
            //this.setABBlend(0.5f);
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
        if ( m_bMultiHisto )
        {
        	updateLevWidgetState( );
        } 
        if ( (pkCProgram != null) && pkCProgram.GetUC("ABBlend") != null ) 
        {
        	pkCProgram.GetUC("ABBlend").GetData()[0] = m_afABBlendParam[0];
        }

        super.OnLoadPrograms ( 0,  pkVProgram, pkPProgram, pkCProgram );    
    }
    
    public void printProgram() {}

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
    	m_afABBlendParam[0] = fBlend;
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
        for (int i = 0; i < (int)m_kAlphaState.size(); i++)
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
    
    public boolean GetGradientMagnitude()
    {
    	return m_bGradientMag;
    }

    /** 
     * Enables/Disables gradient magnitude filter.
     * @param bShow gradient magnitude filter on/off.
     */
    public void SetGradientMagnitude(boolean bShow)
    {
    	m_bGradientMag = bShow;
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
    public boolean SetLight( String kLightType, float[] afType )
    {
    	int iLight = Integer.valueOf( kLightType.substring( 5, 6 ) ).intValue();
    	boolean lightsChanged = false;
    	if ( m_aafLight[iLight][0] != afType[0] )
    	{
    		m_aafLight[iLight][0] = afType[0];
    		lightsChanged = true;
    	}
        return lightsChanged;
    }
    
    public void setMaxLength( float length )
    {
    	m_fMaxLength = length;
    }
    
    public void setRGBTA(ModelRGB RGBT) {
    	m_kRGBT = RGBT;
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

    public int setVolumeSamples( float fSample )
    {
    	m_fSamples = fSample;    	
        m_iPasses = Math.max(1, (int)(fSample * ms_iMaxSamples));
        //System.err.println( "Samples " + m_iPasses );
        SetRepeatQuantity(m_iPasses);
        Program pkCProgram = GetCProgram(0);
        SetVShader(0,m_pkVShader);
        /* The pixel shader defaults to CMP: */
        SetPShader(0,m_kPShaderCMP);
        SetCProgram(0,pkCProgram);
            
        setCurrentShader();
        
        pkCProgram = GetCProgram(0);
        if ( pkCProgram != null && pkCProgram.GetUC("StepSize") != null )
        {
            pkCProgram.GetUC("StepSize").GetData()[0] = m_fMaxLength/(float)m_iPasses;
        }
        return m_iPasses;
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
        for (int i = 0; i < (int)m_kAlphaState.size(); i++)
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
        for (int i = 0; i < (int)m_kAlphaState.size(); i++)
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
    
    public void updateLevWidgetState( Vector<ClassificationWidget> kLWS )
    {
		m_iUsedWidgets = kLWS.size();
    	boolean bChanged = false;
    	for ( int i = 0; i < kLWS.size(); i++ )
    	{
    		if ( !m_akLevWidget[i].equals( kLWS.elementAt(i).getState() ) )
    		{
    			m_akLevWidget[i].Copy( kLWS.elementAt(i).getState() );
    			bChanged = true;
    		}
    	}
    	if ( bChanged )
    	{
    		updateLevWidgetState();
    	}
    }

    protected void updateLevWidgetState()
    {
    	for ( int i = 0; i < m_iUsedWidgets; i++ )
    	{
    		Program pkCProgram = GetCProgram(0);
    		if ( pkCProgram.GetUC("UseWidget"+i) != null ) 
    		{
    			pkCProgram.GetUC("UseWidget"+i).SetDataSource(m_akLevWidget[i].UseWidget);
    			//System.err.println( "UseWidget"+iState + " " + m_akLevWidget[iState].UseWidget[0]);
    		}
    		if ( pkCProgram.GetUC("LevColor"+i) != null ) 
    		{
    			pkCProgram.GetUC("LevColor"+i).SetDataSource(m_akLevWidget[i].Color);
    			//System.err.println( "LevColor" + fR + " " + fG + " " + fB + " " + fA );
    		}
    		if ( pkCProgram.GetUC("LevMidLine"+i) != null ) 
    		{
    			pkCProgram.GetUC("LevMidLine"+i).SetDataSource(m_akLevWidget[i].MidLine);
    			//System.err.println( "LevMidLine " + m_akLevWidget[i].MidLine[0] + 
    			//		" " + m_akLevWidget[i].MidLine[1] + 
    			//		" " + m_akLevWidget[i].MidLine[2] + 
    			//		" " + m_akLevWidget[i].MidLine[3]  );
    		}
    		if ( pkCProgram.GetUC("LevLeftLine"+i) != null ) 
    		{
    			pkCProgram.GetUC("LevLeftLine"+i).SetDataSource(m_akLevWidget[i].LeftLine);
    			//System.err.println( "LevLeftLine " + m_akLevWidget[i].LeftLine[0] + 
    			//		" " + m_akLevWidget[i].LeftLine[1] + 
    			//		" " + m_akLevWidget[i].LeftLine[2] + 
    			//		" " + m_akLevWidget[i].LeftLine[3]  );
    		}
    		if ( pkCProgram.GetUC("LevRightLine"+i) != null ) 
    		{
    			pkCProgram.GetUC("LevRightLine"+i).SetDataSource(m_akLevWidget[i].RightLine);
    			//System.err.println( "LevRightLine " + m_akLevWidget[i].RightLine[0] + 
    			//		" " + m_akLevWidget[i].RightLine[1] + 
    			//		" " + m_akLevWidget[i].RightLine[2] + 
    			//		" " + m_akLevWidget[i].RightLine[3]  );
    		}
    		if ( pkCProgram.GetUC("BoundaryEmphasis"+i) != null ) 
    		{
    			pkCProgram.GetUC("BoundaryEmphasis"+i).SetDataSource(m_akLevWidget[i].BoundaryEmphasis);
    		}
    		if ( pkCProgram.GetUC("Shift"+i) != null ) 
    		{
    			pkCProgram.GetUC("Shift"+i).SetDataSource(m_akLevWidget[i].Shift);
    			//System.err.println( "Shift " + m_akLevWidget[i].Shift[0] + 
    			//	" " + m_akLevWidget[i].Shift[1] );
    		}
    		if ( pkCProgram.GetUC("InvY0MY1"+i) != null ) 
    		{            
    			pkCProgram.GetUC("InvY0MY1"+i).SetDataSource(m_akLevWidget[i].YRatio);
    			//System.err.println( "Shift " + m_akLevWidget[i].YRatio[0] + 
    			//		" " + m_akLevWidget[i].YRatio[1] + 
    			//		" " + m_akLevWidget[i].YRatio[2] );
    			//System.err.println( "" );
    		}
    		if ( pkCProgram.GetUC("Center"+i) != null ) 
    		{
    			pkCProgram.GetUC("Center"+i).SetDataSource(m_akLevWidget[i].Center); 
    			//System.err.println( "Center " + fX + " " + fY );
    		}
    		if ( pkCProgram.GetUC("Radius"+i) != null ) 
    		{
    			pkCProgram.GetUC("Radius"+i).SetDataSource(m_akLevWidget[i].Radius); 
    			//System.err.println( "Center " + fX + " " + fY );
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
    protected void CreateVolumeTexture ()
    {
        /* Create the vertex shader program, shared by all rendering types. It
         * is implemented in the VolumeShaderVertex.cg file: */        
        m_pkVShader = new VertexShader("VolumeShaderVertex");

        if ( m_kVolumeImageB.GetImage() == null )
        {
            m_kPShaderCMP = new PixelShader("VolumeShaderMultiPass" );
        }
        else
        {
            m_kPShaderCMP = new PixelShader("VolumeShaderABMultiPass");
        }
                 
        SetVShader(0,m_pkVShader);
        SetPShader(0,m_kPShaderCMP);
    }
    /**
     * Sets the IsColor shader parameter values.
     */
    protected void SetColorImage(Program pkPProgram)
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
    
    protected void setCurrentShader()
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


    public void RestoreGlobalState (int iPass, Renderer pkRenderer,
            boolean bPrimaryEffect)
    {
        AlphaState spkSave = pkRenderer.GetAlphaState();
        pkRenderer.SetAlphaState(m_kAlphaState.get(0));
        m_kAlphaState.set(0, spkSave);
    }
//
//    public SamplerInformation GetSamplerInformation (int iPass, int i)
//    {
//    	return super.GetSamplerInformation(0,i);
//    }
//    
//    public Texture GetTexture (int iPass, int i)
//    {
//    	//System.err.println( super.GetTexture(0,i).GetName() + " " + i );
//    	return super.GetTexture(0,i);
//    }
//    
//    public Texture GetTexture (int iPass, String kSamplerImageName)
//    {
//    	return super.GetTexture(0,kSamplerImageName);
//    }
//
//    public int GetTextureQuantity (int iPass)
//    {
//    	return super.GetTextureQuantity(0);
//    }
//    
//    public Program GetCProgram (int iPass)
//    {
//    	return super.GetCProgram(0);
//    }
//    
//    	
//    public void SetPassQuantity (int iPassQuantity)
//    {
//        if ( iPassQuantity > 0 )
//        {
//        	m_iPassQuantity = iPassQuantity;
//        }
//    }

    public void SetGlobalState (int iPass, Renderer pkRenderer,
                                boolean bPrimaryEffect)
    {
    	m_kAlphaState.get(0).BlendEnabled = true;

    	AlphaState spkSave = pkRenderer.GetAlphaState();
    	pkRenderer.SetAlphaState(m_kAlphaState.get(0));
    	m_kAlphaState.set(0, spkSave);
    }
    
//    public void LoadPrograms (Renderer kRenderer, Geometry pkGeometry, int iPass, int iMaxColors, int iMaxTCoords,
//            int iMaxVShaderImages, int iMaxPShaderImages)
//    {
//    	super.LoadPrograms(kRenderer, pkGeometry, 0, iMaxColors, iMaxTCoords, iMaxVShaderImages, iMaxPShaderImages );
//    }

//    public void LoadResources (Renderer pkRenderer, Geometry pkGeometry)
//    {
//    	LoadPrograms(pkRenderer, pkGeometry, 0,pkRenderer.GetMaxColors(),
//    			pkRenderer.GetMaxTCoords(),pkRenderer.GetMaxVShaderImages(),
//    			pkRenderer.GetMaxPShaderImages());
//
//    	// Load the programs into video memory.
//    	//Program pkVProgram = m_kVShader.get(iPass).GetProgram();
//    	//pkRenderer.LoadVProgram(pkVProgram);
//    	//pkRenderer.LoadPProgram(m_kPShader.get(iPass).GetProgram());
//    	Program pkCProgram = m_kCompiledPrograms.get(0);
//    	pkRenderer.LoadProgram(pkCProgram);
//
//    	// Load the textures into video memory.
//    	final int iPTQuantity = GetTextureQuantity(0);
//    	for (int i = 0; i < iPTQuantity; i++)
//    	{
//    		pkRenderer.LoadTexture(m_kPShader.get(0).GetTexture(i));
//    	}
//
//    	if (pkGeometry != null)
//    	{
//    		pkRenderer.LoadVAO(pkGeometry);
//    	}
//    }
//
//    public void ReleaseResources (Renderer pkRenderer,
//    		Geometry pkGeometry)
//    {
//    	final int iPTQuantity = GetTextureQuantity(0);
//    	for (int i = 0; i < iPTQuantity; i++)
//    	{
//    		pkRenderer.ReleaseTexture(m_kPShader.get(0).GetTexture(i));
//    	}
//
//    	// Release the programs from video memory.            
//    	if ( m_kCompiledPrograms != null )
//    	{
//    		Program pkCProgram = m_kCompiledPrograms.get(0);
//    		pkRenderer.ReleaseProgram(pkCProgram);
//
//    		// Release the programs from the shader objects.
//    		ReleasePrograms(0);
//    	}
//    }


}
