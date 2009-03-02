package gov.nih.mipav.view.renderer.WildMagic.Render;


import gov.nih.mipav.view.renderer.J3D.RenderViewBase;
import WildMagic.LibGraphics.ObjectSystem.StreamInterface;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.Shaders.PixelShader;
import WildMagic.LibGraphics.Shaders.Program;
import WildMagic.LibGraphics.Shaders.VertexShader;

/** ShaderEffect class for calculating the volume normals on the GPU.  The
 * volume normals are calculated in two passes with two different shaders. The
 * shader code is based on the normal calculation function in
 * RenderViewBase.java.  The first pass calculates the fDX, fDY, and fDZ
 * values for the voxels.  The second pass calculates the normals as the
 * average of the current voxel normal with it's 6 axis-aligned neighbors.
 *
 * All calculations are done on the GPU, the textures remain on the GPU and
 * are not copied into system memory. After both passes are complete, the
 * texture containing the normals is accessible through the
 * VolumeImage.GetNormalMapTarget() Texture.
 *
 * The VolumeImageViewer class applies the VolumeCalcEffect and maps the
 * output textures to the VolumeImage object normal map texture.
 *
 * @see RenderViewBase.java
 * @see VolumeImageViewer.java
 */
public class VolumeCalcEffect extends VolumeClipEffect
    implements StreamInterface
{
    /** When true the volume data is color. */
    private boolean m_bIsColor = false;

    /** stores the gradient magnitude filter on/off value: */
    private float[] m_afMinMax = new float[]{0,0,0,0};
    
    /** Create a new VolumeCalcEffect shader with the VolumeImage data. This
     * fn creates the second-pass shader.
     * @param kTextureName the name of the output texture from the first pass.
     * @param kTexture the output texture from the first pass.
     */
    public VolumeCalcEffect ( String kTextureName, Texture kTexture, String kShaderName )
    {
        SetPassQuantity(1);
        SetVShader(0,new VertexShader("TextureV"));
        SetPShader(0,new PixelShader(kShaderName, false));
        GetPShader(0).SetTextureQuantity(1);
        GetPShader(0).SetImageName(0,kTextureName);
        GetPShader(0).SetTexture(0, kTexture );
    }

    /** Create a new VolumeCalcEffect shader with the VolumeImage data. This
     * fn creates the first-pass shader.
     * @param kVolumeImageA the shared volume data and textures.
     */
    public VolumeCalcEffect ( VolumeImage kVolumeImage, Texture kTexture, String kShaderName )
    {
        /* Set single-pass rendering: */
        SetPassQuantity(1);
        SetVShader(0,new VertexShader("TextureV"));
        SetPShader(0,new PixelShader(kShaderName + kVolumeImage.GetPostfix(), false));
        GetPShader(0).SetTextureQuantity(1);
        GetPShader(0).SetImageName(0,kTexture.GetImage().GetName());
        GetPShader(0).SetTexture(0, kTexture );
        m_bIsColor = kVolumeImage.GetImage().isColorImage();
        m_afMinMax[0] = (float)kVolumeImage.GetImage().getMin();
        m_afMinMax[1] = (float)kVolumeImage.GetImage().getMax();
    }

    
    /**
     * For creating the surface mask texture.
     * @param kVolumeImage Volume data
     * @param kClip current clipping state.
     */
    public VolumeCalcEffect ( VolumeImage kVolumeImage, VolumeClipEffect kClip )
    {
        /* Set single-pass rendering: */
        SetPassQuantity(1);
        SetVShader(0,new VertexShader("TextureV"));
        SetPShader(0,new PixelShader("CropClipped" + kVolumeImage.GetPostfix(), false));
        GetPShader(0).SetTextureQuantity(1);
        GetPShader(0).SetImageName(0,kVolumeImage.GetVolumeTarget().GetImage().GetName());
        GetPShader(0).SetTexture(0, kVolumeImage.GetVolumeTarget() );
        m_bIsColor = kVolumeImage.GetImage().isColorImage();
        this.m_afClipAll = kClip.m_afClipAll;
        this.m_afDoClip = kClip.m_afDoClip;
        this.m_aafClipData  = kClip.m_aafClipData;
        this.m_afClipEyeData  = kClip.m_afClipEyeData;
        this.m_afClipEyeInvData  = kClip.m_afClipEyeInvData;
        this.m_afClipArbData  = kClip.m_afClipArbData;
        m_afMinMax[0] = (float)kVolumeImage.GetImage().getMin();
        m_afMinMax[1] = (float)kVolumeImage.GetImage().getMax();
    }

    
    /**
     * For creating the surface mask texture.
     * @param kVolumeImage Volume data
     * @param kClip current clipping state.
     */
    public VolumeCalcEffect ( VolumeImage kVolumeImage, VolumeClipEffect kClip, boolean bGM )
    {
        /* Set single-pass rendering: */
        SetPassQuantity(1);
        SetVShader(0,new VertexShader("TextureV"));
        SetPShader(0,new PixelShader("SurfaceExtract", false));
        GetPShader(0).SetTextureQuantity(4);
        GetPShader(0).SetImageName(0,kVolumeImage.GetVolumeTarget().GetImage().GetName());
        GetPShader(0).SetTexture(0, kVolumeImage.GetVolumeTarget() );
        GetPShader(0).SetImageName(1,kVolumeImage.GetOpacityMapTarget().GetImage().GetName());
        GetPShader(0).SetTexture(1, kVolumeImage.GetOpacityMapTarget() );
        GetPShader(0).SetImageName(2,kVolumeImage.GetNormalMapTarget().GetImage().GetName());
        GetPShader(0).SetTexture(2, kVolumeImage.GetNormalMapTarget() );
        GetPShader(0).SetImageName(3,kVolumeImage.GetOpacityMapGMTarget().GetImage().GetName());
        GetPShader(0).SetTexture(3, kVolumeImage.GetOpacityMapGMTarget() );
        m_bIsColor = kVolumeImage.GetImage().isColorImage();
        this.m_afClipAll = kClip.m_afClipAll;
        this.m_afDoClip = kClip.m_afDoClip;
        this.m_aafClipData  = kClip.m_aafClipData;
        this.m_afClipEyeData  = kClip.m_afClipEyeData;
        this.m_afClipEyeInvData  = kClip.m_afClipEyeInvData;
        this.m_afClipArbData  = kClip.m_afClipArbData;
        m_afMinMax[0] = (float)kVolumeImage.GetImage().getMin();
        m_afMinMax[1] = (float)kVolumeImage.GetImage().getMax();
    }


    
    /**
     * memory cleanup.
     */
    public void dispose()
    {
        m_afMinMax = null;
        super.dispose();
    }
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.Render.VolumeClipEffect#OnLoadPrograms(int, WildMagic.LibGraphics.Shaders.Program, WildMagic.LibGraphics.Shaders.Program)
     */
    public void OnLoadPrograms (int iPass, Program pkVProgram,
                                Program pkPProgram)
    {
        // Sets the IsColor shader parameter based on whether the data is a
        // color volume or not.
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("IsColor") != null ) 
        {
            if ( m_bIsColor )
            {
                pkProgram.GetUC("IsColor").GetData()[0] = 1.0f;
            }
            else
            {
                pkProgram.GetUC("IsColor").GetData()[0] = 0.0f;
            }
        }  
        if ( pkProgram.GetUC("MinMax") != null ) 
        {
            pkProgram.GetUC("MinMax").SetDataSource(m_afMinMax);
        }  
        super.OnLoadPrograms ( iPass,  pkVProgram, pkPProgram );
    }
    
    
    /** Sets the step size shader parameter.
     * @param kVolumeImage the shared volume data and textures.
     */
    public void SetStepSize(VolumeImage kVolumeImage)
    {
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("StepSize") != null ) 
        {
            pkProgram.GetUC("StepSize").GetData()[0] = 1.0f/((float)(kVolumeImage.GetImage().getExtents()[0])-1);
            pkProgram.GetUC("StepSize").GetData()[1] = 1.0f/((float)(kVolumeImage.GetImage().getExtents()[1])-1);
            pkProgram.GetUC("StepSize").GetData()[2] = 1.0f/((float)(kVolumeImage.GetImage().getExtents()[2])-1);
            //System.err.println( m_afExtents[0] + " " + m_afExtents[1] + " " + m_afExtents[2] );
        }
    }
    
    /** Sets the step size shader parameter.
     * @param kVolumeImage the shared volume data and textures.
     */
    public void SetStepSize(float fX, float fY, float fZ)
    {
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("StepSize") != null ) 
        {
            pkProgram.GetUC("StepSize").GetData()[0] = fX;
            pkProgram.GetUC("StepSize").GetData()[1] = fY;
            pkProgram.GetUC("StepSize").GetData()[2] = fZ;
            //System.err.println( m_afExtents[0] + " " + m_afExtents[1] + " " + m_afExtents[2] );
        }
    }
    
    public void SetIsoVal(float fVal)
    {
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("IsoVal") != null ) 
        {
            pkProgram.GetUC("IsoVal").GetData()[0] = fVal;
        }
    }
    
    
    public void SetVolumeIndex(int iIndex)
    {
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("VolumeIndex") != null ) 
        {
            pkProgram.GetUC("VolumeIndex").GetData()[0] = iIndex;
        }

        if ( iIndex != 0 )
        {        
            if ( pkProgram.GetUC("IsColor") != null ) 
            {
                pkProgram.GetUC("IsColor").GetData()[0] = 0.0f;
            }  
        }
    }
    
}
