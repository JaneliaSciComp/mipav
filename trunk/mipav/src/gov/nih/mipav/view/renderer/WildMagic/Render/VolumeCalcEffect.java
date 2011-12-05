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
    /**  */
    private static final long serialVersionUID = -4844378132828150930L;

    /** When true the volume data is color. */
    private boolean m_bIsColor = false;

    /** stores the gradient magnitude filter on/off value: */
    private float[] m_afMinMax = new float[]{0,0,0,0};
    

    /** Create a new VolumeCalcEffect shader with the VolumeImage data. This
     * fn creates the first-pass shader.
     * @param kVolumeImageA the shared volume data and textures.
     */
    public VolumeCalcEffect ( VolumeImage kVolumeImage, Texture kTexture, String kShaderName, boolean bPostFix )
    {
        /* Set single-pass rendering: */
        SetPassQuantity(1);
        SetVShader(0,new VertexShader("TextureV"));
        String kName = kShaderName + (bPostFix ? kVolumeImage.GetPostfix() : "");
        PixelShader kPShader = new PixelShader(kName, false);
        SetPShader(0,kPShader);
        kPShader.SetTextureQuantity(1);
        kPShader.SetImageName(0,kTexture.GetImage().GetName());
        kPShader.SetTexture(0, kTexture );
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
        PixelShader kPShader = new PixelShader("CropClipped" + kVolumeImage.GetPostfix(), false);
        SetPShader(0,kPShader);
        kPShader.SetTextureQuantity(1);
        kPShader.SetImageName(0,kVolumeImage.GetVolumeTarget().GetImage().GetName());
        kPShader.SetTexture(0, kVolumeImage.GetVolumeTarget() );
        m_bIsColor = kVolumeImage.GetImage().isColorImage();
        this.m_afClipAll = kClip.m_afClipAll;
        this.m_afDoClip = kClip.m_afDoClip;
        this.m_aafClipData  = kClip.m_aafClipData;
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
        PixelShader kPShader = new PixelShader("SurfaceExtract", false);
        SetPShader(0,kPShader);
        kPShader.SetTextureQuantity(2);
        kPShader.SetImageName(0,kVolumeImage.GetVolumeTarget().GetImage().GetName());
        kPShader.SetTexture(0, kVolumeImage.GetVolumeTarget() );
        kPShader.SetImageName(1,kVolumeImage.GetColorMapTarget().GetImage().GetName());
        kPShader.SetTexture(1, kVolumeImage.GetColorMapTarget() );
        m_bIsColor = kVolumeImage.GetImage().isColorImage();
        this.m_afClipAll = kClip.m_afClipAll;
        this.m_afDoClip = kClip.m_afDoClip;
        this.m_aafClipData  = kClip.m_aafClipData;
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
                                Program pkPProgram, Program pkCProgram)
    {
        // Sets the IsColor shader parameter based on whether the data is a
        // color volume or not.
        if ( pkCProgram.GetUC("IsColor") != null ) 
        {
            if ( m_bIsColor )
            {
                pkCProgram.GetUC("IsColor").GetData()[0] = 1.0f;
            }
            else
            {
                pkCProgram.GetUC("IsColor").GetData()[0] = 0.0f;
            }
        }  
        if ( pkCProgram.GetUC("MinMax") != null ) 
        {
            pkCProgram.GetUC("MinMax").SetDataSource(m_afMinMax);
        }  
        super.OnLoadPrograms ( iPass,  pkVProgram, pkPProgram, pkCProgram );
    }
    
    
    public void SetIsoVal(float fVal)
    {
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram.GetUC("IsoVal") != null ) 
        {
            pkCProgram.GetUC("IsoVal").GetData()[0] = fVal;
        }
    }
    
    /** Sets the step size shader parameter.
     * @param kVolumeImage the shared volume data and textures.
     */
    public void SetStepSize(float fX, float fY, float fZ)
    {
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram.GetUC("StepSize") != null ) 
        {
            pkCProgram.GetUC("StepSize").GetData()[0] = fX;
            pkCProgram.GetUC("StepSize").GetData()[1] = fY;
            pkCProgram.GetUC("StepSize").GetData()[2] = fZ;
            //System.err.println( m_afExtents[0] + " " + m_afExtents[1] + " " + m_afExtents[2] );
        }
    }
    
    /** Sets the step size shader parameter.
     * @param kVolumeImage the shared volume data and textures.
     */
    public void SetStepSize(VolumeImage kVolumeImage)
    {
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram.GetUC("StepSize") != null ) 
        {
            pkCProgram.GetUC("StepSize").GetData()[0] = 1.0f/((float)(kVolumeImage.GetImage().getExtents()[0])-1);
            pkCProgram.GetUC("StepSize").GetData()[1] = 1.0f/((float)(kVolumeImage.GetImage().getExtents()[1])-1);
            pkCProgram.GetUC("StepSize").GetData()[2] = 1.0f/((float)(kVolumeImage.GetImage().getExtents()[2])-1);
            //System.err.println( m_afExtents[0] + " " + m_afExtents[1] + " " + m_afExtents[2] );
        }
    }
    
    
    public void SetVolumeIndex(int iIndex)
    {
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram.GetUC("VolumeIndex") != null ) 
        {
            pkCProgram.GetUC("VolumeIndex").GetData()[0] = iIndex;
        }

        if ( iIndex != 0 )
        {        
            if ( pkCProgram.GetUC("IsColor") != null ) 
            {
                pkCProgram.GetUC("IsColor").GetData()[0] = 0.0f;
            }  
        }
    }
    
}
