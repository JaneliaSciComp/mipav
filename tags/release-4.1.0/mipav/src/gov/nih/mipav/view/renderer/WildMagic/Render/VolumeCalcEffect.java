package gov.nih.mipav.view.renderer.WildMagic.Render;


import WildMagic.LibFoundation.Mathematics.*;
import WildMagic.LibGraphics.Rendering.*;
import WildMagic.LibGraphics.Shaders.*;
import WildMagic.LibGraphics.ObjectSystem.*;
import WildMagic.LibGraphics.Effects.*;

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
public class VolumeCalcEffect extends ShaderEffect
    implements StreamInterface
{

    /** Create a new VolumeCalcEffect shader with the VolumeImage data. This
     * fn creates the first-pass shader.
     * @param kVolumeImageA, the shared volume data and textures.
     */
    public VolumeCalcEffect ( VolumeImage kVolumeImageA )
    {
        /* Set single-pass rendering: */
        SetPassQuantity(1);
        SetVShader(0,new VertexShader("CalcNormalsPerSlice_Pass1"));
        SetPShader(0,new PixelShader("CalcNormalsPerSlice_Pass1", false));
        GetPShader(0).SetTextureQuantity(1);
        GetPShader(0).SetImageName(0,"VolumeImageA");
        GetPShader(0).SetTexture(0, kVolumeImageA.GetVolumeTarget() );

        m_bIsColor = kVolumeImageA.GetImage().isColorImage();
    }

    /** Create a new VolumeCalcEffect shader with the VolumeImage data. This
     * fn creates the second-pass shader.
     * @param kTextureName, the name of the output texture from the first pass.
     * @param kTexture, the output texture from the first pass.
     */
    public VolumeCalcEffect ( String kTextureName, Texture kTexture )
    {
        SetPassQuantity(1);
        SetVShader(0,new VertexShader("CalcNormalsPerSlice_Pass2"));
        SetPShader(0,new PixelShader("CalcNormalsPerSlice_Pass2", false));
        GetPShader(0).SetTextureQuantity(1);
        GetPShader(0).SetImageName(0,kTextureName);
        GetPShader(0).SetTexture(0, kTexture );
    }

    /** This function is called in LoadPrograms once the shader programs are
     * created.  It gives the ShaderEffect-derived classes a chance to do
     * any additional work to hook up the effect with the low-level objects.
     * @param iPass the ith rendering pass
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
                pkProgram.GetUC("IsColor").SetDataSource(new float[]{1,0,0,0});
            }
            else
            {
                pkProgram.GetUC("IsColor").SetDataSource(new float[]{0,0,0,0});
            }
        }       
    }

    
    /**
     * memory cleanup.
     */
    public void dispose()
    {
        m_afExtents = null;
        super.dispose();
    }

    /** Sets the step size shader parameter.
     * @param kVolumeImageA, the shared volume data and textures.
     */
    public void SetStepSize(VolumeImage kVolumeImageA)
    {
        m_afExtents[0] = 1.0f/((float)(kVolumeImageA.GetImage().getExtents()[0])-1);
        m_afExtents[1] = 1.0f/((float)(kVolumeImageA.GetImage().getExtents()[1])-1);
        m_afExtents[2] = 1.0f/((float)(kVolumeImageA.GetImage().getExtents()[2])-1);
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("StepSize") != null ) 
        {
            pkProgram.GetUC("StepSize").SetDataSource(m_afExtents);
            //System.err.println( m_afExtents[0] + " " + m_afExtents[1] + " " + m_afExtents[2] );
        }
    }
    /** Extents of the volume data for setting the texture step size. */    
    private float[] m_afExtents = new float[]{0,0,0,0};
    /** When true the volume data is color. */
    private boolean m_bIsColor = false;
}
