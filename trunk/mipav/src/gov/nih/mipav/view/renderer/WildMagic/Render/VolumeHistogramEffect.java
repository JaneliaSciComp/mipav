package gov.nih.mipav.view.renderer.WildMagic.Render;


import gov.nih.mipav.model.structures.ModelRGB;
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
public class VolumeHistogramEffect extends ShaderEffect
implements StreamInterface
{
    /** Shared volume data and textures. */
    private VolumeImage m_kVolumeImageA;

    /** Shared volume data and textures. */
    private VolumeImage m_kVolumeImageB;
    
    float m_fMin1;
    float m_fMin2;
    float m_fMax1;
    float m_fMax2;
    float m_fRange1;
    float m_fRange2;
    float m_fScale1;
    float m_fScale2;

    /** 
     * Creates a new VolumeShaderEffect object.
     * @param kVolumeImageA the VolumeImage containing shared data and textures for rendering.
     * @param kVolumeImageB second VolumeImage.
     */
    public VolumeHistogramEffect ( VolumeImage kVolumeImageA, VolumeImage kVolumeImageB )
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
    public VolumeHistogramEffect ( VolumeImage kVolumeImageA, VolumeImage kVolumeImageB, boolean bUnique )
    {
        m_kVolumeImageA = kVolumeImageA;
        m_kVolumeImageB = kVolumeImageB;
        Init( bUnique );
    }

    /**
     * memory cleanup.
     */
    public void dispose()
    {
        m_kVolumeImageA = null;
        m_kVolumeImageB = null;
        super.dispose();
    }

    /* (non-Javadoc)
     * @see WildMagic.LibGraphics.Effects.ShaderEffect#OnLoadPrograms(int, WildMagic.LibGraphics.Shaders.Program, WildMagic.LibGraphics.Shaders.Program)
     */
    public void OnLoadPrograms (int iPass, Program pkVProgram,
            Program pkPProgram)
    {
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("InvDataCount") != null ) 
        {
            pkProgram.GetUC("InvDataCount").GetData()[0] = 1.0f/(float)m_kVolumeImageA.GetImage().getSize();
        }
    }


    public void ZSlice( float fZ )
    {
        Program pkProgram = GetPProgram(0);
        if ( pkProgram != null && pkProgram.GetUC("ZSlice") != null ) 
        {
            pkProgram.GetUC("ZSlice").GetData()[0] = fZ;
        } 
        if ( pkProgram != null && pkProgram.GetUC("UseZSlice") != null ) 
        {
            pkProgram.GetUC("UseZSlice").GetData()[0] = 1;
        }  
        if ( pkProgram != null && pkProgram.GetUC("Min") != null ) 
        {
            pkProgram.GetUC("Min").GetData()[0] = m_fMin1;
            pkProgram.GetUC("Min").GetData()[1] = m_fMin2;
        } 
        if ( pkProgram != null && pkProgram.GetUC("Max") != null ) 
        {
            pkProgram.GetUC("Max").GetData()[0] = m_fMax1;
            pkProgram.GetUC("Max").GetData()[1] = m_fMax2;
        } 
        if ( pkProgram != null && pkProgram.GetUC("Range") != null ) 
        {
            pkProgram.GetUC("Range").GetData()[0] = m_fRange1;
            pkProgram.GetUC("Range").GetData()[1] = m_fRange2;
        } 
        if ( pkProgram != null && pkProgram.GetUC("Scale") != null ) 
        {
            pkProgram.GetUC("Scale").GetData()[0] = m_fScale1;
            pkProgram.GetUC("Scale").GetData()[1] = m_fScale2;
        } 
        if ( pkProgram != null && pkProgram.GetUC("AB") != null ) 
        {
            pkProgram.GetUC("AB").GetData()[0] = m_fRange1 / m_fRange2;
            pkProgram.GetUC("AB").GetData()[1] = ((m_fMin1 * m_fMax2) - (m_fMax1 * m_fMin2)) / m_fRange2;
        } 
    }

    /** Initializes the ShaderEffect vertex and pixel shader programs. */
    private void Init ( boolean bUnique )
    {
        /* Set single-pass rendering: */
        SetPassQuantity(1);

        SetVShader(0,new VertexShader("VolumeHistogramV"));
        SetPShader(0,new PixelShader("VolumeHistogramP", bUnique));

        GetPShader(0).SetTextureQuantity(2);
        int iTex = 0;
        GetPShader(0).SetImageName(iTex,m_kVolumeImageA.GetVolumeTarget().GetName());
        GetPShader(0).SetTexture(iTex++, m_kVolumeImageA.GetVolumeTarget() );
        GetPShader(0).SetImageName(iTex,m_kVolumeImageA.GetGradientMapTarget().GetName());
        GetPShader(0).SetTexture(iTex++, m_kVolumeImageA.GetGradientMapTarget() );
        
        m_fMin1 = (float)m_kVolumeImageA.GetImage().getMin();
        m_fMin2 = m_kVolumeImageA.GetGMMin();
        m_fMax1 = (float)m_kVolumeImageA.GetImage().getMax();
        m_fMax2 = m_kVolumeImageA.GetGMMax();
        m_fRange1 = m_fMax1 - m_fMin1;
        m_fRange2 = m_fMax2 - m_fMin2;
        m_fScale1 = (256 - 1) / m_fRange1;
        m_fScale2 = (256 - 1) / m_fRange2;
    }
}
