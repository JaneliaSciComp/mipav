package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.model.structures.ModelSimpleImage;

import WildMagic.LibFoundation.Mathematics.Matrix4f;
import WildMagic.LibGraphics.Effects.ShaderEffect;
import WildMagic.LibGraphics.ObjectSystem.StreamInterface;
import WildMagic.LibGraphics.Rendering.Texture;
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
    float m_fMin1;
    float m_fMin2;
    float m_fMax1;
    float m_fMax2;
    float m_fRange1;
    float m_fRange2;
    float m_fScale1;
    float m_fScale2;
    int m_iWidth;
    int m_iHeight;
    int[] m_aiExtents = new int[3];
    Matrix4f m_kImageTransform;
    boolean m_bUseTransform;
    int m_iNumTextures;
    boolean m_bHisto2D = false;


    public VolumeHistogramEffect ( Texture kTexA, float fMinA, float fMaxA, 
            int iWidth, int iHeight )
    {
        m_iWidth = iWidth;
        m_iHeight = iHeight;
        m_kImageTransform = Matrix4f.IDENTITY;
        m_bUseTransform = false;
        

        /* Set single-pass rendering: */
        SetPassQuantity(1);

        PixelShader kPShader = new PixelShader("VolumeHistogramP", true);
        VertexShader kVShader = new VertexShader("VolumeHistogram1DV", true);
        SetVShader(0,kVShader);
        SetPShader(0,kPShader);

        m_iNumTextures = 1;
        kPShader.SetTextureQuantity(2);
        kPShader.SetTexture( 0, kTexA );
        kPShader.SetImageName( 0, kTexA.GetName() );        
        m_fMin1 = fMinA;
        m_fMax1 = fMaxA;
        m_fRange1 = m_fMax1 - m_fMin1;
        m_fScale1 = 1.0f / m_fRange1;       
        
        for ( int i = 0; i < 3; i++ )
        {
            m_aiExtents[i] = kTexA.GetImage().GetBound(i);
        }
    }

    public VolumeHistogramEffect ( Texture kTexA, float fMinA, float fMaxA, 
            int iWidth, int iHeight, Matrix4f kImageTransform, boolean bUseTransform )
    {
        m_iWidth = iWidth;
        m_iHeight = iHeight;
        m_kImageTransform = kImageTransform;
        m_bUseTransform = bUseTransform;
        

        /* Set single-pass rendering: */
        SetPassQuantity(1);
        PixelShader kPShader = new PixelShader("VolumeHistogramP", true);
        VertexShader kVShader = new VertexShader("VolumeHistogram1D_MovingV", true);
        SetVShader(0,kVShader);
        SetPShader(0,kPShader);

        m_iNumTextures = 1;
        kPShader.SetTextureQuantity(2);
        kPShader.SetTexture( 0, kTexA );
        kPShader.SetImageName( 0, kTexA.GetName() );        
        m_fMin1 = fMinA;
        m_fMax1 = fMaxA;
        m_fRange1 = m_fMax1 - m_fMin1;
        m_fScale1 = 1.0f / m_fRange1;       
        
        for ( int i = 0; i < 3; i++ )
        {
            m_aiExtents[i] = kTexA.GetImage().GetBound(i);
        }
    }

    public VolumeHistogramEffect ( Texture kTexA, Texture kTexB, 
            float fMinA, float fMaxA, float fMinB, float fMaxB,
            int iWidth, int iHeight, Matrix4f kImageTransform, boolean bUseTransform )
    {
        m_iWidth = iWidth;
        m_iHeight = iHeight;
        m_kImageTransform = kImageTransform;
        m_bUseTransform = bUseTransform;
        

        /* Set single-pass rendering: */
        SetPassQuantity(1);
        PixelShader kPShader = new PixelShader("VolumeHistogramP", true);
        VertexShader kVShader = new VertexShader("VolumeHistogram2DV", true);
        SetVShader(0,kVShader);
        SetPShader(0,kPShader);
        
        m_bHisto2D = true ;
        m_iNumTextures =  2;
        kPShader.SetTextureQuantity(2);
        kPShader.SetTexture( 0, kTexA );
        kPShader.SetImageName( 0, kTexA.GetName() );
        kPShader.SetTexture( 1, kTexB );
        kPShader.SetImageName( 1, kTexB.GetName() );
        
       
        m_fMin1 = fMinA;
        m_fMax1 = fMaxA;
        m_fRange1 = m_fMax1 - m_fMin1;
        m_fScale1 = 1.0f / m_fRange1;       

        m_fMin2 = fMinB;
        m_fMax2 = fMaxB;
        m_fRange2 = m_fMax2 - m_fMin2;
        m_fScale2 = 1.0f / m_fRange2;

        for ( int i = 0; i < 3; i++ )
        {
            m_aiExtents[i] = kTexA.GetImage().GetBound(i);
        }
    }

    /**
     * memory cleanup.
     */
    public void dispose()
    {
        m_aiExtents = null;
        m_kImageTransform = null;
        super.dispose();
    }

    /* (non-Javadoc)
     * @see WildMagic.LibGraphics.Effects.ShaderEffect#OnLoadPrograms(int, WildMagic.LibGraphics.Shaders.Program, WildMagic.LibGraphics.Shaders.Program)
     */
    public void OnLoadPrograms (int iPass, Program pkVProgram,
            Program pkPProgram, Program pkCProgram)
    {
        if ( pkCProgram != null && pkCProgram.GetUC("Min") != null ) 
        {
            pkCProgram.GetUC("Min").GetData()[0] = m_fMin1;
            pkCProgram.GetUC("Min").GetData()[1] = m_fMin2;
        } 
        if ( pkCProgram != null && pkCProgram.GetUC("Scale") != null ) 
        {
            pkCProgram.GetUC("Scale").GetData()[0] = m_fScale1;
            pkCProgram.GetUC("Scale").GetData()[1] = m_fScale2;
        } 
        if ( pkCProgram != null && pkCProgram.GetUC("AB") != null ) 
        {
            pkCProgram.GetUC("AB").GetData()[0] = m_fRange1 / m_fRange2;
            pkCProgram.GetUC("AB").GetData()[1] = ((m_fMin1 * m_fMax2) - (m_fMax1 * m_fMin2)) / m_fRange2;
            

            //System.err.println( "AB = " + pkProgram.GetUC("AB").GetData()[0] + " " 
            //        + pkProgram.GetUC("AB").GetData()[1]);
        } 
        if ( pkCProgram != null && pkCProgram.GetUC("VertexScale") != null ) 
        {
            pkCProgram.GetUC("VertexScale").GetData()[0] = m_iWidth;
            pkCProgram.GetUC("VertexScale").GetData()[1] = m_iHeight;
        }  
        if ( pkCProgram != null && pkCProgram.GetUC("ImageSize") != null ) 
        {
            pkCProgram.GetUC("ImageSize").GetData()[0] = (m_aiExtents[0]-1);
            pkCProgram.GetUC("ImageSize").GetData()[1] = (m_aiExtents[1]-1);
            pkCProgram.GetUC("ImageSize").GetData()[2] = (m_aiExtents[2]-1);
            //System.err.println( m_aiExtents[0] + " " + m_aiExtents[1] + " " + m_aiExtents[2]);
        } 
        if ( pkCProgram != null && pkCProgram.GetUC("ImageSizeInv") != null ) 
        {
            pkCProgram.GetUC("ImageSizeInv").GetData()[0] = 1.0f / (m_aiExtents[0]-1);
            pkCProgram.GetUC("ImageSizeInv").GetData()[1] = 1.0f / (m_aiExtents[1]-1);
            pkCProgram.GetUC("ImageSizeInv").GetData()[2] = 1.0f / (m_aiExtents[2]-1);
        } 
        if ( pkCProgram != null && pkCProgram.GetUC("InverseTransformMatrix") != null ) 
        {
            for ( int i = 0; i < 4; i++ )
            {
                for ( int j = 0; j < 4; j++ )
                {
                    pkCProgram.GetUC("InverseTransformMatrix").GetData()[i*4+j] = m_kImageTransform.Get(i,j);
                }
            }
        } 
        if ( pkCProgram != null && pkCProgram.GetUC("UseTransform") != null ) 
        {
            if ( m_iNumTextures == 1 )
            {
                pkCProgram.GetUC("UseTransform").GetData()[0] = m_bUseTransform ? 1 : 0;
                pkCProgram.GetUC("UseTransform").GetData()[1] = 0;
            }
            else
            {
                pkCProgram.GetUC("UseTransform").GetData()[0] = 0;
                pkCProgram.GetUC("UseTransform").GetData()[1] =  m_bUseTransform ? 1 : 0;
            }
            //System.err.println( "UseTransform = " + pkProgram.GetUC("UseTransform").GetData()[0] + " " 
            //        + pkProgram.GetUC("UseTransform").GetData()[1]);
        } 
    }
    
    public void SetTransform( Matrix4f kMat)
    {
        m_kImageTransform = kMat;
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram != null && pkCProgram.GetUC("InverseTransformMatrix") != null ) 
        {
            for ( int i = 0; i < 4; i++ )
            {
                for ( int j = 0; j < 4; j++ )
                {
                    pkCProgram.GetUC("InverseTransformMatrix").GetData()[i*4+j] = m_kImageTransform.Get(i,j);
                }
            }
        }
    }


    public void ZSlice( float fZ )
    {
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram != null && pkCProgram.GetUC("ZSlice") != null ) 
        {
            pkCProgram.GetUC("ZSlice").GetData()[0] = fZ;
        } 
        if ( pkCProgram != null && pkCProgram.GetUC("UseZSlice") != null ) 
        {
            pkCProgram.GetUC("UseZSlice").GetData()[0] = 1;
        }  
    }

}
