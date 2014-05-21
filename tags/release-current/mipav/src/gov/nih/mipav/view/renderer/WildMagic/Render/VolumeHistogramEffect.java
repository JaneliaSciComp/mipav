package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.view.renderer.J3D.PlaneRender;
import WildMagic.LibFoundation.Mathematics.Matrix4f;
import WildMagic.LibFoundation.Mathematics.Matrix4d;
import WildMagic.LibGraphics.Effects.ShaderEffect;
import WildMagic.LibGraphics.ObjectSystem.StreamInterface;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.Shaders.PixelShader;
import WildMagic.LibGraphics.Shaders.Program;
import WildMagic.LibGraphics.Shaders.Shader;
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
    /**  */
    private static final long serialVersionUID = 547439954218104278L;
    public static final int COLLAPSE_COLUMNS = 0;
    public static final int COLLAPSE_ROWS = 1;
    public static final int NONE = 2;
    float m_fMin1;
    float m_fMin2;
    float m_fScale1;
    float m_fScale2;
    int m_iWidth;
    int m_iHeight;
    int[] m_aiExtents = new int[3];
    Matrix4f m_kImageTransform;
    Matrix4d m_kImageTransformd;
    float m_fZSlice = 0.0f;
    float m_fUseZSlice = 0.0f;

    public VolumeHistogramEffect ( Texture kTexA, int iType )
    {
        VertexShader kVShader;
        PixelShader kPShader;
        /* Set single-pass rendering: */
        SetPassQuantity(1);
        if ( iType == COLLAPSE_COLUMNS )
        {
            kVShader = new VertexShader("VolumeHistogramColumnsV");
            kPShader = new PixelShader("VolumeHistogramColumnsP");
        }
        else if ( iType == COLLAPSE_ROWS )
        {
            kVShader = new VertexShader("VolumeHistogramRowsV");
            kPShader = new PixelShader("VolumeHistogramRowsP");
        }
        else
        {
            kVShader = new VertexShader("VolumeHistogramPassThroughV");
            kPShader = new PixelShader("TextureP", Shader.pixelShaderTexture2);
        }
        SetVShader(0,kVShader);
        SetPShader(0,kPShader);
        
        kPShader.SetTextureQuantity(1);
        kPShader.SetTexture( 0, kTexA, "BaseSampler" );
        kPShader.SetImageName( 0, kTexA.GetName(), "BaseSampler" );
    }



    public VolumeHistogramEffect ( Texture kTexA, Matrix4f kImageTransform )
    {

        for ( int i = 0; i < 3; i++ )
        {
            m_aiExtents[i] = kTexA.GetImage().GetBound(i);
        }
        m_kImageTransform = kImageTransform;
        
        VertexShader kVShader = new VertexShader("TransformImageV");
        PixelShader kPShader = new PixelShader("TransformImageP");
        SetPassQuantity(1);
        SetVShader(0,kVShader);
        SetPShader(0,kPShader);
        
        kPShader.SetTextureQuantity(1);
        kPShader.SetTexture( 0, kTexA, "BaseSampler" );
        kPShader.SetImageName( 0, kTexA.GetName(), "BaseSampler" );
    }

    public VolumeHistogramEffect ( Texture kTexA, Texture kTexB, 
            float fMinA, float fMaxA, float fMinB, float fMaxB,
            int iWidth, int iHeight, Matrix4f kImageTransform )
    {
        m_iWidth = iWidth;
        m_iHeight = iHeight;
        m_kImageTransform = kImageTransform;
        

        /* Set single-pass rendering: */
        SetPassQuantity(1);
        VertexShader kVShader = new VertexShader("VolumeHistogram2DV");
        PixelShader kPShader = new PixelShader("VolumeHistogramP");
        SetVShader(0,kVShader);
        SetPShader(0,kPShader);
        
        kPShader.SetTextureQuantity(2);
        kPShader.SetTexture( 0, kTexA, "imageA" );
        kPShader.SetImageName( 0, kTexA.GetName(), "imageA" );
        kPShader.SetTexture( 1, kTexB, "imageB" );
        kPShader.SetImageName( 1, kTexB.GetName(), "imageB" );
        
        /*
        kPShader.SetTextureQuantity(1);
        kPShader.SetTexture( 0, kTexA );
        kPShader.SetImageName( 0, kTexA.GetName() );
        */
       
        m_fMin1 = fMinA;
        m_fScale1 = 1;
        if ((fMaxA - fMinA) != 0) {
            //m_fScale1 = (iNBins - 1) / (fMaxA - fMinA);
            m_fScale1 = (1.0f) / (fMaxA - fMinA);
        }

        m_fMin2 = fMinB;
        m_fScale2 = 1;
        if ((fMaxB - fMinB) != 0) {
            //m_fScale2 = (iNBins - 1) / (fMaxB - fMinB);
            m_fScale2 = (1.0f) / (fMaxB - fMinB);
        }

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

        if ( pkCProgram != null && pkCProgram.GetUC("ZSlice") != null ) 
        {
            pkCProgram.GetUC("ZSlice").GetData()[0] = m_fZSlice;
            //System.err.println( "ZSlice: " + m_fZSlice );
        } 
        if ( pkCProgram != null && pkCProgram.GetUC("Min") != null ) 
        {
            pkCProgram.GetUC("Min").GetData()[0] = m_fMin1;
            pkCProgram.GetUC("Min").GetData()[1] = m_fMin2;
            //System.err.println( "Min = " + m_fMin1 + " " + m_fMin2 );
        } 
        if ( pkCProgram != null && pkCProgram.GetUC("Scale") != null ) 
        {
            pkCProgram.GetUC("Scale").GetData()[0] = m_fScale1;
            pkCProgram.GetUC("Scale").GetData()[1] = m_fScale2;
            //System.err.println( "Scale = " + m_fScale1 + " " + m_fScale2 );
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
            if ( (m_aiExtents[2]-1) > 0 )
            {
                pkCProgram.GetUC("ImageSizeInv").GetData()[2] = 1.0f / (m_aiExtents[2]-1);
            }
            else
            {
                pkCProgram.GetUC("ImageSizeInv").GetData()[2] = 0f;
            }
            //System.err.println( pkCProgram.GetUC("ImageSizeInv").GetData()[2] );
        } 
        if ( pkCProgram != null && pkCProgram.GetUC("InverseTransformMatrix") != null ) 
        {
            for ( int i = 0; i < 4; i++ )
            {
                for ( int j = 0; j < 4; j++ )
                {
                    pkCProgram.GetUC("InverseTransformMatrix").GetData()[i*4+j] = m_kImageTransform.get(i,j);
                }
            }
        } 
        if ( pkCProgram != null && pkCProgram.GetUC("InverseTransform") != null ) 
        {
            m_kImageTransform.getData(pkCProgram.GetUC("InverseTransform").GetData());
        }
        if ( pkCProgram != null && pkCProgram.GetUC("UseZSlice") != null ) 
        {
            pkCProgram.GetUC("UseZSlice").GetData()[0] = m_fUseZSlice;
        } 
    }
    
    public void SetImageSize( int iX, int iY, int iZ )
    {
        m_aiExtents[0] = iX;
        m_aiExtents[1] = iY;
        m_aiExtents[2] = iZ;

        Program pkCProgram = GetCProgram(0);
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
            if ( (m_aiExtents[2]-1) > 0 )
            {
                pkCProgram.GetUC("ImageSizeInv").GetData()[2] = 1.0f / (m_aiExtents[2]-1);
            }
            else
            {
                pkCProgram.GetUC("ImageSizeInv").GetData()[2] = 0f;
            }
            //System.err.println( pkCProgram.GetUC("ImageSizeInv").GetData()[2] );
        } 
    }
    
    public void SetTransform( Matrix4f kMat)
    {
        m_kImageTransform = kMat;
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram != null && pkCProgram.GetUC("InverseTransform") != null ) 
        {
            m_kImageTransform.getData(pkCProgram.GetUC("InverseTransform").GetData());
        }
        if ( pkCProgram != null && pkCProgram.GetUC("InverseTransformMatrix") != null ) 
        {
            m_kImageTransform.getData(pkCProgram.GetUC("InverseTransformMatrix").GetData());
            /*
            for ( int i = 0; i < 4; i++ )
            {
                for ( int j = 0; j < 4; j++ )
                {
                    pkCProgram.GetUC("InverseTransformMatrix").GetData()[i*4+j] = m_kImageTransform.Get(i,j);
                }
            }
            */
        }
    }
    
    public void SetTransform( Matrix4d kMat)
    {
        m_kImageTransformd = kMat;
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram != null && pkCProgram.GetUC("InverseTransform") != null ) 
        {
            m_kImageTransformd.getData(pkCProgram.GetUC("InverseTransform").GetData());
        }
        if ( pkCProgram != null && pkCProgram.GetUC("InverseTransformMatrix") != null ) 
        {
            m_kImageTransformd.getData(pkCProgram.GetUC("InverseTransformMatrix").GetData());
            /*
            for ( int i = 0; i < 4; i++ )
            {
                for ( int j = 0; j < 4; j++ )
                {
                    pkCProgram.GetUC("InverseTransformMatrix").GetData()[i*4+j] = m_kImageTransform.Get(i,j);
                }
            }
            */
        }
    }


    public void UseZSlice( )
    {
        m_fUseZSlice = 1f;
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram != null && pkCProgram.GetUC("UseZSlice") != null ) 
        {
            pkCProgram.GetUC("UseZSlice").GetData()[0] = m_fUseZSlice;
        } 
        //System.err.println( "ZSlice: " + m_fZSlice + " Step: " + m_fZStep + " " + (m_fZSlice + m_fZStep) );
    }
    
    public void ZSlice( float fZ )
    {
        m_fZSlice = fZ;
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram != null && pkCProgram.GetUC("ZSlice") != null ) 
        {
            pkCProgram.GetUC("ZSlice").GetData()[0] = m_fZSlice;
        } 
        //System.err.println( "ZSlice: " + m_fZSlice + " Step: " + m_fZStep + " " + (m_fZSlice + m_fZStep) );
    }

}
