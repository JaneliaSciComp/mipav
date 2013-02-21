package gov.nih.mipav.view.renderer.WildMagic.Render;

import WildMagic.LibFoundation.Mathematics.Matrix4f;
import WildMagic.LibGraphics.Effects.ShaderEffect;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.Shaders.PixelShader;
import WildMagic.LibGraphics.Shaders.Program;
import WildMagic.LibGraphics.Shaders.VertexShader;

public class LineMinimizationEffect extends ShaderEffect
{
    /**  */
    private static final long serialVersionUID = 7303288656271198298L;
    float m_fMin1;
    float m_fMin2;
    float m_fScale1;
    float m_fScale2;
    int m_iWidth;
    int m_iHeight;
    int[] m_aiExtents = new int[3];

    Matrix4f m_kToOrigin = Matrix4f.IDENTITY;
    Matrix4f m_kFromOrigin = Matrix4f.IDENTITY;
    float m_fRigid = 1.0f;
    float m_fDim;
    float[] m_afStartPoint;
    float[] m_afPt;
    float m_fPtLength;
    float[] m_afUnitDirections;
    private float m_fUnitTolerance;
    private float m_fMinDist;


    private float m_dNumSamples;
    
    public LineMinimizationEffect ( Texture kTexBracket, boolean bIs2D,
                                    Matrix4f kToOrigin, Matrix4f kFromOrigin,
                                    float rigid, float dim, float[] startPoint, float[] pt, float ptLength,
                                    float[] unitDirections, float unit_tolerance, float fMinDist )
    {

        m_kToOrigin = kToOrigin;
        m_kFromOrigin = kFromOrigin;

        m_fRigid = rigid;
        m_fDim = dim;
        m_afStartPoint = startPoint;
        m_afPt = pt;
        m_afUnitDirections = unitDirections;
        m_fUnitTolerance = unit_tolerance;
        m_fPtLength = ptLength;
        m_fMinDist = fMinDist;

        /* Set single-pass rendering: */
        SetPassQuantity(1);
        VertexShader kVShader = null;
        if ( bIs2D )
        {
            kVShader = new VertexShader("LineMinimizationV", true);
        }
        else
        {
            kVShader = new VertexShader("LineMinimization3DV", true);
        }
        
        PixelShader kPShader = new PixelShader("PassThrough4", true);
        SetVShader(0,kVShader);
        SetPShader(0,kPShader);
        /*
        kPShader.SetTextureQuantity(2);
        kPShader.SetTexture( 0, kTexBracket );
        kPShader.SetImageName( 0, kTexBracket.GetName() );
        kPShader.SetTexture( 1, kTransform );
        kPShader.SetImageName( 1, kTransform.GetName() );
        */

        kPShader.SetTextureQuantity(1);
        kPShader.SetTexture( 0, kTexBracket, "bracketImage" );
        kPShader.SetImageName( 0, kTexBracket.GetName(), "bracketImage" );
    }
    

    public LineMinimizationEffect ( Texture kTexA, Texture kTexB, float fMinDist, float dNumSamples, int iDim )
    {
        super(1);
        m_fMinDist = fMinDist;
        m_dNumSamples = dNumSamples;
        m_fDim = iDim;
        
        VertexShader kVShader = new VertexShader("LineMinimizationStep2V", true);
        PixelShader kPShader = new PixelShader("PassThrough4", true);
        SetVShader(0,kVShader);
        SetPShader(0,kPShader);

        kPShader.SetTextureQuantity(2);
        kPShader.SetTexture( 0, kTexA, "bracketImage" );
        kPShader.SetImageName( 0, kTexA.GetName(), "bracketImage" );
        kPShader.SetTexture( 1, kTexB, "entropy" );
        kPShader.SetImageName( 1, kTexB.GetName(), "entropy" );
    }
    
    public LineMinimizationEffect ( Texture kTexA, Texture kTexB, Texture kTexBracket,
                                    float fMinA, float fMaxA, float fMinB, float fMaxB,
                                    int iWidth, int iHeight )
    {
        m_iWidth = iWidth;
        m_iHeight = iHeight;

        /* Set single-pass rendering: */
        SetPassQuantity(1);
        VertexShader kVShader = new VertexShader("VolumeHistogramTransformV", true);
        PixelShader kPShader = new PixelShader("VolumeHistogramP", true);
        SetVShader(0,kVShader);
        SetPShader(0,kPShader);
        
        kPShader.SetTextureQuantity(3);
        kPShader.SetTexture( 0, kTexA, "imageA" );
        kPShader.SetImageName( 0, kTexA.GetName(), "imageA" );
        kPShader.SetTexture( 1, kTexB, "imageB" );
        kPShader.SetImageName( 1, kTexB.GetName(), "imageB" );
        kPShader.SetTexture( 2, kTexBracket, "transformImage" );
        kPShader.SetImageName( 2, kTexBracket.GetName(), "transformImage" );
        
        m_fMin1 = fMinA;
        m_fScale1 = 1;
        if ((fMaxA - fMinA) != 0) {
            m_fScale1 = (1.0f) / (fMaxA - fMinA);
        }

        m_fMin2 = fMinB;
        m_fScale2 = 1;
        if ((fMaxB - fMinB) != 0) {
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
        super.dispose();
    }

    /* (non-Javadoc)
     * @see WildMagic.LibGraphics.Effects.ShaderEffect#OnLoadPrograms(int, WildMagic.LibGraphics.Shaders.Program, WildMagic.LibGraphics.Shaders.Program)
     */
    public void OnLoadPrograms (int iPass, Program pkVProgram,
            Program pkPProgram, Program pkCProgram)
    {
        if ( pkCProgram.GetUC("dLogN") != null ) 
        {
            double dLogN = Math.log(m_dNumSamples);
            pkCProgram.GetUC("dLogN").GetData()[0] = (float)dLogN;
       }
        if ( pkCProgram.GetUC("nSamples") != null ) 
        {
            pkCProgram.GetUC("nSamples").GetData()[0] = m_dNumSamples;
            //System.err.println( "nSamples = " + pkCProgram.GetUC("nSamples").GetData()[0] );
       }
        if ( pkCProgram.GetUC("nSamplesInv") != null ) 
        {
            pkCProgram.GetUC("nSamplesInv").GetData()[0] = 1.0f/m_dNumSamples;
            //System.err.println( "nSamplesInv = " + pkCProgram.GetUC("nSamplesInv").GetData()[0] );
       }
        if ( pkCProgram.GetUC("nDims") != null ) 
        {
            pkCProgram.GetUC("nDims").GetData()[0] = m_fDim;
            //System.err.println( "nDims = " + pkCProgram.GetUC("nDims").GetData()[0] );
       }
        if ( pkCProgram.GetUC("logSamples") != null ) 
        {
            pkCProgram.GetUC("logSamples").GetData()[0] = (float)Math.log(m_fDim);
            //System.err.println( pkCProgram.GetUC("logSamples").GetData()[0] );
            //System.err.println( GetName() +  " dLogN = " + pkCProgram.GetUC("dLogN").GetData()[0]);
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
        } 
        if ( pkCProgram != null && pkCProgram.GetUC("toOrigin") != null ) 
        {
            m_kToOrigin.getData(pkCProgram.GetUC("toOrigin").GetData());
        }
        if ( pkCProgram != null && pkCProgram.GetUC("fromOrigin") != null ) 
        {
            m_kFromOrigin.getData(pkCProgram.GetUC("fromOrigin").GetData());
        }
        if ( pkCProgram != null && pkCProgram.GetUC("toOriginMatrix") != null ) 
        {
            m_kToOrigin.getData(pkCProgram.GetUC("toOriginMatrix").GetData());
        }
        if ( pkCProgram != null && pkCProgram.GetUC("fromOriginMatrix") != null ) 
        {
            m_kFromOrigin.getData(pkCProgram.GetUC("fromOriginMatrix").GetData());
        }
        if ( pkCProgram != null && pkCProgram.GetUC("rigid") != null ) 
        {
            pkCProgram.GetUC("rigid").GetData()[0] = m_fRigid;
        }
        if ( pkCProgram != null && pkCProgram.GetUC("Dim_2D") != null ) 
        {
            pkCProgram.GetUC("Dim_2D").GetData()[0] = m_fDim;
        }
        if ( pkCProgram != null && pkCProgram.GetUC("startPoint") != null ) 
        {
            pkCProgram.GetUC("startPoint").SetDataSource(m_afStartPoint);
        }
        if ( pkCProgram != null && pkCProgram.GetUC("pt") != null ) 
        {
            pkCProgram.GetUC("pt").SetDataSource(m_afPt);
        }
        if ( pkCProgram != null && pkCProgram.GetUC("ptLength") != null ) 
        {
            pkCProgram.GetUC("ptLength").GetData()[0] = m_fPtLength;
        }
        if ( pkCProgram != null && pkCProgram.GetUC("unit_directions") != null ) 
        {
            pkCProgram.GetUC("unit_directions").SetDataSource(m_afUnitDirections);
        }
        if ( pkCProgram.GetUC("unit_tolerance") != null ) 
        {
            pkCProgram.GetUC("unit_tolerance").GetData()[0] = m_fUnitTolerance;
        }             
        if ( pkCProgram.GetUC("minDist") != null ) 
        {
            pkCProgram.GetUC("minDist").GetData()[0] = m_fMinDist;
        }  
        if ( pkCProgram.GetUC("params") != null ) 
        {
            pkCProgram.GetUC("params").GetData()[0] = m_fPtLength;
            pkCProgram.GetUC("params").GetData()[1] = m_fMinDist;
            pkCProgram.GetUC("params").GetData()[2] = m_fUnitTolerance;
       }
    }
    
    public void setDim( float fDim )
    {
        m_fDim = fDim;
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram != null && pkCProgram.GetUC("Dim_2D") != null ) 
        {
            pkCProgram.GetUC("Dim_2D").GetData()[0] = m_fDim;
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
    
    public void setMinDist( float fMinDist )
    {
        m_fMinDist = fMinDist;
        Program pkCProgram = GetCProgram(0);
        if ( (pkCProgram != null) && pkCProgram.GetUC("minDist") != null ) 
        {
            pkCProgram.GetUC("minDist").GetData()[0] = m_fMinDist;
        }     
    }
    
    public void SetOrigin( Matrix4f kToOrigin, Matrix4f kFromOrigin )
    {
        m_kToOrigin = kToOrigin;
        m_kFromOrigin = kFromOrigin;
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram != null && pkCProgram.GetUC("toOrigin") != null ) 
        {
            m_kToOrigin.getData(pkCProgram.GetUC("toOrigin").GetData());
        }
        if ( pkCProgram != null && pkCProgram.GetUC("fromOrigin") != null ) 
        {
            m_kFromOrigin.getData(pkCProgram.GetUC("fromOrigin").GetData());
        }
    }

    public void setPt( float[] pt, float ptLength )
    {
        m_afPt = pt;
        m_fPtLength = ptLength;
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram != null && pkCProgram.GetUC("pt") != null ) 
        {
            pkCProgram.GetUC("pt").SetDataSource(m_afPt);
        }
        if ( pkCProgram != null && pkCProgram.GetUC("ptLength") != null ) 
        {
            pkCProgram.GetUC("ptLength").GetData()[0] = m_fPtLength;
        }
    }

    public void setRigid( float fRigid )
    {
        m_fRigid = fRigid;
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram != null && pkCProgram.GetUC("rigid") != null ) 
        {
            pkCProgram.GetUC("rigid").GetData()[0] = m_fRigid;
        }
    }
    
    public void setStartPoint( float[] startPoint )
    {
        m_afStartPoint = startPoint;
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram != null && pkCProgram.GetUC("startPoint") != null ) 
        {
            pkCProgram.GetUC("startPoint").SetDataSource(m_afStartPoint);
        }
    }
    
    public void setUnitDirections( float[] directions )
    {
        m_afUnitDirections = directions;
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram != null && pkCProgram.GetUC("unit_directions") != null ) 
        {
            pkCProgram.GetUC("unit_directions").SetDataSource(m_afUnitDirections);
        }
    }
    
    
    public void updateParameters( Matrix4f kToOrigin, Matrix4f kFromOrigin,
                                    float rigid, float dim, float[] startPoint, float[] pt, float ptLength,
                                    float[] unitDirections, float unit_tolerance, float fMinDist )
    {
        m_kToOrigin = kToOrigin;
        m_kFromOrigin = kFromOrigin;
        m_fRigid = rigid;
        m_fDim = dim;
        m_afStartPoint = startPoint;
        m_afPt = pt;
        m_fPtLength = ptLength;
        m_afUnitDirections = unitDirections;
        m_fUnitTolerance = unit_tolerance;
        m_fMinDist = fMinDist;
        
        Program pkCProgram = GetCProgram(0);
        if ( pkCProgram == null )
        {
            return;
        }
        if ( pkCProgram.GetUC("toOrigin") != null ) 
        {
            m_kToOrigin.getData(pkCProgram.GetUC("toOrigin").GetData());
        }
        if ( pkCProgram.GetUC("fromOrigin") != null ) 
        {
            m_kFromOrigin.getData(pkCProgram.GetUC("fromOrigin").GetData());
        }
        if ( pkCProgram.GetUC("toOriginMatrix") != null ) 
        {
            m_kToOrigin.getData(pkCProgram.GetUC("toOriginMatrix").GetData());
        }
        if ( pkCProgram.GetUC("fromOriginMatrix") != null ) 
        {
            m_kFromOrigin.getData(pkCProgram.GetUC("fromOriginMatrix").GetData());
        }
        if ( pkCProgram.GetUC("rigid") != null ) 
        {
            pkCProgram.GetUC("rigid").GetData()[0] = m_fRigid;
        }
        if ( pkCProgram.GetUC("Dim_2D") != null ) 
        {
            pkCProgram.GetUC("Dim_2D").GetData()[0] = m_fDim;
        }
        if ( pkCProgram.GetUC("startPoint") != null ) 
        {
            pkCProgram.GetUC("startPoint").SetDataSource(m_afStartPoint);
        }
        if ( pkCProgram.GetUC("pt") != null ) 
        {
            pkCProgram.GetUC("pt").SetDataSource(m_afPt);
        }
        if ( pkCProgram.GetUC("ptLength") != null ) 
        {
            pkCProgram.GetUC("ptLength").GetData()[0] = m_fPtLength;
        }
        if ( pkCProgram.GetUC("unit_directions") != null ) 
        {
            pkCProgram.GetUC("unit_directions").SetDataSource(m_afUnitDirections);
        }     
        if ( pkCProgram.GetUC("unit_tolerance") != null ) 
        {
            pkCProgram.GetUC("unit_tolerance").GetData()[0] = m_fUnitTolerance;
        }             
        if ( pkCProgram.GetUC("minDist") != null ) 
        {
            pkCProgram.GetUC("minDist").GetData()[0] = m_fMinDist;
        }     
        if ( pkCProgram.GetUC("params") != null ) 
        {
            pkCProgram.GetUC("params").GetData()[0] = m_fPtLength;
            pkCProgram.GetUC("params").GetData()[1] = m_fMinDist;
            pkCProgram.GetUC("params").GetData()[2] = m_fUnitTolerance;
       }
    }
    
}

