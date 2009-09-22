package gov.nih.mipav.view.renderer.WildMagic.Render;
import WildMagic.LibGraphics.Effects.*;
import WildMagic.LibGraphics.ObjectSystem.*;
import WildMagic.LibGraphics.Shaders.*;
import WildMagic.LibGraphics.Rendering.*;

public class ImageReduceEffect extends ShaderEffect
    implements StreamInterface
{

    public static final int SUM = 0;
    public static final int SUM_X = 1;
    public static final int SUM_Y = 2;
    public static final int ENTROPY = 3;
    public static final int ENTROPY_X = 4;
    public static final int ENTROPY_Y = 5;
    public static final int SUM_XA = 6;
    public static final int SUM_YA = 7;
    
    private int m_iWidth;
    private int m_iHeight;
    private double m_dNumSamples;
    
    /** Creates a new TextureEffect with the texture specified.
     * @param rkBaseName the name of the texture image.
     */
    public ImageReduceEffect (Texture kTarget, Texture kTargetB, int iWidth, int iHeight, double dNumSamples, int iType)
    {
        super(1);
        m_kVShader.set(0, new VertexShader("TextureV", true));

        if ( iType == SUM_X )
        {
            m_kPShader.set(0, new PixelShader("ImageReduce_Sum_2D_to_1D_x", true));
            SetName( "ImageReduce_Sum_2D_to_1D_x" + " " + iWidth + " " + iHeight );
        }
        else if ( iType == SUM_Y )
        {
            m_kPShader.set(0, new PixelShader("ImageReduce_Sum_2D_to_1D_y", true));
            SetName( "ImageReduce_Sum_2D_to_1D_y" + " " + iWidth + " " + iHeight );
        }
        else if ( iType == ENTROPY_X )
        {
            m_kPShader.set(0, new PixelShader("ImageReduce_Entropy_1D_x", true));
            SetName( "ImageReduce_Entropy_1D_x" + " " + iWidth + " " + iHeight );
        }
        else if ( iType == ENTROPY_Y )
        {
            m_kPShader.set(0, new PixelShader("ImageReduce_Entropy_1D_y", true));
            SetName( "ImageReduce_Entropy_1D_y" + " " + iWidth + " " + iHeight );
        }
        else if ( iType == SUM_XA )
        {
            m_kPShader.set(0, new PixelShader("ImageReduce_Sum_2D_to_1DA_x", true));
            SetName( "ImageReduce_Sum_2D_to_1DA_x" + " " + iWidth + " " + iHeight );
        }
        else if ( iType == SUM_YA )
        {
            m_kPShader.set(0, new PixelShader("ImageReduce_Sum_2D_to_1DA_y", true));
            SetName( "ImageReduce_Sum_2D_to_1DA_y" + " " + iWidth + " " + iHeight );
        }
        else if ( iHeight > 1 )
        {
            if ( iType == SUM )
            {
                m_kPShader.set(0, new PixelShader("ImageReduce_Sum_2DP", true));
                SetName( "ImageReduce_Sum_2DP" + " " + iWidth + " " + iHeight );
            }
            else
            {
                m_kPShader.set(0, new PixelShader("ImageReduce_Entropy_2DP", true));
                SetName( "ImageReduce_Entropy_2DP" + " " + iWidth + " " + iHeight );
            }
        }
        else
        {
            if ( iType == SUM )
            {
                m_kPShader.set(0, new PixelShader("ImageReduce_Sum_1DP", true));
                SetName( "ImageReduce_Sum_1DP" + " " + iWidth + " " + iHeight );
            }
            else
            {
                m_kPShader.set(0, new PixelShader("ImageReduce_Entropy_1DP", true));
                SetName( "ImageReduce_Entropy_1DP" + " " + iWidth + " " + iHeight );
            }
        }

        m_kPShader.get(0).SetTextureQuantity(1);
        m_kPShader.get(0).SetTexture(0,kTarget);
        m_kPShader.get(0).SetImageName(0,kTarget.GetName());
        if ( kTargetB != null )
        {
            //m_kPShader.get(0).SetTexture(1,kTargetB);
            //m_kPShader.get(0).SetImageName(1,kTargetB.GetName());
        }
        
        m_iWidth = iWidth;
        m_iHeight = iHeight;      
        m_dNumSamples = dNumSamples;
    }
    



    public ImageReduceEffect ( Texture kTexA, Texture kTexB, double dNumSamples )
    {
        super(1);
        m_dNumSamples = dNumSamples;
        
        VertexShader kVShader = new VertexShader("EntropyV", true);
        PixelShader kPShader = new PixelShader("EntropyP", true);
        SetVShader(0,kVShader);
        SetPShader(0,kPShader);
        
        kPShader.SetTextureQuantity(1);
        kPShader.SetTexture( 0, kTexA );
        kPShader.SetImageName( 0, kTexA.GetName() );
        /*
        kPShader.SetTextureQuantity(2);
        kPShader.SetTexture( 0, kTexA );
        kPShader.SetImageName( 0, kTexA.GetName() );
        kPShader.SetTexture( 1, kTexB );
        kPShader.SetImageName( 1, kTexB.GetName() );
        */
    }
    
    public void OnLoadPrograms (int iPass, Program pkVProgram,
            Program pkPProgram, Program pkCProgram)
    {
        if ( pkCProgram.GetUC("Step") != null ) 
        {
            pkCProgram.GetUC("Step").GetData()[0] = (1.0f/(float)(m_iWidth*2.0));
            pkCProgram.GetUC("Step").GetData()[1] = (1.0f/(float)(m_iHeight*2.0));

            //System.err.println( GetName() +  " OnLoad " + pkCProgram.GetUC("Step").GetData()[0] + " " +
            //        pkCProgram.GetUC("Step").GetData()[1] );
        }
        if ( pkCProgram.GetUC("dLogN") != null ) 
        {
            double dLogN = Math.log(m_dNumSamples);
            pkCProgram.GetUC("dLogN").GetData()[0] = (float)dLogN;
            //System.err.println( GetName() +  " dLogN = " + pkCProgram.GetUC("dLogN").GetData()[0]);
       }
        if ( pkCProgram.GetUC("nVoxels") != null ) 
        {
            pkCProgram.GetUC("nVoxels").GetData()[0] = (float)m_dNumSamples;
            //System.err.println( "dLogN = " + pkProgram.GetUC("dLogN").GetData()[0]);
       }
    }

}
