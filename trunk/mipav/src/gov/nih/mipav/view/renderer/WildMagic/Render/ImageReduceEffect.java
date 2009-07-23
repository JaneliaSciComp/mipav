package gov.nih.mipav.view.renderer.WildMagic.Render;
import WildMagic.LibGraphics.Effects.*;
import WildMagic.LibGraphics.ObjectSystem.*;
import WildMagic.LibGraphics.Shaders.*;
import WildMagic.LibGraphics.Rendering.*;

public class ImageReduceEffect extends ShaderEffect
    implements StreamInterface
{

    public static final int SUM = 0;
    public static final int ENTROPY = 1;
    
    private int m_iWidth;
    private int m_iHeight;
    private double m_dNumSamples;
    
    /** Creates a new TextureEffect with the texture specified.
     * @param rkBaseName the name of the texture image.
     */
    public ImageReduceEffect (Texture kTarget, int iWidth, int iHeight, double dNumSamples, int iType)
    {
        super(1);
        m_kVShader.set(0, new VertexShader("TextureV", true));
        if ( iHeight > 1 )
        {
            if ( iType == SUM )
            {
                m_kPShader.set(0, new PixelShader("ImageReduce_Sum_2DP", true));
            }
            else
            {
                m_kPShader.set(0, new PixelShader("ImageReduce_Entropy_2DP", true));
            }
        }
        else
        {
            if ( iType == SUM )
            {
                m_kPShader.set(0, new PixelShader("ImageReduce_Sum_1DP", true));
            }
            else
            {
                m_kPShader.set(0, new PixelShader("ImageReduce_Entropy_1DP", true));
            }
        }

        m_kPShader.get(0).SetTextureQuantity(1);
        m_kPShader.get(0).SetTexture(0,kTarget);
        m_kPShader.get(0).SetImageName(0,kTarget.GetName());
        
        m_iWidth = iWidth;
        m_iHeight = iHeight;      
        m_dNumSamples = dNumSamples;
    }
    
    public void OnLoadPrograms (int iPass, Program pkVProgram,
            Program pkPProgram, Program pkCProgram)
    {
       if ( pkCProgram.GetUC("Step") != null ) 
        {
           pkCProgram.GetUC("Step").GetData()[0] = (1.0f/(float)(m_iWidth*2.0));
           pkCProgram.GetUC("Step").GetData()[1] = (1.0f/(float)(m_iHeight*2.0));
            
/*
            pkProgram.GetUC("Step").GetData()[0] = fScale * (1.0f/(float)(m_iWidth*2.0));
            pkProgram.GetUC("Step").GetData()[1] = fScale * (1.0f/(float)(m_iHeight*2.0));
            System.err.println( "OnLoad " + pkProgram.GetUC("Step").GetData()[0] + " " +
                    pkProgram.GetUC("Step").GetData()[1] );
*/
        }
        if ( pkCProgram.GetUC("dLogN") != null ) 
        {
            double dLogN = Math.log(m_dNumSamples);
            pkCProgram.GetUC("dLogN").GetData()[0] = (float)dLogN;
            //System.err.println( "dLogN = " + pkProgram.GetUC("dLogN").GetData()[0]);
       }
        if ( pkCProgram.GetUC("nVoxels") != null ) 
        {
            pkCProgram.GetUC("nVoxels").GetData()[0] = (float)m_dNumSamples;
            //System.err.println( "dLogN = " + pkProgram.GetUC("dLogN").GetData()[0]);
       }
    }

}
