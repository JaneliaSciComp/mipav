package gov.nih.mipav.view.renderer.WildMagic.Render;
import WildMagic.LibGraphics.Effects.*;
import WildMagic.LibGraphics.ObjectSystem.*;
import WildMagic.LibGraphics.Shaders.*;
import WildMagic.LibGraphics.Rendering.*;

public class ImageReduceEffect extends ShaderEffect
    implements StreamInterface
{
    private double m_dNumSamples;
    

    public ImageReduceEffect ( Texture kTexA, double dNumSamples )
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
    }
    
    public void OnLoadPrograms (int iPass, Program pkVProgram,
            Program pkPProgram, Program pkCProgram)
    {
        if ( pkCProgram.GetUC("dLogN") != null ) 
        {
            double dLogN = Math.log(m_dNumSamples);
            pkCProgram.GetUC("dLogN").GetData()[0] = (float)dLogN;
            //System.err.println( GetName() +  " dLogN = " + pkCProgram.GetUC("dLogN").GetData()[0]);
       }
    }

}
