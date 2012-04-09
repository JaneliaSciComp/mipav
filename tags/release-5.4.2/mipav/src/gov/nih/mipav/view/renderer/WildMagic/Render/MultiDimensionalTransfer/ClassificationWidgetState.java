
package gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer;

import gov.nih.mipav.model.structures.ModelLUT;

import java.io.Serializable;

import WildMagic.LibGraphics.Rendering.Texture;

/**
 * This class stores the information needed for calculating how the multi-histogram is applied to the volume
 * to create the rendered image. It encapsulates all the parameters that can be set for the widget and is used to 
 * pass those parameters to the GLSL shader program that renders the volume.
 *
 * Each widget used in the multi-histogram tool contains it's own ClassificationWidgetState which is passed to
 * the GLSL shader program. The number of widgets is limited to VolumeImageMultiDimensionalTransfer.MAX_WIDGETS.
 */
public class ClassificationWidgetState implements Serializable
{
    /**  */
    private static final long serialVersionUID = 711728604033191355L;

    public static int m_iTextureID = 0;
    
    /** Default, unused state. */
    public static ClassificationWidgetState ZERO_STATE = new ClassificationWidgetState();
    /** Default color for the color transfer function is white: */
    public float[] Color = new float[] {1,1,1,.5f};
    /** Widget Center: */
    public float[] Center = new float[4];
    /** Transfer function mid-line: */
    public float[] MidLine = new float[4];
    /** Transfer function left line: */
    public float[] LeftLine = new float[4];
    /** Transfer function right line: */
    public float[] RightLine = new float[4];
    /** Transfer function shift: */
    public float[] Shift = new float[4];
    /** Transfer function y-ratios: */
    public float[] YRatio = new float[4];
    /** Transfer function radius: */
    public float[] Radius = new float[4];
    /** contribution of the boundary emphasis slider and therefor the contribution of the 2nd derivative of the data.*/
    public float[] BoundaryEmphasis = new float[4];
    /** turns the widget on/off in the GLSL shader code. */
    public float[] UseWidget = new float[4];
    
    /** turns the widget color map on/off in the GLSL shader code. */
    public float[] UseColorMap = new float[4];
    public boolean InvertLUT = false;

    /**
     * Default Constructor:
     */
    public ClassificationWidgetState ()
    {
        for ( int i = 0; i < 4; i++ )
        {
            Center[i] = 0f;
            MidLine[i] = 0f;
            LeftLine[i] = 0f;
            RightLine[i] = 0f;
            Shift[i] = 0f;
            YRatio[i] = 0f;
            BoundaryEmphasis[i] = 0f;
            UseWidget[i] = 0;
            UseColorMap[i] = 0f;
            BoundaryEmphasis[i] = 0.0f;
            UseWidget[i] = 0.0f;
            UseColorMap[i] = -1.0f;
            Radius[i] = -1.0f;
        } 
    }
    
    /**
     * Copies the input ClassificationWidgetState:
     * @param kIn ClassificationWidgetState to copy.
     */
    public void Copy( ClassificationWidgetState kIn )
    {
        for ( int i = 0; i < 4; i++ )
        {
            Color[i] = kIn.Color[i];
            Center[i] = kIn.Center[i];
            MidLine[i] = kIn.MidLine[i];
            LeftLine[i] = kIn.LeftLine[i];
            RightLine[i] = kIn.RightLine[i];
            Shift[i] = kIn.Shift[i];
            YRatio[i] = kIn.YRatio[i];
            Radius[i] = kIn.Radius[i];
            BoundaryEmphasis[i] = kIn.BoundaryEmphasis[i];
            UseWidget[i] = kIn.UseWidget[i];
            UseColorMap[i] = kIn.UseColorMap[i];
        }
        InvertLUT = kIn.InvertLUT;
    }
    
    /**
     * Dispose local memory.
     */
    public void dispose()
    {
        Center = null;
        Color = null;
        MidLine = null;
        LeftLine = null;
        RightLine = null;
        BoundaryEmphasis = null;
        UseWidget = null;
        UseColorMap = null;
        Radius = null;
        Shift = null;
        YRatio = null;
    }
    
    public boolean equals( ClassificationWidgetState kIn )
    {
        for ( int i = 0; i < 4; i++ )
        {
        	if ( (Color[i] != kIn.Color[i]) || 
        			(Center[i] != kIn.Center[i]) ||
        			(Radius[i] != kIn.Radius[i]) ||
        			(MidLine[i] != kIn.MidLine[i]) ||
        			(LeftLine[i] != kIn.LeftLine[i] ) ||
        			(RightLine[i] != kIn.RightLine[i]) ||
                    (Shift[i] != kIn.Shift[i]) ||
                    (YRatio[i] != kIn.YRatio[i])	||
                    (BoundaryEmphasis[i] != kIn.BoundaryEmphasis[i]) ||
                    (UseWidget[i] != kIn.UseWidget[i]) ||
                    (UseColorMap[i] != kIn.UseColorMap[i]) ||
                    (InvertLUT != kIn.InvertLUT) )
        		return false;
        }
        return true;
    }
}
