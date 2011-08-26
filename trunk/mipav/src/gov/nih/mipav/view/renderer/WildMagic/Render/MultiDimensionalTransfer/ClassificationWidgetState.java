
package gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer;

import java.io.Serializable;

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
    /** Default, unused state. */
    public static ClassificationWidgetState ZERO_STATE = new ClassificationWidgetState();
    /** Default color for the color transfer function is white: */
    public float[] Color = new float[] {1,1,1,1};
    /** Transfer function mid-line: */
    public float[] MidLine = new float[4];
    /** Transfer function left line: */
    public float[] LeftLine = new float[4];
    /** Transfer function right line: */
    public float[] RightLine = new float[4];
    /** contribution of the boundary emphasis slider and therefor the contribution of the 2nd derivative of the data.*/
    public float[] BoundaryEmphasis = new float[4];
    /** turns the widget on/off in the GLSL shader code. */
    public float[] UseWidget = new float[4];

    /**
     * Default Constructor:
     */
    public ClassificationWidgetState ()
    {
        for ( int i = 0; i < 4; i++ )
        {
            MidLine[i] = 0f;
            LeftLine[i] = 0f;
            RightLine[i] = 0f;
        } 
        BoundaryEmphasis[0] = 0.0f;
        UseWidget[0] = 0.0f;
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
            MidLine[i] = kIn.MidLine[i];
            LeftLine[i] = kIn.LeftLine[i];
            RightLine[i] = kIn.RightLine[i];
        }
        BoundaryEmphasis[0] = kIn.BoundaryEmphasis[0];
        UseWidget[0] = kIn.UseWidget[0];
    }
    
    /**
     * Dispose local memory.
     */
    public void dispose()
    {
        Color = null;
        MidLine = null;
        LeftLine = null;
        RightLine = null;
        BoundaryEmphasis = null;
        UseWidget = null;
    }
    
    public boolean equals( ClassificationWidgetState kIn )
    {
        for ( int i = 0; i < 4; i++ )
        {
        	if ( (Color[i] != kIn.Color[i]) || 
        			(MidLine[i] != kIn.MidLine[i]) ||
        			(LeftLine[i] != kIn.LeftLine[i] ) ||
        			(RightLine[i] != kIn.RightLine[i]) )
        		return false;
        }
        if ( BoundaryEmphasis[0] != kIn.BoundaryEmphasis[0])
        	return false;
        if ( UseWidget[0] != kIn.UseWidget[0])
        	return false;
        return true;
    }
}
