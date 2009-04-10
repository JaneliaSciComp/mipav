
package gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer;

public class ClassificationWidgetState
{
    public static ClassificationWidgetState ZERO_STATE = new ClassificationWidgetState();
    public float[] Color = new float[] {1,1,1,1};
    public float[] MidLine = new float[4];
    public float[] LeftLine = new float[4];
    public float[] RightLine = new float[4];
    public float[] BoundaryEmphasis = new float[4];
    public float[] UseWidget = new float[4];

    public ClassificationWidgetState ()
    {
        for ( int i = 0; i < 4; i++ )
        {
            MidLine[i] = 0f;
            LeftLine[i] = 0f;
            RightLine[i] = 0f;
        } 
        BoundaryEmphasis[0] = 1.0f;
        UseWidget[0] = 0.0f;
    }
    
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
    
    public void dispose()
    {
        Color = null;
        MidLine = null;
        LeftLine = null;
        RightLine = null;
        BoundaryEmphasis = null;
        UseWidget = null;
    }
}
