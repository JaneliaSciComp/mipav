
package gov.nih.mipav.view.renderer.WildMagic.Render;

public class LevWidgetState
{
    public static LevWidgetState ZERO_STATE = new LevWidgetState();
    public float[] Color = new float[] {1,1,1,1};
    public float[] MidLine = new float[4];
    public float[] LeftLine = new float[4];
    public float[] RightLine = new float[4];
    public float[]  BoundaryEmphasis = new float[4];

    public LevWidgetState ()
    {
        for ( int i = 0; i < 4; i++ )
        {
            MidLine[i] = 0f;
            LeftLine[i] = 0f;
            RightLine[i] = 0f;
        } 
        BoundaryEmphasis[0] = 1.0f;
    }
    
    public void Copy( LevWidgetState kIn )
    {
        for ( int i = 0; i < 4; i++ )
        {
            Color[i] = kIn.Color[i];
            MidLine[i] = kIn.MidLine[i];
            LeftLine[i] = kIn.LeftLine[i];
            RightLine[i] = kIn.RightLine[i];
        }
        BoundaryEmphasis[0] = kIn.BoundaryEmphasis[0];
    }
    
    public void dispose()
    {
        Color = null;
        MidLine = null;
        LeftLine = null;
        RightLine = null;
    }
}
