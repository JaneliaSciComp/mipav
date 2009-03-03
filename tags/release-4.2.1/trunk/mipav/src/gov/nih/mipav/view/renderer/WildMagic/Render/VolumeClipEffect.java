package gov.nih.mipav.view.renderer.WildMagic.Render;


import WildMagic.LibGraphics.Effects.ShaderEffect;
import WildMagic.LibGraphics.Shaders.Program;

/**
 * Clipping effect for Volume Objects.
 */
public abstract class VolumeClipEffect extends ShaderEffect
{
    /** Axis-aligned clip plane shader parameter names: */
    private final static String[] m_akClip =
        new String[]{ "clipXInv", "clipX", "clipYInv", "clipY", "clipZInv", "clipZ" };

    /** Turn clipping on/off. */
    protected float[] m_afDoClip = { 0, 0, 0, 0 };

    /** Turn clipping on/off per-axis aligned clip plane, eye-clip planes and arbitrary plane. */
    protected boolean[] m_afClipAll = { false, false, false, false, false, false, false, false, false };
    
    /** stores the axis-aligned clip plane information: */
    protected float[][] m_aafClipData =  { { 0, 0, 0, 0 },
                                                          { 1, 1, 0, 0 },
                                                          { 0, 0, 0, 0 },
                                                          { 1, 1, 0, 0 },
                                                          { 0, 0, 0, 0 },
                                                          { 1, 1, 0, 0 } };

    /** stores the eye clip plane information: */
    protected float[] m_afClipEyeData = {0f,0f,0f,0f};


    /** stores the inverse-eye clip plane information: */
    protected float[] m_afClipEyeInvData = {0f,0f,0f,0f};
    
    /** stores the arbitrary clip plane information: */
    protected float[] m_afClipArbData = {0f,0f,0f,0f};


    /* (non-Javadoc)
     * @see WildMagic.LibGraphics.Effects.ShaderEffect#OnLoadPrograms(int, WildMagic.LibGraphics.Shaders.Program, WildMagic.LibGraphics.Shaders.Program)
     */
    public void OnLoadPrograms (int iPass, Program pkVProgram, Program pkPProgram)
    {
        for ( int i = 0; i < 6; i++ )
        {       
            if ( pkPProgram.GetUC(m_akClip[i]) != null ) 
            {
                pkPProgram.GetUC(m_akClip[i]).SetDataSource(m_aafClipData[i]);
            }
        }
        if ( pkPProgram.GetUC("DoClip") != null ) 
        {
            pkPProgram.GetUC("DoClip").SetDataSource(m_afDoClip);
        }       
        if ( pkPProgram.GetUC("clipArb") != null ) 
        {
            pkPProgram.GetUC("clipArb").SetDataSource(m_afClipArbData);
        }
        if ( pkPProgram.GetUC("clipEye") != null ) 
        {
            pkPProgram.GetUC("clipEye").SetDataSource(m_afClipEyeData);
        }
        if ( pkPProgram.GetUC("clipEyeInv") != null ) 
        {
            pkPProgram.GetUC("clipEyeInv").SetDataSource(m_afClipEyeInvData);
        }
        super.OnLoadPrograms( iPass, pkVProgram, pkPProgram );
    }


    /* (non-Javadoc)
     * @see WildMagic.LibGraphics.Effects.ShaderEffect#dispose()
     */
    public void dispose()
    {
        m_afDoClip = null;
        m_afClipAll = null;
        m_aafClipData = null;
        m_afClipEyeData = null;
        m_afClipEyeInvData = null;
        m_afClipArbData = null;
        super.dispose();
    }

    /**
     * Reset the axis-aligned clip planes, eye, inverse-eye and arbitrary clip
     * planes to neutral.
     */
    public void ResetClip()
    {
        for ( int i = 0; i < 6; i++ )
        {
            if ( m_aafClipData[i] != null )
            {
                SetClip( i, m_aafClipData[i][0], m_afClipAll[i] );
            }
        }
        if ( m_afClipEyeData != null )
        {
            SetClipEye(m_afClipEyeData, m_afClipAll[6]);
        }
        if ( m_afClipEyeInvData != null )
        {
            SetClipEyeInv(m_afClipEyeInvData, m_afClipAll[7]);
        }
        if ( m_afClipArbData != null )
        {
            SetClipArb(m_afClipArbData, m_afClipAll[8]);
        }
    }
    /**
     * Enable and set the axis-aligned clip plane.
     * @param iWhich one of 6 clip-planes to set.
     * @param data the distance to the clip-plane.
     */
    public void SetClip(int iWhich, float data, boolean bEnable)
    {
        m_afClipAll[iWhich] = bEnable;
        m_aafClipData[iWhich][0] = data;
        EnableClip();
    }
    /**
     * Enable and set the arbitrary clip plane.
     * @param afEquation the clip-plane equation.
     */
    public void SetClipArb(float[] afEquation, boolean bEnable)
    {
        m_afClipAll[8] = bEnable;
        for ( int i = 0; i < 4; i++ )
        {
            m_afClipArbData[i] = afEquation[i];
        }
        EnableClip();
    }
    /**
     * Enable and set the eye clip plane.
     * @param afEquation the clip-plane equation.
     */
    public void SetClipEye(float[] afEquation, boolean bEnable)
    {
        m_afClipAll[6] = bEnable;
        for ( int i = 0; i < 4; i++ )
        {
            m_afClipEyeData[i] = afEquation[i];
        }
        EnableClip();
    }
    /**
     * Enable and set the inverse-eye clip plane.
     * @param afEquation the clip-plane equation.
     */
    public void SetClipEyeInv(float[] afEquation, boolean bEnable)
    {
        m_afClipAll[7] = bEnable;
        for ( int i = 0; i < 4; i++ )
        {
            m_afClipEyeInvData[i] = afEquation[i];
        }
        EnableClip();
    }
    /**
     * Enable clipping.
     */
    private void EnableClip()
    {
        boolean bEnable = false;
        for ( int i = 0; i < m_afClipAll.length; i++ )
        {
            bEnable |= m_afClipAll[i];
        }
        m_afDoClip[0] = (bEnable) ? 1 : 0;
    }

}
