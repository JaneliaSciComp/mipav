package gov.nih.mipav.view.renderer.WildMagic.Render;


import WildMagic.LibGraphics.Shaders.*;
import WildMagic.LibGraphics.Effects.*;

public abstract class VolumeClipEffect extends ShaderEffect
{
    /** Axis-aligned clip plane shader paramter names: */
    private final static String[] m_akClip =
        new String[]{ "clipXInv", "clipX", "clipYInv", "clipY", "clipZInv", "clipZ" };

    public void dispose()
    {
        m_aafClipData = null;
        m_afClipEyeData = null;
        m_afClipEyeInvData = null;
        m_afClipArbData = null;
    }

    /**
     * Init the axis-aligned clip planes.
     * @param afData, the axis-aligned clip plane default positions.
     */
    public void InitClip( float[] afData )
    {
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("DoClip") != null ) 
        {
            pkProgram.GetUC("DoClip").SetDataSource(new float[]{0,0,0,0});
        }       

        for ( int i = 0; i < 6; i++ )
        {
            if ( pkProgram.GetUC(m_akClip[i]) != null ) 
            {
                pkProgram.GetUC(m_akClip[i]).SetDataSource(new float[]{afData[i],0,0,0});
            }       
            m_aafClipData[i][0] = afData[i];
        }
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
                SetClip( i, m_aafClipData[i] );
            }
        }
        if ( m_afClipEyeData != null )
        {
            SetClipEye(m_afClipEyeData);
        }
        if ( m_afClipEyeInvData != null )
        {
            SetClipEyeInv(m_afClipEyeInvData);
        }
        if ( m_afClipArbData != null )
        {
            SetClipArb(m_afClipArbData);
        }
    }

    /**
     * Enable and set the axis-aligned clip plane.
     * @param iWhich, one of 6 clip-planes to set.
     * @param data, the distance to the clip-plane.
     */
    public void SetClip(int iWhich, float[] data)
    {
        m_aafClipData[iWhich] = data;
        EnableClip();
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC(m_akClip[iWhich]) != null ) 
        {
            pkProgram.GetUC(m_akClip[iWhich]).SetDataSource(data);
        }
    }
    /**
     * Enable clipping.
     */
    private void EnableClip()
    {
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("DoClip") != null ) 
        {
            pkProgram.GetUC("DoClip").SetDataSource(new float[]{1,0,0,0});
        }       
    }

    /**
     * Enable and set the eye clip plane.
     * @param afEquation, the clip-plane equation.
     */
    public void SetClipEye(float[] afEquation)
    {
        m_afClipEyeData = afEquation;
        EnableClip();
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("clipEye") != null ) 
        {
            pkProgram.GetUC("clipEye").SetDataSource(afEquation);
        }
    }

    /**
     * Enable and set the inverse-eye clip plane.
     * @param afEquation, the clip-plane equation.
     */
    public void SetClipEyeInv(float[] afEquation)
    {
        m_afClipEyeInvData = afEquation;
        EnableClip();
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("clipEyeInv") != null ) 
        {
            pkProgram.GetUC("clipEyeInv").SetDataSource(afEquation);
        }
    }

    /**
     * Enable and set the arbitrary clip plane.
     * @param afEquation, the clip-plane equation.
     */
    public void SetClipArb(float[] afEquation)
    {
        m_afClipArbData = afEquation;
        EnableClip();
        Program pkProgram = GetPProgram(0);
        if ( pkProgram.GetUC("clipArb") != null ) 
        {
            pkProgram.GetUC("clipArb").SetDataSource(afEquation);
        }
    }

    /** stores the axis-aligned clip plane information: */
    private float[][] m_aafClipData = new float[6][4];
    /** stores the eye clip plane information: */
    private float[] m_afClipEyeData = null;
    /** stores the inverse-eye clip plane information: */
    private float[] m_afClipEyeInvData = null;
    /** stores the arbitrary clip plane information: */
    private float[] m_afClipArbData = null;

}
