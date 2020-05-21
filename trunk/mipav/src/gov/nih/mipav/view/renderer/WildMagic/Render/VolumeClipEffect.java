package gov.nih.mipav.view.renderer.WildMagic.Render;


import WildMagic.LibFoundation.Mathematics.Ellipsoid3f;
import WildMagic.LibFoundation.Mathematics.Matrix4f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Mathematics.Vector4f;
import WildMagic.LibGraphics.Effects.ShaderEffect;
import WildMagic.LibGraphics.Shaders.Program;

/**
 * Clipping effect for Volume Objects.
 */
public abstract class VolumeClipEffect extends ShaderEffect
{
    /**  */
    private static final long serialVersionUID = -833857381096103948L;
    public static final int CLIP_X = 0;
    public static final int CLIP_X_INV = 1;
    public static final int CLIP_Y = 2;
    public static final int CLIP_Y_INV = 3;
    public static final int CLIP_Z = 4;
    public static final int CLIP_Z_INV = 5;
    public static final int CLIP_EYE = 6;
    public static final int CLIP_EYE_INV = 7;
    public static final int CLIP_A = 8;
    public static final int CLIP_A_INV = 9;
    public static final int MAX_CLIP_PLANES = 10;
    
    /** Axis-aligned clip plane shader parameter names: */
    private final static String[] m_akClip =
        new String[]{ "clipXInv", "clipX", "clipYInv", "clipY", "clipZInv", "clipZ" };

    /** Turn clipping on/off. */
    protected float[] m_afDoClip = { 0, 0, 0, 0 };

    /** Turn clipping on/off per-axis aligned clip plane, eye-clip planes and arbitrary plane. */
    protected boolean[] m_afClipAll = { false, false, false, false, false, false, false, false, false, false };
    
    /** stores t=clip plane information: */
    protected float[][] m_aafClipData =  { { 0, 0, 0, 0 },
                                                          { 1, 1, 0, 0 },
                                                          { 0, 0, 0, 0 },
                                                          { 1, 1, 0, 0 },
                                                          { 0, 0, 0, 0 },
                                                          { 1, 1, 0, 0 },
                                                          { 0, 0, 1, 0 },
                                                          { 0, 0, 1, 1 },
                                                          { 0, 0, 0, 0 },
                                                          { 0, 0, 0, 0 }
                                                          };


    protected float[] m_afVolumeMatrix = { 1f, 0f, 0f, 0f,  0f, 1f, 0f, 0f,  0f, 0f, 1f, 0f,  0f, 0f, 0f, 1f };

    protected boolean m_bClipSphere = false;
    protected float[] m_afClipSphereCenter = {0,0,0,0};
    protected float[] m_afClipSphereScale = {0,0,0,0};
    protected float[] m_afClipOBBAxis0 = {0,0,0,0};
    protected float[] m_afClipOBBAxis1 = {0,0,0,0};
    protected float[] m_afClipOBBAxis2 = {0,0,0,0};
    protected float[] m_afClipOBBExtent = {0,0,0,0};

    protected boolean m_bClipOBB = false;

    /* (non-Javadoc)
     * @see WildMagic.LibGraphics.Effects.ShaderEffect#dispose()
     */
    public void dispose()
    {
        m_afDoClip = null;
        m_afClipAll = null;
        m_aafClipData = null;
        super.dispose();
    }


    /* (non-Javadoc)
     * @see WildMagic.LibGraphics.Effects.ShaderEffect#OnLoadPrograms(int, WildMagic.LibGraphics.Shaders.Program, WildMagic.LibGraphics.Shaders.Program)
     */
    public void OnLoadPrograms (int iPass, Program pkVProgram, Program pkPProgram, Program pkCProgram)
    {
        for ( int i = 0; i < 6; i++ )
        {       
            if ( pkCProgram.GetUC(m_akClip[i]) != null ) 
            {
                pkCProgram.GetUC(m_akClip[i]).SetDataSource(m_aafClipData[i]);
            }
        }
        if ( pkCProgram.GetUC("DoClip") != null ) 
        {
            pkCProgram.GetUC("DoClip").SetDataSource(m_afDoClip);
        }       
        if ( pkCProgram.GetUC("clipArb") != null ) 
        {
            pkCProgram.GetUC("clipArb").SetDataSource(m_aafClipData[CLIP_A]);
        }       
        if ( pkCProgram.GetUC("clipArbInv") != null ) 
        {
            pkCProgram.GetUC("clipArbInv").SetDataSource(m_aafClipData[CLIP_A_INV]);
        }
        if ( pkCProgram.GetUC("clipEye") != null ) 
        {
            pkCProgram.GetUC("clipEye").SetDataSource(m_aafClipData[CLIP_EYE]);
        }
        if ( pkCProgram.GetUC("clipEyeInv") != null ) 
        {
            pkCProgram.GetUC("clipEyeInv").SetDataSource(m_aafClipData[CLIP_EYE_INV]);
        }
        if ( pkCProgram.GetUC("volumeMatrix") != null ) 
        {
            pkCProgram.GetUC("volumeMatrix").SetDataSource(m_afVolumeMatrix);
        }
        if ( pkCProgram.GetUC("sphereCenter") != null ) 
        {
            pkCProgram.GetUC("sphereCenter").SetDataSource(m_afClipSphereCenter);
        }
        if ( pkCProgram.GetUC("sphereScale") != null ) 
        {
            pkCProgram.GetUC("sphereScale").SetDataSource(m_afClipSphereScale);
        }
        if ( pkCProgram.GetUC("obbClipAxis0") != null ) 
        {
            pkCProgram.GetUC("obbClipAxis0").SetDataSource(m_afClipOBBAxis0);
        }
        if ( pkCProgram.GetUC("obbClipAxis1") != null ) 
        {
            pkCProgram.GetUC("obbClipAxis1").SetDataSource(m_afClipOBBAxis1);
        }
        if ( pkCProgram.GetUC("obbClipAxis2") != null ) 
        {
            pkCProgram.GetUC("obbClipAxis2").SetDataSource(m_afClipOBBAxis2);
        }
        if ( pkCProgram.GetUC("obbClipExtent") != null ) 
        {
            pkCProgram.GetUC("obbClipExtent").SetDataSource(m_afClipOBBExtent);
        }
        super.OnLoadPrograms( iPass, pkVProgram, pkPProgram, pkCProgram );
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
        if ( m_aafClipData[CLIP_EYE] != null )
        {
            SetClipEye(m_aafClipData[CLIP_EYE], m_afClipAll[6]);
        }
        if ( m_aafClipData[CLIP_EYE_INV] != null )
        {
            SetClipEyeInv(m_aafClipData[CLIP_EYE_INV], m_afClipAll[7]);
        }
        if ( m_aafClipData[CLIP_A] != null )
        {
            SetClipArb(m_aafClipData[CLIP_A], m_afClipAll[8]);
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
        m_afClipAll[CLIP_A] = bEnable;
        for ( int i = 0; i < 4; i++ )
        {
            m_aafClipData[CLIP_A][i] = afEquation[i];
        }
        EnableClip();
    }
    /**
     * Enable and set the arbitrary clip plane.
     * @param afEquation the clip-plane equation.
     */
    public void SetClipArbInv(float[] afEquation, boolean bEnable)
    {
        m_afClipAll[CLIP_A_INV] = bEnable;
        for ( int i = 0; i < 4; i++ )
        {
            m_aafClipData[CLIP_A_INV][i] = afEquation[i];
        }
        EnableClip();
    }
    /**
     * Enable and set the eye clip plane.
     * @param afEquation the clip-plane equation.
     */
    public void SetClipEye(float[] afEquation, boolean bEnable)
    {
        m_afClipAll[CLIP_EYE] = bEnable;
        for ( int i = 0; i < 4; i++ )
        {
            m_aafClipData[CLIP_EYE][i] = afEquation[i];
        }
        EnableClip();
    }
    /**
     * Enable and set the inverse-eye clip plane.
     * @param afEquation the clip-plane equation.
     */
    public void SetClipEyeInv(float[] afEquation, boolean bEnable)
    {
        m_afClipAll[CLIP_EYE_INV] = bEnable;
        for ( int i = 0; i < 4; i++ )
        {
            m_aafClipData[CLIP_EYE_INV][i] = afEquation[i];
        }
        EnableClip();
    }
    
    public void SetClipSphere( Vector3f center, Vector3f scale, float radius, boolean bEnable ) {
    	if ( center == null || !bEnable )
    	{
    		m_bClipSphere = false;
            m_afDoClip[0] = (bEnable) ? 1 : m_afDoClip[0];
            return;
    	}
    	
    	m_bClipSphere = true;
    	m_afClipSphereCenter[0] = center.X;    	m_afClipSphereCenter[1] = center.Y;    	m_afClipSphereCenter[2] = center.Z;    	m_afClipSphereCenter[3] = (radius*radius);
    	m_afClipSphereScale[0] = scale.X;    	m_afClipSphereScale[1] = scale.Y;    	m_afClipSphereScale[2] = scale.Z;

        m_afDoClip[0] = (bEnable) ? 1 : m_afDoClip[0];
    }
    
    public void SetClipOBB( Vector3f center, Vector3f scale, Vector3f[] axes, float[] extents, boolean bEnable )
    {
    	if ( !bEnable ) {
    		m_bClipOBB = false;
            m_afDoClip[0] = (bEnable) ? 1 : m_afDoClip[0];
    		return;
    	}
    	
		m_bClipOBB = bEnable;
       	m_afClipSphereCenter[0] = center.X;    	m_afClipSphereCenter[1] = center.Y;    	m_afClipSphereCenter[2] = center.Z;
    	m_afClipSphereScale[0] = scale.X;    	m_afClipSphereScale[1] = scale.Y;    	m_afClipSphereScale[2] = scale.Z;


    	m_afClipOBBAxis0[0] = axes[0].X;    	m_afClipOBBAxis0[1] = axes[0].Y;    	m_afClipOBBAxis0[2] = axes[0].Z;
    	m_afClipOBBAxis1[0] = axes[1].X;    	m_afClipOBBAxis1[1] = axes[1].Y;    	m_afClipOBBAxis1[2] = axes[1].Z;
    	m_afClipOBBAxis2[0] = axes[2].X;    	m_afClipOBBAxis2[1] = axes[2].Y;    	m_afClipOBBAxis2[2] = axes[2].Z;
    	
    	m_afClipOBBExtent[0] = extents[0];    	m_afClipOBBExtent[1] = extents[1];    	m_afClipOBBExtent[2] = extents[2];    	

        m_afDoClip[0] = (bEnable) ? 1 : m_afDoClip[0];
    }
        
    public void setVolumeMatrix( float[] volumeMatrix )
    {
    	m_afVolumeMatrix = volumeMatrix;
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
    
    public boolean isClipAE()
    {
    	return (m_afClipAll[8] | m_afClipAll[7] | m_afClipAll[6]);
    }    
    
    public boolean isClipSphere()
    {
    	return m_bClipSphere;
    } 
    
    public boolean isClipOBB()
    {
    	return m_bClipOBB;
    }
    
    public Vector3f getClip()
    {
    	return new Vector3f( m_aafClipData[CLIP_X][0], m_aafClipData[CLIP_Y][0], m_aafClipData[CLIP_Z][0] );
    }
    
    public Vector4f getClipEye()
    {
    	return new Vector4f( m_aafClipData[CLIP_EYE][0], m_aafClipData[CLIP_EYE][1], m_aafClipData[CLIP_EYE][2], m_aafClipData[CLIP_EYE][3] );
    }
    
    public Vector4f getClipEyeInv()
    {
    	return new Vector4f( m_aafClipData[CLIP_EYE_INV][0], m_aafClipData[CLIP_EYE_INV][1], m_aafClipData[CLIP_EYE_INV][2], m_aafClipData[CLIP_EYE_INV][3] );
    }
    
    public Vector4f getClipArb()
    {
    	return new Vector4f( m_aafClipData[CLIP_A][0], m_aafClipData[CLIP_A][1], m_aafClipData[CLIP_A][2], m_aafClipData[CLIP_A][3] );
    }    
    
    public Vector4f getClipArbInv()
    {
    	return new Vector4f( m_aafClipData[CLIP_A_INV][0], m_aafClipData[CLIP_A_INV][1], m_aafClipData[CLIP_A_INV][2], m_aafClipData[CLIP_A_INV][3] );
    }
    
    public Vector3f getClipInv()
    {
    	return new Vector3f( m_aafClipData[CLIP_X_INV][0], 
    			m_aafClipData[CLIP_Y_INV][0], m_aafClipData[CLIP_Z_INV][0] );
    }
    
    public boolean isClip()
    {
        boolean bEnable = false;
        for ( int i = 0; i < 6; i++ )
        {
            bEnable |= m_afClipAll[i];
        }
        return bEnable;
    }
}
