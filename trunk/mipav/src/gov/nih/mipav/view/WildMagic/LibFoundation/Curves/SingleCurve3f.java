package gov.nih.mipav.view.WildMagic.LibFoundation.Curves;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;


public abstract class SingleCurve3f extends Curve3f {
	/*
    public abstract float GetTime (float fLength, int iIterations = 32,
        float fTolerance = (float)1e-06) const;
    */
    // public abstract float GetTime (float fLength);
	
  //----------------------------------------------------------------------------
    public SingleCurve3f(float fTMin, float fTMax) 
    {
    	super(fTMin,fTMax);
    }
    
    //----------------------------------------------------------------------------
    protected static float GetSpeedWithData(float fTime, Curve3f pvData)
    {
        return ((Curve3f)pvData).GetSpeed(fTime);
    }
    
    //----------------------------------------------------------------------------
    public float GetLength(float fT0, float fT1)
    {
        assert(m_fTMin <= fT0 && fT0 <= m_fTMax);
        assert(m_fTMin <= fT1 && fT1 <= m_fTMax);
        assert(fT0 <= fT1);

        /*
        return Integrate1<Real>::RombergIntegral(8,fT0,fT1,GetSpeedWithData,
            this);
        */
        return 0;
    }
    
    //----------------------------------------------------------------------------
    public float GetTime(float fLength)
    {
    	int iIterations = 32;
    	float fTolerance = (float)1e-06;
        if (fLength <= (float)0.0)
        {
            return m_fTMin;
        }

        if (fLength >= GetTotalLength())
        {
            return m_fTMax;
        }

        // initial guess for Newton's method
        float fRatio = fLength/GetTotalLength();
        float fOmRatio = (float)1.0 - fRatio;
        float fTime = fOmRatio*m_fTMin + fRatio*m_fTMax;

        for (int i = 0; i < iIterations; i++)
        {
            float fDifference = GetLength(m_fTMin,fTime) - fLength;
            if (Math.abs(fDifference) < fTolerance)
            {
                return fTime;
            }

            fTime -= fDifference/GetSpeed(fTime);
        }

        // Newton's method failed.  You might want to increase iterations or
        // tolerance or integration accuracy.  However, in this application it
        // is likely that the time values are oscillating, due to the limited
        // numerical precision of 32-bit floats.  It is safe to use the last
        // computed time.
        return fTime;
    }
    
}