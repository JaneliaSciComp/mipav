package gov.nih.mipav.view.WildMagic.LibFoundation.Curves;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;


public abstract class Curve3f {
	// curve parameter is t where tmin <= t <= tmax
	protected float m_fTMin, m_fTMax;
    
    public abstract Vector3f GetPosition (float fTime);
    public abstract Vector3f GetFirstDerivative (float fTime);
    public abstract Vector3f GetSecondDerivative (float fTime);
    public abstract Vector3f GetThirdDerivative (float fTime);
    
    public abstract float GetLength (float fT0, float fT1);
    // inverse mapping of s = Length(t) given by t = Length^{-1}(s)
    // int iIterations = 32, float fTolerance = (float)1e-06
    // public abstract float GetTime (float fLength, int iIterations, float fTolerance);
    public abstract float GetTime (float fLength);
    
    //Vector3f pkP0 = null, Vector3f pkP1 = null
    public abstract float GetVariation (float fT0, float fT1, Vector3f pkP0, Vector3f pkP1);
    
    
  //----------------------------------------------------------------------------
    public Curve3f(float fTMin, float fTMax)
    {
        m_fTMin = fTMin;
        m_fTMax = fTMax;
    }
    
    //----------------------------------------------------------------------------
    public float GetMinTime ()
    {
        return m_fTMin;
    }
    
    //----------------------------------------------------------------------------
    public float GetMaxTime ()
    {
        return m_fTMax;
    }
    
    //----------------------------------------------------------------------------
    public void SetTimeInterval (float fTMin, float fTMax)
    {
        assert(fTMin < fTMax);
        m_fTMin = fTMin;
        m_fTMax = fTMax;
    }
    //----------------------------------------------------------------------------
    public float GetSpeed (float fTime)
    {
        Vector3f kVelocity = GetFirstDerivative(fTime);
        float fSpeed = kVelocity.Length();
        return fSpeed;
    }
    
    //----------------------------------------------------------------------------
    public float GetTotalLength ()
    {
        return GetLength(m_fTMin,m_fTMax);
    }
    
    //----------------------------------------------------------------------------
    public Vector3f GetTangent(float fTime)
    {
        Vector3f kVelocity = GetFirstDerivative(fTime);
        kVelocity.Normalize();
        return kVelocity;
    }
    
    //----------------------------------------------------------------------------
    public Vector3f GetNormal (float fTime)
    {
        Vector3f kVelocity = GetFirstDerivative(fTime);
        Vector3f kAcceleration = GetSecondDerivative(fTime);
        float fVDotV = kVelocity.Dot(kVelocity);
        float fVDotA = kVelocity.Dot(kAcceleration);
        // Vector3<Real> kNormal = fVDotV*kAcceleration - fVDotA*kVelocity;
        Vector3f kNormal = kAcceleration.scale(fVDotV).sub(kVelocity.scale(fVDotA));
        kNormal.Normalize();
        return kNormal;
    }
    
    //----------------------------------------------------------------------------
    public Vector3f GetBinormal (float fTime)
    {
        Vector3f kVelocity = GetFirstDerivative(fTime);
        Vector3f kAcceleration = GetSecondDerivative(fTime);
        float fVDotV = kVelocity.Dot(kVelocity);
        float fVDotA = kVelocity.Dot(kAcceleration);
        // Vector3<Real> kNormal = fVDotV*kAcceleration - fVDotA*kVelocity;
        Vector3f kNormal = kAcceleration.scale(fVDotV).sub(kVelocity.scale(fVDotA));
        kNormal.Normalize();
        kVelocity.Normalize();
        Vector3f kBinormal = kVelocity.Cross(kNormal);
        return kBinormal;
    }
    
  //----------------------------------------------------------------------------
   public void GetFrame (float fTime, Vector3f rkPosition,
        Vector3f rkTangent, Vector3f rkNormal,
        Vector3f rkBinormal)
    {
        rkPosition = GetPosition(fTime);
        Vector3f kVelocity = GetFirstDerivative(fTime);
        Vector3f kAcceleration = GetSecondDerivative(fTime);
        float fVDotV = kVelocity.Dot(kVelocity);
        float fVDotA = kVelocity.Dot(kAcceleration);
        // rkNormal = fVDotV*kAcceleration - fVDotA*kVelocity;
        rkNormal = kAcceleration.scale(fVDotV).sub(kVelocity.scale(fVDotA));
        rkNormal.Normalize();
        rkTangent = kVelocity;
        rkTangent.Normalize();
        rkBinormal = rkTangent.Cross(rkNormal);
    }
   
    //----------------------------------------------------------------------------
    public float GetCurvature(float fTime)
    {
        Vector3f kVelocity = GetFirstDerivative(fTime);
        float fSpeedSqr = kVelocity.SquaredLength();

        if (fSpeedSqr >= Mathf.ZERO_TOLERANCE)
        {
            Vector3f kAcceleration = GetSecondDerivative(fTime);
            Vector3f kCross = kVelocity.Cross(kAcceleration);
            float fNumer = kCross.Length();
            float fDenom = (float)Math.pow(fSpeedSqr,(float)1.5);
            return fNumer/fDenom;
        }
        else
        {
            // curvature is indeterminate, just return 0
            return (float)0.0;
        }
    }
 
    //----------------------------------------------------------------------------
    public float GetTorsion(float fTime)
    {
        Vector3f kVelocity = GetFirstDerivative(fTime);
        Vector3f kAcceleration = GetSecondDerivative(fTime);
        Vector3f kCross = kVelocity.Cross(kAcceleration);
        float fDenom = kCross.SquaredLength();

        if (fDenom >= Mathf.ZERO_TOLERANCE)
        {
            Vector3f kJerk = GetThirdDerivative(fTime);
            float fNumer = kCross.Dot(kJerk);
            return fNumer/fDenom;
        }
        else
        {
            // torsion is indeterminate, just return 0
            return (float)0.0;
        }
    }
    
    //----------------------------------------------------------------------------
    public void SubdivideByTime (int iNumPoints, Vector3f[] rakPoint)
    {
        assert( iNumPoints >= 2 );
        rakPoint = new Vector3f[iNumPoints];

        float fDelta = (m_fTMax - m_fTMin)/(iNumPoints-1);

        for (int i = 0; i < iNumPoints; i++)
        {
            float fTime = m_fTMin + fDelta*i;
            rakPoint[i] = GetPosition(fTime);
        }
    }
    
    //----------------------------------------------------------------------------
    public void SubdivideByLength (int iNumPoints, Vector3f[] rakPoint)
    {
        assert(iNumPoints >= 2);
        rakPoint = new Vector3f[iNumPoints];

        float fDelta = GetTotalLength()/(iNumPoints-1);

        for (int i = 0; i < iNumPoints; i++)
        {
            float fLength = fDelta*i;
           
            float fTime = GetTime(fLength);
            rakPoint[i] = GetPosition(fTime);
        }
    }    
    
  //----------------------------------------------------------------------------
    public void SubdivideByVariation (float fT0, Vector3f rkP0,
        float fT1, Vector3f rkP1, float fMinVariation, int iLevel,
        int riNumPoints, PointList rpkList) 
    {
        if (iLevel > 0 && GetVariation(fT0,fT1,rkP0,rkP1) > fMinVariation)
        {
            // too much variation, subdivide interval
            iLevel--;
            float fTMid = ((float)0.5)*(fT0+fT1);
            Vector3f kPMid = GetPosition(fTMid);

            SubdivideByVariation(fT0,rkP0,fTMid,kPMid,fMinVariation,iLevel,
                riNumPoints,rpkList);

            SubdivideByVariation(fTMid,kPMid,fT1,rkP1,fMinVariation,iLevel,
                riNumPoints,rpkList);
        }
        else
        {
            // add right end point, left end point was added by neighbor
            rpkList = new PointList(rkP1,rpkList);
            riNumPoints++;
        }
    }
    
    //----------------------------------------------------------------------------
    public void SubdivideByVariation (float fMinVariation, int iMaxLevel,
        int riNumPoints, Vector3f[] rakPoint)
    {
        // compute end points of curve
        Vector3f kPMin = GetPosition(m_fTMin);
        Vector3f kPMax = GetPosition(m_fTMax);

        // add left end point to list
        PointList pkList = new PointList(kPMin,null);
        riNumPoints = 1;

        // binary subdivision, leaf nodes add right end point of subinterval
        SubdivideByVariation(m_fTMin,kPMin,m_fTMax,kPMax,fMinVariation,
            iMaxLevel,riNumPoints,pkList.m_kNext);

        // repackage points in an array
        assert(riNumPoints >= 2);
        rakPoint = new Vector3f[riNumPoints];
        for (int i = 0; i < riNumPoints; i++)
        {
            assert(pkList != null);
            rakPoint[i] = pkList.m_kPoint;

            PointList pkSave = pkList;
            pkList = pkList.m_kNext;
            pkSave = null;
        }
        assert(pkList == null);
    }
    
}

class PointList
{
    public PointList (Vector3f rkPoint, PointList pkNext)
    {
        m_kPoint = rkPoint;
        m_kNext = pkNext;
    }

    Vector3f m_kPoint;
    PointList m_kNext;
};