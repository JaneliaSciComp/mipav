package gov.nih.mipav.view.WildMagic.LibGraphics.Surfaces;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibFoundation.Curves.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;

public class TubeSurface extends TriMesh
    implements StreamInterface
{
	 Curve3f m_pkMedial;
	 float m_oRadial;
	 int m_iMedialSamples, m_iSliceSamples;
	 Vector3f m_kUpVector;
	 float[] m_afSin;
	 float[] m_afCos;
	 boolean m_bClosed, m_bSampleByArcLength;
	
	// ----------------------------------------------------------------------------
	public TubeSurface (Curve3f pkMedial, float oRadial,
	     boolean bClosed, Vector3f rkUpVector, int iMedialSamples,
	     int iSliceSamples, Attributes rkAttr, boolean bSampleByArcLength,
	     boolean bInsideView, Vector2f pkUVMin, Vector2f pkUVMax)
	 {
	     // assert(pkMedial && oRadial);
	     // assert((pkUVMin && pkUVMax) || (!pkUVMin && !pkUVMax));

	     m_pkMedial = pkMedial;
	     m_oRadial = oRadial;
	     m_kUpVector = rkUpVector;
	     m_iMedialSamples = iMedialSamples;
	     m_iSliceSamples = iSliceSamples;
	     m_bClosed = bClosed;
	     m_bSampleByArcLength = bSampleByArcLength;

	     // compute the surface vertices
	     int iVQuantity;
	     if (m_bClosed)
	     {
	         iVQuantity = (m_iSliceSamples+1)*(m_iMedialSamples+1);
	     }
	     else
	     {
	         iVQuantity = (m_iSliceSamples+1)*m_iMedialSamples;
	     }

	     VBuffer = new VertexBuffer(rkAttr,iVQuantity);
	     ComputeSinCos();
	     ComputeVertices();

	     // compute the surface normals
	     if (rkAttr.HasNormal())
	     {
	         ComputeNormals();
	     }

	     // compute the surface textures coordinates
	     if (pkUVMin != null && pkUVMax != null )
	     {
	         assert(rkAttr.GetMaxTCoords() > 0);
	         ComputeUVs(pkUVMin, pkUVMax);
	     }

	     // compute the surface triangle connectivity
	     ComputeIndices(bInsideView);

	     UpdateModelBound();
	 }

	 // ----------------------------------------------------------------------------
	 public TubeSurface ()
	 {
	     m_pkMedial = null;
	     m_oRadial = 0;
	     m_afSin = null;
	     m_afCos = null;
	 }	
	
	//----------------------------------------------------------------------------
	private void ComputeSinCos ()
	{
	    // Compute slice vertex coefficients.  The first and last coefficients
	    // are duplicated to allow a closed cross section that has two different
	    // pairs of texture coordinates at the shared vertex.

	    m_afSin = new float[m_iSliceSamples + 1];
	    m_afCos = new float[m_iSliceSamples + 1];

	    float fInvSliceSamples = 1.0f/(float)m_iSliceSamples;
	    for (int i = 0; i < m_iSliceSamples; i++)
	    {
	        float fAngle = Mathf.TWO_PI*fInvSliceSamples*i;
	        m_afCos[i] = (float)Math.cos(fAngle);
	        m_afSin[i] = (float)Math.sin(fAngle);
	    }
	    m_afSin[m_iSliceSamples] = m_afSin[0];
	    m_afCos[m_iSliceSamples] = m_afCos[0];
	}
	
	//----------------------------------------------------------------------------
	private void ComputeVertices ()
	{
	    float fTMin = m_pkMedial.GetMinTime();
	    float fTRange = m_pkMedial.GetMaxTime() - fTMin;
	    Vector3f kResult;

	    // sampling by arc length requires the total length of the curve
	    float fTotalLength;
	    if (m_bSampleByArcLength)
	    {
	        fTotalLength = m_pkMedial.GetTotalLength();
	    }
	    else
	    {
	        fTotalLength = 0.0f;
	    }

	    // vertex construction requires a normalized time (uses a division)
	    float fDenom;
	    if (m_bClosed)
	    {
	        fDenom = 1.0f/(float)m_iMedialSamples;
	    }
	    else
	    {
	        fDenom = 1.0f/(float)(m_iMedialSamples-1);
	    }

	    for (int iM = 0, iV = 0; iM < m_iMedialSamples; iM++)
	    {
	        float fT;
	        if (m_bSampleByArcLength)
	        {
	            fT = m_pkMedial.GetTime(iM*fTotalLength*fDenom);
	        }
	        else
	        {
	            fT = fTMin + iM*fTRange*fDenom;
	        }

	        // float fRadius = m_oRadial(fT);   // ?????????
	        float fRadius = 0.025f;
	        
	        // compute frame (position P, tangent T, normal N, binormal B)
	        Vector3f kP = new Vector3f();
	        Vector3f kT = new Vector3f();
	        Vector3f kN = new Vector3f();
	        Vector3f kB = new Vector3f();
	        
	        if (m_kUpVector != Vector3f.ZERO)
	        {
	            // Always use 'up' vector N rather than curve normal.  You must
	            // constrain the curve so that T and N are never parallel.  To
	            // build the frame from this, let
	            //     B = Cross(T,N)/Length(Cross(T,N))
	            // and replace
	            //     N = Cross(B,T)/Length(Cross(B,T)).
	            kP = m_pkMedial.GetPosition(fT);
	            kT = m_pkMedial.GetTangent(fT);
	            kT.UnitCross(m_kUpVector,kB);
	            kB.UnitCross(kT, kN);
	        }
	        else
	        {
	            // use Frenet frame to create slices
	            m_pkMedial.GetFrame(fT,kP,kT,kN,kB);
	        }

	        // compute slice vertices, duplication at end point as noted earlier
	        int iSave = iV;
	        for (int i = 0; i < m_iSliceSamples; i++)
	        {
                    kResult = kN.scale( m_afCos[i]).add(kB.scale(m_afSin[i]) );
                    kResult.scaleEquals(fRadius);
                    kResult.addEquals(kP);
                    VBuffer.SetPosition3(iV, kResult);
                    iV++;
	        }
	        VBuffer.SetPosition3(iV, VBuffer.GetPosition3(iSave));
	        iV++;
	    }

	    if (m_bClosed)
	    {
	        for (int i = 0; i <= m_iSliceSamples; i++)
	        {
	            int i1 = Index(i,m_iMedialSamples);
	            int i0 = Index(i,0);
	            VBuffer.SetPosition3(i1, VBuffer.GetPosition3(i0));
	        }
	    }
	}
	
	//----------------------------------------------------------------------------
	private void ComputeNormals ()
	{
	    int iS, iSm, iSp, iM, iMm, iMp;
	    Vector3f kDir0, kDir1;
	    Vector3f kResult;
	    Vector3f kNormal;
	    
	    // interior normals (central differences)
	    for (iM = 1; iM <= m_iMedialSamples-2; iM++)
	    {
	        for (iS = 0; iS < m_iSliceSamples; iS++)
	        {
	            iSm = (iS > 0 ? iS-1 : m_iSliceSamples-1);
	            iSp = iS + 1;
	            iMm = iM - 1;
	            iMp = iM + 1;
	            kDir0 = VBuffer.GetPosition3(Index(iSm,iM)).sub(
	                VBuffer.GetPosition3(Index(iSp,iM)));
	            kDir1 = VBuffer.GetPosition3(Index(iS,iMm)).sub(
	                VBuffer.GetPosition3(Index(iS,iMp)));
	            kResult = new Vector3f();
	            // VBuffer.Normal3(Index(iS,iM)) = kDir0.UnitCross(kDir1);
	            kDir0.UnitCross(kDir1, kResult);
	            VBuffer.SetNormal3(Index(iS,iM), kResult);
	        }
	        // VBuffer.Normal3(Index(m_iSliceSamples,iM)) = VBuffer.Normal3(Index(0,iM));
	        kNormal = new Vector3f();
	        VBuffer.GetNormal3(Index(0,iM), kNormal);
	        VBuffer.SetNormal3(Index(m_iSliceSamples,iM), kNormal);
	    }

	    // boundary normals
	    if (m_bClosed)
	    {
	        // central differences
	        for (iS = 0; iS < m_iSliceSamples; iS++)
	        {
	            iSm = (iS > 0 ? iS-1 : m_iSliceSamples-1);
	            iSp = iS + 1;

	            // m = 0
	            kDir0 = VBuffer.GetPosition3(Index(iSm,0)).sub(
	                VBuffer.GetPosition3(Index(iSp,0)));
	            kDir1 = VBuffer.GetPosition3(Index(iS,m_iMedialSamples-1)).sub(
	                VBuffer.GetPosition3(Index(iS,1)));
	            // VBuffer.Normal3(iS) = kDir0.UnitCross(kDir1);
	            kResult = new Vector3f();
	            kDir0.UnitCross(kDir1, kResult);
	            VBuffer.SetNormal3(iS, kResult);

	            // m = max
	            // VBuffer.Normal3(Index(iS,m_iMedialSamples)) = VBuffer.Normal3(Index(iS,0));
	            kNormal = new Vector3f();
	            VBuffer.GetNormal3(Index(iS,0), kNormal);
	            VBuffer.SetNormal3(Index(iS,m_iMedialSamples), kNormal);
	        }
	        // VBuffer.Normal3(Index(m_iSliceSamples,0)) = VBuffer.Normal3(Index(0,0));
	        kNormal = new Vector3f();
	        VBuffer.GetNormal3(Index(0,0), kNormal);
	        VBuffer.SetNormal3(Index(m_iSliceSamples,0), kNormal);
	        // VBuffer.Normal3(Index(m_iSliceSamples,m_iMedialSamples)) = VBuffer.Normal3(Index(0,m_iMedialSamples));
	        kNormal = new Vector3f();
	        VBuffer.GetNormal3(Index(0,m_iMedialSamples), kNormal);
	        VBuffer.SetNormal3(Index(m_iSliceSamples,m_iMedialSamples), kNormal);
	        
	    }
	    else
	    {
	        // one-sided finite differences

	        // m = 0
	        for (iS = 0; iS < m_iSliceSamples; iS++)
	        {
	            iSm = (iS > 0 ? iS-1 : m_iSliceSamples-1);
	            iSp = iS + 1;
	            kDir0 = VBuffer.GetPosition3(Index(iSm,0)).sub(
	                VBuffer.GetPosition3(Index(iSp,0)));
	            kDir1 = VBuffer.GetPosition3(Index(iS,0)).sub(
	                VBuffer.GetPosition3(Index(iS,1)));
	            // VBuffer.Normal3(Index(iS,0)) = kDir0.UnitCross(kDir1);
	            kResult = new Vector3f();
	            kDir0.UnitCross(kDir1, kResult);
	            VBuffer.SetNormal3(Index(iS,0), kResult);
	        }
	        /*
	        VBuffer.Normal3(Index(m_iSliceSamples,0)) =
	            VBuffer.Normal3(Index(0,0));
	        */
	        kNormal = new Vector3f();
	        VBuffer.GetNormal3(Index(0,0), kNormal);
	        VBuffer.SetNormal3(Index(m_iSliceSamples,0), kNormal);

	        // m = max-1
	        for (iS = 0; iS < m_iSliceSamples; iS++)
	        {
	            iSm = (iS > 0 ? iS-1 : m_iSliceSamples-1);
	            iSp = iS + 1;
	            kDir0 = VBuffer.GetPosition3(Index(iSm,m_iMedialSamples-1)).sub(
	                VBuffer.GetPosition3(Index(iSp,m_iMedialSamples-1)));
	            kDir1 = VBuffer.GetPosition3(Index(iS,m_iMedialSamples-2)).sub(
	                VBuffer.GetPosition3(Index(iS,m_iMedialSamples-1)));
	            kResult = new Vector3f();
	            kDir0.UnitCross(kDir1, kResult);
	            VBuffer.SetNormal3(iS, kResult);
	        }
	        /*
	        VBuffer.Normal3(Index(m_iSliceSamples,m_iMedialSamples-1)) =
	            VBuffer.Normal3(Index(0,m_iMedialSamples-1));
	        */
	        kNormal = new Vector3f();
	        VBuffer.GetNormal3(Index(0,m_iMedialSamples-1), kNormal);
	        VBuffer.SetNormal3(Index(m_iSliceSamples,m_iMedialSamples-1), kNormal);
	    }
	}
	//----------------------------------------------------------------------------
	private void ComputeUVs (Vector2f rkUVMin, Vector2f rkUVMax)
	{
	    Attributes rkAttr = VBuffer.GetAttributes();
	    Vector2f kUVRange = rkUVMax.sub(rkUVMin);
	    int iMMax = (m_bClosed ? m_iMedialSamples : m_iMedialSamples - 1);
	    int iV = 0;
	    for (int iM = 0; iM <= iMMax; iM++)
	    {
	        float fMRatio = ((float)iM)/((float)iMMax);
	        float fMValue = rkUVMin.Y() + fMRatio*kUVRange.Y();
	        for (int iS = 0; iS <= m_iSliceSamples; iS++)
	        {
	            float fSRatio = ((float)iS)/((float)m_iSliceSamples);
	            float fSValue = rkUVMin.X() + fSRatio*kUVRange.X();
	            Vector2f kTCoord = new Vector2f(fSValue,fMValue);
	            // pkVB.SetTCoord2(iUnit,i, 1.0f,fAxisFraction);
	            for (int iUnit = 0; iUnit < rkAttr.GetMaxTCoords(); iUnit++)
	            {
	                if (rkAttr.HasTCoord(iUnit))
	                {
	                    // assert(rkAttr.GetTCoordChannels(iUnit) == 2);
	                    // VBuffer.TCoord2(iUnit,iV) = kTCoord;
	                    VBuffer.SetTCoord2(iUnit, iV, kTCoord);
	                }
	            }
	            iV++;
	        }
	    }
	}
	
	//----------------------------------------------------------------------------
	private void ComputeIndices (boolean bInsideView)
	{
	    int iTQuantity;
	    if (m_bClosed)
	    {
	        iTQuantity = 2*m_iSliceSamples*m_iMedialSamples;
	    }
	    else
	    {
	        iTQuantity = 2*m_iSliceSamples*(m_iMedialSamples-1);
	    }

	    IBuffer = new IndexBuffer(3*iTQuantity);
	    // int[] aiIndex = IBuffer.GetData();
	    int[] aiLocalIndex = IBuffer.GetData();

	    int iM, iMStart, i0, i1, i2, i3, i;
	    // int[] aiLocalIndex = aiIndex;
	    int iIndex = 0;
	    for (iM = 0, iMStart = 0; iM < m_iMedialSamples-1; iM++)
	    {
	        i0 = iMStart;
	        i1 = i0 + 1;
	        iMStart += m_iSliceSamples + 1;
	        i2 = iMStart;
	        i3 = i2 + 1;
	        for (i = 0; i < m_iSliceSamples; i++)
	        {
	            if (bInsideView)
	            {
	                aiLocalIndex[iIndex++] = i0++;
	                aiLocalIndex[iIndex++] = i2;
	                aiLocalIndex[iIndex++] = i1;
	                aiLocalIndex[iIndex++] = i1++;
	                aiLocalIndex[iIndex++] = i2++;
	                aiLocalIndex[iIndex++] = i3++;
	            }
	            else  // outside view
	            {
	                aiLocalIndex[iIndex++] = i0++;
	                aiLocalIndex[iIndex++] = i1;
	                aiLocalIndex[iIndex++] = i2;
	                aiLocalIndex[iIndex++] = i1++;
	                aiLocalIndex[iIndex++] = i3++;
	                aiLocalIndex[iIndex++] = i2++;
	            }
	        }
	    }

	    if (m_bClosed)
	    {
	        i0 = iMStart;
	        i1 = i0 + 1;
	        i2 = 0;
	        i3 = i2 + 1;
	        for (i = 0; i < m_iSliceSamples; i++)
	        {
	            if (bInsideView)
	            {
	                aiLocalIndex[iIndex++] = i0++;
	                aiLocalIndex[iIndex++] = i2;
	                aiLocalIndex[iIndex++] = i1;
	                aiLocalIndex[iIndex++] = i1++;
	                aiLocalIndex[iIndex++] = i2++;
	                aiLocalIndex[iIndex++] = i3++;
	            }
	            else  // outside view
	            {
	                aiLocalIndex[iIndex++] = i0++;
	                aiLocalIndex[iIndex++] = i1;
	                aiLocalIndex[iIndex++] = i2;
	                aiLocalIndex[iIndex++] = i1++;
	                aiLocalIndex[iIndex++] = i3++;
	                aiLocalIndex[iIndex++] = i2++;
	            }
	        }
	    }
	}
	
	//----------------------------------------------------------------------------
	public void GetTMinSlice (Vector3f[] akSlice)
	{
	    for (int i = 0; i <= m_iSliceSamples; i++)
	    {
	        akSlice[i] = VBuffer.GetPosition3(i);
	    }
	}
	
	//----------------------------------------------------------------------------
	public void GetTMaxSlice (Vector3f[] akSlice)
	{
	    int j = VBuffer.GetVertexQuantity() - m_iSliceSamples - 1;
	    for (int i = 0; i <= m_iSliceSamples; i++, j++)
	    {
	        akSlice[i] = VBuffer.GetPosition3(j);
	    }
	}
	
	//----------------------------------------------------------------------------
	public void UpdateSurface ()
	{
	    ComputeVertices();
	    UpdateModelBound();

	    if (VBuffer.GetAttributes().HasNormal())
	    {
	        ComputeNormals();
	    }
	}
	
	// ----------------------------------------------------------------------------
	Curve3f setMedial (Curve3f medial)
	{
		m_pkMedial = medial;
	    return m_pkMedial;
	}
	// ----------------------------------------------------------------------------
	Curve3f GetMedial ()
	{
	    return m_pkMedial;
	}
	// ----------------------------------------------------------------------------
    float setRadial (float radial)
	{
    	m_oRadial = radial;
	    return m_oRadial;
	}
	// ----------------------------------------------------------------------------
	float GetRadial ()
	{
	    return m_oRadial;
	}
	// ----------------------------------------------------------------------------
	Vector3f UpVector ()
	{
	    return m_kUpVector;
	}
	// ----------------------------------------------------------------------------
	Vector3f GetUpVector () 
	{
	    return m_kUpVector;
	}
	// ----------------------------------------------------------------------------
	int GetSliceSamples ()
	{
	    return m_iSliceSamples;
	}
	// ----------------------------------------------------------------------------
	int Index (int iS, int iM)
	{
	    return iS + (m_iSliceSamples+1)*iM;
	}
	// ----------------------------------------------------------------------------

	
}
