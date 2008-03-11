package gov.nih.mipav.view.WildMagic.LibFoundation.Curves;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;

public class BSplineCurve3f extends SingleCurve3f {
	protected int m_iNumCtrlPoints;
	protected Vector3f[] m_akCtrlPoint; // ctrl[n+1]
	protected boolean m_bLoop;
	protected BSplineBasisf m_kBasis;
	protected int m_iReplicate; // the number of replicated control points

	// Construction and destruction.  The caller is responsible for deleting
	// the input arrays if they were dynamically allocated.  Internal copies
	// of the arrays are made, so to dynamically change control points or
	// knots you must use the 'SetControlPoint', 'GetControlPoint', and
	// 'Knot' member functions.

	// Uniform spline.  The number of control points is n+1 >= 2.  The degree
	// of the B-spline is d and must satisfy 1 <= d <= n.  The knots are
	// implicitly calculated in [0,1].  If bOpen is 'true', the spline is
	// open and the knots are
	//   t[i] = 0,               0 <= i <= d
	//          (i-d)/(n+1-d),   d+1 <= i <= n
	//          1,               n+1 <= i <= n+d+1
	// If bOpen is 'false', the spline is periodic and the knots are
	//   t[i] = (i-d)/(n+1-d),   0 <= i <= n+d+1
	// If bLoop is 'true', extra control points are added to generate a closed
	// curve.  For an open spline, the control point array is reallocated and
	// one extra control point is added, set to the first control point
	// C[n+1] = C[0].  For a periodic spline, the control point array is
	// reallocated and the first d points are replicated.  In either case the
	// knot array is calculated accordingly.
	public BSplineCurve3f(int iNumCtrlPoints, Vector3f[] akCtrlPoint,
			int iDegree, boolean bLoop, boolean bOpen) {

		super((float) 0.0, (float) 1.0);
		m_bLoop = bLoop;
		assert (iNumCtrlPoints >= 2);
		assert (1 <= iDegree && iDegree <= iNumCtrlPoints - 1);

		m_iNumCtrlPoints = iNumCtrlPoints;
		m_iReplicate = (bLoop ? (bOpen ? 1 : iDegree) : 0);
		CreateControl(akCtrlPoint);
		m_kBasis = new BSplineBasisf();
		m_kBasis.Create(m_iNumCtrlPoints + m_iReplicate, iDegree, bOpen);
	}

	// Open, nonuniform spline.  The knot array must have n-d elements.  The
	// elements must be nondecreasing.  Each element must be in [0,1].
	public BSplineCurve3f(int iNumCtrlPoints, Vector3f[] akCtrlPoint,
			int iDegree, boolean bLoop, float[] afKnot) {
		super((float) 0.0, (float) 1.0);
		m_bLoop = bLoop;

		assert (iNumCtrlPoints >= 2);
		assert (1 <= iDegree && iDegree <= iNumCtrlPoints - 1);

		m_iNumCtrlPoints = iNumCtrlPoints;
		m_iReplicate = (bLoop ? 1 : 0);
		CreateControl(akCtrlPoint);
		m_kBasis.Create(m_iNumCtrlPoints + m_iReplicate, iDegree, afKnot);
	}

	//----------------------------------------------------------------------------
	public void dispose() {
		// m_akCtrlPoint = null;
	}

	//----------------------------------------------------------------------------
	protected void CreateControl(Vector3f[] akCtrlPoint) {
		int i;
		int iNewNumCtrlPoints = m_iNumCtrlPoints + m_iReplicate;
		m_akCtrlPoint = new Vector3f[iNewNumCtrlPoints];
		int uiDstSize = iNewNumCtrlPoints; // *sizeof(Vector3f);
		int uiSrcSize = m_iNumCtrlPoints; // *sizeof(Vector3f);
		// System::Memcpy(m_akCtrlPoint,uiDstSize,akCtrlPoint,uiSrcSize);
		
		System.arraycopy(akCtrlPoint, 0, m_akCtrlPoint, 0, akCtrlPoint.length);
		/*
		for ( i = 0; i < m_iNumCtrlPoints; i++ ) {
	    	System.err.println("akCtrlPoint[" + i + "] = " + akCtrlPoint[i].X() + ", " + akCtrlPoint[i].Y() + ", " + akCtrlPoint[i].Z() );
	    }
	    */
		// size_t uiDstSize = iNewNumCtrlPoints*sizeof(Vector3<Real>);
	    // size_t uiSrcSize = m_iNumCtrlPoints*sizeof(Vector3<Real>);
	    // System::Memcpy(m_akCtrlPoint,uiDstSize,akCtrlPoint,uiSrcSize);
		for (i = 0; i < m_iNumCtrlPoints; i++)
	    {
			// m_akCtrlPoint[i] = new Vector3f();
	        m_akCtrlPoint[i] = akCtrlPoint[i];
	    }
		
	    for (i = 0; i < m_iReplicate; i++)
	    {
	    	// m_akCtrlPoint[m_iNumCtrlPoints+i] = new Vector3f();
	        m_akCtrlPoint[m_iNumCtrlPoints+i] = akCtrlPoint[i];
	    }
	    // System.err.println("m_iNumCtrlPoints = " + m_iNumCtrlPoints + " m_iReplicate = " + m_iReplicate + " iNewNumCtrlPoints = " + iNewNumCtrlPoints);
	    /*
	    for ( i = 0; i < iNewNumCtrlPoints; i++ ) {
	    	System.err.println("m_akCtrlPoint[" + i + "] = " + m_akCtrlPoint[i].X() + ", " + m_akCtrlPoint[i].Y() + ", " + m_akCtrlPoint[i].Z() );
	    }
	    */
	    // if ( true ) System.exit(0);
	}
	
	//----------------------------------------------------------------------------
	public int GetNumCtrlPoints ()
	{
	    return m_iNumCtrlPoints;
	}
	//----------------------------------------------------------------------------
	public int GetDegree ()
	{
	    return m_kBasis.GetDegree();
	}
	//----------------------------------------------------------------------------
	public boolean IsOpen ()
	{
	    return m_kBasis.IsOpen();
	}
	//----------------------------------------------------------------------------
	public boolean IsUniform ()
	{
	    return m_kBasis.IsUniform();
	}
	//----------------------------------------------------------------------------
	public boolean IsLoop()
	{
	    return m_bLoop;
	}

	//----------------------------------------------------------------------------
	public void SetControlPoint (int i, Vector3f rkCtrl)
	{
	    if (0 <= i && i < m_iNumCtrlPoints)
	    {
	        // set the control point
	        m_akCtrlPoint[i] = rkCtrl;

	        // set the replicated control point
	        if (i < m_iReplicate)
	        {
	            m_akCtrlPoint[m_iNumCtrlPoints+i] = rkCtrl;
	        }
	    }
	}
	//----------------------------------------------------------------------------
	public Vector3f GetControlPoint (int i)
	{
	    if (0 <= i && i < m_iNumCtrlPoints)
	    {
	        return m_akCtrlPoint[i];
	    }

	    return new Vector3f(Float.MAX_VALUE,Float.MAX_VALUE, Float.MAX_VALUE);
	}
	
	//----------------------------------------------------------------------------
	public void SetKnot (int i, float fKnot)
	{
	    m_kBasis.SetKnot(i,fKnot);
	}
	
	//----------------------------------------------------------------------------
	public float GetKnot(int i)
	{
	    return m_kBasis.GetKnot(i);
	}
	
	//----------------------------------------------------------------------------
	public BSplineBasisf GetBasis ()
	{
	    return m_kBasis;
	}
	
	//----------------------------------------------------------------------------
	public Vector3f GetPosition(float fTime)
	{
	    Vector3f kPos = new Vector3f();
	    Get(fTime,kPos,null,null,null);
	    return kPos;
	}
	//----------------------------------------------------------------------------
	public Vector3f GetFirstDerivative (float fTime) 
	{
	    Vector3f kDer1 = new Vector3f();
	    Get(fTime,null,kDer1,null,null);
	    return kDer1;
	}
	
	//----------------------------------------------------------------------------
	public Vector3f GetSecondDerivative (float fTime)
	{
	    Vector3f kDer2 = new Vector3f();
	    Get(fTime,null,null,kDer2,null);
	    return kDer2;
	}
	
	//----------------------------------------------------------------------------
	public Vector3f GetThirdDerivative (float fTime) 
	{
	    Vector3f kDer3 = new Vector3f();
	    Get(fTime,null,null,null,kDer3);
	    return kDer3;
	}
	//----------------------------------------------------------------------------
	public float GetVariation (float f1, float f2, Vector3f v1, Vector3f v2)
	{
	    return (float)0.0;
	}
	
	//----------------------------------------------------------------------------
	public void Get (float fTime, Vector3f pkPos,
	    Vector3f pkDer1, Vector3f pkDer2, Vector3f pkDer3)
	{
	    int i;
	    int[] iMin = new int[1];
	    int[] iMax = new int[1];
	    if (pkDer3 != null )
	    {
	        m_kBasis.Compute(fTime,3,iMin,iMax);
	    }
	    else if (pkDer2 != null )
	    {
	        m_kBasis.Compute(fTime,2,iMin,iMax);
	    }
	    else if (pkDer1 != null )
	    {
	        m_kBasis.Compute(fTime,1,iMin,iMax);
	    }
	    else
	    {
	        m_kBasis.Compute(fTime,0,iMin,iMax);
	    }

	    if (pkPos != null)
	    {
	        pkPos = Vector3f.ZERO;
	        
	        for (i = iMin[0]; i <= iMax[0]; i++)
	        {
	            // *pkPos += m_kBasis.GetD0(i)*m_akCtrlPoint[i];
	        	pkPos.addEquals(m_akCtrlPoint[i].scale(m_kBasis.GetD0(i)));
	        }
	    }

	    if (pkDer1 != null)
	    {
	        pkDer1 = Vector3f.ZERO;
	        for (i = iMin[0]; i <= iMax[0]; i++)
	        {
	            // *pkDer1 += m_kBasis.GetD1(i)*m_akCtrlPoint[i];
	        	pkDer1.addEquals(m_akCtrlPoint[i].scale(m_kBasis.GetD1(i)));
	        }
	    }

	    if (pkDer2 != null)
	    {
	        pkDer2 = Vector3f.ZERO;
	        for (i = iMin[0]; i <= iMax[0]; i++)
	        {
	            // *pkDer2 += m_kBasis.GetD2(i)*m_akCtrlPoint[i];
	        	pkDer2.addEquals(m_akCtrlPoint[i].scale(m_kBasis.GetD2(i)));
	        }
	    }

	    if (pkDer3 != null )
	    {
	        pkDer3 = Vector3f.ZERO;
	        for (i = iMin[0]; i <= iMax[0]; i++)
	        {
	            // *pkDer3 += m_kBasis.GetD3(i)*m_akCtrlPoint[i];
	        	pkDer3.addEquals(m_akCtrlPoint[i].scale(m_kBasis.GetD3(i)));
	        }
	    }
	}
	
}