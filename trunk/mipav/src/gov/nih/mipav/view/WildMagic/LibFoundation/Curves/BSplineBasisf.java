package gov.nih.mipav.view.WildMagic.LibFoundation.Curves;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;

public class BSplineBasisf {
	protected int m_iNumCtrlPoints;    // n+1
	protected int m_iDegree;           // d
	protected float[] m_afKnot;          // knot[n+d+2]
	protected boolean m_bOpen, m_bUniform;

    // Storage for the basis functions and their derivatives first three
    // derivatives.  The basis array is always allocated by the constructor
    // calls.  A derivative basis array is allocated on the first call to a
    // derivative member function.
	protected float[][] m_aafBD0;             // bd0[d+1][n+d+1]
	protected float[][] m_aafBD1;     // bd1[d+1][n+d+1]
	protected float[][] m_aafBD2;     // bd2[d+1][n+d+1]
	protected float[][] m_aafBD3;     // bd3[d+1][n+d+1]
	
	public BSplineBasisf() {
		
	}
	
	// Open uniform or periodic uniform.  The knot array is internally
    // generated with equally spaced elements.
	//----------------------------------------------------------------------------
	public BSplineBasisf(int iNumCtrlPoints, int iDegree, boolean bOpen)
	{
	    Create(iNumCtrlPoints,iDegree,bOpen);
	}
	
	//----------------------------------------------------------------------------
	public void Create (int iNumCtrlPoints, int iDegree, boolean bOpen)
	{
	    m_bUniform = true;

	    int i, iNumKnots = Initialize(iNumCtrlPoints,iDegree,bOpen);
	    float fFactor = ((float)1.0)/(m_iNumCtrlPoints-m_iDegree);
	    if (m_bOpen)
	    {
	        for (i = 0; i <= m_iDegree; i++)
	        {
	            m_afKnot[i] = (float)0.0;
	        }

	        for (/**/; i < m_iNumCtrlPoints; i++)
	        {
	            m_afKnot[i] = (i-m_iDegree)*fFactor;
	        }

	        for (/**/; i < iNumKnots; i++)
	        {
	            m_afKnot[i] = (float)1.0;
	        }
	    }
	    else
	    {
	        for (i = 0; i < iNumKnots; i++)
	        {
	            m_afKnot[i] = (i-m_iDegree)*fFactor;
	        }
	    }
	}
    
    

    // Open nonuniform.  The knot array must have n-d elements.  The elements
    // must be nondecreasing.  Each element must be in [0,1].  The caller is
    // responsible for deleting afKnot.  An internal copy is made, so to
    // dynamically change knots you must use the SetKnot function.
	//----------------------------------------------------------------------------
	public BSplineBasisf(int iNumCtrlPoints, int iDegree, float[] afKnot)
	{
	    Create(iNumCtrlPoints,iDegree,afKnot);
	}
	
	//----------------------------------------------------------------------------
	public void Create (int iNumCtrlPoints, int iDegree, float[] afKnot)
	{
	    m_bUniform = false;

	    int i, iNumKnots = Initialize(iNumCtrlPoints,iDegree,true);
	    for (i = 0; i <= m_iDegree; i++)
	    {
	        m_afKnot[i] = (float)0.0;
	    }

	    for (int j = 0; i < m_iNumCtrlPoints; i++, j++)
	    {
	        m_afKnot[i] = afKnot[j];
	    }

	    for (/**/; i < iNumKnots; i++)
	    {
	        m_afKnot[i] = (float)1.0;
	    }
	}
	
	//----------------------------------------------------------------------------
	public void dispose()
	{
	    m_afKnot = null;
	    m_aafBD0 = null;
	    m_aafBD1 = null;
	    m_aafBD2 = null;
	    m_aafBD3 = null;
	}
	//----------------------------------------------------------------------------
	public int GetNumCtrlPoints ()
	{
	    return m_iNumCtrlPoints;
	}
	
	//----------------------------------------------------------------------------
	public int GetDegree()
	{
	    return m_iDegree;
	}
	
	public boolean IsOpen()
	{
	    return m_bOpen;
	}
	
	public boolean IsUniform()
	{
	    return m_bUniform;
	}
	
	//----------------------------------------------------------------------------
	public float GetD0 (int i)
	{
	    return m_aafBD0[m_iDegree][i];
	}
	
	//----------------------------------------------------------------------------
	public float GetD1 (int i)
	{
	    return m_aafBD1[m_iDegree][i];
	}
	
	
	//----------------------------------------------------------------------------
	public float GetD2 (int i)
	{
	    return m_aafBD2[m_iDegree][i];
	}
	
	//----------------------------------------------------------------------------
	public float GetD3 (int i)
	{
	    return m_aafBD3[m_iDegree][i];
	}
	
	//----------------------------------------------------------------------------
	public float[][] Allocate()
	{
	    int iRows = m_iDegree + 1;
	    int iCols = m_iNumCtrlPoints + m_iDegree;
	    float[][] aafArray;
	    aafArray = new float[iRows][iCols];
	    return aafArray;
	}
	
	//----------------------------------------------------------------------------
	public void Deallocate (float[][] aafArray)
	{
	    if (aafArray != null)
	    {
	        aafArray = null;
	    }
	}
	
	//----------------------------------------------------------------------------
	public int Initialize (int iNumCtrlPoints, int iDegree, boolean bOpen)
	{
	    assert(iNumCtrlPoints >= 2);
	    assert(1 <= iDegree && iDegree <= iNumCtrlPoints-1);

	    m_iNumCtrlPoints = iNumCtrlPoints;
	    m_iDegree = iDegree;
	    m_bOpen = bOpen;

	    int iNumKnots = m_iNumCtrlPoints+m_iDegree+1;
	    m_afKnot = new float[iNumKnots];

	    m_aafBD0 = Allocate();
	    m_aafBD1 = null;
	    m_aafBD2 = null;
	    m_aafBD3 = null;

	    return iNumKnots;
	}
	
	//----------------------------------------------------------------------------
	public void SetKnot (int i, float fKnot)
	{
	    if (!m_bUniform)
	    {
	        // access only allowed to elements d+1 <= j <= n
	        int j = i + m_iDegree + 1;
	        if (m_iDegree+1 <= j && j <= m_iNumCtrlPoints - 1)
	        {
	            m_afKnot[j] = fKnot;
	        }
	    }
	}
	
	//----------------------------------------------------------------------------
	public float GetKnot (int i)
	{
	    if (!m_bUniform)
	    {
	        // access only allowed to elements d+1 <= j <= n
	        int j = i + m_iDegree + 1;
	        if (m_iDegree+1 <= j && j <= m_iNumCtrlPoints - 1)
	        {
	            return m_afKnot[j];
	        }
	    }

	    return Float.MAX_VALUE;
	}
	
	//----------------------------------------------------------------------------
	public int GetKey (float[] rfTime)
	{
	    if (m_bOpen)
	    {
	        // open splines clamp to [0,1]
	        if (rfTime[0] <= (float)0.0)
	        {
	            rfTime[0] = (float)0.0;
	            return m_iDegree;
	        }
	        else if (rfTime[0] >= (float)1.0)
	        {
	            rfTime[0] = (float)1.0;
	            return m_iNumCtrlPoints-1;
	        }
	    }
	    else
	    {
	        // periodic splines wrap to [0,1]
	        if (rfTime[0] < (float)0.0 || rfTime[0] > (float)1.0)
	        {
	            rfTime[0] -= Math.floor((double)rfTime[0]);
	        }
	    }


	    int i;

	    if (m_bUniform)
	    {
	        i = m_iDegree + (int)((m_iNumCtrlPoints-m_iDegree)*rfTime[0]);
	    }
	    else
	    {
	        for (i = m_iDegree+1; i <= m_iNumCtrlPoints; i++)
	        {
	            if (rfTime[0] < m_afKnot[i])
	            {
	                break;
	            }
	        }
	        i--;
	    }

	    return i;
	}
	
	//----------------------------------------------------------------------------
	public void Compute (float fTime, int uiOrder, int[] riMinIndex, int[] riMaxIndex) 
	{
	    // only derivatives through third order currently supported
	    assert(uiOrder <= 3);

	    if (uiOrder >= 1)
	    {
	        if (m_aafBD1 == null )
	        {
	            m_aafBD1 = Allocate();
	        }

	        if (uiOrder >= 2)
	        {
	            if (m_aafBD2 == null)
	            {
	                m_aafBD2 = Allocate();
	            }

	            if (uiOrder >= 3)
	            {
	                if (m_aafBD3 == null)
	                {
	                    m_aafBD3 = Allocate();
	                }
	            }
	        }
	    }
        float[] time = new float[1];
        time[0] = fTime;
	    int i = GetKey(time);
	    fTime = time[0];
	    m_aafBD0[0][i] = (float)1.0;

	    if (uiOrder >= 1)
	    {
	        m_aafBD1[0][i] = (float)0.0;
	        if (uiOrder >= 2)
	        {
	            m_aafBD2[0][i] = (float)0.0;
	            if (uiOrder >= 3)
	            {
	                m_aafBD3[0][i] = (float)0.0;
	            }
	        }
	    }

	    float fN0 = fTime -m_afKnot[i], fN1 = m_afKnot[i+1]-fTime;
	    float fInvD0, fInvD1;
	    int j;
	    for (j = 1; j <= m_iDegree; j++)
	    {
	        fInvD0 = ((float)1.0)/(m_afKnot[i+j]-m_afKnot[i]);
	        fInvD1 = ((float)1.0)/(m_afKnot[i+1]-m_afKnot[i-j+1]);

	        m_aafBD0[j][i] = fN0*m_aafBD0[j-1][i]*fInvD0;
	        m_aafBD0[j][i-j] = fN1*m_aafBD0[j-1][i-j+1]*fInvD1;

	        if (uiOrder >= 1)
	        {
	            m_aafBD1[j][i] = (fN0*m_aafBD1[j-1][i]+m_aafBD0[j-1][i])*fInvD0;
	            m_aafBD1[j][i-j] = (fN1*m_aafBD1[j-1][i-j+1]-m_aafBD0[j-1][i-j+1])
	                *fInvD1;

	            if (uiOrder >= 2)
	            {
	                m_aafBD2[j][i] = (fN0*m_aafBD2[j-1][i] +
	                    ((float)2.0)*m_aafBD1[j-1][i])*fInvD0;
	                m_aafBD2[j][i-j] = (fN1*m_aafBD2[j-1][i-j+1] -
	                    ((float)2.0)*m_aafBD1[j-1][i-j+1])*fInvD1;

	                if (uiOrder >= 3)
	                {
	                    m_aafBD3[j][i] = (fN0*m_aafBD3[j-1][i] +
	                        ((float)3.0)*m_aafBD2[j-1][i])*fInvD0;
	                    m_aafBD3[j][i-j] = (fN1*m_aafBD3[j-1][i-j+1] -
	                        ((float)3.0)*m_aafBD2[j-1][i-j+1])*fInvD1;
	                }
	            }
	        }
	    }

	    for (j = 2; j <= m_iDegree; j++)
	    {
	        for (int k = i-j+1; k < i; k++)
	        {
	            fN0 = fTime-m_afKnot[k];
	            fN1 = m_afKnot[k+j+1]-fTime;
	            fInvD0 = ((float)1.0)/(m_afKnot[k+j]-m_afKnot[k]);
	            fInvD1 = ((float)1.0)/(m_afKnot[k+j+1]-m_afKnot[k+1]);

	            m_aafBD0[j][k] = fN0*m_aafBD0[j-1][k]*fInvD0 + fN1*
	                m_aafBD0[j-1][k+1]*fInvD1;

	            if (uiOrder >= 1)
	            {
	                m_aafBD1[j][k] = (fN0*m_aafBD1[j-1][k]+m_aafBD0[j-1][k])*
	                    fInvD0 + (fN1*m_aafBD1[j-1][k+1]-m_aafBD0[j-1][k+1])*
	                    fInvD1;

	                if (uiOrder >= 2)
	                {
	                    m_aafBD2[j][k] = (fN0*m_aafBD2[j-1][k] +
	                        ((float)2.0)*m_aafBD1[j-1][k])*fInvD0 +
	                        (fN1*m_aafBD2[j-1][k+1]- ((float)2.0)*
	                        m_aafBD1[j-1][k+1])*fInvD1;

	                    if (uiOrder >= 3)
	                    {
	                        m_aafBD3[j][k] = (fN0*m_aafBD3[j-1][k] +
	                            ((float)3.0)*m_aafBD2[j-1][k])*fInvD0 +
	                            (fN1*m_aafBD3[j-1][k+1] - ((float)3.0)*
	                            m_aafBD2[j-1][k+1])*fInvD1;
	                    }
	                }
	            }
	        }
	    }

	    riMinIndex[0] = i - m_iDegree;
	    riMaxIndex[0] = i;
	}
	
}