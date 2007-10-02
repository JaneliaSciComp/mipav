// Wild Magic Source Code
// David Eberly
// http://www.geometrictools.com
// Copyright (c) 1998-2007
//
// This library is free software; you can redistribute it and/or modify it
// under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation; either version 2.1 of the License, or (at
// your option) any later version.  The license is available for reading at
// either of the locations:
//     http://www.gnu.org/copyleft/lgpl.html
//     http://www.geometrictools.com/License/WildMagicLicense.pdf
//
// Version: 4.0.0 (2006/06/28)

package gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics;

public class GVectorf
{
    // construction
    public GVectorf ()
    {
        m_iSize = 0;
    }

    public GVectorf (int iSize)
    {
        if (iSize > 0)
        {
            m_iSize = iSize;
            m_afTuple = new float[m_iSize];
        }
        else
        {
            m_iSize = 0;
        }
    }

    public GVectorf (int iSize, final float[] afTuple)
    {
        if (iSize > 0)
        {
            m_iSize = iSize;
            m_afTuple = new float[m_iSize];
            for ( int i = 0; i < m_iSize; i++ )
            {
                m_afTuple[i] = afTuple[i];
            }
        }
        else
        {
            m_iSize = 0;
        }
    }

    public GVectorf (final GVectorf rkV)
    {
        m_iSize = rkV.m_iSize;
        if (m_iSize > 0)
        {
            m_afTuple = new float[m_iSize];
            for ( int i = 0; i < m_iSize; i++ )
            {
                m_afTuple[i] = rkV.m_afTuple[i];
            }
        }
    }

    public void finalize ()
    {
        m_afTuple = null;
    }

    // coordinate access
    public void SetSize (int iSize)
    {
        m_afTuple = null;
        if (iSize > 0)
        {
            m_iSize = iSize;
            m_afTuple = new float[m_iSize];
        }
        else
        {
            m_iSize = 0;
        }
    }

    public int GetSize ()
    {
        return m_iSize;
    }

    public void Set( int i, float fValue )
    {
        assert(0 <= i && i < m_iSize);
        m_afTuple[i] = fValue;
    }

    public float Get( int i )
    {
        assert(0 <= i && i < m_iSize);
        return m_afTuple[i];
    }

//     operator const Real* () const;
//     operator Real* ();
//     Real operator[] (int i) const;
//     Real& operator[] (int i);

    // assignment
//     GVectorf& operator= (const GVectorf& rkV);

    // comparison
//     bool operator== (const GVectorf& rkV) const;
//     bool operator!= (const GVectorf& rkV) const;
//     bool operator<  (const GVectorf& rkV) const;
//     bool operator<= (const GVectorf& rkV) const;
//     bool operator>  (const GVectorf& rkV) const;
//     bool operator>= (const GVectorf& rkV) const;

    // arithmetic operations
//     GVectorf operator+ (const GVectorf& rkV) const;
//     GVectorf operator- (const GVectorf& rkV) const;
//     GVectorf operator* (Real fScalar) const;
//     GVectorf operator/ (Real fScalar) const;
//     GVectorf operator- () const;

    // arithmetic updates
//     GVectorf& operator+= (const GVectorf& rkV);
//     GVectorf& operator-= (const GVectorf& rkV);
//     GVectorf& operator*= (Real fScalar);
//     GVectorf& operator/= (Real fScalar);

    // vector operations
    public float Length ()
    {
        float fSqrLen = 0.0f;
        for (int i = 0; i < m_iSize; i++)
        {
            fSqrLen += m_afTuple[i]*m_afTuple[i];
        }
        return (float)Math.sqrt(fSqrLen);
    }

    public float SquaredLength ()
    {
        float fSqrLen = 0.0f;
        for (int i = 0; i < m_iSize; i++)
        {
            fSqrLen += m_afTuple[i]*m_afTuple[i];
        }
        return fSqrLen;
    }

    public float Dot (final GVectorf rkV)
    {
        float fDot = 0.0f;
        for (int i = 0; i < m_iSize; i++)
        {
            fDot += m_afTuple[i]*rkV.m_afTuple[i];
        }
        return fDot;
    }

    public float Normalize ()
    {
        float fLength = Length();
        int i;

        if (fLength > Mathf.ZERO_TOLERANCE)
        {
            float fInvLength = (1.0f)/fLength;
            for (i = 0; i < m_iSize; i++)
            {
                m_afTuple[i] *= fInvLength;
            }
        }
        else
        {
            fLength = 0.0f;
            for (i = 0; i < m_iSize; i++)
            {
                m_afTuple[i] = 0.0f;
            }
        }
        return fLength;
    }

    /** support for comparisons */
    //private int CompareArrays (const GVectorf& rkV) const;

    private int m_iSize;
    private float[] m_afTuple = null;
};

// template <class Real>
// GVectorf<Real> operator* (Real fScalar, const GVectorf<Real>& rkV);
