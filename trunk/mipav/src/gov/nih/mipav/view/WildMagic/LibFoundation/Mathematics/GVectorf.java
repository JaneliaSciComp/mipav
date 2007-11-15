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

/** General sized float vector. */
public class GVectorf
{
    /** Construct a general vector of size 0. */
    public GVectorf ()
    {
        m_iSize = 0;
    }

    /** Construct a general vector of size iSize.
     * @param iSize, size of the new vector.
     */
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

    /** Construct a general vector of size iSize. Copy the afTuple
     * data into the new vector.
     * @param iSize, size of the new vector.
     * @param afTuple, new vector values.
     */
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

    /** Construct a general vector that is the copy of the input vector.
     * @param rkV, the vector to copy.
     */
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

    /** Delete the vector data. */
    public void dispose ()
    {
        m_afTuple = null;
    }

    /** Set the vector size.
     * @param iSize, new vector size.
     */
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

    /** Get the vector size.
     * @return vector size.
     */
    public int GetSize ()
    {
        return m_iSize;
    }

    /** Set the vector value at position i.
     * @param i, position to set.
     * @param fValue new value.
     */
    public void Set( int i, float fValue )
    {
        assert(0 <= i && i < m_iSize);
        m_afTuple[i] = fValue;
    }

    /** Get the vector value at position i.
     * @param i, position to get.
     * @return valor of vector at position i.
     */
    public float Get( int i )
    {
        assert(0 <= i && i < m_iSize);
        return m_afTuple[i];
    }

    /** Return the length of this vector.
     * @return the length of this vector.
     */
    public float Length ()
    {
        float fSqrLen = 0.0f;
        for (int i = 0; i < m_iSize; i++)
        {
            fSqrLen += m_afTuple[i]*m_afTuple[i];
        }
        return (float)Math.sqrt(fSqrLen);
    }

    /** Return the squared length of this vector.
     * @return the squared length of this vector.
     */
    public float SquaredLength ()
    {
        float fSqrLen = 0.0f;
        for (int i = 0; i < m_iSize; i++)
        {
            fSqrLen += m_afTuple[i]*m_afTuple[i];
        }
        return fSqrLen;
    }

    /** Return the dot-product of this vector with the input vector.
     * @param rkV, input vector.
     * @return dot product this*rkV.
     */
    public float Dot (final GVectorf rkV)
    {
        float fDot = 0.0f;
        for (int i = 0; i < m_iSize; i++)
        {
            fDot += m_afTuple[i]*rkV.m_afTuple[i];
        }
        return fDot;
    }

    /** Normalize this vector, return the length.
     * @return the length of this vector prior to normalization.
     */
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

    /** Vector size. */
    private int m_iSize;
    /** Vector data. */
    private float[] m_afTuple = null;
};
