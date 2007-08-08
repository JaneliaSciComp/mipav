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
//
// Ported to Java by Alexandra Bokinsky, PhD, Geometric Tools, Inc. (July 2007)
//

package gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics;

public class Vector4f
{
    /** Zero vector: */
    public static final Vector4f ZERO = new Vector4f(0.0f,0.0f,0.0f,0.0f);
    /** Unit-X vector: */
    public static final Vector4f UNIT_X = new Vector4f(1.0f,0.0f,0.0f,0.0f);  // (1,0,0,0)
    /** Unit-Y vector: */
    public static final Vector4f UNIT_Y = new Vector4f(0.0f,1.0f,0.0f,0.0f);  // (0,1,0,0)
    /** Unit-Z vector: */
    public static final Vector4f UNIT_Z = new Vector4f(0.0f,0.0f,1.0f,0.0f);  // (0,0,1,0)
    /** Unit-W vector: */
    public static final Vector4f UNIT_W = new Vector4f(0.0f,0.0f,0.0f,1.0f);  // (0,0,0,1)
    /** One vector: */
    public static final Vector4f ONE = new Vector4f(1.0f,1.0f,1.0f,1.0f);     // (1,1,1,1)

    /** construction, uninitialized */
    public Vector4f () {}  // uninitialized
    /** construction
     * @param fX, x-value
     * @param fY, y-value
     * @param fZ, z-value
     * @param fW, w-value
     */
    public Vector4f (float fX, float fY, float fZ, float fW)
    {
        m_afTuple[0] = fX;
        m_afTuple[1] = fY;
        m_afTuple[2] = fZ;
        m_afTuple[3] = fW;
    }

    /** construction
     * @param afTuple, x,y,z,w values
     */
    public Vector4f ( float[] afTuple)
    {
        m_afTuple[0] = afTuple[0];
        m_afTuple[1] = afTuple[1];
        m_afTuple[2] = afTuple[2];
        m_afTuple[3] = afTuple[3];
    }

    /** copy construction
     * @param rkV, vector to copy.
     */
    public Vector4f ( Vector4f rkV)
    {
        m_afTuple[0] = rkV.m_afTuple[0];
        m_afTuple[1] = rkV.m_afTuple[1];
        m_afTuple[2] = rkV.m_afTuple[2];
        m_afTuple[3] = rkV.m_afTuple[3];
    }

    /**
     * delete memory
     */
    public void finalize()
    {
        m_afTuple = null;
    }

    /** Return x-value
     * @return x-value
     */
    public float X ()
    {
        return m_afTuple[0];
    }

    /** Return y-value
     * @return y-value
     */
    public float Y ()
    {
        return m_afTuple[1];
    }

    /** Return z-value
     * @return z-value
     */
    public float Z ()
    {
        return m_afTuple[2];
    }

    /** Return w-value
     * @return w-value
     */
    public float W ()
    {
        return m_afTuple[3];
    }

    /** Set x-value
     * @param fValue x-value
     */
    public void X (float fValue)
    {
         m_afTuple[0] = fValue;
    }

    /** Set y-value
     * @param fValue y-value
     */
    public void Y (float fValue)
    {
        m_afTuple[1] = fValue;
    }

    /** Set z-value
     * @param fValue z-value
     */
    public void Z (float fValue)
    {
        m_afTuple[2] = fValue;
    }

    /** Set w-value
     * @param fValue w-value
     */
    public void W (float fValue)
    {
        m_afTuple[3] = fValue;
    }

    /** Compute length this vector:
     * @return length this vector
     */
    public float Length ()
    {
        return (float)Math.sqrt(
                                m_afTuple[0]*m_afTuple[0] +
                                m_afTuple[1]*m_afTuple[1] +
                                m_afTuple[2]*m_afTuple[2] +
                                m_afTuple[3]*m_afTuple[3]);
    }

    /** Compute squared-length this vector:
     * @return squared-length this vector
     */
    public float SquaredLength ()
    {
        return
            m_afTuple[0]*m_afTuple[0] +
            m_afTuple[1]*m_afTuple[1] +
            m_afTuple[2]*m_afTuple[2] +
            m_afTuple[3]*m_afTuple[3];
    }

    /** Compute dot-product of this vector with input vector:
     * @return dot-product of this vector with input vector:
     */
    public float Dot ( Vector4f rkV)
    {
        return
            m_afTuple[0]*rkV.m_afTuple[0] +
            m_afTuple[1]*rkV.m_afTuple[1] +
            m_afTuple[2]*rkV.m_afTuple[2] +
            m_afTuple[3]*rkV.m_afTuple[3];
    }

    /** Normalize this vector, return original length:
     * @return original length:
     */
    public float Normalize ()
    {
        float fLength = Length();

        if (fLength > Mathf.ZERO_TOLERANCE)
        {
            float fInvLength = ((float)1.0)/fLength;
            m_afTuple[0] *= fInvLength;
            m_afTuple[1] *= fInvLength;
            m_afTuple[2] *= fInvLength;
            m_afTuple[3] *= fInvLength;
        }
        else
        {
            fLength = (float)0.0;
            m_afTuple[0] = (float)0.0;
            m_afTuple[1] = (float)0.0;
            m_afTuple[2] = (float)0.0;
            m_afTuple[3] = (float)0.0;
        }

        return fLength;
    }

    /** Vector data: */
    private float[] m_afTuple = new float[4];
}
