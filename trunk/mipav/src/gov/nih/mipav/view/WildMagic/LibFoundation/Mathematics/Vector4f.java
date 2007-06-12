// Geometric Tools, Inc.
// http://www.geometrictools.com
// Copyright (c) 1998-2006.  All Rights Reserved
//
// The Wild Magic Version 4 Foundation Library source code is supplied
// under the terms of the license agreement
//     http://www.geometrictools.com/License/Wm4FoundationLicense.pdf
// and may not be copied or disclosed except in accordance with the terms
// of that agreement.
//
// Version: 4.0.1 (2006/07/25)

package gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics;

public class Vector4f
{
    // special vectors
    public static final Vector4f ZERO = new Vector4f(0.0f,0.0f,0.0f,0.0f);
    public static final Vector4f UNIT_X = new Vector4f(1.0f,0.0f,0.0f,0.0f);  // (1,0,0,0)
    public static final Vector4f UNIT_Y = new Vector4f(0.0f,1.0f,0.0f,0.0f);  // (0,1,0,0)
    public static final Vector4f UNIT_Z = new Vector4f(0.0f,0.0f,1.0f,0.0f);  // (0,0,1,0)
    public static final Vector4f UNIT_W = new Vector4f(0.0f,0.0f,0.0f,1.0f);  // (0,0,0,1)
    public static final Vector4f ONE = new Vector4f(1.0f,1.0f,1.0f,1.0f);     // (1,1,1,1)

    // construction
    public Vector4f () {}  // uninitialized
    public Vector4f (float fX, float fY, float fZ, float fW)
    {
        m_afTuple[0] = fX;
        m_afTuple[1] = fY;
        m_afTuple[2] = fZ;
        m_afTuple[3] = fW;
    }

    public Vector4f ( float[] afTuple)
    {
        m_afTuple[0] = afTuple[0];
        m_afTuple[1] = afTuple[1];
        m_afTuple[2] = afTuple[2];
        m_afTuple[3] = afTuple[3];
    }

    public Vector4f ( Vector4f rkV)
    {
        m_afTuple[0] = rkV.m_afTuple[0];
        m_afTuple[1] = rkV.m_afTuple[1];
        m_afTuple[2] = rkV.m_afTuple[2];
        m_afTuple[3] = rkV.m_afTuple[3];
    }

    // coordinate access
    public float X ()
    {
        return m_afTuple[0];
    }

    public float Y ()
    {
        return m_afTuple[1];
    }

    public float Z ()
    {
        return m_afTuple[2];
    }

    public float W ()
    {
        return m_afTuple[3];
    }

    // vector operations
    public float Length ()
    {
        return (float)Math.sqrt(
                                m_afTuple[0]*m_afTuple[0] +
                                m_afTuple[1]*m_afTuple[1] +
                                m_afTuple[2]*m_afTuple[2] +
                                m_afTuple[3]*m_afTuple[3]);
    }

    public float SquaredLength ()
    {
        return
            m_afTuple[0]*m_afTuple[0] +
            m_afTuple[1]*m_afTuple[1] +
            m_afTuple[2]*m_afTuple[2] +
            m_afTuple[3]*m_afTuple[3];
    }

    public float Dot ( Vector4f rkV)
    {
        return
            m_afTuple[0]*rkV.m_afTuple[0] +
            m_afTuple[1]*rkV.m_afTuple[1] +
            m_afTuple[2]*rkV.m_afTuple[2] +
            m_afTuple[3]*rkV.m_afTuple[3];
    }

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

    private float[] m_afTuple = new float[4];
}
