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
// Version: 4.0.2 (2006/07/25)

package gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics;

public class Vector3f
{
    // special vectors
    public static final Vector3f ZERO = new Vector3f(0.0f,0.0f,0.0f);
    public static final Vector3f UNIT_X = new Vector3f(1.0f,0.0f,0.0f);
    public static final Vector3f UNIT_Y = new Vector3f(0.0f,1.0f,0.0f);
    public static final Vector3f UNIT_Z = new Vector3f(0.0f,0.0f,1.0f);
    public static final Vector3f UNIT_Z_NEG = new Vector3f(0.0f,0.0f,-1.0f);
    public static final Vector3f ONE = new Vector3f(1.0f,1.0f,1.0f);

    // construction
    public Vector3f () { }  // uninitialized

    public Vector3f ( float fX, float fY, float fZ )
    {
        m_afTuple[0] = fX;
        m_afTuple[1] = fY;
        m_afTuple[2] = fZ;
    }


    public Vector3f ( float[] afTuple )
    {
        m_afTuple[0] = afTuple[0];
        m_afTuple[1] = afTuple[1];
        m_afTuple[2] = afTuple[2];
    }

    public Vector3f (Vector3f rkV)
    {
        m_afTuple[0] = rkV.m_afTuple[0];
        m_afTuple[1] = rkV.m_afTuple[1];
        m_afTuple[2] = rkV.m_afTuple[2];
    }

    public float X ()
    {
        return m_afTuple[0];
    }

    public void X ( float fX)
    {
        m_afTuple[0] = fX;
    }

    public float Y ()
    {
        return m_afTuple[1];
    }
    public void Y ( float fY)
    {
        m_afTuple[1] = fY;
    }

    public float Z ()
    {
        return m_afTuple[2];
    }
    public void Z ( float fZ )
    {
        m_afTuple[2] = fZ;
    }

    // arithmetic operations
    public Vector3f add (Vector3f rkV)
    {
        return new Vector3f(
                            m_afTuple[0]+rkV.m_afTuple[0],
                            m_afTuple[1]+rkV.m_afTuple[1],
                            m_afTuple[2]+rkV.m_afTuple[2]);
    }

    public Vector3f sub( Vector3f rkV)
    {
        return new Vector3f(
                            m_afTuple[0]-rkV.m_afTuple[0],
                            m_afTuple[1]-rkV.m_afTuple[1],
                            m_afTuple[2]-rkV.m_afTuple[2]);
    }
    public Vector3f scale( float fScalar )
    {
        return new Vector3f(
                            fScalar*m_afTuple[0],
                            fScalar*m_afTuple[1],
                            fScalar*m_afTuple[2]);
    }

    public Vector3f neg ()
    {
        return new Vector3f(
                            -m_afTuple[0],
                            -m_afTuple[1],
                            -m_afTuple[2]);
    }

    // arithmetic updates
    public void addEquals (Vector3f rkV)
    {
        m_afTuple[0] += rkV.m_afTuple[0];
        m_afTuple[1] += rkV.m_afTuple[1];
        m_afTuple[2] += rkV.m_afTuple[2];
    }

    public void subEquals (Vector3f rkV)
    {
        m_afTuple[0] -= rkV.m_afTuple[0];
        m_afTuple[1] -= rkV.m_afTuple[1];
        m_afTuple[2] -= rkV.m_afTuple[2];
    }

    public void scaleEquals( float fScalar )
    {
        m_afTuple[0] *= fScalar;
        m_afTuple[1] *= fScalar;
        m_afTuple[2] *= fScalar;
    }
    
    public void divEquals( float fScalar )
    {
        if (fScalar != (float)0.0)
        {
            float fInvScalar = ((float)1.0)/fScalar;
            m_afTuple[0] *= fInvScalar;
            m_afTuple[1] *= fInvScalar;
            m_afTuple[2] *= fInvScalar;
        }
        else
        {
            m_afTuple[0] = Float.MAX_VALUE;
            m_afTuple[1] = Float.MAX_VALUE;
            m_afTuple[2] = Float.MAX_VALUE;
        }
    }

    public Vector3f div( float fScalar )
    {
        Vector3f kQuot = new Vector3f();

        if (fScalar != (float)0.0)
        {
            float fInvScalar = ((float)1.0)/fScalar;
            kQuot.m_afTuple[0] = fInvScalar*m_afTuple[0];
            kQuot.m_afTuple[1] = fInvScalar*m_afTuple[1];
            kQuot.m_afTuple[2] = fInvScalar*m_afTuple[2];
        }
        else
        {
            kQuot.m_afTuple[0] = Float.MAX_VALUE;
            kQuot.m_afTuple[1] = Float.MAX_VALUE;
            kQuot.m_afTuple[2] = Float.MAX_VALUE;
        }

        return kQuot;
    }

    // vector operations
    public float Length ()
    {
        return (float)Math.sqrt(
                         m_afTuple[0]*m_afTuple[0] +
                         m_afTuple[1]*m_afTuple[1] +
                         m_afTuple[2]*m_afTuple[2]);
    }


    public float SquaredLength ()
    {
        return
            m_afTuple[0]*m_afTuple[0] +
            m_afTuple[1]*m_afTuple[1] +
            m_afTuple[2]*m_afTuple[2];
    }

    public float Dot (Vector3f rkV)
    {
        return
            m_afTuple[0]*rkV.m_afTuple[0] +
            m_afTuple[1]*rkV.m_afTuple[1] +
            m_afTuple[2]*rkV.m_afTuple[2];
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
        }
        else
        {
            fLength = (float)0.0;
            m_afTuple[0] = (float)0.0;
            m_afTuple[1] = (float)0.0;
            m_afTuple[2] = (float)0.0;
        }

        return fLength;
    }


    /** The cross products are computed using the right-handed rule.  Be aware
     * that some graphics APIs use a left-handed rule.  If you have to compute
     * a cross product with these functions and send the result to the API
     * that expects left-handed, you will need to change sign on the vector
     * (replace each component value c by -c).
     */
    public Vector3f Cross (Vector3f rkV)
    {
        return new Vector3f(
                           m_afTuple[1]*rkV.m_afTuple[2] - m_afTuple[2]*rkV.m_afTuple[1],
                           m_afTuple[2]*rkV.m_afTuple[0] - m_afTuple[0]*rkV.m_afTuple[2],
                           m_afTuple[0]*rkV.m_afTuple[1] - m_afTuple[1]*rkV.m_afTuple[0]);
    }

    public Vector3f UnitCross (Vector3f rkV)
    {
        Vector3f kCross = new Vector3f(
                                       m_afTuple[1]*rkV.m_afTuple[2] - m_afTuple[2]*rkV.m_afTuple[1],
                                       m_afTuple[2]*rkV.m_afTuple[0] - m_afTuple[0]*rkV.m_afTuple[2],
                                       m_afTuple[0]*rkV.m_afTuple[1] - m_afTuple[1]*rkV.m_afTuple[0]);
        kCross.Normalize();
        return kCross;
    }

    /** Input W must be a nonzero vector. The output is an orthonormal basis
     * {U,V,W}.  The input W is normalized by this function.  If you know
     * W is already unit length, use GenerateComplementBasis to compute U
     * and V.
     */
    public static void GenerateOrthonormalBasis (Vector3f rkU, Vector3f rkV,
                                                 Vector3f rkW)
    {
        rkW.Normalize();
        GenerateComplementBasis(rkU,rkV,rkW);
    }

    /** Input W must be a unit-length vector.  The output vectors {U,V} are
     * unit length and mutually perpendicular, and {U,V,W} is an orthonormal
     * basis.
     */
    public static void GenerateComplementBasis (Vector3f rkU, Vector3f rkV,
                                                Vector3f rkW)
    {
        float fInvLength;

        if (Math.abs(rkW.m_afTuple[0]) >=
            Math.abs(rkW.m_afTuple[1]) )
        {
            // W.x or W.z is the largest magnitude component, swap them
            fInvLength = Mathf.InvSqrt(rkW.m_afTuple[0]*rkW.m_afTuple[0] +
                                             rkW.m_afTuple[2]*rkW.m_afTuple[2]);
            rkU.m_afTuple[0] = -rkW.m_afTuple[2]*fInvLength;
            rkU.m_afTuple[1] = (float)0.0;
            rkU.m_afTuple[2] = +rkW.m_afTuple[0]*fInvLength;
            rkV.m_afTuple[0] = rkW.m_afTuple[1]*rkU.m_afTuple[2];
            rkV.m_afTuple[1] = rkW.m_afTuple[2]*rkU.m_afTuple[0] -
                rkW.m_afTuple[0]*rkU.m_afTuple[2];
            rkV.m_afTuple[2] = -rkW.m_afTuple[1]*rkU.m_afTuple[0];
        }
        else
        {
            // W.y or W.z is the largest magnitude component, swap them
            fInvLength = Mathf.InvSqrt(rkW.m_afTuple[1]*rkW.m_afTuple[1] +
                                             rkW.m_afTuple[2]*rkW.m_afTuple[2]);
            rkU.m_afTuple[0] = (float)0.0;
            rkU.m_afTuple[1] = +rkW.m_afTuple[2]*fInvLength;
            rkU.m_afTuple[2] = -rkW.m_afTuple[1]*fInvLength;
            rkV.m_afTuple[0] = rkW.m_afTuple[1]*rkU.m_afTuple[2] -
                rkW.m_afTuple[2]*rkU.m_afTuple[1];
            rkV.m_afTuple[1] = -rkW.m_afTuple[0]*rkU.m_afTuple[2];
            rkV.m_afTuple[2] = rkW.m_afTuple[0]*rkU.m_afTuple[1];
        }
    }

    private float[] m_afTuple = new float[3];
}
