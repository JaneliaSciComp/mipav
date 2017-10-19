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

public class Vector3f
{
    /** Zero vector: */
    public static final Vector3f ZERO = new Vector3f(0.0f,0.0f,0.0f);
    /** Unit-X vector: */
    public static final Vector3f UNIT_X = new Vector3f(1.0f,0.0f,0.0f);
    /** Unit-Y vector: */
    public static final Vector3f UNIT_Y = new Vector3f(0.0f,1.0f,0.0f);
    /** Unit-Z vector: */
    public static final Vector3f UNIT_Z = new Vector3f(0.0f,0.0f,1.0f);
    /** negative unit-Z vector: */
    public static final Vector3f UNIT_Z_NEG = new Vector3f(0.0f,0.0f,-1.0f);
    /** One vector: */
    public static final Vector3f ONE = new Vector3f(1.0f,1.0f,1.0f);

    /** construction, uninitialized */
    public Vector3f () { }  // uninitialized

    /** construction
     * @param fX, x-value
     * @param fY, y-value
     * @param fZ, z-value
     */
    public Vector3f ( float fX, float fY, float fZ )
    {
        m_afTuple[0] = fX;
        m_afTuple[1] = fY;
        m_afTuple[2] = fZ;
    }

    /** construction
     * @param afTuple, x,y,z values
     */
    public Vector3f ( float[] afTuple )
    {
        m_afTuple[0] = afTuple[0];
        m_afTuple[1] = afTuple[1];
        m_afTuple[2] = afTuple[2];
    }

    /** copy construction
     * @param rkV, vector to copy.
     */
    public Vector3f (Vector3f rkV)
    {
        m_afTuple[0] = rkV.m_afTuple[0];
        m_afTuple[1] = rkV.m_afTuple[1];
        m_afTuple[2] = rkV.m_afTuple[2];
    }

    /**
     * delete memory
     */
    public void dispose()
    {
        if ( (this == Vector3f.ZERO) || (this ==  Vector3f.UNIT_X)
                || (this ==  Vector3f.UNIT_Y)
                || (this ==  Vector3f.UNIT_Z)
                || (this ==  Vector3f.UNIT_Z_NEG)
                || (this ==  Vector3f.ONE))
        {
            System.err.println( "Vector3f" );
        }
        m_afTuple = null;
    }

    /** Return x-value
     * @return x-value
     */
    public final float X ()
    {
        return m_afTuple[0];
    }

    /** Set x-value
     * @param fX x-value
     */
    public final void X ( float fX)
    {
        m_afTuple[0] = fX;
    }

    /** Return y-value
     * @return y-value
     */
    public final float Y ()
    {
        return m_afTuple[1];
    }

    /** Set y-value
     * @param fY y-value
     */
    public final void Y ( float fY)
    {
        m_afTuple[1] = fY;
    }

    /** Return z-value
     * @return z-value
     */
    public final float Z ()
    {
        return m_afTuple[2];
    }

    /** Set z-value
     * @param fZ z-value
     */
    public final void Z ( float fZ )
    {
        m_afTuple[2] = fZ;
    }
    
    /**
     */
    public final float GetData ( int i )
    {
        return m_afTuple[i];
    }    
    
    /**
     */
    public final void SetData ( int i, float fValue )
    {
        m_afTuple[i] = fValue;
    }

    
    /** construction
     * @param fX, x-value
     * @param fY, y-value
     * @param fZ, z-value
     */
    public void SetData ( float fX, float fY, float fZ )
    {
        m_afTuple[0] = fX;
        m_afTuple[1] = fY;
        m_afTuple[2] = fZ;
    }

    
    /** Add the input vector to this, return result, this vector is
     * unchanged:
     * @param rkV, input vector to add to this
     * @return this+rkV
     */
    public void add (Vector3f rkV, Vector3f kResult)
    {
        kResult.SetData(
                        m_afTuple[0]+rkV.m_afTuple[0],
                        m_afTuple[1]+rkV.m_afTuple[1],
                        m_afTuple[2]+rkV.m_afTuple[2]);
    }
    
    /** Add the input vector to this, return result, this vector is
     * unchanged:
     * @param rkV, input vector to add to this
     * @return this+rkV
     */
    public Vector3f add (Vector3f rkV)
    {
        return new Vector3f(
                        m_afTuple[0]+rkV.m_afTuple[0],
                        m_afTuple[1]+rkV.m_afTuple[1],
                        m_afTuple[2]+rkV.m_afTuple[2]);
    }


    /** Subtract the input vector from this, return result, this vector is
     * unchanged:
     * @param rkV, input vector to subtract from this
     * @return this-rkV
     */
    public void sub( Vector3f rkV, Vector3f kResult )
    {
        kResult.SetData(
                        m_afTuple[0]-rkV.m_afTuple[0],
                        m_afTuple[1]-rkV.m_afTuple[1],
                        m_afTuple[2]-rkV.m_afTuple[2]);
    }
    
    /** Subtract the input vector from this, return result, this vector is
     * unchanged:
     * @param rkV, input vector to subtract from this
     * @return this-rkV
     */
    public Vector3f sub( Vector3f rkV )
    {
        return new Vector3f(
                        m_afTuple[0]-rkV.m_afTuple[0],
                        m_afTuple[1]-rkV.m_afTuple[1],
                        m_afTuple[2]-rkV.m_afTuple[2]);
    }
    
    /** Multiply the input vector to this, return result, this vector is
     * unchanged:
     * @param rkV, input vector to multiply to this
     * @return this*rkV
     */
    public void mult (Vector3f rkV, Vector3f kResult)
    {
        kResult.SetData( m_afTuple[0] * rkV.m_afTuple[0],
                         m_afTuple[1] * rkV.m_afTuple[1],
                         m_afTuple[2] * rkV.m_afTuple[2] );
    }

    /** Multiply the input vector to this, this vector is
     * changed:
     * @param rkV, input vector to multiply to this
     */
    public void multEquals (Vector3f rkV)
    {
        m_afTuple[0] = m_afTuple[0] * rkV.m_afTuple[0];
        m_afTuple[1] = m_afTuple[1] * rkV.m_afTuple[1];
        m_afTuple[2] = m_afTuple[2] * rkV.m_afTuple[2];
    }

    /** Invert this vector, return result, this vector is unchanged:
     * @return 1/this
     */
    public void invert ( Vector3f kResult )
    {
        kResult.SetData(
                        (m_afTuple[0] == 0) ? 0 : 1.0f/m_afTuple[0],
                        (m_afTuple[1] == 0) ? 0 : 1.0f/m_afTuple[1],
                        (m_afTuple[2] == 0) ? 0 : 1.0f/m_afTuple[2]);
    }

    /** Scale this vector by input, return result, this vector is unchanged:
     * @param fScalar, scale value
     * @return this*fScalar
     */
    public void scale( float fScalar, Vector3f kResult )
    {
        kResult.SetData(
                        fScalar*m_afTuple[0],
                        fScalar*m_afTuple[1],
                        fScalar*m_afTuple[2]);
    }
    
    /** Scale this vector by input, return result, this vector is unchanged:
     * @param fScalar, scale value
     * @return this*fScalar
     */
    public Vector3f scale( float fScalar )
    {
        return new Vector3f(
                        fScalar*m_afTuple[0],
                        fScalar*m_afTuple[1],
                        fScalar*m_afTuple[2]);
    }

    /** Negate this vector, return result, this vector is unchanged:
     * @return -1*this
     */
    public void neg ( Vector3f kResult )
    {
        kResult.SetData(
                        -m_afTuple[0],
                        -m_afTuple[1],
                        -m_afTuple[2]);
    }

    /** Negate this vector.
     */
    public void negEquals ()
    {
        m_afTuple[0] = -m_afTuple[0];
        m_afTuple[1] = -m_afTuple[1];
        m_afTuple[2] = -m_afTuple[2];
    }
    
    /** copy into this vector.
     * @param rkV, vector to copy.
     */
    public void SetData (Vector3f rkV)
    {
        m_afTuple[0] = rkV.m_afTuple[0];
        m_afTuple[1] = rkV.m_afTuple[1];
        m_afTuple[2] = rkV.m_afTuple[2];
    }

    /** Return true if the input Vector3f is equal to this.
     * @param rkV, input vector to compare to this
     * @return true if the input Vector3f is equal to this, false otherwise.
     */
    public boolean isEqual (Vector3f rkV)
    {
        return ( (Math.abs(m_afTuple[0] - rkV.m_afTuple[0]) < Mathf.ZERO_TOLERANCE) &&
                 (Math.abs(m_afTuple[1] - rkV.m_afTuple[1]) < Mathf.ZERO_TOLERANCE) &&
                 (Math.abs(m_afTuple[2] - rkV.m_afTuple[2]) < Mathf.ZERO_TOLERANCE) );
    }
 
    /** Return true if the input Vector3f is less than to this.
     * @param rkV, input vector to compare to this
     * @return true if the input Vector3f is less than to this, false otherwise.
     */
    public boolean isLessThan (Vector3f rkV)
    {
        return ( (Math.abs(m_afTuple[0] - rkV.m_afTuple[0]) < Mathf.ZERO_TOLERANCE) &&
                 (Math.abs(m_afTuple[1] - rkV.m_afTuple[1]) < Mathf.ZERO_TOLERANCE) &&
                 (Math.abs(m_afTuple[2] - rkV.m_afTuple[2]) < Mathf.ZERO_TOLERANCE) );
    }


    /** Add the input vector to this, this vector is changed.
     * @param rkV, input vector to add to this
     */
    public void addEquals (Vector3f rkV)
    {
        m_afTuple[0] += rkV.m_afTuple[0];
        m_afTuple[1] += rkV.m_afTuple[1];
        m_afTuple[2] += rkV.m_afTuple[2];
    }
    
    /** Add the input vector to this, this vector is changed.
     * @param rkV, input vector to add to this
     */
    public void addEquals (float fX, float fY, float fZ)
    {
        m_afTuple[0] += fX;
        m_afTuple[1] += fY;
        m_afTuple[2] += fZ;
    }

    /** Subtract the input vector from this, this vector is changed.
     * @param rkV, input vector to multiply to this
     */
    public void subEquals (Vector3f rkV)
    {
        m_afTuple[0] -= rkV.m_afTuple[0];
        m_afTuple[1] -= rkV.m_afTuple[1];
        m_afTuple[2] -= rkV.m_afTuple[2];
    }

    /** Scale this vector by input, this vector is changed.
     * @param fScalar, scale value
     */
    public void scaleEquals( float fScalar )
    {
        m_afTuple[0] *= fScalar;
        m_afTuple[1] *= fScalar;
        m_afTuple[2] *= fScalar;
    }
    
    /** Divide this vector by input, this vector is changed.
     * @param fScalar, scale value
     */
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

    /** Divide this vector by input, return result, this vector is unchanged:
     * @param fScalar, scale value
     * @return this/fScalar
     */
    public void div( float fScalar, Vector3f kQuot )
    {
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
    }

    /** Compute length this vector:
     * @return length this vector
     */
    public float Length ()
    {
        return (float)Math.sqrt(
                         m_afTuple[0]*m_afTuple[0] +
                         m_afTuple[1]*m_afTuple[1] +
                         m_afTuple[2]*m_afTuple[2]);
    }

    /** Compute squared-length this vector:
     * @return squared-length this vector
     */
    public float SquaredLength ()
    {
        return
            m_afTuple[0]*m_afTuple[0] +
            m_afTuple[1]*m_afTuple[1] +
            m_afTuple[2]*m_afTuple[2];
    }

    /** Compute dot-product of this vector with input vector:
     * @return dot-product of this vector with input vector:
     */
    public float Dot (Vector3f rkV)
    {
        return
            m_afTuple[0]*rkV.m_afTuple[0] +
            m_afTuple[1]*rkV.m_afTuple[1] +
            m_afTuple[2]*rkV.m_afTuple[2];
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
     * @param rkV, vector to cross with this
     * @return cross-product this^rkV
     */
    public void Cross (Vector3f rkV, Vector3f kResult)
    {
        kResult.SetData(
                        m_afTuple[1]*rkV.m_afTuple[2] - m_afTuple[2]*rkV.m_afTuple[1],
                        m_afTuple[2]*rkV.m_afTuple[0] - m_afTuple[0]*rkV.m_afTuple[2],
                        m_afTuple[0]*rkV.m_afTuple[1] - m_afTuple[1]*rkV.m_afTuple[0]);
    }

    /** The cross products are computed using the right-handed rule.  Be aware
     * that some graphics APIs use a left-handed rule.  If you have to compute
     * a cross product with these functions and send the result to the API
     * that expects left-handed, you will need to change sign on the vector
     * (replace each component value c by -c).
     * @param rkV, vector to cross with this
     * @return cross-product this^rkV
     */
    public Vector3f Cross (Vector3f rkV)
    {
        return new Vector3f(
                        m_afTuple[1]*rkV.m_afTuple[2] - m_afTuple[2]*rkV.m_afTuple[1],
                        m_afTuple[2]*rkV.m_afTuple[0] - m_afTuple[0]*rkV.m_afTuple[2],
                        m_afTuple[0]*rkV.m_afTuple[1] - m_afTuple[1]*rkV.m_afTuple[0]);
    }

    /** The cross products are computed using the right-handed rule.  Be aware
     * that some graphics APIs use a left-handed rule.  If you have to compute
     * a cross product with these functions and send the result to the API
     * that expects left-handed, you will need to change sign on the vector
     * (replace each component value c by -c).
     * @param rkV, vector to cross with this
     * @return cross-product this^rkV
     */
    public void CrossEquals (Vector3f rkV)
    {
        float fX = m_afTuple[1]*rkV.m_afTuple[2] - m_afTuple[2]*rkV.m_afTuple[1];
        float fY = m_afTuple[2]*rkV.m_afTuple[0] - m_afTuple[0]*rkV.m_afTuple[2];
        float fZ = m_afTuple[0]*rkV.m_afTuple[1] - m_afTuple[1]*rkV.m_afTuple[0];
        SetData( fX, fY, fZ );
    }
    
    
    /** The cross products are computed using the right-handed rule.  Be aware
     * that some graphics APIs use a left-handed rule.  If you have to compute
     * a cross product with these functions and send the result to the API
     * that expects left-handed, you will need to change sign on the vector
     * (replace each component value c by -c).
     * @param rkV, vector to cross with this
     * @return normalized cross-product this^rkV 
     */
    public void UnitCross (Vector3f rkV, Vector3f kResult)
    {
        kResult.SetData(
                        m_afTuple[1]*rkV.m_afTuple[2] - m_afTuple[2]*rkV.m_afTuple[1],
                        m_afTuple[2]*rkV.m_afTuple[0] - m_afTuple[0]*rkV.m_afTuple[2],
                        m_afTuple[0]*rkV.m_afTuple[1] - m_afTuple[1]*rkV.m_afTuple[0]);
        kResult.Normalize();
    }

    /** The cross products are computed using the right-handed rule.  Be aware
     * that some graphics APIs use a left-handed rule.  If you have to compute
     * a cross product with these functions and send the result to the API
     * that expects left-handed, you will need to change sign on the vector
     * (replace each component value c by -c).
     * @param rkV, vector to cross with this
     * @return normalized cross-product this^rkV 
     */
    public void UnitCrossEquals (Vector3f rkV)
    {
        float fX = m_afTuple[1]*rkV.m_afTuple[2] - m_afTuple[2]*rkV.m_afTuple[1];
        float fY = m_afTuple[2]*rkV.m_afTuple[0] - m_afTuple[0]*rkV.m_afTuple[2];
        float fZ = m_afTuple[0]*rkV.m_afTuple[1] - m_afTuple[1]*rkV.m_afTuple[0];
        SetData( fX, fY, fZ );
        Normalize();
    }
    
    /** Input W must be a nonzero vector. The output is an orthonormal basis
     * {U,V,W}.  The input W is normalized by this function.  If you know
     * W is already unit length, use GenerateComplementBasis to compute U
     * and V.
     * @param rkU, U
     * @param rkV, V
     * @param rkW, W
     * @return orthonormal basis (U,V,W)
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
     * @param rkU, U
     * @param rkV, V
     * @param rkW, W
     * @return orthonormal basis (U,V,W)
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


    public static void ComputeExtremes (int iVQuantity, final Vector3f[] akPoint,
                                        Vector3f rkMin, Vector3f rkMax)
    {
        if ((iVQuantity <= 0) || (akPoint == null))
        {
            return;
        }

        rkMin.SetData(akPoint[0]);
        rkMax.SetData(akPoint[0]);
        for (int i = 1; i < iVQuantity; i++)
        {
            Vector3f rkPoint = akPoint[i];
            for (int j = 0; j < 3; j++)
            {
                if (rkPoint.m_afTuple[j] < rkMin.m_afTuple[j])
                {
                    rkMin.m_afTuple[j] = rkPoint.m_afTuple[j];
                }
                else if (rkPoint.m_afTuple[j] > rkMax.m_afTuple[j])
                {
                    rkMax.m_afTuple[j] = rkPoint.m_afTuple[j];
                }
            }
        }
    }


    /** Vector data: */
    private float[] m_afTuple = new float[]{0,0,0};
}