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

public class Vector2f
{
    /** Zero vector: */
    public static final Vector2f ZERO = new Vector2f(0.0f,0.0f);    // (0,0)
    /** Unit-X vector: */
    public static final Vector2f UNIT_X = new Vector2f(1.0f,0.0f);  // (1,0)
    /** Unit-Y vector: */
    public static final Vector2f UNIT_Y = new Vector2f(0.0f,1.0f);  // (0,1)
    /** One vector: */
    public static final Vector2f ONE = new Vector2f(1.0f,1.0f);     // (1,1)

    /** construction, uninitialized */
    public Vector2f () {}  

    /** construction
     * @param fX, x-value
     * @param fY, y-value
     */
    public Vector2f (float fX, float fY)
    {
        m_afTuple[0] = fX;
        m_afTuple[1] = fY;
    }

    /** construction
     * @param afTuple, x,y values
     */
    public Vector2f (final float[] afTuple)
    {
        m_afTuple[0] = afTuple[0];
        m_afTuple[1] = afTuple[1];
    }

    /** copy construction
     * @param rkV, vector to copy.
     */
    public Vector2f (final Vector2f rkV)
    {
        m_afTuple[0] = rkV.m_afTuple[0];
        m_afTuple[1] = rkV.m_afTuple[1];
    }

    /**
     * delete memory
     */
    public void dispose()
    {
        if ( (this == Vector2f.ZERO) || (this ==  Vector2f.UNIT_X)
                || (this ==  Vector2f.UNIT_Y)
                || (this ==  Vector2f.ONE))
        {
            System.err.println( "Vector2f" );
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

    /** Return y-value
     * @return y-value
     */
    public final float Y ()
    {
        return m_afTuple[1];
    }

    /** Set x-value
     * @param fValue x-value
     */
    public final void X (float fValue)
    {
        m_afTuple[0] = fValue;
    }

    /** Set y-value
     * @param fValue y-value
     */
    public final void Y (float fValue)
    {
        m_afTuple[1] = fValue;
    }

    /** construction
     * @param fX, x-value
     * @param fY, y-value
     */
    public final void SetData (float fX, float fY)
    {
        m_afTuple[0] = fX;
        m_afTuple[1] = fY;
    }
    
    public final void SetData (final Vector2f rkV)
    {
        m_afTuple[0] = rkV.m_afTuple[0];
        m_afTuple[1] = rkV.m_afTuple[1];
    }
    
    /** Add the input vector to this, return result, this vector is
     * unchanged:
     * @param rkV, input vector to add to this
     * @return this+rkV
     */
    public Vector2f add( final Vector2f rkV )
    {
        return new Vector2f(
                           m_afTuple[0]+rkV.m_afTuple[0],
                           m_afTuple[1]+rkV.m_afTuple[1]);
    }
    
    

    public void addEquals( final Vector2f rkV )
    {
        m_afTuple[0] += rkV.m_afTuple[0];
        m_afTuple[1] += rkV.m_afTuple[1];
    }

    /** Subtract the input vector from this, return result, this vector is
     * unchanged:
     * @param rkV, input vector to subtract from this
     * @return this-rkV
     */
    public Vector2f sub( final Vector2f rkV )
    {
        return new Vector2f(
                           m_afTuple[0]-rkV.m_afTuple[0],
                           m_afTuple[1]-rkV.m_afTuple[1]);
    }
    
    public void sub( final Vector2f rkV, Vector2f kResult )
    {
        kResult.SetData(m_afTuple[0]-rkV.m_afTuple[0],
                m_afTuple[1]-rkV.m_afTuple[1]);
    }
    
    public void subEquals( final Vector2f rkV )
    {
        m_afTuple[0] -= rkV.m_afTuple[0];
        m_afTuple[1] -= rkV.m_afTuple[1];
    }

    /** Scale this vector by input, return result, this vector is unchanged:
     * @param fScalar, scale value
     * @return this*fScalar
     */
    public Vector2f scale( float fScalar )
    {
        return new Vector2f(
                            fScalar*m_afTuple[0],
                            fScalar*m_afTuple[1]);
    }
    
    public void scaleEquals( float fScalar )
    {
        m_afTuple[0] *= fScalar;
        m_afTuple[1] *= fScalar;
    }


    /** Compute length this vector:
     * @return length this vector
     */
    public float Length ()
    {
        return (float)Math.sqrt(
                         m_afTuple[0]*m_afTuple[0] +
                         m_afTuple[1]*m_afTuple[1]);
    }

    /** Compute squared-length this vector:
     * @return squared-length this vector
     */
    public float SquaredLength ()
    {
        return
            m_afTuple[0]*m_afTuple[0] +
            m_afTuple[1]*m_afTuple[1];
    }

    /** Compute dot-product of this vector with input vector:
     * @return dot-product of this vector with input vector:
     */
    public float Dot (final Vector2f rkV) 
    {
        return
            m_afTuple[0]*rkV.m_afTuple[0] +
            m_afTuple[1]*rkV.m_afTuple[1];
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
        }
        else
        {
            fLength = (float)0.0;
            m_afTuple[0] = (float)0.0;
            m_afTuple[1] = (float)0.0;
        }

        return fLength;
    }
    

    /**
     * Returns Cross((x,y,0),(V.x,V.y,0)) = x*V.y - y*V.x.
     *
     * @param   kV  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float Cross(Vector2f kV) {
        return (m_afTuple[0] * kV.m_afTuple[1]) - (m_afTuple[1] * kV.m_afTuple[0]);
    }
    
    /**
     * Set this vector to the 'perp' of the specified vector.
     *
     * @param  kVector  DOCUMENT ME!
     */
    public void Perp(Vector2f kVector) {
        SetData(kVector.m_afTuple[1], -kVector.m_afTuple[0]);
    }
    
    /** Vector data: */
    private float[] m_afTuple = new float[2];
}
