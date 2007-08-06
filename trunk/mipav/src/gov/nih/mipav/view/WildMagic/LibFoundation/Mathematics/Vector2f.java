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
    // special vectors
    public static final Vector2f ZERO = new Vector2f(0.0f,0.0f);    // (0,0)
    public static final Vector2f UNIT_X = new Vector2f(1.0f,0.0f);  // (1,0)
    public static final Vector2f UNIT_Y = new Vector2f(0.0f,1.0f);  // (0,1)
    public static final Vector2f ONE = new Vector2f(1.0f,1.0f);     // (1,1)

    // construction
    public Vector2f () {}  // uninitialized
    public Vector2f (float fX, float fY)
    {
        m_afTuple[0] = fX;
        m_afTuple[1] = fY;
    }

    public Vector2f (final float[] afTuple)
    {
        m_afTuple[0] = afTuple[0];
        m_afTuple[1] = afTuple[1];
    }

    public Vector2f (final Vector2f rkV)
    {
        m_afTuple[0] = rkV.m_afTuple[0];
        m_afTuple[1] = rkV.m_afTuple[1];
    }


    public float X ()
    {
        return m_afTuple[0];
    }

    public float Y ()
    {
        return m_afTuple[1];
    }

    public void X (float fValue)
    {
        m_afTuple[0] = fValue;
    }

    public void Y (float fValue)
    {
        m_afTuple[1] = fValue;
    }

    // arithmetic operations
    public Vector2f add( final Vector2f rkV )
    {
        return new Vector2f(
                           m_afTuple[0]+rkV.m_afTuple[0],
                           m_afTuple[1]+rkV.m_afTuple[1]);
    }

    public Vector2f sub( final Vector2f rkV )
    {
        return new Vector2f(
                           m_afTuple[0]-rkV.m_afTuple[0],
                           m_afTuple[1]-rkV.m_afTuple[1]);
    }

    public Vector2f scale( float fScalar )
    {
        return new Vector2f(
                            fScalar*m_afTuple[0],
                            fScalar*m_afTuple[1]);
    }

    // vector operations
    public float Length ()
    {
        return (float)Math.sqrt(
                         m_afTuple[0]*m_afTuple[0] +
                         m_afTuple[1]*m_afTuple[1]);
    }

    public float SquaredLength ()
    {
        return
            m_afTuple[0]*m_afTuple[0] +
            m_afTuple[1]*m_afTuple[1];
    }

    public float Dot (final Vector2f rkV) 
    {
        return
            m_afTuple[0]*rkV.m_afTuple[0] +
            m_afTuple[1]*rkV.m_afTuple[1];
    }

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

    private float[] m_afTuple = new float[2];
}
