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

public class ColorRGB
{
    public static final ColorRGB BLACK = new ColorRGB(0f,0f,0f);       // = (0,0,0) 
    public static final ColorRGB WHITE = new ColorRGB(1f,1f,1f);       // = (1,1,1)
    public static final ColorRGB INVALID = new ColorRGB(-1f,-1f,-1f);  // = (-1,-1,-1)

    /** Construction.  The components are intended to be in the interval
     * [0,1].  For accessing of colors by array index, the map is 0 = red, 1 =
     * green, and 2 = blue.
     */
    public ColorRGB ()  // initial values (0,0,0)
    {
        m_afTuple[0] = 0.0f;
        m_afTuple[1] = 0.0f;
        m_afTuple[2] = 0.0f;
    }

    /** Construction.  The components are intended to be in the interval
     * [0,1].  For accessing of colors by array index, the map is 0 = red, 1 =
     * green, and 2 = blue.
     * @param fR, red value
     * @param fG, green value
     * @param fB, blue value
     */
    public ColorRGB (float fR, float fG, float fB)
    {
        m_afTuple[0] = fR;
        m_afTuple[1] = fG;
        m_afTuple[2] = fB;
    }

    /** Construction.  The components are intended to be in the interval
     * [0,1].  For accessing of colors by array index, the map is 0 = red, 1 =
     * green, and 2 = blue.
     * @param afTuple, float[3] with [r,g,b]
     */
    public ColorRGB ( float[] afTuple)
    {
        m_afTuple[0] = afTuple[0];
        m_afTuple[1] = afTuple[1];
        m_afTuple[2] = afTuple[2];
    }

    /** Construction.  The components are intended to be in the interval
     * [0,1].  For accessing of colors by array index, the map is 0 = red, 1 =
     * green, and 2 = blue.
     * @param rkC, copies from the ColorRGB input
     */
    public ColorRGB ( ColorRGB rkC)
    {
        m_afTuple[0] = rkC.m_afTuple[0];
        m_afTuple[1] = rkC.m_afTuple[1];
        m_afTuple[2] = rkC.m_afTuple[2];
    }

    /**
     * delete memory
     */
    public void finalize()
    {
        if ( (this == ColorRGB.BLACK) || (this == ColorRGB.WHITE) )
        {
            System.err.println( "ColorRGB" );
        }
        m_afTuple = null;
    }

    /** Returns the color red value
     * @return  the color red value
     */
    public final float R ()
    {
        return m_afTuple[0];
    }

    /** Returns the color green value
     * @return  the color green value
     */
    public final float G ()
    {
        return m_afTuple[1];
    }

    /** Returns the color blue value
     * @return  the color blue value
     */
    public final float B ()
    {
        return m_afTuple[2];
    }

    /** Sets the color red value
     * @param fValue the new red value
     */
    public final void R (float fValue)
    {
        m_afTuple[0] = fValue;
    }

    /** Sets the color green value
     * @param fValue the new green value
     */
    public final void G (float fValue)
    {
        m_afTuple[1] = fValue;
    }

    /** Sets the color blue value
     * @param fValue the new blue value
     */
    public final void B (float fValue)
    {
        m_afTuple[2] = fValue;
    }

    /** Gets the color red,green,blue value
     * @return float[3] with [r,g,b]
     */
    public final float[] GetData ()
    {
        return m_afTuple;
    }

    /** Construction.  The components are intended to be in the interval
     * [0,1].  For accessing of colors by array index, the map is 0 = red, 1 =
     * green, and 2 = blue.
     * @param fR, red value
     * @param fG, green value
     * @param fB, blue value
     */
    public final void SetData (float fR, float fG, float fB)
    {
        m_afTuple[0] = fR;
        m_afTuple[1] = fG;
        m_afTuple[2] = fB;
    }
    
    /** Transform the color channels to [0,1].  Clamp sets negative values to
     * zero and values larger than one to one.  ScaleByMax assumes the color
     * channels are nonnegative, finds the maximum color channel, and divides
     * all channels by that value.
     */
    public void Clamp ()
    {
        for (int i = 0; i < 3; i++)
        {
            if (m_afTuple[i] > 1.0f)
            {
                m_afTuple[i] = 1.0f;
            }
            else if (m_afTuple[i] < 0.0f)
            {
                m_afTuple[i] = 0.0f;
            }
        }
    }

    /** Transform the color channels to [0,1].  Clamp sets negative values to
     * zero and values larger than one to one.  ScaleByMax assumes the color
     * channels are nonnegative, finds the maximum color channel, and divides
     * all channels by that value.
     */
    public void ScaleByMax ()
    {
        float fMax = m_afTuple[0];
        if (m_afTuple[1] > fMax)
        {
            fMax = m_afTuple[1];
        }
        if (m_afTuple[2] > fMax)
        {
            fMax = m_afTuple[2];
        }

        if (fMax > 1.0f)
        {
            float fInvMax = 1.0f/fMax;
            for (int i = 0; i < 3; i++)
            {
                m_afTuple[i] *= fInvMax;
            }
        }
    }

    /** Red,Green,Blue: */
    private float[] m_afTuple = new float[3];
}
