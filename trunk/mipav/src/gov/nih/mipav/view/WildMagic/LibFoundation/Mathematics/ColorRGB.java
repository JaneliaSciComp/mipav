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
// Version: 4.0.0 (2006/06/28)

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

    // member access
    /** Returns the color red value
     * @return  the color red value
     */
    public float R ()
    {
        return m_afTuple[0];
    }

    /** Returns the color green value
     * @return  the color green value
     */
    public float G ()
    {
        return m_afTuple[1];
    }

    /** Returns the color blue value
     * @return  the color blue value
     */
    public float B ()
    {
        return m_afTuple[2];
    }

    /** Sets the color red value
     * @param  the new red value
     */
    public void R (float fValue)
    {
        m_afTuple[0] = fValue;
    }

    /** Sets the color green value
     * @param  the new green value
     */
    public void G (float fValue)
    {
        m_afTuple[1] = fValue;
    }

    /** Sets the color blue value
     * @param  the new blue value
     */
    public void B (float fValue)
    {
        m_afTuple[2] = fValue;
    }

    /** Sets the color red,green,blue value
     * @return float[3] with [r,g,b]
     */
    public float[] GetData ()
    {
        return m_afTuple;
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
