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

public class ColorRGBA
{

    public static final ColorRGBA BLACK = new ColorRGBA(0f,0f,0f,1f);
    public static final ColorRGBA WHITE = new ColorRGBA(1f,1f,1f,1f);
    public static final ColorRGBA INVALID = new ColorRGBA(-1f,-1f,-1f,-1f);


    /** Construction.  The components are intended to be in the interval [0,1].
     * For accessing of colors by array index, the map is 0 = red, 1 = green,
     * 2 = blue, and 3 = alpha. initial values (0,0,0,0)
     */
    public ColorRGBA () {}  

    /** Construction.  The components are intended to be in the interval [0,1].
     * For accessing of colors by array index, the map is 0 = red, 1 = green,
     * 2 = blue, and 3 = alpha.
     * @param fR, red
     * @param fG, green
     * @param fB, blue
     * @param fA, alpha
     */
    public ColorRGBA (float fR, float fG, float fB, float fA)
    {
        m_afTuple[0] = fR;
        m_afTuple[1] = fG;
        m_afTuple[2] = fB;
        m_afTuple[3] = fA;
    }

    /** Construction.  The components are intended to be in the interval [0,1].
     * For accessing of colors by array index, the map is 0 = red, 1 = green,
     * 2 = blue, and 3 = alpha.
     * @param afTuple float[4] with [r,g,b,a]
     */
    public ColorRGBA (float[] afTuple)
    {
        m_afTuple[0] = afTuple[0];
        m_afTuple[1] = afTuple[1];
        m_afTuple[2] = afTuple[2];
    }

    /** Construction.  The components are intended to be in the interval [0,1].
     * For accessing of colors by array index, the map is 0 = red, 1 = green,
     * 2 = blue, and 3 = alpha.
     * @param fkC copies from input ColorRGBA
     */
    public ColorRGBA (ColorRGBA rkC)
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
        m_afTuple = null;
    }

    /** Gets the color red,green,blue,alpha value
     * @return float[4] with [r,g,b,a]
     */
    public final float[] GetData () 
    {
        return m_afTuple;
    }

    /** Sets the color red,green,blue value
     * @param fR, red
     * @param fG, green
     * @param fB, blue
     * @param fA, alpha
     */
    public final void SetData ( float fR, float fG, float fB, float fA ) 
    {
        m_afTuple[0] = fR;
        m_afTuple[1] = fG;
        m_afTuple[2] = fB;
        m_afTuple[3] = fA;
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

    /** Returns the color alpha value
     * @return  the color alpha value
     */
    public final float A ()
    {
        return m_afTuple[3];
    }

    /** Sets the color red value
     * @param fR the new red value
     */
    public final void R ( float fR ) 
    {
        m_afTuple[0] = fR;
    }

    /** Sets the color green value
     * @param fG the new green value
     */
    public final void G ( float fG ) 
    {
        m_afTuple[1] = fG;
    }

    /** Sets the color blue value
     * @param fB  the new blue value
     */
    public final void B ( float fB ) 
    {
        m_afTuple[2] = fB;
    }

    /** Sets the color alpha value
     * @param fA the new alpha value
     */
    public final void A ( float fA ) 
    {
        m_afTuple[3] = fA;
    }

    /** r,g,b,a: */
    private float[] m_afTuple = new float[]{0,0,0,0};
}
