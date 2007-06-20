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


    // member access
    public float[] GetData () 
    {
        return m_afTuple;
    }
    public void SetData ( float fR, float fG, float fB, float fA ) 
    {
        m_afTuple[0] = fR;
        m_afTuple[1] = fG;
        m_afTuple[2] = fB;
        m_afTuple[3] = fA;
    }

    public float R () 
    {
        return m_afTuple[0];
    }
    public float G ()
    {
        return m_afTuple[1];
    }
    public float B ()
    {
        return m_afTuple[2];
    }
    public float A ()
    {
        return m_afTuple[3];
    }
    public void R ( float fR ) 
    {
        m_afTuple[0] = fR;
    }
    public void G ( float fG ) 
    {
        m_afTuple[1] = fG;
    }
    public void B ( float fB ) 
    {
        m_afTuple[2] = fB;
    }
    public void A ( float fA ) 
    {
        m_afTuple[3] = fA;
    }

    /** r,g,b,a: */
    private float[] m_afTuple = new float[]{0,0,0,0};
}
