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
package gov.nih.mipav.view.WildMagic.LibGraphics.Shaders;

public class NumericalConstant
{
    // Construction and destruction.  The register must be nonnegative.  A
    // numerical constant corresponds to four floating-point numbers.
    public NumericalConstant (int iRegister, float[] afData)
    {
        assert(iRegister >= 0);

        m_iRegister = iRegister;
        for (int i = 0; i < 4; i++)
        {
            m_afData[i] = afData[i];
        }
    }

    // Member access.  The renderer will use these to pass the information to
    // the graphics API (specifically, to DirectX).
    public int GetRegister ()
    {
        return m_iRegister;
    }

    public float[] GetData ()
    {
        return m_afData;
    }

    private int m_iRegister;
    private float[] m_afData = new float[4];
}
