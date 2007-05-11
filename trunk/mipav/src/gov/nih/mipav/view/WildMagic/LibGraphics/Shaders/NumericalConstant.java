// Geometric Tools, Inc.
// http://www.geometrictools.com
// Copyright (c) 1998-2006.  All Rights Reserved
//
// The Wild Magic Version 4 Restricted Libraries source code is supplied
// under the terms of the license agreement
//     http://www.geometrictools.com/License/Wm4RestrictedLicense.pdf
// and may not be copied or disclosed except in accordance with the terms
// of that agreement.
//
// Version: 4.0.0 (2006/06/28)

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
