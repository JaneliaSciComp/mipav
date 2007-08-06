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

import gov.nih.mipav.view.WildMagic.LibApplications.OpenGLApplication.ApplicationGUI;

public class UserConstant
{
    // Construction and destruction.  The base register must be nonnegative.
    // The register quantity must be positive.  Each register represents four
    // floating-point values.
    public UserConstant (String kProgramName, String rkName, int iBaseRegister,
                         int iRegisterQuantity, int iNumFloats)
    {
        m_kName = new String(rkName);
        assert(iBaseRegister >= 0);
        assert(iRegisterQuantity > 0);

        m_iBaseRegister = iBaseRegister;
        m_iRegisterQuantity = iRegisterQuantity;

        // To be set later in the effect creation.
        m_afData = null;
        m_iNumFloats = iNumFloats;
    }

    // Member access.  The renderer will use these to pass the information to
    // the graphics API.
    public String GetName ()
    {
        return m_kName;
    }

    public int GetBaseRegister ()
    {
        return m_iBaseRegister;
    }

    public int GetRegisterQuantity ()
    {
        return m_iRegisterQuantity;
    }

    public float[] GetData ()
    {
        return m_afData;
    }

    public int GetDataSize()
    {
        return m_iNumFloats;
    }

    // The Shader base class provides storage for the user constants and
    // will set the float pointer to this storage when the shader program is
    // loaded.  However, Shader-derived classes may provide their own
    // storage and set the float pointer accordingly.  Such derived classes
    // are responsible for deallocating the storage if it was dynamically
    // allocated.
    public void SetDataSource (float[] afData)
    {
        assert(afData != null);

        m_afData = afData;
    }

    public void SetData ( int iPos, float fValue )
    {
        if ( (m_afData != null) && (iPos >= 0) && (iPos < m_afData.length) )
        {
            m_afData[iPos] = fValue;
        }
    }

    public float GetData ( int iPos )
    {
        if ( (m_afData != null) && (iPos >= 0) && (iPos < m_afData.length) )
        {
            return m_afData[iPos];
        }
        return 0;
    }

    private String m_kName;
    private int m_iBaseRegister;
    private int m_iRegisterQuantity;
    private float[] m_afData;
    private int m_iNumFloats;
}
