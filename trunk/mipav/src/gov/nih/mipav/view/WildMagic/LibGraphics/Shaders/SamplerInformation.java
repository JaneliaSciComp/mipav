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

public class SamplerInformation
{
    public enum Type
    {
        SAMPLER_1D,       // dimension 1
        SAMPLER_2D,       // dimension 2
        SAMPLER_3D,       // dimension 3
        SAMPLER_CUBE,     // dimension 2 (a set of 2D images)
        SAMPLER_PROJ,     // dimension 2
        MAX_SAMPLER_TYPES;

        Type( )
        {
            m_iValue = Init();
        }
        private int Init ()
        {
            return m_iInitValue++;
        }
        public int Value() { return m_iValue; }
        private int m_iValue;
        private static int m_iInitValue = 0;
    };

    // Construction and destruction.  The name, sampler type, and texture
    // unit are stored in the shader program files.  The dimension is
    // deduced from the sampler type.  The sampler unit is created when
    // loading a shader program.  The filter mode and wrap modes are specific
    // to a texture object and stored by such an object.  TO DO:  When a
    // file system *.wmfx is implemented, it will store filter and wrap mode
    // values.  A mechanism needs to be created to provide these to texture
    // objects.
    public SamplerInformation ( String rkName, Type eType,
                                int iTextureUnit)
    {
        m_kName = new String(rkName);

        m_eType = eType;
        m_iTextureUnit = iTextureUnit;

        switch (m_eType)
        {
        case SAMPLER_1D:
            m_iDimension = 1;
            break;
        case SAMPLER_2D:
            m_iDimension = 2;
            break;
        case SAMPLER_3D:
            m_iDimension = 3;
            break;
        case SAMPLER_CUBE:
            m_iDimension = 2;
            break;
        case SAMPLER_PROJ:
            m_iDimension = 2;
            break;
        default:
            assert(false);
            m_iDimension = 0;
            break;
        }
    }

    public void finalize () {}

    // Member read-only access.
    public String GetName ()
    {
        return m_kName;
    }

    public Type GetType ()
    {
        return m_eType;
    }

    public int GetTextureUnit ()
    {
        return m_iTextureUnit;
    }

    public int GetDimension ()
    {
        return m_iDimension;
    }

    private String m_kName;
    private Type m_eType;
    private int m_iTextureUnit;
    private int m_iDimension;
}
