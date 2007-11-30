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

public class SamplerInformation
{
    /** Type of Sampler */
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

    /** Construction and destruction.  The name, sampler type, and texture
     * unit are stored in the shader program files.  The dimension is deduced
     * from the sampler type.  The sampler unit is created when loading a
     * shader program.  The filter mode and wrap modes are specific to a
     * texture object and stored by such an object. 
     * @param rkName, Sampler name.
     * @param eType, Sampler type.
     * @param iTextureUnit, Sampler texture unit.
     */
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

    /** Delete memory */
    public void dispose ()
    {
        m_kName = null;
    }

    /** Return name of sampler.
     * @return name of sampler.
     */
    public final String GetName ()
    {
        return m_kName;
    }

    /** Return type of sampler.
     * @return type of sampler.
     */
    public final Type GetType ()
    {
        return m_eType;
    }

    /** Return sampler texture unit.
     * @return sampler texture unit.
     */
    public final int GetTextureUnit ()
    {
        return m_iTextureUnit;
    }

    /** Return sampler dimensions.
     * @return sampler dimensions.
     */
    public final int GetDimension ()
    {
        return m_iDimension;
    }
    /** Sampler name -- matches shader program sampler name. */
    private String m_kName;
    /** Sampler type. */
    private Type m_eType;
    /** Sampler texture unit -- matches shader program. */
    private int m_iTextureUnit;
    /** Sampler dimensions (1D, 2D, 3D) */
    private int m_iDimension;
}
