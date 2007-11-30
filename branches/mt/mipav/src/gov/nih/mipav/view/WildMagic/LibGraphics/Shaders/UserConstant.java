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
    /** Construction and destruction.  The base register must be nonnegative.
     * The register quantity must be positive.  Each register represents four
     * floating-point values.
     * @param kProgramName, the name of the program the UserConstant belogs to.
     * @param rkName, the name of the UserConstant
     * @param iBaseRegister (nonnegative)
     * @param iRegisterQuantity (positive)
     * @param iNumFloats, the number of floats represented by this parameter.
     */
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

    /** Delete memory. */
    public void dispose()
    {
        m_kName = null;
        m_afData = null;
    }

    /** Return name of this UserConstant.
     * @return name of this UserConstant.
     */
    public final String GetName ()
    {
        return m_kName;
    }
 
     /** Member access.  The renderer will use these to set the registers with
     * the appropriate values.
     * @return base register value.
     */
   public final int GetBaseRegister ()
    {
        return m_iBaseRegister;
    }

    /** Member access.  The renderer will use these to set the registers with
     * the appropriate values.
     * @return base register quantity.
     */
    public final int GetRegisterQuantity ()
    {
        return m_iRegisterQuantity;
    }

    /** Member access.  The renderer will use these to set the registers with
     * the appropriate values.
     * @return UserConstant data values.
     */
    public final float[] GetData ()
    {
        return m_afData;
    }

    /** Get the number of floats.
     * @return UserConstant data size.
     */
    public final int GetDataSize()
    {
        return m_iNumFloats;
    }

    /** The Shader base class provides storage for the user constants and
     * will set the float pointer to this storage when the shader program is
     * loaded.  However, Shader-derived classes may provide their own
     * storage and set the float pointer accordingly.  Such derived classes
     * are responsible for deallocating the storage if it was dynamically
     * allocated.
     * @param afData, data source.
     */
    public void SetDataSource (float[] afData)
    {
        assert(afData != null);

        m_afData = afData;
    }

    /** The Shader base class provides storage for the user constants and
     * will set the float pointer to this storage when the shader program is
     * loaded.  However, Shader-derived classes may provide their own
     * storage and set the float pointer accordingly.  Such derived classes
     * are responsible for deallocating the storage if it was dynamically
     * allocated.
     * @param iPos, position in data array to write.
     * @param fValue, new value.
     */
    public void SetData ( int iPos, float fValue )
    {
        if ( (m_afData != null) && (iPos >= 0) && (iPos < m_afData.length) )
        {
            m_afData[iPos] = fValue;
        }
    }

    /** The Shader base class provides storage for the user constants and
     * will set the float pointer to this storage when the shader program is
     * loaded.  However, Shader-derived classes may provide their own
     * storage and set the float pointer accordingly.  Such derived classes
     * are responsible for deallocating the storage if it was dynamically
     * allocated.
     * @param iPos, position in data array to read.
     * @return value at iPos.
     */
    public float GetData ( int iPos )
    {
        if ( (m_afData != null) && (iPos >= 0) && (iPos < m_afData.length) )
        {
            return m_afData[iPos];
        }
        return 0;
    }
    /** UserConstant name. */
    private String m_kName;
    /** Base register (nonnegative) */
    private int m_iBaseRegister;
    /** Register quantity (positive) */
    private int m_iRegisterQuantity;
    /** Data */
    private float[] m_afData;
    /** Data size. */
    private int m_iNumFloats;
}
