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

package gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph;

import java.util.Vector;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;

public class VertexBuffer extends Bindable
    implements StreamInterface
{
    /** Create a VertexBuffer with the given attributes and number of vertices.
     * Calculate the size of the VertexBuffer based on the Attributes.
     * @param rkAttributes, vertex buffer attributes.
     * @param iVertexQuantity, number of vertices.
     */
    public VertexBuffer (Attributes rkAttributes, int iVertexQuantity)
    {
        m_kAttributes = rkAttributes;
        assert(iVertexQuantity > 0);
        m_iVertexQuantity = iVertexQuantity;
        m_iVertexSize = m_kAttributes.GetChannelQuantity();
        m_iChannelQuantity = m_iVertexQuantity*m_iVertexSize;
        m_afChannel = new float[m_iChannelQuantity];
    }

    /**
     * Copy constructor.
     * @param pkVBuffer, VertexBuffer to copy into this.
     */
    public VertexBuffer (VertexBuffer pkVBuffer)
    {
        assert(pkVBuffer != null);
        m_kAttributes = new Attributes( pkVBuffer.m_kAttributes );
        m_iVertexQuantity = pkVBuffer.m_iVertexQuantity;
        m_iVertexSize = m_kAttributes.GetChannelQuantity();
        m_iChannelQuantity = m_iVertexQuantity*m_iVertexSize;
        m_afChannel = new float[m_iChannelQuantity];
        for ( int i = 0; i < m_iChannelQuantity; i++ )
        {
            m_afChannel[i] = pkVBuffer.m_afChannel[i];
        }
    }

    /** Delete memory. */
    public void dispose()
    {
        if ( m_kAttributes != null )
        {
            m_kAttributes.dispose();
            m_kAttributes = null;
        }
        m_afChannel = null;
        super.dispose();
    }

    /** The format of a single vertex in the buffer.
     * @return vertex attributes.
     */
    public final Attributes GetAttributes ()
    {
        return m_kAttributes;
    }


    /** The size of a single vertex in the buffer, measured as number of
     * 'float' values.  The number of bytes for a single vertex is
     * GetVertexSize()*sizeof(float).
     * @return vertex size.
     */
    public final int GetVertexSize ()
    {
        return m_iVertexSize;
    }

    /** The number of vertices in the buffer.
     * @return number of vertices.
     */
    public final int GetVertexQuantity ()
    {
        return m_iVertexQuantity;
    }

    /** Direct access to the vertex buffer data.  The quantity is the number of
     * float elements.  The number of bytes for the entire vertex buffer is
     * GetChannelQuantity()*sizeof(float).
     * @return channel quantity.
     */
    public final int GetChannelQuantity ()
    {
        return m_iChannelQuantity;
    }

    /** Get the position at the given index. Use these accessors for
     * convenience.  No range checking is performed, so you should be sure
     * that the attribute exists and that the number of channels is correct.
     * @param i, vertex index.
     * @return position.
     */
    public Vector3f GetPosition3 (int i)
    {
        assert(m_kAttributes.GetPChannels() == 3);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetPOffset();
        return new Vector3f( m_afChannel[ iIndex + 0 ],
                             m_afChannel[ iIndex + 1 ],
                             m_afChannel[ iIndex + 2 ] );
    }
    
    /** Get the position at the given index. Use these accessors for
     * convenience.  No range checking is performed, so you should be sure
     * that the attribute exists and that the number of channels is correct.
     * @param i, vertex index.
     * @return position.
     */
    public void GetPosition3 (int i, Vector3f kResult)
    {
        assert(m_kAttributes.GetPChannels() == 3);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetPOffset();
        kResult.SetData( m_afChannel[ iIndex + 0 ],
                             m_afChannel[ iIndex + 1 ],
                             m_afChannel[ iIndex + 2 ] );
    }
    
    /** Get the position at the given index. Use these accessors for
     * convenience.  No range checking is performed, so you should be sure
     * that the attribute exists and that the number of channels is correct.
     * @param i, vertex index.
     * @return position.
     */
    public float GetPosition3fX (int i)
    {
        assert(m_kAttributes.GetPChannels() == 3);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetPOffset();
        return m_afChannel[ iIndex + 0 ];
    }
    
    public float GetPosition3fY (int i)
    {
        assert(m_kAttributes.GetPChannels() == 3);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetPOffset();
        return m_afChannel[ iIndex + 1 ];
    }
    
    public float GetPosition3fZ (int i)
    {
        assert(m_kAttributes.GetPChannels() == 3);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetPOffset();
        return m_afChannel[ iIndex + 2 ];
    }
    
    /** Set the position at the given index.
     * @param i, vertex index.
     * @param kP, new position.
     */
    public void SetPosition3 (int i, Vector3f kP)
    {
        assert(m_kAttributes.GetPChannels() == 3);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetPOffset();
        m_afChannel[ iIndex + 0 ] = kP.X();
        m_afChannel[ iIndex + 1 ] = kP.Y();
        m_afChannel[ iIndex + 2 ] = kP.Z();
    }

    /** Set the position at the given index.
     * @param i, vertex index.
     * @param kP, new position.
     */
    public void SetPosition3 (int i, float fX, float fY, float fZ)
    {
        assert(m_kAttributes.GetPChannels() == 3);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetPOffset();
        m_afChannel[ iIndex + 0 ] = fX;
        m_afChannel[ iIndex + 1 ] = fY;
        m_afChannel[ iIndex + 2 ] = fZ;
    }
    
    /** Get the normal at the given index. Use these accessors for
     * convenience.  No range checking is performed, so you should be sure
     * that the attribute exists and that the number of channels is correct.
     * @param i, vertex index.
     * @return normal.
     */
    public void GetNormal3 (int i, Vector3f kResult)
    {
        assert(m_kAttributes.GetNChannels() == 3);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetNOffset();
        kResult.SetData( m_afChannel[ iIndex + 0 ],
                             m_afChannel[ iIndex + 1 ],
                             m_afChannel[ iIndex + 2 ] );
    }
    
    public float GetNormal3fX (int i)
    {
        assert(m_kAttributes.GetNChannels() == 3);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetNOffset();
        return m_afChannel[ iIndex + 0 ];
    }
    
    public float GetNormal3fY (int i)
    {
        assert(m_kAttributes.GetNChannels() == 3);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetNOffset();
        return m_afChannel[ iIndex + 1 ];
    }
    
    public float GetNormal3fZ (int i)
    {
        assert(m_kAttributes.GetNChannels() == 3);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetNOffset();
        return m_afChannel[ iIndex + 2 ];
    }

    /** Set the normal at the given index.
     * @param i, vertex index.
     * @param kN, new normal.
     */
    public void SetNormal3 (int i, Vector3f kN)
    {
        assert(m_kAttributes.GetNChannels() == 3);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetNOffset();
        m_afChannel[ iIndex + 0 ] = kN.X();
        m_afChannel[ iIndex + 1 ] = kN.Y();
        m_afChannel[ iIndex + 2 ] = kN.Z();
    }
    
    /** Set the normal at the given index.
     * @param i, vertex index.
     * @param kN, new normal.
     */
    public void SetNormal3 (int i, float fX, float fY, float fZ)
    {
        assert(m_kAttributes.GetNChannels() == 3);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetNOffset();
        m_afChannel[ iIndex + 0 ] = fX;
        m_afChannel[ iIndex + 1 ] = fY;
        m_afChannel[ iIndex + 2 ] = fZ;
    }
   
    /** Get the color at the given index. Use these accessors for convenience.
     * No range checking is performed, so you should be sure that the
     * attribute exists and that the number of channels is correct.
     * @param iUnit, color unit (1-4).
     * @param i, vertex index.
     * @return color.
     */
    public void GetColor3 (int iUnit, int i, ColorRGB kResult)
    {
        assert(m_kAttributes.GetCChannels(iUnit) == 3);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetCOffset(iUnit);
        kResult.SetData( m_afChannel[ iIndex + 0 ], 
                             m_afChannel[ iIndex + 1 ],
                             m_afChannel[ iIndex + 2 ] );
    }
    
    /** Get the color at the given index. Use these accessors for convenience.
     * No range checking is performed, so you should be sure that the
     * attribute exists and that the number of channels is correct.
     * @param iUnit, color unit (1-4).
     * @param i, vertex index.
     * @return color.
     */
    public void GetColor4 (int iUnit, int i, ColorRGBA kResult)
    {
        assert(m_kAttributes.GetCChannels(iUnit) == 4);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetCOffset(iUnit);
        kResult.SetData( m_afChannel[ iIndex + 0 ], 
                             m_afChannel[ iIndex + 1 ],
                             m_afChannel[ iIndex + 2 ],
                             m_afChannel[ iIndex + 3 ] );
    }

    /** Set the color at the given index.
     * @param iUnit, color unit (1-4).
     * @param i, vertex index.
     * @param kC, new color.
     */
    public void SetColor3 (int iUnit, int i, ColorRGB kC)
    {
        assert(m_kAttributes.GetCChannels(iUnit) == 3);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetCOffset(iUnit);
        m_afChannel[ iIndex + 0 ] = kC.R(); 
        m_afChannel[ iIndex + 1 ] = kC.G();
        m_afChannel[ iIndex + 2 ] = kC.B();
    }
    
    /** Set the color at the given index.
     * @param iUnit, color unit (1-4).
     * @param i, vertex index.
     * @param kC, new color.
     */
    public void SetColor3 (int iUnit, int i, float fR, float fG, float fB)
    {
        assert(m_kAttributes.GetCChannels(iUnit) == 3);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetCOffset(iUnit);
        m_afChannel[ iIndex + 0 ] = fR; 
        m_afChannel[ iIndex + 1 ] = fG;
        m_afChannel[ iIndex + 2 ] = fB;
    }
    
    /** Set the color at the given index.
     * @param iUnit, color unit (1-4).
     * @param i, vertex index.
     * @param kC, new color.
     */
    public void SetColor4 (int iUnit, int i, float fR, float fG, float fB, float fA)
    {
        assert(m_kAttributes.GetCChannels(iUnit) == 4);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetCOffset(iUnit);
        m_afChannel[ iIndex + 0 ] = fR; 
        m_afChannel[ iIndex + 1 ] = fG;
        m_afChannel[ iIndex + 2 ] = fB;
        m_afChannel[ iIndex + 3 ] = fA;
    }    
    
    /** Set the color at the given index.
     * @param iUnit, color unit (1-4).
     * @param i, vertex index.
     * @param kC, new color.
     */
    public void SetColor4 (int iUnit, int i, ColorRGBA kC)
    {
        assert(m_kAttributes.GetCChannels(iUnit) == 4);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetCOffset(iUnit);
        m_afChannel[ iIndex + 0 ] = kC.R(); 
        m_afChannel[ iIndex + 1 ] = kC.G();
        m_afChannel[ iIndex + 2 ] = kC.B();
        m_afChannel[ iIndex + 3 ] = kC.A();
    }
    
    /** Get the texture coordinate (1D) at the given index. Use these
     * accessors for convenience.  No range checking is performed, so you
     * should be sure that the attribute exists and that the number of
     * channels is correct.
     * @param iUnit, texture coordinate unit (1-4).
     * @param i, vertex index.
     * @return texture coordinate.
     */
    public float GetTCoord1 (int iUnit, int i)
    {
        assert(m_kAttributes.GetTChannels(iUnit) == 1);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetTOffset(iUnit);
        return m_afChannel[ iIndex ];
    }

    /** Set the texture coordiante (1D) at the given index.
     * @param iUnit, texture coordinate unit (1-4).
     * @param i, vertex index.
     * @param fValue, new texture coordinate.
     */
    public void SetTCoord1 (int iUnit, int i, float fValue)
    {
        assert(m_kAttributes.GetTChannels(iUnit) == 1);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetTOffset(iUnit);
        m_afChannel[ iIndex ] = fValue;
    }

    /** Get the texture coordinate (2D) at the given index. Use these
     * accessors for convenience.  No range checking is performed, so you
     * should be sure that the attribute exists and that the number of
     * channels is correct.
     * @param iUnit, texture coordinate unit (1-4).
     * @param i, vertex index.
     * @return texture coordinate.
     */
    public void GetTCoord2 (int iUnit, int i, Vector2f kResult)
    {
        assert(m_kAttributes.GetTChannels(iUnit) == 2);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetTOffset(iUnit);
        kResult.SetData( m_afChannel[ iIndex + 0 ],
                             m_afChannel[ iIndex + 1 ] );
    }

    /** Get the texture coordinate (2D) at the given index. Use these
     * accessors for convenience.  No range checking is performed, so you
     * should be sure that the attribute exists and that the number of
     * channels is correct.
     * @param iUnit, texture coordinate unit (1-4).
     * @param i, vertex index.
     * @return texture coordinate.
     */
    public float GetTCoord2fX (int iUnit, int i)
    {
        assert(m_kAttributes.GetTChannels(iUnit) == 2);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetTOffset(iUnit);
        return m_afChannel[ iIndex + 0 ];
    }

    public float GetTCoord2fY (int iUnit, int i)
    {
        assert(m_kAttributes.GetTChannels(iUnit) == 2);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetTOffset(iUnit);
        return m_afChannel[ iIndex + 1 ];
    }


    /** Set the texture coordiante (2D) at the given index.
     * @param iUnit, texture coordinate unit (1-4).
     * @param i, vertex index.
     * @param kTC, new texture coordinate.
     */
    public void SetTCoord2 (int iUnit, int i, Vector2f kTC)
    {
        assert(m_kAttributes.GetTChannels(iUnit) == 2);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetTOffset(iUnit);
        m_afChannel[ iIndex + 0 ] = kTC.X();
        m_afChannel[ iIndex + 1 ] = kTC.Y();
    }
 
    /** Set the texture coordiante (2D) at the given index.
     * @param iUnit, texture coordinate unit (1-4).
     * @param i, vertex index.
     * @param kTC, new texture coordinate.
     */
    public void SetTCoord2 (int iUnit, int i,float fX, float fY)
    {
        assert(m_kAttributes.GetTChannels(iUnit) == 2);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetTOffset(iUnit);
        m_afChannel[ iIndex + 0 ] = fX;
        m_afChannel[ iIndex + 1 ] = fY;
    }
    
    /** Get the texture coordinate (3D) at the given index. Use these
     * accessors for convenience.  No range checking is performed, so you
     * should be sure that the attribute exists and that the number of
     * channels is correct.
     * @param i, vertex index.
     * @return texture coordinate.
     */
    public void GetTCoord3 (int iUnit, int i, Vector3f kResult)
    {
        assert(m_kAttributes.GetTChannels(iUnit) == 2);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetTOffset(iUnit);
        System.err.println("public Vector3f TCoord3 (int iUnit, int i)");
        kResult.SetData( m_afChannel[ iIndex + 0 ],
                             m_afChannel[ iIndex + 1 ],
                             m_afChannel[ iIndex + 2 ] );
    }

    /** Set the texture coordiante (3D) at the given index.
     * @param iUnit, texture coordinate unit (1-4).
     * @param i, vertex index.
     * @param kTC, new texture coordinate.
     */
    public void SetTCoord3 (int iUnit, int i, Vector3f kTC)
    {
        assert(m_kAttributes.GetTChannels(iUnit) == 3);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetTOffset(iUnit);
        m_afChannel[ iIndex + 0 ] = kTC.X();
        m_afChannel[ iIndex + 1 ] = kTC.Y();
        m_afChannel[ iIndex + 2 ] = kTC.Z();
    }
    
    /** Set the texture coordiante (3D) at the given index.
     * @param iUnit, texture coordinate unit (1-4).
     * @param i, vertex index.
     * @param kTC, new texture coordinate.
     */
    public void SetTCoord3 (int iUnit, int i, float fX, float fY, float fZ)
    {
        assert(m_kAttributes.GetTChannels(iUnit) == 3);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetTOffset(iUnit);
        m_afChannel[ iIndex + 0 ] = fX;
        m_afChannel[ iIndex + 1 ] = fY;
        m_afChannel[ iIndex + 2 ] = fZ;
    }

    public int GetIndex( int i )
    {
        return m_iVertexSize*i;
    }
    
    /** Support for building an array from the vertex buffer data, but
     * compatible with the vertex program inputs.  The output array,
     * rafCompatible, if null on input is dynamically allocated.  The caller
     * is responsible for deleting it.  You may pass in an already allocated
     * array as long as you are certain it has enough channels to store the
     * data.
     * @param rkIAttr, vertex program input attributes
     * @return compatible array data.
     */
    public float[] BuildCompatibleArray (Attributes rkIAttr)
    {
        // The use of "unsigned int" is to allow storage of "float" channels and
        // of the ARGB-formatted colors, which are "unsigned int".  Typecasting
        // "float" pointers to "unsigned int" pointers and then dereferencing
        // them works as expected.  The alternative is to use a vector of "float"
        // and typecast "unsigned int" pointers to "float" pointers.  However,
        // dereferencing to a "float" to allow push_back has problems.  The
        // memory pattern for an "unsigned int" might correspond to an invalid
        // "float".  The floating-point unit actually makes adjustments to these
        // values, changing what it is you started with.
        //System.err.println("public float[] BuildCompatibleArray (Attributes rkIAttr)");
        Vector<Float> kCompatible = new Vector<Float>();    
        int iUnit, iIChannels, iVBChannels;
        int iIndex;
        for (int i = 0, j; i < m_iVertexQuantity; i++)
        {
            if (rkIAttr.HasPosition())
            {
                iIChannels = rkIAttr.GetPChannels();
                iVBChannels = m_kAttributes.GetPChannels();

                iIndex = m_iVertexSize*i + m_kAttributes.GetPOffset();
                if (iVBChannels < iIChannels)
                {
                    for ( j = 0; j < iVBChannels; j++ )
                    {
                        kCompatible.add(m_afChannel[iIndex + j]);
                    }                    
                    for (j = iVBChannels; j < iIChannels; j++)
                    {
                        // Fill with 1 so that the w-component is compatible with
                        // a homogeneous point.
                        kCompatible.add(1.0f);
                    }
                }
                else
                {
                    for (j = 0; j < iIChannels; j++)
                    {
                        kCompatible.add(m_afChannel[iIndex + j]);
                    }
                }
            }

            if (rkIAttr.HasNormal())
            {
                iIChannels = rkIAttr.GetNChannels();
                iVBChannels = m_kAttributes.GetNChannels();
                
                iIndex = m_iVertexSize*i + m_kAttributes.GetNOffset();
                if (iVBChannels < iIChannels)
                {
                    for (j = 0; j < iVBChannels; j++)
                    {
                        kCompatible.add(m_afChannel[iIndex + j]);
                    }
                    for (j = iVBChannels; j < iIChannels; j++)
                    {
                        // Fill with 0 so that the w-component is compatible with
                        // a homogeneous vector.
                        kCompatible.add(0.0f);
                    }
                }
                else
                {
                    for (j = 0; j < iIChannels; j++)
                    {
                        kCompatible.add(m_afChannel[iIndex + j]);
                    }
                }
            }

            for (iUnit = 0; iUnit < (int)rkIAttr.GetMaxColors(); iUnit++)
            {
                if (rkIAttr.HasColor(iUnit))
                {
                    iIChannels = rkIAttr.GetCChannels(iUnit);
                    iVBChannels = m_kAttributes.GetCChannels(iUnit);
                    iIndex = m_iVertexSize*i + m_kAttributes.GetCOffset(iUnit);                  
                    if (iVBChannels < iIChannels)
                    {
                        for (j = 0; j < iVBChannels; j++)
                        {
                            kCompatible.add(m_afChannel[iIndex + j]);
                        }
                        for (j = iVBChannels; j < iIChannels; j++)
                        {
                            // Fill with 1 so that the a-component is compatible
                            // with an opaque color.
                            kCompatible.add(1.0f);
                        }
                    }
                    else
                    {
                        for (j = 0; j < iIChannels; j++)
                        {
                            kCompatible.add(m_afChannel[iIndex + j]);
                        }
                    }
                }
            }

            for (iUnit = 0; iUnit < (int)rkIAttr.GetMaxTCoords(); iUnit++)
            {
                if (rkIAttr.HasTCoord(iUnit))
                {
                    iIChannels = rkIAttr.GetTChannels(iUnit);
                    iVBChannels = m_kAttributes.GetTChannels(iUnit);
                    iIndex = m_iVertexSize*i + m_kAttributes.GetTOffset(iUnit);                   
                    if (iVBChannels < iIChannels)
                    {
                        for (j = 0; j < iVBChannels; j++)
                        {
                            kCompatible.add(m_afChannel[iIndex + j]);
                        }
                        for (j = iVBChannels; j < iIChannels; j++)
                        {
                            // Fill with 0 so that the components are compatible
                            // with a higher-dimensional image embedded in a
                            // lower-dimensional one.
                            kCompatible.add(0.0f);
                        }
                    }
                    else
                    {
                        for (j = 0; j < iIChannels; j++)
                        {
                            kCompatible.add(m_afChannel[iIndex + j]);
                        }
                    }
                }
            }
        }

        float[] afReturn = new float[kCompatible.size()];
        for ( int i = 0; i < kCompatible.size(); i++)
        {
            afReturn[i] = kCompatible.get(i).floatValue();
        }
        return afReturn;
    }


    /** Support for building an array from the vertex buffer data, but
     * compatible with the vertex program inputs.  The output array,
     * rafCompatible, if null on input is dynamically allocated.  The caller
     * is responsible for deleting it.  You may pass in an already allocated
     * array as long as you are certain it has enough channels to store the
     * data.
     * @param rkIAttr, vertex program input attributes
     * @return compatible array data.
     */
    public float[] BuildCompatibleSubArray (Attributes rkIAttr, int iMin, int iMax)
    {
        Vector<Float> kCompatible = new Vector<Float>();    
        int iUnit, iIChannels, iVBChannels;
        int iIndex;
        for (int i = iMin, j; i <= iMax; i++)
        {
            if (rkIAttr.HasPosition())
            {
                iIChannels = rkIAttr.GetPChannels();
                iVBChannels = m_kAttributes.GetPChannels();

                iIndex = m_iVertexSize*i + m_kAttributes.GetPOffset();
                if (iVBChannels < iIChannels)
                {
                    for ( j = 0; j < iVBChannels; j++ )
                    {
                        kCompatible.add(m_afChannel[iIndex + j]);
                    }                    
                    for (j = iVBChannels; j < iIChannels; j++)
                    {
                        // Fill with 1 so that the w-component is compatible with
                        // a homogeneous point.
                        kCompatible.add(1.0f);
                    }
                }
                else
                {
                    for (j = 0; j < iIChannels; j++)
                    {
                        kCompatible.add(m_afChannel[iIndex + j]);
                    }
                }
            }

            if (rkIAttr.HasNormal())
            {
                iIChannels = rkIAttr.GetNChannels();
                iVBChannels = m_kAttributes.GetNChannels();
                
                iIndex = m_iVertexSize*i + m_kAttributes.GetNOffset();
                if (iVBChannels < iIChannels)
                {
                    for (j = 0; j < iVBChannels; j++)
                    {
                        kCompatible.add(m_afChannel[iIndex + j]);
                    }
                    for (j = iVBChannels; j < iIChannels; j++)
                    {
                        // Fill with 0 so that the w-component is compatible with
                        // a homogeneous vector.
                        kCompatible.add(0.0f);
                    }
                }
                else
                {
                    for (j = 0; j < iIChannels; j++)
                    {
                        kCompatible.add(m_afChannel[iIndex + j]);
                    }
                }
            }

            for (iUnit = 0; iUnit < (int)rkIAttr.GetMaxColors(); iUnit++)
            {
                if (rkIAttr.HasColor(iUnit))
                {
                    iIChannels = rkIAttr.GetCChannels(iUnit);
                    iVBChannels = m_kAttributes.GetCChannels(iUnit);
                    iIndex = m_iVertexSize*i + m_kAttributes.GetCOffset(iUnit);                  
                    if (iVBChannels < iIChannels)
                    {
                        for (j = 0; j < iVBChannels; j++)
                        {
                            kCompatible.add(m_afChannel[iIndex + j]);
                        }
                        for (j = iVBChannels; j < iIChannels; j++)
                        {
                            // Fill with 1 so that the a-component is compatible
                            // with an opaque color.
                            kCompatible.add(1.0f);
                        }
                    }
                    else
                    {
                        for (j = 0; j < iIChannels; j++)
                        {
                            kCompatible.add(m_afChannel[iIndex + j]);
                        }
                    }
                }
            }

            for (iUnit = 0; iUnit < (int)rkIAttr.GetMaxTCoords(); iUnit++)
            {
                if (rkIAttr.HasTCoord(iUnit))
                {
                    iIChannels = rkIAttr.GetTChannels(iUnit);
                    iVBChannels = m_kAttributes.GetTChannels(iUnit);
                    iIndex = m_iVertexSize*i + m_kAttributes.GetTOffset(iUnit);                   
                    if (iVBChannels < iIChannels)
                    {
                        for (j = 0; j < iVBChannels; j++)
                        {
                            kCompatible.add(m_afChannel[iIndex + j]);
                        }
                        for (j = iVBChannels; j < iIChannels; j++)
                        {
                            // Fill with 0 so that the components are compatible
                            // with a higher-dimensional image embedded in a
                            // lower-dimensional one.
                            kCompatible.add(0.0f);
                        }
                    }
                    else
                    {
                        for (j = 0; j < iIChannels; j++)
                        {
                            kCompatible.add(m_afChannel[iIndex + j]);
                        }
                    }
                }
            }
        }

        float[] afReturn = new float[kCompatible.size()];
        for ( int i = 0; i < kCompatible.size(); i++)
        {
            afReturn[i] = kCompatible.get(i).floatValue();
        }
        return afReturn;
    }



    /** Support for building an array from the vertex buffer data, but
     * compatible with the vertex program inputs.  The output array,
     * rafCompatible, if null on input is dynamically allocated.  The caller
     * is responsible for deleting it.  You may pass in an already allocated
     * array as long as you are certain it has enough channels to store the
     * data.
     * @param rkIAttr, vertex program input attributes
     * @param kVB compatible array VertexBuffer output.
     */
    public void BuildCompatibleArray (Attributes rkIAttr, VertexBuffer kVB)
    {
        // The use of "unsigned int" is to allow storage of "float" channels and
        // of the ARGB-formatted colors, which are "unsigned int".  Typecasting
        // "float" pointers to "unsigned int" pointers and then dereferencing
        // them works as expected.  The alternative is to use a vector of "float"
        // and typecast "unsigned int" pointers to "float" pointers.  However,
        // dereferencing to a "float" to allow push_back has problems.  The
        // memory pattern for an "unsigned int" might correspond to an invalid
        // "float".  The floating-point unit actually makes adjustments to these
        // values, changing what it is you started with.
        int iUnit, iIChannels, iVBChannels;

        int ikVBIndex = 0;
        int iIndex;
        for (int i = 0, j; i < m_iVertexQuantity; i++)
        {
            if (rkIAttr.HasPosition())
            {
                iIChannels = rkIAttr.GetPChannels();
                iVBChannels = m_kAttributes.GetPChannels();

                iIndex = m_iVertexSize*i + m_kAttributes.GetPOffset();
                if (iVBChannels < iIChannels)
                {
                    for (j = 0; j < iVBChannels; j++)
                    {
                        kVB.m_afChannel[ikVBIndex++] = m_afChannel[iIndex + j];
                    }
                    for (j = iVBChannels; j < iIChannels; j++)
                    {
                        // Fill with 1 so that the w-component is compatible with
                        // a homogeneous point.
                        kVB.m_afChannel[ikVBIndex++] = 1.0f;
                    }
                }
                else
                {
                    for (j = 0; j < iIChannels; j++)
                    {
                        kVB.m_afChannel[ikVBIndex++] = m_afChannel[iIndex + j];
                    }
                }
            }

            if (rkIAttr.HasNormal())
            {
                iIChannels = rkIAttr.GetNChannels();
                iVBChannels = m_kAttributes.GetNChannels();
                iIndex = m_iVertexSize*i + m_kAttributes.GetNOffset();
                if (iVBChannels < iIChannels)
                {
                    for (j = 0; j < iVBChannels; j++)
                    {
                        kVB.m_afChannel[ikVBIndex++] = m_afChannel[iIndex + j];
                    }
                    for (j = iVBChannels; j < iIChannels; j++)
                    {
                        // Fill with 0 so that the w-component is compatible with
                        // a homogeneous vector.
                        kVB.m_afChannel[ikVBIndex++] = 0.0f;
                    }
                }
                else
                {
                    for (j = 0; j < iIChannels; j++)
                    {
                        kVB.m_afChannel[ikVBIndex++] = m_afChannel[iIndex + j];
                    }
                }
            }

            for (iUnit = 0; iUnit < (int)rkIAttr.GetMaxColors(); iUnit++)
            {
                if (rkIAttr.HasColor(iUnit))
                {
                    iIChannels = rkIAttr.GetCChannels(iUnit);
                    iVBChannels = m_kAttributes.GetCChannels(iUnit);
                    iIndex = m_iVertexSize*i + m_kAttributes.GetCOffset(iUnit);                  
                    if (iVBChannels < iIChannels)
                    {
                        for (j = 0; j < iVBChannels; j++)
                        {
                            kVB.m_afChannel[ikVBIndex++] = m_afChannel[iIndex + j];
                        }
                        for (j = iVBChannels; j < iIChannels; j++)
                        {
                            // Fill with 1 so that the a-component is compatible
                            // with an opaque color.
                            kVB.m_afChannel[ikVBIndex++] = 1.0f;
                        }
                    }
                    else
                    {
                        for (j = 0; j < iIChannels; j++)
                        {
                            kVB.m_afChannel[ikVBIndex++] = m_afChannel[iIndex + j];
                        }
                    }
                }
            }

            for (iUnit = 0; iUnit < (int)rkIAttr.GetMaxTCoords(); iUnit++)
            {
                if (rkIAttr.HasTCoord(iUnit))
                {
                    iIChannels = rkIAttr.GetTChannels(iUnit);
                    iVBChannels = m_kAttributes.GetTChannels(iUnit);
                    iIndex = m_iVertexSize*i + m_kAttributes.GetTOffset(iUnit);                   
                    if (iVBChannels < iIChannels)
                    {
                        for (j = 0; j < iVBChannels; j++)
                        {
                            kVB.m_afChannel[ikVBIndex++] = m_afChannel[iIndex + j];
                        }
                        for (j = iVBChannels; j < iIChannels; j++)
                        {
                            // Fill with 0 so that the components are compatible
                            // with a higher-dimensional image embedded in a
                            // lower-dimensional one.
                            kVB.m_afChannel[ikVBIndex++] = 0.0f;
                        }
                    }
                    else
                    {
                        for (j = 0; j < iIChannels; j++)
                        {
                            kVB.m_afChannel[ikVBIndex++] = m_afChannel[iIndex + j];
                        }
                    }
                }
            }
        }
    }

    /** An application might want to vary the "active quantity" of vertices.
     * Use this function to do so.  It does not change the data storage,
     * only the m_iVertexQuantity member.  The caller is responsible for
     * saving the full quantity of vertices and resetting this when finished
     * with the vertex buffer.  The caller also should not pass in a quantity
     * that is larger than the original full quantity.
     * @param iVQuantity, set vertex quantity.
     */
    public final void SetVertexQuantity (int iVQuantity)
    {
        m_iVertexQuantity = iVQuantity;
    }

    /** Default constructor. */
    public VertexBuffer () 
    {
        m_kAttributes = new Attributes();
        m_iVertexQuantity = 0;
        m_iVertexSize = 0;
        m_iChannelQuantity = 0;
        m_afChannel = null;
    }


    /** The format of a single vertex in the buffer. */
    private Attributes m_kAttributes;

    /** The size of a single vertex in the buffer. */
    private int m_iVertexSize;

    /** The number of vertices in the buffer. */
    private int m_iVertexQuantity;

    /** The vertex buffer data size. */
    private int m_iChannelQuantity;  // = m_iVertexQuantity*m_iVertexSize
    /** The vertex buffer data. */
    private float[] m_afChannel;

    /**
     * Loads this object from the input parameter rkStream, using the input
     * Stream.Link to store the IDs of children objects of this object
     * for linking after all objects are loaded from the Stream.
     * @param rkStream, the Stream from which this object is being read.
     * @param pkLink, the Link class for storing the IDs of this object's
     * children objcts.
     */
    public void Load (Stream rkStream, Stream.Link pkLink)
    {
        super.Load(rkStream,pkLink);

        m_iVertexSize = rkStream.ReadInt();
        m_iVertexQuantity = rkStream.ReadInt();
        m_iChannelQuantity = rkStream.ReadInt();
        m_afChannel = new float[m_iChannelQuantity];
        rkStream.Read(m_iChannelQuantity,m_afChannel);

        int iPChannels = rkStream.ReadInt();
        m_kAttributes.SetPChannels(iPChannels);

        int iNChannels = rkStream.ReadInt();
        m_kAttributes.SetNChannels(iNChannels);

        int iMaxColors = rkStream.ReadInt();
        for (int i = 0; i < iMaxColors; i++)
        {
            int iCChannels = rkStream.ReadInt();
            m_kAttributes.SetCChannels(i,iCChannels);
        }

        int iMaxTCoords = rkStream.ReadInt();
        for (int i = 0; i < iMaxTCoords; i++)
        {
            int iTChannels = rkStream.ReadInt();
            m_kAttributes.SetTChannels(i,iTChannels);
        }
    }

    /**
     * Copies this objects children objects from the input Stream's HashTable,
     * based on the LinkID of the child stored in the pkLink paramter.
     * @param rkStream, the Stream where the child objects are stored.
     * @param pkLink, the Link class from which the child object IDs are read.
     */
    public void Link (Stream rkStream, Stream.Link pkLink)
    {
        super.Link(rkStream,pkLink);
    }

    /**
     * Registers this object with the input Stream parameter. All objects
     * streamed to disk are registered with the Stream so that a unique list
     * of objects is maintained.
     * @param rkStream, the Stream where the child objects are stored.
     * @return true if this object is registered, false if the object has
     * already been registered.
     */
    public boolean Register (Stream rkStream)
    {
        return super.Register(rkStream);
    }

    /**
     * Write this object and all it's children to the Stream.
     * @param rkStream, the Stream where the child objects are stored.
     */
    public void Save (Stream rkStream)
    {
        super.Save(rkStream);
        rkStream.Write(m_iVertexSize);
        rkStream.Write(m_iVertexQuantity);
        rkStream.Write(m_iChannelQuantity);
        rkStream.Write(m_iChannelQuantity,m_afChannel);

        rkStream.Write(m_kAttributes.GetPChannels());
        rkStream.Write(m_kAttributes.GetNChannels());
        rkStream.Write(m_kAttributes.GetMaxColors());

        for (int i = 0; i < m_kAttributes.GetMaxColors(); i++)
        {
            rkStream.Write(m_kAttributes.GetCChannels(i));
        }
        rkStream.Write(m_kAttributes.GetMaxTCoords());
        for (int i = 0; i < m_kAttributes.GetMaxTCoords(); i++)
        {
            rkStream.Write(m_kAttributes.GetTChannels(i));
        }
    }

    /**
     * Returns the size of this object and it's children on disk for the
     * current StreamVersion parameter.
     * @param rkVersion, the current version of the Stream file being created.
     * @return the size of this object on disk.
     */
    public int GetDiskUsed (StreamVersion rkVersion)
    {
        return super.GetDiskUsed(rkVersion) +
            Stream.SIZEOF_INT + //sizeof(m_iVertexSize) +
            Stream.SIZEOF_INT + //sizeof(m_iVertexQuantity) +
            Stream.SIZEOF_INT + //sizeof(m_iChannelQuantity) +
            m_iChannelQuantity*Stream.SIZEOF_FLOAT + //m_iChannelQuantity*sizeof(m_afChannel[0]) +
            4*Stream.SIZEOF_INT + //4*sizeof(int) +
            Stream.SIZEOF_INT*m_kAttributes.GetMaxColors() + //sizeof(int)*m_kAttributes.GetMaxColors() +
            Stream.SIZEOF_INT*m_kAttributes.GetMaxTCoords(); //sizeof(int)*m_kAttributes.GetMaxTCoords();
    }

    /**
     * Write this object into a StringTree for the scene-graph visualization.
     * @param acTitle, the header for this object in the StringTree.
     * @return StringTree containing a String-based representation of this
     * object and it's children.
     */
    public StringTree SaveStrings (final String acTitle)
    {
        StringTree pkTree = new StringTree();
        // strings
        pkTree.Append(StringTree.Format("VertexBuffer",GetName()));
        pkTree.Append(super.SaveStrings(null));
        pkTree.Append(StringTree.Format("vertex quantity = ",m_iVertexQuantity));
        pkTree.Append(StringTree.Format("vertex size = ",m_iVertexSize));

        pkTree.Append(StringTree.Format("p channels =",m_kAttributes.GetPChannels()));
        pkTree.Append(StringTree.Format("n channels =",m_kAttributes.GetNChannels()));

        pkTree.Append(StringTree.Format("c units =",m_kAttributes.GetMaxColors()));
        for (int i = 0; i < m_kAttributes.GetMaxColors(); i++)
        {
            String kPrefix = new String("c[" + i + "] channels =");
            pkTree.Append(StringTree.Format(kPrefix,m_kAttributes.GetCChannels(i)));
        }

        pkTree.Append(StringTree.Format("t units =",m_kAttributes.GetMaxTCoords()));
        for (int i = 0; i < m_kAttributes.GetMaxTCoords(); i++)
        {
            String kPrefix = new String("t[" + i + "] channels =");
            pkTree.Append(StringTree.Format(kPrefix,m_kAttributes.GetTChannels(i)));
        }

        // children
        pkTree.Append(StringTree.Format(acTitle,m_iChannelQuantity,m_afChannel));

        return pkTree;
    }
}
