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
    public void finalize()
    {
        if ( m_kAttributes != null )
        {
            m_kAttributes.finalize();
            m_kAttributes = null;
        }
        m_afChannel = null;
        super.finalize();
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

    /** Access to positions.
     * @param i, vertex index.
     * @return position data at index i.
     */
    public float[] PositionTuple (int i)
    {
        if (m_kAttributes.HasPosition() && 0 <= i && i < m_iVertexQuantity)
        {
            int iVBChannels = m_kAttributes.GetPChannels();
            float[] afPosition = new float[iVBChannels];

            int iIndex = m_iVertexSize*i + m_kAttributes.GetPOffset();
            for ( int j = 0; j < iVBChannels; j++ )
            {
                afPosition[j] = m_afChannel[iIndex + j];
            }
            return afPosition;
        }
        return null;
    }

    /** Access to normals.
     * @param i, vertex index.
     * @return normal data at index i.
     */
    public float[] NormalTuple (int i)
    {
        if (m_kAttributes.HasNormal() && 0 <= i && i < m_iVertexQuantity)
        {
            int iVBChannels = m_kAttributes.GetNChannels();
            float[] afNormal = new float[iVBChannels];
            int iIndex = m_iVertexSize*i + m_kAttributes.GetNOffset();
            for ( int j = 0; j < iVBChannels; j++ )
            {
                afNormal[j] = m_afChannel[iIndex + j];
            }
            return afNormal;
        }
        return null;
    }

    /** Access to colors.
     * @param iUnit, color unit (1-4).
     * @param i, vertex index.
     * @return color data at index i.
     */
    public float[] ColorTuple (int iUnit, int i)
    {
        if (m_kAttributes.HasColor(iUnit) && 0 <= i && i < m_iVertexQuantity)
        {
            int iVBChannels = m_kAttributes.GetCChannels(iUnit);
            float[] afColor = new float[iVBChannels];
            int iIndex = m_iVertexSize*i + m_kAttributes.GetCOffset(iUnit);
            for ( int j = 0; j < iVBChannels; j++ )
            {
                afColor[j] = m_afChannel[iIndex + j];
            }
            return afColor;
        }
        return null;
    }

    /** Access to texture coordinates.
     * @param iUnit, texture coordinate unit (1-4).
     * @param i, vertex index.
     * @return texture coordinate data at index i.
     */
     public float[] TCoordTuple (int iUnit, int i)
    {
        if (m_kAttributes.HasTCoord(iUnit) && 0 <= i && i < m_iVertexQuantity)
        {
            int iVBChannels = m_kAttributes.GetTChannels(iUnit);
            float[] afTCoord = new float[iVBChannels];
            int iIndex = m_iVertexSize*i + m_kAttributes.GetTOffset(iUnit);
            for ( int j = 0; j < iVBChannels; j++ )
            {
                afTCoord[j] = m_afChannel[iIndex + j];
            }
            return afTCoord;
        }
        return null;
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

    /** Direct access to the vertex buffer data.
     * @return vertex buffer data.
     */
    public final float[] GetData ()
    {
        return m_afChannel;
    }

    /** Get the position at the given index. Use these accessors for
     * convenience.  No range checking is performed, so you should be sure
     * that the attribute exists and that the number of channels is correct.
     * @param i, vertex index.
     * @return position.
     */
    public Vector3f Position3 (int i)
    {
        assert(m_kAttributes.GetPChannels() == 3);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetPOffset();
        return new Vector3f( m_afChannel[ iIndex + 0 ],
                             m_afChannel[ iIndex + 1 ],
                             m_afChannel[ iIndex + 2 ] );
    }

    /** Set the position at the given index.
     * @param i, vertex index.
     * @param kP, new position.
     */
    public void Position3 (int i, Vector3f kP)
    {
        assert(m_kAttributes.GetPChannels() == 3);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetPOffset();
        m_afChannel[ iIndex + 0 ] = kP.X();
        m_afChannel[ iIndex + 1 ] = kP.Y();
        m_afChannel[ iIndex + 2 ] = kP.Z();
    }

    /** Get the normal at the given index. Use these accessors for
     * convenience.  No range checking is performed, so you should be sure
     * that the attribute exists and that the number of channels is correct.
     * @param i, vertex index.
     * @return normal.
     */
    public Vector3f Normal3 (int i)
    {
        assert(m_kAttributes.GetNChannels() == 3);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetNOffset();
        return new Vector3f( m_afChannel[ iIndex + 0 ],
                             m_afChannel[ iIndex + 1 ],
                             m_afChannel[ iIndex + 2 ] );
    }

    /** Set the normal at the given index.
     * @param i, vertex index.
     * @param kN, new normal.
     */
    public void Normal3 (int i, Vector3f kN)
    {
        assert(m_kAttributes.GetNChannels() == 3);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetNOffset();
        m_afChannel[ iIndex + 0 ] = kN.X();
        m_afChannel[ iIndex + 1 ] = kN.Y();
        m_afChannel[ iIndex + 2 ] = kN.Z();
    }
   
    /** Get the color at the given index. Use these accessors for convenience.
     * No range checking is performed, so you should be sure that the
     * attribute exists and that the number of channels is correct.
     * @param iUnit, color unit (1-4).
     * @param i, vertex index.
     * @return color.
     */
    public ColorRGB Color3 (int iUnit, int i)
    {
        assert(m_kAttributes.GetCChannels(iUnit) == 3);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetCOffset(iUnit);
        return new ColorRGB( m_afChannel[ iIndex + 0 ], 
                             m_afChannel[ iIndex + 1 ],
                             m_afChannel[ iIndex + 2 ] );
    }

    /** Set the color at the given index.
     * @param iUnit, color unit (1-4).
     * @param i, vertex index.
     * @param kC, new color.
     */
    public void Color3 (int iUnit, int i, ColorRGB kC)
    {
        assert(m_kAttributes.GetCChannels(iUnit) == 3);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetCOffset(iUnit);
        m_afChannel[ iIndex + 0 ] = kC.R(); 
        m_afChannel[ iIndex + 1 ] = kC.G();
        m_afChannel[ iIndex + 2 ] = kC.B();
    }

    /** Get the texture coordinate (1D) at the given index. Use these
     * accessors for convenience.  No range checking is performed, so you
     * should be sure that the attribute exists and that the number of
     * channels is correct.
     * @param iUnit, texture coordinate unit (1-4).
     * @param i, vertex index.
     * @return texture coordinate.
     */
    public float TCoord1 (int iUnit, int i)
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
    public void TCoord1 (int iUnit, int i, float fValue)
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
    public Vector2f TCoord2 (int iUnit, int i)
    {
        assert(m_kAttributes.GetTChannels(iUnit) == 2);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetTOffset(iUnit);
        return new Vector2f( m_afChannel[ iIndex + 0 ],
                             m_afChannel[ iIndex + 1 ] );
    }

    /** Set the texture coordiante (2D) at the given index.
     * @param iUnit, texture coordinate unit (1-4).
     * @param i, vertex index.
     * @param kTC, new texture coordinate.
     */
    public void TCoord2 (int iUnit, int i, Vector2f kTC)
    {
        assert(m_kAttributes.GetTChannels(iUnit) == 2);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetTOffset(iUnit);
        m_afChannel[ iIndex + 0 ] = kTC.X();
        m_afChannel[ iIndex + 1 ] = kTC.Y();
    }
 
    /** Get the texture coordinate (3D) at the given index. Use these
     * accessors for convenience.  No range checking is performed, so you
     * should be sure that the attribute exists and that the number of
     * channels is correct.
     * @param i, vertex index.
     * @return texture coordinate.
     */
    public Vector3f TCoord3 (int iUnit, int i)
    {
        assert(m_kAttributes.GetTChannels(iUnit) == 2);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetTOffset(iUnit);
        return new Vector3f( m_afChannel[ iIndex + 0 ],
                             m_afChannel[ iIndex + 1 ],
                             m_afChannel[ iIndex + 2 ] );
    }

    /** Set the texture coordiante (3D) at the given index.
     * @param iUnit, texture coordinate unit (1-4).
     * @param i, vertex index.
     * @param kTC, new texture coordinate.
     */
    public void TCoord3 (int iUnit, int i, Vector3f kTC)
    {
        assert(m_kAttributes.GetTChannels(iUnit) == 3);
        int iIndex = m_iVertexSize*i + m_kAttributes.GetTOffset(iUnit);
        m_afChannel[ iIndex + 0 ] = kTC.X();
        m_afChannel[ iIndex + 1 ] = kTC.Y();
        m_afChannel[ iIndex + 2 ] = kTC.Z();
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
        Vector<Float> kCompatible = new Vector<Float>();    
        int iUnit, iIChannels, iVBChannels;
        float[] afData;

        for (int i = 0, j; i < m_iVertexQuantity; i++)
        {
            if (rkIAttr.HasPosition())
            {
                iIChannels = rkIAttr.GetPChannels();
                iVBChannels = m_kAttributes.GetPChannels();
                afData = PositionTuple(i);
                if (iVBChannels < iIChannels)
                {
                    for (j = 0; j < iVBChannels; j++)
                    {
                        kCompatible.add(afData[j]);
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
                        kCompatible.add(afData[j]);
                    }
                }
            }

            if (rkIAttr.HasNormal())
            {
                iIChannels = rkIAttr.GetNChannels();
                iVBChannels = m_kAttributes.GetNChannels();
                afData = NormalTuple(i);
                if (iVBChannels < iIChannels)
                {
                    for (j = 0; j < iVBChannels; j++)
                    {
                        kCompatible.add(afData[j]);
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
                        kCompatible.add(afData[j]);
                    }
                }
            }

            for (iUnit = 0; iUnit < (int)rkIAttr.GetMaxColors(); iUnit++)
            {
                if (rkIAttr.HasColor(iUnit))
                {
                    iIChannels = rkIAttr.GetCChannels(iUnit);
                    iVBChannels = m_kAttributes.GetCChannels(iUnit);
                    afData = ColorTuple(iUnit,i);
                    if (iVBChannels < iIChannels)
                    {
                        for (j = 0; j < iVBChannels; j++)
                        {
                            kCompatible.add(afData[j]);
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
                            kCompatible.add(afData[j]);
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
                    afData = TCoordTuple(iUnit,i);
                    if (iVBChannels < iIChannels)
                    {
                        for (j = 0; j < iVBChannels; j++)
                        {
                            kCompatible.add(afData[j]);
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
                            kCompatible.add(afData[j]);
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
        float[] afData;

        int iIndex = 0;
        for (int i = 0, j; i < m_iVertexQuantity; i++)
        {
            if (rkIAttr.HasPosition())
            {
                iIChannels = rkIAttr.GetPChannels();
                iVBChannels = m_kAttributes.GetPChannels();
                afData = PositionTuple(i);
                if (iVBChannels < iIChannels)
                {
                    for (j = 0; j < iVBChannels; j++)
                    {
                        kVB.m_afChannel[iIndex++] = afData[j];
                    }
                    for (j = iVBChannels; j < iIChannels; j++)
                    {
                        // Fill with 1 so that the w-component is compatible with
                        // a homogeneous point.
                        kVB.m_afChannel[iIndex++] = 1.0f;
                    }
                }
                else
                {
                    for (j = 0; j < iIChannels; j++)
                    {
                        kVB.m_afChannel[iIndex++] = afData[j];
                    }
                }
            }

            if (rkIAttr.HasNormal())
            {
                iIChannels = rkIAttr.GetNChannels();
                iVBChannels = m_kAttributes.GetNChannels();
                afData = NormalTuple(i);
                if (iVBChannels < iIChannels)
                {
                    for (j = 0; j < iVBChannels; j++)
                    {
                        kVB.m_afChannel[iIndex++] = afData[j];
                    }
                    for (j = iVBChannels; j < iIChannels; j++)
                    {
                        // Fill with 0 so that the w-component is compatible with
                        // a homogeneous vector.
                        kVB.m_afChannel[iIndex++] = 0.0f;
                    }
                }
                else
                {
                    for (j = 0; j < iIChannels; j++)
                    {
                        kVB.m_afChannel[iIndex++] = afData[j];
                    }
                }
            }

            for (iUnit = 0; iUnit < (int)rkIAttr.GetMaxColors(); iUnit++)
            {
                if (rkIAttr.HasColor(iUnit))
                {
                    iIChannels = rkIAttr.GetCChannels(iUnit);
                    iVBChannels = m_kAttributes.GetCChannels(iUnit);
                    afData = ColorTuple(iUnit,i);
                    if (iVBChannels < iIChannels)
                    {
                        for (j = 0; j < iVBChannels; j++)
                        {
                            kVB.m_afChannel[iIndex++] = afData[j];
                        }
                        for (j = iVBChannels; j < iIChannels; j++)
                        {
                            // Fill with 1 so that the a-component is compatible
                            // with an opaque color.
                            kVB.m_afChannel[iIndex++] = 1.0f;
                        }
                    }
                    else
                    {
                        for (j = 0; j < iIChannels; j++)
                        {
                            kVB.m_afChannel[iIndex++] = afData[j];
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
                    afData = TCoordTuple(iUnit,i);
                    if (iVBChannels < iIChannels)
                    {
                        for (j = 0; j < iVBChannels; j++)
                        {
                            kVB.m_afChannel[iIndex++] = afData[j];
                        }
                        for (j = iVBChannels; j < iIChannels; j++)
                        {
                            // Fill with 0 so that the components are compatible
                            // with a higher-dimensional image embedded in a
                            // lower-dimensional one.
                            kVB.m_afChannel[iIndex++] = 0.0f;
                        }
                    }
                    else
                    {
                        for (j = 0; j < iIChannels; j++)
                        {
                            kVB.m_afChannel[iIndex++] = afData[j];
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
