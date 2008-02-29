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

import gov.nih.mipav.view.WildMagic.LibGraphics.Effects.*;

public class VisibleSet
{
    /** Construct a visible set with the maximum size and grow step.
     * @param iMaxQuantity, maximum size of the set.
     * @param iGrowBy, the amount to grow the set when it reaches maximum
     * size.
     */
    public VisibleSet (int iMaxQuantity, int iGrowBy)
    {
        m_akVisible = null;
        Resize(iMaxQuantity,iGrowBy);
    }

    /** Delete memory. */
    public void dispose ()
    {
        for ( int i = 0; i < m_iQuantity; i++ )
        {
            if ( m_akVisible[i] != null )
            {
                m_akVisible[i].dispose();
                m_akVisible[i] = null;
            }
        }
        m_akVisible = null;
    }

    /** Get the size of the set.
     * @return the size of the set.
     */
    public final int GetQuantity ()
    {
        return m_iQuantity;
    }

    /** Get the visible objects in the set.
     * @return the visible objects in the set.
     */
    public final VisibleObject[] GetVisible ()
    {
        return m_akVisible;
    }

    /** Get the visible object at the specified index.
     * @param i, index.
     * @return the visible object at the specified index.
     */
    public VisibleObject GetVisible (int i)
    {
        assert(0 <= i && i < m_iQuantity);
        return m_akVisible[i];
    }


    /** Creates a VisibleObject from the input and appends it to the end of the
     * VisibleObject array.
     * @param pkObject, Spatial object to add.
     * @param pkGlobalEffect, effect to add.
     */
    public void Insert (Spatial pkObject, Effect pkGlobalEffect)
    {
        // Increase the array storage if necessary.
        if (++m_iQuantity > m_iMaxQuantity)
        {
            int iNewMaxQuantity = m_iMaxQuantity + m_iGrowBy;
            VisibleObject[] akNewVisible = new VisibleObject[iNewMaxQuantity];
            for ( int i = 0; i < iNewMaxQuantity; i++ )
            {
                if ( i < (m_iQuantity-1) )
                {
                    akNewVisible[i] = m_akVisible[i];
                }
                else
                {
                    akNewVisible[i] = new VisibleObject();
                }
            }
            m_akVisible = akNewVisible;
            m_iMaxQuantity = iNewMaxQuantity;
        }

        // Append the potentially visible object to the set.
        int iIndex = m_iQuantity-1;
        m_akVisible[iIndex].Object = pkObject;
        m_akVisible[iIndex].GlobalEffect = pkGlobalEffect;
    }

    /** Clear the visible set. */
    public void Clear ()
    {
        m_iQuantity = 0;
    }

    /** Resize based on the maximum quantity and the grow step size.
     * @param iMaxQuantity, maximum size of the set.
     * @param iGrowBy, the amount to grow the set when it reaches maximum
     * size.
     */
    public void Resize (int iMaxQuantity, int iGrowBy)
    {
        if (iMaxQuantity > 0)
        {
            m_iMaxQuantity = iMaxQuantity;
        }
        else
        {
            m_iMaxQuantity = VS_DEFAULT_MAX_QUANTITY;
        }

        if (iGrowBy > 0)
        {
            m_iGrowBy = iGrowBy;
        }
        else
        {
            m_iGrowBy = VS_DEFAULT_GROWBY;
        }

        m_akVisible = null;
        m_iQuantity = 0;
        m_akVisible = new VisibleObject[m_iMaxQuantity];
        for ( int i = 0; i < m_iMaxQuantity; i++ )
        {
            m_akVisible[i] = new VisibleObject();
        }
    }

    /** Default maximum set size. */
    private static final int VS_DEFAULT_MAX_QUANTITY = 32;
    /** Default grow step size. */
    private static final int VS_DEFAULT_GROWBY = 32;
    /** Local maximum set size. */
    private int m_iMaxQuantity;
    /** Local grow step size. */
    private int m_iGrowBy;
    /** Current set size. */
    private int m_iQuantity;
    /** Set of visible objects. */
    private VisibleObject[] m_akVisible;
}
