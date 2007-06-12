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

package gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph;

import gov.nih.mipav.view.WildMagic.LibGraphics.Effects.*;

public class VisibleSet
{
    public VisibleSet (int iMaxQuantity, int iGrowBy)
    {
        m_akVisible = null;
        Resize(iMaxQuantity,iGrowBy);
    }

    public void finalize ()
    {
        m_akVisible = null;
        try {
            super.finalize();
        } catch (Throwable e) {
            e.printStackTrace();
        }
    }

    public int GetQuantity ()
    {
        return m_iQuantity;
    }

    public VisibleObject[] GetVisible ()
    {
        return m_akVisible;
    }

    public VisibleObject GetVisible (int i)
    {
        assert(0 <= i && i < m_iQuantity);
        return m_akVisible[i];
    }


    // Creates a VisibleObject from the input and appends it to the end of the
    // VisibleObject array.
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

    public void Clear ()
    {
        m_iQuantity = 0;
    }

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

    private static final int VS_DEFAULT_MAX_QUANTITY = 32;
    private static final int VS_DEFAULT_GROWBY = 32;

    private int m_iMaxQuantity, m_iGrowBy, m_iQuantity;
    private VisibleObject[] m_akVisible;
}
