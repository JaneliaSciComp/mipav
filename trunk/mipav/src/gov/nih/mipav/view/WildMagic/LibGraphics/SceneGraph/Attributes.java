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

public class Attributes
{
    public Attributes ()
    {
        m_iChannelQuantity = 0;
        m_iPOffset = -1;
        m_iPChannels = 0;
        m_iNOffset = -1;
        m_iNChannels = 0;
    }

    public Attributes ( Attributes rkA )
    {
        m_iChannelQuantity = rkA.m_iChannelQuantity;
        m_iPOffset = rkA.m_iPOffset;
        m_iPChannels = rkA.m_iPChannels;
        m_iNOffset = rkA.m_iNOffset;
        m_iNChannels = rkA.m_iNChannels;


        for (int iUnit = 0; iUnit < rkA.m_kCChannels.size(); iUnit++)
        {
            SetCChannels( iUnit, rkA.m_kCChannels.get(iUnit) );
        }
        for (int iUnit = 0; iUnit < rkA.m_kTChannels.size(); iUnit++)
        {
            SetTChannels( iUnit, rkA.m_kTChannels.get(iUnit) );
        }
    }

    public void finalize()
    {
        // colors
        m_kCOffset.clear();
        m_kCOffset = null;
        m_kCChannels.clear();
        m_kCChannels = null;

        // texture coordinates
        m_kTOffset.clear();
        m_kTOffset = null;
        m_kTChannels.clear();
        m_kTChannels = null;
    }

    // Set the attributes you want.  The currently supported attributes are
    //    p  = position
    //         3 or 4 channels (xyz,xyzw)
    //    n  = normal vector
    //         3 or 4 channels (xyz,xyzw)
    //    ci = color
    //         1 to 4 channels (r,rg,rgb,rgba)
    //    ti = texture coordinate
    //         1 to 4 channels (s,st,str,strq)
    // The attrbitues are organized internally to have the ordering:
    // p, n, c0, c1, ..., t0, t1, ...

    public void SetPChannels (int iPChannels)
    {
        assert(0 <= iPChannels && iPChannels <= 4);

        m_iPChannels = iPChannels;
        UpdateOffsets();
    }

    public void SetNChannels (int iNChannels)
    {
        assert(0 <= iNChannels && iNChannels <= 4);

        m_iNChannels = iNChannels;
        UpdateOffsets();
    }

    public void SetCChannels (int iUnit, int iCChannels)
    {
        assert(0 <= iUnit && 0 <= iCChannels && iCChannels <= 4);

        m_kCChannels.add( iUnit, iCChannels );
        UpdateOffsets();
    }

    public void SetTChannels (int iUnit, int iTChannels)
    {
        assert(0 <= iUnit && 0 <= iTChannels && iTChannels <= 4);

        m_kTChannels.add( iUnit, iTChannels );
        UpdateOffsets();
    }


    // The number of 'float' channels used by all the attributes.
    public int GetChannelQuantity ()
    {
        return m_iChannelQuantity;
    }

    // Access to position information.
    public int GetPOffset ()
    {
        return m_iPOffset;
    }

    public int GetPChannels ()
    {
        return m_iPChannels;
    }

    public boolean HasPosition ()
    {
        return m_iPChannels > 0;
    }


    // Access to normal information.
    public int GetNOffset ()
    {
        return m_iNOffset;
    }

    public int GetNChannels ()
    {
        return m_iNChannels;
    }

    public boolean HasNormal ()
    {
        return m_iNChannels > 0;
    }


    // Access to color information.
    public int GetMaxColors ()
    {
        return (int)m_kCChannels.size();
    }

    public int GetCOffset (int iUnit)
    {
        if (0 <= iUnit && iUnit < (int)m_kCOffset.size())
        {
            return m_kCOffset.get(iUnit);
        }
        return -1;
    }

    public int GetCChannels (int iUnit)
    {
        if (0 <= iUnit && iUnit < (int)m_kCChannels.size())
        {
            return m_kCChannels.get(iUnit);
        }
        return 0;
    }

    public boolean HasColor (int iUnit)
    {
        if (0 <= iUnit && iUnit < (int)m_kCChannels.size())
        {
            return m_kCChannels.get(iUnit) > 0;
        }
        return false;
    }


    // Access to texture coordinate information.
    public int GetMaxTCoords ()
    {
        return (int)m_kTChannels.size();
    }

    public int GetTOffset (int iUnit)
    {
        if (0 <= iUnit && iUnit < (int)m_kTOffset.size())
        {
            return m_kTOffset.get(iUnit);
        }
        return -1;
    }

    public int GetTChannels (int iUnit)
    {
        if (0 <= iUnit && iUnit < (int)m_kTChannels.size())
        {
            return m_kTChannels.get(iUnit);
        }
        return 0;
    }

    public boolean HasTCoord (int iUnit)
    {
        if (0 <= iUnit && iUnit < (int)m_kTChannels.size())
        {
            return m_kTChannels.get(iUnit) > 0;
        }
        return false;
    }


    // Support for comparing vertex program outputs with pixel program inputs.
    public boolean Matches ( Attributes rkAttr, boolean bIncludeP, boolean bIncludeN,
                             boolean bIncludeC, boolean bIncludeT)
    {
        int i;

        if (bIncludeP)
        {
            if (m_iPChannels != rkAttr.m_iPChannels)
            {
                return false;
            }
        }

        if (bIncludeN)
        {
            if (m_iNChannels != rkAttr.m_iNChannels)
            {
                return false;
            }
        }

        if (bIncludeC)
        {
            if (m_kCChannels.size() != rkAttr.m_kCChannels.size())
            {
                return false;
            }
            for (i = 0; i < (int)m_kCChannels.size(); i++)
            {
                if (m_kCChannels.get(i) != rkAttr.m_kCChannels.get(i))
                {
                    return false;
                }
            }
        }

        if (bIncludeT)
        {
            if (m_kTChannels.size() != rkAttr.m_kTChannels.size())
            {
                return false;
            }
            for (i = 0; i < (int)m_kTChannels.size(); i++)
            {
                if (m_kTChannels.get(i) != rkAttr.m_kTChannels.get(i))
                {
                    return false;
                }
            }
        }

        return true;
    }


    private void UpdateOffsets ()
    {
        m_iChannelQuantity = 0;
        m_iPOffset = -1;
        m_iNOffset = -1;
        m_kCOffset.setSize(m_kCChannels.size());
        m_kTOffset.setSize(m_kTChannels.size());

        if (m_iPChannels > 0)
        {
            m_iPOffset = m_iChannelQuantity;
            m_iChannelQuantity += m_iPChannels;
        }

        if (m_iNChannels > 0)
        {
            m_iNOffset = m_iChannelQuantity;
            m_iChannelQuantity += m_iNChannels;
        }

        int i;
        for (i = 0; i < (int)m_kCChannels.size(); i++)
        {
            if (m_kCChannels.get(i) > 0)
            {
                m_kCOffset.set(i, m_iChannelQuantity);
                m_iChannelQuantity += m_kCChannels.get(i);
            }
        }

        for (i = 0; i < (int)m_kTChannels.size(); i++)
        {
            if (m_kTChannels.get(i) > 0)
            {
                m_kTOffset.set(i, m_iChannelQuantity);
                m_iChannelQuantity += m_kTChannels.get(i);
            }
        }
    }


    // The number of 'float' channels used by all attributes.
    private int m_iChannelQuantity;

    // positions
    private int m_iPOffset;
    private int m_iPChannels;

    // normals
    private int m_iNOffset;
    private int m_iNChannels;

    // colors
    private Vector<Integer> m_kCOffset = new Vector<Integer>();
    private Vector<Integer> m_kCChannels = new Vector<Integer>();

    // texture coordinates
    private Vector<Integer> m_kTOffset = new Vector<Integer>();
    private Vector<Integer> m_kTChannels = new Vector<Integer>();
}
