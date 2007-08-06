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

public class Polyline extends Geometry
    implements StreamInterface
{
    // construction and destruction
    public Polyline (VertexBuffer pkVBuffer, boolean bClosed, boolean bContiguous)
    {
        super(pkVBuffer,null);
        int iVQuantity = VBuffer.GetVertexQuantity();
        assert(iVQuantity >= 2);

        m_iActiveQuantity = iVQuantity;
        m_bClosed = bClosed;
        m_bContiguous = bContiguous;

        // One extra slot is allocated for the indices to allow for closed
        // polylines.  In that case, aiIndex[iVQuantity] is set to aiIndex[0].
        IBuffer = new IndexBuffer(iVQuantity+1);
        int[] aiIndex = IBuffer.GetData();
        for (int i = 0; i < iVQuantity; i++)
        {
            aiIndex[i] = i;
        }
        aiIndex[iVQuantity] = 0;

        if (!m_bContiguous || !m_bClosed)
        {
            // If the curve starts out not closed, reduce the index quantity so
            // that the renderer ignores the extra index.
            IBuffer.SetIndexQuantity(iVQuantity);
        }

        SetGeometryType();
    }

    // member access
    public void SetActiveQuantity (int iActiveQuantity)
    {
        int iVQuantity = VBuffer.GetVertexQuantity();
        if (0 <= iActiveQuantity && iActiveQuantity <= iVQuantity)
        {
            m_iActiveQuantity = iActiveQuantity;
        }
        else
        {
            m_iActiveQuantity = iVQuantity;
        }

        IBuffer.SetIndexQuantity(m_iActiveQuantity);
    }

    public int GetActiveQuantity ()
    {
        return m_iActiveQuantity;
    }


    public void SetClosed (boolean bClosed)
    {
        m_bClosed = bClosed;
        SetGeometryType();
    }

    public boolean GetClosed ()
    {
        return m_bClosed;
    }

    public void SetContiguous (boolean bContiguous)
    {
        m_bContiguous = bContiguous;
        SetGeometryType();
    }

    public boolean GetContiguous ()
    {
        return m_bContiguous;
    }

    protected Polyline ()
    {
        m_iActiveQuantity = 0;
        m_bClosed = false;
        m_bContiguous = false;
        SetGeometryType();
    }

    protected void SetGeometryType ()
    {
        if (m_bContiguous)
        {
            if (m_bClosed)
            {
                if (Type != GeometryType.GT_POLYLINE_CLOSED)
                {
                    // Increase the index quantity to account for closing the
                    // polyline.
                    IBuffer.SetIndexQuantity(IBuffer.GetIndexQuantity()+1);
                    IBuffer.Release();
                }
                Type = GeometryType.GT_POLYLINE_CLOSED;
            }
            else
            {
                if (Type == GeometryType.GT_POLYLINE_CLOSED)
                {
                    // Decrease the index quantity to account for closing the
                    // polyline.
                    IBuffer.SetIndexQuantity(IBuffer.GetIndexQuantity()-1);
                    IBuffer.Release();
                }
                Type = GeometryType.GT_POLYLINE_OPEN;
            }
        }
        else
        {
            if (Type == GeometryType.GT_POLYLINE_CLOSED)
            {
                // Decrease the index quantity to account for closing the
                // polyline.
                IBuffer.SetIndexQuantity(IBuffer.GetIndexQuantity()-1);
                IBuffer.Release();
            }
            Type = GeometryType.GT_POLYLINE_SEGMENTS;
        }
    }

    // streaming
    public void Load (Stream rkStream, Stream.Link pkLink)
    {
        super.Load(rkStream,pkLink);

        // native data
        m_iActiveQuantity = rkStream.ReadInt();
        m_bClosed = rkStream.ReadBoolean();
        m_bContiguous = rkStream.ReadBoolean();

        SetGeometryType();
    }

    public void Link (Stream rkStream, Stream.Link pkLink)
    {
        super.Link(rkStream,pkLink);
    }

    public boolean Register (Stream rkStream)
    {
        return super.Register(rkStream);
    }

    public void Save (Stream rkStream)
    {
        super.Save(rkStream);

        // native data
        rkStream.Write(m_iActiveQuantity);
        rkStream.Write(m_bClosed);
        rkStream.Write(m_bContiguous);
    }

    public int GetDiskUsed (final StreamVersion rkVersion)
    {
        return super.GetDiskUsed(rkVersion) +
            Stream.SIZEOF_INT + //sizeof(m_iActiveQuantity) +
            Stream.SIZEOF_BOOLEAN + //sizeof(char) + // m_bClosed
            Stream.SIZEOF_BOOLEAN; //sizeof(char);  // m_bContiguous
    }

    public StringTree SaveStrings (final String acTitle)
    {
        StringTree pkTree = new StringTree();

        // strings
        pkTree.Append(StringTree.Format("Polyline",GetName()));
        pkTree.Append(StringTree.Format("active quantity =",m_iActiveQuantity));
        pkTree.Append(StringTree.Format("closed =",m_bClosed));
        pkTree.Append(StringTree.Format("contiguous =",m_bContiguous));

        // children
        pkTree.Append(super.SaveStrings(acTitle));

        return pkTree;
    }

    // Allow application to specify fewer than the maximum number of vertices
    // to draw.
    protected int m_iActiveQuantity;

    // polyline is open or closed, contiguous or disjoint segments
    protected boolean m_bClosed, m_bContiguous;
}
