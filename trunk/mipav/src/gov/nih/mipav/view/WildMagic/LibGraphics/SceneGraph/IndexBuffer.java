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
// Version: 4.0.1 (2006/11/24)

package gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph;

import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;

public class IndexBuffer extends Bindable
    implements StreamInterface
{
    public IndexBuffer (int iIQuantity)
    {
        assert(iIQuantity > 0);

        m_iIQuantity = iIQuantity;
        m_aiIndex = new int[m_iIQuantity];
        m_iOffset = 0;
    }

    public IndexBuffer (IndexBuffer pkIBuffer)
    {
        assert(pkIBuffer != null);
        m_iIQuantity = pkIBuffer.m_iIQuantity;
        m_aiIndex = new int[m_iIQuantity];
        for ( int i = 0; i < m_iIQuantity; i++ )
        {
            m_aiIndex[i] = pkIBuffer.m_aiIndex[i];
        }
        m_iOffset = pkIBuffer.m_iOffset;
    }

    public void finalize ()
    {
        // Inform all renderers using this index buffer that it is being
        // destroyed.  This allows the renderer to free up any associated
        // resources.
        Release();

        m_aiIndex = null;
        super.finalize();
    }

    // Direct access to the index buffer data.  The quantity is the number of
    // int elements.  The number of bytes for the entire index buffer is
    // GetIndexQuantity()*sizeof(int).
    public int GetIndexQuantity ()
    {
        return m_iIQuantity;
    }

    public int[] GetData ()
    {
        return m_aiIndex;
    }

    // An application might want to vary the "active quantity" of indices.
    // Use this function to do so.  It does not change the data storage,
    // only the m_iQuantity member.  The caller is responsible for saving the
    // full quantity of indices and resetting this when finished with the
    // index buffer.  The caller also should not pass in a quantity that is
    // larger than the original full quantity.
    public void SetIndexQuantity (int iIQuantity)
    {
        m_iIQuantity = iIQuantity;
    }

    // The offset into the indices is used by the renderer for drawing.  The
    // ability to set this is useful when multiple geometric primitives share
    // an index buffer, each primitive using a continguous set of indices.  In
    // this case, SetIndexQuantity and SetOffset will be called dynamically
    // during the application for each such geometric primitive.
    public void SetOffset (int iOffset)
    {
        assert(iOffset >= 0);
        m_iOffset = iOffset;
    }

    public int GetOffset ()
    {
        return m_iOffset;
    }

    // streaming support
    public IndexBuffer ()
    {
        m_iIQuantity = 0;
        m_aiIndex = null;
        m_iOffset = 0;
    }

    protected int m_iIQuantity;
    protected int[] m_aiIndex;
    protected int m_iOffset;

    public void Load (Stream rkStream, Stream.Link pkLink)
    {
        super.Load(rkStream,pkLink);

        m_iIQuantity = rkStream.ReadInt();
        m_aiIndex = new int[m_iIQuantity];
        rkStream.Read(m_iIQuantity,m_aiIndex);

        if (rkStream.GetVersion().GreaterEqual( new StreamVersion(4,2)) )
        {
            m_iOffset = rkStream.ReadInt();
        }
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
        rkStream.Write(m_iIQuantity);
        rkStream.Write(m_iIQuantity,m_aiIndex);
        rkStream.Write(m_iOffset);
    }

    public int GetDiskUsed (StreamVersion rkVersion)
    {
        return super.GetDiskUsed(rkVersion) +
            Stream.SIZEOF_INT + //sizeof(m_iIQuantity) +
            m_iIQuantity*Stream.SIZEOF_INT + //m_iIQuantity*sizeof(m_aiIndex[0]) +
            Stream.SIZEOF_INT; //sizeof(m_iOffset);
    }

    public StringTree SaveStrings (final String acTitle)
    {
        StringTree pkTree = new StringTree();
        // strings
        pkTree.Append(StringTree.Format("IndexBuffer",GetName()));

        // children
        pkTree.Append(super.SaveStrings(null));
        pkTree.Append(StringTree.Format(acTitle,m_iIQuantity,m_aiIndex));

        return pkTree;
    }
}
