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

import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;

public class IndexBuffer extends Bindable
    implements StreamInterface
{
    /** Create an IndexBuffer with the input number of indices.
     * @param iIQuantity, the number of int elements.
     */
    public IndexBuffer (int iIQuantity)
    {
        assert(iIQuantity > 0);

        m_iIQuantity = iIQuantity;
        m_aiIndex = new int[m_iIQuantity];
        m_iOffset = 0;
    }

    /** Copy constructor.
     * @param pkIBuffer, the IndexBuffer to copy into this.
     */
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

    /** Delete memory. */
    public void finalize ()
    {
        // Inform all renderers using this index buffer that it is being
        // destroyed.  This allows the renderer to free up any associated
        // resources.
        Release();

        m_aiIndex = null;
        super.finalize();
    }

    /** Direct access to the index buffer data.  The quantity is the number of
     * int elements.  The number of bytes for the entire index buffer is
     * GetIndexQuantity()*sizeof(int).
     * @return number of int elements.
     */
    public final int GetIndexQuantity ()
    {
        return m_iIQuantity;
    }

    /** Direct access to the index buffer data.  The quantity is the number of
     * int elements.  The number of bytes for the entire index buffer is
     * GetIndexQuantity()*sizeof(int).
     * @return index data array.
     */
    public final int[] GetData ()
    {
        return m_aiIndex;
    }

    /** An application might want to vary the "active quantity" of indices.
     * Use this function to do so.  It does not change the data storage,
     * only the m_iQuantity member.  The caller is responsible for saving the
     * full quantity of indices and resetting this when finished with the
     * index buffer.  The caller also should not pass in a quantity that is
     * larger than the original full quantity.
     * @param iIQuantity, number of int elements.
     */
    public final void SetIndexQuantity (int iIQuantity)
    {
        m_iIQuantity = iIQuantity;
    }

    /** The offset into the indices is used by the renderer for drawing.  The
     * ability to set this is useful when multiple geometric primitives share
     * an index buffer, each primitive using a continguous set of indices.  In
     * this case, SetIndexQuantity and SetOffset will be called dynamically
     * during the application for each such geometric primitive.
     * @param iOffset, index offset.
     */
    public void SetOffset (int iOffset)
    {
        assert(iOffset >= 0);
        m_iOffset = iOffset;
    }

    /** The offset into the indices is used by the renderer for drawing.  The
     * ability to set this is useful when multiple geometric primitives share
     * an index buffer, each primitive using a continguous set of indices.  In
     * this case, SetIndexQuantity and SetOffset will be called dynamically
     * during the application for each such geometric primitive.
     * @return index offset.
     */
    public final int GetOffset ()
    {
        return m_iOffset;
    }
    
    /** streaming support */
    public IndexBuffer ()
    {
        m_iIQuantity = 0;
        m_aiIndex = null;
        m_iOffset = 0;
    }

    /** Number of int elements. */
    protected int m_iIQuantity;
    /** Index data array. */
    protected int[] m_aiIndex;
    /** Index offset. */
    protected int m_iOffset;

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

        m_iIQuantity = rkStream.ReadInt();
        m_aiIndex = new int[m_iIQuantity];
        rkStream.Read(m_iIQuantity,m_aiIndex);

        if (rkStream.GetVersion().GreaterEqual( new StreamVersion(4,2)) )
        {
            m_iOffset = rkStream.ReadInt();
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
        rkStream.Write(m_iIQuantity);
        rkStream.Write(m_iIQuantity,m_aiIndex);
        rkStream.Write(m_iOffset);
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
            Stream.SIZEOF_INT + //sizeof(m_iIQuantity) +
            m_iIQuantity*Stream.SIZEOF_INT + //m_iIQuantity*sizeof(m_aiIndex[0]) +
            Stream.SIZEOF_INT; //sizeof(m_iOffset);
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
        pkTree.Append(StringTree.Format("IndexBuffer",GetName()));

        // children
        pkTree.Append(super.SaveStrings(null));
        pkTree.Append(StringTree.Format(acTitle,m_iIQuantity,m_aiIndex));

        return pkTree;
    }
}
