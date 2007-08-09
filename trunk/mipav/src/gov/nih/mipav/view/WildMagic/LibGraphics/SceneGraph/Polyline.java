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
    /** construction
     * @param pkVBuffer, VertexBuffer containing Polyline data.
     * @param bClosed, when true polyline is closed, when false polyline is open.
     * @param bContiguous, when true polyline is contiguous or when false
     * polyline is disjoint segments
     */
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

    /** Set the number of active points.
     * @param iActiveQuantity the number of active points.
     */
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

    /** Get the number of active points.
     * @return the number of active points.
     */
    public final int GetActiveQuantity ()
    {
        return m_iActiveQuantity;
    }

    /** Set closed value.
     * @param bClosed polyline closed value.
     */
    public void SetClosed (boolean bClosed)
    {
        m_bClosed = bClosed;
        SetGeometryType();
    }

    /** Get closed value.
     * @return polyline closed value.
     */
    public final boolean GetClosed ()
    {
        return m_bClosed;
    }

    /** Set contiguous value.
     * @param bContiguous polyline contiguous value.
     */
    public void SetContiguous (boolean bContiguous)
    {
        m_bContiguous = bContiguous;
        SetGeometryType();
    }

    /** Get contiguous value.
     * @return polyline contiguous value.
     */
    public final boolean GetContiguous ()
    {
        return m_bContiguous;
    }

    /** Default constructor. */
    protected Polyline ()
    {
        m_iActiveQuantity = 0;
        m_bClosed = false;
        m_bContiguous = false;
        SetGeometryType();
    }

    /** Sets the geometric type based on the closed and contiguous values. */
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

        // native data
        m_iActiveQuantity = rkStream.ReadInt();
        m_bClosed = rkStream.ReadBoolean();
        m_bContiguous = rkStream.ReadBoolean();

        SetGeometryType();
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

        // native data
        rkStream.Write(m_iActiveQuantity);
        rkStream.Write(m_bClosed);
        rkStream.Write(m_bContiguous);
    }

    /**
     * Returns the size of this object and it's children on disk for the
     * current StreamVersion parameter.
     * @param rkVersion, the current version of the Stream file being created.
     * @return the size of this object on disk.
     */
    public int GetDiskUsed (final StreamVersion rkVersion)
    {
        return super.GetDiskUsed(rkVersion) +
            Stream.SIZEOF_INT + //sizeof(m_iActiveQuantity) +
            Stream.SIZEOF_BOOLEAN + //sizeof(char) + // m_bClosed
            Stream.SIZEOF_BOOLEAN; //sizeof(char);  // m_bContiguous
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
        pkTree.Append(StringTree.Format("Polyline",GetName()));
        pkTree.Append(StringTree.Format("active quantity =",m_iActiveQuantity));
        pkTree.Append(StringTree.Format("closed =",m_bClosed));
        pkTree.Append(StringTree.Format("contiguous =",m_bContiguous));

        // children
        pkTree.Append(super.SaveStrings(acTitle));

        return pkTree;
    }

    /** Allow application to specify fewer than the maximum number of vertices
     * to draw.
     */
    protected int m_iActiveQuantity;

    /** polyline is open or closed */
    protected boolean m_bClosed;
    /** polyline is contiguous or disjoint segments */
    protected boolean m_bContiguous;
}
