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
// Version: 4.0.1 (2006/08/05)

package gov.nih.mipav.view.WildMagic.LibGraphics.Detail;

import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;

public class ClodMesh extends TriMesh
{

    // Construction and destruction.  ClodMesh will make a copy of the index
    // buffer, because it needs to be able to update the indices independently
    // when two or more ClodMesh objects share the same vertex buffer and
    // collapse records.
    public ClodMesh (VertexBuffer pkVBuffer, final IndexBuffer pkIBuffer,
                     CollapseRecordArray pkRecordArray)
    {
        super(pkVBuffer,null);
        assert(pkRecordArray != null);

        m_iTargetRecord = 0;
        m_iCurrentRecord = 0;
        m_spkRecordArray = pkRecordArray;

        // Make a copy of the indices.
        IBuffer = new IndexBuffer(pkIBuffer);
    }

    // LOD selection is based on manual selection by the application.  To
    // use distance from camera or screen space coverage, derive a class
    // from Wm4ClodMesh and override 'GetAutomatedTargetRecord'.
    public int GetRecordQuantity ()
    {
        return m_spkRecordArray.GetQuantity();
    }

    public int TargetRecord ()
    {
        return m_iTargetRecord;
    }
    

    public void TargetRecord ( int iTarget )
    {
        m_iTargetRecord = iTarget;
    }

    public int GetAutomatedTargetRecord ()
    {
        return m_iTargetRecord;
    }

    public int GetMaximumLOD() {
        return m_spkRecordArray.GetQuantity() - 1;
    }

    // Geometric updates.  The Draw method will call this update and adjust
    // the TriMesh quantities according to the current value of the target
    // record.  You can call this manually in an application that does not
    // need to display the mesh.
    public void SelectLevelOfDetail ()
    {
        CollapseRecord[] akRecord = m_spkRecordArray.GetData();

        // Get the target record.  The virtual function may be overridden by a
        // derived class to obtain a desired automated change in the target.
        int iTargetRecord = GetAutomatedTargetRecord();

        // Collapse the mesh, if necessary.
        int[] aiIndex = IBuffer.GetData();
        int i, iC;
        while (m_iCurrentRecord < iTargetRecord)
        {
            m_iCurrentRecord++;

            // Replace indices in the connectivity array.
            CollapseRecord rkRecord = akRecord[m_iCurrentRecord];
            for (i = 0; i < rkRecord.IQuantity; i++)
            {
                iC = rkRecord.Index[i];
                assert(aiIndex[iC] == rkRecord.VThrow);
                aiIndex[iC] = rkRecord.VKeep;
            }

            // Reduce the vertex count; the vertices are properly ordered.
            VBuffer.SetVertexQuantity(rkRecord.VQuantity);

            // Reduce the triangle count; the triangles are properly ordered.
            IBuffer.SetIndexQuantity(3*rkRecord.TQuantity);

            // The vertices are unchanged, so only the index buffer needs
            // refreshing.
            IBuffer.Release();
        }

        // Expand the mesh, if necessary.
        while (m_iCurrentRecord > iTargetRecord)
        {
            // Restore indices in the connectivity array.
            CollapseRecord rkRecord = akRecord[m_iCurrentRecord];
            for (i = 0; i < rkRecord.IQuantity; i++)
            {
                iC = rkRecord.Index[i];
                assert(aiIndex[iC] == rkRecord.VKeep);
                aiIndex[iC] = rkRecord.VThrow;
            }

            m_iCurrentRecord--;
            CollapseRecord rkPrevRecord = akRecord[m_iCurrentRecord];

            // Increase the vertex count; the vertices are properly ordered.
            VBuffer.SetVertexQuantity(rkPrevRecord.VQuantity);

            // Increase the triangle count; the triangles are properly ordered.
            IBuffer.SetIndexQuantity(3*rkPrevRecord.TQuantity);

            // The vertices are unchanged, so only the index buffer needs
            // refreshing.
            IBuffer.Release();
        }
    }


    protected ClodMesh ()
    {
        m_iTargetRecord = 0;
        m_iCurrentRecord = 0;
    }


    // culling
    protected void GetVisibleSet (Culler rkCuller, boolean bNoCull)
    {
        SelectLevelOfDetail();
        super.GetVisibleSet(rkCuller,bNoCull);
    }


    // selection of LOD
    protected int m_iCurrentRecord, m_iTargetRecord;
    protected CollapseRecordArray m_spkRecordArray;
}
