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

package gov.nih.mipav.view.WildMagic.LibGraphics.Collision;

import java.util.Arrays;
import java.util.Comparator;
import java.util.Vector;

import gov.nih.mipav.view.WildMagic.LibFoundation.Containment.*;
import gov.nih.mipav.view.WildMagic.LibFoundation.Intersection.*;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;

public class BoxBVTree extends BoundingVolumeTree
{
    public BoxBVTree ()
    {
        super();
    }
    
    public BoxBVTree (final TriMesh pkMesh)
    {
        m_pkMesh = pkMesh;
        m_pkLChild = null;
        m_pkRChild = null;
        m_iTriangleQuantity = 0;
        m_aiTriangle = null;
    }

    public BoxBVTree (final TriMesh pkMesh, int iMaxTrisPerLeaf,
                      boolean bStoreInteriorTris)
    {
        super(BoundingVolume.BVType.BV_BOX,pkMesh,iMaxTrisPerLeaf,
              bStoreInteriorTris);
    }


    protected static BoundingVolume CreateModelBound (final TriMesh pkMesh, int i0,
                                                      int i1, int[] aiISplit, Line3f rkLine)
    {
        // tag vertices that are used in the submesh
        int iVQuantity = pkMesh.VBuffer.GetVertexQuantity();
        int[] aiIndex = pkMesh.IBuffer.GetData();
        boolean[] abValid = new boolean[iVQuantity];
        int i;
        for (i = i0; i <= i1; i++)
        {
            int j = 3*aiISplit[i];
            abValid[aiIndex[j++]] = true;
            abValid[aiIndex[j++]] = true;
            abValid[aiIndex[j++]] = true;
        }

        // Create a contiguous set of vertices in the submesh.
        Vector<Vector3f> kMeshVertices = new Vector<Vector3f>();
        for (i = 0; i < iVQuantity; i++)
        {
            if (abValid[i])
            {
                kMeshVertices.add(pkMesh.VBuffer.GetPosition3(i));
            }
        }
        abValid = null;

        BoxBV pkModelBound = new BoxBV();
        Vector3f[] akVertices = new Vector3f[kMeshVertices.size()];
        for (i = 0; i < kMeshVertices.size(); i++)
        {
            akVertices[i] = kMeshVertices.get(i);
        }
        pkModelBound.SetBox( ContBox3f.ContOrientedBox((int)kMeshVertices.size(),
                akVertices) );
        for (i = 0; i < kMeshVertices.size(); i++)
        {
            if ( !ContBox3f.InBox(akVertices[i],pkModelBound.GetBox()) )
            {
                System.err.println( "ContOrientedBox incorrect" );
            }
        }
        
        rkLine.Origin = new Vector3f(pkModelBound.GetBox().Center);
        rkLine.Direction = new Vector3f(pkModelBound.GetBox().Axis[2]);
        return pkModelBound;
    }

  
    protected static BoundingVolume CreateWorldBound ()
    {
        return new BoxBV();
    }

}
