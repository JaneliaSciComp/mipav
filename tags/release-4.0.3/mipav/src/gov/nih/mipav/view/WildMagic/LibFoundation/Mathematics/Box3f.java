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

package gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics;

public class Box3f
{
    // A box has center C, axis directions U[0], U[1], and U[2] (all
    // unit-length vectors), and extents e[0], e[1], and e[2] (all nonnegative
    // numbers).  A point X = C+y[0]*U[0]+y[1]*U[1]+y[2]*U[2] is inside or
    // on the box whenever |y[i]| <= e[i] for all i.

    // construction
    public Box3f ()  // uninitialized
    {}

    public Box3f (final Vector3f rkCenter, final Vector3f[] akAxis,
                  final float[] afExtent)
    {
        Center = new Vector3f(rkCenter);
        for (int i = 0; i < 3; i++)
        {
            Axis[i] = new Vector3f(akAxis[i]);
            Extent[i] = afExtent[i];
        }
    }

    public Box3f (final Vector3f rkCenter, final Vector3f rkAxis0,
                  final Vector3f rkAxis1, final Vector3f rkAxis2,
                  float fExtent0, float fExtent1, float fExtent2)
    {
        Center = new Vector3f(rkCenter);
        Axis[0] = new Vector3f(rkAxis0);
        Axis[1] = new Vector3f(rkAxis1);
        Axis[2] = new Vector3f(rkAxis2);
        Extent[0] = fExtent0;
        Extent[1] = fExtent1;
        Extent[2] = fExtent2;
    }

    public void dispose()
    {
        Center = null;

        Axis[0] = null;
        Axis[1] = null;
        Axis[2] = null;
        Extent = null;
    }
    
    public void ComputeVertices (Vector3f[] akVertex)
    {
        Vector3f[] akEAxis =
            {
                Axis[0].scale(Extent[0]),
                Axis[1].scale(Extent[1]),
                Axis[2].scale(Extent[2])
            };

        akVertex[0] = Center.sub( akEAxis[0] ).sub( akEAxis[1] ).sub( akEAxis[2] );
        akVertex[1] = Center.add( akEAxis[0] ).sub( akEAxis[1] ).sub( akEAxis[2] );
        akVertex[2] = Center.add( akEAxis[0] ).add( akEAxis[1] ).sub( akEAxis[2] );
        akVertex[3] = Center.sub( akEAxis[0] ).add( akEAxis[1] ).sub( akEAxis[2] );
        akVertex[4] = Center.sub( akEAxis[0] ).sub( akEAxis[1] ).add( akEAxis[2] );
        akVertex[5] = Center.sub( akEAxis[0] ).sub( akEAxis[1] ).add( akEAxis[2] );
        akVertex[6] = Center.add( akEAxis[0] ).add( akEAxis[1] ).add( akEAxis[2] );
        akVertex[7] = Center.sub( akEAxis[0] ).add( akEAxis[1] ).add( akEAxis[2] );
    }


    public Vector3f Center;
    public Vector3f[] Axis = new Vector3f[3];  // must be an orthonormal set of vectors
    public float[] Extent = new float[3];         // must be nonnegative
}

