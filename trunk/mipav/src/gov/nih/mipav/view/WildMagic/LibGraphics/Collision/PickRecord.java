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
// Version: 4.0.0 (2006/08/07)

package gov.nih.mipav.view.WildMagic.LibGraphics.Collision;

import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;

public class PickRecord
{
    // The intersected object.
    public Spatial Intersected;

    // The linear component is parameterized by P + t*D.  The T member is
    // the value of parameter t at the intersection point.
    public float T;

    // The index of the triangle that is intersected by the ray.
    public int Triangle;

    // The barycentric coordinates of the point of intersection.  All of the
    // coordinates are in [0,1] and b0 + b1 + b2 = 1.
    public float B0, B1, B2;

    // For sorting purposes.
//     bool operator== (const PickRecord& rkRecord) const;
//     bool operator!= (const PickRecord& rkRecord) const;
//     bool operator<  (const PickRecord& rkRecord) const;
//     bool operator<= (const PickRecord& rkRecord) const;
//     bool operator>  (const PickRecord& rkRecord) const;
//     bool operator>= (const PickRecord& rkRecord) const;
}
