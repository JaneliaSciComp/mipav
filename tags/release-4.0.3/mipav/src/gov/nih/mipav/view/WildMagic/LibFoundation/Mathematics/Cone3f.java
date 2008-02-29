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

public class Cone3f
{
    // An acute cone is Dot(A,X-V) = |X-V| cos(T) where V is the vertex, A
    // is the unit-length direction of the axis of the cone, and T is the
    // cone angle with 0 < T < PI/2.  The cone interior is defined by the
    // inequality Dot(A,X-V) >= |X-V| cos(T).  Since cos(T) > 0, we can avoid
    // computing square roots.  The solid cone is defined by the inequality
    // Dot(A,X-V)^2 >= Dot(X-V,X-V) cos(T)^2.

    // construction
    public Cone3f ()
    {}

    public Cone3f (final Vector3f rkVertex, final Vector3f rkAxis,
                   float fAngle)
    {
        Vertex = rkVertex;
        Axis = rkAxis;
        CosAngle = (float)Math.cos(fAngle);
        SinAngle = (float)Math.sin(fAngle);
    }

    public Cone3f (final Vector3f rkVertex, final Vector3f rkAxis,
                   float fCosAngle, float fSinAngle)
    {
        Vertex = rkVertex;
        Axis = rkAxis;
        CosAngle = fCosAngle;
        SinAngle = fSinAngle;
    }


    public Vector3f Vertex;
    public Vector3f Axis;
    public float CosAngle, SinAngle;  // cos(T), sin(T)
};
