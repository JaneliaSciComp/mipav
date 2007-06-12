// Geometric Tools, Inc.
// http://www.geometrictools.com
// Copyright (c) 1998-2006.  All Rights Reserved
//
// The Wild Magic Version 4 Foundation Library source code is supplied
// under the terms of the license agreement
//     http://www.geometrictools.com/License/Wm4FoundationLicense.pdf
// and may not be copied or disclosed except in accordance with the terms
// of that agreement.
//
// Version: 4.0.0 (2006/06/28)

package gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics;

/** The sphere is represented as |X-C| = R where C is the center and R is
 * the radius.
 */
public class Sphere3f
{
    public Sphere3f () {}  // uninitialized
    public Sphere3f ( Vector3f rkCenter, float fRadius)
    {
        Center = new Vector3f(rkCenter);
        Radius = fRadius;
    }

    public Sphere3f ( Sphere3f rkSphere )
    {
        Center = new Vector3f(rkSphere.Center);
        Radius = rkSphere.Radius;
    }

    public void Copy ( Sphere3f rkSphere )
    {
        Center = new Vector3f(rkSphere.Center);
        Radius = rkSphere.Radius;
    }

    public static Sphere3f ContSphereAverage (int iQuantity, final Vector3f[] akPoint)
    {
        Sphere3f kSphere = new Sphere3f();
        kSphere.Center = new Vector3f( akPoint[0] );
        int i;
        for (i = 1; i < iQuantity; i++)
        {
            kSphere.Center.addEquals(akPoint[i]);
        }
        kSphere.Center.divEquals( (float)iQuantity );

        for (i = 0; i < iQuantity; i++)
        {
            Vector3f kDiff = akPoint[i].sub( kSphere.Center );
            float fRadiusSqr = kDiff.SquaredLength();
            if (fRadiusSqr > kSphere.Radius)
            {
                kSphere.Radius = fRadiusSqr;
            }
        }

        kSphere.Radius = (float)Math.sqrt(kSphere.Radius);
        return kSphere;
    }

    public static boolean InSphere (final Vector3f rkPoint, final Sphere3f rkSphere)
    {
        Vector3f kDiff = rkPoint.sub( rkSphere.Center );
        return (kDiff.Length() <= rkSphere.Radius);
    }

    public static Sphere3f MergeSpheres (final Sphere3f rkSphere0,
                                         final Sphere3f rkSphere1)
    {
        Vector3f kCDiff = rkSphere1.Center.sub( rkSphere0.Center );
        float fLSqr = kCDiff.SquaredLength();
        float fRDiff = rkSphere1.Radius - rkSphere0.Radius;
        float fRDiffSqr = fRDiff*fRDiff;

        if (fRDiffSqr >= fLSqr)
        {
            return ( fRDiff >= (float)0.0 ? rkSphere1 : rkSphere0 );
        }

        float fLength = (float)Math.sqrt(fLSqr);
        Sphere3f kSphere = new Sphere3f();

        if (fLength > Mathf.ZERO_TOLERANCE)
        {
            float fCoeff = (fLength + fRDiff)/(((float)2.0)*fLength);
            kSphere.Center = rkSphere0.Center.add( kCDiff.scale(fCoeff) );
        }
        else
        {
            kSphere.Center = rkSphere0.Center;
        }

        kSphere.Radius = ((float)0.5)*(fLength + rkSphere0.Radius +
                                       rkSphere1.Radius);

        return kSphere;
    }


    public Vector3f Center;
    public float Radius;
}
