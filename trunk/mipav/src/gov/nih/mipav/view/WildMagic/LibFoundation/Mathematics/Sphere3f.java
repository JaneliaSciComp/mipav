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

package gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics;

/** The sphere is represented as |X-C| = R where C is the center and R is
 * the radius.
 */
public class Sphere3f
{
    /** Constructor: uninitialized */
    public Sphere3f () {}  

    /** Constructor 
     * @param rkCenter, sphere center
     * @param fRadius, sphere radius
     */
    public Sphere3f ( Vector3f rkCenter, float fRadius)
    {
        Center = new Vector3f(rkCenter);
        Radius = fRadius;
    }

    /** Copy Constructor 
     * @param rkSphere, sphere to copy.
     */
    public Sphere3f ( Sphere3f rkSphere )
    {
        Center = new Vector3f(rkSphere.Center);
        Radius = rkSphere.Radius;
    }

    /**
     * delete memory
     */
    public void finalize()
    {
        Center.finalize();
        Center = null;
    }

    /** Copy 
     * @param rkSphere, sphere to copy.
     */
    public void Copy ( Sphere3f rkSphere )
    {
        Center = new Vector3f(rkSphere.Center);
        Radius = rkSphere.Radius;
    }

    /**
     * Creates a sphere containing all input points, with the center of the
     * sphere the center of the points.
     * @param iQuantity, number of input points.
     * @param akPoint, input points
     * @return new sphere containing all points.
     */
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
        Vector3f kDiff = new Vector3f();
        for (i = 0; i < iQuantity; i++)
        {
            akPoint[i].sub( kSphere.Center, kDiff );
            float fRadiusSqr = kDiff.SquaredLength();
            if (fRadiusSqr > kSphere.Radius)
            {
                kSphere.Radius = fRadiusSqr;
            }
        }
        kDiff = null;
        kSphere.Radius = (float)Math.sqrt(kSphere.Radius);
        return kSphere;
    }

    /**
     * Test if input point is inside input sphere.
     * @param rkPoint, point
     * @param rkSphere, sphere
     * @return true if point is inside sphere.
     */
    public static boolean InSphere (final Vector3f rkPoint, final Sphere3f rkSphere)
    {
        Vector3f kDiff = new Vector3f();
        rkPoint.sub( rkSphere.Center, kDiff );
        boolean bReturn = (kDiff.Length() <= rkSphere.Radius);
        kDiff = null;
        return bReturn;
    }

    /** Merge the two input spheres, return result.
     * @param rkSphere0, sphere0 to merge
     * @param rkSphere1, sphere1 to merge
     * @return sphere result of merge
     */
    public static Sphere3f MergeSpheres (final Sphere3f rkSphere0,
                                         final Sphere3f rkSphere1)
    {
        Vector3f kCDiff = new Vector3f();
        rkSphere1.Center.sub( rkSphere0.Center, kCDiff );
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
            kCDiff.scaleEquals(fCoeff);
            rkSphere0.Center.add( kCDiff, kSphere.Center );
        }
        else
        {
            kSphere.Center = rkSphere0.Center;
        }

        kSphere.Radius = ((float)0.5)*(fLength + rkSphere0.Radius +
                                       rkSphere1.Radius);
        kCDiff = null;
        return kSphere;
    }

    /** Merge the two input spheres, return result.
     * @param rkSphere0, sphere0 to merge
     * @param rkSphere1, sphere1 to merge
     * @return sphere result of merge
     */
    public void MergeSpheres (final Sphere3f rkSphere)
    {
        float fX = Center.X() - rkSphere.Center.X();
        float fY = Center.Y() - rkSphere.Center.Y();
        float fZ = Center.Z() - rkSphere.Center.Z();
        float fLSqr = fX*fX + fY*fY + fZ*fZ;
        float fRDiff = rkSphere.Radius - Radius;
        float fRDiffSqr = fRDiff*fRDiff;

        if (fRDiffSqr >= fLSqr)
        {
            if( fRDiff >= (float)0.0 )
            {
                Center.SetData(rkSphere.Center);
                Radius = rkSphere.Radius;
            }
            return;
        }

        float fLength = (float)Math.sqrt(fLSqr);
        if (fLength > Mathf.ZERO_TOLERANCE)
        {
            float fCoeff = (fLength + fRDiff)/(((float)2.0)*fLength);
            Center.SetData( fX * fCoeff, fY * fCoeff, fZ * fCoeff);
            Center.addEquals( rkSphere.Center );
        }
        else
        {
            Center.SetData(rkSphere.Center);
        }

        Radius = ((float)0.5)*(fLength + rkSphere.Radius +
                                       Radius);
    }


    /** The sphere is represented as |X-C| = R where C is the center */
    public Vector3f Center;
    /** Sphere Radius*/
    public float Radius;
}
