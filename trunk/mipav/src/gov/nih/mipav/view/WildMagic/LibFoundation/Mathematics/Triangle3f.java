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

/** The triangle is represented as an array of three vertices, V0, V1,
 * and V2.
 */
public class Triangle3f
{
    // construction
    public Triangle3f () {}  // uninitialized
    public Triangle3f (final Vector3f rkV0, final Vector3f rkV1,
                       final Vector3f rkV2)
    {
        V[0] = rkV0;
        V[1] = rkV1;
        V[2] = rkV2;
    }

    public Triangle3f (final Vector3f[] akV)
    {
        for (int i = 0; i < 3; i++)
        {
            V[i] = akV[i];
        }
    }


    /** distance from the point Q to the triangle */
    public float DistanceTo (final Vector3f rkQ)
    {
        Vector3f kDiff = V[0].sub( rkQ );
        Vector3f kE0 = V[1].sub( V[0] );
        Vector3f kE1 = V[2].sub( V[0] );
        float fA00 = kE0.SquaredLength();
        float fA01 = kE0.Dot(kE1);
        float fA11 = kE1.SquaredLength();
        float fB0 = kDiff.Dot(kE0);
        float fB1 = kDiff.Dot(kE1);
        float fC = kDiff.SquaredLength();
        float fDet = Math.abs(fA00*fA11-fA01*fA01);
        float fS = fA01*fB1-fA11*fB0;
        float fT = fA01*fB0-fA00*fB1;
        float fSqrDist;

        if (fS + fT <= fDet)
        {
            if (fS < (float)0.0)
            {
                if (fT < (float)0.0)  // region 4
                {
                    if (fB0 < (float)0.0)
                    {
                        if (-fB0 >= fA00)
                        {
                            fSqrDist = fA00+((float)2.0)*fB0+fC;
                        }
                        else
                        {
                            fSqrDist = fC-fB0*fB0/fA00;
                        }
                    }
                    else
                    {
                        if (fB1 >= (float)0.0)
                        {
                            fSqrDist = fC;
                        }
                        else if (-fB1 >= fA11)
                        {
                            fSqrDist = fA11+((float)2.0)*fB1+fC;
                        }
                        else
                        {
                            fSqrDist = fC-fB1*fB1/fA11;
                        }
                    }
                }
                else  // region 3
                {
                    if (fB1 >= (float)0.0)
                    {
                        fSqrDist = fC;
                    }
                    else if (-fB1 >= fA11)
                    {
                        fSqrDist = fA11+((float)2.0)*fB1+fC;
                    }
                    else
                    {
                        fSqrDist = fC-fB1*fB1/fA11;
                    }
                }
            }
            else if (fT < (float)0.0)  // region 5
            {
                if (fB0 >= (float)0.0)
                {
                    fSqrDist = fC;
                }
                else if (-fB0 >= fA00)
                {
                    fSqrDist = fA00+((float)2.0)*fB0+fC;
                }
                else
                {
                    fSqrDist = fC-fB0*fB0/fA00;
                }
            }
            else  // region 0
            {
                // minimum at interior point
                float fInvDet = ((float)1.0)/fDet;
                fS *= fInvDet;
                fT *= fInvDet;
                fSqrDist = fS*(fA00*fS+fA01*fT+((float)2.0)*fB0) +
                    fT*(fA01*fS+fA11*fT+((float)2.0)*fB1)+fC;
            }
        }
        else
        {
            float fTmp0, fTmp1, fNumer, fDenom;

            if (fS < (float)0.0)  // region 2
            {
                fTmp0 = fA01 + fB0;
                fTmp1 = fA11 + fB1;
                if (fTmp1 > fTmp0)
                {
                    fNumer = fTmp1 - fTmp0;
                    fDenom = fA00-2.0f*fA01+fA11;
                    if (fNumer >= fDenom)
                    {
                        fSqrDist = fA00+((float)2.0)*fB0+fC;
                    }
                    else
                    {
                        fS = fNumer/fDenom;
                        fT = (float)1.0 - fS;
                        fSqrDist = fS*(fA00*fS+fA01*fT+2.0f*fB0) +
                            fT*(fA01*fS+fA11*fT+((float)2.0)*fB1)+fC;
                    }
                }
                else
                {
                    if (fTmp1 <= (float)0.0)
                    {
                        fSqrDist = fA11+((float)2.0)*fB1+fC;
                    }
                    else if (fB1 >= (float)0.0)
                    {
                        fSqrDist = fC;
                    }
                    else
                    {
                        fSqrDist = fC-fB1*fB1/fA11;
                    }
                }
            }
            else if (fT < (float)0.0)  // region 6
            {
                fTmp0 = fA01 + fB1;
                fTmp1 = fA00 + fB0;
                if (fTmp1 > fTmp0)
                {
                    fNumer = fTmp1 - fTmp0;
                    fDenom = fA00-((float)2.0)*fA01+fA11;
                    if (fNumer >= fDenom)
                    {
                        fT = (float)1.0;
                        fS = (float)0.0;
                        fSqrDist = fA11+((float)2.0)*fB1+fC;
                    }
                    else
                    {
                        fT = fNumer/fDenom;
                        fS = (float)1.0 - fT;
                        fSqrDist = fS*(fA00*fS+fA01*fT+((float)2.0)*fB0) +
                            fT*(fA01*fS+fA11*fT+((float)2.0)*fB1)+fC;
                    }
                }
                else
                {
                    if (fTmp1 <= (float)0.0)
                    {
                        fSqrDist = fA00+((float)2.0)*fB0+fC;
                    }
                    else if (fB0 >= (float)0.0)
                    {
                        fSqrDist = fC;
                    }
                    else
                    {
                        fSqrDist = fC-fB0*fB0/fA00;
                    }
                }
            }
            else  // region 1
            {
                fNumer = fA11 + fB1 - fA01 - fB0;
                if (fNumer <= (float)0.0)
                {
                    fSqrDist = fA11+((float)2.0)*fB1+fC;
                }
                else
                {
                    fDenom = fA00-2.0f*fA01+fA11;
                    if (fNumer >= fDenom)
                    {
                        fSqrDist = fA00+((float)2.0)*fB0+fC;
                    }
                    else
                    {
                        fS = fNumer/fDenom;
                        fT = (float)1.0 - fS;
                        fSqrDist = fS*(fA00*fS+fA01*fT+((float)2.0)*fB0) +
                            fT*(fA01*fS+fA11*fT+((float)2.0)*fB1)+fC;
                    }
                }
            }
        }

        return (float)Math.sqrt(Math.abs(fSqrDist));
    }

        public Vector3f[] V = new Vector3f[3];
}
