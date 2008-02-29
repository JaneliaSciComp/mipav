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

import java.util.Random;

public final class Mathf
{
    /** Calculation of 1/sqrt(fValue)
     * @param fValue, value to calculate 1/sqrt(fValue)
     * @return 1/sqrt(fValue)
     */
    public static float InvSqrt (float fValue)
    {
        return (float)(1.0/Math.sqrt((double)fValue));
    }

    // Generate a random number in [0,1).  The random number generator may
    // be seeded by a first call to UnitRandom with a positive seed.
    public static float UnitRandom ( /*unsigned int uiSeed*/ )
    {
//         if (uiSeed > 0)
//         {
//             srand(uiSeed);
//         }

        Random kRand = new Random();
        double dRatio = ((double)kRand.nextDouble());
        return (float)dRatio;
    }


    /** Generate a random number in [-1,1).  The random number generator may
     * be seeded by a first call to SymmetricRandom with a positive seed.
     * @return a random number in [-1,1)
     */
    public static float SymmetricRandom (/*unsigned int uiSeed = 0*/)
    {
        /*
        if (uiSeed > 0.0)
        {
            srand(uiSeed);
        }
        */
        Random kRand = new Random();
        double dRatio = ((double)kRand.nextDouble());
        return (float)(2.0*dRatio - 1.0);
    }

    // common constants
    public static final float ZERO_TOLERANCE = 1e-06f;
    public static final float TWO_PI = (float)(2.0f*Math.PI);
    public static final float HALF_PI = (float)(0.5f*Math.PI);
    public static final float INV_PI = (float)(1.0f/Math.PI);
    public static final float DEG_TO_RAD = (float)Math.PI/180.0f;
    public static final float SQRT2 = (float)Math.sqrt(2);
}
