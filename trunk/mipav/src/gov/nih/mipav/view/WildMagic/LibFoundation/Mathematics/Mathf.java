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
// Version: 4.0.1 (2006/08/22)

package gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics;

import java.util.Random;

public final class Mathf
{
     public static float InvSqrt (float fValue)
    {
        return (float)(1.0/Math.sqrt((double)fValue));
    }

    /** Generate a random number in [-1,1).  The random number generator may
     * be seeded by a first call to SymmetricRandom with a positive seed.
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
