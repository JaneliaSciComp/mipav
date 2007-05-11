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

// Bit hacks are available at
//     http://graphics.stanford.edu/~seander/bithacks.html

package gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics;
public class BitHacks
{
    /** Deterimes if the input is an exact power of two.
     * @param uiValue
     * @return true if the input is an exact power of two, false otherwise.
     */
    public static boolean IsPowerOfTwo (int uiValue)
    {                                                               
        return (uiValue > 0) && ((uiValue & (uiValue - 1)) == 0);   
    }

    /** Returns the Log2 of an integer that is a power of two.
     * @param uiPowerOfTwo, int must be an exact power of two
     * @return the Log2 value of the input parameter.
     */
    public static int Log2OfPowerOfTwo (int uiPowerOfTwo)
    {
        int uiLog2 = ((uiPowerOfTwo & 0xAAAAAAAA) != 0) ? 1 : 0;
        uiLog2 |= ((((uiPowerOfTwo & 0xFFFF0000) != 0)) ? 1 : 0) << 4;
        uiLog2 |= ((((uiPowerOfTwo & 0xFF00FF00) != 0)) ? 1 : 0) << 3;
        uiLog2 |= ((((uiPowerOfTwo & 0xF0F0F0F0) != 0)) ? 1 : 0) << 2;
        uiLog2 |= ((((uiPowerOfTwo & 0xCCCCCCCC) != 0)) ? 1 : 0) << 1;
        return uiLog2;
    }

}
