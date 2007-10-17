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
