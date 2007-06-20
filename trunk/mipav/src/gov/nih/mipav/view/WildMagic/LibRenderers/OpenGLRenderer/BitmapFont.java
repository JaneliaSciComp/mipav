// Geometric Tools, Inc.
// http://www.geometrictools.com
// Copyright (c) 1998-2006.  All Rights Reserved
//
// The Wild Magic Version 4 Restricted Libraries source code is supplied
// under the terms of the license agreement
//     http://www.geometrictools.com/License/Wm4RestrictedLicense.pdf
// and may not be copied or disclosed except in accordance with the terms
// of that agreement.
//
// Version: 4.0.0 (2006/06/28)

package gov.nih.mipav.view.WildMagic.LibRenderers.OpenGLRenderer;

public class BitmapFont
{
    /**
     * Create a new BitmapFont
     * @param acName, the font name
     * @param iQuantity, the number of font characters
     * @param pkChars, the BitmapFontChar characters
     */
    public BitmapFont (final String acName, int iQuantity,
                       final BitmapFontChar[] pkChars)
    {
        Name = acName;
        Quantity = iQuantity;
        Chars = pkChars;
    }

    /** Font name: */
    public String Name;
    /** Number of characters */
    public int Quantity;
    /** Font characters */
    public BitmapFontChar[] Chars;
}
