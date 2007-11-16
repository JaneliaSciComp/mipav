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

    /** Delete memory */
    public void dispose()
    {
        Name = null;
        for ( int i = 0; i < Quantity; i++ )
        {
            if ( Chars[i] != null )
            {
                Chars[i].dispose();
                Chars[i] = null;
            }
        }
        Chars = null;
    }

    /** Font name: */
    public String Name;
    /** Number of characters */
    public int Quantity;
    /** Font characters */
    public BitmapFontChar[] Chars;
}
