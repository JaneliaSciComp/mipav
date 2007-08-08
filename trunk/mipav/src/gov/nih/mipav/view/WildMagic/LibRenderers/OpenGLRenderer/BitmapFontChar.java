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

public class BitmapFontChar
{
    /**
     * Creates a new BitmapFontChar
     * @param iXOrigin, the x-origin of the character
     * @param iYOrigin, the y-origin of the character
     * @param iXSize, the x-size of the character
     * @param iYSize, the y-size of the character
     * @param aucBitmap, the bitmap representing a character.
     */
    public BitmapFontChar (int iXOrigin, int iYOrigin, int iXSize, int iYSize,
                           final byte[] aucBitmap)
    {
        XOrigin = iXOrigin;
        YOrigin = iYOrigin;
        XSize = iXSize;
        YSize = iYSize;
        Bitmap = aucBitmap;
    }

    /** Delete memory */
    public void finalize()
    {
        Bitmap = null;
    }

    /** BitmapFontChar position and size: */
    public int XOrigin, YOrigin, XSize, YSize;
    /** Bitmap representing a character: */
    public byte[] Bitmap;
}
