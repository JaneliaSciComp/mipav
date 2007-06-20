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

    /** BitmapFontChar position and size: */
    public int XOrigin, YOrigin, XSize, YSize;
    /** Bitmap representing a character: */
    public final byte[] Bitmap;
}
