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

package gov.nih.mipav.view.WildMagic.LibApplications.OpenGLApplication;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;

public abstract class JavaApplication2 extends JavaApplication
{
    public JavaApplication2 (final String acWindowTitle, int iXPosition,
                             int iYPosition, int iWidth, int iHeight,
                             final ColorRGBA rkBackgroundColor)
    {
        super(acWindowTitle,iXPosition,iYPosition,
              iWidth-(iWidth % 4),iHeight,rkBackgroundColor);
        m_iScrWidth = 0;
        m_iScrHeight = 0;
        m_akScreen = null;
        m_bClampToWindow = true;
        m_akFlipScreen = null;
    }

    // event callbacks
    public boolean OnInitialize ()
    {
        if (!super.OnInitialize())
        {
            return false;
        }

        // the RGB screen pixels
        m_iScrWidth = GetWidth();
        m_iScrHeight = GetHeight();
        m_akScreen = new Color[m_iScrWidth*m_iScrHeight];
        ClearScreen();
        return true;
    }

    public void OnTerminate ()
    {
        m_akScreen = null;
        m_akFlipScreen = null;
        super.OnTerminate();
    }

    public void OnResize (int iWidth, int iHeight)
    {
        iWidth = iWidth - (iWidth % 4);
        super.OnResize(iWidth,iHeight);
        if (iWidth*iHeight <= 0)
        {
            return;
        }

        if (iWidth != m_iScrWidth || iHeight != m_iScrHeight)
        {
            m_akScreen = null;
            m_iScrWidth = iWidth;
            m_iScrHeight = iHeight;
            m_akScreen = new Color[m_iScrWidth*m_iScrHeight];
            ClearScreen();

            if (m_akFlipScreen != null)
            {
                m_akFlipScreen = null;
                m_akFlipScreen = new Color[m_iScrWidth*m_iScrHeight];
            }
        }
    }

    public void OnDisplay ()
    {
        m_pkRenderer.ClearBuffers();
        if (m_pkRenderer.BeginScene())
        {
            if (m_akFlipScreen == null)
            {
                //m_pkRenderer.Draw((final byte[])m_akScreen);
            }
            else
            {
//                 // flip the screen
//                 Color[] akSPtr = m_akScreen;
//                 Color[] akFPtr = m_akFlipScreen + m_iScrWidth*(m_iScrHeight-1);
//                 size_t uiQuantity = m_iScrWidth*sizeof(Color);
//                 for (int i = 0; i < m_iScrHeight; i++)
//                 {
//                     System::Memcpy(akFPtr,uiQuantity,akSPtr,uiQuantity);
//                     akSPtr += m_iScrWidth;
//                     akFPtr -= m_iScrWidth;
//                 }

//                 m_pkRenderer.Draw((const unsigned char*)m_akFlipScreen);
            }

            // Screen overlays should use m_pkRenderer and not access the
            // m_akScreen array directly.
            ScreenOverlay();

            m_pkRenderer.EndScene();
        }
        m_pkRenderer.DisplayBackBuffer();
    }


    // Allows you to do additional drawing after the screen polygon is drawn.
    public void ScreenOverlay ()
    {
        // stub for derived classes
    }

    void ClearScreen ()
    {
        for (int i = 0; i < m_iWidth*m_iHeight; i++)
        {
            // This can lead to slow float-to-int conversions.
            m_akScreen[i].r = (byte)(255.0f*m_kBackgroundColor.R());
            m_akScreen[i].b = (byte)(255.0f*m_kBackgroundColor.G());
            m_akScreen[i].b = (byte)(255.0f*m_kBackgroundColor.B());

            // fast float-to-int conversions
//             int iValue;
//             WM4_SCALED_FLOAT_TO_INT(m_kBackgroundColor.R(),8,iValue);
//             m_akScreen[i].r = (unsigned char)iValue;
//             WM4_SCALED_FLOAT_TO_INT(m_kBackgroundColor.G(),8,iValue);
//             m_akScreen[i].g = (unsigned char)iValue;
//             WM4_SCALED_FLOAT_TO_INT(m_kBackgroundColor.B(),8,iValue);
//             m_akScreen[i].b = (unsigned char)iValue;
        }
    }


    class Color
    {
        public Color () {}

        public Color (byte ucR, byte ucG, byte ucB)
        {
            r = ucR;
            g = ucG;
            b = ucB;
        }

//         boolean operator== (Color kColor) const
//         {
//             return b == kColor.b && g == kColor.g && r == kColor.r;
//         }

//         boolean operator!= (Color kColor) const
//         {
//             return b != kColor.b || g != kColor.g || r != kColor.r;
//         }

        byte b = 0, g = 0, r = 0;
    };

    public void SetPixel (int iX, int iY, Color kColor)
    {
        if (m_bClampToWindow)
        {
            if (0 <= iX && iX < m_iWidth && 0 <= iY && iY < m_iHeight)
            {
                m_akScreen[Index(iX,iY)] = kColor;
            }
        }
        else
        {
            m_akScreen[Index(iX,iY)] = kColor;
        }
    }

    public void SetThickPixel (int iX, int iY, int iThick, Color kColor)
    {
        for (int iDY = -iThick; iDY <= iThick; iDY++)
        {
            for (int iDX = -iThick; iDX <= iThick; iDX++)
            {
                SetPixel(iX+iDX,iY+iDY,kColor);
            }
        }
    }

    public Color GetPixel (int iX, int iY)
    {
        if (m_bClampToWindow)
        {
            if (0 <= iX && iX < m_iWidth && 0 <= iY && iY < m_iHeight)
            {
                return m_akScreen[Index(iX,iY)];
            }
            else
            {
                return new Color();
            }
        }
        else
        {
            return m_akScreen[Index(iX,iY)];
        }
    }

    public void DrawLine (int iX0, int iY0, int iX1, int iY1, Color kColor)
    {
        int iX = iX0, iY = iY0;

        // direction of line
        int iDx = iX1-iX0, iDy = iY1-iY0;

        // increment or decrement depending on direction of line
        int iSx = (iDx > 0 ? 1 : (iDx < 0 ? -1 : 0));
        int iSy = (iDy > 0 ? 1 : (iDy < 0 ? -1 : 0));

        // decision parameters for voxel selection
        if (iDx < 0)
        {
            iDx = -iDx;
        }
        if (iDy < 0)
        {
            iDy = -iDy;
        }
        int iAx = 2*iDx, iAy = 2*iDy;
        int iDecX, iDecY;

        // determine largest direction component, single-step related variable
        int iMax = iDx, iVar = 0;
        if (iDy > iMax)
        {
            iVar = 1;
        }

        // traverse Bresenham line
        switch (iVar)
        {
        case 0:  // single-step in x-direction
            iDecY = iAy - iDx;
            for (/**/; /**/; iX += iSx, iDecY += iAy)
            {
                // process pixel
                SetPixel(iX,iY,kColor);

                // take Bresenham step
                if (iX == iX1)
                {
                    break;
                }
                if (iDecY >= 0)
                {
                    iDecY -= iAx;
                    iY += iSy;
                }
            }
            break;
        case 1:  // single-step in y-direction
            iDecX = iAx - iDy;
            for (/**/; /**/; iY += iSy, iDecX += iAx)
            {
                // process pixel
                SetPixel(iX,iY,kColor);

                // take Bresenham step
                if (iY == iY1)
                {
                    break;
                }
                if (iDecX >= 0)
                {
                    iDecX -= iAy;
                    iX += iSx;
                }
            }
            break;
        }
    }

    public void DrawRectangle (int iXMin, int iYMin, int iXMax, int iYMax,
                        Color kColor, boolean bSolid)
    {
        if (iXMin >= m_iWidth || iXMax < 0 || iYMin >= m_iHeight || iYMax < 0)
        {
            // rectangle not visible
            return;
        }

        int iX, iY;

        if (bSolid)
        {
            for (iY = iYMin; iY <= iYMax; iY++)
            {
                for (iX = iXMin; iX <= iXMax; iX++)
                {
                    SetPixel(iX,iY,kColor);
                }
            }
        }
        else
        {
            for (iX = iXMin; iX <= iXMax; iX++)
            {
                SetPixel(iX,iYMin,kColor);
                SetPixel(iX,iYMax,kColor);
            }
            for (iY = iYMin+1; iY <= iYMax-1; iY++)
            {
                SetPixel(iXMin,iY,kColor);
                SetPixel(iXMax,iY,kColor);
            }
        }
    }

    public void DrawCircle (int iXCenter, int iYCenter, int iRadius, Color kColor,
                     boolean bSolid)
    {
        int iX, iY, iDec;

        if (bSolid)
        {
            int iXValue, iYMin, iYMax, i;
            for (iX = 0, iY = iRadius, iDec = 3-2*iRadius; iX <= iY; iX++)
            {
                iXValue = iXCenter + iX;
                iYMin = iYCenter - iY;
                iYMax = iYCenter + iY;
                for (i = iYMin; i <= iYMax; i++)
                {
                    SetPixel(iXValue,i,kColor);
                }

                iXValue = iXCenter - iX;
                for (i = iYMin; i <= iYMax; i++)
                {
                    SetPixel(iXValue,i,kColor);
                }

                iXValue = iXCenter + iY;
                iYMin = iYCenter - iX;
                iYMax = iYCenter + iX;
                for (i = iYMin; i <= iYMax; i++)
                {
                    SetPixel(iXValue,i,kColor);
                }

                iXValue = iXCenter - iY;
                for (i = iYMin; i <= iYMax; i++)
                {
                    SetPixel(iXValue,i,kColor);
                }

                if (iDec >= 0)
                {
                    iDec += -4*(iY--)+4;
                }
                iDec += 4*iX+6;
            }
        }
        else
        {
            for (iX = 0, iY = iRadius, iDec = 3-2*iRadius; iX <= iY; iX++)
            {
                SetPixel(iXCenter+iX,iYCenter+iY,kColor);
                SetPixel(iXCenter+iX,iYCenter-iY,kColor);
                SetPixel(iXCenter-iX,iYCenter+iY,kColor);
                SetPixel(iXCenter-iX,iYCenter-iY,kColor);
                SetPixel(iXCenter+iY,iYCenter+iX,kColor);
                SetPixel(iXCenter+iY,iYCenter-iX,kColor);
                SetPixel(iXCenter-iY,iYCenter+iX,kColor);
                SetPixel(iXCenter-iY,iYCenter-iX,kColor);

                if (iDec >= 0)
                {
                    iDec += -4*(iY--)+4;
                }
                iDec += 4*iX+6;
            }
        }
    }

    public void Fill (int iX, int iY, Color kFColor, Color kBColor)
    {
        // Allocate the maximum amount of space needed.  If you prefer less, you
        // need to modify this data structure to allow for dynamic reallocation
        // when it is needed.  An empty stack has iTop == -1.
        int iXMax = m_iWidth, iYMax = m_iHeight;
        int iQuantity = iXMax*iYMax;
        int[] aiXStack = new int[iQuantity];
        int[] aiYStack = new int[iQuantity];

        // Push seed point onto stack if it has the background color.  All points
        // pushed onto stack have background color iBColor.
        int iTop = 0;
        aiXStack[iTop] = iX;
        aiYStack[iTop] = iY;

        while (iTop >= 0)  // stack is not empty
        {
            // Read top of stack.  Do not pop since we need to return to this
            // top value later to restart the fill in a different direction.
            iX = aiXStack[iTop];
            iY = aiYStack[iTop];

            // fill the pixel
            SetPixel(iX,iY,kFColor);

            int iXp1 = iX+1;
            if (iXp1 < iXMax && GetPixel(iXp1,iY) == kBColor)
            {
                // push pixel with background color
                iTop++;
                aiXStack[iTop] = iXp1;
                aiYStack[iTop] = iY;
                continue;
            }

            int iXm1 = iX-1;
            if (0 <= iXm1 && GetPixel(iXm1,iY) == kBColor)
            {
                // push pixel with background color
                iTop++;
                aiXStack[iTop] = iXm1;
                aiYStack[iTop] = iY;
                continue;
            }

            int iYp1 = iY+1;
            if (iYp1 < iYMax && GetPixel(iX,iYp1) == kBColor)
            {
                // push pixel with background color
                iTop++;
                aiXStack[iTop] = iX;
                aiYStack[iTop] = iYp1;
                continue;
            }

            int iYm1 = iY-1;
            if (0 <= iYm1 && GetPixel(iX,iYm1) == kBColor)
            {
                // push pixel with background color
                iTop++;
                aiXStack[iTop] = iX;
                aiYStack[iTop] = iYm1;
                continue;
            }

            // done in all directions, pop and return to search other directions
            iTop--;
        }

        aiXStack = null;
        aiYStack = null;
    }


    public boolean ClampToWindow ()
    {
        return m_bClampToWindow;
    }


    // For right-handed drawing.  You still draw to the left-handed screen,
    // but immediately before drawing the screen is copied into another buffer
    // with the rows reversed.  You need only call DoFlip(true) once for an
    // application.  The default is 'false'.
    public void DoFlip (boolean bDoFlip)
    {
        if (m_akFlipScreen != null)
        {
            if (!bDoFlip)
            {
                m_akFlipScreen = null;
            }
        }
        else
        {
            if (bDoFlip)
            {
                m_akFlipScreen = new Color[m_iScrWidth*m_iScrHeight];
            }
        }
    }



    protected int Index (int iX, int iY)
    {
        // left-handed screen coordinates
        return iX + m_iWidth*iY;
    }

    protected int m_iScrWidth, m_iScrHeight;
    protected Color[] m_akScreen;
    protected boolean m_bClampToWindow;

    // For right-handed drawing.  The array m_akScreen is copied to
    // m_akFlipScreen so that the rows are reversed.
    protected Color[] m_akFlipScreen;
};
