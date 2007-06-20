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
// Version: 4.0.3 (2006/11/25)

package gov.nih.mipav.view.WildMagic.LibGraphics.Rendering;

public class ReleaseFunctionTexture extends ReleaseFunction
{
    public ReleaseFunctionTexture ( Renderer kRenderer )
    {
        super(kRenderer);
    }
    public void Release ( Bindable kBindable )
    {
        m_kRenderer.ReleaseTexture( kBindable );
    }
}
