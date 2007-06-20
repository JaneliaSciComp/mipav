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

package gov.nih.mipav.view.WildMagic.LibGraphics.Rendering;

public abstract class ResourceIdentifier
{
    // Abstract base class.  The destructor is *NOT* virtual so that the
    // derived-class destructors hide it.  This is intentional to avoid a
    // virtual function table pointer, a safe thing here because the base
    // class has no data.  This allows the derived classes that represent
    // vertex buffer information to store the input attributes first, and
    // allow typecasting of the following form.
    //
    //   class VBufferIdentifier : public ResourceIdentifier
    //   {
    //   public:  Attributes IAttr;
    //   }
    //   VBufferIdentifier* pkID = <some identifier>;
    //   Attributes& rkIAttr = *(Attributes*)pkID;

    public void finalize () {} //~ResourceIdentifier () {/**/}

    protected ResourceIdentifier () {/**/}
};
