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
