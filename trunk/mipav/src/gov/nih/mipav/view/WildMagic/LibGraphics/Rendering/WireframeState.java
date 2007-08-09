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

import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;

public class WireframeState extends GlobalState
    implements StreamInterface
{

    public final StateType GetStateType () { return StateType.WIREFRAME; }

    public WireframeState ()
    {
        if ( !DefaultInitialized[StateType.WIREFRAME.Value()] )
        {
            DefaultInitialized[StateType.WIREFRAME.Value()] = true;
            Default[StateType.WIREFRAME.Value()] = new WireframeState();
        }
    }

    public boolean Enabled = false;  // default: false

    public void Load (Stream rkStream, Stream.Link pkLink)
    {
        super.Load(rkStream,pkLink);

        // native data
        Enabled = rkStream.ReadBoolean();
    }

    public void Link (Stream rkStream, Stream.Link pkLink)
    {
        super.Link(rkStream,pkLink);
    }

    public boolean Register (Stream rkStream) 
    {
        return super.Register(rkStream);
    }

    public void Save (Stream rkStream) 
    {
        super.Save(rkStream);

        // native data
        rkStream.Write(Enabled);
    }

    public int GetDiskUsed (StreamVersion rkVersion) 
    {
        return super.GetDiskUsed(rkVersion) +
            Stream.SIZEOF_BOOLEAN; //sizeof(char);  // Enabled
    }

    public StringTree SaveStrings (final String acTitle)
    {
        StringTree pkTree = new StringTree();
        // strings
        pkTree.Append(StringTree.Format("WireframeState",GetName()));
        pkTree.Append(super.SaveStrings(null));
        pkTree.Append(StringTree.Format("enabled =",Enabled));
        return pkTree;
    }
    //----------------------------------------------------------------------------


}
