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

import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;

public class WireframeState extends GlobalState
    implements StreamInterface
{

    public StateType GetStateType () { return StateType.WIREFRAME; }

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
