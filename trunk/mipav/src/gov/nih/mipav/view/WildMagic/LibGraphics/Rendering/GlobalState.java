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

public abstract class GlobalState extends GraphicsObject
    implements StreamInterface
{
    // abstract base class

    // supported global states
    public enum StateType
    {
        ALPHA (0),
        CULL (1),
        MATERIAL (2),
        POLYGONOFFSET (3),
        STENCIL (4),
        WIREFRAME (5),
        ZBUFFER (6),
        MAX_STATE_TYPE (7);
        private StateType(int iValue) { m_iValue = iValue; }
        public int Value() { return m_iValue; }
        private int m_iValue;
    };

    public abstract StateType GetStateType ();

    // default states
    public static GlobalState[] Default = new GlobalState[StateType.MAX_STATE_TYPE.Value()];
    public static boolean[] DefaultInitialized =
        new boolean[]{ false, false, false, false,
                       false, false, false };

    public GlobalState () {}

    public void Load (Stream rkStream, Stream.Link pkLink)
    {
        super.Load(rkStream,pkLink);
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
    }

    public int GetDiskUsed (StreamVersion rkVersion)
    {
        return super.GetDiskUsed(rkVersion);
    }

    public StringTree SaveStrings (final String acTitle)
    {
        StringTree pkTree = new StringTree();
        pkTree.Append(StringTree.Format("GlobalState",GetName()));
        pkTree.Append(super.SaveStrings(null));
        return pkTree;
    }

}
