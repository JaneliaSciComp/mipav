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

public abstract class GlobalState extends GraphicsObject
    implements StreamInterface
{
    /** supported global states */
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

    /** Return the type of state.
     * @return the type of state. */
    public abstract StateType GetStateType ();

    /** default states */
    public static GlobalState[] Default = new GlobalState[StateType.MAX_STATE_TYPE.Value()];
    /** default states initialized */
    public static boolean[] DefaultInitialized =
        new boolean[]{ false, false, false, false,
                       false, false, false };

    /** Default constructor. */
    public GlobalState () {}

    /**
     * Write this object into a StringTree for the scene-graph visualization.
     * @param acTitle, the header for this object in the StringTree.
     * @return StringTree containing a String-based representation of this
     * object and it's children.
     */
    public StringTree SaveStrings (final String acTitle)
    {
        StringTree pkTree = new StringTree();
        pkTree.Append(StringTree.Format("GlobalState",GetName()));
        pkTree.Append(super.SaveStrings(null));
        return pkTree;
    }

}
