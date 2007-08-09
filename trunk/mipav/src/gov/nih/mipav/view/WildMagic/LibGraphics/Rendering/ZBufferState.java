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

import java.util.HashMap;

import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;

public class ZBufferState extends GlobalState
    implements StreamInterface
{
    private static HashMap<Integer,CompareMode> ms_pkCompareModeMap = new HashMap<Integer,CompareMode>();

    public final StateType GetStateType () { return StateType.ZBUFFER; }

    public ZBufferState ()
    {
        if ( !DefaultInitialized[StateType.ZBUFFER.Value()] )
        {
            DefaultInitialized[StateType.ZBUFFER.Value()] = true;
            Default[StateType.ZBUFFER.Value()] = new ZBufferState();
        }
    }

    public enum CompareMode
    {
        CF_NEVER("CF_NEVER"),
        CF_LESS("CF_LESS"),
        CF_EQUAL("CF_EQUAL"),
        CF_LEQUAL("CF_LEQUAL"),
        CF_GREATER("CF_GREATER"),
        CF_NOTEQUAL("CF_NOTEQUAL"),
        CF_GEQUAL("CF_GEQUAL"),
        CF_ALWAYS("CF_ALWAYS"),
        CF_QUANTITY("CF_QUANTITY");

        CompareMode( String kName )
        {
            m_iValue = Init();
            m_kName = kName;
            ms_pkCompareModeMap.put( m_iValue, this );
        }
        private int Init ()
        {
            return m_iInitValue++;
        }
        public int Value() { return m_iValue; }
        public String Name() { return m_kName; }
        private int m_iValue;
        private String m_kName;
        private static int m_iInitValue = 0;
    };

    private static CompareMode ms_eCompareModeStatic = CompareMode.CF_QUANTITY;

    public boolean Enabled = true;         // default: true
    public boolean Writable = true;        // default: true
    public CompareMode Compare = CompareMode.CF_LEQUAL;  // default: CF_LEQUAL

    public void Load (Stream rkStream, Stream.Link pkLink)
    {
        super.Load(rkStream,pkLink);

        // native data
        Enabled = rkStream.ReadBoolean();
        Writable = rkStream.ReadBoolean();
        int iCompare = rkStream.ReadInt();
        Compare = ms_pkCompareModeMap.get(iCompare);
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
        rkStream.Write(Writable);
        rkStream.Write(Compare.Value());
    }

    public int GetDiskUsed (StreamVersion rkVersion)
    {
        return super.GetDiskUsed(rkVersion) +
            Stream.SIZEOF_BOOLEAN + //sizeof(char) + // Enabled
            Stream.SIZEOF_BOOLEAN + //sizeof(char) + // Writable
            Stream.SIZEOF_INT; //sizeof(int);   // Compare
    }

    public StringTree SaveStrings (final String acTitle)
    {
        StringTree pkTree = new StringTree();
        // strings
        pkTree.Append(StringTree.Format("ZBufferState",GetName()));
        pkTree.Append(super.SaveStrings(null));
        pkTree.Append(StringTree.Format("enabled =",Enabled));
        pkTree.Append(StringTree.Format("writable = ",Writable));
        pkTree.Append(StringTree.Format("compare = ",Compare.Name()));
        return pkTree;
    }
}
