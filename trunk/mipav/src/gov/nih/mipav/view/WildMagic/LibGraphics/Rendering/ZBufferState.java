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
    /** Maps compare mode enum to int values. */
    private static HashMap<Integer,CompareMode> ms_pkCompareModeMap = new HashMap<Integer,CompareMode>();

    /** Static initialization of the ZBufferState in the GlobalState.Default
     * array. */
    static
    {
        if ( !DefaultInitialized[StateType.ZBUFFER.Value()] )
        {
            DefaultInitialized[StateType.ZBUFFER.Value()] = true;
            Default[StateType.ZBUFFER.Value()] = new ZBufferState();
        }
    }

    /** Depth compre modes. */
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

    /** Initializes static enum for depth compare modes. */
    private static CompareMode ms_eCompareModeStatic = CompareMode.CF_QUANTITY;

    /** Default constructor */
    public ZBufferState () {}

    /** Return type.
     * @return StateType.ZBUFFER;
     */
    public final StateType GetStateType () { return StateType.ZBUFFER; }

    /** ZBuffer enabled default: true */
    public boolean Enabled = true;         
    /** ZBuffer writeable  default: true */
    public boolean Writable = true;
    /** CompareMode default: CF_LEQUAL */
    public CompareMode Compare = CompareMode.CF_LEQUAL;

    /**
     * Loads this object from the input parameter rkStream, using the input
     * Stream.Link to store the IDs of children objects of this object
     * for linking after all objects are loaded from the Stream.
     * @param rkStream, the Stream from which this object is being read.
     * @param pkLink, the Link class for storing the IDs of this object's
     * children objcts.
     */
    public void Load (Stream rkStream, Stream.Link pkLink)
    {
        super.Load(rkStream,pkLink);

        // native data
        Enabled = rkStream.ReadBoolean();
        Writable = rkStream.ReadBoolean();
        int iCompare = rkStream.ReadInt();
        Compare = ms_pkCompareModeMap.get(iCompare);
    }

    /**
     * Write this object and all it's children to the Stream.
     * @param rkStream, the Stream where the child objects are stored.
     */
    public void Save (Stream rkStream)
    {
        super.Save(rkStream);

        // native data
        rkStream.Write(Enabled);
        rkStream.Write(Writable);
        rkStream.Write(Compare.Value());
    }

    /**
     * Returns the size of this object and it's children on disk for the
     * current StreamVersion parameter.
     * @param rkVersion, the current version of the Stream file being created.
     * @return the size of this object on disk.
     */
    public int GetDiskUsed (StreamVersion rkVersion)
    {
        return super.GetDiskUsed(rkVersion) +
            Stream.SIZEOF_BOOLEAN + //sizeof(char) + // Enabled
            Stream.SIZEOF_BOOLEAN + //sizeof(char) + // Writable
            Stream.SIZEOF_INT; //sizeof(int);   // Compare
    }

    /**
     * Write this object into a StringTree for the scene-graph visualization.
     * @param acTitle, the header for this object in the StringTree.
     * @return StringTree containing a String-based representation of this
     * object and it's children.
     */
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
