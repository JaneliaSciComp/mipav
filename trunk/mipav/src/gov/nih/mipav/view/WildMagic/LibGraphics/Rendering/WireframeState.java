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
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.ZBufferState.CompareMode;

public class WireframeState extends GlobalState
    implements StreamInterface
{

    /** Maps compare mode enum to int values. */
    private static HashMap<Integer,FillMode> ms_pkFillModeMap = new HashMap<Integer,FillMode>();

    /** Static initialization of the WireframeState in the GlobalState.Default
     * array. */
    static
    {
        if ( !DefaultInitialized[StateType.WIREFRAME.Value()] )
        {
            DefaultInitialized[StateType.WIREFRAME.Value()] = true;
            Default[StateType.WIREFRAME.Value()] = new WireframeState();
        }
    }
    
    /** Depth compre modes. */
    public enum FillMode
    {
        FM_FILL("FM_FILL"),
        FM_LINE("FM_LINE"),
        FM_POINT("FM_POINT"),
        FM_QUANTITY("FM_QUANTITY");

        FillMode( String kName )
        {
            m_iValue = Init();
            m_kName = kName;
            ms_pkFillModeMap.put( m_iValue, this );
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
    private static FillMode ms_eFillModeStatic = FillMode.FM_QUANTITY;
    
    /** Default constructor. */
    public WireframeState () {}

    /** Return type.
     * @return StateType.WIREFRAME;
     */
    public final StateType GetStateType () { return StateType.WIREFRAME; }

    /** Wireframe enabled default: false */
    public boolean Enabled = false;  
    /** FillMode default: FM_FILL */
    public FillMode Fill = FillMode.FM_FILL;

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
            Stream.SIZEOF_BOOLEAN; //sizeof(char);  // Enabled
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
        pkTree.Append(StringTree.Format("WireframeState",GetName()));
        pkTree.Append(super.SaveStrings(null));
        pkTree.Append(StringTree.Format("enabled =",Enabled));
        return pkTree;
    }
}
