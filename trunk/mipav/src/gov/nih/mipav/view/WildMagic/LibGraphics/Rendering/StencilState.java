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

public class StencilState extends GlobalState
    implements StreamInterface
{
    /** Maps compare function enum to int values. */
    private static HashMap<Integer,CompareFunction> ms_pkCompareFunctionMap = new HashMap<Integer,CompareFunction>();
    /** Maps operation type enum to int values. */
    private static HashMap<Integer,OperationType> ms_pkOperationTypeMap = new HashMap<Integer,OperationType>();

    /** Static initialization of the StencilState in the GlobalState.Default
     * array. */
    static
    {
        if ( !DefaultInitialized[StateType.STENCIL.Value()] )
        {
            DefaultInitialized[StateType.STENCIL.Value()] = true;
            Default[StateType.STENCIL.Value()] = new StencilState();
        }
    }

    /** Default constructor. */
    public StencilState () { }

    /** Return type.
     * @return StateType.STENCIL;
     */
    public final StateType GetStateType () { return StateType.STENCIL; }

    /** Stencil compare function. */
    public enum CompareFunction
    {
        CF_NEVER ("CF_NEVER"),
        CF_LESS ("CF_LESS"),
        CF_EQUAL ("CF_EQUAL"),
        CF_LEQUAL ("CF_LEQUAL"),
        CF_GREATER ("CF_GREATER"),
        CF_NOTEQUAL ("CF_NOTEQUAL"),
        CF_GEQUAL ("CF_GEQUAL"),
        CF_ALWAYS ("CF_ALWAYS"),
        CF_QUANTITY ("CF_QUANTITY");

        CompareFunction( String kName )
        {
            m_kName = kName;
            m_iValue = Init();
            ms_pkCompareFunctionMap.put( m_iValue, this );
        }
        private int Init ()
        {
            return m_iInitValue++;
        }
        public int Value() { return m_iValue; }
        public String Name() { return m_kName; }
        private String m_kName;
        private int m_iValue;
        private static int m_iInitValue = 0;
    };

    /** Stencil operation type. */
    public enum OperationType
    {
        OT_KEEP ("OT_KEEP"),
        OT_ZERO ("OT_ZERO"),
        OT_REPLACE ("OT_REPLACE"),
        OT_INCREMENT ("OT_INCREMENT"),
        OT_DECREMENT ("OT_DECREMENT"),
        OT_INVERT ("OT_INVERT"),
        OT_QUANTITY ("OT_QUANTITY");

        OperationType( String kName )
        {
            m_kName = kName;
            m_iValue = Init();
            ms_pkOperationTypeMap.put( m_iValue, this );
        }
        private int Init ()
        {
            return m_iInitValue++;
        }
        public int Value() { return m_iValue; }
        public String Name() { return m_kName; }
        private String m_kName;
        private int m_iValue;
        private static int m_iInitValue = 0;
    };

    /** Initializes static enum for compare function. */
    private static CompareFunction ms_eCompareFunctionStatic = CompareFunction.CF_QUANTITY;
    /** Initializes static enum for operation type. */
    private static OperationType ms_eOperationTypeStatic = OperationType.OT_QUANTITY;

    /** Enabled  default: false */
    public boolean Enabled = false;
    /** Compare function  default: CF_NEVER */
    public CompareFunction Compare = CompareFunction.CF_NEVER;
    /** Renference  default: 0 */
    public int Reference = 0;
    /** Mask  default: ~0 */
    public int Mask = ~0;
    /** Write Mask  default: ~0 */
    public int WriteMask = ~0;
    /** OnFail  default: OT_KEEP */
    public OperationType OnFail = OperationType.OT_KEEP;
    /** OnZFail  default: OT_KEEP */
    public OperationType OnZFail = OperationType.OT_KEEP;
    /** OnZPass default: OT_KEEP */
    public OperationType OnZPass = OperationType.OT_KEEP;

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
        int iCompare = rkStream.ReadInt();
        Compare = ms_pkCompareFunctionMap.get(iCompare);
        Reference = rkStream.ReadInt();
        Mask = rkStream.ReadInt();
        WriteMask = rkStream.ReadInt();
        int iOnFail = rkStream.ReadInt();
        OnFail = ms_pkOperationTypeMap.get(iOnFail);
        int iOnZFail = rkStream.ReadInt();
        OnZFail = ms_pkOperationTypeMap.get(iOnZFail);
        int iOnZPass = rkStream.ReadInt();
        OnZPass = ms_pkOperationTypeMap.get(iOnZPass);
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
        rkStream.Write(Compare.Value());
        rkStream.Write(Reference);
        rkStream.Write(Mask);
        rkStream.Write(WriteMask);
        rkStream.Write(OnFail.Value());
        rkStream.Write(OnZFail.Value());
        rkStream.Write(OnZPass.Value());
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
            Stream.SIZEOF_INT + //sizeof(int) +  // Compare
            Stream.SIZEOF_INT + //sizeof(Reference) +
            Stream.SIZEOF_INT + //sizeof(Mask) +
            Stream.SIZEOF_INT + //sizeof(WriteMask) +
            Stream.SIZEOF_INT + //sizeof(int) +  // OnFail
            Stream.SIZEOF_INT + //sizeof(int) +  // OnZFail
            Stream.SIZEOF_INT; //sizeof(int);   // OnZPass
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
        pkTree.Append(StringTree.Format("StencilState",GetName()));
        pkTree.Append(super.SaveStrings(null));
        pkTree.Append(StringTree.Format("enabled =",Enabled));
        pkTree.Append(StringTree.Format("compare =",Compare.Name()));
        pkTree.Append(StringTree.Format("reference =",Reference));
        pkTree.Append(StringTree.Format("mask =",Mask));
        pkTree.Append(StringTree.Format("write mask =",WriteMask));
        pkTree.Append(StringTree.Format("on fail =",OnFail.Name()));
        pkTree.Append(StringTree.Format("on z-fail =",OnZFail.Name()));
        pkTree.Append(StringTree.Format("on z-pass =",OnZPass.Name()));
        return pkTree;
    }
}
