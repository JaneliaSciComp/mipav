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

import java.util.HashMap;

import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;

public class StencilState extends GlobalState
    implements StreamInterface
{

    private static HashMap<Integer,CompareFunction> ms_pkCompareFunctionMap = new HashMap<Integer,CompareFunction>();
    private static HashMap<Integer,OperationType> ms_pkOperationTypeMap = new HashMap<Integer,OperationType>();

    public StateType GetStateType () { return StateType.STENCIL; }

    public StencilState ()
    {
        if ( !DefaultInitialized[StateType.STENCIL.Value()] )
        {
            DefaultInitialized[StateType.STENCIL.Value()] = true;
            Default[StateType.STENCIL.Value()] = new StencilState();
        }
    }

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

    private static CompareFunction ms_eCompareFunctionStatic = CompareFunction.CF_QUANTITY;
    private static OperationType ms_eOperationTypeStatic = OperationType.OT_QUANTITY;

    public boolean Enabled = false;                             // default: false
    public CompareFunction Compare = CompareFunction.CF_NEVER;  // default: CF_NEVER
    public int Reference = 0;                                   // default: 0
    public int Mask = ~0;        // default: ~0
    public int WriteMask = ~0;   // default: ~0
    public OperationType OnFail = OperationType.OT_KEEP;     // default: OT_KEEP
    public OperationType OnZFail = OperationType.OT_KEEP;    // default: OT_KEEP
    public OperationType OnZPass = OperationType.OT_KEEP;    // default: OT_KEEP

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
        rkStream.Write(Compare.Value());
        rkStream.Write(Reference);
        rkStream.Write(Mask);
        rkStream.Write(WriteMask);
        rkStream.Write(OnFail.Value());
        rkStream.Write(OnZFail.Value());
        rkStream.Write(OnZPass.Value());
    }

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
