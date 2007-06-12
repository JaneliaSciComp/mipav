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

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;

public class AlphaState extends GlobalState
    implements StreamInterface
{
    private static HashMap<Integer,SrcBlendMode> ms_pkSrcBlendModeMap = new HashMap<Integer,SrcBlendMode>();
    private static HashMap<Integer,DstBlendMode> ms_pkDstBlendModeMap = new HashMap<Integer,DstBlendMode>();
    private static HashMap<Integer,TestMode> ms_pkTestModeMap = new HashMap<Integer,TestMode>();

    static
    {
        if ( !DefaultInitialized[StateType.ALPHA.Value()] )
        {
            DefaultInitialized[StateType.ALPHA.Value()] = true;
            Default[StateType.ALPHA.Value()] = new AlphaState();
        }

    }
     
    public enum SrcBlendMode
    {
        SBF_ZERO ("SBF_ZERO"),
        SBF_ONE ("SBF_ONE"),
        SBF_DST_COLOR ("SBF_DST_COLOR"),
        SBF_ONE_MINUS_DST_COLOR ("SBF_ONE_MINUS_DST_COLOR"),
        SBF_SRC_ALPHA ("SBF_SRC_ALPHA"),
        SBF_ONE_MINUS_SRC_ALPHA ("SBF_ONE_MINUS_SRC_ALPHA"),
        SBF_DST_ALPHA ("SBF_DST_ALPHA"),
        SBF_ONE_MINUS_DST_ALPHA ("SBF_ONE_MINUS_DST_ALPHA"),
        SBF_SRC_ALPHA_SATURATE ("SBF_SRC_ALPHA_SATURATE"),
        SBF_CONSTANT_COLOR ("SBF_CONSTANT_COLOR"),
        SBF_ONE_MINUS_CONSTANT_COLOR ("SBF_ONE_MINUS_CONSTANT_COLOR"),
        SBF_CONSTANT_ALPHA ("SBF_CONSTANT_ALPHA"),
        SBF_ONE_MINUS_CONSTANT_ALPHA ("SBF_ONE_MINUS_CONSTANT_ALPHA"),
        SBF_QUANTITY ("SBF_QUANTITY");

        private int m_iValue;
        private String m_kName;
        private static int m_iInitValue = 0;
        SrcBlendMode( String kName )
        {
            m_kName = kName;
            m_iValue = Init();
            ms_pkSrcBlendModeMap.put( m_iValue, this );
        }
        private int Init ()
        {
            return m_iInitValue++;
        }
        public int Value() { return m_iValue; }
        public String Name() { return m_kName; }
    };

    public enum DstBlendMode
    {
        DBF_ZERO ("DBF_ZERO"),
        DBF_ONE ("DBF_ONE"),
        DBF_SRC_COLOR ("DBF_SRC_COLOR"),
        DBF_ONE_MINUS_SRC_COLOR ("DBF_ONE_MINUS_SRC_COLOR"),
        DBF_SRC_ALPHA ("DBF_SRC_ALPHA"),
        DBF_ONE_MINUS_SRC_ALPHA ("DBF_ONE_MINUS_SRC_ALPHA"),
        DBF_DST_ALPHA ("DBF_DST_ALPHA"),
        DBF_ONE_MINUS_DST_ALPHA ("DBF_ONE_MINUS_DST_ALPHA"),
        DBF_CONSTANT_COLOR ("DBF_CONSTANT_COLOR"),
        DBF_ONE_MINUS_CONSTANT_COLOR ("DBF_ONE_MINUS_CONSTANT_COLOR"),
        DBF_CONSTANT_ALPHA ("DBF_CONSTANT_ALPHA"),
        DBF_ONE_MINUS_CONSTANT_ALPHA ("DBF_ONE_MINUS_CONSTANT_ALPHA"),
        DBF_QUANTITY ("DBF_QUANTITY");

        private int m_iValue;
        private String m_kName;
        private static int m_iInitValue = 0;
        DstBlendMode( String kName )
        {
            m_kName = kName;
            m_iValue = Init();
            ms_pkDstBlendModeMap.put( m_iValue, this );
        }
        private int Init ()
        {
            return m_iInitValue++;
        }
        public int Value() { return m_iValue; }
        public String Name() { return m_kName; }
    };

    public enum TestMode
    {
        TF_NEVER ("TF_NEVER"),
        TF_LESS ("TF_LESS"),
        TF_EQUAL ("TF_EQUAL"),
        TF_LEQUAL ("TF_LEQUAL"),
        TF_GREATER ("TF_GREATER"),
        TF_NOTEQUAL ("TF_NOTEQUAL"),
        TF_GEQUAL ("TF_GEQUAL"),
        TF_ALWAYS ("TF_ALWAYS"),
        TF_QUANTITY ("TF_QUANTITY");

        TestMode( String kName )
        {
            m_kName = kName;
            m_iValue = Init();
            ms_pkTestModeMap.put( m_iValue, this );
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

    private static SrcBlendMode ms_eSrcBlendModeStatic = SrcBlendMode.SBF_QUANTITY;
    private static DstBlendMode ms_eDstBlendModeStatic = DstBlendMode.DBF_QUANTITY;
    private static TestMode ms_eTestModeStatic = TestMode.TF_QUANTITY;


    public StateType GetStateType () { return StateType.ALPHA; }

    public AlphaState () {}

    public void Load (Stream rkStream, Stream.Link pkLink)
    {
        super.Load(rkStream,pkLink);

        // native data
        BlendEnabled = rkStream.ReadBoolean();
        int iSrcBlend = rkStream.ReadInt();
        SrcBlend = ms_pkSrcBlendModeMap.get(iSrcBlend);
        int iDstBlend = rkStream.ReadInt();
        DstBlend = ms_pkDstBlendModeMap.get(iDstBlend);
        TestEnabled = rkStream.ReadBoolean();
        int iTest = rkStream.ReadInt();
        Test = ms_pkTestModeMap.get(iTest);
        Reference = rkStream.ReadFloat();
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
        rkStream.Write(BlendEnabled);
        rkStream.Write(SrcBlend.Value());
        rkStream.Write(DstBlend.Value());
        rkStream.Write(TestEnabled);
        rkStream.Write(Test.Value());
        rkStream.Write(Reference);
    }

    public int GetDiskUsed (final StreamVersion rkVersion)
    {
        return super.GetDiskUsed(rkVersion) +
            Stream.SIZEOF_BOOLEAN + // BlendEnabled
            Stream.SIZEOF_INT + // SrcBlend
            Stream.SIZEOF_INT +  // DstBlend
            Stream.SIZEOF_BOOLEAN + // TestEnabled
            Stream.SIZEOF_INT + // Test
            Stream.SIZEOF_FLOAT;
    }

    public StringTree SaveStrings (final String kString)
    {
        StringTree pkTree = new StringTree();

        // strings
        pkTree.Append(StringTree.Format("AlphaState",GetName()));
        pkTree.Append(super.SaveStrings(null));
        pkTree.Append(StringTree.Format("blend =",BlendEnabled));
        pkTree.Append(StringTree.Format("src = ",SrcBlend.Name()));
        pkTree.Append(StringTree.Format("dst = ",DstBlend.Name()));
        pkTree.Append(StringTree.Format("test =",TestEnabled));
        pkTree.Append(StringTree.Format("test func = ",Test.Name()));
        pkTree.Append(StringTree.Format("test ref =",Reference));
        return pkTree;
    }

    public boolean BlendEnabled = false;                                  // default: false
    public SrcBlendMode SrcBlend = SrcBlendMode.SBF_SRC_ALPHA;            // default: SBF_SRC_ALPHA
    public DstBlendMode DstBlend = DstBlendMode.DBF_ONE_MINUS_SRC_ALPHA;  // default: DBF_ONE_MINUS_SRC_ALPHA
    public boolean TestEnabled = false;                                   // default: false;
    public TestMode Test = TestMode.TF_ALWAYS;                            // default: TF_ALWAYS
    public float Reference = 0.0f;                                        // default: 0, always in [0,1]

    // TO DO.  New member, needs to be streamed.
    public ColorRGBA ConstantColor = new ColorRGBA(0f,0f,0f,0f);          // default: (0,0,0,0)
}
