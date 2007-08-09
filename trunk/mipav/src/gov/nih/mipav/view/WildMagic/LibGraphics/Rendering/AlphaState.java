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

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;

public class AlphaState extends GlobalState
    implements StreamInterface
{
    /** Maps source blend enum to int values. */
    private static HashMap<Integer,SrcBlendMode> ms_pkSrcBlendModeMap = new HashMap<Integer,SrcBlendMode>();
    /** Maps destination blend enum to int values. */
    private static HashMap<Integer,DstBlendMode> ms_pkDstBlendModeMap = new HashMap<Integer,DstBlendMode>();
    /** Maps alpha-test enum to int values. */
    private static HashMap<Integer,TestMode> ms_pkTestModeMap = new HashMap<Integer,TestMode>();

    /** Static initialization of the AlphaState in the GlobalState.Default
     * array. */
    static
    {
        if ( !DefaultInitialized[StateType.ALPHA.Value()] )
        {
            DefaultInitialized[StateType.ALPHA.Value()] = true;
            Default[StateType.ALPHA.Value()] = new AlphaState();
        }

    }
     
    /** Alpha blending: source blend modes. */
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

    /** Alpha blending: destination blend modes. */
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

    /** Alpha blending: alpha-test modes. */
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

    /** Initializes static enum for src blend modes. */
    private static SrcBlendMode ms_eSrcBlendModeStatic = SrcBlendMode.SBF_QUANTITY;
    /** Initializes static enum for dst blend modes. */
    private static DstBlendMode ms_eDstBlendModeStatic = DstBlendMode.DBF_QUANTITY;
    /** Initializes static enum for alpha-test modes. */
    private static TestMode ms_eTestModeStatic = TestMode.TF_QUANTITY;

    /** Return type.
     * @return StateType.ALPHA;
     */
    public final StateType GetStateType () { return StateType.ALPHA; }

    /** Default constructor. */
    public AlphaState () {}

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

    /**
     * Copies this objects children objects from the input Stream's HashTable,
     * based on the LinkID of the child stored in the pkLink paramter.
     * @param rkStream, the Stream where the child objects are stored.
     * @param pkLink, the Link class from which the child object IDs are read.
     */
    public void Link (Stream rkStream, Stream.Link pkLink)
    {
        super.Link(rkStream,pkLink);
    }

    /**
     * Registers this object with the input Stream parameter. All objects
     * streamed to disk are registered with the Stream so that a unique list
     * of objects is maintained.
     * @param rkStream, the Stream where the child objects are stored.
     * @return true if this object is registered, false if the object has
     * already been registered.
     */
    public boolean Register (Stream rkStream)
    {
        return super.Register(rkStream);
    }

    /**
     * Write this object and all it's children to the Stream.
     * @param rkStream, the Stream where the child objects are stored.
     */
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

    /**
     * Returns the size of this object and it's children on disk for the
     * current StreamVersion parameter.
     * @param rkVersion, the current version of the Stream file being created.
     * @return the size of this object on disk.
     */
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

    /**
     * Write this object into a StringTree for the scene-graph visualization.
     * @param acTitle, the header for this object in the StringTree.
     * @return StringTree containing a String-based representation of this
     * object and it's children.
     */
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

    /** Alpha blending enabled: default: false. */
    public boolean BlendEnabled = false;
    /** Source blend mode default: SBF_SRC_ALPHA */
    public SrcBlendMode SrcBlend = SrcBlendMode.SBF_SRC_ALPHA;
    /** Destination blend mode default: DBF_ONE_MINUS_SRC_ALPHA */
    public DstBlendMode DstBlend = DstBlendMode.DBF_ONE_MINUS_SRC_ALPHA;
    /** Alpha test enabled  default: false; */
    public boolean TestEnabled = false;
    /** Test mode: default: TF_ALWAYS */
    public TestMode Test = TestMode.TF_ALWAYS;
    /** Reference default: 0, always in [0,1] */
    public float Reference = 0.0f;

    // TO DO.  New member, needs to be streamed.
    /** Constant color default: (0,0,0,0) */
    public ColorRGBA ConstantColor = new ColorRGBA(0f,0f,0f,0f);
}
