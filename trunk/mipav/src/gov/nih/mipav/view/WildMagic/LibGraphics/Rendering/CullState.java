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

public class CullState extends GlobalState
    implements StreamInterface
{

    private static HashMap<Integer,FrontMode> ms_pkFrontModeMap = new HashMap<Integer,FrontMode>();
    private static HashMap<Integer,CullMode> ms_pkCullModeMap = new HashMap<Integer,CullMode>();

    static 
    {
        if ( !DefaultInitialized[StateType.CULL.Value()] )
        {
            DefaultInitialized[StateType.CULL.Value()] = true;
            Default[StateType.CULL.Value()] = new CullState();
        }
    }

    public StateType GetStateType () { return StateType.CULL; }

    public CullState () {}

    public enum FrontMode
    {
        FT_CCW ( "FT_CCW" ),  // front faces are counterclockwise ordered
        FT_CW ("FT_CW"),   // front faces are clockwise ordered
        FT_QUANTITY ( "FT_QUANTITY" );

        FrontMode( String kName )
        {
            m_kName = kName;
            m_iValue = Init();
            ms_pkFrontModeMap.put( m_iValue, this );
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

    public enum CullMode
    {
        CT_FRONT ("CT_FRONT"),  // cull front-facing triangles
        CT_BACK ("CT_BACK"),   // cull back-facing triangles
        CT_QUANTITY ("CT_QUANTITY");

        CullMode( String kName )
        {
            m_kName = kName;
            m_iValue = Init();
            ms_pkCullModeMap.put( m_iValue, this );
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

    private static FrontMode ms_eFrontModeStatic = FrontMode.FT_QUANTITY;
    private static CullMode ms_eCullModeStatic = CullMode.CT_QUANTITY;

    public boolean Enabled = true;                  // default: true
    public FrontMode FrontFace = FrontMode.FT_CCW;  // default: FT_CCW
    public CullMode CullFace = CullMode.CT_BACK;    // default: CT_BACK

    public void Load (Stream rkStream, Stream.Link pkLink)
    {
        super.Load(rkStream,pkLink);

        // native data
        Enabled = rkStream.ReadBoolean();
        int iFrontFace = rkStream.ReadInt();
        FrontFace = ms_pkFrontModeMap.get(iFrontFace);
        int iCullFace = rkStream.ReadInt();
        CullFace = ms_pkCullModeMap.get(iCullFace);
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
        rkStream.Write(FrontFace.Value());
        rkStream.Write(CullFace.Value());
    }

    public int GetDiskUsed (final StreamVersion rkVersion)
    {
        return super.GetDiskUsed(rkVersion) +
            Stream.SIZEOF_BOOLEAN + //sizeof(char) + // Enabled
            Stream.SIZEOF_INT + //sizeof(int) + // FrontFace
            Stream.SIZEOF_INT; //sizeof(int); // CullFace
    }

    public StringTree SaveStrings (final String acTitle)
    {
        StringTree pkTree = new StringTree();
        // strings
        pkTree.Append(StringTree.Format("CullState",GetName()));
        pkTree.Append(super.SaveStrings(null));
        pkTree.Append(StringTree.Format("enabled =",Enabled));
        pkTree.Append(StringTree.Format("front face = ",FrontFace.Name()));
        pkTree.Append(StringTree.Format("cull face = ",CullFace.Name()));
        return pkTree;
    }


}
