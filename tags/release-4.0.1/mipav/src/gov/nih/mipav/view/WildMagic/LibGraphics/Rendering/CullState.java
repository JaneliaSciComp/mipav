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

public class CullState extends GlobalState
    implements StreamInterface
{
    /** Maps front facing mode enum to int values. */
    private static HashMap<Integer,FrontMode> ms_pkFrontModeMap = new HashMap<Integer,FrontMode>();
    /** Maps culling mode enum to int values. */
    private static HashMap<Integer,CullMode> ms_pkCullModeMap = new HashMap<Integer,CullMode>();

    /** Static initialization of the CullState in the GlobalState.Default
     * array. */
    static 
    {
        if ( !DefaultInitialized[StateType.CULL.Value()] )
        {
            DefaultInitialized[StateType.CULL.Value()] = true;
            Default[StateType.CULL.Value()] = new CullState();
        }
    }

    /** Return type.
     * @return StateType.CULL;
     */
    public final StateType GetStateType () { return StateType.CULL; }

    /** Default constructor. */
    public CullState () {}

    /** Front-facing polygon mode. */
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

    /** Culling mode. */
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

    /** Initializes static FrontMode enum */
    private static FrontMode ms_eFrontModeStatic = FrontMode.FT_QUANTITY;
    /** Initializes static CullMode enum */
    private static CullMode ms_eCullModeStatic = CullMode.CT_QUANTITY;

    /** Culling enabled default: true */
    public boolean Enabled = true;
    /** FrontMode default: FT_CCW */
    public FrontMode FrontFace = FrontMode.FT_CCW;
    /** CullMode default: CT_BACK */
    public CullMode CullFace = CullMode.CT_BACK;

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
        int iFrontFace = rkStream.ReadInt();
        FrontFace = ms_pkFrontModeMap.get(iFrontFace);
        int iCullFace = rkStream.ReadInt();
        CullFace = ms_pkCullModeMap.get(iCullFace);
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
        rkStream.Write(Enabled);
        rkStream.Write(FrontFace.Value());
        rkStream.Write(CullFace.Value());
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
            Stream.SIZEOF_BOOLEAN + //sizeof(char) + // Enabled
            Stream.SIZEOF_INT + //sizeof(int) + // FrontFace
            Stream.SIZEOF_INT; //sizeof(int); // CullFace
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
        pkTree.Append(StringTree.Format("CullState",GetName()));
        pkTree.Append(super.SaveStrings(null));
        pkTree.Append(StringTree.Format("enabled =",Enabled));
        pkTree.Append(StringTree.Format("front face = ",FrontFace.Name()));
        pkTree.Append(StringTree.Format("cull face = ",CullFace.Name()));
        return pkTree;
    }
}
