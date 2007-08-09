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

public class PolygonOffsetState extends GlobalState
    implements StreamInterface
{

    /** Static initialization of the PolygonOffsetState in the
     * GlobalState.Default array. */
    static
    {
        if ( !DefaultInitialized[StateType.POLYGONOFFSET.Value()] )
        {
            DefaultInitialized[StateType.POLYGONOFFSET.Value()] = true;
            Default[StateType.POLYGONOFFSET.Value()] = new PolygonOffsetState();
        }
    }

    /** Default constructor. */
    public PolygonOffsetState () {}

    /** Return type.
     * @return StateType.POLYGONOFFSET;
     */
    public final StateType GetStateType () { return StateType.POLYGONOFFSET; }

    /** Set whether offset should be enabled for the various polygon drawing
     * modes (fill, line, point).
     */
    public boolean FillEnabled = false;   // default: false
    /** Set whether offset should be enabled for the various polygon drawing
     * modes (fill, line, point).
     */
    public boolean LineEnabled = false;   // default: false
    /** Set whether offset should be enabled for the various polygon drawing
     * modes (fill, line, point).
     */
    public boolean PointEnabled = false;  // default: false

    /** The offset is Scale*dZ + Bias*r where dZ is the change in depth
     * relative to the screen space area of the poly, and r is the smallest
     * resolvable depth difference.  Negative values move polygons closer to
     * the eye.
     */
    public float Scale = 0.0f;  // default: 0.0
    /** The offset is Scale*dZ + Bias*r where dZ is the change in depth
     * relative to the screen space area of the poly, and r is the smallest
     * resolvable depth difference.  Negative values move polygons closer to
     * the eye.
     */
    public float Bias = 0.0f;   // default: 0.0

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
        FillEnabled = rkStream.ReadBoolean();
        LineEnabled = rkStream.ReadBoolean();
        PointEnabled = rkStream.ReadBoolean();
        Scale = rkStream.ReadFloat();
        Bias = rkStream.ReadFloat();

    }

    /**
     * Write this object and all it's children to the Stream.
     * @param rkStream, the Stream where the child objects are stored.
     */
    public void Save (Stream rkStream)
    {
        super.Save(rkStream);

        // native data
        rkStream.Write(FillEnabled);
        rkStream.Write(LineEnabled);
        rkStream.Write(PointEnabled);
        rkStream.Write(Scale);
        rkStream.Write(Bias);
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
            Stream.SIZEOF_BOOLEAN + //sizeof(char) + // FillEnabled
            Stream.SIZEOF_BOOLEAN + //sizeof(char) + // LineEnabled
            Stream.SIZEOF_BOOLEAN + //sizeof(char) + // PointEnabled
            Stream.SIZEOF_FLOAT + //sizeof(Scale) +
            Stream.SIZEOF_FLOAT; //sizeof(Bias);
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
        pkTree.Append(StringTree.Format("PolygonOffsetState",GetName()));
        pkTree.Append(super.SaveStrings(null));
        pkTree.Append(StringTree.Format("fill enabled =",FillEnabled));
        pkTree.Append(StringTree.Format("line enabled =",LineEnabled));
        pkTree.Append(StringTree.Format("point enabled =",PointEnabled));
        pkTree.Append(StringTree.Format("scale =",Scale));
        pkTree.Append(StringTree.Format("bias =",Bias));
        return pkTree;
    }
}
