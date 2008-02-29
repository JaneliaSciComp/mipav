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
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;

public class MaterialState extends GlobalState
    implements StreamInterface
{
    /** Static initialization of the MaterialState in the GlobalState.Default
     * array. */
    static
    {
        if ( !DefaultInitialized[StateType.MATERIAL.Value()] )
        {
            DefaultInitialized[StateType.MATERIAL.Value()] = true;
            Default[StateType.MATERIAL.Value()] = new MaterialState();
        }
    }

    /** Default constructor. */
    public MaterialState() {}

    /** delete memory */
    public void dispose ()
    {
        if ( Emissive != null )
        {
            Emissive.dispose();
            Emissive = null;
        }
        if ( Ambient != null )
        {
            Ambient.dispose();
            Ambient = null;
        }
        if ( Diffuse != null )
        {
            Diffuse.dispose();
            Diffuse = null;
        }
        if ( Specular != null )
        {
            Specular.dispose();
            Specular = null;
        }
        super.dispose();
    }

    /** Return type.
     * @return StateType.MATERIAL;
     */
    public final StateType GetStateType () { return StateType.MATERIAL; }


    /** Emissive color default: ColorRGB(0,0,0) */
    public ColorRGB Emissive = new ColorRGB(0f,0f,0f);        
    /** Ambient color default: ColorRGB(0.2,0.2,0.2) */
    public ColorRGB Ambient = new ColorRGB(0.2f,0.2f,0.2f);
    /** Diffuse color default: ColorRGB(0.8,0.8,0.8) */
    public ColorRGB Diffuse = new ColorRGB(0.8f,0.8f,0.8f);
    /** Specular color default: ColorRGB(0,0,0) */
    public ColorRGB Specular = new ColorRGB(0.0f,0.0f,0.0f);
    /** Alpha, default: 1 */
    public float Alpha = 1.0f;
    /** Shininess default: 1 */
    public float Shininess = 1.0f;

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
        rkStream.Read(Emissive);
        rkStream.Read(Ambient);
        rkStream.Read(Diffuse);
        rkStream.Read(Specular);
        Alpha = rkStream.ReadFloat();
        Shininess = rkStream.ReadFloat();
    }

    /**
     * Write this object and all it's children to the Stream.
     * @param rkStream, the Stream where the child objects are stored.
     */
    public void Save (Stream rkStream)
    {
        super.Save(rkStream);

        // native data
        rkStream.Write(Emissive);
        rkStream.Write(Ambient);
        rkStream.Write(Diffuse);
        rkStream.Write(Specular);
        rkStream.Write(Alpha);
        rkStream.Write(Shininess);
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
            3 * Stream.SIZEOF_FLOAT + //sizeof(Emissive) +
            3 * Stream.SIZEOF_FLOAT + //sizeof(Ambient) +
            3 * Stream.SIZEOF_FLOAT + //sizeof(Diffuse) +
            3 * Stream.SIZEOF_FLOAT + //sizeof(Specular) +
            Stream.SIZEOF_FLOAT + //sizeof(Alpha) +
            Stream.SIZEOF_FLOAT; //sizeof(Shininess);
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
        pkTree.Append(StringTree.Format("MaterialState",GetName()));
        pkTree.Append(super.SaveStrings(null));
        pkTree.Append(StringTree.Format("emissive =",Emissive));
        pkTree.Append(StringTree.Format("ambient =",Ambient));
        pkTree.Append(StringTree.Format("diffuse =",Diffuse));
        pkTree.Append(StringTree.Format("alpha =",Alpha));
        pkTree.Append(StringTree.Format("shininess =",Shininess));
        return pkTree;
    }
}
