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
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;

public class MaterialState extends GlobalState
    implements StreamInterface
{
    public StateType GetStateType () { return StateType.MATERIAL; }

    public MaterialState ()
    {
        if ( !DefaultInitialized[StateType.MATERIAL.Value()] )
        {
            DefaultInitialized[StateType.MATERIAL.Value()] = true;
            Default[StateType.MATERIAL.Value()] = new MaterialState();
        }
    }
    public ColorRGB Emissive = new ColorRGB(0f,0f,0f);        // default: ColorRGB(0,0,0)
    public ColorRGB Ambient = new ColorRGB(0.2f,0.2f,0.2f);   // default: ColorRGB(0.2,0.2,0.2)
    public ColorRGB Diffuse = new ColorRGB(0.8f,0.8f,0.8f);   // default: ColorRGB(0.8,0.8,0.8)
    public ColorRGB Specular = new ColorRGB(0.0f,0.0f,0.0f);  // default: ColorRGB(0,0,0)
    public float Alpha = 1.0f;                                // default: 1
    public float Shininess = 1.0f;                            // default: 1

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
        rkStream.Write(Emissive);
        rkStream.Write(Ambient);
        rkStream.Write(Diffuse);
        rkStream.Write(Specular);
        rkStream.Write(Alpha);
        rkStream.Write(Shininess);
    }

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
