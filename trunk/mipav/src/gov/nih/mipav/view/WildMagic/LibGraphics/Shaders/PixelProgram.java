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
package gov.nih.mipav.view.WildMagic.LibGraphics.Shaders;

import com.sun.opengl.cg.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
public class PixelProgram extends Program
    implements StreamInterface
{
    // Construction and destruction.
    public static PixelProgram Load (String rkProgramName, String rkDirectory)
    {
        int iProfile = CgGL.cgGLGetLatestProfile(CgGL.CG_GL_FRAGMENT);
        PixelProgram pkProgram = new PixelProgram();
        pkProgram.SetName(rkProgramName);

        boolean bLoaded = false;
        String kFilename = new String( rkDirectory + "\\Shaders\\Cg\\" + rkProgramName + ".cg" );
        String kProgramName = new String( "p_" + rkProgramName );
        //System.err.println(kProgramName);
        CGprogram kCGProgram = CgGL.cgCreateProgramFromFile( pkProgram.m_kContext,
                                                             CgGL.CG_SOURCE,
                                                             kFilename,
                                                             iProfile,
                                                             kProgramName, null);
        pkProgram.m_kCGProgram = kCGProgram;
        if ( kCGProgram != null )
        {
            bLoaded = true;
            pkProgram.RecurseParamsInProgram();
            pkProgram.m_kProgramText = CgGL.cgGetProgramString(kCGProgram,CgGL.CG_COMPILED_PROGRAM);
        }

        if (!bLoaded)
        {
            System.err.println( CgGL.cgGetLastListing(pkProgram.m_kContext) );
            pkProgram.finalize();
            pkProgram = null;
            return null;
        }      
        
        PixelProgramCatalog.GetActive().Insert(pkProgram);
        return pkProgram;
    }

    public void finalize () 
    {
        ms_Options--;
        PixelProgramCatalog.GetActive().Remove(this);
        super.finalize();
    }


    public PixelProgram ()
    {
        if ( ms_Options == 0 )
        {
            int iProfile = CgGL.cgGLGetLatestProfile(CgGL.CG_GL_FRAGMENT);
            //System.err.println( CgGL.cgGetProfileString(iProfile) );
            CgGL.cgGLSetOptimalOptions(iProfile);
        }
        ms_Options++;
    }

    public void Load (Stream rkStream, Stream.Link pkLink)
    {
        super.Load(rkStream,pkLink);
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
    }

    public int GetDiskUsed (StreamVersion rkVersion)
    {
        return super.GetDiskUsed(rkVersion);
    }

    public StringTree SaveStrings (final String acTitle)
    {
        StringTree pkTree = new StringTree();
        pkTree.Append(StringTree.Format("PixelProgram",GetName()));
        pkTree.Append(super.SaveStrings(null));
        return pkTree;
    }

    private static int ms_Options = 0;
}

