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

package gov.nih.mipav.view.WildMagic.LibGraphics.Shaders;

import com.sun.opengl.cg.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
public class VertexProgram extends Program
    implements StreamInterface
{
    // Construction and destruction.
    public static VertexProgram Load (String rkProgramName, String rkDirectory)
    {
        int iProfile = CgGL.cgGLGetLatestProfile(CgGL.CG_GL_VERTEX);
        VertexProgram pkProgram = new VertexProgram();
        pkProgram.SetName(rkProgramName);

        boolean bLoaded = false;

        String kFilename = new String( rkDirectory + "\\Shaders\\Cg\\" + rkProgramName + ".cg" );
        String kProgramName = new String( "v_" + rkProgramName );

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
            pkProgram  = null;
            return null;
        }
        
        VertexProgramCatalog.GetActive().Insert(pkProgram);
        return pkProgram;
    }

    
    public void finalize ()
    {
        ms_Options--;
        VertexProgramCatalog.GetActive().Remove(this);
        super.finalize();
    }

    public VertexProgram ()
    {
        if ( ms_Options == 0 )
        {
            int iProfile = CgGL.cgGLGetLatestProfile(CgGL.CG_GL_VERTEX);
            System.err.println( CgGL.cgGetProfileString(iProfile) );
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
        pkTree.Append(StringTree.Format("VertexProgram",GetName()));
        pkTree.Append(super.SaveStrings(null));
        return pkTree;
    }

    private static int ms_Options = 0;
}
