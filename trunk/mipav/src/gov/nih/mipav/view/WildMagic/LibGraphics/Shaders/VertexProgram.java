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
    public static VertexProgram Load (String rkProgramName,
                                      String rkRendererType, char cCommentPrefix)
    {
        String kFilename = new String("v_" + rkProgramName +
                                      "." + rkRendererType + ".wmsp");

        VertexProgram pkProgram = new VertexProgram();
        pkProgram.SetName(rkProgramName);
        System.err.println( rkProgramName );

        boolean bLoaded = Program.Load(kFilename,cCommentPrefix,pkProgram);
        if (!bLoaded)
        {
            //String kFilename = new String( rkProgramName + ".cg" );
            kFilename = new String( rkProgramName + ".cg" );
            String kProgramName = new String( "v_" + rkProgramName );

            int iProfile = CgGL.cgGLGetLatestProfile(CgGL.CG_GL_VERTEX);
            System.err.println( CgGL.cgGetProfileString(iProfile) );
            CgGL.cgGLSetOptimalOptions(iProfile);

            if ( ms_kContext == null )
            {
                ms_kContext = CgGL.cgCreateContext();
            }
            CGprogram kCGProgram = CgGL.cgCreateProgramFromFile( ms_kContext,
                                                                 CgGL.CG_SOURCE,
                                                                 kFilename,
                                                                 iProfile,
                                                                 kProgramName, null);
            RecurseParamsInProgram( pkProgram, kCGProgram );
            bLoaded = true;
            pkProgram.m_kProgramText = CgGL.cgGetProgramString(kCGProgram,CgGL.CG_COMPILED_PROGRAM);

//              bLoaded = Program.LoadCg( CgGL.cgGetProgramString(kCGProgram,CgGL.CG_COMPILED_PROGRAM),
//                                        cCommentPrefix,pkProgram);

            if (!bLoaded)
            {
                pkProgram  = null;
                return null;
            }
        }
        
        VertexProgramCatalog.GetActive().Insert(pkProgram);
        return pkProgram;
    }

    public void finalize ()
    {
        VertexProgramCatalog.GetActive().Remove(this);
        super.finalize();
    }

    public VertexProgram () {}
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
}
