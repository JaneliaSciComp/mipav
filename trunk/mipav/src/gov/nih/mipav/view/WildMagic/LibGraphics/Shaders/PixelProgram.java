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

import java.io.File;
public class PixelProgram extends Program
    implements StreamInterface
{
    /** Load a PixelProgram.
     * @param rkProgramName, the name of the program to load.
     * @param rkDirectory, the directory where the program is located.
     * @return a new PixelProgram, or null if it cannot be loaded.
     */
    public static PixelProgram Load (String rkProgramName, String rkDirectory)
    {
        int iProfile = CgGL.cgGLGetLatestProfile(CgGL.CG_GL_FRAGMENT);
        PixelProgram pkProgram = new PixelProgram();
        pkProgram.SetName(rkProgramName);

        boolean bLoaded = false;
        String kFilename = new String( rkDirectory + File.separator + "Shaders" + File.separator + "Cg" + File.separator + rkProgramName + ".cg" );
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
    /** Delete memory. */
    public void finalize () 
    {
        ms_Options--;
        PixelProgramCatalog.GetActive().Remove(this);
        super.finalize();
    }

    /** Create a PixelProgram, if the profile has not been set, or has been
     * remove, reset the profile options. */
    public PixelProgram ()
    {
        if ( ms_Options == 0 )
        {
            int iProfile = CgGL.cgGLGetLatestProfile(CgGL.CG_GL_FRAGMENT);
            CgGL.cgGLSetOptimalOptions(iProfile);
        }
        ms_Options++;
    }

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
    }

    /**
     * Returns the size of this object and it's children on disk for the
     * current StreamVersion parameter.
     * @param rkVersion, the current version of the Stream file being created.
     * @return the size of this object on disk.
     */
    public int GetDiskUsed (StreamVersion rkVersion)
    {
        return super.GetDiskUsed(rkVersion);
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
        pkTree.Append(StringTree.Format("PixelProgram",GetName()));
        pkTree.Append(super.SaveStrings(null));
        return pkTree;
    }

    /** Keeps track how many times the profile options have been set/reset. */
    private static int ms_Options = 0;
}

