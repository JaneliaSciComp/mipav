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

import java.util.HashMap;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;

public class PixelProgramCatalog
{
    public PixelProgramCatalog ( String rkName, String rkDefaultDir)
    {
        m_kName = new String(rkName);
        m_kDefaultDir = new String(rkDefaultDir);
        m_kRendererType = new String(ms_kNullString);
        m_cCommentChar = 0;
    }

    // For deferred setting of the renderer type and comment character.  This
    // cannot be called until the application layer has created a renderer.
    // The layer does so in WindowApplication::SetRenderer.
    public void SetInformation ( String rkRendererType,
                                 char cCommentChar)
    {
        m_kRendererType = rkRendererType;
        m_cCommentChar = cCommentChar;

        if (m_cCommentChar != 0)
        {
            // Create the default shader, which sets every pixel to magenta.  This
            // is used when your shader cannot be found.  The color should catch
            // your attention.
            m_spkDefaultPProgram = PixelProgram.Load(ms_kDefaultString,m_kDefaultDir);
            assert(m_spkDefaultPProgram != null);
        }
        else
        {
            // Release the default shader.
            m_spkDefaultPProgram = null;
        }
    }

    public String GetName ()
    {
        return m_kName;
    }

    public String GetDefaultDir ()
    {
        return m_kDefaultDir;
    }

    public boolean Insert (PixelProgram pkProgram)
    {
        if (pkProgram == null)
        {
            assert(false);
            return false;
        }

        String kProgramName = new String(pkProgram.GetName());
        if (kProgramName == ms_kNullString
            ||  kProgramName == ms_kDefaultString
            ||  pkProgram == m_spkDefaultPProgram)
        {
            return false;
        }

        // Attempt to find the program in the catalog.
        PixelProgram kLocalProgram = m_kEntry.get(kProgramName);
        if (kLocalProgram != null)
        {
            // The program already exists in the catalog.
            return true;
        }

        // The program does not exist in the catalog, so insert it.
        m_kEntry.put(kProgramName,pkProgram);
        return true;
    }

    public boolean Remove (PixelProgram pkProgram)
    {
        if (pkProgram == null)
        {
            assert(false);
            return false;
        }

        String kProgramName = new String(pkProgram.GetName());
        if (kProgramName == ms_kNullString
            ||  kProgramName == ms_kDefaultString
            ||  pkProgram == m_spkDefaultPProgram)
        {
            return false;
        }

        // Attempt to find the program in the catalog.
        PixelProgram kLocalProgram = m_kEntry.get(kProgramName);
        if (kLocalProgram == null)
        {
            // The program does not exist in the catalog.
            return false;
        }

        // The program exists in the catalog.
        m_kEntry.remove(kProgramName);
        return true;
    }

    public PixelProgram Find ( String rkProgramName, String rkDirectory)
    {
        if (rkProgramName == ms_kNullString
            ||  rkProgramName == ms_kDefaultString)
        {
            return (PixelProgram)(m_spkDefaultPProgram);
        }

        // Attempt to find the program in the catalog.
        PixelProgram kLocalProgram = m_kEntry.get(rkProgramName);
        if (kLocalProgram != null)
        {
            // The program exists in the catalog, so return it.
            return kLocalProgram;
        }

        // Attempt to load the program from disk.
        assert(m_cCommentChar != 0);
        PixelProgram pkProgram = PixelProgram.Load(rkProgramName, rkDirectory);
        if (pkProgram != null)
        {
            // The program exists on disk and is already in the catalog.  The
            // (name,program) pair was automatically inserted into m_kEntry by
            // PixelProgram::Load, so there is no need to insert it again
            // explicitly.
            return pkProgram;
        }

        // The program does not exist.  Use the default program.
        return (PixelProgram)(m_spkDefaultPProgram);
    }

    public boolean PrintContents ( String rkFilename)
    {
//         const char* acDecorated = System::GetPath(rkFilename.c_str(),
//                                                   System::SM_WRITE);
/*
        if (acDecorated)
        {
            std::ofstream kOStr(acDecorated);
            assert(kOStr);

            std::map<std::string,PixelProgram*>::const_iterator pkIter;
            for (pkIter = m_kEntry.begin(); pkIter != m_kEntry.end(); pkIter++)
            {
                // TO DO.  Print out information about the program?
                kOStr << pkIter->first << std::endl;
                kOStr << std::endl;
            }
            kOStr.close();
            return true;
        }
*/
        return false;
    }


    public static void SetActive (PixelProgramCatalog pkActive)
    {
        ms_pkActive = pkActive;
    }

    public static PixelProgramCatalog GetActive ()
    {
        return ms_pkActive;
    }


    private String m_kName;
    private String m_kDefaultDir;
    private HashMap<String,PixelProgram> m_kEntry = new HashMap<String,PixelProgram>();
    private GraphicsObject m_spkDefaultPProgram;
    private String m_kRendererType;
    private char m_cCommentChar;

    private static final String ms_kNullString = new String("");
    private static final String ms_kDefaultString = new String("Default");;
    private static PixelProgramCatalog ms_pkActive;
}
