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

import java.util.HashMap;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;

public class PixelProgramCatalog
{
    public PixelProgramCatalog ( String rkName)
    {
        m_kName = new String(rkName);
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
            m_spkDefaultPProgram = PixelProgram.Load(ms_kDefaultString,
                                                     m_kRendererType,m_cCommentChar);
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

    public PixelProgram Find ( String rkProgramName)
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
        PixelProgram pkProgram = PixelProgram.Load(rkProgramName,
                                                   m_kRendererType,m_cCommentChar);
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
    private HashMap<String,PixelProgram> m_kEntry = new HashMap<String,PixelProgram>();
    private WmObject m_spkDefaultPProgram;
    private String m_kRendererType;
    private char m_cCommentChar;

    private static final String ms_kNullString = new String("");
    private static final String ms_kDefaultString = new String("Default");;
    private static PixelProgramCatalog ms_pkActive;
}
