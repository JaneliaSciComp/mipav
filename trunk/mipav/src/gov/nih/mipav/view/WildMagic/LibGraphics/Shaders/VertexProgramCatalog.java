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

public class VertexProgramCatalog
{
    public VertexProgramCatalog (String rkName, String rkDefaultDir)
    {
        m_kName = new String(rkName);
        m_kDefaultDir = new String(rkDefaultDir);
        m_kRendererType = new String(ms_kNullString);
        m_cCommentChar = 0;
    }

    // For deferred setting of the renderer type and comment character.  This
    // cannot be called until the application layer has created a renderer.
    // The layer does so in WindowApplication::SetRenderer.
    public void SetInformation (String rkRendererType,
                                char cCommentChar)
    {
        m_kRendererType = rkRendererType;
        m_cCommentChar = cCommentChar;

        if (m_cCommentChar != 0)
        {
            // Create the default shader, which sets every pixel to magenta.  This
            // is used when your shader cannot be found.  The color should catch
            // your attention.
            m_spkDefaultVProgram = VertexProgram.Load(ms_kDefaultString,m_kDefaultDir);
            assert(m_spkDefaultVProgram != null);
        }
        else
        {
            // Release the default shader.
            m_spkDefaultVProgram = null;
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

    public boolean Insert (VertexProgram pkProgram)
    {
        if (pkProgram == null)
        {
            assert(false);
            return false;
        }

        String kProgramName = new String(pkProgram.GetName());
        if (kProgramName == ms_kNullString
            ||  kProgramName == ms_kDefaultString
            ||  pkProgram == m_spkDefaultVProgram)
        {
            return false;
        }

        // Attempt to find the program in the catalog.
        VertexProgram kLocalProgram = m_kEntry.get(kProgramName);
        if (kLocalProgram != null)
        {
            // The program already exists in the catalog.
            return true;
        }

        // The program does not exist in the catalog, so insert it.
        m_kEntry.put(kProgramName,pkProgram);
        return true;
    }

    public boolean Remove (VertexProgram pkProgram)
    {
        if (pkProgram == null)
        {
            assert(false);
            return false;
        }

        String kProgramName = new String(pkProgram.GetName());
        if (kProgramName == ms_kNullString
            ||  kProgramName == ms_kDefaultString
            ||  pkProgram == m_spkDefaultVProgram)
        {
            return false;
        }

        // Attempt to find the program in the catalog.
        VertexProgram kLocalProgram = m_kEntry.get(kProgramName);
//         std::map<std::string,VertexProgram*>::iterator pkIter =
//             m_kEntry.find(kProgramName);
        if (kLocalProgram == null)
        {
            // The program does not exist in the catalog.
            return false;
        }

        // The program exists in the catalog.
        m_kEntry.remove(kProgramName);
        return true;
    }

    public VertexProgram Find (String rkProgramName, String rkDirectory)
    {
        if (rkProgramName == ms_kNullString
            ||  rkProgramName == ms_kDefaultString)
        {
            return (VertexProgram)(m_spkDefaultVProgram);
        }

        // Attempt to find the program in the catalog.
        VertexProgram kLocalProgram = m_kEntry.get(rkProgramName);
        if (kLocalProgram != null)
        {
            // The program exists in the catalog, so return it.
            return kLocalProgram;
        }

        // Attempt to load the program from disk.  On a successful load, the
        // (name,program) pair was automatically inserted into m_kEntry, so
        // there is no need to insert it again explicitly.
        assert(m_cCommentChar != 0);
        VertexProgram pkProgram = VertexProgram.Load(rkProgramName,rkDirectory);
        if (pkProgram != null)
        {
            // The program exists on disk and is already in the catalog.  The
            // (name,program) pair was automatically inserted into m_kEntry by
            // VertexProgram::Load, so there is no need to insert it again
            // explicitly.
            return pkProgram;
        }

        // The program does not exist.  Use the default program.
        return (VertexProgram)(m_spkDefaultVProgram);
    }

    public boolean PrintContents (String rkFilename)
    {
        /*
        const char* acDecorated = System::GetPath(rkFilename.c_str(),
                                                  System::SM_WRITE);

        if (acDecorated)
        {
            std::ofstream kOStr(acDecorated);
            assert(kOStr);

            std::map<std::string,VertexProgram*>::const_iterator pkIter;
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


    public static void SetActive (VertexProgramCatalog pkActive)
    {
        ms_pkActive = pkActive;
    }

    public static VertexProgramCatalog GetActive ()
    {
        return ms_pkActive;
    }

    private String m_kName;
    private String m_kDefaultDir;
    private HashMap<String,VertexProgram> m_kEntry = new HashMap<String,VertexProgram>();
    private GraphicsObject m_spkDefaultVProgram;
    private String m_kRendererType;
    private char m_cCommentChar;

    private static final String ms_kNullString = new String("");
    private static final String ms_kDefaultString = new String("Default");
    private static VertexProgramCatalog ms_pkActive = null;
}
