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
import java.util.HashSet;
import java.util.Iterator;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;

public class PixelProgramCatalog
{
    /** Create the PixelProgramCatalog, with the name of the catalog and the
     * default directory where the images are located.
     * @param rkName, name of the pixel program catalog.
     * @param rkDefaultDir, default directory where programs are located.
     */
    public PixelProgramCatalog ( String rkName, String rkDefaultDir)
    {
        m_kName = new String(rkName);
        m_kDefaultDir = new String(rkDefaultDir);
        m_kRendererType = new String(ms_kNullString);
        m_cCommentChar = 0;
    }

    /** Delete memory. */
    public void dispose()
    {
        m_kName = null;
        m_kDefaultDir = null;
        m_kRendererType = null;
        m_kEntry.clear();
        m_kEntry = null;
        if ( m_spkDefaultPProgram != null )
        {
            m_spkDefaultPProgram.dispose();
            m_spkDefaultPProgram = null;
        }
    }

    /** For deferred setting of the renderer type and comment character.  This
     * cannot be called until the application layer has created a renderer.
     * The layer does so in WindowApplication::SetRenderer.
     * @param rkRendererType, renderer type.
     * @param cCommentChar, comment character.
     */
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

   /** Get the name of the pixel program catalog.
     * @return the name of the pixel program catalog.
     */
    public final String GetName ()
    {
        return m_kName;
    }

    /** Get the name of the default program directory.
     * @return the name of the program directory.
     */
    public final String GetDefaultDir ()
    {
        return m_kDefaultDir;
    }

    /** Add a program to the catalog, do not add if it already exists in the
     * catalog.
     * @param pkProgram, pixel program to add.
     * @return true if the program is added, false otherwise.
     */
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

    /** Remove the pixel program from the catalog.
     * @param pkProgram, program to remove.
     * @return true if the program is removed, false otherwise.
     */
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
        String kKey;
        Iterator kIterator = m_kEntry.keySet().iterator();
        while ( kIterator.hasNext() )
        {
            kKey = (String)kIterator.next();
            if ( kKey.equals(kProgramName) )
            {
                PixelProgram kLocalProgram = m_kEntry.get(kKey);
                if ( kLocalProgram == pkProgram )
                {
                    kIterator.remove();
                    return true;
                }
            }
        }
        // The program does not exist in the catalog.
        return false;

//         PixelProgram kLocalProgram = m_kEntry.get(kProgramName);
//         if (kLocalProgram == null)
//         {
//             // The program does not exist in the catalog.
//             return false;
//         }

//         // The program exists in the catalog.
//         m_kEntry.remove(kProgramName);
//         return true;
    }

    /** Find a pixel program in the catalog based on the program's name. If
     * not in the catalog, try to load from disk.
     * @param rkProgramName, name of the program to fine.
     * @param rkDirectory, name of the directory.
     * @return the desired pixel program, or the default program.
     */
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

    /** Set the active pixel program catalog.
     * @param pkActive, new active pixel program catalog.
     */
    public static void SetActive (PixelProgramCatalog pkActive)
    {
        ms_pkActive = pkActive;
    }

    /** Get the active pixel program catalog.
     * @return the active pixel program catalog.
     */
    public final static PixelProgramCatalog GetActive ()
    {
        return ms_pkActive;
    }

    /** Name of the PixelProgramCatalog -- typically "Main" */
    private String m_kName;
    /** Default directory where programs are stored. */
    private String m_kDefaultDir;
    /** Map <String,PixelProgram> for mapping an program to its name. */
    private HashMap<String,PixelProgram> m_kEntry = new HashMap<String,PixelProgram>();
    /** Default program when no program can be found. */
    private GraphicsObject m_spkDefaultPProgram;
    /** Renderer type. */
    private String m_kRendererType;
    /** Comment character. */
    private char m_cCommentChar;
    /** null string comparison */
    private static final String ms_kNullString = new String("");
    /** default string comparison */
    private static final String ms_kDefaultString = new String("Default");;
    /** Active PixelProgramCatalog. */
    private static PixelProgramCatalog ms_pkActive;
}
