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
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;

/**
 * The CompiledProgramCatalog uses the vertex and pixel shader ids to retrieve
 * the compiled GLSL program id.
 */
public class CompiledProgramCatalog
{
    /** Create the CompiledProgramCatalog. */
    public CompiledProgramCatalog () {}

    /** Delete memory. */
    public void dispose()
    {
        m_kEntry.clear();
        m_kEntry = null;
    }

    /** Add a program to the catalog, do not add if it already exists in the catalog.
     * @param pkProgram, the program to add.
     * @param iVertexShaderID, the vertex shader id for the vertex shader contained in the program.
     * @param iPixelShaderID, the pixel shader id for the pixel shader contained in the program.
     * @return true if the program is added, false otherwise.
     */
    public boolean Insert (Program pkProgram, int iVertexShaderID, int iPixelShaderID)
    {
        if (pkProgram == null)
        {
            assert(false);
            return false;
        }

        String kProgramName = new String( iVertexShaderID + " " + iPixelShaderID );

        // Attempt to find the program in the catalog.
        Program kLocalProgram = m_kEntry.get(kProgramName);
        if (kLocalProgram != null)
        {
            // The program already exists in the catalog.
            return true;
        }

        // The program does not exist in the catalog, so insert it.
        m_kEntry.put(kProgramName,pkProgram);
        return true;
    }

    /** Find a program in the catalog.
     * @param iVertexShaderID, the vertex shader id for the vertex shader contained in the program.
     * @param iPixelShaderID, the pixel shader id for the pixel shader contained in the program.
     * @return the program if it exists, null otherwise.
     */
    public Program Find (int iVertexShaderID, int iPixelShaderID)
    {
        String kProgramName = new String( iVertexShaderID + " " + iPixelShaderID );

        if ( kProgramName == ms_kNullString )
        {
            return null;
        }

        // Attempt to find the program in the catalog.
        Program kLocalProgram = m_kEntry.get(kProgramName);
        if (kLocalProgram != null)
        {
            // The program exists in the catalog, so return it.
            return kLocalProgram;
        }
        return null;
    }

    /** Set the active compiled program catalog.
     * @param pkActive, new active compiled program catalog.
     */
    public static void SetActive (CompiledProgramCatalog pkActive)
    {
        ms_pkActive = pkActive;
    }

    /** Get the active compiled program catalog.
     * @return the active compiled program catalog.
     */
    public final static CompiledProgramCatalog GetActive ()
    {
        return ms_pkActive;
    }

    /** Map <String,Program> for mapping an program to its name. */
    private HashMap<String,Program> m_kEntry = new HashMap<String,Program>();
    /** null string comparison */
    private static final String ms_kNullString = new String("-1 -1");
    /** Active CompiledProgramCatalog. */
    private static CompiledProgramCatalog ms_pkActive;
}
