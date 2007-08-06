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

package gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem;

/** Scene graphs are stored in files with extension wmof.  The header is of the
 * form "Wild Magic Object File 4.xx" where the major version is 4 and the
 * the minor version is xx in [00,99].  The length of the string is 27, but
 * the null terminator is written to disk, so total number of file bytes used
 * by the version is 28.  The current version is "4.02"
 */
public class StreamVersion
{
    public static final int MAJOR = 4;     // 4
    public static final int MINOR = 0;     // 1
    public static final String LABEL = new String( "Wild Magic Object File 4.02" );
    public static final int LENGTH = LABEL.length();    // 28 = strlen(LABEL)+1
    public static final StreamVersion CURRENT = new StreamVersion(MAJOR,MINOR);

    public StreamVersion ()
    {
        m_iMajor = MAJOR;
        m_iMinor = MINOR;
    }

    public StreamVersion (int iMajor, int iMinor)
    {
        m_iMajor = iMajor;
        m_iMinor = iMinor;
    }

    public StreamVersion (final String acString)
    {
        m_iMajor = -1;
        m_iMinor = -1;

        if ((acString != null)
            &&  acString.length() >= LENGTH-1
            &&  acString.regionMatches( 0,LABEL,0,LENGTH-5) )  // 5 = strlen(" x.yy")
        {
            // The version string is "x.yy".
            final String acVersion = acString.substring( LENGTH - 5 ).trim();
            final String acMajor = acVersion.substring(0,1);
            //m_iMajor = (int)(acVersion[0]-'0');
            m_iMajor = (new Integer(acMajor)).intValue();
            final String acMinor = acVersion.substring(2);
            //m_iMinor = 10*(int)(acVersion[2]-'0') + (int)(acVersion[3]-'0');
            m_iMinor = (new Integer(acMinor)).intValue();
        }
    }


    public int GetMajor ()
    {
        return m_iMajor;
    }

    public int GetMinor ()
    {
        return m_iMinor;
    }

    // The version is valid if major is 4 and minor in [0,99].
    public boolean IsValid ()
    {
        return m_iMajor == StreamVersion.MAJOR
            && 0 <= m_iMinor && m_iMinor < 100;
    }

    // For comparisons of versions.  This is useful in the Stream support in
    // an Object-derived class whenever a change to that class causes a file
    // format change.
    public boolean GreaterEqual (final StreamVersion rkVersion)
    {
        return GetCombined() >= rkVersion.GetCombined();
    }

    protected int GetCombined ()  // 100*major + minor
    {
        return 100*m_iMajor + m_iMinor;
    }

    protected int m_iMajor, m_iMinor;
}
