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

/** Images are stored in files with extension wmif.  The header is of the
 * form "Wild Magic Image File 4.xx" where the major version is 4 and the
 * the minor version is xx in [00,99].  The length of the string is 26, but
 * the null terminator is written to disk, so total number of file bytes used
 * by the version is 27.  The current version is "4.00"
 */
public class ImageVersion
{

    /** Version major number: */
    public static final int MAJOR = 4;
    /** Version minor number: */
    public static final int MINOR = 0;
    /** Version label: */
    public static final String LABEL = new String("Wild Magic Image File 4.00");
    /** Version label length: */
    public static final int LENGTH = LABEL.length(); //27;  // 27 = strlen(LABEL)+1
    /** Current Version: */
    public static final ImageVersion CURRENT = new ImageVersion(MAJOR,MINOR);

    /** Create a new ImageVersion with the iMajor and iMinor version numbers
     * @param iMajor the major version number
     * @param iMinor the minor version number
     */
    public ImageVersion (int iMajor, int iMinor)
    {
        m_iMajor = iMajor;
        m_iMinor = iMinor;
    }

    /** Create a new ImageVersion from a Header String
     * @param acString the Header string containing the major and minor version numbers
     */
    public ImageVersion (final String acString)
    {
        m_iMajor = -1;
        m_iMinor = -1;

        if ((acString != null)
            &&  acString.length() >= LENGTH-1
            &&  ((new Integer(acString.substring(LENGTH-1,LENGTH))).intValue() == 0)
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

    /** Return the major version number
     * @return the major version number
     */
    public int GetMajor ()
    {
        return m_iMajor;
    }
    /** Return the minor version number
     * @return the minor version number
     */
    public int GetMinor ()
    {
        return m_iMinor;
    }

    /** The version is valid if major is 4 and minor in [0,99].
     * @return valid if major is 4 and minor in [0,99].
     */
    public boolean IsValid ()
    {
        return m_iMajor == MAJOR && 0 <= m_iMinor && m_iMinor < 100;
    }

    /** For comparisons of versions.  This is useful whenever a change to the
     * Image class causes a file format change.
     * @param rkVersion the ImageVersion to comare with
     * @return if this ImageVersion is greater or equal to the input ImageVersion
     */
    public boolean GreaterEqual (final ImageVersion rkVersion)
    {
        return GetCombined() >= rkVersion.GetCombined();
    }

    /** Returns the combined version number values: 100*major + minor
     * @return the combined version number values: 100*major + minor
     */
    protected int GetCombined ()  
    {
        return 100*m_iMajor + m_iMinor;
    }

    /** Major and Minor version numbers: */
    protected int m_iMajor, m_iMinor;
}
