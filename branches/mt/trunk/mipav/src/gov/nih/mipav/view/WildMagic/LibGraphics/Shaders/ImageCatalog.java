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
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;

import java.util.*;

public class ImageCatalog
{

    /** Create the image catalog, with the name of the catalog and the default
     * directory where the images are located.
     * @param rkName, name of the image catalog.
     * @param rkDefaultDir, default directory where images are located.
     */
    public ImageCatalog (String rkName, String rkDefaultDir)
    {
        m_kName = new String( rkName );
        m_kDefaultDir = new String(rkDefaultDir);
        // A magenta image to catch your attention that your requested image was
        // not found.
        byte[] aucData = new byte[3];
        aucData[0] = (byte)255;
        aucData[1] = (byte)0;
        aucData[2] = (byte)255;
        m_spkDefaultImage = new GraphicsImage(GraphicsImage.FormatMode.IT_RGB888,1,aucData,
                                      ms_kDefaultString);
    }
    /** Delete memory. */
    public void finalize()
    {
        m_kName = null;
        m_kDefaultDir = null;
        m_kEntry.clear();
        m_kEntry = null;
        if ( m_spkDefaultImage != null )
        {
            m_spkDefaultImage.finalize();
            m_spkDefaultImage = null;
        }
    }

   /** Get the name of the image catalog.
     * @return the name of the image catalog.
     */
    public final String GetName ()
    {
        return m_kName;
    }
    
    /** Get the name of the default image directory.
     * @return the name of the image directory.
     */
    public final String GetDefaultDir ()
    {
        return m_kDefaultDir;
    }

    /** Add an image to the catalog, do not add if it already exists in the
     * catalog.
     * @param pkImage, image to add.
     * @return true if the image is added, false otherwise.
     */
    public boolean Insert (GraphicsImage pkImage)
    {
        if (pkImage == null)
        {
            assert(false);
            return false;
        }

        String kImageName = new String(pkImage.GetName());
        if (kImageName == ms_kNullString
            ||  kImageName == ms_kDefaultString
            ||  pkImage == m_spkDefaultImage)
        {
            return false;
        }

        // Attempt to find the image in the catalog.
        if ( m_kEntry.containsKey( kImageName ) )
        {
            // The image already exists in the catalog.
            return true;
        }

        // The image does not exist in the catalog, so insert it.
        m_kEntry.put( kImageName,pkImage );
        return true;
    }

    /** Remove the image from the catalog.
     * @param pkImage, image to remove.
     * @return true if the image is removed, false otherwise.
     */
    public boolean Remove (GraphicsImage pkImage)
    {
        if (pkImage == null)
        {
            assert(false);
            return false;
        }

        String kImageName = new String(pkImage.GetName());
        if (kImageName == ms_kNullString
            ||  kImageName == ms_kDefaultString
            ||  pkImage == m_spkDefaultImage)
        {
            return false;
        }

        Object kObj = m_kEntry.remove( kImageName );
        if ( kObj == null )
        {
            // The image does not exist in the catalog.
            return false;
        }
        // The image exists in the catalog.
        return true;
    }

    /** Find an image in the catalog based on the image's name. If not in the
     * catalog, try to load from disk.
     * @param rkImageName, name of the image to fine.
     * @return the desired image, or the default image.
     */
    public GraphicsImage Find ( String rkImageName)
    {
        if (rkImageName == ms_kNullString 
            ||  rkImageName == ms_kDefaultString)
        {
            return m_spkDefaultImage;
        }

        // Attempt to find the image in the catalog.
        GraphicsImage pkImage = (GraphicsImage)m_kEntry.get( rkImageName );
        if ( pkImage != null )
        {
            // The image exists in the catalog, so return it.
            return pkImage;
        }

        // Attempt to load the image from disk.
        pkImage = GraphicsImage.Load(rkImageName, m_kDefaultDir);
        if (pkImage != null)
        {
            // The image exists on disk and is already in the catalog.  The
            // (name,image) pair was automatically inserted into m_kEntry by
            // Image::Load, so there is no need to insert it again
            // explicitly.
            return pkImage;
        }

        // The image does not exist.  Use the default image.
        return m_spkDefaultImage;
    }

    /** Set the active image catalog.
     * @param pkActive, new active image catalog.
     */
    public static void SetActive (ImageCatalog pkActive)
    {
        ms_pkActive = pkActive;
    }

    /** Get the active image catalog.
     * @return the active image catalog.
     */
    public final static ImageCatalog GetActive ()
    {
        return ms_pkActive;
    }

    /** Name of the ImageCatalog -- typically "Main" */
    private String m_kName;
    /** Default directory where images are stored. */
    private String m_kDefaultDir;
    /** Map <String,GraphicsImage> for mapping an image to its name. */
    private HashMap<String,GraphicsImage> m_kEntry = new HashMap<String,GraphicsImage>();
    /** Default image when no image can be found. */
    private GraphicsImage m_spkDefaultImage;
    /** null string comparison */
    private static final String ms_kNullString = new String("");;
    /** default string comparison */
    public static final String ms_kDefaultString = new String("Default");
    /** Active ImageCatalog. */
    private static ImageCatalog ms_pkActive = null;
}
