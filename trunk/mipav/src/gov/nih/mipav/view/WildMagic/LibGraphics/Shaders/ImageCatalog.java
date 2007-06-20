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
import gov.nih.mipav.view.WildMagic.LibGraphics.Rendering.*;

import java.util.*;

public class ImageCatalog
{

    public ImageCatalog (String rkName)
    {
        m_kName = new String( rkName );
        // A magenta image to catch your attention that your requested image was
        // not found.
        byte[] aucData = new byte[3];
        aucData[0] = (byte)255;
        aucData[1] = (byte)0;
        aucData[2] = (byte)255;
        m_spkDefaultImage = new GraphicsImage(GraphicsImage.FormatMode.IT_RGB888,1,aucData,
                                      ms_kDefaultString);
    }

    public String GetName ()
    {
        return m_kName;
    }

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
        pkImage = GraphicsImage.Load(rkImageName);
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

    public boolean PrintContents (String rkFilename)
    {
//         const char* acDecorated = System::GetPath(rkFilename.c_str(),
//                                                   System::SM_WRITE);

//         if (acDecorated)
//         {
//             std::ofstream kOStr(acDecorated);
//             assert(kOStr);

//             std::map<std::string,Image*>::const_iterator pkIter;
//             for (pkIter = m_kEntry.begin(); pkIter != m_kEntry.end(); pkIter++)
//             {
//                 Image* pkImage = pkIter->second;
//                 kOStr << pkIter->first << ":" << std::endl;
//                 kOStr << "    dimension = " << pkImage->GetDimension()
//                       << std::endl;
//                 for (int i = 0; i < pkImage->GetDimension(); i++)
//                 {
//                     kOStr << "    bound(" << i << ") = " << pkImage->GetBound(i)
//                           << std::endl;
//                 }
//                 kOStr << "    format = " << pkImage->GetFormatName() << std::endl;
//                 kOStr << std::endl;
//             }
//             kOStr.close();
//             return true;
//         }

        return false;
    }


    public static void SetActive (ImageCatalog pkActive)
    {
        ms_pkActive = pkActive;
    }

    public static ImageCatalog GetActive ()
    {
        return ms_pkActive;
    }

    private String m_kName;
    HashMap<String,GraphicsImage> m_kEntry = new HashMap<String,GraphicsImage>();
    GraphicsImage m_spkDefaultImage;

    private static final String ms_kNullString = new String("");;
    public static final String ms_kDefaultString = new String("Default");
    private static ImageCatalog ms_pkActive = null;
}
