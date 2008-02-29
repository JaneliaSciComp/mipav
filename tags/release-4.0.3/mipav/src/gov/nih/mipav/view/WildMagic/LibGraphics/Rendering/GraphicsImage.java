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

package gov.nih.mipav.view.WildMagic.LibGraphics.Rendering;

import java.nio.Buffer;
import com.sun.opengl.util.BufferUtil;
import java.nio.ByteBuffer;
import java.nio.FloatBuffer;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.ByteArrayInputStream;

import java.util.HashMap;

import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.Shaders.*;

/** The color image data is stored so that the red channel is in low
 * memory, the green channel is next, the blue channel after that, and the
 * alpha channel in high memory.  For example, the first pixel has the
 * following layout.
 *
 *   IT_RGB888:
 *     r = aucData[0];
 *     g = aucData[1];
 *     b = aucData[2];
 *
 *   IT_RGBA8888:
 *     r = aucData[0];
 *     g = aucData[1];
 *     b = aucData[2];
 *     a = aucData[3];
 *
 * Depth image data is always 'float' with values in [0,1].  The
 * specification of 16, 24, or 32 is for setting up the hardware with the
 * correct size depth buffer.  Data read into a depth image from the depth
 * buffer will be converted to [0,1] if necessary.
 *
 * Construction and destruction.  GraphicsImage accepts responsibility for
 * deleting the input array.  The acImageName field is used as a unique
 * identifier for the image for purposes of sharing.  The caller of
 * the constructor may provided a name.  If not, the constructor generates
 * a unique name "imageN.wmif" where N is the Object::m_uiID field. A
 * global map of images is maintained for sharing purposes.
 *
 * NOTE:  GraphicsImage dimensions must be a power of two.  Assert statements are
 * placed in the constructors to trap when the dimensions are not power
 * of two.
 */
public class GraphicsImage extends GraphicsObject
    implements StreamInterface
{
    /** For a lookup of the renderer constant type from its string name. */
    private static HashMap<Integer,FormatMode> ms_pkFormatModeMap = new HashMap<Integer,FormatMode>();

    /** For a lookup of the renderer constant type from its string name. */
    private static HashMap<Integer,Type> ms_pkTypeMap = new HashMap<Integer,Type>();

    /** GraphicsImage format mode: */
    public enum FormatMode
    {
        IT_RGB888 ( 3, "IT_RGB888" ),
        IT_RGBA8888 ( 4, "IT_RGBA8888" ),
        IT_DEPTH16 ( 4, "IT_DEPTH16" ),
        IT_DEPTH24 ( 4, "IT_DEPTH24" ),
        IT_DEPTH32 ( 4, "IT_DEPTH32" ),
        IT_CUBE_RGB888 ( 3, "IT_CUBE_RGB888" ),
        IT_CUBE_RGBA8888 ( 4, "IT_CUBE_RGBA8888" ),
        IT_RGB32 ( 12, "IT_RGB32" ),
        IT_RGBA32 ( 16, "IT_RGBA32" ),
        IT_L8 ( BufferUtil.SIZEOF_FLOAT, "IT_L8" ),
        IT_QUANTITY ( 0, "" );

        private final int ms_aiBytesPerPixel;
        private final String ms_akFormatName;
        FormatMode( int iBytesPerPixel, String kFormatName )
        {
            this.ms_aiBytesPerPixel = iBytesPerPixel;
            this.ms_akFormatName = kFormatName;
            m_iValue = Init();
            ms_pkFormatModeMap.put( m_iValue, this );
        }
        public int BytesPerPixel() { return ms_aiBytesPerPixel; }
        public String FormatName() { return ms_akFormatName; }

        private int Init ()
        {
            return m_iInitValue++;
        }
        public int Value() { return m_iValue; }
        private static int m_iInitValue = 0;
        private int m_iValue;

    };

    /** GraphicsImage format mode: */
    public enum Type
    {
        IT_BYTE ( 1, "IT_BYTE" ),
        IT_UBYTE ( 1, "IT_UBYTE" ),
        IT_SHORT ( 2, "IT_SHORT" ),
        IT_USHORT ( 2, "IT_USHORT" ),
        IT_INT ( 4, "IT_INT" ),
        IT_UINT ( 4, "IT_UINT" ),
        IT_LONG ( 8, "IT_LONG" ),
        IT_FLOAT ( 4, "IT_FLOAT" ),
        IT_DOUBLE ( 8, "IT_DOUBLE" ),
        IT_QUANTITY ( 0, "" );

        private final int ms_aiBytesPerPixel;
        private final String ms_kName;
        Type( int iBytesPerPixel, String kFormatName )
        {
            this.ms_aiBytesPerPixel = iBytesPerPixel;
            this.ms_kName = kFormatName;
            m_iValue = Init();
            ms_pkTypeMap.put( m_iValue, this );
        }
        public int BytesPerPixel() { return ms_aiBytesPerPixel; }
        public String Name() { return ms_kName; }

        private int Init ()
        {
            return m_iInitValue++;
        }
        public int Value() { return m_iValue; }
        private static int m_iInitValue = 0;
        private int m_iValue;

    };

    /** Initializes format mode static enum: */
    private static FormatMode m_eFormatModeStatic = FormatMode.IT_QUANTITY;
    /** Initializes format mode static enum: */
    private static Type m_eTypeStatic = Type.IT_QUANTITY;


    /** Construct a 1D image (float)
     * @param eFormat, image format
     * @param iBound0, size of image.
     * @param afData, image data.
     * @param acImageName, image name.
     */
    public GraphicsImage (FormatMode eFormat, int iBound0,
                          float[] afData, String acImageName )
    {
        assert(BitHacks.IsPowerOfTwo(iBound0));
        assert(acImageName != null);
        
        m_eFormat = eFormat;
        m_iDimension = 1;
        m_aiBound[0] = iBound0;
        m_aiBound[1] = 1;
        m_aiBound[2] = 1;
        m_iQuantity = iBound0;
        m_afData = afData;
        SetName(acImageName);

        if( acImageName == ImageCatalog.ms_kDefaultString )
        {
            return;
        }
        ImageCatalog.GetActive().Insert(this);
    }

    /** Construct a 1D image (byte)
     * @param eFormat, image format
     * @param iBound0, size of image.
     * @param aucData, image data.
     * @param acImageName, image name.
     */
    public GraphicsImage (FormatMode eFormat, int iBound0,
                          byte[] aucData, String acImageName )
    {
        assert(BitHacks.IsPowerOfTwo(iBound0));
        assert(acImageName != null);
  
        m_eFormat = eFormat;
        m_iDimension = 1;
        m_aiBound[0] = iBound0;
        m_aiBound[1] = 1;
        m_aiBound[2] = 1;
        m_iQuantity = iBound0;
        m_aucData = aucData;
        SetName(acImageName);

        if( acImageName == ImageCatalog.ms_kDefaultString )
        {
            return;
        }
        ImageCatalog.GetActive().Insert(this);
    }

    
    /** Construct a 2D image (byte)
     * @param eFormat, image format
     * @param iBound0, image dimension 1.
     * @param iBound1, image dimension 2.
     * @param aucData, image data.
     * @param acImageName, image name.
     */
    public GraphicsImage (FormatMode eFormat, int iBound0, int iBound1,
                          byte[] aucData, String acImageName)
    {
        assert(BitHacks.IsPowerOfTwo(iBound0)
               && BitHacks.IsPowerOfTwo(iBound1));
        assert(acImageName != null);

        m_eFormat = eFormat;
        m_iDimension = 2;
        m_aiBound[0] = iBound0;
        m_aiBound[1] = iBound1;
        m_aiBound[2] = 1;
        m_iQuantity = iBound0*iBound1;
        m_aucData = aucData;
        SetName(acImageName);
        ImageCatalog.GetActive().Insert(this);
    }


    /** Construct a 3D image (byte)
     * @param eFormat, image format
     * @param iBound0, image dimension 1.
     * @param iBound1, image dimension 2.
     * @param iBound1, image dimension 2.
     * @param aucData, image data.
     * @param acImageName, image name.
     */
    public GraphicsImage (FormatMode eFormat, int iBound0, int iBound1, int iBound2,
                          byte[] aucData, String acImageName)
    {
        assert(BitHacks.IsPowerOfTwo(iBound0)
               && BitHacks.IsPowerOfTwo(iBound1)
               && BitHacks.IsPowerOfTwo(iBound2));
        assert(acImageName != null);

        m_eFormat = eFormat;
        m_iDimension = 3;
        m_aiBound[0] = iBound0;
        m_aiBound[1] = iBound1;
        m_aiBound[2] = iBound2;
        m_iQuantity = iBound0*iBound1*iBound2;
        m_aucData = aucData;
        SetName(acImageName);
        ImageCatalog.GetActive().Insert(this);
    }

    /** Construct a 3D image (float)
     * @param eFormat, image format
     * @param iBound0, image dimension 1.
     * @param iBound1, image dimension 2.
     * @param iBound1, image dimension 2.
     * @param afData, image data.
     * @param acImageName, image name.
     */
    public GraphicsImage (FormatMode eFormat, int iBound0, int iBound1, int iBound2,
                          float[] afData, String acImageName)
    {
        assert(BitHacks.IsPowerOfTwo(iBound0)
               && BitHacks.IsPowerOfTwo(iBound1)
               && BitHacks.IsPowerOfTwo(iBound2));
        assert(acImageName != null);

        m_eFormat = eFormat;
        m_iDimension = 3;
        m_aiBound[0] = iBound0;
        m_aiBound[1] = iBound1;
        m_aiBound[2] = iBound2;
        m_iQuantity = iBound0*iBound1*iBound2;
        m_afData = afData;
        SetName(acImageName);
        ImageCatalog.GetActive().Insert(this);
    }

    /** Construct a 3D image (float)
     * @param eFormat, image format
     * @param iBound0, image dimension 1.
     * @param iBound1, image dimension 2.
     * @param iBound1, image dimension 2.
     * @param afData, image data.
     * @param acImageName, image name.
     */
    public GraphicsImage (FormatMode eFormat, int iBound0, int iBound1, int iBound2,
                          Buffer kBuffer, String acImageName)
    {
        assert(BitHacks.IsPowerOfTwo(iBound0)
               && BitHacks.IsPowerOfTwo(iBound1)
               && BitHacks.IsPowerOfTwo(iBound2));
        assert(acImageName != null);

        m_eFormat = eFormat;
        m_iDimension = 3;
        m_aiBound[0] = iBound0;
        m_aiBound[1] = iBound1;
        m_aiBound[2] = iBound2;
        m_iQuantity = iBound0*iBound1*iBound2;

        m_kBuffer = kBuffer;

        SetName(acImageName);
        ImageCatalog.GetActive().Insert(this);
    }



    /** Default constructor. */
    public GraphicsImage ()
    {
        m_eFormat = FormatMode.IT_QUANTITY;
        m_iDimension = 0;
        m_aiBound[0] = 0;
        m_aiBound[1] = 0;
        m_aiBound[2] = 0;
        m_iQuantity = 0;
        m_aucData = null;
    }
    
    /** delete memory */
    public void dispose ()
    {
        m_aiBound = null;
        m_aucData = null;
        m_afData = null;
        ImageCatalog.GetActive().Remove(this);
        super.dispose();
    }

    /** Return image format.
     * @return image format. */
    public final FormatMode GetFormat ()
    {
        return m_eFormat;
    }
    
    /** Return image format name.
     * @return image format name. */
    public final String GetFormatName ()
    {
        return m_eFormat.FormatName();
    }

    /** Return image format name for the input enum.
     * @param eFormat format enum.
     * @return image format name. */
    public static String GetFormatName (FormatMode eFormat)
    {
        return eFormat.FormatName();
    }

    /** Return true if this is a depth image.
     * @return true if this is a depth image.
     */
    public final boolean IsDepthImage ()
    {
        return (m_eFormat == FormatMode.IT_DEPTH16
            || m_eFormat == FormatMode.IT_DEPTH24
            || m_eFormat == FormatMode.IT_DEPTH32);
    }

    /** Return true if this is a cube image.
     * @return true if this is a cube image.
     */
    public final boolean IsCubeImage ()
    {
        return (m_eFormat == FormatMode.IT_CUBE_RGB888 || m_eFormat == FormatMode.IT_CUBE_RGBA8888);
    }

    /** Get the number of bytes per pixel in the image.
     * @return the number of bytes per pixel in the image. */
    public final int GetBytesPerPixel ()
    {
        return m_eFormat.BytesPerPixel();
    }

    /** Get the number of bytes per pixel for the given format enum.
     * @param eFormat, format type.
     * @return the number of bytes per pixel for the given format enum. */
    public final static int GetBytesPerPixel (FormatMode eFormat)
    {
        return eFormat.BytesPerPixel();
    }

    /** Get the dimension of this image (1D, 2D, 3D).
     * @return the dimension of this image (1D, 2D, 3D).
     */
    public final int GetDimension ()
    {
        return m_iDimension;
    }

    /** Get the image extent in the given dimension.
     * @param i, dimension.
     * @return the image extent in the given dimension.
     */
    public int GetBound (int i)
    {
        assert(0 <= i && i < 3);
        return m_aiBound[i];
    }

    /** Get the image size.
     * @return the image size.
     */
    public final int GetQuantity ()
    {
        return m_iQuantity;
    }

    /** Get the image data.
     * @return the image data.
     */
    public final byte[] GetData ()
    {
        return m_aucData;
    }

    /** Set the image data for 1D image.
     * @param aucData, new image data.
     * @param iSize, new image size.
     */
    public void SetData ( byte[] aucData, int iSize )
    {
        if ( m_aucData != null )
        {
            m_aucData = null;
        }
        m_aucData = aucData;
        m_aiBound[0] = iSize;
    }

    /** Set the image data for 2D image.
     * @param aucData, new image data.
     * @param iXSize, new image x-size.
     * @param iYSize, new image y-size.
     */
    public void SetData ( byte[] aucData, int iXSize, int iYSize )
    {
        if ( m_aucData != null )
        {
            m_aucData = null;
        }
        m_aucData = aucData;
        m_aiBound[0] = iXSize;
        m_aiBound[1] = iYSize;
    }

    /** Set the image data for 3D image.
     * @param aucData, new image data.
     * @param iXSize, new image x-size.
     * @param iYSize, new image y-size.
     * @param iZSize, new image z-size.
     */
    public void SetData ( byte[] aucData, int iXSize, int iYSize, int iZSize )
    {
        if ( m_aucData != null )
        {
            m_aucData = null;
        }
        m_aucData = aucData;
        m_aiBound[0] = iXSize;
        m_aiBound[1] = iYSize;
        m_aiBound[2] = iZSize;
    }

    /** Get image float data.
     * @return image float data.
     */
    public float[] GetFloatData ()
    {
        return m_afData;
    }

    /** Set the image data for 1D image.
     * @param afData, new image data.
     * @param iSize, new image size.
     */
    public void SetFloatData ( float[] afData, int iSize )
    {
        if ( m_afData != null )
        {
            m_afData = null;
        }
        m_afData = afData;
        m_aiBound[0] = iSize;
    }

    /** Set the image data for 2D image.
     * @param afData, new image data.
     * @param iXSize, new image x-size.
     * @param iYSize, new image y-size.
     */
    public void SetFloatData ( float[] afData, int iXSize, int iYSize )
    {
        if ( m_afData != null )
        {
            m_afData = null;
        }
        m_afData = afData;
        m_aiBound[0] = iXSize;
        m_aiBound[1] = iYSize;
    }

    /** Set the image data for 3D image.
     * @param afData, new image data.
     * @param iXSize, new image x-size.
     * @param iYSize, new image y-size.
     * @param iZSize, new image z-size.
     */
    public void SetFloatData ( float[] afData, int iXSize, int iYSize, int iZSize )
    {
        if ( m_afData != null )
        {
            m_afData = null;
        }
        m_afData = afData;
        m_aiBound[0] = iXSize;
        m_aiBound[1] = iYSize;
        m_aiBound[2] = iZSize;
    }

    /** Get the imag data in Buffer format.
     * @return the imag data in Buffer format.
     */
    public Buffer GetDataBuffer ()
    {
        if ( m_kBuffer != null )
        {
            return m_kBuffer;
        }

        Buffer imageBuf = null;
        if ( m_aucData != null )
        {
            imageBuf = ByteBuffer.wrap( m_aucData );        
            imageBuf.rewind();
        }
        else if ( m_afData != null )
        {
            imageBuf = FloatBuffer.wrap( m_afData );          
            imageBuf.rewind();          
        }
        if ( imageBuf == null )
        {
            //m_aucData = new byte[m_iQuantity * 4];
            //imageBuf = ByteBuffer.wrap( m_aucData );        
            //imageBuf.rewind();
        }
        return imageBuf;                  
    }

    /** Create an image of ColorRGBA values.  
     * @return an image of the same width and height for these formats.  The
     * returned image is dynamically allocated; the caller is responsible for
     * deleting it.
     */
    public ColorRGBA[] CreateRGBA ()
    {
        if (!IsCubeImage())
        {
            ColorRGBA[] akCImage = new ColorRGBA[m_iQuantity];
            CopyRGBA(akCImage);
            return akCImage;
        }

        // Cube maps are handled as six separate images, so there is no need to
        // create an RGBA image here.
        return null;
    }

    /** Copy to an already existing image of ColorRGBA values.  
     * @param akCImage, The input array must have the correct dimensions as the
     * GraphicsImage itself.
     */
    public void CopyRGBA (ColorRGBA[] akCImage)
    {
        if (m_eFormat == FormatMode.IT_RGB888)
        {
            for (int i = 0; i < m_iQuantity; i++)
            {
                akCImage[i].R( fInv255*(float)(m_aucData[i*3 + 0]) );
                akCImage[i].G( fInv255*(float)(m_aucData[i*3 + 1]) );
                akCImage[i].B( fInv255*(float)(m_aucData[i*3 + 2]) );
                akCImage[i].A( 1.0f );
            }
        }
        else if (m_eFormat == FormatMode.IT_RGBA8888)
        {
            for (int i = 0; i < m_iQuantity; i++)
            {
                akCImage[i].R( fInv255*(float)(m_aucData[i*4 + 0]) );
                akCImage[i].G( fInv255*(float)(m_aucData[i*4 + 1]) );
                akCImage[i].B( fInv255*(float)(m_aucData[i*4 + 2]) );
                akCImage[i].A( fInv255*(float)(m_aucData[i*4 + 3]) );
            }
        }
        else if (IsDepthImage())
        {
            for (int i = 0; i < m_iQuantity; i++)
            {
                float fValue = (float)(m_aucData[i]);
                akCImage[i].R( fValue );
                akCImage[i].G( fValue );
                akCImage[i].B( fValue );
                akCImage[i].A( 1.0f );
            }
        }
        else
        {
            // Cube images are handled as six 2D images in the software renderer,
            // so no conversion is needed.
        }
    }


    /** Streaming support.  The sharing system is automatically invoked by
     * these calls.  In Load, if an image corresponding to the filename is
     * already in memory, then that image is returned (i.e. shared).
     * Otherwise, a new image is created and returned.  The filename is used
     * as the image name.
     * @param acImageName, image name
     * @param rkDirectory, directory where the image is located.
     * @return new GraphicsImage or null if it cannot be loaded.
     */
    public static GraphicsImage Load (String acImageName, String rkDirectory)
    {
        assert(acImageName != null);

        String kFilename = new String( rkDirectory + File.separator + "Shaders" + File.separator + "Images" + File.separator + acImageName + ".wmif");

        ByteArrayInputStream acBuffer = GraphicsImage.LoadFile(kFilename);
        if (acBuffer == null)
        {
            // file does not exist
            return null;
        }
        int iSize = acBuffer.available();
        if (iSize < ImageVersion.LENGTH)
        {
            // file not large enough to store version string
            acBuffer = null;
            return null;
        }

        // read the file version
        byte[] abHeader = new byte[ImageVersion.LENGTH];
        acBuffer.read( abHeader, 0, ImageVersion.LENGTH);

        String kHeader = new String( abHeader );
        ImageVersion kVersion = new ImageVersion(kHeader);
        if (!kVersion.IsValid())
        {
            acBuffer = null;
            return null;
        }
        // read the image format and dimensions
        acBuffer.read( abHeader, 0, 1);

        byte[] abCurrent = new byte[4];
        acBuffer.read( abCurrent, 0, 4);
        // read the image format and dimensions
        int iFormat = 0;
        for (int i=3;i>=0;i--) iFormat= (iFormat << 8) | (abCurrent[i] & 0xFF);

        int iDimension = 0;
        if (kVersion.GreaterEqual( new ImageVersion(3,1) ))
        {
            acBuffer.read( abCurrent, 0, 4);
            for (int i=3;i>=0;i--) iDimension= (iDimension << 8) | (abCurrent[i] & 0xFF);
        }
        else
        {
            iDimension = 2;
        }
        int[] aiBound = new int[]{0,0,0};
        acBuffer.read( abCurrent, 0, 4);
        for (int i=3;i>=0;i--) aiBound[0]= (aiBound[0] << 8) | (abCurrent[i] & 0xFF);
        acBuffer.read( abCurrent, 0, 4);
        for (int i=3;i>=0;i--) aiBound[1]= (aiBound[1] << 8) | (abCurrent[i] & 0xFF);

        if (kVersion.GreaterEqual( new ImageVersion(3,1) ))
        {
            acBuffer.read( abCurrent, 0, 4);
            for (int i=3;i>=0;i--) aiBound[2]= (aiBound[2] << 8) | (abCurrent[i] & 0xFF);
        }
        else
        {
            aiBound[2] = 1;
        }
        FormatMode eFormat = ms_pkFormatModeMap.get(iFormat);
        int iQuantity = aiBound[0]*aiBound[1]*aiBound[2];

        // read the image data
        int iDataSize = eFormat.BytesPerPixel()*iQuantity;
        byte[] aucData = new byte[iDataSize];
        acBuffer.read( aucData, 0, iDataSize );

        GraphicsImage pkImage = null;
        switch (iDimension)
        {
        case 1:
            pkImage = new GraphicsImage(eFormat,aiBound[0],aucData,acImageName);
            break;
        case 2:
            pkImage = new GraphicsImage(eFormat,aiBound[0],aiBound[1],aucData,
                                        acImageName);
            break;
        case 3:
            pkImage = new GraphicsImage(eFormat,aiBound[0],aiBound[1],aiBound[2],
                                        aucData,acImageName);
            break;
        default:
            assert(false);
        }

        acBuffer = null;
        return pkImage;
    }
    /** Opens an input stream from the input filename.
     * @param acFilename, image file name.
     * @return  an input stream for reading the file.
     */
    private static ByteArrayInputStream LoadFile (String acFilename)
    {
        File kFile = new File(acFilename);
        if ( !kFile.exists() || !kFile.canRead() )
        {
            return null;
        }
        int iLength = (int)kFile.length();
        if ( iLength <= 0 )
        {
            return null;
        }
        try {
            FileInputStream kFileReader = new FileInputStream(kFile);
            byte[] racBuffer = new byte[iLength];
            kFileReader.read(racBuffer,0,iLength);
            kFileReader.close();
            ByteArrayInputStream kByteStream = new ByteArrayInputStream(racBuffer);
            return kByteStream;
        } catch ( FileNotFoundException e1 ) {} catch ( IOException e2 ) {}

        return null;
    }

    /** Image format mode. */
    protected FormatMode m_eFormat;
    /** Image format mode. */
    protected Type m_eType = Type.IT_QUANTITY;
    /** Image number of dimensions. */
    protected int m_iDimension;
    /** Image extents. */
    protected int[] m_aiBound = new int[3];
    /** Image size. */
    protected int m_iQuantity;
    /** Image byte data. */
    protected byte[] m_aucData = null;
    /** Image float data. */
    protected float[] m_afData = null;

    protected Buffer m_kBuffer = null;

    /** Converting from 0-255 range to 0-1 range. */
    private static final float fInv255 = 1.0f/255.0f;


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

        // native data
        int iFormat = rkStream.ReadInt();
        m_eFormat = ms_pkFormatModeMap.get(iFormat);
        m_iDimension = rkStream.ReadInt();
        m_aiBound[0] = rkStream.ReadInt();
        m_aiBound[1] = rkStream.ReadInt();
        m_aiBound[2] = rkStream.ReadInt();
        m_iQuantity = rkStream.ReadInt();
        int iBytes = m_eFormat.BytesPerPixel()*m_iQuantity;
        m_aucData = new byte[iBytes];
        rkStream.Read(iBytes,m_aucData);

        ImageCatalog.GetActive().Insert(this);
    }

    /**
     * Write this object and all it's children to the Stream.
     * @param rkStream, the Stream where the child objects are stored.
     */
    public void Save (Stream rkStream)
    {
        super.Save(rkStream);

        // native data
        rkStream.Write(m_eFormat.Value());
        rkStream.Write(m_iDimension);
        rkStream.Write(m_aiBound[0]);
        rkStream.Write(m_aiBound[1]);
        rkStream.Write(m_aiBound[2]);
        rkStream.Write(m_iQuantity);
        int iBytes = m_eFormat.BytesPerPixel()*m_iQuantity;
        rkStream.Write(iBytes,m_aucData);
    }

    /**
     * Returns the size of this object and it's children on disk for the
     * current StreamVersion parameter.
     * @param rkVersion, the current version of the Stream file being created.
     * @return the size of this object on disk.
     */
    public int GetDiskUsed (final StreamVersion rkVersion)
    {
        int iSize = super.GetDiskUsed(rkVersion) +
            Stream.SIZEOF_INT; //sizeof(int); // m_eFormat

        if (rkVersion.GreaterEqual(new StreamVersion(3,2)))
        {
            iSize +=
                Stream.SIZEOF_INT + //sizeof(m_iDimension) +
                Stream.SIZEOF_INT + //sizeof(m_aiBound[0]) +
                Stream.SIZEOF_INT + //sizeof(m_aiBound[1]) +
                Stream.SIZEOF_INT + //sizeof(m_aiBound[2]) +
                Stream.SIZEOF_INT; //sizeof(m_iQuantity);

            int iBytes = m_eFormat.BytesPerPixel()*m_iQuantity;
            iSize += iBytes*Stream.SIZEOF_BYTE;//sizeof(m_aucData[0]);
        }
        else
        {
            iSize +=
                Stream.SIZEOF_INT + //sizeof(m_aiBound[0]) +
                Stream.SIZEOF_INT + //sizeof(m_aiBound[1]) +
                Stream.SIZEOF_INT; //sizeof(m_iQuantity);

            iSize++;  // existence of data stored as Boolean flag
            if (m_aucData != null)
            {
                int iBytes = m_eFormat.BytesPerPixel() *
                    m_aiBound[0]*m_aiBound[1];
                iSize += iBytes*Stream.SIZEOF_BYTE;//sizeof(m_aucData[0]);
            }
        }

        return iSize;
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
        // strings
        pkTree.Append(StringTree.Format("GraphicsImage",GetName()));
        pkTree.Append(super.SaveStrings(null));
        pkTree.Append(StringTree.Format("format =", m_eFormat.FormatName()));
        pkTree.Append(StringTree.Format("dimension = ",m_iDimension));

        for (int i = 0; i < m_iDimension; i++)
        {
            String prefix = new String( "bound[" + i + "] =");
            pkTree.Append(StringTree.Format(prefix,m_aiBound[i]));
        }
        
        return pkTree;
    }
}
