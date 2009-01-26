package gov.nih.mipav.model.file;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.actions.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;

import java.util.*;

import javax.swing.*;


/**
 * AVI file writer and reader.
 *
 * @version  1.0 Dec 2000
 * @author   William Gandler, Matthew J. McAuliffe, Ph.D. The Microsoft Video 1 decoding was mostly derived from Mike
 *           Melanson's Open Source Video 1 decoder found at
 *           http://zebra.fh-weingarten.de/~maxi/html/mplayer-dev-eng/2001-11/txt00008.txt Installing codecs may be done
 *           by running the folowing 2 files in mipav/apps: jmf_2_1_1e-windows-i586.exe DivX412Codec.exe
 *           CVID and CYUV decoders were ported from the codecs written by Dr. Tim Ferguson at 
 *           http://www.csse.monash.edu.au/~timf/.
 *           The MJPEG decoder is derived from the FFmpeg code, with the main source being mjpegdec.c.  The
 *           FFmpeg MJPEG decoder is copyright in 2000, 2001 by Fabrice Bellard, in 2003 by Alex Beregszaszi, and
 *           in 2003-2004 by Michael Niedermayer.
 *           A good source of sample avi files is http://samples.mplayerhq.hu.
 */

public class FileAvi extends FileBase {
    // JPEG marker codes
    /* start of frame */
    private final int SOF0  = 0xc0;       /* baseline */
    private final int SOF1  = 0xc1;       /* extended sequential, huffman */
    private final int SOF2  = 0xc2;       /* progressive, huffman */
    private final int SOF3  = 0xc3;       /* lossless, huffman */
    private final int DHT   = 0xc4;       /* define huffman tables */

    private final int SOF5  = 0xc5;       /* differential sequential, huffman */
    private final int SOF6  = 0xc6;       /* differential progressive, huffman */
    private final int SOF7  = 0xc7;       /* differential lossless, huffman */
    private final int JPG   = 0xc8;       /* reserved for JPEG extension */
    private final int SOF9  = 0xc9;       /* extended sequential, arithmetic */
    private final int SOF10 = 0xca;       /* progressive, arithmetic */
    private final int SOF11 = 0xcb;       /* lossless, arithmetic */

    private final int SOF13 = 0xcd;       /* differential sequential, arithmetic */
    private final int SOF14 = 0xce;       /* differential progressive, arithmetic */
    private final int SOF15 = 0xcf;      /* differential lossless, arithmetic */
    /* restart with modulo 8 count "m" */
    private final int RST0  = 0xd0;
    private final int RST1  = 0xd1;
    private final int RST2  = 0xd2;
    private final int RST3  = 0xd3;
    private final int RST4  = 0xd4;
    private final int RST5  = 0xd5;
    private final int RST6  = 0xd6;
    private final int RST7  = 0xd7;
    private final int SOI   = 0xd8;       /* start of image */
    private final int EOI   = 0xd9;       /* end of image */
    private final int SOS   = 0xda;       /* start of scan */
    private final int DQT   = 0xdb;       /* define quantization tables */
    private final int DRI   = 0xdd;       /* define restart interval */
    private final int APP0  = 0xe0;
    private final int APP1  = 0xe1;
    private final int APP2  = 0xe2;
    private final int APP3  = 0xe3;
    private final int APP4  = 0xe4;
    private final int APP5  = 0xe5;
    private final int APP6  = 0xe6;
    private final int APP7  = 0xe7;
    private final int APP8  = 0xe8;
    private final int APP9  = 0xe9;
    private final int APP10 = 0xea;
    private final int APP11 = 0xeb;
    private final int APP12 = 0xec;
    private final int APP13 = 0xed;
    private final int APP14 = 0xee;
    private final int APP15 = 0xef;
    
    private final int JPG0  = 0xf0;
    private final int JPG1  = 0xf1;
    private final int JPG2  = 0xf2;
    private final int JPG3  = 0xf3;
    private final int JPG4  = 0xf4;
    private final int JPG5  = 0xf5;
    private final int JPG6  = 0xf6;
    private final int SOF48 = 0xf7;       ///< JPEG-LS
    private final int LSE   = 0xf8;       ///< JPEG-LS extension parameters
    private final int JPG9  = 0xf9;
    private final int JPG10 = 0xfa;
    private final int JPG11 = 0xfb;
    private final int JPG12 = 0xfc;
    private final int JPG13 = 0xfd;
    private final int COM   = 0xfe;       /* comment */
    
    private final int ff_zigzag_direct[] = new int[]{
            0,   1,  8, 16,  9,  2,  3, 10,
            17, 24, 32, 25, 18, 11,  4,  5,
            12, 19, 26, 33, 40, 48, 41, 34,
            27, 20, 13,  6,  7, 14, 21, 28,
            35, 42, 49, 56, 57, 50, 43, 36,
            29, 22, 15, 23, 30, 37, 44, 51,
            58, 59, 52, 45, 38, 31, 39, 46,
            53, 60, 61, 54, 47, 55, 62, 63
        };
    
    /* Set up the standard Huffman tables (cf. JPEG standard section K.3) */
    /* IMPORTANT: these are only valid for 8-bit data precision! */
    // 17 entries
    private final byte ff_mjpeg_bits_dc_luminance[] = new byte[]
    { /* 0-base */ 0, 0, 1, 5, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0 };
    // 12 entries
    private final byte ff_mjpeg_val_dc[] = new byte[]
    { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 };
    // 17 entries
    private final byte ff_mjpeg_bits_dc_chrominance[] = new byte[]
    { /* 0-base */ 0, 0, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0 };
    // 17 entries
    private final byte ff_mjpeg_bits_ac_luminance[] = new byte[]
    { /* 0-base */ 0, 0, 2, 1, 3, 3, 2, 4, 3, 5, 5, 4, 4, 0, 0, 1, 0x7d };
    private final byte ff_mjpeg_val_ac_luminance[] = new byte[]
    { 0x01, 0x02, 0x03, 0x00, 0x04, 0x11, 0x05, 0x12,
      0x21, 0x31, 0x41, 0x06, 0x13, 0x51, 0x61, 0x07,
      0x22, 0x71, 0x14, 0x32, (byte)0x81, (byte)0x91, (byte)0xa1, 0x08,
      0x23, 0x42, (byte)0xb1, (byte)0xc1, 0x15, 0x52, (byte)0xd1, (byte)0xf0,
      0x24, 0x33, 0x62, 0x72, (byte)0x82, 0x09, 0x0a, 0x16,
      0x17, 0x18, 0x19, 0x1a, 0x25, 0x26, 0x27, 0x28,
      0x29, 0x2a, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39,
      0x3a, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49,
      0x4a, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59,
      0x5a, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69,
      0x6a, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79,
      0x7a, (byte)0x83, (byte)0x84, (byte)0x85, (byte)0x86, (byte)0x87, (byte)0x88, (byte)0x89,
      (byte)0x8a, (byte)0x92, (byte)0x93, (byte)0x94, (byte)0x95, (byte)0x96, (byte)0x97, (byte)0x98,
      (byte)0x99, (byte)0x9a, (byte)0xa2, (byte)0xa3, (byte)0xa4, (byte)0xa5, (byte)0xa6, (byte)0xa7,
      (byte)0xa8, (byte)0xa9, (byte)0xaa, (byte)0xb2, (byte)0xb3, (byte)0xb4, (byte)0xb5, (byte)0xb6,
      (byte)0xb7, (byte)0xb8, (byte)0xb9, (byte)0xba, (byte)0xc2, (byte)0xc3, (byte)0xc4, (byte)0xc5,
      (byte)0xc6, (byte)0xc7, (byte)0xc8, (byte)0xc9, (byte)0xca, (byte)0xd2, (byte)0xd3, (byte)0xd4,
      (byte)0xd5, (byte)0xd6, (byte)0xd7, (byte)0xd8, (byte)0xd9, (byte)0xda, (byte)0xe1, (byte)0xe2,
      (byte)0xe3, (byte)0xe4, (byte)0xe5, (byte)0xe6, (byte)0xe7, (byte)0xe8, (byte)0xe9, (byte)0xea,
      (byte)0xf1, (byte)0xf2, (byte)0xf3, (byte)0xf4, (byte)0xf5, (byte)0xf6, (byte)0xf7, (byte)0xf8,
      (byte)0xf9, (byte)0xfa
    };

    // 17 entries
    private final byte ff_mjpeg_bits_ac_chrominance[] = new byte[]
    { /* 0-base */ 0, 0, 2, 1, 2, 4, 4, 3, 4, 7, 5, 4, 4, 0, 1, 2, 0x77 };

    private final byte ff_mjpeg_val_ac_chrominance[] = new byte[]
    { 0x00, 0x01, 0x02, 0x03, 0x11, 0x04, 0x05, 0x21,
      0x31, 0x06, 0x12, 0x41, 0x51, 0x07, 0x61, 0x71,
      0x13, 0x22, 0x32, (byte)0x81, 0x08, 0x14, 0x42, (byte)0x91,
      (byte)0xa1, (byte)0xb1, (byte)0xc1, 0x09, 0x23, 0x33, 0x52, (byte)0xf0,
      0x15, 0x62, 0x72, (byte)0xd1, 0x0a, 0x16, 0x24, 0x34,
      (byte)0xe1, 0x25, (byte)0xf1, 0x17, 0x18, 0x19, 0x1a, 0x26,
      0x27, 0x28, 0x29, 0x2a, 0x35, 0x36, 0x37, 0x38,
      0x39, 0x3a, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48,
      0x49, 0x4a, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58,
      0x59, 0x5a, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68,
      0x69, 0x6a, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78,
      0x79, 0x7a, (byte)0x82, (byte)0x83, (byte)0x84, (byte)0x85, (byte)0x86, (byte)0x87,
      (byte)0x88, (byte)0x89, (byte)0x8a, (byte)0x92, (byte)0x93, (byte)0x94, (byte)0x95, (byte)0x96,
      (byte)0x97, (byte)0x98, (byte)0x99, (byte)0x9a, (byte)0xa2, (byte)0xa3, (byte)0xa4, (byte)0xa5,
      (byte)0xa6, (byte)0xa7, (byte)0xa8, (byte)0xa9, (byte)0xaa, (byte)0xb2, (byte)0xb3, (byte)0xb4,
      (byte)0xb5, (byte)0xb6, (byte)0xb7, (byte)0xb8, (byte)0xb9, (byte)0xba, (byte)0xc2, (byte)0xc3,
      (byte)0xc4, (byte)0xc5, (byte)0xc6, (byte)(byte)0xc7, (byte)0xc8, (byte)0xc9, (byte)0xca, (byte)0xd2,
      (byte)0xd3, (byte)0xd4, (byte)0xd5, (byte)0xd6, (byte)0xd7, (byte)0xd8, (byte)0xd9, (byte)0xda,
      (byte)0xe2, (byte)0xe3, (byte)0xe4, (byte)0xe5, (byte)0xe6, (byte)0xe7, (byte)0xe8, (byte)0xe9,
      (byte)0xea, (byte)0xf2, (byte)0xf3, (byte)0xf4, (byte)0xf5, (byte)0xf6, (byte)0xf7, (byte)0xf8,
      (byte)0xf9, (byte)0xfa
    };
    
    private final int MAX_COMPONENTS = 4;
    
    /**
     * Required number of additionally allocated bytes at the end of the input bitstream for decoding.
     * This is mainly needed because some optimized bitstream readers read
     * 32 or 64 bit at once and could read over the end.<br>
     * Note: If the first 23 bits of the additional bytes are not 0, then damaged
     * MPEG bitstreams could cause overread and segfault.
     */
    private final int FF_INPUT_BUFFER_PADDING_SIZE = 8;
    
    /**
     * Pixel format. Notes:
     *
     * PIX_FMT_RGB32 is handled in an endian-specific manner. A RGBA
     * color is put together as:
     *  (A << 24) | (R << 16) | (G << 8) | B
     * This is stored as BGRA on little endian CPU architectures and ARGB on
     * big endian CPUs.
     *
     * When the pixel format is palettized RGB (PIX_FMT_PAL8), the palettized
     * image data is stored in AVFrame.data[0]. The palette is transported in
     * AVFrame.data[1] and, is 1024 bytes long (256 4-byte entries) and is
     * formatted the same as in PIX_FMT_RGB32 described above (i.e., it is
     * also endian-specific). Note also that the individual RGB palette
     * components stored in AVFrame.data[1] should be in the range 0..255.
     * This is important as many custom PAL8 video codecs that were designed
     * to run on the IBM VGA graphics adapter use 6-bit palette components.
     */
        private final int PIX_FMT_NONE= -1;
        private final int PIX_FMT_YUV420P = 0;   ///< Planar YUV 4:2:0, 12bpp, (1 Cr & Cb sample per 2x2 Y samples)
        private final int PIX_FMT_YUYV422 = 1;   ///< Packed YUV 4:2:2, 16bpp, Y0 Cb Y1 Cr
        private final int PIX_FMT_RGB24 = 2;     ///< Packed RGB 8:8:8, 24bpp, RGBRGB...
        private final int PIX_FMT_BGR24 = 3;     ///< Packed RGB 8:8:8, 24bpp, BGRBGR...
        private final int PIX_FMT_YUV422P = 4;   ///< Planar YUV 4:2:2, 16bpp, (1 Cr & Cb sample per 2x1 Y samples)
        private final int PIX_FMT_YUV444P = 5;  ///< Planar YUV 4:4:4, 24bpp, (1 Cr & Cb sample per 1x1 Y samples)
        private final int PIX_FMT_RGB32 = 6;    ///< Packed RGB 8:8:8, 32bpp, (msb)8A 8R 8G 8B(lsb), in cpu endianness
        private final int PIX_FMT_YUV410P = 7;   ///< Planar YUV 4:1:0,  9bpp, (1 Cr & Cb sample per 4x4 Y samples)
        private final int PIX_FMT_YUV411P = 8;   ///< Planar YUV 4:1:1, 12bpp, (1 Cr & Cb sample per 4x1 Y samples)
        private final int PIX_FMT_RGB565 = 9;    ///< Packed RGB 5:6:5, 16bpp, (msb)   5R 6G 5B(lsb), in cpu endianness
        private final int PIX_FMT_RGB555 = 10;    ///< Packed RGB 5:5:5, 16bpp, (msb)1A 5R 5G 5B(lsb), in cpu endianness most significant bit to 0
        private final int PIX_FMT_GRAY8 = 11;     ///<        Y        ,  8bpp
        private final int PIX_FMT_MONOWHITE = 12; ///<        Y        ,  1bpp, 0 is white, 1 is black
        private final int PIX_FMT_MONOBLACK = 13; ///<        Y        ,  1bpp, 0 is black, 1 is white
        private final int PIX_FMT_PAL8 = 14;      ///< 8 bit with PIX_FMT_RGB32 palette
        private final int PIX_FMT_YUVJ420P = 15;  ///< Planar YUV 4:2:0, 12bpp, full scale (jpeg)
        private final int PIX_FMT_YUVJ422P= 16;  ///< Planar YUV 4:2:2, 16bpp, full scale (jpeg)
        private final int PIX_FMT_YUVJ444P = 17;  ///< Planar YUV 4:4:4, 24bpp, full scale (jpeg)
        private final int PIX_FMT_XVMC_MPEG2_MC = 18;///< XVideo Motion Acceleration via common packet passing(xvmc_render.h)
        private final int PIX_FMT_XVMC_MPEG2_IDCT = 19;
        private final int PIX_FMT_UYVY422 = 20;   ///< Packed YUV 4:2:2, 16bpp, Cb Y0 Cr Y1
        private final int PIX_FMT_UYYVYY411 = 21; ///< Packed YUV 4:1:1, 12bpp, Cb Y0 Y1 Cr Y2 Y3
        private final int PIX_FMT_BGR32 = 22;     ///< Packed RGB 8:8:8, 32bpp, (msb)8A 8B 8G 8R(lsb), in cpu endianness
        private final int PIX_FMT_BGR565 = 23;    ///< Packed RGB 5:6:5, 16bpp, (msb)   5B 6G 5R(lsb), in cpu endianness
        private final int PIX_FMT_BGR555 = 24;    ///< Packed RGB 5:5:5, 16bpp, (msb)1A 5B 5G 5R(lsb), in cpu endianness most significant bit to 1
        private final int PIX_FMT_BGR8 = 25;      ///< Packed RGB 3:3:2,  8bpp, (msb)2B 3G 3R(lsb)
        private final int PIX_FMT_BGR4 = 26;      ///< Packed RGB 1:2:1,  4bpp, (msb)1B 2G 1R(lsb)
        private final int PIX_FMT_BGR4_BYTE = 27; ///< Packed RGB 1:2:1,  8bpp, (msb)1B 2G 1R(lsb)
        private final int PIX_FMT_RGB8 = 28;      ///< Packed RGB 3:3:2,  8bpp, (msb)2R 3G 3B(lsb)
        private final int PIX_FMT_RGB4 = 29;      ///< Packed RGB 1:2:1,  4bpp, (msb)1R 2G 1B(lsb)
        private final int PIX_FMT_RGB4_BYTE = 30; ///< Packed RGB 1:2:1,  8bpp, (msb)1R 2G 1B(lsb)
        private final int PIX_FMT_NV12 = 31;      ///< Planar YUV 4:2:0, 12bpp, 1 plane for Y and 1 for UV
        private final int PIX_FMT_NV21 = 32;      ///< as above, but U and V bytes are swapped

        private final int PIX_FMT_RGB32_1 = 33;   ///< Packed RGB 8:8:8, 32bpp, (msb)8R 8G 8B 8A(lsb), in cpu endianness
        private final int PIX_FMT_BGR32_1 = 34;   ///< Packed RGB 8:8:8, 32bpp, (msb)8B 8G 8R 8A(lsb), in cpu endianness

        private final int PIX_FMT_GRAY16BE = 35;  ///<        Y        , 16bpp, big-endian
        private final int PIX_FMT_GRAY16LE = 36;  ///<        Y        , 16bpp, little-endian
        private final int PIX_FMT_YUV440P = 37;  ///< Planar YUV 4:4:0 (1 Cr & Cb sample per 1x2 Y samples)
        private final int PIX_FMT_YUVJ440P = 38;  ///< Planar YUV 4:4:0 full scale (jpeg)
        private final int PIX_FMT_YUVA420P = 39;  ///< Planar YUV 4:2:0, 20bpp, (1 Cr & Cb sample per 2x2 Y & A samples)
        
        // frame type
        private final int FF_I_TYPE = 1; ///< Intra
        
        private final int MIN_CACHE_BITS = 25;
        
        private final long ROW0_MASK = 0xffff000000000000L;
        
        private final int W1 = 22725;  //cos(i*M_PI/16)*sqrt(2)*(1<<14) + 0.5
        private final int W2 = 21407;  //cos(i*M_PI/16)*sqrt(2)*(1<<14) + 0.5
        private final int W3 = 19266;  //cos(i*M_PI/16)*sqrt(2)*(1<<14) + 0.5
        private final int W4 = 16383;  //cos(i*M_PI/16)*sqrt(2)*(1<<14) + 0.5
        private final int W5 = 12873;  //cos(i*M_PI/16)*sqrt(2)*(1<<14) + 0.5
        private final int W6 = 8867;   //cos(i*M_PI/16)*sqrt(2)*(1<<14) + 0.5
        private final int W7 = 4520;   //cos(i*M_PI/16)*sqrt(2)*(1<<14) + 0.5
        private final int ROW_SHIFT = 11;
        private final int COL_SHIFT = 20; // 6
        
        private final int MAX_NEG_CROP = 1024;


    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean AVIF_ISINTERLEAVED;

    /** globals needed for read - set in readHeader, used in readImage. */
    private boolean AVIF_MUSTUSEINDEX;
    
    private boolean AVIF_HASINDEX;

    /** DOCUMENT ME! */
    private short bitCount;

    /** DOCUMENT ME! */
    private int blankFramesSent;

    /** DOCUMENT ME! */
    private byte[] bufferWrite;

    /**
     * 0 for RGB 24 bit per pixel uncompressed 1 for RLE 8 bit per pixel compressed 1296126531 for Microsoft video 1
     * compression.
     */
    private int compression = -1; //

    /** DOCUMENT ME! */
    private float compressionQuality = 0.80f;

    /** DOCUMENT ME! */
    private int dataFramesSent;

    /** globals needed for write. */
    private byte[] dataSignature;

    /** DOCUMENT ME! */
    private int[] dcLength = null;

    /** If true use Microsoft video 1 compression. */
    private boolean doMSVC = false;
    
    /** Cinepak (CVID) */
    private boolean doCVID = false;
    
    /** Creative YUV (CYUV) */
    private boolean doCYUV = false;
    
    private boolean doMJPEG = false;

    /** true for big-endian and false for little-endian. */
    private boolean endianess;

    /** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private String fileDir;

    /** DOCUMENT ME! */
    private FileInfoAvi fileInfo;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private int height;

    /** DOCUMENT ME! */
    private long idx1Position;

    /** DOCUMENT ME! */
    private ModelImage imageA;

    /** DOCUMENT ME! */
    private ModelImage imageB = null;

    /** DOCUMENT ME! */
    private float[] imageBufferA;

    /** DOCUMENT ME! */
    private float[] imageBufferB;

    /** DOCUMENT ME! */
    private float imageMinA;

    /** DOCUMENT ME! */
    private long indexPointer;

    /** Size of the index block in bytes. */
    private int indexSize;

    /** for saving within scripts. */
    private boolean isScript = false;

    /** DOCUMENT ME! */
    private int LIST2Size;

    /** DOCUMENT ME! */
    private int LIST2subchunkSize;

    /** DOCUMENT ME! */
    private ModelLUT LUTa;

    /** DOCUMENT ME! */
    private byte[] lutBuffer = null;

    /** DOCUMENT ME! */
    private int[] lutBufferRemapped = null;

    /** DOCUMENT ME! */
    private int microSecPerFrame = (int) ((1.0f / Preferences.getDefaultFrameRate()) * 1000000);
    
    private int scale;
    
    private int rate;

    /** DOCUMENT ME! */
    private long moviPosition;

    /** DOCUMENT ME! */
    private long moviSubchunkPosition;

    /** 2 for .mov, 3 for mjpeg, 4 for mp4v2. */
    private int newCompressionType = 0;

    /** DOCUMENT ME! */
    private ProgressBarInterface progressBar = null;

    /** DOCUMENT ME! */
    private boolean readQT = false;

    /** DOCUMENT ME! */
    private float remapConstA;

    /** DOCUMENT ME! */
    private long[] savedbLength;

    /** DOCUMENT ME! */
    private long saveFileSize; // location of file size in bytes not counting first 8 bytes

    /** DOCUMENT ME! */
    private long saveLIST2Size;

    /** DOCUMENT ME! */
    private long savemovi;

    /** DOCUMENT ME! */
    private int streams;

    /** DOCUMENT ME! */
    private int totalBlankFrames;

    /** These are used for writing the AVI frame by frame from the surface renderer. */
    private int totalDataFrames;

    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    /** DOCUMENT ME! */
    private boolean useNewCompression = false;

    /** DOCUMENT ME! */
    private int width;

    /** DOCUMENT ME! */
    private boolean writeQT = false;

    /** DOCUMENT ME! */
    private int xDim, yDim, zDim, tDim;

    /** DOCUMENT ME! */
    private int xPad;
    
    private String outputFileName;
    
    private float captureTime;
    
    private float skipTime;
    
    private File fileW;
    private RandomAccessFile raFileW;
    
    private int framesToCapture;
    
    private int framesToSkip;
    
    private boolean mjpegDecodeInit = false;
    
    //private int idct_permutation[] = null;
    
    private int scantable_permutated[] = null;
    //private int stInverse[] = null;
    private int stRasterEnd[] = null;
    private short quant_matrixes[][] = null;
    private int qscale[] = null;      ///< quantizer scale calculated from quant_matrixes
    private short vlcs[][][][] = null;
    private int vlcs_bits[][] = null;
    private int vlcs_table_size[][] = null;
    private int vlcs_table_allocated[][] = null;
    private boolean thpCodec = false;
    private boolean AMVCodec = false;
    private int last_dc[];
    // GetBitContext
    private byte gbc_buffer[];
    private int gbc_buffer_end;
    private int gbc_index;
    private int gbc_size_in_bits;
    private byte ff_cropTbl[];

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Avi reader/writer constructor.
     *
     * @param      fileName  File name.
     * @param      fileDir   File directory.
     *
     * @exception  IOException  if there is an error making the file
     */
    public FileAvi(String fileName, String fileDir) throws IOException {

        UI = ViewUserInterface.getReference();
        this.fileName = fileName;
        this.fileDir = fileDir;
    }
    
    public void setCaptureTime(float captureTime) {
        this.captureTime = captureTime;
    }
    
    public void setSkipTime(float skipTime) {
        this.skipTime = skipTime;
    }
    
    public void setOutputFileName(String outputFileName) {
        this.outputFileName = outputFileName;
    }
    
    public int getMicroSecPerFrame() {
        return microSecPerFrame;
    }
    
    // Rate/scale = samples/second
    public int getScale() {
        return scale;
    }
    
    public int getRate() {
        return rate;
    }
    
    public boolean getHasIndex() {
        return AVIF_HASINDEX;
    }
    
    public boolean getMustUseIndex() {
        return AVIF_MUSTUSEINDEX;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public void close() {

        try {
            raFile.close();
        } catch (IOException error) { }

    }

    /**
     * Prepares this class for cleanup.
     */
    public void finalize() {
        fileName = null;
        fileDir = null;
        UI = null;
        file = null;
        fileInfo = null;
        imageA = null;
        imageB = null;

        LUTa = null;
        imageBufferA = null;
        imageBufferB = null;
        lutBuffer = null;
        lutBufferRemapped = null;
        bufferWrite = null;
        dcLength = null;
        savedbLength = null;

        try {
            super.finalize();
        } catch (Throwable er) { }
    }

    /**
     * Returns fileDir.
     *
     * @return  fileDir
     */
    public String getFileDir() {
        return this.fileDir;
    }

    /**
     * Returns fileName.
     *
     * @return  filename
     */
    public String getFileName() {
        return this.fileName;
    }

    /**
     * Returns LUT if defined.
     *
     * @return  The LUT if defined, otherwise null.
     */
    public ModelLUT getModelLUT() {
        return LUTa;
    }

    /**
     * Reads the AVI file header and data.
     *
     * @param      one  if true only reads one slice per file
     *
     * @return     An ARGB image
     *
     * @exception  IOException  if there is an error reading the file
     */
    public ModelImage readImage(boolean one) throws IOException {
        int[] imgExtents;
        byte[] imgBuffer;
        byte[] imgBuffer2 = null;
        byte[] fileBuffer;
        int bufferSize;
        int x, y, z, k;
        int col1, col2, totalC;
        int totalDataArea;
        int remainingFileLength;
        int totalBytesRead;
        int dataLength;
        boolean dataFound;
        int moviOffset;
        int signature;
        int CHUNKtype;
        boolean haveMoviSubchunk = false;
        int subchunkDataArea = 0;
        int subchunkBytesRead = 0;
        int subchunkBlocksRead = 0;
        boolean chunkRead;
        long startPosition; // position to start reading data
        int actualFrames = 0; // number of frames with data found on first read thru.
        int indexBytesRead = 0;
        long firstDataSignature;
        boolean firstRun;
        byte[] uiclp = null;

        boolean wasCompressed = false;

        System.err.println("AVI readImage(" + one + ")");

        try {
            int compressionType = 0;

            // if file is QuickTime... we must transcode before reading the header
            // else if file is compressed (readHeader > 0) we must transcode, then
            // re-read the header with the newly created uncompressedRGB avi
            if (readQT || (readHeader() > 0)) {
                wasCompressed = true;

                String newFileName = fileName.substring(0, fileName.length() - 4) + "_RGB.avi";

                AlgorithmTranscode at = new AlgorithmTranscode(new File(fileDir + fileName).toURI().toURL(),
                                                               fileDir + newFileName, AlgorithmTranscode.TRANSCODE_RGB);

                //at.setQuality(compressionQuality);
                at.setQuality(1);
                at.run();

                // set the filename to the new file name and read header again
                this.fileName = newFileName;
                // The line 
                // oml = new MediaLocator(outputFile.toURI().toURL()); 
                // in AlgorithmTranscode.runAlgorithm() replaces every space in the file path name
                // with %20.
                for (int i = 0; i < fileDir.length(); i++) {
                    if (fileDir.charAt(i) == 0x20) {
                        fileDir = fileDir.substring(0,i) + "%20" + fileDir.substring(i+1);
                    }
                }

                if (readHeader() != 0) {
                	Preferences.debug("FileAVI.readImage: Something messed up - could not read avi image transcoded to RGB-AVI" + "\n", Preferences.DEBUG_FILEIO);
                    System.err.println("Something messed up!!!");
                }
            }

            startPosition = raFile.getFilePointer();
            // Do first read thru the data to find the actual number of frames used by MIPAV. This must be done before
            // the MIPAV image can be created.

            dataSignature = new byte[4];

            totalDataArea = LIST2Size - 4; // Subtract out 4 'movi' bytes

            // Have encountered LiST2Size > raFile.length(), an impossibility
            remainingFileLength = (int) (raFile.length() - startPosition);

            if (totalDataArea > remainingFileLength) {
                Preferences.debug("File appears to be truncated\n");
                Preferences.debug("totalDataArea = " + totalDataArea + " remainingFileLength = " + remainingFileLength + "\n");
                totalDataArea = remainingFileLength;
            }

            totalBytesRead = 0;

            // Check for LIST rec<sp> subchunks
            if (!AVIF_MUSTUSEINDEX) {
                signature = getInt(endianess);

                if (signature == 0x5453494C) {

                    // have read LIST
                    LIST2subchunkSize = getInt(endianess);
                    moviSubchunkPosition = raFile.getFilePointer();
                    CHUNKtype = getInt(endianess);

                    if (CHUNKtype == 0x20636572) {

                        // have read rec<sp>
                        haveMoviSubchunk = true;
                        Preferences.debug("LIST rec found\n", Preferences.DEBUG_FILEIO);
                        subchunkDataArea = LIST2subchunkSize - 4;
                        subchunkBytesRead = 0;
                        subchunkBlocksRead = 0;
                    } else {
                        raFile.close();
                        throw new IOException("CHUNK type in second LIST sub CHUNK is an illegal = " + CHUNKtype);
                    }
                } else {
                    raFile.seek(startPosition);
                }
            } // if (!AVIF_MUSTUSEINDEX)

            chunkRead = true;

            firstDataSignature = raFile.getFilePointer();
            firstRun = true;
            loop1:
            while (((!AVIF_MUSTUSEINDEX) && (totalBytesRead < totalDataArea) && chunkRead) ||
                       (AVIF_MUSTUSEINDEX && (indexBytesRead < indexSize))) {

                if (AVIF_MUSTUSEINDEX) {
                    raFile.seek(indexPointer);
                    dataFound = false;

                    while (!dataFound) {
                        raFile.read(dataSignature);

                        if ((dataSignature[2] != 0x64 /*d */) || (dataSignature[3] < 0x62 /* b */) ||
                                (dataSignature[3] > 0x63 /* c */)) {
                            indexPointer = indexPointer + 16;
                            indexBytesRead += 16;
                            if (indexBytesRead >= indexSize) {
                                break loop1;
                            }
                            raFile.seek(indexPointer);
                        } else {
                            dataFound = true;
                        }
                    } // while (!dataFound)

                    indexPointer = indexPointer + 8;
                    raFile.seek(indexPointer);
                    moviOffset = getInt(endianess);
                    if (firstRun && (moviOffset == firstDataSignature)) {
                        moviPosition = 0L;
                    }
                    indexPointer = indexPointer + 8;
                    indexBytesRead += 16;
                    raFile.seek(moviPosition + (long) moviOffset);
                    firstRun = false;
                } // if (AVIFMUSTINDEX)

                raFile.read(dataSignature);
                totalBytesRead = totalBytesRead + 4;
                subchunkBytesRead = subchunkBytesRead + 4;

                if ((dataSignature[2] == 0x64 /* d */) &&
                        ((dataSignature[3] == 0x63 /* c */) || (dataSignature[3] == 0x62 /* b */))) {
                    dataLength = getInt(endianess);

                    if ((dataLength % 2) == 1) {
                        dataLength++;
                    }

                    if (dataLength > 0) {
                        actualFrames++;
                    }

                    totalBytesRead = totalBytesRead + 4;
                    subchunkBytesRead += 4;

                    long ptr = raFile.getFilePointer();
                    raFile.seek(ptr + dataLength);
                    totalBytesRead = totalBytesRead + dataLength;
                    subchunkBytesRead += dataLength;
                } // else if ((dataSignature[2] == 0x64 /* d */) && (dataSignature[3] == 0x63 /* c */))
                else {
                    dataLength = getInt(endianess);

                    if ((dataLength % 2) == 1) {
                        dataLength++;
                    }

                    totalBytesRead = totalBytesRead + 4;
                    subchunkBytesRead += 4;

                    long ptr = raFile.getFilePointer();
                    raFile.seek(ptr + dataLength);
                    totalBytesRead = totalBytesRead + dataLength;
                    subchunkBytesRead += dataLength;
                } // else

                subchunkBlocksRead++;

                if (haveMoviSubchunk && (subchunkBlocksRead == streams) && (totalBytesRead < totalDataArea)) {
                    totalBytesRead += moviSubchunkPosition + LIST2subchunkSize - raFile.getFilePointer();
                    raFile.seek(moviSubchunkPosition + LIST2subchunkSize);

                    // Check for LIST rec<sp> subchunks
                    signature = getInt(endianess);
                    totalBytesRead += 4;

                    if (signature == 0x5453494C) {

                        // have read LIST
                        LIST2subchunkSize = getInt(endianess);
                        totalBytesRead += 4;
                        moviSubchunkPosition = raFile.getFilePointer();
                        CHUNKtype = getInt(endianess);

                        if (CHUNKtype == 0x20636572) {

                            // have read rec<sp>
                            totalBytesRead += 4;
                            subchunkDataArea = LIST2subchunkSize - 4;
                            subchunkBytesRead = 0;
                            subchunkBlocksRead = 0;
                        } else {
                            raFile.close();
                            throw new IOException("CHunktype for LIST2sbuchunk is an illegal = " + CHUNKtype);
                        }
                    } else {
                        chunkRead = false;
                    }
                } // if (haveMoviSubchunk && (subchunkBlocksRead == streams))
            } // while ((totalBytesRead < totalDataArea) && chunkRead)

            Preferences.debug("totalBytesRead = " + totalBytesRead + "\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("totalDataArea = " + totalDataArea + "\n", Preferences.DEBUG_FILEIO);
            indexPointer = idx1Position + 8;
            indexBytesRead = 0;

            if (actualFrames > 1) {
                imgExtents = new int[3];
                imgExtents[2] = actualFrames;
            } else {
                imgExtents = new int[2];
            }

            fileInfo.setNumFrames(actualFrames);
            imgExtents[0] = width;
            imgExtents[1] = height;
            fileInfo.setExtents(imgExtents);

            int[] moreExtents;

            if (one) {
                moreExtents = new int[2];
                moreExtents[0] = width;
                moreExtents[1] = height;
            } else {
                moreExtents = imgExtents;
            }

            if ((bitCount == 16) || (bitCount == 24) || (bitCount == 32) || doMSVC || doCVID) {
                fileInfo.setDataType(ModelStorageBase.ARGB);
                imageA = new ModelImage(ModelStorageBase.ARGB, moreExtents, fileName);
            } else if ((bitCount == 4) || (bitCount == 8)) {
                fileInfo.setDataType(ModelStorageBase.UBYTE);
                imageA = new ModelImage(ModelStorageBase.UBYTE, moreExtents, fileName);
            }
            if (doCVID) {
                uiclp = new byte[1024];
                for (k = 0; k < 512; k++) {
                    uiclp[k] = 0;
                }
                for (k = 512; k < 768; k++) {
                    uiclp[k] = (byte)(k - 512);
                }
                for (k = 768; k < 1024; k++) {
                    uiclp[k] = (byte)255;
                }
            }

            // Now that the image is created this second read thru actually imports the data into the image.
            raFile.seek(startPosition);

            int middleSlice = 0;

            if (imgExtents.length > 2) {
                middleSlice = imgExtents[2] / 2;
            }

            if (compression == 0) {

                if ((bitCount == 16) || (bitCount == 24) || (bitCount == 32)) {
                    bufferSize = 4 * imgExtents[0] * imgExtents[1];
                } else { // bitCount == 8
                    bufferSize = imgExtents[0] * imgExtents[1];
                }

                imgBuffer = new byte[bufferSize];
                dataSignature = new byte[4];

                totalDataArea = LIST2Size - 4; // Subtract out 4 'movi' bytes

                // Have encountered LiST2Size > raFile.length(), an impossibility
                remainingFileLength = (int) (raFile.length() - startPosition);

                if (totalDataArea > remainingFileLength) {
                    totalDataArea = remainingFileLength;
                }

                totalBytesRead = 0;

                // Check for LIST rec<sp> subchunks
                if (!AVIF_MUSTUSEINDEX) {
                    signature = getInt(endianess);

                    if (signature == 0x5453494C) {

                        // have read LIST
                        LIST2subchunkSize = getInt(endianess);
                        moviSubchunkPosition = raFile.getFilePointer();
                        CHUNKtype = getInt(endianess);

                        if (CHUNKtype == 0x20636572) {

                            // have read rec<sp>
                            haveMoviSubchunk = true;
                            subchunkDataArea = LIST2subchunkSize - 4;
                            subchunkBytesRead = 0;
                            subchunkBlocksRead = 0;
                        } else {
                            raFile.close();
                            throw new IOException("CHUNK type in second LIST sub CHUNK is an illegal = " + CHUNKtype);
                        }
                    } else {
                        raFile.seek(startPosition);
                    }
                } // if (1AVIF_MUSTUSEINDEX)

                x = 0;
                y = imgExtents[1] - 1;
                z = 0;
                chunkRead = true;

                loop2:
                while (((!AVIF_MUSTUSEINDEX) && (totalBytesRead < totalDataArea) && chunkRead) ||
                           (AVIF_MUSTUSEINDEX && (indexBytesRead < indexSize))) {

                    if (AVIF_MUSTUSEINDEX) {
                        raFile.seek(indexPointer);
                        dataFound = false;

                        while (!dataFound) {
                            raFile.read(dataSignature);

                            if ((dataSignature[2] != 0x64 /*d */) || (dataSignature[3] < 0x62 /* b */) ||
                                    (dataSignature[3] > 0x63 /* c */)) {
                                indexPointer = indexPointer + 16;
                                indexBytesRead += 16;
                                if (indexBytesRead >= indexSize) {
                                    break loop2;
                                }
                                raFile.seek(indexPointer);
                            } else {
                                dataFound = true;
                            }
                        } // while (!dataFound)

                        indexPointer = indexPointer + 8;
                        raFile.seek(indexPointer);
                        moviOffset = getInt(endianess);
                        indexPointer = indexPointer + 8;
                        indexBytesRead += 16;
                        raFile.seek(moviPosition + (long) moviOffset);
                    } // if (AVIFMUSTINDEX)

                    raFile.read(dataSignature);
                    totalBytesRead = totalBytesRead + 4;
                    subchunkBytesRead += 4;

                    if ((dataSignature[2] != 0x64 /*d */) || (dataSignature[3] < 0x62 /* b */) ||
                            (dataSignature[3] > 0x63 /* c */)) {
                        dataLength = getInt(endianess);

                        if ((dataLength % 2) == 1) {
                            dataLength++;
                        }

                        totalBytesRead = totalBytesRead + 4;
                        subchunkBytesRead += 4;

                        long ptr = raFile.getFilePointer();
                        raFile.seek(ptr + dataLength);
                        totalBytesRead = totalBytesRead + dataLength;
                        subchunkBytesRead += 4;
                    } else {
                        dataLength = getInt(endianess);

                        if ((dataLength % 2) == 1) {
                            dataLength++;
                        }

                        totalBytesRead = totalBytesRead + 4;
                        subchunkBytesRead += 4;

                        if ((totalBytesRead + dataLength) <= totalDataArea) {

                            if (one && (z != middleSlice)) {
                                long ptr = raFile.getFilePointer();
                                raFile.seek(ptr + dataLength);
                                totalBytesRead = totalBytesRead + dataLength;
                                subchunkBytesRead += dataLength;
                                z++;
                            } else {
                                fileBuffer = new byte[dataLength];
                                raFile.read(fileBuffer);
                                totalBytesRead = totalBytesRead + dataLength;
                                subchunkBytesRead += dataLength;

                                if (bitCount == 24) {

                                    for (int j = 0; j < dataLength; j = j + 3) {
                                        k = 4 * (x + (imgExtents[0] * y));
                                        imgBuffer[k] = (byte) 255;
                                        imgBuffer[k + 1] = fileBuffer[j + 2];
                                        imgBuffer[k + 2] = fileBuffer[j + 1];
                                        imgBuffer[k + 3] = fileBuffer[j];
                                        x++;

                                        if ((x == imgExtents[0]) && (y > 0)) {
                                            x = 0;
                                            y--;
                                        } else if ((x == imgExtents[0]) && (y == 0)) {
                                            x = 0;
                                            y = imgExtents[1] - 1;

                                            if (one) {
                                                imageA.importData(0, imgBuffer, false);
                                            } else {
                                                imageA.importData(z * bufferSize, imgBuffer, false);
                                            }

                                            fireProgressStateChanged(100 * z / (imgExtents[2] - 1));
                                            z++;
                                        } // else if ((x == imgExtents[0] && (y == 0))
                                    } // for (j = 0; j < datalength;j=j+3)
                                } // if (bitCount == 24)
                                else if (bitCount == 32) {

                                    for (int j = 0; j < dataLength; j = j + 4) {
                                        k = 4 * (x + (imgExtents[0] * y));
                                        imgBuffer[k] = (byte) 255;
                                        imgBuffer[k + 1] = fileBuffer[j + 2];
                                        imgBuffer[k + 2] = fileBuffer[j + 1];
                                        imgBuffer[k + 3] = fileBuffer[j];
                                        x++;

                                        if ((x == imgExtents[0]) && (y > 0)) {
                                            x = 0;
                                            y--;
                                        } else if ((x == imgExtents[0]) && (y == 0)) {
                                            x = 0;
                                            y = imgExtents[1] - 1;

                                            if (one) {
                                                imageA.importData(0, imgBuffer, false);
                                            } else {
                                                imageA.importData(z * bufferSize, imgBuffer, false);
                                            }

                                            fireProgressStateChanged(100 * z / (imgExtents[2] - 1));
                                            z++;
                                        } // else if ((x == imgExtents[0] && (y == 0))
                                    } // for (j = 0; j < datalength;j=j+4)
                                } // else if (bitCount == 32)
                                else if (bitCount == 16) {

                                    for (int j = 0; j < dataLength;) {
                                        k = 4 * (x + (imgExtents[0] * y));
                                        col1 = fileBuffer[j++] & 0xff;
                                        col2 = fileBuffer[j++] & 0xff;
                                        totalC = (col2 << 8) | col1;
                                        imgBuffer[k] = (byte) 255;
                                        imgBuffer[k + 1] = (byte) ((totalC >> 7) & 0xF8);
                                        imgBuffer[k + 2] = (byte) ((totalC >> 2) & 0xF8);
                                        imgBuffer[k + 3] = (byte) ((totalC << 3) & 0xF8);
                                        x++;

                                        if ((x == imgExtents[0]) && (y > 0)) {
                                            x = 0;
                                            y--;
                                        } else if ((x == imgExtents[0]) && (y == 0)) {
                                            x = 0;
                                            y = imgExtents[1] - 1;

                                            if (one) {
                                                imageA.importData(0, imgBuffer, false);
                                            } else {
                                                imageA.importData(z * bufferSize, imgBuffer, false);
                                            }

                                            fireProgressStateChanged(100 * z / (imgExtents[2] - 1));
                                            z++;
                                        } // else if ((x == imgExtents[0] && (y == 0))
                                    } // for (int j = 0; j < dataLength;)
                                } // else if (bitCount == 16)
                                else if (bitCount == 8) {

                                    // Rows are stored in multiples of 4
                                    // so extra bytes may appear at the end
                                    int xPad = 0;
                                    int xMod = imgExtents[0] % 4;

                                    if (xMod != 0) {
                                        xPad = 4 - xMod;
                                    }

                                    for (int j = 0; j < dataLength; j++) {
                                        k = x + (imgExtents[0] * y);
                                        imgBuffer[k] = fileBuffer[j];
                                        x++;

                                        if ((x == imgExtents[0]) && (y > 0)) {
                                            j = j + xPad;
                                            x = 0;
                                            y--;
                                        } else if ((x == imgExtents[0]) && (y == 0)) {
                                            j = j + xPad;
                                            x = 0;
                                            y = imgExtents[1] - 1;

                                            if (one) {
                                                imageA.importData(0, imgBuffer, false);
                                            } else {
                                                imageA.importData(z * bufferSize, imgBuffer, false);
                                            }

                                            fireProgressStateChanged(100 * z / (imgExtents[2] - 1));
                                            z++;
                                        } // else if ((x == imgExtents[0]) && (y == 0))
                                    } // for (j = 0; j < dataLength; j++)
                                } // else if (bitCount == 8)
                                else if (bitCount == 4) {

                                    // Rows are stored in multiples of 4
                                    // so extra bytes may appear at the end
                                    int xPad = 0;
                                    int rowBytes = imgExtents[0]/2 + imgExtents[0]%2;
                                    int xMod = rowBytes % 4;

                                    if (xMod != 0) {
                                        xPad = 4 - xMod;
                                    }

                                    for (int j = 0; j < dataLength; j++) {
                                        k = x + (imgExtents[0] * y);
                                        imgBuffer[k] = (byte)(fileBuffer[j] & 0x0f);
                                        x++;

                                        if ((x == imgExtents[0]) && (y > 0)) {
                                            j = j + xPad;
                                            x = 0;
                                            y--;
                                        } else if ((x == imgExtents[0]) && (y == 0)) {
                                            j = j + xPad;
                                            x = 0;
                                            y = imgExtents[1] - 1;

                                            if (one) {
                                                imageA.importData(0, imgBuffer, false);
                                            } else {
                                                imageA.importData(z * bufferSize, imgBuffer, false);
                                            }

                                            fireProgressStateChanged(100 * z / (imgExtents[2] - 1));
                                            z++;
                                        } // else if ((x == imgExtents[0]) && (y == 0))
                                        else {
                                            imgBuffer[k+1] = (byte)((fileBuffer[j] >> 4) & 0x0f);
                                            x++;
    
                                            if ((x == imgExtents[0]) && (y > 0)) {
                                                j = j + xPad;
                                                x = 0;
                                                y--;
                                            } else if ((x == imgExtents[0]) && (y == 0)) {
                                                j = j + xPad;
                                                x = 0;
                                                y = imgExtents[1] - 1;
    
                                                if (one) {
                                                    imageA.importData(0, imgBuffer, false);
                                                } else {
                                                    imageA.importData(z * bufferSize, imgBuffer, false);
                                                }
    
                                                fireProgressStateChanged(100 * z / (imgExtents[2] - 1));
                                                z++;
                                            } // else if ((x == imgExtents[0]) && (y == 0))
                                        } // else
                                    } // for (j = 0; j < dataLength; j++)
                                } // else if (bitCount == 4)
                            } // else
                        } // if ((totalBytesRead + dataLength) <= totalDataArea)
                    } // else

                    subchunkBlocksRead++;

                    if (haveMoviSubchunk && (subchunkBlocksRead == streams) && (totalBytesRead < totalDataArea)) {
                        totalBytesRead += moviSubchunkPosition + LIST2subchunkSize - raFile.getFilePointer();
                        raFile.seek(moviSubchunkPosition + LIST2subchunkSize);

                        // Check for LIST rec<sp> subchunks
                        signature = getInt(endianess);
                        totalBytesRead += 4;

                        if (signature == 0x5453494C) {

                            // have read LIST
                            LIST2subchunkSize = getInt(endianess);
                            totalBytesRead += 4;
                            moviSubchunkPosition = raFile.getFilePointer();
                            CHUNKtype = getInt(endianess);

                            if (CHUNKtype == 0x20636572) {

                                // have read rec<sp>
                                totalBytesRead += 4;
                                subchunkDataArea = LIST2subchunkSize - 4;
                                subchunkBytesRead = 0;
                                subchunkBlocksRead = 0;
                            } else {
                                raFile.close();
                                throw new IOException("CHunktype for LIST2sbuchunk is an illegal = " + CHUNKtype);
                            }
                        } else {
                            chunkRead = false;
                        }
                    } // if (haveMoviSubchunk && (subchunkBlocksRead == streams))
                } // while ((totalBytesRead < (totalDataArea-8)) && chunkRead)
            } // if (compression == 0)
            else if (compression == 1) {
                bufferSize = imgExtents[0] * imgExtents[1];
                imgBuffer = new byte[bufferSize];
                dataSignature = new byte[4];
                totalDataArea = LIST2Size - 4; // Subtract out 4 'movi' bytes
                totalBytesRead = 0;

                // Check for LIST rec<sp> subchunks
                if (AVIF_MUSTUSEINDEX) {
                    signature = getInt(endianess);

                    if (signature == 0x5453494C) {

                        // have read LIST
                        LIST2subchunkSize = getInt(endianess);
                        moviSubchunkPosition = raFile.getFilePointer();
                        CHUNKtype = getInt(endianess);

                        if (CHUNKtype == 0x20636572) {

                            // have read rec<sp>
                            haveMoviSubchunk = true;
                            subchunkDataArea = LIST2subchunkSize - 4;
                            subchunkBytesRead = 0;
                            subchunkBlocksRead = 0;
                        } else {
                            raFile.close();
                            throw new IOException("CHUNK type in second LIST sub CHUNK is an illegal = " + CHUNKtype);
                        }
                    } else {
                        raFile.seek(startPosition);
                    }
                } // if (!AVIF_MUSTUSEINDEX)

                x = 0;
                y = imgExtents[1] - 1;
                z = 0;
                chunkRead = true;

                loop3:
                while (((!AVIF_MUSTUSEINDEX) && (totalBytesRead < totalDataArea) && chunkRead) ||
                           (AVIF_MUSTUSEINDEX && (indexBytesRead < indexSize))) {

                    if (AVIF_MUSTUSEINDEX) {
                        raFile.seek(indexPointer);
                        dataFound = false;

                        while (!dataFound) {
                            raFile.read(dataSignature);

                            if ((dataSignature[2] != 0x64 /*d */) || (dataSignature[3] < 0x62 /* b */) ||
                                    (dataSignature[3] > 0x63 /* c */)) {
                                indexPointer = indexPointer + 16;
                                indexBytesRead += 16;
                                if (indexBytesRead >= indexSize) {
                                    break loop3;
                                }
                                raFile.seek(indexPointer);
                            } else {
                                dataFound = true;
                            }
                        } // while (!dataFound)

                        indexPointer = indexPointer + 8;
                        raFile.seek(indexPointer);
                        moviOffset = getInt(endianess);
                        indexPointer = indexPointer + 8;
                        indexBytesRead += 16;
                        raFile.seek(moviPosition + (long) moviOffset);
                    } // if (AVIFMUSTINDEX)

                    raFile.read(dataSignature);
                    totalBytesRead = totalBytesRead + 4;
                    subchunkBytesRead += 4;

                    if ((dataSignature[2] == 0x64 /* d */) &&
                            ((dataSignature[3] == 0x63 /* c */) || (dataSignature[3] == 0x62 /* b */))) {
                        dataLength = getInt(endianess);
                        totalBytesRead = totalBytesRead + 4;
                        subchunkBytesRead += 4;

                        if (one && (z != middleSlice)) {
                            long ptr = raFile.getFilePointer();
                            raFile.seek(ptr + dataLength);
                            totalBytesRead = totalBytesRead + dataLength;
                            subchunkBytesRead += dataLength;
                            z++;
                        } else {
                            fileBuffer = new byte[dataLength];
                            raFile.read(fileBuffer);
                            totalBytesRead = totalBytesRead + dataLength;
                            subchunkBytesRead += dataLength;

                            for (int j = 0; j < dataLength;) {

                                if (fileBuffer[j] != 0) {

                                    for (k = 0; k < (fileBuffer[j] & 0x000000ff); k++) {
                                        imgBuffer[x + (imgExtents[0] * y)] = fileBuffer[j + 1];
                                        x++;

                                        if ((x == imgExtents[0]) && (y > 0)) {
                                            x = 0;
                                            y--;
                                        } // if ((x == imgExtents[0]) && (y > 0))
                                        else if ((x == imgExtents[0]) && (y == 0)) {
                                            x = 0;
                                            y = imgExtents[1] - 1;
                                        } // else if ((x == imgExtents[0]) && (y == 0))
                                    } // for (k = 0; k < (fileBuffer[j] & 0x000000ff); k++)

                                    j = j + 2;
                                } // if (fileBuffer[j] != 0)
                                else if ((fileBuffer[j] == 0) && ((fileBuffer[j + 1] & 0x000000ff) > 2)) {

                                    for (k = 0; k < (fileBuffer[j + 1] & 0x000000ff); k++) {
                                        imgBuffer[x + (imgExtents[0] * y)] = fileBuffer[j + k + 2];
                                        x++;

                                        if ((x == imgExtents[0]) && (y > 0)) {
                                            x = 0;
                                            y--;
                                        } // if ((x == imgExtents[0]) && (y > 0))
                                        else if ((x == imgExtents[0]) && (y == 0)) {
                                            x = 0;
                                            y = imgExtents[1] - 1;
                                        } // else if ((x == imgExtents[0]) && (y == 0))
                                    } // for (k = 0; k < (fileBuffer[j+1] & 0x000000ff); k++)

                                    j = j + 2 + (fileBuffer[j + 1] & 0x000000ff) +
                                        ((fileBuffer[j + 1] & 0x000000ff) % 2);
                                } // else if ((fileBuffer[j] == 0) && ((fileBuffer[j+1] & 0x000000ff) > 2))
                                else if ((fileBuffer[j] == 0) && (fileBuffer[j + 1] == 2)) {
                                    x = x + (fileBuffer[j + 2] & 0x000000ff);
                                    y = y - (fileBuffer[j + 3] & 0x000000ff);
                                    j = j + 4;
                                } // else if ((fileBuffer[j] == 0) && (fileBuffer[j+1] == 2))
                                else if ((fileBuffer[j] == 0) && (fileBuffer[j + 1] == 0)) {

                                    // end of a line
                                    if (x != 0) {
                                        x = 0;

                                        if (y > 0) {
                                            y = y - 1;
                                        } else {
                                            y = imgExtents[1] - 1;
                                        }
                                    } // if (x != 0)

                                    j = j + 2;
                                } // else if ((fileBuffer[j] == 0) && (fileBuffer[j+1] == 0))
                                else if ((fileBuffer[j] == 0) && (fileBuffer[j + 1] == 1)) {

                                    // end of RLE bitmap
                                    x = 0;
                                    y = imgExtents[1] - 1;

                                    if (one) {
                                        imageA.importData(0, imgBuffer, false);
                                    } else {
                                        imageA.importData(z * bufferSize, imgBuffer, false);
                                    }

                                    if (actualFrames > 1) {
                                        fireProgressStateChanged(100 * z / (imgExtents[2] - 1));
                                    }

                                    z++;
                                    j = j + 2;
                                } // else if ((fileBuffer[j] == 0) && (fileBuffer[j+1] == 1))
                            } // for (j = 0; j < dataLength;)
                        }
                    } // else if ((dataSignature[2] == 0x64 /* d */) && (dataSignature[3] == 0x63 /* c */))
                    else {
                        dataLength = getInt(endianess);

                        if ((dataLength % 2) == 1) {
                            dataLength++;
                        }

                        totalBytesRead = totalBytesRead + 4;
                        subchunkBytesRead += 4;

                        long ptr = raFile.getFilePointer();
                        raFile.seek(ptr + dataLength);
                        totalBytesRead = totalBytesRead + dataLength;
                        subchunkBytesRead += 4;
                    } // else

                    subchunkBlocksRead++;

                    if (haveMoviSubchunk && (subchunkBlocksRead == streams) && (totalBytesRead < totalDataArea)) {
                        totalBytesRead += moviSubchunkPosition + LIST2subchunkSize - raFile.getFilePointer();
                        raFile.seek(moviSubchunkPosition + LIST2subchunkSize);

                        // Check for LIST rec<sp> subchunks
                        signature = getInt(endianess);
                        totalBytesRead += 4;

                        if (signature == 0x5453494C) {

                            // have read LIST
                            LIST2subchunkSize = getInt(endianess);
                            totalBytesRead += 4;
                            moviSubchunkPosition = raFile.getFilePointer();
                            CHUNKtype = getInt(endianess);

                            if (CHUNKtype == 0x20636572) {

                                // have read rec<sp>
                                totalBytesRead += 4;
                                subchunkDataArea = LIST2subchunkSize - 4;
                                subchunkBytesRead = 0;
                                subchunkBlocksRead = 0;
                            } else {
                                raFile.close();
                                throw new IOException("CHUNKtype for LIST2sbuchunk is an illegal = " + CHUNKtype);
                            }
                        } else {
                            chunkRead = false;
                        }
                    } // if (haveMoviSubchunk && (subchunkBlocksRead == streams))
                } // while ((totalBytesRead < totalDataArea) && chunkRead)

            } // else if (compression == 1)
            else if (doMSVC && (bitCount == 8)) {
                bufferSize = 4 * imgExtents[0] * imgExtents[1];
                imgBuffer = new byte[bufferSize];
                dataSignature = new byte[4];
                totalDataArea = LIST2Size - 4; // Subtract out 4 'movi' bytes
                totalBytesRead = 0;

                // Check for LIST rec<sp> subchunks
                if (!AVIF_MUSTUSEINDEX) {
                    signature = getInt(endianess);

                    if (signature == 0x5453494C) {

                        // have read LIST
                        LIST2subchunkSize = getInt(endianess);
                        moviSubchunkPosition = raFile.getFilePointer();
                        CHUNKtype = getInt(endianess);

                        if (CHUNKtype == 0x20636572) {

                            // have read rec<sp>
                            haveMoviSubchunk = true;
                            subchunkDataArea = LIST2subchunkSize - 4;
                            subchunkBytesRead = 0;
                            subchunkBlocksRead = 0;
                        } else {
                            raFile.close();
                            throw new IOException("CHUNK type in second LIST sub CHUNK is an illegal = " + CHUNKtype);
                        }
                    } else {
                        raFile.seek(startPosition);
                    }
                } // if (!AVIF_MUSTUSEINDEX)

                x = 0;
                y = imgExtents[1] - 1;
                z = 0;

                int blocksWide = imgExtents[0] / 4;
                int blocksHigh = imgExtents[1] / 4;
                int bytesPerPixel = 4;
                int blockInc = 4 * bytesPerPixel;
                int rowDec = (imgExtents[0] + 4) * bytesPerPixel;
                int blockIndex = 0;
                int pixelIndex = 0;
                int skipBlocks = 0;
                int blockX;
                int blockY;
                byte byteA;
                byte byteB;
                boolean bufferFinished = false;
                int flags;
                int[][] c1 = new int[2][2];
                int[][] c2 = new int[2][2];
                byte[][] c1r = new byte[2][2];
                byte[][] c1g = new byte[2][2];
                byte[][] c1b = new byte[2][2];
                byte[][] c2r = new byte[2][2];
                byte[][] c2g = new byte[2][2];
                byte[][] c2b = new byte[2][2];
                int pixelY;
                int pixelX;

                chunkRead = true;

                loop4:
                while (((!AVIF_MUSTUSEINDEX) && (totalBytesRead < totalDataArea) && chunkRead) ||
                           (AVIF_MUSTUSEINDEX && (indexBytesRead < indexSize))) {

                    if (AVIF_MUSTUSEINDEX) {
                        raFile.seek(indexPointer);
                        dataFound = false;

                        while (!dataFound) {
                            raFile.read(dataSignature);

                            if ((dataSignature[2] != 0x64 /*d */) || (dataSignature[3] < 0x62 /* b */) ||
                                    (dataSignature[3] > 0x63 /* c */)) {
                                indexPointer = indexPointer + 16;
                                indexBytesRead += 16;
                                if (indexBytesRead >= indexSize) {
                                    break loop4;
                                }
                                raFile.seek(indexPointer);
                            } else {
                                dataFound = true;
                            }
                        } // while (!dataFound)

                        indexPointer = indexPointer + 8;
                        raFile.seek(indexPointer);
                        moviOffset = getInt(endianess);
                        indexPointer = indexPointer + 8;
                        indexBytesRead += 16;
                        raFile.seek(moviPosition + (long) moviOffset);
                    } // if (AVIFMUSTINDEX)

                    raFile.read(dataSignature);
                    totalBytesRead = totalBytesRead + 4;
                    subchunkBytesRead += 4;

                    if ((dataSignature[2] == 0x64 /* d */) &&
                            ((dataSignature[3] == 0x63 /* c */) || (dataSignature[3] == 0x62 /* b */))) {
                        dataLength = getInt(endianess);

                        if ((dataLength % 2) == 1) {
                            dataLength++;
                        }

                        totalBytesRead = totalBytesRead + 4;
                        subchunkBytesRead += 4;

                        if (one && (z != middleSlice)) {
                            long ptr = raFile.getFilePointer();
                            raFile.seek(ptr + dataLength);
                            totalBytesRead = totalBytesRead + dataLength;
                            subchunkBytesRead += dataLength;
                            z++;
                        } else {
                            fileBuffer = new byte[dataLength];
                            raFile.read(fileBuffer);
                            totalBytesRead = totalBytesRead + dataLength;
                            subchunkBytesRead += dataLength;
                            bufferFinished = false;
                            blockIndex = 0;
                            pixelIndex = 0;
                            skipBlocks = 0;

                            for (int j = 0; (j < dataLength) && (!bufferFinished);) {

                                for (blockY = blocksHigh; (blockY > 0) && (!bufferFinished); blockY--) {
                                    blockIndex = ((4 * blockY) - 1) * (imgExtents[0] * bytesPerPixel);

                                    for (blockX = blocksWide; (blockX > 0) && (!bufferFinished); blockX--) {

                                        // Check if this block should be skipped
                                        if (skipBlocks > 0) {
                                            blockIndex += blockInc;
                                            skipBlocks--;

                                            continue;
                                        }

                                        pixelIndex = blockIndex;

                                        // get the next two bytes in the encoded data stream
                                        byteA = fileBuffer[j++];
                                        byteB = fileBuffer[j++];

                                        // Check if decode is finished
                                        if ((byteA == 0) && (byteB == 0)) {
                                            bufferFinished = true;

                                            if (one) {
                                                imageA.importData(0, imgBuffer, false);
                                            } else {
                                                imageA.importData(z * bufferSize, imgBuffer, false);
                                            }

                                            if (actualFrames > 1) {
                                                fireProgressStateChanged(100 * z / (imgExtents[2] - 1));
                                            }

                                            z++;
                                        }

                                        // Check if this is a skip code
                                        else if ((byteB & 0xFC) == 0x84) {

                                            // but don't count the current block
                                            skipBlocks = (((byteB & 0xff) - 0x84) << 8) + (byteA & 0xff) - 1;
                                        }

                                        // Check if this is a 2-color block
                                        else if ((byteB & 0xff) < 0x80) {
                                            flags = ((byteB & 0xff) << 8) | (byteA & 0xff);
                                            c1[0][0] = fileBuffer[j++] & 0xff;
                                            c2[0][0] = fileBuffer[j++] & 0xff;
                                            c1r[0][0] = lutBuffer[(4 * c1[0][0]) + 2];
                                            c1g[0][0] = lutBuffer[(4 * c1[0][0]) + 1];
                                            c1b[0][0] = lutBuffer[4 * c1[0][0]];
                                            c2r[0][0] = lutBuffer[(4 * c2[0][0]) + 2];
                                            c2g[0][0] = lutBuffer[(4 * c2[0][0]) + 1];
                                            c2b[0][0] = lutBuffer[4 * c2[0][0]];

                                            // 2-color encoding
                                            for (pixelY = 0; pixelY < 4; pixelY++) {

                                                for (pixelX = 0; pixelX < 4; pixelX++) {

                                                    if ((flags & 1) > 0) {
                                                        imgBuffer[pixelIndex++] = (byte) 255;
                                                        imgBuffer[pixelIndex++] = c1r[0][0];
                                                        imgBuffer[pixelIndex++] = c1g[0][0];
                                                        imgBuffer[pixelIndex++] = c1b[0][0];
                                                    } else {
                                                        imgBuffer[pixelIndex++] = (byte) 255;
                                                        imgBuffer[pixelIndex++] = c2r[0][0];
                                                        imgBuffer[pixelIndex++] = c2g[0][0];
                                                        imgBuffer[pixelIndex++] = c2b[0][0];
                                                    }

                                                    // get the next flag ready to go
                                                    flags >>= 1;
                                                } // for (pixelX = 0; pixelX < 4; pixelX++)

                                                pixelIndex -= rowDec;
                                            } // for (pixelY = 0; pixelY < 4; pixelY++)
                                        } // else if ((byteB & 0xff) < 0x80)

                                        // check if it's an 8-color block
                                        else if ((byteB & 0xff) >= 0x90) {

                                            // 8-color encoding
                                            flags = ((byteB & 0xff) << 8) | (byteA & 0xff);
                                            c1[0][0] = fileBuffer[j++] & 0xff;
                                            c2[0][0] = fileBuffer[j++] & 0xff;
                                            c1[1][0] = fileBuffer[j++] & 0xff;
                                            c2[1][0] = fileBuffer[j++] & 0xff;
                                            c1[0][1] = fileBuffer[j++] & 0xff;
                                            c2[0][1] = fileBuffer[j++] & 0xff;
                                            c1[1][1] = fileBuffer[j++] & 0xff;
                                            c2[1][1] = fileBuffer[j++] & 0xff;
                                            c1r[0][0] = lutBuffer[(4 * c1[0][0]) + 2];
                                            c1g[0][0] = lutBuffer[(4 * c1[0][0]) + 1];
                                            c1b[0][0] = lutBuffer[4 * c1[0][0]];
                                            c2r[0][0] = lutBuffer[(4 * c2[0][0]) + 2];
                                            c2g[0][0] = lutBuffer[(4 * c2[0][0]) + 1];
                                            c2b[0][0] = lutBuffer[4 * c2[0][0]];
                                            c1r[1][0] = lutBuffer[(4 * c1[1][0]) + 2];
                                            c1g[1][0] = lutBuffer[(4 * c1[1][0]) + 1];
                                            c1b[1][0] = lutBuffer[4 * c1[1][0]];
                                            c2r[1][0] = lutBuffer[(4 * c2[1][0]) + 2];
                                            c2g[1][0] = lutBuffer[(4 * c2[1][0]) + 1];
                                            c2b[1][0] = lutBuffer[4 * c2[1][0]];
                                            c1r[0][1] = lutBuffer[(4 * c1[0][1]) + 2];
                                            c1g[0][1] = lutBuffer[(4 * c1[0][1]) + 1];
                                            c1b[0][1] = lutBuffer[4 * c1[0][1]];
                                            c2r[0][1] = lutBuffer[(4 * c2[0][1]) + 2];
                                            c2g[0][1] = lutBuffer[(4 * c2[0][1]) + 1];
                                            c2b[0][1] = lutBuffer[4 * c2[0][1]];
                                            c1r[1][1] = lutBuffer[(4 * c1[1][1]) + 2];
                                            c1g[1][1] = lutBuffer[(4 * c1[1][1]) + 1];
                                            c1b[1][1] = lutBuffer[4 * c1[1][1]];
                                            c2r[1][1] = lutBuffer[(4 * c2[1][1]) + 2];
                                            c2g[1][1] = lutBuffer[(4 * c2[1][1]) + 1];
                                            c2b[1][1] = lutBuffer[4 * c2[1][1]];

                                            for (pixelY = 0; pixelY < 4; pixelY++) {

                                                for (pixelX = 0; pixelX < 4; pixelX++) {

                                                    if ((flags & 1) > 0) {
                                                        imgBuffer[pixelIndex++] = (byte) 255;
                                                        imgBuffer[pixelIndex++] = c1r[pixelX >> 1][pixelY >> 1];
                                                        imgBuffer[pixelIndex++] = c1g[pixelX >> 1][pixelY >> 1];
                                                        imgBuffer[pixelIndex++] = c1b[pixelX >> 1][pixelY >> 1];
                                                    } else {
                                                        imgBuffer[pixelIndex++] = (byte) 255;
                                                        imgBuffer[pixelIndex++] = c2r[pixelX >> 1][pixelY >> 1];
                                                        imgBuffer[pixelIndex++] = c2g[pixelX >> 1][pixelY >> 1];
                                                        imgBuffer[pixelIndex++] = c2b[pixelX >> 1][pixelY >> 1];
                                                    }

                                                    // get the next flag ready to go
                                                    flags >>= 1;
                                                } // for (pixelX = 0; pixelX < 4; pixelX++)

                                                pixelIndex -= rowDec;
                                            } // for (pixelY = 0; pixelY < 4; pixelY++) {
                                        } // else if ((byteB & 0xff) >= 0x90)
                                        else { // 1-color block
                                            c1[0][0] = byteA & 0xff;
                                            c1r[0][0] = lutBuffer[(4 * c1[0][0]) + 2];
                                            c1g[0][0] = lutBuffer[(4 * c1[0][0]) + 1];
                                            c1b[0][0] = lutBuffer[4 * c1[0][0]];

                                            for (pixelY = 0; pixelY < 4; pixelY++) {

                                                for (pixelX = 0; pixelX < 4; pixelX++) {
                                                    imgBuffer[pixelIndex++] = (byte) 255;
                                                    imgBuffer[pixelIndex++] = c1r[0][0];
                                                    imgBuffer[pixelIndex++] = c1g[0][0];
                                                    imgBuffer[pixelIndex++] = c1b[0][0];
                                                } // for (pixelX = 0; pixelX < 4; pixelX++)

                                                pixelIndex -= rowDec;
                                            } // for (pixelY = 0; pixelY < 4; pixelY++)
                                        } // else 1-color block

                                        blockIndex += blockInc;
                                    } // for (blockX = blocksWide; (blockX > 0) && (!bufferFinished); blockX--)
                                } // for (blockY = blocksHigh; (blockY > 0) && (!bufferFinished); blockY--)
                            } // for (int j = 0; (j < dataLength) && (!bufferFinished); )
                        }
                    } // else if ((dataSignature[2] == 0x64 /* d */) && (dataSignature[3] == 0x63 /* c */))
                    else {
                        dataLength = getInt(endianess);

                        if ((dataLength % 2) == 1) {
                            dataLength++;
                        }

                        totalBytesRead = totalBytesRead + 4;
                        subchunkBytesRead += 4;

                        long ptr = raFile.getFilePointer();
                        raFile.seek(ptr + dataLength);
                        totalBytesRead = totalBytesRead + dataLength;
                        subchunkBytesRead += dataLength;
                    } // else

                    subchunkBlocksRead++;

                    if (haveMoviSubchunk && (subchunkBlocksRead == streams) && (totalBytesRead < totalDataArea)) {
                        totalBytesRead += moviSubchunkPosition + LIST2subchunkSize - raFile.getFilePointer();
                        raFile.seek(moviSubchunkPosition + LIST2subchunkSize);

                        // Check for LIST rec<sp> subchunks
                        signature = getInt(endianess);
                        totalBytesRead += 4;

                        if (signature == 0x5453494C) {

                            // have read LIST
                            LIST2subchunkSize = getInt(endianess);
                            totalBytesRead += 4;
                            moviSubchunkPosition = raFile.getFilePointer();
                            CHUNKtype = getInt(endianess);

                            if (CHUNKtype == 0x20636572) {

                                // have read rec<sp>
                                totalBytesRead += 4;
                                subchunkDataArea = LIST2subchunkSize - 4;
                                subchunkBytesRead = 0;
                                subchunkBlocksRead = 0;
                            } else {
                                raFile.close();
                                throw new IOException("CHunktype for LIST2sbuchunk is an illegal = " + CHUNKtype);
                            }
                        } else {
                            chunkRead = false;
                        }
                    } // if (haveMoviSubchunk && (subchunkBlocksRead == streams))
                } // while ((totalBytesRead < totalDataArea) && chunkRead)
            } // else if (doMSVC && (bitCount == 8))
            else if (doMSVC && (bitCount == 16)) {
                bufferSize = 4 * imgExtents[0] * imgExtents[1];
                imgBuffer = new byte[bufferSize];
                dataSignature = new byte[4];
                totalDataArea = LIST2Size - 4; // Subtract out 4 'movi' bytes
                totalBytesRead = 0;

                // Check for LIST rec<sp> subchunks
                if (!AVIF_MUSTUSEINDEX) {
                    signature = getInt(endianess);

                    if (signature == 0x5453494C) {

                        // have read LIST
                        LIST2subchunkSize = getInt(endianess);
                        moviSubchunkPosition = raFile.getFilePointer();
                        CHUNKtype = getInt(endianess);

                        if (CHUNKtype == 0x20636572) {

                            // have read rec<sp>
                            haveMoviSubchunk = true;
                            subchunkDataArea = LIST2subchunkSize - 4;
                            subchunkBytesRead = 0;
                            subchunkBlocksRead = 0;
                        } else {
                            raFile.close();
                            throw new IOException("CHUNK type in second LIST sub CHUNK is an illegal = " + CHUNKtype);
                        }
                    } else {
                        raFile.seek(startPosition);
                    }
                } // if (!AVIF_MUSTUSEINDEX)

                x = 0;
                y = imgExtents[1] - 1;
                z = 0;

                int blocksWide = imgExtents[0] / 4;
                int blocksHigh = imgExtents[1] / 4;
                int bytesPerPixel = 4;
                int blockInc = 4 * bytesPerPixel;
                int rowDec = (imgExtents[0] + 4) * bytesPerPixel;
                int blockIndex = 0;
                int pixelIndex = 0;
                int skipBlocks = 0;
                int blockX;
                int blockY;
                byte byteA = 0;
                byte byteB = 0;
                int lowC;
                int highC;
                boolean bufferFinished = false;
                int flags;
                int[][] c1 = new int[2][2];
                int[][] c2 = new int[2][2];
                byte[][] c1r = new byte[2][2];
                byte[][] c1g = new byte[2][2];
                byte[][] c1b = new byte[2][2];
                byte[][] c2r = new byte[2][2];
                byte[][] c2g = new byte[2][2];
                byte[][] c2b = new byte[2][2];
                int pixelY;
                int pixelX;

                chunkRead = true;

                loop5:
                while (((!AVIF_MUSTUSEINDEX) && (totalBytesRead < totalDataArea) && chunkRead) ||
                           (AVIF_MUSTUSEINDEX && (indexBytesRead < indexSize))) {

                    if (AVIF_MUSTUSEINDEX) {
                        raFile.seek(indexPointer);
                        dataFound = false;

                        while (!dataFound) {
                            raFile.read(dataSignature);

                            if ((dataSignature[2] != 0x64 /*d */) || (dataSignature[3] < 0x62 /* b */) ||
                                    (dataSignature[3] > 0x63 /* c */)) {
                                indexPointer = indexPointer + 16;
                                indexBytesRead += 16;
                                if (indexBytesRead >= indexSize) {
                                    break loop5;
                                }
                                raFile.seek(indexPointer);
                            } else {
                                dataFound = true;
                            }
                        } // while (!dataFound)

                        indexPointer = indexPointer + 8;
                        raFile.seek(indexPointer);
                        moviOffset = getInt(endianess);
                        indexPointer = indexPointer + 8;
                        indexBytesRead += 16;
                        raFile.seek(moviPosition + (long) moviOffset);
                    } // if (AVIFMUSTINDEX)

                    raFile.read(dataSignature);
                    totalBytesRead = totalBytesRead + 4;
                    subchunkBytesRead += 4;

                    if ((dataSignature[2] == 0x64 /* d */) &&
                            ((dataSignature[3] == 0x63 /* c */) || (dataSignature[3] == 0x62 /* b */))) {
                        dataLength = getInt(endianess);

                        if ((dataLength % 2) == 1) {
                            dataLength++;
                        }

                        totalBytesRead = totalBytesRead + 4;
                        subchunkBytesRead += 4;

                        if (one && (z != middleSlice)) {
                            long ptr = raFile.getFilePointer();
                            raFile.seek(ptr + dataLength);
                            totalBytesRead = totalBytesRead + dataLength;
                            subchunkBytesRead += dataLength;
                            z++;
                        } else {
                            fileBuffer = new byte[dataLength];
                            raFile.read(fileBuffer);
                            totalBytesRead = totalBytesRead + dataLength;
                            subchunkBytesRead += dataLength;
                            bufferFinished = false;
                            blockIndex = 0;
                            pixelIndex = 0;
                            skipBlocks = 0;

                            for (int j = 0; (j < dataLength) && (!bufferFinished);) {

                                for (blockY = blocksHigh; (blockY > 0) && (!bufferFinished); blockY--) {
                                    blockIndex = ((4 * blockY) - 1) * (imgExtents[0] * bytesPerPixel);

                                    for (blockX = blocksWide; (blockX > 0) && (!bufferFinished); blockX--) {

                                        // Check if this block should be skipped
                                        if (skipBlocks > 0) {
                                            blockIndex += blockInc;
                                            skipBlocks--;

                                            continue;
                                        }

                                        pixelIndex = blockIndex;

                                        // get the next two bytes in the encoded data stream
                                        if (j < dataLength) {
                                            byteA = fileBuffer[j++];
                                        }

                                        if (j < dataLength) {
                                            byteB = fileBuffer[j++];
                                        }

                                        // Check if decode is finished
                                        // if ((byteA == 0) && (byteB == 0)) {
                                        if ((pixelIndex == bufferSize) || (j == dataLength)) {

                                            bufferFinished = true;

                                            if (one) {
                                                imageA.importData(0, imgBuffer, false);
                                            } else {
                                                imageA.importData(z * bufferSize, imgBuffer, false);
                                            }

                                            if (actualFrames > 1) {
                                                fireProgressStateChanged(100 * z / (imgExtents[2] - 1));
                                            }

                                            z++;
                                        }

                                        // Check if this is a skip code
                                        else if ((byteB & 0xFC) == 0x84) {

                                            // but don't count the current block
                                            skipBlocks = (((byteB & 0xff) - 0x84) << 8) + (byteA & 0xff) - 1;
                                        }

                                        // Check if this is in the 2- or 8-color classes
                                        else if ((byteB & 0xff) < 0x80) {
                                            flags = ((byteB & 0xff) << 8) | (byteA & 0xff);

                                            lowC = fileBuffer[j++] & 0xff;
                                            highC = fileBuffer[j++] & 0xff;
                                            c1[0][0] = (highC << 8) | lowC;
                                            lowC = fileBuffer[j++] & 0xff;
                                            highC = fileBuffer[j++] & 0xff;
                                            c2[0][0] = (highC << 8) | lowC;


                                            c1r[0][0] = (byte) ((c1[0][0] >> 7) & 0xF8);
                                            c1g[0][0] = (byte) ((c1[0][0] >> 2) & 0xF8);
                                            c1b[0][0] = (byte) ((c1[0][0] << 3) & 0xF8);
                                            c2r[0][0] = (byte) ((c2[0][0] >> 7) & 0xF8);
                                            c2g[0][0] = (byte) ((c2[0][0] >> 2) & 0xF8);
                                            c2b[0][0] = (byte) ((c2[0][0] << 3) & 0xF8);

                                            if ((c1[0][0] & 0x8000) != 0) {

                                                // 8-color encoding
                                                lowC = fileBuffer[j++] & 0xff;
                                                highC = fileBuffer[j++] & 0xff;
                                                c1[1][0] = (highC << 8) | lowC;
                                                lowC = fileBuffer[j++] & 0xff;
                                                highC = fileBuffer[j++] & 0xff;
                                                c2[1][0] = (highC << 8) | lowC;

                                                lowC = fileBuffer[j++] & 0xff;
                                                highC = fileBuffer[j++] & 0xff;
                                                c1[0][1] = (highC << 8) | lowC;
                                                lowC = fileBuffer[j++] & 0xff;
                                                highC = fileBuffer[j++] & 0xff;
                                                c2[0][1] = (highC << 8) | lowC;

                                                lowC = fileBuffer[j++] & 0xff;
                                                highC = fileBuffer[j++] & 0xff;
                                                c1[1][1] = (highC << 8) | lowC;
                                                lowC = fileBuffer[j++] & 0xff;
                                                highC = fileBuffer[j++] & 0xff;
                                                c2[1][1] = (highC << 8) | lowC;

                                                c1r[0][1] = (byte) ((c1[0][1] >> 7) & 0xF8);
                                                c1g[0][1] = (byte) ((c1[0][1] >> 2) & 0xF8);
                                                c1b[0][1] = (byte) ((c1[0][1] << 3) & 0xF8);
                                                c2r[0][1] = (byte) ((c2[0][1] >> 7) & 0xF8);
                                                c2g[0][1] = (byte) ((c2[0][1] >> 2) & 0xF8);
                                                c2b[0][1] = (byte) ((c2[0][1] << 3) & 0xF8);

                                                c1r[1][0] = (byte) ((c1[1][0] >> 7) & 0xF8);
                                                c1g[1][0] = (byte) ((c1[1][0] >> 2) & 0xF8);
                                                c1b[1][0] = (byte) ((c1[1][0] << 3) & 0xF8);
                                                c2r[1][0] = (byte) ((c2[1][0] >> 7) & 0xF8);
                                                c2g[1][0] = (byte) ((c2[1][0] >> 2) & 0xF8);
                                                c2b[1][0] = (byte) ((c2[1][0] << 3) & 0xF8);

                                                c1r[1][1] = (byte) ((c1[1][1] >> 7) & 0xF8);
                                                c1g[1][1] = (byte) ((c1[1][1] >> 2) & 0xF8);
                                                c1b[1][1] = (byte) ((c1[1][1] << 3) & 0xF8);
                                                c2r[1][1] = (byte) ((c2[1][1] >> 7) & 0xF8);
                                                c2g[1][1] = (byte) ((c2[1][1] >> 2) & 0xF8);
                                                c2b[1][1] = (byte) ((c2[1][1] << 3) & 0xF8);

                                                for (pixelY = 0; pixelY < 4; pixelY++) {

                                                    for (pixelX = 0; pixelX < 4; pixelX++) {

                                                        if ((flags & 1) > 0) {
                                                            imgBuffer[pixelIndex++] = (byte) 255;
                                                            imgBuffer[pixelIndex++] = c1r[pixelX >> 1][pixelY >> 1];
                                                            imgBuffer[pixelIndex++] = c1g[pixelX >> 1][pixelY >> 1];
                                                            imgBuffer[pixelIndex++] = c1b[pixelX >> 1][pixelY >> 1];
                                                        } else {
                                                            imgBuffer[pixelIndex++] = (byte) 255;
                                                            imgBuffer[pixelIndex++] = c2r[pixelX >> 1][pixelY >> 1];
                                                            imgBuffer[pixelIndex++] = c2g[pixelX >> 1][pixelY >> 1];
                                                            imgBuffer[pixelIndex++] = c2b[pixelX >> 1][pixelY >> 1];
                                                        }

                                                        // get the next flag ready to go
                                                        flags >>= 1;
                                                    } // for (pixelX = 0; pixelX < 4; pixelX++)

                                                    pixelIndex -= rowDec;
                                                } // for (pixelY = 0; pixelY < 4; pixelY++) {
                                            } // if ((c1[0][0] & 0x8000) != 0)
                                            else { // 2-color encoding

                                                for (pixelY = 0; pixelY < 4; pixelY++) {

                                                    for (pixelX = 0; pixelX < 4; pixelX++) {

                                                        if ((flags & 1) > 0) {
                                                            imgBuffer[pixelIndex++] = (byte) 255;
                                                            imgBuffer[pixelIndex++] = c1r[0][0];
                                                            imgBuffer[pixelIndex++] = c1g[0][0];
                                                            imgBuffer[pixelIndex++] = c1b[0][0];
                                                        } else {
                                                            imgBuffer[pixelIndex++] = (byte) 255;
                                                            imgBuffer[pixelIndex++] = c2r[0][0];
                                                            imgBuffer[pixelIndex++] = c2g[0][0];
                                                            imgBuffer[pixelIndex++] = c2b[0][0];
                                                        }

                                                        // get the next flag ready to go
                                                        flags >>= 1;
                                                    } // for (pixelX = 0; pixelX < 4; pixelX++)

                                                    pixelIndex -= rowDec;
                                                } // for (pixelY = 0; pixelY < 4; pixelY++)
                                            } // else 2-color encoding
                                        } // else if ((byteB & 0xff) < 0x80)
                                        else { // 1-color block
                                            c1[0][0] = ((byteB & 0xff) << 8) | (byteA & 0xff);
                                            c1r[0][0] = (byte) ((c1[0][0] >> 7) & 0xF8);
                                            c1g[0][0] = (byte) ((c1[0][0] >> 2) & 0xF8);
                                            c1b[0][0] = (byte) ((c1[0][0] << 3) & 0xF8);

                                            for (pixelY = 0; pixelY < 4; pixelY++) {

                                                for (pixelX = 0; pixelX < 4; pixelX++) {
                                                    imgBuffer[pixelIndex++] = (byte) 255;
                                                    imgBuffer[pixelIndex++] = c1r[0][0];
                                                    imgBuffer[pixelIndex++] = c1g[0][0];
                                                    imgBuffer[pixelIndex++] = c1b[0][0];
                                                } // for (pixelX = 0; pixelX < 4; pixelX++)

                                                pixelIndex -= rowDec;
                                            } // for (pixelY = 0; pixelY < 4; pixelY++)
                                        } // else 1-color block

                                        blockIndex += blockInc;
                                    } // for (blockX = blocksWide; (blockX > 0) && (!bufferFinished); blockX--)
                                } // for (blockY = blocksHigh; (blockY > 0) && (!bufferFinished); blockY--)
                            } // for (int j = 0; (j < dataLength) && (!bufferFinished); )
                        }
                    } // else if ((dataSignature[2] == 0x64 /* d */) && (dataSignature[3] == 0x63 /* c */))
                    else {
                        dataLength = getInt(endianess);

                        if ((dataLength % 2) == 1) {
                            dataLength++;
                        }

                        totalBytesRead = totalBytesRead + 4;
                        subchunkBytesRead += 4;

                        long ptr = raFile.getFilePointer();
                        raFile.seek(ptr + dataLength);
                        totalBytesRead = totalBytesRead + dataLength;
                        subchunkBytesRead += dataLength;
                    } // else

                    subchunkBlocksRead++;

                    if (haveMoviSubchunk && (subchunkBlocksRead == streams) && (totalBytesRead < totalDataArea)) {
                        totalBytesRead += moviSubchunkPosition + LIST2subchunkSize - raFile.getFilePointer();
                        raFile.seek(moviSubchunkPosition + LIST2subchunkSize);

                        // Check for LIST rec<sp> subchunks
                        signature = getInt(endianess);
                        totalBytesRead += 4;

                        if (signature == 0x5453494C) {

                            // have read LIST
                            LIST2subchunkSize = getInt(endianess);
                            totalBytesRead += 4;
                            moviSubchunkPosition = raFile.getFilePointer();
                            CHUNKtype = getInt(endianess);

                            if (CHUNKtype == 0x20636572) {

                                // have read rec<sp>
                                totalBytesRead += 4;
                                subchunkDataArea = LIST2subchunkSize - 4;
                                subchunkBytesRead = 0;
                                subchunkBlocksRead = 0;
                            } else {
                                raFile.close();
                                throw new IOException("CHunktype for LIST2sbuchunk is an illegal = " + CHUNKtype);
                            }
                        } else {
                            chunkRead = false;
                        }
                    } // if (haveMoviSubchunk && (subchunkBlocksRead == streams))
                } // while ((totalBytesRead < totalDataArea) && chunkRead)
            } // else if (doMSVC && (bitCount == 16))
            else if (doCVID && (bitCount == 24)) {
                int y_bottom;
                byte frame_flags;
                int len;
                int i;
                int j;
                int c;
                int cv_width;
                int cv_height;
                int strips;
                int oldStrips = -1;
                // strips, 260
                byte v4y0[][] = null;
                byte v4y1[][] = null;
                byte v4y2[][] = null;
                byte v4y3[][] = null;
                byte v4u[][] = null;
                byte v4v[][] = null;
                // strips, 260, 4
                byte v4r[][][] = null;
                byte v4g[][][] = null;
                byte v4b[][][] = null;
                // strips, 260
                byte v1y0[][] = null;
                byte v1y1[][] = null;
                byte v1y2[][] = null;
                byte v1y3[][] = null;
                byte v1u[][] = null;
                byte v1v[][] = null;
                // strips, 260, 4
                byte v1r[][][] = null;
                byte v1g[][][] = null;
                byte v1b[][][] = null;
                int cur_strip;
                int strip_id;
                int top_size;
                int x0;
                int x1;
                int y0;
                int y1;
                int chunk_id;
                int chunk_size;
                int cnum;
                int uvr;
                int uvg;
                int uvb;
                int ci;
                int flag;
                int d0;
                int d1;
                int d2;
                int d3;
                int row_inc = 4 * (imgExtents[0] - 4);
                int pixelIndex;
                int index;
                int mask;
                boolean sizeChanged = false;
                bufferSize = 4 * imgExtents[0] * imgExtents[1];
                imgBuffer = new byte[bufferSize];
                dataSignature = new byte[4];
                totalDataArea = LIST2Size - 4; // Subtract out 4 'movi' bytes
                totalBytesRead = 0;

                // Have encountered LiST2Size > raFile.length(), an impossibility
                remainingFileLength = (int) (raFile.length() - startPosition);

                if (totalDataArea > remainingFileLength) {
                    totalDataArea = remainingFileLength;
                }

                totalBytesRead = 0;

                // Check for LIST rec<sp> subchunks
                if (!AVIF_MUSTUSEINDEX) {
                    signature = getInt(endianess);

                    if (signature == 0x5453494C) {

                        // have read LIST
                        LIST2subchunkSize = getInt(endianess);
                        moviSubchunkPosition = raFile.getFilePointer();
                        CHUNKtype = getInt(endianess);

                        if (CHUNKtype == 0x20636572) {

                            // have read rec<sp>
                            haveMoviSubchunk = true;
                            subchunkDataArea = LIST2subchunkSize - 4;
                            subchunkBytesRead = 0;
                            subchunkBlocksRead = 0;
                        } else {
                            raFile.close();
                            throw new IOException("CHUNK type in second LIST sub CHUNK is an illegal = " + CHUNKtype);
                        }
                    } else {
                        raFile.seek(startPosition);
                    }
                } // if (!AVIF_MUSTUSEINDEX)

                x = 0;
                y = 0;
                z = 0;
                y_bottom = 0;
                chunkRead = true;
                pixelIndex = 0;

                loop6:
                while (((!AVIF_MUSTUSEINDEX) && (totalBytesRead < totalDataArea) && chunkRead) ||
                           (AVIF_MUSTUSEINDEX && (indexBytesRead < indexSize))) {

                    if (AVIF_MUSTUSEINDEX) {
                        raFile.seek(indexPointer);
                        dataFound = false;

                        while (!dataFound) {
                            raFile.read(dataSignature);

                            if ((dataSignature[2] != 0x64 /*d */) || (dataSignature[3] < 0x62 /* b */) ||
                                    (dataSignature[3] > 0x63 /* c */)) {
                                indexPointer = indexPointer + 16;
                                indexBytesRead += 16;
                                if (indexBytesRead >= indexSize) {
                                    break loop6;
                                }
                                raFile.seek(indexPointer);
                            } else {
                                dataFound = true;
                            }
                        } // while (!dataFound)

                        indexPointer = indexPointer + 8;
                        raFile.seek(indexPointer);
                        moviOffset = getInt(endianess);
                        indexPointer = indexPointer + 8;
                        indexBytesRead += 16;
                        raFile.seek(moviPosition + (long) moviOffset);
                    } // if (AVIFMUSTINDEX)

                    raFile.read(dataSignature);
                    totalBytesRead = totalBytesRead + 4;
                    subchunkBytesRead += 4;

                    if ((dataSignature[2] != 0x64 /*d */) || (dataSignature[3] < 0x62 /* b */) ||
                            (dataSignature[3] > 0x63 /* c */)) {
                        dataLength = getInt(endianess);

                        if ((dataLength % 2) == 1) {
                            dataLength++;
                        }

                        totalBytesRead = totalBytesRead + 4;
                        subchunkBytesRead += 4;

                        long ptr = raFile.getFilePointer();
                        raFile.seek(ptr + dataLength);
                        totalBytesRead = totalBytesRead + dataLength;
                        subchunkBytesRead += 4;
                    } else {
                        
                        dataLength = getInt(endianess);
                        Preferences.debug("dataLength = " + dataLength + "\n");

                        if ((dataLength % 2) == 1) {
                            dataLength++;
                        }

                        totalBytesRead = totalBytesRead + 4;
                        subchunkBytesRead += 4;

                        if ((dataLength > 0) && ((totalBytesRead + dataLength) <= totalDataArea)) {

                            if (one && (z != middleSlice)) {
                                long ptr = raFile.getFilePointer();
                                raFile.seek(ptr + dataLength);
                                totalBytesRead = totalBytesRead + dataLength;
                                subchunkBytesRead += dataLength;
                                z++;
                            } else {
                                fileBuffer = new byte[dataLength];
                                raFile.read(fileBuffer);
                                totalBytesRead = totalBytesRead + dataLength;
                                subchunkBytesRead += dataLength;

                                j = 0;
                                x = 0;
                                y = 0;
                                y_bottom = 0;
                                frame_flags = fileBuffer[j++];
                                len = (fileBuffer[j++] & 0xff) << 16;
                                len |= (fileBuffer[j++] & 0xff) << 8;
                                len |= (fileBuffer[j++] & 0xff);
                                if ((len % 2) == 1) {
                                    len++;
                                }
                                Preferences.debug("Length of CVID data = " + len + "\n");
                                cv_width = (fileBuffer[j++] & 0xff) << 8;
                                cv_width |= (fileBuffer[j++] & 0xff);
                                Preferences.debug("Width of coded frame = " + cv_width + "\n");
                                cv_height = (fileBuffer[j++] & 0xff) << 8;
                                cv_height |= (fileBuffer[j++] & 0xff);
                                Preferences.debug("Height of coded frame = " + cv_height + "\n");
                                strips = (fileBuffer[j++] & 0xff) << 8;
                                strips |= (fileBuffer[j++] & 0xff);
                                Preferences.debug("Number of coded strips = " + strips + "\n");
                                
                                if (oldStrips != strips) {
                                    oldStrips = strips;
                                    v4y0 = new byte[strips][260];
                                    v4y1 = new byte[strips][260];
                                    v4y2 = new byte[strips][260];
                                    v4y3 = new byte[strips][260];
                                    v4u = new byte[strips][260];
                                    v4v = new byte[strips][260];
                                    v4r = new byte[strips][260][4];
                                    v4g = new byte[strips][260][4];
                                    v4b = new byte[strips][260][4];
                                    v1y0 = new byte[strips][260];
                                    v1y1 = new byte[strips][260];
                                    v1y2 = new byte[strips][260];
                                    v1y3 = new byte[strips][260];
                                    v1u = new byte[strips][260];
                                    v1v = new byte[strips][260];
                                    v1r = new byte[strips][260][4];
                                    v1g = new byte[strips][260][4];
                                    v1b = new byte[strips][260][4];
                                } // if (oldStrips != strips)
                                for (cur_strip = 0; cur_strip < strips; cur_strip++) {
                                    if ((cur_strip > 0) && ((frame_flags & 0x01) == 0)) {
                                      for (i = 0; i < 260; i++) {
                                          v4y0[cur_strip][i] = v4y0[cur_strip-1][i];
                                          v4y1[cur_strip][i] = v4y1[cur_strip-1][i];
                                          v4y2[cur_strip][i] = v4y2[cur_strip-1][i];
                                          v4y3[cur_strip][i] = v4y3[cur_strip-1][i];
                                          v4u[cur_strip][i] = v4u[cur_strip-1][i];
                                          v4v[cur_strip][i] = v4v[cur_strip-1][i];
                                          v1y0[cur_strip][i] = v1y0[cur_strip-1][i];
                                          v1y1[cur_strip][i] = v1y1[cur_strip-1][i];
                                          v1y2[cur_strip][i] = v1y2[cur_strip-1][i];
                                          v1y3[cur_strip][i] = v1y3[cur_strip-1][i];
                                          v1u[cur_strip][i] = v1u[cur_strip-1][i];
                                          v1v[cur_strip][i] = v1v[cur_strip-1][i];
                                          for (k = 0; k < 4; k++) {
                                              v4r[cur_strip][i][k] = v4r[cur_strip-1][i][k]; 
                                              v4g[cur_strip][i][k] = v4g[cur_strip-1][i][k];
                                              v4b[cur_strip][i][k] = v4b[cur_strip-1][i][k];
                                              v1r[cur_strip][i][k] = v1r[cur_strip-1][i][k]; 
                                              v1g[cur_strip][i][k] = v1g[cur_strip-1][i][k];
                                              v1b[cur_strip][i][k] = v1b[cur_strip-1][i][k];
                                          }
                                      }
                                    } // if ((cur_strip > 0) && ((frame_flags & 0x01) == 0))
                                    /* 1000 = key strip, 1100 = iter strip */
                                    strip_id = (fileBuffer[j++] & 0xff) << 8;
                                    strip_id |= (fileBuffer[j++] & 0xff);
                                    top_size = (fileBuffer[j++] & 0xff) << 8;
                                    top_size |= (fileBuffer[j++] & 0xff);
                                    y0 = (fileBuffer[j++] & 0xff) << 8;
                                    y0 |= (fileBuffer[j++] & 0xff);
                                    x0 = (fileBuffer[j++] & 0xff) << 8;
                                    x0 |= (fileBuffer[j++] & 0xff);
                                    y1 = (fileBuffer[j++] & 0xff) << 8;
                                    y1 |= (fileBuffer[j++] & 0xff);
                                    x1 = (fileBuffer[j++] & 0xff) << 8;
                                    x1 |= (fileBuffer[j++] & 0xff);
                                    Preferences.debug("x0 = " + x0 + " x1 = " + x1 + " y0 = " + y0 + " y1 = " + y1 + "\n");
                                    
                                    y_bottom += y1;
                                    top_size -= 12;
                                    x = 0;
                                    if ((!sizeChanged) && ((cv_width != imgExtents[0]) || (cv_height != imgExtents[1]))) {
                                        sizeChanged = true;
                                        if (cv_width != imgExtents[0]) {
                                            Preferences.debug("cv_width = " + cv_width + " instead of " +
                                                              imgExtents[0] + "\n");
                                        }
                                        if (cv_height != imgExtents[1]) {
                                            Preferences.debug("cv_height = " + cv_height + " instead of " +
                                                              imgExtents[1] + "\n");
                                        }
                                        row_inc = 4 * (cv_width - 4);
                                        imgBuffer = new byte[4 * cv_width * cv_height];
                                        imgBuffer2 = new byte[4 * imgExtents[0] * imgExtents[1]];
                                    }
                                    while (top_size > 0) {
                                        chunk_id = (fileBuffer[j++] & 0xff) << 8;
                                        chunk_id |= (fileBuffer[j++] & 0xff);
                                        chunk_size = (fileBuffer[j++] & 0xff) << 8;
                                        chunk_size |= (fileBuffer[j++] & 0xff);
                                        Preferences.debug("chunk_id = " + chunk_id + " chunk_size = " + chunk_size + "\n");
                                        top_size -= chunk_size;
                                        chunk_size -= 4;
                                        
                                        switch(chunk_id) {
                                            /* Codebook entries */
                                            case 0x2000: // color
                                            case 0x2400: // black and white
                                                // List of blocks in 12 bit V4 codebook
                                                if (chunk_id == 0x2000) {
                                                    cnum = chunk_size/6;
                                                }
                                                else {
                                                    cnum = chunk_size/4;
                                                }
                                                for (i = 0; i < cnum; i++) {
                                                    v4y0[cur_strip][i] = fileBuffer[j++]; // luma
                                                    v4y1[cur_strip][i] = fileBuffer[j++];
                                                    v4y2[cur_strip][i] = fileBuffer[j++];
                                                    v4y3[cur_strip][i] = fileBuffer[j++];
                                                    if (chunk_id == 0x2400) {
                                                        v4u[cur_strip][i] = 0;
                                                        v4v[cur_strip][i] = 0;
                                                        
                                                        v4r[cur_strip][i][0] = v4y0[cur_strip][i];
                                                        v4g[cur_strip][i][0] = v4y0[cur_strip][i];
                                                        v4b[cur_strip][i][0] = v4y0[cur_strip][i];
                                                        v4r[cur_strip][i][1] = v4y1[cur_strip][i];
                                                        v4g[cur_strip][i][1] = v4y1[cur_strip][i];
                                                        v4b[cur_strip][i][1] = v4y1[cur_strip][i];
                                                        v4r[cur_strip][i][2] = v4y2[cur_strip][i];
                                                        v4g[cur_strip][i][2] = v4y2[cur_strip][i];
                                                        v4b[cur_strip][i][2] = v4y2[cur_strip][i];
                                                        v4r[cur_strip][i][3] = v4y3[cur_strip][i];
                                                        v4g[cur_strip][i][3] = v4y3[cur_strip][i];
                                                        v4b[cur_strip][i][3] = v4y3[cur_strip][i];
                                                    }
                                                    else {
                                                        v4u[cur_strip][i] = fileBuffer[j++]; // chroma
                                                        v4v[cur_strip][i] = fileBuffer[j++];
                                                        
                                                        uvr = v4v[cur_strip][i] << 1;
                                                        uvg = -((v4u[cur_strip][i] + 1) >> 1) - v4v[cur_strip][i];
                                                        uvb = v4u[cur_strip][i] << 1;
                                                        
                                                        v4r[cur_strip][i][0] = uiclp[(v4y0[cur_strip][i] & 0xff) +
                                                                                      uvr + 512];
                                                        v4g[cur_strip][i][0] = uiclp[(v4y0[cur_strip][i] & 0xff) +
                                                                                     uvg + 512];
                                                        v4b[cur_strip][i][0] = uiclp[(v4y0[cur_strip][i] & 0xff) +
                                                                                     uvb + 512];
                                                        v4r[cur_strip][i][1] = uiclp[(v4y1[cur_strip][i] & 0xff) +
                                                                                     uvr + 512];
                                                        v4g[cur_strip][i][1] = uiclp[(v4y1[cur_strip][i] & 0xff) +
                                                                                    uvg + 512];
                                                        v4b[cur_strip][i][1] = uiclp[(v4y1[cur_strip][i] & 0xff) +
                                                                                    uvb + 512];
                                                        v4r[cur_strip][i][2] = uiclp[(v4y2[cur_strip][i] & 0xff) +
                                                                                     uvr + 512];
                                                        v4g[cur_strip][i][2] = uiclp[(v4y2[cur_strip][i] & 0xff) +
                                                                                    uvg + 512];
                                                        v4b[cur_strip][i][2] = uiclp[(v4y2[cur_strip][i] & 0xff) +
                                                                                    uvb + 512];
                                                        v4r[cur_strip][i][3] = uiclp[(v4y3[cur_strip][i] & 0xff) +
                                                                                     uvr + 512];
                                                        v4g[cur_strip][i][3] = uiclp[(v4y3[cur_strip][i] & 0xff) +
                                                                                    uvg + 512];
                                                        v4b[cur_strip][i][3] = uiclp[(v4y3[cur_strip][i] & 0xff) +
                                                                                    uvb + 512];
                                                    }
                                                } // for (i = 0; i < cnum; i++)
                                                break;
                                            case 0x2200: // color
                                            case 0x2600: // black and white
                                                // List of blocks in 12 bit V1 codebook
                                                if (chunk_id == 0x2200) {
                                                    cnum = chunk_size/6;
                                                }
                                                else {
                                                    cnum = chunk_size/4;
                                                }
                                                for (i = 0; i < cnum; i++) {
                                                    v1y0[cur_strip][i] = fileBuffer[j++]; // luma
                                                    v1y1[cur_strip][i] = fileBuffer[j++];
                                                    v1y2[cur_strip][i] = fileBuffer[j++];
                                                    v1y3[cur_strip][i] = fileBuffer[j++];
                                                    if (chunk_id == 0x2600) {
                                                        v1u[cur_strip][i] = 0;
                                                        v1v[cur_strip][i] = 0;
                                                        
                                                        v1r[cur_strip][i][0] = v1y0[cur_strip][i];
                                                        v1g[cur_strip][i][0] = v1y0[cur_strip][i];
                                                        v1b[cur_strip][i][0] = v1y0[cur_strip][i];
                                                        v1r[cur_strip][i][1] = v1y1[cur_strip][i];
                                                        v1g[cur_strip][i][1] = v1y1[cur_strip][i];
                                                        v1b[cur_strip][i][1] = v1y1[cur_strip][i];
                                                        v1r[cur_strip][i][2] = v1y2[cur_strip][i];
                                                        v1g[cur_strip][i][2] = v1y2[cur_strip][i];
                                                        v1b[cur_strip][i][2] = v1y2[cur_strip][i];
                                                        v1r[cur_strip][i][3] = v1y3[cur_strip][i];
                                                        v1g[cur_strip][i][3] = v1y3[cur_strip][i];
                                                        v1b[cur_strip][i][3] = v1y3[cur_strip][i];    
                                                    }
                                                    else {
                                                        v1u[cur_strip][i] = fileBuffer[j++]; // chroma
                                                        v1v[cur_strip][i] = fileBuffer[j++];
                                                        
                                                        uvr = v1v[cur_strip][i] << 1;
                                                        uvg = -((v1u[cur_strip][i] + 1) >> 1) - v1v[cur_strip][i];
                                                        uvb = v1u[cur_strip][i] << 1;
                                                        
                                                        v1r[cur_strip][i][0] = uiclp[(v1y0[cur_strip][i] & 0xff) +
                                                                                      uvr + 512];
                                                        v1g[cur_strip][i][0] = uiclp[(v1y0[cur_strip][i] & 0xff) +
                                                                                     uvg + 512];
                                                        v1b[cur_strip][i][0] = uiclp[(v1y0[cur_strip][i] & 0xff) +
                                                                                     uvb + 512];
                                                        v1r[cur_strip][i][1] = uiclp[(v1y1[cur_strip][i] & 0xff) +
                                                                                     uvr + 512];
                                                        v1g[cur_strip][i][1] = uiclp[(v1y1[cur_strip][i] & 0xff) +
                                                                                    uvg + 512];
                                                        v1b[cur_strip][i][1] = uiclp[(v1y1[cur_strip][i] & 0xff) +
                                                                                    uvb + 512];
                                                        v1r[cur_strip][i][2] = uiclp[(v1y2[cur_strip][i] & 0xff) +
                                                                                     uvr + 512];
                                                        v1g[cur_strip][i][2] = uiclp[(v1y2[cur_strip][i] & 0xff) +
                                                                                    uvg + 512];
                                                        v1b[cur_strip][i][2] = uiclp[(v1y2[cur_strip][i] & 0xff) +
                                                                                    uvb + 512];
                                                        v1r[cur_strip][i][3] = uiclp[(v1y3[cur_strip][i] & 0xff) +
                                                                                     uvr + 512];
                                                        v1g[cur_strip][i][3] = uiclp[(v1y3[cur_strip][i] & 0xff) +
                                                                                    uvg + 512];
                                                        v1b[cur_strip][i][3] = uiclp[(v1y3[cur_strip][i] & 0xff) +
                                                                                    uvb + 512];
                                                    }
                                                } // for (i = 0; i < cnum; i++)
                                                break;
                                            case 0x2100: // color
                                            case 0x2500: // black and white
                                                // Selective list of blocks to update 12 bit V4 codebook
                                                ci = 0;
                                                while (chunk_size > 0) {
                                                    flag = (fileBuffer[j++] & 0xff) << 24;
                                                    flag |= (fileBuffer[j++] & 0xff) << 16;
                                                    flag |= (fileBuffer[j++] & 0xff) << 8;
                                                    flag |= (fileBuffer[j++] & 0xff);
                                                    chunk_size -= 4;
                                                    
                                                    for (i = 0; i < 32; i++) {
                                                        if ((flag & 0x80000000)!= 0) {
                                                            if (chunk_id == 0x2100) {
                                                                chunk_size -= 6;
                                                            }
                                                            else {
                                                                chunk_size -= 4;
                                                            }
                                                            v4y0[cur_strip][ci] = fileBuffer[j++]; // luma
                                                            v4y1[cur_strip][ci] = fileBuffer[j++];
                                                            v4y2[cur_strip][ci] = fileBuffer[j++];
                                                            v4y3[cur_strip][ci] = fileBuffer[j++];
                                                            if (chunk_id == 0x2500) {
                                                                v4u[cur_strip][ci] = 0;
                                                                v4v[cur_strip][ci] = 0;
                                                                
                                                                v4r[cur_strip][ci][0] = v4y0[cur_strip][ci];
                                                                v4g[cur_strip][ci][0] = v4y0[cur_strip][ci];
                                                                v4b[cur_strip][ci][0] = v4y0[cur_strip][ci];
                                                                v4r[cur_strip][ci][1] = v4y1[cur_strip][ci];
                                                                v4g[cur_strip][ci][1] = v4y1[cur_strip][ci];
                                                                v4b[cur_strip][ci][1] = v4y1[cur_strip][ci];
                                                                v4r[cur_strip][ci][2] = v4y2[cur_strip][ci];
                                                                v4g[cur_strip][ci][2] = v4y2[cur_strip][ci];
                                                                v4b[cur_strip][ci][2] = v4y2[cur_strip][ci];
                                                                v4r[cur_strip][ci][3] = v4y3[cur_strip][ci];
                                                                v4g[cur_strip][ci][3] = v4y3[cur_strip][ci];
                                                                v4b[cur_strip][ci][3] = v4y3[cur_strip][ci];    
                                                            }
                                                            else {
                                                                v4u[cur_strip][ci] = fileBuffer[j++]; // chroma
                                                                v4v[cur_strip][ci] = fileBuffer[j++];
                                                                
                                                                uvr = v4v[cur_strip][ci] << 1;
                                                                uvg = -((v4u[cur_strip][ci] + 1) >> 1) - v4v[cur_strip][ci];
                                                                uvb = v4u[cur_strip][ci] << 1;
                                                                
                                                                v4r[cur_strip][ci][0] = uiclp[(v4y0[cur_strip][ci] & 0xff) +
                                                                                              uvr + 512];
                                                                v4g[cur_strip][ci][0] = uiclp[(v4y0[cur_strip][ci] & 0xff) +
                                                                                             uvg + 512];
                                                                v4b[cur_strip][ci][0] = uiclp[(v4y0[cur_strip][ci] & 0xff) +
                                                                                             uvb + 512];
                                                                v4r[cur_strip][ci][1] = uiclp[(v4y1[cur_strip][ci] & 0xff) +
                                                                                             uvr + 512];
                                                                v4g[cur_strip][ci][1] = uiclp[(v4y1[cur_strip][ci] & 0xff) +
                                                                                            uvg + 512];
                                                                v4b[cur_strip][ci][1] = uiclp[(v4y1[cur_strip][ci] & 0xff) +
                                                                                            uvb + 512];
                                                                v4r[cur_strip][ci][2] = uiclp[(v4y2[cur_strip][ci] & 0xff) +
                                                                                             uvr + 512];
                                                                v4g[cur_strip][ci][2] = uiclp[(v4y2[cur_strip][ci] & 0xff) +
                                                                                            uvg + 512];
                                                                v4b[cur_strip][ci][2] = uiclp[(v4y2[cur_strip][ci] & 0xff) +
                                                                                            uvb + 512];
                                                                v4r[cur_strip][ci][3] = uiclp[(v4y3[cur_strip][ci] & 0xff) +
                                                                                             uvr + 512];
                                                                v4g[cur_strip][ci][3] = uiclp[(v4y3[cur_strip][ci] & 0xff) +
                                                                                            uvg + 512];
                                                                v4b[cur_strip][ci][3] = uiclp[(v4y3[cur_strip][ci] & 0xff) +
                                                                                            uvb + 512];
                                                            }
                                                        } // if ((flag & 0x80000000) != 0)
                                                        ci++;
                                                        flag <<= 1;
                                                    } // for (i = 0; i < 32; i++)
                                                } // while (chunk_size > 0)
                                                while (chunk_size > 0) {
                                                    // Skip byte
                                                    j++;
                                                    chunk_size--;
                                                }
                                                break;
                                            case 0x2300: // color
                                            case 0x2700: // black and white
                                                // Selective list of blocks to update 12 bit V1 codebook
                                                ci = 0;
                                                while (chunk_size > 0) {
                                                    flag = (fileBuffer[j++] & 0xff) << 24;
                                                    flag |= (fileBuffer[j++] & 0xff) << 16;
                                                    flag |= (fileBuffer[j++] & 0xff) << 8;
                                                    flag |= (fileBuffer[j++] & 0xff);
                                                    chunk_size -= 4;
                                                    
                                                    for (i = 0; i < 32; i++) {
                                                        if ((flag & 0x80000000)!= 0) {
                                                            if (chunk_id == 0x2300) {
                                                                chunk_size -= 6;
                                                            }
                                                            else {
                                                                chunk_size -= 4;
                                                            }
                                                            v1y0[cur_strip][ci] = fileBuffer[j++]; // luma
                                                            v1y1[cur_strip][ci] = fileBuffer[j++];
                                                            v1y2[cur_strip][ci] = fileBuffer[j++];
                                                            v1y3[cur_strip][ci] = fileBuffer[j++];
                                                            if (chunk_id == 0x2700) {
                                                                v1u[cur_strip][i] = 0;
                                                                v1v[cur_strip][i] = 0;
                                                                
                                                                v1r[cur_strip][ci][0] = v1y0[cur_strip][ci];
                                                                v1g[cur_strip][ci][0] = v1y0[cur_strip][ci];
                                                                v1b[cur_strip][ci][0] = v1y0[cur_strip][ci];
                                                                v1r[cur_strip][ci][1] = v1y1[cur_strip][ci];
                                                                v1g[cur_strip][ci][1] = v1y1[cur_strip][ci];
                                                                v1b[cur_strip][ci][1] = v1y1[cur_strip][ci];
                                                                v1r[cur_strip][ci][2] = v1y2[cur_strip][ci];
                                                                v1g[cur_strip][ci][2] = v1y2[cur_strip][ci];
                                                                v1b[cur_strip][ci][2] = v1y2[cur_strip][ci];
                                                                v1r[cur_strip][ci][3] = v1y3[cur_strip][ci];
                                                                v1g[cur_strip][ci][3] = v1y3[cur_strip][ci];
                                                                v1b[cur_strip][ci][3] = v1y3[cur_strip][ci];       
                                                            }
                                                            else {
                                                                v1u[cur_strip][ci] = fileBuffer[j++]; // chroma
                                                                v1v[cur_strip][ci] = fileBuffer[j++];
                                                                
                                                                uvr = v1v[cur_strip][ci] << 1;
                                                                uvg = -((v1u[cur_strip][ci] + 1) >> 1) - v1v[cur_strip][ci];
                                                                uvb = v1u[cur_strip][ci] << 1;
                                                                
                                                                v1r[cur_strip][ci][0] = uiclp[(v1y0[cur_strip][ci] & 0xff) +
                                                                                              uvr + 512];
                                                                v1g[cur_strip][ci][0] = uiclp[(v1y0[cur_strip][ci] & 0xff) +
                                                                                             uvg + 512];
                                                                v1b[cur_strip][ci][0] = uiclp[(v1y0[cur_strip][ci] & 0xff) +
                                                                                             uvb + 512];
                                                                v1r[cur_strip][ci][1] = uiclp[(v1y1[cur_strip][ci] & 0xff) +
                                                                                             uvr + 512];
                                                                v1g[cur_strip][ci][1] = uiclp[(v1y1[cur_strip][ci] & 0xff) +
                                                                                            uvg + 512];
                                                                v1b[cur_strip][ci][1] = uiclp[(v1y1[cur_strip][ci] & 0xff) +
                                                                                            uvb + 512];
                                                                v1r[cur_strip][ci][2] = uiclp[(v1y2[cur_strip][ci] & 0xff) +
                                                                                             uvr + 512];
                                                                v1g[cur_strip][ci][2] = uiclp[(v1y2[cur_strip][ci] & 0xff) +
                                                                                            uvg + 512];
                                                                v1b[cur_strip][ci][2] = uiclp[(v1y2[cur_strip][ci] & 0xff) +
                                                                                            uvb + 512];
                                                                v1r[cur_strip][ci][3] = uiclp[(v1y3[cur_strip][ci] & 0xff) +
                                                                                             uvr + 512];
                                                                v1g[cur_strip][ci][3] = uiclp[(v1y3[cur_strip][ci] & 0xff) +
                                                                                            uvg + 512];
                                                                v1b[cur_strip][ci][3] = uiclp[(v1y3[cur_strip][ci] & 0xff) +
                                                                                            uvb + 512];
                                                            }
                                                        } // if ((flag & 0x80000000) != 0)
                                                        ci++;
                                                        flag <<= 1;
                                                    } // for (i = 0; i < 32; i++)
                                                } // while (chunk_size > 0)
                                                while (chunk_size > 0) {
                                                    // Skip byte
                                                    j++;
                                                    chunk_size--;
                                                }
                                                break;
                                            case 0x3000:
                                                // Vectors used to encode a frame
                                                while ((chunk_size > 0) && (y < y_bottom)) {
                                                    flag = (fileBuffer[j++] & 0xff) << 24;
                                                    flag |= (fileBuffer[j++] & 0xff) << 16;
                                                    flag |= (fileBuffer[j++] & 0xff) << 8;
                                                    flag |= (fileBuffer[j++] & 0xff);
                                                    chunk_size -= 4;
                                                    
                                                    for (i = 0; i < 32; i++) {
                                                        if (y >= y_bottom) {
                                                            break;
                                                        }
                                                        if ((flag & 0x80000000) != 0) {
                                                            // 4 bytes per block
                                                            d0 = fileBuffer[j++] & 0xff;
                                                            d1 = fileBuffer[j++] & 0xff;
                                                            d2 = fileBuffer[j++] & 0xff;
                                                            d3 = fileBuffer[j++] & 0xff;
                                                            chunk_size -= 4;
                                                            pixelIndex = 4 * cv_width * y + 4 * x;
                                                            
                                                            imgBuffer[pixelIndex + 1] = v4r[cur_strip][d0][0];
                                                            imgBuffer[pixelIndex + 2] = v4g[cur_strip][d0][0];
                                                            imgBuffer[pixelIndex + 3] = v4b[cur_strip][d0][0];
                                                            pixelIndex += 4;
                                                            imgBuffer[pixelIndex + 1] = v4r[cur_strip][d0][1];
                                                            imgBuffer[pixelIndex + 2] = v4g[cur_strip][d0][1];
                                                            imgBuffer[pixelIndex + 3] = v4b[cur_strip][d0][1];
                                                            pixelIndex += 4;
                                                            imgBuffer[pixelIndex + 1] = v4r[cur_strip][d1][0];
                                                            imgBuffer[pixelIndex + 2] = v4g[cur_strip][d1][0];
                                                            imgBuffer[pixelIndex + 3] = v4b[cur_strip][d1][0];
                                                            pixelIndex += 4;
                                                            imgBuffer[pixelIndex + 1] = v4r[cur_strip][d1][1];
                                                            imgBuffer[pixelIndex + 2] = v4g[cur_strip][d1][1];
                                                            imgBuffer[pixelIndex + 3] = v4b[cur_strip][d1][1];
                                                            pixelIndex += 4;
                                                            pixelIndex += row_inc;
                                                            if (pixelIndex < bufferSize) {
                                                                imgBuffer[pixelIndex + 1] = v4r[cur_strip][d0][2];
                                                                imgBuffer[pixelIndex + 2] = v4g[cur_strip][d0][2];
                                                                imgBuffer[pixelIndex + 3] = v4b[cur_strip][d0][2];
                                                                pixelIndex += 4;
                                                                imgBuffer[pixelIndex + 1] = v4r[cur_strip][d0][3];
                                                                imgBuffer[pixelIndex + 2] = v4g[cur_strip][d0][3];
                                                                imgBuffer[pixelIndex + 3] = v4b[cur_strip][d0][3];
                                                                pixelIndex += 4;
                                                                imgBuffer[pixelIndex + 1] = v4r[cur_strip][d1][2];
                                                                imgBuffer[pixelIndex + 2] = v4g[cur_strip][d1][2];
                                                                imgBuffer[pixelIndex + 3] = v4b[cur_strip][d1][2];
                                                                pixelIndex += 4;
                                                                imgBuffer[pixelIndex + 1] = v4r[cur_strip][d1][3];
                                                                imgBuffer[pixelIndex + 2] = v4g[cur_strip][d1][3];
                                                                imgBuffer[pixelIndex + 3] = v4b[cur_strip][d1][3];
                                                                pixelIndex += 4;
                                                                pixelIndex += row_inc;
                                                                if (pixelIndex < bufferSize) {
                                                                    imgBuffer[pixelIndex + 1] = v4r[cur_strip][d2][0];
                                                                    imgBuffer[pixelIndex + 2] = v4g[cur_strip][d2][0];
                                                                    imgBuffer[pixelIndex + 3] = v4b[cur_strip][d2][0];
                                                                    pixelIndex += 4;
                                                                    imgBuffer[pixelIndex + 1] = v4r[cur_strip][d2][1];
                                                                    imgBuffer[pixelIndex + 2] = v4g[cur_strip][d2][1];
                                                                    imgBuffer[pixelIndex + 3] = v4b[cur_strip][d2][1];
                                                                    pixelIndex += 4;
                                                                    imgBuffer[pixelIndex + 1] = v4r[cur_strip][d3][0];
                                                                    imgBuffer[pixelIndex + 2] = v4g[cur_strip][d3][0];
                                                                    imgBuffer[pixelIndex + 3] = v4b[cur_strip][d3][0];
                                                                    pixelIndex += 4;
                                                                    imgBuffer[pixelIndex + 1] = v4r[cur_strip][d3][1];
                                                                    imgBuffer[pixelIndex + 2] = v4g[cur_strip][d3][1];
                                                                    imgBuffer[pixelIndex + 3] = v4b[cur_strip][d3][1];
                                                                    pixelIndex += 4;
                                                                    pixelIndex += row_inc;
                                                                    if (pixelIndex < bufferSize) {
                                                                        imgBuffer[pixelIndex + 1] = v4r[cur_strip][d2][2];
                                                                        imgBuffer[pixelIndex + 2] = v4g[cur_strip][d2][2];
                                                                        imgBuffer[pixelIndex + 3] = v4b[cur_strip][d2][2];
                                                                        pixelIndex += 4;
                                                                        imgBuffer[pixelIndex + 1] = v4r[cur_strip][d2][3];
                                                                        imgBuffer[pixelIndex + 2] = v4g[cur_strip][d2][3];
                                                                        imgBuffer[pixelIndex + 3] = v4b[cur_strip][d2][3];
                                                                        pixelIndex += 4;
                                                                        imgBuffer[pixelIndex + 1] = v4r[cur_strip][d3][2];
                                                                        imgBuffer[pixelIndex + 2] = v4g[cur_strip][d3][2];
                                                                        imgBuffer[pixelIndex + 3] = v4b[cur_strip][d3][2];
                                                                        pixelIndex += 4;
                                                                        imgBuffer[pixelIndex + 1] = v4r[cur_strip][d3][3];
                                                                        imgBuffer[pixelIndex + 2] = v4g[cur_strip][d3][3];
                                                                        imgBuffer[pixelIndex + 3] = v4b[cur_strip][d3][3];
                                                                        pixelIndex += 4;
                                                                    }
                                                                }
                                                            }
                                                        } // if ((flag & 0x80000000) != 0)
                                                        else {
                                                            // 1 byte per block
                                                            index = fileBuffer[j++] & 0xff;
                                                            pixelIndex = 4 * cv_width * y + 4 * x;
                                                            imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][0];
                                                            imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][0];
                                                            imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][0];
                                                            pixelIndex += 4;
                                                            imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][0];
                                                            imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][0];
                                                            imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][0];
                                                            pixelIndex += 4;
                                                            imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][1];
                                                            imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][1];
                                                            imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][1];
                                                            pixelIndex += 4;
                                                            imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][1];
                                                            imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][1];
                                                            imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][1];
                                                            pixelIndex += 4;
                                                            pixelIndex += row_inc;
                                                            if (pixelIndex < bufferSize) {
                                                                imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][0];
                                                                imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][0];
                                                                imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][0];
                                                                pixelIndex += 4;
                                                                imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][0];
                                                                imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][0];
                                                                imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][0];
                                                                pixelIndex += 4;
                                                                imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][1];
                                                                imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][1];
                                                                imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][1];
                                                                pixelIndex += 4;
                                                                imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][1];
                                                                imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][1];
                                                                imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][1];
                                                                pixelIndex += 4;
                                                                pixelIndex += row_inc;
                                                                if (pixelIndex < bufferSize) {
                                                                    imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][2];
                                                                    imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][2];
                                                                    imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][2];
                                                                    pixelIndex += 4;
                                                                    imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][2];
                                                                    imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][2];
                                                                    imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][2];
                                                                    pixelIndex += 4;
                                                                    imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][3];
                                                                    imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][3];
                                                                    imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][3];
                                                                    pixelIndex += 4;
                                                                    imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][3];
                                                                    imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][3];
                                                                    imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][3];
                                                                    pixelIndex += 4;
                                                                    pixelIndex += row_inc;
                                                                    if (pixelIndex < bufferSize) {
                                                                        imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][2];
                                                                        imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][2];
                                                                        imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][2];
                                                                        pixelIndex += 4;
                                                                        imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][2];
                                                                        imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][2];
                                                                        imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][2];
                                                                        pixelIndex += 4;
                                                                        imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][3];
                                                                        imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][3];
                                                                        imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][2];
                                                                        pixelIndex += 4;
                                                                        imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][3];
                                                                        imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][3];
                                                                        imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][3];
                                                                        pixelIndex += 4;
                                                                    }
                                                                }
                                                            }
                                                            chunk_size--;
                                                        } // else 1 byte per block
                                                        x += 4;
                                                        if (x >= cv_width) {
                                                            x = 0;
                                                            y += 4;
                                                        }
                                                        flag <<= 1;
                                                    } // for (i = 0; i < 32; i++)
                                                } // while ((chunk_size > 0) && (y >= 0))
                                                while (chunk_size > 0) {
                                                    // skip byte
                                                    j++;
                                                    chunk_size--;
                                                }
                                                break;
                                            case 0x3100:
                                                // Selective set of vectors use to encode a frame
                                                while ((chunk_size > 0) && (y < y_bottom)) {
                                                    /* flag bits: 0 = SKIP, 10 = V1, 11 = V4 */ 
                                                    flag = (fileBuffer[j++] & 0xff) << 24;
                                                    flag |= (fileBuffer[j++] & 0xff) << 16;
                                                    flag |= (fileBuffer[j++] & 0xff) << 8;
                                                    flag |= (fileBuffer[j++] & 0xff);
                                                    chunk_size -= 4;
                                                    mask = 0x80000000;
                                                    while ((mask != 0) && (y < y_bottom)) {
                                                        if ((flag & mask) != 0) {
                                                            if (mask == 1) {
                                                                if (chunk_size < 0) {
                                                                    break;
                                                                }
                                                                flag = (fileBuffer[j++] & 0xff) << 24;
                                                                flag |= (fileBuffer[j++] & 0xff) << 16;
                                                                flag |= (fileBuffer[j++] & 0xff) << 8;
                                                                flag |= (fileBuffer[j++] & 0xff);
                                                                chunk_size -= 4;
                                                                mask = 0x80000000;
                                                            } // if (mask == 1)
                                                            else {
                                                                mask >>>= 1;
                                                            }
                                                            if ((flag & mask) != 0) {
                                                                // V4
                                                                d0 = fileBuffer[j++] & 0xff;
                                                                d1 = fileBuffer[j++] & 0xff;
                                                                d2 = fileBuffer[j++] & 0xff;
                                                                d3 = fileBuffer[j++] & 0xff;
                                                                chunk_size -= 4;
                                                                pixelIndex = 4 * cv_width * y + 4 * x;
                                                                
                                                                imgBuffer[pixelIndex + 1] = v4r[cur_strip][d0][0];
                                                                imgBuffer[pixelIndex + 2] = v4g[cur_strip][d0][0];
                                                                imgBuffer[pixelIndex + 3] = v4b[cur_strip][d0][0];
                                                                
                                                                pixelIndex += 4;
                                                                imgBuffer[pixelIndex + 1] = v4r[cur_strip][d0][1];
                                                                imgBuffer[pixelIndex + 2] = v4g[cur_strip][d0][1];
                                                                imgBuffer[pixelIndex + 3] = v4b[cur_strip][d0][1];
                                                                pixelIndex += 4;
                                                                imgBuffer[pixelIndex + 1] = v4r[cur_strip][d1][0];
                                                                imgBuffer[pixelIndex + 2] = v4g[cur_strip][d1][0];
                                                                imgBuffer[pixelIndex + 3] = v4b[cur_strip][d1][0];
                                                                pixelIndex += 4;
                                                                imgBuffer[pixelIndex + 1] = v4r[cur_strip][d1][1];
                                                                imgBuffer[pixelIndex + 2] = v4g[cur_strip][d1][1];
                                                                imgBuffer[pixelIndex + 3] = v4b[cur_strip][d1][1];
                                                                pixelIndex += 4;
                                                                pixelIndex += row_inc;
                                                                if (pixelIndex < bufferSize) {
                                                                    imgBuffer[pixelIndex + 1] = v4r[cur_strip][d0][2];
                                                                    imgBuffer[pixelIndex + 2] = v4g[cur_strip][d0][2];
                                                                    imgBuffer[pixelIndex + 3] = v4b[cur_strip][d0][2];
                                                                    pixelIndex += 4;
                                                                    imgBuffer[pixelIndex + 1] = v4r[cur_strip][d0][3];
                                                                    imgBuffer[pixelIndex + 2] = v4g[cur_strip][d0][3];
                                                                    imgBuffer[pixelIndex + 3] = v4b[cur_strip][d0][3];
                                                                    pixelIndex += 4;
                                                                    imgBuffer[pixelIndex + 1] = v4r[cur_strip][d1][2];
                                                                    imgBuffer[pixelIndex + 2] = v4g[cur_strip][d1][2];
                                                                    imgBuffer[pixelIndex + 3] = v4b[cur_strip][d1][2];
                                                                    pixelIndex += 4;
                                                                    imgBuffer[pixelIndex + 1] = v4r[cur_strip][d1][3];
                                                                    imgBuffer[pixelIndex + 2] = v4g[cur_strip][d1][3];
                                                                    imgBuffer[pixelIndex + 3] = v4b[cur_strip][d1][3];
                                                                    pixelIndex += 4;
                                                                    pixelIndex += row_inc;
                                                                    if (pixelIndex < bufferSize) {
                                                                        imgBuffer[pixelIndex + 1] = v4r[cur_strip][d2][0];
                                                                        imgBuffer[pixelIndex + 2] = v4g[cur_strip][d2][0];
                                                                        imgBuffer[pixelIndex + 3] = v4b[cur_strip][d2][0];
                                                                        pixelIndex += 4;
                                                                        imgBuffer[pixelIndex + 1] = v4r[cur_strip][d2][1];
                                                                        imgBuffer[pixelIndex + 2] = v4g[cur_strip][d2][1];
                                                                        imgBuffer[pixelIndex + 3] = v4b[cur_strip][d2][1];
                                                                        pixelIndex += 4;
                                                                        imgBuffer[pixelIndex + 1] = v4r[cur_strip][d3][0];
                                                                        imgBuffer[pixelIndex + 2] = v4g[cur_strip][d3][0];
                                                                        imgBuffer[pixelIndex + 3] = v4b[cur_strip][d3][0];
                                                                        pixelIndex += 4;
                                                                        imgBuffer[pixelIndex + 1] = v4r[cur_strip][d3][1];
                                                                        imgBuffer[pixelIndex + 2] = v4g[cur_strip][d3][1];
                                                                        imgBuffer[pixelIndex + 3] = v4b[cur_strip][d3][1];
                                                                        pixelIndex += 4;
                                                                        pixelIndex += row_inc;
                                                                        if (pixelIndex < bufferSize) {
                                                                            imgBuffer[pixelIndex + 1] = v4r[cur_strip][d2][2];
                                                                            imgBuffer[pixelIndex + 2] = v4g[cur_strip][d2][2];
                                                                            imgBuffer[pixelIndex + 3] = v4b[cur_strip][d2][2];
                                                                            pixelIndex += 4;
                                                                            imgBuffer[pixelIndex + 1] = v4r[cur_strip][d2][3];
                                                                            imgBuffer[pixelIndex + 2] = v4g[cur_strip][d2][3];
                                                                            imgBuffer[pixelIndex + 3] = v4b[cur_strip][d2][3];
                                                                            pixelIndex += 4;
                                                                            imgBuffer[pixelIndex + 1] = v4r[cur_strip][d3][2];
                                                                            imgBuffer[pixelIndex + 2] = v4g[cur_strip][d3][2];
                                                                            imgBuffer[pixelIndex + 3] = v4b[cur_strip][d3][2];
                                                                            pixelIndex += 4;
                                                                            imgBuffer[pixelIndex + 1] = v4r[cur_strip][d3][3];
                                                                            imgBuffer[pixelIndex + 2] = v4g[cur_strip][d3][3];
                                                                            imgBuffer[pixelIndex + 3] = v4b[cur_strip][d3][3];
                                                                            pixelIndex += 4;
                                                                        }
                                                                    }
                                                                }
                                                            } // if ((flag & mask) != 0)
                                                            else {
                                                                // V1
                                                                chunk_size--;
                                                                index = fileBuffer[j++] & 0xff;
                                                                pixelIndex = 4 * cv_width * y + 4 * x;
                                                                
                                                                imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][0];
                                                                imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][0];
                                                                imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][0];
                                                                pixelIndex += 4;
                                                                imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][0];
                                                                imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][0];
                                                                imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][0];
                                                                pixelIndex += 4;
                                                                imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][1];
                                                                imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][1];
                                                                imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][1];
                                                                pixelIndex += 4;
                                                                imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][1];
                                                                imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][1];
                                                                imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][1];
                                                                pixelIndex += 4;
                                                                pixelIndex += row_inc;
                                                                if (pixelIndex < bufferSize) {
                                                                    imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][0];
                                                                    imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][0];
                                                                    imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][0];
                                                                    pixelIndex += 4;
                                                                    imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][0];
                                                                    imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][0];
                                                                    imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][0];
                                                                    pixelIndex += 4;
                                                                    imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][1];
                                                                    imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][1];
                                                                    imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][1];
                                                                    pixelIndex += 4;
                                                                    imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][1];
                                                                    imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][1];
                                                                    imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][1];
                                                                    pixelIndex += 4;
                                                                    pixelIndex += row_inc;
                                                                    if (pixelIndex < bufferSize) {
                                                                        imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][2];
                                                                        imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][2];
                                                                        imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][2];
                                                                        pixelIndex += 4;
                                                                        imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][2];
                                                                        imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][2];
                                                                        imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][2];
                                                                        pixelIndex += 4;
                                                                        imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][3];
                                                                        imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][3];
                                                                        imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][3];
                                                                        pixelIndex += 4;
                                                                        imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][3];
                                                                        imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][3];
                                                                        imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][3];
                                                                        pixelIndex += 4;
                                                                        pixelIndex += row_inc;
                                                                        if (pixelIndex < bufferSize) {
                                                                            imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][2];
                                                                            imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][2];
                                                                            imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][2];
                                                                            pixelIndex += 4;
                                                                            imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][2];
                                                                            imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][2];
                                                                            imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][2];
                                                                            pixelIndex += 4;
                                                                            imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][3];
                                                                            imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][3];
                                                                            imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][2];
                                                                            pixelIndex += 4;
                                                                            imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][3];
                                                                            imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][3];
                                                                            imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][3];
                                                                            pixelIndex += 4;
                                                                        } // if (pixelIndex < bufferSize)
                                                                    } // if (pixelIndex < bufferSize)
                                                                } // if (pixelIndex < bufferSize)
                                                            } // else V1
                                                        } // if ((flag & mask) != 0)
                                                        mask >>>= 1;
                                                        x += 4;
                                                        if (x >= cv_width) {
                                                            x = 0;
                                                            y += 4;
                                                        }
                                                    } // while ((mask != 0) && (y < y_bottom))
                                                } // while ((chunk_size > 0) && (y < y_bottom)
                                                while (chunk_size > 0) {
                                                    // skip byte
                                                    j++;
                                                    chunk_size--;
                                                }
                                                break;
                                            case 0x3200:
                                                // List of blocks from only the V1 codebook
                                                while((chunk_size > 0) && (y < y_bottom)) {
                                                    index = fileBuffer[j++] & 0xff;
                                                    pixelIndex = 4 * cv_width * y + 4 * x;
                                                    
                                                    imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][0];
                                                    imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][0];
                                                    imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][0];
                                                    pixelIndex += 4;
                                                    imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][0];
                                                    imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][0];
                                                    imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][0];
                                                    pixelIndex += 4;
                                                    imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][1];
                                                    imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][1];
                                                    imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][1];
                                                    pixelIndex += 4;
                                                    imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][1];
                                                    imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][1];
                                                    imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][1];
                                                    pixelIndex += 4;
                                                    pixelIndex += row_inc;
                                                    if (pixelIndex < bufferSize) {
                                                        imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][0];
                                                        imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][0];
                                                        imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][0];
                                                        pixelIndex += 4;
                                                        imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][0];
                                                        imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][0];
                                                        imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][0];
                                                        pixelIndex += 4;
                                                        imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][1];
                                                        imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][1];
                                                        imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][1];
                                                        pixelIndex += 4;
                                                        imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][1];
                                                        imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][1];
                                                        imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][1];
                                                        pixelIndex += 4;
                                                        pixelIndex += row_inc;
                                                        if (pixelIndex < bufferSize) {
                                                            imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][2];
                                                            imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][2];
                                                            imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][2];
                                                            pixelIndex += 4;
                                                            imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][2];
                                                            imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][2];
                                                            imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][2];
                                                            pixelIndex += 4;
                                                            imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][3];
                                                            imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][3];
                                                            imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][3];
                                                            pixelIndex += 4;
                                                            imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][3];
                                                            imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][3];
                                                            imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][3];
                                                            pixelIndex += 4;
                                                            pixelIndex += row_inc;
                                                            if (pixelIndex < bufferSize) {
                                                                imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][2];
                                                                imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][2];
                                                                imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][2];
                                                                pixelIndex += 4;
                                                                imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][2];
                                                                imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][2];
                                                                imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][2];
                                                                pixelIndex += 4;
                                                                imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][3];
                                                                imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][3];
                                                                imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][2];
                                                                pixelIndex += 4;
                                                                imgBuffer[pixelIndex + 1] = v1r[cur_strip][index][3];
                                                                imgBuffer[pixelIndex + 2] = v1g[cur_strip][index][3];
                                                                imgBuffer[pixelIndex + 3] = v1b[cur_strip][index][3];
                                                                pixelIndex += 4;
                                                            } // if (pixelIndex < bufferSize)
                                                        } // if (pixelIndex < bufferSize)
                                                    } // if (pixelIndex < bufferSize)
                                                    chunk_size--;
                                                    x += 4;
                                                    if (x >= cv_width) {
                                                        x = 0;
                                                        y += 4;
                                                    }
                                                } // while ((chunk_size > 0) && (y < y_bottom))
                                                while (chunk_size > 0) {
                                                    // skip byte
                                                    j++;
                                                    chunk_size--;
                                                }
                                                break;
                                            default:
                                                Preferences.debug("CVID: unknown chunk_id = " + chunk_id + "\n");
                                                while (chunk_size > 0) {
                                                    // skip byte
                                                    j++;
                                                    chunk_size--;
                                                }
                                        } // switch(chunk_id)
                                        
                                    } // while (top_size > 0)
                                } // for (cur_strip = 0; cur_strip < strips; cur_strip++)

                                if ((cv_width != imgExtents[0]) || (cv_height != imgExtents[1])) {
                                    for (y = 0; y < imgExtents[1]; y++) {
                                        for (x = 0; x < imgExtents[0]; x++) {
                                            for (c = 0; c < 4; c++) {
                                                imgBuffer2[4*(x + y * imgExtents[0]) + c] = imgBuffer[4*(x + y * cv_width) + c];
                                            }
                                        }
                                    }
                                    if (one) {
                                        imageA.importData(0, imgBuffer2, false);
                                    } else {
                                        imageA.importData(z * bufferSize, imgBuffer2, false);
                                    }
                                }
                                else {
                                    if (one) {
                                        imageA.importData(0, imgBuffer, false);
                                    } else {
                                        imageA.importData(z * bufferSize, imgBuffer, false);
                                    }
                                }

                                if (actualFrames > 1) {
                                    fireProgressStateChanged(100 * z / (imgExtents[2] - 1));
                                }
                                x = 0;
                                y = 0;
                                y_bottom = 0;
                                j = 0;
                                pixelIndex = 0;
                                z++;
                            } // else
                        } // if ((dataLength > 0) && ((totalBytesRead + dataLength) <= totalDataArea))
                    } // else

                    subchunkBlocksRead++;

                    if (haveMoviSubchunk && (subchunkBlocksRead == streams) && (totalBytesRead < totalDataArea)) {
                        totalBytesRead += moviSubchunkPosition + LIST2subchunkSize - raFile.getFilePointer();
                        raFile.seek(moviSubchunkPosition + LIST2subchunkSize);

                        // Check for LIST rec<sp> subchunks
                        signature = getInt(endianess);
                        totalBytesRead += 4;

                        if (signature == 0x5453494C) {

                            // have read LIST
                            LIST2subchunkSize = getInt(endianess);
                            totalBytesRead += 4;
                            moviSubchunkPosition = raFile.getFilePointer();
                            CHUNKtype = getInt(endianess);

                            if (CHUNKtype == 0x20636572) {

                                // have read rec<sp>
                                totalBytesRead += 4;
                                subchunkDataArea = LIST2subchunkSize - 4;
                                subchunkBytesRead = 0;
                                subchunkBlocksRead = 0;
                            } else {
                                raFile.close();
                                throw new IOException("CHunktype for LIST2sbuchunk is an illegal = " + CHUNKtype);
                            }
                        } else {
                            chunkRead = false;
                        }
                    } // if (haveMoviSubchunk && (subchunkBlocksRead == streams))
                } // while ((totalBytesRead < (totalDataArea-8)) && chunkRead)
                
                
            } // else if (doCVID && (bitCount == 24)) 
            else if (doCYUV && (bitCount == 16)) {
                bufferSize = 4 * imgExtents[0] * imgExtents[1];
                imgBuffer = new byte[bufferSize];
                dataSignature = new byte[4];
                totalDataArea = LIST2Size - 4; // Subtract out 4 'movi' bytes
                totalBytesRead = 0;

                // Check for LIST rec<sp> subchunks
                if (!AVIF_MUSTUSEINDEX) {
                    signature = getInt(endianess);

                    if (signature == 0x5453494C) {

                        // have read LIST
                        LIST2subchunkSize = getInt(endianess);
                        moviSubchunkPosition = raFile.getFilePointer();
                        CHUNKtype = getInt(endianess);

                        if (CHUNKtype == 0x20636572) {

                            // have read rec<sp>
                            haveMoviSubchunk = true;
                            subchunkDataArea = LIST2subchunkSize - 4;
                            subchunkBytesRead = 0;
                            subchunkBlocksRead = 0;
                        } else {
                            raFile.close();
                            throw new IOException("CHUNK type in second LIST sub CHUNK is an illegal = " + CHUNKtype);
                        }
                    } else {
                        raFile.seek(startPosition);
                    }
                } // if (!AVIF_MUSTUSEINDEX)

                x = 0;
                y = imgExtents[1] - 1;
                z = 0;

                int pixelIndex = 0;
                boolean bufferFinished = false;
                byte[] delta_y_tbl = new byte[16];
                byte[] delta_c_tbl = new byte[16];
                int cur_Y = 0;
                int cur_U = 0;
                int cur_V = 0;
                int Y[] = new int[4];
                int i;
                int r;
                int g;
                int b;
                

                chunkRead = true;

                loopCYUV:
                while (((!AVIF_MUSTUSEINDEX) && (totalBytesRead < totalDataArea) && chunkRead) ||
                           (AVIF_MUSTUSEINDEX && (indexBytesRead < indexSize))) {
                    if (AVIF_MUSTUSEINDEX) {
                        raFile.seek(indexPointer);
                        dataFound = false;

                        while (!dataFound) {
                            raFile.read(dataSignature);

                            if ((dataSignature[2] != 0x64 /*d */) || (dataSignature[3] < 0x62 /* b */) ||
                                    (dataSignature[3] > 0x63 /* c */)) {
                                indexPointer = indexPointer + 16;
                                indexBytesRead += 16;
                                if (indexBytesRead >= indexSize) {
                                    break loopCYUV;
                                }
                                raFile.seek(indexPointer);
                            } else {
                                dataFound = true;
                            }
                        } // while (!dataFound)

                        indexPointer = indexPointer + 8;
                        raFile.seek(indexPointer);
                        moviOffset = getInt(endianess);
                        indexPointer = indexPointer + 8;
                        indexBytesRead += 16;
                        raFile.seek(moviPosition + (long) moviOffset);
                    } // if (AVIFMUSTINDEX)

                    raFile.read(dataSignature);
                    totalBytesRead = totalBytesRead + 4;
                    subchunkBytesRead += 4;

                    if ((dataSignature[2] == 0x64 /* d */) &&
                            ((dataSignature[3] == 0x63 /* c */) || (dataSignature[3] == 0x62 /* b */))) {
                        dataLength = getInt(endianess);

                        if ((dataLength % 2) == 1) {
                            dataLength++;
                        }

                        totalBytesRead = totalBytesRead + 4;
                        subchunkBytesRead += 4;

                        if (one && (z != middleSlice)) {
                            long ptr = raFile.getFilePointer();
                            raFile.seek(ptr + dataLength);
                            totalBytesRead = totalBytesRead + dataLength;
                            subchunkBytesRead += dataLength;
                            z++;
                        } else {
                            fileBuffer = new byte[dataLength];
                            raFile.read(fileBuffer);
                            totalBytesRead = totalBytesRead + dataLength;
                            subchunkBytesRead += dataLength;
                            bufferFinished = false;
                            pixelIndex = 0;

                            for (int j = 0; (j < dataLength) && (!bufferFinished);) {
                                // 16 byte prediction table 1 is unused
                                j += 16;
                                for (k = 0; k < 16; k++) {
                                    delta_y_tbl[k] = fileBuffer[j++];
                                }
                                for (k = 0; k < 16; k++) {
                                    delta_c_tbl[k] = fileBuffer[j++];
                                }
                                
                                for (y = 0; y < imgExtents[1]; y++) {
                                    for (x = 0; x < imgExtents[0]; x += 4) {
                                        if (x == 0) { /* first pixels in scanline */
                                           cur_U = fileBuffer[j++];
                                           cur_Y = (cur_U & 0x0f) << 4;
                                           cur_U = cur_U & 0xf0;
                                           Y[0] = cur_Y;
                                           
                                           cur_V = fileBuffer[j++];
                                           cur_Y = (cur_Y + delta_y_tbl[cur_V & 0x0f]) & 0xff;
                                           cur_V = cur_V & 0xf0;
                                           Y[1] = cur_Y;
                                        } // if (x == 0)
                                        else { /** subsequent pixels in scanline */
                                           i = fileBuffer[j++] & 0xff; 
                                           cur_U = (cur_U + delta_c_tbl[i >>> 4]) & 0xff;
                                           cur_Y = (cur_Y + delta_y_tbl[i & 0x0f]) & 0xff;
                                           Y[0] = cur_Y;
                                           
                                           i = fileBuffer[j++] & 0xff;
                                           cur_V = (cur_V + delta_c_tbl[i >>> 4]) & 0xff;
                                           cur_Y = (cur_Y + delta_y_tbl[i & 0x0f]) & 0xff;
                                           Y[1] = cur_Y;
                                        } // else x != 0
                                        
                                        i = fileBuffer[j++] & 0xff;
                                        cur_Y = (cur_Y + delta_y_tbl[i & 0x0f]) & 0xff;
                                        Y[2] = cur_Y;
                                        
                                        cur_Y = (cur_Y + delta_y_tbl[i >>> 4]) & 0xff;
                                        Y[3] = cur_Y;
                                        
                                        for (k = 0; k < 4; k++) {
                                            // ITU-R 610 conversion formula for SDTV:
                                           r = (int)Math.round(1.164 * (Y[k] - 16) + 1.596 * (cur_V - 128));
                                           if (r < 0) {
                                               r = 0;
                                           }
                                           else if (r > 255) {
                                               r = 255;
                                           }
                                           imgBuffer[4 * pixelIndex + 1] = (byte)r;
                                           g = (int)Math.round(1.164 * (Y[k] - 16) - 0.813 * (cur_V - 128) - 0.391 * (cur_U - 128));
                                           if (g < 0) {
                                               g = 0;
                                           }
                                           else if (g > 255) {
                                               g = 255;
                                           }
                                           imgBuffer[4 * pixelIndex + 2] = (byte)g;
                                           b = (int)Math.round(1.164 * (Y[k] - 16) + 2.018 * (cur_U - 128));
                                           if (b < 0) {
                                               b = 0;
                                           }
                                           else if (b > 255) {
                                               b = 255;
                                           }
                                           imgBuffer[4 * pixelIndex + 3] = (byte)b;
                                           pixelIndex++;
                                           /**
                                            * ITU-R 709 for HDTV
                                            * R = 1.164 * (Y - 16) + 2.115 * (V - 128)
                                            * G = 1.164 * (Y - 16) - 0.534 * (V - 128) - 0.213 * (U - 128)
                                            * B = 1.164 * (Y - 16) + 1.793 * (U - 128)
                                            */
                                        } // for (k = 0; k < 4; k++)
                                           
                                       // Check if decode is finished
                                       if ((pixelIndex == imgExtents[0]*imgExtents[1]) || (j == dataLength)) {

                                           bufferFinished = true;

                                           if (one) {
                                               imageA.importData(0, imgBuffer, false);
                                           } else {
                                               imageA.importData(z * bufferSize, imgBuffer, false);
                                           }

                                           if (actualFrames > 1) {
                                               fireProgressStateChanged(100 * z / (imgExtents[2] - 1));
                                           }
                                           z++;
                                       }
                                    }
                                        
                                    } // for (x = 0; x < imgExtents[0]; x += 4)
                                } // for (y = 0; y < imgExtents[1]; y++)

                                
                            } // for (int j = 0; (j < dataLength) && (!bufferFinished); )
                    } // else if ((dataSignature[2] == 0x64 /* d */) && (dataSignature[3] == 0x63 /* c */))
                    else {
                        dataLength = getInt(endianess);

                        if ((dataLength % 2) == 1) {
                            dataLength++;
                        }

                        totalBytesRead = totalBytesRead + 4;
                        subchunkBytesRead += 4;

                        long ptr = raFile.getFilePointer();
                        raFile.seek(ptr + dataLength);
                        totalBytesRead = totalBytesRead + dataLength;
                        subchunkBytesRead += dataLength;
                    } // else

                    subchunkBlocksRead++;

                    if (haveMoviSubchunk && (subchunkBlocksRead == streams) && (totalBytesRead < totalDataArea)) {
                        totalBytesRead += moviSubchunkPosition + LIST2subchunkSize - raFile.getFilePointer();
                        raFile.seek(moviSubchunkPosition + LIST2subchunkSize);

                        // Check for LIST rec<sp> subchunks
                        signature = getInt(endianess);
                        totalBytesRead += 4;

                        if (signature == 0x5453494C) {

                            // have read LIST
                            LIST2subchunkSize = getInt(endianess);
                            totalBytesRead += 4;
                            moviSubchunkPosition = raFile.getFilePointer();
                            CHUNKtype = getInt(endianess);

                            if (CHUNKtype == 0x20636572) {

                                // have read rec<sp>
                                totalBytesRead += 4;
                                subchunkDataArea = LIST2subchunkSize - 4;
                                subchunkBytesRead = 0;
                                subchunkBlocksRead = 0;
                            } else {
                                raFile.close();
                                throw new IOException("CHunktype for LIST2sbuchunk is an illegal = " + CHUNKtype);
                            }
                        } else {
                            chunkRead = false;
                        }
                    } // if (haveMoviSubchunk && (subchunkBlocksRead == streams))
                } // while ((totalBytesRead < totalDataArea) && chunkRead)    
            } // else if (doCYUV && (bitCount == 16))
            else if (doMJPEG) {
                boolean rgb;
                if (bitCount == 24) {
                    bufferSize = 4 * imgExtents[0] * imgExtents[1];
                    rgb = true;
                }
                else {
                    bufferSize = imgExtents[0] * imgExtents[1];
                    rgb = false;
                }
                imgBuffer = new byte[bufferSize];
                dataSignature = new byte[4];
                totalDataArea = LIST2Size - 4; // Subtract out 4 'movi' bytes
                totalBytesRead = 0;
                // M-JPEG Format A
                // The contents of the app 1 marker is described below.
                // The app 1 marker appears in each field.
                
                // In keeping with the JPEG specification, all values appear in the stream wiht
                // the most significant byte first (i.e. Motorola byte ordering).  All fields below
                // are 4 byte long integers.  All sizes and bytes are calculated from the beginning
                // of the compressed data stream.
                // For historical (i.e. hardware specific reasons) this field's contents are undefined.
                // If possible, it should be set to zero.
                int fieldSize;
                int len;
                byte app0[] = new byte[4];
                String id;
                byte polarity;
                int fieldSizeLessPadding;
                int  v;
                int v2;
                int startCode = -1;
                int restart_interval = 0;
                int restart_count = 0;
                int index;
                int i;
                int js;
                boolean lossless = false;
                boolean ls = false;
                boolean progressive = false;
                int bits = 8; /* bits per component */
                boolean pegasus_rct = false; /* pegasus reversible colorspace transform */
                boolean rct = false; /* standard reversible colorspace transform */
                int swidth = 0;
                int sheight = 0;
                boolean interlaced = false; /* true if interlaced */
                int org_height = imgExtents[1]; /* Size at codec init */
                boolean first_picture = true; /* true if decoding first picture */
                boolean bottom_field = false; /* true if bottom field */
                byte bottomField = -1;
                int nb_components = 0;
                int h_max = 0;
                int v_max = 0;
                int component_id[] = new int[MAX_COMPONENTS];
                int h_count[] = new int[MAX_COMPONENTS]; /* horizontal and vertical count for each component */
                int v_count[] = new int[MAX_COMPONENTS];
                int h_scount[] = new int[MAX_COMPONENTS]; /* horizontal and vertical count for each component */
                int v_scount[] = new int[MAX_COMPONENTS];
                int comp_index[] = new int[MAX_COMPONENTS];
                int dc_index[] = new int[MAX_COMPONENTS];
                int ac_index[] = new int[MAX_COMPONENTS];
                int nb_blocks[] = new int[MAX_COMPONENTS];
                last_dc = new int[MAX_COMPONENTS]; /* last DEQUANTIZED dc  */
                int linesize[] = new int[MAX_COMPONENTS];                   ///< linesize << interlaced
                int quant_index[] = new int[4];   /* quant table index for each component */
                boolean interlace_polarity = false; /* true for bottom field first */
                int pix_fmt_id;
                /**
                 * low resolution decoding, 1-> 1/2 size, 2->1/4 size
                 * - encoding: unused
                 * - decoding: Set by user.
                 */
                 int lowres = 0;
                 boolean cs_itu601 = false;
                 // If buggy avid, it puts EOI only at every 10th frame
                 boolean buggy_avid = false;
                 int pix_fmt = PIX_FMT_NONE;
                 byte buffer[] = null;
                 int bufp[] = new int[1];
                 byte xb;
                 int block_size;
                 int idsos;
                 int predictor;
                 int ilv;
                 int prev_shift;
                 int point_transform;
                 int mb_width = 0;
                 int mb_height = 0;
                 int h;
                 int mjpb_skiptosod = 0;
                 int c;
                 int ptr[] = new int[1];
                 int dataPtr[] = new int[4];
                 byte data[][] = new byte[4][];
                 byte destBuf[] = new byte[4 * imgExtents[0] * imgExtents[1]];
                 int sofWidth = imgExtents[0];
                 int sofHeight = imgExtents[1];
                 int zs = 0;
                 int x_chroma_shift;
                 int y_chroma_shift;
                 int w2;
                 int pSize;
                 int h2;
                 int size2;
                 int dhtClass;
                 byte bits_table[] = null;
                 int code_max;
                 byte val_table[] = null;
                 int fieldPaddingBytes = 0;
               
                int mb_x;
                int mb_y;
                int n;
                short block[] = new short[64];
                int EOBRUN[] = new int[1];
                byte cbuf[] = null;
                String comStr = null;
                int colorSpace;
                boolean srgb = true; // rgb direct as opposed to yuv
                int bit_count = 0;
                int t = 0;
                int b = 0;
                int cur_scan = 0; /* current scan, used by JPEG-LS */
                int bit_left = 32;
                int bit_buf = 0;
                short rgb_buffer[][] = null;
                int mask;
                int modified_predictor = 1;
                int left[];
                int top[];
                int topleft[];
                int pred;
                
                if (!mjpegDecodeInit) {
                    int end;
                    mjpegDecodeInit = true;
                    //idct_permutation = new int[64];
                    //for (i = 0; i < 64; i++) {
                        //idct_permutation[i] = i;
                    //}
                    scantable_permutated = new int[64];
                    //stInverse = new int[64];
                    stRasterEnd = new int[64];
                    quant_matrixes = new short[4][64];
                    qscale = new int[4];
                    for (i = 0; i < 64; i++) {
                        int j;
                        j = ff_zigzag_direct[i];
                        scantable_permutated[i] = j;
                        // stInverse[j] = i;
                    }
                    end = -1;
                    for (i = 0; i < 64; i++) {
                        int j;
                        j = scantable_permutated[i];
                        if (j > end) {
                            end = j;
                        }
                        stRasterEnd[i] = end;
                    }
                    vlcs = new short[2][4][][];
                    vlcs_bits = new int[2][4];
                    vlcs_table_size = new int[2][4];
                    vlcs_table_allocated = new int[2][4];
                    build_basic_mjpeg_vlc();
                    ff_cropTbl = new byte[256 + 2 * MAX_NEG_CROP];
                    for (i = 0; i < 256; i++) {
                        ff_cropTbl[i + MAX_NEG_CROP] = (byte)i;
                    }
                    for (i = 0; i < MAX_NEG_CROP; i++) {
                        ff_cropTbl[i] = 0;
                        ff_cropTbl[i + MAX_NEG_CROP + 256] = (byte)255;
                    }
                } if (!mjpegDecodeInit)
                // Check for LIST rec<sp> subchunks
                if (!AVIF_MUSTUSEINDEX) {
                    signature = getInt(endianess);

                    if (signature == 0x5453494C) {

                        // have read LIST
                        LIST2subchunkSize = getInt(endianess);
                        moviSubchunkPosition = raFile.getFilePointer();
                        CHUNKtype = getInt(endianess);

                        if (CHUNKtype == 0x20636572) {

                            // have read rec<sp>
                            haveMoviSubchunk = true;
                            subchunkDataArea = LIST2subchunkSize - 4;
                            subchunkBytesRead = 0;
                            subchunkBlocksRead = 0;
                        } else {
                            raFile.close();
                            throw new IOException("CHUNK type in second LIST sub CHUNK is an illegal = " + CHUNKtype);
                        }
                    } else {
                        raFile.seek(startPosition);
                    }
                } // if (!AVIF_MUSTUSEINDEX)

                x = 0;
                y = imgExtents[1] - 1;
                z = 0;

                boolean bufferFinished = false;
                

                chunkRead = true;

                loopMJPG:
                while (((!AVIF_MUSTUSEINDEX) && (totalBytesRead < totalDataArea) && chunkRead) ||
                           (AVIF_MUSTUSEINDEX && (indexBytesRead < indexSize))) {
                    if (AVIF_MUSTUSEINDEX) {
                        raFile.seek(indexPointer);
                        dataFound = false;

                        while (!dataFound) {
                            raFile.read(dataSignature);

                            if ((dataSignature[2] != 0x64 /*d */) || (dataSignature[3] < 0x62 /* b */) ||
                                    (dataSignature[3] > 0x63 /* c */)) {
                                indexPointer = indexPointer + 16;
                                indexBytesRead += 16;
                                if (indexBytesRead >= indexSize) {
                                    break loopMJPG;
                                }
                                raFile.seek(indexPointer);
                            } else {
                                dataFound = true;
                            }
                        } // while (!dataFound)

                        indexPointer = indexPointer + 8;
                        raFile.seek(indexPointer);
                        moviOffset = getInt(endianess);
                        indexPointer = indexPointer + 8;
                        indexBytesRead += 16;
                        raFile.seek(moviPosition + (long) moviOffset);
                    } // if (AVIFMUSTINDEX)

                    raFile.read(dataSignature);
                    totalBytesRead = totalBytesRead + 4;
                    subchunkBytesRead += 4;

                    if ((dataSignature[2] == 0x64 /* d */) &&
                            ((dataSignature[3] == 0x63 /* c */) || (dataSignature[3] == 0x62 /* b */))) {
                        dataLength = getInt(endianess);

                        if ((dataLength % 2) == 1) {
                            dataLength++;
                        }

                        totalBytesRead = totalBytesRead + 4;
                        subchunkBytesRead += 4;

                        if (one && (z != middleSlice)) {
                            long lptr = raFile.getFilePointer();
                            raFile.seek(lptr + dataLength);
                            totalBytesRead = totalBytesRead + dataLength;
                            subchunkBytesRead += dataLength;
                            z++;
                        } else {
                            fileBuffer = new byte[dataLength];
                            raFile.read(fileBuffer);
                            totalBytesRead = totalBytesRead + dataLength;
                            subchunkBytesRead += dataLength;
                            bufferFinished = false;
                            bigloop:
                            for (int j = 0; (j < dataLength) && (!bufferFinished);) {
                                startCode = -1;
                                while (j < dataLength) {
                                    if (j == dataLength - 1) {
                                        j++;
                                        bufferFinished = true;
                                        break bigloop;
                                    }
                                    v = fileBuffer[j++] & 0xff;
                                    v2 = fileBuffer[j] & 0xff;
                                    if ((v == 0xff) && (v2 >= 0xc0) && (v2 <= 0xfe) && (j < dataLength)) {
                                       startCode = fileBuffer[j++] & 0xff;
                                       break;
                                    }
                                }
                                switch (startCode) {
                                    case SOF0:
                                        Preferences.debug("startCode = SOF0\n");
                                        break;
                                    case SOF1:
                                        Preferences.debug("startCode = SOF1\n");
                                        break;
                                    case SOF2:
                                        Preferences.debug("startCode = SOF2\n");
                                        break;
                                    case SOF3:
                                        Preferences.debug("startCode = SOF3\n");
                                        break;
                                    case RST0:
                                        Preferences.debug("startCode = RST0\n");
                                        break;
                                    case RST1:
                                        Preferences.debug("startCode = RST1\n");
                                        break;
                                    case RST2:
                                        Preferences.debug("startCode = RST2\n");
                                        break;
                                    case RST3:
                                        Preferences.debug("startCode = RST3\n");
                                        break;
                                    case RST4:
                                        Preferences.debug("startCode = RST4\n");
                                        break;
                                    case RST5:
                                        Preferences.debug("startCode = RST5\n");
                                        break;
                                    case RST6:
                                        Preferences.debug("startCode = RST6\n");
                                        break;
                                    case RST7:
                                        Preferences.debug("startCode = RST7\n");
                                        break;
                                    case DHT:
                                        Preferences.debug("startCode = DHT\n");
                                        break;
                                    case SOI:
                                        Preferences.debug("startCode = SOI\n");
                                        break;
                                    case EOI:
                                        Preferences.debug("startCode = EOI\n");
                                        break;
                                    case SOS:
                                        Preferences.debug("startCode = SOS\n");
                                        break;
                                    case DQT:
                                        Preferences.debug("startCode = DQT\n");
                                        break;
                                    case APP0:
                                        Preferences.debug("startCode = APP0\n");
                                        break;
                                    case APP1:
                                        Preferences.debug("startCode = APP1\n");
                                        break;
                                    case APP2:
                                        Preferences.debug("startCode = APP2\n");
                                        break;
                                    case APP3:
                                        Preferences.debug("startCode = APP3\n");
                                        break;
                                    case APP4:
                                        Preferences.debug("startCode = APP4\n");
                                        break;
                                    case APP5:
                                        Preferences.debug("startCode = APP5\n");
                                        break;
                                    case APP6:
                                        Preferences.debug("startCode = APP6\n");
                                        break;
                                    case APP7:
                                        Preferences.debug("startCode = APP7\n");
                                        break;
                                    case APP8:
                                        Preferences.debug("startCode = APP8\n");
                                        break;
                                    case APP9:
                                        Preferences.debug("startCode = APP9\n");
                                        break;
                                    case APP10:
                                        Preferences.debug("startCode = APP10\n");
                                        break;
                                    case APP11:
                                        Preferences.debug("startCode = APP11\n");
                                        break;
                                    case APP12:
                                        Preferences.debug("startCode = APP12\n");
                                        break;
                                    case APP13:
                                        Preferences.debug("startCode = APP13\n");
                                        break;
                                    case APP14:
                                        Preferences.debug("startCode = APP14\n");
                                        break;
                                    case APP15:
                                        Preferences.debug("startCode = APP15\n");
                                        break;
                                    case DRI:
                                        Preferences.debug("startCode = DRI\n");
                                        break;
                                    case JPG13:
                                        Preferences.debug("startCode = JPG13\n");
                                        break;
                                    case COM:
                                        Preferences.debug("startCode = COM\n");
                                        break;
                                    default:
                                        Preferences.debug("startCode = " + startCode + "\n");
                                }
                                if (startCode == SOI) {
                                    restart_interval = 0;
                                    restart_count = 0;
                                }
                                else if (startCode == DQT) {
                                    if (j >= fileBuffer.length) {
                                        continue loopMJPG;
                                    }
                                    len = (fileBuffer[j++] & 0xff) << 8;
                                    len |= (fileBuffer[j++] & 0xff);
                                    len -= 2;
                                    
                                    while (len >= 65) {
                                        /* Only 8 bit precision handled */
                                        index = fileBuffer[j++] & 0xff;
                                        Preferences.debug("index = " + index + "\n");
                                        if (index >= 4) {
                                            // >= 16 would be 16 bits precision
                                            // also cannot have >= 4
                                            MipavUtil.displayError("index = " + index + " startCode == DQT");
                                            return null;
                                        }
                                        /* read quant table */
                                        for (i = 0; i < 64; i++) {
                                            js = scantable_permutated[i];  
                                            quant_matrixes[index][js] = (short)(fileBuffer[j++] & 0xff);
                                        } // for (i = 0; i < 64; i++)
                                        
                                        qscale[index] = (Math.max(quant_matrixes[index][scantable_permutated[1]], 
                                                                  quant_matrixes[index][scantable_permutated[8]]) >> 1);
                                        len -= 65;
                                    } // while (len >= 65)
                                } // else if (startCode == DQT)
                                else if (startCode == DRI) {
                                    len = (fileBuffer[j++] & 0xff) << 8;
                                    len |= (fileBuffer[j++] & 0xff);
                                    if (len != 4) {
                                        MipavUtil.displayError("len = " + len + " instead of 4 for DRI");
                                        return null;
                                    }
                                    restart_interval = (fileBuffer[j++] & 0xff) << 8;
                                    restart_interval |= (fileBuffer[j++] & 0xff);
                                    Preferences.debug("DRI set restart_interval to " + restart_interval + "\n");
                                    restart_count = 0;
                                } // else if (startCode == DRI)
                                else if (startCode == EOI) {
                                    cur_scan = 0;    
                                } // else if (startCode == EOI)
                                else if (startCode == SOS) {
                                    buffer = new byte[dataLength - j + FF_INPUT_BUFFER_PADDING_SIZE];
                                    bufp[0] = 0;
                                    if (ls) {
                                        bit_count = 0;
                                        t = 0;
                                        b = 0;
                                        
                                        cur_scan++;
                                        
                                        /* find marker */
                                        while (j + t < dataLength) {
                                            xb = fileBuffer[j + t];
                                            t++;
                                            if (xb == (byte)0xff) {
                                                while ((j + t < dataLength) && (xb == (byte)0xff)) { 
                                                    xb = fileBuffer[j + t];
                                                    t++;
                                                } // while ((j + t < dataLength) && (xb == (byte)0xff))
                                                if ((xb & 0x80) != 0) {
                                                    t -= 2;
                                                    break;
                                                } // if ((xb & 0x80) != 0)
                                            } // if (xb == (byte)0xff)
                                        } // while (j + t < dataLength)
                                        bit_count = 8 * t;
                                        
                                        bit_left = 32;
                                        bit_buf = 0;
                                        
                                        /* unescape bitstream */
                                        while (b < t) {
                                            xb = fileBuffer[j + b];
                                            b++;
                                            if (8 < bit_left) {
                                                bit_buf = (bit_buf << 8) | (xb & 0xff);
                                                bit_left -= 8;
                                            }
                                            else {
                                                bit_buf <<= bit_left;
                                                bit_buf |= (xb & 0xff) >> (8 - bit_left);
                                                buffer[bufp[0]] = (byte)((bit_buf >>> 24) & 0xff);
                                                buffer[bufp[0]+1] = (byte)((bit_buf >>> 16) & 0xff);
                                                buffer[bufp[0]+2] = (byte)((bit_buf >>> 8) & 0xff);
                                                buffer[bufp[0]+3] = (byte)(bit_buf & 0xff);
                                                bufp[0] += 4;
                                                bit_left += 32 - 8;
                                                bit_buf = (xb & 0xff);
                                            }
                                            if (xb == (byte)0xff) {
                                                xb = fileBuffer[j + b];
                                                b++;
                                                if (7 < bit_left) {
                                                    bit_buf = (bit_buf << 7) | (xb & 0xff);
                                                    bit_left -= 7;
                                                }
                                                else {
                                                    bit_buf <<= bit_left;
                                                    bit_buf |= (xb & 0xff) >> (7 - bit_left);
                                                    buffer[bufp[0]] = (byte)((bit_buf >>> 24) & 0xff);
                                                    buffer[bufp[0]+1] = (byte)((bit_buf >>> 16) & 0xff);
                                                    buffer[bufp[0]+2] = (byte)((bit_buf >>> 8) & 0xff);
                                                    buffer[bufp[0]+3] = (byte)(bit_buf & 0xff);
                                                    bufp[0] += 4;
                                                    bit_left += 32 - 8;
                                                    bit_buf = (xb & 0xff);
                                                }
                                                bit_count--;
                                            } // if (xb == (byte)0xff)
                                        } // while (b < t)
                                        
                                        bit_buf <<= bit_left;
                                        while (bit_left < 32) {
                                            buffer[bufp[0]++] = (byte)((bit_buf >>> 24) & 0xff);
                                            bit_buf <<= 8;
                                            bit_left += 8;
                                        } // while (bit_left < 32)
                                        bit_left = 32;
                                        bit_buf = 0;
                                    } // if (ls)
                                    else { // !ls
                                        while (j < dataLength) {
                                          xb = fileBuffer[j++]; 
                                          buffer[bufp[0]++] = xb;
                                          if (!thpCodec) {
                                              if (xb == (byte)0xff) {
                                                  while ((j < dataLength) && (xb == (byte)0xff)) {
                                                      xb = fileBuffer[j++];
                                                  } // while ((j < dataLength) && (xb == (byte)0xff))
                                                  if (((xb & 0xff) >= 0xd0) && ((xb & 0xff) <= 0xd7)) {
                                                      buffer[bufp[0]++] = xb;
                                                  }
                                                  else if (xb != 0) {
                                                      if (((fileBuffer[j-2] & 0xff) == 0xff) && ((fileBuffer[j-1] & 0xff) == EOI)) {
                                                          j -= 2; // needed to recover EOI.  EOI is after SOS and EOI is before SOI.
                                                      }
                                                      break;
                                                  }   
                                              } // if (xb == (byte)0xff)
                                          } // if (!thpCodec)
                                        } // while (j < dataLength)
                                    } // else !ls
                                    bufp[0] = 0;
                                    block_size = lossless ? 1: 8;
                                    len = (buffer[bufp[0]++] & 0xff) << 8;
                                    len |= (buffer[bufp[0]++] & 0xff);
                                    nb_components = buffer[bufp[0]++] & 0xff;
                                    if (len != 6 + 2*nb_components) {
                                        MipavUtil.displayError("Error on decode sos invalid len = " + len);
                                        return null;
                                    }
                                    for (i = 0; i < nb_components; i++) {
                                        idsos = (buffer[bufp[0]++] & 0xff) - 1;
                                        /* find component index */
                                        for (index = 0; index < nb_components; index++) {
                                            if (idsos == component_id[index]) {
                                                break;
                                            }
                                        }
                                        if (index == nb_components) {
                                            MipavUtil.displayError("decode_sos: index " + index + " out of components");
                                            return null;
                                        }
                                        comp_index[i] = index;
                                        nb_blocks[i] = h_count[index] * v_count[index];
                                        h_scount[i] = h_count[index];
                                        v_scount[i] = v_count[index];
                                   
                                        dc_index[i] = (buffer[bufp[0]] & 0xf0) >> 4;
                                        ac_index[i] = buffer[bufp[0]++] & 0x0f;
                                        
                                        if ((dc_index[i] < 0) || (ac_index[i] < 0) ||
                                            (dc_index[i] >= 4) || (ac_index[i] >= 4)) {
                                            MipavUtil.displayError("decode sos ac/dc index out of range");
                                            return null;
                                        }
                                    } // for (i = 0; i < nb_components; i++)
                                    
                                    predictor = buffer[bufp[0]++] & 0xff; /* JPEG Ss / lossless JPEG predictor / JPEG-LS NEAR */
                                    ilv = buffer[bufp[0]++] & 0xff; /* JPEG Se / JPEG-LS ILV */
                                    prev_shift = (buffer[bufp[0]] & 0xf0) >> 4; /* Ah */
                                    point_transform = buffer[bufp[0]++] & 0x0f; /* Al */
                                    
                                    for (i = 0; i < nb_components; i++) {
                                        last_dc[i] = 1024;
                                    }
                                    
                                    Preferences.debug("In SOS: sofWidth = " + sofWidth + " sofHeight = " + sofHeight + "\n");
                                    if (nb_components > 1) {
                                        /* interleaved stream */
                                        mb_width = (sofWidth + h_max * block_size - 1)/ (h_max * block_size);
                                        mb_height = (sofHeight + v_max * block_size - 1)/ (v_max * block_size);
                                    }
                                    else if (!ls) { /* Skip this for JPEG-LS */
                                        h = h_max /h_scount[0];   
                                        v = v_max / v_scount[0];
                                        mb_width = (sofWidth + h * block_size - 1) / (h * block_size);
                                        mb_height = (sofHeight + v * block_size - 1) / ( v * block_size);
                                        nb_blocks[0] = 1;
                                        h_scount[0] = 1;
                                        v_scount[0] = 1;
                                    }
                                    
                                    /* mjpeg-b can have padding bytes between sos and image data, skip them */
                                    for (i = mjpb_skiptosod; i > 0; i--) {
                                        bufp[0]++;
                                    }
                                    
                                    if (lossless) {
                                        if (ls) {
                                            
                                        }
                                        else if (srgb) {
                                            rgb_buffer = new short[32768][4];
                                            left = new int[3];
                                            top = new int[3];
                                            topleft = new int[3];
                                            mask = (1 << bits) - 1;
                                            gbc_buffer = buffer;
                                            gbc_buffer_end = buffer.length;
                                            gbc_index = 8 * bufp[0];
                                            gbc_size_in_bits = 8 * buffer.length;
                                            for (i = 0; i < 3; i++) {
                                                rgb_buffer[0][i] = (short)(1 << (bits + point_transform - 1));
                                            }
                                            for (mb_y = 0; mb_y < mb_height; mb_y++) {
                                                if (mb_y > 0) {
                                                    modified_predictor = predictor;
                                                }
                                                else {
                                                    modified_predictor = 1;
                                                }
                                                if (interlaced) {
                                                    ptr[0] = dataPtr[0] + (2 * linesize[0] * mb_y);
                                                }
                                                else {
                                                    ptr[0] = dataPtr[0] + (linesize[0] * mb_y);
                                                }
                                                
                                                if (interlaced && ((zs % 2) == 1)) {
                                                    ptr[0] += linesize[0];
                                                }
                                              
                                                for (i = 0; i < 3; i++) {
                                                    top[i] = left[i] = topleft[i] = rgb_buffer[0][i];
                                                }
                                                
                                                for (mb_x = 0; mb_x < mb_width; mb_x++) {
                                                    if ((restart_interval != 0) && (restart_count == 0)) {
                                                        restart_count = restart_interval;
                                                    }
                                                    
                                                    for (i = 0; i < 3; i++) {
                                                        topleft[i] = top[i];
                                                        top[i] = rgb_buffer[mb_x][i];
                                                        
                                                        switch(modified_predictor) {
                                                            case 1:
                                                                pred = left[i];
                                                                break;
                                                            case 2:
                                                                pred = top[i];
                                                                break;
                                                            case 3:
                                                                pred = topleft[i];
                                                                break;
                                                            case 4:
                                                                pred = left[i] + top[i] - topleft[i];
                                                                break;
                                                            case 5:
                                                                pred = left[i] + ((top[i] - topleft[i]) >> 1);
                                                                break;
                                                            case 6:
                                                                pred = top[i] + ((left[i] - topleft[i]) >> 1);
                                                                break;
                                                            case 7:
                                                                pred = (left[i] + top[i]) >> 1;
                                                                break;
                                                            default:
                                                                pred = (left[i] + top[i]) >> 1;
                                                        } // switch(modified_predictor)
                                                        
                                                        left[i] = rgb_buffer[mb_x][i] = (short)(mask & (pred + mjpeg_decode_dc(dc_index[i]) <<
                                                                  point_transform));
                                                    } // for (i = 0; i < 3; i++)
                                                    
                                                    if ((restart_interval != 0) && (--restart_count == 0)) {
                                                        n = (-gbc_index) & 0x07;
                                                        if (n != 0) {                                                          
                                                            // OPEN_READER(re, &s->gb)
                                                            int re_index= gbc_index;
                                                            //int re_cache= 0;
                                                            //  UPDATE_CACHE(name, gb)
                                                            //re_cache = (gbc_buffer[re_index>>3] & 0xff) << 24;
                                                            //re_cache |= (gbc_buffer[1 + re_index>>3] & 0xff) << 16;
                                                            //re_cache |= (gbc_buffer[2 + re_index>>3] & 0xff) << 8;
                                                            //re_cache |= (gbc_buffer[3 + re_index>>3] & 0xff);
                                                            //re_cache = re_cache << (re_index & 0x07);
                                                            // LAST_SKIP_BITS(name, gb, n)
                                                            re_index += n;
                                                            // CLOSE_READER(re, s);
                                                            gbc_index = re_index;
                                                        }
                                                        gbc_index += 16; // Skip RSTn
                                                    } // if ((restart_interval != 0) && (--restart_count == 0))
                                                } // for (mb_x = 0; mb_x < mb_width; mb_x++)
                                                
                                                if (rct) {
                                                    for (mb_x = 0; mb_x < mb_width; mb_x++) {
                                                        destBuf[ptr[0] + 4*mb_x + 2] = (byte)(rgb_buffer[mb_x][0] -
                                                               ((rgb_buffer[mb_x][1] + rgb_buffer[mb_x][2] - 0x200)>>> 2));
                                                        destBuf[ptr[0] + 4*mb_x + 1] = (byte)(rgb_buffer[mb_x][1] +
                                                                (destBuf[ptr[0] + 4*mb_x + 2] & 0xff));
                                                        destBuf[ptr[0] + 4*mb_x + 3] = (byte)(rgb_buffer[mb_x][2]  +
                                                                (destBuf[ptr[0] + 4*mb_x + 2] & 0xff));
                                                    }    
                                                }
                                                else if (pegasus_rct) {
                                                    for (mb_x = 0; mb_x < mb_width; mb_x++) {
                                                        destBuf[ptr[0] + 4*mb_x + 2] = (byte)(rgb_buffer[mb_x][0] -
                                                               ((rgb_buffer[mb_x][1] + rgb_buffer[mb_x][2])>>> 2));
                                                        destBuf[ptr[0] + 4*mb_x + 1] = (byte)(rgb_buffer[mb_x][1] +
                                                                (destBuf[ptr[0] + 4*mb_x + 2] & 0xff));
                                                        destBuf[ptr[0] + 4*mb_x + 3] = (byte)(rgb_buffer[mb_x][2]  +
                                                                (destBuf[ptr[0] + 4*mb_x + 2] & 0xff));
                                                    }        
                                                }
                                                else {
                                                    for (mb_x = 0; mb_x < mb_width; mb_x++) {
                                                        destBuf[ptr[0] + 4*mb_x + 1] = (byte)rgb_buffer[mb_x][0];
                                                        destBuf[ptr[0] + 4*mb_x + 2] = (byte)rgb_buffer[mb_x][1];
                                                        destBuf[ptr[0] + 4*mb_x + 3] = (byte)rgb_buffer[mb_x][2];
                                                    }
                                                }
                                            } // for (mb_y; mb_y < mb_height; mb_y++)
                                            bufp[0] = gbc_index/8;
                                        } // else if (srgb)
                                        else { // yuv
                                            
                                        }
                                    } // else if (lossless)
                                    else { // not lossless
                                        if (prev_shift == 0) {
                                            EOBRUN[0] = 0;
                                            for (i = 0; i < nb_components; i++) {
                                                c = comp_index[i];
                                                dataPtr[c] = 0;
                                                if (AMVCodec) {
                                                    dataPtr[c] = (linesize[c] * (v_scount[i] * (8 * mb_height - ((sofHeight/v_max) & 7)) - 1));
                                                    linesize[c] *= -1;
                                                }
                                            } // for (i = 0; i < nb_components; i++)
                                            
                                            Preferences.debug("mb_width = " + mb_width + "\n");
                                            Preferences.debug("mb_height = " + mb_height + "\n");
                                            gbc_buffer = buffer;
                                            gbc_buffer_end = buffer.length;
                                            gbc_index = 8 * bufp[0];
                                            gbc_size_in_bits = 8 * buffer.length;
                                            loopmby:
                                            for (mb_y = 0; mb_y < mb_height; mb_y++) {
                                                for (mb_x = 0; mb_x < mb_width; mb_x++) {
                                                    if ((restart_interval != 0) && (restart_count == 0)) {
                                                        restart_count = restart_interval;
                                                    }
                                                    
                                                    for (i = 0; i < nb_components; i++) {
                                                        n = nb_blocks[i];
                                                        c = comp_index[i];
                                                        h = h_scount[i];
                                                        v = v_scount[i];
                                                        x = 0;
                                                        y = 0;
                                                        for (js = 0; js < n; js++) {
                                                             for (k = 0; k < block.length; k++) {
                                                                 block[k] = 0;
                                                             }
                                                             if (!progressive && decode_block(block, i, dc_index[i], ac_index[i], 
                                                                 quant_matrixes[quant_index[c]]) < 0) {
                                                                 Preferences.debug("decode_block error for mb_x = " + mb_x +
                                                                                        " mb_y = " + mb_y + "\n");
                                                                 break loopmby;
                                                             } // if (!progressive && decode_block)
                                                             if (progressive && decode_block_progressive(block, i, dc_index[i],
                                                                 ac_index[i], quant_matrixes[quant_index[c]], predictor, ilv,
                                                                 prev_shift, point_transform, EOBRUN) < 0) {
                                                                 Preferences.debug("decode_block_progressive error for mb_x = " +
                                                                         mb_x + " mb_y = " + mb_y + "\n");
                                                                 break loopmby;
                                                             }
                                                             
                                                             if (interlaced) {
                                                                 ptr[0] = dataPtr[c] + (((2*linesize[c] * (v * mb_y + y) * 8) +
                                                                         (h * mb_x + x) * 8 ) >> lowres);    
                                                             }
                                                             else {
                                                                 ptr[0] = dataPtr[c] + (((linesize[c] * (v * mb_y + y) * 8) +
                                                                       (h * mb_x + x) * 8 ) >> lowres);
                                                             }
                                                             if (interlaced && ((zs % 2) == 1)) {
                                                                 ptr[0] += linesize[c];
                                                             }
                                                             if (!progressive) {
                                                                 if (interlaced) {
                                                                     ff_simple_idct_put(data[c], ptr, 2*linesize[c], block);
                                                                 }
                                                                 else {
                                                                     ff_simple_idct_put(data[c], ptr, linesize[c], block);
                                                                 }
                                                             }
                                                             else {
                                                                 if (interlaced) {
                                                                     ff_simple_idct_add(data[c], ptr, 2*linesize[c], block);
                                                                 }
                                                                 else {
                                                                     ff_simple_idct_add(data[c], ptr, linesize[c], block);
                                                                 }
                                                             }
                                                             if (++x == h) {
                                                                 x = 0;
                                                                 y++;
                                                             }
                                                        } // for (js = 0; js < n; js++)
                                                    } // for (i = 0; i < nb_components; i++)
                                                    
                                                    /* (< 1350) buggy workaround for Spectralfan.mov, should be fixed */
                                                    if ((restart_interval != 0) && (restart_interval < 1350) &&
                                                        ((--restart_count) == 0)) {
                                                        n = (-gbc_index) & 0x07;
                                                        if (n != 0) {                                                          
                                                            // OPEN_READER(re, &s->gb)
                                                            int re_index= gbc_index;
                                                            //int re_cache= 0;
                                                            //  UPDATE_CACHE(name, gb)
                                                            //re_cache = (gbc_buffer[re_index>>3] & 0xff) << 24;
                                                            //re_cache |= (gbc_buffer[1 + re_index>>3] & 0xff) << 16;
                                                            //re_cache |= (gbc_buffer[2 + re_index>>3] & 0xff) << 8;
                                                            //re_cache |= (gbc_buffer[3 + re_index>>3] & 0xff);
                                                            //re_cache = re_cache << (re_index & 0x07);
                                                            // LAST_SKIP_BITS(name, gb, n)
                                                            re_index += n;
                                                            // CLOSE_READER(re, s);
                                                            gbc_index = re_index;
                                                        }
                                                        gbc_index += 16; /* Skip RSTn */
                                                        for (i = 0; i < nb_components; i++) { /* reset dc */
                                                            last_dc[i] = 1024;
                                                        }
                                                    }
                                                } // for (mb_x = 0; mb_x < mb_width; mb_x++)
                                            } // for (mb_y = 0; mb_y < mb_height; mb_y++)
                                            bufp[0] = gbc_index/8;
                                            for (i = fieldPaddingBytes; i > 0; i--) {
                                                bufp[0]++;
                                            }
                                            
                                        } // if (prev_shift == 0)
                                    } 
                                    zs++;
                                    if ((sofHeight == imgExtents[1]) || ((sofHeight < imgExtents[1]) && ((zs % 2) == 0))) {
                                        switch(pix_fmt) {
                                            case PIX_FMT_YUVJ420P:
                                                yuvj420p_to_argb(destBuf, data, imgExtents[0], imgExtents[1],
                                                        linesize);
                                                break;
                                            case PIX_FMT_YUVJ422P:
                                                yuvj422p_to_argb(destBuf, data, imgExtents[0], imgExtents[1],
                                                        linesize);
                                                break;
                                        }
                                    
                                        if (one) {
                                            imageA.importData(0, destBuf, false);
                                        } else {
                                            imageA.importData(4 * z * imgExtents[0] * imgExtents[1], destBuf, false);
                                        }

                                        if (actualFrames > 1) {
                                            fireProgressStateChanged(100 * z / (imgExtents[2] - 1));
                                        }
                                        z++;
                                        if (z == imgExtents[2]) {
                                            break loopMJPG;
                                        }
                                    } // if ((sofHeight == imgExtents[1]) || ((sofHeight < imgExtents[1]) && ((zs % 2) == 0)))
                                    // emms_c();
                                } // else if (startCode == SOS)
                                else if ((startCode == SOF0) || (startCode == SOF2) || (startCode == SOF3)){
                                    if (startCode == SOF0) {
                                        lossless = false;
                                        ls = false;
                                        progressive = false;
                                    }
                                    else if (startCode == SOF2) {
                                        lossless = false;
                                        ls = false;
                                        progressive = true;
                                    }
                                    else if (startCode == SOF3) {
                                        lossless = true;
                                        ls = false;
                                        progressive = false;
                                    }
                                    if (j >= fileBuffer.length) {
                                        continue loopMJPG;
                                    }
                                    len = (fileBuffer[j++] & 0xff) << 8;
                                    len |= (fileBuffer[j++] & 0xff);
                                    Preferences.debug("len = " + len + "\n");
                                    bits = (fileBuffer[j++] & 0xff);
                                    Preferences.debug("bits = " + bits + "\n");
                                    if (bits > 128) {
                                        break loopMJPG;
                                    }
                                    if (pegasus_rct) {
                                        bits = 9;
                                    }
                                    if ((bits == 9) && (!pegasus_rct)) {
                                        rct = true;
                                    }
                                    
                                    if ((bits != 8) && (!lossless)) {
                                        Preferences.debug("Only 8 bits/component accepted\n");
                                        break loopMJPG;
                                    }
                                    
                                    sofHeight = (fileBuffer[j++] & 0xff) << 8;
                                    sofHeight |= (fileBuffer[j++] & 0xff);
                                    Preferences.debug("sofHeight = " + sofHeight + "\n");
                                    if (sofHeight > 2048) {
                                        break loopMJPG;
                                    }
                                    else if (sofHeight > imgExtents[1]) {
                                        imgExtents[1] = sofHeight;
                                        if (bitCount == 24) {
                                            bufferSize = 4 * imgExtents[0] * imgExtents[1];
                                        }
                                        else {
                                            bufferSize = imgExtents[0] * imgExtents[1];
                                        }
                                        imgBuffer = new byte[bufferSize];
                                        destBuf = new byte[4 * imgExtents[0] * imgExtents[1]];
                                        fileInfo.setExtents(imgExtents);

                                        if (one) {
                                            moreExtents = new int[2];
                                            moreExtents[0] = sofWidth;
                                            moreExtents[1] = sofHeight;
                                        } else {
                                            moreExtents = imgExtents;
                                        }

                                        if (bitCount == 24) {
                                            imageA = new ModelImage(ModelStorageBase.ARGB, moreExtents, fileName);
                                        } else {
                                            imageA = new ModelImage(ModelStorageBase.UBYTE, moreExtents, fileName);
                                        }
                                    } // if (sofHeight > imgExtents[1])
                                    
                                    sofWidth = (fileBuffer[j++] & 0xff) << 8;
                                    sofWidth |= (fileBuffer[j++] & 0xff);
                                    Preferences.debug("sofWidth = " + sofWidth + "\n");
                                    
                                    //HACK for odd_height.mov
                                    if (interlaced && (swidth == sofWidth) && (sheight == (sofHeight + 1))) {
                                        sofHeight = sheight;
                                    }
                                    
                                    if((sofWidth <= 0) || (sofHeight <= 0) || ((sofWidth+128)*(sofHeight+128) >= Integer.MAX_VALUE/4)) {
                                        MipavUtil.displayError("Illegal sofWidth and/or sofHeight value");
                                        return null;
                                    }
                                    
                                    nb_components = fileBuffer[j++] & 0xff;
                                    Preferences.debug("nb_components = " + nb_components + "\n");
                                    if ((nb_components <= 0) || (nb_components > MAX_COMPONENTS)) {
                                        MipavUtil.displayError("Illegal nb_components = " + nb_components);
                                        return null;
                                    }
                                    
                                    if (ls && !((bits <= 8) || (nb_components == 1))) {
                                        MipavUtil.displayError("Only 8 bits/component or 16-bit gray accepted for JPEG-LS");
                                        return null;
                                    }
                                    h_max = 1;
                                    v_max = 1;
                                    for (i = 0; i < nb_components; i++) {
                                        /* component id */ 
                                        component_id[i] = (fileBuffer[j++] & 0xff) - 1;
                                        h_count[i] = (fileBuffer[j] & 0xf0) >> 4;
                                        v_count[i] = (fileBuffer[j++] & 0x0f);
                                        /* compute hmax and vmax (only used in interleaved case) */
                                        if (h_count[i] > h_max) {
                                            h_max = h_count[i];
                                        }
                                        if (v_count[i] > v_max) {
                                            v_max = v_count[i];
                                        }
                                        quant_index[i] = (fileBuffer[j++] & 0xff);
                                        if (quant_index[i] >= 4) {
                                            MipavUtil.displayError("Illegal quant_index[" + i + "] = " + quant_index[i]);
                                            return null;
                                        }
                                    } // for (i = 0; i < nb_components; i++)
                                    
                                    if (ls && ((h_max > 1) || (v_max > 1))) {
                                        MipavUtil.displayError("Subsampling in JPEG-LS is not supported");
                                        return null;
                                    }
                                    
                                    if ((v_max == 1) && (h_max == 1) && lossless) {
                                       rgb = true;    
                                    }
                                    
                                    /* If different size, reallocate/allocate picture */
                                    if ((sofWidth != swidth) || (sofHeight != sheight)) {
                                        swidth =  sofWidth;
                                        sheight = sofHeight;
                                        interlaced = false;
                                        
                                        /* test interlaced mode */
                                        if (first_picture && (org_height != 0) && 
                                            (sheight < ((org_height * 3)/4))) {
                                            interlaced = true;
                                            Preferences.debug("interlaced\n");
                                            bottom_field = interlace_polarity;
                                        }
                                        
                                        swidth = -((-sofWidth )>>lowres);
                                        sheight= -((-sofHeight)>>lowres);
                                        first_picture = false;
                                    } // if ((width != swidth) || (height != sheight))
                                    
                                    if (interlaced && (bottom_field == !interlace_polarity)) {
                                        return null;
                                    }
                                    
                                    pix_fmt_id = (h_count[0] << 28) | (v_count[0] << 24) |
                                                 (h_count[1] << 20) | (v_count[1] << 16) |
                                                 (h_count[2] << 12) | (v_count[2] << 8) |
                                                 (h_count[3] << 4) | v_count[3];
                                    Preferences.debug("pix fmt id = " + pix_fmt_id + "\n");
                                    if ((pix_fmt_id & 0x10101010) == 0) {
                                        pix_fmt_id -= (pix_fmt_id & 0xF0F0F0F0)>>1;
                                    }
                                    if ((pix_fmt_id & 0x01010101) == 0) {
                                        pix_fmt_id -= (pix_fmt_id & 0x0F0F0F0F)>>1;
                                    }
                                    
                                    switch(pix_fmt_id) {
                                        case 0x11111100:
                                            if (rgb) {
                                                pix_fmt = PIX_FMT_RGB32;
                                            }
                                            else {
                                                pix_fmt = cs_itu601 ? PIX_FMT_YUV444P: PIX_FMT_YUVJ444P;
                                            }
                                            assert(nb_components == 3);
                                            break;
                                        case 0x11000000:
                                            pix_fmt = PIX_FMT_GRAY8;
                                            break;
                                        case 0x12111100:
                                            pix_fmt = cs_itu601 ? PIX_FMT_YUV440P: PIX_FMT_YUVJ440P;
                                            break;
                                        case 0x21111100:
                                            pix_fmt = cs_itu601 ? PIX_FMT_YUV422P: PIX_FMT_YUVJ422P;
                                            break;
                                        case 0x22111100:
                                            pix_fmt = cs_itu601 ? PIX_FMT_YUV420P: PIX_FMT_YUVJ420P;
                                            break;
                                        default:
                                            MipavUtil.displayError("Unhandled pixel format = " + pix_fmt_id);
                                    }
                                    if (ls) {
                                        if (nb_components > 1) {
                                            pix_fmt = PIX_FMT_RGB24;
                                        }
                                        else if (bits <= 8) {
                                            pix_fmt = PIX_FMT_GRAY8;
                                        }
                                        else if (endianess){
                                            pix_fmt = PIX_FMT_GRAY16BE;
                                        }
                                        else {
                                            pix_fmt = PIX_FMT_GRAY16LE;
                                        }
                                    }
                                    switch(pix_fmt) {
                                        case PIX_FMT_RGB32:
                                            Preferences.debug("pix_fmt = PIX_FMT_RGB32\n");
                                            linesize[0] = imgExtents[0] * 4;
                                            pSize = linesize[0] * imgExtents[1];
                                            if ((data[0] == null) || (data[0].length != pSize)) {
                                                data[0] = new byte[pSize];
                                            }
                                            break;
                                        case PIX_FMT_YUV444P:
                                            Preferences.debug("pix_fmt = PIX_FMT_YUV444P\n");
                                            x_chroma_shift = 0;
                                            y_chroma_shift = 0;
                                            w2 = (imgExtents[0] + (1 << x_chroma_shift) - 1) >> x_chroma_shift;
                                            linesize[0] = imgExtents[0];
                                            linesize[1] = w2;
                                            linesize[2] = w2;
                                            pSize = linesize[0] * imgExtents[1];
                                            h2 = (imgExtents[1]+ (1 << y_chroma_shift) - 1) >> y_chroma_shift;
                                            size2 = linesize[1] * h2;
                                            if ((data[0] == null) || (data[0].length != pSize)) {
                                                data[0] = new byte[pSize];
                                            }
                                            if ((data[1] == null) || (data[1].length != size2)) {
                                                data[1] = new byte[size2];
                                            }
                                            if ((data[2] == null) || (data[2].length != size2)) {
                                                data[2] = new byte[size2];
                                            }
                                            break;
                                        case PIX_FMT_YUVJ444P:
                                            Preferences.debug("pix_fmt = PIX_FMT_YUVJ444P\n");
                                            x_chroma_shift = 0;
                                            y_chroma_shift = 0;
                                            w2 = (imgExtents[0] + (1 << x_chroma_shift) - 1) >> x_chroma_shift;
                                            linesize[0] = imgExtents[0];
                                            linesize[1] = w2;
                                            linesize[2] = w2;
                                            pSize = linesize[0] * imgExtents[1];
                                            h2 = (imgExtents[1]+ (1 << y_chroma_shift) - 1) >> y_chroma_shift;
                                            size2 = linesize[1] * h2;
                                            if ((data[0] == null) || (data[0].length != pSize)) {
                                                data[0] = new byte[pSize];
                                            }
                                            if ((data[1] == null) || (data[1].length != size2)) {
                                                data[1] = new byte[size2];
                                            }
                                            if ((data[2] == null) || (data[2].length != size2)) {
                                                data[2] = new byte[size2];
                                            }
                                            break;
                                        case PIX_FMT_GRAY8:
                                            Preferences.debug("pix_fmt = PIX_FMT_GRAY8\n");
                                            linesize[0] = imgExtents[0];
                                            pSize = linesize[0] * imgExtents[1];
                                            if ((data[0] == null) || (data[0].length != pSize)) {
                                                data[0] = new byte[pSize];
                                            }
                                            break;
                                        case PIX_FMT_YUV440P:
                                            Preferences.debug("pix_fmt = PIX_FMT_YUV440P\n");
                                            x_chroma_shift = 0;
                                            y_chroma_shift = 1;
                                            w2 = (imgExtents[0] + (1 << x_chroma_shift) - 1) >> x_chroma_shift;
                                            linesize[0] = imgExtents[0];
                                            linesize[1] = w2;
                                            linesize[2] = w2;
                                            pSize = linesize[0] * imgExtents[1];
                                            h2 = (imgExtents[1]+ (1 << y_chroma_shift) - 1) >> y_chroma_shift;
                                            size2 = linesize[1] * h2;
                                            if ((data[0] == null) || (data[0].length != pSize)) {
                                                data[0] = new byte[pSize];
                                            }
                                            if ((data[1] == null) || (data[1].length != size2)) {
                                                data[1] = new byte[size2];
                                            }
                                            if ((data[2] == null) || (data[2].length != size2)) {
                                                data[2] = new byte[size2];
                                            }
                                            break;
                                        case PIX_FMT_YUVJ440P:
                                            Preferences.debug("pix_fmt = PIX_FMT_YUVJ440P\n");
                                            x_chroma_shift = 0;
                                            y_chroma_shift = 1;
                                            w2 = (imgExtents[0] + (1 << x_chroma_shift) - 1) >> x_chroma_shift;
                                            linesize[0] = imgExtents[0];
                                            linesize[1] = w2;
                                            linesize[2] = w2;
                                            pSize = linesize[0] * imgExtents[1];
                                            h2 = (imgExtents[1]+ (1 << y_chroma_shift) - 1) >> y_chroma_shift;
                                            size2 = linesize[1] * h2;
                                            if ((data[0] == null) || (data[0].length != pSize)) {
                                                data[0] = new byte[pSize];
                                            }
                                            if ((data[1] == null) || (data[1].length != size2)) {
                                                data[1] = new byte[size2];
                                            }
                                            if ((data[2] == null) || (data[2].length != size2)) {
                                                data[2] = new byte[size2];
                                            }
                                            break;
                                        case PIX_FMT_YUV422P:
                                            Preferences.debug("pix_fmt = PIX_FMT_YUV422P\n");
                                            x_chroma_shift = 1;
                                            y_chroma_shift = 0;
                                            w2 = (imgExtents[0] + (1 << x_chroma_shift) - 1) >> x_chroma_shift;
                                            linesize[0] = imgExtents[0];
                                            linesize[1] = w2;
                                            linesize[2] = w2;
                                            pSize = linesize[0] * imgExtents[1];
                                            h2 = (imgExtents[1]+ (1 << y_chroma_shift) - 1) >> y_chroma_shift;
                                            size2 = linesize[1] * h2;
                                            if ((data[0] == null) || (data[0].length != pSize)) {
                                                data[0] = new byte[pSize];
                                            }
                                            if ((data[1] == null) || (data[1].length != size2)) {
                                                data[1] = new byte[size2];
                                            }
                                            if ((data[2] == null) || (data[2].length != size2)) {
                                                data[2] = new byte[size2];
                                            }
                                            break;
                                        case PIX_FMT_YUVJ422P:
                                            Preferences.debug("pix_fmt = PIX_FMT_YUVJ422P\n");
                                            x_chroma_shift = 1;
                                            y_chroma_shift = 0;
                                            w2 = (imgExtents[0] + (1 << x_chroma_shift) - 1) >> x_chroma_shift;
                                            linesize[0] = imgExtents[0];
                                            linesize[1] = w2;
                                            linesize[2] = w2;
                                            pSize = linesize[0] * imgExtents[1];
                                            h2 = (imgExtents[1]+ (1 << y_chroma_shift) - 1) >> y_chroma_shift;
                                            size2 = linesize[1] * h2;
                                            if ((data[0] == null) || (data[0].length != pSize)) {
                                                data[0] = new byte[pSize];
                                            }
                                            if ((data[1] == null) || (data[1].length != size2)) {
                                                data[1] = new byte[size2];
                                            }
                                            if ((data[2] == null) || (data[2].length != size2)) {
                                                data[2] = new byte[size2];
                                            }
                                            break;
                                        case PIX_FMT_YUV420P:
                                            Preferences.debug("pix_fmt = PIX_FMT_YUV420P\n");
                                            x_chroma_shift = 1;
                                            y_chroma_shift = 1;
                                            w2 = (imgExtents[0] + (1 << x_chroma_shift) - 1) >> x_chroma_shift;
                                            linesize[0] = imgExtents[0];
                                            linesize[1] = w2;
                                            linesize[2] = w2;
                                            pSize = linesize[0] * imgExtents[1];
                                            h2 = (imgExtents[1]+ (1 << y_chroma_shift) - 1) >> y_chroma_shift;
                                            size2 = linesize[1] * h2;
                                            if ((data[0] == null) || (data[0].length != pSize)) {
                                                data[0] = new byte[pSize];
                                            }
                                            if ((data[1] == null) || (data[1].length != size2)) {
                                                data[1] = new byte[size2];
                                            }
                                            if ((data[2] == null) || (data[2].length != size2)) {
                                                data[2] = new byte[size2];
                                            }
                                            break;
                                        case PIX_FMT_YUVJ420P:
                                            Preferences.debug("pix_fmt = PIX_FMT_YUVJ420P\n");
                                            x_chroma_shift = 1;
                                            y_chroma_shift = 1;
                                            w2 = (imgExtents[0] + (1 << x_chroma_shift) - 1) >> x_chroma_shift;
                                            Preferences.debug("w2 = " + w2 + "\n");
                                            linesize[0] = imgExtents[0];
                                            linesize[1] = w2;
                                            linesize[2] = w2;
                                            pSize = linesize[0] * imgExtents[1];
                                            h2 = (imgExtents[1]+ (1 << y_chroma_shift) - 1) >> y_chroma_shift;
                                            Preferences.debug("h2 = " + h2 + "\n");
                                            size2 = linesize[1] * h2;
                                            Preferences.debug("size2 = " + size2 + "\n");
                                            if ((data[0] == null) || (data[0].length != pSize)) {
                                                data[0] = new byte[pSize];
                                            }
                                            if ((data[1] == null) || (data[1].length != size2)) {
                                                data[1] = new byte[size2];
                                            }
                                            if ((data[2] == null) || (data[2].length != size2)) {
                                                data[2] = new byte[size2];
                                            }
                                            break;
                                        case PIX_FMT_RGB24:
                                            Preferences.debug("pix_fmt = PIX_FMT_RGB24\n");
                                            linesize[0] = 3 * imgExtents[0];
                                            pSize = linesize[0] * imgExtents[1];
                                            if ((data[0] == null) || (data[0].length != pSize)) {
                                                data[0] = new byte[pSize];
                                            }
                                            break;
                                        case PIX_FMT_GRAY16BE:
                                            Preferences.debug("pix_fmt = PIX_FMT_GRAY16BE\n");
                                            linesize[0] = 2 * imgExtents[0];
                                            pSize = linesize[0] * imgExtents[1];
                                            if ((data[0] == null) || (data[0].length != pSize)) {
                                                data[0] = new byte[pSize];
                                            }
                                            break;
                                        case PIX_FMT_GRAY16LE:
                                            Preferences.debug("pix_fmt = PIX_FMT_GRAY16LE\n");
                                            linesize[0] = 2 * imgExtents[0];
                                            pSize = linesize[0] * imgExtents[1];
                                            if ((data[0] == null) || (data[0].length != pSize)) {
                                                data[0] = new byte[pSize];
                                            }
                                            break;
                                    }
                                    
                                    if (len != (8+(3*nb_components))) {
                                        MipavUtil.displayError("decode_sof0 error, len = " + len);
                                        return null;
                                    }
                                } // else if (startCode == SOF0)
                                else if (startCode == DHT) {
                                    bits_table = new byte[17];
                                    val_table = new byte[256];
                                    len = (fileBuffer[j++] & 0xff) << 8;
                                    len |= (fileBuffer[j++] & 0xff);
                                    len -= 2;
                                    
                                    while (len > 0) {
                                        if (len < 17) {
                                            MipavUtil.displayError("len < 17 for startCode == DHT");
                                            return null;
                                        }
                                        dhtClass = (fileBuffer[j] & 0xf0) >> 4;
                                        if (dhtClass >= 2) {
                                            MipavUtil.displayError("dhtClass >= 2 for startCode == DHT");
                                            return null;
                                        }
                                        index = (fileBuffer[j++] & 0x0f);
                                        if (index >= 4) {
                                            Preferences.debug("index >= 4 for startCode == DHT\n");
                                            break loopMJPG;
                                        }
                                        n = 0;
                                        for (i = 1; i <= 16; i++) {
                                            bits_table[i] = fileBuffer[j++];
                                            n += (bits_table[i] & 0xff);
                                        }
                                        len -= 17;
                                        if (len < n) {
                                            MipavUtil.displayError("len < n for startCode == DHT");
                                            return null;
                                        }
                                        if (n > 256) {
                                            MipavUtil.displayError("n > 256 for startCode == DHT");
                                            return null;
                                        }
                                        code_max = 0;
                                        for (i = 0; i < n; i++) {
                                            v = (fileBuffer[j] & 0xff);
                                            if (v > code_max) {
                                                code_max = v;
                                            }
                                            val_table[i] = fileBuffer[j++];
                                        } // for (i = 0; i < n; i++)
                                        len -= n;
                                        
                                        /* build VLC and flush previous vlc if present */
                                        vlcs[dhtClass][index] = null;
                                        vlcs_bits[dhtClass][index] = 0;
                                        vlcs_table_size[dhtClass][index] = 0;
                                        vlcs_table_allocated[dhtClass][index] = 0;
                                        if (build_vlc(dhtClass, index, bits_table, val_table, code_max + 1, (dhtClass > 0)) < 0) {
                                            MipavUtil.displayError("build_vlc failed for startCode == DHT");
                                            return null;
                                        }
                                    } // while (len > 0)
                                } // else if (startCode == DHT)
                                else if ((startCode >= APP0) && (startCode <= APP15)) {
                                    if (j >= fileBuffer.length) {
                                        continue loopMJPG;
                                    }
                                    len = (fileBuffer[j++] & 0xff) << 8;
                                    len |= (fileBuffer[j++] & 0xff);
                                    Preferences.debug("len = " + len + "\n");
                                    app0[0] = fileBuffer[j++];
                                    app0[1] = fileBuffer[j++];
                                    app0[2] = fileBuffer[j++];
                                    app0[3] = fileBuffer[j++];
                                    len -= 6;
                                    id = new String(app0);
                                    Preferences.debug("APP0 marker = " + id + "\n");
                                    if (id.equalsIgnoreCase("AVI1")) {
                                        // 4 bytes AVI
                                        // 1 byte polarity
                                        // 1 byte always zero
                                        // 4 bytes fieldSize
                                        // 4 bytes fieldSizeLessPadding
                                        polarity = fileBuffer[j++];
                                        Preferences.debug("polarity = " + polarity + "\n");
                                        if (polarity == 2) {
                                            bottomField = 1;
                                        }
                                        else if (polarity == 1) {
                                            bottomField = 0;
                                        }
                                        j++; // Skip zero byte
                                        fieldSize = (fileBuffer[j++] & 0xff) << 24;
                                        fieldSize |= (fileBuffer[j++] & 0xff) << 16;
                                        fieldSize |= (fileBuffer[j++] & 0xff) << 8;
                                        fieldSize |= (fileBuffer[j++] & 0xff);
                                        Preferences.debug("fieldSize = " + fieldSize + "\n");
                                        fieldSizeLessPadding = (fileBuffer[j++] & 0xff) << 24;
                                        fieldSizeLessPadding |= (fileBuffer[j++] & 0xff) << 16;
                                        fieldSizeLessPadding |= (fileBuffer[j++] & 0xff) << 8;
                                        fieldSizeLessPadding |= (fileBuffer[j++] & 0xff);
                                        len -= 10;
                                        Preferences.debug("fieldSizeLessPadding = " + fieldSizeLessPadding + "\n");
                                        fieldPaddingBytes = fieldSize - fieldSizeLessPadding;
                                        if ((fieldPaddingBytes < 0) || (fieldPaddingBytes > 255)) {
                                            fieldPaddingBytes = 0;
                                        }
                                    } // if (id.equalsIgnoreCase("AVI1")
                                    else if (id.equalsIgnoreCase("LJIF")) {
                                        Preferences.debug("Pegasus lossless jpeg header found\n");
                                        j += 8; // Skip 8 bytes
                                        colorSpace = (fileBuffer[j++] & 0xff);
                                        switch (colorSpace) {
                                            case 1:
                                                srgb = true;
                                                pegasus_rct = false;
                                                Preferences.debug("pegasus_rct = false\n");
                                                break;
                                            case 2:
                                                srgb = true;
                                                pegasus_rct = true;
                                                Preferences.debug("pegasus_rct = true\n");
                                                break;
                                            default:
                                                MipavUtil.displayError("Unknown color space\n");
                                        }
                                        len -= 9;
                                    } // else if (id.equalsIgnoreCase("LJIF"))
                                    if (--len > 0) {
                                        j++;
                                    }
                                } // else if ((startCode >= APP0) && (startCode <= APP15))
                                else if (startCode == COM) {
                                    len = (fileBuffer[j++] & 0xff) << 8;
                                    len |= (fileBuffer[j++] & 0xff);
                                    if (len >= 2) {
                                        cbuf = new byte[len - 1];
                                        for (i = 0; i < len - 2; i++) {
                                            cbuf[i] = fileBuffer[j++];
                                        }
                                        if (i > 0 && cbuf[i-1] == '\n') {
                                            cbuf[i-1] = 0;
                                        }
                                        else {
                                            cbuf[i] = 0;
                                        }
                                        comStr = new String(cbuf);
                                        Preferences.debug("Comment = " + comStr + "\n");
                                        if (comStr.equalsIgnoreCase("AVID")) {
                                            buggy_avid = true;
                                        }
                                        else if (comStr.equalsIgnoreCase("CS=ITU601")) {
                                            cs_itu601 = true;
                                        }
                                    }
                                } // else if (startCode == COM)
                                else {
                                    
                                }
                                
                                /*for (y = 0; y < imgExtents[1]; y++) {
                                    for (x = 0; x < imgExtents[0]; x ++) {
                                        
                                       pixelIndex++;  
                                        //Check if decode is finished
                                       if ((pixelIndex == imgExtents[0]*imgExtents[1]) || (j == dataLength)) {

                                           bufferFinished = true;

                                           if (one) {
                                               imageA.importData(0, imgBuffer, false);
                                           } else {
                                               imageA.importData(z * bufferSize, imgBuffer, false);
                                           }

                                           if (actualFrames > 1) {
                                               fireProgressStateChanged(100 * z / (imgExtents[2] - 1));
                                           }
                                           z++;
                                       }
                                        
                                    } // for (x = 0; x < imgExtents[0]; x ++)
                                } // for (y = 0; y < imgExtents[1]; y++)*/

                                
                            } // for (int j = 0; (j < dataLength) && (!bufferFinished);)
                    } // else 
                    } // if ((dataSignature[2] == 0x64 /* d */) && (dataSignature[3] == 0x63 /* c */))
                    else {
                        dataLength = getInt(endianess);

                        if ((dataLength % 2) == 1) {
                            dataLength++;
                        }

                        totalBytesRead = totalBytesRead + 4;
                        subchunkBytesRead += 4;

                        long lptr = raFile.getFilePointer();
                        raFile.seek(lptr + dataLength);
                        totalBytesRead = totalBytesRead + dataLength;
                        subchunkBytesRead += dataLength;
                    } // else

                    subchunkBlocksRead++;

                    if (haveMoviSubchunk && (subchunkBlocksRead == streams) && (totalBytesRead < totalDataArea)) {
                        totalBytesRead += moviSubchunkPosition + LIST2subchunkSize - raFile.getFilePointer();
                        raFile.seek(moviSubchunkPosition + LIST2subchunkSize);

                        // Check for LIST rec<sp> subchunks
                        signature = getInt(endianess);
                        totalBytesRead += 4;

                        if (signature == 0x5453494C) {

                            // have read LIST
                            LIST2subchunkSize = getInt(endianess);
                            totalBytesRead += 4;
                            moviSubchunkPosition = raFile.getFilePointer();
                            CHUNKtype = getInt(endianess);

                            if (CHUNKtype == 0x20636572) {

                                // have read rec<sp>
                                totalBytesRead += 4;
                                subchunkDataArea = LIST2subchunkSize - 4;
                                subchunkBytesRead = 0;
                                subchunkBlocksRead = 0;
                            } else {
                                raFile.close();
                                throw new IOException("CHunktype for LIST2sbuchunk is an illegal = " + CHUNKtype);
                            }
                        } else {
                            chunkRead = false;
                        }
                    } // if (haveMoviSubchunk && (subchunkBlocksRead == streams))
                } // while ((totalBytesRead < totalDataArea) && chunkRead)     
            } // else if (doMJPEG && (bitCount == 24))

            raFile.close();

            if (wasCompressed) {
                FileDeleter fd = new FileDeleter(file.getPath());
                fd.start();
            }

            if (one) {
                imageA.setFileInfo(fileInfo, 0);
            } else {

                for (int i = 0; i < actualFrames; i++) {
                    imageA.setFileInfo(fileInfo, i);
                }
            }


            return imageA;

        } catch (OutOfMemoryError error) {

            if (imageA != null) {
                imageA.disposeLocal();
                imageA = null;
            }

            System.gc();

            throw error;
        }
    }
    
    private void yuvj422p_to_argb(byte dst[], byte data[][], int width, int height, int slinesize[]) {
        int width2;
        int dlinesize = 4 * width;
        int d = 0;
        int d1;
        int w;
        byte y_data[] = data[0];
        int y1_ptr = 0;
        byte cb_data[] = data[1];
        int cb_ptr = 0;
        byte cr_data[] = data[2];
        int cr_ptr = 0;
        int y;
        int cb;
        int cr;
        int SCALEBITS = 10;
        int ONE_HALF = (1 << (SCALEBITS - 1));
        int r_add;
        int g_add;
        int b_add;
        width2 = (width + 1) >> 1;
        for (; height >= 1; height--) {
            d1 = d;
            for (w = width; w >= 2; w-= 2) {
              cb = (cb_data[cb_ptr] & 0xff) - 128;
              cr = (cr_data[cr_ptr] & 0xff) - 128;
              r_add = ((int) ((1.402) * (1 << SCALEBITS) + 0.5)) * cr + ONE_HALF;
              g_add = -((int) ((0.34414) * (1 << SCALEBITS) + 0.5)) * cb 
                      -((int) ((0.71414) * (1 << SCALEBITS) + 0.5)) * cr + ONE_HALF;
              b_add = ((int) ((1.772) * (1 << SCALEBITS) + 0.5)) * cb + ONE_HALF;
              /* output 4 pixels */
              y = (y_data[y1_ptr] & 0xff) << SCALEBITS;
              dst[d1] = 0;
              dst[d1 + 1] = ff_cropTbl[MAX_NEG_CROP + ((y + r_add) >> SCALEBITS)];
              dst[d1 + 2] = ff_cropTbl[MAX_NEG_CROP + ((y + g_add) >> SCALEBITS)];
              dst[d1 + 3] = ff_cropTbl[MAX_NEG_CROP + ((y + b_add) >> SCALEBITS)];
              
              y = (y_data[y1_ptr+1] & 0xff) << SCALEBITS;
              dst[d1 + 4] = 0;
              dst[d1 + 5] = ff_cropTbl[MAX_NEG_CROP + ((y + r_add) >> SCALEBITS)];
              dst[d1 + 6] = ff_cropTbl[MAX_NEG_CROP + ((y + g_add) >> SCALEBITS)];
              dst[d1 + 7] = ff_cropTbl[MAX_NEG_CROP + ((y + b_add) >> SCALEBITS)];
              
              d1 += 8;
              
              y1_ptr += 2;
              cb_ptr++;
              cr_ptr++;
            } // for (w = width; w >= 2; w -=2)
            /* handle odd width */
            if (w != 0) {
                cb = (cb_data[cb_ptr] & 0xff) - 128;
                cr = (cr_data[cr_ptr] & 0xff) - 128;
                r_add = ((int) ((1.402) * (1 << SCALEBITS) + 0.5)) * cr + ONE_HALF;
                g_add = -((int) ((0.34414) * (1 << SCALEBITS) + 0.5)) * cb 
                        -((int) ((0.71414) * (1 << SCALEBITS) + 0.5)) * cr + ONE_HALF;
                b_add = ((int) ((1.772) * (1 << SCALEBITS) + 0.5)) * cb + ONE_HALF;
                y = (y_data[y1_ptr] & 0xff) << SCALEBITS;
                dst[d1] = 0;
                dst[d1 + 1] = ff_cropTbl[MAX_NEG_CROP + ((y + r_add) >> SCALEBITS)];
                dst[d1 + 2] = ff_cropTbl[MAX_NEG_CROP + ((y + g_add) >> SCALEBITS)];
                dst[d1 + 3] = ff_cropTbl[MAX_NEG_CROP + ((y + b_add) >> SCALEBITS)];
                
                d1 += 4;
                y1_ptr++;
                cb_ptr++;
                cr_ptr++;
            } // if (w != 0)
            d += dlinesize;
            y1_ptr += slinesize[0] - width;
            cb_ptr += slinesize[1] - width2;
            cr_ptr += slinesize[2] - width2;
        } // for (; height >= 1; height--)
    }
    
    private void yuvj420p_to_argb(byte dst[], byte data[][], int width, int height, int slinesize[]) {
        int width2;
        int dlinesize = 4 * width;
        int d = 0;
        int d1;
        int d2;
        int y2_ptr;
        int w;
        byte y_data[] = data[0];
        int y1_ptr = 0;
        byte cb_data[] = data[1];
        int cb_ptr = 0;
        byte cr_data[] = data[2];
        int cr_ptr = 0;
        int y;
        int cb;
        int cr;
        int SCALEBITS = 10;
        int ONE_HALF = (1 << (SCALEBITS - 1));
        int r_add;
        int g_add;
        int b_add;
        width2 = (width + 1) >> 1;
        for (; height >= 2; height -= 2) {
            d1 = d;
            d2 = d + dlinesize;
            y2_ptr = y1_ptr + slinesize[0];
            for (w = width; w >= 2; w-= 2) {
              cb = (cb_data[cb_ptr] & 0xff) - 128;
              cr = (cr_data[cr_ptr] & 0xff) - 128;
              r_add = ((int) ((1.402) * (1 << SCALEBITS) + 0.5)) * cr + ONE_HALF;
              g_add = -((int) ((0.34414) * (1 << SCALEBITS) + 0.5)) * cb 
                      -((int) ((0.71414) * (1 << SCALEBITS) + 0.5)) * cr + ONE_HALF;
              b_add = ((int) ((1.772) * (1 << SCALEBITS) + 0.5)) * cb + ONE_HALF;
              /* output 4 pixels */
              y = (y_data[y1_ptr] & 0xff) << SCALEBITS;
              dst[d1] = 0;
              dst[d1 + 1] = ff_cropTbl[MAX_NEG_CROP + ((y + r_add) >> SCALEBITS)];
              dst[d1 + 2] = ff_cropTbl[MAX_NEG_CROP + ((y + g_add) >> SCALEBITS)];
              dst[d1 + 3] = ff_cropTbl[MAX_NEG_CROP + ((y + b_add) >> SCALEBITS)];
              
              y = (y_data[y1_ptr+1] & 0xff) << SCALEBITS;
              dst[d1 + 4] = 0;
              dst[d1 + 5] = ff_cropTbl[MAX_NEG_CROP + ((y + r_add) >> SCALEBITS)];
              dst[d1 + 6] = ff_cropTbl[MAX_NEG_CROP + ((y + g_add) >> SCALEBITS)];
              dst[d1 + 7] = ff_cropTbl[MAX_NEG_CROP + ((y + b_add) >> SCALEBITS)];
              
              y = (y_data[y2_ptr] & 0xff) << SCALEBITS;
              dst[d2] = 0;
              dst[d2 + 1] = ff_cropTbl[MAX_NEG_CROP + ((y + r_add) >> SCALEBITS)];
              dst[d2 + 2] = ff_cropTbl[MAX_NEG_CROP + ((y + g_add) >> SCALEBITS)];
              dst[d2 + 3] = ff_cropTbl[MAX_NEG_CROP + ((y + b_add) >> SCALEBITS)];
              
              y = (y_data[y2_ptr+1] & 0xff) << SCALEBITS;
              dst[d2 + 4] = 0;
              dst[d2 + 5] = ff_cropTbl[MAX_NEG_CROP + ((y + r_add) >> SCALEBITS)];
              dst[d2 + 6] = ff_cropTbl[MAX_NEG_CROP + ((y + g_add) >> SCALEBITS)];
              dst[d2 + 7] = ff_cropTbl[MAX_NEG_CROP + ((y + b_add) >> SCALEBITS)];
              
              d1 += 8;
              d2 += 8;
              
              y1_ptr += 2;
              y2_ptr += 2;
              cb_ptr++;
              cr_ptr++;
            } // for (w = width; w >= 2; w -=2)
            /* handle odd width */
            if (w != 0) {
                cb = (cb_data[cb_ptr] & 0xff) - 128;
                cr = (cr_data[cr_ptr] & 0xff) - 128;
                r_add = ((int) ((1.402) * (1 << SCALEBITS) + 0.5)) * cr + ONE_HALF;
                g_add = -((int) ((0.34414) * (1 << SCALEBITS) + 0.5)) * cb 
                        -((int) ((0.71414) * (1 << SCALEBITS) + 0.5)) * cr + ONE_HALF;
                b_add = ((int) ((1.772) * (1 << SCALEBITS) + 0.5)) * cb + ONE_HALF;
                y = (y_data[y1_ptr] & 0xff) << SCALEBITS;
                dst[d1] = 0;
                dst[d1 + 1] = ff_cropTbl[MAX_NEG_CROP + ((y + r_add) >> SCALEBITS)];
                dst[d1 + 2] = ff_cropTbl[MAX_NEG_CROP + ((y + g_add) >> SCALEBITS)];
                dst[d1 + 3] = ff_cropTbl[MAX_NEG_CROP + ((y + b_add) >> SCALEBITS)];
                
                y = (y_data[y2_ptr] & 0xff) << SCALEBITS;
                dst[d2] = 0;
                dst[d2 + 1] = ff_cropTbl[MAX_NEG_CROP + ((y + r_add) >> SCALEBITS)];
                dst[d2 + 2] = ff_cropTbl[MAX_NEG_CROP + ((y + g_add) >> SCALEBITS)];
                dst[d2 + 3] = ff_cropTbl[MAX_NEG_CROP + ((y + b_add) >> SCALEBITS)];
                
                d1 += 4;
                d2 += 4;
                y1_ptr++;
                y2_ptr++;
                cb_ptr++;
                cr_ptr++;
            } // if (w != 0)
            d += 2 * dlinesize;
            y1_ptr += 2 * slinesize[0] - width;
            cb_ptr += slinesize[1] - width2;
            cr_ptr += slinesize[2] - width2;
        } // for (; height >= 2; height -=2)
        /* handle odd height */
        if (height != 0) {
            d1 = d;
            for (w = width; w >= 2; w -= 2) {
                cb = (cb_data[cb_ptr] & 0xff) - 128;
                cr = (cr_data[cr_ptr] & 0xff) - 128;
                r_add = ((int) ((1.402) * (1 << SCALEBITS) + 0.5)) * cr + ONE_HALF;
                g_add = -((int) ((0.34414) * (1 << SCALEBITS) + 0.5)) * cb 
                        -((int) ((0.71414) * (1 << SCALEBITS) + 0.5)) * cr + ONE_HALF;
                b_add = ((int) ((1.772) * (1 << SCALEBITS) + 0.5)) * cb + ONE_HALF;
                /* output 2 pixels */
                y = (y_data[y1_ptr] & 0xff) << SCALEBITS;
                dst[d1] = 0;
                dst[d1 + 1] = ff_cropTbl[MAX_NEG_CROP + ((y + r_add) >> SCALEBITS)];
                dst[d1 + 2] = ff_cropTbl[MAX_NEG_CROP + ((y + g_add) >> SCALEBITS)];
                dst[d1 + 3] = ff_cropTbl[MAX_NEG_CROP + ((y + b_add) >> SCALEBITS)];
                
                y = (y_data[y1_ptr+1] & 0xff) << SCALEBITS;
                dst[d1 + 4] = 0;
                dst[d1 + 5] = ff_cropTbl[MAX_NEG_CROP + ((y + r_add) >> SCALEBITS)];
                dst[d1 + 6] = ff_cropTbl[MAX_NEG_CROP + ((y + g_add) >> SCALEBITS)];
                dst[d1 + 7] = ff_cropTbl[MAX_NEG_CROP + ((y + b_add) >> SCALEBITS)];
                
                d1 += 8;
                y1_ptr += 2;
                cb_ptr++;
                cr_ptr++;
            } // for (w = width; w >= 2; w -= 2)
            /* handle off width */
            if (w != 0) {
                cb = (cb_data[cb_ptr] & 0xff) - 128;
                cr = (cr_data[cr_ptr] & 0xff) - 128;
                r_add = ((int) ((1.402) * (1 << SCALEBITS) + 0.5)) * cr + ONE_HALF;
                g_add = -((int) ((0.34414) * (1 << SCALEBITS) + 0.5)) * cb 
                        -((int) ((0.71414) * (1 << SCALEBITS) + 0.5)) * cr + ONE_HALF;
                b_add = ((int) ((1.772) * (1 << SCALEBITS) + 0.5)) * cb + ONE_HALF;
                /* output 1 pixel */
                y = (y_data[y1_ptr] & 0xff) << SCALEBITS;
                dst[d1] = 0;
                dst[d1 + 1] = ff_cropTbl[MAX_NEG_CROP + ((y + r_add) >> SCALEBITS)];
                dst[d1 + 2] = ff_cropTbl[MAX_NEG_CROP + ((y + g_add) >> SCALEBITS)];
                dst[d1 + 3] = ff_cropTbl[MAX_NEG_CROP + ((y + b_add) >> SCALEBITS)];
                
                d1 += 4;
                y1_ptr++;
                cb_ptr++;
                cr_ptr++;
            } // if (w != 0)
        } // if (height != 0)
    }
    
    private void ff_simple_idct_put(byte dest[], int destPtr[], int line_size, short block[]) {
        int i;
        long row0;
        long row1;
        long temp;
        int a0, a1, a2, a3, b0, b1, b2, b3;
        int destOrg = destPtr[0];
        for (i = 0; i < 8; i++) {
            row0 = (block[i*8] & 0xffffL) << 48;
            row0 |= (block[i*8+1] & 0xffffL) << 32;
            row0 |= (block[i*8+2] & 0xffffL) << 16;
            row0 |= (block[i*8+3] & 0xffffL);
            row1 = (block[i*8 + 4] & 0xffffL) << 48;
            row1 |= (block[i*8 + 5] & 0xffffL) << 32;
            row1 |= (block[i*8 + 6] & 0xffffL) << 16;
            row1 |= (block[i*8 + 7] & 0xffffL);
            if (((row0 & (~ROW0_MASK)) | row1) == 0) {
                temp = (block[i*8] << 3) & 0xffff;
                temp |= temp << 16;
                temp |= temp << 32;
                block[i*8] = (short)((temp >>> 48) & 0xffff);
                block[i*8+1] = (short)((temp >>> 32) & 0xffff);
                block[i*8+2] = (short)((temp >>> 16) & 0xffff);
                block[i*8+3] = (short)(temp & 0xffff);
                block[i*8+4] = block[i*8];
                block[i*8+5] = block[i*8+1];
                block[i*8+6] = block[i*8+2];
                block[i*8+7] = block[i*8+3];
                continue;
            } // if (((row0 & (~ROW0_MASK)) | row1) == 0)
            a0 = (W4 * block[i*8]) + ( 1 << (ROW_SHIFT - 1));
            a1 = a0;
            a2 = a0;
            a3 = a0;
            
            a0 += W2 * block[i*8 + 2];
            a1 += W6 * block[i*8 + 2];
            a2 -= W6 * block[i*8 + 2];
            a3 -= W2 * block[i*8 + 2];
            
            b0 = W1 * block[i*8 + 1];
            b0 += W3 * block[i*8 + 3];
            b1 = W3 * block[i*8 + 1];
            b1 += -W7 * block[i*8 + 3];
            b2 = W5 * block[i*8 + 1];
            b2 += -W1 * block[i*8 + 3];
            b3 = W7 * block[i*8 + 1];
            b3 += -W5 * block[i*8 + 3];
            
            temp = (block[i*8 + 4] & 0xffffL) << 48;
            temp |= (block[i*8 + 5] & 0xffffL) << 32;
            temp |= (block[i*8 + 6] & 0xffffL) << 16;
            temp |= (block[i*8 + 7] & 0xffffL);
            
            if (temp != 0) {
                a0 += W4 * block[i*8+4]+ W6 * block[i*8+6];
                a1 += -W4 * block[i*8+4] - W2 * block[i*8+6];
                a2 += -W4 * block[i*8+4] + W2 * block[i*8+6];
                a3 += W4 * block[i*8 + 4] - W6 *block[i*8 + 6];
                
                b0 += W5 * block[i*8 + 5];
                b0 += W7 * block[i*8 + 7];
                
                b1 += -W1 * block[i*8 + 5];
                b1 += -W5 * block[i*8 + 7];
                
                b2 += W7 * block[i*8 + 5];
                b2 += W3 * block[i*8 + 7];
                
                b3 += W3 * block[i*8 + 5];
                b3 += -W1 * block[i*8 + 7];
            } // if (temp != 0)
            
            block[i*8] = (short)((a0 + b0) >> ROW_SHIFT);
            block[i*8 + 7] = (short)((a0 - b0) >> ROW_SHIFT);
            block[i*8 + 1] = (short)((a1 + b1) >> ROW_SHIFT);
            block[i*8 + 6] = (short)((a1 - b1) >> ROW_SHIFT);
            block[i*8 + 2] = (short)((a2 + b2) >> ROW_SHIFT);
            block[i*8 + 5] = (short)((a2 - b2) >> ROW_SHIFT);
            block[i*8 + 3] = (short)((a3 + b3) >> ROW_SHIFT);
            block[i*8 + 4] = (short)((a3 - b3) >> ROW_SHIFT);
        } // for (i = 0; i < 8; i++)
        
        for (i = 0; i < 8; i++) {
            a0 = W4 * (block[i] + ((1 << (COL_SHIFT-1))/W4));
            a1 = a0;
            a2 = a0;
            a3 = a0;
            
            a0 += W2 * block[i + 16];
            a1 += W6 * block[i + 16];
            a2 += -W6 * block[i + 16];
            a3 += -W2 * block[i + 16];
            
            b0 = W1 * block[i + 8];
            b1 = W3 * block[i + 8];
            b2 = W5 * block[i + 8];
            b3 = W7 * block[i + 8];
            
            b0 += W3 * block[i + 24];
            b1 += -W7 * block[i + 24];
            b2 += -W1 * block[i + 24];
            b3 += -W5 * block[i + 24];
            
            if (block[i + 32] != 0) {
                a0 += W4 * block[i + 32];
                a1 += -W4 * block[i + 32];
                a2 += -W4 * block[i + 32];
                a3 += W4 * block[i + 32];
            } // if (block[i + 32] != 0)
            
            if (block[i + 40] != 0) {
                b0 += W5 * block[i + 40];
                b1 += -W1 * block[i + 40];
                b2 += W7 * block[i + 40];
                b3 += W3 * block[i + 40];
            } // if (block[i + 40] != 0)
            
            if (block[i + 48] != 0) {
                a0 += W6 * block[i + 48];
                a1 += -W2 * block[i + 48];
                a2 += W2 * block[i + 48];
                a3 += -W6 * block[i + 48];
            } // if (block[i + 48] != 0)
            
            if (block[i + 56] != 0) {
                b0 += W7 * block[i + 56];
                b1 += -W5 * block[i + 56];
                b2 += W3 * block[i + 56];
                b3 += -W1 * block[i + 56];
            } // if (block[i + 56] != 0)
            
            destPtr[0] = destOrg;
            if (destPtr[0] + i >= dest.length) {
                continue;
            }
            dest[destPtr[0] + i] = ff_cropTbl[MAX_NEG_CROP + ((a0 + b0) >> COL_SHIFT)];
            destPtr[0] += line_size;
            if (destPtr[0] + i >= dest.length) {
                continue;
            }
            dest[destPtr[0] + i] = ff_cropTbl[MAX_NEG_CROP + ((a1 + b1) >> COL_SHIFT)];
            destPtr[0] += line_size;
            if (destPtr[0] + i >= dest.length) {
                continue;
            }
            dest[destPtr[0] + i] = ff_cropTbl[MAX_NEG_CROP + ((a2 + b2) >> COL_SHIFT)];
            destPtr[0] += line_size;
            if (destPtr[0] + i >= dest.length) {
                continue;
            }
            dest[destPtr[0] + i] = ff_cropTbl[MAX_NEG_CROP + ((a3 + b3) >> COL_SHIFT)];
            destPtr[0] += line_size;
            if (destPtr[0] + i >= dest.length) {
                continue;
            }
            dest[destPtr[0] + i] = ff_cropTbl[MAX_NEG_CROP + ((a3 - b3) >> COL_SHIFT)];
            destPtr[0] += line_size;
            if (destPtr[0] + i >= dest.length) {
                continue;
            }
            dest[destPtr[0] + i] = ff_cropTbl[MAX_NEG_CROP + ((a2 - b2) >> COL_SHIFT)];
            destPtr[0] += line_size;
            if (destPtr[0] + i >= dest.length) {
                continue;
            }
            dest[destPtr[0] + i] = ff_cropTbl[MAX_NEG_CROP + ((a1 - b1) >> COL_SHIFT)];
            destPtr[0] += line_size;
            if (destPtr[0] + i >= dest.length) {
                continue;
            }
            dest[destPtr[0] + i] = ff_cropTbl[MAX_NEG_CROP + ((a0 - b0) >> COL_SHIFT)];
        } // for (i = 0; i < 8; i++)
    }
    
    private void ff_simple_idct_add(byte dest[], int destPtr[], int line_size, short block[]) {
        
    }
    
    private int get_vlc2(short table[][], int bits, int max_depth) {
        int n, index, nb_bits;
        int code = 0;
        // OPEN_READER(re, s)
        int re_index= gbc_index;
        int re_cache= 0;
        // UPDATE_CACHE(re, s)
        re_cache = (gbc_buffer[re_index>>3] & 0xff) << 24;
        re_cache |= (gbc_buffer[1 + (re_index>>3)] & 0xff) << 16;
        re_cache |= (gbc_buffer[2 + (re_index>>3)] & 0xff) << 8;
        re_cache |= (gbc_buffer[3 + (re_index>>3)] & 0xff);
        re_cache = re_cache << (re_index & 0x07);
        // index = SHOW_UBITS(name, gb, bits)
        index = re_cache >>> (32 - bits);
        code = table[index][0];
        //Preferences.debug("index = " + index + "\n");
        //Preferences.debug("code1 = " + code + "\n");
        n = table[index][1];
        //Preferences.debug("n1 = " + n + "\n");
        
        if ((max_depth > 1) && (n < 0)) {
            // LAST_SKIP_BITS(name, gb, bits)
            re_index += bits;
            // UPDATE_CACHE(name, gb)
            re_cache = (gbc_buffer[re_index>>3] & 0xff) << 24;
            re_cache |= (gbc_buffer[1 + (re_index>>3)] & 0xff) << 16;
            re_cache |= (gbc_buffer[2 + (re_index>>3)] & 0xff) << 8;
            re_cache |= (gbc_buffer[3 + (re_index>>3)] & 0xff);
            re_cache = re_cache << (re_index & 0x07);
            
            nb_bits = -n;
            
            // index = SHOW_UBITS(name, gb, nb_bits) + code;
            index = (re_cache >>> (32 - nb_bits)) + code;
            code = table[index][0];
            //Preferences.debug("index = " + index + "\n");
            //Preferences.debug("code2 = " + code + "\n");
            n = table[index][1];
            //Preferences.debug("n2 = " + n + "\n");
            if ((max_depth > 2) && (n < 0)) {
                // LAST_SKIP_BITS(name, gb, nb_bits)
                re_index += nb_bits;
                // UPDATE_CACHE(name, gb)
                re_cache = (gbc_buffer[re_index>>3] & 0xff) << 24;
                re_cache |= (gbc_buffer[1 + (re_index>>3)] & 0xff) << 16;
                re_cache |= (gbc_buffer[2 + (re_index>>3)] & 0xff) << 8;
                re_cache |= (gbc_buffer[3 + (re_index>>3)] & 0xff);
                re_cache = re_cache << (re_index & 0x07);
                
                nb_bits = -n;
                // index = SHOW_UBITS(name, gb, nb_bits) + code;
                index = (re_cache >>> (32 - nb_bits)) + code;
                code = table[index][0];
                //Preferences.debug("index = " + index + "\n");
                //Preferences.debug("code3 = " + code + "\n");
                n = table[index][1];
                //Preferences.debug("n3 = " + n + "\n");
            } // if ((max_depth > 2) && (n < 0))
        } // if ((max_depth > 1) && (n < 0))
        // SKIP_BITS(name, gb, n)
        re_cache <<= n;
        re_index += n;
        // CLOSE_READER(re, s);
        gbc_index = re_index;
        return code;
    }
    
    private int get_xbits(int n) {
        int sign;
        int cache;
        // OPEN_READER(re, s)
        int re_index= gbc_index;
        int re_cache= 0;
        // UPDATE_CACHE(re, s)
        re_cache = (gbc_buffer[re_index>>3] & 0xff) << 24;
        re_cache |= (gbc_buffer[1 + (re_index>>3)] & 0xff) << 16;
        re_cache |= (gbc_buffer[2 + (re_index>>3)] & 0xff) << 8;
        re_cache |= (gbc_buffer[3 + (re_index>>3)] & 0xff);
        re_cache = re_cache << (re_index & 0x07);
        // cache = GET_CACHE(re, s)
        cache = re_cache;
        sign = (~cache)>>31;
        // LAST_SKIP_BITS(re, s, n)
        re_index += n;
        // CLOSE_READER(re, s);
        gbc_index = re_index;
        return (((sign ^ cache) >>> (32 - n)) ^ sign) - sign;
    }
    
    private int mjpeg_decode_dc(int dc_index) {
        int code;
        code = get_vlc2(vlcs[0][dc_index], 9, 2);
        if (code < 0) {
            Preferences.debug("get_vlc2(vlcs[0]["+ dc_index + "], 9, 2) returned code = " + code + "\n");
            Preferences.debug("mjpeg_decode_dc: bad vlcs at dc_index = " + dc_index + "\n");
            return 0xffff;
        }
        if (code != 0) {
            return get_xbits(code);
        }
        else {
            return 0;
        }
    }
    
    /**
     * decode block and dequantize
     * @param srcBuf
     * @param int srcPtr
     * @param block
     * @param component
     * @param dc_index
     * @param ac_index
     * @param quant_matrix
     * @return
     */
    private int decode_block(short block[], int component, int dc_index, int ac_index, short quant_matrix[]) {
        int code, i, j, level, val;
        int n, index, nb_bits;
        int cache, sign;
        val = mjpeg_decode_dc(dc_index);
        if (val == 0xffff) {
            Preferences.debug("mjpeg_decode_dc error\n");
            return -1;
        }
        val = val * quant_matrix[0] + last_dc[component];
        last_dc[component] = val;
        block[0] = (short)val;
        /* AC coefs */
        i = 0;
        // OPEN_READER(re, &s->gb)
        int re_index= gbc_index;
        int re_cache= 0;
        for (;;) {
            if ((3 + (re_index>>3)) >= gbc_buffer.length) {
                Preferences.debug("Out of memory in decode_block\n");
                return -1;
            }
            // UPDATE_CACHE(re, &s->gb)
            re_cache = (gbc_buffer[re_index>>3] & 0xff) << 24;
            re_cache |= (gbc_buffer[1 + (re_index>>3)] & 0xff) << 16;
            re_cache |= (gbc_buffer[2 + (re_index>>3)] & 0xff) << 8;
            re_cache |= (gbc_buffer[3 + (re_index>>3)] & 0xff);
            re_cache = re_cache << (re_index & 0x07);
            
            // GET_VLC(code, re, &s->gb, s->vlcs[1][ac_index].table, 9, 2)
            short table[][] = vlcs[1][ac_index];
            int bits = 9;
            int max_depth = 2;
            // index = SHOW_UBITS(name, gb, bits)
            index = re_cache >>> (32 - bits);
            code = table[index][0];
            n = table[index][1];
            
            if ((max_depth > 1) && (n < 0)) {
                // LAST_SKIP_BITS(name, gb, bits)
                re_index += bits;
                // UPDATE_CACHE(name, gb)
                re_cache = (gbc_buffer[re_index>>3] & 0xff) << 24;
                re_cache |= (gbc_buffer[1 + (re_index>>3)] & 0xff) << 16;
                re_cache |= (gbc_buffer[2 + (re_index>>3)] & 0xff) << 8;
                re_cache |= (gbc_buffer[3 + (re_index>>3)] & 0xff);
                re_cache = re_cache << (re_index & 0x07);
                
                nb_bits = -n;
                
                // index = SHOW_UBITS(name, gb, nb_bits) + code;
                index = (re_cache >>> (32 - nb_bits)) + code;
                code = table[index][0];
                n = table[index][1];
                if ((max_depth > 2) && (n < 0)) {
                    // LAST_SKIP_BITS(name, gb, nb_bits)
                    re_index += nb_bits;
                    // UPDATE_CACHE(name, gb)
                    re_cache = (gbc_buffer[re_index>>3] & 0xff) << 24;
                    re_cache |= (gbc_buffer[1 + (re_index>>3)] & 0xff) << 16;
                    re_cache |= (gbc_buffer[2 + (re_index>>3)] & 0xff) << 8;
                    re_cache |= (gbc_buffer[3 + (re_index>>3)] & 0xff);
                    re_cache = re_cache << (re_index & 0x07);
                    
                    nb_bits = -n;
                    // index = SHOW_UBITS(name, gb, nb_bits) + code;
                    index = (re_cache >>> (32 - nb_bits)) + code;
                    code = table[index][0];
                    n = table[index][1];
                } // if ((max_depth > 2) && (n < 0))
            } // if ((max_depth > 1) && (n < 0))
            // SKIP_BITS(name, gb, n)
            re_cache <<= n;
            re_index += n;
            
            /* EOB */
            if (code == 0x10) {
                break;
            }
            i += code >>> 4;
            if (code != 0x100) {
                code &= 0xf; 
                if (code > MIN_CACHE_BITS - 16) {
                    // UPDATE_CACHE(re, &s->gb)
                    re_cache = (gbc_buffer[re_index>>3] & 0xff) << 24;
                    re_cache |= (gbc_buffer[1 + (re_index>>3)] & 0xff) << 16;
                    re_cache |= (gbc_buffer[2 + (re_index>>3)] & 0xff) << 8;
                    re_cache |= (gbc_buffer[3 + (re_index>>3)] & 0xff);
                    re_cache = re_cache << (re_index & 0x07);
                }
                // cache = GET_CACHE(re, s)
                cache = re_cache;
                sign = (~cache)>>31;
                level = (((sign ^ cache) >>> (32 - code)) ^ sign) - sign;
                
                // LAST_SKIP_BITS(re, &s->gb, code)
                re_index += code;
                
                if (i >= 63) {
                    if (i == 63) {
                        j = scantable_permutated[63];
                        block[j] = (short)(level * quant_matrix[j]);
                        break;
                    } // if (i == 63)
                    Preferences.debug("In decode_block error_count = " + i + "\n");
                    return -1;
                } // if (i >= 63)
                j = scantable_permutated[i];
                block[j] = (short)(level * quant_matrix[j]);
            } // if (code != 0x100)
        } // for (;;)
        // CLOSE_READER(re, &s->gb)
        gbc_index = re_index;
        return 0;
    }
    
    /**
     * decode block and dequantize - progressive JPEG version 
     * @param block
     * @param component
     * @param dc_index
     * @param ax_index
     * @param quant_matrix
     * @param ss
     * @param se
     * @param Ah
     * @param Al
     * @param EOBRUN
     * @return
     */
    private int decode_block_progressive(short block[], int component, int dc_index, int ax_index, short quant_matrix[],
                                         int ss, int se, int Ah, int Al, int EOBRUN[]) {
        return 0;
    }
    
    private void build_basic_mjpeg_vlc() {
        build_vlc(0, 0, ff_mjpeg_bits_dc_luminance, ff_mjpeg_val_dc, 12, false);
        build_vlc(0, 1, ff_mjpeg_bits_dc_chrominance, ff_mjpeg_val_dc, 12, false);
        build_vlc(1, 0, ff_mjpeg_bits_ac_luminance, ff_mjpeg_val_ac_luminance, 251, true);
        build_vlc(1, 1, ff_mjpeg_bits_ac_chrominance, ff_mjpeg_val_ac_chrominance, 251, true);
    }
    
    private int build_vlc(int index0, int index1, byte bits_table[], byte val_table[], int nb_codes,
                           boolean is_ac) {
        int i;
        byte huff_size[] = new byte[256+16];
        short huff_code[] = new short[256+16];
        
        ff_mjpeg_build_huffman_codes(huff_size, huff_code, bits_table, val_table);
        if (is_ac) {
            for (i = nb_codes-1; i >= 0; i--) {
                huff_size[i + 16] = huff_size[i];
                huff_code[i + 16] = huff_code[i];
            } // for (i = 0; i < nb_codes; i++)
            for (i = 0; i < 16; i++) {
               huff_size[i] = 0;
               huff_code[i] = 0;
            } // for (i = 0; i < 16; i++)
            nb_codes += 16;
        } // if (is_ac)
        return init_vlc_sparse(index0, index1, 9, nb_codes, huff_size, 1, 1, huff_code, 2, 2, null, 0, 0);
    }
    
    private void ff_mjpeg_build_huffman_codes(byte huff_size[], short huff_code[], byte bits_table[], 
                                              byte val_table[]) {
        int i, j, k, nb, code, sym;
        
        code = 0;
        k = 0;
        for (i = 1; i <= 16; i++) {
            nb = bits_table[i] & 0xff;
            for (j = 0; j < nb; j++) {
                sym = (val_table[k++] & 0xff);
                huff_size[sym] = (byte)i;
                huff_code[sym] = (short)code;
                code++;
            }
            code <<= 1;
        }
    }
    
    /**
     * Build VLC decoding tables suitable for use with get_vlc().
     * @param index0
     * @param index1
     * @param nb_bits set decoding table size (2^nb_bits) entries.  The bigger it is, the faster the decoding.  But 
     *                it should not be tto big to save memory and L1 cache.  '9' is a good compromise.
     * @param nb_codes  number of vlcs_codes
     * @param bits  table which gives the size in bits of each vlc code.
     * @param bits_wrap  gives the number of bytes between each entry of the bits table  
     * @param bits_size  gives the number of bytes between each entry of the bits table
     * @param codes  table which gives the bit_pattern of each vlc code.
     * @param codes_wrap  gives the number of bytes between each entry of the codes table 
     * @param codes_size  gives the number of bytes between each entry of the codes table
     * @param symbols table which gives the values to be returned form get_vlc().
     * @param symbols_wrap
     * @param symbols_size
     */
    private int init_vlc_sparse(int index0, int index1, int nb_bits, int nb_codes, byte bits[], int bits_wrap, int bits_size,
                                 short codes[], int codes_wrap, int codes_size, int symbols[], int symbols_wrap,
                                 int symbols_size) {
 
        vlcs_bits[index0][index1] = nb_bits; 
        vlcs_table_allocated[index0][index1] = 0;
        vlcs_table_size[index0][index1] = 0;
        
        if (build_table(index0, index1, nb_bits, nb_codes, bits, bits_wrap, bits_size, codes, codes_wrap, codes_size, symbols, 
                    symbols_wrap, symbols_size, 0, 0) < 0) {
            return -1;
        }
        return 0;
    }
    
    private int build_table(int index0, int index1, int table_nb_bits, int nb_codes, byte bits[], int bits_wrap, int bits_size,
            short codes[], int codes_wrap, int codes_size, int symbols[], int symbols_wrap,
            int symbols_size, int code_prefix, int n_prefix) {
        int table_size;
        int table_index;
        int i;
        int n;
        int code;
        int symbol;
        int code_prefix2;
        int j;
        int nb;
        int k;
        int n1;
        int index;
        short temp[][];
        //Preferences.debug("Entering build_table with index0 = " + index0 + " index1 = " + index1 + "\n");
        
        table_size = 1 << table_nb_bits;
        table_index = vlcs_table_size[index0][index1];
        vlcs_table_size[index0][index1] += table_size;
        if (vlcs_table_size[index0][index1] > vlcs_table_allocated[index0][index1]) {
            //Preferences.debug("New allocation in build_table\n");
            vlcs_table_allocated[index0][index1] += (1 << vlcs_bits[index0][index1]); 
            if (vlcs[index0][index1] != null) {
                temp = new short[vlcs[index0][index1].length][2];
                for (i = 0; i < temp.length; i++) {
                    temp[i][0] = vlcs[index0][index1][i][0];
                    temp[i][1] = vlcs[index0][index1][i][1];
                }
                vlcs[index0][index1] = new short[vlcs_table_allocated[index0][index1]][2];
                for (i = 0; i < temp.length; i++) {
                    vlcs[index0][index1][i][0] = temp[i][0];
                    vlcs[index0][index1][i][1] = temp[i][1];
                }
            }
            else {
                vlcs[index0][index1] = new short[vlcs_table_allocated[index0][index1]][2];    
            }
        }
        if (table_index < 0) {
            return -1;
        }
        //Preferences.debug("initializing table_index = " + table_index + " table_size = " + table_size + "\n");
        for (i = 0; i < table_size; i++) {
            vlcs[index0][index1][i+table_index][1] = 0; // bits
            vlcs[index0][index1][i+table_index][0] = -1; // codes
        }
        
        /* first pass: map codes and compute auxiliary table sizes */
        //Preferences.debug("nb_codes = " + nb_codes + "\n");
        //Preferences.debug("table_nb_bits = " + table_nb_bits + "\n");
        for (i = 0; i < nb_codes; i++) {
            //Preferences.debug("i = " + i + "\n");
            n = bits[i]; 
            //Preferences.debug("n = " + n + "\n");
            code = codes[i] & 0xffff;
            //Preferences.debug("code = " + code + "\n");
            /* We accept tables with holes */
            if (n <= 0) {
                continue;
            }
            if (symbols == null) {
                symbol = i;
            }
            else {
                symbol = symbols[i];    
            }
            //Preferences.debug("symbol = " + symbol + "\n");
            /* if code matches the prefix, it is in the table */
            n -= n_prefix;
            code_prefix2 = code >>> n;
            if ((n > 0) && (code_prefix2 == code_prefix)) {
                if (n <= table_nb_bits) {
                    /* no need to add another table */
                    j = (code << (table_nb_bits - n)) & (table_size - 1);
                    nb = 1 << (table_nb_bits - n);
                    //Preferences.debug("j = " + j + " nb = " + nb + "\n");
                    for (k = 0; k < nb; k++) {
                        if (vlcs[index0][index1][j+table_index][1] /* bits */ != 0) {
                            MipavUtil.displayError("Incorrect codes ");
                            Preferences.debug("Incorrect codes\n");
                            Preferences.debug("index0 = " + index0 + "\n");
                            Preferences.debug("index1 = " + index1 + "\n");
                            Preferences.debug("j = " + j + "\n");
                            Preferences.debug("table_index = " + table_index + "\n");
                            Preferences.debug("vlcs[index0][index1][j+table_index][1] = " +
                                    vlcs[index0][index1][j+table_index][1] + "\n");
                            Preferences.debug("k = " + k + "\n");
                            Preferences.debug("nb = " + nb + "\n");
                            return -1;
                        }
                        vlcs[index0][index1][j+table_index][1] = (short)n; // bits
                        vlcs[index0][index1][j+table_index][0] = (short)symbol;
                        j++;
                    } // for (k = 0; k < nb; k++)
                } // if (n <= table_nb_bits)
                else {
                    n -= table_nb_bits;
                    j = (code >>> n) & ((1 << table_nb_bits) - 1);
                    /* compute table size */
                    n1 = -vlcs[index0][index1][j+table_index][1]; // bits
                    if (n > n1) {
                        n1 = n;
                    }
                    vlcs[index0][index1][j+table_index][1] = (short)-n1; // bits
                }
            } // if ((n > 0) && (code_prefix2 == code_prefix))
        } // for (i = 0; i < nb_codes; i++)
        
        /* second pass: fill auxiliary tables recursively */
        for (i = 0; i < table_size; i++) {
            n = vlcs[index0][index1][i+table_index][1]; // bits
            if (n < 0) {
                n = -n;
                if (n > table_nb_bits) {
                    n = table_nb_bits;
                    vlcs[index0][index1][i+table_index][1] = (short)-n; // bits
                }
                //Preferences.debug("Recursive entry into build_table\n");
                index = build_table(index0, index1, n, nb_codes, bits, bits_wrap, bits_size, codes, codes_wrap,
                                    codes_size, symbols, symbols_wrap, symbols_size, 
                                    ((code_prefix << table_nb_bits) | i), n_prefix + table_nb_bits);
                if (index < 0) {
                    return -1;
                }
                vlcs[index0][index1][i + table_index][0] = (short)index; // code
            } // if (n < 0)
        } // for (i = 0; i < table_size; i++)
        return table_index;
    }

    /**
     * This method sets up for an AVI image write. This is only used for RGB images where only an imageA and no imageB
     * will be present. The image will be written as a series of 2D images and a series of frames without data used for
     * repeating existing frames. This method will use the first 2D image.
     *
     * @param      _imageA            First 2D image for AVI.
     * @param      microSecPerFrame   Frame rate.
     * @param      _totalDataFrames   Number of frames containing true data.
     * @param      _totalBlankFrames  Number of frames that are "blank" and should be filled with previous frame's data.
     *
     * @exception  IOException  if there is an error writing the file.
     */
    public void setAVIWrite(ModelImage _imageA, int microSecPerFrame, int _totalDataFrames, int _totalBlankFrames)
            throws IOException {

        // System.err.println("in setAVIWrite");
        byte[] signature;
        byte[] RIFFtype;
        byte[] CHUNKsignature;
        long saveLIST1Size; // location of length of CHUNK with first LIST - not including

        // first 8 bytes with LIST and size.  JUNK follows the end of
        // this CHUNK
        byte[] CHUNKtype;
        byte[] avihSignature;
        int[] extents;
        long saveLIST1subSize; // location of length of CHUNK with second LIST - not including

        // first 8 bytes with LIST and size.  Note that saveLIST1subSize =
        // saveLIST1Size + 76, and that the length size written to
        // saveLIST2Size is 76 less than that written to saveLIST1Size.
        // JUNK follows the end of this CHUNK.
        byte[] strhSignature;
        byte[] type;
        byte[] handler;
        byte[] strfSignature;
        long savestrfSize; // location of lenght of strf CHUNK - not including the first

        // 8 bytes with strf and size.  strn follows the end of this
        // CHUNK.
        int resXUnit = 0;
        int resYUnit = 0;
        float xResol = 0.0f; // in distance per pixel
        float yResol = 0.0f; // in distance per pixel
        long biXPelsPerMeter = 0L;
        long biYPelsPerMeter = 0L;
        byte[] strnSignature;
        byte[] text;
        long savestrnPos;
        byte[] JUNKsignature;
        long saveJUNKsignature;
        int paddingBytes;
        int i;
        byte[] dataSignature;
        int xMod;
        int bufferFactor;

        imageA = _imageA;
        totalDataFrames = _totalDataFrames;
        totalBlankFrames = _totalBlankFrames;

        dataFramesSent = 1;
        blankFramesSent = 0;
        compression = 0; // 24 bit RGB uncompressed
        bufferFactor = 4; // ARGB
        extents = imageA.getExtents();
        xDim = extents[0];
        yDim = extents[1];
        xPad = 0;
        xMod = xDim % 4;

        if (xMod != 0) {
            xPad = 4 - xMod;
            xDim = xDim + xPad;
        }

        try {
            file = new File(fileDir + fileName);

            if (raFile != null) {

                try {
                    raFile.close();
                } catch (IOException ex) { }
            }

            raFile = new RandomAccessFile(file, "rw");

            // Necessary so that if this is an overwritten file there isn't any
            // junk at the end
            raFile.setLength(0);
            signature = new byte[4];
            RIFFtype = new byte[4];
            CHUNKsignature = new byte[4];
            CHUNKtype = new byte[4];
            avihSignature = new byte[4];
            strhSignature = new byte[4];
            type = new byte[4];
            handler = new byte[4];
            strfSignature = new byte[4];
            strnSignature = new byte[4];
            text = new byte[16];
            JUNKsignature = new byte[4];
            CHUNKsignature = new byte[4];
            savedbLength = new long[totalDataFrames + totalBlankFrames];
            imageBufferA = new float[bufferFactor * xDim * yDim];
            dataSignature = new byte[4];
            bufferWrite = new byte[3 * xDim * yDim];
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Error while trying to write AVI file.");

            return;
        }

        endianess = false; // for little-endianess; big-endianess did not work with either the

        // Windows Media Player or PowerPoint

        signature[0] = 82; // R
        signature[1] = 73; // I
        signature[2] = 70; // F
        signature[3] = 70; // F
        raFile.write(signature);
        saveFileSize = raFile.getFilePointer();

        // Bytes 4 thru 7 contain the length of the file.  This length does
        // not include bytes 0 thru 7.
        writeInt(0, endianess); // for now write 0 in the file size location
        RIFFtype[0] = 65; // A
        RIFFtype[1] = 86; // V
        RIFFtype[2] = 73; // I
        RIFFtype[3] = 32; // space
        raFile.write(RIFFtype);

        // Write the first LIST chunk, which contains information on data decoding
        CHUNKsignature[0] = 76; // L
        CHUNKsignature[1] = 73; // I
        CHUNKsignature[2] = 83; // S
        CHUNKsignature[3] = 84; // T
        raFile.write(CHUNKsignature);

        // Write the length of the LIST CHUNK not including the first 8 bytes with LIST and
        // size.  Note that the end of the LIST CHUNK is followed by JUNK.
        saveLIST1Size = raFile.getFilePointer();
        writeInt(0, endianess); // for now write 0 in avih sub-CHUNK size location

        // Write the chunk type
        CHUNKtype[0] = 104; // h
        CHUNKtype[1] = 100; // d
        CHUNKtype[2] = 114; // r
        CHUNKtype[3] = 108; // l
        raFile.write(CHUNKtype);

        // Write the avih sub-CHUNK
        avihSignature[0] = 97; // a
        avihSignature[1] = 118; // v
        avihSignature[2] = 105; // i
        avihSignature[3] = 104; // h
        raFile.write(avihSignature);

        writeInt(0x38, endianess); // Write the length of the avih sub-CHUNK (38H) not including the

        // the first 8 bytes for avihSignature and the length
        writeInt(microSecPerFrame, endianess); // dwMicroSecPerFrame - Write the microseconds per frame

        // default is 140,000.
        writeInt(500000, endianess); // dwMaxBytesPerSec

        // Write the maximum data rate of the file in bytes per second
        writeInt(0, endianess); // dwReserved1 - Reserved1 field set to zero
        writeInt(0x10, endianess); // dwFlags - just set the bit for AVIF_HASINDEX

        // 10H AVIF_HASINDEX: The AVI file has an idx1 chunk containing
        // an index at the end of the file.  For good performance, all
        // AVI files should contain an index.
        // 20H AVIF_MUSTUSEINDEX: Index CHUNK, rather than the physical
        // ordering of the chunks in the file, must be used to determine the
        // order of the frames.
        // 100H AVIF_ISINTERLEAVED: Indicates that the AVI file is interleaved.
        // This is used to read data from a CD-ROM more efficiently.
        // 800H AVIF_TRUSTCKTYPE: USE CKType to find key frames
        // 10000H AVIF_WASCAPTUREFILE: The AVI file is used for capturing
        // real-time video.  Applications should warn the user before
        // writing over a file with this fla set because the user
        // probably defragmented this file.
        // 20000H AVIF_COPYRIGHTED: The AVI file contains copyrighted data
        // and software.  When, this flag is used, software should not
        // permit the data to be duplicated.

        // writeInt(totalFrames);
        writeInt(totalDataFrames + totalBlankFrames, endianess); // dwTotalFrames - total frame number
        writeInt(0, endianess); // dwInitialFrames -Initial frame for interleaved files.

        // Noninterleaved files should specify 0.
        writeInt(1, endianess); // dwStreams - number of streams in the file - here 1 video and zero audio.
        writeInt(3 * xDim * yDim, endianess); // dwSuggestedBufferSize - Suggested buffer size for reading the file.

        // Generally, this size should be large enough to contain the largest
        // chunk in the file.
        writeInt(xDim, endianess); // dwWidth - image width in pixels
        writeInt(yDim, endianess); // dwHeight - image height in pixels

        // dwReserved[4] - Microsoft says to set the following 4 values to 0.
        writeInt(0, endianess);
        writeInt(0, endianess);
        writeInt(0, endianess);
        writeInt(0, endianess);

        // Write the Stream line header CHUNK
        raFile.write(CHUNKsignature); // Write LIST to the file

        // Write the size of the first LIST subCHUNK not including the first 8 bytes with
        // LIST and size.  Note that saveLIST1subSize = saveLIST1Size + 76, and that
        // the length written to saveLIST1subSize is 76 less than the length written to saveLIST1Size.
        // The end of the first LIST subCHUNK is followed by JUNK.
        saveLIST1subSize = raFile.getFilePointer();
        writeInt(0, endianess); // for now write 0 in CHUNK size location

        // Write the chunk type
        CHUNKtype[0] = 115; // s
        CHUNKtype[1] = 116; // t
        CHUNKtype[2] = 114; // r
        CHUNKtype[3] = 108; // l
        raFile.write(CHUNKtype);

        // Write the strh sub-CHUNK
        strhSignature[0] = 115; // s
        strhSignature[1] = 116; // t
        strhSignature[2] = 114; // r
        strhSignature[3] = 104; // h
        raFile.write(strhSignature);
        writeInt(56, endianess); // Write the length of the strh sub-CHUNK

        // fccType - Write the type of data stream - here vids for video stream
        type[0] = 118; // v
        type[1] = 105; // i
        type[2] = 100; // d
        type[3] = 115; // s
        raFile.write(type);

        // fccHandler - Write the handler for data compression/decompression
        // If compression == 0  for 24 bit per pixel uncompressed RGB:
        // Write DIB for Microsoft Device Independent Bitmap.  Note: Unfortunately,
        // at least 3 other four character codes are sometimes used for uncompressed
        // AVI videos: 'RGB ', 'RAW ', 0x00000000
        handler[0] = 68; // D
        handler[1] = 73; // I
        handler[2] = 66; // B
        handler[3] = 32; // space
        raFile.write(handler);
        writeInt(0, endianess); // dwFlags

        // 0x00000001 AVISF_DISABLED The stram data should be rendered only when
        // explicitly enabled.
        // 0x00010000 AVISF_VIDEO_PALCHANGES Indicates that a palette change is included
        // in the AVI file.  This flag warns the playback software that it
        // will need to animate the palette.
        writeInt(0, endianess); // dwPriority - priority of a stream type.  For example, in a file with

        // multiple audio streams, the one with the highest priority might be the
        // default one.
        writeInt(0, endianess); // dwInitialFrames - Specifies how far audio data is skewed ahead of video

        // frames in interleaved files.  Typically, this is about 0.75 seconds.  In
        // interleaved files specify the number of frames in the file prior
        // to the initial frame of the AVI sequence.
        // Noninterleaved files should use zero.
        // rate/scale = samples/second
        writeInt((microSecPerFrame / 10000), endianess); // dwScale
        writeInt(100, endianess); // dwRate - frame rate for video streams
        writeInt(0, endianess); // dwStart - this field is usually set to zero
        writeInt(totalDataFrames + totalBlankFrames, endianess);

        // dwLength - playing time of AVI file as defined by scale and rate
        // Set equal to the number of frames
        writeInt(3 * xDim * yDim, endianess); // dwSuggestedBufferSize - Suggested buffer size for reading the stream.

        // Typically, this contains a value corresponding to the largest chunk
        // in a stream.
        writeInt(0, endianess); // dwQuality - encoding quality given by an integer between

        // 0 and 10,000.  If set to -1, drivers use the default
        // quality value.
        writeInt(0, endianess); // dwSampleSize

        // 0 if the video frames may or may not vary in size
        // If 0, each sample of data(such as a video frame) must
        // be in a separate chunk.
        // If nonzero, then multiple samples of data can be grouped into
        // a single chunk within the file.
        // rcFrame - Specifies the destination rectangle for a text or video stream within the movie
        // rectangle specified by the dwWidth and dwHeight members of the AVI main header structure.
        // The rcFrame member is typically used in support of multiple video streams.  Set this
        // rectangle to the coordinates corresponding to the movie rectangle to update the whole
        // movie rectangle.  Units for this member are pixels.  The upper-left corner of the destination
        // rectangle is relative to the upper-left corner of the movie rectangle.
        writeShort((short) 0, endianess); // left
        writeShort((short) 0, endianess); // top
        writeShort((short) (xDim), endianess); // right
        writeShort((short) (yDim), endianess); // bottom

        // Write the stream format chunk
        strfSignature[0] = 115; // s
        strfSignature[1] = 116; // t
        strfSignature[2] = 114; // r
        strfSignature[3] = 102; // f
        raFile.write(strfSignature);

        // Write the size of the stream format CHUNK not including the first 8 bytes for
        // strf and the size.  Note that the end of the stream format CHUNK is followed by
        // strn.
        savestrfSize = raFile.getFilePointer();
        writeInt(0, endianess); // for now write 0 in the strf CHUNK size location
        writeInt(40, endianess); // biSize - Write header size of BITMAPINFO header structure

        // Applications should use this size to determine which BITMAPINFO header structure is
        // being used.  This size includes this biSize field.
        writeInt(xDim, endianess); // biWidth - image width in pixels
        writeInt(yDim, endianess); // biHeight - image height in pixels.  If height is positive,

        // the bitmap is a bottom up DIB and its origin is in the lower left corner.  If
        // height is negative, the bitmap is a top-down DIB and its origin is the upper
        // left corner.  This negative sign feature is supported by the Windows Media Player, but it is not
        // supported by PowerPoint.
        writeShort((short) 1, endianess); // biPlanes - number of color planes in which the data is stored

        // This must be set to 1.
        writeShort((short) 24, endianess); // biBitCount - number of bits per pixel

        writeInt(compression, endianess); // biCompression - type of compression used

        // 0L for BI_RGB, uncompressed data as bitmap
        // 1L for BI_RLE8, a run-length encoded(RLE) format for bitmaps
        // with 8 bits per pixel.  The compression format is a 2-byte
        // format consisting of a byte count followed by a byte containing
        // a color index.  In addition, the first byte of the pair can be
        // set to zero to indicate an escape character that denotes the end
        // of a line, the end of a bitmap, a delta, or the number of bytes
        // which follow, each of which contains the color index of a single
        // pixel, depending on the
        // value of the second byte of the pair, which can be one of the
        // following values:
        // value             meaning
        // 0                 End of line.
        // 1                 End of bitmap.
        // 2                 Delta.  The two bytes following the
        // escape contain unsigned values indicating
        // the horizontal and vertical offsets
        // of the next pixel from the current
        // position.
        // 3-255             number of bytes that folow, each of which
        // contains the color index of a single pixel
        // Must be padded if an odd value so that it
        // ends on a word boundary.
        // 2L for BI_RLE4, a RLE format for bits with 4 bits per pixel.
        // The compression format is a 2-byte format consisting of a count
        // byte followed by two word-length color indexes.
        // 3L for BI_BITFIELDS, specifies that the bitmap is not compressed
        // and that the color table consists of three DWORD color masks
        // that specify the red, green, and blue components, respectively,
        // of each pixel.  This is valid when used with 16- and 32-bit-
        // per-pixel bitmaps.

        writeInt(3 * xDim * yDim, endianess); // biSizeImage

        // Specifies the size in bytes of the image frame.  This can be set to zero for uncompressed
        // RGB bitmaps.
        resXUnit = imageA.getFileInfo(0).getUnitsOfMeasure(0);

        if ((resXUnit == FileInfoBase.INCHES) || (resXUnit == FileInfoBase.CENTIMETERS) ||
                (resXUnit == FileInfoBase.ANGSTROMS) || (resXUnit == FileInfoBase.NANOMETERS) ||
                (resXUnit == FileInfoBase.MICROMETERS) || (resXUnit == FileInfoBase.MILLIMETERS) ||
                (resXUnit == FileInfoBase.METERS) || (resXUnit == FileInfoBase.KILOMETERS) ||
                (resXUnit == FileInfoBase.MILES)) {
            xResol = imageA.getFileInfo(0).getResolutions()[0];

            if (resXUnit == FileInfoBase.INCHES) {
                xResol = 0.0254f * xResol;
            } else if (resXUnit == FileInfoBase.CENTIMETERS) {
                xResol = 0.01f * xResol;
            } else if (resXUnit == FileInfoBase.ANGSTROMS) {
                xResol = 1.0e-10f * xResol;
            } else if (resYUnit == FileInfoBase.NANOMETERS) {
                yResol = 1.0e-9f * yResol;
            } else if (resXUnit == FileInfoBase.MICROMETERS) {
                xResol = 1.0e-6f * xResol;
            } else if (resXUnit == FileInfoBase.MILLIMETERS) {
                xResol = 1.0e-3f * xResol;
            } else if (resXUnit == FileInfoBase.KILOMETERS) {
                xResol = 1.0e3f * xResol;
            } else if (resXUnit == FileInfoBase.MILES) {
                xResol = 1.6093e3f * xResol;
            }

            if (xResol > 0.0f) {
                biXPelsPerMeter = (long) ((1 / xResol) + 0.5);
            }
        }

        writeInt((int) biXPelsPerMeter, endianess); // biXPelsPerMeter - horizontal resolution in pixels

        // per meter
        resYUnit = imageA.getFileInfo(0).getUnitsOfMeasure(1);

        if ((resYUnit == FileInfoBase.INCHES) || (resYUnit == FileInfoBase.CENTIMETERS) ||
                (resYUnit == FileInfoBase.ANGSTROMS) || (resYUnit == FileInfoBase.NANOMETERS) ||
                (resYUnit == FileInfoBase.MICROMETERS) || (resYUnit == FileInfoBase.MILLIMETERS) ||
                (resYUnit == FileInfoBase.METERS) || (resYUnit == FileInfoBase.KILOMETERS) ||
                (resYUnit == FileInfoBase.MILES)) {
            yResol = imageA.getFileInfo(0).getResolutions()[1];

            if (resYUnit == FileInfoBase.INCHES) {
                yResol = 0.0254f * yResol;
            } else if (resYUnit == FileInfoBase.CENTIMETERS) {
                yResol = 0.01f * yResol;
            } else if (resYUnit == FileInfoBase.ANGSTROMS) {
                yResol = 1.0e-10f * yResol;
            } else if (resYUnit == FileInfoBase.NANOMETERS) {
                yResol = 1.0e-9f * yResol;
            } else if (resYUnit == FileInfoBase.MICROMETERS) {
                yResol = 1.0e-6f * yResol;
            } else if (resYUnit == FileInfoBase.MILLIMETERS) {
                yResol = 1.0e-3f * yResol;
            } else if (resYUnit == FileInfoBase.KILOMETERS) {
                yResol = 1.0e3f * yResol;
            } else if (resYUnit == FileInfoBase.MILES) {
                yResol = 1.6093e3f * yResol;
            }

            if (yResol > 0.0f) {
                biYPelsPerMeter = (long) ((1 / yResol) + 0.5);
            }
        }

        writeInt((int) biYPelsPerMeter, endianess); // biYPelsPerMeter - vertical resolution in pixels

        // per meter
        writeInt(0, endianess); // biClrUsed - here indicates no color table

        // Provides a way for getting smaller color tables.  When this
        // field is set to 0, the number of colors in the color table is based on
        // the biBitCount field (1 indicates 2 colors, 4 indicates 16 colors,
        // 8 indicates 256, and 24 indicates no color table).  A nonzero value
        // specifies the exact number of colors in the table.  So, for example,
        // if an 8-bit DIB uses only 17 colors, then only those 17 colors need
        // to be defined in the table, and biClrUsed is set to 17.  If nonzero
        // for a 24-bit DIB, it indicates the existence of a color table that the
        // application can use for color reference.
        writeInt(0, endianess); // biClrImportant - specifies that the first x colors of the color table

        // are important to the DIB.  If the rest of the colors are not available,
        // the image still retains its meaning in an acceptable manner.  When this
        // field is set to zero, all the colors are important, or, rather, their
        // relative importance has not been computed.

        // Use strn to provide a zero terminated text string describing the stream
        savestrnPos = raFile.getFilePointer();
        raFile.seek(savestrfSize);
        writeInt((int) (savestrnPos - (savestrfSize + 4)), endianess);
        raFile.seek(savestrnPos);
        strnSignature[0] = 115; // s
        strnSignature[1] = 116; // t
        strnSignature[2] = 114; // r
        strnSignature[3] = 110; // n
        raFile.write(strnSignature);
        writeInt(16, endianess); // Write the length of the strn sub-CHUNK
        text[0] = 70; // F
        text[1] = 105; // i
        text[2] = 108; // l
        text[3] = 101; // e
        text[4] = 65; // A
        text[5] = 118; // v
        text[6] = 105; // i
        text[7] = 32; // space
        text[8] = 119; // w
        text[9] = 114; // r
        text[10] = 105; // i
        text[11] = 116; // t
        text[12] = 101; // e
        text[13] = 32; // space
        text[14] = 32; // space
        text[15] = 0; // termination byte
        raFile.write(text);

        // write a JUNK CHUNK for padding
        saveJUNKsignature = raFile.getFilePointer();
        raFile.seek(saveLIST1Size);
        writeInt((int) (saveJUNKsignature - (saveLIST1Size + 4)), endianess);
        raFile.seek(saveLIST1subSize);
        writeInt((int) (saveJUNKsignature - (saveLIST1subSize + 4)), endianess);
        raFile.seek(saveJUNKsignature);
        JUNKsignature[0] = 74; // J
        JUNKsignature[1] = 85; // U
        JUNKsignature[2] = 78; // N
        JUNKsignature[3] = 75; // K
        raFile.write(JUNKsignature);
        paddingBytes = (int) (4084 - (saveJUNKsignature + 8));
        writeInt(paddingBytes, endianess);

        for (i = 0; i < (paddingBytes / 2); i++) {
            writeShort((short) 0, endianess);
        }

        // Write the second LIST chunk, which contains the actual data
        CHUNKsignature[0] = 76; // L
        CHUNKsignature[1] = 73; // I
        CHUNKsignature[2] = 83; // S
        CHUNKsignature[3] = 84; // T
        raFile.write(CHUNKsignature);

        // Write the length of the LIST CHUNK not including the first 8 bytes with LIST and
        // size.  The end of the second LIST CHUNK is followed by idx1.
        saveLIST2Size = raFile.getFilePointer();
        writeInt(0, endianess); // For now write 0
        savemovi = raFile.getFilePointer();

        // Write CHUNK type 'movi'
        CHUNKtype[0] = 109; // m
        CHUNKtype[1] = 111; // 0
        CHUNKtype[2] = 118; // v
        CHUNKtype[3] = 105; // i
        raFile.write(CHUNKtype);

        // Write the data record signature '00db' where db means that DIB bitmap data (uncompressed)
        // follows.  The characters 00 are used to identify the stream.
        dataSignature[0] = 48; // 0
        dataSignature[1] = 48; // 0
        dataSignature[2] = 100; // d
        dataSignature[3] = 98; // b

        // Write the data.  Each 3-byte triplet in the bitmap array represents the relative intensities
        // of blue, green, and red, respectively, for a pixel.  The color bytes are in reverse order
        // from the Windows convention.
        raFile.write(dataSignature);
        savedbLength[dataFramesSent + blankFramesSent - 1] = raFile.getFilePointer();

        // Write the data length
        writeInt(3 * xDim * yDim, endianess);
        writeAVITripletCFrames();
    }

    /**
     * DOCUMENT ME!
     *
     * @param  quality  DOCUMENT ME!
     */
    public void setCompressionQuality(float quality) {
        this.compressionQuality = quality;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  isScript  DOCUMENT ME!
     */
    public void setIsScript(boolean isScript) {
        this.isScript = isScript;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  microSec  DOCUMENT ME!
     */
    public void setMicroSecPerFrame(int microSec) {
        this.microSecPerFrame = microSec;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  pBar  DOCUMENT ME!
     */
    public void setProgressBar(ProgressBarInterface pBar) {
        this.progressBar = pBar;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  readQT  DOCUMENT ME!
     */
    public void setReadQT(boolean readQT) {
        this.readQT = readQT;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  writeQuickTime  DOCUMENT ME!
     */
    public void setWriteQT(boolean writeQuickTime) {
        this.writeQT = writeQuickTime;
        this.fileName = fileName.substring(0, fileName.length() - 3) + "avi";
    }

    /**
     * DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public void writeBlankFrame() throws IOException {

        blankFramesSent++;
        dataSignature = new byte[4];
        dataSignature[0] = 48; // 0
        dataSignature[1] = 48; // 0
        dataSignature[2] = 100; // d
        dataSignature[3] = 99; // c
        raFile.write(dataSignature);
        savedbLength[dataFramesSent + blankFramesSent - 1] = raFile.getFilePointer();
        writeInt(0, endianess);

        if ((dataFramesSent == totalDataFrames) && (blankFramesSent == totalBlankFrames)) {
            writeidx1CHUNK();
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param   _imageA  DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public void writeDataFrame(ModelImage _imageA) throws IOException {
        imageA = _imageA;
        dataFramesSent++;

        if (imageBufferA == null) {
            imageBufferA = new float[4 * xDim * yDim];
        }

        // Write the data record signature '00db' where db means that DIB bitmap data (uncompressed)
        // follows.  The characters 00 are used to identify the stream.
        dataSignature = new byte[4];
        dataSignature[0] = 48; // 0
        dataSignature[1] = 48; // 0
        dataSignature[2] = 100; // d
        dataSignature[3] = 98; // b

        // Write the data.  Each 3-byte triplet in the bitmap array represents the relative intensities
        // of blue, green, and red, respectively, for a pixel.  The color bytes are in reverse order
        // from the Windows convention.
        if (bufferWrite == null) {
            bufferWrite = new byte[3 * xDim * yDim];
        }

        raFile.write(dataSignature);
        savedbLength[dataFramesSent + blankFramesSent - 1] = raFile.getFilePointer();

        // Write the data length
        writeInt(3 * xDim * yDim, endianess);
        writeAVITripletCFrames();

        if ((dataFramesSent == totalDataFrames) && (blankFramesSent == totalBlankFrames)) {
            writeidx1CHUNK();
        }
    }

    /**
     * This method writes an AVI image file.
     *
     * @param      _imageA      DOCUMENT ME!
     * @param      _imageB      DOCUMENT ME!
     * @param      _LUTa        DOCUMENT ME!
     * @param      LUTb         DOCUMENT ME!
     * @param      RGBTA        DOCUMENT ME!
     * @param      RGBTB        DOCUMENT ME!
     * @param      red          DOCUMENT ME!
     * @param      green        DOCUMENT ME!
     * @param      blue         DOCUMENT ME!
     * @param      OPACITY      DOCUMENT ME!
     * @param      alphaBlend   DOCUMENT ME!
     * @param      paintBitmap  DOCUMENT ME!
     * @param      compression  -1 = unchosen, 0 = 24 bit uncompressed RGB, 1 = 8 bit per pixel compressed RLE
     *
     * @return     DOCUMENT ME!
     *
     * @exception  IOException  if there is an error writing the file.
     */
    public boolean writeImage(ModelImage _imageA, ModelImage _imageB, ModelLUT _LUTa, ModelLUT LUTb, ModelRGB RGBTA,
                              ModelRGB RGBTB, int red, int green, int blue, float OPACITY, float alphaBlend,
                              BitSet paintBitmap, int compression) throws IOException {

        byte[] signature;
        byte[] RIFFtype;
        byte[] CHUNKsignature;
        long saveLIST1Size; // location of length of CHUNK with first LIST - not including

        // first 8 bytes with LIST and size.  JUNK follows the end of
        // this CHUNK
        byte[] CHUNKtype;
        byte[] avihSignature;
        int[] extents;
        long saveLIST1subSize; // location of length of CHUNK with second LIST - not including

        // first 8 bytes with LIST and size.  Note that saveLIST1subSize =
        // saveLIST1Size + 76, and that the length size written to
        // saveLIST2Size is 76 less than that written to saveLIST1Size.
        // JUNK follows the end of this CHUNK.
        byte[] strhSignature;
        byte[] type;
        byte[] handler;
        byte[] strfSignature;
        long savestrfSize; // location of lenght of strf CHUNK - not including the first

        // 8 bytes with strf and size.  strn follows the end of this
        // CHUNK.
        int resXUnit = 0;
        int resYUnit = 0;
        float xResol = 0.0f; // in distance per pixel
        float yResol = 0.0f; // in distance per pixel
        long biXPelsPerMeter = 0L;
        long biYPelsPerMeter = 0L;
        byte[] strnSignature;
        byte[] text;
        long savestrnPos;
        byte[] JUNKsignature;
        long saveJUNKsignature;
        int paddingBytes;
        int i;
        long[] savedcLength;
        long idx1Pos;
        long endPos;
        long saveidx1Length;
        int xMod;
        int bufferFactor;

        imageA = _imageA;
        imageB = _imageB;
        LUTa = _LUTa;

        int newMicroSecPerFrame = microSecPerFrame;
        int totalFrames = imageA.getExtents()[2];
        int realFrames = imageA.getExtents()[2];

        if (imageA.getExtents().length > 3) {
            realFrames *= imageA.getExtents()[3];
        }

        // System.err.println("Res [2] type is: " + imageA.getFileInfo()[0].getResolutions()[2]);
        if (imageA.getFileInfo()[0].getUnitsOfMeasure()[2] == FileInfoBase.NANOSEC) {
            newMicroSecPerFrame = (int) (imageA.getFileInfo()[0].getResolutions()[2] / 1000);
        } else if (imageA.getFileInfo()[0].getUnitsOfMeasure()[2] == FileInfoBase.MICROSEC) {
            newMicroSecPerFrame = (int) (imageA.getFileInfo()[0].getResolutions()[2]);
        } else if (imageA.getFileInfo()[0].getUnitsOfMeasure()[2] == FileInfoBase.MILLISEC) {
            newMicroSecPerFrame = (int) (imageA.getFileInfo()[0].getResolutions()[2] * 1000);
        } else if (imageA.getFileInfo()[0].getUnitsOfMeasure()[2] == FileInfoBase.SECONDS) {
            newMicroSecPerFrame = (int) (imageA.getFileInfo()[0].getResolutions()[2] * 1000000);
        } else if (imageA.getFileInfo()[0].getUnitsOfMeasure()[2] == FileInfoBase.MINUTES) {
            newMicroSecPerFrame = (int) (imageA.getFileInfo()[0].getResolutions()[2] * 1000000 * 60);
        } else if (imageA.getFileInfo()[0].getUnitsOfMeasure()[2] == FileInfoBase.HOURS) {
            newMicroSecPerFrame = (int) (imageA.getFileInfo()[0].getResolutions()[2] * 1000000 * 60 * 60);
        }
        // System.err.println("new micro sec per frame: " + newMicroSecPerFrame);

        if ((newMicroSecPerFrame > 1000000) || (newMicroSecPerFrame < 10000)) {
            newMicroSecPerFrame = microSecPerFrame;
        }

        if (imageA.getFileInfo()[0] instanceof FileInfoAvi) {
            totalFrames = ((FileInfoAvi) imageA.getFileInfo()[0]).getTotalFrames();

            int firstFrames = ((FileInfoAvi) imageA.getFileInfo()[0]).getNumFrames();

            // adjust the total frames if frames have been added/removed since first reading in the AVI
            if (firstFrames != realFrames) {
                totalFrames = (int) (((double) realFrames / (double) firstFrames) * totalFrames);
            }
        }

        if ((compression != -1) && (compression != 1)) {

            if (compression != 0) {
                newCompressionType = compression;
                compression = 0;
                useNewCompression = true;
            }

            if (compression == 2) {
                writeQT = true;
            }
        } else if (writeQT) {
            compression = 0;
            newCompressionType = AlgorithmTranscode.TRANSCODE_QT;
            useNewCompression = true;
        } else if ((imageA.isColorImage()) || (imageB != null)) {

            // if imageA is color or imageB is not null,
            // must have 24 bit uncompressed RGB
            compression = 0;

            JDialogAVIChoice choice = new JDialogAVIChoice(ViewUserInterface.getReference().getMainFrame(), true);

            if (!choice.okayPressed()) {
                return false;
            }

            int compressionType = choice.getCompression();

            if (compressionType > 1) {

                compression = 0;
                useNewCompression = true;
                newCompressionType = compressionType;

                if (compressionType == 2) {
                    writeQT = true;
                } else if (compressionType == AlgorithmTranscode.TRANSCODE_MJPG) {
                    setCompressionQuality(choice.getMJPEGQuality());
                }
            }

            compression = 0;
        } else {
            // Even if LUTa and LUTb are the same height, the same pixel can refer to one index
            // in one LUTa and another index in LUT

            if ((compression != 0) && (compression != 1)) {
                JDialogAVIChoice choice = new JDialogAVIChoice(ViewUserInterface.getReference().getMainFrame(), false);

                if (!choice.okayPressed()) {

                    // MipavUtil.displayWarning("AVI Save Cancelled");
                    return false;
                }

                int compressionType = choice.getCompression();

                if (compressionType > 1) {
                    compression = 0;
                    useNewCompression = true;
                    newCompressionType = compressionType;

                    if (compressionType == AlgorithmTranscode.TRANSCODE_MJPG) {
                        setCompressionQuality(choice.getMJPEGQuality());

                    }
                } else {
                    compression = compressionType;
                }
            }
        }

        FileWriteOptions options = new FileWriteOptions(true);
        options.setAVICompression(newCompressionType);
        options.setFileType(FileUtility.AVI);
        options.setBeginSlice(0);
        options.setEndSlice(imageA.getExtents()[2] - 1);
        ScriptRecorder.getReference().addLine(new ActionSaveImageAs(imageA, options));

        if (useNewCompression) {
            // if the compression will be M-JPEG, X and Y extents must be multiples of 8... otherwise 4

            int mod = (newCompressionType == AlgorithmTranscode.TRANSCODE_MJPG) ? 8 : 4;
            int leftPadding = 0;

            if ((imageA.getExtents()[0] % mod) != 0) {
                leftPadding = mod - (imageA.getExtents()[0] % mod);
            }

            int topPadding = 0;

            if ((imageA.getExtents()[1] % mod) != 0) {
                topPadding = mod - (imageA.getExtents()[1] % mod);
            }

            if ((leftPadding != 0) || (topPadding != 0)) {
                // this means we must resize the image to save it with this compression ask the user if he/she wants to
                // resize first

                String compStr = new String("");

                switch (newCompressionType) {

                    case AlgorithmTranscode.TRANSCODE_IV32:
                        compStr = "IR32";
                        break;

                    case AlgorithmTranscode.TRANSCODE_IV41:
                        compStr = "IR41";
                        break;

                    case AlgorithmTranscode.TRANSCODE_IV50:
                        compStr = "Indeo Video 5";
                        break;

                    case AlgorithmTranscode.TRANSCODE_MJPG:
                        compStr = "M-JPEG";
                        break;

                    case AlgorithmTranscode.TRANSCODE_MP42:
                        compStr = "MP42";
                        break;

                    case AlgorithmTranscode.TRANSCODE_MPG4:
                        compStr = "Microsoft Mpeg4";
                        break;

                    case AlgorithmTranscode.TRANSCODE_DIVX:
                        compStr = "DivX";
                        break;

                    case AlgorithmTranscode.TRANSCODE_DX50:
                        compStr = "DX50";
                        break;

                    default:
                        compStr = "this";
                        break;

                }
                // String compStr = (newCompressionType == AlgorithmTranscode.TRANSCODE_MP42) ?
                // new String("MP42") : new String("M-JPEG");

                int response = JOptionPane.NO_OPTION;

                if (!isScript) {
                    response = JOptionPane.showConfirmDialog(UI.getMainFrame(),
                                                             new String("AVI must be resized to save with " + compStr +
                                                                        " compression.  Resize?"), "Resize?",
                                                             JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
                } else {
                    response = JOptionPane.YES_OPTION;
                }

                if (response == JOptionPane.NO_OPTION) {
                    return false;
                } else {
                    int[] newExtents = new int[imageA.getExtents().length];
                    newExtents[0] = imageA.getExtents()[0] + leftPadding;
                    newExtents[1] = imageA.getExtents()[1] + topPadding;
                    newExtents[2] = imageA.getExtents()[2];

                    // System.err.println("New extents: " + newExtents[0] + " by " + newExtents[1] + " by " +
                    // newExtents[2]);

                    ModelImage paddedImage = new ModelImage(imageA.getType(), newExtents, "TEMPImage");

                    for (int index = 0; index < newExtents.length; index++) {
                        paddedImage.getFileInfo()[0].setResolutions(imageA.getFileInfo()[0].getResolutions()[index],
                                                                    index);
                        paddedImage.getFileInfo()[0].setUnitsOfMeasure(imageA.getFileInfo()[0].getUnitsOfMeasure()[index],
                                                                       index);
                    }

                    AlgorithmAddMargins algoMarg = null;

                    if (imageA.isColorImage()) {

                        // System.err.println("using color add image margins");
                        algoMarg = new AlgorithmAddMargins(imageA, paddedImage, 0, 0, 0, leftPadding, 0, topPadding, 0,
                                                           0);
                    } else {

                        // System.err.println("using b\\w add image margins");
                        algoMarg = new AlgorithmAddMargins(imageA, paddedImage, 0, leftPadding, 0, topPadding, 0, 0);
                    }

                    algoMarg.performCopiesWithBuffers(false);
                    algoMarg.setRunningInSeparateThread(false);
                    algoMarg.run();
                    algoMarg.finalize();
                    algoMarg = null;
                    paddedImage.calcMinMax();

                    boolean wasOkay = writeImage(paddedImage, null, _LUTa, null, RGBTA, RGBTB, red, green, blue,
                                                 OPACITY, alphaBlend, paintBitmap, newCompressionType);

                    paddedImage.disposeLocal();
                    paddedImage = null;

                    return wasOkay;
                }
            }
        }

        lutBufferRemapped = new int[1];

        if (useNewCompression) {
            file = new File(fileDir + "TEMPFILE.avi");
        } else {
            file = new File(fileDir + fileName);
        }

        if (raFile != null) {

            try {
                raFile.close();
            } catch (IOException ex) { }
        }

        raFile = new RandomAccessFile(file, "rw");
        endianess = false; // for little-endianess; big-endianess did not work with either the

        // Windows Media Player or PowerPoint
        // Necessary so that if this is an overwritten file there isn't any
        // junk at the end
        raFile.setLength(0);
        signature = new byte[4];
        signature[0] = 82; // R
        signature[1] = 73; // I
        signature[2] = 70; // F
        signature[3] = 70; // F
        raFile.write(signature);
        saveFileSize = raFile.getFilePointer();

        // Bytes 4 thru 7 contain the length of the file.  This length does
        // not include bytes 0 thru 7.
        writeInt(0, endianess); // for now write 0 in the file size location
        RIFFtype = new byte[4];
        RIFFtype[0] = 65; // A
        RIFFtype[1] = 86; // V
        RIFFtype[2] = 73; // I
        RIFFtype[3] = 32; // space
        raFile.write(RIFFtype);

        // Write the first LIST chunk, which contains information on data decoding
        CHUNKsignature = new byte[4];
        CHUNKsignature[0] = 76; // L
        CHUNKsignature[1] = 73; // I
        CHUNKsignature[2] = 83; // S
        CHUNKsignature[3] = 84; // T
        raFile.write(CHUNKsignature);

        // Write the length of the LIST CHUNK not including the first 8 bytes with LIST and
        // size.  Note that the end of the LIST CHUNK is followed by JUNK.
        saveLIST1Size = raFile.getFilePointer();
        writeInt(0, endianess); // for now write 0 in avih sub-CHUNK size location

        // Write the chunk type
        CHUNKtype = new byte[4];
        CHUNKtype[0] = 104; // h
        CHUNKtype[1] = 100; // d
        CHUNKtype[2] = 114; // r
        CHUNKtype[3] = 108; // l
        raFile.write(CHUNKtype);

        // Write the avih sub-CHUNK
        avihSignature = new byte[4];
        avihSignature[0] = 97; // a
        avihSignature[1] = 118; // v
        avihSignature[2] = 105; // i
        avihSignature[3] = 104; // h
        raFile.write(avihSignature);

        writeInt(0x38, endianess); // Write the length of the avih sub-CHUNK (38H) not including the

        // the first 8 bytes for avihSignature and the length

        int difMicSecPerFrame = Math.round(newMicroSecPerFrame * ((float) totalFrames / realFrames));

        // System.err.println("dif mic sec per frame: " + difMicSecPerFrame);
        writeInt(difMicSecPerFrame, endianess); // dwMicroSecPerFrame - Write the microseconds per frame

        // default is 140,000.
        writeInt(500000, endianess); // dwMaxBytesPerSec

        // Write the maximum data rate of the file in bytes per second
        writeInt(0, endianess); // dwReserved1 - Reserved1 field set to zero
        writeInt(0x10, endianess); // dwFlags - just set the bit for AVIF_HASINDEX

        // 10H AVIF_HASINDEX: The AVI file has an idx1 chunk containing
        // an index at the end of the file.  For good performance, all
        // AVI files should contain an index.
        // 20H AVIF_MUSTUSEINDEX: Index CHUNK, rather than the physical
        // ordering of the chunks in the file, must be used to determine the
        // order of the frames.
        // 100H AVIF_ISINTERLEAVED: Indicates that the AVI file is interleaved.
        // This is used to read data from a CD-ROM more efficiently.
        // 800H AVIF_TRUSTCKTYPE: USE CKType to find key frames
        // 10000H AVIF_WASCAPTUREFILE: The AVI file is used for capturing
        // real-time video.  Applications should warn the user before
        // writing over a file with this flag set because the user
        // probably defragmented this file.
        // 20000H AVIF_COPYRIGHTED: The AVI file contains copyrighted data
        // and software.  When, this flag is used, software should not
        // permit the data to be duplicated.

        extents = imageA.getExtents();

        if (imageA.getNDims() >= 4) {
            tDim = extents[3];
        } else {
            tDim = 1;
        }

        if (imageA.getNDims() >= 3) {
            zDim = extents[2];
        } else {
            zDim = 1;
        }

        yDim = extents[1];
        xDim = extents[0];
        xPad = 0;
        xMod = xDim % 4;

        if (xMod != 0) {
            xPad = 4 - xMod;
            xDim = xDim + xPad;
        }

        // realFrames = zDim * tDim;
        writeInt(realFrames, endianess); // dwTotalFrames - total frame number
        writeInt(0, endianess); // dwInitialFrames -Initial frame for interleaved files.

        // Noninterleaved files should specify 0.
        writeInt(1, endianess); // dwStreams - number of streams in the file - here 1 video and zero audio.
        writeInt(0x100000, endianess); // dwSuggestedBufferSize - Suggested buffer size for reading the file.

        // Generally, this size should be large enough to contain the largest
        // chunk in the file.
        writeInt(xDim, endianess); // dwWidth - image width in pixels
        writeInt(yDim, endianess); // dwHeight - image height in pixels

        // dwReserved[4] - Microsoft says to set the following 4 values to 0.
        writeInt(0, endianess);
        writeInt(0, endianess);
        writeInt(0, endianess);
        writeInt(0, endianess);

        // Write the Stream line header CHUNK
        raFile.write(CHUNKsignature); // Write LIST to the file

        // Write the size of the first LIST subCHUNK not including the first 8 bytes with
        // LIST and size.  Note that saveLIST1Size = saveLIST1subSize + 76, and that
        // the length written to saveLIST1subSize is 76 less than the length written to saveLIST1Size.
        // The end of the first LIST subCHUNK is followed by JUNK.
        saveLIST1subSize = raFile.getFilePointer();
        writeInt(0, endianess); // for now write 0 in CHUNK size location

        // Write the chunk type
        CHUNKtype[0] = 115; // s
        CHUNKtype[1] = 116; // t
        CHUNKtype[2] = 114; // r
        CHUNKtype[3] = 108; // l
        raFile.write(CHUNKtype);

        // Write the strh sub-CHUNK
        strhSignature = new byte[4];
        strhSignature[0] = 115; // s
        strhSignature[1] = 116; // t
        strhSignature[2] = 114; // r
        strhSignature[3] = 104; // h
        raFile.write(strhSignature);
        writeInt(56, endianess); // Write the length of the strh sub-CHUNK

        // fccType - Write the type of data stream - here vids for video stream
        type = new byte[4];
        type[0] = 118; // v
        type[1] = 105; // i
        type[2] = 100; // d
        type[3] = 115; // s
        raFile.write(type);
        handler = new byte[4];

        // fccHandler - Write the handler for data compression/decompression
        // If compression == 0  for 24 bit per pixel uncompressed RGB:
        // Write DIB for Microsoft Device Independent Bitmap.  Note: Unfortunately,
        // at least 3 other four character codes are sometimes used for uncompressed
        // AVI videos: 'RGB ', 'RAW ', 0x00000000
        if (compression == 0) {
            handler[0] = 68; // D
            handler[1] = 73; // I
            handler[2] = 66; // B
            handler[3] = 32; // space
        }
        // else if compression == 1 for 8 bit per pixel compressed RLE
        else {
            handler[0] = 0x6D; // m
            handler[1] = 0x72; // r
            handler[2] = 0x6C; // l
            handler[3] = 0x65; // e
        }

        raFile.write(handler);
        writeInt(0, endianess); // dwFlags

        // 0x00000001 AVISF_DISABLED The stream data should be rendered only when
        // explicitly enabled.
        // 0x00010000 AVISF_VIDEO_PALCHANGES Indicates that a palette change is included
        // in the AVI file.  This flag warns the playback software that it
        // will need to animate the palette.
        writeInt(0, endianess); // dwPriority - priority of a stream type.  For example, in a file with

        // multiple audio streams, the one with the highest priority might be the
        // default one.
        writeInt(0, endianess); // dwInitialFrames - Specifies how far audio data is skewed ahead of video

        // frames in interleaved files.  Typically, this is about 0.75 seconds.  In
        // interleaved files specify the number of frames in the file prior
        // to the initial frame of the AVI sequence.
        // Noninterleaved files should use zero.
        // rate/scale = samples/second

        int rate = (int)
                       Math.round((realFrames) /
                                      ((double) totalFrames / (1.0 / ((double) newMicroSecPerFrame / 1000000.0))));
        // System.err.println("Microsec per frame: " + newMicroSecPerFrame + " dwScale: " +
        // 1 + " dwRate: " + rate);

        writeInt(1, endianess); // dwScale
        writeInt(rate, endianess); // dwRate - frame rate for video streams
        writeInt(0, endianess); // dwStart - this field is usually set to zero
        writeInt(realFrames, endianess); // dwLength - playing time of AVI file as defined by scale and rate

        // Set equal to the number of frames
        writeInt(0x100000, endianess); // dwSuggestedBufferSize - Suggested buffer size for reading the stream.

        // Typically, this contains a value corresponding to the largest chunk
        // in a stream.
        writeInt(10000, endianess); // dwQuality - encoding quality given by an integer between

        // 0 and 10,000.  If set to -1, drivers use the default
        // quality value.

        if (compression == 0) {
            writeInt(3 * xDim * yDim, endianess); // dwSampleSize

            // writeInt(1572864, endianess);
        } else if (compression == 1) {
            writeInt(xDim * yDim, endianess); // dwSwampleSize
        }

        // 0 if the video frames may or may not vary in size
        // If 0, each sample of data(such as a video frame) must
        // be in a separate chunk.
        // If nonzero, then multiple samples of data can be grouped into
        // a single chunk within the file.
        // rcFrame - Specifies the destination rectangle for a text or video stream within the movie
        // rectangle specified by the dwWidth and dwHeight members of the AVI main header structure.
        // The rcFrame member is typically used in support of multiple video streams.  Set this
        // rectangle to the coordinates corresponding to the movie rectangle to update the whole
        // movie rectangle.  Units for this member are pixels.  The upper-left corner of the destination
        // rectangle is relative to the upper-left corner of the movie rectangle.
        writeShort((short) 0, endianess); // left
        writeShort((short) 0, endianess); // top
        writeShort((short) (xDim), endianess); // right
        writeShort((short) (yDim), endianess); // bottom

        // Write the stream format chunk
        strfSignature = new byte[4];
        strfSignature[0] = 115; // s
        strfSignature[1] = 116; // t
        strfSignature[2] = 114; // r
        strfSignature[3] = 102; // f
        raFile.write(strfSignature);

        // Write the size of the stream format CHUNK not including the first 8 bytes for
        // strf and the size.  Note that the end of the stream format CHUNK is followed by
        // strn.
        savestrfSize = raFile.getFilePointer();
        writeInt(0, endianess); // for now write 0 in the strf CHUNK size location
        writeInt(40, endianess); // biSize - Write header size of BITMAPINFO header structure

        // Applications should use this size to determine which BITMAPINFO header structure is
        // being used.  This size includes this biSize field.
        writeInt(xDim, endianess); // biWidth - image width in pixels
        writeInt(yDim, endianess); // biHeight - image height in pixels.  If height is positive,

        // the bitmap is a bottom up DIB and its origin is in the lower left corner.  If
        // height is negative, the bitmap is a top-down DIB and its origin is the upper
        // left corner.  This negative sign feature is supported by the Windows Media Player, but it is not
        // supported by PowerPoint.
        writeShort((short) 1, endianess); // biPlanes - number of color planes in which the data is stored

        // This must be set to 1.
        if (compression == 0) {
            writeShort((short) 24, endianess); // biBitCount - number of bits per pixel
        } else if (compression == 1) {
            writeShort((short) 8, endianess); // biBitCount - number of bits per pixel
        }

        writeInt(compression, endianess); // biCompression - type of compression used

        // 0L for BI_RGB, uncompressed data as bitmap
        // 1L for BI_RLE8, a run-length encoded(RLE) format for bitmaps
        // with 8 bits per pixel.  The compression format is a 2-byte
        // format consisting of a byte count followed by a byte containing
        // a color index.  In addition, the first byte of the pair can be
        // set to zero to indicate an escape character that denotes the end
        // of a line, the end of a bitmap, a delta, or the number of bytes
        // which follow, each of which contains the color index of a single
        // pixel, depending on the
        // value of the second byte of the pair, which can be one of the
        // following values:
        // value             meaning
        // 0                 End of line.
        // 1                 End of bitmap.
        // 2                 Delta.  The two bytes following the
        // escape contain unsigned values indicating
        // the horizontal and vertical offsets
        // of the next pixel from the current
        // position.
        // 3-255             number of bytes that folow, each of which
        // contains the color index of a single pixel
        // Must be padded if an odd value so that it
        // ends on a word boundary.
        // 2L for BI_RLE4, a RLE format for bits with 4 bits per pixel.
        // The compression format is a 2-byte format consisting of a count
        // byte followed by two word-length color indexes.
        // 3L for BI_BITFIELDS, specifies that the bitmap is not compressed
        // and that the color table consists of three DWORD color masks
        // that specify the red, green, and blue components, respectively,
        // of each pixel.  This is valid when used with 16- and 32-bit-
        // per-pixel bitmaps.
        if (compression == 0) {
            writeInt(3 * xDim * yDim, endianess); // biSizeImage
        } else if (compression == 1) {
            writeInt(xDim * yDim, endianess); // biSizeImage
        }

        // Specifies the size in bytes of the image frame.  This can be set to zero for uncompressed
        // RGB bitmaps.
        resXUnit = imageA.getFileInfo(0).getUnitsOfMeasure(0);

        if ((resXUnit == FileInfoBase.INCHES) || (resXUnit == FileInfoBase.CENTIMETERS) ||
                (resXUnit == FileInfoBase.ANGSTROMS) || (resXUnit == FileInfoBase.NANOMETERS) ||
                (resXUnit == FileInfoBase.MICROMETERS) || (resXUnit == FileInfoBase.MILLIMETERS) ||
                (resXUnit == FileInfoBase.METERS) || (resXUnit == FileInfoBase.KILOMETERS) ||
                (resXUnit == FileInfoBase.MILES)) {
            xResol = imageA.getFileInfo(0).getResolutions()[0];

            if (resXUnit == FileInfoBase.INCHES) {
                xResol = 0.0254f * xResol;
            } else if (resXUnit == FileInfoBase.CENTIMETERS) {
                xResol = 0.01f * xResol;
            } else if (resXUnit == FileInfoBase.ANGSTROMS) {
                xResol = 1.0e-10f * xResol;
            } else if (resXUnit == FileInfoBase.NANOMETERS) {
                xResol = 1.0e-9f * xResol;
            } else if (resXUnit == FileInfoBase.MICROMETERS) {
                xResol = 1.0e-6f * xResol;
            } else if (resXUnit == FileInfoBase.MILLIMETERS) {
                xResol = 1.0e-3f * xResol;
            } else if (resXUnit == FileInfoBase.KILOMETERS) {
                xResol = 1.0e3f * xResol;
            } else if (resXUnit == FileInfoBase.MILES) {
                xResol = 1.6093e3f * xResol;
            }

            if (xResol > 0.0f) {
                biXPelsPerMeter = (long) ((1 / xResol) + 0.5);
            }
        }

        writeInt((int) biXPelsPerMeter, endianess); // biXPelsPerMeter - horizontal resolution in pixels

        // per meter
        resYUnit = imageA.getFileInfo(0).getUnitsOfMeasure(1);

        if ((resYUnit == FileInfoBase.INCHES) || (resYUnit == FileInfoBase.CENTIMETERS) ||
                (resYUnit == FileInfoBase.ANGSTROMS) || (resYUnit == FileInfoBase.NANOMETERS) ||
                (resYUnit == FileInfoBase.MICROMETERS) || (resYUnit == FileInfoBase.MILLIMETERS) ||
                (resYUnit == FileInfoBase.METERS) || (resYUnit == FileInfoBase.KILOMETERS) ||
                (resYUnit == FileInfoBase.MILES)) {
            yResol = imageA.getFileInfo(0).getResolutions()[1];

            if (resYUnit == FileInfoBase.INCHES) {
                yResol = 0.0254f * yResol;
            } else if (resYUnit == FileInfoBase.CENTIMETERS) {
                yResol = 0.01f * yResol;
            } else if (resYUnit == FileInfoBase.ANGSTROMS) {
                yResol = 1.0e-10f * yResol;
            } else if (resYUnit == FileInfoBase.NANOMETERS) {
                yResol = 1.0e-9f * yResol;
            } else if (resYUnit == FileInfoBase.MICROMETERS) {
                yResol = 1.0e-6f * yResol;
            } else if (resYUnit == FileInfoBase.MILLIMETERS) {
                yResol = 1.0e-3f * yResol;
            } else if (resYUnit == FileInfoBase.KILOMETERS) {
                yResol = 1.0e3f * yResol;
            } else if (resYUnit == FileInfoBase.MILES) {
                yResol = 1.6093e3f * yResol;
            }

            if (yResol > 0.0f) {
                biYPelsPerMeter = (long) ((1 / yResol) + 0.5);
            }
        }

        // System.err.println("X Resolution on write is: " + xResol + " Y Resolution: " + yResol);

        writeInt((int) biYPelsPerMeter, endianess); // biYPelsPerMeter - vertical resolution in pixels

        // per meter
        if (compression == 0) {
            writeInt(0, endianess); // biClrUsed - here indicates no color table
        } else if (compression == 1) {

            // biClrUsed - here a color table is used
            writeInt(LUTa.getExtents()[1], endianess);
        }

        // Provides a way for getting smaller color tables.  When this
        // field is set to 0, the number of colors in the color table is based on
        // the biBitCount field (1 indicates 2 colors, 4 indicates 16 colors,
        // 8 indicates 256, and 24 indicates no color table).  A nonzero value
        // specifies the exact number of colors in the table.  So, for example,
        // if an 8-bit DIB uses only 17 colors, then only those 17 colors need
        // to be defined in the table, and biClrUsed is set to 17.  If nonzero
        // for a 24-bit DIB, it indicates the existence of a color table that the
        // application can use for color reference.
        writeInt(0, endianess); // biClrImportant - specifies that the first x colors of the color table

        // are important to the DIB.  If the rest of the colors are not available,
        // the image still retains its meaning in an acceptable manner.  When this
        // field is set to zero, all the colors are important, or, rather, their
        // relative importance has not been computed.
        // Write the LUTa.getExtents()[1] color table entries here if RLE8 compression used.  They are written:
        // blue byte, green byte, red byte, 0 byte
        if (compression == 1) {
            raFile.write(createLUT());
        }

        // Use strn to provide a zero terminated text string describing the stream
        savestrnPos = raFile.getFilePointer();
        raFile.seek(savestrfSize);
        writeInt((int) (savestrnPos - (savestrfSize + 4)), endianess);
        raFile.seek(savestrnPos);
        strnSignature = new byte[4];
        strnSignature[0] = 115; // s
        strnSignature[1] = 116; // t
        strnSignature[2] = 114; // r
        strnSignature[3] = 110; // n
        raFile.write(strnSignature);
        writeInt(16, endianess); // Write the length of the strn sub-CHUNK
        text = new byte[16];
        text[0] = 70; // F
        text[1] = 105; // i
        text[2] = 108; // l
        text[3] = 101; // e
        text[4] = 65; // A
        text[5] = 118; // v
        text[6] = 105; // i
        text[7] = 32; // space
        text[8] = 119; // w
        text[9] = 114; // r
        text[10] = 105; // i
        text[11] = 116; // t
        text[12] = 101; // e
        text[13] = 32; // space
        text[14] = 32; // space
        text[15] = 0; // termination byte
        raFile.write(text);

        // write a JUNK CHUNK for padding
        saveJUNKsignature = raFile.getFilePointer();
        raFile.seek(saveLIST1Size);
        writeInt((int) (saveJUNKsignature - (saveLIST1Size + 4)), endianess);
        raFile.seek(saveLIST1subSize);
        writeInt((int) (saveJUNKsignature - (saveLIST1subSize + 4)), endianess);
        raFile.seek(saveJUNKsignature);
        JUNKsignature = new byte[4];
        JUNKsignature[0] = 74; // J
        JUNKsignature[1] = 85; // U
        JUNKsignature[2] = 78; // N
        JUNKsignature[3] = 75; // K
        raFile.write(JUNKsignature);
        paddingBytes = (int) (4084 - (saveJUNKsignature + 8));
        writeInt(paddingBytes, endianess);

        for (i = 0; i < (paddingBytes / 2); i++) {
            writeShort((short) 0, endianess);
        }

        // Write the second LIST chunk, which contains the actual data
        CHUNKsignature = new byte[4];
        CHUNKsignature[0] = 76; // L
        CHUNKsignature[1] = 73; // I
        CHUNKsignature[2] = 83; // S
        CHUNKsignature[3] = 84; // T
        raFile.write(CHUNKsignature);

        // Write the length of the LIST CHUNK not including the first 8 bytes with LIST and
        // size.  The end of the second LIST CHUNK is followed by idx1.
        saveLIST2Size = raFile.getFilePointer();
        writeInt(0, endianess); // For now write 0
        savemovi = raFile.getFilePointer();

        // Write CHUNK type 'movi'
        CHUNKtype[0] = 109; // m
        CHUNKtype[1] = 111; // 0
        CHUNKtype[2] = 118; // v
        CHUNKtype[3] = 105; // i
        raFile.write(CHUNKtype);
        savedbLength = new long[tDim * zDim];
        savedcLength = new long[tDim * zDim];
        dcLength = new int[tDim * zDim];

        if (compression == 0) {
            bufferFactor = 1;

            if (imageA.isColorImage()) {
                bufferFactor = 4;
            }

            imageBufferA = new float[bufferFactor * xDim * yDim];

            if (imageB != null) {
                imageBufferB = new float[bufferFactor * xDim * yDim];
            }

            // Write the data record signature '00db' where db means that DIB bitmap data (uncompressed)
            // follows.  The characters 00 are used to identify the stream.
            dataSignature = new byte[4];
            dataSignature[0] = 48; // 0
            dataSignature[1] = 48; // 0
            dataSignature[2] = 100; // d
            dataSignature[3] = 98; // b

            // Write the data.  Each 3-byte triplet in the bitmap array represents the relative intensities
            // of blue, green, and red, respectively, for a pixel.  The color bytes are in reverse order
            // from the Windows convention.
            bufferWrite = new byte[3 * xDim * yDim];

            if (imageA.isColorImage() == false) {

                for (int t = 0; t < tDim; t++) {

                    for (int z = 0; z < zDim; z++) {
                        raFile.write(dataSignature);
                        savedbLength[z + (t * zDim)] = raFile.getFilePointer();

                        // Write the data length
                        writeInt(3 * xDim * yDim, endianess);
                        writeAVITriplet(t, z, LUTb, red, green, blue, 1 - OPACITY, alphaBlend, paintBitmap);

                        if (((zDim * tDim) - 1) >= 1) {
                            fireProgressStateChanged(100 * (z + (t * zDim)) / ((zDim * tDim) - 1));
                        }
                    }
                }
            } else {

                for (int t = 0; t < tDim; t++) {

                    for (int z = 0; z < zDim; z++) {
                        raFile.write(dataSignature);
                        savedbLength[z + (t * zDim)] = raFile.getFilePointer();

                        // Write the data length
                        writeInt(3 * xDim * yDim, endianess);
                        writeAVITripletC(t, z, RGBTA, RGBTB, red, green, blue, 1 - OPACITY, alphaBlend, paintBitmap);

                        if (((zDim * tDim) - 1) >= 1) {
                            fireProgressStateChanged(100 * (z + (t * zDim)) / ((zDim * tDim) - 1));
                        }
                    }
                }
            }
        } // if (compression == 0)
        else { // compression == 1
            imageBufferA = new float[xDim * yDim];

            byte[] pixStore = new byte[xDim * yDim];
            byte[] lastStore = new byte[xDim * yDim];
            byte[] encodeStore = new byte[(2 * xDim * yDim) + xDim]; // largest possible size

            // 1 count for every index and an end of line or end of bitmap ends every row
            // Write the data record signature '00dc' where dc means that DIB bitmap data (compressed)
            // follows.  The characters 00 are used to identify the stream.
            dataSignature = new byte[4];
            dataSignature[0] = 48; // 0
            dataSignature[1] = 48; // 0
            dataSignature[2] = 100; // d
            dataSignature[3] = 99; // c

            for (int t = 0; t < tDim; t++) {

                for (int z = 0; z < zDim; z++) {
                    raFile.write(dataSignature);
                    savedcLength[z + (t * zDim)] = raFile.getFilePointer();

                    // Write the data length follwed by run length encoded data
                    writeRLE8(t, z, pixStore, lastStore, encodeStore);

                    if (((zDim * tDim) - 1) >= 1) {
                        fireProgressStateChanged(100 * (z + (t * zDim)) / ((zDim * tDim) - 1));
                    }
                }
            }
        } // else compression == 1

        // Write the idx1 CHUNK
        // Write the 'idx1' signature
        idx1Pos = raFile.getFilePointer();
        raFile.seek(saveLIST2Size);
        writeInt((int) (idx1Pos - (saveLIST2Size + 4)), endianess);
        raFile.seek(idx1Pos);

        byte[] idx1Signature = new byte[4];
        idx1Signature[0] = 105; // i
        idx1Signature[1] = 100; // d
        idx1Signature[2] = 120; // x
        idx1Signature[3] = 49; // 1
        raFile.write(idx1Signature);

        // Write the length of the idx1 CHUNK not including the idx1 signature and the 4 length
        // bytes. Write 0 for now.
        saveidx1Length = raFile.getFilePointer();
        writeInt(0, endianess);

        for (int t = 0; t < tDim; t++) {

            for (int z = 0; z < zDim; z++) {

                // In the ckid field write the 4 character code to identify the chunk 00db or 00dc
                raFile.write(dataSignature);

                if ((z == 0) && (t == 0)) {
                    writeInt(0x10, endianess); // Write the flags - select AVIIF_KEYFRAME
                } else {
                    writeInt(0x00, endianess);
                }

                // AVIIF_KEYFRAME 0x00000010L
                // The flag indicates key frames in the video sequence.
                // Key frames do not need previous video information to be decompressed.
                // AVIIF_NOTIME 0x00000100L The CHUNK does not influence video timing(for
                // example a palette change CHUNK).
                // AVIIF_LIST 0x00000001L Marks a LIST CHUNK.
                // AVIIF_TWOCC 2L
                // AVIIF_COMPUSE 0x0FFF0000L These bits are for compressor use.
                if (compression == 0) {
                    writeInt((int) (savedbLength[z + (t * zDim)] - 4 - savemovi), endianess);

                    // Write the offset (relative to the 'movi' field) to the relevant CHUNK
                    writeInt(3 * xDim * yDim, endianess); // Write the length of the relevant

                    // CHUNK.  Note that this length is
                    // also written at savedbLength
                } else if (compression == 1) {
                    writeInt((int) (savedcLength[z + (t * zDim)] - 4 - savemovi), endianess);

                    // Write the offset (relative to the 'movi' field) to the relevant CHUNK
                    writeInt(dcLength[z + (t * zDim)], endianess); // Write the length of the relevant CHUNK.  Note

                    // that this length is also written at savedcLength
                }
            } // for (z = 0; z < zDim; z++)
        } // for (t = 0; t < tDim; t++)

        endPos = raFile.getFilePointer();
        raFile.seek(saveFileSize);
        writeInt((int) (endPos - (saveFileSize + 4)), endianess);
        raFile.seek(saveidx1Length);
        writeInt((int) (endPos - (saveidx1Length + 4)), endianess);

        // raFile.close();


        // do some cleanup...
        imageBufferA = null;
        imageBufferB = null;
        _imageA = null;
        imageA = null;
        imageB = null;

        // if new compression is to be used, we aren't done yet...
        if (!useNewCompression) {
            raFile.close();
            raFile = null;
        } else {
            Preferences.debug("About to transcode to: " + fileDir + fileName + "\n", Preferences.DEBUG_FILEIO);

            AlgorithmTranscode at = new AlgorithmTranscode(file.toURI().toURL(), fileDir + fileName,
                                                           newCompressionType);

            at.setRunningInSeparateThread(false);
            at.setQuality(compressionQuality);
            at.run();
            at.finalize();
            at = null;

            raFile.close();
            raFile = null;

            FileDeleter fd = new FileDeleter(file.getPath());
            fd.start();
            file = null;
        }

        return true;
    }

    /**
     * Creates a LUT to write.
     *
     * @return  DOCUMENT ME!
     */
    private byte[] createLUT() {
        int i, value, lutHeightA;
        float rangeA, imageMaxA;

        if (imageA == null) {
            MipavUtil.displayError("imageA is null");

            return null;
        }

        if (LUTa == null) {
            MipavUtil.displayError("LUTa is null");

            return null;
        }

        lutHeightA = LUTa.getExtents()[1];

        byte[] lutWrite = new byte[4 * lutHeightA];

        if (lutHeightA != lutBufferRemapped.length) {

            try {
                lutBufferRemapped = new int[lutHeightA];
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: FileAvi.createLUT");

                return null;
            }
        }

        lutBufferRemapped = LUTa.exportIndexedLUT();

        // imageMinA    = (float)imageA.getMin();
        // imageMaxA    = (float)imageA.getMax();

        if (imageA.getType() == ModelStorageBase.UBYTE) {
            imageMinA = 0;
            imageMaxA = 255;
        } else if (imageA.getType() == ModelStorageBase.BYTE) {
            imageMinA = -128;
            imageMaxA = 127;
        } else {
            imageMinA = (float) imageA.getMin();
            imageMaxA = (float) imageA.getMax();
        }

        rangeA = imageMaxA - imageMinA;

        if (rangeA == 0) {
            rangeA = 1;
        }

        if ((lutHeightA - 1) == 0) {
            remapConstA = 1;
        } else if (rangeA < 255) {
            remapConstA = 1;
        } else {
            remapConstA = (lutHeightA - 1) / rangeA;
        }

        for (i = 0; i < lutHeightA; i++) {
            value = lutBufferRemapped[i];

            lutWrite[4 * i] = (byte) (value & 0x000000ff); // blue
            lutWrite[(4 * i) + 1] = (byte) ((value & 0x0000ff00) >> 8); // green
            lutWrite[(4 * i) + 2] = (byte) ((value & 0x00ff0000) >> 16); // red

            // System.out.println("Hey i = " + i + " value == " + (lutWrite[4 * i + 2] & 0xff) + ", " +
            // (lutWrite[4 * i + 1] & 0xff) + ", " +
            // (lutWrite[4 * i + 0] & 0xff));
            lutWrite[(4 * i) + 3] = (byte) 0;
        }

        return lutWrite;
    }

    /**
     * Reads the image header.
     *
     * @return  DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private int readHeader() throws IOException {
        long LIST1Marker, LISTsubchunkMarker, marker;
        int loop;

        try {
            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "r");

            endianess = FileBase.LITTLE_ENDIAN; // false
            fileInfo = new FileInfoAvi(fileName, fileDir, FileUtility.AVI);
            fileInfo.setEndianess(endianess);

            int signature = getInt(endianess);

            if (signature == 0x46464952) {
                // have read RIFF
            } else {
                raFile.close();
                throw new IOException("AVI read header error first 4 bytes = " + signature);
            }

            getInt(endianess); // the file size excluding the first 8 bytes

            int RIFFtype = getInt(endianess);

            if (RIFFtype == 0x20495641) {
                // have read AVI<sp>
            } else {
                raFile.close();
                throw new IOException("AVI read header error bytes 8-11 = " + RIFFtype);
            }

            int CHUNKsignature = getInt(endianess);

            if (CHUNKsignature == 0x5453494C) {
                // have read LIST for first LIST CHUNK with information on data decoding
            } else {
                raFile.close();
                throw new IOException("AVI read header error bytes 12-15 = " + CHUNKsignature);
            }

            int LIST1Size = getInt(endianess); // size of first LIST CHUNK excluding first 8 bytes
            Preferences.debug("LIST1Size = " + LIST1Size + "\n", Preferences.DEBUG_FILEIO);

            // with CHUNKsignature and LIST1Size
            LIST1Marker = raFile.getFilePointer();

            int CHUNKtype = getInt(endianess);

            if (CHUNKtype == 0x6C726468) {
                // have read hdrl
            } else {
                raFile.close();
                throw new IOException("AVI read header error bytes 16-19 = " + CHUNKtype);
            }

            int avihSignature = getInt(endianess); // signature of avih sub-CHUNK

            if (avihSignature == 0x68697661) {
                // have read avih
            } else {
                raFile.close();
                throw new IOException("AVI read header error bytes 2-23 = " + avihSignature);
            }

            int avihLength = getInt(endianess); // read the size of the avih sub-CHUNK not

            // including the first 8 bytes for the signature and length
            if (avihLength == 56) {
                // avih sub-CHUNK has expected length
            } else {
                raFile.close();
                throw new IOException("AVI read header error avih sub-CHUNK length = " + avihLength);
            }

            microSecPerFrame = getInt(endianess);

            // System.err.println("Microsec per frame: " + microSecPerFrame);
            Preferences.debug("microSecPerFrame = " + microSecPerFrame + "\n", Preferences.DEBUG_FILEIO);

            int maxBytesPerSecond = getInt(endianess);
            Preferences.debug("maxBytesPerSecond = " + maxBytesPerSecond + "\n", Preferences.DEBUG_FILEIO);

            // System.err.println("Unknown int: " + getInt(endianess));
            getInt(endianess);

            int flags = getInt(endianess);

            if ((flags & 0x10) != 0) {
                AVIF_HASINDEX = true;
            } else {
                AVIF_HASINDEX = false;
            }

            if ((flags & 0x20) != 0) {
                AVIF_MUSTUSEINDEX = true;
            } else {
                AVIF_MUSTUSEINDEX = false;
            }

            if ((flags & 0x100) != 0) {
                AVIF_ISINTERLEAVED = true;
                Preferences.debug("AVIF_ISINTERLEAVED = true\n", Preferences.DEBUG_FILEIO);
            } else {
                AVIF_ISINTERLEAVED = false;
                Preferences.debug("AVIF_ISINTERLEAVED = false\n", Preferences.DEBUG_FILEIO);
            }

            if (AVIF_HASINDEX) {
                Preferences.debug("AVIF_HASINDEX = true\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("AVIF_HASINDEX = false\n", Preferences.DEBUG_FILEIO);
            }

            if (AVIF_MUSTUSEINDEX) {
                Preferences.debug("AVIF_MUSTUSEINDEX = true\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("AVIF_MUSTUSEINDEX = false\n", Preferences.DEBUG_FILEIO);
            }

            if ((flags & 0x800) != 0) {
                Preferences.debug("AVIF_TRUSTCKTYPE = true\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("AVIF_TRUSTCKTYPE = false\n", Preferences.DEBUG_FILEIO);
            }

            if ((flags & 0x10000) != 0) {
                Preferences.debug("AVIF_WASCAPTUREFILE = true\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("AVIF_WASCAPTUREFILE = false\n", Preferences.DEBUG_FILEIO);
            }

            if ((flags & 0x20000) != 0) {
                Preferences.debug("AVIF_COPYRIGHTED = true\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("AVIF_COPYRIGHTED = false\n", Preferences.DEBUG_FILEIO);
            }

            int totalFrames = getInt(endianess);
            fileInfo.setTotalFrames(totalFrames);

            // System.err.println("total frames: " + totalFrames);
            Preferences.debug("totalFrames = " + totalFrames + "\n", Preferences.DEBUG_FILEIO);

            // However, many AVI frames will have no data and will just be used to repeat the
            // previous frames.  So we will need to read thru the data to get the actual number
            // of frames used in MIPAV before the image is created.  Then a second read thru will
            // take place to import the data into the image.
            int initialFrames = getInt(endianess);
            Preferences.debug("initialFrames = " + initialFrames + "\n", Preferences.DEBUG_FILEIO);
            streams = getInt(endianess);
            Preferences.debug("Number of streams: " + streams + "\n", Preferences.DEBUG_FILEIO);

            int suggestedBufferSize = getInt(endianess);
            Preferences.debug("suggestedBufferSize = " + suggestedBufferSize + "\n", Preferences.DEBUG_FILEIO);
            width = getInt(endianess); // xDim
            Preferences.debug("width = " + width + "\n", Preferences.DEBUG_FILEIO);
            height = getInt(endianess); // yDim
            Preferences.debug("height = " + height + "\n", Preferences.DEBUG_FILEIO);

            // read 4 reserved integers
            for (int i = 0; i < 4; i++) {
                getInt(endianess);
            }

            for (loop = 0; loop < streams; loop++) {

                // read the LIST subCHUNK
                CHUNKsignature = getInt(endianess);
                
                while ((CHUNKsignature == 0x6E727473 /* strn */) || (CHUNKsignature == 0x64727473 /* strd */) ||
                       (CHUNKsignature == 0x4B4E554A /* JUNK */)) {
                    // read strn instead of CHUNK
                    int strnLength = getInt(endianess);
                    if ((strnLength % 2) == 1) {
                        strnLength++;
                    }
                    byte[] text = new byte[strnLength];
                    raFile.read(text);

                    CHUNKsignature = getInt(endianess);
                } // ((CHUNKsignature == 0x6E727473 /* strn */) || (CHUNKsignature == 0x64727473 /* strd */) ||

                if (CHUNKsignature == 0x5453494C) {
                    // have read LIST for LIST subCHUNK with information on data decoding
                } else {
                    raFile.close();
                    throw new IOException("AVI read header error signature first LIST subCHUNK = " + CHUNKsignature);
                }

                int LISTsubchunkSize = getInt(endianess); // size of the first LIST subCHUNK not including
                LISTsubchunkMarker = raFile.getFilePointer();

                // the first 8 signature and length bytes
                CHUNKtype = getInt(endianess);

                if (CHUNKtype == 0x6C727473) {
                    // have read strl
                } else {
                    raFile.close();
                    throw new IOException("AVI read header error no strl in first LIST subCHUNK but = " + CHUNKtype);
                }

                // read the strh subCHUNK
                int strhSignature = getInt(endianess);

                if (strhSignature == 0x68727473) {
                    // have read strh
                } else {
                    raFile.close();
                    throw new IOException("AVI read header error no strhSignature found but = " + strhSignature);
                }

                int strhLength = getInt(endianess); // length of strh subCHUNK not including first 8

                // signature and length bytes
                // AVI standard documentation mentioned a minimum length of 56,
                // but the Windows Media player was observed to play 5 mjpeg
                // files with strhLength = 48.
                if (strhLength < 48) {
                    raFile.close();
                    throw new IOException("AVI read header error with strhLength = " + strhLength);
                }

                int fccType = getInt(endianess);

                if (fccType == 0x73646976) {
                    // vids read for video stream
                } else if (streams > 1) {
                    raFile.seek(LISTsubchunkSize + LISTsubchunkMarker);

                    continue;
                } else {
                    raFile.close();
                    throw new IOException("AVI read header error with fccType = " + fccType);
                }

                int handler = getInt(endianess);

                byte[] handlerByte = new byte[5];
                handlerByte[0] = (byte) (handler & 0xFF);
                handlerByte[1] = (byte) ((handler >> 8) & 0xFF);
                handlerByte[2] = (byte) ((handler >> 16) & 0xFF);
                handlerByte[3] = (byte) ((handler >> 24) & 0xFF);
                handlerByte[4] = (byte) 0;

                String handlerString = new String(handlerByte);

                System.err.println("Handler String is: " + handlerString);

                if ((handler == 0x20424944 /* DIB<sp> */) || (handler == 0x20424752 /* RGB<sp> */) ||
                        (handler == 0x20574152 /* RAW<sp> */) || (handler == 0x00000000) ||
                        (handlerString.startsWith("00dc"))) {
                    // uncompressed data
                    Preferences.debug("Uncompressed data\n", Preferences.DEBUG_FILEIO);
                } else if (handlerString.toUpperCase().startsWith("MRLE") ||
                               handlerString.toUpperCase().startsWith("RLE")) {
                    Preferences.debug("Microsoft run length encoding\n", Preferences.DEBUG_FILEIO);
                    /* mrle microsoft run length encoding */
                } else if (handlerString.toUpperCase().startsWith("MSVC")) {
                    Preferences.debug("Microsoft video 1 compression\n", Preferences.DEBUG_FILEIO);
                    // Microsoft video 1 compression
                    doMSVC = true;
                } else if (handlerString.toUpperCase().startsWith("CVID")) {
                    Preferences.debug("Cinepak (CVID) encoding\n", Preferences.DEBUG_FILEIO);
                    doCVID = true;
                } else if (handlerString.toUpperCase().startsWith("CYUV")) {
                    Preferences.debug("Creative YUV (CYUV) encoding\n", Preferences.DEBUG_FILEIO);
                    doCYUV = true;
                } else if (handlerString.toUpperCase().startsWith("MP42")) {
                    return AlgorithmTranscode.TRANSCODE_MP42;
                } else if ((handlerString.toUpperCase().startsWith("MJPG")) ||
                           (handlerString.toUpperCase().startsWith("AVRN")) ||
                           (handlerString.toUpperCase().startsWith("LJPG"))) {
                    Preferences.debug("MJPEG (MJPG) encoding\n", Preferences.DEBUG_FILEIO);
                    doMJPEG = true;
                } else if (handlerString.toUpperCase().startsWith("DIV")) {
                    return AlgorithmTranscode.TRANSCODE_DIVX;
                } else if (handlerString.toUpperCase().startsWith("MPG4")) {
                    return AlgorithmTranscode.TRANSCODE_MPG4;
                } else if (handlerString.toUpperCase().startsWith("IV32")) {
                    return AlgorithmTranscode.TRANSCODE_IV32;
                } else if (handlerString.toUpperCase().startsWith("IV41")) {
                    return AlgorithmTranscode.TRANSCODE_IV41;
                } else if (handlerString.toUpperCase().startsWith("IV50")) {
                    return AlgorithmTranscode.TRANSCODE_IV50;
                } else if (handlerString.toUpperCase().startsWith("GEOV")) {
                    return AlgorithmTranscode.TRANSCODE_GEOV;
                } else {
                    raFile.close();
                    throw new IOException("Unrecognized compression handler is " + handlerString);
                    // tscc is the TechSmith Screen Capture Codec put out by the Techsmith Corporation for use with
                    // their Camtasia Screen "Camcorder" application.  Camtasia is a Microsoft windows application only.
                    //  The company has no plans to develop a version for the apple. Could the program be designed to
                    // use the tscc.exe codec provided by Techsmith?
                }

                flags = getInt(endianess);

                if ((flags & 0x00000001) != 0) {
                    raFile.close();
                    throw new IOException("Cannot presently handle AVISF_DISABLED");
                }

                if ((flags & 0x00010000) != 0) {
                    raFile.close();
                    throw new IOException("Cannot presently handle AVISF_VIDEO_PALCHANGES");
                }

                int priority = getInt(endianess);
                Preferences.debug("priority = " + priority + "\n", Preferences.DEBUG_FILEIO);
                initialFrames = getInt(endianess);

                if (initialFrames != 0) {
                    raFile.close();
                    throw new IOException("initialFrames should be 0 for noninterleaved files");
                }

                scale = getInt(endianess);
                Preferences.debug("scale = " + scale + "\n", Preferences.DEBUG_FILEIO);

                // System.err.println("Scale is: " + scale);
                rate = getInt(endianess);

                // System.err.println("Rate is: " + rate);
                Preferences.debug("rate = " + rate + "\n", Preferences.DEBUG_FILEIO);

                int start = getInt(endianess);
                Preferences.debug("start = " + start + "\n", Preferences.DEBUG_FILEIO);

                int length = getInt(endianess);
                Preferences.debug("length = " + length + "\n", Preferences.DEBUG_FILEIO);

                // System.err.println("DWLength: " + length);
                suggestedBufferSize = getInt(endianess);
                Preferences.debug("suggestedBufferSize = " + suggestedBufferSize + "\n", Preferences.DEBUG_FILEIO);

                int quality = getInt(endianess);

                if ((quality > 10000) || (quality < -1)) {
                    raFile.close();
                    throw new IOException("quality = " + quality);
                }

                Preferences.debug("quality = " + quality + "\n", Preferences.DEBUG_FILEIO);

                int sampleSize = getInt(endianess);
                Preferences.debug("sampleSize = " + sampleSize + "\n", Preferences.DEBUG_FILEIO);

                // read destination rectangle within movie rectangle
                short left = (short) getSignedShort(endianess);
                Preferences.debug("left = " + left + "\n");

                short top = (short) getSignedShort(endianess);
                Preferences.debug("top = " + top + "\n", Preferences.DEBUG_FILEIO);

                short right = (short) getSignedShort(endianess);
                Preferences.debug("right = " + right + "\n");

                short bottom = (short) getSignedShort(endianess);
                Preferences.debug("bottom = " + bottom + "\n", Preferences.DEBUG_FILEIO);

                if (strhLength > 56) {
                    byte[] extra = new byte[strhLength - 56];
                    raFile.read(extra);
                }
                
                long strfPos = raFile.getFilePointer();
                if ((left == 29811) && (top == 26226)) {
                    // The left, top, right, and bottom  fields have been omitted
                    raFile.seek(strfPos - 8);
                }

                // read the stream format CHUNK
                int strfSignature = getInt(endianess);

                if (strfSignature == 0x66727473) {
                    // read strf
                } else {
                    raFile.close();
                    throw new IOException("strf signature incorrectly read as = " + strfSignature);
                }

                int strfSize = getInt(endianess);
                Preferences.debug("strfSize = " + strfSize + "\n", Preferences.DEBUG_FILEIO);
                int BITMAPINFOsize = getInt(endianess);
                Preferences.debug("BITMAPINFOsize = " + BITMAPINFOsize + "\n", Preferences.DEBUG_FILEIO);

                if (BITMAPINFOsize > strfSize) {
                    BITMAPINFOsize = strfSize;
                }

                if (BITMAPINFOsize < 40) {
                    raFile.close();
                    throw new IOException("Cannot handle BITMAPINFO size = " + BITMAPINFOsize);
                }

                width = getInt(endianess);
                Preferences.debug("width = " + width + "\n", Preferences.DEBUG_FILEIO);
                height = getInt(endianess);
                Preferences.debug("height = " + height + "\n", Preferences.DEBUG_FILEIO);

                short planes = (short) getSignedShort(endianess);

                if (planes != 1) {
                    raFile.close();
                    throw new IOException("planes has an incorrect value = " + planes);
                }

                bitCount = (short) getSignedShort(endianess);
                Preferences.debug("bitCount = " + bitCount + "\n", Preferences.DEBUG_FILEIO);

                compression = getInt(endianess);

                if (compression == 0) {
                    Preferences.debug("Compression is BI_RGB\n", Preferences.DEBUG_FILEIO);
                    // BI_RGB uncompressed
                } else if (compression == 1) {
                    Preferences.debug("Compression is BI_RLE8\n", Preferences.DEBUG_FILEIO);
                    // BI_RLE8
                } else if (compression == 2) {

                    // BI_RLE4
                    raFile.close();
                    throw new IOException("Cannot currently handle 4 bit run length encoding");
                } else if (compression == 3) {
                    // BI_BITFIELDS
                    // To allow for arbitrarily packed RGB samples, BI_BITFIELDS specifies a
                    // mask field for each of the red, green, and blue pixel components.
                    // These masks indicate the bit positions occupied by each color
                    // component in a pixel.  In general, the masks are passed to a driver
                    // or video API using means other than a basic BITMAPINFOHEADER(such
                    // as using the appropriate fields in a DirectDraw DDPIXELFORMAT
                    // structure) but it might be valid to append the masks to the end of
                    // the BITMAPINFOHEADER in much the same way that a palette is appended
                    // for palettised formats.
                    //
                    // For example, 16 bit RGB 5:6:5 can be described using BI_BITFIELDS
                    // and the following bitmasks:

                    // Red  0xF800 (5 bits of red)
                    // Green 0x07E0 (6 bits of green)
                    // Blue  0x001F (5 bits of blue)

                    // In this case, if used with a BITMAPINFOHEADER, the bitmasks are
                    // u_int16s (16 bit) since the biBitFields field is set to 16.  For
                    // a 32bpp version, the bitmasks are each u_int32s.
                    raFile.close();
                    throw new IOException("Cannot currently handle BI_BITFIELDS compresion");
                } else if (compression == 1296126531) {
                    Preferences.debug("compression is Microsoft video 1\n", Preferences.DEBUG_FILEIO);
                    doMSVC = true;
                } else if (compression == 1684633187) {
                    Preferences.debug("compression is Cinepak (CVID)\n", Preferences.DEBUG_FILEIO);
                    doCVID = true;
                } else if (compression == 1987410275) {
                    Preferences.debug("compression is Creative YUV (CYUV)\n", Preferences.DEBUG_FILEIO);
                    doCYUV = true;
                } else if (compression == 1196444237) {
                    Preferences.debug("compression is MJPEG (MJPG)\n", Preferences.DEBUG_FILEIO);
                    doMJPEG = true;
                } else if (compression == 1196444233) {
                    Preferences.debug("compression is IJPG (MJPG)\n", Preferences.DEBUG_FILEIO);
                    doMJPEG = true;
                } else if (compression == 1850889793) {
                    Preferences.debug("compression is AVRn\n", Preferences.DEBUG_FILEIO);
                    doMJPEG = true;
                } else if (compression == 1279742026) {
                    Preferences.debug("compression is LJPG\n", Preferences.DEBUG_FILEIO);
                    doMJPEG = true;
                } else {
                    raFile.close();
                    throw new IOException("Unknown compression with value = " + compression);
                }

                Preferences.debug("compression = " + compression + "\n", Preferences.DEBUG_FILEIO);

                if (((compression == 0) &&
                         ((bitCount == 4) || (bitCount == 8) || (bitCount == 16) || (bitCount == 24) || (bitCount == 32))) ||
                        ((compression == 1) && (bitCount == 8)) || (doMSVC && (bitCount == 8)) ||
                        (doMSVC && (bitCount == 16)) || (doCVID && (bitCount == 8)) || (doCVID && (bitCount == 24)) || 
                        (doCVID && (bitCount == 40)) || (doCYUV && (bitCount == 16)) ||
                        (doMJPEG && (bitCount == 24))) {
                    // OK
                } else {
                    raFile.close();
                    throw new IOException("Cannot currently handle bit count = " + bitCount);
                }
                
                if (doCVID && (bitCount == 40)) {
                    // Greyscale depth 40(really depth 8) cinepak encoded AVI file
                    // The depth 40 is an artifact of it being converted from quicktime where 
                    // depth 40 is really depth 8 greyscale.  Must be handled with
                    // 24 bit cvid decoding forming 8, 8, 8 image with equal red, green, and blue.
                    bitCount = 24;
                }

                int imageSize = getInt(endianess);
                Preferences.debug("imageSize = " + imageSize + "\n", Preferences.DEBUG_FILEIO);

                float[] imgResols = new float[5];
                imgResols[0] = imgResols[1] = imgResols[2] = imgResols[3] = imgResols[4] = 1.0f;

                int xPixelsPerMeter = getInt(endianess);
                Preferences.debug("xPixelsPerMeter = " + xPixelsPerMeter + "\n", Preferences.DEBUG_FILEIO);

                // System.err.println("xPixelsPerMeter = " + xPixelsPerMeter);
                if (xPixelsPerMeter > 0) {
                    fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 0);
                    imgResols[0] = (1.0f / xPixelsPerMeter) * 1000.0f;
                }

                int yPixelsPerMeter = getInt(endianess);
                Preferences.debug("yPixelsPerMeter = " + yPixelsPerMeter + "\n", Preferences.DEBUG_FILEIO);

                // System.err.println("yPixelsPerMeter = " + yPixelsPerMeter);
                if (yPixelsPerMeter > 0) {
                    fileInfo.setUnitsOfMeasure(FileInfoBase.MILLIMETERS, 1);
                    imgResols[1] = (1.0f / yPixelsPerMeter) * 1000.0f;
                }

                imgResols[2] = microSecPerFrame;

                // System.err.println("Microseconds per frame (on read): " + microSecPerFrame);
                fileInfo.setUnitsOfMeasure(FileInfoBase.MICROSEC, 2);
                fileInfo.setResolutions(imgResols);

                int colorsUsed = getInt(endianess);
                Preferences.debug("colorsUsed = " + colorsUsed + "\n", Preferences.DEBUG_FILEIO);

                if ((compression == 0) && ((bitCount == 24) || (bitCount == 32)) && (colorsUsed != 0)) {
                    raFile.close();
                    throw new IOException("For 24 and 32 bit uncompressed data software does not currently support colorsUsed = " +
                                          colorsUsed);
                }

                if ((bitCount == 8) && (colorsUsed == 0)) {
                    colorsUsed = 8;
                }

                int colorsImportant = getInt(endianess);
                Preferences.debug("colorsImportant = " + colorsImportant + "\n", Preferences.DEBUG_FILEIO);

                if (BITMAPINFOsize > 40) {
                    byte[] extra = new byte[BITMAPINFOsize - 40];
                    raFile.read(extra);
                }

                if (bitCount == 4) {

                    // read the color table into a LUT
                    int[] dimExtentsLUT = new int[2];
                    dimExtentsLUT[0] = 4;
                    dimExtentsLUT[1] = 256;

                    // FileIO obtains via getModelLUT.
                    // Then, ViewOpenFileUI obtains from FileIO via getModelLUT.
                    LUTa = new ModelLUT(ModelLUT.GRAY, colorsUsed, dimExtentsLUT);
                    lutBuffer = new byte[4 * colorsUsed];
                    raFile.read(lutBuffer);

                    for (int i = 0; i < colorsUsed; i++) {
                        LUTa.set(0, i, 1.0f); // alpha
                        LUTa.set(1, i, (lutBuffer[(4 * i) + 2] & 0x000000ff)); // red
                        LUTa.set(2, i, (lutBuffer[(4 * i) + 1] & 0x000000ff)); // green
                        LUTa.set(3, i, (lutBuffer[4 * i] & 0x000000ff)); // blue
                    } // for (i = 0; i < colorsUsed; i++)

                    for (int i = colorsUsed; i < 256; i++) {
                        LUTa.set(0, i, 1.0f);
                        LUTa.set(1, i, 0);
                        LUTa.set(2, i, 0);
                        LUTa.set(3, i, 0);
                    } // for (i = colorsUsed; i < 256; i++)

                    LUTa.makeIndexedLUT(null);
                } // if (bitCount == 4)
                else if (bitCount == 8) {

                    // read the color table into a LUT
                    int[] dimExtentsLUT = new int[2];
                    dimExtentsLUT[0] = 4;
                    dimExtentsLUT[1] = 256;

                    // FileIO obtains via getModelLUT.
                    // Then, ViewOpenFileUI obtains from FileIO via getModelLUT.
                    LUTa = new ModelLUT(ModelLUT.GRAY, colorsUsed, dimExtentsLUT);
                    lutBuffer = new byte[4 * colorsUsed];
                    raFile.read(lutBuffer);

                    for (int i = 0; i < colorsUsed; i++) {
                        LUTa.set(0, i, 1.0f); // alpha
                        LUTa.set(1, i, (lutBuffer[(4 * i) + 2] & 0x000000ff)); // red
                        LUTa.set(2, i, (lutBuffer[(4 * i) + 1] & 0x000000ff)); // green
                        LUTa.set(3, i, (lutBuffer[4 * i] & 0x000000ff)); // blue
                    } // for (i = 0; i < colorsUsed; i++)

                    for (int i = colorsUsed; i < 256; i++) {
                        LUTa.set(0, i, 1.0f);
                        LUTa.set(1, i, 0);
                        LUTa.set(2, i, 0);
                        LUTa.set(3, i, 0);
                    } // for (i = colorsUsed; i < 256; i++)

                    LUTa.makeIndexedLUT(null);
                } // else if (bitCount == 8)

                // Calculate the number of strf CHUNK bytes after the end of BITMAPINFO
                int strfEndBytes = strfSize - BITMAPINFOsize;

                if ((bitCount == 4) ||(bitCount == 8)) {
                    strfEndBytes = strfEndBytes - (4 * colorsUsed);
                }

                for (int i = 0; i < strfEndBytes; i++) {
                    raFile.readUnsignedByte();
                }
            } // for (loop = 0; loop < streams; loop++)

            marker = raFile.getFilePointer();

            if (marker < (LIST1Marker + LIST1Size)) {

                // read strn subCHUNK
                int strnSignature = getInt(endianess);

                if (strnSignature == 0x6E727473) {
                    int strnLength = getInt(endianess);
                    if ((strnLength % 2) == 1) {
                        strnLength++;
                    }
                    byte[] text = new byte[strnLength];
                    raFile.read(text);

                    if (text[strnLength - 1] != 0) {
                        raFile.close();
                        throw new IOException("strn string ends with illegal temination = " + text[strnLength - 1]);
                    }
                } // if (strnSignature == 0x6E727473)
                else if (strnSignature == 0x4B4E554A) {

                    // have read JUNK for a JUNK padding CHUNK
                    int JUNKlength = getInt(endianess);
                    marker = raFile.getFilePointer();
                    raFile.seek(marker + JUNKlength);
                    CHUNKsignature = getInt(endianess);

                    if (CHUNKsignature != 0x5453494C) {
                        raFile.close();
                        throw new IOException("After JUNK CHUNK unexpected signature = " + CHUNKsignature);
                    }
                } // else if (strnSignature == 0x4B4E554A)
                else if (strnSignature == 0x54465349) {

                    // have read ISFT
                    int ISFTlength = getInt(endianess);
                    if ((ISFTlength % 2) == 1) {
                        ISFTlength++;
                    }
                    marker = raFile.getFilePointer();
                    raFile.seek(marker + ISFTlength);
                } // else if (strnSignature == 0x54465349)
                else if (strnSignature == 0x54494449) {

                    // have read IDIT
                    int IDITlength = getInt(endianess);
                    if ((IDITlength % 2) == 1) {
                        IDITlength++;
                    }
                    marker = raFile.getFilePointer();
                    raFile.seek(marker + IDITlength);
                } // else if (strnSignature == 0x54494449)
                else if (strnSignature == 0x74646576) {

                    // have read vedt
                    int vedtLength = getInt(endianess);
                    if ((vedtLength %2) == 1) {
                        vedtLength++;
                    }
                    byte[] vedt = new byte[vedtLength];
                    raFile.read(vedt);
                } else if (strnSignature == 0x5453494C) {
                    // have read LIST
                    int listLength = getInt(endianess);
                    if ((listLength % 2) == 1) {
                        listLength++;
                    }
                    marker = raFile.getFilePointer();
                    raFile.seek(marker + listLength);
                } else if (strnSignature == 0x78646E69) {
                    // have read icdx
                    int icdxLength = getInt(endianess);
                    if ((icdxLength % 2) == 1) {
                        icdxLength++;
                    }
                    marker = raFile.getFilePointer();
                    raFile.seek(marker + icdxLength);
                } else {
                    raFile.close();
                    throw new IOException("strn signature is an erroneous = " + strnSignature);
                }
            }

            raFile.seek(LIST1Marker + LIST1Size);
            do {
                signature = getInt(endianess);
                if (signature == 0x4B4E554A) {
                    // have read JUNK for a JUNK padding CHUNK
                    int JUNKlength = getInt(endianess);
                    marker = raFile.getFilePointer();
                    raFile.seek(marker + JUNKlength);    
                }
                else if (signature != 0x5453494C /* LIST */) {
                    raFile.close();
                    throw new IOException("After first LIST CHUNK unexpected signature = " + signature);
                }
                else {
                    // At this point have read LIST for the second LIST CHUNK which contains the actual data.
                    LIST2Size = getInt(endianess);
                    moviPosition = raFile.getFilePointer(); 
                    CHUNKtype = getInt(endianess);
                    if (CHUNKtype == 0x4F464E49 /* INFO */) {
                        raFile.seek(moviPosition + LIST2Size);        
                    }
                    else {
                        idx1Position = moviPosition + LIST2Size;
                        indexPointer = idx1Position + 8; 
                        if ((idx1Position + 4) < raFile.length()) {
                            raFile.seek(idx1Position + 4);
                            indexSize = getInt(endianess);
                        }
                        raFile.seek(moviPosition + 4);
                    }
                }
            } while ((signature == 0x4B4E554A /* JUNK */) || 
                     ((signature == 0x5453494C /* LIST */) && (CHUNKtype == 0x4F464E49 /* INFO */)));

            if (CHUNKtype != 0x69766F6D) {

                // have not read movi
                raFile.close();
                throw new IOException("CHUNK type in second LIST CHUNK is an illegal = " + CHUNKtype);
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }
        Preferences.debug("Finished readHeader\n", Preferences.DEBUG_FILEIO);
        return 0;
    }

    /**
     * Writes the AVI BGR triplet.
     *
     * @param   timeSlice     t (time) slice to show
     * @param   slice         z slice to show
     * @param   LUTb          DOCUMENT ME!
     * @param   red           DOCUMENT ME!
     * @param   green         DOCUMENT ME!
     * @param   blue          DOCUMENT ME!
     * @param   opacityPrime  DOCUMENT ME!
     * @param   alphaBlend    DOCUMENT ME!
     * @param   paintBitmap   DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void writeAVITriplet(int timeSlice, int slice, ModelLUT LUTb, int red, int green, int blue,
                                 float opacityPrime, float alphaBlend, BitSet paintBitmap) throws IOException {

        float alphaPrime = 1 - alphaBlend;
        float rangeA = 0;
        float rangeB = 0;
        float remapConstB = 1;
        float imageMaxA;
        float imageMinB;
        float imageMaxB;
        int bufferSize, bufferAdr;
        int lutHeightA = 0;
        int lutHeightB = 0;
        float[][] RGB_LUTa = null;
        float[][] RGB_LUTb = null;
        float Ra, Ga, Ba, Rb, Gb, Bb;
        int indexA, indexB;

        if (imageA == null) {
            MipavUtil.displayError("imageA is null");

            return;
        }

        if (LUTa == null) {
            MipavUtil.displayError("LUTa is null");

            return;
        }

        lutHeightA = LUTa.getExtents()[1];

        if (LUTb != null) {
            lutHeightB = LUTb.getExtents()[1];
        }

        if (lutHeightA != lutBufferRemapped.length) {

            try {
                lutBufferRemapped = new int[lutHeightA];
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: FileAvi.writeAVITriplet");

                return;
            }
        }

        if (imageB == null) {
            lutBufferRemapped = LUTa.exportIndexedLUT();
        }

        bufferSize = (xDim - xPad) * yDim;
        bufferAdr = 0;

        if (imageA.getType() == ModelStorageBase.UBYTE) {
            imageMinA = 0;
            imageMaxA = 255;
        } else if (imageA.getType() == ModelStorageBase.BYTE) {
            imageMinA = -128;
            imageMaxA = 127;
        } else {
            imageMinA = (float) imageA.getMin();
            imageMaxA = (float) imageA.getMax();
        }

        rangeA = imageMaxA - imageMinA;

        if (rangeA == 0) {
            rangeA = 1;
        }

        if ((lutHeightA - 1) == 0) {
            remapConstA = 1;
        } else if (rangeA < 255) {
            remapConstA = 1;
        } else {
            remapConstA = (lutHeightA - 1) / rangeA;
        }

        if (imageB != null) {

            // imageMinB    = (float)imageB.getMin();
            // imageMaxB    = (float)imageB.getMax();
            if (imageA.getType() == ModelStorageBase.UBYTE) {
                imageMinB = 0;
                imageMaxB = 255;
            } else if (imageA.getType() == ModelStorageBase.BYTE) {
                imageMinB = -128;
                imageMaxB = 127;
            } else {
                imageMinB = (float) imageB.getMin();
                imageMaxB = (float) imageB.getMax();
            }

            rangeB = imageMaxB - imageMinB;

            if (rangeB == 0) {
                rangeB = 1;
            }

            if ((lutHeightB - 1) == 0) {
                remapConstB = 1;
            } else {
                remapConstB = (lutHeightB - 1) / rangeB;
            }

            RGB_LUTa = LUTa.exportRGB_LUT(true);
            RGB_LUTb = LUTb.exportRGB_LUT(true);
        }

        int zDimSlices = 0;

        if (imageA.getNDims() >= 3) {
            zDimSlices = zDim;
        }

        try {

            // if (imageA.getType() == ModelStorageBase.DCOMPLEX)
            // imageA.ExportDComplexSliceXY(timeSlice*zDimSlices + slice,imageBufferA, logMagDisplay);
            if (imageA.getType() == ModelStorageBase.COMPLEX) {
                imageA.exportComplexSliceXY((timeSlice * zDimSlices) + slice, imageBufferA, false);
            } else {
                imageA.exportSliceXY((timeSlice * zDimSlices) + slice, imageBufferA);
            }

            if (imageB != null) {

                // if (imageB.getType() == ModelStorageBase.DCOMPLEX)
                // imageB.exportDComplexSliceXY(timeSlice*zDimSlices + slice,imageBufferB, logMagDisplay);
                if (imageB.getType() == ModelStorageBase.COMPLEX) {
                    imageB.exportComplexSliceXY((timeSlice * zDimSlices) + slice, imageBufferB, false);
                } else {
                    imageB.exportSliceXY((timeSlice * zDimSlices) + slice, imageBufferB);
                }
            }
        } catch (IOException error) {
            MipavUtil.displayError("" + error); // Need to fix this

            return;
        }

        int index, pix;

        if (imageB == null) {
            int offset = slice * bufferSize;
            int value;

            for (int y = yDim - 1; y >= 0; y--) {

                for (int x = 0; x < (xDim - xPad); x++) {
                    index = x + (y * (xDim - xPad));
                    pix = (short) (((imageBufferA[index] - imageMinA) * remapConstA) + 0.5);

                    if (paintBitmap.get(offset + index) == true) {
                        value = lutBufferRemapped[pix];
                        Ra = (value & 0x00ff0000) >> 16;
                        Ga = (value & 0x0000ff00) >> 8;
                        Ba = (value & 0x000000ff);
                        bufferWrite[bufferAdr] = (byte) ((Ba * opacityPrime) + blue);
                        bufferWrite[bufferAdr + 1] = (byte) ((Ga * opacityPrime) + green);
                        bufferWrite[bufferAdr + 2] = (byte) ((Ra * opacityPrime) + red);
                        bufferAdr = bufferAdr + 3;
                    } else {
                        bufferWrite[bufferAdr] = (byte) (lutBufferRemapped[pix] & 0x000000ff);
                        bufferWrite[bufferAdr + 1] = (byte) ((lutBufferRemapped[pix] & 0x0000ff00) >> 8);
                        bufferWrite[bufferAdr + 2] = (byte) ((lutBufferRemapped[pix] & 0x00ff0000) >> 16);
                        bufferAdr = bufferAdr + 3;
                    }
                } // for (x = 0; x < xDim - xPad; x++)

                for (int x = 0; x < xPad; x++) {
                    bufferWrite[bufferAdr] = (byte) 0;
                    bufferWrite[bufferAdr + 1] = (byte) 0;
                    bufferWrite[bufferAdr + 2] = (byte) 0;
                    bufferAdr = bufferAdr + 3;
                } // for (x = 0; x < xPad; x++)
            } // for (y = yDim - 1; y >= 0; y--)
        } // if (imageB == null )
        else {
            int offset = slice * bufferSize;

            for (int y = yDim - 1; y >= 0; y--) {

                for (int x = 0; x < (xDim - xPad); x++) {
                    index = x + (y * (xDim - xPad));
                    indexA = (short) (((imageBufferA[index] - imageA.getMin()) * remapConstA) + 0.5);
                    indexB = (short) (((imageBufferB[index] - imageB.getMin()) * remapConstB) + 0.5);
                    Ra = RGB_LUTa[0][indexA];
                    Rb = RGB_LUTb[0][indexB];
                    Ga = RGB_LUTa[1][indexA];
                    Gb = RGB_LUTb[1][indexB];
                    Ba = RGB_LUTa[2][indexA];
                    Bb = RGB_LUTb[2][indexB];

                    Ra = (int) ((Ra * alphaBlend) + (Rb * alphaPrime));
                    Ga = (int) ((Ga * alphaBlend) + (Gb * alphaPrime));
                    Ba = (int) ((Ba * alphaBlend) + (Bb * alphaPrime));

                    if (paintBitmap.get(offset + index) == true) {
                        bufferWrite[bufferAdr] = (byte) ((Ba * opacityPrime) + blue);
                        bufferWrite[bufferAdr + 1] = (byte) ((Ga * opacityPrime) + green);
                        bufferWrite[bufferAdr + 2] = (byte) ((Ra * opacityPrime) + red);
                        bufferAdr = bufferAdr + 3;
                    } else {
                        bufferWrite[bufferAdr] = (byte) Ba;
                        bufferWrite[bufferAdr + 1] = (byte) Ga;
                        bufferWrite[bufferAdr + 2] = (byte) Ra;
                        bufferAdr = bufferAdr + 3;
                    }
                } // for (x = 0; x < xDim-xPad; x++)

                for (int x = 0; x < xPad; x++) {
                    bufferWrite[bufferAdr] = (byte) 0;
                    bufferWrite[bufferAdr + 1] = (byte) 0;
                    bufferWrite[bufferAdr + 2] = (byte) 0;
                    bufferAdr = bufferAdr + 3;
                } // for (x = 0; x < xPad; x++)
            } // for (y = yDim - 1; y >= 0; y--)
        }

        raFile.write(bufferWrite);
    }

    /**
     * Write BGR AVI triplet for color image.
     *
     * @param   timeSlice     t (time) slice to show
     * @param   slice         z slice to show
     * @param   RGBTA         DOCUMENT ME!
     * @param   RGBTB         DOCUMENT ME!
     * @param   red           DOCUMENT ME!
     * @param   green         DOCUMENT ME!
     * @param   blue          DOCUMENT ME!
     * @param   opacityPrime  DOCUMENT ME!
     * @param   alphaBlend    DOCUMENT ME!
     * @param   paintBitmap   DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void writeAVITripletC(int timeSlice, int slice, ModelRGB RGBTA, ModelRGB RGBTB, int red, int green,
                                  int blue, float opacityPrime, float alphaBlend, BitSet paintBitmap)
            throws IOException {
        // Note that alphaBlending is applied with 1 component taken as zero if both components
        // are not present - for example, if either imageA or imageB but not both has red, then
        // the red component is alphaBlended with zero.

        int j;
        int bufferSize;
        int offset;
        int index;
        float Ra, Ga, Ba, Rb, Gb, Bb;
        int imageSize;
        float redMapped, greenMapped, blueMapped;
        int[] RGBIndexBufferA;
        int[] RGBIndexBufferB;
        int x, y;
        int bufferAdr;
        float alphaPrime = 1 - alphaBlend;

        bufferSize = (xDim - xPad) * yDim * 4;
        imageSize = (xDim - xPad) * yDim;
        bufferAdr = 0;

        try {
            RGBIndexBufferA = new int[256];
            RGBIndexBufferB = new int[256];
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Error while writing AVI file.");

            return;
        }

        if (RGBTA != null) {
            RGBIndexBufferA = RGBTA.exportIndexedRGB();
        }

        if ((imageB != null) && (RGBTB != null)) {
            RGBIndexBufferB = RGBTB.exportIndexedRGB();
        }

        int zDimSlices = 0;

        if (imageA.getNDims() >= 3) {
            zDimSlices = zDim;
        }

        try {
            imageA.exportData(((timeSlice * zDimSlices * bufferSize) + (slice * bufferSize)), bufferSize, imageBufferA);

            if (imageB != null) {
                imageB.exportData(((timeSlice * zDimSlices * bufferSize) + (slice * bufferSize)), bufferSize,
                                  imageBufferB);
            }
        } catch (IOException error) {
            MipavUtil.displayError("" + error);

            return;
        }

        if (imageB == null) {
            offset = slice * imageSize;

            for (y = yDim - 1; y >= 0; y--) {

                for (x = 0; x < (xDim - xPad); x++) {
                    j = x + (y * (xDim - xPad));
                    index = 4 * j;

                    if (RGBTA != null) {

                        if (RGBTA.getROn()) {
                            redMapped = (RGBIndexBufferA[(int) imageBufferA[index + 1]] & 0x00ff0000) >> 16;
                        } else {
                            redMapped = 0;
                        }

                        if (RGBTA.getGOn()) {
                            greenMapped = (RGBIndexBufferA[(int) imageBufferA[index + 2]] & 0x0000ff00) >> 8;
                        } else {
                            greenMapped = 0;
                        }

                        if (RGBTA.getBOn()) {
                            blueMapped = (RGBIndexBufferA[(int) imageBufferA[index + 3]] & 0x000000ff);
                        } else {
                            blueMapped = 0;
                        }
                    } // end of if (RGBTA != null)
                    else {
                        redMapped = imageBufferA[index + 1];
                        greenMapped = imageBufferA[index + 2];
                        blueMapped = imageBufferA[index + 3];
                    }

                    if (paintBitmap.get(offset + j) == true) {
                        bufferWrite[bufferAdr] = (byte) ((blueMapped * opacityPrime) + blue);
                        bufferWrite[bufferAdr + 1] = (byte) ((greenMapped * opacityPrime) + green);
                        bufferWrite[bufferAdr + 2] = (byte) ((redMapped * opacityPrime) + red);
                        bufferAdr = bufferAdr + 3;
                    } else {
                        bufferWrite[bufferAdr] = (byte) blueMapped;
                        bufferWrite[bufferAdr + 1] = (byte) greenMapped;
                        bufferWrite[bufferAdr + 2] = (byte) redMapped;
                        bufferAdr = bufferAdr + 3;
                    }
                } // for (x = 0; x < xDim-xPad; x++)

                for (x = 0; x < xPad; x++) {
                    bufferWrite[bufferAdr] = (byte) 0;
                    bufferWrite[bufferAdr + 1] = (byte) 0;
                    bufferWrite[bufferAdr + 2] = (byte) 0;
                    bufferAdr = bufferAdr + 3;
                } // for (x = 0; x < xPad; x++)
            } // for (y = yDim - 1; y >= 0; y--)
        } // if (imageB == null )
        else {
            offset = slice * bufferSize;

            for (y = yDim - 1; y >= 0; y--) {

                for (x = 0; x < (xDim - xPad); x++) {
                    j = x + (y * (xDim - xPad));
                    index = 4 * j;

                    if ((RGBTA != null) && (RGBTB != null)) {

                        if (RGBTA.getROn()) {
                            Ra = (RGBIndexBufferA[(int) imageBufferA[index + 1]] & 0x00ff0000) >> 16;
                        } else {
                            Ra = 0;
                        }

                        if (RGBTB.getROn()) {
                            Rb = (RGBIndexBufferB[(int) imageBufferB[index + 1]] & 0x00ff0000) >> 16;
                        } else {
                            Rb = 0;
                        }

                        if (RGBTA.getGOn()) {
                            Ga = (RGBIndexBufferA[(int) imageBufferA[index + 2]] & 0x0000ff00) >> 8;
                        } else {
                            Ga = 0;
                        }

                        if (RGBTB.getGOn()) {
                            Gb = (RGBIndexBufferB[(int) imageBufferB[index + 2]] & 0x0000ff00) >> 8;
                        } else {
                            Gb = 0;
                        }

                        if (RGBTA.getBOn()) {
                            Ba = (RGBIndexBufferA[(int) imageBufferA[index + 3]] & 0x000000ff);
                        } else {
                            Ba = 0;
                        }

                        if (RGBTB.getBOn()) {
                            Bb = (RGBIndexBufferB[(int) imageBufferB[index + 3]] & 0x000000ff);
                        } else {
                            Bb = 0;
                        }
                    } else {
                        Ra = imageBufferA[index + 1];
                        Rb = imageBufferB[index + 1];
                        Ga = imageBufferA[index + 2];
                        Gb = imageBufferB[index + 2];
                        Ba = imageBufferA[index + 3];
                        Bb = imageBufferB[index + 3];
                    }

                    Ra = (int) ((Ra * alphaBlend) + (Rb * alphaPrime));
                    Ga = (int) ((Ga * alphaBlend) + (Gb * alphaPrime));
                    Ba = (int) ((Ba * alphaBlend) + (Bb * alphaPrime));

                    if (paintBitmap.get(offset + j) == true) {
                        bufferWrite[bufferAdr] = (byte) ((Ba * opacityPrime) + blue);
                        bufferWrite[bufferAdr + 1] = (byte) ((Ga * opacityPrime) + green);
                        bufferWrite[bufferAdr + 2] = (byte) ((Ra * opacityPrime) + red);
                        bufferAdr = bufferAdr + 3;
                    } else {
                        bufferWrite[bufferAdr] = (byte) Ba;
                        bufferWrite[bufferAdr + 1] = (byte) Ga;
                        bufferWrite[bufferAdr + 2] = (byte) Ra;
                        bufferAdr = bufferAdr + 3;
                    }
                } // for (x = 0; x < xDim - xPad; x++)

                for (x = 0; x < xPad; x++) {
                    bufferWrite[bufferAdr] = (byte) 0;
                    bufferWrite[bufferAdr + 1] = (byte) 0;
                    bufferWrite[bufferAdr + 2] = (byte) 0;
                    bufferAdr = bufferAdr + 3;
                } // for (x = 0; x < xPad; x++)
            } // for (y = yDim - 1; y >= 0; y--)
        }

        raFile.write(bufferWrite);
    }

    /**
     * Write BGR AVI triplet for color image. Since this is a frame captured on screen, this method is much simpler.
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void writeAVITripletCFrames() throws IOException {

        int j;
        int bufferSize;
        int index;
        float redMapped, greenMapped, blueMapped;
        int x, y;
        int bufferAdr;

        bufferSize = (xDim - xPad) * yDim * 4;
        bufferAdr = 0;

        try {
            imageA.exportData(0, bufferSize, imageBufferA);
        } catch (IOException error) {
            MipavUtil.displayError("" + error);

            return;
        }

        for (y = yDim - 1; y >= 0; y--) {

            for (x = 0; x < (xDim - xPad); x++) {
                j = x + (y * (xDim - xPad));
                index = 4 * j;
                redMapped = imageBufferA[index + 1];
                greenMapped = imageBufferA[index + 2];
                blueMapped = imageBufferA[index + 3];
                bufferWrite[bufferAdr] = (byte) blueMapped;
                bufferWrite[bufferAdr + 1] = (byte) greenMapped;
                bufferWrite[bufferAdr + 2] = (byte) redMapped;
                bufferAdr = bufferAdr + 3;
            } // for (x = 0; x < xDim-xPad; x++)

            for (x = 0; x < xPad; x++) {
                bufferWrite[bufferAdr] = (byte) 0;
                bufferWrite[bufferAdr + 1] = (byte) 0;
                bufferWrite[bufferAdr + 2] = (byte) 0;
                bufferAdr = bufferAdr + 3;
            } // for (x = 0; x < xPad; x++)
        } // for (y = yDim - 1; y >= 0; y--)

        raFile.write(bufferWrite);
    }

    /**
     * DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void writeidx1CHUNK() throws IOException {

        // Write the idx1 CHUNK
        // Write the 'idx1' signature
        int i;
        long idx1Pos;
        long saveidx1Length;
        long endPos;
        long idx1Place;
        int frameSize;

        idx1Pos = raFile.getFilePointer();
        raFile.seek(saveLIST2Size);
        writeInt((int) (idx1Pos - (saveLIST2Size + 4)), endianess);
        raFile.seek(idx1Pos);

        byte[] idx1Signature = new byte[4];
        idx1Signature[0] = 105; // i
        idx1Signature[1] = 100; // d
        idx1Signature[2] = 120; // x
        idx1Signature[3] = 49; // 1
        raFile.write(idx1Signature);

        // Write the length of the idx1 CHUNK not including the idx1 signature and the 4 length
        // bytes. Write 0 for now.
        saveidx1Length = raFile.getFilePointer();
        writeInt(0, endianess);

        for (i = 0; i < (totalBlankFrames + totalDataFrames); i++) {

            // In the ckid field write the 4 character code to identify the chunk 00db or 00dc
            idx1Place = raFile.getFilePointer();
            raFile.seek(savedbLength[i] - 1);
            dataSignature[3] = raFile.readByte(); // b for data frames and c for blank frames
            frameSize = getInt(endianess);
            raFile.seek(idx1Place);
            raFile.write(dataSignature);

            if (dataSignature[3] == 0x62) { // 00db
                writeInt(0x10, endianess); // Write the flags - select AVIIF_KEYFRAME
            } else {
                writeInt(0x00, endianess);
            }

            // AVIIF_KEYFRAME 0x00000010L
            // The flag indicates key frames in the video sequence.
            // Key frames do not need previous video information to be decompressed.
            // AVIIF_NOTIME 0x00000100L The CHUNK does not influence video timing(for
            // example a palette change CHUNK).
            // AVIIF_LIST 0x00000001L Marks a LIST CHUNK.
            // AVIIF_TWOCC 2L
            // AVIIF_COMPUSE 0x0FFF0000L These bits are for compressor use.
            writeInt((int) (savedbLength[i] - 4 - savemovi), endianess);

            // Write the offset (relative to the 'movi' field) to the relevant CHUNK
            writeInt(frameSize, endianess); // Write the length of the relevant

            // CHUNK.  Note that this length is
            // also written at savedbLength

        } // for (i = 0; i < (totalBlankFrames+totalDataFrames); i++)

        endPos = raFile.getFilePointer();
        raFile.seek(saveFileSize);
        writeInt((int) (endPos - (saveFileSize + 4)), endianess);
        raFile.seek(saveidx1Length);
        writeInt((int) (endPos - (saveidx1Length + 4)), endianess);
        raFile.close();
    }

    /**
     * Writes the RLE8 encoded LUT index.
     *
     * @param   timeSlice    t (time) slice to show
     * @param   slice        z slice to show
     * @param   pixStore     DOCUMENT ME!
     * @param   lastStore    DOCUMENT ME!
     * @param   encodeStore  DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void writeRLE8(int timeSlice, int slice, byte[] pixStore, byte[] lastStore, byte[] encodeStore)
            throws IOException {
        int pixIndex;
        int count;
        int encodeIndex;
        int i, index;
        int zDimSlices = 0;
        boolean sameA = false;
        int deltaX, deltaY;
        int sameX = 0;
        int sameY = 0;

        if (imageA.getNDims() >= 3) {
            zDimSlices = zDim;
        }

        try {

            // if (imageA.getType() == ModelStorageBase.DCOMPLEX)
            // imageA.ExportDComplexSliceXY(timeSlice*zDimSlices + slice,imageBufferA, logMagDisplay);
            if (imageA.getType() == ModelStorageBase.COMPLEX) {
                imageA.exportComplexSliceXY((timeSlice * zDimSlices) + slice, imageBufferA, false);
            } else {
                imageA.exportSliceXY((timeSlice * zDimSlices) + slice, imageBufferA);
            }

        } catch (IOException error) {
            MipavUtil.displayError("" + error); // Need to fix this

            return;
        }

        pixIndex = 0;

        for (int y = yDim - 1; y >= 0; y--) {

            for (int x = 0; x < (xDim - xPad); x++) {
                index = x + (y * (xDim - xPad));
                pixStore[pixIndex++] = (byte) (((imageBufferA[index] - imageMinA) * remapConstA) + 0.5);
            } // for (x = 0; x < xDim - xPad; x++)

            for (int x = xDim - xPad; x < xDim; x++) {
                pixStore[pixIndex++] = (byte) 0;
            } // for (x = xDim - xPad; x < xDim; x++)
        } // for (y = yDim - 1; y >= 0; y--)

        encodeIndex = 0;
        count = 0;

        for (int y = 0; y < yDim; y++) {

            for (int x = 0; x < xDim; x++) {
                index = x + (y * xDim);

                if ((pixStore[index] == lastStore[index]) && (((timeSlice * zDimSlices) + slice) > 0)) {

                    if (count > 0) {
                        encodeStore[encodeIndex++] = (byte) count;
                        encodeStore[encodeIndex++] = pixStore[index - 1];
                        count = 0;
                    }

                    if ((x == (xDim - 1)) && (y == (yDim - 1))) {

                        // end of bitmap
                        encodeStore[encodeIndex++] = (byte) 0;
                        encodeStore[encodeIndex++] = (byte) 1;
                    } else if (!sameA) {
                        sameA = true;
                        sameX = x;
                        sameY = y;
                    }
                } else if ((sameA) && (pixStore[index] != lastStore[index]) &&
                               (((timeSlice * zDimSlices) + slice) > 0)) {
                    sameA = false;

                    if (x < sameX) {
                        count = 1;

                        // position to end of line
                        encodeStore[encodeIndex++] = (byte) 0;
                        encodeStore[encodeIndex++] = (byte) 0;

                        // set position
                        deltaX = x;
                        deltaY = y - (sameY + 1);

                        while ((deltaX > 0) || (deltaY > 0)) {
                            encodeStore[encodeIndex++] = (byte) 0;
                            encodeStore[encodeIndex++] = (byte) 2;
                            encodeStore[encodeIndex++] = (byte) Math.min(deltaX, 255);
                            encodeStore[encodeIndex++] = (byte) Math.min(deltaY, 255);
                            deltaX -= 255;

                            if (deltaX < 0) {
                                deltaX = 0;
                            }

                            deltaY -= 255;

                            if (deltaY < 0) {
                                deltaY = 0;
                            }
                        } // while ((deltaX > 0) || (deltaY > 0))
                    } // if (x < sameX)
                    else if ((x == (xDim - 1)) && (y == (yDim - 1))) {
                        count = 0;

                        // set position
                        deltaX = x - sameX;
                        deltaY = y - sameY;

                        while ((deltaX > 0) || (deltaY > 0)) {
                            encodeStore[encodeIndex++] = (byte) 0;
                            encodeStore[encodeIndex++] = (byte) 2;
                            encodeStore[encodeIndex++] = (byte) Math.min(deltaX, 255);
                            encodeStore[encodeIndex++] = (byte) Math.min(deltaY, 255);
                            deltaX -= 255;

                            if (deltaX < 0) {
                                deltaX = 0;
                            }

                            deltaY -= 255;

                            if (deltaY < 0) {
                                deltaY = 0;
                            }
                        } // while ((deltaX > 0) || (deltaY > 0))

                        // write the byte at (xDim-1,yDim-1)
                        encodeStore[encodeIndex++] = (byte) 1;
                        encodeStore[encodeIndex++] = pixStore[index];

                        // end of bitmap for this dc
                        encodeStore[encodeIndex++] = (byte) 0;
                        encodeStore[encodeIndex++] = (byte) 1;
                    } // else if ((x == xDim-1) && (y == yDim-1))
                    else if (x == (xDim - 1)) {
                        count = 0;

                        // set position
                        deltaX = x - sameX;
                        deltaY = y - sameY;

                        while ((deltaX > 0) || (deltaY > 0)) {
                            encodeStore[encodeIndex++] = (byte) 0;
                            encodeStore[encodeIndex++] = (byte) 2;
                            encodeStore[encodeIndex++] = (byte) Math.min(deltaX, 255);
                            encodeStore[encodeIndex++] = (byte) Math.min(deltaY, 255);
                            deltaX -= 255;

                            if (deltaX < 0) {
                                deltaX = 0;
                            }

                            deltaY -= 255;

                            if (deltaY < 0) {
                                deltaY = 0;
                            }
                        } // while ((deltaX > 0) || (deltaY > 0))

                        // write the byte
                        encodeStore[encodeIndex++] = (byte) 1;
                        encodeStore[encodeIndex++] = pixStore[index];

                        // end of line
                        encodeStore[encodeIndex++] = (byte) 0;
                        encodeStore[encodeIndex++] = (byte) 0;
                    } // else if (x == (xDim-1))
                    else {
                        count = 1;

                        // set position
                        deltaX = x - sameX;
                        deltaY = y - sameY;

                        while ((deltaX > 0) || (deltaY > 0)) {
                            encodeStore[encodeIndex++] = (byte) 0;
                            encodeStore[encodeIndex++] = (byte) 2;
                            encodeStore[encodeIndex++] = (byte) Math.min(deltaX, 255);
                            encodeStore[encodeIndex++] = (byte) Math.min(deltaY, 255);
                            deltaX -= 255;

                            if (deltaX < 0) {
                                deltaX = 0;
                            }

                            deltaY -= 255;

                            if (deltaY < 0) {
                                deltaY = 0;
                            }
                        } // while ((deltaX > 0) || (deltaY > 0))
                    } // else
                } // else if ((sameA) && (imageBufferA[index] != lastBufferA[index]))
                else if (x == 0) {
                    count = 1;
                } else if ((count > 0) && (pixStore[index] == pixStore[index - 1])) {
                    count++;

                    if (x == (xDim - 1)) {
                        encodeStore[encodeIndex++] = (byte) count;
                        encodeStore[encodeIndex++] = pixStore[index];
                        count = 0;

                        if (y < (yDim - 1)) {

                            // end of line
                            encodeStore[encodeIndex++] = (byte) 0;
                            encodeStore[encodeIndex++] = (byte) 0;
                        } // if (y < (yDim - 1))
                        else { // y == (yDim - 1)

                            // end of bitmap for this dc
                            encodeStore[encodeIndex++] = (byte) 0;
                            encodeStore[encodeIndex++] = (byte) 1;
                        } // else for y == (yDim - 1)
                    } // if (x == (xDim - 1))
                    else if (count == 255) {
                        encodeStore[encodeIndex++] = (byte) 255;
                        encodeStore[encodeIndex++] = pixStore[index];
                        count = 0;
                    } // else if (count == 255)
                } // else if (((count > 0) && (pixStore[index] == pixStore[index-1]))
                else if ((count > 0) && (pixStore[index] != pixStore[index - 1])) {
                    encodeStore[encodeIndex++] = (byte) count;
                    encodeStore[encodeIndex++] = pixStore[index - 1];
                    count = 1;

                    if (x == (xDim - 1)) {
                        encodeStore[encodeIndex++] = (byte) 1;
                        encodeStore[encodeIndex++] = pixStore[index];
                        count = 0;

                        if (y < (yDim - 1)) {

                            // end of line
                            encodeStore[encodeIndex++] = (byte) 0;
                            encodeStore[encodeIndex++] = (byte) 0;
                        } // if (y < (yDim - 1))
                        else { // y == (yDim - 1)

                            // end of bitmap for this dc
                            encodeStore[encodeIndex++] = (byte) 0;
                            encodeStore[encodeIndex++] = (byte) 1;
                        } // else for y == (yDim - 1)
                    } // if (x == xDim - 1)
                } // else if ((count > 0) && (pixStore[index] != pixStore[index-1]))
                else if (count == 0) {
                    count = 1;

                    if (x == (xDim - 1)) {
                        encodeStore[encodeIndex++] = (byte) 1;
                        encodeStore[encodeIndex++] = pixStore[index];
                        count = 0;

                        if (y < (yDim - 1)) {

                            // end of line
                            encodeStore[encodeIndex++] = (byte) 0;
                            encodeStore[encodeIndex++] = (byte) 0;
                        } // if (y < (yDim - 1))
                        else { // y == (yDim - 1)

                            // end of bitmap for this dc
                            encodeStore[encodeIndex++] = (byte) 0;
                            encodeStore[encodeIndex++] = (byte) 1;
                        } // else for y == (yDim - 1)
                    } // if (x == (xDim - 1))
                } // else if (count == 0)
            } // for (x = 0; x < xDim; x++)
        } // for (y = 0; y < yDim; y++)

        // write the length of the encoded data
        writeInt(encodeIndex, endianess);
        bufferWrite = new byte[encodeIndex];
        dcLength[slice + (zDim * timeSlice)] = encodeIndex;

        for (i = 0; i < encodeIndex; i++) {
            bufferWrite[i] = encodeStore[i];
        }

        for (i = 0; i < pixStore.length; i++) {
            lastStore[i] = pixStore[i];
        }

        raFile.write(bufferWrite);
    }
    
    public boolean readWriteImage() throws IOException {
        long LIST1Marker, LISTsubchunkMarker, marker;
        int loop;
        long saveLIST1Size;
        int totalFramesW;
        long saveLIST1subSize = 0L;
        byte handlerW[];
        int lengthW;
        long savestrfSize = 0L;
        float secPerFrame;
        long savestrnPos;
        long saveJUNKsignature;
        int[] imgExtents;
        byte[] fileBuffer;
        int z;
        int totalDataArea;
        int remainingFileLength;
        int totalBytesRead;
        int dataLength;
        boolean dataFound;
        int moviOffset;
        int signature;
        int CHUNKtype;
        boolean haveMoviSubchunk = false;
        int subchunkBytesRead = 0;
        int subchunkBlocksRead = 0;
        boolean chunkRead;
        long startPosition; // position to start reading data
        int actualFrames = 0; // number of frames with data found on first read thru.
        int indexBytesRead = 0;
        int actualFramesW = 0;
        long savedibPos[];
        boolean doWrite[];
        byte dataSignatureW[]; 
        long idx1Pos;
        int zw;
        long saveidx1Length;
        long endPos;
        long saveTotalFramesW;
        long saveLengthW = 0L;
        long saveHere;
        long streamPositionW;
        boolean vidsRead = false;
        long firstDataSignature;
        boolean firstRun;

        try {
            System.out.println("Started " + outputFileName + " creation");
            file = new File(fileDir + fileName);
            raFile = new RandomAccessFile(file, "r");
            
            fileW = new File(fileDir + outputFileName);
            raFileW = new RandomAccessFile(fileW, "rw");
            // Necessary so that if this is an overwritten file there isn't any
            // junk at the end
            raFileW.setLength(0);

            endianess = FileBase.LITTLE_ENDIAN; // false

            signature = getInt(endianess);

            if (signature == 0x46464952) {
                // have read RIFF
            } else {
                raFile.close();
                throw new IOException("AVI read header error first 4 bytes = " + signature);
            }
            
            writeIntW(signature, endianess);

            getInt(endianess); // the file size excluding the first 8 bytes
            
            saveFileSize = raFileW.getFilePointer();
            // Bytes 4 thru 7 contain the length of the file.  The length does
            // not include bytes 0 to 7.
            writeIntW(0, endianess); // For now write 0 in the file size location

            int RIFFtype = getInt(endianess);

            if (RIFFtype == 0x20495641) {
                // have read AVI<sp>
            } else {
                raFile.close();
                throw new IOException("AVI read header error bytes 8-11 = " + RIFFtype);
            }
            
            writeIntW(RIFFtype, endianess);

            int CHUNKsignature = getInt(endianess);

            if (CHUNKsignature == 0x5453494C) {
                // have read LIST for first LIST CHUNK with information on data decoding
            } else {
                raFile.close();
                throw new IOException("AVI read header error bytes 12-15 = " + CHUNKsignature);
            }
            
            // Write the first LIST chunk, which contains information on data decoding
            writeIntW(CHUNKsignature, endianess);

            int LIST1Size = getInt(endianess); // size of first LIST CHUNK excluding first 8 bytes
            
            // Write the length of the LIST CHUNK not including the first 8 bytes with LIST and 
            // size.  Note that the end of the LIST CHUNK is followed by JUNK.
            saveLIST1Size = raFileW.getFilePointer();
            writeIntW(0, endianess); // For now write 0 in the avih sub-chunk size location

            // with CHUNKsignature and LIST1Size
            LIST1Marker = raFile.getFilePointer();

            CHUNKtype = getInt(endianess);

            if (CHUNKtype == 0x6C726468) {
                // have read hdrl
            } else {
                raFile.close();
                throw new IOException("AVI read header error bytes 16-19 = " + CHUNKtype);
            }
            
            // Write the chunk type
            writeIntW(CHUNKtype, endianess);

            int avihSignature = getInt(endianess); // signature of avih sub-CHUNK

            if (avihSignature == 0x68697661) {
                // have read avih
            } else {
                raFile.close();
                throw new IOException("AVI read header error bytes 2-23 = " + avihSignature);
            }
            
            // Write the avih sub-CHUNK
            writeIntW(avihSignature, endianess);

            int avihLength = getInt(endianess); // read the size of the avih sub-CHUNK not

            // including the first 8 bytes for the signature and length
            if (avihLength == 56) {
                // avih sub-CHUNK has expected length
            } else {
                raFile.close();
                throw new IOException("AVI read header error avih sub-CHUNK length = " + avihLength);
            }
            
            // Write the length of the avih sub-CHUNK (56) not including
            // the first 8 bytes for the avihSignature and length
            writeIntW(56, endianess);

            microSecPerFrame = getInt(endianess);

            // System.err.println("Microsec per frame: " + microSecPerFrame);
            Preferences.debug("microSecPerFrame = " + microSecPerFrame + "\n", Preferences.DEBUG_FILEIO);
            writeIntW(microSecPerFrame, endianess);
            
            secPerFrame = 1.0E-6F * microSecPerFrame;
            framesToCapture = Math.max(1, Math.round(captureTime/secPerFrame));
            Preferences.debug("Frames to capture = " + framesToCapture + "\n", Preferences.DEBUG_FILEIO);
            framesToSkip = Math.round(skipTime/secPerFrame);
            Preferences.debug("Frames to skip = " + framesToSkip + "\n", Preferences.DEBUG_FILEIO);

            int maxBytesPerSecond = getInt(endianess);
            Preferences.debug("maxBytesPerSecond = " + maxBytesPerSecond + "\n", Preferences.DEBUG_FILEIO);
            writeIntW(maxBytesPerSecond, endianess);

            // System.err.println("Unknown int: " + getInt(endianess));
            getInt(endianess);
            writeIntW(0, endianess); // dwReserved1 - Reserved 1 field set to zero

            int flags = getInt(endianess);

            if ((flags & 0x10) != 0) {
                AVIF_HASINDEX = true;
            } else {
                AVIF_HASINDEX = false;
            }

            if ((flags & 0x20) != 0) {
                AVIF_MUSTUSEINDEX = true;
            } else {
                AVIF_MUSTUSEINDEX = false;
            }

            if ((flags & 0x100) != 0) {
                AVIF_ISINTERLEAVED = true;
                Preferences.debug("AVIF_ISINTERLEAVED = true\n", Preferences.DEBUG_FILEIO);
            } else {
                AVIF_ISINTERLEAVED = false;
                Preferences.debug("AVIF_ISINTERLEAVED = false\n", Preferences.DEBUG_FILEIO);
            }

            if (AVIF_HASINDEX) {
                Preferences.debug("AVIF_HASINDEX = true\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("AVIF_HASINDEX = false\n", Preferences.DEBUG_FILEIO);
            }

            if (AVIF_MUSTUSEINDEX) {
                Preferences.debug("AVIF_MUSTUSEINDEX = true\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("AVIF_MUSTUSEINDEX = false\n", Preferences.DEBUG_FILEIO);
            }
            
            if (AVIF_HASINDEX && (!AVIF_MUSTUSEINDEX)) {
                AVIF_MUSTUSEINDEX = true;
                Preferences.debug("Changing AVIF_MUSTUSEINDEX from false to true for fast skipping\n", Preferences.DEBUG_FILEIO);
            }

            if ((flags & 0x800) != 0) {
                Preferences.debug("AVIF_TRUSTCKTYPE = true\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("AVIF_TRUSTCKTYPE = false\n", Preferences.DEBUG_FILEIO);
            }

            if ((flags & 0x10000) != 0) {
                Preferences.debug("AVIF_WASCAPTUREFILE = true\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("AVIF_WASCAPTUREFILE = false\n", Preferences.DEBUG_FILEIO);
            }

            if ((flags & 0x20000) != 0) {
                Preferences.debug("AVIF_COPYRIGHTED = true\n", Preferences.DEBUG_FILEIO);
            } else {
                Preferences.debug("AVIF_COPYRIGHTED = false\n", Preferences.DEBUG_FILEIO);
            }
            
            writeIntW(0x10, endianess); // dwFlags - just set the bit for AVIF_HASINDEX

            int totalFrames = getInt(endianess);

            //System.err.println("total frames: " + totalFrames);
            Preferences.debug("totalFrames = " + totalFrames + "\n", Preferences.DEBUG_FILEIO);
            totalFramesW = (totalFrames * framesToCapture)/(framesToCapture + framesToSkip);
            int remainderFrames = totalFrames % (framesToCapture + framesToSkip);
            if (remainderFrames > framesToCapture) {
                remainderFrames = framesToCapture;
            }
            totalFramesW += remainderFrames;
            saveTotalFramesW = raFileW.getFilePointer();
            writeIntW(totalFramesW, endianess);

            // However, many AVI frames will have no data and will just be used to repeat the
            // previous frames.  So we will need to read thru the data to get the actual number
            // of frames used in MIPAV before the image is created.  Then a second read thru will
            // take place to import the data into the image.
            int initialFrames = getInt(endianess);
            Preferences.debug("initialFrames = " + initialFrames + "\n", Preferences.DEBUG_FILEIO);
            // dwInitinalFrames - Initial frame for interleaved files
            // Noninterleaved files should specify 0.
            writeIntW(0, endianess);
            
            streams = getInt(endianess);
            Preferences.debug("Number of streams: " + streams + "\n", Preferences.DEBUG_FILEIO);
            // dwStreams - number of streams in the file - here 1 video and zero audio.
            writeIntW(1, endianess);

            int suggestedBufferSize = getInt(endianess);
            Preferences.debug("suggestedBufferSize = " + suggestedBufferSize + "\n", Preferences.DEBUG_FILEIO);
            writeIntW(suggestedBufferSize, endianess);
            width = getInt(endianess); // xDim
            Preferences.debug("width = " + width + "\n", Preferences.DEBUG_FILEIO);
            xDim = width;
            writeIntW(width, endianess);
            height = getInt(endianess); // yDim
            Preferences.debug("height = " + height + "\n", Preferences.DEBUG_FILEIO);
            yDim = height;
            writeIntW(height, endianess);

            // read 4 reserved integers
            for (int i = 0; i < 4; i++) {
                getInt(endianess);
            }
            // dwreserved[4] - Microsoft says to set the following 4 values to 0.
            for (int i = 0; i < 4; i++) {
                writeIntW(0, endianess);
            }
            
            streamPositionW = raFileW.getFilePointer();

            for (loop = 0; (loop < streams); loop++) {

                // read the LIST subCHUNK
                CHUNKsignature = getInt(endianess);
                if (!vidsRead) {
                    writeIntW(CHUNKsignature, endianess);
                }
                
                if (CHUNKsignature == 0x6E727473) {
                    // read strn instead of CHUNK
                    int strnLength = getInt(endianess);
                    if (!vidsRead) {
                        writeIntW(strnLength, endianess);
                    }
                    if ((strnLength % 2) == 1) {
                        strnLength++;
                    }
                    byte[] text = new byte[strnLength];
                    raFile.read(text);
                    if (!vidsRead) {
                        raFileW.write(text);
                    }

                    if (text[strnLength - 1] != 0) {
                        raFile.close();
                        throw new IOException("strn string ends with illegal temination at loop start = " + text[strnLength - 1]);
                    }
                    CHUNKsignature = getInt(endianess);
                    if (!vidsRead) {
                        writeIntW(CHUNKsignature, endianess);
                    }
                } // if (CHUNKSignature == 0x6E727473)

                if (CHUNKsignature == 0x5453494C) {
                    // have read LIST for LIST subCHUNK with information on data decoding
                } else {
                    raFile.close();
                    throw new IOException("AVI read header error signature first LIST subCHUNK = " + CHUNKsignature);
                }

                int LISTsubchunkSize = getInt(endianess); // size of the first LIST subCHUNK not including
                LISTsubchunkMarker = raFile.getFilePointer();
                
                // Write the size of the first LIST subCHUNK not including the first 8 bytes with
                // LIST and size.  Note that saveLIST1Size = saveLIST1subSize + 76, and that
                // the length written to saveLIST1subSize is 76 less than the length written to saveLIST1Size.
                // The end of the first LIST subCHUNK is followed by JUNK.
                if (!vidsRead) {
                    saveLIST1subSize = raFileW.getFilePointer();
                }
                // For now write 0 in CHUNK size location
                if (!vidsRead) {
                    writeIntW(0, endianess);
                }

                // the first 8 signature and length bytes
                CHUNKtype = getInt(endianess);

                if (CHUNKtype == 0x6C727473) {
                    // have read strl
                } else {
                    raFile.close();
                    throw new IOException("AVI read header error no strl in first LIST subCHUNK but = " + CHUNKtype);
                }
                
                // Write the chunk type
                if (!vidsRead) {
                    writeIntW(CHUNKtype, endianess);
                }

                // read the strh subCHUNK
                int strhSignature = getInt(endianess);

                if (strhSignature == 0x68727473) {
                    // have read strh
                } else {
                    raFile.close();
                    throw new IOException("AVI read header error no strhSignature found but = " + strhSignature);
                }
                if (!vidsRead) {
                    writeIntW(strhSignature, endianess);
                }

                int strhLength = getInt(endianess); // length of strh subCHUNK not including first 8

                // signature and length bytes
                // AVI standard documentation mentioned a minimum length of 56,
                // but the Windows Media player was observed to play 5 mjpeg
                // files with strhLength = 48.
                if (strhLength < 48) {
                    raFile.close();
                    throw new IOException("AVI read header error with strhLength = " + strhLength);
                }
                
                // Write the length of the strh sub-CHUNK
                if (!vidsRead) {
                    writeIntW(strhLength, endianess);
                }

                int fccType = getInt(endianess);

                if (fccType == 0x73646976) {
                    // vids read for video stream
                    vidsRead = true;
                } else if (streams > 1) {
                    raFile.seek(LISTsubchunkSize + LISTsubchunkMarker);
                    if (!vidsRead) {
                        raFileW.seek(streamPositionW);
                    }

                    continue;
                } else {
                    raFile.close();
                    throw new IOException("AVI read header error with fccType = " + fccType);
                }
                
                // fccType - Write the type of data stream - here vids for video stream
                writeIntW(fccType, endianess);

                int handler = getInt(endianess);

                byte[] handlerByte = new byte[5];
                handlerByte[0] = (byte) (handler & 0xFF);
                handlerByte[1] = (byte) ((handler >> 8) & 0xFF);
                handlerByte[2] = (byte) ((handler >> 16) & 0xFF);
                handlerByte[3] = (byte) ((handler >> 24) & 0xFF);
                handlerByte[4] = (byte) 0;

                String handlerString = new String(handlerByte);

                System.err.println("Handler String is: " + handlerString);
                
                handlerW = new byte[4];

                if ((handler == 0x20424944 /* DIB<sp> */) || (handler == 0x20424752 /* RGB<sp> */) ||
                        (handler == 0x20574152 /* RAW<sp> */) || (handler == 0x00000000) ||
                        (handlerString.startsWith("00dc"))) {
                    // uncompressed data
                    handlerW[0] = 68; // D
                    handlerW[1] = 73; // I
                    handlerW[2] = 66; // B
                    handlerW[3] = 32; // space
                    compression = 0;
                    Preferences.debug("Uncompressed data\n", Preferences.DEBUG_FILEIO);
                } else if (handlerString.toUpperCase().startsWith("MRLE") ||
                               handlerString.toUpperCase().startsWith("RLE")) {
                    Preferences.debug("Microsoft run length encoding\n", Preferences.DEBUG_FILEIO);
                    /* mrle microsoft run length encoding */
                    handlerW[0] = 0x6D; // m
                    handlerW[1] = 0x72; // r
                    handlerW[2] = 0x6c; // l
                    handlerW[3] = 0x65; // e
                    compression = 1;
                } else if (handlerString.toUpperCase().startsWith("MSVC")) {
                    Preferences.debug("Microsoft video 1 compression\n", Preferences.DEBUG_FILEIO);
                    // Microsoft video 1 compression
                    doMSVC = true;
                    handlerW[0] = 109; // m
                    handlerW[1] = 115; // s
                    handlerW[2] = 118; // v
                    handlerW[3] =  99; // c
                    compression = 1296126531;
                } else {
                    raFile.close();
                    throw new IOException("Unrecognized compression handler is " + handlerString);
                    // tscc is the TechSmith Screen Capture Codec put out by the Techsmith Corporation for use with
                    // their Camtasia Screen "Camcorder" application.  Camtasia is a Microsoft windows application only.
                    //  The company has no plans to develop a version for the apple. Could the program be designed to
                    // use the tscc.exe codec provided by Techsmith?
                }
                raFileW.write(handlerW);

                flags = getInt(endianess);

                if ((flags & 0x00000001) != 0) {
                    raFile.close();
                    throw new IOException("Cannot presently handle AVISF_DISABLED");
                }

                if ((flags & 0x00010000) != 0) {
                    raFile.close();
                    throw new IOException("Cannot presently handle AVISF_VIDEO_PALCHANGES");
                }
                
                // 0x00000001 AVISF_DISABLED The stream data sould be rendered only when
                // explicitly enabled.
                // 0x00010000 AVISF_VIDEO_PALCHANGES Indicates that a palette change is included
                // in the AVI file.  The flag warns the playback software that it
                // will need to animate the palette.
                writeIntW(0, endianess); // dwFlags

                int priority = getInt(endianess);
                Preferences.debug("priority = " + priority + "\n", Preferences.DEBUG_FILEIO);
                // dwPriority - priority of a stream type.  For example, in a file with
                // multiple audio streams, the one with the highest priority might be the
                // default one.
                writeIntW(0, endianess);
                
                initialFrames = getInt(endianess);

                if (initialFrames != 0) {
                    raFile.close();
                    throw new IOException("initialFrames should be 0 for noninterleaved files");
                }
                
                // dwInitialFrames - Specifies how far audio data is skewed ahead of video
                // frames in interleaved files.  Typically, this is about 0.75 seconds.  In
                // interleaved files specify the number of frames in the file prior
                // to the initial frame of the AVI sequence.
                // Noninterleaved files should used zero.
                writeIntW(0, endianess);

                // rate/scale = samples/second
                scale = getInt(endianess);
                Preferences.debug("scale = " + scale + "\n", Preferences.DEBUG_FILEIO);

                // System.err.println("Scale is: " + scale);
                writeIntW(scale, endianess);
                rate = getInt(endianess);

                // System.err.println("Rate is: " + rate);
                Preferences.debug("rate = " + rate + "\n", Preferences.DEBUG_FILEIO);
                writeIntW(rate, endianess);
                
                float samplesPerSecond = (float)rate/(float)scale;
                Preferences.debug("Samples per second = " + samplesPerSecond + "\n", Preferences.DEBUG_FILEIO);
                if (Math.abs(((1.0/samplesPerSecond) - secPerFrame)/secPerFrame) < 0.01) {
                    Preferences.debug("Frame times from 1.0E-6*microSecPerFrame and scale/rate match\n", Preferences.DEBUG_FILEIO);
                }
                else {
                    Preferences.debug("Frame times from 1.0E-6*microSecPerFrame and scale/rate don't match", Preferences.DEBUG_FILEIO);
                }

                int start = getInt(endianess);
                Preferences.debug("start = " + start + "\n", Preferences.DEBUG_FILEIO);
                // dwStart - this field is usually set to zero
                writeIntW(0, endianess);

                int length = getInt(endianess);
                Preferences.debug("length = " + length + "\n", Preferences.DEBUG_FILEIO);
                lengthW = (length * framesToCapture)/(framesToCapture + framesToSkip);
                remainderFrames = length % (framesToCapture + framesToSkip);
                if (remainderFrames > framesToCapture) {
                    remainderFrames = framesToCapture;
                }
                lengthW += remainderFrames;
                saveLengthW = raFileW.getFilePointer();
                // dwLength - set equal to the number of frames
                writeIntW(lengthW, endianess);

                // System.err.println("DWLength: " + length);
                suggestedBufferSize = getInt(endianess);
                Preferences.debug("suggestedBufferSize = " + suggestedBufferSize + "\n", Preferences.DEBUG_FILEIO);
                // dwSuggestedBufferSize - suggested buffer size for reading the stream
                writeIntW(suggestedBufferSize, endianess);

                int quality = getInt(endianess);

                if ((quality > 10000) || (quality < -1)) {
                    raFile.close();
                    throw new IOException("quality = " + quality);
                }

                Preferences.debug("quality = " + quality + "\n", Preferences.DEBUG_FILEIO);
                // dwQuality - encoding quality given by an integer between
                // 0 and 10,000.  If set to -1, drivers use the default
                // quality value.
                writeIntW(quality, endianess);

                int sampleSize = getInt(endianess);
                Preferences.debug("sampleSize = " + sampleSize + "\n", Preferences.DEBUG_FILEIO);
                if ((compression == 0) || (compression == 1296126531)) {
                    writeIntW(3 * width * height, endianess);
                }
                else if (compression == 1) {
                    writeIntW(width * height, endianess);
                }

                // read destination rectangle within movie rectangle
                short left = (short) getSignedShort(endianess);
                Preferences.debug("left = " + left + "\n", Preferences.DEBUG_FILEIO);

                short top = (short) getSignedShort(endianess);
                Preferences.debug("top = " + top + "\n");

                short right = (short) getSignedShort(endianess);
                Preferences.debug("right = " + right + "\n", Preferences.DEBUG_FILEIO);

                short bottom = (short) getSignedShort(endianess);
                Preferences.debug("bottom = " + bottom + "\n", Preferences.DEBUG_FILEIO);
                // rcFrame - Specifies the destination rectangle for a text or video stream within the movie
                // rectangle specified by the dwWidth and dwHeight members of the AVI main header structure.
                // The rcFrame member is typically used in support of multiple video streams.  Set this
                // rectangle to the coordinates corresponding to the movie rectangle to update the whole
                // movie rectangle.  Units for this member are pixels.  The upper-left corner of the destination
                // rectangle is relative to the upper-left corner of the movie rectangle.
                writeShortW(left, endianess);
                writeShortW(top, endianess);
                writeShortW(right, endianess);
                writeShortW(bottom, endianess);

                if (strhLength > 56) {
                    byte[] extra = new byte[strhLength - 56];
                    raFile.read(extra);
                    raFileW.write(extra);
                }

                // read the stream format CHUNK
                int strfSignature = getInt(endianess);

                if (strfSignature == 0x66727473) {
                    Preferences.debug("Read strf\n", Preferences.DEBUG_FILEIO);
                    // read strf
                } else {
                    raFile.close();
                    throw new IOException("strf signature incorrectly read as = " + strfSignature);
                }
                
                // Write the stream format chunk
                writeIntW(strfSignature, endianess);

                int strfSize = getInt(endianess);
                // Write the size of the stream format CHUNK not including the first 8 bytes for
                // strf and the size.  Note that the end of the stream format CHUNK is followed by
                // strn.
                savestrfSize = raFileW.getFilePointer();
                // For now write 0 in the strf CHUNK size location
                writeIntW(0, endianess);
                int BITMAPINFOsize = getInt(endianess);

                if (BITMAPINFOsize > strfSize) {
                    BITMAPINFOsize = strfSize;
                }

                if (BITMAPINFOsize < 40) {
                    raFile.close();
                    throw new IOException("Cannot handle BITMAPINFO size = " + BITMAPINFOsize);
                }
                
                // biSize - write header size of BITMAPINFO header structure
                writeIntW(BITMAPINFOsize, endianess);

                width = getInt(endianess);
                Preferences.debug("width = " + width + "\n", Preferences.DEBUG_FILEIO);
                // biWidth - image width in pixels
                writeIntW(width, endianess);
                height = getInt(endianess);
                Preferences.debug("height = " + height + "\n", Preferences.DEBUG_FILEIO);
                // biHeight - image height in pixels.  If height is positive,
                // the bitmap is a bottom up DIB and its origin is in the lower left corner.  If
                // height is negative, the bitmap is a top-down DIB and its origin is the upper
                // left corner.  This negative sign feature is supported by the 
                // Windows Media Player, but it is not
                // supported by PowerPoint
                writeIntW(height, endianess);

                short planes = (short) getSignedShort(endianess);

                if (planes != 1) {
                    raFile.close();
                    throw new IOException("planes has an incorrect value = " + planes);
                }
                // biPlanes - number of color planes in whcih the data is stored
                writeShortW((short)1, endianess);

                bitCount = (short) getSignedShort(endianess);
                Preferences.debug("bitCount = " + bitCount + "\n", Preferences.DEBUG_FILEIO);
                // biBitCount - number of bits per pixel
                writeShortW(bitCount, endianess);

                compression = getInt(endianess);

                if (compression == 0) {
                    Preferences.debug("Compression is BI_RGB\n", Preferences.DEBUG_FILEIO);
                    // BI_RGB uncompressed
                } else if (compression == 1) {
                    Preferences.debug("Compression is BI_RLE8\n", Preferences.DEBUG_FILEIO);
                    // BI_RLE8
                } else if (compression == 2) {

                    // BI_RLE4
                    raFile.close();
                    throw new IOException("Cannot currently handle 4 bit run length encoding");
                } else if (compression == 3) {
                    // BI_BITFIELDS
                    // To allow for arbitrarily packed RGB samples, BI_BITFIELDS specifies a
                    // mask field for each of the red, green, and blue pixel components.
                    // These masks indicate the bit positions occupied by each color
                    // component in a pixel.  In general, the masks are passed to a driver
                    // or video API using means other than a basic BITMAPINFOHEADER(such
                    // as using the appropriate fields in a DirectDraw DDPIXELFORMAT
                    // structure) but it might be valid to append the masks to the end of
                    // the BITMAPINFOHEADER in much the same way that a palette is appended
                    // for palettised formats.
                    //
                    // For example, 16 bit RGB 5:6:5 can be described using BI_BITFIELDS
                    // and the following bitmasks:

                    // Red  0xF800 (5 bits of red)
                    // Green 0x07E0 (6 bits of green)
                    // Blue  0x001F (5 bits of blue)

                    // In this case, if used with a BITMAPINFOHEADER, the bitmasks are
                    // u_int16s (16 bit) since the biBitFields field is set to 16.  For
                    // a 32bpp version, the bitmasks are each u_int32s.
                    raFile.close();
                    throw new IOException("Cannot currently handle BI_BITFIELDS compresion");
                } else if (compression == 1296126531) {
                    Preferences.debug("compression is Microsoft video 1\n", Preferences.DEBUG_FILEIO);
                    doMSVC = true;
                } else {
                    raFile.close();
                    throw new IOException("Unknown compression with value = " + compression);
                }

                Preferences.debug("compression = " + compression + "\n", Preferences.DEBUG_FILEIO);

                if (((compression == 0) &&
                         ((bitCount == 4) || (bitCount == 8) || (bitCount == 16) || (bitCount == 24) || (bitCount == 32))) ||
                        ((compression == 1) && (bitCount == 8)) || (doMSVC && (bitCount == 8)) ||
                        (doMSVC && (bitCount == 16))) {
                    // OK
                } else {
                    raFile.close();
                    throw new IOException("Cannot currently handle bit count = " + bitCount);
                }
                
                // biCompression - type of compression used
                // 0L for BI_RGB, uncompressed data as bitmap
                // 1L for BI_RLE8, a run-length encoded(RLE) format for bitmaps
                // with 8 bits per pixel.  The compression format is a 2-byte
                // format consisting of a byte count followed by a byte containing
                // a color index.  In addition, the first byte of the pair can be
                // set to zero to indicate an escape character that denotes the end
                // of a line, the end of a bitmap, a delta, or the number of bytes
                // which follow, each of which contains the color index of a single
                // pixel, depending on the
                // value of the second byte of the pair, which can be one of the
                // following values:
                // value             meaning
                // 0                 End of line.
                // 1                 End of bitmap.
                // 2                 Delta.  The two bytes following the
                // escape contain unsigned values indicating
                // the horizontal and vertical offsets
                // of the next pixel from the current
                // position.
                // 3-255             number of bytes that folow, each of which
                // contains the color index of a single pixel
                // Must be padded if an odd value so that it
                // ends on a word boundary.
                // 2L for BI_RLE4, a RLE format for bits with 4 bits per pixel.
                // The compression format is a 2-byte format consisting of a count
                // byte followed by two word-length color indexes.
                // 3L for BI_BITFIELDS, specifies that the bitmap is not compressed
                // and that the color table consists of three DWORD color masks
                // that specify the red, green, and blue components, respectively,
                // of each pixel.  This is valid when used with 16- and 32-bit-
                // per-pixel bitmaps.
                writeIntW(compression, endianess);

                int imageSize = getInt(endianess);
                Preferences.debug("imageSize = " + imageSize + "\n", Preferences.DEBUG_FILEIO);
                // biSizeImage - specifies the size in bytes of the image frame.  This can be
                // set to zero for uncompressed RGB bitmaps.
                if ((compression == 0) || (compression == 1296126531)) {
                    writeIntW(3 * width * height, endianess);
                }
                else if (compression == 1) {
                    writeIntW(width * height, endianess);
                }

                float[] imgResols = new float[5];
                imgResols[0] = imgResols[1] = imgResols[2] = imgResols[3] = imgResols[4] = 1.0f;

                int xPixelsPerMeter = getInt(endianess);
                Preferences.debug("xPixelsPerMeter = " + xPixelsPerMeter + "\n", Preferences.DEBUG_FILEIO);
                // biXPelsPerMeter - horizontal resolution in pixels
                writeIntW(xPixelsPerMeter, endianess);

                // System.err.println("xPixelsPerMeter = " + xPixelsPerMeter);
                if (xPixelsPerMeter > 0) {
                    imgResols[0] = (1.0f / xPixelsPerMeter) * 1000.0f;
                }

                int yPixelsPerMeter = getInt(endianess);
                Preferences.debug("yPixelsPerMeter = " + yPixelsPerMeter + "\n", Preferences.DEBUG_FILEIO);
                // biYPelsPerMeter - vertical resolution in pixels
                writeIntW(yPixelsPerMeter, endianess);

                // System.err.println("yPixelsPerMeter = " + yPixelsPerMeter);
                if (yPixelsPerMeter > 0) {
                    imgResols[1] = (1.0f / yPixelsPerMeter) * 1000.0f;
                }

                imgResols[2] = microSecPerFrame;

                // System.err.println("Microseconds per frame (on read): " + microSecPerFrame);

                int colorsUsed = getInt(endianess);
                Preferences.debug("colorsUsed = " + colorsUsed + "\n", Preferences.DEBUG_FILEIO);

                if ((compression == 0) && ((bitCount == 24) || (bitCount == 32)) && (colorsUsed != 0)) {
                    raFile.close();
                    throw new IOException("For 24 and 32 bit uncompressed data software does not currently support colorsUsed = " +
                                          colorsUsed);
                }

                if ((bitCount == 8) && (colorsUsed == 0)) {
                    colorsUsed = 8;
                }
                // biClrUsed - Provides a way for getting smaller color tables.  When this
                // field is set to 0, the number of colors in the color table is based on
                // the biBitCount field (1 indicates 2 colors, 4 indicates 16 colors,
                // 8 indicates 256, and 24 indicates no color table).  A nonzero value
                // specifies the exact number of colors in the table.  So, for example,
                // if an 8-bit DIB uses only 17 colors, then only those 17 colors need
                // to be defined in the table, and biClrUsed is set to 17.  If nonzero
                // for a 24-bit DIB, it indicates the existence of a color table that the
                // application can use for color reference.
                if ((compression == 0) && (bitCount > 8)) {
                    writeIntW(0, endianess);
                }
                else {
                    writeIntW(colorsUsed, endianess);
                }

                int colorsImportant = getInt(endianess);
                Preferences.debug("colorsImportant = " + colorsImportant + "\n", Preferences.DEBUG_FILEIO);
                // biClrImportant - specifies that the first x colors of the color table
                // are important to the DIB.  If the rest of the colors are not available,
                // the image still retains its meaning in an acceptable manner.  When this
                // field is set to zero, all the colors are important, or, rather, their
                // relative importance has not been computed.
                if (compression == 0) {
                    writeIntW(0, endianess);
                }
                else {
                    writeIntW(colorsImportant, endianess);
                }

                if (BITMAPINFOsize > 40) {
                    byte[] extra = new byte[BITMAPINFOsize - 40];
                    raFile.read(extra);
                    raFileW.write(extra);
                }

                if (bitCount == 4) {

                    // read the color table into a LUT
                    int[] dimExtentsLUT = new int[2];
                    dimExtentsLUT[0] = 4;
                    dimExtentsLUT[1] = 256;

                    // FileIO obtains via getModelLUT.
                    // Then, ViewOpenFileUI obtains from FileIO via getModelLUT.
                    LUTa = new ModelLUT(ModelLUT.GRAY, colorsUsed, dimExtentsLUT);
                    lutBuffer = new byte[4 * colorsUsed];
                    raFile.read(lutBuffer);
                    raFileW.write(lutBuffer);   

                    for (int i = 0; i < colorsUsed; i++) {
                        LUTa.set(0, i, 1.0f); // alpha
                        LUTa.set(1, i, (lutBuffer[(4 * i) + 2] & 0x000000ff)); // red
                        LUTa.set(2, i, (lutBuffer[(4 * i) + 1] & 0x000000ff)); // green
                        LUTa.set(3, i, (lutBuffer[4 * i] & 0x000000ff)); // blue
                    } // for (i = 0; i < colorsUsed; i++)

                    for (int i = colorsUsed; i < 256; i++) {
                        LUTa.set(0, i, 1.0f);
                        LUTa.set(1, i, 0);
                        LUTa.set(2, i, 0);
                        LUTa.set(3, i, 0);
                    } // for (i = colorsUsed; i < 256; i++)

                    LUTa.makeIndexedLUT(null);
                } // if (bitCount == 4)
                else if (bitCount == 8) {

                    // read the color table into a LUT
                    int[] dimExtentsLUT = new int[2];
                    dimExtentsLUT[0] = 4;
                    dimExtentsLUT[1] = 256;

                    // FileIO obtains via getModelLUT.
                    // Then, ViewOpenFileUI obtains from FileIO via getModelLUT.
                    LUTa = new ModelLUT(ModelLUT.GRAY, colorsUsed, dimExtentsLUT);
                    lutBuffer = new byte[4 * colorsUsed];
                    raFile.read(lutBuffer);
                    raFileW.write(lutBuffer);

                    for (int i = 0; i < colorsUsed; i++) {
                        LUTa.set(0, i, 1.0f); // alpha
                        LUTa.set(1, i, (lutBuffer[(4 * i) + 2] & 0x000000ff)); // red
                        LUTa.set(2, i, (lutBuffer[(4 * i) + 1] & 0x000000ff)); // green
                        LUTa.set(3, i, (lutBuffer[4 * i] & 0x000000ff)); // blue
                    } // for (i = 0; i < colorsUsed; i++)

                    for (int i = colorsUsed; i < 256; i++) {
                        LUTa.set(0, i, 1.0f);
                        LUTa.set(1, i, 0);
                        LUTa.set(2, i, 0);
                        LUTa.set(3, i, 0);
                    } // for (i = colorsUsed; i < 256; i++)

                    LUTa.makeIndexedLUT(null);
                } // else if (bitCount == 8)

                // Calculate the number of strf CHUNK bytes after the end of BITMAPINFO
                int strfEndBytes = strfSize - BITMAPINFOsize;

                if ((bitCount == 4) ||(bitCount == 8)) {
                    strfEndBytes = strfEndBytes - (4 * colorsUsed);
                }

                if (strfEndBytes > 0) {
                    byte[] byteb = new byte[strfEndBytes];
                    raFile.read(byteb);
                    raFileW.write(byteb);
                }
            } // for (loop = 0; loop < streams; loop++)

            marker = raFile.getFilePointer();
            
            savestrnPos = raFileW.getFilePointer();
            raFileW.seek(savestrfSize);
            writeIntW((int)(savestrnPos - (savestrfSize + 4)), endianess);
            raFileW.seek(savestrnPos);

            if (marker < (LIST1Marker + LIST1Size)) {
                // read strn subCHUNK
                
                int strnSignature = getInt(endianess);
                writeIntW(strnSignature, endianess);

                if (strnSignature == 0x6E727473) {

                    int strnLength = getInt(endianess);
                    writeIntW(strnLength, endianess);
                    if ((strnLength % 2) == 1) {
                        strnLength++;
                    }
                    byte[] text = new byte[strnLength];
                    raFile.read(text);
                    raFileW.write(text);

                    if (text[strnLength - 1] != 0) {
                        raFile.close();
                        throw new IOException("strn string ends with illegal temination = " + text[strnLength - 1]);
                    }
                } // if (strnSignature == 0x6E727473)
                else if (strnSignature == 0x4B4E554A) {

                    // have read JUNK for a JUNK padding CHUNK
                    int JUNKlength = getInt(endianess);
                    writeIntW(JUNKlength, endianess);
                    marker = raFile.getFilePointer();
                    raFile.seek(marker + JUNKlength);
                    byte byteb[] = new byte[JUNKlength];
                    raFileW.write(byteb);
                    CHUNKsignature = getInt(endianess);
                    writeIntW(CHUNKsignature, endianess);

                    if (CHUNKsignature != 0x5453494C) {
                        raFile.close();
                        throw new IOException("After JUNK CHUNK unexpected signature = " + CHUNKsignature);
                    }
                } // else if (strnSignature == 0x4B4E554A)
                else if (strnSignature == 0x54465349) {
 
                    // have read ISFT
                    int ISFTlength = getInt(endianess);
                    writeIntW(ISFTlength,endianess);
                    if ((ISFTlength % 2) == 1) {
                        ISFTlength++;
                    }
                    marker = raFile.getFilePointer();
                    raFile.seek(marker + ISFTlength);
                    byte byteb[] = new byte[ISFTlength];
                    raFileW.write(byteb);
                } // else if (strnSignature == 0x54465349)
                else if (strnSignature == 0x74646576) {

                    // have read vedt
                    int vedtLength = getInt(endianess);
                    writeIntW(vedtLength, endianess);
                    if ((vedtLength %2) == 1) {
                        vedtLength++;
                    }
                    byte[] vedt = new byte[vedtLength];
                    raFile.read(vedt);
                    raFileW.write(vedt);
                } else {
                    raFile.close();
                    throw new IOException("strn signature is an erroneous = " + strnSignature);
                }
            }

            raFile.seek(LIST1Marker + LIST1Size);
            signature = getInt(endianess);
            saveJUNKsignature = raFileW.getFilePointer();
            raFileW.seek(saveLIST1Size);
            writeIntW((int)(saveJUNKsignature - (saveLIST1Size + 4)), endianess);
            raFileW.seek(saveLIST1subSize);
            writeIntW((int)(saveJUNKsignature - (saveLIST1subSize + 4)), endianess);
            raFileW.seek(saveJUNKsignature);
            writeIntW(signature, endianess);

            if (signature == 0x4B4E554A) {

                // have read JUNK for a JUNK padding CHUNK
                int JUNKlength = getInt(endianess);
                writeIntW(JUNKlength, endianess);
                marker = raFile.getFilePointer();
                raFile.seek(marker + JUNKlength);
                byte byteb[] = new byte[JUNKlength];
                raFileW.write(byteb);
                CHUNKsignature = getInt(endianess);
                // Write the second LIST chunk, which contains the actual data
                writeIntW(CHUNKsignature, endianess);

                if (CHUNKsignature != 0x5453494C) {
                    raFile.close();
                    throw new IOException("After JUNK CHUNK unexpected signature = " + CHUNKsignature);
                }
            } else if (signature != 0x5453494C) {
                raFile.close();
                throw new IOException("After first LIST CHUNK unexpected signature = " + signature);
            }

            // At this point have read LIST for the second LIST CHUNK which contains the actual data.
            LIST2Size = getInt(endianess);
            // Write the lengthof the LIST CHUNK not including the first 8 bytes with LIST and
            // size.  The end of the second LIST CHUNK is followed by idx1.
            saveLIST2Size = raFileW.getFilePointer();
            // For now write 0
            writeIntW(0, endianess);
            moviPosition = raFile.getFilePointer();
            savemovi = raFileW.getFilePointer();
            idx1Position = moviPosition + LIST2Size;
            raFile.seek(idx1Position + 4);
            indexSize = getInt(endianess);
            raFile.seek(moviPosition);
            indexPointer = idx1Position + 8;
            CHUNKtype = getInt(endianess);
            // Write CHUNK type 'movi'
            writeIntW(CHUNKtype, endianess);

            if (CHUNKtype != 0x69766F6D) {

                // have not read movi
                raFile.close();
                throw new IOException("CHUNK type in second LIST CHUNK is an illegal = " + CHUNKtype);
            }
            
            startPosition = raFile.getFilePointer();
            doWrite = new boolean[totalFrames];
            z = 0;
            int skipCount = 0;
            int captureCount = 0;
            // Do first read thru the data to find the actual number of frames used by MIPAV. This must be done before
            // the MIPAV image can be created.

            dataSignature = new byte[4];

            totalDataArea = LIST2Size - 4; // Subtract out 4 'movi' bytes

            // Have encountered LiST2Size > raFile.length(), an impossibility
            remainingFileLength = (int) (raFile.length() - startPosition);

            if (totalDataArea > remainingFileLength) {
                MipavUtil.displayWarning("File appears to be truncated");
                totalDataArea = remainingFileLength;
            }

            totalBytesRead = 0;

            // Check for LIST rec<sp> subchunks
            if (!AVIF_MUSTUSEINDEX) {
                signature = getInt(endianess);

                if (signature == 0x5453494C) {

                    // have read LIST
                    LIST2subchunkSize = getInt(endianess);
                    moviSubchunkPosition = raFile.getFilePointer();
                    CHUNKtype = getInt(endianess);

                    if (CHUNKtype == 0x20636572) {

                        // have read rec<sp>
                        haveMoviSubchunk = true;
                        Preferences.debug("LIST rec found\n", Preferences.DEBUG_FILEIO);
                        subchunkBytesRead = 0;
                        subchunkBlocksRead = 0;
                    } else {
                        raFile.close();
                        throw new IOException("CHUNK type in second LIST sub CHUNK is an illegal = " + CHUNKtype);
                    }
                } else {
                    raFile.seek(startPosition);
                }
            } // if (!AVIF_MUSTUSEINDEX)

            chunkRead = true;

            firstDataSignature = raFile.getFilePointer();
            firstRun = true;
            loop1:
            while (((!AVIF_MUSTUSEINDEX) && (totalBytesRead < totalDataArea) && chunkRead) ||
                       (AVIF_MUSTUSEINDEX && (indexBytesRead < indexSize))) {

                if (AVIF_MUSTUSEINDEX) {
                    raFile.seek(indexPointer);
                    dataFound = false;

                    while (!dataFound) {
                        raFile.read(dataSignature);

                        if ((dataSignature[2] != 0x64 /*d */) || (dataSignature[3] < 0x62 /* b */) ||
                                (dataSignature[3] > 0x63 /* c */)) {
                            indexPointer = indexPointer + 16;
                            indexBytesRead += 16;
                            if (indexBytesRead >= indexSize) {
                                break loop1;
                            }
                            raFile.seek(indexPointer);
                        } else {
                            dataFound = true;
                        }
                    } // while (!dataFound)

                    indexPointer = indexPointer + 8;
                    raFile.seek(indexPointer);
                    moviOffset = getInt(endianess);
                    if (firstRun && (moviOffset == firstDataSignature)) {
                        moviPosition = 0L;
                    }
                    indexPointer = indexPointer + 8;
                    indexBytesRead += 16;
                    raFile.seek(moviPosition + (long) moviOffset);
                    firstRun = false;
                } // if (AVIFMUSTINDEX)

                raFile.read(dataSignature);
                totalBytesRead = totalBytesRead + 4;
                subchunkBytesRead = subchunkBytesRead + 4;

                if ((dataSignature[2] == 0x64 /* d */) &&
                        ((dataSignature[3] == 0x63 /* c */) || (dataSignature[3] == 0x62 /* b */))) {
                    dataLength = getInt(endianess);

                    if ((dataLength % 2) == 1) {
                        dataLength++;
                    }

                    if (dataLength > 2) {
                        // Only consider frames with dataLength > 0 in capturing
                        // and skipping
                        // RLE uses 2 bytes to show a repeat frame
                        actualFrames++;
                        if (captureCount < framesToCapture) {
                            doWrite[z] = true;
                            captureCount++;
                        }
                        else {
                            doWrite[z] = false;
                            skipCount++;
                        }
                        if (skipCount == framesToSkip) {
                            captureCount = 0;
                            skipCount = 0;
                        }
                    }
                    z++;

                    totalBytesRead = totalBytesRead + 4;
                    subchunkBytesRead += 4;

                    long ptr = raFile.getFilePointer();
                    raFile.seek(ptr + dataLength);
                    totalBytesRead = totalBytesRead + dataLength;
                    subchunkBytesRead += dataLength;
                } // else if ((dataSignature[2] == 0x64 /* d */) && (dataSignature[3] == 0x63 /* c */))
                else {
                    dataLength = getInt(endianess);

                    if ((dataLength % 2) == 1) {
                        dataLength++;
                    }

                    totalBytesRead = totalBytesRead + 4;
                    subchunkBytesRead += 4;

                    long ptr = raFile.getFilePointer();
                    raFile.seek(ptr + dataLength);
                    totalBytesRead = totalBytesRead + dataLength;
                    subchunkBytesRead += dataLength;
                } // else

                subchunkBlocksRead++;

                if (haveMoviSubchunk && (subchunkBlocksRead == streams) && (totalBytesRead < totalDataArea)) {
                    totalBytesRead += moviSubchunkPosition + LIST2subchunkSize - raFile.getFilePointer();
                    raFile.seek(moviSubchunkPosition + LIST2subchunkSize);

                    // Check for LIST rec<sp> subchunks
                    signature = getInt(endianess);
                    totalBytesRead += 4;

                    if (signature == 0x5453494C) {

                        // have read LIST
                        LIST2subchunkSize = getInt(endianess);
                        totalBytesRead += 4;
                        moviSubchunkPosition = raFile.getFilePointer();
                        CHUNKtype = getInt(endianess);

                        if (CHUNKtype == 0x20636572) {

                            // have read rec<sp>
                            totalBytesRead += 4;
                            subchunkBytesRead = 0;
                            subchunkBlocksRead = 0;
                        } else {
                            raFile.close();
                            throw new IOException("CHunktype for LIST2sbuchunk is an illegal = " + CHUNKtype);
                        }
                    } else {
                        chunkRead = false;
                    }
                } // if (haveMoviSubchunk && (subchunkBlocksRead == streams))
            } // while ((totalBytesRead < totalDataArea) && chunkRead)

            Preferences.debug("totalBytesRead = " + totalBytesRead + "\n", Preferences.DEBUG_FILEIO);
            Preferences.debug("totalDataArea = " + totalDataArea + "\n", Preferences.DEBUG_FILEIO);
            indexPointer = idx1Position + 8;
            indexBytesRead = 0;

            if (actualFrames > 1) {
                imgExtents = new int[3];
                imgExtents[2] = actualFrames;
            } else {
                imgExtents = new int[2];
            }
            actualFramesW = (actualFrames * framesToCapture)/(framesToCapture + framesToSkip);
            remainderFrames = actualFrames % (framesToCapture + framesToSkip);
            if (remainderFrames > framesToCapture) {
                remainderFrames = framesToCapture;   
            }
            actualFramesW += remainderFrames;
            saveHere = raFileW.getFilePointer();
            raFileW.seek(saveTotalFramesW);
            writeIntW(actualFramesW, endianess);
            raFileW.seek(saveLengthW);
            writeIntW(actualFramesW, endianess);
            raFileW.seek(saveHere);
            savedibPos = new long[actualFramesW];
            dcLength = new int[actualFramesW];
            zw = 0;

            imgExtents[0] = width;
            imgExtents[1] = height;
            
            if (compression == 0) {
                
                // Write the data record signature '00db' where db means the DIB bitmap data (uncompressed)
                // follows.  The characters 00 are used to identify the stream.
                dataSignatureW = new byte[4];
                dataSignatureW[0] = 48; // 0
                dataSignatureW[1] = 48; // 0
                dataSignatureW[2] = 100; // d
                dataSignatureW[3] = 98; // b
                
                // Each 3-byte triplet in the bitmap array represents the relative intensities
                // of blue, green, and red, respectively, for a pixel.  The color bytes are
                // in reverse order from the Windows convention.
            } // if (compression == 0)
            else { // compression == 1 or MSVC
                // Write the data record signature '00dc' where dc means that DIB bitmap data (compressed)
                // follows.  The characters 00 are used to identify the stream.
                dataSignatureW = new byte[4];
                dataSignatureW[0] = 48; // 0
                dataSignatureW[1] = 48; // 0
                dataSignatureW[2] = 100; // d
                dataSignatureW[3] = 99; // c
            } // else compression == 1

            // Now that the image is created this second read thru actually imports the data into the image.
            raFile.seek(startPosition);

            if (compression == 0) {

                dataSignature = new byte[4];

                totalDataArea = LIST2Size - 4; // Subtract out 4 'movi' bytes

                // Have encountered LiST2Size > raFile.length(), an impossibility
                remainingFileLength = (int) (raFile.length() - startPosition);

                if (totalDataArea > remainingFileLength) {
                    totalDataArea = remainingFileLength;
                }

                totalBytesRead = 0;

                // Check for LIST rec<sp> subchunks
                if (!AVIF_MUSTUSEINDEX) {
                    signature = getInt(endianess);

                    if (signature == 0x5453494C) {

                        // have read LIST
                        LIST2subchunkSize = getInt(endianess);
                        moviSubchunkPosition = raFile.getFilePointer();
                        CHUNKtype = getInt(endianess);

                        if (CHUNKtype == 0x20636572) {

                            // have read rec<sp>
                            haveMoviSubchunk = true;
                            subchunkBytesRead = 0;
                            subchunkBlocksRead = 0;
                        } else {
                            raFile.close();
                            throw new IOException("CHUNK type in second LIST sub CHUNK is an illegal = " + CHUNKtype);
                        }
                    } else {
                        raFile.seek(startPosition);
                    }
                } // if (1AVIF_MUSTUSEINDEX
                
                z = 0;
                chunkRead = true;

                while ((((!AVIF_MUSTUSEINDEX) && (totalBytesRead < totalDataArea) && chunkRead) ||
                           (AVIF_MUSTUSEINDEX && (indexBytesRead < indexSize))) && (zw < actualFramesW)) {

                    if (AVIF_MUSTUSEINDEX) {
                        raFile.seek(indexPointer);
                        dataFound = false;

                        while (!dataFound) {
                            raFile.read(dataSignature);

                            if ((dataSignature[2] != 0x64 /*d */) || (dataSignature[3] < 0x62 /* b */) ||
                                    (dataSignature[3] > 0x63 /* c */)) {
                                indexPointer = indexPointer + 16;
                                indexBytesRead += 16;
                                raFile.seek(indexPointer);
                            } else if (doWrite[z]) {
                                z++;
                                dataFound = true;
                            } else {
                                z++;
                                indexPointer = indexPointer + 16;
                                indexBytesRead += 16;
                                raFile.seek(indexPointer);
                            }
                        } // while (!dataFound)

                        indexPointer = indexPointer + 8;
                        raFile.seek(indexPointer);
                        moviOffset = getInt(endianess);
                        indexPointer = indexPointer + 8;
                        indexBytesRead += 16;
                        raFile.seek(moviPosition + (long) moviOffset);
                    } // if (AVIFMUSTINDEX)

                    raFile.read(dataSignature);
                    totalBytesRead = totalBytesRead + 4;
                    subchunkBytesRead += 4;

                    if ((dataSignature[2] != 0x64 /*d */) || (dataSignature[3] < 0x62 /* b */) ||
                            (dataSignature[3] > 0x63 /* c */)) {
                        dataLength = getInt(endianess);

                        if ((dataLength % 2) == 1) {
                            dataLength++;
                        }

                        totalBytesRead = totalBytesRead + 4;
                        subchunkBytesRead += 4;

                        long ptr = raFile.getFilePointer();
                        raFile.seek(ptr + dataLength);
                        totalBytesRead = totalBytesRead + dataLength;
                        subchunkBytesRead += 4;
                    } else {
                        dataLength = getInt(endianess);

                        if ((dataLength % 2) == 1) {
                            dataLength++;
                        }

                        totalBytesRead = totalBytesRead + 4;
                        subchunkBytesRead += 4;

                        if ((totalBytesRead + dataLength) <= totalDataArea) {
                            raFileW.write(dataSignature);
                            savedibPos[zw] = raFileW.getFilePointer();
                            dcLength[zw++] = dataLength;
                            writeIntW(dataLength, endianess);
                            fileBuffer = new byte[dataLength];
                            raFile.read(fileBuffer);
                            raFileW.write(fileBuffer);
                            totalBytesRead = totalBytesRead + dataLength;
                            subchunkBytesRead += dataLength;
                        } // if ((totalBytesRead + dataLength) <= totalDataArea)
                    } // else

                    subchunkBlocksRead++;

                    if (haveMoviSubchunk && (subchunkBlocksRead == streams) && (totalBytesRead < totalDataArea)) {
                        totalBytesRead += moviSubchunkPosition + LIST2subchunkSize - raFile.getFilePointer();
                        raFile.seek(moviSubchunkPosition + LIST2subchunkSize);

                        // Check for LIST rec<sp> subchunks
                        signature = getInt(endianess);
                        totalBytesRead += 4;

                        if (signature == 0x5453494C) {

                            // have read LIST
                            LIST2subchunkSize = getInt(endianess);
                            totalBytesRead += 4;
                            moviSubchunkPosition = raFile.getFilePointer();
                            CHUNKtype = getInt(endianess);

                            if (CHUNKtype == 0x20636572) {

                                // have read rec<sp>
                                totalBytesRead += 4;
                                subchunkBytesRead = 0;
                                subchunkBlocksRead = 0;
                            } else {
                                raFile.close();
                                throw new IOException("CHunktype for LIST2sbuchunk is an illegal = " + CHUNKtype);
                            }
                        } else {
                            chunkRead = false;
                        }
                    } // if (haveMoviSubchunk && (subchunkBlocksRead == streams))
                } // while ((totalBytesRead < (totalDataArea-8)) && chunkRead)
            } // if (compression == 0)
            else if (compression == 1) {
                dataSignature = new byte[4];
                totalDataArea = LIST2Size - 4; // Subtract out 4 'movi' bytes
                totalBytesRead = 0;

                // Check for LIST rec<sp> subchunks
                if (AVIF_MUSTUSEINDEX) {
                    signature = getInt(endianess);

                    if (signature == 0x5453494C) {

                        // have read LIST
                        LIST2subchunkSize = getInt(endianess);
                        moviSubchunkPosition = raFile.getFilePointer();
                        CHUNKtype = getInt(endianess);

                        if (CHUNKtype == 0x20636572) {

                            // have read rec<sp>
                            haveMoviSubchunk = true;
                            subchunkBytesRead = 0;
                            subchunkBlocksRead = 0;
                        } else {
                            raFile.close();
                            throw new IOException("CHUNK type in second LIST sub CHUNK is an illegal = " + CHUNKtype);
                        }
                    } else {
                        raFile.seek(startPosition);
                    }
                } // if (!AVIF_MUSTUSEINDEX)

                z = 0;
                chunkRead = true;

                while ((((!AVIF_MUSTUSEINDEX) && (totalBytesRead < totalDataArea) && chunkRead) ||
                           (AVIF_MUSTUSEINDEX && (indexBytesRead < indexSize))) && (zw < actualFramesW)) {

                    if (AVIF_MUSTUSEINDEX) {
                        raFile.seek(indexPointer);
                        dataFound = false;

                        while (!dataFound) {
                            raFile.read(dataSignature);

                            if ((dataSignature[2] != 0x64 /*d */) || (dataSignature[3] < 0x62 /* b */) ||
                                    (dataSignature[3] > 0x63 /* c */)) {
                                indexPointer = indexPointer + 16;
                                indexBytesRead += 16;
                                raFile.seek(indexPointer);
                            } else if (doWrite[z]) {
                                z++;
                                dataFound = true;
                            } else {
                                z++;
                                indexPointer = indexPointer + 16;
                                indexBytesRead += 16;
                                raFile.seek(indexPointer);
                            }
                        } // while (!dataFound)

                        indexPointer = indexPointer + 8;
                        raFile.seek(indexPointer);
                        moviOffset = getInt(endianess);
                        indexPointer = indexPointer + 8;
                        indexBytesRead += 16;
                        raFile.seek(moviPosition + (long) moviOffset);
                    } // if (AVIFMUSTINDEX)

                    raFile.read(dataSignature);
                    totalBytesRead = totalBytesRead + 4;
                    subchunkBytesRead += 4;

                    if ((dataSignature[2] == 0x64 /* d */) &&
                            ((dataSignature[3] == 0x63 /* c */) || (dataSignature[3] == 0x62 /* b */))) {
                       
                        dataLength = getInt(endianess);
                        totalBytesRead = totalBytesRead + 4;
                        subchunkBytesRead += 4;
                        raFileW.write(dataSignature);
                        savedibPos[zw]= raFileW.getFilePointer();
                        dcLength[zw++] = dataLength;
                        writeIntW(dataLength, endianess);
                        fileBuffer = new byte[dataLength];
                        raFile.read(fileBuffer);
                        raFileW.write(fileBuffer);
                        totalBytesRead = totalBytesRead + dataLength;
                        subchunkBytesRead += dataLength;

                        
                    } // else if ((dataSignature[2] == 0x64 /* d */) && (dataSignature[3] == 0x63 /* c */))
                    else {
                        dataLength = getInt(endianess);

                        if ((dataLength % 2) == 1) {
                            dataLength++;
                        }

                        totalBytesRead = totalBytesRead + 4;
                        subchunkBytesRead += 4;

                        long ptr = raFile.getFilePointer();
                        raFile.seek(ptr + dataLength);
                        totalBytesRead = totalBytesRead + dataLength;
                        subchunkBytesRead += 4;
                    } // else

                    subchunkBlocksRead++;

                    if (haveMoviSubchunk && (subchunkBlocksRead == streams) && (totalBytesRead < totalDataArea)) {
                        totalBytesRead += moviSubchunkPosition + LIST2subchunkSize - raFile.getFilePointer();
                        raFile.seek(moviSubchunkPosition + LIST2subchunkSize);

                        // Check for LIST rec<sp> subchunks
                        signature = getInt(endianess);
                        totalBytesRead += 4;

                        if (signature == 0x5453494C) {

                            // have read LIST
                            LIST2subchunkSize = getInt(endianess);
                            totalBytesRead += 4;
                            moviSubchunkPosition = raFile.getFilePointer();
                            CHUNKtype = getInt(endianess);

                            if (CHUNKtype == 0x20636572) {

                                // have read rec<sp>
                                totalBytesRead += 4;
                                subchunkBytesRead = 0;
                                subchunkBlocksRead = 0;
                            } else {
                                raFile.close();
                                throw new IOException("CHUNKtype for LIST2sbuchunk is an illegal = " + CHUNKtype);
                            }
                        } else {
                            chunkRead = false;
                        }
                    } // if (haveMoviSubchunk && (subchunkBlocksRead == streams))
                } // while ((totalBytesRead < totalDataArea) && chunkRead)

            } // else if (compression == 1)
            else if (doMSVC && (bitCount == 8)) {
                dataSignature = new byte[4];
                totalDataArea = LIST2Size - 4; // Subtract out 4 'movi' bytes
                totalBytesRead = 0;

                // Check for LIST rec<sp> subchunks
                if (!AVIF_MUSTUSEINDEX) {
                    signature = getInt(endianess);

                    if (signature == 0x5453494C) {

                        // have read LIST
                        LIST2subchunkSize = getInt(endianess);
                        moviSubchunkPosition = raFile.getFilePointer();
                        CHUNKtype = getInt(endianess);

                        if (CHUNKtype == 0x20636572) {

                            // have read rec<sp>
                            haveMoviSubchunk = true;
                            subchunkBytesRead = 0;
                            subchunkBlocksRead = 0;
                        } else {
                            raFile.close();
                            throw new IOException("CHUNK type in second LIST sub CHUNK is an illegal = " + CHUNKtype);
                        }
                    } else {
                        raFile.seek(startPosition);
                    }
                } // if (!AVIF_MUSTUSEINDEX)

                z = 0;
                chunkRead = true;

                while ((((!AVIF_MUSTUSEINDEX) && (totalBytesRead < totalDataArea) && chunkRead) ||
                           (AVIF_MUSTUSEINDEX && (indexBytesRead < indexSize))) && (zw < actualFramesW)) {

                    if (AVIF_MUSTUSEINDEX) {
                        raFile.seek(indexPointer);
                        dataFound = false;

                        while (!dataFound) {
                            raFile.read(dataSignature);

                            if ((dataSignature[2] != 0x64 /*d */) || (dataSignature[3] < 0x62 /* b */) ||
                                    (dataSignature[3] > 0x63 /* c */)) {
                                indexPointer = indexPointer + 16;
                                indexBytesRead += 16;
                                raFile.seek(indexPointer);
                            } else if (doWrite[z]) {
                                z++;
                                dataFound = true;
                            } else {
                                z++;
                                indexPointer = indexPointer + 16;
                                indexBytesRead += 16;
                                raFile.seek(indexPointer);
                            }
                        } // while (!dataFound)

                        indexPointer = indexPointer + 8;
                        raFile.seek(indexPointer);
                        moviOffset = getInt(endianess);
                        indexPointer = indexPointer + 8;
                        indexBytesRead += 16;
                        raFile.seek(moviPosition + (long) moviOffset);
                    } // if (AVIFMUSTINDEX)

                    raFile.read(dataSignature);
                    totalBytesRead = totalBytesRead + 4;
                    subchunkBytesRead += 4;

                    if ((dataSignature[2] == 0x64 /* d */) &&
                            ((dataSignature[3] == 0x63 /* c */) || (dataSignature[3] == 0x62 /* b */))) {
                        raFileW.write(dataSignature);
                        savedibPos[zw] = raFileW.getFilePointer();
                        dataLength = getInt(endianess);
                        dcLength[zw++] = dataLength;
                        writeIntW(dataLength, endianess);

                        if ((dataLength % 2) == 1) {
                            dataLength++;
                        }

                        totalBytesRead = totalBytesRead + 4;
                        subchunkBytesRead += 4;
                        fileBuffer = new byte[dataLength];
                        raFile.read(fileBuffer);
                        raFileW.write(fileBuffer);
                        totalBytesRead = totalBytesRead + dataLength;
                        subchunkBytesRead += dataLength;
                        
                    } // else if ((dataSignature[2] == 0x64 /* d */) && (dataSignature[3] == 0x63 /* c */))
                    else {
                        dataLength = getInt(endianess);

                        if ((dataLength % 2) == 1) {
                            dataLength++;
                        }

                        totalBytesRead = totalBytesRead + 4;
                        subchunkBytesRead += 4;

                        long ptr = raFile.getFilePointer();
                        raFile.seek(ptr + dataLength);
                        totalBytesRead = totalBytesRead + dataLength;
                        subchunkBytesRead += dataLength;
                    } // else

                    subchunkBlocksRead++;

                    if (haveMoviSubchunk && (subchunkBlocksRead == streams) && (totalBytesRead < totalDataArea)) {
                        totalBytesRead += moviSubchunkPosition + LIST2subchunkSize - raFile.getFilePointer();
                        raFile.seek(moviSubchunkPosition + LIST2subchunkSize);

                        // Check for LIST rec<sp> subchunks
                        signature = getInt(endianess);
                        totalBytesRead += 4;

                        if (signature == 0x5453494C) {

                            // have read LIST
                            LIST2subchunkSize = getInt(endianess);
                            totalBytesRead += 4;
                            moviSubchunkPosition = raFile.getFilePointer();
                            CHUNKtype = getInt(endianess);

                            if (CHUNKtype == 0x20636572) {

                                // have read rec<sp>
                                totalBytesRead += 4;
                                subchunkBytesRead = 0;
                                subchunkBlocksRead = 0;
                            } else {
                                raFile.close();
                                throw new IOException("CHunktype for LIST2sbuchunk is an illegal = " + CHUNKtype);
                            }
                        } else {
                            chunkRead = false;
                        }
                    } // if (haveMoviSubchunk && (subchunkBlocksRead == streams))
                } // while ((totalBytesRead < totalDataArea) && chunkRead)
            } // else if (doMSVC && (bitCount == 8))
            else { // doMSVC && (bitCount == 16)
                dataSignature = new byte[4];
                totalDataArea = LIST2Size - 4; // Subtract out 4 'movi' bytes
                totalBytesRead = 0;

                // Check for LIST rec<sp> subchunks
                if (!AVIF_MUSTUSEINDEX) {
                    signature = getInt(endianess);

                    if (signature == 0x5453494C) {

                        // have read LIST
                        LIST2subchunkSize = getInt(endianess);
                        moviSubchunkPosition = raFile.getFilePointer();
                        CHUNKtype = getInt(endianess);

                        if (CHUNKtype == 0x20636572) {

                            // have read rec<sp>
                            haveMoviSubchunk = true;
                            subchunkBytesRead = 0;
                            subchunkBlocksRead = 0;
                        } else {
                            raFile.close();
                            throw new IOException("CHUNK type in second LIST sub CHUNK is an illegal = " + CHUNKtype);
                        }
                    } else {
                        raFile.seek(startPosition);
                    }
                } // if (!AVIF_MUSTUSEINDEX)

                z = 0;

                chunkRead = true;

                while ((((!AVIF_MUSTUSEINDEX) && (totalBytesRead < totalDataArea) && chunkRead) ||
                           (AVIF_MUSTUSEINDEX && (indexBytesRead < indexSize))) && (zw < actualFramesW)) {

                    if (AVIF_MUSTUSEINDEX) {
                        raFile.seek(indexPointer);
                        dataFound = false;

                        while (!dataFound) {
                            
                            raFile.read(dataSignature);

                            if ((dataSignature[2] != 0x64 /*d */) || (dataSignature[3] < 0x62 /* b */) ||
                                    (dataSignature[3] > 0x63 /* c */)) {
                                indexPointer = indexPointer + 16;
                                indexBytesRead += 16;
                                raFile.seek(indexPointer);
                            } else if (doWrite[z]) {
                                z++;
                                dataFound = true;
                            } else {
                                z++;
                                indexPointer = indexPointer + 16;
                                indexBytesRead += 16;
                                raFile.seek(indexPointer);
                            }
                        } // while (!dataFound)
                        indexPointer = indexPointer + 8;
                        raFile.seek(indexPointer);
                        moviOffset = getInt(endianess);
                        indexPointer = indexPointer + 8;
                        indexBytesRead += 16;
                        raFile.seek(moviPosition + (long) moviOffset);
                    } // if (AVIFMUSTINDEX)

                    raFile.read(dataSignature);
                    totalBytesRead = totalBytesRead + 4;
                    subchunkBytesRead += 4;

                    if ((dataSignature[2] == 0x64 /* d */) &&
                            ((dataSignature[3] == 0x63 /* c */) || (dataSignature[3] == 0x62 /* b */))) {
                        raFileW.write(dataSignature);
                        savedibPos[zw] = raFileW.getFilePointer();
                        dataLength = getInt(endianess);
                        dcLength[zw++] = dataLength;
                        writeIntW(dataLength, endianess);

                        if ((dataLength % 2) == 1) {
                            dataLength++;
                        }

                        totalBytesRead = totalBytesRead + 4;
                        subchunkBytesRead += 4;
                        fileBuffer = new byte[dataLength];
                        raFile.read(fileBuffer);
                        raFileW.write(fileBuffer);
                        totalBytesRead = totalBytesRead + dataLength;
                        subchunkBytesRead += dataLength;
                    } // else if ((dataSignature[2] == 0x64 /* d */) && (dataSignature[3] == 0x63 /* c */))
                    else {
                        dataLength = getInt(endianess);

                        if ((dataLength % 2) == 1) {
                            dataLength++;
                        }

                        totalBytesRead = totalBytesRead + 4;
                        subchunkBytesRead += 4;

                        long ptr = raFile.getFilePointer();
                        raFile.seek(ptr + dataLength);
                        totalBytesRead = totalBytesRead + dataLength;
                        subchunkBytesRead += dataLength;
                    } // else

                    subchunkBlocksRead++;

                    if (haveMoviSubchunk && (subchunkBlocksRead == streams) && (totalBytesRead < totalDataArea)) {
                        totalBytesRead += moviSubchunkPosition + LIST2subchunkSize - raFile.getFilePointer();
                        raFile.seek(moviSubchunkPosition + LIST2subchunkSize);

                        // Check for LIST rec<sp> subchunks
                        signature = getInt(endianess);
                        totalBytesRead += 4;

                        if (signature == 0x5453494C) {

                            // have read LIST
                            LIST2subchunkSize = getInt(endianess);
                            totalBytesRead += 4;
                            moviSubchunkPosition = raFile.getFilePointer();
                            CHUNKtype = getInt(endianess);

                            if (CHUNKtype == 0x20636572) {

                                // have read rec<sp>
                                totalBytesRead += 4;
                                subchunkBytesRead = 0;
                                subchunkBlocksRead = 0;
                            } else {
                                raFile.close();
                                throw new IOException("CHunktype for LIST2sbuchunk is an illegal = " + CHUNKtype);
                            }
                        } else {
                            chunkRead = false;
                        }
                    } // if (haveMoviSubchunk && (subchunkBlocksRead == streams))
                } // while ((totalBytesRead < totalDataArea) && chunkRead)
            } // else for doMSVC && (bitCount == 16)

            raFile.close();
            
            // Write the idx1 CHUNK
            // Write the 'idx1' signature
            idx1Pos = raFileW.getFilePointer();
            raFileW.seek(saveLIST2Size);
            writeIntW((int) (idx1Pos - (saveLIST2Size + 4)), endianess);
            raFileW.seek(idx1Pos);
            
            byte[] idx1Signature = new byte[4];
            idx1Signature[0] = 105; // i
            idx1Signature[1] = 100; // d
            idx1Signature[2] = 120; // x
            idx1Signature[3] = 49; // 1
            raFileW.write(idx1Signature);
            
            // Write the length of the idx1 CHUNK not including the idx1 signature and the 4 length
            // bytes.  Write 0 for now.
            saveidx1Length = raFileW.getFilePointer();
            writeIntW(0, endianess);
            for (z = 0; z < actualFramesW; z++) {
                raFileW.write(dataSignatureW);
                writeIntW(0x10, endianess);
                writeIntW((int)(savedibPos[z] - 4 - savemovi), endianess);
                writeIntW(dcLength[z], endianess);
            }
            
            endPos = raFileW.getFilePointer();
            raFileW.seek(saveFileSize);
            writeIntW((int)(endPos - (saveFileSize + 4)), endianess);
            raFileW.seek(saveidx1Length);
            writeIntW((int)(endPos - (saveidx1Length + 4)), endianess);
            raFileW.close();
            System.out.println("Finished " + outputFileName + " creation");
        } catch (OutOfMemoryError error) {
            System.gc();
            throw error;
        }
        return true;    
    }
    
    /**
     * Writes an int as four bytes to a file.
     *
     * @param      data       Data to be written to file.
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @exception  IOException  if there is an error writing the file
     */
    public final void writeIntW(int data, boolean bigEndian) throws IOException {
        byte[] buffer = new byte[4];

        if (bigEndian) {
            buffer[0] = (byte) (data >>> 24);
            buffer[1] = (byte) (data >>> 16);
            buffer[2] = (byte) (data >>> 8);
            buffer[3] = (byte) (data & 0xff);
        } else {
            buffer[0] = (byte) (data & 0xff);
            buffer[1] = (byte) (data >>> 8);
            buffer[2] = (byte) (data >>> 16);
            buffer[3] = (byte) (data >>> 24);
        }

        raFileW.write(buffer);
    }
    
    /**
     * Writes a short as two bytes to a file.
     *
     * @param      data       Data to be written to file.
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @exception  IOException  if there is an error writing the file
     */
    public final void writeShortW(short data, boolean bigEndian) throws IOException {
        byte[] buffer = new byte[2];

        if (bigEndian) {
            buffer[0] = (byte) (data >>> 8);
            buffer[1] = (byte) (data & 0xff);
        } else {
            buffer[0] = (byte) (data & 0xff);
            buffer[1] = (byte) (data >>> 8);
        }

        raFileW.write(buffer);
    }
}
