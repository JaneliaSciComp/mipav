package gov.nih.mipav.model.file;


import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.ModelImage;

import java.io.*;
import javax.swing.event.EventListenerList;
import javax.swing.event.ChangeListener;
import javax.swing.event.ChangeEvent;

/**
 * FileBase is an abstract class that has many methods that support the reading/writing of files.
 *
 * @version  0.9 June 30, 1998
 * @see      FileInfoBase
 */
public abstract class FileBase{

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Ill defined file type. */
    public static final int ERROR = -1;

    /** Undefined file type. */
    public static final int UNDEFINED = 0;

    /** Not presently implemented. */
    public static final int MIPAV = 1;

    /** RAW image data, no header. */
    public static final int RAW = 2;

    /** RAW MULTIFLE image data, no header. */
    public static final int RAW_MULTIFILE = 25;

    /** TIFF file; tagged header. */
    public static final int TIFF = 3;

    /** VOI file, used to read VOIs. */
    public static final int VOI_FILE = 4;

    /** Analyze format (Mayo). */
    public static final int ANALYZE = 5;

    /** Digital Imaging and COmmunications in Medicine file type. Fully implemented versions 2 & 3. */
    public static final int DICOM = 6;

    /** Medvision file type. */
    public static final int MEDVISION = 7;

    /** Benes Trus special file type. */
    public static final int MAP = 8;

    /** Java Image Manangement Interface file type. */
    public static final int JIMI = 9;

    /** Multiple files of TIFF images. */
    public static final int TIFF_MULTIFILE = 10;

    /**
     * MINC file type. MINC is a medical imaging oriented extension of the NetCDF file format. NetCDF stands for
     * `Network Common Data Form'.
     */
    public static final int MINC = 11;

    /** AVI file type. Windows Media. */
    public static final int AVI = 12;

    /** Multiple files of type analyze. */
    public static final int ANALYZE_MULTIFILE = 13;

    /** Quicktime file type. */
    public static final int QT = 14;

    /** Cheshire file type (a kind of Analyze). */
    public static final int CHESHIRE = 15;

    /** Cheshire overlay file type. Contains VOIs. */
    public static final int CHESHIRE_OVERLAY = 16;

    /** AFNI file type. */
    public static final int AFNI = 17;

    /** FITS file type. */
    public static final int FITS = 18;

    /** MetaMorph Stack (STK) file type. */
    public static final int STK = 19;

    /** Siemens MAGNETOM VISION. */
    public static final int MAGNETOM_VISION = 20;

    /** GE Genesis 5X and LX. */
    public static final int GE_GENESIS = 21;

    /** MRC file format used by IMOD. */
    public static final int MRC = 22;

    /** Interfile file format used in Nuclear Medicine. */
    public static final int INTERFILE = 23;


    /** Micro CT format for small animal imaging. */
    public static final int MICRO_CAT = 24;

    /** Used by the Zeiss LSM 510 Dataserver. */
    public static final int LSM = 26;

    /** Used by the Zeiss LSM 510 Dataserver. */
    public static final int LSM_MULTIFILE = 37;

    /** Used by the Bio-Rad Pic format. */
    public static final int BIORAD = 27;

    /** Used by FreeSurfer software. */
    public static final int COR = 28;

    /** Bruker file format. */
    public static final int BRUKER = 29;

    /** MIPAV XML file format. */
    public static final int XML = 30;

    /** MIPAV XML file format. */
    public static final int XML_MULTIFILE = 31;

    /** SPM file format. */
    public static final int SPM = 32;

    /** MIPAV project format. */
    public static final int PROJECT = 33;

    /** NIFTI format. */
    public static final int NIFTI = 34;

    /** NIFTI multi-file format. */
    public static final int NIFTI_MULTIFILE = 35;

    /** Image Cytometry Standard. */
    public static final int ICS = 36;

    /** Optical coherence tomography. */
    public static final int TMG = 38;

    /** Washington University OSM dataset structure. */
    public static final int OSM = 39;

    /** MIPAV Surface XML file format. */
    public static final int SURFACE_XML = 40;

    /** Gatan's Digital Micrograph version 3 file format. */
    public static final int DM3 = 41;
    
    /** Nearly raw raster data */
    public static final int NRRD = 42;
    
    /** GE Signa 4.x */
    public static final int GE_SIGNA4X = 43;
    
    /** MGH/MGZ volume format */
    public static final int MGH = 44;

    /** Arrary of strings describing the file formats. */
    private static String[] fileFormatStr = {
        "Undefined", "MIPAV", "Raw", "Tiff", "VOI", "Analyze", "DICOM", "Medvision", "Map", "JIMI", "Tiff multifile",
        "MINC", "Avi", "Analyze multifile", "QT", "Chesire", "Chesire Overlay", "Afni", "FITS", "STK",
        "Magnetom Vision", "GE Genesis", "MRC", "Interfile", "Raw multifile", "Micro CAT", "LSM", "Bio-Rad", "COR",
        "BRUKER", "XML", "XML multifile", "SPM", "Project", "NIFTI", "NIFTI multifile", "ICS", "LSM multifile", "TMG",
        "OSM", "Surface XML", "DM3", "NRRD", "GE Signa4x", "MGH"
    };

    /** Byte order. Rightmost byte is most significant. */
    public static boolean LITTLE_ENDIAN = false;

    /** Byte order. Leftmost byte is most significant. */
    public static boolean BIG_ENDIAN = true;

    /** Read only access. */
    public static final int READ = 0;

    /** Read-write access. */
    public static final int READ_WRITE = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Flag indicating if the progress bar should be shown. */
    protected boolean pBarVisible = true;

    /** Progress bar to show when reading in image file. */
    protected ProgressBarInterface progressBar;

    /** Pointer to file to read or write from. */
    protected RandomAccessFile raFile;
    
    /**
     * A list of the ChangeListeners which are interested in the ChangeEvent. 
     */
    private EventListenerList listenerList;
    
    private boolean bigEndian;
    protected int bitsPerPixel;
    /**
     * The file names which are opened.
     */
    protected String[] fileNames;
    private int dataType;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor.
     */
    public FileBase() {
        this(null);
    }

    public FileBase(String[] fileNames){
        this.fileNames = fileNames;
        listenerList = new EventListenerList();        
    }
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Returns the string for a particular file format.
     *
     * @param   format  int representing the file format (see the static definitions)
     *
     * @return  string representing the file format
     */
    public static String getFileFormatStr(int format) {

        if (format == FileBase.ERROR) {
            return "Error";
        }

        try {
            return FileBase.fileFormatStr[format];
        } catch (ArrayIndexOutOfBoundsException ae) { }

        return "";

    } // end getFileFormatStr()

//    public abstract String getHeaderFile();
    
//    public abstract String[] getImageFiles();
    /**
     * Prepares this class for cleanup.
     */
    public void finalize() {
        progressBar = null;

        if (raFile != null) {

            try {
                raFile.close();
            } catch (IOException ioe) {
                // Do nothing
            }
        }

        raFile = null;
    }

    /**
     * Converts byte data to float data.
     *
     * @param   buffer     Array of byte data.
     * @param   index      Index into array data.
     * @param   bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                     endian.
     *
     * @return  Float value extracted from byte array.
     */
    public final float getBufferFloat(byte[] buffer, int index, boolean bigEndian) {
        int tmpInt;

        if (bigEndian) {
            tmpInt = (((buffer[index] & 0xff) << 24) | ((buffer[index + 1] & 0xff) << 16) |
                          ((buffer[index + 2] & 0xff) << 8) | (buffer[index + 3] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        } else {
            tmpInt = (((buffer[index + 3] & 0xff) << 24) | ((buffer[index + 2] & 0xff) << 16) |
                          ((buffer[index + 1] & 0xff) << 8) | (buffer[index] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        }
    }


    /**
     * Converts byte data to int data.
     *
     * @param   buffer     Array of byte data.
     * @param   index      Index into array data.
     * @param   bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                     endian.
     *
     * @return  Integer value extracted from byte array.
     */
    public final int getBufferInt(byte[] buffer, int index, boolean bigEndian) {

        if (bigEndian) {
            return (((buffer[index] & 0xff) << 24) | ((buffer[index + 1] & 0xff) << 16) |
                        ((buffer[index + 2] & 0xff) << 8) | (buffer[index + 3] & 0xff));
        } else {
            return (((buffer[index + 3] & 0xff) << 24) | ((buffer[index + 2] & 0xff) << 16) |
                        ((buffer[index + 1] & 0xff) << 8) | (buffer[index] & 0xff));
        }
    }


    /**
     * Converts byte data to short data.
     *
     * @param   buffer     Array of byte data.
     * @param   index      Index into array data.
     * @param   bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                     endian.
     *
     * @return  Short value extracted from byte array.
     */
    public final short getBufferShort(byte[] buffer, int index, boolean bigEndian) {

        if (bigEndian) {
            return (short) (((buffer[index] & 0xff) << 8) | (buffer[index + 1] & 0xff));
        } else {
            return (short) (((buffer[index + 1] & 0xff) << 8) | (buffer[index] & 0xff));
        }
    }

    /**
     * Reads eight unsigned bytes from file.
     *
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of the double read from the file.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final double getDouble(boolean bigEndian) throws IOException {
        byte[] buffer = new byte[8];
        raFile.readFully(buffer);

        long tmpLong;

        if (bigEndian) {
            tmpLong = (((buffer[0] & 0xffL) << 56) | ((buffer[1] & 0xffL) << 48) |
                           ((buffer[2] & 0xffL) << 40) | ((buffer[3] & 0xffL) << 32) |
                           ((buffer[4] & 0xffL) << 24) | ((buffer[5] & 0xffL) << 16) |
                           ((buffer[6] & 0xffL) << 8) | (buffer[7] & 0xffL));

            return (Double.longBitsToDouble(tmpLong));
        } else {
            tmpLong = (((buffer[7] & 0xffL) << 56) | ((buffer[6] & 0xffL) << 48) |
                           ((buffer[5] & 0xffL) << 40) | ((buffer[4] & 0xffL) << 32) |
                           ((buffer[3] & 0xffL) << 24) | ((buffer[2] & 0xffL) << 16) |
                           ((buffer[1] & 0xffL) << 8) | (buffer[0] & 0xffL));

            return (Double.longBitsToDouble(tmpLong));
        }
    }


    /**
     * Reads four unsigned bytes from file.
     *
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of the float read from the file.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final float getFloat(boolean bigEndian) throws IOException {
        byte[] buffer = new byte[4];
        raFile.readFully(buffer);

        int tmpInt;

        if (bigEndian) {
            tmpInt = (((buffer[0] & 0xff) << 24) | ((buffer[1] & 0xff) << 16) |
                          ((buffer[2] & 0xff) << 8) | (buffer[3] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        } else {
            tmpInt = (((buffer[3] & 0xff) << 24) | ((buffer[2] & 0xff) << 16) |
                          ((buffer[1] & 0xff) << 8) | (buffer[0] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        }
    }


    /**
     * Reads four signed bytes from file.
     *
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of the integer read from the file.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final int getInt(boolean bigEndian) throws IOException {
        byte[] buffer = new byte[4];
        raFile.readFully(buffer);

        if (bigEndian) {
            return (((buffer[0] & 0xff) << 24) | ((buffer[1] & 0xff) << 16) | ((buffer[2] & 0xff) << 8) |
                        (buffer[3] & 0xff)); // Big Endian
        } else {
            return (((buffer[3] & 0xff) << 24) | ((buffer[2] & 0xff) << 16) | ((buffer[1] & 0xff) << 8) |
                        (buffer[0] & 0xff));
        }
    }

    /**
     * Reads eight unsigned bytes from file.
     *
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of the long read from the file.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final long getLong(boolean bigEndian) throws IOException {
        byte[] buffer = new byte[8];

        raFile.readFully(buffer);

        long tmpLong;

        if (bigEndian) {
            tmpLong = (((buffer[0] & 0xffL) << 56) | ((buffer[1] & 0xffL) << 48) |
                           ((buffer[2] & 0xffL) << 40) | ((buffer[3] & 0xffL) << 32) |
                           ((buffer[4] & 0xffL) << 24) | ((buffer[5] & 0xffL) << 16) |
                           ((buffer[6] & 0xffL) << 8) | (buffer[7] & 0xffL));

            return (tmpLong);
        } else {
            tmpLong = (((buffer[7] & 0xffL) << 56) | ((buffer[6] & 0xffL) << 48) |
                           ((buffer[5] & 0xffL) << 40) | ((buffer[4] & 0xffL) << 32) |
                           ((buffer[3] & 0xffL) << 24) | ((buffer[2] & 0xffL) << 16) |
                           ((buffer[1] & 0xffL) << 8) | (buffer[0] & 0xffL));

            return (tmpLong);
        }
    }

    /**
     * Reads two byte signed short from file.
     *
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of signed short read from the file returned as an int.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final int getSignedShort(boolean bigEndian) throws IOException {
        int b3 = 0;
        byte[] buffer = new byte[2];

        raFile.readFully(buffer);

        if (bigEndian) {
            b3 = ((buffer[0] & 0xff) << 8) | (buffer[1] & 0xff);
        } else {
            b3 = ((buffer[1] & 0xff) << 8) | (buffer[0] & 0xff);
        }

        if ((b3 & 0x0080) != 0) {
            b3 = b3 | 0xff00;
        }

        return b3;
    }


    /**
     * Reads a string from a file of given <code>length</code>.
     *
     * @param      length  Number of bytes that form the string.
     *
     * @return     The string read from the file.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final String getString(int length) throws IOException {

        if (length <= 0) {
            return new String("");
        }

        byte[] b = new byte[length];
        raFile.readFully(b);

        return new String(b);
    }

    /**
     * Reads four unsigned bytes from file.
     *
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of the integer read from the file.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final long getUInt(boolean bigEndian) throws IOException {
        byte[] buffer = new byte[4];
        raFile.readFully(buffer);

        if (bigEndian) {
            return (((buffer[0] & 0xffL) << 24) | ((buffer[1] & 0xffL) << 16) |
                        ((buffer[2] & 0xffL) << 8) | (buffer[3] & 0xffL)); // Big Endian
        } else {
            return (((buffer[3] & 0xffL) << 24) | ((buffer[2] & 0xffL) << 16) |
                        ((buffer[1] & 0xffL) << 8) | (buffer[0] & 0xffL));
        }
    }

    /**
     * Converts byte data to unsigned int.
     *
     * @param   buffer  Buffer of signed bytes.
     * @param   index   Index points to location in buffer.
     *
     * @return  Integer converted from as "signed byte".
     */
    public final int getUnsignedByte(byte[] buffer, int index) {
        return (buffer[index] & 0xff);
    }

    /**
     * Reads two unsigned bytes from file.
     *
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of unsigned short read from the file returned as an int.
     *
     * @exception  IOException  if there is an error reading the file
     */
    public final int getUnsignedShort(boolean bigEndian) throws IOException {
        byte[] buffer = new byte[2];
        raFile.readFully(buffer);

        if (bigEndian) {
            return (((buffer[0] & 0xff) << 8) | (buffer[1] & 0xff)); // Big Endian
        } else {
            return (((buffer[1] & 0xff) << 8) | (buffer[0] & 0xff)); // Little Endian
        }
    }

    /**
     * Returns flag that indicates that the progressBar is visible.
     *
     * @return  <code>true</code> if progress bar is visible, <code>false</code> if not visible.
     */
    public boolean isProgressBarVisible() {
        return pBarVisible;
    }

    /**
     * Sets byte buffer with int.
     *
     * @param  buffer     Byte buffers where data is to be stored.
     * @param  data       Float data is broken down in bytes and stored in the byte buffer.
     * @param  i          Index into byte buffer.
     * @param  bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
     */
    public final void setBufferFloat(byte[] buffer, float data, int i, boolean bigEndian) {
        int tmpInt;

        tmpInt = Float.floatToIntBits(data);
        setBufferInt(buffer, tmpInt, i, bigEndian);
    }

    /**
     * Sets byte buffer with int.
     *
     * @param  buffer     Byte buffers where data is to be stored.
     * @param  data       Integer data is broken down in bytes and stored in the byte buffer.
     * @param  i          Index into byte buffer.
     * @param  bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
     */
    public final void setBufferInt(byte[] buffer, int data, int i, boolean bigEndian) {

        if (bigEndian) {
            buffer[i] = (byte) (data >>> 24);
            buffer[i + 1] = (byte) (data >>> 16);
            buffer[i + 2] = (byte) (data >>> 8);
            buffer[i + 3] = (byte) (data & 0xff);
        } else {
            buffer[i] = (byte) (data & 0xff);
            buffer[i + 1] = (byte) (data >>> 8);
            buffer[i + 2] = (byte) (data >>> 16);
            buffer[i + 3] = (byte) (data >>> 24);
        }
    }

    /**
     * Sets byte buffer with long.
     *
     * @param  buffer     Byte buffers where data is to be stored.
     * @param  data       Long data is broken down in bytes and stored in the byte buffer.
     * @param  i          Index into byte buffer.
     * @param  bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
     */
    public final void setBufferLong(byte[] buffer, long data, int i, boolean bigEndian) {

        if (bigEndian) {
            buffer[i] = (byte) (data >>> 56);
            buffer[i + 1] = (byte) (data >>> 48);
            buffer[i + 2] = (byte) (data >>> 40);
            buffer[i + 3] = (byte) (data >>> 32);
            buffer[i + 4] = (byte) (data >>> 24);
            buffer[i + 5] = (byte) (data >>> 16);
            buffer[i + 6] = (byte) (data >>> 8);
            buffer[i + 7] = (byte) (data & 0xff);
        } else {
            buffer[i] = (byte) (data & 0xff);
            buffer[i + 1] = (byte) (data >>> 8);
            buffer[i + 2] = (byte) (data >>> 16);
            buffer[i + 3] = (byte) (data >>> 24);
            buffer[i + 4] = (byte) (data >>> 32);
            buffer[i + 5] = (byte) (data >>> 40);
            buffer[i + 6] = (byte) (data >>> 48);
            buffer[i + 7] = (byte) (data >>> 56);
        }
    }


    /**
     * Sets byte buffer with int.
     *
     * @param  buffer     Byte buffers where data is to be stored.
     * @param  data       Short data is broken down in bytes and stored in the byte buffer.
     * @param  i          Index into byte buffer.
     * @param  bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
     */
    public final void setBufferShort(byte[] buffer, short data, int i, boolean bigEndian) {

        if (bigEndian) {
            buffer[i] = (byte) (data >>> 8);
            buffer[i + 1] = (byte) (data & 0xff);
        } else {
            buffer[i] = (byte) (data & 0xff);
            buffer[i + 1] = (byte) (data >>> 8);
        }
    }

    /**
     * Return true if the byte order is big endian.
     * @return true if the byte order is big endian.
     */
    public boolean isBigEndian(){
        return bigEndian;
    }
    /**
     * Sets byte buffer with int.
     *
     * @param  buffer  Byte buffers where data is to be stored.
     * @param  str     String containing integer data which is broken down in bytes and stored in the byte buffer.
     * @param  i       Index into byte buffer.
     */
    public final void setBufferString(byte[] buffer, String str, int i) {

        byte[] tmpBuffer;

        tmpBuffer = str.getBytes();

        for (int c = 0; c < tmpBuffer.length; c++) {
            buffer[i + c] = tmpBuffer[c];
        }
    }

    /**
     * Sets whether or not the progress bar should be visible.
     *
     * @param  flag  <code>true</code> if should be visible, <code>false</code> if not visible.
     */
    public void setProgressBarVisible(boolean flag) {
        pBarVisible = flag;
    }

    /**
     * Writes a double as eight bytes to a file.
     *
     * @param      data       Data to be written to file.
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @exception  IOException  if there is an error writing the file
     */
    public final void writeDouble(double data, boolean bigEndian) throws IOException {

        long tmpLong;

        tmpLong = Double.doubleToLongBits(data);
        writeLong(tmpLong, bigEndian);
    }

    /**
     * Writes a float as four bytes to a file.
     *
     * @param      data       Data to be written to file.
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @exception  IOException  if there is an error writing the file
     */
    public final void writeFloat(float data, boolean bigEndian) throws IOException {
        int tmpInt;

        tmpInt = Float.floatToIntBits(data);
        writeInt(tmpInt, bigEndian);
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
    public final void writeInt(int data, boolean bigEndian) throws IOException {
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

        raFile.write(buffer);
    }

    /**
     * Writes a long as eight bytes to a file.
     *
     * @param      data       Data to be written to file.
     * @param      bigEndian  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @exception  IOException  if there is an error writing the file
     */
    public final void writeLong(long data, boolean bigEndian) throws IOException {
        byte[] buffer = new byte[8];

        if (bigEndian) {
            buffer[0] = (byte) (data >>> 56);
            buffer[1] = (byte) (data >>> 48);
            buffer[2] = (byte) (data >>> 40);
            buffer[3] = (byte) (data >>> 32);
            buffer[4] = (byte) (data >>> 24);
            buffer[5] = (byte) (data >>> 16);
            buffer[6] = (byte) (data >>> 8);
            buffer[7] = (byte) (data & 0xff);
        } else {
            buffer[0] = (byte) (data & 0xff);
            buffer[1] = (byte) (data >>> 8);
            buffer[2] = (byte) (data >>> 16);
            buffer[3] = (byte) (data >>> 24);
            buffer[4] = (byte) (data >>> 32);
            buffer[5] = (byte) (data >>> 40);
            buffer[6] = (byte) (data >>> 48);
            buffer[7] = (byte) (data >>> 56);
        }

        raFile.write(buffer);
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
    public final void writeShort(short data, boolean bigEndian) throws IOException {
        byte[] buffer = new byte[2];

        if (bigEndian) {
            buffer[0] = (byte) (data >>> 8);
            buffer[1] = (byte) (data & 0xff);
        } else {
            buffer[0] = (byte) (data & 0xff);
            buffer[1] = (byte) (data >>> 8);
        }

        raFile.write(buffer);
    }
    
    // public abstract String[] getExtensions();
    /**
     * Adds the ProgressChangeListener to this FileBase object.
     * 
     * @param l
     */
    public void addProgressChangeListener(ProgressChangeListener l){
        listenerList.add(ProgressChangeListener.class, l);
    }
    
    /**
     * Removes the ChangeListener from the FileBase object.
     * 
     * @param l
     */
    public void removeProgressChangeListener(ProgressChangeListener l){
        listenerList.remove(ProgressChangeListener.class, l);
    }
    
    /**
     * Notifies all listeners that have registered interest for notification
     * on this event type.
     * 
     * @param value   the value of the progress bar.
     * @param title   the title of the progress dialog.
     * @param message the message for that specific progress value.
     */
    public void fireProgressStateChanged(int value, String title, String message){
        Object[] listeners = listenerList.getListenerList();
        for(int i = listeners.length-2; i >= 0; i -= 2){
            if(listeners[i] == ProgressChangeListener.class){
                ProgressChangeEvent event = new ProgressChangeEvent(this, value, title, message);
                ((ProgressChangeListener)listeners[i+1]).progressStateChanged(event);
            }
        }
    }

    /**
     * Notifies all listeners that have registered interest for notification
     * on this event type.
     * 
     * @param value  the value of the progress bar.
     */
    public void fireProgressStateChanged(int value){
        fireProgressStateChanged(value, null, null);
    }

    /**
     * 
     * @param dataType
     */
    public static int getBitsPerPixel(int dataType){
        return 0;
    }

    public int getDataType(){
        return dataType;
    }

    /**
     * 
     * @param dataType
     */
    public void setDataType(int dataType){
        this.dataType = dataType;
    }

    /**
     * 
     * @param bigEndian
     */
    public void setEndianess(boolean bigEndian){
        this.bigEndian = bigEndian;
    }

    public void setBigEndian(boolean bigEndian){
        this.bigEndian = bigEndian;
    }
    /**
     * 
     * @param bigEndian
     * @param index
     * @param bytes
     */
    public static float bytesToFloat(boolean bigEndian, int index, byte[] buffer){
        int tmpInt;

        if (bigEndian) {
            tmpInt = (((buffer[index] & 0xff) << 24) | ((buffer[index + 1] & 0xff) << 16) |
                          ((buffer[index + 2] & 0xff) << 8) | (buffer[index + 3] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        } else {
            tmpInt = (((buffer[index + 3] & 0xff) << 24) | ((buffer[index + 2] & 0xff) << 16) |
                          ((buffer[index + 1] & 0xff) << 8) | (buffer[index] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        }
    }

    /**
     * 
     * @param bigEndian
     * @param index
     * @param buffer
     */
    public static int bytesToInt(boolean bigEndian, int index, byte[] buffer){
        if (bigEndian) {
            return (((buffer[index] & 0xff) << 24) | ((buffer[index + 1] & 0xff) << 16) |
                        ((buffer[index + 2] & 0xff) << 8) | (buffer[index + 3] & 0xff));
        } else {
            return (((buffer[index + 3] & 0xff) << 24) | ((buffer[index + 2] & 0xff) << 16) |
                        ((buffer[index + 1] & 0xff) << 8) | (buffer[index] & 0xff));
        }
    }

    /**
     * 
     * @param bigEndian
     * @param index
     * @param buffer
     */
    public static short bytesToShort(boolean bigEndian, int index, byte[] buffer){
        if (bigEndian) {
            return (short) (((buffer[index] & 0xff) << 8) | (buffer[index + 1] & 0xff));
        } else {
            return (short) (((buffer[index + 1] & 0xff) << 8) | (buffer[index] & 0xff));
        }
    }

    /**
     * 
     * @param data
     * @param bigEndian
     */
    public static byte[] floatToBytes(float data, boolean bigEndian){
        int tmpInt;

        tmpInt = Float.floatToIntBits(data);
        return intToBytes(tmpInt, bigEndian);
    }

    // public abstract String[] getExtensions();

    /**
     * 
     * @param data
     * @param bigEndian
     */
    public static byte[] intToBytes(int data, boolean bigEndian){
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
        return buffer;
    }

    /**
     * 
     * @param data
     * @param bigEndian
     */
    public static byte[] longToBytes(long data, boolean bigEndian){
        byte[] buffer = new byte[8];
        if (bigEndian) {
            buffer[0] = (byte) (data >>> 56);
            buffer[1] = (byte) (data >>> 48);
            buffer[2] = (byte) (data >>> 40);
            buffer[3] = (byte) (data >>> 32);
            buffer[4] = (byte) (data >>> 24);
            buffer[5] = (byte) (data >>> 16);
            buffer[6] = (byte) (data >>> 8);
            buffer[7] = (byte) (data & 0xff);
        } else {
            buffer[0] = (byte) (data & 0xff);
            buffer[1] = (byte) (data >>> 8);
            buffer[2] = (byte) (data >>> 16);
            buffer[3] = (byte) (data >>> 24);
            buffer[4] = (byte) (data >>> 32);
            buffer[5] = (byte) (data >>> 40);
            buffer[6] = (byte) (data >>> 48);
            buffer[7] = (byte) (data >>> 56);
        }
        return buffer;
    }

    /**
     * 
     * @param data
     * @param bigEndian
     */
    public static byte[] shortToBytes(short data, boolean bigEndian){
        byte[] buffer = new byte[2];
        if (bigEndian) {
            buffer[0] = (byte) (data >>> 8);
            buffer[1] = (byte) (data & 0xff);
        } else {
            buffer[0] = (byte) (data & 0xff);
            buffer[1] = (byte) (data >>> 8);
        }
        return buffer;
    }

    /**
     * 
     * @param data
     * @param index
     */
    public static byte[] stringToBytes(String data, int index){
        return null;
    }

    /**
     * 
     * @param bigEndian
     */
    public final double readDouble(boolean bigEndian) throws IOException{
        byte[] buffer = new byte[8];
        raFile.readFully(buffer);
        
        long tmpLong;
        if (bigEndian) {
            tmpLong = (((buffer[0] & 0xffL) << 56) | ((buffer[1] & 0xffL) << 48) |
                           ((buffer[2] & 0xffL) << 40) | ((buffer[3] & 0xffL) << 32) |
                           ((buffer[4] & 0xffL) << 24) | ((buffer[5] & 0xffL) << 16) |
                           ((buffer[6] & 0xffL) << 8) | (buffer[7] & 0xffL));

        } else {
            tmpLong = (((buffer[7] & 0xffL) << 56) | ((buffer[6] & 0xffL) << 48) |
                           ((buffer[5] & 0xffL) << 40) | ((buffer[4] & 0xffL) << 32) |
                           ((buffer[3] & 0xffL) << 24) | ((buffer[2] & 0xffL) << 16) |
                           ((buffer[1] & 0xffL) << 8) | (buffer[0] & 0xffL));

        }
        return (Double.longBitsToDouble(tmpLong));
    }

    /**
     * 
     * @param bigEndian
     */
    public final int readInt(boolean bigEndian) throws IOException{
        byte[] buffer = new byte[4];
        raFile.readFully(buffer);

        if (bigEndian) {
            return (((buffer[0] & 0xff) << 24) | ((buffer[1] & 0xff) << 16) | ((buffer[2] & 0xff) << 8) |
                        (buffer[3] & 0xff)); // Big Endian
        } else {
            return (((buffer[3] & 0xff) << 24) | ((buffer[2] & 0xff) << 16) | ((buffer[1] & 0xff) << 8) |
                        (buffer[0] & 0xff));
        }
    }

    /**
     * 
     * @param bigEndian
     */
    public final float readFloat(boolean bigEndian) throws IOException{
        byte[] buffer = new byte[4];
        raFile.readFully(buffer);

        int tmpInt;

        if (bigEndian) {
            tmpInt = (((buffer[0] & 0xff) << 24) | ((buffer[1] & 0xff) << 16) |
                          ((buffer[2] & 0xff) << 8) | (buffer[3] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        } else {
            tmpInt = (((buffer[3] & 0xff) << 24) | ((buffer[2] & 0xff) << 16) |
                          ((buffer[1] & 0xff) << 8) | (buffer[0] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        }
    }

    /**
     * 
     * @param bigEndian
     */
    public final long readLong(boolean bigEndian) throws IOException{
        byte[] buffer = new byte[8];

        raFile.readFully(buffer);

        long tmpLong;

        if (bigEndian) {
            tmpLong = (((buffer[0] & 0xffL) << 56) | ((buffer[1] & 0xffL) << 48) |
                           ((buffer[2] & 0xffL) << 40) | ((buffer[3] & 0xffL) << 32) |
                           ((buffer[4] & 0xffL) << 24) | ((buffer[5] & 0xffL) << 16) |
                           ((buffer[6] & 0xffL) << 8) | (buffer[7] & 0xffL));

        } else {
            tmpLong = (((buffer[7] & 0xffL) << 56) | ((buffer[6] & 0xffL) << 48) |
                           ((buffer[5] & 0xffL) << 40) | ((buffer[4] & 0xffL) << 32) |
                           ((buffer[3] & 0xffL) << 24) | ((buffer[2] & 0xffL) << 16) |
                           ((buffer[1] & 0xffL) << 8) | (buffer[0] & 0xffL));

        }
        return (tmpLong);
    }

    /**
     * 
     * @param bigEndian
     */
    public final short readShort(boolean bigEndian) throws IOException{
        short tempShort = 0;
        byte[] buffer = new byte[2];

        raFile.readFully(buffer);

        if (bigEndian) {
            tempShort = (short)(((buffer[0] & 0xff) << 8) | (buffer[1] & 0xff));
        } else {
            tempShort = (short)(((buffer[1] & 0xff) << 8) | (buffer[0] & 0xff));
        }

        if ((tempShort & 0x0080) != 0) {
            tempShort = (short)(tempShort | 0xff00);
        }

        return tempShort;
    }

    /**
     * 
     * @param bigEndian
     */
    public final int readUnsignedShort(boolean bigEndian) throws IOException{
        byte[] buffer = new byte[2];
        raFile.readFully(buffer);

        if (bigEndian) {
            return (((buffer[0] & 0xff) << 8) | (buffer[1] & 0xff));
        } else {
            return (((buffer[1] & 0xff) << 8) | (buffer[0] & 0xff));
        }
    }
    
    /**
     * Reads the length of the characters from the file.
     * 
     * @param length         the length of the string
     * @return               the string read from the file.
     * @throws IOException   throw IOException if I/O error happens
     */
    public final String readString(int length) throws IOException {
        if(length <= 0){
            return null;
        }
        byte[] buffer = new byte[length];
        raFile.readFully(buffer);
        return new String(buffer);
    }
}
