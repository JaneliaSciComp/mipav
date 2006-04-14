package gov.nih.mipav.model.file;

import gov.nih.mipav.view.*;

import java.io.*;


/**
*	  FileBase is an abstract class that has many methods that support the reading/writing of files.
*
*	@version    0.9 June 30, 1998
*       @see        FileInfoBase
*
*/
public abstract class FileBase {

    /** Ill defined file type.				*/
    public static final int ERROR        = -1;

    /** Undefined file type.				*/
    public static final int UNDEFINED    = 0;

    /** Not presently implemented.			*/
    public static final int MIPAV        = 1;

    /** RAW image data, no header.			*/
    public static final int RAW          = 2;

     /** RAW MULTIFLE image data, no header. */
    public static final int RAW_MULTIFILE = 25;

    /** TIFF file; tagged header			*/
    public static final int TIFF         = 3;

    /** VOI file, used to read VOIs.		*/
    public static final int VOI_FILE     = 4;

    /** Analyze format (Mayo).				*/
    public static final int ANALYZE      = 5;

    /** Digital Imaging and COmmunications in Medicine file type.
    	Fully implemented versions 2 & 3.	*/
    public static final int DICOM        = 6;

    /** Medvision file type.				*/
    public static final int MEDVISION    = 7;

    /** Benes Trus special file type.		*/
    public static final int MAP          = 8;

    /** Java Image Manangement Interface file type. */
    public static final int JIMI         = 9;

    /** Multiple files of TIFF images.		*/
    public static final int TIFF_MULTIFILE = 10;

    /** MINC file type.  MINC is a medical imaging oriented extension
    	of the NetCDF file format. NetCDF stands for `Network Common Data Form'.  */
    public static final int MINC         = 11;

    /** AVI file type.  Windows Media.*/
    public static final int AVI          = 12;

    /** Multiple files of type analyze.		*/
    public static final int ANALYZE_MULTIFILE = 13;

    /** Quicktime file type.			*/
    public static final int QT           = 14;

    /** Cheshire file type (a kind of Analyze).*/
    public static final int CHESHIRE     = 15;

    /** Cheshire overlay file type.  Contains VOIs.	*/
    public static final int CHESHIRE_OVERLAY = 16;

    /** AFNI file type.	*/
    public static final int AFNI = 17;

    /** FITS file type. */
    public static final int FITS = 18;

    /** MetaMorph Stack (STK) file type. */
    public static final int STK = 19;

    /** Siemens MAGNETOM VISION */
    public static final int MAGNETOM_VISION = 20;

    /** GE Genesis 5X and LX */
    public static final int GE_GENESIS = 21;

    /** MRC file format used by IMOD */
    public static final int MRC = 22;

    /** Interfile file format used in Nuclear Medicine */
    public static final int INTERFILE = 23;


    /** Micro CT format for small animal imaging */
    public static final int MICRO_CAT = 24;

    /** Used by the Zeiss LSM 510 Dataserver */
    public static final int LSM = 26;

    /** Used by the Zeiss LSM 510 Dataserver */
    public static final int LSM_MULTIFILE = 37;

    /** Used by the Bio-Rad Pic format */
    public static final int BIORAD = 27;

    /** Used by FreeSurfer software */
    public static final int COR = 28;

    /** Bruker file format */
    public static final int BRUKER = 29;

    /** MIPAV XML file format */
    public static final int XML     = 30;

    /** MIPAV XML file format */
    public static final int XML_MULTIFILE = 31;

    /** SPM file format */
    public static final int SPM = 32;

    /** MIPAV project format */
    public static final int PROJECT = 33;

    /** NIFTI format */
    public static final int NIFTI = 34;

    /** NIFTI multi-file format */
    public static final int NIFTI_MULTIFILE = 35;

    /* Image Cytometry Standard */
    public static final int ICS = 36;

    /* Optical coherence tomography */
    public static final int TMG = 38;

    /* Washington University OSM dataset structure */
    public static final int OSM = 39;

    /** MIPAV Surface XML file format */
    public static final int SURFACE_XML     = 40;

    /** Gatan's Digital Micrograph version 3 file format */
    public static final int DM3 = 41;

    /** Arrary of strings describing the file formats */
    private static String[] fileFormatStr = { "Undefined", "MIPAV", "Raw", "Tiff", "VOI",
                                             "Analyze", "DICOM", "Medvision", "Map", "JIMI", "Tiff multifile",
                                             "MINC", "Avi", "Analyze multifile", "QT", "Chesire",
                                             "Chesire Overlay", "Afni", "FITS", "STK", "Magnetom Vision",
                                             "GE Genesis", "MRC", "Interfile", "Raw multifile",
                                             "Micro CAT", "LSM", "Bio-Rad", "COR", "BRUKER", "XML",
                                             "XML multifile", "SPM", "Project", "NIFTI",
                                             "NIFTI multifile", "ICS", "LSM multifile", "TMG",
                                             "OSM", "Surface XML", "DM3"};

    /** Byte order.  Rightmost byte is most significant. */
    public static final boolean LITTLE_ENDIAN   = false;

    /** Byte order.  Leftmost byte is most significant. */
    public static final boolean BIG_ENDIAN      = true;

    /** Read only access.	*/
    public static final int READ                = 0;

    /** Read-write access.	*/
    public static final int READ_WRITE          = 1;

    /** Two byte array used to read/write in data so that one
    *   doesn't't need to be allocated with each read/write
    */
    protected byte[] byteBuffer2 = new byte[2];

    /** Four byte array used to read/write in data so that one
    *   doesn't need to be allocated with each read/write
    */
    protected byte[] byteBuffer4 = new byte[4];

    /** Eight byte array used to read/write in data so that they
    *   don't need to be allocated with each read/write
    */
    protected byte[] byteBuffer8 = new byte[8];

    /** Progress bar to show when reading in image file. */
    protected ProgressBarInterface      progressBar;

    /** Flag indicating if the progress bar should be shown. */
    protected boolean               pBarVisible = true;

    /** Pointer to file to read or write from.	*/
    protected RandomAccessFile raFile;


    /**
    *   Empty constructor.
    */
    public FileBase() {
    }

    /**
    *   Prepares this class for cleanup
    */
    public void finalize() {
        byteBuffer2    = null;
        byteBuffer4    = null;
        byteBuffer8    = null;
        progressBar    = null;
        if (raFile != null) {
            try {
                raFile.close();
            }
            catch(IOException ioe) {
                // Do nothing
            }
        }
        raFile         = null;
    }

    /**
    *   Returns flag that indicates that the progressBar is visible.
    *	@return		<code>true</code> if progress bar is visible, <code>false</code> if not visible.
    */
    public boolean  isProgressBarVisible()   { return pBarVisible;}

    /**
    *   Sets whether or not the progress bar should be visible.
    *   @param flag    <code>true</code> if should be visible, <code>false</code> if not visible.
    */
    public void     setProgressBarVisible(boolean flag)   {
        pBarVisible = flag;
    }

    /**
    *   Returns the string for a particular file format.
    *   @param format  int representing the file format (see the static definitions)
    *   @return        string representing the file format
    */
    public static String getFileFormatStr(int format) {

        if (format == FileBase.ERROR)
            return "Error";

        try {
           return FileBase.fileFormatStr[format];
        }
        catch (ArrayIndexOutOfBoundsException ae) { }

        return "";

    } // end getFileFormatStr()

    /**
    *   Reads two unsigned bytes from file.
    *   @param endianess        <code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
    *   @return                 The value of unsigned short read from the file returned as an int.
    *   @exception IOException  if there is an error reading the file
    */
    public final int getUnsignedShort(boolean endianess) throws IOException {
        raFile.readFully(byteBuffer2);

        if (endianess == BIG_ENDIAN) {
            return ( ((byteBuffer2[0]& 0xff) << 8) | (byteBuffer2[1]& 0xff) );  // Big Endian
        }
        else {
            return ( ((byteBuffer2[1]& 0xff) << 8) | (byteBuffer2[0]& 0xff) );  // Little Endian
        }
    }

    /**
    *   Reads two byte signed short from file.
    *   @param endianess        <code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
    *   @return                 The value of signed short read from the file returned as an int.
    *   @exception IOException  if there is an error reading the file
    */
    public final int getSignedShort(boolean endianess) throws IOException {
         int b3 = 0;
         raFile.readFully(byteBuffer2);

        if (endianess == BIG_ENDIAN) {
            b3 = ((byteBuffer2[0]& 0xff) << 8) | (byteBuffer2[1]& 0xff);
        }
        else {
            b3 = ((byteBuffer2[1]& 0xff) << 8) | (byteBuffer2[0]& 0xff);
        }

        if ((b3 & 0x0080)!= 0)  b3 = b3 | 0xff00;

        return b3;
    }


    /**
    *   Reads four signed bytes from file.
    *   @param endianess        <code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
    *   @return                 The value of the integer read from the file.
    *   @exception IOException  if there is an error reading the file
    */
    public final int getInt(boolean endianess) throws IOException {

        raFile.readFully(byteBuffer4);

        if (endianess == BIG_ENDIAN) {
            return( ((byteBuffer4[0]& 0xff) << 24) | ((byteBuffer4[1]& 0xff) << 16) |
                    ((byteBuffer4[2]& 0xff) << 8)  |  (byteBuffer4[3]& 0xff));  // Big Endian
        }
        else {
            return( ((byteBuffer4[3]& 0xff) << 24) | ((byteBuffer4[2]& 0xff) << 16) |
                    ((byteBuffer4[1]& 0xff) << 8)  |  (byteBuffer4[0]& 0xff));
        }
    }

    /**
    *   Reads four unsigned bytes from file.
    *   @param endianess        <code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
    *   @return                 The value of the integer read from the file.
    *   @exception IOException  if there is an error reading the file
    */
    public final long getUInt(boolean endianess) throws IOException {

        raFile.readFully(byteBuffer4);

        if (endianess == BIG_ENDIAN) {
            return( ((byteBuffer4[0]& 0xffL) << 24) | ((byteBuffer4[1]& 0xffL) << 16) |
                    ((byteBuffer4[2]& 0xffL) << 8)  |  (byteBuffer4[3]& 0xffL));  // Big Endian
        }
        else {
            return( ((byteBuffer4[3]& 0xffL) << 24) | ((byteBuffer4[2]& 0xffL) << 16) |
                    ((byteBuffer4[1]& 0xffL) << 8)  |  (byteBuffer4[0]& 0xffL));
        }
    }


    /**
    *   Reads four unsigned bytes from file.
    *   @param endianess        <code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
    *   @return                 The value of the float read from the file.
    *   @exception IOException  if there is an error reading the file
    */
    public final float getFloat(boolean endianess) throws IOException {
        raFile.readFully(byteBuffer4);
        int     tmpInt;

        if (endianess == BIG_ENDIAN) {
            tmpInt = ( ((byteBuffer4[0]& 0xff) << 24) | ((byteBuffer4[1]& 0xff) << 16) |
                       ((byteBuffer4[2]& 0xff) << 8)  |  (byteBuffer4[3]& 0xff) );
            return(Float.intBitsToFloat(tmpInt));
        }
        else {
            tmpInt = ( ((byteBuffer4[3]& 0xff) << 24) | ((byteBuffer4[2]& 0xff) << 16) |
                       ((byteBuffer4[1]& 0xff) << 8)  |  (byteBuffer4[0]& 0xff));
            return(Float.intBitsToFloat(tmpInt));
        }
    }

    /**
    *   Reads eight unsigned bytes from file
    *   @param endianess        <code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
    *   @return                 The value of the double read from the file.
    *   @exception IOException  if there is an error reading the file
    */
    public final double getDouble(boolean endianess) throws IOException {
        raFile.readFully(byteBuffer8);
        long tmpLong;

        if (endianess == BIG_ENDIAN) {
            tmpLong =( ((byteBuffer8[0] & 0xffL) << 56) | ((byteBuffer8[1] & 0xffL) << 48) |
                       ((byteBuffer8[2] & 0xffL) << 40) | ((byteBuffer8[3] & 0xffL) << 32) |
                       ((byteBuffer8[4] & 0xffL) << 24) | ((byteBuffer8[5] & 0xffL) << 16) |
                       ((byteBuffer8[6] & 0xffL) << 8) |   (byteBuffer8[7] & 0xffL) );
            return(Double.longBitsToDouble(tmpLong));
        }
        else {
            tmpLong =( ((byteBuffer8[7] & 0xffL) << 56) | ((byteBuffer8[6] & 0xffL) << 48) |
                       ((byteBuffer8[5] & 0xffL) << 40) | ((byteBuffer8[4] & 0xffL) << 32) |
                       ((byteBuffer8[3] & 0xffL) << 24) | ((byteBuffer8[2] & 0xffL) << 16) |
                       ((byteBuffer8[1] & 0xffL) << 8) |   (byteBuffer8[0] & 0xffL) );
            return(Double.longBitsToDouble(tmpLong));
        }
    }

    /**
    *   Reads eight unsigned bytes from file.
    *   @param endianess        <code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
    *   @return                 The value of the long read from the file.
    *   @exception IOException  if there is an error reading the file
    */
    public final long getLong(boolean endianess) throws IOException {
        raFile.readFully(byteBuffer8);
        long tmpLong;

        if (endianess == BIG_ENDIAN) {
            tmpLong =( ((byteBuffer8[0] & 0xffL) << 56) | ((byteBuffer8[1] & 0xffL) << 48) |
                       ((byteBuffer8[2] & 0xffL) << 40) | ((byteBuffer8[3] & 0xffL) << 32) |
                       ((byteBuffer8[4] & 0xffL) << 24) | ((byteBuffer8[5] & 0xffL) << 16) |
                       ((byteBuffer8[6] & 0xffL) << 8) |   (byteBuffer8[7] & 0xffL) );
            return(tmpLong);
        }
        else {
            tmpLong =( ((byteBuffer8[7] & 0xffL) << 56) | ((byteBuffer8[6] & 0xffL) << 48) |
                       ((byteBuffer8[5] & 0xffL) << 40) | ((byteBuffer8[4] & 0xffL) << 32) |
                       ((byteBuffer8[3] & 0xffL) << 24) | ((byteBuffer8[2] & 0xffL) << 16) |
                       ((byteBuffer8[1] & 0xffL) << 8) |   (byteBuffer8[0] & 0xffL) );
            return(tmpLong);
        }
    }


    /**
    *  Reads a string from a file of given <code>length</code>.
    *  @param length           Number of bytes that form the string.
    *  @return                 The string read from the file.
    *  @exception IOException  if there is an error reading the file
    */
    public final String getString(int length) throws IOException {
	    if (length <= 0) return new String("");
	    byte[] b = new byte[length];
	    raFile.readFully(b);
	    return new String(b);
    }


    /**
    *  Converts byte data to short data
    *  @param buffer      Array of byte data.
    *  @param index       Index into array data.
    *  @param endianess   <code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
    *  @return            Short value extracted from byte array.
    */
    public final short getBufferShort(byte buffer[], int index, boolean endianess) {

        if (endianess == BIG_ENDIAN) {
            return (short)(((buffer[index]   & 0xff) << 8)   | (buffer[index+1]   & 0xff));
        }
        else {
            return (short)(((buffer[index+1]   & 0xff) << 8) | (buffer[index]   & 0xff));
        }
    }


    /**
    *  Converts byte data to int data.
    *  @param buffer      Array of byte data.
    *  @param index       Index into array data.
    *  @param endianess   <code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
    *  @return            Integer value extracted from byte array.
    */
    public final int getBufferInt(byte buffer[], int index, boolean endianess) {

        if (endianess == BIG_ENDIAN) {
            return ( ((buffer[index]   & 0xff) << 24) | ((buffer[index+1] & 0xff) << 16) |
                     ((buffer[index+2] & 0xff) << 8)  |  (buffer[index+3] & 0xff) );
        }
        else {
            return ( ((buffer[index+3] & 0xff) << 24) | ((buffer[index+2] & 0xff) << 16) |
                     ((buffer[index+1] & 0xff) << 8)  |  (buffer[index]   & 0xff) );
        }
    }

    /**
    *  Converts byte data to float data.
    *  @param buffer      Array of byte data.
    *  @param index       Index into array data.
    *  @param endianess   <code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
    *  @return            Float value extracted from byte array.
    */
    public final float getBufferFloat(byte buffer[], int index, boolean endianess) {
        int     tmpInt;

        if (endianess == BIG_ENDIAN) {
            tmpInt =(((buffer[index]   & 0xff) << 24) | ((buffer[index+1] & 0xff) << 16) |
                     ((buffer[index+2] & 0xff) << 8)  |  (buffer[index+3] & 0xff));
            return(Float.intBitsToFloat(tmpInt));
        }
        else {
            tmpInt = (((buffer[index+3] & 0xff) << 24) | ((buffer[index+2] & 0xff) << 16) |
                      ((buffer[index+1] & 0xff) << 8)  |  (buffer[index]   & 0xff));
            return(Float.intBitsToFloat(tmpInt));
        }
    }

    /**
    *  Converts byte data to unsigned int.
    *  @param buffer     Buffer of signed bytes.
    *  @param index      Index points to location in buffer.
    *  @return           Integer converted from as "signed byte".
    */
    public final int getUnsignedByte(byte buffer[], int index){
        return(buffer[index] & 0xff);
    }

    /**
    *   Writes a short as two bytes to a file.
    *   @param data             Data to be written to file.
    *   @param endianess		<code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
    *   @exception IOException  if there is an error writing the file
    */
    public final void writeShort(short data, boolean endianess) throws IOException {
        if ( endianess == BIG_ENDIAN) {
            byteBuffer2[0] = (byte)(data >>> 8);
            byteBuffer2[1] = (byte)(data        & 0xff);
        }
        else {
            byteBuffer2[0] = (byte)(data        & 0xff);
            byteBuffer2[1] = (byte)(data >>> 8);
        }
        raFile.write(byteBuffer2);
    }

    /**
    *   Writes an int as four bytes to a file.
    *   @param data             Data to be written to file.
    *   @param endianess		<code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
    *   @exception IOException  if there is an error writing the file
    */
    public final void writeInt(int data, boolean endianess) throws IOException {

        if ( endianess == BIG_ENDIAN) {
            byteBuffer4[0] = (byte)(data >>> 24);
            byteBuffer4[1] = (byte)(data >>> 16);
            byteBuffer4[2] = (byte)(data >>> 8);
            byteBuffer4[3] = (byte)(data  & 0xff);
        }
        else {
            byteBuffer4[0] = (byte)(data  & 0xff);
            byteBuffer4[1] = (byte)(data >>> 8);
            byteBuffer4[2] = (byte)(data >>> 16);
            byteBuffer4[3] = (byte)(data >>> 24);
        }
        raFile.write(byteBuffer4);
    }

    /**
    *   Writes a float as four bytes to a file.
    *   @param data             Data to be written to file.
    *   @param endianess		<code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
    *   @exception IOException  if there is an error writing the file
    */
    public final void writeFloat(float data, boolean endianess) throws IOException {
        int   tmpInt;

        tmpInt = Float.floatToIntBits(data);
        writeInt(tmpInt,endianess);
    }

    /**
    *   Writes a double as eight bytes to a file.
    *   @param data             Data to be written to file.
    *   @param endianess		<code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
    *   @exception IOException  if there is an error writing the file
    */
    public final void writeDouble(double data, boolean endianess) throws IOException {

        long   tmpLong;

        tmpLong = Double.doubleToLongBits(data);
        writeLong(tmpLong,endianess);
    }

    /**
    *   Writes a long as eight bytes to a file.
    *   @param data             Data to be written to file.
    *   @param endianess		<code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
    *   @exception IOException  if there is an error writing the file
    */
    public final void writeLong(long data, boolean endianess) throws IOException {

        if ( endianess == BIG_ENDIAN) {
            byteBuffer8[0] = (byte)(data >>> 56);
            byteBuffer8[1] = (byte)(data >>> 48);
            byteBuffer8[2] = (byte)(data >>> 40);
            byteBuffer8[3] = (byte)(data >>> 32);
            byteBuffer8[4] = (byte)(data >>> 24);
            byteBuffer8[5] = (byte)(data >>> 16);
            byteBuffer8[6] = (byte)(data >>>  8);
            byteBuffer8[7] = (byte)( data  & 0xff);
        }
        else {
            byteBuffer8[0] = (byte)(data  & 0xff);
            byteBuffer8[1] = (byte)(data >>> 8);
            byteBuffer8[2] = (byte)(data >>> 16);
            byteBuffer8[3] = (byte)(data >>> 24);
            byteBuffer8[4] = (byte)(data >>> 32);
            byteBuffer8[5] = (byte)(data >>> 40);
            byteBuffer8[6] = (byte)(data >>> 48);
            byteBuffer8[7] = (byte)(data >>> 56);
        }
        raFile.write(byteBuffer8);
    }
    /**
    *   Sets byte buffer with int.
    *   @param buffer  Byte buffers where data is to be stored.
    *   @param str     String containing integer data which is broken down
    *                  in bytes and stored in the byte buffer.
    *   @param i       Index into byte buffer.
    */
    public final void setBufferString(byte buffer[], String str, int i){

        byte tmpBuffer[];

        tmpBuffer   = str.getBytes();

        for(int c = 0; c < tmpBuffer.length; c++){
            buffer[i+c] = tmpBuffer[c];
        }
    }

    /**
    *   Sets byte buffer with int.
    *   @param buffer     Byte buffers where data is to be stored.
    *   @param data       Float data is broken down in bytes and stored in
    *                     the byte buffer.
    *   @param i          Index into byte buffer.
    *   @param endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
    */
    public final void setBufferFloat(byte buffer[], float data, int i, boolean endianess) {
        int   tmpInt;

        tmpInt = Float.floatToIntBits(data);
        setBufferInt(buffer, tmpInt, i, endianess);
    }


    /**
    *   Sets byte buffer with int.
    *   @param buffer     Byte buffers where data is to be stored.
    *   @param data       Short data is broken down in bytes and stored in
    *                     the byte buffer.
    *   @param i          Index into byte buffer.
    *   @param endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
    */
    public final void setBufferShort(byte buffer[], short data, int i, boolean endianess) {

        if ( endianess == BIG_ENDIAN) {
            buffer[i]   = (byte)(data >>> 8);
            buffer[i+1] = (byte)(data        & 0xff);
        }
        else {
            buffer[i]   = (byte)(data        & 0xff);
            buffer[i+1] = (byte)(data >>> 8);
        }
    }

    /**
    *   Sets byte buffer with int.
    *   @param buffer     Byte buffers where data is to be stored.
    *   @param data       Integer data is broken down in bytes and stored in
    *                     the byte buffer.
    *   @param i          Index into byte buffer.
    *   @param endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
    */
    public final void setBufferInt(byte buffer[], int data, int i, boolean endianess) {

        if ( endianess == BIG_ENDIAN) {
            buffer[i]   = (byte)(data >>> 24);
            buffer[i+1] = (byte)(data >>> 16);
            buffer[i+2] = (byte)(data >>> 8);
            buffer[i+3] = (byte) (data        & 0xff);
        }
        else {
            buffer[i]   = (byte)(data        & 0xff);
            buffer[i+1] = (byte)(data >>> 8);
            buffer[i+2] = (byte)(data >>> 16);
            buffer[i+3] = (byte)(data >>> 24);
        }
    }

    /**
    *   Sets byte buffer with long.
    *   @param buffer     Byte buffers where data is to be stored.
    *   @param data       Long data is broken down in bytes and stored in
    *                     the byte buffer.
    *   @param i          Index into byte buffer.
    *   @param endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little endian.
    */
    public final void setBufferLong (byte buffer[], long data, int i, boolean endianess) {

        if ( endianess == BIG_ENDIAN) {
            buffer[i]   = (byte)(data >>> 56);
            buffer[i+1] = (byte)(data >>> 48);
            buffer[i+2] = (byte)(data >>> 40);
            buffer[i+3] = (byte)(data >>> 32);
            buffer[i+4] = (byte)(data >>> 24);
            buffer[i+5] = (byte)(data >>> 16);
            buffer[i+6] = (byte)(data >>>  8);
            buffer[i+7] = (byte)( data  & 0xff);
        }
        else {
            buffer[i]   = (byte)(data  & 0xff);
            buffer[i+1] = (byte)(data >>> 8);
            buffer[i+2] = (byte)(data >>> 16);
            buffer[i+3] = (byte)(data >>> 24);
            buffer[i+4] = (byte)(data >>> 32);
            buffer[i+5] = (byte)(data >>> 40);
            buffer[i+6] = (byte)(data >>> 48);
            buffer[i+7] = (byte)(data >>> 56);
        }
    }

}
