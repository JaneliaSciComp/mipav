package gov.nih.mipav.model.dicomcomm;


import java.io.*;


/**
 * DICOM communication package reads the data from the socket and saves the data to a file stream. When pushing, data is
 * read (streamed) from the file and push out to the socket.
 */
public class DICOM_FileIO extends DICOM_Comms {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** File input stream object. */
    public FileInputStream inFileStream = null;

    /** File output stream object. */
    public FileOutputStream outFileStream = null;

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes both the input and output streams if not null.
     */
    public void close() {

        if (inFileStream != null) {

            try {
                inFileStream.close();
            } catch (Exception e) { }
        }

        if (outFileStream != null) {

            try {
                outFileStream.close();
            } catch (Exception e) { }
        }
    }
    
    public void finalize(){
        if (outFileStream != null){
            try {
                outFileStream.close();
            } catch (Exception e) { }
        }
        outFileStream = null;
        
        if (inFileStream != null){
            try {
                inFileStream.close();
            } catch (Exception e) { }
        }
        inFileStream = null;
        
        super.finalize();
    }

    /**
     * Open file input stream for file.
     *
     * @param   fileName  name of the file to open
     *
     * @return  true if sucessfully open file for reading
     */
    public boolean openForRead(String fileName) {

        try {
            inFileStream = new FileInputStream(fileName);
        } catch (Exception e) {
            return (false);
        }

        return (true);
    }

    /**
     * Open file output stream for the file.
     *
     * @param   fileName  name of the file to open for writing
     *
     * @return  true if sucessfully opened file for writing
     */
    public boolean openForWrite(String fileName) {

        try {
            outFileStream = new FileOutputStream(fileName);
        } catch (Exception e) {
            return (false);
        }

        return (true);
    }

    /**
     * Read file stream data into buffer.
     *
     * @param   data   buffer in which to store data from file stream
     * @param   count  number of bytes to read into buffer
     *
     * @return  The actual number of bytes read.
     *
     * @throws  DICOM_Exception  Throws an exception if there was a problem readiing in the data to the port.
     */
    public int readBinary(byte[] data, int count) throws DICOM_Exception {
        int actualNumOfByteRead = 0;

        try {
            actualNumOfByteRead = inFileStream.read(data, 0, count);
        } catch (Exception e) {
            close();
            throw new DICOM_Exception("DICOM_FileIO.readBinary( " + data + ", " + count + " ): " + e);
        }

        if (actualNumOfByteRead < 0) {
            close();
            throw new DICOM_Exception("DICOM_FileIO.readBinary( " + data + ", " + count + " ) = " + actualNumOfByteRead);
        }

        return (actualNumOfByteRead);
    }


    /**
     * Writes bytes from the data buffer into the file stream.
     *
     * @param   data   buffer which holds the info to output
     * @param   count  number of bytes to output
     *
     * @throws  DICOM_Exception  Throws an exception if there was a problem outputting the data to the port.
     */
    public void sendBinary(byte[] data, int count) throws DICOM_Exception {

        try {
            outFileStream.write(data, 0, count);
        } catch (Exception e) {
            close();
            throw new DICOM_Exception("DICOM_FileIO.writeBinary( " + data + ", " + count + " ):" + e);
        }
    }

}
