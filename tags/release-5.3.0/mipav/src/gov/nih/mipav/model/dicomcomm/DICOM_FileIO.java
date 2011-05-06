package gov.nih.mipav.model.dicomcomm;


import java.io.*;


/**
 * DICOM communication package reads the data from the socket and saves the data to a file stream. When pushing, data is
 * read (streamed) from the file and push out to the socket.
 * 
 * <hr>
 * 
 * This DICOM communication package was originally based on the Java Dicom Package, whose license is below:
 * 
 * <pre>
 * Java Dicom Package (com.zmed.dicom)
 * 
 *  Copyright (c) 1996-1997 Z Medical Imaging Systems, Inc.
 * 
 *  This software is provided, as is, for non-commercial educational
 *  purposes only.   Use or incorporation of this software or derivative
 *  works in commercial applications requires written consent from
 *  Z Medical Imaging Systems, Inc.
 * 
 *  Z MEDICAL IMAGING SYSTEMS MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT
 *  THE SUITABILITY OF THE SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING
 *  BUT NOT LIMITED TO THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS
 *  FOR A PARTICULAR PURPOSE, NON-INFRINGEMENT, OR CONFORMANCE TO ANY
 *  SPECIFICATION OR STANDARD.  Z MEDICAL IMAGING SYSTEMS SHALL NOT BE
 *  LIABLE FOR ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING OR
 *  MODIFYING THIS SOFTWARE OR ITS DERIVATIVES.
 * 
 *  =============================================================================
 * 
 *  This software package is implemented similarly to the UC Davis public
 *  domain C++ DICOM implementation which contains the following copyright
 *  notice:
 * 
 *  Copyright (C) 1995, University of California, Davis
 * 
 *  THIS SOFTWARE IS MADE AVAILABLE, AS IS, AND THE UNIVERSITY
 *  OF CALIFORNIA DOES NOT MAKE ANY WARRANTY ABOUT THE SOFTWARE, ITS
 *  PERFORMANCE, ITS MERCHANTABILITY OR FITNESS FOR ANY PARTICULAR
 *  USE, FREEDOM FROM ANY COMPUTER DISEASES OR ITS CONFORMITY TO ANY
 *  SPECIFICATION. THE ENTIRE RISK AS TO QUALITY AND PERFORMANCE OF
 *  THE SOFTWARE IS WITH THE USER.
 * 
 *  Copyright of the software and supporting documentation is
 *  owned by the University of California, and free access
 *  is hereby granted as a license to use this software, copy this
 *  software and prepare derivative works based upon this software.
 *  However, any distribution of this software source code or
 *  supporting documentation or derivative works (source code and
 *  supporting documentation) must include this copyright notice.
 * 
 *  The UC Davis C++ source code is publicly available from the following
 *  anonymous ftp site:
 * 
 *  ftp://imrad.ucdmc.ucdavis.edu/pub/dicom/UCDMC/
 * </pre>
 */
public class DICOM_FileIO extends DICOM_Comms {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** File input stream object. */
    public FileInputStream inFileStream = null;

    /** File output stream object. */
    public FileOutputStream outFileStream = null;

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Closes both the input and output streams if not null.
     */
    public void close() {

        if (inFileStream != null) {

            try {
                inFileStream.close();
            } catch (final Exception e) {}
        }

        if (outFileStream != null) {

            try {
                outFileStream.close();
            } catch (final Exception e) {}
        }
    }

    public void finalize() {
        if (outFileStream != null) {
            try {
                outFileStream.close();
            } catch (final Exception e) {}
        }
        outFileStream = null;

        if (inFileStream != null) {
            try {
                inFileStream.close();
            } catch (final Exception e) {}
        }
        inFileStream = null;

        super.finalize();
    }

    /**
     * Open file input stream for file.
     * 
     * @param fileName name of the file to open
     * 
     * @return true if sucessfully open file for reading
     */
    public boolean openForRead(final String fileName) {

        try {
            inFileStream = new FileInputStream(fileName);
        } catch (final Exception e) {
            return (false);
        }

        return (true);
    }

    /**
     * Open file output stream for the file.
     * 
     * @param fileName name of the file to open for writing
     * 
     * @return true if sucessfully opened file for writing
     */
    public boolean openForWrite(final String fileName) {

        try {
            outFileStream = new FileOutputStream(fileName);
        } catch (final Exception e) {
            return (false);
        }

        return (true);
    }

    /**
     * Read file stream data into buffer.
     * 
     * @param data buffer in which to store data from file stream
     * @param count number of bytes to read into buffer
     * 
     * @return The actual number of bytes read.
     * 
     * @throws DICOM_Exception Throws an exception if there was a problem readiing in the data to the port.
     */
    public int readBinary(final byte[] data, final int count) throws DICOM_Exception {
        int actualNumOfByteRead = 0;

        try {
            actualNumOfByteRead = inFileStream.read(data, 0, count);
        } catch (final Exception e) {
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
     * @param data buffer which holds the info to output
     * @param count number of bytes to output
     * 
     * @throws DICOM_Exception Throws an exception if there was a problem outputting the data to the port.
     */
    public void sendBinary(final byte[] data, final int count) throws DICOM_Exception {

        try {
            outFileStream.write(data, 0, count);
        } catch (final Exception e) {
            close();
            throw new DICOM_Exception("DICOM_FileIO.writeBinary( " + data + ", " + count + " ):" + e);
        }
    }

}
