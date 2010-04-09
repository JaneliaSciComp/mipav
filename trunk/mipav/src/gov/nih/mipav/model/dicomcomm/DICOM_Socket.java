package gov.nih.mipav.model.dicomcomm;


import gov.nih.mipav.view.Preferences;

import java.io.*;
import java.net.*;


/**
 * Simple class to setup and the socket and streams.
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
public class DICOM_Socket {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public static final int timeout = 6000000; // 100 minute read timeout -- seems big

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** Flag indicating whether of not the socket is connected. */
    public boolean connected = false;

    /** Input stream object acquired from the socket. */
    public InputStream inStream = null;

    /** Output stream object acquired from the socket. */
    public OutputStream outStream = null;

    /** Socket object. */
    public Socket socket = null;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * DICOMSocket constructor.
     */
    public DICOM_Socket() {}

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Attaches this DICOMSocket to a JAVA socket.
     * 
     * @param socket the socket to attach
     * 
     * @throws DICOM_Exception DOCUMENT ME!
     */
    public void attach(final Socket socket) throws DICOM_Exception {

        close();

        if (socket == null) {
            throw new DICOM_Exception("DICOMSocket.attach( <null> ): attempt to attach to null socket");
        }

        this.socket = socket;

        try {
            socket.setSoTimeout(DICOM_Socket.timeout);

            inStream = socket.getInputStream();
            outStream = socket.getOutputStream();
            connected = true;
        } catch (final Exception e) {
            close();
            throw new DICOM_Exception("DICOMSocket attach( " + socket + " ):" + e);
        }
    }

    /**
     * Closes the socket.
     */
    public void close() {

        if ( (socket != null) && (connected == true)) {

            try {

                if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                    Preferences.debug("**************  DICOMSocket.close: Socket closing.");
                }

                socket.close();
                inStream.close();
                outStream.close();
            } catch (final Exception e) {}
        }

        socket = null;
        inStream = null;
        outStream = null;
        connected = false;
    }

    /**
     * Open (and connect) a socket.
     * 
     * @param ip the ip address to connect to
     * @param port the port number to connect to
     * 
     * @throws DICOM_Exception DOCUMENT ME!
     */
    public void open(final String ip, final int port) throws DICOM_Exception {

        // Close any open sockets first
        close();

        try {
            final Socket socket = new Socket(InetAddress.getByName(ip), port);
            attach(socket);
        } catch (final Exception e) {
            close();
            throw new DICOM_Exception("DICOMSocket.open( " + ip + ", " + port + " ):" + e);
        }
    }

    /**
     * Low level socket read.
     * 
     * @param data buffer to store the data
     * @param count number of bytes to be read
     * 
     * @return DOCUMENT ME!
     * 
     * @throws DICOM_Exception DOCUMENT ME!
     */
    public int readBinary(final byte[] data, final int count) throws DICOM_Exception {
        int actual = 0;

        try {

            // Preferences.debug( "\n" + DICOM_Util.timeStamper() + " **** DICOMSocket.readBinary: data buffer length =
            // " + data.length + " byte count requested = " + count +" **** \n");
            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug("DICOMSocket readBinary( socket = " + socket + ") \n");
            }

            actual = inStream.read(data, 0, count);

            // System.out.println("actual = " + actual);
            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug("\n" + DICOM_Util.timeStamper() + " **** DICOMSocket.readBinary: length" + actual
                        + " **** \n");
            }

            if (actual < 0) {
                close();

                if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                    Preferences.debug(DICOM_Util.timeStamper() + " DICOMSocket.readBinary( Trying to read nBytes = "
                            + count + " ) actual = " + actual + " \n");
                }

                throw new DICOM_Exception("DICOMSocket.readBinary( Trying to read nBytes = " + count + " ) = " + actual);
            }
        } catch (final IOException e) {
            /*
             * if (e instanceof BindException) { System.out.println("BindException "); } if (e instanceof
             * ConnectException) { System.out.println("ConnectionException "); } if (e instanceof
             * NoRouteToHostException) { System.out.println("NoRouteToHostException "); } if (e instanceof
             * PortUnreachableException) { System.out.println("PortUnreachableException "); } System.out.println("Is
             * connectd ? " + socket.isConnected() + " Is closesd ? " + socket.isClosed());
             */

            // Preferences.debug("DICOMSocket readBinary( socket = " + socket + ") \n"); Preferences.debug( "\n" +
            // DICOM_Util.timeStamper() + " **** DICOMSocket.readBinary: length " + actual +" **** \n");
            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug("\n" + DICOM_Util.timeStamper() + " DICOMSocket.readBinary: IOException - " + e
                        + "\n");
            }

            close();
            throw new DICOM_Exception("DICOMSocket.readBinary( " + data + ", " + count + " ): " + e);
        }

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            DICOM_Socket.showDataBuffer(data, 0, actual);
        }

        return (actual);
    }

    /**
     * Low level socket write.
     * 
     * @param data buffer of data send out the port (connection)\
     * @param offset offset
     * @param count number of bytes to be sent
     * 
     * @throws DICOM_Exception DOCUMENT ME!
     */
    public void writeBinary(final byte[] data, final int offset, final int count) throws DICOM_Exception {

        try {

            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug("DICOMSocket writeBinary( socket = " + socket + ") \n");
            }

            outStream.write(data, offset, count);
            outStream.flush();

            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug("\n" + DICOM_Util.timeStamper() + " **** DICOMSocket.writeBinary: ****\n");
            }
        } catch (final Exception e) {
            close();
            throw new DICOM_Exception("DICOMSocket writeBinary( " + data + ", " + offset + ", " + count + " ):" + e);
        }

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            DICOM_Socket.showDataBuffer(data, offset, count);
        }
    }

    /**
     * Simple utility to convert a number to ASCII.
     * 
     * @param num an ASCII number
     * 
     * @return a string with the corresponding ASCII value.
     */
    private static char convertToASCII(final int num) {

        if ( (num < 32) || (num > 126)) {
            return ('_');
        }

        switch (Math.abs(num)) {

            case 32:
                return (' ');

            case 33:
                return ('!');

            case 34:
                return ('@');

            case 35:
                return ('#');

            case 36:
                return ('$');

            case 37:
                return ('%');

            case 38:
                return ('&');

            case 39:
                return (' ');

            case 40:
                return ('(');

            case 41:
                return (')');

            case 42:
                return ('*');

            case 43:
                return ('+');

            case 44:
                return ('`');

            case 45:
                return ('-');

            case 46:
                return ('.');

            case 47:
                return ('/');

            case 48:
                return ('0');

            case 49:
                return ('1');

            case 50:
                return ('2');

            case 51:
                return ('3');

            case 52:
                return ('4');

            case 53:
                return ('5');

            case 54:
                return ('6');

            case 55:
                return ('7');

            case 56:
                return ('8');

            case 57:
                return ('9');

            case 58:
                return (':');

            case 59:
                return (';');

            case 60:
                return ('<');

            case 61:
                return ('=');

            case 62:
                return ('>');

            case 63:
                return ('?');

            case 64:
                return ('@');

            case 65:
                return ('A');

            case 66:
                return ('B');

            case 67:
                return ('C');

            case 68:
                return ('D');

            case 69:
                return ('E');

            case 70:
                return ('F');

            case 71:
                return ('G');

            case 72:
                return ('H');

            case 73:
                return ('I');

            case 74:
                return ('J');

            case 75:
                return ('K');

            case 76:
                return ('L');

            case 77:
                return ('M');

            case 78:
                return ('N');

            case 79:
                return ('O');

            case 80:
                return ('P');

            case 81:
                return ('Q');

            case 82:
                return ('R');

            case 83:
                return ('S');

            case 84:
                return ('T');

            case 85:
                return ('U');

            case 86:
                return ('V');

            case 87:
                return ('W');

            case 88:
                return ('X');

            case 89:
                return ('Y');

            case 90:
                return ('Z');

            case 91:
                return ('[');

            case 92:
                return ('\\');

            case 93:
                return (']');

            case 94:
                return ('^');

            case 95:
                return ('_');

            case 96:
                return ('`');

            case 97:
                return ('a');

            case 98:
                return ('b');

            case 99:
                return ('c');

            case 100:
                return ('d');

            case 101:
                return ('e');

            case 102:
                return ('f');

            case 103:
                return ('g');

            case 104:
                return ('h');

            case 105:
                return ('i');

            case 106:
                return ('j');

            case 107:
                return ('k');

            case 108:
                return ('l');

            case 109:
                return ('m');

            case 110:
                return ('n');

            case 111:
                return ('o');

            case 112:
                return ('p');

            case 113:
                return ('q');

            case 114:
                return ('r');

            case 115:
                return ('s');

            case 116:
                return ('t');

            case 117:
                return ('u');

            case 118:
                return ('v');

            case 119:
                return ('w');

            case 120:
                return ('x');

            case 121:
                return ('y');

            case 122:
                return ('z');

            case 123:
                return ('{');

            case 124:
                return ('|');

            case 125:
                return ('}');

            case 126:
                return ('~');

        }

        return ('.');
    }

    /**
     * Simple utility to convert a number [0:15] to hex.
     * 
     * @param num a number [0:15] to be converted to hex String
     * 
     * @return hex string. If number is outside range then a space is returned.
     */
    private static char convertToHex(final int num) {

        switch (Math.abs(num)) {

            case 0:
                return ('0');

            case 1:
                return ('1');

            case 2:
                return ('2');

            case 3:
                return ('3');

            case 4:
                return ('4');

            case 5:
                return ('5');

            case 6:
                return ('6');

            case 7:
                return ('7');

            case 8:
                return ('8');

            case 9:
                return ('9');

            case 10:
                return ('A');

            case 11:
                return ('B');

            case 12:
                return ('C');

            case 13:
                return ('D');

            case 14:
                return ('E');

            case 15:
                return ('F');
        }

        return (' ');
    }

    /**
     * Utility method to dump hex and ASCII to the MIPAV debug frame.
     * 
     * @param data data buffer to be displayed
     * @param offset starting offset
     * @param count number of bytes to be displayed from data buffer
     */
    private static synchronized void showDataBuffer(final byte[] data, final int offset, final int count) {
        int i, j, k, ih;
        int end = 4096; // 512;

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug(DICOM_Util.timeStamper() + " DICOMSocket.showDataBuffer : Count = " + count + "\n");
        }

        if (count < 512) {
            end = offset + count;
        }

        final char[] ASCIIchar = new char[65];

        for (i = offset, ih = 0; i < end; i++) {
            ASCIIchar[ih++] = DICOM_Socket.convertToHex( (data[i] & 0xff) / 16);
            ASCIIchar[ih++] = DICOM_Socket.convertToHex( (data[i] & 0xff) % 16);
            ASCIIchar[ih++] = ' ';

            if ( ( (i + 1) % 16) == 0) {
                ih = 0;

                // Show ASCII for 16 bytes of HEX
                ASCIIchar[48] = ' ';

                for (j = 15, k = 0; j >= 0; k++, j--) {
                    ASCIIchar[49 + k] = DICOM_Socket.convertToASCII(data[i - j] & 0xff);
                }

                if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                    Preferences.debug(new String(ASCIIchar) + "\n");
                }
            }
        }

        if (ih != 0) {

            for (j = ih; j < 48;) {
                ASCIIchar[j++] = '_';
                ASCIIchar[j++] = '_';
                ASCIIchar[j++] = ' ';
            }
        }

        if (ih != 0) {

            for (i = 48; i < 65; i++) {
                ASCIIchar[i] = ' ';
            }

            for (i = count % 16, j = 0; i > 0; i--, j++) {
                ASCIIchar[49 + j] = DICOM_Socket.convertToASCII(data[count - i] & 0xff);
            }

            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug(new String(ASCIIchar) + "\n");
            }
        }

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug("\n");
        }
    }
}
