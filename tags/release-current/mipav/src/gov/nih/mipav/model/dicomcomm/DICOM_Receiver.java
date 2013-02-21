package gov.nih.mipav.model.dicomcomm;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.util.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogText;

import java.io.*;
import java.net.*;
import java.util.Vector;

import javax.swing.JFrame;


/**
 * This is the DICOM server class that hangs a listener on a given port for incoming image store requests from a remote
 * DICOM client.
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
public class DICOM_Receiver extends DICOM_PDUService implements Runnable, Observable {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Used to number XRay image file names so that they are numbered differently. */
    private static int crNum = 1;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** Socket used to receive the data. */
    private Socket acceptedSocket = null;

    /** Flag to indicate if the receive process should be cancelled. Cancel if true. */
    private boolean cancelled = false;

    /** DOCUMENT ME! */
    private boolean changed = false;

    /** DOCUMENT ME! */
    private String defaultStorageDir;

    /** A semicolon delimited list of the storage properties to use for this receiver */
    private final String storageProperty;

    /** DOCUMENT ME! */
    private final Vector fileNameList = new Vector();

    /** DOCUMENT ME! */
    private final Vector observerList = new Vector();

    /** Port number used to accept data. */
    private int port = 3100;

    /** DOCUMENT ME! */
    private ByteBuffer pre_and_fullData = null;

    /** DICOM part 10 preample buffer. */
    private byte[] preambleBuffer;

    /** Socket manager. */
    private ServerSocket recSocket = null;

    /** Thread object manager for running receiver. */
    private RunnerThread runner;

    /** Reference to the DICOM verification object. */
    private final DICOM_Verification verification;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Constructor that starts the thread.
     */
    public DICOM_Receiver() {
        this(Preferences.getProperty(Preferences.getDefaultStorageKey()));
    }

    public DICOM_Receiver(final String storageKey) {
        super();
        storageProperty = storageKey;
        verification = new DICOM_Verification(null, null);
        start(Thread.MIN_PRIORITY);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * The implementation of the Observable interface.
     * 
     * @param o DOCUMENT ME!
     */
    /**
     * @see Observable#addObserver(Observer)
     */
    public void addObserver(final Observer o) {

        if ( !observerList.contains(o)) {
            observerList.add(o);
        }
    }

    /**
     * @see Observable#clearChanged()
     */
    public void clearChanged() {
        changed = false;
    }

    /**
     * @see Observable#countObservers()
     */
    public int countObservers() {
        return observerList.size();
    }

    /**
     * @see Observable#deleteObserver(Observer)
     */
    public void deleteObserver(final Observer o) {

        if ( (o != null) && observerList.contains(o)) {
            observerList.remove(o);
        }
    }

    /**
     * @see Observable#deleteObservers()
     */
    public void deleteObservers() {
        observerList.clear();
        clearChanged();
    }

    /**
     * DOCUMENT ME!
     */
    public void finalize() {

        if (pre_and_fullData != null) {
            pre_and_fullData.finalize();
        }

        preambleBuffer = null;
        super.finalize();
    }

    /**
     * Returns the default storage directory for the received dicom files.
     * 
     * @return the default storage directory for the received dicom files.
     */
    public String getDefaultStorageDir() {
        return defaultStorageDir;
    }

    /**
     * @see Observable#hasChanged()
     */
    public boolean hasChanged() {
        return changed;
    }

    /**
     * Checks to see if the thread controlling the receivers execution is alive.
     * 
     * @return flag indicating whether or not the thread is alive
     */
    public boolean isAlive() {

        return runner.isAlive();
    }

    /**
     * Starts the DICOM image receiver.
     */
    public void mipavReceiver() {

        // accept storage SOP UIDs listed in pdu.addAllSupportedAbstractSyntaxes().
        addAllSupportedAbstractSyntaxes();

        // synchronous routine for handling connection requests
        while (runner.keepGoing()) {

            try {

                try {
                    recSocket.setSoTimeout(1500);
                    acceptedSocket = recSocket.accept();
                } catch (final IOException error) {
                    // if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) Preferences.debug( "DICOMReceiver - error
                    // during socket acceptance " + error + "\n");
                }

                if (acceptedSocket != null) {

                    // Return the port number on the remote host to which this socket is connected.
                    final InetAddress inetAddress = acceptedSocket.getInetAddress();

                    if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                        Preferences.debug("DICOMReceiver.mipavReciever: (" + this.hashCode()
                                + ") Accepted connection from " + inetAddress + "\n");
                    }

                    try {

                        // Call the client routine to process the DICOM C-Store Request or C-Echo
                        receiverClient(acceptedSocket);
                    } catch (final Exception e) {
                        showMessage("receiverClient() exception: " + e);

                        // report it even if showMessage() doesn't ...
                        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                            Preferences.debug("DICOMReceiver.mipavReciever: " + e);
                        }
                    }

                    if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                        Preferences.debug("DICOMReceiver.mipavReciever: (" + this.hashCode()
                                + ") Closing connection from " + inetAddress + "\n");
                    }

                    try {
                        acceptedSocket.close();
                    } catch (final Exception e) {

                        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                            Preferences.debug("DICOMReceiver.mipavReciever: Problems closing socket." + "\n");
                        }
                    }

                    acceptedSocket = null;

                }
            } catch (final Exception e) {

                if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                    Preferences.debug("DICOMReceiver.mipavReciever: Error mipav_rcvr " + e + "\n");
                }

                e.printStackTrace();
            }

            /**
             * Notifies the observers that this object has changed.
             */
            if (hasChanged()) {
                notifyObservers(fileNameList);
            }

        }

        // cleanup
        if (recSocket != null) {

            try {
                recSocket.close();
            } catch (final Exception ex) {
                System.err.println("Caught exception closing rec socket");
            }

            recSocket = null;
        }

        if (acceptedSocket != null) {

            try {
                acceptedSocket.close();
            } catch (final Exception ex) {
                System.err.println("Caught exception closing acceptedsocket");
            }

            acceptedSocket = null;
        }
    }

    /**
     * @see Observable#notifyObservers(Object)
     */
    public void notifyObservers(final Object obj) {

        for (int i = 0; i < countObservers(); i++) {
            final Observer observer = (Observer) observerList.get(i);
            observer.update(this, obj);
        }

        clearChanged();
    }

    /**
     * Routine to Process a DICOM Store Request.
     * 
     * @param socket this is a socket
     */
    public void receiverClient(final Socket socket) {

        final DICOM_Object dco = new DICOM_Object(); /* Reference to DICOM command object */
        final DICOM_Object ddo = new DICOM_Object(); /* Reference to DICOM data object */

        /**
         * Clear the file name list
         */
        fileNameList.clear();

        final DICOM_CResponse cStoreRSP = new DICOM_CResponse(DICOM_Constants.COMMAND_CStoreRSP);

        JDialogText receivedImageDialog = null;

        int command;
        int process = 0;
        FileInfoDicom fileInfo = null;
        FileDicom fileDicom = null;

        try {

            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug("\n" + DICOM_Util.timeStamper() + " DICOMReceiver.recieverClient: Begin ("
                        + this.hashCode() + ")  \n");
            }

            handleConnectionFromServer(socket);// handle association negotiation with server requester

            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug(DICOM_Util.timeStamper() + " DICOMReceiver.receiverClient(" + this.hashCode()
                        + ") Handled connection, now go read DICOM command object. " + "\n");
            }

            while (readInObject(dco)) { // Reads in DICOM command object.

                command = dco.getInt16(DICOM_RTC.DD_CommandField);

                if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                    Preferences.debug(DICOM_Util.timeStamper() + " DICOMReceiver.receiverClient: "
                            + dco.toString("receiverClient(): Command Group: ") + "\n");
                    Preferences.debug(DICOM_Util.timeStamper() + " DICOMReceiver.receiverClient: Command Group = "
                            + command + " (" + DICOM_Constants.convertCommandToString(command) + ") \n");
                }

                switch (command) {

                    case DICOM_Constants.COMMAND_CStoreRQ:
                        break;

                    case DICOM_Constants.COMMAND_CEchoRQ:
                        break;

                    default:
                        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                            Preferences.debug(DICOM_Util.timeStamper()
                                    + " DICOMReceiver.receiverClient: Unrecognized command name "
                                    + DICOM_Util.toHexString((short) command) + "\n");
                        }

                        return;
                }

                if (command == DICOM_Constants.COMMAND_CEchoRQ) {

                    if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                        Preferences.debug(DICOM_Util.timeStamper() + " DICOMReceiver.recieverClient: Begin Echo \n");
                    }

                    // Read echo.
                    verification.read(this, dco);

                    if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                        Preferences.debug(DICOM_Util.timeStamper() + " DICOMReceiver.receiverClient: "
                                + dco.toString("COMMAND_CEchoRQ: Command Group: ") + "\n");
                    }
                } else if (command == DICOM_Constants.COMMAND_CStoreRQ) {

                    // Time to go read the data object from the received message
                    if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                        Preferences.debug(DICOM_Util.timeStamper()
                                + " DICOMReceiver.recieverClient: Begin reading DICOM data object \n");
                    }

                    try {
                        readInObject(null); // This fills the compData buffer
                        fileDicom = new FileDicom("temp.dcm");
                        addPreambleAndGroupTwoTags(dco); // Build DICOM part 10 preample.

                        if ( (pre_and_fullData == null)
                                || (pre_and_fullData.length() < (preambleBuffer.length + compData.length()))) {
                            pre_and_fullData = new ByteBuffer(preambleBuffer.length + compData.length());
                        }

                        System.arraycopy(preambleBuffer, 0, pre_and_fullData.data, 0, preambleBuffer.length);
                        System.arraycopy(compData.data, 0, pre_and_fullData.data, preambleBuffer.length, compData
                                .length());
                        pre_and_fullData.endIndex = preambleBuffer.length + compData.length();
                        fileDicom.setTagBuffer(pre_and_fullData.data);

                        if (fileDicom.readHeader(false) == false) {

                            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                                Preferences.debug(DICOM_Util.timeStamper()
                                        + " DICOMReceiver.recieverClient: Failure reading DICOM image. \n");
                            }

                            break; // Should do something more expressive reporting
                        }
                    } catch (final IOException ioe) {
                        ioe.printStackTrace();
                    }

                    if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                        Preferences.debug(DICOM_Util.timeStamper()
                                + " DICOMReceiver.recieverClient: Completed reading DICOM data object \n");
                    }

                    // Get fileInfo object because we will need tag information later.
                    fileInfo = (FileInfoDicom) fileDicom.getFileInfo();

                    final String uid = DICOM_Util.determineSOPClassUID(dco, null);

                    if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                        Preferences.debug(DICOM_Util.timeStamper() + " DICOMReceiver.recieverClient: UID  = " + uid
                                + "\n");
                    }

                    final int ID = dco.getInt16(DICOM_RTC.DD_MoveOriginatorMessageID);
                    process = DICOMDisplayer.getRowFromID(ID); // process = row in message table

                    if (process == -1) {
                        process = 0;
                    }

                    // read patient name, remove all delimiters and trim all spaces
                    String patientName = (String) (fileInfo.getTagTable().getValue("0010,0010"));
                    patientName = patientName.trim();

                    // Matt and Dave patientID = might be better or UID
                    if ( (patientName == null) || patientName.equals("")) {
                        patientName = new String("NA");
                    } else {
                        patientName = patientName.replace('^', '_');
                        patientName = patientName.replace(' ', '_');
                        patientName = patientName.trim();
                    }

                    final String modality = ((String) (fileInfo.getTagTable().getValue("0008,0060"))).trim();
                    String studyNo = ((String) (fileInfo.getTagTable().getValue("0020,000D"))).trim();
                    final String imageNo = ((String) (fileInfo.getTagTable().getValue("0020,0013"))).trim();
                    String seriesNo = ((String) (fileInfo.getTagTable().getValue("0020,0011"))).trim();

                    if ( (studyNo == null) || (studyNo.length() == 0)) {
                        studyNo = (String) (fileInfo.getTagTable().getValue("0008,1030"));
                        studyNo = studyNo.trim();
                    }

                    if ( (seriesNo == null) || (seriesNo.length() == 0)) {
                        seriesNo = (String) (fileInfo.getTagTable().getValue("0008,103E"));
                        studyNo = studyNo.trim();
                    }

                    defaultStorageDir = DICOM_PDUService.parseServerInfo(Preferences.getProperty(Preferences
                            .getDefaultStorageKey()))[2]
                            + File.separatorChar;

                    String filePath = defaultStorageDir;
                    String fileName = new String("");

                    // set up the path and filename for received image
                    filePath = filePath.concat(patientName + File.separatorChar + "st_" + studyNo + File.separatorChar
                            + "ser_" + seriesNo);

                    DICOMDisplayer.setMessageType(process, DICOMDisplayer.DESTINATION);
                    showMessage(filePath);

                    if (modality.equals("PT")) { // If modality is PET

                        final int imageIndex = ddo.getInt16(DICOM_RTC.DD_ImageIndex);
                        fileName = fileName.concat(File.separatorChar + "i_" + imageIndex + ".dcm");
                    } else if (modality.equals("CR")) {
                        fileName = fileName.concat(File.separatorChar + "i_" + DICOM_Receiver.crNum + ".dcm");
                        DICOM_Receiver.crNum++;
                    } else if ( (imageNo != null) || !imageNo.equals("")) {

                        try {

                            // pad image instance number string with preceding zeros so that 0001, 0002, ... 0010,
                            // 0011, ... 0654, ...
                            String imageNumStr = new String();
                            final int im = Integer.parseInt(imageNo);

                            if (im < 10) {
                                imageNumStr += "000";
                            } else if (im < 100) {
                                imageNumStr += "00";
                            } else if (im < 1000) {
                                imageNumStr += "0";
                            }

                            fileName = fileName.concat(File.separatorChar + "i_" + imageNumStr + imageNo + ".dcm");
                        } catch (final NumberFormatException nfe) {
                            String imageNumStr = new String();

                            if (DICOM_Receiver.crNum < 10) {
                                imageNumStr += "000";
                            } else if (DICOM_Receiver.crNum < 100) {
                                imageNumStr += "00";
                            } else if (DICOM_Receiver.crNum < 1000) {
                                imageNumStr += "0";
                            }

                            fileName = fileName.concat(File.separatorChar + "i_" + imageNumStr + DICOM_Receiver.crNum
                                    + ".dcm");
                            DICOM_Receiver.crNum++;
                        } catch (final Exception e) {
                            e.printStackTrace();
                        }
                    } else {
                        String imageNumStr = new String();

                        if (DICOM_Receiver.crNum < 10) {
                            imageNumStr += "000";
                        } else if (DICOM_Receiver.crNum < 100) {
                            imageNumStr += "00";
                        } else if (DICOM_Receiver.crNum < 1000) {
                            imageNumStr += "0";
                        }

                        fileName = fileName.concat(File.separatorChar + "i_" + imageNumStr + DICOM_Receiver.crNum
                                + ".dcm");
                        DICOM_Receiver.crNum++;
                    }

                    DICOMDisplayer.setMessageType(process, DICOMDisplayer.PROGRESS);
                    showMessage(imageNo);
                    DICOMDisplayer.setMessageType(process, DICOMDisplayer.STATUS);
                    showMessage("Saving images");

                    final File DICOMFile = new File(filePath);

                    // create the directories for the received image
                    final boolean status = DICOMFile.mkdirs();

                    if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                        Preferences.debug(DICOM_Util.timeStamper() + " DICOMReceiver.recieverClient: Saving image: "
                                + filePath + fileName + "\n");
                    }

                    // Save image (with preample appended) data to file
                    saveImageToFile(pre_and_fullData, filePath + fileName);
                    fileNameList.add(filePath + fileName);
                    // preambleBuffer = null;

                    if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                        Preferences.debug(DICOM_Util.timeStamper()
                                + " DICOMReceiver.recieverClient: StoreRSP: Success " + filePath + fileName + "\n");
                    }

                    if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                        Preferences.debug(DICOM_Util.timeStamper()
                                + " DICOMReceiver.recieverClient: Sending Status Store Success RSP \n");
                    }

                    cStoreRSP.write(this, dco, uid, DICOM_Constants.STATUS_STORE_SUCCESS, null, null, 0, 0, 0, 0);

                    if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                        Preferences.debug(DICOM_Util.timeStamper()
                                + " DICOMReceiver.recieverClient: Completed StoreRSP  Success  \n");
                    }

                    // We need to check on this -- Matt 6/15/2000
                    // When images are pushed dicomMessageDisplayer appears null
                    if (dicomMessageDisplayer != null) {
                        (dicomMessageDisplayer).setSucceeded(true);
                    } else if ( (dicomMessageDisplayer == null) && (receivedImageDialog == null)) {

                        // make non-modal message frame
                        final JFrame parent = new JFrame();

                        try {
                            parent.setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
                        } catch (final FileNotFoundException error) {
                            Preferences.debug("Exception ocurred while getting <" + error.getMessage()
                                    + ">.  Check that this file is available.\n");
                            System.err.println("Exception ocurred while getting <" + error.getMessage()
                                    + ">.  Check that this file is available.\n");
                        }

                        receivedImageDialog = new JDialogText(parent, "Received Images");

                        if (receivedImageDialog != null) {
                            receivedImageDialog.setLocation(200, 50);
                            receivedImageDialog.setVisible(true);
                        }

                        receivedImageDialog.append("From: " + new String(super.getAAssociateRQ().getCallingAppTitle())
                                + ": " + filePath + fileName + "\n");

                    } else if ( (dicomMessageDisplayer == null) && (receivedImageDialog != null)) {

                        // Update message with name of newly arrived image
                        receivedImageDialog.append("From: " + new String(super.getAAssociateRQ().getCallingAppTitle())
                                + ": " + filePath + fileName + "\n");
                    }

                    if (cancelled) {
                        DICOMDisplayer.setMessageType(process, DICOMDisplayer.STATUS);
                        showMessage("Cancelled");
                        cancelled = false;

                        break;
                    }
                }
            }
        } catch (final DICOM_Exception e) {
            DICOMDisplayer.setMessageType(process, DICOMDisplayer.ERROR);
            showMessage("" + e);
        }

        System.gc();

        /**
         * Marks this object as having been changed.
         */
        if (fileNameList.size() > 0) {
            setChanged();
        }
    }

    /**
     * Resets the port and changes where the receiver is listening.
     * 
     * @param port port to listen on
     */
    public synchronized void resetPort(final int port) {
        this.port = port;

        try {
            runner.setStop();
            wait(3000);
            start(Thread.MIN_PRIORITY);
        } catch (final Exception e) {

            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug("DICOMReceiver.mipavReciever: Reset fatal: Server listen failed: " + e + "\n");
            }

            return;
        }

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug("DICOMReceiver.mipavReciever: Reset listening on port " + port + ".\n");
        }
    }

    /**
     * Run method so that this receiver can execute in its own thread.
     */
    public void run() {

        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug("Starting DICOMReceiver.run\n");
        }

        mipavReceiver();
    }

    /**
     * Turns the cancelled flag on or off.
     * 
     * @param flag indicator flag
     */
    public void setCancelled(final boolean flag) {
        cancelled = flag;
    }

    /**
     * @see Observable#setChanged()
     */
    public void setChanged() {
        changed = true;
    }

    /**
     * Stops the receiver thread.
     */
    public void setStop() {
        runner.setStop();
    }

    /**
     * Checks to see if a thread is already running on this object, and if not starts it.
     * 
     * @param priority thread priority
     */
    public void start(final int priority) {

        try {
            runner = new RunnerThread(this);

        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: DICOMReceiver.start");

            return;
        }

        if (storageProperty != null) {
            port = Integer.valueOf(DICOM_PDUService.parseServerInfo(storageProperty)[3]).intValue();
            boolean bindSuccess = createServerSocket();
            if ( !bindSuccess) {
                return; // returns w/o starting dicom receiver, could optionally have a dialog here to modify the
                        // storage destinations
            }
        } else {
            MipavUtil.displayError("Cannot find port number from preference file.  Go to DICOM\n"
                    + "database access and set storage destination properties.");

            final String values = "MIPAV;MIPAV;C:\\images;3100";
            Preferences.setProperty(Preferences.PREF_DICOM_STORAGE_DIR, values);

            return;
        }

        if (runner.isAlive() == true) {
            return;
        } else {

            if ( (priority < Thread.MAX_PRIORITY) && (priority > Thread.MIN_PRIORITY)) {
                runner.setPriority(priority);
            } else {
                runner.setPriority(Thread.MIN_PRIORITY);
            }

            runner.start();
        }

        return;
    }

    /**
     * Creates the initial server socket binded to the given port. If unable to be created, will return without having
     * started the dicom receiver.
     * 
     * @param port
     */
    private boolean createServerSocket() {
        // Create an unconnected server socket to the port.
        try {
            recSocket = new ServerSocket(port);
        } catch (final BindException e) {
            MipavUtil.displayWarning("Port " + port + " is already in use by another DICOM receiver. Please use\n"
                    + "the DICOM Communication Panel to choose a new storage destination.");

            return false;
        } catch (final Exception e) {

            if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                Preferences.debug("DICOMReceiver.mipavReciever: Fatal: MIPAV's receiver failed to start: " + e + "\n");
            }

            MipavUtil.displayWarning("MIPAV's receiver failed to start: " + e);

            return false;
        }
        if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
            Preferences.debug("DICOMReceiver.mipavReciever: Listening on port " + port + ". \n");
        }

        return true;

    }

    /**
     * Builds buffer of DICOM part 10 preamble and require Part 10 group 2 tags.
     * 
     * @param dco DICOM_Object DICOM command object needed to extract UID information to be included in the preample.
     */
    private void addPreambleAndGroupTwoTags(final DICOM_Object dco) {

        String classUID;
        String instanceUID;
        String implementationClassUID;
        String transferSyntaxUID;

        // Tags to add
        // 0002, 0000 byte length from after this File Meta Element (end of value field)
        // up to and including the last File Meta Element of the Group 2
        // File Meta Information.
        // 0002, 0001 file Meta Information Version
        // 0002, 0002 Media storage SOP class UID
        // 0002, 0003 Media storage SOP instance UID
        // 0002, 0010 Transfer Syntax UID
        // 0002, 0012 Implementation Class UID

        classUID = dco.getStr(DICOM_RTC.DD_AffectedSOPClassUID);
        instanceUID = dco.getStr(DICOM_RTC.DD_AffectedSOPInstanceUID);
        implementationClassUID = getImplementationClassUID();
        transferSyntaxUID = getTransferSyntaxID();

        int classUIDLength = classUID.length();

        if ( (classUID.length() % 2) == 1) {
            classUIDLength++;
        }

        int instanceUIDLength = instanceUID.length();

        if ( (instanceUID.length() % 2) == 1) {
            instanceUIDLength++;
        }

        int implementationClassUIDLength = implementationClassUID.length();

        if ( (implementationClassUID.length() % 2) == 1) {
            implementationClassUIDLength++;
        }

        int transferSyntaxUIDLength = transferSyntaxUID.length();

        if ( (transferSyntaxUID.length() % 2) == 1) {
            transferSyntaxUIDLength++;
        }

        // TODO: adjust length to include vr values of the preable
        // Length equals full length of preample.
        final int length = 132 + 12 + 14 + 8 + classUIDLength + 8 + instanceUIDLength + 8 + transferSyntaxUIDLength + 8
                + implementationClassUIDLength;

        if ( (preambleBuffer == null) || (preambleBuffer.length != length)) {
            preambleBuffer = new byte[length];
        }

        int idx = 128;

        byte[] byteArray = null;

        preambleBuffer[idx++] = 0x44; // D
        preambleBuffer[idx++] = 0x49; // I
        preambleBuffer[idx++] = 0x43; // C
        preambleBuffer[idx++] = 0x4d; // M

        // 0200 0000 0400 0000 xx00 0000 xx =
        // (0002, 0000)
        preambleBuffer[idx++] = 0x02;
        preambleBuffer[idx++] = 0x00;
        preambleBuffer[idx++] = 0x00;
        preambleBuffer[idx++] = 0x00;

        // vr
        preambleBuffer[idx++] = 0x55; // U
        preambleBuffer[idx++] = 0x4c; // L

        // Length
        preambleBuffer[idx++] = 4;
        preambleBuffer[idx++] = 0x00;

        DICOM_Comms.int32ToBuffer(preambleBuffer, idx, length - (idx + 4), DICOM_Comms.LITTLE_ENDIAN);

        idx += 4;

        // ***** (0002, 0001)
        preambleBuffer[idx++] = 0x02;
        preambleBuffer[idx++] = 0x00;
        preambleBuffer[idx++] = 0x01;
        preambleBuffer[idx++] = 0x00;

        // vr
        preambleBuffer[idx++] = 0x4f; // O
        preambleBuffer[idx++] = 0x42; // B
        preambleBuffer[idx++] = 0x00;
        preambleBuffer[idx++] = 0x00;

        // Length
        preambleBuffer[idx++] = 2;
        preambleBuffer[idx++] = 0x00;
        preambleBuffer[idx++] = 0x00;
        preambleBuffer[idx++] = 0x00;

        // TODO: what is this value?
        preambleBuffer[idx++] = 0x33;
        preambleBuffer[idx++] = 0x37;

        // ***** fileInfo.get("0008,0016");
        preambleBuffer[idx++] = 0x02;
        preambleBuffer[idx++] = 0x00;
        preambleBuffer[idx++] = 0x02;
        preambleBuffer[idx++] = 0x00;

        // vr
        preambleBuffer[idx++] = 0x55; // U
        preambleBuffer[idx++] = 0x49; // I

        // Length
        byteArray = classUID.getBytes();
        preambleBuffer[idx++] = (byte) classUIDLength;
        preambleBuffer[idx++] = 0x00;

        for (int i = 0, j = idx; i < classUID.length(); i++, j++) {
            preambleBuffer[j] = byteArray[i];
            idx++;
        }

        if (classUID.length() != classUIDLength) {
            preambleBuffer[idx++] = 0x00;
        }

        // ***** fileInfo.get("0008,0018");
        preambleBuffer[idx++] = 0x02;
        preambleBuffer[idx++] = 0x00;
        preambleBuffer[idx++] = 0x03;
        preambleBuffer[idx++] = 0x00;

        // vr
        preambleBuffer[idx++] = 0x55; // U
        preambleBuffer[idx++] = 0x49; // I

        // Length
        byteArray = instanceUID.getBytes();
        preambleBuffer[idx++] = (byte) instanceUIDLength;
        preambleBuffer[idx++] = 0x00;

        for (int i = 0, j = idx; i < instanceUID.length(); i++, j++) {
            preambleBuffer[j] = byteArray[i];
            idx++;
        }

        if (instanceUID.length() != instanceUIDLength) {
            preambleBuffer[idx++] = 0x00;
        }

        // 0200 1000
        preambleBuffer[idx++] = 0x02;
        preambleBuffer[idx++] = 0x00;
        preambleBuffer[idx++] = 0x10;
        preambleBuffer[idx++] = 0x00;

        // vr
        preambleBuffer[idx++] = 0x55; // U
        preambleBuffer[idx++] = 0x49; // I

        // length
        byteArray = transferSyntaxUID.getBytes();
        preambleBuffer[idx++] = (byte) transferSyntaxUIDLength;
        preambleBuffer[idx++] = 0x00;

        for (int i = 0, j = idx; i < transferSyntaxUID.length(); i++, j++) {
            preambleBuffer[j] = byteArray[i];
            idx++;
        }

        if (transferSyntaxUID.length() != transferSyntaxUIDLength) {
            preambleBuffer[idx++] = 0x00;
        }

        // ***** (0002, 0012)
        preambleBuffer[idx++] = 0x02;
        preambleBuffer[idx++] = 0x00;
        preambleBuffer[idx++] = 0x12;
        preambleBuffer[idx++] = 0x00;

        // vr
        preambleBuffer[idx++] = 0x55; // U
        preambleBuffer[idx++] = 0x49; // I

        // Length
        preambleBuffer[idx++] = (byte) implementationClassUIDLength;
        preambleBuffer[idx++] = 0x00;

        // new String("1.2.840.34379.17");
        byteArray = implementationClassUID.getBytes();

        for (int i = 0, j = idx; i < implementationClassUID.length(); i++, j++) {
            preambleBuffer[j] = byteArray[i];
        }

        if (implementationClassUID.length() != implementationClassUIDLength) {
            preambleBuffer[idx++] = 0x00;
        }

        // Implement version name "NIH MIPAV 05 1.1" + 16 --- optional
        // parse string into buffer
        // 0200 1300 1000 0000 4e49 4820 4d49 5041 5620 3035 2031 2e31

        return;
    }

    // ~ Inner Classes
    // --------------------------------------------------------------------------------------------------

    /**
     * Thread manager for running the DICOM receiver.
     */
    public class RunnerThread extends Thread {

        /** DOCUMENT ME! */
        private boolean keepGoing = true;

        /**
         * Creates a new RunnerThread object.
         * 
         * @param target DOCUMENT ME!
         */
        public RunnerThread(final Runnable target) {
            super(target);
        }

        /**
         * DOCUMENT ME!
         * 
         * @return DOCUMENT ME!
         */
        public boolean keepGoing() {
            return keepGoing;
        }

        /**
         * DOCUMENT ME!
         */
        public void setStop() {
            keepGoing = false;
        }
    }
}
