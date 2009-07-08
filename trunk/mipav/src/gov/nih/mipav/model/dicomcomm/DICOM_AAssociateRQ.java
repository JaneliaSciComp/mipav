package gov.nih.mipav.model.dicomcomm;


import gov.nih.mipav.view.*;

import java.util.*;


/**
 * DICOM Association Request PDU Type class.
 */
public class DICOM_AAssociateRQ extends DICOM_PDUType {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Application context PDU type.  */
    protected DICOM_PDUItemType appContext = new DICOM_PDUItemType(PDUTYPE_ApplicationContext);

    /** The called application entity name, must be 16 or less characters. */
    protected byte[] calledAppTitle = new byte[16];

    /** The calling application entity name, must be 16 or less characters. */
    protected byte[] callingAppTitle = new byte[16];

    /** A list of presentation context objects. */
    protected Vector presContexts = new Vector();

    /** Protocol version identifier. */
    protected int protocolVersion = DICOM_Constants.PROTOCOLVERSION;

    /** Reserved by array. */
    protected byte[] reserved2 = new byte[2];

    /** Reserved by array. */
    protected byte[] reserved3 = new byte[32];

    /** User information structure. */
    protected DICOM_UserInformation userInfo = new DICOM_UserInformation();

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new DICOM_AAssociateRQ object.
     */
    public DICOM_AAssociateRQ() {
        itemType = PDUTYPE_AAssociateRQ;

        DICOM_Util.zeroByteArray(reserved2);
        DICOM_Util.clearByteArray(calledAppTitle);
        DICOM_Util.clearByteArray(callingAppTitle);
        DICOM_Util.zeroByteArray(reserved3);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Adds a presentation object to a vector list of presentation contexts.
     *
     * @param  pc  presentation context object to be added
     */
    public void addPresentationContext(DICOM_PresentationContext pc) {
        presContexts.addElement(pc);
    }


    /**
     * Removes all elements from the presentation contexts vector list.
     */
    public void clearPresentationContexts() {
        presContexts.removeAllElements();
    }

    /**
     * Accessor to return the application context.
     *
     * @return  the application context
     */
    public DICOM_PDUItemType getApplicationContext() {
        return appContext;
    }

    /**
     * Accessor to return the called application entity title.
     *
     * @return  the called application's entity title in an array of bytes (<= 16)
     */
    public byte[] getCalledAppTitle() {
        return calledAppTitle;
    }

    /**
     * Accessor to return the calling application entity title.
     *
     * @return  the calling application's entity title in an array of bytes (<= 16)
     */
    public byte[] getCallingAppTitle() {
        return callingAppTitle;
    }

    /**
     * Gets a presentation context ID for a given abstrax syntax.
     *
     * @param   ID  the abstract syntax UID
     *
     * @return  the presentation context ID
     */
    public String getPresentationContextFromID(int ID) {
        DICOM_PresentationContext pc = null;

        for (int i = 0; i < presContexts.size(); i++) {

            if (((DICOM_PresentationContext) (presContexts.elementAt(i))).presentationContextID == ID) {
                return (((DICOM_PresentationContext) (presContexts.elementAt(i))).absSyntax.getUID());
            }
        }

        // if( pc == null ) {
        // throw new DICOM_Exception( "Unable determine presentation abstract syntax UID from context ID ");
        // }

        return ("Not found");
    }

    /**
     * Gets a presentation context ID for a given abstrax syntax.
     *
     * @param      absUID  the abstract syntax UID
     *
     * @return     the presentation context ID
     *
     * @exception  DICOM_Exception  unknown presentation context ID
     */
    public byte getPresentationContextID(String transferSyntax, String absUID) throws DICOM_Exception {
        DICOM_PresentationContext pc = null;
        DICOM_PresentationContext tpc = null;

        for (int i = presContexts.size()-1; i <=0; i--) {
            tpc = ((DICOM_PresentationContext) (presContexts.elementAt(i)));
            if (tpc.absSyntax.getUID().equals(absUID)) {
                
                Vector  tferSyntaxVect =  ((DICOM_PresentationContext) (presContexts.elementAt(i))).trnSyntax;
                if (tferSyntaxVect != null) {
                    String tStr = ((DICOM_PDUItemType)(tferSyntaxVect.elementAt(0))).getUID();
                    if ( tStr.trim().equals(transferSyntax.trim())) {
                        pc = (DICOM_PresentationContext) (presContexts.elementAt(i));
                    }
                }
            }
        }

        if (pc == null) {
            throw new DICOM_Exception("Unable determine presentation context ID for abstract syntax UID: " + absUID +
                                      ".");
        }

        return (pc.presentationContextID);
    }
    
    /**
     * Gets a presentation context ID for a given abstrax syntax.
     *
     * @param      absUID  the abstract syntax UID
     *
     * @return     the presentation context ID
     *
     * @exception  DICOM_Exception  unknown presentation context ID
     */
    public byte getPresentationContextID( String absUID) throws DICOM_Exception {
        DICOM_PresentationContext pc = null;

        for (int i = presContexts.size()-1; i <= 0; i--) {

            pc = (DICOM_PresentationContext) (presContexts.elementAt(i));
        }

        if (pc == null) {
            throw new DICOM_Exception("Unable determine presentation context ID for abstract syntax UID: " + absUID +
                                      ".");
        }

        return (pc.presentationContextID);
    }

    /**
     * Accessor to return the presentation context.
     *
     * @return  the application context
     */
    public Vector getPresentationContexts() {
        return presContexts;
    }

    /**
     * Accessor to return User Information.
     *
     * @return  the user information for this PDU type
     */
    public DICOM_UserInformation getUserInformation() {
        return userInfo;
    }

    /**
     * Calculates length of message.
     *
     * @return  the length of the message including application context + presentation context(s) + userInfo
     */
    public int length() {
        int messageLength;

        // protocol version + reserved2 + calledApp + callingApp + reserved3
        messageLength = 68; // 2          +     2     +     16    +     16     +    32;
        messageLength += appContext.calcSize();

        for (int i = 0; i < presContexts.size(); i++) {
            messageLength += ((DICOM_PresentationContext) (presContexts.elementAt(i))).calcSize();
        }

        messageLength += userInfo.calcSize();

        return (messageLength);
    }

    /**
     * Reads the body of an association request.
     *
     * @param      connection  the I/O Buffer to read from
     *
     * @exception  DICOM_Exception  unknown PDUType
     */
    public void readBody(DICOM_Comms connection) throws DICOM_Exception {
        int readLength;
        DICOM_PresentationContext presContext;

        protocolVersion = connection.readShort16();
        connection.read(reserved2);
        connection.read(calledAppTitle);
        connection.read(callingAppTitle);
        connection.read(reserved3);

        // (length) - protocol version - reserved2 - calledApp - callingApp - reserved3
        readLength = length - 68; // 2        -     2     -    16     -     16     -    32;

        while (readLength > 0) {

            switch (connection.peekFirstByte()) {

                case PDUTYPE_ApplicationContext:
                    appContext.read(connection);
                    readLength = readLength - appContext.calcSize();
                    break;

                case PDUTYPE_PresentationContext:
                    presContext = new DICOM_PresentationContext();
                    presContext.read(connection);
                    readLength = readLength - presContext.calcSize();
                    addPresentationContext(presContext);

                    if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                        Preferences.debug(DICOM_Util.timeStamper() +
                                          " DICOM_AAssociateRQ.readBody: added possible presentation context.  = " +
                                          presContext.absSyntax.getUID() + "\n");
                    }

                    break;

                case PDUTYPE_UserInformation:
                    if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                        Preferences.debug(DICOM_Util.timeStamper() +
                                          " DICOM_AAssociateRQ.readBody: just before reading user information. Read length = " +
                                          readLength + "\n");
                    }

                    userInfo.read(connection);
                    readLength = readLength - userInfo.calcSize(); // - userInfo.userInfoSize;

                    if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                        Preferences.debug(DICOM_Util.timeStamper() +
                                          " DICOM_AAssociateRQ.readBody: just after reading user information. Read length = " +
                                          readLength + "\n");
                    }

                    if (Preferences.debugLevel(Preferences.DEBUG_COMMS)) {
                        Preferences.debug(DICOM_Util.timeStamper() + " DICOM_AAssociateRQ.readBody UserInfo. = " +
                                          userInfo.implementationVersion.getUID() + "\n");
                    }

                    break;

                default:
                    throw new DICOM_Exception("DICOMError: error in AAssociateRQ.readBody: unknown PDUType");
            }
        }
    }

    /**
     * Sets the application context.
     *
     * @param  appContext  the application context
     */
    public void setApplicationContext(DICOM_PDUItemType appContext) {
        this.appContext = appContext;
    }

    /**
     * Sets the application context's UID.
     *
     * @param  UID  the application context's UID
     */
    public void setApplicationContextUID(String UID) {
        appContext.setUID(UID);
    }

    /**
     * Sets the called application entity title.
     *
     * @param  calledApp  the called application entity title
     */
    public void setCalledAppTitle(byte[] calledApp) {
        DICOM_Util.clearByteArray(calledAppTitle);
        DICOM_Util.copyByteArray(calledAppTitle, calledApp);
    }

    /**
     * Sets the calling application entity title.
     *
     * @param  callingApp  the calling application entity title
     */
    public void setCallingAppTitle(byte[] callingApp) {
        DICOM_Util.clearByteArray(callingAppTitle);
        DICOM_Util.copyByteArray(callingAppTitle, callingApp);
    }

    /**
     * Sets the user information.
     *
     * @param  userInformation  the user information
     */
    public void setUserInformation(DICOM_UserInformation userInformation) {
        userInfo = userInformation;
    }

    /**
     * Writes the body of the association request.
     *
     * @param      connection  the I/O Buffer to write to
     *
     * @exception  DICOM_Exception  problem with writing association request
     */
    public void writeBody(DICOM_Comms connection) throws DICOM_Exception {
        connection.writeShort16(protocolVersion);
        connection.write(reserved2);
        connection.write(calledAppTitle);
        connection.write(callingAppTitle);
        connection.write(reserved3);

        appContext.write(connection);

        for (int i = 0; i < presContexts.size(); i++) {
            ((DICOM_PresentationContext) presContexts.elementAt(i)).write(connection);
        }

        userInfo.write(connection);
        connection.flush();
    }


}
