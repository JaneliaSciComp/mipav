package gov.nih.mipav.model.dicomcomm;


import java.util.*;


/**
 * Implements a DICOM Association Acceptance PDU Type.
 */
public class DICOM_AAssociateAC extends DICOM_AAssociateRQ {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private Vector presContexts = new Vector();

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new DICOM_AAssociateAC object.
     */
    public DICOM_AAssociateAC() {
        itemType = PDUTYPE_AAssociateAC;
        DICOM_Util.zeroByteArray(reserved2);
        DICOM_Util.clearByteArray(calledAppTitle);
        DICOM_Util.clearByteArray(callingAppTitle);
        DICOM_Util.zeroByteArray(reserved3);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Adds the presentation context to the accepted context list.
     *
     * @param  pca  accepted presentation context to be added the list (vector)
     */
    public void addPresentationContextAccept(DICOM_PresentationContextAccept pca) {
        presContexts.addElement(pca);
    }

    /**
     * Returns the list of presentation contexts.
     *
     * @return  the list (vector) of accepted presentation contextes.
     */
    public Vector getPresentationContextes() {
        return presContexts;
    }

    /**
     * Calculates length of message.
     *
     * @return  the length of the message including application context + presentation context(s) + userInfo
     */
    public int length() {
        int messageLength;

        // protocol version + reserved2 + calledApp + callingApp + reserved3
        messageLength = 68; // 2         +      2    +     16    +      16    +      32;
        messageLength += appContext.calcSize();

        for (int i = 0; i < presContexts.size(); i++) {
            messageLength += ((DICOM_PresentationContextAccept) presContexts.elementAt(i)).calcSize();
        }

        messageLength += userInfo.calcSize();

        return (messageLength);
    }

    /**
     * Reads the body of an association accept.
     *
     * @param      connection  the I/O Buffer to read from
     *
     * @exception  DICOM_Exception  unknown PDUType
     */
    public void readBody(DICOM_Comms connection) throws DICOM_Exception {
        DICOM_PresentationContextAccept presentationContext;
        int readLength;

        protocolVersion = connection.readShort16();
        connection.read(reserved2);
        connection.read(calledAppTitle);
        connection.read(callingAppTitle);
        connection.read(reserved3);

        // length - protocol version - reserved2 - calledApp - callingApp - reserved3
        readLength = length - 68; // 2       -     2     -     16    -     16     -    32;

        while (readLength > 0) {

            switch (connection.peekFirstByte()) {

                case PDUTYPE_ApplicationContext:
                    appContext.read(connection);
                    readLength = readLength - appContext.calcSize();
                    break;

                case PDUTYPE_UserInformation:
                    userInfo.read(connection);
                    readLength = readLength - userInfo.calcSize(); // - userInfo.userInfoSize;
                    break;

                case PDUTYPE_PresentationContextAccept:
                    presentationContext = new DICOM_PresentationContextAccept();
                    presentationContext.read(connection);
                    readLength = readLength - presentationContext.calcSize();
                    addPresentationContextAccept(presentationContext);
                    break;

                default:
                    connection.readBytes(readLength - 1);
                    throw new DICOM_Exception("DICOMerror: AAssociateAC.readBody: unknown PDU type");
            }
        }
    }

    /**
     * Removes all presentation contextes from the list (vector).
     */
    public void resetPresentationContext() {
        presContexts.removeAllElements();
    }

    /**
     * Writes the body of the association accept.
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
            ((DICOM_PresentationContextAccept) presContexts.elementAt(i)).write(connection);
        }

        userInfo.write(connection);
        connection.flush();
    }

}
