package gov.nih.mipav.model.dicomcomm;


import java.util.Vector;


/**
 * DICOM User Information PDU Item Type (part 8 section 9.3).
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
public class DICOM_UserInformation extends DICOM_PDUItemType {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public DICOM_AsyncOpWindowSubItem aSyncSubItem = null; // new DICOM_AsyncOpWindowSubItem();

    /** DOCUMENT ME! */
    public DICOM_PDUItemType implementationClass = new DICOM_PDUItemType(DICOM_PDUTypeBase.PDUTYPE_ImplementationClass);

    /** DOCUMENT ME! */
    public DICOM_PDUItemType implementationVersion = new DICOM_PDUItemType(
            DICOM_PDUTypeBase.PDUTYPE_ImplementationVersion);

    /** DOCUMENT ME! */
    public DICOM_MaximumSubLength maxSubLength = new DICOM_MaximumSubLength();

    /** public DICOM_SCPSCURoleSelect SCPSCURole = null; //new DICOM_SCPSCURoleSelect();. */
    public Vector SCPSCURoleVector = new Vector();

    /** DOCUMENT ME! */
    private int userInfoSize = 0;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Constructs a DICOM user information object. This object extends the DICOM_PDUItemType
     */
    public DICOM_UserInformation() {
        super(DICOM_PDUTypeBase.PDUTYPE_UserInformation);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Calculates the PDU item type size.
     * 
     * @return the size = parent size + userInfoSize
     */
    public int calcSize() {
        return (super.calcSize());
    }

    /**
     * Accessor that returns the length in bytes.
     * 
     * @return item length
     */
    public int length() {
        int length;

        length = implementationClass.calcSize();
        length += implementationVersion.calcSize();
        length += maxSubLength.calcSize();

        for (int i = 0; i < SCPSCURoleVector.size(); i++) {
            length += ((DICOM_SCPSCURoleSelect) (SCPSCURoleVector.elementAt(i))).calcSize();
        }

        if (aSyncSubItem != null) {
            length += aSyncSubItem.calcSize();
        }

        length += userInfoSize;

        return (length);
    }

    /**
     * Reads the body of the user information packet.
     * 
     * @param connection the connection to read from
     * 
     * @exception DICOM_Exception DICOM_Exception if problem occurs
     */
    public void readBody(final DICOM_Comms connection) throws DICOM_Exception {
        int count = length;

        userInfoSize = 0;

        while (count > 0) {

            // Preferences.debug(DICOM_Util.timeStamper() + " DICOM_UserInformation.readBody: count = " + length + "
            // \n");

            switch (connection.peekFirstByte()) {

                case PDUTYPE_ImplementationClass:
                    implementationClass.read(connection);
                    count = count - implementationClass.calcSize();
                    break;

                case PDUTYPE_ImplementationVersion:
                    implementationVersion.read(connection);
                    count = count - implementationVersion.calcSize();
                    break;

                case PDUTYPE_MaximumSubLength:
                    maxSubLength.read(connection);
                    count = count - maxSubLength.calcSize();
                    break;

                case PDUTYPE_SCPSCURoleSelect:

                    // Not sure how this info is used. Kodak's Auto Rads send an array of them).
                    final DICOM_SCPSCURoleSelect SCPSCURole = new DICOM_SCPSCURoleSelect();
                    SCPSCURole.read(connection);
                    count = count - SCPSCURole.calcSize();
                    SCPSCURoleVector.add(SCPSCURole);
                    break;

                case PDUTYPE_AsyncOpWindowSubItem:

                    // Not supported and is optional - however it is read in.
                    aSyncSubItem = new DICOM_AsyncOpWindowSubItem();
                    aSyncSubItem.read(connection);
                    count = count - aSyncSubItem.calcSize();
                    break;

                default:
                    connection.readBytes(count - 1);
                    userInfoSize = count;
                    throw new DICOM_Exception("DICOMError: Error reading user info.");
            }
        }
    }

    /**
     * Writes the body of the user information.
     * 
     * @param connection the connection to write to
     * 
     * @exception DICOM_Exception DICOM_Exception if problem occurs
     */
    public void writeBody(final DICOM_Comms connection) throws DICOM_Exception {
        maxSubLength.write(connection);
        implementationClass.write(connection);
        implementationVersion.write(connection);
    }

}
