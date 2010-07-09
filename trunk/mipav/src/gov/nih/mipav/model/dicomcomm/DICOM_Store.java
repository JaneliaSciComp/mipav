package gov.nih.mipav.model.dicomcomm;


import gov.nih.mipav.model.file.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * This is the DICOM store class that defines functions to compose and send a store request to a DICOM image file
 * archive such as the image file server located in NIH's Clinical Center.
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
public class DICOM_Store implements Runnable {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** Class UID. */
    private String classUID;

    /** File name of the image to be stored. */
    private final String fileName;

    /** Reference to the Query/Retrieve GUI object. */
    private ViewJFrameDICOMQuery frame;

    /** The instance UID. */
    private String instanceUID;

    /** Protocol Data Units (PDU) object. */
    private DICOM_PDUService pdu;

    /** The remote Application Entity Title. */
    private final String remoteAETitle;

    /** The transfer syntax used when storing images. */
    private String transferSyntax = null;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new DICOM_Store object.
     * 
     * @param _fileName file name of the images
     * @param _remoteAETitle remote application entity name
     * @param _frame MIPAV's query frame
     */
    public DICOM_Store(final String _fileName, final String _remoteAETitle, final ViewJFrameDICOMQuery _frame) {
        fileName = _fileName;
        remoteAETitle = _remoteAETitle;
        frame = _frame;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public void finalize() {

        if (pdu != null) {
            pdu.finalize();
        }

        pdu = null;
        frame = null;

    }

    /**
     * Runs this move request in a separate thread.
     */
    public void run() {

        // sendStoreRQ(fileName, remoteAEtitle);
        try {

            // define an instance of the PDU_Service class to set up the connection
            // to the remote AE
            pdu = new DICOM_PDUService();

            // grab the first DICOM file (if directory) to see what the transfer syntax is:

            if (fileName != null) {
                getUIDs(fileName);
            }

            pdu.connectClientToServer(remoteAETitle, false, transferSyntax, classUID);

            if (sendImages(fileName) == true) {
                frame.appendSendMessage("Success -- " + fileName + " to " + remoteAETitle + "\n");
            } else {
                frame.appendSendMessage("Failed -- " + fileName + " to " + remoteAETitle + "\n");
            }

            pdu.close();
        } catch (final OutOfMemoryError error) {
            frame.appendSendMessage("Failed -- " + fileName + " to " + remoteAETitle + "\n");
            MipavUtil.displayError("Error opening directory:" + error);
            pdu.close();

            return;
        } catch (final DICOM_Exception e) {
            frame.appendSendMessage("Failed -- " + fileName + " to " + remoteAETitle + "\n");
            MipavUtil.displayError("Error: sendStoreRQ():" + e);
            pdu.close();

            return;
        }
    }

    /**
     * Reads in the DICOM image file from disk, composes a DICOM C-Store request and sends the request.
     * 
     * @param fileName name of DICOM image file
     * 
     * @return true if the method succeeded in sending all images else false
     */
    public boolean sendStoreRQ(final String fileName) {
        DICOM_Object ddo;
        final DICOM_StdStorage storageSOP = new DICOM_StdStorage();
        boolean returnVal = false;

        try {
            ddo = pdu.readDICOMDataObjectFromFile(fileName);

            if (ddo == null) {
                MipavUtil.displayError("DICOMStore.sendStoreRQ(): DDO = null");

                return false;
            } else {

                // Get UIDs and pass it as a parameter in the next method (write. )

                // ben update: this has been moved to when we connect to the server so that we can tell the server
                // exactly what type of file we are going to send

                // getUIDs(fileName);
                storageSOP.write(pdu, ddo, transferSyntax, classUID, instanceUID);
                returnVal = true;
            }
        } catch (final DICOM_Exception e) {
            MipavUtil.displayError("DICOMStore.sendStoreRQ():" + e);

            return false;
        }

        return (returnVal);
    }

    /**
     * DOCUMENT ME!
     * 
     * @param dir DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    private String getFirstFile(final String dir) {
        int i;
        File fileDir = null;
        String[] dirList;
        String sample = "";

        try {
            fileDir = new File(dir);

            if (fileDir.isFile() == true) {
                return dir;
            } else if (fileDir.isDirectory() == false) {
                MipavUtil.displayError("DICOM send images: Not a directory");
            } else {
                dirList = fileDir.list();

                String test = null;

                for (i = 0; i < dirList.length; i++) {
                    test = dir + File.separatorChar + dirList[i];
                    if (test != null && test.lastIndexOf('.') != -1) {
                        sample = test.substring(test.lastIndexOf('.')).trim();
                        if (sample.contains("dcm")) {
                            return test;
                        }
                    }
                }
            }

        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Error opening directory:" + error);

            return null;
        }

        return null;

    }

    /**
     * Read in the DICOM file and parses the header to find the UIDs needed to send the image.
     * 
     * @param fileName DICOM file to be read.
     */
    private void getUIDs(final String file) {
        FileDicom fileDICOM = null;
        FileInfoDicom fInfoDicom = null;
        String modality = null;
        final File fileDir = new File(file);

        final String fileName = getFirstFile(file);

        try {
            fileDICOM = new FileDicom(fileName);

            if (fileDICOM.readHeader(true)) {
                fInfoDicom = (FileInfoDicom) (fileDICOM.getFileInfo());
                transferSyntax = (String) (fInfoDicom.getTagTable().getValue("0002,0010"));
                classUID = (String) (fInfoDicom.getTagTable().getValue("0008,0016"));

                if ( !fileDir.isDirectory()) {
                    instanceUID = (String) (fInfoDicom.getTagTable().getValue("0008,0018"));
                } else {
                    getDirUIDs(fileDir);
                }

                if (classUID != null) {
                    classUID = DICOM_Util.trimIgnorableChar(classUID.trim());
                }

                if (instanceUID != null) {
                    instanceUID = DICOM_Util.trimIgnorableChar(instanceUID.trim());
                }

                if (transferSyntax != null) {
                    transferSyntax = DICOM_Util.trimIgnorableChar(transferSyntax.trim());
                }

                if (classUID == null) {
                    modality = (String) (fInfoDicom.getTagTable().getValue("0008,0060"));
                    modality = modality.trim();

                    if (modality != null) {

                        if (modality.equals("CT")) {
                            classUID = DICOM_Constants.UID_CTStorage;
                        } else if (modality.equals("CR")) {
                            classUID = DICOM_Constants.UID_CRStorage;
                        } else if (modality.equals("MR")) {
                            classUID = DICOM_Constants.UID_MRStorage;
                        } else if (modality.equals("NM")) {
                            classUID = DICOM_Constants.UID_NMStorage;
                        } else if (modality.equals("PT")) {
                            classUID = DICOM_Constants.UID_PetStorage;
                        } else if (modality.equals("US")) {
                            classUID = DICOM_Constants.UID_USStorage;
                        } else if (modality.equals("XA")) {
                            classUID = DICOM_Constants.UID_XRayAngioStorage;
                        } else if (modality.equals("XF")) {
                            classUID = DICOM_Constants.UID_XRayFluoroStorage;
                        } else {
                            classUID = DICOM_Constants.UID_SCStorage;
                        }
                    }
                }
            }
        } catch (final IOException ioe) {
            System.out.println("DICOM_Store.getUIDs: " + ioe);
        }

        return;
    }

    private void getDirUIDs(final File fileDir) {
        int i;
        String[] dirList;

        try {
            final String dir = fileDir.getAbsolutePath();
            String sample = "";
            dirList = fileDir.list();
            String test = null;
            FileDicom fileDICOM = null;
            FileInfoDicom fInfoDicom = null;

            instanceUID = "";

            for (i = 0; i < dirList.length; i++) {
                test = dir + File.separatorChar + dirList[i];
                if (test != null && test.lastIndexOf('.') != -1) {
                    sample = test.substring(test.lastIndexOf('.')).trim();
                    if (sample.contains("dcm")) {
                        fileDICOM = new FileDicom(test);

                        if (fileDICOM.readHeader(true)) {
                            fInfoDicom = (FileInfoDicom) (fileDICOM.getFileInfo());
                            if (instanceUID == "") {
                                instanceUID = (String) (fInfoDicom.getTagTable().getValue("0008,0018"));
                            } else {
                                instanceUID = instanceUID + "\\"
                                        + (String) (fInfoDicom.getTagTable().getValue("0008,0018"));

                            }
                        }
                    }
                }
            }

        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Error opening directory:" + error);

        } catch (final IOException e) {
            e.printStackTrace();
        }

    }

    /**
     * Recursily desends the directory tree to send the images. This is a patient level is specified as the directly,
     * all studies, series, and images for that patient are sent
     * 
     * @param dir directory of where images to be sent are located
     * 
     * @return true is the method succeed in sending all images else false
     */
    private boolean sendImages(final String dir) {
        int i;
        File fileDir = null;
        String[] dirList;

        try {
            fileDir = new File(dir);

            if (fileDir.isFile() == true) {

                if (sendStoreRQ(fileDir.getPath()) == false) {
                    return false;
                }
            } else if (fileDir.isDirectory() == false) {
                MipavUtil.displayError("DICOM send images: Not a directory");
            } else {
                dirList = fileDir.list();

                for (i = 0; i < dirList.length; i++) {

                    if (sendImages(dir + File.separatorChar + dirList[i]) == false) {
                        return false;
                    }
                }
            }
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Error opening directory:" + error);

            return false;
        }

        return true;
    }

}
