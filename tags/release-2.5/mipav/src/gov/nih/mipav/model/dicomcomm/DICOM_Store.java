package gov.nih.mipav.model.dicomcomm;


import gov.nih.mipav.model.file.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * This is the DICOM store class that defines functions to compose and send a store request to a DICOM image file
 * archive such as the image file server located in NIH's Clinical Center.
 *
 * @version  1.0
 */

public class DICOM_Store implements Runnable {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private String classUID;

    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private ViewJFrameDICOMQuery frame;

    /** DOCUMENT ME! */
    private String instanceUID;

    /** DOCUMENT ME! */
    private DICOM_PDUService pdu;

    /** DOCUMENT ME! */
    private String remoteAETitle;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new DICOM_Store object.
     *
     * @param  _fileName       file name of the images
     * @param  _remoteAETitle  remote application entity name
     * @param  _frame          MIPAV's query frame
     */
    public DICOM_Store(String _fileName, String _remoteAETitle, ViewJFrameDICOMQuery _frame) {
        fileName = _fileName;
        remoteAETitle = _remoteAETitle;
        frame = _frame;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Runs this move request in a separate thread.
     */
    public void run() {

        // sendStoreRQ(fileName, remoteAEtitle);
        try {

            // define an instance of the PDU_Service class to set up the connection
            // to the remote AE
            pdu = new DICOM_PDUService();
            pdu.connectClientToServer(remoteAETitle, false);

            if (sendImages(fileName) == true) {
                frame.appendSendMessage("Success -- " + fileName + " to " + remoteAETitle + "\n");
            } else {
                frame.appendSendMessage("Failed -- " + fileName + " to " + remoteAETitle + "\n");
            }

            pdu.close();
        } catch (OutOfMemoryError error) {
            frame.appendSendMessage("Failed -- " + fileName + " to " + remoteAETitle + "\n");
            MipavUtil.displayError("Error opening directory:" + error);
            pdu.close();

            return;
        } catch (DICOM_Exception e) {
            frame.appendSendMessage("Failed -- " + fileName + " to " + remoteAETitle + "\n");
            MipavUtil.displayError("Error: sendStoreRQ():" + e);
            pdu.close();

            return;
        }
    }

    /**
     * Reads in the DICOM image file from disk, composes a DICOM C-Store request and sends the request.
     *
     * @param   fileName  name of DICOM image file
     *
     * @return  true if the method succeeded in sending all images else false
     */
    public boolean sendStoreRQ(String fileName) {
        DICOM_Object ddo;
        DICOM_StdStorage storageSOP = new DICOM_StdStorage();
        boolean returnVal = false;

        try {
            ddo = pdu.readDICOMDataObjectFromFile(fileName);

            if (ddo == null) {
                MipavUtil.displayError("DICOMStore.sendStoreRQ(): DDO = null");

                return false;
            } else {

                // Get UIDs  and pass it as a parameter in the next method (write. )
                getUIDs(fileName);
                storageSOP.write(pdu, ddo, classUID, instanceUID);
                returnVal = true;
            }
        } catch (DICOM_Exception e) {
            MipavUtil.displayError("DICOMStore.sendStoreRQ():" + e);

            return false;
        }

        return (returnVal);
    }

    /**
     * Read in the DICOM file and parses the header to find the UIDs needed to send the image.
     *
     * @param  fileName  DICOM file to be read.
     */
    private void getUIDs(String fileName) {
        FileDicom fileDICOM = null;
        FileInfoDicom fInfoDicom = null;
        String modality = null;

        try {
            fileDICOM = new FileDicom(fileName);

            if (fileDICOM.readHeader(true)) {
                fInfoDicom = (FileInfoDicom) (fileDICOM.getFileInfo());
                classUID = (String) (fInfoDicom.getValue("0008,0016"));
                instanceUID = (String) (fInfoDicom.getValue("0008,0018"));

                if (classUID != null) {
                    classUID = DICOM_Util.trimIgnorableChar(classUID.trim());
                }

                if (instanceUID != null) {
                    instanceUID = DICOM_Util.trimIgnorableChar(instanceUID.trim());
                }

                if (classUID == null) {
                    modality = (String) (fInfoDicom.getValue("0008,0060"));
                    modality.trim();

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
        } catch (IOException ioe) {
            System.out.println("DICOM_Store.getUIDs: " + ioe);
        }

        return;
    }

    /**
     * Recursily desends the directory tree to send the images. This is a patient level is specified as the directly,
     * all studies, series, and images for that patient are sent
     *
     * @param   dir  directory of where images to be sent are located
     *
     * @return  true is the method succeed in sending all images else false
     */
    private boolean sendImages(String dir) {
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
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Error opening directory:" + error);

            return false;
        }

        return true;
    }

}
