import gov.nih.mipav.model.file.*;

import gov.nih.mipav.view.MipavUtil;

import java.io.*;
import java.security.*;
import java.text.*;
import java.util.*;

import javax.swing.*;


public class PlugInAlgorithmNINDSIdentificationTool extends PlugInAlgorithmNINDSAnonymizationTool {

    protected static FileDicomKey studyTimeKey;

    static {
        PlugInAlgorithmNINDSIdentificationTool.studyTimeKey = new FileDicomKey("0008,0030");
    }

    public PlugInAlgorithmNINDSIdentificationTool(final String inputDirectoryPath, final String outputDirectoryPath,
            final JTextArea outputTextArea, final JLabel errorMessageLabel, final boolean enableTextArea,
            final boolean renameGrandParentDir, final JDialog parentDialog, final String csvFilePath,
            final boolean newCSVFile) {
        super(inputDirectoryPath, outputDirectoryPath, null, null, outputTextArea, errorMessageLabel, parentDialog,
                csvFilePath, newCSVFile);
        // TODO Auto-generated constructor stub
    }

    /**
     * run algorithm
     */
    public void runAlgorithm() {
        final long begTime = System.currentTimeMillis();

        try {
            System.out.println("here ");
            outputTextFileName = "output_" + System.currentTimeMillis() + ".txt";
            System.out.println("ccc");
            outputFile = new File(inputDirectoryPath + File.separator + outputTextFileName);
            outputStream = new FileOutputStream(outputFile);
            printStream = new PrintStream(outputStream);
            csvFile = new File(csvFilePath);
            outputStreamCSV = new FileOutputStream(csvFile, true);
            printStreamCSV = new PrintStream(outputStreamCSV);
            if (newCSVFile) {
                printStreamCSV
                        .println("fileLoc,patientID,dob,patientsAge,studyDate,studyID,studyTime,seriesNo,sequenceName");
            }
            System.out.println("aaa");
            final Calendar t = Calendar.getInstance();
            final SimpleDateFormat sdf = new SimpleDateFormat("MMddyyyy");
            todaysDateString = sdf.format(t.getTime());
            System.out.println(todaysDateString);
            System.out.println("bbb");
        } catch (final Exception e) {
            System.out.println(e.getMessage());
        }

        final DateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy HH:mm:ss");
        final Date date = new Date();
        if (enableTextArea) {
            outputTextArea.append(dateFormat.format(date) + "\n\n");
        }
        printStream.println(dateFormat.format(date));
        printStream.println();

        if (enableTextArea) {
            outputTextArea.append("** Beginning NINDS Anonymization **\n\n");
        }
        printStream.println("** Beginning NINDS Anonymization **");
        printStream.println();

        if (enableTextArea) {
            outputTextArea.append("Input Directory is " + inputDirectoryPath + "\n");
        }
        printStream.println("Input Directory is " + inputDirectoryPath);

        if (enableTextArea) {
            outputTextArea.append("Output Directory is " + outputDirectoryPath + "\n\n");
        }
        printStream.println("Output Directory is " + outputDirectoryPath);
        printStream.println();

        // first create a File object based upon the study path
        final File inputDirectoryRoot = new File(inputDirectoryPath);

        success = parse(inputDirectoryRoot);

        if (success == false) {
            if (enableTextArea) {
                outputTextArea.append("! Algorithm Canceled \n");
            }
            errorMessageLabel.setText("! Algorithm Canceled");
            printStream.println("! Algorithm Canceled");
            finalize();
            setCompleted(true);
        }

        if (enableTextArea) {
            outputTextArea.append("** Ending NINDS Identification **\n\n");
        }
        printStream.println("** Ending NINDS Identification **");
        printStream.println();

        final long endTime = System.currentTimeMillis();
        final long diffTime = endTime - begTime;
        final float seconds = ((float) diffTime) / 1000;

        if (enableTextArea) {
            outputTextArea.append("Algorithm took " + seconds + " seconds \n");
        }
        printStream.println("Algorithm took " + seconds + " seconds");

        finalize();
        setCompleted(true);
    }

    /**
     * identifies the dicom tags in a file
     * 
     * @return
     */
    public boolean anonymizeDICOMTags(final File f) {
        tagTable = fileInfoDicom.getTagTable();
        final String studyID = ((String) tagTable.getValue(PlugInAlgorithmNINDSAnonymizationTool.studyIDKey)).trim();
        final String seriesNo = ((String) tagTable.getValue(PlugInAlgorithmNINDSAnonymizationTool.seriesNoKey)).trim();
        String sequenceName = "";
        if (tagTable.getValue(PlugInAlgorithmNINDSAnonymizationTool.sequenceNameKey) != null) {
            sequenceName = ((String) tagTable.getValue(PlugInAlgorithmNINDSAnonymizationTool.sequenceNameKey)).trim();
        }

        if (enableTextArea) {
            outputTextArea.append("Study ID is " + studyID + " \n");
        }
        printStream.println("Study ID is " + studyID);

        boolean validDOB = true;
        String studyDate = "";

        // couple of the tags (patient name (0010,0010)and patient id(0010,0020) will be replaced with a new UID which
        // is Suject ID + DOB
        // example: id = 1234, DOB=9/23/1968 => 1234 + 9231968 = 9233202
        String patientID = "";
        String dob = "";
        newUID = "";
        int patientIDInt = 0;
        if (tagTable.containsTag(PlugInAlgorithmNINDSAnonymizationTool.patientIDKey)) {
            patientID = ((String) tagTable.getValue(PlugInAlgorithmNINDSAnonymizationTool.patientIDKey)).trim();

        }

        try {
            patientIDInt = new Integer(patientID).intValue();
        } catch (final NumberFormatException e) {
            patientIDInt = 0;
            System.err.println("Note unusual patientID, new UID not created");
            e.printStackTrace();
        }

        studyDate = ((String) tagTable.getValue(PlugInAlgorithmNINDSAnonymizationTool.studyDateKey)).trim();
        if (studyDate.contains("/")) {
            studyDate = studyDate.replaceAll("\\/", "");
        }
        if (studyDate.length() != 8) {
            if (enableTextArea) {
                outputTextArea.append("! Study Date(0008,0020) value is not a valid entry \n");
            }
            printStream.println("! Study Date(0008,0020) value is not a valid entry");
            return false;
        }
        String sdmmString = studyDate.substring(0, 2);
        String sdddString = studyDate.substring(2, 4);
        final String sdyyyyString = studyDate.substring(4, 8);

        if (tagTable.containsTag(PlugInAlgorithmNINDSAnonymizationTool.patientDOBKey)) {
            dob = ((String) tagTable.getValue(PlugInAlgorithmNINDSAnonymizationTool.patientDOBKey)).trim();
            if (dob.contains("/")) {
                dob = dob.replaceAll("\\/", "");
            }
        }
        // dob length is 8 MMYYDDDD
        // dob might be written in "years"...but it also might have its length as 8...so test for that
        int dobInt = 0;
        if (dob.equals("")) {
            validDOB = false;
        } else {
            if (dob.length() == 8) {
                try {
                    dobInt = Integer.valueOf(dob);
                    validDOB = true;
                } catch (final NumberFormatException e) {
                    validDOB = false;
                }
            } else {
                validDOB = false;
            }
        }

        int newUIDInt = 0;
        // if dob is there and in right format , create newUID using this field....otherwise if dob is not there
        String dobString = "";
        if (validDOB) {
            String mmString = dob.substring(0, 2);
            String ddString = dob.substring(2, 4);
            final String yyyyString = dob.substring(4, dob.length());
            if (mmString.startsWith("0")) {
                mmString = mmString.substring(1, 2);
            }
            if (ddString.startsWith("0")) {
                ddString = ddString.substring(1, 2);
            }
            dobString = mmString + "/" + ddString + "/" + yyyyString;
            dobInt = Integer.valueOf(dobString.replaceAll("/", ""));
            newUIDInt = patientIDInt + dobInt;
            newUID = String.valueOf(newUIDInt);
        } else {
            // print unknown
        }

        // MD5
        MessageDigest digest = null;
        try {
            digest = MessageDigest.getInstance("MD5");
        } catch (final NoSuchAlgorithmException e) {
            if (enableTextArea) {
                outputTextArea.append("! Error in MD5 hash algorithm \n");
            }
            printStream.println("! Error in MD5 hash algorithm");
            e.printStackTrace();
            return false;
        }

        // patient's birthdate (0010,0030) will be replaced with patients age...so get patients age
        String patientsAge = "";
        if (tagTable.containsTag(PlugInAlgorithmNINDSAnonymizationTool.patientAgeKey)) {
            if (calculatedAge != -1) {
                patientsAge = String.valueOf(calculatedAge);
            } else {
                patientsAge = (String) tagTable.getValue(PlugInAlgorithmNINDSAnonymizationTool.patientAgeKey);
            }
        }

        String studyTime = "";
        if (tagTable.containsTag(PlugInAlgorithmNINDSIdentificationTool.studyTimeKey)) {
            studyTime = (String) tagTable.getValue(PlugInAlgorithmNINDSIdentificationTool.studyTimeKey);
        }

        // Study Instance UID (0020,000D) and Series Instance UID (0002,000E) need MIPAV version and time in
        // milliseconds
        String mipavVersion = MipavUtil.getVersion();
        mipavVersion = mipavVersion.replaceAll("\\.", "");

        // write out csvFile
        final String csvCheck = patientID + studyID + seriesNo;

        // remove beginning zeros for study date and todays date when writing to csv
        if (sdmmString.startsWith("0")) {
            sdmmString = sdmmString.substring(1, 2);
        }
        if (sdddString.startsWith("0")) {
            sdddString = sdddString.substring(1, 2);
        }
        studyDate = sdmmString + "/" + sdddString + "/" + sdyyyyString;
        // write out to csv
        printStreamCSV.println(f.getAbsolutePath() + "," + patientID + "," + dobString + "," + patientsAge + ","
                + studyDate + "," + studyID + "," + studyTime + "," + seriesNo + "," + sequenceName);
        donePatientIDs.add(csvCheck);

        return true;
    }

    /**
     * anonymize dicom images
     * 
     * @return
     */
    public boolean anonymizeDICOM(final File file) {
        // read in image
        if (enableTextArea) {
            outputTextArea.append("Reading in " + file.getName() + " from " + file.getParent() + " \n");
        }
        printStream.println("Reading in " + file.getName() + " from " + file.getParent());
        final String absPath = file.getAbsolutePath();
        inputImage = fileIO.readImage(absPath);
        fileInfoDicom = (FileInfoDicom) inputImage.getFileInfo(0);

        // anonymize
        if (enableTextArea) {
            outputTextArea.append("Extracting from " + file.getName() + " \n");
        }
        printStream.println("Extracting from " + file.getName());
        success = anonymizeDICOMTags(file);
        if (success == false) {
            fileInfoDicom.finalize();
            inputImage.disposeLocal();
            inputImage = null;
            return false;
        }

        // save anonymized image
        final String outputDir = file.getParent().replace(inputDirectoryPath, outputDirectoryPath);

        inputImage.disposeLocal();
        inputImage = null;
        fileInfoDicom = null;

        return true;
    }

}
