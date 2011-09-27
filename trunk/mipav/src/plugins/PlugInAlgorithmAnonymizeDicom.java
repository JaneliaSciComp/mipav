import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileDicomTagInfo.VR;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;
import java.util.*;


public class PlugInAlgorithmAnonymizeDicom extends AlgorithmBase {

    public static final String ANONYMIZED = "ANONYMIZED :";

    // ~ Instance fields ---------------------------------------------------------------------------------------
    /** File selected by the user */
    private File[] selectedFiles;

    /** Additional tag list provided in the dialog */
    private final String[] tagListFromDialog;

    /** Stream for writing out anonymized/private/sequence tags. */
    private PrintStream printToLogFile;

    /**
     * Location of the anonymized data, wouldn't mind making this a static directory in MIPAV preferences that as a log
     * file
     */
    private String anonLoc;

    /** Location for anonymized images to go */
    private final String submitImageLocation;

    // ~ Constructors -----------------------------------------------------------------------------------------
    /**
     * Main constructor, notes works best when inputFiles come from one image set, since output is meant to occur in one
     * place.
     */
    public PlugInAlgorithmAnonymizeDicom(final File[] inputFiles, final String[] tagList,
            final String submitImageLocation, final String submitPatientLocation) {
        final ArrayList<File> selectedFilesList = new ArrayList<File>();
        for (final File element : inputFiles) {
            if (element.isDirectory()) {
                for (final File f : element.listFiles()) {
                    selectedFilesList.add(f);
                }
            } else {
                selectedFilesList.add(element);
            }
        }

        this.submitImageLocation = submitImageLocation;
        this.selectedFiles = inputFiles;
        this.tagListFromDialog = tagList;

        File f = new File(submitPatientLocation);
        if ( !f.exists()) {
            f.mkdirs();
        }

        f = new File(submitImageLocation);
        if ( !f.exists()) {
            f.mkdirs();
        }

        if (selectedFiles.length > 0) {
            anonLoc = submitPatientLocation + File.separator + "AnonymizationResults.txt";
        } else {
            anonLoc = "";
        }

    }

    // ~ Methods ----------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        printToLogFile = null;
        selectedFiles = null;
    }

    public String getAnonResultsLoc() {
        return anonLoc;
    }

    @Override
    public void runAlgorithm() {
        if (selectedFiles == null) {
            displayError("Selected file is null.");
            return;
        }

        final File[] allTempFiles = constructTemporaryFiles();

        try {
            printToLogFile = new PrintStream(new FileOutputStream(anonLoc));
            printToLogFile.println("Note: The tags listed below were anonymized by the DICOM Anonymization Tool.");
            printToLogFile
                    .println("Private tags and sequence tags are not anonymized but are listed so that the user can anonymize them manually.");
            printToLogFile.println();
        } catch (final IOException ioe) {
            ioe.printStackTrace();
        }

        anonymizeTemporaryFiles(allTempFiles);

        Preferences.debug("Finished reading files");
        printToLogFile.flush();
        printToLogFile.close();

        moveTemporaryFiles(allTempFiles);

        Preferences.debug("Finished writing files");

    }

    private File[] constructTemporaryFiles() {
        final int numOfFiles = selectedFiles.length;
        final File[] allTempFiles = new File[selectedFiles.length];
        File toWrite;
        for (int i = 0; i < numOfFiles; i++) {
            try {
                toWrite = File.createTempFile(selectedFiles[i].getName(), ".tmp");
                toWrite.deleteOnExit();
                System.out.println("Created file: " + toWrite.getAbsolutePath());
                final BufferedInputStream in = new BufferedInputStream(new FileInputStream(selectedFiles[i]));
                final BufferedOutputStream out = new BufferedOutputStream(new FileOutputStream(toWrite));
                int n;
                while ( (n = in.read()) != -1) {
                    out.write(n);
                }
                out.flush();
                out.close();
                in.close();
                allTempFiles[i] = toWrite;
            } catch (final Exception e) {
                e.printStackTrace();
            }
        }

        return allTempFiles;
    }

    private void anonymizeTemporaryFiles(final File[] allTempFiles) {
        final int numOfFiles = selectedFiles.length;
        final ArrayList<Integer> filesNotRead = new ArrayList<Integer>();
        int progressNum = minProgressValue;
        HashMap<FileDicomKey, FileDicomSQ> prevSliceSequenceTags = new HashMap<FileDicomKey, FileDicomSQ>();
        HashMap<FileDicomKey, Object> prevSlicePrivateTags = new HashMap<FileDicomKey, Object>();
        HashMap<FileDicomKey, Object> prevSliceAnonymizeTags = new HashMap<FileDicomKey, Object>();
        for (int i = 0; i < numOfFiles; i++) {
            // use the selectedFileName as the reference slice for the file info tag tables
            progressNum = minProgressValue
                    + (int) ( (maxProgressValue - minProgressValue) * ( ((double) i) / ((double) numOfFiles)));
            fireProgressStateChanged(progressNum, null, "Reading file " + i);
            try {
                if (i > 0) {
                    printToLogFile.println();
                    printToLogFile.println();
                    printToLogFile
                            .println("***********************************NEXT FILE*******************************************");
                    printToLogFile.println();
                }
                printToLogFile.println("Reading " + (i > 0 ? "next " : "") + "file " + selectedFiles[i].getName()); // TODO:
                                                                                                                    // path
                                                                                                                    // fix
                                                                                                                    // from
                                                                                                                    // temp
                                                                                                                    // file
                final FileDicom imageFile = new FileDicom(selectedFiles[i].getName(), allTempFiles[i].getParent()
                        + File.separator);
                //TODO: Test readability of new header here (could be provided as a user option.)
                printToLogFile.println();
                printToLogFile.println("The " + (i > 0 ? "unique " : "")
                        + "anonymized tags for this file are printed below:");
                //prevSliceAnonymizeTags = imageFile.storeAnonymizeTags(prevSliceAnonymizeTags);
                printToLogFile.println();
                printToLogFile.println("The " + (i > 0 ? "unique " : "")
                        + "private tags for this file are printed below:");
                //prevSlicePrivateTags = imageFile.storePrivateTags(prevSlicePrivateTags);
                printToLogFile.println();
                printToLogFile.println("The " + (i > 0 ? "unique " : "")
                        + "sequence tags for this file are printed below:");
                //prevSliceSequenceTags = imageFile.storeSequenceTags(prevSliceSequenceTags);
                printToLogFile.println();
                printToLogFile.println();
            } catch (final Exception e) {
                e.printStackTrace();
                filesNotRead.add(i);
            }
        }

        String stringExp = "";
        for (int i = 0; i < filesNotRead.size(); i++) {
            stringExp = stringExp + selectedFiles[filesNotRead.get(i)].getAbsolutePath() + "\n";
        }
        if (stringExp.length() > 0) {
            MipavUtil.displayError("The following files could not be anonymized:\n" + stringExp);
        }
    }

    private void moveTemporaryFiles(final File[] allTempFiles) {
        final int numOfFiles = selectedFiles.length;
        for (int i = 0; i < numOfFiles; i++) {
            try {
                final BufferedInputStream in = new BufferedInputStream(new FileInputStream(allTempFiles[i]));
                final BufferedOutputStream out = new BufferedOutputStream(new FileOutputStream(submitImageLocation
                        + selectedFiles[i].getName()));
                Preferences.debug("Writing anonymized file: " + submitImageLocation + selectedFiles[i].getName());
                int n;
                while ( (n = in.read()) != -1) {
                    out.write(n);
                }
                out.flush();
                out.close();
                in.close();
            } catch (final Exception e) {
                e.printStackTrace();
            }
        }
    }

    protected boolean tagExistInAnonymizeTagIDs(final String tagName) {
        final int len = FileInfoDicom.anonymizeTagIDs.length;

        if (tagListFromDialog != null) {
            final int lenTagList = tagListFromDialog.length;

            for (int j = 0; j < lenTagList; j++) {

                if (tagListFromDialog[j].equals(tagName)) {
                    return true;
                }
            }
        }

        if (len == 0) {
            return false;
        }

        for (int i = 0; i < len; i++) {

            if (FileInfoDicom.anonymizeTagIDs[i].equals(tagName)) {
                return true;
            }

        }

        return false;
    }

    private class KeyComparator implements Comparator<FileDicomKey> {

        /*
         * (non-Javadoc)
         * 
         * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
         */
        public int compare(final FileDicomKey o1, final FileDicomKey o2) {
            if ( (o2).getGroupNumber() == (o1).getGroupNumber()) {
                return (o1).getElementNumber() - (o2).getElementNumber();
            }
            return (o1).getGroupNumber() - (o2).getGroupNumber();
        }
    }

}
