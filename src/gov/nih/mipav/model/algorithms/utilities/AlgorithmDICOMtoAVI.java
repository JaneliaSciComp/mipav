package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;


/**
 * Recursively traverses a directory and its subfolders, converting all 3D DICOM files to AVI with MP42 Compression.
 *
 * @author   Ben Link
 * @version  1.0
 */
public class AlgorithmDICOMtoAVI extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int compression = 0;

    /** Directory to recursively operate in. */
    private String dirPath;

    /** DOCUMENT ME! */
    private FileIO fileIO;

    /** Output path to build new tree. */
    private String outputPath; //

    /** DOCUMENT ME! */
    private float quality;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default Constructor.
     *
     * @param  dir          full pathname of directory to traverse
     * @param  outputDir    DOCUMENT ME!
     * @param  compression  DOCUMENT ME!
     */
    public AlgorithmDICOMtoAVI(String dir, String outputDir, int compression) {
        this.dirPath = dir;
        this.outputPath = outputDir;

        // clip off the trailing file separator
        if (dirPath.endsWith(File.separator)) {
            dirPath = dirPath.substring(0, dirPath.length() - 1);
        }

        if (outputPath.endsWith(File.separator)) {
            outputPath = outputPath.substring(0, outputPath.length() - 1);
        }

        this.compression = compression;

        fileIO = new FileIO();
        fileIO.setQuiet(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public void runAlgorithm() {
        Vector<String> fileVector = new Vector<String>();

        fireProgressStateChanged("DICOM -> Compressed AVI", "Searching for DICOM images");
        fireProgressStateChanged(0);


        addFilesToVector(dirPath, fileVector);

        int size = fileVector.size();
        Preferences.debug("Number of DICOM images to be compressed: " + size + "\n", Preferences.DEBUG_FILEIO);
        System.err.println("Number of DICOM images to be compressed: " + size);

        if (size > 0) {
            float percentDone = 0;
            fireProgressStateChanged((int) percentDone);

            for (int i = 0; i < size; i++) {
                fireProgressStateChanged("Opening DICOM image...");
                runConversion((String) fileVector.elementAt(i));
                System.gc();

                percentDone += (100.0 / (float) size);
                fireProgressStateChanged((int) percentDone);
            }

            fireProgressStateChanged(100);
        } else {
            fireProgressStateChanged(100);
            Preferences.debug("No DICOM files to convert" + "\n", Preferences.DEBUG_FILEIO);
        }

        fileIO = null;

    }

    /**
     * DOCUMENT ME!
     *
     * @param  q  DOCUMENT ME!
     */
    public void setQuality(float q) {
        this.quality = q;
    }

    /**
     * Recursively adds DICOM filenames and directory paths to a Vector.
     *
     * @param  name  The name of either file or directory
     * @param  vec   Vector that holds all files to be processed
     */
    private void addFilesToVector(String name, Vector<String> vec) {
        File tempFile = new File(name);

        if (tempFile.isDirectory()) {
            String[] fileList = tempFile.list();
            tempFile = null;

            for (int i = 0; i < fileList.length; i++) {
                addFilesToVector(name + File.separator + fileList[i], vec);
            }
            // vec.add(0, name);
        } else if (name.endsWith(".dcm") || name.endsWith(".DCM")) {
            vec.add(name);
        }

        tempFile = null;
    }

    /**
     * Opens a dicom image, checks to see if it is 3d, adds margins if the image dimensions are not multiples of 4, and
     * saves the image as an AVI with MP42 compression**note - the directory structure is intact with the only
     * difference being that the name of the top level directory now has "_AVI" appended.
     *
     * @param  fileName  name of file to convert
     */
    private void runConversion(String fileName) {

        int index = fileName.lastIndexOf(File.separator);
        String directory = fileName.substring(0, index + 1);
        String newDirectory = outputPath + directory.substring(dirPath.length(), directory.length());
        String name = "";

        Preferences.debug("New directory is: " + newDirectory + ", new file name is: " + name + "\n", 
        		Preferences.DEBUG_FILEIO);

        ModelLUT lutA = null;

        try {
            File dicomFile = new File(fileName);
            System.err.println("filename: " + fileName);

            fileIO.setFileDir(dicomFile.getParent() + File.separator);

            ModelImage dicomImage = fileIO.readDicom(dicomFile.getName(), new String[] { dicomFile.getName() }, false);

            dicomFile = null;

            if (dicomImage == null) {

                // System.err.println("Name of file is: " + fileName);
                Preferences.debug(fileName + ": DICOM image is null... skipping" + "\n", Preferences.DEBUG_FILEIO);

                return;
            } else if (dicomImage.getNDims() == 2) {
                fireProgressStateChanged("Saving image as tiff...");
                dicomImage.setImageName(fileName);

                String patientID = null;

                try {
                    patientID = ((String)
                                    ((FileInfoDicom) dicomImage.getFileInfo(0)).getTagTable().get("0010,0020").getValue(false)).trim();
                    // System.err.println("patient id is: " + patientID);
                } catch (Exception ex) { // do nothing
                }

                name = fileName.substring((index + 1), (fileName.length() - 4));

                if ((patientID != null) && !patientID.equals("")) {
                    name += "_" + patientID;
                }

                name += ".tif";

                dicomFile = new File(newDirectory);

                // if the new directory structure doesnt exist, create it, then transcode to AVI
                if (dicomFile.exists() || dicomFile.mkdirs()) {
                    FileWriteOptions opts = new FileWriteOptions(true);
                    opts.setFileType(FileUtility.TIFF);
                    opts.setFileDirectory(newDirectory);
                    opts.setFileName(name);
                    opts.setPackBitEnabled(false);
                    opts.setOptionsSet(true);
                    fileIO.writeImage(dicomImage, opts);
                    Preferences.debug("Successfully saved Dicom as Tiff" + "\n", Preferences.DEBUG_FILEIO);
                }

                dicomFile = null;
                dicomImage.disposeLocal();
                dicomImage = null;
            } else {

                if (dicomImage.isColorImage() && (compression == 1)) {
                    System.err.println("Ignoring " + fileName + ".  Can not encode 8 bit RLE with an RGB image");
                    MipavUtil.displayError("Ignoring " + fileName + ".  Can not encode 8 bit RLE with an RGB image");
                }

                dicomImage.setImageName(fileName);

                lutA = fileIO.getModelLUT();

                /**
                 * Look for the patientID tag so it can be appended to the file's name
                 */
                String patientID = null;

                try {
                    patientID = (String)
                                    ((FileInfoDicom) dicomImage.getFileInfo(0)).getTagTable().get("0010,0020").getValue(false);
                    // System.err.println("patient id is: " + patientID);
                } catch (Exception ex) { // do nothing
                }

                name = fileName.substring((index + 1), (fileName.length() - 4));

                if ((patientID != null) && !patientID.equals("")) {
                    name += "_" + patientID;
                }

                name += ".avi";

                FileInfoXML[] fInfos = new FileInfoXML[dicomImage.getExtents()[2]];

                for (int i = 0; i < fInfos.length; i++) {
                    fInfos[i] = new FileInfoImageXML(patientID, directory, dicomImage.getType());
                }

                dicomImage.setFileInfo(fInfos);
                dicomImage.calcMinMax();

                /**
                 * now check to see if the dimensions are multiples of 4 (multiples of 8 for M-JPEG compression)
                 */
                int[] extents = dicomImage.getExtents();

                int leftPadding = 0;
                int mod = 4;

                if (compression == AlgorithmTranscode.TRANSCODE_MJPG) {
                    mod = 8;
                }

                if ((extents[0] % mod) != 0) {
                    leftPadding = mod - (extents[0] % mod);
                }

                int topPadding = 0;

                if ((extents[1] % mod) != 0) {
                    topPadding = mod - (extents[1] % mod);
                }

                if ((leftPadding != 0) || (topPadding != 0)) {

                    // System.out.println("Padding image");
                    int[] newExtents = new int[extents.length];
                    newExtents[0] = extents[0] + leftPadding;
                    newExtents[1] = extents[1] + topPadding;
                    newExtents[2] = extents[2];

                    int[] marginX = new int[]{leftPadding,0};
                    int[] marginY = new int[]{topPadding,0};
                    int[] marginZ = new int[]{0,0};

                    ModelImage paddedImage = new ModelImage(dicomImage.getType(), newExtents, "TEMPImage");

                    fireProgressStateChanged("Adding margins to image...");

                    AlgorithmAddMargins algoMarg = new AlgorithmAddMargins(dicomImage, paddedImage, 
                                marginX, marginY, marginZ );
                    algoMarg.setPadValue( new float[]{0,0,0} );

                    algoMarg.run();
                    algoMarg.finalize();
                    algoMarg = null;
                    paddedImage.calcMinMax();
                    dicomImage.disposeLocal();

                    // System.err.println("Disposed dicom image after padding");
                    dicomImage = paddedImage;
                    paddedImage = null;
                }

                dicomFile = new File(newDirectory);

                // if the new directory structure doesnt exist, create it, then transcode to AVI
                if (dicomFile.exists() || dicomFile.mkdirs()) {
                    FileAvi aviFile;
                    aviFile = new FileAvi(name, newDirectory);
                    aviFile.setCompressionQuality(quality);

                    if (compression != 0) {
                        fireProgressStateChanged("Encoding image as compressed avi...");
                    } else {
                        fireProgressStateChanged("Saving image as avi...");
                    }

                    if (aviFile.writeImage(dicomImage, null, lutA, null, null, null, 0, 0, 0, 0, 0,
                                           dicomImage.getMask(), compression)) {
                        Preferences.debug("Successfully transcoded " + " to compressed AVI" + "\n", Preferences.DEBUG_FILEIO);
                    } else {
                        Preferences.debug("Error transcoding " + " to compressed AVI" + "\n", Preferences.DEBUG_FILEIO);
                    }

                    aviFile.finalize();
                    aviFile = null;

                }

                dicomFile = null;
                lutA = null;
                dicomImage.disposeLocal();
                dicomImage = null;
            }
        } catch (Exception ex) {

            // System.err.println("Caught an exception in runConversion");
            Preferences.debug("Exception while converting DICOM to Tiff and/or AVI" + "\n");
            ex.printStackTrace();
        }
    }

}
