package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.Vector;


/**
 * Recursively traverses a directory and its subfolders, converting all 3D DICOM or uncompressed AVI files to AVI with
 * MP42 Compression.
 *
 * @author   Ben Link
 * @version  1.0
 */

public class AlgorithmDCCIEConversion extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int compression = 0;

    /** Directory to recursively operate in. */
    private String dirPath;

    /** DOCUMENT ME! */
    private FileIO fileIO;

    /** DOCUMENT ME! */
    private boolean inputDICOM = true;

    /** Output path to build new tree. */
    private String outputPath; //

    /** DOCUMENT ME! */
    private ViewJProgressBar progressBar = null;

    /** DOCUMENT ME! */
    private float quality;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default Constructor.
     *
     * @param  ui           User Interface
     * @param  dir          full pathname of directory to traverse
     * @param  outputDir    DOCUMENT ME!
     * @param  compression  DOCUMENT ME!
     * @param  doDICOM      DOCUMENT ME!
     */
    public AlgorithmDCCIEConversion(ViewUserInterface ui, String dir, String outputDir, int compression,
                                    boolean doDICOM) {
        this.dirPath = dir;
        this.outputPath = outputDir;
        this.inputDICOM = doDICOM;

        // clip off the trailing file separator
        if (dirPath.endsWith(File.separator)) {
            dirPath = dirPath.substring(0, dirPath.length() - 1);
        }

        if (outputPath.endsWith(File.separator)) {
            outputPath = outputPath.substring(0, outputPath.length() - 1);
        }

        this.userInterface = ui;
        this.compression = compression;

        fileIO = new FileIO();
        fileIO.setQuiet(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public void runAlgorithm() {
        Vector fileVector = new Vector();

        fireProgressStateChanged("DCCIE Conversion", "Searching for images");
        fireProgressStateChanged(0);
        
        

        addFilesToVector(dirPath, fileVector);

        int size = fileVector.size();
        Preferences.debug("Number of images to be compressed: " + size + "\n");
        System.err.println("Number of images to be compressed: " + size);

        if (size > 0) {
            float percentDone = 0;
            fireProgressStateChanged((int) percentDone);

            for (int i = 0; i < size; i++) {
                fireProgressStateChanged("Opening image...");
                runConversion((String) fileVector.elementAt(i));
                System.gc();

                percentDone += (100.0 / (float) size);
                fireProgressStateChanged((int) percentDone);
            }

            fireProgressStateChanged(100);
        } else {
            fireProgressStateChanged(100);
            Preferences.debug("No files to convert" + "\n");
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
     * Recursively adds DICOM or AVI filenames and directory paths to a Vector.
     *
     * @param  name  The name of either file or directory
     * @param  vec   Vector that holds all files to be processed
     */
    private void addFilesToVector(String name, Vector vec) {
        File tempFile = new File(name);

        if (tempFile.isDirectory()) {
            String[] fileList = tempFile.list();
            tempFile = null;

            for (int i = 0; i < fileList.length; i++) {
                addFilesToVector(name + File.separator + fileList[i], vec);
            }
            // vec.add(0, name);
        } else if (name.endsWith(".tif") || name.endsWith(".tiff") || name.endsWith("TIF") || name.endsWith(".TIFF")) {
            vec.add(name);
        } else if (inputDICOM && (name.endsWith(".dcm") || name.endsWith(".DCM"))) {
            vec.add(name);
        } else if (!inputDICOM && (name.endsWith(".avi") || name.endsWith(".AVI"))) {
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

        File inputFile = null;
        int index = fileName.lastIndexOf(File.separator);
        String directory = fileName.substring(0, index + 1);
        String newDirectory = outputPath + directory.substring(dirPath.length(), directory.length());


        // before we do anything, if the file is a tiff file, we will simply copy it over
        // to the destination directory
        if (fileName.endsWith(".tif") || fileName.endsWith(".TIF") || fileName.endsWith(".tiff") ||
                fileName.endsWith(".TIFF")) {

            inputFile = new File(fileName);

            File outputFile = new File(newDirectory + File.separator +
                                       fileName.substring(fileName.lastIndexOf(File.separator), fileName.length()));
            new File(newDirectory).mkdirs();

            try {
                InputStream in = new FileInputStream(inputFile);
                OutputStream out = new FileOutputStream(outputFile);

                // Transfer bytes from in to out
                byte[] buf = new byte[1024];
                int len;

                while ((len = in.read(buf)) > 0) {
                    out.write(buf, 0, len);
                }

                in.close();
                out.close();
            } catch (Exception ex) {
                ex.printStackTrace();
            }

            return;
        }


        String name = "";

        Preferences.debug("New directory is: " + newDirectory + ", new file name is: " + name + "\n");

        ModelLUT lutA = null;

        ModelImage inputImage = null;

        try {
            inputFile = new File(fileName);
            System.err.println("filename: " + fileName);

            if (inputDICOM) {
            	fileIO.setFileDir(inputFile.getParent() + File.separator);
            	inputImage = fileIO.readDicom(inputFile.getName(), new String[] {inputFile.getName()}, false);
            } else {
                inputImage = fileIO.readImage(inputFile.getName(), inputFile.getParent() + File.separator);
            }

            inputFile = null;

            if (inputImage == null) {
                Preferences.debug(fileName + ": image is null... skipping" + "\n");

                return;
            } else if (inputImage.getNDims() == 2) {
                fireProgressStateChanged("Saving image as tiff...");
                inputImage.setImageName(fileName);

                String patientID = null;

                try {
                    patientID = (String)
                                    ((FileDicomTag) ((FileInfoDicom) inputImage.getFileInfo(0)).getEntry("0010,0020"))
                                        .getValue(false);
                    // System.err.println("patient id is: " + patientID);
                } catch (Exception ex) { // do nothing
                }

                name = fileName.substring((index + 1), (fileName.length() - 4));

                if ((patientID != null) && !patientID.equals("")) {
                    name += "_" + patientID;
                }

                name += ".tif";

                inputFile = new File(newDirectory);

                // if the new directory structure doesnt exist, create it, then transcode to AVI
                if (inputFile.exists() || inputFile.mkdirs()) {
                    FileWriteOptions opts = new FileWriteOptions(true);
                    opts.setFileType(FileUtility.TIFF);
                    opts.setFileDirectory(newDirectory);
                    opts.setFileName(name);
                    opts.setPackBitEnabled(false);
                    opts.setOptionsSet(true);
                    fileIO.writeImage(inputImage, opts);
                    Preferences.debug("Successfully saved Dicom as Tiff" + "\n");
                }

                inputFile = null;
                inputImage.disposeLocal();
                inputImage = null;
            } else {

                if (inputImage.isColorImage() && (compression == 1)) {
                    System.err.println("Ignoring " + fileName + ".  Can not encode 8 bit RLE with an RGB image");
                    MipavUtil.displayError("Ignoring " + fileName + ".  Can not encode 8 bit RLE with an RGB image");
                }

                inputImage.setImageName(fileName);

                lutA = fileIO.getModelLUT();

                /**
                 * Look for the patientID tag so it can be appended to the file's name
                 */
                String patientID = null;

                if (inputDICOM) {

                    try {
                        patientID = (String)
                                        ((FileDicomTag) ((FileInfoDicom) inputImage.getFileInfo(0)).getEntry("0010,0020"))
                                            .getValue(false);
                        // System.err.println("patient id is: " + patientID);
                    } catch (Exception ex) { // do nothing
                    }
                } else {
                    patientID = new String("");
                }

                name = fileName.substring((index + 1), (fileName.length() - 4));

                if ((patientID != null) && !patientID.equals("")) {
                    name += "_" + patientID;
                }

                name += ".avi";

                FileInfoXML[] fInfos = new FileInfoXML[inputImage.getExtents()[2]];

                for (int i = 0; i < fInfos.length; i++) {
                    fInfos[i] = new FileInfoImageXML(patientID, directory, inputImage.getType());
                }

                inputImage.setFileInfo(fInfos);
                inputImage.calcMinMax();

                /**
                 * now check to see if the dimensions are multiples of 4 (multiples of 8 for M-JPEG compression)
                 */
                int[] extents = inputImage.getExtents();

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

                    ModelImage paddedImage = new ModelImage(inputImage.getType(), newExtents, "TEMPImage",
                                                            userInterface);

                    fireProgressStateChanged("Adding margins to image...");

                    AlgorithmAddMargins algoMarg = null;

                    if (paddedImage.isColorImage()) {
                        algoMarg = new AlgorithmAddMargins(inputImage, paddedImage, 0, 0, 0, leftPadding, topPadding, 0,
                                                           0);
                    } else {
                        algoMarg = new AlgorithmAddMargins(inputImage, paddedImage, leftPadding, 0, topPadding, 0, 0);
                    }

                    algoMarg.run();
                    algoMarg.finalize();
                    algoMarg = null;
                    paddedImage.calcMinMax();
                    inputImage.disposeLocal();

                    // System.err.println("Disposed dicom image after padding");
                    inputImage = paddedImage;
                    paddedImage = null;
                }

                inputFile = new File(newDirectory);

                // if the new directory structure doesnt exist, create it, then transcode to AVI
                if (inputFile.exists() || inputFile.mkdirs()) {
                    FileAvi aviFile;
                    aviFile = new FileAvi(userInterface, name, newDirectory);
                    aviFile.setCompressionQuality(quality);

                    if (compression != 0) {
                        fireProgressStateChanged("Encoding image as compressed avi...");
                    } else {
                        fireProgressStateChanged("Saving image as avi...");
                    }

                    if (aviFile.writeImage(inputImage, null, lutA, null, null, null, 0, 0, 0, 0, 0,
                                               inputImage.getMask(), compression)) {
                        Preferences.debug("Successfully transcoded " + " to compressed AVI" + "\n");
                    } else {
                        Preferences.debug("Error transcoding " + " to compressed AVI" + "\n");
                    }

                    aviFile.finalize();
                    aviFile = null;

                }

                inputFile = null;
                lutA = null;
                inputImage.disposeLocal();
                inputImage = null;
            }
        } catch (Exception ex) {

            // System.err.println("Caught an exception in runConversion");
            Preferences.debug("Exception while converting image to Tiff and/or AVI" + "\n");
            ex.printStackTrace();
        }
    }

}
