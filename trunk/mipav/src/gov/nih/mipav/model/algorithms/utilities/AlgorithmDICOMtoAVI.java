package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.util.Vector;
import java.io.File;
import java.awt.Toolkit;
import java.util.*;


/**
 * <p>Title: AlgorithmDICOMtoAVI </p>
 * <p>Description: Recursively traverses a directory and its subfolders,
 *    converting all 3D DICOM files to AVI with MP42 Compression </p>
 * @author Ben Link
 * @version 1.0
 */

public class AlgorithmDICOMtoAVI extends AlgorithmBase {

    /**
     * Directory to recursively operate in
     */
    private String dirPath;

    /**
     * Output path to build new tree
     */
    private String outputPath; //

    private ViewUserInterface userInterface = null;
    private ViewJProgressBar progressBar = null;

    private int compression = 0;
    private float quality;
    private FileIO fileIO;

    /**
     * Default Constructor
     * @param ui User Interface
     * @param dir full pathname of directory to traverse
     */
    public AlgorithmDICOMtoAVI( ViewUserInterface ui, String dir, String outputDir, int compression ) {
        this.dirPath = dir;
        this.outputPath = outputDir;
        //clip off the trailing file separator
        if ( dirPath.endsWith( File.separator ) ) {
            dirPath = dirPath.substring( 0, dirPath.length() - 1 );
        }
        if ( outputPath.endsWith( File.separator ) ) {
            outputPath = outputPath.substring( 0, outputPath.length() - 1 );
        }
        this.userInterface = ui;
        this.compression = compression;

        fileIO = new FileIO();
        fileIO.setQuiet( true );
    }

    public void setQuality( float q ) {
        this.quality = q;
    }

    public void runAlgorithm() {
        Vector fileVector = new Vector();

        progressBar = new ViewJProgressBar( "DICOM -> Compressed AVI", "Searching for DICOM images", 0, 100, false, null,
                null );
        progressBar.updateValue( 0, activeImage );
        progressBar.setLocation( (int) Toolkit.getDefaultToolkit().getScreenSize().getWidth() / 2, 50 );
        progressBar.setVisible( true );

        addFilesToVector( dirPath, fileVector );

        int size = fileVector.size();
        Preferences.debug( "Number of DICOM images to be compressed: " + size + "\n" );
        System.err.println( "Number of DICOM images to be compressed: " + size );
        if ( size > 0 ) {
            float percentDone = 0;
            progressBar.updateValue( (int) percentDone, activeImage );

            for ( int i = 0; i < size; i++ ) {
                progressBar.setMessage( "Opening DICOM image..." );
                runConversion( (String) fileVector.elementAt( i ) );
                System.gc();

                percentDone += ( 100.0 / (float) size );
                progressBar.updateValue( (int) percentDone, activeImage );
            }
            progressBar.updateValue( 100, activeImage );
        } else {
            progressBar.updateValue( 100, activeImage );
            Preferences.debug( "No DICOM files to convert" + "\n" );
        }
        fileIO = null;
        progressBar.setVisible( false );
    }

    /**
     * Opens a dicom image, checks to see if it is 3d, adds margins if the image dimensions
     * are not multiples of 4, and saves the image as an AVI with MP42 compression
     * **note - the directory structure is intact with the only difference being that the name of the
     * top level directory now has "_AVI" appended
     * @param fileName name of file to convert
     */
    private void runConversion( String fileName ) {

        int index = fileName.lastIndexOf( File.separator );
        String directory = fileName.substring( 0, index + 1 );
        String newDirectory = outputPath + directory.substring( dirPath.length(), directory.length() );
        String name = "";

        Preferences.debug( "New directory is: " + newDirectory + ", new file name is: " + name + "\n" );

        ModelLUT lutA = null;

        try {
            File dicomFile = new File( fileName );
            System.err.println( "filename: " + fileName );
            ModelImage dicomImage = fileIO.readDicom( dicomFile );
            dicomFile = null;

            if ( dicomImage == null ) {
                //System.err.println("Name of file is: " + fileName);
                Preferences.debug( fileName + ": DICOM image is null... skipping" + "\n" );
                return;
            } else if ( dicomImage.getNDims() == 2 ) {
                progressBar.setMessage( "Saving image as tiff..." );
                dicomImage.setImageName( fileName );

                String patientID = null;
                try {
                    patientID = (String) ( (FileDicomTag) ( (FileInfoDicom) dicomImage.getFileInfo( 0 ) ).getEntry( "0010,0020" ) ).getValue(
                            false );
                    //System.err.println("patient id is: " + patientID);
                } catch ( Exception ex ) {//do nothing
                }

                name = fileName.substring( ( index + 1 ), ( fileName.length() - 4 ) );
                if ( patientID != null && !patientID.equals( "" ) ) {
                    name += "_" + patientID;
                }
                name += ".tif";

                dicomFile = new File( newDirectory );

                // if the new directory structure doesnt exist, create it, then transcode to AVI
                if ( dicomFile.exists() || dicomFile.mkdirs() ) {
                    FileWriteOptions opts = new FileWriteOptions( true );
                    opts.setFileType( FileBase.TIFF );
                    opts.setFileDirectory( newDirectory );
                    opts.setFileName( name );
                    opts.setPackBitEnabled( false );
                    opts.setOptionsSet( true );
                    fileIO.writeImage( dicomImage, opts );
                    Preferences.debug( "Successfully saved Dicom as Tiff" + "\n" );
                }
                dicomFile = null;
                dicomImage.disposeLocal();
                dicomImage = null;
            } else {
                if ( dicomImage.isColorImage() && compression == 1 ) {
                    System.err.println( "Ignoring " + fileName + ".  Can not encode 8 bit RLE with an RGB image" );
                    MipavUtil.displayError( "Ignoring " + fileName + ".  Can not encode 8 bit RLE with an RGB image" );
                }
                dicomImage.setImageName( fileName );

                lutA = fileIO.getModelLUT();

                /**
                 * Look for the patientID tag so it can be appended to the file's name
                 */
                String patientID = null;
                try {
                    patientID = (String) ( (FileDicomTag) ( (FileInfoDicom) dicomImage.getFileInfo( 0 ) ).getEntry( "0010,0020" ) ).getValue(
                            false );
                    //System.err.println("patient id is: " + patientID);
                } catch ( Exception ex ) {//do nothing
                }

                name = fileName.substring( ( index + 1 ), ( fileName.length() - 4 ) );
                if ( patientID != null && !patientID.equals( "" ) ) {
                    name += "_" + patientID;
                }
                name += ".avi";

                FileInfoXML[] fInfos = new FileInfoXML[dicomImage.getExtents()[2]];
                for ( int i = 0; i < fInfos.length; i++ ) {
                    fInfos[i] = new FileInfoImageXML( patientID, directory, dicomImage.getType() );
                }
                dicomImage.setFileInfo( fInfos );
                dicomImage.calcMinMax();

                /**
                 * now check to see if the dimensions are multiples of 4
                 * (multiples of 8 for M-JPEG compression)
                 */
                int[] extents = dicomImage.getExtents();

                int leftPadding = 0;
                int mod = 4;
                if ( compression == AlgorithmTranscode.TRANSCODE_MJPG ) {
                    mod = 8;
                }
                if ( extents[0] % mod != 0 ) {
                    leftPadding = mod - ( extents[0] % mod );
                }
                int topPadding = 0;
                if ( extents[1] % mod != 0 ) {
                    topPadding = mod - ( extents[1] % mod );
                }

                if ( leftPadding != 0 || topPadding != 0 ) {
                    //System.out.println("Padding image");
                    int[] newExtents = new int[extents.length];
                    newExtents[0] = extents[0] + leftPadding;
                    newExtents[1] = extents[1] + topPadding;
                    newExtents[2] = extents[2];

                    ModelImage paddedImage = new ModelImage( dicomImage.getType(), newExtents, "TEMPImage",
                            userInterface );

                    progressBar.setMessage( "Adding margins to image..." );
                    AlgorithmAddMargins algoMarg = null;

                    if ( paddedImage.isColorImage() ) {
                        algoMarg = new AlgorithmAddMargins( dicomImage, paddedImage, 0, 0, 0, leftPadding, topPadding, 0,
                                0 );
                    } else {
                        algoMarg = new AlgorithmAddMargins( dicomImage, paddedImage, leftPadding, 0, topPadding, 0, 0 );
                    }

                    algoMarg.setProgressBarVisible( false );
                    algoMarg.run();
                    algoMarg.finalize();
                    algoMarg = null;
                    paddedImage.calcMinMax();
                    dicomImage.disposeLocal();
                    //System.err.println("Disposed dicom image after padding");
                    dicomImage = paddedImage;
                    paddedImage = null;
                }

                dicomFile = new File( newDirectory );

                // if the new directory structure doesnt exist, create it, then transcode to AVI
                if ( dicomFile.exists() || dicomFile.mkdirs() ) {
                    FileAvi aviFile;
                    aviFile = new FileAvi( userInterface, name, newDirectory );
                    aviFile.setCompressionQuality( quality );

                    aviFile.setProgressBarVisible( false );

                    if ( compression != 0 ) {
                        progressBar.setMessage( "Encoding image as compressed avi..." );
                    } else {
                        progressBar.setMessage( "Saving image as avi..." );
                    }

                    if ( aviFile.writeImage( dicomImage, null, lutA, null, null, null, 0, 0, 0, 0, 0,
                            dicomImage.getMask(), compression ) ) {
                        Preferences.debug( "Successfully transcoded " + " to compressed AVI" + "\n" );
                    } else {
                        Preferences.debug( "Error transcoding " + " to compressed AVI" + "\n" );
                    }
                    aviFile.finalize();
                    aviFile = null;

                }
                dicomFile = null;
                lutA = null;
                dicomImage.disposeLocal();
                dicomImage = null;
            }
        } catch ( Exception ex ) {
            //System.err.println("Caught an exception in runConversion");
            Preferences.debug( "Exception while converting DICOM to Tiff and/or AVI" + "\n" );
            ex.printStackTrace();
        }
    }

    /**
     * Recursively adds DICOM filenames and directory paths to a Vector
     * @param name The name of either file or directory
     * @param vec Vector that holds all files to be processed
     */
    private void addFilesToVector( String name, Vector vec ) {
        File tempFile = new File( name );

        if ( tempFile.isDirectory() ) {
            String[] fileList = tempFile.list();
            tempFile = null;
            for ( int i = 0; i < fileList.length; i++ ) {
                addFilesToVector( name + File.separator + fileList[i], vec );
            }
            //vec.add(0, name);
        } else if ( name.endsWith( ".dcm" ) || name.endsWith( ".DCM" ) ) {
            vec.add( name );
        }
        tempFile = null;
    }

}
